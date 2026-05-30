!> summary: Near-SHETRAN visualisation accessor interface.
!>
!> This module is the left-hand side of the SHETRAN/SHEGRAPH visualisation
!> interface. It reads model state from the core SHETRAN modules using native
!> element, face, layer, and grid numbering, converts selected fluxes to
!> plotting units, and exposes small accessor functions for the central
!> visualisation translation layer.
!>
!> Conventions:
!>
!> | Item | Convention |
!> |:-----|:-----------|
!> | Face order | SHETRAN native order: 1 east, 2 north, 3 west, 4 south. |
!> | Element type | `ETYPE=0` grid square, `1:2` banks, `3` river/link. |
!> | Missing integer | `i_not_exist = -1`. |
!> | Missing real | `r_not_exist = -1.0`. |
!> | Rates for plotting | Selected m/s fluxes are converted to mm/hour or mm/day. |
!>
!> Maintenance rules retained from the legacy interface:
!>
!> - Add accessor functions here when new variables need raw SHETRAN state.
!> - Keep the module `PRIVATE`; extend only explicit `PUBLIC` lists.
!> - Do not remove public accessors without updating dependent interfaces.
!> - [[visualisation_interface_centre]] depends on this exported accessor contract.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 200407 | JE | 2.0 | Created for SHEGRAPH Version 2. |
!> | 20041122 | JE | - | Made common to SHETRAN Versions 3 and 4. |
!> @endhistory
MODULE visualisation_interface_left

   USE SGLOBAL, ONLY    : fffatal, pppri, ERROR, dxqq, dyqq, zgrund, total_no_elements, top_cell_no, nlf=>total_no_links
   USE AL_C, ONLY       : cmd,                &  !file unit for contaminants
      draina,             & !drainage from intercepted canopy water (m/s)
   !dxqq, dyqq, cwidth, & !element x,y widths and river width
      cwidth,             &  !river width
   !FATAL,              & !for use with ERROR
   !hrf,                & !surface water elevation (m)
      nlyr,               & !no. of soil layers
      nlyrbt,             & !bottom cell layer in each soil layer
      ntsoil,             & !soil type in each soil layer
      nvc,                & !vegetation index
      pnetto,             & !net_rainfall (m/s)
      qoc,                & !overland flow (m^3/s)
      syd,                & !file unit for sediments
      wberr!,              & !water balance error (m)
   !zgrund                !surface elevation(M)
!USE PERTURBATIONS, ONLY : spatial1
   !spacetime1
   USE AL_C, ONLY       : deltaz,             & !cell thickness
      esoila,             & !Evap from soil surface (m/s)
   !PRI,                & !unit no for ASCII results
      qvsv,               & !vertical subsurface flow (m/s)
      vspsi,              & !psi
      vsthe,              & !moisture content
      zvspsl                !phreatic surface elevation (m)
   USE AL_D, ONLY       : bexcm,              & !IS CONTAMINANT ON?
      bexsy,              & !IS SEDIMENT ON?
      cstore,             & !canopy storage (mm)
      dxin, dyin,         & !internode spacings for full grid
      einta,              & !Evap from intercepted canopy water (m/s/)
      epot,               & !potential evap (m/s)
      erza,               & !transpiration (m/s)
      sd                    !snowpack depth (mm)
   USE AL_G, ONLY       : icmref, icmxy, nx, ny !grid size and indices, total no of elements
   USE SGLOBAL, ONLY       : DIRQQ, shever, ROOTDIR, hdf5filename, uznow,  &
      planfile=>visualisation_plan_filename, &
      checkfile=>visualisation_check_filename !catchment directory name
   USE CONT_CC, ONLY    : cccc,               & !rel conc in dynamic region (-)
      nnncon=>ncon,       & !number of contaminants
      ssss                  !rel conc in dead space (-)
   USE CONST_SY, ONLY   : rhosed                !density of sediment (kg/m^3)
   USE SED_CS, ONLY     : dls,                & !total depth of sediment (m)
      gnu,                & !rate of ground surface erosion (m/s)
      nnnsed=>nsed,       & !number of sediments
      qsed                  !sediment discharge rate (m/s)
   USE OCmod2, ONLY     : hrfzz !GETHRF
   IMPLICIT NONE
   INTEGER, PARAMETER :: east=1        !! SHETRAN east face number.
   INTEGER, PARAMETER :: north=2       !! SHETRAN north face number.
   INTEGER, PARAMETER :: west=3        !! SHETRAN west face number.
   INTEGER, PARAMETER :: south=4       !! SHETRAN south face number.
   INTEGER, PARAMETER :: i_not_exist=-1 !! Integer missing-value sentinel.
   REAL, PARAMETER    :: zero=0.0      !! Real zero constant.
   REAL, PARAMETER    :: half=0.5      !! Half factor used for grid-cell widths.
   REAL, PARAMETER    :: r_not_exist=-1.0 !! Real missing-value sentinel.
   REAL, PARAMETER    :: m_to_mm     = 1000.0             !! Conversion from metres to millimetres.
   REAL, PARAMETER    :: ps_to_ph    = 3600.0             !! Conversion from per-second to per-hour rates.
   REAL, PARAMETER    :: ps_to_pd    = 24.0    * ps_to_ph !! Conversion from per-second to per-day rates.
   REAL, PARAMETER    :: mps_to_mmph = m_to_mm * ps_to_ph !! Conversion from m/s to mm/hour.
   REAL, PARAMETER    :: mps_to_mmpd = m_to_mm * ps_to_pd !! Conversion from m/s to mm/day.
   PRIVATE
   PUBLIC :: BAL_ERR,         BANK_NO,       BANK_WIDTH,                                &
      CAN_STOR,        C_C_DR,        C_C_DS,     CELL_THICKNESS,                &
      DRAINAGE,                                                                  &
      ELEMENT,                        ELEMENT_DX, ELEMENT_DY, EXISTS,            &
      GET_NCON_EARLY, GET_NSED_EARLY, GRID_DX,    GRID_DY,    GRID_NX, GRID_NY,  &
      INT_EVAP,       IS_BANK,        IS_LINK,    IS_SQUARE,                     &
      NET_RAIN,       NO_EL,          NO_CON,     NO_SED,                        &
      OVR_FLOW,                                                                  &
      PH_DEPTH,       POT_EVAP,       PSI,                                       &
      RIVER_NO,       RIVER_WIDTH,                                               &
      S_DIS,          S_ELEVATION,    SNOW_DEP,   SOIL_TYPE,  SRF_DEP,           &
      SRF_EVAP,       S_T_DP,         S_V_ER,                                    &
      THETA,          TOP_CELL,       TRNSP,                                     &
      VERSION,        V_FLOW !,                                                    &
   !spatial1,     SPACE_TIME1
   PUBLIC :: DIRQQ, ROOTDIR, north, east, south, west, hdf5filename, planfile, checkfile, etype, ADJACENT_ELEMENT

CONTAINS
! Legacy perturbation accessor retained here as commented reference.
!ELEMENTAL REAL FUNCTION space_time1(iel) RESULT(r)
!INTEGER, INTENT(IN) :: iel
!INTEGER :: i
!i = INT(uznow+1.0)
!r = spacetime1(iel,i)
!END FUNCTION space_time1

!> Returns the neighbouring element across a SHETRAN face.
   ELEMENTAL INTEGER FUNCTION adjacent_element(iel, face) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Source element number.
      INTEGER, INTENT(IN) :: face !! SHETRAN face number.
      r = ICMREF(iel, face + 4)
   END FUNCTION adjacent_element
!> Returns the opposite face number for the neighbouring element.
   ELEMENTAL INTEGER FUNCTION adjacent_face(iel, face) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Source element number.
      INTEGER, INTENT(IN) :: face !! SHETRAN face number.
      r = ICMREF(iel, face + 8)
   END FUNCTION adjacent_face
!> Returns the water-balance error for an element.
   ELEMENTAL REAL FUNCTION bal_err(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      r = wberr(iel)
   END FUNCTION bal_err
!> Returns the bank element adjacent to a subunit face, or `i_not_exist`.
   ELEMENTAL INTEGER FUNCTION bank_no(su, face) RESULT(r)
      INTEGER, INTENT(IN) :: su !! Subunit element number.
      INTEGER, INTENT(IN) :: face !! SHETRAN face number.
      INTEGER             :: adj !! Adjacent element across `face`.
      IF(.NOT.IS_SQUARE(su)) THEN
         r = i_not_exist
      ELSE
         adj = ADJACENT_ELEMENT(su,face)
         IF(IS_BANK(adj)) THEN
            r=adj
         ELSE
            r = i_not_exist
         ENDIF
      ENDIF
   END FUNCTION bank_no
!> Returns the visualisation width assigned to a bank face.
   ELEMENTAL REAL FUNCTION bank_width(bk, face) RESULT(r)
      INTEGER, INTENT(IN) :: bk !! Bank element number.
      INTEGER, INTENT(IN) :: face !! SHETRAN face number.
      IF(EXISTS(bk)) THEN
         IF(ANY(face==(/east,west/))) THEN
            r = dxqq(bk)
         ELSE
            r = dyqq(bk)
         ENDIF
      ENDIF
   END FUNCTION bank_width
!> Returns canopy storage for an element.
   ELEMENTAL REAL FUNCTION can_stor(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      r = cstore(iel)
   END FUNCTION can_stor
!> Returns contaminant concentration in the soil dynamic region.
   ELEMENTAL REAL FUNCTION c_c_dr(iel, ilay, ncon) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Cell-layer number.
      INTEGER, INTENT(IN) :: ncon !! Contaminant group number.
      r = cccc(iel, ilay, ncon)
   END FUNCTION c_c_dr
!> Returns contaminant concentration in the soil dead-space region.
   ELEMENTAL REAL FUNCTION c_c_ds(iel, ilay, ncon) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Cell-layer number.
      INTEGER, INTENT(IN) :: ncon !! Contaminant group number.
      r = ssss(iel, ilay, ncon)
   END FUNCTION c_c_ds
!> Returns vertical cell thickness for an element and SHETRAN cell layer.
   ELEMENTAL REAL FUNCTION cell_thickness(iel, j) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      INTEGER, INTENT(IN) :: j !! Cell-layer number.
!INTEGER             :: kk !nett 090805
      IF(EXISTS(iel)) THEN
         r = DELTAZ(j,iel)
      ELSE
         r=r_not_exist
      ENDIF
   END FUNCTION cell_thickness
!> Returns canopy drainage converted to millimetres per hour.
   ELEMENTAL REAL FUNCTION drainage(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      r = mps_to_mmph*draina(iel)
   END FUNCTION drainage
!> Returns east-west grid-cell width from internode spacings.
   ELEMENTAL REAL FUNCTION dxx(i) RESULT(r)
!grid cell widths E-W
      INTEGER, INTENT(IN) :: i !! X grid index.
      IF(i==1) THEN
         r = dxin(1)
      ELSEIF(i==nx) THEN
         r = dxin(nx-1)  !altered compared to original  (-1 added)
      ELSE
         r = (dxin(i-1) + dxin(i)) * 0.5
      ENDIF
   END FUNCTION dxx
!> Returns north-south grid-cell width from internode spacings.
   ELEMENTAL REAL FUNCTION dyy(i) RESULT(r)
!grid cell widths N-S
      INTEGER, INTENT(IN) :: i !! Y grid index.
      IF(i==1) THEN
         r = dyin(1)
      ELSEIF(i==ny) THEN
         r = dyin(ny-1)  !altered compared to original  (-1 added)
      ELSE
         r = (dyin(i-1) + dyin(i)) * 0.5
      ENDIF
   END FUNCTION dyy
!> Returns the SHETRAN element number at grid coordinates.
   ELEMENTAL INTEGER FUNCTION element(i,j) RESULT(r)
      INTEGER, INTENT(IN) :: i !! X grid index.
      INTEGER, INTENT(IN) :: j !! Y grid index.
      r = icmxy(i,j)
   END FUNCTION element
!> Returns element width in the x direction.
   ELEMENTAL REAL FUNCTION element_dx(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = dxqq(iel)
   END FUNCTION element_dx
!> Returns element width in the y direction.
   ELEMENTAL REAL FUNCTION element_dy(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = dyqq(iel)
   END FUNCTION element_dy
!> Returns the SHETRAN element type code used by the visualisation interface.
   ELEMENTAL INTEGER FUNCTION etype(iel) RESULT(r)
!element type: 999 gridsquare ;  1,2 bank ; 3 link
      INTEGER, INTENT(IN) :: iel !! Element number, or zero for no element.
      IF(iel/=0) THEN
         r = icmref(iel, 1)
      ELSE
         r = 0
      ENDIF
   END FUNCTION etype
!> Returns whether an element or index exists.
   ELEMENTAL LOGICAL FUNCTION exists(i) RESULT(r)
      INTEGER, INTENT(IN) :: i !! Element or index value to test.
      r = i>0
   END FUNCTION exists
!> Reads the contaminant count early from the contaminant input file.
!>
!> Scans the open contaminant file `cmd` for record `CM3`, reads `nnncon`, then
!> rewinds the file for normal input processing. Missing or unreadable `CM3`
!> records are fatal.
   SUBROUTINE get_ncon_early()
      CHARACTER(4)  :: dd   !! Candidate record tag read from the contaminant file.
      CHARACTER(64) :: mess !! Error message assembled before calling `ERROR`.
      DO
         READ(cmd,'(A)', ERR=90, END=90) dd
         IF(DD(2:4)=='CM3') THEN
            READ(cmd,*, ERR=91) nnncon
            EXIT
         ENDIF
      ENDDO
      REWIND(cmd)
      RETURN
90    mess='failed to find line :CM3 in contaminant data file' ; GOTO 1000
91    mess='failed to read NCON '
1000  mess = 'GET_NCON_EARLY '//TRIM(mess)
      CALL ERROR(FFFATAL, 1, PPPRI, 0, 0,  mess)
   END SUBROUTINE get_ncon_early
!> Reads the sediment-size count early from the sediment input file.
!>
!> Scans the open sediment file `syd` for record `SY11`, reads `nnnsed`, then
!> rewinds the file for normal input processing. Missing or unreadable `SY11`
!> records are fatal.
   SUBROUTINE get_nsed_early()
      CHARACTER(5)  :: dd   !! Candidate record tag read from the sediment file.
      CHARACTER(64) :: mess !! Error message assembled before calling `ERROR`.
      DO
         READ(syd,'(A)', END=90) dd
         IF(DD(2:5)=='SY11') THEN
            READ(syd,*, ERR=91) nnnsed
            EXIT
         ENDIF
      ENDDO
      REWIND(syd)
      RETURN
90    mess='failed to find line :SY11 in sediment data file' ; GOTO 1000
91    mess='failed to read NSED '
1000  mess = 'GET_NSED_EARLY '//TRIM(mess)
      CALL ERROR(FFFATAL, 1, PPPRI, 0, 0,  mess)
   END SUBROUTINE get_nsed_early
!> Returns grid-cell width in the x direction.
   ELEMENTAL REAL FUNCTION grid_dx(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! X grid index.
      r = DXX(iel)
   END FUNCTION grid_dx
!> Returns grid-cell width in the y direction.
   ELEMENTAL REAL FUNCTION grid_dy(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Y grid index.
      r = DYY(iel)
   END FUNCTION grid_dy
!> Returns the number of grid cells in the x direction.
   PURE INTEGER FUNCTION grid_nx() RESULT(r)
      r = nx
   END FUNCTION grid_nx
!> Returns the number of grid cells in the y direction.
   PURE INTEGER FUNCTION grid_ny() RESULT(r)
      r = ny
   END FUNCTION grid_ny
!> Returns intercepted-canopy evaporation converted to millimetres per hour.
   ELEMENTAL REAL FUNCTION int_evap(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*einta(iel)
   END FUNCTION int_evap
!> Returns whether an element is a bank element.
   ELEMENTAL LOGICAL FUNCTION is_bank(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      INTEGER             :: typ !! Element type from [[etype]].
      typ = ETYPE(iel)
      r   = typ==1 .OR. typ==2
   END FUNCTION is_bank
!> Returns whether an element is a river-link element.
   ELEMENTAL LOGICAL FUNCTION is_link(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      INTEGER             :: typ !! Element type from [[etype]].
      typ = ETYPE(iel)
      r   = typ==3
   END FUNCTION is_link
!> Returns whether an element is a grid-square subunit.
   ELEMENTAL LOGICAL FUNCTION is_square(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      INTEGER             :: typ !! Element type from [[etype]].
      typ = ETYPE(iel)
      r   = typ==0
   END FUNCTION is_square
!> Returns net rainfall converted to millimetres per hour.
   ELEMENTAL REAL FUNCTION net_rain(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*pnetto(iel)
   END FUNCTION net_rain
!> Returns the number of contaminants available for visualisation.
   PURE INTEGER FUNCTION no_con() RESULT(r)
      r = nnncon
   END FUNCTION no_con
!> Returns the total number of SHETRAN elements.
   INTEGER FUNCTION no_el() RESULT(r)
      r = total_no_elements
   END FUNCTION no_el
!> Returns the number of sediment fractions available for visualisation.
   PURE INTEGER FUNCTION no_sed() RESULT(r)
      r = nnnsed
   END FUNCTION no_sed
!> Returns overland flow on an element face.
   ELEMENTAL REAL FUNCTION ovr_flow(iel, face) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: face !! SHETRAN face number.
      r = qoc(iel,face)
   END FUNCTION ovr_flow
!> Returns phreatic depth below ground surface.
   ELEMENTAL REAL FUNCTION ph_depth(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = zgrund(iel)-zvspsl(iel)
   END FUNCTION ph_depth
!> Returns potential evapotranspiration converted to millimetres per hour.
   ELEMENTAL REAL FUNCTION pot_evap(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*epot(iel)
   END FUNCTION pot_evap
!> Returns soil-water pressure head for an element and cell layer.
   ELEMENTAL REAL FUNCTION psi(iel, ilay) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Cell-layer number.
      r = r_not_exist
      r = vspsi(ilay,iel)
   END FUNCTION psi
!> Returns the river-link element adjacent to a subunit face, or `i_not_exist`.
   ELEMENTAL INTEGER FUNCTION river_no(su, face) RESULT(r)
      INTEGER, INTENT(IN) :: su   !! Subunit element number.
      INTEGER, INTENT(IN) :: face !! SHETRAN face number.
      INTEGER             :: adj  !! Adjacent element across `face`.
      IF(.NOT.IS_SQUARE(su)) THEN
         r = i_not_exist
      ELSE
         adj = ADJACENT_ELEMENT(su,face)
         IF(IS_LINK(adj)) THEN
            r = adj
         ELSEIF(IS_BANK(adj)) THEN
            r = ADJACENT_ELEMENT(adj,face)
         ELSE
            r = i_not_exist
         ENDIF
      ENDIF
   END FUNCTION river_no
!> Returns the visualisation width assigned to a river link.
   ELEMENTAL REAL FUNCTION river_width(ir) RESULT(r)
      INTEGER, INTENT(IN) :: ir !! River-link element number.
      IF(EXISTS(ir)) THEN
         r = cwidth(ir)
      ELSE
         r = i_not_exist
      ENDIF
   END FUNCTION river_width
!> Returns sediment discharge for an element face and sediment fraction.
   ELEMENTAL REAL FUNCTION s_dis(iel, face, nsed) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: face !! SHETRAN face number.
      INTEGER, INTENT(IN) :: nsed !! Sediment fraction number.
      r = rhosed*qsed(iel, nsed,face)
   END FUNCTION s_dis
!> Returns ground-surface elevation for an element.
   ELEMENTAL REAL FUNCTION s_elevation(iel) RESULT(r)
!surface elevation
      INTEGER, INTENT(IN) :: iel !! Element number.
      IF(iel>0) THEN
         r =ZGRUND(iel)
      ELSE
         r = r_not_exist
      ENDIF
   END FUNCTION s_elevation
!> Returns snowpack depth for an element.
   ELEMENTAL REAL FUNCTION snow_dep(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = sd(iel)
   END FUNCTION snow_dep
!> Returns the soil type at an element and cell layer.
!>
!> Link elements return zero. For non-link elements, `ilay` is a SHETRAN cell
!> layer, not a soil-layer number; the function maps it through `NLYRBT` before
!> returning `NTSOIL`.
   ELEMENTAL INTEGER FUNCTION soil_type(iel, ilay) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Cell-layer number, not soil-layer number.
      INTEGER             :: j    !! Soil-layer index found from `NLYRBT`.
      IF(IS_LINK(iel)) THEN
         r = 0
      ELSEIF(ANY(nlyrbt(iel,:)/=0)) THEN
         j = 1
         DO WHILE (ilay >= nlyrbt(iel,j+1))
            j = j + 1
         ENDDO
         r = ntsoil(iel,j)
      ELSE
         r = 0
      ENDIF
   END FUNCTION soil_type
!> Returns surface-water depth above ground surface.
   ELEMENTAL REAL FUNCTION srf_dep(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
!r = GEThrf(iel)-zgrund(iel)  !eliminate ELEMENTAL in GETHRF
      r = hrfzz(iel)-zgrund(iel)
   END FUNCTION srf_dep
!> Returns soil-surface evaporation converted to millimetres per hour.
   ELEMENTAL REAL FUNCTION srf_evap(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*esoila(iel)
   END FUNCTION srf_evap
!> Returns total sediment depth converted to millimetres.
   ELEMENTAL REAL FUNCTION s_t_dp(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = m_to_mm*dls(iel)
   END FUNCTION s_t_dp
!> Returns ground-surface erosion rate converted to millimetres per day.
   ELEMENTAL REAL FUNCTION s_v_er(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmpd*gnu(iel)  !note is mm per day
   END FUNCTION s_v_er
!> Returns volumetric soil-water content for an element and cell layer.
   ELEMENTAL REAL FUNCTION theta(iel, ilay) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Cell-layer number.
      r = vsthe(ilay,iel)
   END FUNCTION theta
!> Returns the top active SHETRAN cell layer number.
   PURE INTEGER FUNCTION top_cell() RESULT(r)
      r = top_cell_no
   END FUNCTION top_cell
!> Returns transpiration converted to millimetres per hour.
   ELEMENTAL REAL FUNCTION trnsp(iel) RESULT(r)
      INTEGER, INTENT(IN) :: iel !! Element number.
      r = mps_to_mmph*erza(iel)
   END FUNCTION trnsp
!> Returns the integer SHETRAN version number.
   PURE INTEGER FUNCTION version() RESULT(r)
      r = INT(shever)
   END FUNCTION version
!> Returns vertical subsurface flow for an element and cell layer.
   ELEMENTAL REAL FUNCTION v_flow(iel, ilay) RESULT(r)
      INTEGER, INTENT(IN) :: iel  !! Element number.
      INTEGER, INTENT(IN) :: ilay !! Cell-layer number.
      r = qvsv(ilay, iel)
   END FUNCTION v_flow
END MODULE visualisation_interface_left
