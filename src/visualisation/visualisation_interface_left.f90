!> summary: Near-SHETRAN visualisation accessor interface.
!>
!> This module is the left-hand side of the SHETRAN/SHEGRAPH visualisation
!> interface. It reads model state from the core SHETRAN modules using native
!> element, face, layer, and grid numbering, converts selected fluxes to
!> plotting units, and exposes small accessor functions for the central
!> visualisation translation layer.
MODULE visualisation_interface_left

!JE for SHEGRAPH Version 2.0 Created July 2004
!JE made common for SV3 and SV4 221104
!This is the left hand (i.e. near-SHETRAN) part of the 
!   interface between SHETRAN Versions 3 and 4 and SHEGRAPH Version 2
!uses SHETRAN numbering, faces and coordinates
!It is the only module of SHEGRAPH Version 2 which accesses the SHETRAN modules
!It is used only by SHEGRAPH Version 2 modules VISUALISATION_INTERFACE_CENTRE

!Ammendment guidance
!1. New functions may have to be added to this module if new variables are added to 
!   the SHEGRAPH Version 2 list in VISUALISATION_INTERFACE_CENTRE
!2. Do not remove any variables and functions functions from the PUBLIC list.
!3. Add new variables or functions to the PUBLIC list if they are needed in VISUALISATION_INTERFACE_CENTRE
!2. Keep this module PRIVATE.
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
                       nvc,                & !vegeration index
                       pnetto,             & !net_rainfall (m/s)
                       qoc,                & !overland flow (m^3/s)
                       syd,                & !file unot for sediments
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
                       einta,              & !Evap from interecpted canopy water (m/s/)
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
INTEGER, PARAMETER :: east=1, north=2, west=3, south=4,   & !SHETRAN face numbering
                      i_not_exist=-1
REAL, PARAMETER    :: zero=0.0, half=0.5, r_not_exist=-1.0, &
                      m_to_mm     = 1000.0,                 &
                      ps_to_ph    = 3600.0,                 &
                      ps_to_pd    = 24.0    * ps_to_ph,     &
                      mps_to_mmph = m_to_mm * ps_to_ph,     &
                      mps_to_mmpd = m_to_mm * ps_to_pd
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
INTEGER             :: adj
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
INTEGER, INTENT(IN) :: iel, ilay, ncon  !! Element, cell layer, and contaminant group numbers.
r = cccc(iel, ilay, ncon)
END FUNCTION c_c_dr
!> Returns contaminant concentration in the soil dead-space region.
ELEMENTAL REAL FUNCTION c_c_ds(iel, ilay, ncon) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay, ncon  !! Element, cell layer, and contaminant group numbers.
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
INTEGER, INTENT(IN) :: i
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
INTEGER, INTENT(IN) :: i
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
INTEGER, INTENT(IN) :: i,j
r = icmxy(i,j)
END FUNCTION element
!> Returns element width in the x direction.
ELEMENTAL REAL FUNCTION element_dx(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
r = dxqq(iel)
END FUNCTION element_dx
!> Returns element width in the y direction.
ELEMENTAL REAL FUNCTION element_dy(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
r = dyqq(iel)
END FUNCTION element_dy
!> Returns the SHETRAN element type code used by the visualisation interface.
ELEMENTAL INTEGER FUNCTION etype(iel) RESULT(r)
!element type: 999 gridsquare ;  1,2 bank ; 3 link
INTEGER, INTENT(IN) :: iel
IF(iel/=0) THEN
    r = icmref(iel, 1)
ELSE
    r = 0
ENDIF
END FUNCTION etype
!> Returns whether an element or index exists.
ELEMENTAL LOGICAL FUNCTION exists(i) RESULT(r)
INTEGER, INTENT(IN) :: i
r = i>0
END FUNCTION exists
!> Reads the contaminant count early from the contaminant input file.
SUBROUTINE get_ncon_early()
CHARACTER(4)  :: dd
CHARACTER(64) :: mess
DO
    READ(cmd,'(A)', ERR=90, END=90) dd
    IF(DD(2:4)=='CM3') THEN
        READ(cmd,*, ERR=91) nnncon
        EXIT
    ENDIF
ENDDO
REWIND(cmd)
RETURN
90 mess='failed to find line :CM3 in contaminant data file' ; GOTO 1000
91 mess='failed to read NCON '
1000 mess = 'GET_NCON_EARLY '//TRIM(mess)
CALL ERROR(FFFATAL, 1, PPPRI, 0, 0,  mess)
END SUBROUTINE get_ncon_early
!> Reads the sediment-size count early from the sediment input file.
SUBROUTINE get_nsed_early()
CHARACTER(5)  :: dd
CHARACTER(64) :: mess
DO
    READ(syd,'(A)', END=90) dd
    IF(DD(2:5)=='SY11') THEN
        READ(syd,*, ERR=91) nnnsed
        EXIT
    ENDIF
ENDDO
REWIND(syd)
RETURN
90 mess='failed to find line :SY11 in sediment data file' ; GOTO 1000
91 mess='failed to read NSED '
1000 mess = 'GET_NSED_EARLY '//TRIM(mess)
CALL ERROR(FFFATAL, 1, PPPRI, 0, 0,  mess)
END SUBROUTINE get_nsed_early
!> Returns grid-cell width in the x direction.
ELEMENTAL REAL FUNCTION grid_dx(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
r = DXX(iel)
END FUNCTION grid_dx
!> Returns grid-cell width in the y direction.
ELEMENTAL REAL FUNCTION grid_dy(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
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
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*einta(iel)
END FUNCTION int_evap
!> Returns whether an element is a bank element.
ELEMENTAL LOGICAL FUNCTION is_bank(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
INTEGER             :: typ
typ = ETYPE(iel)
r   = typ==1 .OR. typ==2
END FUNCTION is_bank
!> Returns whether an element is a river-link element.
ELEMENTAL LOGICAL FUNCTION is_link(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
INTEGER             :: typ
typ = ETYPE(iel)
r   = typ==3
END FUNCTION is_link
!> Returns whether an element is a grid-square subunit.
ELEMENTAL LOGICAL FUNCTION is_square(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
INTEGER             :: typ
typ = ETYPE(iel)
r   = typ==0
END FUNCTION is_square
!> Returns net rainfall converted to millimetres per hour.
ELEMENTAL REAL FUNCTION net_rain(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
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
INTEGER, INTENT(IN) :: iel, face  !element no and face no
r = qoc(iel,face)
END FUNCTION ovr_flow
!> Returns phreatic depth below ground surface.
ELEMENTAL REAL FUNCTION ph_depth(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = zgrund(iel)-zvspsl(iel)
END FUNCTION ph_depth
!> Returns potential evapotranspiration converted to millimetres per hour.
ELEMENTAL REAL FUNCTION pot_evap(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*epot(iel)
END FUNCTION pot_evap
!> Returns soil-water pressure head for an element and cell layer.
ELEMENTAL REAL FUNCTION psi(iel, ilay) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay  !element no, cell layer no.
r = r_not_exist
r = vspsi(ilay,iel)
END FUNCTION psi
!> Returns the river-link element adjacent to a subunit face, or `i_not_exist`.
ELEMENTAL INTEGER FUNCTION river_no(su, face) RESULT(r)
INTEGER, INTENT(IN) :: su, face
INTEGER             :: adj
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
INTEGER, INTENT(IN) :: ir
IF(EXISTS(ir)) THEN
    r = cwidth(ir)
ELSE
    r = i_not_exist
ENDIF
END FUNCTION river_width
!> Returns sediment discharge for an element face and sediment fraction.
ELEMENTAL REAL FUNCTION s_dis(iel, face, nsed) RESULT(r)
INTEGER, INTENT(IN) :: iel, face, nsed  !element, face and sediment group no
r = rhosed*qsed(iel, nsed,face)
END FUNCTION s_dis
!> Returns ground-surface elevation for an element.
ELEMENTAL REAL FUNCTION s_elevation(iel) RESULT(r)
!surface elavation
INTEGER, INTENT(IN) :: iel
IF(iel>0) THEN
    r =ZGRUND(iel)
ELSE
    r = r_not_exist
ENDIF
END FUNCTION s_elevation
!> Returns snowpack depth for an element.
ELEMENTAL REAL FUNCTION snow_dep(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = sd(iel)
END FUNCTION snow_dep
!> Returns the soil type at an element and cell layer.
ELEMENTAL INTEGER FUNCTION soil_type(iel, ilay) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay  !element no, cell layer no. (NB - NOT SOIL LAYER NO)
INTEGER             :: j
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
INTEGER, INTENT(IN) :: iel  !element no
!r = GEThrf(iel)-zgrund(iel)  !eliminate ELEMENTAL in GETHRF
r = hrfzz(iel)-zgrund(iel)
END FUNCTION srf_dep
!> Returns soil-surface evaporation converted to millimetres per hour.
ELEMENTAL REAL FUNCTION srf_evap(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*esoila(iel)
END FUNCTION srf_evap
!> Returns total sediment depth converted to millimetres.
ELEMENTAL REAL FUNCTION s_t_dp(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = m_to_mm*dls(iel)
END FUNCTION s_t_dp
!> Returns ground-surface erosion rate converted to millimetres per day.
ELEMENTAL REAL FUNCTION s_v_er(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmpd*gnu(iel)  !note is mm per day
END FUNCTION s_v_er
!> Returns volumetric soil-water content for an element and cell layer.
ELEMENTAL REAL FUNCTION theta(iel, ilay) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay  !element no, cell layer no.
r = vsthe(ilay,iel)
END FUNCTION theta
!> Returns the top active SHETRAN cell layer number.
PURE INTEGER FUNCTION top_cell() RESULT(r)
r = top_cell_no
END FUNCTION top_cell
!> Returns transpiration converted to millimetres per hour.
ELEMENTAL REAL FUNCTION trnsp(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*erza(iel)
END FUNCTION trnsp
!> Returns the integer SHETRAN version number.
PURE INTEGER FUNCTION version() RESULT(r)
r = INT(shever)
END FUNCTION version
!> Returns vertical subsurface flow for an element and cell layer.
ELEMENTAL REAL FUNCTION v_flow(iel, ilay) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay  !element no and layer no
r = qvsv(ilay, iel)
END FUNCTION v_flow
END MODULE visualisation_interface_left
