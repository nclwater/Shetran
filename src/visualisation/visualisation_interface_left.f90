!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_interface_left
!DEC$ DEFINE V=4 !compiler directive (set to 3 for Version 3 and to 4 for version 4)
!DEC$ REAL:4
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
!DEC$ IF(V==4)
USE AL_C, ONLY       : deltaz,             & !cell thickness
                       esoila,             & !Evap from soil surface (m/s)
                       !PRI,                & !unit no for ASCII results
                       qvsv,               & !vertical subsurface flow (m/s)
                       vspsi,              & !psi
                       vsthe,              & !moisture content
                       zvspsl                !phreatic surface elevation (m)
!DEC$ ELSEIF(V==3)
USE AL_C, ONLY       : ddz,                & !cell thickness
                       hsz,                & !phreatic head ? (m)
                       th3                   !3D moisture content
!DEC$ ENDIF
USE AL_D, ONLY       : bexcm,              & !IS CONTAMINANT ON?
                       bexsy,              & !IS SEDIMENT ON?
                       cstore,             & !canopy storage (mm)
                       dxin, dyin,         & !internode spacings for full grid
                       einta,              & !Evap from interecpted canopy water (m/s/)
                       epot,               & !potential evap (m/s)
                       erza,               & !transpiration (m/s)
                       sd                    !snowpack depth (mm)
!DEC$ IF(V==3)
USE AL_D, ONLY       : esoila,             & !Evap from soil surface (m/s)#
                       PRI,                & !unit no for ASCII results
                       psi3,               & !3D head
                       thuz                  !vertical velocity at top of uz (???) (m/s)
!DEC$ ENDIF
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
!!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
!ELEMENTAL REAL FUNCTION space_time1(iel) RESULT(r)
!INTEGER, INTENT(IN) :: iel
!INTEGER :: i
!i = INT(uznow+1.0)
!r = spacetime1(iel,i)
!END FUNCTION space_time1

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION adjacent_element(iel, face) RESULT(r)
INTEGER, INTENT(IN) :: iel, face
r = ICMREF(iel, face + 4)
END FUNCTION adjacent_element
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION adjacent_face(iel, face) RESULT(r)
INTEGER, INTENT(IN) :: iel, face
r = ICMREF(iel, face + 8)
END FUNCTION adjacent_face
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION bal_err(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element
r = wberr(iel)
END FUNCTION bal_err
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION bank_no(su, face) RESULT(r)
INTEGER, INTENT(IN) :: su, face
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
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION bank_width(bk, face) RESULT(r)
INTEGER, INTENT(IN) :: bk, face
IF(EXISTS(bk)) THEN
    IF(ANY(face==(/east,west/))) THEN
        r = dxqq(bk)
    ELSE
        r = dyqq(bk)
    ENDIF
ENDIF
END FUNCTION bank_width
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION can_stor(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = cstore(iel)
END FUNCTION can_stor
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION c_c_dr(iel, ilay, ncon) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay, ncon  !element, layer and contaminant group no
r = cccc(iel, ilay, ncon)
END FUNCTION c_c_dr
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION c_c_ds(iel, ilay, ncon) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay, ncon  !element, layer and contaminant group no
r = ssss(iel, ilay, ncon)
END FUNCTION c_c_ds
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION cell_thickness(iel, j) RESULT(r)
INTEGER, INTENT(IN) :: iel, j
!INTEGER             :: kk !nett 090805
IF(EXISTS(iel)) THEN
!DEC$ IF(v==4)
    r = DELTAZ(j,iel)
!DEC$ ELSEIF(V==3)
    kk = NVC(iel)  !nett 090805
    IF(kk>0) THEN  !nett 090805
        r = DDZ(j,kk)
    ELSE
        r = zero
    ENDIF
!DEC$ ENDIF
ELSE
    r=r_not_exist
ENDIF
END FUNCTION cell_thickness
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION drainage(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*draina(iel)
END FUNCTION drainage
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
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
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
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
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION element(i,j) RESULT(r)
INTEGER, INTENT(IN) :: i,j
r = icmxy(i,j)
END FUNCTION element
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION element_dx(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
r = dxqq(iel)
END FUNCTION element_dx
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION element_dy(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
r = dyqq(iel)
END FUNCTION element_dy
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION etype(iel) RESULT(r)
!element type: 999 gridsquare ;  1,2 bank ; 3 link
INTEGER, INTENT(IN) :: iel
IF(iel/=0) THEN
    r = icmref(iel, 1)
ELSE
    r = 0
ENDIF
END FUNCTION etype
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL LOGICAL FUNCTION exists(i) RESULT(r)
INTEGER, INTENT(IN) :: i
r = i>0
END FUNCTION exists
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
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
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
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
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION grid_dx(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
r = DXX(iel)
END FUNCTION grid_dx
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION grid_dy(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
r = DYY(iel)
END FUNCTION grid_dy
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION grid_nx() RESULT(r)
r = nx
END FUNCTION grid_nx
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION grid_ny() RESULT(r)
r = ny
END FUNCTION grid_ny
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION int_evap(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*einta(iel)
END FUNCTION int_evap
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL LOGICAL FUNCTION is_bank(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
INTEGER             :: typ
typ = ETYPE(iel)
r   = typ==1 .OR. typ==2
END FUNCTION is_bank
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL LOGICAL FUNCTION is_link(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
INTEGER             :: typ
typ = ETYPE(iel)
r   = typ==3
END FUNCTION is_link
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL LOGICAL FUNCTION is_square(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel
INTEGER             :: typ
typ = ETYPE(iel)
r   = typ==0
END FUNCTION is_square
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION net_rain(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*pnetto(iel)
END FUNCTION net_rain
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION no_con() RESULT(r)
r = nnncon
END FUNCTION no_con
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
INTEGER FUNCTION no_el() RESULT(r)
r = total_no_elements
END FUNCTION no_el
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION no_sed() RESULT(r)
r = nnnsed
END FUNCTION no_sed
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION ovr_flow(iel, face) RESULT(r)
INTEGER, INTENT(IN) :: iel, face  !element no and face no
r = qoc(iel,face)
END FUNCTION ovr_flow
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION ph_depth(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
!DEC$ IF(V==4)
r = zgrund(iel)-zvspsl(iel)
!DEC$ ELSEIF(V==3)
r = zgrund(iel) - hsz(iel)
!DEC$ ENDIF
END FUNCTION ph_depth
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION pot_evap(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*epot(iel)
END FUNCTION pot_evap
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION psi(iel, ilay) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay  !element no, cell layer no.
!DEC$ IF(V==4)
r = vspsi(ilay,iel)
!DEC$ ELSEIF(V==3)
r = psi3(iel,ilay)
!DEC$ ENDIF
END FUNCTION psi
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
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
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION river_width(ir) RESULT(r)
INTEGER, INTENT(IN) :: ir
IF(EXISTS(ir)) THEN
    r = cwidth(ir)
ELSE
    r = i_not_exist
ENDIF
END FUNCTION river_width
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION s_dis(iel, face, nsed) RESULT(r)
INTEGER, INTENT(IN) :: iel, face, nsed  !element, face and sediment group no
r = rhosed*qsed(iel, nsed,face)
END FUNCTION s_dis
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION s_elevation(iel) RESULT(r)
!surface elavation
INTEGER, INTENT(IN) :: iel
IF(iel>0) THEN
    r =ZGRUND(iel)
ELSE
    r = r_not_exist
ENDIF
END FUNCTION s_elevation
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION snow_dep(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = sd(iel)
END FUNCTION snow_dep
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
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
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION srf_dep(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
!r = GEThrf(iel)-zgrund(iel)  !eliminate ELEMENTAL in GETHRF
r = hrfzz(iel)-zgrund(iel)
END FUNCTION srf_dep
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION srf_evap(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*esoila(iel)
END FUNCTION srf_evap
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION s_t_dp(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = m_to_mm*dls(iel)
END FUNCTION s_t_dp
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION s_v_er(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmpd*gnu(iel)  !note is mm per day
END FUNCTION s_v_er
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION theta(iel, ilay) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay  !element no, cell layer no.
!DEC$ IF(V==4)
r = vsthe(ilay,iel)
!DEC$ ELSEIF(V==3)
r = th3(iel,ilay)
!DEC$ ENDIF
END FUNCTION theta
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION top_cell() RESULT(r)
r = top_cell_no
END FUNCTION top_cell
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION trnsp(iel) RESULT(r)
INTEGER, INTENT(IN) :: iel  !element no
r = mps_to_mmph*erza(iel)
END FUNCTION trnsp
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION version() RESULT(r)
r = INT(shever)
END FUNCTION version
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION v_flow(iel, ilay) RESULT(r)
INTEGER, INTENT(IN) :: iel, ilay  !element no and layer no
!DEC$ IF(V==4)
r = qvsv(ilay, iel)
!DEC$ ELSEIF(V==3)
r = thuz(iel)  !does not vary with ilay
!DEC$ ENDIF
END FUNCTION v_flow
END MODULE visualisation_interface_left