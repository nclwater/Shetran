!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_interface_centre
!DEC$ REAL:4
!JE 2.0 190704 Created
!This is the central part of the interface between SHETRAN Version 4 and SHEGRAPH Version 2
!If new variables are to be added to SHEGRAPH Version 2, this module will have to be edited as follows:
!1. Ammend the outtype list as necessary - note that zero and negative numbers correspond to
!   static variables (i.e. constants) and positive numbers correspond to dynamic variables 
!   (i.e. varaibles which cange with time).
!2. Ammend variables first_type and last_type to match the ammended outtype list.
!3. Depending on its type, add code to either SHETRAN_INTEGER_DATA or SHETRAN_REAL_DATA so the new &
!   variable can be evaluated.

!Ammendment guidance
!1. This module and the VISUALISATION_INTERFACE_LEFT modules are the only modules that need be
!   ammended if new variables are added to SHEGRAPH Version 2. 
!2. This module should USE only the moudule VISUALISATION_INTERFACE_LEFT
!3. If raw SHETRAN variables are to be accessed, do this via VISUALISATION_INTERFACE_LEFT in the saem manner 
!   as used for CELL_THICKNESS.
!4. Don't alter GET_OUTPUT_TYPE in any way.
!5. Keep this module PRIVATE and don't add anything else to the PUBLIC list.
USE VISUALISATION_INTERFACE_LEFT, ONLY  : &
         BANK_NO, BANK_WIDTH, CELL_THICKNESS, ELEMENT, ELEMENT_DX, ELEMENT_DY,                     &
         GRID_DX, GRID_DY, GRID_NX, GRID_NY, IS_BANK, IS_LINK, RIVER_WIDTH, RIVER_NO, S_ELEVATION, &
         DIRQQ,                                                                                    &
         SOIL_TYPE, TOP_CELL, north, east, south, west, EXISTS, IS_SQUARE, NO_EL,                  &
         NET_RAIN, POT_EVAP, TRNSP, SRF_EVAP, INT_EVAP,   &
         DRAINAGE, CAN_STOR, V_FLOW, SNOW_DEP, PH_DEPTH,  &
         OVR_FLOW, SRF_DEP, PSI, THETA, S_T_DP,         &
         S_V_ER, S_DIS, C_C_DR, C_C_DS, BAL_ERR, NO_SED, NO_CON, VERSION, ROOTDIR,                 &
         hdf5filename, planfile, checkfile !, &
         !spatial1 , SPACE_TIME1
IMPLICIT NONE

INTEGER, PARAMETER :: first_type=-7, last_type=44  !limits for outtype
INTEGER, PARAMETER :: csz = 70                     !name length
REAL,PARAMETER     :: zero=0.0, half=0.5
LOGICAL, PARAMETER :: T=.TRUE., F=.FALSE.

!In the structure below, the typ component can be:
!B - real for banks
!E - integer for banks
!F - integer for rivers
!G - real for compounds
!I - integer for gridsquares
!L - real for rivers
!M - real for gridsquares
!N - integer for compounds
!A compound is a grouping of a gridsquare and all the banks and rivers segments asociasted with it
!  so a compound has (potentially) 9 parts - one gridsquare, 4 banks and 4 river segments
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE output_type
INTEGER        :: number
CHARACTER(8)   :: name                       !as used in the visualisation input file (called the visualisation plan)
CHARACTER(70)  :: title                      !appears on plots and printouts
CHARACTER(8)   :: units                      !e.g.metres, appears on plots and printouts
CHARACTER      :: typ                        !data type character - see list above
CHARACTER(11)  :: extra_dimensions           !'-', 'faces', 'left_right', or 'X_Y' 
LOGICAL        :: varies_with_elevation,   & !i.e. the variable chnages with depth in the soil, as well as with plan location in the catchmnet
                  varies_with_sediment_no, &
                  varies_with_contaminant_no, &
                  implemented

END TYPE output_type

!DEFAULTS
TYPE(OUTPUT_TYPE), DIMENSION(first_type:last_type), PARAMETER :: outtype = &
  (/OUTPUT_TYPE(-7, 'spatial1', 'Spatial1                                                     ', '-       ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(-6, 'soil_typ', 'Soil type                                                    ', '-       ', 'N', '-   ', T, F, F, T), &
    OUTPUT_TYPE(-5, 'surf_elv', 'Elevation of surface                                         ', 'm       ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(-4, 'vert_thk', 'Cell vertical thickness                                      ', 'm       ', 'G', '-   ', T, F, F, T), &
    OUTPUT_TYPE(-3, 'r_span  ', 'radial spans, measured along radial from gridsquare centroid ', 'm       ', 'G', 'faces',F, F, F, T), &
    OUTPUT_TYPE(-2, 'number  ', 'Index number                                                 ', '-       ', 'N', '-   ', F, F, F, T), &
    OUTPUT_TYPE(-1, 'centroid', 'coordinates of cell centroid                                 ', 'm       ', 'G', 'X_Y',  F, F, F, T), &
    OUTPUT_TYPE(0,  'grid_dxy', 'Grid thicknesses                                             ', 'm       ', 'M', 'X_Y',  F, F, F, T), &
    OUTPUT_TYPE(1,  'net_rain', 'Net rainfall                                                 ', 'mm/hour ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(2,  'pot_evap', 'Potential Evapotranspiration                                 ', 'mm/hour ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(3,  'trnsp   ', 'Transpiration                                                ', 'mm/hour ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(4,  'srf_evap', 'Evaporation from soil surface                                ', 'mm/hour ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(5,  'int_evap', 'Evaporation from intercepted storage                         ', 'mm/hour ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(6,  'drainage', 'Drainage from intercepted storage                            ', 'mm/hour ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(7,  'can_stor', 'Canopy storage                                               ', 'mm      ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(8,  'infilt  ', 'Infiltration                                                 ', 'mm/hour ', 'G', '-   ', F, F, F, F), &
    OUTPUT_TYPE(9,  'v_flow  ', 'Vertical flows                                               ', 'm/s     ', 'G', '-   ', T, F, F, T), &
    OUTPUT_TYPE(10, 'snow_dep', 'Snow pack depth                                              ', 'm       ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(11, 'snow_tmp', 'Temperature of snow pack                                     ', 'deg C   ', 'G', '-   ', F, F, F, F), &
    OUTPUT_TYPE(12, 'ph_depth', 'Phreatic depth below surface                                 ', 'm       ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(13, 'lat_flow', 'Lateral flows                                                ', 'm3/s    ', 'G', 'faces',T, F, F, F), &
    OUTPUT_TYPE(14, 'ovr_flow', 'Overland flow                                                ', 'm3/s    ', 'G', 'faces',F, F, F, T), &
    OUTPUT_TYPE(15, 'srf_dep ', 'Surface water depth                                          ', 'm       ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(16, 'recharge', 'Recharge                                                     ', 'm/s     ', 'G', '-   ', T, F, F, F), &
    OUTPUT_TYPE(17, 'st_aq_fl', 'Stream-aquifer flow                                          ', 'm3/s    ', 'G', '-   ', F, F, F, F), &
    OUTPUT_TYPE(18, 'sp_dis  ', 'Spring discharge                                             ', 'm3/s    ', 'G', '-   ', F, F, F, F), &
    OUTPUT_TYPE(19, 'psi     ', 'Soil water potential                                         ', 'm       ', 'G', '-   ', T, F, F, T), &
    OUTPUT_TYPE(20, 'theta   ', 'Soil water content                                           ', 'm3/m3   ', 'G', '-   ', T, F, F, T), &
    OUTPUT_TYPE(21, 's_t_dp  ', 'Total depth of sediment                                      ', 'mm      ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(22, 's_p_dp  ', 'Depth of sediment in particle size fraction                  ', 'mm      ', 'G', '-   ', F, T, F, F), &
    OUTPUT_TYPE(23, 's_in_d  ', 'Sediment infiltration rate into deep bed layer               ', 'kg/m2/s ', 'L', '-   ', F, T, F, F), &
    OUTPUT_TYPE(24, 's_if_s  ', 'Sediment infiltration rate into bed surface layer            ', 'kg/m2/s ', 'L', '-   ', F, T, F, F), &
    OUTPUT_TYPE(25, 's_v_er  ', 'Rate of ground surface erosion                               ', 'mm/day  ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(26, 's_l_er  ', 'Rate of lateral erosion of each stream bank                  ', 'm/s', 'L', 'left_right', F, F, F, F), &
    OUTPUT_TYPE(27, 's_dis   ', 'Sediment discharge rate                                      ', 'kg/s    ', 'G', 'faces',F, T, F, T), &
    OUTPUT_TYPE(28, 's_n_di  ', 'Net sediment discharge rate                                  ', 'kg/s    ', 'G', '-   ', F, T, F, F), &
    OUTPUT_TYPE(29, 's_dena  ', 'Density of sediments in the active layer                     ', '-       ', 'L', '-   ', F, T, F, F), &
    OUTPUT_TYPE(30, 's_conc  ', 'Concentration of sediment                                    ', 'mg/l    ', 'G', 'faces',F, T, F, F), &
    OUTPUT_TYPE(31, 's_x_dp  ', 'Total cross-sectional area of net sed. deposition            ', 'm2      ', 'L', '-   ', F, F, F, F), &
    OUTPUT_TYPE(32, 'c_c_dr  ', 'Rel. conc. in soil dynamic region                            ', '-       ', 'G', '-   ', T, F, T, T), &
    OUTPUT_TYPE(33, 'c_c_ds  ', 'Rel. conc. in soil dead-space                                ', '-       ', 'G', '-   ', T, F, T, T), &
    OUTPUT_TYPE(34, 'c_c_sw  ', 'Rel. conc. in surface waters                                 ', '-       ', 'G', '-   ', F, F, T, F), &
    OUTPUT_TYPE(35, 'c_c_sl  ', 'Rel. conc. in stream bed surface layer                       ', '-       ', 'L', '-   ', F, F, T, F), &
    OUTPUT_TYPE(36, 'c_c_dl  ', 'Rel. conc. in stream bed deep layer                          ', '-       ', 'L', '-   ', F, F, T, F), &
    OUTPUT_TYPE(37, 'c_c_bs  ', 'Rel. conc. at base of columns                                ', '-       ', 'G', '-   ', F, F, T, F), &
    OUTPUT_TYPE(38, 'c_c_we  ', 'Rel. conc. in well water                                     ', '-       ', 'G', '-   ', F, F, T, F), &
    OUTPUT_TYPE(39, 'c_c_pp  ', 'Rel. conc. in permanent plant material                       ', '-       ', 'G', '-   ', F, F, T, F), &
    OUTPUT_TYPE(40, 'c_c_tp  ', 'Rel. conc. in non-permanent plant material                   ', '-       ', 'G', '-   ', F, F, T, F), &
    OUTPUT_TYPE(41, 'well_t_a', 'Total well abstraction rate                                  ', 'm3/s    ', 'G', '-   ', F, F, F, F), &
    OUTPUT_TYPE(42, 'well_a_s', 'Well abstraction rate for well screen                        ', 'm3/s    ', 'G', '-   ', T, F, F, F), &
    OUTPUT_TYPE(43, 'bal_err ', 'Water mass balance error                                     ', 'm       ', 'G', '-   ', F, F, F, T), &
    OUTPUT_TYPE(44, 'sd_loss ', 'Total soil loss                                              ', 'mm      ', 'G', '-   ', F, F, F, F)/)
    !OUTPUT_TYPE(45, 'spacet1 ', 'Spacetime1                                                   ', '-       ', 'G', '-   ', F, F, F, T)/)

    PRIVATE
    PUBLIC :: OUTPUT_TYPE, GET_OUTPUT_TYPE, SHETRAN_INTEGER_DATA, SHETRAN_REAL_DATA, csz, DIRQQ,       &
!             !pass tyhe following through to righthand side
              BANK_NO, ELEMENT, GRID_NX, GRID_NY, RIVER_NO, TOP_CELL, north, east, south, west, EXISTS, &
              IS_SQUARE, IS_BANK, IS_LINK, NO_EL, NO_SED, NO_CON, VERSION, ROOTDIR, SHETRAN_LAYER,      &
              hdf5filename, planfile, checkfile

CONTAINS




!Evaluate variables for use in SHEGRAPH Version2
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION shetran_integer_data(name, iel, ix, iy, ilay, ext, nsed, ncon) RESULT(r)
!will be passed element no (i.e. iel) or grid coordinates (ix,iy)
INTEGER, INTENT(IN), OPTIONAL :: iel,  &  !SHETRAN element no. (numbering: 1 - NEL)
                                 ix,   &  !x coordinate on grid (grid is NX by NY)
                                 iy,   &  !y coordinate on grid (grid is NX by NY)ilay,
                                 ilay, &  !layer no. (top layer is LL)
                                 ext,  &  !for 'faces'       FACES ARE:       1-E, 2-N, 3-W, 4-S
                                          !for 'X_Y',        DIRECTIONS ARE:  1-E/W, 2-N/S
                                          !for 'left_right', DIRECTIONS ARE:  1-left, 2-right
                                 nsed, ncon
CHARACTER(*), INTENT(IN)      :: name      !corresponds to outtype component
INTEGER                       :: face, direction  !working variables
face      = ext  !face number in SV3 and SV4
direction = ext
SELECT CASE(name)
    CASE('number')   ; r = iel
    CASE('soil_typ') ; r = SOIL_TYPE(iel, ilay)
    CASE DEFAULT     ; r = HUGE(0)
END SELECT
END FUNCTION shetran_integer_data

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION shetran_real_data(name, iel, ix, iy, ilay, ext, nsed, ncon) RESULT(r)
INTEGER, INTENT(IN), OPTIONAL :: iel,  &  !SHETRAN element no. (numbering: 1 - NEL)
                                 ix,   &  !x coordinate on grid (grid is NX by NY)
                                 iy,   &  !y coordinate on grid (grid is NX by NY)
                                 ilay, &  !layer no. (top layer is LL)
                                 ext,  &  !for 'faces'       FACES ARE:       1-E, 2-N, 3-W, 4-S
                                          !for 'X_Y',        DIRECTIONS ARE:  1-E/W, 2-N/S
                                          !for 'left_right', DIRECTIONS ARE:  1-left, 2-right
                                 nsed, ncon
INTEGER                       :: ii, face, direction  !working variables
REAL                          :: dx, asumdx !working variable
REAL                          :: dum       !working variable
CHARACTER(*), INTENT(IN)      :: name      !corresponds to outtype component

face      = ext  !face number in SV3 and SV4
direction = ext
SELECT CASE(name)
    CASE('grid_dxy') 
        SELECT CASE(direction)
        CASE(1) ; r = GRID_DX(ix)
        CASE(2) ; r = GRID_DY(iy)
        END SELECT
    CASE('net_rain') ; r = NET_RAIN(iel)
    CASE('pot_evap') ; r = POT_EVAP(iel)
    CASE('trnsp')    ; r = TRNSP(iel)
    CASE('srf_evap') ; r = SRF_EVAP(iel)
    CASE('int_evap') ; r = INT_EVAP(iel)
    CASE('drainage') ; r = DRAINAGE(iel)
    CASE('can_stor') ; r = CAN_STOR(iel)
    CASE('v_flow')   ; r = V_FLOW(iel,ilay)
    CASE('snow_dep') ; r = SNOW_DEP(iel)
    CASE('ph_depth') ; r = PH_DEPTH(iel)
    CASE('ovr_flow') ; r = OVR_FLOW(iel,face)
    CASE('srf_dep')  ; r = SRF_DEP(iel)
    CASE('psi')      ; r = PSI(iel,ilay)
    CASE('theta')    ; r = THETA(iel,ilay)
    CASE('s_t_dp')   ; r = S_T_DP(iel)
    CASE('s_v_er')   ; r = S_V_ER(iel)
    CASE('s_dis')    ; r = S_DIS(iel, face, nsed)
    CASE('c_c_dr')   ; r = C_C_DR(iel, ilay, ncon)
    CASE('c_c_ds')   ; r = C_C_DS(iel, ilay, ncon)
    CASE('bal_err')  ; r = BAL_ERR(iel)
    CASE('vert_thk') ; r = CELL_THICKNESS(iel,ilay)
    CASE('surf_elv') ; r = S_ELEVATION(iel)
    CASE('r_span')
        IF(IS_BANK(iel)) THEN
            r = BANK_WIDTH(iel,face)
        ELSEIF(IS_LINK(iel)) THEN
            r = half * RIVER_WIDTH(iel)  !river shared with neighbouring gridsquare
        ELSE  !is gridsquare
            SELECT CASE(face)
                CASE(east)  ; dum = ELEMENT_DX(iel)
                CASE(north) ; dum = ELEMENT_DY(iel)
                CASE(west)  ; dum = ELEMENT_DX(iel)
                CASE(south) ; dum = ELEMENT_DY(iel)
            END SELECT
            r = half * dum
        ENDIF
    CASE('centroid')
        SELECT CASE(direction)
        CASE(1) ; dx = GRID_DX(ix) ;  asumdx = SUM(GRID_DX((/(ii, ii=1,ix-1)/)))
        CASE(2) ; dx = GRID_DY(ix) ;  asumdx = SUM(GRID_DY((/(ii, ii=GRID_NY(),iy+1,-1)/)))
        END SELECT
        r = asumdx + half*dx
!    CASE('spatial1') ; r=spatial1(iel)
!    CASE('spacet1')  ; r=SPACE_TIME1(iel)
    CASE DEFAULT     ; r = HUGE(zero)
END SELECT
END FUNCTION shetran_real_data


!DON'T CHANGE THIS
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION get_output_type(text)  RESULT(r)
TYPE(OUTPUT_TYPE), DIMENSION(:), POINTER :: r
CHARACTER(*), INTENT(IN)                 :: text
SELECT CASE(text)
CASE('static')
    ALLOCATE(r(first_type:0))
    r = outtype(first_type:0)
CASE('dynamic')
    ALLOCATE(r(1:last_type))
    r = outtype(1:last_type)
END SELECT
END FUNCTION get_output_type

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION shetran_layer(sgv2layer) RESULT(r) !vertical layering
INTEGER, INTENT(IN) :: sgv2layer
r = TOP_CELL() - sgv2layer + 1
END FUNCTION shetran_layer

END MODULE visualisation_interface_centre