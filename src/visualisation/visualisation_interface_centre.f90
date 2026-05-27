!> summary: Central translation layer for visualisation output variables.
!>
!> This module defines the visualisation output catalogue and translates named
!> visualisation variables into SHETRAN integer or real values. Static output
!> types have non-positive type numbers, dynamic output types have positive
!> numbers, and the catalogue records whether each variable varies by elevation,
!> sediment fraction, contaminant, or extra face/direction dimension.
!>
!> The visualisation plan and HDF5 metadata use compass-order face dimensions
!> (`N`, `E`, `S`, `W`) as described in the manual. The SHETRAN core uses its
!> internal face numbering in the accessors below, and the right-hand
!> visualisation interface remaps between the two orders before data are stored.
!>
!> Catalogue maintenance:
!>
!> | Step | Requirement |
!> |:-----|:------------|
!> | Add or rename a variable | Amend `outtype`; non-positive `number` values are static and positive values are dynamic. |
!> | Change catalogue bounds | Keep `first_type` and `last_type` aligned with the `outtype` constructor bounds. |
!> | Add implemented data | Add the corresponding dispatch branch in [[shetran_integer_data]] or [[shetran_real_data]]. |
!> | Access raw SHETRAN state | Route access through [[visualisation_interface_left]]. |
!> | Preserve public contract | Keep this module private except for the explicit `PUBLIC` list; keep [[get_output_type]] stable. |
!>
!> `outtype%typ` identifies the spatial object and value kind:
!>
!> | Code | Values returned for |
!> |:-----|:--------------------|
!> | `B` | Real bank values. |
!> | `E` | Integer bank values. |
!> | `F` | Integer river/link values. |
!> | `G` | Real compound values. |
!> | `I` | Integer gridsquare values. |
!> | `L` | Real river/link values. |
!> | `M` | Real gridsquare values. |
!> | `N` | Integer compound values. |
!>
!> A compound is one gridsquare plus, where present, its four banks and four
!> river/link segments.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 20190704 | JE | 2.0 | Created central SHETRAN v4/SHEGRAPH v2 interface. |
!> @endhistory
MODULE visualisation_interface_centre

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

   INTEGER, PARAMETER :: first_type=-7 !! Lower `outtype` bound; non-positive entries are static.
   INTEGER, PARAMETER :: last_type=44  !! Upper `outtype` bound; positive entries are dynamic.
   INTEGER, PARAMETER :: csz = 70      !! Character length used for visualisation titles.
   REAL, PARAMETER    :: zero=0.0      !! Real zero sentinel used when requesting `HUGE`.
   REAL, PARAMETER    :: half=0.5      !! Half factor used for widths and centroids.
   LOGICAL, PARAMETER :: T=.TRUE.      !! Short true value for compact `outtype` constructors.
   LOGICAL, PARAMETER :: F=.FALSE.     !! Short false value for compact `outtype` constructors.

!> Metadata for one visualisation output variable.
   TYPE output_type
      INTEGER        :: number                  !! Static/dynamic catalogue number.
      CHARACTER(8)   :: name                    !! Name used in the visualisation plan.
      CHARACTER(70)  :: title                   !! Plot and printout title.
      CHARACTER(8)   :: units                   !! Display units for plots and printouts.
      CHARACTER      :: typ                     !! Spatial object and value-kind code.
      CHARACTER(11)  :: extra_dimensions        !! Extra axis: `-`, `faces`, `left_right`, or `X_Y`.
      LOGICAL        :: varies_with_elevation   !! True when values vary by subsurface layer as well as plan location.
      LOGICAL        :: varies_with_sediment_no !! True when values vary by sediment fraction number.
      LOGICAL        :: varies_with_contaminant_no !! True when values vary by contaminant number.
      LOGICAL        :: implemented             !! True when an accessor branch currently supplies the variable.

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
      OUTPUT_TYPE(10, 'snow_dep', 'Snow pack depth                                              ', 'mm      ', 'G', '-   ', F, F, F, T), &
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




!> Evaluates an integer-valued SHETRAN visualisation variable.
!>
!> The optional indices identify either element/grid position, vertical layer,
!> extra face or direction, sediment fraction, and contaminant number depending
!> on the variable metadata. `ext` is copied before dispatch, so current callers
!> must supply it even when the selected integer branch does not use a face or
!> direction. Other optional indices are required only by the selected `name`.
!> Unsupported names return `HUGE(0)`.
!>
!> Extra-dimension conventions:
!>
!> | `extra_dimensions` | `ext` convention |
!> |:-------------------|:-----------------|
!> | `faces` | SHETRAN internal faces: 1 east, 2 north, 3 west, 4 south. |
!> | `X_Y` | 1 east-west coordinate/width, 2 north-south coordinate/width. |
!> | `left_right` | 1 left, 2 right. |
   ELEMENTAL INTEGER FUNCTION shetran_integer_data(name, iel, ix, iy, ilay, ext, nsed, ncon) RESULT(r)
      CHARACTER(*), INTENT(IN)       :: name !! `outtype%name` selector.
      INTEGER, INTENT(IN), OPTIONAL  :: iel  !! SHETRAN element number, numbered `1:NEL`.
      INTEGER, INTENT(IN), OPTIONAL  :: ix   !! X coordinate on the model grid.
      INTEGER, INTENT(IN), OPTIONAL  :: iy   !! Y coordinate on the model grid.
      INTEGER, INTENT(IN), OPTIONAL  :: ilay !! SHETRAN layer number; the top active layer is `LL`.
      INTEGER, INTENT(IN), OPTIONAL  :: ext  !! Extra face or direction index.
      INTEGER, INTENT(IN), OPTIONAL  :: nsed !! Sediment fraction number, where applicable.
      INTEGER, INTENT(IN), OPTIONAL  :: ncon !! Contaminant number, where applicable.
      INTEGER                        :: face !! Copy of `ext` used as a SHETRAN face number.
      INTEGER                        :: direction !! Copy of `ext` used as a non-face direction selector.
      face      = ext
      direction = ext
      SELECT CASE(name)
       CASE('number')   ; r = iel
       CASE('soil_typ') ; r = SOIL_TYPE(iel, ilay)
       CASE DEFAULT     ; r = HUGE(0)
      END SELECT
   END FUNCTION shetran_integer_data

!> Evaluates a real-valued SHETRAN visualisation variable.
!>
!> The dispatch is controlled by `name`, which is matched against the
!> visualisation output catalogue. Units are those advertised in `outtype`.
!> `ext` is copied before dispatch, so current callers must supply it even when
!> the selected branch does not use a face or direction. Other optional indices
!> are required only by the selected catalogue entry. Unsupported names return
!> `HUGE(zero)`.
   ELEMENTAL REAL FUNCTION shetran_real_data(name, iel, ix, iy, ilay, ext, nsed, ncon) RESULT(r)
      CHARACTER(*), INTENT(IN)       :: name !! `outtype%name` selector.
      INTEGER, INTENT(IN), OPTIONAL  :: iel  !! SHETRAN element number, numbered `1:NEL`.
      INTEGER, INTENT(IN), OPTIONAL  :: ix   !! X coordinate on the model grid.
      INTEGER, INTENT(IN), OPTIONAL  :: iy   !! Y coordinate on the model grid.
      INTEGER, INTENT(IN), OPTIONAL  :: ilay !! SHETRAN layer number; the top active layer is `LL`.
      INTEGER, INTENT(IN), OPTIONAL  :: ext  !! Extra face or direction index.
      INTEGER, INTENT(IN), OPTIONAL  :: nsed !! Sediment fraction number, where applicable.
      INTEGER, INTENT(IN), OPTIONAL  :: ncon !! Contaminant number, where applicable.
      INTEGER                        :: ii   !! Loop index for accumulated grid lengths.
      INTEGER                        :: face !! Copy of `ext` used as a SHETRAN face number.
      INTEGER                        :: direction !! Copy of `ext` used as a non-face direction selector.
      REAL                           :: dx   !! Current grid interval used for centroid coordinates.
      REAL                           :: asumdx !! Accumulated grid interval before the current centroid.
      REAL                           :: dum  !! Temporary width selected from element dimensions.

      face      = ext
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


!> Returns the static or dynamic subset of the visualisation output catalogue.
!>
!> `text='static'` returns a newly allocated pointer with bounds
!> `first_type:0`; `text='dynamic'` returns bounds `1:last_type`. The legacy
!> interface expects this allocation and bound behaviour.
   FUNCTION get_output_type(text)  RESULT(r)
      TYPE(OUTPUT_TYPE), DIMENSION(:), POINTER :: r    !! Allocated catalogue subset.
      CHARACTER(*), INTENT(IN)                 :: text !! Catalogue subset selector: `static` or `dynamic`.
      SELECT CASE(text)
       CASE('static')
         ALLOCATE(r(first_type:0))
         r = outtype(first_type:0)
       CASE('dynamic')
         ALLOCATE(r(1:last_type))
         r = outtype(1:last_type)
      END SELECT
   END FUNCTION get_output_type

!> Converts a SHEGRAPH vertical layer number to a SHETRAN cell-layer number.
!>
!> \[
!> r = TOP\_CELL() - sgv2layer + 1
!> \]
   ELEMENTAL INTEGER FUNCTION shetran_layer(sgv2layer) RESULT(r) !vertical layering
      INTEGER, INTENT(IN) :: sgv2layer !! SHEGRAPH vertical layer number.
      r = TOP_CELL() - sgv2layer + 1
   END FUNCTION shetran_layer

END MODULE visualisation_interface_centre
