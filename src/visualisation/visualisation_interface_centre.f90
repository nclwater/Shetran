!> @brief Catalogues visualisation outputs and translates them into SHETRAN values.
!>
!> This is the central layer between the SHETRAN 4 solver and the visualisation
!> metadata/writer modules descended from the SHEGRAPH 2 interface. The private
!> `outtype` catalogue contains eight static entries numbered -7 through 0 and
!> 44 dynamic entries numbered 1 through 44. Twenty dynamic entries are marked
!> as implemented. The metadata setup exposes those implemented dynamic entries
!> in the check file and rejects plan requests for the others; all eight static
!> entries are registered without consulting their `implemented` flag.
!>
!> Each catalogue entry describes its plan-file name, display title and units,
!> spatial/value type, optional extra axis, and any layer, sediment-fraction, or
!> contaminant dependence. [[get_output_type]] returns a caller-owned copy of
!> the requested catalogue half. [[shetran_integer_data]] and
!> [[shetran_real_data]] then dispatch supported names to accessors imported
!> exclusively through [[visualisation_interface_left]]. [[shetran_layer]]
!> reverses the top-down visualisation layer order into the solver's bottom-up
!> order.
!>
!> `output_type%typ` identifies the member scope and value kind:
!>
!> | Code | Value kind | Members represented |
!> |:-----|:-----------|:--------------------|
!> | `B` | Real | Banks. |
!> | `E` | Integer | Banks. |
!> | `F` | Integer | River/link segments. |
!> | `G` | Real | Compounds. |
!> | `I` | Integer | Gridsquares. |
!> | `L` | Real | River/link segments. |
!> | `M` | Real | Gridsquares. |
!> | `N` | Integer | Compounds. |
!>
!> These are catalogue defaults. For a dynamic plan item, the metadata layer
!> can refine the code to match a grid/list basis and an all/squares/banks/rivers
!> scope before the metadata layer appends the second-character `S` consumed
!> by the fill dispatch.
!>
!> A compound contains one gridsquare and, where present, its four banks and
!> four river/link segments. `extra_dimensions` is `-`, `faces`, `left_right`,
!> or `X_Y`. Stored face values are ordered north, east, south, west, but the
!> right-hand interface converts them to the solver constants east=1, north=2,
!> west=3, south=4 before calling the accessors in this module.
!>
!> Catalogue maintenance requires coordinated edits:
!>
!> | Change | Required companion work |
!> |:-------|:------------------------|
!> | Add or rename an output | Amend `outtype`; use non-positive numbers for static and positive numbers for dynamic entries. |
!> | Extend either catalogue range | Keep `first_type` and `last_type` equal to the constructor bounds. |
!> | Mark an entry implemented | Add its integer or real dispatcher branch and expose raw state through the left interface. |
!> | Change an axis or member scope | Recheck metadata dimensions and the right-interface iteration/remapping. |
!>
!> This module also re-exports the topology queries, face constants, file names,
!> dimensions, version strings, and sediment/contaminant counts needed by the
!> right-hand interface, allowing that consumer to depend on this central API.
!>
!> @warning
!> Three current catalogue/accessor discrepancies are retained. `spatial1` is
!> registered as an implemented static real output although its accessor is
!> commented out, so it receives `HUGE(zero)`. `snow_dep` is labelled `m` even
!> though [[visualisation_interface_left:SNOW_DEP]] returns the model's
!> millimetre-valued snow depth without conversion. For `centroid`'s north-south
!> coordinate, [[shetran_real_data]] selects the current row thickness with
!> `GRID_DY(ix)` rather than `GRID_DY(iy)`; non-uniform grids can therefore
!> receive an incorrect coordinate or an out-of-bounds lookup.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07-19 | JE | 2.0 | Created the central SHETRAN 4/SHEGRAPH 2 interface and output catalogue. |
!> | 2019-11-28 | SB | - | Imported the standard visualisation interface into the current source history. |
!> | 2026-04-04 | SvB | - | Applied the project-wide Fortran formatting pass without changing behavior. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the obsolete Intel `REAL:4` directive during the IFX compiler update. |
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
      hdf5filename, planfile, checkfile

   USE MOD_PARAMETERS, ONLY : I_P
   USE MOD_ERROR, ONLY : errstat_alloc

   IMPLICIT NONE

   INTEGER, PARAMETER :: first_type=-7 !! Lower catalogue bound; static entries occupy `first_type:0`.
   INTEGER, PARAMETER :: last_type=44  !! Upper catalogue bound; dynamic entries occupy `1:last_type`.
   INTEGER, PARAMETER :: csz = 70      !! Character length used by visualisation metadata consumers.
   REAL, PARAMETER    :: zero=0.0      !! Default-real zero used to obtain the unsupported-value sentinel.
   REAL, PARAMETER    :: half=0.5      !! Factor used for centroids and shared river radial spans.
   LOGICAL, PARAMETER :: T=.TRUE.      !! Compact true value used in the catalogue constructor.
   LOGICAL, PARAMETER :: F=.FALSE.     !! Compact false value used in the catalogue constructor.

!> Describes one static or dynamic visualisation output.
   TYPE output_type
      INTEGER        :: number                     !! Non-positive static or positive dynamic catalogue number.
      CHARACTER(8)   :: name                       !! Lowercase selector used in the visualisation plan and dispatchers.
      CHARACTER(70)  :: title                      !! Human-readable plot, printout, and HDF5 title.
      CHARACTER(8)   :: units                      !! Display-unit label stored with the output.
      CHARACTER      :: typ                        !! Member-scope and integer/real code documented above.
      CHARACTER(11)  :: extra_dimensions           !! Additional axis: `-`, `faces`, `left_right`, or `X_Y`.
      LOGICAL        :: varies_with_elevation      !! Whether the output has a vertical-layer dimension.
      LOGICAL        :: varies_with_sediment_no    !! Whether the output has a sediment-fraction dimension.
      LOGICAL        :: varies_with_contaminant_no !! Whether the output has a contaminant dimension.
      LOGICAL        :: implemented                !! Whether a dynamic entry is offered for selection by current metadata setup.

   END TYPE output_type

!> Complete immutable output catalogue. Consumers receive allocated copies
!> through [[get_output_type]] rather than direct access to this private array.
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
   PRIVATE
   PUBLIC :: OUTPUT_TYPE, GET_OUTPUT_TYPE, SHETRAN_INTEGER_DATA, SHETRAN_REAL_DATA, csz, DIRQQ,       &
      BANK_NO, ELEMENT, GRID_NX, GRID_NY, RIVER_NO, TOP_CELL, north, east, south, west, EXISTS, &
      IS_SQUARE, IS_BANK, IS_LINK, NO_EL, NO_SED, NO_CON, VERSION, ROOTDIR, SHETRAN_LAYER,      &
      hdf5filename, planfile, checkfile

CONTAINS

!> Evaluates an integer-valued visualisation selector.
!>
!> Only two catalogue selectors currently return integers:
!>
!> | `name` | Required indices | Returned value |
!> |:-------|:-----------------|:---------------|
!> | `number` | `iel` | The SHETRAN element number. |
!> | `soil_typ` | `iel`, `ilay` | [[visualisation_interface_left:SOIL_TYPE]] for that element and solver layer. |
!>
!> All index arguments are optional because this function shares the real
!> dispatcher's generic calling pattern. The current right-hand interface
!> always supplies `ext`, even for entries with no extra axis. This is required
!> in practice because the function copies `ext` to two legacy locals before
!> selecting `name`. It must also supply every index required by the selected
!> row above. The remaining indices and the two copied locals are unused by the
!> current integer branches.
!>
!> @warning
!> No `PRESENT` or bounds checks are performed. Omitting `ext` or a required
!> selector index is invalid. An unsupported or case-mismatched name returns
!> `HUGE(0)` as a sentinel rather than reporting an error.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07-19 | JE | 2.0 | Added integer dispatch for element numbers and soil types. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION shetran_integer_data(name, iel, ix, iy, ilay, ext, nsed, ncon) RESULT(r)
      CHARACTER(*), INTENT(IN)      :: name !! Exact `output_type%name` selector, without catalogue padding.
      INTEGER, INTENT(IN), OPTIONAL :: iel  !! SHETRAN element number in `1:NO_EL()` when required.
      INTEGER, INTENT(IN), OPTIONAL :: ix   !! Grid x index; unused by the current integer selectors.
      INTEGER, INTENT(IN), OPTIONAL :: iy   !! Grid y index; unused by the current integer selectors.
      INTEGER, INTENT(IN), OPTIONAL :: ilay !! Solver layer number, bottom-up, when required.
      INTEGER, INTENT(IN), OPTIONAL :: ext  !! Extra index; required by the common call contract despite being unused after copying.
      INTEGER, INTENT(IN), OPTIONAL :: nsed !! Sediment-fraction index; unused by the current integer selectors.
      INTEGER, INTENT(IN), OPTIONAL :: ncon !! Contaminant index; unused by the current integer selectors.
      INTEGER                       :: face !! Legacy copy of `ext`; unused by current integer branches.
      INTEGER                       :: direction !! Legacy copy of `ext`; unused by current integer branches.
      face      = ext
      direction = ext
      SELECT CASE(name)
       CASE('number')   ; r = iel
       CASE('soil_typ') ; r = SOIL_TYPE(iel, ilay)
       CASE DEFAULT     ; r = HUGE(0)
      END SELECT
   END FUNCTION shetran_integer_data

!> Evaluates a real-valued visualisation selector.
!>
!> Exact lowercase `name` matching selects one of the implemented real
!> catalogue entries. Most values are supplied by same-named accessors in
!> [[visualisation_interface_left]]; the locally derived geometry is:
!>
!> | Selector | Returned value or derivation |
!> |:---------|:-----------------------------|
!> | `grid_dxy` | `GRID_DX(ix)` for `ext=1`, or `GRID_DY(iy)` for `ext=2`. |
!> | `net_rain`, `pot_evap`, `trnsp` | Corresponding element water-flux accessor. |
!> | `srf_evap`, `int_evap`, `drainage` | Corresponding element water-flux accessor. |
!> | `can_stor`, `snow_dep`, `ph_depth`, `srf_dep` | Corresponding element storage/depth accessor. |
!> | `v_flow`, `psi`, `theta` | Corresponding element-and-layer accessor. |
!> | `ovr_flow` | Element flow through internal SHETRAN face `ext`. |
!> | `s_t_dp`, `s_v_er`, `bal_err` | Corresponding element sediment or balance accessor. |
!> | `s_dis` | Sediment discharge for element, internal face, and fraction `nsed`. |
!> | `c_c_dr`, `c_c_ds` | Dynamic-region or dead-space relative concentration for element, layer, and contaminant `ncon`. |
!> | `vert_thk`, `surf_elv` | Cell thickness for element/layer, or element surface elevation. |
!> | `r_span` | Bank width; half a shared river width; or half the gridsquare width normal to internal face `ext`. |
!> | `centroid` | Half the current grid interval plus the intervals preceding it from the west or north model edge. |
!>
!> The caller translates stored face order N/E/S/W into the solver constants
!> E=1, N=2, W=3, S=4 before passing `ext`. For `X_Y`, `ext=1` selects the
!> east-west/x value and `ext=2` the north-south/y value. A `left_right` axis
!> would use 1 for left and 2 for right, although no such entry is implemented.
!>
!> All numeric arguments are optional solely to support the shared dispatch
!> signature. `ext` is nevertheless required on every current call because it
!> is copied before dispatch. Each branch additionally requires the element,
!> grid, layer, sediment, or contaminant indices shown above. Metadata setup is
!> responsible for producing valid combinations.
!>
!> @warning
!> No `PRESENT`, range, or direction-default checks are made. Missing required
!> indices are invalid; invalid `ext` values in the nested geometry selections
!> can leave the result or a temporary undefined. Unsupported and
!> case-mismatched names return `HUGE(zero)`. The module-level warning records
!> the existing `snow_dep` unit mismatch and y-centroid indexing defect.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07-19 | JE | 2.0 | Added real dispatch and derived grid, centroid, and radial-span geometry. |
!> @endhistory
   ELEMENTAL REAL FUNCTION shetran_real_data(name, iel, ix, iy, ilay, ext, nsed, ncon) RESULT(r)
      CHARACTER(*), INTENT(IN)      :: name !! Exact `output_type%name` selector, without catalogue padding.
      INTEGER, INTENT(IN), OPTIONAL :: iel  !! SHETRAN element number in `1:NO_EL()` when required.
      INTEGER, INTENT(IN), OPTIONAL :: ix   !! Grid x index when required by `grid_dxy` or `centroid`.
      INTEGER, INTENT(IN), OPTIONAL :: iy   !! Grid y index when required by `grid_dxy` or `centroid`.
      INTEGER, INTENT(IN), OPTIONAL :: ilay !! Solver layer number, bottom-up, for layer-dependent outputs.
      INTEGER, INTENT(IN), OPTIONAL :: ext  !! Internal face or x/y selector; required by the common call contract.
      INTEGER, INTENT(IN), OPTIONAL :: nsed !! Sediment-fraction number for `s_dis`.
      INTEGER, INTENT(IN), OPTIONAL :: ncon !! Contaminant number for `c_c_dr` and `c_c_ds`.
      INTEGER                       :: ii !! Implied-DO index used to construct ranges for centroid sums.
      INTEGER                       :: face !! Copy of `ext` interpreted as an internal solver face.
      INTEGER                       :: direction !! Copy of `ext` interpreted as an x/y direction.
      REAL                          :: dx !! Current grid interval used by `centroid`.
      REAL                          :: asumdx !! Sum of intervals before the requested centroid.
      REAL                          :: dum !! Full gridsquare width normal to the requested radial face.

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
            r = half * RIVER_WIDTH(iel)  ! The river is shared with a neighbouring gridsquare.
         ELSE
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
       CASE DEFAULT     ; r = HUGE(zero)
      END SELECT
   END FUNCTION shetran_real_data


!> Allocates and returns a copy of one half of the output catalogue.
!>
!> Exact selector `static` allocates `r(first_type:0)` and copies all eight
!> non-positive entries. Exact selector `dynamic` allocates `r(1:last_type)`
!> and copies all 44 positive entries, including those whose `implemented`
!> component is false. Because the private parameter catalogue is copied, a
!> caller may modify the returned records without altering later queries. The
!> caller owns the target and must deallocate it; the current right-hand
!> interface does so after registering each subset.
!>
!> @warning
!> A value other than exact lowercase `static` or `dynamic` executes no branch,
!> leaving the pointer result with undefined association status. Allocation
!> failure is not handled.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07-19 | JE | 2.0 | Added allocation and copying of the static and dynamic catalogue ranges. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   FUNCTION get_output_type(text)  RESULT(r)
      TYPE(OUTPUT_TYPE), DIMENSION(:), POINTER :: r    !! Newly allocated copy of the selected catalogue range.
      CHARACTER(*), INTENT(IN)                 :: text !! Exact subset selector: `static` or `dynamic`.
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_INTERFACE_CENTRE:get_output_type"
      SELECT CASE(text)
       CASE('static')
         ALLOCATE(r(first_type:0), STAT=ios)
         CALL errstat_alloc(ios, "r", location)
         r = outtype(first_type:0)
       CASE('dynamic')
         ALLOCATE(r(1:last_type), STAT=ios)
         CALL errstat_alloc(ios, "r", location)
         r = outtype(1:last_type)
      END SELECT
   END FUNCTION get_output_type



!> Converts a top-down visualisation layer number to a solver cell-layer number.
!>
!> The visualisation/HDF5 convention numbers the top cell as 1, whereas the
!> solver numbers layers from the bottom and exposes the top active index as
!> `TOP_CELL()`. The conversion is therefore
!>
!> \[
!> r = TOP\_CELL() - sgv2layer + 1 .
!> \]
!>
!> Applying the same expression again converts in the opposite direction.
!> Valid input is `1:TOP_CELL()`; no bounds check is performed, so values
!> outside that range produce corresponding out-of-range indices.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07-19 | JE | 2.0 | Added reversal between SHEGRAPH and SHETRAN vertical numbering. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION shetran_layer(sgv2layer) RESULT(r)
      INTEGER, INTENT(IN) :: sgv2layer !! Top-down visualisation layer number.
      r = TOP_CELL() - sgv2layer + 1
   END FUNCTION shetran_layer

END MODULE visualisation_interface_centre
