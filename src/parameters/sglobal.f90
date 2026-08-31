!> summary: Global model capacities, run state, numeric constants, and error-reporting state.
!> author: GP; AB/RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> `sglobal` consolidates the former `AL_P`-family include-file state used
!> throughout SHETRAN. It owns compile-time array limits, active catchment
!> dimensions and geometry, model/file identity, and numeric helper constants.
!> The module defaults to `PRIVATE`; only the names in the explicit `PUBLIC`
!> lists form its interface. The imported `I_P`, `R8P`, and `LENGTH_FILEPATH`
!> parameters remain private.
!>
!> Error reporting itself now lives in [[mod_error]]. Only the two
!> timestep-reduction request flags remain here, because `mod_error` uses
!> `sglobal` and the dependency cannot run both ways.
!>
!> Compile-time limits are capacities, not the active problem size. The manual's
!> array-size table describes their configured values; setup must keep every
!> active index within the corresponding capacity.
!>
!> | Capacity group | Parameters | Bounded data |
!> |:---------------|:-----------|:-------------|
!> | Horizontal topology | `NXEE`, `NYEE`, `NLFEE`, `NELEE` | Grid extents, links, and all elements. |
!> | Vertical subsurface | `LLEE`, `NLYREE`, `NSEE`, `NVSEE` | Cells, layer boundaries, soil types, and VSS tables. |
!> | Vegetation and forcing | `NVEE`, `NVBP`, `NUZTAB` | Vegetation/meteorological series, breakpoints, and ET entries. |
!> | Process and output | `NSETEE`, `NOCTAB`, `NSEDEE`, `NCONEE`, `NOLEE` | Result sets and OC, sediment, and contaminant tables. |
!> | Plants, snow, channel tables | `NPLTEE`, `NPELEE`, `max_no_snowmelt_slugs`, `NXSCEE` | Types/slots, slugs, and samples. |
!>
!> | Run state | Initializer or updater | Meaning and lifetime |
!> |:----------|:-----------------------|:---------------------|
!> | `total_no_links`, `total_no_elements` | [[frmod:FRIND]] | Active link and total-element counts; both start at `-1`. |
!> | `top_cell_no` | [[vsmod:VSIN]] | Greatest active VSS cell index; starts at `-1` and is recomputed during VSS setup. |
!> | `DXQQ`, `DYQQ`, `cellarea` | [[frmod:FRDIM]] | Active element plan dimensions and their product after overlap corrections. |
!> | `ZGRUND` | Frame and OC input/setup | Ground-surface elevation for active elements. |
!> | `UZNOW` | [[shetran]], [[frmod:FRINIT]], [[run_sim:SIMULATION]] | Current time [h]; hot start replaces initial zero. |
!> | `DIRQQ`, `CNAM`, `rootdir` | [[getdirqq:get_dir_and_catch]] | Rundata directory, catchment name, and launch directory. |
!> | `filnam` | Command-line setup and [[frmod:FROPEN]] | Rundata path, then mutable `FROPEN` record buffer. |
!> | Visualisation filenames | [[frmod:FROPEN]] | Nonblank records 48--50 provide plan, check, and HDF5 paths. |
!>
!> `marker999` is the end-of-input time sentinel produced by
!> [[utilsmod:FINPUT]] and [[utilsmod:HINPUT]]. The one-element integer and real
!> constants support legacy scalar/array checker interfaces; `vsmall` is the
!> strict absolute tolerance used by the comparison helpers in this module.
!>
!> @note
!> `SHEVER=4.6` is a legacy numeric major/minor value printed in the PRI output
!> and written to the binary results file. It does not encode the full project
!> patch version, currently 4.6.4. `RUNFIL` is still passed to command-line
!> setup as the historical rundata prefix, but that routine does not currently
!> read the argument.
!> @endnote
!>
!> @warning
!> `flag_runtime_reduction_errors` and `flag_runtime_reduction_e1060` have no
!> declaration initializers, and every [[mod_error:ERROR]] call clears both
!> before setting one for error 1024/1030 or 1060. Consequently an intervening
!> error call can erase a pending timestep-reduction request before
!> [[rest:TMSTEP]] consumes it.
!>
!> `EARRAY(1)` is printed for errors 1003 and 1024, but no current assignment to
!> `EARRAY` exists in the source tree. Those numeric diagnostic values are
!> therefore undefined. These current behaviours are documented here rather than
!> changed.
!> @endwarning
!>
!> The retained Monte Carlo names (`szmonte`, `ran2monte1`, `ran2monte2`,
!> `pcmonte`, and `montec`) and `text32` have no current consumers. Their more
!> specific historical meanings cannot be established from the active code.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-02 | GP | 2.0 | Implemented SHE88 on the Newcastle AMDAHL. |
!> | 1989-03 | GP | 2.1 | Added saturated-zone drain-boundary dimensions. |
!> | 1989-04 | GP | 2.2 | Integrated sediment-yield dimensions and the overall version number. |
!> | 1990-03 | GP | 3.0 | Added the development flag and further dimension variables. |
!> | 1992-01 | GP | 3.3 | Added `NVBP` and revised the `NUZTAB` and `NOCTAB` definitions. |
!> | Unknown | GP | 3.4 | Added plant capacities and revised version, banner, and dimension values. |
!> | 1994-09-30 | AB/RAH | 3.4.1 | Standardized explicit declarations, headers, dimensions, and comments. |
!> | 1996-10-24 | GP | 4.0 | Revised capacities and added dimensions for the new VSS module. |
!> | 1997-02-18 | RAH | 4.1 | Removed redundant dimensions and updated version/banner metadata. |
!> | 1998-02-20 | RAH | 4.2 | Updated the version/banner and removed unused dimensions. |
!> | 2004-07 | JE | - | Converted the source to Fortran 95 during SHEGRAPH v2 integration. |
!> | 2009-01 | JE | 4.3.5F90 | Created `sglobal` during the Fortran 90 conversion, replacing `AL_P` and related includes. |
!> | 2026-03-28 | SvB | - | Added selected-kind declarations, explicit visibility, and the initial FORD conversion. |
!> | 2026-03-30 | SB | 4.6.1 | Increased capacities after major multidimensional arrays became allocatable; set `NXOCEE=4*NXEE`. |
!> | 2026-08-20 | SB | - |  remove code for initial error call and sort out helpmessages |
!> | 2026-08-22 | SvB | 4.6.4 | Removed `NXOCEE`; the OC row solver is sized from the active maximum row width established by [[ocmod:ocind]]. |
!> | 2026-08-31 | SvB | - | Moved `ERROR`, `ALSTOP`, and the error-accounting state to [[mod_error]]; renamed the retained timestep-reduction flags. |
!> @endhistory
MODULE sglobal

   USE MOD_PARAMETERS, ONLY : I_P, R8P, LENGTH_FILEPATH

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: SHEVER, BDEVER, BANNER, RUNFIL
   PUBLIC :: nxee, nyee, nlfee, nelee, LLEE, NVEE, NSEE, NVSEE, NVBP, NUZTAB, NLYREE, NSETEE, &
      NOCTAB, NSEDEE, NCONEE, NOLEE, NPLTEE, NPELEE, max_no_snowmelt_slugs, NXSCEE
   PUBLIC :: total_no_elements, total_no_links, top_cell_no, szmonte, ran2monte1, ran2monte2, pcmonte, montec
   PUBLIC :: DIRQQ, filnam, cnam, rootdir, hdf5filename, visualisation_plan_filename, visualisation_check_filename
   PUBLIC :: UZNOW, cellarea, DXQQ, DYQQ, ZGRUND
   PUBLIC :: flag_runtime_reduction_errors, flag_runtime_reduction_e1060
   PUBLIC :: marker999, imarker, izero, ione, izero1, ione1, zero, half, one, two, three, five, vsmall, zero1, one1
   PUBLIC :: EARRAY, text32
   PUBLIC :: eqmarker, gtzero, gezero, ltzero, lezero, iszero, iszero_a, i_iszero_a2, notzero, isone, notone
   PUBLIC :: idimje, dimje
   PUBLIC :: error_mode

   ! --------------------------------------------------------------------
   ! System Version and Banners
   ! --------------------------------------------------------------------

   REAL(KIND=R8P), PARAMETER :: SHEVER = 4.7_R8P !! SHETRAN version number (Major.Minor format).
   LOGICAL, PARAMETER :: BDEVER = .TRUE. !! Development version flag. `.TRUE.` for development, `.FALSE.` for release.
   CHARACTER(*), PARAMETER :: BANNER = 'SHETRAN Hydrological Model' !! Banner for local implementation.
   CHARACTER(*), PARAMETER :: RUNFIL = 'rundata_' !! Base filename for run data files.

   ! --------------------------------------------------------------------
   ! Array Dimensions and Sizing Parameters
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P), PARAMETER :: nxee = 1000 !! Maximum basic-grid extent in the x direction.
   INTEGER(KIND=I_P), PARAMETER :: nyee = 1000 !! Maximum basic-grid extent in the y direction.
   INTEGER(KIND=I_P), PARAMETER :: nlfee = 20000 !! Maximum number of channel links.
   INTEGER(KIND=I_P), PARAMETER :: nelee = 250000 !! Maximum total number of grid, bank, and channel-link elements.
   INTEGER(KIND=I_P), PARAMETER :: LLEE = 50 !! Maximum number of vertical computational cells per element.

   INTEGER(KIND=I_P), PARAMETER :: NVEE = 250000 !! Vegetation/meteorological-series capacity, including rainfall stations.
   INTEGER(KIND=I_P), PARAMETER :: NSEE = 1000 !! Maximum number of soil types.
   INTEGER(KIND=I_P), PARAMETER :: NVSEE = 20 !! Maximum number of entries in VSS lookup and boundary tables.
   INTEGER(KIND=I_P), PARAMETER :: NVBP = 140 !! Maximum time-varying vegetation breakpoints per vegetation type.
   INTEGER(KIND=I_P), PARAMETER :: NUZTAB = 20 !! Maximum PSI, RCF, and FET lookup entries per vegetation type.
   INTEGER(KIND=I_P), PARAMETER :: NLYREE = 20 !! Soil-layer boundary capacity (maximum layers plus one).
   INTEGER(KIND=I_P), PARAMETER :: NSETEE = 45 !! Maximum output sets in legacy binary results metadata.
   INTEGER(KIND=I_P), PARAMETER :: NOCTAB = 20 !! Maximum OC roughness, cross-section, or boundary-table category count.
   INTEGER(KIND=I_P), PARAMETER :: NSEDEE = 7 !! Maximum number of sediment size fractions.
   INTEGER(KIND=I_P), PARAMETER :: NCONEE = 3 !! Maximum number of numeric contaminants.
   INTEGER(KIND=I_P), PARAMETER :: NOLEE = 2 * LLEE !! Maximum contaminant column-overlap entries.
   INTEGER(KIND=I_P), PARAMETER :: NPLTEE = NVEE !! Maximum number of contaminant plant types.
   INTEGER(KIND=I_P), PARAMETER :: NPELEE = 2 !! Maximum number of contaminant plant slots per element.
   INTEGER(KIND=I_P), PARAMETER :: max_no_snowmelt_slugs = 400 !! Maximum stored snowmelt-slug records per element.
   INTEGER(KIND=I_P), PARAMETER :: NXSCEE = 100000 !! Number of samples in each channel cross-section/conveyance table.

   ! --------------------------------------------------------------------
   ! Global Variables
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P) :: total_no_elements = -1 !! Active total number of grid, bank, and channel-link elements.
   INTEGER(KIND=I_P) :: total_no_links = -1 !! Active number of channel links; link elements occupy the first indices.
   INTEGER(KIND=I_P) :: top_cell_no = -1 !! Greatest active VSS cell index across all element columns.
   INTEGER(KIND=I_P) :: szmonte = -1 !! Inactive retained Monte Carlo state; no current consumer.
   INTEGER(KIND=I_P) :: ran2monte1 = -1 !! Inactive retained Monte Carlo state; no current consumer.
   INTEGER(KIND=I_P) :: ran2monte2 = -1 !! Inactive retained Monte Carlo state; no current consumer.
   INTEGER(KIND=I_P) :: pcmonte = -1 !! Inactive retained Monte Carlo state; no current consumer.
   INTEGER(KIND=I_P), DIMENSION(:, :), ALLOCATABLE :: montec !! Inactive Monte Carlo array; never allocated by current code.
   CHARACTER(LEN=LENGTH_FILEPATH) :: DIRQQ !! Parent directory of the selected rundata file; may be `.` or a pathname.
   CHARACTER(LEN=LENGTH_FILEPATH) :: filnam !! Mutable filename buffer used by command-line setup and rundata-record reading.
   CHARACTER(LEN=LENGTH_FILEPATH) :: cnam !! Catchment name derived from the selected rundata filename stem.
   CHARACTER(LEN=LENGTH_FILEPATH) :: rootdir !! Process working directory captured at command-line setup.
   CHARACTER(LEN=LENGTH_FILEPATH) :: hdf5filename !! HDF5 pathname from nonblank rundata record 50.
   CHARACTER(LEN=LENGTH_FILEPATH) :: visualisation_plan_filename !! Visualisation-plan pathname from nonblank rundata record 48.
   CHARACTER(LEN=LENGTH_FILEPATH) :: visualisation_check_filename !! Visualisation-check pathname from nonblank rundata record 49.
   REAL(KIND=R8P) :: UZNOW !! Current simulation time measured from the configured start [h].
   REAL(KIND=R8P), DIMENSION(nelee) :: cellarea !! Plan area of each active element, `DXQQ*DYQQ` [m2].
   REAL(KIND=R8P), DIMENSION(nelee) :: DXQQ !! Corrected x-direction plan dimension of each active element [m].
   REAL(KIND=R8P), DIMENSION(nelee) :: DYQQ !! Corrected y-direction plan dimension of each active element [m].
   REAL(KIND=R8P), DIMENSION(nelee) :: ZGRUND !! Ground-surface elevation of each active element [m].
   ! --------------------------------------------------------------------
   ! Model Timestep Control Flags
   ! --------------------------------------------------------------------
   ! Written by [[mod_error:ERROR]] and consumed by [[rest:TMSTEP]]. They stay
   ! here rather than in [[mod_error]] so that `mod_error` can use `sglobal`
   ! without a circular dependency.
   LOGICAL :: flag_runtime_reduction_errors !! Latest `ERROR` call requested timestep reduction for error 1024 or 1030.
   LOGICAL :: flag_runtime_reduction_e1060 !! Latest `ERROR` call requested the separate timestep reduction for error 1060.

   ! --------------------------------------------------------------------
   ! Run Mode Flags
   ! --------------------------------------------------------------------
   LOGICAL :: error_mode !! State of command-line option `-error`; suppresses the interactive wait in [[mod_error:ALSTOP]].

   ! --------------------------------------------------------------------
   ! Mathematical and Numerical Constants
   ! --------------------------------------------------------------------
   REAL(KIND=R8P), PARAMETER :: marker999 = 999999.9_R8P !! End-of-input time sentinel returned by `FINPUT` and `HINPUT` [h].
   INTEGER(KIND=I_P), PARAMETER :: imarker = INT(marker999) !! Truncated sentinel used internally by `eqmarker`.
   INTEGER(KIND=I_P), PARAMETER :: izero = 0 !! Integer zero constant.
   INTEGER(KIND=I_P), PARAMETER :: ione = 1 !! Integer one constant.
   INTEGER(KIND=I_P), PARAMETER, DIMENSION(1) :: izero1 = [0] !! One-element integer-zero vector for checker calls.
   INTEGER(KIND=I_P), PARAMETER, DIMENSION(1) :: ione1 = [1] !! One-element integer-one vector for checker calls.
   REAL(KIND=R8P), PARAMETER :: zero = 0.0_R8P !! `R8P` zero constant.
   REAL(KIND=R8P), PARAMETER :: half = 0.5_R8P !! `R8P` one-half constant.
   REAL(KIND=R8P), PARAMETER :: one = 1.0_R8P !! `R8P` one constant.
   REAL(KIND=R8P), PARAMETER :: two = 2.0_R8P !! `R8P` two constant.
   REAL(KIND=R8P), PARAMETER :: three = 3.0_R8P !! `R8P` three constant.
   REAL(KIND=R8P), PARAMETER :: five = 5.0_R8P !! `R8P` five constant.
   REAL(KIND=R8P), PARAMETER :: vsmall = 1.0e-20_R8P !! Strict absolute tolerance used by zero/one comparison helpers.
   REAL(KIND=R8P), PARAMETER, DIMENSION(1) :: zero1 = [0.0_R8P] !! One-element `R8P` zero vector for checker calls.
   REAL(KIND=R8P), PARAMETER, DIMENSION(1) :: one1 = [1.0_R8P] !! One-element `R8P` one vector for checker calls.

   ! --------------------------------------------------------------------
   ! Miscellaneous Global Variables
   ! --------------------------------------------------------------------
   REAL(KIND=R8P) :: EARRAY(1) !! Numeric context read for errors 1003/1024; no current producer initializes it.
   CHARACTER(32) :: text32 !! Inactive retained shared text workspace; no current consumer.

CONTAINS



   !> summary: Detects the time-series end marker by integer truncation.
   !>
   !> Returns true when `INT(a)==imarker`, where `imarker` is
   !> `INT(marker999)=999999`. For positive finite input this accepts the entire
   !> half-open interval `999999.0 <= a < 1000000.0`, not only the real marker
   !> value `999999.9`; it performs neither exact real equality nor a tolerance
   !> comparison.
   !>
   !> [[utilsmod:FINPUT]] writes `marker999` after end-of-file, while
   !> [[utilsmod:HINPUT]] writes it after any failed read. [[ocmod:OCEXT]] tests
   !> its head and flux boundary times, and [[vsmod:VSPREP]] tests the well,
   !> lateral-flow, lateral-head, lateral-head-gradient, base-flow, and base-head
   !> times. Each current caller supplies a scalar and raises a fatal error for
   !> the boundary data when this function returns true.
   !>
   !> The scalar dummy makes the `ELEMENTAL` function callable with either a
   !> scalar or an array; an array call returns a conformable logical array. The
   !> function has no side effects.
   !>
   !> @warning
   !> A legitimate time in the accepted integer bucket is indistinguishable
   !> from the marker. Conversion with `INT` also assumes `a` is finite and lies
   !> within the range representable by the default integer kind; the function
   !> performs no guard for unsupported values.
   !> @endwarning
   !>
   !> @note
   !> The pre-FORD source labelled this wrapper “needed for AD” but recorded no
   !> further rationale.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION eqmarker(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate time value [h].
      eqmarker = INT(a)==imarker
   END FUNCTION eqmarker



   !> summary: Tests whether a value is strictly positive.
   !>
   !> Returns the result of `a > 0`. Unlike [[gezero]], this function does not
   !> use the module tolerance `vsmall`: zero and every negative value,
   !> including values within the zero band, return false. An unordered
   !> comparison with a NaN also returns false.
   !>
   !> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
   !> no side effects. Current callers use scalar hydraulic, soil, sediment,
   !> snowmelt, and contaminant values to guard calculations that require a
   !> positive quantity.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION gtzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      gtzero = a>zero
   END FUNCTION gtzero



   !> summary: Tests whether a value is positive or within the zero band.
   !>
   !> Returns `iszero(a) .OR. a > 0`. Because [[iszero]] uses the strict test
   !> `ABS(a) < vsmall`, this is equivalent to `a > -vsmall` for finite input:
   !> small negative values inside the band are accepted, but `-vsmall` is not.
   !> A NaN returns false.
   !>
   !> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
   !> no side effects. Current scalar callers use the result in contaminant and
   !> variably saturated subsurface calculations.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION gezero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      gezero = ISZERO(a) .OR. a>zero
   END FUNCTION gezero



   !> summary: Tests whether a value is strictly negative.
   !>
   !> Returns the result of `a < 0`. Unlike [[lezero]], this function does not
   !> use the module tolerance `vsmall`: zero and every positive value,
   !> including values within the zero band, return false. An unordered
   !> comparison with a NaN also returns false.
   !>
   !> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
   !> no side effects. Current callers use scalar contaminant, ET, snowmelt,
   !> and variably saturated subsurface values to guard negative-value paths.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION ltzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      ltzero = a<zero
   END FUNCTION ltzero



   !> summary: Tests whether a value is negative or within the zero band.
   !>
   !> Returns `iszero(a) .OR. a < 0`. Because [[iszero]] uses the strict test
   !> `ABS(a) < vsmall`, this is equivalent to `a < vsmall` for finite input:
   !> small positive values inside the band are accepted, but `vsmall` is not.
   !> A NaN returns false.
   !>
   !> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
   !> no side effects. Current scalar callers use the result in ET and snowmelt
   !> calculations.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION lezero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      lezero = ISZERO(a) .OR. a<zero
   END FUNCTION lezero



   !> summary: Tests whether a value lies strictly inside the zero band.
   !>
   !> Returns `ABS(a) < vsmall`, where `vsmall` is `1.0e-20_R8P`. The boundary
   !> values `-vsmall` and `vsmall` therefore return false. NaNs and infinities
   !> also return false.
   !>
   !> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
   !> no side effects. This predicate supplies the zero-band semantics used by
   !> [[gezero]], [[lezero]], [[notzero]], and [[iszero_a]], as well as by
   !> numerical checks throughout the simulation modules.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION iszero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      iszero = ABS(a)<vsmall
   END FUNCTION iszero



   !> summary: Tests whether every value in a rank-one array is in the zero band.
   !>
   !> Applies [[iszero]] to each element and returns true only if every element
   !> satisfies `ABS(a(i)) < vsmall`. A zero-size array returns true because the
   !> result is initialized to true and the loop has no iterations.
   !>
   !> [[utilsmod:AREADR]] is the only current caller; it passes the active slice
   !> `AOUT(1:total_no_elements)` so that an all-zero real grid can be printed
   !> compactly. The function is `PURE` and has no side effects.
   !>
   !> @note
   !> After the first false result the loop continues, but the `CYCLE` statement
   !> prevents any further array elements from being evaluated. The retained
   !> source comment labels this control-flow form “FOR AD” without identifying
   !> the automatic-differentiation tool or further rationale.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the array and loop index to selected kinds, made the helper pure, and added the initial FORD block. |
   !> @endhistory
   PURE LOGICAL FUNCTION iszero_a(a)
      INTEGER(KIND=I_P) :: i !! Array index.
      REAL(KIND=R8P), DIMENSION(:), INTENT(IN) :: a !! Values to test against the strict zero band.
      iszero_a=.TRUE.
      DO i=1,SIZE(a)
         IF(.NOT.iszero_a) CYCLE     !FOR AD
         iszero_a = iszero(a(i))
      ENDDO
   END FUNCTION iszero_a



   !> summary: Tests whether every value in a rank-two integer array is zero.
   !>
   !> Uses exact integer equality and returns true only if every `a(i,j)` is
   !> zero. An array with either extent equal to zero returns true because the
   !> result is initialized to true and no element changes it.
   !>
   !> [[utilsmod:AREADI]] is the only current caller; it passes the active slice
   !> `IA(1:NX,1:NY)` so that an all-zero integer grid can be printed compactly.
   !> The function is `PURE` and has no side effects.
   !>
   !> @note
   !> After the first false result the nested loops continue, but the `CYCLE`
   !> statement prevents any further array elements from being evaluated. The
   !> retained source comment labels this control-flow form “FOR AD” without
   !> identifying the automatic-differentiation tool or further rationale.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the array and loop indices to selected kinds, made the helper pure, and added the initial FORD block. |
   !> @endhistory
   PURE LOGICAL FUNCTION i_iszero_a2(a)
      INTEGER(KIND=I_P)                             :: i, j !! Array indices.
      INTEGER(KIND=I_P), DIMENSION(:,:), INTENT(IN) :: a !! Integer values to test for exact zero.
      i_iszero_a2=.TRUE.
      DO i=1,SIZE(a, DIM=1)
         DO j=1,SIZE(a, DIM=2)
            IF(.NOT.i_iszero_a2) CYCLE     !FOR AD
            i_iszero_a2 = a(i,j)==0
         ENDDO
      ENDDO
   END FUNCTION i_iszero_a2



   !> summary: Tests whether a value lies outside the strict zero band.
   !>
   !> Returns the logical complement of [[iszero]]. It is false only when
   !> `ABS(a) < vsmall`; the boundary values `-vsmall` and `vsmall` return true.
   !> Because `iszero` returns false for a NaN, this function returns true for a
   !> NaN.
   !>
   !> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
   !> no side effects. Current callers use scalar values throughout the frame,
   !> ET, OC, VSS, contaminant, and input-utility code.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION notzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      notzero = .NOT.ISZERO(a)
   END FUNCTION notzero



   !> summary: Tests whether a value lies strictly inside the band around one.
   !>
   !> Returns `ABS(a-one) < vsmall`, using the same `1.0e-20_R8P` tolerance as
   !> [[iszero]]. A mathematically exact tolerance boundary is excluded, as are
   !> NaNs and infinities.
   !>
   !> @note
   !> With the current gfortran `R8P` kind, `vsmall` is narrower than the spacing
   !> between representable values around one. For that build this predicate is
   !> therefore true only for the exactly represented value `1.0_R8P`.
   !> @endnote
   !>
   !> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
   !> no side effects. [[vsmod:VSCOEF]] is the only current consumer and uses a
   !> scalar result to select the arithmetic-mean conductivity case.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION isone(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      isone = ABS(a-one)<vsmall
   END FUNCTION isone



   !> summary: Tests whether a value lies outside the strict band around one.
   !>
   !> Returns the logical complement of [[isone]]. A mathematically exact
   !> tolerance boundary returns true. Because `isone` returns false for a NaN,
   !> this function returns true for a NaN.
   !>
   !> @note
   !> With the current gfortran `R8P` kind, `vsmall` is narrower than the spacing
   !> between representable values around one. For that build this predicate is
   !> therefore false only for the exactly represented value `1.0_R8P`.
   !> @endnote
   !>
   !> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
   !> no side effects. [[vsmod:VSCOEF]] is the only current consumer and uses a
   !> scalar result to decide whether exponentiation is required.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL LOGICAL FUNCTION notone(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      notone = .NOT.ISONE(a)
   END FUNCTION notone



   !> summary: Returns the positive part of an integer difference.
   !>
   !> Returns `x-y` when `x > y`, and zero otherwise. As an `ELEMENTAL`
   !> function it accepts scalar or conformable array arguments and has no side
   !> effects. Current scalar calls use it to derive non-fine sediment capacity
   !> in [[symod:SYERR2]] and to round an input allocation extent upward in
   !> [[mod_load_filedata:ALSPRD]].
   !>
   !> @warning
   !> The function result is default integer because the function statement does
   !> not declare `INTEGER(KIND=I_P)`, although both arguments use `I_P`. The
   !> subtraction and conversion are not checked for overflow or an
   !> unrepresentable result if those kinds differ on another compiler.
   !> @endwarning
   !>
   !> @note
   !> The pre-FORD source labelled this wrapper “AD PROBLEM” but recorded no
   !> further rationale.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the arguments to selected integer kind, made the helper elemental, and added the initial FORD block; the default-integer result was retained. |
   !> @endhistory
   ELEMENTAL INTEGER FUNCTION idimje(x,y)
      INTEGER(KIND=I_P), INTENT(IN) :: x !! Minuend.
      INTEGER(KIND=I_P), INTENT(IN) :: y !! Subtrahend.
      IF(x>y) THEN
         idimje = x-y
      ELSE
         idimje = 0
      ENDIF
   END FUNCTION idimje



   !> summary: Returns the positive part of a selected-kind real difference.
   !>
   !> Returns `x-y` when `x > y`, and `0.0_R8P` otherwise. An unordered
   !> comparison, including one involving a NaN, follows the latter branch and
   !> returns zero. The function does not apply `vsmall`.
   !>
   !> The `ELEMENTAL` interface accepts scalar or conformable array arguments
   !> and has no side effects. Current scalar calls truncate negative depth,
   !> stress, concentration, storage, and transport differences in OC and
   !> sediment calculations.
   !>
   !> @note
   !> The pre-FORD source labelled this wrapper “AD PROBLEM” but recorded no
   !> further rationale.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-03-28 | SvB | Converted the result and arguments to selected real kind, made the helper elemental, and added the initial FORD block. |
   !> @endhistory
   ELEMENTAL FUNCTION dimje(x,y)
      REAL(KIND=R8P) :: dimje !! Positive difference.
      REAL(KIND=R8P), INTENT(IN) :: x !! Minuend.
      REAL(KIND=R8P), INTENT(IN) :: y !! Subtrahend.
      IF(x>y) THEN
         dimje = x-y
      ELSE
         dimje = zero
      ENDIF
   END FUNCTION dimje

END MODULE sglobal
