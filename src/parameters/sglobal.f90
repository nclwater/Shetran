!> summary: Global model capacities, run state, numeric constants, and error-reporting state.
!> author: GP; AB/RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> `sglobal` consolidates the former `AL_P`-family include-file state used
!> throughout SHETRAN. It owns compile-time array limits, active catchment
!> dimensions and geometry, model/file identity, numeric helper constants,
!> and the state used by [[sglobal:ERROR]]. The module defaults to `PRIVATE`;
!> only the names in the explicit `PUBLIC` lists form its interface. The
!> imported `I_P`, `R8P`, and `LENGTH_FILEPATH` parameters remain private.
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
!> `ISERROR` and `ISERROR2` have no declaration initializers. The normal
!> [[shetran]] entry path initializes them through `ERROR(-999,...)`, but every
!> later `ERROR` call clears both flags before setting one for error 1024/1030
!> or 1060. Consequently an intervening error call can erase a pending timestep-
!> reduction request before [[rest:TMSTEP]] consumes it.
!>
!> `EARRAY(1)` is printed for errors 1003 and 1024, but no current assignment to
!> `EARRAY` exists in the source tree. Those numeric diagnostic values are
!> therefore undefined. `error_mode` records the command-line `-error` option,
!> but no current routine reads the flag, so it does not alter stop behaviour.
!> These current behaviours are documented here rather than changed.
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
!> | 2026-08-22 | SvB | 4.6.4 | Removed `NXOCEE`; the OC row solver is sized from the active maximum row width established by [[ocmod:ocind]]. |
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
   PUBLIC :: ERRNEE, FFFATAL, EEERR, WWWARN, pppri, ERRC, ERRTOT, helppath, ISERROR, ISERROR2
   PUBLIC :: marker999, imarker, izero, ione, izero1, ione1, zero, half, one, two, three, five, vsmall, zero1, one1
   PUBLIC :: EARRAY, text32
   PUBLIC :: eqmarker, gtzero, gezero, ltzero, lezero, iszero, iszero_a, i_iszero_a2, notzero, isone, notone
   PUBLIC :: idimje, dimje
   PUBLIC :: ERROR, ALSTOP, error_mode

   ! --------------------------------------------------------------------
   ! System Version and Banners
   ! --------------------------------------------------------------------
   REAL(KIND=R8P), PARAMETER :: SHEVER = 4.6_R8P !! Legacy numeric major/minor version written to PRI and RES output.
   LOGICAL, PARAMETER :: BDEVER = .TRUE. !! Selects the development-version label in PRI output.
   CHARACTER(*), PARAMETER :: BANNER = 'SHETRAN Hydrological Model' !! Model banner printed during startup and to PRI output.
   CHARACTER(*), PARAMETER :: RUNFIL = 'rundata_' !! Historical rundata prefix passed to command-line setup; currently unused there.

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
   ! Error Handling Constants and Variables
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P), PARAMETER :: ERRNEE = 100 !! Greatest error-code remainder represented in each module-group counter.
   INTEGER(KIND=I_P), PARAMETER :: FFFATAL = 1 !! Fatal error severity passed to `ERROR`.
   INTEGER(KIND=I_P), PARAMETER :: EEERR = 2 !! Nonfatal error severity passed to `ERROR`.
   INTEGER(KIND=I_P), PARAMETER :: WWWARN = 3 !! Warning severity passed to `ERROR`.
   INTEGER(KIND=I_P), PARAMETER :: pppri = 23 !! Default Fortran unit for primary PRI output.
   INTEGER(KIND=I_P) :: ERRC(0:ERRNEE, 0:3) = 0 !! Occurrence counts by error-code remainder and module group.
   INTEGER(KIND=I_P) :: ERRTOT = 0 !! Total number of errors and warnings recorded by `ERROR`.
   CHARACTER(LEN=LENGTH_FILEPATH) :: helppath !! Help-directory fragment set to `/helpmessages` by each `ERROR` call.
   LOGICAL :: ISERROR !! Latest `ERROR` call requested timestep reduction for error 1024 or 1030.
   LOGICAL :: ISERROR2 !! Latest `ERROR` call requested the separate timestep reduction for error 1060.
   LOGICAL :: error_mode !! State of command-line option `-error`; currently has no consumer.

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



   !> summary: Reports a SHETRAN diagnostic, records it, and terminates fatal runs.
   !>
   !> @author R. A. Heath, Newcastle University
   !>
   !> This is the shared reporter used by 155 active call sites across the
   !> input utilities, process modules, simulation driver, and visualisation
   !> interface. In agreement with User Manual section 1.6.6, ordinary calls
   !> write a numbered diagnostic to a component print unit such as PRI, SPR,
   !> CPR, or MNPR. `IEL` and `CELL` add spatial context when their zero sentinel
   !> is not used, and the module time `UZNOW` supplies the reported time [h].
   !>
   !> Message selectors have the following current behavior:
   !>
   !> | `ETYPE` | Immediate record and accounting | Control behavior |
   !> |:--------|:--------------------------------|:-----------------|
   !> | `FFFATAL=1` | Writes a `FATAL ERROR` header and `TEXT` to `OUT`; increments `ERRTOT` and, for a representable code, `ERRC`. | Prints the summary, then calls [[alstop]] for error termination. |
   !> | `EEERR=2` | Writes an `ERROR` header and `TEXT`; increments the counters as above. | Returns to the caller. |
   !> | `WWWARN=3` | Writes a `WARNING` header and `TEXT`; increments the counters as above. | Returns to the caller. |
   !> | `0` | Writes `TEXT` without a severity header and does not increment either counter. | `ERRNUM=0` would also request a summary; current callers use code 12 only for continuation text from [[mod_load_filedata:ALCHK]] and [[mod_load_filedata:ALCHKI]]. |
   !> | `-999` | Sets `helppath`, clears both timestep flags, and returns before using `OUT` or `TEXT`. | Startup-only initialization call from the main program. |
   !>
   !> Every non-startup call writes `TEXT` to `OUT`, even if `ETYPE` lies
   !> outside zero through three. Only selectors one through three receive a
   !> formatted severity line, and only `FFFATAL` terminates. A zero `IEL`
   !> suppresses both spatial fields; otherwise a zero `CELL` suppresses only
   !> the cell field. Codes 1003 and 1024 append a numeric line read from
   !> `EARRAY(1)`.
   !>
   !> The per-code summary decomposes `ERRNUM` into `AMODL=ERRNUM/1000` and
   !> `ERRN=MOD(ERRNUM,1000)`. `ERRC(0:ERRNEE,0:3)`, with `ERRNEE=100`, can
   !> therefore represent only these inclusive domains:
   !>
   !> | Representable codes | Current component convention |
   !> |:--------------------|:-----------------------------|
   !> | 0000--0100 | General library and input utilities. |
   !> | 1000--1100 | Water-flow components. |
   !> | 2000--2100 | Sediment component. |
   !> | 3000--3100 | Contaminant and nitrate components. |
   !>
   !> `ERRTOT` counts every nonzero-selector call regardless of whether its code
   !> is representable. `ERRC` aggregates by code, not by severity. A fatal call
   !> or any regular call with `ERRNUM=0` writes the summary heading to standard
   !> output and `OUT`, obtains the connected filename for `OUT`, and tells the
   !> user to inspect it. Per-code counts, constructed help paths, help text,
   !> and the final total are then written only to standard output.
   !>
   !> Errors 1024 and 1030 request the stronger timestep reduction through
   !> `ISERROR`; error 1060 requests the separate reduction through `ISERROR2`.
   !> [[rest:TMSTEP]] divides its proposed timestep by 100 or 10 respectively,
   !> subject to a 0.0003 h floor, and clears the flags after consuming them.
   !>
   !> @warning
   !> This routine clears both timestep-reduction flags at the start of every
   !> call. Consequently the flags describe only the most recent diagnostic:
   !> any later unrelated error or warning can erase a pending request before
   !> `TMSTEP` reads it.
   !> @endwarning
   !>
   !> @warning
   !> No current source assignment initializes `EARRAY(1)`, although codes 1003
   !> and 1024 print it as maximum head difference and surface-water depth.
   !> Those appended numeric diagnostics are therefore undefined.
   !> @endwarning
   !>
   !> @warning
   !> Current calls with codes 4820, 4998, 4999, and 2107 lie outside the
   !> representable `ERRC` domains. Their immediate messages, `ERRTOT` increments,
   !> and fatal termination still occur, but their per-code counts and help-file
   !> lookups are omitted from the summary. The manual's numbered component list
   !> is also offset from the zero-through-three code groups used by the routine.
   !> @endwarning
   !>
   !> @warning
   !> Help-message discovery is not operational in the current checkout. The
   !> `-999` path fixes `helpcheck` at 60, so its directory check and prompt can
   !> never run. Summary lookup uses the launch working directory followed by
   !> `/helpmessages`, a backslash, and a four-digit code with no extension;
   !> failed opens are silent, help lines are limited to 80 characters, and no
   !> `helpmessages` directory is present in this repository. Each counted code
   !> also exposes the raw `DIRQQ`, `rootdir`, and constructed filename on
   !> standard output. This differs from
   !> both the manual's “main program directory” description and the old
   !> documentation branch, which used a forward slash, a `.txt` extension, and
   !> copied summary details to `OUT`.
   !> @endwarning
   !>
   !> @note
   !> `ERRCEE`, `PATH1`, `slash`, and `present` are retained but have no effect on
   !> current execution. The startup `-999` call is therefore a state reset, not
   !> a functioning help-path check.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 1994-10-08 | RAH | Created v3.4.1 from v3.4: introduced severity zero, local/range-checked counters, conditional element/cell fields, help summaries, and fatal dispatch through `ALSTOP`. |
   !> | 1997-08-04 | RAH | Restored `EARRAY(1)` output for error 1024 in v4.1. |
   !> | 1997-08-11 | RAH | Added the legacy external declaration after the include block. |
   !> | 2020-07-07 | SB | Added the 1024/1030 and 1060 flags used to reduce the subsequent timestep. |
   !> | 2026-03-28 | SvB | Converted the interface and locals to selected kinds with input intents, replaced the `CTYPE` data statement, and added the initial FORD block. |
   !> | 2026-04-13 | SvB | Replaced labelled summary/help loops and error branches with named loops and `IOSTAT` handling. |
   !> | 2026-05-08 | SB | Reworked summary output to name the selected print file and write the summary heading to both standard output and `OUT`. |
   !> | 2026-05-10 | SvB | Removed the interactive wait before help-file lookup for noninteractive scripted use. |
   !> @endhistory
   SUBROUTINE ERROR(ETYPE, ERRNUM, OUT, IEL, CELL, TEXT)

      IMPLICIT NONE

      ! IO-related parameters and variables
      INTEGER(KIND=I_P), INTENT(IN) :: ETYPE  !! Severity/control selector: 0--3, or startup sentinel -999.
      INTEGER(KIND=I_P), INTENT(IN) :: ERRNUM !! Diagnostic code; zero requests a summary outside the startup path.
      INTEGER(KIND=I_P), INTENT(IN) :: OUT    !! Connected formatted unit receiving the immediate diagnostic.
      INTEGER(KIND=I_P), INTENT(IN) :: IEL    !! Element identifier; zero omits both element and cell fields.
      INTEGER(KIND=I_P), INTENT(IN) :: CELL   !! VSS cell identifier; zero omits the cell field.
      CHARACTER(LEN=*),  INTENT(IN) :: TEXT   !! Immediate diagnostic or continuation text.

      INTEGER(KIND=I_P), PARAMETER :: NONE = 0 !! No-severity selector.
      INTEGER(KIND=I_P), PARAMETER :: ERRCEE = (1 + ERRNEE) * 4 !! Unused retained counter-capacity value.
      INTEGER(KIND=I_P), PARAMETER :: HLP = 8 !! Fixed unit used for an available help file.

      ! Local variables
      CHARACTER(LEN=*), PARAMETER :: PATH1 = '/shetran/' !! Unused retained path fragment.
      CHARACTER(LEN=256) :: FIL, fname !! Constructed help path and name queried for `OUT`.
      CHARACTER(LEN=80)  :: HLPMSG !! One fixed-width help-file line.
      CHARACTER(LEN=1)   :: cc !! Dormant startup-prompt response.
      CHARACTER(LEN=1), PARAMETER :: slash = '/' !! Unused retained path separator.

      INTEGER(KIND=I_P) :: COUNT, ERRN, AMODL !! Summary count, code remainder, and component group.
      INTEGER(KIND=I_P) :: IO_STATUS !! Help-file open/read status.
      INTEGER(KIND=I_P) :: helpcheck !! Fixed startup help-directory status; currently set to 60.

      LOGICAL :: VALID, present !! Counter-index validity and unused startup-presence flag.

      ! Modernization Fix: Replaced legacy DATA statement with a strict PARAMETER array
      CHARACTER(LEN=11), PARAMETER :: CTYPE(3) = ['FATAL ERROR', '      ERROR', '    WARNING'] !! Labels for `ETYPE` 1--3.

      !-------------------------------------------------------------------*

      helppath = '/helpmessages'

      ! SB 07072020 reduce timestep if there are errors 1024,1030,1060
      ISERROR  = .FALSE.
      ISERROR2 = .FALSE.

      IF (ETYPE == -999) THEN
         present = .TRUE.
         helpcheck = 60

         IF (helpcheck == 0) THEN
            PRINT *, "Failed to find the 'helpmessages' directory"
            PRINT *, "  (which contains the help message files)"
            PRINT *, "Its name must be 'helpmessages'"

            ! helpcheck = GETDRIVEDIRQQ (helppath)
            IF (helpcheck /= 0) THEN
               PRINT *, "and it must be in "//TRIM(helppath)
            END IF

            PRINT *, "Type 's' to stop or 'c' to continue"

            ! Intentional bypass by setting cc='c' before the loop
            cc = 'c'
            bypass_loop: DO WHILE (cc /= 'c' .AND. cc /= 's' .AND. cc /= 'C' .AND. cc /= 'S')
               ! cc = GETCHARQQ ()
            END DO bypass_loop

            IF (cc == 's' .OR. cc == 'S') STOP
         END IF
         RETURN
      END IF

      ! Write general error message
      ! ---------------------------
      IF (ETYPE >= 1 .AND. ETYPE <= 3) THEN
         IF (ETYPE == FFFATAL) WRITE(OUT, '(//)')

         IF (IEL == 0) THEN
            WRITE(OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW
         ELSE IF (CELL == 0) THEN
            WRITE(OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW, IEL
         ELSE
            WRITE(OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW, IEL, CELL
         END IF
      END IF

      WRITE(OUT, '(8X,A)') TEXT

      ! Decompose ERRNUM and update counters
      ! ------------------------------------
      IF (ETYPE /= NONE) THEN
         ERRTOT = ERRTOT + 1
         AMODL  = ERRNUM / 1000
         ERRN   = MOD(ERRNUM, 1000)

         VALID  = (AMODL >= 0 .AND. AMODL <= 3 .AND. ERRN >= 0 .AND. ERRN <= ERRNEE)
         IF (VALID) ERRC(ERRN, AMODL) = ERRC(ERRN, AMODL) + 1
      END IF

      ! Write specific error messages
      ! -----------------------------
      IF (ERRNUM == 1003) THEN
         WRITE(OUT, 91003) EARRAY(1)
         ! 970804
      ELSE IF (ERRNUM == 1024) THEN
         WRITE(OUT, 91024) EARRAY(1)
         !
      END IF

      ! SB 07072020 reduce timestep if there are errors 1024,1030,1060
      IF (ERRNUM == 1024 .OR. ERRNUM == 1030) THEN
         ISERROR = .TRUE.
      END IF
      IF (ERRNUM == 1060) THEN
         ISERROR2 = .TRUE.
      END IF

      ! Write summary
      ! -------------
      IF (ETYPE == FFFATAL .OR. ERRNUM == 0) THEN
          WRITE(*,'(/,A,/,A,/)') &
                        ' ### Error Summary and Advice ###', &
                        '     ------------------------'
          WRITE(OUT,'(/,A,/,A,/)') &
                        ' ### Error Summary and Advice ###', &
                        '     ------------------------'
          INQUIRE(OUT, NAME=fname)

          IF (ERRTOT > 0) WRITE(*, '(A,A,A/)') ' ==> Check the pri file: "', trim(fname), '" for more details <=='

         module_loop: DO AMODL = 0, 3
            error_loop: DO ERRN = 0, ERRNEE
               COUNT = ERRC(ERRN, AMODL)

               IF (COUNT > 0) THEN
                  ! Print number of occurrences
                  WRITE(*, 9500) ERRN + AMODL * 1000, COUNT

                  ! Print contents of help file (if any)
                  WRITE(FIL, 9200) TRIM(rootdir) // TRIM(helppath) // '\', AMODL, ERRN
                  PRINT *, dirqq, rootdir
                  PRINT *, FIL

                  OPEN(HLP, FILE=FIL, STATUS='OLD', IOSTAT=IO_STATUS)
                  IF (IO_STATUS == 0) THEN
                     read_help: DO
                        READ(HLP, '(A)', IOSTAT=IO_STATUS) HLPMSG
                        IF (IO_STATUS /= 0) EXIT read_help
                        WRITE(*, '(A)') HLPMSG
                     END DO read_help
                     CLOSE(HLP)
                  END IF

                  WRITE(*, *)

               END IF
            END DO error_loop
         END DO module_loop

         WRITE(*, 9600) ERRTOT
      END IF

      ! Stop?
      ! -----
      IF (ETYPE == FFFATAL) CALL ALSTOP(1)

      ! String format statements
      ! ------------------------
9100  FORMAT(/ ' !!!', A, I5.4, ' at time =', F12.2, ' hours': &
      &        ', iel =', I5:', cell =', I5 )
9200  FORMAT(A,I1,I3.3)

9500  FORMAT(' No. of occurrences of error number',I5.4,' is',I6)
9600  FORMAT(/' ### End of summary: recorded error count is',I7,' ###'/)
91003 FORMAT(' MAXIMUM DIFFERENCE (DHMAX) = ',G12.6,' METRES')
! 970804
91024 FORMAT(' DEPTH OF SURFACE WATER BELOW GROUND = ',G12.6,' METRES')
!
   END SUBROUTINE ERROR



   !> summary: Performs noninteractive termination after a fatal error.
   !>
   !> When `FLAG > 0`, writes a fatal-error message to standard output and then
   !> initiates Fortran error termination with `ERROR STOP`. When `FLAG <= 0`,
   !> it returns without output or other action. [[error]] is the only current
   !> caller and always passes `1` after it has printed the fatal-error summary.
   !>
   !> The current routine never reads from standard input and does not inspect
   !> `error_mode`; the old documentation-branch behavior that conditionally
   !> waited for Enter is obsolete.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 1994-09-17 | RAH | Created the v3.4.1 routine; the dated source note was added on 1994-09-30. |
   !> | 2000-03-07 | SB | Removed the legacy IEEE calls for the v4g-pc version. |
   !> | 2026-03-28 | SvB | Converted `FLAG` to selected integer kind with input intent, replaced the legacy pause with an explicit prompt/read, and added the initial FORD block. |
   !> | 2026-05-08 | SB | Skipped the interactive prompt when `error_mode` (the `-error` command-line flag) was set. |
   !> | 2026-05-10 | SvB | Removed the interactive wait and changed normal `STOP` to `ERROR STOP` for scripted use. |
   !> @endhistory
   SUBROUTINE ALSTOP (FLAG)
      INTEGER(KIND=I_P), INTENT(IN) :: FLAG !! Termination flag; positive requests fatal error termination.

      IF (FLAG.GT.0) THEN
         WRITE(*, '(A)') 'FATAL ERROR: Program will terminate.'
         ERROR STOP 'Program terminating due to fatal error'
      ENDIF
   END SUBROUTINE ALSTOP

END MODULE sglobal
