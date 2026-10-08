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
!> declaration initializers, and every [[mod_error:RAISE_ERROR]] call clears both
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

   USE MOD_PARAMETERS, ONLY: I_P, R8P, LENGTH_FILEPATH

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
   INTEGER(KIND=I_P), PARAMETER :: NOLEE = 2*LLEE !! Maximum contaminant column-overlap entries.
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
   ! Written by [[mod_error:RAISE_ERROR]] and consumed by [[rest:TMSTEP]]. They stay
   ! here rather than in [[mod_error]] so that `mod_error` can use `sglobal`
   ! without a circular dependency.
   LOGICAL :: flag_runtime_reduction_errors !! Latest `ERROR` call requested timestep reduction for error 1024 or 1030.
   LOGICAL :: flag_runtime_reduction_e1060 !! Latest `ERROR` call requested the separate timestep reduction for error 1060.

   ! --------------------------------------------------------------------
   ! Run Mode Flags
   ! --------------------------------------------------------------------
   LOGICAL :: error_mode !! State of command-line option `-error`; suppresses the interactive wait in [[mod_error:ERR_STOP]].

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

END MODULE sglobal
