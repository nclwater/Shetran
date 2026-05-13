!> summary: Defines global parameters, constants, variables, and utility functions.
!> author: J. Ewen, Newcastle University
!>
!> This module is a central component of SHETRAN, replacing legacy `INCLUDE`
!> files like `al_p.inc`. It provides a wide range of globally accessible
!> entities, including:
!>
!> - System version information and banners.
!> - Core array dimensions and size parameters (e.g., `nxee`, `nyee`).
!> - Fundamental mathematical and numerical constants (e.g., `zero`, `vsmall`).
!> - Global variables for simulation state (e.g., `UZNOW`).
!> - Widely used utility functions for numerical comparisons (e.g., `iszero`).
!> - Centralized error handling and reporting routines (`ERROR`, `ALSTOP`).
!>
!> @note This module is monolithic and is a primary candidate for refactoring.
!> The plan is to split it into more focused modules as outlined in
!> `docs/reports/refactor_parameters/plan_parameter_refactor.md`.
!>
!> @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | 1989-02 | GP | v2.0: 'SHE88' implementation on Newcastle Amdahl. |
!> | 1989-03 | GP | v2.1: Add NREFE8 for new SZ drain variables, add NSZBOU. |
!> | 1989-04 | GP | v2.2: Integrate sediment yield, add overall version number. |
!> | 1990-03 | GP | v3.0: Add development version flag and further dimension variables. |
!> | 1992-01 | GP | v3.3: Add NVBP and change definitions of NUZTAB/NOCTAB. |
!> | ? | GP | v3.4: Add NPLTEE,NPELEE. Update various parameters. |
!> | 1994-09-30 | RAH | v3.4.1: Modernized declarations, removed INTEGER*2, standard header. |
!> | 1996-10-24 | GP | v4.0: Alter dimensions. Add NRDEE,NVSEE for new VSS module. |
!> | 1997-01-17 | RAH | Update SHEVER, BDEVER, BANNER. |
!> | 1997-02-18 | RAH | v4.1: Remove NRDEE (redundant). Set SHEVER, BDEVER, BANNER. |
!> | 1997-02-20 | RAH | Restore history. |
!> | 1998-02-20 | RAH | v4.2: Update SHEVER, BANNER. Remove NWELEE, NSZBOU, NPSITH. |
!> | 2004-07 | JE | Converted to Fortran 95 as part of SHEGRAPH Version 2 integration. |
!> | 2009-01-01 | JE | v4.3.5F90: Created `sglobal` module from legacy include files (e.g. al_p). |
!> | 2024-09-05 | SvB | Added KIND parameters and FORD docs. |
!> | 2026-03 | SB |  Increase array sizes now all the 2 and 3D arrays are allocatable, NXOCEE=4*nxee |
MODULE sglobal

   USE MOD_PARAMETERS, ONLY : I_P, R8P, LENGTH_FILEPATH

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: SHEVER, BDEVER, BANNER, RUNFIL
   PUBLIC :: nxee, nyee, nlfee, nelee, LLEE, NVEE, NSEE, NVSEE, NVBP, NUZTAB, NLYREE, NSETEE, &
      NXOCEE, NOCTAB, NSEDEE, NCONEE, NOLEE, NPLTEE, NPELEE, max_no_snowmelt_slugs, NXSCEE
   PUBLIC :: total_no_elements, total_no_links, top_cell_no, szmonte, ran2monte1, ran2monte2, pcmonte, montec
   PUBLIC :: DIRQQ, filnam, cnam, rootdir, hdf5filename, visualisation_plan_filename, visualisation_check_filename
   PUBLIC :: UZNOW, cellarea, DXQQ, DYQQ, ZGRUND
   PUBLIC :: marker999, imarker, izero, ione, izero1, ione1, zero, half, one, two, three, five, vsmall, zero1, one1
   PUBLIC :: EARRAY, text32
   PUBLIC :: eqmarker, gtzero, gezero, ltzero, lezero, iszero, iszero_a, i_iszero_a2, notzero, isone, notone
   PUBLIC :: idimje, dimje
   PUBLIC :: ISERROR, ISERROR2

   ! --------------------------------------------------------------------
   ! System Version and Banners
   ! --------------------------------------------------------------------
   REAL(KIND=R8P), PARAMETER :: SHEVER = 4.6_R8P !! SHETRAN version number (Major.Minor format).
   LOGICAL, PARAMETER :: BDEVER = .TRUE. !! Development version flag. `.TRUE.` for development, `.FALSE.` for release.
   CHARACTER(*), PARAMETER :: BANNER = 'SHETRAN Hydrological Model' !! Banner for local implementation.
   CHARACTER(*), PARAMETER :: RUNFIL = 'rundata_' !! Base filename for run data files.

   ! --------------------------------------------------------------------
   ! Array Dimensions and Sizing Parameters
   ! --------------------------------------------------------------------
   ! Default 'large' settings. Alternative configurations for specific
   ! catchments are provided below in commented blocks.
   INTEGER(KIND=I_P), PARAMETER :: nxee = 1000 !! Max grid points in x-direction.
   INTEGER(KIND=I_P), PARAMETER :: nyee = 1000 !! Max grid points in y-direction.
   INTEGER(KIND=I_P), PARAMETER :: nlfee = 20000 !! Max number of river links.
   INTEGER(KIND=I_P), PARAMETER :: nelee = 250000 !! Max number of grid elements.
   INTEGER(KIND=I_P), PARAMETER :: LLEE = 50 !! Max grid points in vertical direction plus one.
   !
   ! Alternative settings for sv4 large
   ! INTEGER, PARAMETER :: nxee=200, nyee=200, nlfee=10000, nelee=40000
   !
   ! Alternative settings for Dunsop200
   ! INTEGER, PARAMETER :: nxee=35, nyee=40, nlfee=240, nelee=900
   !
   ! Alternative settings for Cobres
   ! INTEGER, PARAMETER :: NXEE=40, NYEE=40, NLFEE=132, NELEE=320
   ! INTEGER, PARAMETER :: LLEE=50
   !
   ! Alternative settings for Slapton
   ! INTEGER, PARAMETER :: NXEE=40, NYEE=40, NLFEE=40, NELEE=800

   INTEGER(KIND=I_P), PARAMETER :: NVEE = 250000 !! Max number of vegetation types.
   INTEGER(KIND=I_P), PARAMETER :: NSEE = 1000 !! Max number of soil types.
   INTEGER(KIND=I_P), PARAMETER :: NVSEE = 20 !! Max number of tables for VSS component.
   INTEGER(KIND=I_P), PARAMETER :: NVBP = 140 !! Max number of time-varying vegetation breakpoints.
   INTEGER(KIND=I_P), PARAMETER :: NUZTAB = 20 !! Max table entries for ET component (PSI/RCF/FET).
   INTEGER(KIND=I_P), PARAMETER :: NLYREE = 20 !! Max number of soil layers plus one.
   INTEGER(KIND=I_P), PARAMETER :: NSETEE = 45 !! Max number of output sets for binary results file.
   INTEGER(KIND=I_P), PARAMETER :: NXOCEE = 4000 !! Max number of elements (grids, banks, links) in a row.
   INTEGER(KIND=I_P), PARAMETER :: NOCTAB = 20 !! Max categories for OC tables (roughness, x-section, boundaries).
   INTEGER(KIND=I_P), PARAMETER :: NSEDEE = 7 !! Max number of sediment size fractions.
   INTEGER(KIND=I_P), PARAMETER :: NCONEE = 3 !! Max number of contaminants.
   INTEGER(KIND=I_P), PARAMETER :: NOLEE = 2 * LLEE !! Max number of overlaps for contaminant calculations.
   INTEGER(KIND=I_P), PARAMETER :: NPLTEE = NVEE !! Max plants in an element (for contaminants).
   INTEGER(KIND=I_P), PARAMETER :: NPELEE = 2 !! Total number of plants (for contaminants).
   INTEGER(KIND=I_P), PARAMETER :: max_no_snowmelt_slugs = 400 !! Max number of snowmelt slugs.
   INTEGER(KIND=I_P), PARAMETER :: NXSCEE = 100000 !! Legacy parameter, usage to be reviewed.

   ! --------------------------------------------------------------------
   ! Global Variables
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P) :: total_no_elements = -1 !! Actual number of elements in the simulation.
   INTEGER(KIND=I_P) :: total_no_links = -1 !! Actual number of links in the simulation.
   INTEGER(KIND=I_P) :: top_cell_no = -1 !! Index of the top-most cell.
   INTEGER(KIND=I_P) :: szmonte = -1 !! Monte Carlo simulation parameter.
   INTEGER(KIND=I_P) :: ran2monte1 = -1 !! Monte Carlo simulation parameter.
   INTEGER(KIND=I_P) :: ran2monte2 = -1 !! Monte Carlo simulation parameter.
   INTEGER(KIND=I_P) :: pcmonte = -1 !! Monte Carlo simulation parameter.
   INTEGER(KIND=I_P), DIMENSION(:, :), ALLOCATABLE :: montec !! Monte Carlo data array (rows x cols), use I_P kind for portability.
   CHARACTER(LEN=LENGTH_FILEPATH) :: DIRQQ !! Working directory / absolute path (string, portable length from MOD_PARAMETERS)
   CHARACTER(LEN=LENGTH_FILEPATH) :: filnam !! Primary run filename (string)
   CHARACTER(LEN=LENGTH_FILEPATH) :: cnam !! Secondary/name string (string)
   CHARACTER(LEN=LENGTH_FILEPATH) :: rootdir !! Root install directory (string)
   CHARACTER(LEN=LENGTH_FILEPATH) :: hdf5filename !! HDF5 output filename (string)
   CHARACTER(LEN=LENGTH_FILEPATH) :: visualisation_plan_filename !! Visualization plan filename (string)
   CHARACTER(LEN=LENGTH_FILEPATH) :: visualisation_check_filename !! Visualization check filename (string)
   REAL(KIND=R8P) :: UZNOW !! Current simulation time in hours.
   REAL(KIND=R8P), DIMENSION(nelee) :: cellarea !! Area of each grid cell [m^2].
   REAL(KIND=R8P), DIMENSION(nelee) :: DXQQ !! Length of cell face in x-direction [m].
   REAL(KIND=R8P), DIMENSION(nelee) :: DYQQ !! Length of cell face in y-direction [m].
   REAL(KIND=R8P), DIMENSION(nelee) :: ZGRUND !! Ground surface elevation of each cell [m].


   ! --------------------------------------------------------------------
   ! Model Timestep Control Flags
   ! --------------------------------------------------------------------
   LOGICAL :: ISERROR !! Flag set to .TRUE. on critical errors (e.g., 1024, 1030) to trigger timestep reduction.
   LOGICAL :: ISERROR2 !! Flag set to .TRUE. on error 1060 to trigger timestep reduction.


   ! --------------------------------------------------------------------
   ! Mathematical and Numerical Constants
   ! --------------------------------------------------------------------
   REAL(KIND=R8P), PARAMETER :: marker999 = 999999.9_R8P !! A large marker value to indicate missing or invalid data.
   INTEGER(KIND=I_P), PARAMETER :: imarker = INT(marker999) !! Integer version of the marker value.
   INTEGER(KIND=I_P), PARAMETER :: izero = 0 !! Basic integer constant zero.
   INTEGER(KIND=I_P), PARAMETER :: ione = 1 !! Basic integer constant one.
   INTEGER(KIND=I_P), PARAMETER, DIMENSION(1) :: izero1 = [0] !! Single-element array version of integer zero.
   INTEGER(KIND=I_P), PARAMETER, DIMENSION(1) :: ione1 = [1] !! Single-element array version of integer one.
   REAL(KIND=R8P), PARAMETER :: zero = 0.0_R8P !! Double precision zero.
   REAL(KIND=R8P), PARAMETER :: half = 0.5_R8P !! Double precision one-half.
   REAL(KIND=R8P), PARAMETER :: one = 1.0_R8P !! Double precision one.
   REAL(KIND=R8P), PARAMETER :: two = 2.0_R8P !! Double precision two.
   REAL(KIND=R8P), PARAMETER :: three = 3.0_R8P !! Double precision three.
   REAL(KIND=R8P), PARAMETER :: five = 5.0_R8P !! Double precision five.
   REAL(KIND=R8P), PARAMETER :: vsmall = 1.0e-20_R8P !! A very small value used for floating-point comparisons (tolerance).
   REAL(KIND=R8P), PARAMETER, DIMENSION(1) :: zero1 = [0.0_R8P] !! Single-element array version of double precision zero.
   REAL(KIND=R8P), PARAMETER, DIMENSION(1) :: one1 = [1.0_R8P] !! Single-element array version of double precision one.

   ! --------------------------------------------------------------------
   ! Miscellaneous Global Variables
   ! --------------------------------------------------------------------
   REAL(KIND=R8P) :: EARRAY(1) !! A temporary array for passing values to the ERROR subroutine.
   CHARACTER(32) :: text32 !! A temporary 32-character string variable.

CONTAINS



   !> summary: Checks if a double precision value is equal to the global marker.
   !>
   !> Compares the input value against the integer representation of `marker999`.
   ELEMENTAL LOGICAL FUNCTION eqmarker(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      eqmarker = INT(a)==imarker
   END FUNCTION eqmarker



   !> summary: Checks if a double precision value is greater than zero.
   ELEMENTAL LOGICAL FUNCTION gtzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      gtzero = a>zero
   END FUNCTION gtzero



   !> summary: Checks if a double precision value is greater than or equal to zero.
   !>
   !> Uses `iszero` to handle floating-point comparisons near zero.
   ELEMENTAL LOGICAL FUNCTION gezero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      gezero = ISZERO(a) .OR. a>zero
   END FUNCTION gezero



   !> summary: Checks if a double precision value is less than zero.
   ELEMENTAL LOGICAL FUNCTION ltzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      ltzero = a<zero
   END FUNCTION ltzero



   !> summary: Checks if a double precision value is less than or equal to zero.
   !>
   !> Uses `iszero` to handle floating-point comparisons near zero.
   ELEMENTAL LOGICAL FUNCTION lezero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      lezero = ISZERO(a) .OR. a<zero
   END FUNCTION lezero



   !> summary: Checks if a double precision value is effectively zero.
   !>
   !> Compares the absolute value of the input against a small tolerance (`vsmall`).
   ELEMENTAL LOGICAL FUNCTION iszero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      iszero = ABS(a)<vsmall
   END FUNCTION iszero



   !> summary: Checks if all elements in a 1D double precision array are zero.
   PURE LOGICAL FUNCTION iszero_a(a)
      INTEGER(KIND=I_P) :: i
      REAL(KIND=R8P), DIMENSION(:), INTENT(IN) :: a !! The array to check.
      iszero_a=.TRUE.
      DO i=1,SIZE(a)
         IF(.NOT.iszero_a) CYCLE     !FOR AD
         iszero_a = iszero(a(i))
      ENDDO
   END FUNCTION iszero_a



   !> summary: Checks if all elements in a 2D integer array are zero.
   PURE LOGICAL FUNCTION i_iszero_a2(a)
      INTEGER(KIND=I_P)                             :: i, j
      INTEGER(KIND=I_P), DIMENSION(:,:), INTENT(IN) :: a !! The 2D array to check.
      i_iszero_a2=.TRUE.
      DO i=1,SIZE(a, DIM=1)
         DO j=1,SIZE(a, DIM=2)
            IF(.NOT.i_iszero_a2) CYCLE     !FOR AD
            i_iszero_a2 = a(i,j)==0
         ENDDO
      ENDDO
   END FUNCTION i_iszero_a2



   !> summary: Checks if a double precision value is not zero.
   ELEMENTAL LOGICAL FUNCTION notzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      notzero = .NOT.ISZERO(a)
   END FUNCTION notzero



   !> summary: Checks if a double precision value is effectively one.
   !>
   !> Compares the value against one using a small tolerance (`vsmall`).
   ELEMENTAL LOGICAL FUNCTION isone(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      isone = ABS(a-one)<vsmall
   END FUNCTION isone



   !> summary: Checks if a double precision value is not one.
   ELEMENTAL LOGICAL FUNCTION notone(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! The value to check.
      notone = .NOT.ISONE(a)
   END FUNCTION notone



   !> summary: Integer positive difference function (equivalent to `MAX(x-y, 0)`).
   ELEMENTAL INTEGER FUNCTION idimje(x,y)
      INTEGER(KIND=I_P), INTENT(IN) :: x !! The first integer.
      INTEGER(KIND=I_P), INTENT(IN) :: y !! The second integer.
      IF(x>y) THEN
         idimje = x-y
      ELSE
         idimje = 0
      ENDIF
   END FUNCTION idimje



   !> summary: Double precision positive difference function (equivalent to `MAX(x-y, 0.0)`).
   ELEMENTAL FUNCTION dimje(x,y)
      REAL(KIND=R8P) :: dimje
      REAL(KIND=R8P), INTENT(IN) :: x !! The first value.
      REAL(KIND=R8P), INTENT(IN) :: y !! The second value.
      IF(x>y) THEN
         dimje = x-y
      ELSE
         dimje = zero
      ENDIF
   END FUNCTION dimje

END MODULE sglobal
