!> summary: Defines global parameters, constants, variables, and utility functions.
!> author: J. Ewen, Newcastle University
!> date: 2009-01-01
!>
!> This module is a central component of SHETRAN, replacing legacy `INCLUDE`
!> files like `al_p.inc`. It provides a wide range of globally accessible
!> entities, including:
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
!> | 2009-01 | JE | v4.3.5F90: Created `sglobal` module from legacy include files (e.g. al_p). |
MODULE sglobal

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: SHEVER, BDEVER, BANNER, RUNFIL
   PUBLIC :: nxee, nyee, nlfee, nelee, LLEE, NVEE, NSEE, NVSEE, NVBP, NUZTAB, NLYREE, NSETEE, &
      NXOCEE, NOCTAB, NSEDEE, NCONEE, NOLEE, NPLTEE, NPELEE, max_no_snowmelt_slugs, NXSCEE
   PUBLIC :: total_no_elements, total_no_links, top_cell_no, szmonte, ran2monte1, ran2monte2, pcmonte, montec
   PUBLIC :: DIRQQ, filnam, cnam, rootdir, hdf5filename, visualisation_plan_filename, visualisation_check_filename
   PUBLIC :: UZNOW, cellarea, DXQQ, DYQQ, ZGRUND
   PUBLIC :: ERRNEE, FFFATAL, EEERR, WWWARN, pppri, ERRC, ERRTOT, helppath, ISERROR, ISERROR2
   PUBLIC :: marker999, imarker, izero, ione, izero1, ione1, zero, half, one, two, three, five, vsmall, zero1, one1
   PUBLIC :: EARRAY, text32
   PUBLIC :: eqmarker, gtzero, gezero, ltzero, lezero, iszero, iszero_a, i_iszero_a2, notzero, isone, notone
   PUBLIC :: idimje, dimje
   PUBLIC :: ERROR, ALSTOP

   ! --------------------------------------------------------------------
   ! System Version and Banners
   ! --------------------------------------------------------------------
   DOUBLEPRECISION, PARAMETER :: SHEVER = 4.5 !! SHETRAN version number (Major.Minor format).
   LOGICAL, PARAMETER :: BDEVER = .TRUE. !! Development version flag. `.TRUE.` for development, `.FALSE.` for release.
   CHARACTER(*), PARAMETER :: BANNER = 'SHETRAN Version 4.5 for PC - incorporating SHEGRAPH Version 2' !! Banner for local implementation.
   CHARACTER(*), PARAMETER :: RUNFIL = 'rundata_' !! Base filename for run data files.

   ! --------------------------------------------------------------------
   ! Array Dimensions and Sizing Parameters
   ! --------------------------------------------------------------------
   ! Default 'large' settings. Alternative configurations for specific
   ! catchments are provided below in commented blocks.
   INTEGER, PARAMETER :: nxee = 250 !! Max grid points in x-direction.
   INTEGER, PARAMETER :: nyee = 250 !! Max grid points in y-direction.
   INTEGER, PARAMETER :: nlfee = 10000 !! Max number of river links.
   INTEGER, PARAMETER :: nelee = 30000 !! Max number of grid elements.
   INTEGER, PARAMETER :: LLEE = 50 !! Max grid points in vertical direction plus one.
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

   INTEGER, PARAMETER :: NVEE = 30000 !! Max number of vegetation types.
   INTEGER, PARAMETER :: NSEE = 1000 !! Max number of soil types.
   INTEGER, PARAMETER :: NVSEE = 20 !! Max number of tables for VSS component.
   INTEGER, PARAMETER :: NVBP = 140 !! Max number of time-varying vegetation breakpoints.
   INTEGER, PARAMETER :: NUZTAB = 20 !! Max table entries for ET component (PSI/RCF/FET).
   INTEGER, PARAMETER :: NLYREE = 20 !! Max number of soil layers plus one.
   INTEGER, PARAMETER :: NSETEE = 45 !! Max number of output sets for binary results file.
   INTEGER, PARAMETER :: NXOCEE = 500 !! Max number of elements (grids, banks, links) in a row.
   INTEGER, PARAMETER :: NOCTAB = 20 !! Max categories for OC tables (roughness, x-section, boundaries).
   INTEGER, PARAMETER :: NSEDEE = 7 !! Max number of sediment size fractions.
   INTEGER, PARAMETER :: NCONEE = 3 !! Max number of contaminants.
   INTEGER, PARAMETER :: NOLEE = 2 * LLEE !! Max number of overlaps for contaminant calculations.
   INTEGER, PARAMETER :: NPLTEE = NVEE !! Max plants in an element (for contaminants).
   INTEGER, PARAMETER :: NPELEE = 2 !! Total number of plants (for contaminants).
   INTEGER, PARAMETER :: max_no_snowmelt_slugs = 400 !! Max number of snowmelt slugs.
   INTEGER, PARAMETER :: NXSCEE = 100000 !! Legacy parameter, usage to be reviewed.

   ! --------------------------------------------------------------------
   ! Global Variables
   ! --------------------------------------------------------------------
   INTEGER :: total_no_elements = -1 !! Actual number of elements in the simulation.
   INTEGER :: total_no_links = -1 !! Actual number of links in the simulation.
   INTEGER :: top_cell_no = -1 !! Index of the top-most cell.
   INTEGER :: szmonte = -1, ran2monte1 = -1, ran2monte2 = -1, pcmonte = -1 !! Monte Carlo simulation parameters.
   INTEGER(1), DIMENSION(:, :), ALLOCATABLE :: montec !! Monte Carlo data array.
   CHARACTER(256) :: DIRQQ, filnam, cnam, rootdir !! Directory and file path variables.
   CHARACTER(256) :: hdf5filename, visualisation_plan_filename, visualisation_check_filename !! Filenames for I/O.
   DOUBLEPRECISION :: UZNOW !! Current simulation time in hours.
   DOUBLEPRECISION, DIMENSION(nelee) :: cellarea !! Area of each grid cell [m^2].
   DOUBLEPRECISION, DIMENSION(nelee) :: DXQQ !! Length of cell face in x-direction [m].
   DOUBLEPRECISION, DIMENSION(nelee) :: DYQQ !! Length of cell face in y-direction [m].
   DOUBLEPRECISION, DIMENSION(nelee) :: ZGRUND !! Ground surface elevation of each cell [m].
   ! --------------------------------------------------------------------
   ! Error Handling Constants and Variables
   ! --------------------------------------------------------------------
   INTEGER, PARAMETER :: ERRNEE = 100 !! Max number of distinct error codes per module.
   INTEGER, PARAMETER :: FFFATAL = 1 !! Error type for fatal errors.
   INTEGER, PARAMETER :: EEERR = 2 !! Error type for non-fatal errors.
   INTEGER, PARAMETER :: WWWARN = 3 !! Error type for warnings.
   INTEGER, PARAMETER :: pppri = 23 !! File unit for primary output.
   INTEGER :: ERRC(0:ERRNEE, 0:3) = 0 !! Counters for error occurrences.
   INTEGER :: ERRTOT = 0 !! Total count of all errors and warnings.
   CHARACTER(128) :: helppath !! Path to help message files.
   LOGICAL :: ISERROR !! Flag set to .TRUE. on critical errors (e.g., 1024, 1030) to trigger timestep reduction.
   LOGICAL :: ISERROR2 !! Flag set to .TRUE. on error 1060 to trigger timestep reduction.

   ! --------------------------------------------------------------------
   ! Mathematical and Numerical Constants
   ! --------------------------------------------------------------------
   DOUBLEPRECISION, PARAMETER :: marker999 = 999999.9D0 !! A large marker value to indicate missing or invalid data.
   INTEGER, PARAMETER :: imarker = INT(marker999) !! Integer version of the marker value.
   INTEGER, PARAMETER :: izero = 0, ione = 1 !! Basic integer constants.
   INTEGER, PARAMETER, DIMENSION(1) :: izero1 = [0], ione1 = [1] !! Single-element array versions of integer constants.
   DOUBLEPRECISION, PARAMETER :: zero = 0.0d0 !! Double precision zero.
   DOUBLEPRECISION, PARAMETER :: half = 0.5d0 !! Double precision one-half.
   DOUBLEPRECISION, PARAMETER :: one = 1.0d0 !! Double precision one.
   DOUBLEPRECISION, PARAMETER :: two = 2.0d0 !! Double precision two.
   DOUBLEPRECISION, PARAMETER :: three = 3.0d0 !! Double precision three.
   DOUBLEPRECISION, PARAMETER :: five = 5.0d0 !! Double precision five.
   DOUBLEPRECISION, PARAMETER :: vsmall = 1.0d-20 !! A very small value used for floating-point comparisons (tolerance).
   DOUBLEPRECISION, PARAMETER, DIMENSION(1) :: zero1 = [0.0d0], one1 = [1.0d0] !! Single-element array versions of double precision constants.

   ! --------------------------------------------------------------------
   ! Miscellaneous Global Variables
   ! --------------------------------------------------------------------
   DOUBLEPRECISION :: EARRAY(1) !! A temporary array for passing values to the ERROR subroutine.
   CHARACTER(32) :: text32 !! A temporary 32-character string variable.

CONTAINS


   !> summary: Checks if a double precision value is equal to the global marker.
   !>
   !> Compares the input value against the integer representation of `marker999`.
   LOGICAL FUNCTION eqmarker(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      eqmarker = INT(a)==imarker
   END FUNCTION eqmarker

   !> summary: Checks if a double precision value is greater than zero.
   LOGICAL FUNCTION gtzero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      gtzero = a>zero
   END FUNCTION gtzero

   !> summary: Checks if a double precision value is greater than or equal to zero.
   !>
   !> Uses `iszero` to handle floating-point comparisons near zero.
   LOGICAL FUNCTION gezero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      gezero = ISZERO(a) .OR. a>zero
   END FUNCTION gezero

   !> summary: Checks if a double precision value is less than zero.
   LOGICAL FUNCTION ltzero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      ltzero = a<zero
   END FUNCTION ltzero

   !> summary: Checks if a double precision value is less than or equal to zero.
   !>
   !> Uses `iszero` to handle floating-point comparisons near zero.
   LOGICAL FUNCTION lezero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      lezero = ISZERO(a) .OR. a<zero
   END FUNCTION lezero

   !> summary: Checks if a double precision value is effectively zero.
   !>
   !> Compares the absolute value of the input against a small tolerance (`vsmall`).
   LOGICAL FUNCTION iszero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      iszero = ABS(a)<vsmall
   END FUNCTION iszero

   !> summary: Checks if all elements in a 1D double precision array are zero.
   LOGICAL FUNCTION iszero_a(a)
      INTEGER :: i
      DOUBLEPRECISION, DIMENSION(:), INTENT(IN) :: a !! The array to check.
      iszero_a=.TRUE.
      DO i=1,SIZE(a)
         IF(.NOT.iszero_a) CYCLE     !FOR AD
         iszero_a = iszero(a(i))
      ENDDO
   END FUNCTION iszero_a

   !> summary: Checks if all elements in a 2D integer array are zero.
   LOGICAL FUNCTION i_iszero_a2(a)
      INTEGER                             :: i, j
      INTEGER, DIMENSION(:,:), INTENT(IN) :: a !! The 2D array to check.
      i_iszero_a2=.TRUE.
      DO i=1,SIZE(a, DIM=1)
         DO j=1,SIZE(a, DIM=2)
            IF(.NOT.i_iszero_a2) CYCLE     !FOR AD
            i_iszero_a2 = a(i,j)==0
         ENDDO
      ENDDO
   END FUNCTION i_iszero_a2


   !> summary: Checks if a double precision value is not zero.
   LOGICAL FUNCTION notzero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      notzero = .NOT.ISZERO(a)
   END FUNCTION notzero

   !> summary: Checks if a double precision value is effectively one.
   !>
   !> Compares the value against one using a small tolerance (`vsmall`).
   LOGICAL FUNCTION isone(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      isone = ABS(a-one)<vsmall
   END FUNCTION isone

   !> summary: Checks if a double precision value is not one.
   LOGICAL FUNCTION notone(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! The value to check.
      notone = .NOT.ISONE(a)
   END FUNCTION notone


   !> summary: Integer positive difference function (equivalent to `MAX(x-y, 0)`).
   INTEGER FUNCTION idimje(x,y)
      INTEGER, INTENT(IN) :: x !! The first integer.
      INTEGER, INTENT(IN) :: y !! The second integer.
      IF(x>y) THEN
         idimje = x-y
      ELSE
         idimje = 0
      ENDIF
   END FUNCTION idimje

   !> summary: Double precision positive difference function (equivalent to `MAX(x-y, 0.0)`).
   DOUBLEPRECISION FUNCTION dimje(x,y)
      DOUBLEPRECISION, INTENT(IN) :: x !! The first value.
      DOUBLEPRECISION, INTENT(IN) :: y !! The second value.
      IF(x>y) THEN
         dimje = x-y
      ELSE
         dimje = zero
      ENDIF
   END FUNCTION dimje

   !> summary: Prints an error message, updates error counters, and optionally stops the program.
   !> author: R. A. Heath, Newcastle University
   !>
   !> This is the central error handling routine for SHETRAN. It formats and
   !> prints error messages, maintains a count of different errors, and can
   !> terminate the simulation for fatal errors. It can also print a summary
   !> of all recorded errors and associated help messages.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:----:|:------:|-------------|
   !> | 1994-10-08 | RAH | v3.4.1: Created from v3.4. replace common counter arrays with local ERRC; extend ERRNUM range below 1000; introduce ETYPE=0; print IEL, CELL only if non-zero; print help files along with final asummary; declare everything; no INTEGER*2; test subscript ranges; tidy comments; call ALSTOP to stop; use local IFATAL etc instead of common FATAL etc; 1024 no longer uses EARRAY. |
   !> | 1997-08-04 | RAH | v4.1: Use EARRAY for error 1024. |
   !> | 1997-08-11 | RAH | Added EXTERNAL after INCLUDE. |
   SUBROUTINE ERROR (ETYPE, ERRNUM, OUT, IEL, CELL, TEXT)

!----------------------------------------------------------------------*
! Commons and constants
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ETYPE !! The type of error (FFFATAL, EEERR, WWWARN). A value of -999 triggers a help path check.
      INTEGER, INTENT(IN) :: ERRNUM !! The unique error number code.
      INTEGER, INTENT(IN) :: OUT !! The output file unit for the message.
      INTEGER, INTENT(IN) :: IEL !! The element number where the error occurred (optional).
      INTEGER, INTENT(IN) :: CELL !! The cell number where the error occurred (optional).
      CHARACTER (LEN=*), INTENT(IN) :: TEXT !! The descriptive error text.

      INTEGER :: NONE, ERRCEE, HLP
      PARAMETER (NONE = 0, ERRCEE = (1 + ERRNEE) * 4)
      PARAMETER (HLP = 8)
      CHARACTER (LEN=*) :: PATH1
      PARAMETER (PATH1 = '/shetran/')
      INTEGER :: COUNT, ERRN, AMODL
      CHARACTER (11) :: CTYPE (3)
      CHARACTER(256) :: FIL
      CHARACTER(80)  :: HLPMSG
      LOGICAL :: VALID
      INTEGER :: IO_STATUS

      DATA CTYPE / 'FATAL ERROR', '      ERROR', '    WARNING' /
!----------------------------------------------------------------------*
      INTEGER :: helpcheck !! Status from checking for help directory.
      CHARACTER :: cc
      character, parameter :: slash='/'

      LOGICAL :: present

      helppath = '\helpmessages'

!**SB 07072020 reduce timestep if there are errors 1024,1030,1060
      ISERROR = .FALSE.
      ISERROR2 = .FALSE.

      IF (ETYPE == - 999) THEN
         present = .TRUE.;
         !helppath = ''
         !nett 09080
         !""helpcheck = FULLPATHQQ ('helpmessages', helppath)
         helpcheck = 60
         ! IF(helpcheck/=0) INQUIRE(FILE=helppath, EXIST=present)
         !IF(.NOT.present) THEN
         !nett 090805
         IF (helpcheck == 0) THEN
            PRINT * , "Failedtofindthe'helpmessages'directory"
            PRINT * , '  (which contains the help message files)'
            PRINT * , "Itsnamemustbe'helpmessages'"
            !""helpcheck = GETDRIVEDIRQQ (helppath)
            IF (helpcheck /= 0) THEN
               !!PRINT *, "anditmustbein"//TRIM (helppath)
            ENDIF
            PRINT * , "Type's'tostopor'c'tocontinue"
            !""cc = GETCHARQQ ()
            cc = 'c'
            DO WHILE (cc /= 'c'.AND.cc /= 's'.AND.cc /= 'C'.AND.cc /= 'S')
               !""cc = GETCHARQQ ()
            ENDDO
            IF (cc == 's'.OR.cc == 'S') STOP
         ENDIF
         RETURN


      ENDIF
! Write general error message
! ---------------------------
      IF (ETYPE.GE.1.AND.ETYPE.LE.3) THEN
         IF (ETYPE.EQ.FFFATAL) WRITE (OUT, '(//)')
         IF (IEL.EQ.0) THEN
            WRITE (OUT, 9100) CTYPE (ETYPE), ERRNUM, UZNOW
         ELSEIF (CELL.EQ.0) THEN
            WRITE (OUT, 9100) CTYPE (ETYPE), ERRNUM, UZNOW, IEL
         ELSE
            WRITE (OUT, 9100) CTYPE (ETYPE), ERRNUM, UZNOW, IEL, CELL
         ENDIF
      ENDIF



      WRITE (OUT, '(8X,A)') TEXT
! Decompose ERRNUM and update counters
! ------------------------------------
      IF (ETYPE.NE.NONE) THEN
         ERRTOT = ERRTOT + 1
         AMODL = ERRNUM / 1000
         ERRN = MOD (ERRNUM, 1000)
         VALID = &
            AMODL.GE.0.AND.AMODL.LE.3.AND.ERRN.GE.0.AND.ERRN.LE.ERRNEE
         IF (VALID) ERRC (ERRN, AMODL) = ERRC (ERRN, AMODL) + 1



      ENDIF
! Write specific error messages
! -----------------------------
      IF (ERRNUM.EQ.1003) THEN
         WRITE (OUT, 91003) EARRAY (1)
!*970804
      ELSEIF (ERRNUM.EQ.1024) THEN
         WRITE (OUT, 91024) EARRAY (1)
!*



      ENDIF


!**SB 07072020 reduce timestep if there are errors 1024,1030,1060
      IF ((ERRNUM.EQ.1024).OR.(ERRNUM.EQ.1030)) THEN
         ISERROR=.TRUE.
      ENDIF
      IF (ERRNUM.EQ.1060) THEN
         ISERROR2=.TRUE.
      ENDIF
! Write asummary
! -------------

      IF (ETYPE.EQ.FFFATAL.OR.ERRNUM.EQ.0) THEN
         WRITE ( * , '(//A/A/)') ' ### Error asummary and Advice ###', '  ------------------------'

         IF (ERRTOT.GT.0) WRITE ( * , '(A/)') ' ==> Check printed output files for more details <=='
         DO 50 AMODL = 0, 3
            DO 10 ERRN = 0, ERRNEE
               COUNT = ERRC (ERRN, AMODL)

               IF (COUNT.GT.0) THEN
!             * Print number of occurrences

                  WRITE ( *, 9500) ERRN + AMODL * 1000, COUNT
!             * Print contents of help file (if any)
                  WRITE (FIL, 9200) trim(rootdir)//TRIM (helppath) //'\', AMODL, ERRN
                  print*,dirqq,rootdir
                  print*,fil
                  WRITE(*, '(A)', ADVANCE='NO') 'Press Enter to continue...'
                  READ(*,*)
                  OPEN (HLP, FILE = FIL, STATUS = 'OLD', IOSTAT = IO_STATUS)
                  IF (IO_STATUS == 0) THEN
                     DO
                        READ (HLP, '(A)', IOSTAT = IO_STATUS) HLPMSG
                        IF (IO_STATUS /= 0) EXIT
                        WRITE ( * , '(A)') HLPMSG
                     END DO
                     CLOSE (HLP)
                  ENDIF

                  WRITE ( *, * )
               ENDIF
10          END DO
50       END DO

         WRITE ( *, 9600) ERRTOT



      ENDIF
! Stop?
! -----

      IF (ETYPE.EQ.FFFATAL) CALL ALSTOP (1)
9100  FORMAT(/ ' !!!', A, I5.4, ' at time =', F12.2, ' hours': &
      &         ', iel =', I5:', cell =', I5 )
9200  FORMAT(A,I1,I3.3)
9500  FORMAT(' No. of occurrences of error number',I5.4,' is',I6)
9600  FORMAT(/' ### End of asummary: recorded error count is',I7,' ###'/)
91003 FORMAT(' MAXIMUM DIFFERENCE (DHMAX) = ',G12.6,' METRES')
!*970804
91024 FORMAT(' DEPTH OF SURFACE WATER BELOW GROUND = ',G12.6,' METRES')
!*
   END SUBROUTINE ERROR

   !> summary: Performs system-level tasks and terminates the program.
   !> author: R. A. Heath, Newcastle University
   !>
   !> This subroutine is called to stop the program, typically after a fatal
   !> error. It provides a final message to the user before termination.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:----:|:------:|-------------|
   !> | 1994-09-17 | RAH | v3.4.1: File created. |
   !> | 2000-03-07 | SB | v4g-pc: Removed IEEE calls for PC version. |
   SUBROUTINE ALSTOP (FLAG)
      INTEGER, INTENT(IN) :: FLAG !! A flag indicating the reason for stopping. If > 0, it's a fatal error.

      IF (FLAG.GT.0) THEN
         WRITE(*, '(A)') 'FATAL ERROR: Program will terminate. Press Enter to exit...'
         READ(*,*)
         STOP 'Program terminating due to fatal error'
      ENDIF
   END SUBROUTINE ALSTOP
END MODULE sglobal
