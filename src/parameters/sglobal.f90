!> summary: Legacy global constants, run state, and error helpers.
!>
!> This module contains SHETRAN-wide size limits, version information, run
!> filenames, catchment paths, global time, geometry arrays, numeric comparison
!> helpers, and the legacy error-reporting interface. It is a Fortran 90
!> consolidation of earlier COMMON-block include files and remains widely used
!> by the older model components.
!>
!> Size parameters are compile-time maxima; the active problem size is held in
!> `total_no_elements`, `total_no_links`, and related run-state variables after
!> input has been read.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-02 | GP | 2.0 | SHE88 implementation on Newcastle AMDAHL. |
!> | 1989-03 | GP | 2.1 | Added SZ drain boundary dimensions. |
!> | 1989-04 | GP | 2.2 | Integrated sediment-yield dimensions and overall version number. |
!> | 1990-03 | GP | 3.0 | Added development version flag and further dimension variables. |
!> | 1992-01 | GP | 3.3 | Updated vegetation breakpoint and ET table dimensions. |
!> | 1994-09-30 | RAH | 3.4.1 | Standardised declarations, headers, dimensions, and comments. |
!> | 1996-10-24 | GP | 4.0 | Altered model dimensions and added VSS module dimensions. |
!> | 1997-02-18 | RAH | 4.1 | Removed redundant dimensions and updated version/banner metadata. |
!> | 1998-02-20 | RAH | 4.2 | Updated version/banner and removed unused dimensions. |
!> | 2004-07 | JE | - | Converted to Fortran 95 during SHEGRAPH v2 integration. |
!> | 2009-01 | JE | 4.3.5F90 | Created module form during the Fortran 90 conversion, replacing `AL_P`. |
!> | 2026-03 | SB | 4.6 | Increased array sizes after 2-D and 3-D arrays became allocatable, including `NXOCEE=4*NXEE`. |
!> @endhistory
MODULE sglobal
!USE BUFF_DISK
   USE MOD_PARAMETERS, ONLY : I_P, R8P, LENGTH_FILEPATH
   IMPLICIT NONE

   DOUBLEPRECISION, PARAMETER :: SHEVER=4.6 !! SHETRAN version number in `major.minor` form.
   LOGICAL, PARAMETER :: BDEVER=.TRUE.      !! Development-version flag.
   CHARACTER(*), PARAMETER :: BANNER='SHETRAN Hydrological Model' !! Banner text written by the model.
   CHARACTER(*), PARAMETER :: RUNFIL='rundata_' !! Prefix for catchment-specific run-data filenames.

   INTEGER, PARAMETER :: nxee=1000   !! Maximum grid points in the x direction.
   INTEGER, PARAMETER :: nyee=1000   !! Maximum grid points in the y direction.
   INTEGER, PARAMETER :: nlfee=20000 !! Maximum number of river/channel links.
   INTEGER, PARAMETER :: nelee=250000 !! Maximum total number of elements.

   INTEGER :: total_no_elements=-1 !! Active number of elements after setup.
   INTEGER :: total_no_links=-1    !! Active number of channel links after setup.
   INTEGER :: top_cell_no=-1       !! Top VSS cell number used by column setup.
   INTEGER :: szmonte=-1           !! Monte Carlo saturated-zone seed/state value.
   INTEGER :: ran2monte1=-1        !! First Monte Carlo random-number state value.
   INTEGER :: ran2monte2=-1        !! Second Monte Carlo random-number state value.
   INTEGER :: pcmonte=-1           !! Monte Carlo process-control state value.
   INTEGER(1), DIMENSION(:,:), ALLOCATABLE :: montec !! Monte Carlo classification grid.

   INTEGER, PARAMETER :: LLEE=50 !! Maximum vertical grid points plus one.

   INTEGER, PARAMETER :: NVEE=250000 !! Maximum vegetation types; also used for precipitation/PET station counts.
   INTEGER, PARAMETER :: NSEE=1000   !! Maximum soil types.
   INTEGER, PARAMETER :: NVSEE=20    !! Maximum table entries used in the VSS component.
   INTEGER, PARAMETER :: NVBP=140    !! Maximum time-varying vegetation breakpoints.
   INTEGER, PARAMETER :: NUZTAB=20   !! Maximum ET table entries for PSI, RCF, and FET values.
   INTEGER, PARAMETER :: NLYREE=20   !! Maximum number of soil layers plus one.
   INTEGER, PARAMETER :: NSETEE=45   !! Maximum number of output sets for `res` file output.
   INTEGER, PARAMETER :: NXOCEE=4*nxee !! Maximum number of grids, banks, and links in one row.
   INTEGER, PARAMETER :: NOCTAB=20   !! Maximum OC roughness, cross-section, and boundary-element categories.
   INTEGER, PARAMETER :: NSEDEE=7    !! Maximum sediment size fractions.
   INTEGER, PARAMETER :: NCONEE=3    !! Maximum contaminants.
   INTEGER, PARAMETER :: NOLEE=2*LLEE !! Maximum contaminant column-overlap entries.
   INTEGER, PARAMETER :: NPLTEE=NVEE !! Maximum plant types.
   INTEGER, PARAMETER :: NPELEE=2    !! Maximum plant slots per element.
   INTEGER, PARAMETER :: max_no_snowmelt_slugs=400 !! Maximum snowmelt slug records.

   CHARACTER(256) :: DIRQQ   !! Catchment directory path.
   CHARACTER(256) :: filnam  !! Current filename workspace.
   CHARACTER(256) :: cnam    !! Catchment name.
   CHARACTER(256) :: rootdir !! Root directory for model resources.
   CHARACTER(256) :: hdf5filename !! HDF5 output filename.
   CHARACTER(256) :: visualisation_plan_filename  !! Visualisation plan filename.
   CHARACTER(256) :: visualisation_check_filename !! Visualisation check filename.

   INTEGER, PARAMETER :: NXSCEE = 100000 !! Maximum cross-section table entries.
   INTEGER, PARAMETER :: ERRNEE = 100    !! Maximum error number per error module.
   INTEGER, PARAMETER :: FFFATAL = 1     !! Fatal error severity code.
   INTEGER, PARAMETER :: EEERR = 2       !! Non-fatal error severity code.
   INTEGER, PARAMETER :: WWWARN = 3      !! Warning severity code.
   INTEGER, PARAMETER :: pppri = 23      !! Default PRI output unit number.
   DOUBLEPRECISION :: UZNOW              !! Current model time in hours for reporting.
   DOUBLEPRECISION, PARAMETER :: marker999=999999.9D0 !! Legacy real missing-value marker.
   INTEGER, PARAMETER :: izero=0         !! Integer zero constant.
   INTEGER, PARAMETER :: izero1(1)=0     !! One-element integer zero vector for checker calls.
   INTEGER, PARAMETER :: ione=1          !! Integer one constant.
   INTEGER, PARAMETER :: ione1(1)=1      !! One-element integer one vector for checker calls.
   INTEGER, PARAMETER :: imarker=INT(marker999) !! Integer form of the legacy missing-value marker.
   DOUBLEPRECISION, PARAMETER :: zero=0.0d0    !! Double-precision zero.
   DOUBLEPRECISION, PARAMETER :: zero1(1)=0.0d0 !! One-element double-precision zero vector.
   DOUBLEPRECISION, PARAMETER :: half=0.5d0    !! Double-precision one half.
   DOUBLEPRECISION, PARAMETER :: one=1.0d0     !! Double-precision one.
   DOUBLEPRECISION, PARAMETER :: one1(1)=1.0d0 !! One-element double-precision one vector.
   DOUBLEPRECISION, PARAMETER :: two=2.0d0     !! Double-precision two.
   DOUBLEPRECISION, PARAMETER :: three=3.0d0   !! Double-precision three.
   DOUBLEPRECISION, PARAMETER :: five=5.0d0    !! Double-precision five.
   DOUBLEPRECISION, PARAMETER :: vsmall=1.0d-20 !! Tolerance used by zero/one comparison helpers.
   DOUBLEPRECISION :: EARRAY(1)          !! Numeric context value printed by selected error messages.
   INTEGER :: ERRC(0:ERRNEE,0:3)=0       !! Error occurrence counts by error number and module group.
   INTEGER :: ERRTOT=0                   !! Total recorded error and warning count.
   CHARACTER(128) :: helppath            !! Relative path to help-message files.
   LOGICAL :: ISERROR                    !! Timestep-reduction flag for selected flow errors.
   LOGICAL :: ISERROR2                   !! Secondary timestep-reduction flag for selected flow errors.
   LOGICAL :: error_mode                 !! If true, fatal stops do not wait for an Enter keypress.

   DOUBLEPRECISION, DIMENSION(NELEE) :: cellarea !! Element area.
   DOUBLEPRECISION, DIMENSION(NELEE) :: DXQQ     !! Element face length in the x direction.
   DOUBLEPRECISION, DIMENSION(NELEE) :: DYQQ     !! Element face length in the y direction.
   DOUBLEPRECISION, DIMENSION(NELEE) :: ZGRUND   !! Ground-surface elevation.

   CHARACTER(32) :: text32 !! Short shared text workspace.
!PRIVATE
!PUBLIC :: izero, izero1, ione, ione1, zero, zero1, half, one, one1, two, three, five, marker999, &
!          IDIMJE, DIMJE, &
!          ISZERO, ISZERO_A, LTZERO, LEZERO, GEZERO, GTZERO, NOTZERO, ISONE, NOTONE, &
!          EQMARKER, I_ISZERO_A2, fatal, err, warn, pri, &
!          ERROR, ERRC, ERRNEE, HELPPATH, ERRTOT, UZNOW, &
!          cellarea, DXQQ, DYQQ, ZGRUND
CONTAINS


!> Returns whether a double-precision value matches the integer missing-value marker.
   LOGICAL FUNCTION eqmarker(a) !needed for ad
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to compare with `marker999`.
      eqmarker = INT(a)==imarker
   END FUNCTION eqmarker

!> Returns whether a value is greater than zero.
   LOGICAL FUNCTION gtzero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
      gtzero = a>zero
   END FUNCTION gtzero

!> Returns whether a value is zero within tolerance or greater than zero.
   LOGICAL FUNCTION gezero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a>=zero
      gezero = ISZERO(a) .OR. a>zero
   END FUNCTION gezero

!> Returns whether a value is less than zero.
   LOGICAL FUNCTION ltzero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
      ltzero = a<zero
   END FUNCTION ltzero

!> Returns whether a value is zero within tolerance or less than zero.
   LOGICAL FUNCTION lezero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a<=zero
      lezero = ISZERO(a) .OR. a<zero
   END FUNCTION lezero

!> Returns whether a value is numerically zero using `vsmall`.
   LOGICAL FUNCTION iszero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a==zero
      iszero = ABS(a)<vsmall
   END FUNCTION iszero

!> Returns whether all values in a vector are numerically zero.
   LOGICAL FUNCTION iszero_a(a)
      INTEGER :: i
      DOUBLEPRECISION, DIMENSION(:), INTENT(IN) :: a !! Values to test.
      iszero_a=.TRUE.
      DO i=1,SIZE(a)
         IF(.NOT.iszero_a) CYCLE     !FOR AD
         iszero_a = iszero(a(i))
      ENDDO
   END FUNCTION iszero_a

!> Returns whether all values in an integer matrix are zero.
   LOGICAL FUNCTION i_iszero_a2(a)
      INTEGER                             :: i, j
      INTEGER, DIMENSION(:,:), INTENT(IN) :: a !! Integer values to test.
      i_iszero_a2=.TRUE.
      DO i=1,SIZE(a, DIM=1)
         DO j=1,SIZE(a, DIM=2)
            IF(.NOT.i_iszero_a2) CYCLE     !FOR AD
            i_iszero_a2 = a(i,j)==0
         ENDDO
      ENDDO
   END FUNCTION i_iszero_a2


!> Returns whether a value is not numerically zero.
   LOGICAL FUNCTION notzero(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a/=zero
      notzero = .NOT.ISZERO(a)
   END FUNCTION notzero

!> Returns whether a value is numerically one using `vsmall`.
   LOGICAL FUNCTION isone(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a==one
      isone = ABS(a-one)<vsmall
   END FUNCTION isone

!> Returns whether a value is not numerically one.
   LOGICAL FUNCTION notone(a)
      DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a/=one
      notone = .NOT.ISONE(a)
   END FUNCTION notone


!> Returns `max(x-y,0)` for integer values.
   INTEGER FUNCTION idimje(x,y)  !AD PROBLEM
      INTEGER, INTENT(IN) :: x !! Left-hand value.
      INTEGER, INTENT(IN) :: y !! Right-hand value.
      IF(x>y) THEN
         idimje = x-y
      ELSE
         idimje = 0
      ENDIF
   END FUNCTION idimje

!> Returns `max(x-y,0)` for double-precision values.
   DOUBLEPRECISION FUNCTION dimje(x,y) !AD PROBLEM
      DOUBLEPRECISION, INTENT(IN) :: x !! Left-hand value.
      DOUBLEPRECISION, INTENT(IN) :: y !! Right-hand value.
      IF(x>y) THEN
         dimje = x-y
      ELSE
         dimje = zero
      ENDIF
   END FUNCTION dimje

!> Reports a SHETRAN error or warning, updates counters, and may stop the run.
!>
!> This is the legacy error-reporting routine used by older components. It
!> formats the message with simulation time and optional element/cell context,
!> records per-module error counts, prints available help-message files in the
!> final summary, and sets timestep-reduction flags for selected flow errors.
!>
!> Severity codes:
!>
!> | `ETYPE` | Meaning |
!> |:--------|:--------|
!> | `FFFATAL` | Fatal error; print a final summary and stop via [[alstop]]. |
!> | `EEERR` | Non-fatal error. |
!> | `WWWARN` | Warning. |
!> | `0` | Summary-only path used at model shutdown. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-07-07 | SB | - | Added timestep-reduction flags for errors 1024, 1030, and 1060. |
!> @endhistory
   SUBROUTINE ERROR(ETYPE, ERRNUM, OUT, IEL, CELL, TEXT)

      ! Assumed global variables provided via host module:
      ! I_P, FFFATAL, EEERR, WWWARN, UZNOW, ERRTOT, ERRC, ERRNEE,
      ! EARRAY, ISERROR, ISERROR2, rootdir, helppath, dirqq

      IMPLICIT NONE

      ! IO-related parameters and variables
      INTEGER(KIND=I_P), INTENT(IN) :: ETYPE  !! The type of error (FFFATAL, EEERR, WWWARN). -999 triggers a help path check.
      INTEGER(KIND=I_P), INTENT(IN) :: ERRNUM !! The unique error number code.
      INTEGER(KIND=I_P), INTENT(IN) :: OUT    !! The output file unit for the message.
      INTEGER(KIND=I_P), INTENT(IN) :: IEL    !! The element number where the error occurred (optional).
      INTEGER(KIND=I_P), INTENT(IN) :: CELL   !! The cell number where the error occurred (optional).
      CHARACTER(LEN=*),  INTENT(IN) :: TEXT   !! The descriptive error text.

      INTEGER(KIND=I_P), PARAMETER :: NONE = 0
      ! Assumes ERRNEE is accessible from host module
      INTEGER(KIND=I_P), PARAMETER :: ERRCEE = (1 + ERRNEE) * 4
      INTEGER(KIND=I_P), PARAMETER :: HLP = 8

      ! Local variables
      CHARACTER(LEN=*), PARAMETER :: PATH1 = '/shetran/'
      CHARACTER(LEN=256) :: FIL,fname
      CHARACTER(LEN=256)  :: HLPMSG
      CHARACTER(LEN=1)   :: cc
      CHARACTER(LEN=1), PARAMETER :: slash = '/'

      INTEGER(KIND=I_P) :: COUNT, ERRN, AMODL
      INTEGER(KIND=I_P) :: IO_STATUS
      INTEGER(KIND=I_P) :: helpcheck !! Status from checking for help directory.

      LOGICAL :: VALID, present

      CHARACTER(LEN=11), PARAMETER :: CTYPE(3) = ['FATAL ERROR', '      ERROR', '    WARNING']

      !-------------------------------------------------------------------*

      helppath = '/helpmessages'

      ISERROR  = .FALSE.
      ISERROR2 = .FALSE.


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
         inquire(out,name=fname)

         IF (ERRTOT > 0) WRITE(*, '(A,A,A/)') ' ==> Check the pri file: "', trim(fname), '" for more details <=='

         module_loop: DO AMODL = 0, 3
            error_loop: DO ERRN = 0, ERRNEE
               COUNT = ERRC(ERRN, AMODL)

               IF (COUNT > 0) THEN
                  ! Print number of occurrences
                  WRITE(*, 9500) ERRN + AMODL * 1000, COUNT
                  WRITE(OUT, 9500) ERRN + AMODL * 1000, COUNT
                  WRITE(*, *)
                  WRITE(OUT, *)


                  ! Print contents of help file (if any)
                  WRITE(FIL, 9200) TRIM(rootdir) // TRIM(helppath) // '/', AMODL, ERRN, '.txt'
                  OPEN(HLP, FILE=FIL, STATUS='OLD', IOSTAT=IO_STATUS)
                  IF (IO_STATUS == 0) THEN
                     read_help: DO
                        READ(HLP, '(A)', IOSTAT=IO_STATUS) HLPMSG
                        IF (IO_STATUS /= 0) EXIT read_help
                        WRITE(*, '(A)') trim(HLPMSG)
                        WRITE(OUT, '(A)') trim(HLPMSG)
                     END DO read_help
                     CLOSE(HLP)
                  END IF

                  WRITE(*, *)
                  WRITE(OUT, *)
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
      &        ', iel =', I6:', cell =', I5 )
9200  FORMAT(A,I1,I3.3,A)
9500  FORMAT(' No. of occurrences of error number',I5.4,' is',I6)
9600  FORMAT(/' ### End of summary: recorded error count is',I7,' ###'/)
91003 FORMAT(' MAXIMUM DIFFERENCE (DHMAX) = ',G12.6,' METRES')
! 970804
91024 FORMAT(' DEPTH OF SURFACE WATER BELOW GROUND = ',G12.6,' METRES')
!
   END SUBROUTINE ERROR


   !> Stops the program, typically after a fatal error.
   !>
   !> This subroutine is called to stop the program, typically after a fatal
   !> error. It provides a final message to the user before termination.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:----:|:------:|-------------|
   !> | 1994-09-17 | RAH | v3.4.1: File created. |
   !> | 2000-03-07 | SB | v4g-pc: Removed IEEE calls for PC version. |
   !> @endhistory
   SUBROUTINE ALSTOP (FLAG)
      INTEGER(KIND=I_P), INTENT(IN) :: FLAG !! A flag indicating the reason for stopping. If > 0, it's a fatal error.
      ! if error_mode is true then there is no need to press enter to continue

      IF (FLAG.GT.0) THEN
         if (error_mode) then
            STOP 'Program terminating due to fatal error'
         else
            WRITE(*, '(A)') 'FATAL ERROR: Program will terminate. Press Enter to exit...'
            READ(*,*)
            STOP 'Program terminating due to fatal error'
         endif
      ENDIF
   END SUBROUTINE ALSTOP


END MODULE sglobal
