!> summary: Error handling.
!> author: Sven Berendsen, Southampton University
!> date: 2026-05-12
!>
!> This module provides various methods for error handling, including error
!> reporting, errormessage formatting and logging. It is designed to be used
!> throughout the Shetran codebase to ensure consistent error reporting and
!>handling practices.
!>
!> @history
!> | Date       | Author | Version | Description                               |
!> |:-----------|:-------|:--------|:------------------------------------------|
!> | 2026-05-12 | SvB     | -       | Initial version, partly extracted from sglobal.f90 |
!>
!> @note Currently, the interface for the error methods is still the same for easier integration,
!> but it will be updated in the future to be more flexible and user-friendly.
!>
!> @todo - Figure a way to do ISERROR and ISERROR2 more elegantly - best might be with the general state tracker
!>
module mod_error

   use mod_parameters, only: I_P, LENGTH_FILEPATH, LENGTH_LINE
   use sglobal, only: UZNOW, EARRAY, rootdir, dirqq, ISERROR, ISERROR2

   implicit none


   private


   ! Error levels
   INTEGER(KIND=I_P), PARAMETER :: FFFATAL = 1 !! Error type for fatal errors.
   INTEGER(KIND=I_P), PARAMETER :: EEERR = 2 !! Error type for non-fatal errors.
   INTEGER(KIND=I_P), PARAMETER :: WWWARN = 3 !! Error type for warnings.

   ! Error handling control
   INTEGER(KIND=I_P), PARAMETER :: ERRNEE = 100 !! Max number of distinct error codes per module.
   INTEGER(KIND=I_P) :: ERRC(0:ERRNEE, 0:3) = 0 !! Counters for error occurrences.
   INTEGER(KIND=I_P) :: ERRTOT = 0 !! Total count of all errors and warnings.
   LOGICAL :: flag_wait_on_exit = .FALSE. !! Flag to control waiting for user input before exiting on fatal error.

   ! Error file information
   INTEGER(KIND=I_P), PARAMETER :: pppri = 23 !! File unit for primary output.
   CHARACTER(LEN=LENGTH_FILEPATH) :: helppath !! Path to help message files (use LENGTH_FILEPATH for portability)


   PUBLIC :: ERROR, ALSTOP, err_set_wait_on_exit
   PUBLIC :: FFFATAL, EEERR, WWWARN
   PUBLIC :: pppri
   PUBLIC :: ERRNEE

contains

   !> Sets whether the program should wait for user input before exiting on a fatal error.
   !> @author S. Berendsen, Southampton University
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:----:|:------:|-------------|
   !> | 2026-05-15 | SvB | Initial Version |
   !>
   SUBROUTINE err_set_wait_on_exit(wait)
      LOGICAL, INTENT(IN) :: wait !! If .TRUE., the program will wait for user input before exiting on a fatal error.

      flag_wait_on_exit = wait
   END SUBROUTINE err_set_wait_on_exit



   !> Prints an error message, updates error counters, and optionally stops the program.
   !>
   !> @author R. A. Heath, Newcastle University
   !>
   !> This is the central error handling routine for SHETRAN. It formats and
   !> prints error messages, maintains a count of different errors, and can
   !> terminate the simulation for fatal errors. It can also print a summary
   !> of all recorded errors and associated help messages.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:----:|:------:|-------------|
   !> | 1994-10-08 | RAH | v3.4.1: Created from v3.4., for changes see below. |
   !> | 1997-08-04 | RAH | v4.1: Use EARRAY for error 1024. |
   !> | 1997-08-11 | RAH | Added EXTERNAL after INCLUDE. |
   !>
   !> Changes in v3.4.1 (from v3.4):
   !>
   !> - replace common counter arrays with local ERRC
   !> - extend ERRNUM range below 1000
   !> - introduce ETYPE=0
   !> - print IEL, CELL only if non-zero
   !> - print help files along with final summary
   !> - declare everything
   !> - no INTEGER*2
   !> - test subscript ranges
   !> - tidy comments
   !> - call ALSTOP to stop
   !> - use local IFATAL etc instead of common FATAL etc
   !> - 1024 no longer uses EARRAY
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
      CHARACTER(LEN=256) :: FIL
      CHARACTER(LEN=LENGTH_LINE)  :: HLPMSG
      CHARACTER(LEN=1)   :: cc
      CHARACTER(LEN=1), PARAMETER :: slash = '/'

      INTEGER(KIND=I_P) :: COUNT, ERRN, AMODL
      INTEGER(KIND=I_P) :: IO_STATUS
      INTEGER(KIND=I_P) :: helpcheck !! Status from checking for help directory.

      LOGICAL :: VALID, present

      ! Modernization Fix: Replaced legacy DATA statement with a strict PARAMETER array
      CHARACTER(LEN=11), PARAMETER :: CTYPE(3) = ['FATAL ERROR', '      ERROR', '    WARNING']

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
         WRITE(*, '(//A/A/)') ' ### Error summary and Advice ###', '  ------------------------'

         IF (ERRTOT > 0) WRITE(*, '(A/)') ' ==> Check printed output files for more details <=='

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

                  WRITE(*, '(A)', ADVANCE='NO') 'Press Enter to continue...'
                  READ(*, *)

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
      INTEGER(KIND=I_P), INTENT(IN) :: FLAG !! A flag indicating the reason for stopping. If > 0, it's a fatal error.

      IF (FLAG > 0 .and. flag_wait_on_exit) THEN
         WRITE(*, '(A)') 'FATAL ERROR: Program will terminate. Press Enter to exit...'
         READ(*,*)
         STOP 'Program terminating due to fatal error'
      ELSE IF (FLAG > 0) THEN
         STOP 'Program terminating due to fatal error'
      ENDIF
   END SUBROUTINE ALSTOP

end module mod_error
