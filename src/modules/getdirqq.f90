!> summary: Cross-platform rundata selection with optional Intel QuickWin support.
!> author: Stephen Birkinshaw (Newcastle University), Sven Berendsen (Newcastle University)
MODULE GETDIRQQ

   USE mod_parameters
   USE sglobal, ONLY : error_mode
   USE stdlib_system, ONLY : base_name, dir_name, get_cwd

#ifdef SHETRAN_HAVE_QUICKWIN
   USE IFWIN
#endif

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: get_dir_and_catch

#ifdef SHETRAN_HAVE_QUICKWIN
   CHARACTER(LEN=LENGTH_FILEPATH) :: FileName
#endif

CONTAINS

   !> summary: Obtains the input directory and catches command-line errors.
   SUBROUTINE get_dir_and_catch(runfil, fn, catch, dirqq, rootdir)

      CHARACTER(LEN=*), INTENT(IN)  :: runfil
      CHARACTER(LEN=*), INTENT(OUT) :: fn, catch, dirqq
      CHARACTER(LEN=*), INTENT(OUT) :: rootdir

      CHARACTER(LEN=*), PARAMETER    :: catchment_file = 'catchments.txt'
      CHARACTER(LEN=LENGTH_LINE)     :: message, dum1, dum2, code
      CHARACTER(LEN=LENGTH_FILEPATH) :: cli_argument, fn_part
      LOGICAL                        :: ex, found_catchment
      INTEGER                        :: dir_len, ios, na
#ifdef SHETRAN_HAVE_QUICKWIN
      INTEGER(KIND=I_P)              :: ierror
      LOGICAL(KIND=4)                :: bret
      CHARACTER(LEN=LENGTH_FILEPATH) :: allfilters
      CHARACTER(LEN=60)              :: dlgtitle
      TYPE(T_OPENFILENAME)           :: opn
      INTEGER                        :: null_pos
#endif

      CALL get_current_dir(rootdir)

      error_mode = .FALSE.
      na = COMMAND_ARGUMENT_COUNT()

      IF (na > 0) THEN
         CALL GET_COMMAND_ARGUMENT(1, code)
      ELSE
#ifdef SHETRAN_HAVE_QUICKWIN
         code = '-a'
#else
         code = '-f'
#endif
      END IF

      message = ''

      SELECT CASE(TRIM(code))
#ifdef SHETRAN_HAVE_QUICKWIN
      CASE('-a')
         FileName = CHAR(0)
         allfilters = 'rundata files (*rundata*.txt)' // CHAR(0) // '*rundata*.txt' // CHAR(0) // &
                       'All files (*.*)' // CHAR(0) // '*.*' // CHAR(0) // CHAR(0)
         dlgtitle = 'Select a SHETRAN rundata file'C

         opn%lStructSize       = 0
         opn%HWNDOWNER         = NULL
         opn%HINSTANCE         = NULL
         opn%LPSTRFILTER       = NULL
         opn%LPSTRCUSTOMFILTER = NULL
         opn%NMAXCUSTFILTER    = 0
         opn%NFILTERINDEX      = 0
         opn%LPSTRFILE         = NULL
         opn%NMAXFILE          = 0
         opn%LPSTRFILETITLE    = NULL
         opn%NMAXFILETITLE     = 0
         opn%LPSTRINITIALDIR   = NULL
         opn%LPSTRTITLE        = NULL
         opn%FLAGS             = 0
         opn%NFILEOFFSET       = 0
         opn%NFILEEXTENSION    = 0
         opn%LPSTRDEFEXT       = NULL
         opn%LCUSTDATA         = 0
         opn%LPFNHOOK          = NULL
         opn%LPTEMPLATENAME    = NULL
         opn%PVRESERVED        = NULL
         opn%DWRESERVED        = 0
         opn%FLAGSEX           = 0

         opn%lStructSize       = SIZEOF(opn)
         opn%LPSTRFILTER       = LOC(allfilters)
         opn%NFILTERINDEX      = 1
         opn%LPSTRFILE         = LOC(FileName)
         opn%NMAXFILE          = LEN(FileName)
         opn%LPSTRTITLE        = LOC(dlgtitle)
         opn%FLAGS             = OFN_EXPLORER + OFN_FILEMUSTEXIST + OFN_PATHMUSTEXIST + OFN_NOCHANGEDIR

         bret = GETOPENFILENAME(opn)
         CALL comdlger(ierror)

         IF (.NOT. bret) THEN
            CALL print_usage_and_stop('No rundata file selected')
         END IF

         null_pos = INDEX(FileName, CHAR(0))
         IF (null_pos > 1) THEN
            cli_argument = FileName(1:null_pos - 1)
         ELSE
            cli_argument = FileName
         END IF
#endif

      CASE('-f')
         IF (na < 2) THEN
            CALL print_usage_and_stop('Missing filename. Usage: shetran -f filename.txt')
         END IF
         CALL GET_COMMAND_ARGUMENT(2, cli_argument)

      CASE('-c')
         IF (na < 2) THEN
            cli_argument = 'default'
         ELSE
            CALL GET_COMMAND_ARGUMENT(2, cli_argument)
         END IF

         INQUIRE(FILE=catchment_file, EXIST=ex)
         IF (ex) THEN
            OPEN(UNIT=875, FILE=catchment_file, STATUS='OLD', IOSTAT=ios)
            IF (ios /= 0) CALL print_usage_and_stop('Error reading catchment file')

            found_catchment = .FALSE.

            read_catchment: DO
               READ(875, '(A)', IOSTAT=ios) dum1
               IF (ios /= 0) EXIT read_catchment

               READ(875, *, IOSTAT=ios) dum2
               IF (ios /= 0) EXIT read_catchment

               IF (TRIM(dum1) == TRIM(cli_argument)) THEN
                  cli_argument = dum2
                  found_catchment = .TRUE.
                  EXIT read_catchment
               END IF
            END DO read_catchment

            CLOSE(875)

            IF (.NOT. found_catchment) THEN
               CALL print_usage_and_stop('Cannot find catchment ' // TRIM(cli_argument) // ' in ' // catchment_file)
            END IF
         ELSE
            message = 'Cannot find file ' // catchment_file // ' in executable directory'
         END IF

#ifndef SHETRAN_HAVE_QUICKWIN
      CASE('-a', '-m', '-af', '-sd', '-pattern', '-delinc', '-results')
         CALL print_usage_and_stop('Interactive file selection requires Intel QuickWin on Windows. Use: shetran -f filename.txt')
#endif

      CASE DEFAULT
         message = 'Unrecognised command line argument ' // TRIM(code) // '. Recognise only -a, -c and -f'
      END SELECT

      CALL set_error_mode_from_arguments()

      IF (message /= '') CALL print_usage_and_stop(message)

      INQUIRE(FILE=cli_argument, EXIST=ex)
      IF (.NOT. ex) THEN
         IF (LEN_TRIM(cli_argument) == 0) THEN
            message = 'Missing filename. Use: shetran -f filename.txt'
         ELSE
            message = 'Cannot find rundata file ' // TRIM(cli_argument)
         END IF
         CALL handle_command_line_error(message)
      END IF

      IF (INDEX(cli_argument, '/') == 0 .AND. INDEX(cli_argument, '\') == 0) THEN
         dirqq = '.'
         fn_part = TRIM(cli_argument)
      ELSE
         dirqq = dir_name(TRIM(cli_argument))
         fn_part = base_name(TRIM(cli_argument))
         dir_len = LEN_TRIM(dirqq)
         IF (dir_len > 1) THEN
            IF (dirqq(dir_len:dir_len) == '/' .OR. dirqq(dir_len:dir_len) == '\') THEN
               IF (.NOT. (dir_len == 3 .AND. dirqq(2:2) == ':')) THEN
                  dirqq = dirqq(1:dir_len - 1)
               END IF
            END IF
         END IF
      END IF

      fn = TRIM(fn_part)
      catch = derive_catch_from_filename(fn)

   CONTAINS

      FUNCTION derive_catch_from_filename(filename) RESULT(catch_name)
         CHARACTER(LEN=*), INTENT(IN) :: filename
         CHARACTER(LEN=LENGTH_FILEPATH) :: catch_name
         CHARACTER(LEN=LENGTH_FILEPATH) :: stem
         INTEGER :: dot_pos

         stem = TRIM(filename)
         dot_pos = INDEX(stem, '.', BACK=.TRUE.)
         IF (dot_pos > 1) stem = stem(1:dot_pos - 1)

         IF (LEN_TRIM(stem) > 8) THEN
            IF (stem(1:8) == 'rundata_') THEN
               catch_name = TRIM(stem(9:))
               RETURN
            END IF
         END IF

         catch_name = TRIM(stem)
      END FUNCTION derive_catch_from_filename

      SUBROUTINE print_usage_and_stop(err_msg)
         CHARACTER(LEN=*), INTENT(IN) :: err_msg

         WRITE(*, '(A)') 'ERROR: ' // TRIM(err_msg)
         WRITE(*, '(A)') 'Usage: shetran -f rundata_file.txt'
         WRITE(*, '(A)') '   or: shetran -c catchment_name'
         STOP 1
      END SUBROUTINE print_usage_and_stop

      SUBROUTINE set_error_mode_from_arguments()
         INTEGER :: arg_index
         CHARACTER(LEN=LENGTH_LINE) :: argument

         DO arg_index = 1, na
            CALL GET_COMMAND_ARGUMENT(arg_index, argument)
            SELECT CASE(TRIM(argument))
            CASE('-error')
               error_mode = .TRUE.
            END SELECT
         END DO
      END SUBROUTINE set_error_mode_from_arguments

   END SUBROUTINE get_dir_and_catch

   !> summary: Gets the current working directory through Fortran stdlib.
   SUBROUTINE get_current_dir(current_dir)

      CHARACTER(LEN=*), INTENT(OUT) :: current_dir
      CHARACTER(LEN=:), ALLOCATABLE :: cwd

      CALL get_cwd(cwd)

      IF (ALLOCATED(cwd)) THEN
         current_dir = cwd
      ELSE
         current_dir = '.'
      END IF

   END SUBROUTINE get_current_dir

   !> summary: Handles command-line argument errors.
   SUBROUTINE handle_command_line_error(error_msg)

      CHARACTER(LEN=*), INTENT(IN) :: error_msg

      WRITE(*, '(A)') 'ERROR: ' // TRIM(error_msg)
      WRITE(*, '(A)') 'Usage: shetran -f rundata_file.txt'
      WRITE(*, '(A)') '   or: shetran -c catchment_name'
      STOP 1

   END SUBROUTINE handle_command_line_error

#ifdef SHETRAN_HAVE_QUICKWIN
   !> summary: Error handling for Windows common dialog errors.
   SUBROUTINE comdlger(iret)

      INTEGER(KIND=I_P), INTENT(OUT) :: iret
      CHARACTER(30) :: msg1
      CHARACTER(210) :: msg2

      iret = COMMDLGEXTENDEDERROR()
      msg1 = 'FILE OPEN DIALOG FAILURE'C

      SELECT CASE(iret)
      CASE (CDERR_FINDRESFAILURE)
         msg2 = 'The common dialog box procedure failed to find a specified resource.'C
      CASE (CDERR_INITIALIZATION)
         msg2 = 'The common dialog box procedure failed during initialization.'C
      CASE (CDERR_LOCKRESFAILURE)
         msg2 = 'The common dialog box procedure failed to lock a specified resource.'C
      CASE (CDERR_LOADRESFAILURE)
         msg2 = 'The common dialog box procedure failed to load a specified resource.'C
      CASE (CDERR_LOADSTRFAILURE)
         msg2 = 'The common dialog box procedure failed to load a specified string.'C
      CASE (CDERR_MEMALLOCFAILURE)
         msg2 = 'The common dialog box procedure was unable to allocate memory for internal structures.'C
      CASE (CDERR_MEMLOCKFAILURE)
         msg2 = 'The common dialog box procedure was unable to lock memory associated with a handle.'C
      CASE (CDERR_NOHINSTANCE)
         msg2 = 'The common dialog box requires an instance handle but none was provided.'C
      CASE (CDERR_NOHOOK)
         msg2 = 'The common dialog box requires a hook procedure but none was provided.'C
      CASE (CDERR_NOTEMPLATE)
         msg2 = 'The common dialog box requires a template but none was provided.'C
      CASE (CDERR_STRUCTSIZE)
         msg2 = 'The common dialog box structure size is invalid.'C
      CASE (FNERR_BUFFERTOOSMALL)
         msg2 = 'The buffer for a filename is too small.'C
      CASE (FNERR_INVALIDFILENAME)
         msg2 = 'A filename is invalid.'C
      CASE (FNERR_SUBCLASSFAILURE)
         msg2 = 'An attempt to subclass a list box failed because insufficient memory was available.'C
      CASE DEFAULT
         msg2 = 'Unknown error number'C
      END SELECT

      IF (iret /= 0) THEN
         PRINT *, msg1
         PRINT *, msg2
         STOP
      END IF

   END SUBROUTINE comdlger
#endif

END MODULE GETDIRQQ
