!> summary: Provides a portable, standard Fortran implementation for obtaining the input filename.
!> author: Stephen Birkinshaw (Newcastle University), Sven Berendsen (Newcastle University)
!> date: 2025-08-11
!>
!> This module is the cross-platform version for handling command-line arguments
!> to determine the simulation's input file. It replaces the Windows-specific
!> `getdirqq_winIntel.f90` and uses standard Fortran intrinsics for portability.
!>
!> @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | ? | Original | Original Windows-specific version |
!> | 2020-03-05 | SvenB | Formatting, doxygen docs and cleanup |
!> | 2025-08-11 | AI | Ported to standard Fortran, removed Windows dependencies |
MODULE GETDIRQQ

   use mod_parameters
   ! Removed Windows-specific modules:
   ! USE IFWIN, USE IFPORT, USE IFQWIN - replaced with standard Fortran

   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Private by default
   PRIVATE

   ! --------------------------------------------------------------------------
   ! Public methods
   PUBLIC  :: get_dir_and_catch

   ! Code =====================================================================

CONTAINS

   !> summary: Obtains the input directory and runfile from command-line arguments.
   !>
   !> This is the main entry point for the module. It parses command-line arguments
   !> to identify the run data file. It supports getting the filename directly (`-f`)
   !> or looking it up from a `catchments.txt` file (`-c`). It replaces the
   !> Windows GUI dialogs with a pure command-line interface.
   SUBROUTINE get_dir_and_catch(runfil, fn, catch, dirqq, rootdir)

      ! IO-vars
      CHARACTER(len=*), INTENT(IN)    :: runfil   !! The runfile name (often unused in command-line mode)
      CHARACTER(len=*), INTENT(OUT)   :: fn       !! The base name of the runfile
      CHARACTER(len=*), INTENT(OUT)   :: catch    !! The catchment name (legacy, now unused)
      CHARACTER(len=*), INTENT(OUT)   :: dirqq    !! The directory path of the runfile
      CHARACTER(len=*), INTENT(OUT)   :: rootdir  !! The root directory where the executable was run

      ! Other vars
      INTEGER(kind=I_P)               :: length, i, na, j, k, last_slash
      CHARACTER(len=*), PARAMETER     :: catchment_file='catchments.txt'
      CHARACTER(len=LENGTH_LINE)      :: message, dum1, dum2, code
      CHARACTER(len=LENGTH_FILEPATH)  :: cli_argument
      CHARACTER(len=LENGTH_FILEPATH)  :: fn_part
      LOGICAL                         :: ex

      ! Code =================================================================

      ! Get current working directory (portable replacement for GETDRIVEDIRQQ)
      CALL GET_CURRENT_DIR(rootdir)

      ! Get command line arguments (portable replacement for NARGS/GETARG)
      na = COMMAND_ARGUMENT_COUNT()
      IF(na > 0) THEN
         CALL GET_COMMAND_ARGUMENT(1, code)
      ELSE
         code = '-f'  ! Default to filename mode for portable version
      ENDIF

      message = ''
      SELECT CASE(code)
       CASE ('-f') ! treat as filename - main portable mode
         IF(na < 2) THEN
            message = 'Missing filename. Usage: shetran -f filename.txt'
            GOTO 1000
         ENDIF
         CALL GET_COMMAND_ARGUMENT(2, cli_argument)

       CASE ('-c')  ! treat as catchment name (kept for compatibility)
         IF (na < 2) THEN
            cli_argument = 'default'
         ELSE
            CALL GET_COMMAND_ARGUMENT(2, cli_argument)
         ENDIF
         INQUIRE(FILE=catchment_file, exist=ex)
         IF(ex) THEN
            OPEN(875,FILE=catchment_file, ERR=999)
            DO
               read(875,'(A,a)', END=999, ERR=999) dum1
               read(875,*, END=999, ERR=999) dum2
               IF(dum1==cli_argument) EXIT
            ENDDO
            cli_argument = dum2
            CLOSE(875)
         ELSE
            message='Cannot find file ' // TRIM(catchment_file) // ' in executable directory'
         ENDIF

       CASE ('-a', '-m', '-af', '-sd', '-pattern', '-delinc', '-results')
         ! Windows GUI modes not supported in portable version
         message = 'Interactive file selection not supported in portable version. Use: shetran -f filename.txt'
         GOTO 1000

       CASE DEFAULT
         message = 'Unrecognised command line argument ' // TRIM(code) // &
            '. Portable version supports: -f filename, -c catchment'
      END SELECT

      IF(message/='') GOTO 1000

      INQUIRE(FILE=cli_argument, EXIST=ex)
      IF(.NOT.ex) THEN
         IF(LEN_TRIM(cli_argument)==0) THEN
            message = 'Missing filename. Use: shetran -f filename.txt'
         ELSE
            message = 'Cannot find rundata file '//TRIM(cli_argument)
         ENDIF
         CALL handle_command_line_error(message)
      ENDIF

      ! Portable path splitting (replacement for SPLITPATHQQ)
      CALL SPLIT_PATH_PORTABLE(cli_argument, dirqq, fn_part)

      fn = trim(fn_part)
      catch = ''  ! Catchment info not used in modern version

      RETURN

999   CONTINUE
      message = 'Error reading catchment file'
      CLOSE(875, ERR=1000)

1000  CONTINUE
      ! Error handling - write to standard error
      WRITE(*,'(A)') 'ERROR: ' // TRIM(message)
      WRITE(*,'(A)') 'Usage: shetran -f rundata_file.txt'
      WRITE(*,'(A)') '   or: shetran -c catchment_name'
      STOP 1

   END SUBROUTINE get_dir_and_catch


   !> summary: Gets the current working directory in a portable way.
   !>
   !> Uses the standard Fortran 2008 `GET_ENVIRONMENT_VARIABLE` intrinsic to
   !> retrieve the 'PWD' variable. If that fails, it defaults to '.' (current directory).
   SUBROUTINE GET_CURRENT_DIR(current_dir)

      CHARACTER(len=*), INTENT(OUT) :: current_dir  !! The path of the current working directory

      ! Use standard Fortran intrinsic (F2008)
      CALL GET_ENVIRONMENT_VARIABLE('PWD', current_dir)

      ! If PWD not available, try alternative approaches
      IF (LEN_TRIM(current_dir) == 0) THEN
         current_dir = '.'  ! Current directory
      ENDIF

   END SUBROUTINE GET_CURRENT_DIR

   !> summary: Splits a full file path into its directory and filename components.
   !>
   !> This is a portable replacement for the non-standard `SPLITPATHQQ` function.
   !> It searches for the last path separator (`/` or `\`) to split the string.
   SUBROUTINE SPLIT_PATH_PORTABLE(fullpath, dir_part, file_part)

      CHARACTER(len=*), INTENT(IN)  :: fullpath   !! The full path of the file to split
      CHARACTER(len=*), INTENT(OUT) :: dir_part   !! The directory part of the path
      CHARACTER(len=*), INTENT(OUT) :: file_part  !! The filename part of the path

      INTEGER :: last_slash, i, len_path

      len_path = LEN_TRIM(fullpath)
      last_slash = 0

      ! Find last slash (works for both / and \ path separators)
      DO i = len_path, 1, -1
         IF (fullpath(i:i) == '/' .OR. fullpath(i:i) == '\') THEN
            last_slash = i
            EXIT
         ENDIF
      END DO

      IF (last_slash > 0) THEN
         ! Keep trailing separator for compatibility with legacy code that
         ! concatenates as TRIM(dirqq)//filename.
         dir_part = fullpath(1:last_slash)
         file_part = fullpath(last_slash+1:len_path)
      ELSE
         ! No path separator found: keep directory empty so concatenation keeps
         ! relative filenames unchanged.
         dir_part = ''
         file_part = fullpath
      ENDIF

      ! Keep empty directory as-is for compatibility.

   END SUBROUTINE SPLIT_PATH_PORTABLE


   !> summary: Handles command-line argument errors by printing a message and stopping.
   !>
   !> Centralizes error reporting for command-line issues, providing a consistent
   !> usage message before terminating the program with a non-zero exit code.
   SUBROUTINE handle_command_line_error(error_msg)

      CHARACTER(len=*), INTENT(IN) :: error_msg  !! The specific error message to display

      ! Error handling - write to standard error
      WRITE(*,'(A)') 'ERROR: ' // TRIM(error_msg)
      WRITE(*,'(A)') 'Usage: shetran -f rundata_file.txt'
      WRITE(*,'(A)') '   or: shetran -c catchment_name'
      STOP 1

   END SUBROUTINE handle_command_line_error

END MODULE GETDIRQQ
