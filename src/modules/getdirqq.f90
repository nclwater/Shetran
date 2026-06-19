!> summary: Provides a portable, standard Fortran implementation for obtaining the input filename.
!> author: Stephen Birkinshaw (Newcastle University), Sven Berendsen (Newcastle University)
!> date: 2025-08-11
!>
!> This module is the cross-platform version for handling command-line arguments
!> to determine the simulation's input file. It replaces the Windows-specific
!> `getdirqq_winIntel.f90` and uses standard Fortran intrinsics for portability.
!> A popup window can be used is there is a fortran compiler on Windows.
!>
!> @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | ? | Original | Original Windows-specific version |
!> | 2020-03-05 | SvenB | Formatting, doxygen docs and cleanup |
!> | 2025-08-11 | AI | Ported to standard Fortran, removed Windows dependencies |
!> | 2026-06-02 | SB | added popup window back into the code |
MODULE GETDIRQQ

    use mod_parameters
    use sglobal, only : error_mode

#if defined(__INTEL_COMPILER) && defined(_WIN32)
#define USE_QUICKWIN 1
#endif    
#ifdef USE_QUICKWIN
    USE IFWIN
    USE IFQWIN, ONLY : QWIN$FRAMEWINDOW, GETHWNDQQ
#endif

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

      ! Assumed external module dependencies providing global variables/kinds:
      ! I_P, LENGTH_LINE, LENGTH_FILEPATH, GET_CURRENT_DIR, SPLIT_PATH_PORTABLE,
      ! handle_command_line_error

      IMPLICIT NONE

      ! IO-vars
      CHARACTER(len=*), INTENT(IN)    :: runfil   !! The base of runfile name (often unused in command-line mode) e.g rundata_
      CHARACTER(len=*), INTENT(OUT)   :: fn       !! The full path of the runfile e.g. C:\shetran\rundata_slapton.txt
      CHARACTER(len=*), INTENT(OUT)   :: catch    !! The catchment name e.g. slapton
      CHARACTER(len=*), INTENT(OUT)   :: dirqq    !! The directory path of the runfile
      CHARACTER(len=*), INTENT(OUT)   :: rootdir  !! The root directory where the executable was run

      ! Other vars
      INTEGER(kind=I_P)               :: length, i, na, j, k, last_slash
      CHARACTER(len=*), PARAMETER     :: catchment_file='catchments.txt'
      CHARACTER(len=LENGTH_LINE)      :: message, dum1, dum2, code
      CHARACTER(len=LENGTH_FILEPATH)  :: cli_argument
      CHARACTER(len=LENGTH_FILEPATH)  :: fn_part
      LOGICAL                         :: ex, found_catchment
      INTEGER                         :: ios
#ifdef USE_QUICKWIN
        TYPE(T_OPENFILENAME) :: opn
      CHARACTER(len=LENGTH_FILEPATH)  :: ALLFILTERS
      CHARACTER(len=LENGTH_FILEPATH)  :: DLGTITLE
      LOGICAL                         :: bRET
      INTEGER(kind=I_P)               :: IERROR
#endif       

      ! Code =================================================================

      ! Get current working directory (portable replacement for GETDRIVEDIRQQ)
      error_mode = .FALSE.
      CALL GET_CURRENT_DIR(rootdir)

      ! Get command line arguments (portable replacement for NARGS/GETARG)
      na = COMMAND_ARGUMENT_COUNT()
        IF(na>0) THEN
         CALL GET_COMMAND_ARGUMENT(1, code)
        ELSE
#ifdef USE_QUICKWIN
            code = '-a'  !popup window is default if there is a fortran compiler on Windows
#else
            code = '-f'  !otherwise filename is default and user must provide it as an argument
#endif           
        ENDIF
        !write(*,*) 'Debug: Command line argument count=', na, ' first argument (code)=', TRIM(code)

      message = ''
      SELECT CASE(code)
#ifdef USE_QUICKWIN
       CASE ('-a') !use popup
            ALLFILTERS            =  'rundata files (*rundata*.txt)'//CHAR(0)//'*rundata*.txt'//CHAR(0)// &
                                    'All files (*.*)'//CHAR(0)//'*.*'//CHAR(0)//CHAR(0)
            DLGTITLE              = 'Select a SHETRAN rundata file'C
            opn%lStructSize       = SIZEOF(Opn)
            opn%HWNDOWNER         = GETHWNDQQ(QWIN$FRAMEWINDOW)
            opn%HINSTANCE         = NULL
            opn%LPSTRFILTER       = LOC(ALLFILTERS)
            opn%LPSTRCUSTOMFILTER = NULL
            opn%NMAXCUSTFILTER    = NULL
            opn%NFILTERINDEX      = 1
            opn%LPSTRFILE         = LOC(cli_argument) 
            opn%NMAXFILE          = LEN(cli_argument) 
            opn%LPSTRFILETITLE    = NULL 
            opn%NMAXFILETITLE     = NULL
            opn%LPSTRINITIALDIR   = NULL
            opn%LPSTRTITLE        = LOC(DLGTITLE)
            opn%FLAGS             = NULL 
            opn%NFILEOFFSET       = NULL
            opn%NFILEEXTENSION    = NULL
            opn%LPSTRDEFEXT       = NULL
            opn%LCUSTDATA         = NULL
            opn%LPFNHOOK          = NULL
            opn%LPTEMPLATENAME    = NULL 
            bRET                  = GETOPENFILENAME(opn)
            CALL COMDLGER(IERROR)
#endif
      CASE ('-f') ! treat as filename - main portable mode
         CALL GET_COMMAND_ARGUMENT(2, cli_argument)

      CASE ('-c')  ! treat as catchment name (kept for compatibility)
            IF (na<2) THEN
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
               ! Standardized '(A,a)' format typo to '(A)'
               READ(875, '(A)', IOSTAT=ios) dum1
               IF (ios /= 0) EXIT read_catchment

               READ(875, *, IOSTAT=ios) dum2
               IF (ios /= 0) EXIT read_catchment

               IF (TRIM(dum1) == TRIM(cli_argument)) THEN
                   !write(*,*) 'Debug: Found catchment match in file: ', TRIM(dum1), ' with filename: ', TRIM(dum2)
                  cli_argument = dum2
                  found_catchment = .TRUE.
                  EXIT read_catchment
               END IF
            END DO read_catchment

            CLOSE(875)

            ! If we hit EOF without a match, it mirrors the old END=999 behavior
            IF (.NOT. found_catchment) THEN
               CALL print_usage_and_stop('Error reading catchment file')
            END IF
         ELSE
            message = 'Cannot find file ' // TRIM(catchment_file) // ' in executable directory'
         END IF

      CASE DEFAULT
         message = 'Unrecognised command line argument ' // TRIM(code) // &
                   '. Portable version supports: -f filename, -c catchment, -a popup with intel compiler on windows'
      END SELECT
        !
        ! addtional -error option at the end of the command line argument
        ! if this is present when an error message is produced the smiluation does not require a manual enter command to continue
        IF (na==3) THEN
          CALL GET_COMMAND_ARGUMENT(3, cli_argument)
            SELECT CASE(code)
            CASE('-error')
                error_mode = .TRUE.
            END SELECT
        ENDIF

      IF (message /= '') CALL print_usage_and_stop(message)

      !write(*,*) 'Debug: cli_argument after processing=', TRIM(cli_argument)
      INQUIRE(FILE=cli_argument, EXIST=ex)
      IF (.NOT. ex) THEN
         IF (LEN_TRIM(cli_argument) == 0) THEN
            message = 'Missing filename. Use: shetran -f filename.txt'
         ELSE
            message = 'Cannot find rundata file ' // TRIM(cli_argument)
         END IF
         CALL handle_command_line_error(message)
      END IF

      ! Portable path splitting (replacement for SPLITPATHQQ)
      CALL SPLIT_PATH_PORTABLE(cli_argument, dirqq, fn_part)
      
      !write(*,*) 'Debug: cli_argument=', TRIM(cli_argument), ' dirqq=', TRIM(dirqq), ' fn_part=', TRIM(fn_part)

      fn = TRIM(dirqq)//TRIM(fn_part)

      ! Restore legacy naming behavior for output files by deriving CNAM from
      ! the rundata filename (e.g., rundata_slapton.txt -> slapton).
      catch = derive_catch_from_filename(fn_part)

      RETURN

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

      ! Replaces the old 1000 GOTO block
      SUBROUTINE print_usage_and_stop(err_msg)
         CHARACTER(LEN=*), INTENT(IN) :: err_msg

         WRITE(*,'(A)') 'ERROR: ' // TRIM(err_msg)
         WRITE(*,'(A)') 'Usage: shetran -f rundata_file.txt'
         WRITE(*,'(A)') '   or: shetran -c catchment_name'
         STOP 1
      END SUBROUTINE print_usage_and_stop

   END SUBROUTINE get_dir_and_catch
   
   
    !! Error handling for common dialog errors..
    ! 
    ! REVISION HISTORY:
    ! 20200305 - SvenB - formatting & cleanup
    ! 20260602 - SB - added popup window back into the code if there is an Intel fortran compiler on Windows.
    !--------------------------------------------------------------------------- 
#ifdef USE_QUICKWIN
    SUBROUTINE comdlger(IRET)
        
        ! IO-Vars
        INTEGER(KIND=I_P)   :: IRET

        ! Other vars
        CHARACTER(30)       :: MSG1
        CHARACTER(210)      :: MSG2

        ! Code =================================================================

        IRET = COMMDLGEXTENDEDERROR()
        MSG1 = 'FILE OPEN DIALOG FAILURE'C
        
        SELECT CASE(IRET)

        CASE (CDERR_FINDRESFAILURE)
            MSG2 = 'The common dialog box procedure failed to find a specified resource.'C

        CASE (CDERR_INITIALIZATION)
            MSG2 = 'The common dialog box procedure failed during initialization. &
            This error often occurs when insufficient memory is available.'C

        CASE (CDERR_LOCKRESFAILURE)
            MSG2 = 'The common dialog box procedure failed to lock a specified resource.'C

        CASE (CDERR_LOADRESFAILURE)
            MSG2 = 'The common dialog box procedure failed to load a specified resource.'C

        CASE (CDERR_LOADSTRFAILURE)
            MSG2 = 'The common dialog box procedure failed to load a specified string.'C

        CASE (CDERR_MEMALLOCFAILURE)
            MSG2 = 'The common dialog box procedure was unable to allocate memory for &
            internal structures.'C

        CASE (CDERR_MEMLOCKFAILURE)
            MSG2 = 'The common dialog box procedure was unable to lock the memory associated &
            with a handle.'C

        CASE (CDERR_NOHINSTANCE)
            MSG2 = 'The ENABLETEMPLATE flag was specified in the Flags member of a structure &
            for the corresponding common dialog box, but the application failed to provide a &
            corresponding instance handle.'C

        CASE (CDERR_NOHOOK)
            MSG2 = 'The ENABLEHOOK flag was specified in the Flags member of a structure for &
            the corresponding common dialog box, but the application failed to provide a &
            pointer to a corresponding hook function'C

        CASE (CDERR_NOTEMPLATE)
            MSG2 = 'The ENABLETEMPLATE flag was specified in the Flags member of a structure &
            for the corresponding common dialog box, but the application failed to provide a &
            corresponding template.'C

        CASE (CDERR_STRUCTSIZE)
            MSG2 = 'The lStructSize member of a structure for the corresponding common dialog &
            box is invalid.'C

        CASE (FNERR_BUFFERTOOSMALL)
            MSG2 = 'The buffer for a filename is too small. (This buffer is pointed to by the &
            lpstrFile member of the structure for a common dialog box.)'C

        CASE (FNERR_INVALIDFILENAME)
            MSG2 = 'A filename is invalid.'C

        CASE (FNERR_SUBCLASSFAILURE)
            MSG2 = 'An attempt to subclass a list box failed because insufficient memory was &
            available.'C

        CASE DEFAULT
            MSG2 = 'Unknown error number'C

        END SELECT

        IF(IRET /= 0)THEN
            PRINT*, MSG1
            PRINT*, MSG2
            STOP
        ENDIF

    END SUBROUTINE comdlger
#endif



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
