!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
!-------------------------------------------------------------------------------
!
!> @file GETDIRQQ.f90
!
!> @author Stephen Birkinshaw, Newcastle University (Original)
!> @author GitHub Copilot (Standard Fortran Port)
!
!> @brief Gets the input filename(s) - PORTABLE VERSION
!
! REVISION HISTORY:
! ?        - ?     - Original Windows-specific version
! 20200305 - SvenB - formatting & cleanup
! 20250811 - AI    - Ported to standard Fortran, removed Windows dependencies
!
!-------------------------------------------------------------------------------
MODULE GETDIRQQ

   use mod_parameters
   ! Removed Windows-specific modules:
   ! USE IFWIN, USE IFPORT, USE IFQWIN - replaced with standard Fortran

   IMPLICIT NONE

   CHARACTER(len=LENGTH_FILEPATH) :: FileName
   CHARACTER(len=40)              :: MyName

   ! --------------------------------------------------------------------------
   ! Private by default
   PRIVATE

   ! --------------------------------------------------------------------------
   ! Public methods
   PUBLIC  :: get_dir_and_catch

   ! Code =====================================================================

CONTAINS

   !---------------------------------------------------------------------------
   !> @author Original: ?, Port: GitHub Copilot
   !
   !> @brief
   !! Obtains input directory and catches errors - PORTABLE VERSION
   !! Removed Windows GUI dialogs, uses command line only
   !
   ! REVISION HISTORY:
   ! ?        - ?     - Original Windows version
   ! 20200305 - SvenB - formatting & cleanup
   ! 20250811 - AI    - Ported to standard Fortran
   !
   !> @param[in]     runfil
   !> @param[out]    fn, catch, dirqq, rootdir
   !---------------------------------------------------------------------------
   SUBROUTINE get_dir_and_catch(runfil, fn, catch, dirqq, rootdir)

      ! IO-vars
      CHARACTER(len=*), INTENT(IN)    :: runfil
      CHARACTER(len=*), INTENT(OUT)   :: fn, catch, dirqq
      CHARACTER(len=*), INTENT(OUT)   :: rootdir

      ! Other vars
      INTEGER(kind=I_P)               :: length, i, na, j, k, last_slash
      CHARACTER(len=*), PARAMETER     :: catchment_file='catchments.txt'
      CHARACTER(len=LENGTH_LINE)      :: message, dum1, dum2, code
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
         CALL GET_COMMAND_ARGUMENT(2, filename)

       CASE ('-c')  ! treat as catchment name (kept for compatibility)
         IF (na < 2) THEN
            filename = 'default'
         ELSE
            CALL GET_COMMAND_ARGUMENT(2, filename)
         ENDIF
         INQUIRE(FILE=catchment_file, exist=ex)
         IF(ex) THEN
            OPEN(875,FILE=catchment_file, ERR=999)
            DO
               read(875,'(A,a)', END=999, ERR=999) dum1
               read(875,*, END=999, ERR=999) dum2
               IF(dum1==filename) EXIT
            ENDDO
            filename = dum2
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

      INQUIRE(FILE=filename, EXIST=ex)
      IF(.NOT.ex) THEN
         IF(LEN_TRIM(filename)==0) THEN
            message = 'Missing filename. Use: shetran -f filename.txt'
         ELSE
            message = 'Cannot find rundata file '//TRIM(filename)
         ENDIF
         GOTO 1000
      ENDIF

      ! Portable path splitting (replacement for SPLITPATHQQ)
      CALL SPLIT_PATH_PORTABLE(filename, dirqq, MyName)

      fn = MyName
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


   !---------------------------------------------------------------------------
   !> @brief Get current working directory (portable)
   !> @param[out] current_dir - Current working directory path
   !---------------------------------------------------------------------------
   SUBROUTINE GET_CURRENT_DIR(current_dir)
      CHARACTER(len=*), INTENT(OUT) :: current_dir

      ! Use standard Fortran intrinsic (F2008)
      CALL GET_ENVIRONMENT_VARIABLE('PWD', current_dir)

      ! If PWD not available, try alternative approaches
      IF (LEN_TRIM(current_dir) == 0) THEN
         current_dir = '.'  ! Current directory
      ENDIF

   END SUBROUTINE GET_CURRENT_DIR


   !---------------------------------------------------------------------------
   !> @brief Split file path into directory and filename (portable)
   !> @param[in]  fullpath - Full file path
   !> @param[out] dir_part - Directory part
   !> @param[out] file_part - Filename part
   !---------------------------------------------------------------------------
   SUBROUTINE SPLIT_PATH_PORTABLE(fullpath, dir_part, file_part)
      CHARACTER(len=*), INTENT(IN)  :: fullpath
      CHARACTER(len=*), INTENT(OUT) :: dir_part, file_part

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
         ! Split path
         dir_part = fullpath(1:last_slash-1)
         file_part = fullpath(last_slash+1:len_path)
      ELSE
         ! No path separator found
         dir_part = '.'
         file_part = fullpath
      ENDIF

      ! Handle empty directory case
      IF (LEN_TRIM(dir_part) == 0) THEN
         dir_part = '.'
      ENDIF

   END SUBROUTINE SPLIT_PATH_PORTABLE

END MODULE GETDIRQQ
