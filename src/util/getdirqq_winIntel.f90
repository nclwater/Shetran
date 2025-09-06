!> summary: Provides Windows-specific file dialog functionality for obtaining input filenames.
!> author: Stephen Birkinshaw (Newcastle University), Sven Berendsen (Newcastle University)
!> date: Original version (date unknown), formatted 2020-03-05
!>
!> This module provides Windows-specific file handling capabilities using Intel Fortran
!> libraries. It includes GUI file selection dialogs and Windows-specific path operations.
!> This module is used only on Windows systems and requires Intel Fortran compiler libraries.
!>
!> @note
!> This module is not being read by FORD because there is the portable version with the same name.
!>
!> @history
!> | Date | Author | Description |
!> |------|--------|-------------|
!> | ? | Original | Original Windows-specific version with GUI dialogs |
!> | 2020-03-05 | SvenB | Formatting and cleanup |
MODULE GETDIRQQ

   use mod_parameters

   USE IFWIN
   USE IFPORT, ONLY : SPLITPATHQQ, SYSTEMQQ, GETDRIVEDIRQQ
   USE IFQWIN, ONLY : QWIN$FRAMEWINDOW, GETHWNDQQ

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

   !> summary: Obtains the input directory and runfile from command-line arguments or GUI dialog.
   !>
   !> This subroutine provides multiple ways to obtain the input filename:
   !> - GUI file selection dialog (default and various modes)
   !> - Command-line filename argument (-f)
   !> - Catchment lookup from file (-c)
   !> It uses Windows-specific APIs for file dialogs and path manipulation.
   SUBROUTINE get_dir_and_catch(runfil, fn, catch, dirqq, rootdir)

      ! IO-vars
      CHARACTER(len=*), INTENT(IN)    :: runfil   !! The runfile name (often unused when using GUI)
      CHARACTER(len=*), INTENT(OUT)   :: fn       !! The base name of the runfile
      CHARACTER(len=*), INTENT(OUT)   :: catch    !! The catchment name extracted from filename
      CHARACTER(len=*), INTENT(OUT)   :: dirqq    !! The directory path of the runfile
      CHARACTER(len=*), INTENT(OUT)   :: rootdir  !! The root directory where the executable was run

      ! Other vars
      INTEGER(kind=I_P)               :: length, IERROR, iret, i, idum, na, j, k
      LOGICAL(KIND=4)                 :: STATUS, bRET
      CHARACTER(len=3)                :: drive
      CHARACTER(len=*), PARAMETER     :: catchment_file='catchments.txt'
      CHARACTER(len=LENGTH_FILEPATH)  :: path, ext, ALLFILTERS
      CHARACTER(len=60)               :: DLGTITLE, code
      CHARACTER(len=LENGTH_LINE)      :: message, dum1, dum2
      LOGICAL                         :: ex
      TYPE(T_OPENFILENAME)            :: opn

      ! Code =================================================================
      idum = GETDRIVEDIRQQ(rootdir)

      na = NARGS()
      IF(na>1) THEN
         CALL GETARG(INT(1,KIND=2), code)
      ELSE
         code = '-a'  !treat as default filname
      ENDIF

      message=''
      SELECT CASE(code)
       CASE ('-a', '-m', '-af', '-sd', '-pattern', '-delinc', '-results') !use popup
         ALLFILTERS            = 'All files(*.*)'//CHAR(0)//'*.*'//CHAR(0)//CHAR(0)
         DLGTITLE              = 'Select a SHETRAN rundata file'C
         opn%lStructSize       = SIZEOF(Opn)
         opn%HWNDOWNER         = GETHWNDQQ(QWIN$FRAMEWINDOW)
         opn%HINSTANCE         = NULL
         opn%LPSTRFILTER       = LOC(ALLFILTERS)
         opn%LPSTRCUSTOMFILTER = NULL
         opn%NMAXCUSTFILTER    = NULL
         opn%NFILTERINDEX      = 1
         opn%LPSTRFILE         = LOC(FileName)
         opn%NMAXFILE          = LEN(FileName)
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

       CASE('-f') !treat as filename
         CALL GETARG(INT(2,KIND=2), filename)

       CASE('-c')  !treat as catchment name
         IF (na<3) THEN
            filename = 'default'
         ELSE
            !CALL GETARG(INT(2,KIND=2), filename)
         ENDIF
         INQUIRE(FILE=catchment_file, exist=ex)
         IF(ex) THEN
            OPEN(875,FILE=catchment_file, ERR=999)
            DO
               READ(875,'(A,a)', END=999, ERR=999) dum1
               READ(875,*, END=999, ERR=999) dum2
               IF(dum1==filename) EXIT
            ENDDO
            filename=dum2

         ELSE
            message='Cannot find file ' // TRIM(catchment_file) // ' in executable directory'
         ENDIF

       CASE DEFAULT
         message = 'Unrecognised command line argument ' // TRIM(code) // ' Recognise only -a, -c and -f'
      END SELECT

      IF(message/='') CALL handle_error(message)

      INQUIRE(FILE=filename, EXIST=ex)
      IF(.NOT.ex) THEN
         IF(LEN_TRIM(filename)==0) THEN
            message = 'Missing filename   Use -f filneme'
         ELSE
            message = 'Cannot find rundata file '//TRIM(filename)
         ENDIF
         CALL handle_error(message)
      ENDIF

      length = SPLITPATHQQ(FileName, drive, path, MyName, ext)
      dirqq  = path
      length = LEN_TRIM(Filename)
      j      = 0
      k      = 0

      DO i = length-1, 1, -1

         IF(Filename(i:i) == '.') THEN
            j=i

         ELSEIF(Filename(i:i) == '_') THEN
            dum2 = Filename(MAX(1,i-7):i)
            IF(TRIM(dum2) == 'rundata_') THEN
               k=i
               EXIT
            ENDIF
         ENDIF
      ENDDO

      IF (k*j == 0) THEN
         print*, '   RUNDATA FILENAME MUST HAVE FORM "rundata_name.txt"'
         STOP
      ENDIF

      catch = Filename(i+1:j-1)
      fn    = TRIM(Filename)

      RETURN

999   message = 'cannot find catchment ' // TRIM(filename) // ' in ' // TRIM(catchment_file)
      CALL handle_error(message)

   END SUBROUTINE get_dir_and_catch

   !> summary: Handles error messages by printing and stopping program execution.
   !>
   !> Simple error handler that prints an error message and terminates the program.
   !> Used for critical errors where the program cannot continue.
   SUBROUTINE handle_error(message)
      CHARACTER(len=*), INTENT(IN) :: message  !! The error message to display
      PRINT*, message
      STOP
   END SUBROUTINE handle_error

   !> summary: Handles and reports Windows common dialog box errors.
   !>
   !> This subroutine diagnoses and reports specific error conditions that can occur
   !> when using Windows common dialog boxes (file open/save dialogs). It provides
   !> detailed error messages for various failure modes and terminates the program
   !> if an error is detected.
   SUBROUTINE comdlger(IRET)

      ! IO-Vars
      INTEGER(KIND=I_P)   :: IRET  !! Return code from common dialog operation

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

END MODULE GETDIRQQ
