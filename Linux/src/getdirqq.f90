!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
!-------------------------------------------------------------------------------
!
!> @file GETDIRQQ.f90 
! 
!> @author Stephen Birkinshaw, Newcastle University
! 
!> @brief Gets the input filename(s).
!
! REVISION HISTORY:
! ?        - ?     - ?
! 20200305 - SvenB - formatting & cleanup
! 
!-------------------------------------------------------------------------------
MODULE GETDIRQQ

    use mod_parameters

!    USE IFWIN
    USE IFPORT, ONLY : SPLITPATHQQ, SYSTEMQQ, GETDRIVEDIRQQ
!    USE IFQWIN, ONLY : QWIN$FRAMEWINDOW, GETHWNDQQ

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
    !> @author ?
    ! 
    !> @brief
    !! Obtains input directory and catches errors.
    ! 
    ! REVISION HISTORY:
    ! ?        - ?     - ?
    ! 20200305 - SvenB - formatting & cleanup
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
        INTEGER(kind=I_P)               :: length, IERROR, iret, i, idum, na, j, k
        LOGICAL(KIND=4)                 :: STATUS, bRET
        CHARACTER(len=3)                :: drive
        CHARACTER(len=*), PARAMETER     :: catchment_file='catchments.txt'
        CHARACTER(len=LENGTH_FILEPATH)  :: path, ext, ALLFILTERS
        CHARACTER(len=60)               :: DLGTITLE, code
        CHARACTER(len=LENGTH_LINE)      :: message, dum1, dum2
        LOGICAL                         :: ex
!        TYPE(T_OPENFILENAME)            :: opn
        
        ! Code =================================================================
        idum = GETDRIVEDIRQQ(rootdir)

        na = NARGS()
        IF(na>1) THEN
            CALL GETARG(INT(1,KIND=2), code)
        ELSE
            code = '-f'  !treat as default filname
        ENDIF

        message=''
        SELECT CASE(code)

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

        IF(message/='') GOTO 1000

        INQUIRE(FILE=filename, EXIST=ex)
        IF(.NOT.ex) THEN
            IF(LEN_TRIM(filename)==0) THEN
                message = 'Missing filename   Use -f filneme'
            ELSE
                message = 'Cannot find rundata file '//TRIM(filename)
            ENDIF
            GOTO 1000
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

 999    message = 'cannot find catchment ' // TRIM(filename) // ' in ' // TRIM(catchment_file)

 1000   PRINT*, message
        STOP

    END SUBROUTINE get_dir_and_catch

    !---------------------------------------------------------------------------  
    !> @author ?
    ! 
    !> @brief
    !! Error handling for common dialog errors..
    ! 
    ! REVISION HISTORY:
    ! ?        - ?     - ?
    ! 20200305 - SvenB - formatting & cleanup
    !--------------------------------------------------------------------------- 

END MODULE GETDIRQQ
