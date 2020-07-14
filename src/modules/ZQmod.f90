module ZQmod

    USE sglobal,    ONLY: UZNOW                                                                     ! UZNOW is sim time (hours)
    USE AL_C,       ONLY: DTUZ,UZNEXT                                                               ! DZ is sim time (seconds),  UZNEXT is time step to be added to previous time to get current time
    USE AL_D,       ONLY: zqd,NoZQTables,ZQTableLink,ZQTableFace,ZQweirSill                         ! these are specifically for ZQmod
    USE mod_parameters
    
    
    IMPLICIT NONE
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Variables
        
    ! set everything to private by default
    PRIVATE
        
    ! variables
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQcols                              ! use to dimension allocatable arrays
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQrows                              ! use to dimension allocatable arrays
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: zcol                                 ! use to dimension allocatable arrays
    REAL(kind=R8P), DIMENSION(:,:), ALLOCATABLE     :: headerRealArray                      ! real array to store weirEq stage thresholds
    REAL(kind=R8P), DIMENSION(:,:,:), ALLOCATABLE   :: ZQ                                   ! ZQ = 2D array (nZQrows, nZQcols)
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: ZQTableOpHour                        ! the hour at which sluices are operated
    INTEGER(kind=I_P)                               :: ZQTableRef                           ! the reference number of the ZQtable
        
    ! what is public from this module?
    PUBLIC                                          :: ReadZQTable,ZQTable                  ! subroutine names
    
    CONTAINS
    
    !****************************************************************************
    !
    !  PROGRAM: ReadZQTable
    !
    !  PURPOSE: This script is the enginge of the reservoir model designed by Daryl Hughes and Steve Birksinshaw in 2020
    !           For models which contain reservoirs, it outputs downstream discharge as a function of upstream water elevation.
    !           The user must create an elevation-discharge (ZQ) set up file and reference this in the RunDatafile (module 51).
    !           This file may contain multiple ZQ tables i.e. for multiple channel links
    !           These can be created in Excel etc., and saved as .txt. The file should be space-delimited, so may require replacement of tabs with spaces
    !           The ZQtables require a Z column (first), followed by at least one discharge column.
    !           These should have names along the format 'ZQ>##.##' i.e. discharge at elevations above this threshold
    !           The number of rows and the interval between Zs is arbitrary (for example, 0.01m intervals would be suitable)
    !
    !           This script is integrated into SHETRAN4.4.6.Res2 to create the SHETRAN-Reservoir programme
    !           Version control: This is the version associated with SHETRAN-Res2 (Tidied and send to Steve Birkinshaw)
    !****************************************************************************
    
    ! SUBROUTINE ReadZQTable
    ! ReadZQTable reads in the user-defined ZQ file, which includes ZQ tables and ZQ meta data
    ! ZQ table(s) contain column and row headers, and the actual values needed.
    ! Metadata includes the number of ZQ tables needed (i.e. reservoirs in the model), link and face numbers, and operation hours
    ! These are converted into a 2D array and used as a lookuptable
    SUBROUTINE ReadZQTable()
    
        ! general variables
        INTEGER(kind=I_P)                               :: i, j, k, printRow, printCol, pos     ! useful local integers

        ! specific variables
        CHARACTER(LEN = 120)                            :: headerRaw                            ! stores the entire first line of the ZQtable file
        CHARACTER(LEN = 9), DIMENSION(:,:), ALLOCATABLE :: headerRawArray                       ! character array to store ZQtable header names
        CHARACTER(LEN = 9), DIMENSION(:,:), ALLOCATABLE :: headerCharArray                      ! character array to store trimmed ZQtable header names
        INTEGER(kind=I_P)                               :: maxnumberRows, maxnumberCols         ! use to dimension allocatable arrays
        LOGICAL                                         :: IsZQreadOK=.FALSE.                   ! sets initial value for error catching

        INTEGER(kind=I_P)                               :: fid_ZQ_log                           ! file-id of the ZQ-table-logfile


        ! Code -----------------------------------------------------------------
        OPEN(newunit = fid_ZQ_log,FILE='output_readZQTable.txt', ERR=101)
    
        ! read ZQ tables
        READ(zqd,*)                                                                                  ! skip line 1 ': NUMBER OF ZQ TABLES NEEDED'
        READ(zqd,*, END = 101) NoZQTables                                                             ! read line 2 as NoZQTables. This is used to allocate the number of ZQ arrays expected
    
        ALLOCATE(nZQcols(NoZQTables))
        ALLOCATE(nZQrows(NoZQTables))
        ALLOCATE(zcol(NoZQTables))
        ALLOCATE(ZQTableLink(NoZQTables))
        ALLOCATE(ZQTableFace(NoZQTables))
        ALLOCATE(ZQTableOpHour(NoZQTables))
        ALLOCATE(ZQWeirSill(NoZQTables))
    
        DO i = 1,NoZQTables                                                                          ! start loop through number of ZQtables defined
            DO j = 1,9
                READ(zqd,*)                                                                            ! skip lines 3-11
            ENDDO
            
            READ(zqd,*, END = 101) nZQrows(i)                                                          ! read line 12 as number of ZQrows (nZQrows)
            READ(zqd,*)                                                                               ! skip line 13
            READ(zqd, "(A)", END = 101) headerRaw                                                      ! read line 14 as headerRaw
            
            nZQcols(i) = 0                                                                            ! initialise nZQcols counter
            DO WHILE(LEN(TRIM(headerRaw)) > 0)                                                        ! start loop through headerRaw count nZQcols using space delimiters
                pos        = INDEX(headerRaw, " ")                                            ! store position of first space delimiter
                headerRaw  = headerRaw(pos+1:)                                                ! store remaining headerRaw (from pos+1 to end) as headerRaw
                nZQcols(i) = nZQcols(i) + 1                                                   ! increase nZQcols counter
            END DO
            
            DO j=1,nZQrows(i)
                READ(zqd,*)                                                                            ! read ZQ table as zqd
            ENDDO
        ENDDO
    
        maxnumberRows = maxval(nZQrows)
        maxnumberCols = maxval(nZQcols)
    
        ! allocate array dimensions using maxnumberRows and maxnumberCols
        ALLOCATE(ZQ(maxnumberRows,maxnumberCols,NoZQTables))
        ALLOCATE(headerRawArray(maxnumberCols,NoZQTables))
        ALLOCATE(headerCharArray(maxnumberCols,NoZQTables))
        ALLOCATE(headerRealArray(maxnumberCols,NoZQTables))
        REWIND (zqd)
    
        ! read ZQ metadata
        READ(zqd,*)                                                                                  ! skip line 1
        READ(zqd,*)                                                                                  ! skip line 2
    
        DO i = 1,NoZQTables
    
            zcol(i) = 2                                                                               ! set zcol=2 to start with
    
            READ(zqd,*)                                                                               ! skip line 3
            READ(zqd,*, END = 101)ZQTableRef                                                          ! read line 4 as ZQTableRef
            READ(zqd,*)                                                                               ! skip line 5
            READ(zqd,*, END = 101)ZQTableLink(i)                                                      ! read line 6 as ZQTableLink
            READ(zqd,*)                                                                               ! skip line 7
            READ(zqd,*, END = 101)ZQTableFace(i)                                                      ! read line 8 as ZQTableFace
            READ(zqd,*)                                                                               ! skip line 9
            READ(zqd,*, END = 101)ZQTableOpHour(i)                                                    ! read line 10 as ZQTableOpHour
            READ(zqd,*)                                                                               ! skip line 11
            READ(zqd,*)                                                                               ! skip line 12
            READ(zqd,*)                                                                               ! skip line 13
            READ(zqd, "(A)", END = 101)headerRaw                                                      ! read in line as headerRaw
    
            ! convert headerRaw to headerRawArray
            DO j = 1,nZQcols(i)                                                                       ! start loop through headerRaw count nZQcols using space delimiters
                pos                 = INDEX(headerRaw, " ")                                            ! store position of first space delimiter
                headerRawArray(j,i) = headerRaw(1:pos)                                                 ! store substring of headerRaw from 1 to pos (1st col)
                headerRaw           = headerRaw(pos+1:)                                                ! store remaining headerRaw (from pos+1 to end) as headerRaw
            END DO
    
            ! convert headerRawArray to headerCharArray and then headerRealArray e.g. 'ZQ>96.8' -> 96.80
            headerCharArray(1,i) = 'Z'                                                                ! set the col/row header as 'Z'
            DO j = 2,nZQcols(i)                                                                       ! start loop, skipping first item as this is the col header
                headerCharArray(j,i) = headerRawArray(j,i)(INDEX(headerRawArray(j,i),'>')+1:)          ! return numpart of alphanum string header, by finding index of substring '>', and adding 1
                READ(headerCharArray(j,i),*) headerRealArray(j,i)                                       ! convert character to real
            END DO
    
            ! read ZQweirSill as lowest value of headers
            ZQweirSill(i) = headerRealArray(2,i)                                                      ! NB: this relies on the user ensuring that the ZQtable file cols start from minimum and ascend left to right. Error catch needed?
    
            DO j = 1, nZQrows(i)                                                                      ! for subsequent lines in file(1), do the following:
                READ(zqd,*, END = 101) (ZQ(j,k,i), k=1, nZQcols(i))                                      ! implied do: read 1st value as ZQ(i,1) 2nd as ZQ(i,2)...
            END DO
    
    
            ! write ZQTables to fid_ZQ_log.fort
            WRITE(fid_ZQ_log, *) 'ZQTableRef   =', ZQTableRef
            WRITE(fid_ZQ_log, *) 'ZQTableLink  =', ZQTableLink(i)
            WRITE(fid_ZQ_log, *) 'ZQTableFace  =', ZQTableFace(i)
            WRITE(fid_ZQ_log, *) 'ZQTableOpHour=', ZQTableOpHour(i)
            WRITE(fid_ZQ_log, *) 'nZQcols      =', nZQcols(i)
            WRITE(fid_ZQ_log, *) 'nZQrows      =', nZQrows(i)
            WRITE(fid_ZQ_log, '(5(A))'), 'ZQ headers: ', headerRawArray(1:nZQcols(i),i)                      ! write headers, character format
            DO printRow = 1, nZQrows(i)                                                               ! specify which rows to print
                WRITE(fid_ZQ_log,'(5(f12.3))') (ZQ(printRow, printCol,i),printCol=1,nZQcols(i))               ! implied do: in array ZQ, print each col, real format
            END DO
        ENDDO
    
        CLOSE(zqd)                                                                                   ! close file zqd
        CLOSE(fid_ZQ_log)
    
        return

        ! error management
    101   CONTINUE
        PRINT*,'error reading ZQ table'
        STOP(255)
    
    END SUBROUTINE ReadZQTable
    
    

    ! SUBROUTINE ZQTable
    ! ZQTable uses the ZQ array from ReadZQTable to calculate downstream flow (Qd)
    ! It activates the specified ZQcol using the ZQTableOpHour from ReadZQTable
    SUBROUTINE ZQTable(ZQref,zu,qd)
    
        ! IO variables    
        INTEGER(kind=I_P), INTENT(IN)   :: ZQref    ! reference number of weir
        REAL(kind=R8P), INTENT(IN)      :: Zu       ! Zu = upstream stage
        REAL(kind=R8P), INTENT(OUT)     :: Qd       ! Qd = downstream discharge
    
        ! general variables
        INTEGER(kind=I_P)               :: i        ! loop counter 

    
        ! start sluice operation loop
        IF (INT(UZNOW + ZQTableOpHour(ZQref)) / 24 > INT(UZNOW + ZQTableOpHour(ZQref) - UZNEXT) / 24) THEN     ! if current day integer > previous day INTEGER(kind=I_P), then operate sluices:
            !WRITE(778, *), 'new day'                                                                 ! write for test purposes
    
            ! select weir equation (Zcol) based on which range of stages Zu falls into                ! NB if Zu < min ZQ threshold, will return an error
            DO i = nZQcols(ZQref), 2, -1                                                              ! start loop in descending order of ZQ thresholds
                IF(Zu > headerRealArray(i,ZQref)) THEN                                                 ! test if Zu > ZQ threshold
                    zcol(ZQref) = i                                                                     ! if TRUE, then pass i (nZQcol) to zcol...
                    EXIT                                                                                ! ...and exit
                ELSEIF(Zu > headerRealArray(2,ZQref)) THEN                                             ! elseif Zu is greater than the minimum ZQ threshold ->restart loop
                ELSE                                                                                   ! else Zu is below threshold...
                    PRINT*, 'warning: Zu is below minimum ZQthreshold defined in ZQtable'               ! ...print warning...
                    EXIT                                                                                ! ...and exit loop
                ENDIF
            END DO
        ENDIF                                                                                        ! end sluice operation loop
    
    
    
        ! look up z value in ZQ array which matches Zu and return corresponding Qd
        DO i = 1, nZQrows(ZQref)                                                                     ! start loop through rows for a given table
            IF(Zu > ZQ(i, 1, ZQref)) THEN                                                               ! if Zu is greater than the ith value in the z column...
                Qd = -999                                                                              ! return dummy value -999
            ELSE
                Qd = ZQ(i, zcol(ZQref), ZQref)                                                           ! when Zu is found, finds Qd from zcol, and assigns to Qd
                EXIT                                                                                   ! exit loop, preserving Qd. NB STOP wipes variable assignment
            END IF
        END DO
    
        !PRINT*, ZQref,zu,qd                                                                         ! NB duplicates print from OCMOD2 line 664
    
        ! write everytimestep outputs to 778.fort
        !IF(UZNOW <0.1) THEN                                                                          ! write header at sim start
        !    WRITE(778, *), '        UZNOW,      Zu,         Qd'
        !    WRITE(778, *), '        i,      zcol'
        !ENDIF
        !WRITE(778,'(6(f12.2,1a))')  uznow,  ',', &                                                   ! write real output
        !                            Zu,     ',', &
        !                            Qd,     ','
        !WRITE(778, *)               i,      ',', &                                                   ! write integer output
        !                           zcol,   ','
    
        END SUBROUTINE ZQTable
    
    END MODULE ZQmod
    