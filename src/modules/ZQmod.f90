!> summary: Reservoir stage-discharge lookup tables.
!> author: Daryl Hughes, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
!>
!> This module implements reservoir outflow lookup from user-supplied ZQ
!> tables. Each table relates upstream water level (stage, `Z`) to downstream
!> discharge (`Q`) for a channel link and face. A run data file entry opens the
!> ZQ setup file, and [[ReadZQTable]] loads the table metadata and values into
!> module arrays used later by [[get_ZQTable_value]].
!>
!> The implementation is a tabulated stage-discharge relationship rather than a
!> fitted hydraulic formula. This is the same hydrological concept as a rating
!> curve, where discharge is obtained from water level using an established
!> stage-discharge relation. See the USGS overview of rating curves:
!> https://www.usgs.gov/faqs/how-a-rating-curve-used-convert-gage-height-streamflow
!>
!> The ZQ file may contain several tables, one per reservoir/channel link. Each
!> table has a first column of stage values and one or more discharge columns
!> whose headers are of the form `ZQ>##.##`; these header values are treated as
!> operational stage thresholds for selecting the active discharge column.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | - | DH | - | Initial version. |
!> | - | SB | SHETRAN 4.4.6.Res2 | Reworked for inclusion in SHETRAN. |
!> @endhistory
!>
!> @note The table parser assumes space-delimited input and ascending discharge
!> threshold headers. The lookup returns the first table row where `Zu` is not
!> greater than the stored stage value; it does not interpolate between rows.
!> The active discharge column is selected from the `ZQ>threshold` headers only
!> when the configured sluice operation hour crosses a new day.
!> @endnote
!>
!>
module ZQmod

    USE sglobal,    ONLY: UZNOW                                                 ! UZNOW is sim time (hours)
    USE AL_C,       ONLY: DTUZ,UZNEXT                                           ! DZ is sim time (seconds),  UZNEXT is time step to be added to previous time to get current time
    USE AL_D,       ONLY: zqd,NoZQTables,ZQTableLink,ZQTableFace,ZQweirSill     ! these are specifically for ZQmod
    USE mod_parameters                                                          ! general parameters


    IMPLICIT NONE

    ! Module variables

    ! set everything to private by default
    PRIVATE

    ! module variables
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQcols                  !< use to dimension allocatable arrays
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQrows                  !< use to dimension allocatable arrays
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: zcol                     !< use to dimension allocatable arrays
    REAL(kind=R8P), DIMENSION(:,:), ALLOCATABLE     :: headerRealArray          !< real array to store weirEq stage thresholds
    REAL(kind=R8P), DIMENSION(:,:,:), ALLOCATABLE   :: ZQ                       !< ZQ = 2D array (nZQrows, nZQcols)
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: ZQTableOpHour            !< the hour at which sluices are operated
    INTEGER(kind=I_P)                               :: ZQTableRef               !< the reference number of the ZQtable

    ! what is public from this module?
    PUBLIC                                          :: ReadZQTable, get_ZQTable_value   ! subroutine names

    CONTAINS


    !> author: Dary Hughes, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
    !
    !> Reads the user-defined reservoir ZQ table file.
    !>
    !> The routine reads the number of ZQ tables, scans each table to determine
    !> its row and column count, allocates the module lookup arrays, rewinds the
    !> file, then loads metadata and table values. Metadata include the table
    !> reference, channel link, channel face, and sluice operation hour. Header
    !> strings such as `ZQ>96.8` are converted to numeric stage thresholds and
    !> stored in `headerRealArray`.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | - | DH | - | Initial version. |
    !> | - | SB | SHETRAN 4.4.6.Res2 | Reworked for inclusion in SHETRAN. |
    !>
    !> @note This routine has no dummy arguments. It reads from the globally
    !> opened `zqd` unit, allocates module arrays, allocates ZQ metadata arrays
    !> from `AL_D`, writes `output_readZQTable.txt`, closes `zqd`, and stops the
    !> program with status 255 if the table cannot be read.
    SUBROUTINE ReadZQTable()

        ! general variables
        INTEGER(kind=I_P)                               :: i, j, k, printRow, printCol, pos     !< useful local integers

        ! specific variables
        CHARACTER(LEN = 120)                            :: headerRaw                            !< stores the entire first line of the ZQtable file
        CHARACTER(LEN = 9), DIMENSION(:,:), ALLOCATABLE :: headerRawArray                       !< character array to store ZQtable header names
        CHARACTER(LEN = 9), DIMENSION(:,:), ALLOCATABLE :: headerCharArray                      !< character array to store trimmed ZQtable header names
        INTEGER(kind=I_P)                               :: maxnumberRows, maxnumberCols         !< use to dimension allocatable arrays
        LOGICAL                                         :: IsZQreadOK=.FALSE.                   !< sets initial value for error catching

        INTEGER(kind=I_P)                               :: fid_ZQ_log                           !< file-id of the ZQ-table-logfile


        ! Code -----------------------------------------------------------------
        OPEN(newunit = fid_ZQ_log,FILE='output_readZQTable.txt', ERR=101)

        ! read ZQ tables
        READ(zqd,*)                                                             ! skip line 1 ': NUMBER OF ZQ TABLES NEEDED'
        READ(zqd,*, END = 101) NoZQTables                                       ! read line 2 as NoZQTables. This is used to allocate the number of ZQ arrays expected

        ALLOCATE(nZQcols(NoZQTables))
        ALLOCATE(nZQrows(NoZQTables))
        ALLOCATE(zcol(NoZQTables))
        ALLOCATE(ZQTableLink(NoZQTables))
        ALLOCATE(ZQTableFace(NoZQTables))
        ALLOCATE(ZQTableOpHour(NoZQTables))
        ALLOCATE(ZQWeirSill(NoZQTables))

        DO i = 1,NoZQTables                                                     ! start loop through number of ZQtables defined
            DO j = 1,9
                READ(zqd,*)                                                     ! skip lines 3-11
            ENDDO

            READ(zqd,*, END = 101) nZQrows(i)                                   ! read line 12 as number of ZQrows (nZQrows)
            READ(zqd,*)                                                         ! skip line 13
            READ(zqd, "(A)", END = 101) headerRaw                               ! read line 14 as headerRaw

            nZQcols(i) = 0                                                      ! initialise nZQcols counter
            DO WHILE(LEN(TRIM(headerRaw)) > 0)                                  ! start loop through headerRaw count nZQcols using space delimiters
                pos        = INDEX(headerRaw, " ")                              ! store position of first space delimiter
                headerRaw  = headerRaw(pos+1:)                                  ! store remaining headerRaw (from pos+1 to end) as headerRaw
                nZQcols(i) = nZQcols(i) + 1                                     ! increase nZQcols counter
            END DO

            DO j=1,nZQrows(i)
                READ(zqd,*)                                                     ! read ZQ table as zqd
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
        READ(zqd,*)                                                             ! skip line 1
        READ(zqd,*)                                                             ! skip line 2

        DO i = 1,NoZQTables

            zcol(i) = 2                                                         ! set zcol=2 to start with

            READ(zqd,*)                                                         ! skip line 3
            READ(zqd,*, END = 101)ZQTableRef                                    ! read line 4 as ZQTableRef
            READ(zqd,*)                                                         ! skip line 5
            READ(zqd,*, END = 101)ZQTableLink(i)                                ! read line 6 as ZQTableLink
            READ(zqd,*)                                                         ! skip line 7
            READ(zqd,*, END = 101)ZQTableFace(i)                                ! read line 8 as ZQTableFace
            READ(zqd,*)                                                         ! skip line 9
            READ(zqd,*, END = 101)ZQTableOpHour(i)                              ! read line 10 as ZQTableOpHour
            READ(zqd,*)                                                         ! skip line 11
            READ(zqd,*)                                                         ! skip line 12
            READ(zqd,*)                                                         ! skip line 13
            READ(zqd, "(A)", END = 101)headerRaw                                ! read in line as headerRaw

            ! convert headerRaw to headerRawArray
            DO j = 1,nZQcols(i)                                                 ! start loop through headerRaw count nZQcols using space delimiters
                pos                 = INDEX(headerRaw, " ")                     ! store position of first space delimiter
                headerRawArray(j,i) = headerRaw(1:pos)                          ! store substring of headerRaw from 1 to pos (1st col)
                headerRaw           = headerRaw(pos+1:)                         ! store remaining headerRaw (from pos+1 to end) as headerRaw
            END DO

            ! convert headerRawArray to headerCharArray and then headerRealArray e.g. 'ZQ>96.8' -> 96.80
            headerCharArray(1,i) = 'Z'                                          ! set the col/row header as 'Z'
            DO j = 2,nZQcols(i)                                                 ! start loop, skipping first item as this is the col header
                headerCharArray(j,i) = headerRawArray(j,i)(                     &
&                                          INDEX(headerRawArray(j,i),'>')+1:)   ! return numpart of alphanum string header, by finding index of substring '>', and adding 1
                READ(headerCharArray(j,i),*) headerRealArray(j,i)               ! convert character to real
            END DO

            ! read ZQweirSill as lowest value of headers
            ZQweirSill(i) = headerRealArray(2,i)                                ! NB: this relies on the user ensuring that the ZQtable file cols start from minimum and ascend
                                                                                !     left to right. Error catch needed?

            DO j = 1, nZQrows(i)                                                ! for subsequent lines in file(1), do the following:
                READ(zqd,*, END = 101) (ZQ(j,k,i), k=1, nZQcols(i))             ! implied do: read 1st value as ZQ(i,1) 2nd as ZQ(i,2)...
            END DO


            ! write ZQTables to fid_ZQ_log.fort
            WRITE(fid_ZQ_log, *) 'ZQTableRef   =', ZQTableRef
            WRITE(fid_ZQ_log, *) 'ZQTableLink  =', ZQTableLink(i)
            WRITE(fid_ZQ_log, *) 'ZQTableFace  =', ZQTableFace(i)
            WRITE(fid_ZQ_log, *) 'ZQTableOpHour=', ZQTableOpHour(i)
            WRITE(fid_ZQ_log, *) 'nZQcols      =', nZQcols(i)
            WRITE(fid_ZQ_log, *) 'nZQrows      =', nZQrows(i)
            WRITE(fid_ZQ_log, '(5(A))'), 'ZQ headers: ',                        &
&               headerRawArray(1:nZQcols(i),i)                                  ! write headers, character format
            DO printRow = 1, nZQrows(i)                                         ! specify which rows to print
                WRITE(fid_ZQ_log,'(5(f12.3))')                                  &
&                   (ZQ(printRow, printCol,i), printCol=1,nZQcols(i))           ! implied do: in array ZQ, print each col, real format
            END DO
        ENDDO

        CLOSE(zqd)                                                              ! close file zqd
        CLOSE(fid_ZQ_log)

        return

        ! error management
    101   CONTINUE
        PRINT*,'error reading ZQ table'
        STOP(255)

    END SUBROUTINE ReadZQTable


    !> author: Dary Hughes, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
    !
    !> Returns downstream discharge from a reservoir ZQ lookup table.
    !>
    !> The function selects the active discharge column for `ZQref` when a new
    !> operating day is crossed, using `ZQTableOpHour` and the current SHETRAN
    !> time. It then scans the stage column and returns the discharge value from
    !> the selected column. If `Zu` is above the largest checked row before a
    !> match is found, the current implementation assigns `-999` as a missing or
    !> out-of-range value.
    !>
    !> Column selection is stepwise: on a sluice-operation boundary the highest
    !> header threshold lower than the current upstream stage is selected and held
    !> until the next operation boundary. Row selection is also stepwise and does
    !> not interpolate between tabulated stages.
    !>
    !> @history
    !> | Date | Author | Version | Description |
    !> |:-----|:-------|:--------|:------------|
    !> | - | DH | - | Initial version. |
    !> | - | SB | SHETRAN 4.4.6.Res2 | Reworked for inclusion in SHETRAN. |
    !>
    !> @note This routine uses `UZNOW`, `UZNEXT`, `ZQTableOpHour`,
    !> `headerRealArray`, `nZQcols`, `nZQrows`, `zcol`, and `ZQ` from module or
    !> imported state. The stage-discharge lookup is table based and does not
    !> interpolate.
    FUNCTION get_ZQTable_value(ZQref,zu) RESULT(qd)

        ! IO variables
        INTEGER(kind=I_P), INTENT(IN)   :: ZQref    !! Index of the ZQ table to use for this reservoir/channel link.
        REAL(kind=R8P), INTENT(IN)      :: Zu       !! Upstream water level or stage used to query the ZQ table.
        REAL(kind=R8P)                  :: Qd       !! Downstream discharge returned from the selected ZQ table column.

        ! general variables
        INTEGER(kind=I_P)               :: i        !< loop counter

        ! Code -----------------------------------------------------------------

        ! start sluice operation loop
        IF (INT(UZNOW + ZQTableOpHour(ZQref)) / 24 >                            &
&           INT(UZNOW + ZQTableOpHour(ZQref) - UZNEXT) / 24) THEN               ! if current day integer > previous day INT(UZNOW), then operate sluices:
            !WRITE(778, *), 'new day'                                            ! write for test purposes

            ! select weir equation (Zcol) based on which range of stages Zu falls into
            ! NB if Zu < min ZQ threshold, will return an error
            DO i = nZQcols(ZQref), 2, -1                                        ! start loop in descending order of ZQ thresholds
                IF(Zu > headerRealArray(i,ZQref)) THEN                          ! test if Zu > ZQ threshold
                    zcol(ZQref) = i                                             ! if TRUE, then pass i (nZQcol) to zcol...
                    EXIT                                                        ! ...and exit
                ELSEIF(Zu > headerRealArray(2,ZQref)) THEN                      ! elseif Zu is greater than the minimum ZQ threshold ->restart loop
                ELSE                                                            ! else Zu is below threshold, print warning and exit loop
                    PRINT*,                                                     &
&                       'warning: Zu is below minimum ZQthreshold defined in ZQtable'
                    EXIT
                ENDIF
            END DO
        ENDIF                                                                   ! end sluice operation loop



        ! look up z value in ZQ array which matches Zu and return corresponding Qd
        DO i = 1, nZQrows(ZQref)                                                ! start loop through rows for a given table
            IF(Zu > ZQ(i, 1, ZQref)) THEN                                       ! if Zu is greater than the ith value in the z column...
                Qd = -999                                                       ! return dummy value -999
            ELSE
                Qd = ZQ(i, zcol(ZQref), ZQref)                                  ! when Zu is found, finds Qd from zcol, and assigns to Qd
                EXIT                                                            ! exit loop, preserving Qd. NB STOP wipes variable assignment
            END IF
        END DO

        !PRINT*, ZQref,zu,qd                                                     ! NB duplicates print from OCMOD2 line 664

        ! write everytimestep outputs to 778.fort
        !IF(UZNOW <0.1) THEN                                                     ! write header at sim start
        !    WRITE(778, *), '        UZNOW,      Zu,         Qd'
        !    WRITE(778, *), '        i,      zcol'
        !ENDIF
        !WRITE(778,'(6(f12.2,1a))')  uznow,  ',', &                              ! write real output
        !                            Zu,     ',', &
        !                            Qd,     ','
        !WRITE(778, *)               i,      ',', &                              ! write integer output
        !                           zcol,   ','

        END FUNCTION get_ZQTable_value

    END MODULE ZQmod
