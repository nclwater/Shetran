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
!> fitted hydraulic formula: discharge is selected from the active column by the
!> current upstream stage.
!>
!> The ZQ file may contain several tables, one per reservoir/channel link. Each
!> table has a first column of stage values and one or more discharge columns
!> whose headers are of the form `ZQ>##.##`; these header values are treated as
!> operational stage thresholds for selecting the active discharge column.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020 | DH/SB | SHETRAN 4.4.6.Res2 | Added reservoir ZQ lookup-table support. |
!> @endhistory
!>
!> @note The table parser assumes space-delimited input, one trailing delimiter
!> after each header token, and ascending `ZQ>threshold` headers. The lookup
!> returns the first table row where `Zu` is not greater than the stored stage
!> value; it does not interpolate between rows. The active discharge column is
!> selected from the `ZQ>threshold` headers only when the configured sluice
!> operation hour crosses a new day.
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
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQcols                  !! Number of columns in each ZQ table.
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQrows                  !! Number of data rows in each ZQ table.
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: zcol                     !! Currently active discharge-column index for each ZQ table.
    REAL(kind=R8P), DIMENSION(:,:), ALLOCATABLE     :: headerRealArray          !! Numeric stage thresholds parsed from `ZQ>threshold` headers.
    REAL(kind=R8P), DIMENSION(:,:,:), ALLOCATABLE   :: ZQ                       !! Stage-discharge table values, indexed by row, column, and table.
    INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: ZQTableOpHour            !! Hour offset at which each reservoir's sluices are operated.
    INTEGER(kind=I_P)                               :: ZQTableRef               !! Reference number read for the current ZQ table.

    ! what is public from this module?
    PUBLIC                                          :: ReadZQTable, get_ZQTable_value   ! subroutine names

    CONTAINS


    !> author: Daryl Hughes, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
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
    !> | 2020 | DH/SB | SHETRAN 4.4.6.Res2 | Added reservoir ZQ lookup-table support. |
    !> @endhistory
    !>
    !> Expected table layout:
    !>
    !> | File item | Code use |
    !> |:----------|:---------|
    !> | Number of tables | Allocates per-table metadata and lookup arrays. |
    !> | Table reference | Read into `ZQTableRef` while loading each table. |
    !> | Link and face | Stored in `ZQTableLink` and `ZQTableFace` for [[ocqdq]] dispatch. |
    !> | Operation hour | Stored in `ZQTableOpHour`; controls when the active discharge column is reconsidered. |
    !> | Header row | First column is stage `Z`; later columns are `ZQ>stage_threshold` discharge columns. |
    !> | Data rows | Stage in column 1 and discharges in the selected lookup columns. |
    !>
    !> Input assumptions:
    !>
    !> | Assumption | Consequence in the code |
    !> |:-----------|:------------------------|
    !> | Header tokens are separated by spaces and retain a trailing space while parsed. | `INDEX(headerRaw," ")` is used to count and split columns. |
    !> | `ZQ>threshold` columns are in ascending threshold order. | `ZQweirSill` is taken from column 2 and column selection scans from high to low. |
    !> | Stage rows are in ascending stage order. | [[get_ZQTable_value]] returns the first row with `Zu <= ZQ(row,1,table)`. |
    !>
    !> @note This routine has no dummy arguments. It reads from the globally
    !> opened `zqd` unit, allocates module arrays, allocates ZQ metadata arrays
    !> from `AL_D`, writes `output_readZQTable.txt`, closes `zqd`, and stops the
    !> program with status 255 if the table cannot be read.
    !> @endnote
    SUBROUTINE ReadZQTable()

        ! general variables
        INTEGER(kind=I_P)                               :: i                  !! Table and row loop index.
        INTEGER(kind=I_P)                               :: j                  !! Header, row, and column loop index.
        INTEGER(kind=I_P)                               :: k                  !! Implied-DO column index while reading table values.
        INTEGER(kind=I_P)                               :: printRow           !! Row index used when echoing a table to the log file.
        INTEGER(kind=I_P)                               :: printCol           !! Column index used when echoing a table to the log file.
        INTEGER(kind=I_P)                               :: pos                !! Position of the next space delimiter in `headerRaw`.

        ! specific variables
        CHARACTER(LEN = 120)                            :: headerRaw          !! Raw ZQ table header line while it is being split.
        CHARACTER(LEN = 9), DIMENSION(:,:), ALLOCATABLE :: headerRawArray     !! Raw header tokens by column and table.
        CHARACTER(LEN = 9), DIMENSION(:,:), ALLOCATABLE :: headerCharArray    !! Numeric part of each `ZQ>threshold` header as text.
        INTEGER(kind=I_P)                               :: maxnumberRows      !! Maximum row count over all ZQ tables.
        INTEGER(kind=I_P)                               :: maxnumberCols      !! Maximum column count over all ZQ tables.
        LOGICAL                                         :: IsZQreadOK=.FALSE. !! Unused legacy read-status flag.

        INTEGER(kind=I_P)                               :: fid_ZQ_log         !! Unit number for `output_readZQTable.txt`.


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


    !> author: Daryl Hughes, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
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
    !> | 2020 | DH/SB | SHETRAN 4.4.6.Res2 | Added reservoir ZQ lookup-table support. |
    !> @endhistory
    !>
    !> @note This routine uses `UZNOW`, `UZNEXT`, `ZQTableOpHour`,
    !> `headerRealArray`, `nZQcols`, `nZQrows`, `zcol`, and `ZQ` from module or
    !> imported state. The stage-discharge lookup is table based and does not
    !> interpolate.
    !> @endnote
    FUNCTION get_ZQTable_value(ZQref,zu) RESULT(qd)

        ! IO variables
        INTEGER(kind=I_P), INTENT(IN)   :: ZQref    !! Index of the ZQ table to use for this reservoir/channel link.
        REAL(kind=R8P), INTENT(IN)      :: Zu       !! Upstream water level or stage used to query the ZQ table.
        REAL(kind=R8P)                  :: Qd       !! Downstream discharge returned from the selected ZQ table column.

        ! general variables
        INTEGER(kind=I_P)               :: i        !! Row or column loop index.

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
