!> summary: Reservoir stage-discharge lookup tables.
!>
!> `ZQmod` implements reservoir outflow lookup from user-supplied ZQ tables.
!> Each table relates upstream water level (stage, `Z`) to downstream discharge
!> (`Q`) for a channel link and face. [[FRmod:frinit]] calls [[ReadZQTable]] to
!> load the table metadata and values into module arrays, which
!> [[ocqdqmod:ocqdq]] and [[get_ZQTable_value]] use later during the simulation.
!>
!> The implementation is a tabulated stage-discharge relationship rather than a
!> fitted hydraulic formula: discharge is selected from the active column by
!> the current upstream stage.
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
!> | 2026-04-03 | SvB | | Removed a non-standard trailing comma from a `WRITE` statement in [[ReadZQTable]] (accepted by some compilers as an extension, but not standard Fortran). |
!> | 2026-04-03 | SvB | | Modernised [[ReadZQTable]] to free-form style: replaced `GOTO`/labelled `STOP` error handling with `IOSTAT` checks and a centralised internal `handle_zq_error` subroutine, made the header-token counting and splitting loops robust to runs of multiple spaces via `ADJUSTL`, and switched to unlimited-repeat `(*(...))` format descriptors for the log output. |
!> @endhistory
!>
!> @note The table parser assumes space-delimited input and ascending
!> `ZQ>threshold` headers; the token counting and splitting loops use
!> `ADJUSTL` so runs of multiple spaces between headers are handled correctly.
!> The lookup returns the first table row where `Zu` is not greater than the
!> stored stage value; it does not interpolate between rows. The active
!> discharge column is selected from the `ZQ>threshold` headers only when the
!> configured sluice operation hour crosses a new day.
!> @endnote
!>
!> @note `DTUZ` is imported from `AL_C` but not referenced anywhere in this
!> module; this is pre-existing unused-import legacy, not introduced by any
!> recent change.
!> @endnote
module ZQmod

   USE sglobal, ONLY: UZNOW                                                 ! simulation time (hours)
   USE AL_C, ONLY: DTUZ, UZNEXT                                           ! DTUZ is unused; UZNEXT is the time step to be added to the previous time to get the current time
   USE AL_D, ONLY: zqd, NoZQTables, ZQTableLink, ZQTableFace, ZQweirSill     ! module state shared with OCQDQ
   USE mod_parameters                                                          ! general parameters
   USE mod_error, ONLY: errstat_alloc

   IMPLICIT NONE

   ! Module variables

   ! set everything to private by default
   PRIVATE

   ! module variables
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQcols                  !! Number of columns in each ZQ table.
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQrows                  !! Number of data rows in each ZQ table.
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: zcol                     !! Currently active discharge-column index for each ZQ table.
   REAL(kind=R8P), DIMENSION(:, :), ALLOCATABLE     :: headerRealArray          !! Numeric stage thresholds parsed from `ZQ>threshold` headers.
   REAL(kind=R8P), DIMENSION(:, :, :), ALLOCATABLE   :: ZQ                       !! Stage-discharge table values, indexed by row, column, and table.
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: ZQTableOpHour            !! Hour offset at which each reservoir's sluices are operated.
   INTEGER(kind=I_P)                               :: ZQTableRef               !! Reference number read for the current ZQ table.

   ! what is public from this module?
   PUBLIC                                          :: ReadZQTable, get_ZQTable_value   ! subroutine names

CONTAINS

!---------------------------------------------------------------------------
!> Reads the user-defined reservoir ZQ table file.
!!
!! The routine reads the number of ZQ tables, scans each table to determine
!! its row and column count, allocates the module lookup arrays, rewinds the
!! file, then loads metadata and table values. Metadata include the table
!! reference, channel link, channel face, and sluice operation hour. Header
!! strings such as `ZQ>96.8` are converted to numeric stage thresholds and
!! stored in `headerRealArray`.
!!
!! Expected table layout:
!!
!! | File item | Code use |
!! |:----------|:---------|
!! | Number of tables | Allocates per-table metadata and lookup arrays. |
!! | Table reference | Read into `ZQTableRef` while loading each table. |
!! | Link and face | Stored in `ZQTableLink` and `ZQTableFace` for [[ocqdqmod:ocqdq]] dispatch. |
!! | Operation hour | Stored in `ZQTableOpHour`; controls when the active discharge column is reconsidered. |
!! | Header row | First column is stage `Z`; later columns are `ZQ>stage_threshold` discharge columns. |
!! | Data rows | Stage in column 1 and discharges in the selected lookup columns. |
!!
!! Input assumptions:
!!
!! | Assumption | Consequence in the code |
!! |:-----------|:------------------------|
!! | `ZQ>threshold` columns are in ascending threshold order. | `ZQweirSill` is taken from column 2 and column selection scans from high to low. |
!! | Stage rows are in ascending stage order. | [[get_ZQTable_value]] returns the first row with `Zu <= ZQ(row,1,table)`. |
!!
!! @note This routine has no dummy arguments. It reads from the globally
!! opened `zqd` unit, allocates module arrays, allocates ZQ metadata arrays
!! from `AL_D`, writes `output_readZQTable.txt`, closes `zqd`, and stops the
!! program via the internal `handle_zq_error` subroutine (status 255) if the
!! table cannot be read.
!! @endnote
!!
!! @history
!! | Date | Author | Version | Description |
!! |:-----|:-------|:--------|:------------|
!! | 2020 | DH/SB | SHETRAN 4.4.6.Res2 | Added reservoir ZQ lookup-table support. |
!! | 2026-04-03 | SvB | | Replaced `GOTO`/labelled `STOP` error handling with `IOSTAT` checks and the internal `handle_zq_error` subroutine; made the header-token loops robust to runs of multiple spaces via `ADJUSTL`. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!! @endhistory
!---------------------------------------------------------------------------
   SUBROUTINE ReadZQTable()

      ! general variables
      INTEGER(KIND=I_P)                               :: i                                 !! Table and row loop index.
      INTEGER(KIND=I_P)                               :: j                                 !! Header, row, and column loop index.
      INTEGER(KIND=I_P)                               :: k                                 !! Implied-DO column index while reading table values.
      INTEGER(KIND=I_P)                               :: printRow                          !! Row index used when echoing a table to the log file.
      INTEGER(KIND=I_P)                               :: printCol                          !! Column index used when echoing a table to the log file.
      INTEGER(KIND=I_P)                               :: pos                               !! Position of the next space delimiter in `headerRaw`.
      INTEGER(KIND=I_P)                               :: ios                               !! I/O status integer.
      CHARACTER(LEN=*), PARAMETER :: location = "ZQmod:ReadZQTable"                        !! Location string for error messages.

      ! specific variables
      CHARACTER(LEN=120)                              :: headerRaw                         !! Raw ZQ table header line while it is being split.
      CHARACTER(LEN=9), DIMENSION(:, :), ALLOCATABLE   :: headerRawArray                    !! Raw header tokens by column and table.
      CHARACTER(LEN=9), DIMENSION(:, :), ALLOCATABLE   :: headerCharArray                   !! Numeric part of each `ZQ>threshold` header as text.
      INTEGER(KIND=I_P)                               :: maxnumberRows, maxnumberCols      !! Maximum row/column count over all ZQ tables.
      LOGICAL                                         :: IsZQreadOK = .FALSE.              !! Unused legacy read-status flag.

      INTEGER(KIND=I_P)                               :: fid_ZQ_log                        !! Unit number for `output_readZQTable.txt`.

      ! Code -----------------------------------------------------------------
      OPEN (NEWUNIT=fid_ZQ_log, FILE='output_readZQTable.txt', IOSTAT=ios)
      IF (ios /= 0) CALL handle_zq_error()

      ! read ZQ tables
      READ (zqd, *, IOSTAT=ios)                                                        ! skip line 1
      IF (ios /= 0) CALL handle_zq_error()

      READ (zqd, *, IOSTAT=ios) NoZQTables                                             ! read line 2
      IF (ios /= 0) CALL handle_zq_error()

      ALLOCATE (nZQcols(NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "nZQcols", location)
      ALLOCATE (nZQrows(NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "nZQrows", location)
      ALLOCATE (zcol(NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "zcol", location)
      ALLOCATE (ZQTableLink(NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "ZQTableLink", location)
      ALLOCATE (ZQTableFace(NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "ZQTableFace", location)
      ALLOCATE (ZQTableOpHour(NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "ZQTableOpHour", location)
      ALLOCATE (ZQWeirSill(NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "ZQWeirSill", location)

      DO i = 1, NoZQTables                                                            ! loop through ZQtables
         DO j = 1, 9
            READ (zqd, *, IOSTAT=ios)                                                ! skip lines 3-11
            IF (ios /= 0) CALL handle_zq_error()
         END DO

         READ (zqd, *, IOSTAT=ios) nZQrows(i)                                         ! read line 12
         IF (ios /= 0) CALL handle_zq_error()

         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 13
         IF (ios /= 0) CALL handle_zq_error()

         READ (zqd, "(A)", IOSTAT=ios) headerRaw                                      ! read line 14
         IF (ios /= 0) CALL handle_zq_error()

         nZQcols(i) = 0                                                              ! initialise nZQcols counter

         ! Robust token counting loop
         headerRaw = ADJUSTL(headerRaw)
         DO WHILE (LEN_TRIM(headerRaw) > 0)
            pos = INDEX(headerRaw, " ")
            IF (pos == 0) THEN
               headerRaw = ""                                                      ! No more spaces, reached the last element
            ELSE
               headerRaw = ADJUSTL(headerRaw(pos + 1:))                              ! Advance string past space
            END IF
            nZQcols(i) = nZQcols(i) + 1
         END DO

         DO j = 1, nZQrows(i)
            READ (zqd, *, IOSTAT=ios)                                                ! read ZQ table as zqd
            IF (ios /= 0) CALL handle_zq_error()
         END DO
      END DO

      maxnumberRows = MAXVAL(nZQrows)
      maxnumberCols = MAXVAL(nZQcols)

      ! allocate array dimensions using maxnumberRows and maxnumberCols
      ALLOCATE (ZQ(maxnumberRows, maxnumberCols, NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "ZQ", location)
      ALLOCATE (headerRawArray(maxnumberCols, NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "headerRawArray", location)
      ALLOCATE (headerCharArray(maxnumberCols, NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "headerCharArray", location)
      ALLOCATE (headerRealArray(maxnumberCols, NoZQTables), STAT=ios)
      CALL errstat_alloc(ios, "headerRealArray", location)
      REWIND (zqd)

      ! read ZQ metadata
      READ (zqd, *, IOSTAT=ios)                                                        ! skip line 1
      READ (zqd, *, IOSTAT=ios)                                                        ! skip line 2

      DO i = 1, NoZQTables

         zcol(i) = 2                                                                 ! set zcol=2 to start with

         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 3
         IF (ios /= 0) CALL handle_zq_error()
         READ (zqd, *, IOSTAT=ios) ZQTableRef                                         ! read line 4
         IF (ios /= 0) CALL handle_zq_error()
         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 5
         IF (ios /= 0) CALL handle_zq_error()
         READ (zqd, *, IOSTAT=ios) ZQTableLink(i)                                     ! read line 6
         IF (ios /= 0) CALL handle_zq_error()
         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 7
         IF (ios /= 0) CALL handle_zq_error()
         READ (zqd, *, IOSTAT=ios) ZQTableFace(i)                                     ! read line 8
         IF (ios /= 0) CALL handle_zq_error()
         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 9
         IF (ios /= 0) CALL handle_zq_error()
         READ (zqd, *, IOSTAT=ios) ZQTableOpHour(i)                                   ! read line 10
         IF (ios /= 0) CALL handle_zq_error()

         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 11
         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 12
         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 13
         READ (zqd, "(A)", IOSTAT=ios) headerRaw                                      ! read line 14
         IF (ios /= 0) CALL handle_zq_error()

         ! convert headerRaw to headerRawArray
         headerRaw = ADJUSTL(headerRaw)
         DO j = 1, nZQcols(i)
            pos = INDEX(headerRaw, " ")
            IF (pos == 0) pos = LEN_TRIM(headerRaw) + 1
            headerRawArray(j, i) = headerRaw(1:pos - 1)
            IF (pos <= LEN(headerRaw)) THEN
               headerRaw = ADJUSTL(headerRaw(pos + 1:))
            ELSE
               headerRaw = ""
            END IF
         END DO

         ! convert headerRawArray to headerCharArray and then headerRealArray e.g. 'ZQ>96.8' -> 96.80
         headerCharArray(1, i) = 'Z'                                                  ! set the col/row header as 'Z'
         DO j = 2, nZQcols(i)
            pos = INDEX(headerRawArray(j, i), '>')
            headerCharArray(j, i) = headerRawArray(j, i) (pos + 1:)
            READ (headerCharArray(j, i), *, IOSTAT=ios) headerRealArray(j, i)
            IF (ios /= 0) CALL handle_zq_error()
         END DO

         ! read ZQweirSill as lowest value of headers
         ZQweirSill(i) = headerRealArray(2, i)

         DO j = 1, nZQrows(i)
            READ (zqd, *, IOSTAT=ios) (ZQ(j, k, i), k=1, nZQcols(i))
            IF (ios /= 0) CALL handle_zq_error()
         END DO

         ! write ZQTables to fid_ZQ_log.fort
         WRITE (fid_ZQ_log, *) 'ZQTableRef   =', ZQTableRef
         WRITE (fid_ZQ_log, *) 'ZQTableLink  =', ZQTableLink(i)
         WRITE (fid_ZQ_log, *) 'ZQTableFace  =', ZQTableFace(i)
         WRITE (fid_ZQ_log, *) 'ZQTableOpHour=', ZQTableOpHour(i)
         WRITE (fid_ZQ_log, *) 'nZQcols      =', nZQcols(i)
         WRITE (fid_ZQ_log, *) 'nZQrows      =', nZQrows(i)

         ! Uses the modern unlimited repeat formatter "(*(...))"
         WRITE (fid_ZQ_log, '(A, *(A10))') 'ZQ headers: ', headerRawArray(1:nZQcols(i), i)

         DO printRow = 1, nZQrows(i)
            WRITE (fid_ZQ_log, '(*(F12.3))') (ZQ(printRow, printCol, i), printCol=1, nZQcols(i))
         END DO
      END DO

      CLOSE (zqd)
      CLOSE (fid_ZQ_log)

      RETURN

   CONTAINS

      !> Centralised error handler for [[ReadZQTable]], replacing legacy `GOTO` jumps to a labelled statement.
      !!
      !! Prints a fixed diagnostic message and halts the program with
      !! `ERROR STOP 255`, non-interactively, whenever an `IOSTAT` check in the
      !! host subroutine detects a read or open failure.
      SUBROUTINE handle_zq_error()
         PRINT *, 'error reading ZQ table'
         ! Uses F2008+ standard ERROR STOP to safely exit execution with a status code
         ERROR STOP 255
      END SUBROUTINE handle_zq_error

   END SUBROUTINE ReadZQTable

   !---------------------------------------------------------------------------
   !> Returns downstream discharge from a reservoir ZQ lookup table.
   !!
   !! The function selects the active discharge column for `ZQref` when a new
   !! operating day is crossed, using `ZQTableOpHour` and the current SHETRAN
   !! time. It then scans the stage column and returns the discharge value
   !! from the selected column. If `Zu` is above the largest checked row
   !! before a match is found, the current implementation assigns `-999` as a
   !! missing or out-of-range value.
   !!
   !! Column selection is stepwise: on a sluice-operation boundary the highest
   !! header threshold lower than the current upstream stage is selected and
   !! held until the next operation boundary. Row selection is also stepwise
   !! and does not interpolate between tabulated stages.
   !!
   !! @history
   !! | Date | Author | Version | Description |
   !! |:-----|:-------|:--------|:------------|
   !! | 2020 | DH/SB | SHETRAN 4.4.6.Res2 | Added reservoir ZQ lookup-table support; the routine was originally the `ZQTable` subroutine and was changed to the `get_ZQTable_value` function shortly afterwards. |
   !! @endhistory
   !!
   !! @note This routine uses `UZNOW`, `UZNEXT`, `ZQTableOpHour`,
   !! `headerRealArray`, `nZQcols`, `nZQrows`, `zcol`, and `ZQ` from module or
   !! imported state. The stage-discharge lookup is table based and does not
   !! interpolate.
   !! @endnote
   FUNCTION get_ZQTable_value(ZQref, zu) RESULT(qd)

      ! IO variables
      INTEGER(kind=I_P), INTENT(IN)   :: ZQref    !! Index of the ZQ table to use for this reservoir/channel link.
      REAL(kind=R8P), INTENT(IN)      :: Zu       !! Upstream water level or stage used to query the ZQ table.
      REAL(kind=R8P)                  :: Qd       !! Downstream discharge returned from the selected ZQ table column.

      ! general variables
      INTEGER(kind=I_P)               :: i        !! Row or column loop index.

      ! Code -----------------------------------------------------------------

      ! start sluice operation loop
      IF (INT(UZNOW + ZQTableOpHour(ZQref))/24 >                            &
      &           INT(UZNOW + ZQTableOpHour(ZQref) - UZNEXT)/24) THEN               ! if current day integer > previous day INT(UZNOW), then operate sluices:
         !WRITE(778, *), 'new day'                                            ! write for test purposes

         ! select weir equation (Zcol) based on which range of stages Zu falls into
         ! NB if Zu < min ZQ threshold, will return an error
         DO i = nZQcols(ZQref), 2, -1                                        ! start loop in descending order of ZQ thresholds
            IF (Zu > headerRealArray(i, ZQref)) THEN                          ! test if Zu > ZQ threshold
               zcol(ZQref) = i                                             ! if TRUE, then pass i (nZQcol) to zcol...
               EXIT                                                        ! ...and exit
            ELSEIF (Zu > headerRealArray(2, ZQref)) THEN                      ! elseif Zu is greater than the minimum ZQ threshold ->restart loop
            ELSE                                                            ! else Zu is below threshold, print warning and exit loop
               PRINT *,                                                     &
               &                       'warning: Zu is below minimum ZQthreshold defined in ZQtable'
               EXIT
            END IF
         END DO
      END IF                                                                   ! end sluice operation loop

      ! look up z value in ZQ array which matches Zu and return corresponding Qd
      DO i = 1, nZQrows(ZQref)                                                ! start loop through rows for a given table
         IF (Zu > ZQ(i, 1, ZQref)) THEN                                       ! if Zu is greater than the ith value in the z column...
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
