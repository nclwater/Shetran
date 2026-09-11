!> summary: The optional reservoir stage--discharge (ZQ) tables, their metadata and their reader.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> When reservoir routing is enabled, each controlled link face has a tabulated
!> relation between water level and discharge that replaces the usual weir or
!> channel formula. This module holds those tables and the metadata saying
!> which link and face each one applies to, plus the weir-sill elevation below
!> which the table does not apply.
!>
!> [[ReadZQTable]] reads the tables from the `zqd` unit during frame setup and
!> [[get_ZQTable_value]] interpolates one during the weir-flow calculation in
!> [[oc_discharge:QWEIR]]. `iszq` says whether the feature is enabled at all;
!> when it is not, the allocatable arrays are never allocated. Module state is
!> public by default.
!>
!> @warning
!> `ZQTableRef` was declared twice in the pre-reorganisation source, once in
!> `AL_D` and once in the reader, with different documented meanings. Both are
!> kept: the reader's copy is `ZQTableRefRead`, the name `variables.csv` gives
!> it. Whether the two are really one quantity is deliberately left open.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_D; see docs/rename/proposal.md. |
!> | 2026-09-11 | SvB | - | Gained ZQmod's table storage and its two procedures; ZQmod's own `ZQTableRef` is renamed `ZQTableRefRead`. |
!> @endhistory
MODULE zq_tables

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, R8P
   USE simulation_clock, ONLY: UZNOW, UZNEXT
   USE file_units, ONLY: zqd
   USE error_status, ONLY: errstat_alloc, errstat_fileclose, errstat_fileopen, &
                           errstat_read, errstat_rewind, errstat_write

   IMPLICIT NONE

   INTEGER :: NoZQTables !! Number of reservoir ZQ tables read from `zqd`.
   INTEGER :: ZQTableRef !! Index of the ZQ table selected for the current link-face calculation.
   LOGICAL :: iszq       !! Whether reservoir ZQ-table routing is enabled.
   INTEGER, DIMENSION(:), ALLOCATABLE :: ZQTableLink !! Channel-link number for each ZQ table.
   INTEGER, DIMENSION(:), ALLOCATABLE :: ZQTableFace !! Channel-link face number for each ZQ table.
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ZQweirSill !! Weir-sill elevation for each ZQ table (m).


! Table contents and the reader's own state (from ZQmod).
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQcols                  !! Number of columns in each ZQ table.
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQrows                  !! Number of data rows in each ZQ table.
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: zcol                     !! Currently active discharge-column index for each ZQ table.
   REAL(kind=R8P), DIMENSION(:, :), ALLOCATABLE     :: headerRealArray          !! Numeric stage thresholds parsed from `ZQ>threshold` headers.
   REAL(kind=R8P), DIMENSION(:, :, :), ALLOCATABLE   :: ZQ                       !! Stage-discharge table values, indexed by row, column, and table.
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: ZQTableOpHour            !! Hour offset at which each reservoir's sluices are operated.
   INTEGER(kind=I_P)                               :: ZQTableRefRead               !! Reference number read for the current ZQ table.

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
!! | Table reference | Read into `ZQTableRefRead` while loading each table. |
!! | Link and face | Stored in `ZQTableLink` and `ZQTableFace` for [[oc_stage_discharge:OCQDQ]] dispatch. |
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
!! program through [[error_status:errstat_read]] / [[error_status:errstat_fileopen]]
!! if the log file cannot be opened or the table cannot be read.
!! @endnote
!!
!! @history
!! | Date | Author | Version | Description |
!! |:-----|:-------|:--------|:------------|
!! | 2020 | DH/SB | SHETRAN 4.4.6.Res2 | Added reservoir ZQ lookup-table support. |
!! | 2026-04-03 | SvB | | Replaced `GOTO`/labelled `STOP` error handling with `IOSTAT` checks and the internal `handle_zq_error` subroutine; made the header-token loops robust to runs of multiple spaces via `ADJUSTL`. |
!! | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!! | 2026-09-06 | SvB | - | Routed the log-file `OPEN` and every checked `READ` through the standardised [[error_status]] checks, reporting `IOSTAT`/`IOMSG`, and removed the internal `handle_zq_error` subroutine. |
!! | 2026-09-06 | SvB | - | Checked both `CLOSE` statements through [[error_status:errstat_fileclose]]. |
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
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG=/IOMSG= text from a failed (de)allocation, open, or read.
      CHARACTER(LEN=*), PARAMETER :: location = "ZQmod:ReadZQTable"                        !! Location string for error messages.

      ! specific variables
      CHARACTER(LEN=120)                              :: headerRaw                         !! Raw ZQ table header line while it is being split.
      CHARACTER(LEN=9), DIMENSION(:, :), ALLOCATABLE   :: headerRawArray                    !! Raw header tokens by column and table.
      CHARACTER(LEN=9), DIMENSION(:, :), ALLOCATABLE   :: headerCharArray                   !! Numeric part of each `ZQ>threshold` header as text.
      INTEGER(KIND=I_P)                               :: maxnumberRows, maxnumberCols      !! Maximum row/column count over all ZQ tables.
      LOGICAL                                         :: IsZQreadOK = .FALSE.              !! Unused legacy read-status flag.

      INTEGER(KIND=I_P)                               :: fid_ZQ_log                        !! Unit number for `output_readZQTable.txt`.

      ! Code -----------------------------------------------------------------
      OPEN (NEWUNIT=fid_ZQ_log, FILE='output_readZQTable.txt', IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileopen(ios, 'output_readZQTable.txt', emsg)

      ! read ZQ tables
      READ (zqd, *, IOSTAT=ios, IOMSG=emsg)                                             ! skip line 1
      CALL errstat_read(ios, location, emsg)

      READ (zqd, *, IOSTAT=ios, IOMSG=emsg) NoZQTables                                  ! read line 2
      CALL errstat_read(ios, location, emsg)

      ALLOCATE (nZQcols(NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "nZQcols", location, emsg)
      ALLOCATE (nZQrows(NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "nZQrows", location, emsg)
      ALLOCATE (zcol(NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "zcol", location, emsg)
      ALLOCATE (ZQTableLink(NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ZQTableLink", location, emsg)
      ALLOCATE (ZQTableFace(NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ZQTableFace", location, emsg)
      ALLOCATE (ZQTableOpHour(NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ZQTableOpHour", location, emsg)
      ALLOCATE (ZQWeirSill(NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ZQWeirSill", location, emsg)

      DO i = 1, NoZQTables                                                            ! loop through ZQtables
         DO j = 1, 9
            READ (zqd, *, IOSTAT=ios, IOMSG=emsg)                                     ! skip lines 3-11
            CALL errstat_read(ios, location, emsg)
         END DO

         READ (zqd, *, IOSTAT=ios, IOMSG=emsg) nZQrows(i)                             ! read line 12
         CALL errstat_read(ios, location, emsg)

         READ (zqd, *, IOSTAT=ios, IOMSG=emsg)                                        ! skip line 13
         CALL errstat_read(ios, location, emsg)

         READ (zqd, "(A)", IOSTAT=ios, IOMSG=emsg) headerRaw                          ! read line 14
         CALL errstat_read(ios, location, emsg)

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
            READ (zqd, *, IOSTAT=ios, IOMSG=emsg)                                     ! read ZQ table as zqd
            CALL errstat_read(ios, location, emsg)
         END DO
      END DO

      maxnumberRows = MAXVAL(nZQrows)
      maxnumberCols = MAXVAL(nZQcols)

      ! allocate array dimensions using maxnumberRows and maxnumberCols
      ALLOCATE (ZQ(maxnumberRows, maxnumberCols, NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ZQ", location, emsg)
      ALLOCATE (headerRawArray(maxnumberCols, NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "headerRawArray", location, emsg)
      ALLOCATE (headerCharArray(maxnumberCols, NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "headerCharArray", location, emsg)
      ALLOCATE (headerRealArray(maxnumberCols, NoZQTables), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "headerRealArray", location, emsg)
      REWIND (zqd, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_rewind(ios, fid=zqd, iomsg=emsg)

      ! read ZQ metadata
      READ (zqd, *, IOSTAT=ios)                                                        ! skip line 1
      READ (zqd, *, IOSTAT=ios)                                                        ! skip line 2

      DO i = 1, NoZQTables

         zcol(i) = 2                                                                 ! set zcol=2 to start with

         READ (zqd, *, IOSTAT=ios, IOMSG=emsg)                                        ! skip line 3
         CALL errstat_read(ios, location, emsg)
         READ (zqd, *, IOSTAT=ios, IOMSG=emsg) ZQTableRefRead                             ! read line 4
         CALL errstat_read(ios, location, emsg)
         READ (zqd, *, IOSTAT=ios, IOMSG=emsg)                                        ! skip line 5
         CALL errstat_read(ios, location, emsg)
         READ (zqd, *, IOSTAT=ios, IOMSG=emsg) ZQTableLink(i)                         ! read line 6
         CALL errstat_read(ios, location, emsg)
         READ (zqd, *, IOSTAT=ios, IOMSG=emsg)                                        ! skip line 7
         CALL errstat_read(ios, location, emsg)
         READ (zqd, *, IOSTAT=ios, IOMSG=emsg) ZQTableFace(i)                         ! read line 8
         CALL errstat_read(ios, location, emsg)
         READ (zqd, *, IOSTAT=ios, IOMSG=emsg)                                        ! skip line 9
         CALL errstat_read(ios, location, emsg)
         READ (zqd, *, IOSTAT=ios, IOMSG=emsg) ZQTableOpHour(i)                       ! read line 10
         CALL errstat_read(ios, location, emsg)

         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 11
         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 12
         READ (zqd, *, IOSTAT=ios)                                                    ! skip line 13
         READ (zqd, "(A)", IOSTAT=ios, IOMSG=emsg) headerRaw                          ! read line 14
         CALL errstat_read(ios, location, emsg)

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
            READ (headerCharArray(j, i), *, IOSTAT=ios, IOMSG=emsg) headerRealArray(j, i)
            CALL errstat_read(ios, location, emsg)
         END DO

         ! read ZQweirSill as lowest value of headers
         ZQweirSill(i) = headerRealArray(2, i)

         DO j = 1, nZQrows(i)
            READ (zqd, *, IOSTAT=ios, IOMSG=emsg) (ZQ(j, k, i), k=1, nZQcols(i))
            CALL errstat_read(ios, location, emsg)
         END DO

         ! write ZQTables to fid_ZQ_log.fort
         ios = 0
         IF (ios == 0) WRITE (fid_ZQ_log, *, IOSTAT=ios, IOMSG=emsg) 'ZQTableRefRead   =', ZQTableRefRead
         IF (ios == 0) WRITE (fid_ZQ_log, *, IOSTAT=ios, IOMSG=emsg) 'ZQTableLink  =', ZQTableLink(i)
         IF (ios == 0) WRITE (fid_ZQ_log, *, IOSTAT=ios, IOMSG=emsg) 'ZQTableFace  =', ZQTableFace(i)
         IF (ios == 0) WRITE (fid_ZQ_log, *, IOSTAT=ios, IOMSG=emsg) 'ZQTableOpHour=', ZQTableOpHour(i)
         IF (ios == 0) WRITE (fid_ZQ_log, *, IOSTAT=ios, IOMSG=emsg) 'nZQcols      =', nZQcols(i)
         IF (ios == 0) WRITE (fid_ZQ_log, *, IOSTAT=ios, IOMSG=emsg) 'nZQrows      =', nZQrows(i)

         ! Uses the modern unlimited repeat formatter "(*(...))"
         IF (ios == 0) WRITE (fid_ZQ_log, '(A, *(A10))', IOSTAT=ios, IOMSG=emsg) 'ZQ headers: ', headerRawArray(1:nZQcols(i), i)

         DO printRow = 1, nZQrows(i)
            IF (ios == 0) WRITE (fid_ZQ_log, '(*(F12.3))', IOSTAT=ios, IOMSG=emsg) (ZQ(printRow, printCol, i), printCol=1, nZQcols(i))
         END DO
         CALL errstat_write(ios, location//' (ZQ table log)', emsg, 'output_readZQTable.txt')
      END DO

      CLOSE (zqd, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileclose(ios, fid=zqd, iomsg=emsg)

      CLOSE (fid_ZQ_log, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileclose(ios, 'output_readZQTable.txt', iomsg=emsg)

      RETURN

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

END MODULE zq_tables

