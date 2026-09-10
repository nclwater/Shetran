!> summary: The legacy `AL*` record readers for the typed input formats.
!> author: AB / RAH, Newcastle University; JE, Newcastle University
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
!>
!> The typed readers for the generic list formats defined in User Manual
!> Appendix A: [[ALREAD]] and its contained `throw_fatal` helper for the mixed
!> format, and [[ALRED2]], [[ALREDC]], [[ALREDF]], [[ALREDI]] and [[ALREDL]]
!> for the single-type ones.
!>
!> Input headings are matched as case-sensitive substrings rather than exact
!> records. A mismatch is a warning and reading continues; missing or malformed
!> data are normally fatal through [[error_reporting:RAISE_ERROR]]. The readers
!> retain explicit-shape legacy interfaces, so some callers use valid Fortran
!> sequence association by passing an array element as the start of a
!> contiguous data sequence.
!>
!> @warning
!> `ALREAD` currently declares all three possible data destinations as
!> `INTENT(OUT)`. Standard Fortran therefore makes `CDATA`, `IDATA`, and
!> `RDATA` undefined on every call, including the two arrays not selected by
!> `FLAG`. In the multi-category path, [[spatial_fields:ALALLF]] expects values
!> previously read into `DUMMY` to survive later integer-only `ALREAD` calls.
!> That expectation conflicts with the current interface and is documented here
!> without changing it.
!>
!> Only `HEAD0_alread` is maintained as a previous-heading diagnostic.
!> `HEAD0_alredc`, `HEAD0_alredi`, `HEAD0_alredf`, and `HEAD0_alredl` are read
!> on heading failures but never updated, so they retain their initial text;
!> `HEAD0_alred2` is updated but never read. These are current diagnostic-state
!> limitations, not automatic per-reader histories.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of mod_load_filedata; see docs/rename/proposal.md. |
!> @endhistory
MODULE record_readers

   USE MOD_PARAMETERS, ONLY: I_P, LENGTH_LINE, R8P
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERRLVL_warn
   USE error_status, ONLY: errstat_fileclose

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: ALREAD, ALRED2, ALREDC, ALREDF, ALREDI, ALREDL

   CHARACTER(len=80) :: HEAD0_alread = '( nothing read yet )' !! Most recent successful heading or status text processed by `ALREAD`.
   CHARACTER(len=80) :: HEAD0_alredc = '( nothing read yet )' !! Fixed fallback text used by `ALREDC` heading-read errors; never updated.
   CHARACTER(len=80) :: HEAD0_alredi = '( nothing read yet )' !! Fixed fallback text used by `ALREDI` heading-read errors; never updated.
   CHARACTER(len=80) :: HEAD0_alred2 = '( nothing read yet )' !! Most recent `ALRED2` status text; currently written but never read.
   CHARACTER(len=80) :: HEAD0_alredl = '( nothing read yet )' !! Fixed fallback text used by `ALREDL` heading-read errors; never updated.
   CHARACTER(len=80) :: HEAD0_alredf = '( nothing read yet )' !! Fixed fallback text used by `ALREDF` heading-read errors; never updated.

CONTAINS

   !> Performs mixed file-status, character, integer, real, grid, and VSS input operations.
   !>
   !> For positive `FLAG`, `ALREAD` first reads an 80-character heading and
   !> checks whether it contains `LINE`. A mismatch raises warning 2 but does
   !> not prevent the selected data read. Zero and negative modes instead use
   !> `LINE` as the label in the file-status message.
   !>
   !> | `FLAG` | Operation | Principal dimensions/count |
   !> |:-------|:----------|:---------------------------|
   !> | `-1` | Close `IUNIT` and echo its status. | None. |
   !> | `0` | Require `IUNIT` to be open and echo its status. | None. |
   !> | `1` | Read one `(A)` character record into `CDATA`. | Character length of `CDATA`. |
   !> | `2` | Read a list-directed integer array. | `IDATA(N1,N2)`. |
   !> | `3` | Read a list-directed real array. | `RDATA(N1,N2)`. |
   !> | `4` | Read indexed integer-grid rows from `N2` down to 1. | `NUM_CATEGORIES_TYPES < 10` selects compact `I1`; otherwise list-directed input. |
   !> | `5` | Read indexed list-directed real-grid rows from `N2` down to 1. | Each record starts with the expected row number. |
   !> | `6` | Read indexed VSS item records. | Repeat `NUM_CATEGORIES_TYPES` times: item number, item count, integer values, then real values. |
   !> | `7` | Read VSS soil physical-property records. | For each category, three integers whose first value is the sequential ID, then eight reals. |
   !>
   !> Successful calls store the current heading or status in the private
   !> `HEAD0_alread`, which a later heading-read failure includes in its message.
   !> Error codes 3--7, 10--11, 14, and 16 distinguish heading, file/data,
   !> grid-row, soil, and VSS-record failures; contained helper `throw_fatal` dispatches
   !> each through [[error_reporting:RAISE_ERROR]] with fatal severity.
   !>
   !> @warning
   !> There is no `CASE DEFAULT`; unsupported flags may consume a heading and
   !> return without reading data, while unsupported negative flags can copy an
   !> undefined `HEAD` into `HEAD0_alread`. Modes 6 and 7 do not validate input
   !> item indices/counts against `N1` and `N2` before using them as subscripts.
   !>
   !> Because `CDATA`, `IDATA`, and `RDATA` all have `INTENT(OUT)`, standard
   !> Fortran makes every actual destination undefined on entry regardless of
   !> which `FLAG` is selected. Callers must not rely on either unused array
   !> retaining its previous value, although current [[ALALLF]] does so in its
   !> multi-category path.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial mixed-format reader. |
   !> | 1994-09-12 | GP | 4.0 | Added VSS modes 6 and 7. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy input reader. |
   !> | 1997-08-04 | RAH | 4.1 | Added end-of-file handling to modes 6 and 7 and renumbered the VSS error as 16. |
   !> | 2025-10-02 | SB | - | Increased the diagnostic message buffer from 132 to 140 characters. |
   !> | 2026-04-06 | SvB | - | Replaced error jumps with `SELECT CASE`, `IOSTAT`, and the contained fatal-error helper. |
   !> | 2026-09-06 | SvB | - | Checked the `CLOSE` through [[error_status:errstat_fileclose]], reporting `IOSTAT`/`IOMSG`. |
   !> @endhistory
   SUBROUTINE ALREAD(FLAG, IUNIT, OUNIT, LINE, N1, N2, NUM_CATEGORIES_TYPES, &
                     CDATA, IDATA, RDATA)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, R8P, ERRLVL_warn, ERRLVL_fatal, HEAD0_alread, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Operation selector from -1 through 7.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Input unit to inspect, close, or read.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving status output and diagnostics.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! First array extent or grid x extent.
      INTEGER(kind=I_P), INTENT(IN) :: N2 !! Second array extent or grid y extent.
      INTEGER(kind=I_P), INTENT(IN) :: NUM_CATEGORIES_TYPES !! Grid-code threshold or VSS record count, depending on `FLAG`.
      CHARACTER(LEN=*), INTENT(IN) :: LINE !! Expected heading substring or file-status label.

      ! Output arguments
      CHARACTER(LEN=*), INTENT(OUT) :: CDATA !! Character record returned by mode 1; undefined on entry for every mode.
      INTEGER(kind=I_P), INTENT(OUT) :: IDATA(N1, N2) !! Integer destination for modes 2, 4, 6, and 7; undefined on entry for every mode.
      REAL(kind=R8P), INTENT(OUT) :: RDATA(N1, N2) !! Real destination for modes 3, 5, 6, and 7; undefined on entry for every mode.

      ! Locals, etc
      CHARACTER(LEN=80) :: HEAD !! Heading read from input or formatted file-status text.
      CHARACTER(LEN=140) :: MSG !! Fatal/warning message buffer.
      CHARACTER(LEN=48) :: FILNAM !! Possibly truncated filename returned by `INQUIRE`.
      CHARACTER(LEN=17) :: FORM !! Generated compact integer-grid format.
      INTEGER(kind=I_P) :: IX !! Grid x or inner implied-DO index.
      INTEGER(kind=I_P) :: IY !! Expected indexed-grid row, processed north to south.
      INTEGER(kind=I_P) :: KY !! Row number read from an indexed-grid record.
      INTEGER(kind=I_P) :: IDUM1 !! VSS item index read by mode 6.
      INTEGER(kind=I_P) :: IDUM2 !! VSS item value count read by mode 6.
      INTEGER(kind=I_P) :: ICOUNT !! VSS/soil record iterator.
      INTEGER(kind=I_P) :: I !! Inner implied-DO index for VSS records.
      INTEGER(kind=I_P) :: ios !! I/O status from the most recent read or close.
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed close.
      LOGICAL :: BOPEN !! True when `IUNIT` is connected.
      LOGICAL :: BNAMED !! True when the connected unit has a filename.

      ! Code =================================================================

      !----------------------------------------------------------------------*
      ! Preliminaries
      ! -------------

      IF (FLAG > 0) THEN
         ! Check data header against what the caller expects to find
         READ (IUNIT, '(A)', IOSTAT=ios) HEAD

         IF (ios /= 0) THEN
            WRITE (MSG, 9801) LINE, HEAD0_alread
            CALL throw_fatal(3, MSG)
         END IF

         IF (INDEX(HEAD, LINE) == 0) THEN
            WRITE (MSG, 9002) LINE, HEAD
            CALL RAISE_ERROR(ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
         END IF

      ELSE
         ! Get file status and name
         INQUIRE (IUNIT, OPENED=BOPEN, NAMED=BNAMED, NAME=FILNAM)
         IF (.NOT. BNAMED) FILNAM = '(no name)'
      END IF

      ! Take Specified Action
      ! ---------------------
      SELECT CASE (FLAG)

         ! Check that input file is open
      CASE (0)
         IF (.NOT. BOPEN) THEN
            WRITE (MSG, 9000) LINE, 'not open', IUNIT
            CALL throw_fatal(4, MSG)
         END IF

         ! Write (and store) an informative message
         WRITE (HEAD, 9000) LINE, 'open', IUNIT, FILNAM
         WRITE (OUNIT, 9001) HEAD

         ! Close input file
      CASE (-1)
         CLOSE (IUNIT, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, TRIM(FILNAM), IUNIT, emsg)

         ! Write (and store) an informative message
         WRITE (HEAD, 9000) LINE, 'closed', IUNIT, FILNAM
         WRITE (OUNIT, 9001) HEAD

         ! Read a character string
      CASE (1)
         READ (IUNIT, '(A)', IOSTAT=ios) CDATA
         IF (ios /= 0) THEN
            WRITE (MSG, 9810) 'character', HEAD
            CALL throw_fatal(5, MSG)
         END IF

         ! Read an INTEGER(kind=I_P) array
      CASE (2)
         READ (IUNIT, *, IOSTAT=ios) IDATA
         IF (ios /= 0) THEN
            WRITE (MSG, 9810) 'integer', HEAD
            CALL throw_fatal(6, MSG)
         END IF

         ! Read a floating-point array
      CASE (3)
         READ (IUNIT, *, IOSTAT=ios) RDATA
         IF (ios /= 0) THEN
            WRITE (MSG, 9810) 'floating-point', HEAD
            CALL throw_fatal(7, MSG)
         END IF

         ! Read an INTEGER(kind=I_P) grid array
      CASE (4)
         ! Set format string to read single digit integers if possible
         IF (NUM_CATEGORIES_TYPES < 10) WRITE (FORM, 9410) N1

         ! All grid rows: North to South
         DO IY = N2, 1, -1
            IF (NUM_CATEGORIES_TYPES < 10) THEN
               READ (IUNIT, FORM, IOSTAT=ios) KY, (IDATA(IX, IY), IX=1, N1)
            ELSE
               READ (IUNIT, *, IOSTAT=ios) KY, (IDATA(IX, IY), IX=1, N1)
            END IF

            IF (ios /= 0 .OR. KY /= IY) THEN
               WRITE (MSG, 9842) 'integer', IY, HEAD
               CALL throw_fatal(10, MSG)
            END IF
         END DO

         ! Read a floating point grid array
      CASE (5)
         ! All grid rows: North to South
         DO IY = N2, 1, -1
            READ (IUNIT, *, IOSTAT=ios) KY, (RDATA(IX, IY), IX=1, N1)
            IF (ios /= 0 .OR. KY /= IY) THEN
               WRITE (MSG, 9842) 'floating-point', IY, HEAD
               CALL throw_fatal(11, MSG)
            END IF
         END DO

         ! Read data in VSS format for each element
      CASE (6)
         DO ICOUNT = 1, NUM_CATEGORIES_TYPES
            READ (IUNIT, *, IOSTAT=ios) IDUM1, IDUM2
            IF (ios == 0) READ (IUNIT, *, IOSTAT=ios) (IDATA(IDUM1, I), I=1, IDUM2)
            IF (ios == 0) READ (IUNIT, *, IOSTAT=ios) (RDATA(IDUM1, I), I=1, IDUM2)

            IF (ios /= 0) THEN
               WRITE (MSG, 9600) IDUM1, HEAD
               CALL throw_fatal(16, MSG)
            END IF
         END DO

         ! Read soil physical property data for VSS
      CASE (7)
         DO ICOUNT = 1, NUM_CATEGORIES_TYPES
            READ (IUNIT, *, IOSTAT=ios) (IDATA(ICOUNT, I), I=1, 3)
            IF (ios == 0 .AND. IDATA(ICOUNT, 1) == ICOUNT) THEN
               READ (IUNIT, *, IOSTAT=ios) (RDATA(ICOUNT, I), I=1, 8)
            ELSE
               ! Trigger the error format if the IDs don't match
               ios = 1
            END IF

            IF (ios /= 0) THEN
               WRITE (MSG, 9700) ICOUNT, HEAD
               CALL throw_fatal(14, MSG)
            END IF
         END DO

      END SELECT

      ! Epilogue
      ! --------
      ! Store current title as old title
      HEAD0_alread = HEAD

      RETURN

      ! Format Statements ----------------------------------------------------

      ! -----------------
      ! Note: Take care not to exceed internal file length
9000  FORMAT(A, ' data file ', A, ': unit', I3:'; ', A)
9001  FORMAT(1X, A/)
9002  FORMAT('Title line mismatch: expected "', A, &
             '" but found "', A, '"')
9410  FORMAT('(I7,1X,', I4, 'I1)')
9600  FORMAT('Reading VSS data for item no. ', I4, ' under title: ', A)
9700  FORMAT('Reading soils data for soil no. ', I4, ' under title: ', A)
9801  FORMAT('Reading heading: ', A, '; last item was: ', A)
9810  FORMAT('Reading ', A, ' data under heading: ', A)
9842  FORMAT('Reading ', A, ' grid (IY=', I4, ') under title: ', A)

   CONTAINS

      !> Reports one `ALREAD` I/O failure as fatal.
      !>
      !> This contained helper host-associates `OUNIT` from [[ALREAD]] and calls
      !> [[error_reporting:RAISE_ERROR]] with `ERRLVL_fatal`, the supplied legacy error identifier,
      !> no element/cell context, and the already formatted message. `ERROR`
      !> terminates normal execution for fatal severity.
      !>
      !> @history
      !> | Date | Author | Version | Description |
      !> |:-----|:-------|:--------|:------------|
      !> | 2026-04-06 | SvB | - | Extracted repeated fatal-error dispatch while replacing `GOTO` paths. |
      !> @endhistory
      SUBROUTINE throw_fatal(err_id, err_msg)
         INTEGER(kind=I_P), INTENT(IN) :: err_id !! Legacy error code passed to `ERROR`.
         CHARACTER(LEN=*), INTENT(IN) :: err_msg !! Fully formatted diagnostic text.

         CALL RAISE_ERROR(ERRLVL_fatal, err_id, OUNIT, 0, 0, err_msg)
      END SUBROUTINE throw_fatal

   END SUBROUTINE ALREAD

   !> Checks or closes a legacy AL-family input file and echoes its status.
   !>
   !> `ALRED2` obtains the connection state and filename for `IUNIT`. With
   !> `FLAG=0` it requires the unit to be open; any other flag closes the unit.
   !> It writes a blank-line-terminated status message containing `LINE`, the
   !> operation, unit number, and filename to `OUNIT`, then stores that message
   !> in private `HEAD0_alred2`.
   !>
   !> The old header's statement that this routine contains `ENTRY` statements
   !> is obsolete. The 1995 refactor separated the character, real, integer,
   !> and logical operations into [[ALREDC]], [[ALREDF]], [[ALREDI]], and
   !> [[ALREDL]]; callers invoke those routines independently after this file
   !> lifecycle check.
   !>
   !> @note
   !> `HEAD0_alred2` has no current reader, so storing the status has no effect
   !> on later diagnostics. Its 80-character length also truncates the
   !> 152-character `HEAD`, as reported by gfortran. Closing is selected by
   !> every nonzero `FLAG`, not only by one distinguished value.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial input-file management routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Replaced the former `ENTRY` interface with separate type-specific `ALRED*` routines. |
   !> | 2025-10 | SB | - | Expanded the status, filename, and diagnostic buffers. |
   !> | 2026-04-06 | SvB | - | Replaced the file-not-open jump with structured error handling. |
   !> | 2026-09-06 | SvB | - | Checked the `CLOSE` through [[error_status:errstat_fileclose]], reporting `IOSTAT`/`IOMSG`. |
   !> @endhistory
   SUBROUTINE ALRED2(FLAG, IUNIT, OUNIT, LINE)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, ERRLVL_fatal, HEAD0_alred2, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Zero checks the connection; any nonzero value closes it.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Input unit to inspect or close.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving the status message and fatal diagnostic.
      CHARACTER(LEN=*), INTENT(IN) :: LINE !! File-role label included in status text.

      ! Locals
      CHARACTER(152) :: HEAD !! Formatted open/closed status stored and written to `OUNIT`.
      CHARACTER(120) :: FILNAM !! Filename returned by `INQUIRE`, or `(no name)`.
      CHARACTER(200) :: MSG !! Fatal file-not-open message.
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed close.
      INTEGER(kind=I_P) :: ios !! I/O status from the close.
      LOGICAL :: BOPEN !! True when `IUNIT` is connected.
      LOGICAL :: BNAMED !! True when the connected unit has an associated filename.

      ! Code -----------------------------------------------------------------

      !
      ! File Management
      ! ---------------
      !
      ! Get file status and name
      INQUIRE (IUNIT, OPENED=BOPEN, NAMED=BNAMED, NAME=FILNAM)
      IF (.NOT. BNAMED) FILNAM = '(no name)'

      IF (FLAG == 0) THEN

         ! Check that input file is open
         IF (.NOT. BOPEN) THEN
            WRITE (MSG, 9000) LINE, 'not open', IUNIT
            CALL RAISE_ERROR(ERRLVL_fatal, 4, OUNIT, 0, 0, MSG)
            RETURN
         END IF

         WRITE (HEAD, 9000) LINE, 'open', IUNIT, FILNAM

      ELSE
         ! Close input file
         CLOSE (IUNIT, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, TRIM(FILNAM), IUNIT, emsg)
         WRITE (HEAD, 9000) LINE, 'closed', IUNIT, FILNAM
      END IF

      ! HEAD now contains an informative message
      WRITE (OUNIT, 9001) HEAD

      ! Store current title as old title
      HEAD0_alred2 = HEAD

      RETURN

      ! Formats --------------------------------------------------------------
9000  FORMAT(A, ' data file ', A, ': unit', I3:'; ', A)
9001  FORMAT(1X, A/)

   END SUBROUTINE ALRED2

   !> Reads a heading followed by fixed-format character records.
   !>
   !> `ALREDC` reads a heading into a 150-character buffer and tests whether it
   !> contains `LINE`. A mismatch raises warning 2 and input continues. It then
   !> reads `CDATA(N1,N2)` with format `(A)`; format reversion advances through
   !> successive records when more than one array item is requested. Current
   !> contaminant and nitrate callers request a single item.
   !>
   !> `FLAG` is retained for family-wide call compatibility but is not read.
   !> Heading and character-data failures are fatal errors 3 and 5. The heading
   !> error reports `HEAD0_alredc`, but that module field is never updated and
   !> therefore always says that nothing has yet been read.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial character input routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Created the separate character reader during removal of the former `ENTRY` interface. |
   !> | 2025-10 | SB | - | Increased heading and message buffers to 150 and 200 characters. |
   !> @endhistory
   SUBROUTINE ALREDC(FLAG, IUNIT, OUNIT, LINE, N1, N2, CDATA)

      ! Input arguments
      INTEGER(kind=I_P) :: FLAG !! Unused selector retained for interface consistency.
      INTEGER(kind=I_P) :: IUNIT !! Open input unit positioned before the heading.
      INTEGER(kind=I_P) :: OUNIT !! Unit receiving warning/fatal diagnostics.
      INTEGER(kind=I_P) :: N1 !! First extent of `CDATA`.
      INTEGER(kind=I_P) :: N2 !! Second extent of `CDATA`.
      CHARACTER(LEN=*) :: LINE !! Expected case-sensitive heading substring.

      ! Output arguments
      CHARACTER(LEN=*) :: CDATA(N1, N2) !! Character records read in Fortran array element order.
      CHARACTER(len=150) :: HEAD !! Heading record read from `IUNIT`.
      CHARACTER(len=200) :: MSG !! Warning/fatal message buffer.

      ! Code -----------------------------------------------------------------

      READ (IUNIT, '(A)', ERR=8010, END=8010) HEAD
      IF (INDEX(HEAD, LINE) == 0) THEN
         WRITE (MSG, 9002) LINE, HEAD
         CALL RAISE_ERROR(ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
      END IF

      !  Read character data
      !  -------------------
      READ (IUNIT, '(A)', ERR=8100, END=8100) CDATA

      RETURN

      ! Errors ---------------------------------------------------------------

      ! Title line read error
8010  WRITE (MSG, 9801) LINE, HEAD0_alredc
      CALL RAISE_ERROR(ERRLVL_fatal, 3, OUNIT, 0, 0, MSG)

      ! Char data error
8100  WRITE (MSG, 9810) 'character', HEAD
      CALL RAISE_ERROR(ERRLVL_fatal, 5, OUNIT, 0, 0, MSG)

      ! Format ---------------------------------------------------------------

9002  FORMAT('Title line mismatch: expected "', A, &
             '" but found "', A, '"')

9801  FORMAT('Reading heading: ', A, '; last item was: ', A)

9810  FORMAT('Reading ', A, ' data under heading: ', A)

9842  FORMAT('Reading ', A, ' grid (IY=', I4, ') under title: ', A)

   END SUBROUTINE ALREDC

   !> Reads a heading followed by a real list or indexed real grid.
   !>
   !> A heading containing `LINE` is expected first. A mismatch raises warning
   !> 2 but reading continues. `FLAG=0` reads all of `FDATA(N1,N2)` by
   !> list-directed input. Any nonzero flag reads `N2` indexed grid records in
   !> north-to-south order (`IY=N2,...,1`), each containing its row number and
   !> `N1` floating-point values. An I/O failure or wrong row number raises
   !> fatal error 11; heading and simple-array failures use errors 3 and 7.
   !>
   !> `HEAD0_alredf` is displayed on a heading-read failure but is never updated,
   !> so its text remains `( nothing read yet )` throughout the run.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial real input routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Created the separate real reader and renamed the destination `FDATA`. |
   !> | 2026-04-06 | SvB | - | Replaced error jumps with `IOSTAT` checks and structured returns. |
   !> @endhistory
   SUBROUTINE ALREDF(FLAG, IUNIT, OUNIT, LINE, N1, N2, FDATA)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, R8P, ERRLVL_warn, ERRLVL_fatal, HEAD0_alredf, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Zero selects a list; nonzero selects an indexed grid.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Open input unit positioned before the heading.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving warning/fatal diagnostics.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! First result extent or grid x extent.
      INTEGER(kind=I_P), INTENT(IN) :: N2 !! Second result extent or grid y extent.
      CHARACTER(LEN=*), INTENT(IN) :: LINE !! Expected case-sensitive heading substring.

      ! Output arguments
      REAL(kind=R8P), INTENT(OUT) :: FDATA(N1, N2) !! Real list or grid values read from `IUNIT`.

      ! Locals, etc
      INTEGER(kind=I_P) :: IY !! Expected grid row, processed from north to south.
      INTEGER(kind=I_P) :: KY !! Row number read from the current grid record.
      INTEGER(kind=I_P) :: IX !! Grid x index in the implied-DO input list.
      INTEGER(kind=I_P) :: ios !! I/O status from the latest read.
      CHARACTER(len=80) :: HEAD !! Heading record read from `IUNIT`.
      CHARACTER(len=132) :: MSG !! Warning/fatal message buffer.

      ! Code =================================================================

      READ (IUNIT, '(A)', IOSTAT=ios) HEAD

      IF (ios /= 0) THEN
         ! Title line read error
         WRITE (MSG, 9801) LINE, HEAD0_alredf
         CALL RAISE_ERROR(ERRLVL_fatal, 3, OUNIT, 0, 0, MSG)
         RETURN
      END IF

      IF (INDEX(HEAD, LINE) == 0) THEN
         WRITE (MSG, 9002) LINE, HEAD
         CALL RAISE_ERROR(ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
      END IF

      ! Read floating-point data
      ! ------------------------
      IF (FLAG == 0) THEN
         ! Simple array
         READ (IUNIT, *, IOSTAT=ios) FDATA

         IF (ios /= 0) THEN
            ! Real data error
            WRITE (MSG, 9810) 'floating-point', HEAD
            CALL RAISE_ERROR(ERRLVL_fatal, 7, OUNIT, 0, 0, MSG)
            RETURN
         END IF

      ELSE
         ! Grid-based array: read indexed rows, North to South
         DO IY = N2, 1, -1
            READ (IUNIT, *, IOSTAT=ios) KY, (FDATA(IX, IY), IX=1, N1)

            IF (ios /= 0 .OR. KY /= IY) THEN
               ! Real grid error (or index mismatch)
               WRITE (MSG, 9842) 'floating-point', IY, HEAD
               CALL RAISE_ERROR(ERRLVL_fatal, 11, OUNIT, 0, 0, MSG)
               RETURN
            END IF
         END DO
      END IF

      RETURN

      ! Format ---------------------------------------------------------------
      !
      ! Note: Take care not to exceed internal file length
      !
9002  FORMAT('Title line mismatch: expected "', A, '" but found "', A, '"')

9801  FORMAT('Reading heading: ', A, '; last item was: ', A)

9810  FORMAT('Reading ', A, ' data under heading: ', A)

9842  FORMAT('Reading ', A, ' grid (IY=', I4, ') under title: ', A)

   END SUBROUTINE ALREDF

   !> Reads a heading followed by an integer list or indexed integer grid.
   !>
   !> A heading containing `LINE` is expected first; a mismatch raises warning
   !> 2 and reading continues. `FLAG=0` reads `IDATA(N1,N2)` by list-directed
   !> input. A nonzero flag reads indexed rows from `N2` down to 1. For flags
   !> below ten it generates format `(I7,1X,N1 I1)`, matching the manual's
   !> compact integer-grid form; flags of ten or more use list-directed rows.
   !> Every row number must equal the expected `IY`.
   !>
   !> Heading, list, and grid failures are fatal errors 3, 6, and 10.
   !> `HEAD0_alredi`, used in the heading error, is never updated and always
   !> retains its initial `( nothing read yet )` text.
   !>
   !> @warning
   !> Nonzero negative flags also enter the compact-grid branch because the
   !> code tests only `FLAG < 10`; callers are expected to pass a positive
   !> category limit for grid input.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial integer input routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Created the separate integer reader during removal of the former `ENTRY` interface. |
   !> | 2026-04-06 | SvB | - | Replaced error jumps with `IOSTAT` checks and structured returns. |
   !> @endhistory
   SUBROUTINE ALREDI(FLAG, IUNIT, OUNIT, LINE, N1, N2, IDATA)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, ERRLVL_warn, ERRLVL_fatal, HEAD0_alredi, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Zero selects a list; nonzero selects a grid and also controls compact formatting.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Open input unit positioned before the heading.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving warning/fatal diagnostics.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! First result extent or grid x extent.
      INTEGER(kind=I_P), INTENT(IN) :: N2 !! Second result extent or grid y extent.
      CHARACTER(LEN=*), INTENT(IN) :: LINE !! Expected case-sensitive heading substring.

      ! Output arguments
      INTEGER(kind=I_P), INTENT(OUT) :: IDATA(N1, N2) !! Integer list or grid values read from `IUNIT`.

      ! Locals, etc
      INTEGER(kind=I_P) :: IY !! Expected grid row, processed from north to south.
      INTEGER(kind=I_P) :: KY !! Row number read from the current grid record.
      INTEGER(kind=I_P) :: IX !! Grid x index in the implied-DO input list.
      INTEGER(kind=I_P) :: ios !! I/O status from the latest read.
      CHARACTER(len=80) :: HEAD !! Heading record read from `IUNIT`.
      CHARACTER(len=17) :: FORM !! Generated compact `I1` grid format.
      CHARACTER(len=132) :: MSG !! Warning/fatal message buffer.

      ! Code -----------------------------------------------------------------

      READ (IUNIT, '(A)', IOSTAT=ios) HEAD

      IF (ios /= 0) THEN
         ! Title line read error
         WRITE (MSG, 9801) LINE, HEAD0_alredi
         CALL RAISE_ERROR(ERRLVL_fatal, 3, OUNIT, 0, 0, MSG)
         RETURN
      END IF

      IF (INDEX(HEAD, LINE) == 0) THEN
         WRITE (MSG, 9002) LINE, HEAD
         CALL RAISE_ERROR(ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
      END IF

      ! Read INTEGER(kind=I_P) data
      ! -----------------
      IF (FLAG == 0) THEN
         ! Simple array
         READ (IUNIT, *, IOSTAT=ios) IDATA

         IF (ios /= 0) THEN
            ! Integer data error
            WRITE (MSG, 9810) 'integer', HEAD
            CALL RAISE_ERROR(ERRLVL_fatal, 6, OUNIT, 0, 0, MSG)
            RETURN
         END IF

      ELSE
         ! Grid-based array: read indexed rows, North to South
         ! (using single digit integers if possible)
         IF (FLAG < 10) WRITE (FORM, 9410) N1

         DO IY = N2, 1, -1
            IF (FLAG < 10) THEN
               READ (IUNIT, FORM, IOSTAT=ios) KY, (IDATA(IX, IY), IX=1, N1)
            ELSE
               READ (IUNIT, *, IOSTAT=ios) KY, (IDATA(IX, IY), IX=1, N1)
            END IF

            IF (ios /= 0 .OR. KY /= IY) THEN
               ! Integer grid error
               WRITE (MSG, 9842) 'integer', IY, HEAD
               CALL RAISE_ERROR(ERRLVL_fatal, 10, OUNIT, 0, 0, MSG)
               RETURN
            END IF
         END DO
      END IF

      RETURN

      ! Format ---------------------------------------------------------------
      !
      ! Note: Take care not to exceed internal file length
      !
9002  FORMAT('Title line mismatch: expected "', A, '" but found "', A, '"')

9410  FORMAT('(I7,1X,', I4, 'I1)')

9801  FORMAT('Reading heading: ', A, '; last item was: ', A)

9810  FORMAT('Reading ', A, ' data under heading: ', A)

9842  FORMAT('Reading ', A, ' grid (IY=', I4, ') under title: ', A)

   END SUBROUTINE ALREDI

   !> Reads a heading followed by list-directed logical data.
   !>
   !> The routine checks the heading for the case-sensitive substring `LINE`,
   !> warning on a mismatch, then reads `LDATA(N1,N2)` by list-directed input.
   !> `FLAG` is accepted for interface compatibility but not used. Heading and
   !> logical-data failures invoke fatal errors 3 and 14.
   !>
   !> `HEAD0_alredl` is used by the heading-error message but never updated, so
   !> it retains the initial `( nothing read yet )` text.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial logical input routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Added the logical reader while replacing the former `ENTRY` interface with separate routines. |
   !> @endhistory
   SUBROUTINE ALREDL(FLAG, IUNIT, OUNIT, LINE, N1, N2, LDATA)

      ! Input arguments
      INTEGER(kind=I_P) :: FLAG !! Unused selector retained for interface consistency.
      INTEGER(kind=I_P) :: IUNIT !! Open input unit positioned before the heading.
      INTEGER(kind=I_P) :: OUNIT !! Unit receiving warning/fatal diagnostics.
      INTEGER(kind=I_P) :: N1 !! First extent of `LDATA`.
      INTEGER(kind=I_P) :: N2 !! Second extent of `LDATA`.
      CHARACTER(LEN=*) :: LINE !! Expected case-sensitive heading substring.

      ! Output arguments
      LOGICAL :: LDATA(N1, N2) !! Logical values read in Fortran array element order.
      CHARACTER(80) :: HEAD !! Heading record read from `IUNIT`.
      CHARACTER(132) :: MSG !! Warning/fatal message buffer.

      ! Code -----------------------------------------------------------------

      READ (IUNIT, '(A)', ERR=8010, END=8010) HEAD
      IF (INDEX(HEAD, LINE) == 0) THEN
         WRITE (MSG, 9002) LINE, HEAD
         CALL RAISE_ERROR(ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
      END IF

      ! Read logical data
      ! -----------------
      READ (IUNIT, *, ERR=8600, END=8600) LDATA

      RETURN

      ! Error ----------------------------------------------------------------

      ! Title line read error
8010  WRITE (MSG, 9801) LINE, HEAD0_ALREDL
      CALL RAISE_ERROR(ERRLVL_fatal, 3, OUNIT, 0, 0, MSG)

      ! Logical data error
8600  WRITE (MSG, 9810) 'logical', HEAD
      CALL RAISE_ERROR(ERRLVL_fatal, 14, OUNIT, 0, 0, MSG)

      ! Format ---------------------------------------------------------------
      !
      ! Note: Take care not to exceed internal file length
      !
      !
9002  FORMAT('Title line mismatch: expected "', A, &
             '" but found "', A, '"')

9801  FORMAT('Reading heading: ', A, '; last item was: ', A)

9810  FORMAT('Reading ', A, ' data under heading: ', A)

   END SUBROUTINE ALREDL

END MODULE record_readers

