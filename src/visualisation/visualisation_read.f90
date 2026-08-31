!> @brief Provides the stateful, typed visualisation-plan reader façade.
!>
!> This module adapts the status-returning, record-buffered lexer in
!> [[visualisation_read_parser]] to the legacy fatal API used by
!> [[visualisation_metadata]]. [[copy]] preprocesses and opens one plan, then
!> the `R_C`, `R_I`, and `R_R` generics consume its tokens from a single private
!> `visualisation_token_reader`. Public message buffers and
!> [[error_visualisation]] also provide the metadata layer's shared fatal-error
!> channel.
!>
!> | Public API | Current contract |
!> |:-----------|:-----------------|
!> | `COPY(dirqq,filename)` | Preprocesses `filename`, opens the fixed temporary on `vp_in`, and resets the reader. |
!> | `R_C(text,value)` | For a one-character result, consumes one non-space character; otherwise consumes one complete token. |
!> | `R_I` | Reads one integer, an explicit-size vector, or two to five scalar integers. |
!> | `R_R` | Reads one real, an explicit-size vector, or two to five scalar reals. |
!> | `error_visualisation` | Writes `mess`, `mess2`, and `mess3` to `vp_out` and the console, then executes `STOP`. |
!>
!> `COPY` applies this visualisation-plan grammar before tokenization:
!>
!> | Input feature | Handling |
!> |:--------------|:---------|
!> | Title record | Must equal case-sensitive `visualisation plan` after trimming and optional matching outer quotes. |
!> | `!` | Begins a comment; the delimiter and remaining characters are ignored. |
!> | `:` or `^` | Splits one source record into separate parser records. |
!> | Empty segments | Omitted after leading/trailing ASCII spaces are removed. |
!> | Significant record length | At most 500 characters before tokenization. |
!> | Character set | Printable ASCII 32:126 in uncommented content; a tab receives an additional diagnostic. |
!>
!> Tokens are nonempty sequences separated only by ASCII spaces and never span
!> records. Character results use normal Fortran truncation or blank padding.
!> Integers accept an optional sign followed by digits and must fit default
!> `INTEGER`. Reals accept a signed decimal mantissa and optional signed
!> `E`/`e`/`D`/`d` exponent; conversion must produce a finite default `REAL`.
!> Malformed tokens are consumed before the reader reports failure.
!>
!> The only production consumer is [[visualisation_metadata]]. The
!> `visualisation_read.unit` test suite exercises preprocessing, token progress,
!> EOF, character semantics, numeric grammar/range, and diagnostics;
!> `visualisation_read.examples` scans every selected example plan through the
!> production `COPY` path and requires a final `stop` token.
!>
!> @warning
!> The module is stateful and non-reentrant. `COPY` must precede every typed
!> read, and only one plan may be active. Fixed units 48 and 49, the single
!> reader, and the public message buffers cannot be shared concurrently or used
!> for an independent second parse without completing/replacing the first.
!> @endwarning
!>
!> @warning
!> Preprocessing replaces the fixed `<dirqq>/temporary.txt`, then opens it
!> `readwrite`; the metadata caller normally closes it with `STATUS='delete'`.
!> Fatal `STOP` paths do not guarantee deletion or other cleanup. `vp_out` must
!> already denote the intended check file when an error is reported, and each
!> public diagnostic buffer silently truncates beyond 100 characters.
!> @endwarning
!>
!> @note
!> The generic façade intentionally returns default-kind integers and reals.
!> The lower-level parser reports `VIS_READ_*` statuses without stopping, but
!> this compatibility module converts every non-success status into the fatal
!> shared diagnostic path.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1999-12 | JE | - | Created the legacy line-stripping utility for informative input text. |
!> | 2004-07 | JE | SHEGRAPH 2.0 | Created the visualisation-plan reader façade. |
!> | 2005-08-09 | NETT | - | Hardened blank-line trimming in the legacy stripper. |
!> | 2020-09-08 | SB | - | Imported the visualisation sources into the repository. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the Intel default-real directive during the IFX update. |
!> | 2026-04-14 | SvB | - | Exposed the renamed fatal error service used by GFortran-safe callers. |
!> | 2026-07-08 | SB | - | Corrected record-boundary failures in the non-advancing reader. |
!> | 2026-07-09 | SvB | - | Replaced non-advancing reads with the buffered lexer and added CMake regression suites. |
!> @endhistory
MODULE visualisation_read

   USE visualisation_read_parser, ONLY: visualisation_token_reader, transform_visualisation_record, &
                                        visualisation_title_matches, VIS_READ_OK, VIS_READ_END, &
                                        VIS_MAX_RECORD_LENGTH, VIS_RECORD_BUFFER_LENGTH

   IMPLICIT NONE

   INTEGER, PARAMETER :: vp_in  = 48 !! Unit for the stripped plan, opened `readwrite` by [[strip]].
   INTEGER, PARAMETER :: vp_out = 49 !! Caller-opened visualisation check/diagnostic unit.
   CHARACTER(100)     :: mess  = ''  !! Primary shared fatal diagnostic, truncated to 100 characters.
   CHARACTER(100)     :: mess2 = ''  !! Optional secondary shared fatal diagnostic.
   CHARACTER(100)     :: mess3 = ''  !! Optional tertiary shared fatal diagnostic.

   TYPE(visualisation_token_reader) :: reader !! Private cursor over the current `vp_in` stream.

   INTERFACE R_C; MODULE PROCEDURE R_C; END INTERFACE
   INTERFACE R_I; MODULE PROCEDURE R_I_0, R_I_1, R_I_M; END INTERFACE
   INTERFACE R_R; MODULE PROCEDURE R_R_0, R_R_1, R_R_M; END INTERFACE

   PRIVATE
   PUBLIC :: vp_in, vp_out, mess, mess2, mess3, error_visualisation, R_C, R_I, R_R, COPY

CONTAINS

!> @brief Preprocesses one visualisation plan and resets the shared token reader.
!>
!> The input filename is passed unchanged to [[strip]]. The catchment directory
!> is used only to locate the fixed `temporary.txt` output. On success,
!> `vp_in` is open at the start of that stripped file and the private reader is
!> reset to consume it.
!>
!> @warning
!> This replaces `<dirqq>/temporary.txt` and abandons any previous reader
!> position. It is therefore a one-plan-at-a-time operation and is not safe for
!> concurrent or nested parsing.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added the visualisation-plan entry point. |
!> | 2026-07-09 | SvB | - | Reset the new record-buffered reader after preprocessing. |
!> @endhistory
   SUBROUTINE copy(dirqq, filename)
      CHARACTER(*), INTENT(IN) :: dirqq   !! Directory in which [[strip]] replaces `temporary.txt`.
      CHARACTER(*), INTENT(IN) :: filename !! Input plan path, opened exactly as supplied.

      CALL strip(file=filename, u=vp_in, checktitle='visualisation plan', delimiter='!', &
                 separator=(/':', '^'/), dir=dirqq)
      CALL reader%reset(vp_in)
   END SUBROUTINE copy

!> @brief Reads the next character value through the `R_C` generic.
!>
!> `text` labels any fatal diagnostic; it is not matched against input. A
!> one-character result consumes the next non-space character and can therefore
!> advance within a token. A longer result consumes one complete token, with
!> normal Fortran truncation or blank padding. Tokens never span records.
!>
!> Any lexer status other than `VIS_READ_OK` is converted by [[parser_error]]
!> into the shared fatal-error path.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added character reads for visualisation metadata. |
!> | 2026-07-08 | SB | - | Corrected record-boundary handling in the non-advancing reader. |
!> | 2026-07-09 | SvB | - | Delegated character reads to the buffered token reader. |
!> @endhistory
   SUBROUTINE r_c(text, r)
      CHARACTER(*), INTENT(IN)  :: text !! Context label included in a read-failure diagnostic.
      CHARACTER(*), INTENT(OUT) :: r    !! Character or token value read from the active plan.

      INTEGER :: status                   !! `VIS_READ_*` status returned by the lexer.
      CHARACTER(:), ALLOCATABLE :: detail !! Lexer-supplied failure detail.

      CALL reader%read_text(r, status, detail)
      IF (status /= VIS_READ_OK) CALL parser_error(text, 'text', status, detail)
   END SUBROUTINE r_c

!> @brief Implements the validated scalar-integer read used by `R_I`.
!>
!> The buffered lexer accepts an optional leading sign followed by one or more
!> digits and checks conversion to default `INTEGER`. It consumes a malformed
!> token before reporting failure. `text` is diagnostic context only.
!>
!> @note
!> This private backend is reached by [[r_i_0]]; the public interface is the
!> `R_I` generic.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added scalar integer parsing. |
!> | 2026-07-08 | SB | - | Corrected record-boundary handling in the non-advancing reader. |
!> | 2026-07-09 | SvB | - | Replaced the eight-character legacy parser with validated buffered conversion. |
!> @endhistory
   SUBROUTINE r_ii(text, r)
      CHARACTER(*), INTENT(IN) :: text !! Context label included in a read-failure diagnostic.
      INTEGER, INTENT(OUT)     :: r    !! Default-kind integer read from the active plan.

      INTEGER :: status                   !! `VIS_READ_*` status returned by the lexer.
      CHARACTER(:), ALLOCATABLE :: detail !! Lexer-supplied failure detail.

      CALL reader%read_integer(r, status, detail)
      IF (status /= VIS_READ_OK) CALL parser_error(text, 'integer', status, detail)
   END SUBROUTINE r_ii

!> @brief Implements the validated scalar-real read used by `R_R`.
!>
!> The buffered lexer accepts a signed decimal mantissa and an optional signed
!> `E`, `e`, `D`, or `d` exponent. Conversion must produce a finite default
!> `REAL`. A malformed token is consumed before failure is reported, and
!> `text` is used only as diagnostic context.
!>
!> @note
!> This private backend is reached by [[r_r_0]]; the public interface is the
!> `R_R` generic.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added scalar real parsing. |
!> | 2026-07-08 | SB | - | Corrected record-boundary handling in the non-advancing reader. |
!> | 2026-07-09 | SvB | - | Replaced the 20-character legacy parser with validated buffered conversion. |
!> @endhistory
   SUBROUTINE r_rr(text, r)
      CHARACTER(*), INTENT(IN) :: text !! Context label included in a read-failure diagnostic.
      REAL, INTENT(OUT)        :: r    !! Finite default-kind real read from the active plan.

      INTEGER :: status                   !! `VIS_READ_*` status returned by the lexer.
      CHARACTER(:), ALLOCATABLE :: detail !! Lexer-supplied failure detail.

      CALL reader%read_real(r, status, detail)
      IF (status /= VIS_READ_OK) CALL parser_error(text, 'real', status, detail)
   END SUBROUTINE r_rr

!> @brief Supplies the scalar-integer overload of the `R_I` generic.
!>
!> This compatibility wrapper delegates the read and all validation to
!> [[r_ii]]. A failure enters the fatal visualisation-error path and does not
!> return normally.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added the scalar `R_I` overload. |
!> | 2026-07-09 | SvB | - | Retained the generic wrapper over the buffered scalar reader. |
!> @endhistory
   SUBROUTINE r_i_0(text, r)
      CHARACTER(*), INTENT(IN) :: text !! Context label passed to [[r_ii]].
      INTEGER, INTENT(OUT)     :: r    !! Scalar integer returned to the caller.

      CALL r_ii(text, r)
   END SUBROUTINE r_i_0

!> @brief Supplies the two-to-five-scalar integer overload of `R_I`.
!>
!> Values are consumed in argument order. The first two are mandatory; each
!> optional value consumes one additional token only when present. Every scalar
!> call uses the same diagnostic label.
!>
!> @warning
!> Reads are not transactional. If a later token is invalid or missing, earlier
!> output arguments have already been assigned before the fatal stop.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added the multiple-scalar `R_I` overload. |
!> | 2026-07-09 | SvB | - | Retained ordered generic reads over the buffered scalar parser. |
!> @endhistory
   SUBROUTINE r_i_m(text, i1, i2, i3, i4, i5)
      CHARACTER(*), INTENT(IN)       :: text !! Context label used for every scalar read.
      INTEGER, INTENT(OUT)           :: i1   !! First integer read.
      INTEGER, INTENT(OUT)           :: i2   !! Second integer read.
      INTEGER, INTENT(OUT), OPTIONAL :: i3   !! Optional third integer read.
      INTEGER, INTENT(OUT), OPTIONAL :: i4   !! Optional fourth integer read.
      INTEGER, INTENT(OUT), OPTIONAL :: i5   !! Optional fifth integer read.

      CALL R_I(text, i1)
      CALL R_I(text, i2)
      IF (PRESENT(i3)) CALL R_I(text, i3)
      IF (PRESENT(i4)) CALL R_I(text, i4)
      IF (PRESENT(i5)) CALL R_I(text, i5)
   END SUBROUTINE r_i_m

!> @brief Supplies the explicit-size integer-vector overload of `R_I`.
!>
!> Exactly `sz` sequential scalar reads fill `r(1:sz)`. A nonpositive effective
!> extent performs no reads.
!>
!> @warning
!> The operation is not transactional: a fatal error after the first element
!> leaves earlier elements assigned. The caller is responsible for a conforming
!> actual array and a meaningful size.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added the integer-vector `R_I` overload. |
!> | 2026-07-09 | SvB | - | Retained sequential generic reads over the buffered scalar parser. |
!> @endhistory
   SUBROUTINE r_i_1(text, sz, r)
      CHARACTER(*), INTENT(IN)           :: text !! Context label used for every element read.
      INTEGER, INTENT(IN)                :: sz   !! Declared result extent and number of tokens to consume.
      INTEGER, DIMENSION(sz), INTENT(OUT) :: r    !! Integer vector filled in index order.

      INTEGER :: i !! Result index.

      DO i = 1, sz
         CALL R_I(text, r(i))
      END DO
   END SUBROUTINE r_i_1

!> @brief Supplies the scalar-real overload of the `R_R` generic.
!>
!> This compatibility wrapper delegates the read and all validation to
!> [[r_rr]]. A failure enters the fatal visualisation-error path and does not
!> return normally.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added the scalar `R_R` overload. |
!> | 2026-07-09 | SvB | - | Retained the generic wrapper over the buffered scalar reader. |
!> @endhistory
   SUBROUTINE r_r_0(text, r)
      CHARACTER(*), INTENT(IN) :: text !! Context label passed to [[r_rr]].
      REAL, INTENT(OUT)        :: r    !! Scalar real returned to the caller.

      CALL r_rr(text, r)
   END SUBROUTINE r_r_0

!> @brief Supplies the two-to-five-scalar real overload of `R_R`.
!>
!> Values are consumed in argument order. The first two are mandatory; each
!> optional value consumes one additional token only when present. Every scalar
!> call uses the same diagnostic label.
!>
!> @warning
!> Reads are not transactional. If a later token is invalid or missing, earlier
!> output arguments have already been assigned before the fatal stop.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added the multiple-scalar `R_R` overload. |
!> | 2026-07-09 | SvB | - | Retained ordered generic reads over the buffered scalar parser. |
!> @endhistory
   SUBROUTINE r_r_m(text, r1, r2, r3, r4, r5)
      CHARACTER(*), INTENT(IN)    :: text !! Context label used for every scalar read.
      REAL, INTENT(OUT)           :: r1   !! First real read.
      REAL, INTENT(OUT)           :: r2   !! Second real read.
      REAL, INTENT(OUT), OPTIONAL :: r3   !! Optional third real read.
      REAL, INTENT(OUT), OPTIONAL :: r4   !! Optional fourth real read.
      REAL, INTENT(OUT), OPTIONAL :: r5   !! Optional fifth real read.

      CALL R_R(text, r1)
      CALL R_R(text, r2)
      IF (PRESENT(r3)) CALL R_R(text, r3)
      IF (PRESENT(r4)) CALL R_R(text, r4)
      IF (PRESENT(r5)) CALL R_R(text, r5)
   END SUBROUTINE r_r_m

!> @brief Supplies the explicit-size real-vector overload of `R_R`.
!>
!> Exactly `sz` sequential scalar reads fill `r(1:sz)`. A nonpositive effective
!> extent performs no reads.
!>
!> @warning
!> The operation is not transactional: a fatal error after the first element
!> leaves earlier elements assigned. The caller is responsible for a conforming
!> actual array and a meaningful size.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added the real-vector `R_R` overload. |
!> | 2026-07-09 | SvB | - | Retained sequential generic reads over the buffered scalar parser. |
!> @endhistory
   SUBROUTINE r_r_1(text, sz, r)
      CHARACTER(*), INTENT(IN)        :: text !! Context label used for every element read.
      INTEGER, INTENT(IN)             :: sz   !! Declared result extent and number of tokens to consume.
      REAL, DIMENSION(sz), INTENT(OUT) :: r    !! Real vector filled in index order.

      INTEGER :: i !! Result index.

      DO i = 1, sz
         CALL R_R(text, r(i))
      END DO
   END SUBROUTINE r_r_1

!> @brief Translates a lexer failure into the shared fatal diagnostic channel.
!>
!> All three public message buffers are cleared first. End-of-file receives a
!> dedicated “unexpected end” message; every other non-success status receives
!> the generic failed-read message and copies the parser's detail into `mess2`.
!> [[error_visualisation]] then reports the messages and stops.
!>
!> | Status class | `mess` | `mess2` |
!> |:-------------|:-------|:--------|
!> | `VIS_READ_END` | `<context> - unexpected end of file while reading <expected>` | blank |
!> | Any other failure | `<context> - failed to read <expected>` | Lexer detail, truncated by the shared buffer |
!>
!> @note
!> Invalid input, I/O failure, and any unrecognised non-success status share the
!> second branch; only end-of-file is distinguished here.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added status-to-diagnostic translation for the buffered lexer. |
!> @endhistory
   SUBROUTINE parser_error(context, expected, status, detail)
      CHARACTER(*), INTENT(IN) :: context  !! Caller-supplied field or operation label.
      CHARACTER(*), INTENT(IN) :: expected !! Human-readable expected value type.
      INTEGER, INTENT(IN)      :: status   !! Non-success `VIS_READ_*` status.
      CHARACTER(*), INTENT(IN) :: detail   !! Lexer detail used for non-EOF failures.

      mess = ''
      mess2 = ''
      mess3 = ''
      IF (status == VIS_READ_END) THEN
         WRITE (mess, '(A,A,A)') TRIM(context), ' - unexpected end of file while reading ', TRIM(expected)
      ELSE
         WRITE (mess, '(A,A,A)') TRIM(context), ' - failed to read ', TRIM(expected)
         mess2 = detail
      END IF
      CALL error_visualisation()
   END SUBROUTINE parser_error

!> @brief Reports the shared visualisation diagnostics and terminates execution.
!>
!> The check stream receives the banner followed by all three message records,
!> including blank secondary or tertiary records. The console receives the
!> banner and primary message, but prints `mess2` and `mess3` only when nonblank.
!>
!> @warning
!> `vp_out` must already be connected to the intended check file. This routine
!> does not clear message state, close units, or remove `temporary.txt`; it ends
!> with an unconditional `STOP`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Added the fatal visualisation error reporter. |
!> | 2026-04-14 | SvB | - | Renamed and exposed the service for GFortran-safe callers. |
!> | 2026-07-09 | SvB | - | Retained the fatal reporter for buffered-parser diagnostics. |
!> @endhistory
   SUBROUTINE error_visualisation()
      CHARACTER(27), PARAMETER :: mm = '*** VISUALISATION ERROR ***' !! File and console error banner.

      WRITE (vp_out, '(/A)') mm
      WRITE (vp_out, '(A)') TRIM(mess)
      WRITE (vp_out, '(A)') TRIM(mess2)
      WRITE (vp_out, '(A)') TRIM(mess3)
      PRINT '(/A)', mm
      PRINT '(A)', TRIM(mess)
      IF (mess2 /= '') PRINT '(A)', TRIM(mess2)
      IF (mess3 /= '') PRINT '(A)', TRIM(mess3)
      ERROR STOP
   END SUBROUTINE error_visualisation

!> @brief Validates and preprocesses a plan into a compact parser stream.
!>
!> The first physical record must match `checktitle`; it is validation metadata
!> and is not copied. Each later record is transformed independently, then its
!> nonempty segments are written as separate records to `temporary.txt`.
!>
!> | Stage | Current behavior |
!> |:------|:-----------------|
!> | Destination | Uses `<dir>/temporary.txt` when `dir` is present, otherwise `temporary.txt`; an existing file is replaced. |
!> | Unit preparation | Closes an already-open `u` with `STATUS='keep'`; internal source and output units use `NEWUNIT`. |
!> | Title | Requires the case-sensitive expected title after trimming and optional matching outer quotes. |
!> | Content | Removes the delimiter and comment tail, splits at either separator, trims ASCII spaces, and omits empty segments. |
!> | Validation | Limits significant uncommented content to 500 characters and accepts printable ASCII 32:126 only. |
!> | Result | Closes both internal units and reopens the temporary on `u` with `ACTION='readwrite'`. |
!>
!> Read, transformation, write, and open failures populate the shared
!> diagnostics and call [[error_visualisation]]. A tab is rejected as ASCII 9
!> and receives the additional replacement hint in `mess3`.
!>
!> @warning
!> The input `file` is opened exactly as supplied; `dir` affects only the
!> temporary path. That path is fixed, its 512-character storage can truncate a
!> longer name, and concurrent calls can collide. A failure after replacement
!> can leave a partial temporary file, while fatal cleanup is not guaranteed.
!> @endwarning
!>
!> @warning
!> This routine does not clear all shared message buffers before populating an
!> error branch. Under the intended first-error-then-`STOP` flow that is benign,
!> but externally preloaded or stale optional messages can also be reported.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1999-12 | JE | - | Created the line-stripping utility for informative input text. |
!> | 2004-07 | JE | SHEGRAPH 2.0 | Adapted stripping for visualisation plans. |
!> | 2005-08-09 | NETT | - | Hardened blank-line trimming in the legacy implementation. |
!> | 2026-07-09 | SvB | - | Replaced character-at-a-time stripping with complete-record transformation and checked I/O. |
!> @endhistory
   SUBROUTINE strip(file, u, checktitle, delimiter, separator, dir)
      INTEGER, INTENT(IN)                :: u          !! Caller-selected unit for the stripped stream.
      CHARACTER(*), INTENT(IN)           :: file       !! Source plan path, opened unchanged.
      CHARACTER(*), INTENT(IN)           :: checktitle !! Required title record.
      CHARACTER, INTENT(IN)              :: delimiter  !! Character beginning an ignored comment tail.
      CHARACTER, INTENT(IN)              :: separator(2) !! Characters that split a source record.
      CHARACTER(*), INTENT(IN), OPTIONAL :: dir        !! Optional directory for `temporary.txt`.

      INTEGER :: source_unit !! Runtime-selected source-plan unit.
      INTEGER :: output_unit !! Runtime-selected temporary-output unit.
      INTEGER :: ios         !! File I/O status.
      INTEGER :: status      !! Record-transformer `VIS_READ_*` status.
      INTEGER :: lineno      !! One-based physical source line number.
      INTEGER :: i           !! Index of a transformed output segment.
      CHARACTER(VIS_RECORD_BUFFER_LENGTH) :: record !! Complete source record buffer.
      CHARACTER(512) :: iomsg    !! I/O-library diagnostic text.
      CHARACTER(512) :: tempfile !! Resolved fixed temporary-file path.
      CHARACTER(:), ALLOCATABLE :: detail !! Record-transformer failure detail.
      CHARACTER(VIS_MAX_RECORD_LENGTH), ALLOCATABLE :: segments(:) !! Trimmed nonempty output records.
      LOGICAL :: opened !! Whether caller unit `u` is already connected.

      IF (PRESENT(dir)) THEN
         tempfile = TRIM(dir)//'/temporary.txt'
      ELSE
         tempfile = 'temporary.txt'
      END IF

      INQUIRE (UNIT=u, OPENED=opened)
      IF (opened) CLOSE (UNIT=u, STATUS='keep')

      iomsg = ''
      OPEN (NEWUNIT=source_unit, FILE=file, STATUS='old', ACTION='read', IOSTAT=ios, IOMSG=iomsg)
      IF (ios /= 0) THEN
         mess = 'failed to open '//TRIM(file)
         mess2 = TRIM(iomsg)
         CALL error_visualisation()
         RETURN
      END IF

      record = ''
      READ (source_unit, '(A)', IOSTAT=ios, IOMSG=iomsg) record
      IF (ios /= 0 .OR. .NOT. visualisation_title_matches(record, checktitle)) THEN
         mess = 'wrong key in '//TRIM(file)
         mess2 = 'Read '//TRIM(ADJUSTL(record))//' expecting '//TRIM(checktitle)
         IF (ios /= 0) mess3 = TRIM(iomsg)
         CLOSE (source_unit)
         CALL error_visualisation()
         RETURN
      END IF

      iomsg = ''
      OPEN (NEWUNIT=output_unit, FILE=TRIM(tempfile), STATUS='replace', ACTION='write', &
            IOSTAT=ios, IOMSG=iomsg)
      IF (ios /= 0) THEN
         mess = 'failed to create '//TRIM(tempfile)
         mess2 = TRIM(iomsg)
         CLOSE (source_unit)
         CALL error_visualisation()
         RETURN
      END IF

      lineno = 1
      DO
         record = ''
         iomsg = ''
         READ (source_unit, '(A)', IOSTAT=ios, IOMSG=iomsg) record
         IF (IS_IOSTAT_END(ios)) EXIT
         lineno = lineno + 1
         IF (ios /= 0) THEN
            WRITE (mess, '(A,I0,A)') 'failed to read line ', lineno, ' from '//TRIM(file)
            mess2 = TRIM(iomsg)
            CLOSE (source_unit)
            CLOSE (output_unit)
            CALL error_visualisation()
            RETURN
         END IF

         CALL transform_visualisation_record(record, delimiter, separator, segments, status, detail)
         IF (status /= VIS_READ_OK) THEN
            WRITE (mess, '(A,I0,A)') 'invalid input at line ', lineno, ' in '//TRIM(file)
            mess2 = detail
            IF (INDEX(detail, 'ASCII character 9 ') > 0) &
               mess3 = 'This is probably a tab character - remove or replace it with spaces'
            CLOSE (source_unit)
            CLOSE (output_unit)
            CALL error_visualisation()
            RETURN
         END IF

         DO i = 1, SIZE(segments)
            iomsg = ''
            WRITE (output_unit, '(A)', IOSTAT=ios, IOMSG=iomsg) TRIM(segments(i))
            IF (ios /= 0) THEN
               mess = 'failed to write '//TRIM(tempfile)
               mess2 = TRIM(iomsg)
               CLOSE (source_unit)
               CLOSE (output_unit)
               CALL error_visualisation()
               RETURN
            END IF
         END DO
      END DO

      CLOSE (source_unit)
      CLOSE (output_unit)

      iomsg = ''
      OPEN (UNIT=u, FILE=TRIM(tempfile), STATUS='old', ACTION='readwrite', IOSTAT=ios, IOMSG=iomsg)
      IF (ios /= 0) THEN
         mess = 'failed to open stripped visualisation plan '//TRIM(tempfile)
         mess2 = TRIM(iomsg)
         CALL error_visualisation()
      END IF
   END SUBROUTINE strip

END MODULE visualisation_read
