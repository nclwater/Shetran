!> @brief Provides the status-returning, record-buffered visualisation lexer.
!>
!> This module replaces the former non-advancing, character-at-a-time input
!> path with complete formatted-sequential record reads. A
!> [[visualisation_token_reader]] then advances through an in-memory cursor, so
!> token boundaries do not depend on processor-specific end-of-record handling.
!> [[visualisation_read]] is the only production consumer; the dedicated test
!> program also uses the public type and preprocessing helpers directly.
!>
!> A token is a nonempty sequence of characters separated only by ASCII space.
!> Tokens never span records. Consuming reader operations return an allocated
!> diagnostic string and one of these statuses instead of terminating:
!>
!> | Status | Value | Meaning |
!> |:-------|------:|:--------|
!> | `VIS_READ_OK` | 0 | The requested operation completed. |
!> | `VIS_READ_END` | 1 | No further non-space input exists; EOF is sticky until `reset`. |
!> | `VIS_READ_INVALID` | 2 | Reader state, record length, lexical syntax, conversion, or finiteness is invalid. |
!> | `VIS_READ_IO_ERROR` | 3 | External formatted I/O failed for a reason other than EOF. |
!>
!> The public surface has two layers:
!>
!> | API | Responsibility |
!> |:----|:---------------|
!> | `visualisation_token_reader` | Reads characters, tokens, numbers, or progress coordinates from a caller-owned unit. |
!> | [[visualisation_title_matches]] | Normalizes and checks the plan title record. |
!> | [[transform_visualisation_record]] | Removes comments, validates and splits content, and returns trimmed records. |
!>
!> `VIS_MAX_RECORD_LENGTH` is 500 significant characters. Complete input is
!> first read into the 1001-character `VIS_RECORD_BUFFER_LENGTH` look-ahead
!> buffer, then copied into the validated 500-character reader buffer.
!>
!> @warning
!> A reader does not own, inquire, open, rewind, close, or lock its unit. The
!> caller must call `reset` before reading and after any external positioning or
!> replacement, and must not independently consume the unit while buffered
!> state remains active. Values from consuming operations are meaningful only
!> with `VIS_READ_OK`.
!> @endwarning
!>
!> @warning
!> Direct reader use treats only ASCII space as whitespace and does not enforce
!> printable ASCII; normal production input gains that validation from
!> [[transform_visualisation_record]]. The 1001-character look-ahead detects
!> ordinary overlength records, but formatted input can discard content beyond
!> that buffer, so a far-tail nonblank character after a sufficiently blank
!> prefix is not guaranteed to be detected.
!> @endwarning
!>
!> @note
!> `visualisation_read.unit` covers reader initialization, record and EOF
!> progression, text modes, integer/real grammar and range, preprocessing, and
!> diagnostics. `visualisation_read.examples` scans selected production plans,
!> asserts monotonic progress and clean EOF, and requires `stop` as the final
!> token.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Replaced non-advancing reads with the buffered lexer/preprocessor; added façade integration and tests. |
!> @endhistory
MODULE visualisation_read_parser

   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END
   USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, LENGTH_LINELONG
   USE MOD_ERROR, ONLY: errstat_alloc, errstat_dealloc

   IMPLICIT NONE

   PRIVATE

   INTEGER, PARAMETER, PUBLIC :: VIS_READ_OK = 0 !! Successful operation.
   INTEGER, PARAMETER, PUBLIC :: VIS_READ_END = 1 !! Sticky end of non-space input.
   INTEGER, PARAMETER, PUBLIC :: VIS_READ_INVALID = 2 !! Invalid state, record, syntax, conversion, or value.
   INTEGER, PARAMETER, PUBLIC :: VIS_READ_IO_ERROR = 3 !! External I/O failure other than normal EOF.
   INTEGER, PARAMETER, PUBLIC :: VIS_MAX_RECORD_LENGTH = 500 !! Maximum significant record length.
   INTEGER, PARAMETER, PUBLIC :: VIS_RECORD_BUFFER_LENGTH = 2*VIS_MAX_RECORD_LENGTH + 1 !! Raw look-ahead extent.

!> @brief Holds the cursor and validated record for one caller-owned input unit.
!>
!> `reset` associates a unit and restores `(record_number,column)=(0,1)`. The
!> public type-bound operations then advance through records and tokens; private
!> components prevent callers from bypassing the state transitions. Malformed
!> numeric tokens are consumed before `VIS_READ_INVALID` is returned.
!>
!> @warning
!> Intrinsic assignment copies the unit number and cursor state but not the
!> external unit. Two copied readers must not independently consume the same
!> connected stream.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added the stateful buffered-reader type and its seven public bindings. |
!> @endhistory
   TYPE, PUBLIC :: visualisation_token_reader
      PRIVATE
      INTEGER :: unit = 0 !! Caller-owned external unit; valid only after `reset`.
      CHARACTER(VIS_MAX_RECORD_LENGTH) :: record = '' !! Current validated record.
      INTEGER :: cursor = 1 !! One-based column of the next unconsumed character.
      INTEGER :: record_length = 0 !! `LEN_TRIM(record)`; trailing spaces are insignificant.
      INTEGER :: record_number = 0 !! Successfully fetched physical records since `reset`.
      LOGICAL :: initialized = .FALSE. !! Whether `reset` has associated a unit.
      LOGICAL :: at_end = .FALSE. !! Sticky EOF flag preventing repeated external reads.
   CONTAINS
      PROCEDURE :: reset => token_reader_reset !! Associate a unit and restore initial state.
      PROCEDURE :: read_character => token_reader_read_character !! Consume one non-space character.
      PROCEDURE :: read_text => token_reader_read_text !! Consume one character or one token by result length.
      PROCEDURE :: read_integer => token_reader_read_integer !! Validate and convert a default integer.
      PROCEDURE :: read_real => token_reader_read_real !! Validate and convert a finite default real.
      PROCEDURE :: next_token => token_reader_next_token !! Return one complete token without conversion.
      PROCEDURE :: get_position => token_reader_get_position !! Inspect the current record/cursor coordinate.
   END TYPE visualisation_token_reader

   PUBLIC :: transform_visualisation_record, visualisation_title_matches

CONTAINS

!> @brief Associates a unit and restores the reader's initial state.
!>
!> The unit number is stored, buffered text is cleared, the cursor becomes one,
!> the fetched-record count becomes zero, initialization becomes true, and
!> sticky EOF is cleared. The next consuming operation reads the external unit's
!> current record.
!>
!> @warning
!> This routine deliberately performs no `INQUIRE`, `OPEN`, `REWIND`, or other
!> positioning. Passing a closed, unsuitable, or incorrectly positioned unit is
!> accepted here and diagnosed, if possible, by a later read.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added explicit association and complete buffered-state reset. |
!> @endhistory
   SUBROUTINE token_reader_reset(self, unit)
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self !! Reader whose association/state are replaced.
      INTEGER, INTENT(IN) :: unit !! Caller-owned open formatted-sequential unit at the desired position.

      self%unit = unit
      self%record = ''
      self%cursor = 1
      self%record_length = 0
      self%record_number = 0
      self%initialized = .TRUE.
      self%at_end = .FALSE.
   END SUBROUTINE token_reader_reset

!> @brief Fetches and validates the next complete external record.
!>
!> | Condition | State and result |
!> |:----------|:-----------------|
!> | Reader not reset | Returns `VIS_READ_INVALID` without external I/O. |
!> | Sticky EOF already set | Returns `VIS_READ_END` without another read. |
!> | New EOF | Sets sticky EOF, clears `record_length`, and returns `VIS_READ_END`. |
!> | Other read failure | Returns `VIS_READ_IO_ERROR` with the processor diagnostic. |
!> | Record over 500 significant characters | Counts the fetched record, resets the cursor, and returns `VIS_READ_INVALID`. |
!> | Valid record | Counts it, stores its `LEN_TRIM`, resets the cursor, copies the text, and returns `VIS_READ_OK`. |
!>
!> The 1001-character raw buffer provides look-ahead for normal accidental
!> overlength records without enlarging the persistent 500-character buffer.
!>
!> @warning
!> After the overlength-record branch, `record_length` can exceed the stored
!> record bound and the prior `record` contents have not been replaced. Do not
!> retry this reader after that `VIS_READ_INVALID`; reset or abandon it. A retry
!> can observe stale text or exceed bounds when bounds checking is enabled.
!> @endwarning
!>
!> @warning
!> Formatted input discards columns beyond the 1001-character raw variable. A
!> record whose first 1001 columns trim to at most 500 can therefore hide later
!> non-space content from this length check.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added complete-record input, sticky EOF, look-ahead length checking, and status diagnostics. |
!> @endhistory
   SUBROUTINE load_record(self, status, message)
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self !! Reader buffer and lifecycle state to update.
      INTEGER, INTENT(OUT) :: status !! `VIS_READ_OK`, `VIS_READ_END`, `VIS_READ_INVALID`, or `VIS_READ_IO_ERROR`.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: message !! Allocated diagnostic; empty on success/normal EOF.

      INTEGER :: ios !! External read status.
      CHARACTER(256) :: iomsg !! Processor-supplied external I/O diagnostic.
      CHARACTER(VIS_RECORD_BUFFER_LENGTH) :: raw_record !! Doubled look-ahead record.
      CHARACTER(LENGTH_LINELONG) :: detail !! Formatted module diagnostic.

      message = ''
      IF (.NOT. self%initialized) THEN
         status = VIS_READ_INVALID
         message = 'visualisation token reader has not been initialized'
         RETURN
      END IF

      IF (self%at_end) THEN
         status = VIS_READ_END
         RETURN
      END IF

      raw_record = ''
      iomsg = ''
      READ (self%unit, '(A)', IOSTAT=ios, IOMSG=iomsg) raw_record

      IF (ios == IOSTAT_END) THEN
         self%at_end = .TRUE.
         self%record_length = 0
         status = VIS_READ_END
         RETURN
      ELSE IF (ios /= 0) THEN
         status = VIS_READ_IO_ERROR
         message = 'failed to read visualisation record: '//TRIM(iomsg)
         RETURN
      END IF

      self%record_number = self%record_number + 1
      self%record_length = LEN_TRIM(raw_record)
      self%cursor = 1

      IF (self%record_length > VIS_MAX_RECORD_LENGTH) THEN
         status = VIS_READ_INVALID
         WRITE (detail, '(A,I0,A,I0)') 'visualisation record ', self%record_number, &
            ' exceeds maximum length ', VIS_MAX_RECORD_LENGTH
         message = TRIM(detail)
         RETURN
      END IF

      self%record = ''
      IF (self%record_length > 0) self%record(:self%record_length) = raw_record(:self%record_length)
      status = VIS_READ_OK
   END SUBROUTINE load_record

!> @brief Advances to the next non-space character, crossing blank records.
!>
!> The routine iteratively loads records whenever the cursor lies beyond the
!> current significant length, then advances across ASCII spaces. Empty and
!> all-space records still increment the reader's record count. It returns at
!> the first other character or propagates the status from [[load_record]].
!>
!> @note
!> Tabs and other characters are not whitespace here. Production plans reject
!> them during preprocessing, but a directly associated raw stream does not.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added iterative space and blank-record skipping over the buffered stream. |
!> @endhistory
   SUBROUTINE skip_blanks(self, status, message)
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self !! Reader whose cursor/record may advance.
      INTEGER, INTENT(OUT) :: status !! `VIS_READ_OK` or status propagated by [[load_record]].
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: message !! Allocated diagnostic; empty on success/normal EOF.

      message = ''
      DO
         IF (self%cursor > self%record_length) THEN
            CALL load_record(self, status, message)
            IF (status /= VIS_READ_OK) RETURN
         END IF

         DO WHILE (self%cursor <= self%record_length)
            IF (self%record(self%cursor:self%cursor) /= ' ') THEN
               status = VIS_READ_OK
               RETURN
            END IF
            self%cursor = self%cursor + 1
         END DO
      END DO
   END SUBROUTINE skip_blanks

!> @brief Consumes and returns the next non-space character.
!>
!> [[skip_blanks]] first crosses spaces and empty records. On success, exactly
!> one character is copied and the cursor advances by one column, preserving
!> any remaining characters in the same token. This mode supports compact mask
!> strings such as `.==1` and long runs of `=`.
!>
!> `value` is initialized to blank before reading. On failure, `status` and
!> `message` come from the positioning path. That path may already have crossed
!> spaces or blank records, but it consumes no non-space result character.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added buffered single-character reads for visualisation masks. |
!> @endhistory
   SUBROUTINE token_reader_read_character(self, value, status, message)
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self !! Reader to consume.
      CHARACTER, INTENT(OUT) :: value !! Character read; blank when no value is returned.
      INTEGER, INTENT(OUT) :: status !! Reader status.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: message !! Allocated reader diagnostic.

      value = ''
      CALL skip_blanks(self, status, message)
      IF (status /= VIS_READ_OK) RETURN

      value = self%record(self%cursor:self%cursor)
      self%cursor = self%cursor + 1
   END SUBROUTINE token_reader_read_character

!> @brief Returns one complete ASCII-space-delimited token without conversion.
!>
!> Leading spaces and blank records are skipped. The token extends from the
!> first non-space character to the next space or the significant end of that
!> record; it never crosses a record boundary. The allocatable result has the
!> token's exact length.
!>
!> After success, the cursor is on the terminating space or one column beyond
!> the record. The next operation skips that space or loads another record. On
!> failure, `token` is allocated with zero length.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added record-contained token extraction and exact-length results. |
!> @endhistory
   SUBROUTINE token_reader_next_token(self, token, status, message)
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self !! Reader to consume.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: token !! Exact token, or allocated empty text on failure.
      INTEGER, INTENT(OUT) :: status !! Reader status.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: message !! Allocated reader diagnostic.

      INTEGER :: first !! First token column.
      INTEGER :: last  !! First space after the token, or `record_length+1`.

      token = ''
      CALL skip_blanks(self, status, message)
      IF (status /= VIS_READ_OK) RETURN

      first = self%cursor
      last = first
      DO WHILE (last <= self%record_length)
         IF (self%record(last:last) == ' ') EXIT
         last = last + 1
      END DO

      token = self%record(first:last - 1)
      self%cursor = last
   END SUBROUTINE token_reader_next_token

!> @brief Implements the two compatibility text modes required by `R_C`.
!>
!> | Destination length | Operation |
!> |-------------------:|:----------|
!> | 1 | Calls `read_character`, consuming one non-space character within a token. |
!> | Any other length | Calls `next_token`, then assigns with ordinary truncation or blank padding. |
!>
!> Unlike the legacy list-directed remainder read, token mode leaves following
!> tokens on the same record available. `value` is blanked before dispatch.
!>
!> @warning
!> A zero-length destination selects token mode: one complete token is consumed
!> successfully even though no characters can be returned.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added length-selected character/token compatibility semantics. |
!> @endhistory
   SUBROUTINE token_reader_read_text(self, value, status, message)
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self !! Reader to consume.
      CHARACTER(*), INTENT(OUT) :: value !! Blank-initialized fixed-length destination.
      INTEGER, INTENT(OUT) :: status !! Reader status.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: message !! Allocated reader diagnostic.

      CHARACTER(:), ALLOCATABLE :: token !! Exact token used by non-unit-length mode.

      value = ''
      IF (LEN(value) == 1) THEN
         CALL self%read_character(value, status, message)
      ELSE
         CALL self%next_token(token, status, message)
         IF (status == VIS_READ_OK) value = token
      END IF
   END SUBROUTINE token_reader_read_text

!> @brief Validates and converts one token to default `INTEGER`.
!>
!> The lexical grammar is `[+|-]digit...`; a sign alone, decimal point,
!> exponent, or other character within the token is rejected. ASCII space ends
!> one token and therefore begins separation from the next.
!> List-directed internal conversion with `IOSTAT`/`IOMSG` then enforces the
!> processor's default-integer range.
!>
!> The token is consumed before either syntax or range failure is returned, so
!> parsing can continue at the following token. Diagnostics identify the token
!> and fetched record. `value` is initialized to zero, but callers must use it
!> only with `VIS_READ_OK` because failed conversion need not preserve that zero.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added whole-token integer grammar, checked conversion, and progress-preserving failures. |
!> @endhistory
   SUBROUTINE token_reader_read_integer(self, value, status, message)
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self !! Reader to consume.
      INTEGER, INTENT(OUT) :: value !! Converted default integer; meaningful only on success.
      INTEGER, INTENT(OUT) :: status !! Reader status or `VIS_READ_INVALID` for syntax/range.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: message !! Allocated token/record diagnostic.

      INTEGER :: ios !! Internal conversion status.
      CHARACTER(256) :: iomsg !! Processor-supplied conversion diagnostic.
      CHARACTER(LENGTH_LINELONG) :: detail !! Formatted module diagnostic.
      CHARACTER(:), ALLOCATABLE :: token !! Exact consumed token.

      value = 0
      CALL self%next_token(token, status, message)
      IF (status /= VIS_READ_OK) RETURN

      IF (.NOT. is_integer_token(token)) THEN
         status = VIS_READ_INVALID
         WRITE (detail, '(A,A,A,I0)') 'expected integer, read "', token, &
            '" at record ', self%record_number
         message = TRIM(detail)
         RETURN
      END IF

      iomsg = ''
      READ (token, *, IOSTAT=ios, IOMSG=iomsg) value
      IF (ios /= 0) THEN
         status = VIS_READ_INVALID
         WRITE (detail, '(A,A,A,I0,A,A)') 'cannot convert integer "', token, &
            '" at record ', self%record_number, ': ', TRIM(iomsg)
         message = TRIM(detail)
      END IF
   END SUBROUTINE token_reader_read_integer

!> @brief Validates and converts one token to a finite default `REAL`.
!>
!> A signed or unsigned decimal mantissa must contain at least one digit; the
!> decimal point and an `E`, `e`, `D`, or `d` exponent are optional. An exponent
!> may have a sign but must contain at least one digit. Integer-form reals,
!> leading/trailing decimal points, and signed exponents are therefore valid;
!> `NaN`, infinities, malformed syntax, conversion failures, overflow, and any
!> non-finite result are rejected.
!>
!> The token is consumed before failure, allowing progress to the next token.
!> Diagnostics identify its text and fetched record. `value` is initialized to
!> zero, but it is meaningful only with `VIS_READ_OK`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added decimal/exponent grammar, checked conversion, IEEE finiteness, and consumptive failures. |
!> @endhistory
   SUBROUTINE token_reader_read_real(self, value, status, message)
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self !! Reader to consume.
      REAL, INTENT(OUT) :: value !! Converted finite default real; meaningful only on success.
      INTEGER, INTENT(OUT) :: status !! Reader status or `VIS_READ_INVALID` for syntax/value.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: message !! Allocated token/record diagnostic.

      INTEGER :: ios !! Internal conversion status.
      CHARACTER(256) :: iomsg !! Processor-supplied conversion diagnostic.
      CHARACTER(LENGTH_LINELONG) :: detail !! Formatted module diagnostic.
      CHARACTER(:), ALLOCATABLE :: token !! Exact consumed token.

      value = 0.0
      CALL self%next_token(token, status, message)
      IF (status /= VIS_READ_OK) RETURN

      IF (.NOT. is_real_token(token)) THEN
         status = VIS_READ_INVALID
         WRITE (detail, '(A,A,A,I0)') 'expected real, read "', token, &
            '" at record ', self%record_number
         message = TRIM(detail)
         RETURN
      END IF

      iomsg = ''
      READ (token, *, IOSTAT=ios, IOMSG=iomsg) value
      IF (ios /= 0) THEN
         status = VIS_READ_INVALID
         WRITE (detail, '(A,A,A,I0,A,A)') 'cannot convert real "', token, &
            '" at record ', self%record_number, ': ', TRIM(iomsg)
         message = TRIM(detail)
      ELSE IF (.NOT. IEEE_IS_FINITE(value)) THEN
         status = VIS_READ_INVALID
         WRITE (detail, '(A,A,A,I0)') 'real value is not finite "', token, &
            '" at record ', self%record_number
         message = TRIM(detail)
      END IF
   END SUBROUTINE token_reader_read_real

!> @brief Returns a stable progress coordinate without modifying reader state.
!>
!> `record_number` counts successfully fetched physical records, including
!> empty records; `column` is the next cursor position within the current
!> significant record. The coordinate is `(0,1)` after `reset`. It advances
!> within a record or when another record is fetched, which supports diagnostics
!> and the example suite's monotonic-progress assertion.
!>
!> @warning
!> This is not an external file position and cannot be passed to a positioning
!> statement. The EOF read does not increment `record_number`, and repeated
!> sticky EOF leaves the coordinate unchanged; the initial EOF attempt may first
!> advance `column` across remaining spaces. An overlength fetched record
!> increments `record_number` before returning invalid status.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added non-mutating record/cursor progress reporting. |
!> @endhistory
   SUBROUTINE token_reader_get_position(self, record_number, column)
      CLASS(visualisation_token_reader), INTENT(IN) :: self !! Reader state to inspect.
      INTEGER, INTENT(OUT) :: record_number !! Count of successfully fetched physical records.
      INTEGER, INTENT(OUT) :: column !! Next buffered cursor column.

      record_number = self%record_number
      column = self%cursor
   END SUBROUTINE token_reader_get_position

!> @brief Tests whether an entire token matches the integer lexical grammar.
!>
!> The accepted form is an optional leading `+` or `-` followed by one or more
!> ASCII decimal digits. Empty text, a sign alone, and every non-digit elsewhere
!> are false. Because the full declared length is inspected, embedded or
!> trailing blanks are invalid.
!>
!> @note
!> This pure private helper checks syntax only; [[token_reader_read_integer]]
!> performs range conversion separately.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added whole-token signed-integer syntax validation. |
!> @endhistory
   PURE LOGICAL FUNCTION is_integer_token(token) RESULT(valid)
      CHARACTER(*), INTENT(IN) :: token !! Exact text to validate, including declared trailing blanks.
      INTEGER :: i     !! Current digit position.
      INTEGER :: first !! First required digit after any sign.

      valid = .FALSE.
      IF (LEN(token) == 0) RETURN

      first = 1
      IF (token(1:1) == '+' .OR. token(1:1) == '-') first = 2
      IF (first > LEN(token)) RETURN

      DO i = first, LEN(token)
         IF (.NOT. is_digit(token(i:i))) RETURN
      END DO
      valid = .TRUE.
   END FUNCTION is_integer_token

!> @brief Tests whether an entire token matches the supported real grammar.
!>
!> | Part | Rule |
!> |:-----|:-----|
!> | Mantissa sign | Optional leading `+` or `-`. |
!> | Mantissa digits | At least one digit in total before or after the optional decimal point. |
!> | Exponent marker | Optional `E`, `e`, `D`, or `d`. |
!> | Exponent | Optional sign followed by at least one digit when a marker exists. |
!> | Remaining text | None; the entire declared token must match. |
!>
!> @note
!> This pure private helper validates syntax only. Conversion range and IEEE
!> finiteness are checked by [[token_reader_read_real]].
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added complete decimal and E/D-exponent syntax validation. |
!> @endhistory
   PURE LOGICAL FUNCTION is_real_token(token) RESULT(valid)
      CHARACTER(*), INTENT(IN) :: token !! Exact text to validate.
      INTEGER :: i               !! Current token position.
      INTEGER :: digits_before   !! Mantissa digits before the decimal point.
      INTEGER :: digits_after    !! Mantissa digits after the decimal point.
      INTEGER :: exponent_digits !! Digits following any exponent marker/sign.

      valid = .FALSE.
      IF (LEN(token) == 0) RETURN

      i = 1
      IF (token(i:i) == '+' .OR. token(i:i) == '-') i = i + 1
      IF (i > LEN(token)) RETURN

      digits_before = 0
      DO WHILE (i <= LEN(token))
         IF (.NOT. is_digit(token(i:i))) EXIT
         digits_before = digits_before + 1
         i = i + 1
      END DO

      digits_after = 0
      IF (i <= LEN(token)) THEN
         IF (token(i:i) == '.') THEN
            i = i + 1
            DO WHILE (i <= LEN(token))
               IF (.NOT. is_digit(token(i:i))) EXIT
               digits_after = digits_after + 1
               i = i + 1
            END DO
         END IF
      END IF
      IF (digits_before + digits_after == 0) RETURN

      IF (i <= LEN(token)) THEN
         IF (INDEX('EeDd', token(i:i)) == 0) RETURN
         i = i + 1
         IF (i <= LEN(token)) THEN
            IF (token(i:i) == '+' .OR. token(i:i) == '-') i = i + 1
         END IF

         exponent_digits = 0
         DO WHILE (i <= LEN(token))
            IF (.NOT. is_digit(token(i:i))) RETURN
            exponent_digits = exponent_digits + 1
            i = i + 1
         END DO
         IF (exponent_digits == 0) RETURN
      END IF

      valid = i > LEN(token)
   END FUNCTION is_real_token

!> @brief Returns true for a default-character decimal digit from `0` to `9`.
!>
!> The parser's production preprocessing accepts printable ASCII, so the
!> processor character comparisons implement the intended ASCII digit class on
!> supported builds.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added the shared pure digit classifier. |
!> @endhistory
   PURE LOGICAL FUNCTION is_digit(character) RESULT(valid)
      CHARACTER, INTENT(IN) :: character !! Single default character to classify.

      valid = character >= '0' .AND. character <= '9'
   END FUNCTION is_digit

!> @brief Normalizes and compares a visualisation-plan title record.
!>
!> Leading and trailing spaces around `record` are removed. If the resulting
!> text begins and ends with the same single-quote or double-quote character,
!> exactly that one outer pair is removed. The result is compared
!> case-sensitively with `expected` using ordinary blank-padded Fortran
!> character equality.
!>
!> @note
!> Outer whitespace and one matching quote pair are syntax; quote characters
!> inside the title are not interpreted or escaped. Because equality is
!> blank-padded, trailing spaces in either operand are insignificant, while
!> leading or internal spaces remain significant.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added quoted/unquoted, case-sensitive plan-title matching. |
!> @endhistory
   PURE LOGICAL FUNCTION visualisation_title_matches(record, expected) RESULT(matches)
      CHARACTER(*), INTENT(IN) :: record   !! Source title record to normalize.
      CHARACTER(*), INTENT(IN) :: expected !! Expected unquoted title.
      CHARACTER(:), ALLOCATABLE :: title !! Allocated normalized source title.
      INTEGER :: length !! Normalized source length before quote removal.

      title = TRIM(ADJUSTL(record))
      length = LEN(title)
      IF (length >= 2) THEN
         IF ((title(1:1) == "'" .AND. title(length:length) == "'") .OR. &
            (title(1:1) == '"' .AND. title(length:length) == '"')) THEN
            title = title(2:length - 1)
         END IF
      END IF
      matches = title == expected
   END FUNCTION visualisation_title_matches

!> @brief Validates and splits one source visualisation-plan record.
!>
!> Processing is deterministic and has no external I/O:
!>
!> | Order | Transformation |
!> |------:|:---------------|
!> | 1 | Ignore the first `delimiter` and everything following it. |
!> | 2 | Reject uncommented content longer than `VIS_MAX_RECORD_LENGTH`. |
!> | 3 | Reject uncommented characters outside printable ASCII 32:126. |
!> | 4 | Split at either member of `separators`. |
!> | 5 | Left-adjust and trim each part, omit empty parts, and preserve order. |
!>
!> `segments` is always allocated: initially with size zero, and on success
!> reallocated to the exact emitted count. Comment-only, blank, or
!> separator-only input succeeds with zero segments. Invalid length or character
!> input leaves the zero-sized result and returns an allocated diagnostic.
!>
!> @note
!> Comment removal precedes both validation steps, so nonprintable or overlength
!> text after the delimiter is ignored. Physical trailing spaces are removed by
!> `LEN_TRIM`; spaces immediately before a delimiter remain part of the length
!> check even though segment trimming would later discard them.
!> @endnote
!>
!> @note
!> Delimiters and separators have no escape syntax. If a delimiter also appears
!> in `separators`, its first occurrence starts the comment before splitting.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-07-09 | SvB | - | Added record preprocessing with comments, validation, splitting, and exact allocation. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE transform_visualisation_record(record, delimiter, separators, segments, status, message)
      CHARACTER(*), INTENT(IN) :: record !! Source record; physical trailing spaces are insignificant.
      CHARACTER, INTENT(IN) :: delimiter !! First character beginning an ignored comment tail.
      CHARACTER, INTENT(IN) :: separators(2) !! Two record-splitting characters.
      CHARACTER(VIS_MAX_RECORD_LENGTH), ALLOCATABLE, INTENT(OUT) :: segments(:) !! Exact trimmed segment array.
      INTEGER, INTENT(OUT) :: status !! `VIS_READ_OK` or `VIS_READ_INVALID`.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: message !! Allocated diagnostic; empty on success.

      INTEGER :: content_length     !! Un-commented extent checked and processed.
      INTEGER :: delimiter_position !! First delimiter column, or zero.
      INTEGER :: first              !! First column of the current candidate segment.
      INTEGER :: last               !! Last column of the current candidate segment.
      INTEGER :: count              !! First-pass segment count, then second-pass output index.
      INTEGER :: i                  !! Source-column loop index.
      INTEGER :: code               !! `IACHAR` code of the current content character.
      CHARACTER(VIS_MAX_RECORD_LENGTH) :: content !! Validated uncommented content buffer.
      CHARACTER(LENGTH_LINELONG) :: detail !! Formatted module diagnostic.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = 'visualisation_read_parser:transform_visualisation_record'

      status = VIS_READ_OK
      message = ''
      ALLOCATE (segments(0), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "segments", location, emsg)

      content_length = LEN_TRIM(record)
      IF (content_length > 0) THEN
         delimiter_position = INDEX(record(:content_length), delimiter)
         IF (delimiter_position > 0) content_length = delimiter_position - 1
      END IF
      IF (content_length == 0) RETURN

      IF (content_length > VIS_MAX_RECORD_LENGTH) THEN
         status = VIS_READ_INVALID
         WRITE (detail, '(A,I0)') 'input record exceeds maximum length ', VIS_MAX_RECORD_LENGTH
         message = TRIM(detail)
         RETURN
      END IF

      DO i = 1, content_length
         code = IACHAR(record(i:i))
         IF (code < 32 .OR. code > 126) THEN
            status = VIS_READ_INVALID
            WRITE (detail, '(A,I0,A,I0)') 'ASCII character ', code, ' at column ', i
            message = TRIM(detail)
            RETURN
         END IF
      END DO

      content = ''
      content(:content_length) = record(:content_length)

      count = 0
      first = 1
      DO i = 1, content_length + 1
         IF (i > content_length) THEN
            last = i - 1
            IF (LEN_TRIM(ADJUSTL(content(first:last))) > 0) count = count + 1
            first = i + 1
         ELSE IF (ANY(content(i:i) == separators)) THEN
            last = i - 1
            IF (LEN_TRIM(ADJUSTL(content(first:last))) > 0) count = count + 1
            first = i + 1
         END IF
      END DO

      DEALLOCATE (segments, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "segments", location, emsg)
      ALLOCATE (segments(count), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "segments", location, emsg)
      count = 0
      first = 1
      DO i = 1, content_length + 1
         IF (i > content_length) THEN
            last = i - 1
            IF (LEN_TRIM(ADJUSTL(content(first:last))) > 0) THEN
               count = count + 1
               segments(count) = TRIM(ADJUSTL(content(first:last)))
            END IF
            first = i + 1
         ELSE IF (ANY(content(i:i) == separators)) THEN
            last = i - 1
            IF (LEN_TRIM(ADJUSTL(content(first:last))) > 0) THEN
               count = count + 1
               segments(count) = TRIM(ADJUSTL(content(first:last)))
            END IF
            first = i + 1
         END IF
      END DO
   END SUBROUTINE transform_visualisation_record

END MODULE visualisation_read_parser
