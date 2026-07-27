MODULE visualisation_read_parser
   !! Record-buffered lexical reader for preprocessed visualisation plans.
   !!
   !! External formatted input is performed only as complete sequential
   !! records. Tokens are then consumed from an in-memory cursor, avoiding
   !! processor-dependent end-of-record handling associated with
   !! non-advancing input.
   !!
   !! A token is a non-empty sequence of non-blank characters contained in
   !! one record. Only the ASCII space character is treated as lexical
   !! whitespace. Tokens never span records. Public reader operations report
   !! one of the `VIS_READ_*` status constants and do not terminate execution.
   !!
   !! `transform_visualisation_record` implements the preprocessing grammar
   !! used before tokenization: comment removal, printable ASCII validation,
   !! separator splitting, trimming, and omission of empty segments.

   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END
   USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
   USE mod_parameters, ONLY: LENGTH_LINELONG

   IMPLICIT NONE

   PRIVATE

   INTEGER, PARAMETER, PUBLIC :: VIS_READ_OK = 0
      !! Operation completed successfully.
   INTEGER, PARAMETER, PUBLIC :: VIS_READ_END = 1
      !! No further non-blank input is available; repeated reads remain at EOF.
   INTEGER, PARAMETER, PUBLIC :: VIS_READ_INVALID = 2
      !! Reader state, record content, token syntax, or conversion is invalid.
   INTEGER, PARAMETER, PUBLIC :: VIS_READ_IO_ERROR = 3
      !! External formatted I/O failed for a reason other than normal EOF.
   INTEGER, PARAMETER, PUBLIC :: VIS_MAX_RECORD_LENGTH = 500
      !! Maximum significant record length, excluding trailing blanks.
   INTEGER, PARAMETER, PUBLIC :: VIS_RECORD_BUFFER_LENGTH = 2 * VIS_MAX_RECORD_LENGTH + 1
      !! Raw input buffer length used to detect overlong records before truncation.

   TYPE, PUBLIC :: visualisation_token_reader
      !! Stateful reader over one caller-owned formatted sequential unit.
      !!
      !! Call `reset` before any read operation and whenever the unit is
      !! rewound, repositioned, closed, or replaced. The type neither opens
      !! nor closes the unit. A successful operation advances monotonically;
      !! malformed numeric tokens are consumed before `VIS_READ_INVALID` is
      !! returned.
      PRIVATE
      INTEGER :: unit = 0
         !! Caller-owned external unit; valid only after `reset`.
      CHARACTER(VIS_MAX_RECORD_LENGTH) :: record = ''
         !! Current record after maximum-length validation.
      INTEGER :: cursor = 1
         !! One-based column of the next unconsumed character.
      INTEGER :: record_length = 0
         !! `LEN_TRIM` of `record`; therefore trailing blanks are insignificant.
      INTEGER :: record_number = 0
         !! Number of successfully fetched records since the last `reset`.
      LOGICAL :: initialized = .FALSE.
         !! True after a unit has been associated by `reset`.
      LOGICAL :: at_end = .FALSE.
         !! Sticky EOF state; prevents further external reads after EOF.
   CONTAINS
      PROCEDURE :: reset => token_reader_reset
         !! Associate a unit and restore the initial reader state.
      PROCEDURE :: read_character => token_reader_read_character
         !! Return one non-blank character, preserving intra-token position.
      PROCEDURE :: read_text => token_reader_read_text
         !! Read one character or one complete token according to result length.
      PROCEDURE :: read_integer => token_reader_read_integer
         !! Validate and convert one default-integer token.
      PROCEDURE :: read_real => token_reader_read_real
         !! Validate and convert one finite default-real token.
      PROCEDURE :: next_token => token_reader_next_token
         !! Return the next complete token without type conversion.
      PROCEDURE :: get_position => token_reader_get_position
         !! Return the current logical record and cursor position.
   END TYPE visualisation_token_reader

   PUBLIC :: transform_visualisation_record, visualisation_title_matches

CONTAINS

   SUBROUTINE token_reader_reset(self, unit)
      !! Associate `self` with `unit` and discard all buffered state.
      !!
      !! The routine does not inquire, open, rewind, or otherwise position the
      !! unit. The next read fetches the unit's current external record.
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self
         !! Reader to initialize.
      INTEGER, INTENT(IN)                             :: unit
         !! Open formatted sequential unit positioned by the caller.

      self%unit = unit
      self%record = ''
      self%cursor = 1
      self%record_length = 0
      self%record_number = 0
      self%initialized = .TRUE.
      self%at_end = .FALSE.
   END SUBROUTINE token_reader_reset

   SUBROUTINE load_record(self, status, message)
      !! Fetch one complete external record into the reader buffer.
      !!
      !! EOF is sticky. Successful reads increment `record_number`, reset
      !! `cursor` to one, and derive the significant length with `LEN_TRIM`.
      !! A raw record is read into a doubled look-ahead buffer before it is
      !! copied into the fixed-size token buffer. This catches common accidental
      !! overlength input without increasing the normal token buffer size.
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self
         !! Reader whose buffer and state are updated.
      INTEGER, INTENT(OUT)                            :: status
         !! `VIS_READ_OK`, `VIS_READ_END`, `VIS_READ_INVALID`, or
         !! `VIS_READ_IO_ERROR`.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)          :: message
         !! Allocated diagnostic text; empty unless an error is reported.

      INTEGER :: ios
      CHARACTER(256) :: iomsg
      CHARACTER(VIS_RECORD_BUFFER_LENGTH) :: raw_record
      CHARACTER(LENGTH_LINELONG) :: detail

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

   SUBROUTINE skip_blanks(self, status, message)
      !! Position the cursor at the next non-space character.
      !!
      !! Blank and empty records are crossed iteratively. The routine loads
      !! records until input is found, EOF is reached, or an error occurs.
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self
         !! Reader whose cursor and possibly record are advanced.
      INTEGER, INTENT(OUT)                            :: status
         !! Status propagated from record loading, or `VIS_READ_OK`.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)          :: message
         !! Allocated diagnostic text; empty on success and normal EOF.

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

   SUBROUTINE token_reader_read_character(self, value, status, message)
      !! Read the next non-space character.
      !!
      !! Exactly one character is consumed. Subsequent calls continue at the
      !! following column, which permits character-by-character mask parsing.
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self
         !! Reader to consume.
      CHARACTER, INTENT(OUT)                          :: value
         !! Character read; blank when no value is returned.
      INTEGER, INTENT(OUT)                            :: status
         !! Reader status.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)          :: message
         !! Allocated diagnostic text.

      value = ''
      CALL skip_blanks(self, status, message)
      IF (status /= VIS_READ_OK) RETURN

      value = self%record(self%cursor:self%cursor)
      self%cursor = self%cursor + 1
   END SUBROUTINE token_reader_read_character

   SUBROUTINE token_reader_next_token(self, token, status, message)
      !! Read one blank-delimited token from a single record.
      !!
      !! Leading spaces and blank records are skipped. The cursor is left on
      !! the terminating space, or one column beyond the significant record
      !! length when the token ends at end-of-record.
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self
         !! Reader to consume.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)          :: token
         !! Allocated exact-length token; empty when no token is returned.
      INTEGER, INTENT(OUT)                            :: status
         !! Reader status.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)          :: message
         !! Allocated diagnostic text.

      INTEGER :: first, last

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

   SUBROUTINE token_reader_read_text(self, value, status, message)
      !! Read text using the compatibility semantics required by `R_C`.
      !!
      !! If `LEN(value)==1`, one non-space character is consumed. Otherwise,
      !! one complete token is consumed and assigned with normal Fortran
      !! truncation or blank-padding semantics. This preserves the behaviour
      !! needed by visualisation-plan keywords and values while avoiding the
      !! legacy list-directed read of the rest of the current record; later
      !! tokens on the same record remain available to subsequent reads.
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self
         !! Reader to consume.
      CHARACTER(*), INTENT(OUT)                       :: value
         !! Character destination; blank when no value is returned.
      INTEGER, INTENT(OUT)                            :: status
         !! Reader status.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)          :: message
         !! Allocated diagnostic text.

      CHARACTER(:), ALLOCATABLE :: token

      value = ''
      IF (LEN(value) == 1) THEN
         CALL self%read_character(value, status, message)
      ELSE
         CALL self%next_token(token, status, message)
         IF (status == VIS_READ_OK) value = token
      END IF
   END SUBROUTINE token_reader_read_text

   SUBROUTINE token_reader_read_integer(self, value, status, message)
      !! Validate and convert one token to default `INTEGER`.
      !!
      !! Accepted syntax is `[+|-]digit...`. The entire token must match.
      !! Internal list-directed conversion with `IOSTAT`/`IOMSG` then enforces
      !! the processor's default-integer range.
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self
         !! Reader to consume.
      INTEGER, INTENT(OUT)                            :: value
         !! Converted value; initialized to zero before tokenization.
      INTEGER, INTENT(OUT)                            :: status
         !! Reader status, or `VIS_READ_INVALID` for syntax/range failure.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)          :: message
         !! Allocated diagnostic including token and record on failure.

      INTEGER :: ios
      CHARACTER(256) :: iomsg
      CHARACTER(LENGTH_LINELONG) :: detail
      CHARACTER(:), ALLOCATABLE :: token

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

   SUBROUTINE token_reader_read_real(self, value, status, message)
      !! Validate and convert one token to finite default `REAL`.
      !!
      !! Accepted syntax is a signed or unsigned decimal mantissa with at
      !! least one digit and an optional `E`, `e`, `D`, or `d` exponent with
      !! optional sign and at least one digit. `NaN`, infinities, malformed
      !! tokens, conversion failures, and non-finite conversion results are
      !! rejected.
      CLASS(visualisation_token_reader), INTENT(INOUT) :: self
         !! Reader to consume.
      REAL, INTENT(OUT)                               :: value
         !! Converted value; initialized to zero before tokenization.
      INTEGER, INTENT(OUT)                            :: status
         !! Reader status, or `VIS_READ_INVALID` for syntax/conversion failure.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)          :: message
         !! Allocated diagnostic including token and record on failure.

      INTEGER :: ios
      CHARACTER(256) :: iomsg
      CHARACTER(LENGTH_LINELONG) :: detail
      CHARACTER(:), ALLOCATABLE :: token

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

   SUBROUTINE token_reader_get_position(self, record_number, column)
      !! Return a stable progress coordinate without modifying reader state.
      !!
      !! Coordinates are `(0,1)` immediately after `reset`. They are intended
      !! for diagnostics and monotonic-progress assertions, not external unit
      !! positioning.
      CLASS(visualisation_token_reader), INTENT(IN) :: self
         !! Reader to inspect.
      INTEGER, INTENT(OUT)                         :: record_number, column
         !! Current one-based logical record count and next cursor column.

      record_number = self%record_number
      column = self%cursor
   END SUBROUTINE token_reader_get_position

   PURE LOGICAL FUNCTION is_integer_token(token) RESULT(valid)
      !! Return true when the entire token matches the integer lexical grammar.
      CHARACTER(*), INTENT(IN) :: token
         !! Exact token text; embedded or trailing blanks are invalid.
      INTEGER :: i, first

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

   PURE LOGICAL FUNCTION is_real_token(token) RESULT(valid)
      !! Return true when the entire token matches the real lexical grammar.
      !!
      !! This routine validates syntax only; range and finiteness are checked
      !! after conversion by `token_reader_read_real`.
      CHARACTER(*), INTENT(IN) :: token
         !! Exact token text.
      INTEGER :: i, digits_before, digits_after, exponent_digits

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

   PURE LOGICAL FUNCTION is_digit(character) RESULT(valid)
      !! Return true only for an ASCII decimal digit (`0` through `9`).
      CHARACTER, INTENT(IN) :: character
         !! Character to classify.

      valid = character >= '0' .AND. character <= '9'
   END FUNCTION is_digit

   PURE LOGICAL FUNCTION visualisation_title_matches(record, expected) RESULT(matches)
      !! Compare a title record with its expected unquoted value.
      !!
      !! Leading and trailing blanks are removed. One matching pair of outer
      !! single or double quotes is then removed before exact, case-sensitive
      !! comparison.
      CHARACTER(*), INTENT(IN) :: record, expected
         !! Source title record and expected normalized title.
      CHARACTER(:), ALLOCATABLE :: title
      INTEGER :: length

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

   SUBROUTINE transform_visualisation_record(record, delimiter, separators, segments, status, message)
      !! Validate and split one source visualisation-plan record.
      !!
      !! Processing order is:
      !!
      !! 1. Remove `delimiter` and all following text.
      !! 2. Reject more than `VIS_MAX_RECORD_LENGTH` significant characters in
      !!    the uncommented content.
      !! 3. Reject characters outside printable ASCII codes 32 through 126.
      !! 4. Split on either separator.
      !! 5. trim each segment and omit empty segments.
      !!
      !! Segment order is preserved. The routine performs no external I/O.
      CHARACTER(*), INTENT(IN)                       :: record
         !! Source record; trailing blanks are insignificant.
      CHARACTER, INTENT(IN)                          :: delimiter
         !! Character beginning an inline comment.
      CHARACTER, INTENT(IN)                          :: separators(2)
         !! Two characters that terminate and separate output segments.
      CHARACTER(VIS_MAX_RECORD_LENGTH), ALLOCATABLE, INTENT(OUT) :: segments(:)
         !! Allocated segment array; zero-sized when the record emits nothing.
      INTEGER, INTENT(OUT)                           :: status
         !! `VIS_READ_OK` or `VIS_READ_INVALID`.
      CHARACTER(:), ALLOCATABLE, INTENT(OUT)         :: message
         !! Allocated diagnostic text; empty on success.

      INTEGER :: content_length, delimiter_position, first, last, count, i, code
      CHARACTER(VIS_MAX_RECORD_LENGTH) :: content
      CHARACTER(LENGTH_LINELONG) :: detail

      status = VIS_READ_OK
      message = ''
      ALLOCATE (segments(0))

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

      DEALLOCATE (segments)
      ALLOCATE (segments(count))
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
