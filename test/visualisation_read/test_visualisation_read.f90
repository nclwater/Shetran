PROGRAM test_visualisation_read

   USE visualisation_read_parser, ONLY: visualisation_token_reader, transform_visualisation_record, &
      visualisation_title_matches, VIS_READ_OK, VIS_READ_END, VIS_READ_INVALID, &
      VIS_MAX_RECORD_LENGTH, VIS_RECORD_BUFFER_LENGTH
   USE visualisation_read, ONLY: COPY, R_C, R_I, R_R, vp_in
   USE visualisation_read_test_support, ONLY: assert_true, assert_equal_integer, &
      assert_equal_character, assert_close_real, finish_tests

   IMPLICIT NONE

   CHARACTER(32) :: suite

   IF (COMMAND_ARGUMENT_COUNT() < 1) THEN
      WRITE (*, '(A)') 'usage: visualisation_read_tests <unit|examples> [arguments]'
      ERROR STOP 2
   END IF

   CALL GET_COMMAND_ARGUMENT(1, suite)
   SELECT CASE (TRIM(suite))
    CASE ('unit')
      CALL run_unit_tests()
    CASE ('examples')
      CALL run_example_tests()
    CASE DEFAULT
      WRITE (*, '(A)') 'unknown test suite: '//TRIM(suite)
      ERROR STOP 2
   END SELECT

   CALL finish_tests()

CONTAINS

   SUBROUTINE run_unit_tests()
      CHARACTER(1024) :: data_directory

      IF (COMMAND_ARGUMENT_COUNT() /= 2) THEN
         WRITE (*, '(A)') 'unit suite requires its fixture directory'
         ERROR STOP 2
      END IF
      CALL GET_COMMAND_ARGUMENT(2, data_directory)

      CALL test_record_progression(TRIM(data_directory))
      CALL test_text_and_characters(TRIM(data_directory))
      CALL test_integers()
      CALL test_reals()
      CALL test_record_transform()
      CALL test_strip(TRIM(data_directory))
   END SUBROUTINE run_unit_tests

   SUBROUTINE test_record_progression(data_directory)
      CHARACTER(*), INTENT(IN) :: data_directory
      TYPE(visualisation_token_reader) :: test_reader
      INTEGER :: unit, status, value, record_number, column, old_record, old_column
      CHARACTER(VIS_RECORD_BUFFER_LENGTH) :: overlength_record
      CHARACTER(:), ALLOCATABLE :: detail, token

      OPEN (NEWUNIT=unit, FILE=path_join(data_directory, 'records_basic.txt'), &
         STATUS='old', ACTION='read')
      CALL test_reader%reset(unit)

      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_OK, 'first integer status')
      CALL assert_equal_integer(value, 1, 'first integer value')
      CALL test_reader%get_position(old_record, old_column)

      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(value, 3, 'same-record integer')
      CALL test_reader%get_position(record_number, column)
      CALL assert_true(position_advanced(old_record, old_column, record_number, column), &
         'same-record position advances')

      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(value, 12, 'end-of-record integer')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(value, 36, 'integer after blank records')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(value, 48, 'next-record integer')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(value, 8760, 'final integer')

      value = -99
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_END, 'EOF status')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_END, 'repeated EOF status')
      CLOSE (unit)

      OPEN (NEWUNIT=unit, FILE=path_join(data_directory, 'records_no_final_newline.txt'), &
         STATUS='old', ACTION='read')
      CALL test_reader%reset(unit)
      CALL test_reader%next_token(token, status, detail)
      CALL assert_equal_integer(status, VIS_READ_OK, 'no-final-newline token status')
      CALL assert_equal_character(token, 'final', 'no-final-newline token')
      CALL test_reader%next_token(token, status, detail)
      CALL assert_equal_integer(status, VIS_READ_END, 'no-final-newline EOF')
      CLOSE (unit)

      OPEN (NEWUNIT=unit, STATUS='scratch', ACTION='readwrite')
      overlength_record = REPEAT('x', VIS_MAX_RECORD_LENGTH)//' '//'x'
      WRITE (unit, '(A)') TRIM(overlength_record)
      REWIND (unit)
      CALL test_reader%reset(unit)
      CALL test_reader%next_token(token, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'overlength record after blank lookahead')
      CALL assert_true(INDEX(detail, 'exceeds maximum length') > 0, 'overlength record diagnostic')
      CLOSE (unit)
   END SUBROUTINE test_record_progression

   SUBROUTINE test_text_and_characters(data_directory)
      CHARACTER(*), INTENT(IN) :: data_directory
      TYPE(visualisation_token_reader) :: test_reader
      INTEGER :: unit, status, i, j
      INTEGER, PARAMETER :: mask_lengths(6) = (/3, 20, 61, 86, 100, 122/)
      CHARACTER(12) :: text
      CHARACTER(4) :: short_text
      CHARACTER :: character
      CHARACTER(:), ALLOCATABLE :: detail

      OPEN (NEWUNIT=unit, FILE=path_join(data_directory, 'character_records.txt'), &
         STATUS='replace', ACTION='readwrite')
      WRITE (unit, '(A)') '   item NUMBER ENDITEM'
      WRITE (unit, '(A)') '   ABCDEFGHIJKLM next'
      WRITE (unit, '(A)') ''
      WRITE (unit, '(A)') '  .==1'
      DO i = 1, SIZE(mask_lengths)
         WRITE (unit, '(A)') REPEAT('=', mask_lengths(i))
      END DO
      REWIND (unit)
      CALL test_reader%reset(unit)

      CALL test_reader%read_text(text, status, detail)
      CALL assert_equal_character(text, 'item        ', 'read keyword and blank-pad')
      CALL test_reader%read_text(text, status, detail)
      CALL assert_equal_character(text, 'NUMBER      ', 'read uppercase field')
      CALL test_reader%read_text(text, status, detail)
      CALL assert_equal_character(text, 'ENDITEM     ', 'read item terminator')
      CALL test_reader%read_text(short_text, status, detail)
      CALL assert_equal_integer(status, VIS_READ_OK, 'truncated text read status')
      CALL assert_equal_character(short_text, 'ABCD', 'truncate long text token')
      CALL test_reader%read_text(text, status, detail)
      CALL assert_equal_character(text, 'next        ', 'preserve following text token')

      CALL test_reader%read_character(character, status, detail)
      CALL assert_equal_character(character, '.', 'first mask character')
      CALL test_reader%read_character(character, status, detail)
      CALL assert_equal_character(character, '=', 'second mask character')
      CALL test_reader%read_character(character, status, detail)
      CALL assert_equal_character(character, '=', 'third mask character')
      CALL test_reader%read_character(character, status, detail)
      CALL assert_equal_character(character, '1', 'fourth mask character')
      DO i = 1, SIZE(mask_lengths)
         DO j = 1, mask_lengths(i)
            CALL test_reader%read_character(character, status, detail)
            CALL assert_equal_character(character, '=', 'long mask character')
         END DO
      END DO
      CLOSE (unit, STATUS='delete')
   END SUBROUTINE test_text_and_characters

   SUBROUTINE test_integers()
      TYPE(visualisation_token_reader) :: test_reader
      INTEGER :: unit, status, value
      CHARACTER(:), ALLOCATABLE :: detail

      OPEN (NEWUNIT=unit, STATUS='scratch', ACTION='readwrite')
      WRITE (unit, '(A)') '0 1 876000 0007 +42 -42'
      WRITE (unit, '(I0)') HUGE(0)
      WRITE (unit, '(A)') '+ - 12x 1.0 1E3 999999999999999999999999'
      WRITE (unit, '(A)') REPEAT('x', VIS_MAX_RECORD_LENGTH)
      REWIND (unit)
      CALL test_reader%reset(unit)

      CALL read_expected_integer(test_reader, 0, 'integer zero')
      CALL read_expected_integer(test_reader, 1, 'integer one')
      CALL read_expected_integer(test_reader, 876000, 'integer corpus value')
      CALL read_expected_integer(test_reader, 7, 'integer leading zeros')
      CALL read_expected_integer(test_reader, 42, 'positive signed integer')
      CALL read_expected_integer(test_reader, -42, 'negative signed integer')
      CALL read_expected_integer(test_reader, HUGE(0), 'largest default integer')

      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'reject sign-only plus')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'reject sign-only minus')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'reject integer trailing junk')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'reject decimal as integer')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'reject exponent as integer')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'reject integer overflow')
      CALL assert_true(INDEX(detail, 'cannot convert integer') > 0, 'integer overflow diagnostic')
      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'reject maximum-length bad integer token')
      CALL assert_true(INDEX(detail, 'expected integer') > 0, 'maximum-length token diagnostic')
      CLOSE (unit)
   END SUBROUTINE test_integers

   SUBROUTINE read_expected_integer(test_reader, expected, name)
      TYPE(visualisation_token_reader), INTENT(INOUT) :: test_reader
      INTEGER, INTENT(IN)                            :: expected
      CHARACTER(*), INTENT(IN)                       :: name
      INTEGER :: status, value
      CHARACTER(:), ALLOCATABLE :: detail

      CALL test_reader%read_integer(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_OK, TRIM(name)//' status')
      CALL assert_equal_integer(value, expected, name)
   END SUBROUTINE read_expected_integer

   SUBROUTINE test_reals()
      TYPE(visualisation_token_reader) :: test_reader
      INTEGER :: unit, status, i
      REAL :: value
      CHARACTER(:), ALLOCATABLE :: detail
      CHARACTER(8), PARAMETER :: invalid(11) = [CHARACTER(8) :: '.', '+', '-', '--1', &
         '1.2.3', '1E', '1D+', '12x', 'NaN', 'Inf', '1E9999']

      OPEN (NEWUNIT=unit, STATUS='scratch', ACTION='readwrite')
      WRITE (unit, '(A)') '876000 -1000. -0.100 1.0e-3 4.00E-02 2.5D-2 .1D-3 1.537D3 1.00E-4'
      WRITE (unit, '(A)') '0 0.0 .5 5. +2.5 -2.5E-4 1E+3 1d0'
      WRITE (unit, '(A)') '. + - --1 1.2.3 1E 1D+ 12x NaN Inf 1E9999'
      REWIND (unit)
      CALL test_reader%reset(unit)

      CALL read_expected_real(test_reader, 876000.0, 'real integer form')
      CALL read_expected_real(test_reader, -1000.0, 'real trailing decimal')
      CALL read_expected_real(test_reader, -0.100, 'negative decimal')
      CALL read_expected_real(test_reader, 1.0E-3, 'lowercase E exponent')
      CALL read_expected_real(test_reader, 4.0E-2, 'uppercase E exponent')
      CALL read_expected_real(test_reader, 2.5E-2, 'uppercase D exponent')
      CALL read_expected_real(test_reader, 0.1E-3, 'leading decimal D exponent')
      CALL read_expected_real(test_reader, 1.537E3, 'D exponent')
      CALL read_expected_real(test_reader, 1.0E-4, 'small exponent')
      CALL read_expected_real(test_reader, 0.0, 'real zero integer form')
      CALL read_expected_real(test_reader, 0.0, 'real zero decimal')
      CALL read_expected_real(test_reader, 0.5, 'leading decimal')
      CALL read_expected_real(test_reader, 5.0, 'trailing decimal')
      CALL read_expected_real(test_reader, 2.5, 'positive signed real')
      CALL read_expected_real(test_reader, -2.5E-4, 'negative exponent real')
      CALL read_expected_real(test_reader, 1.0E3, 'positive exponent sign')
      CALL read_expected_real(test_reader, 1.0, 'lowercase D exponent')

      DO i = 1, SIZE(invalid)
         CALL test_reader%read_real(value, status, detail)
         CALL assert_equal_integer(status, VIS_READ_INVALID, 'reject real '//TRIM(invalid(i)))
      END DO
      CLOSE (unit)
   END SUBROUTINE test_reals

   SUBROUTINE read_expected_real(test_reader, expected, name)
      TYPE(visualisation_token_reader), INTENT(INOUT) :: test_reader
      REAL, INTENT(IN)                               :: expected
      CHARACTER(*), INTENT(IN)                       :: name
      INTEGER :: status
      REAL :: value
      CHARACTER(:), ALLOCATABLE :: detail

      CALL test_reader%read_real(value, status, detail)
      CALL assert_equal_integer(status, VIS_READ_OK, TRIM(name)//' status')
      CALL assert_close_real(value, expected, 1.0E-5, name)
   END SUBROUTINE read_expected_real

   SUBROUTINE test_record_transform()
      CHARACTER(VIS_MAX_RECORD_LENGTH), ALLOCATABLE :: segments(:)
      CHARACTER(:), ALLOCATABLE :: detail
      CHARACTER(VIS_MAX_RECORD_LENGTH + 1) :: long_record
      INTEGER :: status

      CALL transform_visualisation_record(' item : NUMBER^1 ! comment', '!', &
         (/ ':', '^' /), segments, status, detail)
      CALL assert_equal_integer(status, VIS_READ_OK, 'record transform status')
      CALL assert_equal_integer(SIZE(segments), 3, 'record transform segment count')
      CALL assert_equal_character(TRIM(segments(1)), 'item', 'first transformed segment')
      CALL assert_equal_character(TRIM(segments(2)), 'NUMBER', 'second transformed segment')
      CALL assert_equal_character(TRIM(segments(3)), '1', 'third transformed segment')

      CALL transform_visualisation_record(':: item ^^', '!', (/ ':', '^' /), &
         segments, status, detail)
      CALL assert_equal_integer(SIZE(segments), 1, 'adjacent separators omitted')

      CALL transform_visualisation_record('item'//ACHAR(9)//'NUMBER', '!', &
         (/ ':', '^' /), segments, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'tab rejected')
      CALL assert_true(INDEX(detail, 'ASCII character 9') > 0, 'tab diagnostic')

      CALL transform_visualisation_record('item ! comment'//ACHAR(9), '!', &
         (/ ':', '^' /), segments, status, detail)
      CALL assert_equal_integer(status, VIS_READ_OK, 'tab after comment ignored')
      CALL assert_equal_integer(SIZE(segments), 1, 'commented tab segment count')
      CALL assert_equal_character(TRIM(segments(1)), 'item', 'commented tab content')

      CALL transform_visualisation_record('!'//ACHAR(1), '!', &
         (/ ':', '^' /), segments, status, detail)
      CALL assert_equal_integer(status, VIS_READ_OK, 'non-printable after full-line comment ignored')
      CALL assert_equal_integer(SIZE(segments), 0, 'full-line comment emits no segments')

      CALL transform_visualisation_record('item'//ACHAR(1), '!', &
         (/ ':', '^' /), segments, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'non-printable rejected')

      long_record = REPEAT('x', LEN(long_record))
      CALL transform_visualisation_record(long_record, '!', (/ ':', '^' /), &
         segments, status, detail)
      CALL assert_equal_integer(status, VIS_READ_INVALID, 'long record rejected')

      CALL assert_true(visualisation_title_matches("'visualisation plan'", 'visualisation plan'), &
         'single-quoted title')
      CALL assert_true(visualisation_title_matches('"visualisation plan"', 'visualisation plan'), &
         'double-quoted title')
      CALL assert_true(.NOT. visualisation_title_matches("'wrong plan'", 'visualisation plan'), &
         'wrong title rejected')
   END SUBROUTINE test_record_transform

   SUBROUTINE test_strip(data_directory)
      CHARACTER(*), INTENT(IN) :: data_directory
      CHARACTER(512) :: actual, expected
      CHARACTER(12) :: text
      CHARACTER :: mask_character
      INTEGER :: expected_unit, ios_actual, ios_expected
      INTEGER :: integer_value, integer_value_2
      REAL :: real_value, real_value_2

      CALL COPY('.', path_join(data_directory, 'strip_input.txt'))
      OPEN (NEWUNIT=expected_unit, FILE=path_join(data_directory, 'strip_expected.txt'), &
         STATUS='old', ACTION='read')

      DO
         actual = ''
         expected = ''
         READ (vp_in, '(A)', IOSTAT=ios_actual) actual
         READ (expected_unit, '(A)', IOSTAT=ios_expected) expected
         CALL assert_equal_integer(ios_actual, ios_expected, 'strip record status')
         IF (ios_actual /= 0) EXIT
         CALL assert_equal_character(TRIM(actual), TRIM(expected), 'strip record')
      END DO

      CLOSE (expected_unit)
      CLOSE (vp_in, STATUS='delete')

      CALL COPY('.', path_join(data_directory, 'strip_input.txt'))
      CALL R_C('test item', text)
      CALL assert_equal_character(text, 'item        ', 'facade text item')
      CALL R_C('test number heading', text)
      CALL assert_equal_character(text, 'NUMBER      ', 'facade text heading')
      CALL R_I('test item number', integer_value)
      CALL assert_equal_integer(integer_value, 1, 'facade integer')
      CALL R_C('test mask', text)
      CALL assert_equal_character(text, 'mask        ', 'facade mask heading')
      CALL R_I('test mask bounds', integer_value, integer_value_2)
      CALL assert_equal_integer(integer_value, 1, 'facade first mask bound')
      CALL assert_equal_integer(integer_value_2, 1, 'facade second mask bound')
      CALL R_I('test mask bounds', integer_value, integer_value_2, ios_actual)
      CALL assert_equal_integer(integer_value, 1, 'facade third mask bound')
      CALL assert_equal_integer(integer_value_2, 3, 'facade fourth mask bound')
      CALL assert_equal_integer(ios_actual, 1, 'facade fifth mask bound')
      CALL R_C('test mask character', mask_character)
      CALL assert_equal_character(mask_character, '.', 'facade first mask character')
      CALL R_C('test mask character', mask_character)
      CALL assert_equal_character(mask_character, '=', 'facade second mask character')
      CALL R_C('test mask character', mask_character)
      CALL assert_equal_character(mask_character, '1', 'facade third mask character')
      CALL R_C('test times', text)
      CALL assert_equal_character(text, 'times       ', 'facade times heading')
      CALL R_I('test time header', integer_value, integer_value_2)
      CALL assert_equal_integer(integer_value, 9, 'facade time number')
      CALL assert_equal_integer(integer_value_2, 2, 'facade time size')
      CALL R_R('test time pair', real_value, real_value_2)
      CALL assert_close_real(real_value, 12.0, 1.0E-5, 'facade first timestep')
      CALL assert_close_real(real_value_2, 36.0, 1.0E-5, 'facade first stop time')
      CALL R_R('test time pair', real_value, real_value_2)
      CALL assert_close_real(real_value, 48.0, 1.0E-5, 'facade second timestep')
      CALL assert_close_real(real_value_2, 8760.0, 1.0E-5, 'facade second stop time')
      CALL R_C('test stop', text)
      CALL assert_equal_character(text, 'stop        ', 'facade stop')
      CLOSE (vp_in, STATUS='delete')
   END SUBROUTINE test_strip

   SUBROUTINE run_example_tests()
      INTEGER :: argument, plans_with_times, plans_with_lists, plans_with_masks
      CHARACTER(1024) :: plan

      IF (COMMAND_ARGUMENT_COUNT() < 2) THEN
         WRITE (*, '(A)') 'examples suite requires at least one visualisation plan'
         ERROR STOP 2
      END IF

      plans_with_times = 0
      plans_with_lists = 0
      plans_with_masks = 0
      DO argument = 2, COMMAND_ARGUMENT_COUNT()
         CALL GET_COMMAND_ARGUMENT(argument, plan)
         CALL test_example_plan(TRIM(plan), plans_with_times, plans_with_lists, plans_with_masks)
      END DO

      CALL assert_true(plans_with_times > 0, 'example corpus exercises typed times')
      CALL assert_true(plans_with_lists > 0, 'example corpus exercises typed lists')
      CALL assert_true(plans_with_masks > 0, 'example corpus exercises typed mask bounds')
   END SUBROUTINE run_example_tests

   SUBROUTINE test_example_plan(plan, plans_with_times, plans_with_lists, plans_with_masks)
      CHARACTER(*), INTENT(IN) :: plan
      INTEGER, INTENT(INOUT)   :: plans_with_times, plans_with_lists, plans_with_masks
      TYPE(visualisation_token_reader) :: test_reader
      CHARACTER(:), ALLOCATABLE :: token, detail, last_token
      CHARACTER(1024) :: case_name
      INTEGER :: status, tokens, old_record, old_column, record_number, column
      INTEGER :: number, size, i, value
      REAL :: real_value
      LOGICAL :: found_times, found_lists, found_masks

      case_name = 'example '//TRIM(plan)
      CALL COPY(parent_directory(plan), plan)
      CALL test_reader%reset(vp_in)
      tokens = 0
      token = ''
      last_token = ''
      found_times = .FALSE.
      found_lists = .FALSE.
      found_masks = .FALSE.

      DO
         CALL test_reader%get_position(old_record, old_column)
         CALL test_reader%next_token(token, status, detail)
         IF (status == VIS_READ_END) EXIT
         CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' token status')
         IF (status /= VIS_READ_OK) EXIT

         tokens = tokens + 1
         last_token = token
         CALL test_reader%get_position(record_number, column)
         CALL assert_true(position_advanced(old_record, old_column, record_number, column), &
            TRIM(case_name)//' forward progress')
         CALL assert_true(SCAN(token, '!:^') == 0, TRIM(case_name)//' stripped delimiters')
         CALL assert_true(tokens < 1000000, TRIM(case_name)//' token limit')
         IF (tokens >= 1000000) EXIT

         SELECT CASE (token)
          CASE ('LAYERS')
            CALL test_reader%read_integer(number, status, detail)
            CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' lower layer')
            CALL test_reader%read_integer(value, status, detail)
            CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' upper layer')
          CASE ('times')
            found_times = .TRUE.
            CALL test_reader%read_integer(number, status, detail)
            CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' time number')
            CALL test_reader%read_integer(size, status, detail)
            CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' time size')
            IF (status /= VIS_READ_OK) EXIT
            DO i = 1, size
               CALL test_reader%read_real(real_value, status, detail)
               CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' timestep')
               CALL test_reader%read_real(real_value, status, detail)
               CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' stop time')
            END DO
          CASE ('list')
            found_lists = .TRUE.
            CALL test_reader%read_integer(number, status, detail)
            CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' list number')
            CALL test_reader%read_integer(size, status, detail)
            CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' list size')
            IF (status /= VIS_READ_OK) EXIT
            DO i = 1, size
               CALL test_reader%read_integer(value, status, detail)
               CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' list member')
            END DO
          CASE ('mask')
            found_masks = .TRUE.
            DO i = 1, 5
               CALL test_reader%read_integer(value, status, detail)
               CALL assert_equal_integer(status, VIS_READ_OK, TRIM(case_name)//' mask bound')
            END DO
         END SELECT
      END DO

      CALL assert_equal_integer(status, VIS_READ_END, TRIM(case_name)//' clean EOF')
      CALL assert_equal_character(last_token, 'stop', TRIM(case_name)//' final token')
      CALL assert_true(tokens > 0, TRIM(case_name)//' non-empty token stream')
      IF (found_times) plans_with_times = plans_with_times + 1
      IF (found_lists) plans_with_lists = plans_with_lists + 1
      IF (found_masks) plans_with_masks = plans_with_masks + 1
      CLOSE (vp_in, STATUS='delete')
   END SUBROUTINE test_example_plan

   PURE LOGICAL FUNCTION position_advanced(old_record, old_column, record_number, column) RESULT(advanced)
      INTEGER, INTENT(IN) :: old_record, old_column, record_number, column

      advanced = record_number > old_record
      IF (record_number == old_record) advanced = column > old_column
   END FUNCTION position_advanced

   FUNCTION path_join(directory, name) RESULT(path)
      CHARACTER(*), INTENT(IN) :: directory, name
      CHARACTER(:), ALLOCATABLE :: path

      IF (directory(LEN_TRIM(directory):LEN_TRIM(directory)) == '/' .OR. &
          directory(LEN_TRIM(directory):LEN_TRIM(directory)) == '\') THEN
         path = TRIM(directory)//name
      ELSE
         path = TRIM(directory)//'/'//name
      END IF
   END FUNCTION path_join

   FUNCTION parent_directory(path) RESULT(directory)
      CHARACTER(*), INTENT(IN) :: path
      CHARACTER(:), ALLOCATABLE :: directory
      INTEGER :: i

      DO i = LEN_TRIM(path), 1, -1
         IF (path(i:i) == '/' .OR. path(i:i) == '\') THEN
            directory = path(:i - 1)
            RETURN
         END IF
      END DO
      directory = '.'
   END FUNCTION parent_directory

END PROGRAM test_visualisation_read
