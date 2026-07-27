MODULE visualisation_read_test_support

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: assert_true, assert_equal_integer, assert_equal_character, assert_close_real, finish_tests

   INTEGER :: assertions = 0
   INTEGER :: failures = 0

CONTAINS

   SUBROUTINE assert_true(condition, name, detail)
      LOGICAL, INTENT(IN)                    :: condition
      CHARACTER(*), INTENT(IN)               :: name
      CHARACTER(*), INTENT(IN), OPTIONAL     :: detail

      assertions = assertions + 1
      IF (condition) RETURN

      failures = failures + 1
      WRITE (*, '(A)') 'FAIL: '//TRIM(name)
      IF (PRESENT(detail)) THEN
         IF (LEN_TRIM(detail) > 0) WRITE (*, '(A)') '  '//TRIM(detail)
      END IF
   END SUBROUTINE assert_true

   SUBROUTINE assert_equal_integer(actual, expected, name)
      INTEGER, INTENT(IN)      :: actual, expected
      CHARACTER(*), INTENT(IN) :: name
      CHARACTER(160) :: detail

      WRITE (detail, '(A,I0,A,I0)') 'expected ', expected, ', actual ', actual
      CALL assert_true(actual == expected, name, detail)
   END SUBROUTINE assert_equal_integer

   SUBROUTINE assert_equal_character(actual, expected, name)
      CHARACTER(*), INTENT(IN) :: actual, expected, name
      CHARACTER(512) :: detail

      WRITE (detail, '(A,A,A,A,A)') 'expected "', TRIM(expected), '", actual "', TRIM(actual), '"'
      CALL assert_true(actual == expected, name, detail)
   END SUBROUTINE assert_equal_character

   SUBROUTINE assert_close_real(actual, expected, tolerance, name)
      REAL, INTENT(IN)         :: actual, expected, tolerance
      CHARACTER(*), INTENT(IN) :: name
      CHARACTER(200) :: detail

      WRITE (detail, '(A,ES14.6,A,ES14.6,A,ES14.6)') 'expected ', expected, &
         ', actual ', actual, ', tolerance ', tolerance
      CALL assert_true(ABS(actual - expected) <= tolerance*MAX(1.0, ABS(expected)), name, detail)
   END SUBROUTINE assert_close_real

   SUBROUTINE finish_tests()
      WRITE (*, '(A,I0,A,I0)') 'Assertions: ', assertions, ', failures: ', failures
      IF (failures /= 0) ERROR STOP 1
   END SUBROUTINE finish_tests

END MODULE visualisation_read_test_support
