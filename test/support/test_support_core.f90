!> Assertion bookkeeping shared by every SHETRAN test executable.
!>
!> This module owns the counters and the single point at which a test decides
!> that it has failed. The type-specific assertion modules formulate their
!> diagnostic and then delegate here, so that assertion counts and exit status
!> behave identically across suites.
MODULE test_support_core

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: assert_true, finish_tests

   INTEGER :: assertions = 0
   INTEGER :: failures = 0

CONTAINS

   !> Records one assertion, reporting `name` and the optional `detail` when
   !> `condition` is false.
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

   !> Prints the assertion summary and stops with a non-zero status if any
   !> assertion failed.
   SUBROUTINE finish_tests()
      WRITE (*, '(A,I0,A,I0)') 'Assertions: ', assertions, ', failures: ', failures
      IF (failures /= 0) ERROR STOP 1
   END SUBROUTINE finish_tests

END MODULE test_support_core
