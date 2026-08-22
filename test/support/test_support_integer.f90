!> Integer assertions for SHETRAN test executables.
MODULE test_support_integer

   USE test_support_core, ONLY: assert_true

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: assert_equal_integer

CONTAINS

   !> Asserts exact integer equality, reporting both values on failure.
   SUBROUTINE assert_equal_integer(actual, expected, name)
      INTEGER, INTENT(IN)      :: actual, expected
      CHARACTER(*), INTENT(IN) :: name
      CHARACTER(160) :: detail

      WRITE (detail, '(A,I0,A,I0)') 'expected ', expected, ', actual ', actual
      CALL assert_true(actual == expected, name, detail)
   END SUBROUTINE assert_equal_integer

END MODULE test_support_integer
