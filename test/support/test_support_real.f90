!> Default-`REAL` assertions for SHETRAN test executables.
MODULE test_support_real

   USE test_support_core, ONLY: assert_true

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: assert_close_real

CONTAINS

   !> Asserts that `actual` and `expected` agree within `tolerance`, applied
   !> relatively for magnitudes above one and absolutely below it:
   !> `|actual - expected| <= tolerance * MAX(1, |expected|)`.
   SUBROUTINE assert_close_real(actual, expected, tolerance, name)
      REAL, INTENT(IN)         :: actual, expected, tolerance
      CHARACTER(*), INTENT(IN) :: name
      CHARACTER(200) :: detail

      WRITE (detail, '(A,ES14.6,A,ES14.6,A,ES14.6)') 'expected ', expected, &
         ', actual ', actual, ', tolerance ', tolerance
      CALL assert_true(ABS(actual - expected) <= tolerance*MAX(1.0, ABS(expected)), name, detail)
   END SUBROUTINE assert_close_real

END MODULE test_support_real

