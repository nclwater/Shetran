!> Character assertions for SHETRAN test executables.
MODULE test_support_character

   USE test_support_core, ONLY: assert_true

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: assert_equal_character

CONTAINS

   !> Asserts character equality. Fortran pads the shorter operand with blanks
   !> for the comparison, so this accepts values that differ only in trailing
   !> blanks; the diagnostic prints both values trimmed.
   SUBROUTINE assert_equal_character(actual, expected, name)
      CHARACTER(*), INTENT(IN) :: actual, expected, name
      CHARACTER(512) :: detail

      WRITE (detail, '(A,A,A,A,A)') 'expected "', TRIM(expected), '", actual "', TRIM(actual), '"'
      CALL assert_true(actual == expected, name, detail)
   END SUBROUTINE assert_equal_character

END MODULE test_support_character
