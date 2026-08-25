!> Single entry point to the SHETRAN test assertions.
!>
!> Test programs use this module rather than the individual assertion modules,
!> so that adding an assertion type does not require every suite to add a
!> `USE` statement. The implementations live in `test_support_core` and the
!> type-specific modules beside it.
MODULE shetran_test_support

   USE test_support_core, ONLY: assert_true, finish_tests
   USE test_support_integer, ONLY: assert_equal_integer
   USE test_support_character, ONLY: assert_equal_character
   USE test_support_real, ONLY: assert_close_real

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: assert_true, assert_equal_integer, assert_equal_character, assert_close_real, finish_tests

END MODULE shetran_test_support
