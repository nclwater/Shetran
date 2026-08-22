!> Tests the open-channel solver row-width derivation used to size the
!> [[ocmod:ocsim]] workspace. See `test/README.md`.
PROGRAM test_oc_row_width

   USE OC_ROW_WIDTH, ONLY: MAX_ACTIVE_ROW_WIDTH
   USE shetran_test_support, ONLY: assert_equal_integer, finish_tests

   IMPLICIT NONE

   CALL test_degenerate_input()
   CALL test_uniform_rows()
   CALL test_empty_and_widest_rows()
   CALL test_matches_incremental_reference()

   CALL finish_tests()

CONTAINS

   !> Fewer than two pointers describe no row, and a malformed (decreasing)
   !> pointer array must not produce a negative size.
   SUBROUTINE test_degenerate_input()
      INTEGER :: none(0), one(1), decreasing(3)

      one = [1]
      decreasing = [7, 4, 1]

      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(none), 0, 'no row-start pointers')
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(one), 0, 'single row-start pointer')
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(decreasing), 0, 'decreasing row-start pointers')
   END SUBROUTINE test_degenerate_input

   !> Equal-width rows, including the single-row case.
   SUBROUTINE test_uniform_rows()
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(row_starts([5])), 5, 'single row')
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(row_starts([3, 3, 3])), 3, 'three equal rows')
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(row_starts([0])), 0, 'single empty row')
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(row_starts([0, 0, 0])), 0, 'no active elements')
   END SUBROUTINE test_uniform_rows

   !> Empty rows must not raise the result, and the widest row must be found
   !> wherever it lies - in particular in the last row, which is bounded by the
   !> end-of-last-row marker [[ocmod:ocind]] writes into `NROWST(NY+1)`.
   SUBROUTINE test_empty_and_widest_rows()
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(row_starts([0, 4, 1, 0, 3])), 4, 'widest row in the interior')
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(row_starts([6, 2, 1])), 6, 'widest row first')
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(row_starts([1, 2, 6])), 6, 'widest row last')
      CALL assert_equal_integer(MAX_ACTIVE_ROW_WIDTH(row_starts([0, 0, 2, 0, 0])), 2, 'empty leading and trailing rows')
   END SUBROUTINE test_empty_and_widest_rows

   !> Reproduces the running maximum [[ocmod:ocind]] carried in its row loop
   !> before the derivation was extracted, over a spread of row patterns. The
   !> replacement must agree with it exactly, since the workspace allocation
   !> depends on the value.
   SUBROUTINE test_matches_incremental_reference()
      INTEGER, PARAMETER :: rows = 37
      INTEGER :: widths(rows), pattern, j, expected, actual
      CHARACTER(64) :: name

      DO pattern = 1, 4
         DO j = 1, rows
            SELECT CASE (pattern)
             CASE (1)
               widths(j) = MOD(j*7, 13)                  ! irregular, some empty
             CASE (2)
               widths(j) = j                             ! monotonically widening
             CASE (3)
               widths(j) = rows - j                      ! monotonically narrowing
             CASE DEFAULT
               widths(j) = MERGE(11, 0, MOD(j, 5) == 0)  ! isolated active rows
            END SELECT
         END DO

         expected = incremental_maximum(widths)
         actual = MAX_ACTIVE_ROW_WIDTH(row_starts(widths))
         WRITE (name, '(A,I0)') 'incremental reference agreement, pattern ', pattern
         CALL assert_equal_integer(actual, expected, TRIM(name))
      END DO
   END SUBROUTINE test_matches_incremental_reference

   !> Builds the row-start pointers [[ocmod:ocind]] would write for the given
   !> per-row element counts, including the final end-of-last-row marker.
   PURE FUNCTION row_starts(widths) RESULT(nrowst)
      INTEGER, INTENT(IN) :: widths(:)
      INTEGER :: nrowst(SIZE(widths) + 1)
      INTEGER :: j, k

      k = 0
      DO j = 1, SIZE(widths)
         nrowst(j) = k + 1
         k = k + widths(j)
      END DO
      nrowst(SIZE(widths) + 1) = k + 1
   END FUNCTION row_starts

   !> The running maximum the row loop used to keep, expressed on the same
   !> pointer differences: `MAX_ROW_WIDTH = MAX(MAX_ROW_WIDTH, K + 1 - NROWST(J))`.
   PURE FUNCTION incremental_maximum(widths) RESULT(width)
      INTEGER, INTENT(IN) :: widths(:)
      INTEGER :: width, j

      width = 0
      DO j = 1, SIZE(widths)
         width = MAX(width, widths(j))
      END DO
   END FUNCTION incremental_maximum

END PROGRAM test_oc_row_width
