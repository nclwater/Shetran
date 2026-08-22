!> @brief Row widths of the open-channel implicit solver.
!>
!> The open-channel row solver in [[OCmod]] works on one block row of the
!> basic grid at a time. [[ocmod:ocind]] lists the elements of every row
!> contiguously in `NROWEL` and records where each row starts in `NROWST`, so
!> the width of row `j` is the difference of successive row-start pointers.
!> The greatest of those widths sizes the solver workspace allocated by
!> [[ocmod:initialise_ocsim_workspace]].
!>
!> This module holds only that derivation. It deliberately depends on nothing
!> else so the sizing rule can be exercised directly by
!> `test/oc_row_width/test_oc_row_width.f90` without linking the model.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-08-22 | SvB | 4.6.4 | Extracted the maximum-row-width derivation from [[ocmod:ocind]] so it can be tested on its own. |
!> @endhistory
MODULE OC_ROW_WIDTH

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: MAX_ACTIVE_ROW_WIDTH

CONTAINS

!> @brief Returns the greatest row width described by a row-start pointer array.
!>
!> For a row-start array holding `N+1` pointers, the width of row `j` is
!>
!> \[
!> n_j = NROWST(j+1)-NROWST(j),
!> \]
!>
!> and the result is \(\max_j n_j\).
!>
!> Empty rows contribute zero, so they cannot raise the result. Fewer than two
!> pointers describe no row at all and give zero, as does an array whose
!> entries are not increasing; the caller is expected to treat a zero result as
!> a topology error rather than allocate from it.
   PURE INTEGER FUNCTION MAX_ACTIVE_ROW_WIDTH(NROWST) RESULT(WIDTH)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NROWST(:) !! Row-start pointers, one per row plus a final end-of-last-row marker.

      ! Locals
      INTEGER :: N !! Number of supplied row-start pointers.

      !----------------------------------------------------------------------*

      N = SIZE(NROWST)

      IF (N < 2) THEN
         WIDTH = 0
         RETURN
      END IF

      WIDTH = MAX(0, MAXVAL(NROWST(2:N) - NROWST(1:N - 1)))

   END FUNCTION MAX_ACTIVE_ROW_WIDTH

END MODULE OC_ROW_WIDTH
