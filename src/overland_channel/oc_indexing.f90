!> summary: The row-solver index arrays and the element/link lookups.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[OCIND]] builds the row-by-row ordering the overland solver sweeps in:
!> `NROWST` points at the start of each row, `NROWEL` lists the elements in
!> order, and `MAX_SOLVER_ROW_WIDTH` is the widest active row, which sizes the
!> solver's working arrays. [[LINKNO]] maps a grid face to its channel link.
!>
!> The widest-row derivation itself is in [[oc_indexing]], which deliberately
!> depends on nothing so that it can be unit-tested without linking the model;
!> this module imports it.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_indexing

   USE array_limits, ONLY: nelee, nyee
   USE element_geometry, ONLY: total_no_links
   USE grid_topology, ONLY: ICMREF, ICMXY, NX, NY
   USE channel_geometry, ONLY: ICMBK, LINKNS
   USE file_units, ONLY: FID_logfile
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal
   USE oc_row_width, ONLY: MAX_ACTIVE_ROW_WIDTH

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: OCIND, LINKNO
   PUBLIC :: NELIND, NROWEL, NROWF, NROWL, NROWST, MAX_SOLVER_ROW_WIDTH


   ! Row-solver indexing (see [[ocind]])
   INTEGER            :: NELIND(NELEE)         !! Position of each element within its implicit-solver row.
   INTEGER            :: NROWF                  !! First non-empty OC solver row.
   INTEGER            :: NROWL                  !! Last non-empty OC solver row.
   INTEGER            :: MAX_SOLVER_ROW_WIDTH = 0      !! Greatest active OC solver-row width established by [[ocind]].
   INTEGER            :: NROWEL(NELEE)         !! Contiguous list of OC elements in row-solver order.
   INTEGER            :: NROWST(NYEE + 1)        !! Row-start pointer into `NROWEL`.

CONTAINS

!> @brief Builds row-order indexing for the implicit OC solver.
!>
!> The catchment is split into y-coordinate rows, with west/south channel
!> links and optional bank elements inserted beside the grid elements. The
!> resulting `NROWST`, `NROWEL`, and `NELIND` arrays define the block rows
!> used by [[OCSIM]].
!>
!> Rows follow the basic grid `y` coordinate. East-west links and their banks
!> are included in the row above the link, matching the legacy OC row solver
!> ordering.
!>
!> | Array | Meaning |
!> |:------|:--------|
!> | `NROWF` | First non-empty row number. |
!> | `NROWL` | Last non-empty row number. |
!> | `NROWST(j)` | Pointer into `NROWEL` for the first element in row `j`. |
!> | `NROWEL` | Contiguous list of elements in row order. |
!> | `NELIND(e)` | Position of element `e` within its row. |
!>
!> If element `i` is the `p`th entry in row `j`, then
!>
!> \[
!> e = NROWEL(NROWST(j)+p-1),
!> \]
!>
!> and `NELIND` is the partial inverse:
!>
!> \[
!> NELIND\left(NROWEL(NROWST(j)+p-1)\right)=p.
!> \]
!>
!> The row of grid element `ICMXY(x,y)`, and of any associated link/bank
!> elements inserted while processing that grid square, is `y`.
!>
!> For each grid square `(i,j)`, `OCIND` scans the west face (`FACE=3`) and
!> then the south face (`FACE=4`). If a link is present, the row receives
!>
!> | Bank option | Inserted sequence |
!> |:------------|:------------------|
!> | `BEXBK=.FALSE.` | `link` |
!> | `BEXBK=.TRUE.` | bank on one side, `link`, bank on the other side |
!>
!> using `ICMBK(link,5-FACE)` before the link and `ICMBK(link,FACE-2)` after
!> the link. On the west-face pass only, the active grid element
!> `ICMXY(i,j)` is then inserted. Thus the current row length is
!>
!> \[
!> n_j = NROWST(j+1)-NROWST(j),
!> \]
!>
!> and the maximum row width retained as module state for workspace sizing is
!>
!> \[
!> MAX\_ROW\_WIDTH = \max_j n_j,
!> \]
!>
!> evaluated by [[oc_row_width:MAX_ACTIVE_ROW_WIDTH]] once every row start,
!> including the end-of-last-row marker `NROWST(NY+1)`, has been written.
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NLFEE >= max(total_no_links,1)` | Link-indexed arrays cover the active link set. |
!> | `NXEE >= max(NX,1)` and `NY >= 1` | Grid-indexed arrays cover the active grid. |
!> | `LINKNO` on west and south faces is at most `total_no_links` | Link lookup stays within the defined `ICMBK` extent. |
!> | `1 <= ICMBK(1:total_no_links,1:2) <= total_no_elements` when banks are active | Bank elements can be indexed in `NELIND`/`NROWEL`. |
!> | Active grid elements, active links, and optional bank elements partition `1:total_no_elements` | Every OC element appears exactly once in row order. |
   SUBROUTINE OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)

      IMPLICIT NONE

      ! Arguments
      LOGICAL, INTENT(IN)  :: BEXBK        !! True when explicit bank elements are inserted beside their links.
      INTEGER, INTENT(OUT) :: NROWF        !! First non-empty row number.
      INTEGER, INTENT(OUT) :: NROWL        !! Last non-empty row number.
      INTEGER, INTENT(OUT) :: NROWST(NY + 1) !! Row-start pointer into `NROWEL`.
      INTEGER, INTENT(OUT) :: NELIND(:)    !! Position of each element within its row.
      INTEGER, INTENT(OUT) :: NROWEL(:)    !! Contiguous list of elements in row order.

      ! Locals
      INTEGER :: BANK, FACE, I, ICOUNT, IELv, J, K, LINK

      !----------------------------------------------------------------------*

      ! Initialize counters
      NROWF = 0
      NROWL = 0
      K = 0

      ! LOOP OVER BASIC GRID SYSTEM
      ! - LOOP OVER EACH ROW

      row_loop: DO J = 1, NY
         NROWST(J) = K + 1
         IF (K == 0) NROWF = J

         ! ---- LOOP OVER EACH GRID SQUARE IN ROW
         ICOUNT = 0

         col_loop: DO I = 1, NX

            ! ------- Loop over west & south faces
            face_loop: DO FACE = 3, 4

               ! ---------- Test for link at face of grid
               LINK = LINKNO(I, J, FACE == 3)

               IF (LINK > 0) THEN
                  IF (BEXBK) THEN
                     BANK = ICMBK(LINK, 5 - FACE)
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = BANK
                     NELIND(BANK) = ICOUNT
                  END IF

                  K = K + 1
                  ICOUNT = ICOUNT + 1
                  NROWEL(K) = LINK
                  NELIND(LINK) = ICOUNT

                  IF (BEXBK) THEN
                     BANK = ICMBK(LINK, FACE - 2)
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = BANK
                     NELIND(BANK) = ICOUNT
                  END IF
               END IF

               ! ---------- Test for active grid square
               IF (FACE == 3) THEN
                  IELv = ICMXY(I, J)
                  IF (IELv > 0) THEN
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = IELv
                     NELIND(IELv) = ICOUNT
                  END IF
               END IF

            END DO face_loop
         END DO col_loop

         ! ---- Next row
         IF (ICOUNT > 0) NROWL = J

      END DO row_loop

      ! - This marks the end of the last row (+1)
      ! Modern Fix: Explicitly use NY + 1 instead of relying on the leaked loop variable 'J'
      NROWST(NY + 1) = K + 1

      ! Every row start, including the end marker just written, is now known,
      ! so the widest row follows from the pointer differences.
      MAX_SOLVER_ROW_WIDTH = MAX_ACTIVE_ROW_WIDTH(NROWST)

      ! The row solver is allocated from the active topology after this call.
      IF (MAX_SOLVER_ROW_WIDTH <= 0) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 1006, FID_logfile, 0, 0, 'OC topology contains no active solver row')
      END IF

   END SUBROUTINE OCIND

!> @brief Returns the channel link number at a grid coordinate and orientation.
!>
!> `LINKNO` searches the link reference table for a north-south or east-west
!> link whose stored grid coordinate matches the requested `(I,J)` location.
!> The orientation argument is compared directly with `LINKNS`; no geometric
!> inference is made here. If there are no links, or no matching link is
!> found, the function returns zero.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04 | SvB | - | Marked the function `PURE` and replaced the legacy `iscycle`-flag loop with a direct `EXIT`, without changing its search order or result. |
!> @endhistory
   PURE INTEGER FUNCTION LINKNO(I, J, NSOUTH)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: I      !! Grid x-coordinate.
      INTEGER, INTENT(IN) :: J      !! Grid y-coordinate.
      LOGICAL, INTENT(IN) :: NSOUTH !! True to match a north-south link; false for east-west.

      ! Locals
      INTEGER :: L

      !----------------------------------------------------------------------*

      LINKNO = 0

      IF (total_no_links == 0) RETURN

      ! High-Performance Fix: Replaced 'iscycle' AD-hack with a direct EXIT
      ! to immediately terminate the loop once the correct link is found.
      search_loop: DO L = 1, total_no_links

         ! Integer comparison first for fast short-circuiting
         IF (ICMREF(L, 2) == I .AND. ICMREF(L, 3) == J) THEN

            ! Logical equivalence check
            IF (NSOUTH .EQV. LINKNS(L)) THEN
               LINKNO = L
               EXIT search_loop
            END IF

         END IF

      END DO search_loop

   END FUNCTION LINKNO

END MODULE oc_indexing

