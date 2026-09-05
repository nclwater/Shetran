!> summary: Legacy pointer buffers for an auxiliary visualisation interface.
!>
!> `VISUALISATION_EXTRAS` owns two module-level pointer arrays and exposes
!> [[react]] to allocate or enlarge them. The arrays were originally exported
!> through Intel `DLLEXPORT` directives, consistent with an external
!> visualisation interface. Those directives have since been removed. The
!> current source tree contains no `USE VISUALISATION_EXTRAS` statement or
!> reference to any of its public symbols, although CMake still compiles the
!> file into the SHETRAN executable through recursive source discovery.
!>
!> | Public symbol | Allocated shape | Stored state |
!> |:--------------|:----------------|:-------------|
!> | `acol` | `(capacity)` | Legacy integer column/index buffer. |
!> | `vpsed` | `(j, 2, capacity)` | Legacy double-precision sediment-value buffer. |
!> | `react` | - | Allocates both buffers or grows their final extent together. |
!>
!> The two buffer names and the old documentation indicate their intended
!> visualisation role; with no current consumer, the meaning of each index and
!> the units of `vpsed` cannot be verified from executable code. Neither array
!> is initialized after allocation, and the module provides no deallocation
!> routine. Because the pointers themselves are public, an external consumer
!> could also change their association or contents without using `react`.
!>
!> @warning
!> Growth deallocates the old pointer targets. Any additional pointer aliases
!> to those targets consequently acquire undefined association status. Calling
!> `react` with `j` present more than once allocates new targets without first
!> deallocating the old ones, losing the module's references to the old storage
!> unless another alias retains them.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-09-08 | SB | - | Added the module with Intel `DLLEXPORT` directives for `acol`, `vpsed`, and `react`. |
!> | 2026-04-04 | SvB | - | Applied the project-wide Fortran formatting pass without changing behavior. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the legacy Intel export directives during the IFX compiler update. |
!> @endhistory
MODULE VISUALISATION_EXTRAS

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE MOD_ERROR, ONLY: errstat_alloc, errstat_dealloc

   IMPLICIT NONE

   INTEGER, DIMENSION(:), POINTER              :: acol  !! Public legacy integer buffer; allocated and resized by [[react]].
   DOUBLE PRECISION, DIMENSION(:, :, :), POINTER :: vpsed !! Public legacy sediment buffer; final extent tracks `SIZE(acol)`.

   PRIVATE
   PUBLIC :: REACT, acol, vpsed

CONTAINS

!> Allocates or conditionally grows the two module buffers.
!>
!> The optional argument selects between initialization and growth:
!>
!> | Call state | Action |
!> |:-----------|:-------|
!> | `j` present | Allocates `acol(p)` and `vpsed(j,2,p)` without inspecting their existing association. |
!> | `j` absent and `p <= SIZE(acol)` | Returns without changing either buffer or checking `vpsed`. |
!> | `j` absent and `p > SIZE(acol)` | Adds a roughly ten-percent increment to both final extents. |
!>
!> Growth therefore preserves the existing values but leaves the appended
!> entries undefined. With `j` absent, `acol` must be associated. If growth is
!> required, `vpsed` must also be associated and its third extent should equal
!> `SIZE(acol)` so that the two capacities remain synchronized.
!>
!> @warning
!> A growth call performs only one approximately ten-percent increment. It
!> guarantees room for a sequential request `p=SIZE(acol)+1`, but a larger jump
!> can still leave the new capacity smaller than `p`. No postcondition check is
!> made. Allocation errors are also unhandled.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-09-08 | SB | - | Added the allocation/growth routine with an Intel `DLLEXPORT` directive. |
!> | 2026-04-08 | SB | 4.6.1 | Removed the compiler-specific export directive; allocation behavior was unchanged. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE react(p, j)
      INTEGER, INTENT(IN)           :: p !! Capacity threshold, or initial capacity when `j` is present.
      INTEGER, INTENT(IN), OPTIONAL :: j !! Initial first extent of `vpsed`; its presence selects allocation rather than growth.
      INTEGER                       :: n !! Existing capacity, then the positive increment used by both grow helpers.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_EXTRAS:react"

      IF (PRESENT(j)) THEN
         ALLOCATE (acol(p), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "acol", location, emsg)
         ALLOCATE (vpsed(j, 2, p), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "vpsed", location, emsg)
      ELSE
         n = SIZE(acol)
         IF (p > n) THEN
            n = MAX(1, n/10)
            CALL INCREMENT_I1(acol, n)
            CALL INCREMENT_D3(vpsed, n)
         END IF
      END IF
   END SUBROUTINE react

!> Reallocates an integer pointer with `n` additional elements.
!>
!> If `s` is associated, the routine saves its target through `old`, allocates
!> a replacement of size `SIZE(s)+n`, copies the old values into the leading
!> section, and deallocates the old target. If `s` is disassociated, it simply
!> allocates `s(n)`. The appended elements are not initialized.
!>
!> This is a private helper currently called only by [[react]], which supplies
!> a positive `n`. Other aliases to a replaced target become undefined when
!> `old` is deallocated. An associated zero-sized input is a special unchecked
!> case: the `sz>0` guard skips both the copy and `DEALLOCATE(old)`, so its old
!> zero-sized target is not released.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-09-08 | SB | - | Added the rank-one pointer-growth helper. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE increment_I1(s, n)
      INTEGER, DIMENSION(:), POINTER :: s           !! Integer pointer to grow; existing positive-size values are preserved.
      INTEGER, DIMENSION(:), POINTER :: old => NULL() !! Saved alias to the old target during reallocation.
      INTEGER, INTENT(IN)            :: n           !! Number of elements appended by the current caller.
      INTEGER                        :: sz          !! Original element count, or zero for a disassociated pointer.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_EXTRAS:increment_I1"

      IF (ASSOCIATED(s)) THEN; sz = SIZE(s); old => s; NULLIFY (s); ELSE; sz = 0; END IF
      ALLOCATE (s(sz + n), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "s", location, emsg)
      IF (sz > 0) THEN; s(1:sz) = old; DEALLOCATE (old); END IF
   END SUBROUTINE increment_I1

!> Reallocates an associated rank-three pointer with a longer final extent.
!>
!> The replacement shape is
!> `(SIZE(s,1), SIZE(s,2), SIZE(s,3)+n)`. Existing values are copied into its
!> leading third-dimension section, the old target is deallocated, and the new
!> final-dimension entries remain undefined. `s` must be associated before the
!> call because the routine evaluates `SHAPE(s)` unconditionally.
!>
!> This private helper is called only by [[react]], which supplies a positive
!> `n`. Deallocating `old` gives any other aliases to the former target
!> undefined association status.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-09-08 | SB | - | Added the rank-three pointer-growth helper. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE increment_D3(s, n)
      DOUBLE PRECISION, DIMENSION(:, :, :), POINTER :: s           !! Pointer whose shape and existing values are preserved.
      DOUBLE PRECISION, DIMENSION(:, :, :), POINTER :: old => NULL() !! Saved alias to the old target during reallocation.
      INTEGER, INTENT(IN)                         :: n           !! Number of entries appended to the third extent.
      INTEGER                                     :: sh(3)       !! Original three-dimensional shape.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_EXTRAS:increment_D3"

      sh = SHAPE(s)
      old => s
      NULLIFY (s)
      ALLOCATE (s(sh(1), sh(2), sh(3) + n), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "s", location, emsg)
      s(:, :, 1:sh(3)) = old
      DEALLOCATE (old, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "old", location, emsg)
   END SUBROUTINE increment_D3

END MODULE VISUALISATION_EXTRAS
