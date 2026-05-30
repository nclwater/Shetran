!> summary: Auxiliary visualisation arrays for dynamically sized sediment output.
!>
!> This legacy helper owns two dynamically sized pointer buffers for
!> visualisation sediment output. No current source file references this module,
!> but it is retained as part of the visualisation support code.
!>
!> Buffer layout:
!>
!> | Symbol | Shape | Meaning |
!> |:-------|:------|:--------|
!> | `acol` | `(p)` | Active output-column indices. |
!> | `vpsed` | `(j,2,p)` | Sediment values by row/item, side, and output column. |
MODULE VISUALISATION_EXTRAS
   IMPLICIT NONE

   INTEGER, DIMENSION(:), POINTER               :: acol  !! Active output-column indices.
   DOUBLE PRECISION, DIMENSION(:,:,:), POINTER  :: vpsed !! Sediment value buffer indexed as `(j,2,p)`.

   PRIVATE
   PUBLIC :: REACT, acol, vpsed

CONTAINS

!> Allocates or extends visualisation sediment helper arrays.
!>
!> If `j` is present, the routine performs the first allocation with
!> `SIZE(acol)=p` and `SHAPE(vpsed)=[j,2,p]`. If `j` is absent, `acol` and
!> `vpsed` must already be associated; when `p` exceeds the current column
!> count both buffers are extended by `MAX(1,SIZE(acol)/10)` columns.
   SUBROUTINE react(p, j)
      INTEGER, INTENT(IN)           :: p !! Required output-column count.
      INTEGER, INTENT(IN), OPTIONAL :: j !! First dimension for `vpsed` during first allocation.
      INTEGER                       :: n !! Current or additional column count.
      IF(PRESENT(j)) THEN
         ALLOCATE(acol(p), vpsed(j,2,p))
      ELSE
         n = SIZE(acol)
         IF(p>n) THEN
            n = MAX(1,n/10)
            CALL INCREMENT_I1(acol, n)
            CALL INCREMENT_D3(vpsed, n)
         ENDIF
      ENDIF
   END SUBROUTINE react

!> Extends a rank-one integer pointer array by `n` elements.
   SUBROUTINE increment_I1(s,n)
      INTEGER, DIMENSION(:), POINTER :: s          !! Pointer array to extend.
      INTEGER, DIMENSION(:), POINTER :: old=>NULL() !! Temporary pointer to the old storage.
      INTEGER, INTENT(IN)            :: n          !! Number of elements to append.
      INTEGER                        :: sz         !! Original array size.
      IF(ASSOCIATED(s)) THEN ; sz=SIZE(s) ; old=>s ; NULLIFY(s) ; ELSE ; sz=0 ; ENDIF
      ALLOCATE(s(sz+n))
      IF(sz>0) THEN ; s(1:sz)=old ; DEALLOCATE(old) ; ENDIF
   END SUBROUTINE increment_I1

!> Extends the third dimension of an associated rank-three double-precision pointer array.
   SUBROUTINE increment_D3(s,n)
      DOUBLE PRECISION, DIMENSION(:,:,:), POINTER :: s           !! Pointer array to extend.
      DOUBLE PRECISION, DIMENSION(:,:,:), POINTER :: old=>NULL()  !! Temporary pointer to the old storage.
      INTEGER, INTENT(IN)                         :: n           !! Number of third-dimension entries to append.
      INTEGER                                     :: sh(3)       !! Original array shape.
      sh=SHAPE(s)
      old=>s
      NULLIFY(s)
      ALLOCATE(s(sh(1),sh(2),sh(3)+n))
      s(:,:,1:sh(3))=old
      DEALLOCATE(old)
   END SUBROUTINE increment_D3

END MODULE VISUALISATION_EXTRAS
