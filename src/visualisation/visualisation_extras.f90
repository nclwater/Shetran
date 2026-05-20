!> summary: Auxiliary visualisation arrays for dynamically sized sediment output.
!>
!> This module stores auxiliary pointer arrays used by the visualisation output
!> path. `react` either allocates the arrays for the first use or extends them
!> when additional sediment/output columns are required.
MODULE VISUALISATION_EXTRAS
IMPLICIT NONE

INTEGER, DIMENSION(:), POINTER               :: acol
DOUBLE PRECISION, DIMENSION(:,:,:), POINTER  :: vpsed

PRIVATE
PUBLIC :: REACT, acol, vpsed

CONTAINS

!> Allocates or extends visualisation sediment helper arrays.
SUBROUTINE react(p, j)
INTEGER, INTENT(IN)           :: p !! Required number of output columns.
INTEGER, INTENT(IN), OPTIONAL :: j !! First dimension for `vpsed` during initial allocation.
INTEGER                       :: n
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
INTEGER, DIMENSION(:), POINTER :: s,old=>NULL()
INTEGER, INTENT(IN)            :: n
INTEGER                        :: sz
IF(ASSOCIATED(s)) THEN ; sz=SIZE(s) ; old=>s ; NULLIFY(s) ; ELSE ; sz=0 ; ENDIF
ALLOCATE(s(sz+n))
IF(sz>0) THEN ; s(1:sz)=old ; DEALLOCATE(old) ; ENDIF
END SUBROUTINE increment_I1

!> Extends the third dimension of a rank-three double-precision pointer array.
SUBROUTINE increment_D3(s,n)
DOUBLE PRECISION, DIMENSION(:,:,:), POINTER :: s,old=>NULL()
INTEGER, INTENT(IN)                         :: n
INTEGER                                     :: sh(3)
sh=SHAPE(s)
old=>s
NULLIFY(s)
ALLOCATE(s(sh(1),sh(2),sh(3)+n))
s(:,:,1:sh(3))=old
DEALLOCATE(old)
END SUBROUTINE increment_D3

END MODULE VISUALISATION_EXTRAS
