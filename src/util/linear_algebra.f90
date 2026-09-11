!> summary: Matrix and vector primitives: copy, tridiagonal solve, products, inversion.
!> author: J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> The numerical primitives the solvers share: [[TRIDAG]] for the tridiagonal
!> systems that every column solver forms, [[invertmat]] with its `lubksb`/
!> `ludcmp` pair, the two matrix products, and [[dcopy]]. `eps` is the
!> singularity tolerance used by the inversion and stays here with it.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of utilsmod; see docs/rename/proposal.md. |
!> | 2026-09-11 | SvB | - | Dropped the shared `msg` buffer; [[datetime]] now uses a local one, which removes the dependency on this module (D9, D17). |
!> @endhistory
MODULE linear_algebra

   USE float_compare, ONLY: iszero, notzero
   USE mod_parameters, ONLY: one, zero

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: TRIDAG, DCOPY, JEMATMUL_VM, JEMATMUL_MM, INVERTMAT

   DOUBLEPRECISION, PARAMETER :: eps = 1.0d-15 !! Singularity/zero tolerance used by matrix inversion.

CONTAINS

   !> Copies a double-precision vector into another vector.
   !>
   !> This is the BLAS `dcopy` operation implemented locally, including support
   !> for non-unit and negative increments.
   !>
   !> @note The routine follows the simple BLAS indexing convention but does not
   !> validate `incx` or `incy`. Zero increments and overlapping source/destination
   !> storage are therefore caller responsibilities.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-13 | SvB | Corrected the `n<-0` typo to `n<=0` (behaviour-preserving for `n=0`, since both the array-slice and strided branches already reduce to zero-trip no-ops); changed `dy`'s intent from `OUT` to `INOUT`, since an `OUT` array can be copied back from an uninitialised compiler temporary and overwrite elements skipped by a non-unit stride. |
   !> @endhistory
   PURE SUBROUTINE dcopy(n, dx, incx, dy, incy)
      !----------------------------------------------------------------------*
      !     copies vector x to vector y
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: n    !! Number of values to copy.
      INTEGER, INTENT(IN) :: incx !! Increment between values in `dx`.
      INTEGER, INTENT(IN) :: incy !! Increment between values in `dy`.
      DOUBLE PRECISION, DIMENSION(*), INTENT(IN) :: dx !! Source vector.

      ! Input/Output arguments
      ! Modernization Fix: MUST be INOUT. If incy > 1, an OUT declaration
      ! would destroy the interleaved elements that are skipped by the stride!
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: dy !! Destination vector.

      ! Locals
      INTEGER :: i, ix, iy

      !----------------------------------------------------------------------*

      ! Modernization Fix: Corrected strange 'n<-0' syntax to standard <= 0
      IF (n <= 0) THEN
         RETURN
      ELSE IF (incx == 1 .AND. incy == 1) THEN
         dy(1:n) = dx(1:n)
      ELSE
         ix = 1
         iy = 1
         IF (incx < 0) ix = (-n + 1)*incx + 1
         IF (incy < 0) iy = (-n + 1)*incy + 1

         DO i = 1, n
            dy(iy) = dx(ix)
            ix = ix + incx
            iy = iy + incy
         END DO
      END IF

   END SUBROUTINE dcopy

   !> Multiplies two dense matrices using explicit loops.
   !>
   !> With the declared storage, the returned array satisfies
   !>
   !> \[
   !>   A(i,j)=\sum_{k=1}^{n2} C(i,k)\,B(k,j),
   !> \]
   !>
   !> for `A(n3,n1)`, `B(n2,n1)`, and `C(n3,n2)`. In conventional matrix
   !> notation this is `A = C * B`, despite the old inline comment `A = B * C`.
   !>
   !> @note The local `ZERO` parameter shadows the identical module-wide `ZERO`
   !> constant brought in from [[mod_parameters]]; the added declaration is redundant
   !> (both equal `0.0D0`) but harmless. [[jematmul_vm]] below still relies on
   !> the module-wide constant directly.
   !> @endnote
   PURE FUNCTION jematmul_mm(b, c, n1, n2, n3) RESULT(a)
      !----------------------------------------------------------------------*
      ! A = B * C  (Note: Indexing implies A(i,j) = sum(B(k,j)*C(i,k)))
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n1 !! Number of columns in the returned matrix.
      INTEGER, INTENT(IN) :: n2 !! Shared inner dimension.
      INTEGER, INTENT(IN) :: n3 !! Number of rows in the returned matrix.
      DOUBLE PRECISION, INTENT(IN) :: b(n2, n1) !! Right-hand matrix in declared storage.
      DOUBLE PRECISION, INTENT(IN) :: c(n3, n2) !! Left-hand matrix in declared storage.
      DOUBLE PRECISION :: a(n3, n1)              !! Matrix product `C * B`.

      INTEGER :: i, j, k

      ! Modernization Fix: ZERO was undeclared
      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0

      DO i = 1, n3
         DO j = 1, n1
            a(i, j) = ZERO
            DO k = 1, n2
               a(i, j) = a(i, j) + b(k, j)*c(i, k)
            END DO
         END DO
      END DO

   END FUNCTION jematmul_mm

   !> Multiplies a dense matrix by a vector using explicit loops.
   !>
   !> The returned vector satisfies
   !>
   !> \[
   !>   A_i=\sum_{k=1}^{n2} B(k,i)\,C_k,
   !> \]
   !>
   !> so the declared `B(n2,n1)` is used as the transpose of the conventional
   !> `n1 x n2` matrix.
   PURE FUNCTION jematmul_vm(b, c, n1, n2) RESULT(a)
      !----------------------------------------------------------------------*
      ! A = B * C
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n1 !! Length of the returned vector.
      INTEGER, INTENT(IN) :: n2 !! Shared inner dimension.
      DOUBLE PRECISION, INTENT(IN) :: b(n2, n1) !! Matrix stored transposed relative to conventional notation.
      DOUBLE PRECISION, INTENT(IN) :: c(n2)     !! Input vector.
      DOUBLE PRECISION :: a(n1)                  !! Matrix-vector product.

      INTEGER :: i, k

      DO i = 1, n1
         a(i) = ZERO
         DO k = 1, n2
            a(i) = a(i) + b(k, i)*c(k)
         END DO
      END DO

   END FUNCTION jematmul_vm

   !> Solves a tridiagonal linear system.
   !>
   !> This is the Thomas algorithm for a tridiagonal matrix with lower diagonal
   !> `A`, diagonal `B`, upper diagonal `C`, right-hand side `R`, and solution `U`.
   !> It solves for the vector `U` of length `N` in
   !>
   !> \[
   !> A_i U_{i-1} + B_i U_i + C_i U_{i+1} = R_i,
   !> \qquad i=1,\ldots,N,
   !> \]
   !>
   !> with the usual endpoint interpretation that `A(1)` and `C(N)` are not used.
   !> The routine performs a forward elimination followed by back substitution,
   !> overwriting only the output vector `U` and local work array `GAM`.
   !>
   !> @note No pivoting or zero-pivot protection is performed. `B(1)` and every
   !> subsequent reduced diagonal `BET` must be non-zero.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-06 | SvB | Made the routine `PURE`. |
   !> | 2026-04-06 | SvB | Changed `A`, `B`, `C`, `R`, and `U` from assumed-shape (`(:)`) to explicit-shape (`(N)`) arguments, guaranteeing no copy-in/copy-out overhead for non-contiguous actual arguments. |
   !> @endhistory
   PURE SUBROUTINE TRIDAG(A, B, C, R, U, N)
      !----------------------------------------------------------------------*
      !                            SOLVES FOR VECTOR U OF LENGTH N
      !                            THE TRIDIAGONAL SET A,B,C WHERE
      !                            R IS THE R.H.S.
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN)             :: N !! Number of equations.
      ! Explicit-shape arrays guarantee zero copy-in/copy-out overhead
      DOUBLE PRECISION, INTENT(IN)    :: A(N) !! Lower diagonal; `A(1)` is not used.
      DOUBLE PRECISION, INTENT(IN)    :: B(N) !! Main diagonal.
      DOUBLE PRECISION, INTENT(IN)    :: C(N) !! Upper diagonal; `C(N)` is not used.
      DOUBLE PRECISION, INTENT(IN)    :: R(N) !! Right-hand-side vector.
      DOUBLE PRECISION, INTENT(INOUT) :: U(N) !! Solution vector.

      ! Locals
      INTEGER :: J
      DOUBLE PRECISION :: GAM(N), BET, OOBET

      BET = B(1)
      OOBET = 1.0d0/BET
      U(1) = OOBET*R(1)

      forward_sweep: DO J = 2, N
         GAM(J) = OOBET*C(J - 1)
         BET = B(J) - A(J)*GAM(J)
         OOBET = 1.0d0/BET
         U(J) = OOBET*(R(J) - A(J)*U(J - 1))
      END DO forward_sweep

      backward_sweep: DO J = N - 1, 1, -1
         U(J) = U(J) - GAM(J + 1)*U(J + 1)
      END DO backward_sweep

   END SUBROUTINE TRIDAG

   !> Inverts a dense matrix in place using LU decomposition.
   !>
   !> `invertmat` replaces the input matrix `A` by `A^{-1}`. For `N=1` it returns
   !> the scalar reciprocal directly. For `N>1` it forms the identity matrix,
   !> factors `A` with [[ludcmp]], and solves
   !>
   !> \[
   !> A x_j = e_j,\qquad j=1,\ldots,N,
   !> \]
   !>
   !> with [[lubksb]] for each identity-column right-hand side \(e_j\). The solved
   !> columns \(x_j\) are then copied back into `A`, giving
   !>
   !> \[
   !> A^{-1} = [x_1\;x_2\;\cdots\;x_N].
   !> \]
   !>
   !> `ICOD=0` indicates success. `ICOD=1` indicates an invalid size, a zero
   !> scalar, or a singular matrix detected by the LU factorisation.
   !>
   !> @note For `N > 1`, the input matrix is passed directly to [[ludcmp]] and is
   !> overwritten by the LU factors before singularity status is known. If
   !> `ICOD=1` is returned after factorisation, `A` should not be assumed to retain
   !> the original matrix.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-13 | SvB | Made the routine `PURE`; removed the local `ret` flag, which was set on the `N=1` branch but never read anywhere (the original `N<1`/`N=1`/else structure already dispatched correctly without it). |
   !> @endhistory
   PURE SUBROUTINE invertmat(a, n, icod)
      !----------------------------------------------------------------------*
      ! Inverts a square matrix 'a' of size 'n' using LU decomposition.
      ! Returns icod = 0 (success) or icod = 1 (singular/failure).
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: n !! Matrix order.

      ! Output arguments
      INTEGER, INTENT(OUT) :: icod !! Status code: `0` success, `1` failure.

      ! Input/Output arguments
      DOUBLE PRECISION, DIMENSION(n, n), INTENT(INOUT) :: a !! Matrix to replace with its inverse.

      ! Locals
      INTEGER :: i, j
      INTEGER :: indx(n)
      DOUBLE PRECISION, DIMENSION(n, n) :: y
      DOUBLE PRECISION :: d
      LOGICAL :: issing

      !----------------------------------------------------------------------*

      icod = 0

      IF (n < 1) THEN
         icod = 1

      ELSE IF (n == 1) THEN
         IF (ABS(a(1, 1)) <= EPS) THEN
            icod = 1
         ELSE
            a(1, 1) = ONE/a(1, 1)
         END IF

      ELSE
         ! Initialize 'y' as the identity matrix
         y = ZERO
         DO i = 1, n
            y(i, i) = ONE
         END DO

         ! Perform LU Decomposition
         CALL LUDCMP(a, n, indx, d, issing)

         IF (issing) THEN
            icod = 1
         ELSE
            ! Back-substitute against each column of the identity matrix
            DO j = 1, n
               CALL LUBKSB(a, n, indx, y(:, j))
            END DO

            ! The array 'y' now contains the inverse; copy it back to 'a'
            a = y
         END IF

      END IF

   END SUBROUTINE invertmat

   !> Solves an LU-decomposed linear system by back substitution.
   !>
   !> `lubksb` applies the row permutation stored in `indx` and overwrites `b` with
   !> the solution vector for the matrix factors produced by [[ludcmp]]. This is
   !> the Numerical Recipes LU back-substitution algorithm used by [[invertmat]].
   !>
   !> After [[ludcmp]], the array `A` stores the combined lower and upper
   !> triangular factors of a pivoted decomposition
   !>
   !> \[
   !> P A_{orig} = L U,
   !> \]
   !>
   !> where `L` has an implicit unit diagonal and `U` is stored on and above the
   !> diagonal. `lubksb` solves
   !>
   !> \[
   !> L y = P b,\qquad U x = y,
   !> \]
   !>
   !> by forward substitution followed by back substitution, returning `x` in
   !> `b`. The `ii` marker skips leading zero terms in the permuted right-hand
   !> side, matching the Numerical Recipes implementation.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-03 | SvB | Replaced the labelled inner-product loops with `DOT_PRODUCT` over array sections. |
   !> @endhistory
   PURE SUBROUTINE lubksb(a, n, indx, b)
      !----------------------------------------------------------------------*
      ! Solves the linear system A*x = b using LU Decomposition.
      ! 'a' is the LU-decomposed matrix output from 'ludcmp'.
      ! 'indx' is the row permutation vector output from 'ludcmp'.
      ! 'b' is the right-hand side vector on input, and contains the
      !     solution vector 'x' on output.
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)             :: n       !! Matrix order.
      INTEGER, INTENT(IN)             :: indx(n) !! Pivot-row indices from `ludcmp`.
      DOUBLE PRECISION, INTENT(IN)    :: a(n, n)  !! Combined LU factors from `ludcmp`.
      DOUBLE PRECISION, INTENT(INOUT) :: b(n)    !! Right-hand side on entry; solution on exit.

      ! Local Variables
      INTEGER                         :: i, ii, ll
      DOUBLE PRECISION                :: asum

      !----------------------------------------------------------------------*

      ii = 0

      ! 1. Forward Substitution (Solving L*y = b)
      forward_sub: DO i = 1, n
         ll = indx(i)
         asum = b(ll)
         b(ll) = b(i)

         IF (ii /= 0) THEN
            ! Replaced inner j loop with DOT_PRODUCT
            asum = asum - DOT_PRODUCT(a(i, ii:i - 1), b(ii:i - 1))
         ELSE IF (NOTZERO(asum)) THEN
            ! Optimization: Record the first non-zero element to
            ! avoid doing math on a bunch of leading zeros.
            ii = i
         END IF

         b(i) = asum
      END DO forward_sub

      ! 2. Backward Substitution (Solving U*x = y)
      backward_sub: DO i = n, 1, -1
         ! Replaced inner j loop with DOT_PRODUCT
         ! Note: when i=n, the slice i+1:n is empty, so DOT_PRODUCT safely returns 0.0
         asum = b(i) - DOT_PRODUCT(a(i, i + 1:n), b(i + 1:n))

         b(i) = asum/a(i, i)
      END DO backward_sub

   END SUBROUTINE lubksb

   !> Performs LU decomposition with partial pivoting.
   !>
   !> `ludcmp` factors `a` in place, records pivot rows in `indx`, returns the
   !> parity factor `d`, and sets `issing` when the matrix is singular or has a
   !> zero scaling row. The factorisation is used by [[invertmat]] before
   !> [[lubksb]] solves each right-hand side.
   !>
   !> The decomposition is a scaled partial-pivoting LU factorisation. For each
   !> row, the scaling value
   !>
   !> \[
   !> v_i = \frac{1}{\max_j |a_{ij}|}
   !> \]
   !>
   !> is used to choose the pivot row that maximises \(v_i |a_{ij}|\) in the
   !> current column. Row swaps are recorded in `INDX`, and each swap changes the
   !> sign of `D`. On successful return, `A` stores `L` below the diagonal and `U`
   !> on and above the diagonal:
   !>
   !> \[
   !> P A_{orig} = L U.
   !> \]
   !>
   !> If a row has zero scale the matrix is singular and `ISSING` is set. If a
   !> selected pivot is exactly zero after elimination, the routine substitutes the
   !> small value `TINY=1.0d-20`, preserving the legacy Numerical Recipes behaviour.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-03 | SvB | Replaced the labelled inner-product loops with `DOT_PRODUCT` over array sections, `MAXVAL` for the row-scaling search, and whole-row array slices for pivot swapping. |
   !> @endhistory
   PURE SUBROUTINE ludcmp(a, n, indx, d, issing)
      !----------------------------------------------------------------------*
      ! Performs LU Decomposition on matrix 'a' using partial pivoting.
      ! 'a' is replaced by its LU decomposition.
      ! 'indx' records the row permutations.
      ! 'd' outputs +1 or -1 depending on whether row swaps were even or odd.
      ! 'issing' is flagged .TRUE. if the matrix is singular.
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)             :: n       !! Matrix order.
      DOUBLE PRECISION, INTENT(INOUT) :: a(n, n)  !! Matrix overwritten by combined LU factors.
      INTEGER, INTENT(OUT)            :: indx(n) !! Pivot-row index for each column.
      DOUBLE PRECISION, INTENT(OUT)   :: d       !! Pivot-parity factor.
      LOGICAL, INTENT(OUT)            :: issing  !! True if a zero scaling row marks the matrix singular.

      ! Local Variables
      INTEGER                         :: i, imax, j
      DOUBLE PRECISION                :: aamax, dum, vv(n), dum_row(n)
      DOUBLE PRECISION, PARAMETER     :: TINY = 1.0D-20

      !----------------------------------------------------------------------*

      issing = .FALSE.
      d = 1.0D0

      ! 1. Calculate implicit scaling information for each row
      DO i = 1, n
         aamax = MAXVAL(ABS(a(i, :)))

         IF (ISZERO(aamax)) THEN
            issing = .TRUE.
            RETURN ! Singular matrix, exit immediately
         END IF

         vv(i) = 1.0D0/aamax
      END DO

      ! Crout's Algorithm
      outer_col_loop: DO j = 1, n

         ! 2. Upper triangular part
         upper_loop: DO i = 1, j - 1
            a(i, j) = a(i, j) - DOT_PRODUCT(a(i, 1:i - 1), a(1:i - 1, j))
         END DO upper_loop

         aamax = 0.0D0
         imax = j

         ! 3. Lower triangular part and pivot search
         lower_loop: DO i = j, n
            a(i, j) = a(i, j) - DOT_PRODUCT(a(i, 1:j - 1), a(1:j - 1, j))

            dum = vv(i)*ABS(a(i, j))
            IF (dum >= aamax) THEN
               imax = i
               aamax = dum
            END IF
         END DO lower_loop

         ! Row swapping (Pivoting)
         IF (j /= imax) THEN
            ! 4. Whole-array row slices for rapid memory swapping
            dum_row(:) = a(imax, :)
            a(imax, :) = a(j, :)
            a(j, :) = dum_row(:)

            d = -d
            vv(imax) = vv(j)
         END IF

         indx(j) = imax

         IF (ISZERO(a(j, j))) a(j, j) = TINY

         ! 5. Direct column scaling
         IF (j /= n) THEN
            dum = 1.0D0/a(j, j)
            a(j + 1:n, j) = a(j + 1:n, j)*dum
         END IF

      END DO outer_col_loop

   END SUBROUTINE ludcmp

END MODULE linear_algebra

