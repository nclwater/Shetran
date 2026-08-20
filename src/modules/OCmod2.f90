!> Overland/channel hydraulic flux helper routines.
!>
!> This module contains the hydraulic calculation kernels used by the
!> overland/channel flow component. It stores water-surface elevation and
!> face-discharge arrays behind small accessor routines, builds channel
!> conveyance lookup tables, evaluates grid-grid, link-link, grid-link,
!> bank-link, boundary, confluence, and weir exchange flows, and applies final
!> flow/depth corrections after a timestep.
!>
!> The SHETRAN User Guide and Data Input Manual supplies the OC hydraulic inputs:
!> Strickler roughness through `OC3a`, `OC4`, `OC14`-`OC19`, channel-link
!> cross-section and roughness through `OC30`-`OC36`, and weir or river-plus-weir
!> boundary data through `OC38`-`OC41`. These records provide the coefficients
!> used by the routines here; the formulas in each routine's documentation
!> describe the actual code paths.
!>
!> [[conveyan]] evaluates the conveyance kernels used by overland, channel,
!> bank, and boundary exchange. Away from the near-zero-depth smoothing branch,
!> the area-based channel form used with `ty=0` is
!>
!> \[
!> C = K\,A\,h^{2/3},
!> \]
!>
!> where `K` is the relevant Strickler coefficient, `A` is flow area, and `h` is
!> depth. The depth-width form used with `ty=1` is
!>
!> \[
!> C = (K\,W)\,h^{5/3},
!> \]
!>
!> where `W` has already been folded into the passed roughness-width factor.
!> For \(10^{-9} \le h < 10^{-3}\) m, both forms use a smoothed polynomial
!> branch to avoid singular derivatives near zero depth; below \(10^{-9}\) m
!> conveyance and derivative are zero.
!>
!> Exchange routines then combine conveyance with the water-level difference
!> \(\Delta z\) and flow-path length \(L\):
!>
!> \[
!> Q = C\,\frac{\sqrt{\max(\Delta z,0)}}{\sqrt{L}},
!> \]
!>
!> with signs chosen so paired face/link fluxes are conservative.
!>
!> Weir routines use the manual's `OC38`-`OC41` weir coefficient, sill
!> elevation, downstream water level, and submerged-flow ratio in the
!> horizontal-crest weir solver [[qweir]]. Reservoir/channel links may instead
!> obtain discharge from [[zqmod]] rating tables through `get_ZQTable_value`; in
!> those cases discharge is a tabulated stage-discharge lookup rather than a
!> direct conveyance or weir calculation.
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-1998 | GP/RAH | 3.4.1-4.2 | Reworked OC hydraulic routines, boundary types, confluences, weir handling, and derivative outputs. |
!> | 1999-02 | SB | 4.27 | Adjusted confluence mass conservation and small adverse-flow correction behaviour. |
!> | 2008-12 | JE | 4.3.5F90 | Converted part of the OC `.F` files into this Fortran 90 helper module. |
!> | 2020-05 | SB | - | Added ZQ-table reservoir/channel link support. |
!> | 2012-12-12 | SB | - | Made `XSTAB` dynamically allocatable (see [[initialise_ocmod]]). |
!> | 2026-04-11 | SvB | - | Modernization pass: array-bounds/aliasing safety fixes, `EXIT`-based loop control, and minor performance tidy-ups (see individual routine documentation for details). |
!> | 2026-05-21 | SB | 4.6 | Improved channel-junction convergence (see [[ocnode]]). |
!> @endhistory
MODULE OCmod2
   USE SGLOBAL
   USE ZQmod,     ONLY : get_ZQTable_value
   USE AL_D,      ONLY : ZQweirsill,ZQTableRef
   IMPLICIT NONE

   DOUBLEPRECISION, PARAMETER   :: F23=2.0D0/3.0D0      !! Exponent \(2/3\) used in Strickler conveyance.
   DOUBLEPRECISION, PARAMETER   :: F53=5.0D0/3.0D0      !! Exponent factor \(5/3\) used by the implemented derivative branches.
   DOUBLEPRECISION, PARAMETER   :: DZMIN = 1.0D-3       !! Small depth/head-difference threshold, in metres.
   DOUBLEPRECISION, PARAMETER   :: RDZMIN=3.16227766d-2 !! Square root of `DZMIN`.
   DOUBLEPRECISION, PARAMETER   :: H23MIN=1.0d-2        !! `DZMIN**(2/3)`, retained for legacy comments and comparisons.
   DOUBLEPRECISION, PARAMETER   :: ROOT2G = 4.42944d0   !! Approximation to \(\sqrt{2g}\) for weir flow.
   DOUBLEPRECISION, DIMENSION(NELEE)          :: HRFZZ    !! Water-surface elevation by element; abstracted for AD and solver access.
   DOUBLEPRECISION, DIMENSION(NELEE,4)        :: qsazz    !! Face discharge by element and face; positive into the indexed element.

   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: xstab
   !! Channel lookup table: depth, conveyance, and conveyance slope by row and link.
   !! Allocated once by [[initialise_ocmod]] to shape `(3,NXSCEE,total_no_links)`.

   PRIVATE
   PUBLIC :: GETHRF, SETHRF, GETQSA, SETQSA, GETQSA_ALL, CONVEYAN, OCQBC, OCQMLN, OCQLNK, OCQGRD, OCQBNK, OCFIX, XSTAB, &
      hrfzz, qsazz, OCNODE, initialise_ocmod  !THESE PUBLIC ONLY FOR USE IN AD
CONTAINS

   !> Returns the stored water-surface elevation for an element.
   !>
   !> `HRFZZ` is the module-local storage used to abstract the OC water level
   !> array for automatic-differentiation and solver calls. This is the
   !> read-side accessor for [[sethrf]].
   PURE DOUBLE PRECISION FUNCTION gethrf(i)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: i !! Element index.

      gethrf = hrfzz(i)

   END FUNCTION gethrf

   !> Stores the water-surface elevation for an element.
   !>
   !> This is the write-side accessor for [[gethrf]].
   SUBROUTINE sethrf(i, v)

      IMPLICIT NONE

      INTEGER, INTENT(IN)          :: i !! Element index.
      DOUBLE PRECISION, INTENT(IN) :: v !! Water-surface elevation to store.

      hrfzz(i) = v

   END SUBROUTINE sethrf


   !> Returns the stored face discharge for an element and face.
   !>
   !> `QSAZZ` follows the OC sign convention used by [[ocfix]]: positive
   !> discharge is into the indexed element. This is the read-side accessor
   !> for [[setqsa]].
   PURE DOUBLE PRECISION FUNCTION getqsa(i, j)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: i !! Element index.
      INTEGER, INTENT(IN) :: j !! Face number.

      getqsa = qsazz(i, j)

   END FUNCTION getqsa

   !> Stores the face discharge for an element and face.
   !>
   !> This is the write-side accessor for [[getqsa]].
   SUBROUTINE setqsa(i, j, v)

      IMPLICIT NONE

      INTEGER, INTENT(IN)          :: i !! Element index.
      INTEGER, INTENT(IN)          :: j !! Face number.
      DOUBLE PRECISION, INTENT(IN) :: v !! Face discharge to store; positive into element `i`.

      qsazz(i, j) = v

   END SUBROUTINE setqsa


   !> Returns the stored face-discharge array for the first `n` elements.
   !>
   !> The returned array has shape `(n,4)` and is a value copy of
   !> `QSAZZ(1:n,:)`.
   PURE FUNCTION getqsa_all(n) RESULT(res)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n !! Number of leading elements to return.
      DOUBLE PRECISION, DIMENSION(n, 4) :: res !! Face-discharge copy for elements `1:n`.

      res = qsazz(1:n, :)

   END FUNCTION getqsa_all

   !> Allocates the channel cross-section conveyance lookup table.
   !>
   !> `XSTAB(1:3,1:NXSCEE,1:total_no_links)` stores tabulated channel depth,
   !> conveyance, and conveyance slope for each channel link. The routine
   !> assumes `NXSCEE` and `total_no_links` have already been set by the
   !> frame/OC input processing.
   !>
   !> @warning
   !> `XSTAB` is allocated once with a fixed shape and never resized or
   !> reallocated. The `ALLOCATED` guard makes a repeated call a no-op rather
   !> than an error, but it does not re-size the table for a changed
   !> `total_no_links`.
   !> @endwarning
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 2012-12-12 | SB | - | Made `XSTAB` dynamically allocatable in place of a fixed-size `(3,NXSCEE,NLFEE)` array. |
   !> | 2026-04-11 | SvB | - | Added the `ALLOCATED` guard so a repeated call does not attempt to re-allocate an already-allocated table. |
   !> @endhistory
   SUBROUTINE initialise_ocmod()

      IMPLICIT NONE

      IF (.NOT. ALLOCATED(xstab)) THEN
         ALLOCATE(xstab(3, nxscee, total_no_links))
      END IF

   END SUBROUTINE initialise_ocmod



   !> Solves a multi-link confluence so branch flows sum to zero.
   !>
   !> `OCNODE` finds the junction water level by bracketing and false-position
   !> iteration, using [[fnode]] to evaluate the net flow leaving the node.
   !> After convergence, the largest branch flow is adjusted by the small
   !> residual to enforce local mass conservation at the confluence.
   !>
   !> This is the confluence solve used by the OC channel-link routines after
   !> each active branch has supplied a water level `ZI`, conveyance `CI`,
   !> conveyance derivative `DI`, and square-root path length `ROOTLI`. Absent
   !> branches are flagged by `ROOTLI(j)=0` and assigned zero flow.
   !>
   !> The node elevation \(z_n\) is initially bracketed by the minimum and
   !> maximum active branch water levels:
   !>
   !> \[
   !> A=\min_j ZI_j,\qquad B=\max_j ZI_j.
   !> \]
   !>
   !> For a trial node elevation, [[fnode]] evaluates the branch flow leaving
   !> the node as
   !>
   !> \[
   !> \Delta z_j = z_n-ZI_j,\qquad
   !> C_j^\* = CI_j + DI_j\max(0,\Delta z_j),
   !> \]
   !>
   !> \[
   !> Q_j =
   !> \operatorname{sign}(\Delta z_j)\,
   !> C_j^\*\frac{\sqrt{|\Delta z_j|}}{ROOTLI_j},
   !> \]
   !>
   !> and the confluence residual is
   !>
   !> \[
   !> F(z_n)=\sum_j Q_j.
   !> \]
   !>
   !> The routine applies up to 200 false-position iterations,
   !>
   !> \[
   !> z_n^{new}=\frac{A F(B)-B F(A)}{F(B)-F(A)}.
   !> \]
   !>
   !> If the residual keeps the same sign as the previous residual, the
   !> opposite endpoint residual is halved before the next interpolation step;
   !> this is the legacy damping used to avoid stagnation. The accepted
   !> convergence test is
   !>
   !> \[
   !> |F(z_n)| \le 10^{-3}\sum_j |Q_j|
   !> \quad\text{and}\quad |B-A|\le 10^{-4}\ \mathrm{m}.
   !> \]
   !>
   !> At convergence, the branch with the largest absolute flow is corrected
   !> by the residual,
   !>
   !> \[
   !> Q_{j_{\max}} \leftarrow Q_{j_{\max}}-F(z_n),
   !> \]
   !>
   !> so that the returned branch flows sum exactly to zero to working
   !> precision. If the iteration limit is reached, warning `1027` is issued.
   !> Warning `1028` is also issued when the residual is greater than one
   !> percent of total absolute flow or the bracket is wider than `1D-3`.
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | - | GP | 3.4 | Called `ERROR` and terminated iterations if `NC == 50`; added `ZNODE` argument for [[ocqmln]]. |
   !> | 1998-02-12 | RAH | 4.2 | Supplied missing `PRI`, `FATAL`, and `WARN` for `ERROR`; removed `ZNODE`; added explicit typing; removed local `TESTZ`; added `TEST`; clarified description as flow out of the node; returned immediately if `FA = 0`; tested `NC` before updating bracket endpoints; set `QJ` at absent branches. |
   !> | 1998-02-20 | RAH | 4.2 | Added `IEL` argument for `ERROR` handling from [[ocqmln]]. |
   !> | 1998-03-18 | RAH | 4.2 | Added `DI` argument passed to [[fnode]]. |
   !> | 1999-02-04 | SB | 4.27 | Fixed junction mass conservation by adjusting the largest absolute branch flow so the branch-flow sum is zero. |
   !> | 2026-05-21 | SB | 4.6 | Increased the iteration limit from 50 to 200 and tightened the convergence criteria (residual and bracket-width tolerances) at channel junctions, replacing the `iscycle`-flagged `DO`/`CYCLE` loop with an `EXIT`-based loop. |
   !> @endhistory
   SUBROUTINE OCNODE(IELA, ZI, CI, DI, ROOTLI, QJ)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IELA !! Element number used in confluence warning diagnostics.
      DOUBLE PRECISION, INTENT(IN) :: CI(0:3)     !! Branch conveyance at the current branch water level.
      DOUBLE PRECISION, INTENT(IN) :: DI(0:3)     !! Branch conveyance derivative with respect to water level.
      DOUBLE PRECISION, INTENT(IN) :: ZI(0:3)     !! Water-surface elevation at each branch.
      DOUBLE PRECISION, INTENT(IN) :: ROOTLI(0:3) !! Square root of branch flow length; zero marks an absent branch (branch `J=0` is never absent).

      ! Input/Output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: QJ(0:3) !! Flow from the solved node into each branch; set at absent branches on entry and overwritten at active branches on exit.

      ! Locals
      INTEGER :: J, NC, JMAJOR
      DOUBLE PRECISION :: A, B, FA, FB, FN, FNM1, SIGMAQ, WN
      LOGICAL :: TEST, FAILED

   !----------------------------------------------------------------------*

   ! FIRST GUESSES (CHOOSE VALUES A,B SUCH THAT F(A)*F(B) .le. 0 )
   ! (USE MIN AND MAX OF VALID ELEVATIONS); also, set QJ at absent branches

      A = ZI(0)
      B = A
      
      init_loop: DO J = 1, 3
         IF (ISZERO(ROOTLI(J))) THEN
            QJ(J) = ZERO
         ELSE
            A = MIN(ZI(J), A)
            B = MAX(ZI(J), B)
         END IF
      END DO init_loop
      
      CALL FNODE(A, DI, CI, ZI, ROOTLI, QJ, FA)
      IF (ISZERO(FA)) RETURN
      
      CALL FNODE(B, DI, CI, ZI, ROOTLI, QJ, FB)
      IF (ISZERO(FB)) RETURN

   ! Iterate to convergence, using successive linear interpolation
   
      FN = FA
      FAILED = .FALSE.
      
      ! Increase iteration limit and tighten convergence for difficult junctions.
      iteration_loop: DO NC = 1, 200
         
         WN = (A * FB - B * FA) / (FB - FA)
         FNM1 = FN
         
         CALL FNODE(WN, DI, CI, ZI, ROOTLI, QJ, FN)
         
         SIGMAQ = ABS(QJ(0)) + ABS(QJ(1)) + ABS(QJ(2)) + ABS(QJ(3))
         
         IF (ABS(FN) <= SIGMAQ * 1.0D-3 .AND. ABS(B - A) <= 1.0D-4) THEN
            JMAJOR = 0
            DO J = 1, 3
               IF (ABS(QJ(J)) > ABS(QJ(JMAJOR))) JMAJOR = J
            END DO
            QJ(JMAJOR) = QJ(JMAJOR) - FN
            FAILED = .FALSE.
            EXIT iteration_loop
         END IF
         
         FAILED = .TRUE.

         ! * ... carry on: replace either A or B with WN; and
         ! * adjust interpolation factor if sign of F didn't change
         TEST = GTZERO(FN * FNM1)  ! TAKE CARE - PRECEDENCE
         
         IF (FN * FA >= 0.0D0) THEN
            A = WN
            FA = FN
            IF (TEST) FB = FB * HALF
         ELSE
            B = WN
            FB = FN
            IF (TEST) FA = FA * HALF
         END IF
         
      END DO iteration_loop

      IF (FAILED) THEN
         CALL ERROR(WWWARN, 1027, PPPRI, IELA, 0, 'maximum iterations exceeded for OC confluence')
         IF (ABS(FN) > SIGMAQ * 1.0D-2 .OR. ABS(B - A) > 1.0D-3) THEN
            CALL ERROR(WWWARN, 1028, PPPRI, IELA, 0, 'Bad iteration failure for OC confluence')
         END IF
      END IF

   END SUBROUTINE OCNODE



   !> Evaluates net flow leaving a confluence for a trial node elevation.
   !>
   !> `FNODE` calculates the residual used by [[ocnode]]:
   !>
   !> \[
   !> F(ZNODE)=\sum_{j=0}^{3} Q_j,
   !> \]
   !>
   !> where branches with `ROOTLI(j)=0` are treated as absent and left at
   !> `QJ(j)=0`. For active branches,
   !>
   !> \[
   !> \Delta z_j = ZNODE-ZI_j,\qquad
   !> \sigma_j = \operatorname{sign}(\Delta z_j),
   !> \]
   !>
   !> and the branch conveyance is linearly increased only when the trial node
   !> elevation is above the branch water level:
   !>
   !> \[
   !> C_j^\* = CI_j + DI_j\max(0,\Delta z_j).
   !> \]
   !>
   !> The returned branch flow is
   !>
   !> \[
   !> Q_j =
   !> \sigma_j\,C_j^\*
   !> \frac{\sqrt{\sigma_j\Delta z_j}}{ROOTLI_j}
   !> =
   !> \operatorname{sign}(\Delta z_j)\,C_j^\*
   !> \frac{\sqrt{|\Delta z_j|}}{ROOTLI_j}.
   !> \]
   !>
   !> Positive `QJ(j)` is flow leaving the trial node into branch `j`; negative
   !> values represent flow entering the node from that branch.
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1998-02-11 | RAH | 4.2 | Added explicit typing, generic intrinsics, local flow/residual variables, integer direction handling, and replaced the old `CI2` array with scalar conveyance logic. |
   !> | 1998-02-12 | RAH | 4.2 | Removed local `CI3` and extended the active-branch loop logic. |
   !> | 1998-03-18 | RAH | 4.2 | Set downstream branch conveyance using `ZNODE` and added the `DI` argument passed from [[ocnode]]. |
   !> @endhistory
   PURE SUBROUTINE FNODE(ZNODE, DI, CI, ZI, ROOTLI, QJ, RESFNODE)

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: ZNODE !! Trial node water-surface elevation.
      DOUBLE PRECISION, INTENT(IN) :: DI(0:3)     !! Branch conveyance derivative with respect to water level.
      DOUBLE PRECISION, INTENT(IN) :: CI(0:3)     !! Branch conveyance at the current branch water level.
      DOUBLE PRECISION, INTENT(IN) :: ZI(0:3)     !! Water-surface elevation at each branch.
      DOUBLE PRECISION, INTENT(IN) :: ROOTLI(0:3) !! Square root of branch flow length; zero marks an absent branch.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: QJ(0:3)   !! Flow from the trial node into each branch; defined only where `ROOTLI(J) /= 0`.
      DOUBLE PRECISION, INTENT(OUT) :: RESFNODE  !! Sum of branch flows for the trial node elevation.

      ! Locals
      INTEGER :: J
      DOUBLE PRECISION :: CJ, DZ, QASUM, SIG

   !----------------------------------------------------------------------*

      QASUM = ZERO
      QJ = ZERO
      
      flow_loop: DO J = 0, 3
         IF (ISZERO(ROOTLI(J))) CYCLE flow_loop
         
         DZ = ZNODE - ZI(J)
         SIG = SIGN(ONE, DZ)
         CJ = CI(J) + DI(J) * MAX(ZERO, DZ)
         QJ(J) = SIG * CJ * SQRT(SIG * DZ) / ROOTLI(J)
         QASUM = QJ(J) + QASUM
      END DO flow_loop
      
      RESFNODE = QASUM
      
   END SUBROUTINE FNODE


   
   !> Calculates channel-link conveyance and derivative at a water elevation.
   !>
   !> Below bank-full the routine interpolates precomputed cross-section
   !> tables. Above the table range it extends the cross-sectional area with
   !> top width and evaluates the Gauckler-Manning-Strickler-style conveyance
   !> through [[conveyan]].
   !>
   !> Entry requirements are `Z >= ZG`, positive `STR`, `CWIDTH`, `XAFULL`, and
   !> full-table depth `XS(1,NXSCEE)`, and `NXSCEE >= 1`. For lookup intervals,
   !> the table is assumed to have uniformly spaced depths
   !>
   !> \[
   !> XS(1,i)=XS(1,NXSCEE)\frac{i-1}{NXSCEE-1},
   !> \]
   !>
   !> with non-negative stored conveyance and positive stored conveyance
   !> slope. The routine returns non-negative `CONV` and positive `DERIV` when
   !> these conditions are satisfied.
   !>
   !> The local water depth and full-table depth are
   !>
   !> \[
   !> H=Z-ZG,\qquad H_{full}=XS(1,NXSCEE).
   !> \]
   !>
   !> For \(H < H_{full}\), the table interval is selected by
   !>
   !> \[
   !> i=\left\lfloor \frac{H}{H_{full}}(NXSCEE-1)+1\right\rfloor,
   !> \]
   !>
   !> and the piecewise-linear conveyance is
   !>
   !> \[
   !> DERIV=XS(3,i),\qquad
   !> CONV=XS(2,i)+DERIV\,(H-XS(1,i)).
   !> \]
   !>
   !> For water above the tabulated range, the full-bank area is extended by
   !> the rectangular top width:
   !>
   !> \[
   !> A=XAFULL+CWIDTH\,(H-H_{full}),
   !> \]
   !>
   !> and [[conveyan]] is called with `ty=2`, which evaluates
   !>
   !> \[
   !> CONV=STR\,A\,H^{2/3},
   !> \]
   !>
   !> with derivative
   !>
   !> \[
   !> DERIV=CONV\left(\frac{CWIDTH}{A}+\frac{2}{3H}\right).
   !> \]
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
   !> | 1998-04-23 | RAH | 4.2 | Added explicit typing; moved `ZG` before `Z`; replaced common-block inputs with arguments for roughness, full area, cross-section width, and lookup table; replaced loop search with direct interval calculation; rearranged above-bankfull conveyance/derivative expressions. |
   !> @endhistory
   PURE SUBROUTINE OCCODE(ZG, STR, AFROMCWIDTH, AFROMXAFULL, AFROMXSTYPES, Z, CONV, DERIV)

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: ZG           !! Channel-bed elevation.
      DOUBLE PRECISION, INTENT(IN) :: STR           !! Channel Strickler roughness coefficient.
      DOUBLE PRECISION, INTENT(IN) :: AFROMCWIDTH   !! Channel top width used above the tabulated cross-section.
      DOUBLE PRECISION, INTENT(IN) :: AFROMXAFULL   !! Full-flow cross-sectional area at the top of the lookup table.
      DOUBLE PRECISION, INTENT(IN) :: Z             !! Water-surface elevation to evaluate.
      DOUBLE PRECISION, INTENT(IN) :: AFROMXSTYPES(3, NXSCEE) !! Cross-section lookup rows: depth, conveyance, and conveyance slope.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CONV  !! Conveyance at `Z`.
      DOUBLE PRECISION, INTENT(OUT) :: DERIV !! Implemented derivative term returned with `CONV`.

      ! Locals
      INTEGER :: I
      DOUBLE PRECISION :: H, HFULL, XA

   !----------------------------------------------------------------------*
      
      H = Z - ZG
      HFULL = AFROMXSTYPES(1, NXSCEE)

      I = INT((H / HFULL) * DBLE(NXSCEE - 1) + ONE)

      IF (I < NXSCEE) THEN
         ! * use look-up tables
         DERIV = AFROMXSTYPES(3, I)
         CONV  = AFROMXSTYPES(2, I) + DERIV * DIMJE(H, AFROMXSTYPES(1, I))
      ELSE
         ! * calculate values directly
         XA = AFROMXAFULL + AFROMCWIDTH * DIMJE(H, HFULL)
         CALL CONVEYAN(STR, H, CONV, DERIV, 2, XA, AFROMCWIDTH)
      END IF
      
   END SUBROUTINE OCCODE



   !> Calculates flow and derivative at an external overland/channel boundary.
   !>
   !> Boundary types include prescribed head, prescribed flow, normal/
   !> resistance flow, and weir control. The routine returns both the
   !> boundary flux and its derivative with respect to the local water level
   !> for the OC Newton system.
   !>
   !> Entry requirements retained from the legacy routine are: `NXSCEE >= 2`,
   !> positive path length `LI`, non-negative weir coefficient and
   !> submergence ratio when `NTYPE` is 7 or 8, and, for resistance-flow
   !> branches, `ZI >= ZGI` with positive `STR`, `W`, `XAFULL`, and full-table
   !> depth. The `XSTAB` table is expected to have uniformly spaced depths and
   !> non-negative conveyance values with positive slopes.
   !>
   !> Boundary type is reduced with
   !>
   !> \[
   !> MTYPE = NTYPE \bmod 6.
   !> \]
   !>
   !> The direct boundary branches are:
   !>
   !> | `MTYPE`/`NTYPE` | Boundary behaviour |
   !> |:----------------|:-------------------|
   !> | `MTYPE=3` | Prescribed time-varying head; sets `ZX=HOCNOW`, with no direct flux before the resistance-flow part. |
   !> | `MTYPE=4` | Prescribed time-varying inflow; `Q=QOCF`, `dQ/dZI=0`. `QOCF` is already an inflow rate, not a discharge computed from local hydraulics. |
   !> | `MTYPE=5` | Polynomial function of local head. |
   !> | `NTYPE=7` | Weir only. |
   !> | `NTYPE=8` | River/resistance flow in parallel with a weir. |
   !>
   !> For polynomial boundaries, with \(H=ZI-ZGI\) and coefficients
   !> \(a_1,\ldots,a_5=COCBCD(1:5)\),
   !>
   !> \[
   !> Q = -\left(a_1H^4+a_2H^3+a_3H^2+a_4H+a_5\right),
   !> \]
   !>
   !> \[
   !> \frac{dQ}{dZI} =
   !> -\left(4a_1H^3+3a_2H^2+2a_3H+a_4\right).
   !> \]
   !>
   !> For weir boundaries, `COCBCD` supplies weir coefficient, submergence
   !> ratio, sill elevation, and external downstream water level. [[qweir]] is
   !> called with the higher and lower of local/external water level. If the
   !> local element is upstream (`ZI >= ZX`), the returned flow and derivative
   !> are sign-adjusted so the boundary flux follows the OC convention.
   !>
   !> Prescribed head (`NTYPE=3`) and river-plus-weir (`NTYPE=8`) also include
   !> a resistance-flow contribution between the local level and external
   !> level `ZX`. With
   !>
   !> \[
   !> \Delta z = ZX-ZI,\qquad \sigma=\operatorname{sign}(\Delta z),
   !> \]
   !>
   !> \[
   !> L = LI(4-MTYPE),\qquad C^\* = C + C_H\,\max(DZMIN,|\Delta z|),
   !> \]
   !>
   !> where `C` and `C_H` are the conveyance and derivative from [[conveyan]]
   !> for grid head boundaries or [[occode]] for channel river-plus-weir
   !> boundaries, the code adds
   !>
   !> \[
   !> Q \leftarrow Q + \sigma C\frac{\sqrt{|\Delta z|}}{\sqrt{L}},
   !> \]
   !>
   !> \[
   !> \frac{dQ}{dZI} \leftarrow \frac{dQ}{dZI}
   !> + \frac{\sigma C_H\sqrt{|\Delta z|}
   !>       -\frac{1}{2}C^\*/\max(\sqrt{DZMIN},\sqrt{|\Delta z|})}
   !>      {\sqrt{L}}.
   !> \]
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
   !> | 1998-02-25 | RAH | 4.2 | Replaced include-common data with arguments; removed `INDEX`, `NCODE`, and redundant derivative output; reduced `COCBCD` dimension; added explicit typing; merged boundary types `10` and `4`; zeroed undefined derivatives; rewrote polynomial expressions without exponentiation. |
   !> | 1998-02-26 | RAH | 4.2 | Changed weir coefficient to an array, avoided unnecessary initialisation before [[qweir]], used `AH` for `A*H`, defined local `RDZMIN`, and zeroed outputs by default. |
   !> | 1998-04-09 | RAH | 4.2 | Reordered/replaced arguments for `OCQDQ`; added prescribed-head types `3` and `9`; fixed signs for polynomial and river-plus-weir branches; used smoothed conveyance/gradient terms. |
   !> | 1998-04-16 | RAH | 4.2 | Allowed `ZI < ZX` in the call to [[qweir]]. |
   !> | 1998-04-27 | RAH | 4.2 | Removed element/face arguments, added cross-section table inputs, and updated [[occode]] argument order. |
   !> | 1998-07-30 | RAH | 4.2 | Protected against zero-depth exponentiation. |
   !> | 2026-04-11 | SvB | - | Default-initialized `FROMQ`/`FROMDQ` to zero at entry to prevent returning uninitialized values for boundary types not covered by Part 1. |
   !> @endhistory
   SUBROUTINE OCQBC(NTYPE, LI, ZGI, STR, W, AFROMXAFULL, LINK, AFROMCOCBCD, ZI, AFROMHOCNOW, AFROMQOCF, FROMQ, FROMDQ)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NTYPE !! OC boundary type code.
      INTEGER, INTENT(IN) :: LINK  !! Channel link used for `XSTAB` lookup in river-plus-weir branches.
      DOUBLE PRECISION, INTENT(IN) :: LI             !! Boundary flow-path length.
      DOUBLE PRECISION, INTENT(IN) :: ZGI            !! Ground or bed elevation at the boundary element.
      DOUBLE PRECISION, INTENT(IN) :: STR            !! Strickler roughness coefficient.
      DOUBLE PRECISION, INTENT(IN) :: W              !! Boundary face width or channel width.
      DOUBLE PRECISION, INTENT(IN) :: AFROMXAFULL    !! Full-flow channel area for `LINK`.
      DOUBLE PRECISION, INTENT(IN) :: ZI             !! Local water-surface elevation.
      DOUBLE PRECISION, INTENT(IN) :: AFROMHOCNOW    !! Current prescribed boundary head.
      DOUBLE PRECISION, INTENT(IN) :: AFROMQOCF       !! Current prescribed boundary inflow rate.
      DOUBLE PRECISION, INTENT(IN) :: AFROMCOCBCD(5) !! Boundary coefficients for polynomial, weir, or river-plus-weir branches.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: FROMQ  !! Boundary flow; sign follows the OC face convention.
      DOUBLE PRECISION, INTENT(OUT) :: FROMDQ !! Derivative of `FROMQ` with respect to `ZI`.

      ! Locals
      INTEGER :: MTYPE
      DOUBLE PRECISION :: AH, B, C, D, DERIVM, DHH, DQU, DUM, DZ, E
      DOUBLE PRECISION :: H, HM, ROOTDZ, ROOTL
      DOUBLE PRECISION :: SIG, STRW, SUBRIO, ZSILL, ZL, ZU, ZX, COEFF(2)
      DOUBLE PRECISION :: CONVM, CONVMM

   !----------------------------------------------------------------------*

   ! Prologue
   ! --------
   ! Modernization Fix: Default initialize outputs to zero to prevent passing back uninitialized garbage
      FROMQ = ZERO
      FROMDQ = ZERO
      MTYPE = MOD(NTYPE, 6)

   ! Part 1
   ! ------
      SELECT CASE (MTYPE)
         ! Prescribed time-varying head - grid (3) or channel (9)
         ! NB: see Part 2
         CASE (3)
            ZX = AFROMHOCNOW
            FROMQ = ZERO
            FROMDQ = ZERO

         ! Prescribed time-varying flow - grid (4) or channel (10)
         ! NB: QOCF is rate of INFLOW, not discharge
         CASE (4)
            FROMQ = AFROMQOCF
            FROMDQ = ZERO

         ! Flow a polynomial function of head - grid (5) or channel (11)
         CASE (5)
            H = ZI - ZGI
            AH = AFROMCOCBCD(1) * H
            B = AFROMCOCBCD(2)
            C = AFROMCOCBCD(3)
            D = AFROMCOCBCD(4)
            E = AFROMCOCBCD(5)
            
            FROMQ = -((((AH + B) * H + C) * H + D) * H + E)
            FROMDQ = -(((4.0D0 * AH + 3.0D0 * B) * H + 2.0D0 * C) * H + D)
            
         CASE DEFAULT
            ! Weir (7) ... with river in parallel (8) - see Part 2
            IF (NTYPE == 7 .OR. NTYPE == 8) THEN
               COEFF(1) = AFROMCOCBCD(1)
               SUBRIO   = AFROMCOCBCD(2)
               ZSILL    = AFROMCOCBCD(3)
               ZX       = AFROMCOCBCD(4)
               COEFF(2) = COEFF(1)
               
               ZU = MAX(ZX, ZI)
               ZL = MIN(ZX, ZI)
               
               CALL QWEIR(ZU, ZSILL, ZL, COEFF, SUBRIO, FROMQ, DQU, FROMDQ)
               
               IF (ZI >= ZX) THEN
                  FROMQ = -FROMQ
                  FROMDQ = -DQU
               END IF
            END IF
      END SELECT


   ! Part 2
   ! ------
   ! Head, or river-part of river+weir
   ! Note: river has fictitious d/s link, same size as u/s

      IF (MTYPE == 3 .OR. NTYPE == 8) THEN
         DZ = ZX - ZI
         SIG = SIGN(ONE, DZ)
         DZ = SIG * DZ
         ROOTDZ = SQRT(DZ)
         DHH = LI * DBLE(4 - MTYPE)
         ROOTL = SQRT(DHH)

         IF (NTYPE == 3) THEN
            HM = ZI - ZGI
            STRW = STR * W
            CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)
         ELSE
            CALL OCCODE(ZGI, STR, W, AFROMXAFULL, XSTAB(:,:,LINK), ZI, CONVM, DERIVM)
         END IF

         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = HALF * CONVMM / MAX(RDZMIN, ROOTDZ)

         FROMQ = FROMQ + SIG * CONVM * ROOTDZ / ROOTL
         FROMDQ = FROMDQ + (SIG * DERIVM * ROOTDZ - DUM) / ROOTL
      END IF

   END SUBROUTINE OCQBC



   !> Calculates exchange flow and derivatives between a channel link and a bank element.
   !>
   !> Depending on bank-full and ground elevations, the exchange is
   !> represented by a resistance relation or by weir-like overflow across the
   !> bank crest.
   !>
   !> The manual describes bank elements as narrow strips beside every
   !> channel link when the bank component is active, and defines bankfull
   !> elevation from the final depth in the channel cross-section table. This
   !> routine uses that bankfull level (`ZBG(0)`) and adjacent bank/ground
   !> level (`ZBG(1)`) to choose the exchange formulation. Entry requirements
   !> retained from the legacy routine are
   !>
   !> \[
   !> W \ge 0,\qquad LI_0+LI_1>0.
   !> \]
   !>
   !> Subscript `0` is the channel link and subscript `1` is the bank/land
   !> element. The higher and lower water levels are identified from
   !>
   !> \[
   !> \Delta z=ZI_1-ZI_0,\qquad
   !> HI=\frac{1+\operatorname{nint}(\operatorname{sign}(\Delta z))}{2},
   !> \qquad LO=1-HI.
   !> \]
   !>
   !> If the adjacent ground is at or above bankfull (`ZBG(1) >= ZBG(0)`),
   !> exchange is computed with the resistance-flow relation. The effective
   !> head difference prevents flow from the lower side until it is above
   !> bankfull:
   !>
   !> \[
   !> \Delta z^\* = |\Delta z|+\min(ZI_{LO}-ZBG_0,0).
   !> \]
   !>
   !> The hydraulic depth is taken from the upstream/higher side,
   !> \(H_m=ZI_{HI}-ZBG_{HI}\), and the width-weighted Strickler factor is
   !>
   !> \[
   !> K_W =
   !> W\,\frac{STR_0LI_0+STR_1LI_1}{LI_0+LI_1}.
   !> \]
   !>
   !> With \(L=LI_0+LI_1\), [[conveyan]] is called with `ty=1` to evaluate
   !> \(C=K_WH_m^{5/3}\) away from the near-zero smoothing branch. The branch
   !> flow from `HI` to `LO` is
   !>
   !> \[
   !> Q_{LO}=C\,\frac{\sqrt{\Delta z^\*}}{\sqrt{L}}.
   !> \]
   !>
   !> Derivatives are assembled from the conveyance derivative and the
   !> square-root head term, using the same `DZMIN`/`RDZMIN` smoothing as the
   !> other OC resistance-flow routines.
   !>
   !> If bankfull is above the adjacent ground (`ZBG(1) < ZBG(0)`), the bank
   !> exchange is treated as a flat-crested weir over sill `ZBG(0)`:
   !>
   !> \[
   !> COEFF_1=\sqrt{2g}\,W,\qquad COEFF_2=0.386\,COEFF_1,
   !> \]
   !>
   !> and [[qweir]] supplies the flow and derivatives. In both branches the
   !> paired outputs are made conservative:
   !>
   !> \[
   !> Q_{HI}=-Q_{LO},\qquad
   !> DQ_{HI,HI}=-DQ_{LO,HI},\qquad
   !> DQ_{HI,LO}=-DQ_{LO,LO}.
   !> \]
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1992-06 | GP | 3.4 | Fixed the no-flow case to zero weir derivatives rather than unrelated derivative variables. |
   !> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
   !> | 1998-04-06 | RAH | 4.2 | Removed local `ALPHA`; replaced common-block inputs with arguments; replaced scalar flow/derivative outputs with arrays; removed redundant `DDDZ`; introduced conservative `HI`/`LO` handling; replaced inlined weir code with [[qweir]], fixing drowned and undrowned derivative/flow errors. |
   !> | 1998-04-08 | RAH | 4.2 | Renamed channel length argument to `W`, made flow lengths an argument array, reordered statements, used `H23MIN`/`CONVMM`, and added `DZL`. |
   !> | 1998-07-30 | RAH | 4.2 | Protected against zero-depth exponentiation. |
   !> | 2026-04-11 | SvB | - | Isolated the `QWEIR` sill-branch output from the `DQ` array through local `RDUM` to avoid aliasing the array element being written. |
   !> @endhistory
   SUBROUTINE OCQBNK(W, LI, ZBG, STR, ZI, Q, DQ)

      IMPLICIT NONE

      ! Input arguments
      ! Note: Subscript 0 refers to the link, 1 to the land element
      DOUBLE PRECISION, INTENT(IN) :: W        !! Channel-bank exchange width or channel length used by the exchange formula.
      DOUBLE PRECISION, INTENT(IN) :: LI(0:1)  !! Link-side and land-side flow lengths.
      DOUBLE PRECISION, INTENT(IN) :: ZBG(0:1) !! Bed or ground elevations, with index 0 for link and 1 for land.
      DOUBLE PRECISION, INTENT(IN) :: STR(0:1) !! Link-side and land-side Strickler roughness coefficients.
      DOUBLE PRECISION, INTENT(IN) :: ZI(0:1)  !! Link-side and land-side water-surface elevations.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: Q(0:1)         !! Paired exchange flows.
      DOUBLE PRECISION, INTENT(OUT) :: DQ(0:1, 0:1)   !! Derivatives of paired exchange flows with respect to water levels.

      ! Locals
      INTEGER :: HI, LO
      DOUBLE PRECISION :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
      DOUBLE PRECISION :: ROOTDZ, ROOTL, SIG, STRW
      DOUBLE PRECISION :: DZL, ZB, ZG, COEFF(2), RDUM

   !----------------------------------------------------------------------*

      DZ = ZI(1) - ZI(0)
      SIG = SIGN(ONE, DZ)
      HI = (1 + NINT(SIG)) / 2
      LO = 1 - HI
      ZB = ZBG(0)
      ZG = ZBG(1)

      DZL = ZI(LO) - ZB

   ! Channel bank-full lower than adjacent ground: resistance equation
   ! NB: HM has an implicit upstream weighting factor, ie ALPHA=1
      IF (ZG >= ZB) THEN
         DZ = SIG * DZ + MIN(DZL, ZERO)
         ROOTDZ = SQRT(DZ)
         HM = ZI(HI) - ZBG(HI)
         
         DHH = LI(0) + LI(1)
         STRW = W * (STR(0) * LI(0) + STR(1) * LI(1)) / DHH
         ROOTL = SQRT(DHH)
         
         CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)
         
         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = HALF * CONVMM / MAX(RDZMIN, ROOTDZ)
         
         Q(LO) = CONVM * ROOTDZ / ROOTL
         DQ(LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
         
         IF (DZL < -DZMIN) DUM = ZERO
         
         DQ(LO, LO) = -DUM / ROOTL

   ! Channel bank-full higher than adjacent ground: flat-crested weir eqn
      ELSE
         COEFF(1) = ROOT2G * W
         COEFF(2) = 0.386D0 * COEFF(1)
         
         ! AD aliasing fix: rdum isolates the output variable from DQ array memory
         CALL QWEIR(ZI(HI), ZB, ZI(LO), COEFF, F23, Q(LO), DQ(LO, HI), RDUM)
         DQ(LO, LO) = RDUM
      END IF

   ! Copy LO to HI
      Q(HI) = -Q(LO)
      DQ(HI, HI) = -DQ(LO, HI)
      DQ(HI, LO) = -DQ(LO, LO)

   END SUBROUTINE OCQBNK



   !> Calculates overland flow and derivatives between two land elements.
   !>
   !> The routine applies no-flow handling for impermeable boundaries and
   !> otherwise uses the local water-surface gradient, effective width, flow
   !> length, and Strickler roughness to compute paired conservative face
   !> fluxes.
   !>
   !> Entry requirements retained from the legacy routine are
   !>
   !> \[
   !> W>0,\qquad ZI_i \ge ZGI_i,\qquad LI_i>0,\qquad STR_i\ge 0
   !> \quad (i=0,1).
   !> \]
   !>
   !> For an internal impermeable boundary (`NTYPE=1`), both paired flows and
   !> all derivatives are zero. Otherwise, the routine identifies the higher
   !> and lower water levels from
   !>
   !> \[
   !> \Delta z = ZI_1-ZI_0,\qquad
   !> HI=\frac{1+\operatorname{nint}(\operatorname{sign}(\Delta z))}{2},
   !> \qquad LO=1-HI,
   !> \]
   !>
   !> and uses the positive head difference \(|\Delta z|\). The hydraulic
   !> depth is taken from the higher-side element,
   !>
   !> \[
   !> H_m=ZI_{HI}-ZGI_{HI},
   !> \]
   !>
   !> and the width-weighted Strickler factor is averaged over the two flow
   !> path lengths:
   !>
   !> \[
   !> K_W =
   !> W\,\frac{STR_0LI_0+STR_1LI_1}{LI_0+LI_1}.
   !> \]
   !>
   !> Because roughness is length-averaged, the resulting conveyance is not a
   !> strictly upstream-only quantity even though the hydraulic depth is
   !> taken from the higher-side element.
   !>
   !> With \(L=LI_0+LI_1\), [[conveyan]] is called with `ty=1`; away from the
   !> near-zero smoothing branch this gives
   !>
   !> \[
   !> C=K_WH_m^{5/3}.
   !> \]
   !>
   !> The flow into the lower element is
   !>
   !> \[
   !> Q_{LO}=C\,\frac{\sqrt{|\Delta z|}}{\sqrt{L}},
   !> \]
   !>
   !> and the opposite flow is enforced conservatively:
   !>
   !> \[
   !> Q_{HI}=-Q_{LO}.
   !> \]
   !>
   !> The derivative terms use the conveyance derivative and a smoothed
   !> square-root head term:
   !>
   !> \[
   !> C^\*=C+C_H\max(DZMIN,|\Delta z|),\qquad
   !> D=\frac{1}{2}\frac{C^\*}{\max(RDZMIN,\sqrt{|\Delta z|})}.
   !> \]
   !>
   !> The lower-row derivatives are
   !>
   !> \[
   !> DQ_{LO,HI}=\frac{C_H\sqrt{|\Delta z|}+D}{\sqrt{L}},\qquad
   !> DQ_{LO,LO}=-\frac{D}{\sqrt{L}},
   !> \]
   !>
   !> and the higher-row derivatives are the negative of these values, so
   !> `Q(1)=-Q(0)` and `DQ(1,i)=-DQ(0,i)`.
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
   !> | 1998-03-31 | RAH | 4.2 | Removed local `ALPHA`; replaced element/face/common inputs with explicit width, length, level, roughness, and elevation arguments; replaced scalar flow/derivative outputs with arrays; removed redundant arguments and locals; added `HI`/`LO` direction handling and generic intrinsics. |
   !> | 1998-04-27 | RAH | 4.2 | Reordered arguments for `OCQDQ` and replaced local roughness-width handling with `STRW = STRM*W`. |
   !> | 1998-07-30 | RAH | 4.2 | Protected against zero-depth exponentiation. |
   !> | 2026-04-11 | SvB | - | Replaced the explicit `DO`-loop zeroing of `Q`/`DQ` on the impermeable-boundary branch with whole-array assignment. |
   !> @endhistory
   PURE SUBROUTINE OCQGRD(NTYPE, LI, ZGI, STR, W, ZI, Q, DQ)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NTYPE !! Internal boundary type code.
      DOUBLE PRECISION, INTENT(IN) :: W        !! Shared face width.
      DOUBLE PRECISION, INTENT(IN) :: LI(0:1)  !! Flow lengths for the two land elements.
      DOUBLE PRECISION, INTENT(IN) :: ZGI(0:1) !! Ground elevations for the two land elements.
      DOUBLE PRECISION, INTENT(IN) :: STR(0:1) !! Directional Strickler roughness values for the two land elements.
      DOUBLE PRECISION, INTENT(IN) :: ZI(0:1)  !! Water-surface elevations for the two land elements.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: Q(0:1)       !! Paired land-land exchange flows.
      DOUBLE PRECISION, INTENT(OUT) :: DQ(0:1, 0:1) !! Derivatives of paired exchange flows with respect to water levels.
      
      ! Locals
      INTEGER :: HI, LO
      DOUBLE PRECISION :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
      DOUBLE PRECISION :: ROOTDZ, ROOTL, SIG, STRW

   !----------------------------------------------------------------------*

   ! INTERNAL IMPERMEABLE BOUNDARY
   ! NB: NTYPE 3,4,5 not allowed internally
      IF (NTYPE == 1) THEN
         ! Modernization Fix: Scalar-to-array broadcasting replaces the DO loop
         Q = ZERO
         DQ = ZERO
         RETURN
      END IF

   ! Set up local variables
   ! NB: HM has an implicit upstream weighting factor, ie ALPHA=1; but
   !     note STR is averaged, so CONVM will NOT be strictly "upstream"
   ! Note: ZGI(LO) is not required
      DZ = ZI(1) - ZI(0)
      SIG = SIGN(ONE, DZ)
      HI = (1 + NINT(SIG)) / 2
      LO = 1 - HI
      DZ = SIG * DZ
      ROOTDZ = SQRT(DZ)
      HM = ZI(HI) - ZGI(HI)
      
      DHH = LI(0) + LI(1)
      STRW = W * (STR(0) * LI(0) + STR(1) * LI(1)) / DHH
      ROOTL = SQRT(DHH)

   ! CALCULATE FLOW AND DERIVATIVES
   ! NB:   H23MIN          in DERIVM  prevents small DQ when HM is small
   !        DZMIN          in CONVMM  prevents small DQ when DZ is small
   !       RDZMIN          in DUM     prevents overflow when DZ is small
   !       ROOTDZ (no MAX) in DQ gives symmetric values when DZ is small

      CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)

      CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
      DUM = HALF * CONVMM / MAX(RDZMIN, ROOTDZ)
      
      Q(LO) = CONVM * ROOTDZ / ROOTL
      DQ(LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL

      DQ(LO, LO) = -DUM / ROOTL
      Q(HI) = -Q(LO)
      DQ(HI, HI) = -DQ(LO, HI)

      DQ(HI, LO) = -DQ(LO, LO)

   END SUBROUTINE OCQGRD



   !> Calculates flow and derivatives between two channel links.
   !>
   !> Link-link exchange can be controlled by an internal weir, by a ZQ
   !> stage-discharge table, or by channel conveyance of the upstream link.
   !> The returned `Q` and `DQ` arrays are antisymmetric for the two connected
   !> links.
   !>
   !> The routine first identifies the higher and lower water levels:
   !>
   !> \[
   !> \Delta z=ZI_1-ZI_0,\qquad
   !> HI=\frac{1+\operatorname{nint}(\operatorname{sign}(\Delta z))}{2},
   !> \qquad LO=1-HI.
   !> \]
   !>
   !> Internal weir links (`NTYPE=7`) use the weir coefficient, submergence
   !> ratio, and sill elevation stored in `COCBCD(1:3)`. The code calls
   !> [[qweir]] with the upstream level `ZI(HI)`, sill `ZSILL`, and downstream
   !> level `ZI(LO)`, then stores the returned derivatives for the high- and
   !> low-level arguments.
   !>
   !> Reservoir/ZQ links (`NTYPE=12`) use the configured ZQ table rather than
   !> the conveyance formula:
   !>
   !> \[
   !> Q_{LO}=ZQ(ZI_{HI}),
   !> \]
   !>
   !> through `get_ZQTable_value`. The current derivative approximation is
   !>
   !> \[
   !> DQ_{LO,HI}=50\cdot1.5\sqrt{\max(ZI_{HI}-ZQWeirSill,0)},\qquad
   !> DQ_{LO,LO}=0.
   !> \]
   !>
   !> @warning
   !> The ZQ derivative is not derived from the tabulated rating curve. The
   !> source comment notes that this approximation was suitable for the
   !> Crummock case and should be stability-tested during step changes,
   !> especially for small-area reservoirs.
   !> @endwarning
   !>
   !> All other link-link exchanges use upstream-link channel conveyance.
   !> With \(L=LI_0+LI_1\), [[occode]] supplies upstream conveyance \(C\) and
   !> derivative \(C_H\), using the cross-section table indexed by
   !> `JXSWORK(HI)`. The flow into the lower link is
   !>
   !> \[
   !> Q_{LO}=C\,\frac{\sqrt{|\Delta z|}}{\sqrt{L}},
   !> \]
   !>
   !> with derivative smoothing
   !>
   !> \[
   !> C^\*=C+C_H\max(DZMIN,|\Delta z|),\qquad
   !> D=\frac{1}{2}\frac{C^\*}{\max(RDZMIN,\sqrt{|\Delta z|})},
   !> \]
   !>
   !> \[
   !> DQ_{LO,HI}=\frac{C_H\sqrt{|\Delta z|}+D}{\sqrt{L}},\qquad
   !> DQ_{LO,LO}=-\frac{D}{\sqrt{L}}.
   !> \]
   !>
   !> The paired outputs are then made conservative:
   !>
   !> \[
   !> Q_{HI}=-Q_{LO},\qquad
   !> DQ_{HI,HI}=-DQ_{LO,HI},\qquad
   !> DQ_{HI,LO}=-DQ_{LO,LO}.
   !> \]
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
   !> | 1998-02-25 | RAH | 4.2 | Swapped `COCBCD` subscripts to match `SPEC.OC`. |
   !> | 1998-02-26 | RAH | 4.2 | Changed weir coefficient handling to an array through [[qweir]]. |
   !> | 1998-04-03 | RAH | 4.2 | Removed local `ALPHA`; replaced common-block inputs with arguments; replaced scalar outputs with arrays; simplified conveyance/derivative locals; skipped conveyance calculation for internal weirs; introduced `HI`/`LO` branch handling; used smoothed derivative terms. |
   !> | 1998-04-24 | RAH | 4.2 | Removed element arguments, added cross-section table/roughness/width/area inputs, and updated [[occode]] argument order. |
   !> | 2020-05-20 | SB | - | Added ZQ-table reservoir/channel link branch using `get_ZQTable_value`. |
   !> | 2026-04-11 | SvB | - | Isolated the `QWEIR` sill-branch output from the `DQ` array through local `RDUM` to avoid aliasing the array element being written. |
   !> @endhistory
   SUBROUTINE OCQLNK(NTYPE, LI, ZGI, STR, CW, XA, JXSWORK, AFROMCOCBCD, ZI, Q, DQ)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NTYPE !! Internal link-link boundary type code.
      DOUBLE PRECISION, INTENT(IN) :: LI(0:1)         !! Flow lengths for the two links.
      DOUBLE PRECISION, INTENT(IN) :: ZGI(0:1)        !! Bed elevations for the two links.
      DOUBLE PRECISION, INTENT(IN) :: STR(0:1)        !! Strickler roughness coefficients for the two links.
      DOUBLE PRECISION, INTENT(IN) :: CW(0:1)         !! Channel widths for the two links.
      DOUBLE PRECISION, INTENT(IN) :: XA(0:1)         !! Full-flow areas for the two links.
      INTEGER, INTENT(IN) :: JXSWORK(0:3)              !! Link indices used to select each participant's `XSTAB` table.
      DOUBLE PRECISION, INTENT(IN) :: AFROMCOCBCD(3)  !! Internal weir coefficients: coefficient, submergence ratio, and sill.
      DOUBLE PRECISION, INTENT(IN) :: ZI(0:1)         !! Water-surface elevations for the two links.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: Q(0:1)       !! Paired link-link exchange flows.
      DOUBLE PRECISION, INTENT(OUT) :: DQ(0:1, 0:1) !! Derivatives of paired exchange flows with respect to water levels.

      ! Locals
      INTEGER :: HI, LO
      DOUBLE PRECISION :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ
      DOUBLE PRECISION :: ROOTDZ, ROOTL, SIG, SUBRIO, ZSILL
      DOUBLE PRECISION :: COEFF(2), RDUM
      DOUBLE PRECISION :: DZU, WEIRSILL

   !----------------------------------------------------------------------*

   ! Set up local variables - part 1
      DZ = ZI(1) - ZI(0)
      SIG = SIGN(ONE, DZ)
      HI = (1 + NINT(SIG)) / 2
      LO = 1 - HI

   ! Internal weir
   ! NB: NTYPE 1,8,9,10,11 not allowed internally

      IF (NTYPE == 7) THEN
         COEFF(1) = AFROMCOCBCD(1)
         SUBRIO = AFROMCOCBCD(2)
         ZSILL = AFROMCOCBCD(3)
         COEFF(2) = COEFF(1)
         
         ! AD aliasing fix: rdum isolates the output variable from DQ array memory
         CALL QWEIR(ZI(HI), ZSILL, ZI(LO), COEFF, SUBRIO, Q(LO), DQ(LO, HI), RDUM)
         DQ(LO, LO) = RDUM

   ! ***ZQ Module 200520
      ELSE IF (NTYPE == 12) THEN
         ! print*, ZQTableRef, ZI(HI)
         
         Q(LO) = GET_ZQTABLE_VALUE(ZQTABLEREF, ZI(HI))
         WEIRSILL = ZQWEIRSILL(ZQTABLEREF)
         DZU = DIMJE(ZI(HI), WEIRSILL)
         
         ! This works for Crummock. Stability during step changes should be tested e.g. for a small area reservoir
         DQ(LO, HI) = 50.0D0 * 1.5D0 * SQRT(DZU)
         DQ(LO, LO) = 0.0D0
         
         ! write(779,*) ZI(HI), Q(LO), DQ(LO,HI)

   ! Standard Channel Flow
      ELSE
         ! Set up local variables - part 2
         DZ = SIG * DZ
         ROOTDZ = SQRT(DZ)
         DHH = LI(0) + LI(1)
         ROOTL = SQRT(DHH)
         
         ! CALCULATE FLOW AND DERIVATIVES
         ! NB: CONVM has an implicit upstream weighting factor, ie ALPHA=1
         CALL OCCODE(ZGI(HI), STR(HI), CW(HI), XA(HI), XSTAB(:, :, JXSWORK(HI)), ZI(HI), CONVM, DERIVM)
         
         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = HALF * CONVMM / MAX(RDZMIN, ROOTDZ)
         
         ! Note: ZGI(LO), etc are not required
         Q(LO) = CONVM * ROOTDZ / ROOTL
         DQ(LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
         DQ(LO, LO) = -DUM / ROOTL
      END IF

      Q(HI) = -Q(LO)
      DQ(HI, HI) = -DQ(LO, HI)
      DQ(HI, LO) = -DQ(LO, LO)

   END SUBROUTINE OCQLNK



   !> Calculates confluence flows and derivatives for a multi-link junction.
   !>
   !> For each active branch the routine evaluates conveyance and derivative,
   !> solves the junction balance with [[ocnode]], and perturbs branch levels
   !> to populate the derivative matrix used by the OC flow solver.
   !>
   !> Define the active branch set as
   !>
   !> \[
   !> \mathcal{A}=\{j\in\{0,1,2,3\}: JEL2_j>0\}.
   !> \]
   !>
   !> Entry requirements retained from the legacy routine are:
   !>
   !> | Requirement | Meaning |
   !> |:------------|:--------|
   !> | `NXSCEE >= 1` | Cross-section lookup tables have at least one row. |
   !> | `PRI >= 0` and formatted output open | Diagnostics can be written by the confluence solver. |
   !> | `LI(j) >= 0` for \(j\in\mathcal{A}\) | Branch lengths are non-negative. |
   !> | `ZI(j) >= ZGI(j)` for \(j\in\mathcal{A}\) | Branch water level is not below bed/ground level. |
   !> | `STR(j)`, `CW(j)`, `XA(j)`, and full-table depth are positive for \(j\in\mathcal{A}\) | Conveyance can be evaluated. |
   !> | `XSTAB(1,i,j)` is uniformly spaced and `XSTAB(2,i,j) >= 0`, `XSTAB(3,i,j) > 0` | Lookup table assumptions used by [[occode]]. |
   !>
   !> For each active branch, the routine sets
   !>
   !> \[
   !> ROOTLI_j=\sqrt{LI_j},
   !> \]
   !>
   !> and uses [[occode]] to calculate conveyance \(C_j\) and derivative
   !> \(C'_j\) at the branch water level. Inactive branches have `ROOTLI=0`,
   !> which [[ocnode]] uses as the absent-branch flag.
   !>
   !> The branch flows are obtained by solving the node balance
   !>
   !> \[
   !> \sum_{j=0}^{3} Q_j = 0
   !> \]
   !>
   !> through [[ocnode]]. The returned flows satisfy conservation to working
   !> precision, including the residual correction applied in [[ocnode]].
   !>
   !> Derivatives are evaluated by finite differences. For each active branch
   !> \(j\), the perturbation is
   !>
   !> \[
   !> \Delta z_j = \max(10^{-3},\ 10^{-2}(ZI_j-ZGI_j)).
   !> \]
   !>
   !> The routine temporarily raises only branch `j`, recomputes its
   !> conveyance and derivative, resolves the confluence, and stores
   !>
   !> \[
   !> DQIJ_{i,j} =
   !> \frac{Q_i(ZI_j+\Delta z_j)-Q_i(ZI_j)}{\Delta z_j},
   !> \qquad i=0,\ldots,3.
   !> \]
   !>
   !> Exit conditions retained from the legacy routine are:
   !>
   !> \[
   !> \sum_i QJ_i = 0,\qquad
   !> \sum_i DQIJ_{i,j}=0\quad \text{for each }j\in\mathcal{A}.
   !> \]
   !>
   !> Flow direction also follows the water levels: if one returned branch
   !> flow is positive and another is negative, the positive-flow branch is
   !> connected to a lower water level than the negative-flow branch.
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-07 | GP | 3.4 | Added `ZNODE` argument to [[ocnode]] and used it to set the old `ZOCMLN` value. |
   !> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
   !> | 1998-02-12 | RAH | 4.2 | Moved `WLMIN` into this routine, removed `ZNODE`/`ZOCMLN`, added explicit typing, removed unnecessary initialisation, merged loops, and recalculated branch conveyance only where needed for derivatives. |
   !> | 1998-02-20 | RAH | 4.2 | Updated [[ocnode]] call arguments for diagnostic element handling. |
   !> | 1998-02-24 | RAH | 4.2 | Replaced old element/face and common-block inputs with branch arrays, removed redundant outputs and locals, and stopped setting conveyance for null branches. |
   !> | 1998-02-25 | RAH | 4.2 | Removed redundant face input and used local `ZJ` so input `ZI` is not altered. |
   !> | 1998-03-18 | RAH | 4.2 | Obtained conveyance derivative `DI` from [[occode]] and passed it to [[ocnode]]. |
   !> | 1998-04-24 | RAH | 4.2 | Added roughness, width, area, and cross-section table arguments; updated [[occode]] arguments; added `ONEPC`; removed special single-wet-branch treatment. |
   !> | 2026-04-11 | SvB | - | Changed the derivative-perturbation call to `OCCODE` to pass the full `XSTAB(:,:,JXSWORK(J))` slice instead of a scalar element, matching the `OCCODE` array interface. |
   !> @endhistory
   SUBROUTINE OCQMLN(IELB, JEL2, LI, ZGI, STR, CW, XA, ZI, QJ, DQIJ, JXSWORK)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IELB       !! Element number used in confluence diagnostics.
      INTEGER, INTENT(IN) :: JEL2(0:3)  !! Participant element numbers; non-positive entries are inactive.
      DOUBLE PRECISION, INTENT(IN) :: LI(0:3)  !! Flow lengths for participant branches.
      DOUBLE PRECISION, INTENT(IN) :: ZGI(0:3) !! Bed elevations for participant branches.
      DOUBLE PRECISION, INTENT(IN) :: STR(0:3) !! Strickler roughness coefficients for participant branches.
      DOUBLE PRECISION, INTENT(IN) :: CW(0:3)  !! Channel widths for participant branches.
      DOUBLE PRECISION, INTENT(IN) :: XA(0:3)  !! Full-flow areas for participant branches.
      DOUBLE PRECISION, INTENT(IN) :: ZI(0:3)  !! Water-surface elevations for participant branches.
      INTEGER, INTENT(IN) :: JXSWORK(0:3) !! Link indices used to select participant `XSTAB` tables.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: QJ(0:3)        !! Flow from the solved node into each branch.
      DOUBLE PRECISION, INTENT(OUT) :: DQIJ(0:3, 0:3)  !! Finite-difference branch-flow derivative matrix; defined for active `j` only.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: ONEPC = 1.0D-2, WLMIN = 1.0D-3
      INTEGER :: I, J
      DOUBLE PRECISION :: CSAVE, DSAVE, CI(0:3), DI(0:3), QDUM2(0:3)
      DOUBLE PRECISION :: ZINC, ZSAVE, ROOTLI(0:3), ZJ(0:3)

   !----------------------------------------------------------------------*

   ! Calculate conveyance & its derivative (both.ge.0), & set local arrays
      DO J = 0, 3
         IF (JEL2(J) <= 0) THEN
            ! * OCNODE uses ROOTLI as a flag
            ROOTLI(J) = ZERO
         ELSE
            ROOTLI(J) = SQRT(LI(J))
            ZJ(J) = ZI(J)
            CALL OCCODE(ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, JXSWORK(J)), ZJ(J), CI(J), DI(J))
         END IF
      END DO

   ! Find flows out of node
      CALL OCNODE(IELB, ZI, CI, DI, ROOTLI, QJ)

   ! CALC. DQi/DHj
      DO J = 0, 3
         IF (JEL2(J) <= 0) CYCLE
         
         ! * temporarily increase ZJ and recalculate CI,DI
         ZSAVE = ZJ(J)
         CSAVE = CI(J)
         DSAVE = DI(J)
         
         ZINC = MAX(WLMIN, (ZSAVE - ZGI(J)) * ONEPC)  ! zgi is ground elevation
         ZJ(J) = ZSAVE + ZINC
         
         ! Modernization Fix: Changed scalar array pass (XSTAB(1,1,...)) to full slice to match OCCODE interface
         CALL OCCODE(ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, JXSWORK(J)), ZJ(J), CI(J), DI(J))
         
         ! * calculate resultant flows & evaluate derivative
         CALL OCNODE(IELB, ZJ, CI, DI, ROOTLI, QDUM2)
         
         DO I = 0, 3
            DQIJ(I, J) = (QDUM2(I) - QJ(I)) / ZINC
         END DO
         
         ZJ(J) = ZSAVE
         CI(J) = CSAVE
         DI(J) = DSAVE
      END DO

   END SUBROUTINE OCQMLN


   !> Evaluates conveyance and derivative for OC resistance-flow formulae.
   !>
   !> `ty=0` and `ty=1` handle area-based and depth-width forms with a
   !> near-zero smooth polynomial branch for AD stability. `ty=2` handles
   !> channel cross-section extension above the tabulated range.
   !>
   !> Implemented branches:
   !>
   !> | `ty` | Required inputs | Conveyance for ordinary depths \(h \ge 10^{-3}\) m | Returned derivative |
   !> |:-----|:----------------|:---------------------------------------------------|:--------------------|
   !> | `0` | `str`, `h`, `xa` | \(C=str\,xa\,h^{2/3}\) | \(str\,h^{2/3}\,5/3\), as implemented |
   !> | `1` | `str`, `h` where `str=K W` | \(C=str\,h^{5/3}\) | \(str\,h^{2/3}\,5/3\) |
   !> | `2` | `str`, `h`, `xa`, `extra` | \(C=str\,xa\,h^{2/3}\) | \(C(extra/xa+2/(3h))\) |
   !>
   !> For `ty=0` and `ty=1`, depths below \(10^{-9}\) m return zero conveyance
   !> and derivative. For \(10^{-9} \le h < 10^{-3}\) m the implementation
   !> uses the cubic smoothing polynomial
   !>
   !> \[
   !> p(h)=\frac{10}{3}h^2(4-1000h),\qquad
   !> p'(h)=\frac{10}{3}h(8-3000h).
   !> \]
   !>
   !> In that smoothed range, `ty=1` returns `conv=str*p(h)` and
   !> `deriv=str*p'(h)`. `ty=0` returns `conv=str*p(h)*xa/h`, while `deriv`
   !> remains `str*p'(h)` exactly as implemented.
   !>
   !> @warning
   !> `xa` is required for `ty=0` and `ty=2`, and `extra` is required for
   !> `ty=2`; the routine does not test `PRESENT()` before using them.
   !> @endwarning
   PURE SUBROUTINE CONVEYAN(STR, H, CONV, DERIV, TY, XA, EXTRA)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: TY            !! Conveyance branch selector: 0 area based, 1 depth-width, 2 above-table channel.
      DOUBLE PRECISION, INTENT(IN) :: STR  !! Strickler coefficient, or Strickler-width product for `ty=1`.
      DOUBLE PRECISION, INTENT(IN) :: H    !! Water depth.
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: XA    !! Cross-sectional flow area, required for `ty=0` and `ty=2`.
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: EXTRA !! Channel top width, required for `ty=2`.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CONV  !! Returned conveyance.
      DOUBLE PRECISION, INTENT(OUT) :: DERIV !! Returned derivative term used by OC linearisations.

      ! Locals
      DOUBLE PRECISION :: HM23
      DOUBLE PRECISION, PARAMETER :: MUL = 10.0D0 / 3.0D0

   !----------------------------------------------------------------------*

      IF (TY == 0) THEN
         IF (H < 1.0D-9) THEN
            CONV = 0.0D0
            DERIV = 0.0D0
         ELSE IF (H < 1.0D-3) THEN
            ! conv  = deriv * h          ! LINEARIZE NEAR ZERO
            CONV = STR * MUL * H * H * (4.0D0 - 1.0D3 * H)  ! TAKE CARE valid only for threshold of 1 mm
            CONV = CONV * XA / H
            DERIV = STR * MUL * H * (8.0D0 - 3.0D3 * H)     ! TAKE CARE valid only for threshold of 1 mm
         ELSE
            HM23 = H**F23
            CONV = STR * XA * HM23      ! NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
            DERIV = STR * HM23 * F53
         END IF
         
      ELSE IF (TY == 1) THEN
         IF (H < 1.0D-9) THEN
            CONV = 0.0D0
            DERIV = 0.0D0
         ELSE IF (H < 1.0D-3) THEN
            ! conv  = deriv * h          ! LINEARIZE NEAR ZERO
            CONV = STR * MUL * H * H * (4.0D0 - 1.0D3 * H)  ! TAKE CARE valid only for threshold of 1 mm
            DERIV = STR * MUL * H * (8.0D0 - 3.0D3 * H)     ! TAKE CARE valid only for threshold of 1 mm
         ELSE
            HM23 = H**F23
            CONV = STR * H * HM23       ! NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
            DERIV = STR * HM23 * F53
         END IF
         
      ELSE IF (TY == 2) THEN
         HM23 = H**F23
         CONV = STR * XA * HM23
         DERIV = CONV * (EXTRA / XA + F23 / H)  ! is f23 correct here?
      END IF

      ! Legacy Disabled Block
      ! IF(ty<2) THEN
      !    IF(h<dzmin) THEN
      !        deriv = str * h23min * f23
      !        conv  = deriv * h  !LINEARIZE NEAR ZERO (FOR AD)
      !        hm23  = zero
      !    ELSE
      !        hm23 = h**f23
      !        conv = str * xo * hm23
      !        deriv = str * hm23 * f53  !str * MAX(h23min, hm23) * f53
      !    ENDIF
      ! ELSE

   END SUBROUTINE CONVEYAN



   !> Calculates horizontal-crest weir flow and derivatives.
   !>
   !> The routine switches between no-flow, drowned, and undrowned conditions
   !> using upstream level, sill elevation, downstream level, coefficients,
   !> and the submergence ratio. Derivatives are returned for the upstream
   !> and downstream levels.
   !>
   !> Input meanings and restrictions are:
   !>
   !> | Variable | Meaning |
   !> |:---------|:--------|
   !> | `ZU` | Gauged upstream head or water level above the weir. |
   !> | `ZSILL` | Weir sill elevation. |
   !> | `ZL` | Gauged downstream head or water level below the weir. |
   !> | `COEFF(1)` | Drowned-weir discharge coefficient. |
   !> | `COEFF(2)` | Undrowned-weir discharge coefficient. |
   !> | `SUBRIO` | Submergence-ratio threshold for switching to drowned flow. |
   !>
   !> The expected entry conditions are `SUBRIO >= 0`, `COEFF(1:2) >= 0`, and
   !> `ZU >= ZL`. The legacy exit condition is `Q >= 0` and `DQU >= 0`.
   !>
   !> If the upstream level is below the sill, with the `DZMIN` tolerance, no
   !> flow is returned:
   !>
   !> \[
   !> Z_U < Z_s - DZMIN \quad\Rightarrow\quad Q = DQ_U = DQ_L = 0 .
   !> \]
   !>
   !> Otherwise the code forms the upstream sill head
   !> \(H_U = \mathrm{DIMJE}(Z_U,Z_s)\), the downstream sill head
   !> \(H_L = Z_L-Z_s\), and switches to the drowned branch when
   !> \(H_L > SUBRIO\,H_U\). For drowned flow,
   !>
   !> \[
   !> Q = C_d\,H_L\sqrt{Z_U-Z_L},
   !> \]
   !>
   !> with \(C_d=COEFF(1)\), and the returned linearisation terms are
   !>
   !> \[
   !> DQ_U =
   !> \frac{C_d\,\max(DZMIN,H_L)}
   !>      {2\max(RDZMIN,\sqrt{Z_U-Z_L})},
   !> \qquad
   !> DQ_L = C_d\sqrt{Z_U-Z_L} - DQ_U .
   !> \]
   !>
   !> For undrowned flow, with \(C_u=COEFF(2)\),
   !>
   !> \[
   !> Q = C_u\,H_U^{3/2},\qquad
   !> DQ_U = 1.5\,C_u\,\max(RDZMIN,\sqrt{H_U}),\qquad
   !> DQ_L = 0 .
   !> \]
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1998-02-26 | RAH | 4.2 | Made `COEFF` a two-entry array in [[qweir]] and callers; added explicit typing; zeroed outputs in no-flow cases; added generic intrinsics; added the missing drowned-flow downstream derivative term; replaced `ROOTDM` with `RDZMIN`/local terms. |
   !> | 1998-07-30 | RAH | 4.2 | Used `MAX` to keep `DQU` positive outside the no-flow case, added `DZMIN`/`DML`, and subtracted `DZMIN` from the sill in the no-flow criterion. |
   !> @endhistory
   PURE SUBROUTINE QWEIR(ZU, ZSILL, ZL, COEFF, SUBRIO, Q, DQU, DQL)

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: ZU       !! Upstream water level.
      DOUBLE PRECISION, INTENT(IN) :: ZSILL    !! Weir sill elevation.
      DOUBLE PRECISION, INTENT(IN) :: ZL       !! Downstream water level.
      DOUBLE PRECISION, INTENT(IN) :: COEFF(2) !! Drowned and undrowned weir discharge coefficients.
      DOUBLE PRECISION, INTENT(IN) :: SUBRIO   !! Submergence-ratio threshold for drowned flow.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: Q   !! Weir discharge, non-negative when entry conditions hold.
      DOUBLE PRECISION, INTENT(OUT) :: DQU !! Derivative of `Q` with respect to upstream level.
      DOUBLE PRECISION, INTENT(OUT) :: DQL !! Derivative of `Q` with respect to downstream level.

      ! Locals
      DOUBLE PRECISION :: CR, DML, DZU, DZL, ROOTDZ

   !----------------------------------------------------------------------*

   ! NO FLOW ACROSS WEIR
      IF (ZU < ZSILL - DZMIN) THEN
         Q = ZERO
         DQU = ZERO
         DQL = ZERO
      ELSE
         DZU = DIMJE(ZU, ZSILL)
         DZL = ZL - ZSILL

   ! DROWNED WEIR
         IF (DZL > SUBRIO * DZU) THEN
            ROOTDZ = SQRT(ZU - ZL)
            DML = MAX(DZMIN, DZL)
            CR = COEFF(1) * ROOTDZ
            Q = CR * DZL
            DQU = COEFF(1) * DML * HALF / MAX(RDZMIN, ROOTDZ)
            DQL = CR - DQU

   ! UNDROWNED WEIR
         ELSE
            ROOTDZ = SQRT(DZU)
            Q = COEFF(2) * DZU * ROOTDZ
            DQU = COEFF(2) * 1.5D0 * MAX(RDZMIN, ROOTDZ)
            DQL = ZERO
         END IF
      END IF

   END SUBROUTINE QWEIR



   !> Applies final OC flow and depth consistency corrections after a timestep.
   !>
   !> `OCFIX` reduces small inconsistent flows, prevents flow against a
   !> non-negative water-surface gradient, and adjusts elevations
   !> conservatively where water depths fall below configured thresholds.
   !>
   !> The routine treats three post-solve consistency cases:
   !>
   !> | Case | Test | Correction |
   !> |:-----|:-----|:-----------|
   !> | Adverse gradient | An outflow from element `iel` goes toward a neighbour with `HRF(neighbour) >= HRF(iel)`. | Reduce the offending discharge until either the paired face flow is exhausted, the local outflow is exhausted, or a small favourable head difference `DZMIN` is restored. |
   !> | Negligible flow | `-QSA(iel,iface) < face_length * UHCRIT` for an outflow face. | Remove the small flow by adding `DQ = -QSA(iel,iface)`. |
   !> | Negligible or negative depth | `HRF(iel) - ZGRUND(iel) < HCRIT` but the depth is non-zero. | Reduce the signed set of contributing face flows and finally reset `HRF(iel)` to `ZGRUND(iel)`. |
   !>
   !> `QSA` is positive into an element, so candidate corrections mainly
   !> operate on negative `QSA` values. A discharge correction `DQ` is
   !> applied conservatively to water level through
   !>
   !> \[
   !> \Delta Z_{iel} = \frac{DTOC}{AREA_{iel}}\,\Delta Q .
   !> \]
   !>
   !> For a paired neighbour `jel`, the opposite face is adjusted by
   !>
   !> \[
   !> \Delta Q_{jel} =
   !> -\operatorname{sign}(\Delta Q_{iel})
   !>  \min\left(|\Delta Q_{iel}|, |QSA_{jel,jface}|\right),
   !> \qquad
   !> \Delta Z_{jel} = \frac{DTOC}{AREA_{jel}}\,\Delta Q_{jel}.
   !> \]
   !>
   !> For adverse-gradient corrections the candidate reduction is
   !>
   !> \[
   !> \Delta Q =
   !> \min\left(QSA_{jel,jface}, -QSA_{iel,iface},
   !> \frac{DZMIN + HRF_{jel} - HRF_{iel}}
   !>      {DTOC/AREA_{jel} + DTOC/AREA_{iel}}\right).
   !> \]
   !>
   !> The routine can make up to `NPASS=100` passes through all elements and
   !> faces. Warnings are issued when a created-depth adjustment exceeds
   !> `HERROR`, or when the criteria are still not satisfied after the final
   !> pass.
   !>
   !> `afromICMREF` and `afromICMRF2` retain the native two-dimensional
   !> topology layouts. Regular neighbours and reciprocal faces occupy
   !> `afromICMREF(:,5:8)` and `afromICMREF(:,9:12)`; confluence participants
   !> and their faces occupy `afromICMRF2(:,1:3)` and
   !> `afromICMRF2(:,4:6)`. This avoids duplicating the static topology before
   !> every call.
   !>
   !> Entry requirements retained from the legacy routine are:
   !>
   !> | Requirement | Meaning |
   !> |:------------|:--------|
   !> | `NEL >= 1`, `NELEE >= NEL` | Active element count must fit the compiled element extent. |
   !> | `DTOC > 0` | OC timestep must be positive. |
   !> | `PRI >= 0` and open for formatted output | Diagnostics can be written. |
   !> | `NLFEE >= 1`, `AREA(1:NEL) > 0` | Link extent and element areas must be valid. |
   !> | For every `iel=1:NEL`, `iface=1:4`, `ICMREF(iel,iface+4) <= NEL` | Regular neighbour elements must be in range. |
   !> | If `ICMREF(iel,iface+4) >= 1`, then `1 <= ICMREF(iel,iface+8) <= 4` | Regular neighbour face numbers must be valid. |
   !> | If `ICMREF(iel,iface+4) < 0`, with `ibr=-ICMREF(iel,iface+4)`, then `ibr <= NLFEE` | Confluence branch references must fit the link extent. |
   !> | For each confluence participant `pel=ICMRF2(ibr,p)` with `pel >= 1`, `pel <= NEL` and `1 <= ICMRF2(ibr,p+3) <= 4` | Confluence participant elements and faces must be valid, and at least one participant must exist. |
   !>
   !> @history
   !>
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-10-08 | RAH | 3.4.1 | Created from part of `OCSIM`; repeated element loop up to `NPASS`; changed zero critical velocity handling; fixed missing `DTOC` factor in water-level correction; included confluence-flow adjustments. |
   !> | 1998-01-15 | RAH | 4.2 | Added intrinsic declarations in the original source. |
   !> | 1998-06-17 | RAH | 4.2 | Restricted adverse-gradient and small-flow tests to discharges, clarified non-negative-gradient wording, replaced unreliable confluence tests, and made flow adjustments less severe. |
   !> | 1998-06-18 | RAH | 4.2 | Added diagnostic message details for any mass created or lost. |
   !> | 1998-06-23 | RAH | 4.2 | Merged flow and depth loops, with depth adjustment taking priority. |
   !> | 1998-06-24 | RAH | 4.2 | Made depth adjustments conservative, removed unset local references, swapped `HERROR` sign, and used it in error criteria. |
   !> | 1998-06-25 | RAH | 4.2 | Adjusted each `HRF(IEL)` once using an interim elevation array. |
   !> | 1998-07-29 | RAH | 4.2 | Increased `NPASS` from 50 to 100, introduced error `1060`, and replaced statement function `FNDXY` with array `DXY`. |
   !> | 1999-02-04 | SB | 4.27 | Modified `DQE0` to address small flows from lower to higher elements. |
   !> | 1999-02-08 | SB | 4.27 | Set `AOK = .FALSE.` in the final depth adjustment for the same small adverse-flow issue. |
   !> | 2020-07-08 | SB | 4.5 | Demoted the final error 1060 response from fatal to a warning, so the timestep-reduction flag (see `SGLOBAL:ERROR`) can take effect instead of stopping the run. |
   !> | 2026-04-06 | SvB | - | Replaced the labelled `DO`/`CYCLE`/`GOTO`-style pass, element, face, and confluence loops with named `pass_loop`/`element_loop`/`face_loop`/`confluence_loop` constructs using `EXIT`/`CYCLE`; replaced the per-element `HRF`/`QSA` copy loop with whole-array assignment; and unrolled the `rdum4` array-slice arguments to the two diagnostic `WRITE` statements. |
   !> | 2026-08-20 | - | - | Changed the topology arguments to the native `ICMREF(NELEE,12)` and `ICMRF2(NLFEE,6)` layouts, eliminating caller-side staging. |
   !> @endhistory
   SUBROUTINE OCFIX(afromICMREF, afromICMRF2, nel, dtoc, inhrf, GGGETHRF, inqsa, GGGETQSA)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nel                        !! Number of active elements to correct.
      INTEGER, INTENT(IN) :: afromICMREF(NELEE, 12) !! Native table; columns 5:8 are neighbours and 9:12 reciprocal faces.
      INTEGER, INTENT(IN) :: afromICMRF2(NLFEE, 6)  !! Native branch table; columns 1:3 are participants and 4:6 their faces.
      DOUBLE PRECISION, INTENT(IN) :: dtoc !! OC timestep in seconds.
      DOUBLE PRECISION, DIMENSION(nel), INTENT(IN)     :: inhrf     !! Input water-surface elevations.
      DOUBLE PRECISION, DIMENSION(nel), INTENT(OUT)    :: GGGETHRF  !! Corrected water-surface elevations.
      DOUBLE PRECISION, DIMENSION(nel, 4), INTENT(IN)  :: inqsa     !! Input face discharges; positive into each element.
      DOUBLE PRECISION, DIMENSION(nel, 4), INTENT(OUT) :: GGGETQSA  !! Corrected face discharges.

      INTEGER, PARAMETER :: NPASS = 100 !! Maximum number of passes through the element/face consistency loop.
      DOUBLE PRECISION, PARAMETER :: UHCRIT = 1.0D-7 !! Minimum admissible flow rate, \([L^2/T]\).
      DOUBLE PRECISION, PARAMETER :: HCRIT = 1.0D-7  !! Minimum admissible surface water depth, \([L]\).
      DOUBLE PRECISION, PARAMETER :: HERROR = 1.0D-5 !! Minimum inoffensive negative surface water depth, \([L]\), used as the diagnostic-warning threshold.

      INTEGER          :: IELc, IFACE, IBR, idum
      INTEGER          :: JEL, JFACE, PPP, PASSS, PEL, PEL0, PFACE, PFACE0
      DOUBLE PRECISION :: DQE, DZE, QE, ZE, DHQ, DHH, DDZ, DQE0, FDQE, H
      DOUBLE PRECISION :: DQA, DZA, QA, ZA, QQ, QQMIN, Qasum, SGN, ZG, DXY (0:1), rdum4(4)
      LOGICAL          :: AOK, QSMALL, HSMALL, FAIL, FAILP, TEST, FLAG (4)
      CHARACTER(132)   :: MSG

   !----------------------------------------------------------------------*
   ! Control Loop
   ! ------------
      
      ! Fast whole-array copies outside the iteration loop
      GGGETHRF = inhrf
      GGGETQSA = inqsa
      AOK = .FALSE.
      
      pass_loop: DO PASSS = 1, NPASS

         AOK = .TRUE.
         
         element_loop: DO ielc = 1, NEL
            ZE = GGGETHRF (ielc)
            DZE = DTOC / cellarea (ielc)
            DXY (0) = DXQQ (ielc)
            DXY (1) = DYQQ (ielc)

            ZG = ZGRUND (ielc)
            H = ZE - ZG
            HSMALL = (H < HCRIT) .AND. NOTZERO(H)
            FDQE = ZERO
            
            IF (HSMALL) THEN
               DQE0 = -H / DZE
               SGN = SIGN (ONE, DQE0)
               Qasum = ZERO
               
               DO IFACE = 1, 4
                  QE = GGGETQSA (ielc, IFACE)
                  FLAG (IFACE) = QE * SGN < ZERO
                  IF (FLAG (IFACE)) Qasum = Qasum + QE
               END DO
               
               IF (NOTZERO(Qasum)) FDQE = MAX (-ONE, DQE0 / Qasum)
            END IF
            
            ! Face Loop
            Qasum = ZERO
            face_loop: DO IFACE = 1, 4
               QE = GGGETQSA (ielc, IFACE)
               
               TEST = QE < ZERO
               IF (HSMALL) TEST = FLAG (IFACE)
               IF (.NOT. TEST) CYCLE face_loop
               
               QSMALL = -QE < DXY (MOD (IFACE, 2)) * UHCRIT
               TEST = QSMALL .OR. HSMALL
               
               JEL = afromICMREF (ielc, IFACE + 4)
               IF (JEL > 0) THEN
                  JFACE = afromICMREF (ielc, IFACE + 8)
                  FAIL = GGGETHRF (JEL) >= ZE
               ELSE IF (JEL == 0) THEN
                  FAIL = .FALSE.
               ELSE
                  IBR = -JEL
                  QQMIN = ZERO
                  FAIL = .FALSE.
                  
                  confluence_loop: DO PPP = 1, 3
                     PEL = afromICMRF2 (IBR, PPP)
                     IF (PEL < 1) CYCLE confluence_loop
                     
                     PFACE = afromICMRF2 (IBR, PPP + 3)
                     QQ = GGGETQSA (PEL, PFACE) * QE
                     FAILP = (GGGETHRF (PEL) >= ZE) .AND. (QQ < ZERO)
                     
                     IF ((FAILP .OR. TEST) .AND. QQ < QQMIN) THEN
                        JEL = PEL
                        JFACE = PFACE
                        QQMIN = QQ
                     END IF
                     
                     FAIL = FAIL .OR. FAILP
                     PEL0 = PEL
                     PFACE0 = PFACE
                  END DO confluence_loop
                  
                  IF (JEL < 0) THEN
                     JEL = PEL0
                     JFACE = PFACE0
                  END IF
               END IF

               ! Adjustments
               IF (FAIL .OR. TEST) THEN
                  AOK = .FALSE.
                  
                  IF (JEL > 0) THEN
                     DZA = DTOC / cellarea (JEL)
                     ZA = GGGETHRF (JEL)
                     QA = GGGETQSA (JEL, JFACE)
                  END IF
                  
                  IF (HSMALL) THEN
                     DQE = FDQE * QE
                  ELSE IF (QSMALL) THEN
                     DQE = -QE
                  ELSE
                     DDZ = DZMIN + ZA - ZE
                     DQE = MIN (+QA, -QE, DDZ / (DZA + DZE))
                  END IF
                  
                  Qasum = Qasum + DQE
                  GGGETQSA(ielc, IFACE) = QE + DQE
                  ZE = ZE + DQE * DZE
                  
                  IF (JEL > 0) THEN
                     SGN = SIGN (ONE, DQE)
                     DQA = -SGN * MIN (SGN * DQE, SGN * QA)
                     Qasum = Qasum + DQA
                     GGGETQSA(JEL, JFACE) = QA + DQA
                     GGGETHRF(JEL) = ZA + DQA * DZA
                  END IF
                  
                  IF (.NOT. HSMALL) THEN
                     DHQ = Qasum * DZE
                     Qasum = ZERO
                     
                     IF ((ABS (DHQ) > HERROR) .OR. (passs == npass)) THEN
                        rdum4(1) = -QE 
                        rdum4(2) = -1.0D2 * DQE / QE 
                        idum = IFACE 
                        rdum4(4) = DHQ
                        
                        ! PERF FIX: Unrolled the array slice rdum4(1:2)
                        WRITE (MSG, 91030) rdum4(1), rdum4(2), idum, rdum4(4)
                        CALL ERROR(WWWARN, 1030, PPPRI, ielc, 0, MSG)
                     END IF
                  END IF
               END IF
            END DO face_loop

            ! Final Depth Adjustment
            IF (HSMALL) THEN
               AOK = .FALSE.
               DHQ = Qasum * DZE
               DHH = ZG - ZE
               ZE = ZG
               
               IF ((ABS (DHQ) + ABS (DHH) > HERROR) .OR. (passs == npass)) THEN
                  rdum4(1) = H 
                  rdum4(2) = DHQ 
                  rdum4(3) = DHH
                  
                  ! PERF FIX: Unrolled the array slice rdum4(1:3)
                  WRITE (MSG, 91024) rdum4(1), rdum4(2), rdum4(3)
                  CALL ERROR(WWWARN, 1024, PPPRI, ielc, 0, MSG)
               END IF
            END IF
            
            GGGETHRF(ielc) = ZE
         END DO element_loop

         ! Clean break out if network satisfies all stability criteria
         IF (AOK) EXIT pass_loop
         
      END DO pass_loop
      
      IF (.NOT. AOK) CALL ERROR(WWWARN, 1060, PPPRI, 0, 0, 'OC flow criteria could not be met')

      ! FORMAT STATEMENTS (Safely compiled exactly once)
91024 FORMAT('Surface water depth adjusted from', SP, 1PG15.7, ' to zero', ': depth created =', 2G15.7)
91030 FORMAT('Surface water discharge rate', 1PG14.7, ' reduced by', 0PF7.2, '% at face', I4, ': depth created =', SP, 1PG15.7)

   END SUBROUTINE OCFIX

END MODULE OCmod2
