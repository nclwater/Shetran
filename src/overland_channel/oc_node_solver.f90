!> summary: The nodal overland/channel solve, and the abstracted state accessors.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[OCNODE]] and [[FNODE]] solve for the water-surface elevation at a node;
!> [[OCCODE]] classifies a face and evaluates its conveyance; [[OCFIX]]
!> applies the small-depth corrections.
!>
!> The four accessors [[gethrf]], [[sethrf]], [[getqsa]] and [[setqsa]] are the
!> tree's interface to `HRFZZ` and `qsazz` in [[oc_state]]. They exist so that
!> an automatic-differentiation build can interpose on the water level and face
!> discharge, and they are what nine modules outside `overland_channel/` use
!> rather than touching the arrays. `initialise_ocmod` allocates `xstab`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCmod2; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_node_solver

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, half, one, zero
   USE array_limits, ONLY: nelee, nlfee, NXSCEE
   USE element_geometry, ONLY: cellarea, DXQQ, DYQQ, total_no_links, ZGRUND
   USE oc_state, ONLY: HRFZZ, qsazz, xstab
   USE oc_conveyance, ONLY: CONVEYAN, DZMIN
   USE float_compare, ONLY: notzero, iszero, gtzero, dimje
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_warn
   USE error_status, ONLY: errstat_alloc
   USE file_units, ONLY: FID_logfile

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: GETHRF, SETHRF, GETQSA, SETQSA
   PUBLIC :: OCNODE, OCCODE, OCFIX, initialise_ocmod

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
   !> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
   !> @endhistory
   SUBROUTINE initialise_ocmod()

      IMPLICIT NONE
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.

      IF (.NOT. ALLOCATED(xstab)) THEN
         ALLOCATE (xstab(3, nxscee, total_no_links), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "xstab", "OCmod:initialise_ocmod", emsg)
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

         WN = (A*FB - B*FA)/(FB - FA)
         FNM1 = FN

         CALL FNODE(WN, DI, CI, ZI, ROOTLI, QJ, FN)

         SIGMAQ = ABS(QJ(0)) + ABS(QJ(1)) + ABS(QJ(2)) + ABS(QJ(3))

         IF (ABS(FN) <= SIGMAQ*1.0D-3 .AND. ABS(B - A) <= 1.0D-4) THEN
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
         TEST = GTZERO(FN*FNM1)  ! TAKE CARE - PRECEDENCE

         IF (FN*FA >= 0.0D0) THEN
            A = WN
            FA = FN
            IF (TEST) FB = FB*HALF
         ELSE
            B = WN
            FB = FN
            IF (TEST) FA = FA*HALF
         END IF

      END DO iteration_loop

      IF (FAILED) THEN
         CALL RAISE_ERROR(ERRLVL_warn, 1027, FID_logfile, IELA, 0, 'maximum iterations exceeded for OC confluence')
         IF (ABS(FN) > SIGMAQ*1.0D-2 .OR. ABS(B - A) > 1.0D-3) THEN
            CALL RAISE_ERROR(ERRLVL_warn, 1028, FID_logfile, IELA, 0, 'Bad iteration failure for OC confluence')
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
         CJ = CI(J) + DI(J)*MAX(ZERO, DZ)
         QJ(J) = SIG*CJ*SQRT(SIG*DZ)/ROOTLI(J)
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

      I = INT((H/HFULL)*DBLE(NXSCEE - 1) + ONE)

      IF (I < NXSCEE) THEN
         ! * use look-up tables
         DERIV = AFROMXSTYPES(3, I)
         CONV = AFROMXSTYPES(2, I) + DERIV*DIMJE(H, AFROMXSTYPES(1, I))
      ELSE
         ! * calculate values directly
         XA = AFROMXAFULL + AFROMCWIDTH*DIMJE(H, HFULL)
         CALL CONVEYAN(STR, H, CONV, DERIV, 2, XA, AFROMCWIDTH)
      END IF

   END SUBROUTINE OCCODE

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
   !> The corrected quantities are the module arrays `HRFZZ` and `QSAZZ`,
   !> which the routine reads and updates in place. Nothing is staged into or
   !> out of caller-side buffers.
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
   !> | 2026-08-22 | - | - | Dropped the `inhrf`/`GGGETHRF`/`inqsa`/`GGGETQSA` buffer arguments and corrected `HRFZZ`/`QSAZZ` in place, removing three round trips of the full OC state per timestep. |
   !> @endhistory
   SUBROUTINE OCFIX(afromICMREF, afromICMRF2, nel, dtoc)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nel                        !! Number of active elements to correct.
      INTEGER, INTENT(IN) :: afromICMREF(NELEE, 12) !! Native table; columns 5:8 are neighbours and 9:12 reciprocal faces.
      INTEGER, INTENT(IN) :: afromICMRF2(NLFEE, 6)  !! Native branch table; columns 1:3 are participants and 4:6 their faces.
      DOUBLE PRECISION, INTENT(IN) :: dtoc !! OC timestep in seconds.

      INTEGER, PARAMETER :: NPASS = 100 !! Maximum number of passes through the element/face consistency loop.
      DOUBLE PRECISION, PARAMETER :: UHCRIT = 1.0D-7 !! Minimum admissible flow rate, \([L^2/T]\).
      DOUBLE PRECISION, PARAMETER :: HCRIT = 1.0D-7  !! Minimum admissible surface water depth, \([L]\).
      DOUBLE PRECISION, PARAMETER :: HERROR = 1.0D-5 !! Minimum inoffensive negative surface water depth, \([L]\), used as the diagnostic-warning threshold.

      INTEGER          :: IELc, IFACE, IBR, idum
      INTEGER          :: JEL, JFACE, PPP, PASSS, PEL, PEL0, PFACE, PFACE0
      DOUBLE PRECISION :: DQE, DZE, QE, ZE, DHQ, DHH, DDZ, DQE0, FDQE, H
      DOUBLE PRECISION :: DQA, DZA, QA, ZA, QQ, QQMIN, Qasum, SGN, ZG, DXY(0:1), rdum4(4)
      LOGICAL          :: AOK, QSMALL, HSMALL, FAIL, FAILP, TEST, FLAG(4)
      CHARACTER(132)   :: MSG

      !----------------------------------------------------------------------*
      ! Control Loop
      ! ------------

      ! `HRFZZ`/`QSAZZ` are corrected in place: they are module state of this
      ! same module, so no staging buffers are needed.
      !
      ! AD NOTE: an AD (tangent/adjoint) build needs the state to arrive as
      ! explicit arguments rather than as module variables, so the
      ! differentiated code can carry the matching derivative arrays. To set
      ! that up cleanly, add a preprocessor-guarded alternative interface,
      !
      !    #ifdef SHETRAN_AD
      !       SUBROUTINE OCFIX(afromICMREF, afromICMRF2, nel, dtoc, hrf, qsa)
      !          DOUBLE PRECISION, INTENT(INOUT) :: hrf(nel), qsa(nel,4)
      !    #else
      !       SUBROUTINE OCFIX(afromICMREF, afromICMRF2, nel, dtoc)
      !          ASSOCIATE (hrf => HRFZZ, qsa => QSAZZ)
      !    #endif
      !
      ! with the body below written against `hrf`/`qsa` only, and have the AD
      ! caller pass `HRFZZ(1:nel)`/`QSAZZ(1:nel,:)` (or its own arrays). Note
      ! `QSAZZ` is dimensioned `(NELEE,4)`, so a `(1:nel,:)` actual argument is
      ! non-contiguous and will be copied in and out by the compiler - that
      ! cost is what the in-place production path avoids, and it is the reason
      ! the buffered form must not be the default. INTENT(INOUT) is required:
      ! the routine reads the incoming state before correcting it.
      AOK = .FALSE.

      pass_loop: DO PASSS = 1, NPASS

         AOK = .TRUE.

         element_loop: DO ielc = 1, NEL
            ZE = HRFZZ(ielc)
            DZE = DTOC/cellarea(ielc)
            DXY(0) = DXQQ(ielc)
            DXY(1) = DYQQ(ielc)

            ZG = ZGRUND(ielc)
            H = ZE - ZG
            HSMALL = (H < HCRIT) .AND. NOTZERO(H)
            FDQE = ZERO

            IF (HSMALL) THEN
               DQE0 = -H/DZE
               SGN = SIGN(ONE, DQE0)
               Qasum = ZERO

               DO IFACE = 1, 4
                  QE = QSAZZ(ielc, IFACE)
                  FLAG(IFACE) = QE*SGN < ZERO
                  IF (FLAG(IFACE)) Qasum = Qasum + QE
               END DO

               IF (NOTZERO(Qasum)) FDQE = MAX(-ONE, DQE0/Qasum)
            END IF

            ! Face Loop
            Qasum = ZERO
            face_loop: DO IFACE = 1, 4
               QE = QSAZZ(ielc, IFACE)

               TEST = QE < ZERO
               IF (HSMALL) TEST = FLAG(IFACE)
               IF (.NOT. TEST) CYCLE face_loop

               QSMALL = -QE < DXY(MOD(IFACE, 2))*UHCRIT
               TEST = QSMALL .OR. HSMALL

               JEL = afromICMREF(ielc, IFACE + 4)
               IF (JEL > 0) THEN
                  JFACE = afromICMREF(ielc, IFACE + 8)
                  FAIL = HRFZZ(JEL) >= ZE
               ELSE IF (JEL == 0) THEN
                  FAIL = .FALSE.
               ELSE
                  IBR = -JEL
                  QQMIN = ZERO
                  FAIL = .FALSE.

                  confluence_loop: DO PPP = 1, 3
                     PEL = afromICMRF2(IBR, PPP)
                     IF (PEL < 1) CYCLE confluence_loop

                     PFACE = afromICMRF2(IBR, PPP + 3)
                     QQ = QSAZZ(PEL, PFACE)*QE
                     FAILP = (HRFZZ(PEL) >= ZE) .AND. (QQ < ZERO)

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
                     DZA = DTOC/cellarea(JEL)
                     ZA = HRFZZ(JEL)
                     QA = QSAZZ(JEL, JFACE)
                  END IF

                  IF (HSMALL) THEN
                     DQE = FDQE*QE
                  ELSE IF (QSMALL) THEN
                     DQE = -QE
                  ELSE
                     DDZ = DZMIN + ZA - ZE
                     DQE = MIN(+QA, -QE, DDZ/(DZA + DZE))
                  END IF

                  Qasum = Qasum + DQE
                  QSAZZ(ielc, IFACE) = QE + DQE
                  ZE = ZE + DQE*DZE

                  IF (JEL > 0) THEN
                     SGN = SIGN(ONE, DQE)
                     DQA = -SGN*MIN(SGN*DQE, SGN*QA)
                     Qasum = Qasum + DQA
                     QSAZZ(JEL, JFACE) = QA + DQA
                     HRFZZ(JEL) = ZA + DQA*DZA
                  END IF

                  IF (.NOT. HSMALL) THEN
                     DHQ = Qasum*DZE
                     Qasum = ZERO

                     IF ((ABS(DHQ) > HERROR) .OR. (passs == npass)) THEN
                        rdum4(1) = -QE
                        rdum4(2) = -1.0D2*DQE/QE
                        idum = IFACE
                        rdum4(4) = DHQ

                        ! PERF FIX: Unrolled the array slice rdum4(1:2)
                        WRITE (MSG, 91030) rdum4(1), rdum4(2), idum, rdum4(4)
                        CALL RAISE_ERROR(ERRLVL_warn, 1030, FID_logfile, ielc, 0, MSG)
                     END IF
                  END IF
               END IF
            END DO face_loop

            ! Final Depth Adjustment
            IF (HSMALL) THEN
               AOK = .FALSE.
               DHQ = Qasum*DZE
               DHH = ZG - ZE
               ZE = ZG

               IF ((ABS(DHQ) + ABS(DHH) > HERROR) .OR. (passs == npass)) THEN
                  rdum4(1) = H
                  rdum4(2) = DHQ
                  rdum4(3) = DHH

                  ! PERF FIX: Unrolled the array slice rdum4(1:3)
                  WRITE (MSG, 91024) rdum4(1), rdum4(2), rdum4(3)
                  CALL RAISE_ERROR(ERRLVL_warn, 1024, FID_logfile, ielc, 0, MSG)
               END IF
            END IF

            HRFZZ(ielc) = ZE
         END DO element_loop

         ! Clean break out if network satisfies all stability criteria
         IF (AOK) EXIT pass_loop

      END DO pass_loop

      IF (.NOT. AOK) CALL RAISE_ERROR(ERRLVL_warn, 1060, FID_logfile, 0, 0, 'OC flow criteria could not be met')

      ! FORMAT STATEMENTS (Safely compiled exactly once)
91024 FORMAT('Surface water depth adjusted from', SP, 1PG15.7, ' to zero', ': depth created =', 2G15.7)
91030 FORMAT('Surface water discharge rate', 1PG14.7, ' reduced by', 0PF7.2, '% at face', I4, ': depth created =', SP, 1PG15.7)

   END SUBROUTINE OCFIX

END MODULE oc_node_solver

