!> summary: Overland/channel hydraulic flux helper routines.
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
!> used by the routines here; the formulas below describe the actual code paths.
!>
!> `CONVEYAN` evaluates the conveyance kernels used by overland, channel, bank,
!> and boundary exchange. Away from the near-zero-depth smoothing branch, the
!> area-based channel form used with `ty=0` is
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
!> with signs chosen so paired face/link fluxes are conservative. Derivative
!> arrays such as `DQ0ST`, `DQIST`, and `DQIST2` are the corresponding
!> linearisations with respect to the upstream/downstream water levels.
!>
!> Weir routines use the manual's `OC38`-`OC41` weir coefficient, sill
!> elevation, downstream water level, and submerged-flow ratio in the
!> horizontal-crest weir solver [[qweir]]. Reservoir/channel links may instead
!> obtain discharge from [[zqmod]] rating tables through `get_ZQTable_value`; in
!> those cases discharge is a tabulated stage-discharge lookup rather than a
!> direct conveyance or weir calculation.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-1998 | GP/RAH | 3.4.1-4.2 | Reworked OC hydraulic routines, boundary types, confluences, weir handling, and derivative outputs. |
!> | 1999-02 | SB | 4.27 | Adjusted confluence mass conservation and small adverse-flow correction behaviour. |
!> | 2008-12 | JE | 4.3.5F90 | Converted part of the OC `.F` files into this Fortran 90 helper module. |
!> | 2020-05 | SB | - | Added ZQ-table reservoir/channel link support. |
MODULE OCmod2
USE SGLOBAL
USE ZQmod,     ONLY : get_ZQTable_value
USE AL_D,      ONLY : ZQweirsill,ZQTableRef
IMPLICIT NONE

DOUBLEPRECISION, PARAMETER   :: F23=2.0D0/3.0D0,      &
                                F53=5.0D0/3.0D0,      &
                                DZMIN = 1.0D-3,       &
                                RDZMIN=3.16227766d-2, & !(=sqrt(dzmin))
                                H23MIN=1.0d-2,        & !(=DZMIN^^F23)
                                ROOT2G = 4.42944d0      !=sqrt(2x9.81)
DOUBLEPRECISION, DIMENSION(NELEE)          :: HRFZZ    !water surface elevation - here for data abstraction AD
DOUBLEPRECISION, DIMENSION(NELEE,4)        :: qsazz    !discharge elevation - here for data abstraction AD

! sb 121212
!DOUBLEPRECISION, DIMENSION(3,NXSCEE,NLFEE) :: xstab
DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: xstab

PRIVATE
PUBLIC :: GETHRF, SETHRF, GETQSA, SETQSA, GETQSA_ALL, CONVEYAN, OCQBC, OCQMLN, OCQLNK, OCQGRD, OCQBNK, OCFIX, XSTAB, &
          hrfzz, qsazz, OCNODE, initialise_ocmod  !THESE PUBLIC ONLY FOR USE IN AD
CONTAINS



!> Returns the stored water-surface elevation for an element.
DOUBLEPRECISION FUNCTION gethrf(i)
INTEGER, INTENT(IN) :: i
gethrf = hrfzz(i)
END FUNCTION gethrf



!> Stores the water-surface elevation for an element.
SUBROUTINE sethrf(i,v)
INTEGER, INTENT(IN)         :: i
DOUBLEPRECISION, INTENT(IN) :: v
hrfzz(i) = v
END SUBROUTINE sethrf



!> Returns the stored face discharge for an element and face.
DOUBLEPRECISION FUNCTION getqsa(i,j)
INTEGER, INTENT(IN) :: i, j
getqsa = qsazz(i,j)
END FUNCTION getqsa



!> Stores the face discharge for an element and face.
SUBROUTINE setqsa(i,j, v)
INTEGER, INTENT(IN)         :: i, j
DOUBLEPRECISION, INTENT(IN) :: v
qsazz(i,j) = v
END SUBROUTINE setqsa



!> Returns the stored face-discharge array for the first `n` elements.
FUNCTION getqsa_all(n)
INTEGER, INTENT(IN)             :: n
DOUBLEPRECISION, DIMENSION(n,4) :: getqsa_all
getqsa_all = qsazz(1:n,:)
END FUNCTION getqsa_all



!> Allocates channel cross-section conveyance lookup tables.
SUBROUTINE initialise_ocmod()
!print*,nxscee,total_no_links
ALLOCATE(xstab(3,nxscee,total_no_links))
!print*,'here'
END SUBROUTINE initialise_ocmod



!> Solves a multi-link confluence so branch flows sum to zero.
!>
!> `OCNODE` finds the junction water level by bracketing and false-position
!> iteration, using [[fnode]] to evaluate the net flow leaving the node. After
!> convergence, the largest branch flow is adjusted by the small residual to
!> enforce local mass conservation at the confluence.
!>
!> This is the confluence solve used by the OC channel-link routines after each
!> active branch has supplied a water level `ZI`, conveyance `CI`, conveyance
!> derivative `DI`, and square-root path length `ROOTLI`. Absent branches are
!> flagged by `ROOTLI(j)=0` and assigned zero flow.
!>
!> The node elevation \(z_n\) is initially bracketed by the minimum and maximum
!> active branch water levels:
!>
!> \[
!> A=\min_j ZI_j,\qquad B=\max_j ZI_j.
!> \]
!>
!> For a trial node elevation, [[fnode]] evaluates the branch flow leaving the
!> node as
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
!> If the residual keeps the same sign as the previous residual, the opposite
!> endpoint residual is halved before the next interpolation step; this is the
!> legacy damping used to avoid stagnation. The accepted convergence test is
!>
!> \[
!> |F(z_n)| \le 10^{-3}\sum_j |Q_j|
!> \quad\text{and}\quad |B-A|\le 10^{-4}\ \mathrm{m}.
!> \]
!>
!> At convergence, the branch with the largest absolute flow is corrected by
!> the residual,
!>
!> \[
!> Q_{j_{\max}} \leftarrow Q_{j_{\max}}-F(z_n),
!> \]
!>
!> so that the returned branch flows sum exactly to zero to working precision.
!> If the iteration limit is reached, warning `1027` is issued. Warning `1028`
!> is also issued when the residual is greater than one percent of total
!> absolute flow or the bracket is wider than `1D-3`.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | - | - | SHETRAN/OC/OCNODE/4.27 | Routine calculates flows out of a node as a function of adjacent water elevations. |
!> | - | GP | 3.4 | Called `ERROR` and terminated iterations if `NC == 50`; added `ZNODE` argument for [[ocqmln]]. |
!> | 1998-02-12 | RAH | 4.2 | Supplied missing `PRI`, `FATAL`, and `WARN` for `ERROR`; removed `ZNODE`; added explicit typing; removed local `TESTZ`; added `TEST`; clarified description as flow out of the node; returned immediately if `FA = 0`; tested `NC` before updating bracket endpoints; set `QJ` at absent branches. |
!> | 1998-02-20 | RAH | 4.2 | Added `IEL` argument for `ERROR` handling from [[ocqmln]]. |
!> | 1998-03-18 | RAH | 4.2 | Added `DI` argument passed to [[fnode]]. |
!> | 1999-02-04 | SB | 4.27 | Fixed junction mass conservation by adjusting the largest absolute branch flow so the branch-flow sum is zero. |
!> | 2026-05-21 | SB | 4.6 | Increased the iteration limit and tightened convergence criteria at channel junctions. |
SUBROUTINE OCNODE (iela, ZI, CI, DI, ROOTLI, QJ)

INTEGER, INTENT(IN)         :: IELa
DOUBLEPRECISION, INTENT(IN) :: CI (0:3), DI (0:3), ZI (0:3), ROOTLI (0:3)
! NB:
!         ROOTLI(J)   is zero for any absent branches J.gt.0
!                     Note: branch J=0 is never absent.
!   DI(J),CI(J),ZI(J) are undefined for absent branches
DOUBLEPRECISION, INTENT(OUT) :: QJ (0:3)
INTEGER                      :: J, NC
DOUBLEPRECISION              :: A, B, FA, FB, FN, FNM1, SIGMAQ, WN
LOGICAL                      :: TEST
!^^^^RAH/SB 4/2/99 CONSERVE MASS AT JUNCTIONS ^^^^^^^^^^^^^
INTEGER                      :: JMAJOR
LOGICAL :: iscycle, failed
!----------------------------------------------------------------------*
!
! FIRST GUESSES (CHOOSE VALUES A,B SUCH THAT F(A)*F(B) .le. 0 )
! (USE MIN AND MAX OF VALID ELEVATIONS); also, set QJ at absent branches
!
A = ZI (0)
B = A
DO J = 1, 3
    IF(ISZERO(ROOTLI(J))) THEN
        QJ (J) = zero
    ELSE
        A = MIN (ZI (J), A)
        B = MAX (ZI (J), B)
    ENDIF
ENDDO
CALL FNODE(A, DI, CI, ZI, ROOTLI, QJ, FA)
IF (ISZERO(FA)) RETURN
CALL FNODE(B, DI, CI, ZI, ROOTLI, QJ, FB)
IF (ISZERO(FB)) RETURN
!
! Iterate to convergence, using successive linear interpolation
!
FN = FA
NC = 0
failed =.FALSE.
iscycle=.FALSE.
!     * Start of iteration loop: set new point WN and calculate FN
! changes by sb 20260521
! increase max iterations to 200 to allow for more difficult junctions
! exit the do loop if iscycle is true, which is set to true if convergence criteria are met
! reduce convergence criteria to 1.0D-3 of flow and 1.0D-4 of head difference to allow for more difficult junctions
DO nc=1,200
    IF(iscycle) EXIT
    WN   = (A*FB - B*FA) / (FB-FA)
    FNM1 = FN
    CALL FNODE(WN, DI, CI, ZI, ROOTLI, QJ, FN)

    SIGMAQ = ABS(QJ(0) ) + ABS(QJ(1) ) + ABS(QJ(2) ) + ABS(QJ(3) )
    ! previous convergence IF (ABS(FN) .LE. SIGMAQ*1.0D-2 .AND. ABS(B-A) .LE. 1.0D-3) THEN
    IF (ABS(FN) .LE. SIGMAQ*1.0D-3 .AND. ABS(B-A) .LE. 1.0D-4) THEN
        JMAJOR = 0
        DO J = 1, 3
            IF (ABS(QJ(J)) .GT. ABS(QJ(JMAJOR))) JMAJOR = J
        ENDDO
        QJ(JMAJOR) = QJ(JMAJOR) - FN
        iscycle=.TRUE.
        failed =.false.
        EXIT

    else
        failed =.TRUE.
    ENDIF
    !            * ... carry on: replace either A or B with WN; and
    !            * adjust interpolation factor if sign of F didn't change
    TEST = GTZERO(FN * FNM1)  !TAKE CARE - PRECEDENCE
    IF (FN * FA.GE.0D0) THEN
        A = WN
        FA = FN
        IF (TEST) FB = FB * half
    ELSE
        B = WN
        FB = FN
        IF (TEST) FA = FA * half
    ENDIF
ENDDO
IF(failed) THEN
    !write(672,*) 'iela', iela
    !write(672,*) 'fn', FN
    !write(672,*) 'b,a,b-a', B, A, B-A
    !write(672,*) 'ZI', (zi(j),j=0,3)
    !write(672,*) 'CI', (ci(j),j=0,3)
    !write(672,*) 'DI', (di(j),j=0,3)
    !write(672,*) 'ROOTLI', (ROOTLI(j),j=0,3)
    !write(672,*) 'QJ', (QJ(j),j=0,3)

    CALL ERROR(WWWARN, 1027, PPPRI, iela, 0, 'maximum iterations exceeded for OC confluence')
    IF (ABS (FN) .GT.SIGMAQ * 1.0D-2.OR.ABS (B - A) .GT.1.0D-3) THEN
        CALL ERROR(WWWARN, 1028, PPPRI, iela, 0, 'Bad iteration failure for OC confluence')
    ENDIF
ENDIF
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
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-11 | RAH | 4.2 | Added explicit typing, generic intrinsics, local flow/residual variables, integer direction handling, and replaced the old `CI2` array with scalar conveyance logic. |
!> | 1998-02-12 | RAH | 4.2 | Removed local `CI3` and extended the active-branch loop logic. |
!> | 1998-03-18 | RAH | 4.2 | Set downstream branch conveyance using `ZNODE` and added the `DI` argument passed from [[ocnode]]. |
SUBROUTINE FNODE (ZNODE, DI, CI, ZI, ROOTLI, QJ, resfnode)
DOUBLEPRECISION, INTENT(IN) ::  ZNODE, DI (0:3), CI (0:3), ZI (0:3), ROOTLI (0:3)
DOUBLEPRECISION, INTENT(OUT) ::  QJ (0:3), resfnode
! NB:
!         QJ(J) is output, but only for those J with ROOTLI(J).ne.0
! Locals, etc
INTEGER         :: J
DOUBLEPRECISION :: CJ, DZ, Qasum, SIG
!----------------------------------------------------------------------*
Qasum = zero
qj = zero
DO J = 0, 3
   IF (ISZERO(ROOTLI(J))) CYCLE
!                            >>>>>>>>
   DZ = ZNODE-ZI (J)
   SIG = SIGN (ONE, DZ)
   CJ = CI (J) + DI (J) * MAX (ZERO, DZ)
   QJ (J) = SIG * CJ * SQRT (SIG * DZ) / ROOTLI (J)
   Qasum = QJ (J) + Qasum
ENDDO
resfnode = Qasum
END SUBROUTINE FNODE



!> Calculates channel-link conveyance and derivative at a water elevation.
!>
!> Below bank-full the routine interpolates precomputed cross-section tables.
!> Above the table range it extends the cross-sectional area with top width and
!> evaluates the Gauckler-Manning-Strickler-style conveyance through
!> [[conveyan]].
!>
!> Entry requirements are `Z >= ZG`, positive `STR`, `CWIDTH`, `XAFULL`, and
!> full-table depth `XSTAB(1,NXSCEE)`, and `NXSCEE >= 1`. For lookup intervals,
!> the table is assumed to have uniformly spaced depths
!>
!> \[
!> XSTAB(1,i)=XSTAB(1,NXSCEE)\frac{i-1}{NXSCEE-1},
!> \]
!>
!> with non-negative stored conveyance and positive stored conveyance slope.
!> The routine returns non-negative `CONV` and positive `DERIV` when these
!> conditions are satisfied.
!>
!> The local water depth and full-table depth are
!>
!> \[
!> H=Z-ZG,\qquad H_{full}=XSTAB(1,NXSCEE).
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
!> DERIV=XSTAB(3,i),\qquad
!> CONV=XSTAB(2,i)+DERIV\,(H-XSTAB(1,i)).
!> \]
!>
!> For water above the tabulated range, the full-bank area is extended by the
!> rectangular top width:
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
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
!> | 1998-04-23 | RAH | 4.2 | Added explicit typing; moved `ZG` before `Z`; replaced common-block inputs with arguments for roughness, full area, cross-section width, and lookup table; replaced loop search with direct interval calculation; rearranged above-bankfull conveyance/derivative expressions. |
SUBROUTINE OCCODE(ZG, STR, afromCWIDTH, afromXAFULL, afromXStypes, Z, CONV, DERIV)
DOUBLEPRECISION, INTENT(IN) ::  ZG, STR, afromCWIDTH, afromXAFULL, Z
DOUBLEPRECISION, INTENT(IN) ::  afromXStypes(3, NXSCEE)
DOUBLEPRECISION, INTENT(OUT) :: CONV, DERIV
INTEGER :: I
DOUBLEPRECISION H, HFULL, XA
!----------------------------------------------------------------------*asum1
H = Z - ZG
HFULL = afromXStypes (1, NXSCEE)

I = INT((H / HFULL) * DBLE(NXSCEE-1) + one)
!I = (H / HFULL) * (NXSCEE-1) + one
IF (I.LT.NXSCEE) THEN
!         * use look-up tables
   DERIV = afromXStypes (3, I)
   CONV = afromXStypes (2, I) + DERIV * DIMJE(H, afromXStypes (1, I) )
ELSE
!         * calculate values directly
   XA = afromXAFULL + afromCWIDTH * DIMJE(H, HFULL)
   !CONV = STR * XA * H**F23
   CALL CONVEYAN(str, h, conv, deriv, 2, xa, afromCWIDTH)
   !DERIV = CONV * (afromCWIDTH / XA + F23 / H)

ENDIF
END SUBROUTINE OCCODE



!SUBROUTINE OCQBC(NTYPE, LI, ZGI, STR, W, afromXAFULL, afromXSTAB, afromCOCBCD, ZI, afromHOCNOW, afromQOCF, fromQ, fromDQ)
!> Calculates flow and derivative at an external overland/channel boundary.
!>
!> Boundary types include prescribed head, prescribed flow, normal/resistance
!> flow, and weir control. The routine returns both the boundary flux and its
!> derivative with respect to the local water level for the OC Newton system.
!>
!> Entry requirements retained from the legacy routine are: `NXSCEE >= 2`,
!> positive path length `LI`, non-negative weir coefficient and submergence
!> ratio when `NTYPE` is 7 or 8, and, for resistance-flow branches, `ZI >= ZGI`
!> with positive `STR`, `W`, `XAFULL`, and full-table depth. The `XSTAB` table is
!> expected to have uniformly spaced depths and non-negative conveyance values
!> with positive slopes.
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
!> | `MTYPE=4` | Prescribed time-varying inflow; `Q=QOCF`, `dQ/dZI=0`. |
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
!> For weir boundaries, `COCBCD` supplies weir coefficient, submergence ratio,
!> sill elevation, and external downstream water level. `QWEIR` is called with
!> the higher and lower of local/external water level. If the local element is
!> upstream (`ZI >= ZX`), the returned flow and derivative are sign-adjusted so
!> the boundary flux follows the OC convention.
!>
!> Prescribed head (`NTYPE=3`) and river-plus-weir (`NTYPE=8`) also include a
!> resistance-flow contribution between the local level and external level
!> `ZX`. With
!>
!> \[
!> \Delta z = ZX-ZI,\qquad \sigma=\operatorname{sign}(\Delta z),
!> \]
!>
!> \[
!> L = LI(4-MTYPE),\qquad C^\* = C + C_H\,\max(DZMIN,|\Delta z|),
!> \]
!>
!> where `C` and `C_H` are the conveyance and derivative from [[conveyan]] for
!> grid head boundaries or [[occode]] for channel river-plus-weir boundaries,
!> the code adds
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
!> History:
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
SUBROUTINE OCQBC(NTYPE, LI, ZGI, STR, W, afromXAFULL, link, afromCOCBCD, ZI, afromHOCNOW, afromQOCF, fromQ, fromDQ)
! Input arguments
INTEGER, INTENT(IN)         :: NTYPE, LINK
DOUBLEPRECISION, INTENT(IN) ::  LI, ZGI, STR, W, afromXAFULL, ZI, afromHOCNOW, afromQOCF, &
                                afromCOCBCD(5) !, afromXSTAB (3, NXSCEE)
DOUBLEPRECISION, INTENT(OUT) :: fromQ, fromDQ
INTEGER                      :: MTYPE
DOUBLEPRECISION              :: AH, B, C, CONVM, CONVMM, D, DERIVM, DHH, DQU, DUM, DZ, E
DOUBLEPRECISION              :: H, HM, ROOTDZ, ROOTL
DOUBLEPRECISION              :: SIG, STRW, SUBRIO, ZSILL, ZL, ZU, ZX, COEFF (2)
!----------------------------------------------------------------------*
! Prologue
! --------
MTYPE = MOD (NTYPE, 6)
! Part 1
! ------
! Prescribed time-varying head - grid (3) or channel (9)
!     NB: see Part 2

IF (MTYPE.EQ.3) THEN
   ZX = afromHOCNOW
   fromQ = zero
   fromDQ = zero
! Prescribed time-varying flow - grid (4) or channel (10)
!     NB: QOCF is rate of INFLOW, not discharge

ELSEIF (MTYPE.EQ.4) THEN
   fromQ = afromQOCF
   fromDQ = zero
! Flow a polynomial function of head - grid (5) or channel (11)

ELSEIF (MTYPE.EQ.5) THEN
   H = ZI - ZGI
   AH = afromCOCBCD (1) * H
   B = afromCOCBCD (2)
   C = afromCOCBCD (3)
   D = afromCOCBCD (4)
   E = afromCOCBCD (5)
   fromQ = - ( ( ( (AH + B) * H + C) * H + D) * H + E)
   fromDQ = - ( ( (4D0 * AH + 3D0 * B) * H + 2D0 * C) * H + D)
! Weir (7) ... with river in parallel (8) - see Part 2

ELSEIF (NTYPE.EQ.7.OR.NTYPE.EQ.8) THEN
   COEFF (1) = afromCOCBCD (1)
   SUBRIO = afromCOCBCD (2)
   ZSILL = afromCOCBCD (3)
   ZX = afromCOCBCD (4)
   COEFF (2) = COEFF (1)
   ZU = MAX (ZX, ZI)
   ZL = MIN (ZX, ZI)
   CALL QWEIR (ZU, ZSILL, ZL, COEFF, SUBRIO, fromQ, DQU, fromDQ)
   IF (ZI.GE.ZX) THEN
      fromQ = - fromQ
      fromDQ = - DQU
   ENDIF
ENDIF
! Part 2
! ------
! Head, or river-part of river+weir
!     Note: river has fictitious d/s link, same size as u/s

IF (MTYPE.EQ.3.OR.NTYPE.EQ.8) THEN
   DZ = ZX - ZI
   SIG = SIGN (ONE, DZ)
   DZ = SIG * DZ
   ROOTDZ = SQRT (DZ)
   DHH = LI * DBLE(4 - MTYPE)

   ROOTL = SQRT (DHH)
   IF (NTYPE.EQ.3) THEN
      HM = ZI - ZGI
      !HM23 = zero
      !IF (GTZERO(HM)) HM23 = HM**F23
      STRW = STR * W
      !CONVM = STRW * HM23 * HM
      CALL CONVEYAN(strw, hm, convm, derivm, 1)
      !DERIVM = STRW * MAX (H23MIN, HM23) * F53
   ELSE
      !CALL OCCODE (ZGI, STR, W, afromXAFULL, afromXSTAB, ZI, CONVM, DERIVM)
      CALL OCCODE (ZGI, STR, W, afromXAFULL, XSTAB(:,:,link), ZI, CONVM, DERIVM)

   ENDIF
   CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)

   DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
   fromQ = fromQ + SIG * CONVM * ROOTDZ / ROOTL

   fromDQ = fromDQ + (SIG * DERIVM * ROOTDZ - DUM) / ROOTL

ENDIF
END SUBROUTINE OCQBC



!> Calculates exchange flow and derivatives between a channel link and a bank element.
!>
!> Depending on bank-full and ground elevations, the exchange is represented by
!> a resistance relation or by weir-like overflow across the bank crest.
!>
!> The manual describes bank elements as narrow strips beside every channel link
!> when the bank component is active, and defines bankfull elevation from the
!> final depth in the channel cross-section table. This routine uses that
!> bankfull level (`ZBG(0)`) and adjacent bank/ground level (`ZBG(1)`) to choose
!> the exchange formulation. Entry requirements retained from the legacy routine
!> are
!>
!> \[
!> W \ge 0,\qquad LI_0+LI_1>0.
!> \]
!>
!> Subscript `0` is the channel link and subscript `1` is the bank/land element.
!> The higher and lower water levels are identified from
!>
!> \[
!> \Delta z=ZI_1-ZI_0,\qquad
!> HI=\frac{1+\operatorname{nint}(\operatorname{sign}(\Delta z))}{2},
!> \qquad LO=1-HI.
!> \]
!>
!> If the adjacent ground is at or above bankfull (`ZBG(1) >= ZBG(0)`), exchange
!> is computed with the resistance-flow relation. The effective head difference
!> prevents flow from the lower side until it is above bankfull:
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
!> \(C=K_WH_m^{5/3}\) away from the near-zero smoothing branch. The branch flow
!> from `HI` to `LO` is
!>
!> \[
!> Q_{LO}=C\,\frac{\sqrt{\Delta z^\*}}{\sqrt{L}}.
!> \]
!>
!> Derivatives are assembled from the conveyance derivative and the square-root
!> head term, using the same `DZMIN`/`RDZMIN` smoothing as the other OC
!> resistance-flow routines.
!>
!> If bankfull is above the adjacent ground (`ZBG(1) < ZBG(0)`), the bank
!> exchange is treated as a flat-crested weir over sill `ZBG(0)`:
!>
!> \[
!> COEFF_1=\sqrt{2g}\,W,\qquad COEFF_2=0.386\,COEFF_1,
!> \]
!>
!> and [[qweir]] supplies the flow and derivatives. In both branches the paired
!> outputs are made conservative:
!>
!> \[
!> Q_{HI}=-Q_{LO},\qquad
!> DQ_{HI,HI}=-DQ_{LO,HI},\qquad
!> DQ_{HI,LO}=-DQ_{LO,LO}.
!> \]
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1992-06 | GP | 3.4 | Fixed the no-flow case to zero weir derivatives rather than unrelated derivative variables. |
!> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
!> | 1998-04-06 | RAH | 4.2 | Removed local `ALPHA`; replaced common-block inputs with arguments; replaced scalar flow/derivative outputs with arrays; removed redundant `DDDZ`; introduced conservative `HI`/`LO` handling; replaced inlined weir code with [[qweir]], fixing drowned and undrowned derivative/flow errors. |
!> | 1998-04-08 | RAH | 4.2 | Renamed channel length argument to `W`, made flow lengths an argument array, reordered statements, used `H23MIN`/`CONVMM`, and added `DZL`. |
!> | 1998-07-30 | RAH | 4.2 | Protected against zero-depth exponentiation. |
SUBROUTINE OCQBNK (W, LI, ZBG, STR, ZI, Q, DQ)
! Note: Subscript 0 refers to the link, 1 to the land element
DOUBLEPRECISION, INTENT(IN)  :: W, LI (0:1), ZBG (0:1), STR (0:1), ZI (0:1)
DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)
INTEGER                      :: HI, LO
DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, STRW
DOUBLEPRECISION              :: DZL, ZB, ZG, COEFF (2), rdum
DZ = ZI (1) - ZI (0)
SIG = SIGN (ONE, DZ)
HI = (1 + NINT (SIG) ) / 2
LO = 1 - HI
ZB = ZBG (0)
ZG = ZBG (1)

DZL = ZI (LO) - ZB
!
! Channel bank-full lower than adjacent ground: resistance equation
!
!     NB: HM has an implicit upstream weighting factor, ie ALPHA=1

IF (ZG.GE.ZB) THEN
   DZ = SIG * DZ + MIN (DZL, ZERO)
   ROOTDZ = SQRT (DZ)
   HM = ZI (HI) - ZBG (HI)
   !HM23 = ZERO
   !IF (HM.GT.ZERO) HM23 = HM**F23
   DHH = LI (0) + LI (1)
   STRW = W * (STR (0) * LI (0) + STR (1) * LI (1) ) / DHH

   ROOTL = SQRT (DHH)
   !CONVM = STRW * HM23 * HM
   CALL CONVEYAN(strw, hm, convm, derivm, 1)
   !DERIVM = STRW * MAX (H23MIN, HM23) * F53
   CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)

   DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
   Q (LO) = CONVM * ROOTDZ / ROOTL
   DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
   IF (DZL.LT. - DZMIN) DUM = ZERO


   DQ (LO, LO) = - DUM / ROOTL
!
! Channel bank-full higher than adjacent ground: flat-crested weir eqn
!

ELSE
   COEFF (1) = ROOT2G * W
   COEFF (2) = 386D-3 * COEFF (1)

   CALL QWEIR(ZI(HI), ZB, ZI(LO), COEFF, F23, Q(LO), DQ(LO,HI), rdum)  !AD aliasing
   DQ(LO,LO) = rdum



ENDIF
!
! Copy LO to HI
!
Q (HI) = - Q (LO)
DQ (HI, HI) = - DQ (LO, HI)

DQ (HI, LO) = - DQ (LO, LO)
END SUBROUTINE OCQBNK



!> Calculates overland flow and derivatives between two land elements.
!>
!> The routine applies no-flow handling for impermeable boundaries and otherwise
!> uses the local water-surface gradient, effective width, flow length, and
!> Strickler roughness to compute paired conservative face fluxes.
!>
!> Entry requirements retained from the legacy routine are
!>
!> \[
!> W>0,\qquad ZI_i \ge ZGI_i,\qquad LI_i>0,\qquad STR_i\ge 0
!> \quad (i=0,1).
!> \]
!>
!> For an internal impermeable boundary (`NTYPE=1`), both paired flows and all
!> derivatives are zero. Otherwise, the routine identifies the higher and lower
!> water levels from
!>
!> \[
!> \Delta z = ZI_1-ZI_0,\qquad
!> HI=\frac{1+\operatorname{nint}(\operatorname{sign}(\Delta z))}{2},
!> \qquad LO=1-HI,
!> \]
!>
!> and uses the positive head difference \(|\Delta z|\). The hydraulic depth is
!> taken from the higher-side element,
!>
!> \[
!> H_m=ZI_{HI}-ZGI_{HI},
!> \]
!>
!> and the width-weighted Strickler factor is averaged over the two flow path
!> lengths:
!>
!> \[
!> K_W =
!> W\,\frac{STR_0LI_0+STR_1LI_1}{LI_0+LI_1}.
!> \]
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
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
!> | 1998-03-31 | RAH | 4.2 | Removed local `ALPHA`; replaced element/face/common inputs with explicit width, length, level, roughness, and elevation arguments; replaced scalar flow/derivative outputs with arrays; removed redundant arguments and locals; added `HI`/`LO` direction handling and generic intrinsics. |
!> | 1998-04-27 | RAH | 4.2 | Reordered arguments for `OCQDQ` and replaced local roughness-width handling with `STRW = STRM*W`. |
!> | 1998-07-30 | RAH | 4.2 | Protected against zero-depth exponentiation. |
SUBROUTINE OCQGRD (NTYPE, LI, ZGI, STR, W, ZI, Q, DQ)
! Input arguments
INTEGER, INTENT(IN)          :: NTYPE
DOUBLEPRECISION, INTENT(IN)  :: W, LI (0:1), ZGI (0:1), STR (0:1), ZI (0:1)
DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)
INTEGER                      :: HI, LO, I
DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, STRW
!----------------------------------------------------------------------*
!
! INTERNAL IMPERMEABLE BOUNDARY
!
! NB: NTYPE 3,4,5 not allowed internally
IF (NTYPE.EQ.1) THEN
   DO 10 I = 0, 1
      Q (I) = zero
      DQ (I, 0) = zero
      DQ (I, 1) = zero
   10    END DO
   RETURN
!         ^^^^^^
ENDIF
!
! Set up local variables
!
!     NB: HM has an implicit upstream weighting factor, ie ALPHA=1; but
!         note STR is averaged, so CONVM will NOT be strictly "upstream"
!     Note: ZGI(LO) is not required
DZ = ZI (1) - ZI (0)
SIG = SIGN (ONE, DZ)
HI = (1 + NINT (SIG) ) / 2
LO = 1 - HI
DZ = SIG * DZ
ROOTDZ = SQRT (DZ)
HM = ZI (HI) - ZGI (HI)
!HM23 = zero
!IF (GTZERO(HM)) HM23 = HM**F23
DHH = LI (0) + LI (1)
STRW = W * (STR (0) * LI (0) + STR (1) * LI (1) ) / DHH
ROOTL = SQRT (DHH)
!
! CALCULATE FLOW AND DERIVATIVES
!
! NB:   H23MIN          in DERIVM  prevents small DQ when HM is small
!        DZMIN          in CONVMM  prevents small DQ when DZ is small
!       RDZMIN          in DUM     prevents overflow when DZ is small
!       ROOTDZ (no MAX) in DQ gives symmetric values when DZ is small
!
!CONVM = STRW * HM23 * HM
CALL CONVEYAN(strw, hm, convm, derivm, 1)
!DERIVM = STRW * MAX (H23MIN, HM23) * F53
CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)

DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
Q (LO) = CONVM * ROOTDZ / ROOTL
DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL

DQ (LO, LO) = - DUM / ROOTL
Q (HI) = - Q (LO)
DQ (HI, HI) = - DQ (LO, HI)

DQ (HI, LO) = - DQ (LO, LO)
END SUBROUTINE OCQGRD



!> Calculates flow and derivatives between two channel links.
!>
!> Link-link exchange can be controlled by an internal weir, by a ZQ
!> stage-discharge table, or by channel conveyance of the upstream link. The
!> returned `Q` and `DQ` arrays are antisymmetric for the two connected links.
!>
!> The routine first identifies the higher and lower water levels:
!>
!> \[
!> \Delta z=ZI_1-ZI_0,\qquad
!> HI=\frac{1+\operatorname{nint}(\operatorname{sign}(\Delta z))}{2},
!> \qquad LO=1-HI.
!> \]
!>
!> Internal weir links (`NTYPE=7`) use the weir coefficient, submergence ratio,
!> and sill elevation stored in `COCBCD(1:3)`. The code calls [[qweir]] with the
!> upstream level `ZI(HI)`, sill `ZSILL`, and downstream level `ZI(LO)`, then
!> stores the returned derivatives for the high- and low-level arguments.
!>
!> Reservoir/ZQ links (`NTYPE=12`) use the configured ZQ table rather than the
!> conveyance formula:
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
!> All other link-link exchanges use upstream-link channel conveyance. With
!> \(L=LI_0+LI_1\), [[occode]] supplies upstream conveyance \(C\) and derivative
!> \(C_H\), using the cross-section table indexed by `JXSWORK(HI)`. The flow
!> into the lower link is
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
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought implicit double-precision assumptions from `SPEC.AL` into the routine context. |
!> | 1998-02-25 | RAH | 4.2 | Swapped `COCBCD` subscripts to match `SPEC.OC`. |
!> | 1998-02-26 | RAH | 4.2 | Changed weir coefficient handling to an array through [[qweir]]. |
!> | 1998-04-03 | RAH | 4.2 | Removed local `ALPHA`; replaced common-block inputs with arguments; replaced scalar outputs with arrays; simplified conveyance/derivative locals; skipped conveyance calculation for internal weirs; introduced `HI`/`LO` branch handling; used smoothed derivative terms. |
!> | 1998-04-24 | RAH | 4.2 | Removed element arguments, added cross-section table/roughness/width/area inputs, and updated [[occode]] argument order. |
!> | 2020-05-20 | SB | - | Added ZQ-table reservoir/channel link branch using `get_ZQTable_value`. |
SUBROUTINE OCQLNK(NTYPE, LI, ZGI, STR, CW, XA, jXSwork, afromCOCBCD, ZI, Q, DQ)

! Input arguments
INTEGER, INTENT(IN)          :: NTYPE
DOUBLEPRECISION, INTENT(IN)  :: LI (0:1), ZGI (0:1), CW (0:1), afromCOCBCD(3)
DOUBLEPRECISION, INTENT(IN)  ::  ZI (0:1), STR (0:1), XA (0:1)
!DOUBLEPRECISION, INTENT(IN)  :: afromXSwork (3, NXSCEE, 0:1)
INTEGER, INTENT(IN)          :: JXSWORK(0:3)
DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)
INTEGER                      :: HI, LO
DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ
DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, SUBRIO, ZSILL
DOUBLEPRECISION              :: COEFF (2), rdum
! ZQ Module 200520
DOUBLEPRECISION              :: dzu
DOUBLEPRECISION              :: weirsill
!----------------------------------------------------------------------*
!
! Set up local variables - part 1
!
DZ = ZI (1) - ZI (0)
SIG = SIGN (ONE, DZ)
HI = (1 + NINT (SIG) ) / 2
LO = 1 - HI
!
! Internal weir
!
! NB: NTYPE 1,8,9,10,11 not allowed internally
!
IF (NTYPE.EQ.7) THEN
   COEFF (1) = afromCOCBCD (1)
   SUBRIO = afromCOCBCD (2)
   ZSILL = afromCOCBCD (3)
   COEFF (2) = COEFF (1)
   CALL QWEIR(ZI(HI), ZSILL, ZI(LO), COEFF, SUBRIO, Q(LO), DQ(LO,HI), rdum) !AD ailising
    DQ(LO,LO)=rdum
! ZQ Module 200520
ELSEIF (NTYPE.EQ.12) THEN
    !print*,ZQTableRef,zi(hi)
    Q(LO)     = get_ZQTable_value(ZQTableRef,ZI(HI))
    weirsill  = ZQWeirSill(ZQTableRef)
    DZU       = DIMJE(ZI(HI), weirsill)
    DQ(LO,HI) = 50.0*1.5*sqrt(dzu)                            ! This works for Crummock. Stability during step changes should be tested e.g. for a small area reservoir
    DQ(LO,LO) = 0
    !write(779,*) zi(hi),Q(lo),dq(lo,hi)
! ZQ Module 200520 end
ELSE
    !
    ! Set up local variables - part 2
    !
    DZ = SIG * DZ
    ROOTDZ = SQRT (DZ)
    DHH = LI (0) + LI (1)
    ROOTL = SQRT (DHH)
    !
    ! CALCULATE FLOW AND DERIVATIVES
    ! NB: CONVM has an implicit upstream weighting factor, ie ALPHA=1
    !
    !CALL OCCODE (ZGI (HI), STR (HI), CW (HI), XA (HI), afromXSwork (:, :, HI), ZI (HI), CONVM, DERIVM)
    CALL OCCODE (ZGI(HI), STR(HI), CW(HI), XA(HI), XSTAB(:, :, jxswork(HI)), ZI(HI), CONVM, DERIVM)
    CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
    DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
    !     * Note: ZGI(LO),etc are not required
    Q (LO) = CONVM * ROOTDZ / ROOTL
    DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
    DQ (LO, LO) = - DUM / ROOTL
ENDIF
Q (HI) = - Q (LO)
DQ (HI, HI) = - DQ (LO, HI)
DQ (HI, LO) = - DQ (LO, LO)
END SUBROUTINE OCQLNK



!> Calculates confluence flows and derivatives for a multi-link junction.
!>
!> For each active branch the routine evaluates conveyance and derivative,
!> solves the junction balance with [[ocnode]], and perturbs branch levels to
!> populate the derivative matrix used by the OC flow solver.
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
!> \(C'_j\) at the branch water level. Inactive branches have `ROOTLI=0`, which
!> [[ocnode]] uses as the absent-branch flag.
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
!> The routine temporarily raises only branch `j`, recomputes its conveyance and
!> derivative, resolves the confluence, and stores
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
!> Flow direction also follows the water levels: if one returned branch flow is
!> positive and another is negative, the positive-flow branch is connected to a
!> lower water level than the negative-flow branch.
!>
!> History:
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
SUBROUTINE OCQMLN(ielb, JEL2, LI, ZGI, STR, CW, XA,  ZI, QJ, DQIJ, JXSwork)
INTEGER, INTENT(IN)          :: IELb, JEL2 (0:3)
DOUBLEPRECISION, INTENT(IN)  :: LI (0:3), ZGI (0:3), STR (0:3)
DOUBLEPRECISION, INTENT(IN)  ::  CW (0:3), XA (0:3), ZI (0:3)
!DOUBLEPRECISION, INTENT(IN)  :: XSwork(3,NXSCEE,0:3)
INTEGER, INTENT(IN)          :: jxswork(0:3)
DOUBLEPRECISION, INTENT(OUT) ::  QJ (0:3), DQIJ (0:3, 0:3)
! NB:
!     DQIJ(i,j) is defined for active_j only
!
DOUBLEPRECISION             :: ONEPC, WLMIN
PARAMETER (ONEPC = 1D-2, WLMIN = 1D-3)
INTEGER                     :: I, J
DOUBLEPRECISION             :: CSAVE, DSAVE, CI (0:3), DI (0:3), QDUM2 (0:3)
DOUBLEPRECISION             :: ZINC, ZSAVE, ROOTLI (0:3), ZJ (0:3)
!----------------------------------------------------------------------*
!
! Calculate conveyance & its derivative (both.ge.0), & set local arrays
!
DO J = 0, 3
    IF (JEL2 (J) .LE.0) THEN
        !            * OCNODE uses ROOTLI as a flag
        ROOTLI (J) = zero
    ELSE
        ROOTLI (J) = SQRT (LI (J) )
        ZJ (J) = ZI (J)
        !CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSwork(:, :, J), ZJ(J), CI(J), DI(J))
        CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, jxswork(J)), ZJ(J), CI(J), DI(J))
    ENDIF
ENDDO
!
! Find flows out of node
!
CALL OCNODE (ielb, ZI, CI, DI, ROOTLI, QJ)
!
! CALC. DQi/DHj
!
DO J = 0, 3
   IF (JEL2 (J) .LE.0) CYCLE
    !        * temporarily increase ZJ and recalculate CI,DI
   ZSAVE  = ZJ(J)
   CSAVE  = CI(J)
   DSAVE  = DI(J)
   ZINC   = MAX(WLMIN, (ZSAVE-ZGI(J))*ONEPC)  !zgi is ground elevation
   ZJ (J) = ZSAVE+ZINC
   !CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSwork(1, 1, J), ZJ(J), CI(J), DI(J) )
   CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSTAB(1, 1, JXSWORK(J)), ZJ(J), CI(J), DI(J) )
                                                                     !++++++++++++++out
    !        * calculate resultant flows & evaluate derivative
   CALL OCNODE (ielb, ZJ, CI, DI, ROOTLI, QDUM2)
                                        !+++++out
    DO I = 0, 3
        DQIJ (I, J) = (QDUM2 (I) - QJ (I) ) / ZINC
    ENDDO
   ZJ(J) = ZSAVE
   CI(J) = CSAVE
   DI(J) = DSAVE
ENDDO
END SUBROUTINE OCQMLN

!> Evaluates conveyance and derivative for OC resistance-flow formulae.
!>
!> `ty=0` and `ty=1` handle area-based and depth-width forms with a near-zero
!> smooth polynomial branch for AD stability. `ty=2` handles channel
!> cross-section extension above the tabulated range.
SUBROUTINE conveyan(str, h, conv, deriv, ty, xa, extra)
!to bring this all to one place (its messy!)
INTEGER, INTENT(IN)         :: ty
DOUBLEPRECISION, INTENT(IN) :: str, & !strickler or strickler*width
                               h      ! depth
DOUBLEPRECISION, INTENT(IN), OPTIONAL :: xa, extra  !!x-sect area
DOUBLEPRECISION, INTENT(OUT)          :: conv, deriv
DOUBLEPRECISION                       :: hm23
DOUBLEPRECISION, PARAMETER            :: mul = 10.0d0/3.0d0
IF(ty==0) THEN
    IF(h<1.0d-9) THEN
        conv = 0.0d0
        deriv = 0.0d0
    ELSEIF(h<1.0d-3) THEN
        !deriv = str * h23min * f23
        !conv  = deriv * h           !LINEARIZE NEAR ZERO
        conv  = str * mul * h * h * (4.0d0 - 1.0d3*h)  !TAKE CARE valid only for threshold of 1 mm
        conv  = conv * xa / h
        deriv = str * mul * h * (8.0d0 - 3.0d3*h)      !TAKE CARE valid only for threshold of 1 mm
    ELSE
        hm23 = h**f23
        conv = str * xa * hm23      !NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
        deriv = str * hm23 * f53
    ENDIF
ELSEIF(ty==1) THEN
    iF(h<1.0d-9) THEN
        conv = 0.0d0
        deriv = 0.0d0
    ELSEIF(h<1.0d-3) THEN
        !deriv = str * h23min * f23
        !conv  = deriv * h           !LINEARIZE NEAR ZERO
        conv  = str * mul * h * h * (4.0d0 - 1.0d3*h)  !TAKE CARE valid only for threshold of 1 mm
        deriv = str * mul * h * (8.0d0 - 3.0d3*h)      !TAKE CARE valid only for threshold of 1 mm
    ELSE
        hm23 = h**f23
        conv = str * h * hm23       !NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
        deriv = str * hm23 * f53
    ENDIF
ELSEIF(ty==2) THEN
    hm23 = h**f23
    conv = str * xa * hm23
    deriv = conv * (extra / xa + f23 / h)  !is f23 correct here?
ENDIF

!IF(ty<2) THEN
!    IF(h<dzmin) THEN
!        deriv = str * h23min * f23
!        conv  = deriv * h  !LINEARIZE NEAR ZERO (FOR AD)
!        hm23  = zero
!    ELSE
!        hm23 = h**f23
!        conv = str * xo * hm23
!        deriv = str * hm23 * f53  !str * MAX(h23min, hm23) * f53
!    ENDIF
!ELSE
END SUBROUTINE conveyan



!> Calculates horizontal-crest weir flow and derivatives.
!>
!> The routine switches between no-flow, drowned, and undrowned conditions using
!> upstream level, sill elevation, downstream level, coefficients, and the
!> submergence ratio. Derivatives are returned for the upstream and downstream
!> levels.
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
!> If the upstream level is below the sill, with the `DZMIN` tolerance, no flow
!> is returned:
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
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-26 | RAH | 4.2 | Made `COEFF` a two-entry array in [[qweir]] and callers; added explicit typing; zeroed outputs in no-flow cases; added generic intrinsics; added the missing drowned-flow downstream derivative term; replaced `ROOTDM` with `RDZMIN`/local terms. |
!> | 1998-07-30 | RAH | 4.2 | Used `MAX` to keep `DQU` positive outside the no-flow case, added `DZMIN`/`DML`, and subtracted `DZMIN` from the sill in the no-flow criterion. |
SUBROUTINE QWEIR (ZU, ZSILL, ZL, COEFF, SUBRIO, Q, DQU, DQL)
DOUBLEPRECISION, INTENT(IN) :: ZU, ZSILL, ZL, SUBRIO, COEFF (2)
DOUBLEPRECISION, INTENT(OUT) ::  Q, DQU, DQL
DOUBLEPRECISION CR, DML, DZU, DZL, ROOTDZ
! NO FLOW ACROSS WEIR
IF (ZU.LT.ZSILL - DZMIN) THEN
   Q = zero
   DQU = zero
   DQL = zero
ELSE
   DZU = DIMJE(ZU, ZSILL)

   DZL = ZL - ZSILL
! DROWNED WEIR
   IF (DZL.GT.SUBRIO * DZU) THEN
      ROOTDZ = SQRT (ZU - ZL)
      DML = MAX (DZMIN, DZL)
      CR = COEFF (1) * ROOTDZ
      Q = CR * DZL
      DQU = COEFF (1) * DML * half / MAX (RDZMIN, ROOTDZ)
      DQL = CR - DQU
! UNDROWNED WEIR
   ELSE
      ROOTDZ = SQRT (DZU)
      Q = COEFF (2) * DZU * ROOTDZ
      DQU = COEFF (2) * 1.5D0 * MAX (RDZMIN, ROOTDZ)
      DQL = zero
   ENDIF
ENDIF
END SUBROUTINE QWEIR



!> Applies final OC flow and depth consistency corrections after a timestep.
!>
!> `OCFIX` reduces small inconsistent flows, prevents flow against a
!> non-negative water-surface gradient, and adjusts elevations conservatively
!> where water depths fall below configured thresholds.
!>
!> The routine treats three post-solve consistency cases:
!>
!> | Case | Test | Correction |
!> |:-----|:-----|:-----------|
!> | Adverse gradient | An outflow from element `iel` goes toward a neighbour with `HRF(neighbour) >= HRF(iel)`. | Reduce the offending discharge until either the paired face flow is exhausted, the local outflow is exhausted, or a small favourable head difference `DZMIN` is restored. |
!> | Negligible flow | `-QSA(iel,iface) < face_length * UHCRIT` for an outflow face. | Remove the small flow by adding `DQ = -QSA(iel,iface)`. |
!> | Negligible or negative depth | `HRF(iel) - ZGRUND(iel) < HCRIT` but the depth is non-zero. | Reduce the signed set of contributing face flows and finally reset `HRF(iel)` to `ZGRUND(iel)`. |
!>
!> `QSA` is positive into an element, so candidate corrections mainly operate
!> on negative `QSA` values. A discharge correction `DQ` is applied
!> conservatively to water level through
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
!> The routine can make up to `NPASS=100` passes through all elements and faces.
!> Warnings are issued when a created-depth adjustment exceeds `HERROR`, or
!> when the criteria are still not satisfied after the final pass.
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NEL >= 1`, `NELEE >= NEL` | Active element count must fit the compiled element extent. |
!> | `DTOC > 0` | OC timestep must be positive. |
!> | `PRI >= 0` and open for formatted output | Diagnostics can be written. |
!> | `NLFEE >= 1`, `AREA(1:NEL) > 0` | Link extent and element areas must be valid. |
!> | For every `iel=1:NEL`, `iface=1:4`, `ICMREF(iel,iface,2) <= NEL` | Regular neighbour elements must be in range. |
!> | If `ICMREF(iel,iface,2) >= 1`, then `1 <= ICMREF(iel,iface,3) <= 4` | Regular neighbour face numbers must be valid. |
!> | If `ICMREF(iel,iface,2) < 0`, with `ibr=-ICMREF(iel,iface,2)`, then `ibr <= NLFEE` | Confluence branch references must fit the link extent. |
!> | For each confluence participant `pel=ICMRF2(ibr,p,1)` with `pel >= 1`, `pel <= NEL` and `1 <= ICMRF2(ibr,p,2) <= 4` | Confluence participant elements and faces must be valid, and at least one participant must exist. |
!>
!> History:
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
SUBROUTINE OCFIX(afromICMREF, afromICMRF2, nel, dtoc, inhrf, GGGETHRF, inqsa, GGGETQSA)
INTEGER, INTENT(IN) :: nel, afromICMREF (NELEE, 4, 2:3), afromICMRF2 (NLFEE, 3, 2)
DOUBLEPRECISION, INTENT(IN) :: dtoc
!     NB: QSA is positive in
!     *  NPASS: maximum number of passes through the test loop
!     * UHCRIT: minimum admissible flow rate [L^^2/T]
!     *  HCRIT: minimum admissible surface water depth [L]
!     * HERROR: minimum inoffensive negative surface water depth [L]
!     *  DZMIN: target elevation difference in flow adjustments [L]
INTEGER         :: NPASS
PARAMETER (NPASS = 100)
DOUBLEPRECISION :: UHCRIT, HCRIT, HERROR
DOUBLEPRECISION, DIMENSION(nel), INTENT(IN)    :: inhrf
DOUBLEPRECISION, DIMENSION(nel), INTENT(OUT)   :: GGGETHRF
DOUBLEPRECISION, DIMENSION(nel,4), INTENT(IN)  :: inqsa
DOUBLEPRECISION, DIMENSION(nel,4), INTENT(OUT) :: GGGETQSA
PARAMETER (UHCRIT = 1D-7, HCRIT = 1D-7, HERROR = 1D-5)
INTEGER          :: IELc, IFACE, IBR, idum
INTEGER          :: JEL, JFACE, PPP, PASSS, PEL, PEL0, PFACE, PFACE0
DOUBLEPRECISION  :: DQE, DZE, QE, ZE, DHQ, DHH, DDZ, DQE0, FDQE, H
DOUBLEPRECISION  :: DQA, DZA, QA, ZA, QQ, QQMIN, Qasum, SGN, ZG, DXY (0:1), rdum4(4)
LOGICAL          :: AOK, QSMALL, HSMALL, FAIL, FAILP, TEST, FLAG (4)
CHARACTER(132)  :: MSG
!----------------------------------------------------------------------*
! Control Loop
! ------------
GGGETHRF = inhrf
GGGETQSA = inqsa
aok = .FALSE.
out900 : DO PASSS = 1, NPASS  !AP LOOP PROBLEMS
    IF(aok) THEN
        CYCLE out900  !AD Irreductible entry into loop problem
    ELSE
        AOK = .TRUE.
    ENDIF
    out400 : DO ielc = 1, NEL
        ZE = GGGETHRF (ielc)
        DZE = DTOC / cellarea (ielc)
        DXY (0) = DXQQ (ielc)
        DXY (1) = DYQQ (ielc)
        !           Depth Criterion: flag outflow (D<0) or inflow (D>0) faces
        !           ---------------------------------------------------------
        ZG = ZGRUND (ielc)
        H = ZE-ZG
        HSMALL = (H.LT.HCRIT).AND.NOTZERO(H)
        FDQE = ZERO
        IF (HSMALL) THEN
            DQE0 = - H / DZE
            !^^^^ RAH/SB small flows ^^^^^^^^^^^^^^^^^^^
            SGN = SIGN (ONE, DQE0)
            Qasum = ZERO
            DO IFACE = 1, 4
                QE = GGGETQSA (ielc, IFACE)
                !^^^^ RAH/SB small flows ^^^^^^^^^^^^^^^^^^^
                FLAG (IFACE) = QE * SGN.LT.ZERO
                !                   FLAG(IFACE) = QE*DQE0 .LT. ZERO
                IF (FLAG (IFACE) ) Qasum = Qasum + QE
            ENDDO
            IF (NOTZERO(Qasum)) FDQE = MAX ( - ONE, DQE0 / Qasum)
        ENDIF
        !           Face Loop
        !           ---------
        Qasum = ZERO
        out300 : DO IFACE = 1, 4
            QE = GGGETQSA (ielc, IFACE)
            !              * apply flow criteria to discharges only
            TEST = QE.LT.ZERO
            IF (HSMALL) TEST = FLAG (IFACE)
            IF (.NOT.TEST) CYCLE out300 !GOTO 300
            !                             >>>>>>>>
            QSMALL = - QE.LT.DXY (MOD (IFACE, 2) ) * UHCRIT
            TEST = QSMALL.OR.HSMALL
            !              Gradient Criterion & Neighbour Location
            !              ---------------------------------------
            JEL = afromICMREF (ielc, IFACE, 2)
            IF (JEL.GT.0) THEN
                !                  * regular face
                JFACE = afromICMREF (ielc, IFACE, 3)
                FAIL = GGGETHRF (JEL) .GE.ZE
            ELSEIF (JEL.EQ.0) THEN
                !                  * external boundary
                FAIL = .FALSE.
            ELSE
                !                  * confluence: choose branch with largest flow
                IBR = - JEL
                QQMIN = ZERO
                FAIL = .FALSE.
                out200 : DO PPP = 1, 3  !200
                    PEL = afromICMRF2 (IBR, PPP, 1)
                    IF (PEL.LT.1) CYCLE out200 !GOTO 200
                    PFACE = afromICMRF2 (IBR, PPP, 2)
                    QQ = GGGETQSA (PEL, PFACE) * QE
                    FAILP = (GGGETHRF (PEL) .GE.ZE).AND.(QQ.LT.ZERO)
                    IF ( (FAILP.OR.TEST) .AND.QQ.LT.QQMIN) THEN
                        JEL = PEL
                        JFACE = PFACE
                        QQMIN = QQ
                    ENDIF
                    FAIL = FAIL.OR.FAILP
                    PEL0 = PEL
                    PFACE0 = PFACE
                ENDDO out200 !200
                IF (JEL.LT.0) THEN
                    JEL = PEL0
                    JFACE = PFACE0
                ENDIF
            ENDIF
            !              Adjustments
            !                 -----------
            IF (FAIL.OR.TEST) THEN
                AOK = .FALSE.
                IF (JEL.GT.0) THEN
                    DZA = DTOC / cellarea (JEL)
                    ZA = GGGETHRF (JEL)
                    QA = GGGETQSA (JEL, JFACE)
                ENDIF
                IF (HSMALL) THEN
                    DQE = FDQE * QE
                ELSEIF (QSMALL) THEN
                    DQE = - QE
                ELSE
                    DDZ = DZMIN + ZA - ZE
                    DQE = MIN ( + QA, - QE, DDZ / (DZA + DZE) )
                ENDIF
                Qasum = Qasum + DQE
                !CALL SETQSA(ielc, IFACE, QE+DQE)
                GGGETQSA(ielc, IFACE) = QE+DQE
                ZE = ZE+DQE * DZE
                IF (JEL.GT.0) THEN
                    SGN = SIGN (ONE, DQE)
                    DQA = - SGN * MIN (SGN * DQE, SGN * QA)
                    Qasum = Qasum + DQA
                    !CALL SETQSA(JEL, JFACE, QA + DQA)
                    GGGETQSA(JEL, JFACE) = QA + DQA
                    !CALL SETHRF(JEL, ZA + DQA * DZA)
                    GGGETHRF(JEL) = ZA + DQA * DZA
                ENDIF
                IF (.NOT.HSMALL) THEN
                    DHQ = Qasum * DZE
                    Qasum = ZERO
             ! sb 021009 Error message always produced if pass.eq.npass
             IF ((ABS (DHQ) .GT.HERROR) .or.(passs.eq.npass)) THEN
                    rdum4(1)= - QE ; rdum4(2)=- 1D2 * DQE / QE ; idum=IFACE ; rdum4(4)=DHQ !AD
                        WRITE (MSG, 91030) rdum4(1:2),idum,rdum4(4:4)
                        CALL ERROR(WWWARN, 1030, PPPRI, ielc, 0, MSG)
                    ENDIF
                ENDIF
            ENDIF
        ENDDO out300
        !           Final Depth Adjustment
        !           ----------------------
        IF (HSMALL) THEN
            !^^^^ RAH/SB small flows ^^^^^^^^^^^^^^^^^^^
            AOK = .FALSE.
            DHQ = Qasum * DZE
            DHH = ZG - ZE
            ZE = ZG
            ! sb 021009 Error message always produced if pass.eq.npass
            IF ((ABS (DHQ) + ABS (DHH) .GT.HERROR) .or.(passs.eq.npass)) THEN
            rdum4(1)=H ; rdum4(2)=DHQ ; rdum4(3)=DHH  !AD
                WRITE (MSG, 91024) rdum4(1:3)
                CALL ERROR(WWWARN, 1024, PPPRI, ielc, 0, MSG)
            ENDIF
        ENDIF
        !CALL SETHRF(ielc, ZE)
        GGGETHRF(ielc) =ZE
        ! End of Control Loop
        ! -------------------
    ENDDO out400
    !IF (AOK) EXIT out900 !GOTO 901
ENDDO out900
IF(.not.aok) CALL ERROR(WWWARN, 1060, PPPRI, 0, 0, 'OC flow criteria could not be met')

!901 CONTINUE
!33+15+8+17+30=103

91024 FORMAT( 'Surface water depth adjusted from',SP,1PG15.7,' to zero',         ': depth created =',2G15.7 )
!28+14+11+7+9+2+17+15=103

91030 FORMAT( 'Surface water discharge rate',1PG14.7,' reduced by', &
        0PF7.2,'% at face',I4,': depth created =',SP,1PG15.7 )
END SUBROUTINE OCFIX


END MODULE OCmod2
