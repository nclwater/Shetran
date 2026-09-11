!> summary: Face discharge for each kind of overland/channel face.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> One routine per face type: [[OCQGRD]] for a grid-to-grid face, [[OCQLNK]]
!> and [[OCQMLN]] for channel links and multi-link confluences, [[OCQBNK]] for
!> a bank face, [[OCQBC]] for a boundary face, and [[QWEIR]] for weir flow —
!> which is where the reservoir stage-discharge tables of [[zq_tables]] are
!> consulted when they are enabled.
!>
!> All of them evaluate conveyance through [[oc_conveyance:CONVEYAN]], which is
!> in its own module so that this one and [[oc_node_solver]] need not `USE`
!> each other.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCmod2; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_discharge

   USE MOD_PARAMETERS, ONLY: half, one, zero, TWO_THIRDS, SQRT_TWO_G
   USE oc_state, ONLY: xstab
   USE oc_conveyance, ONLY: CONVEYAN, DZMIN, RDZMIN
   USE oc_node_solver, ONLY: OCNODE, OCCODE
   USE zq_tables, ONLY: get_ZQTable_value, ZQTableRef, ZQweirSill
   USE float_compare, ONLY: dimje

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: OCQBC, OCQBNK, OCQGRD, OCQLNK, OCQMLN

CONTAINS

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
         AH = AFROMCOCBCD(1)*H
         B = AFROMCOCBCD(2)
         C = AFROMCOCBCD(3)
         D = AFROMCOCBCD(4)
         E = AFROMCOCBCD(5)

         FROMQ = -((((AH + B)*H + C)*H + D)*H + E)
         FROMDQ = -(((4.0D0*AH + 3.0D0*B)*H + 2.0D0*C)*H + D)

      CASE DEFAULT
         ! Weir (7) ... with river in parallel (8) - see Part 2
         IF (NTYPE == 7 .OR. NTYPE == 8) THEN
            COEFF(1) = AFROMCOCBCD(1)
            SUBRIO = AFROMCOCBCD(2)
            ZSILL = AFROMCOCBCD(3)
            ZX = AFROMCOCBCD(4)
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
         DZ = SIG*DZ
         ROOTDZ = SQRT(DZ)
         DHH = LI*DBLE(4 - MTYPE)
         ROOTL = SQRT(DHH)

         IF (NTYPE == 3) THEN
            HM = ZI - ZGI
            STRW = STR*W
            CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)
         ELSE
            CALL OCCODE(ZGI, STR, W, AFROMXAFULL, XSTAB(:, :, LINK), ZI, CONVM, DERIVM)
         END IF

         CONVMM = CONVM + DERIVM*DIMJE(DZMIN, DZ)
         DUM = HALF*CONVMM/MAX(RDZMIN, ROOTDZ)

         FROMQ = FROMQ + SIG*CONVM*ROOTDZ/ROOTL
         FROMDQ = FROMDQ + (SIG*DERIVM*ROOTDZ - DUM)/ROOTL
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
      HI = (1 + NINT(SIG))/2
      LO = 1 - HI
      ZB = ZBG(0)
      ZG = ZBG(1)

      DZL = ZI(LO) - ZB

      ! Channel bank-full lower than adjacent ground: resistance equation
      ! NB: HM has an implicit upstream weighting factor, ie ALPHA=1
      IF (ZG >= ZB) THEN
         DZ = SIG*DZ + MIN(DZL, ZERO)
         ROOTDZ = SQRT(DZ)
         HM = ZI(HI) - ZBG(HI)

         DHH = LI(0) + LI(1)
         STRW = W*(STR(0)*LI(0) + STR(1)*LI(1))/DHH
         ROOTL = SQRT(DHH)

         CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)

         CONVMM = CONVM + DERIVM*DIMJE(DZMIN, DZ)
         DUM = HALF*CONVMM/MAX(RDZMIN, ROOTDZ)

         Q(LO) = CONVM*ROOTDZ/ROOTL
         DQ(LO, HI) = (DERIVM*ROOTDZ + DUM)/ROOTL

         IF (DZL < -DZMIN) DUM = ZERO

         DQ(LO, LO) = -DUM/ROOTL

         ! Channel bank-full higher than adjacent ground: flat-crested weir eqn
      ELSE
         COEFF(1) = SQRT_TWO_G*W
         COEFF(2) = 0.386D0*COEFF(1)

         ! AD aliasing fix: rdum isolates the output variable from DQ array memory
         CALL QWEIR(ZI(HI), ZB, ZI(LO), COEFF, TWO_THIRDS, Q(LO), DQ(LO, HI), RDUM)
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
      HI = (1 + NINT(SIG))/2
      LO = 1 - HI
      DZ = SIG*DZ
      ROOTDZ = SQRT(DZ)
      HM = ZI(HI) - ZGI(HI)

      DHH = LI(0) + LI(1)
      STRW = W*(STR(0)*LI(0) + STR(1)*LI(1))/DHH
      ROOTL = SQRT(DHH)

      ! CALCULATE FLOW AND DERIVATIVES
      ! NB:   H23MIN          in DERIVM  prevents small DQ when HM is small
      !        DZMIN          in CONVMM  prevents small DQ when DZ is small
      !       RDZMIN          in DUM     prevents overflow when DZ is small
      !       ROOTDZ (no MAX) in DQ gives symmetric values when DZ is small

      CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)

      CONVMM = CONVM + DERIVM*DIMJE(DZMIN, DZ)
      DUM = HALF*CONVMM/MAX(RDZMIN, ROOTDZ)

      Q(LO) = CONVM*ROOTDZ/ROOTL
      DQ(LO, HI) = (DERIVM*ROOTDZ + DUM)/ROOTL

      DQ(LO, LO) = -DUM/ROOTL
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
      HI = (1 + NINT(SIG))/2
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
         DQ(LO, HI) = 50.0D0*1.5D0*SQRT(DZU)
         DQ(LO, LO) = 0.0D0

         ! write(779,*) ZI(HI), Q(LO), DQ(LO,HI)

         ! Standard Channel Flow
      ELSE
         ! Set up local variables - part 2
         DZ = SIG*DZ
         ROOTDZ = SQRT(DZ)
         DHH = LI(0) + LI(1)
         ROOTL = SQRT(DHH)

         ! CALCULATE FLOW AND DERIVATIVES
         ! NB: CONVM has an implicit upstream weighting factor, ie ALPHA=1
         CALL OCCODE(ZGI(HI), STR(HI), CW(HI), XA(HI), XSTAB(:, :, JXSWORK(HI)), ZI(HI), CONVM, DERIVM)

         CONVMM = CONVM + DERIVM*DIMJE(DZMIN, DZ)
         DUM = HALF*CONVMM/MAX(RDZMIN, ROOTDZ)

         ! Note: ZGI(LO), etc are not required
         Q(LO) = CONVM*ROOTDZ/ROOTL
         DQ(LO, HI) = (DERIVM*ROOTDZ + DUM)/ROOTL
         DQ(LO, LO) = -DUM/ROOTL
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

         ZINC = MAX(WLMIN, (ZSAVE - ZGI(J))*ONEPC)  ! zgi is ground elevation
         ZJ(J) = ZSAVE + ZINC

         ! Modernization Fix: Changed scalar array pass (XSTAB(1,1,...)) to full slice to match OCCODE interface
         CALL OCCODE(ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, JXSWORK(J)), ZJ(J), CI(J), DI(J))

         ! * calculate resultant flows & evaluate derivative
         CALL OCNODE(IELB, ZJ, CI, DI, ROOTLI, QDUM2)

         DO I = 0, 3
            DQIJ(I, J) = (QDUM2(I) - QJ(I))/ZINC
         END DO

         ZJ(J) = ZSAVE
         CI(J) = CSAVE
         DI(J) = DSAVE
      END DO

   END SUBROUTINE OCQMLN

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
         IF (DZL > SUBRIO*DZU) THEN
            ROOTDZ = SQRT(ZU - ZL)
            DML = MAX(DZMIN, DZL)
            CR = COEFF(1)*ROOTDZ
            Q = CR*DZL
            DQU = COEFF(1)*DML*HALF/MAX(RDZMIN, ROOTDZ)
            DQL = CR - DQU

            ! UNDROWNED WEIR
         ELSE
            ROOTDZ = SQRT(DZU)
            Q = COEFF(2)*DZU*ROOTDZ
            DQU = COEFF(2)*1.5D0*MAX(RDZMIN, ROOTDZ)
            DQL = ZERO
         END IF
      END IF

   END SUBROUTINE QWEIR

END MODULE oc_discharge

