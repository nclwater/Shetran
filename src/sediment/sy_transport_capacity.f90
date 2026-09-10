!> summary: Transport capacity, critical shear stress and the size-class properties derived from them.
!> author: AB / RAH / BTL, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> How much sediment the flow can carry. [[SYCLTR]] evaluates channel capacity
!> through either [[SYACKW]] (Ackers-White and Ackers-White-Day) or [[SYENGH]]
!> (Engelund-Hansen); [[SYOVTR]] evaluates overland capacity by the Yalin or
!> Engelund-Hansen option; [[SYCRIT]] returns the critical shear stress both
!> use; [[SYDR]] derives the representative diameters. `FDGR` and `FA` are
!> contained inside `SYACKW`.
!>
!> The module also holds the six derived coefficients and first-call values
!> that belong to these formulae rather than to the input: `K1_syovtr`,
!> `K3_syovtr` and `K4_syovtr` are compile-time combinations of the physical
!> constants, and `WSED_syfine` caches the fine-sediment settling velocity.
!>
!> @note
!> `FIRST_syackw` is no longer read or written anywhere. The one-time
!> Ackers-White constant setup it used to guard was converted to compile-time
!> `PARAMETER`s local to [[SYACKW]], because the physical constants it depends
!> on (`GRAVITY`, `RHO_SEDIMENT`, `RHO_WATER_SEDIMENT`, `NU_WATER` from
!> [[mod_parameters]]) are themselves `PARAMETER`s. It is dead state left
!> behind by that change.
!>
!> `FIRST_syfine` and `WSED_syfine` are the first-call state of
!> [[sy_hillslope:SYFINE]], which is in a sibling module; they are placed here
!> by `variables.csv` and are public for that reason.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into a single Fortran 90 module. |
!> | 2026-04 to 2026-05 | SvB | 4.6.1 | Modernised the whole component: free-form layout, `IMPLICIT NONE`/`INTENT` throughout, structured control flow in place of `GOTO`s, compile-time `PARAMETER`s for the cached first-call constants, and `symain`'s work arrays moved to allocate-once module storage. |
!> | 2026-09-10 | SvB | - | Split out of SYmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_transport_capacity

   USE MOD_PARAMETERS, ONLY: half, one, zero, GRAVITY, RHO_SEDIMENT, &
                             RHO_WATER_SEDIMENT, NU_WATER
   USE float_compare, ONLY: dimje, gtzero, iszero

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SYCLTR, SYCRIT, SYDR, SYOVTR
   PUBLIC :: FIRST_syfine, WSED_syfine

   LOGICAL         :: FIRST_syackw = .TRUE. !! Unused; see the module-level note above `MODULE SYmod`.
   LOGICAL         :: FIRST_syfine = .TRUE. !! True until the fine-sediment settling velocity `WSED_syfine` has been cached.
   DOUBLEPRECISION :: WSED_syfine         !! Cached fine-sediment settling velocity, set on the first call to [[syfine]].
   DOUBLE PRECISION, PARAMETER :: K1_syovtr = 0.05D0*RHO_WATER_SEDIMENT**2/((RHO_SEDIMENT - RHO_WATER_SEDIMENT)**2*SQRT(GRAVITY)) !! Engelund-Hansen overland-capacity coefficient.
   DOUBLE PRECISION, PARAMETER :: K3_syovtr = 2.45D0*(RHO_SEDIMENT/RHO_WATER_SEDIMENT)**(-0.4D0)/SQRT((RHO_SEDIMENT - RHO_WATER_SEDIMENT)*GRAVITY) !! Yalin overland-capacity coefficient.
   DOUBLE PRECISION, PARAMETER :: K4_syovtr = 0.635D0/SQRT(RHO_WATER_SEDIMENT) !! Yalin overland-capacity coefficient.

CONTAINS

!> Calculates Ackers-White channel transport capacity for non-fine sediment.
!>
!> This is the `ISACKW = 1` or `ISACKW = 2` channel-capacity option described
!> in the sediment manual. It is called by [[sycltr]] only for non-fine size
!> groups (`NFINE+1:NSED`); fine material is handled separately because the
!> manual limits fine channel capacity directly by `FPCRIT`.
!>
!> For a sediment diameter \(d\), relative submerged density
!> \(s - 1 = \rho_s / \rho_w - 1\), and kinematic viscosity \(\nu\), the
!> routine first forms the Ackers-White dimensionless grain size
!>
!> \[
!>   D_* = \max\left(1, \min\left(d\left({g(s-1)\over\nu^2}\right)^{1/3},
!>          10^{1/0.56}\right)\right).
!> \]
!>
!> For the standard Ackers-White option (`ISACKW = 1`), the per-size parameters
!> cached in `ACKW` are
!>
!> \[
!> \begin{aligned}
!>   n &= \max(1 - 0.56\log_{10}D_*, 0),\\
!>   A &= 0.14 + {0.23\over\sqrt{D_*}},\\
!>   m &= 1.34 + {9.66\over D_*},\\
!>   C &= 10^{(2.86-\log_{10}D_*)\log_{10}D_* - 3.53}.
!> \end{aligned}
!> \]
!>
!> For the Day modification (`ISACKW = 2`), the mobility threshold \(A\) is
!> recalculated for each link end from the current bed-size distribution:
!>
!> \[
!>   d_a = 1.62D_{50}\left({D_{16}\over D_{84}}\right)^{0.28},\qquad
!>   A_s = A(d_a)\left(0.6 + 0.4\sqrt{d_a/d_s}\right),
!> \]
!>
!> where \(D_{16}\), \(D_{50}\), and \(D_{84}\) are obtained from `DCBSED`
!> with [[sydr]], and \(d_s\) is the representative diameter of the current
!> size class.
!>
!> For each outflowing link end, with outflow \(Q\), water depth \(h\), mean
!> velocity \(U = Q/ARXL\), shear velocity \(u_*=\sqrt{\tau/\rho_w}\), and
!> \(U_g = U / \{\sqrt{32}\log_{10}(10h/d)\}\), the mobility and capacity are
!>
!> \[
!>   F = {u_*^n U_g^{1-n}\over\sqrt{g(s-1)d}},
!> \]
!>
!> \[
!>   G = d{Q\over h}C\max\left({F\over A}-1,0\right)^m
!>       \left({U\over u_*}\right)^n .
!> \]
!>
!> `GSED(link,sed)` accumulates \(G\) over the outflowing ends of each link.
!> Concentration caps, sediment velocity selection, and exchange with
!> suspended and bed material are applied later in [[sycltr]] and [[sylink]].
!>
!> @note Link ends are mapped onto `QOC` faces 1 and 3 for east-west links, or
!> faces 2 and 4 for north-south links. Only outflowing ends contribute to
!> `GSED`; non-outflowing ends leave the accumulated capacity unchanged.
!> @endnote
!>
!> @note The base of the capacity exponent \((F/A-1)\) is computed once as
!> `BASE` and the `**MAW` power and `GSED` accumulation are skipped entirely
!> when `BASE <= 0`. This is equivalent to the original
!> `DIMJE(F/A,1)**MAW`-based formula (which forces a zero contribution the
!> same way) but avoids evaluating a real power of a known-zero base.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-06 | SvB | 4.6.1 | Removed `GOTO`-driven control flow; replaced the legacy statement functions `FDGR`/`FA` with internal `FUNCTION`s. |
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `K2_syackw`/`DGRMAX_syackw`/`ROOT32_syackw` with compile-time `PARAMETER`s; skipped the capacity exponentiation/accumulation when its base is non-positive (see the preceding note on `BASE`). |
!> @endhistory
   PURE SUBROUTINE SYACKW(NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, DRSED, ARXL, DCBSED, DWAT1, &
                          QOC, TAUJ, ACKW, GSED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISACKW !! Ackers-White option: 1 standard, 2 Day bed-percentile modification.
      INTEGER, INTENT(IN) :: NFINE  !! Number of fine sediment classes excluded from this calculation.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      LOGICAL, INTENT(IN)          :: LINKNS(NLF)                  !! True for north-south channel links.
      DOUBLE PRECISION, INTENT(IN) :: DRSED(NFINE + 1:NSED)        !! Representative non-fine particle diameters.
      DOUBLE PRECISION, INTENT(IN) :: ARXL(NLF)                    !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1(NLF)                   !! Channel water depth by link.
      DOUBLE PRECISION, INTENT(IN) :: QOC(NELEE, 4)                !! Face water fluxes.
      DOUBLE PRECISION, INTENT(IN) :: DCBSED(NLFEE, NFINE + 1:NSED) !! Active-bed sediment depth by link and non-fine class.
      DOUBLE PRECISION, INTENT(IN) :: TAUJ(NELEE, 4)               !! Face shear stress.

      ! In/Out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: ACKW(5, NFINE + 1:NSED) !! Cached Ackers-White parameters by non-fine class.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT)   :: GSED(NLF, NFINE + 1:NSED) !! Channel transport capacity by link and non-fine class.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: DGRSML = 1.0D-4
      DOUBLE PRECISION, PARAMETER :: F16 = 0.16D0, F50 = 0.5D0, F56 = 0.56D0, F84 = 0.84D0
      DOUBLE PRECISION, PARAMETER :: THIRD = 1.0D0/3.0D0

      DOUBLE PRECISION, PARAMETER :: KRHO = RHO_SEDIMENT/RHO_WATER_SEDIMENT - 1.0D0
      DOUBLE PRECISION, PARAMETER :: K2_syackw = (GRAVITY*KRHO/NU_WATER**2)**THIRD
      DOUBLE PRECISION, PARAMETER :: DGRMAX_syackw = 10.0D0**(ONE/F56) + DGRSML
      DOUBLE PRECISION, PARAMETER :: ROOT32_syackw = SQRT(32.0D0)
      DOUBLE PRECISION :: AAW, ARXLE, CAW, DAAA, DBED16, DBED50, DBED84, DGR
      DOUBLE PRECISION :: DSED, DWAT1E, FGR, G, H10, LGR, MAW
      DOUBLE PRECISION :: NAW, QK, UGR, USTR, UK, BASE
      INTEGER          :: FACE, IEND, LINK, NFP1, NNF, SED, SGN

      !----------------------------------------------------------------------*

      ! Initialization
      NNF = NSED - NFINE
      NFP1 = NFINE + 1

      DO SED = NFP1, NSED
         DGR = FDGR(DRSED(SED))
         LGR = LOG10(DGR)
         ACKW(1, SED) = MAX(ZERO, ONE - F56*LGR) ! Replaced DIMJE with standard intrinsic
         IF (ISACKW == 1) ACKW(2, SED) = FA(DGR)
         ACKW(3, SED) = 1.34D0 + 9.66D0/DGR
         ACKW(4, SED) = 10.0D0**((2.86D0 - LGR)*LGR - 3.53D0)
         ACKW(5, SED) = ONE/SQRT(GRAVITY*KRHO*DRSED(SED))
      END DO

      ! Zero GSED array slice
      GSED(:, :) = ZERO

      ! Loop over ends of each link
      DO IEND = 1, 3, 2
         SGN = 2 - IEND

         ! Loop over all channel links
         DO LINK = 1, NLF

            ! Determine face equivalent to this end, and flow rate there
            FACE = IEND
            IF (LINKNS(LINK)) FACE = FACE + 1
            QK = SGN*QOC(LINK, FACE)

            ! Check that this end is outflowing
            IF (QK > ZERO) THEN

               ! Copy array elements to local variables
               ARXLE = ARXL(LINK)
               DWAT1E = DWAT1(LINK)
               H10 = 10.0D0*DWAT1E

               ! Determine shear velocity and water flow velocity
               USTR = SQRT(TAUJ(LINK, FACE)/RHO_WATER_SEDIMENT)
               UK = ZERO
               IF (ARXLE > ZERO) UK = QK/ARXLE

               ! Set A-W parameters for the Day modification if needed
               IF (ISACKW == 2) THEN

                  DBED84 = SYDR(F84, NLFEE, NNF, DCBSED(LINK, NFP1), DRSED(NFP1))

                  IF (DBED84 > ZERO) THEN
                     DBED50 = SYDR(F50, NLFEE, NNF, DCBSED(LINK, NFP1), DRSED(NFP1))
                     DBED16 = SYDR(F16, NLFEE, NNF, DCBSED(LINK, NFP1), DRSED(NFP1))
                     DAAA = 1.62D0*DBED50*(DBED16/DBED84)**0.28D0
                  ELSE
                     DAAA = ZERO
                  END IF

                  DGR = FDGR(DAAA)
                  AAW = FA(DGR)

                  DO SED = NFP1, NSED
                     ACKW(2, SED) = AAW*(0.6D0 + 0.4D0*SQRT(DAAA/DRSED(SED)))
                  END DO

               END IF

               ! Loop over sediment types
               DO SED = NFP1, NSED

                  ! Set A-W parameters for this Sediment size group
                  NAW = ACKW(1, SED)
                  AAW = ACKW(2, SED)
                  MAW = ACKW(3, SED)
                  CAW = ACKW(4, SED)
                  DSED = DRSED(SED)

                  ! Calculate particle mobility
                  UGR = ZERO
                  IF (DSED < H10) UGR = UK/(ROOT32_syackw*LOG10(H10/DSED))
                  FGR = ACKW(5, SED)
                  IF (NAW > ZERO) FGR = FGR*USTR**NAW
                  IF (NAW < ONE) FGR = FGR*UGR**(ONE - NAW)

                  ! Determine discharge capacity for this end
                  ! High-Performance Fix: Do not perform exponentiation (0.0**MAW) if base is zero or less.
                  IF (DWAT1E > ZERO) THEN
                     BASE = (FGR/AAW) - ONE
                     IF (BASE > ZERO) THEN
                        G = DSED*(QK/DWAT1E)*CAW*(BASE**MAW)
                        IF (NAW > ZERO) G = G*(UK/USTR)**NAW

                        ! Determine the total discharge capacity of both ends
                        GSED(LINK, SED) = GSED(LINK, SED) + G
                     END IF
                  END IF

               END DO

            END IF

         END DO
      END DO

   CONTAINS

      !> Clamps a scaled diameter to the Ackers-White dimensionless grain-size range \([1,D_{*,max}]\).
      ELEMENTAL FUNCTION FDGR(DUM_VAL) RESULT(RES)
         DOUBLE PRECISION, INTENT(IN) :: DUM_VAL !! Scaled particle diameter.
         DOUBLE PRECISION :: RES !! Clamped dimensionless grain size \(D_*\).
         RES = MAX(ONE, MIN(K2_syackw*DUM_VAL, DGRMAX_syackw))
      END FUNCTION FDGR

      !> Ackers-White mobility-threshold parameter \(A\) for a given dimensionless grain size.
      ELEMENTAL FUNCTION FA(DUM_VAL) RESULT(RES)
         DOUBLE PRECISION, INTENT(IN) :: DUM_VAL !! Dimensionless grain size \(D_*\).
         DOUBLE PRECISION :: RES !! Mobility-threshold parameter \(A\).
         RES = 0.14D0 + 0.23D0/SQRT(DUM_VAL)
      END FUNCTION FA

   END SUBROUTINE SYACKW

!> Determines channel sediment transport capacity and advection coefficients.
!>
!> `SYCLTR` implements the manual's channel transport switches before channel
!> routing in [[sylink]]. Non-fine streamwise capacity `GSED` is calculated by
!> [[syengh]] when `ISACKW = 0`, or by [[syackw]] when `ISACKW = 1` or `2`.
!> Fine sediment capacity is not calculated by these formulae; for every fine
!> size group, the notional channel capacity concentration is set directly to
!> `FPCRIT`.
!>
!> The routine also builds the advection coefficients `QSDWAT` used to move
!> sediment with water through outflow faces. If `ISUSED = 0`, the manual's
!> water-speed option is used for every size class:
!>
!> \[
!>   QSDWAT_{s,f} = Q_f \qquad (Q_f > 0).
!> \]
!>
!> If `ISUSED = 1`, fine material still moves at the water speed, while each
!> non-fine fraction at link ends is limited by a shear-dependent velocity:
!>
!> \[
!>   QSDWAT_s =
!>   \min\left(
!>     {8.5\,ARXL\over\sqrt{\rho_w}}\,
!>     \sqrt{\max(\tau-\sqrt{\tau\tau_c},0)},\; Q
!>   \right),
!> \]
!>
!> where \(\tau_c\) is returned by [[sycrit]] using the Shields option for the
!> current particle diameter. `QSWSUM` accumulates these end-face coefficients
!> for use in converting discharge capacity into concentration capacity.
!>
!> For each non-fine fraction, capacity concentration is allocated using the
!> larger of a small background share, the existing suspended composition, and
!> the active-bed composition:
!>
!> \[
!>   f_s = \max\left(0.05,\,
!>                  {FDEL_s\over\sum_n FDEL_n},\,
!>                  {DCBSED_s\over DCBED}\right),
!> \]
!>
!> \[
!>   CONCI_s =
!>   \begin{cases}
!>     \min\left(FPCRIT,\; f_s\,GSED_s / QSWSUM_s\right), & QSWSUM_s > 0,\\
!>     0, & QSWSUM_s = 0.
!>   \end{cases}
!> \]
!>
!> For side outflows with `ISUSED = 1`, non-fine exchange is suppressed until
!> the notional concentration exceeds the manual overbank threshold `CONCOB`:
!>
!> \[
!>   QSDWAT_{s,side} =
!>   \begin{cases}
!>     Q_{side}{\max(CONCI_s-CONCOB,0)\over CONCI_s}, & CONCI_s > 0,\\
!>     0, & CONCI_s = 0.
!>   \end{cases}
!> \]
!>
!> @note `QSDWAT` entries are assigned only for outflow faces handled by this
!> routine. Callers should not interpret non-outflow entries as newly computed
!> values unless they have been cleared before the call.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-05 | SvB | 4.6.1 | Replaced the `ALINIT` zeroing of `QSWSUM` and the per-fine-class `ALINIT` of `CONCI` with array-slice assignment. |
!> | 2026-04-06 | SvB | 4.6.1 | Removed `GOTO`-driven control flow. |
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `K1_sycltr` with a compile-time `PARAMETER`. |
!> @endhistory
   SUBROUTINE SYCLTR(CONCOB, FPCRIT, ISACKW, ISUSED, NELEE, NFINE, NLF, NLFEE, NSED, NSEDEE, &
                     DRSED, ARXL, CWIDTH, DCBED, LINKNS, DWAT1, QOC, SLOPEJ, DCBSED, FDEL, TAUJ, &
                     ACKW, CONCI, QSDWAT, GSED, QSWSUM)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISACKW !! Channel transport-capacity option.
      INTEGER, INTENT(IN) :: ISUSED !! Sediment velocity option.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NFINE  !! Number of fine sediment classes.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      DOUBLE PRECISION, INTENT(IN) :: CONCOB !! Mobile concentration threshold for overbank exchange.
      DOUBLE PRECISION, INTENT(IN) :: FPCRIT !! Maximum sediment concentration fraction.
      DOUBLE PRECISION, INTENT(IN) :: DRSED(NFINE + 1:NSED) !! Representative non-fine particle diameters.
      DOUBLE PRECISION, INTENT(IN) :: ARXL(NLF)    !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(IN) :: CWIDTH(NLF)  !! Channel width by link.
      DOUBLE PRECISION, INTENT(IN) :: DCBED(NLF)   !! Active upper-bed layer depth by link.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1(NLF)   !! Channel water depth by link.
      DOUBLE PRECISION, INTENT(IN) :: QOC(NELEE, 4) !! Face water fluxes.
      DOUBLE PRECISION, INTENT(IN) :: SLOPEJ(NELEE, 4) !! Face water-surface slopes.
      DOUBLE PRECISION, INTENT(IN) :: DCBSED(NLFEE, NFINE + 1:NSED) !! Active-bed sediment depth by link and non-fine class.
      DOUBLE PRECISION, INTENT(IN) :: FDEL(NELEE, NFINE + 1:NSED)   !! Mobile sediment concentration fraction by element and non-fine class.
      DOUBLE PRECISION, INTENT(IN) :: TAUJ(NELEE, 4) !! Face shear stress.
      LOGICAL, INTENT(IN)          :: LINKNS(NLF)    !! True for north-south channel links.

      ! Input/output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: ACKW(5, NFINE + 1:NSED) !! Cached Ackers-White parameters by non-fine class.

      ! Output arguments
      ! NB: QSDWAT defined for outflow faces only
      DOUBLE PRECISION, INTENT(OUT)   :: CONCI(NLFEE, NSED)      !! Capacity concentration by link and sediment class.
      DOUBLE PRECISION, INTENT(OUT)   :: QSDWAT(NLFEE, NSEDEE, 4) !! Sediment advection coefficient for outflow faces only.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: GSED(NLF, NFINE + 1:NSED) !! Channel transport capacity workspace.
      DOUBLE PRECISION, INTENT(INOUT) :: QSWSUM(NLF, NSED)         !! Sum of outflowing sediment advection coefficients by link/class.

      ! Locals, etc
      DOUBLE PRECISION, PARAMETER :: ZZ5 = 0.05D0
      DOUBLE PRECISION, PARAMETER :: k1_sycltr = 8.5D0/SQRT(RHO_WATER_SEDIMENT)

      INTEGER :: FACE, IEND, ISIDE, LINK, NFP1, NSDWAT, SED, SGN
      DOUBLE PRECISION :: CONCID, DCSUM, DUM, FDSUM, FRACT, KQ, QK
      DOUBLE PRECISION :: TAUEC, TAUD, QSW, FRACT1, FRACT2
      LOGICAL :: BODD

      !----------------------------------------------------------------------*

      NFP1 = NFINE + 1

      ! Replaced ALINIT with array slice operation
      QSWSUM(1:NLF, 1:NSED) = ZERO

      ! Streamwise capacity discharge rates ...
      ! ---------------------------------------
      !
      !     ... using specified method
      !
      IF (ISACKW == 1 .OR. ISACKW == 2) THEN
         CALL SYACKW(NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, DRSED, ARXL, DCBSED, DWAT1, &
                     QOC, TAUJ, ACKW, GSED)
      ELSE
         CALL SYENGH(NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, QOC, LINKNS, SLOPEJ, GSED)
      END IF

      ! Advection Coefficients (outflow faces only) Part 1 ...
      ! ------------------------------------------------------
      !
      !     ... for size groups which move with water velocity
      !
      NSDWAT = NFINE
      IF (ISUSED == 0) NSDWAT = NSED

      IF (NSDWAT > 0) THEN
         ! * All faces (both ends and sides)
         DO FACE = 1, 4
            SGN = SIGN(1, 2 - FACE)
            BODD = MOD(FACE, 2) == 1

            ! * All links (but skip over non-outflow faces)
            DO LINK = 1, NLF
               QK = SGN*QOC(LINK, FACE)
               IF (GTZERO(QK)) THEN

                  ! * Set QSWSUM increment for link ends only
                  QSW = ZERO
                  IF (BODD .NEQV. LINKNS(LINK)) QSW = QK

                  ! * Fines only, or all size groups, as appropriate
                  DO SED = 1, NSDWAT
                     QSDWAT(LINK, SED, FACE) = QK
                     ! * Don't actually need QSWSUM for fines, but ...
                     QSWSUM(LINK, SED) = QSWSUM(LINK, SED) + QSW
                  END DO

               END IF
            END DO
         END DO
      END IF

      ! Advection Coefficients (outflow faces only)  Part 2 ...
      ! -------------------------------------------------------
      !
      !     ... at link ends for each size group which moves with an
      !         independent velocity.
      !
      IF (ISUSED == 1) THEN

         ! * Loop over both ends ( of every link )
         DO IEND = 1, 3, 2
            SGN = 2 - IEND

            ! * Loop over every link (but skip over non-outflow faces)
            DO LINK = 1, NLF
               FACE = IEND
               IF (LINKNS(LINK)) FACE = FACE + 1
               QK = SGN*QOC(LINK, FACE)

               IF (GTZERO(QK)) THEN

                  TAUD = TAUJ(LINK, FACE)
                  KQ = K1_sycltr*ARXL(LINK)

                  ! * Loop over non-fine size groups
                  DO SED = NFP1, NSED
                     CALL SYCRIT(0, DRSED(SED), TAUD, DUM, TAUEC)
                     QSW = MIN(KQ*SQRT(DIMJE(TAUD, SQRT(TAUD*TAUEC))), QK)
                     QSDWAT(LINK, SED, FACE) = QSW
                     QSWSUM(LINK, SED) = QSWSUM(LINK, SED) + QSW
                  END DO

               END IF

               ! * Next link
            END DO

            ! * Next end
         END DO

      END IF

      ! Determine notional particle concentrations at flow capacity
      ! -----------------------------------------------------------
      !
      ! * Loop over fines (Replaced ALINIT loop with single slice assignment)
      ! Note: Assuming FPCRIT is properly assigned to the entire NLF dimension for each fine
      CONCI(1:NLF, 1:NFINE) = FPCRIT

      ! * Loop over links
      DO LINK = 1, NLF

         ! * Determine denominators for scaling factors
         FDSUM = ZERO
         DO SED = NFP1, NSED
            FDSUM = FDSUM + FDEL(LINK, SED)
         END DO
         IF (ISZERO(FDSUM)) FDSUM = ONE

         DCSUM = DCBED(LINK)
         IF (ISZERO(DCSUM)) DCSUM = ONE

         ! * Loop over non-fines
         DO SED = NFP1, NSED
            QSW = QSWSUM(LINK, SED)
            IF (GTZERO(QSW)) THEN
               FRACT1 = FDEL(LINK, SED)/FDSUM
               FRACT2 = DCBSED(LINK, SED)/DCSUM
               FRACT = MAX(ZZ5, FRACT1, FRACT2)
               CONCID = MIN(FPCRIT, FRACT*GSED(LINK, SED)/QSW)
            ELSE
               CONCID = ZERO
            END IF
            CONCI(LINK, SED) = CONCID
         END DO

      END DO

      ! Advection Coefficients (outflow faces only) Part 3 ...
      ! ------------------------------------------------------
      !
      !     ... at link sides, for each size group which moves at an
      !         independent velocity.
      !
      IF (ISUSED == 1) THEN

         ! * Loop over both sides
         DO ISIDE = 2, 4, 2
            SGN = 3 - ISIDE

            ! * Loop over every link (but skip over non-outflow sides)
            DO LINK = 1, NLF
               FACE = ISIDE
               IF (LINKNS(LINK)) FACE = ISIDE - 1
               QK = SGN*QOC(LINK, FACE)

               IF (GTZERO(QK)) THEN

                  ! * Loop over non-fine size groups
                  DO SED = NFP1, NSED
                     DUM = CONCI(LINK, SED)
                     IF (GTZERO(DUM)) DUM = QK*DIMJE(DUM, CONCOB)/DUM
                     QSDWAT(LINK, SED, FACE) = DUM
                  END DO

               END IF

               ! * Next link
            END DO

            ! * Next side
         END DO

      END IF

   END SUBROUTINE SYCLTR

!> Calculates critical shear stress for incipient sediment motion.
!>
!> `SYCRIT` implements the two critical-shear options referenced by the
!> sediment manual's `ISTEC` switch. Callers pass `FLAG = 1` for the simple
!> clay-content relation, and any other value for the Shields-style relation.
!>
!> With `FLAG = 1`, the critical shear stress is calculated directly from the
!> fractional clay content `FPCLAE`:
!>
!> \[
!>   \tau_c = 0.493\exp\left(1.83\ln(10)\,FPCLAE\right).
!> \]
!>
!> Otherwise, the routine computes a particle Reynolds number using the current
!> applied shear `TAUX`, representative diameter `DRX50`, water density
!> \(\rho_w\), and kinematic viscosity \(\nu\):
!>
!> \[
!>   R_* =
!>   \max\left(0.03,\min\left({DRX50\sqrt{TAUX}\over\sqrt{\rho_w}\nu},
!>   400\right)\right).
!> \]
!>
!> A coefficient pair \((a,b)\) is selected from the interval containing
!> \(R_*\):
!>
!> | Range of \(R_*\) | \(a\) | \(b\) |
!> |:-----------------|:------|:------|
!> | \(0.03 \le R_* \le 1\) | 0.1 | -0.3 |
!> | \(1 < R_* \le 6\) | 0.1 | -0.62 |
!> | \(6 < R_* \le 30\) | 0.033 | 0 |
!> | \(30 < R_* \le 135\) | 0.013 | 0.28 |
!> | \(135 < R_* \le 400\) | 0.03 | 0.1 |
!>
!> The Shields-style critical stress is then
!>
!> \[
!>   \tau_c = a(\rho_s-\rho_w)g\,DRX50\,R_*^b .
!> \]
!>
!> `TAUX` is used only to place the particle in the appropriate \(R_*\) class;
!> the returned value is `TAUEC`.
!>
!> @note The `IS` class index is still selected with the original branchless
!> `SF(RSTR,R)=0.5-\mathrm{sign}(0.5,R-RSTR)` switch function, summed over the
!> four interval boundaries, rather than an `IF`/`ELSE IF` chain: an
!> intermediate modernisation replaced it with branches and was reverted for
!> performance, restoring this original form.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `K1_sycrit`/`K2_sycrit`/`K3_sycrit` with compile-time `PARAMETER`s, and replaced the legacy `DATA`-initialised `AEC`/`BEC` arrays with array constructors. |
!> @endhistory
   PURE SUBROUTINE SYCRIT(FLAG, DRX50, TAUX, FPCLAE, TAUEC)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: FLAG !! Critical-shear option: 1 clay relation, otherwise Shields relation.
      DOUBLE PRECISION, INTENT(IN) :: DRX50  !! Median particle diameter.
      DOUBLE PRECISION, INTENT(IN) :: TAUX   !! Current shear stress used to select Shields class.
      DOUBLE PRECISION, INTENT(IN) :: FPCLAE !! Clay fraction for the clay-content relation.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: TAUEC !! Returned critical shear stress.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: R0 = 3.0D-2, R1 = 1.0D0
      DOUBLE PRECISION, PARAMETER :: R2 = 6.0D0, R3 = 30.0D0, R4 = 135.0D0, R5 = 400.0D0

      DOUBLE PRECISION, PARAMETER :: AEC(5) = [0.1D0, 0.1D0, 0.033D0, 0.013D0, 0.03D0]
      DOUBLE PRECISION, PARAMETER :: BEC(5) = [-0.3D0, -0.62D0, 0.0D0, 0.28D0, 0.1D0]

      ! High-Performance Fix: Compile-time evaluation of constants
      ! (Completely replaces the runtime FIRST_sycrit block)
      DOUBLE PRECISION, PARAMETER :: K1_sycrit = 1.0D0/(SQRT(RHO_WATER_SEDIMENT)*NU_WATER)
      DOUBLE PRECISION, PARAMETER :: K2_sycrit = (RHO_SEDIMENT - RHO_WATER_SEDIMENT)*GRAVITY
      DOUBLE PRECISION, PARAMETER :: K3_sycrit = 1.83D0*LOG(10.0D0)

      INTEGER :: IS
      DOUBLE PRECISION :: RSTR

      ! Legacy branchless statement function
      DOUBLE PRECISION :: SF, RSTR_DUM, R_DUM
      SF(RSTR_DUM, R_DUM) = HALF - SIGN(HALF, R_DUM - RSTR_DUM)

      !----------------------------------------------------------------------*

      IF (FLAG == 1) THEN
         ! Quick method
         TAUEC = 0.493D0*EXP(K3_sycrit*FPCLAE)
      ELSE
         ! Shields method
         RSTR = MAX(R0, MIN(DRX50*SQRT(TAUX)*K1_sycrit, R5))

         ! Performance Reversion: Branchless execution
         IS = NINT(ONE + SF(RSTR, R1) + SF(RSTR, R2) + SF(RSTR, R3) + SF(RSTR, R4))

         TAUEC = AEC(IS)*K2_sycrit*DRX50*(RSTR**BEC(IS))
      END IF

   END SUBROUTINE SYCRIT

!> Returns a percentile grain diameter from a discrete size distribution.
!>
!> `SYDR` is used where the sediment routines need a representative diameter
!> from a discrete size distribution, for example bed-material \(D_{16}\),
!> \(D_{50}\), and \(D_{84}\) in [[syackw]], or soil median diameter in
!> [[syinit]]. `D(1:N)` contains the particle diameters. The corresponding
!> distribution weights are read from `F`, but only every `INCF`-th element is
!> relevant:
!>
!> \[
!>   w_i = F(1 + (i-1)INCF),\qquad d_i = D(i).
!> \]
!>
!> `FSED` is the requested fraction in the range 0-1 rather than a percentage.
!> The code treats the discrete distribution as values located at the supplied
!> diameters and integrates with trapezoidal midpoints. With total weight
!> \(W=\sum_i w_i\), it searches for the first interval whose doubled
!> cumulative midpoint sum exceeds
!>
!> \[
!>   2FSED\,W .
!> \]
!>
!> It then linearly interpolates between the two bracketing diameters:
!>
!> \[
!>   d = d_{hi} - (d_{hi}-d_{lo}){S-2FSED\,W\over w_{lo}+w_{hi}},
!> \]
!>
!> where \(S\) is the doubled cumulative midpoint sum at the selected interval.
!> A zero requested fraction or null distribution returns zero.
!>
!> @note The routine assumes the supplied distribution weights are non-negative
!> and ordered with increasing diameters. It does not normalise or validate the
!> weights; upstream input checks are responsible for valid sediment
!> distributions.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-06 | SvB | 4.6.1 | Replaced the `GOTO`-driven search loop with `EXIT search_loop`. |
!> @endhistory
   PURE DOUBLE PRECISION FUNCTION SYDR(FSED, INCF, N, F, D)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: INCF !! Stride between distribution weights in `F`.
      INTEGER, INTENT(IN) :: N    !! Number of sediment size classes.
      DOUBLE PRECISION, INTENT(IN) :: FSED !! Target cumulative fraction.
      DOUBLE PRECISION, INTENT(IN) :: F(1 + (N - 1)*INCF) !! Sediment distribution weights.
      DOUBLE PRECISION, INTENT(IN) :: D(N) !! Representative particle diameters.

      ! Locals
      DOUBLE PRECISION, PARAMETER  :: ALMOST = 0.9999D0
      DOUBLE PRECISION :: DR, DRHI, DRLO, F02, FLO, FHI, FSUM2, FTOT
      INTEGER :: FRPTR, SED

      !----------------------------------------------------------------------*

      ! * Initialize local variables
      FHI = 0.0d0
      DRHI = 0.0d0
      FSUM2 = 0.0d0
      FTOT = 0.0d0
      FRPTR = 1

      ! * Double the selected 'percentile' (actually a fraction 0-1)
      ! * and scale it relative to the sum of distribution ratios
      DO SED = 1, N
         FTOT = FTOT + F(FRPTR)
         FRPTR = FRPTR + INCF
      END DO

      F02 = 2.0d0*FSED*FTOT

      IF (ISZERO(F02)) THEN
         ! * Zeroth percentile or null distribution
         DR = 0.0d0

      ELSE
         ! * Reset fraction pointer
         FRPTR = 1

         ! * Loop over sediment types until target percentile surpassed
         search_loop: DO SED = 1, N

            ! * Calculate midpoint of cumulative fraction (doubled)
            FLO = FHI
            DRLO = DRHI
            FHI = F(FRPTR)
            DRHI = D(SED)
            FSUM2 = FSUM2 + FLO + FHI

            ! * Break out of loop if target percentile has been reached
            ! * (allowing for rounding error)
            IF (FSUM2 >= F02*ALMOST) EXIT search_loop

            ! * Increment fraction pointer
            FRPTR = FRPTR + INCF

         END DO search_loop

         ! * Interpolate between last two Fraction/Diameter pairs to find
         ! * target percentile.
         ! * Note :- Combination of precondition FSED<1 and use of ALMOST
         ! * should ensure (FLO+FHI) > 0
         DR = DRHI - (DRHI - DRLO)*(FSUM2 - F02)/(FLO + FHI)

      END IF

      SYDR = DR

   END FUNCTION SYDR

!> Calculates Engelund-Hansen channel transport capacity.
!>
!> This is the `ISACKW = 0` channel-capacity option used by [[sycltr]]. As in
!> the manual, it applies only to non-fine sediment fractions
!> (`NFINE+1:NSED`); fine sediment is capacity-limited separately by `FPCRIT`.
!>
!> The routine zeroes `GSED`, then loops over the two link ends and adds a
!> contribution only where the selected face is outflowing and the link is wet.
!> Link orientation is handled through `LINKNS`, so the upstream/downstream end
!> is mapped to the correct `QOC` and `SLOPEJ` face.
!>
!> For each outflowing end, with discharge \(Q\), bed slope \(S\), channel
!> width \(w\), water depth \(h\), representative particle diameter \(d_s\),
!> and relative submerged density \(R=\rho_s/\rho_w-1\), the implemented
!> Engelund-Hansen capacity increment is
!>
!> \[
!>   \Delta G_s =
!>   {0.05\,Q^2S^{3/2}\over \sqrt{g}\,R^2\,w\sqrt{h}\,d_s}.
!> \]
!>
!> `GSED(link,s)` is the sum of \(\Delta G_s\) over outflowing ends of that
!> link. [[sycltr]] later converts this streamwise discharge capacity into a
!> notional concentration capacity and applies `FPCRIT`.
!>
!> @note The implemented formula uses `SLOPEJ**1.5` directly for each outflowing
!> end. It relies on the upstream water-interface calculations and validation to
!> provide non-negative channel slopes for active outflows.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-05 | SvB | 4.6.1 | Replaced the `ALINIT` zeroing of `GSED` with a whole-array slice assignment. |
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `KG_syengh` with a compile-time `PARAMETER`. |
!> @endhistory
   PURE SUBROUTINE SYENGH(NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, QOC, LINKNS, SLOPEJ, GSED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NFINE !! Number of fine sediment classes excluded from this calculation.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NSED  !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      DOUBLE PRECISION, INTENT(IN) :: DRSED(NFINE + 1:NSED) !! Representative non-fine particle diameters.
      DOUBLE PRECISION, INTENT(IN) :: CWIDTH(NLF) !! Channel width by link.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1(NLF)  !! Channel water depth by link.
      DOUBLE PRECISION, INTENT(IN) :: QOC(NELEE, 4)    !! Face water fluxes.
      DOUBLE PRECISION, INTENT(IN) :: SLOPEJ(NELEE, 4) !! Face water-surface slopes.
      LOGICAL, INTENT(IN)          :: LINKNS(NLF) !! True for north-south channel links.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: GSED(NLF, NFINE + 1:NSED) !! Channel transport capacity by link and non-fine class.

      ! Locals, etc
      INTEGER          :: FACE, IEND, LINK, NFP1, SED, SGN
      DOUBLE PRECISION :: DWAT1E, GD, QK
      DOUBLE PRECISION, PARAMETER :: KG_syengh = 0.05D0/(SQRT(GRAVITY)*(RHO_SEDIMENT/RHO_WATER_SEDIMENT - 1.0D0)**2)

      ! External/Module functions implicitly referenced
      ! LOGICAL :: GTZERO

      !----------------------------------------------------------------------*

      ! * Initialization
      NFP1 = NFINE + 1

      ! Replaced ALINIT with a whole-array slice assignment
      GSED(:, :) = ZERO

      ! * Loop over ends of link
      DO IEND = 1, 3, 2
         SGN = 2 - IEND

         ! * Loop over links
         DO LINK = 1, NLF

            ! * Determine current face number, outflow rate & water depth
            FACE = IEND
            IF (LINKNS(LINK)) FACE = FACE + 1
            QK = SGN*QOC(LINK, FACE)
            DWAT1E = DWAT1(LINK)

            ! * Increment capacity rate for non-dry outflow ends only
            IF (GTZERO(DWAT1E) .AND. GTZERO(QK)) THEN

               ! * Loop invariant
               GD = QK**2*SLOPEJ(LINK, FACE)**1.5D0*KG_syengh/(CWIDTH(LINK)*SQRT(DWAT1E))

               ! * All sediment types
               DO SED = NFP1, NSED
                  GSED(LINK, SED) = GD/DRSED(SED) + GSED(LINK, SED)
               END DO

            END IF

            ! * Next link
         END DO

         ! * Next iend
      END DO

   END SUBROUTINE SYENGH

!> Calculates overland-flow sediment transport capacity for one element.
!>
!> `SYOVTR` implements the manual's overland transport-capacity switch
!> `ISGSED` for the current land element. The manual notes that both available
!> formulae were derived for non-cohesive channel transport and their use for
!> rainfall-driven overland flow is uncertain; [[sycolm]] applies the additional
!> total concentration cap `FPCRIT` after this routine returns.
!>
!> The representative particle diameter \(d_{50}\) is the median diameter of
!> the currently available sediment mix, obtained from [[sydr]] with `VDSED`
!> and `DRSED`.
!>
!> Capacity is summed only over faces with positive `QWAT`. The face length
!> follows the OC face convention used by the code:
!>
!> | Faces | Length |
!> |:------|:-------|
!> | 1 and 3 | `DYQQE` |
!> | 2 and 4 | `DXQQE` |
!>
!> For `ISGSED = 1`, the Engelund-Hansen-style branch uses water depth \(h\),
!> face discharge \(Q_f\), and slope \(S_f\):
!>
!> \[
!>   G_f =
!>   {0.05\rho_w^2\,Q_f^2S_f^{3/2}\over
!>    (\rho_s-\rho_w)^2\sqrt{g}\,L_f\sqrt{h}\,d_{50}} .
!> \]
!>
!> For `ISGSED = 0`, the Yalin-style branch first obtains a critical shear
!> stress \(\tau_c\) from [[sycrit]] using the Shields option and computes
!>
!> \[
!>   T = {\max(\tau_f-\tau_c,0)\over\tau_c},\qquad
!>   A = {2.45(\rho_s/\rho_w)^{-0.4}\over
!>        \sqrt{(\rho_s-\rho_w)g}}\sqrt{\tau_c/d_{50}},
!> \]
!>
!> \[
!>   G_f =
!>   {0.635\over\sqrt{\rho_w}}\sqrt{\tau_f}\,d_{50}L_f
!>   \left(T-{\log(1+AT)\over A}\right).
!> \]
!>
!> Any other `ISGSED` value gives zero capacity. The returned `GJSUM` is
!> \(\sum_f G_f\) over outflowing faces.
!>
!> @note The Engelund-Hansen branch is skipped when `DWAT1E <= 0`, returning
!> zero capacity. Both active formula branches use powers or square roots of
!> slope/shear directly, so upstream hydraulic calculations are expected to
!> provide non-negative active-face values.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `K1_syovtr`/`K3_syovtr`/`K4_syovtr` with compile-time `PARAMETER`s (declared at module scope, since they no longer need `SAVE`d state). |
!> | 2026-04-12 | SvB | 4.6.1 | Precalculated the four face lengths into `FLJ_ARRAY` instead of a per-face `MOD` test. |
!> @endhistory
   PURE SUBROUTINE SYOVTR(DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, &
                          DRSED, QWAT, SLOPEE, TAUJE, GJSUM)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISGSED !! Overland transport-capacity option.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      DOUBLE PRECISION, INTENT(IN) :: DXQQE  !! Element width.
      DOUBLE PRECISION, INTENT(IN) :: DYQQE  !! Element length.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1E !! Current surface water depth.
      DOUBLE PRECISION, INTENT(IN) :: VDSED(NSED) !! Available sediment volume by size class.
      DOUBLE PRECISION, INTENT(IN) :: DRSED(NSED) !! Representative particle diameters by size class.
      DOUBLE PRECISION, INTENT(IN) :: QWAT(4)   !! Outward water flux by face.
      DOUBLE PRECISION, INTENT(IN) :: SLOPEE(4) !! Water-surface slope by face.
      DOUBLE PRECISION, INTENT(IN) :: TAUJE(4)  !! Face shear stress.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: GJSUM !! Total overland sediment transport capacity for the element.

      ! Locals
      DOUBLE PRECISION :: K2, AJ, DRD50, FTAU, DUM, GJ, GSUM
      DOUBLE PRECISION :: LJ, TAUEC, TAUJEE
      INTEGER :: FACE, NOUT, I, J(4)
      DOUBLE PRECISION :: FLJ_ARRAY(4)

      !----------------------------------------------------------------------*
      ! Preliminaries
      ! -------------
      !

      ! Initialize variables
      GSUM = ZERO

      ! High-Performance Fix: Pre-calculate face lengths into an array instead of using MOD()
      FLJ_ARRAY = [DYQQE, DXQQE, DYQQE, DXQQE]

      ! Obtain median diameter of sediment available for discharge
      DRD50 = SYDR(HALF, 1, NSED, VDSED, DRSED)

      ! Count and record faces with outflow
      NOUT = 0
      DO FACE = 1, 4
         IF (QWAT(FACE) > ZERO) THEN
            NOUT = NOUT + 1
            J(NOUT) = FACE
         END IF
      END DO

      !
      ! Transport Capacity
      ! ------------------
      !
      IF (ISGSED == 1 .AND. DWAT1E > ZERO) THEN

         ! ^^^ ENGELUND-HANSEN METHOD ^^^

         ! Precalculate constant over faces (note K2 may be very small)
         K2 = SQRT(DWAT1E)*DRD50

         ! Loop over faces with outflow
         DO I = 1, NOUT
            FACE = J(I)

            ! Discharge capacity at this face
            LJ = FLJ_ARRAY(FACE)
            GJ = (K1_syovtr*QWAT(FACE)**2*SLOPEE(FACE)**1.5D0)/(LJ*K2)

            ! Accumulated discharge capacity for this element
            GSUM = GSUM + GJ
         END DO

      ELSE IF (ISGSED == 0) THEN

         ! ^^^^^^^ YALIN METHOD ^^^^^^^^^

         ! Loop over faces with outflow
         DO I = 1, NOUT
            FACE = J(I)

            ! Get face length
            LJ = FLJ_ARRAY(FACE)

            ! Obtain critical shear stress at the ground surface
            TAUJEE = TAUJE(FACE)
            CALL SYCRIT(0, DRD50, TAUJEE, DUM, TAUEC)

            ! Calculate discharge capacity at this face
            ! High-Performance Fixes: MAX replaces DIMJE, LOG1P replaces LOG(1+X) for precision
            FTAU = MAX(ZERO, TAUJEE - TAUEC)/TAUEC
            AJ = K3_syovtr*SQRT(TAUEC/DRD50)
            GJ = K4_syovtr*SQRT(TAUJEE)*DRD50*LJ*(FTAU - LOG(1.0D0 + AJ*FTAU)/AJ)

            ! Accumulated capacity for this element
            GSUM = GSUM + GJ
         END DO

      ELSE
         ! ^^^ Zero capacity ^^^
      END IF

      GJSUM = GSUM

   END SUBROUTINE SYOVTR

END MODULE sy_transport_capacity

