!> summary: Channel conveyance, and the depth thresholds that guard it.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[CONVEYAN]] evaluates the Strickler conveyance of a channel cross-section.
!> It is called from [[oc_node_solver:OCCODE]], from four of the discharge
!> routines in [[oc_discharge]], and from [[oc_cross_sections:OCXS]] while it
!> tabulates the conveyance curve.
!>
!> That is why this module exists and why it is a **leaf**: without it,
!> [[oc_node_solver]] and [[oc_discharge]] would each have to `USE` the other.
!> `DZMIN`, `RDZMIN` and `H23MIN` travel with it for the same reason —
!> [[oc_node_solver:OCFIX]] reads `DZMIN` — and they are depth thresholds
!> belonging to these formulae rather than model configuration. See
!> `docs/rename/constants_review.md`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCmod2; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_conveyance

   USE MOD_PARAMETERS, ONLY: TWO_THIRDS, FIVE_THIRDS

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CONVEYAN
   PUBLIC :: DZMIN, RDZMIN, H23MIN

   DOUBLEPRECISION, PARAMETER   :: DZMIN = 1.0D-3       !! Small depth/head-difference threshold, in metres.
   DOUBLEPRECISION, PARAMETER   :: RDZMIN = 3.16227766d-2 !! Square root of `DZMIN`.
   DOUBLEPRECISION, PARAMETER   :: H23MIN = 1.0d-2        !! `DZMIN**(2/3)`, retained for legacy comments and comparisons.

CONTAINS

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
      DOUBLE PRECISION, PARAMETER :: MUL = 10.0D0/3.0D0

      !----------------------------------------------------------------------*

      IF (TY == 0) THEN
         IF (H < 1.0D-9) THEN
            CONV = 0.0D0
            DERIV = 0.0D0
         ELSE IF (H < 1.0D-3) THEN
            ! conv  = deriv * h          ! LINEARIZE NEAR ZERO
            CONV = STR*MUL*H*H*(4.0D0 - 1.0D3*H)  ! TAKE CARE valid only for threshold of 1 mm
            CONV = CONV*XA/H
            DERIV = STR*MUL*H*(8.0D0 - 3.0D3*H)     ! TAKE CARE valid only for threshold of 1 mm
         ELSE
            HM23 = H**TWO_THIRDS
            CONV = STR*XA*HM23      ! NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
            DERIV = STR*HM23*FIVE_THIRDS
         END IF

      ELSE IF (TY == 1) THEN
         IF (H < 1.0D-9) THEN
            CONV = 0.0D0
            DERIV = 0.0D0
         ELSE IF (H < 1.0D-3) THEN
            ! conv  = deriv * h          ! LINEARIZE NEAR ZERO
            CONV = STR*MUL*H*H*(4.0D0 - 1.0D3*H)  ! TAKE CARE valid only for threshold of 1 mm
            DERIV = STR*MUL*H*(8.0D0 - 3.0D3*H)     ! TAKE CARE valid only for threshold of 1 mm
         ELSE
            HM23 = H**TWO_THIRDS
            CONV = STR*H*HM23       ! NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
            DERIV = STR*HM23*FIVE_THIRDS
         END IF

      ELSE IF (TY == 2) THEN
         HM23 = H**TWO_THIRDS
         CONV = STR*XA*HM23
         DERIV = CONV*(EXTRA/XA + TWO_THIRDS/H)  ! is two_thirds correct here?
      END IF

      ! Legacy Disabled Block
      ! IF(ty<2) THEN
      !    IF(h<dzmin) THEN
      !        deriv = str * h23min * two_thirds
      !        conv  = deriv * h  !LINEARIZE NEAR ZERO (FOR AD)
      !        hm23  = zero
      !    ELSE
      !        hm23 = h**two_thirds
      !        conv = str * xo * hm23
      !        deriv = str * hm23 * five_thirds  !str * MAX(h23min, hm23) * five_thirds
      !    ENDIF
      ! ELSE

   END SUBROUTINE CONVEYAN

END MODULE oc_conveyance

