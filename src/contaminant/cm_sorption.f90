!> summary: Soil and sediment retardation factors.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> Two functions, and nothing else: [[RET]] returns the retardation factor for
!> a soil column and [[FRET]] the equivalent for channel sediment. Both are
!> pure arithmetic on their arguments, which is why they sit in a module of
!> their own rather than with either solver — [[cm_column]] and
!> [[cm_channel]] each need one of them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993--1998 | GP / RAH / SB | 3.4--4.2 | Developed and reorganised the contaminant transport routines. |
!> | 2008-12 | JE | 4.3.5F90 | Created `CMmod` while converting the former CM `COLM` and `LINK` Fortran sources to Fortran 90. |
!> | 2020-03-05 | SvB | - | Replaced the complete `SGLOBAL` include with selected imports. |
!> | 2026-09-10 | SvB | - | Split out of CMmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE cm_sorption

   USE MOD_PARAMETERS, ONLY: one, two, zero
   USE float_compare, ONLY: iszero

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: FRET, RET

CONTAINS

!> @brief Calculates ground-surface retardation and its linearisation derivatives.
!>
!> This pure helper forms old and new particle-weighted distribution
!> coefficients \(K_o=\sum FRNO_jKDREF_j\) and
!> \(K_n=\sum FRN_jKDREF_j\). For linear adsorption it returns
!>
!> \[
!> R=1+K_o/THO,\qquad R_C=0,\qquad
!> R_T=(K_n/TH-K_o/THO)/DT.
!> \]
!>
!> For nonlinear adsorption, with
!> \(D_o=(K_o/THO)C^{GN-2}\) and \(D_n=(K_n/TH)C^{GN-2}\), it returns
!>
!> \[
!> R=1+D_oC,\qquad R_C=(GN-1)D_o,\qquad
!> R_T=(D_n-D_o)C/DT.
!> \]
!>
!> @warning The routine assumes positive/nonzero `THO`, `TH`, and `DT`.
!> Unlike [[fret]], the nonlinear branch does not special-case `C=0` before
!> evaluating `C**(GN-2)`.
!> @endwarning
   PURE SUBROUTINE RET(C, GN, THO, TH, FRNO, FRN, KDREF, R, RC, RT, DT, NSED, ISNL)

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: NSED !! Active sediment-fraction count.
      LOGICAL, INTENT(IN) :: ISNL !! True for nonlinear Freundlich adsorption.
      DOUBLE PRECISION, INTENT(IN) :: C !! Current surface-water concentration.
      DOUBLE PRECISION, INTENT(IN) :: GN !! Freundlich isotherm power.
      DOUBLE PRECISION, INTENT(IN) :: THO !! Old surface moisture/storage content.
      DOUBLE PRECISION, INTENT(IN) :: TH !! New surface moisture/storage content.
      DOUBLE PRECISION, INTENT(IN) :: DT !! Dimensionless contaminant timestep.

      ! Modernization Fix: Changed (*) to (NSED) to allow vector operations
      DOUBLE PRECISION, INTENT(IN) :: FRNO(NSED) !! Old sediment-size fractions.
      DOUBLE PRECISION, INTENT(IN) :: FRN(NSED) !! New sediment-size fractions.
      DOUBLE PRECISION, INTENT(IN) :: KDREF(NSED) !! Reference distribution coefficients by fraction.

      DOUBLE PRECISION, INTENT(OUT) :: R !! Retardation/storage factor at the old state.
      DOUBLE PRECISION, INTENT(OUT) :: RC !! Concentration derivative of the retardation factor.
      DOUBLE PRECISION, INTENT(OUT) :: RT !! Timestep derivative of the retardation factor.

      ! Locals
      DOUBLE PRECISION :: DUMO, DUM, SUMO, SUMN, CDUM, DUMKO, DUMK

      !----------------------------------------------------------------------*

      DUMO = ONE/THO
      DUM = ONE/TH

      ! Modernization Fix: Replaced DO loop with highly optimized DOT_PRODUCT
      SUMO = DOT_PRODUCT(FRNO(1:NSED), KDREF(1:NSED))
      SUMN = DOT_PRODUCT(FRN(1:NSED), KDREF(1:NSED))

      IF (.NOT. ISNL) THEN
         ! IS LINEAR ADSORPTION
         R = ONE + SUMO*DUMO
         RT = (SUMN*DUM - SUMO*DUMO)/DT
         RC = ZERO
      ELSE
         ! NON-LINEAR ADSORPTION
         CDUM = C**(GN - TWO)
         DUMKO = SUMO*DUMO*CDUM
         DUMK = SUMN*DUM*CDUM

         R = ONE + DUMKO*C
         RT = (DUMK - DUMKO)*C/DT
         RC = (GN - ONE)*DUMKO
      END IF

   END SUBROUTINE RET

!> @brief Calculates link-compartment retardation and its linearisation derivatives.
!>
!> For zero concentration this pure helper returns water storage alone:
!> \(F=THO\), \(F_C=0\), and \(F_T=(TH-THO)/DT\). Otherwise it corrects the
!> old/new particle-weighted distribution coefficients for porosity,
!>
!> \[
!> J_o=\frac{1-PO}{1-PREF}\sum_j FRNO_jKDREF_j,\qquad
!> J_n=\frac{1-P}{1-PREF}\sum_j FRN_jKDREF_j.
!> \]
!>
!> Linear adsorption returns
!> \(F=THO+J_o\), \(F_C=0\), and
!> \(F_T=(TH-THO+J_n-J_o)/DT\). For nonlinear adsorption, defining
!> \(D_o=J_oC^{GN-2}\) and \(D_n=J_nC^{GN-2}\), it returns
!>
!> \[
!> F=TH+D_oC,\qquad F_C=(GN-1)D_o,\qquad
!> F_T=[TH-THO+(D_n-D_o)C]/DT.
!> \]
!>
!> @warning Nonzero-concentration calls assume `PREF/=1` and `DT/=0`; no local
!> validation is performed.
!> @endwarning
   PURE SUBROUTINE FRET(C, GN, THO, TH, FRNO, FRN, KDREF, PO, P, PREF, F, &
                        FC, FT, DT, NSED, ISNL)

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: NSED !! Active sediment-fraction count.
      LOGICAL, INTENT(IN) :: ISNL !! True for nonlinear Freundlich adsorption.
      DOUBLE PRECISION, INTENT(IN) :: C !! Current compartment concentration.
      DOUBLE PRECISION, INTENT(IN) :: GN !! Freundlich isotherm power.
      DOUBLE PRECISION, INTENT(IN) :: THO !! Old compartment moisture content.
      DOUBLE PRECISION, INTENT(IN) :: TH !! New compartment moisture content.
      DOUBLE PRECISION, INTENT(IN) :: PO !! Old compartment porosity.
      DOUBLE PRECISION, INTENT(IN) :: P !! New compartment porosity.
      DOUBLE PRECISION, INTENT(IN) :: PREF !! Reference sediment porosity.
      DOUBLE PRECISION, INTENT(IN) :: DT !! Dimensionless contaminant timestep.

      ! Modernization Fix: Changed (*) to explicit shape (NSED) for vector math
      DOUBLE PRECISION, INTENT(IN) :: FRNO(NSED) !! Old sediment-size fractions.
      DOUBLE PRECISION, INTENT(IN) :: FRN(NSED) !! New sediment-size fractions.
      DOUBLE PRECISION, INTENT(IN) :: KDREF(NSED) !! Reference distribution coefficients by fraction.

      DOUBLE PRECISION, INTENT(OUT) :: F !! Retardation/storage factor.
      DOUBLE PRECISION, INTENT(OUT) :: FC !! Concentration derivative of the retardation factor.
      DOUBLE PRECISION, INTENT(OUT) :: FT !! Timestep derivative of the retardation factor.

      ! Locals
      DOUBLE PRECISION :: DUMA, DUMO, DUM, SUMO, SUM, DUMJO, DUMJ, CDUM, DUMKO, DUMK

      !----------------------------------------------------------------------*

      IF (ISZERO(C)) THEN
         F = THO
         FC = ZERO
         FT = (TH - THO)/DT
      ELSE
         DUMA = ONE/(ONE - PREF)
         DUMO = (ONE - PO)*DUMA
         DUM = (ONE - P)*DUMA

         ! Modernization Fix: Replaced DO loop with highly optimized DOT_PRODUCT
         SUMO = DOT_PRODUCT(FRNO(1:NSED), KDREF(1:NSED))
         SUM = DOT_PRODUCT(FRN(1:NSED), KDREF(1:NSED))

         DUMJO = DUMO*SUMO
         DUMJ = DUM*SUM

         IF (.NOT. ISNL) THEN
            ! IS LINEAR ADSORPTION
            F = THO + DUMJO
            FC = ZERO
            FT = (TH - THO + DUMJ - DUMJO)/DT
         ELSE
            ! IS NON-LINEAR ADSORPTION
            CDUM = C**(GN - TWO)
            DUMKO = DUMJO*CDUM
            DUMK = DUMJ*CDUM

            F = TH + DUMKO*C
            FC = (GN - ONE)*DUMKO
            FT = (TH - THO + (DUMK - DUMKO)*C)/DT
         END IF
      END IF

   END SUBROUTINE FRET

END MODULE cm_sorption

