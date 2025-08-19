MODULE contaminant_utilities
! [REFACTORING] 19/08/2025 - Extracted from CMmod.f90 as utilities module
!                           Contains utility functions and helper routines used across
!                           the contaminant transport system
!
   USE contaminant_common

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: PHI, DISP, RET

CONTAINS

   !FFFFFF DOUBLEPRECISION FUNCTION PHI
   DOUBLEPRECISION FUNCTION PHI (JSOIL, THETA)
      !                             FRACTION OF SOIL WATER WHICH IS MOBILE
      INTEGER, INTENT(IN) :: JSOIL
      DOUBLEPRECISION, INTENT(IN) :: THETA

      PHI = half
      !                             ########## SOIL INFO NEEDED HERE #########
   END FUNCTION PHI

   !FFFFFF DOUBLEPRECISION FUNCTION DISP
   DOUBLEPRECISION FUNCTION DISP (NCONT, JSOIL, THETA, UM, UP)
      !                             (VERTICAL) EFFECTIVE LONGITUDINAL
      !                             DISPERSION COEFFICIENT FOR SOIL
      INTEGER, INTENT(IN) :: NCONT, JSOIL
      DOUBLEPRECISION, INTENT(IN) :: THETA, UM, UP

      DISP = 3.0D-8
      !                             ########## SOIL INFO NEEDED HERE #########
   END FUNCTION DISP

   !SSSSSS SUBROUTINE RET
   SUBROUTINE RET (C, GN, THO, TH, FRNO, FRN, KDREF, R, RC, RT, DT, NSED, ISNL)
      !                             CALCULATES THE GROUND SURFACE RETARDATION
      !                             FACTOR, R,
      !                             ITS CONCENTRATION DERIVATIVE ,RC,
      !                             AND ITS TIME DERIVATIVE ,RT,
      !                             DEPENDING ON THE CONCENTRATION IN THE
      !                             SURFACE WATER ,C,
      !                             PARTICLE SIZE FRACTIONS, FRNO AND FRN,
      !                             FREUNDLICH POWER, GN,
      !                             OLD AND NEW MOISTURE CONTENTS, THO AND TH,
      !                             REFERENCE Kd, KDREF,
      !                             SCALED TIME STEP, DT,
      !                             NUMBER OF SEDIMENT FRACTIONS, NSED,
      !                             AND THE 'NON-LINEAR ADSORPTION' FLAG, ISNL

      DOUBLEPRECISION, INTENT(IN) :: C, GN, THO, TH, DT
      DOUBLEPRECISION, INTENT(IN) :: FRNO(*), FRN(*), KDREF(*)
      INTEGER, INTENT(IN) :: NSED
      LOGICAL, INTENT(IN) :: ISNL
      DOUBLEPRECISION, INTENT(OUT) :: R, RC, RT

      ! Local variables
      INTEGER :: JSED
      DOUBLEPRECISION :: DUMO, DUM, SUMO, SUM, CDUM, DUMKO, DUMK

      DUMO = one / THO
      DUM = one / TH
      SUMO = zero
      SUM = zero
      DO JSED = 1, NSED
         SUMO = SUMO + FRNO (JSED) * KDREF (JSED)
         SUM = SUM + FRN (JSED) * KDREF (JSED)
      END DO
      IF (.NOT.ISNL) THEN
         !                             IS LINEAR ADSORPTION
         R = one + SUMO * DUMO
         RT = (SUM * DUM - SUMO * DUMO) / DT
         RC = zero
      ELSE
         CDUM = C** (GN - two)
         DUMKO = SUMO * DUMO * CDUM
         DUMK = SUM * DUM * CDUM
         R = one + DUMKO * C
         RT = (DUMK - DUMKO) * C / DT
         RC = (GN - one) * DUMKO
      ENDIF
   END SUBROUTINE RET

END MODULE contaminant_utilities
