MODULE contaminant_plant
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! [REFACTORING] 19/08/2025 - Extracted from CMmod.f90 as plant uptake module
!                           Contains plant contaminant uptake routines: PLCOLM, PLANT, PLPREP
!
   USE SGLOBAL, ONLY :                                                             &
      NOTZERO, ZERO, ONE, TWO, HALF,                                                &
      ISZERO, GTZERO, LTZERO, GEZERO
   USE CONT_CC
   USE COLM_C1
   USE COLM_C2
   USE COLM_CC
   USE PLANT_CC
   USE AL_C, ONLY: NRD, CLAI

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: PLCOLM, PLANT, PLPREP

CONTAINS

!SSSSSS SUBROUTINE PLCOLM (NCL, NCONT)
   SUBROUTINE PLCOLM (NCL, NCONT)
!                 Updates the plant compartment concentrations for
!                 for one column for one timestep for one contaminant
      INTEGER :: NCL, NCONT, jplant, nce, jplty, nrbot
      DOUBLEPRECISION :: d1dum, d2dum, d3dum, d4dum, o2dum, f1dum, f2dum, pkdum, &
         pmdum, sum, z2dum, xdum, cdum, sdum, tdum, dum, eddum, qdum, bcdum, dum1, dum3, bcpaa1, bcpbb1
      IF (NCONT.EQ.1) THEN
         DO 100 JPLANT = 1, NPL (NCL)
            GENAA (JPLANT) = zero
            GENBB (JPLANT) = zero
100      END DO

      ENDIF
!                 Set generation variables to zero if call is for first
!                 contaminant
      DO 900 NCE = 1, NCETOP
         EDCAP (NCE) = zero
         EDCAPC (NCE) = zero
         EDCAPT (NCE) = zero
         ESCAP (NCE) = zero
         ESCAPS (NCE) = zero
         ESCAPT (NCE) = zero
900   END DO
!                 Set uptake variables to zero in preparation for
!                 summing net uptake over all plant types on
!                 column NCL
!                 Main calculation loops
      DO 1000 JPLANT = 1, NPL (NCL)
!                 For each plant type on soil column NCL
         JPLTY = NPLTYP (NCL, JPLANT)
!                 Plant type number

         NRBOT = NCETOP - NRD (JPLTY)
!                 Number of bottom rooted cell
         D1DUM = DELONE (JPLTY)
         D2DUM = DELTWO (JPLTY)
         D3DUM = DELTHR (JPLTY)
         D4DUM = DELFOU (JPLTY)
         O2DUM = one - D2DUM
         F1DUM = PFONE (NCL, JPLANT)
         F2DUM = PFTWO (JPLTY) / PF2MAX (JPLTY)
         PKDUM = PKMAX (JPLTY, NCONT)

         PMDUM = PMASS (JPLTY)
         GCPL = GCPLA (NCONT)
!                 Non dimensioned decay variable, set up in MUZ
         GMCPAA = (one - D1DUM)

         GMCPBB = F2DUM * D1DUM
         SUM = zero
         Z2DUM = Z2SQOD * F2DUM * PKDUM
         DO 1610 NCE = NRBOT, NCETOP
            XDUM = XXI * PPHI (NCE)
            CDUM = XDUM * COLCAP (NCE)
            SDUM = (one - XDUM) * SOLCAP (NCE)
            TDUM = CDUM + SDUM
            DUM = Z2DUM * PDZF3 (NCL, NCE, JPLANT) * TDUM
            SUM = SUM + DUM
            EDDUM = DUM * F1DUM / (TDUM * (Z2 * KSP (NCE) ) )
            EDCAP (NCE) = EDCAP (NCE) + CDUM * EDDUM
            ESCAP (NCE) = ESCAP (NCE) + SDUM * EDDUM
!                  Set net scaled uptake rates for use in routine COLM
!            ----- NB sums up over all plant types
!            ----- NB THE RECYLING TERMS FOR EDCAP AND ESCAP ARE
!                    ADDED BELOW
1610     END DO
         QDUM = SUM / (PMDUM * (GMCPAA + (D3DUM * GMCPBB) ) )
         QCPAA = GMCPAA * QDUM

         QCPBB = D3DUM * GMCPBB * QDUM
!                 Evaluate scaled values for Qa and Qb using
!                 equations in section 3 of WRSRU/TR/9107/12
         GMCBBD = (GMCPBB - GMCBBO (NCL, JPLANT) ) / TSE
         GMCBBO (NCL, JPLANT) = GMCPBB
         IF (LTZERO(GMCBBD)) THEN
            BCDUM = BCPBB (NCL, JPLANT, NCONT)
            DUM1 = F1DUM * D4DUM * BCDUM * GMCBBD
            DUM3 = O2DUM * PDZF3 (NCL, NCETOP, JPLANT)
            EDCAP (NCETOP) = EDCAP (NCETOP) + DUM1 * (D2DUM + DUM3) &
               / (Z2 * KSP (NCETOP) * RHOPL)
            DO 1630 NCE = NRBOT, NCETOP - 1
               EDCAP (NCE) = EDCAP (NCE) + DUM1 * DUM3 / (Z2 * KSP (NCE) &
                  * RHOPL)
1630        END DO

         ENDIF
         CALL PLANT (JPLANT, BCPAA (NCL, JPLANT, NCONT), BCPAA1, BCPBB ( &
            NCL, JPLANT, NCONT), BCPBB1, TSE)
         BCPAA (NCL, JPLANT, NCONT) = BCPAA1

         BCPBB (NCL, JPLANT, NCONT) = BCPBB1
!                 Call solve routine and update concentrations

1000  END DO
      RETURN
   END SUBROUTINE PLCOLM

!SSSSSS SUBROUTINE PLANT (JPLANT, BCAA, BCAA1, BCBB, BCBB1, TSE_IN)
   SUBROUTINE PLANT (JPLANT, BCAA, BCAA1, BCBB, BCBB1, TSE_IN)
!
!                       SETS UP AND SOLVES THE PLANT DIFFERENCE
!                       DIFFERENCE EQUATIONS IN
!                       WRSRU/TR/9107/12 SECTION 4
!                       RETURNS THE UPDATED CONCENTRATIONS IN THE
!                       PLANT COMPARTMENTS: BCAA1 AND BCBB1
!
      INTEGER :: JPLANT
      DOUBLEPRECISION :: BCAA, BCAA1, BCBB, BCBB1, TSE_IN
      DOUBLEPRECISION :: gdum, wcpaa, topdum, botdum, wcpbb

      GDUM = one + GCPL * TSE_IN
      IF (GTZERO(GMCPAA)) THEN
         WCPAA = (RHOPL * QCPAA + GMCPAA * (GENAA (JPLANT) - GCPL * &
            BCAA) ) / (GMCPAA * GDUM)
         BCAA1 = BCAA + WCPAA * TSE_IN
      ELSE
!                             No plant mass in compartment A
         BCAA1 = zero

      ENDIF
      IF (GTZERO(GMCPBB)) THEN
         TOPDUM = RHOPL * QCPBB + GMCPBB * (GENBB (JPLANT) - GCPL * &
            BCBB)
         BOTDUM = GMCPBB * GDUM
         IF (GEZERO(GMCBBD)) THEN
            TOPDUM = TOPDUM - BCBB * GMCBBD
         ELSE
            BOTDUM = BOTDUM - GMCBBD * TSE_IN
         ENDIF
         IF (NOTZERO(BOTDUM)) THEN
            WCPBB = TOPDUM / BOTDUM
            BCBB1 = BCBB + WCPBB * TSE_IN
         ELSE
            BCBB1 = zero
         ENDIF
      ELSE
         BCBB1 = zero
!                 No mass in compartment B

      ENDIF
      GENAA (JPLANT) = GCPL * BCAA

      GENBB (JPLANT) = GCPL * BCBB
!                 Decay generation values to be used for next
!                 contaminant
      RETURN
   END SUBROUTINE PLANT

!SSSSSS SUBROUTINE PLPREP
   SUBROUTINE PLPREP
!                 Preparatory routine for plant uptake called once
!                 every timestep
      INTEGER :: jplty
      DO 100 JPLTY = 1, NPLT
         PFTWO (JPLTY) = CLAI (JPLTY)
         IF (NOTZERO(PFTWO (JPLTY))) THEN
            DELFOU (JPLTY) = one
         ELSE
            DELFOU (JPLTY) = FLEFT (JPLTY)
         ENDIF

100   END DO
!                 Set f2 delta4 for each plant type
      RETURN
   END SUBROUTINE PLPREP

END MODULE contaminant_plant
