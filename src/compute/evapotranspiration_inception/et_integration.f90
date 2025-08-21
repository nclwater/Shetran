MODULE et_integration
!----------------------------------------------------------------------*
!
! Integration routines for evapotranspiration calculations
! Contains ETIN subroutine that integrates ET calculations with the model
!
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE et_variables
   USE et_core
   USE AL_C,     ONLY : NVC, CLAI, PLAI, NVSWLT, QVSWEL, DTUZ, DELTAZ, &
      PNETTO, DRAINA, ESOILA, eevap
   USE AL_D,     ONLY : NMC, NRAINC, PE, PNET, CPLAI, EINT, DRAIN, ERZ, ESOIL, &
      NSMT, S, BEXSM, ESWA, HRUZ, EPOT, EINTA, ERZA
   USE SMmod,    ONLY : SMIN
   USE OCMOD2,   ONLY : getHRF

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: ETIN

CONTAINS

!SSSSSS SUBROUTINE ETIN (IEL)
   SUBROUTINE ETIN (IEL)
!----------------------------------------------------------------------*
! Version:  SHETRAN/ET/ETIN/4.1
! Modifications:
! RAH  941001 3.4.1 Add IMPLICIT DOUBLEPRECISION (see AL.P).
!  GP  950118  4.0  Scrap call to PRIET.  Replace IRRC, RSZWEL & DDZ
!                   with NVSWLT, QVSWEL & DELTAZ.
! RAH  970516  4.1  Swap DELTAZ indices.  Explicit typing.  Local WEL.
!                   Scrap AL.D outputs DWETOC, DWETEX & DWEXET, and
!                   SPEC.ET outputs DWETER & WSET.  Amend comments.
!                   Use MIN for CPLAI.  Remove redundant setting of N.
!----------------------------------------------------------------------*
! Commons and constants
! Output common
!     SPEC.AL:         NSMT
!                      CPLAI,HRUZ,PE,PNET,...
! Input arguments

      INTEGER :: IEL
! Locals, etc
      INTEGER :: MR, MS, N, WEL
      DOUBLEPRECISION EDUM
!----------------------------------------------------------------------*
      MS = NMC (IEL)
      MR = NRAINC (IEL)
      N = NVC (IEL)

!     CALCULATE INTERCEPTION AREA OF VEGETATION
      CPLAI = MIN (CLAI (N), ONE) * PLAI (N)
!
!     CHECK FOR SNOWMELT CALCULATIONS, & SOLVE ET IF NECESSARY.
!     NSMT IS AUTOMATICALLY SET TO 1 IF ET-CALCS FOR TEMP > 0 ARE NEEDED
      NSMT = 0
      IF (BEXSM) CALL SMIN (IEL)
      IF (.NOT. (NSMT.EQ.0.AND.BEXSM) ) THEN
         CALL ET (IEL)
         IF (BEXSM) CALL SMIN (IEL)
      ENDIF
!
!-----Calculate potential evapotranspiration
      PE = PE-EINT / DTUZ
!
!-----STORE RESULTS IN ARRAYS
      PNETTO (IEL) = PNET / 1000.
      EPOT (IEL) = PE / 1000.
      EINTA (IEL) = EINT / (1000. * DTUZ)
      DRAINA (IEL) = DRAIN / (1000. * DTUZ)
      ERZA (IEL) = ERZ / 1000.
      ESOILA (IEL) = ESOIL / 1000.
!
! ADD IRRIGATION FLUX FROM WELLS INTO PNETTO
      WEL = NVSWLT (IEL)
      IF (WEL.NE.0) PNETTO (IEL) = PNETTO (IEL) + QVSWEL (WEL) * &
         (cellarea (WEL) / cellarea (IEL) )
!
! Calculations for HRUZ(net), ESWA, EEVAP, ESOILA
! If surface water exists at start of timestep, available potential
!   evaporation is partitioned into evaporation from surface water and
!   evaporation from the soil (asasuming the soil is near saturation).
! If no surface water exists, evaporation from the soil has already been
!   calculated in the ET subroutine.
! ESOILA switched off for evap. from dry soil when surface water
! initially exists GP 11/12/92
      IF (GTZERO(HRUZ)) THEN
         HRUZ = getHRF(IEL) - ZGRUND (IEL) + (PNETTO (IEL) - EPOT (IEL) ) &
            * DTUZ
         IF (LTZERO(HRUZ)) THEN
            EDUM = - HRUZ / DTUZ
            ESWA (IEL) = EPOT (IEL) - EDUM
            IF (PSI4 (top_cell_no) .LT. - 150.0D0) THEN
               ESOILA (IEL) = zero
            ELSE
               ESOILA (IEL) = EDUM
            ENDIF
            HRUZ = zero
            PNET = zero
         ELSE
            ESOILA (IEL) = zero
            ESWA (IEL) = EPOT (IEL)
         ENDIF
      ELSE
         ESWA (IEL) = zero
      ENDIF
      EEVAP (IEL) = ESWA (IEL) + ESOILA (IEL)

      S (top_cell_no) = S (top_cell_no) + ESOILA (IEL) / DELTAZ (top_cell_no, IEL)

   END SUBROUTINE ETIN

END MODULE et_integration
