MODULE et_core
!----------------------------------------------------------------------*
!
! Core evapotranspiration and interception calculations
! Contains the main ET subroutine
!
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE et_variables
   USE AL_G,     ONLY : ICMREF
   USE AL_C,     ONLY : NVC, DTUZ, NRD, RDF, ERUZ, DELTAZ, NHBED, CWIDTH, FHBED
   USE AL_D,     ONLY : NMC, NRAINC, U, PE, OBSPE, RN, VPD, PNET, precip_m_per_s, &
      CPLAI, EINT, CSTOLD, CSTORE, DRAIN, ERZ, AE, HRUZ, ESOIL, &
      S, BWIDTH
   USE SMmod,    ONLY : SMIN
   USE OCMOD2,   ONLY : GETHRF

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: ET

CONTAINS

!SSSSSS SUBROUTINE ET
   SUBROUTINE ET (IEL)
!----------------------------------------------------------------------*
!
!        INTERCEPTION AND EVAPOTRANSPIRATION CALCULATIONS
!        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!  THIS SUBROUTINE REPRESENTS THE MAJOR PART OF THE EVAPOTRANSPIRATION
!  COMPONENT, AND SHOULD BE CALLED FOR EACH ELEMENT IEL.
!  THREE MODES OF OPERATION ARE CURRENTLY INCORPORATED FOR THE
!  CALCULATION OF ACTUAL EVAPOTRANSPIRATION :
!
!    MODE 1 : POTENTIAL AND ACTUAL EVAPOTRANSPIRATION CALCULATED
!             BY THE PENMAN-MONTEITH EQUATION WITH CONSTANT RC
!    MODE 2 : AS MODE 1 BUT WITH A VARIABLE RC DEPENDENT ON THE
!             SUPPLIED VALUE OF PSI4
!    MODE 3 : POTENTIAL EVAPOTRANSPIRATION CALCULATED BY THE
!             PENMAN-MONTEITH EQUATION (WITH RC=0.), ACTUAL
!             EVAPOTRANSPIRATION CALCULATED FROM THE RATIO AE/PE
!             AS DEPENDENT ON THE SUPPLIED VALUE OF PSI4
!
!  IN ADDITION POTENTIAL EVAPORATION CAN EITHER BE CALCULATED USING
!  THE PENMAN EQUATION OR BE READ IN DIRECTLY AS A MEASURED QUANTITY.
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/ET/ET/4.2
! Modifications since v3.3:
!  JE  920908  3.4  Set ERUZ 4 times instead of once (fixes error).
!      920928       Replace (.eq.)0 with (.lt.)ZGRUND+1D-8 in HRF tests.
!  GP  921211       Move dry soil code & setting of S(LL) to ETIN.
! RAH  941001 3.4.1 Add IMPLICIT DOUBLEPRECISION (see AL.P).
!  GP  950713  4.0  Remove "MODE 4" calculations (AE/PE=f(moisture)), &
!                   calculation of QBKI (see VSSIM). Use DELTAZ for DDZ.
! RAH  970515  4.1  Swap DELTAZ indices.Explicit typing.Amend comments.
! RAH  981021  4.2  Use block-IFs instead of GOTOs.
!                   Replace DLOG with generic LOG.
!                   Scrap output EPOTR (see AL.D).
!                   More MAX,ONE,ZERO,CSTOLD.  Bring FE from SPEC.ET.
!      981027       New locals BOTTOM,RABIG.  GAMMA units were wrong.
!                   Don't use EINT as intermediate variable.
!      981103       Don't set ERUZ(IEL,LL+1); and use 1 for local RDLINK
!                   (both used NHSAT, which has no defined value!).
!                   Scrap locals NEX (use MAX) & M2 (redundant), & hence
!                   also NSOIL (AL.D).
!----------------------------------------------------------------------*

      INTEGER :: IEL
! Locals, etc
      DOUBLEPRECISION RABIG
      PARAMETER (RABIG = 1D10)
      INTEGER :: II, IL, ITYPE, K, KF, KK, KL, M1, MR, MS, N
      DOUBLEPRECISION BOTTOM, CALC, CT1, DFET, DPS1, DRCF, DUM, F1, FE
      LOGICAL         :: first=.TRUE.
      DOUBLEPRECISION Q, TOP, XPSTOR
!----------------------------------------------------------------------*
!-----------------
!  Preliminaries
!-----------------
!-----Local indices
      MS = NMC (IEL)
      MR = NRAINC (IEL)
      N = NVC (IEL)
!-----Aerodynamic resistance (unless constant)
      IF (BAR (N) ) THEN
         IF (U (MS) .GT.ZERO) THEN
            RA (N) = RTOP (N) / U (MS)
         ELSE
            RA (N) = RABIG
         ENDIF
      ENDIF
!-----Potential evapotranspiration & Penman equation numerator
!! sb 20/6/07 has del been defined here? I think not
      BOTTOM = LAMDA * (DEL (MS) + GAMMA)
      IF (MEASPE (MS) .NE.0) THEN
!---------PE ALREADY KNOWN AS A MEASURED QUANTITY
         PE = OBSPE (MS)
         TOP = PE * BOTTOM
      ELSE
!---------PE MUST BE CALCULATED USING PENMAN EQUATION
         TOP = MAX (ZERO, RN (MS) * DEL (MS) + RHO * CP * VPD (MS) &
            / RA (N) )
!         TOP = TOP * 1D3 / densityOfWater   is implied!
         PE = TOP / BOTTOM
      ENDIF
!--------------------------------------
!  INTERCEPTION COMPONENT CALCULATION
!--------------------------------------
!-----NET RAIN NOT FALLING ON VEGETATION (mm)
      PNET = precip_m_per_s(iel) * 1000. * (ONE-CPLAI) * DTUZ
!-----EVAPORATION OF INTERCEPTED RAIN (mm)
      EINT = PE * CPLAI * DTUZ
!-----NET SUPPLY TO CANOPY (mm/s)
      Q = CPLAI * (precip_m_per_s(iel) * 1000. - PE)
!-----Update storage of, & calculate drainage from, canopy
!! sb 4/9/07 note that the canopy storage is often greater than canopy s
!! hence with very small cstcap, canopy evap. is often quite large
      CSTOLD = CSTORE (IEL)
!sb 4/9/07 changed GE to GT to stop error if cstcap=0
!      IF ( CSTOLD .GE. CSTCAP(N) ) THEN
      IF (CSTOLD.GT.CSTCAP (N) ) THEN
         F1 = ONE
         IF (Q.GT.ZERO) THEN
!------------------------------
!--CASE OF CSTORE>=CSTCAP , Q>0
            CALC = CB (N) * (CSTOLD-CSTCAP (N) + DTUZ * Q)
            DUM = CB (N) * (CSTOLD-CSTCAP (N) )
            CALC = CALC - LOG (CK (N) * EXP (CALC) - CK (N) * EXP (DUM) &
               + Q)
            CSTORE (IEL) = MAX (ZERO, CSTCAP (N) + (LOG (Q) + CALC) &
               / CB (N) )
            DRAIN = - CSTORE (IEL) + CSTOLD+Q * DTUZ
         ELSE
!-------------------------------
!--CASE OF CSTORE>=CSTCAP , Q<=0
            CT1 = CSTOLD+DTUZ * Q
            IF (CT1.LE.CSTCAP (N) ) THEN
               CSTORE (IEL) = MAX (ZERO, CT1)
               IF (CT1.LT.ZERO) EINT = EINT + CT1
               DRAIN = ZERO
            ELSE
               XPSTOR = EXP ( - CB (N) * (CT1 - CSTCAP (N) ) )
               CALC = LOG (DTUZ * CB (N) * CK (N) + XPSTOR)
               CSTORE (IEL) = MAX (ZERO, CSTCAP (N) - CALC / CB (N) )
               DRAIN = - CSTORE (IEL) + CSTOLD+Q * DTUZ
            ENDIF
         ENDIF
      ELSE
!-----------------------
!--CASE OF CSTORE<CSTCAP
         CT1 = CSTOLD+DTUZ * CPLAI * precip_m_per_s(iel) * 1000.
         F1 = MIN (CT1 / CSTCAP (N), ONE)
!sb 4/9/07
         if (LEZERO(CSTCAP(n))) then
            if (LEZERO(CT1)) then
               f1 = zero
            else
               f1 = one
            endif
         endif
!end of sb 4/9/07
         EINT = EINT * F1
         CT1 = CT1 - EINT
         IF (CT1.GT.CSTCAP (N) ) THEN
            XPSTOR = EXP ( - CB (N) * (CT1 - CSTCAP (N) ) )
            CALC = LOG (DTUZ * CB (N) * CK (N) + XPSTOR)
            CSTORE (IEL) = MAX (ZERO, CSTCAP (N) - CALC / CB (N) )
            DRAIN = - CSTORE (IEL) + CT1
         ELSE
            CSTORE (IEL) = MAX (ZERO, CT1)
!              IF (CT1.LT.ZERO) EINT = EINT + CT1
!sb 4/9/07 remove loss of evap if evap is more than rain plus storage
            IF (CT1.LT.ZERO) then
               f1 = (eint + CT1) / eint
               EINT = EINT + CT1
            endif
!end of sb 4/9/07
            DRAIN = ZERO
         ENDIF
      ENDIF
!
!
!
!-----TOTAL THROUGHFALLMM AND MM/S
      PNET = PNET + DRAIN
      PNET = PNET / DTUZ
!
!------------------------------------------------
!  EVAPOTRANSPIRATION COMPONENT CALCULATIONS
!------------------------------------------------
!
!  NOTE THAT POTENTIAL (PE) AND ACTUAL (AE)
!  EVAPOTRANSPIRATION RATES ARE CALCULATED IN MM/SEC
!
      M1 = MODE (N)
      K = NRD (N)
      ERZ = ZERO
!
! CALCULATE EXPOSED BED CELL, EXTEND LOOP 310 TO CHANNEL BED.
! CALCULATE PLANT UPTAKE FROM STREAM FOR BANK ELEMENTS ONLY
!
      ITYPE = ICMREF (IEL, 1)
      IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
         IL = ICMREF (IEL, 4)
         K = MAX (top_cell_no - NHBED (IL, ITYPE), K)
      ENDIF

      IF(top_cell_no - K <0) THEN
         k= top_cell_no
         WRITE(msg,'(A)') 'root zone extends below aquifer bed. Values below aquifer bed are ignored'
         if (first) then
            CALL ERROR(WWWARN, 4999, pppri, 0, 0, msg)
            first = .false.
         endif
      ENDIF
!
!     Count from the top cell down (for RDF subscript)
      DO 310 KK = 1, K
!
!        Corresponding bottom-up cell number
         II = top_cell_no - KK + 1
!
         IF (M1.NE.2.AND.M1.NE.3) THEN
!
!--------------------------------
!  MODE 1 CALCULATIONS.....
!--------------------------------
!
            IF (PSI4 (II) .GE.ZERO) THEN
               AE = PE
            ELSE
               AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE+RC (N) &
                  / RA (N) ) ) )
            ENDIF
!
         ELSEIF (M1.EQ.2) THEN
!
!--------------------------------
!  MODE 2 CALCULATIONS.....
!--------------------------------
!
!--LINEAR INTERPOLATION FOR VALUE OF RC DEPENDENT ON PSI4 (EITHER
!--WATER CONTENT OR TENSION BUT MUST BE COMPATIBLE WITH UZ COMP.)
!
            KF = NF (N)
            IF (PSI4 (II) .GE.ZERO) THEN
               RC (N) = RCF (N, KF)
            ELSEIF (PSI4 (II) .LE.PS1 (N, 1) ) THEN
               RC (N) = RCF (N, 1)
            ELSEIF (PSI4 (II) .GT.PS1 (N, KF) ) THEN
               RC (N) = RCF (N, KF)
            ELSE
               DO KL = 2, KF
                  IF (PSI4 (II) .GT.PS1 (N, KL) ) CYCLE
                  DPS1 = PS1 (N, KL) - PS1 (N, KL - 1)
                  DRCF = RCF (N, KL) - RCF (N, KL - 1)
                  CALC = (PSI4 (II) - PS1 (N, KL - 1) ) * DRCF / DPS1
                  RC (N) = RCF (N, KL - 1) + CALC
                  EXIT
               END DO
            ENDIF
            AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE+RC (N) / RA (N) &
               ) ) )
!
         ELSEIF (M1.EQ.3) THEN
!
!--------------------------------
!  MODE 3 CALCULATIONS.....
!--------------------------------
!
!  CALCULATE AE/PE RATIO DEPENDENT ON PSI4 BY LINEAR INTERPOLATION
!
            KF = NF (N)
            IF (PSI4 (II) .GE.ZERO) THEN
               FE = ONE
            ELSEIF (PSI4 (II) .LT.PS1 (N, 1) ) THEN
               FE = FET (N, 1)
            ELSEIF (PSI4 (II) .GT.PS1 (N, KF) ) THEN
               FE = FET (N, KF)
            ELSE
               DO KL = 2, KF
                  IF (PSI4 (II) .GT.PS1 (N, KL) ) CYCLE
                  DFET = FET (N, KL) - FET (N, KL - 1)
                  DPS1 = PS1 (N, KL) - PS1 (N, KL - 1)
                  CALC = (PSI4 (II) - PS1 (N, KL - 1) ) * DFET / DPS1
                  FE = FET (N, KL - 1) + CALC
                  EXIT
               END DO
            ENDIF
            AE = PE * FE
!
         ENDIF
!
!-----PUT PLANT UPTAKE INTO GLOBAL ARRAY FOR CONTAMINANTS
!-----AE IS IN MM/S AND S IS IN M/S
!
         DUM = ZERO
         IF (HRUZ.LE.ZERO) DUM = AE * CPLAI * (ONE-F1) * RDF (N, KK) &
            / (ONE+UZALFA (II) )
         ERZ = ERZ + DUM
         DUM = DUM * 1D-3
         ERUZ (IEL, II) = DUM
         IF(NOTZERO(dum)) THEN
            S (II) = DUM / DELTAZ (II, IEL)
         ELSE
            S(II) = zero
         ENDIF
!
!-----CALCULATE SOIL-EVAPORATION : ESOIL IN MM/S
!
! sb 270515 soil evap shouls be less than short grass evap
         IF (II.EQ.top_cell_no) ESOIL = 0.5 * AE * (1 - CPLAI)
!   IF (II.EQ.top_cell_no) ESOIL = AE * (1 - CPLAI)
!
!
310   END DO
!
   END SUBROUTINE ET

END MODULE et_core
