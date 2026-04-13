MODULE ETmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the ET .F files
   USE SGLOBAL
!USE SGLOBAL,     ONLY : NVEE, NUZTAB, NVBP, LLEE, &
!                     nelee  !NEEDED ONLY FOR AD
   USE AL_G,     ONLY : ICMREF, NGDBGN, ICMREF
   USE AL_C,     ONLY : NVC, DTUZ, NRD, RDF, ERUZ, DELTAZ, CLAI, PNETTO, DRAINA, ESOILA, &
      NHBED, PLAI, NVSWLT, QVSWEL, eevap, UZNEXT, CWIDTH, &
      FHBED, NLYRBT, vspsi, NV
   USE AL_D,     ONLY : NMC, NRAINC, U, PE, OBSPE, RN, VPD, PNET, precip_m_per_s, CPLAI, EINT, CSTOLD, CSTORE, &
      EPOT, EINTA, ERZA, ESWA, BEXSM, DRAIN, ERZ, AE, HRUZ, ESOIL, &
      NSMT, S, TIMEUZ, BWIDTH, &
      sf, sd, ts, nsmc !THESE NEEDED ONLY FOR AD
   USE mod_load_filedata,    ONLY : ALCHK
   USE UTILSMOD, ONLY : DCOPY
   USE SMmod,    ONLY : SMIN, &
      smelt, tmelt !THESE NEEDED ONLY FOR AD
!NEEDED ONLY FOR AD
   USE SMmod,    ONLY : rhos
   USE OCMOD2, ONLY  : GETHRF



   IMPLICIT NONE
!FROM SPEC_ET
!MODULE SPEC_ET
!-------------------------- Start of SPEC.ET --------------------------*
!
!     COMMON BLOCKS  ** ET-COMPONENT **
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/SPEC.ET/4.2
! Modifications:
!   GP        FEB 89    2.0   'SHE88' IMPLEMENTATION ON NEWCASTLE AMDAHL
!                              NB. THIS IS A COMBINED OLD/NEW ET VERSION
!   GP        APR 89    2.1     REMOVE VARIABLE SDEPTH
!   GP        NOV 90    3.0     STANDARDISE TO F77
!                               + MOVE VARIABLES TO SPEC.AL
!   GP        JAN 92    3.3     DIMENSION OF VARIABLES AMENDED
!                               (TO NUZTAB OR NVBP)
!  GP          3.4  Add EPLAST,PEIN.  Remove RDF.
!  GP  940809  4.0  Bring PSI4,UZALFA from AL.D
! RAH  970220  4.1  Remove EOC,EA,PS,ESNOW,QSNOW,CSNOW,CMELT,TTHRES,
!                   NUMROT,NCTROT,NROT,NCLA,QLAI,CLA,CINT,C1,C2,C3,ROT,
!                   RELROT,TIMROT,AROOT (redundant).  Explicit typing.
!                   Move HEAD from ETCB6.  Move ETCB6,ETCB7.
!      970516       Remove DWETER (see ETIN & INET) and DTDAYS,DTHRS,
!                   DTMIN,DTSEC (were used in PRIET).  Move: HEAD,
!                   ZU,ZD,ZO to INET; EPLAST,IDATA,PA,PEIN to METIN.
!                   Remove NUMCST,NUMPLA,NUMCLA,NUMVHT (INET & METIN).
! RAH  981021  4.2  Move FE to ET.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*

! Imported constants
!                      LLEE,NVBP,NVEE,NUZTAB

! Commons

!
! ^^^ EXTRA COMMON BLOCK FOR VARIABLES IN BLOCK DATA SEGMENT
!USE SGLOBAL, ONLY : NVEE, NUZTAB, NVBP, LLEE
!IMPLICIT NONE
!DOUBLEPRECISION LAMDA, GAMMA, RHO, CP
!COMMON / ETCB6 / LAMDA, GAMMA, RHO, CP

   DOUBLEPRECISION, PARAMETER :: LAMDA=2465000., &
      GAMMA=0.659, &
      RHO=1.2, &
      CP=1003.
!
! ^^^ LOGICAL VARIABLES AND ARRAYS
!
   LOGICAL :: BAR (NVEE), BMETP, BINETP, BMETAL, BMETDATES
!COMMON / ETCB3 / BAR, BMETP, BINETP, BMETAL
!
! ^^^ INTEGER ARRAYS
!
   INTEGER :: MODE (NVEE), NF (NVEE), MEASPE (NVEE)
   INTEGER :: MODECS (NVEE), MODEPL (NVEE), MODECL (NVEE), MODEVH ( &
      NVEE)
   INTEGER :: NCTCST (NVEE), NCTPLA (NVEE), NCTCLA (NVEE), NCTVHT ( &
      NVEE)
!COMMON / ETCB4 / MODE, NF, MEASPE, MODECS, MODEPL, MODECL, MODEVH, &
   !NCTCST, NCTPLA, NCTCLA, NCTVHT
!
! ^^^ FLOATING-POINT ARRAYS
!
!DOUBLEPRECISION RA (NVEE), RC (NVEE), RTOP (NVEE)
!DOUBLEPRECISION CSTCAP (NVEE), CK (NVEE), CB (NVEE), DEL (NVEE)
!DOUBLEPRECISION PS1 (NVEE, NUZTAB)
!DOUBLEPRECISION PSI4 (LLEE), UZALFA (LLEE)
!DOUBLEPRECISION FET (NVEE, NUZTAB), CSTCA1 (NVEE), PLAI1 (NVEE)
!DOUBLEPRECISION RCF (NVEE, NUZTAB), CLAI1 (NVEE), VHT1 (NVEE)
!DOUBLEPRECISION RELCST (NVEE, NVBP), TIMCST (NVEE, NVBP)
!DOUBLEPRECISION RELPLA (NVEE, NVBP), TIMPLA (NVEE, NVBP)
!DOUBLEPRECISION RELCLA (NVEE, NVBP), TIMCLA (NVEE, NVBP)
!DOUBLEPRECISION RELVHT (NVEE, NVBP), TIMVHT (NVEE, NVBP)

   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: RA,RC,RTOP
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: CSTCAP,CK,CB,DEL
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: PSI4,UZALFA
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: CSTCA1,PLAI1
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: CLAI1,VHT1
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: PS1,FET,RCF
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RELCST,TIMCST
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RELPLA,TIMPLA
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RELCLA,TIMCLA
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RELVHT,TIMVHT

   CHARACTER(132) :: msg
!PRIVATE :: NVEE, NUZTAB, NVBP, LLEE
!END MODULE SPEC_ET
   PRIVATE
   PUBLIC :: ETSIM, BMETP, BINETP, BMETAL, BMETDATES, MEASPE, CSTCAP, RC, BAR, RA, MODE, &
      NF, CK, CB, MODECS, MODEPL, MODECL, MODEVH, NCTCST, CSTCA1, RELCST, TIMCST, &
      NCTPLA, PLAI1, RELPLA, TIMPLA, NCTCLA, CLAI1, NCTVHT, VHT1, RELVHT, TIMVHT, &
      PS1, RCF, FET, RTOP, RELCLA, TIMCLA, del, &
      psi4, uzalfa, INITIALISE_ETMOD !THESE NEEDED ONLY FOR AD
CONTAINS


   SUBROUTINE INITIALISE_ETMOD()

      ALLOCATE (RA(NV),RC(NV),RTOP(NV))
      ALLOCATE (CSTCAP(NV),CK(NV),CB(NV),DEL(NV))
      ALLOCATE (PSI4(LLEE),UZALFA(LLEE))
      ALLOCATE (CSTCA1(NV),PLAI1(NV))
      ALLOCATE (CLAI1(NV),VHT1(NV))
      ALLOCATE (PS1(NV,NUZTAB),FET(NV,NUZTAB),RCF(NV,NUZTAB))
      ALLOCATE (RELCST(NV,NVBP),TIMCST(NV,NVBP))
      ALLOCATE (RELPLA(NV,NVBP),TIMPLA(NV,NVBP))
      ALLOCATE (RELCLA(NV,NVBP),TIMCLA(NV,NVBP))
      ALLOCATE (RELVHT(NV,NVBP),TIMVHT(NV,NVBP))
      RA=0.0d0
      RC=0.0d0
      RTOP=0.0d0
      cstcap=0.0d0
      ck=0.0d0
      cb=0.0d0
      del=0.0d0
      PSI4=0.0d0
      UZALFA=0.0d0
      CSTCA1=0.0d0
      PLAI1=0.0d0
      CLAI1=0.0d0
      VHT1=0.0d0
      PS1=0.0d0
      FET=0.0d0
      RCF=0.0d0
      RELCST=0.0d0
      TIMCST=0.0d0
      RELPLA=0.0d0
      TIMPLA=0.0d0
      RELCLA=0.0d0
      TIMCLA=0.0d0
      RELVHT=0.0d0
      TIMVHT=0.0d0

   END SUBROUTINE INITIALISE_ETMOD

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

      ! Assumed external module dependencies providing global variables:
      ! NMC, NRAINC, NVC, BAR, U, RA, RTOP, LAMDA, DEL, GAMMA, MEASPE, PE,
      ! OBSPE, RN, RHO, CP, VPD, precip_m_per_s, CPLAI, DTUZ, PNET, EINT,
      ! CSTORE, CSTCAP, CB, CK, DRAIN, AE, MODE, NRD, ERZ, ICMREF, top_cell_no,
      ! NHBED, msg, WWWARN, pppri, ERROR, PSI4, RC, RCF, PS1, NF, FET, RDF,
      ! UZALFA, ERUZ, S, DELTAZ, HRUZ, ESOIL, ZERO, ONE, LEZERO, NOTZERO

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IEL

      ! Locals, etc
      DOUBLE PRECISION, PARAMETER :: RABIG = 1.0D10
      INTEGER :: II, IL, ITYPE, K, KF, KK, KL, M1, MR, MS, N
      DOUBLE PRECISION :: BOTTOM, CALC, CT1, DFET, DPS1, DRCF, DUM, F1, FE
      DOUBLE PRECISION :: Q, TOP, XPSTOR
      LOGICAL, SAVE :: first = .TRUE.

      !----------------------------------------------------------------------*
      !-----------------
      !  Preliminaries
      !-----------------
      !-----Local indices
      MS = NMC (IEL)
      MR = NRAINC (IEL)
      N = NVC (IEL)

      !-----Aerodynamic resistance (unless constant)
      IF (BAR (N)) THEN
         IF (U (MS) > ZERO) THEN
            RA (N) = RTOP (N) / U (MS)
         ELSE
            RA (N) = RABIG
         END IF
      END IF

      !-----Potential evapotranspiration & Penman equation numerator
      !! sb 20/6/07 has del been defined here? I think not
      BOTTOM = LAMDA * (DEL (MS) + GAMMA)
      IF (MEASPE (MS) /= 0) THEN
         !---------PE ALREADY KNOWN AS A MEASURED QUANTITY
         PE = OBSPE (MS)
         TOP = PE * BOTTOM
      ELSE
         !---------PE MUST BE CALCULATED USING PENMAN EQUATION
         TOP = MAX (ZERO, RN (MS) * DEL (MS) + RHO * CP * VPD (MS) / RA (N))
         !         TOP = TOP * 1D3 / densityOfWater   is implied!
         PE = TOP / BOTTOM
      END IF

      !--------------------------------------
      !  INTERCEPTION COMPONENT CALCULATION
      !--------------------------------------
      !-----NET RAIN NOT FALLING ON VEGETATION (mm)
      PNET = precip_m_per_s(IEL) * 1000.0D0 * (ONE - CPLAI) * DTUZ

      !-----EVAPORATION OF INTERCEPTED RAIN (mm)
      EINT = PE * CPLAI * DTUZ

      !-----NET SUPPLY TO CANOPY (mm/s)
      Q = CPLAI * (precip_m_per_s(IEL) * 1000.0D0 - PE)

      !-----Update storage of, & calculate drainage from, canopy
      !! sb 4/9/07 note that the canopy storage is often greater than canopy s
      !! hence with very small cstcap, canopy evap. is often quite large
      CSTOLD = CSTORE (IEL)

      !sb 4/9/07 changed GE to GT to stop error if cstcap=0
      IF (CSTOLD > CSTCAP (N)) THEN
         F1 = ONE
         IF (Q > ZERO) THEN
            !------------------------------
            !--CASE OF CSTORE>=CSTCAP , Q>0
            CALC = CB (N) * (CSTOLD - CSTCAP (N) + DTUZ * Q)
            DUM = CB (N) * (CSTOLD - CSTCAP (N))
            CALC = CALC - LOG (CK (N) * EXP (CALC) - CK (N) * EXP (DUM) + Q)
            CSTORE (IEL) = MAX (ZERO, CSTCAP (N) + (LOG (Q) + CALC) / CB (N))
            DRAIN = -CSTORE (IEL) + CSTOLD + Q * DTUZ
         ELSE
            !-------------------------------
            !--CASE OF CSTORE>=CSTCAP , Q<=0
            CT1 = CSTOLD + DTUZ * Q
            IF (CT1 <= CSTCAP (N)) THEN
               CSTORE (IEL) = MAX (ZERO, CT1)
               IF (CT1 < ZERO) EINT = EINT + CT1
               DRAIN = ZERO
            ELSE
               XPSTOR = EXP (-CB (N) * (CT1 - CSTCAP (N)))
               CALC = LOG (DTUZ * CB (N) * CK (N) + XPSTOR)
               CSTORE (IEL) = MAX (ZERO, CSTCAP (N) - CALC / CB (N))
               DRAIN = -CSTORE (IEL) + CSTOLD + Q * DTUZ
            END IF
         END IF
      ELSE
         !-----------------------
         !--CASE OF CSTORE<CSTCAP
         CT1 = CSTOLD + DTUZ * CPLAI * precip_m_per_s(IEL) * 1000.0D0
         F1 = MIN (CT1 / CSTCAP (N), ONE)

         !sb 4/9/07
         IF (LEZERO(CSTCAP(N))) THEN
            IF (LEZERO(CT1)) THEN
               F1 = ZERO
            ELSE
               F1 = ONE
            END IF
         END IF
         !end of sb 4/9/07

         EINT = EINT * F1
         CT1 = CT1 - EINT

         IF (CT1 > CSTCAP (N)) THEN
            XPSTOR = EXP (-CB (N) * (CT1 - CSTCAP (N)))
            CALC = LOG (DTUZ * CB (N) * CK (N) + XPSTOR)
            CSTORE (IEL) = MAX (ZERO, CSTCAP (N) - CALC / CB (N))
            DRAIN = -CSTORE (IEL) + CT1
         ELSE
            CSTORE (IEL) = MAX (ZERO, CT1)
            !sb 4/9/07 remove loss of evap if evap is more than rain plus storage
            IF (CT1 < ZERO) THEN
               F1 = (EINT + CT1) / EINT
               EINT = EINT + CT1
            END IF
            !end of sb 4/9/07
            DRAIN = ZERO
         END IF
      END IF

      !-----TOTAL THROUGHFALLMM AND MM/S
      PNET = PNET + DRAIN
      PNET = PNET / DTUZ

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

      ! CALCULATE EXPOSED BED CELL, EXTEND LOOP 310 TO CHANNEL BED.
      ! CALCULATE PLANT UPTAKE FROM STREAM FOR BANK ELEMENTS ONLY
      !
      ITYPE = ICMREF (IEL, 1)
      IF (ITYPE == 1 .OR. ITYPE == 2) THEN
         IL = ICMREF (IEL, 4)
         K = MAX (top_cell_no - NHBED (IL, ITYPE), K)
      END IF

      IF (top_cell_no - K < 0) THEN
         K = top_cell_no
         WRITE(msg,'(A)') 'root zone extends below aquifer bed. Values below aquifer bed are ignored'
         IF (first) THEN
            CALL ERROR(WWWARN, 4999, pppri, 0, 0, msg)
            first = .FALSE.
         END IF
      END IF

      !     Count from the top cell down (for RDF subscript)
      DO KK = 1, K
         !        Corresponding bottom-up cell number
         II = top_cell_no - KK + 1

         IF (M1 /= 2 .AND. M1 /= 3) THEN
            !--------------------------------
            !  MODE 1 CALCULATIONS.....
            !--------------------------------
            IF (PSI4 (II) >= ZERO) THEN
               AE = PE
            ELSE
               AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE + RC (N) / RA (N))))
            END IF

         ELSE IF (M1 == 2) THEN
            !--------------------------------
            !  MODE 2 CALCULATIONS.....
            !--------------------------------
            !--LINEAR INTERPOLATION FOR VALUE OF RC DEPENDENT ON PSI4 (EITHER
            !--WATER CONTENT OR TENSION BUT MUST BE COMPATIBLE WITH UZ COMP.)
            KF = NF (N)
            IF (PSI4 (II) >= ZERO) THEN
               RC (N) = RCF (N, KF)
            ELSE IF (PSI4 (II) <= PS1 (N, 1)) THEN
               RC (N) = RCF (N, 1)
            ELSE IF (PSI4 (II) > PS1 (N, KF)) THEN
               RC (N) = RCF (N, KF)
            ELSE
               ! Modernized interpolation loop replacing GOTO logic
               DO KL = 2, KF
                  IF (PSI4 (II) <= PS1 (N, KL)) THEN
                     DPS1 = PS1 (N, KL) - PS1 (N, KL - 1)
                     DRCF = RCF (N, KL) - RCF (N, KL - 1)
                     CALC = (PSI4 (II) - PS1 (N, KL - 1)) * DRCF / DPS1
                     RC (N) = RCF (N, KL - 1) + CALC
                     EXIT
                  END IF
               END DO
            END IF

            AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE + RC (N) / RA (N))))

         ELSE IF (M1 == 3) THEN
            !--------------------------------
            !  MODE 3 CALCULATIONS.....
            !--------------------------------
            !  CALCULATE AE/PE RATIO DEPENDENT ON PSI4 BY LINEAR INTERPOLATION
            KF = NF (N)
            IF (PSI4 (II) >= ZERO) THEN
               FE = ONE
            ELSE IF (PSI4 (II) < PS1 (N, 1)) THEN
               FE = FET (N, 1)
            ELSE IF (PSI4 (II) > PS1 (N, KF)) THEN
               FE = FET (N, KF)
            ELSE
               ! Modernized interpolation loop replacing GOTO logic
               DO KL = 2, KF
                  IF (PSI4 (II) <= PS1 (N, KL)) THEN
                     DFET = FET (N, KL) - FET (N, KL - 1)
                     DPS1 = PS1 (N, KL) - PS1 (N, KL - 1)
                     CALC = (PSI4 (II) - PS1 (N, KL - 1)) * DFET / DPS1
                     FE = FET (N, KL - 1) + CALC
                     EXIT
                  END IF
               END DO
            END IF

            AE = PE * FE

         END IF

         !-----PUT PLANT UPTAKE INTO GLOBAL ARRAY FOR CONTAMINANTS
         !-----AE IS IN MM/S AND S IS IN M/S
         DUM = ZERO
         IF (HRUZ <= ZERO) DUM = AE * CPLAI * (ONE - F1) * RDF (N, KK) / (ONE + UZALFA (II))

         ERZ = ERZ + DUM
         DUM = DUM * 1.0D-3
         ERUZ (IEL, II) = DUM

         IF (NOTZERO(DUM)) THEN
            S (II) = DUM / DELTAZ (II, IEL)
         ELSE
            S (II) = ZERO
         END IF

         !-----CALCULATE SOIL-EVAPORATION : ESOIL IN MM/S
         ! sb 270515 soil evap should be less than short grass evap
         IF (II == top_cell_no) ESOIL = 0.5D0 * AE * (1.0D0 - CPLAI)

      END DO

   END SUBROUTINE ET




!SSSSSS SUBROUTINE ETCHK2 (PRI, NV, RDL, LDUM1)
   SUBROUTINE ETCHK2 (PRI, NV, RDL, LDUM1)
!----------------------------------------------------------------------*
!
!  Check ET input data
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/ET/ETCHK2/4.2
! Modifications:
! RAH  981103  4.2  New (from OCCHK2).
!----------------------------------------------------------------------*
! Entry requirements:
!  NV.ge.1    PRI open for F output
!----------------------------------------------------------------------*
      INTEGER :: PRI, NV

      DOUBLEPRECISION RDL (NV)
! Workspace arguments

      LOGICAL :: LDUM1 (NV)
! Locals, etc
      INTEGER :: FATAL, ERR
      PARAMETER (FATAL = 1, ERR = 2)
      INTEGER :: IUNDEF, NERR
      DATA NERR / 0 /
!----------------------------------------------------------------------*
! 1. Vegetation Properties
! ------------------------
!RDL



      CALL ALCHK (ERR, 1062, PRI, 1, NV, IUNDEF, IUNDEF, 'RDL(veg)', &
         'EQ', ZERO1, ZERO , RDL, NERR, LDUM1)
! 2. Finish
! ---------
!

      IF (NERR.GT.0) CALL ERROR(FFFATAL, 1000, PRI, 0, 0, 'Error(s) detected while checking ET input data')
   END SUBROUTINE ETCHK2



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

      ! Assumed external module dependencies providing global variables:
      ! NMC, NRAINC, NVC, CLAI, ONE, PLAI, NSMT, BEXSM, SMIN, ET, PE, EINT,
      ! DTUZ, PNETTO, PNET, EPOT, EINTA, DRAINA, DRAIN, ERZA, ERZ, ESOILA,
      ! ESOIL, NVSWLT, QVSWEL, cellarea, GTZERO, HRUZ, getHRF, ZGRUND,
      ! LTZERO, ESWA, PSI4, top_cell_no, zero, EEVAP, S, DELTAZ

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IEL

      ! Locals, etc
      INTEGER :: MR, MS, N, WEL
      DOUBLE PRECISION :: EDUM

      !----------------------------------------------------------------------*
      MS = NMC (IEL)
      MR = NRAINC (IEL)
      N = NVC (IEL)

      ! CALCULATE INTERCEPTION AREA OF VEGETATION
      CPLAI = MIN (CLAI (N), ONE) * PLAI (N)

      ! CHECK FOR SNOWMELT CALCULATIONS, & SOLVE ET IF NECESSARY.
      ! NSMT IS AUTOMATICALLY SET TO 1 IF ET-CALCS FOR TEMP > 0 ARE NEEDED
      NSMT = 0
      IF (BEXSM) CALL SMIN (IEL)

      ! Modernized logic to eliminate GOTO 10
      IF (NSMT /= 0 .OR. .NOT. BEXSM) THEN
         CALL ET (IEL)
         IF (BEXSM) CALL SMIN (IEL)
      END IF

      !-----Calculate potential evapotranspiration
      PE = PE - EINT / DTUZ

      !-----STORE RESULTS IN ARRAYS
      ! Upgraded constants to strict double precision
      PNETTO (IEL) = PNET / 1000.0D0
      EPOT (IEL)   = PE / 1000.0D0
      EINTA (IEL)  = EINT / (1000.0D0 * DTUZ)
      DRAINA (IEL) = DRAIN / (1000.0D0 * DTUZ)
      ERZA (IEL)   = ERZ / 1000.0D0
      ESOILA (IEL) = ESOIL / 1000.0D0

      ! ADD IRRIGATION FLUX FROM WELLS INTO PNETTO
      WEL = NVSWLT (IEL)

      IF (WEL /= 0) THEN
         PNETTO (IEL) = PNETTO (IEL) + QVSWEL (WEL) * (cellarea (WEL) / cellarea (IEL))
      END IF

      ! Calculations for HRUZ(net), ESWA, EEVAP, ESOILA
      ! If surface water exists at start of timestep, available potential
      !   evaporation is partitioned into evaporation from surface water and
      !   evaporation from the soil (assuming the soil is near saturation).
      ! If no surface water exists, evaporation from the soil has already been
      !   calculated in the ET subroutine.
      ! ESOILA switched off for evap. from dry soil when surface water
      ! initially exists GP 11/12/92
      IF (GTZERO(HRUZ)) THEN
         HRUZ = getHRF(IEL) - ZGRUND (IEL) + (PNETTO (IEL) - EPOT (IEL)) * DTUZ

         IF (LTZERO(HRUZ)) THEN
            EDUM = -HRUZ / DTUZ
            ESWA (IEL) = EPOT (IEL) - EDUM

            IF (PSI4 (top_cell_no) < -150.0D0) THEN
               ESOILA (IEL) = zero
            ELSE
               ESOILA (IEL) = EDUM
            END IF

            HRUZ = zero
            PNET = zero
         ELSE
            ESOILA (IEL) = zero
            ESWA (IEL) = EPOT (IEL)
         END IF
      ELSE
         ESWA (IEL) = zero
      END IF

      EEVAP (IEL) = ESWA (IEL) + ESOILA (IEL)

      S (top_cell_no) = S (top_cell_no) + ESOILA (IEL) / DELTAZ (top_cell_no, IEL)

   END SUBROUTINE ETIN




   !----------------------------------------------------------------------*
   ! Controlling routine for evapotranspiration/interception module
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/ET/ETSIM/4.2
   ! Modifications:
   !  GP  08.08.94  written (v4.0 finished 3/10/95)
   ! RAH  970516  4.1  Swap VSPSI indices. Amend comments. Explicit typing.
   ! RAH  981103  4.2  Scrap AL.D output NSOIL (see ET).
   !                   Replace DO-loops with calls to ALINIT & DCOPY.
   !----------------------------------------------------------------------*
   SUBROUTINE ETSIM ()

      ! Assumed external module dependencies providing global variables:
      ! DTUZ, UZNEXT, TIMEUZ, NGDBGN, total_no_elements, ICMREF, CWIDTH,
      ! BWIDTH, NHBED, UZALFA, FHBED, top_cell_no, ZERO, HRUZ, getHRF,
      ! ZGRUND, NLYRBT, VSPSI, PSI4, ETIN

      IMPLICIT NONE

      ! Locals, etc
      INTEGER :: ICE, IEL, IL, ITYPE
      DOUBLE PRECISION :: ALFA

      !----------------------------------------------------------------------*

      DTUZ = UZNEXT * 3600.0D0

      TIMEUZ = TIMEUZ + UZNEXT

      ! Loop over land-elements
      DO IEL = NGDBGN, total_no_elements
         ITYPE = ICMREF (IEL, 1)

         IF (ITYPE == 1 .OR. ITYPE == 2) THEN
            IL = ICMREF (IEL, 4)
            ALFA = 0.5D0 * CWIDTH (IL) / BWIDTH
            ICE = NHBED (IL, ITYPE) + 2

            ! Replaced ALINIT with array slice
            UZALFA (1 : ICE - 2) = ALFA
            UZALFA (ICE - 1) = ALFA * FHBED (IL, ITYPE)
         ELSE
            ICE = 1
         END IF

         ! Replaced ALINIT with array slice starting at index ICE
         IF (ICE <= top_cell_no) UZALFA (ICE : top_cell_no) = ZERO

         HRUZ = getHRF(IEL) - ZGRUND (IEL)
         ICE = NLYRBT (IEL, 1)

         CALL DCOPY (top_cell_no - ICE + 1, VSPSI (ICE, IEL), 1, PSI4 (ICE), 1)

         CALL ETIN (IEL)

      END DO

   END SUBROUTINE ETSIM

END MODULE ETmod
