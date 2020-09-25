MODULE ETmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the ET .F files
USE SGLOBAL
!USE SGLOBAL,     ONLY : NVEE, NUZTAB, NVBP, LLEE, &
!                     nelee  !NEEDED ONLY FOR AD
USE AL_G,     ONLY : ICMREF, NGDBGN, ICMREF
USE AL_C,     ONLY : NVC, DTUZ, NRD, RDF, ERUZ, DELTAZ, CLAI, PNETTO, DRAINA, ESOILA, &
                     NHBED, PLAI, NVSWLT, QVSWEL, eevap, UZNEXT, CWIDTH, &
                     FHBED, NLYRBT, vspsi
USE AL_D,     ONLY : NMC, NRAINC, U, PE, OBSPE, RN, VPD, PNET, precip_m_per_s, CPLAI, EINT, CSTOLD, CSTORE, &
                     EPOT, EINTA, ERZA, ESWA, BEXSM, DRAIN, ERZ, AE, HRUZ, ESOIL, &
                     NSMT, S, TIMEUZ, BWIDTH, &
                     sf, sd, ts, nsmc !THESE NEEDED ONLY FOR AD
USE mod_load_filedata,    ONLY : ALCHK, ALINIT
USE mod_load_filedata,    ONLY : ERRC, ERRNEE, ERRTOT !HELPPATH !AD NEEDS THIS
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
LOGICAL :: BAR (NVEE), BMETP, BINETP, BMETAL  
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
DOUBLEPRECISION RA (NVEE), RC (NVEE), RTOP (NVEE)  
DOUBLEPRECISION CSTCAP (NVEE), CK (NVEE), CB (NVEE), DEL (NVEE)  
DOUBLEPRECISION PS1 (NVEE, NUZTAB), PSI4 (LLEE), UZALFA (LLEE)  
DOUBLEPRECISION FET (NVEE, NUZTAB), CSTCA1 (NVEE), PLAI1 (NVEE)  
DOUBLEPRECISION RCF (NVEE, NUZTAB), CLAI1 (NVEE), VHT1 (NVEE)  
DOUBLEPRECISION RELCST (NVEE, NVBP), TIMCST (NVEE, NVBP)  
DOUBLEPRECISION RELPLA (NVEE, NVBP), TIMPLA (NVEE, NVBP)  
DOUBLEPRECISION RELCLA (NVEE, NVBP), TIMCLA (NVEE, NVBP)  
DOUBLEPRECISION RELVHT (NVEE, NVBP), TIMVHT (NVEE, NVBP)
CHARACTER(132) :: msg
!PRIVATE :: NVEE, NUZTAB, NVBP, LLEE
!END MODULE SPEC_ET 
PRIVATE
PUBLIC :: ETSIM, BMETP, BINETP, BMETAL, MEASPE, CSTCAP, RC, BAR, RA, MODE, &
          NF, CK, CB, MODECS, MODEPL, MODECL, MODEVH, NCTCST, CSTCA1, RELCST, TIMCST, &
          NCTPLA, PLAI1, RELPLA, TIMPLA, NCTCLA, CLAI1, NCTVHT, VHT1, RELVHT, TIMVHT, &
          PS1, RCF, FET, RTOP, RELCLA, TIMCLA, del, &
          psi4, uzalfa !THESE NEEDED ONLY FOR AD
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
!  THREE MODES OF OPeRATION ARE CURRENTLY INCORPORATED FOR THE
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
!
!     Input Variables
!     ^^^^^^^^^^^^^^^
!
!     U      - wind speed                              m/s
!     RTOP   - product RA*U (invariant)                -
!     LAMDA  - LATENT HEAT OF VAPORISATION             J/kg
!     DEL    - derivative of sat.vap.pres wrt temp     mbar/C
!     GAMMA  - PSYCHROMETRIC CONSTANT                  mbar/C
!     OBSPE  - measured value of PE                    mm/s
!     RN     - net radiation                           W/m^2
!     RHO    - DENSITY OF AIR                          kg/m^3
!     CP     - SPECIFIC HEAT OF AIR                    J/kg/C
!     VPD    - vapour pressure deficit of air          mbar
!     DTUZ   - CURRENT TIME STEP length                s
!
!     Control Variables
!     ^^^^^^^^^^^^^^^^^
!
!     BAR    - .TRUE. iff RA is variable
!     MEASPE - zero iff PE is not measured
!
!     Output Variables
!     ^^^^^^^^^^^^^^^^
!
!     RA     - aerodynamic resistance                  s/m
!     PE     - POTENTIAL EVAPOTRANSPIRATION            mm/s
!     AE     - ATUAL EVAPOTRANSPIRATION                mm/s
!     PNET   - NET PRECIPITATION                       mm/s
!     ERZ    - TRANSPIRATION                           mm/s
!     ESOIL  - EVAPORATION FROM SOIL                   mm/s
!     EINT   - EVAPORATION FROM CANOPY                 mm
!     DRAIN  - DRAINAGE FROM CANOPY                    mm
!     CSTOLD - value of CSTORE at start of time-step   m^3
!
!----------------------------------------------------------------------*
! Entry requirements:
!  LL.ge.1    NRD.le.LL
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     SPEC.AL          NELEE,NLFEE,NVEE
! Input common
!     SPEC.AL          LL,ICMREF(NELEE,4)
!              [NMC]-> MEASPE
!              [IEL]-> NHSAT,[NMC,NRAINC,NVC]
!        [link,bank]-> NHBED
!                      CPLAI,DTUZ,HRUZ
!              [IEL]-> DELTAZ(LL)
!              [NMC]-> OBSPE,RN,U,VPD
!           [NRAINC]-> P
!              [NVC]-> BAR
!     SPEC.ET  [NVC]-> MODE,NF,NRD
!                      CP,GAMMA,LAMDA,RHO
!              [NVC]-> CB,CK,CSTCAP
!              [NMC]-> DEL
!                      FET(NVEE,NF),PS1(NVEE,NF),PSI4(LL)
!                      RCF(NVEE,NF),           UZALFA(LL)
! In+out common
!     SPEC.AL  [IEL]-> CSTORE
! In|out common
!     SPEC.ET  [NVC]-> RA,RC
! Output common
!     SPEC.AL:         AE,CSTOLD,DRAIN,EINT,ERZ,ESOIL,PE,PNET
!                      ERUZ(NELEE,LL),S(LL)
! Input arguments

INTEGER :: IEL  
! Locals, etc
!INTRINSIC EXP, LOG, MAX, MIN  
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
         DO 170 KL = 2, KF  
            IF (PSI4 (II) .GT.PS1 (N, KL) ) GOTO 170  
            DPS1 = PS1 (N, KL) - PS1 (N, KL - 1)  
            DRCF = RCF (N, KL) - RCF (N, KL - 1)  
            CALC = (PSI4 (II) - PS1 (N, KL - 1) ) * DRCF / DPS1  
            RC (N) = RCF (N, KL - 1) + CALC  
            GOTO 200  
  170          END DO  
  200          CONTINUE  
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
         DO 240 KL = 2, KF  
            IF (PSI4 (II) .GT.PS1 (N, KL) ) GOTO 240  
            DFET = FET (N, KL) - FET (N, KL - 1)  
            DPS1 = PS1 (N, KL) - PS1 (N, KL - 1)  
            CALC = (PSI4 (II) - PS1 (N, KL - 1) ) * DFET / DPS1  
            FE = FET (N, KL - 1) + CALC  
            GOTO 270  
  240          END DO  
  270          CONTINUE  
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
  310 END DO  
!
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
! Input arguments

INTEGER :: IEL  
! Locals, etc
!INTRINSIC MIN  
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
IF (NSMT.EQ.0.AND. (BEXSM) ) GOTO 10  
CALL ET (IEL)  
IF (BEXSM) CALL SMIN (IEL)  
   10 CONTINUE  
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




!SSSSSS SUBROUTINE ETSIM
SUBROUTINE ETSIM ()  
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
! Commons and constants
!  
! Locals, etc
INTEGER :: ICE, IEL, IL, ITYPE  


DOUBLEPRECISION ALFA  
DTUZ = UZNEXT * 3600.0D0  


TIMEUZ = TIMEUZ + UZNEXT  
! Loop over land-elements

DO 1000 IEL = NGDBGN, total_no_elements  
   ITYPE = ICMREF (IEL, 1)  
   IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN  
      IL = ICMREF (IEL, 4)  
      ALFA = 0.5 * CWIDTH (IL) / BWIDTH  
      ICE = NHBED (IL, ITYPE) + 2  
      CALL ALINIT (ALFA, ICE-2, UZALFA)  
      UZALFA (ICE-1) = ALFA * FHBED (IL, ITYPE)  
   ELSE  
      ICE = 1  
   ENDIF  

   IF (ICE.LE.top_cell_no) CALL ALINIT (ZERO, top_cell_no - ICE+1, UZALFA (ICE) )  

   HRUZ = getHRF(IEL) - ZGRUND (IEL)  
   ICE = NLYRBT (IEL, 1)  

   CALL DCOPY (top_cell_no - ICE+1, VSPSI (ICE, IEL), 1, PSI4 (ICE), &
    1)

   CALL ETIN (IEL)  

 1000 END DO  
END SUBROUTINE ETSIM
END MODULE ETmod