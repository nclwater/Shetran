!> summary: Evapotranspiration, interception, and vegetation-control calculations.
!>
!> This module implements the SHETRAN evapotranspiration component. It stores
!> vegetation and meteorological control tables, allocates ET work arrays after
!> run dimensions are known, computes canopy interception and actual/potential
!> evapotranspiration for each land element, checks ET input data, and writes
!> ET results back into the shared flow arrays used by the rest of the model.
!>
!> The user manual describes this component in the
!> Evapotranspiration/Interception Module input section. The relevant controls
!> are the ET records for meteorological-printing and alternate meteorological
!> files (`BMETP`, `BINETP`, `BMETAL`, `BMETDATES`), measured-versus-calculated
!> potential evaporation (`MEASPE`), vegetation/aerodynamic parameters (`BAR`,
!> `RA`, `ZU`, `ZD`, `ZO`, `RC`, `MODE`, `NF`), canopy and root parameters
!> (`PLAI`, `CSTCAP`, `CK`, `CB`, `NRD`, `CLAI`, `VHT`, `RDL`), time-varying
!> vegetation parameters, and the `PS1`/`RCF`/`FET` soil-moisture-tension tables.
!>
!> Fluxes are converted between the legacy millimetre-per-second ET calculation
!> variables and SHETRAN's shared metre-per-second water-flow arrays in
!> [[etin]]. Canopy storage capacity, drainage coefficients, time-varying canopy
!> and vegetation parameters, root-density functions, and measured-versus-
!> calculated potential evaporation are controlled by the ET input-file records
!> described in the manual's Evapotranspiration/Interception Module section.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-02 | GP | 2.0 | SHE88 implementation of the combined ET component. |
!> | 1989-04 to 1994-08 | GP | 2.1-4.0 | Removed obsolete storage-depth state, standardised to Fortran 77, amended table dimensions, added `EPLAST`/`PEIN`, and moved `PSI4`/`UZALFA` from `AL_D`. |
!> | 1997-05-16 | RAH | 4.1 | Moved ET input/meteorological variables and removed redundant outputs. |
!> | 1998-10-21 | RAH | 4.2 | Moved `FE` into the ET component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted ET `.F` files into this Fortran 90 module. |
!> | 2026-03 | SB | 4.6 | Added date-aware meteorological input through `BMETDATES` and allocated ET meteorological/control arrays in [[initialise_etmod]]. |
MODULE ETmod

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
USE mod_load_filedata,    ONLY : ALCHK, ALINIT
USE mod_load_filedata,    ONLY : ERRC, ERRNEE, ERRTOT !HELPPATH !AD NEEDS THIS
USE UTILSMOD, ONLY : DCOPY
USE SMmod,    ONLY : SMIN, &
                     smelt, tmelt !THESE NEEDED ONLY FOR AD
!NEEDED ONLY FOR AD
USE SMmod,    ONLY : rhos
USE OCMOD2, ONLY  : GETHRF




IMPLICIT NONE
! Legacy SPEC.ET variables retained as module state.
!USE SGLOBAL, ONLY : NVEE, NUZTAB, NVBP, LLEE
!IMPLICIT NONE
!DOUBLEPRECISION LAMDA, GAMMA, RHO, CP
!COMMON / ETCB6 / LAMDA, GAMMA, RHO, CP

DOUBLEPRECISION, PARAMETER :: LAMDA=2465000., &
GAMMA=0.659, &
RHO=1.2, &
CP=1003.
! ET logical variables and arrays.
LOGICAL :: BAR (NVEE), BMETP, BINETP, BMETAL, BMETDATES
!COMMON / ETCB3 / BAR, BMETP, BINETP, BMETAL
! ET integer control arrays.
INTEGER :: MODE (NVEE), NF (NVEE), MEASPE (NVEE)
INTEGER :: MODECS (NVEE), MODEPL (NVEE), MODECL (NVEE), MODEVH ( &
 NVEE)
INTEGER :: NCTCST (NVEE), NCTPLA (NVEE), NCTCLA (NVEE), NCTVHT ( &
 NVEE)
!COMMON / ETCB4 / MODE, NF, MEASPE, MODECS, MODEPL, MODECL, MODEVH, &
 !NCTCST, NCTPLA, NCTCLA, NCTVHT
! ET floating-point work arrays.
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


!> Allocates and zero-initialises evapotranspiration work arrays.
!>
!> The arrays depend on the run-time vegetation count `NV`, the global vertical
!> layer limit `LLEE`, and the ET table dimensions `NUZTAB` and `NVBP`. This
!> routine must be called after those dimensions have been read.
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



!> Computes interception, potential ET, actual ET, transpiration, and soil evaporation for one element.
!>
!> This routine is the main evapotranspiration calculation and is called once
!> per active element. As described by the manual records `ET6`, `ME3`/`ME5`,
!> and `EP2`, potential evaporation may be calculated from the full
!> meteorological data or read directly as measured `OBSPE` when `MEASPE=1`.
!>
!> The supported actual-ET modes are:
!>
!> | Mode | Calculation |
!> |:-----|:------------|
!> | 1 | Actual evapotranspiration uses a constant canopy resistance `RC`, varying only with vegetation type. |
!> | 2 | `RC` varies with soil-moisture tension as well as vegetation type, using the `PS1`/`RCF` table from `ET16`. |
!> | 3 | Actual evapotranspiration is derived from the dependency of the `AE/PE` ratio on soil-moisture tension, using the `PS1`/`FET` table from `ET16`. |
!>
!> Key inputs and controls are:
!>
!> | Variable | Meaning | Units |
!> |:---------|:--------|:------|
!> | `U` | Wind speed at height `ZU` above the ground, read from the meteorological data. | m/s |
!> | `RTOP` | Invariant product `RA * U`, derived from `ZU`, `ZD`, and `ZO` when `BAR=.TRUE.`. | - |
!> | `LAMDA` | Latent heat of vaporisation. | J/kg |
!> | `DEL` | Slope of the saturation vapour-pressure/temperature curve. | mbar/C |
!> | `GAMMA` | Psychrometric constant. | mbar/C |
!> | `OBSPE` | Measured potential evapotranspiration/evaporation from `ME3`, `ME5`, or `EP2`; input-file units are mm/hr and the model stores converted internal rates. | mm/hr input |
!> | `RN` | Net radiation from the meteorological data. | W/m^2 |
!> | `RHO` | Air density. | kg/m^3 |
!> | `CP` | Specific heat of air. | J/kg/C |
!> | `VPD` | Vapour-pressure deficit of air from the meteorological data. | mbar |
!> | `DTUZ` | Current timestep length used by the model calculation. | s |
!> | `BAR` | ET8 flag: if true, `RA` is evaluated from `ZU`, `ZD`, and `ZO`; if false, constant `RA` is used. | logical |
!> | `MEASPE` | ET6 flag: `1` means potential evaporation is measured and read from meteorological/potential-evaporation data; `0` means it is not measured. | flag |
!> | `MODE` | ET8 actual-ET mode. | 1, 2, or 3 |
!> | `PS1` | Soil moisture tension table used by modes 2 and 3. | m |
!> | `RCF` | Canopy-resistance table paired with `PS1` for mode 2. | s/m |
!> | `FET` | Actual/potential evapotranspiration ratio paired with `PS1` for mode 3. | - |
!>
!> Key outputs and updated state are:
!>
!> | Variable | Meaning | Units |
!> |:---------|:--------|:------|
!> | `RA` | Aerodynamic resistance; `RABIG = 1D10` is used as the calm-wind limit. | s/m |
!> | `PE` | Potential evapotranspiration/evaporation after conversion to the routine's internal rate units. | mm/s |
!> | `AE` | Actual evapotranspiration. | mm/s |
!> | `PNET` | Net precipitation. | mm/s |
!> | `ERZ` | Transpiration. | mm/s |
!> | `ESOIL` | Evaporation from soil. | mm/s |
!> | `EINT` | Evaporation from canopy storage during the timestep. | mm |
!> | `DRAIN` | Drainage from canopy storage during the timestep. | mm |
!> | `CSTOLD` | Canopy storage at the start of the timestep. | m^3 |
!>
!> Plant uptake is distributed vertically using the root-density function and
!> written to `ERUZ` and `S`. The routine updates `RA`, `RC`, `PE`, `AE`, `PNET`,
!> `ERZ`, `ESOIL`, `EINT`, `DRAIN`, `CSTOLD`, and `CSTORE` for downstream use by
!> [[etin]]. Entry requirements are `LL >= 1` and `NRD <= LL`.
!>
!> @note The mode definitions and input meanings above follow the SHETRAN User
!> Guide and Data Input Manual records `ET6`, `ET8`, `ET16`, `ME2`/`ME3`, and
!> `EP2`. `BMETAL=.TRUE.` means only potential evapotranspiration is available,
!> so the manual requires `MODE=3`.
!> @endnote
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1992-09-08 | JE | 3.4 | Set `ERUZ` four times instead of once. |
!> | 1995-07-13 | GP | 4.0 | Removed mode 4 and updated subsurface-layer variables. |
!> | 1997-05-15 | RAH | 4.1 | Swapped `DELTAZ` indices, explicitly typed variables, and amended comments. |
!> | 1998-10-21 | RAH | 4.2 | Replaced GOTOs with block IFs, used generic intrinsics, and removed redundant outputs. |
SUBROUTINE ET (IEL)
INTEGER, INTENT(IN) :: IEL !! Element number for which ET and interception are computed.
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




!> Checks evapotranspiration input data for vegetation properties.
!>
!> `ETCHK2` currently validates the vegetation rooting depth array `RDL` for the
!> `NV` vegetation types. Entry requirements are `NV >= 1` and `PRI` open for
!> formatted diagnostic output.
SUBROUTINE ETCHK2 (PRI, NV, RDL, LDUM1)
INTEGER, INTENT(IN) :: PRI !! Output unit for check/error messages.
INTEGER, INTENT(IN) :: NV !! Number of vegetation types to check.

DOUBLEPRECISION, INTENT(IN) :: RDL (NV) !! Rooting-depth values for vegetation types.
! Workspace arguments

LOGICAL, INTENT(INOUT) :: LDUM1 (NV) !! Logical workspace used by `ALCHK`.
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





!> Runs ET/interception setup and stores flux results for one element.
!>
!> This wrapper prepares vegetation and snowmelt state, calls [[et]] when ET
!> calculations are needed, converts millimetre-based ET quantities to SHETRAN
!> metres/seconds shared arrays, adds irrigation-well fluxes to net rainfall,
!> and partitions potential evaporation between surface water and soil when
!> ponded water is present.
!>
!> The shared outputs are `PNETTO` for net input to the land surface, `ERUZ` and
!> `S` for root-zone extraction, `EEVAP`/`ESOILA` for soil evaporation, `DRAINA`
!> for canopy drainage, and `EPOT` for potential evaporation in water-flow
!> units. It also updates shared ET/flow state including `NSMT`, `CPLAI`, `HRUZ`,
!> `PE`, and `PNET`.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-01 | RAH | 3.4.1 | Added legacy double-precision typing. |
!> | 1995-01-18 | GP | 4.0 | Replaced old wetting variables with `NVSWLT`, `QVSWEL`, and `DELTAZ`. |
!> | 1997-05-16 | RAH | 4.1 | Swapped `DELTAZ` indices, removed redundant outputs, and used `MIN` for `CPLAI`. |
SUBROUTINE ETIN (IEL)
INTEGER, INTENT(IN) :: IEL !! Element number to process.
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




!> Controls evapotranspiration and interception calculations for all land elements.
!>
!> `ETSIM` converts the next model timestep to seconds, advances ET time, builds
!> bank/link root-access weighting factors where needed, copies current soil
!> pressure heads into `PSI4`, and calls [[etin]] for each active land element.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-08 | GP | 4.0 | Written as the controlling ET/interception routine. |
!> | 1997-05-16 | RAH | 4.1 | Swapped `VSPSI` indices and explicitly typed variables. |
!> | 1998-11-03 | RAH | 4.2 | Removed redundant `NSOIL` output and replaced loops with `ALINIT`/`DCOPY`. |
SUBROUTINE ETSIM ()

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
