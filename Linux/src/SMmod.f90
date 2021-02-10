MODULE SMmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the SM.F files
USE SGLOBAL
!USE SGLOBAL, ONLY : NVEE
USE AL_C, ONLY : nvc, dtuz, ispack, nrd
USE AL_D, ONLY : AE, CSTOLD, CSTORE, CPLAI, ERZ, ESOIL, EINT, & 
                 msm, nsmc, nrainc, nmc, nsmt, precip_m_per_s, pnet, PE, RHOSAR, rn, s, sf, sd, ta, ts, &
                 timeuz, u, vpd, VHT
IMPLICIT NONE
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: smelt, tmelt

!TAKEN FROM SPEC_SM
!MODULE SPEC_SM
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/SPEC.SM/4.2
! Modifications:
!  GP       FEB 89    2.0     'SHE88' IMPLEMENTATION ON NEWCASTLE AMDAHL
!  GP       JUN 90    2.2     AMENDMENTS FOR VARIABLE SNOWPACK
!                             + STANDARDISE F77
!  GP       FEB 91    3.0     SHETRAN AMENDMENTS
!  GP       JUN 92    3.4     VARIABLES MOVED TO AL.D FOR HOTSTART
!                             (arrays NSMC,TM,SMELT).  PNSNOW added.
! RAH  980308  4.2  Remove DTDAYS,DTHRS,DTMIN,DTSEC.  Explicit typing.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
DOUBLEPRECISION :: USM, DDF, RHOS, ESM, HFC, HFR, HFE, HFT, ZUS, ZDS, ZOS
DOUBLEPRECISION :: RHODEF, TOPNET, PNSNOW  
LOGICAL         :: BINSMP  
INTEGER         :: IMET (NVEE), NSD  
DOUBLEPRECISION :: HEAD (20)  
DOUBLEPRECISION, PARAMETER :: RHOA = 1.29d0, &
                              RHOW = 1000.0d0, &
                              CPA = 1003.0d0, &
                              CPW = 4187.0d0, &
                              CPI = 2093.0d0, &
                              LWI = 334000.0d0, & 
                              LVW = 2500000.0d0, &
                              HFG = 2.0d0
                            !
                            !     RHOA  - DENSITY OF AIR                              KG/M**3
                            !     RHOW  - DENSITY OF WATER                            KG/M**3
                            !     CPA   - SPECIFIC HEAT OF AIR AT CONSTANT PRESSURE   J/KG/C
                            !     CPW   -    ''     ''  '' WATER ''   ''      ''      J/KG/C
                            !     CPI   -    ''     ''  '' ICE ''     ''      ''      J/KG/C
                            !     LWI   - LATENT HEAT OF FUSION                       J/KG
                            !     LVW   - LATENT HEAT OF VAPORISATION                 J/KG
                            !     HFG   - HEAT FLUX FROM GROUND              W/M**2 = J/S/M^^2
                            !     THESE QUANTITIES ARE ASasumED TO BE CONSTANT
!END MODULE SPEC_SM


PRIVATE
PUBLIC :: SMIN, rhos, head, binsmp, ddf, zos, zds, zus, nsd, rhodef, imet, smelt, tmelt, initialise_smmod
CONTAINS


!SSSSSS SUBROUTINE initialise_smmod
SUBROUTINE initialise_smmod
LOGICAL         :: first=.TRUE.
if (FIRST) then
  ALLOCATE (TMELT(max_no_snowmelt_slugs,total_no_elements))
  ALLOCATE (SMELT(max_no_snowmelt_slugs,total_no_elements))
  FIRST = .FALSE.
endif
END SUBROUTINE initialise_smmod

!SSSSSS SUBROUTINE SM
SUBROUTINE SM (IEL)  
!
!----------------------------------------------------------------------*
!
!         SNOWMELT CALCULATIONS
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     THIS SUBROUTINE CALCULATES THE AMOUNT OF SNOWMELT FROM THE
!     SNOWPACK AND THE THICKNESS OF THE SNOWPACK. IT IS CALLED
!     FROM SUBROUTINE ET FOR EACH IEL NODE AT WHICH A SNOWPACK
!     EXISTS OR AT WHICH IT IS SNOWING. THE SUBROUTINE EITHER
!     COMPLEMENTS OR BYPASSES THE ET/IT CALCULATION. CONSEQUENTLY
!     THE THROUGHFALL IS THE  NET INPUT AT THE SNOW SURFACE WHILE
!     THE PREDICTED SNOWMELT BECOMES THE NEW VALUE FOR PNET, THE
!     NET DELIVERY OF WATER TO THE GROUND SURFACE.
!     TWO CALCULATION METHODS ARE AVAILABLE:
!               MSM=1 -- DEGREE DAY
!               MSM=2 -- ENERGY BUDGET
!
!       SUBROUTINE CREATED APRIL 1981 BY JCB & EMM
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/SM/SM/4.1
! Modifications:
! RAH  961228  4.1  Initialize EFFDEP (was undefined).
!                   No leading comments.
!----------------------------------------------------------------------*
!
! Commons and distributed constants
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     VARIABLE AND UNIT SPECIFICATION
!     USM   - SNOWMELT (MM OF SNOW) DURING TIME DTUZ      MM
!     SMELT - SNOWMELT AND THROUGHFALL (MM OF WATER)      MM
!     SD    - SNOWDEPTH (MM OF SNOW)                      MM
!     DDF   - DEGREE DAY FACTOR                           MM/S/C
!     RHOS  - SPECIFIC GRAVITY OF SNOW                    --
!     TM    - TIME IN SIMULATION AT WHICH SLUG OF
!               MELTWATER REACHES BOTTOM OF SNOWPACK      HR
!     pnsnow  - INITIALLY THE NET PRECIPITATION OR
!               THROUGHFALL (MM OF WATER) TO THE TOP
!               OF THE SNOWPACK. FINALLY THE MELTWATER
!               FROM THE BOTTOM OF THE SNOWPACK TO THE
!               GROUND (MM OF WATER).                     MM
!     SF    - NET SNOWFALL (MM OF SNOW) TO THE TOP OF
!               THE SNOWPACK                              MM
!                                                         MM/HR
!     NSMC  - COUNTER USED IN ROUTING MELTWATER
!               THROUGH SNOWPACK. EQUALS NUMBER OF
!               SLUGS OF MELTWATER MOVING THROUGH SNOWPACK--
!     MSM   - EQUALS 1 FOR DEGREE DAY
!                    2 FOR ENERGY BUDGET                  --
!
!     ESM   - SNOW LOST (MM OF SNOW) TO EVAPORATION OR
!               SUBLIMATION                               MM
!     TSM   - TOTAL SNOW LOST (MM OF SNOW) FROM PACK      MM
!     HFC   - HEAT FLUX FROM ATMOSPHERIC CONVECTION       J/M^^2
!     HFE   - HEAT FROM PHASE CHANGES (EVAP. OR CONDEN.)  J/M^^2
!     HFR   - HEAT FROM RAINFALL                          J/M^^2
!     HFT   - TOTAL HEAT FLUX FROM AIR AND SOIL TO SNOW   J/M**2
!     TS    - SNOW TEMPERATURE                            C
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
! Input arguments
INTEGER         :: IEL, mr, ms, n, nnc, kl, kk, kkk, ncc
DOUBLEPRECISION :: e, dn, rich, esat, po, q, esata, ea, qa, ts2, ee, tsm
!
! Locals, etc
DOUBLEPRECISION EFFDEP  
DATA EFFDEP / 0.0D0 /  
!
!----------------------------------------------------------------------*
!
MR = NRAINC (IEL)  
MS = NMC (IEL)  
N = NVC (IEL)  
!
!         CALCULATE NET PRECIPITATION FALLING ON SNOWPACK (MM OF SNOW)
SF (IEL) = pnsnow / RHOS  
TOPNET = pnsnow  
pnsnow = zero 
pnet = zero
USM = zero
!
!         CALCULATE SNOWMELT AND SNOWDEPTH
!         --------------------------------
!
!         IF TEMPERATURE IS ABOVE FREEZING ANY PRECIPITATION
!         IS RAINFALL AND IS ADDED TO THE SNOWMELT.
!         OTHERWISE IT IS SNOW AND IS ADDED TO THE SNOWPACK.
IF (LEZERO(TA(MS))) SD (IEL) = SD (IEL) + SF (IEL)  
!
!         CHOOSE DEGREE DAY OR ENERGY BUDGET

IF (MSM.EQ.2) GOTO 10  
!
!         -----------------
!         DEGREE DAY METHOD
!         -----------------
!         calculates melt rate directly. SPA, 05/11/92
usm = ddf * (ta (ms) - two) * dtuz  
if (ta (ms) .lt.two) usm = zero
!
!        set evaporation to zero
e = 0  

goto 27  
!
!         --------------------
!         ENERGY BUDGET METHOD
!         --------------------
!
!         CALCULATE HEAT GAINED BY CONVECTION
!
!         EFFDEP (snowpack depth at anemometer site) removed from calcu-
!         lation of DN to prevent ln of 0 or negative no. SPA, 05/11/92.
   10 DN = ( (0.4 / DLOG ( (ZUS - ZDS) / ZOS) ) **2) * U (MS)  
!
!         CORRECT DN USING RICHARDSON NUMBER (SD - MM; ZUS,ZDS,ZOS - M)
!
RICH = 9.81 * (ZUS - EFFDEP / 1000.0d0 - ZDS) * (TA (MS) - TS (IEL) ) &
 / ( (TA (MS) + 273.0d0) * U (MS) * U (MS) )
IF (TA (MS) .GT.TS (IEL) ) GOTO 20  
DN = DN * (1 - 10.0d0 * RICH)  
GOTO 21  
   20 DN = DN / (1 + 10.0d0 * RICH)  
!
!         HEAT FLUX FROM CONVECTION IN TIME DTUZ (J/M^^2)
   21 HFC = RHOA * CPA * DN * (TA (MS) - TS (IEL) ) * DTUZ  
!
!         HEAT FROM RAINFALL OR SNOWFALL (MM OF WATER) IN TIME
!              DTUZ (J/M^^2)
!              (NOTE THAT SF IS IN MM OF SNOW)
!
!         IF TEMPERATURE IS ABOVE FREEZING, HEAT IS FROM RAIN
! ^^^^^^ REMOVED + LWI FROM END OF NEXT EQUATION
HFR = CPW * TA (MS)  
!         IF TEMPERATURE IS BELOW FREEZING, HEAT IS FROM SNOW
! --- CORRECTIONS MADE HERE ACCORDING TO JCB
!        IF(TA(MS).LE.0.0) HFR = CPI * TA(MS)
!        HFR = HFR - CPI * TS(IEL)
IF (LEZERO(TA(MS))) HFR = CPI * (TA (MS) - TS (IEL) )  
! --- CORRECTIONS ENDED
HFR = RHOW * SF (IEL) * RHOS * HFR / 1000.0d0  
!
!         CALCULATE HEAT FROM WATER PHASE CHANGE
!
!           ESAT=SATURATED VAPOUR PRESSURE AT SNOW TEMPERATURE
!           QA=SPECIFIC HUMIDITY
!           Q=SATURATED SPECIFIC HUMIDITY AT SNOW TEMPERATURE
!           PO=STANDARD PRESSURE AT GROUND ELEVATION
!
ESAT = (17.044d0 + (TS (IEL) / five - three) * (5.487d0 + (TS (IEL) &
 / five - three) * (0.776d0 + (TS (IEL) / five - three) * (0.1063d0 + &
 (TS (IEL) / five - three) * 0.003d0) ) ) )
PO = 1012. * (one - 0.0065d0 * ZGRUND (IEL) / 288.0d0) * 100.0d0  
Q = (0.62197d0 * ESAT) / ( (PO / 1.0045d0) - (0.37803d0 * ESAT) )  
ESATA = (17.044d0 + (TA (MS) / five - three) * (5.487d0 + (TA (MS) &
 / five - three) * (0.776d0 + (TA (MS) / five - three) * (0.1063d0 + (TA (MS) &
 / five - three) * 0.003d0) ) ) )
EA = ESATA - VPD (MS)  
QA = (0.62197d0 * EA) / ( (PO / 1.0045d0) - (0.37803d0 * EA) )  
!         MASS EVAPORATED (E) IN KG/S/M^^2
E = RHOA * DN * (Q - QA)  
!
!         HEAT FROM PHASE CHANGE IN TIME DTUZ (J/M^^2)
HFE = (LVW + LWI - CPI * TS (IEL) ) * E * DTUZ  
!
!         TOTAL HEAT FLUX FROM AIR AND SOIL TO SNOW
!         IN TIME DTUZ (J/M^^2)
!
!
HFT = HFC + HFR - HFE+ (HFG + RN (MS) ) * DTUZ  
!
!         Fix incorporated to stop excessive energy fluxes in/out
!         of thin snowpacks. SPA, 05/11/92.
!
if ( (sd (iel) .le.100.0d0) .and. (LTZERO(hft)) ) then  
   hft = (ta (ms) - ts (iel) ) * (cpi * rhos * sd (iel) )  
endif  
!
!         CALCULATE SNOWMELT USM (MM OF SNOW)
!         -----------------------------------
!         CALCULATE HYPOTHETICAL SNOW TEMPERATURE AS IF ALL HEAT
!         INPUT CREATED TEMPERATURE RISE ONLY WITH NO PHASE CHANGE
!         ( N.B. RHOS IS SPECIFIC GRAVITY AND SD IS IN MM. THEREFORE
!         SNOWDEPTH IN METRES * SNOW DENSITY, WHICH IS REQUIRED IN
!         THE FOLLOWING, IS (SD/1000)*(RHOS*1000) WHICH EQUALS
!         SD*RHOS.)
TS2 = (HFT / (CPI * RHOS * SD (IEL) ) ) + TS (IEL)  
IF (TS2.LT. - 50.0d0) TS2 = - 50.0d0  
IF (LTZERO(TS2)) GOTO 22  
!
!         SNOW TEMPERATURE > 0 SO CALCULATE EXCESS HEAT
!         AVAILABLE FOR MELTING SNOW
HFT = HFT - ( ( - TS (IEL) ) * CPI * RHOS * SD (IEL) )  
TS2 = zero
USM = HFT / (LWI * RHOS)  
GOTO 23  
!
!         SNOW TEMPERATURE < 0 SO NO MELTING
   22 USM = 0.0  
   23 TS (IEL) = TS2  
!
!         CALCULATE SNOWDEPTH SD (MM OF SNOW)
!
EE = E  
!         EVAPORATION CHANGES DEPTH BUT CONDENSATION DOES NOT
IF (LTZERO(E) .AND. ISZERO(TS (IEL))) E = zero  
!         SNOWMELT CHANGES DEPTH BUT FREEZING DOES NOT
   27 IF (LTZERO(USM)) USM = zero
!         EVAPORATION LOSS ESM IN TIME DTUZ (MM OF SNOW)
ESM = E * DTUZ / RHOS  
!         TOTAL LOSS FROM SNOWPACK TSM IN TIME DTUZ (MM OF SNOW)
TSM = USM + ESM  
IF (TSM.GT.SD (IEL) ) GOTO 24  
SD (IEL) = SD (IEL) - TSM  
GOTO 5  
   24 IF (ESM.GT.SD (IEL) ) GOTO 25  
SD (IEL) = SD (IEL) - ESM  
USM = SD (IEL)  
GOTO 26  
   25 ESM = SD (IEL)  
USM = zero  
   26 SD (IEL) = zero  
!
!         ROUTE SNOWMELT THROUGH SNOWPACK
!         -------------------------------
!         SET COUNTERS
    5 NCC = 0  
!          NSMC IS NUMBER OF SLUGS OF MELTWATER STILL
!          MOVING THROUGH SNOWPACK
NSMC (IEL) = NSMC (IEL) + 1  
NNC = NSMC (IEL)  
IF (NSMC (IEL) .LE.max_no_snowmelt_slugs) GOTO 34  
WRITE (6, 30) NSMC (IEL), IEL  
   30 FORMAT(1H0,'NO OF MELTWATER SLUGS IS',I5,' AT ELEMENT', &
& I4,' WHICH EXCEEDS AVAILABLE MEMORY STORE SIZE')
STOP  
!
!          ADD ANY RAINFALL TO SNOWMELT AND CONVERT TOTAL TO MM OF WATER
!  ^^^^^  TENTATIVELY: IF SNOW TEMPERATURE REMAINS <0, CHANGE RAIN TO
!         SNOW AND ADD TO SNOWPACK DEPTH
   34 IF (LEZERO(TA(MS))) GOTO 32  
SMELT (NNC, IEL) = (USM + SF (IEL) ) * RHOS  
SF (IEL) = zero  
GOTO 33  
!  36 SD(IEL) = SD(IEL) + SF(IEL)
   32 SMELT (NNC, IEL) = USM * RHOS  
   33 IF (GTZERO(SMELT(NNC,IEL))) GOTO 35  
NSMC (IEL) = NSMC (IEL) - 1  
NNC = NSMC (IEL)  
GOTO 31  
!
!         CALCULATE TIME TM AT WHICH MELTWATER SLUG WILL REACH
!         BOTTOM OF SNOWPACK (TM - HR, SD - MM OF SNOW)
   35 tmelt (NNC, IEL) = (0.7448 * SD (IEL) / 1000. + 1.429) * SD (IEL) &
 / 1000. + TIMEUZ
!         HAS CURRENT TIME TIMEUZ REACHED TIME AT WHICH
!         MELTWATER SLUGS REACH BOTTOM OF SNOWPACK?
   14 DO 11 KL = 1, NNC  
   IF (TIMEUZ.LT.tmelt(KL, IEL) ) GOTO 11  
!         CALCULATE MELTWATER REACHING GROUND
   pnsnow = SMELT (KL, IEL) + pnsnow  
   NCC = NCC + 1  
   11 END DO  
!         IF MELTWATER SLUG HAS REACHED BOTTOM OF SNOWPACK
!         IN LATEST TIMESTEP, REMOVE THAT SLUG FROM STORE
!         AND REPLACE WITH SUBSEQUENT SLUG. ADJUST ORDER
!         OF ALL OTHER SLUGS ACCORDINGLY.
IF (NCC.EQ.0) GOTO 3  
NSMC (IEL) = NSMC (IEL) - NCC  
KK = NSMC (IEL)  
!         IF NSMC = 0 THERE ARE NO MELTWATER SLUGS IN SNOWPACK
IF (KK.EQ.0) GOTO 3  
DO 13 KL = 1, KK  
   KKK = KL + NCC  
   tmelt(KL, IEL) = tmelt(KKK, IEL)  
   SMELT (KL, IEL) = SMELT (KKK, IEL)  
   13 END DO  
GOTO 3  
!
!         CHECK IF THERE ARE ANY MELTWATER SLUGS STILL IN SNOWPACK
   31 IF (NSMC (IEL) .LE.0) GOTO 3  
NCC = 0  
GOTO 14  
!         CONVERT SF TO MM OF SNOW / HOUR
    3 SF (IEL) = (SF (IEL) / DTUZ) * 3600.  
!         CONVERT pnsnow (mm) to PNET TO MM OF WATER / SEC

pnet = pnsnow / dtuz  
IF (GTZERO(SD(IEL))) THEN  
   ISPACK (IEL) = .TRUE.  
ELSE  
   ISPACK (IEL) = .FALSE.  

ENDIF  
RETURN  
END SUBROUTINE SM


!SSSSSS SUBROUTINE SMET  
SUBROUTINE SMET (IEL)  
INTEGER, INTENT(IN) :: iel
INTEGER :: ms, mr, n, k, kk
DOUBLEPRECISION :: sndep
!
!       IT/ET CALCULATIONS FOR SNOWMELT COMPONENT
!       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!  THIS SUBROUTINE CARRIES OUT EVAPOTRANSPIRATION AND
!  INTERCEPTION CALCULATIONS IF THERE IS A SNOWPACK, IF IT IS
!  SNOWING OR IF THE TEMPERATURE IS BELOW FREEZING. IT IS
!  CALLED FROM SUBROUTINE ET(IEL) FOR EACH IEL UZ COMPONENT
!  NODE. VARIABLES ARE AS IN ET,SM,ETC.
!
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
MS = NMC (IEL)  
MR = NRAINC (IEL)  
N = NVC (IEL)  
!
!         USE SPATIALLY VARIABLE RHOS (OR DEFAULT IF ZERO)
!
IF (NSD.EQ.1) RHOS = RHOSAR (IEL)  
IF (ISZERO(RHOS)) RHOS = RHODEF  
!
!         IS THE SNOWDEPTH GREATER THAN THE VEGETATION HEIGHT?
SNDEP = SD (IEL) / 1000.  
IF (ISZERO(SNDEP)) GOTO 309  
IF (SNDEP.LT.VHT (N) ) GOTO 302  
!
!         SNOW COVERS THE VEGETATION SO THERE IS NO CANOPY INTERCEPTION
!         NO EVAPOTRANSPIRATION AND NO SOIL EVAPORATION
pnsnow = precip_m_per_s(iel) * 1000. * DTUZ  
CSTOLD = CSTORE (IEL)  
CPLAI = zero  
ERZ = zero  
ESOIL = zero  
EINT = zero
AE = zero
PE = zero
K = NRD (N)  
DO 303 KK = 1, K  
   S (KK) = zero  
  303 END DO  
!
!         CALL SNOWMELT ROUTINE
CALL SM (IEL)  
RETURN  
  302 CPLAI = CPLAI * (VHT (N) - SD (IEL) / 1000.) / VHT (N)  
!
!         IS THE TEMPERATURE ABOVE FREEZING?
IF (GTZERO(TA(MS))) GOTO 308  
!
!         TEMPERATURE IS BELOW FREEZING
!
!  INTERCEPTION CALCULATIONS FOR TEMPERATURES BELOW FREEZING
!  ---------------------------------------------------------
!
!         THERE IS NO EVAPOTRANSPIRATION AND NO SOIL EVAPORATION.
!         PRECIPITATION FALLING ON THE CANOPY IS ASasumED TO PASS
!         WITHOUT DELAY THROUGH THE VEGETATION LAYER. IE THERE
!         IS NO INTERCEPTION OR CANOPY STORAGE OF SNOW.
!
!         SNOWFALL (IN MM OF WATER) REACHING GROUND OR SNOWPACK
  309 pnsnow = precip_m_per_s(IEL) * 1000. * DTUZ  
CSTOLD = CSTORE (IEL)  
ERZ = zero  
ESOIL = zero  
EINT = zero
AE = zero
PE = zero
K = NRD (N)  
DO 307 KK = 1, K  
   S (KK) = zero  
  307 END DO  
!
!         IS IT SNOWING OR IS THERE A SNOWPACK?
IF (GTZERO(precip_m_per_s(IEL))) GOTO 306  
IF (GTZERO(SD(IEL))) GOTO 306  
RETURN  
!
!         CALL SNOWMELT ROUTINE
  306 CALL SM (IEL)  
RETURN  
!
!         TEMPERATURE IS ABOVE FREEZING
!
!  INTERCEPTION CALCULATIONS FOR TEMPERATURES ABOVE FREEZING
!  ---------------------------------------------------------
!
!         THERE IS EVAPOTRANSPIRATION AND INTERCEPTION (OF
!         RAINFALL) WHICH MUST BE MODELLED BY SUBROUTINE ET.
!         IT IS ASasumED THAT THERE IS NO CANOPY STORAGE OF SNOW
!         TO BE MODELLED. IF THERE IS A SNOWPACK THERE IS NO
!         SOIL EVAPORATION.
  308 NSMT = 1  
RETURN  
END SUBROUTINE SMET


!SSSSSS SUBROUTINE SMIN
SUBROUTINE SMIN (IEL)  
INTEGER, INTENT(IN) :: iel
INTEGER :: ms
!
!       TESTS FOR SNOW CALCULATION REQUIREMENTS
!       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!  THIS SUBROUTINE CHECKS TO SEE IF A SNOWMELT OR SNOW ET
!  CALCULATION IS NEEDED. IF THE ET CALCULATIONS HAVE NOT BEEN
!  CARRIED OUT, THE SNOW ET ROUTINE IS CALLED IF A SNOWPACK
!  EXISTS OR IF IT IS SNOWING (PRECIPITATION WITH TEMPERATURE
!  BELOW FREEZING) OR IF THE TEMPERATURE IS BELOW FREZING.
!  IF THE ET CALCULATIONS HAVE BEEN CARRIED OUT, THE SNOWMELT
!  CALCULATION IS CALLED IF A SNOWPACK EXISTS AND THE
!  TEMPERATURE IS ABOVE FREEZING.
!
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
CALL INITIALISE_SMMOD()
MS = NMC (IEL)  
!         IF ET CALCULATIONS HAVE ALREADY BEEN CARRIED OUT AND
!         TEMPERATURE IS ABOVE FREEZING (REQUIRING THE CONDITION
!         NSMT = 1) CALL SNOWMELT ROUTINE IF A SNOWPACK EXISTS
IF (NSMT.EQ.1) GOTO 11  
!
!         IF ET CALCULATIONS HAVE NOT YET BEEN CARRIED OUT, IS
!         THERE A SNOWPACK?
IF (GTZERO(SD(IEL))) GOTO 10  
!
!         IF ET CALCULATIONS HAVE NOT YET BEEN CARRIED OUT,IS
!         TEMPERATURE ABOVE FREEZING?
IF (LEZERO(TA(MS))) GOTO 10  
NSMT = 1  
RETURN  
!
!         CALL ET ROUTINE FOR SNOW/FREEZING TEMPERATURES
   10 CALL SMET (IEL)  
RETURN  
!
!         IF ET CALCULATIONS HAVE ALREADY BEEN CARRIED OUT,
!         SNOWMELT CALCULATION IS REQUIRED IF A SNOWPACK EXISTS.
!         (THE FOLLOWING CAN BE REACHED ONLY IF TEMPERATURE
!         IS ABOVE FREEZING)
   11 IF (LEZERO(SD(IEL))) RETURN  
!
!         THERE IS STILL A SNOWPACK SO THERE IS NO SOIL
!         EVAPORATION
ESOIL = zero
!
!         addition by spa, 17/11/92. pnet output from et(iel) as a rate.
!         Needs to be a depth for input into sm(iel).
pnsnow = pnet * dtuz  
!
!
!         CALL SNOWMELT ROUTINE
CALL SM (IEL)  
RETURN  
END SUBROUTINE SMIN
END MODULE SMmod