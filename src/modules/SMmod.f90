!> summary: Snow accumulation and melt calculations.
!> author: JCB; EMM; GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University
!>
!> `SMmod` implements the SHETRAN snow model. It updates snowpack depth,
!> snowpack temperature, evaporation/sublimation losses, and meltwater delivery
!> to the ground surface. The main routine supports both a degree-day method and
!> an energy-budget method selected by `MSM`.
!>
!> The degree-day option estimates melt directly from air temperature and a
!> degree-day factor. The energy-budget option computes heat fluxes from
!> atmospheric convection, rainfall or snowfall, phase change, ground heat flux,
!> and net radiation.
!>
!> Snow depth `SD`, snowfall `SF`, and routed meltwater `SMELT` are stored in
!> millimetres in the legacy snow calculations. `SM` replaces `PNET` with the
!> meltwater delivery from the bottom of the snowpack, so downstream ET/VSS/OC
!> calculations receive liquid-water input rather than raw snowfall.
!>
!> The manual's snowmelt input file supplies:
!>
!> | Records | Data |
!> |:--------|:-----|
!> | `SM2` | `BINSMP`, the snow-input print flag. |
!> | `SM4` | Degree-day factor `DDF`, default snow specific gravity `RHOS`, initial snow temperature `TSIN`, spatial snowpack flag `NSD`, and method flag `MSM`. |
!> | `SM6`/`SM6b` | Energy-budget aerodynamic parameters `ZOS`, `ZDS`, `ZUS`, and meteorological-station element locations `IMET`, required only for `MSM=2`. |
!> | `SM8` | Uniform initial snow depth, used when `NSD=0`. |
!> | `SM11`/`SM14` | Spatial initial snow depth `SD` and snow specific gravity `RHOSAR`, used when `NSD=1`. |
!>
!> @note In the degree-day branch the implemented melt threshold is `TA >= 2 C`,
!> not simply air temperature above freezing.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1981-04 | JCB/EMM | - | Original snowmelt subroutine created. |
!> | 1989-02 | GP | 2.0 | SHE88 implementation on Newcastle AMDAHL. |
!> | 1990-06 | GP | 2.2 | Added variable snowpack amendments and standardized Fortran 77. |
!> | 1991-02 | GP | 3.0 | SHETRAN amendments. |
!> | 1992-06 | GP | 3.4 | Moved selected variables to `AL_D` for hotstart and added `PNSNOW`. |
!> | 1996-12-28 | RAH | 4.1 | Initialized `EFFDEP`. |
!> | 1998-03-08 | RAH | 4.2 | Removed redundant time constants and added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90 and replaced the `SM.F` files. |
!> @endhistory
MODULE SMmod
USE SGLOBAL
!USE SGLOBAL, ONLY : NVEE
USE AL_C, ONLY : nvc, dtuz, ispack, nrd
USE AL_D, ONLY : AE, CSTOLD, CSTORE, CPLAI, ERZ, ESOIL, EINT, &
                 msm, nsmc, nrainc, nmc, nsmt, precip_m_per_s, pnet, PE, RHOSAR, rn, s, sf, sd, ta, ts, &
                 timeuz, u, vpd, VHT
IMPLICIT NONE
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: smelt, tmelt

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


!> Allocates snowmelt slug storage arrays.
!>
!> The routine allocates `TMELT` and `SMELT` once, using the maximum configured
!> number of snowmelt slugs and active element count.
SUBROUTINE initialise_smmod
LOGICAL         :: first=.TRUE.
if (FIRST) then
  ALLOCATE (TMELT(max_no_snowmelt_slugs,total_no_elements))
  ALLOCATE (SMELT(max_no_snowmelt_slugs,total_no_elements))
  FIRST = .FALSE.
endif
END SUBROUTINE initialise_smmod



!> Updates snowpack and meltwater delivery for one element.
!>
!> `SM` is called for each element with an existing snowpack or snowfall input.
!> It converts net precipitation into snowfall or rainfall, updates snow depth,
!> computes melt by either degree-day or energy-budget logic, routes meltwater
!> slugs through the snowpack, and writes the resulting water delivery to
!> `PNET`.
!>
!> Most internal snow amounts are in millimetres. `SD` is snowpack depth, `SF`
!> is snowfall expressed as snow depth, and `PNET` is overwritten with liquid
!> water delivery from the snowpack.
!>
!> `SM` either complements or bypasses the normal ET/interception calculation:
!> the current throughfall or net precipitation `PNSNOW` is the water input to
!> the top of the snowpack, and the snowmelt delivered from the bottom of the
!> snowpack becomes the new `PNET`, the liquid-water input to the ground
!> surface. Two snowmelt methods are available:
!>
!> | `MSM` | Method | Main calculation |
!> |:------|:-------|:-----------------|
!> | 1 | Degree day | Melt is proportional to air temperature above the implemented 2 C threshold. |
!> | 2 | Energy budget | Melt is calculated from atmospheric, rainfall/snowfall, phase-change, ground, and radiation heat fluxes. |
!>
!> The principal snow variables and units are:
!>
!> | Variable | Meaning | Units |
!> |:---------|:--------|:------|
!> | `USM` | Snowmelt during the timestep, expressed as snow depth. | mm snow |
!> | `SMELT` | Routed melt plus liquid throughfall held as meltwater slugs. | mm water |
!> | `SD` | Snowpack depth. | mm snow |
!> | `DDF` | Degree-day melt factor. | mm/s/C |
!> | `RHOS` | Specific gravity of snow. | - |
!> | `TMELT` | Time at which a meltwater slug reaches the bottom of the snowpack. | h |
!> | `PNSNOW` | Initially water input to the snowpack top; finally water released to the ground. | mm water |
!> | `SF` | Net snowfall to the top of the snowpack. | mm snow, converted to mm snow/hr at return |
!> | `NSMC` | Number of meltwater slugs still moving through the snowpack. | - |
!> | `ESM` | Snow depth lost to evaporation or sublimation. | mm snow |
!> | `TSM` | Total snow depth lost from the pack. | mm snow |
!> | `HFC` | Atmospheric-convection heat flux over the timestep. | J/m^2 |
!> | `HFE` | Heat from evaporation/condensation phase change. | J/m^2 |
!> | `HFR` | Heat from rainfall or snowfall. | J/m^2 |
!> | `HFT` | Total heat flux from air, ground, and radiation to snow. | J/m^2 |
!> | `TS` | Snow temperature. | C |
!>
!> The input water depth is first converted to snowfall depth with
!>
!> \[
!> SF = \frac{PNSNOW}{RHOS}.
!> \]
!>
!> If air temperature is at or below 0 C, this snowfall is added to `SD`; if it
!> is above 0 C, the precipitation is treated as rainfall and is added to the
!> meltwater slug after melt has been calculated.
!>
!> In degree-day mode, the implemented melt depth is
!>
!> \[
!> USM = \max\left(0,\; DDF\,(TA-2)\,DTUZ\right),
!> \]
!>
!> and evaporation loss is set to zero.
!>
!> In energy-budget mode, the exchange coefficient is
!>
!> \[
!> DN =
!> \left(\frac{0.4}{\ln((ZUS-ZDS)/ZOS)}\right)^2 U,
!> \]
!>
!> modified by a Richardson-number stability correction. The main heat terms are
!>
!> \[
!> HFC = \rho_a c_{p,a} DN (TA-TS)DTUZ,
!> \]
!>
!> \[
!> HFR =
!> \rho_w\,SF\,RHOS\,c_p^\* / 1000,
!> \]
!>
!> where \(c_p^\*=c_{p,w}TA\) for rain and
!> \(c_p^\*=c_{p,i}(TA-TS)\) for snowfall, and
!>
!> \[
!> HFE = (L_{vw}+L_{wi}-c_{p,i}TS)\,E\,DTUZ.
!> \]
!>
!> The total heat available to the pack is
!>
!> \[
!> HFT = HFC + HFR - HFE + (HFG + RN)DTUZ.
!> \]
!>
!> The hypothetical new snow temperature is
!>
!> \[
!> TS_2 = TS + \frac{HFT}{c_{p,i}\,RHOS\,SD}.
!> \]
!>
!> If `TS2 < 0`, no snow melts and `TS` is updated to `TS2`. Otherwise, the
!> heat first warms the snowpack to 0 C and the residual melts snow:
!>
!> \[
!> USM =
!> \frac{HFT - (-TS)c_{p,i}RHOS\,SD}{L_{wi}RHOS},
!> \qquad TS \leftarrow 0.
!> \]
!>
!> Two guards limit extreme energy-budget behaviour: if the pack is no deeper
!> than 100 mm and `HFT` is negative, `HFT` is replaced by the heat needed to
!> move the pack toward air temperature; and `TS2` is floored at -50 C.
!>
!> Snowpack depth is reduced by melt plus evaporation,
!>
!> \[
!> TSM = USM + ESM,\qquad ESM = \frac{E\,DTUZ}{RHOS},
!> \]
!>
!> capped so losses cannot exceed the available snowpack. The meltwater slug
!> inserted into the snowpack routing store is
!>
!> \[
!> SMELT =
!> \begin{cases}
!> (USM + SF)RHOS, & TA > 0,\\
!> USM\,RHOS, & TA \le 0.
!> \end{cases}
!> \]
!>
!> A positive slug is assigned a release time
!>
!> \[
!> TMELT =
!> \left(0.7448\frac{SD}{1000}+1.429\right)\frac{SD}{1000}
!> + TIMEUZ.
!> \]
!>
!> Slugs whose `TMELT` is less than or equal to `TIMEUZ` are summed into
!> `PNSNOW`, removed from the routing store, and returned to the wider model as
!>
!> \[
!> PNET = \frac{PNSNOW}{DTUZ}.
!> \]
!>
!> At return, `SF` is converted from a timestep snow depth to a snow-depth rate
!> in mm/hr, `ISPACK` records whether snow remains, and `PNET` is a liquid-water
!> delivery rate in mm/s.
!>
!> @note The routine operates mainly through module/global state imported from
!> `SGLOBAL`, `AL_C`, and `AL_D`; its only dummy argument is the element index.
!> @endnote
SUBROUTINE SM (IEL)
! Input arguments
INTEGER         :: IEL !! Element index for which snowpack and melt are updated.
INTEGER         :: mr, ms, n, nnc, kl, kk, kkk, ncc
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



!> Applies evapotranspiration losses to an existing snowpack for one element.
!>
!> `SMET` carries out the snow-related evapotranspiration and interception
!> control for an ET element when a snowpack exists, when precipitation is snow,
!> or when air temperature is below freezing. It is called from the ET path for
!> each upper-zone element and uses the same shared ET and snow variables as
!> [[sm]] and the ET routines.
!>
!> The routine reduces snowpack depth for evaporation or sublimation, updates
!> snow temperature in the energy-budget case through [[sm]], and identifies any
!> excess rainfall or evaporation demand that must still be handled by the
!> normal ET calculation.
!>
!> Its main branch logic is:
!>
!> | Condition | Action |
!> |:----------|:-------|
!> | Snow depth exceeds vegetation height. | Snow covers the vegetation; canopy interception, transpiration, soil evaporation, actual ET, and potential ET are set to zero, throughfall is sent directly to [[sm]], and snowmelt processing supplies `PNET`. |
!> | Snow depth is non-zero but below vegetation height. | Canopy leaf area `CPLAI` is reduced in proportion to exposed vegetation height: `CPLAI = CPLAI * (VHT - SD/1000) / VHT`. |
!> | `TA <= 0`. | Precipitation is treated as snowfall reaching the ground or snowpack without canopy storage delay; ET terms are zeroed and [[sm]] is called if precipitation or snowpack is present. |
!> | `TA > 0`. | Rainfall interception and evapotranspiration must be handled by normal ET processing, so `NSMT` is set to 1. If a snowpack remains, soil evaporation is suppressed later by [[smin]]. |
!>
!> Snowpack input passed to [[sm]] is
!>
!> \[
!> PNSNOW = precip\_m\_per\_s(IEL)\,1000\,DTUZ,
!> \]
!>
!> converting the current precipitation rate from m/s to a millimetre water
!> depth over the timestep.
!>
!> When `NSD=1`, the active snow specific gravity is taken from `RHOSAR(IEL)`;
!> if that value is zero, the default `RHODEF` is used.
SUBROUTINE SMET (IEL)
INTEGER, INTENT(IN) :: iel
INTEGER :: ms, mr, n, k, kk
DOUBLEPRECISION :: sndep
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
!> Snow wrapper called from ET/interception processing for one element.
!>
!> `SMIN` decides whether snow processing is needed, converts the current net
!> precipitation to snowpack input, calls [[sm]] when snowfall or snowpack is
!> present, or calls [[smet]] when only snowpack evaporation/sublimation is
!> required.
!>
!> | State | Action |
!> |:------|:-------|
!> | `NSMT /= 1`, snowpack exists, or `TA <= 0` | Call [[smet]] so snow/freezing-temperature ET and interception logic runs. |
!> | `NSMT /= 1`, no snowpack, and `TA > 0` | Set `NSMT=1` and return so normal ET/interception can proceed. |
!> | `NSMT == 1` and no snowpack remains | Return; normal ET has already handled the timestep. |
!> | `NSMT == 1` and snowpack remains | Suppress soil evaporation, convert the ET-produced `PNET` rate to a depth with `PNSNOW=PNET*DTUZ`, and call [[sm]]. |
SUBROUTINE SMIN (IEL)
INTEGER, INTENT(IN) :: iel
INTEGER :: ms

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
