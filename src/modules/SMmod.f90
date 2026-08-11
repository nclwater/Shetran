!> @brief Snow accumulation and melt calculations.
!>
!> `SMmod` implements the SHETRAN snow model. It updates snowpack depth,
!> snowpack temperature, evaporation/sublimation losses, and meltwater
!> delivery to the ground surface. The main routine, [[sm]], supports both a
!> degree-day method and an energy-budget method selected by `MSM`.
!>
!> The degree-day option estimates melt directly from air temperature and a
!> degree-day factor. The energy-budget option computes heat fluxes from
!> atmospheric convection, rainfall or snowfall, phase change, ground heat
!> flux, and net radiation.
!>
!> Snow depth `SD`, snowfall `SF`, and routed meltwater `SMELT` are stored in
!> millimetres. [[sm]] replaces `PNET` with the meltwater delivered from the
!> bottom of the snowpack, so downstream [[etmod]]/[[vsmod]]/[[ocmod]]
!> calculations receive liquid-water input rather than raw snowfall. [[smin]]
!> is the entry point called from [[etmod:etin]]; it dispatches to [[smet]]
!> (snow/freezing-temperature ET) or [[sm]] (melt routing) as required.
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
!> @note
!> In the degree-day branch the implemented melt threshold is `TA >= 2 C`, not
!> simply air temperature above freezing.
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
!> | 1996-12-28 | RAH | 4.1 | Initialised `EFFDEP`, which was previously undefined. |
!> | 1998-03-08 | RAH | 4.2 | Removed redundant time constants and added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90 and replaced the `SM.F` files. |
!> | 2026-04-03 to 2026-04-13 | SvB | 4.6.1 | Modernisation pass: replaced the `1H0` Hollerith edit descriptor, removed `GOTO`-driven control flow in favour of structured `IF`/`DO` blocks with explicit `IMPLICIT NONE`/`INTENT`, replaced `DLOG` with the generic `LOG`, and pre-computed the repeated `ESAT`/`ESATA` temperature-ratio subexpression (see [[sm]] for details). |
!> @endhistory
MODULE SMmod
   USE SGLOBAL
!USE SGLOBAL, ONLY : NVEE
   USE AL_C, ONLY : nvc, dtuz, ispack, nrd
   USE AL_D, ONLY : AE, CSTOLD, CSTORE, CPLAI, ERZ, ESOIL, EINT, &
      msm, nsmc, nrainc, nmc, nsmt, precip_m_per_s, pnet, PE, RHOSAR, rn, s, sf, sd, ta, ts, &
      timeuz, u, vpd, VHT
   IMPLICIT NONE
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: smelt !! Routed meltwater slugs by slug number and element (mm water).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: tmelt !! Release time for each routed meltwater slug (h).

   DOUBLEPRECISION :: USM    !! Snowmelt during the current timestep (mm snow).
   DOUBLEPRECISION :: DDF    !! Degree-day melt factor (mm/s/C).
   DOUBLEPRECISION :: RHOS   !! Active snow specific gravity for the current element.
   DOUBLEPRECISION :: ESM    !! Snow depth lost to evaporation or sublimation (mm snow).
   DOUBLEPRECISION :: HFC    !! Atmospheric-convection heat flux over the timestep (J/m^2).
   DOUBLEPRECISION :: HFR    !! Heat supplied by rainfall or snowfall over the timestep (J/m^2).
   DOUBLEPRECISION :: HFE    !! Latent heat term for evaporation or condensation over the timestep (J/m^2).
   DOUBLEPRECISION :: HFT    !! Net heat flux available to the snowpack over the timestep (J/m^2).
   DOUBLEPRECISION :: ZUS    !! Anemometer height above ground for energy-budget snowmelt (m).
   DOUBLEPRECISION :: ZDS    !! Zero-plane displacement height for snow aerodynamic exchange (m).
   DOUBLEPRECISION :: ZOS    !! Snow-surface roughness height for aerodynamic exchange (m).
   DOUBLEPRECISION :: RHODEF !! Default snow specific gravity used when spatial `RHOSAR` is zero.
   DOUBLEPRECISION :: TOPNET !! Water input to the snowpack before routing (mm water).
   DOUBLEPRECISION :: PNSNOW !! Water depth passed into or released from the snowpack in the current step (mm water).
   LOGICAL         :: BINSMP !! Snow-input echo-print flag.
   INTEGER         :: IMET (NVEE) !! Meteorological-station element index for each vegetation type in energy-budget mode.
   INTEGER         :: NSD         !! Initial snowpack mode: uniform (`0`) or spatial (`1`).
   DOUBLEPRECISION :: HEAD (20)   !! Snow input title/header workspace retained for legacy I/O.
   DOUBLEPRECISION, PARAMETER :: RHOA = 1.29d0      !! Density of air (kg/m^3).
   DOUBLEPRECISION, PARAMETER :: RHOW = 1000.0d0    !! Density of water (kg/m^3).
   DOUBLEPRECISION, PARAMETER :: CPA = 1003.0d0     !! Specific heat of air at constant pressure (J/kg/C).
   DOUBLEPRECISION, PARAMETER :: CPW = 4187.0d0     !! Specific heat of water (J/kg/C).
   DOUBLEPRECISION, PARAMETER :: CPI = 2093.0d0     !! Specific heat of ice (J/kg/C).
   DOUBLEPRECISION, PARAMETER :: LWI = 334000.0d0   !! Latent heat of fusion (J/kg).
   DOUBLEPRECISION, PARAMETER :: LVW = 2500000.0d0  !! Latent heat of vaporisation (J/kg).
   DOUBLEPRECISION, PARAMETER :: HFG = 2.0d0        !! Ground heat flux to snow (W/m^2).

   PRIVATE
   PUBLIC :: SMIN, rhos, head, binsmp, ddf, zos, zds, zus, nsd, rhodef, imet, smelt, tmelt, initialise_smmod
CONTAINS


!> Allocates snowmelt slug storage arrays.
!>
!> The routine allocates `TMELT` and `SMELT` once, on the first call, using
!> the maximum configured number of snowmelt slugs (`max_no_snowmelt_slugs`,
!> see [[sglobal]]) and the active element count (`total_no_elements`). It is
!> called unconditionally from [[smin]] on every timestep; the local `FIRST`
!> flag is initialised to `.TRUE.` in its declaration, which gives it an
!> implicit `SAVE` attribute, so the guard and the allocation both run only on
!> the first call.
!>
!> @note
!> A historical defect (`CHANGELOG.md`, undated, between the 2012-11-21 and
!> pre-2015 entries) records that this `FIRST`-guarded allocation pattern was
!> itself the fix for a prior bug; the faulty behaviour it replaced and the
!> exact fix date are not otherwise recorded.
!> @endnote
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
   !> `SM` is called for each element with an existing snowpack or snowfall
   !> input. It converts net precipitation into snowfall or rainfall, updates
   !> snow depth, computes melt by either degree-day or energy-budget logic,
   !> routes meltwater slugs through the snowpack, and writes the resulting
   !> water delivery to `PNET`.
   !>
   !> Most internal snow amounts are in millimetres. `SD` is snowpack depth,
   !> `SF` is snowfall expressed as snow depth, and `PNET` is overwritten with
   !> liquid water delivery from the snowpack.
   !>
   !> `SM` either complements or bypasses the normal ET/interception
   !> calculation: the current throughfall or net precipitation `PNSNOW` is
   !> the water input to the top of the snowpack, and the snowmelt delivered
   !> from the bottom of the snowpack becomes the new `PNET`, the liquid-water
   !> input to the ground surface. Two snowmelt methods are available:
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
   !> If air temperature is at or below 0 C, this snowfall is added to `SD`;
   !> if it is above 0 C, the precipitation is treated as rainfall and is
   !> added to the meltwater slug after melt has been calculated.
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
   !> modified by a Richardson-number stability correction. The main heat
   !> terms are
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
   !> Two guards limit extreme energy-budget behaviour: if the pack is no
   !> deeper than 100 mm and `HFT` is negative, `HFT` is replaced by the heat
   !> needed to move the pack toward air temperature; and `TS2` is floored at
   !> -50 C.
   !>
   !> Snowpack depth is reduced by melt plus evaporation,
   !>
   !> \[
   !> TSM = USM + ESM,\qquad ESM = \frac{E\,DTUZ}{RHOS},
   !> \]
   !>
   !> capped so losses cannot exceed the available snowpack. The meltwater
   !> slug inserted into the snowpack routing store is
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
   !> `PNSNOW`, removed from the routing store, and returned to the wider
   !> model as
   !>
   !> \[
   !> PNET = \frac{PNSNOW}{DTUZ}.
   !> \]
   !>
   !> At return, `SF` is converted from a timestep snow depth to a snow-depth
   !> rate in mm/hr, `ISPACK` records whether snow remains, and `PNET` is a
   !> liquid-water delivery rate in mm/s.
   !>
   !> @note
   !> The routine operates mainly through module/global state imported from
   !> [[sglobal]], [[al_c]], and [[al_d]]; its only dummy argument is the
   !> element index. The energy-budget saturation-vapour-pressure polynomial
   !> is evaluated for both the snow surface and the air using a shared
   !> `TEMP_RATIO` local to avoid repeating the `(T/5-3)` subexpression; this
   !> is a performance change only, with no effect on the computed values. If
   !> `NSMC(IEL)` exceeds the configured `max_no_snowmelt_slugs` capacity, the
   !> routine reports the overflow and calls `STOP`, unconditionally
   !> terminating the process; a comment in the source suggests replacing
   !> this with a graceful error flag instead.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1981-04 | JCB/EMM | - | Created the original snowmelt calculation. |
   !> | 1996-12-28 | RAH | 4.1 | Initialised `EFFDEP`, which was previously undefined. |
   !> | 2026-04-03 | SvB | 4.6.1 | Replaced the `1H0` Hollerith carriage-control descriptor in the memory-overflow `FORMAT` with a portable leading `/`. |
   !> | 2026-04-06 | SvB | 4.6.1 | Replaced `GOTO`-driven control flow with structured `IF`/`DO` blocks; added `IMPLICIT NONE` and explicit `INTENT(IN)` for `IEL`. |
   !> | 2026-04-07 | SvB | 4.6.1 | Pre-computed the repeated `ESAT`/`ESATA` temperature-ratio subexpression as `TEMP_RATIO`. |
   !> | 2026-04-13 | SvB | 4.6.1 | Replaced the `DLOG` double-precision-specific intrinsic with the generic `LOG`. |
   !> @endhistory
   SUBROUTINE SM (IEL)
      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IEL !! Element index for which snowpack and melt are updated.

      ! Locals
      INTEGER :: mr, ms, n, nnc, kkk, kl, kk, ncc
      DOUBLE PRECISION :: e, dn, rich, esat, po, q, esata, ea, qa, ts2, ee, tsm
      DOUBLE PRECISION :: hfc, hfr, hfe, hft
      DOUBLE PRECISION :: EFFDEP, TEMP_RATIO

   !----------------------------------------------------------------------*

      EFFDEP = 0.0D0

      MR = NRAINC(IEL)
      MS = NMC(IEL)
      N = NVC(IEL)

      ! CALCULATE NET PRECIPITATION FALLING ON SNOWPACK (MM OF SNOW)
      SF(IEL) = pnsnow / RHOS
      TOPNET = pnsnow
      pnsnow = zero
      pnet = zero
      USM = zero

      ! CALCULATE SNOWMELT AND SNOWDEPTH
      ! --------------------------------
      ! IF TEMPERATURE IS ABOVE FREEZING ANY PRECIPITATION
      ! IS RAINFALL AND IS ADDED TO THE SNOWMELT.
      ! OTHERWISE IT IS SNOW AND IS ADDED TO THE SNOWPACK.
      IF (LEZERO(TA(MS))) SD(IEL) = SD(IEL) + SF(IEL)

      ! CHOOSE DEGREE DAY OR ENERGY BUDGET
      IF (MSM == 1) THEN
         ! -----------------
         ! DEGREE DAY METHOD
         ! -----------------
         ! calculates melt rate directly. SPA, 05/11/92
         USM = ddf * (TA(MS) - two) * dtuz
         IF (TA(MS) < two) USM = zero
         ! set evaporation to zero
         e = 0.0d0
      ELSE
         ! --------------------
         ! ENERGY BUDGET METHOD
         ! --------------------
         ! CALCULATE HEAT GAINED BY CONVECTION
         ! EFFDEP (snowpack depth at anemometer site) removed from calculation of DN
         ! to prevent ln of 0 or negative no. SPA, 05/11/92.
         DN = ((0.4d0 / LOG((ZUS - ZDS) / ZOS))**2) * U(MS)

         ! CORRECT DN USING RICHARDSON NUMBER (SD - MM; ZUS,ZDS,ZOS - M)
         RICH = 9.81d0 * (ZUS - EFFDEP / 1000.0d0 - ZDS) * (TA(MS) - TS(IEL)) &
              / ((TA(MS) + 273.0d0) * U(MS) * U(MS))

         IF (TA(MS) > TS(IEL)) THEN
            DN = DN / (1.0d0 + 10.0d0 * RICH)
         ELSE
            DN = DN * (1.0d0 - 10.0d0 * RICH)
         END IF

         ! HEAT FLUX FROM CONVECTION IN TIME DTUZ (J/M^^2)
         HFC = RHOA * CPA * DN * (TA(MS) - TS(IEL)) * DTUZ

         ! HEAT FROM RAINFALL OR SNOWFALL (MM OF WATER) IN TIME DTUZ (J/M^^2)
         ! (NOTE THAT SF IS IN MM OF SNOW)
         ! IF TEMPERATURE IS ABOVE FREEZING, HEAT IS FROM RAIN
         ! ^^^^^^ REMOVED + LWI FROM END OF NEXT EQUATION
         HFR = CPW * TA(MS)
         ! IF TEMPERATURE IS BELOW FREEZING, HEAT IS FROM SNOW
         IF (LEZERO(TA(MS))) HFR = CPI * (TA(MS) - TS(IEL))
         HFR = RHOW * SF(IEL) * RHOS * HFR / 1000.0d0

         ! CALCULATE HEAT FROM WATER PHASE CHANGE
         ! High-Performance Fix: Pre-calculate the temperature ratio to avoid repeated division/subtraction
         TEMP_RATIO = (TS(IEL) / five) - three
         ESAT = (17.044d0 + TEMP_RATIO * (5.487d0 + TEMP_RATIO * (0.776d0 + TEMP_RATIO * (0.1063d0 + TEMP_RATIO * 0.003d0))))
         
         PO = 1012.0d0 * (one - 0.0065d0 * ZGRUND(IEL) / 288.0d0) * 100.0d0
         Q = (0.62197d0 * ESAT) / ((PO / 1.0045d0) - (0.37803d0 * ESAT))
         
         TEMP_RATIO = (TA(MS) / five) - three
         ESATA = (17.044d0 + TEMP_RATIO * (5.487d0 + TEMP_RATIO * (0.776d0 + TEMP_RATIO * (0.1063d0 + TEMP_RATIO * 0.003d0))))
         
         EA = ESATA - VPD(MS)
         QA = (0.62197d0 * EA) / ((PO / 1.0045d0) - (0.37803d0 * EA))
         
         ! MASS EVAPORATED (E) IN KG/S/M^^2
         E = RHOA * DN * (Q - QA)

         ! HEAT FROM PHASE CHANGE IN TIME DTUZ (J/M^^2)
         HFE = (LVW + LWI - CPI * TS(IEL)) * E * DTUZ

         ! TOTAL HEAT FLUX FROM AIR AND SOIL TO SNOW IN TIME DTUZ (J/M^^2)
         HFT = HFC + HFR - HFE + (HFG + RN(MS)) * DTUZ

         ! Fix incorporated to stop excessive energy fluxes in/out
         ! of thin snowpacks. SPA, 05/11/92.
         IF ((SD(IEL) <= 100.0d0) .AND. (LTZERO(HFT))) THEN
            HFT = (TA(MS) - TS(IEL)) * (CPI * RHOS * SD(IEL))
         END IF

         ! CALCULATE SNOWMELT USM (MM OF SNOW)
         ! -----------------------------------
         ! CALCULATE HYPOTHETICAL SNOW TEMPERATURE AS IF ALL HEAT
         ! INPUT CREATED TEMPERATURE RISE ONLY WITH NO PHASE CHANGE
         ! ( N.B. RHOS IS SPECIFIC GRAVITY AND SD IS IN MM. THEREFORE
         ! SNOWDEPTH IN METRES * SNOW DENSITY, WHICH IS REQUIRED IN
         ! THE FOLLOWING, IS (SD/1000)*(RHOS*1000) WHICH EQUALS SD*RHOS.)
         TS2 = (HFT / (CPI * RHOS * SD(IEL))) + TS(IEL)
         IF (TS2 < -50.0d0) TS2 = -50.0d0

         IF (LTZERO(TS2)) THEN
            ! SNOW TEMPERATURE < 0 SO NO MELTING
            USM = 0.0d0
         ELSE
            ! SNOW TEMPERATURE > 0 SO CALCULATE EXCESS HEAT AVAILABLE FOR MELTING SNOW
            HFT = HFT - ((-TS(IEL)) * CPI * RHOS * SD(IEL))
            TS2 = zero
            USM = HFT / (LWI * RHOS)
         END IF
         TS(IEL) = TS2
      END IF

      ! CALCULATE SNOWDEPTH SD (MM OF SNOW)
      EE = E
      ! EVAPORATION CHANGES DEPTH BUT CONDENSATION DOES NOT
      IF (LTZERO(E) .AND. ISZERO(TS(IEL))) E = zero
      ! SNOWMELT CHANGES DEPTH BUT FREEZING DOES NOT
      IF (LTZERO(USM)) USM = zero
      
      ! EVAPORATION LOSS ESM IN TIME DTUZ (MM OF SNOW)
      ESM = E * DTUZ / RHOS
      ! TOTAL LOSS FROM SNOWPACK TSM IN TIME DTUZ (MM OF SNOW)
      TSM = USM + ESM

      IF (TSM > SD(IEL)) THEN
         IF (ESM > SD(IEL)) THEN
            ESM = SD(IEL)
            USM = zero
         ELSE
            USM = SD(IEL) - ESM
         END IF
         SD(IEL) = zero
      ELSE
         SD(IEL) = SD(IEL) - TSM
      END IF

      ! ROUTE SNOWMELT THROUGH SNOWPACK
      ! -------------------------------
      ! SET COUNTERS
      NCC = 0
      ! NSMC IS NUMBER OF SLUGS OF MELTWATER STILL MOVING THROUGH SNOWPACK
      NSMC(IEL) = NSMC(IEL) + 1
      NNC = NSMC(IEL)
      
      ! Note: Consider replacing STOP with an ERROR flag to allow the host to shut down gracefully
      IF (NSMC(IEL) > max_no_snowmelt_slugs) THEN
         WRITE (6, 30) NSMC(IEL), IEL
         STOP
      END IF

      ! ADD ANY RAINFALL TO SNOWMELT AND CONVERT TOTAL TO MM OF WATER
      ! ^^^^^ TENTATIVELY: IF SNOW TEMPERATURE REMAINS <0, CHANGE RAIN TO
      !       SNOW AND ADD TO SNOWPACK DEPTH
      IF (LEZERO(TA(MS))) THEN
         SMELT(NNC, IEL) = USM * RHOS
      ELSE
         SMELT(NNC, IEL) = (USM + SF(IEL)) * RHOS
         SF(IEL) = zero
      END IF

      IF (GTZERO(SMELT(NNC, IEL))) THEN
         ! CALCULATE TIME TM AT WHICH MELTWATER SLUG WILL REACH
         ! BOTTOM OF SNOWPACK (TM - HR, SD - MM OF SNOW)
         tmelt(NNC, IEL) = (0.7448d0 * SD(IEL) / 1000.0d0 + 1.429d0) * SD(IEL) / 1000.0d0 + TIMEUZ
      ELSE
         NSMC(IEL) = NSMC(IEL) - 1
         NNC = NSMC(IEL)
      END IF

      ! CHECK IF THERE ARE ANY MELTWATER SLUGS STILL IN SNOWPACK
      IF (NSMC(IEL) > 0) THEN
         ! HAS CURRENT TIME TIMEUZ REACHED TIME AT WHICH
         ! MELTWATER SLUGS REACH BOTTOM OF SNOWPACK?
         DO KL = 1, NSMC(IEL)
            IF (TIMEUZ >= tmelt(KL, IEL)) THEN
               ! CALCULATE MELTWATER REACHING GROUND
               pnsnow = SMELT(KL, IEL) + pnsnow
               NCC = NCC + 1
            END IF
         END DO

         ! IF MELTWATER SLUG HAS REACHED BOTTOM OF SNOWPACK
         ! IN LATEST TIMESTEP, REMOVE THAT SLUG FROM STORE
         ! AND REPLACE WITH SUBSEQUENT SLUG. ADJUST ORDER
         ! OF ALL OTHER SLUGS ACCORDINGLY.
         IF (NCC > 0) THEN
            NSMC(IEL) = NSMC(IEL) - NCC
            KK = NSMC(IEL)
            
            ! Performance Reversion: Explicit DO loop is faster for micro-arrays
            ! than building F90 array-slice dope vectors.
            IF (KK > 0) THEN
               DO KL = 1, KK
                  KKK = KL + NCC
                  tmelt(KL, IEL) = tmelt(KKK, IEL)
                  SMELT(KL, IEL) = SMELT(KKK, IEL)
               END DO
            END IF
         END IF
      END IF

      ! CONVERT SF TO MM OF SNOW / HOUR
      SF(IEL) = (SF(IEL) / DTUZ) * 3600.0d0
      
      ! CONVERT pnsnow (mm) to PNET TO MM OF WATER / SEC
      pnet = pnsnow / dtuz
      
      IF (GTZERO(SD(IEL))) THEN
         ISPACK(IEL) = .TRUE.
      ELSE
         ISPACK(IEL) = .FALSE.
      END IF

      RETURN

      ! FORMAT STATEMENTS
30    FORMAT(/,'NO OF MELTWATER SLUGS IS', I5, ' AT ELEMENT', I4, &
             ' WHICH EXCEEDS AVAILABLE MEMORY STORE SIZE')

   END SUBROUTINE SM



   !> Applies evapotranspiration losses to an existing snowpack for one
   !> element.
   !>
   !> `SMET` carries out the snow-related evapotranspiration and interception
   !> control for an ET element when a snowpack exists, when precipitation is
   !> snow, or when air temperature is below freezing. It is called from
   !> [[smin]] for each upper-zone element and uses the same shared ET and
   !> snow variables as [[sm]] and the [[etmod]] routines.
   !>
   !> The routine reduces snowpack depth for evaporation or sublimation,
   !> updates snow temperature in the energy-budget case through [[sm]], and
   !> identifies any excess rainfall or evaporation demand that must still be
   !> handled by the normal ET calculation.
   !>
   !> Its main branch logic is:
   !>
   !> | Condition | Action |
   !> |:----------|:-------|
   !> | No snowpack (`SNDEP=0`). | Falls through to the shared handling described below. |
   !> | Snow depth at or above vegetation height. | Snow covers the vegetation, so canopy leaf area `CPLAI` is set to zero (no interception). |
   !> | Snow depth below vegetation height (nonzero). | Canopy leaf area `CPLAI` is reduced in proportion to exposed vegetation height: `CPLAI = CPLAI * (VHT - SNDEP) / VHT`; if `TA > 0` the routine sets `NSMT = 1` and returns immediately so normal ET/interception handles the timestep. |
   !>
   !> Unless the temperature-above-freezing return above is taken, the shared
   !> tail handles snowfall reaching the ground or snowpack without canopy
   !> storage delay: `ERZ`, `ESOIL`, `EINT`, `AE`, `PE`, and `S(1:NRD(N))` are
   !> zeroed, `CSTOLD` is set from `CSTORE`, and
   !>
   !> \[
   !> PNSNOW = precip\_m\_per\_s(IEL)\,1000\,DTUZ
   !> \]
   !>
   !> converts the current precipitation rate from m/s to a millimetre water
   !> depth over the timestep. [[sm]] is called if there is precipitation or
   !> an existing snowpack.
   !>
   !> When `NSD=1`, the active snow specific gravity is taken from
   !> `RHOSAR(IEL)`; if that value is zero, the default `RHODEF` is used.
   !>
   !> @note
   !> [[etmod:etin]] documents cross-module consequences of `SMET`'s state:
   !> `DRAIN` is never assigned here, `PNET` can be left unchanged from a
   !> previous element when neither a snowpack nor precipitation triggers
   !> [[sm]], and the `S(1:NRD)` zeroing here does not align with the
   !> surface-indexed cells that [[et]] writes root sinks to.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 2026-04-06 | SvB | 4.6.1 | Replaced `GOTO`-driven control flow with structured `IF`/`ELSE IF` blocks; added `IMPLICIT NONE` and explicit `INTENT(IN)` for `IEL`. |
   !> @endhistory
   SUBROUTINE SMET (IEL)
      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: iel !! Element index for which snow-affected ET/interception is processed.

      ! Locals
      INTEGER :: ms, mr, n, k, kk
      DOUBLE PRECISION :: sndep

      !----------------------------------------------------------------------*

      MS = NMC(IEL)
      MR = NRAINC(IEL)
      N = NVC(IEL)

      ! USE SPATIALLY VARIABLE RHOS (OR DEFAULT IF ZERO)
      IF (NSD == 1) RHOS = RHOSAR(IEL)
      IF (ISZERO(RHOS)) RHOS = RHODEF

      ! IS THE SNOWDEPTH GREATER THAN THE VEGETATION HEIGHT?
      SNDEP = SD(IEL) / 1000.0d0

      IF (ISZERO(SNDEP)) THEN
         ! No snowpack exists. Proceed to generic freezing/precipitation checks.
         CONTINUE
         
      ELSE IF (SNDEP >= VHT(N)) THEN
         ! SNOW COVERS THE VEGETATION SO THERE IS NO CANOPY INTERCEPTION,
         ! NO EVAPOTRANSPIRATION AND NO SOIL EVAPORATION
         CPLAI = zero
         
      ELSE
         ! 0 < SNDEP < VHT(N): Snow partially covers the vegetation.
         CPLAI = CPLAI * (VHT(N) - SNDEP) / VHT(N)
         
         ! IS THE TEMPERATURE ABOVE FREEZING?
         IF (GTZERO(TA(MS))) THEN
            ! INTERCEPTION CALCULATIONS FOR TEMPERATURES ABOVE FREEZING
            ! ---------------------------------------------------------
            ! THERE IS EVAPOTRANSPIRATION AND INTERCEPTION (OF RAINFALL)
            ! WHICH MUST BE MODELLED BY SUBROUTINE ET.
            ! IT IS ASSUMED THAT THERE IS NO CANOPY STORAGE OF SNOW TO BE 
            ! MODELLED. IF THERE IS A SNOWPACK THERE IS NO SOIL EVAPORATION.
            NSMT = 1
            RETURN
         END IF
      END IF

      ! INTERCEPTION CALCULATIONS FOR TEMPERATURES BELOW FREEZING (OR SNOW-COVERED VEG)
      ! ---------------------------------------------------------
      ! THERE IS NO EVAPOTRANSPIRATION AND NO SOIL EVAPORATION.
      ! PRECIPITATION FALLING ON THE CANOPY IS ASSUMED TO PASS
      ! WITHOUT DELAY THROUGH THE VEGETATION LAYER. IE THERE
      ! IS NO INTERCEPTION OR CANOPY STORAGE OF SNOW.
      
      ! SNOWFALL (IN MM OF WATER) REACHING GROUND OR SNOWPACK
      pnsnow = precip_m_per_s(IEL) * 1000.0d0 * DTUZ
      CSTOLD = CSTORE(IEL)
      ERZ = zero
      ESOIL = zero
      EINT = zero
      AE = zero
      PE = zero
      K = NRD(N)
      
      DO KK = 1, K
         S(KK) = zero
      END DO

      ! IS IT SNOWING OR IS THERE A SNOWPACK?
      IF (GTZERO(precip_m_per_s(IEL)) .OR. GTZERO(SD(IEL))) THEN
         ! CALL SNOWMELT ROUTINE
         CALL SM(IEL)
      END IF

      RETURN
   END SUBROUTINE SMET



   !> Snow wrapper called from ET/interception processing for one element.
   !>
   !> `SMIN` decides whether snow processing is needed, converts the current
   !> net precipitation to snowpack input, calls [[sm]] when snowfall or
   !> snowpack is present, or calls [[smet]] when only snowpack
   !> evaporation/sublimation is required. It is called from [[etmod:etin]].
   !>
   !> | State | Action |
   !> |:------|:-------|
   !> | `NSMT /= 1`, snowpack exists, or `TA <= 0` | Call [[smet]] so snow/freezing-temperature ET and interception logic runs. |
   !> | `NSMT /= 1`, no snowpack, and `TA > 0` | Set `NSMT=1` and return so normal ET/interception can proceed. |
   !> | `NSMT == 1` and no snowpack remains | Return; normal ET has already handled the timestep. |
   !> | `NSMT == 1` and snowpack remains | Suppress soil evaporation, convert the ET-produced `PNET` rate to a depth with `PNSNOW=PNET*DTUZ`, and call [[sm]]. |
   !>
   !> The routine calls [[initialise_smmod]] on every invocation; that
   !> routine's own one-time guard makes the repeated call harmless after the
   !> first.
   !>
   !> @note
   !> This routine has no result-affecting side effects beyond those
   !> described above and in [[sm]]/[[smet]]; [[etmod:etin]] documents further
   !> cross-module caveats about the state `SMIN`'s callees leave behind.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 2026-04-06 | SvB | 4.6.1 | Replaced `GOTO`-driven control flow with structured `IF`/`ELSE` blocks; added `IMPLICIT NONE` and explicit `INTENT(IN)` for `IEL`. |
   !> @endhistory
   SUBROUTINE SMIN (IEL)
      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: iel !! Element index for which snow processing is dispatched.

      ! Locals
      INTEGER :: ms

      !----------------------------------------------------------------------*

      CALL INITIALISE_SMMOD()
      
      MS = NMC(IEL)

      ! IF ET CALCULATIONS HAVE ALREADY BEEN CARRIED OUT AND
      ! TEMPERATURE IS ABOVE FREEZING (REQUIRING THE CONDITION NSMT = 1)
      IF (NSMT == 1) THEN
         
         ! SNOWMELT CALCULATION IS REQUIRED IF A SNOWPACK EXISTS.
         ! (THE FOLLOWING CAN BE REACHED ONLY IF TEMPERATURE IS ABOVE FREEZING)
         IF (GTZERO(SD(IEL))) THEN
            
            ! THERE IS STILL A SNOWPACK SO THERE IS NO SOIL EVAPORATION
            ESOIL = zero
            
            ! addition by spa, 17/11/92. pnet output from et(iel) as a rate.
            ! Needs to be a depth for input into sm(iel).
            pnsnow = pnet * dtuz
            
            ! CALL SNOWMELT ROUTINE
            CALL SM(IEL)
            
         END IF
         
      ELSE
         
         ! IF ET CALCULATIONS HAVE NOT YET BEEN CARRIED OUT,
         ! IS THERE A SNOWPACK OR IS TEMPERATURE BELOW FREEZING?
         IF (GTZERO(SD(IEL)) .OR. LEZERO(TA(MS))) THEN
            
            ! CALL ET ROUTINE FOR SNOW/FREEZING TEMPERATURES
            CALL SMET(IEL)
            
         ELSE
            
            ! NO SNOWPACK EXISTS AND TEMPERATURE IS ABOVE FREEZING
            NSMT = 1
            
         END IF
         
      END IF

      RETURN
   END SUBROUTINE SMIN
   
END MODULE SMmod
