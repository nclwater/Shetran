MODULE snowmelt_calculation
! Core snowmelt calculation routines extracted from SMmod.f90
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the SM.F files
! Refactor 2025: Extracted core snowmelt calculations into separate module

   USE SGLOBAL
   USE AL_C, ONLY : nvc, dtuz, ispack, nrd
   USE AL_D, ONLY : AE, CSTOLD, CSTORE, CPLAI, ERZ, ESOIL, EINT, &
      msm, nsmc, nrainc, nmc, nsmt, precip_m_per_s, pnet, PE, RHOSAR, rn, s, sf, sd, ta, ts, &
      timeuz, u, vpd, VHT
   USE snow_constants
   USE snow_variables
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: SM

CONTAINS

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

      IF (MSM.NE.2) THEN
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
      ELSE
!
!         --------------------
!         ENERGY BUDGET METHOD
!         --------------------
!
!         CALCULATE HEAT GAINED BY CONVECTION
!
!         EFFDEP (snowpack depth at anemometer site) removed from calcu-
!         lation of DN to prevent ln of 0 or negative no. SPA, 05/11/92.
         DN = ( (0.4 / DLOG ( (ZUS - ZDS) / ZOS) ) **2) * U (MS)
!
!         CORRECT DN USING RICHARDSON NUMBER (SD - MM; ZUS,ZDS,ZOS - M)
!
         RICH = 9.81 * (ZUS - EFFDEP / 1000.0d0 - ZDS) * (TA (MS) - TS (IEL) ) &
            / ( (TA (MS) + 273.0d0) * U (MS) * U (MS) )
         IF (TA (MS) .GT.TS (IEL) ) THEN
            DN = DN / (1 + 10.0d0 * RICH)
         ELSE
            DN = DN * (1 - 10.0d0 * RICH)
         ENDIF
!
!         HEAT FLUX FROM CONVECTION IN TIME DTUZ (J/M^^2)
         HFC = RHOA * CPA * DN * (TA (MS) - TS (IEL) ) * DTUZ
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
         IF (LTZERO(TS2)) THEN
!
!         SNOW TEMPERATURE < 0 SO NO MELTING
            USM = 0.0
            TS (IEL) = TS2
         ELSE
!
!         SNOW TEMPERATURE > 0 SO CALCULATE EXCESS HEAT
!         AVAILABLE FOR MELTING SNOW
            HFT = HFT - ( ( - TS (IEL) ) * CPI * RHOS * SD (IEL) )
            TS2 = zero
            USM = HFT / (LWI * RHOS)
            TS (IEL) = TS2
         ENDIF
      ENDIF
!
!         CALCULATE SNOWDEPTH SD (MM OF SNOW)
!
      EE = E
!         EVAPORATION CHANGES DEPTH BUT CONDENSATION DOES NOT
      IF (LTZERO(E) .AND. ISZERO(TS (IEL))) E = zero
!         SNOWMELT CHANGES DEPTH BUT FREEZING DOES NOT
      IF (LTZERO(USM)) USM = zero
!         EVAPORATION LOSS ESM IN TIME DTUZ (MM OF SNOW)
      ESM = E * DTUZ / RHOS
!         TOTAL LOSS FROM SNOWPACK TSM IN TIME DTUZ (MM OF SNOW)
      TSM = USM + ESM
      IF (TSM.GT.SD (IEL) ) THEN
         IF (ESM.GT.SD (IEL) ) THEN
            ESM = SD (IEL)
            USM = zero
         ELSE
            SD (IEL) = SD (IEL) - ESM
            USM = SD (IEL)
         ENDIF
         SD (IEL) = zero
      ELSE
         SD (IEL) = SD (IEL) - TSM
      ENDIF
!
!         ROUTE SNOWMELT THROUGH SNOWPACK
!         -------------------------------
!         SET COUNTERS
      NCC = 0
!          NSMC IS NUMBER OF SLUGS OF MELTWATER STILL
!          MOVING THROUGH SNOWPACK
      NSMC (IEL) = NSMC (IEL) + 1
      NNC = NSMC (IEL)
      IF (NSMC (IEL) .GT.max_no_snowmelt_slugs) THEN
         WRITE (6, 30) NSMC (IEL), IEL
30       FORMAT(1H0,'NO OF MELTWATER SLUGS IS',I5,' AT ELEMENT', &
         & I4,' WHICH EXCEEDS AVAILABLE MEMORY STORE SIZE')
         STOP
      ENDIF
!
!          ADD ANY RAINFALL TO SNOWMELT AND CONVERT TOTAL TO MM OF WATER
!  ^^^^^  TENTATIVELY: IF SNOW TEMPERATURE REMAINS <0, CHANGE RAIN TO
!         SNOW AND ADD TO SNOWPACK DEPTH
      IF (LEZERO(TA(MS))) THEN
         SMELT (NNC, IEL) = USM * RHOS
      ELSE
         SMELT (NNC, IEL) = (USM + SF (IEL) ) * RHOS
         SF (IEL) = zero
      ENDIF
      IF (LEZERO(SMELT(NNC,IEL))) THEN
         NSMC (IEL) = NSMC (IEL) - 1
         NNC = NSMC (IEL)
      ELSE
!
!         CALCULATE TIME TM AT WHICH MELTWATER SLUG WILL REACH
!         BOTTOM OF SNOWPACK (TM - HR, SD - MM OF SNOW)
         tmelt (NNC, IEL) = (0.7448 * SD (IEL) / 1000. + 1.429) * SD (IEL) &
            / 1000. + TIMEUZ
      ENDIF
!         HAS CURRENT TIME TIMEUZ REACHED TIME AT WHICH
!         MELTWATER SLUGS REACH BOTTOM OF SNOWPACK?
      DO
         IF (NSMC (IEL) .LE.0) EXIT
         NCC = 0
         DO KL = 1, NNC
            IF (TIMEUZ.GE.tmelt(KL, IEL) ) THEN
!         CALCULATE MELTWATER REACHING GROUND
               pnsnow = SMELT (KL, IEL) + pnsnow
               NCC = NCC + 1
            ENDIF
         END DO
!         IF MELTWATER SLUG HAS REACHED BOTTOM OF SNOWPACK
!         IN LATEST TIMESTEP, REMOVE THAT SLUG FROM STORE
!         AND REPLACE WITH SUBSEQUENT SLUG. ADJUST ORDER
!         OF ALL OTHER SLUGS ACCORDINGLY.
         IF (NCC.EQ.0) EXIT
         NSMC (IEL) = NSMC (IEL) - NCC
         KK = NSMC (IEL)
!         IF NSMC = 0 THERE ARE NO MELTWATER SLUGS IN SNOWPACK
         IF (KK.EQ.0) EXIT
         DO KL = 1, KK
            KKK = KL + NCC
            tmelt(KL, IEL) = tmelt(KKK, IEL)
            SMELT (KL, IEL) = SMELT (KKK, IEL)
         END DO
      ENDDO
!         CONVERT SF TO MM OF SNOW / HOUR
      SF (IEL) = (SF (IEL) / DTUZ) * 3600.
!         CONVERT pnsnow (mm) to PNET TO MM OF WATER / SEC

      pnet = pnsnow / dtuz
      IF (GTZERO(SD(IEL))) THEN
         ISPACK (IEL) = .TRUE.
      ELSE
         ISPACK (IEL) = .FALSE.

      ENDIF
      RETURN
   END SUBROUTINE SM

END MODULE snowmelt_calculation
