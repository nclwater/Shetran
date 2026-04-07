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
   !        SNOWMELT CALCULATIONS
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
   !             MSM=1 -- DEGREE DAY
   !             MSM=2 -- ENERGY BUDGET
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
   !     USM   - SNOWMELT (MM OF SNOW) DURING TIME DTUZ       MM
   !     SMELT - SNOWMELT AND THROUGHFALL (MM OF WATER)       MM
   !     SD    - SNOWDEPTH (MM OF SNOW)                       MM
   !     DDF   - DEGREE DAY FACTOR                            MM/S/C
   !     RHOS  - SPECIFIC GRAVITY OF SNOW                     --
   !     TM    - TIME IN SIMULATION AT WHICH SLUG OF
   !               MELTWATER REACHES BOTTOM OF SNOWPACK       HR
   !     pnsnow  - INITIALLY THE NET PRECIPITATION OR
   !               THROUGHFALL (MM OF WATER) TO THE TOP
   !               OF THE SNOWPACK. FINALLY THE MELTWATER
   !               FROM THE BOTTOM OF THE SNOWPACK TO THE
   !               GROUND (MM OF WATER).                      MM
   !     SF    - NET SNOWFALL (MM OF SNOW) TO THE TOP OF
   !               THE SNOWPACK                               MM
   !                                                          MM/HR
   !     NSMC  - COUNTER USED IN ROUTING MELTWATER
   !               THROUGH SNOWPACK. EQUALS NUMBER OF
   !               SLUGS OF MELTWATER MOVING THROUGH SNOWPACK --
   !     MSM   - EQUALS 1 FOR DEGREE DAY
   !                    2 FOR ENERGY BUDGET                   --
   !
   !     ESM   - SNOW LOST (MM OF SNOW) TO EVAPORATION OR
   !               SUBLIMATION                                MM
   !     TSM   - TOTAL SNOW LOST (MM OF SNOW) FROM PACK       MM
   !     HFC   - HEAT FLUX FROM ATMOSPHERIC CONVECTION        J/M^^2
   !     HFE   - HEAT FROM PHASE CHANGES (EVAP. OR CONDEN.)   J/M^^2
   !     HFR   - HEAT FROM RAINFALL                           J/M^^2
   !     HFT   - TOTAL HEAT FLUX FROM AIR AND SOIL TO SNOW    J/M**2
   !     TS    - SNOW TEMPERATURE                             C
   !
   !     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      ! Assumed external module dependencies providing global variables:
      ! NRAINC, NMC, NVC, SF, pnsnow, RHOS, TOPNET, zero, pnet, USM, TA, SD,
      ! MSM, ddf, two, dtuz, ZUS, ZDS, ZOS, U, TS, RHOA, CPA, CPW, CPI, RHOW,
      ! five, three, ZGRUND, VPD, LVW, LWI, HFG, RN, ESM, NSMC, max_no_snowmelt_slugs,
      ! SMELT, tmelt, TIMEUZ, ISPACK, LEZERO, LTZERO, ISZERO, GTZERO

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IEL
      
      ! Locals
      INTEGER :: mr, ms, n, nnc, kl, kk, ncc
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
         DN = ((0.4d0 / DLOG((ZUS - ZDS) / ZOS))**2) * U(MS)

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
            
            ! High-Performance Fix: Utilize native Fortran array slicing for instant memory shift
            IF (KK > 0) THEN
               tmelt(1:KK, IEL) = tmelt(1+NCC:KK+NCC, IEL)
               SMELT(1:KK, IEL) = SMELT(1+NCC:KK+NCC, IEL)
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



   !SSSSSS SUBROUTINE SMET
   SUBROUTINE SMET (IEL)
   !----------------------------------------------------------------------*
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
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! NMC, NRAINC, NVC, NSD, RHOS, RHOSAR, ISZERO, RHODEF, SD, VHT, pnsnow, 
      ! precip_m_per_s, DTUZ, CSTOLD, CSTORE, CPLAI, zero, ERZ, ESOIL, EINT, 
      ! AE, PE, NRD, S, SM, TA, GTZERO, NSMT

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: iel
      
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



   !SSSSSS SUBROUTINE SMIN
   SUBROUTINE SMIN (IEL)
   !----------------------------------------------------------------------*
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
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! NMC, NSMT, SD, TA, ESOIL, zero, pnsnow, pnet, dtuz, 
      ! INITIALISE_SMMOD, SMET, SM, GTZERO, LEZERO

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: iel
      
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
