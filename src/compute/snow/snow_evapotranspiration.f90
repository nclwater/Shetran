MODULE snow_evapotranspiration
! Snow evapotranspiration calculations extracted from SMmod.f90
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactor 2025: Extracted evapotranspiration handling into separate module

   USE SGLOBAL
   USE AL_C, ONLY : nvc, dtuz, ispack, nrd
   USE AL_D, ONLY : AE, CSTOLD, CSTORE, CPLAI, ERZ, ESOIL, EINT, &
      msm, nsmc, nrainc, nmc, nsmt, precip_m_per_s, pnet, PE, RHOSAR, rn, s, sf, sd, ta, ts, &
      timeuz, u, vpd, VHT
   USE snow_variables, ONLY : RHOS, RHODEF, PNSNOW, NSD
   USE snowmelt_calculation, ONLY : SM
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: SMET

CONTAINS

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
      IF (ISZERO(SNDEP)) THEN
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
         pnsnow = precip_m_per_s(IEL) * 1000. * DTUZ
         CSTOLD = CSTORE (IEL)
         ERZ = zero
         ESOIL = zero
         EINT = zero
         AE = zero
         PE = zero
         K = NRD (N)
         DO KK = 1, K
            S (KK) = zero
         END DO
      ELSEIF (SNDEP.LT.VHT (N) ) THEN
         CPLAI = CPLAI * (VHT (N) - SD (IEL) / 1000.) / VHT (N)
      ELSE
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
         DO KK = 1, K
            S (KK) = zero
         END DO
      ENDIF
!
!         IS THE TEMPERATURE ABOVE FREEZING?
      IF (GTZERO(TA(MS))) THEN
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
         NSMT = 1
      ELSE
!
!         IS IT SNOWING OR IS THERE A SNOWPACK?
         IF (GTZERO(precip_m_per_s(IEL)) .OR. GTZERO(SD(IEL))) THEN
!
!         CALL SNOWMELT ROUTINE
            CALL SM (IEL)
         ENDIF
      ENDIF
   END SUBROUTINE SMET

END MODULE snow_evapotranspiration
