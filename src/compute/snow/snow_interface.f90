MODULE snow_interface
! Main snow model interface extracted from SMmod.f90
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactor 2025: Extracted main interface into separate module

   USE SGLOBAL
   USE AL_C, ONLY : dtuz
   USE AL_D, ONLY : ESOIL, nsmt, pnet, sd, ta, nmc
   USE snow_variables, ONLY : PNSNOW
   USE snow_initialization, ONLY : initialise_smmod
   USE snow_evapotranspiration, ONLY : SMET
   USE snowmelt_calculation, ONLY : SM
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: SMIN

CONTAINS

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
      IF (NSMT.NE.1) THEN
!
!         IF ET CALCULATIONS HAVE NOT YET BEEN CARRIED OUT, IS
!         THERE A SNOWPACK?
         IF (GTZERO(SD(IEL))) THEN
!
!         CALL ET ROUTINE FOR SNOW/FREEZING TEMPERATURES
            CALL SMET (IEL)
         ELSE
!
!         IF ET CALCULATIONS HAVE NOT YET BEEN CARRIED OUT,IS
!         TEMPERATURE ABOVE FREEZING?
            IF (LEZERO(TA(MS))) THEN
!
!         CALL ET ROUTINE FOR SNOW/FREEZING TEMPERATURES
               CALL SMET (IEL)
            ELSE
               NSMT = 1
            ENDIF
         ENDIF
      ELSE
!
!         IF ET CALCULATIONS HAVE ALREADY BEEN CARRIED OUT,
!         SNOWMELT CALCULATION IS REQUIRED IF A SNOWPACK EXISTS.
!         (THE FOLLOWING CAN BE REACHED ONLY IF TEMPERATURE
!         IS ABOVE FREEZING)
         IF (GTZERO(SD(IEL))) THEN
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
         ENDIF
      ENDIF
   END SUBROUTINE SMIN

END MODULE snow_interface
