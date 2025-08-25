MODULE snow_constants
! Snow physical constants and parameters extracted from SMmod.f90
! JE  12/08   4.3.5F90  Originally from SPEC_SM module in SMmod
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
! Refactor 2025: Extracted constants into separate module
!----------------------------------------------------------------------*

   IMPLICIT NONE

   ! Physical constants for snow calculations
   DOUBLEPRECISION, PARAMETER :: RHOA = 1.29d0, &      ! Density of air (kg/m^3)
      RHOW = 1000.0d0, &                               ! Density of water (kg/m^3)
      CPA = 1003.0d0, &                                ! Specific heat of air at constant pressure (J/kg/C)
      CPW = 4187.0d0, &                                ! Specific heat of water (J/kg/C)
      CPI = 2093.0d0, &                                ! Specific heat of ice (J/kg/C)
      LWI = 334000.0d0, &                              ! Latent heat of fusion (J/kg)
      LVW = 2500000.0d0, &                             ! Latent heat of vaporisation (J/kg)
      HFG = 2.0d0                                      ! Heat flux from ground (W/m^2 = J/S/m^2)

   ! Public access to all constants
   PUBLIC :: RHOA, RHOW, CPA, CPW, CPI, LWI, LVW, HFG

END MODULE snow_constants
