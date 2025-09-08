!> summary: Defines distributed physical constants for the SY component.
!> author: R.A.H., J. Ewen
!> date: 2004-11-01
!>
!> This module provides fundamental physical constants used throughout the sediment transport (SY) component of Shetran.
!>
!> @history
!> | Date       | Author | Version | Description                          |
!> |:-----------|:-------|:--------|:------------------------------------ |
!> | 1993-10-05 | AB/RAH | -       | File created.                        |
!> | 1994-06-01 | RAH    | 3.4.1   | Version update.                      |
!> | 2004-11-01 | JE     | -       | Converted to Fortran 95.             |
!> | 2025-08-11 | AI     | -       | Added KIND parameters and FORD docs. |
MODULE const_sy

   USE MOD_PARAMETERS, ONLY: R8P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public parameters
   PUBLIC :: GRAVTY, RHOSED, RHOWAT, VISCOS

   ! Code =====================================================================

   REAL(KIND=R8P), PARAMETER :: GRAVTY = 9.80665_R8P  !! Acceleration due to gravity (m/s2)
   REAL(KIND=R8P), PARAMETER :: RHOSED = 2650.0_R8P   !! Density of sediment (kg/m3)
   REAL(KIND=R8P), PARAMETER :: RHOWAT = 998.0_R8P    !! Density of water (kg/m3)
   REAL(KIND=R8P), PARAMETER :: VISCOS = 1.0E-6_R8P   !! Kinematic viscosity of water (m2/s)

END MODULE const_sy
