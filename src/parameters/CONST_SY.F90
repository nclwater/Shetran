!> summary: Physical constants used by the sediment yield component.
!> author: RAH, Newcastle University; JE, Newcastle University
!>
!> This module stores distributed constants used by the SHETRAN sediment yield
!> (`SY`) component. The constants define gravitational acceleration, sediment
!> and water density, and water kinematic viscosity for sediment transport and
!> erosion calculations.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-10-05 | AB/RAH | - | Original file created. |
!> | 1994-06-01 | RAH | 3.4.1 | Updated for SHETRAN version 3.4.1. |
!> | 2004-11 | JE | - | Converted to Fortran 95. |
!> @endhistory
MODULE const_sy
IMPLICIT NONE
DOUBLEPRECISION, PARAMETER :: GRAVTY = 9.80665d0 !! Gravitational acceleration in metres per second squared.
DOUBLEPRECISION, PARAMETER :: RHOSED = 2650.0d0  !! Representative sediment particle density in kilograms per cubic metre.
DOUBLEPRECISION, PARAMETER :: RHOWAT = 998.0d0   !! Representative water density in kilograms per cubic metre.
DOUBLEPRECISION, PARAMETER :: VISCOS = 1.0D-6    !! Representative water kinematic viscosity in square metres per second.
END MODULE const_sy
