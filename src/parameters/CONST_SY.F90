!> summary: Physical constants used by the sediment yield component.
!>
!> Provides fixed gravitational acceleration, sediment-particle and water
!> densities, and water kinematic viscosity. All four constants support sediment
!> transport, entrainment, critical-shear-stress, and settling-velocity
!> calculations in [[symod]]. [[frmod]] and [[visualisation_interface_left]] also
!> use `RHOSED` to convert sediment-volume quantities to mass-based outputs.
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
   DOUBLEPRECISION, PARAMETER :: RHOSED = 2650.0d0  !! Representative sediment-particle density in kilograms per cubic metre.
   DOUBLEPRECISION, PARAMETER :: RHOWAT = 998.0d0   !! Representative water density in kilograms per cubic metre.
   DOUBLEPRECISION, PARAMETER :: VISCOS = 1.0D-6    !! Representative water kinematic viscosity in square metres per second.
END MODULE const_sy
