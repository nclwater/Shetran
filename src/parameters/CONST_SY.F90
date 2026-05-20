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
!------------------- Start of CONST.SY   ------------------------------*
!
!   Distributed constants for the SY component.
!
!----------------------------------------------------------------------*
! Version:  3.4.1      Notes:  SSR29
!  Module:  SY       Program:  SHETRAN
! Modifications:
!  RAH  01.06.94  Version 3.4.1 by AB/RAH. File created 05.10.93.
!  JE  NOV 04 ---- Convert to FORTRAN 95
!----------------------------------------------------------------------*
!
   IMPLICIT NONE
   DOUBLEPRECISION, PARAMETER :: GRAVTY = 9.80665d0, RHOSED = 2650.0d0, RHOWAT = 998.0d0  , VISCOS = 1.0D-6
END MODULE const_sy
