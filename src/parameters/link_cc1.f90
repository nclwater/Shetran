!> summary: Defines contaminant transport variables for the LINK subroutine.
!> author: J. Ewen
!> date: 2008-08-12
!>
!> This module contains variables related to contaminant transport calculations,
!> specifically scaled lengths and thicknesses used within the LINK subroutine.
!> It was converted from a legacy `INCLUDE` file.
!>
!> @history
!> | Date       | Author | Version | Description                                             |
!> |:-----------|:-------|:--------|:--------------------------------------------------------|
!> | 1991-05-18 | JE     | 3.1     | Original `INCLUDE` file written.                        |
!> | 1991-06-13 | JE     | 3.1     | Completed.                                              |
!> | 1993-02-08 | GP     | 3.4     | Moved variables to new module.                          |
!> | 2008-08-12 | JE     | 4.3.5F90| Converted to Fortran 90 module.                         |
!> | 2025-08-11 | AI     | -       | Added FORD-compliant documentation and kind parameters. |
MODULE LINK_CC1

   USE SGLOBAL, ONLY : LLEE
   USE MOD_PARAMETERS, ONLY: R8P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: KS, KSPBK

   ! Code =====================================================================

   REAL(KIND=R8P) :: KS          !! Scaled length.
   REAL(KIND=R8P) :: KSPBK(2, LLEE) !! Scaled lengths and thicknesses.

   ! The legacy COMMON block /SIZE/ was replaced by these module variables.

END MODULE LINK_CC1
