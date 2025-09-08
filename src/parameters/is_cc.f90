!> summary: Defines logical flags for the contaminant component.
!> author: J. Ewen, GP, R.A.H., J. Ewen
!> date: 2008-12-01
!>
!> This module provides a set of logical variables that act as switches to control
!> various features and boundary conditions within the contaminant transport calculations.
!> It was converted from a legacy Fortran 77 INCLUDE file.
!>
!> @history
!> | Date       | Author | Version  | Description                               |
!> |:-----------|:-------|:---------|:------------------------------------------|
!> | 1991-05-01 | JE     | 3.0      | Original `INCLUDE` file written.          |
!> | 1991-06-13 | JE     | 3.1      | Checked, no changes.                      |
!> | unknown    | GP     | 3.4      | Added ISPLT. Renamed LGIC as LGIC4.       |
!> | 1997-02-21 | RAH    | 4.1      | Amended comment.                          |
!> | 1998-03-08 | RAH    | 4.2      | Amended history.                          |
!> | 2008-12-01 | JE     | 4.3.5F90 | Converted to Fortran 90.                  |
!> | 2025-08-11 | AI     | -        | Modernized documentation and formatting.  |
MODULE IS_CC

   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: ISADNL, ISBK, ISFLXB, ISPLT

   ! Code =====================================================================

   LOGICAL :: ISADNL !! If true, enables advection in non-linear flow.
   LOGICAL :: ISBK   !! If true, enables bank erosion.
   LOGICAL :: ISFLXB !! If true, a flux boundary condition is applied.
   LOGICAL :: ISPLT  !! If true, enables plant uptake calculations.

END MODULE IS_CC
