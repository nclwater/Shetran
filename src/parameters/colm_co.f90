!> summary: Defines initial state variables for soil column water flow calculations.
!> author: J. Ewen, GP, R.A.H.
!> date: 2008-12-01
!>
!> This module provides variables used to store the initial or "original" state
!> of water-related quantities before running the `COLM` subroutine. These variables
!> are used for setup and are not modified within `COLM` itself. It was converted
!> from a legacy Fortran 77 INCLUDE file.
!>
!> @history
!> | Date       | Author | Version  | Description                                       |
!> |:-----------|:-------|:---------|:--------------------------------------------------|
!> | 1991-04-26 | JE     | 3.1      | Original `INCLUDE` file written.                  |
!> | 1991-06-16 | JE     | 3.1      | Completed.                                        |
!> | 1994-08-08 | GP     | 4.0      | Replaced TH3O with VSTHEO.                        |
!> | 1997-02-20 | RAH    | 4.1      | Explicit typing.                                  |
!> | 1998-03-08 | RAH    | 4.2      | Removed WELDRO.                                   |
!> | 1998-11-03 | RAH    | 4.2      | Removed ERUZO.                                    |
!> | 2008-12-01 | JE     | 4.3.5F90 | Converted to Fortran 90.                          |
!> | 2025-08-11 | AI     | -        | Added KIND parameters and FORD docs.              |
MODULE COLM_CO

   USE SGLOBAL, ONLY : NELEE, LLEE
   USE MOD_PARAMETERS, ONLY: R8P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: DSWO, GGAMMO, QIO, QQO, QQRFO, QQQSWO, RSZWLO, UUAJPO, ZONEO, VSTHEO

   ! Code =====================================================================

   ! The 'O' suffix on these variables likely stands for 'Original', representing
   ! the state at the beginning of a time step or simulation phase.
   REAL(KIND=R8P) :: DSWO(NELEE)          !! Original depth of surface water (m)
   REAL(KIND=R8P) :: GGAMMO(NELEE, LLEE)  !! Original soil water retention parameter
   REAL(KIND=R8P) :: QIO(NELEE)           !! Original inflow rate (m/s)
   REAL(KIND=R8P) :: QQO(NELEE, LLEE, 4)  !! Original inter-cell water fluxes (m/s)
   REAL(KIND=R8P) :: QQRFO(NELEE)         !! Original flow into the bottom cell (m/s)
   REAL(KIND=R8P) :: QQQSWO(NELEE, 4)     !! Original lateral surface water flow rates (m3/s)
   REAL(KIND=R8P) :: RSZWLO(NELEE)        !! Original value related to well/water level
   REAL(KIND=R8P) :: UUAJPO(NELEE, LLEE)  !! Original adjusted vertical water flux (m/s)
   REAL(KIND=R8P) :: ZONEO(NELEE)         !! Original soil zone parameter
   REAL(KIND=R8P) :: VSTHEO(NELEE, LLEE)  !! Original volumetric soil moisture content

END MODULE COLM_CO
