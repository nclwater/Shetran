!> summary: Defines grid-related variables used in all components.
!> author: G. Parkin, R. A. Heath, J. Ewen (Newcastle University)
!> date: 2004-07-01
!>
!> This module contains grid-related variables that are shared across
!> all components of the SHETRAN model. It includes grid dimensions
!> and mapping arrays.
!>
!> @note This module is a legacy component derived from an old `INCLUDE` file.
!> The refactoring plan in `docs/reports/refactor_parameters/plan_parameter_refactor.md`
!> outlines renaming this module to `grid_variables.f90` and moving it to
!> `src/parameters/global_system/`.
!>
!> @history
!> | Date       | Author | Version | Description                                                              |
!> |:-----------|:-------|:--------|:-------------------------------------------------------------------------|
!> | 1991-03-01 | GP     | 3.0     | Original `INCLUDE` file written.                                         |
!> | 1994-10-01 | RAH    | 3.4.1   | Modernized declarations, removed INTEGER*2, standard header.             |
!> | 1998-03-07 | RAH    | 4.2     | Cosmetics.                                                               |
!> | 2004-07-01 | JE     | -       | Converted to Fortran 95.                                                 |
!> | 2025-08-11 | AI     | -       | Added KIND parameters and FORD docs.                                     |
MODULE AL_G
   USE SGLOBAL, ONLY : NELEE, NXEE, NYEE
   USE MOD_PARAMETERS, ONLY: I_P
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: NX, NY, NGDBGN
   PUBLIC :: ICMREF, ICMXY

   INTEGER(KIND=I_P) :: NX !! Number of grid squares in x-direction.
   INTEGER(KIND=I_P) :: NY !! Number of grid squares in y-direction.
   INTEGER(KIND=I_P) :: NGDBGN !! First grid element number.
   INTEGER(KIND=I_P), DIMENSION(NELEE,12) :: ICMREF !! Grid element reference mapping array.
   INTEGER(KIND=I_P), DIMENSION(NXEE,NYEE) :: ICMXY !! Grid element number mapping from (x,y) coordinates.
END MODULE AL_G
