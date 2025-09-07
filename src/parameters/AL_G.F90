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
! @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | 1991-03 | GP | v3.0: Written. |
!> | 1994-10-01 | RAH | v3.4.1: Modernized declarations, removed INTEGER*2, standard header. |
!> | 1998-03-07 | RAH | v4.2: Cosmetics. |
!> | 2004-07 | JE | Converted to Fortran 95. |
!> | 2024-09-05 | Gemini | Converted documentation to FORD format. |
MODULE AL_G
   USE SGLOBAL, ONLY : NELEE, NXEE, NYEE
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: NX, NY, NGDBGN
   PUBLIC :: ICMREF, ICMXY

   INTEGER :: NX !! Number of grid squares in x-direction.
   INTEGER :: NY !! Number of grid squares in y-direction.
   INTEGER :: NGDBGN !! First grid element number.
   INTEGER, DIMENSION(NELEE,12) :: ICMREF !! Grid element reference mapping array.
   INTEGER, DIMENSION(NXEE,NYEE) :: ICMXY !! Grid element number mapping from (x,y) coordinates.
END MODULE AL_G
