!> summary: Defines setup variables for soil column calculations.
!> author: J. Ewen, R.A.H., J. Ewen
!> date: 2008-12-01
!>
!> This module provides variables used in the preparation for running the `COLM` subroutine,
!> but which are not used directly within `COLM` itself. It includes data related to
!> grid geometry, soil layers, and well properties. It was converted from a legacy
!> Fortran 77 INCLUDE file.
!>
!> @history
!> | Date       | Author | Version  | Description                                       |
!> |:-----------|:-------|:---------|:--------------------------------------------------|
!> | 1991-04-26 | JE     | 3.0      | Original `INCLUDE` file written.                  |
!> | 1991-06-13 | JE     | 3.1      | Checked and tidied text.                          |
!> | 1991-07-16 | JE     | 3.1      | Reordered names in WELPRO.                        |
!> | 1997-02-24 | RAH    | 4.1      | Explicit typing. Separated /WELPRI/ from /WELPRO/.|
!> | 2008-12-01 | JE     | 4.3.5F90 | Converted to Fortran 90.                          |
!> | 2025-08-11 | AI     | -        | Added KIND parameters and FORD docs.              |
MODULE COLM_CG

   USE SGLOBAL, ONLY : NELEE, LLEE, NVEE, NOLEE
   USE MOD_PARAMETERS, ONLY: R8P, I_P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: KSPE, KSPPE
   PUBLIC :: JBTLYR, NCOLMB
   PUBLIC :: ZCOLMB
   PUBLIC :: SCL, OODO
   PUBLIC :: JKZCOB, JKZCOL, JOLFN, NOL, NOLBT, NOLCE, NOLCEA
   PUBLIC :: WELDRA
   PUBLIC :: JKZWEL, JKZWCE

   ! Code =====================================================================

   ! Non-dimensioned cell thicknesses
   REAL(KIND=R8P) :: KSPE(LLEE, NVEE)   !! Non-dimensioned cell thickness
   REAL(KIND=R8P) :: KSPPE(LLEE, NVEE)  !! Non-dimensioned cell thickness at previous time step

   ! Numbers for the bottom soil layer and cell in soil columns
   INTEGER(KIND=I_P) :: JBTLYR(NELEE)    !! Index of the bottom soil layer in each column
   INTEGER(KIND=I_P) :: NCOLMB(NELEE)    !! Number of cells in each soil column

   ! Elevation to base of soil columns
   REAL(KIND=R8P) :: ZCOLMB(NELEE)      !! Elevation of the base of each soil column (m)

   ! Constants
   REAL(KIND=R8P) :: SCL                !! A scaling constant
   REAL(KIND=R8P) :: OODO               !! A constant, possibly 1.0 / D0

   ! Face overlap and lateral transmissivity values
   INTEGER(KIND=I_P) :: JKZCOB(NELEE, 4)           !! Index related to column base overlap
   INTEGER(KIND=I_P) :: JKZCOL(NELEE, NOLEE, 4)    !! Index related to column overlap
   INTEGER(KIND=I_P) :: JOLFN(NELEE, NOLEE, 4)     !! Index related to face overlap
   INTEGER(KIND=I_P) :: NOL(NELEE, 4)              !! Number of lateral overlaps
   INTEGER(KIND=I_P) :: NOLBT(NELEE, LLEE, 4)      !! Index related to bottom layer overlap
   INTEGER(KIND=I_P) :: NOLCE(NELEE, NOLEE, 4)     !! Index related to cell overlap
   INTEGER(KIND=I_P) :: NOLCEA(NELEE, NOLEE, 4)    !! Index related to cell overlap (alternative)

   ! Withdrawal rates for wells
   REAL(KIND=R8P) :: WELDRA(LLEE)       !! Withdrawal rates for wells (m3/s)

   ! Well location indices
   INTEGER(KIND=I_P) :: JKZWEL(NELEE)              !! Index for well location in a column
   INTEGER(KIND=I_P) :: JKZWCE(NELEE, LLEE)        !! Index for well location in a cell

END MODULE COLM_CG
