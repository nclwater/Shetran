!> summary: Supplementary column contaminant variables.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the legacy `COLM.CC1` common block. It supplements
!> `COLM_CC` with vertical-layer arrays used by contaminant calculations in
!> `COLM`, including transport coefficients, source terms, and intermediate
!> solution vectors.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-01 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked, no changes. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE COLM_CC1
USE SGLOBAL, ONLY : LLEE
IMPLICIT NONE
DOUBLEPRECISION :: DLT(LLEE)    !! Upper-cell coefficient in the mobile-concentration equation.
DOUBLEPRECISION :: ELT(LLEE)    !! Diagonal coefficient for the mobile-concentration equation.
DOUBLEPRECISION :: ELTSTR(LLEE) !! Mobile-equation derivative coefficient for nonlinear adsorption.
DOUBLEPRECISION :: EPS(LLEE)    !! Solved sorbed-concentration rate increment.
DOUBLEPRECISION :: FLT(LLEE)    !! Lower-cell coefficient in the mobile-concentration equation.
DOUBLEPRECISION :: GLT(LLEE)    !! Sorbed-rate coupling coefficient in the mobile-concentration equation.
DOUBLEPRECISION :: OME(LLEE)    !! Solved mobile-concentration rate increment.
DOUBLEPRECISION :: PLT(LLEE)    !! Diagonal coefficient for the sorbed-concentration equation.
DOUBLEPRECISION :: PLTSTR(LLEE) !! Sorbed-equation derivative coefficient for nonlinear adsorption.
DOUBLEPRECISION :: QLT(LLEE)    !! Right-hand side for the sorbed-concentration equation.
DOUBLEPRECISION :: SLT(LLEE)    !! Right-hand side for the mobile-concentration equation.
DOUBLEPRECISION :: TLT(LLEE)    !! Mobile-rate coupling coefficient in the sorbed-concentration equation.
!PRIVATE :: LLEE
end MODULE COLM_CC1
