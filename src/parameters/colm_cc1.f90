!> summary: Coupled equation workspace for the contaminant column solver.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> `COLM_CC1` replaces the legacy `COLM.CC1` common block and supplements
!> [[colm_cc]]. [[cmmod:colm]] assembles these coefficient and right-hand-side
!> arrays for one contaminant in one column, then [[cmmod:slvclm]] eliminates
!> the dead-space unknown, calls [[utilsmod:tridag]], and returns the dynamic-
!> and dead-space-region concentration rates in `OME` and `EPS`.
!>
!> For equation row (i), the stored linear system is
!>
!> \[
!>   \mathrm{FLT}_i\Omega_{i-1}+\mathrm{ELT}_i\Omega_i
!>   +\mathrm{DLT}_i\Omega_{i+1}-\mathrm{GLT}_i\epsilon_i=\mathrm{SLT}_i,
!>   \qquad
!>   \mathrm{PLT}_i\epsilon_i-\mathrm{TLT}_i\Omega_i=\mathrm{QLT}_i.
!> \]
!>
!> `COLM` maps physical cell `NC` to row
!> `i=NC-NCEBOT+1`; therefore the active range is
!> `1:NCETOP-NCEBOT+1`, within the capacity bound `LLEE`. `FLT`, `ELT`, and
!> `DLT` are respectively the lower, diagonal, and upper tridiagonal
!> coefficients. For nonlinear adsorption, `SLVCLM` adds
!> `ELTSTR*OME` and `PLTSTR*EPS` to the two diagonals and performs exactly ten
!> Picard iterations, without a convergence test.
!>
!> This module is transient shared workspace, not persistent per-element
!> state. It supplies no initial values; `COLM` must assemble every active row
!> before `SLVCLM` uses it. All arrays and imported `LLEE` are public because
!> no `PRIVATE` statement is active.
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

   DOUBLEPRECISION DLT(LLEE)     !! Coefficient of the next-row dynamic rate, `OME(i+1)`.
   DOUBLEPRECISION ELT(LLEE)     !! Diagonal coefficient of the dynamic rate, `OME(i)`.
   DOUBLEPRECISION ELTSTR(LLEE)  !! Nonlinear correction multiplying `OME(i)` in its diagonal.
   DOUBLEPRECISION EPS(LLEE)     !! Solved dead-space concentration rate with respect to scaled time.
   DOUBLEPRECISION FLT(LLEE)     !! Coefficient of the previous-row dynamic rate, `OME(i-1)`.
   DOUBLEPRECISION GLT(LLEE)     !! Magnitude of the negative `EPS(i)` coupling in the dynamic equation.
   DOUBLEPRECISION OME(LLEE)     !! Solved dynamic-region concentration rate with respect to scaled time.
   DOUBLEPRECISION PLT(LLEE)     !! Diagonal coefficient of the dead-space rate, `EPS(i)`.
   DOUBLEPRECISION PLTSTR(LLEE)  !! Nonlinear correction multiplying `EPS(i)` in its diagonal.
   DOUBLEPRECISION QLT(LLEE)     !! Right-hand side of the dead-space equation.
   DOUBLEPRECISION SLT(LLEE)     !! Right-hand side of the dynamic-region equation.
   DOUBLEPRECISION TLT(LLEE)     !! Magnitude of the negative `OME(i)` coupling in the dead-space equation.
!PRIVATE :: LLEE
END MODULE COLM_CC1
