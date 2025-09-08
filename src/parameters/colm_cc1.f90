!> summary: Defines supplementary contaminant variables for the `COLM` subroutine.
!> author: J. Ewen, R.A.H.
!> date: 2008-12-01
!>
!> This module provides arrays used as coefficients and temporary storage for solving
!> the contaminant transport equations within the `COLM` subroutine. It supplements
!> the variables found in `colm_cc.f90` and was converted from a legacy Fortran 77 INCLUDE file.
!>
!> @history
!> | Date       | Author | Version  | Description                               |
!> |:-----------|:-------|:---------|:------------------------------------------|
!> | 1991-05-01 | JE     | 3.0      | Original `INCLUDE` file written.          |
!> | 1991-06-13 | JE     | 3.1      | Checked, no changes.                      |
!> | 1997-03-13 | RAH    | 4.1      | Explicit typing.                          |
!> | 2008-12-01 | JE     | 4.3.5F90 | Converted to Fortran 90.                  |
!> | 2025-08-11 | AI     | -        | Added KIND parameters and FORD docs.      |
MODULE COLM_CC1

   USE SGLOBAL, ONLY : LLEE
   USE MOD_PARAMETERS, ONLY: R8P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: DLT, ELT, ELTSTR, EPS, FLT, GLT, OME, PLT, PLTSTR, QLT, SLT, TLT

   ! Code =====================================================================

   ! These arrays are likely coefficients (sub-diagonal, diagonal, super-diagonal) and
   ! right-hand side vectors for a tridiagonal matrix solver used for contaminant transport.
   REAL(KIND=R8P) :: DLT(LLEE)    !! Diagonal element of the tridiagonal matrix
   REAL(KIND=R8P) :: ELT(LLEE)    !! Super-diagonal element of the tridiagonal matrix
   REAL(KIND=R8P) :: ELTSTR(LLEE) !! Temporary storage for ELT array
   REAL(KIND=R8P) :: EPS(LLEE)    !! Epsilon coefficient array, possibly for weighting
   REAL(KIND=R8P) :: FLT(LLEE)    !! Right-hand side vector of the linear system
   REAL(KIND=R8P) :: GLT(LLEE)    !! Temporary calculation array
   REAL(KIND=R8P) :: OME(LLEE)    !! Omega coefficient array, possibly for weighting
   REAL(KIND=R8P) :: PLT(LLEE)    !! Temporary storage array used in the solver
   REAL(KIND=R8P) :: PLTSTR(LLEE) !! Temporary storage for PLT array
   REAL(KIND=R8P) :: QLT(LLEE)    !! Temporary storage array used in the solver
   REAL(KIND=R8P) :: SLT(LLEE)    !! Sub-diagonal element of the tridiagonal matrix
   REAL(KIND=R8P) :: TLT(LLEE)    !! Temporary calculation array

END MODULE COLM_CC1
