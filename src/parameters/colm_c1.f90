!> summary: Defines common variables for contaminant transport calculations in soil columns.
!> author: J. Ewen, R.A.H.
!> date: 2008-12-01
!>
!> This module provides shared variables used in the `COLM` subroutine for contaminant transport.
!> It was converted from a legacy Fortran 77 INCLUDE file.
!>
!> @history
!> | Date       | Author | Version  | Description                                       |
!> |:-----------|:-------|:---------|:--------------------------------------------------|
!> | 1991-04-26 | JE     | 3.1      | Original `INCLUDE` file written.                  |
!> | 1991-06-13 | JE     | 3.1      | Completed.                                        |
!> | 1991-06-16 | JE     | 3.1      | References to LNCONT and LNSOIL removed.          |
!> | 1991-07-17 | JE     | 3.1      | Reordered names in CLNUM.                         |
!> | 1991-08-26 | JE     | 3.1      | Moved parameter NCETOP to BLOCKCLNUM.             |
!> | 1997-03-13 | RAH    | 4.1      | Explicit typing. Split mixed-type /CLNUM/.        |
!> | 2008-12-01 | JE     | 4.3.5F90 | Converted to Fortran 90.                          |
!> | 2025-08-11 | AI     | -        | Added KIND parameters and FORD docs.              |
MODULE COLM_C1

   USE MOD_PARAMETERS, ONLY: R8P, I_P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: D0, Z2, Z2SQ, Z2OD, Z2SQOD
   PUBLIC :: CST1, CST2, CST3, SGMA, SGSQ, SGTSE, SGSTSE, OMSGMA, OPSGL, OPSGSL, TSE
   PUBLIC :: FNCPSF
   PUBLIC :: NCEBOT, NCETOP, NCEPSF

   ! Code =====================================================================

   ! Scale references
   REAL(KIND=R8P) :: D0       !! Molecular diffusion coefficient (m2/s)
   REAL(KIND=R8P) :: Z2       !! Vertical dispersivity (m)
   REAL(KIND=R8P) :: Z2SQ     !! Square of vertical dispersivity (m2)
   REAL(KIND=R8P) :: Z2OD     !! Vertical dispersivity divided by D0 (s/m)
   REAL(KIND=R8P) :: Z2SQOD   !! Square of vertical dispersivity divided by D0 (s)

   ! Finite difference and scaling constants
   REAL(KIND=R8P) :: CST1     !! Scaling constant
   REAL(KIND=R8P) :: CST2     !! Scaling constant
   REAL(KIND=R8P) :: CST3     !! Scaling constant
   REAL(KIND=R8P) :: SGMA     !! Scaling factor
   REAL(KIND=R8P) :: SGSQ     !! Square of scaling factor
   REAL(KIND=R8P) :: SGTSE    !! Scaling factor related to time step
   REAL(KIND=R8P) :: SGSTSE   !! Scaling factor related to time step
   REAL(KIND=R8P) :: OMSGMA   !! 1.0 - SGMA
   REAL(KIND=R8P) :: OPSGL    !! Scaling factor
   REAL(KIND=R8P) :: OPSGSL   !! Scaling factor
   REAL(KIND=R8P) :: TSE      !! Time step for contaminant transport (s)

   REAL(KIND=R8P) :: FNCPSF   !! Cell fraction for highest cell below phreatic surface

   INTEGER(KIND=I_P) :: NCEBOT !! Index of the bottom cell in the column
   INTEGER(KIND=I_P) :: NCETOP !! Index of the top cell in the column
   INTEGER(KIND=I_P) :: NCEPSF !! Index of the cell containing the phreatic surface

END MODULE COLM_C1
