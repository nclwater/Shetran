!> summary: Column water-flow scaling and numbering variables.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the legacy `COLM.C1` common blocks. It stores scale
!> references, finite-difference constants, and column-numbering values used by
!> the `COLM` water and contaminant column calculations.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.1 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1991-06-16 | JE | 3.1 | Removed references to `LNCONT` and `LNSOIL`. |
!> | 1991-07-17 | JE | 3.1 | Reordered names in `CLNUM`. |
!> | 1991-08-26 | JE | 3.1 | Moved parameter `NCETOP` to `BLOCKCLNUM`. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing and split mixed-type `CLNUM`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE COLM_C1
IMPLICIT NONE
!
! Commons
DOUBLEPRECISION D0, Z2, Z2SQ, Z2OD, Z2SQOD  !! Column scale references and derived scale factors.

!COMMON / CLREF / D0, Z2, Z2SQ, Z2OD, Z2SQOD  
!                            SCALE REFERENCES
DOUBLEPRECISION CST1, CST2, CST3, SGMA, SGSQ, SGTSE, SGSTSE, &
 OMSGMA  !! Finite-difference and scaling constants.
DOUBLEPRECISION OPSGL, OPSGSL, TSE  !! Finite-difference and time-scale constants.

!COMMON / FD / CST1, CST2, CST3, SGMA, SGSQ, SGTSE, SGSTSE, OMSGMA, &
! OPSGL, OPSGSL, TSE
!                             FINITE DIFFERENCE AND SCALING CONSTANTS
DOUBLEPRECISION FNCPSF  !! Fraction of the highest cell below the phreatic surface.

!COMMON / CLNUM / FNCPSF  
!                             CELL FRACTION FOR HIGHEST CELL BELOW
!                             PHREATIC SURFACE
INTEGER :: NCEBOT, NCETOP, NCEPSF  !! Bottom, top, and phreatic-surface column cell indices.

END MODULE COLM_C1
