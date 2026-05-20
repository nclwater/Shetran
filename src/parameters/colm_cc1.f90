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
DOUBLEPRECISION DLT (LLEE), ELT (LLEE), ELTSTR (LLEE), EPS (LLEE)  !! Layer contaminant transport/work arrays.
DOUBLEPRECISION FLT (LLEE), GLT (LLEE), OME (LLEE), PLT (LLEE)  !! Layer contaminant coefficient/work arrays.
DOUBLEPRECISION PLTSTR (LLEE), QLT (LLEE), SLT (LLEE), TLT (LLEE)  !! Layer contaminant source and solution arrays.
!PRIVATE :: LLEE
end MODULE COLM_CC1
