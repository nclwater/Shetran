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
!------------------------------- Start of COLM.CC1 --------------------*
!
!                      INCLUDE FILE FOR CONTAMINANT VARIABLES USED IN
!                      SUBROUTINE COLM (SUPPLEMENTS COLM.CC)
!
!----------------------------------------------------------------------*
! Version:  /SHETRAN/INCLUDE/COLM.CC1/4.1
! Modifications:
!                          JE    1/5/91     3.0     WRITTEN
!                          JE   13/6/91     3.1     CHECKED, NO CHANGES
! RAH  970313  4.1  Explicit typing.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Commons
   USE SGLOBAL, ONLY : LLEE
   IMPLICIT NONE
   DOUBLEPRECISION DLT (LLEE), ELT (LLEE), ELTSTR (LLEE), EPS (LLEE)
   DOUBLEPRECISION FLT (LLEE), GLT (LLEE), OME (LLEE), PLT (LLEE)
   DOUBLEPRECISION PLTSTR (LLEE), QLT (LLEE), SLT (LLEE), TLT (LLEE)
!PRIVATE :: LLEE
end MODULE COLM_CC1
