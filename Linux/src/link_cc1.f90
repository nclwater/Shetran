MODULE LINK_CC1
!                                         LINK.CC1
!
!                      INCLUDE FILE FOR CONTAMINANT VARIABLES USED IN
!                      SUBROUTINE LINK
!
!                                 PROGRAM AMENDMENT HISTORY
!
!                      AMENDED BY  DATE   VERSION   REASON FOR AMENDMENT
!                      ----------  ----   -------   --------------------
!                          JE     18/5/91   3.1     WRITTEN
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!-----------------------------------------------------------------------
USE SGLOBAL, ONLY : LLEE
IMPLICIT NONE
DOUBLEPRECISION :: KS, KSPBK (2, LLEE) 
!COMMON / SIZE / KS, KSPBK (2, LLEE)  
!                             SCALED LENGTHS AND THICKNESSES
!PRIVATE :: LLEE
END MODULE LINK_CC1
