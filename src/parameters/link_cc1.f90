!> summary: Contaminant link scaling variables.
!> author: JE, Newcastle University
!>
!> This module replaces the legacy `LINK.CC1` common block. It stores scaled
!> lengths and thicknesses used by contaminant calculations in the `LINK`
!> routine, including values for the two link-bank sides over vertical layers.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-18 | JE | 3.1 | Original version written. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
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
