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
USE SGLOBAL, ONLY : LLEE
IMPLICIT NONE
DOUBLEPRECISION :: KS                  !! Scaled link length used by contaminant link calculations.
DOUBLEPRECISION :: KSPBK (2, LLEE)     !! Scaled bank-side thicknesses by side and vertical layer.
!COMMON / SIZE / KS, KSPBK (2, LLEE)  
!                             SCALED LENGTHS AND THICKNESSES
!PRIVATE :: LLEE
END MODULE LINK_CC1
