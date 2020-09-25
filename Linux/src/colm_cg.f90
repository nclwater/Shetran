MODULE COLM_CG
!---------------------------- Start of COLM.CG ------------------------*
!
!                      INCLUDE FILE FOR WATER VARIABLES USED IN
!                      THE PREPARATION FOR RUNNING SUBROUTINE COLM
!                      BUT NOT USED IN SUBROUTINE COLM
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/COLM.CG/4.1
! Modifications:
!                          JE     26/4/91   3.0     WRITTEN
!                          JE     13/6/91   3.1     CHECKED, TIDIED TEXT
!                          JE     16/7/91   3.1     REORDERED NAMES IN
!                                                   WELPRO
! RAH  970224  4.1  Explicit typing.
!                   Separate /WELPRI/ from mixed-type /WELPRO/.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Imported constants
!                      LLEE,NELEE,NOLEE,NVEE
! Commons
USE SGLOBAL, ONLY : NELEE, LLEE, NVEE, NOLEE
IMPLICIT NONE
DOUBLEPRECISION KSPE (LLEE, NVEE), KSPPE (LLEE, NVEE)  

!COMMON / CELLTK / KSPE, KSPPE  
!                             NON-DIMENSIONED CELL THICKNESSES
INTEGER :: JBTLYR (NELEE), NCOLMB (NELEE)  

!COMMON / COLUMN / JBTLYR, NCOLMB  
!                             NUMBERS FOR THE BOTTOM SOIL LAYER
!                             AND CELL IN SOIL COLUMNS
DOUBLEPRECISION ZCOLMB (NELEE)  

!COMMON / ZCLUMN / ZCOLMB  
!                             ELEVATION TO BASE OF SOIL COLUMNS
DOUBLEPRECISION SCL, OODO  

!COMMON / OCONST / SCL, OODO  
!                             CONSTANTS
INTEGER :: JKZCOB (NELEE, 4), JKZCOL (NELEE, NOLEE, 4)  
INTEGER :: JOLFN (NELEE, NOLEE, 4), NOL (NELEE, 4)  
INTEGER :: NOLBT (NELEE, LLEE, 4), NOLCE (NELEE, NOLEE, 4)  
INTEGER :: NOLCEA (NELEE, NOLEE, 4)  

!COMMON / OVRLAP / JKZCOB, JKZCOL, JOLFN, NOL, NOLBT, NOLCE, &
 !NOLCEA
!                             FACE OVERLAP AND LATERAL
!                             TRANSMISIVITY VALUES
DOUBLEPRECISION WELDRA (LLEE)  

!COMMON / WELPRO / WELDRA  
!                             WITHDRAWL RATES FOR WELLS
INTEGER :: JKZWEL (NELEE), JKZWCE (NELEE, LLEE)  
!PRIVATE :: NELEE, LLEE, NVEE, NOLEE
END MODULE COLM_CG
