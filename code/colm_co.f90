MODULE COLM_CO
!-------------------------- Start of COLM.CO --------------------------*
!
!         CM-COMPONENT INCLUDE-FILE FOR WATER VARIABLES USED IN
!                      THE PREPARATION FOR RUNNING SUBROUTINE COLM
!                      BUT NOT USED IN COLM ITSELF
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/COLM.CO/4.2
! Modifications:
!                          JE     26/4/91   3.1     WRITTEN
!                          JE     16/6/91   3.1     COMPLETED
!  GP  940808  4.0  Replace TH3O with VSTHEO (see INCM,COLMW).
! RAH  970220  4.1  Explicit typing.
! RAH  980308  4.2  Remove WELDRO.
!      981103       Remove ERUZO (see INCM,COLMW).
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*

! Imported constants
!                             NELEE,LLEE

! Commons
USE SGLOBAL, ONLY : NELEE, LLEE
IMPLICIT NONE
DOUBLEPRECISION DSWO (NELEE), GGAMMO (NELEE, LLEE)  
DOUBLEPRECISION QIO (NELEE), QQO (NELEE, LLEE, 4)  
DOUBLEPRECISION QQRFO (NELEE), QQQSWO (NELEE, 4)  
DOUBLEPRECISION RSZWLO (NELEE), UUAJPO (NELEE, LLEE)  
DOUBLEPRECISION ZONEO (NELEE), VSTHEO (NELEE, LLEE)  
!PRIVATE :: NELEE, LLEE
END MODULE COLM_CO
