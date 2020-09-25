MODULE LINK_CW
!------------------------------- Start of LINK.CW ---------------------*
!
!                      INCLUDE FILE FOR WATER VARIABLES USED IN
!                      PREPARING TO RUN SUBROUTINE LINK
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/LINK.CW/4.2
! Modifications:
!                          JE     20/5/91   3.0     WRITTEN
!                          JE     13/6/91   3.1     COMPLETED
!                          JE     16/6/91   3.1     ADD ACPBSG, ACPSFO,
!                                                   OLDB, QSTRM; RENAME
!                                                   DBDM AS DBDI
!                          JE     18/6/91   3.1     LINK.CC1 INCLUDED
!                          JE     26/8/91   3.1     BLOCK OLOL REMOVED
!  GP  930208  3.4  Move QLINK,QDEFF to SED.CS.
! RAH  970313  4.1  Explicit typing.
! RAH  980308  4.2  Amend comment.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*

! Other commons and constants

USE SGLOBAL, ONLY : NLFEE
USE LINK_CC1
IMPLICIT NONE
! Imported constants
!     LINK.CC1:        NLFEE
! Commons
DOUBLEPRECISION ACPBDO (NLFEE), ACPBSG (NLFEE), ACPBI (NLFEE)  
DOUBLEPRECISION ACPSFO (NLFEE)  

!COMMON / AREAO / ACPBDO, ACPBSG, ACPBI, ACPSFO  
!                             X-SECTIONAL AREAS
DOUBLEPRECISION DBS, DBDI  

!COMMON / DBED / DBS, DBDI  
!                             THICKNESS OF BED SURFACE LAYER, AND THE
!                             MINIMUM ALLOWABLE THICKNESS OF THE
!                             COMBINED BED SURFACE AND DEEP LAYERS
INTEGER :: LENDA (6)  

!COMMON / POINT / LENDA  
!                             POINTERS FOR THE NUMBER FOR THE END OF THE
!                             LINKS WHICH CAN BE ATTACHED TO A GIVEN
!                             LINK
DOUBLEPRECISION THBED (NLFEE), THBEDO (NLFEE)  
!PRIVATE :: NLFEE
END MODULE LINK_CW
