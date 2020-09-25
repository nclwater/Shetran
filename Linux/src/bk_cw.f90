MODULE BK_CW
!--------------------------- Start of BK.CW ---------------------------*
!
!  CM COMPONENT INCLUDE-FILE FOR WATER VARIABLES FOR BANKS
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/BK.CW/4.2
! Modifications:
!                          JE     13/6/91   3.1     WRITTEN
!                          JE     16/6/91   3.1     SYNTAX ERRORS
!                                                   CORRECTED
!                          JE     17/7/91   3.1     REORDERED NAMES IN
!                                                   BKOL AND LFBK
!  GP  950310  4.0  Replace BFSCL (see LINKW,COLMW) with BK*.
! RAH  970219  4.1  Remove FNOLCH,NOLCH,JKZCH,LINKF,NBKA,BKTHE,BKQV,BKQH
!                   (redundant).  Explicit typing.
!                   Separate /LFBKI/ from mixed-type /LFBK/.
! RAH  970220       Amend descriptions of NCEBD,FNCEBD.
! RAH  980308  4.2  Remove OLBD (see INCM).
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*

! Imported constants
!                      LLEE,NLFEE
USE SGLOBAL, ONLY : NLFEE, LLEE
IMPLICIT NONE
DOUBLEPRECISION FNCEBD (NLFEE, 2)  

!COMMON / LFBK / FNCEBD  
!                             FRACTION OF CELL NCEBD+1 WHICH LIES
!                             BELOW THE BED DEEP LAYER
INTEGER :: NBANK (NLFEE, 2), NCEAB (NLFEE, 2), NCEBD (NLFEE, 2)  

!COMMON / LFBKI / NBANK, NCEAB, NCEBD  
!                             NUMBER FOR THE BANK ADJACENT TO A LINK;
!                             NUMBER FOR THE LOWEST CELL TO EXCHANGE
!                             WATER WITH THE STREAM;
!                             NUMBER OF THE HIGHEST CELL WHICH LIES
!                             FULLY BELOW THE BED DEEP LAYER
DOUBLEPRECISION QQRVO (NLFEE, LLEE, 2)  
!PRIVATE :: NLFEE, LLEE
END MODULE BK_CW
