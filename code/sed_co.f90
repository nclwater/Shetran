MODULE SED_CO
!------------------------------- Start of SED.CO ----------------------*
!
!                       INCLUDE FILE FOR OLD SEDIMENT-VARIABLES USED IN
!                       THE PREPARATION FOR RUNNING SUBROUTINES COLM
!                       AND LINK
!
!----------------------------------------------------------------------*
! Version:  /SHETRAN/INCLUDE/SED.CO/4.1
! Modifications:
!                           JE      APR 91   3.0     WRITTEN
!                           JE     13/6/91   3.1     COMPLETED
! RAH  970314  4.1  Explicit typing.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Imported constants
!                       NELEE,NLFEE,NSEDEE
!
! Commons
USE SGLOBAL, ONLY : NELEE, NLFEE, NSEDEE
IMPLICIT NONE
DOUBLEPRECISION DLSO (NELEE), GNUO (NELEE)  

!COMMON / SDDEPO / DLSO, GNUO  
!                             OLD VALUES OF DEPTH OF LOOSE SEDIMENT
!                             AND SURFACE WATER, AND THE RATE OF EROSION
!                             OF THE UNSATURATED ZONE
DOUBLEPRECISION FBETAO (NELEE, NSEDEE), FDELO (NELEE, NSEDEE)  
DOUBLEPRECISION FBBEDO (NLFEE, NSEDEE), FBTSDO (NLFEE, NSEDEE)  
!PRIVATE :: NELEE, NLFEE, NSEDEE
end MODULE SED_CO
