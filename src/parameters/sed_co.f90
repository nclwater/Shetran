!> summary: Previous sediment state used while preparing column and link calculations.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the legacy `SED.CO` common block. It stores old or
!> previous-step sediment variables used when preparing to run the column
!> (`COLM`) and link calculations. These arrays carry loose-sediment, surface
!> water, erosion, and sediment fraction state between component calls.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1997-03-14 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
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
