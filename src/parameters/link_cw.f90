!> summary: Link water variables used before running contaminant link calculations.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the legacy `LINK.CW` common blocks. It stores
!> cross-sectional areas, bed-layer thicknesses, link-end pointers, and previous
!> bed thickness values used while preparing to run the `LINK` contaminant
!> transport routine.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-20 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1991-06-16 | JE | 3.1 | Added `ACPBSG`, `ACPSFO`, `OLDB`, and `QSTRM`; renamed `DBDM` as `DBDI`. |
!> | 1991-06-18 | JE | 3.1 | Included `LINK.CC1`. |
!> | 1991-08-26 | JE | 3.1 | Removed block `OLOL`. |
!> | 1993-02-08 | GP | 3.4 | Moved `QLINK` and `QDEFF` to `SED_CS`. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> | 1998-03-08 | RAH | 4.2 | Amended comments. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
!>
!> @note The manual requires `DBDI > DBS` and notes an implementation bug when
!> `DBDI` equals `2*DBI`.
!> @endnote

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
