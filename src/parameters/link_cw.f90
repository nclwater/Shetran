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

MODULE LINK_CW
USE SGLOBAL, ONLY : NLFEE
USE LINK_CC1
IMPLICIT NONE
! Imported constants
!     LINK.CC1:        NLFEE
! Commons
DOUBLEPRECISION ACPBDO (NLFEE), ACPBSG (NLFEE), ACPBI (NLFEE)  !! Link cross-sectional area state.
DOUBLEPRECISION ACPSFO (NLFEE)  !! Link surface-flow cross-sectional area state.

!COMMON / AREAO / ACPBDO, ACPBSG, ACPBI, ACPSFO  
!                             X-SECTIONAL AREAS
DOUBLEPRECISION DBS, DBDI  !! Bed surface-layer thickness and minimum combined bed-layer thickness.

!COMMON / DBED / DBS, DBDI  
!                             THICKNESS OF BED SURFACE LAYER, AND THE
!                             MINIMUM ALLOWABLE THICKNESS OF THE
!                             COMBINED BED SURFACE AND DEEP LAYERS
INTEGER :: LENDA (6)  !! Pointers to link-end numbers that can be attached to a given link.

!COMMON / POINT / LENDA  
!                             POINTERS FOR THE NUMBER FOR THE END OF THE
!                             LINKS WHICH CAN BE ATTACHED TO A GIVEN
!                             LINK
DOUBLEPRECISION THBED (NLFEE), THBEDO (NLFEE)  !! Current and previous bed thickness values by link.
!PRIVATE :: NLFEE
END MODULE LINK_CW
