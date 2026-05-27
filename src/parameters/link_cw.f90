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
USE SGLOBAL, ONLY : NLFEE
USE LINK_CC1
IMPLICIT NONE

DOUBLEPRECISION :: ACPBDO(NLFEE) !! Previous deeper-bed/deposited-material cross-sectional area.
DOUBLEPRECISION :: ACPBSG(NLFEE) !! Bed-surface-layer cross-sectional area, `DBS*CWIDTH/Z2**2`.
DOUBLEPRECISION :: ACPBI(NLFEE)  !! Initial deeper-bed/deposited-material cross-sectional area.
DOUBLEPRECISION :: ACPSFO(NLFEE) !! Previous stream-water cross-sectional area, `ARXL/Z2**2`.

DOUBLEPRECISION :: DBS  !! Bed surface-layer depth below the river bed.
DOUBLEPRECISION :: DBDI !! Bed deep-layer depth below the river bed; must be greater than `DBS`.

INTEGER :: LENDA(6) !! Link-end pointer table for ends that can attach to a link.

DOUBLEPRECISION :: THBED(NLFEE)  !! Current stream-bed moisture content by link.
DOUBLEPRECISION :: THBEDO(NLFEE) !! Previous stream-bed moisture content by link.
!PRIVATE :: NLFEE
END MODULE LINK_CW
