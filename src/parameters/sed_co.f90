!> summary: Previous sediment state used while preparing column and link calculations.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the legacy `SED.CO` common block. It stores old or
!> previous-step sediment variables used when preparing to run the column
!> (`COLM`) and link calculations. These arrays carry loose-sediment, surface
!> water, erosion, and sediment fraction state between component calls.
!>
!> State convention:
!>
!> | Symbol | Previous value stored |
!> |:-------|:----------------------|
!> | `DLSO` | Loose/bed sediment depth by element. |
!> | `GNUO` | Unsaturated-zone erosion/source term by element. |
!> | `FBETAO` | Loose/bed sediment composition fraction by element and size class. |
!> | `FDELO` | Mobile sediment concentration fraction by element and size class. |
!> | `FBBEDO` | Link bed sediment composition fraction by link and size class. |
!> | `FBTSDO` | Link transported-sediment fraction by link and size class. |
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
USE SGLOBAL, ONLY : NELEE, NLFEE, NSEDEE
IMPLICIT NONE
DOUBLEPRECISION :: DLSO (NELEE)  !! Previous loose/bed sediment depth by element.
DOUBLEPRECISION :: GNUO (NELEE)  !! Previous unsaturated-zone erosion/source term by element.

DOUBLEPRECISION :: FBETAO (NELEE, NSEDEE) !! Previous loose/bed sediment composition fraction by element and size class.
DOUBLEPRECISION :: FDELO (NELEE, NSEDEE)  !! Previous mobile sediment concentration fraction by element and size class.
DOUBLEPRECISION :: FBBEDO (NLFEE, NSEDEE) !! Previous link bed sediment composition fraction by link and size class.
DOUBLEPRECISION :: FBTSDO (NLFEE, NSEDEE) !! Previous link transported-sediment fraction by link and size class.
!PRIVATE :: NELEE, NLFEE, NSEDEE
end MODULE SED_CO
