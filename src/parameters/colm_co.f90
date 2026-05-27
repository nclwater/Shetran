!> summary: Allocatable column water-state arrays used before running `COLM`.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `COLM_CO` replaces the legacy `COLM.CO` common blocks. It stores water-flow
!> variables used while preparing to run the column routine `COLM`, but not used
!> directly inside `COLM` itself. Recent versions allocate these arrays to the
!> active model size rather than using the fixed maximum dimensions.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.1 | Original version written. |
!> | 1991-06-16 | JE | 3.1 | Completed. |
!> | 1994-08-08 | GP | 4.0 | Replaced `TH3O` with `VSTHEO`. |
!> | 1997-02-20 | RAH | 4.1 | Added explicit typing. |
!> | 1998-03-08 | RAH | 4.2 | Removed `WELDRO`. |
!> | 1998-11-03 | RAH | - | Removed `ERUZO`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> | 2026-03 | SB | 4.6 | Made column preparation arrays allocatable and added `initialise_colm_co`. |
!> @endhistory
MODULE COLM_CO
USE SGLOBAL, ONLY : total_no_elements, top_cell_no
IMPLICIT NONE

DOUBLEPRECISION, ALLOCATABLE :: DSWO(:)     !! Previous surface-water depth above each column.
DOUBLEPRECISION, ALLOCATABLE :: QIO(:)      !! Previous net rainfall input over each column area.
DOUBLEPRECISION, ALLOCATABLE :: QQRFO(:)    !! Previous base flow into each column bottom cell.
DOUBLEPRECISION, ALLOCATABLE :: RSZWLO(:)   !! Previous well-flow value for each well column.
DOUBLEPRECISION, ALLOCATABLE :: ZONEO(:)    !! Previous nondimensional saturated depth for each column.
DOUBLEPRECISION, ALLOCATABLE :: GGAMMO(:,:) !! Previous dead-space water-change coefficient by column and cell.
DOUBLEPRECISION, ALLOCATABLE :: QQQSWO(:,:) !! Previous lateral surface-water flow by column and face.
DOUBLEPRECISION, ALLOCATABLE :: QQO(:,:,:)  !! Previous lateral subsurface flow by column, cell, and face.
DOUBLEPRECISION, ALLOCATABLE :: UUAJPO(:,:) !! Previous vertical water flux by column and cell interface.
DOUBLEPRECISION, ALLOCATABLE :: VSTHEO(:,:) !! Previous volumetric water content by column and cell.


CONTAINS

!> Allocates and zero-initializes column preparation arrays.
!>
!> The allocation uses the active model size from `total_no_elements` and
!> `top_cell_no`, avoiding the fixed maximum dimensions that were previously
!> used for these legacy common-block arrays.
!>
!> Entry assumptions:
!>
!> | Assumption | Reason |
!> |:-----------|:-------|
!> | `total_no_elements` and `top_cell_no` are set. | They define every allocated extent. |
!> | The allocatable old-state arrays are not already allocated. | The routine allocates unconditionally. |
!>
!> @note This routine has no dummy arguments and mutates module allocatable
!> arrays in `COLM_CO`.
!> @endnote
SUBROUTINE initialise_colm_co()

   allocate (DSWO(total_no_elements))
   allocate (QIO(total_no_elements))
   allocate (QQRFO(total_no_elements))
   allocate (RSZWLO(total_no_elements))
   allocate (ZONEO(total_no_elements))
   allocate (GGAMMO(total_no_elements, top_cell_no+1))
   allocate (QQQSWO(total_no_elements, 4))
   allocate (QQO(total_no_elements, top_cell_no+1, 4))
   allocate (UUAJPO(total_no_elements, top_cell_no+1))
   allocate (VSTHEO(total_no_elements, top_cell_no+1))
   DSWO=0.0d0
   QIO=0.0d0
   QQRFO=0.0d0
   RSZWLO=0.0d0
   ZONEO=0.0d0
   GGAMMO=0.0d0
   QQQSWO=0.0d0
   QQO=0.0d0
   UUAJPO=0.0d0
   VSTHEO=0.0d0

END SUBROUTINE initialise_colm_co

END MODULE COLM_CO
