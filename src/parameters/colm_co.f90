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
USE SGLOBAL, ONLY : NELEE, LLEE, total_no_elements, top_cell_no
IMPLICIT NONE
!DOUBLEPRECISION DSWO (NELEE), GGAMMO (NELEE, LLEE)  
!DOUBLEPRECISION QIO (NELEE), QQO (NELEE, LLEE, 4)  
!DOUBLEPRECISION QQRFO (NELEE), QQQSWO (NELEE, 4)  
!DOUBLEPRECISION RSZWLO (NELEE), UUAJPO (NELEE, LLEE)  
!DOUBLEPRECISION ZONEO (NELEE), VSTHEO (NELEE, LLEE)  
DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: DSWO, QIO
DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: QQRFO, RSZWLO, ZONEO
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: GGAMMO,QQQSWO
DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: QQO
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: UUAJPO,VSTHEO

!PRIVATE :: NELEE, LLEE


CONTAINS

!> Allocates and zero-initializes column preparation arrays.
!>
!> The allocation uses the active model size from `total_no_elements` and
!> `top_cell_no`, avoiding the fixed maximum dimensions that were previously
!> used for these legacy common-block arrays.
!>
!> @note This routine has no dummy arguments and mutates module allocatable
!> arrays in `COLM_CO`.
!> @endnote
SUBROUTINE initialise_colm_co()

!                             FACE OVERLAP AND LATERAL
!                             TRANSMISIVITY VALUES
! NOLBT and NOLCEA are in contaminant transport component the other variables only during the initialisation
   allocate   (DSWO(total_no_elements),QIO(total_no_elements))
   allocate   (QQRFO(total_no_elements),RSZWLO(total_no_elements))
   allocate   (ZONEO(total_no_elements))
   allocate   (GGAMMO(total_no_elements,top_cell_no+1),QQQSWO(total_no_elements,4))
   allocate   (QQO(total_no_elements,top_cell_no+1,4))
   allocate   (UUAJPO(total_no_elements,top_cell_no+1),VSTHEO(total_no_elements,top_cell_no+1))
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
