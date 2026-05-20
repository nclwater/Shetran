!> summary: Column geometry and face-overlap arrays used before running `COLM`.
!> author: JE, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `COLM_CG` stores column geometry, overlap, lateral-transmissivity, and well
!> withdrawal state used during preparation for the column routine `COLM`. The
!> largest overlap arrays are allocated to the active model size during
!> initialization and can be released after contaminant setup.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked and tidied text. |
!> | 1991-07-16 | JE | 3.1 | Reordered names in `WELPRO`. |
!> | 1997-02-24 | RAH | 4.1 | Added explicit typing and separated `WELPRI` from mixed-type `WELPRO`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> | 2026-03 | SB | 4.6 | Removed unused legacy arrays, made overlap arrays allocatable, and added allocation/deallocation routines. |
!> @endhistory
MODULE COLM_CG
USE SGLOBAL, ONLY : NELEE, LLEE, NVEE, NOLEE, total_no_elements,top_cell_no
IMPLICIT NONE

!sb 040326 comment out KSPE and KSPPE as no longer used
!DOUBLEPRECISION KSPE (LLEE, NVEE), KSPPE (LLEE, NVEE)  

!COMMON / CELLTK / KSPE, KSPPE  
!                             NON-DIMENSIONED CELL THICKNESSES
INTEGER :: JBTLYR (NELEE), NCOLMB (NELEE)  

!COMMON / COLUMN / JBTLYR, NCOLMB  
!                             NUMBERS FOR THE BOTTOM SOIL LAYER
!                             AND CELL IN SOIL COLUMNS
DOUBLEPRECISION ZCOLMB (NELEE)  

!COMMON / ZCLUMN / ZCOLMB  
!                             ELEVATION TO BASE OF SOIL COLUMNS
DOUBLEPRECISION SCL, OODO  

!COMMON / OCONST / SCL, OODO  
!                             CONSTANTS



!sb 020326 comment out JKZCOB as no longer used
!INTEGER :: JKZCOB (NELEE, 4)
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: JKZCOL,JOLFN
INTEGER, DIMENSION(:,:), ALLOCATABLE :: NOL
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: NOLBT,NOLCE
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: NOLCEA

!INTEGER :: JKZCOL (NELEE, NOLEE, 4)  
!INTEGER :: JOLFN (NELEE, NOLEE, 4), NOL (NELEE, 4)  
!INTEGER :: NOLBT (NELEE, LLEE, 4), NOLCE (NELEE, NOLEE, 4)  
!INTEGER :: NOLCEA (NELEE, NOLEE, 4)  
!COMMON / OVRLAP / JKZCOB, JKZCOL, JOLFN, NOL, NOLBT, NOLCE, &
 !NOLCEA
!                             FACE OVERLAP AND LATERAL
!                             TRANSMISIVITY VALUES


DOUBLEPRECISION WELDRA (LLEE)  

!COMMON / WELPRO / WELDRA  
!                             WITHDRAWL RATES FOR WELLS
!sb 020326 comment out JKZWEL and JKZWCE as no longer used
!INTEGER :: JKZWEL (NELEE), JKZWCE (NELEE, LLEE)  
!PRIVATE :: NELEE, LLEE, NVEE, NOLEE

CONTAINS

!> Allocates and zero-initializes column overlap arrays.
!>
!> The routine sizes overlap and lateral-transmissivity arrays from
!> `total_no_elements` and `top_cell_no`.
!>
!> @note This routine has no dummy arguments and mutates allocatable arrays in
!> `COLM_CG`.
!> @endnote
SUBROUTINE initialise_colm_cg()

!                             FACE OVERLAP AND LATERAL
!                             TRANSMISIVITY VALUES
! NOLBT and NOLCEA are in contaminant transport component the other variables only during the initialisation
   allocate   (JKZCOL(total_no_elements,2*top_cell_no+1,4),JOLFN(total_no_elements,2*top_cell_no+1,4))
   allocate   (NOL(total_no_elements,4))
   allocate   (NOLBT(total_no_elements,top_cell_no+1,4),NOLCE(total_no_elements,2*top_cell_no+1,4))
   allocate   (NOLCEA(total_no_elements,2*top_cell_no+1,4))
   JKZCOL=0
   JOLFN=0
   NOL=0
   NOLBT=0
   NOLCE=0
   NOLCEA=0
   
END SUBROUTINE initialise_colm_cg

!> Deallocates setup-only column overlap arrays.
!>
!> `NOLBT` and `NOLCEA` are retained for contaminant transport; the other
!> overlap arrays are used only during initialization and can be released.
SUBROUTINE deallocate_colm_cg()

! NOLBT and NOLCEA are in contaminant transport component the other variables only during the initialisation so can be deallocated
   deallocate (JKZCOL)
   deallocate (JOLFN)
   deallocate (NOL)
   deallocate (NOLCE)
   
END SUBROUTINE deallocate_colm_cg



END MODULE COLM_CG
