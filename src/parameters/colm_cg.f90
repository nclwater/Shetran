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
!> | 2026-03 | SB | 4.6 | Removed unused legacy arrays; made overlap arrays allocatable; added allocation helpers. |
!> @endhistory
MODULE COLM_CG
USE SGLOBAL, ONLY : NELEE, LLEE, total_no_elements, top_cell_no
IMPLICIT NONE

INTEGER :: JBTLYR(NELEE)  !! Legacy bottom soil-layer index for each element; currently retained but unused.
INTEGER :: NCOLMB(NELEE)  !! Bottom active contaminant cell for each soil column.

DOUBLEPRECISION :: ZCOLMB(NELEE) !! Elevation of the base node of each soil column.
DOUBLEPRECISION :: SCL           !! Scale factor for the legacy 32500 overlap-fraction encoding.
DOUBLEPRECISION :: OODO          !! Reciprocal reference diffusion scale, `1/D0`.

INTEGER, ALLOCATABLE :: JKZCOL(:,:,:) !! Integer lateral-transmissivity adjustment by element, overlap, and face.
INTEGER, ALLOCATABLE :: JOLFN(:,:,:)  !! Overlap fraction by element, overlap, and face on the 32500 scale.
INTEGER, ALLOCATABLE :: NOL(:,:)      !! Number of lateral overlap records by element and face.
INTEGER, ALLOCATABLE :: NOLBT(:,:,:)  !! First overlap record for each element, cell, and face.
INTEGER, ALLOCATABLE :: NOLCE(:,:,:)  !! Local cell number for each overlap record.
INTEGER, ALLOCATABLE :: NOLCEA(:,:,:) !! Adjacent cell number for each overlap record.

DOUBLEPRECISION :: WELDRA(LLEE) !! Well withdrawal rate mapped to each active column cell.
!PRIVATE :: NELEE, LLEE

CONTAINS

!> Allocates and zero-initializes column overlap arrays.
!>
!> The routine sizes overlap and lateral-transmissivity arrays from
!> `total_no_elements` and `top_cell_no`.
!>
!> Entry assumptions:
!>
!> | Assumption | Reason |
!> |:-----------|:-------|
!> | `total_no_elements` and `top_cell_no` are set. | They define every allocated extent. |
!> | The allocatable overlap arrays are not already allocated. | The routine allocates unconditionally. |
!>
!> @note This routine has no dummy arguments and mutates allocatable arrays in
!> `COLM_CG`.
!> @endnote
SUBROUTINE initialise_colm_cg()

   allocate (JKZCOL(total_no_elements, 2*top_cell_no+1, 4))
   allocate (JOLFN(total_no_elements, 2*top_cell_no+1, 4))
   allocate (NOL(total_no_elements, 4))
   allocate (NOLBT(total_no_elements, top_cell_no+1, 4))
   allocate (NOLCE(total_no_elements, 2*top_cell_no+1, 4))
   allocate (NOLCEA(total_no_elements, 2*top_cell_no+1, 4))
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
!>
!> Entry assumptions:
!>
!> | Assumption | Reason |
!> |:-----------|:-------|
!> | `initialise_colm_cg` has already allocated the setup-only arrays. | The routine deallocates unconditionally. |
SUBROUTINE deallocate_colm_cg()

   deallocate (JKZCOL)
   deallocate (JOLFN)
   deallocate (NOL)
   deallocate (NOLCE)

END SUBROUTINE deallocate_colm_cg



END MODULE COLM_CG
