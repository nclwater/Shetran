MODULE COLM_CG
!---------------------------- Start of COLM.CG ------------------------*
!
!                      INCLUDE FILE FOR WATER VARIABLES USED IN
!                      THE PREPARATION FOR RUNNING SUBROUTINE COLM
!                      BUT NOT USED IN SUBROUTINE COLM
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/COLM.CG/4.1
! Modifications:
!                          JE     26/4/91   3.0     WRITTEN
!                          JE     13/6/91   3.1     CHECKED, TIDIED TEXT
!                          JE     16/7/91   3.1     REORDERED NAMES IN
!                                                   WELPRO
! RAH  970224  4.1  Explicit typing.
!                   Separate /WELPRI/ from mixed-type /WELPRO/.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Imported constants
!                      LLEE,NELEE,NOLEE,NVEE
! Commons
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

   SUBROUTINE deallocate_colm_cg()

! NOLBT and NOLCEA are in contaminant transport component the other variables only during the initialisation so can be deallocated
      deallocate (JKZCOL)
      deallocate (JOLFN)
      deallocate (NOL)
      deallocate (NOLCE)

   END SUBROUTINE deallocate_colm_cg



END MODULE COLM_CG
