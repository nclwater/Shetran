MODULE COLM_CO
!-------------------------- Start of COLM.CO --------------------------*
!
!         CM-COMPONENT INCLUDE-FILE FOR WATER VARIABLES USED IN
!                      THE PREPARATION FOR RUNNING SUBROUTINE COLM
!                      BUT NOT USED IN COLM ITSELF
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/COLM.CO/4.2
! Modifications:
!                          JE     26/4/91   3.1     WRITTEN
!                          JE     16/6/91   3.1     COMPLETED
!  GP  940808  4.0  Replace TH3O with VSTHEO (see INCM,COLMW).
! RAH  970220  4.1  Explicit typing.
! RAH  980308  4.2  Remove WELDRO.
!      981103       Remove ERUZO (see INCM,COLMW).
! JE  12/08   4.3.5F90  Convert to FORTRAN90
! SB Mar26  4.6     Make the following arrays allocatable: DSWO, QIO, QQRFO, RSZWLO, ZONEO, GGAMMO,QQQSWO, QQO, UUAJPO,VSTHEO
!                   Add subroutine initialise_colm_co
!----------------------------------------------------------------------*

! Imported constants
!                             NELEE,LLEE

! Commons
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
