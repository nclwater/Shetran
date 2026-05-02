MODULE CONT_CC
   USE SGLOBAL, ONLY : NELEE, NCONEE, LLEE, NSEE, NSEDEE, NLFEE, total_no_elements,top_cell_no,total_no_links
   IMPLICIT NONE
!*------------------------------ Start of CONT.CC ----------------------*
!*
!*                       INCLUDE FILE FOR CONTAMIANT VARIABLES AND DATA
!*
!*----------------------------------------------------------------------*
!* Version:  SHETRAN/INCLUDE/CONT.CC/4.1
!* Modifications:
!*                           JE     26/4/91   3.1     WRITTEN
!*                           JE     13/6/91   3.1     COMPLETED
!*                           JE     18/6/91   3.1     BLOCK WELC ADDED
!* RAH  970224  4.1  Explicit typing.
!  JE  NOV 04 ---- Convert to FORTRAN 95
! SB    Mar26   4.6 added total_no_elements,top_cell_no,total_no_links to subroutine
!                    make the following allocatable CCCC, CCCCO, SSSS, SSSSO, SSS1, SSS2, FCPBKO, GCPBKO
!                    Add initialise_cont_cc() where these arrays are allocated
!*----------------------------------------------------------------------*
!* Imported constants
!*                      LLEE,NCONEE,NELEE,NLFEE,NSEE
!* Commons
   DOUBLEPRECISION CCAPB(NELEE,NCONEE),CCPBO(NELEE,NCONEE)
   DOUBLEPRECISION CCAPE(NELEE,NCONEE)
   DOUBLEPRECISION CCAPI(NCONEE),      CCAPIO(NCONEE)
   DOUBLEPRECISION CCAPR(NELEE,NCONEE),CCAPRO(NELEE,NCONEE)
   DOUBLEPRECISION IIICF(NCONEE),      IIICFO(NCONEE)
!      COMMON/  CBDY   /CCAPB,CCPBO,CCAPE,CCAPI,CCAPIO,CCAPR,CCAPRO,
!     $                 IIICF,IIICFO
!*                             CONTAMINANT CONCENTRATION AND
!*                             FLUX BOUNDARY CONDITIONS

   DOUBLEPRECISION CCCCW(NELEE,NCONEE)
!      COMMON/  WELC   /CCCCW
!*                             CONTAMINANT CONCENTRATION IN WELL WATER

   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: CCCC, CCCCO
   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: SSSS, SSSSO
!     DOUBLEPRECISION CCCC(NELEE,LLEE,NCONEE),CCCCO(NELEE,LLEE,NCONEE)
!      DOUBLEPRECISION SSSS(NELEE,LLEE,NCONEE),SSSSO(NELEE,LLEE,NCONEE)
!      COMMON/  CONC   /CCCC,CCCCO,SSSS,SSSSO
!*                             CONCENTRATIONS WITHIN CATCHMENT

   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: SSS1, SSS2
!     DOUBLEPRECISION SSS1(NELEE,LLEE,NCONEE), SSS2(NELEE,LLEE,NCONEE)
!                           SOURCE/SINK TERMS FOR PLANT UPTAKE AND NITRATE


   DOUBLEPRECISION GCPLA(NCONEE),GGLMSO(NCONEE)
!      COMMON/  GEN    /GCPLA,GGLMSO
!*                             CONTAMINANT DECAY RATES

   DOUBLEPRECISION CCAPIN(NCONEE)
!      COMMON/  INIT   /CCAPIN
!*                             INITIAL CONCENTRATION

   DOUBLEPRECISION ALPHA(NSEE,NCONEE),FADS(NSEE,NCONEE)
   DOUBLEPRECISION GNN(NCONEE)
   DOUBLEPRECISION KDDLS(NSEDEE,NCONEE)
   DOUBLEPRECISION KDDSOL(NSEE,NCONEE)
!      COMMON/  NNNN   /ALPHA,FADS,GNN,KDDLS,KDDSOL
!*                             CONTAMINANT PROPERTIES FOR SOIL
!*                             AND SEDIMENT

   INTEGER          NCON
!      COMMON/  NCONS  /NCON
!*                             NUMBER OF CONTAMINANTS


<<<<<<< HEAD
   DOUBLEPRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: FCPBKO
   DOUBLEPRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: GCPBKO
!      DOUBLEPRECISION FCPBKO(NLFEE,2,LLEE,NCONEE)
!      DOUBLEPRECISION GCPBKO(NLFEE,2,LLEE,NCONEE)
   DOUBLEPRECISION    FSF(NLFEE,NCONEE),       FSFC(NLFEE,NCONEE)
   DOUBLEPRECISION   FSFT(NLFEE,NCONEE),        RSW(NELEE,NCONEE)
   DOUBLEPRECISION   RSWC(NELEE,NCONEE),       RSWT(NELEE,NCONEE)
=======
      DOUBLEPRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: FCPBKO
      DOUBLEPRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: GCPBKO
      DOUBLEPRECISION    FSF(NLFEE,NCONEE),       FSFC(NLFEE,NCONEE)
      DOUBLEPRECISION   FSFT(NLFEE,NCONEE),        RSW(NELEE,NCONEE)
      DOUBLEPRECISION   RSWC(NELEE,NCONEE),       RSWT(NELEE,NCONEE)
>>>>>>> 08f5db2c06d27ba1583117178dc681485ed0f215
!      COMMON/  RETN   /FCPBKO,GCPBKO,FSF,FSFC,FSFT,RSW,RSWC,RSWT
!*                             RETARDATION VARIABLES USED IN THE
!*                             CALCULATIONS FOR IMPLICIT LATERAL
!*                             COUPLING BY BANK EROSION AND WITH
!*                             CONVECTION WITH SURFACE FLOWS

   DOUBLEPRECISION ALPHBD(NCONEE),ALPHBS(NCONEE)
!      COMMON/  SBED   /ALPHBD,ALPHBS
!*                             COEFFICIENTS FOR EXCHANGE BETWEEN CELLS
!*                             OF A LINK

!PRIVATE :: NELEE, NCONEE, LLEE, NSEE, NSEDEE, NLFEE

CONTAINS

   SUBROUTINE initialise_cont_cc()

      allocate   (cccc(total_no_elements,top_cell_no+1,ncon),cccco(total_no_elements,top_cell_no+1,ncon))
      allocate   (ssss(total_no_elements,top_cell_no+1,ncon),sssso(total_no_elements,top_cell_no+1,ncon))
      allocate   (sss1(total_no_elements,top_cell_no+1,ncon),sss2(total_no_elements,top_cell_no+1,ncon))
      allocate   (FCPBKO(total_no_links,2,top_cell_no+1,ncon))
      allocate   (GCPBKO(total_no_links,2,top_cell_no+1,ncon))
      cccc=0
      cccco=0
      ssss=0
      sssso=0
      sss1=0
      sss2=0
      FCPBKO=0
      GCPBKO=0

   END SUBROUTINE initialise_cont_cc


END MODULE CONT_CC
