!> summary: Contaminant component state arrays and properties.
!> author: JE, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> This module replaces the legacy `CONT.CC` common blocks. It stores
!> contaminant concentrations, boundary conditions, decay and adsorption
!> parameters, retardation variables, and exchange coefficients used by the
!> contaminant transport component.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.1 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1991-06-18 | JE | 3.1 | Added `WELC` block. |
!> | 1997-02-24 | RAH | 4.1 | Added explicit typing. |
!> | 2004-11 | JE | - | Converted to Fortran 95. |
!> | 2026-03 | SB | 4.6 | Made major concentration/source arrays allocatable and added `initialise_cont_cc`. |
!> @endhistory
MODULE CONT_CC
USE SGLOBAL, ONLY : NELEE, NCONEE, LLEE, NSEE, NSEDEE, NLFEE, total_no_elements,top_cell_no,total_no_links
IMPLICIT NONE
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
!      COMMON/  CONC   /CCCC,CCCCO,SSSS,SSSSO
!*                             CONCENTRATIONS WITHIN CATCHMENT

      DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: SSS1, SSS2
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


      DOUBLEPRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: FCPBKO
      DOUBLEPRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: GCPBKO
      DOUBLEPRECISION    FSF(NLFEE,NCONEE),       FSFC(NLFEE,NCONEE)
      DOUBLEPRECISION   FSFT(NLFEE,NCONEE),        RSW(NELEE,NCONEE)
      DOUBLEPRECISION   RSWC(NELEE,NCONEE),       RSWT(NELEE,NCONEE)
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

!> Allocates and zero-initializes contaminant concentration and retardation arrays.
!>
!> The allocation uses the active model dimensions and the configured number of
!> contaminants (`NCON`) instead of the fixed maximum dimensions used by the
!> original common-block implementation.
!>
!> @note This routine has no dummy arguments and mutates allocatable module
!> arrays in `CONT_CC`.
!> @endnote
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
