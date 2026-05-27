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
USE SGLOBAL, ONLY : NELEE, NCONEE, NSEE, NSEDEE, NLFEE, total_no_elements, &
                    top_cell_no, total_no_links
IMPLICIT NONE

DOUBLEPRECISION :: CCAPB(NELEE, NCONEE)  !! Prescribed base-cell concentration by element and contaminant.
DOUBLEPRECISION :: CCPBO(NELEE, NCONEE)  !! Previous prescribed base-cell concentration by element and contaminant.
DOUBLEPRECISION :: CCAPE(NELEE, NCONEE)  !! External-flow boundary concentration by element and contaminant.
DOUBLEPRECISION :: CCAPI(NCONEE)         !! Current rainfall concentration by contaminant.
DOUBLEPRECISION :: CCAPIO(NCONEE)        !! Previous rainfall concentration by contaminant.
DOUBLEPRECISION :: CCAPR(NELEE, NCONEE)  !! Base-flux concentration by element and contaminant.
DOUBLEPRECISION :: CCAPRO(NELEE, NCONEE) !! Previous base-flux concentration by element and contaminant.
DOUBLEPRECISION :: IIICF(NCONEE)         !! Current dry-deposition rate by contaminant.
DOUBLEPRECISION :: IIICFO(NCONEE)        !! Previous dry-deposition rate by contaminant.

DOUBLEPRECISION :: CCCCW(NELEE, NCONEE)  !! Contaminant concentration in well water by element and contaminant.

DOUBLEPRECISION, ALLOCATABLE :: CCCC(:,:,:)  !! Current dynamic-region concentration by element, cell, and contaminant.
DOUBLEPRECISION, ALLOCATABLE :: CCCCO(:,:,:) !! Previous dynamic-region concentration by element, cell, and contaminant.
DOUBLEPRECISION, ALLOCATABLE :: SSSS(:,:,:)  !! Current dead-space/sorbed concentration by element, cell, and contaminant.
DOUBLEPRECISION, ALLOCATABLE :: SSSSO(:,:,:) !! Previous dead-space/sorbed concentration by element, cell, and contaminant.

DOUBLEPRECISION, ALLOCATABLE :: SSS1(:,:,:)  !! Mobile-phase source/sink term for plant uptake and nitrate.
DOUBLEPRECISION, ALLOCATABLE :: SSS2(:,:,:)  !! Dead-space/sorbed source/sink term for plant uptake and nitrate.

DOUBLEPRECISION :: GCPLA(NCONEE)  !! Scaled contaminant generation/decay coefficient by contaminant.
DOUBLEPRECISION :: GGLMSO(NCONEE) !! Input first-order generation/decay coefficient by contaminant.

DOUBLEPRECISION :: CCAPIN(NCONEE) !! Initial concentration by contaminant.

DOUBLEPRECISION :: ALPHA(NSEE, NCONEE)  !! Soil-region exchange coefficient by soil type and contaminant.
DOUBLEPRECISION :: FADS(NSEE, NCONEE)   !! Equilibrium-adsorption fraction by soil type and contaminant.
DOUBLEPRECISION :: GNN(NCONEE)          !! Freundlich exponent by contaminant.
DOUBLEPRECISION :: KDDLS(NSEDEE, NCONEE) !! Loose-sediment distribution coefficient by sediment and contaminant.
DOUBLEPRECISION :: KDDSOL(NSEE, NCONEE) !! Soil distribution coefficient by soil type and contaminant.

INTEGER :: NCON !! Number of active contaminants.

DOUBLEPRECISION, ALLOCATABLE :: FCPBKO(:,:,:,:) !! Previous bank mobile-region retardation factor.
DOUBLEPRECISION, ALLOCATABLE :: GCPBKO(:,:,:,:) !! Previous bank dead-space retardation factor.
DOUBLEPRECISION :: FSF(NLFEE, NCONEE)  !! Stream-water retardation factor by link and contaminant.
DOUBLEPRECISION :: FSFC(NLFEE, NCONEE) !! Concentration derivative of `FSF`.
DOUBLEPRECISION :: FSFT(NLFEE, NCONEE) !! Time derivative of `FSF`.
DOUBLEPRECISION :: RSW(NELEE, NCONEE)  !! Surface-water retardation factor by element and contaminant.
DOUBLEPRECISION :: RSWC(NELEE, NCONEE) !! Concentration derivative of `RSW`.
DOUBLEPRECISION :: RSWT(NELEE, NCONEE) !! Time derivative of `RSW`.

DOUBLEPRECISION :: ALPHBD(NCONEE) !! Exchange coefficient between channel bed layers.
DOUBLEPRECISION :: ALPHBS(NCONEE) !! Exchange coefficient between stream water and bed surface.

!PRIVATE :: NELEE, NCONEE, NSEE, NSEDEE, NLFEE

CONTAINS

!> Allocates and zero-initializes contaminant concentration and retardation arrays.
!>
!> The allocation uses the active model dimensions and the configured number of
!> contaminants (`NCON`) instead of the fixed maximum dimensions used by the
!> original common-block implementation.
!>
!> Entry assumptions:
!>
!> | Assumption | Reason |
!> |:-----------|:-------|
!> | `total_no_elements`, `total_no_links`, `top_cell_no`, and `NCON` are set. | They define every allocated extent. |
!> | The allocatable concentration/source arrays are not already allocated. | The routine allocates unconditionally. |
!>
!> @note This routine has no dummy arguments and mutates allocatable module
!> arrays in `CONT_CC`.
!> @endnote
SUBROUTINE initialise_cont_cc()

   allocate (CCCC(total_no_elements, top_cell_no+1, NCON))
   allocate (CCCCO(total_no_elements, top_cell_no+1, NCON))
   allocate (SSSS(total_no_elements, top_cell_no+1, NCON))
   allocate (SSSSO(total_no_elements, top_cell_no+1, NCON))
   allocate (SSS1(total_no_elements, top_cell_no+1, NCON))
   allocate (SSS2(total_no_elements, top_cell_no+1, NCON))
   allocate (FCPBKO(total_no_links, 2, top_cell_no+1, NCON))
   allocate (GCPBKO(total_no_links, 2, top_cell_no+1, NCON))
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
