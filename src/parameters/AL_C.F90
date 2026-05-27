!> summary: Shared integrated flow, sediment, and contaminant state.
!> author: GP, Newcastle University; RJL, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> `AL_C` stores common state used across the integrated SHETRAN flow,
!> sediment, and contaminant components. It includes file-unit constants,
!> static element/link/soil/vegetation metadata, time-dependent hydrological
!> state, and allocatable arrays for VSS, soil layering, and root-density data.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-03 | GP | 3.0 | Original version written. |
!> | 1991-07 | GP | 3.1 | Moved variables from `AL_D`. |
!> | 1991-10 | GP | 3.2 | Added `IRRC`. |
!> | 1992-02 | RJL | 3.4 | Added `CMT` and `CMB`; moved `UZNOW` and `TIH` from `AL_D`. |
!> | 1993-07 | GP | 3.4 | Moved `NRD` from `AL_D` and `RDF` from `SPEC.ET`; added `ISPACK`, `SBERR`, and `WBERR`. |
!> | 1994-09-30 | RAH | 3.4.1 | Declared variables, removed `INTEGER*2`, tidied comments, and reorganized common blocks. |
!> | 1995-05-04 | GP | 4.0 | Added VSS variables and moved selected variables from `AL_D`. |
!> | 1997-02 | RAH | 4.1 | Retained `THSAT`, removed redundant variables, and swapped several array subscript conventions. |
!> | 1998-03 | RAH | 4.2 | Removed redundant VSS/OC variables. |
!> | 2004-07 | JE | - | Converted to Fortran 95. |
!> | 2026-03 | SB | 4.6 | Made selected VSS, soil-layer, and root-density arrays allocatable and added initializer routines. |
!> @endhistory
MODULE AL_C
USE SGLOBAL, ONLY : NELEE, LLEE, NLFEE, NVSEE, NXEE, NYEE, NSEDEE, NVEE, NLYREE, NSEE, top_cell_no, total_no_elements
IMPLICIT NONE

!

! ----- Static stuff
! File unit numbers. Values were historically assigned in FRINIT.
INTEGER, PARAMETER :: SFB = 9876   !! Sediment flow-boundary input unit placeholder.
INTEGER, PARAMETER :: SRB = 9877   !! Sediment rating/boundary input unit placeholder.
INTEGER, PARAMETER :: VSD = 11     !! VSS data-file unit.
INTEGER, PARAMETER :: SYD = 17     !! Sediment data-file unit.
INTEGER, PARAMETER :: CMD = 18     !! Contaminant data-file unit.
INTEGER, PARAMETER :: SPR = 24     !! General printed-output unit.
INTEGER, PARAMETER :: CMP = 25     !! Contaminant printed-output unit.
INTEGER, PARAMETER :: BUG = 26     !! Debug-output unit.
INTEGER, PARAMETER :: VSI = 29     !! VSS initial-condition file unit.
INTEGER, PARAMETER :: WLD = 31     !! Well data-file unit.
INTEGER, PARAMETER :: LFB = 32     !! VSS lateral-flow boundary file unit.
INTEGER, PARAMETER :: LHB = 33     !! VSS lateral-head boundary file unit.
INTEGER, PARAMETER :: LGB = 34     !! VSS lateral-gradient boundary file unit.
INTEGER, PARAMETER :: BFB = 35     !! VSS base-flow boundary file unit.
INTEGER, PARAMETER :: BHB = 36     !! VSS base-head boundary file unit.
INTEGER, PARAMETER :: CMT = 39     !! Contaminant time-series file unit.
INTEGER, PARAMETER :: CMB = 40     !! Contaminant boundary file unit.
INTEGER, PARAMETER :: MND = 53     !! Nitrate main data-file unit.
INTEGER, PARAMETER :: MNFC = 54    !! Nitrate carbon-addition file unit.
INTEGER, PARAMETER :: MNFN = 55    !! Nitrate nitrogen-addition file unit.
INTEGER, PARAMETER :: MNPL = 56    !! Nitrate plant-growth file unit.
INTEGER, PARAMETER :: MNPR = 57    !! Nitrate printed-output unit.
INTEGER, PARAMETER :: MNOUT1 = 58  !! Nitrate output file unit 1.
INTEGER, PARAMETER :: MNOUT2 = 59  !! Nitrate output file unit 2.
INTEGER, PARAMETER :: MNOUTPL = 60 !! Nitrate plant-output file unit.

DOUBLEPRECISION :: TIH !! Simulation start time as an absolute hour count.

! 2D plan state (NELEE)
INTEGER, DIMENSION(NELEE) :: NBFACE !! Boundary face number for boundary elements.
INTEGER, DIMENSION(NELEE) :: NLYR   !! Number of soil layers for each element.
INTEGER, DIMENSION(NELEE) :: NVC    !! Vegetation category for each element.
INTEGER, DIMENSION(NELEE) :: NWELBT !! Bottom VSS cell of each well screen.
INTEGER, DIMENSION(NELEE) :: NWELTP !! Top VSS cell of each well screen.
INTEGER, DIMENSION(NELEE) :: NVSWLT !! Well category or linked well source for each element.
INTEGER, DIMENSION(NELEE) :: NVSSPC !! VSS cell containing the spring source for each element.
INTEGER, DIMENSION(NELEE) :: NVSSPT !! Target element for water from the spring record.
INTEGER, DIMENSION(NELEE) :: NVSWLI !! Well element number for each well category.
DOUBLEPRECISION, DIMENSION(NELEE,4) :: DHF !! Distance from element node to each face.
LOGICAL, DIMENSION(NELEE) :: ISPACK        !! True when an element has a snowpack.

INTEGER, ALLOCATABLE :: JVSACN(:,:,:)       !! VSS adjacent-cell index by face, cell, and element.
INTEGER, ALLOCATABLE :: JVSDEL(:,:,:)       !! VSS split-cell connection indicator by face, cell, and element.
DOUBLEPRECISION, ALLOCATABLE :: DELTAZ(:,:) !! VSS cell thickness by cell and element.
DOUBLEPRECISION, ALLOCATABLE :: ZVSNOD(:,:) !! VSS node elevation by cell and element.

! Link state (NLFEE)
INTEGER, DIMENSION(NLFEE,2) :: ICMBK        !! Bank-element number by link and bank side.
INTEGER, DIMENSION(NLFEE,2) :: NHBED        !! VSS bed cell index below each channel link and bank side.
INTEGER, DIMENSION(NLFEE,6) :: ICMRF2       !! Multi-link confluence map: elements in columns 1:3 and faces in 4:6.
DOUBLEPRECISION, DIMENSION(NLFEE) :: CLENTH !! Channel link length.
DOUBLEPRECISION, DIMENSION(NLFEE) :: CWIDTH !! Channel link width.
DOUBLEPRECISION, DIMENSION(NLFEE) :: ZBEFF  !! Effective channel bed elevation.
DOUBLEPRECISION, DIMENSION(NLFEE) :: ZBFULL !! Bankfull channel elevation.
DOUBLEPRECISION, DIMENSION(NLFEE,2) :: FHBED !! Fractional bed-cell size below each channel link and bank side.
LOGICAL :: BEXBK                            !! True when explicit bank elements are present.
LOGICAL, DIMENSION(NLFEE) :: LINKNS         !! True when a link is aligned north-south.

! Vegetation state (NVEE)
INTEGER :: NV                             !! Number of vegetation types.
INTEGER, DIMENSION(NVEE) :: NRD           !! Number of UZ cells in the root zone by vegetation type.
DOUBLEPRECISION, DIMENSION(NVEE) :: RDL   !! Proportion of roots drawing water from the channel.
DOUBLEPRECISION, ALLOCATABLE :: RDF(:,:)  !! Root-density function by vegetation type and VSS cell.

! Soil and soil-layer state
INTEGER :: NS                              !! Number of soil types.
DOUBLEPRECISION, DIMENSION(NSEE) :: THSAT  !! Saturated moisture content by soil type.
DOUBLEPRECISION, DIMENSION(NSEE) :: VSPOR  !! VSS porosity by soil type.
INTEGER, ALLOCATABLE :: NLYRBT(:,:)        !! Bottom VSS cell number by element and soil layer.
INTEGER, ALLOCATABLE :: NTSOIL(:,:)        !! Soil type by element and soil layer.
DOUBLEPRECISION, ALLOCATABLE :: ZLYRBT(:,:) !! Bottom elevation by element and soil layer.

! ----- Time-dependent stuff
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM      !! Integer workspace for spatial input and category reads.
INTEGER, DIMENSION(NELEE) :: ISORT         !! Element solution/order list used by flow components.
INTEGER, DIMENSION(NELEE) :: NHSAT         !! Legacy saturation-state array; currently not used.
DOUBLEPRECISION, DIMENSION(NELEE) :: DRAINA !! Canopy-drip rainfall reaching the ground.
DOUBLEPRECISION, DIMENSION(NELEE) :: DUMMY  !! Floating-point workspace for spatial input and checks.
DOUBLEPRECISION, DIMENSION(NELEE) :: ESOILA !! Soil evaporation rate.
DOUBLEPRECISION, DIMENSION(NELEE) :: EEVAP  !! Actual evapotranspiration rate.
DOUBLEPRECISION, DIMENSION(NELEE) :: PNETTO !! Net precipitation/input rate available to the ground or surface water.
DOUBLEPRECISION, DIMENSION(NELEE) :: QH     !! Top vertical VSS flux by element.
DOUBLEPRECISION, DIMENSION(NELEE) :: WBERR  !! Cumulative water-balance error by element.
DOUBLEPRECISION, DIMENSION(NELEE) :: ZVSPSL !! VSS phreatic-surface elevation by element.
DOUBLEPRECISION, DIMENSION(NELEE) :: QVSBF  !! Bottom VSS flux by element.
DOUBLEPRECISION, DIMENSION(NELEE) :: QVSSPR !! VSS spring discharge by element.
DOUBLEPRECISION, DIMENSION(NELEE) :: QVSWEL !! VSS well abstraction or recharge by element.
DOUBLEPRECISION, DIMENSION(NELEE,4) :: QOC  !! Overland/channel face flow by element and face.
DOUBLEPRECISION, ALLOCATABLE :: QVSV(:,:)   !! Vertical VSS flux by cell and element.
DOUBLEPRECISION, ALLOCATABLE :: VSPSI(:,:)  !! VSS pressure head by cell and element.
DOUBLEPRECISION, ALLOCATABLE :: VSTHE(:,:)  !! VSS volumetric water content by cell and element.
DOUBLEPRECISION, ALLOCATABLE :: QVSWLI(:,:) !! Well flux by VSS cell and well element.
DOUBLEPRECISION, ALLOCATABLE :: ERUZ(:,:)   !! Root-zone extraction by element and VSS cell.
DOUBLEPRECISION, ALLOCATABLE :: QVSH(:,:,:) !! Lateral VSS flux by face, cell, and element.

DOUBLEPRECISION, DIMENSION(NLFEE) :: ARXL    !! Channel cross-sectional flow area by link.
DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKB  !! Bank-to-link surface exchange by link and bank side.
DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKF  !! Bank/grid lateral VSS exchange by link and bank side.
DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKI  !! Dry-bank exchange component by link and bank side.

DOUBLEPRECISION, DIMENSION(NVEE) :: CLAI     !! Current canopy leaf-area index by vegetation type.
DOUBLEPRECISION, DIMENSION(NVEE) :: PLAI     !! Proportion of maximum seasonal ground cover by vegetation type.

DOUBLEPRECISION, DIMENSION(NELEE,NSEDEE) :: SBERR !! Sediment balance error by element and size fraction.

DOUBLEPRECISION :: DTUZ   !! Current unsaturated-zone timestep in seconds.
DOUBLEPRECISION :: UZNEXT !! Current unsaturated-zone timestep in hours.
!PRIVATE :: NELEE, LLEE, NLFEE, NVSEE, NXEE, NYEE, NSEDEE, NVEE, NLYREE, NSEE


CONTAINS

!> Allocates and zero-initializes VSS state arrays.
!>
!> The allocation uses `top_cell_no` and `total_no_elements` for the active
!> model dimensions. Call after those dimensions have been set and before any
!> of `QVSH`, `QVSV`, `VSPSI`, `VSTHE`, `QVSWLI`, `ERUZ`, `JVSACN`, or
!> `JVSDEL` has already been allocated.
SUBROUTINE initialise_al_c()

ALLOCATE(qvsh(4,top_cell_no,total_no_elements), qvsv(top_cell_no,total_no_elements), &
         vspsi(top_cell_no,total_no_elements), vsthe(top_cell_no,total_no_elements), &
         qvswli(top_cell_no,total_no_elements), eruz(total_no_elements,top_cell_no))
ALLOCATE (JVSACN(4,top_cell_no,total_no_elements), JVSDEL(4,top_cell_no,total_no_elements)) 

         qvsh=0.0d0
         qvsv=0.0d0
         vspsi=0.0d0
         vsthe=0.0d0
         qvswli=0.0d0
         eruz=0.0d0
         JVSACN=0
         JVSDEL=0

END SUBROUTINE initialise_al_c

!> Allocates and zero-initializes soil-layer geometry arrays.
!>
!> This routine allocates cell thicknesses, VSS node elevations, bottom-cell
!> indices, soil-type indices, and soil-layer bottom elevations. Call after
!> `total_no_elements` has been set and before these arrays have already been
!> allocated.
SUBROUTINE initialise_al_c2()

ALLOCATE (DELTAZ(LLEE,total_no_elements), ZVSNOD(LLEE,total_no_elements)) 
ALLOCATE (NLYRBT(total_no_elements,NLYREE), NTSOIL(total_no_elements,NLYREE)) 
ALLOCATE (ZLYRBT(total_no_elements,NLYREE)) 
         DELTAZ=0.0d0
         ZVSNOD=0.0d0
         NLYRBT=0
         NTSOIL=0
         ZLYRBT=0.0d0


END SUBROUTINE initialise_al_c2

!> Allocates and zero-initializes the vegetation root-density function array.
!>
!> The first dimension uses the configured number of vegetation types, `NV`.
!> Call after `NV` has been read and before `RDF` has already been allocated.
SUBROUTINE initialise_al_c3()

ALLOCATE (RDF(NV,LLEE))
         RDF=0.0d0

END SUBROUTINE initialise_al_c3



END MODULE AL_C
