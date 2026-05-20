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
!File unit numbers
!    INTEGER :: PRI,WLD,SYD,SPR,SFB,SRB,BUG,CMD,CMP,CMT,CMB, VSD,VSI,LFB,LHB,LGB,BFB,BHB
INTEGER, PARAMETER :: &   !VALUES USED TO BE SET IN FRINIT 
SFB=9876, & !NO VALUE WAS ALLOCATED TO THIS
SRB=9877, & !NO VALUE WAS ALLOCATED TO THIS
!FRD = 10, &
VSD = 11, &
!OCD = 12, &
!ETD = 13, &
!PPD = 14, &
!SMD = 15, &
!BKD = 16, &
SYD = 17, &
CMD = 18, &
!MED = 19, &
!PRD = 20, &
!EPD = 21, &
!TIM = 22, &
!PRI = 23, & now in sglobal
SPR = 24, &
CMP = 25, &
BUG = 26, &
!RES = 27, &
!HOT = 28, &
VSI = 29, &
!VED = 30, &
WLD = 31, &
LFB = 32, &
LHB = 33, &
LGB = 34, &
BFB = 35, &
BHB = 36, &
!OFB = 37, &
!OHB = 38, &
CMT = 39, &
CMB = 40, &
!DIS = 41, &
!VSE = 42, &
!MAS = 43, &
!dis2 = 44
MND = 53 , &
MNFC = 54 , &
MNFN = 55 , &
MNPL = 56 , &
MNPR = 57 , &
MNOUT1 =58 , &
MNOUT2 = 59 , &
MNOUTPL = 60
   
!????????????
    DOUBLEPRECISION TIH

!2D PLAN(NELEE)
    INTEGER, DIMENSION(NELEE)   :: NBFACE,     &  !no. of boundary face
                                   NLYR,       &  !no. of layers
                                   NVC,        &  !vegetation category for each element
                                   NWELBT, NWELTP, NVSWLT, & !well number bottom and top layers of screen, well categories
                                   NVSSPC,     &  !spring source element ???
                                   NVSSPT,     &  !Target element for water from spring VS13a
                                   NVSWLI    !well element numbers
    DOUBLEPRECISION, DIMENSION(NELEE,4) :: DHF            !distance from node to face
    LOGICAL, DIMENSION(NELEE)   :: ISPACK         !is there a snow pack?

    INTEGER, DIMENSION(:,:,:) , ALLOCATABLE :: JVSACN,JVSDEL
    DOUBLEPRECISION, DIMENSION(:,:)  , ALLOCATABLE :: DELTAZ, ZVSNOD   
!                                       DELTAZ  cell thickness, ZVSNOD   node elevations
    

!LINK (NLFEE)
    INTEGER,DIMENSION(NLFEE,2)  :: ICMBK, NHBED
    INTEGER,DIMENSION(NLFEE,6)  :: ICMRF2            !for link branching
    DOUBLEPRECISION, DIMENSION(NLFEE)   :: CLENTH, CWIDTH, & !lenghth and width of link
                                   ZBEFF,ZBFULL      !elevation of bed and bank full
    DOUBLEPRECISION, DIMENSION(NLFEE,2) :: FHBED             !cell sizes under channel link
    LOGICAL                     :: BEXBK             !are there banks?
    LOGICAL, DIMENSION(NLFEE)   :: LINKNS            !does link run NS ?

!VEGETATION (NVEE)
    INTEGER                       :: NV  !no. of vegetation types
    INTEGER, DIMENSION(NVEE)      :: NRD !no. of UZ cells in root zone
    DOUBLEPRECISION, DIMENSION(NVEE)      :: RDL !proportion of roots that take water from the channel
    DOUBLEPRECISION, DIMENSION(:,:)  , ALLOCATABLE ::  RDF !root density function

!SOIL (NSEE)
    INTEGER                          :: NS  !no. of soil types
    DOUBLEPRECISION, DIMENSION(NSEE)         :: THSAT, VSPOR !saturated m/c and porosity
!SOIL LAYERS (NLYREE)
    INTEGER, DIMENSION(:,:) , ALLOCATABLE :: NLYRBT,NTSOIL  !bottom cell no in each soil layer and soil type in soil layer
    DOUBLEPRECISION, DIMENSION(:,:) , ALLOCATABLE :: ZLYRBT  !elevatuion of bottom of soil layer



!----- Time-dependent stuff
!?????????
    !2D PLAN(NELEE)
    INTEGER, DIMENSION(NXEE*NYEE)   :: IDUM
    !INTEGER, DIMENSION(NELEE)   :: IDUM,ISORT, & !
    INTEGER, DIMENSION(NELEE)   :: ISORT, & !
                                   NHSAT !not used ????
    DOUBLEPRECISION, DIMENSION(NELEE)   :: DRAINA, & !
                                   DUMMY,  & !
                                   ESOILA, & !
                                   EEVAP,  & !
                                   PNETTO, & !
                                   QH,     & ! 
                                   WBERR,  & !
                                   ZVSPSL, & !phreatic surface level?
                                   QVSBF,  & !
                                   QVSSPR, & !
                                   QVSWEL
    DOUBLEPRECISION, DIMENSION(NELEE,4) :: QOC  !overalnd flow through face  (inflow or outflow???)
!3D (LLEE)
    !DOUBLEPRECISION, DIMENSION(NELEE,LLEE)   :: ERUZ     !transpiration ??>>
    !DOUBLEPRECISION, DIMENSION(LLEE,NELEE)   :: QVSV,  & !vertical flow
    !                                    VSPSI, & !psi
    !                                    VSTHE    !theta
    !DOUBLEPRECISION, DIMENSION(4,LLEE,NELEE) :: QVSH     !cell horiziontal flow ????
    DOUBLEPRECISION, DIMENSION(:,:)  , ALLOCATABLE :: QVSV, VSPSI, VSTHE, QVSWLI, ERUZ
    DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: QVSH

!LINK (NLFEE)
    DOUBLEPRECISION, DIMENSION(NLFEE)   :: ARXL  !cross-sectional area of flow????
    DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKB, QBKF, QBKI  !bank flows

!VEGETATION (NVEE)
    DOUBLEPRECISION, DIMENSION(NVEE) :: CLAI, & !canopy leaf area index
                                PLAI    !proportion of ground cover at maximum seasonal extent

!SEDIMENT SIZE FRACTIONS (NSEDEE)
    DOUBLEPRECISION, DIMENSION(NELEE,NSEDEE) :: SBERR

!VSS STUFF (NVSEE)
    !DOUBLEPRECISION, DIMENSION(LLEE,NVSEE) :: QVSWLI

!UZ STUFF
    DOUBLEPRECISION DTUZ,UZNEXT
!PRIVATE :: NELEE, LLEE, NLFEE, NVSEE, NXEE, NYEE, NSEDEE, NVEE, NLYREE, NSEE


CONTAINS

!> Allocates and zero-initializes VSS state arrays.
!>
!> The allocation uses `top_cell_no` and `total_no_elements` for the active
!> model dimensions.
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
!> indices, soil-type indices, and soil-layer bottom elevations.
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
SUBROUTINE initialise_al_c3()

ALLOCATE (RDF(NV,LLEE))
         RDF=0.0d0

END SUBROUTINE initialise_al_c3



END MODULE AL_C
