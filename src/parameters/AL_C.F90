!> summary: Defines common variables for integrated flow, sediment, and contaminant components.
!> author: G. Parkin, R.J.L., R. A. Heath, J. Ewen (Newcastle University)
!> date: 2004-07-01
!>
!> This module contains a large collection of global variables and arrays
!> that are shared across different components of the SHETRAN model,
!> particularly for the integrated flow, sediment, and contaminant transport
!> simulations. It includes file unit numbers, static grid and link properties,
!> and time-dependent simulation arrays. This module is a legacy component
!> derived from an old `INCLUDE` file.
!>
!> @note The refactoring plan in `docs/reports/refactor_parameters/plan_parameter_refactor.md`
!> outlines splitting this module into more focused ones, such as `file_units.f90`,
!> `global_arrays.f90`, and `vegetation_arrays.f90`.
!>
!> @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | 1991-03 | GP | v3.0: Written. |
!> | 1991-07 | GP | v3.1: Variables moved from AL.D. |
!> | 1991-10 | GP | v3.2: Added IRRC. |
!> | 1992-02 | RJL | v3.4: Added CMT, CMB. Moved UZNOW, TIH from AL.D. |
!> | 1993-07 | GP | v3.4: Moved NRD from AL.D; RDF from SPEC.ET. Added ISPACK, ZOCMLN, SBERR, WBERR. |
!> | 1994-09-30 | RAH | v3.4.1: Modernized declarations, removed INTEGER*2, standard header, added SFB, SRB. |
!> | 1995-05-04 | GP | v4.0: Brought variables from AL.D. Added new VSS variables. Replaced CPR with CMP. Removed several variables. |
!> | 1997-02-12 | RAH | v4.1: Retained THSAT for SY. Removed BPRNOW. |
!> | 1997-02-13 | RAH | v4.1: Removed NVSCIT. |
!> | 1997-02-14 | RAH | v4.1: Swapped subscripts on QVSH, DELTAZ. |
!> | 1997-02-17 | RAH | v4.1: Swapped subscripts on JVSACN, JVSDEL, ZVSNOD, QVSV, QVSWLI, VSPSI, VSTHE. Removed QVSL. |
!> | 1997-02-20 | RAH | v4.1: Restored history. |
!> | 1998-03-02 | RAH | v4.2: Removed ZOCMLN, VSSTMP, FHSAT. |
!> | 1998-03-07 | RAH | v4.2: Amended line above. |
!> | 2004-07-01 | JE | Converted to Fortran 95. |
!> | 2024-09-05 | Gemini | Converted documentation to FORD format. |
MODULE AL_C
   USE SGLOBAL, ONLY : NELEE, LLEE, NLFEE, NVSEE, NXEE, NYEE, NSEDEE, NVEE, &
      NLYREE, NSEE, top_cell_no, total_no_elements

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SFB, SRB, VSD, SYD, CMD, SPR, CMP, BUG, VSI, WLD, LFB, LHB, &
      LGB, BFB, BHB, CMT, CMB
   PUBLIC :: TIH
   PUBLIC :: NBFACE, NLYR, NVC, NWELBT, NWELTP, NVSWLT, NVSSPC, NVSSPT, &
      NVSWLI, DHF, ISPACK
   PUBLIC :: JVSACN, JVSDEL, DELTAZ, ZVSNOD
   PUBLIC :: ICMBK, NHBED, ICMRF2, CLENTH, CWIDTH, ZBEFF, ZBFULL, FHBED, &
      BEXBK, LINKNS
   PUBLIC :: NV, NRD, RDL, RDF
   PUBLIC :: NS, THSAT, VSPOR
   PUBLIC :: NLYRBT, NTSOIL, ZLYRBT
   PUBLIC :: IDUM, ISORT, NHSAT, DRAINA, DUMMY, ESOILA, EEVAP, PNETTO, QH, &
      WBERR, ZVSPSL, QVSBF, QVSSPR, QVSWEL, QOC
   PUBLIC :: QVSV, VSPSI, VSTHE, QVSWLI, ERUZ, QVSH
   PUBLIC :: ARXL, QBKB, QBKF, QBKI
   PUBLIC :: CLAI, PLAI
   PUBLIC :: SBERR
   PUBLIC :: DTUZ, UZNEXT
   PUBLIC :: initialise_al_c

   !-----------------------------------------------------------------------
   ! File Unit Numbers
   !-----------------------------------------------------------------------
   ! VALUES USED TO BE SET IN FRINIT
   INTEGER, PARAMETER :: SFB = 9876 !! Sediment flow binary output (unallocated).
   INTEGER, PARAMETER :: SRB = 9877 !! Sediment results binary output (unallocated).
   ! INTEGER, PARAMETER :: FRD = 10
   INTEGER, PARAMETER :: VSD = 11 !! Vadose-saturated zone input data.
   ! INTEGER, PARAMETER :: OCD = 12
   ! INTEGER, PARAMETER :: ETD = 13
   ! INTEGER, PARAMETER :: PPD = 14
   ! INTEGER, PARAMETER :: SMD = 15
   ! INTEGER, PARAMETER :: BKD = 16
   INTEGER, PARAMETER :: SYD = 17 !! Sediment yield input data.
   INTEGER, PARAMETER :: CMD = 18 !! Contaminant model input data.
   ! INTEGER, PARAMETER :: MED = 19
   ! INTEGER, PARAMETER :: PRD = 20
   ! INTEGER, PARAMETER :: EPD = 21
   ! INTEGER, PARAMETER :: TIM = 22
   ! INTEGER, PARAMETER :: PRI = 23 is now in sglobal
   INTEGER, PARAMETER :: SPR = 24 !! Summary print output.
   INTEGER, PARAMETER :: CMP = 25 !! Comparison output file.
   INTEGER, PARAMETER :: BUG = 26 !! Debug information output.
   ! INTEGER, PARAMETER :: RES = 27
   ! INTEGER, PARAMETER :: HOT = 28
   INTEGER, PARAMETER :: VSI = 29 !! VS zone initialization.
   ! INTEGER, PARAMETER :: VED = 30
   INTEGER, PARAMETER :: WLD = 31 !! Water level data.
   INTEGER, PARAMETER :: LFB = 32 !! Link flow binary output.
   INTEGER, PARAMETER :: LHB = 33 !! Link head binary output.
   INTEGER, PARAMETER :: LGB = 34 !! Link general binary output.
   INTEGER, PARAMETER :: BFB = 35 !! Bank flow binary output.
   INTEGER, PARAMETER :: BHB = 36 !! Bank head binary output.
   ! INTEGER, PARAMETER :: OFB = 37
   ! INTEGER, PARAMETER :: OHB = 38
   INTEGER, PARAMETER :: CMT = 39 !! Contaminant text output.
   INTEGER, PARAMETER :: CMB = 40 !! Contaminant binary output.
   ! INTEGER, PARAMETER :: DIS = 41
   ! INTEGER, PARAMETER :: VSE = 42
   ! INTEGER, PARAMETER :: MAS = 43
   ! INTEGER, PARAMETER :: dis2 = 44


   !-----------------------------------------------------------------------
   ! Static Grid, Link, and Component Properties
   !-----------------------------------------------------------------------
   DOUBLEPRECISION :: TIH !! Time in hours, usage to be reviewed.

   ! 2D Plan View Arrays (NELEE)
   INTEGER, DIMENSION(NELEE) :: NBFACE !! Number of boundary faces for each element.
   INTEGER, DIMENSION(NELEE) :: NLYR !! Number of layers in each element.
   INTEGER, DIMENSION(NELEE) :: NVC !! Vegetation category for each element.
   INTEGER, DIMENSION(NELEE) :: NWELBT, NWELTP, NVSWLT !! Well number bottom and top layers of screen, well categories.
   INTEGER, DIMENSION(NELEE) :: NVSSPC !! Spring source element.
   INTEGER, DIMENSION(NELEE) :: NVSSPT !! Target element for water from spring VS13a.
   INTEGER, DIMENSION(NELEE) :: NVSWLI !! Well element numbers.
   DOUBLEPRECISION, DIMENSION(NELEE,4) :: DHF !! Distance from node to face [m].
   LOGICAL, DIMENSION(NELEE) :: ISPACK !! Flag indicating if a snow pack exists.

   ! 3D Arrays (LLEE, NELEE)
   INTEGER, DIMENSION(4,LLEE,NELEE) :: JVSACN, JVSDEL !! Indices for VSS calculations.
   DOUBLEPRECISION, DIMENSION(LLEE,NELEE) :: DELTAZ !! Cell thickness [m].
   DOUBLEPRECISION, DIMENSION(LLEE,NELEE) :: ZVSNOD !! Node elevations [m].

   ! Link Arrays (NLFEE)
   INTEGER, DIMENSION(NLFEE,2) :: ICMBK, NHBED !! Link-bank connection and bed data.
   INTEGER, DIMENSION(NLFEE,6) :: ICMRF2 !! Indices for link branching.
   DOUBLEPRECISION, DIMENSION(NLFEE) :: CLENTH, CWIDTH !! Length and width of link [m].
   DOUBLEPRECISION, DIMENSION(NLFEE) :: ZBEFF, ZBFULL !! Elevation of bed and bank full [m].
   DOUBLEPRECISION, DIMENSION(NLFEE,2) :: FHBED !! Cell sizes under channel link [m].
   LOGICAL :: BEXBK !! Flag indicating if banks exist.
   LOGICAL, DIMENSION(NLFEE) :: LINKNS !! Flag indicating if link runs North-South.

   ! Vegetation Arrays (NVEE)
   INTEGER :: NV !! Number of vegetation types.
   INTEGER, DIMENSION(NVEE) :: NRD !! Number of UZ cells in root zone.
   DOUBLEPRECISION, DIMENSION(NVEE) :: RDL !! Proportion of roots that take water from the channel.
   DOUBLEPRECISION, DIMENSION(NVEE,LLEE) :: RDF !! Root density function.

   ! Soil Arrays (NSEE, NLYREE)
   INTEGER :: NS !! Number of soil types.
   DOUBLEPRECISION, DIMENSION(NSEE) :: THSAT, VSPOR !! Saturated moisture content and porosity.
   INTEGER, DIMENSION(NELEE,NLYREE) :: NLYRBT !! Bottom cell number in each soil layer.
   INTEGER, DIMENSION(NELEE,NLYREE) :: NTSOIL !! Soil type in each soil layer.
   DOUBLEPRECISION, DIMENSION(NELEE,NLYREE) :: ZLYRBT !! Elevation of the bottom of each soil layer [m].

   !-----------------------------------------------------------------------
   ! Time-Dependent Global Arrays
   !-----------------------------------------------------------------------
   ! 2D Plan View Arrays (NELEE)
   INTEGER, DIMENSION(NXEE*NYEE) :: IDUM !! Dummy integer array, usage to be reviewed.
   INTEGER, DIMENSION(NELEE) :: ISORT !! Sorting index array.
   INTEGER, DIMENSION(NELEE) :: NHSAT !! Not used, candidate for removal.
   DOUBLEPRECISION, DIMENSION(NELEE) :: DRAINA !! Drainage from element [m].
   DOUBLEPRECISION, DIMENSION(NELEE) :: DUMMY !! Dummy double precision array.
   DOUBLEPRECISION, DIMENSION(NELEE) :: ESOILA !! Soil evaporation [m].
   DOUBLEPRECISION, DIMENSION(NELEE) :: EEVAP !! Evaporation from element [m].
   DOUBLEPRECISION, DIMENSION(NELEE) :: PNETTO !! Net precipitation [m].
   DOUBLEPRECISION, DIMENSION(NELEE) :: QH !! Surface water depth [m].
   DOUBLEPRECISION, DIMENSION(NELEE) :: WBERR !! Water balance error [m].
   DOUBLEPRECISION, DIMENSION(NELEE) :: ZVSPSL !! Phreatic surface level [m].
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSBF !! Baseflow from VSS [m^3/s].
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSSPR !! Spring flow from VSS [m^3/s].
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSWEL !! Well flow from VSS [m^3/s].
   DOUBLEPRECISION, DIMENSION(NELEE,4) :: QOC !! Overland flow through cell faces [m^3/s].

   ! 3D Allocatable Arrays (LLEE, NELEE)
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: QVSV   !! Vertical flow rate between cells [m³/s].
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VSPSI  !! Soil water potential (pressure head) in unsaturated zone [m].
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VSTHE  !! Volumetric moisture content in unsaturated zone [-].
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: QVSWLI !! Vertical flow rate from wells and line sources [m³/s].
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ERUZ   !! Plant transpiration rate from unsaturated zone [m/s].
   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: QVSH !! Horizontal flow rate between adjacent cells [m³/s].

   ! Link Arrays (NLFEE)
   DOUBLEPRECISION, DIMENSION(NLFEE) :: ARXL !! Cross-sectional area of flow in link [m^2].
   DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKB !! Bank baseflow [m^3/s].
   DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKF !! Bank flow [m^3/s].
   DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKI !! Bank interflow [m^3/s].

   ! Vegetation Arrays (NVEE)
   DOUBLEPRECISION, DIMENSION(NVEE) :: CLAI !! Canopy leaf area index.
   DOUBLEPRECISION, DIMENSION(NVEE) :: PLAI !! Proportion of ground cover at maximum seasonal extent.

   ! Sediment Arrays (NSEDEE)
   DOUBLEPRECISION, DIMENSION(NELEE,NSEDEE) :: SBERR !! Sediment balance error.

   ! Unsaturated Zone Variables
   DOUBLEPRECISION :: DTUZ, UZNEXT !! Timestep for UZ and next UZ time.

CONTAINS

   !> summary: Allocates and initializes time-dependent VSS arrays.
   !>
   !> This subroutine allocates the main time-dependent arrays used in the
   !> vadose-saturated-surface (VSS) flow calculations. It initializes them
   !> to zero at the beginning of the simulation. The arrays are sized based
   !> on the actual grid dimensions (`total_no_elements`, `top_cell_no`) read
   !> from the input data.
   !>
   !> It allocates the following arrays:
   !>
   !> - `QVSH`: Horizontal flow between cells.
   !> - `QVSV`: Vertical flow between cells.
   !> - `VSPSI`: Soil water potential (psi).
   !> - `VSTHE`: Soil moisture content (theta).
   !> - `QVSWLI`: Water flow from wells/line sources.
   !> - `ERUZ`: Transpiration from the unsaturated zone.
   SUBROUTINE initialise_al_c()

      ALLOCATE(qvsh(4,top_cell_no,total_no_elements))
      ALLOCATE(qvsv(top_cell_no,total_no_elements))
      ALLOCATE(vspsi(top_cell_no,total_no_elements))
      ALLOCATE(vsthe(top_cell_no,total_no_elements))
      ALLOCATE(qvswli(top_cell_no,total_no_elements))
      ALLOCATE(eruz(total_no_elements,top_cell_no))
      qvsh=0.0d0
      qvsv=0.0d0
      vspsi=0.0d0
      vsthe=0.0d0
      qvswli=0.0d0
      eruz=0.0d0

   END SUBROUTINE initialise_al_c
END MODULE AL_C
