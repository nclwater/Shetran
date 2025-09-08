!> summary: Defines common variables for integrated flow, sediment, and contaminant components.
!> author: G. Parkin, R.J.L., R. A. Heath, J. Ewen
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
!> | Date       | Author | Version | Description                                                              |
!> |:-----------|:-------|:--------|:-------------------------------------------------------------------------|
!> | 1991-03-01 | GP     | 3.0     | Original `INCLUDE` file written.                                         |
!> | 1991-07-01 | GP     | 3.1     | Variables moved from AL.D.                                               |
!> | 1991-10-01 | GP     | 3.2     | Added IRRC.                                                              |
!> | 1992-02-01 | RJL    | 3.4     | Added CMT, CMB. Moved UZNOW, TIH from AL.D.                              |
!> | 1993-07-01 | GP     | 3.4     | Moved NRD from AL.D; RDF from SPEC.ET. Added ISPACK, ZOCMLN, SBERR, WBERR. |
!> | 1994-09-30 | RAH    | 3.4.1   | Modernized declarations, removed INTEGER*2, standard header, added SFB, SRB. |
!> | 1995-05-04 | GP     | 4.0     | Brought variables from AL.D. Added new VSS variables. Replaced CPR with CMP. Removed several variables. |
!> | 1997-02-12 | RAH    | 4.1     | Retained THSAT for SY. Removed BPRNOW.                                   |
!> | 1997-02-13 | RAH    | 4.1     | Removed NVSCIT.                                                          |
!> | 1997-02-14 | RAH    | 4.1     | Swapped subscripts on QVSH, DELTAZ.                                      |
!> | 1997-02-17 | RAH    | 4.1     | Swapped subscripts on JVSACN, JVSDEL, ZVSNOD, QVSV, QVSWLI, VSPSI, VSTHE. Removed QVSL. |
!> | 1997-02-20 | RAH    | 4.1     | Restored history.                                                        |
!> | 1998-03-02 | RAH    | 4.2     | Removed ZOCMLN, VSSTMP, FHSAT.                                           |
!> | 1998-03-07 | RAH    | 4.2     | Amended line above.                                                      |
!> | 2004-07-01 | JE     | -       | Converted to Fortran 95.                                                 |
!> | 2025-08-11 | AI     | -       | Added KIND parameters and FORD docs.                                     |
MODULE AL_C
   USE SGLOBAL, ONLY : NELEE, LLEE, NLFEE, NVSEE, NXEE, NYEE, NSEDEE, NVEE, &
      NLYREE, NSEE, top_cell_no, total_no_elements
   USE MOD_PARAMETERS, ONLY: R8P, I_P

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
   INTEGER(KIND=I_P), PARAMETER :: SFB = 9876 !! Sediment flow binary output (unallocated).
   INTEGER(KIND=I_P), PARAMETER :: SRB = 9877 !! Sediment results binary output (unallocated).
   ! INTEGER(KIND=I_P), PARAMETER :: FRD = 10
   INTEGER(KIND=I_P), PARAMETER :: VSD = 11 !! Vadose-saturated zone input data.
   ! INTEGER(KIND=I_P), PARAMETER :: OCD = 12
   ! INTEGER(KIND=I_P), PARAMETER :: ETD = 13
   ! INTEGER(KIND=I_P), PARAMETER :: PPD = 14
   ! INTEGER(KIND=I_P), PARAMETER :: SMD = 15
   ! INTEGER(KIND=I_P), PARAMETER :: BKD = 16
   INTEGER(KIND=I_P), PARAMETER :: SYD = 17 !! Sediment yield input data.
   INTEGER(KIND=I_P), PARAMETER :: CMD = 18 !! Contaminant model input data.
   ! INTEGER(KIND=I_P), PARAMETER :: MED = 19
   ! INTEGER(KIND=I_P), PARAMETER :: PRD = 20
   ! INTEGER(KIND=I_P), PARAMETER :: EPD = 21
   ! INTEGER(KIND=I_P), PARAMETER :: TIM = 22
   ! INTEGER(KIND=I_P), PARAMETER :: PRI = 23 is now in sglobal
   INTEGER(KIND=I_P), PARAMETER :: SPR = 24 !! Summary print output.
   INTEGER(KIND=I_P), PARAMETER :: CMP = 25 !! Comparison output file.
   INTEGER(KIND=I_P), PARAMETER :: BUG = 26 !! Debug information output.
   ! INTEGER(KIND=I_P), PARAMETER :: RES = 27
   ! INTEGER(KIND=I_P), PARAMETER :: HOT = 28
   INTEGER(KIND=I_P), PARAMETER :: VSI = 29 !! VS zone initialization.
   ! INTEGER(KIND=I_P), PARAMETER :: VED = 30
   INTEGER(KIND=I_P), PARAMETER :: WLD = 31 !! Water level data.
   INTEGER(KIND=I_P), PARAMETER :: LFB = 32 !! Link flow binary output.
   INTEGER(KIND=I_P), PARAMETER :: LHB = 33 !! Link head binary output.
   INTEGER(KIND=I_P), PARAMETER :: LGB = 34 !! Link general binary output.
   INTEGER(KIND=I_P), PARAMETER :: BFB = 35 !! Bank flow binary output.
   INTEGER(KIND=I_P), PARAMETER :: BHB = 36 !! Bank head binary output.
   ! INTEGER(KIND=I_P), PARAMETER :: OFB = 37
   ! INTEGER(KIND=I_P), PARAMETER :: OHB = 38
   INTEGER(KIND=I_P), PARAMETER :: CMT = 39 !! Contaminant text output.
   INTEGER(KIND=I_P), PARAMETER :: CMB = 40 !! Contaminant binary output.
   ! INTEGER(KIND=I_P), PARAMETER :: DIS = 41
   ! INTEGER(KIND=I_P), PARAMETER :: VSE = 42
   ! INTEGER(KIND=I_P), PARAMETER :: MAS = 43
   ! INTEGER(KIND=I_P), PARAMETER :: dis2 = 44


   !-----------------------------------------------------------------------
   ! Static Grid, Link, and Component Properties
   !-----------------------------------------------------------------------
   REAL(KIND=R8P) :: TIH !! Time in hours, usage to be reviewed.

   ! 2D Plan View Arrays (NELEE)
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NBFACE !! Number of boundary faces for each element.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NLYR   !! Number of layers in each element.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NVC    !! Vegetation category for each element.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NWELBT !! Well number bottom layer of screen.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NWELTP !! Well number top layer of screen.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NVSWLT !! Well categories.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NVSSPC !! Spring source element.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NVSSPT !! Target element for water from spring VS13a.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NVSWLI !! Well element numbers.
   REAL(KIND=R8P), DIMENSION(NELEE,4) :: DHF    !! Distance from node to face [m].
   LOGICAL, DIMENSION(NELEE) :: ISPACK !! Flag indicating if a snow pack exists.

   ! 3D Arrays (LLEE, NELEE)
   INTEGER(KIND=I_P), DIMENSION(4,LLEE,NELEE) :: JVSACN !! Vertex adjacency connectivity indices for finite element calculations
   INTEGER(KIND=I_P), DIMENSION(4,LLEE,NELEE) :: JVSDEL !! Vertex deletion indices for finite element mesh operations
   REAL(KIND=R8P), DIMENSION(LLEE,NELEE) :: DELTAZ !! Vertical cell thickness in unsaturated zone [m]
   REAL(KIND=R8P), DIMENSION(LLEE,NELEE) :: ZVSNOD !! Node elevation coordinates in 3D mesh [m]

   ! Link Arrays (NLFEE)
   INTEGER(KIND=I_P), DIMENSION(NLFEE,2) :: ICMBK  !! Channel bank connection indices for linking to adjacent elements
   INTEGER(KIND=I_P), DIMENSION(NLFEE,2) :: NHBED  !! Channel bed node numbers for flow calculations
   INTEGER(KIND=I_P), DIMENSION(NLFEE,6) :: ICMRF2 !! Channel link branching connection indices for flow routing
   REAL(KIND=R8P), DIMENSION(NLFEE) :: CLENTH !! Channel link length [m]
   REAL(KIND=R8P), DIMENSION(NLFEE) :: CWIDTH !! Channel link width [m]
   REAL(KIND=R8P), DIMENSION(NLFEE) :: ZBEFF  !! Channel bed bottom elevation [m]
   REAL(KIND=R8P), DIMENSION(NLFEE) :: ZBFULL !! Bank-full elevation (top of channel banks) [m]
   REAL(KIND=R8P), DIMENSION(NLFEE,2) :: FHBED  !! Cell sizes under channel bed for subsurface flow [m]
   LOGICAL :: BEXBK !! Global flag indicating whether channel banks exist in the model domain
   LOGICAL, DIMENSION(NLFEE) :: LINKNS !! Channel orientation flag: true if link runs North-South, false if East-West

   ! Vegetation Arrays (NVEE)
   INTEGER(KIND=I_P) :: NV !! Number of vegetation types defined in the simulation
   INTEGER(KIND=I_P), DIMENSION(NVEE) :: NRD !! Number of unsaturated zone cells within root zone for each vegetation type
   REAL(KIND=R8P), DIMENSION(NVEE) :: RDL !! Proportion of roots taking water directly from channel (0-1)
   REAL(KIND=R8P), DIMENSION(NVEE,LLEE) :: RDF !! Root density distribution function by depth layer (0-1)

   ! Soil Arrays (NSEE, NLYREE)
   INTEGER(KIND=I_P) :: NS !! Number of soil types defined in the simulation
   REAL(KIND=R8P), DIMENSION(NSEE) :: THSAT  !! Saturated volumetric moisture content for each soil type (0-1)
   REAL(KIND=R8P), DIMENSION(NSEE) :: VSPOR  !! Total porosity for each soil type (0-1)
   INTEGER(KIND=I_P), DIMENSION(NELEE,NLYREE) :: NLYRBT !! Bottom cell number for each soil layer in each element
   INTEGER(KIND=I_P), DIMENSION(NELEE,NLYREE) :: NTSOIL !! Soil type index for each soil layer in each element
   REAL(KIND=R8P), DIMENSION(NELEE,NLYREE) :: ZLYRBT !! Elevation of bottom boundary of each soil layer [m]

   !-----------------------------------------------------------------------
   ! Time-Dependent Global Arrays
   !-----------------------------------------------------------------------
   ! 2D Plan View Arrays (NELEE)
   INTEGER(KIND=I_P), DIMENSION(NXEE*NYEE) :: IDUM !! Temporary integer workspace array for general calculations
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: ISORT !! Element sorting indices for computational ordering
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NHSAT !! Unused array - candidate for removal in future refactoring
   REAL(KIND=R8P), DIMENSION(NELEE) :: DRAINA !! Cumulative drainage area contributing to each element [m²]
   REAL(KIND=R8P), DIMENSION(NELEE) :: DUMMY  !! Temporary workspace array for general double precision calculations
   REAL(KIND=R8P), DIMENSION(NELEE) :: ESOILA !! Actual soil evaporation rate from bare ground [m/time]
   REAL(KIND=R8P), DIMENSION(NELEE) :: EEVAP  !! Total evaporation rate (soil + vegetation) from element [m/time]
   REAL(KIND=R8P), DIMENSION(NELEE) :: PNETTO !! Net precipitation after interception losses [m/time]
   REAL(KIND=R8P), DIMENSION(NELEE) :: QH     !! Surface water depth on element [m]
   REAL(KIND=R8P), DIMENSION(NELEE) :: WBERR  !! Water balance error for mass conservation checking [m]
   REAL(KIND=R8P), DIMENSION(NELEE) :: ZVSPSL !! Phreatic surface elevation (water table level) [m]
   REAL(KIND=R8P), DIMENSION(NELEE) :: QVSBF  !! Baseflow discharge from vadose-saturated zone [m³/s]
   REAL(KIND=R8P), DIMENSION(NELEE) :: QVSSPR !! Spring discharge from vadose-saturated zone [m³/s]
   REAL(KIND=R8P), DIMENSION(NELEE) :: QVSWEL !! Well discharge from vadose-saturated zone [m³/s]
   REAL(KIND=R8P), DIMENSION(NELEE,4) :: QOC !! Overland flow discharge rate through each element face [m³/s]

   ! 3D Allocatable Arrays (LLEE, NELEE)
   REAL(KIND=R8P), DIMENSION(:,:), ALLOCATABLE :: QVSV   !! Vertical flow rate between vertically adjacent cells [m³/s]
   REAL(KIND=R8P), DIMENSION(:,:), ALLOCATABLE :: VSPSI  !! Soil water pressure head in unsaturated zone [m]
   REAL(KIND=R8P), DIMENSION(:,:), ALLOCATABLE :: VSTHE  !! Volumetric soil moisture content in unsaturated zone [dimensionless]
   REAL(KIND=R8P), DIMENSION(:,:), ALLOCATABLE :: QVSWLI !! Vertical flow rate from wells and line sources with infiltration [m³/s]
   REAL(KIND=R8P), DIMENSION(:,:), ALLOCATABLE :: ERUZ   !! Plant transpiration rate extracted from unsaturated zone [m/s]
   REAL(KIND=R8P), DIMENSION(:,:,:), ALLOCATABLE :: QVSH !! Horizontal flow rate between horizontally adjacent cells [m³/s]

   ! Link Arrays (NLFEE)
   REAL(KIND=R8P), DIMENSION(NLFEE) :: ARXL   !! Cross-sectional area of water flow in channel link [m²]
   REAL(KIND=R8P), DIMENSION(NLFEE,2) :: QBKB !! Bank baseflow discharge in backward direction [m³/s]
   REAL(KIND=R8P), DIMENSION(NLFEE,2) :: QBKF !! Bank flow discharge in forward direction [m³/s]
   REAL(KIND=R8P), DIMENSION(NLFEE,2) :: QBKI !! Bank infiltration flow rate from channel to banks [m³/s]

   ! Vegetation Arrays (NVEE)
   REAL(KIND=R8P), DIMENSION(NVEE) :: CLAI !! Canopy leaf area index for each vegetation type [m²/m²]
   REAL(KIND=R8P), DIMENSION(NVEE) :: PLAI !! Proportion of ground area covered by vegetation at maximum seasonal extent [0-1]

   ! Sediment Arrays (NSEDEE)
   REAL(KIND=R8P), DIMENSION(NELEE,NSEDEE) :: SBERR !! Sediment mass balance error by size class [kg/m²]

   ! Unsaturated Zone Variables
   REAL(KIND=R8P) :: DTUZ   !! Time step size for unsaturated zone calculations [s]
   REAL(KIND=R8P) :: UZNEXT !! Next scheduled time for unsaturated zone update [s]

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
      qvsh=0.0_R8P
      qvsv=0.0_R8P
      vspsi=0.0_R8P
      vsthe=0.0_R8P
      qvswli=0.0_R8P
      eruz=0.0_R8P

   END SUBROUTINE initialise_al_c
END MODULE AL_C
