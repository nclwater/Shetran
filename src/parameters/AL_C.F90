!> summary: Shared geometry and state for coupled flow, sediment, and contaminant calculations.
!> author: GP, Newcastle University; RJL, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> `AL_C` replaces the legacy `AL.C` common blocks. [[frmod]] and [[vsmod]]
!> construct the element, link, soil-layer, well, spring, and VSS geometry;
!> [[etmod]], [[vsmod]], and [[run_sim:simulation]] update the water and
!> vegetation state; sediment, contaminant, nitrate, result, and visualisation
!> routines consume selected values.
!>
!> Fixed-size arrays use compile-time capacity bounds. Active element, link,
!> vegetation, soil, and vertical-cell ranges are normally
!> `1:total_no_elements`, the link range established by `FRIND`, `1:NV`,
!> `1:NS`, and the per-element range `NLYRBT(element,1):top_cell_no`.
!> The three initializer routines allocate the active VSS topology/state,
!> soil-layer geometry, and root-density arrays. Module state is public by
!> default and has no automatic initialization unless stated by an initializer.
!>
!> | Array family | Index order | Principal producer |
!> |:-------------|:------------|:-------------------|
!> | `JVSACN`, `JVSDEL`, `QVSH` | face, vertical cell, element | [[vsmod:vsconc]] / [[vsmod:vssim]] |
!> | `DELTAZ`, `ZVSNOD`, `QVSV`, `VSPSI`, `VSTHE`, `QVSWLI` | vertical cell, element or well | [[vsmod]] |
!> | `ERUZ` | element, vertical cell | [[etmod:et]] |
!> | `NLYRBT`, `NTSOIL`, `ZLYRBT` | element, soil layer | [[vsmod:vsread]] / [[vsmod:vsconc]] |
!> | `ICMBK`, `NHBED`, `FHBED`, `QBK*` | link, bank side | frame and VSS setup/simulation |
!>
!> Flux units depend on the control surface. Vertical column rates such as
!> `QH`, `QVSBF`, `QVSWEL`, and `QVSWLI` are depths per second (m/s), while
!> face, channel-bank, and spring discharges such as `QOC`, `QVSH`, `QBK*`,
!> and `QVSSPR` are volumetric rates (m3/s). Positive/sign conventions are
!> those of the producing solver; [[rest:balwat]] applies the required face
!> signs when forming an element balance.
!>
!> @warning
!> Manual section 2.3 still describes nonzero `RDL` as reducing the root
!> distribution assigned to bank elements. The current [[etmod:etchk2]]
!> instead requires every active `RDL` value to equal zero, and the ET solver
!> does not otherwise read the array. This documentation records the current
!> implementation and does not change that discrepancy.
!>
!> The allocatable arrays are managed by unconditional one-shot allocation:
!> there are no `ALLOCATED` guards, `STAT=` handlers, or matching deallocation
!> routines in `AL_C`. Re-entering an initializer while any of its arrays is
!> allocated is a Fortran runtime error.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-03 | GP | 3.0 | Original version written. |
!> | 1991-07 | GP | 3.1 | Moved shared variables from `AL_D`. |
!> | 1991-10 | GP | 3.2 | Added the former `IRRC` state. |
!> | 1992-02 | RJL | 3.4 | Added `CMT` and `CMB`; moved `UZNOW` and `TIH` from `AL_D`. |
!> | 1993-07 | GP | 3.4 | Moved `NRD` and `RDF`; added `ISPACK`, `SBERR`, and `WBERR`. |
!> | 1994-09-30 | RAH | 3.4.1 | Declared all variables, removed `INTEGER*2`, tidied comments, and reorganized the common blocks. |
!> | 1995-05-04 | GP | 4.0 | Added the VSS state and moved selected variables from `AL_D`. |
!> | 1997-02 | RAH | 4.1 | Retained `THSAT`, removed redundant state, and standardized VSS array subscript order. |
!> | 1998-03 | RAH | 4.2 | Removed redundant VSS and overland/channel variables. |
!> | 2004-07 | JE | - | Converted the shared state to Fortran 95. |
!> | 2026-03-30 | SB | 4.6.1 | Made the active VSS, soil-layer, and root-density arrays allocatable and added three initializers. |
!> @endhistory
MODULE AL_C
   USE SGLOBAL, ONLY : NELEE, LLEE, NLFEE, NVSEE, NXEE, NYEE, NSEDEE, NVEE, NLYREE, NSEE, top_cell_no, total_no_elements
   IMPLICIT NONE

! File units occupy their rundata positions. SFB and SRB are non-opened
! placeholders retained by the sediment interface.
   INTEGER, PARAMETER :: SFB = 9876   !! Placeholder for the unimplemented sediment flow-boundary stream.
   INTEGER, PARAMETER :: SRB = 9877   !! Placeholder for the unimplemented sediment rating/boundary stream.
   INTEGER, PARAMETER :: VSD = 11     !! Variably saturated subsurface data input unit.
   INTEGER, PARAMETER :: SYD = 17     !! Sediment data input unit.
   INTEGER, PARAMETER :: CMD = 18     !! Contaminant data input unit.
   INTEGER, PARAMETER :: SPR = 24     !! Formatted sediment diagnostic/output unit.
   INTEGER, PARAMETER :: CMP = 25     !! Formatted contaminant diagnostic/output unit (called `CPR` in the manual).
   INTEGER, PARAMETER :: BUG = 26     !! Developer debug-output unit.
   INTEGER, PARAMETER :: VSI = 29     !! VSS initial-condition input unit.
   INTEGER, PARAMETER :: WLD = 31     !! Time-varying well-abstraction input unit.
   INTEGER, PARAMETER :: LFB = 32     !! Time-varying lateral subsurface-flow boundary unit.
   INTEGER, PARAMETER :: LHB = 33     !! Time-varying lateral subsurface-head boundary unit.
   INTEGER, PARAMETER :: LGB = 34     !! Time-varying lateral head-gradient boundary unit.
   INTEGER, PARAMETER :: BFB = 35     !! Time-varying aquifer-base flow boundary unit.
   INTEGER, PARAMETER :: BHB = 36     !! Time-varying aquifer-base head boundary unit.
   INTEGER, PARAMETER :: CMT = 39     !! First contaminant-migration boundary input unit.
   INTEGER, PARAMETER :: CMB = 40     !! Second contaminant-migration boundary input unit.
   INTEGER, PARAMETER :: MND = 53     !! Nitrogen and carbon data input unit.
   INTEGER, PARAMETER :: MNFC = 54    !! External carbon-input unit.
   INTEGER, PARAMETER :: MNFN = 55    !! External nitrogen-input unit.
   INTEGER, PARAMETER :: MNPL = 56    !! Nitrate plant-uptake input unit.
   INTEGER, PARAMETER :: MNPR = 57    !! Formatted nitrate diagnostic/output unit.
   INTEGER, PARAMETER :: MNOUT1 = 58  !! Nitrate carbon extra-output unit.
   INTEGER, PARAMETER :: MNOUT2 = 59  !! Nitrate nitrogen extra-output unit.
   INTEGER, PARAMETER :: MNOUTPL = 60 !! Nitrate plant-output unit.

   DOUBLEPRECISION TIH !! Simulation start as an absolute hour count returned by `HOUR_FROM_DATE` (h).

! Plan and column geometry.
   INTEGER, DIMENSION(NELEE) :: NBFACE !! External boundary-face number by boundary element; zero otherwise.
   INTEGER, DIMENSION(NELEE) :: NLYR   !! Number of defined soil/lithology layers by element.
   INTEGER, DIMENSION(NELEE) :: NVC    !! Vegetation-category number by element.
   INTEGER, DIMENSION(NELEE) :: NWELBT !! Bottom VSS cell of the well screen by well element.
   INTEGER, DIMENSION(NELEE) :: NWELTP !! Top VSS cell of the well screen by well element.
   INTEGER, DIMENSION(NELEE) :: NVSWLT !! Irrigating well element keyed by its target element; zero means no target mapping.
   INTEGER, DIMENSION(NELEE) :: NVSSPC !! VSS cell containing a spring source by source element.
   INTEGER, DIMENSION(NELEE) :: NVSSPT !! Spring source element keyed by its target element; zero means no target mapping.
   INTEGER, DIMENSION(NELEE) :: NVSWLI !! Well-record number keyed by the element containing the well; zero means no well.
   DOUBLEPRECISION, DIMENSION(NELEE,4) :: DHF !! Distance from an element node/centroid to each face (m).
   LOGICAL, DIMENSION(NELEE) :: ISPACK !! Whether a snowpack is present on each element.

   INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: JVSACN !! Adjacent VSS cell number by face, cell, and element; zero means no connection.
   INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: JVSDEL !! Signed split-cell connectivity code by face, cell, and element.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: DELTAZ !! VSS cell thickness by cell and element (m).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ZVSNOD !! VSS node elevation by cell and element (m).

! Link and bank geometry.
   INTEGER, DIMENSION(NLFEE,2) :: ICMBK  !! Explicit bank-element number by link and bank side.
   INTEGER, DIMENSION(NLFEE,2) :: NHBED  !! Highest VSS cell below the channel bed by link and bank side.
   INTEGER, DIMENSION(NLFEE,6) :: ICMRF2 !! Multi-link branch map: adjacent elements in columns 1:3 and their faces in 4:6.
   DOUBLEPRECISION, DIMENSION(NLFEE) :: CLENTH !! Channel-link length (m).
   DOUBLEPRECISION, DIMENSION(NLFEE) :: CWIDTH !! Channel-link width (m).
   DOUBLEPRECISION, DIMENSION(NLFEE) :: ZBEFF  !! Effective channel-bed elevation (m).
   DOUBLEPRECISION, DIMENSION(NLFEE) :: ZBFULL !! Bankfull channel elevation (m).
   DOUBLEPRECISION, DIMENSION(NLFEE,2) :: FHBED !! Fractional vertical-cell overlap at the channel bed by link and bank side.
   LOGICAL :: BEXBK                            !! Whether explicit bank elements are enabled.
   LOGICAL, DIMENSION(NLFEE) :: LINKNS         !! Whether each channel link is aligned north-south.

! Vegetation and soil metadata.
   INTEGER :: NV                           !! Number of active vegetation types.
   INTEGER, DIMENSION(NVEE) :: NRD         !! Number of VSS nodes in the root zone by vegetation type.
   DOUBLEPRECISION, DIMENSION(NVEE) :: RDL !! Legacy bank-root fraction read from ET8; current validation requires zero.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RDF !! Root-density fraction by vegetation type and VSS node.

   INTEGER :: NS                              !! Number of active soil types.
   DOUBLEPRECISION, DIMENSION(NSEE) :: THSAT  !! Saturated moisture content used by sediment coupling, by soil type (m3/m3).
   DOUBLEPRECISION, DIMENSION(NSEE) :: VSPOR  !! VSS porosity/saturated volumetric water content by soil type (m3/m3).
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: NLYRBT !! Bottom VSS cell number by element and soil layer.
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: NTSOIL !! Soil-type number by element and soil layer.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ZLYRBT !! Bottom elevation by element and soil layer (m).

! Time-dependent and workspace state.
   INTEGER, DIMENSION(NXEE*NYEE) :: IDUM !! Integer workspace for spatial/category input.
   INTEGER, DIMENSION(NELEE) :: ISORT    !! Element processing order, normally sorted from highest water level downward.
   INTEGER, DIMENSION(NELEE) :: NHSAT    !! Unused legacy saturation-state array with no current producer or consumer.
   DOUBLEPRECISION, DIMENSION(NELEE) :: DRAINA !! Canopy drainage reaching the surface by element (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: DUMMY  !! Floating-point workspace for spatial input and validation.
   DOUBLEPRECISION, DIMENSION(NELEE) :: ESOILA !! Soil-surface evaporation rate by element (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: EEVAP  !! Soil plus surface-water evaporation rate by element (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: PNETTO !! Net surface-water input, including mapped well irrigation (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: QH     !! VSS flux through the top of each column (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: WBERR  !! Cumulative element water-balance residual expressed as depth (m).
   DOUBLEPRECISION, DIMENSION(NELEE) :: ZVSPSL !! Phreatic-surface elevation by element (m).
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSBF  !! VSS flux through the base of each active column (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSSPR !! Spring discharge by source element (m3/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSWEL !! Actual total well abstraction/recharge as an element-area flux (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE,4) :: QOC  !! Signed overland/channel discharge through each element face (m3/s).

   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: QVSV   !! Signed vertical VSS flux by cell interface and element (m/s).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VSPSI  !! VSS pressure head by cell and element (m).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VSTHE  !! Volumetric water content by cell and element (m3/m3).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: QVSWLI !! Actual well flux by VSS cell and well record (m/s).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ERUZ   !! Root-water extraction rate by element and VSS cell (m/s).
   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: QVSH !! Signed lateral VSS discharge by face, cell, and element (m3/s).

   DOUBLEPRECISION, DIMENSION(NLFEE) :: ARXL    !! Current channel-flow cross-sectional area by link (m2).
   DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKB  !! Saturated channel-bed exchange for wet channel area (m3/s).
   DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKF  !! Lateral VSS exchange between channel/link and surrounding column (m3/s).
   DOUBLEPRECISION, DIMENSION(NLFEE,2) :: QBKI  !! Channel-bed exchange assigned to dry channel area (m3/s).

   DOUBLEPRECISION, DIMENSION(NVEE) :: CLAI !! Current canopy leaf-area index by vegetation type.
   DOUBLEPRECISION, DIMENSION(NVEE) :: PLAI !! Current maximum-season ground-cover proportion by vegetation type.

   DOUBLEPRECISION, DIMENSION(NELEE,NSEDEE) :: SBERR !! Sediment balance-error state by element and size fraction.

   DOUBLEPRECISION DTUZ   !! Current coupled VSS/ET timestep in seconds (s).
   DOUBLEPRECISION UZNEXT !! Current model timestep expressed in hours (h).
!PRIVATE :: NELEE, LLEE, NLFEE, NVSEE, NXEE, NYEE, NSEDEE, NVEE, NLYREE, NSEE


CONTAINS

!> Allocates and zero-initializes active VSS topology and timestep state.
!>
!> [[vsmod:vsconc]] calls this routine once after `top_cell_no` and
!> `total_no_elements` have been established and before it builds vertical
!> cell connectivity. Later VSS, ET, balance, contaminant, nitrate, result,
!> and visualisation routines use the allocated state.
!>
!> | Arrays | Allocated shape | Initial value |
!> |:-------|:----------------|:--------------|
!> | `QVSH`, `JVSACN`, `JVSDEL` | `(4, top_cell_no, total_no_elements)` | Zero |
!> | `QVSV`, `VSPSI`, `VSTHE`, `QVSWLI` | `(top_cell_no, total_no_elements)` | Zero |
!> | `ERUZ` | `(total_no_elements, top_cell_no)` | Zero |
!>
!> The well-flow array's second dimension is the element-capacity-sized
!> well-record domain even though only records established by `NVSWLI` are
!> active. Allocation is unconditional and has no `STAT=` handler; all eight
!> arrays must be unallocated on entry. No current `AL_C` routine releases
!> them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2019-11-28 | - | - | Active-size allocation and zero-initialization for the six VSS/ET arrays was present in the initial repository snapshot. |
!> | 2026-03-30 | SB | 4.6.1 | Added active-size allocation and zero-initialization for `JVSACN` and `JVSDEL`. |
!> @endhistory
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

!> Allocates and zero-initializes VSS cell and soil-layer geometry.
!>
!> [[vsmod:vsin]] calls this routine once before [[vsmod:vsread]] reads the
!> soil/lithology layers and before [[vsmod:vsconc]] constructs the vertical
!> mesh. `DELTAZ` and `ZVSNOD` retain the compile-time vertical capacity
!> `LLEE`, while the layer arrays retain `NLYREE`; their element extent is the
!> active `total_no_elements`.
!>
!> | Arrays | Allocated shape | Initial value |
!> |:-------|:----------------|:--------------|
!> | `DELTAZ`, `ZVSNOD` | `(LLEE, total_no_elements)` | Zero |
!> | `NLYRBT`, `NTSOIL`, `ZLYRBT` | `(total_no_elements, NLYREE)` | Zero |
!>
!> Allocation is unconditional and has no `STAT=` handler; all five arrays
!> must be unallocated and `total_no_elements` established on entry. No
!> current `AL_C` routine releases them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03-30 | SB | 4.6.1 | Added active-element allocation and zero-initialization for VSS and soil-layer geometry. |
!> @endhistory
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

!> Allocates and zero-initializes the root-density function table.
!>
!> [[frmod:frinit]] calls this routine once after `NV` has been read and before
!> [[frmod:inet]] reads ET17 root-density values. The allocated shape is
!> `(NV,LLEE)`, but
!> only `RDF(vegetation,1:NRD(vegetation))` is populated and subsequently used.
!> ET17 supplies node-depth/value pairs from the surface downward; the depths
!> are read for compatibility but current storage retains only the fractions.
!>
!> Allocation is unconditional and has no `STAT=` handler, so `RDF` must be
!> unallocated and `NV` established on entry. No current routine deallocates
!> it.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03-30 | SB | 4.6.1 | Added active-vegetation allocation and zero-initialization for `RDF`. |
!> @endhistory
   SUBROUTINE initialise_al_c3()

      ALLOCATE (RDF(NV,LLEE))
      RDF=0.0d0

   END SUBROUTINE initialise_al_c3



END MODULE AL_C
