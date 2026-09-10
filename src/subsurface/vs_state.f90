!> summary: The variably saturated subsurface state: column geometry, heads, moisture and fluxes.
!> author: GP, Newcastle University; RJL, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> The state of the subsurface solution: the vertical discretisation of each
!> element column, the soil-layer and well geometry, and the pressure heads,
!> moisture contents and fluxes the solver advances. [[frmod]] and [[vsmod]]
!> construct the geometry; [[vsmod]] advances the state; sediment, contaminant,
!> nitrate, result and visualisation routines read selected values. It is the
!> most widely read of the component state modules.
!>
!> Active element, link, soil and vertical-cell ranges are normally
!> `1:total_no_elements`, the link range established by `FRIND`, `1:NS`, and the
!> per-element range `NLYRBT(element,1):top_cell_no`. Module state is public by
!> default and has no automatic initialization unless an initializer sets it.
!>
!> | Array family | Index order | Principal producer |
!> |:-------------|:------------|:-------------------|
!> | `JVSACN`, `JVSDEL`, `QVSH` | face, vertical cell, element | [[vs_connectivity:VSCONC]] / [[vs_driver:VSSIM]] |
!> | `DELTAZ`, `ZVSNOD`, `QVSV`, `VSPSI`, `VSTHE`, `QVSWLI` | vertical cell, element or well | [[vsmod]] |
!> | `NLYRBT`, `NTSOIL`, `ZLYRBT` | element, soil layer | [[vs_input:VSREAD]] / [[vs_connectivity:VSCONC]] |
!>
!> Flux units depend on the control surface. Vertical column rates such as
!> `QH`, `QVSBF`, `QVSWEL` and `QVSWLI` are depths per second (m/s), while
!> face, channel-bank and spring discharges such as `QVSH`, `QBK*` and
!> `QVSSPR` are volumetric rates (m3/s). Positive/sign conventions are those of
!> the producing solver; [[water_balance:BALWAT]] applies the required face
!> signs when forming an element balance.
!>
!> `initialise_al_c` and `initialise_al_c2` allocate and zero the active VSS
!> topology, state and soil-layer geometry. They keep names that refer to the
!> module they came from; renaming them is a follow-up commit.
!>
!> @warning
!> The allocatable arrays are managed by unconditional one-shot allocation:
!> there are no `ALLOCATED` guards or matching deallocation routines.
!> Re-entering an initializer while any of its arrays is allocated is a Fortran
!> runtime error.
!>
!> `initialise_al_c` also allocates and zeroes `ERUZ`, which belongs to
!> [[et_state]]. That is why this module imports from `et_state`; the edge runs
!> one way only.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_state

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE array_limits, ONLY: nelee, LLEE, nlfee, NLYREE, NSEE
   USE element_geometry, ONLY: top_cell_no, total_no_elements
   USE error_status, ONLY: errstat_alloc
   USE et_state, ONLY: ERUZ

   IMPLICIT NONE


! Plan and column geometry.
   INTEGER, DIMENSION(NELEE) :: NLYR   !! Number of defined soil/lithology layers by element.
   INTEGER, DIMENSION(NELEE) :: NWELBT !! Bottom VSS cell of the well screen by well element.
   INTEGER, DIMENSION(NELEE) :: NWELTP !! Top VSS cell of the well screen by well element.
   INTEGER, DIMENSION(NELEE) :: NVSWLT !! Irrigating well element keyed by its target element; zero means no target mapping.
   INTEGER, DIMENSION(NELEE) :: NVSSPC !! VSS cell containing a spring source by source element.
   INTEGER, DIMENSION(NELEE) :: NVSSPT !! Spring source element keyed by its target element; zero means no target mapping.
   INTEGER, DIMENSION(NELEE) :: NVSWLI !! Well-record number keyed by the element containing the well; zero means no well.
   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: JVSACN !! Adjacent VSS cell number by face, cell, and element; zero means no connection.
   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: JVSDEL !! Signed split-cell connectivity code by face, cell, and element.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: DELTAZ !! VSS cell thickness by cell and element (m).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: ZVSNOD !! VSS node elevation by cell and element (m).

! Soil metadata.
   INTEGER :: NS                              !! Number of active soil types.
   DOUBLEPRECISION, DIMENSION(NSEE) :: THSAT  !! Saturated moisture content used by sediment coupling, by soil type (m3/m3).
   DOUBLEPRECISION, DIMENSION(NSEE) :: VSPOR  !! VSS porosity/saturated volumetric water content by soil type (m3/m3).
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: NLYRBT !! Bottom VSS cell number by element and soil layer.
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: NTSOIL !! Soil-type number by element and soil layer.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: ZLYRBT !! Bottom elevation by element and soil layer (m).

! Time-dependent state and fluxes.
   DOUBLEPRECISION, DIMENSION(NELEE) :: QH     !! VSS flux through the top of each column (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: WBERR  !! Cumulative element water-balance residual expressed as depth (m).
   DOUBLEPRECISION, DIMENSION(NELEE) :: ZVSPSL !! Phreatic-surface elevation by element (m).
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSBF  !! VSS flux through the base of each active column (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSSPR !! Spring discharge by source element (m3/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: QVSWEL !! Actual total well abstraction/recharge as an element-area flux (m/s).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: QVSV   !! Signed vertical VSS flux by cell interface and element (m/s).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: VSPSI  !! VSS pressure head by cell and element (m).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: VSTHE  !! Volumetric water content by cell and element (m3/m3).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: QVSWLI !! Actual well flux by VSS cell and well record (m/s).
   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: QVSH !! Signed lateral VSS discharge by face, cell, and element (m3/s).
   DOUBLEPRECISION, DIMENSION(NLFEE, 2) :: QBKB  !! Saturated channel-bed exchange for wet channel area (m3/s).
   DOUBLEPRECISION, DIMENSION(NLFEE, 2) :: QBKF  !! Lateral VSS exchange between channel/link and surrounding column (m3/s).
   DOUBLEPRECISION, DIMENSION(NLFEE, 2) :: QBKI  !! Channel-bed exchange assigned to dry channel area (m3/s).

CONTAINS

!> Allocates and zero-initializes active VSS topology and timestep state.
!>
!> [[vs_connectivity:VSCONC]] calls this routine once after `top_cell_no` and
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
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE initialise_al_c()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "AL_C:initialise_al_c"

      ALLOCATE (qvsh(4, top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "qvsh", location, emsg)
      ALLOCATE (qvsv(top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "qvsv", location, emsg)
      ALLOCATE (vspsi(top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "vspsi", location, emsg)
      ALLOCATE (vsthe(top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "vsthe", location, emsg)
      ALLOCATE (qvswli(top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "qvswli", location, emsg)
      ALLOCATE (eruz(total_no_elements, top_cell_no), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "eruz", location, emsg)
      ALLOCATE (JVSACN(4, top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "JVSACN", location, emsg)
      ALLOCATE (JVSDEL(4, top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "JVSDEL", location, emsg)

      ! Initialize to default values
      qvsh = 0.0d0
      qvsv = 0.0d0
      vspsi = 0.0d0
      vsthe = 0.0d0
      qvswli = 0.0d0
      eruz = 0.0d0
      JVSACN = 0
      JVSDEL = 0

   END SUBROUTINE initialise_al_c

!> Allocates and zero-initializes VSS cell and soil-layer geometry.
!>
!> [[vs_input:VSIN]] calls this routine once before [[vs_input:VSREAD]] reads the
!> soil/lithology layers and before [[vs_connectivity:VSCONC]] constructs the vertical
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
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE initialise_al_c2()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "AL_C:initialise_al_c2"

      ALLOCATE (DELTAZ(LLEE, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DELTAZ", location, emsg)
      ALLOCATE (ZVSNOD(LLEE, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ZVSNOD", location, emsg)
      ALLOCATE (NLYRBT(total_no_elements, NLYREE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NLYRBT", location, emsg)
      ALLOCATE (NTSOIL(total_no_elements, NLYREE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NTSOIL", location, emsg)
      ALLOCATE (ZLYRBT(total_no_elements, NLYREE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ZLYRBT", location, emsg)

      DELTAZ = 0.0d0
      ZVSNOD = 0.0d0
      NLYRBT = 0
      NTSOIL = 0
      ZLYRBT = 0.0d0

   END SUBROUTINE initialise_al_c2

END MODULE vs_state

