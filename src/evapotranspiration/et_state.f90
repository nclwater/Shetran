!> summary: Vegetation metadata, canopy storage and the evapotranspiration fluxes.
!> author: GP, Newcastle University; RJL, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> What the evapotranspiration and interception calculation reads and writes:
!> the per-element vegetation category and the root-zone distribution, the
!> canopy interception store, and the potential and actual evaporation and
!> transpiration rates. [[et_process]] writes most of it; the subsurface
!> solver reads `ERUZ` as its root-extraction sink, and the water balance and
!> the visualisation interface read the flux arrays.
!>
!> Rates are per second unless the comment says otherwise. `CSTORE` and the
!> per-element workspace scalars carry timestep depths in mm, which is the unit
!> the legacy canopy formulation uses. Module state is public by default.
!>
!> `initialise_al_c3` allocates the root-density array once the active
!> vegetation count is known; `initialise_eruz` allocates the root-extraction
!> array once the VSS column discretisation is known. The two extents become
!> valid at different points in start-up, which is why there are two
!> initialisers rather than one — see `docs/rename/issues/ERUZ_init_loc.md`.
!> `initialise_al_c3` keeps the name of the module it came from; renaming it is
!> a follow-up commit.
!>
!> @warning
!> Manual section 2.3 still describes nonzero `RDL` as reducing the root
!> distribution assigned to bank elements. The current [[et_process:ETCHK2]]
!> instead requires every active `RDL` value to equal zero, and the ET solver
!> does not otherwise read the array. This documentation records the current
!> implementation and does not change that discrepancy.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C, AL_D; see docs/rename/proposal.md. |
!> | 2026-09-12 | SvB | - | Took over the `ERUZ` allocation from [[vs_state:initialise_al_c]] as `initialise_eruz`; the `USE et_state` edge out of `vs_state` is gone. |
!> @endhistory
MODULE et_state

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE array_limits, ONLY: nelee, LLEE, NVEE
   USE element_geometry, ONLY: top_cell_no, total_no_elements
   USE error_status, ONLY: errstat_alloc

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: NVC, NV, NRD, RDL, RDF, DRAINA, ESOILA, EEVAP, PNETTO, ERUZ, CLAI, PLAI, HRUZ, PNET, PE, &
             EINT, ERZ, DRAIN, ESOIL, AE, CSTOLD, CPLAI, CSTORE, ERZA, EPOT, EINTA, S, VHT, ESWA, &
             initialise_al_c3, initialise_eruz


! Vegetation metadata and the root-zone distribution.
   INTEGER, DIMENSION(NELEE) :: NVC    !! Vegetation-category number by element.
   INTEGER :: NV                           !! Number of active vegetation types.
   INTEGER, DIMENSION(NVEE) :: NRD         !! Number of VSS nodes in the root zone by vegetation type.
   DOUBLEPRECISION, DIMENSION(NVEE) :: RDL !! Legacy bank-root fraction read from ET8; current validation requires zero.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RDF !! Root-density fraction by vegetation type and VSS node.
   DOUBLEPRECISION, DIMENSION(NELEE) :: DRAINA !! Canopy drainage reaching the surface by element (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: ESOILA !! Soil-surface evaporation rate by element (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: EEVAP  !! Soil plus surface-water evaporation rate by element (m/s).
   DOUBLEPRECISION, DIMENSION(NELEE) :: PNETTO !! Net surface-water input, including mapped well irrigation (m/s).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: ERUZ   !! Root-water extraction rate by element and VSS cell (m/s).
   DOUBLEPRECISION, DIMENSION(NVEE) :: CLAI !! Current canopy leaf-area index by vegetation type.
   DOUBLEPRECISION, DIMENSION(NVEE) :: PLAI !! Current maximum-season ground-cover proportion by vegetation type.

! Per-element workspace and the interception/evaporation state.
   DOUBLEPRECISION :: HRUZ   !! Current element's surface-water depth workspace (m).
   DOUBLEPRECISION :: PNET   !! Current element's net throughfall rate workspace (mm/s).
   DOUBLEPRECISION :: PE     !! Current element's available potential-evaporation rate (mm/s).
   DOUBLEPRECISION :: EINT   !! Current element's canopy-interception evaporation over the step (mm).
   DOUBLEPRECISION :: ERZ    !! Current element's root-zone extraction rate workspace (mm/s).
   DOUBLEPRECISION :: DRAIN  !! Current element's canopy drainage over the step (mm).
   DOUBLEPRECISION :: ESOIL  !! Current element's soil-evaporation rate workspace (mm/s).
   DOUBLEPRECISION :: AE     !! Current cell's actual-evapotranspiration rate workspace (mm/s).
   DOUBLEPRECISION :: CSTOLD !! Current element's canopy storage at step start (mm).
   DOUBLEPRECISION :: CPLAI  !! Current element's intercepted-area fraction, `min(CLAI,1)*PLAI`.
   DOUBLEPRECISION :: CSTORE(NELEE) !! Canopy interception storage by element (mm).
   DOUBLEPRECISION :: ERZA(NELEE)   !! Root-zone extraction rate by element (m/s).
   DOUBLEPRECISION :: EPOT(NELEE)   !! Potential-evaporation rate by element (m/s).
   DOUBLEPRECISION :: EINTA(NELEE)  !! Canopy-interception evaporation rate by element (m/s).
   DOUBLEPRECISION :: S(LLEE)       !! Current column's volumetric root/soil extraction sink by VSS cell (s-1).
   DOUBLEPRECISION :: VHT(NVEE)     !! Current vegetation height by vegetation type (m).
   DOUBLEPRECISION :: ESWA(NELEE)    !! Surface-water evaporation rate by element (m/s).

CONTAINS

!> Allocates and zero-initializes the root-density function table.
!>
!> [[frame_setup:FRINIT]] calls this routine once after `NV` has been read and before
!> [[et_input:INET]] reads ET17 root-density values. The allocated shape is
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
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE initialise_al_c3()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "et_state:initialise_al_c3"

      ALLOCATE (RDF(NV, LLEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RDF", location, emsg)
      RDF = 0.0d0

   END SUBROUTINE initialise_al_c3

!> Allocates and zero-initializes the root-extraction sink array.
!>
!> [[vs_connectivity:VSCONC]] calls this routine once, beside
!> [[vs_state:initialise_al_c]], after `top_cell_no` and `total_no_elements`
!> have been established and before the vertical cell connectivity is built.
!> [[et_process:ET]] writes `ERUZ`; [[vs_driver]], [[water_balance:BALWAT]] and
!> [[cm_column]] read it.
!>
!> | Array | Allocated shape | Initial value |
!> |:------|:----------------|:--------------|
!> | `ERUZ` | `(total_no_elements, top_cell_no)` | Zero |
!>
!> This cannot be folded into `initialise_al_c3`. [[frame_setup:FRINIT]] calls
!> that one from `INFR` onwards, long before [[vs_connectivity:VSCONC]] computes
!> `top_cell_no`, so the second extent would still be the declaration
!> initializer `-1` — a legal `ALLOCATE` that yields a zero-sized dimension and
!> then corrupts memory on the first write. See
!> `docs/rename/issues/ERUZ_init_loc.md`.
!>
!> Allocation is unconditional and has no `ALLOCATED` guard, so `ERUZ` must be
!> unallocated on entry. No current routine deallocates it.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-12 | SvB | - | Split out of [[vs_state:initialise_al_c]] so `ERUZ` is allocated by the module that owns it (D11). Same call site, same allocated shape. |
!> @endhistory
   SUBROUTINE initialise_eruz()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "et_state:initialise_eruz"

      ALLOCATE (ERUZ(total_no_elements, top_cell_no), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ERUZ", location, emsg)
      ERUZ = 0.0d0

   END SUBROUTINE initialise_eruz

END MODULE et_state

