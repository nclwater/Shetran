# Refactor plan — data structures & derived types

**Status:** scannable plan for incremental refactor of global data into coherent Fortran derived types and small focused modules.

## Goals
- Replace pervasive module globals with a small set of coherent derived types.
- Reduce number of `USE` statements and global coupling by passing typed objects to routines.
- Enable safe OpenMP parallelisation for gfortran and ifx targets.
- Keep changes incremental: add types and adapters first, then migrate callers.
- Follow naming convention: short, but descriptive. Mutability chosen per‑data semantics.

## Constraints & key findings (src scan)
- `mod_parameters.f90` centralises KINDs and many `PARAMETER` values — keep and extend.
- `sglobal.f90` is monolithic and widely used; treat it as legacy during migration.
- Many modules expose groups of allocatable arrays and per-component variables (hydraulics, subsurface, snow, sediment, contaminant).
- OpenMP is the chosen parallel model: types and APIs must favour thread‑safe patterns.

## High-level design
- Keep `mod_parameters` for kinds/constants.
- Introduce a set of small modules defining derived types and APIs:
  - `core_types` — primary derived types (Mesh, State, Diagnostics, IOConfig).
  - `params_types` — physics/config parameter types (Hydraulic, Sediment, Snow, Contaminant).
  - `zq_types` — ZQ tables (reader + accessor).
  - `mesh_api`, `state_api`, `io_api` — init/finalize and helpers.
  - `legacy_sglobal_adapter` — temporary adapter mapping old `SGLOBAL` to typed objects.

- Migration approach: create types and adapter, then migrate modules one-by-one to accept typed arguments rather than `USE sglobal`.

## Proposed derived types (short descriptions)

- Mesh_t (mesh_types or core_types)
  - Fields: nx, ny, nel, nxee, nyee, index maps, coordinate arrays.
  - Mutability: sizes fixed after init; internal arrays allocatable and modifiable.
  - Purpose: centralise topology and indexing.

- State_t (state_types or core_types)
  - Fields: flattened runtime arrays (water, q, heads, masks), per-layer data.
  - Mutability: fully mutable at runtime (intent(inout) in routines).
  - Purpose: group all time‑varying fields for timestep routines.

- PhysicsParams_t and subtypes (params_types)
  - Fields: scalar parameters and small lookup arrays; split by domain (hydraulic_t, sediment_t, snow_t, contaminant_t).
  - Mutability: effectively immutable after initialisation (read-only API).

- ZQTable_t (zq_types)
  - Fields: dimensions and 3‑D table array(s), headers, indexes.
  - Mutability: initialised by reader, read-only during simulation.
  - Purpose: replace existing ZQ global arrays with a single encapsulated object.
  - Init & lifecycle: provide type‑bound procedures for init/read_from_file(), validate(), and finalize() so parsing and validation logic sits with the data.

- IOConfig_t and IOHandles_t (io_types)
  - Fields: filenames, directories, format flags, file/HDF5 handles.
  - Mutability: config immutable after init; handles managed by API.

- Diagnostics_t (diag_types)
  - Fields: counters, error states, logging options.
  - Mutability: mutable; provide thread-safe increment helpers.

- SoilProps_t / ElementProps_t (soil_types)
  - Fields: per-element static properties (porosity, conductivity per layer).
  - Mutability: immutable after init.

## Suggested module layout & short names
- `mod_parameters.f90` — kinds/constants (keep)
- `core_types.f90` — Mesh_t, State_t, Diagnostics_t, IOConfig_t
- `params_types.f90` — PhysicsParams_t + subtypes
- `zq_types.f90` — ZQTable_t + reader API
- `mesh_api.f90` — create/init Mesh_t, index utilities
- `state_api.f90` — init/finalize State_t, copy/snapshot utilities
- `io_api.f90` — IO functions using IOConfig_t and State_t
- `legacy_sglobal_adapter.f90` — transitional compatibility layer

## API recommendations
- Prefer explicit typed arguments:
  - subroutine compute_step(mesh, state, params, io, diag)
    - `type(Mesh_t), intent(in) :: mesh`
    - `type(State_t), intent(inout) :: state`
    - `type(PhysicsParams_t), intent(in) :: params`
- Use `intent(in)` for immutable data; `intent(inout)` for state.
- Use 1-D flattened arrays for large fields for better OpenMP performance and contiguous memory.
- Ownership rules: the API that creates a type should own allocations and provide `finalize` routine.

### ZQ tables: init, access and performance pattern
- Use type‑bound procedures for lifecycle management and encapsulation:
  - Implement `type ZQTable_t` with private components and the following TBPs:
    - `init_from_file(this, filename)` — parse and allocate internal arrays
    - `validate(this)` — check dims/consistency
    - `finalize(this)` — deallocate and cleanup
- Provide fast, zero‑overhead access for hot loops:
  - After `init`, either expose the internal allocatable array as a public component (documented read‑only) or provide a lightweight view function that returns an array pointer/associate:
    - `function get_zq_view(this) result(zqptr)` returning a pointer/associate to the internal array
  - Pass the raw array (the pointer/associate or public allocatable) into inner compute routines as `intent(in)` to avoid per‑element accessor overhead.
- Thread-safety:
  - Since the ZQ data is write‑once/read‑many, concurrent reads are safe under OpenMP. Ensure no code mutates the internal arrays after init.
- Rationale:
  - Type‑bound procedures keep parsing and validation close to the data and prevent half‑initialized objects.
  - Exposing a single cheap view gives both encapsulation for setup and zero‑cost access during simulation, preserving performance in hot loops.

## OpenMP & thread-safety guidance
- Avoid global writable state; pass typed objects to parallel regions.
- Partition work per element/cell; each OpenMP thread should write disjoint array regions.
- For counters/logging use OpenMP reductions or thread-local accumulators combined at sync points; provide atomic helpers in `Diagnostics_t` where necessary.
- Ensure arrays in types are contiguous to avoid false sharing and enable vectorisation.

## Migration strategy (incremental)
1. Create `core_types`, `params_types`, `zq_types` modules (types only). Add `legacy_sglobal_adapter`.
2. Implement ZQ reader to populate `ZQTable_t`. Migrate ZQ consumers one-by-one.
3. Implement `Mesh_t` and modify spatial setup to produce a `Mesh_t` instance.
4. Implement `State_t`. Change primary drivers (timestep loop, run_sim) to accept typed instances.
5. Convert one compute component (e.g. subsurface or hydraulic solver) to typed API and validate performance/OpenMP behavior.
6. Iterate migrating remaining modules; keep adapter to maintain compatibility.
7. Remove `sglobal` and adapter once migration is complete.

## Example starter snippet (place in `core_types.f90`)
```fortran
! Example: core_types.f90 (starter)
module core_types
  use mod_parameters, only: I_P, R_P
  implicit none

  type :: Mesh_t
    integer(kind=I_P) :: nx, ny, nel, nxee, nyee
    integer(kind=I_P), allocatable :: icmxy(:,:)
  contains
    procedure :: finalize => mesh_finalize
  end type Mesh_t

  type :: State_t
    real(kind=R_P), allocatable :: water(:)    ! flattened over cells
    real(kind=R_P), allocatable :: head(:,:)   ! (nel, nlayers)
  contains
    procedure :: finalize => state_finalize
  end type State_t

contains

  subroutine mesh_finalize(this)
    class(Mesh_t), intent(inout) :: this
    if (allocated(this%icmxy)) deallocate(this%icmxy)
  end subroutine mesh_finalize

  subroutine state_finalize(this)
    class(State_t), intent(inout) :: this
    if (allocated(this%water))   deallocate(this%water)
    if (allocated(this%head))    deallocate(this%head)
  end subroutine state_finalize

end module core_types
```

## Checklist (implementation)
- [ ] Create modules: `core_types.f90`, `params_types.f90`, `zq_types.f90`, `mesh_api.f90`, `state_api.f90`, `io_api.f90`, `legacy_sglobal_adapter.f90`
- [ ] Implement ZQ reader & migrate `zq_interpolator`
- [ ] Implement `Mesh_t` creation in spatial setup
- [ ] Implement `State_t` and migrate primary timestep driver
- [ ] Migrate one solver and validate OpenMP patterns
- [ ] Continue migration, deprecate `sglobal`, remove adapter

## Risks & mitigations
- Regression risk: mitigate by incremental migration and regression tests.
- Performance risk: ensure arrays are contiguous and hot loops use intent arrays; profile after migration.

## Estimated phasing
- Phase 1 (1–2 weeks): types modules + ZQ migration + adapter
- Phase 2 (2–4 weeks): mesh & state init; migrate one solver
- Phase 3 (4–8 weeks): migrate remaining modules; remove adapter

## Next actions
- Produce a per-module variable→type mapping CSV to guide the mechanical migration (I can generate this if requested).
- Implement `core_types.f90` and `legacy_sglobal_adapter.f90` as first PRs.