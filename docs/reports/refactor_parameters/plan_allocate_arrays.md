## Turn static arrays into dynamically allocated arrays — migration plan

This document describes a pragmatic, incremental plan to convert large, statically-sized module arrays in the `src/parameters` and `src` tree into dynamically allocated (allocatable) arrays. It also covers the necessary changes to file-input modules so arrays can be allocated from sizes discovered at runtime, quality gates, rollback, and testing.

## Brief checklist (requirements extracted)
- Create a migration plan to convert static arrays to allocatable arrays and put it in `docs/reports/refactor_parameters/plan_allocate_arrays.md` (this file).  
- Include adjustments required in file input modules so arrays can be allocated before being filled.  
- Provide incremental, low-risk steps, error handling, validation, build/test steps, and rollback guidance.

## Assumptions
- The codebase compiles with a modern Fortran compiler supporting allocatable arrays and `ALLOCATE(..., STAT=...)` (Fortran 90/95 with allocatable support or later).  
- Input files either already carry array-dimension metadata, or can be adapted to include it (or a short pre-scan can determine dimensions).  
- The `plan_parameter_refactor.md` refactor plan will be followed in parallel (naming and module splits).  

If any of these assumptions are false, the migration steps include fallbacks (pre-scan, compatibility shims).

## Contract: inputs, outputs, success criteria
- Inputs: existing code base with statically-sized module arrays (e.g. large arrays in `AL_C.F90` and similar), input files used at initialization.  
- Outputs: allocatable arrays used throughout runtime, allocation/deallocation lifecycle utilities, updated file-readers that can allocate arrays, updated build configuration where needed.  
- Success criteria: project compiles and runs existing examples with identical numerical behaviour, no memory leaks on typical runs, and improved flexibility for different grid sizes without recompilation.

## Edge cases to cover
- Input files that do not include explicit dimension metadata (need pre-scan or convention).  
- Partial allocation failures due to insufficient memory (must fail gracefully).  
- Modules or routines that assume compile-time constant sizes (must be detected and refactored).  
- Backwards compatibility for external interfaces (APIs, Fortran COMMON blocks, or precompiled binaries).

## High-level migration phases
1. Inventory & design (low risk) — find static arrays and classify their usage.  
2. Introduce allocation helpers and a central `array_shapes` module (low risk).  
3. Update file-readers to provide shape metadata and perform allocations (medium risk).  
4. Replace static declarations with allocatable arrays and wrap with accessors/shims (medium risk).  
5. Rigorous testing, performance checks, and integration (high risk).  

## Phase 1 — Inventory & quick wins
- Create an inventory of the largest static arrays and the modules that expose them (start with the list in `plan_parameter_refactor.md`, e.g. `AL_C.F90`, `AL_G.F90`, `contaminant_*`, `colm_*`).  
- Produce a simple spreadsheet or file (`docs/reports/refactor_parameters/array_inventory.csv`) listing: file, module, variable name, current dimension parameters, whether filled by file input, and estimated memory.  
- Identify arrays that are only used within a single component — these are lowest-risk to convert first.  

## Phase 2 — API and helper modules
Create a small set of helper modules to centralize allocation and shape data. Recommended modules:
- `src/parameters/core/array_shapes.f90` — stores integer parameters for array sizes used globally: NELE, NLAYER, NX, NY, etc. These are ordinary integer module variables set at initialization.  
- `src/util/memory/alloc_helpers.f90` — exposes procedures:
  - `allocate_global_arrays(status)` — allocate common global arrays; returns non-zero `status` on failure.  
  - `deallocate_global_arrays()` — safe deallocation of all global arrays (guards with `IF (ALLOCATED(...)) DEALLOCATE(...)
  - `reallocate_if_needed(array, newshape, stat)` — utility for resizing.  

Design notes:
- Keep shapes as plain integers exported from `array_shapes` so file readers can write them early.  
- Keep allocation routines idempotent: calling `allocate_global_arrays` multiple times will not reallocate already-allocated arrays unless size changed.  

## Phase 3 — File input module changes
Problems to solve: many file readers currently assume arrays already exist with fixed sizes. We must ensure readers either provide sizes or perform a two-step read.

Approaches (choose per-file based on complexity):
- Preferred: Add explicit header metadata to input files (recommended long-term). The header includes the grid dimensions and counts. File readers first parse this small header, set values in `array_shapes`, then call `allocate_global_arrays()` and finally read bulk arrays.  
- Alternative: Implement a pre-scan stage: open file, read only the parts that define dimensions (or count lines/records), then rewind/close and re-open to read full data after allocation. This is compatible with most legacy formats.  
- Fallback: Allocate with conservative upper bounds read from a small configuration file or environment variable and check at runtime if shape is sufficient; if not, fail with informative error.  

Recommended file-reader pattern (pseudo-Fortran):
  - Open file (with `NEWUNIT` where possible).  
  - Read dimensions or pre-scan to detect them.  
  - Set `array_shapes::NELE = value`, etc.  
  - CALL `allocate_global_arrays(stat)`; if `stat /= 0` call error reporter and exit.  
  - Read the remaining file contents directly into the allocated arrays.  

Implementation details and Fortran idioms:
- Prefer allocatable deferred-shape arrays in modules, e.g. `REAL(dp), ALLOCATABLE :: hflow(:,:)` or `hflow(:,:,:,:)` as needed.  
- Always use `ALLOCATE(hflow(1:NELE,1:NLAY), STAT=stat)` and check `stat`.  
- Use `ASSOCIATE` or explicit array sections when reading into arrays.  
- Avoid raw POINTER unless you need aliasing; use pointers only for alias shims to preserve old public symbol names if necessary.  

## Phase 4 — Replace static declarations with allocatable arrays
Two strategies to minimize code churn:
- Direct replacement: edit modules to change `REAL, DIMENSION(NELEE,NLAY) :: arr` to `REAL, ALLOCATABLE :: arr(:,:)` and update initialization code to allocate after `array_shapes` are set. Update all routines that rely on assumed-shape constants.  
- Shim layers (safer): create a new module exposing allocatable arrays, and in the old module create small pointer aliases or accessor functions that forward to the new arrays. That keeps most uses unchanged while the internals migrate. Example shim:
  - In `parameters/core/global_arrays_mod.f90`:
    REAL(dp), ALLOCATABLE :: bigarr(:,:)
  - In original module (compat layer):
    REAL(dp), POINTER :: bigarr_old(:,:)
    ! After allocation: bigarr_old => bigarr

Choose shim approach where many modules reference the array by USE; direct replacement is simpler when usage is localized.

## Phase 5 — Allocation lifecycle, safety and error handling
- All allocation calls must use `STAT` and translate positive `stat` values into readable errors via the existing `ERROR(...)` or `ALSTOP(...)` routines.  
- Implement a global initialization sequence:  
  1) parse control/driver input(s) (small files) to get grid sizes and component counts,  
  2) set numbers in `array_shapes`,  
  3) call `allocate_global_arrays(stat)`,  
  4) call component initialisers that assume arrays are present.  
- On failure: deallocate any partially allocated arrays and abort initialization with clear message.  
- At normal termination: call `deallocate_global_arrays()` to release memory.  

## Phase 6 — Build system and module dependencies
- Update `CMakeLists.txt` to include the new modules in correct order. Ensure `array_shapes.f90` and `alloc_helpers.f90` are compiled before modules that USE them.  
- Where build ordering is automatic (CMake Fortran module handling), ensure the new module files are placed under `src/parameters/core/` and added to the source file list if present.  

## Phase 7 — Testing & validation
Quality gates:
- Compile: Project must compile without errors on a target compiler (GNU Fortran, Intel, or other supported compiler).  
- Unit test: Create small Fortran unit tests that exercise allocation, deallocation, and error paths (for example, allocate intentionally large memory with `STAT` to check failure handling).  
- Integration smoke test: Run at least 2 existing examples (one small, one full) and compare outputs to baseline (numerical checksums or difference tolerances).  
- Memory test: run with a memory-checker (valgrind, ASAN-enabled builds where available) to detect leaks.  
- Performance test: ensure similar runtime and memory characteristics for representative runs.  

Suggested test harnesses and incremental checks:
- `tests/alloc_smoke/` — small program that reads a minimal config, calls allocation helpers, writes/reads arrays, deallocates.  
- A script to run an example, compute a checksum of key output files, and compare with a stored baseline.

## Backwards compatibility and rollbacks
- Keep original static modules untouched in a separate branch until migration for that module is validated. Use Git feature branches for each module.  
- Implement shim modules providing the old names mapped to new allocatables for a transitional period.  
- If runtime incompatibilities are detected, revert the module change only (git) and keep other modules migrated.  

## Timeline and prioritisation (suggested)
- Week 0: Inventory, choose 3 low-risk modules and create `array_shapes` and `alloc_helpers`.  
- Week 1: Migrate one small component (unit test) and update reader to pre-scan header.  
- Week 2–3: Migrate 3–5 medium modules (colm, link parts) with shim approach. Run tests after each.  
- Week 4: Migrate big global modules like `AL_C.F90` carefully; run full example suites and performance tests.  

## Example allocation pattern (recommended style)
This pattern should be used inside reader/initializer code. Use `NEWUNIT` where available to avoid hardcoded unit numbers.

```fortran
! Pseudocode pattern (conceptual)
INTEGER :: stat
! set shape variables first, e.g. array_shapes::NELE = nEle
CALL allocate_global_arrays(stat)
IF (stat /= 0) THEN
  CALL ERROR('FATAL', stat, 'Allocation of global arrays failed', ...)
END IF
! read file contents into allocated arrays
```

And for each allocation inside `allocate_global_arrays`:

```fortran
ALLOCATE(bigarr(NELE,NLAY), STAT=stat)
IF (stat /= 0) RETURN
```

Always guard deallocation:

```fortran
IF (ALLOCATED(bigarr)) DEALLOCATE(bigarr)
```

## Practical issues & recommendations
- File formats: if input files are binary and include header info, prefer that; if not, add minimal metadata to driver files. Document this change in `ford_project.md` and `docs/create_new_code_file.md`.  
- Memory footprint: converting to allocatable usually reduces memory (no large reserved arrays for maximum sizes), but watch fragmentation. Perform a large-run memory profile.  
- Thread safety: if any arrays are accessed concurrently (OpenMP/MPI), be careful to allocate before any parallel region and to ensure proper synchronization.  

## Next steps and suggested immediate actions
1. Create `docs/reports/refactor_parameters/array_inventory.csv` by scanning the `src/parameters` modules for large arrays.  
2. Add `src/parameters/core/array_shapes.f90` and `src/util/memory/alloc_helpers.f90` skeletons (small risk).  
3. Migrate one small module (example: a single `colm_*` file) end-to-end: update reader to pre-scan, allocate, read, test.  
4. Iterate with shims as needed and expand migration scope.  

## Requirements coverage
- Create plan file at `docs/reports/refactor_parameters/plan_allocate_arrays.md`: Done.  
- Include adjustments to file input modules and allocation lifecycle: Covered in Phase 3 and examples.  
- Provide incremental migration, testing, and rollback guidance: Provided above.

## Final notes
This plan is deliberate and conservative: it avoids a big-bang rewrite and recommends shims, pre-scans, and incremental validation to maintain a working codebase throughout the transition. If you want, I can now:
- generate the `array_inventory.csv` scaffold, or  
- add skeleton Fortran modules for `array_shapes` and `alloc_helpers` and a small smoke test program to the repo.

---
Generated by migration planning tooling for the SHETRAN repository.
