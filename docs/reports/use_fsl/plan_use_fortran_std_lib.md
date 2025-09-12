# Plan — where to use the Fortran Standard Library (stdlib) in Shetran

Status: focused, actionable mapping of stdlib modules to project components for safe, incremental adoption.

Summary recommendations
- Add stdlib as a managed dependency (fpm) or vendor a small set of modules into third_party/.
- Adopt stdlib initially for parsing, IO, indexing and tests — low-risk, high-value areas. Delay use of container-like APIs in hot compute loops.
- Start with: stdlib_strings, stdlib_filesystem, stdlib_unittest, stdlib_sorting, stdlib_stats. Evaluate stdlib_hashmap and stdlib_bitsets only when maps/bitsets are created once and used read-only in compute.

Per-module analysis and concrete uses

- zq_types.f90 (ZQTable_t, reader)
  - Use: stdlib_strings, stdlib_filesystem, stdlib_unittest.
  - Why: robust tokenisation, whitespace handling, numeric parsing and reliable file/path checks reduce parser edge cases.
  - Pattern: parse with split/strip → populate internal allocatable array → validate → expose view for hot loops.

- io_api.f90 / IOConfig_t
  - Use: stdlib_filesystem, stdlib_strings.
  - Why: portable path join/exists/is_dir/mkdir and safer config parsing. Simplifies cross-platform output-directory handling and filename construction.

- core_types.f90 / mesh_api.f90
  - Use: stdlib_sorting (argsort, sort), stdlib_bitsets (optional).
  - Why: deterministic reindexing, stable permutations for flattened arrays, efficient mask operations during setup. After building maps, pass contiguous arrays into OpenMP regions.

- state_api.f90 / State_t
  - Use: stdlib_unittest, stdlib_random (for tests).
  - Why: reproducible test data and lifecycle/unit tests for allocation/serialize/deserialize behaviours.

- params_types.f90 / soil_types.f90
  - Use: minimal; consider stdlib_hashmap for name→index mappings created at init and used read-only.
  - Why: reduces bespoke string lookup tables; avoid in inner compute loops.

- diagnostics / diag_types
  - Use: stdlib_stats, stdlib_unittest.
  - Why: standardized summary metrics (mean/variance/min/max) and unit tests for diagnostics logic. Use OpenMP-safe accumulation strategies (reductions or thread-local accumulation) for parallel stats.

- legacy_sglobal_adapter.f90
  - Use: stdlib_strings, optional stdlib_hashmap (init-time only).
  - Why: concise mapping of legacy names/indices to typed objects; unit-testable adapter code.

- sglobal.f90 (legacy)
  - Use: none initially — keep untouched; expose adapter until migration completes.

Performance & OpenMP guidance
- Use stdlib for setup/IO/indexing only. Do not introduce dynamic containers into inner loops.
- After stdlib-assisted setup, pass raw 1-D contiguous arrays (intent(in) / intent(inout)) into OpenMP regions for best memory and vectorisation behaviour.
- stdlib routines used at init are safe; if any stdlib component is used concurrently, ensure it is read-only during parallel regions.

Adoption phasing (recommended)
1. Phase 1 (days): Add stdlib, wire stdlib_strings and stdlib_filesystem into zq_types init_from_file and io_api. Add stdlib_unittest tests for parsing and IO.
2. Phase 2 (1–2 weeks): Use stdlib_sorting for mesh index building and stdlib_stats for diagnostics summaries. Expand tests.
3. Phase 3 (2–4 weeks): Evaluate stdlib_hashmap and stdlib_bitsets for adapter or static-metadata use; only adopt if maps are constructed once and read-only.

Concrete example snippets (to copy into modules)
- ZQ parsing (concept)
```fortran
use stdlib_strings, only: split, strip
call split(line, delimiters=',', tokens=toks)
name = strip(toks(1))
read(toks(2), *) value
```
- IO setup (concept)
```fortran
use stdlib_filesystem, only: join, is_dir, mkdir
outdir = join(cfg%base_dir, 'outputs')
if (.not. is_dir(outdir)) call mkdir(outdir)
```

Caveats
- Pin stdlib version or vendor specific modules if not using fpm.
- Benchmark and avoid container use in tight loops; keep stdlib usage primarily in init/IO/test code.
- Maintain strict ownership rules: modules that allocate must provide finalize routines.

Next immediate actions
- Add stdlib to fpm.toml or vendor selected modules into third_party/.
- Refactor ZQTable_t::init_from_file to use stdlib_strings and add unit tests with stdlib_unittest.
- Refactor io_api to use stdlib_filesystem for path handling.

If required, provide: ZQ reader skeleton using stdlib_strings and an fpm.toml patch to add stdlib.// filepath: c:\Users\tolstoi\Documents\Code\Shetran\docs\reports\use_fsl\plan_use_fortran_std_lib.md

## Plan — where to use the Fortran Standard Library (stdlib) in Shetran

Status: focused, actionable mapping of stdlib modules to project components for safe, incremental adoption.

Summary recommendations
- Add stdlib as a managed dependency (fpm) or vendor a small set of modules into third_party/.
- Adopt stdlib initially for parsing, IO, indexing and tests — low-risk, high-value areas. Delay use of container-like APIs in hot compute loops.
- Start with: stdlib_strings, stdlib_filesystem, stdlib_unittest, stdlib_sorting, stdlib_stats. Evaluate stdlib_hashmap and stdlib_bitsets only when maps/bitsets are created once and used read-only in compute.

Per-module analysis and concrete uses

- zq_types.f90 (ZQTable_t, reader)
  - Use: stdlib_strings, stdlib_filesystem, stdlib_unittest.
  - Why: robust tokenisation, whitespace handling, numeric parsing and reliable file/path checks reduce parser edge cases.
  - Pattern: parse with split/strip → populate internal allocatable array → validate → expose view for hot loops.

- io_api.f90 / IOConfig_t
  - Use: stdlib_filesystem, stdlib_strings.
  - Why: portable path join/exists/is_dir/mkdir and safer config parsing. Simplifies cross-platform output-directory handling and filename construction.

- core_types.f90 / mesh_api.f90
  - Use: stdlib_sorting (argsort, sort), stdlib_bitsets (optional).
  - Why: deterministic reindexing, stable permutations for flattened arrays, efficient mask operations during setup. After building maps, pass contiguous arrays into OpenMP regions.

- state_api.f90 / State_t
  - Use: stdlib_unittest, stdlib_random (for tests).
  - Why: reproducible test data and lifecycle/unit tests for allocation/serialize/deserialize behaviours.

- params_types.f90 / soil_types.f90
  - Use: minimal; consider stdlib_hashmap for name→index mappings created at init and used read-only.
  - Why: reduces bespoke string lookup tables; avoid in inner compute loops.

- diagnostics / diag_types
  - Use: stdlib_stats, stdlib_unittest.
  - Why: standardized summary metrics (mean/variance/min/max) and unit tests for diagnostics logic. Use OpenMP-safe accumulation strategies (reductions or thread-local accumulation) for parallel stats.

- legacy_sglobal_adapter.f90
  - Use: stdlib_strings, optional stdlib_hashmap (init-time only).
  - Why: concise mapping of legacy names/indices to typed objects; unit-testable adapter code.

- sglobal.f90 (legacy)
  - Use: none initially — keep untouched; expose adapter until migration completes.

Performance & OpenMP guidance
- Use stdlib for setup/IO/indexing only. Do not introduce dynamic containers into inner loops.
- After stdlib-assisted setup, pass raw 1-D contiguous arrays (intent(in) / intent(inout)) into OpenMP regions for best memory and vectorisation behaviour.
- stdlib routines used at init are safe; if any stdlib component is used concurrently, ensure it is read-only during parallel regions.

Adoption phasing (recommended)
1. Phase 1 (days): Add stdlib, wire stdlib_strings and stdlib_filesystem into zq_types init_from_file and io_api. Add stdlib_unittest tests for parsing and IO.
2. Phase 2 (1–2 weeks): Use stdlib_sorting for mesh index building and stdlib_stats for diagnostics summaries. Expand tests.
3. Phase 3 (2–4 weeks): Evaluate stdlib_hashmap and stdlib_bitsets for adapter or static-metadata use; only adopt if maps are constructed once and read-only.

Concrete example snippets (to copy into modules)
- ZQ parsing (concept)
```fortran
use stdlib_strings, only: split, strip
call split(line, delimiters=',', tokens=toks)
name = strip(toks(1))
read(toks(2), *) value
```
- IO setup (concept)
```fortran
use stdlib_filesystem, only: join, is_dir, mkdir
outdir = join(cfg%base_dir, 'outputs')
if (.not. is_dir(outdir)) call mkdir(outdir)
```

Caveats
- Pin stdlib version or vendor specific modules if not using fpm.
- Benchmark and avoid container use in tight loops; keep stdlib usage primarily in init/IO/test code.
- Maintain strict ownership rules: modules that allocate must provide finalize routines.

Next immediate actions
- Add stdlib to fpm.toml or vendor selected modules into third_party/.
- Refactor ZQTable_t::init_from_file to use stdlib_strings and add unit tests with stdlib_unittest.
- Refactor io_api to use stdlib_filesystem for path handling.

If required, provide: ZQ reader skeleton using stdlib_strings

{ 
Per-module analysis and concrete uses (mapped to actual src/ files)

Note: the project does not currently have an io_api module. References to io_api in earlier drafts should be mapped to existing IO modules (src/io/simulation_output.f90, src/io/meteorological_input.f90, src/util/mod_load_filedata.f90, and the platform helpers src/util/getdirqq*.f90) or to a new io_api facade module created during refactor.

- ZQ-related code
  - Files: src/compute/ZQmod.f90, src/compute/hydraulic_structures/zq_table_reader.f90, src/compute/hydraulic_structures/zq_data_types.f90, src/compute/hydraulic_structures/zq_interpolator.f90
  - Use: stdlib_strings, stdlib_filesystem, stdlib_unittest
  - Why: tokenisation and robust numeric parsing for table readers, portable path checks before OPEN, and unit tests for the ZQ parser/init.

- IO / input & output
  - Files: src/io/simulation_output.f90, src/io/meteorological_input.f90, src/util/mod_load_filedata.f90, src/util/getdirqq*.f90
  - Use: stdlib_filesystem (join, exists, is_dir, mkdir), stdlib_strings (config/token parsing), stdlib_unittest
  - Why: portable path construction and directory checks, safer parsing of input metadata. Consider introducing a new io_api.f90 wrapper that centralises stdlib usage and exposes a stable IOConfig_t for the rest of the codebase.

- Parameters & precision
  - Files: src/parameters/mod_parameters.f90 and other src/parameters/*.f90 (AL_C, BK_CW, CONST_SY, sglobal etc.)
  - Use: stdlib_kinds, stdlib_math, stdlib_unittest, stdlib_filesystem (only for any env/config helpers)
  - Why: replace bespoke SELECTED_REAL_KIND helpers with stdlib_kinds inside a retained precision_kinds module; import math/epsilon helpers and add unit tests to lock behaviour across compilers. Keep project-specific PARAMETER values in mod_parameters.

- Mesh, spatial setup & sorting
  - Files: src/simulation/framework/framework_spatial_setup.f90, src/simulation/framework/framework_element_sorting.f90, src/simulation/framework/framework_initialization.f90
  - Use: stdlib_sorting (argsort, sort), stdlib_bitsets (optional, init-time masks)
  - Why: deterministic element ordering, creation of permutation arrays for flattened state arrays; build masks/read-only maps at init time then pass contiguous arrays to compute kernels.

- State, lifecycle & framework
  - Files: src/simulation/framework/framework_shared.f90, src/simulation/framework/framework_component_initialization.f90, src/Shetran.f90
  - Use: stdlib_unittest, stdlib_random (test data)
  - Why: unit tests for allocation/initialization/finalize semantics and reproducible synthetic test inputs.

- Compute modules (hydraulics, subsurface, snow, sediment, contaminant)
  - Files: src/compute/* and subdirectories (hydraulic_flow, subsurface_flow, sediment, snow, contaminant)
  - Use: stdlib_strings for any init-time parsing, stdlib_sorting for ordering helpers, stdlib_stats for diagnostics summaries (init/tests)
  - Why: keep stdlib use to init/IO/test code. Hot compute loops should operate on raw contiguous arrays for OpenMP efficiency.

- Diagnostics & utilities
  - Files: src/util/utilsmod.f90, src/simulation/framework/framework_mass_balance.f90
  - Use: stdlib_stats, stdlib_unittest
  - Why: standardized summary metrics and testable diagnostic helpers. Use OpenMP-safe reduction patterns for parallel stats.

- Legacy globals adapter
  - Files: src/parameters/sglobal.f90 and many parameter modules that currently expose globals
  - Use: stdlib_strings, optional stdlib_hashmap (init-only)
  - Why: build a legacy_sglobal_adapter that maps old global names to new typed objects; create maps at startup and treat them read-only during runs.

Adoption notes mapped to repo
1. Phase 1 (days): Add stdlib dependency and update
   - zq_table_reader.f90 to use stdlib_strings for parsing,
   - simulation_output.f90 / meteorological_input.f90 to use stdlib_filesystem for path handling,
   - add stdlib_unittest tests for the above.
2. Phase 2 (1–2 weeks): Use stdlib_sorting in framework_element_sorting.f90 and framework_spatial_setup.f90; add stdlib_stats to utilsmod.f90 for summaries.
3. Phase 3 (2–4 weeks): Consider stdlib_hashmap or bitsets for adapter/index maps only if created once at init and used read-only.

Important: do not introduce container-like stdlib APIs into hot compute loops. Use stdlib to remove ad-hoc parsing, kind/math helpers, and platform-specific IO code while preserving ownership/alloc semantics in the project's modules.