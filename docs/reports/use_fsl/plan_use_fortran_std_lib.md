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