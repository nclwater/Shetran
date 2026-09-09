# Proposal: reorganising `src/` into descriptive, right-sized modules

Analysis and plan only. **No source file has been changed.**

## Scope

In scope: every Fortran source under `src/` except `src/visualisation/`,
`src/resource/` and `src/Shetran.f90`, as requested.

That is 40 files, 41,152 lines, holding **269 procedures**, **1,091
module-level variables** and **4 derived-type definitions** across 40
modules — one module per file. (`src/` as a whole is 51,206 lines; the
difference is the excluded directories.)

## Deliverables

| File | Contents |
|:-----|:---------|
| `functions.csv` | One row per procedure: current file, line range, its full FORD documentation block, and the proposed target file and module. |
| `variables.csv` | One row per module-level variable: current file, line, declared type, dimensions, its `!!` documentation, and the proposed target. |
| `types.csv` | One row per derived-type definition: current file, line range (a type moves as a block), component count, and the proposed target. |
| `constants_review.md` | The physical constants that are duplicated with **disagreeing values**, so unification can be decided on the science. |
| `proposal.md` | This overview. |

Supporting raw data, regenerable from the scripts in `scripts/`:
`_inventory_units.csv`, `_inventory_vars.csv`, `_inventory_types.csv`,
`_usage_units.csv`, `_usage_vars.csv`, `_usage_types.csv`,
`_usage_modules.csv`.

Regenerate everything with:

```
python3 scripts/inventory_units.py
python3 scripts/analyse_usage.py
python3 scripts/build_rename_proposal.py
```

Two further scripts check the result rather than produce it:
`scripts/call_graph.py` prints the intra-module call graph, and
`scripts/check_target_cycles.py` verifies that the proposed modules have no
circular dependencies (exit status 1 if any appear).

## Decisions taken

| Question | Decision |
|:---------|:---------|
| How to split the large modules | New sibling modules, and rename the opaque survivors too. Not submodules. |
| Duplicated physical constants | Preserve every value under a distinct name; record the conflicts separately for a later informed choice. |
| `AL_C` / `AL_D` shared state | Decompose by actual consumer, pushing component state into component directories. |
| Fortran unit numbers | One central table. |

### Why sibling modules rather than submodules

The large modules have almost no external consumers, so renaming and splitting
them is cheap:

| Module | Lines | Procedures | Files that `USE` it |
|:-------|------:|-----------:|--------------------:|
| `FRmod` | 6,086 | 47 | 3 |
| `VSmod` | 5,230 | 22 | 2 |
| `MNmod` | 5,139 | 30 | 1 |
| `SYmod` | 4,671 | 27 | 1 |
| `CMmod` | 3,342 | 17 | 2 |
| `OCmod` | 2,675 | 18 | 3 |

136 of the 235 top-level procedures are never referenced outside their own
file at all; counting the 34 nested procedures too, 170 of 269.

Submodules would have cost an `interface` block duplicated per procedure, plus
an update to `cmake/FortranDependencies.cmake`, whose `extract_fortran_modules`
regex recognises `MODULE` and `USE` but not `SUBMODULE`. That cost buys nothing
here.

`CMakeLists.txt` discovers sources with `file(GLOB_RECURSE ...)` and then
topologically sorts them by their `USE` graph, so **adding, moving, splitting
and renaming files requires no build-system change**. Re-run CMake to refresh
the glob.

## Proposed structure

106 files in place of the 40 in scope. Sizes below are procedure bodies
including their documentation blocks; module headers and declarations add to
each.

### `src/core/` — genuinely cross-cutting only

| File | Module | Vars | Holds |
|:-----|:-------|-----:|:------|
| `mod_parameters.f90` | `mod_parameters` | 51 | Kinds, buffer lengths, sentinels, mathematical constants, **all physical constants** |
| `array_limits.f90` | `array_limits` | 23 | `NELEE`, `LLEE`, `NVEE`, … capacity bounds |
| `build_info.f90` | `build_info` | 4 | `SHEVER`, `BDEVER`, `BANNER`, `RUNFIL` |
| `file_units.f90` | `file_units` | 50 | **Every** Fortran unit number, from `AL_C`, `AL_D` and `mod_error` |
| `run_context.f90` | `run_context` | 8 | Rundata paths, catchment name, `error_mode` |
| `simulation_clock.f90` | `simulation_clock` | 8 | `UZNOW`, `TIH`, `TTH`, `NSTEP`, `DTUZ`, `UZNEXT`, … |
| `runtime_flags.f90` | `runtime_flags` | 2 | The two timestep-reduction flags |
| `grid_topology.f90` | `grid_topology` | 7 | `NX`, `NY`, `ICMREF`, `ICMXY`, `ICMRF2`, `INGRID`, `NGDBGN` |
| `element_geometry.f90` | `element_geometry` | 20 | Element counts, plan dimensions, areas, `ZGRUND`, `ISORT` |
| `legacy_retained.f90` | `legacy_retained` | 26 | State documented as having no current producer or consumer |

`sglobal`, `AL_C`, `AL_D` and `AL_G` all disappear.

`runtime_flags` exists for one reason: `flag_runtime_reduction_errors` and
`flag_runtime_reduction_e1060` are written by error reporting and read by
timestep control, and both of those `USE` each other's area. A two-variable
module keeps that acyclic, which is what the comment in today's `sglobal`
already explains less directly.

`mod_parameters` keeps its name because you named it explicitly. It is the only
module left carrying the `mod_` prefix; renaming it to `parameters` later would
make the naming uniform.

### `src/util/` — domain-independent helpers

| File | Module | Procs | ~Lines |
|:-----|:-------|------:|-------:|
| `error/error_reporting.f90` | `error_reporting` | 3 | 307 |
| `error/error_status.f90` | `error_status` | 7 | 270 |
| `datetime.f90` | `datetime` | 5 | 203 |
| `interpolation.f90` | `interpolation` | 2 | 222 |
| `linear_algebra.f90` | `linear_algebra` | 7 | 492 |
| `random_numbers.f90` | `random_numbers` | 1 | 91 |
| `float_compare.f90` | `float_compare` | 13 | 340 |
| `platform_traps.f90` | `platform_traps` | 1 | 38 |

`mod_error` splits along a real seam: `error_status` holds the `errstat_*`
wrappers that inspect an `IOSTAT`/`STAT`, and `error_reporting` holds
`RAISE_ERROR`, `ERR_STOP` and the counters. `error_status` calls
`error_reporting`, not the reverse.

Per your instruction, component-specific error routines stay with their
component: `MNERR0`–`MNERR4` in `src/nitrate/mn_validation.f90`,
`SYERR0`–`SYERR3` in `src/sediment/sy_validation.f90`, `MUERR2` in
`src/contaminant/cm_input.f90`. Nothing is pulled into a global error module.

### `src/io/` — the SHETRAN input-file format layer

`mod_load_filedata` (an opaque name for the legacy `AL*` readers) and the
input half of `utilsmod` become the modules below. Two of
`mod_load_filedata`'s procedures are not input at all and go to `src/util/`
instead: `ALTRAP` to `platform_traps` and `ALINTP` to `interpolation`.

| File | Module | Procs | ~Lines |
|:-----|:-------|------:|-------:|
| `record_readers.f90` | `record_readers` | 6 (+1 nested) | 728 |
| `spatial_fields.f90` | `spatial_fields` | 4 | 525 |
| `input_validation.f90` | `input_validation` | 2 | 403 |
| `grid_arrays.f90` | `grid_arrays` | 2 | 361 |
| `timeseries_input.f90` | `timeseries_input` | 2 | 252 |
| `input_workspace.f90` | `input_workspace` | – | `IDUM`, `DUMMY` |

### `src/meteorology/` — new

Meteorological forcing is currently split between `rest` (the reader) and
`AL_D` (the values). Both halves come here: `met_input.f90` (`METIN`,
`READ_DATED_RECORD`, `RESIZE_MET_RECORD`) and `met_forcing.f90` (`TA`, `U`,
`VPD`, `RN`, `OBSPE`, `precip_m_per_s`, station mapping, input intervals).

### `src/driver/`

`rest` — the least descriptive name in the tree — is dissolved:

| Was | Becomes |
|:----|:--------|
| `TMSTEP` | `driver/timestep_control.f90` |
| `BALWAT` | `driver/water_balance.f90` |
| `EXTRA_OUTPUT` | `driver/run_summary.f90` |
| `METIN`, `READ_DATED_RECORD`, `RESIZE_MET_RECORD` | `meteorology/met_input.f90` |

`getdirqq` becomes `driver/command_line.f90`; `run_sim` becomes
`driver/simulation_driver.f90`.

### `src/frame/` — `FRmod` split six ways

| File | Module | Procs | ~Lines |
|:-----|:-------|------:|-------:|
| `frame_setup.f90` | `frame_setup` | 6 (+4 nested) | 1,008 |
| `frame_geometry.f90` | `frame_geometry` | 4 | 1,370 |
| `frame_output.f90` | `frame_output` | 3 (+20 nested) | 972 |
| `legacy_result_files.f90` | `legacy_result_files` | 3 | 585 |
| `mass_balance_report.f90` | `mass_balance_report` | 1 | 278 |
| `run_control.f90` | `run_control` | – | 24 vars: title, `BEX*` switches, hotstart settings |

The five `IN*` component-setup routines move **out** of the frame and into the
component they set up, which is what your "keep functionality for X together"
rule asks for:

| Routine | New home |
|:--------|:---------|
| `INET` | `src/evapotranspiration/et_input.f90` |
| `INSM` | `src/snow/snow_input.f90` |
| `INBK` | `src/overland_channel/bank_setup.f90` |
| `INCM`, `MUERR2` | `src/contaminant/cm_input.f90` |
| `INPL` | `src/contaminant/cm_plant.f90` |

This is safe: `FRmod` already `USE`s `OCmod`, `SMmod`, `ETmod`, `VSmod` and
`CMmod`, and nothing outside `src/driver/` and `Shetran.f90` uses `FRmod`. The
dependency direction is unchanged.

### Component directories

Seven directories take 27 of the 40 in-scope files and turn them into 69. The
shape is the same in each: procedures split by role — input, process, solver,
validation, output — the component's own state gathered into named data
modules, and the parts of `AL_C`, `AL_D` and `FRmod` that only this component
uses pulled in.

| Directory | Source files | New files | Split of |
|:----------|-------------:|----------:|:---------|
| `src/evapotranspiration/` | 1 | 4 | `ETmod` |
| `src/snow/` | 1 | 4 | `SMmod` |
| `src/overland_channel/` | 5 | 14 | `OCmod`, `OCmod2`, `OCQDQMOD`, `ZQmod`, `oc_row_width` |
| `src/subsurface/` | 1 | 9 | `VSmod` |
| `src/sediment/` | 3 | 9 | `SYmod`, `CONST_SY`, `SED_CS` |
| `src/contaminant/` | 15 | 20 | `CMmod` plus the 14 opaque `COLM_*` / `LINK_*` / `*_CC` data modules |
| `src/nitrate/` | 1 | 9 | `MNmod` |

In the tables below, **Procs** counts top-level procedures with contained
procedures shown separately, **~Lines** is the sum of the procedure bodies
including their documentation blocks, and **Vars** is module-level variables.

#### `src/evapotranspiration/` — `ETmod` split four ways

| File | Module | Procs | ~Lines | Vars | Holds |
|:-----|:-------|------:|-------:|-----:|:------|
| `et_process.f90` | `et_process` | 5 | 814 | – | Penman–Monteith evaporation, interception and transpiration |
| `et_config.f90` | `et_config` | – | – | 41 | ET parameters and the time-varying vegetation tables |
| `et_input.f90` | `et_input` | 1 | 373 | – | Reads the ET input data and initialises ET state (`INET`) |
| `et_state.f90` | `et_state` | 1 | 30 | 29 | Canopy, vegetation and evaporation state shared with other components |

`ETmod`'s 45 module-level variables divide without argument: 41 are input
parameters and vegetation tables, and four are physical constants that leave
for `mod_parameters` (`LAMDA`, `GAMMA`, `RHO`, `CP`; see `constants_review.md`
§3–5). The 41 go to a data-only `et_config`, matching `vs_config` and
`sy_config`, rather than staying with the reader: `INET` writes them and
`et_process` reads them, so parking them in `et_input` would make those two
modules mutually dependent.

`ETIN` is **not** a reader despite its name — it is the per-element ET and
interception step, called only from `ETSIM`, and it calls `ET` and `SMIN`. It
therefore belongs in `et_process`, which leaves `et_input` holding exactly one
procedure: `INET`, the actual reader, arriving from `FRmod`.

The evaporation *state* is not in `ETmod` at all today — `et_state` is
assembled entirely from `AL_C` (12 variables: `PNETTO`, `ERUZ`, `DRAINA`,
`CLAI`, …) and `AL_D` (17: `PE`, `EINT`, `ERZ`, `CSTORE`, …), together with
`initialise_al_c3`, the existing initialiser that already covers exactly that
group.

#### `src/snow/` — `SMmod` split four ways

| File | Module | Procs | ~Lines | Vars | Holds |
|:-----|:-------|------:|-------:|-----:|:------|
| `snowmelt.f90` | `snowmelt` | 4 | 657 | – | Degree-day and energy-budget melt, and meltwater routing |
| `snow_config.f90` | `snow_config` | – | – | 18 | Snowmelt parameters and initial-snowpack settings |
| `snow_input.f90` | `snow_input` | 1 | 140 | – | Reads the snowmelt data file (`INSM`) |
| `snow_state.f90` | `snow_state` | – | – | 10 | Pack depth, temperature, density and meltwater-slug state |

The same shape as evapotranspiration: 18 input variables into a data-only
`snow_config`, eight physical constants out to `mod_parameters` (`RHOA`,
`RHOW`, `CPA`, `CPW`, `CPI`, `LWI`, `LVW`, `HFG`), and the pack state gathered
in `snow_state` from `AL_C` (`ISPACK`), `AL_D` (`SD`, `TS`, `SF`, `MSM`,
`RHOSAR`, `NSMT`, `NSMC`) and `SMmod`'s own `smelt` and `tmelt`. `INSM` arrives
from `FRmod` as the whole of `snow_input`; `SMIN`, like `ETIN`, is a per-element
process wrapper — it is called from `ETIN` and calls `SM` and `SMET` — so it
goes to `snowmelt`.

Four of the eight constants leaving snow and three of the four leaving ET are
duplicated across the two components. Three of those pairs disagree in value
(air density, water density, latent heat of vaporisation); the fourth,
`CP`/`CPA`, is identical. All keep distinct names, so nothing moves
numerically; `constants_review.md` records which is which.

#### `src/overland_channel/` — five modules re-cut into 14

| File | Module | Procs | ~Lines | Vars | Holds |
|:-----|:-------|------:|-------:|-----:|:------|
| `oc_driver.f90` | `oc_driver` | 5 | 610 | 3 | Setup, the implicit row solver and its workspace |
| `oc_input.f90` | `oc_input` | 3 | 764 | – | The overland/channel data file and its boundary records |
| `oc_validation.f90` | `oc_validation` | 4 | 447 | – | Consistency checks on input and geometry |
| `oc_indexing.f90` | `oc_indexing` | 3 | 248 | 6 | Row-solver ordering, row widths, link-number lookup |
| `oc_boundaries.f90` | `oc_boundaries` | 3 | 323 | 14 | Time-varying stage and flow boundary conditions |
| `oc_cross_sections.f90` | `oc_cross_sections` | 1 | 191 | 5 | Width–depth cross-section and conveyance tables |
| `oc_node_solver.f90` | `oc_node_solver` | 9 | 743 | – | Per-node water-level solution and its accessors |
| `oc_discharge.f90` | `oc_discharge` | 6 | 950 | – | Grid, link, bank, confluence and weir face discharge |
| `oc_conveyance.f90` | `oc_conveyance` | 1 | 101 | 3 | `CONVEYAN` and the depth thresholds it shares with the node solver |
| `oc_stage_discharge.f90` | `oc_stage_discharge` | 3 | 334 | – | Stage–discharge relations and their derivatives |
| `zq_tables.f90` | `zq_tables` | 2 | 313 | 13 | Reservoir and weir stage–discharge lookup tables |
| `bank_setup.f90` | `bank_setup` | 1 | 266 | – | Explicit-bank element data and bankfull geometry |
| `oc_state.f90` | `oc_state` | – | – | 16 | Water levels, face discharges, roughness, flow derivatives, `dtoc` |
| `channel_geometry.f90` | `channel_geometry` | – | – | 9 | Link length, width, bed elevation, bank-element mapping |

The five modules are re-cut by role rather than preserved. `OCmod`'s readers
and checks become `oc_input` and `oc_validation`, its driver `oc_driver` and
its ordering `oc_indexing`; `OCmod2` divides into the per-node solver, the
discharge relations and the conveyance leaf they share; `OCQDQMOD` becomes
`oc_stage_discharge`; `ZQmod` becomes `zq_tables`; `oc_row_width`'s single
function `MAX_ACTIVE_ROW_WIDTH` joins `oc_indexing`. `INBK` arrives from
`FRmod` as `bank_setup`.

`CONVEYAN` needs its own module because it is called from both sides —
`OCCODE` in `oc_node_solver` and four of the discharge routines — and `DZMIN`,
`RDZMIN` and `H23MIN` travel with it for the same reason (`OCFIX` reads
`DZMIN`). Without that leaf, `oc_node_solver` and `oc_discharge` would `USE`
each other.

`OCmod2` is `USE`d by nine files — more than any other of the modules being
split, though the data modules `sglobal` (36), `mod_parameters` (31),
`mod_error` (29), `AL_D` (13), `AL_C` (12) and `AL_G` (10) reach further — so
this is the split that touches most of the tree. What those nine consumers
actually need is narrow: `HRFZZ` and the accessors `gethrf`, `sethrf`, `getqsa`,
`setqsa`. Keeping the arrays in `oc_state` and the accessors in
`oc_node_solver` turns those nine `USE` lines into imports from two small
modules rather than from one 2,000-line one.

`zq_tables` is where the duplicated `ZQTableRef` — one declaration in `AL_D`,
one in `ZQmod`, with different documented meanings — collides and has to be
resolved; see "Things found on the way" below.

#### `src/subsurface/` — `VSmod` split nine ways

| File | Module | Procs | ~Lines | Vars | Holds |
|:-----|:-------|------:|-------:|-----:|:------|
| `vs_driver.f90` | `vs_driver` | 4 | 885 | – | Column iteration and the subsurface mass balance |
| `vs_input.f90` | `vs_input` | 3 (+1 nested) | 1,056 | 6 | The subsurface data and initial-condition files |
| `vs_column_solver.f90` | `vs_column_solver` | 4 | 1,062 | 1 | Tridiagonal pressure-head correction |
| `vs_connectivity.f90` | `vs_connectivity` | 2 (+1 nested) | 889 | – | Cell, layer and link connectivity of the mesh |
| `vs_sources.f90` | `vs_sources` | 6 | 727 | – | Well, spring, boundary and interception source terms |
| `vs_soil_tables.f90` | `vs_soil_tables` | 1 | 297 | 11 | Soil hydraulic-property lookup tables |
| `vs_state.f90` | `vs_state` | 2 | 107 | 34 | Pressure head, water content and the subsurface fluxes |
| `vs_config.f90` | `vs_config` | – | – | 66 | Soil parameters, initial-condition options, setup switches |
| `vs_boundaries.f90` | `vs_boundaries` | – | – | 25 | Time-varying lateral, base and well boundary series |

For subsurface, the split follows the legacy `.INC` groupings that the source
still marks in comments (`VSCOM1`, `VSSOIL`, `VSINIT`) plus the call structure,
so the boundaries match how the code is already organised internally.

`VSmod` carries 112 module-level variables, and the split separates the three
kinds it mixes: 66 configuration values, 25 boundary series, and the state
proper (11 soil tables, 6 reader buffers, 1 solver limit and 3 state
variables). `vs_state` is the largest single beneficiary of the `AL_C`
decomposition — 31 of its 34 variables (`VSPSI`, `VSTHE`, `QVSV`, `DELTAZ`,
`NLYR`, …) come from there — and the most widely read of the new state
modules: frame, driver, ET, overland/channel, contaminant and the visualisation
interface all reference it.

#### `src/sediment/` — `SYmod`, `CONST_SY` and `SED_CS` into nine

| File | Module | Procs | ~Lines | Vars | Holds |
|:-----|:-------|------:|-------:|-----:|:------|
| `sy_driver.f90` | `sy_driver` | 3 | 505 | – | Timestep driver and workspace allocation |
| `sy_input.f90` | `sy_input` | 3 | 567 | – | The sediment data groups and their initialisation |
| `sy_validation.f90` | `sy_validation` | 4 (+1 nested) | 1,058 | – | Consistency checks on input and state (`SYERR0`–`SYERR3`) |
| `sy_transport_capacity.f90` | `sy_transport_capacity` | 6 (+2 nested) | 1,013 | 6 | Transport capacity, critical shear, settling velocity |
| `sy_hillslope.f90` | `sy_hillslope` | 3 | 517 | – | Detachment, overland routing, fine-sediment exchange |
| `sy_channel.f90` | `sy_channel` | 4 (+1 nested) | 781 | – | Channel routing, bed layers and bank erosion |
| `sy_state.f90` | `sy_state` | – | – | 21 | Mobile, loose and bed sediment state (was `SED_CS`) |
| `sy_config.f90` | `sy_config` | – | – | 44 | Options, thresholds, per-class and per-soil parameters |
| `sy_workspace.f90` | `sy_workspace` | – | – | 21 | Per-timestep work arrays shared between the routines |

`CONST_SY` disappears entirely: its four constants (`GRAVTY`, `RHOSED`,
`RHOWAT`, `VISCOS`) move to `mod_parameters`, all four renamed.
`SED_CS` — the module that actually holds sediment state — becomes `sy_state`,
20 variables plus `SBERR` from `AL_C`. `SYmod`'s own 71 variables divide into
`sy_config` (44), `sy_workspace` (21) and six values that stay beside the
formulas using them in `sy_transport_capacity` — the derived coefficients
`K1_syovtr`, `K3_syovtr` and `K4_syovtr`, plus the first-call flags
`FIRST_syackw`, `FIRST_syfine` and the cached `WSED_syfine`, as recorded in
`constants_review.md`.

#### `src/contaminant/` — `CMmod` and 14 data modules into 20

| File | Module | Procs | ~Lines | Vars | Holds |
|:-----|:-------|------:|-------:|-----:|:------|
| `cm_input.f90` | `cm_input` | 3 | 1,312 | – | The contaminant data file (`CMRD`, `INCM`, `MUERR2`) |
| `cm_driver.f90` | `cm_driver` | 1 | 133 | – | One subsurface timestep for every active contaminant |
| `cm_column.f90` | `cm_column` | 6 | 1,187 | 13 | Column advection–dispersion–reaction assembly and solve |
| `cm_channel.f90` | `cm_channel` | 4 | 965 | 6 | Channel-link assembly and solve (`LINK` ↔ `SNL3`) |
| `cm_plant.f90` | `cm_plant` | 4 | 364 | – | Two-compartment plant uptake (`PLCOLM`, `PLANT`, `PLPREP`, `INPL`) |
| `cm_sorption.f90` | `cm_sorption` | 2 | 163 | – | Soil and sediment sorption retardation factors |
| `cm_parameters.f90` | `cm_parameters` | 1 | 58 | 35 | Contaminant properties and shared transport state |
| `column/cm_column_geometry.f90` | `cm_column_geometry` | 2 | 92 | 12 | Column-base, face-overlap and well-flow geometry |
| `column/cm_column_previous.f90` | `cm_column_previous` | 1 | 77 | 10 | Previous-timestep column water state |
| `column/cm_column_state.f90` | `cm_column_state` | – | – | 38 | Per-column concentrations and source/sink terms |
| `column/cm_column_water.f90` | `cm_column_water` | – | – | 35 | Per-column water state used by the transport solver |
| `column/cm_column_scaling.f90` | `cm_column_scaling` | – | – | 20 | Scaling factors and active-cell range for the column solver |
| `column/cm_column_equations.f90` | `cm_column_equations` | – | – | 12 | Coupled equation workspace for the column solver |
| `link/cm_link_state.f90` | `cm_link_state` | – | – | 63 | Three-compartment workspace for one stream link |
| `link/cm_link_water.f90` | `cm_link_water` | – | – | 9 | Bed geometry and retained water state for link preparation |
| `link/cm_link_scaling.f90` | `cm_link_scaling` | – | – | 2 | Nondimensional link length and bank-cell thickness |
| `cm_plant_state.f90` | `cm_plant_state` | – | – | 29 | Plant-uptake state and parameters |
| `cm_bank_geometry.f90` | `cm_bank_geometry` | – | – | 5 | Bank geometry and exchange indices |
| `cm_sediment_previous.f90` | `cm_sediment_previous` | – | – | 6 | Previous-timestep sediment state used by the equations |
| `cm_solver_flags.f90` | `cm_solver_flags` | – | – | 5 | Switches selecting the contaminant and nitrate paths |

The `column/` and `link/` subdirectories stay, holding the per-column and
per-link data modules; the procedure-bearing files and the remaining data
modules sit directly in `src/contaminant/`. The `cm_column_*` and `cm_link_*`
prefixes make the grouping legible either way. The full mapping of the opaque
data modules:

| Was | Becomes |
|:----|:--------|
| `CONT_CC` | `cm_parameters` |
| `COLM_CC` | `cm_column_state` |
| `COLM_CC1` | `cm_column_equations` |
| `COLM_C1` | `cm_column_scaling` |
| `COLM_C2` | `cm_column_water` |
| `COLM_CG` | `cm_column_geometry` |
| `COLM_CO` | `cm_column_previous` |
| `LINK_CC` | `cm_link_state` |
| `LINK_CC1` | `cm_link_scaling` |
| `LINK_CW` | `cm_link_water` |
| `BK_CW` | `cm_bank_geometry` |
| `SED_CO` | `cm_sediment_previous` |
| `IS_CC` | `cm_solver_flags` |
| `PLANT_CC` | `cm_plant_state` |

`CMmod`'s 17 procedures go to six files and its 19 module-level variables to
`cm_column` (13) and `cm_channel` (6); nothing arrives from `AL_C` or `AL_D`.
`INCM`, `MUERR2` and `INPL` arrive from `FRmod`. The `LINK` ↔ `SNL3` mutual
recursion stays inside `cm_channel`.

#### `src/nitrate/` — `MNmod` split nine ways

| File | Module | Procs | ~Lines | Vars | Holds |
|:-----|:-------|------:|-------:|-----:|:------|
| `mn_driver.f90` | `mn_driver` | 6 | 779 | – | Allocation, initialisation and the per-timestep sequence |
| `mn_input.f90` | `mn_input` | 3 | 918 | – | Static data and the scheduled N and C additions |
| `mn_validation.f90` | `mn_validation` | 5 | 1,114 | – | Consistency checks (`MNERR0`–`MNERR4`) |
| `mn_environment.f90` | `mn_environment` | 6 | 586 | – | Soil temperature and the temperature/moisture factors |
| `mn_nitrogen.f90` | `mn_nitrogen` | 4 | 650 | – | Ammonium, nitrification, mineralisation, litter nitrogen |
| `mn_organic_matter.f90` | `mn_organic_matter` | 3 | 394 | – | Litter, humus and manure carbon turnover, CO₂ production |
| `mn_plant.f90` | `mn_plant` | 2 | 246 | – | Plant nitrogen uptake |
| `mn_output.f90` | `mn_output` | 1 | 261 | – | Nitrate and carbon budget output files |
| `mn_state.f90` | `mn_state` | – | – | 57 | Carbon and nitrogen pools, rates, per-cell nitrate state |

`MNmod` is the most self-contained module in the tree. It `USE`s only
`sglobal`, `mod_parameters`, `mod_error`, `mod_load_filedata` and `utilsmod`,
and the only file that `USE`s it is `CMmod`, with `ONLY: MNCONT,
MNINITIALISE, MNISINITIALISED`. Its 30 procedures divide by subject; all of its
state — including the three derived types `MN_CONFIG_TYPE`,
`MN_WORKSPACE_TYPE` and `MN_PLANT_STATE_TYPE` and their instances `MN_CONFIG`,
`MN_WORK` and `MN_PLANT_STATE`, which already group most of it — becomes
`mn_state`. Nothing
arrives from `AL_C` or `AL_D`, which is why the execution order starts here.

#### What the components gain from `AL_C` and `AL_D`

96 of the variables in the dissolved `AL_*` modules are component state,
mostly touched only by their own component and the frame:

| Component | Target module | From `AL_C` | From `AL_D` | Examples |
|:----------|:--------------|------------:|------------:|:---------|
| Evapotranspiration | `et_state` | 12 | 17 | `PNETTO`, `ERUZ` / `PE`, `EINT`, `CSTORE` |
| Snow | `snow_state` | 1 | 7 | `ISPACK` / `SD`, `TS`, `SF`, `MSM` |
| Overland/channel | `channel_geometry` | 9 | – | `CLENTH`, `CWIDTH`, `ZBFULL` |
| Overland/channel | `oc_state` | 2 | 8 | `QOC`, `ARXL` / `QMAX`, `LCODEX`, `DQ0ST` |
| Overland/channel | `oc_boundaries` | – | 2 | `NOCBCC`, `NOCBCD` |
| Overland/channel | `zq_tables` | – | 6 | `NoZQTables`, `ZQTableLink`, `ZQweirSill` |
| Subsurface | `vs_state` | 31 | – | `VSPSI`, `VSTHE`, `QVSV`, `DELTAZ` |
| Sediment | `sy_state` | 1 | – | `SBERR` |

Contaminant and nitrate gain nothing from `AL_C` or `AL_D`: their state is
already in their own modules.

#### Out-of-scope files that have to be touched

Thirteen files outside the scope `USE` in-scope modules, so their `USE` lines
have to follow the renames. No other change is needed in any of them.

**`src/Shetran.f90`** — every one of its eight `USE` lines names a module that
moves or dissolves:

| Currently | Becomes |
|:----------|:--------|
| `SGLOBAL` (whole-module) | the `core/` modules it actually needs |
| `AL_D`: `nstep` | `simulation_clock` |
| `mod_load_filedata`: `ALTRAP` | `platform_traps` |
| `GETDIRQQ`: `GET_DIR_AND_CATCH`, `RUNDATA_FROM_FILE_DIALOG` | `command_line` |
| `FRmod`: `FROPEN`, … | `frame_setup` |
| `REST`: `extra_output` | `run_summary` |
| `RUN_SIM`: `SIMULATION` | `simulation_driver` |
| `OCmod`: `FINALISE_OCSIM_WORKSPACE` | `oc_driver` |

**`src/visualisation/visualisation_interface_left.f90`** is the only
visualisation file that reaches into the model state:

| Currently | Becomes |
|:----------|:--------|
| `CONT_CC`: `cccc`, `ncon`, `ssss` | `cm_parameters` |
| `SED_CS`: `dls`, `gnu`, `nsed`, `qsed` | `sy_state` |
| `CONST_SY`: `rhosed` | `mod_parameters` as `RHO_SEDIMENT` |
| `OCmod2`: `hrfzz` | `oc_state` |
| `AL_C`, `AL_D`, `AL_G`, `SGLOBAL` (43 names) | `vs_state`, `et_state`, `snow_state`, `channel_geometry`, `oc_state`, `grid_topology`, `element_geometry`, `simulation_clock`, `run_control`, `run_context`, `build_info`, and `file_units` for `cmd` and `syd` |
| `MOD_ERROR`: `errstat_alloc`, `errstat_dealloc`, `errstat_rewind`, `errstat_read` / `RAISE_ERROR`, `ERRLVL_fatal` / `FID_logfile` | `error_status` / `error_reporting` / `file_units` |

**Eleven further `src/visualisation/` files** `USE MOD_ERROR` for the
`errstat_*` wrappers and so follow the `error_status` rename:
`visualisation_read`, `visualisation_structure`, `visualisation_extras`,
`visualisation_hdf5`, `visualisation_map`, `visualisation_metadata`,
`visualisation_pass`, `visualisation_read_parser`,
`visualisation_interface_centre`, `visualisation_interface_right` and
`visualisation_interface_left` itself. `visualisation_metadata` also imports
`errstat_fileclose`. Their `MOD_PARAMETERS` imports are unaffected, because
`mod_parameters` keeps its name — which is a second reason to leave it alone.

#### How the counts are measured

**Derived types are inventoried separately.** Four types are defined in scope
— `MN_CONFIG_TYPE`, `MN_WORKSPACE_TYPE` and `MN_PLANT_STATE_TYPE` in `MNmod`,
`OCSIM_WORKSPACE_TYPE` in `OCmod` — holding 57 components between them. They
are listed in `types.csv` with a line range, because a type moves as a block,
and their components are *not* counted as module-level variables. Each type
travels with its instance: `OCSIM_WORKSPACE_TYPE` to `oc_driver` alongside
`OCSIM_WORKSPACE`, the three `MN_*_TYPE`s to `mn_state`.

**References are reported as a bracket, not a single number.**
`scripts/analyse_usage.py` parses `USE` statements rather than merely detecting
them, so it knows what each file actually imports:

| Column | Meaning |
|:-------|:--------|
| `referenced_by` | The owning module is `USE`d *and* an `ONLY` list names the entity. The reference is demonstrably real — a lower bound. |
| `possibly_referenced_by` | The name is reachable only through a whole-module `USE`, so the token may equally be a local or a dummy argument — the residual upper bound. |

Renames are followed (`USE OCMOD2, ONLY: hrf => hrfzz` counts a use of `hrf`
as a reference to `hrfzz`), continuation lines are joined before the `ONLY`
list is read, identifiers inside character literals are ignored, and
transitive re-export is honoured: a bare `USE AL_C` also imports what `AL_C`
itself took from `sglobal`, which is how `CMmod` reaches the capacity
parameters.

Of the 1,091 variables, 507 have at least one confirmed reference, 214 only
possible ones, and 370 none at all. A placement resting solely on a
`possibly_referenced_by` hit still deserves a look at the source: `CMmod`, for
example, contains tokens named `IDUM`, `DUMMY`, `NPL` and `PPHI` that are its
own locals, not `MNmod`'s — it imports exactly three procedure names from
`MNmod`, so none of them are counted.

#### Open point: duplicated plant state

`cm_plant_state` (`PLANT_CC`) and `MNmod`'s `MN_PLANT_STATE` declare four
identically named quantities — `NPL`, `NPLTYP`, `GMCPBB` and `PFONE` — in
separate storage; `MNmod` does not `USE` `PLANT_CC`. The move keeps both,
unchanged, under their new names. It is recorded here because co-locating and
renaming the modules is what makes the duplication visible. The four are not
one finding, though.

**`NPL`, `NPLTYP` and `PFONE` are redundant, not ambiguous.** `FRmod:INPL` and
`MNmod:MNPLANTINITIALISE` derive them by the same statements from the same
run-wide inputs (`NVC`, `PLAI`) over the same element range — `INPL`'s
`total_no_links+1 .. total_no_elements` is `MNPLANTINITIALISE`'s
`NLF+1 .. NEL` under the renaming `CMmod` applies on import. Only the route to
slot 2 of `NPLTYP` differs: a declaration initialiser in `PLANT_CC`, an
explicit loop in `MNmod`. Merging the storage would nonetheless change
behaviour, because `INPL` is gated on the never-assigned `ISPLT` while the
nitrate path is gated on `ISMN`, so `MNmod` would start depending on whether
`INPL` ran. De-duplication therefore belongs in a later commit, not this one.

**`GMCPBB` is a false friend.** `PLANT_CC`'s is a scalar workspace recomputed
per column, plant slot and contaminant inside `PLCOLM`; `MNmod`'s is persistent
`(NELEE,NPELEE)` state. The contaminant-side counterpart of
`MN_PLANT_STATE%GMCPBB` is `GMCBBO`, which shares its shape, lifetime and
initialising formula; the two differ only in the normaliser — `PF2MAX` (2/6/10
by plant type) against `CLAIMX` (2.0 flat) — and in a crop-development factor
carried only by the nitrate path. That difference is the genuine modelling
question.

The full trace is in `docs/todo/issue_plant_declarations.md`.

## Why this is low risk

**No dependency cycles are created.** Splitting one module into siblings turns
internal calls into `USE` dependencies, so a cycle between two proposed modules
would be a hard blocker. Two kinds of edge can close one: a *call* from a
procedure in one target module into another, and a *reference* from a procedure
in one target module to a module-level variable placed in another. Both were
checked over the proposed placement.

`scripts/call_graph.py` builds the intra-module call graph. Across all ten
multi-procedure modules it finds mutual recursion in two groups only, and each
lands inside a single target module:

- `FRRESC` / `FRRESP` ↔ `res_write_check` (two pairs, three procedures) → all
  three in `legacy_result_files`
- `LINK` ↔ `SNL3` → both in `cm_channel`

Mutual recursion is not the whole test, though: a cycle can also be closed by
two different procedures, or by where a variable is put.
`scripts/check_target_cycles.py` resolves every identifier in every procedure
body against the entities of its own module — ignoring locals and dummy
arguments — and looks for strongly connected components over the *proposed*
modules. It found six cycles in an earlier draft of this plan. Each is resolved
by a placement recorded above, and several of those placements exist for no
other reason:

| Cycle | What closed it | Resolution |
|:------|:---------------|:-----------|
| `et_input` ↔ `et_process` | `ETSIM` calls `ETIN`, `ETIN` calls `ET`; `ET` reads the ET parameters | `ETIN` is a process routine and moves to `et_process`; the parameters move to `et_config` |
| `snow_input` ↔ `snowmelt` | `SMIN` calls `SM`/`SMET`, which read the snow parameters | `SMIN` moves to `snowmelt`; the parameters move to `snow_config` |
| `legacy_result_files` ↔ `mass_balance_report` | `FRMB` calls `FRRESP`, which reads `PREVTM` | `PREVTM` is `FRRESP`'s own state, so it goes to `legacy_result_files` |
| `oc_boundaries` ↔ `oc_driver` | `OCSIM` calls `OCABC`, which reads `dtoc` | `dtoc` goes to `oc_state` |
| `oc_node_solver` ↔ `oc_discharge` | `OCCODE` calls `CONVEYAN`; `OCFIX` reads `DZMIN` | `CONVEYAN` and the depth thresholds become the leaf `oc_conveyance` |
| `vs_driver` ↔ `vs_column_solver` | `VSCOLM` reads `errcntallowed` | the parameter goes to `vs_column_solver`, its only reader |

With those placements `check_target_cycles.py` reports no cycles, and every
remaining edge points from a driver to a leaf.

**Placement is derived from measured usage, not guesswork.**
`scripts/analyse_usage.py` records, for each of the 1,091 variables, which
files reference it *and* can actually see it through their `USE` statements.
That is what showed `AL_D` is not actually global: 51 of its 154 variables are
used only by the frame, 27 only by frame plus driver, and 14 by nothing at
all. The `referenced_by` and `possibly_referenced_by` columns carry this
evidence per row.

**The move is numerically neutral.** Every constant keeps its value; only names
and locations change. The value conflicts surfaced by co-locating the constants
are documented in `constants_review.md` and deliberately left unresolved.

## Things found on the way, not acted on

Kept as-is per your instruction, but worth recording:

- **`DINET`, `DINOC`, `DOCIN`** (`FRmod`) are stand-in routines for a disabled
  component. `FRINIT` calls `DINET` when ET is off and `DINOC` when
  overland/channel is off; `DINOC` only prints a string, and `DINET` prints one
  and sets `BMETAL = .TRUE.`, so it is not quite inert. `DOCIN` has an empty
  body and **no caller at all**.
- **26 variables have no producer or consumer** and are documented as such in
  the source. They are quarantined in `core/legacy_retained.f90` rather than
  deleted, which makes them easy to remove as one later commit.
- **`ETmod`'s `RHO = 1.2`, `CP = 1003.`, `GAMMA = 0.659` and
  `LAMDA = 2465000.`** are default-real literals assigned to `DOUBLEPRECISION`
  parameters. `CP` and `LAMDA` are exactly representable in single precision so
  they are unaffected, but `RHO` stores 1.2000000476837158 and `GAMMA` stores
  0.6589999794960022. See `constants_review.md` §3.
- **`ZQTableRef` is declared twice**, in `AL_D` and in `ZQmod`, with different
  documented meanings ("index of the table selected for the current link-face
  calculation" vs "reference number read for the current ZQ table"). Both are
  mapped to `zq_tables`; `variables.csv` keeps them apart by renaming `ZQmod`'s
  copy to `ZQTableRefRead`, which makes the move mechanical. Whether they are
  in fact one quantity is still open.
- **`input_workspace` exists only to hold `IDUM` and `DUMMY`**, two scratch
  globals of the legacy readers. They are carried over so the move stays
  mechanical, but they are candidates for becoming locals in a later commit.

## Suggested execution order

Each step compiles on its own, so the work can stop at any point.

1. `core/` — constants, limits, units, paths, clock. Touches the most `USE`
   lines but is pure data movement with no logic.
2. `util/` and `io/` — leaf modules, nothing depends on their internals.
3. `AL_C` / `AL_D` / `AL_G` decomposition into the component state modules.
4. One component at a time: nitrate and sediment first (one external consumer
   each), then contaminant, subsurface, overland/channel. Within a component,
   create the `*_config` / `*_state` data modules before moving procedures, so
   each intermediate state stays acyclic.
5. `FRmod`, then `rest`, then `driver`, then the `USE` lines in
   `src/Shetran.f90` and `src/visualisation/`.

Per the recorded testing scope, verification is compile plus the parser tests;
example simulations are run manually.
