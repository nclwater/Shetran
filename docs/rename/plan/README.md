# Implementation plan: reorganising `src/` into descriptive, right-sized modules

This directory turns [`../proposal.md`](../proposal.md) into an ordered set of
steps that an agent can implement one at a time. It adds no new decisions about
*what* the target structure is — that is settled in the proposal and in the
four move tables (`functions.csv`, `variables.csv`, `types.csv`,
`constants_review.md`). It settles *how* and *in which order*, and it records
the places where the mechanical move meets something that the proposal does not
cover.

## How to use this directory

1. Read [`00_working_rules.md`](00_working_rules.md) once. It holds the move
   recipe, the verification commands and the rules that apply to every step.
   Do not start a step without it.
   - [`00_tooling.md`](00_tooling.md) — the three scripts that do the work, in
     full. **The moves are made by script, not by hand:** each script renames
     the file it is about to change to `<path>.backup` and writes the new
     content by copying from that backup, so no code or documentation is ever
     retyped. `*.backup` is already in `.gitignore` and is deleted at the end of
     each step.
   - [`00_ford.md`](00_ford.md) — what the move does to the FORD documentation:
     doc blocks travelling with their entity, the header every new module needs,
     redistributing the header of a dissolved module, and the 281 qualified
     cross-references (287 links rewrite automatically, 68 need a decision).
2. Do the steps in the numbered order. Each step is self-contained: it names
   its target files, its source files, its consumers and its hazards.
3. After each step: build, run the tests, commit. **The tree compiles and the
   tests pass at the end of every step** — that is the central invariant, and
   it is what makes the work stoppable at any point.
4. Tick the step off in the status table below in the same commit.
5. Anything you had to decide that this plan does not answer goes in
   `deviations.md` (create it when first needed), not into silent improvisation.

## The invariant, stated once

Every step is a **pure move**, made by script. Names, values, declarations, bodies and
documentation travel unchanged; only their location changes, plus the `USE`
lines needed to keep them visible and the 20 constant renames listed in
`constants_review.md`. No refactoring, no de-duplication, no reformatting, no
"while I am here" fixes, no behaviour change. The observable output of the model
must be bit-identical.

The proposal records several things that are wrong or redundant in the current
code (duplicated plant state, the dead `DOCIN`, the 26 orphan variables, the
single-precision `RHO`/`GAMMA` literals, `ZQTableRef` declared twice). **None of
them are fixed here.** They are made visible by the move and left for later
commits.

## Step order and status

| # | Step | New files | Sources retired | Status |
|--:|:-----|----------:|:----------------|:-------|
| 01 | [Baseline and tooling](01_baseline.md) | – | – | ☑ |
| 02 | [`core/`](02_core.md) | 10 | `sglobal`, `AL_G`, `CONST_SY` | ☑ |
| 03 | [`util/` and `io/`](03_util_io.md) | 14 | `mod_error`, `tolerance_testing`, `utilsmod`, `mod_load_filedata` | ☐ |
| 04 | [Component state out of `AL_C`/`AL_D`](04_component_state.md) | 8 | `AL_C` | ☐ |
| 05 | [`nitrate/`](05_nitrate.md) | 9 | `MNmod` | ☐ |
| 06 | [`sediment/`](06_sediment.md) | 9 | `SYmod`, `SED_CS` | ☐ |
| 07 | [`evapotranspiration/`](07_evapotranspiration.md) | 4 | `ETmod` | ☐ |
| 08 | [`snow/`](08_snow.md) | 4 | `SMmod` | ☐ |
| 09 | [`contaminant/`](09_contaminant.md) | 20 | `CMmod` + 14 data modules | ☐ |
| 10 | [`subsurface/`](10_subsurface.md) | 9 | `VSmod` | ☐ |
| 11 | [`overland_channel/`](11_overland_channel.md) | 14 | `OCmod`, `OCmod2`, `OCQDQMOD`, `ZQmod`, `oc_row_width` | ☐ |
| 12 | [`frame/`](12_frame.md) | 6 | `FRmod` | ☐ |
| 13 | [`meteorology/` and `driver/`](13_meteorology_driver.md) | 7 | `rest`, `run_sim`, `getdirqq`, `AL_D` | ☐ |
| 14 | [Close-out](14_closeout.md) | – | – | ☐ |

106 target files in place of the 40 in scope; 269 procedures, 1,091 module-level
variables and 4 derived types move. The "new files" column sums to 114 because
the eight state modules created in step 04 are listed again in the component
step that completes them.

The per-step extraction commands have been run end to end over a scratch copy
of `src/`: they produce all 106 target files, and every retired source is
reduced to its module shell. What they do not do — and what the steps below
spend their words on — is the `USE` lines, the FORD headers and the decisions.

### Why this order

It is the proposal's suggested order, made concrete. Two constraints fix it:

- **Data before consumers.** `core/` first because every other module reads it;
  `util/`+`io/` next because they are leaves; then the component state modules,
  because a component's procedures cannot move until the state they touch has a
  home.
- **Sheds before dissolution.** `FRmod` gives up its five `IN*` component-setup
  routines during the component steps (ET, snow, contaminant, overland/channel),
  so it can only be dissolved after them. `rest`, `run_sim` and `getdirqq` come
  last because `Shetran.f90` names them directly.

Components are ordered by isolation: nitrate has one consumer and gains nothing
from `AL_*`; sediment has few; ET and snow are small and their state modules
already exist by step 04; contaminant, subsurface and overland/channel are the
large ones and come once the pattern is established.

### What each step must not defer

`USE` lines are updated **in the step that moves the entity**, everywhere in the
tree — including `src/Shetran.f90`, `src/visualisation/` and
`test/CMakeLists.txt`. There is no "fix the imports later" step; a step that
leaves them stale does not compile, which is the point.

## Open decisions

Three points where the proposal and the existing tree do not quite meet. Each
is raised again in the step that hits it; the recommendation is the default to
take if nobody says otherwise.

1. **`oc_row_width` versus its unit test** (step 11). `test/CMakeLists.txt`
   builds `oc_row_width_tests` from *only* `src/overland_channel/oc_row_width.f90`
   plus the assertion support, and the module's own header says it "deliberately
   depends on nothing else so the sizing rule can be exercised directly …
   without linking the model". Folding `MAX_ACTIVE_ROW_WIDTH` into `oc_indexing`
   as the proposal asks makes that target need `oc_indexing`'s whole dependency
   chain. *Recommendation:* keep `MAX_ACTIVE_ROW_WIDTH` in its own leaf module,
   renamed to `src/overland_channel/oc_row_width.f90` / `oc_row_width` (i.e. no
   change), and have `oc_indexing` `USE` it. That is 15 target modules in
   `overland_channel/` instead of 14 and preserves a working test; the
   alternative is to extend the test target's source list.
2. **`INITIALISE_AL_C`, `INITIALISE_AL_C2`, `INITIALISE_AL_C3`** (step 04) move
   to `vs_state` and `et_state` but keep names that refer to a module that will
   no longer exist. *Recommendation:* keep the names in this work (a pure move)
   and rename them in a follow-up commit, so the diff stays mechanical.
3. **Renamed constants at their use sites** (step 02). The 20 renames in
   `variables.csv` change identifiers inside `SMmod`, `ETmod`, `SYmod`,
   `OCmod2` and `visualisation_interface_left`. *Recommendation:* rewrite the
   identifiers at the use sites rather than aliasing them back with
   `USE mod_parameters, ONLY: RHOA => RHO_AIR_SNOW`; an alias would hide exactly
   the ambiguity `constants_review.md` exists to expose.

## Files this plan does not touch

`src/visualisation/` and `src/resource/` keep their structure; only their `USE`
lines follow the renames. `docs/code_analysis/`, `docs/problems/` and the older
analysis notes are historical records and are left alone even where they quote
paths that this work invalidates.
