# Step 04 — Component state out of `AL_C` and `AL_D`

**Goal.** Create the eight component state modules and move into them the 96
`AL_C`/`AL_D` variables that are component state, plus `AL_C`'s three
initialisers. `AL_C` disappears at the end of this step; `AL_D` keeps the 54
variables that belong to the frame, the driver and meteorology until steps 12
and 13.

This step creates modules in component directories *before* those components are
split. That is deliberate: a component's procedures cannot move until the state
they read has a stable home, and doing all of `AL_C`/`AL_D` at once means the
`USE` sweep over its 12–13 consumers happens once instead of seven times.

## Target files

| File | Module | Vars | From `AL_C` | From `AL_D` |
|:-----|:-------|-----:|------------:|------------:|
| `evapotranspiration/et_state.f90` | `et_state` | 29 + 1 proc | 12 + `initialise_al_c3` | 17 |
| `snow/snow_state.f90` | `snow_state` | 8 | 1 (`ISPACK`) | 7 |
| `overland_channel/channel_geometry.f90` | `channel_geometry` | 9 | 9 | – |
| `overland_channel/oc_state.f90` | `oc_state` | 10 | 2 | 8 |
| `overland_channel/oc_boundaries.f90` | `oc_boundaries` | 2 | – | 2 |
| `overland_channel/zq_tables.f90` | `zq_tables` | 6 | – | 6 |
| `subsurface/vs_state.f90` | `vs_state` | 31 + 2 procs | 31 + `initialise_al_c`, `initialise_al_c2` | – |
| `sediment/sy_state.f90` | `sy_state` | 1 (`SBERR`) | 1 | – |

Each of these modules is **created here with its `AL_*` rows only** and grows in
its component's own step: `snow_state` +2 (step 08), `sy_state` +20 (06),
`oc_state` +6, `oc_boundaries` +12, `zq_tables` +7 (all 11), `vs_state` +3 (10),
`et_state` +0. Check the file against
`python3 scripts/rename_rows.py --target <file>` at both points; only the rows
whose `source_file` is `AL_C.F90` or `AL_D.f90` belong here.

## Sources

- **Retired here:** `src/core/state/AL_C.F90` — its remaining 56 variables and 3
  procedures all leave.
- **Shrinking:** `src/core/state/AL_D.f90`, 40 of its remaining 94 variables
  leave; 54 stay.

`src/core/state/` keeps `AL_D.f90` and is removed in step 13. Do not relocate
`AL_D.f90` in the meantime — moving it twice only churns the history.

## Consumers to update

`AL_C` is imported by 12 files, `AL_D` by 13 (`_usage_modules.csv`). The
interesting ones:

- `src/visualisation/visualisation_interface_left.f90` — the only visualisation
  file that reads model state; its `AL_C`/`AL_D` names split across `vs_state`,
  `et_state`, `snow_state`, `channel_geometry` and `oc_state`.
- `CMmod` reaches `AL_C` through a bare `USE`; give it an explicit list.
- `FRmod` calls `INITIALISE_AL_C`, `INITIALISE_AL_C2` and `INITIALISE_AL_C3` —
  they now live in `vs_state` and `et_state`.

## Hazards

- **Names.** `initialise_al_c*` keep their names (README open decision 2), even
  though `AL_C` no longer exists. Renaming them is a follow-up commit.
- **Allocation.** The initialisers allocate the arrays they now sit beside; make
  sure every array they touch really moved to the same module, or the
  initialiser will need a `USE` back into a module that `USE`s it — a cycle.
  `initialise_al_c`/`_al_c2` → `vs_state`, `initialise_al_c3` → `et_state` are
  the placements the proposal checked.
- **`vs_state` is the widely-read one:** frame, driver, ET, overland/channel,
  contaminant and the visualisation interface all reference it. Its `USE` sweep
  is the largest in this step.
- Data modules stay public by default; do not add `PRIVATE`.
- The state modules must not `USE` their own component's procedure modules —
  they are leaves. If one seems to need to, the placement is wrong: stop and
  check `variables.csv`.

## Run

```bash
python3 scripts/rename_extract.py --dry-run \
    --source src/core/state/AL_C.F90 \
    --source src/core/state/AL_D.f90 \
    --target src/evapotranspiration/et_state.f90 \
    --target src/snow/snow_state.f90 \
    --target src/overland_channel/oc_state.f90 \
    --target src/overland_channel/channel_geometry.f90 \
    --target src/overland_channel/oc_boundaries.f90 \
    --target src/overland_channel/zq_tables.f90 \
    --target src/subsurface/vs_state.f90 \
    --target src/sediment/sy_state.f90

# then drop --dry-run
```

The eight `--target` filters are what keeps `AL_D`'s frame, driver and
meteorology variables out of this step. Expect 29 declaration lines for
`et_state`, 31 for `vs_state`, 10 for `oc_state`, 9 for `channel_geometry`, 8
for `snow_state`, 6 for `zq_tables`, 2 for `oc_boundaries` and 1 for
`sy_state`, plus the three `initialise_al_c*` procedures.

`AL_C` is left as a module shell — 74 non-blank lines, all header and `USE` —
which is what you then `git rm`.

Then, in the same step:

```bash
python3 scripts/rename_ford_links.py --check src test   # read the report first
python3 scripts/rename_ford_links.py src test           # rewrite what it can
```

Resolve by hand the `MANUAL` links it reports for the modules this step retires
(`00_ford.md` §3), write the `USE` lines and the FORD headers, then build, test,
and only then:

```bash
grep -rn "TODO" src                       # must be empty
find src test -name '*.append'            # must be empty
find src test -name '*.backup' -delete
```

## Done when

- [ ] The eight modules exist with exactly their `AL_C`/`AL_D` rows.
- [ ] `src/core/state/AL_C.F90` is `git rm`-ed; `AL_D.f90` holds 54 variables.
- [ ] `grep -rniE '\bUSE +AL_C\b' src test` is empty.
- [ ] `grep -rniE '\[\[al_c[]:]' src test` is empty.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
