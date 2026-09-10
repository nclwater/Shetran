# Step 10 — `src/subsurface/`

**Goal.** Split `VSmod` (5,230 lines, 22 procedures, 112 module-level variables)
nine ways.

**Rows.** `--target-dir src/subsurface` — 167 rows, of which 33 (`vs_state`)
moved in step 04.

## Target files

| File | Module | Procs | Vars | Holds |
|:-----|:-------|------:|-----:|:------|
| `vs_state.f90` | `vs_state` | 2 | 34 | 31 from `AL_C` (step 04) + 3 from `VSmod` |
| `vs_config.f90` | `vs_config` | – | 66 | Soil parameters, initial-condition options, setup switches |
| `vs_boundaries.f90` | `vs_boundaries` | – | 25 | Time-varying lateral, base and well boundary series |
| `vs_soil_tables.f90` | `vs_soil_tables` | 1 | 11 | Soil hydraulic-property lookup tables |
| `vs_driver.f90` | `vs_driver` | 4 | – | Column iteration and the subsurface mass balance |
| `vs_input.f90` | `vs_input` | 3 (+1 nested) | 6 | The subsurface data and initial-condition files |
| `vs_column_solver.f90` | `vs_column_solver` | 4 | 1 | Tridiagonal pressure-head correction |
| `vs_connectivity.f90` | `vs_connectivity` | 2 (+1 nested) | – | Cell, layer and link connectivity |
| `vs_sources.f90` | `vs_sources` | 6 | – | Well, spring, boundary and interception source terms |

## Order within the step

1. The three data modules — `vs_config`, `vs_boundaries`, `vs_soil_tables` — and
   the three `VSmod` variables that join `vs_state`.
2. The six procedure modules.
3. `git rm src/subsurface/VSmod.f90`.

The split follows the legacy `.INC` groupings the source still marks in comments
(`VSCOM1`, `VSSOIL`, `VSINIT`) plus the call structure, so the boundaries match
how the file is already organised internally. Use those comments as a
cross-check on `variables.csv`, not as an override.

## Hazards

- **`errcntallowed` goes to `vs_column_solver`, its only reader.** Putting it in
  a config module closes a `vs_driver` ↔ `vs_column_solver` cycle
  (`constants_review.md`, "Constants deliberately not moved").
- `vs_state` is the most widely read of the new state modules — frame, driver,
  ET, overland/channel, contaminant and the visualisation interface all
  reference it. Its imports were fixed in step 04; the three variables added
  here must not need any new one.
- Two nested procedures, in `vs_input` and `vs_connectivity`; they travel inside
  their parents.
- `VSmod` mixes three kinds of state on purpose-looking lines: 66 configuration
  values, 25 boundary series, 11 soil tables, 6 reader buffers, 1 solver limit,
  3 state variables. Trust `variables.csv` for each one.

## Consumers

`VSmod` is imported by `run_sim` and `FRmod`.

## Run

```bash
python3 scripts/rename_extract.py --dry-run --source src/subsurface/VSmod.f90
python3 scripts/rename_extract.py --source src/subsurface/VSmod.f90
```

5,028 of 5,230 lines move; `vs_state` exists from step 04, so its three extra
variables arrive as `vs_state.f90.append`.

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

- [ ] Nine files; `VSmod.f90` is `git rm`-ed.
- [ ] `grep -rniE '\bUSE +VSMOD\b|\[\[vsmod[]:]' src test` is empty.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
