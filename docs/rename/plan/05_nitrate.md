# Step 05 — `src/nitrate/`

**Goal.** Split `MNmod` (5,139 lines, 30 procedures, 57 variables, 3 derived
types) into nine files. This is the first component step because `MNmod` is the
most self-contained module in the tree: it gains nothing from `AL_C`/`AL_D`, and
exactly one file imports it.

**Rows.** `--target-dir src/nitrate` — 90 rows, all from `MNmod`.

## Target files

| File | Module | Procs | Vars | Holds |
|:-----|:-------|------:|-----:|:------|
| `mn_state.f90` | `mn_state` | – | 57 (+3 types) | Carbon and nitrogen pools, rates, per-cell nitrate state |
| `mn_driver.f90` | `mn_driver` | 6 | – | Allocation, initialisation, the per-timestep sequence |
| `mn_input.f90` | `mn_input` | 3 | – | Static data and the scheduled N and C additions |
| `mn_validation.f90` | `mn_validation` | 5 | – | `MNERR0`–`MNERR4` |
| `mn_environment.f90` | `mn_environment` | 6 | – | Soil temperature, temperature/moisture factors |
| `mn_nitrogen.f90` | `mn_nitrogen` | 4 | – | Ammonium, nitrification, mineralisation, litter nitrogen |
| `mn_organic_matter.f90` | `mn_organic_matter` | 3 | – | Litter, humus and manure carbon turnover, CO₂ |
| `mn_plant.f90` | `mn_plant` | 2 | – | Plant nitrogen uptake |
| `mn_output.f90` | `mn_output` | 1 | – | Nitrate and carbon budget output files |

## Order within the step

1. `mn_state.f90` first, with all 57 variables **and** the three derived types
   `MN_CONFIG_TYPE`, `MN_WORKSPACE_TYPE`, `MN_PLANT_STATE_TYPE` together with
   their instances `MN_CONFIG`, `MN_WORK`, `MN_PLANT_STATE`. A type moves as a
   block (`types.csv` gives the line range) and travels with its instance.
2. The eight procedure modules, in any order; each `USE`s `mn_state` and the
   `core`/`util`/`io` modules from steps 02–03.
3. `git rm src/nitrate/MNmod.f90`.

## Consumers

One: `src/contaminant/CMmod.f90`, which imports
`ONLY: MNCONT, MNINITIALISE, MNISINITIALISED`. Look up each in
`functions.csv` — they land in `mn_driver` — and rewrite the single `USE`.

## Hazards

- `MNmod`'s own imports (`sglobal`, `mod_parameters`, `mod_error`,
  `mod_load_filedata`, `utilsmod`) were all rewritten in steps 02–03; carry the
  rewritten form into each new module, importing only what that module's bodies
  actually use.
- `MNERR0`–`MNERR4` stay in the component (`mn_validation`). Nothing goes into a
  global error module.
- `mn_state` holds the nitrate side of the duplicated plant state (`NPL`,
  `NPLTYP`, `GMCPBB`, `PFONE`, `GMCBBO`). It is *not* merged with
  `cm_plant_state` — see `docs/todo/issue_plant_declarations.md` and the
  proposal's "Open point".
- This is the first split of a large module: it is the place to settle the
  header-documentation convention (§6 of the working rules) that the remaining
  component steps copy.

## Run

```bash
python3 scripts/rename_extract.py --dry-run --source src/nitrate/MNmod.f90
python3 scripts/rename_extract.py --source src/nitrate/MNmod.f90
```

5,029 of `MNmod`'s 5,139 lines move mechanically; the 67 non-blank lines in the
coverage report are the module header, the `USE` block and the module shell.
The three derived types land in `mn_state` ahead of their instances, because the
extractor keeps source order within a target.

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

- [ ] Nine files exist; `MNmod.f90` is `git rm`-ed.
- [ ] `python3 scripts/rename_rows.py --target-dir src/nitrate` accounts for
      every declaration and procedure in the new files (90 rows).
- [ ] `grep -rniE '\bUSE +MNMOD\b|\[\[mnmod[]:]' src test` is empty.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
