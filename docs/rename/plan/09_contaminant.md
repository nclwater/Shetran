# Step 09 — `src/contaminant/`

**Goal.** Turn `CMmod` and the 14 opaquely named data modules into 20 files, and
take `INCM`, `MUERR2` and `INPL` out of `FRmod`. The largest step by file count;
most of it is renaming whole data modules, which is mechanical.

**Rows.** `--target-dir src/contaminant` — 324 rows.

## Data-module renames — one file each, contents unchanged

| Was | Becomes | File | Vars |
|:----|:--------|:-----|-----:|
| `CONT_CC` | `cm_parameters` | `cm_parameters.f90` | 35 (+1 proc) |
| `COLM_CC` | `cm_column_state` | `column/cm_column_state.f90` | 38 |
| `COLM_CC1` | `cm_column_equations` | `column/cm_column_equations.f90` | 12 |
| `COLM_C1` | `cm_column_scaling` | `column/cm_column_scaling.f90` | 20 |
| `COLM_C2` | `cm_column_water` | `column/cm_column_water.f90` | 35 |
| `COLM_CG` | `cm_column_geometry` | `column/cm_column_geometry.f90` | 12 (+2 procs) |
| `COLM_CO` | `cm_column_previous` | `column/cm_column_previous.f90` | 10 (+1 proc) |
| `LINK_CC` | `cm_link_state` | `link/cm_link_state.f90` | 63 |
| `LINK_CC1` | `cm_link_scaling` | `link/cm_link_scaling.f90` | 2 |
| `LINK_CW` | `cm_link_water` | `link/cm_link_water.f90` | 9 |
| `BK_CW` | `cm_bank_geometry` | `cm_bank_geometry.f90` | 5 |
| `SED_CO` | `cm_sediment_previous` | `cm_sediment_previous.f90` | 6 |
| `IS_CC` | `cm_solver_flags` | `cm_solver_flags.f90` | 5 |
| `PLANT_CC` | `cm_plant_state` | `cm_plant_state.f90` | 29 |

Use `git mv` for these: the file is renamed, the `MODULE`/`END MODULE` lines and
the header are updated, and the contents stay put.

## `CMmod` split

| File | Module | Procs | Vars | Note |
|:-----|:-------|------:|-----:|:-----|
| `cm_input.f90` | `cm_input` | 3 | – | `CMRD` from `CMmod`; `INCM` and `MUERR2` from `FRmod` |
| `cm_driver.f90` | `cm_driver` | 1 | – | One subsurface timestep per active contaminant |
| `cm_column.f90` | `cm_column` | 6 | 13 | Column advection–dispersion–reaction |
| `cm_channel.f90` | `cm_channel` | 4 | 6 | Channel-link assembly and solve |
| `cm_plant.f90` | `cm_plant` | 4 | – | `PLCOLM`, `PLANT`, `PLPREP` from `CMmod`; `INPL` from `FRmod` |
| `cm_sorption.f90` | `cm_sorption` | 2 | – | Soil and sediment retardation factors |

`CMmod`'s 19 module-level variables go to `cm_column` (13) and `cm_channel` (6);
nothing arrives from `AL_C` or `AL_D`.

## Order within the step

1. The 14 data-module renames, one commit-sized batch, updating every `USE` as
   you go. After this the tree still has `CMmod` intact and builds.
2. `CMmod`'s six-way split.
3. `INCM` + `MUERR2` → `cm_input`, `INPL` → `cm_plant`, out of `FRmod`.
4. `git rm src/contaminant/CMmod.f90`.

## Consumers

`CMmod`: `run_sim`, `FRmod`. The data modules: `CMmod`, `FRmod`, `run_sim` and
`visualisation_interface_left` (`CONT_CC, ONLY: cccc, ncon, ssss` →
`cm_parameters`). `_usage_modules.csv` has the exact lists.

`CMmod` reaches `sglobal`, `AL_C` and `AL_G` names through bare `USE`
statements that steps 02 and 04 already turned into explicit lists — carry those
explicit imports into whichever new module needs each name, rather than
duplicating the whole list six times.

## Hazards

- **`LINK` ↔ `SNL3` are mutually recursive** and must both stay in
  `cm_channel`. This is one of only two mutual-recursion groups in the tree.
- **`MUERR2` stays in the component** (`cm_input`), not in a global error
  module.
- **`cm_plant_state` and `mn_state` both declare `NPL`, `NPLTYP`, `GMCPBB` and
  `PFONE`** in separate storage, and `GMCPBB` means different things on the two
  sides. Both copies are kept, unchanged. Co-locating them makes the duplication
  visible; resolving it is a later commit
  (`docs/todo/issue_plant_declarations.md`).
- The `column/` and `link/` subdirectories stay. Procedure-bearing files and the
  remaining data modules sit directly in `src/contaminant/`.
- `cm_column_geometry` and `cm_column_previous` carry procedures as well as data
  — they are not pure renames of pure data modules.

## Run

The 14 data modules are 1:1 renames, so use `git mv` for them and edit the
`MODULE`/`END MODULE` lines and the header — that keeps the file history. The
extractor then does `CMmod` and the three routines arriving from `FRmod`:

```bash
python3 scripts/rename_extract.py --dry-run --target-dir src/contaminant \
    --source src/contaminant/CMmod.f90 \
    --source src/frame/FRmod.f90

# then drop --dry-run
```

If you prefer to let the script do the renames too, add a `--source` for each of
the 14 files; it writes each new module with its skeleton header, which then has
to be filled in by hand rather than carried over. `git mv` is the better trade
for a pure rename.

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

- [ ] 20 files under `src/contaminant/`; `CMmod.f90` and the 14 old data-module
      files are gone.
- [ ] `grep -rniE '\bUSE +(CMMOD|CONT_CC|COLM_CC1?|COLM_C[12]|COLM_CG|COLM_CO|LINK_CC1?|LINK_CW|BK_CW|SED_CO|IS_CC|PLANT_CC)\b' src test` is empty.
- [ ] The same names are gone from `[[…]]` links.
- [ ] `INCM`, `MUERR2`, `INPL` are no longer in `FRmod`.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
