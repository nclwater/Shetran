# Step 06 — `src/sediment/`

**Goal.** Re-cut `SYmod` (4,671 lines) and `SED_CS` into nine files.
`CONST_SY` is already gone (step 02).

**Rows.** `--target-dir src/sediment` — 119 rows.

## Target files

| File | Module | Procs | Vars | Comes from |
|:-----|:-------|------:|-----:|:-----------|
| `sy_state.f90` | `sy_state` | – | 21 | `SED_CS` 20 + `SBERR` (moved in step 04) |
| `sy_config.f90` | `sy_config` | – | 44 | `SYmod` |
| `sy_workspace.f90` | `sy_workspace` | – | 21 | `SYmod` |
| `sy_driver.f90` | `sy_driver` | 3 | – | `SYmod` |
| `sy_input.f90` | `sy_input` | 3 | – | `SYmod` |
| `sy_validation.f90` | `sy_validation` | 4 (+1 nested) | – | `SYmod` (`SYERR0`–`SYERR3`) |
| `sy_transport_capacity.f90` | `sy_transport_capacity` | 6 (+2 nested) | 6 | `SYmod` |
| `sy_hillslope.f90` | `sy_hillslope` | 3 | – | `SYmod` |
| `sy_channel.f90` | `sy_channel` | 4 (+1 nested) | – | `SYmod` |

## Order within the step

1. Extend `sy_state` (created in step 04 with `SBERR`) with `SED_CS`'s 20
   variables, and `git rm src/sediment/SED_CS.F90`.
2. Create `sy_config` and `sy_workspace` — data only, no procedures.
3. Move the procedures into the six process modules.
4. `git rm src/sediment/SYmod.f90`.

## Consumers

| Retired module | Files |
|:---------------|:------|
| `SED_CS` | `CMmod`, `run_sim`, `FRmod`, `visualisation_interface_left` (`dls`, `gnu`, `nsed`, `qsed` → `sy_state`) |
| `SYmod` | `run_sim` only |

## Hazards

- **Six variables stay with their formulas**, in `sy_transport_capacity`, not in
  `sy_config`: the derived coefficients `K1_syovtr`, `K3_syovtr`, `K4_syovtr`
  and the first-call state `FIRST_syackw`, `FIRST_syfine`, `WSED_syfine`
  (`constants_review.md`, "Constants deliberately not moved"). `variables.csv`
  places them; do not tidy them into the config module.
- The sediment constants now come from `mod_parameters` under their new names —
  `GRAVITY`, `RHO_SEDIMENT`, `RHO_WATER_SEDIMENT`, `NU_WATER` — rewritten in
  step 02. Nothing to do here beyond checking the imports.
- `SYmod` has three nested procedures across three parents; they travel inside
  their parents' spans.

## Run

```bash
python3 scripts/rename_extract.py --dry-run \
    --source src/sediment/SYmod.f90 --source src/sediment/SED_CS.F90

# then drop --dry-run
```

`sy_state` already exists (step 04 put `SBERR` in it), so `SED_CS`'s 20
declarations arrive as `src/sediment/sy_state.f90.append`.

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

- [ ] Nine files exist; `SYmod.f90` and `SED_CS.F90` are `git rm`-ed.
- [ ] `grep -rniE '\bUSE +(SYMOD|SED_CS)\b|\[\[(symod|sed_cs)[]:]' src test` is empty.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
