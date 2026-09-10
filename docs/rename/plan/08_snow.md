# Step 08 — `src/snow/`

**Goal.** Split `SMmod` four ways and take `INSM` out of `FRmod`. Same shape as
step 07.

**Rows.** `--target-dir src/snow` — 33 rows.

## Target files

| File | Module | Procs | Vars | Comes from |
|:-----|:-------|------:|-----:|:-----------|
| `snow_state.f90` | `snow_state` | – | 10 | 8 from step 04 + `smelt`, `tmelt` from `SMmod` |
| `snow_config.f90` | `snow_config` | – | 18 | `SMmod` |
| `snowmelt.f90` | `snowmelt` | 4 | – | `SMmod` |
| `snow_input.f90` | `snow_input` | 1 | – | `FRmod` (`INSM`) |

## Order within the step

1. Add `smelt` and `tmelt` to `snow_state`.
2. `snow_config.f90` — 18 parameters and initial-snowpack settings, data only.
3. `snowmelt.f90` — the four `SMmod` procedures, **including `SMIN`**.
4. `snow_input.f90` — `INSM` from `FRmod`.
5. `git rm src/snow/SMmod.f90`.

## Hazards

- **`SMIN` goes to `snowmelt`, not `snow_input`.** Like `ETIN` it is a
  per-element process wrapper: it is called from `ETIN` and calls `SM` and
  `SMET`. And, as in ET, the parameters go to a data-only `snow_config` rather
  than staying with the reader. Both placements exist to break a cycle.
- `SMmod`'s eight physical constants left in step 02. `snowmelt` imports
  `RHO_AIR_SNOW`, `RHO_WATER_SNOW`, `CP_AIR_SNOW`, `CP_WATER`, `CP_ICE`,
  `L_FUSION`, `L_VAPORISATION_SNOW` and `GROUND_HEAT_FLUX_SNOW` from
  `mod_parameters`.
- The literal `9.81d0` inside `SMmod`'s Richardson-number expression stays a
  literal (`constants_review.md` §1). Do not replace it with `GRAVITY`.
- `et_process` imports `SMIN`; update that import here if step 07 pointed it at
  `SMmod`.

## Consumers

`SMmod` is imported by `run_sim`, `ETmod`/`et_process` and `FRmod`.

## Run

```bash
python3 scripts/rename_extract.py --dry-run --target-dir src/snow \
    --source src/snow/SMmod.f90 \
    --source src/frame/FRmod.f90

# then drop --dry-run
```

`FRmod` has already shrunk in step 07; the extractor finds `INSM` by content, so
the stale line numbers in the CSVs do not matter. `snow_state` exists, so
`smelt` and `tmelt` arrive as `snow_state.f90.append`.

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

- [ ] Four files; `SMmod.f90` is `git rm`-ed; `INSM` is no longer in `FRmod`.
- [ ] `grep -rniE '\bUSE +SMMOD\b|\[\[smmod[]:]' src test` is empty.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
