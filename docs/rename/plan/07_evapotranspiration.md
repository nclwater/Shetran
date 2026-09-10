# Step 07 — `src/evapotranspiration/`

**Goal.** Split `ETmod` four ways and take `INET` out of `FRmod`. Small, but it
is the first step that edits `FRmod`, so it sets the pattern for steps 08, 09
and 11.

**Rows.** `--target-dir src/evapotranspiration` — 77 rows, 30 of them the
`et_state` rows already moved in step 04.

## Target files

| File | Module | Procs | Vars | Comes from |
|:-----|:-------|------:|-----:|:-----------|
| `et_state.f90` | `et_state` | 1 | 29 | done in step 04 — nothing to add |
| `et_config.f90` | `et_config` | – | 41 | `ETmod` |
| `et_process.f90` | `et_process` | 5 | – | `ETmod` |
| `et_input.f90` | `et_input` | 1 | – | `FRmod` (`INET`) |

## Order within the step

1. `et_config.f90` — the 41 input parameters and vegetation tables, data only.
2. `et_process.f90` — all five `ETmod` procedures.
3. `et_input.f90` — move `INET` out of `FRmod` (`functions.csv` gives the span).
4. `git rm src/evapotranspiration/ETmod.f90`.

## Hazards — the two that the cycle check turned on

- **`ETIN` is not a reader.** Despite the name it is the per-element ET and
  interception step, called only from `ETSIM`, and it calls `ET` and `SMIN`. It
  goes to `et_process`. Putting it in `et_input` closes a cycle.
- **The parameters must not stay with the reader.** `INET` writes the 41
  parameters and `et_process` reads them; parking them in `et_input` makes
  `et_input` and `et_process` mutually dependent. That is the entire reason
  `et_config` exists. `et_input` is left holding exactly one procedure.

Also:

- `ETmod`'s four physical constants left in step 02; `et_process` now imports
  `L_VAPORISATION_ET`, `PSYCHROMETRIC_CONSTANT`, `RHO_AIR_ET` and `CP_AIR_ET`
  from `mod_parameters`.
- `INET`'s arrival direction is safe: `FRmod` already `USE`s `ETmod`, and
  nothing outside `driver/` and `Shetran.f90` uses `FRmod`, so the dependency
  arrow is unchanged.
- `et_process` calls `SMIN`, which is in `snow/snowmelt` after step 08 and in
  `SMmod` until then. Import from wherever it currently lives and update in step
  08 — or do steps 07 and 08 back to back and fix the import once.

## Consumers

`ETmod` is imported by `rest`, `run_sim` and `FRmod`. `et_state` already has its
consumers from step 04.

## Run

```bash
python3 scripts/rename_extract.py --dry-run --target-dir src/evapotranspiration \
    --source src/evapotranspiration/ETmod.f90 \
    --source src/frame/FRmod.f90

# then drop --dry-run
```

The `--target-dir` filter is what takes `INET` — and only `INET` — out of
`FRmod`; the other 5,713 lines of it are written straight back from the backup.
`et_state` already exists, so nothing is written to it here.

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

- [ ] Four files; `ETmod.f90` is `git rm`-ed; `INET` is no longer in `FRmod`.
- [ ] `grep -rniE '\bUSE +ETMOD\b|\[\[etmod[]:]' src test` is empty.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
