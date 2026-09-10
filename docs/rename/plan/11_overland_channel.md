# Step 11 — `src/overland_channel/`

**Goal.** Re-cut five modules — `OCmod`, `OCmod2`, `OCQDQMOD`, `ZQmod`,
`oc_row_width` — into 14 modules by role, and take `INBK` out of `FRmod`. This
is the split that touches the most of the tree: `OCmod2` alone is imported by
nine files.

**Rows.** `--target-dir src/overland_channel` — 111 rows, of which 27 moved in
step 04 (`channel_geometry`, `oc_state`, `oc_boundaries`, `zq_tables`).

## Target files

| File | Module | Procs | Vars | Comes from |
|:-----|:-------|------:|-----:|:-----------|
| `oc_driver.f90` | `oc_driver` | 5 | 3 (+1 type) | `OCmod` |
| `oc_input.f90` | `oc_input` | 3 | – | `OCmod` |
| `oc_validation.f90` | `oc_validation` | 4 | – | `OCmod` |
| `oc_indexing.f90` | `oc_indexing` | 3 | 6 | `OCmod`, `oc_row_width` |
| `oc_boundaries.f90` | `oc_boundaries` | 3 | 14 | `OCmod` 12, `OCQDQMOD` 3, +2 from step 04 |
| `oc_cross_sections.f90` | `oc_cross_sections` | 1 | 5 | `OCmod`, `OCQDQMOD` |
| `oc_node_solver.f90` | `oc_node_solver` | 9 | – | `OCmod2` |
| `oc_discharge.f90` | `oc_discharge` | 6 | – | `OCmod2` |
| `oc_conveyance.f90` | `oc_conveyance` | 1 | 3 | `OCmod2` |
| `oc_stage_discharge.f90` | `oc_stage_discharge` | 3 | – | `OCQDQMOD` |
| `zq_tables.f90` | `zq_tables` | 2 | 13 | `ZQmod` 9, +6 from step 04 |
| `bank_setup.f90` | `bank_setup` | 1 | – | `FRmod` (`INBK`) |
| `oc_state.f90` | `oc_state` | – | 16 | `OCmod2` 3, `OCQDQMOD` 2, `OCmod` 1, +10 from step 04 |
| `channel_geometry.f90` | `channel_geometry` | – | 9 | done in step 04 |

## Order within the step

1. Extend the four state/data modules created in step 04 with their `OC*`/`ZQ*`
   rows.
2. `oc_conveyance` — the leaf both solver halves depend on. Create it before
   `oc_node_solver` and `oc_discharge`.
3. `oc_node_solver`, `oc_discharge`, `oc_stage_discharge`, `oc_cross_sections`.
4. `oc_indexing`, `oc_input`, `oc_validation`, `oc_driver`.
5. `bank_setup` — `INBK` out of `FRmod`.
6. `git rm` the five source files (subject to the `oc_row_width` decision).

## Hazards

- **`oc_conveyance` must stay a leaf.** `CONVEYAN` is called from both
  `OCCODE` (in `oc_node_solver`) and four of the discharge routines, and
  `DZMIN`, `RDZMIN`, `H23MIN` travel with it because `OCFIX`, in
  `oc_node_solver`, reads `DZMIN`. Without this module `oc_node_solver` and
  `oc_discharge` `USE` each other.
- **`dtoc` belongs in `oc_state`,** not in `oc_driver`: `OCSIM` calls `OCABC`,
  which reads it, so keeping it in the driver closes an `oc_boundaries` ↔
  `oc_driver` cycle. It was moved in step 04; do not move it again.
- **`OCmod2`'s nine consumers need almost nothing.** What they actually use is
  `HRFZZ` and the accessors `gethrf`, `sethrf`, `getqsa`, `setqsa`. After the
  split those nine `USE` lines import from `oc_state` (the arrays) and
  `oc_node_solver` (the accessors). Check each of the nine against
  `functions.csv`/`variables.csv` rather than importing the whole of both.
- **`ZQTableRef` is declared twice** — once in `AL_D` (moved in step 04), once in
  `ZQmod` — with different documented meanings. Both land in `zq_tables`;
  `variables.csv` renames `ZQmod`'s copy to `ZQTableRefRead` to keep the move
  mechanical. Apply that rename at its use sites inside `ZQmod`'s procedures.
  Whether the two are really one quantity stays open.
- **`OCSIM_WORKSPACE_TYPE` moves as a block** (`types.csv` gives the range) and
  travels with its instance `OCSIM_WORKSPACE` into `oc_driver`.
- `OCmod2`'s three constants (`F23`, `F53`, `ROOT2G`) left in step 02 and are
  now `TWO_THIRDS`, `FIVE_THIRDS`, `SQRT_TWO_G` in `mod_parameters`.
  `SQRT_TWO_G` keeps the value 4.42944 and is *not* replaced by
  `SQRT(2*GRAVITY)`.

## `oc_row_width` and its unit test — decide before you start

`variables.csv`/`functions.csv` put `MAX_ACTIVE_ROW_WIDTH` into `oc_indexing`.
But `test/CMakeLists.txt` builds `oc_row_width_tests` from that one source file
plus the assertion support, precisely because the module depends on nothing, and
its header says so.

- **Recommended:** leave `MAX_ACTIVE_ROW_WIDTH` in `src/overland_channel/oc_row_width.f90`
  as its own dependency-free leaf, have `oc_indexing` `USE oc_row_width, ONLY:
  MAX_ACTIVE_ROW_WIDTH`, and record the deviation from the CSV in
  `deviations.md`. `test/CMakeLists.txt` needs no change.
- **Alternative:** follow the CSV and extend the `oc_row_width_tests` target
  with `oc_indexing`'s full dependency chain (the `core/` data modules and
  whatever else `oc_indexing` imports), and rename the test and its directory.

Either way the test must still build and pass; do not delete it.

## Consumers

| Retired module | Files |
|:---------------|------:|
| `OCmod2` | 9 |
| `OCmod` | 3 (`Shetran.f90` imports `FINALISE_OCSIM_WORKSPACE` → `oc_driver`) |
| `OCQDQMOD` | 3 |
| `ZQmod` | 2 |

`visualisation_interface_left` imports `OCmod2, ONLY: hrfzz` → `oc_state`.

## Run

```bash
python3 scripts/rename_extract.py --dry-run --target-dir src/overland_channel \
    --source src/overland_channel/OCmod.f90 \
    --source src/overland_channel/OCmod2.f90 \
    --source src/overland_channel/OCQDQMOD.F90 \
    --source src/overland_channel/ZQmod.f90 \
    --source src/frame/FRmod.f90

# add --source src/overland_channel/oc_row_width.f90 only if you take the
# alternative in the decision below; the recommendation leaves that file alone

# then drop --dry-run
```

`oc_state`, `oc_boundaries`, `zq_tables` and `channel_geometry` exist from
step 04, so their `OC*`/`ZQ*` rows arrive as `.append` files.

This is the step with the most FORD work: `OCmod2` and `OCmod` are the most
linked-to modules after `mod_error`, and `test/oc_row_width/test_oc_row_width.f90`
carries four `[[ocmod:…]]` links.

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

- [ ] 14 (or 15) files; `OCmod.f90`, `OCmod2.f90`, `OCQDQMOD.F90`, `ZQmod.f90`
      are `git rm`-ed.
- [ ] `grep -rniE '\bUSE +(OCMOD2?|OCQDQMOD|ZQMOD)\b' src test` is empty; the
      same for `[[…]]` links.
- [ ] `ctest --output-on-failure` runs **both** `visualisation_read.*` and
      `oc_row_width.unit`, and both pass.
- [ ] The `oc_row_width` decision is recorded in `deviations.md`.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Committed.
