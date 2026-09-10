# Step 02 — `src/core/`

**Goal.** Replace `sglobal`, `AL_G` and `CONST_SY` with ten small, named
`core/` modules, and move every physical constant into `mod_parameters`. Pure
data movement, but it touches more `USE` lines than any other step: 36 files
`USE sglobal`, 13 of them bare.

**Rows.** `python3 scripts/rename_rows.py --target-dir src/core` — 199
variables, no procedures.

## Target files

| File | Module | Vars | Comes from |
|:-----|:-------|-----:|:-----------|
| `core/mod_parameters.f90` | `mod_parameters` | 51 | keeps its 17, gains sglobal 15, SMmod 8, ETmod 4, CONST_SY 4, OCmod2 3 |
| `core/array_limits.f90` | `array_limits` | 23 | sglobal 20, AL_D 3 |
| `core/build_info.f90` | `build_info` | 4 | sglobal 4 |
| `core/file_units.f90` | `file_units` | 50 | AL_C 25, AL_D 24, mod_error 1 |
| `core/run_context.f90` | `run_context` | 8 | sglobal 8 |
| `core/simulation_clock.f90` | `simulation_clock` | 8 | AL_D 4, AL_C 3, sglobal 1 |
| `core/runtime_flags.f90` | `runtime_flags` | 2 | sglobal 2 |
| `core/grid_topology.f90` | `grid_topology` | 7 | AL_G 5, AL_C 1, AL_D 1 |
| `core/element_geometry.f90` | `element_geometry` | 20 | AL_D 10, sglobal 7, AL_C 3 |
| `core/legacy_retained.f90` | `legacy_retained` | 26 | AL_D 18, sglobal 7, AL_C 1 |

## Sources

- **Retired here:** `src/core/sglobal.f90` (all 64 rows leave),
  `src/core/state/AL_G.F90` (all 5), `src/sediment/CONST_SY.F90` (all 4).
- **Shrinking:** `src/core/state/AL_C.F90` (33 of 91 leave; 58 remain until
  steps 03–04), `src/core/state/AL_D.f90` (60 of 154 leave; the rest go in
  steps 04, 12 and 13), `src/util/mod_error.f90` (its one unit number leaves),
  `src/snow/SMmod.f90`, `src/evapotranspiration/ETmod.f90`,
  `src/overland_channel/OCmod2.f90` (their constants leave).

## Order within the step

1. Create the nine new `core/` modules and extend `mod_parameters`. They are
   leaves: `mod_parameters` depends on nothing, and the others depend at most on
   `mod_parameters` (kinds) and `array_limits` (capacity bounds used as
   dimensions). Keep it that way — a `core/` module must never `USE` anything
   outside `core/`.
2. Apply the renames (§ below) at the declarations *and* at every use site.
3. Empty `sglobal.f90`, `AL_G.F90` and `CONST_SY.F90`, then `git rm` them.
4. Strip the moved declarations from `AL_C`, `AL_D`, `mod_error`, `SMmod`,
   `ETmod` and `OCmod2`, and give each of those files the `USE` lines it now
   needs.
5. Update all consumers.

## The 20 constant renames

From `constants_review.md`; `variables.csv` carries them in `proposed_rename`.

| Old | New | Old home |
|:----|:----|:---------|
| `GRAVTY`, `RHOSED`, `RHOWAT`, `VISCOS` | `GRAVITY`, `RHO_SEDIMENT`, `RHO_WATER_SEDIMENT`, `NU_WATER` | `CONST_SY` |
| `LAMDA`, `GAMMA`, `RHO`, `CP` | `L_VAPORISATION_ET`, `PSYCHROMETRIC_CONSTANT`, `RHO_AIR_ET`, `CP_AIR_ET` | `ETmod` |
| `RHOA`, `RHOW`, `CPA`, `CPW`, `CPI`, `LWI`, `LVW`, `HFG` | `RHO_AIR_SNOW`, `RHO_WATER_SNOW`, `CP_AIR_SNOW`, `CP_WATER`, `CP_ICE`, `L_FUSION`, `L_VAPORISATION_SNOW`, `GROUND_HEAT_FLUX_SNOW` | `SMmod` |
| `F23`, `F53`, `ROOT2G` | `TWO_THIRDS`, `FIVE_THIRDS`, `SQRT_TWO_G` | `OCmod2` |

`vsmall` moves from `sglobal` to `mod_parameters` under its own name.

Hazards:

- `CP`, `RHO`, `GAMMA`, `F23`, `F53` are short identifiers. Rewrite them with a
  word-boundary match, one file at a time, and count the replacements before and
  after (`grep -cwi`). Check every hit that is inside a comment, a string or a
  format label by eye.
- The values do not change. `RHO = 1.2`, `GAMMA = 0.659`, `CP = 1003.` and
  `LAMDA = 2465000.` stay written exactly as they are, default-real literal and
  all (`constants_review.md` §3).
- `src/visualisation/visualisation_interface_left.f90` imports
  `CONST_SY, ONLY: rhosed`; it becomes `mod_parameters` / `RHO_SEDIMENT`.

## Consumers to update

| Retired module | Files importing it | Bare `USE` |
|:---------------|-------------------:|-----------:|
| `sglobal` | 36 | 13 |
| `AL_G` | 10 | 1 (`CMmod`) |
| `CONST_SY` | 4 | 2 (`OCmod`, `SYmod`) |

`_usage_modules.csv` lists them by name. The 13 bare `USE SGLOBAL` statements
each become several explicit `ONLY` imports; build the first draft from
`variables.csv` (`referenced_by` / `possibly_referenced_by` for the sglobal
rows), then compile and let the errors complete it.

`test/CMakeLists.txt:43` names `src/core/sglobal.f90` in the
`visualisation_read_tests` target because `mod_error` pulls it in. After this
step `mod_error` imports from the `core/` modules instead — put exactly those
files in the target's source list, in dependency order, and check the test still
builds and passes.

## Hazards

- `runtime_flags` exists only to keep error reporting and timestep control
  acyclic. Its two variables must not be merged into anything larger.
- `legacy_retained` holds the 26 variables with no producer or consumer. They
  are quarantined, not deleted.
- `file_units` gathers unit numbers from three modules; watch for two different
  names holding the same number — that is expected and must be preserved.
- After removing entities from `AL_C`/`AL_D`, files that reached them through a
  bare `USE AL_C` may lose names they never imported directly (see
  `00_working_rules.md` §7).

## Run

```bash
# look first
python3 scripts/rename_extract.py --dry-run --target-dir src/core \
    --source src/core/sglobal.f90 \
    --source src/core/state/AL_G.F90 \
    --source src/sediment/CONST_SY.F90 \
    --source src/core/state/AL_C.F90 \
    --source src/core/state/AL_D.f90 \
    --source src/util/mod_error.f90 \
    --source src/snow/SMmod.f90 \
    --source src/evapotranspiration/ETmod.f90 \
    --source src/overland_channel/OCmod2.f90

# then drop --dry-run
```

`src/core/mod_parameters.f90` already exists, so its 34 arriving constants land
in `src/core/mod_parameters.f90.append`; merge and delete that file. The nine
other `core/` modules are written whole. The renames are applied afterwards, by
hand, at the declarations in `mod_parameters` and at every use site.

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

- [ ] `src/core/` holds exactly the ten files above; `src/core/state/` still
      holds `AL_C.F90` and `AL_D.f90` only.
- [ ] `sglobal.f90`, `AL_G.F90`, `CONST_SY.F90` are `git rm`-ed.
- [ ] `grep -rniE '\bUSE +(SGLOBAL|AL_G|CONST_SY)\b' src test` is empty.
- [ ] `grep -rniE '\[\[(sglobal|al_g|const_sy)[]:]' src test` is empty.
- [ ] The old constant names appear nowhere: `grep -rnwE '(GRAVTY|RHOSED|RHOWAT|VISCOS|LAMDA|RHOA|RHOW|CPA|CPW|CPI|LWI|LVW|HFG|ROOT2G|F23|F53)' src test`.
- [ ] Debug build + `ctest` pass; `test/CMakeLists.txt` updated.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Committed.
