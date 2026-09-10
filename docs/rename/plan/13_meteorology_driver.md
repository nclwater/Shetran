# Step 13 — `src/meteorology/` and `src/driver/`

**Goal.** Dissolve `rest` — the least descriptive name in the tree — plus
`run_sim` and `getdirqq`, create `src/meteorology/`, and retire the last 54
variables of `AL_D`.

**Rows.** `--target-dir src/meteorology` (28) and `--target-dir src/driver` (26).

## Target files

| File | Module | Procs | Vars | Comes from |
|:-----|:-------|------:|-----:|:-----------|
| `meteorology/met_input.f90` | `met_input` | 3 | 11 | `rest` (`METIN`, `READ_DATED_RECORD`, `RESIZE_MET_RECORD`) |
| `meteorology/met_forcing.f90` | `met_forcing` | – | 14 | `AL_D` (`TA`, `U`, `VPD`, `RN`, `OBSPE`, `precip_m_per_s`, station mapping, input intervals) |
| `driver/timestep_control.f90` | `timestep_control` | 1 | 4 | `rest` (`TMSTEP`), `AL_D` |
| `driver/water_balance.f90` | `water_balance` | 1 | 9 | `rest` (`BALWAT`), `AL_D` |
| `driver/run_summary.f90` | `run_summary` | 1 | – | `rest` (`EXTRA_OUTPUT`) |
| `driver/command_line.f90` | `command_line` | 4 (+3 nested) | 2 | `getdirqq` |
| `driver/simulation_driver.f90` | `simulation_driver` | 1 | – | `run_sim` (`SIMULATION`) |

Meteorological forcing is currently split between `rest` (the reader) and `AL_D`
(the values); both halves arrive here.

## Order within the step

1. `met_forcing` (data) and `met_input`.
2. The three `driver/` modules out of `rest`; `git rm src/driver/rest.f90`.
3. `command_line` out of `getdirqq`; `simulation_driver` out of `run_sim`;
   `git rm` both.
4. `AL_D.f90` is now empty — `git rm` it and remove `src/core/state/`.
5. Rewrite `src/Shetran.f90`'s `USE` block (see below).

## `src/Shetran.f90`

All eight of its `USE` lines name modules that move. Most were already rewritten
in earlier steps; this is where the last of them land. The full mapping, from the
proposal:

| Currently | Becomes | Step |
|:----------|:--------|:-----|
| `SGLOBAL` (whole module) | the `core/` modules it needs | 02 |
| `AL_D, ONLY: nstep` | `simulation_clock` | 02 |
| `mod_load_filedata, ONLY: ALTRAP` | `platform_traps` | 03 |
| `GETDIRQQ, ONLY: GET_DIR_AND_CATCH, RUNDATA_FROM_FILE_DIALOG` | `command_line` | 13 |
| `FRmod, ONLY: FROPEN, …` | `frame_setup` | 12 |
| `REST, ONLY: extra_output` | `run_summary` | 13 |
| `RUN_SIM, ONLY: SIMULATION` | `simulation_driver` | 13 |
| `OCmod, ONLY: FINALISE_OCSIM_WORKSPACE` | `oc_driver` | 11 |

## Hazards

- `getdirqq` carries the Windows-only `IFWIN` import and the QuickWin file
  dialog. Keep the conditional structure exactly as it is; this step must not
  change what is compiled on Windows.
- `run_sim` imports `visualisation_interface_left` and
  `visualisation_interface_right`; `simulation_driver` inherits those imports
  unchanged.
- After `AL_D` is gone, no module named `AL_*` or `sglobal` exists anywhere.
  This is the step where the two sweeps in `00_working_rules.md` §8 must come
  back completely empty.

## Run

```bash
python3 scripts/rename_extract.py --dry-run \
    --target-dir src/driver --target-dir src/meteorology \
    --source src/driver/rest.f90 \
    --source src/driver/run_sim.f90 \
    --source src/driver/getdirqq.f90 \
    --source src/core/state/AL_D.f90

# then drop --dry-run
```

`AL_D` ends this step as four lines — `MODULE`, its `USE`, `IMPLICIT NONE`,
`END MODULE` — and is deleted. `run_sim`'s residual is its long `USE` block,
which moves to `simulation_driver` rewritten against the new module names.

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

- [ ] Seven files; `rest.f90`, `run_sim.f90`, `getdirqq.f90` and `AL_D.f90` are
      `git rm`-ed; `src/core/state/` is gone.
- [ ] `grep -rniE '\bUSE +(REST|RUN_SIM|GETDIRQQ|AL_D)\b' src test` is empty;
      the same for `[[…]]` links.
- [ ] `src/Shetran.f90` matches the table above.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
