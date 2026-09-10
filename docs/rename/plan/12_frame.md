# Step 12 — `src/frame/`

**Goal.** Dissolve `FRmod` (6,086 lines, 47 procedures) into six modules. By now
it has already given up its five `IN*` component-setup routines to the
components (steps 07, 08, 09, 11), so what is left is frame work proper.

**Rows.** `--target-dir src/frame` — 109 rows.

## Target files

| File | Module | Procs | Vars | Comes from |
|:-----|:-------|------:|-----:|:-----------|
| `frame_setup.f90` | `frame_setup` | 6 (+4 nested) | – | `FRmod` |
| `frame_geometry.f90` | `frame_geometry` | 4 | – | `FRmod` |
| `frame_output.f90` | `frame_output` | 3 (+20 nested) | 20 | `FRmod` |
| `legacy_result_files.f90` | `legacy_result_files` | 3 | 22 | `FRmod` 12, `AL_D` 13 |
| `mass_balance_report.f90` | `mass_balance_report` | 1 | 2 | `FRmod` |
| `run_control.f90` | `run_control` | – | 24 | `AL_D` 16, `FRmod` 8 |

`run_control` holds the run title, the `BEX*` component switches and the
hotstart settings.

## Order within the step

1. `run_control` — data only, no procedures. Everything else may read it.
2. `legacy_result_files`, then `mass_balance_report` (which calls into it).
3. `frame_geometry`, `frame_output`, `frame_setup`.
4. `git rm src/frame/FRmod.f90`.

## Hazards

- **`FRRESC`/`FRRESP` ↔ `res_write_check`** are mutually recursive (two pairs,
  three procedures) and all three must land in `legacy_result_files`. With
  `LINK`/`SNL3` in `cm_channel`, these are the only mutual recursions in the
  tree.
- **`PREVTM` is `FRRESP`'s own state** and goes to `legacy_result_files`, not to
  `mass_balance_report`. `FRMB` calls `FRRESP`, which reads `PREVTM`; the other
  placement closes a cycle.
- `frame_output` carries 20 nested procedures inside 3 parents — by far the
  densest nesting in the tree. They travel inside their parents' spans; do not
  promote any of them.
- **`DINET`, `DINOC` and `DOCIN` stay as they are.** They are stand-ins for
  disabled components: `DINET` prints a message and sets `BMETAL = .TRUE.`,
  `DINOC` prints a message, and `DOCIN` has an empty body and no caller at all.
  Move them with the routines that call them; do not delete `DOCIN`.
- `FRmod` `USE`s all the component modules; after steps 05–11 those imports name
  the new modules. Rebuild each new frame module's import list from what its own
  bodies use.

## Consumers

`FRmod` is imported by `src/Shetran.f90` (`FROPEN`, … → `frame_setup`),
`rest` and `run_sim`. Those three are dissolved in step 13, so expect to touch
their `USE` lines twice — that is cheaper than reordering the steps, because
`Shetran.f90` names `rest` and `run_sim` directly.

## Run

```bash
python3 scripts/rename_extract.py --dry-run --target-dir src/frame \
    --source src/frame/FRmod.f90 \
    --source src/core/state/AL_D.f90

# then drop --dry-run
```

By now `FRmod` is down to about 4,400 lines; 4,253 of them move here and roughly
120 non-blank lines are left as the shell to `git rm`. `FRmod`'s header is the
longest in the tree — read the coverage report before deleting anything.

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

- [ ] Six files; `FRmod.f90` is `git rm`-ed.
- [ ] `grep -rniE '\bUSE +FRMOD\b|\[\[frmod[]:]' src test` is empty.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Build + `ctest` pass. Committed.
