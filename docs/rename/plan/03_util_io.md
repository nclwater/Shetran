# Step 03 — `src/util/` and `src/io/`

**Goal.** Split the four generic helper modules into eight `util/` leaves and
six `io/` modules. Nothing depends on their internals, but `mod_error` has 29
consumers, so this is the second-widest `USE` sweep after step 02.

**Rows.** `--target-dir src/util` (56) and `--target-dir src/io` (25).

## Target files

| File | Module | Procs | Vars | Comes from |
|:-----|:-------|------:|-----:|:-----------|
| `util/error/error_reporting.f90` | `error_reporting` | 3 | 8 | `mod_error` |
| `util/error/error_status.f90` | `error_status` | 7 | 7 | `mod_error` |
| `util/datetime.f90` | `datetime` | 5 | – | `utilsmod` |
| `util/interpolation.f90` | `interpolation` | 2 | – | `utilsmod` (`TERPO1`), `mod_load_filedata` (`ALINTP`) |
| `util/linear_algebra.f90` | `linear_algebra` | 7 | 2 | `utilsmod` |
| `util/random_numbers.f90` | `random_numbers` | 1 | – | `utilsmod` (`ran2`) |
| `util/float_compare.f90` | `float_compare` | 13 | – | `tolerance_testing` |
| `util/platform_traps.f90` | `platform_traps` | 1 | – | `mod_load_filedata` (`ALTRAP`) |
| `io/record_readers.f90` | `record_readers` | 6 (+1 nested) | 6 | `mod_load_filedata` |
| `io/spatial_fields.f90` | `spatial_fields` | 4 | – | `mod_load_filedata` |
| `io/input_validation.f90` | `input_validation` | 2 | – | `mod_load_filedata` |
| `io/grid_arrays.f90` | `grid_arrays` | 2 | – | `utilsmod` (`AREADI`, `AREADR`) |
| `io/timeseries_input.f90` | `timeseries_input` | 2 | – | `utilsmod` (`FINPUT`, `HINPUT`) |
| `io/input_workspace.f90` | `input_workspace` | – | 2 | `AL_C` (`IDUM`, `DUMMY`) |

`src/util/error/` and `src/io/` are new directories.

## Sources

Retired here in full: `src/util/mod_error.f90`, `src/util/tolerance_testing.f90`,
`src/util/utilsmod.f90`, `src/util/mod_load_filedata.f90`. `AL_C` gives up two
more variables (56 remain for step 04).

## Order within the step

1. `error_reporting` first, then `error_status` — the seam is one-directional:
   `error_status` holds the `errstat_*` wrappers that inspect an
   `IOSTAT`/`STAT` and calls `error_reporting`, never the reverse. Do not let a
   `USE` point the other way.
2. `float_compare`, `datetime`, `random_numbers`, `linear_algebra`,
   `interpolation`, `platform_traps` — independent leaves, any order.
3. The `io/` modules.
4. Delete the four source files and update consumers.

## Consumers to update

| Retired module | Files | Note |
|:---------------|------:|:-----|
| `mod_error` | 29 | 11 of them are `src/visualisation/` files that import only `errstat_*` → `error_status`; `visualisation_metadata` also takes `errstat_fileclose` |
| `tolerance_testing` | 10 | straight rename to `float_compare` |
| `utilsmod` | 8 | imports split across `datetime`, `linear_algebra`, `interpolation`, `random_numbers`, `grid_arrays`, `timeseries_input` |
| `mod_load_filedata` | 8 | split across the `io/` modules plus `platform_traps` and `interpolation` |

`visualisation_interface_left` takes `RAISE_ERROR` and `ERRLVL_fatal` from
`error_reporting`, the `errstat_*` names from `error_status`, and `FID_logfile`
from `file_units` (moved in step 02).

`src/Shetran.f90` imports `mod_load_filedata, ONLY: ALTRAP` → `platform_traps`.

`test/CMakeLists.txt:44` names `src/util/mod_error.f90`: replace with
`src/util/error/error_reporting.f90` and `src/util/error/error_status.f90`, in
that order (the test target is not topologically sorted — it lists sources by
hand).

## Hazards

- Two of `mod_load_filedata`'s procedures are not input at all: `ALTRAP` goes to
  `util/platform_traps`, `ALINTP` to `util/interpolation`. Do not sweep them
  into `io/` with the rest.
- `record_readers` has one nested procedure (`throw_fatal`, inside `ALREAD`) —
  it travels inside its parent's span.
- `mod_error` currently `USE`s `mod_parameters`, `sglobal` (rewritten in step
  02) and `stdlib_strings`. `error_reporting` keeps the `stdlib_strings`
  dependency; make sure the `fortran_stdlib_strings` link in the test target
  still matches whichever new module needs it.
- `input_workspace` exists only to hold `IDUM` and `DUMMY`. It is deliberately
  trivial; do not merge it into a reader.

## Run

```bash
python3 scripts/rename_extract.py --dry-run \
    --target-dir src/util --target-dir src/io \
    --source src/util/mod_error.f90 \
    --source src/util/tolerance_testing.f90 \
    --source src/util/utilsmod.f90 \
    --source src/util/mod_load_filedata.f90 \
    --source src/core/state/AL_C.F90

# then drop --dry-run
```

`mod_error` is backed up a second time here (step 02 already took its unit
number), which is why the extractor locates entities by content rather than by
the CSV line numbers.

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

- [ ] The 14 files above exist; the four source files are `git rm`-ed.
- [ ] `grep -rniE '\bUSE +(MOD_ERROR|TOLERANCE_TESTING|UTILSMOD|MOD_LOAD_FILEDATA)\b' src test` is empty.
- [ ] `grep -rniE '\[\[(mod_error|tolerance_testing|utilsmod|mod_load_filedata)[]:]' src test` is empty.
- [ ] `test/CMakeLists.txt` updated; both test targets build and pass.
- [ ] Every new module has `summary:`, author, prose and an `@history` row;
      `grep -rn "TODO" src` is empty.
- [ ] The retired modules' header prose, tables and `@warning` blocks are
      redistributed; section comments from the coverage report are placed.
- [ ] `python3 scripts/rename_ford_links.py --check src test` shows nothing left
      for the modules this step retired, and its `MANUAL` links are resolved.
- [ ] No `*.backup` or `*.append` file is left in the tree.
- [ ] Committed.
