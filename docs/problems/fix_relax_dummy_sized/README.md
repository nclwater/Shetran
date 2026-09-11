# Relaxing the fixed-size input workspaces

**Status:** planned, not started.
**Anchor commit:** `e09f465` ("optimised dummy array sizes").
**Predecessor work:** [idum_idummy_analysis.md](../../rename/issues/idum_idummy_analysis.md)
(the size audit) and [idum_idummy_rework_impact.md](../../rename/issues/idum_idummy_rework_impact.md)
(why assumed-shape is the wrong instrument). Read both before starting.

## The problem in one paragraph

A family of legacy input readers and validators take scratch "workspace"
arrays as arguments. The dummies are declared at compile-time worst-case
extents (`IDUM(NXEE*NYEE)`, `DUMMY(NELEE)`, `NOTOK(N0:N1)`), which forces every
caller to own an array of that worst-case size even when the real requirement
is three orders of magnitude smaller. Worse, most call sites pass these arrays
purely as placeholders for arguments the callee never touches for that mode.
`e09f465` trimmed the extents that could be trimmed without touching an
interface; this plan removes the interface constraint itself.

## What is left to fix (measured at `e09f465`, Debug binary)

| Workspace class | Symbols | Bytes |
|:----------------|--------:|------:|
| `idum`/`dummy` locals | 15 | 26,164,400 |
| `ldum1` (ALCHK failure-mask scratch) | 1 | 1,000,000 |
| **Total in scope** | **16** | **27,164,400** |

Reproduce with:

```bash
nm --size-sort -S build/debug/bin/shetran \
  | grep -E '\b(idum|dummy|ldum1)\.[0-9]+' \
  | awk '{t+=strtonum("0x"$2)} END{printf "%d bytes in %d symbols\n", t, NR}'
```

Placeholder arguments that carry no data:

| Callee | Call sites | Dead data arguments |
|:-------|-----------:|--------------------:|
| `ALREAD` | 52 | 103 of 156 |
| `ALCHK` + `ALCHKI` | 234 | 234 of 234 (`NOTOK` is never read by any caller) |

## Decisions already taken

These were decided by the maintainer. Do not revisit them; if you believe one
is wrong, stop and raise it rather than deviating.

1. **`ALREAD`'s data arguments become `OPTIONAL`.** Not a generic-interface
   split. `FLAG` stays. The change is backward compatible, so call sites can be
   converted incrementally.
2. **Workspaces become module-owned, one named array per distinct need.** Do
   not reintroduce a single shared `IDUM`/`DUMMY` pair, and do not carry the
   names `IDUM`/`DUMMY` forward — each array is named for what it holds.
3. **Scope includes the shared helpers** — `ALCHK`, `ALCHKI` and `ALALLI`, not
   only the `IDUM`/`DUMMY` chain.
4. **Acceptance is identical output on all 13 baseline models**, per phase,
   verified with the project harness. See "Acceptance" below.

### One refinement to decision 2

The `NREQ` checks in `CMRD` (three) and `SYREAD` (three) look like capacity
management but are **input validation** — they reject a malformed input file
whose declared counts are absurd. Keep them exactly as they are. Only the
allocation changes. A grow-on-demand workspace must not silently honour a
request for 10^9 elements because an input file said so.

## Phases

Each phase is a separate commit with its own full verification run. Phases 1
and 2 are independent of each other and of everything else; 4 through 8 depend
on 3 but are independent of one another and may be done in parallel.

| # | File | Unit of work | Depends on |
|:--|:-----|:-------------|:-----------|
| 1 | [01-alread-optional.md](01-alread-optional.md) | `ALREAD` data arguments become `OPTIONAL`; 103 placeholders deleted | — |
| 2 | [02-alchk-optional.md](02-alchk-optional.md) | `ALCHK`/`ALCHKI` `NOTOK` becomes `OPTIONAL`; 215 placeholders deleted | — |
| 3 | [03-workspace-support.md](03-workspace-support.md) | Shared `ensure_capacity` helper and the workspace-module convention | — |
| 4 | [04-chain-sediment.md](04-chain-sediment.md) | `SIMULATION` → `SYMAIN` → `SYERR1/2/3`, `SYREAD` | 3 |
| 5 | [05-chain-contaminant.md](05-chain-contaminant.md) | `INCM` → `CMRD` → `ALREDI`/`ALREDF`/`ALALLI` | 3 |
| 6 | [06-chain-subsurface.md](06-chain-subsurface.md) | `VSREAD`, `VSCONC` | 3 |
| 7 | [07-chain-overland.md](07-chain-overland.md) | `OCINI`/`OCCHK2`, `OCREAD`, `JEOCBC`, `INBK` | 3 |
| 8 | [08-local-only-workspaces.md](08-local-only-workspaces.md) | `OCCHK1`, `COLMSM` — demote to ordinary locals | — |
| — | [reference-inventory.md](reference-inventory.md) | Measured inventory; consult, do not edit | — |

## Rules that apply to every phase

**Do not change numerical behaviour.** Every phase in this plan is a storage
and interface change. If a phase produces a different number anywhere, you have
introduced a bug — do not rationalise it as "within tolerance".

**Fortran constraints that will bite you.**

- `OPTIONAL` requires an explicit interface. Every routine here is a module
  procedure, so this holds — but confirm the caller has the `USE` rather than
  assuming it.
- A dummy that is `OPTIONAL` and `INTENT(OUT)` must be guarded by `PRESENT()`
  before *any* reference, including appearing in an I/O list.
- A non-present optional cannot be passed on to a non-optional dummy.
- Assumed-size (`(*)`) dummies cannot take an open-ended section: `IDUM(I:)` is
  invalid and must become `IDUM(I:N)`. Two such sites exist
  (`src/sediment/sy_validation.f90`, the two `ALCHKI` calls with `IDUM(ICOL1:)`).
- Assumed-size dummies cannot be whole-array references in an I/O list.
  `READ (unit,*) IDATA` must become `READ (unit,*) IDATA(1:N1*N2)`.
- Passing an array element (`DUMMY(INDX+1)`) to a sequence-associated dummy is
  legal only if the parent is contiguous. Allocatables are contiguous; do not
  make these arrays `POINTER`.

**Do not grow the stack.** `scripts/list_stack_size_hits.py` exists because
stack pressure is already a known problem in this codebase. Where a phase needs
a scratch buffer that was previously a caller-supplied array, use a
module-owned allocatable — not an automatic array sized from a dummy argument.

**Naming.** No new array may be called `IDUM`, `DUMMY`, `LDUM`, or any numbered
variant. Name for content: `sy_element_codes`, `cm_record_values`,
`vs_grid_codes`, `oc_link_values`. Add the `!!` doc comment stating what the
array holds and what bounds its extent, matching the style already used at the
trimmed declarations in `e09f465`.

## Acceptance

Per phase, all of the following must hold before the commit is considered done:

1. `cmake --build build/debug` and `cmake --build build/release` both complete
   with no new warnings. Capture the warning count before and after.
2. A `-fcheck=bounds` Debug run (the Debug preset already sets this) of at
   least `Cobres`, `Slapton-3D-1year-nitrate` and `38014-100m-SurfaceErrors`
   completes with "Normal completion of SHETRAN run" and no bounds diagnostic.
   These three are chosen to exercise the sediment, nitrate/contaminant, and
   large-grid paths respectively.
3. **No numerical difference on any of the 13 models that have a baseline**,
   established with the project harness — see "Running the checks" below for
   the invocation and for the two ways a green result can still hide a
   difference.
4. The storage figure moved in the expected direction, evidenced by the `nm`
   command above, and the number is recorded in the phase's commit message.

The 13 models with baselines are: `38014-100m-SurfaceErrors`,
`Aire_at_Kildwick_Bridge-AllOptions-withdates`, `Aire_at_Kildwick_Bridge-simple`,
`Cobres`, `Cobres1D`, `dunsop`, `dunsop-hot1`, `dunsop-hot2`, `foston100m`,
`reservoir-ZQmodule-example`, `Slapton`, `Slapton-1D-1year-nitrate`,
`Slapton-3D-1year-nitrate`.

### Running the checks

Use the project's own harness. It runs each model in `examples/<model>/compute`
— the same directory the baselines were generated from — and writes a per-model
`comparison_results.csv` plus an overview CSV. Neither is tracked by git, and
`examples/*/compute/` is gitignored, so a run leaves the tree clean.

The required packages live in the `shetran` conda environment. Note that
`conda run -n shetran python` misresolves to `/usr/bin/python` on this machine
and will fail with `ModuleNotFoundError: pandas`. Use either the fish hook or
the interpreter path:

```fish
conda activate shetran
cd examples
python check_results_consistency.py -l all -o comparison_overview_<phase>.csv
```

```bash
# equivalent, no activation needed
cd examples
/home/tolstoi/Apps/miniforge3/envs/shetran/bin/python \
    check_results_consistency.py -l all -o comparison_overview_<phase>.csv
```

Useful flags: `-m <model>` (repeatable) to run one model while iterating,
`-s <model>` to skip one, `--skip-simulation` to re-compare an existing
`compute/` without rerunning, `--shetran-exe` to point at a different binary
(the default is already `build/release/bin/shetran`).

`-l` selects from the lists in `examples/_methods/settings.py`. At `e09f465`:
`all` = 13 models, `short` = 10, `medium` = 13, and `long` = 13 because
`list_long_running` is empty. **Use `-l all`** — `short` omits
`38014-100m-SurfaceErrors`, `foston100m` and `reservoir-ZQmodule-example`, and
the first of those is the large-grid model that most exercises the grid
workspaces this plan resizes.

### Reading the result

The overview CSV is the gate. For every model:

- `any_differences` must be `False`;
- `num_files_with_different_contents` must be `0`;
- `num_files_too_large_to_compare` must be **no higher than the pre-change
  run**, and you must account for any file it names.

That last point matters. Files above the size thresholds in `settings.py`
(text 20 MB, table 50 MB, HDF5 250 MB) are **skipped, not compared**, and a
skipped file still reports as no-difference. At `e09f465` two are skipped:
`output_Aire_at_Kildwick_Bridge_spr.txt` and `output_slap-shegraph.h5`. A green
overview therefore does not by itself cover those two. Compare them by hand —
`diff` for the text file, `h5diff -c` for the HDF5 one (it takes several
minutes; that is not a hang).

The harness is exact for plain text files (line-by-line `difflib` after
`rstrip`) but tolerance-based for table and HDF5 data (`tolerance_numeric`
1e-5, `tolerance_table` 1e-3). **Every phase in this plan is a storage and
interface change and must produce exactly the same numbers.** So also confirm
that `abs_max_difference` is zero or empty for every row of every
`comparison_results.csv`. A difference that is merely within tolerance is still
a bug here:

```bash
cd examples
awk -F, 'FNR>1 && $7!="" && $7+0!=0 {print FILENAME": "$0}' */comparison_results.csv
```

(check the column index against the header before trusting that one-liner).

### Do not regenerate the baselines

`setup_results_check.py` overwrites `output_should/` with whatever the current
binary produces. Running it makes any phase pass. It is the right tool when the
baselines are genuinely stale or absent, and the wrong tool every other time.
If you believe a baseline is wrong, stop and say so rather than refreshing it.

## What this is worth, honestly

Completed in full, this returns roughly 27 MB of a 1.64 GB `.bss` — about 1.6%.
The larger payoff is the 337 deleted placeholder arguments and the removal of a
class of latent sequence-association defects (of which the `ALREAD` VS08c case
documented in the predecessor analysis is one confirmed instance). If the goal
is purely static footprint, `cm_plant_state:PDZF3` (191 MB) and
`mn_state:MN_PLANT_STATE` (137 MB) are each worth more than this entire plan,
and neither is touched here.
