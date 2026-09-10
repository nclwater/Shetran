# Deviations

Everything the implementation had to decide that
[`README.md`](README.md) and the four move tables do not answer, plus every
place where the plan's own text disagrees with the tree.

**Baseline commit: `f31d1b82e578b2c618b16236a2e221367a5be0f0`**
(branch `reorg_functions`). Every `git show <BASE>:<path>` in the steps refers
to this commit, because the move tables' line numbers are only valid there.

## Baseline environment (step 01)

| Item | State at the baseline |
|:-----|:----------------------|
| `check_target_cycles.py` | `No cycles among the 69 proposed modules that have edges.`, exit 0 |
| Debug build (gfortran) | succeeds |
| Release build (gfortran) | succeeds |
| `ctest` | 3 tests, all pass — but see D1 |
| `rename_rows.py` | 1364 rows; 199 for `--target-dir src/core` — both as documented |
| `rename_extract.py --verify` | 0 mismatches over all 40 source files |
| `rename_ford_links.py --check` | 42 files would change, **57** links need a decision — see D2 |
| `audit_ford_docs.py` | 329 lines, captured as `/tmp/ford_audit_baseline.txt` |
| `ford` | **not installed** — see D3 |
| `ifx` | not installed — the close-out ifx build cannot be run |

## D1 — `oc_row_width_tests` is not built by `build.sh --test`

*What the plan says.* `00_working_rules.md` §10: "`build.sh --test` runs only
`-R '^visualisation_read\.'`; the bare `ctest` above is what also exercises
`oc_row_width.unit`."

*What the tree does.* `build.sh --test` builds only the
`visualisation_read_tests` target, so the `oc_row_width_tests` executable does
not exist and the bare `ctest` reports `oc_row_width.unit ... ***Not Run`. That
is a missing binary, not a failing test.

*What was done.* The step-verification command sequence gained an explicit
target build:

```bash
./build.sh -t Debug -c gfortran --clean-app --test
cmake --build build/debug --target oc_row_width_tests -j"$(nproc)"
( cd build/debug && ctest --output-on-failure )     # 3/3 pass
```

*Cost.* None to the move; one extra command per step. Making `build.sh --test`
build both targets would be an improvement to the build script, which is out of
scope here.

## D2 — the plan's "68 links needing a decision" is 57

*What the plan says.* `00_ford.md` §3 and `00_tooling.md` §3: 68 links need a
human decision — 57 bare `[[module]]` links whose module splits several ways,
plus "11 links that are already broken today", namely
`[[mod_error:RAISE_ERROR]]`, `[[mod_error:ERR_STOP]]` and
`[[mod_error:err_set_wait_on_exit]]` in `sglobal.f90`, `mod_error.f90` and
`mod_load_filedata.f90`.

*What the tree does.* Those three entities all exist in `mod_error`:
`functions.csv` records `err_set_wait_on_exit` at `mod_error.f90:108-112`,
`RAISE_ERROR` at `487-627` and `ERR_STOP` at `654-675`. The links therefore
resolve, the rewriter re-points them automatically along with the rest, and
`--check` reports 57 `MANUAL` lines, all of the single kind
`module split; pick the successor`.

*What was done.* 57 is taken as the baseline figure. The "pre-existing dead
links" paragraph of `00_ford.md` §3 has no cases to apply to and is not acted
on; no dead links are recorded here.

*Cost.* None. It removes work the plan expected, and there is no lost check:
had those links really been broken they would have shown up as `MANUAL` lines.

## D3 — FORD is not installed

`command -v ford` finds nothing, so `./build.sh --docs-only` cannot be run and
the FORD half of the per-step checklist reduces to what
`00_working_rules.md` §8 calls "the link sweeps are the only check":
`rename_ford_links.py --check` plus the `grep` sweeps for retired names. The
generated `docs/ford/` pages named in `14_closeout.md` §4 cannot be inspected.
`audit_ford_docs.py` does run and is diffed against the baseline as planned.

## D4 — a fourth script: `scripts/rename_use_lines.py`

*What the plan says.* `00_working_rules.md` §1: "Hand editing is for what a
script cannot decide: `USE` lines, header prose, `PUBLIC` lists."

*What the tree needed.* Step 02 alone re-points 67 explicit
`USE …, ONLY:` statements across 30 files, because entities left `sglobal`,
`AL_G`, `CONST_SY`, `AL_C`, `AL_D` and `mod_error` in the same step. Which
target each name goes to, and under which of the 20 new names, is decided
entirely by the move tables — there is nothing for a human to choose, and by
hand it is 67 chances to mistype an identifier.

*What was done.* Added `scripts/rename_use_lines.py`, which splits an explicit
`ONLY` list by target module, preserves `alias => name` forms and the renames,
drops a statement whose every name has left, and reports bare
`USE <module>` statements — which it cannot rewrite, having no name list — for
the hand work the plan intends. It follows the same `.backup` rule as the other
three scripts and takes the same `--target-dir`/`--target` filters.

The hand work the plan asks for is unchanged and still substantial: the 13 bare
`USE SGLOBAL` statements, every FORD header, every `PUBLIC` list, and the
imports that a *shrinking source* needs for names it still uses but no longer
declares (`AL_D` gained `NCLASS`, `mod_error` gained `FID_logfile`, `CMmod`
gained four statements for what it used to reach through `USE AL_C`).

*Cost.* One more script to keep in step with the tables. It is committed with
step 02 rather than step 01.

## D5 — the ten `core/` modules are public by default

*What the plan says.* `00_working_rules.md` §4: "data modules are public by
default today (`AL_C`, `AL_D`, `sglobal`, the `COLM_*` modules) and must stay
that way. New *procedure* modules get `PRIVATE` plus an explicit `PUBLIC ::`
list."

*What the tree does.* `sglobal` is not public by default: it declares `PRIVATE`
and lists all 64 of its own names in nine explicit `PUBLIC` statements, so that
"the imported `I_P`, `R8P`, and `LENGTH_FILEPATH` parameters remain private"
(its own header). `AL_C`, `AL_D` and `AL_G` *are* public by default — `AL_G`
even has its `PRIVATE :: NELEE, NXEE, NYEE` commented out, so it already
re-exports its imports.

*What was done.* The rule's intent — these are data modules and their contents
stay visible — was followed over its parenthesis: none of the ten `core/`
modules declares `PRIVATE`, and `sglobal`'s nine `PUBLIC` statements are not
carried over. Every name that was public stays public.

*Cost.* The kind parameters are now re-exported by the seven `core/` modules
that import them, which `sglobal` deliberately prevented. Nothing in the tree
takes a bare `USE` of a `core/` module, so no consumer picks up a name this
way; and a duplicate path to the same `mod_parameters` entity is not an error
in Fortran. Restoring the arrangement would mean writing a `PUBLIC ::` list for
each of the ten modules plus `PRIVATE :: I_P, R8P, LENGTH_FILEPATH` — worth
doing if the `core/` modules ever acquire a bare consumer.

## D6 — deletions forced by a module ceasing to exist

Three things were deleted rather than moved, all because they name a module
that no longer exists and the step-02 sweeps require the name to be gone:

- **Seven commented-out `USE SGLOBAL` statements** (`run_sim.f90:59`,
  `rest.f90:30`, `SMmod.f90:50`, `VSmod.f90:110`, `:169`, `:239`, `:258`).
  Each is a stale alternative form of the bare `USE SGLOBAL` on the line above
  it, which this step replaced with an explicit list. `grep -rniE '\bUSE
  +SGLOBAL\b'` matches inside a comment, so they had to go either way.
- **`USE CONST_SY` inside `OCCHK2`** (`OCmod.f90`). The routine's own
  `@warning` block records that this import "is unused in this routine's
  current body", and `grep` confirms it: none of `CONST_SY`'s four constants
  appears in `OCCHK2`. The statement went with the module; the `@warning` now
  records that it did.
- **Two orphaned section comments.** `! File units occupy their rundata
  positions.` in `AL_D` and its longer form in `AL_C` described declarations of
  which none remain in either file — all 49 unit numbers went to `file_units`.
  Both sentences are carried into `file_units`'s header, so nothing is lost.

## D7 — renamed constants inside LaTeX math

Four doc-comment lines name a renamed constant inside a `\[ … \]` display-math
block: `ETmod.f90:278`, `:285`, `:324` and `SMmod.f90:215`. The old names
(`LAMDA`, `GAMMA`, `RHO`, `CP`, `HFG`) contain no underscore; every new name
does, and `_` is the subscript operator in math mode, so
`L_VAPORISATION_ET` would render as an L with a subscript. The underscores are
escaped as `\_` on those four lines only. This could not be verified against
rendered output, because FORD is not installed (D3).

## D8 — the link rewriter points forward, mid-migration

`rename_ford_links.py` reads the move tables, not the tree, so running it in
step 02 as the step instructs re-points links at their *final* module even when
that module arrives in a later step: `mod_parameters`'s header now links to
`[[zq_tables]]`, `[[command_line]]` and `[[timeseries_input:FINPUT]]`, none of
which exist yet. This is inherent to running the rewriter once per step and is
self-correcting — by step 13 every target exists, and re-running the rewriter
does not touch a link that already names its final target. It is noted here so
that a link that looks broken in an intermediate commit is not mistaken for an
error.
