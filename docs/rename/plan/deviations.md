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
