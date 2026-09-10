# Working rules

Read once, apply to every step.

## 1. Ground rules

- **Pure move.** An entity's name, declaration, initialiser, body and
  documentation are carried over byte-for-byte. The only edits allowed are:
  the `USE` statements needed to keep names visible, the 20 constant renames
  from `constants_review.md`, FORD cross-reference targets that name a
  renamed module, and module header prose that must be redistributed when a
  module dissolves.
- **No opportunistic changes.** No reformatting, no `IMPLICIT NONE` additions
  to code you are not moving, no dead-code removal, no fixing of the issues the
  proposal lists under "Things found on the way". If you find a new one, write
  it in `deviations.md` and move on.
- **Numerically neutral.** Every literal keeps its exact text, including the
  default-real `1.2`, `0.659`, `1003.` and `2465000.` in `ETmod`. Do not add
  `d0`.
- **One module per file, file stem = module name**, lower case, `.f90`. New
  files are `.f90` even when the file they came from was `.F90` (no
  preprocessor is used: `preprocess: false` in `ford_project.md`, and nothing
  in the tree uses `#`-directives, `INCLUDE`, `COMMON` or `EQUIVALENCE` —
  verified).
- **Scope.** `src/visualisation/`, `src/resource/` and `src/Shetran.f90` are not
  reorganised; their `USE` lines are updated as the modules they import move.
- **Moved by script, not by hand.** Code and documentation blocks are extracted
  with `scripts/rename_extract.py` and cross-references are re-pointed with
  `scripts/rename_ford_links.py` (`00_tooling.md`). Both rename the file they
  are about to change to `<path>.backup` and build the new content by copying
  from that backup, so nothing is ever retyped. Hand editing is for what a
  script cannot decide: `USE` lines, header prose, `PUBLIC` lists.

## 2. The move tables

`docs/rename/functions.csv`, `variables.csv` and `types.csv` are the
authoritative mapping: every one of the 269 procedures, 1,091 variables and 4
types has a `target_file` and `target_module`. Between them they name the 106
target files. Where this plan and the CSVs disagree, the CSVs win — except for
the three open decisions in the README.

**The CSVs are a frozen snapshot of the pre-move tree.** `source_file`, `line`,
`starting_line` and `ending_line` refer to the tree as it was at the baseline
commit recorded in step 01. Do **not** re-run
`scripts/inventory_units.py`, `scripts/analyse_usage.py` or
`scripts/build_rename_proposal.py` during the migration; they would rewrite the
tables against a half-moved tree. If you need an original file after it has been
changed, use `git show <BASE>:<path>`.

`scripts/check_target_cycles.py` reads the CSVs *and* the current source files
by line range, so it is meaningful only at the baseline (step 01). After that,
the cycle check is the build itself: `cmake/FortranDependencies.cmake`
topologically sorts sources by their `USE` graph, so a circular dependency
either breaks the sort or fails to compile.

Useful columns:

| Column | Use |
|:-------|:----|
| `move_block_start_line` … `ending_line` | The exact span of a procedure, doc block included |
| `nested_in_parent` | `True` = a contained procedure; it travels inside its parent, never on its own |
| `line` (variables) | The declaration line |
| `proposed_rename` | Non-empty for the 20 renamed constants and `ZQTableRef` |
| `referenced_by` | Files that import this entity by name — **this is your consumer list** |
| `possibly_referenced_by` | Files that could see it only through a whole-module `USE`; check by hand |
| `documentation` | The FORD block as it stands, for checking nothing was dropped |

## 3. The scripts

All three live in `00_tooling.md`, with their full source and their usage:

| Script | Does |
|:-------|:-----|
| `scripts/rename_rows.py` | Read-only queries over the move tables |
| `scripts/rename_extract.py` | Moves entities out of a source file into their targets, via `.backup` |
| `scripts/rename_ford_links.py` | Re-points FORD `[[…]]` cross-references, via `.backup` |

Create them in step 01. The `.backup` rule, the one-generation-at-a-time
constraint and the end-of-step clean-up are described there.

## 4. What the extractor moves, and what it leaves you

`rename_extract.py` locates entities **by content**, not by the line numbers in
the CSVs, so a file that several steps take from (`AL_D`, `FRmod`, `ETmod`,
`SMmod`, `OCmod2`, `AL_C`, `mod_error`) can be processed again after it has
already shrunk. `--verify` confirms the locator reproduces the recorded line
numbers; on the untouched tree it matches all 1,330 top-level rows exactly.

It moves:

- **procedures and types** as the whole block from the top of the comment block
  above the code down to the matching `END`, so the FORD documentation travels
  with them; contained procedures travel inside their parent;
- **variables** as the declaration line plus a `!>` block above it and `!!`
  continuation lines below it. Every tracked declaration line declares exactly
  one tracked variable — one line in 1,091 declares two, and both go to the same
  module — so no declaration has to be split.

It leaves you:

- **the coverage report** — every line it did not move, printed with its line
  number: the module header, the `USE` block, `IMPLICIT NONE`, the
  `PUBLIC`/`PRIVATE` lists and the plain-comment section headers. Place all of
  it; see `00_ford.md` §1–2 for the documentation half.
- **the `USE` lines** (§7) and the `PUBLIC` lists.
- **visibility**: data modules are public by default today (`AL_C`, `AL_D`,
  `sglobal`, the `COLM_*` modules) and must stay that way. New *procedure*
  modules get `PRIVATE` plus an explicit `PUBLIC ::` list, following
  `src/util/tolerance_testing.f90`. Mirror the visibility each entity had.
- **`PARAMETER` operands**: a parameter written in terms of another parameter
  needs that operand visible in the target module.
- **deleting the emptied source** once only the module shell is left (§9).

Allocation and initialisation of a moved array travel with it — which is why
`initialise_al_c`/`_al_c2` go to `vs_state` and `initialise_al_c3` to
`et_state`.

## 5. When the target already exists

The eight state modules created in step 04 are extended by their component step.
For an existing target the extractor writes `<target>.append` with two labelled
blocks — declarations to paste before `CONTAINS`, procedures to paste inside it
— instead of overwriting the file. Delete the `.append` file once it is merged.

## 6. Finishing a new module

The extractor writes the skeleton — `summary:` filled in from the move tables,
everything else marked `TODO` — and the extracted blocks. What you add:

1. The FORD header: author line, two or three sentences of prose, and the
   `@history` row. Rules and the template are in `00_ford.md` §2; the model to
   follow is `src/util/tolerance_testing.f90`, the most recently written module
   in the tree.
2. The `USE … , ONLY:` lines (§7).
3. `PRIVATE` and an explicit `PUBLIC ::` list — procedure modules only.
4. The section comments and header paragraphs that the coverage report handed
   back (`00_ford.md` §1–2).

`grep -rn "TODO" src` must be empty before the step is committed.

## 7. `USE` lines

- `USE …, ONLY:` is required by `CODING.md` for everything except
  `mod_parameters`, which may be used bare.
- The consumer list for an entity is its `referenced_by` column. Work through
  it file by file; then check `possibly_referenced_by`, which lists files that
  reach the name only through a whole-module `USE` and may or may not use it.
- **Whole-module `USE` re-exports.** `USE AL_C` also re-exports what `AL_C`
  itself imported from `sglobal`; `CMmod` reaches the capacity parameters that
  way. When you change a data module's own imports, its bare consumers can lose
  names they never imported directly. The compiler catches this — expect it,
  do not paper over it by adding a bare `USE`.
- Bare `USE` statements to convert to explicit lists: 13 for `sglobal`, one
  each for `AL_C` and `AL_G` (`CMmod`), plus the ones listed per step. Build the
  list from the CSVs, compile, and add whatever the compiler still reports
  missing.
- Rename-on-import (`USE X, ONLY: a => b`) is used in the current tree; when the
  target of such an import moves, the local alias stays and only the module
  name changes.

## 8. FORD documentation

`00_ford.md` holds the rules in full: doc blocks travelling with their entity,
the header every new module needs, redistributing the header prose of a
dissolved module, the 281 qualified cross-references, and the per-step
checklist. In short:

```bash
python3 scripts/rename_ford_links.py --check src test   # what would change
python3 scripts/rename_ford_links.py src test           # rewrite, via .backup
```

287 links in 42 files rewrite automatically; 68 need a decision by hand. After
every step, sweep for names that no longer exist:

```bash
grep -rniE '\[\[(sglobal|al_c|al_d|al_g|frmod|rest|cmmod)[]:]' src test
grep -rniE '\bUSE +(SGLOBAL|AL_C|AL_D|AL_G)\b' src test
```

## 9. Retiring a source file

A source file is deleted when `python3 scripts/rename_rows.py --source <file>`
accounts for everything left in it and the file holds nothing but its module
shell. Use `git rm` so the deletion is recorded, and remove the directory if it
becomes empty (`src/core/state/` disappears in step 04).

Paths spelled out elsewhere must follow. The only ones outside `src/` are in
`test/CMakeLists.txt`:

| Line | Path | Step that must update it |
|:-----|:-----|:-------------------------|
| 42 | `src/core/mod_parameters.f90` | unchanged (module keeps its name and path) |
| 43 | `src/core/sglobal.f90` | 02 — replace with the `core/` modules `mod_error` needs |
| 44 | `src/util/mod_error.f90` | 03 — replace with `src/util/error/error_reporting.f90` and `error_status.f90` |
| 81 | `src/overland_channel/oc_row_width.f90` | 11 — see open decision 1 |

`CMakeLists.txt` itself needs no change: it discovers sources with
`file(GLOB_RECURSE …)` over `src/` and sorts them by their `USE` graph. Re-running
CMake (which `build.sh` does on every invocation) refreshes the glob.

## 10. Verifying a step

```bash
./build.sh -t Debug -c gfortran --clean-app --test   # configure, build, run the parser tests
( cd build/debug && ctest --output-on-failure )      # also runs oc_row_width.unit
```

Notes:

- `--clean-app` is important: it removes the stale `.mod` files that would
  otherwise let a module you just deleted still resolve. It does **not** clear
  the test targets' module directories — after a step that changes a
  test-compiled module, also
  `rm -rf build/debug/test/modules build/debug/test/modules_oc_row_width`.
- `build.sh --test` runs only `-R '^visualisation_read\.'`; the bare `ctest`
  above is what also exercises `oc_row_width.unit`.
- `--clean` rebuilds HDF5 and stdlib from source. Do not use it per step; use it
  once at the close-out.
- A `Release` build and, where the compiler is installed, an `ifx` build are
  part of the close-out, not of every step.

Then the sweeps from §8, plus, before committing:

```bash
grep -rn "TODO" src                       # no skeleton marker may survive
find src test -name '*.append'            # every append file merged and deleted
find src test -name '*.backup' -delete    # only once the build and tests pass
```

and:

```bash
git diff --stat            # a step that touches far more than its file list is a warning sign
python3 scripts/rename_rows.py --target-dir src/<dir>   # every row accounted for?
```

**Per the recorded testing scope, verification stops at compile plus the parser
tests.** The example simulations under `examples/` are run manually by the
maintainer; do not run them as part of a step, but say in the commit message
which step is ready for that check.

## 11. Committing

One commit per step, on the working branch chosen in step 01. Message shape,
following the existing history:

```
reorg step NN: <what moved>

<what came from where, one line per retired module>
Pure move: no behaviour change. See docs/rename/plan/NN_<name>.md.
```

No `*.backup` or `*.append` file is ever committed; both are cleared before the
commit. Update the status table in `README.md` in the same commit. `CHANGELOG.md` gets
one entry for the whole reorganisation, at the close-out, not per step.

## 12. When something does not fit

Stop, write the case in `deviations.md` (what the plan said, what the code
actually does, what you did, what it costs), and take the smallest option that
keeps the move mechanical. Do not resolve a modelling question — the duplicated
plant state, the disagreeing physical constants and the double `ZQTableRef` are
all recorded as open on purpose.
