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

## D9 — `datetime` depends on `linear_algebra`, for a message buffer

`variables.csv` places `utilsmod`'s two module variables, `eps` and `msg`, in
`linear_algebra`. `eps` belongs there — it is the singularity tolerance the
matrix inversion reads. `msg` is a shared `CHARACTER(128)` error-message
buffer, and the only routine outside `linear_algebra` that writes into it is
`hour_from_date`, which goes to `datetime`.

The mechanical consequence is that [[datetime]] imports from
[[linear_algebra]], and that `msg` — private in `utilsmod`, where both routines
lived — has to become **public**. Both are recorded in `linear_algebra`'s
header.

The alternative would be to give `datetime` its own local buffer, which is a
behaviour-preserving but non-mechanical change (the buffer is written and then
passed straight to `RAISE_ERROR`, so nothing reads it across the call). Left as
a follow-up; the point of the move is that the oddity is now visible in a
`USE` line instead of hidden inside a 1,900-line module.

Every `MSG` in the former `mod_load_filedata` is a *local* declaration, so none
of `record_readers`, `spatial_fields` or `input_validation` imports it.

## D10 — a fifth script: `scripts/rename_imports.py`

Same reasoning as D4, for the other half of the `USE` work. `rename_extract.py`
leaves a `! TODO USE ..., ONLY: ...` marker in each new module; working out what
to put there means intersecting what the source module could see with what the
extracted block actually references. `scripts/rename_imports.py` drafts that,
from a deliberately narrow candidate set (the source's own imports, followed
through the tables, plus the sibling modules of the same split), and it skips
any name the target declares itself.

That last filter is what makes it usable: `ran2`'s dummy argument `idum` is not
[[input_workspace]]'s `IDUM`, and `ALALLF` takes `NELEE`, `ICMXY`, `ICMBK`,
`IDUM` and `DUMMY` as dummy arguments rather than importing them. It also
refuses to follow the tables to a module that does not exist yet, so a step-03
module correctly imports `icmbk` from `AL_C` rather than from the
`channel_geometry` that step 04 will create.

It is a draft, not an oracle, and the compiler is still the check. Its one known
blind spot is scope: the filter is file-wide, so a name declared locally in one
procedure is dropped for the whole file. That happened once — `jematmul_mm`
declares a local `ZERO`, which hid `linear_algebra`'s need for
`mod_parameters`'s `zero` — and the build caught it immediately.

## D11 — `initialise_al_c` allocates an array that belongs to `et_state`

*What the plan says.* `04_component_state.md`, hazards: "**Allocation.** The
initialisers allocate the arrays they now sit beside; make sure every array
they touch really moved to the same module, or the initialiser will need a
`USE` back into a module that `USE`s it — a cycle. `initialise_al_c`/`_al_c2` →
`vs_state`, `initialise_al_c3` → `et_state` are the placements the proposal
checked."

*What the tree does.* One array does not follow its initializer.
`initialise_al_c` allocates and zeroes eight arrays, of which seven are
`vs_state`'s; the eighth is `ERUZ`, which `variables.csv` places in `et_state`
because it is the root-extraction sink the ET solver writes.

*What was done.* `vs_state` imports `USE et_state, ONLY: ERUZ`. It is not a
cycle: `et_state` needs nothing from `vs_state`, so the edge runs one way and
the dependency sort is unaffected. Both modules' headers record it — `et_state`
says `ERUZ` is allocated elsewhere, `vs_state` says it allocates an array it
does not own.

*Cost.* A component state module depends on another component's state module,
which the plan's "state modules are leaves" intent did not anticipate. Moving
the `ERUZ` allocation into `initialise_al_c3` would remove the edge and is a
one-line change, but it alters *when* the array is allocated relative to the
other seven, so it is not a pure move and is left as a follow-up.

## D12 — `--from-source` added to `rename_use_lines.py`

`sy_state` is a target of both step 04 (`SBERR`, from `AL_C`) and step 06
(20 variables, from `SED_CS`). Selecting it with `--target` alone therefore
re-pointed `USE SED_CS` statements in step 04, three steps before those
variables actually move — which compiles only by accident, or more likely not
at all.

`--from-source` restricts the remapping to entities that left a named source
file, matching `rename_extract.py`'s own filtering. Every step that touches a
target shared with another step must pass it. The wrongly rewritten statements
were restored from the run's `.backup` files before the step continued.

## D13 — two names had to become public when their module split

A procedure or variable that was private inside a large module becomes an
interface as soon as the split puts its caller in a sibling module. Two cases
so far, both recorded in the header of the module that now exports the name:

| Name | Was | Now | Why |
|:-----|:----|:----|:----|
| `msg` | private in `utilsmod` | public in [[linear_algebra]] | [[datetime]] writes into it (D9) |
| `MNPLANTINITIALISE` | private in `MNmod` | public in [[mn_plant]] | [[mn_driver:MNINITIALISE]] calls it |

`mn_state`'s 57 variables and three derived types are the same case in bulk:
`MNmod` declared `PRIVATE`, so all of them were private, and all eight sibling
modules read them. `mn_state` follows the data-module convention and is public
by default.

This is the one visibility change the move cannot avoid, and it is worth
noticing: each of these names is now part of a module's published interface
because of where the boundary was drawn, not because anything decided it should
be.

## D14 — the example-model check, and the stale fixtures that hid it

The maintainer allowed the example simulations to be run in this session, so the
numerical invariant was checked directly rather than deferred to the close-out.

**Result after step 04 (and re-confirmed by comparison after step 05): all 13
runnable models report `any_differences = False`.** Two files are reported as
`too large to compare` — `Aire_at_Kildwick_Bridge-AllOptions`'s `spr.txt` and
`Slapton-3D`'s `shegraph.h5` — which is a size threshold in
`examples/_methods/settings.py`, not a difference.

Getting to that answer needed `examples/*/output_should/` cleaned first.
`setup_results_check.py` copies `output_*` files in and never removes any, so
every expected-result directory still held fixtures from 2026-04-16 whose
filenames the model no longer writes:

| Stale fixture | Superseded by |
|:--------------|:--------------|
| `output_<c>_log.txt` (13 models) | `info_<c>_SHETRAN_log.txt` |
| `Slapton/output_slap_mb.txt` | `output_slap_mb.csv` |
| `Aire…-simple/output_…_mb.csv` | `output_…_mass_balance.csv` |

The comparison counts a fixture with no counterpart as `status: missing`, which
sets `data_differs`, which sets `any_differences` — so **every** model reported
a difference before and after the reorganisation began, for reasons that have
nothing to do with it. That is why the `comparison_overview.csv` committed
before this work also shows `True` for all 13 rows.

`output_should/` is untracked generated data, so the stale files were deleted
and the comparison re-run. Worth fixing at source: `setup_results_check.py`
should clear the directory before copying, otherwise a renamed output file
silently poisons the check again.

**Collateral, and not asked for:** the blanket delete also removed the
expected-result fixtures of `Cobres-ExtraOutputDischargePoints`,
`Cobres-ExtraOutputWaterTable` and `dano100m`. All three have no `model/`
input directory in the repository and appear in no active list in
`settings.py`, so neither script could run or compare them, and the fixtures
could not have been regenerated. They were untracked, so the deletion is not
recoverable from git.

## D15 — `SYFINE`'s first-call state is in a sibling module

`constants_review.md` lists `FIRST_syackw`, `FIRST_syfine` and `WSED_syfine`
under "Constants deliberately *not* moved", to stay "with their formulas" in
`sy_transport_capacity`, and `variables.csv` places them there.

But `WSED_syfine`'s formula is not in `sy_transport_capacity`: it is the two
lines inside `SYFINE` that cache the fine-sediment settling velocity on the
first call, and `SYFINE` goes to `sy_hillslope`. `FIRST_syfine` is the guard on
those same two lines. `FIRST_syackw` is genuinely dead — its own note in the
source says so — and its placement costs nothing.

*What was done.* The tables win (`00_working_rules.md` §2), so both names stay
in `sy_transport_capacity`, are declared **public** there, and `sy_hillslope`
imports them. Both module headers say so.

*Cost.* A module's private first-call state became part of another module's
published interface, and reading `SYFINE` now requires looking somewhere else
for the two variables it owns. Moving them to `sy_hillslope` would be a
one-line change with no behaviour effect and would remove the edge; it is a
follow-up, not part of a move that follows the tables.

## D16 — `ETmod`'s three "AD only" imports are dropped

`ETmod` carried

```fortran
USE SMmod, ONLY: SMIN, &
                 smelt, tmelt !THESE NEEDED ONLY FOR AD
!NEEDED ONLY FOR AD
USE SMmod, ONLY: rhos
```

`SMIN` is called by `ETIN` and follows it to [[et_process]]. `smelt`, `tmelt`
and `rhos` appear nowhere in any of `ETmod`'s bodies — they were imported so
that an automatic-differentiation build could see them through this module.
No target references them, so the imports went with `ETmod`'s shell rather
than being carried into a module that does not use them.

If the AD build depends on those names being visible through the ET component,
this is the change that would break it. Nothing in `src/` or `test/` reads
them that way today, and `run_sim` imports `smelt`/`tmelt` from
[[snow_state]] and `rhos` from [[snow_config]] directly.

The same pattern survives elsewhere and was *not* touched: `FRmod`'s
`PUBLIC` list still carries a `!REST NEEDED FOR AD ONLY` comment over a dozen
names, and [[et_config]] still exports `psi4`/`uzalfa` for the same reason.

## D17 — the import drafter cannot tell two `msg`s apart

`scripts/rename_imports.py` keys its ownership map on the lowercased entity
name, so where the same name is a module variable in more than one source it
reports whichever row it read last. Three modules declare a `msg`:

| Source | Target | Kind |
|:-------|:-------|:-----|
| `utilsmod` | `linear_algebra` | `CHARACTER(128)` |
| `ETmod` | `et_config` | `CHARACTER(132)` |
| `FRmod` | `run_control` | `CHARACTER(256)` |

In step 07 the drafter offered `et_process` a `USE linear_algebra, ONLY: msg`,
which would have compiled and would have written the ET diagnostics into the
matrix-inversion module's buffer. `et_config`'s `msg` is the right one, and is
what `et_process` imports.

Checked for the remaining steps: `eps` (three sources) and `temp` are the other
colliding names, and no step so far has needed either across a module boundary.
Every draft the script produces is read against `rename_rows.py --target`
before it is used.

## D18 — the drafter checked the module, not the entity

`scripts/rename_imports.py` refused to follow the move tables to a target
module that did not exist yet (D10), which is not the same test as the one that
matters. `oc_state` has existed since step 04, but it does not hold `OCmod2`'s
`HRFZZ` until step 11 — so in step 09 the drafter offered `cm_column`
`USE oc_state, ONLY: hrf => HRFZZ`, which cannot compile.

`existing_modules()` now returns, for each module in the tree, the set of names
it actually declares, and a candidate is only re-pointed if the target owns the
name **now**. `cm_column` correctly imports `hrf => hrfzz` from `OCmod2` until
step 11 moves it.

## D19 — `cm_input`'s imports had to be worked out per procedure

The file-wide scope approximation in `rename_imports.py` (noted in D10) fails
badly on `cm_input`, which holds `CMRD` from `CMmod` and `INCM`/`MUERR2` from
`FRmod`. Between them those readers declare hundreds of locals and dummy
arguments, and `declared_locally` — which does not know which procedure a
declaration is in — suppressed roughly 60 genuine module-level imports,
including `ALPHA`, `CCAPI`, `ICMREF` and `NCOLMB`.

The block was rebuilt by scanning each of the three procedures separately, with
its own locals and its own procedure-level `USE` statements excluded. The two
readers also disagree about names: `CMRD` uses `CMmod`'s rename-on-import
aliases `NEL`/`nlf`, while `INCM` uses the plain `total_no_elements` and
`total_no_links`. Both forms are needed, in different modules —
[[cm_driver]] keeps the aliases and `cm_input` the plain names.

Making the drafter scope-aware would be the real fix. It is a helper, not part
of the deliverable, and the compiler catches every case it gets wrong.

## D20 — two more deletions forced by the split

- **`USE cm_input, ONLY: CMRD` inside `INCM`.** `INCM` came from `FRmod` and
  imported `CMRD` from `CMmod`; the `USE` rewriter re-pointed it at `cm_input`,
  which is now `INCM`'s *own* module. A module cannot `USE` itself, so the
  statement goes: `CMRD` is visible by host association.
- **`! USE CONT_CC ! (Duplicate removed)`** in what is now `cm_channel`, a
  commented-out import of a module that no longer exists under that name.

## D21 — `PARAMETER(NSOLEE=200)` did not travel with its declaration

`vs_soil_tables` is sized by `NSOLEE`, which `VSmod` declared in the old
two-statement form:

```fortran
   INTEGER NSOLEE
   PARAMETER(NSOLEE=200)
```

`rename_extract.py` moves the *declaration* line of a tracked variable. The
separate `PARAMETER` statement is not a declaration of anything the move tables
list, so it stayed in `VSmod`'s residual and would have been deleted with it,
leaving `NSOLEE` an uninitialised integer used as an array bound in seven
declarations — which does not compile, so it could not have escaped notice, but
it is the one case so far where the extractor silently left a moved entity
incomplete.

It is carried into `vs_soil_tables` immediately after the declaration. Checked
for others: this is the only old-style `PARAMETER(...)` statement left in any
residual across the ten steps done so far.

## D22 — `errcntallowed` is read by two modules, not one

`constants_review.md` places `errcntallowed` in `vs_column_solver` because it
is "read only by `VSCOLM`". It is not: `vs_driver`'s `VSSIM` reads it too,
twice, in the same convergence-warning pattern.

The placement is still right — it is `VSCOLM`'s limit and putting it in
`vs_config` would make `vs_driver` and `vs_column_solver` mutually dependent,
which is the cycle the note exists to avoid. But it has to be `PUBLIC` in
`vs_column_solver` rather than private, and `vs_driver` imports it.

## D23 — a `.append` block has to go before `CONTAINS`

`rename_extract.py` writes `<target>.append` with the declarations and
procedures to paste in, and the step instructions say to merge it by hand.
`sy_state` and `snow_state` have no `CONTAINS`, so appending before
`END MODULE` was correct there; `vs_state` has one (the two `initialise_al_c*`
routines), and the same mechanical merge put three declarations inside the
`CONTAINS` section. gfortran rejects it outright
(`Unexpected data declaration statement in CONTAINS section`). Declarations go
before `CONTAINS`; only the procedure half of an `.append` goes after it.

## D24 — open decision 1 resolved: `oc_row_width` stays as it is

`README.md` open decision 1 offered two ways to handle `MAX_ACTIVE_ROW_WIDTH`,
which `functions.csv` places in `oc_indexing`. The **recommended** option was
taken: the function stays in `src/overland_channel/oc_row_width.f90` as its own
dependency-free leaf module, and `oc_indexing` takes
`USE OC_ROW_WIDTH, ONLY: MAX_ACTIVE_ROW_WIDTH`.

`test/CMakeLists.txt` needs no change, and `oc_row_width.unit` still builds and
passes from that one source file plus the assertion support — which is the
whole point of the module, and what folding it into `oc_indexing` would have
cost. `overland_channel/` therefore has 15 modules rather than 14, and this is
the one deliberate departure from `functions.csv`.

The test's own four cross-references were re-pointed by the rewriter as the
plan predicted: `[[ocmod:ocsim]]` became `[[oc_driver:OCSIM]]` and the three
`[[ocmod:ocind]]` became `[[oc_indexing:OCIND]]`.

## D25 — `run_control` had to be created in step 11, not step 12

`INBK` moves out of `FRmod` into `bank_setup` in step 11, and its first
statement reads the bank file's title into `TITLE` — which `variables.csv`
places in `run_control`, a step-12 target. Leaving `TITLE` in `FRmod` for one
step would make `FRmod` and `bank_setup` `USE` each other, which Fortran
forbids.

So `run_control` was extracted in step 11, with all 24 of its rows (16 from
`AL_D`, 8 from `FRmod`), and `FRmod` now imports back the eight names it gave
up. This is step 12's own first sub-step — "`run_control` — data only, no
procedures. Everything else may read it" — brought forward, not a change of
plan; `12_frame.md` has one fewer file to create.

The same reasoning applies to the four other `IN*` routines that left `FRmod`
in steps 07--09, and it is why they needed no equivalent: `INET`, `INSM`,
`INCM` and `INPL` read no `FRmod` module variable.

## D26 — `rm -rf` of a test module directory needs a CMake re-configure

`00_working_rules.md` §10 says that after a step that changes a test-compiled
module, `rm -rf build/debug/test/modules build/debug/test/modules_oc_row_width`.
Those directories are created by `file(MAKE_DIRECTORY ...)` at *configure*
time, and `build.sh`'s re-configure does not recreate them once CMake decides
nothing has changed. The next build of `oc_row_width_tests` then fails with

```
Error copying Fortran module "test/modules_oc_row_width/oc_row_width.mod".
```

which looks like a source problem and is not. Removing
`build/debug/test/CMakeFiles/oc_row_width_tests.dir` and re-running
`cmake -S . -B build/debug` fixes it. Worth adding to §10, or dropping the
`rm -rf` in favour of `--clean-app`, which does not need it.

## D27 — `--target-dir` selects rows that an earlier step already moved

Running `rename_extract.py --target-dir src/frame` in step 12 fails with

```
cannot locate BHOTTI in src/core/state/AL_D.f90
```

because `run_control` is under `src/frame/` and its 24 rows left in step 11
(D25). The extractor locates entities by content, finds nothing, and stops —
which is the right behaviour, but the fix is to name the step's five targets
explicitly with `--target` instead of the directory.

The same trap applies to any step whose directory contains a module completed
earlier: it is the extraction-side counterpart of the `--from-source` filter
D12 added to `rename_use_lines.py`. Five of the eight remaining `.backup` files
had already been written when the script stopped, so the run also has to be
cleaned up (`find src test -name '*.backup' -delete` plus removing the
part-written targets) before retrying.

## D28 — `FRmod` re-exported three names it did not own

`FRmod`'s `PUBLIC` list carried `DATE_FROM_HOUR`, `bsoft`, `tsh` and `tch`,
which it imported from `utilsmod` and its own state and re-exported "for AD
only". Once `FRmod` is gone the consumers have to name the real owner:
`run_sim` now takes `DATE_FROM_HOUR` from [[datetime]], and `bsoft`/`tsh`/`tch`
from [[run_control]] (the `USE` rewriter did those two, because they are
tracked rows; `DATE_FROM_HOUR` is not a `FRmod` row at all, so it needed the
hand fix).

Worth noticing because the re-export was invisible in the move tables: an
entity that a dissolved module merely passed through has no row, so nothing
flags its consumers.

## D29 — the tree *does* use preprocessor directives, and the guards were split

`00_working_rules.md` §1 states that new files are `.f90` even when the source
was `.F90` because "no preprocessor is used: `preprocess: false` in
`ford_project.md`, and nothing in the tree uses `#`-directives, `INCLUDE`,
`COMMON` or `EQUIVALENCE` — verified."

That is true of FORD and false of the compiler. `src/driver/getdirqq.f90` has
seven `#ifdef SHETRAN_HAVE_QUICKWIN` blocks guarding the Windows QuickWin file
dialog, `CMakeLists.txt:819` passes `-cpp`, and `SHETRAN_ENABLE_QUICKWIN`
defines the macro.

`rename_extract.py` moves an entity's own span, so guards *inside* a moved
procedure travelled correctly — but two guards that *enclosed* moved entities
did not:

| Guarded entity | What was left behind |
|:---------------|:---------------------|
| `USE IFWIN` | needed by `command_line`'s `T_OPENFILENAME` use |
| `FileName` declaration | its `#ifdef`/`#endif` pair |
| `comdlger` (whole routine) | its `#ifdef`/`#endif` pair |

Without the third, a QuickWin build would compile `comdlger` unconditionally
and a Linux build would fail on `CommDlgExtendedError` and the `CDERR_*`
constants — which is how it was found. All three were restored by hand.

*Verified*, since the Windows path cannot be compiled here: `cpp -P
-traditional` over the original `getdirqq.f90` and the new `command_line.f90`,
both with and without `-DSHETRAN_HAVE_QUICKWIN`, produces **byte-identical**
procedure bodies (403 and 278 lines respectively); the only difference is the
`END MODULE` name. The guard structure is the same seven blocks in the same
order, including the one `#else`.

Any later step that moves code out of a file containing `#` directives must
check the enclosing guards explicitly; the extractor does not see them.

## D30 — two modules still have no author line

The close-out asks that every new module have `summary:`, an author line, prose
and an `@history` row. 105 of the 107 do. The two that do not:

| Module | Why |
|:-------|:----|
| `cm_parameters` (was `CONT_CC`) | never had one; a 1:1 `git mv` rename with contents unchanged |
| `oc_row_width` | never had one; untouched by this work (D24) |

Both gaps predate this work — `git show <BASE>:src/contaminant/CONT_CC.F90`
and the baseline `oc_row_width.f90` have no `!> author:` line either. Neither
file's contents were rewritten here, so there is nothing this move could
attribute. `cm_parameters` does carry an `@history` table naming JE for the
original 1991 implementation, which is the available provenance; inventing an
author line from it would be a guess, and the `@history` table already records
what is known.

## D31 — the stack-size warning count moved, for a reason that is not a change

The clean Debug build has 1,595 warnings against the baseline's 1,610, with no
new *kinds*. Three counts moved:

| Warning | Baseline | Now | Why |
|:--------|---------:|----:|:----|
| `USE statement ... has no ONLY qualifier` | 79 | 53 | the 26 bare `USE`s replaced by explicit lists |
| `Unused PRIVATE module variable` | 10 | 4 | six were private-and-unused in a module that is now a public data module |
| `Array 'X' is larger than limit set by -fmax-stack-var-size` | 160 | 177 | see below |

The third needed checking. gfortran emits that warning **once per `USE` site**
that brings a large module array into scope, not once per array: the log shows
it pointing at `USE sy_state, ONLY: DLS, GNU, ...` in
`visualisation_interface_left.f90` as well as at the declaration in
`sy_state.f90`. Splitting `SYmod` and `CMmod` multiplied the number of modules
importing those arrays, so the same declarations are reported more often —
`dls` 5 times to 7, while `pmass` and `pf2max` went 3 to 2 as `FRmod`'s single
import became none.

The storage class of a module variable is static either way, so nothing about
code generation changed. It is recorded because a warning count that moves in a
"pure move" is worth explaining rather than waving through.

## D32 — one link the rewriter broke, found by a substitute for FORD

FORD is not installed (D3), so the generated `docs/ford/` pages could not be
inspected as `14_closeout.md` §4 asks. Instead every `[[module:entity]]`
cross-reference in `src/` and `test/` was resolved against the modules that
now exist and the names they declare.

That found exactly one break, in `oc_indexing`'s own header:
`[[oc_indexing:MAX_ACTIVE_ROW_WIDTH]]`. The rewriter followed
`functions.csv`, which places that function in `oc_indexing`, but D24 keeps it
in `oc_row_width` — so the link named a module that does not contain it. It is
now `[[oc_row_width:MAX_ACTIVE_ROW_WIDTH]]`.

Final state: 1,359 links, 119 modules, **0 unresolved qualified links**. Bare
`[[entity]]` links are not checked, because FORD resolves those by name across
the whole project and they are unaffected by the move.

## Verification: the move is provably pure

Beyond the per-step builds and sweeps, the whole reorganisation was checked
against the baseline commit by comparing *content* rather than files.

Every `src/**/*.[fF]90` file at `f31d1b8` and at the close-out was reduced to
its logical code lines — continuations joined, comments stripped, whitespace
normalised, lowercased, the 20 constant renames and `ZQTableRef` ->
`ZQTableRefRead` applied, and `USE` / `MODULE` / `END MODULE` /
`IMPLICIT NONE` / `PRIVATE` / `PUBLIC` / `CONTAINS` / `#` lines excluded,
those being precisely what the move is allowed to change. The two multisets
were then compared:

```
baseline 22914 logical code lines -> final 22914
lost:   0
gained: 0
```

Every declaration and every executable statement in the pre-reorganisation
tree is present in the new one, exactly once, unchanged. Nothing was dropped,
duplicated or rewritten. That is the invariant `README.md` states, checked
directly rather than inferred from the build succeeding.

The check is worth keeping for any future move of this kind; it is what caught
nothing here, and would have caught the `PARAMETER(NSOLEE=200)` of D21 had the
compiler not.

## D33 — the `ZQTableRef` rename changed a string literal, and the output with it

**This is the one real defect the reorganisation introduced, and the example
models caught it.**

`variables.csv` renames `ZQmod`'s `ZQTableRef` to `ZQTableRefRead`, to keep it
apart from `AL_D`'s same-named variable (step 11). The rename was applied with
a word-boundary regex over the moved block — which also rewrote the name where
it appears inside a **string literal**:

```fortran
! baseline
IF (ios == 0) WRITE (fid_ZQ_log, *, ...) 'ZQTableRef   =', ZQTableRef
! after the rename, wrongly
IF (ios == 0) WRITE (fid_ZQ_log, *, ...) 'ZQTableRefRead   =', ZQTableRefRead
```

`reservoir-ZQmodule-example` writes that label three times into
`output_readZQTable.txt`, so its output was 12 bytes longer than the reference
and the file compared unequal. Every other model was unaffected, and no build
or sweep could have found it: the code is valid either way.

Fixed by restoring the literal; the variable keeps its new name, which is what
the move tables ask for. Re-running the model gives
`any_differences = False`.

*Audited for others.* Five string literals in the whole baseline tree contain
a name this work renames:

| Literal | State |
|:--------|:------|
| `'ZQTableRef   ='` (`ZQmod`) | was wrongly rewritten; **fixed** |
| `"CONT_CC:initialise_cont_cc"` | untouched |
| `"COLM_CG:initialise_colm_cg"` | untouched |
| `"COLM_CG:deallocate_colm_cg"` | untouched |
| `"COLM_CO:initialise_colm_co"` | untouched |

The four `location` strings survived because the 14 data-module renames matched
only `^\s*USE\s+NAME` and `^\s*(END )?MODULE\s+NAME`, never free text. They now
name modules that no longer exist, which is stale but correct for a pure move:
they are passed to `errstat_alloc` and would appear in a diagnostic, so
changing them *would* change observable output. Updating them is a follow-up,
listed below.

*Lesson.* A rename applied by regex must exclude string literals, or be checked
against them afterwards. The content-equivalence check of the previous section
cannot see this: it compares code lines, and both versions are code.
