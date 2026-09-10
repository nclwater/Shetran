# Step 14 — Close-out

**Goal.** Prove the reorganisation is complete and consistent, and hand the
numerical check to the maintainer.

No entity moves in this step. If a sweep below finds something out of place,
fix it as part of the step that owned it and say so in the commit message.

## 1. Completeness

```bash
# every target file in the move tables exists, and nothing else does
python3 - <<'PY'
import csv, pathlib
targets = set()
for t in ("functions.csv", "variables.csv", "types.csv"):
    for row in csv.DictReader(open(f"docs/rename/{t}")):
        targets.add(row["target_file"])
on_disk = {str(p) for p in pathlib.Path("src").rglob("*.[fF]90")}
skip = lambda p: p.startswith(("src/visualisation/", "src/resource/")) or p == "src/Shetran.f90"
print("planned but missing:", sorted(t for t in targets if t not in on_disk))
print("present but unplanned:", sorted(p for p in on_disk if p not in targets and not skip(p)))
PY
```

Expected: both lists empty, apart from any deviation recorded in
`deviations.md` (for example `oc_row_width.f90`, if open decision 1 was taken
as recommended).

## 2. No retired name survives

```bash
grep -rniE '\bUSE +(SGLOBAL|AL_C|AL_D|AL_G|CONST_SY|SED_CS|MOD_ERROR|TOLERANCE_TESTING|UTILSMOD|MOD_LOAD_FILEDATA|FRMOD|REST|RUN_SIM|GETDIRQQ|ETMOD|SMMOD|VSMOD|SYMOD|MNMOD|CMMOD|OCMOD2?|OCQDQMOD|ZQMOD|CONT_CC|COLM_[A-Z0-9]+|LINK_[A-Z0-9]+|BK_CW|SED_CO|IS_CC|PLANT_CC)\b' src test
grep -rniE '\[\[(sglobal|al_c|al_d|al_g|const_sy|sed_cs|mod_error|tolerance_testing|utilsmod|mod_load_filedata|frmod|rest|run_sim|getdirqq|etmod|smmod|vsmod|symod|mnmod|cmmod|ocmod2?|ocqdqmod|zqmod|cont_cc|colm_[a-z0-9]+|link_[a-z0-9]+|bk_cw|sed_co|is_cc|plant_cc)[]:]' src test
grep -rnwE '(GRAVTY|RHOSED|RHOWAT|VISCOS|LAMDA|RHOA|RHOW|CPA|CPW|CPI|LWI|LVW|HFG|ROOT2G|F23|F53)' src test
```

All three must be empty. The third is a documentation check as much as a code
check: it catches doc blocks that still mention `` `RHOA` `` in prose, which the
link rewriter does not touch.

```bash
python3 scripts/rename_ford_links.py --check src test
```

must report no rewrite and no `MANUAL` line other than the pre-existing dead
links recorded in `deviations.md`.

## 3. Structure

```bash
find src -name '*.[fF]90' | wc -l        # 106 + 12 visualisation + Shetran.f90
find src -type d -empty                  # must be empty output
find src test -name '*.backup' -o -name '*.append'   # must be empty
grep -rn "TODO" src                      # no skeleton marker survived
git log --oneline --diff-filter=R -- src | head -40   # renames recorded
```

Every new module must have a header with `summary:`, an author line, prose and
an `@history` row recording the split. Check one file per directory by eye, and
confirm that the header prose of each dissolved module — `AL_C`, `AL_D`,
`sglobal`, `FRmod`, `CMmod`, `SED_CS` and the rest — is present somewhere, not
lost with the file.

## 4. Documentation

```bash
python3 scripts/audit_ford_docs.py > /tmp/ford_audit_final.txt
diff /tmp/ford_audit_baseline.txt /tmp/ford_audit_final.txt   # no new findings
./build.sh --docs-only                                        # FORD must build cleanly
diff /tmp/ford_baseline.txt <(./build.sh --docs-only 2>&1)     # no new FORD warnings
```

FORD's `[[…]]` links are the part most likely to have rotted, and the generated
`docs/ford/` output is where a broken one shows up. Open the pages for
`mod_parameters`, `vs_state`, `oc_state` and `frame_setup`: those four are the
most linked-to of the new modules, so a link that failed to resolve is most
likely to be visible there.

The rules that produced this state are in `00_ford.md`; if something is wrong,
fix it in the step that owned it rather than patching it here.

## 5. Builds

```bash
./build.sh -t Debug   -c gfortran --clean --test
( cd build/debug && ctest --output-on-failure )
./build.sh -t Release -c gfortran --clean-app
./build.sh -t Debug   -c ifx --clean-app --test     # if oneAPI is installed
```

Compare the warning output against the step-01 baseline. New warnings are
almost always a lost `ONLY` or a moved declaration whose kind changed.

## 6. Changelog and status

- One `CHANGELOG.md` entry for the whole reorganisation: the 40 files in scope
  become 106 named modules, `sglobal`/`AL_C`/`AL_D`/`AL_G` are dissolved, the
  physical constants are gathered in `mod_parameters` under distinct names, and
  **no behaviour changes**. Point at `docs/rename/proposal.md` and
  `docs/rename/constants_review.md`.
- Tick every row of the status table in `README.md`.
- Make sure `deviations.md` is complete: it is the only record of where the
  implementation and the move tables differ.

## 7. Hand over for the numerical check

Per the recorded testing scope, the example simulations are run by the
maintainer, not by the agent. Say so explicitly in the final commit message, and
list what to compare:

- `examples/` run set, output CSVs and runtimes, against the pre-reorganisation
  reference from step 01;
- expected result: **bit-identical output**. Any difference is a bug in the move,
  not an improvement — the most likely causes are a constant renamed to the
  wrong one of a disagreeing pair (`RHO_AIR_ET` vs `RHO_AIR_SNOW`,
  `RHO_WATER_SEDIMENT` vs `RHO_WATER_SNOW`, `L_VAPORISATION_ET` vs
  `L_VAPORISATION_SNOW`), or a variable that ended up in a different module from
  the one its writer updates.

## What is deliberately still open after this work

- The disagreeing physical constants (`constants_review.md`) — kept apart under
  distinct names.
- The duplicated plant state between `cm_plant_state` and `mn_state`
  (`docs/todo/issue_plant_declarations.md`).
- `ZQTableRef` / `ZQTableRefRead` — whether they are one quantity.
- The 26 orphan variables in `core/legacy_retained.f90`.
- `DOCIN`, which has no caller.
- `input_workspace`'s `IDUM` and `DUMMY`, which should become locals.
- The `initialise_al_c*` names, which outlive the module they refer to.

Each is a small, independent follow-up commit.
