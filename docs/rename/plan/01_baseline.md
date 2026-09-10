# Step 01 — Baseline and tooling

**Goal.** Establish the reference point everything else is checked against, and
create the one helper script the later steps use. No source file changes.

## Do

1. **Branch.** Work on a dedicated branch off the current head:
   `git switch -c reorg_modules`. Commit or stash unrelated work first; the
   untracked `docs/` and `scripts/` files already in the tree can stay.
2. **Record the baseline commit.** Put its hash at the top of
   `docs/rename/plan/deviations.md` (create the file with just that header).
   Every `git show <BASE>:<path>` in later steps refers to it, because the move
   tables' line numbers are only valid there.
3. **Baseline build**, from a clean tree, both compilers if available:

   ```bash
   ./build.sh -t Debug   -c gfortran --clean --test
   ( cd build/debug   && ctest --output-on-failure )
   ./build.sh -t Release -c gfortran
   ```
   Record the number of tests and any pre-existing warnings you intend to
   ignore. A warning that appears later and is not in this list is yours.
4. **Baseline cycle check** — the last time it is meaningful (see
   `00_working_rules.md` §2):

   ```bash
   python3 scripts/check_target_cycles.py     # must print "No cycles ..." and exit 0
   python3 scripts/call_graph.py > /tmp/call_graph_baseline.txt
   ```
   If this does **not** exit 0, stop: the proposed placement no longer matches
   the tree and the plan needs revisiting before any file moves.
5. **Baseline FORD state.** The documentation is checked the same way the code
   is, so record where it starts:

   ```bash
   command -v ford || echo "FORD not installed - the link sweeps are the only check"
   ./build.sh --docs-only 2>&1 | tee /tmp/ford_baseline.txt   # if ford is present
   python3 scripts/audit_ford_docs.py > /tmp/ford_audit_baseline.txt
   ```
   A move must not add to the audit output; pre-existing findings are not fixed
   here.
6. **Create the three scripts** from `00_tooling.md` — `scripts/rename_rows.py`,
   `scripts/rename_extract.py`, `scripts/rename_ford_links.py` — and check them
   against the numbers they are known to produce on the untouched tree:

   ```bash
   python3 scripts/rename_rows.py             2>&1 >/dev/null   # 1364 rows
   python3 scripts/rename_rows.py --target-dir src/core 2>&1 >/dev/null   # 199 rows

   # the extractor's content locator must reproduce every recorded line number
   python3 scripts/rename_extract.py --verify \
       $(python3 -c "import csv,sys
       seen=set()
       for t in ('functions','variables','types'):
           for r in csv.DictReader(open(f'docs/rename/{t}.csv')):
               seen.add(r['source_file'])
       print(' '.join('--source ' + s for s in sorted(seen)))")
   # expected: 0 mismatch(es)

   python3 scripts/rename_ford_links.py --check src test
   # expected: 42 files would change, 68 links need a decision
   ```
   Commit the three scripts. If `--verify` reports a mismatch, the tree has
   moved away from the move tables and the plan needs revisiting before
   anything is extracted.
7. **Optional but recommended.** Ask the maintainer to run the example models
   once now and keep the output, so the manual regression comparison at the
   close-out has something to compare against.

## Done when

- [ ] Branch created, baseline commit hash recorded in `deviations.md`.
- [ ] `check_target_cycles.py` exits 0.
- [ ] Debug and Release builds succeed; `ctest` passes.
- [ ] The three scripts exist, are committed, and report the expected numbers;
      `rename_extract.py --verify` reports 0 mismatches over every source file.
- [ ] FORD baseline captured (`ford` availability, `audit_ford_docs.py` output).
- [ ] Committed: `reorg step 01: baseline and the three move scripts`.
