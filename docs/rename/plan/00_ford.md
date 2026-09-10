# FORD documentation during the move

The source is documented for FORD, and the reorganisation touches the
documentation in four different ways. Each has a rule and, where possible, a
script. Read this together with `00_tooling.md`.

## What FORD reads here

`ford_project.md` sets `src_dir: src`, `preprocess: false`, `display: public
private protected`, `graph: true`, and no `docmark`/`predocmark` overrides, so
FORD's defaults apply: **`!>` documents what follows, `!!` documents what
precedes**, and `[[…]]` is a cross-reference.

Two consequences worth stating:

- `src_dir` is scanned recursively, so the new directories (`src/util/error/`,
  `src/io/`, `src/meteorology/`) need **no change to `ford_project.md`**.
- FORD only reads Fortran extensions, so the `*.backup` files that the scripts
  leave in the tree during a step are invisible to it. They are gitignored and
  deleted at the end of the step anyway.

Checked: there are no `[[…]]` links in `ford_project.md` or in `docs/*.md`, so
the link rewriting is confined to `src/` and `test/`.

## 1. Doc blocks travel with their entity

Handled by `rename_extract.py`; stated here so you can check its output.

- **Procedures and types.** The moved block starts at the top of the comment
  block above the code — which for this source often includes a legacy banner
  (`! 14/3/95`, a `!----` ruler) above the `!>` block, and in one case a blank
  line inside it. 267 of the 269 procedures have such a block. The two that do
  not are `mnplant` (`MNmod`) and `OCSIM_WORKSPACE_HAS_ALLOCATIONS` (`OCmod`):
  **do not write documentation for them as part of the move.** They stay
  undocumented, and that is a separate, later commit.
- **Variables.** Their documentation is a trailing `!!` on the declaration line
  (1,078 of 1,091). One has a `!>` block above it, two have `!!` continuation
  lines below, and eleven have no documentation at all. The extractor carries
  all three forms and, again, invents nothing for the eleven.
- **Plain-comment section headers** — `! Static integer controls.`, `! File
units occupy their rundata positions.` — are _not_ FORD documentation, and
  the extractor leaves them behind. There are 42 of them across 10 files, and
  they group declarations that this work often splits across several modules.
  For each one: repeat it in each new module over the declarations it still
  describes, or drop it deliberately. It appears in the coverage report, so it
  cannot be forgotten by accident.

## 2. Every new module needs a header

A module without a `!>` header is an empty page in the generated docs. The
extractor writes a skeleton with the `summary:` filled in from the move tables'
`target_module_purpose` and everything else marked `TODO`:

```fortran
!> summary: <target_module_purpose, verbatim>
!> author: <the author line of the module it came from>
!>
!> <two or three sentences: what this module holds, who writes it, who reads it>
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | <YYYY-MM-DD> | <author> | - | Split out of <SOURCE MODULE>; see docs/rename/proposal.md. |
!> @endhistory
```

`grep -rn "TODO" src` must be empty before a step is committed.

**The dissolved module's own header is documentation, not scaffolding.**
`AL_D`, `AL_C`, `SED_CS`, `CMmod`, `FRmod` and others carry long headers:
prose about who produces and consumes the state, tables (the `BALANC`
water-volume entries), `@warning` blocks recording that `MBLINK`, `MBFACE` and
`MBFLAG` have no current producer, lists of inactive legacy storage, and
`@history` tables going back to 1991. The coverage report prints all of it.

Rules for redistributing it:

- A paragraph goes to the module that now owns the state it describes.
- A paragraph describing state that this work splits is copied to each new
  module, trimmed to what that module actually holds.
- `@warning` blocks about undefined or unproduced state follow the variables
  they name — several of those variables land in `core/legacy_retained.f90`,
  and the warning is the reason that module exists.
- The `@history` table of the dissolved module is history of the code, not of
  the file. Carry it to the module that inherits the bulk of the content, and
  add the new "split out of …" row to every new module.
- Nothing is deleted because it no longer fits. If it truly belongs nowhere,
  it goes in `deviations.md`.

## 3. Cross-references

The source holds 281 qualified `[[module:entity]]` links and about 1,025 bare
`[[entity]]` links. Bare entity links resolve by name and survive the move
untouched — except for the 20 renamed constants. Qualified links name a module
that is about to stop existing.

`scripts/rename_ford_links.py` does the mechanical part. Measured on the
untouched tree: **287 links in 42 files rewrite automatically; 68 need a
decision.**

The 68 split into two kinds:

- **57 bare module links** — `[[sglobal]]` (7), `[[etmod]]` (7),
  `[[mod_error]]` (6), `[[frmod]]` (5), `[[vsmod]]` (4), `[[ocmod2]]` (4),
  `[[al_d]]` (4), `[[rest]]` (3), `[[ocmod]]` (3) and so on. The module splits
  several ways, so the script cannot pick. Read the sentence and name the
  successor that carries the behaviour being described: "[[frmod]] establishes
  the grid, run controls, …" becomes `[[frame_setup]]`; "[[rest]] updates
  meteorological forcing" becomes `[[met_input]]`. Where the sentence really
  means several modules, name several.
- **11 links that are already broken today** — `[[mod_error:RAISE_ERROR]]` (×4),
  `[[mod_error:ERR_STOP]]` (×2) and `[[mod_error:RAISE_ERROR]]` (×5), in `sglobal.f90`,
  `mod_error.f90` and `mod_load_filedata.f90`. Those entities do not exist in
  the current source. They are not this work's to fix, but the move must not
  make them worse: re-point the module half to the successor that inherits the
  text, leave the entity half alone, and list them in `deviations.md` as
  pre-existing dead links.

`test/` is in scope for the sweep: `test/oc_row_width/test_oc_row_width.f90`
carries four links — one `[[ocmod:ocsim]]` and three `[[ocmod:ocind]]` — which
become `[[oc_driver:ocsim]]` and `[[oc_indexing:ocind]]` in step 11.

**Renamed constants in prose.** The link rewriter renames `[[RHOA]]` to
`[[RHO_AIR_SNOW]]`, but a doc block that mentions `` `RHOA` `` in backticks is
ordinary text. The word sweep in step 02's checklist
(`grep -rnwE '(GRAVTY|RHOSED|…)' src test`) catches those too — it is a
documentation check as much as a code check.

## 4. Checking the result

- Per step, after the link rewrite:

  ```bash
  python3 scripts/rename_ford_links.py --check src test
  ```

  No rewrite may remain for a module that this step retired, and every `MANUAL`
  line it prints for those modules must have been resolved by hand.

- `python3 scripts/audit_ford_docs.py` reports legacy comment blocks left inside
  documented procedures and `INTENT` dummies without inline documentation. Its
  output is captured as a baseline in step 01; a move must not add to it. Do not
  fix pre-existing findings here.
- `./build.sh --docs-only` runs FORD over the tree (needs `ford` on `PATH`).
  Unresolved links show up in its output and in `docs/ford/`. Run it at the
  close-out, and after the two steps that move the most documentation (02
  and 11).

## Per-step FORD checklist

Copy into each step's done-when list:

- [ ] Every new module has `summary:`, an author line, prose and an `@history`
      row; `grep -rn "TODO" src` is empty.
- [ ] The retired module's header prose, tables and `@warning` blocks have been
      redistributed, not dropped.
- [ ] Plain-comment section headers from the coverage report have been placed or
      deliberately dropped.
- [ ] `rename_ford_links.py --check src test` shows nothing left for the modules
      this step retired.
- [ ] `MANUAL` links raised by this step are resolved; anything undecidable is
      in `deviations.md`.
