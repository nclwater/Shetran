# Tooling

The moves are done **by script, not by hand**. Every step below runs one of the
three scripts in this file; hand editing is for the things a script cannot
decide -- `USE` lines, header prose, `PUBLIC` lists -- and never for moving code.

## The `.backup` rule

Whenever a script rewrites a file it first renames the existing file to
`<path>.backup` and then produces the new content **by copying from that
backup**. Nothing is retyped, and the original is on disk for the whole step, so
any block can be compared or recovered with an ordinary `diff`.

- `*.backup` is already in `.gitignore`, so backups are never committed.
- Only one generation of backups exists at a time. A script that finds an
  existing backup stops rather than overwriting it.
- Backups are deleted at the end of a step, **after** the build and the tests
  pass and before the commit:

  ```bash
  find src test -name '*.backup' -delete
  ```

- A file that several steps take from -- `AL_D`, `FRmod`, `ETmod`, `SMmod`,
  `OCmod2`, `AL_C`, `mod_error` -- is backed up again, from its current state,
  in each of those steps. That is why the extractor finds entities by content
  rather than by the line numbers in the CSVs, which are only valid for the
  untouched tree.

Create all three scripts in step 01 and commit them; they are part of the work,
not scratch files.

## 1. `scripts/rename_rows.py` -- query the move tables

Read-only. Answers "what goes into this file", "where does this file's content
go", "who references this name".

```bash
python3 scripts/rename_rows.py --target src/core/file_units.f90
python3 scripts/rename_rows.py --source src/core/sglobal.f90
python3 scripts/rename_rows.py --entity HRFZZ --refs
python3 scripts/rename_rows.py --target-dir src/nitrate
```

```python
#!/usr/bin/env python3
"""Read-only queries over the docs/rename/*.csv move tables."""

from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "docs" / "rename"
TABLES = {
    "functions.csv": "function_name",
    "variables.csv": "variable_name",
    "types.csv": "type_name",
}


def rows():
    for table, key in TABLES.items():
        for row in csv.DictReader((OUT / table).open()):
            row["_kind"] = table.split(".")[0][:4]
            row["_name"] = row[key]
            row["_start"] = row.get("move_block_start_line") or row.get("line") or "0"
            row["_end"] = row.get("ending_line") or row["_start"]
            yield row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--target", help="target file, e.g. src/core/file_units.f90")
    parser.add_argument("--target-dir", help="target directory, e.g. src/core")
    parser.add_argument("--source", help="source file, e.g. src/core/sglobal.f90")
    parser.add_argument("--entity", help="entity name (case-insensitive)")
    parser.add_argument("--refs", action="store_true", help="also print the reference columns")
    args = parser.parse_args()

    selected = [
        row
        for row in rows()
        if (not args.target or row["target_file"] == args.target)
        and (not args.target_dir or row["target_file"].startswith(args.target_dir.rstrip("/") + "/"))
        and (not args.source or row["source_file"] == args.source)
        and (not args.entity or row["_name"].lower() == args.entity.lower())
    ]
    selected.sort(key=lambda r: (r["target_file"], r["source_file"], int(r["_start"])))

    for row in selected:
        rename = f"  (rename -> {row['proposed_rename']})" if row.get("proposed_rename") else ""
        nested = "  [nested]" if row.get("nested_in_parent") == "True" else ""
        print(
            f"{row['_kind']:4s} {row['_name']:32s} "
            f"{row['source_file']}:{row['_start']}-{row['_end']} "
            f"-> {row['target_module']} ({row['target_file']}){rename}{nested}"
        )
        if args.refs:
            print(f"       referenced_by: {row['referenced_by'] or '-'}")
            print(f"       possibly:      {row['possibly_referenced_by'] or '-'}")

    print(f"# {len(selected)} rows", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
```

## 2. `scripts/rename_extract.py` -- move the code

This is the script that does the extraction. It renames each `--source` to
`<source>.backup`, writes every target file from that copy, writes the residual
source back from the same copy, and prints every line it did **not** move.

```bash
# always look first
python3 scripts/rename_extract.py --source src/nitrate/MNmod.f90 --dry-run

# confirm the locator agrees with the recorded line numbers (untouched files only)
python3 scripts/rename_extract.py --source src/nitrate/MNmod.f90 --verify

# then do it
python3 scripts/rename_extract.py --source src/nitrate/MNmod.f90

# partial extraction: only the rows that land in one directory or one file
python3 scripts/rename_extract.py --source src/frame/FRmod.f90 \
        --target-dir src/evapotranspiration
python3 scripts/rename_extract.py --source src/core/state/AL_D.f90 \
        --target src/overland_channel/oc_state.f90
```

Both `--target` and `--target-dir` may be repeated; without them every row of
the source is taken.

What it guarantees:

- a moved block is copied verbatim, **its FORD doc block included** -- for
  procedures and types the block starts at the top of the comment block above
  the code, which is what `move_block_start_line` records; for variables it is
  the declaration line plus a `!>` block above it and `!!` continuation lines
  below it;
- contained procedures travel inside their parent, never separately;
- two rows may not claim the same lines for different targets (the script stops);
- one declaration line naming two variables that share a target is emitted once.

What it leaves to you:

- **the coverage report.** Everything not moved is printed with its line
  number: the module header prose, the `USE` block, `IMPLICIT NONE`, the
  `PUBLIC`/`PRIVATE` lists, and the plain-comment section headers (`! Static
integer controls.`) that group declarations which may now be in different
  modules. Each of those lines needs a decision. None may be dropped silently.
- **the TODO markers** in a generated module: the `summary:` line is filled in
  from the move tables, but the author line, the header prose and the
  `@history` row are marked `TODO` and must be written before the step is
  committed (`grep -rn "TODO" src` must come back empty).
- **the `USE` lines.** The skeleton has a `! TODO USE ..., ONLY: ...` marker;
  the compiler tells you what is missing.
- **an existing target.** The eight state modules created in step 04 already
  exist when their component step runs, so the script writes
  `<target>.append` -- two labelled blocks to paste in -- instead of
  overwriting them.
- **deleting the emptied source.** The residual is written back; when only the
  module shell is left, `git rm` it yourself.

```python
#!/usr/bin/env python3
"""Move the entities listed in the move tables out of a source file.

For every ``--source`` the script renames the file to ``<path>.backup`` and then
writes, from that copy, both the new target modules and the residual source.
Nothing is retyped: every moved line is copied verbatim, doc block included.
``*.backup`` is already in ``.gitignore``.

Entities are located by content, not by the line numbers in the CSVs, so a file
that is emptied over several steps (``AL_D``, ``FRmod``, ``ETmod``, ``SMmod``,
``OCmod2``) can be processed again after it has already shrunk. ``--verify``
checks the locator against the recorded line numbers; it only means anything
while the file is still untouched.

A target that does not exist yet is written as a complete module skeleton with
TODO markers for the FORD header and the USE lines. A target that already
exists gets a ``<target>.append`` file instead, to be pasted in by hand.

What is left over in the source -- module header prose, USE statements,
PUBLIC/PRIVATE lists, plain-comment section headers -- is printed line by line.
Every one of those lines needs a decision; none may be dropped silently.
"""

from __future__ import annotations

import argparse
import csv
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
TABLES = {"functions.csv": "function_name", "variables.csv": "variable_name", "types.csv": "type_name"}
KIND = r"(?:DOUBLE\s*PRECISION|REAL|INTEGER|LOGICAL|CHARACTER|TYPE\s*\([^)]*\))"


def load_rows():
    rows = []
    for table, key in TABLES.items():
        for row in csv.DictReader((ROOT / "docs" / "rename" / table).open()):
            row["_kind"] = {"func": "proc", "vari": "var", "type": "type"}[table.split(".")[0][:4]]
            row["_name"] = row[key]
            rows.append(row)
    return rows


def doc_start(lines, i):
    """Walk up over the comment block that documents line ``i`` (0-based)."""
    start = i
    while start > 0 and (lines[start - 1].lstrip().startswith("!") or not lines[start - 1].strip()):
        start -= 1
    while start < i and not lines[start].strip():
        start += 1
    return start


def find_proc(lines, name):
    n = re.escape(name)
    head = re.compile(rf"^\s*(?:(?:PURE|RECURSIVE|ELEMENTAL|IMPURE|MODULE)\s+)*"
                      rf"(?:{KIND}\s*(?:\([^)]*\))?\s+)?(SUBROUTINE|FUNCTION)\s+{n}\s*(\(|$)", re.I)
    for i, line in enumerate(lines):
        match = head.match(line)
        if not match:
            continue
        end = re.compile(rf"^\s*END\s*{match.group(1)}(\s+{n})?\s*(!.*)?$", re.I)
        for j in range(i + 1, len(lines)):
            if end.match(lines[j]):
                return doc_start(lines, i) + 1, j + 1
        return None
    return None


def find_type(lines, name):
    n = re.escape(name)
    head = re.compile(rf"^\s*TYPE\s*(?:,[^:]*)?(?:::)?\s*{n}\s*$", re.I)
    for i, line in enumerate(lines):
        if not head.match(line):
            continue
        for j in range(i + 1, len(lines)):
            if re.match(rf"^\s*END\s*TYPE(\s+{n})?\s*$", lines[j], re.I):
                return doc_start(lines, i) + 1, j + 1
        return None
    return None


def find_var(lines, name):
    """Declaration line, plus a !> block above it and !! continuations below."""
    n = re.escape(name)
    decl = re.compile(rf"^\s*(?:{KIND})(?![\w]).*?(?<![\w%]){n}\b", re.I)
    inside_type = False
    for i, line in enumerate(lines):
        if re.match(r"^\s*CONTAINS\s*$", line, re.I):
            break
        boundary = re.match(r"^\s*(TYPE\s*(,|::|\s+\w+\s*$)|END\s*TYPE)", line, re.I)
        if boundary:
            inside_type = not line.strip().upper().startswith("END")
            continue
        if inside_type or not decl.match(line.split("!")[0]):
            continue
        start = end = i
        while start > 0 and lines[start - 1].lstrip().startswith("!>"):
            start -= 1
        while end + 1 < len(lines) and lines[end + 1].lstrip().startswith("!!"):
            end += 1
        return start + 1, end + 1
    return None


def locate(row, lines):
    if row["_kind"] == "proc":
        return find_proc(lines, row["_name"])
    if row["_kind"] == "type":
        return find_type(lines, row["_name"])
    return find_var(lines, row["_name"])


def skeleton(module, purpose, sources, decls, procs):
    src = ", ".join(sorted(sources))
    out = [
        f"!> summary: {purpose}",
        f"!> author: TODO carry the author line over from {src}",
        "!>",
        f"!> TODO: two or three sentences, taken from the header of {src}.",
        "!>",
        "!> @history",
        "!> | Date | Author | Version | Description |",
        "!> |:-----|:-------|:--------|:------------|",
        f"!> | TODO date | TODO | - | Split out of {src}; see docs/rename/proposal.md. |",
        "!> @endhistory",
        f"MODULE {module}",
        "",
        "   ! TODO USE ..., ONLY: ...",
        "",
        "   IMPLICIT NONE",
        "",
    ]
    if decls:
        out += decls + [""]
    if procs:
        out += ["CONTAINS", ""] + procs
    out += [f"END MODULE {module}", ""]
    return out


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", action="append", default=[], required=True,
                        help="source file to extract from, e.g. src/nitrate/MNmod.f90")
    parser.add_argument("--target", action="append", default=[],
                        help="only extract the rows landing in this file (repeatable)")
    parser.add_argument("--target-dir", action="append", default=[],
                        help="only extract the rows landing under this directory (repeatable)")
    parser.add_argument("--dry-run", action="store_true", help="report, write nothing")
    parser.add_argument("--verify", action="store_true",
                        help="check the locator against the CSV line numbers and stop")
    args = parser.parse_args()

    rows = load_rows()
    text = {}
    for source in args.source:
        path = ROOT / source
        backup = path.with_name(path.name + ".backup")
        if backup.exists():
            sys.exit(f"{backup} exists; clear the previous step's backups first "
                     f"(find src test -name '*.backup' -delete)")
        if not path.exists():
            sys.exit(f"missing source: {path}")
        text[source] = path.read_text(errors="replace").splitlines()

    if args.verify:
        bad = 0
        for row in rows:
            if row["source_file"] not in text or row.get("nested_in_parent") == "True":
                continue
            found = locate(row, text[row["source_file"]])
            want = (int(row.get("move_block_start_line") or row["line"]),
                    int(row.get("ending_line") or row["line"]))
            if found is None or (row["_kind"] != "var" and found != want):
                print(f"MISMATCH {row['_name']:30s} {row['source_file']} found={found} csv={want}")
                bad += 1
            elif row["_kind"] == "var" and not found[0] <= int(row["line"]) <= found[1]:
                print(f"MISMATCH {row['_name']:30s} {row['source_file']} found={found} csv={row['line']}")
                bad += 1
        print(f"{bad} mismatch(es)")
        return 1 if bad else 0

    dirs = [d.rstrip("/") + "/" for d in args.target_dir]

    def wanted(row):
        if not dirs and not args.target:
            return True
        return row["target_file"] in args.target or any(row["target_file"].startswith(d) for d in dirs)

    selected = [r for r in rows if r["source_file"] in text and wanted(r)]
    owner = {s: {} for s in text}

    for target in sorted({r["target_file"] for r in selected}):
        mine = [r for r in selected if r["target_file"] == target]
        located = []
        for row in mine:
            if row.get("nested_in_parent") == "True":
                continue
            found = locate(row, text[row["source_file"]])
            if found is None:
                sys.exit(f"cannot locate {row['_name']} in {row['source_file']}")
            located.append((row, found))
        located.sort(key=lambda pair: (pair[0]["source_file"], pair[1][0]))

        decls, procs, from_modules = [], [], set()
        purpose = mine[0]["target_module_purpose"]
        for row, (start, end) in located:
            from_modules.add(row["source_module"])
            taken = owner[row["source_file"]]
            if all(taken.get(n) == target for n in range(start, end + 1)):
                continue                      # one line declaring two names for the same target
            clash = {taken[n] for n in range(start, end + 1) if n in taken}
            if clash:
                sys.exit(f"overlap at {row['source_file']}:{start}-{end} "
                         f"({row['_name']} -> {target}, already taken by {', '.join(clash)})")
            for n in range(start, end + 1):
                taken[n] = target
            block = text[row["source_file"]][start - 1:end]
            if row["_kind"] != "var" or end > start:
                block = block + [""]
            (decls if row["_kind"] in ("var", "type") else procs).extend(block)

        path = ROOT / target
        if path.exists():
            out = path.with_name(path.name + ".append")
            body = (["! paste before CONTAINS:"] + decls
                    + ["! paste before END MODULE (inside CONTAINS):"] + procs)
        else:
            out = path
            body = skeleton(mine[0]["target_module"], purpose, from_modules, decls, procs)
        print(f"{'would write' if args.dry_run else 'wrote'} {out.relative_to(ROOT)}"
              f"  ({len(decls)} declaration lines, {len(procs)} procedure lines)")
        if not args.dry_run:
            out.parent.mkdir(parents=True, exist_ok=True)
            out.write_text("\n".join(body) + "\n")

    for source, lines in text.items():
        keep = [n for n in range(1, len(lines) + 1) if n not in owner[source]]
        residual = [lines[n - 1] for n in keep]
        moved = len(lines) - len(residual)
        left = [n for n in keep if lines[n - 1].strip()]
        if not args.dry_run:
            path = ROOT / source
            path.rename(path.with_name(path.name + ".backup"))
            path.write_text("\n".join(residual) + "\n")
        print(f"\n=== {source}: {moved} of {len(lines)} lines moved, {len(left)} non-blank left "
              f"({'nothing written, dry run' if args.dry_run else 'rewritten from the backup'}) ===")
        for n in left:
            print(f"  {n:6d}  {lines[n - 1]}")
        print("--- place every line above by hand: header prose, USE statements,")
        print("--- PUBLIC/PRIVATE lists, section comments. Nothing may be dropped.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
```

## 3. `scripts/rename_ford_links.py` -- re-point the FORD cross-references

Rewrites `[[module:entity]]` links so they name the entity's new module, and
renames links to the 20 renamed constants. Same `.backup` rule.

```bash
python3 scripts/rename_ford_links.py --check src test    # report, write nothing
python3 scripts/rename_ford_links.py src test            # rewrite
```

`--check` exits 1 while any link still needs a human decision, so it can be used
as a gate; that is not a failure of the script.

Measured against the untouched tree: **287 links in 42 files rewrite
automatically, 68 need a human decision** -- 57 bare `[[module]]` links whose
module splits several ways, and 11 links that are already broken today
(`[[mod_error:RAISE_ERROR]]`, `[[mod_error:ERR_STOP]]`, `[[mod_error:RAISE_ERROR]]`, in
`sglobal.f90`, `mod_error.f90` and `mod_load_filedata.f90`). The script prints
those as `MANUAL` lines; see `00_ford.md` for what to do with them.

```python
#!/usr/bin/env python3
"""Rewrite FORD cross-reference links to follow the module split.

FORD resolves `[[module:entity]]` and `[[entity]]`. The reorganisation renames
every in-scope module, so the qualified links have to be re-pointed and the
links naming a renamed constant have to be renamed with it. This reads the same
move tables as the extraction and rewrites the links it can resolve; anything
ambiguous is reported and left alone.

`--check` reports without writing. When writing, a file is first renamed to
`<path>.backup` and the new text is produced from that copy; `*.backup` is in
`.gitignore`.
"""

from __future__ import annotations

import argparse
import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
TABLES = {"functions.csv": "function_name", "variables.csv": "variable_name", "types.csv": "type_name"}
LINK = re.compile(r"\[\[([A-Za-z0-9_]+)(?::([A-Za-z0-9_]+))?\]\]")


def load():
    entity, successors, renamed = {}, defaultdict(set), {}
    for table, key in TABLES.items():
        for row in csv.DictReader((ROOT / "docs" / "rename" / table).open()):
            name = row[key]
            new_name = row.get("proposed_rename") or name
            entity[(row["source_module"].lower(), name.lower())] = (row["target_module"], new_name)
            successors[row["source_module"].lower()].add(row["target_module"])
            if row.get("proposed_rename"):
                renamed[name.lower()] = row["proposed_rename"]
    return entity, successors, renamed


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", default=["src", "test"])
    parser.add_argument("--check", action="store_true", help="report only, write nothing")
    args = parser.parse_args()

    entity, successors, renamed = load()
    unresolved, changed_files = [], 0

    files = []
    for p in args.paths:
        path = ROOT / p
        if path.is_dir():
            files += sorted(path.rglob("*.[fF]90"))
        elif path.is_file():
            files.append(path)

    for path in files:
        text = path.read_text(errors="replace")
        hits = []

        def replace(match):
            module, name = match.group(1), match.group(2)
            if name is None:                                  # [[thing]]
                key = module.lower()
                if key in renamed:                            # a renamed constant
                    hits.append((match.group(0), f"[[{renamed[key]}]]"))
                    return f"[[{renamed[key]}]]"
                if key in successors:                         # a module name
                    targets = successors[key]
                    if len(targets) == 1:
                        new = next(iter(targets))
                        if new.lower() != key:
                            hits.append((match.group(0), f"[[{new}]]"))
                            return f"[[{new}]]"
                    else:
                        unresolved.append((path, match.group(0), "module split; pick the successor"))
                return match.group(0)
            found = entity.get((module.lower(), name.lower()))
            if found is None:
                if module.lower() in successors:
                    unresolved.append((path, match.group(0), "unknown entity in a split module"))
                return match.group(0)
            new_module, new_name = found
            new_link = f"[[{new_module}:{new_name}]]"
            if new_link.lower() != match.group(0).lower():
                hits.append((match.group(0), new_link))
                return new_link
            return match.group(0)

        new_text = LINK.sub(replace, text)
        if not hits:
            continue
        changed_files += 1
        rel = path.relative_to(ROOT)
        print(f"{rel}: {len(hits)} link(s)")
        for old, new in hits:
            print(f"    {old} -> {new}")
        if not args.check:
            backup = path.with_name(path.name + ".backup")
            if backup.exists():
                sys.exit(f"{backup} already exists; clear the backups from the last run first")
            path.rename(backup)
            path.write_text(new_text)

    for path, link, why in unresolved:
        print(f"MANUAL {path.relative_to(ROOT)}: {link} ({why})")
    print(f"\n{changed_files} file(s) {'would change' if args.check else 'rewritten'}, "
          f"{len(unresolved)} link(s) need a decision", file=sys.stderr)
    return 1 if unresolved and args.check else 0


if __name__ == "__main__":
    sys.exit(main())
```
