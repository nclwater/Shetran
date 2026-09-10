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
