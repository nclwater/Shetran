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
