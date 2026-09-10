#!/usr/bin/env python3
"""Re-point explicit ``USE ..., ONLY:`` statements to follow the module split.

A step moves entities out of a module; every consumer that imported one of them
by name has to import it from its new module instead. That rewrite is decided
entirely by the move tables -- which name went where, and under what new name --
so it is done here rather than by hand, for the same reason the code itself is
moved by ``rename_extract.py``: nothing is retyped and nothing is missed.

For each ``USE <source>, ONLY: a, b => c`` statement it splits the name list by
target module, preserves ``alias => name`` forms and the 20 constant renames,
drops the original statement when every name has left, and leaves untouched any
name that this step does not move.

Restrict a run to the step's targets with ``--target-dir``/``--target``,
exactly as for ``rename_extract.py``. Bare ``USE <module>`` statements carry no
name list, so they cannot be rewritten mechanically: they are reported and left
alone.

The ``.backup`` rule of ``00_tooling.md`` applies: a rewritten file is first
renamed to ``<path>.backup`` and the new text is produced from that copy.
"""

from __future__ import annotations

import argparse
import csv
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
TABLES = {"functions.csv": "function_name", "variables.csv": "variable_name",
          "types.csv": "type_name"}
USE_ONLY = re.compile(r"^(\s*)USE\s+([A-Za-z0-9_]+)\s*,\s*ONLY\s*:\s*(.*)$", re.I)
USE_BARE = re.compile(r"^(\s*)USE\s+([A-Za-z0-9_]+)\s*(!.*)?$", re.I)


def load_map(targets, dirs, sources):
    """(source_module.lower(), name.lower()) -> (target_module, new_name)."""
    moved = {}
    for table, key in TABLES.items():
        for row in csv.DictReader((ROOT / "docs" / "rename" / table).open()):
            if sources and row["source_file"] not in sources:
                continue
            if targets or dirs:
                if not (row["target_file"] in targets
                        or any(row["target_file"].startswith(d) for d in dirs)):
                    continue
            name = row[key]
            moved[(row["source_module"].lower(), name.lower())] = (
                row["target_module"], row.get("proposed_rename") or name)
    return moved


def statement(lines, i):
    """The full logical USE statement starting at line ``i``, and its extent."""
    text, j = lines[i], i
    while text.rstrip().endswith("&"):
        j += 1
        text = text.rstrip().rstrip("&") + lines[j].strip().lstrip("&")
    return text, j


def split_names(only):
    """``a, b => c`` -> [(alias or None, name)], comments stripped."""
    only = only.split("!")[0]
    out = []
    for item in only.split(","):
        item = item.strip()
        if not item:
            continue
        if "=>" in item:
            alias, _, name = item.partition("=>")
            out.append((alias.strip(), name.strip()))
        else:
            out.append((None, item))
    return out


def render(indent, module, names, width=100):
    """One USE statement, continued over several lines if it is long."""
    items = [f"{a} => {n}" if a else n for a, n in names]
    line = f"{indent}USE {module}, ONLY: "
    out, current = [], line
    for k, item in enumerate(items):
        piece = item + (", " if k < len(items) - 1 else "")
        if len(current) + len(piece) > width and current != line:
            out.append(current.rstrip() + " &")
            current = indent + "   " + " " * len("USE , ONLY: ") + piece
        else:
            current += piece
    out.append(current)
    return out


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", default=["src", "test"])
    parser.add_argument("--target", action="append", default=[])
    parser.add_argument("--target-dir", action="append", default=[])
    parser.add_argument("--from-source", action="append", default=[],
                        help="only remap entities that left this source file (repeatable). "
                             "Without it, a --target shared by two steps would remap names "
                             "that have not moved yet.")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    moved = load_map(args.target, [d.rstrip("/") + "/" for d in args.target_dir],
                     args.from_source)
    # Only a module that actually loses an entity can make a bare USE stale.
    sources = {m for (m, _), (target, _) in moved.items() if target.lower() != m}

    files = []
    for p in args.paths:
        path = ROOT / p
        files += sorted(path.rglob("*.[fF]90")) if path.is_dir() else [path]

    bare, changed = [], 0
    for path in files:
        lines = path.read_text(errors="replace").splitlines()
        out, i, hits = [], 0, []
        while i < len(lines):
            match = USE_ONLY.match(lines[i])
            if not match:
                plain = USE_BARE.match(lines[i])
                if plain and plain.group(2).lower() in sources:
                    bare.append((path, lines[i].strip()))
                out.append(lines[i])
                i += 1
                continue
            text, end = statement(lines, i)
            indent, module, only = USE_ONLY.match(text).groups()
            groups, stay = {}, []
            for alias, name in split_names(only):
                found = moved.get((module.lower(), name.lower()))
                if found is None:
                    stay.append((alias, name))
                else:
                    target, new_name = found
                    if target.lower() == module.lower():
                        stay.append((alias, new_name))
                    else:
                        groups.setdefault(target, []).append((alias, new_name))
            if not groups:
                out += lines[i:end + 1]
                i = end + 1
                continue
            if stay:
                out += render(indent, module, stay)
            for target in sorted(groups):
                out += render(indent, target, groups[target])
            hits.append((module, {t: [n for _, n in v] for t, v in groups.items()},
                         [n for _, n in stay]))
            i = end + 1
        if not hits:
            continue
        changed += 1
        rel = path.relative_to(ROOT)
        for module, groups, stay in hits:
            moved_to = "; ".join(f"{t}: {', '.join(v)}" for t, v in sorted(groups.items()))
            print(f"{rel}: USE {module} -> {moved_to}"
                  + (f"  (kept in {module}: {', '.join(stay)})" if stay else "  (statement removed)"))
        if not args.dry_run:
            backup = path.with_name(path.name + ".backup")
            if not backup.exists():
                path.rename(backup)
                text = backup.read_text(errors="replace")
            else:
                text = path.read_text(errors="replace")
            path.write_text("\n".join(out) + ("\n" if text.endswith("\n") else ""))

    for path, line in bare:
        print(f"BARE {path.relative_to(ROOT)}: {line} (no name list; rewrite by hand)")
    print(f"\n{changed} file(s) {'would change' if args.dry_run else 'rewritten'}, "
          f"{len(bare)} bare USE statement(s) need a decision", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
