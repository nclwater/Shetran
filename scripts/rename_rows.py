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
