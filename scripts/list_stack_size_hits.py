#!/usr/bin/env python3
"""List every array that gfortran moves off the stack because of `-fmax-stack-var-size`.

gfortran's `-Wsurprising` warning

    Array 'x' at (1) is larger than limit set by '-fmax-stack-var-size=',
    moved from stack to static storage.

is emitted once per *site* that brings the array into scope, not once per
array: the declaration, every `USE ..., ONLY:` that names it, and every
`PUBLIC ::` that lists it each produce their own line. `docs/rename/plan/deviations.md`
D31 records a count that moved for that reason alone, which is why this script
reports the distinct arrays and, separately, where each one was reported.

By default it runs a clean Debug gfortran build and parses its output. Pass
`--log` to parse a build log that already exists.

Usage:
    python scripts/list_stack_size_hits.py
    python scripts/list_stack_size_hits.py --log build.log
    python scripts/list_stack_size_hits.py -o some/other/file.csv

(C) 2026, Sven Berendsen
"""

import argparse
import csv
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_OUT = ROOT / "docs" / "rename" / "issues" / "stack_size_limits_hits.csv"

# The header gfortran prints before each diagnostic: path, line, column.
RE_LOCATION = re.compile(r"^(/.*?\.[fF]90):(\d+):(\d+):\s*$")

# The quotes gfortran uses around the array name are typographic, not ASCII.
RE_WARNING = re.compile(
    r"^Warning: Array [‘'](?P<name>\w+)[’'] at \(1\) is larger than "
    r"limit set by [‘']-fmax-stack-var-size=[’']")

# The echoed source line, e.g. "   42 |    DOUBLEPRECISION :: FOO(NELEE)".
RE_SOURCE = re.compile(r"^\s*(\d+) \|(?P<text>.*)$")


def find_declaration(path, name):
    """Locate the declaration of `name` in `path`.

    gfortran anchors a module array's warning at the last place the
    specification part mentions the name, which since the `PUBLIC` lists were
    added is the export list rather than the declaration. Recover the
    declaration by reading the file.
    """

    pattern = re.compile(
        r"^\s*(?:INTEGER|REAL|DOUBLE\s*PRECISION|DOUBLEPRECISION|LOGICAL|"
        r"CHARACTER|COMPLEX|TYPE\s*\()\b.*?\b" + re.escape(name) + r"\b",
        re.I)

    try:
        lines = (ROOT / path).read_text(encoding="utf-8",
                                        errors="replace").splitlines()
    except OSError:
        return None, None

    for number, line in enumerate(lines, start=1):
        if line.lstrip().startswith("!"):
            continue
        if pattern.match(line):
            return number, line.split("!!")[0].strip()

    return None, None


def classify(source_line):
    """Name the kind of site the warning was reported against."""

    text = source_line.strip().upper()
    if text.startswith("USE "):
        return "use"
    if text.startswith("PUBLIC"):
        return "public"
    if text.startswith("PRIVATE"):
        return "private"
    return "declaration"


def run_build():
    """Run a clean Debug gfortran build and return its combined output."""

    build = ROOT / "build.sh"
    if not build.is_file():
        sys.exit(f"ERROR: {build} not found; use --log to parse an existing log.")

    print("INFO: running ./build.sh -t Debug -c gfortran --clean-app ...",
          file=sys.stderr)
    result = subprocess.run(
        [str(build), "-t", "Debug", "-c", "gfortran", "--clean-app"],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        errors="replace",
    )
    if result.returncode != 0:
        sys.stderr.write(result.stdout)
        sys.exit("ERROR: the build failed; nothing to report.")
    return result.stdout


def parse(log_text):
    """Return one record per warning site, in the order gfortran emitted them."""

    hits = []
    location = None
    source = ""

    for line in log_text.splitlines():
        match = RE_LOCATION.match(line)
        if match:
            location = (match.group(1), int(match.group(2)), int(match.group(3)))
            source = ""
            continue

        match = RE_SOURCE.match(line)
        if match:
            # Keep the last echoed source line; that is the one carrying "(1)".
            source = match.group("text").rstrip()
            continue

        match = RE_WARNING.match(line)
        if match and location is not None:
            path, line_no, column = location
            try:
                relative = str(Path(path).resolve().relative_to(ROOT))
            except ValueError:
                relative = path
            hits.append({
                "array": match.group("name"),
                "file": relative,
                "line": line_no,
                "column": column,
                "site_kind": classify(source),
                "source_line": source.strip(),
            })

    return hits


def main():

    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument(
        "--log",
        default=None,
        help="Parse this build log instead of running a build.",
    )
    parser.add_argument(
        "-o",
        "--output",
        default=str(DEFAULT_OUT),
        help=f"CSV to write (default: {DEFAULT_OUT.relative_to(ROOT)}).",
    )
    parser.add_argument(
        "--sites",
        action="store_true",
        help="Write one row per warning site instead of one row per array.",
    )
    args = parser.parse_args()

    if args.log:
        log_text = Path(args.log).read_text(encoding="utf-8", errors="replace")
    else:
        log_text = run_build()

    hits = parse(log_text)
    if not hits:
        print("INFO: no -fmax-stack-var-size warnings in the build output.",
              file=sys.stderr)

    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    if args.sites:
        fields = ["array", "file", "line", "column", "site_kind", "source_line"]
        rows = hits
    else:
        fields = [
            "array", "declared_in_file", "declared_at_line", "declaration",
            "num_warning_sites", "num_declaration_sites", "num_use_sites",
            "num_public_sites", "reported_at",
        ]
        grouped = {}
        for hit in hits:
            grouped.setdefault(hit["array"].lower(), []).append(hit)

        rows = []
        for key in sorted(grouped):
            sites = grouped[key]
            declarations = [s for s in sites if s["site_kind"] == "declaration"]
            exports = [s for s in sites if s["site_kind"] == "public"]
            if declarations:
                first = declarations[0]
                declared_file = first["file"]
                declared_line = first["line"]
                declaration = first["source_line"]
            else:
                # A module array whose warning gfortran anchored at the export
                # list; the owning module is still that file.
                first = exports[0] if exports else sites[0]
                declared_file = first["file"] if exports else ""
                declared_line, declaration = (
                    find_declaration(declared_file, first["array"])
                    if declared_file else (None, None))
                declared_line = declared_line or ""
                declaration = declaration or ""
            rows.append({
                "array": first["array"],
                "declared_in_file": declared_file,
                "declared_at_line": declared_line,
                "declaration": declaration,
                "num_warning_sites": len(sites),
                "num_declaration_sites": len(declarations),
                "num_use_sites": sum(1 for s in sites if s["site_kind"] == "use"),
                "num_public_sites":
                    sum(1 for s in sites if s["site_kind"] == "public"),
                "reported_at": ";".join(
                    f"{s['file']}:{s['line']}" for s in sites),
            })

    with out_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)

    distinct = len({h["array"].lower() for h in hits})
    print(f"{len(hits)} warning sites over {distinct} distinct arrays "
          f"-> {out_path.relative_to(ROOT) if out_path.is_absolute() else out_path}")


if __name__ == "__main__":
    main()
