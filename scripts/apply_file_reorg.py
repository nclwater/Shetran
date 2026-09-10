#!/usr/bin/env python3
"""Apply the source-tree reorganisation described in docs/file_reorg.csv.

The CSV has three columns -- ``filename``, ``current_dir`` and ``proposed_dir``
-- and lists every Fortran file under ``src/``. Rows where the two directories
are the same are ignored; every other row is a move.

The script validates the whole plan before touching anything: it refuses to run
if a source file is missing, if a destination is already occupied, if two files
would land on the same path, or if a Fortran file under ``src/`` is absent from
the CSV. Nothing is moved unless every check passes.

Tracked files are moved with ``git mv`` so the rename is recorded (and staged);
untracked files are moved with ``shutil.move``. Source directories that end up
empty are removed.

The main CMakeLists.txt needs no change: it collects sources with
``file(GLOB_RECURSE ...)`` over ``src/``. Other files do spell out individual
source paths -- test/CMakeLists.txt names several -- so the script greps the
repository for references to every path it moves and reports them. Those hits
are warnings, not errors: the script does not edit them, they have to be
updated by hand. Re-run CMake afterwards so the glob and the Fortran module
dependency scan pick up the new paths.
"""

from __future__ import annotations

import argparse
import csv
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path, PurePosixPath


REQUIRED_COLUMNS = ("filename", "current_dir", "proposed_dir")
SOURCE_SUFFIXES = {".f90", ".F90"}
SOURCE_ROOT = "src"


@dataclass(frozen=True)
class Move:
    """One planned rename, as repository-relative POSIX paths."""

    filename: str
    source: PurePosixPath
    destination: PurePosixPath


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--root",
        type=Path,
        default=Path(__file__).resolve().parent.parent,
        help="repository root (default: the parent of scripts/)",
    )
    parser.add_argument(
        "--csv",
        type=Path,
        default=None,
        help="reorganisation table (default: <root>/docs/file_reorg.csv)",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="validate and print the plan without moving anything",
    )
    parser.add_argument(
        "--no-git",
        action="store_true",
        help="move with the filesystem even for tracked files",
    )
    parser.add_argument(
        "--keep-empty-dirs",
        action="store_true",
        help="do not remove source directories left empty by the moves",
    )
    return parser.parse_args(argv)


def normalise_dir(value: str) -> PurePosixPath:
    """Turn a CSV directory cell into a clean relative path."""
    cleaned = value.strip().strip("/")
    if not cleaned:
        raise ValueError("empty directory")
    return PurePosixPath(cleaned)


def read_plan(csv_path: Path) -> tuple[list[Move], set[PurePosixPath], list[str]]:
    """Return the moves, every path named by the CSV, and any parse errors."""
    moves: list[Move] = []
    listed: set[PurePosixPath] = set()
    errors: list[str] = []
    seen: dict[str, int] = {}

    with csv_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        missing = [c for c in REQUIRED_COLUMNS if c not in (reader.fieldnames or [])]
        if missing:
            errors.append(f"{csv_path}: missing column(s): {', '.join(missing)}")
            return moves, listed, errors

        for row in reader:
            line = reader.line_num
            filename = (row["filename"] or "").strip()
            if not filename:
                errors.append(f"line {line}: empty filename")
                continue
            if filename in seen:
                errors.append(
                    f"line {line}: {filename} already listed on line {seen[filename]}"
                )
                continue
            seen[filename] = line

            try:
                current = normalise_dir(row["current_dir"] or "")
                proposed = normalise_dir(row["proposed_dir"] or "")
            except ValueError as exc:
                errors.append(f"line {line}: {filename}: {exc}")
                continue

            source = current / filename
            listed.add(source)
            if current != proposed:
                moves.append(Move(filename, source, proposed / filename))

    return moves, listed, errors


def validate(root: Path, moves: list[Move], listed: set[PurePosixPath]) -> list[str]:
    """Check the whole plan against the working tree."""
    errors: list[str] = []

    for move in moves:
        if not (root / move.source).is_file():
            errors.append(f"{move.source}: source file not found")
        destination = root / move.destination
        if destination.exists():
            errors.append(f"{move.destination}: destination already exists")

    targets: dict[PurePosixPath, str] = {}
    for move in moves:
        clash = targets.get(move.destination)
        if clash is not None:
            errors.append(
                f"{move.destination}: target of both {clash} and {move.source}"
            )
        else:
            targets[move.destination] = str(move.source)

    source_root = root / SOURCE_ROOT
    if source_root.is_dir():
        for path in sorted(source_root.rglob("*")):
            if not path.is_file() or path.suffix not in SOURCE_SUFFIXES:
                continue
            relative = PurePosixPath(path.relative_to(root).as_posix())
            if relative not in listed:
                errors.append(f"{relative}: source file not listed in the CSV")

    return errors


def tracked_files(root: Path) -> set[PurePosixPath]:
    """Repository-relative paths known to git, or an empty set outside a repo."""
    try:
        result = subprocess.run(
            ["git", "-C", str(root), "ls-files", "-z"],
            capture_output=True,
            check=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return set()
    return {
        PurePosixPath(entry)
        for entry in result.stdout.decode("utf-8").split("\0")
        if entry
    }


def stale_references(root: Path, moves: list[Move]) -> dict[str, list[str]]:
    """Tracked files outside src/ that spell out a path this plan moves.

    Returns a mapping of file to the line numbers that mention a moved path.
    """
    patterns: list[str] = []
    for move in moves:
        patterns.extend(["-e", str(move.source)])
    try:
        result = subprocess.run(
            ["git", "-C", str(root), "grep", "-n", "-F", *patterns, "--",
             ".", f":(exclude){SOURCE_ROOT}"],
            capture_output=True,
            check=False,
        )
    except OSError:
        return {}
    if result.returncode not in (0, 1):
        return {}

    hits: dict[str, list[str]] = {}
    for line in result.stdout.decode("utf-8", "replace").splitlines():
        path, _, remainder = line.partition(":")
        number, _, _ = remainder.partition(":")
        if path and number.isdigit():
            hits.setdefault(path, []).append(number)
    return hits


def apply_move(root: Path, move: Move, use_git: bool) -> None:
    (root / move.destination).parent.mkdir(parents=True, exist_ok=True)
    if use_git:
        subprocess.run(
            ["git", "-C", str(root), "mv", str(move.source), str(move.destination)],
            check=True,
        )
    else:
        shutil.move(str(root / move.source), str(root / move.destination))


def prune_empty_dirs(root: Path, moves: list[Move]) -> list[PurePosixPath]:
    """Remove source directories the moves left empty, deepest first."""
    candidates = {move.source.parent for move in moves}
    removed: list[PurePosixPath] = []
    for relative in sorted(candidates, key=lambda p: len(p.parts), reverse=True):
        directory = root / relative
        if directory.is_dir() and not any(directory.iterdir()):
            directory.rmdir()
            removed.append(relative)
    return removed


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    root = args.root.resolve()
    csv_path = args.csv if args.csv is not None else root / "docs" / "file_reorg.csv"

    if not csv_path.is_file():
        print(f"error: {csv_path} not found", file=sys.stderr)
        return 1

    moves, listed, errors = read_plan(csv_path)
    errors.extend(validate(root, moves, listed))
    if errors:
        print(f"error: refusing to move anything, {len(errors)} problem(s):",
              file=sys.stderr)
        for message in errors:
            print(f"  {message}", file=sys.stderr)
        return 1

    if not moves:
        print("nothing to do: every file is already in its proposed directory")
        return 0

    tracked = set() if args.no_git else tracked_files(root)
    width = max(len(str(move.source)) for move in moves)

    for move in sorted(moves, key=lambda m: (str(m.destination), m.filename)):
        use_git = move.source in tracked
        marker = "git" if use_git else "fs "
        print(f"{marker}  {str(move.source):<{width}}  ->  {move.destination}")
        if not args.dry_run:
            apply_move(root, move, use_git)

    references = stale_references(root, moves)
    if references:
        total = sum(len(lines) for lines in references.values())
        print(f"\nwarning: {total} reference(s) to moved paths in "
              f"{len(references)} file(s) outside src/; not edited by this "
              "script:")
        for path in sorted(references):
            lines = ", ".join(references[path])
            print(f"  {path}  line(s) {lines}")

    if args.dry_run:
        print(f"\ndry run: {len(moves)} file(s) would move, nothing changed")
        return 0

    print(f"\nmoved {len(moves)} file(s)")
    if not args.keep_empty_dirs:
        for relative in prune_empty_dirs(root, moves):
            print(f"removed empty directory {relative}")
    if tracked:
        print("git mv stages the renames; review with 'git status' before committing")
    print("re-run CMake so the source glob and module dependencies are rebuilt")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
