#!/usr/bin/env python3
"""Draft the ``USE ..., ONLY:`` lines a newly split module needs.

``rename_extract.py`` writes a target module's declarations and procedures but
leaves a ``! TODO USE ..., ONLY: ...`` marker, because which imports the block
needs depends on what its bodies reference. This works that out.

The candidate names are deliberately narrow, so that a local variable that
happens to share a name with some module variable elsewhere in the tree cannot
turn into a spurious import:

* every name the **source** module imported, followed through the move tables to
  whichever module now owns it -- if the source could see it, the piece split
  out of the source may still need it;
* every entity the move tables place in **another target module**, which is how
  the new intra-component edges (``error_status`` -> ``error_reporting``) are
  found;
* names the source imported from a module outside the tables (stdlib, the
  visualisation modules), carried over unchanged.

A name is reported only if the target's own body references it and the target
does not own it. The result is a draft: it is what the compiler then confirms or
corrects, exactly as when the lines are written by hand.

    python3 scripts/rename_imports.py --source src/util/mod_error.f90 \
        src/util/error/error_reporting.f90 src/util/error/error_status.f90
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
USE_ONLY = re.compile(r"^\s*USE\s+([A-Za-z0-9_]+)\s*,\s*ONLY\s*:\s*(.*)$", re.I)
USE_BARE = re.compile(r"^\s*USE\s+([A-Za-z0-9_]+)\s*(!.*)?$", re.I)


def load():
    """name.lower() -> (target_module, name); and source_file -> its entities."""
    owner, by_file = {}, {}
    for table, key in TABLES.items():
        for row in csv.DictReader((ROOT / "docs" / "rename" / table).open()):
            new = row.get("proposed_rename") or row[key]
            owner[new.lower()] = (row["target_module"], new)
            owner.setdefault(row[key].lower(), (row["target_module"], new))
            by_file.setdefault(row["source_file"], set()).add(new.lower())
    return owner, by_file


def statements(lines):
    """Every logical USE statement in ``lines``, continuations joined."""
    out, i = [], 0
    while i < len(lines):
        text, j = lines[i], i
        while text.rstrip().endswith("&"):
            j += 1
            text = text.rstrip().rstrip("&") + lines[j].strip().lstrip("&")
        if re.match(r"^\s*USE\b", text, re.I):
            out.append(text)
        i = j + 1
    return out


def imported(path):
    """{name.lower(): (module, alias_or_None, name)} from a file's USE statements."""
    found = {}
    for text in statements(path.read_text(errors="replace").splitlines()):
        m = USE_ONLY.match(text)
        if not m:
            continue
        module, only = m.groups()
        for item in only.split("!")[0].split(","):
            item = item.strip()
            if not item:
                continue
            if "=>" in item:
                alias, _, name = item.partition("=>")
                alias, name = alias.strip(), name.strip()
            else:
                alias, name = None, item
            found[name.lower()] = (module, alias, name)
    return found


def existing_modules():
    """module.lower() -> the names it declares right now.

    Checking the module exists is not enough: `oc_state` exists from step 04 but
    does not hold `OCmod2`'s `HRFZZ` until step 11, and a draft that sent a
    consumer to `oc_state` for it would not compile. So the entity itself has to
    be found in the target.
    """
    out = {}
    for f in sorted((ROOT / "src").rglob("*.[fF]90")):
        module, names = None, set()
        for line in f.read_text(errors="replace").splitlines():
            m = re.match(r"^\s*MODULE\s+([A-Za-z]\w*)\s*$", line, re.I)
            if m:
                module = m.group(1).lower()
                continue
            code = line.split("!")[0]
            m = re.match(r"^\s*(?:(?:PURE|RECURSIVE|ELEMENTAL|IMPURE|MODULE)\s+)*"
                         r"(?:\w[\w\s(),=*]*?\s+)?(?:SUBROUTINE|FUNCTION)\s+(\w+)", code, re.I)
            if m:
                names.add(m.group(1).lower())
                continue
            if re.match(r"^\s*(?:DOUBLE\s*PRECISION|REAL|INTEGER|LOGICAL|CHARACTER|TYPE\s*[(,])",
                        code, re.I):
                rhs = code.partition("::")[2] or re.sub(
                    r"^\s*(?:DOUBLE\s*PRECISION|REAL|INTEGER|LOGICAL|CHARACTER|TYPE\s*\([^)]*\))",
                    "", code, flags=re.I)
                for item in re.split(r",(?![^(]*\))", rhs):
                    mm = re.match(r"\s*([A-Za-z]\w*)", item)
                    if mm:
                        names.add(mm.group(1).lower())
        if module:
            out.setdefault(module, set()).update(names)
    return out


def declared_locally(path):
    """Names the file declares itself: locals, dummies and derived-type components.

    A candidate that the target declares is not an import -- ``ran2``'s dummy
    ``idum`` is not [[input_workspace]]'s ``IDUM``.
    """
    names = set()
    for line in path.read_text(errors="replace").splitlines():
        code = line.split("!")[0]
        if re.match(r"^\s*(?:DOUBLE\s*PRECISION|REAL|INTEGER|LOGICAL|CHARACTER|TYPE\s*\(|CLASS\s*\()",
                    code, re.I):
            rhs = code.partition("::")[2] or re.sub(
                r"^\s*(?:DOUBLE\s*PRECISION|REAL|INTEGER|LOGICAL|CHARACTER|TYPE\s*\([^)]*\))"
                r"(?:\s*\([^)]*\))?", "", code, flags=re.I)
            for item in re.split(r",(?![^(]*\))", rhs):
                m = re.match(r"\s*([A-Za-z]\w*)", item)
                if m:
                    names.add(m.group(1).lower())
        # dummy-argument lists on a procedure heading
        m = re.match(r"^\s*(?:(?:PURE|RECURSIVE|ELEMENTAL|IMPURE|MODULE)\s+)*"
                     r"(?:\w[\w\s(),=*]*?\s+)?(?:SUBROUTINE|FUNCTION)\s+\w+\s*\(([^)]*)\)",
                     code, re.I)
        if m:
            for item in m.group(1).split(","):
                item = item.strip()
                if re.fullmatch(r"[A-Za-z]\w*", item):
                    names.add(item.lower())
    return names


def body(path, own):
    """The file with comments, strings, USE lines and its own declarations gone."""
    out = []
    for line in path.read_text(errors="replace").splitlines():
        if re.match(r"^\s*(USE\b|!)", line, re.I):
            continue
        code = line.split("!")[0]
        # A declaration of a name the target owns is not a reference to it, but
        # the bounds beside it are: `INTEGER :: LCODEX(NXEE,NYEE)` still needs
        # NXEE and NYEE. Blank the declared names, keep everything else.
        decl = re.match(r"^\s*(?:DOUBLE\s*PRECISION|REAL|INTEGER|LOGICAL|CHARACTER|TYPE\s*\()", code, re.I)
        if decl:
            for n in own:
                code = re.sub(rf"(?<![\w%]){re.escape(n)}\b", " ", code, flags=re.I)
        out.append(re.sub(r"'[^']*'|\"[^\"]*\"", " ", code))
    return "\n".join(out)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("targets", nargs="+", help="target files to draft imports for")
    parser.add_argument("--source", action="append", required=True,
                        help="the file(s) the targets were extracted from (repeatable)")
    args = parser.parse_args()

    owner, by_file = load()
    present = existing_modules()

    # What each source module could see, and what it took from outside the tables.
    candidates, outside, bare, siblings = {}, {}, [], set()
    for rel in args.source:
        path = ROOT / rel
        siblings |= by_file.get(rel.removesuffix(".backup"), set())
        for name, (module, alias, original) in imported(path).items():
            if module.lower() in owner or name in owner:
                target, new = owner.get(name, (module, original))
                if name not in present.get(target.lower(), ()):
                    target, new = module, original
                candidates[new.lower()] = (target, alias, new)
            else:
                outside[name] = (module, alias, original)
        for text in statements(path.read_text(errors="replace").splitlines()):
            m = USE_BARE.match(text)
            if m:
                bare.append((rel, m.group(1)))

    for rel in args.targets:
        path = ROOT / rel
        module = next((l.split()[1] for l in path.read_text().splitlines()
                       if re.match(r"^\s*MODULE\s+\w", l, re.I)), "?")
        own = {n for n, (t, _) in owner.items() if t.lower() == module.lower()}
        own |= declared_locally(path)
        text = body(path, own)
        groups = {}

        def want(name, target, alias, shown):
            if target.lower() == module.lower() or name in own:
                return
            if re.search(rf"(?<![\w%]){re.escape(alias or name)}\b", text, re.I):
                groups.setdefault(target, set()).add(f"{alias} => {shown}" if alias else shown)

        for name, (target, alias, shown) in candidates.items():
            want(name, target, alias, shown)
        for name in siblings:                              # intra-step edges
            target, new = owner[name]
            if name in present.get(target.lower(), ()):
                want(name, target, None, new)
        for name, (mod, alias, shown) in outside.items():
            want(name, mod, alias, shown)

        print(f"\n### {rel}   (MODULE {module})")
        for target in sorted(groups):
            print(f"   USE {target}, ONLY: " + ", ".join(sorted(groups[target], key=str.lower)))
    for rel, mod in sorted(set(bare)):
        print(f"\n# NOTE {rel} took a bare USE {mod}; check by hand what the targets need from it")
    return 0


if __name__ == "__main__":
    sys.exit(main())
