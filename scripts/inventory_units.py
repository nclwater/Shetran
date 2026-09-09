#!/usr/bin/env python3
"""Inventory Fortran program units and module-level variables for the rename proposal.

Scans the in-scope SHETRAN sources and emits three CSV files describing every
procedure, every derived-type definition and every module-level variable, with
line ranges and the extent of the preceding documentation block.
"""

import csv
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from fortran_lex import logical_lines, split_names  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"

EXCLUDE_DIRS = {"visualisation", "resource"}
EXCLUDE_FILES = {"Shetran.f90"}

RE_MODULE = re.compile(r"^\s*module\s+(\w+)\s*$", re.I)
RE_END_MODULE = re.compile(r"^\s*end\s*module\b", re.I)
RE_CONTAINS = re.compile(r"^\s*contains\s*$", re.I)
RE_INTERFACE = re.compile(r"^\s*(abstract\s+)?interface\b", re.I)
RE_END_INTERFACE = re.compile(r"^\s*end\s*interface\b", re.I)
# derived-type-stmt: TYPE name / TYPE :: name / TYPE, attrs :: name. The
# trailing "$" keeps this from matching a TYPE(kind) entity declaration, and
# no parenthesised suffix is allowed, so parameterised derived types and
# SELECT TYPE guards would need an explicit extension (neither occurs here).
RE_TYPE = re.compile(r"^\s*type\s*(?:,[^:]*)?(?:\s*::)?\s*([a-z_]\w*)\s*$", re.I)
RE_END_TYPE = re.compile(r"^\s*end\s*type\b", re.I)

RE_SUB = re.compile(r"^\s*(?:(?:pure|elemental|recursive|module)\s+)*subroutine\s+(\w+)", re.I)
RE_FUNC = re.compile(
    r"^\s*(?:(?:pure|elemental|recursive|module|"
    r"real|integer|logical|character|double\s*precision|complex|type)"
    r"(?:\s*\([^)]*\))?\s*)*function\s+(\w+)",
    re.I,
)
RE_END_SUB = re.compile(r"^\s*end\s*subroutine\b", re.I)
RE_END_FUNC = re.compile(r"^\s*end\s*function\b", re.I)
RE_END_BARE = re.compile(r"^\s*end\s*$", re.I)

DECL_TYPES = (
    r"real|integer|logical|character|double\s*precision|complex|type"
)
RE_DECL = re.compile(
    # Attributes may contain single colons (e.g. DIMENSION(:)), so match any
    # character that is not the start of the "::" separator.
    r"^\s*(" + DECL_TYPES + r")\s*(\([^)]*\))?\s*(,(?:[^:]|:(?!:))*)?::(.*)$",
    re.I,
)
RE_DECL_NOCOLON = re.compile(
    r"^\s*(" + DECL_TYPES + r")\s*(\([^)]*\))?\s+([a-z_]\w*.*)$", re.I
)


def doc_start(lines, idx):
    """First line (1-based) of the comment block immediately above `idx` (0-based)."""
    j = idx - 1
    while j >= 0:
        s = lines[j].strip()
        if s.startswith("!") or s == "":
            j -= 1
        else:
            break
    # skip blank lines directly above the unit back down to the comment block
    k = j + 1
    while k < idx and lines[k].strip() == "":
        k += 1
    if k >= idx:
        return None
    return k + 1


def decl_tail(stmt):
    """Return the entity-decl list of a declaration statement, or None."""
    mdecl = RE_DECL.match(stmt)
    if mdecl:
        return mdecl.group(4), (mdecl.group(3) or ""), mdecl.group(1) + (mdecl.group(2) or "")
    m2 = RE_DECL_NOCOLON.match(stmt)
    if m2 and "=" not in stmt.split("::")[0]:
        return m2.group(3), "", m2.group(1) + (m2.group(2) or "")
    return None


def component_names(stmt):
    """Names declared by a derived-type component statement (empty if not one)."""
    d = decl_tail(stmt)
    if d is None:
        return []
    out = []
    for entity in split_names(d[0]):
        nm = re.match(r"([a-z_]\w*)", entity.strip(), re.I)
        if nm:
            out.append(nm.group(1))
    return out


def scan(path):
    lines = path.read_text(errors="replace").splitlines()
    rel = str(path.relative_to(ROOT))
    units, variables, types = [], [], []

    module = None
    in_contains = False
    depth_stack = []  # nested procedure stack
    interface_depth = 0
    type_depth = 0
    open_type = None  # module-level derived type currently being scanned
    module_body_end = None

    joined = list(logical_lines(lines))
    for k, (idx, code) in enumerate(joined):
        ln = idx + 1
        s = code.strip()
        if not s:
            continue

        m = RE_MODULE.match(s)
        if m and not depth_stack:
            module = m.group(1)
            in_contains = False
            continue
        if RE_END_MODULE.match(s) and not depth_stack:
            module = None
            in_contains = False
            continue

        if RE_INTERFACE.match(s):
            interface_depth += 1
            continue
        if RE_END_INTERFACE.match(s):
            interface_depth = max(0, interface_depth - 1)
            continue
        mtype = RE_TYPE.match(s)
        if mtype and not RE_END_TYPE.match(s):
            type_depth += 1
            if type_depth == 1 and module and not in_contains and not depth_stack:
                open_type = {
                    "name": mtype.group(1),
                    "module": module,
                    "file": rel,
                    "start": ln,
                    "doc_start": doc_start(lines, idx),
                    "n_components": 0,
                }
            continue
        if RE_END_TYPE.match(s):
            type_depth = max(0, type_depth - 1)
            if type_depth == 0 and open_type is not None:
                open_type["end"] = ln
                types.append(open_type)
                open_type = None
            continue

        if RE_CONTAINS.match(s) and module and not depth_stack and not type_depth:
            in_contains = True
            module_body_end = ln
            continue

        if interface_depth or type_depth:
            if type_depth and open_type is not None and not interface_depth:
                open_type["n_components"] += len(component_names(s))
            continue

        msub = RE_SUB.match(s)
        mfun = RE_FUNC.match(s) if not msub else None
        if msub or mfun:
            name = (msub or mfun).group(1)
            kind = "subroutine" if msub else "function"
            depth_stack.append(
                {
                    "name": name,
                    "kind": kind,
                    "start": ln,
                    "doc_start": doc_start(lines, idx),
                    "module": module or "",
                }
            )
            continue

        if depth_stack and (
            RE_END_SUB.match(s) or RE_END_FUNC.match(s) or RE_END_BARE.match(s)
        ):
            u = depth_stack.pop()
            u["end"] = ln
            u["file"] = rel
            u["nested"] = len(depth_stack) > 0
            units.append(u)
            continue

        # module-level variable declarations (module body, before CONTAINS)
        if module and not in_contains and not depth_stack:
            mdecl = RE_DECL.match(s)
            tail = None
            attrs = ""
            if mdecl:
                attrs = (mdecl.group(3) or "")
                tail = mdecl.group(4)
                base = mdecl.group(1) + (mdecl.group(2) or "")
            else:
                m2 = RE_DECL_NOCOLON.match(s)
                if m2 and "=" not in s.split("::")[0]:
                    base = m2.group(1) + (m2.group(2) or "")
                    tail = m2.group(3)
            if tail is not None:
                is_param = "parameter" in attrs.lower()
                for entity in split_names(tail):
                    ename = entity.strip()
                    nm = re.match(r"([a-z_]\w*)", ename, re.I)
                    if not nm:
                        continue
                    dims = ""
                    dm = re.match(r"[a-z_]\w*\s*(\([^=]*\))", ename, re.I)
                    if dm:
                        dims = dm.group(1)
                    if "dimension" in attrs.lower():
                        dm2 = re.search(r"dimension\s*(\([^)]*\))", attrs, re.I)
                        if dm2:
                            dims = dm2.group(1)
                    variables.append(
                        {
                            "name": nm.group(1),
                            "module": module,
                            "file": rel,
                            "line": ln,
                            "type": base.strip(),
                            "attrs": attrs.strip().lstrip(",").strip(),
                            "dims": dims,
                            "is_parameter": is_param,
                            "has_init": "=" in ename,
                            "doc": (
                                lines[idx].split("!!", 1)[1].strip()
                                if "!!" in lines[idx]
                                else ""
                            ),
                        }
                    )
    return units, variables, types


def main():
    files = sorted(
        p
        for p in SRC.rglob("*")
        if p.suffix in (".f90", ".F90")
        and not (set(p.relative_to(SRC).parts) & EXCLUDE_DIRS)
        and p.name not in EXCLUDE_FILES
    )
    all_units, all_vars, all_types = [], [], []
    for p in files:
        u, v, t = scan(p)
        all_units += u
        all_vars += v
        all_types += t

    out = ROOT / "docs" / "rename"
    out.mkdir(parents=True, exist_ok=True)

    with (out / "_inventory_units.csv").open("w", newline="") as f:
        w = csv.DictWriter(
            f,
            fieldnames=[
                "file", "module", "name", "kind", "start", "end",
                "doc_start", "nested",
            ],
        )
        w.writeheader()
        for u in sorted(all_units, key=lambda x: (x["file"], x["start"])):
            w.writerow(u)

    with (out / "_inventory_vars.csv").open("w", newline="") as f:
        w = csv.DictWriter(
            f,
            fieldnames=[
                "file", "module", "name", "line", "type", "attrs", "dims",
                "is_parameter", "has_init", "doc",
            ],
        )
        w.writeheader()
        for v in sorted(all_vars, key=lambda x: (x["file"], x["line"])):
            w.writerow(v)

    with (out / "_inventory_types.csv").open("w", newline="") as f:
        w = csv.DictWriter(
            f,
            fieldnames=[
                "file", "module", "name", "start", "end", "doc_start",
                "n_components",
            ],
        )
        w.writeheader()
        for t in sorted(all_types, key=lambda x: (x["file"], x["start"])):
            w.writerow(t)

    print(
        f"files={len(files)} units={len(all_units)} "
        f"vars={len(all_vars)} types={len(all_types)}"
    )
    from collections import Counter
    c = Counter(u["file"] for u in all_units)
    for k, n in c.most_common():
        print(f"  {n:4d}  {k}")


if __name__ == "__main__":
    sys.exit(main())
