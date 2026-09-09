#!/usr/bin/env python3
"""Map which source files reference each module-level variable and procedure.

Reads the inventory CSVs produced by `inventory_units.py` and scans every
Fortran source (including the out-of-scope visualisation tree, because its
references still constrain what may move) for identifier tokens. Emits usage
CSVs used to decide the target file for each entity.

A token only counts as a reference when the referencing file can actually see
the name, so USE statements are parsed rather than merely detected:

  * `USE m, ONLY: a, b`  imports exactly `a` and `b` -> a hit on either is
    *confirmed*: the name is demonstrably the owning module's.
  * `USE m, ONLY: x => y` imports `y` under the local name `x`, so the token
    to look for is `x` while the entity referenced is `y`.
  * `USE m` imports the whole public namespace -> a hit is only *possible*:
    the token may equally be a local, a dummy argument or an unrelated
    same-named entity. Legacy names such as S, U, CP and PE collide freely.
  * identifiers inside character literals are text, not references, so
    FORMAT strings do not contribute tokens.
  * use-association is transitive: a bare `USE m` also re-exports whatever `m`
    itself imported, unless `m` declares a default `PRIVATE`. Without this the
    ONLY-strict rule would lose real references (`CMmod` reaches `sglobal`'s
    capacity parameters through `AL_C`).

The confirmed and possible counts are reported separately: confirmed is a
lower bound on the true reference set, confirmed+possible an upper bound.
"""

import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from fortran_lex import logical_lines, strip_strings  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
OUT = ROOT / "docs" / "rename"

RE_USE = re.compile(
    r"^\s*use\s*(?:,\s*(?:non_)?intrinsic\s*)?(?:::)?\s*([a-z_]\w*)\s*(,.*)?$", re.I
)
RE_ONLY = re.compile(r"^\s*,\s*only\s*:(.*)$", re.I)
RE_RENAME = re.compile(r"^([a-z_]\w*)\s*=>\s*([a-z_]\w*)$", re.I)
RE_PLAIN = re.compile(r"^([a-z_]\w*)$", re.I)
RE_MODULE = re.compile(r"^\s*module\s+(\w+)\s*$", re.I)
RE_END_MODULE = re.compile(r"^\s*end\s*module\b", re.I)
RE_PRIVATE = re.compile(r"^\s*private\s*$", re.I)

# an ONLY list may name generic interfaces, so tolerate operator/assignment
RE_OPERATOR = re.compile(r"^(?:operator|assignment)\s*\(", re.I)

MAX_REEXPORT_DEPTH = 6


def parse_only_list(text):
    """Return (imported original names, {original: {local aliases}})."""
    names, alias = set(), defaultdict(set)
    for item in text.split(","):
        item = item.strip()
        if not item or RE_OPERATOR.match(item):
            continue
        ren = RE_RENAME.match(item)
        if ren:
            local, orig = ren.group(1).lower(), ren.group(2).lower()
            names.add(orig)
            alias[orig].add(local)
            continue
        plain = RE_PLAIN.match(item)
        if plain:
            names.add(plain.group(1).lower())
    return names, alias


def scan_sources():
    """Return (tokens, file_uses, mod_imports, mod_private)."""
    files = sorted(p for p in SRC.rglob("*") if p.suffix in (".f90", ".F90"))

    tokens = defaultdict(set)          # token -> files mentioning it in code
    file_uses = defaultdict(dict)      # file -> module -> {all, only, alias}
    mod_imports = defaultdict(set)     # module -> {(owner, name-or-"*")}
    mod_private = set()                # modules with a bare PRIVATE statement

    for p in files:
        rel = str(p.relative_to(ROOT))
        cur_module = None
        for _, code in logical_lines(p.read_text(errors="replace").splitlines()):
            if not code.strip():
                continue

            mmod = RE_MODULE.match(code)
            if mmod:
                cur_module = mmod.group(1).lower()
            elif RE_END_MODULE.match(code):
                cur_module = None
            elif cur_module and RE_PRIVATE.match(code):
                mod_private.add(cur_module)

            muse = RE_USE.match(code)
            if muse:
                mod = muse.group(1).lower()
                rest = muse.group(2) or ""
                entry = file_uses[rel].setdefault(
                    mod, {"all": False, "only": set(), "alias": defaultdict(set)}
                )
                monly = RE_ONLY.match(rest)
                if monly:
                    names, alias = parse_only_list(monly.group(1))
                    entry["only"] |= names
                    for orig, locals_ in alias.items():
                        entry["alias"][orig] |= locals_
                    if cur_module:
                        for nm in names:
                            mod_imports[cur_module].add((mod, nm))
                else:
                    entry["all"] = True
                    # rename-only form: USE m, x => y
                    _, alias = parse_only_list(rest.lstrip(","))
                    for orig, locals_ in alias.items():
                        entry["alias"][orig] |= locals_
                    if cur_module:
                        mod_imports[cur_module].add((mod, "*"))

            for t in re.findall(r"[A-Za-z_]\w*", strip_strings(code)):
                tokens[t.lower()].add(rel)

    return tokens, file_uses, mod_imports, mod_private


def make_reexport_test(mod_imports, mod_private):
    """Is `owner::name` visible to a file that bare-USEs `mod`?"""
    memo = {}

    def reexports(mod, owner, name, depth=0):
        key = (mod, owner, name)
        if key in memo:
            return memo[key]
        if depth >= MAX_REEXPORT_DEPTH or mod in mod_private:
            return False
        memo[key] = False  # cycle guard
        result = False
        for om, nm in mod_imports.get(mod, ()):
            if om == owner and nm in (name, "*"):
                result = True
                break
            if nm == "*" and reexports(om, owner, name, depth + 1):
                result = True
                break
        memo[key] = result
        return result

    return reexports


def main():
    tokens, file_uses, mod_imports, mod_private = scan_sources()
    reexports = make_reexport_test(mod_imports, mod_private)

    def references(name, owner, owner_file):
        """Return (confirmed files, possible files) referencing `owner::name`."""
        nm = name.lower()
        confirmed, possible = set(), set()
        for f, used in file_uses.items():
            if f == owner_file:
                continue
            direct = used.get(owner)
            visible_names = {nm}
            level = None

            if direct is not None:
                visible_names |= direct["alias"].get(nm, set())
                if nm in direct["only"]:
                    level = "confirmed"
                elif direct["all"]:
                    level = "possible"

            if level is None:
                # transitive: a bare USE of some module that re-exports the name
                for other, entry in used.items():
                    if other == owner:
                        continue
                    if entry["all"] and reexports(other, owner, nm):
                        level = "possible"
                        break
                    # an explicit ONLY import may still name a re-exported entity
                    if nm in entry["only"] and reexports(other, owner, nm):
                        visible_names |= entry["alias"].get(nm, set())
                        level = "confirmed"
                        break

            if level is None:
                continue
            if any(f in tokens.get(v, ()) for v in visible_names):
                (confirmed if level == "confirmed" else possible).add(f)
        return confirmed, possible

    def usage_rows(inv_name, out_name, extra_fields):
        rows = list(csv.DictReader((OUT / inv_name).open()))
        n_conf = n_poss = 0
        with (OUT / out_name).open("w", newline="") as f:
            w = csv.writer(f)
            w.writerow(
                ["name", "module", "owner_file"] + extra_fields
                + ["n_refs_confirmed", "refs_confirmed",
                   "n_refs_possible", "refs_possible"]
            )
            for r in rows:
                conf, poss = references(r["name"], r["module"].lower(), r["file"])
                n_conf += len(conf)
                n_poss += len(poss)
                w.writerow(
                    [r["name"], r["module"], r["file"]]
                    + [r.get(k, "") for k in extra_fields]
                    + [len(conf), ";".join(sorted(conf)),
                       len(poss), ";".join(sorted(poss))]
                )
        print(f"  {out_name}: {len(rows)} rows, "
              f"{n_conf} confirmed / {n_poss} possible references")
        return rows

    usage_rows(
        "_inventory_vars.csv", "_usage_vars.csv",
        ["line", "type", "is_parameter"],
    )
    usage_rows(
        "_inventory_units.csv", "_usage_units.csv",
        ["kind", "start", "end", "nested"],
    )
    usage_rows(
        "_inventory_types.csv", "_usage_types.csv",
        ["start", "end", "n_components"],
    )

    # module -> which files USE it, and how many do so without an ONLY list
    with (OUT / "_usage_modules.csv").open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["module", "n_users", "n_bare_users", "users", "bare_users"])
        users, bare = defaultdict(set), defaultdict(set)
        for fl, used in file_uses.items():
            for m, entry in used.items():
                users[m].add(fl)
                if entry["all"]:
                    bare[m].add(fl)
        for m in sorted(users):
            w.writerow([
                m, len(users[m]), len(bare[m]),
                ";".join(sorted(users[m])), ";".join(sorted(bare[m])),
            ])

    print("wrote _usage_vars.csv, _usage_units.csv, _usage_types.csv, "
          "_usage_modules.csv")


if __name__ == "__main__":
    main()
