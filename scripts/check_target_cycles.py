#!/usr/bin/env python3
"""Check the proposed module split for circular USE dependencies.

`call_graph.py` reports mutual recursion within one source module, which is
necessary but not sufficient: after the split, two modules also depend on each
other if procedures in each call procedures in the other without being mutually
recursive, or if a procedure in one reads a module-level variable placed in the
other. This walks every procedure body in `functions.csv`, resolves every
identifier against the entities of its own source module, and reports the
strongly connected components of the resulting graph over the *proposed*
modules. Exit status is non-zero if any cycle is found.

Identifiers declared inside a procedure are ignored, so locals and dummy
arguments that happen to share a module-level name do not raise a false cycle.
"""

import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "docs" / "rename"

RE_TOKEN = re.compile(r"[A-Za-z_]\w*")


def strip_comment(line):
    out, quote = [], None
    for ch in line:
        if quote:
            out.append(ch)
            if ch == quote:
                quote = None
        elif ch in "'\"":
            quote = ch
            out.append(ch)
        elif ch == "!":
            break
        else:
            out.append(ch)
    return "".join(out)


def load_entities():
    """name (lowercased), per source module -> proposed target module."""
    entities = defaultdict(dict)
    for name_col, fname in (
        ("function_name", "functions.csv"),
        ("variable_name", "variables.csv"),
        ("type_name", "types.csv"),
    ):
        for row in csv.DictReader((OUT / fname).open()):
            entities[row["source_module"]][row[name_col].lower()] = row["target_module"]
    return entities


def build_edges(entities):
    """target module -> target module -> the references that create the edge."""
    sources = {}
    edges = defaultdict(lambda: defaultdict(set))
    for row in csv.DictReader((OUT / "functions.csv").open()):
        if row["nested_in_parent"] == "True":
            continue  # a contained procedure travels with its parent
        path = row["source_file"]
        if path not in sources:
            sources[path] = (ROOT / path).read_text(errors="replace").splitlines()
        lines = sources[path][int(row["starting_line"]) - 1:int(row["ending_line"])]
        body = [strip_comment(l) for l in lines]
        declared = {t.lower() for l in body if "::" in l for t in RE_TOKEN.findall(l)}
        home = row["target_module"]
        known = entities[row["source_module"]]
        for token in {t.lower() for t in RE_TOKEN.findall(" ".join(body))} - declared:
            target = known.get(token)
            if target and target != home:
                edges[home][target].add(f"{row['function_name']} -> {token}")
    return edges


def cycles(edges):
    """Strongly connected components with more than one member (Tarjan)."""
    index, low, stack, on_stack, found = {}, {}, [], set(), []
    counter = [0]

    def visit(v):
        index[v] = low[v] = counter[0]
        counter[0] += 1
        stack.append(v)
        on_stack.add(v)
        for w in edges.get(v, ()):
            if w not in index:
                visit(w)
                low[v] = min(low[v], low[w])
            elif w in on_stack:
                low[v] = min(low[v], index[w])
        if low[v] == index[v]:
            component = []
            while True:
                w = stack.pop()
                on_stack.discard(w)
                component.append(w)
                if w == v:
                    break
            if len(component) > 1:
                found.append(sorted(component))

    sys.setrecursionlimit(10000)
    for v in list(edges):
        if v not in index:
            visit(v)
    return found


def main():
    edges = build_edges(load_entities())
    found = cycles(edges)
    if not found:
        modules = set(edges) | {t for e in edges.values() for t in e}
        print(f"No cycles among the {len(modules)} proposed modules that have edges.")
        return 0
    for component in found:
        print("CYCLE: " + ", ".join(component))
        for a in component:
            for b in component:
                if a != b and b in edges[a]:
                    print(f"  {a} -> {b}: {', '.join(sorted(edges[a][b])[:6])}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
