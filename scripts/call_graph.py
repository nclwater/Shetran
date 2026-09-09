#!/usr/bin/env python3
"""Build the intra-module call graph for the large SHETRAN modules.

Splitting one module into several sibling modules turns internal calls into
inter-module USE dependencies, so any call cycle between proposed groups would
be a hard blocker. This reports, per module, which procedures each procedure
calls or references.
"""

import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "docs" / "rename"


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


def main():
    units = list(csv.DictReader((OUT / "_inventory_units.csv").open()))
    by_file = defaultdict(list)
    for u in units:
        by_file[u["file"]].append(u)

    target = sys.argv[1:] or sorted({u["file"] for u in units})

    for fname in target:
        us = [u for u in by_file[fname] if u["nested"] == "False"]
        if len(us) < 2:
            continue
        lines = (ROOT / fname).read_text(errors="replace").splitlines()
        names = {u["name"].lower(): u["name"] for u in us}
        edges = defaultdict(set)
        for u in us:
            lo, hi = int(u["start"]), int(u["end"])
            body = " ".join(strip_comment(l) for l in lines[lo:hi])
            for t in re.findall(r"[A-Za-z_]\w*", body):
                tl = t.lower()
                if tl in names and tl != u["name"].lower():
                    edges[u["name"]].add(names[tl])
        print(f"===== {fname}")
        for u in us:
            callees = sorted(edges[u["name"]])
            if callees:
                print(f"  {u['name']:<32} -> {', '.join(callees)}")
        # report mutual pairs, which constrain how groups may be cut
        mutual = {
            tuple(sorted((a, b)))
            for a in edges for b in edges[a] if a in edges.get(b, ())
        }
        if mutual:
            print(f"  [mutual] {sorted(mutual)}")


if __name__ == "__main__":
    main()
