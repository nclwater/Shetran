#!/usr/bin/env python3
"""Shared free-form Fortran lexing helpers for the rename-proposal scripts.

`inventory_units.py` and `analyse_usage.py` both need to see statements rather
than raw lines: comments stripped outside string literals, and continuation
lines joined. Keeping one copy means the two scripts cannot drift apart and
disagree about what a source file says.
"""


def strip_comment(line):
    """Return the code part of a line, dropping trailing comments outside strings."""
    out = []
    quote = None
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


def strip_strings(code):
    """Blank out character-literal contents, keeping the quotes as delimiters.

    Identifiers inside a literal are text, not references: FORMAT strings such
    as `"('0', 1X, 'ET COMPONENT')"` otherwise register as uses of `ET`.
    """
    out = []
    quote = None
    for ch in code:
        if quote:
            if ch == quote:
                quote = None
                out.append(ch)
            else:
                out.append(" ")
        elif ch in "'\"":
            quote = ch
            out.append(ch)
        else:
            out.append(ch)
    return "".join(out)


def logical_lines(lines):
    """Yield (start_idx, joined_code) for continuation-joined statements."""
    i = 0
    n = len(lines)
    while i < n:
        code = strip_comment(lines[i]).rstrip()
        start = i
        while code.endswith("&"):
            code = code[:-1]
            i += 1
            if i >= n:
                break
            nxt = strip_comment(lines[i]).strip()
            if nxt.startswith("&"):
                nxt = nxt[1:]
            code += " " + nxt.rstrip()
        yield start, code
        i += 1


def split_names(decl_tail):
    """Split an entity-decl or ONLY list on commas at paren/bracket depth zero."""
    names, depth, cur = [], 0, ""
    quote = None
    for ch in decl_tail:
        if quote:
            cur += ch
            if ch == quote:
                quote = None
            continue
        if ch in "'\"":
            quote = ch
            cur += ch
        elif ch in "([":
            depth += 1
            cur += ch
        elif ch in ")]":
            depth -= 1
            cur += ch
        elif ch == "," and depth == 0:
            names.append(cur)
            cur = ""
        else:
            cur += ch
    if cur.strip():
        names.append(cur)
    return names
