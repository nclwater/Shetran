#!/usr/bin/env python3
"""One-shot: wire ERRMSG= text through the errstat_alloc / errstat_dealloc sites.

For every `CALL errstat_alloc(<v>, ...)` / `CALL errstat_dealloc(<v>, ...)` the
preceding (DE)ALLOCATE statement gets `, ERRMSG=emsg` and the call gets a
trailing `, emsg` argument. A `CHARACTER(LEN=LENGTH_LINE) :: emsg` declaration is
inserted after the routine's `ios` (or `status`) declaration.
"""
import re
import sys

FILES = [
    "src/parameters/colm_cg.f90",
    "src/parameters/colm_co.f90",
    "src/parameters/CONT_CC.F90",
    "src/parameters/AL_C.F90",
    "src/modules/ZQmod.f90",
    "src/modules/OCmod2.f90",
    "src/modules/OCmod.f90",
    "src/modules/rest.f90",
    "src/modules/run_sim.f90",
    "src/modules/ETmod.f90",
    "src/modules/SYmod.f90",
    "src/modules/SMmod.f90",
    "src/modules/MNmod.f90",
    "src/modules/VSmod.f90",
    "src/modules/FRmod.f90",
    "src/visualisation/visualisation_interface_right.f90",
    "src/visualisation/visualisation_interface_centre.f90",
    "src/visualisation/visualisation_pass.f90",
    "src/visualisation/visualisation_map.f90",
    "src/visualisation/visualisation_structure.f90",
    "src/visualisation/visualisation_extras.f90",
    "src/visualisation/visualisation_hdf5.f90",
    "src/visualisation/visualisation_read_parser.f90",
    "src/visualisation/visualisation_metadata.f90",
]

CALL_RE = re.compile(r'^\s*CALL\s+errstat_(?:alloc|dealloc)\s*\(\s*([A-Za-z_]\w*)\s*,', re.I)
DECL_RE = re.compile(r'^(\s*)INTEGER\(KIND=I_P\)\s*::\s*(ios|status)\s*(!!.*)?$', re.I)
ROUTINE_END_RE = re.compile(r'^\s*END\s+(SUBROUTINE|FUNCTION)\b', re.I)
ROUTINE_HEAD_RE = re.compile(
    r'^\s*((RECURSIVE|PURE|IMPURE|ELEMENTAL|MODULE)\s+)*(SUBROUTINE|FUNCTION)\s+\w+', re.I)
TYPED_FUNC_RE = re.compile(r'^\s*[A-Za-z].*\bFUNCTION\s+\w+\s*\(', re.I)
PARAM_ONLY_RE = re.compile(r'(USE\s+MOD_PARAMETERS\s*,\s*ONLY\s*:)(?!.*\bLENGTH_LINE\b)', re.I)


def ins_before_last_paren(line, text):
    core = line.rstrip('\n')
    idx = core.rfind(')')
    assert idx != -1, line
    return core[:idx] + text + core[idx:] + '\n'


def process(path):
    with open(path, encoding='utf-8') as fh:
        lines = fh.readlines()

    decl_idx = None
    hit_decls = set()
    edits = {}  # idx -> new line
    errors = []

    for i, ln in enumerate(lines):
        if ROUTINE_END_RE.match(ln):
            decl_idx = None
            continue
        if ROUTINE_HEAD_RE.match(ln) or TYPED_FUNC_RE.match(ln):
            decl_idx = None
            continue
        if DECL_RE.match(ln):
            decl_idx = i
            continue
        m = CALL_RE.match(ln)
        if not m:
            continue
        var = m.group(1)
        prev = i - 1
        if not lines[prev].rstrip().endswith(')'):
            errors.append(f"{path}:{i+1}: previous line is not a closed (de)allocate: {lines[prev].rstrip()}")
            continue
        if not re.search(rf'STAT\s*=\s*{re.escape(var)}\b', lines[prev], re.I):
            errors.append(f"{path}:{i+1}: previous line has no STAT={var}: {lines[prev].rstrip()}")
            continue
        if decl_idx is None:
            errors.append(f"{path}:{i+1}: no ios/status declaration found in scope")
            continue
        edits[prev] = ins_before_last_paren(edits.get(prev, lines[prev]), ', ERRMSG=emsg')
        edits[i] = ins_before_last_paren(lines[i], ', emsg')
        hit_decls.add(decl_idx)

    if errors:
        print('\n'.join(errors))
        raise SystemExit(f"ABORT {path}: {len(errors)} problem(s)")

    # decide whether LENGTH_LINE needs importing, based on the ORIGINAL use lines
    use_lines = [ln for ln in lines if re.match(r'\s*USE\s+MOD_PARAMETERS\b', ln, re.I)]
    has_length_line = any(re.search(r'\bLENGTH_LINE\b(?!\w)', ln) for ln in use_lines)
    has_unqualified_use = any(re.match(r'\s*USE\s+MOD_PARAMETERS\s*(!.*)?$', ln, re.I) for ln in use_lines)
    need_import = not has_length_line and not has_unqualified_use

    for idx, new in edits.items():
        lines[idx] = new

    # insert emsg declarations after each hit ios/status declaration (descending)
    for idx in sorted(hit_decls, reverse=True):
        indent = DECL_RE.match(lines[idx]).group(1)
        lines.insert(idx + 1, f"{indent}CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.\n")

    # ensure LENGTH_LINE is imported
    if need_import:
        for i, ln in enumerate(lines):
            m = PARAM_ONLY_RE.search(ln)
            if m:
                lines[i] = ln[:m.end()] + ' LENGTH_LINE,' + ln[m.end():]
                break
        else:
            print(f"  NOTE {path}: could not add LENGTH_LINE import automatically")

    with open(path, 'w', encoding='utf-8') as fh:
        fh.writelines(lines)
    print(f"  {path}: {len(edits)//2} sites, {len(hit_decls)} emsg decls")


if __name__ == '__main__':
    for f in FILES:
        process(f)
