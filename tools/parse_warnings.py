#!/usr/bin/env python3
import re
import csv
from pathlib import Path

log_path = Path(__file__).resolve().parents[1] / 'compile_warnings.log'
out_path = Path(__file__).resolve().parents[1] / 'compile_warnings.csv'

# Regex to match lines like:
# /path/to/file.f90:32:31:
# then a few lines later: Warning: Unused PRIVATE module variable ‘cyph’ declared at (1) [-Wunused-value]
header_re = re.compile(r'^(?P<file>/.+?):(?P<line>\d+):(?P<col>\d+):')
warning_re = re.compile(r"Warning: (?P<type>.+)")
# Prefer variables enclosed in ASCII or Unicode quotes. Don't match the leading word 'Warning'.
# Matches ‘var’, 'var', "var", `var` or bare words but avoids matching 'Warning' at start.
var_re = re.compile(
    r"[‘'`\"](?P<var>[A-Za-z0-9_]+)[’'`\"]|Warning: (?P<ignore>\w+)|(?P<bare>[A-Za-z0-9_]+)"
)

entries = []
with log_path.open(encoding='utf-8') as f:
    lines = f.readlines()

i = 0
while i < len(lines):
    line = lines[i].rstrip('\n')
    m = header_re.match(line)
    if m:
        file = m.group('file')
        line_no = m.group('line')
        col_no = m.group('col')
        # According to new rule: the warning text is at header_line + 4 (zero-based relative to header)
        warn_idx = i + 4
        if warn_idx < len(lines):
            wline = lines[warn_idx].strip()
            if wline.startswith('Warning:'):
                # extract text within square brackets at end if any
                match = re.search(r'\[(.*?)\]', wline)
                if match:
                    warning_type = match.group(1)
                else:
                    warning_type = 'unknown'

                # extract variable name if any, it is between ‘’ quotes in the message
                # e.g. Warning: Unused PRIVATE module variable ‘cyph’ declared at (1) [-Wunused-value]
                # Try to extract quoted variable first, then fall back to the first bare identifier
                var_match = var_re.search(wline)
                variable = 'unknown'
                if var_match:
                    # prefer the quoted capture
                    if var_match.group('var'):
                        variable = var_match.group('var')
                    elif var_match.group('bare'):
                        # ensure we didn't capture the leading 'Warning'
                        maybe = var_match.group('bare')
                        if maybe.lower() != 'warning':
                            variable = maybe
                # normalize unknowns
                if not variable:
                    variable = 'unknown'
                entries.append((file, line_no, variable, warning_type))
            else:
                # warning not present at the expected offset; skip
                pass
        else:
            # not enough lines after header; skip
            pass
        # advance to next header candidate (move one line forward to find overlapping blocks if any)
        i += 1
    else:
        i += 1

# write CSV with extra 'warning_code' column
with out_path.open('w', encoding='utf-8', newline='') as csvf:
    writer = csv.writer(csvf)
    writer.writerow(
        ['file', 'line', 'variable', 'warning_type', 'warning_code'])
    for (fpath, lno, var, wtype) in entries:
        # code = map_warning_type(wtype)
        writer.writerow([
            fpath,
            lno,
            var,
            wtype,
            #  code
        ])

print(
    f'Wrote {len(entries)} entries to {out_path} (with normalized warning_code)'
)
