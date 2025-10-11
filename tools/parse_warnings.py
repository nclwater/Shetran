#!/usr/bin/env python3
import re
import csv
from pathlib import Path

log_path = Path(__file__).resolve().parents[1] / 'compile_warnings.log'
out_path = Path(__file__).resolve().parents[1] / 'compile_warnings.csv'

# Regex to match file location lines like: /path/to/file.f90:32:31:
header_re = re.compile(r'^(?P<file>/.+?):(?P<line>\d+):(?P<col>\d+):')

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
        
        # Search for the Warning line in the next few lines
        warning_found = False
        for offset in range(1, 15):
            if i + offset >= len(lines):
                break
            wline = lines[i + offset].strip()
            if wline.startswith('Warning:'):
                warning_found = True
                
                # Extract warning code from square brackets at the end
                warning_code = 'unknown'
                bracket_match = re.search(r'\[(-W[^\]]+)\]', wline)
                if bracket_match:
                    warning_code = bracket_match.group(1)
                
                # Map warning code to descriptive type
                warning_type_map = {
                    '-Wunused-value': 'unused_variable',
                    '-Wunused-variable': 'unused_variable',
                    '-Wunused-dummy-argument': 'unused_argument',
                    '-Wunused-parameter': 'unused_parameter',
                    '-Wunused-function': 'unused_function',
                    '-Wunused-label': 'unused_label',
                    '-Wmaybe-uninitialized': 'uninitialized_variable',
                    '-Wcharacter-truncation': 'string_truncation',
                    '-Wcompare-reals': 'real_comparison',
                    '-Wconversion': 'type_conversion',
                    '-Winteger-division': 'integer_division',
                    '-Wdo-subscript': 'array_bounds',
                    '-Wintrinsic-shadow': 'intrinsic_shadow',
                    '-Wsurprising': 'surprising_behavior',
                }
                warning_type = warning_type_map.get(warning_code, warning_code)
                
                # Extract variable name - try multiple patterns
                variable = 'unknown'
                
                # Pattern 1: Any type of quoted variable (Unicode curly or ASCII quotes)
                # Matches: 'var', 'var', "var", `var`, including arrays like jface2[1]
                # Use Unicode code points for curly quotes: \u2018 (') and \u2019 (')
                quote_patterns = [
                    r"[\u2018\u2019']([A-Za-z0-9_]+(?:\[[0-9]+\])?)[\u2018\u2019']",  # Unicode/ASCII single quotes
                    r"`([A-Za-z0-9_]+(?:\[[0-9]+\])?)`",  # Backticks
                    r'"([A-Za-z0-9_]+(?:\[[0-9]+\])?)"',  # Double quotes
                ]
                
                for pattern in quote_patterns:
                    match = re.search(pattern, wline)
                    if match:
                        variable = match.group(1)
                        break
                
                # Pattern 2: Look for specific warning types without quoted variables
                if variable == 'unknown':
                    if 'CHARACTER expression will be truncated' in wline:
                        variable = 'CHARACTER'
                    elif 'Array reference' in wline:
                        variable = 'array'
                    elif 'Equality comparison for REAL' in wline:
                        variable = 'REAL'
                    elif 'Integer division truncated' in wline:
                        variable = 'integer'
                    elif 'Change of value in conversion' in wline:
                        variable = 'conversion'
                    elif 'out of bounds' in wline:
                        variable = 'bounds'
                
                entries.append((file, line_no, variable, warning_type, warning_code))
                break
        
        i += 1
    else:
        i += 1

# Write CSV
with out_path.open('w', encoding='utf-8', newline='') as csvf:
    writer = csv.writer(csvf)
    writer.writerow(['file', 'line', 'variable', 'warning_type', 'warning_code'])
    for (fpath, lno, var, wtype, wcode) in entries:
        writer.writerow([fpath, lno, var, wtype, wcode])

print(f'Wrote {len(entries)} entries to {out_path}')
