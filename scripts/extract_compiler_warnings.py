#
# Extract compiler warnings from the build log and save them to a separate file.
#
# Usage:
#   python extract_compiler_warnings.py <build_log_file> <output_warnings_file>
#
# When running build from the command line, you can redirect the build output
# to a log file like this:
#
# (Linux/Mac)
#   make 2>&1 | tee build.log
#
# (Windows)
#   build_command > build.log 2>&1
#
# (C) 2026, Sven Berendsen
#
# TODO The logic can be a lot more elegant than this brute force approach.
# TODO Currently it is setup for ifx output, support others as well.
#

# General Imports
import argparse

# Package Imports
import pandas as pd


def normalise_to_src_path(path):
    """Normalise path separators and keep path from the src directory onward."""
    normalised = path.strip().replace("\\", "/")
    parts = [part for part in normalised.split("/") if part]

    src_index = next(
        (i for i, part in enumerate(parts) if part.lower() == "src"), None)
    if src_index is not None:
        return "/".join(parts[src_index:])

    return "/".join(parts)


def main():

    # parse the arguments
    parser = argparse.ArgumentParser(
        description="Extract compiler warnings from a build log.")
    parser.add_argument("build_log_file", help="Path to the build log file")
    parser.add_argument("output_warnings_file",
                        help="Path to the output file for warnings")

    args = parser.parse_args()

    # read the build log file
    with open(args.build_log_file, "r") as f:
        build_log = f.readlines()

    # extract compiler warnings
    warnings = []
    for line in build_log:
        line_lower = line.lower()
        if ("warning" in line_lower and "src" in line_lower
                and "f90" in line_lower and "build" not in line_lower):

            # extract relevant parts of the warning message
            parts_double_colon = line.split("warning #")
            if len(parts_double_colon) < 2:
                continue

            # get filename and line number from the first part of the warning message
            location_part = parts_double_colon[0].strip()
            if "(" not in location_part or ")" not in location_part:
                continue

            file_part, line_part = location_part.rsplit("(", 1)
            fn = normalise_to_src_path(file_part)
            line_num = line_part.split(")", 1)[0].strip()

            # next exctract the warning id & body from the second part of the warning message
            warning_id = parts_double_colon[1].split(": ")[0].strip()
            warning_msg = ": ".join(
                parts_double_colon[1].split(": ")[1:]).strip()

            warnings.append({
                "filename": fn,
                "typus": "warning",
                "line_number": line_num,
                "warning_id": warning_id,
                "warning_message": warning_msg
            })
        elif ("remark" in line_lower and "src" in line_lower
              and "f90" in line_lower and "build" not in line_lower):

            # extract relevant parts of the warning message
            parts_double_colon = line.split("remark #")
            if len(parts_double_colon) < 2:
                continue

            # get filename and line number from the first part of the warning message
            location_part = parts_double_colon[0].strip()
            if "(" not in location_part or ")" not in location_part:
                continue

            file_part, line_part = location_part.rsplit("(", 1)
            fn = normalise_to_src_path(file_part)
            line_num = line_part.split(")", 1)[0].strip()

            # next exctract the warning id & body from the second part of the warning message
            warning_id = parts_double_colon[1].split(": ")[0].strip()
            warning_msg = ": ".join(
                parts_double_colon[1].split(": ")[1:]).strip()

            warnings.append({
                "filename": fn,
                "typus": "remark",
                "line_number": line_num,
                "warning_id": warning_id,
                "warning_message": warning_msg
            })

    # save warnings to output file
    df = pd.DataFrame(warnings)
    df.to_csv(args.output_warnings_file, index=False)


if __name__ == "__main__":
    main()
