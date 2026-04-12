#
# Main method for comparing the results files.
#

# General Imports
import difflib
import os

# Package Imports
from tqdm import tqdm
from typing import Any

# Local Imports
from . import settings as settings
from . import compare_hdf5 as compare_hdf5
from . import compare_table as compare_table


def compare_textfile(fn_should: str, fn_is: str, fn_delta: str) -> dict:
    # TODO make a text-file-content specific comparison function(s)

    flag_difference = False

    # Read files and normalize lines
    with open(fn_should, "r", encoding="utf-8") as f_should:
        # rstrip() removes trailing spaces, tabs, \r, and \n.
        # Adding '\n' back ensures the diff output remains cleanly formatted.
        should_lines = [line.rstrip() + "\n" for line in f_should]

    with open(fn_is, "r", encoding="utf-8") as f_is:
        is_lines = [line.rstrip() + "\n" for line in f_is]

    # Generate the unified diff
    # n=3 provides 3 lines of context around each change, matching git's default
    diff = list(
        difflib.unified_diff(should_lines,
                             is_lines,
                             fromfile=fn_should,
                             tofile=fn_is,
                             n=3))

    # If the diff list is not empty, differences exist
    if diff:
        os.makedirs(os.path.dirname(fn_delta), exist_ok=True)
        with open(fn_delta, "w", encoding="utf-8") as f_delta:
            f_delta.writelines(diff)
        flag_difference = True

    return {"files_different": flag_difference}


def compare_results(dir_should: str, dir_is: str, dir_diff: str) -> dict:

    # get the files in the should and is directories (sorted)
    files_should = sorted([
        f for f in os.listdir(dir_should)
        if os.path.isfile(os.path.join(dir_should, f))
    ])
    files_is = sorted([
        f for f in os.listdir(dir_is)
        if os.path.isfile(os.path.join(dir_is, f))
    ])

    # prep
    # TODO track if a difference exists
    res = {}

    # comparison loop
    pbar = tqdm(files_should)
    for file in pbar:
        pbar.set_description(f"Comparing {file}")

        # first check if the file exists in the is directory
        if file not in files_is:
            res[file] = {"status": "missing"}
            continue

        # do a file size comparison
        size_should = os.path.getsize(os.path.join(dir_should, file))
        size_is = os.path.getsize(os.path.join(dir_is, file))

        # if the file is too large, skip it (for now)
        fn_should = os.path.join(dir_should, file)
        fn_is = os.path.join(dir_is, file)

        # prep
        res_tmp: dict[str, Any] = {
            "file_size_comparison": (size_should == size_is)
        }
        fn_delta = os.path.join(dir_diff, f"delta_{file}")

        # compare
        if file.endswith(".txt") or file.endswith(".csv"):
            # work depending on it being a table or not
            if any(ind in file for ind in settings.list_table_files_indicators
                   ) or file.endswith(".csv"):
                if (size_should > settings.files_too_large_threshold_table
                        or size_is > settings.files_too_large_threshold_table):
                    res[file] = {
                        "status": "too large",
                        "file_size_comparison": (size_should == size_is),
                    }
                    continue

                res_tmp["file_type"] = "table"
                res_tmp.update(
                    compare_table.compare_table(fn_should, fn_is, fn_delta))
            else:
                if (size_should > settings.files_too_large_threshold_text
                        or size_is > settings.files_too_large_threshold_text):
                    res[file] = {
                        "status": "too large",
                        "file_size_comparison": (size_should == size_is),
                    }
                    continue
                res_tmp["file_type"] = "text"
                res_tmp.update(compare_textfile(fn_should, fn_is, fn_delta))

        elif file.endswith(".hdf5") or file.endswith(".h5"):

            if (size_should > settings.files_too_large_threshold_hdf5
                    or size_is > settings.files_too_large_threshold_hdf5):
                res[file] = {
                    "status": "too large",
                    "file_size_comparison": (size_should == size_is),
                }
                continue

            res_tmp["file_type"] = "hdf5"
            res_tmp.update(
                compare_hdf5.compare_hdf5(fn_should, fn_is, fn_delta))
            if not res_tmp["file_size_comparison"]:
                res_tmp["size_should_bytes"] = size_should
                res_tmp["size_is_bytes"] = size_is
                res_tmp["files_different"] = True

        else:
            raise ValueError(f"  Unrecognized file type for {file}.")

        res[file] = res_tmp

    return res
