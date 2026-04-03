#
# Checks the output files for consistency between different runs.
#
# (C) 2026, Sven Berendsen
#
# TODO Read file-type & name from the config file instead of hardcoding it here.
# TODO Write bespoke comparison functions for the non-standard text files.
# TODO Check up to which file sizes a content comparison is feasible, and implement a more efficient comparison for larger files (e.g. using hashing or chunked reading).
#

# General Imports
import argparse
import os

# Package Imports
import pandas as pd

# Local Imports
from _methods import compare as compare
from _methods import compute as compute
from _methods import settings as settings
from _methods import util as util


def _get_all_models() -> list[str]:

    # Keep order stable while deduplicating list entries.
    return list(
        dict.fromkeys(settings.list_short_runtime +
                      settings.list_medium_runtime +
                      settings.list_long_running))


def do_comparison(dir_main: str,
                  fn_shetran: str,
                  run_simulation: bool = True,
                  run_comparison: bool = True) -> dict:

    print(f"\nRunning comparison with main directory: {dir_main}")

    # set directory names
    dir_model = os.path.join(dir_main, settings.dir_inputs)
    dir_sim = os.path.join(dir_main, settings.dir_compute)
    dir_should = os.path.join(dir_main, settings.dir_results_should)
    dir_diff = os.path.join(dir_main, settings.dir_delta)

    # prep
    res_model = {}

    # run the model
    if run_simulation:

        # check that the model files are present in the inputs directory

        # run the model
        res_model.update(
            compute.run_model(fn_shetran, dir_model, dir_sim, dir_diff))

    if run_comparison:

        # compare the results with the known good ones
        res_files = compare.compare_results(dir_should, dir_sim, dir_diff)

        differing_files = sorted([
            filename for filename, file_res in res_files.items()
            if file_res.get("files_different", False)
            and file_res.get("status") != "too large"
        ])
        too_large_files = sorted([
            filename for filename, file_res in res_files.items()
            if file_res.get("status") == "too large"
        ])

        differing_files_str = ";".join(differing_files)
        too_large_files_str = ";".join(too_large_files)

        # aggregate the results
        df = pd.DataFrame.from_dict(res_files, orient="index")
        df["files_with_different_contents"] = differing_files_str
        df["files_too_large_to_compare"] = too_large_files_str
        df["num_files_with_different_contents"] = len(differing_files)
        df["num_files_too_large_to_compare"] = len(too_large_files)
        df.to_csv(os.path.join(dir_main, "comparison_results.csv"))

        # extract whether there were differences in any of the files
        res_model["any_differences"] = df["files_different"].any()

    return res_model


def main() -> None:

    # settings
    # fn_shetran = os.path.join("_official_exe", "Shetran.exe")

    parser = argparse.ArgumentParser(description=(
        "Run SHETRAN examples and/or compare their outputs against expected results."
    ))
    parser.add_argument(
        "--skip-simulation",
        dest="run_simulation",
        action="store_false",
        default=True,
        help="Do not run SHETRAN before comparing files.",
    )
    parser.add_argument(
        "--skip-comparison",
        dest="run_comparison",
        action="store_false",
        default=True,
        help="Do not compare outputs; only run SHETRAN.",
    )
    parser.add_argument(
        "--shetran-exe",
        dest="fn_shetran",
        default=settings.default_shetran_exe,
        help=("Path to the SHETRAN executable. "
              "If omitted, uses the value from fn_shetran in this script."),
    )
    parser.add_argument(
        "-m",
        "--model",
        action="append",
        default=[],
        help=(
            "Model name to run and compare. Can be repeated; overrides --list "
            "when used."),
    )
    parser.add_argument(
        "-l",
        "--list",
        default="short",
        choices=["all", "long", "medium", "short"],
        help="Predefined model list to run and compare (default: short).",
    )
    args = parser.parse_args()

    if args.model:
        subdirs = args.model
    elif args.list == "all":
        subdirs = _get_all_models()
    elif args.list == "long":
        subdirs = settings.list_long_running
    elif args.list == "medium":
        subdirs = settings.list_medium_runtime
    else:
        subdirs = settings.list_short_runtime

    # deduplicate and sort model names
    subdirs = sorted(set(subdirs))

    # overall check
    util.check_model_names_vs_directory()

    # check the directory contents as necessary
    if args.run_simulation:
        util.check_model_files(subdirs)

    if args.run_comparison:
        util.check_comparison_files(subdirs)

    # loop through the subdirectories and run the comparison for each
    overview = {}
    for dir_main in subdirs:
        overview[dir_main] = do_comparison(
            dir_main,
            args.fn_shetran,
            run_simulation=args.run_simulation,
            run_comparison=args.run_comparison,
        )

    if overview:
        df_overview = pd.DataFrame.from_dict(overview, orient="index")
        df_overview.to_csv("comparison_overview.csv")


if __name__ == "__main__":

    main()
