#
# Updates the results should directories by copying over the compute results.
# Optionally, this can be done for only a subset of the models by specifying
# them with the --model or --list arguments. Additionally, if requested, the
# models themselves can be re-run to generate new compute results before
# copying over to.
#
# (C) 2026, Sven Berendsen
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Only do this if you know that the compute results are correct and want to
# update the expected results with those!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#

# General Imports
import argparse
import os
import shutil
import subprocess

# Package Imports
import pandas as pd

# Local Imports
from _methods import compare as compare
from _methods import compute as compute
from _methods import settings as settings
from _methods import util as util


def _compare_with_should(dir_should: str, dir_compute: str,
                         dir_diff: str) -> bool | None:
    """
    Compares the compute outputs with the existing expected results.
    Returns True if there are no differences, False if there are, and None if
    there are no expected results to compare against.
    """

    if not os.path.isdir(dir_should) or not os.listdir(dir_should):
        print("  No expected results to compare against.")
        return None

    util.remove_non_empty_dir(dir_diff)
    res_files = compare.compare_results(dir_should, dir_compute, dir_diff)

    differing_files = sorted(
        f for f, f_res in res_files.items()
        if f_res.get("status") == "missing" or f_res.get("files_different"))
    too_large_files = sorted(f for f, f_res in res_files.items()
                             if f_res.get("status") == "too large")
    new_files = sorted(
        f for f in os.listdir(dir_compute)
        if f.startswith("output_") and f not in res_files)

    if differing_files:
        print(f"  Files with differences: {differing_files}")
    if new_files:
        print(f"  Output files not in expected results: {new_files}")
    if too_large_files:
        print(f"  Files too large to compare: {too_large_files}")

    return not differing_files and not new_files


def main():

    # parse the arguments
    parser = argparse.ArgumentParser(description=(
        "Update the expected results by copying over the compute results. "
        "Optionally, the models can be re-run to generate new compute results "
        "before copying over."))
    parser.add_argument(
        "-m",
        "--model",
        action="append",
        default=[],
        help=(
            "Model name to update. Can be repeated; overrides --list when used."
        ),
    )
    parser.add_argument(
        "-l",
        "--list",
        default="short",
        choices=["all", "long", "medium", "short"],
        help="Predefined model list to update (default: short).",
    )
    parser.add_argument(
        "--run-simulation",
        action="store_true",
        help=
        "Whether to run the simulations before updating the expected results.",
    )
    parser.add_argument(
        "--shetran-exe",
        dest="fn_shetran",
        default=settings.default_shetran_exe,
        help=("Path to the SHETRAN executable. "
              "If omitted, uses the value from fn_shetran in this script."),
    )
    args = parser.parse_args()

    # get the models to be updated
    if args.model:
        model_names = args.model
    elif args.list == "all":
        model_names = list(
            set(settings.list_short_runtime + settings.list_medium_runtime +
                settings.list_long_running))
    elif args.list == "long":
        model_names = (settings.list_long_running +
                       settings.list_medium_runtime +
                       settings.list_short_runtime)
    elif args.list == "medium":
        model_names = settings.list_medium_runtime + settings.list_short_runtime
    else:
        model_names = settings.list_short_runtime

    model_names = sorted(set(model_names))  # deduplicate and sort model names

    # checks
    util.check_model_names_vs_directory()
    if args.run_simulation:
        util.check_model_files(model_names)

    # if requested, run the simulations before copying over the results
    data_overview = {model_name: {} for model_name in model_names}
    if args.run_simulation:
        for model_name in model_names:
            # generate directory names
            dir_input = os.path.join(model_name, settings.dir_inputs)
            dir_simulation = os.path.join(model_name, settings.dir_compute)
            dir_diff = os.path.join(model_name, settings.dir_delta)
            print(f"\nRunning model {model_name}...")
            try:
                data_overview[model_name].update(
                    compute.run_model(
                        args.fn_shetran,
                        dir_input=dir_input,
                        dir_simulation=dir_simulation,
                        dir_diff=dir_diff,
                    ))
                data_overview[model_name]["exit_code"] = 0
            except subprocess.CalledProcessError as e:
                print(f"  SHETRAN failed with exit code {e.returncode}.")
                data_overview[model_name]["exit_code"] = e.returncode

    # loop through the models, compare with and update the expected results
    for model_name in model_names:
        # generate directory names
        dir_compute = os.path.join(model_name, settings.dir_compute)
        dir_results = os.path.join(model_name, settings.dir_results_should)
        dir_diff = os.path.join(model_name, settings.dir_delta)

        # don't overwrite the expected results with those of a failed run
        if data_overview[model_name].get("exit_code", 0) != 0:
            print(f"\nSkipping model {model_name} as its simulation failed.")
            continue

        # compare before the expected results get overwritten
        print(f"\nComparing with expected results for model {model_name}...")
        data_overview[model_name]["no_differences"] = _compare_with_should(
            dir_results, dir_compute, dir_diff)

        print(f"Updating expected results for model {model_name}...")

        # copy over all the files starting with "output_" from the compute directory to the expected results directory
        os.makedirs(dir_results, exist_ok=True)
        for f in os.listdir(dir_compute):
            if f.startswith("output_"):
                shutil.copy(os.path.join(dir_compute, f),
                            os.path.join(dir_results, f))

    df = pd.DataFrame.from_dict(data_overview, orient="index")
    df.to_csv("update_results_overview.csv")

    # summary
    n_models = len(model_names)
    res_comparison = [
        res.get("no_differences") for res in data_overview.values()
    ]
    n_compared = sum(res is not None for res in res_comparison)
    n_same = sum(res is True for res in res_comparison)

    print("\nSummary:")
    if args.run_simulation:
        n_success = sum(
            res.get("exit_code") == 0 for res in data_overview.values())
        print(f"  Finished successfully (exit code 0): "
              f"{n_success} of {n_models}")
    print(f"  No differences to expected results:  "
          f"{n_same} of {n_compared} compared")
    if n_compared < n_models:
        print(f"  Not compared (failed run or no expected results): "
              f"{n_models - n_compared}")


if __name__ == "__main__":
    main()
