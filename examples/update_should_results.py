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

# Package Imports
import pandas as pd

# Local Imports
from _methods import compute as compute
from _methods import settings as settings
from _methods import util as util


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
    if args.run_simulation:
        data_overview = {}
        for model_name in model_names:
            # generate directory names
            dir_input = os.path.join(model_name, settings.dir_inputs)
            dir_simulation = os.path.join(model_name, settings.dir_compute)
            print(f"\nRunning model {model_name}...")
            data_overview[model_name] = compute.run_model(
                args.fn_shetran,
                dir_input=dir_input,
                dir_simulation=dir_simulation,
            )

        df = pd.DataFrame.from_dict(data_overview, orient="index")
        df.to_csv("update_results_overview.csv")

    # loop through the models and update the expected results
    for model_name in model_names:
        # generate directory names
        dir_compute = os.path.join(model_name, settings.dir_compute)
        dir_results = os.path.join(model_name, settings.dir_results_should)

        print(f"\nUpdating expected results for model {model_name}...")

        # copy over all the files starting with "output_" from the compute directory to the expected results directory
        for f in os.listdir(dir_compute):
            if f.startswith("output_"):
                shutil.copy(os.path.join(dir_compute, f),
                            os.path.join(dir_results, f))
