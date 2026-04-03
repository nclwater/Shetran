#
# Setups the results comparison for the example models. This is intended to be
# run once after cloning the repository, to set up the results comparison for
# the example models. It will run each model and copy over the results to the
# expected results directory.
#
# (C) 2026, Sven Berendsen
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


def _get_all_models() -> list[str]:

    # Keep order stable while deduplicating list entries.
    return list(
        dict.fromkeys(settings.list_short_runtime +
                      settings.list_medium_runtime +
                      settings.list_long_running))


def _parse_args() -> argparse.Namespace:

    parser = argparse.ArgumentParser(description=(
        "Set up expected results for example models by running selected "
        "models and copying output files."))
    parser.add_argument(
        "-m",
        "--model",
        action="append",
        default=[],
        help=(
            "Model name to run. Can be repeated; overrides --list when used."),
    )
    parser.add_argument(
        "-l",
        "--list",
        default="short",
        choices=["all", "long", "medium", "short"],
        help="Predefined model list to run (default: short).",
    )
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "-e",
        "--exe",
        default="",
        help="Path to the SHETRAN executable.",
    )
    group.add_argument(
        "--use-release-exe",
        action="store_true",
        help="Use settings.default_shetran_exe as executable path.",
    )
    return parser.parse_args()


def main():

    # get the arguments
    args = _parse_args()

    fn_shetran = ""
    if args.use_release_exe:
        fn_shetran = settings.default_shetran_exe
    if args.exe:
        fn_shetran = args.exe

    if args.model:
        model_names = args.model
    elif args.list == "all":
        model_names = _get_all_models()
    elif args.list == "long":
        model_names = settings.list_long_running
    elif args.list == "medium":
        model_names = settings.list_medium_runtime
    else:
        model_names = settings.list_short_runtime

    model_names = sorted(set(model_names))  # deduplicate and sort model names

    # checks
    util.check_model_names_vs_directory()
    util.check_model_files(model_names)

    # run the models & copy over the results to the expected results directory
    data_overview = {}
    for model_name in model_names:

        # generate directory names
        dir_input = os.path.join(model_name, settings.dir_inputs)
        dir_simulation = os.path.join(model_name, settings.dir_compute)
        dir_results = os.path.join(model_name, settings.dir_results_should)

        print(f"\nRunning model {model_name}...")

        data_overview[model_name] = compute.run_model(
            fn_shetran,
            dir_input=dir_input,
            dir_simulation=dir_simulation,
            dir_diff=None,
        )

        # copy over all the files starting with "output_" from the compute
        # directory to the results_should directory
        os.makedirs(dir_results, exist_ok=True)
        for f in os.listdir(dir_simulation):
            if f.startswith("output_"):
                shutil.copy(os.path.join(dir_simulation, f),
                            os.path.join(dir_results, f))

    # export overview of the runtimes to a csv file
    df_overview = pd.DataFrame.from_dict(data_overview, orient="index")
    df_overview.to_csv(settings.fn_setup_overview)


if __name__ == "__main__":

    main()
