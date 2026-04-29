#
# Removes the compute & delta directories and, if requested, the output_should
# directories for the models as well. Additionally the generated overview
# output files are removed also.
#

# General Imports
import argparse
import os
import shutil

# Local Imports
from _methods import settings as settings
from _methods import util as util


def main():

    # parse the arguments
    parser = argparse.ArgumentParser(description=(
        "Clean up the compute, delta, and expected results directories for "
        "the example models."))
    parser.add_argument(
        "-m",
        "--model",
        action="append",
        default=[],
        help=(
            "Model name to clean. Can be repeated; overrides --list when used."
        ),
    )
    parser.add_argument(
        "-l",
        "--list",
        default="all",
        choices=["all", "long", "medium", "short"],
        help="Predefined model list to clean (default: all).",
    )
    parser.add_argument(
        "--remove-expected-results",
        action="store_true",
        help="Also remove the expected results directories (output_should).",
    )
    args = parser.parse_args()

    # get the models to be cleaned
    if args.model:
        model_names = args.model
    elif args.list == "all":
        model_names = list(
            set(settings.list_short_runtime + settings.list_medium_runtime +
                settings.list_long_running))
    elif args.list == "long":
        model_names = settings.list_long_running
    elif args.list == "medium":
        model_names = settings.list_medium_runtime
    else:
        model_names = settings.list_short_runtime

    # remove the compute and delta directories for each model, and optionally the
    # expected results directory
    for model_name in model_names:

        # generate directory names
        dir_compute = os.path.join(model_name, settings.dir_compute)
        dir_delta = os.path.join(model_name, settings.dir_delta)
        dir_results = os.path.join(model_name, settings.dir_results_should)

        # remove the directories if they exist even if they're not empty
        util.remove_non_empty_dir(dir_compute)
        util.remove_non_empty_dir(dir_delta)
        if args.remove_expected_results:
            util.remove_non_empty_dir(dir_results)

        # remove the generated analysis files if they exist
        fn_model_analysis = os.path.join(model_name,
                                         settings.fn_model_analysis)
        if os.path.exists(fn_model_analysis):
            os.remove(fn_model_analysis)

    # remove the generated overview files if they exist
    if os.path.exists(settings.fn_setup_overview):
        os.remove(settings.fn_setup_overview)
    if os.path.exists(settings.fn_overall_analysis):
        os.remove(settings.fn_overall_analysis)


if __name__ == "__main__":

    main()
