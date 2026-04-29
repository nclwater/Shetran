#
# Utility functions for dealing with the examples.
#

# General Imports
import os
import shutil

# Local Imports
from . import settings


def remove_non_empty_dir(dir_path: str, flag_makeIt: bool = False) -> None:

    if flag_makeIt and os.path.exists(dir_path):

        # delete only the contents of the directory, not the directory itself
        for f in os.listdir(dir_path):
            os.remove(os.path.join(dir_path, f))
    else:
        if os.path.exists(dir_path):
            shutil.rmtree(dir_path)
        if flag_makeIt:
            os.makedirs(dir_path)

    return None


def check_model_names_vs_directory() -> None:
    """
    Checks that a. the models names given in the settings file actually exist and
    b. that there are no extra directories that aren't in the settings file.
    """

    # get the model names from the settings file
    model_names_from_settings = set(settings.list_long_running).union(
        set(settings.list_medium_runtime)).union(
            set(settings.list_short_runtime))

    # get the model names from the directories in the inputs directory
    model_names_from_directories = set(
        d for d in os.listdir() if os.path.isdir(d) and not d.startswith(".")
        and not d.startswith("_"))

    # check that all models in the settings file have a corresponding directory
    missing_directories = model_names_from_settings - model_names_from_directories
    if missing_directories:
        print(
            f"\nWarning: Models in settings file with no corresponding directory: "
            f"{missing_directories}")

    # check that there are no extra directories that aren't in the settings file
    extra_directories = model_names_from_directories - model_names_from_settings
    if extra_directories:
        print(
            f"\nWarning: Directories with no corresponding model in settings file: "
            f"{extra_directories}")


def check_model_files(list_models: list[str]) -> None:
    """
    Checks that the expected files for each model are present in the inputs directory.
    TODO replace with pulling that from the model config files rather than hardcoding the expected files
    """

    problem_found = False

    for model_name in list_models:

        files = [
            f
            for f in os.listdir(os.path.join(model_name, settings.dir_inputs))
            if os.path.isfile(os.path.join(model_name, settings.dir_inputs, f))
        ]

        if len(files) == 0:
            print(
                f"\nError: No files found in directory for model {model_name}")
            problem_found = True

        # check if there is one, and only one, rundata_*.txt file
        rundata_files = [
            f for f in files if f.startswith("rundata_") and f.endswith(".txt")
        ]
        if len(rundata_files) == 0:
            print(
                f"\nError: No rundata_*.txt file found for model {model_name}")
            problem_found = True

        elif len(rundata_files) > 1:
            print(
                f"\nError: Multiple rundata_*.txt files found for model {model_name}: "
                f"{rundata_files}")
            problem_found = True

    if problem_found:
        raise ValueError("Problems found with model files. "
                         "See above for details. Cannot continue")


def check_comparison_files(list_models: list[str]) -> None:
    """
    Checks that the expected files for each model are present in the results_should directory.
    TODO replace with pulling that from the model config files rather than hardcoding the expected files
    """

    problem_found = False

    for model_name in list_models:

        dir_results_should_model = os.path.join(model_name,
                                                settings.dir_results_should)

        if not os.path.exists(dir_results_should_model):
            print(
                f"\nError: No results_should directory found for model {model_name}"
            )
            problem_found = True
            continue

        files = [
            f for f in os.listdir(dir_results_should_model)
            if os.path.isfile(os.path.join(dir_results_should_model, f))
        ]

        if len(files) == 0:
            print(
                f"\nError: No files found in results_should directory for model {model_name}"
            )
            problem_found = True

    if problem_found:
        raise ValueError("Problems found with results_should files. "
                         f"See above for details. Cannot continue")
