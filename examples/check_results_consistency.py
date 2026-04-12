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


def _build_comparison_results_rows(res_files: dict) -> list[dict]:
    rows = []
    for filename, file_res in res_files.items():
        file_type = file_res.get("file_type")
        status = file_res.get("status")
        files_different = file_res.get("files_different")
        if status == "missing":
            files_different = True
        same_row_count = file_res.get("same_row_count")

        if file_type == "table":
            col_metrics = file_res.get("table_column_metrics", [])
            if col_metrics:
                for col_res in col_metrics:
                    rows.append({
                        "file":
                        filename,
                        "data_item":
                        col_res.get("col_name"),
                        "file_type":
                        file_type,
                        "data_differs":
                        col_res.get("data_differs", files_different),
                        "same_row_count":
                        same_row_count,
                        "abs_max_difference":
                        col_res.get("abs_max_difference"),
                        "perc_max_difference":
                        col_res.get("perc_max_difference"),
                        "abs_mean_difference":
                        col_res.get("abs_mean_difference"),
                        "perc_mean_difference":
                        col_res.get("perc_mean_difference"),
                        "MSE":
                        col_res.get("MSE"),
                        "MAE":
                        col_res.get("MAE"),
                        "MAPE":
                        col_res.get("MAPE"),
                        "NSE":
                        col_res.get("NSE"),
                        "R²":
                        col_res.get("R²"),
                    })
            else:
                rows.append({
                    "file": filename,
                    "data_item": "",
                    "file_type": file_type,
                    "data_differs": files_different,
                    "same_row_count": same_row_count,
                    "abs_max_difference": pd.NA,
                    "perc_max_difference": pd.NA,
                    "abs_mean_difference": pd.NA,
                    "perc_mean_difference": pd.NA,
                    "MSE": pd.NA,
                    "MAE": pd.NA,
                    "MAPE": pd.NA,
                    "NSE": pd.NA,
                    "R²": pd.NA,
                })
        elif file_type == "hdf5":
            dataset_metrics = file_res.get("hdf5_dataset_metrics", [])
            if dataset_metrics:
                for dataset_res in dataset_metrics:
                    rows.append({
                        "file": filename,
                        "data_item": dataset_res.get("data_item", ""),
                        "file_type": file_type,
                        "data_differs": dataset_res.get(
                            "data_differs", files_different),
                        "same_row_count": dataset_res.get("same_row_count"),
                        "abs_max_difference": dataset_res.get(
                            "abs_max_difference"),
                        "perc_max_difference": dataset_res.get(
                            "perc_max_difference"),
                        "abs_mean_difference": dataset_res.get(
                            "abs_mean_difference"),
                        "perc_mean_difference": dataset_res.get(
                            "perc_mean_difference"),
                        "MSE": dataset_res.get("MSE"),
                        "MAE": dataset_res.get("MAE"),
                        "MAPE": dataset_res.get("MAPE"),
                        "NSE": dataset_res.get("NSE"),
                        "R²": dataset_res.get("R²"),
                    })
            else:
                rows.append({
                    "file": filename,
                    "data_item": "",
                    "file_type": file_type,
                    "data_differs": files_different,
                    "same_row_count": same_row_count,
                    "abs_max_difference": pd.NA,
                    "perc_max_difference": pd.NA,
                    "abs_mean_difference": pd.NA,
                    "perc_mean_difference": pd.NA,
                    "MSE": pd.NA,
                    "MAE": pd.NA,
                    "MAPE": pd.NA,
                    "NSE": pd.NA,
                    "R²": pd.NA,
                })
        else:
            rows.append({
                "file": filename,
                "data_item": "",
                "file_type": file_type,
                "data_differs": files_different,
                "same_row_count": same_row_count,
                "abs_max_difference": pd.NA,
                "perc_max_difference": pd.NA,
                "abs_mean_difference": pd.NA,
                "perc_mean_difference": pd.NA,
                "MSE": pd.NA,
                "MAE": pd.NA,
                "MAPE": pd.NA,
                "NSE": pd.NA,
                "R²": pd.NA,
            })

    return rows


def do_comparison(
    dir_main: str,
    fn_shetran: str,
    run_simulation: bool = True,
    run_comparison: bool = True,
) -> dict:

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

        # aggregate the results in a row-based format for easier filtering and
        # post-processing (one row per file/column for table files).
        comparison_rows = _build_comparison_results_rows(res_files)
        df = pd.DataFrame(comparison_rows)
        df.to_csv(os.path.join(dir_main, "comparison_results.csv"),
                  index=False)

        # extract whether there were differences in any of the files
        res_model["any_differences"] = df["data_differs"].any()

        # get maximum percentage metrics from table columns that actually differ
        table_rows = df[(df["file_type"] == "table")
                & (df["data_item"] != "")
                & (df["data_differs"] == True)]

        if not table_rows.empty and table_rows["perc_max_difference"].notna(
        ).any():
            idx_max = table_rows["perc_max_difference"].idxmax()
            res_model["max_perc_diff_any"] = table_rows.loc[
                idx_max, "perc_max_difference"]
            res_model["max_perc_diff_col"] = table_rows.loc[idx_max,
                                                            "data_item"]
        else:
            res_model["max_perc_diff_any"] = None
            res_model["max_perc_diff_col"] = None

        if not table_rows.empty and table_rows["perc_mean_difference"].notna(
        ).any():
            idx_mean_max = table_rows["perc_mean_difference"].idxmax()
            res_model["max_perc_diff_mean_any"] = table_rows.loc[
                idx_mean_max, "perc_mean_difference"]
            res_model["max_perc_diff_mean_col"] = table_rows.loc[idx_mean_max,
                                                                 "data_item"]
        else:
            res_model["max_perc_diff_mean_any"] = None
            res_model["max_perc_diff_mean_col"] = None

        # No sum-abs percentage metric is exported anymore in the per-column output.
        res_model["max_perc_diff_sum_abs_any"] = None
        res_model["max_perc_diff_sum_abs_col"] = None

        # add the file-information columns to the overview metrics
        res_model["num_files_with_different_contents"] = len(differing_files)
        res_model["num_files_too_large_to_compare"] = len(too_large_files)
        res_model["files_with_different_contents"] = differing_files_str
        res_model["files_too_large_to_compare"] = too_large_files_str

    return res_model


def main() -> None:

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
        subdirs = (settings.list_long_running + settings.list_medium_runtime +
                   settings.list_short_runtime)
    elif args.list == "medium":
        subdirs = settings.list_medium_runtime + settings.list_short_runtime
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
