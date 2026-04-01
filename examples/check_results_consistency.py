#
# Checks the output files for consistency between different runs.
#
# (C) 2026, Sven Berendsen
#
# TODO Read file-type & name from the config file instead of hardcoding it here.
# TODO Write bespoke comparison functions for the non-standard text files.
# TODO Check up to which file sizes a content comparison is feasible, and implement a more efficient comparison for larger files (e.g. using hashing or chunked reading).
#

# general imports
import argparse
import datetime
import difflib
import glob
import os
import shutil
import subprocess
from typing import Any

# package imports
import h5py
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from tqdm import tqdm

# global variables
dir_inputs = "backup_input"
dir_compute = "test_compute"
dir_results_should = "output_should"
dir_delta = "diff_delta"

# settings
list_long_running = [
    "dano100m",  # very large area, long runtime
    "foston100m",  # large area, long runtime
    "38014-100m-SurfaceErrors",
    "Aire_at_Kildwick_Bridge-AllOptions-withdates",
]
list_table_files_indicators = [
    "_sim_daily.",
    "_sim_hourly.",
    "_discharge_sim_everytimestep.",
    "_discharge_sim_regulartimestep.",
    "_mb.water_table_depth.",
]
tolerance_numeric = 1e-5
tolerance_table = 0.001
files_too_large_threshold = 10 * 1024 * 1024  # 10 MB


def _plot_col_differences(df_combined: pd.DataFrame, col: str, fn_figure: str,
                          metrics: dict) -> None:
    """
    Generates and saves a plot comparing 'should' and 'is' timeseries data,
    along with their absolute and percentage differences.

    Args:
        df_combined (pd.DataFrame): DataFrame with 'should', 'is', 'diff_abs', 'diff_pct' columns.
        col (str): The name of the data column being plotted.
        fn_figure (str): The output filename for the plot image.
        metrics (dict): A dictionary of similarity metrics to display.
    """
    plt.style.use('dark_background')
    fig = plt.figure(figsize=(20, 12), constrained_layout=True)
    fig.suptitle(f"Comparison for column: {col}", fontsize=16)

    # Use GridSpec for a more complex layout: 3 rows, 2 columns.
    # The first column is for the legend/text, the second for the plots.
    gs = fig.add_gridspec(3, 2, width_ratios=[1, 5])

    # --- Subplots for data ---
    ax1 = fig.add_subplot(gs[0, 1])
    ax2 = fig.add_subplot(gs[1, 1], sharex=ax1)
    ax3 = fig.add_subplot(gs[2, 1], sharex=ax1)

    # --- Subplot for legend and text ---
    ax_legend = fig.add_subplot(gs[:, 0])

    # --- Top plot: Timeseries ---
    line1, = ax1.plot(df_combined.index,
                      df_combined['should'],
                      label='Expected',
                      color='cyan')
    line2, = ax1.plot(df_combined.index,
                      df_combined['is'],
                      label='Actual',
                      color='magenta',
                      linestyle='--')
    ax1.set_title("Timeseries")
    ax1.grid(True, linestyle=':', alpha=0.5)
    plt.setp(ax1.get_xticklabels(),
             visible=False)  # Hide x-axis labels for top and middle plots

    # --- Middle plot: Absolute Difference ---
    ax2.plot(df_combined.index, df_combined['diff_abs'], color='red')
    ax2.set_title("Absolute Difference")
    ax2.grid(True, linestyle=':', alpha=0.5)
    plt.setp(ax2.get_xticklabels(), visible=False)

    # --- Bottom plot: Percentage Difference ---
    ax3.plot(df_combined.index,
             df_combined['diff_pct'].fillna(np.nan),
             color='orange')
    ax3.set_title("Percentage Difference")
    ax3.set_xlabel("Time")
    ax3.grid(True, linestyle=':', alpha=0.5)

    # --- Legend and Metrics Box ---
    ax_legend.axis('off')
    # Place legend in the upper part of the legend axis
    ax_legend.legend(handles=[line1, line2],
                     loc='center left',
                     bbox_to_anchor=(0, 0.85))

    # Format metrics text and place it in a box below the legend
    metrics_text = "Similarity Metrics:\n"
    metrics_text += '\n'.join([f'  {k}: {v:.4f}' for k, v in metrics.items()])
    ax_legend.text(0.0,
                   0.7,
                   metrics_text,
                   transform=ax_legend.transAxes,
                   fontsize=12,
                   verticalalignment='top',
                   bbox=dict(boxstyle='round,pad=0.5',
                             fc='black',
                             ec='grey',
                             lw=1))

    # Save and close the figure to free up memory
    os.makedirs(os.path.dirname(fn_figure), exist_ok=True)
    plt.savefig(fn_figure)
    plt.close(fig)

    return None


def _get_similarity_metrics(df_analysis: pd.DataFrame) -> dict:

    mse = np.mean((df_analysis["should"] - df_analysis["is"])**2)
    mae = np.mean(np.abs(df_analysis["should"] - df_analysis["is"]))

    mask = df_analysis["should"] != 0
    mape = np.mean(
        np.abs((df_analysis["is"][mask] - df_analysis["should"][mask]) /
               df_analysis["should"][mask])) * 100

    nse = 1 - np.sum((df_analysis["should"] - df_analysis["is"])**2) / np.sum(
        (df_analysis["should"] - df_analysis["should"].mean())**2)

    ss_res = np.sum((df_analysis["should"] - df_analysis["is"])**2)
    ss_tot = np.sum((df_analysis["should"] - df_analysis["should"].mean())**2)
    r2 = 1 - ss_res / ss_tot

    res = {
        "MSE": mse,
        "MAE": mae,
        "MAPE": mape,
        "NSE": nse,
        "R²": r2,
    }

    return res


def compare_table(fn_should: str, fn_is: str, fn_delta: str) -> dict:

    # prep
    flag_diff = False
    res = {}

    # compare the first line between is and should
    with open(fn_should, "r") as f_should, open(fn_is, "r") as f_is:
        line_should = f_should.readline()
        line_is = f_is.readline()

    if line_should != line_is:
        flag_diff = True
        with open(fn_delta, "w") as f_delta:
            f_delta.write(line_should)

    # read the rest of the table into pandas dataframes
    df_should = pd.read_csv(fn_should,
                            skiprows=1,
                            parse_dates=True,
                            index_col=0)
    df_is = pd.read_csv(fn_is, skiprows=1, parse_dates=True, index_col=0)

    # compare the dataframes
    # start with the textual content (column names, number of rows, etc.)
    if list(df_should.columns) != list(df_is.columns):
        flag_diff = True
        res["identical_columns"] = False
    else:
        res["identical_columns"] = True

    if len(df_should) != len(df_is):
        flag_diff = True
        res["same_row_count"] = False
    else:
        res["same_row_count"] = True

    # only rows with "time" and "date" in the column names are allowed to be strings
    date_time_should = [
        idx for idx, h in enumerate(df_should.columns)
        if "date" in h.lower() or "time" in h.lower()
    ]
    date_time_is = [
        idx for idx, h in enumerate(df_is.columns)
        if "date" in h.lower() or "time" in h.lower()
    ]
    if date_time_should != date_time_is:
        flag_diff = True
        res["same_date_time_columns"] = False
    else:
        res["same_date_time_columns"] = True

    # now check if those rows are the only ones that are allowed to be non-numeric
    non_numeric_should = []
    non_numeric_is = []
    for idx, col in enumerate(df_should.columns):
        if idx not in date_time_should:
            if not pd.api.types.is_numeric_dtype(df_should[col]):
                non_numeric_should.append(col)
            if not pd.api.types.is_numeric_dtype(df_is[col]):
                non_numeric_is.append(col)
    if non_numeric_should != non_numeric_is:
        flag_diff = True
        res["same_non_numeric_columns"] = False
    else:
        res["same_non_numeric_columns"] = True

    # compare the contents, check both the numeric and non-numeric columns,
    # but only if the structure is the same
    if res["identical_columns"] and res["same_row_count"]:

        # compare the non-numeric columns (only the date/time ones)
        for idx in date_time_should:
            col = df_should.columns[idx]
            if not df_should[col].equals(df_is[col]):
                flag_diff = True
                res[f"non_numeric_column_{col}"] = False
            else:
                res[f"non_numeric_column_{col}"] = True

        # for each numeric column, generate a difference column and check if the
        # differences are within the tolerance
        for idx, col in enumerate(df_should.columns):
            if idx not in date_time_should:
                if not pd.api.types.is_numeric_dtype(df_should[col]):
                    continue

                # combine the two numeric columns into a single column with
                # the values from both
                df_combined = pd.concat([df_should[col], df_is[col]],
                                        axis=1,
                                        keys=["should", "is"])

                # add difference columns, both percentage and absolute
                df_combined[
                    "diff_abs"] = df_combined["should"] - df_combined["is"]
                df_combined["diff_pct"] = df_combined[
                    "diff_abs"] / df_combined["should"].replace(0, pd.NA)

                # check if the differences are within the tolerance
                if not df_combined["diff_abs"].abs().le(
                        tolerance_numeric).all():

                    # get the similarity metrics for this column
                    metrics = _get_similarity_metrics(df_combined)
                    res.update(metrics)

                    # write to file, putting in an extension with the column name
                    safe_col = col.replace(" ", "_").replace("/", "_").replace(
                        "\\", "_")
                    fn_col = os.path.splitext(fn_delta)[
                        0] + f"_{safe_col}" + os.path.splitext(fn_delta)[1]

                    # create the directory if it doesn't exist
                    os.makedirs(os.path.dirname(fn_col), exist_ok=True)
                    df_combined.to_csv(fn_col, index=True)

                    # generate column difference plots
                    fn_figure = os.path.splitext(fn_col)[0] + ".png"
                    _plot_col_differences(df_combined, col, fn_figure, metrics)

                    # remove all blanks and replace all special characters around the column name
                    safe_col = col.strip().replace(" ", "_").replace(
                        "/", "_").replace("\\", "_")

                    flag_diff = True
                    res[f"numeric_column_{col}"] = False
                else:
                    res[f"numeric_column_{col}"] = True

    res["files_different"] = flag_diff
    return res


def compare_textfile(fn_should: str, fn_is: str, fn_delta: str) -> dict:
    # TODO make a text-file-content specific comparison function(s)

    flag_difference = False

    # Read files and normalize lines
    with open(fn_should, 'r', encoding='utf-8') as f_should:
        # rstrip() removes trailing spaces, tabs, \r, and \n.
        # Adding '\n' back ensures the diff output remains cleanly formatted.
        should_lines = [line.rstrip() + '\n' for line in f_should]

    with open(fn_is, 'r', encoding='utf-8') as f_is:
        is_lines = [line.rstrip() + '\n' for line in f_is]

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
        with open(fn_delta, 'w', encoding='utf-8') as f_delta:
            f_delta.writelines(diff)
        flag_difference = True

    return {"files_different": flag_difference}


def _compare_datasets(ds_should, ds_is, f_delta, path, diffs,
                      tolerance_numeric) -> dict:
    """
    Helper function to compare two HDF5 datasets and write deltas if appropriate.
    """
    if ds_should.shape != ds_is.shape:
        diffs["shape_mismatch"].append(path)
        return diffs

    if ds_should.dtype != ds_is.dtype:
        diffs["dtype_mismatch"].append(path)

    if not (np.issubdtype(ds_should.dtype, np.number)
            and np.issubdtype(ds_is.dtype, np.number)):
        if not np.array_equal(ds_should[()], ds_is[()]):
            diffs["non_numeric_diff"].append(path)
        return diffs

    # Load data into memory
    data_should = ds_should[()]
    data_is = ds_is[()]

    # Check if they are equal within the given absolute tolerance.
    # rtol=0.0 ensures we are only strictly checking against the absolute tolerance_numeric.
    # equal_nan=True ensures NaNs in the exact same spot are considered "equal".
    if not np.allclose(
            data_should, data_is, atol=tolerance_numeric, rtol=0.0,
            equal_nan=True):

        diffs["numeric_diff"].append(path)

        # Calculate the delta
        delta = data_is - data_should

        # Write to the already-open delta file.
        parent_path, dataset_name = path.rsplit("/", 1)
        parent_group = f_delta if parent_path in (
            "", "/") else f_delta.require_group(parent_path)
        if dataset_name in parent_group:
            del parent_group[dataset_name]
        parent_group.create_dataset(dataset_name, data=delta)

    return diffs


def _compare_groups(group_should, group_is, f_delta, current_path, diffs,
                    tolerance_numeric) -> dict:
    """
    Recursive helper function to traverse and compare HDF5 groups.
    """
    keys_should = set(group_should.keys())
    keys_is = set(group_is.keys())

    # 1. Record missing paths
    for k in keys_should - keys_is:
        diffs["missing_in_is"].append(f"{current_path}{k}")

    for k in keys_is - keys_should:
        diffs["missing_in_should"].append(f"{current_path}{k}")

    # 2. Iterate through paths that exist in both files
    common_keys = keys_should & keys_is

    for k in common_keys:
        path = f"{current_path}{k}" if current_path == "/" else f"{current_path}/{k}"

        obj_should = group_should[k]
        obj_is = group_is[k]

        if isinstance(obj_should, h5py.Group) and isinstance(
                obj_is, h5py.Group):
            diffs = _compare_groups(obj_should, obj_is, f_delta, path, diffs,
                                    tolerance_numeric)

        elif isinstance(obj_should, h5py.Dataset) and isinstance(
                obj_is, h5py.Dataset):
            diffs = _compare_datasets(obj_should, obj_is, f_delta, path, diffs,
                                      tolerance_numeric)

        else:
            diffs["type_mismatch"].append(path)

    return diffs


def compare_hdf5(fn_should: str, fn_is: str, fn_delta: str) -> dict:

    structural_diffs: dict[str, Any] = {
        "missing_in_is": [],
        "missing_in_should": [],
        "type_mismatch": [],
        "shape_mismatch": [],
        "dtype_mismatch": [],
        "non_numeric_diff": [],
        "numeric_diff": []
    }

    os.makedirs(os.path.dirname(fn_delta), exist_ok=True)

    with h5py.File(fn_should, 'r') as f_should, \
         h5py.File(fn_is, 'r') as f_is, \
         h5py.File(fn_delta, 'w') as f_delta:

        # Start the recursive comparison, passing the tolerance along
        structural_diffs = _compare_groups(f_should, f_is, f_delta, "/",
                                           structural_diffs, tolerance_numeric)

    # If there were no numeric differences, remove the empty delta file.
    if not structural_diffs["numeric_diff"] and os.path.exists(fn_delta):
        os.remove(fn_delta)

    # keep only non-empty difference categories in the output
    res = {k: v for k, v in structural_diffs.items() if len(v) > 0}
    res["files_different"] = len(res) > 0
    return res


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
        if size_should > files_too_large_threshold or size_is > files_too_large_threshold:
            res[file] = {
                "status": "too large",
                "file_size_comparison": (size_should == size_is)
            }
            continue

        # if the file is small enough, do a content comparison

        # prep
        res_tmp: dict[str, Any] = {
            "file_size_comparison": (size_should == size_is)
        }
        fn_delta = os.path.join(dir_diff, f"delta_{file}")

        # compare
        if file.endswith(".txt") or file.endswith(".csv"):

            # work depending on it being a table or not
            if any(ind in file for ind in
                   list_table_files_indicators) or file.endswith(".csv"):
                res_tmp["file_type"] = "table"
                res_tmp.update(compare_table(fn_should, fn_is, fn_delta))
            else:
                res_tmp["file_type"] = "text"
                res_tmp.update(compare_textfile(fn_should, fn_is, fn_delta))

        elif file.endswith(".hdf5") or file.endswith(".h5"):
            res_tmp["file_type"] = "hdf5"
            res_tmp.update(compare_hdf5(fn_should, fn_is, fn_delta))
            if not res_tmp["file_size_comparison"]:
                res_tmp["size_should_bytes"] = size_should
                res_tmp["size_is_bytes"] = size_is
                res_tmp["files_different"] = True

        else:
            raise ValueError(f"  Unrecognized file type for {file}.")

        res[file] = res_tmp

    return res


def _remove_non_empty_dir(dir_path: str, flag_makeIt: bool = False) -> None:

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


def run_model(fn_shetran: str, dir_input: str, dir_simulation: str,
              dir_diff: str) -> dict:

    # delta & compute directories
    _remove_non_empty_dir(dir_simulation, flag_makeIt=True)
    _remove_non_empty_dir(dir_diff)

    # copy all the inputs from dir_inputs to the compute directory
    for f in os.listdir(dir_input):
        shutil.copy2(os.path.join(dir_input, f),
                     os.path.join(dir_simulation, f))

    # run shetran in the compute directory
    # find the rundata file (must match rundata_*.txt per the source code requirement)
    rundata_files = glob.glob(os.path.join(dir_simulation, "rundata_*.txt"))
    if not rundata_files:
        print(f"  No rundata_*.txt file found in {dir_simulation}, skipping.")
        return {}

    # run the model
    rundata_file = os.path.abspath(rundata_files[0])
    t_start = datetime.datetime.now()
    subprocess.run(
        [fn_shetran, "-f", rundata_file],
        cwd=os.path.abspath(dir_simulation),
        check=True,
    )
    t_end = datetime.datetime.now()

    return {"sim_duration": (t_end - t_start).total_seconds()}


def do_comparison(dir_main: str,
                  fn_shetran: str,
                  run_simulation: bool = True,
                  run_comparison: bool = True) -> dict:

    print(f"\nRunning comparison with main directory: {dir_main}")

    # set directory names
    dir_model = os.path.join(dir_main, dir_inputs)
    dir_sim = os.path.join(dir_main, dir_compute)
    dir_should = os.path.join(dir_main, dir_results_should)
    dir_diff = os.path.join(dir_main, dir_delta)

    # prep
    res_model = {}

    # run the model
    if run_simulation:

        # run the model
        res_model.update(run_model(fn_shetran, dir_model, dir_sim, dir_diff))

    if run_comparison:

        # compare the results with the known good ones
        res_files = compare_results(dir_should, dir_sim, dir_diff)

        # aggregate the results
        df = pd.DataFrame.from_dict(res_files, orient="index")
        df.to_csv(os.path.join(dir_main, "comparison_results.csv"))

        # extract whether there were differences in any of the files
        res_model["any_differences"] = df["files_different"].any()

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
    args = parser.parse_args()

    # settings
    # fn_shetran = r"C:\\Users\\tolstoi\\Documents\\Code\\shetran_rework\\build\\debug\\bin\\shetran.exe"
    # fn_shetran = r"C:\\Users\\tolstoi\\Documents\\Code\\shetran_rework\\build\\release\\bin\\shetran.exe"
    fn_shetran = os.path.join("..", "build", "release", "bin", "shetran.exe")
    # fn_shetran = os.path.join("_official_exe", "Shetran.exe")

    # get list of all subdirectories in the current directory
    subdirs = [
        d for d in os.listdir(".")
        if os.path.isdir(d) and not d.startswith("_")
    ]

    # filter out any which are in the long-running list
    subdirs = [d for d in subdirs if d not in list_long_running]

    # loop through the subdirectories and run the comparison for each
    overview = {}
    for dir_main in subdirs:
        overview[dir_main] = do_comparison(
            dir_main,
            fn_shetran,
            run_simulation=args.run_simulation,
            run_comparison=args.run_comparison,
        )

    if overview:
        df_overview = pd.DataFrame.from_dict(overview, orient="index")
        df_overview.to_csv("comparison_overview.csv")


if __name__ == "__main__":

    main()
