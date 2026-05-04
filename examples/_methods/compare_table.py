#
# Methods for comparing table outputs.
#

# General Imports
import os
from datetime import datetime
from sqlite3 import DateFromTicks

# Standard Packages
import numpy as np
import pandas as pd

# Local Imports
from . import compare_plots as compare_plots
from . import settings as settings


def _is_datetime_header(column_name: str) -> bool:
    """Return True when a column header is explicitly date-like."""
    header = column_name.strip().lower()
    return "date" in header or "iso8601" in header


def _parse_table_datetime(
    value: str, *, filename: str | None = None, column: str | None = None
) -> pd.Timestamp:
    """Parse supported table datetime values or raise an explicit format error."""
    valid_formats = ("%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S")
    value = str(value)
    for date_format in valid_formats:
        try:
            return pd.Timestamp(datetime.strptime(value, date_format))
        except ValueError:
            pass

    context = []
    if filename is not None:
        context.append(f"file {filename!r}")
    if column is not None:
        context.append(f"column {column!r}")
    context_text = f" in {', '.join(context)}" if context else ""

    raise ValueError(
        f"Unsupported datetime value {value!r}{context_text}. Expected format "
        "'YYYY-MM-DDTHH:MM:SS' or 'YYYY-MM-DD HH:MM:SS'."
    )


def _read_table_csv(fn_table: str) -> pd.DataFrame:
    """Read table data and only parse explicitly date-like columns as datetimes."""
    header_df = pd.read_csv(fn_table, skiprows=1, nrows=0)
    date_columns = [col for col in header_df.columns if _is_datetime_header(col)]
    converters = {
        col: (
            lambda value, column=col: _parse_table_datetime(
                value, filename=fn_table, column=column
            )
        )
        for col in date_columns
    }

    return pd.read_csv(
        fn_table,
        skiprows=1,
        converters=converters,
        index_col=False,
    )


def _get_similarity_metrics(df_analysis: pd.DataFrame) -> dict:
    df_valid = df_analysis[["should", "is"]].dropna()
    if df_valid.empty:
        return {
            "MSE": np.nan,
            "MAE": np.nan,
            "MAPE": np.nan,
            "NSE": np.nan,
            "R²": np.nan,
        }

    mse = np.mean((df_valid["should"] - df_valid["is"]) ** 2)
    mae = np.mean(np.abs(df_valid["should"] - df_valid["is"]))

    mask = df_valid["should"] != 0
    if mask.any():
        mape = (
            np.mean(
                np.abs(
                    (df_valid["is"][mask] - df_valid["should"][mask])
                    / df_valid["should"][mask]
                )
            )
            * 100
        )
    else:
        mape = np.nan

    ss_res = np.sum((df_valid["should"] - df_valid["is"]) ** 2)
    ss_tot = np.sum((df_valid["should"] - df_valid["should"].mean()) ** 2)

    if ss_tot != 0:
        nse = 1 - ss_res / ss_tot
        r2 = 1 - ss_res / ss_tot
    else:
        nse = np.nan
        r2 = np.nan

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
    table_column_metrics = []

    # compare the first line between is and should
    with open(fn_should, "r") as f_should, open(fn_is, "r") as f_is:
        line_should = f_should.readline()
        line_is = f_is.readline()

    if line_should != line_is:
        flag_diff = True
        os.makedirs(os.path.dirname(fn_delta), exist_ok=True)
        with open(fn_delta, "w") as f_delta:
            f_delta.write(line_should)

    # read the rest of the table into pandas dataframes
    df_should = _read_table_csv(fn_should)
    df_is = _read_table_csv(fn_is)

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

    # only explicitly date-like columns are allowed to be non-numeric
    date_time_should = [col for col in df_should.columns if _is_datetime_header(col)]
    date_time_is = [col for col in df_is.columns if _is_datetime_header(col)]
    if date_time_should != date_time_is:
        flag_diff = True
        res["same_date_time_columns"] = False
    else:
        res["same_date_time_columns"] = True

    # now check if those rows are the only ones that are allowed to be non-numeric
    non_numeric_should = []
    non_numeric_is = []
    for col in df_should.columns:
        if col not in date_time_should:
            if not pd.api.types.is_numeric_dtype(df_should[col]):
                non_numeric_should.append(col)
            if not pd.api.types.is_numeric_dtype(df_is[col]):
                non_numeric_is.append(col)
    if non_numeric_should != non_numeric_is:
        flag_diff = True
        res["same_non_numeric_columns"] = False
    else:
        res["same_non_numeric_columns"] = True

    # check that there is only one date-time column
    if len(date_time_should) > 1:
        flag_diff = True
        res["too_many_date_time_columns"] = True
    else:
        res["too_many_date_time_columns"] = False

    # set index to a date-time column (if present) and compare index values.
    # Some tables have no explicit date-time column, so avoid set_index([]).
    if len(date_time_should) == 1 and len(date_time_is) == 1:
        df_should.set_index(date_time_should[0], inplace=True)
        df_is.set_index(date_time_is[0], inplace=True)
        if not df_should.index.equals(df_is.index):
            flag_diff = True
            res["identical_date_time_index"] = False
        else:
            res["identical_date_time_index"] = True
    elif len(date_time_should) == 0 and len(date_time_is) == 0:
        # No explicit date-time column in either file: compare row-order index.
        if not df_should.index.equals(df_is.index):
            flag_diff = True
            res["identical_date_time_index"] = False
        else:
            res["identical_date_time_index"] = True
    else:
        # Mismatched date-time columns were already detected above.
        flag_diff = True
        res["identical_date_time_index"] = False

    # compare the contents column-by-column and expose per-column metrics.
    # This powers the expanded comparison_results.csv output (one row per column).
    if res["identical_columns"]:
        for col in df_should.columns:
            # remove & replace special characters for legacy per-column flags
            col_save = col.replace(" ", "_").replace("/", "_").replace("\\", "_")
            col_save = "".join(filter(None, col_save.split("_")))
            col_save = "".join(filter(None, col_save.split(".")))

            should_col_name = f"{col}_should"
            is_col_name = f"{col}_is"

            # Outer alignment makes shared index values land in the same row,
            # and keeps index values that exist only in one file as NaN on the other side.
            df_combined = pd.concat(
                [
                    df_should[col].rename(should_col_name),
                    df_is[col].rename(is_col_name),
                ],
                axis=1,
                join="outer",
            )

            # make certain the index is sorted
            df_combined.sort_index(inplace=True)

            # interpolate all "missing" entries using linear interpolation
            df_combined.interpolate(method="linear", inplace=True)

            is_numeric = pd.api.types.is_numeric_dtype(df_should[col])
            if col in date_time_should or not is_numeric:
                # Keep non-numeric/date columns in the overview with NaN metrics.
                data_differs = not df_combined[should_col_name].equals(
                    df_combined[is_col_name]
                )
                if data_differs:
                    flag_diff = True
                    res[f"non_numeric_column_{col_save}"] = False
                else:
                    res[f"non_numeric_column_{col_save}"] = True

                table_column_metrics.append(
                    {
                        "col_name": col.strip(),
                        "data_differs": data_differs,
                        "abs_max_difference": np.nan,
                        "perc_max_difference": np.nan,
                        "abs_mean_difference": np.nan,
                        "perc_mean_difference": np.nan,
                        "MSE": np.nan,
                        "MAE": np.nan,
                        "MAPE": np.nan,
                        "NSE": np.nan,
                        "R²": np.nan,
                    }
                )
                continue

            # add difference columns, both percentage and absolute
            df_combined["diff_abs"] = (
                df_combined[should_col_name] - df_combined[is_col_name]
            )
            df_combined["diff_pct"] = df_combined["diff_abs"] / df_combined[
                should_col_name
            ].replace(0, pd.NA)

            both_zero_mask = (df_combined[should_col_name] == 0) & (
                df_combined[is_col_name] == 0
            )
            df_combined.loc[both_zero_mask, "diff_abs"] = 0
            df_combined.loc[both_zero_mask, "diff_pct"] = 0

            abs_max_difference = df_combined["diff_abs"].abs().max()
            perc_max_difference = df_combined["diff_pct"].abs().max()
            abs_mean_difference = df_combined["diff_abs"].abs().mean()
            perc_mean_difference = df_combined["diff_pct"].abs().mean()

            # Metrics and plots continue to use canonical names.
            df_analysis = df_combined.rename(
                columns={
                    should_col_name: "should",
                    is_col_name: "is",
                }
            )

            # record legacy percentage difference key for backwards compatibility
            res[f"perc_diff_max_col_{col_save}"] = perc_max_difference

            metrics = _get_similarity_metrics(df_analysis)

            # check if the differences are within the tolerance
            within_tolerance = (
                df_combined["diff_abs"]
                .abs()
                .dropna()
                .le(settings.tolerance_numeric)
                .all()
            )
            missing_counterpart_values = (
                df_combined[[should_col_name, is_col_name]].isna().any(axis=1).any()
            )
            data_differs = missing_counterpart_values or not within_tolerance

            table_column_metrics.append(
                {
                    "col_name": col.strip(),
                    "data_differs": data_differs,
                    "abs_max_difference": abs_max_difference,
                    "perc_max_difference": perc_max_difference,
                    "abs_mean_difference": abs_mean_difference,
                    "perc_mean_difference": perc_mean_difference,
                    "MSE": metrics["MSE"],
                    "MAE": metrics["MAE"],
                    "MAPE": metrics["MAPE"],
                    "NSE": metrics["NSE"],
                    "R²": metrics["R²"],
                }
            )

            if data_differs:
                # write to file, putting in an extension with the column name
                safe_col = col.replace(" ", "_").replace("/", "_").replace("\\", "_")
                fn_col = (
                    os.path.splitext(fn_delta)[0]
                    + f"_{safe_col}"
                    + os.path.splitext(fn_delta)[1]
                )

                # any multiple of underscores in a row should be replaced with a single underscore
                fn_col = "_".join(filter(None, fn_col.split("_")))
                # same for dots in the filename (except for the extension)
                fn_col_stem, fn_col_ext = os.path.splitext(fn_col)
                fn_col = ".".join(filter(None, fn_col_stem.split("."))) + fn_col_ext

                # make certain the filename ending is .csv
                if not fn_col.endswith(".csv"):
                    fn_col = os.path.splitext(fn_col)[0] + ".csv"

                # create the directory if it doesn't exist
                os.makedirs(os.path.dirname(fn_col), exist_ok=True)
                df_combined.to_csv(fn_col, index=True)

                # generate column difference plots
                fn_figure = os.path.splitext(fn_col)[0] + ".png"
                compare_plots.plot_col_differences(df_analysis, col, fn_figure, metrics)

                flag_diff = True
                res[f"numeric_column_{col_save}"] = False
            else:
                res[f"numeric_column_{col_save}"] = True

    res["table_column_metrics"] = table_column_metrics

    res["files_different"] = flag_diff
    return res
