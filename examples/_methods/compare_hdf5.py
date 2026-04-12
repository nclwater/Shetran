#
# Methods for comparing the SHETRAN HDF5 file output.
#

# General Imports
import os

# Package Imports
import h5py
import numpy as np
import pandas as pd
from typing import Any

# Local Imports
from . import compare_plots as compare_plots
from . import settings as settings


def _safe_abs_percentage(numerator: np.ndarray,
                         denominator: np.ndarray) -> np.ndarray:
    """
    Compute |numerator| / |denominator| and return NaN for invalid denominators.
    """
    num = np.abs(np.asarray(numerator, dtype=np.float64))
    den = np.abs(np.asarray(denominator, dtype=np.float64))
    valid = np.isfinite(num) & np.isfinite(den) & (den > 0)

    res = np.full_like(num, np.nan, dtype=np.float64)
    np.divide(num, den, out=res, where=valid)
    return res


def _copy_attrs(src_obj, dst_obj) -> None:
    """Copy all attributes from one HDF5 object to another."""
    for key, value in src_obj.attrs.items():
        dst_obj.attrs[key] = value


def _safe_path_token(path: str) -> str:
    token = path.strip("/").replace("/", "_").replace(" ", "_")
    token = token.replace("\\", "_")
    token = "_".join(filter(None, token.split("_")))
    return token if token else "root"


def _default_metric_row(path: str, differs: bool = False) -> dict:
    return {
        "data_item": path,
        "data_differs": differs,
        "same_row_count": True,
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


def _nse_r2_for_1d(data_should: np.ndarray, data_is: np.ndarray) -> tuple:
    """Compute NSE and R2 only for 1D timeseries where variance exists."""
    if data_should.ndim != 1 or data_is.ndim != 1:
        return (np.nan, np.nan)

    mask = np.isfinite(data_should) & np.isfinite(data_is)
    if not mask.any():
        return (np.nan, np.nan)

    should_valid = data_should[mask]
    is_valid = data_is[mask]
    ss_res = np.sum((should_valid - is_valid)**2)
    ss_tot = np.sum((should_valid - should_valid.mean())**2)
    if ss_tot == 0:
        return (np.nan, np.nan)

    nse = 1 - ss_res / ss_tot
    r2 = 1 - ss_res / ss_tot
    return (float(nse), float(r2))


def _plot_hdf5_timeseries(data_should: np.ndarray, data_is: np.ndarray,
                          path: str, fn_delta: str,
                          metrics: dict[str, Any]) -> None:
    """Create table-style timeseries plot for 1D HDF5 datasets."""
    if data_should.ndim != 1 or data_is.ndim != 1:
        return

    df_combined = pd.DataFrame({
        "should": data_should,
        "is": data_is,
    })
    df_combined["diff_abs"] = df_combined["should"] - df_combined["is"]
    df_combined[
        "diff_pct"] = df_combined["diff_abs"] / df_combined["should"].replace(
            0, pd.NA)

    safe_item = _safe_path_token(path)
    fn_figure = os.path.splitext(fn_delta)[0] + f"_{safe_item}.png"
    compare_plots.plot_col_differences(df_combined, path, fn_figure, metrics)


def _plot_hdf5_3d_maps(delta: np.ndarray, data_should: np.ndarray, path: str,
                       fn_delta: str) -> None:
    """Create map plots for 3D datasets (time, y, x) by collapsing time axis."""
    if delta.ndim != 3 or data_should.ndim != 3:
        return

    def _safe_nanmax_over_time(arr: np.ndarray) -> np.ndarray:
        """Compute nanmax along time axis without all-NaN slice warnings."""
        valid_any = np.isfinite(arr).any(axis=0)
        # Replace invalid values with -inf so reduction can proceed silently.
        sanitized = np.where(np.isfinite(arr), arr, -np.inf)
        reduced = np.max(sanitized, axis=0)
        reduced = np.where(valid_any, reduced, np.nan)
        return reduced

    def _safe_nanmean_over_time(arr: np.ndarray) -> np.ndarray:
        """Compute nanmean along time axis without empty-slice warnings."""
        valid = np.isfinite(arr)
        count = valid.sum(axis=0)
        summed = np.where(valid, arr, 0.0).sum(axis=0)
        with np.errstate(invalid="ignore", divide="ignore"):
            mean = summed / count
        mean = np.where(count > 0, mean, np.nan)
        return mean

    abs_diff = np.abs(delta)
    pct_diff = _safe_abs_percentage(delta, data_should)

    abs_max_map = _safe_nanmax_over_time(abs_diff)
    abs_mean_map = _safe_nanmean_over_time(abs_diff)
    pct_max_map = _safe_nanmax_over_time(pct_diff)
    pct_mean_map = _safe_nanmean_over_time(pct_diff)

    safe_item = _safe_path_token(path)
    base = os.path.splitext(fn_delta)[0] + f"_{safe_item}"

    compare_plots.plot_hdf5_map(
        abs_max_map,
        f"HDF5 abs max diff map: {path}",
        base + "_abs_max_map.png",
    )
    compare_plots.plot_hdf5_map(
        pct_max_map,
        f"HDF5 pct max diff map: {path}",
        base + "_pct_max_map.png",
    )
    compare_plots.plot_hdf5_map(
        abs_mean_map,
        f"HDF5 abs mean diff map: {path}",
        base + "_abs_mean_map.png",
    )
    compare_plots.plot_hdf5_map(
        pct_mean_map,
        f"HDF5 pct mean diff map: {path}",
        base + "_pct_mean_map.png",
    )


def _write_delta_dataset(ds_should, ds_is, f_delta, path: str,
                         data_for_delta: np.ndarray) -> None:
    """Write a differing dataset to the delta HDF5 and keep metadata."""
    parent_path, dataset_name = path.rsplit("/", 1)
    parent_group = (f_delta if parent_path in ("", "/") else
                    f_delta.require_group(parent_path))

    if dataset_name in parent_group:
        del parent_group[dataset_name]

    dst = parent_group.create_dataset(dataset_name, data=data_for_delta)
    _copy_attrs(ds_should, dst)


def _compare_datasets(ds_should, ds_is, f_delta, path, diffs,
                      tolerance_numeric, fn_delta) -> dict:
    """
    Helper function to compare two HDF5 datasets and write deltas if appropriate.
    """

    metric_row = _default_metric_row(path)

    # Interpret "row count" as timestep count (first dimension when present).
    if ds_should.ndim > 0 and ds_is.ndim > 0:
        metric_row["same_row_count"] = ds_should.shape[0] == ds_is.shape[0]

    if ds_should.shape != ds_is.shape:
        diffs["shape_mismatch"].append(path)
        metric_row["data_differs"] = True

        # Guard requested by user: timestep mismatch => differs with NaN metrics.
        if ds_should.ndim > 0 and ds_is.ndim > 0 and ds_should.shape[
                0] != ds_is.shape[0]:
            metric_row["same_row_count"] = False

        # Keep differing structure/datasets in delta output.
        _write_delta_dataset(ds_should, ds_is, f_delta, path, ds_is[()])
        diffs["hdf5_dataset_metrics"].append(metric_row)
        return diffs

    if ds_should.dtype != ds_is.dtype:
        diffs["dtype_mismatch"].append(path)

    if not (np.issubdtype(ds_should.dtype, np.number)
            and np.issubdtype(ds_is.dtype, np.number)):
        if not np.array_equal(ds_should[()], ds_is[()]):
            diffs["non_numeric_diff"].append(path)
            metric_row["data_differs"] = True
            _write_delta_dataset(ds_should, ds_is, f_delta, path, ds_is[()])

        diffs["hdf5_dataset_metrics"].append(metric_row)
        return diffs

    # Load numeric data into memory
    data_should = np.asarray(ds_should[()], dtype=np.float64)
    data_is = np.asarray(ds_is[()], dtype=np.float64)

    delta = data_is - data_should
    abs_diff = np.abs(delta)
    pct_diff = _safe_abs_percentage(delta, data_should)

    if np.isfinite(abs_diff).any():
        metric_row["abs_max_difference"] = float(np.nanmax(abs_diff))
        metric_row["abs_mean_difference"] = float(np.nanmean(abs_diff))

    if np.isfinite(pct_diff).any():
        metric_row["perc_max_difference"] = float(np.nanmax(pct_diff))
        metric_row["perc_mean_difference"] = float(np.nanmean(pct_diff))

    valid_mask = np.isfinite(data_should) & np.isfinite(data_is)
    if valid_mask.any():
        should_valid = data_should[valid_mask]
        is_valid = data_is[valid_mask]
        metric_row["MSE"] = float(np.mean((should_valid - is_valid)**2))
        metric_row["MAE"] = float(np.mean(np.abs(should_valid - is_valid)))

        mape_mask = should_valid != 0
        if mape_mask.any():
            metric_row["MAPE"] = float(
                np.mean(
                    np.abs((is_valid[mape_mask] - should_valid[mape_mask]) /
                           should_valid[mape_mask])) * 100)

    nse, r2 = _nse_r2_for_1d(data_should, data_is)
    metric_row["NSE"] = nse
    metric_row["R²"] = r2

    # Check if they are equal within the given absolute tolerance.
    # rtol=0.0 ensures we are only strictly checking against the absolute tolerance_numeric.
    # equal_nan=True ensures NaNs in the exact same spot are considered "equal".
    if not np.allclose(
            data_should, data_is, atol=tolerance_numeric, rtol=0.0,
            equal_nan=True):
        diffs["numeric_diff"].append(path)
        metric_row["data_differs"] = True

        # Optional aggregate lists retained for backwards compatibility.
        if np.isfinite(pct_diff).any():
            diffs["perc_diff_hdf5_max_list"].append(float(np.nanmax(pct_diff)))
            diffs["perc_diff_hdf5_mean_list"].append(
                float(np.nanmean(pct_diff)))
        else:
            diffs["perc_diff_hdf5_max_list"].append(float("nan"))
            diffs["perc_diff_hdf5_mean_list"].append(float("nan"))

        sum_abs_delta = np.nansum(abs_diff)
        sum_abs_should = np.nansum(np.abs(data_should))
        if np.isfinite(sum_abs_delta) and np.isfinite(
                sum_abs_should) and sum_abs_should > 0:
            diffs["perc_diff_hdf5_sum_abs_list"].append(
                float(sum_abs_delta / sum_abs_should))
        else:
            diffs["perc_diff_hdf5_sum_abs_list"].append(float("nan"))

        _write_delta_dataset(ds_should, ds_is, f_delta, path, delta)

        # Plots for differing datasets.
        if data_should.ndim == 3:
            _plot_hdf5_3d_maps(delta, data_should, path, fn_delta)
        if data_should.ndim == 1:
            _plot_hdf5_timeseries(data_should, data_is, path, fn_delta,
                                  metric_row)

    diffs["hdf5_dataset_metrics"].append(metric_row)
    return diffs


def _compare_groups(group_should, group_is, f_delta, current_path, diffs,
                    tolerance_numeric, fn_delta) -> dict:
    """
    Recursive helper function to traverse and compare HDF5 groups.
    """
    keys_should = set(group_should.keys())
    keys_is = set(group_is.keys())

    # 1. Record missing paths
    for k in keys_should - keys_is:
        missing_path = f"{current_path}{k}"
        diffs["missing_in_is"].append(missing_path)
        metric_row = _default_metric_row(missing_path, differs=True)
        metric_row["same_row_count"] = False
        diffs["hdf5_dataset_metrics"].append(metric_row)

    for k in keys_is - keys_should:
        missing_path = f"{current_path}{k}"
        diffs["missing_in_should"].append(missing_path)
        metric_row = _default_metric_row(missing_path, differs=True)
        metric_row["same_row_count"] = False
        diffs["hdf5_dataset_metrics"].append(metric_row)

    # 2. Iterate through paths that exist in both files
    common_keys = keys_should & keys_is

    for k in common_keys:
        path = f"{current_path}{k}" if current_path == "/" else f"{current_path}/{k}"

        obj_should = group_should[k]
        obj_is = group_is[k]

        if isinstance(obj_should, h5py.Group) and isinstance(
                obj_is, h5py.Group):
            diffs = _compare_groups(obj_should, obj_is, f_delta, path, diffs,
                                    tolerance_numeric, fn_delta)

        elif isinstance(obj_should, h5py.Dataset) and isinstance(
                obj_is, h5py.Dataset):
            diffs = _compare_datasets(obj_should, obj_is, f_delta, path, diffs,
                                      tolerance_numeric, fn_delta)

        else:
            diffs["type_mismatch"].append(path)
            metric_row = _default_metric_row(path, differs=True)
            metric_row["same_row_count"] = False
            diffs["hdf5_dataset_metrics"].append(metric_row)

    return diffs


def compare_hdf5(fn_should: str, fn_is: str, fn_delta: str) -> dict:

    structural_diffs: dict[str, Any] = {
        "missing_in_is": [],
        "missing_in_should": [],
        "type_mismatch": [],
        "shape_mismatch": [],
        "dtype_mismatch": [],
        "non_numeric_diff": [],
        "numeric_diff": [],
        "perc_diff_hdf5_max_list": [],
        "perc_diff_hdf5_mean_list": [],
        "perc_diff_hdf5_sum_abs_list": [],
        "hdf5_dataset_metrics": [],
    }

    os.makedirs(os.path.dirname(fn_delta), exist_ok=True)

    with (
            h5py.File(fn_should, "r") as f_should,
            h5py.File(fn_is, "r") as f_is,
            h5py.File(fn_delta, "w") as f_delta,
    ):
        # Start the recursive comparison, passing the tolerance along
        structural_diffs = _compare_groups(f_should, f_is, f_delta, "/",
                                           structural_diffs,
                                           settings.tolerance_numeric,
                                           fn_delta)

    # If there were no differences at all, remove the empty delta file.
    has_any_differences = any(
        row.get("data_differs", False)
        for row in structural_diffs["hdf5_dataset_metrics"])
    if not has_any_differences and os.path.exists(fn_delta):
        os.remove(fn_delta)

    # Keep all dataset metrics and only non-empty diff categories.
    res = {
        k: v
        for k, v in structural_diffs.items()
        if k == "hdf5_dataset_metrics" or len(v) > 0
    }
    res["files_different"] = has_any_differences
    return res
