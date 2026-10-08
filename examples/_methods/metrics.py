#
# Goodness-of-fit and difference metrics shared by the table and HDF5
# comparisons.
#
# Conventions follow common hydrological practice, with the expected results
# ("should") taken as the reference/observation O and the new results ("is")
# as the simulation S:
#
#   difference      S - O (same sign convention for tables and HDF5)
#   relative diff.  100 * (S - O) / O                              [%]
#   RMSE            sqrt(mean((S - O)^2))                          [units]
#   MAE             mean(|S - O|)                                  [units]
#   MAPE            100 * mean(|S - O| / |O|), O != 0               [%]
#   PBIAS           100 * sum(O - S) / sum(O)                       [%]
#                   (Moriasi et al. 2007; positive = underestimation)
#   NSE             1 - sum((S - O)^2) / sum((O - mean(O))^2)
#                   (Nash & Sutcliffe 1970)
#   R²              squared Pearson correlation of O and S
#                   (Legates & McCabe 1999; Moriasi et al. 2007)
#   KGE             1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2),
#                   alpha = sd(S)/sd(O), beta = mean(S)/mean(O)
#                   (Gupta et al. 2009)
#
# All percentages are expressed in percent (not as fractions).
#

# Package Imports
import numpy as np

# column order used in all comparison outputs
METRIC_KEYS = [
    "abs_max_difference",
    "perc_max_difference",
    "abs_mean_difference",
    "perc_mean_difference",
    "RMSE",
    "MAE",
    "MAPE",
    "PBIAS",
    "NSE",
    "R²",
    "KGE",
]


def empty_metrics() -> dict:
    """Return all metric keys set to NaN."""
    return {key: np.nan for key in METRIC_KEYS}


def relative_difference_pct(should: np.ndarray, is_: np.ndarray) -> np.ndarray:
    """
    Element-wise 100 * (is - should) / |should| in percent.

    Entries where both values are zero are 0; entries with a zero (or
    non-finite) reference otherwise are NaN.
    """
    should = np.asarray(should, dtype=np.float64)
    is_ = np.asarray(is_, dtype=np.float64)
    delta = is_ - should

    res = np.full_like(delta, np.nan, dtype=np.float64)
    valid = np.isfinite(delta) & np.isfinite(should) & (should != 0)
    np.divide(100.0 * delta, np.abs(should), out=res, where=valid)
    res[(should == 0) & (is_ == 0)] = 0.0
    return res


def _nanmax_abs(arr: np.ndarray) -> float:
    arr = np.abs(arr[np.isfinite(arr)])
    return float(arr.max()) if arr.size else np.nan


def _nanmean_abs(arr: np.ndarray) -> float:
    arr = np.abs(arr[np.isfinite(arr)])
    return float(arr.mean()) if arr.size else np.nan


def compute_metrics(should: np.ndarray, is_: np.ndarray,
                    goodness_of_fit: bool = True) -> dict:
    """
    Compute all metrics in METRIC_KEYS for paired data.

    Pairs with a non-finite value on either side are ignored. The
    goodness-of-fit statistics (NSE, R², KGE) are only meaningful for
    timeseries and can be switched off for other (e.g. spatial) data.
    """
    should = np.asarray(should, dtype=np.float64).ravel()
    is_ = np.asarray(is_, dtype=np.float64).ravel()
    res = empty_metrics()

    mask = np.isfinite(should) & np.isfinite(is_)
    if not mask.any():
        return res
    obs = should[mask]
    sim = is_[mask]
    err = sim - obs

    rel = relative_difference_pct(obs, sim)
    res["abs_max_difference"] = _nanmax_abs(err)
    res["abs_mean_difference"] = _nanmean_abs(err)
    res["perc_max_difference"] = _nanmax_abs(rel)
    res["perc_mean_difference"] = _nanmean_abs(rel)

    res["RMSE"] = float(np.sqrt(np.mean(err**2)))
    res["MAE"] = float(np.mean(np.abs(err)))

    nonzero = obs != 0
    if nonzero.any():
        res["MAPE"] = float(100.0 * np.mean(np.abs(err[nonzero] / obs[nonzero])))

    sum_obs = np.sum(obs)
    if sum_obs != 0:
        res["PBIAS"] = float(100.0 * np.sum(obs - sim) / sum_obs)

    if not goodness_of_fit:
        return res

    ss_tot = np.sum((obs - obs.mean())**2)
    if ss_tot > 0:
        res["NSE"] = float(1.0 - np.sum(err**2) / ss_tot)

    sd_obs = obs.std()
    sd_sim = sim.std()
    if sd_obs > 0 and sd_sim > 0:
        r = float(np.corrcoef(obs, sim)[0, 1])
        res["R²"] = r**2
        mean_obs = obs.mean()
        if mean_obs != 0:
            alpha = sd_sim / sd_obs
            beta = sim.mean() / mean_obs
            res["KGE"] = float(1.0 - np.sqrt((r - 1.0)**2 + (alpha - 1.0)**2 +
                                             (beta - 1.0)**2))

    return res
