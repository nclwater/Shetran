#
# Variables and settings used for example model runs & comparisons
#

import os

# global variables
dir_inputs = "model"
dir_compute = "compute"
dir_results_should = "output_should"
dir_delta = "diff_delta"

# Model categories
list_long_running = [
    "38014-100m-SurfaceErrors", "Aire_at_Kildwick_Bridge-AllOptions-withdates",
    "Aire_at_Kildwick_Bridge-simple", "Cobres",
    "Cobres-ExtraOutputDischargePoints", "Cobres-ExtraOutputWaterTable",
    "Cobres1D", "dano100m", "dunsop", "dunsop-hot1", "dunsop-hot2",
    "foston100m", "reservoir-ZQmodule-example", "Slapton",
    "Slapton-1D-1year-nitrate", "Slapton-3D-1year-nitrate"
]
list_medium_runtime = [
    "38014-100m-SurfaceErrors", "Aire_at_Kildwick_Bridge-AllOptions-withdates",
    "Aire_at_Kildwick_Bridge-simple", "Cobres",
    "Cobres-ExtraOutputDischargePoints", "Cobres-ExtraOutputWaterTable",
    "Cobres1D", "dano100m", "dunsop", "dunsop-hot1", "dunsop-hot2",
    "foston100m", "reservoir-ZQmodule-example", "Slapton",
    "Slapton-1D-1year-nitrate", "Slapton-3D-1year-nitrate"
]
list_short_runtime = [
    "38014-100m-SurfaceErrors", "Aire_at_Kildwick_Bridge-AllOptions-withdates",
    "Aire_at_Kildwick_Bridge-simple", "Cobres",
    "Cobres-ExtraOutputDischargePoints", "Cobres-ExtraOutputWaterTable",
    "Cobres1D", "dano100m", "dunsop", "dunsop-hot1", "dunsop-hot2",
    "foston100m", "reservoir-ZQmodule-example", "Slapton",
    "Slapton-1D-1year-nitrate", "Slapton-3D-1year-nitrate"
]

# Indicators which files are for what comparison.
# TODO replace with pulling that from the model config files
list_table_files_indicators = [
    "_sim_daily.",
    "_sim_hourly.",
    "_discharge_sim_everytimestep.",
    "_discharge_sim_regulartimestep.",
    "_mb.water_table_depth.",
]

# Tolerances for comparisons
tolerance_numeric = 1e-5
tolerance_table = 0.001

# Filename settings
default_shetran_exe = fn_shetran = os.path.join("..", "build", "release",
                                                "bin", "shetran.exe")
fn_model_analysis = "model_analysis.csv"
fn_overall_analysis = "overall_analysis.csv"
fn_setup_overview = "setup_overview.csv"

# Other Settings
files_too_large_threshold = 10 * 1024 * 1024  # 10 MB
