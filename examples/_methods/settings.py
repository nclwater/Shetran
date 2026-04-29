#
# Variables and settings used for example model runs & comparisons
#

import os

_METHODS_DIR = os.path.dirname(os.path.abspath(__file__))
_EXAMPLES_DIR = os.path.dirname(_METHODS_DIR)
_REPO_ROOT_DIR = os.path.dirname(_EXAMPLES_DIR)

# global variables
dir_inputs = "model"
dir_compute = "compute"
dir_results_should = "output_should"
dir_delta = "diff_delta"

#
# Model categories - these are cumulatively used in code, but separate here for
# clarity and to allow for easy modification. The categories are based on
# runtime, but could be modified to be based on other factors such as model
# complexity, size, etc.
#
# Guideline on my machine: <10min runtime = short, 10min-1h = medium, >1h = long.
#
list_long_running = [
    # "38014-100m-SurfaceErrors",
    # "Aire_at_Kildwick_Bridge-AllOptions-withdates",
    # "Aire_at_Kildwick_Bridge-simple",
    # "Cobres",
    # "Cobres-ExtraOutputDischargePoints",
    # "Cobres-ExtraOutputWaterTable",
    # "Cobres1D",
    "dano100m",  # very slow, suspected >10h runtime
    # "dunsop",
    # "dunsop-hot1",
    # "dunsop-hot2",
    # "foston100m",
    # "reservoir-ZQmodule-example",
    # "Slapton",
    # "Slapton-1D-1year-nitrate",
    # "Slapton-3D-1year-nitrate"
]
list_medium_runtime = [
    "38014-100m-SurfaceErrors",
    # "Aire_at_Kildwick_Bridge-AllOptions-withdates",
    # "Aire_at_Kildwick_Bridge-simple",
    # "Cobres",
    # "Cobres-ExtraOutputDischargePoints",
    # "Cobres-ExtraOutputWaterTable",
    # "Cobres1D",
    # # "dano100m",
    # "dunsop",
    # "dunsop-hot1",
    # "dunsop-hot2",
    "foston100m",
    "reservoir-ZQmodule-example",
    # "Slapton",
    # "Slapton-1D-1year-nitrate",
    # "Slapton-3D-1year-nitrate"
]
list_short_runtime = [
    # "38014-100m-SurfaceErrors",
    "Aire_at_Kildwick_Bridge-AllOptions-withdates",
    "Aire_at_Kildwick_Bridge-simple",
    "Cobres",
    "Cobres-ExtraOutputDischargePoints",
    "Cobres-ExtraOutputWaterTable",
    "Cobres1D",
    # "dano100m",
    "dunsop",
    "dunsop-hot1",
    "dunsop-hot2",
    # "foston100m",
    # "reservoir-ZQmodule-example",
    "Slapton",
    "Slapton-1D-1year-nitrate",
    "Slapton-3D-1year-nitrate"
]

# Indicators which files are for what comparison.
# TODO replace with pulling that from the model config files
list_table_files_indicators = [
    "_sim_daily.",
    "_sim_hourly.",
    "_discharge_sim_everytimestep.",
    "_discharge_sim_regulartimestep.",
    "_mb.water_table_depth.",
    "_mb.txt",
]

# Tolerances for comparisons
tolerance_numeric = 1e-5
tolerance_table = 0.001

# Filename settings
shetran_executable_name = "shetran.exe" if os.name == "nt" else "shetran"
default_shetran_exe = fn_shetran = os.path.join(_REPO_ROOT_DIR, "build",
                                                "release", "bin",
                                                shetran_executable_name)
fn_model_analysis = "model_analysis.csv"
fn_overall_analysis = "overall_analysis.csv"
fn_setup_overview = "setup_overview.csv"

# Other Settings
files_too_large_threshold_text = 20 * 1024 * 1024
files_too_large_threshold_table = 50 * 1024 * 1024
files_too_large_threshold_hdf5 = 250 * 1024 * 1024
