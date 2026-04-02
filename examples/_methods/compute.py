#
# Functions for dealing with running SHETRAN
#

# General Imports
import datetime
import glob
import os
import shutil
import subprocess

# Local Imports
from . import util


def run_model(fn_shetran: str,
              dir_input: str,
              dir_simulation: str,
              dir_diff: str | None = None) -> dict:

    # delta & compute directories
    util.remove_non_empty_dir(dir_simulation, flag_makeIt=True)
    if dir_diff is not None:
        util.remove_non_empty_dir(dir_diff)

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
