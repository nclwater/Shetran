#
# Checks the consistency of the results between different SHETRAN versions.
# This is done by comparing simulation results with the known good ones.
#
# TODO replace the pattern filtering by parsing config files and getting the output type filesnames from there.

# general imports
import datetime
import glob
import hashlib
import os
import pandas as pd
import shutil
import difflib
import subprocess

# other packages
import h5py
import itertools
import numpy as np

# global variables
dir_inputs = "backup_input"
dir_compute = "test_compute"
dir_results_should = "output_should"
dir_delta = "diff_delta"

# settings
list_long_running = [
    "dano100m",  # very large area, long runtime
    # "foston100m",   # large area, long runtime
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


# functions --------------------------------------------------------------------
def _compare_h5(
    name: str,
    obj,
    errors: list,
    file_sample: h5py.File,
    file_delta,
    path_should: str,
    path_sample: str,
):
    """Compare HDF5 objects and write differences to a delta file."""
    if isinstance(obj, h5py.Group):
        if file_delta is not None:
            file_delta.require_group(name)
        return

    if isinstance(obj, h5py.Dataset):
        if name not in file_sample:
            errors.append(
                f"    Dataset {name} in {path_should} but not in {path_sample}."
            )
            return

        data_should = obj[()]
        data_sample = file_sample[name][()]

        is_numeric = np.issubdtype(data_should.dtype,
                                   np.number) and np.issubdtype(
                                       data_sample.dtype, np.number)

        equal = False
        if data_should.shape != data_sample.shape:
            errors.append(
                f"    Dataset {name} shape differs: {data_should.shape} vs {data_sample.shape}"
            )
        elif is_numeric:
            equal = np.allclose(data_should,
                                data_sample,
                                atol=tolerance_numeric,
                                rtol=0,
                                equal_nan=True)
        else:
            try:
                # Element-wise comparison for arrays
                equal = (data_should == data_sample).all()
            except AttributeError:
                # Scalar comparison
                equal = data_should == data_sample

        if not equal:
            errors.append(
                f"    Dataset {name} differs between {path_sample} and {path_should}.\n"
                f"      Shape: {data_should.shape} vs {data_sample.shape}\n"
                f"      Values: {data_should} vs {data_sample}")
            if is_numeric and data_should.shape == data_sample.shape:
                if file_delta is not None:
                    diff_data = data_sample - data_should
                    file_delta.create_dataset(name, data=diff_data)


def _md5(path):
    h = hashlib.md5()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()


def _check_outputs_consistency(dir_sample: str):

    # get all files in dir_results_should
    files_should = set(os.listdir(os.path.join(dir_sample,
                                               dir_results_should)))

    # get all files in the compute directory
    files_sample = set(os.listdir(os.path.join(dir_sample, dir_compute)))

    # keep a deterministic order for iteration
    files_should_sorted = sorted(files_should)
    files_sample_sorted = sorted(files_sample)

    # firstly, check the filenames
    missing = files_should - files_sample
    if missing:
        print(
            f"  Files in {dir_results_should} but missing from {dir_sample}: {missing}"
        )
        return False

    # secondly, check the md5 sums of each file
    # if they differ, check the delta for text files (.txt ending)
    # and the contents for the hdf5 binary files (.hdf5 ending, numberic values
    # within 0.0001 tolerance)
    overall_ok = True
    for f in files_should_sorted:
        if f.casefold() == "temporary.txt".casefold():
            continue

        print(f"\n  Checking file {f}...")

        if not os.path.exists(os.path.join(dir_sample, dir_compute, f)):
            print(
                f"  File {f} in {dir_results_should} but not in {dir_sample}.")
            overall_ok = False
            continue

        ok = _compare_file_contents(dir_sample, f, _md5)
        if not ok:
            overall_ok = False

    return overall_ok


def _compare_file_contents(dir_sample, f, _md5):
    path_should = os.path.join(dir_sample, dir_results_should, f)
    path_sample = os.path.join(dir_sample, dir_compute, f)

    md5_should = _md5(path_should)
    md5_sample = _md5(path_sample)

    if md5_should == md5_sample:
        return True

    # Compare file contents
    if f.endswith(".txt") or f.endswith(".csv"):
        # check sizes and if either is too big, skip the detailed diff and just create a marker file
        if (os.path.getsize(path_should) > files_too_large_threshold
                or os.path.getsize(path_sample) > files_too_large_threshold):
            path_delta = os.path.join(dir_sample, dir_delta,
                                      f"file_too_large_diff_{f}")
            open(path_delta, "w").close()
            print(
                f"    File too large for diff, created empty marker at: {path_delta}"
            )
            return False

        # Read text with utf-8-sig to normalize BOM and use universal newlines
        with (
                open(path_should,
                     "r",
                     encoding="utf-8-sig",
                     errors="replace",
                     newline=None) as file_should,
                open(path_sample,
                     "r",
                     encoding="utf-8-sig",
                     errors="replace",
                     newline=None) as file_sample,
        ):
            lines_should = file_should.read().splitlines()
            lines_sample = file_sample.read().splitlines()

        if lines_should == lines_sample:
            return True

        is_table = any(ind in f for ind in list_table_files_indicators)
        has_diff = True
        first_diff_line_idx = -1
        first_diff_should = None
        first_diff_sample = None
        diff_lines = []

        if is_table:
            has_diff = False
            headers_should = lines_should[1].split(",") if len(
                lines_should) > 1 else []
            date_time_cols = [
                idx for idx, h in enumerate(headers_should)
                if "date" in h.lower() or "time" in h.lower()
            ]

            if len(lines_should) > 1:
                diff_lines.append(lines_should[1] + "\n")

            for i, (l_should, l_sample) in enumerate(
                    itertools.zip_longest(lines_should[2:],
                                          lines_sample[2:],
                                          fillvalue=None),
                    start=3,
            ):
                if l_should == l_sample:
                    continue

                row_match = True
                if l_should is None or l_sample is None:
                    row_match = False
                else:
                    vals_should = l_should.split(",")
                    vals_sample = l_sample.split(",")
                    if len(vals_should) != len(vals_sample):
                        row_match = False
                    else:
                        for col_idx, (v_should, v_sample) in enumerate(
                                zip(vals_should, vals_sample)):
                            if col_idx in date_time_cols:
                                if v_should.strip() != v_sample.strip():
                                    row_match = False
                                    break
                            else:
                                try:
                                    if (abs(float(v_should) - float(v_sample))
                                            > tolerance_table):
                                        row_match = False
                                        break
                                except ValueError:
                                    if v_should.strip() != v_sample.strip():
                                        row_match = False
                                        break

                if not row_match:
                    has_diff = True
                    diff_lines.append(f"< {l_should}\n> {l_sample}\n")
                    if first_diff_line_idx == -1:
                        first_diff_line_idx = i
                        first_diff_should = l_should
                        first_diff_sample = l_sample

            if not has_diff:
                return True
        else:
            diff = difflib.unified_diff(
                lines_should,
                lines_sample,
                fromfile=path_should,
                tofile=path_sample,
                lineterm="",
            )
            diff_lines = [line + "\n" for line in diff]
            for i, (l_should, l_sample) in enumerate(
                    itertools.zip_longest(lines_should,
                                          lines_sample,
                                          fillvalue=None),
                    start=1,
            ):
                if l_should != l_sample:
                    first_diff_line_idx = i
                    first_diff_should = l_should
                    first_diff_sample = l_sample
                    break

        print(f"  File {f} differs between "
              f"{os.path.dirname(path_should)} and "
              f"{os.path.dirname(path_sample)}.")
        size_should = os.path.getsize(path_should)
        size_sample = os.path.getsize(path_sample)
        print(f"    Size: {size_should} vs {size_sample}")
        print(f"    MD5: {md5_should} vs {md5_sample}")

        if (size_should >= files_too_large_threshold
                or size_sample >= files_too_large_threshold):
            path_delta = os.path.join(dir_sample, dir_delta,
                                      f"file_too_large_diff_{f}")
            open(path_delta, "w").close()
            print(
                f"    File too large for diff, created empty marker at: {path_delta}"
            )
        else:
            path_delta = os.path.join(dir_sample, dir_delta, f"diff{f}")
            with open(path_delta, "w", encoding="utf-8") as f_delta:
                f_delta.writelines(diff_lines)
            print(f"    Text diff file created at: {path_delta}")

        if first_diff_line_idx != -1:
            print(
                f"    Line {first_diff_line_idx} differs: {first_diff_should!r}"
                f" vs {first_diff_sample!r}")
    elif f.endswith(".hdf5") or f.endswith(".h5"):
        size_should = os.path.getsize(path_should)
        size_sample = os.path.getsize(path_sample)
        is_too_large = (size_should >= 10 * 1024 * 1024
                        or size_sample >= 10 * 1024 * 1024)

        if is_too_large:
            path_delta = os.path.join(dir_sample, dir_delta,
                                      f"file_too_large_diff_{f}")
            with (
                    h5py.File(path_should, "r") as file_should,
                    h5py.File(path_sample, "r") as file_sample,
            ):
                errors = []
                file_should.visititems(lambda name, obj: _compare_h5(
                    name, obj, errors, file_sample, None, path_should,
                    path_sample))

            if not errors:
                return True

            open(path_delta, "w").close()
            print(f"  File {f} differs between "
                  f"{os.path.dirname(path_should)} and "
                  f"{os.path.dirname(path_sample)}.")
            for error in errors:
                print(error)
            print(
                f"    File too large for detailed diff, created empty marker at: {path_delta}"
            )
        else:
            path_delta = os.path.join(dir_sample, dir_delta, f"diff{f}")
            with (
                    h5py.File(path_should, "r") as file_should,
                    h5py.File(path_sample, "r") as file_sample,
                    h5py.File(path_delta, "w") as file_delta,
            ):
                errors = []
                file_should.visititems(lambda name, obj: _compare_h5(
                    name,
                    obj,
                    errors,
                    file_sample,
                    file_delta,
                    path_should,
                    path_sample,
                ))

            if not errors:
                os.remove(path_delta)
                return True

            print(f"  File {f} differs between "
                  f"{os.path.dirname(path_should)} and "
                  f"{os.path.dirname(path_sample)}.")
            for error in errors:
                print(error)
            print(f"    HDF5 difference file created at: {path_delta}")
    else:
        print(f"  File {f} differs between "
              f"{os.path.dirname(path_should)} and "
              f"{os.path.dirname(path_sample)}.")
        print(f"    MD5 sums differ: {md5_should} vs {md5_sample}")
    return False


def _run_shetran(fn_shetran: str, dir_sample: str):

    # remove the existing files in the compute directory
    if not os.path.exists(os.path.join(dir_sample, dir_compute)):
        os.makedirs(os.path.join(dir_sample, dir_compute))
    else:
        for f in os.listdir(os.path.join(dir_sample, dir_compute)):
            os.remove(os.path.join(dir_sample, dir_compute, f))

    # remove, even if not empty, the dir_delta directory
    dir_delta_sample = os.path.join(dir_sample, dir_delta)
    if os.path.exists(dir_delta_sample):
        shutil.rmtree(dir_delta_sample)
    os.makedirs(dir_delta_sample)

    # copy all the inputs from dir_inputs to the compute directory
    dir_input_sample = os.path.join(dir_sample, dir_inputs)
    dir_compute_sample = os.path.join(dir_sample, dir_compute)
    for f in os.listdir(dir_input_sample):
        shutil.copy2(os.path.join(dir_input_sample, f),
                     os.path.join(dir_compute_sample, f))

    # run shetran in the compute directory
    # find the rundata file (must match rundata_*.txt per the source code requirement)
    rundata_files = glob.glob(os.path.join(dir_compute_sample,
                                           "rundata_*.txt"))
    if not rundata_files:
        print(f"  No rundata_*.txt file found in {dir_sample}, skipping.")
        return None

    # run the model
    rundata_file = os.path.abspath(rundata_files[0])

    t_start = datetime.datetime.now()
    subprocess.run(
        [fn_shetran, "-f", rundata_file],
        cwd=os.path.abspath(dir_compute_sample),
        check=True,
    )
    t_end = datetime.datetime.now()

    return t_end - t_start


def check_results_consistency(fn_shetran: str, full_run: bool = False):

    # get all subdirectories in the current directory (sorted deterministically)
    subdirectories = sorted([d for d in os.listdir() if os.path.isdir(d)])

    # if reduced, filter out the long-running simulations
    if not full_run:
        long_running_folded = {name.casefold() for name in list_long_running}
        subdirectories = [
            d for d in subdirectories
            if d.casefold() not in long_running_folded
        ]

    # loop through each subdirectory and work
    data_overview = {}
    for subdir in subdirectories:
        if subdir.startswith("_"):
            continue

        print("\n\n-----------------------------------------------------"
              "---------------------------")
        print(f"Checking consistency for {subdir}...")
        # run shetran in the subdirectory
        duration = _run_shetran(fn_shetran, subdir)

        # compare the results with the known good ones
        consistent = None
        consistent = _check_outputs_consistency(subdir)
        # if consistent:
        #     print(
        #         f"  Results in {subdir} are consistent with {dir_results_should}."
        #     )
        # else:
        #     print(f"  Results in {subdir} differ from {dir_results_should}.")
        data_overview[subdir] = {
            "duration": duration,
            "consistent": consistent
        }

    data_overview_df = pd.DataFrame.from_dict(data_overview, orient="index")
    data_overview_df.to_csv("results_consistency_check_overview.csv")


if __name__ == "__main__":
    # fn_shetran = r"C:\\Users\\tolstoi\\Documents\\Code\\shetran_rework\\build\\debug\\bin\\shetran.exe"
    # fn_shetran = r"C:\\Users\\tolstoi\\Documents\\Code\\shetran_rework\\build\\release\\bin\\shetran.exe"
    fn_shetran = os.path.join("..", "build", "release", "bin", "shetran.exe")
    # fn_shetran = os.path.join("_official_exe", "Shetran.exe")

    check_results_consistency(fn_shetran)

    # _run_shetran(
    #     fn_shetran,
    #     "Cobres")
    # _run_shetran(
    #     fn_shetran,
    #     "Cobres")
    # _run_shetran(
    #     fn_shetran,
    #     "Dunsop")
    # _run_shetran(fn_shetran, "reservoir-ZQmodule-example")
    # _run_shetran(fn_shetran, "Slapton")

    # _check_outputs_consistency("Cobres")
    # _check_outputs_consistency("Dunsop")
    # _check_outputs_consistency("reservoir-ZQmodule-example")
    # _check_outputs_consistency("Slapton")

    pass
