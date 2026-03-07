#
# Checks the consistency of the results between different SHETRAN versions.
# This is done by comparing simulation results with the known good ones.
#

# general imports
import glob
import hashlib
import os
import shutil
import subprocess

# other packages
import h5py
import numpy as np

# global variables
dir_inputs = "backup_input"
dir_compute = "test_compute"
dir_results_should = "output_should"

# functions --------------------------------------------------------------------


def _md5(path):
    h = hashlib.md5()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(8192), b""):
            h.update(chunk)
    return h.digest()


def _check_outputs_consistency(dir_sample: str):

    # get all files in dir_results_should
    files_should = set(os.listdir(os.path.join(dir_sample,
                                               dir_results_should)))

    # get all files in the compute directory
    files_sample = set(os.listdir(os.path.join(dir_sample, dir_compute)))

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
    for f in files_should:
        if not os.path.exists(os.path.join(dir_sample, dir_compute, f)):
            print(
                f"  File {f} in {dir_results_should} but not in {dir_sample}.")
            continue

        return _compare_file_contents(dir_sample, f, _md5)


def _compare_file_contents(dir_sample, f, _md5):
    md5_should = _md5(os.path.join(dir_sample, dir_results_should, f))
    md5_sample = _md5(os.path.join(dir_sample, dir_compute, f))

    if md5_should != md5_sample:

        print(f"  File {f} differs between "
              f"{os.path.join(dir_sample, dir_results_should)} and "
              f"{os.path.join(dir_sample, dir_compute)}.")

        # Compare file contents
        # TODO add delta computation for the text files.
        if f.endswith(".txt") or f.endswith(".csv"):
            with open(os.path.join(dir_results_should, f),
                      "r") as file_should, open(
                          os.path.join(dir_sample, dir_compute, f),
                          "r") as file_sample:
                for line_should, line_sample in zip(file_should, file_sample):
                    if line_should.rstrip('\r\n') != line_sample.rstrip(
                            '\r\n'):
                        print(
                            f"    Line {line_should.strip()} differs from {line_sample.strip()}."
                        )
        elif f.endswith(".hdf5"):
            with h5py.File(os.path.join(dir_results_should, f),
                           "r") as file_should, h5py.File(
                               os.path.join(dir_sample, dir_compute, f),
                               "r") as file_sample:
                for key in file_should.keys():
                    if key not in file_sample:
                        print(
                            f"    Dataset {key} in {dir_results_should} but not in {dir_sample}."
                        )
                        continue
                    data_should = file_should[key][()]
                    data_sample = file_sample[key][()]
                    if data_should.shape != data_sample.shape:
                        equal = False
                    elif np.issubdtype(data_should.dtype, np.number):
                        equal = np.allclose(data_should,
                                            data_sample,
                                            atol=1e-5,
                                            rtol=0,
                                            equal_nan=True)
                    else:
                        equal = (data_should == data_sample).all()
                    if not equal:
                        print(
                            f"    Dataset {key} differs between {os.path.join(dir_sample, dir_compute)} and {os.path.join(dir_sample, dir_results_should)}."
                        )
                        print(
                            f"      Shape: {data_should.shape} vs {data_sample.shape}"
                        )
                        print(f"      Values: {data_should} vs {data_sample}")
        else:
            print(f"    MD5 sums differ: {md5_should} vs {md5_sample}")
        return False


def _run_shetran(fn_shetran: str, dir_sample: str):

    # remove the existing files in the compute directory
    if not os.path.exists(os.path.join(dir_sample, dir_compute)):
        os.makedirs(os.path.join(dir_sample, dir_compute))
    else:
        for f in os.listdir(os.path.join(dir_sample, dir_compute)):
            os.remove(os.path.join(dir_sample, dir_compute, f))

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
        return
    rundata_file = os.path.abspath(rundata_files[0])
    subprocess.run([fn_shetran, "-f", rundata_file],
                   cwd=os.path.abspath(dir_compute_sample),
                   check=True)


def check_results_consistency(fn_shetran: str):

    # get all subdirectories in the current directory
    subdirectories = [d for d in os.listdir() if os.path.isdir(d)]

    # loop through each subdirectory and work
    for subdir in subdirectories:

        print(f"Checking consistency for {subdir}...")

        # run shetran in the subdirectory
        _run_shetran(fn_shetran, subdir)

        # compare the results with the known good ones
        if _check_outputs_consistency(subdir):
            print(
                f"  Results in {subdir} are consistent with {dir_results_should}."
            )
        else:
            print(f"  Results in {subdir} differ from {dir_results_should}.")


if __name__ == "__main__":

    _check_outputs_consistency("Cobres")

    pass
