# Example & Test Models

This directory contains a series of models which, overall cover most functionality offered by SHETRAN.
It can also be used for change testing between different SHETRAN versions.

## Structure

Each model is in it's own directory beneath this one.
By default there are two subdirectories, "backup_input" and "output_should".
These contain, as named, the model input data for running it and the expected output.
This structure exists so that the comparison script can more easily run and compare all of the models.

## Models

Still to fill in what is special about each model.
There is a shorter readme with specifics in most model directories as well.


## Testing

The `check_results_consistency.py` script enables testing from one SHETRAN version to the next.
It iterates over all the subdirectories (as long as they don't have a underscore in front), builds the model and then compares the results to the expected output.
And differences are being written, per output file, into the `<model>/diff_delta/` subdirectory.
Also an overview file, `comparison_results.csv`, is written to the model directory.
It also writes an overview file with how long each computation took and whether differences in files were observed or not.

The difference directory contains:

- If it is a table, a csv file for each table column including differences.
  Also a graphic with both timeseries and the absolute as well as percentage delta.
- If it is a text-file, a git diff style file containing all differences between both of them.
- For an HDF5 file, a file containing the delta for each entry.

There are some settings in `check_results_consistency.py` to control it's behaviour:

- At the top of the file, there are values for adjusting the difference tolerance for numeric values in tables and the hdf5 file.
- Also at the top, there is a list with which models to ignore.
  This is useful for CI testing or when only testing a known bad model during development.
- The main calling function can take arguments whether a new simulation is done or not, and, similarly, whether an analysis is done or not.
  Can also be controlled via CLI parameters.

Currently, it is hard-coded to not compare files larger than 10MB.
