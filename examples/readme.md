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

## Workflow

The example output files are generally too large to store in Git.
Because of that, the expected ("correct") output files must be created locally first.

Recommended sequence:

1. Generate baseline expected outputs with `setup_results_check.py`.
2. Run `check_results_consistency.py` to compare new runs against those expected outputs.

## CLI Usage

### setup_results_check.py

Creates/refreshes the expected output files in each model `output_should` directory.

- `-m`, `--model <name>`: Select a model explicitly. Repeatable.
  If used, this overrides `-l`.
- `-l`, `--list {all,long,medium,short}`: Select a predefined model set.
  Default is `short`.
- `-e`, `--exe <path>`: Path to SHETRAN executable.
- `--use-release-exe`: Use `settings.default_shetran_exe`.
  Overridden by `-e` if both are given.

Examples:

- `python setup_results_check.py --use-release-exe`
- `python setup_results_check.py -l medium --use-release-exe`
- `python setup_results_check.py -m Cobres -m dunsop -e ..\\build\\release\\bin\\shetran.exe`

### check_results_consistency.py

Runs simulations and/or compares produced outputs against `output_should`.

- `-m`, `--model <name>`: Select a model explicitly. Repeatable.
  If used, this overrides `-l`.
- `-l`, `--list {all,long,medium,short}`: Select a predefined model set.
  Default is `short`.
- `--shetran-exe <path>`: Path to SHETRAN executable.
  Default is `settings.default_shetran_exe`.
- `--skip-simulation`: Do only comparisons.
- `--skip-comparison`: Do only simulations.

Per-model differences are written to `<model>/diff_delta/`.
Each model also gets `comparison_results.csv`, and an overall summary is written to `comparison_overview.csv`.

The difference directory contains:

- If it is a table, a csv file for each table column including differences.
  Also a graphic with both timeseries and the absolute as well as percentage delta.
- If it is a text-file, a git diff style file containing all differences between both of them.
- For an HDF5 file, a file containing the delta for each entry.

Currently, files larger than 10 MB are not compared.
