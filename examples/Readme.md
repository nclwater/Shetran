# Example and Test Models

This directory contains SHETRAN example models that are used for:

- Functional coverage of model features.
- Regression testing between SHETRAN versions/builds.

The expected output files are not stored in Git because they are usually large.
You generate them locally and then compare fresh runs against them.

## Directory Layout Per Model

Each model has its own subdirectory (for example, `Cobres/`), containing:

- `model/`: input files used to run SHETRAN.
- `compute/`: simulation run directory created/refreshed by scripts.
- `output_should/`: expected output baseline.
- `diff_delta/`: detailed comparison output for detected differences.

Only `model/` is expected to exist initially.

## Prerequisites

Run commands from this `examples/` directory.

- A built SHETRAN executable (`shetran` on Linux/macOS, `shetran.exe` on Windows).
- Python 3 with required packages used by these scripts:
  - `pandas`, `numpy`, `h5py`, `matplotlib`, `tqdm`

Example install:

```bash
pip install pandas numpy h5py matplotlib tqdm
```

## Recommended Workflow

1. Create or refresh baseline outputs:

   ```bash
   python setup_results_check.py --use-release-exe
   ```

2. Run consistency checks against that baseline:

   ```bash
   python check_results_consistency.py
   ```

3. If needed, clean generated data:

   ```bash
   python cleanup.py
   ```

## Script Overview

### `setup_results_check.py`

Runs selected models and copies `output_*` files from `compute/` to `output_should/`.

Important behavior:

- Requires exactly one executable selection:
  - `-e/--exe <path>` or
  - `--use-release-exe` (uses `_methods/settings.py` default).
- `-m/--model` can be repeated and overrides `-l/--list`.
- Model list defaults to `short`.
- Writes runtime summary to `setup_overview.csv`.

CLI options:

- `-m`, `--model <name>`: Select model(s) explicitly (repeatable).
- `-l`, `--list {all,long,medium,short}`: Predefined model set (default: `short`).
- `-e`, `--exe <path>`: Path to SHETRAN executable.
- `--use-release-exe`: Use default executable from settings.

Examples:

```bash
python setup_results_check.py --use-release-exe
python setup_results_check.py -l medium --use-release-exe
python setup_results_check.py -m Cobres -m dunsop -e ../build/release/bin/shetran
```

### `check_results_consistency.py`

Runs simulations and/or compares `compute/` outputs with `output_should/`.

Important behavior:

- `-m/--model` can be repeated and overrides `-l/--list`.
- Default model list is `short`.
- By default does both simulation and comparison.
- `--skip-simulation` performs comparison only.
- `--skip-comparison` performs simulation only.
- Writes per-model summary to `<model>/comparison_results.csv`.
- Writes overall summary to `comparison_overview.csv`.

CLI options:

- `-m`, `--model <name>`: Select model(s) explicitly (repeatable).
- `-l`, `--list {all,long,medium,short}`: Predefined model set (default: `short`).
- `--shetran-exe <path>`: Path to SHETRAN executable (default from settings).
- `--skip-simulation`: Skip running SHETRAN.
- `--skip-comparison`: Skip file comparison.

Generated comparison details in `<model>/diff_delta/`:

- Table files: per-column delta CSV and plots.
- Text files: unified diff text output.
- HDF5 files: dataset-level delta tables and plots (where applicable).

### `update_should_results.py`

Copies current `compute/` outputs into `output_should/` for selected models.
Use this only when you intentionally accept new outputs as the new baseline.

Important behavior:

- Can optionally run simulations first with `--run-simulation`.
- If simulations are run, a runtime overview `update_results_overview.csv` is written.

CLI options:

- `-m`, `--model <name>`: Select model(s) explicitly (repeatable).
- `-l`, `--list {all,long,medium,short}`: Predefined model set (default: `short`).
- `--run-simulation`: Re-run selected models before copying.
- `--shetran-exe <path>`: Path to SHETRAN executable.

### `cleanup.py`

Removes generated directories and generated overview files.

Default behavior:

- Removes `compute/` and `diff_delta/` for selected models.
- Keeps `output_should/` unless explicitly requested.

CLI options:

- `-m`, `--model <name>`: Select model(s) explicitly (repeatable).
- `-l`, `--list {all,long,medium,short}`: Predefined model set (default: `all`).
- `--remove-expected-results`: Also remove `output_should/`.

## Model Selection Lists

The `short`, `medium`, and `long` sets are defined in `_methods/settings.py`.
Those lists are currently curated manually and can be adjusted depending on runtime expectations.

## Comparison Tolerances and Limits

In `_methods/settings.py`:

- `tolerance_numeric`: tolerance for numeric/HDF5 comparisons.
- `tolerance_table`: tolerance for table comparisons.
- `files_too_large_threshold_*`: skip thresholds to avoid expensive comparisons for very large files.

If a file exceeds size thresholds, it is marked as too large to compare in summary outputs.
