# SHETRAN

This is the repository for the **SHETRAN Hydrological Model**.

Details of features and the history of the program itself can be found on the [webpage](https://research.ncl.ac.uk/shetran/).

## Installation

Download the zip file from the latest release and then extract it.

## Sample Usage

Double click on exectuable, e.g. 'SHETRAN.exe' in the shetran folder that has been extracted.
Then select a rundata file from one of the examples data sets.

## Documentation

Some documentation, especially the official user manual, can be found in the ``docs`` folder.
Additionally, in-code-documentation can be generated using [FORD](https://forddocs.readthedocs.io/en/stable/index.html).

Generating the documentation is done as follows:

1. Install FORD into a Python environment, by running ```pip install ford```.
2. Generate the documentation itself by running ```ford -o docs/ford ford_project.md``` from the project root. 

The generated documentation can then be found in the ``docs/ford`` folder.
