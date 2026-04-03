# Shetran Hydrological Model

An up to date manual and more can be found on the [homepage](https://research.ncl.ac.uk/shetran/).

## Installation

Download the zip file from the latest release and then extract it.

## Sample Usage

Double click on exectuable, e.g. 'shetran.exe' in the shetran folder that has been extracted.
Then select a rundata file from one of the examples data sets.

## Compiling & Development

See [COMPILING.md](COMPILING.md) for instructions on how to compile the program.

For iterative local development builds on Windows, use `build.bat -t Release --clean-app` to rebuild SHETRAN quickly without rebuilding external libraries.

## Documentation

This project is setup to support [FORD](https://github.com/Fortran-FOSS-Programmers/ford) for in-code documentation.
The current state of the documentation can be generated from the project root using either:

```cmd
build.bat --docs-only
```

or directly via FORD:

```powershell
ford -o docs\ford .\ford_project.md
```

The generated documentation entry point is [FORD index](docs/ford/index.html).
