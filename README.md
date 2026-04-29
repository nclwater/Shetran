# SHETRAN Hydrological Model

An up to date manual and more can be found on the [homepage](https://research.ncl.ac.uk/shetran/).

## Installation

Download the zip file from the latest release and then extract it.

## Sample Usage

SHETRAN is executed from the command line.
Unzip the release, navigate to the corresponding location in a terminal and execute

```bash
shetran.exe -t location_of_rundata_file.txt
```

## Compiling & Development

See [COMPILING.md](COMPILING.md) for instructions on how to compile the program.

For iterative local development builds on Windows, use `build.bat -t Release --clean-app` to rebuild SHETRAN quickly without rebuilding external libraries.
The equivalent Linux command is nearly identical: `./build.sh -t Release --clean-app`.

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
