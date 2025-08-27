# Shetran
Shetran Hydrological Model

Details can be found here:
https://research.ncl.ac.uk/shetran/

## Installation
Download the zip file from the latest release and then extract it.

## Sample Usage
Double click on exectuable, e.g. 'shetran.exe' in the extracted folder.
Then select a rundata file from one of the examples data sets.

## Compiling

A CMake script has been setup to automatically setup the necessary compiler flags and build Shetran as a project.
It is documented in the CMAKE_BUILD.md file.

**Key Features:**

- **Automatic compiler detection**: Prefers gfortran on Linux, Intel Fortran on Windows
- **Smart HDF5 handling**: Automatically finds system HDF5 or downloads and builds from source as needed
- **Cross-platform support**: Works on Linux, Unix, and Windows with multiple compilers

This has been tested in the following configurations and confirmed working:

- Linux, with GFortran, both with the build script and the cmake build.
- Windows, with ifx, the build script only.

Automatic integration of the cmake build in the Visual Studio and Visual Studio Code on Windows is currently very finicky.
An (slightly out-of-date) description of how to setup the corresponding Visual Studio project can be found in the file "COMPILING.md".

## Notes

- Currently the fallback function in the CMake build script is building _every_ file present in the source directory.
  Files which should not be build, should have a different file ending like .sav or similar.
