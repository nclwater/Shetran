# Shetran
Shetran Hydrological Model

Details can be seen here:
https://research.ncl.ac.uk/shetran/

## Installation
Download the zip file from the latest release and then extract it.

## Sample Usage
Double click on exectuable, e.g. 'shetran.exe' in the extracted folder.
Then select a rundata file from one of the examples data sets.

## TODO

Shetran is an old and complex code base.
It has also been written by many different people while the Fortran environment changed.
Therefore a lot could be done to modernize the code.
A non exhaustive list:

- ~~Setup a CMake build system.~~
- ~~Generate a common code base between Linux and Windows.~~
- Update in-code documentation to use FORD.
- The comments and functions can be updated to use the same style.
- Use the Fortran Standard Library where applicable, e.g. for CLI arguments and error messages.
- There are a lot of GOTOs left in the code, often to setup loops and control flow execution.
  Update to appropiate other structures / directives.
- Use a code formatter to enforce a consistent coding style.#
- Update variable declaration to use the KIND= parameter instead of e.g. DOUBLEPRECISION.
- Use IOSTAT instead of END= or ERR= for status & error handling of IO.
- The variable NTYPE seems to be a flag for settings chosen. 
  Make it readable, i.e. hide values behind parameter variables.
- A lot of choices between different methods is using plain integers. 
  Use fixed parameters instead to improve the legibility.
- Add an execution flag to output the array limits set at compile time.
- Make it easier to compile to different array limit sizes.

## Notes

- Currently the CMake build script is building _every_ file present in the source directory.
  Files which should not be build, should have a different file ending like .sav or similar.
