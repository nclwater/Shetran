# TODO

Shetran is an old and complex code base.
It has also been written by many different people while the Fortran environment changed.
Therefore a lot could be done to modernize the code and improve the featureset.

## Functionality

- Make the current compile time array sizes allocatable.
  Get the necessary sizes from the actual input data and use encapsulation to make it safe.
- Replace the config file formats with a more legible and self explanetary one.
- Add test functions, both integration (use the sample models) and per-function / module ones.
- Actually add both the logic and the usage documentation to this repo.
- Allow it to read from a proper timeseries file, i.e. one where the timestamp is included.

## Code Quality

- ~~Setup a CMake build system.~~
- ~~Generate a common code base between Linux and Windows.~~
- Update in-code documentation to use FORD.
- The comments and functions can be updated to use the same style.
- Use the Fortran Standard Library where applicable, e.g. for CLI arguments and error messages.
- There are a lot of GOTOs left in the code, often to setup loops and control flow execution.
  Update to appropiate other structures / directives.
- Use a code formatter to enforce a consistent coding style.
- Update variable declaration to use the KIND= parameter instead of e.g. DOUBLEPRECISION.
- Use IOSTAT instead of END= or ERR= for status & error handling of IO.
- The variable NTYPE seems to be a flag for settings chosen. 
  Make it readable, i.e. hide values behind parameter variables.
- A lot of choices between different methods is using plain integers. 
  Use fixed parameters instead to improve the legibility.
- Add an execution flag to output the array limits set at compile time.
- Make it easier to compile to different array limit sizes.
- Figure out if the performance of any loops can be improved by using OpenMP.
  For this, start using PURE and ELEMENTAL for the subroutines / functions as appropiate.
- When completely exiting nested loops, use "EXIT <loopname>" if possible (double check changes done).
- For OPEN statements, use NEWUNIT to set handles. 
  Correspondingly, replace the explicit unit-numbers with more descriptive variables.
