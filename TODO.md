# TODO

This document outlines planned enhancements and modernization tasks for the SHETRAN codebase, building upon the ongoing refactoring effort.

---

## Core Architecture & Performance

- **Dynamic Memory Allocation**: Convert fixed-size, compile-time arrays to allocatable arrays. Determine sizes from input data to improve memory efficiency and remove hardcoded limits.

- **Performance Profiling & Optimization**:
  - **Refactor with Derived Types**: Transition from global data variables and large `COMMON` blocks to derived types (structs). This improves data encapsulation, allows for clearer function interfaces, and helps the compiler with optimization.
  - Identify performance-critical loops and evaluate potential for parallelization with OpenMP.
  - Systematically apply `PURE` and `ELEMENTAL` attributes to subroutines and functions to aid compiler optimization.

- **I/O Modernization**:
  - Replace hardcoded file unit numbers with `NEWUNIT` for safer, more robust file handling.
  - Use `IOSTAT` for all file operations to standardize error handling, replacing legacy `END=` and `ERR=` constructs.
  - Enhance HDF5 output by integrating compression libraries like `zlib` to reduce output file sizes.

---

## Code Quality & Modernization

- **Style & Readability**:
  - **Decompose Monolithic Modules**: Break up large, monolithic modules and subroutines into smaller, more focused procedures to improve readability and maintainability.
  - Use a code formatter (e.g., `fprettify`) to enforce a consistent coding style across the entire codebase.
  - Replace numbered loop labels with descriptive named constructs (e.g., `outer_loop: DO ... END DO outer_loop`).
  - When exiting nested loops, use named `EXIT` statements for clarity.
  - Standardize comment style and function headers.

- **Modern Fortran Practices**:
  - Replace `DOUBLEPRECISION` and other legacy type declarations with the modern `REAL(KIND=...)` syntax.
  - Refactor "magic numbers" and integer flags (like `NTYPE`) into named `PARAMETER` constants for improved readability.
  - Resolve "masked variable" warnings by explicitly defining variable scope with `PRIVATE` and `PUBLIC` attributes in modules.
  - Leverage the Fortran Standard Library (`stdlib`) for common tasks like command-line argument parsing and string manipulation.

---

## Testing & Quality Assurance

- **Comprehensive Test Suite**: Implement a full testing framework as outlined in the testing plan (`docs/reports/testing/plan_testing.md`).
  - **Integration Tests**: Automate the execution and validation of existing example cases.
  - **Unit Tests**: Develop module- and function-level tests for core computational and utility routines.

---

## User Experience & Documentation

- **Configuration & Usability**:
  - Replace the current input file formats with a more modern, self-documenting format (e.g., TOML, JSON, or XML).
  - Allow input timeseries files to include timestamps for more intuitive data preparation.
  - Add a command-line flag to output the compile-time array limits for easier debugging and user support.

- **Documentation**:
  - **API Documentation**: Complete the in-code documentation using FORD syntax for all modules and public interfaces.
  - **Usage Documentation**: Add comprehensive user guides, tutorials, and scientific logic documentation to the repository.

---

## Completed Tasks

- ✅ **Setup a CMake build system.** (Phase 1)
- ✅ **Generate a common code base between Linux and Windows.** (Phase 1 & 3)
- ✅ **Eliminate GOTO statements.** (Phase 4)
