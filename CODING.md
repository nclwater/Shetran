# Codying Style & Conventions Guide for SHETRAN

This document contains some rules and recommendations for coding in the SHETRAN model.
Implementing these in the code base is still work in progress, but for any new or changed piece of code it is strongly suggested to follow this document.

The style & conventions are split into three parts:

1. REQUIRED: you are expected to follow this.
2. RECOMMENDED: it is strongly recommended to do this.
3. SUGGESTED: it would be good to do this.

While this is (a little bit of) extra effort, it makes the code easier for others to read and cuts down on bugs.

## AI Usage

Using AI is accepted, but you still take full responsibility for correctness, functionality and bugs for any code commited via your account.

## Required

### Development Setup

Use git to record your changes and ask for inclusion into the code base ("pull request").

Only commit unix-style endings in git.
See [the official documentation](https://docs.github.com/en/get-started/git-basics/configuring-git-to-handle-line-endings).

Check for consistency in both outputs and runtime of the changed code.
The example models and scripts to run them in the "examples/" subdirectory make that easy as they create overview CSV files with any differences and the individual runtimes.
See [the readme in the examples directory](examples/README.md) for how to do this.

If feasible, test compiling SHETRAN with different compilers and across platforms.
The build.sh/build.bat scripts make that easier, same with the WSL2 for MS Windows.

### Code

When code leads to indefined content in variables, e.g. an error during a READ statement, it should terminate the program with an useful error message.
The [mod_error.f90](src/util/mod_error.f90) file has standartised checks & message blocks for dealing with the most common cases, i.e. the OPEN, CLOSE, READ, WRITE, ALLOCATE and DEALLOCATE statements.

Do not use implicit variable declaration, i.e. use `IMPLICIT NONE` everywhere.

## Recommended

### Development Setup

Make small, functionality based commits.
This makes it much easier for others to read and understand your changes.

After developing new functionality or fixing a bug, add an example to the example models and / or to the cmake-based unit tests.
This enables regression testing.

### Code

Always add `, ONLY: ` to any USE statements.
The only exception is mod_parameters, which only contains parameters, ranging from KIND indicators to SI constants.

Any non-PARAMETER variables used in a subroutine / function, should be passed as dummy arrays.
Mark them with the `INTENT` keyword so that the compiler can optimise their usage.

Do not hard-code array sizes.
Make them runtime allocatable.

## Suggested
