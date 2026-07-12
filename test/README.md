# Visualisation parser tests

Status: implemented.

The implementation plan is in
[`docs/problems/modernise_vis_read.md`](../docs/problems/modernise_vis_read.md).

## Scope

These tests cover the parser behind `R_C`, `R_I`, and `R_R`, plus the
`COPY`/`strip` preprocessing path in
`src/visualisation/visualisation_read.f90`.

The principal regression is a non-advancing character read that can fail to
move to the next Fortran record. The replacement parser reads complete
records and maintains its token position in memory. Tests assert both
correct values and forward progress at record boundaries and EOF.

The repository call graph uses this parser only for visualisation plans.
Other model files are not production inputs to `R_I` or `R_R`. Numeric forms
found in those files may be included as explicit grammar-extension cases,
but they will be labelled as synthetic parser inputs.

## Suites

`visualisation_read.unit` uses small fixtures under
`test/visualisation_read/data` to test:

- token progression within and across records;
- blank records, EOF, and a final record without a newline;
- keywords, fixed-length character results, and character-by-character
  masks;
- accepted and rejected integer and real tokens;
- conversion overflow and useful diagnostics;
- comment removal, separator splitting, title validation, and invalid input
  in `strip`;
- long records and bounds-sensitive failure paths.

`visualisation_read.examples` discovers visualisation plans under
`examples/<name>/model`, excluding example names beginning with `_`. It:

- copy each plan into a build-tree fixture directory;
- run the production `COPY`/`strip` path on the copy;
- scan the complete token stream;
- assert parser-position progress after every token;
- require clean EOF and a final `stop` token;
- exercise selected typed time, list, layer, mask-bound, and mask-row
  sequences.

Tests must not modify files under `examples/` or create parser temporaries in
the source tree.

## Test structure

The target is one executable, `visualisation_read_tests`, with a
command-line suite selector. Local support code will provide assertion
counts, case-specific diagnostics, non-zero failure status, and documented
tolerance-based default-`REAL` comparisons. No external test framework is
required.

Test module files, generated fixtures, temporary files, and output belong in
the selected CMake build directory.

## Running

Run the complete test flow on Linux with:

```sh
./build.sh --clean -t Debug -c gfortran --test
./build.sh --clean -t Debug -c ifx --test
```

Run a configured build directly with:

```sh
ctest --test-dir build --output-on-failure
ctest --test-dir build -R '^visualisation_read\.unit$' --output-on-failure
ctest --test-dir build -R '^visualisation_read\.examples$' --output-on-failure
```

On Windows with Intel Fortran:

```bat
build.bat --clean -t Debug --test
```

Debug test targets must enable bounds and runtime checking. Release tests
must also be run before completion to catch configuration-dependent behavior.
