# Unit tests

Two independent test executables are built when `SHETRAN_BUILD_TESTS=ON`:
the visualisation parser tests, and the open-channel row-width test
described under *OC row width* below.

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
command-line suite selector. It uses the shared assertion support described
under *Shared assertion support*. No external test framework is required.

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
ctest --test-dir build -R '^oc_row_width\.unit$' --output-on-failure
```

On Windows with Intel Fortran:

```bat
build.bat --clean -t Debug --test
```

Debug test targets must enable bounds and runtime checking. Release tests
must also be run before completion to catch configuration-dependent behavior.

# OC row width

Status: implemented.

`oc_row_width.unit` covers `MAX_ACTIVE_ROW_WIDTH` in
`src/modules/oc_row_width.f90`, which derives the widest active row of the
open-channel solver from the `NROWST` row-start pointers built by
`OCIND`. That value sizes the `OCSIM` workspace, so an off-by-one there
would either overrun the solver matrices or silently over-allocate them.

The suite asserts:

- degenerate input (no rows, one pointer, decreasing pointers) yields zero
  rather than a negative or spurious size;
- empty rows never raise the maximum;
- the widest row is found in the first, an interior, or the last position,
  the last case depending on the end-of-last-row marker `NROWST(NY+1)`; and
- agreement with the running maximum `OCIND` carried inline before the
  derivation was extracted, over several synthetic row patterns.

The module under test depends on nothing else, so the executable links only
that file and the shared assertion support; no model state or input files
are required.

# Shared assertion support

`test/support` holds the assertion code used by every test executable, split
so that each file covers one concern:

| File | Module | Contents |
|:-----|:-------|:---------|
| `test_support_core.f90` | `test_support_core` | Assertion and failure counters, `assert_true`, and `finish_tests`. |
| `test_support_integer.f90` | `test_support_integer` | `assert_equal_integer`. |
| `test_support_character.f90` | `test_support_character` | `assert_equal_character`. |
| `test_support_real.f90` | `test_support_real` | `assert_close_real`, with its documented relative/absolute tolerance. |
| `shetran_test_support.f90` | `shetran_test_support` | Facade re-exporting all of the above. |

Test programs use the facade, so a new assertion type does not require every
suite to add a `USE` statement. The type-specific modules formulate their
diagnostic and delegate the pass/fail decision to `assert_true`, which keeps
counting and exit status identical across suites.

The counters are module variables, so each test executable has its own
independent set; the sources are listed in `SHETRAN_TEST_SUPPORT_SOURCES` and
compiled into every test target.
