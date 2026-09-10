# Plan: finish the persistent `OCSIM` workspace

Status: planning only. This document does not implement the refactor.

## Objective

Finish the partial persistent-workspace change in `src/modules/OCmod.f90` so
that the open-channel solver has one coherent, model-sized workspace whose
lifetime is the model run rather than the timestep.

The implementation should:

- construct the OC row topology before choosing workspace dimensions;
- size the row solver from the maximum active row width, not `4*NX`;
- keep all `OCSIM` scratch storage, including the `OCFIX` head/flow adapters,
  together in a private derived type;
- allocate once for the single model run, with checked allocation and an
  explicit finalisation procedure; and
- preserve the current active-slice assignment behaviour. Removing or adding
  numerical initialisation is not part of this refactor.

The main benefit of persistence relative to V4.5.3 is removal of allocation,
unmapping, and first-touch page faults from every timestep. It should not be
described as a memory saving relative to V4.5.2: that release used fixed-size
static arrays. Sizing to the active row width is a separate improvement over
the current branch's persistent but conservatively sized implementation.

## Current baseline

The branch already contains a partial implementation from `0feebe7`:

- `AA`, `BB`, `CC`, `DD`, `EE`, `FF`, `GG`, `TM1`, `TM2`, `TV1`, `TV2`,
  `inhrf`, `inqsa`, `GGGETHRF`, and `GGGETQSA` are individual module-level
  allocatables;
- `OCINI` calls `INITIALISE_OCSIM_WORKSPACE`; and
- `OCSIM` no longer allocates or deallocates those arrays.

The remaining problems are:

1. `INITIALISE_OCSIM_WORKSPACE` is called before `OCIND`, so it cannot use the
   row information that determines the required leading dimension.
2. Solver arrays use `4*NX`, although `OCIND` already computes the actual
   maximum row width. `4*NX` is not a safe general bound when bank elements are
   enabled.
3. `GG` is allocated with `NY` columns but `OCSIM` writes and reads column
   `NROWL+1`. A model with `NROWL=NY` therefore accesses it out of bounds.
4. `OCABC` still declares `AA` and `CC` with the fixed extent `NXOCEE`, which
   does not match the smaller actual columns passed by `OCSIM`.
5. The `ALLOCATED(AA)` guard does not verify the other components and there is
   no explicit release procedure.
6. Allocations have no `STAT=`/`ERRMSG=` handling, so failure is not reported
   with useful model dimensions.

The current call order is:

```text
OCREAD / checks
  -> INITIALISE_OCMOD
  -> INITIALISE_OCSIM_WORKSPACE      (4*NX; topology not available yet)
  -> OCXS
  -> OCIND
```

## Target design

### One private workspace value

Replace the parallel module allocatables with one private derived type, for
example `OCSIM_WORKSPACE_TYPE`, and one module instance. Keep this type an
implementation detail of `OCmod`; `OCSIM` remains the public timestep API.

The type should own:

- row coefficient matrices: `aa`, `bb`, and `cc`;
- forward/backward row state: `dd`, `ee`, and `gg`;
- row vectors and matrix scratch: `ff`, `tm1`, `tm2`, `tv1`, and `tv2`;
- the `OCFIX` input adapters: `inhrf` and `inqsa`;
- the `OCFIX` output adapters: `GGGETHRF` and `GGGETQSA`; and
- a readiness flag, so allocation state is checked as one invariant rather
  than by looking only at `AA`.

Including the four `OCFIX` adapters is intentional. They are smaller than the
three-dimensional row solver and were already automatic arrays in V4.5.2, but
they are still model-sized scratch with the same lifetime and should not remain
as unrelated module globals. `XSTAB` is not part of this type: it is a static
hydraulic lookup table owned by `OCmod2`, not `OCSIM` scratch.

Use ordinary module procedures to initialise and release the workspace rather
than relying only on process-exit cleanup or a Fortran type finalizer. The
implementation may assume that one program execution runs one model. The
explicit, idempotent release procedure is retained as lifecycle best practice,
not as support for concurrent or sequential multi-model execution.

### Dimensions and bounds

Add a private module variable such as `MAX_ROW_WIDTH`, initialised to zero.
`OCIND` should assign it from the `NXOC` value already calculated while
building `NROWST`; do not add an `INTENT(OUT)` argument or scan the topology a
second time. Keeping the value as module state makes the established active
bound available to allocation, invariant checks, and later row-sized loops.
It should be used as a loop bound or dimension, not as the loop iterator
itself, so the stored model property cannot be overwritten accidentally.

Allocate the components with the following bounds:

| Component | Proposed bounds | Reason |
|:--|:--|:--|
| `aa`, `bb`, `cc`, `tm1`, `tm2` | `(max_row_width, max_row_width)` | At most one dense block for the widest active row pair. |
| `ff`, `tv1`, `tv2` | `(max_row_width)` | Current-row vectors. |
| `dd` | `(max_row_width, NROWF:NROWL)` | Corrections are addressed by the actual model row number. |
| `ee` | `(max_row_width, max_row_width, NROWF+1:NROWL)` | A transfer block is consumed only between adjacent rows. The one-row case is a valid zero-sized third dimension. |
| `gg` | `(max_row_width, NROWF+1:NROWL+1)` | The forward sweep stores row `IROW`'s result at `IROW+1`, including `NROWL+1`. |
| `inhrf`, `GGGETHRF` | `(total_no_elements)` | Exactly the active elements passed to `OCFIX`. |
| `inqsa`, `GGGETQSA` | `(total_no_elements, 4)` | Exactly four active face values per element. |

Explicit row lower bounds make the solver's indexing contract visible and fix
the `GG(:,NROWL+1)` defect without relying on unused padding. If zero-sized
explicit bounds prove troublesome on either supported compiler, allocate
`EE` over `NROWF:NROWL+1` instead; the extra two planes are preferable to
adding special cases to the timestep loop.

Once allocation follows `OCIND`, `NXOCEE` is no longer the capacity of these
arrays. Remove that assumption from `OCABC` and from the `OCIND` workspace
check. Retain fatal topology/invariant checks, but tie them to the actual
objects: the module-level width must be positive for a runnable OC model,
row-list indices must fit their recipients, and every solver leading extent
must equal `MAX_ROW_WIDTH`.

### Lifecycle procedures

Refactor `INITIALISE_OCSIM_WORKSPACE` to operate on the private module
workspace after `OCIND` has established `MAX_ROW_WIDTH`. Its behaviour should
be:

1. Validate `MAX_ROW_WIDTH`, `NROWF:NROWL`, and `total_no_elements` before any
   allocation.
2. Require the workspace to be empty. A ready or partially allocated object at
   this point is a duplicate-initialisation lifecycle error; do not add
   dimension matching, reuse, or resize logic for multiple models.
3. Allocate with `STAT=` and `ERRMSG=`. On failure, release any components that
   were allocated by the failed attempt, then report a fatal initialisation
   error containing `NX`, `NY`, `NROWF`, `NROWL`, `MAX_ROW_WIDTH`, and
   `total_no_elements`, plus the runtime allocation message.
4. Mark the object ready only after every allocation succeeds and its bounds
   have been checked.

`FINALISE_OCSIM_WORKSPACE` should tolerate an empty or partially allocated
object, deallocate every component, clear the readiness flag, and reset
`MAX_ROW_WIDTH` to zero. Call it once from the normal top-level shutdown path,
after the final possible use of `OCSIM`. Its purpose in this change is orderly
ownership and cleanup. It does not promise that `OCINI` can be called again in
the same process; other state, including `OCmod2`'s one-shot `XSTAB`
allocation, remains outside this lifecycle.

## Initialisation reorder

Keep unrelated hydraulic setup in its current relative order and move only the
workspace allocation. The end of `OCINI` should become conceptually:

```text
INITIALISE_OCMOD
OCXS, when links exist
OCIND(...)                            construct rows and set MAX_ROW_WIDTH
INITIALISE_OCSIM_WORKSPACE()
```

Thus every allocation input has been established before allocation, and no
workspace operation remains in `OCSIM`. Document this ordering dependency next
to both calls so a later cleanup does not move allocation above `OCIND` again.

## Solver interface cleanup

Update `OCSIM` to refer to the single workspace object. A local `ASSOCIATE`
block may retain the short mathematical names without introducing pointers;
otherwise use component qualification directly. Do not make copies of any
workspace component.

At the `OCABC` call, pass only the active coefficient sections:

```text
aa(1:NSV, IND), bb(1:NCR, IND), cc(1:NPR, IND)
```

and change `OCABC`'s dummy arguments to assumed-shape vectors. This removes the
invalid `NXOCEE` dummy extent and makes the routine's definition obligation
match the values that the row solve will consume. Preserve its explicit
active-length initialisation loops unless profiling later demonstrates that a
whole active-slice assignment is faster on both supported compilers.

Add a cheap readiness assertion before the first workspace use. It should
identify an `OCSIM`-before-`OCINI` programming error, not attempt allocation in
the timestep routine.

## Zeroing audit and decision

V4.5.3 zeroed every newly allocated array at the start of every `OCSIM` call.
Commit `e5b53a0` has already removed those blanket assignments on this branch,
so they are **not** present in the source being refactored. The proposed
initialiser must not restore them: allocation leaves contents undefined, and
the solver must continue to define every active value before reading it.

The zeroing that remains, and its necessity, is:

| Storage | Current definition before use | Assessment |
|:--|:--|:--|
| `AA(1:NSV,IND)`, `BB(1:NCR,IND)`, `CC(1:NPR,IND)` | `OCABC` explicitly sets each active coefficient vector to zero, then adds sparse neighbour derivatives with `+`. | **Necessary.** Removing this would retain coefficients from a prior column or timestep. It is active work, not a blanket capacity clear. |
| `FF(IND)` | `OCABC` assigns the scalar on both the head-boundary and normal paths before subtracting flows. | No preliminary zero is needed. |
| `TM1`, `TM2`, `TV1`, `TV2` | The exact active slices are assigned from `BB`/`FF` or matrix/vector products before use. | No clear is needed. |
| `EE` and `GG` | Each transfer/result slice needed by a later row is assigned in the forward sweep before it is read. Zero extents across empty rows carry no values. | No clear is needed. |
| `DD` | The last active row is assigned from `GG`; every preceding active correction slice is then assigned during the downward sweep before element updates read it. | No clear is needed. |
| `inhrf` and `inqsa` | `OCSIM` fills every active element and all four faces from `GETHRF`/`GETQSA` immediately before `OCFIX`. | No clear is needed. |
| `GGGETHRF` and `GGGETQSA` | `OCFIX` begins with whole-active-array copies from `inhrf` and `inqsa`, then applies corrections. | No clear is needed while that contract remains in place. |
| Unused allocated capacity | Never belongs to an active slice. | Must remain untouched. |

There is unrelated initialisation elsewhere in `OCmod`, such as zeroing the
active `NOCBCC` boundary map in `JEOCBC`; it is model-state initialisation and
is not part of this workspace change.

Because zero removal preceded this refactor in branch history, the original
recommended two-commit proof (persistence with clears, then clear removal)
cannot be reproduced against current `HEAD` without reverting working code.
Instead, preserve current assignment coverage; the user can validate it later
with manual poisoning and runtime-check runs. If this work is backported
directly to V4.5.3, use the original order: first make allocation persistent
while retaining all clears, then remove the blanket clears in a separately
tested commit.

## Implementation sequence

1. **Preserve the manual-test baseline.** Do not regenerate expected results or
   run models automatically. The user will perform numerical and timing tests
   after the implementation is handed over.
2. **Introduce the workspace type and lifecycle.** Move all fifteen arrays
   into the type, add complete-state checks, checked allocation, and idempotent
   finalisation. Do not change numerical assignments.
3. **Retain the active row width as module state.** Add `MAX_ROW_WIDTH`, set it
   directly in `OCIND` from the existing `NXOC` calculation, and replace the
   obsolete `NXOCEE` workspace-capacity assumption with checks on the actual
   row data.
4. **Reorder `OCINI` and allocate exact shapes.** Run `OCIND` first, then
   initialise the workspace once using the bounds in this plan. This step also
   fixes the `GG` last-row slot.
5. **Rewire `OCSIM` and `OCABC`.** Use the workspace components, active
   coefficient slices, and assumed-shape `OCABC` dummies. Confirm that no
   allocation, deallocation, or whole-workspace clear occurs in `OCSIM`.
6. **Wire orderly shutdown.** Invoke `FINALISE_OCSIM_WORKSPACE` once after the
   final solver use. Keep it idempotent and able to clean up a partial object,
   but do not add multi-model reinitialisation logic.
7. **Perform clean recompilation checks only.** Clean-build the complete source
   in Debug and Release configurations. Do not automatically run CTest,
   example models, regression comparisons, poisoning runs, or timing tests.

## Verification and acceptance criteria

The implementation agent should run clean recompiles with both supported Linux
compilers where available. These are compilation checks, not automatic test
suite or model executions:

```sh
./build.sh --clean -t Debug -c gfortran
./build.sh --clean -t Debug -c ifx
./build.sh --clean -t Release -c gfortran
./build.sh --clean -t Release -c ifx
```

Do not pass `--test` and do not invoke
`examples/check_results_consistency.py` automatically. The user will manually
run model, bounds, numerical-regression, poisoning, and performance checks
afterwards. The recommended manual coverage remains a small model, a
bank-enabled model, a model with `NROWL=NY`, and `foston100m` or another
medium/large network.

The refactor is complete when:

- `OCIND` precedes workspace allocation and sets the module-level active width;
- all workspace arrays are owned by one coherent object and are allocated at
  most once during the program's single model run;
- the explicit finalisation procedure releases the complete or partial object
  and resets `MAX_ROW_WIDTH`;
- allocation failure produces a fatal message with the model dimensions;
- all solver leading dimensions equal `MAX_ROW_WIDTH`;
- `GG` explicitly includes the `NROWL+1` slot required by the solver;
- `OCABC` no longer declares an extent larger than its actual arguments;
- `OCSIM` contains no allocation, deallocation, or blanket workspace clear;
- current active-slice zeroing in `OCABC` is retained;
- all requested clean Debug and Release recompiles succeed; and
- numerical equivalence, bounds-sensitive model execution, and timing remain
  explicit manual follow-up checks rather than automatically run acceptance
  steps.

## Out of scope

- Removing the `inhrf`/`inqsa` adapters by changing the automatic-
  differentiation boundary around `OCFIX`.
- Further reducing the required `EE` storage with a ragged or streaming row
  representation.
- Changing the row-solver arithmetic, matrix routines, or convergence logic.
- General multi-model lifecycle fixes for allocatables owned by other modules.
- Any new blanket zeroing or further zero-removal optimisation.
