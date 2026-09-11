# Phase 3 — Workspace-module convention and the `ensure_capacity` helper

**Depends on:** nothing. **Blocks:** phases 4, 5, 6, 7.
**Target:** one new module; no behavioural change; nothing else moves in this
phase.

This phase delivers only the mechanism. Converting actual workspaces happens in
phases 4–7, one component chain at a time.

## The convention

Each component that owns scratch gets a workspace module, or extends the one it
already has. Within it, **one allocatable array per distinct need**, named for
what it holds. There is no shared general-purpose buffer; that was the original
defect.

Existing precedent to follow, not to replace:
`src/sediment/sy_workspace.f90` already holds `IDUM1A`, `IDUM1X`, `DUMSED` and
others as allocate-once module arrays, set up by `INITIALISE_SYMAIN_WORKSPACE`.
`src/nitrate/mn_state.f90` wraps its equivalents in `MN_WORKSPACE_TYPE`.

Modules to create or extend (details in the per-chain phases):

| Module | Status | Holds |
|:-------|:-------|:------|
| `sy_workspace` | exists, extend | sediment element codes and values |
| `cm_workspace` | **new** | contaminant grid codes and record values |
| `vs_workspace` | **new** | subsurface grid codes and record values |
| `oc_workspace` | **new** | overland/channel element, link and bank scratch |
| `input_validation` (private) | exists, extend | the phase 2 failure mask |

Naming rule, restated because it is the most likely thing to be got wrong: no
new array may be called `IDUM`, `DUMMY`, `LDUM`, or a numbered variant.

## The helper

Create `src/core/workspace_support.f90`, a small module providing a generic
`ensure_capacity` that grows an allocatable to at least `n` elements.

```fortran
MODULE workspace_support

   USE MOD_PARAMETERS, ONLY: I_P, LENGTH_LINE
   USE error_status, ONLY: errstat_alloc

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: ensure_capacity

   INTERFACE ensure_capacity
      MODULE PROCEDURE ensure_capacity_i
      MODULE PROCEDURE ensure_capacity_d
      MODULE PROCEDURE ensure_capacity_l
   END INTERFACE

CONTAINS

   SUBROUTINE ensure_capacity_i(buffer, n, name)
      INTEGER(KIND=I_P), ALLOCATABLE, INTENT(INOUT) :: buffer(:)
      INTEGER(KIND=I_P), INTENT(IN) :: n     !! Minimum required extent.
      CHARACTER(LEN=*),  INTENT(IN) :: name  !! Array name, for diagnostics.
      ...
   END SUBROUTINE ensure_capacity_i

END MODULE workspace_support
```

Required semantics, identical across the three specifics:

1. If already allocated with `SIZE(buffer) >= n`, return immediately. The common
   case must not reallocate.
2. Otherwise deallocate if allocated, then allocate to `n`, reporting failure
   through `errstat_alloc` with `name` and a `location` string, exactly as
   `INITIALISE_SYMAIN_WORKSPACE` does today.
3. **Contents are not preserved across a grow.** Every one of these arrays is
   write-before-read scratch. Document this in the `!!` comment; do not add a
   copy, and do not let a caller come to depend on retention.
4. `n < 1` is a programming error, not an input error. Report it through the
   existing fatal path rather than allocating a zero-size array.
5. The routine must be safe to call on every entry to a reader, not only once —
   that is the whole point of grow-on-demand.

Give the module the file header, `@history` block and `!!` documentation style
used by `src/core/array_limits.f90` and its neighbours, and add it to the
build in the same place the other `src/core` modules are listed.

## What `ensure_capacity` does not replace

The `NREQ` checks in `CMRD` and `SYREAD` stay. They read as capacity checks but
they are input validation — they reject an input file declaring an absurd count
before anything is allocated. Search for them:

```bash
grep -rn --include="*.f90" -E "NREQ" src/contaminant/cm_input.f90 src/sediment/sy_input.f90
```

`CMRD` has three (`2*NCLBND`, `(1+NCONCM)*NFEX`, `(1+NCONCM)*NCBC`); `SYREAD`
has three (`8`, `MAX(MAX(5,NSED)*NS, 3*NV)`, `MAX(3*NSYB, NSED*NSYC(1),
NSED*2*NSYC(3))`). Keep each check and its diagnostic verbatim, then add the
`ensure_capacity` call immediately after it. The `NREQ` expressions are the
authoritative statement of what each workspace must hold — they are the reason
this plan can size anything at all, and they should be cited in the `!!`
comment of the array each one governs.

## Acceptance for this phase

The helper compiles, is covered by the build, and is used by nothing yet. Output
is unchanged by construction; still run the full 13-model comparison, because a
new module in the build order has been known to perturb link order and it is
cheaper to learn that here than in phase 4.
