# Phase 2 — `ALCHK`/`ALCHKI`'s `NOTOK` becomes `OPTIONAL`

**Depends on:** nothing. **Blocks:** phase 7 touches the same files; do this
first or expect conflicts.
**Target:** `src/io/input_validation.f90`, plus 234 call sites.

## Why

`ALCHK` and `ALCHKI` both end with

```fortran
LOGICAL, INTENT(OUT) :: NOTOK(N0:N1) !! Per-value failure mask.
```

**No caller anywhere in the tree reads it.** All 234 sites pass a discarded
scratch array — 166 `LDUM`, 49 `LDUM1`, 19 `LDUM2`. Verify before starting:

```bash
grep -rn --include="*.f90" -E "\b(LDUM|LDUM1|LDUM2)\b" src \
  | grep -vE "ALCHK|::|INTENT|!!|^\s*!"
```

The only hits should be `src/contaminant/cm_channel.f90`, where `LDUM` is an
unrelated scalar. If anything else appears, stop — a reader exists and this
phase's premise is wrong.

The scratch is not free: `oc_driver:OCINI` declares `LOGICAL :: LDUM1(NELEE)`,
which is 1,000,000 bytes of `.bss`, and it is threaded through `OCCHK1` and
`OCCHK2` as an argument along with the `SZLOG` extent that exists only to size
it.

## Work

### 1. Give `input_validation` its own mask workspace

`NOTOK` is written unconditionally in several branches of both routines.
Scattering `PRESENT()` guards over every assignment is invasive and easy to get
wrong. Do not do that, and do not make the fallback an automatic array sized
`(N0:N1)` — `N1` reaches `total_no_elements` at many sites and this codebase
already has a stack-pressure problem.

Instead give the module a grow-on-demand fallback, using the phase 3 helper:

```fortran
LOGICAL, ALLOCATABLE, PRIVATE :: check_failure_mask(:)
   !! Fallback failure mask used when a caller omits NOTOK. Grown on demand to
   !! the width of the widest check performed so far.
```

At the top of each routine:

```fortran
IF (PRESENT(NOTOK)) THEN
   ! write into NOTOK as today
ELSE
   CALL ensure_capacity(check_failure_mask, N1 - N0 + 1)
   ! write into check_failure_mask(1 : N1-N0+1)
END IF
```

Restructuring the body twice over is worse. Prefer one of:

- extract the existing body into a contained procedure taking the mask as a
  non-optional `mask(N0:N1)` dummy, and call it with either `NOTOK` or a
  section of the fallback; or
- keep one code path writing to a local pointer-free alias established once.

The contained-procedure form is the clearer of the two and keeps the diff
reviewable. Whichever you choose, the arithmetic must not change.

### 2. Make the dummy optional

```fortran
LOGICAL, INTENT(OUT), OPTIONAL :: NOTOK(N0:N1)
```

### 3. Delete the argument at all 234 call sites

Count them by joining continuation lines first — a plain `grep -c` on
`CALL ALCHK` undercounts multi-line calls and overcounts fragments. The
measured figures at `e09f465` are 137 `ALCHK` and 97 `ALCHKI`, and the last
argument resolves to `LDUM` (166), `LDUM1` (49) or `LDUM2` (19) — 234 of 234.
Verify your own count before and after.

### 4. Delete the now-dead scratch arrays and their plumbing

Once no call passes them, these become unreferenced and must go:

| Location | Declaration | Note |
|:---------|:------------|:-----|
| `src/overland_channel/oc_driver.f90` | `LOGICAL :: LDUM1(NELEE)` | 1,000,000 B; also drop it from the `OCCHK1`/`OCCHK2` argument lists |
| `src/overland_channel/oc_validation.f90` | `LDUM1(SZLOG)` dummies in `OCCHK1`, `OCCHK2` | and then `SZLOG` itself, which exists only to size them |
| `src/sediment/sy_workspace.f90` | `LOGICAL, ALLOCATABLE :: LDUM(:)` | plus its allocation in `INITIALISE_SYMAIN_WORKSPACE` and its threading through `SYERR1`–`SYERR3` |
| `src/nitrate/mn_state.f90` | `LDUM` in `MN_WORKSPACE_TYPE` | plus `LDUM`/`LDUM2` dummies throughout `mn_validation` |
| `src/evapotranspiration/et_process.f90` | `LOGICAL :: LDUM1(NV)` in `ETCHK2` | |
| various | `LOGICAL :: LDUM(1)`, `LDUM1(1)` one-element placeholders | `cm_input`, `mn_input`, `mn_validation`, `sy_validation` |

Removing `SZLOG` and the `LDUM` arguments changes the signatures of `OCCHK1`,
`OCCHK2`, `SYERR1`, `SYERR2`, `SYERR3`, `ETCHK2` and several `mn_validation`
routines. That is the intended outcome — these arguments carry nothing.

Update each affected routine's `!!` documentation and its `@history` block in
the style already used in those files.

## Pitfalls

- `NOTOK` has the non-default lower bound `N0`. If you introduce a fallback
  indexed from 1, the offset must be handled explicitly and consistently in
  both routines. Getting this wrong will not change any output — the mask is
  discarded — but it will produce an out-of-bounds write that `-fcheck=bounds`
  catches. Run the bounds check.
- `ALCHK` and `ALCHKI` are near-identical twins. Apply the same change to both
  and diff them against each other afterwards to confirm they stayed in step.
- `COUNT` (the cumulative failure counter) *is* read by callers. Do not touch it.
- Some `mn_validation` routines take `LDUM` as `INTENT(INOUT)` with extent
  `NELEE` and `LDUM2` as `LLEE`. Confirm each is genuinely only a pass-through
  to `ALCHK`/`ALCHKI` before deleting it; if one is used locally, keep it as a
  local instead of an argument.

## Expected result

- 234 arguments deleted; `SZLOG` and six-plus `LDUM*` arrays removed.
- ~1,000,064 bytes of `.bss` returned.
- No numerical difference on any of the 13 baselines.
