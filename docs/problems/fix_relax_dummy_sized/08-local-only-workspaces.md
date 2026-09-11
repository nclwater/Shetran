# Phase 8 — Workspaces that should just be locals

**Depends on:** nothing. **Independent of** everything else. Smallest phase;
good first task for an agent new to the codebase.

Two of the fifteen remaining workspaces are small enough that a module is
overkill. They are per-call scratch with a compile-time bound, used and
consumed inside one procedure. They should be ordinary local arrays with no
`SAVE`.

## `OCCHK1` — `src/overland_channel/oc_validation.f90`

```fortran
INTEGER, DIMENSION(NXEE), SAVE :: IDUM   ! 4,000 B
```

Used only as: fill `IDUM(X)` for `X = 1..NX` inside the inner loop, then
`ALCHKI(..., IDUM(1:NX), ...)` — one grid row at a time, rebuilt every row.
Nothing survives between rows, so `SAVE` is meaningless.

Replace with:

```fortran
INTEGER :: link_code_row(NXEE) !! Link-type code for one grid row, rebuilt per row.
```

`NXEE` is 1,000, so 4 KB on the stack per call. `OCCHK1` is called once during
setup; this does not meaningfully add stack pressure. Confirm with
`scripts/list_stack_size_hits.py` if in doubt.

## `COLMSM` — `src/contaminant/cm_column.f90`

```fortran
DOUBLEPRECISION, DIMENSION(LLEE), SAVE :: DUMMY   ! 400 B
```

Used as: zeroed over `NCEBOT..NCETOP`, accumulated in the bank-source loop, then
consumed in the uptake loop. All within one call. `LLEE` is 50.

Replace with:

```fortran
DOUBLE PRECISION :: bank_source_uptake(LLEE)
   !! Per-cell bank-source contribution accumulated for this column, consumed
   !! by the plant-uptake loop below.
```

Read the two loops before settling on the name — `EDCAP(NCE) = EDCAP(NCE) -
DUMMY(NCE) + ...` suggests it is a source term being subtracted from the
uptake capacity, so name it for that.

## Pitfalls

- **Confirm there is no cross-call dependence before dropping `SAVE`.** For
  both arrays the analysis says there is none, but `SAVE` on an array that is
  in fact read before being written in a later call is a behaviour change that
  a single regression run may not expose. Check that the first reference in
  every path through the procedure is a write, not a read.
- `COLMSM` is on a hot path (called per column per timestep). A 400-byte
  automatic array is fine; do not be tempted to make it allocatable, which
  would add an allocation per call.
- `cm_column.f90` had its `nelee` import removed in `e09f465`. Do not
  reintroduce it.

## Expected result

- 4,400 B returned, two `SAVE` attributes removed, two meaningless names gone.
- No numerical difference on any of the 13 baselines.
