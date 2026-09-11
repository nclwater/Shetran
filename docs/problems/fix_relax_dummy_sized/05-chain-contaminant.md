# Phase 5 — Contaminant chain

**Depends on:** phase 3. **Independent of** phases 4, 6, 7.
**Chain:** `INCM` → `CMRD` → `ALREDI`, `ALREDF`, `ALALLI`.

## Current state

`src/contaminant/cm_input.f90` declares in `INCM`:

```fortran
INTEGER, DIMENSION(NXEE*NYEE), SAVE :: IDUM      ! 4,000,000 B
DOUBLEPRECISION, DIMENSION(NELEE), SAVE :: DUMMY ! 2,000,000 B
```

used for one thing — the single `CALL CMRD(...)`. Verify with
`grep -n "IDUM\|DUMMY" src/contaminant/cm_input.f90`; the only other `DUMMY`
hits are the unrelated `DUMMYCONC` allocatable.

Note that **`CMRD` does not call `ALREAD`**. It calls `ALREDI`, `ALREDF` and
`ALALLI`. Phase 1 does not touch this chain; the `ALRED*` family has the same
`IDATA(N1, N2)` pattern but only one data argument each, so it has no
placeholder problem and is not being made optional.

## True requirement

**Integer workspace.** `CMRD` passes it to `ALREDI` at `(1,1)` and
`(2, NCLBND)`, and to `ALALLI`, whose dummy is `IDUM(*)` and which does
`CALL ALREDI(..., NX, NY, IDUM)` followed by `IDUM(XY0 + X)` over the grid.
So the requirement is **`NX*NY`** — this is genuinely a grid buffer and cannot
shrink to an element count.

**Real workspace.** `CMRD` states its own requirement in three `NREQ` checks:

| Record | `NREQ` |
|:-------|:-------|
| `CM9` | `2*NCLBND` |
| `CM29` | `(1 + NCONCM)*NFEX` |
| `CM35` | `(1 + NCONCM)*NCBC` |

plus the un-checked `ALREDF` shapes: `CM26e` at `NDATA = 2*NTBL`, `CM41` at
`4*NSCM`, `CM51` at `(1+NSEDCM)*NCONCM`, `CM53`/`CM55`/`CM61` at
`(1+NSCM)*NCONCM`.

The three `NREQ` checks compare against `NELEE` and fail fatally when exceeded.
**Keep them verbatim** — they reject malformed input before anything is sized.
The un-checked shapes are bounded by `MAX_NUM_DATA_PAIRS`, `NSEE`, `NSEDEE` and
`NCONEE` and are small; confirm this rather than assuming it, and if any can
exceed the checked maximum, add the missing check rather than silently growing
the buffer.

## Target

Create `src/contaminant/cm_workspace.f90`:

```fortran
INTEGER, ALLOCATABLE :: cm_grid_codes(:)
   !! Flattened grid of contaminant category codes, filled by ALALLI via
   !! ALREDI. Extent is NX*NY.
DOUBLE PRECISION, ALLOCATABLE :: cm_record_values(:)
   !! Record value buffer for CMRD's ALREDF reads. Extent is the largest NREQ
   !! computed by CMRD's own input checks (CM9, CM29, CM35).
```

## Work

1. Create the module with the project's standard header and `@history` block.
2. Allocate both via `ensure_capacity`, at the top of `CMRD`, after the record
   counts that determine `NREQ` have been read. `NREQ` is not known until
   `CM9`/`CM19`/`CM29`/`CM35` have been read, so the real buffer must be grown
   at each of the three existing check sites rather than once up front — this
   is exactly what grow-on-demand is for. Place each `ensure_capacity` call
   immediately after the corresponding `NREQ` check, not before it.
3. `cm_grid_codes` can be sized once, as soon as `NX`/`NY` are available.
4. Replace `CMRD`'s `IDUM`/`DUMMY` dummy arguments with `USE cm_workspace`.
5. `ALALLI` (`src/io/spatial_fields.f90`) takes `IDUM(*)`. It is shared with
   other callers, so do not make it module-coupled. Either keep its argument
   and pass `cm_grid_codes`, or — preferred, and consistent with the decision
   to cover the shared helpers — make the argument `OPTIONAL` and have `ALALLI`
   fall back to its own module-owned grid buffer when omitted. Check its other
   callers first: `grep -rn "CALL ALALLI" src`.
6. Drop the two arguments from the `CMRD` call in `INCM` and delete the two
   declarations.

## Also clean up while here

`CMRD` contains several one-element placeholder buffers that exist only because
`ALREDI`/`ALREDF` take rank-2 explicit-shape dummies and the caller wants a
scalar:

```fortran
INTEGER :: rubbish(1, 1)
LOGICAL :: LDUM(1)
```

`rubbish` is used as `CALL ALREDI(0, CMD, CPR, ':CM26d', 1, 1, rubbish)` then
`ntbl = rubbish(1, 1)`. Renaming it is in scope and worthwhile — `rubbish` is
not a name that should survive. `LDUM(1)` is removed by phase 2 if that has
landed; if not, leave it and let phase 2 take it.

Do **not** attempt to add scalar overloads to `ALREDI`/`ALREDF` in this phase.
That is a generic-interface change and the maintainer explicitly deferred it.

## Pitfalls

- `CMRD` also declares locals named `ISFLXB` and `ISADNL` that shadow
  `cm_solver_flags` module variables. This is documented existing behaviour
  (see the module header). Do not "fix" it here; it will change results.
- Contaminant runs only on models with the component active. Of the 13
  baselines, `Slapton-1D-1year-nitrate` and `Slapton-3D-1year-nitrate` exercise
  the nitrate path; confirm which exercise `INCM` before trusting a green run.
- `INCM` allocates `DUMMYCONC` and `KSPDUM` with `total_no_elements` and
  `top_cell_no`. Those are unrelated to this phase — do not fold them in.

## Expected result

- 6,000,000 B removed from `INCM`; replaced by `NX*NY` integers and a real
  buffer sized to the input file's actual declared counts.
