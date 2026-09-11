# Phase 4 — Sediment chain

**Depends on:** phase 3. **Independent of** phases 5, 6, 7.
**Chain:** `SIMULATION` → `SYMAIN` → `SYERR1`, `SYERR2`, `SYERR3`, `SYREAD`,
`SYOVER`, `SYCLTR`, `SYCOLM`.

## Current state

`src/driver/simulation_driver.f90` declares

```fortran
INTEGER, DIMENSION(NXEE*NYEE), SAVE :: IDUM      ! 4,000,000 B
DOUBLEPRECISION, DIMENSION(NELEE), SAVE :: DUMMY ! 2,000,000 B
```

and uses them for exactly one thing: handing them to `SYMAIN`. Confirm:

```bash
grep -n "IDUM\|DUMMY" src/driver/simulation_driver.f90
```

Two declarations and one live call site. Nothing in the driver reads them.

`SYMAIN` then re-presents the same two buffers to seven callees under seven
different names and shapes — `TGMD(NV)` in `SYOVER`, `Q(NSED)` in `SYCOLM`,
`GSED` in `SYCLTR`, `IQ`/`JMIN`/`JSORT` in `SYERR3`, `IDUM(NXEE*NYEE)` and
`DUMMY(NELEE)` in `SYERR1`/`SYERR2`/`SYREAD`. They are compatible only because
the buffer is oversized.

## True requirement

Derived by reading every subscript; re-derive rather than trusting this table.

| Consumer | Integer workspace | Real workspace |
|:---------|:------------------|:---------------|
| `SYERR1` | `IDUM(IEL)` over `1..NEL`; `IDUM(LINK)` over `1..NLF`; **`IDUM(IBR)` over `1..NLFEE`** | — |
| `SYERR2` | `IDUM(1:NSED-1)`, `IDUM(NSYB+BB)` → `2*NSYB` | `DUMMY(1:NEL)`, `DUMMY(NLF+1)` → `NEL` |
| `SYREAD` | `ALREAD` modes 2: `MAX(NNN, 5, 3*NSYB)` | `NREQ` — see below |
| `SYOVER` | — | `TGMD(NV)` → `NV` |
| `SYCOLM` | — | `Q(NSED)` → `NSED` |

So the integer workspace needs `MAX(total_no_elements, NLFEE)` and the real
workspace needs `MAX(total_no_elements, NREQ_syread)`.

**`NLFEE`, not `total_no_links`.** `SYERR1` contains `DO IBR = 1, NLFEE`, and
the `ALCHKI` that consumes the result checks the range `1..NLFEE`. It scans the
whole allocated link capacity, not the active count. This looks like a bug and
is not one to fix here — changing it changes which elements get validated.
Leave the loop alone and size the workspace to accommodate it.

`SYREAD` already states its own real-workspace requirement in three `NREQ`
checks (`8`; `MAX(MAX(5,NSED)*NS, 3*NV)`; `MAX(3*NSYB, NSED*NSYC(1),
NSED*2*NSYC(3))`). Keep all three checks and their diagnostics exactly as they
are — they validate input, they do not manage capacity.

## Target

Extend `src/sediment/sy_workspace.f90`:

```fortran
INTEGER, ALLOCATABLE :: sy_element_codes(:)
   !! Per-element and per-link integer scratch for the SY validators and
   !! readers. Extent is MAX(total_no_elements, NLFEE): SYERR1 scans the full
   !! allocated link capacity at `DO IBR = 1, NLFEE`.
DOUBLE PRECISION, ALLOCATABLE :: sy_element_values(:)
   !! Per-element floating-point scratch for the SY validators and readers.
   !! Extent is MAX(total_no_elements, the NREQ values computed in SYREAD).
```

## Work

1. Add the two arrays and allocate them in `INITIALISE_SYMAIN_WORKSPACE`
   alongside the existing `IDUM1A`/`IDUM1X`/`DUMSED`, via `ensure_capacity`.
   That routine already runs on `SYMAIN`'s first pass, which is after `INFR`
   has established `total_no_elements` — verify this ordering holds before
   relying on it (`src/frame/frame_setup.f90`, `CALL INFR` is first in
   `FRINIT`).
2. In `SYERR1`, `SYERR2`, `SYREAD`, replace the `IDUM`/`DUMMY` dummy arguments
   with `USE sy_workspace` access to the new arrays. Rename the local uses.
3. In `SYERR3`, `SYOVER`, `SYCLTR`, `SYCOLM`, the workspace arrives under a
   different name and a genuinely different meaning (`TGMD(NV)`, `Q(NSED)`).
   These are *not* the same workspace. Give each its own small module array or,
   where the extent is small and fixed (`NV`, `NSED`, both bounded by `NVEE`
   and `NSEDEE`), an ordinary local — see phase 8 for that pattern. Do not
   route them through `sy_element_values`.
4. Drop `IDUM` and `DUMMY` from `SYMAIN`'s argument list, and from
   `simulation_driver.f90`'s call. Delete the two declarations there.
5. Fix the two open-ended sections that assumed-size cannot express, if you end
   up with an assumed-size dummy anywhere: `src/sediment/sy_validation.f90`
   has `IDUM(ICOL1:)` at two adjacent `ALCHKI` calls. With module access these
   become `sy_element_codes(ICOL1:NEL)`.

## Pitfalls

- `SYMAIN`'s argument list is long and positional. Deleting two arguments from
  the end is safe; confirm there is no other caller (there is one live call and
  one commented-out one in `simulation_driver.f90` — update the comment too, or
  delete it).
- `PASS_symain` gates initialisation. `sy_element_codes` must be allocated
  before the first use in `SYERR1`, which runs inside the same first-pass
  block, after `INITIALISE_SYMAIN_WORKSPACE`. Keep that order.
- `SYERR2` writes `DUMMY(NLF+1)` and passes it as an array element to `ALCHK`.
  Module allocatables are contiguous, so this stays legal. Do not convert these
  arrays to `POINTER`.
- Sediment runs only when the component is active. Of the 13 baselines, check
  which exercise `SYMAIN` at all — `Cobres` does. A phase that breaks sediment
  and is only tested on a model without it will pass falsely.

## Expected result

- 6,000,000 B removed from `simulation_driver`; the replacement is heap and
  sized to the actual problem, and is allocated only when sediment is active.
- Two arguments removed from a 60-argument call.
