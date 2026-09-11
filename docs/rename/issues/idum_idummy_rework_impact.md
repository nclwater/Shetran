# Impact assessment: reworking `CMRD`, `SYMAIN` and `ALREAD`

**Scope:** the follow-up proposed at the end of
[idum_idummy_analysis.md](idum_idummy_analysis.md) — "Leave the five
interface-bound locals alone unless `CMRD`, `SYMAIN` and `ALREAD` are reworked
to take assumed-shape dummies; at that point all sixteen could become automatic
arrays of the true problem size and the `SAVE` attributes could go."

**Answer in one line:** the named three are not a closed set — the workspace
arguments reach **at least fourteen further procedures and roughly 380 call
sites**, twenty-four of which pass an array *element* — and assumed-shape is
the wrong instrument anyway, because `ALREAD`'s `N1`/`N2` describe the *record
layout*, not the buffer, so making the dummies assumed-shape does not remove
those arguments and instead forces every caller to supply a differently shaped
buffer. **A runtime-sized allocatable actual behind an assumed-size (`(*)`)
dummy gets the same storage win for a fraction of the change**, and even that
buys under 1% of `.bss`.

## 1. What the rework would actually have to cover

The three named procedures are the *first* explicit-shape dummy each local
meets. They are not where the constraint ends.

| Tier | Procedures | Why they are in scope |
|:-----|:-----------|:----------------------|
| Callers holding the five locals | `INCM`, `SIMULATION`, `VSREAD` | own the `SAVE`d arrays |
| Named in the proposal | `CMRD`, `SYMAIN`, `ALREAD` | first explicit-shape dummy |
| Reached *through* `CMRD` | `ALREDI`, `ALREDF`, `ALALLI` | `CMRD` never calls `ALREAD`; it passes `IDUM`/`DUMMY` on to these |
| Reached *through* `SYMAIN` | `SYERR1`, `SYERR2`, `SYERR3`, `SYREAD`, `SYOVER`, `SYCLTR`, `SYCOLM` | each redeclares the same buffer under its own name and shape |
| Shared sequence-association consumers | `ALCHK`, `ALCHKI`, `DCOPY` | receive `IDUM`/`DUMMY` (often as array elements) from the tier above |

Two corrections to the premise fall out of this:

* **`CMRD` does not call `ALREAD`.** It calls `ALREDI`, `ALREDF` and `ALALLI`.
  Reworking `CMRD` alone changes nothing; the `ALRED*` family has the identical
  `IDATA(N1, N2)` pattern and would have to move with it.
* **`ALALLI` already takes `IDUM(*)`** — assumed-size. It is the one routine in
  the chain that has no size constraint at all, and it is the model the rest
  should follow (see §4).

Measured call-site counts in `src/`:

| Procedure | Call sites | Files |
|:----------|-----------:|------:|
| `ALREAD` | 52 | 3 (`vs_input` 30, `sy_input` 16, `spatial_fields` 6) |
| `ALREDF` | 40 | 4 |
| `ALREDI` | 36 | 4 |
| `ALREDL` | 6 | — |
| `ALREDC` | 3 | — |
| `ALALLI` | 10 | 2 |
| `ALCHK` | 137 | 5 |
| `ALCHKI` | 96 | 4 |
| `DCOPY` | 48 | 6 |

Not every site of the shared helpers is fed from `IDUM`/`DUMMY`, but `ALCHK`,
`ALCHKI` and `DCOPY` cannot change their dummies for the sediment path without
every one of their 281 sites being revalidated.

## 2. Why assumed-shape is the wrong instrument

### 2.1 `N1`/`N2` are the record layout, not the buffer shape

`ALREAD`'s destination dummies are `IDATA(N1, N2)` / `RDATA(N1, N2)`, and modes
2 and 3 read the whole array:

```fortran
READ (IUNIT, *, IOSTAT=ios) IDATA      ! record_readers.f90:207
READ (IUNIT, *, IOSTAT=ios) RDATA      ! record_readers.f90:215
```

`N1`/`N2` therefore say *how many values this record contains*, not *how big
the caller's buffer is*. The two are unrelated at almost every site:

```fortran
CALL ALREAD(3, SYD, SPR, ':SY22', 5, NS, IDUM0, CDUM, DUMMY)   ! sy_input.f90:427
CALL DCOPY(NS, DUMMY(1), 5, GKR, 1)                            !            :428
```

`DUMMY` is declared `(NELEE)`; here it is being used as a `(5, NS)` matrix, and
the caller then de-interleaves it by stride. Make `RDATA` assumed-shape and
`READ (IUNIT,*) RDATA` reads `SIZE(RDATA)` values — 250,000, not `5*NS`. The
`N1`/`N2` arguments cannot be dropped, and the caller must now supply a buffer
whose shape is *exactly* `(5, NS)`.

Across the 52 `ALREAD` sites the requested shape is different almost every
time: `(1,1)`, `(4,1)`, `(NLF,1)`, `(NX,NY)`, `(5,NS)`, `(NSED,NS)`,
`(NSED*2,NC)`, `(3,NSYB)`, `(NELEE,NLYREE)`, … . Assumed-shape converts one
shared oversized buffer into **~25 distinct correctly-shaped automatic arrays**
spread over three caller modules. That is the opposite of a simplification, and
it is the whole of the claimed benefit.

### 2.2 Rank mismatch at every rank-1 site

`IDATA`/`RDATA` are rank 2. Most actuals are rank 1 (`IDUM`, `DUMMY`, `DCSZON`,
`DRBED`, `PBSED`, `DRSED`, `IVSCAT_VSREAD`, `ISRBED`, `NTSOBK`). That works
today only through sequence association, which assumed-shape forbids: rank must
match. Every such site needs either a rank-2 replacement buffer or a
`RESHAPE`/pointer-remap.

### 2.3 Array-element actuals

`spatial_fields.f90:157` passes an array element:

```fortran
CALL ALREAD(3, IUNIT, OUNIT, NEXT(:LN), NLF, 1, IDUM0, CDUM, IDUM, AEL(1, I2))
```

An array-element designator can only be an actual argument for a
sequence-associated dummy. Against an assumed-shape dummy it is simply illegal;
this site needs `AEL(:, I2)` plus a bounds rework, because `AEL`'s own bounds
are the expression `AEL(1 + NLF*(FLAG/N2) : NELEE - (NELEE-NEL)*(1/N2), N2)`.

Downstream there are **24 further sites** that pass `IDUM(...)` or `DUMMY(...)`
as an element — `DCOPY(NCONCM, DUMMY(INDX+1), 1, ...)` in `cm_input`,
`DCOPY(NS, DUMMY(2), 5, ...)` in `sy_input`, `ALCHK(..., DUMMY(NLF+1), ...)`
and `ALCHKI(..., IDUM(NSYB+1), ...)` in `sy_validation`. Each of these is legal
today and becomes non-conforming the moment its parent is assumed-shape without
the `CONTIGUOUS` attribute.

### 2.4 Open sections against an assumed-size fallback

If the alternative instrument — assumed-size `(*)` — is used instead, two sites
still need touching, because an assumed-size array cannot take an open-ended
section:

```fortran
CALL ALCHKI(..., IDUM(ICOL1:), NERR, LDUM)   ! sy_validation.f90:461, 462
```

These become `IDUM(ICOL1:NEL)`. That is the entire cost on that path — two
lines.

### 2.5 One buffer, many identities

`SYMAIN`'s single `DUMMY(NELEE)` is re-presented to its callees under seven
different names and shapes: `TGMD(NV)` in `SYOVER`, `Q(NSED)` in `SYCOLM`,
`GSED` in `SYCLTR`, `DUMMY(NELEE)` in `SYERR2`/`SYREAD`, and so on. The same is
true of `IDUM`, which `SYERR3` receives as `IQ`/`JMIN`/`JSORT`. "Automatic
arrays of the true problem size" presumes one true size exists; here there are
seven, and they are only compatible because the buffer is oversized.

## 3. What the rework would buy

Verified on the current Debug binary:

```
$ nm --size-sort -S build/debug/bin/shetran | grep -E '\b(idum|dummy)\.[0-9]+'
48,000,000 bytes in 16 symbols
$ size -A build/debug/bin/shetran | grep bss
.bss   1,659,796,816
```

| Quantity | Bytes | Share of `.bss` |
|:---------|------:|----------------:|
| All sixteen locals | 48.0 MB | 2.89% |
| The mechanical trim already recommended | 21.8 MB | 1.31% |
| **What this rework adds on top** | **16.0 MB** | **0.96%** |

For scale, the five largest single static objects in the same binary:

| Symbol | Size |
|:-------|-----:|
| `__cm_plant_state_MOD_pdzf3` | 191 MB |
| `__mn_state_MOD_mn_plant_state` | 137 MB |
| `__mn_state_MOD___def_init_...` | 137 MB |
| `vsthen.3` / `vspsin.4` / `cq.5` | 95 MB each |
| `__vs_config_MOD_jvsaln` | 76 MB |

A single one of these is twelve times the rework's yield. If static footprint
is the goal, this is not where the work belongs.

## 4. The cheaper route that gets the same result

The storage win does not come from assumed-shape dummies. It comes from the
**actual argument being sized at run time instead of at compile time**. That
needs only two things, neither of which disturbs sequence association:

1. **Caller side** — replace
   `INTEGER, DIMENSION(NXEE*NYEE), SAVE :: IDUM`
   with `INTEGER, ALLOCATABLE, SAVE :: IDUM(:)`, allocated once to the true
   requirement (`NX*NY`, `total_no_elements`, …). Allocatables are contiguous,
   so every array-element actual in §2.3 stays legal, and `DCOPY`, `ALCHK`,
   `ALCHKI` and `ALALLI` need no change at all.
2. **Callee side** — change only the *declaration* of the workspace dummy from
   `IDUM(NXEE*NYEE)` to `IDUM(*)` in `CMRD`, `SYMAIN`, `SYERR1`, `SYERR2`,
   `SYREAD`. This removes the "actual must have at least `NXEE*NYEE` elements"
   constraint, which is the only thing pinning the caller's size. No call site
   changes.

The precedent is already in the tree, in the same components:

* `sy_workspace` holds `IDUM1A`, `IDUM1X`, `LDUM`, `DUMSED` as allocate-once
  module arrays, set up by `INITIALISE_SYMAIN_WORKSPACE`. `SYMAIN`'s `IDUM` and
  `DUMMY` are the *only two* of its workspaces still travelling as arguments —
  and `SIMULATION` declares its 6 MB pair for no other purpose than to hand
  them to `SYMAIN` (`simulation_driver.f90:169-170`, used only at `:274`).
  Moving them into `sy_workspace` deletes two arguments from a 60-argument
  call, removes 6 MB from `.bss`, and makes the allocation conditional on
  sediment being active.
* `vs_input` already uses allocatable `IVSDUM_VSREAD`, `RVSDUM_VSREAD`,
  `ISDUM_VSREAD`, `RSDUM_VSREAD` for exactly this job.
* `mn_state` wraps the same three names in an allocatable
  `MN_WORKSPACE_TYPE`.

Likewise `INCM`'s pair exists only to feed `CMRD` (`cm_input.f90:762-763`, used
only at `:809`), so the same move applies there.

### `ALREAD`'s latent non-conformance

The VS08c defect recorded in §"A latent non-conformance" of the analysis is
fixed by this route too, and more cheaply: `VSREAD`'s `DUMMY` becomes
allocatable and is sized to `MAX(NELEE, NX*NY)` — or, better, the dead
`DUMMY` argument at that site is replaced by a one-element placeholder, since
mode 4 never references `RDATA`. Neither needs `ALREAD`'s interface touched.

## 5. Effort, risk and verification

| Option | Files | Call sites touched | Risk |
|:-------|------:|-------------------:|:-----|
| A. Mechanical trim (already recommended) | 8 | 0 | Low — extents only |
| B. Allocatable actual + `(*)` dummy (§4) | ~10 | 2 (`sy_validation.f90:461-462`) | Low–moderate — no sequence association changes |
| C. Assumed-shape as proposed | ~20 | ~380 to revalidate, ≥25 rewritten | High |

Option C's risk is not spread evenly. Sequence association is exactly the
construct that compiles silently when wrong: a rank-1 actual against a rank-2
assumed-shape dummy is caught, but a `CONTIGUOUS`-less element actual passed to
`DCOPY(dx(*))` is a diagnosable-but-usually-undiagnosed defect that shows up as
wrong numbers in the sediment and contaminant outputs, not as a crash.

Verification for any option is the same and is already in place: the 17
regression models under `examples/`, compared with
`examples/check_results_consistency.py`. Options A and B should be
bit-identical. Option C should also be bit-identical in principle, but the
`DCOPY` stride de-interleaving in `cm_input` and `sy_input` is where a
reshaping mistake would first show, so those two modules deserve a targeted
diff of the read-in parameter values rather than only end-of-run comparison.
A `-fcheck=bounds` Debug run over the examples is worth adding for B and
mandatory for C.

## 6. Recommendation

1. **Do option A** as the analysis already recommends. It is mechanical,
   independent of everything here, and returns 21.8 MB.
2. **Do option B for `SYMAIN` and `CMRD`**, as a workspace-ownership change
   rather than an interface change: move `IDUM`/`DUMMY` into `sy_workspace`
   and a corresponding `cm_*` workspace, drop the two arguments, and relax the
   receiving declarations to `(*)`. This is the change that actually delivers
   what §"Recommendation" of the analysis wanted — runtime-sized buffers and no
   `SAVE` — and it does so without touching a single call site outside the two
   `ALCHKI` lines.
3. **Do not do option C.** Assumed-shape does not remove `ALREAD`'s `N1`/`N2`,
   it multiplies one shared buffer into ~25 shaped ones, it invalidates 24
   array-element actuals and one array-element call site, and it drags
   `ALREDI`, `ALREDF`, `ALCHK`, `ALCHKI` and `DCOPY` — 317 further call sites —
   into a change whose entire yield is 0.96% of `.bss`. If `ALREAD` is ever
   modernised, the sound version is a set of shape-specific wrappers
   (`ALREAD_LIST_R`, `ALREAD_GRID_I`, …) behind a generic interface, converted
   one record type at a time — not a signature change to the existing routine.

If static footprint is the real driver, `cm_plant_state:PDZF3` (191 MB) and
`mn_state:MN_PLANT_STATE` (137 MB) are each worth more than this entire
exercise.
