# Logical performance assessment: `SYmod`

## Scope and method

This is a **logical, source-only** assessment. No profile was taken and no
timings were measured. Every claim below is derived from reading
`src/modules/SYmod.f90` in full, together with the call site in
`src/modules/run_sim.f90:301`, the state module `src/parameters/SED_CS.F90`,
the dimension parameters and helper functions in `src/parameters/sglobal.f90`,
the constants in `src/parameters/CONST_SY.F90`, the workspace declarations in
`src/parameters/AL_C.F90` / `AL_G.F90`, `DCOPY` in `src/modules/utilsmod.f90`,
`ALALLF` in `src/util/mod_load_filedata.f90`, and the compiler flags in
`CMakeLists.txt`. Where a claim depends on compiler behaviour rather than on the
source alone, that is stated explicitly.

The assessment was requested for `SYmod.f90` only. Callers and callees outside
this module were read where needed to check an interface or an array shape, but
are not themselves assessed.

The relevant compile-time capacities are

| Parameter | Value | Consequence in this module |
|---|---|---|
| `NELEE` | 250 000 | leading dimension of `FDEL`, `FBETA`, `QSED`, `SLOPEJ`, `TAUJ`, `QOC` |
| `NLFEE` | 20 000 | leading dimension of `CONCI`, `QSDWAT`, `DCBSED`, `DCIPRM`, `DDIPRM` |
| `NSEDEE` | 7 | size-class capacity |
| `NVEE` | 250 000 | `FCC` in `SYMAIN` |
| `NXEE`, `NYEE` | 1 000 each | `IDUM` workspace |

`Release` builds use `-O2` (`/O2`) with **IPO/LTO forced on**
(`CMakeLists.txt:83-84`) and `-fno-fast-math` / `-fp-model=precise`
(`:702`, `:658`). The precise-FP setting matters below: `x**1.5D0` is a real
`pow` call, not `x*SQRT(x)`.

The module is entered once per water timestep through `SYMAIN` (`:3299`), which
splits on a saved pass counter. Pass 1 does checking, reading and
initialisation. Every later pass runs

```text
SYERR3 (optional)  →  SYWAT  →  SYOVER  →  SYBKER
   └─ NEPS × [ QSED reset → SYBC → SYCLTR → SYFINE
               → per-element loop { SYLINK | SYCOLM }
               → SYBED → old-time copies ]
```

## Conclusion up front

Three findings dominate, and they are independent of each other.

1. **`SYWAT` zeroes 16 MB of memory per water timestep that nothing reads.**
   `SLOPEJ = 0.0D0` and `TAUJ = 0.0D0` (`:4489-4490`) are whole-array
   assignments over `(NELEE, 4)` — 1 000 000 doubles each — regardless of how
   many elements the model actually has. Every entry that any consumer reads is
   assigned later in the same routine. This was introduced by the 2026-05-03
   "Modernization Fix" recorded at `:4434`, and it is the single most likely
   source of a measurable, catchment-size-independent slowdown in this module.

2. **The per-element gather/scatter in `SYMAIN` is pure strided access.**
   `FDEL`, `FBETA` and `QSED` are element-major, so each of the ~20 `DCOPY`
   calls per element per substep (`:3550-3603`) walks memory with a stride of
   `NELEE × 8 = 2 MB`. Every one of those accesses is its own cache line and its
   own page. This is structural, and it is the dominant cost once (1) is fixed.

3. **Most of `SYCLTR` is recomputed identically on every sediment substep.**
   Everything it consumes except `FDEL`, `DCBSED` and `DCBED` is constant across
   the `NEPS` substeps, so Parts 1 and 2 and (for `ISACKW /= 2`) the whole
   capacity calculation are repeated `NEPS` times for one distinct answer. The
   same applies to `SYFINE`'s `SYCRIT` call, and to `SYOVER`'s entire vegetation
   loop, which is invariant for the whole run.

One correctness defect should be dealt with first: `QSEDB` is read but never
written (`:3520`). Separately, `FCC` is a 2 MB fixed-size local in `SYMAIN`
(`:3384`), wasting static storage in the current builds and becoming a stack
risk under automatic/recursive compiler modes. It was missed by the 2026-05
capacity-sized-local cleanup because it is dimensioned `NVEE` rather than
`NELEE`.

---

## Part 1 — Correctness defects

### 1.1 `QSEDB` is read but never written

`QSEDB(NSEDEE, NSYBEE)` is allocated at `:230` and read at `:3520`:

```fortran
DO IB = 1, NSYB_symain
   IEL  = NSYBCD_symain (IB, 1)
   FACE = NBFACE (IEL)
   CALL DCOPY (NSED, QSEDB (1, IB), 1, QSED (IEL, 1, FACE), NELEE)
END DO
```

Nothing in the file ever assigns `QSEDB`. The only routine that was supposed to
— `SYBC` (`:481-483`) — is empty, as the module header `@warning` at `:76-81`
states. `QSEDB` is a plain `ALLOCATABLE`, so its contents on first use are
whatever the allocator returned.

The result: when `NSYB_symain > 0`, uninitialised heap is copied into `QSED` at
every boundary face on every sediment substep, and is then consumed as an
incoming sediment flux by `SYLINK`/`SYCOLM` (which read all non-outflow faces of
`QSEDE`). Note the sign convention makes this worse rather than harmless:
inflow entries are expected to be **negative**, and `SYCOLM:1173` forms
`Q(SED) = Q(SED) - QSEDE(SED,FACE)/FLS` with a documented precondition that the
result is non-negative. Positive garbage produces a negative supply volume,
which propagates into `VDSUM`, `DLSE` and `FBETAE`.

`QWATB` (`:3510`) is the mirror image: computed every substep, read by nobody,
because its only intended consumer was also `SYBC`.

The honest options are to zero `QSEDB` once in
`INITIALISE_SYMAIN_WORKSPACE`, or — better, since the boundary feature is
explicitly not implemented — to make `NSYB_symain > 0` a fatal error at input
time and delete the block, `QSEDB`, `QWATB` and `SYBC` with it. The current
state is the only one that silently produces wrong numbers.

### 1.2 `FCC(NVEE)` is a 2 MB fixed-size local in `SYMAIN`

`:3384` declares

```fortran
DOUBLE PRECISION :: FBETAE (NSEDEE), FCC (NVEE), FDELE (NSEDEE)
```

`NVEE` is 250 000, so `FCC` is 2 MB. Every other local on those lines is
`NSEDEE`- or 4-sized and therefore trivial; `FCC` is three orders of magnitude
larger than all of them combined.

Only `FCC(1:NV)` is ever touched: `SYWAT:4499` writes that slice and `SYOVER:3796`
reads it. `NV` is the number of vegetation types.

With the current flags, both supported compilers place this 2 MB constant-bound
array in static storage: it exceeds gfortran's default 64 KiB stack threshold,
and ifx defaults non-allocatable local arrays to static storage. Automatic,
recursive, or OpenMP modes can move it to the stack, so it remains a latent
2 MB frame risk, but it is not a live stack-overflow defect in the configured
builds. The capacity waste is real in either case.

It should join the other work arrays in `INITIALISE_SYMAIN_WORKSPACE`, sized
`NV` rather than `NVEE`.

### 1.3 `FPCLAY` is never validated, and `ISGSED`/`ISTEC`/`ISACKW` are not either

`SYERR2` checks `GKR >= 0`, `GKF >= 0`, `RHOSO > 0` and `BKB >= 0`
(`:2252-2260`) but never checks `FPCLAY`. When `ISTEC = 1`, `SYCRIT:1359`
evaluates

```fortran
TAUEC = 0.493D0 * EXP(K3_sycrit * FPCLAE)
```

with `K3_sycrit = 1.83*ln(10) ≈ 4.213`. A mistyped clay fraction of, say, 50
instead of 0.5 gives `EXP(210)`, i.e. `Inf`, and `SYOVER:3804` then divides by
it. The natural check is `0 <= FPCLAY <= 1`, alongside the other `SY22`
quantities.

Separately, none of `ISGSED`, `ISTEC` or `ISACKW` is range-checked. Each has a
documented "anything else" branch (`:29-36`), so a typo in `SY11` silently
selects zero overland transport capacity (`SYOVTR:3973`), the Shields critical
shear, or Engelund-Hansen channel capacity, with no diagnostic. `ISUSED` and
`NFINE` *are* checked (`:2200-2208`); the asymmetry looks accidental.

### 1.4 Uninitialised actual arguments passed to `SYCRIT`

`SYCRIT`'s fourth argument `FPCLAE` is read only when `FLAG == 1`. Three call
sites pass `FLAG = 0` and supply an undefined local:

| Site | Actual |
|---|---|
| `SYCLTR:927` | `DUM`, undefined on first use, stale thereafter |
| `SYFINE:2786` | `DUM`, documented at `:2737-2740` |
| `SYOVTR:3961` | `DUM`, undefined on first use |

This is benign in practice, but it is undefined behaviour under the standard
(an undefined actual associated with an `INTENT(IN)` dummy), and it is exactly
the pattern the module already corrected elsewhere: the history entry at `:105`
records replacing an uninitialised `IUNDEF` in `SYERR1`/`SYERR2`/`SYERR3` with
an explicit `PARAMETER = 0` "avoiding an uninitialised-variable diagnostic under
gfortran". The same fix applies here — a module-level
`DOUBLE PRECISION, PARAMETER :: FPCLAY_UNUSED = 0.0D0`.

### 1.5 Dead workspace-size check in `SYERR2`

`:2245-2249`:

```fortran
IF (NSED > 1 .AND. NERR == COUNT) THEN
   CALL DCOPY (NSED - 1, DRSED, 1, RDUM, 1)
   IDUM(1:NSED - 1) = INT (RDUM(1:NSED - 1))
   CALL ALCHK (..., 'DRSED(sed)', 'GEa', RDUM, ...)
END IF
```

The `INT` conversion writes to `IDUM` and nothing reads it. Since `DRSED`
holds particle diameters of order `1e-4 m`, it writes zeros. It also clobbers
`IDUM(1:NSED-1)`, which is shared workspace (`AL_C.F90:139`) — harmless today
because `IDUM` is rebuilt before its next use, but it is a needless coupling.

### 1.6 `DTSY` is unguarded against `DTUZ == 0`

`SYMAIN:3489` forms `DTSY = DTUZ / NEPS_symain`, and `SYLINK:3145` forms
`DTSYI = 1.0D0 / DTSY`. `SYERR3:2516` checks `DTUZ >= 0`, not `> 0`, and that
check only runs on `ISSYOK` intervals. A zero water timestep therefore produces
`Inf` in `GINFDE`/`GINFSE`. Tightening the `SYERR3` check to `GT` costs nothing.

### 1.7 Documentation that does not match the code

- `SYOVTR:3964` claims "LOG1P replaces LOG(1+X) for precision". The code on the
  next line is `LOG(1.0D0 + AJ * FTAU)`. Fortran has no `LOG1P` intrinsic and
  none was introduced; only the `MAX`-for-`DIMJE` half of that comment is true.
- The `@note` at `:83-91` correctly documents `FIRST_syackw` (`:117`) as dead
  state. It is still declared and still `SAVE`d.
- `SYMAIN`'s `TIH` argument (`:3326`) is documented as unused and is unused.

---

## Part 2 — The dominant per-timestep costs

### 2.1 `SYWAT` zeroes `(NELEE, 4)` twice per water timestep

`:4489-4493`:

```fortran
SLOPEJ = 0.0D0
TAUJ   = 0.0D0
FQCONF = 0.0D0
LRAIN  = 0.0D0
DRDROP = 0.0D0
```

`SLOPEJ` and `TAUJ` are declared `(NELEE, 4)` at `:4471-4472`, and `NELEE`
arrives as the `SGLOBAL` parameter 250 000. Each assignment therefore stores
**1 000 000 doubles = 8 MB**, on every water timestep, for every model, whatever
its size. `FQCONF` adds a further `NLFEE × 3` = 480 kB. `LRAIN` and `DRDROP` are
declared `(NLF+1:NEL)` and are correctly sized.

Now look at what the routine actually assigns. Inside `face_loop`
(`:4544-4666`), for every element and every face:

- link **side** faces are skipped (`:4547-4549`);
- no-flow faces assign `SLOPEJ = 0` and `TAUJ = 0` explicitly (`:4557-4558`);
- every other face falls through to `:4652` and `:4656`, which assign both.

So the *only* entries the blanket zeroing establishes are the two side faces of
each of the `NLF` links — and the `@note` at `:4416-4423` states, correctly, that
no consumer reads those. I verified this: `SYENGH`/`SYACKW` read `SLOPEJ`/`TAUJ`
only at link **end** faces, and `SYMAIN:3587-3588` gathers all four faces only
for **column** elements (`IEL > NLF`), for which no face is a side face.

Two replacements, both bitwise-identical to today's output:

| Replacement | Stores per water step |
|---|---|
| `SLOPEJ(1:NEL,:) = 0` / `TAUJ(1:NEL,:) = 0` | `8 × NEL` |
| Zero only the two side faces of each link | `2 × NLF` |
| Today | `8 × 250 000 = 2 000 000` |

For a 10 000-element catchment the first form is a 25× reduction; the second is
better than 100×. The second is preferable, because it also stops the routine
sweeping 16 MB through the cache and evicting everything else in the timestep.

`FQCONF` is a smaller version of the same thing. Its entries are either assigned
at `:4625` or never read (`SYMAIN:3626` reads `FQCONF(IBR,P)` only under exactly
the conditions that cause `:4622` to be true), so it can be zeroed once at
initialisation rather than per timestep.

### 2.2 Every array in the element loop is traversed against the grain

The per-element block at `SYMAIN:3546-3635` is the hot path: it runs
`NEL × NEPS` times per water timestep. Its structure is

```fortran
DO I = 1, NEL
   IEL = ISORT (I)
   CALL DCOPY (NSED, FDEL (IEL, 1), NELEE, FDELE, 1)
   DO FACE = 1, 4
      QSEDE (1:NSED, FACE) = QSED (IEL, 1:NSED, FACE)
   END DO
   ...
```

Every one of these accesses is strided by the leading dimension:

| Array | Shape | Stride in bytes | Accesses per element per substep |
|---|---|---|---|
| `FDEL` | `(NELEE, NSEDEE)` | 2 000 000 | `NSED` in, `NSED` out |
| `QSED` | `(NELEE, NSEDEE, 4)` | 2 000 000 | `4·NSED` in, `4·NSED` out |
| `FBETA` | `(NELEE, NSEDEE)` | 2 000 000 | `NSED` in, `NSED` out (columns) |
| `SLOPEJ`, `TAUJ` | `(NELEE, 4)` | 2 000 000 | 4 each (columns) |
| `CONCI`, `DCBSED`, `DDBSED` | `(NLFEE, NSEDEE)` | 160 000 | `NSED` each (links) |
| `QSDWAT` | `(NLFEE, NSEDEE, 4)` | 160 000 | `4·NSED` (links) |
| `DCIPRM`, `DDIPRM`, `GINFD`, `GINFS` | `(NLFEE, NSEDEE)` | 160 000 | `NSED` each out (links) |
| `SOSDFN` | `(NSEE, NSEDEE)` | 8 000 | `NSED` |

With `NSED = 5`, a land element performs roughly 70 accesses at a 2 MB stride
and a link element roughly 90 at strides of 160 kB–2 MB. At a 2 MB stride every
access is a distinct 4 kB page: one cache line and one TLB entry consumed per
double loaded, with no spatial locality and no possibility of vectorisation.
`DCOPY`'s strided branch (`utilsmod.f90:85-89`) is a scalar loop, so nothing
recovers it.

The loops cannot be reordered — `ISORT` imposes a donor-before-receptor order on
the element loop, and that ordering is the whole point of the routine. The fix
is to transpose, so that the per-element working set is contiguous.

**Arrays private to `SYmod`, transposable unilaterally** (module allocatables at
`:171-191`, allocated at `:226-231`):

| Current | Proposed |
|---|---|
| `SLOPEJ(NELEE, 4)`, `TAUJ(NELEE, 4)` | `(4, NEL)` |
| `CONCI(NLFEE, NSEDEE)` | `(NSEDEE, NLF)` |
| `QSDWAT(NLFEE, NSEDEE, 4)` | `(NSEDEE, 4, NLF)` |
| `DCIPRM(NLFEE, NSEDEE)`, `DDIPRM(NLFEE, NSEDEE)` | `(NSEDEE, NLF)` |
| `DDBSED_symain(NLFEE, NSEDEE)` | `(NSEDEE, NLF)` |

With `QSDWAT(NSEDEE, 4, NLF)` the four `DCOPY` calls at `:3565` collapse into a
single contiguous copy of 28 doubles — or vanish entirely, because
`QSDWAT(:,:,IEL)` can be passed straight to `SYLINK` in place of `QSDWAE`. The
same applies to `SLOPEE`/`TAUJE` (`:3587-3588`), `CONCIE`, `DCBSEE`, `DDBSEE`,
`DCIPRE`, `DDIPRE`. Most of the gather/scatter machinery disappears rather than
being made faster.

**Arrays owned by `SED_CS`, which need a cross-module decision:** `FDEL`,
`FBETA`, `QSED`, `DCBSED`, `GINFD`, `GINFS`. `QSED(NELEE, NSEDEE, 4)` alone is
56 MB of static storage; transposed to `(NSEDEE, 4, NELEE)` the entire
per-element block becomes 224 contiguous bytes — four cache lines instead of
28 pages. Consumers outside this module are `CMmod`, `FRmod` and the
visualisation interface, so this is a separate, larger piece of work; it is
listed here because it is where the remaining factor lies after the private
arrays are done.

### 2.3 Workspace is allocated at capacity, not at problem size

`INITIALISE_SYMAIN_WORKSPACE` (`:222-234`) allocates every array at its
compile-time capacity:

| Array | Elements | Bytes |
|---|---|---|
| `SLOPEJ`, `TAUJ` | `250 000 × 4` each | 16 MB |
| `QSDWAT` | `20 000 × 7 × 4` | 4.5 MB |
| `DRDROP`, `DWAT1`, `LRAIN`, `TAUK` | `250 000` each | 8 MB |
| `CONCI`, `DCIPRM`, `DDIPRM`, `DUMSED` | `20 000 × 7` each | 4.5 MB |
| `IDUM1A`, `IDUM1X`, `LDUM` | `250 000` each | 3 MB |
| others | | ~1 MB |
| **total** | | **≈ 37 MB** |

on top of roughly 17 MB of fixed-size module state (`NTSOTP_symain(NELEE)`,
`DWATOL_symain(NELEE)`, `FCG/FCROCK/FETA(NELEE)`, `DRDRIP/FDRIP/XDRIP(NVEE)`,
`DDBSED_symain(NLFEE,NSEDEE)`) and roughly 100 MB in `SED_CS`.

`NEL` and `NLF` are both available at the call site (`:3400`), so these could be
sized to the actual problem. The catch is that the dummy declarations and the
explicit `DCOPY` strides all hard-code `NELEE`/`NLFEE`, so the sizes cannot be
changed without changing those too — which is the same edit as 2.2. Doing both
at once is the right sequencing: transpose *and* size to `NEL`/`NLF`.

The 2.1 finding is a direct consequence of this: `SLOPEJ = 0.0D0` would cost
`8 × NEL` rather than `8 × NELEE` if the array were allocated at `NEL`.

---

## Part 3 — Redundant computation

### 3.1 Most of `SYCLTR` does not vary across the substep loop

Within `DO N = 1, NEPS_symain` (`:3490-3651`) the only quantities that change
are `QSED` (reset and refilled), `FDEL` (written by `SYLINK`/`SYCOLM`), and
`DCBSED`/`DCBED`/`DDBSED`/`DLS`/`FBETA`/`ARBDEP` (written by `SYBED`). `QOC`,
`TAUJ`, `SLOPEJ`, `DWAT1`, `ARXL`, `CWIDTH`, `LINKNS` and `DRSED` are all
constant — `SYMAIN` never writes `ARXL`, only reads it.

Mapping that onto `SYCLTR`:

| Section | Lines | Inputs | Varies per substep? |
|---|---|---|---|
| `QSWSUM` zeroing | `:851` | — | no |
| `SYACKW` / `SYENGH` → `GSED` | `:858-863` | `QOC`, `TAUJ`, `ARXL`, `DWAT1`, `CWIDTH`, `SLOPEJ`, `DRSED`; plus `DCBSED` **only when `ISACKW = 2`** | no, unless `ISACKW = 2` |
| Part 1 — water-speed advection | `:874-899` | `QOC`, `LINKNS` | **no** |
| Part 2 — link-end advection | `:908-941` | `QOC`, `TAUJ`, `ARXL`, `DRSED` | **no** |
| `CONCI` | `:949-978` | `FDEL`, `DCBSED`, `DCBED`, `GSED`, `QSWSUM` | yes |
| Part 3 — link-side advection | `:987-1016` | `CONCI` | yes |

So for `ISACKW ∈ {0, 1}`, everything up to and including Part 2 is computed
`NEPS` times for one distinct answer. Part 2 is not cheap: it calls `SYCRIT`
once per (link end × non-fine class), and each `SYCRIT` Shields evaluation ends
in `RSTR**BEC(IS)` (`:1367`) — an out-of-line `pow`. For `NLF = 5000`,
`NSED = 5`, `NFINE = 1` that is 40 000 `pow` calls per substep, `NEPS - 1` of
them redundant.

The restructure is to split `SYCLTR` into a per-water-step half and a
per-substep half, calling the first before `DO N = 1, NEPS_symain` and the
second inside it.

**Implementation caveat, and it is not optional.** `SYCLTR`'s `GSED` and
`QSWSUM` dummies are bound to `SYMAIN`'s `DUMMY` and `DUMSED` (`:3534`), and
both of those are re-used inside the substep loop as `SYCOLM`'s `Q` and `VDSED`
workspace (`:3594`). Today that is safe because `GSED`/`QSWSUM` are dead the
moment `SYCLTR` returns. Hoisting Parts 1 and 2 makes them live across the whole
substep loop, so they must first be given dedicated storage. See 4.1.

### 3.2 `SYOVER`'s vegetation loop is invariant for the entire run

`:3775-3784`:

```fortran
DO NVEG = 1, NV
   XDRIPE = XDRIP(NVEG)
   DRDRPE = DRDRIP(NVEG)
   ISCD   = 1 + NINT(SF2(XDRIPE, X1) + 2.0D0 * SF2(DRDRPE, D1))
   CD     = ADD(ISCD) + DRDRPE * BDD(ISCD)
   TGMD(NVEG) = PRSGOS * CD * (ONE - EXP(-2.0D0 * XDRIPE / CD)) * (DRDRPE**3) * FDRIP(NVEG)
END DO
```

`XDRIP`, `DRDRIP` and `FDRIP` are read once by `SYREAD` (`:4204-4207`) and never
written again. `TGMD` is therefore constant for the whole simulation, yet it is
rebuilt on every water timestep — including an `EXP` and a `**3` per vegetation
type. `PRSGOS` (`:3773`) is likewise recomputed from four `PARAMETER`s on every
call and should be a `PARAMETER` itself.

Moving the loop to the `PASS_symain == 1` branch removes it entirely. The one
wrinkle: `TGMD` is currently bound to `SYMAIN`'s shared `DUMMY` workspace
(`:3477`), which is clobbered between timesteps, so the hoist requires a
dedicated `NV`-sized module allocatable. That is 8 bytes per vegetation type —
and it can share the fix in 1.2, since `FCC` needs exactly the same treatment.

### 3.3 `SYCRIT` results that are constant are recomputed per element

`SYCRIT` is the most-called routine in the module. Three of its call sites
evaluate a function of static input only:

| Site | Call | Why it is constant |
|---|---|---|
| `SYOVER:3802` | `SYCRIT(ISTEC, DRSO50(ISOIL), TAUKE, FPCLAY(ISOIL), TAUEC)` | when `ISTEC = 1`, `SYCRIT:1359` ignores `TAUKE` entirely — the result depends only on `ISOIL` |
| `SYBKER:707` | `SYCRIT(ISTEC, DRSO50(BKSOIL), TAUKE, FPCLAY(BKSOIL), TAUEC)` | same |
| `SYFINE:2786` | `SYCRIT(0, DRSEDF, TAUKL, DUM, TAUEC)` | `DRSEDF` is static and `TAUK` is set once per water step by `SYWAT` — so this is constant across all `NEPS` substeps |

For `ISTEC = 1`, a per-soil `TAUEC` table built once at initialisation removes
one `EXP` per land element per water step and one per link per water step.

`SYFINE` is the better win because it sits inside the substep loop. Of the five
quantities it computes per link (`:2786-2803`), three are substep-invariant:
`TAUEC`, `BARM(LINK)` (`:2796`) and the settling factor `DUM` (`:2790-2791`).
Only `DCFMXL`/`VCFMAX` (which depend on `DCBF` and `FDELF`) and `VINFMX` (which
depends on `FBETAF` and `FDELF`) genuinely vary. Splitting the routine the same
way as `SYCLTR` removes `NLF × (NEPS - 1)` `pow` calls per water step.

### 3.4 `SYACKW` rebuilds its "cached" coefficients on every call

`:358-366`:

```fortran
DO SED = NFP1, NSED
   DGR = FDGR (DRSED (SED))
   LGR = LOG10 (DGR)
   ACKW (1, SED) = MAX (ZERO, ONE - F56 * LGR)
   IF (ISACKW == 1) ACKW (2, SED) = FA (DGR)
   ACKW (3, SED) = 1.34D0 + 9.66D0 / DGR
   ACKW (4, SED) = 10.0D0**( (2.86D0 - LGR) * LGR - 3.53D0)
   ACKW (5, SED) = ONE / SQRT (GRAVTY * KRHO * DRSED (SED))
END DO
```

`ACKW_symain` exists as module state precisely so these can be cached, and the
argument is declared `INTENT(INOUT)` for the same reason — but the loop runs
unconditionally on every call, i.e. every substep. `DRSED` is static, so rows 1,
3, 4 and 5 are constant for the whole run, as is row 2 when `ISACKW = 1`. Only
the `ISACKW = 2` (Day) recalculation at `:412-414` is genuinely per-link.

This is `NSED` iterations of `LOG10`, `10**x` and `SQRT` per substep — small in
absolute terms, but it is a cache that was disabled by the same 2026-04-07
change (`:310`) that removed the `FIRST_syackw` guard. The setup belongs in the
`PASS_symain == 1` branch.

### 3.5 `SYDR` is called three times where one pass would do

`SYACKW:399-403`, in the `ISACKW = 2` branch, per outflowing link end per
substep:

```fortran
DBED84 = SYDR (F84, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))
IF (DBED84 > ZERO) THEN
   DBED50 = SYDR (F50, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))
   DBED16 = SYDR (F16, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))
```

All three calls receive the identical distribution. `SYDR:1444-1447` opens with

```fortran
DO SED = 1, N
   FTOT = FTOT + F(FRPTR)
   FRPTR = FRPTR + INCF
END DO
```

so `FTOT` — a strided sum over `NNF` elements at stride `NLFEE × 8 = 160 kB` — is
recomputed three times, and the search loop then walks the same distribution
three times. A single routine returning all three percentiles from one pass
removes two thirds of the work in the `ISACKW = 2` path.

`SYOVTR:3914` calls `SYDR` once per land element per substep; that one is
genuine.

### 3.6 Two per-substep state copies that only matter on the first substep

`SYMAIN:3646-3647`, inside `DO N = 1, NEPS_symain`:

```fortran
CALL DCOPY (NEL - NLF, DWAT1 (NLF + 1), 1, DWATOL_symain (NLF + 1), 1)
IF (NLF > 0) CALL DCOPY (NLF, ARXL, 1, ARXLOL_symain, 1)
```

`DWAT1` is written once per water step by `SYWAT` and `ARXL` is never written by
this module. After the first substep, `DWATOL_symain` already equals `DWAT1` and
`ARXLOL_symain` already equals `ARXL`, so substeps 2..`NEPS` copy identical
values. Guarding both with `IF (N == 1)` is bitwise-identical and removes
`(NEPS - 1) × (NEL - NLF + NLF)` doubles of copying per water step.

Note this cannot be simplified further by moving the copies outside the loop:
`SYCOLM` reads `DWATOE` on every substep and must see the current `DWAT1` from
substep 2 onwards.

### 3.7 Loop-invariant work and avoidable real powers

- `SYENGH:1580` and `SYOVTR:3942` both evaluate `slope**1.5D0`. With
  `-fno-fast-math` / `-fp-model=precise` this is a `pow` call. `S * SQRT(S)` is
  the same value for `S >= 0` (which `SYWAT:4651` guarantees, being
  `ABS(...)/positive`) and is two instructions.
- `SYACKW:432-441` performs up to four real powers per (link end × size class):
  `USTR**NAW`, `UGR**(ONE - NAW)`, `BASE**MAW`, `(UK/USTR)**NAW`. `LOG(USTR)`
  is invariant across the size-class loop, so two of those could share one
  logarithm. This is the arithmetic hot spot when `ISACKW ∈ {1, 2}`.
- `SYCRIT:1367` evaluates `RSTR**BEC(IS)` unconditionally, but `BEC(3) = 0.0D0`
  — the `6 < R* <= 30` band, a common regime — for which the result is exactly
  1. A single `IF (IS == 3)` fast path skips a `pow`.
- `SYOVTR:3911-3923` builds `FLJ_ARRAY` and scans all four faces to construct
  the outflow list *before* branching on `ISGSED`, and `:3914` calls `SYDR`
  before the branch too. When `ISGSED` selects neither method (`:3973`) all of
  that is discarded.
- `SYERR1:1951-1953` and `:1998` sweep `IBR = 1, NLFEE` — 20 000 iterations —
  regardless of `NLF`. First pass only, so this is startup cost, not per-step.

### 3.8 `SOSDFN` is gathered per element, per substep, with a strided copy

`SYMAIN:3560` and `:3585`:

```fortran
CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)
```

`SOSDFN` is `(NSEE, NSEDEE)` — soil-major — so this is `NSED` loads at a stride
of `NSEE × 8 = 8 kB`. It runs for every element on every substep, and repeats
identical work for every element sharing a soil type. A transposed copy
`(NSEDEE, NSEE)` built once at initialisation makes each gather a single cache
line, and lets the array section be passed directly instead of copied. `NS ≤
NSEE = 1000` and `NSEDEE = 7`, so the transposed table is at most 56 kB.

---

## Part 4 — Memory and workspace management

### 4.1 One shared buffer serves four unrelated roles

`SYMAIN`'s `DUMMY(NELEE)` argument is bound to, in order:

| Role | Site | Lifetime |
|---|---|---|
| `SYREAD`/`SYERR2` scratch | `:3428`, `:3438` | pass 1 |
| `SYOVER`'s `TGMD(NV)` | `:3477` | inside `SYOVER` |
| `SYCLTR`'s `GSED(NLF, NFINE+1:NSED)` | `:3534` | inside `SYCLTR` |
| `SYCOLM`'s `Q(NSED)` | `:3594` | inside `SYCOLM` |

`DUMSED(NLFEE*NSEDEE)` similarly serves as `SYCLTR`'s `QSWSUM(NLF, NSED)` and
`SYCOLM`'s `VDSED(NSED)`.

This is correct today only because the four lifetimes happen not to overlap.
It is fragile, and it directly blocks two of the optimisations above (3.1 and
3.2), both of which need one of these values to survive across the substep loop.
Giving `GSED`, `QSWSUM` and `TGMD` their own module allocatables — sized
`NLF*(NSED-NFINE)`, `NLF*NSED` and `NV` — costs under 3 MB at the largest
configuration and removes the constraint.

There is also a latent sizing gap. `SYERR2:2224-2232` performs the workspace
check that is supposed to protect `DUMMY`:

```fortran
IDUM (1) = NXEE * NYEE
jedumdum = IDIMJE(NSED, NFINE) * NLF
IDUM1 (1) = MAX (MAX(NSED, jedumdum), NS, NSYB * 2)
CALL ALCHKI (ERR, 2018, SPR, 1, 1, ..., 'NELEE', 'GE', IDUM1, IDUM, NERR, LDUM)
```

The check is labelled `NELEE` but compares against `NXEE*NYEE` = 1 000 000, and
the `@note` at `:2083-2087` acknowledges this. `DUMMY` is `NELEE` = 250 000. So
the requirement `NELEE >= NLF*(NSED-NFINE)` is not actually enforced. It is
unreachable at the current capacities (`20 000 × 7 = 140 000 < 250 000`), but it
becomes a live overflow if `NLFEE` or `NSEDEE` is ever raised. Splitting the two
comparisons is a two-line fix.

### 4.2 `SYERR2` allocates 8 MB to hold six doubles

`:2168` and `:2175`:

```fortran
DOUBLE PRECISION, ALLOCATABLE :: RDUM (:)
...
ALLOCATE (RDUM (NXEE*NYEE))
```

`RDUM` is used at `:2246-2248` only, to hold `DRSED(1:NSED-1)` — at most
`NSEDEE - 1 = 6` doubles — for one `ALCHK` comparison. The allocation is
1 000 000 doubles. The history entry at `:2092` records this as a fix that
converted an automatic array to an allocatable to reduce stack usage; the size
was carried over unchanged. `ALLOCATE (RDUM(NSED))` is correct. First pass only,
so this is startup cost and page-table churn, not per-step.

### 4.3 Dead state and dead interfaces

| Item | Site | Status |
|---|---|---|
| `FIRST_syackw` | `:117` | never read or written; documented as dead at `:83-91` |
| `SYNOW_symain` | `:144`, `:3649`, `:3659` | write-only — accumulated per substep, then overwritten with `UZNOW`, and never read anywhere (the module is `PRIVATE` except for three exports) |
| `QWATB` | `:184`, `:3510` | written per substep, never read (see 1.1) |
| `DUMSED` argument of `SYREAD` | `:4019`, `:4094` | declared, never referenced in the routine body |
| `TIH` argument of `SYMAIN` | `:3326` | declared unused, is unused |
| `FORMAT 9003` | `:4352` | no referencing statement |
| `IDUM` write in `SYERR2` | `:2247` | see 1.5 |
| `issyok_symain` | `:199` | exported and `USE`d by `run_sim.f90:99`, but never referenced there |

None of these costs measurable time. They are listed because they obscure the
analysis above — in particular `SYNOW_symain` looks like a live clock and is not.

### 4.4 Obsolescent constructs

`SYCRIT:1352-1353` and `SYOVER:3768-3769` use **statement functions**, deleted
in Fortran 2018. Both are deliberate: the `@note`s at `:1310-1315` and
`:3711-3717` record that an intermediate modernisation replaced them with
`IF`/`ELSE IF` chains and was reverted for performance. The branchless form is
worth keeping — but it can be kept in a conforming way, as an internal
`ELEMENTAL FUNCTION` in a `CONTAINS` block, exactly as `SYACKW`'s `FDGR`/`FA`
and `SYWAT`'s `FQOUT` already are. The `Debug` configuration compiles with
`-stand` / `/stand` (`CMakeLists.txt:653`, `:678`), so these two sites are
generating standards diagnostics today.

`SYERR3:2488-2491` declares its four workspace arguments (`IQ`, `JMIN`, `JSORT`,
`LDUM`) with no `INTENT` at all, alone in a file that otherwise carries `INTENT`
throughout after the 2026-04-03 sweep (`:100`).

---

## Recommended order of work

| Priority | Change | Expected benefit | Numerical risk |
|---|---|---|---|
| P0 | Zero only the link side faces of `SLOPEJ`/`TAUJ` in `SYWAT` (2.1) | Removes ~16 MB of stores and cache eviction per water timestep, independent of model size. Cheapest large win in the file | **None — the extra entries are never read** |
| P0 | Fix or remove the `QSEDB` boundary path (1.1) | Correctness: stops uninitialised heap entering the sediment budget | Changes results **only** for models with `NSYB > 0`, which are wrong today |
| P0 | Move `FCC` out of `SYMAIN`'s locals (1.2) | Sizes 2 MB of static/latent-stack storage to `NV`; closes the gap left by the 2026-05 capacity cleanup | None |
| P1 | Split `SYCLTR` into per-water-step and per-substep halves (3.1); same for `SYFINE` (3.3) | Removes `(NEPS-1)/NEPS` of the channel capacity and advection work, including tens of thousands of `pow` calls per substep | None — the hoisted values are provably invariant. **Requires 4.1 first** |
| P1 | Give `GSED`, `QSWSUM` and `TGMD` dedicated storage (4.1) | Prerequisite for P1 and P2; removes a real maintenance hazard | None |
| P1 | Hoist `SYOVER`'s vegetation loop and `SYACKW`'s coefficient setup to pass 1 (3.2, 3.4) | Removes an `EXP` + `**3` per vegetation type and a `LOG10` + `10**x` + `SQRT` per size class, per timestep | None |
| P2 | Guard the two old-time copies with `IF (N == 1)` (3.6) | Removes `(NEPS-1) × NEL` doubles of copying per water step | **None — the copies are no-ops after substep 1** |
| P2 | Build a transposed `SOSDFN` at initialisation (3.8) | Turns `NEL × NEPS` strided gathers into contiguous reads | None |
| P2 | Collapse the three `SYDR` calls in `SYACKW` into one pass (3.5); replace `**1.5D0` with `S*SQRT(S)` (3.7) | Moderate, in the `ISACKW = 2` and Engelund-Hansen paths respectively | `S*SQRT(S)` is not bitwise identical to `pow(S,1.5)`; expect last-bit differences |
| P2 | Add the missing input validation: `FPCLAY` bounds, `ISGSED`/`ISTEC`/`ISACKW` ranges, `DTUZ > 0` (1.3, 1.6) | Correctness; turns three silent misconfigurations into diagnostics | None on valid input |
| P3 | Transpose the `SYmod`-private arrays and size the workspace to `NEL`/`NLF` (2.2, 2.3) | Largest remaining payoff; removes most of the gather/scatter entirely and shrinks the resident set by ~35 MB | Reordering of storage only — should be bitwise identical |
| P4 | Transpose the `SED_CS` arrays, `QSED` first (2.2) | Completes P3; `QSED` is 56 MB and the most strided array in the loop | Cross-module: `CMmod`, `FRmod` and the visualisation interface must move together |
| P4 | Remove the dead state and interfaces (4.3); convert the two statement functions to internal functions (4.4); fix the `LOG1P` comment (1.7) | Readability; clears `-stand` diagnostics | None |

For every item except the `**1.5D0` rewrite, the appropriate acceptance test is
**bitwise-identical output** over an unchanged sequence of timesteps. The P0
`QSEDB` fix is the one item that legitimately changes results, and only for
configurations that are currently producing garbage.

A closing note on sequencing: P0 and P1 are all local, low-risk edits that
between them address the two mechanisms most likely to show up as a measured
slowdown — a fixed-size 16 MB memset per timestep, and `NEPS`-fold repetition of
invariant work. P3 and P4 are the larger structural fix and should not be
started until the cheap items have been measured, because they will make the
remaining profile much harder to read.
