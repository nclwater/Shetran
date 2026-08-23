# Logical performance assessment: `MNmod`

## Scope and method

This is a **logical, source-only** assessment. No profile was taken and no
timings were measured. Every claim below is derived from reading
`src/modules/MNmod.f90` in full, together with the routines it calls in
`src/util/mod_load_filedata.f90`, the dimension parameters in
`src/parameters/sglobal.f90` and `src/parameters/AL_C.F90`, and the compiler
flags in `CMakeLists.txt`. Where a claim depends on compiler behaviour rather
than on the source alone, that is stated explicitly.

The assessment was requested for `MNmod.f90` only. Callers (`CMmod:cmsim`) and
callees outside this module were read where needed to check an interface, but
are not themselves assessed.

The module is entered once per contaminant timestep through `MNCONT`
(`:424`), which calls `MNPLANT` and then `MNMAIN` (`:3028`). `MNMAIN` splits on
a saved pass counter: pass 1 does checking, reading and initialisation; every
later pass runs the timestep chain

```text
MNERR3 → MNRED2 → MNERR4 → MNINT2
       → MNTEMP → MNEMT → MNENT → MNEMPH → MNENPH → MNEDTH
       → MNMAN → MNLTHM → MNLTN → MNCO2 → MNGAM → MNAMM → MNNIT
       → MNOUT
```

Seventeen routines therefore carry per-timestep cost, and all of them share the
same loop structure and the same array layout. That shared structure is the
dominant finding.

## Conclusion up front

`MNmod` is **memory-latency bound by construction**. Every one of its fifty
private state arrays is dimensioned `(nel, ncetop)` — element-major — while
every compute loop in the module is element-outer, cell-inner. In Fortran's
column-major order that makes essentially every array reference in the module
stride by `nel` doubles. At `nel = 2000` that is a 16 kB stride: one cache line,
and probably one TLB entry, consumed per element touched, with no spatial
locality and no vectorisation available.

Optimisation effort should therefore go into **array layout, redundant sweeps
and dead diagnostics**, not into arithmetic. Before any of that, three
out-of-bounds accesses that follow from a single off-by-one in the layer-count
validation should be fixed, because they block bounds-checked validation of
everything else (`Debug` uses `-fcheck=bounds` / `/check:bounds`,
`CMakeLists.txt:651,694`).

The largest single cheap win identified here was unrelated to layout: `MNOUT`
recomputed six whole-domain area integrals on every timestep and used them
roughly once every twenty-four hours of model time. That finding has since
been fixed in `39a8e8b`.

### Resolution status on `fix_v4.5.3_runtime`

The detailed findings below retain the source evidence from the assessed
baseline. The following findings have since been resolved by commits after
`dba3bad`:

| Finding | Status | Resolution |
|---|---|---|
| **1.7** | ✅ Fixed | `e0aef0b` moves the scheduled-input arrays into persistent module workspace, restoring their documented lifetime between timesteps. |
| **3.1** | ✅ Fixed | `39a8e8b` moves the six current-store integrals inside the periodic-output guard. |
| **4.1** | ✅ Fixed | `e0aef0b` separates one-time setup into `MNINITIALISE`, allocates persistent timestep workspace once, and leaves `MNMAIN` allocation-free. |

All other finding statuses remain unchanged. References below to the old code
and its costs are retained to explain what was fixed.

---

## Part 1 — Correctness and consistency defects

These are reported first because two of them enable memory corruption, and
because the bounds-checked build cannot be used to validate any later change
while they remain.

### 1.1 The `NLYR` bound check is off by one

`MNERR1` validates the soil-layer count as `1 <= NLYR(iel) <= NLYREE`
(`:1294-1296`). But every consumer indexes one past the layer count:

| Site | Expression |
|---|---|
| `mnamm` | `:209` — `nlyrbt(nelm, jlyr + 1) - 1` |
| `mnedth` | `:611` — same |
| `mnint2` | `:2564` — same |
| `mnout` | `:3707` and `:3785` — same |

with `jlyr` running to `nlyr(nelm)`. `NLYRBT` is declared `(NEL, NLYREE)`
throughout. `sglobal.f90:126` sets `NLYREE = 20` and documents it as
"Soil-layer boundary capacity (**maximum layers plus one**)". The check should
therefore be `NLYR(iel) <= NLYREE - 1`.

An element configured with 20 layers passes validation and then reads
`nlyrbt(nelm, 21)`, which in column-major storage aliases `nlyrbt(nelm+1, 1)` —
silently, in a `Release` build.

### 1.2 Two out-of-bounds writes in `MNERR1` follow from the same off-by-one

`DUMMY3` is declared `INTEGER(NLYREE)` (`:1163`, backed by a stack array in
`MNMAIN:3109`). The loop at `:1308-1311` writes `DUMMY3(NLAYER + 1)` for
`NLAYER = 1..TOPLYR`, where `TOPLYR = NLYR(IEL)`. With `NLYR = NLYREE` that is a
write to `DUMMY3(NLYREE + 1)`.

Line `:1317` then reads `DUMMY2(TOPLYR + 1, IEL)` with `DUMMY2(NLYREE, NELEE)` —
same class, and here the aliased read silently returns another element's
layer-1 boundary, so the "top-layer boundary equals `NCETOP+1`" check can pass
against the wrong datum.

Correcting 1.1 closes both.

### 1.3 Two incompatible definitions of the active cell range

The module uses two different vertical ranges for what is nominally the same set
of active cells:

| Range | Used by |
|---|---|
| Layer-clipped: `MAX(ncebot, nlyrbt(jlyr)) .. nlyrbt(jlyr+1)-1` | `mnamm:209`, `mnedth:611`, `mnint2:2564`, `mnout:3707`, `mnout:3785` |
| Plain: `nbotm .. ncetop` | `mnco2:357`, `mngam:2154`, `mnlthm:2779`, `mnltn:2941`, `mnman:3319`, `mnnit:3512`, `mnout:3748` |

`mnamm` **writes** `namm1`, `ntrf`, `vol`, `plamm` and `imamm` over the clipped
range. `mnnit` **reads** `imamm` (`:3525`, `:3580`), `ntrf` (`:3560`, `:3569`)
and `namm1` (`:3516`) over the plain range. If `nlyrbt(nelm,1) > nbotm` for any
element, `mnnit` consumes values this timestep never wrote.

`MNOUT` contains the same split internally, across three loops in one routine:

- zeroes its accumulators over the **clipped** range (`:3707`);
- accumulates into them over the **plain** range (`:3748`);
- sums totals from them over the **clipped** range again (`:3785`).

So cells in the gap accumulate from uninitialised heap, and are then never read.

This works today only because `nlyrbt(:,1)` is normally 1, which makes
`MAX(ncebot, nlyrbt(nelm,1))` collapse to `ncebot`. Nothing in the module or in
`MNERR1` enforces that.

Note also that the layer loops in `mnamm` and `mnedth` exist *solely* to obtain
`jsoil` for `kddsol(jsoil)` and `vspor(jsoil)`. A precomputed per-cell soil
index would flatten both to a single cell loop and remove the divergence at the
same time.

### 1.4 `mnemph` and `mnenph` compute byte-identical functions

`mnemph` (`:686-701`) and `mnenph` (`:849-864`) evaluate the same five-band
segmented function of the same input `vspsi`, with the same coefficients, and
differ only in the array written (`emph` versus `enph`). Both are called every
timestep from `MNMAIN:3212-3213`.

The module therefore evaluates the identical relationship twice over every
active cell, including **two `log10` calls per cell per timestep**. `enph` has
exactly one consumer, `mnamm:238`.

Both routines also assign the **default-real** literal `0.6` to a double
precision array in the wettest band (`:691` and `:855`), giving
0.60000002384185791, while every other constant in those routines carries a
`d0` suffix.

`mnemt` (`:734`) and `mnent` (`:898`) are near-duplicates of each other too, but
they genuinely differ: the lowest temperature band is `>0.0 → 0.02T` in `mnemt`
and `>2.0 → -0.05 + 0.025T` in `mnent`. That pair is a candidate for
parameterisation, not for elimination.

### 1.5 Dummy-argument shapes disagree along the call chain

`MNCONT` declares the arguments it receives from `cmsim` using the **active
counts**; `MNMAIN`, to which it passes the same storage straight through,
re-declares them using the **compile-time capacities**:

| Argument | `MNCONT` | `MNMAIN` |
|---|---|---|
| `ICMBK` | `(NLF, 2)` `:449` | `(NLFEE, 2)` `:3051` |
| `ICMREF` | `(NEL, 4, 2:2)` `:450` | `(NELEE, 4, 2:2)` `:3052` |
| `ICMXY` | `(NX, NY)` `:451` | `(NXEE, NY)` `:3053` |
| `NCOLMB` | `(NEL)` `:472` | `(NELEE)` `:3073` |
| `NLYR` | `(NEL)` `:473` | `(NELEE)` `:3074` |
| `PNETTO` | `(NEL)` `:485` | `(NELEE)` `:3080` |

In each case `MNMAIN`'s dummy is larger than the actual argument it is
associated with. This is benign at `-O2` only because indices never exceed the
true bounds; it is a hard failure under bounds checking.

The worst instance is `PPHI`. `MNMAIN:3241` passes the module array
`pphi(nel, ncetop)` to `MNOUT`, which declares it `PPHI(NELEE, LLEE)`
(`:3648`) — and then **never uses it**. It is also the only place in the module
where a private module array is round-tripped through an argument list rather
than accessed directly. The argument should simply be removed.

`MNINIT` passes `DELTAZ`, declared `(LLEE, NEL)` at `:2275`, to `ALINTP`, which
declares it `(LLEE, NELEE)` (`mod_load_filedata.f90:1023`) — same class, same
benign-under-`-O2` status.

### 1.6 Gaps in `MNERR0`

`MNERR0` (`:969`) checks `NCONEE`, `NELEE`, `NLFEE`, `NSEE`, `NVEE` and `NXEE`
against their active counts, and `NLYREE`, `NMNEEE`, `NMNTEE` against zero. It
does not check:

- `NYEE >= NY`, although `NYEE` dimensions every `ALALLF` call in `MNRED1`
  (`:4488`, `:4490`, `:4534`, `:4536`) and `MNRED2` (`:4677`-`:4723`);
- any relation between `LLEE` and `NLYREE`, although `LDUM2(LLEE)` is used as
  the workspace for layer-length checks at `MNERR1:1313`, whose range can reach
  `NLYREE + 1`.

### 1.7 `MNRED2`'s documented persistence no longer exists

> **Status: ✅ Fixed in `e0aef0b`.** `CDPTHB`, `CLTFCT`, `CMNFCT`, `CNRAL`,
> `CNRAM`, `CTOT`, `NAMFCT`, `NDPTHB`, and `NTOT` now live in the persistent
> `MN_WORK` module workspace allocated during `MNINITIALISE`, so inactive input
> files leave the previous timestep's values available as documented.

The header at `:4588-4590` states: "If a file is not active in the current
timestep, only its flag is set false; the previous data arrays are not
overwritten."

That contract is unimplementable in the current code. `CDPTHB`, `CLTFCT`,
`CMNFCT`, `CNRAL`, `CNRAM`, `CTOT`, `NAMFCT`, `NDPTHB` and `NTOT` are local
allocatables in `MNMAIN` (`:3112-3118`), freshly allocated on every call
(`:3139-3141`) and auto-deallocated on return. Nothing persists between
timesteps, and `INTENT(OUT)` on a non-allocatable dummy does not initialise
them.

It is currently harmless because `MNERR4` and `MNINT2` both gate every use on
`ISADDN`/`ISADDC`. The safety therefore rests entirely on that gating remaining
complete, while the header documents a different mechanism.

---

## Part 2 — Array layout and loop ordering

### 2.1 The whole module traverses memory against the grain

All fifty private arrays are declared `(nel, ncetop)` at `:64-113` and allocated
that way at `:498-520`. Every compute loop in the module has the form

```fortran
do nelm = nlf+1, nel
   do ncl = nbotm, ncetop
      ... array(nelm, ncl) ...
```

In column-major order `array(nelm, ncl)` advances by `nel` elements per inner
iteration. Every load and store in the module's hot path is therefore strided.

The loops **cannot** simply be swapped: the cell range depends on the element,
through `NCOLMB(nelm)` or the per-element layer table, so the element loop must
stay outer. The fix is to transpose the module arrays to `(ncetop, nel)`, which
also aligns them with the arrays arriving from VS — `VSTHE(NCETOP,NEL)`,
`VSPSI(NCETOP,NEL)`, `DELTAZ(LLEE,NEL)`, `ZVSNOD(LLEE,NEL)` are already
cell-major.

Two constraints, both manageable:

- `ALINTP` writes `CELL_CONCENTRATION(NEL, NCETOP)`
  (`mod_load_filedata.f90:1027`) and is shared with `FRmod.f90:4697`, so it
  cannot be changed unilaterally. The nine `ALINTP` calls in `MNINIT`
  (`:2340-2403`) are first-pass only, so a post-call transpose there costs
  nothing measurable.
- `SSS1`/`SSS2` and `CCCC`/`SSSS` are `(NEL, NCETOP+1)` and owned by CM. They
  stay as they are.

The module already half-acknowledges the problem: `MNCONT:502` allocates
`dummy4(ncetop, nel)` and `dummy6(nel, ncetop)` on the same line.

### 2.2 The transposition tax is already being paid, inefficiently

`MNERR3` contains **eight** separate copy nests (`:1828-1957`) that transpose
full `(1:NEL, 1:NCETOP)` arrays into a scratch buffer so that `ALCHK` can be
handed contiguous per-element cell slices. Each nest:

- reads the source with `NCE` inner — that is, strided — and writes unit-stride,
  so each copy is a cache-hostile gather;
- copies the **entire** array although only `NCOLMB(IEL):NCETOP` for
  `IEL = NLF+1..NEL` is ever checked, so links and sub-column cells are copied
  for nothing.

Two of the eight are pure waste today, independently of any layout work:

| Site | Copy | Why redundant |
|---|---|---|
| `MNERR3:1923-1927` | `DUMMY4(NCE,IEL) = VSTHE(NCE,IEL)` | Identical index order **and** identical shape. Pass `VSTHE(NCEBOT:NCETOP, IEL)` directly. |
| `MNERR1:1343-1347` | `DUMMY4(NCE,IEL) = DELTAZ(NCE,IEL)` | Same index order; the column slice is contiguous in both. Pass `DELTAZ(NCEBOT:NCETOP, IEL)` directly. |

`MNERR1:1352-1358` is a third variant: it copies **one** `ZVSNOD` element at a
time inside a loop that calls `ALCHK` once per cell.

Transposing the module arrays per 2.1 removes most of the remaining six nests
outright.

---

## Part 3 — Redundant computation

### 3.1 `MNOUT` computes six whole-domain integrals it almost always discards

> **Status: ✅ Fixed in `39a8e8b`.** The `TOTADN`, `TOTADC`, `TOTLOS`, `TOTN`,
> `TOTC`, and `TOTCO2` initialisation and integration loop now execute only
> inside the periodic-output condition.

The loop at `:3775-3801` sweeps every element and every active cell to form
`TOTADN`, `TOTADC`, `TOTLOS`, `TOTN`, `TOTC` and `TOTCO2`, including a
`**(GNN - 1.0D0)` power per cell (`:3787`).

All six are consumed **only** inside `IF (UZNOW >= HRPRNT * NPRNT + MNSTRT)` at
`:3804` — that is, once per twenty-four hours of model time. Moving the loop
inside the guard is a one-line change and is the cheapest substantial win in the
file.

### 3.2 Seven of `MNOUT`'s fifteen accumulator arrays are write-only

Allocated at `:3687-3689`, zeroed at `:3708-3722`, accumulated at `:3749-3763`,
and never read:

| Array | Accumulates | Read by |
|---|---|---|
| `ADNITT` | nitrate additions | — |
| `DETOT` | denitrification | — |
| `GAMTOT` | raw mineralisation | — |
| `IMAMMT` | ammonium immobilisation | — |
| `MINTOT` | gross mineralisation | — |
| `PLNITT` | nitrate plant uptake | — |
| `STOT` | nitrate source/sink total | — |

The other eight (`ADAMMT`, `ADDCT`, `ADORNT`, `CDOTOT`, `IMNITT`, `NTRTOT`,
`PLAMMT`, `VOLTOT`) are read at `:3790-3798`.

That is 7 × `NEL` × `NCETOP` doubles of permanent allocation, plus seven strided
read-modify-writes per cell per timestep, producing nothing.

Removing them makes five **module** arrays write-only in turn:

| Module array | Written | Only reader |
|---|---|---|
| `gamtmp` | `mngam:2184` | `GAMTOT` `:3755` |
| `denit` | `mnnit:3534` | `DETOT` `:3754` |
| `plnit` | `mnnit:3555` | `PLNITT` `:3761` |
| `snit` | `mnnit:3575` | `STOT` `:3762` |
| `miner` | `mnamm:230,233` | `MINTOT` `:3758` |

So twelve full-size arrays and their per-cell arithmetic hang off seven unused
accumulators. `miner` is additionally a scalar candidate regardless: its only
other read is at `mnamm:255`, in the same iteration that writes it.

**This is a modelling decision, not a mechanical cleanup.** The `@warning` at
`:3618-3624` states that `TOTLOS` omits nitrate plant uptake and
denitrification, and that `TOTADN` omits `ADNITT` — that is, these accumulators
arguably *belong* in the totals and the defect is the accounting, not the
arrays. The choice is to wire them up or to delete them. Leaving them
accumulating into the void is the only option that costs without buying
anything.

### 3.3 The fixed-point iterations store to strided module arrays every iteration

| Routine | Arrays written inside the iteration loop | Iterations |
|---|---|---|
| `mnamm` `:229-256` | `miner`, `imamm`, `ntrf`, `vol`, `plamm`, `namm1` | up to 20 |
| `mnlthm` `:2807,2814` | `clit1`, `chum1` | up to 20 |
| `mnltn` `:2971` | `nlit1` | up to 20 |
| `mnman` `:3342,3345` | `cman1`, `nman1` | up to 20 |

Each is a strided read-modify-write. Accumulating in scalars and storing once
after convergence converts up to twenty strided memory operations per cell into
register work plus one store. This compounds multiplicatively with the layout
problem in 2.1, and is low-risk: the stored value is unchanged.

Two further points inside `mnamm`:

- `miner` is loop-invariant. `gam(nelm,ncl)` does not change inside the
  iteration, so the branch at `:229-235` recomputes a constant. Only
  `imamm = MIN(-gam, kuamm*nammh)` genuinely depends on the iterate.
- `retamm1` (`:225`) evaluates `(namm1/mncref)**(gnn - 1.0d0)` — an out-of-line
  `pow` — on every iteration. When `gnn == 1.0` (linear adsorption) that term is
  identically 1 and `retamm1` is loop-invariant. A `gnn == 1` fast path removes
  up to twenty `pow` calls per cell per timestep, and applies equally to
  `retamm` at `:216` and to the two `**(GNN - 1.0D0)` sites in `MNOUT`
  (`:3724`, `:3787`).

### 3.4 `MNINT2` section 4 is a no-op nest

`:2561-2568` walks the per-element layer structure, computes `JSOIL` — which is
never used — and assigns the constant `0.500D0` to `PPHI` for every active cell,
on every timestep.

`PPHI` has been a compile-time constant since the `sb 240925` change noted in
the comment at `:2565`. Setting it once at initialisation, or better replacing
it with a parameter, removes this nest and stops `mnnit` loading `pphi` and
`1 - pphi` for every term at `:3534`, `:3555`, `:3561`, `:3570`, `:3578`.

`NLYR`, `NLYRBT`, `NTSOIL` and `NLYREE` are passed into `MNINT2` solely to feed
this dead nest.

### 3.5 Loop-invariant expressions recomputed per cell

`MNOUT:3749-3763` forms `DTUZ * DELTAZ(NCL, NELM)` fifteen times per cell, once
per accumulator. `MNOUT:3790-3798` forms `DXQQ(NELM) * DYQQ(NELM)` five times
per cell, although it is invariant in `NCL` and should be hoisted out of the
cell loop entirely.

`MNTEMP:4832-4856` rebuilds `KFCT` and the whole `AMAT`/`BMAT`/`CMAT`
tridiagonal every timestep, although those depend only on `KFCT` and `DTUZ`.

`MNINT2:2700-2701` divides by `DELTAZ(NCETOP, NELM)` twice per line.

### 3.6 Two `MNPLANT` initialisation loops are nested one level too deep

Inside `DO nelm = nlf+1, nel` (`:3994-4029`):

- `:3997-3999` — `claimx(i) = 2.0d0` over `1..npltee`, repeated `nel - nlf`
  times;
- `:4017-4019` — `npltyp(i, 2) = 1` over `1..nel`, giving an **O(nel²)**
  initialisation.

Both are first-pass only, so they do not affect per-timestep cost, but the
quadratic one is a real startup cost on large catchments and is clearly
unintended. (This was already noted in
`review_v4.5.2_vsv4.5.3.md` against the pre-refactor line numbers; it is still
present.)

### 3.7 Repeated `ISBOTC` branch

`IF (isbotc) THEN nbotm = nbotce ELSE nbotm = ncolmb(nelm) END IF` appears
inside the element loop of thirteen routines, at fifteen sites: `mnamm:198`, `mnco2:352`,
`mnedth:600`, `mnemph:681`, `mnemt:757`, `mnenph:844`, `mnent:921`,
`mngam:2148`, `mnlthm:2773`, `mnltn:2935`, `mnman:3313`, `mnnit:3507`,
`mnout:3696` and `:3742` and `:3776`. The compiler will handle the branch; the
duplication is a maintenance cost and is where the range divergence of 1.3 can
drift further.

A related observation: when `ISBOTC` is true, `MNINIT:2408-2413` guarantees
`nbotce >= ncolmb(nelm)` for every land element, so the active range is a strict
subset of `ncolmb..ncetop`. `MNINT2:2539-2546` and `MNPLANT:4033-4035`
nonetheless copy and zero over the full `ncolmb..ncetop` range every timestep,
for cells no consumer will read.

---

## Part 4 — Memory allocation

### 4.1 `MNMAIN` performs about thirty heap allocate/free pairs per timestep

> **Status: ✅ Fixed in `e0aef0b`.** One-time validation and input setup now
> run in `MNINITIALISE`; timestep arrays are held in persistent `MN_WORK`
> storage allocated once by `MNALLOCATE`; and `MNMAIN` performs no allocation.
> First-pass-only arrays are local to `MNINITIALISE` and are released after
> setup rather than on every timestep.

`:3136-3141` allocates, on every call:

- nine integer `NELEE` arrays (`CELEM`, `KD1ELM`, `KD2ELM`, `KHELEM`, `KLELEM`,
  `KMELEM`, `KNELEM`, `KVELEM`, `NAELEM`);
- `DUMMY2(NLYREE, NELEE)`, `IDUM(NELEE)`, `IDUM1X(NELEE+3)`, `DUMMY(NELEE)`,
  `LDUM(NELEE)`;
- fifteen double `NELEE` arrays (`CDPTHB` … `NTOT`).

Of these, **fifteen are used only on pass 1**: the nine category arrays,
`DUMMY2`, `IDUM1X`, `CTOTTP`, `DAMHLF`, `DCHLF`, `NAMTOP`. They should live
inside the `IF (PASS == 1)` block, or be module-level and allocated once.

This is the same pattern that `impact_v4.5.2_vsv4.5.3.md` identifies as the
principal `OCSIM` slowdown mechanism — per-timestep mapping and first-touch page
faulting — at smaller scale but higher count.

See also 1.7: the per-call reallocation is what silently voided `MNRED2`'s
documented persistence.

### 4.2 `MNERR3` declares a large automatic array that shadows the module workspace

`:1786` declares `DOUBLE PRECISION :: DUMMY4(NCETOP, NEL)` as a **local**
automatic array, shadowing the module allocatable `dummy4` (`:75`) that has the
identical name and identical shape and exists for exactly this purpose.

The consequences:

- a stack allocation of `NCETOP × NEL × 8` bytes on every timestep — precisely
  what the 2026-05 "moved large work arrays to heap storage" change set out to
  remove, and a stack-overflow risk on large grids;
- the module's own `dummy4` is consequently used by `MNERR1` alone
  (`:1345`, `:1350`, `:1355-1356`), on pass 1 only, yet stays resident for the
  whole run. `dummy6` (`:76`) is likewise used only by `MNINIT:2341` on pass 1.

Both module workspace arrays should be local to the routines that use them, or
dropped once 2.2 removes the copies.

### 4.3 An `NELEE` array used to carry one scalar per element between two loops

`MNINT2:2550` stores `DUMMY(NELM) = PNETTO(NELM) * 1.0D3`; the only reader is
`:2700-2701`, two loops later. Folding the multiplication into `depo_loop` as a
scalar removes the array from the interface.

### 4.4 Six per-timestep pool copies

`MNINT2:2540-2545` copies `cman1→cman`, `nman1→nman`, `clit1→clit`,
`chum1→chum`, `nlit1→nlit`, `namm1→namm`, cell by cell, strided.

Under a `(ncetop, nel)` layout these become contiguous slice assignments. Beyond
that, all six "`1`" arrays are fully rewritten over the active range each
timestep, so they are candidates for a buffer swap rather than a copy — but
**only after 1.3 is resolved**, because `mnamm` writes `namm1` over the narrower
range and a swap would then expose two-step-old values in the gap cells.

This is the same class of change as the recent `13d3d71` ("reduced OC-state
copying") and `12d3945` ("turned QCO multi-pass copy into one pass").

---

## Part 5 — Vestigial interfaces

Not performance-relevant, but they obscure the analysis above.

**Unused `llee` dummy argument** in eleven routines: `mnamm:161`, `mnco2:323`,
`mnedth:576`, `mnemph:661`, `mnemt:736`, `mnenph:824`, `mnent:900`,
`mngam:2124`, `mnlthm:2747`, `mnltn:2906`, `mnman:3283`, `mnnit:3461`. It used
to dimension the arrays that are now module allocatables. `nelee` survives in
those routines only to dimension `ncolmb`/`nlyr`.

**Other dead declarations:** `mnltn` takes `fh` and never uses it (noted at
`:2898`); `MNERR3` declares `THIRTY_ARR` (`:1793`) unused; `MNERR3` maintains
`UZPREV` (`:1791`, `:1816`, `:1821`) whose only reader is commented out at
`:1820`; `MNINT2:2563` computes `JSOIL` and discards it.

**Misleading commented-out declarations** at `:332-343`, `:673`, `:744`,
`:3474-3495`. These document the *old* `(nelee, llee)` shapes — the exact
transpose of the current layout — and will actively mislead anyone attempting
Part 2.

**Minor style inconsistencies:** integer literals mixed into double expressions
at `mnnit:3534` and `:3570` (`1 - pphi`) where `:3578` uses `1.0d0`, and at
`mnco2:376-378` (`(1-fe)*(1-fh)`); `MNCONT:524-526` writes a scalar loop where
`TA(1:NV) = 10.0D0` would do.

---

## Recommended order of work

| Priority | Change | Expected benefit | Numerical risk |
|---|---|---|---|
| P0 | Fix the `NLYR <= NLYREE` off-by-one (1.1) and the two OOB writes it enables (1.2) | Correctness; unblocks bounds-checked validation of everything below | None — pure defect fix |
| Done | Move `MNOUT`'s totals loop inside its print guard (`39a8e8b`, 3.1) | Removes a whole-domain sweep plus a `pow` per cell from ~23 of every 24 model-hours | **None — the values were discarded** |
| P1 | Remove the unused `PPHI` argument to `MNOUT` (1.5) | Closes an invalid argument association | None |
| P1 | Decide the fate of the seven write-only accumulators and the five module arrays behind them (3.2) | High; removes 12 full-size arrays and their per-cell arithmetic | **Modelling decision** — see the `@warning` at `:3618` |
| P1 | Collapse `mnenph` into `mnemph`; fix the `0.6` literals (1.4) | Removes 2 `log10` per cell per timestep | Bitwise change in the wettest band only, from fixing `0.6` → `0.6d0` |
| P1 | Hoist iteration-loop stores into scalars in `mnamm`/`mnlthm`/`mnltn`/`mnman` (3.3) | High, and compounds with P3 | None by construction |
| Done | Move setup and timestep workspace allocation out of `MNMAIN` (`e0aef0b`, 4.1) | Removes the per-timestep allocate/free pairs | None |
| P2 | Remove the `DUMMY4` shadow in `MNERR3` (4.2) | Removes one large stack array per timestep | None |
| P2 | Drop the identity copies in `MNERR3`/`MNERR1` (2.2); make `PPHI` a parameter (3.4); hoist the invariants in `MNOUT` (3.5) | Moderate, cheap | None |
| P2 | Unify the active-cell range across the routine chain (1.3) | Correctness; prerequisite for P3 and 4.4 | Low, but must be verified against a run with non-trivial `nlyrbt(:,1)` |
| P3 | Transpose the module arrays to `(ncetop, nel)` (2.1) | Largest payoff; makes the whole module unit-stride | Reordering only — no change to evaluation order within a cell |
| P4 | Fix the two quadratic/repeated init loops in `MNPLANT` (3.6); tidy the vestigial interfaces (Part 5) | Startup cost; readability | None |

For P0–P2 the appropriate acceptance test is **bitwise-identical output** with
an unchanged sequence of accepted timesteps. P3 is a reordering of storage, not
of arithmetic, so it should also be bitwise identical; if it is not, that
indicates an aliasing or range assumption that 1.3 did not fully resolve.

The P1 accumulator decision and the `0.6d0` correction are the only items in the
list that can legitimately change results.
