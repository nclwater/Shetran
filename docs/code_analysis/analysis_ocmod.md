# Logical performance assessment: `OCmod`

## Scope and method

This is a **logical, source-only** assessment. No profile was taken and no
timings were measured. Every claim below is derived from reading
`src/overland_channel/OCmod.f90`, the routines it calls in `src/overland_channel/OCmod2.f90`,
`src/overland_channel/OCQDQMOD.F90` and `src/util/utilsmod.f90`, the array
declarations in `src/core/sglobal.f90`, `src/core/state/AL_C.F90` and
`src/core/state/AL_G.F90`, the compiler flags in `CMakeLists.txt`, and the
shipped example datasets. Where a claim depends on compiler behaviour rather
than on the source alone, that is stated explicitly.

The routines that carry simulation-time cost are:

| Routine | Lines | Role |
|---|---|---|
| `OCSIM` | `src/overland_channel/OCmod.f90:2206-2431` | The whole OC timestep: boundary update, flow/derivative evaluation, block-tridiagonal row solve, state advance, flow correction, channel area update |
| `OCABC` | `src/overland_channel/OCmod.f90:444-588` | Assembles one element's row of the implicit matrix; called once per element per timestep |
| `OCXS` | `src/overland_channel/OCmod.f90:2546-2623` | Initialisation only, but builds the largest array in the model |
| `OCIND`, `LINKNO` | `:1362-1457`, `:2638-2673` | Initialisation only; quadratic in the link count |

`OCSIM` is called exactly once per model timestep from `run_sim.f90:295`. There
is **no outer iteration** — the OC scheme performs a single linearised solve per
step using derivatives evaluated at time level *n*. This makes the cost analysis
much simpler than `VSmod`'s: the multiplier on everything below is the timestep
count alone, and there is no iteration histogram to measure first.

The per-timestep zeroing of the whole solver workspace was already removed in
`e5b53a0`; this document deliberately starts from that state and does not
re-report it.

## Conclusion up front

Two findings dominate, and they are of different kinds.

1. **`XSTAB` is between 0.2 and 2.4 GiB on every shipped example, and a third of
   it is a stored linear ramp.** `OCXS` builds `NXSCEE = 100000` lookup rows per
   channel link. For `examples/38014-100m-SurfaceErrors` (1086 links) that is
   **2.43 GiB** and **109 million `CONVEYAN` calls at initialisation**. This is
   almost certainly the largest single memory object in the model, and it is
   consulted once per face per timestep by `OCQDQ`, so it also guarantees a cold
   cache/TLB access on every conveyance lookup. (Finding **M1**.)

2. **The row solve forms an explicit dense inverse and multiplies dense
   matrices whose operands are structurally almost empty.** `AA` and `CC` carry
   roughly one non-zero per column (each element has at most one neighbour in the
   row above and one below), yet `JEMATMUL_MM` treats them as dense. Two of the
   three cubic terms per row collapse to quadratic if that sparsity is used, and
   the explicit inverse can be replaced by a factor-and-solve. Together these are
   worth roughly **2.5× fewer flops**, before any consideration of replacing the
   hand-rolled kernels with BLAS. (Findings **S2**, **S3**.)

The **scaling** statement matters as much as the absolute one: the row solve is
`O(NY · NCR³)` in time and `O(NY · NCR²)` in memory, where `NCR` is the row
width. On the shipped examples `NCR` is small enough that this is tolerable; on
a 300×300 catchment it is not, and no amount of constant-factor work will fix
it. That is a design property of block-elimination on a 2-D grid, not a bug —
but it should be a conscious decision rather than an inherited one.

## 1. The cost structure

### 1.1 Call structure

```text
per timestep (run_sim.f90:295)
  OCSIM                                               OCmod.f90:2206
    OCEXT       boundary series                       :2235
    OCQDQ       flows + derivatives, all faces        :2238   [OCQDQMOD]
    row_loop over NROWF..NROWL                        :2243
      OCABC   once per element in the row             :2271
      JEMATMUL_MM  CC.EE                              :2284   O(NCR^3)
      JEMATMUL_VM  CC.GG                              :2286   O(NCR^2)
      INVERTMAT    explicit inverse of TM2            :2290   O(NCR^3)
      JEMATMUL_MM  TM2.AA -> EE                       :2301   O(NCR^3)
      JEMATMUL_VM  TM2.TV2 -> GG                      :2306   O(NCR^2)
    downward sweep                                    :2317-2324
    state advance, all elements x 4 faces             :2328-2357
    OCFIX     up to NPASS=100 sweeps                  :2368   [OCmod2]
    QOC copy + sign flip                              :2371-2380
    link_loop  channel area                           :2381-2405
    blow-up check                                     :2412-2422
```

There is no inner iteration anywhere in `OCSIM` itself. `OCFIX` is the only
iterative part, and it is in `OCmod2`.

### 1.2 Calibration against the shipped datasets

All five figures below were read from the example inputs and their
`output_should` print files, not assumed:

| Dataset | `NX` | `NY` | Elements | Links | `XSTAB` |
|---|---:|---:|---:|---:|---:|
| `Aire_at_Kildwick_Bridge-simple` | 20 | 29 | 356 | 72 | 165 MiB |
| `Cobres` | — | — | 308 | 132 | 302 MiB |
| `dunsop` | 61 | 69 | 3292 | 826 | **1.85 GiB** |
| `38014-100m-SurfaceErrors` | 94 | 70 | 3542 | 1086 | **2.43 GiB** |
| `foston100m` | 122 | 152 | 6290 | 451 | 1.01 GiB |

`XSTAB` is
`(3, NXSCEE, total_no_links)` doubles allocated in `OCmod2:183`, i.e.
**2.4 MB per link**.

Two things follow immediately:

- On every one of these datasets, `XSTAB` is an order of magnitude larger than
  every other OC array combined, and larger than the rest of the model's state.
- The catchments are **link-dominated**. `dunsop` has 826 links and 3292
  elements; if banks are enabled that is 826 links + 1652 banks + ~814 grid
  elements. Row widths in `OCIND` are therefore driven by the channel network,
  not the grid.

Average row occupancy is 3292/69 ≈ 48 for `dunsop` and 6290/152 ≈ 41 for
`foston100m`. The widest rows will be several times that; the row solve cost is
`Σ_rows NCR³`, so it is dominated by the widest rows and the average is not a
good proxy. **Instrumenting `OCIND` to print the row-width histogram is the
single cheapest measurement available** and it calibrates findings S1, S2 and S3
at once. `OCIND` already computes the maximum (`MAX_SOLVER_ROW_WIDTH`,
`:1450`) but does not report it.

### 1.3 Build flags

`CMakeLists.txt:87-99,826-843`: the default `Release` build is `-O2
-fno-fast-math` for GNU, with no `-march`. `ReleaseNative` (`-O3 -march=native
-fno-fast-math`) exists but is not the default. IPO/LTO is required and enabled
for both. Consequences that matter below:

- **Baseline x86-64 codegen**: SSE2, two doubles per vector, no FMA. The
  hand-rolled `JEMATMUL_MM` / `LUDCMP` kernels are therefore running at a small
  fraction of peak even before their access patterns are considered.
- **Array temporaries are invisible.** `-Warray-temporaries` is not enabled in
  any configuration, so the copy-in/copy-out described in **S4** is silent.

## 2. `OCSIM` findings

### S1 — The row solve is `O(NY · NCR³)`; this is the scaling wall

**P0 for information, P3 for action.**

Per row, the current work is:

| Operation | Line | Flops |
|---|---|---|
| `LUDCMP` inside `INVERTMAT` | `:2290` | ≈ `NCR³/3` |
| `NCR` × `LUBKSB` to form the explicit inverse | `utilsmod.f90:884-886` | ≈ `NCR³` |
| `TM1 = CC·EE` | `:2284` | ≈ `NCR²·NPR` |
| `EE = TM2·AA` | `:2301` | ≈ `NCR²·NSV` |

That is roughly `3.3 NCR³` per row, `Σ_rows 3.3 NCR³` per timestep. Memory is
`EE` at `NCR² · NY` doubles.

This is the standard cost of band elimination on a 2-D stencil, and it is not
in itself wrong. But it should be recorded plainly: doubling the grid resolution
multiplies OC solver time by roughly **16** and OC solver memory by roughly
**8**. A catchment at 300×300 with row widths of ~350 would need on the order of
`3.3 × 350³ × 300 ≈ 4×10^10` flops per timestep and several GB for `EE`.

Structurally better options exist — nested dissection or a sparse direct solver
(`MUMPS`, `UMFPACK`, `PARDISO`) at `O(N^1.5)`, or an iterative solver at
`O(N)` — but any of them is a substantial change with a new dependency, and
none should be attempted before S2 and S3, which are free.

### S2 — `AA` and `CC` are near-empty but multiplied densely

**P1. Bitwise identity depends on summation order — see below.**

Read `OCABC`'s writes into `AA` and `CC` (`:567-568`, `:581-582`):

```fortran
IF (JROW > IROW) AA(JND) = AA(JND) + DQI
IF (JROW < IROW) CC(JND) = CC(JND) + DQI
```

Element `IELZ` has four faces. At most one of them reaches the row above and at
most one the row below (plus the `ICMRF2` confluence expansion, which adds up to
three more but only at junctions). So **column `IND` of `AA` and of `CC` has on
the order of one non-zero entry**, out of `NSV` and `NPR` respectively. `BB` is
similarly banded: same-row neighbours are adjacent or near-adjacent in the
`OCIND` ordering.

Now look at what is done with them.

**`TM1 = CC·EE` at `:2284`.** In `JEMATMUL_MM`'s indexing
(`utilsmod.f90:594-601`) this evaluates

```fortran
a(i,j) = SUM over k of cc(k,j) * ee(i,k)
```

For each output column `j`, only the ~1 value of `k` where `cc(k,j) /= 0`
contributes. Iterating over the non-zeros of `cc` instead of over all `k`
reduces this term from `NCR²·NPR` to `O(NCR · nnz(CC))` ≈ `O(NCR²)` — a
**factor of `NCR`**, i.e. roughly 50–150× on these datasets.

**`EE = TM2·AA` at `:2301`.** This evaluates

```fortran
a(i,j) = SUM over k of aa(i,k) * tm2(k,j)
```

Row `i` of `AA` has few non-zeros, so each output row is a short linear
combination of rows of `TM2`. Again `O(NCR · nnz(AA))` instead of `NCR²·NSV`.

That removes two of the three cubic terms, leaving only the inverse. The
sparsity is structural — it follows from the four-face topology, not from the
data — so the saving is not data-dependent.

**Numerical note.** Skipping zero terms in a summation is not bitwise identical
to including them: `x + 0.0` differs from `x` when `x` is `-0.0`, and dropping a
`0.0 * NaN` term changes NaN propagation. In exact-finite arithmetic with a
preserved accumulation order over the surviving terms, results are otherwise
identical. This should be validated as bitwise-identical-or-signed-zero, the
same category as `VSmod`'s I1.

### S3 — An explicit matrix inverse is formed where a factor-and-solve would do

**P1. Changes results in the last bits.**

`:2290` calls `INVERTMAT`, which (`utilsmod.f90:870-890`) builds an `NCR×NCR`
identity, runs `LUDCMP`, then runs `LUBKSB` once per column, then copies the
result back. `TM2` is subsequently used only in two products: `TM2·AA` (`:2301`)
and `TM2·TV2` (`:2306`).

Forming `M⁻¹` explicitly and then multiplying is the classic redundancy: keep
the LU factors and back-substitute directly against the `NSV` columns of `AA`
and against `TV2`. Counting cubic terms:

- **Current:** `NCR³/3` (factorise) + `NCR³` (invert) + `NCR³` (multiply by
  `AA`) ≈ `2.33 NCR³`.
- **Factor-and-solve:** `NCR³/3` (factorise) + `NCR²·NSV` (solve) ≈
  `1.33 NCR³`.

That is **1.75× on this term alone**, and it composes with S2 — with both
applied, per-row cost drops from ≈ `3.33 NCR³` to ≈ `1.33 NCR³`, about
**2.5× fewer flops overall**.

Three secondary costs disappear with it:

- `INVERTMAT` declares `DOUBLE PRECISION, DIMENSION(n,n) :: y` as an automatic
  array (`utilsmod.f90:852`), sized `NCR²` — a stack allocation per row per
  timestep that is zeroed (`:1030`) and copied back (`:1047`). At `NCR = 150`
  that is 180 kB per row. It is `3 NCR²` of memory traffic that factor-and-solve
  does not need at all.
- `LUDCMP` (`:1173`) is an unblocked Numerical Recipes Crout factorisation with
  `MAXVAL(ABS(a(i,:)))` row scans — row-major access on a column-major array.
  `LUBKSB`'s `DOT_PRODUCT(a(i, ii:i-1), b(ii:i-1))` is likewise a strided read.
- `JEMATMUL_MM`'s inner loop reads `c(i,k)` with `k` innermost — **stride `n3`**
  through the left operand.

**A larger, separate option:** replacing `INVERTMAT`/`LUDCMP`/`LUBKSB`/
`JEMATMUL_MM` with LAPACK `DGETRF`/`DGETRS` and BLAS `DGEMM` would keep the
same flop count as factor-and-solve but run it blocked and vectorised. On a
`-O2` baseline-x86-64 build against a tuned BLAS this is plausibly another
5–20×. It adds a build dependency, so it is a policy decision rather than a
code cleanup — but it is the highest-leverage single change available for large
catchments, and it subsumes the access-pattern problems above rather than
requiring them to be fixed by hand.

### S4 — Every solver call packs and unpacks an array temporary

**P2. Bitwise identical to fix.**

`JEMATMUL_MM`, `JEMATMUL_VM` and `INVERTMAT` all take **explicit-shape** dummy
arguments. The actual arguments at `:2284-2306` are non-contiguous array
sections of arrays whose leading dimension is `MAX_SOLVER_ROW_WIDTH`, the
widest row; every narrower row passes non-contiguous sections:

| Line | Actual argument | Contiguous? |
|---|---|---|
| `:2284` | `cc(1:npr, 1:ncr)` | no — leading dim `MAX_SOLVER_ROW_WIDTH` |
| `:2284` | `ee(1:ncr, 1:npr, irow)` | no |
| `:2285` | `bb(1:ncr, 1:ncr)` | no |
| `:2290` | `TM2(1:ncr, 1:ncr)` | no — and `INTENT(INOUT)`, so pack **and** unpack |
| `:2301` | `tm2(1:ncr,1:ncr)`, `aa(1:nsv,1:ncr)` | no |
| `:2301` | `ee(1:nsv, 1:ncr, irsv)` as assignment target | no |

gfortran must materialise a packed copy for each, plus a temporary for each
array-valued function result before it is copied into its target section. That
is on the order of **six to eight `NCR²` copies per row per timestep** — second
order against `NCR³`, but for `foston100m` it is several MB of pure memory
traffic per row.

**Fix.** Either declare the dummies assumed-shape, or pass whole arrays with an
explicit leading-dimension argument in the BLAS style (`lda`). The latter is
also exactly what is needed to call BLAS directly, so it is the natural
preparation for S3.

Build once with `-Warray-temporaries` to confirm; the current flag set never
reports these.

## 3. `OCABC` findings

`OCABC` is called once per element per timestep (`:2271`), so its multiplier is
`total_no_elements`, not `total_no_elements × iterations`.

### A1 — Row-length zeroing is quadratic in the row width

**P1. Inherent to the dense representation; fixed by S2's sparse handling.**

`:482-496` zero `AA(1:NSV)`, `BB(1:NCR)` and `CC(1:NPR)` on every call. Summed
over the `NCR` calls that make up one row, that is
`NCR × (NSV + NCR + NPR) ≈ 3 NCR²` stores per row, `Σ_rows 3 NCR²` per timestep
— to produce a matrix with `O(NCR)` non-zeros.

The comment at `:481` (*"Performance Rollback: Explicit DO loops bypass
dope-vector overhead for micro-arrays"*) optimises the constant factor of the
wrong operation: these are not micro-arrays, they are full row vectors, and the
explicit loop is if anything harder for the compiler to turn into a `memset`
than `AA(1:NSV) = ZERO` would be.

If S2 is adopted and `AA`/`CC` are held in a sparse form (an index and a value
per element-face), this zeroing disappears entirely rather than being optimised.

### A2 — `AR` can be read uninitialised

**P1. Correctness, with a performance edge.**

```fortran
IF (TEST) THEN
   search_loop: DO I = 2, N                  ! :516
      HI = XINH(IELZ, I)
      IF (H < HI) THEN
         ...
         AR = CL*(WM + (WI - WM)*((H - HM)/(HI - HM)))
         EXIT search_loop
      END IF
   END DO search_loop
ELSE
   AR = AREAE
END IF
BB(IND) = -AR/DTOC                            ! :531
```

If no `I` satisfies `H < XINH(IELZ,I)`, `AR` is never assigned and `:531` reads
an uninitialised local. The routine's own comment at `:515` states the
requirement — `XINH(IEL,N) >= ZBF-ZG`, which with the `Z < ZBF` guard at `:512`
implies `H < XINH(IELZ,N)` — so today this is held by an **invariant, not by a
code guard**.

Note the asymmetry: `OCSIM`'s `link_loop` computes the same interpolation at
`:2387-2404` and *does* carry a `found_level` flag with an explicit fallback.
`OCABC` does not. One of the two is wrong.

The performance edge is the same one as `VSmod`'s C3: uninitialised stack
doubles are frequently subnormal, and subnormal operands cost 100+ cycles on
x86 without flush-to-zero, which `-fno-fast-math` deliberately withholds. A
sporadic, data-dependent slowdown of that kind is very hard to attribute later.

### A3 — `ICMREF` is read at stride `NELEE` in the face loop

**P2.**

`:548-549` read `ICMREF(IELZ, IFACE+4)` and `ICMREF(IELZ, IFACE+8)`, and
`:561` reads `ICMREF(JEL, 3)`. `ICMREF` is `(NELEE, 12)`, so consecutive
*columns* are **1 MB apart**. One element's four faces touch eight distinct
cache lines spread across 8 MB, plus one more per neighbour.

The same pattern recurs at `:2330`, `:2336`, `:2340`, `:2349` in `OCSIM` and at
`:2661` in `LINKNO`.

This layout is shared with the rest of the model and changing it has a wide
blast radius, so it is recorded rather than recommended. The targeted remedy is
to build the `(element, face)` views once at initialisation and read those in
the hot loops.

## 4. Memory and layout

### M1 — `XSTAB` at `NXSCEE = 100000` rows per link

**P0. Largest single object in the model. Numerical-resolution change, needs validation.**

`OCXS`'s `table_loop` (`:2596-2619`) builds a uniformly spaced conveyance
lookup table with `NXSCEE = 100000` rows for **every** channel link, allocated
`(3, NXSCEE, total_no_links)` at `OCmod2.f90:183`. Per link that is
`3 × 100000 × 8 = 2.4 MB`. From the table in §1.2:

- `38014-100m-SurfaceErrors`: 1086 links → **2.43 GiB**
- `dunsop`: 826 links → **1.85 GiB**
- `foston100m`: 451 links → **1.01 GiB**
- even `Aire_at_Kildwick_Bridge-simple`, at 356 elements, pays **165 MiB**

Three separate costs follow.

**Initialisation time.** `:2614` calls `CONVEYAN` once per table row per link:
`1086 × 99999 ≈ 109 million` calls for `38014`, each with a `**` on a real
exponent. This is a one-off, but it is a large one-off.

**Per-timestep cache and TLB behaviour.** `OCCODE` (`OCmod2.f90:508-545`)
indexes the table in `O(1)`:

```fortran
HFULL = AFROMXSTYPES(1, NXSCEE)
I     = INT((H / HFULL) * DBLE(NXSCEE - 1) + ONE)
```

so the lookup itself is cheap — but it touches 24 bytes at an essentially
random offset in a 2.4 MB per-link table, once per face per timestep from
`OCQDQ`. With gigabytes of table and no locality between successive links, every
conveyance evaluation is a cache miss and very likely a TLB miss.

**A third of the table is a stored linear ramp.** `:2616` writes

```fortran
XSTAB(1, J, ielr) = HJ        ! HJ = STEPH*(J-1), STEPH = XINH(N)/(NXSCEE-1)
```

Row 1 is exactly `(J-1) × STEPH`, reconstructible from two per-link scalars.
Dropping it saves a third of the array — 810 MiB on `38014` — with no
approximation whatsoever, and shrinks the per-lookup footprint from 24 to 16
bytes.

**The resolution itself is the real question.** With a bankfull depth of ~2 m
(as in `dunsop`), `STEPH = 2/99999 ≈ 0.02 mm`. The tabulated function is
piecewise-linear interpolation of `C = STR · A · h^(2/3)`, which is smooth away
from the input cross-section breakpoints; interpolation error scales as `Δh²`.
Reducing `NXSCEE` from `10⁵` to `10³` gives a 2 mm depth resolution, increases
interpolation error by `10⁴` — from something around `10⁻¹¹` relative to
something around `10⁻⁷` relative — and cuts both the memory and the
initialisation cost by **100×**.

That is a numerical change and must be validated as such, not accepted on
timing. But it is very likely the single largest improvement available in this
file, and the two structural parts of it — dropping row 1, and making `NXSCEE`
a runtime parameter rather than a compile-time constant so it can be swept —
are themselves risk-free.

### M3 — `NELEE`-sized module state

**P3. Recorded, not recommended.**

`OCmod` declares roughly 11.7 MB of fixed-size module arrays regardless of
catchment: `NELIND(NELEE)` and `NROWEL(NELEE)` at 1 MB each (`:57`, `:63`),
and `XINH`, `XINW`, `XAREA` at `(NLFEE, NOCTAB)` = 3.2 MB each (`:79-81`).
For `Cobres` — 308 elements, 132 links — the live fraction of that is under 1%.

This is a model-wide convention, not an `OCmod` decision, and changing it here
alone would buy little. It is noted because it interacts with M1: the model's
total resident set is dominated by fixed-capacity arrays whose live fraction is
tiny, which is what makes every one of the hot loops above a
cache-and-TLB problem rather than a flop problem.

### M4 — `XINH`/`XINW` stride in `OCABC`, calibrated

**P3. Small in practice — recorded so it is not over-weighted.**

`XINH(NLFEE, NOCTAB)` means `XINH(IELZ, I)` strides `NLFEE × 8 = 160 kB` as `I`
advances, which looks alarming in `OCABC`'s `search_loop` (`:516-526`).

It is worth checking before acting. In `dunsop` every link has **two**
width/depth pairs, so `N = 2` and the loop body executes once, touching about
five separate cache lines per link per timestep — on the order of 4000 misses
per timestep across the whole catchment. That is real but minor.

Conversely, `OCSIM`'s `link_loop` (`:2381-2405`) iterates `iels` in the
*outer* loop with `I` inner, so `XINH(1:nlinks, I)` is read as several
contiguous streams — the current layout is **good** there.

Transposing to `(NOCTAB, NLFEE)` would help `OCABC` and hurt `OCSIM`. Given the
measured `N = 2`, neither is worth doing. Priority belongs to M1.

## 5. Initialisation-time findings

These do not affect the timestep loop, but they are on the startup path and one
of them is quadratic.

### I1 — `LINKNO` makes `OCIND` and `JEOCBC` quadratic in the link count

**P2.**

`LINKNO` (`:2638-2673`) is a linear search over all links. It is called:

- from `OCIND` inside `DO J = 1, NY / DO I = 1, NX / DO FACE = 3, 4` (`:1400`)
- from `JEOCBC` inside `DO I = 1, NX / DO J = 1, NY / DO K = 0, 1` (`:789`)

giving `2 · NX · NY · total_no_links` iterations each. For `38014`
(94 × 70, 1086 links) that is **14.3 million** iterations per site, and each
iteration reads `ICMREF(L,2)` and `ICMREF(L,3)` — two columns **1 MB apart**
(A3), so it is two cache misses per link visited, not two loads.

**Fix.** Build the inverse map once — `LINKID(NS, I, J)` or a hash from
`(I, J, NS)` to link — during or immediately after `FRIND`, and make `LINKNO`
an `O(1)` lookup. `ICMREF(L,2:3)` and `LINKNS(L)` are exactly the data needed
and are static from `FRIND` onward.

### I2 — `OCXS`'s inner search is fine; the trip count is not

**P3, subsumed by M1.**

The bracketing search at `:2601-2606` correctly carries `I` across iterations of
`table_loop` rather than restarting, so the search is amortised `O(N)` over the
whole table, not `O(N)` per row. That part is well written. The cost is entirely
the `NXSCEE` trip count, which is M1.

### I3 — `OCPRI` is gated on variables that are never assigned

**P1. Correctness with a large potential performance consequence.**

```fortran
OCTIME = OCNOW + OCNEXT
IF ((OCTIME >= TDC) .AND. (OCTIME <= TFC)) CALL OCPRI(OCTIME, ARXL, QOC)   ! :2408-2409
```

The module-level `TDC` and `TFC` (`:72-73`) are shadowed by locals of the same
name in `OCINI` (`:149`), which is where `OCREAD` writes them (`:163`). The
module copies are **never assigned** — the routine's own `@warning` at
`:135-141` documents this.

With gfortran these live in `.bss` and are zero, so the test is
`OCTIME >= 0 .AND. OCTIME <= 0` and `OCPRI` effectively never runs. That is
luck, not design. Under a different compiler, a different storage model, or any
change that gives them non-zero values, `OCPRI` would run **every timestep** —
and `OCPRI` (`:1822-1851`) writes one formatted line per element to `FID_logfile`,
allocates and deallocates `ghrf(total_no_links)` on every call, and calls
`GETHRF` once per element.

For `foston100m` that would be 6290 formatted records per timestep. Formatted
Fortran I/O runs at roughly `10⁵`–`10⁶` records/second, so this would add
something on the order of **10 ms per timestep** and produce a print file of
tens of GB.

This is worth fixing purely as a latent-hazard removal, independent of whether
it is currently active.

## 6. Correctness issues adjacent to performance

### C2 — The back-substitution sweep relies on leaked loop variables

**P2. Fragile rather than wrong.**

`:2313-2314` reads:

```fortran
IROW = NROWL
DD(1:ncr, IROW) = GG(1:ncr, IRSV)
```

using the `NCR` and `IRSV` left over from `row_loop`, with the comment *"use
NCR,IRSV from loop above"*. This is correct only because `NROWL` is by
construction non-empty (`:1440`) and so cannot have taken the `CYCLE` at
`:2254`. Recomputing both from `NROWST` costs two integer loads and removes the
dependence on an invariant that lives 60 lines away in a different routine.

This matters more now than before `e5b53a0`: with the workspace no longer
zeroed each timestep, any row whose `GG` or `DD` entries are not written this
step retains last step's values rather than zeros, so a latent indexing error
would produce plausible-looking wrong answers instead of obvious ones.

### C3 — `NELIND` may be read for elements never placed in a row

**P2.**

`:2328-2331` walks **all** elements and reads `DD(NELIND(iels), ICMREF(iels,3))`.
`NELIND` is `INTENT(OUT)` in `OCIND` and is assigned only for elements inserted
into a row (`:1408`, `:1414`, `:1421`, `:1432`). The routine's documentation
asserts that active grid elements, links and banks partition
`1:total_no_elements`, so every element is placed — again an invariant rather
than a guard.

If it were ever violated, `NELIND(iels)` would be an uninitialised integer used
directly as an array index. Same category as A2, with a worse failure mode.

### C4 — `OCFIX`'s sweep is unconditional over all elements

**P2. Context — the code is in `OCmod2`, the call is here.**

`OCFIX` (`OCmod2.f90:1744-1748`) runs up to `NPASS = 100` passes, each a full
sweep over every element and every face, re-checking elements that were fine on
pass 1. Worst case for `foston100m` is `100 × 6290 × 4 ≈ 2.5` million face
visits per timestep.

Whether this matters depends entirely on the observed pass count, which is not
recorded anywhere. **Counting passes is a two-line change and is the second
cheapest measurement available** after the row-width histogram. If the typical
answer is 1–2 the finding is closed; if it is 10+, a worklist of failing
elements rather than a full re-sweep becomes the obvious remedy.

## 7. Recommended order of work

| Priority | Change | Findings | Expected benefit | Numerical risk |
|---|---|---|---|---|
| **P0** | Instrument the row-width histogram in `OCIND` and the `OCFIX` pass count | §1.2, C4 | None directly — calibrates S1/S2/S3 and closes or opens C4 | None |
| **P1** | Drop `XSTAB` row 1 (the stored linear ramp) | M1 | One third of 0.2–2.4 GiB; per-lookup footprint 24 → 16 bytes | **None — exactly reconstructible** |
| **P1** | Exploit the sparsity of `AA`/`CC` in the two matrix products | S2 | Two of three cubic terms per row become quadratic | Signed zero and NaN propagation only |
| **P1** | Replace the explicit inverse with factor-and-solve | S3 | ~1.75× on the remaining cubic term; removes an `NCR²` per-row stack array | Last-bit reassociation |
| **P1** | Add the guard or assertion for `AR` in `OCABC` | A2 | Correctness; closes a subnormal-stall path | None |
| **P1** | Assign the module `TDC`/`TFC`, or delete the shadowing locals | I3 | Removes a latent every-timestep formatted-I/O path | None (fixes a bug) |
| **P2** | Make the solver dummies assumed-shape or `lda`-style | S4 | Removes 6–8 `NCR²` copies per row; prerequisite for BLAS | **None — bitwise identical** |
| **P2** | `O(1)` `LINKNO` via an inverse map | I1 | Removes ~14M cache-missing iterations from startup | **None — same result** |
| **P2** | Recompute `NCR`/`IRSV` for the back-sweep | C2 | Robustness | None |
| **P3** | Reduce `NXSCEE`, ideally to a runtime parameter | M1 | 100× on `XSTAB` memory and on `OCXS` initialisation | **Numerical resolution — validate** |
| **P3** | Replace the solver kernels with LAPACK/BLAS | S3 | Plausibly 5–20× on the solver; subsumes S4 and the access patterns | Different pivoting — validate |
| **P3** | Reconsider the block-elimination algorithm | S1 | The only route past `O(NY·NCR³)` | **Solver change — validate carefully** |

The P1 block splits into two independent tracks that can proceed in parallel:
**memory** (M1's ramp removal) and **flops** (S2, S3). The memory track
is where the certain wins are; the flops track is where the scaling is.

## 8. Validation

For everything marked "bitwise identical", the acceptance test is
**bitwise-identical output** across the example suite. Each of those changes
either preserves the operation sequence exactly or moves it earlier without
reordering, so any diff at all indicates a bookkeeping error. The exceptions to
record explicitly:

- **S2** may flip an exactly-negative-zero coefficient to positive zero, and
  stops propagating non-finite entries of `AA`/`CC` through the products.
- **S3** changes the arithmetic: solving `M x = a` is not bitwise equal to
  forming `M⁻¹` and multiplying, even with identical pivoting. A documented
  tolerance is required, together with an unchanged sequence of accepted
  timesteps.
- **M1's ramp removal** is exact only if `HJ` is reconstructed as
  `STEPH*(J-1)` — the same expression `OCXS:2598` uses — and not as an
  accumulated sum.
- **M1's `NXSCEE` reduction** is a resolution change and cannot be validated on
  timing. It needs a conveyance-error sweep across the actual cross-sections in
  use, and a hydrograph comparison, at several table sizes.

Build the S2/S3 work under `-fcheck=bounds` (the `Debug` configuration at
`CMakeLists.txt:831`) — but note that **A2 and C3 must be fixed first**, or an
uninitialised read will perturb results non-deterministically without
being caught.

Add `-Warray-temporaries` for one build to enumerate the S4 sites rather than
inferring them.

## 9. What this assessment does not establish

- **No attribution of measured runtime.** Nothing here quantifies what fraction
  of a simulation is spent in `OCSIM`, or how `OCSIM` divides between the row
  solve, `OCQDQ` and `OCFIX`. A profile is still required; these findings
  identify avoidable work, not where the time actually goes.
- **Row widths are unknown.** `NCR` is the cube-law variable in S1/S2/S3 and the
  square-law variable in A1, and it was not measured — only averaged
  (`total_no_elements / NY`). The maximum is what
  matters and it is the first item in the work order for that reason.
- **`OCFIX` pass counts are unknown.** C4 could be negligible or could be the
  largest item in `OCSIM`; the source does not say which.
- **`OCQDQ` was read only for its `XSTAB` access pattern.** It is assessed
  separately in `analysis_ocqdqmod.md`.
- **Cache and TLB behaviour is inferred from declared shapes**, not measured.
  The stride and working-set arguments (A3, M3, M4) follow from the array
  declarations and the `NELEE`/`NLFEE`/`NXSCEE` constants; no miss counts were
  taken.
- **Compiler behaviour is inferred from flags and version.** The claims about
  array temporaries at explicit-shape interfaces and about `.bss` zeroing of `TDC`/`TFC` follow from
  gfortran semantics and `CMakeLists.txt:826-843`. They are checkable by reading
  the generated assembly and have not been checked.
- **Dataset calibration is from the shipped examples only.** The link counts,
  element counts and grid sizes in §1.2 come from five example datasets and
  their `output_should` print files, not from production inputs. `N = 2`
  cross-section points — which is what makes M4 low priority — was verified for
  `dunsop` only.
