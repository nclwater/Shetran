# Logical performance assessment: `OCmod`

## Scope and method

This is a **logical, source-only** assessment. No profile was taken and no
timings were measured. Every claim below is derived from reading
`src/modules/OCmod.f90`, the routines it calls in `src/modules/OCmod2.f90`,
`src/modules/OCQDQMOD.F90` and `src/modules/utilsmod.f90`, the array
declarations in `src/parameters/sglobal.f90`, `src/parameters/AL_C.F90` and
`src/parameters/AL_G.F90`, the compiler flags in `CMakeLists.txt`, and the
shipped example datasets. Where a claim depends on compiler behaviour rather
than on the source alone, that is stated explicitly.

The routines that carry simulation-time cost are:

| Routine | Lines | Role |
|---|---|---|
| `OCSIM` | `src/modules/OCmod.f90:2012-2243` | The whole OC timestep: boundary update, flow/derivative evaluation, block-tridiagonal row solve, state advance, flow correction, channel area update |
| `OCABC` | `src/modules/OCmod.f90:301-445` | Assembles one element's row of the implicit matrix; called once per element per timestep |
| `OCXS` | `src/modules/OCmod.f90:2358-2435` | Initialisation only, but builds the largest array in the model |
| `OCIND`, `LINKNO` | `:1210-1301`, `:2450-2485` | Initialisation only; quadratic in the link count |

`OCSIM` is called exactly once per model timestep from `run_sim.f90:285`. There
is **no outer iteration** — the OC scheme performs a single linearised solve per
step using derivatives evaluated at time level *n*. This makes the cost analysis
much simpler than `VSmod`'s: the multiplier on everything below is the timestep
count alone, and there is no iteration histogram to measure first.

The per-timestep zeroing of the whole solver workspace was already removed in
`e5b53a0`; this document deliberately starts from that state and does not
re-report it.

### Resolution status on `fix_v4.5.3_runtime`

The detailed findings below retain the source evidence from the assessed
baseline. The following findings have since been resolved or addressed by
commits after `dba3bad`:

| Finding | Status | Resolution |
|---|---|---|
| **S5** | ✅ Fixed | `2a24a7c` passes `ICMREF` and `ICMRF2` to `OCFIX` in their native layouts and removes `ijedum`/`ijedum2`. |
| **S6** | ✅ Fixed | `13d3d71` makes `OCFIX` update `HRFZZ`/`QSAZZ` in place and removes the four staging buffers. |
| **S7** | ✅ Fixed | `12d3945` writes the four `QOC` columns directly from `QSAZZ`, with sign changes applied during the copy. |
| **S8** | 🟡 Addressed in optimized builds | `ff37c15` enables interprocedural optimisation for `Release`; `23339a8` extends it to `ReleaseNative`, allowing the compiler to inline the trivial cross-module accessors. The calls remain in source and inlining is compiler-controlled. |
| **M2** | ✅ Fixed | `728c1a6` moves `OCIND` before workspace allocation and sizes the workspace to the active maximum row width; `c4604bd` extracts the row-width calculation and `09ea568` adds focused tests. |
| **C1** | ✅ Fixed | The same exact-width allocation removes the mismatched `NXOCEE`/`NX*4` guard and the associated undersized-allocation path. |

All other finding statuses remain unchanged. References below to the old code
and its costs are retained to explain what was fixed.

## Conclusion up front

At the assessed baseline, three findings dominated, and they were of three
different kinds.

1. **`XSTAB` is between 0.2 and 2.4 GiB on every shipped example, and a third of
   it is a stored linear ramp.** `OCXS` builds `NXSCEE = 100000` lookup rows per
   channel link. For `examples/38014-100m-SurfaceErrors` (1086 links) that is
   **2.43 GiB** and **109 million `CONVEYAN` calls at initialisation**. This is
   almost certainly the largest single memory object in the model, and it is
   consulted once per face per timestep by `OCQDQ`, so it also guarantees a cold
   cache/TLB access on every conveyance lookup. (Finding **M1**.)

2. **Resolved since the assessment: `OCSIM` restaged 16 MB of unchanging
   topology on every timestep.**
   Lines `:2157-2171` copy eight full columns of `ICMREF(NELEE,12)` and six of
   `ICMRF2(NLFEE,6)` into `ijedum`/`ijedum2` before calling `OCFIX`. `ICMREF`
   and `ICMRF2` are written only by `FRmod:FRIND` during initialisation. The
   cost is **independent of catchment size** — it is `NELEE = 250000` regardless
   of whether the catchment has 300 elements or 30000. (Finding **S5**.)

3. **The row solve forms an explicit dense inverse and multiplies dense
   matrices whose operands are structurally almost empty.** `AA` and `CC` carry
   roughly one non-zero per column (each element has at most one neighbour in the
   row above and one below), yet `JEMATMUL_MM` treats them as dense. Two of the
   three cubic terms per row collapse to quadratic if that sparsity is used, and
   the explicit inverse can be replaced by a factor-and-solve. Together these are
   worth roughly **2.5× fewer flops**, before any consideration of replacing the
   hand-rolled kernels with BLAS. (Findings **S2**, **S3**.)

Beyond that, `EE` was allocated at `(NX*4, NX*4, NY)` when `OCIND` already
computed the true maximum row width — 276 MiB instead of roughly 9 MiB on
`examples/foston100m`. This has since been resolved (**M2**).

The **scaling** statement matters as much as the absolute one: the row solve is
`O(NY · NCR³)` in time and `O(NY · NCR²)` in memory, where `NCR` is the row
width. On the shipped examples `NCR` is small enough that this is tolerable; on
a 300×300 catchment it is not, and no amount of constant-factor work will fix
it. That is a design property of block-elimination on a 2-D grid, not a bug —
but it should be a conscious decision rather than an inherited one.

## 1. The cost structure

### 1.1 Call structure

```text
per timestep (run_sim.f90:285)
  OCSIM                                               OCmod.f90:2012
    OCEXT       boundary series                       :2031
    OCQDQ       flows + derivatives, all faces        :2034   [OCQDQMOD]
    row_loop over NROWF..NROWL                        :2039
      OCABC   once per element in the row             :2067
      JEMATMUL_MM  CC.EE                              :2080   O(NCR^3)
      JEMATMUL_VM  CC.GG                              :2082   O(NCR^2)
      INVERTMAT    explicit inverse of TM2            :2086   O(NCR^3)
      JEMATMUL_MM  TM2.AA -> EE                       :2097   O(NCR^3)
      JEMATMUL_VM  TM2.TV2 -> GG                      :2102   O(NCR^2)
    downward sweep                                    :2113-2120
    state advance, all elements x 4 faces             :2124-2153
    ijedum / ijedum2 staging [fixed: S5]             :2157-2171
    inhrf / inqsa staging [fixed: S6]                :2174-2179
    OCFIX     up to NPASS=100 sweeps                  :2181   [OCmod2]
    write-back [fixed: S6]                            :2183-2188
    QOC copy + sign flip [fixed: S7]                  :2191-2192
    link_loop  channel area                           :2195-2219
    blow-up check                                     :2226-2236
```

There is no inner iteration anywhere in `OCSIM` itself. `OCFIX` is the only
iterative part, and it is in `OCmod2`.

### 1.2 Calibration against the shipped datasets

All five figures below were read from the example inputs and their
`output_should` print files, not assumed:

| Dataset | `NX` | `NY` | Elements | Links | `EE` as allocated | `XSTAB` |
|---|---:|---:|---:|---:|---:|---:|
| `Aire_at_Kildwick_Bridge-simple` | 20 | 29 | 356 | 72 | 1.4 MiB | 165 MiB |
| `Cobres` | — | — | 308 | 132 | — | 302 MiB |
| `dunsop` | 61 | 69 | 3292 | 826 | 31.3 MiB | **1.85 GiB** |
| `38014-100m-SurfaceErrors` | 94 | 70 | 3542 | 1086 | 75.5 MiB | **2.43 GiB** |
| `foston100m` | 122 | 152 | 6290 | 451 | 276 MiB | 1.01 GiB |

`EE` is `(NX*4, NX*4, NY)` doubles (`:213`); `XSTAB` is
`(3, NXSCEE, total_no_links)` doubles allocated in `OCmod2:192`, i.e.
**2.4 MB per link**.

Two things follow immediately:

- On every one of these datasets, `XSTAB` is an order of magnitude larger than
  every other OC array combined, and larger than the rest of the model's state.
- The catchments are **link-dominated**. `dunsop` has 826 links and 3292
  elements; if banks are enabled that is 826 links + 1652 banks + ~814 grid
  elements. Row widths in `OCIND` are therefore driven by the channel network,
  not the grid, which is what makes the `NX*4` allocation bound in **C1** worth
  checking rather than assuming.

Average row occupancy is 3292/69 ≈ 48 for `dunsop` and 6290/152 ≈ 41 for
`foston100m`. The widest rows will be several times that; the row solve cost is
`Σ_rows NCR³`, so it is dominated by the widest rows and the average is not a
good proxy. **Instrumenting `OCIND` to print the row-width histogram is the
single cheapest measurement available** and it calibrates findings S2, S3 and M2
at once.

### 1.3 Build flags

> **Status: ✅ Fixed for optimized builds.** Commits `ff37c15` and `23339a8`
> enable CMake interprocedural optimisation for `Release` and `ReleaseNative`,
> respectively. The no-LTO observations below describe the assessed baseline.

`CMakeLists.txt:673-691`: the default `Release` build is `-O2 -fno-fast-math`
for GNU, with no `-march`, no LTO and no IPO. `ReleaseNative` (`-O3
-march=native -fno-fast-math`) exists but is not the default. Consequences that
matter below:

- **No LTO.** `GETHRF`, `SETHRF`, `GETQSA`, `SETQSA` are module procedures in
  `OCmod2` and gfortran does not carry procedure bodies in `.mod` files, so
  every one of them is a genuine out-of-line call. `OCSIM:2124-2153` alone makes
  roughly `10 × total_no_elements` such calls per timestep.
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
| `LUDCMP` inside `INVERTMAT` | `:2086` | ≈ `NCR³/3` |
| `NCR` × `LUBKSB` to form the explicit inverse | `utilsmod.f90:893-895` | ≈ `NCR³` |
| `TM1 = CC·EE` | `:2080` | ≈ `NCR²·NPR` |
| `EE = TM2·AA` | `:2097` | ≈ `NCR²·NSV` |

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

Read `OCABC`'s writes into `AA` and `CC` (`:424-425`, `:438-439`):

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

**`TM1 = CC·EE` at `:2080`.** In `JEMATMUL_MM`'s indexing
(`utilsmod.f90:596-603`) this evaluates

```fortran
a(i,j) = SUM over k of cc(k,j) * ee(i,k)
```

For each output column `j`, only the ~1 value of `k` where `cc(k,j) /= 0`
contributes. Iterating over the non-zeros of `cc` instead of over all `k`
reduces this term from `NCR²·NPR` to `O(NCR · nnz(CC))` ≈ `O(NCR²)` — a
**factor of `NCR`**, i.e. roughly 50–150× on these datasets.

**`EE = TM2·AA` at `:2097`.** This evaluates

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

`:2086` calls `INVERTMAT`, which (`utilsmod.f90:879-899`) builds an `NCR×NCR`
identity, runs `LUDCMP`, then runs `LUBKSB` once per column, then copies the
result back. `TM2` is subsequently used only in two products: `TM2·AA` (`:2097`)
and `TM2·TV2` (`:2102`).

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
  array (`utilsmod.f90:861`), sized `NCR²` — a stack allocation per row per
  timestep that is zeroed (`:881`) and copied back (`:898`). At `NCR = 150`
  that is 180 kB per row; at the allocated worst case of `NX*4 = 488` it would
  be 1.9 MB. Either way it is `3 NCR²` of memory traffic that factor-and-solve
  does not need at all.
- `LUDCMP` (`:1024`) is an unblocked Numerical Recipes Crout factorisation with
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
arguments. The actual arguments at `:2080-2102` are non-contiguous array
sections of arrays whose leading dimension is `NX*4`:

| Line | Actual argument | Contiguous? |
|---|---|---|
| `:2080` | `cc(1:npr, 1:ncr)` | no — leading dim `NX*4` |
| `:2080` | `ee(1:ncr, 1:npr, irow)` | no |
| `:2081` | `bb(1:ncr, 1:ncr)` | no |
| `:2086` | `TM2(1:ncr, 1:ncr)` | no — and `INTENT(INOUT)`, so pack **and** unpack |
| `:2097` | `tm2(1:ncr,1:ncr)`, `aa(1:nsv,1:ncr)` | no |
| `:2097` | `ee(1:nsv, 1:ncr, irsv)` as assignment target | no |

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

### S5 — 16 MB of static topology restaged every timestep

> **Status: ✅ Fixed in `2a24a7c`.** `OCFIX` now accepts `ICMREF(NELEE,12)`
> and `ICMRF2(NLFEE,6)` directly. The staging loops and the persistent
> `ijedum`/`ijedum2` duplicates were deleted.

**P0. Bitwise identical to fix. The clearest win in the file.**

```fortran
vv = 5
DO LL = 2, 3
   DO kk = 1, 4
      ijedum(:, kk, LL) = icmref(:, vv)      ! :2160
      vv = vv + 1
   END DO
END DO
```

`ICMREF` is declared `INTEGER :: ICMREF(NELEE,12)` (`AL_G.F90:46`) with
`NELEE = 250000`. The slice `icmref(:, vv)` is the **full 250000-element
column**, not `1:total_no_elements`. Eight such columns are copied, giving
**2,000,000 integers = 8 MB written and 8 MB read, every timestep**. `ijedum2`
adds a further 6 × 20000 = 120000 integers.

`ICMREF` and `ICMRF2` are assigned only in `FRmod:FRIND` (`FRmod.f90:642-1159`),
during initialisation. Neither is touched anywhere in the timestep loop. The
staging is therefore **entirely redundant after the first call**.

Two properties make this finding unusually valuable:

- The cost does not scale down with catchment size. For
  `Aire_at_Kildwick_Bridge-simple` — 356 elements, a 1.4 MiB `EE`, a row solve
  of a few hundred thousand flops — this 16 MB of memory traffic plausibly
  **dominates the entire OC timestep**.
- `ijedum` itself is `(nelee, 4, 2:3)` = 8 MB of permanently resident storage
  duplicating 8 MB of `ICMREF`.

**Fix.** Move the two staging loops into `INITIALISE_OCSIM_WORKSPACE`
(`:203-222`) or into `OCINI` after `FRIND` has run. Better still, change
`OCFIX`'s dummy declarations to take `ICMREF` and `ICMRF2` in their native
layout and delete `ijedum`/`ijedum2` entirely; the reshape exists only to give
`OCFIX` a `(element, face, kind)` view of what `ICMREF` already stores as
columns 5:8 and 9:12.

### S6 — The OC state is copied six times per timestep to call `OCFIX`

> **Status: ✅ Fixed in `13d3d71`.** `OCFIX` now corrects the module-owned
> `HRFZZ` and `QSAZZ` arrays in place; the `inhrf`, `inqsa`, `GGGETHRF`, and
> `GGGETQSA` buffers and their accessor-based copy loops were removed.

**P1. Bitwise identical to fix.**

```fortran
DO vv = 1, total_no_elements                 ! :2174
   inhrf(vv) = GETHRF(vv)
   DO face = 1, 4
      inqsa(vv, face) = GETQSA(vv, face)
   END DO
END DO
CALL OCFIX(..., inhrf, GGGETHRF, inqsa, GGGETQSA)   ! :2181
DO vv = 1, total_no_elements                 ! :2183
   CALL SETHRF(vv, GGGETHRF(vv))
   ...
END DO
```

and inside `OCFIX` (`OCmod2.f90:1741-1742`):

```fortran
GGGETHRF = inhrf
GGGETQSA = inqsa
```

So `HRFZZ`/`QSAZZ` → `inhrf`/`inqsa` → `GGGETHRF`/`GGGETQSA` → back to
`HRFZZ`/`QSAZZ`: **three full round trips of the complete OC state**, with the
outer two done element-by-element through non-inlined accessor calls
(`10 × total_no_elements` calls). The comment at `:2173` — *"untidy mess for
debugging of tangent"* — records that this is known.

`HRFZZ` and `QSAZZ` are module variables in `OCmod2`, the same module that
defines `OCFIX`. `OCFIX` could operate on them directly, or take them as
`INTENT(INOUT)`, removing four of the six copies and all the accessor calls.

If the buffered form must be kept for the AD tangent build, guard it with a
preprocessor conditional rather than paying for it in the production build.

### S7 — `QOC` update makes three passes and a heap temporary

> **Status: ✅ Fixed in `12d3945`.** `QOC` is now filled directly from the
> four `QSAZZ` columns, applying the face-1/face-2 sign changes while copying.
> `GETQSA_ALL` and its array-valued temporary were removed.

**P2. Bitwise identical to fix.**

```fortran
QOC(1:total_no_elements, :) = GETQSA_ALL(total_no_elements)   ! :2191
qoc(1:total_no_elements, 1:2) = -qoc(1:total_no_elements, 1:2) ! :2192
```

`GETQSA_ALL` (`OCmod2.f90:155-164`) returns `res(n,4)` **by value**: gfortran
allocates a temporary, `res = qsazz(1:n,:)` fills it with a strided copy (both
`QSAZZ` and `QOC` are `(NELEE,4)`, so `1:n` rows are non-contiguous), the
temporary is copied into `QOC`, and then two of the four columns are read and
rewritten. Four passes over `4n` doubles and one heap allocation, per timestep,
where a single loop writing `QOC(i,f) = ±QSAZZ(i,f)` would do one pass and no
allocation.

The later face-layout recommendation in `analysis_ocmod2.md` M2 and
`analysis_ocqdqmod.md` P1 must transpose `QSAZZ` and `QOC` together. Transposing
only `QSAZZ` would reintroduce a strided handoff here; with both shaped
`(4,NELEE)`, `OCSIM` can copy one contiguous four-face block per element.

### S8 — Accessor calls in the state-advance loop

> **Status: 🟡 Addressed for `Release`/`ReleaseNative` builds by `ff37c15` and
> `23339a8`.** The accessor calls remain as source-level interfaces, but CMake
> now requires interprocedural optimisation in both optimized configurations,
> allowing the compiler to inline these one-line module procedures across
> translation units. Debug builds intentionally remain unoptimised, and the
> source alone does not prove each compiler's inlining decision.

**P2. Bitwise identical to fix.**

`:2124-2153` walks every element and every face calling `GETHRF`, `SETHRF`,
`GETQSA` and `SETQSA`. With no LTO these are real calls; for `foston100m` that
is roughly 63000 calls per timestep from this loop alone, plus a further 63000
from S6's staging loops and `link_loop`.

Each is a one-line array access. Enabling LTO would inline them all and subsume
this finding, S6's call overhead, and part of S7 — **that is a build-flag change
with no source risk and should be tried before any of the source work**.

## 3. `OCABC` findings

`OCABC` is called once per element per timestep (`:2067`), so its multiplier is
`total_no_elements`, not `total_no_elements × iterations`.

### A1 — Row-length zeroing is quadratic in the row width

**P1. Inherent to the dense representation; fixed by S2's sparse handling.**

`:339-353` zero `AA(1:NSV)`, `BB(1:NCR)` and `CC(1:NPR)` on every call. Summed
over the `NCR` calls that make up one row, that is
`NCR × (NSV + NCR + NPR) ≈ 3 NCR²` stores per row, `Σ_rows 3 NCR²` per timestep
— to produce a matrix with `O(NCR)` non-zeros.

The comment at `:338` (*"Performance Rollback: Explicit DO loops bypass
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
   search_loop: DO I = 2, N                  ! :373
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
BB(IND) = -AR/DTOC                            ! :388
```

If no `I` satisfies `H < XINH(IELZ,I)`, `AR` is never assigned and `:388` reads
an uninitialised local. The routine's own comment at `:372` states the
requirement — `XINH(IEL,N) >= ZBF-ZG`, which with the `Z < ZBF` guard at `:369`
implies `H < XINH(IELZ,N)` — so today this is held by an **invariant, not by a
code guard**.

Note the asymmetry: `OCSIM`'s `link_loop` computes the same interpolation at
`:2201-2218` and *does* carry a `found_level` flag with an explicit fallback.
`OCABC` does not. One of the two is wrong.

The performance edge is the same one as `VSmod`'s C3: uninitialised stack
doubles are frequently subnormal, and subnormal operands cost 100+ cycles on
x86 without flush-to-zero, which `-fno-fast-math` deliberately withholds. A
sporadic, data-dependent slowdown of that kind is very hard to attribute later.

### A3 — `ICMREF` is read at stride `NELEE` in the face loop

**P2.**

`:405-406` read `ICMREF(IELZ, IFACE+4)` and `ICMREF(IELZ, IFACE+8)`, and
`:418` reads `ICMREF(JEL, 3)`. `ICMREF` is `(NELEE, 12)`, so consecutive
*columns* are **1 MB apart**. One element's four faces touch eight distinct
cache lines spread across 8 MB, plus one more per neighbour.

The same pattern recurs at `:2126`, `:2132`, `:2136`, `:2145` in `OCSIM` and at
`:2473` in `LINKNO`.

This layout is shared with the rest of the model and changing it has a wide
blast radius, so it is recorded rather than recommended. The targeted remedy is
the same as S5's: build the `(element, face)` views once at initialisation and
read those in the hot loops.

### A4 — `AA` and `CC` dummies over-declare their actual arguments

**P2. Correctness-adjacent, no direct cost.**

`OCABC` declares `AA(NXOCEE)` and `CC(NXOCEE)` (`:324`, `:326`) with
`NXOCEE = 4 * NXEE = 4000` (`sglobal.f90:127`). The actual arguments are
`AA(:, IND)` and `CC(:, IND)` (`:2069-2070`) — columns of arrays allocated with
leading dimension `NX*4`. For every catchment with `NX < 1000`, **the dummy
claims more storage than the actual column has**.

Writes stay in range today because `JND <= NSV <= NX*4` and `JND <= NPR`, but
the declaration defeats `-fcheck=bounds` for exactly the array where S5/C1 make
an overrun plausible: a genuine overrun into `AA(:, IND+1)` would not be
reported. Declare these `AA(NSV)` and `CC(NPR)`, matching `BB(NCR)` at `:325`,
which is already correct.

## 4. Memory and layout

### M1 — `XSTAB` at `NXSCEE = 100000` rows per link

**P0. Largest single object in the model. Numerical-resolution change, needs validation.**

`OCXS`'s `table_loop` (`:2408-2431`) builds a uniformly spaced conveyance
lookup table with `NXSCEE = 100000` rows for **every** channel link, allocated
`(3, NXSCEE, total_no_links)` at `OCmod2.f90:192`. Per link that is
`3 × 100000 × 8 = 2.4 MB`. From the table in §1.2:

- `38014-100m-SurfaceErrors`: 1086 links → **2.43 GiB**
- `dunsop`: 826 links → **1.85 GiB**
- `foston100m`: 451 links → **1.01 GiB**
- even `Aire_at_Kildwick_Bridge-simple`, at 356 elements, pays **165 MiB**

Three separate costs follow.

**Initialisation time.** `:2426` calls `CONVEYAN` once per table row per link:
`1086 × 99999 ≈ 109 million` calls for `38014`, each with a `**` on a real
exponent. This is a one-off, but it is a large one-off.

**Per-timestep cache and TLB behaviour.** `OCCODE` (`OCmod2.f90:522-559`)
indexes the table in `O(1)`:

```fortran
HFULL = AFROMXSTYPES(1, NXSCEE)
I     = INT((H / HFULL) * DBLE(NXSCEE - 1) + ONE)
```

so the lookup itself is cheap — but it touches 24 bytes at an essentially
random offset in a 2.4 MB per-link table, once per face per timestep from
`OCQDQ`. With gigabytes of table and no locality between successive links, every
conveyance evaluation is a cache miss and very likely a TLB miss.

**A third of the table is a stored linear ramp.** `:2428` writes

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

### M2 — `EE` is sized to `NX*4` when the true row width is already known

> **Status: ✅ Fixed in `728c1a6`, with the width calculation extracted in
> `c4604bd` and covered by the focused tests added in `09ea568`.** `OCIND` now runs before
> `INITIALISE_OCSIM_WORKSPACE`; all row-solver arrays use the computed active
> maximum width, and `EE`/`GG` also use tight active-row bounds.

**P1. Bitwise identical to fix.**

`INITIALISE_OCSIM_WORKSPACE` allocates (`:209-215`):

```fortran
ALLOCATE (AA(NX*4, NX*4), DD(NX*4, NY))
ALLOCATE (BB(NX*4, NX*4), GG(NX*4, NY))
ALLOCATE (CC(NX*4, NX*4))
ALLOCATE (EE(NX*4, NX*4, NY))
ALLOCATE (TM1(NX*4, NX*4), TM2(NX*4, NX*4))
```

`NX*4` is a worst-case bound on the row width. `OCIND` **already computes the
true maximum**:

```fortran
NXOC = MAX(NXOC, K + 1 - NROWST(J))   ! :1287
```

but `NXOC` is local and used only for the dimension check at `:1297`. The only
obstacle is call order: `OCINI` calls `INITIALISE_OCSIM_WORKSPACE` at `:161`
and `OCIND` at `:170`.

For `foston100m` the difference is likely to be large. Average row occupancy is
6290/152 ≈ 41 against an allocated 488. The maximum is what governs, and it was
not measured — but the oversizing factor is `(488/NXOC)²`, so even a maximum row
width as high as 90 would mean `EE` is **29× larger than needed: 276 MiB where
roughly 9 MiB would do**. Confirming `NXOC` is the first item in the work order
for exactly this reason.

Beyond the allocation, the oversizing costs at run time: `ee(1:nsv, 1:ncr, irow)`
walks columns `3904` bytes apart, so a `90 × 90` working block is scattered
across a `1.9 MB` slab and every column start is a separate page.

**Fix.** Swap the two calls in `OCINI` — run `OCIND` first, return `NXOC` from
it, and allocate to `NXOC`. Nothing else changes; the arrays are only ever
indexed `1:ncr`, `1:nsv`, `1:npr`.

### M3 — `NELEE`-sized module state

**P3. Recorded, not recommended.**

`OCmod` declares roughly 11.7 MB of fixed-size module arrays regardless of
catchment: `NELIND(NELEE)` and `NROWEL(NELEE)` at 1 MB each (`:49`, `:54`),
and `XINH`, `XINW`, `XAREA` at `(NLFEE, NOCTAB)` = 3.2 MB each (`:70-72`).
For `Cobres` — 308 elements, 132 links — the live fraction of that is under 1%.

This is a model-wide convention, not an `OCmod` decision, and changing it here
alone would buy little. It is noted because it interacts with M1: the model's
total resident set is dominated by fixed-capacity arrays whose live fraction is
tiny, which is what makes every one of the hot loops above a
cache-and-TLB problem rather than a flop problem.

### M4 — `XINH`/`XINW` stride in `OCABC`, calibrated

**P3. Small in practice — recorded so it is not over-weighted.**

`XINH(NLFEE, NOCTAB)` means `XINH(IELZ, I)` strides `NLFEE × 8 = 160 kB` as `I`
advances, which looks alarming in `OCABC`'s `search_loop` (`:373-383`).

It is worth checking before acting. In `dunsop` every link has **two**
width/depth pairs, so `N = 2` and the loop body executes once, touching about
five separate cache lines per link per timestep — on the order of 4000 misses
per timestep across the whole catchment. That is real but minor.

Conversely, `OCSIM`'s `link_loop` (`:2195-2219`) iterates `iels` in the
*outer* loop with `I` inner, so `XINH(1:nlinks, I)` is read as several
contiguous streams — the current layout is **good** there.

Transposing to `(NOCTAB, NLFEE)` would help `OCABC` and hurt `OCSIM`. Given the
measured `N = 2`, neither is worth doing. Priority belongs to M1 and M2.

## 5. Initialisation-time findings

These do not affect the timestep loop, but they are on the startup path and one
of them is quadratic.

### I1 — `LINKNO` makes `OCIND` and `JEOCBC` quadratic in the link count

**P2.**

`LINKNO` (`:2450-2485`) is a linear search over all links. It is called:

- from `OCIND` inside `DO J = 1, NY / DO I = 1, NX / DO FACE = 3, 4` (`:1247`)
- from `JEOCBC` inside `DO I = 1, NX / DO J = 1, NY / DO K = 0, 1` (`:640`)

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

The bracketing search at `:2413-2418` correctly carries `I` across iterations of
`table_loop` rather than restarting, so the search is amortised `O(N)` over the
whole table, not `O(N)` per row. That part is well written. The cost is entirely
the `NXSCEE` trip count, which is M1.

### I3 — `OCPRI` is gated on variables that are never assigned

**P1. Correctness with a large potential performance consequence.**

```fortran
OCTIME = OCNOW + OCNEXT
IF ((OCTIME >= TDC) .AND. (OCTIME <= TFC)) CALL OCPRI(OCTIME, ARXL, QOC)   ! :2222-2223
```

The module-level `TDC` and `TFC` (`:63-64`) are shadowed by locals of the same
name in `OCINI` (`:141`), which is where `OCREAD` writes them (`:152`). The
module copies are **never assigned** — the routine's own `@warning` at
`:127-133` documents this.

With gfortran these live in `.bss` and are zero, so the test is
`OCTIME >= 0 .AND. OCTIME <= 0` and `OCPRI` effectively never runs. That is
luck, not design. Under a different compiler, a different storage model, or any
change that gives them non-zero values, `OCPRI` would run **every timestep** —
and `OCPRI` (`:1647-1671`) writes one formatted line per element to `PPPRI`,
allocates and deallocates `ghrf(total_no_links)` on every call, and calls
`GETHRF` once per element.

For `foston100m` that would be 6290 formatted records per timestep. Formatted
Fortran I/O runs at roughly `10⁵`–`10⁶` records/second, so this would add
something on the order of **10 ms per timestep** and produce a print file of
tens of GB.

This is worth fixing purely as a latent-hazard removal, independent of whether
it is currently active.

## 6. Correctness issues adjacent to performance

### C1 — The row-width dimension check tests the wrong bound

> **Status: ✅ Fixed in `728c1a6` and `c4604bd`.** The workspace is allocated
> only after the active row width has been derived from `NROWST`, and its
> leading dimensions equal that width. The obsolete comparison with `NXOCEE`
> was removed, so there is no smaller `NX*4` allocation for the check to miss.

**P0. Potential heap overflow with no diagnostic.**

`OCIND` checks (`:1297`):

```fortran
IF (NXOC > NXOCEE) CALL ERROR(FFFATAL, 1006, ...)
```

with `NXOCEE = 4000`. But `AA`, `BB`, `CC`, `TM1`, `TM2`, `DD`, `GG` and `EE`
are allocated to `NX*4` (`:209-215`), which for any catchment with `NX < 1000`
is **smaller than `NXOCEE`**.

Is `NXOC > NX*4` reachable? From `OCIND`'s insertion logic (`:1249-1281`), one
grid column contributes at most:

| Contribution | Count |
|---|---|
| west-face link with banks on both sides | 3 |
| south-face link with banks on both sides | 3 |
| the active grid element | 1 |

so up to **7 per column**, against an allocated `4` per column. Writing the
condition out: with banks enabled, the row overflows once more than half the
columns in a row carry links on both their west and south faces.

The §1.2 calibration makes this more than theoretical: these catchments are
link-dominated — `dunsop` has 826 links against roughly 814 grid elements.

Today `NXOC` for these datasets is evidently within bounds, since they run. But
the guard that is supposed to catch it is testing `4000` while the arrays hold
`NX*4` — for `dunsop` that is `244`. A dataset that crossed the line would
silently corrupt the heap, and A4's over-declared dummies mean
`-fcheck=bounds` would not catch it either.

**Fix.** Check against the allocated bound, not `NXOCEE`. This falls out
naturally from M2: run `OCIND` first, size the arrays to the `NXOC` it computes,
and the failure mode disappears rather than being detected.

### C2 — The back-substitution sweep relies on leaked loop variables

**P2. Fragile rather than wrong.**

`:2109-2110` reads:

```fortran
IROW = NROWL
DD(1:ncr, IROW) = GG(1:ncr, IRSV)
```

using the `NCR` and `IRSV` left over from `row_loop`, with the comment *"use
NCR,IRSV from loop above"*. This is correct only because `NROWL` is by
construction non-empty (`:1288`) and so cannot have taken the `CYCLE` at
`:2050`. Recomputing both from `NROWST` costs two integer loads and removes the
dependence on an invariant that lives 60 lines away in a different routine.

This matters more now than before `e5b53a0`: with the workspace no longer
zeroed each timestep, any row whose `GG` or `DD` entries are not written this
step retains last step's values rather than zeros, so a latent indexing error
would produce plausible-looking wrong answers instead of obvious ones.

### C3 — `NELIND` may be read for elements never placed in a row

**P2.**

`:2124-2127` walks **all** elements and reads `DD(NELIND(iels), ICMREF(iels,3))`.
`NELIND` is `INTENT(OUT)` in `OCIND` and is assigned only for elements inserted
into a row (`:1255`, `:1261`, `:1268`, `:1279`). The routine's documentation
asserts that active grid elements, links and banks partition
`1:total_no_elements`, so every element is placed — again an invariant rather
than a guard.

If it were ever violated, `NELIND(iels)` would be an uninitialised integer used
directly as an array index. Same category as A2, with a worse failure mode.

### C4 — `OCFIX`'s sweep is unconditional over all elements

**P2. Context — the code is in `OCmod2`, the call is here.**

`OCFIX` (`OCmod2.f90:1745-1749`) runs up to `NPASS = 100` passes, each a full
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
| **P0** | Instrument the row-width histogram in `OCIND` and the `OCFIX` pass count | §1.2, C4 | None directly — calibrates S1/S2/S3/M2 and closes or opens C4 | None |
| **Done** | Enable LTO for `Release` and `ReleaseNative` (`ff37c15`, `23339a8`) | §1.3, S8 | Allows the `OCmod2` accessors to be inlined | None |
| **Done** | Pass topology to `OCFIX` in its native layout (`2a24a7c`) | S5 | Removes 16 MB of memory traffic per timestep, independent of catchment size | **None — bitwise identical** |
| **Done** | Allocate from the computed active row width (`728c1a6`, `c4604bd`, `09ea568`) | M2, C1 | Compacts the solver working set and removes the undersized-allocation path | **None — bitwise identical** |
| **P1** | Drop `XSTAB` row 1 (the stored linear ramp) | M1 | One third of 0.2–2.4 GiB; per-lookup footprint 24 → 16 bytes | **None — exactly reconstructible** |
| **P1** | Exploit the sparsity of `AA`/`CC` in the two matrix products | S2 | Two of three cubic terms per row become quadratic | Signed zero and NaN propagation only |
| **P1** | Replace the explicit inverse with factor-and-solve | S3 | ~1.75× on the remaining cubic term; removes an `NCR²` per-row stack array | Last-bit reassociation |
| **Done** | Operate `OCFIX` on `HRFZZ`/`QSAZZ` directly (`13d3d71`) | S6 | Removes 4 of 6 full state copies and ~10·n accessor calls | **None — bitwise identical** |
| **P1** | Add the guard or assertion for `AR` in `OCABC` | A2 | Correctness; closes a subnormal-stall path | None |
| **P1** | Assign the module `TDC`/`TFC`, or delete the shadowing locals | I3 | Removes a latent every-timestep formatted-I/O path | None (fixes a bug) |
| **P2** | Make the solver dummies assumed-shape or `lda`-style | S4 | Removes 6–8 `NCR²` copies per row; prerequisite for BLAS | **None — bitwise identical** |
| **Done** | Direct `QOC` update without an array-valued temporary (`12d3945`) | S7 | 4 passes and a heap temporary → 1 pass | **None — bitwise identical** |
| **P2** | `O(1)` `LINKNO` via an inverse map | I1 | Removes ~14M cache-missing iterations from startup | **None — same result** |
| **P2** | Recompute `NCR`/`IRSV` for the back-sweep; declare `AA(NSV)`/`CC(NPR)` | C2, A4 | Robustness; makes `-fcheck=bounds` meaningful | None |
| **P3** | Reduce `NXSCEE`, ideally to a runtime parameter | M1 | 100× on `XSTAB` memory and on `OCXS` initialisation | **Numerical resolution — validate** |
| **P3** | Replace the solver kernels with LAPACK/BLAS | S3 | Plausibly 5–20× on the solver; subsumes S4 and the access patterns | Different pivoting — validate |
| **P3** | Reconsider the block-elimination algorithm | S1 | The only route past `O(NY·NCR³)` | **Solver change — validate carefully** |

Rows marked **Done** record changes already present on
`fix_v4.5.3_runtime`; the priority labels apply only to the remaining work.

The P1 block splits into two independent tracks that can proceed in parallel:
**memory** (M2, M1's ramp removal, S6) and **flops** (S2, S3). The memory track
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
  `STEPH*(J-1)` — the same expression `OCXS:2410` uses — and not as an
  accumulated sum.
- **M1's `NXSCEE` reduction** is a resolution change and cannot be validated on
  timing. It needs a conveyance-error sweep across the actual cross-sections in
  use, and a hydrograph comparison, at several table sizes.

Build the M2 and S2/S3 work under `-fcheck=bounds` (the `Debug` configuration at
`CMakeLists.txt:679`) — but note that **A4 must be fixed first**, or the
bounds check on `AA` and `CC` is inert, and **A2 and C3 must be fixed first**,
or an uninitialised read will perturb results non-deterministically without
being caught.

Add `-Warray-temporaries` for one build to enumerate the S4 sites rather than
inferring them.

## 9. What this assessment does not establish

- **No attribution of measured runtime.** Nothing here quantifies what fraction
  of a simulation is spent in `OCSIM`, or how `OCSIM` divides between the row
  solve, `OCQDQ` and `OCFIX`. A profile is still required; these findings
  identify avoidable work, not where the time actually goes.
- **Row widths are unknown.** `NCR` is the cube-law variable in S1/S2/S3 and the
  square-law variable in M2 and A1, and it was not measured — only bounded
  (`NCR ≤ NX*4`) and averaged (`total_no_elements / NY`). The maximum is what
  matters and it is the first item in the work order for that reason.
- **`OCFIX` pass counts are unknown.** C4 could be negligible or could be the
  largest item in `OCSIM`; the source does not say which.
- **`OCQDQ` was read only for its `XSTAB` access pattern.** It is called once
  per timestep over every face and may well carry more cost than the row solve
  on small catchments. It deserves its own assessment.
- **Cache and TLB behaviour is inferred from declared shapes**, not measured.
  The stride and working-set arguments (A3, M2, M3, M4) follow from the array
  declarations and the `NELEE`/`NLFEE`/`NXSCEE` constants; no miss counts were
  taken.
- **Compiler behaviour is inferred from flags and version.** The claims about
  array temporaries at explicit-shape interfaces, about module procedures not
  being inlined without LTO, and about `.bss` zeroing of `TDC`/`TFC` follow from
  gfortran semantics and `CMakeLists.txt:673-691`. They are checkable by reading
  the generated assembly and have not been checked.
- **Dataset calibration is from the shipped examples only.** The link counts,
  element counts and grid sizes in §1.2 come from five example datasets and
  their `output_should` print files, not from production inputs. `N = 2`
  cross-section points — which is what makes M4 low priority — was verified for
  `dunsop` only.
