# Logical performance assessment: `VSmod`, `VSCOEF` and `VSINTC`

## Scope and method

This is a **logical, source-only** assessment. No profile was taken and no
timings were measured. Every claim below is derived from reading
`src/modules/VSmod.f90`, its callers, the array declarations in
`src/parameters/AL_C.F90` and `src/parameters/sglobal.f90`, and the compiler
flags in `CMakeLists.txt`. Where a claim depends on compiler behaviour rather
than on the source alone, that is stated explicitly.

The two routines named in the request are:

| Routine | Lines | Role |
|---|---|---|
| `VSCOEF` | `src/modules/VSmod.f90:644-850` | Builds vertical (`CBETM`, `CDBETM`, `CDBTMM`) and lateral (`CGAM1`, `CGAM2`, `CDGAM1`, `CDGAM2`) conductances, plus the diagonal accumulators `CF`/`CDF` |
| `VSINTC` | `src/modules/VSmod.f90:2648-2762` | Assembles the tridiagonal system `CA`, `CB`, `CC`, `CR` from those conductances |

Both are called once per Newton iteration from `VSCOLM`
(`src/modules/VSmod.f90:1068` and `:1075`), inside the loop at `:1060`.

## Conclusion up front

`VSCOEF` and `VSINTC` are **memory-and-latency bound, not flop bound**. A
representative inner lateral iteration in `VSCOEF` performs roughly 14 loads
(four of them indirect, three of them stride-4), 8 stores, 3 divisions and one
predicate evaluation, to produce about 16 useful floating-point results.
Optimisation effort should therefore go into removing loads, stores and
divisions, and hoisting invariant work — not into removing multiplies.

Three findings account for most of the avoidable work, and all three are
mechanical:

1. **`VSCOEF` computes the lateral conductivity arrays `CKIJ`/`CDKIJ` four
   times over when only two distinct values exist, unconditionally, and for a
   fully internal element the results are never read.** (`:788`, `:795-799`)
2. **`VSINTC` multiplies by a compile-time-zero constant `OMSIG` and the build
   flags prevent the compiler from folding it away**, keeping two whole arrays
   (`CPSIN`, `CPSIN1`) — one of them indirectly indexed — in the hot working
   set for no arithmetic effect. (`:2712`, `:2721`, `:2755-2756`)
3. **A large block of the inner-loop arithmetic is invariant** — either static
   for the whole simulation (geometry, connectivity, soil map) or fixed for the
   duration of a `VSCOLM` call (neighbour state) — yet is recomputed on every
   Newton iteration, of every global iteration, of every timestep.

Because everything in these routines sits inside a triple-nested iteration
(`VSSIM` global loop `NITMAX=10` → element loop → `VSCOLM` Newton loop
`NITMAX=100`), the **iteration histogram is the single most valuable
measurement to take**, since it is the multiplier on every item below.

## 1. The cost structure

### 1.1 Loop nesting

```text
per timestep
  VSSIM                                              VSmod.f90:4220
    global iteration NIT = 1..10                     :4405   NITMAX = 10
      element loop over ISORT                        :4410
        skip if OK(IEL)                              :4413
        stage neighbour data for 4 faces             :4430-4466
        VSCOLM                                       :4476
          Newton iteration NIT = 1..100              :1060   NITMAX = 100
            VSFUNC   soil property lookup            :1063
            VSCOEF   conductances                    :1068
            VSINTC   matrix assembly                 :1075
            VSUPPR / VSWELL / VSSPR / VSBC / VSSAI   :1084-1115
            VSLOWR                                   :1119
            TRIDAG                                   :1124
            convergence test                         :1135
          final flux recovery                        :1152-1171
```

The worst case for a single element in a single timestep is 10 × 100 = 1000
full assemblies. `LLEE = 50` (`src/parameters/sglobal.f90:119`), so a column is
at most 50 cells and `VSCOEF`'s face loop is at most 4 × 50 = 200 inner
iterations.

### 1.2 Invariance classes

Classifying each quantity by how often it can actually change is what exposes
the redundancy. Nothing in the current code exploits classes S, T or G.

| Class | Changes | Examples |
|---|---|---|
| **S** — static for the run | never after setup | `CDELZ`, `CA0`, `CAIJ`, `ICSOIL`, `VSK3D`, `JCACN`, `JCDEL`, `JCDEL1`, `JELDUM`, `JCBC`, `CWV`, `CWL` |
| **T** — per timestep | `DT` | `DT` (`DTUZ`) |
| **G** — per `VSCOLM` call | neighbour state | `CPSI1`, `CPSIN1`, `CZ1`, `CKIJ1` — all `INTENT(IN)` to `VSCOLM` |
| **N** — per Newton iteration | current iterate | `CPSI`, `CKR`, `CDKR`, `CETA`, `CDETA` |

`CWV` and `CWL` are read once from `VS04` at `VSmod.f90:3394-3395` and never
reassigned — they are class S, not merely loop-invariant.

**Calibration against real data.** Every example dataset checked
(`examples/Slapton`, `examples/dunsop-hot1`,
`examples/Aire_at_Kildwick_Bridge-simple`) sets `VSWV = VSWL = 1.0`. So in
practice:

- the hot vertical path is the **arithmetic-mean branch** at `:731-745`, not
  the harmonic or general branches;
- `NOTONE(CWL)` at `:824` is **always false**, so none of the `**` operators at
  `:825-831` are ever evaluated.

This matters for prioritisation: the `pow()` calls, which would otherwise
dominate, are dead in practice. The cost is the surrounding memory traffic and
the repeated predicate evaluation; optimized builds now use IPO/LTO, so the
one-line predicate is eligible for inlining.

### 1.3 Build flags

`CMakeLists.txt:72-84,684-706`: the default `Release` build is `-O2
-fno-fast-math` for GNU, with no `-march`; `ReleaseNative` is `-O3
-march=native -fno-fast-math`. CMake requires IPO/LTO support and enables it
for both optimized configurations. Consequences that matter below:

- **`x * 0.0` is not folded** (needs `-ffinite-math-only`), while `x * 1.0`
  *is* folded (only blocked by `-fsignaling-nans`, which is off). This
  asymmetry is exactly what makes finding 2 real.
- **Module `ELEMENTAL` predicates can be inlined across files** in optimized
  builds. The source still evaluates `NOTONE`, `ISZERO`, and `ISONE` at their
  call sites; whether a particular compiler actually inlines each one remains
  a code-generation question.
- **Baseline x86-64 codegen**: SSE2, two doubles per vector, no FMA.
- GCC 16 does enable `-ftree-vectorize` at `-O2`, but with the `very-cheap`
  cost model, which in practice declines loops with runtime trip counts. Every
  column loop here runs `ICBOT:ICTOP` with runtime bounds, so these loops are
  most likely running scalar today.

## 2. `VSCOEF` findings

### V1 — `CKIJ`/`CDKIJ`: computed 4× for 2 values, unconditionally, often dead

**P0. Bitwise identical to fix.**

At `:788`, `M = 1 + MOD(J - 1, 2)` takes only two values across the four faces:

| `J` | 1 | 2 | 3 | 4 |
|---|---|---|---|---|
| `M` | 1 | 2 | 1 | 2 |

So `CKIJ(:,3)` is elementwise identical to `CKIJ(:,1)`, and `CKIJ(:,4)` to
`CKIJ(:,2)` — likewise `CDKIJ`. The fill at `:795-799` runs for all four faces
and all cells, so **half of those 4 × `ncell` gathers, 8 × `ncell` multiplies
and 8 × `ncell` stores are pure duplication**, on every Newton iteration.

Worse, the results are frequently never read at all. `CKIJ`/`CDKIJ` have no
consumer inside `VSCOEF` — `KIJ`/`DKIJ` are used as locals. Their only
consumers are `VSBC` (`VSCOLM:1105-1106`) and `VSSAI` (`:1111-1112`), which are
called only when `JCBC(IFA)` is in `{3,4,5}` or `{9,10}` respectively
(`:1102`, `:1109`). **For an element with all four faces internal
(`JCBC(1:4) = 0`) the entire fill is dead code.** That is the common case in
the interior of a catchment.

There is a second effect. Because the fill sits before the `CYCLE` at `:805`,
the cell loop must be entered for every face even when `TEST` is true — that
is, even when `JELDUM(J) < 1` (no neighbour) or `JCBC(J) == 9` (handled
elsewhere). A boundary element currently walks its full column on a face that
contributes nothing.

**Fix.** Split the loop. Compute two contiguous vectors — `KX(I) = CKR(I) *
VSK3D(ICSOIL(I),1)` and `KY(I) = CKR(I) * VSK3D(ICSOIL(I),2)`, plus their
derivatives — and only when at least one face has a boundary type in
`{3,4,5,9,10}`. Then the face loop can open with `IF (TEST) CYCLE face_loop`
and skip dead faces entirely. This is exactly the same arithmetic, so results
are bitwise identical; the split loops are also contiguous and vectorisable,
which the current fused loop is not (the `CYCLE` at `:805` blocks it).

### V2 — `NOTONE(CWL)` evaluated in the innermost loop

**P0. Bitwise identical to fix.**

`:824` evaluates `NOTONE(CWL)` once per cell, per face, per Newton iteration —
up to 200 evaluations per assembly. `CWL` is class S. `NOTONE` is an
`ELEMENTAL` module function (`src/parameters/sglobal.f90:505`). IPO/LTO is
enabled in optimized builds, so call overhead will usually disappear, but the
invariant comparison and branch remain in the source and need not be hoisted
out of the loop by every compiler.

Hoist to a `LOGICAL` local computed once at routine entry — ideally hoisted
further, out of the Newton loop in `VSCOLM` altogether.

### V3 — Static geometry recomputed on every Newton iteration

**P1. Bitwise identical if the same expression is precomputed unchanged.**

Every one of the following is class S and is re-evaluated per Newton iteration:

| Line | Expression | Cost per cell (or cell-face) |
|---|---|---|
| `:713-714` | `VSK3D(ICSOIL(I),3) * AREA2 / CDELZ(I)` | indirect gather + division |
| `:734`, `:753` | `VSK3D(ICSOIL(I),3)` | indirect gather |
| `:741`, `:763` | `CA0 / (CDELZ(M) + CDELZ(I))` | division |
| `:795` | `VSK3D(ICSOIL(I),M)` | indirect gather |
| `:807-811` | `ABS(JCDEL(J,I))+1`, `JCDEL1(K,J)`, `K+DELKJ`, `ABS(DELKJ)+1` | 3 strided/indirect integer loads |
| `:815` | `CAIJ(J,I) / DBLE(NKJ)` | stride-4 load + int→double + division |

On baseline x86-64 a `divsd` is ~13–14 cycle latency and poorly pipelined,
against ~4 cycles for `mulsd`. Two to three divisions per cell-face is a large
fraction of the routine's real cost, and all of them are avoidable.

**Fix.** Extend the existing one-off setup block in `VSSIM`
(`:4269-4337`, guarded by `FIRSTvssim`, which already precomputes `VSAIJsv`
and `ICSOILsv`) to also build per-element static arrays: the vertical
conductance geometry factor, the lateral area-per-split factor, and the
connectivity triple `(NIJ, NKJ, K1)`. Precomputing the *whole* expression
`VSK3D(ICSOIL(I),3) * AREA2 / CDELZ(I)` as one stored value is bitwise
identical to evaluating it in place, because the operation order is unchanged.

This also removes the stride-4 reads of `JCACN`, `JCDEL` and `CAIJ` from the
hot loop — see V6.

### V4 — `1/DXDUM` not hoisted

**P2. Changes results in the last bit.**

`:834-835` divide by `DXDUM` per cell-face. `DXDUM` is set once per face at
`:790`, and `WO2DX = half * CWL / DXDUM` is *already* hoisted at `:791` — so
the reciprocal is simply missing. Adding `RDX = one / DXDUM` beside it and
multiplying removes two divisions per cell-face.

Flagged P2 because `C1/DXDUM` and `C1*(1/DXDUM)` are not bitwise equal. Given
`WO2DX` establishes the precedent, this is likely acceptable, but it needs to
be decided rather than assumed.

### V5 — Neighbour terms recomputed per Newton iteration

**P1. Bitwise identical to fix.**

`:813-814` compute

```fortran
CKJ  = CKIJ1(K,  J) * CAIJ1(K,  J) / DBLE(NIJ)
CK1J = CKIJ1(K1, J) * CAIJ1(K1, J) / DBLE(NIJ)
```

`CKIJ1` and `CAIJ1` are both `INTENT(IN)` to `VSCOLM` and unchanged across the
Newton loop — class G. `NIJ` is class S. So `CKJ` and `CK1J` are **constant for
the whole duration of a `VSCOLM` call** yet are rebuilt on every iteration, at
a cost of four indirect loads, two multiplies and two divisions per cell-face.

Hoisting them to a per-element precompute done once per `VSCOLM` entry
eliminates that entirely. (In the general-`CWL` case it would also hoist the
`CKJ**CWL` and `CK1J**CWL` calls at `:826-827` — not relevant to the example
datasets, but relevant to any dataset that does use a w-mean.)

### V6 — Stride-4 access in the innermost loop

**P1, subsumed by V3.**

`JCACN(4,ICBOT:ICTOP)`, `JCDEL(4,ICBOT:ICTOP)` and `CAIJ(4,ICBOT:ICTOP)` are all
read as `(J,I)` with `I` innermost, i.e. **stride 4**. For the double-precision
`CAIJ` that is a 32-byte stride: two useful values per 64-byte cache line, so
half the fetched bytes are wasted. For the integer arrays, four per line.

This layout is deliberate — the 1997-05-13 history entry at `:642` records the
index swap, and the global arrays `JVSACN`, `JVSDEL`, `QVSH` and `VSAIJsv` are
all `(4, cell, element)` (`src/parameters/AL_C.F90:108-109`, `:160`)
specifically so `VSSIM` can pass a contiguous per-element slice at `:4477-4482`.
Changing the global layout would be a module-wide change with wide blast
radius.

**It is not necessary to change it.** All three arrays feed only class-S
quantities. Precomputing per V3 moves the strided reads out of the Newton loop
and into one-off setup, capturing the benefit without touching the global
layout at all. This is the cleanest available win-to-risk ratio in the routine.

### V7 — Split vertical loops with a gather

**P1, follows from V3.**

Each vertical branch (`:712-717`, `:733-737`, `:752-756`) makes one pass to
fill `C`/`D`, then a second (`:719-729`, `:739-745`, `:758-767`) to combine
them. The first pass carries the `VSK3D(ICSOIL(I),·)` gather, which is what
blocks vectorisation. Once V3 supplies a contiguous per-cell conductivity
vector, pass one collapses to `C(I) = CKR(I) * KZA(I)` — a clean vector
multiply — and can then be fused into pass two with a rolling scalar, removing
the `C`/`D` array traffic completely.

Note that `C` and `D` are `DWORK1`/`DWORK2` from `VSCOLM` (`:1041`, `:1072`),
and `DWORK1` is subsequently reused as `VSINTC`'s `H` workspace (`:1080`) and
again by `VSBC`/`VSWELL`. The reuse is safe — `C` is dead by then — but it does
mean the same cache lines are being rewritten several times per iteration.

### V8 — Real-exponent `**` (conditional on dataset)

**P3. Not hot in any example dataset.**

The general branches use `(CKR*CKZS)**CWV` (`:754`), `CAVE**WI` (`:764`) and
`(CAVE/CI)**WIM1` twice (`:765-766`) — three `pow()` calls per cell — plus up
to five per cell-face laterally (`:825-831`). At roughly 40–100 cycles each
these would dominate everything else in the routine.

All example datasets set `VSWV = VSWL = 1.0`, so these paths are dead there.
**Confirm against the actual production datasets before deprioritising**; if
any real run uses a w-mean, V5's hoist becomes the highest-value change in the
file rather than a P1.

`RCM**2` / `RCI**2` at `:727-728` use integer exponents and expand to
multiplies. No action.

## 3. `VSINTC` findings

### I1 — Multiplication by a compile-time zero that the compiler must keep

**P0. Identical except for the sign of zero.**

`:2712` declares `SIGMA = 1.0D0, OMSIG = 1.0D0 - SIGMA`, both `PARAMETER`. So
`OMSIG` is exactly `0.0` at compile time, and the scheme is fully implicit.
Yet under `-fno-fast-math` GCC **must not** fold `0.0 * x → 0.0`, because that
transformation is invalid for NaN and signed-zero operands. (It *will* fold
`1.0 * x → x`, since `-fsignaling-nans` is off — hence the `SIGMA` multiplies
cost nothing while the `OMSIG` ones cost everything.)

The result:

- `:2721` — `H(I) = SIGMA*CPSI(I) + OMSIG*CPSIN(I) + CZ(I)` performs a full
  load of `CPSIN(I)`, a multiply and an add, per cell, to add zero.
- `:2755-2756` — `HK` and `HK1` each load `CPSIN1(K,J)` and `CPSIN1(K1,J)`.
  These are **indirect** loads into a `(LLEE,4)` array, so each one may touch a
  fresh cache line, per cell-face, per Newton iteration, to contribute nothing.

Deleting the `OMSIG` terms removes `CPSIN1` from `VSINTC`'s working set
entirely. `CPSIN` itself is still needed for `DPSI = CPSI(I) - CPSIN(I)` at
`:2741`, so it stays — but `CPSIN1` becomes dead across the whole
`VSCOLM`/`VSINTC` path, which in turn lets the staging copy at `VSSIM:4461` be
deleted (see C1).

**Precision caveat, stated exactly.** For finite `CPSIN`, `x + 0.0*y == x`
except when `x` is `-0.0`, where `-0.0 + 0.0` yields `+0.0`. So results are
bitwise identical except that an exactly-negative-zero head could become
positive zero. There is also a behaviour change for non-finite input: the
current code propagates a NaN in `CPSIN`/`CPSIN1` into `H`, the new code would
not. Both are almost certainly improvements, but they should be recorded rather
than described as "no change".

If `SIGMA` is meant to survive as a time-weighting knob, keep it as a named
constant and structure the code so the zero case is resolved at compile time,
rather than paying for it at runtime on every cell.

### I2 — Neighbour heads recomputed on every Newton iteration

**P0. Bitwise identical to fix.**

`:2755-2757` compute

```fortran
HK  = SIGMA * CPSI1(K, J)  + OMSIG * CPSIN1(K, J)  + CZ1(K, J)
HK1 = SIGMA * CPSI1(K1, J) + OMSIG * CPSIN1(K1, J) + CZ1(K1, J)
```

All three source arrays are `INTENT(IN)` to `VSCOLM` — class G, fixed for the
entire Newton loop. These values are **recomputed up to 100 times per column
with identical results**, at a cost of three indirect loads, two multiplies and
two adds per cell-face each.

Worse, the same quantity is computed a *third* time in `VSCOLM`'s flux recovery
loop at `:1166-1167` (`H1 = CZ1(K,J) + CPSI1(K,J)`), which — with `SIGMA = 1` —
is numerically the same expression again.

**Fix.** Build a single `H1(LLEE,4) = CZ1 + CPSI1` array once. The natural
place is `VSSIM`'s neighbour staging loop at `:4456-4464`, which is already
walking exactly those arrays — it can write `H1` directly and stop staging
`CZ1`, `CPSI1` and `CPSIN1` as three separate arrays.

### I3 — `K1` derived three times from static connectivity

**P1, folds into V3.**

`K1 = JCDEL1(K,J) + K` is computed at `VSCOEF:809`, `VSINTC:2754` and
`VSCOLM:1164` — three times per cell-face per Newton iteration, from class-S
data. Precompute once alongside V3.

### I4 — Division inside the cell loop

**P1 partially, P2 fully.**

`:2739` — `VODT = CDELZ(I) * CA0 / DT` — one division per cell per Newton
iteration. `CDELZ(I) * CA0` is class S; `CA0 / DT` is class T.

Two options with different risk:

- **Bitwise safe:** precompute `VOL(I) = CDELZ(I) * CA0` statically and keep
  `/ DT`. Removes the multiply only; the division stays.
- **Reassociating:** hoist `AODT = CA0 / DT` before the loop and use
  `VODT = CDELZ(I) * AODT`. Removes the division, but
  `(CDELZ*CA0)/DT ≠ CDELZ*(CA0/DT)` in the last bit.

The second is the one worth having, and should be decided together with V4 as a
single "reciprocal hoisting" policy rather than case by case.

### I5 — Redundant multiplies from alias locals

**P2. Changes results in the last bit.**

`:2735-2737` set `CDFM = CDBMMI` (`= CDBTMM(I)`) and `CDFP = CDBTPP`
(`= CDBETM(P)`) — pure aliases. So `:2743-2744` read:

```fortran
CA(I) = SIGMA*CBETMI - HI*CDFM + HM*CDBMMI   ! = CBETM(I) + CDBTMM(I)*(HM-HI)
CC(I) = SIGMA*CBETPI - HI*CDFP + HP*CDBTPP   ! = CBETM(P) + CDBETM(P)*(HP-HI)
```

Each is two multiplies where one would do. Saves two multiplies per cell —
modest, and the factoring changes rounding, so it is P2 despite being trivial.
Its real value is legibility: the factored form makes the conductance-times-
head-difference structure obvious, which the alias chain currently hides.

### I6 — Sliding-window loads

**P3. Inspect the assembly before touching.**

`:2730-2734` load indices `I` and `I+1` of `CBETM`, `CDBETM` and `CDBTMM`; the
`I+1` value of one iteration is the `I` value of the next. A rotating-register
formulation would halve these loads. GCC's load PRE may already be doing this
at `-O2`, and hand-rotating would *block* vectorisation if `-O3` is later
adopted. **Do not hand-optimise this without first reading the generated
assembly.**

### I7 — Face masks recomputed per call

**P1, cosmetic but free.**

`:2752` re-tests `JELDUM(J) < 1 .OR. JCBC(J) == 9` on every call, as does
`VSCOEF:789`. Both are class S. A precomputed per-element active-face list
(`nfaces`, `face_list(1:nfaces)`) removes the branch and lets the loop skip
dead faces without entering them.

### I8 — What `VSINTC` does well

For balance: the main loop at `:2726-2748` is otherwise clean — unit-stride
throughout, no gathers, no early exits, no calls. Once I1 and I4 are addressed
it is a genuinely vectorisable loop, and it is the part of the assembly most
likely to benefit from `-O3 -march=native`. The `H(ICBOT-1) = 0` /
`H(ICTOP+1) = 0` guard band at `:2717` and `:2724` correctly removes the
boundary special cases from the loop body — that is the right technique and
should be preserved.

## 4. Surrounding context that governs whether the above matters

These are outside the two named routines but determine their effective cost.

### C1 — `VSSIM` neighbour staging is redundant across elements

**P1. High value.**

`VSSIM:4456-4464` copies six arrays over the neighbour's **entire** column, for
all four faces, of every element, on every global iteration:

```fortran
DO JCL = NLYRBT (JEL, 1), top_cell_no
   JCDEL1 (JCL, IFA) = JVSDEL (JFA, JCL, JEL)
   CAIJ1  (JCL, IFA) = VSAIJsv (JFA, JCL, JEL)
   CZ1    (JCL, IFA) = ZVSNOD (JCL, JEL)
   CPSI1  (JCL, IFA) = VSPSI  (JCL, JEL)
   CPSIN1 (JCL, IFA) = VSPSIN (JCL, JEL)
   N = ICSOILsv (JCL, JEL)
   CKIJ1  (JCL, IFA) = VSKR (JCL, JEL) * VSK3D (N, K)
END DO
```

Three problems:

- **`CKIJ1` is recomputed once per adjoining element.** Each element is a
  neighbour of up to four others, so `VSKR(JCL,JEL) * VSK3D(N,K)` is evaluated
  up to **four times redundantly across the mesh**, every global iteration.
  A single pass computing `VSKX(cell,elem)` and `VSKY(cell,elem)` before the
  element loop removes all of it.
- **The full neighbour column is copied** regardless of which cells actually
  appear in `JCACN`. `JCDEL1` and `CAIJ1` are class S and need not be re-staged
  at all.
- **Six arrays, of which two become dead** once I1 and I2 land (`CPSIN1`
  entirely; `CZ1`/`CPSI1` merge into a single `H1`). The loop drops from six
  streams to roughly two.

### C2 — `VSCOLM` local arrays are sized to `LLEE`, not to the column

**P1. Bitwise identical to fix.**

`VSCOLM:1041-1047` declares about twenty automatic arrays, all dimensioned
`LLEE = 50`, including six of shape `(LLEE,4)`. That is roughly
`32 × 50 × 8 ≈ 12.8 kB` of stack per call, comparable to a 32 kB L1D and
touched on every call.

For a column of, say, 12 active cells, the `(LLEE,4)` arrays place consecutive
face columns **400 bytes apart**, so each face touches its own distinct set of
cache lines and roughly three-quarters of every touched region is never
written. Sizing these to `ICBOT:ICTOP` compacts the working set by the ratio
`ncell/LLEE`.

This is low-risk: `VSCOLM` already passes scalar-start dummy arguments relying
on sequence association (see the 2026-04 history entry at `:953`), so the
bounds are already effectively dynamic at the callee.

### C3 — Partially-assigned `INTENT(OUT)` arrays: correctness, with a performance edge

**P1. Correctness-adjacent.**

`CGAM1`, `CGAM2`, `CDGAM1`, `CDGAM2`, `CKIJ` and `CDKIJ` are `INTENT(OUT)` in
`VSCOEF` but are written only where
`JCACN(J,I) /= 0 .AND. JELDUM(J) >= 1 .AND. JCBC(J) /= 9` (`:789`, `:805`).

`VSCOLM`'s flux recovery loop at `:1157-1171` reads `CGAM1`/`CGAM2` under a
**weaker** guard — `JELDUM(J) >= 1 .AND. JCACN(J,I) >= 1` — with no
`JCBC(J) == 9` exclusion. The routine's own FORD header asserts that type-9
faces have no internal lateral cell connectivity, so today this is held by an
**invariant, not by a code guard**.

If that invariant is ever violated, the read is of uninitialised stack. Beyond
the obvious correctness problem, there is a performance edge: uninitialised
stack doubles are frequently subnormal, and subnormal operands cost 100+ cycles
on x86 without flush-to-zero — which `-fno-fast-math` deliberately withholds. A
sporadic, data-dependent slowdown of that kind is exactly the sort of thing
that is very hard to attribute later.

Add an assertion, or zero the used range explicitly.

### C4 — Build configuration is a free lever, and should be tested first

**P0 in effort terms — no source change at all.**

Under the default `Release` (`-O2`, baseline x86-64, with IPO/LTO):

- the column loops have runtime trip counts and will mostly be declined by GCC
  16's `very-cheap` cost model at `-O2`;
- codegen is SSE2, two doubles per vector, **no FMA**;
- cross-file `NOTONE`/`ISZERO`/`ISONE` calls are eligible for inlining.

`ReleaseNative` (`-O3 -march=native`) already exists in `CMakeLists.txt:704-706`
and would supply the dynamic vectoriser cost model plus AVX2/FMA. **Measure it
before doing any source surgery** — it costs nothing and it recalibrates how
much the source changes are worth. IPO/LTO is already enabled in both optimized
configurations, so V2 is now about invariant work and branch placement rather
than guaranteed call overhead.

Neither lever helps the sparse, gather-driven lateral loop, which stays scalar
regardless. The source changes in V1, V3 and V7 are what make the vertical
loops vectorisable in the first place.

### C5 — Iteration counts dominate everything above

**P3 by risk, P0 by information value.**

`VSCOLM` allows `NITMAX = 100` Newton iterations (`:1037`); `VSSIM` allows
`NITMAX = 10` global iterations (`:4225`). Worst case is 1000 assemblies per
element per timestep.

Since `VSSIM` holds neighbour data fixed for the duration of a `VSCOLM` call, a
column that has not converged in ~15 Newton iterations is usually being driven
by neighbour data that is itself about to change; spending 85 more assemblies
against stale neighbour state is largely wasted. Reducing the inner cap and
leaning on the outer loop is the standard remedy — but it is a **solver
behaviour change, not a code cleanup**, and must be validated on convergence
behaviour, not just on timing.

**Measure the iteration histogram first.** It is cheap (a counter and a
distribution dump), it is the multiplier on every item in this document, and it
will immediately show whether the typical column takes 3 iterations or 40. If
it is 3, the fixed per-call overheads (C2, staging in C1) dominate; if it is
40, the per-iteration work (V1, V3, V5, I1, I2) dominates. **The two answers
imply different work orders**, so this measurement should precede the P1 items.

Related: `VSCOLM` tests convergence only after a full assembly and `TRIDAG`
solve (`:1124-1135`). On re-entry for an already-converged column, one complete
assembly is spent to discover `DPSIMX <= CEPSMX`. The `OK(IEL)` mask at
`:4512-4521` suppresses this, but only from `NIT >= NITMIN = 2` and only when
all four neighbours have also converged.

### C6 — `VSFUNC`: table layout, and a possible Jacobian inconsistency

**P3, but potentially the largest single effect in the file.**

`VSFUNC` (`:2162-2304`) is called once per Newton iteration alongside `VSCOEF`
and `VSINTC`, so it shares their multiplier.

**Layout.** `:2283-2295` read five separate lookup tables — `VSPTHE`, `VSPETA`,
`VSPDKR`, `VSPKR`, `VSPDET` — each indexed `(JLO/JHI, IS)`. `JLO` and
`JHI = JLO+1` are adjacent, so each table costs about one cache line, but there
are five distinct streams per cell. Interleaving them into a single array
indexed `(property, row, soil)` would collapse this to one or two lines per
cell. The hunt-and-bisect with the `ICSTOR` cache (`:2210`, `:2271`) is a sound
design and is near-O(1) once converged — no change needed there.

**Possible Jacobian inconsistency.** `:2286` sets

```fortran
CETA(ICL) = VSPETA(JHI, IS)
```

— a nearest-upper table lookup, **not** an interpolation, while its own
derivative `CDETA` at `:2294-2295` *is* interpolated. As implemented,
`d(CETA)/d(psi)` is zero within a table interval, yet `CDETA` is non-zero
there. That inconsistency feeds `CDG` in `VSINTC:2740` and hence the diagonal
`CB` at `:2745`.

An inexact Jacobian degrades Newton from quadratic to linear convergence.
Since Newton iteration count is the multiplier on every other finding in this
document, **if this is unintentional it could outweigh all the micro-optimisations
combined**. It is stated here as a hypothesis to test — plausibly deliberate
(a smoothing or stability measure) — and the test is cheap: interpolate `CETA`
consistently and compare the iteration histogram from C5.

## 5. Recommended order of work

| Priority | Change | Findings | Expected benefit | Numerical risk |
|---|---|---|---|---|
| **P0** | Measure the Newton/global iteration histogram | C5 | None directly — determines the ordering of everything below | None |
| **P0** | Time a `ReleaseNative` build against `Release` | C4 | Possibly large; zero effort | None |
| **P0** | Hoist `NOTONE(CWL)` out of the inner loop | V2 | Removes up to 200 invariant predicate evaluations and branches per assembly | **None — bitwise identical** |
| **P0** | Split the `CKIJ`/`CDKIJ` fill; compute twice not four times; skip dead faces | V1 | Removes ~half the lateral stores, and all of them for interior elements | **None — bitwise identical** |
| **P0** | Delete the `OMSIG` terms | I1 | Removes `CPSIN1` (indirect) and `CPSIN` from the hot set | Signed zero and NaN propagation only |
| **P0** | Precompute `H1 = CZ1 + CPSI1` once per `VSCOLM` call | I2 | Removes 3 indirect loads + 4 flops per cell-face per iteration | **None — bitwise identical** |
| **P1** | Per-element static geometry/connectivity precompute | V3, V6, V7, I3, I7 | Removes all inner-loop divisions, gathers and stride-4 reads | **None if expressions precomputed verbatim** |
| **P1** | Hoist `CKJ`/`CK1J` out of the Newton loop | V5 | 4 indirect loads + 2 div + 2 mul per cell-face per iteration | **None — bitwise identical** |
| **P1** | Reduce `VSSIM` neighbour staging; compute `VSKX`/`VSKY` once | C1 | Removes up to 4× redundant work across the mesh | **None if multiply order preserved** |
| **P1** | Size `VSCOLM` locals to the active column | C2 | Compacts a ~12.8 kB per-call working set | **None — bitwise identical** |
| **P1** | Assert or zero the partially-assigned `INTENT(OUT)` ranges | C3 | Correctness; closes a subnormal-stall path | None |
| **P2** | Reciprocal hoisting (`1/DXDUM`, `CA0/DT`) | V4, I4 | Removes ~3 divisions per cell-face | Last-bit reassociation |
| **P2** | Factor `CA`/`CC` in `VSINTC` | I5 | 2 multiplies per cell; clearer code | Last-bit reassociation |
| **P3** | Interleave the `VSFUNC` lookup tables | C6 | 5 cache-line streams → 1–2 | **None — layout only** |
| **P3** | Investigate `CETA` vs `CDETA` consistency | C6 | Potentially large via iteration count | **Solver behaviour — validate carefully** |
| **P3** | Review `NITMAX = 100` | C5 | Potentially large | **Solver behaviour — validate carefully** |
| **P3** | Sliding-window loads in `VSINTC` | I6 | Marginal | Read the assembly first |

The P0 block is deliberately all-mechanical: two measurements that change no
code, and four source changes that are bitwise identical or near enough to be
accepted on a timing measurement alone. It should be banked before anything
with a numerical footprint is attempted.

The P1 block is where the bulk of the benefit sits, and it is dominated by one
idea — **move class-S and class-G work out of the class-N loop**. Done as
verbatim precomputation of existing expressions, it carries no numerical risk
at all; the risk is in the bookkeeping of getting the static arrays built and
indexed correctly, which is what the validation below is for.

## 6. Validation

For P0 and P1, the acceptance test is **bitwise-identical output** across the
example suite. Each of those changes either preserves the operation sequence
exactly or moves it earlier without reordering, so any diff at all indicates a
bookkeeping error — most likely in the precomputed static arrays — rather than
an acceptable rounding difference. The two exceptions to record explicitly:

- **I1** may flip an exactly-negative-zero head to positive zero, and stops
  propagating non-finite `CPSIN`/`CPSIN1` values into `H`.
- **C1** is bitwise identical only if `VSKX`/`VSKY` are formed with the same
  multiply order as the current `VSKR(JCL,JEL) * VSK3D(N,K)`.

For P2, bitwise identity is impossible by construction, so a documented
tolerance is required, together with an unchanged sequence of accepted
timesteps.

For P3, timing alone is insufficient — the C5 and C6 items change solver
behaviour and must be validated on convergence and mass balance.

Throughout, build the P1 work under `-fcheck=bounds` (the `Debug`
configuration at `CMakeLists.txt:676-678`). The static-precompute work adds new
indexed arrays to a routine that already relies on sequence association through
scalar-start dummy arguments, which is precisely the situation where a bounds
check earns its cost. Note also that C3 must be fixed before bounds-checked
validation is meaningful, since an uninitialised read will not be caught by a
bounds check but may perturb results non-deterministically.

## 7. What this assessment does not establish

- **No attribution of measured runtime.** Nothing here quantifies what fraction
  of a simulation is spent in `VSCOEF` or `VSINTC`. A profile is still required;
  these findings identify avoidable work, not where the time actually goes.
- **Iteration counts are unknown.** The multiplier on every finding is the
  Newton and global iteration count, which was not measured. This is the
  largest single gap and is the first item in the work order for that reason.
- **Cache behaviour is inferred from declared shapes**, not measured. The
  stride and working-set arguments (V6, C2, C6) follow from the array
  declarations and `LLEE = 50`, but no cache-miss counts were taken.
- **Compiler behaviour is inferred from flags and version.** The claims about
  `x*0.0` not being folded, about `very-cheap` declining runtime-trip-count
  loops, and about IPO making module predicates eligible for inlining follow
  from GCC 16 semantics and `CMakeLists.txt:72-84,684-706`. The actual inlining
  decisions are checkable in generated assembly and have not been checked.
- **Dataset calibration is from the shipped examples only.** The finding that
  `VSWV = VSWL = 1.0` — which is what makes V8 low priority — was verified
  against three example datasets, not against production inputs.
