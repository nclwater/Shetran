# Logical analysis: `ETmod` — correctness, performance, memory/variable management

## Scope and method

Source-only assessment of `src/modules/ETmod.f90` (920 lines). No profile was
taken and no timings were measured. Every claim below is derived from reading
the module together with:

- its only caller, `src/modules/run_sim.f90` (`SIMULATION`),
- `src/modules/SMmod.f90` (`SMIN`, `SMET`, `SM`, `initialise_smmod`) — the
  routine `ETIN` hands control to,
- `src/modules/VSmod.f90:4340-4380` (`VSSIM`'s `CQ` assembly, the hot
  consumer of `ERUZ`) and `src/modules/VSmod.f90:2990-3005` (VSS mass balance),
- `src/modules/CMmod.f90:1572` and `src/modules/rest.f90:280-290` (the other
  `ERUZ` consumers),
- `src/modules/FRmod.f90:4820-5040` (`INET`, which fills this module's inputs)
  and `src/modules/rest.f90:820-925` (`METIN`, which fills `DEL`/`U`/`RN`/`VPD`),
- the declarations in `src/parameters/AL_C.F90`, `src/parameters/AL_D.f90`,
  `src/parameters/AL_G.F90` and `src/parameters/sglobal.f90`,
- `src/modules/utilsmod.f90:51-92` for `DCOPY`,
- the compiler configuration in `CMakeLists.txt`.

Where a claim depends on compiler behaviour rather than on the source alone,
that is stated. Where a claim depends on an input invariant I could not confirm
from the source, it is presented as an unguarded assumption, not as a defect.

Relevant compiled extents:

| Constant | Value | Source |
|---|---|---|
| `LLEE` (vertical cells per column) | 50 | `sglobal.f90:119` |
| `NVEE` (vegetation/met/rain capacity) | 250 000 | `sglobal.f90:121` |
| `NELEE` (element capacity) | 250 000 | `sglobal.f90:118` |
| `NUZTAB` (tension-table rows) | 20 | `sglobal.f90:125` |
| `NVBP` (vegetation breakpoints) | 140 | `sglobal.f90:124` |
| `vsmall` (the `ISZERO` band) | `1.0e-20` | `sglobal.f90:190` |

Relevant array layouts:

| Array | Declared shape | Declared at |
|---|---|---|
| `ERUZ` | `(total_no_elements, top_cell_no)` — **element-major** | `AL_C.F90:159`, `:190` |
| `DELTAZ` | `(LLEE, total_no_elements)` — cell-major | `AL_C.F90:110`, `:232` |
| `VSPSI` | `(top_cell_no, total_no_elements)` — cell-major | `AL_C.F90:207` |
| `CQ` (VSmod consumer of `ERUZ`) | `(LLEE, NELEE)` — cell-major | `VSmod.f90:4243` |
| `ICMREF` | `(NELEE, 12)` — element-major | `AL_G.F90:46` |
| `S` | `(LLEE)` | `AL_D.f90:225` |

---

## Conclusion up front

Five findings dominate; the rest are cheap cleanups.

1. **When `ET` is skipped on the snow path, `DRAIN` and sometimes `PNET` are
   never assigned, so `DRAINA(IEL)` and `PNETTO(IEL)` export the *previous*
   element's values.** This is not a theoretical hazard: with snowmelt enabled,
   every element that is below freezing with no snowpack and no precipitation
   takes this path, and `PNETTO` feeds the VSS column source term while `DRAINA`
   feeds SYmod's raindrop-impact erosion (which asserts `0 <= DRAINA <= PNETTO`).
   The same stale `PNETTO` also propagates into OC surface-water forcing, CM
   contaminant transport, and MN rainfall input. Finding **C1**.

2. **`ERUZ` is the only element-major array in the ET/VSS data path, and both
   its producer and its consumers pay for it.** `ET` writes `ERUZ(IEL,II)` down
   a column — stride `total_no_elements * 8` bytes — and `VSmod:4374` reads it
   back as `CQ(ICL,IEL) = -ERUZ(IEL,ICL)`, a full transpose against a cell-major
   destination. Transposing `ERUZ` to `(cell, element)` makes *both* sides
   contiguous. Finding **M1**.

3. **The array `S` is written on every root cell of every element of every
   timestep and is never read anywhere in the production code.** The writes cost
   a division per cell plus a second division in `ETIN`. `run_sim.f90:87` is the
   only importer outside ETmod/SMmod and it sits under the `!NEEDED ONLY FOR AD`
   comment; SMmod only writes it too. Finding **P2**.

4. **The root-depth warning formats its message on every element and every
   timestep, not once.** `WRITE(msg,...)` at `:506` sits *outside* the
   `IF (first)` guard at `:507`. Once a single element's root zone is deeper
   than its column, an internal formatted `WRITE` runs `total_no_elements` times
   per step for the rest of the run to produce a constant string that is
   discarded. Finding **P1**.

5. **`ETSIM`/`ET` use module-level scalars and per-vegetation arrays as
   per-element scratch.** `PE`, `EINT`, `PNET`, `DRAIN`, `ERZ`, `ESOIL`, `AE`,
   `CSTOLD`, `CPLAI`, `HRUZ` (AL_D) and `RA(N)`, `RC(N)`, `PSI4`, `UZALFA`,
   `msg` (ETmod) all carry state across element iterations. This is the direct
   cause of findings C1 and C2, and it is the single reason the element loop
   cannot be parallelised. Finding **M3**.

The module is otherwise sound. §5 lists what I checked and found correct so it
does not get re-audited.

---

## 1. Correctness

### C1 — `DRAIN` and `PNET` leak between elements on the snow path — *high*

`ETIN:756-763`:

```fortran
NSMT = 0
IF (BEXSM) CALL SMIN (IEL)
IF (NSMT /= 0 .OR. .NOT. BEXSM) THEN
   CALL ET (IEL)
   IF (BEXSM) CALL SMIN (IEL)
END IF
```

If `BEXSM` is true and the first `SMIN` leaves `NSMT == 0`, `ET` never runs.
`SMIN` (`SMmod.f90:729-733`) routes to `SMET` when `SD(IEL) > 0 .OR. TA(MS) <= 0`.
`SMET` (`SMmod.f90:636-654`) assigns `pnsnow`, `CSTOLD`, `ERZ`, `ESOIL`, `EINT`,
`AE`, `PE` and `S(1:NRD)` — but **never `DRAIN`**, and it assigns `PNET` only
indirectly, via `SM`, which it calls only when
`precip_m_per_s(IEL) > 0 .OR. SD(IEL) > 0`.

Two distinct leaks follow at `ETIN:770-773`:

| Output | Leaks when | Consumer |
|---|---|---|
| `DRAINA(IEL) = DRAIN/(1000*DTUZ)` | any `SMET` path (`DRAIN` is assigned nowhere in SMmod) | `SYmod.f90:2481` raindrop-impact erosion; `FRmod.f90:3395` output |
| `PNETTO(IEL) = PNET/1000` | `SD <= 0` **and** `TA <= 0` **and** `precip <= 0` — i.e. every frozen, snow-free, dry element | `VSmod.f90:4360` `CDNET`; `OCmod.f90:2157` surface-water solve; `CMmod.f90:1778` contaminant forcing; `MNmod.f90:2695` rainfall input; `SYmod.f90:4507` erosion; `rest.f90:284` water balance |

The `PNET` case is the more serious of the two. On a cold dry timestep no
element writes `PNET` at all, so *every* land element exports the last value any
element wrote — a real throughfall rate from an earlier wet step — as its net
surface-water input, for as many steps as the freeze lasts. `PNETTO` is not a
diagnostic: it is a shared forcing used by the VSS and OC water equations and
by CM, MN and SY before it reaches the water-balance and output paths.

The `DRAINA` case additionally violates a precondition SYmod documents for
itself (`SYmod.f90:2437`: "`DRAINA >= 0` and `DRAINA <= PNETTO` within
tolerance"), because the stale `DRAIN` bears no relation to the current
element's `PNETTO`.

The module header (`:713-722`) already records this as known cross-module
behaviour and explicitly declines to repair it. It is worth restating that the
consequence is a shared forcing error in VSS, OC, CM, MN, and SY, not a
cosmetic one.

**Fix.** The cheapest correct repair is entirely local to `ETIN`: initialise the
exported scalars before the snow dispatch rather than relying on the callee.

```fortran
NSMT  = 0
PNET  = ZERO
DRAIN = ZERO
IF (BEXSM) CALL SMIN (IEL)
```

This costs two stores per element and closes both leaks without touching SMmod.
(`ET` unconditionally assigns both on its own path, so the pre-zeroing is
behaviour-preserving there.)

### C2 — `K = 0` skips the cell loop and leaves `ESOIL` and `AE` stale — *medium*

`ET:492-604`. `K = NRD(N)`, raised to `MAX(top_cell_no - NHBED(IL,ITYPE), K)`
only for bank elements. For an ordinary land element with `NRD(N) == 0` the loop
`DO KK = 1, K` is zero-trip, so:

- `ESOIL` is never assigned (`:602` is inside the loop, guarded by
  `II == top_cell_no`, which is the `KK = 1` iteration),
- `AE` is never assigned,
- no `S(II)` is written.

`ETIN:775` then exports `ESOILA(IEL) = ESOIL / 1000.0D0` — the previous
element's soil evaporation, applied to this element's area. `ESOILA` reaches
`VSmod:4377` as the top-cell sink and `EEVAP` as the evaporation total, so this
is again a source-term error rather than a diagnostic one.

`NRD` is read verbatim from record `ET8` (`FRmod.f90:4883`) with no lower-bound
check I could find, so `NRD = 0` is reachable input. It is a legitimate
configuration — bare soil with no roots — and it is exactly the configuration
that breaks.

**Fix.** Hoist the soil-evaporation assignment out of the loop, where it belongs
anyway (see P4):

```fortran
AE    = ZERO          ! or compute the top cell's AE unconditionally
ESOIL = 0.5D0 * AE * (ONE - CPLAI)
```

placed before `DO KK = 1, K`, with the in-loop `IF (II == top_cell_no)` branch
deleted.

### C3 — mode 3 can use an undefined `FE` at an exact table boundary — *low*

`ET:556-579` versus `ET:528-552`. The two interpolators use different
comparisons on the *first* table row:

| Mode | Below-range test | `KF = 1` and `PSI4 == PS1(N,1) < 0` |
|---|---|---|
| 2 (`:537`) | `PSI4 <= PS1(N,1)` → `RC = RCF(N,1)` | handled |
| 3 (`:564`) | `PSI4 <  PS1(N,1)` → `FE = FET(N,1)` | **falls through** |

With `KF = 1`, mode 3's `ELSE` branch requires simultaneously
`PSI4 >= PS1(N,1)` and `PSI4 <= PS1(N,KF) == PS1(N,1)`, i.e. exact equality.
The search loop `DO KL = 2, KF` is then `DO KL = 2, 1` — zero-trip — and `FE`
is used at `:581` holding whatever the previous cell or previous element left in
it. `NF = 1` is accepted input (`FRmod.f90:5005`, `N1 = NF(I)`), and the
manual's intent is that a single row covers all negative tensions.

The same asymmetry does *not* bite mode 2: its `<=` makes the `KF = 1` fall-
through unreachable by construction. So this is a one-character inconsistency,
not a design flaw.

**Fix.** Change `:564` from `PSI4 (II) < PS1 (N, 1)` to
`PSI4 (II) <= PS1 (N, 1)`, matching mode 2.

Adjacent equal `PS1` rows still divide by `DPS1 = 0` in both modes (`:545`,
`:573`); that belongs in `INET`'s validation, not here.

### C4 — `PSI4` below `NLYRBT(IEL,1)` is the previous element's data — *low, unguarded*

`ETSIM:910-912` copies only the active range:

```fortran
ICE = NLYRBT (IEL, 1)
CALL DCOPY (top_cell_no - ICE + 1, VSPSI (ICE, IEL), 1, PSI4 (ICE), 1)
```

`ET`'s loop descends to `II = top_cell_no - K + 1`, and `K` is clipped only
against the *array* bound (`:504`, `top_cell_no - K < 0`), never against `ICE`.
For a bank element `K = MAX(top_cell_no - NHBED(IL,ITYPE), NRD(N))`, so the
deepest cell touched is `MIN(NHBED+1, top_cell_no-NRD+1)`. If either falls
below the column base, mode 1/2/3 branches on a pressure head belonging to a
different column.

I could not confirm from the source whether `NHBED + 1 >= NLYRBT(IEL,1)` and
`NRD <= top_cell_no - NLYRBT(IEL,1) + 1` are guaranteed invariants — they are
plausible for well-formed meshes (the aquifer base is normally well below the
channel bed). So I report this as an *unguarded assumption*, not a confirmed
defect. Note that the eliminate-`PSI4` refactor in **P5** removes the class of
error entirely, at no cost.

### C5 — division by zero is evaluated before the zero-capacity repair — *low*

`ET:447-457`:

```fortran
CT1 = CSTOLD + DTUZ * CPLAI * precip_m_per_s(IEL) * 1000.0D0
F1  = MIN (CT1 / CSTCAP (N), ONE)          !  <-- CSTCAP may be exactly 0

IF (LEZERO(CSTCAP(N))) THEN                !  <-- repair runs afterwards
   ...
END IF
```

`INET` initialises `CSTCAP(I) = 0.0D0` (`FRmod.f90:4825`) and then reads it from
`ET8` with no positivity check, and the 2007 comment at `:418` states plainly
that `CSTCAP = 0` is expected input. The division therefore produces `±Inf`
(or `NaN` for `CT1 = 0`) before being overwritten.

In the current build this is benign: `CMakeLists.txt` sets `-O2` / `-O3
-march=native` with no `-ffpe-trap`, so the IEEE flag is raised and ignored,
and `F1` is correctly replaced. It becomes a hard crash the moment anyone builds
with `-ffpe-trap=zero,invalid` for debugging — which is precisely when you least
want a spurious abort.

**Fix.** Reorder into a single branch:

```fortran
IF (LEZERO(CSTCAP(N))) THEN
   F1 = MERGE (ZERO, ONE, LEZERO(CT1))
ELSE
   F1 = MIN (CT1 / CSTCAP(N), ONE)
END IF
```

Behaviour-identical, no spurious exception, one fewer division on the
zero-capacity path.

### C6 — the `Q > 0` canopy branch overflows and cancels unnecessarily — *low to medium*

`ET:424-428`:

```fortran
CALC = CB (N) * (CSTOLD - CSTCAP (N) + DTUZ * Q)
DUM  = CB (N) * (CSTOLD - CSTCAP (N))
CALC = CALC - LOG (CK (N) * EXP (CALC) - CK (N) * EXP (DUM) + Q)
CSTORE (IEL) = MAX (ZERO, CSTCAP (N) + (LOG (Q) + CALC) / CB (N))
```

Write `D = CB*(CSTOLD - CSTCAP)` and `x = CB*DTUZ*Q`, both `>= 0` in this
branch (`CSTOLD > CSTCAP` and `Q > 0`). The expression is

```
CSTORE = CSTCAP + ( LOG(Q) + D + x − LOG( CK·e^D·(e^x − 1) + Q ) ) / CB
```

Two independent numerical problems:

1. **Overflow.** `EXP(D)` overflows to `+Inf` for `D > ~709`, giving
   `Inf − Inf = NaN` and a `NaN` canopy store that then propagates into `DRAIN`,
   `PNET`, `PNETTO` and the VSS source term. The comment at `:414-415` explicitly
   warns that "the canopy storage is often greater than canopy storage capacity",
   so a large `CSTOLD − CSTCAP` is the documented normal case, and `CB` is a
   free input parameter with no upper bound checked in `INET`.
2. **Cancellation.** For small supply (`x → 0`) the term
   `CK·e^{CALC} − CK·e^{DUM}` is a difference of two nearly equal large numbers.
   Relative error grows without bound as `x → 0`, which is the common
   light-drizzle case.

Both are fixed by factoring `e^D` out of the logarithm and using `EXPM1`:

```
CSTORE = CSTCAP + ( LOG(Q) + x − LOG( CK·expm1(x) + Q·e^{−D} ) ) / CB
```

`e^{−D}` now *underflows* to zero harmlessly instead of overflowing, `expm1`
is exact for small `x`, and the argument of the outer `LOG` is still strictly
positive (`CK >= 0`, `expm1(x) > 0`, `Q > 0`). The count of transcendentals
drops from 3 `EXP` + 2 `LOG` to 1 `EXP` + 1 `EXPM1` + 2 `LOG`. Fortran 2008
provides no intrinsic `EXPM1`; gfortran and ifx both expose the C library
`expm1` via `ISO_C_BINDING`, or a three-term series below `|x| < 1e-5` suffices
for this application.

The sibling branches at `:438-441` and `:464-466` use `EXP(−CB*(CT1−CSTCAP))`
with a **negative** argument, so they underflow safely and need no change —
except that `LOG(DTUZ*CB*CK + XPSTOR)` is `LOG(0) = −Inf` when `CK = 0` and
`XPSTOR` has underflowed, which yields `CSTORE = +Inf`. That requires
`CB*(CT1−CSTCAP) > 745` *and* `CK = 0` (no drainage); rare, but a one-line
guard on `CK` in `INET` would retire it.

### C7 — `PNET = zero` in `ETIN` is applied after `PNETTO` has been written — *low*

`ETIN:770` writes `PNETTO(IEL) = PNET / 1000.0D0`. `ETIN:805-806`, inside the
"surface water fully evaporated" branch, then executes:

```fortran
HRUZ = zero
PNET = zero
```

Neither store affects this element. `HRUZ` is unconditionally recomputed for
the next element at `ETSIM:909`, and `PNET` has already been exported. The only
way `PNET = zero` can be observed is by leaking into the *next* element through
the stale path of C1 — which is presumably not the intent.

So line `:806` is either dead, or it is a mis-ordered attempt to write
`PNETTO(IEL) = zero` (and `HRUZ` likewise dead). I cannot tell from the source
which was meant. Worth resolving deliberately rather than leaving as-is; if the
intent was to suppress net input when all surface water evaporates, the
assignment belongs before `:770` or must target `PNETTO(IEL)` directly. Note
that fixing C1 changes nothing here, but does make the current line
unambiguously dead.

### C8 — unguarded denominators — *informational*

Collected for completeness; most belong in `INET` validation rather than in the
hot path.

| Divisor | Site | Guarded? |
|---|---|---|
| `RA(N)` | `:396` | yes — fatal 4998 at `:392` |
| `RA(N)` | `:525`, `:554` (modes 1, 2) | **no** — 4998 only covers the unmeasured-PE path, so measured `PE` + mode 1/2 divides unchecked |
| `CB(N)` | `:427`, `:440`, `:466` | no |
| `DPS1` | `:547`, `:574` | no |
| `CSTCAP(N)` | `:448` | repaired *after* use (C5) |
| `DTUZ` | `:482`, `:766`, `:772`, `:773`, `:796` | no |
| `DELTAZ(II,IEL)` | `:595`, `:817` | `:595` only via the `NOTZERO(DUM)` shortcut |
| `cellarea(IEL)` | `:781` | no |
| `BWIDTH` | `:896` | no |
| `EINT` | `:472` | no — but `CT1 < 0` implies `EINT > 0` here |

The `RA` row is the one worth acting on: the 4998 check at `:392` sits inside
the `MEASPE(MS) == 0` branch, yet `RA(N)` is divided by again at `:525`/`:554`
regardless of `MEASPE`. Hoisting the check above the `IF (MEASPE...)` at `:386`
costs one comparison per element and closes the gap. The module header already
records this at `:302-308`.

---

## 2. Performance

### P1 — the root-depth warning formats a message on every element, every step — *medium*

`ET:504-511`:

```fortran
IF (top_cell_no - K < 0) THEN
   K = top_cell_no
   WRITE(msg,'(A)') 'root zone extends below aquifer bed. ...'   ! <-- unguarded
   IF (first) THEN
      CALL ERROR(WWWARN, 4999, pppri, 0, 0, msg)
      first = .FALSE.
   END IF
END IF
```

The `WRITE` is outside the `first` guard. An internal formatted `WRITE` is on
the order of 100 ns–1 µs — it goes through the runtime's format interpreter and
touches a 132-byte module-level buffer — so once any vegetation type has
`NRD > top_cell_no`, this executes once per land element per timestep for the
remainder of the run and the result is discarded on all but the first call.

Note also that the string is a compile-time constant, so the `WRITE` serves no
purpose at all.

**Fix.** Move the message inside the guard, or better, pass the literal:

```fortran
IF (top_cell_no - K < 0) THEN
   K = top_cell_no
   IF (first) THEN
      CALL ERROR(WWWARN, 4999, pppri, 0, 0, &
                 'root zone extends below aquifer bed. Values below aquifer bed are ignored')
      first = .FALSE.
   END IF
END IF
```

That also removes the last use of the module-scoped `msg` from this routine's
warning path (see M4).

### P2 — the `S` array is written on every root cell and never read — *medium*

`ET:594-598` and `ETIN:817`:

```fortran
IF (NOTZERO(DUM)) THEN
   S (II) = DUM / DELTAZ (II, IEL)
ELSE
   S (II) = ZERO
END IF
...
S (top_cell_no) = S (top_cell_no) + ESOILA (IEL) / DELTAZ (top_cell_no, IEL)
```

`S` is `AL_D.f90:225`, `DOUBLEPRECISION :: S(LLEE)`. Every appearance of `S`
in the whole tree is a write:

| Site | Operation |
|---|---|
| `ETmod.f90:595`, `:597` | write |
| `ETmod.f90:817` | read-modify-write of a value nothing consumes |
| `SMmod.f90:647` | write (`S(KK) = zero`) |
| `run_sim.f90:87` | `USE`d under the `!NEEDED ONLY FOR AD` comment; no reference in the body |

The real VSS source term is built independently in `VSmod.f90:4368-4377` from
`ERUZ` and `ESOILA`, not from `S`. So the entire `S` computation is dead: one
division and one branch per root cell per element per timestep, plus a division
and a read-modify-write per element in `ETIN`.

**Fix.** Delete `:594-598` and `:817`. If `S` must be retained for the AD build,
guard it behind the same preprocessor/comment convention the other AD-only state
uses rather than paying for it in the production path.

Incidentally, this also neutralises a latent bug: `ETIN:817` accumulates onto
`S(top_cell_no)`, which is stale whenever C2 fires or whenever `SMET` ran
(`SMET` zeroes `S(1:NRD)` — bottom-indexed — while `ET` writes
`S(top_cell_no-K+1 : top_cell_no)`, so the two never touch the same cells).
Deleting the dead code is strictly better than fixing the indices.

### P3 — `ERUZ`'s element-major layout costs on both sides

See **M1** below; the layout is a memory-management decision but its cost is
almost entirely a performance one.

### P4 — the mode dispatch and `RC(N)` write sit inside the per-cell loop — *low to medium*

`ET:514-604`. `M1 = MODE(N)`, `N`, `MS`, `PE`, `TOP`, `CPLAI`, `F1` and `RA(N)`
are all loop-invariant, yet each iteration re-tests `M1 /= 2 .AND. M1 /= 3`,
re-loads `NF(N)`, and — in mode 2 — **writes `RC(N)`**, a module-level
allocatable, at `:536`, `:538`, `:540`, `:548`.

The `RC(N)` write is the expensive part. It is a store to memory the compiler
cannot prove is disjoint from `RCF`, `PS1`, `RA` or `DEL` (all module
allocatables reached through descriptors), so it forces a reload of everything
around it, defeats register allocation for `RC`, and creates a loop-carried
memory dependence that blocks vectorisation of a loop that is otherwise
call-free. `LLEE = 50`, so the trip count is small, but the loop runs
`total_no_elements` times per timestep.

**Fix.** Two independent changes, both cheap:

1. Make the mode-2/3 interpolation write a *local* scalar:

   ```fortran
   DOUBLE PRECISION :: rc_cell
   ...
   rc_cell = RCF (N, KF)      ! etc.
   AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE + rc_cell / RA (N))))
   ```

   `RC(N)` is not written to the restart file (I checked `FRmod.f90:3140-3200`),
   and mode 1 reads `RC(N)` only for mode-1 vegetation types, so nothing observes
   the current clobbering except the `INET` echo in the print file. This is
   therefore a pure win: it restores `RC` to the read-only input parameter it is
   documented to be *and* removes the loop-carried store.

2. Hoist the dispatch: `SELECT CASE (M1)` outside the loop with three
   specialised `DO KK = 1, K` bodies. Duplicates ~15 lines; removes a
   correctly-predicted branch per cell but, more usefully, lets each body
   vectorise independently.

Also hoist the loop-invariant `LAMDA * (DEL(MS) + GAMMA * (ONE + rc/RA(N)))`
denominator for mode 1 — in mode 1 `RC(N)` is genuinely constant, so `AE` takes
only two distinct values across the whole column (`PE` when `PSI4 >= 0`, the
Penman–Monteith value otherwise). Computing both once before the loop reduces
mode 1 to a select.

### P5 — `UZALFA` is fully rewritten for every element although it is zero for most — *low*

`ETSIM:894-907`. For an ordinary land element (`ITYPE` neither 1 nor 2 — the
overwhelming majority in any catchment) the code sets `ICE = 1` and then
executes `UZALFA (1 : top_cell_no) = ZERO`. `ET:588` then divides by
`ONE + UZALFA(II)` = exactly 1.0.

That is up to 400 bytes of stores per element per timestep purely to re-assert
zero, plus a redundant division per root cell. For 10⁵ elements that is ~40 MB
of stores per timestep for no information content.

**Fix.** Track whether the buffer is already zero:

```fortran
LOGICAL :: uzalfa_is_zero = .FALSE.   ! module or ETSIM-local with SAVE
...
IF (ITYPE == 1 .OR. ITYPE == 2) THEN
   ... existing bank code ...
   uzalfa_is_zero = .FALSE.
ELSE IF (.NOT. uzalfa_is_zero) THEN
   UZALFA (1 : top_cell_no) = ZERO
   uzalfa_is_zero = .TRUE.
END IF
```

Bank elements are contiguous in neither direction, so the win depends on the
element ordering, but the common case (long runs of ordinary elements) collapses
to a single flag test. Better still, pass a `bank` logical into `ET` and skip
the `/(ONE + UZALFA(II))` division entirely for ordinary elements.

### P6 — `DCOPY` for the pressure-head staging, and the staging itself — *low*

`ETSIM:912` calls `UTILSMOD:DCOPY` with `incx = incy = 1`, which reduces to
`dy(1:n) = dx(1:n)` (`utilsmod.f90:78`). The source is a contiguous column slice
of the cell-major `VSPSI`, so no copy-in temporary is created, but this is still
a cross-module call with a descriptor-free assumed-size interface per element
per timestep. `DCOPY` is `PURE`, so IPO (enabled — `check_ipo_supported` at
`CMakeLists.txt:73`) can inline it, but only in an IPO build.

More to the point, the copy exists only so `ET` can index `PSI4(II)`. `PSI4` is
`PUBLIC` with the comment `!THESE NEEDED ONLY FOR AD` (`:137`), yet a tree-wide
search finds **no reference to `PSI4` or `UZALFA` outside `ETmod.f90`**. The
export is vestigial.

**Fix.** Pass `IEL` into the cell loop's pressure-head reads and delete the
staging:

```fortran
! ETSIM: remove the DCOPY entirely
! ET:522, :535, :562  ->  VSPSI (II, IEL)
! ETIN:799            ->  VSPSI (top_cell_no, IEL)
```

`VSPSI` is `(top_cell_no, total_no_elements)`, so `VSPSI(II,IEL)` walking `II`
downward is contiguous — the same access pattern the `PSI4` copy had, minus the
copy. This removes ~400 bytes of copy traffic per element per timestep, one
cross-module call, one allocatable, and finding **C4** simultaneously.

If the AD build genuinely needs `PSI4`, keep the array and the copy behind the
AD guard rather than in the shared path.

### P7 — `ICMREF` and `NHBED` are looked up twice per bank element — *low*

`ETSIM:892` computes `ITYPE = ICMREF(IEL,1)`; `ETSIM:895` computes
`IL = ICMREF(IEL,4)`; `ETSIM:897` reads `NHBED(IL,ITYPE)`. `ET:498-501` then
recomputes all three for the same element.

`ICMREF` is `(NELEE, 12)` — element-major — so columns 1 and 4 are 2 MB apart
(`NELEE = 250 000 × 4 bytes`). Each lookup is its own cache line and, at that
separation, its own page. Doing it twice doubles the streams for no gain.

**Fix.** `ET` is `PRIVATE` and has exactly one caller. Extend its interface:

```fortran
SUBROUTINE ET (IEL, ITYPE, IL)
```

and pass the values `ETSIM` already holds. Zero risk, removes two element-major
strided loads per bank element per timestep.

(The broader observation — that `ICMREF`'s `(element, field)` layout makes
every multi-column access on one element a multi-page walk — is already owned
by `analysis_frmod.md` §2.3 and `analysis_ocmod2.md` M2 and is not re-litigated
here.)

### P8 — minor items

- `precip_m_per_s(IEL) * 1000.0D0` is written three times (`:405`, `:411`,
  `:447`). There are no calls or aliasing writes between them, so any compiler
  at `-O2` will CSE this; it is a readability item only.
- `ETIN:747-748` computes `MS` and `MR`, and `ET:371` computes `MR`, none of
  which are used. Dead-store elimination removes them; they cost nothing but
  they mislead the reader (and the declarations at `:348`, `:740-741` document
  them as "retained from legacy code but otherwise unused").
- `ETIN` calls `SMIN` up to twice per element, and `SMIN` unconditionally calls
  `INITIALISE_SMMOD` (`SMmod.f90:701`), whose body is a saved-flag test. Two
  cross-module calls per element per timestep to test a boolean. Hoisting
  `INITIALISE_SMMOD` to the same place `INITIALISE_ETMOD` is called would retire
  it. This is SMmod's issue, noted here because ETmod is the caller.
- The `IF (NOTZERO(DUM))` guard at `:594` is *good* and should be kept if `S`
  is retained for AD: `DUM` is exactly zero whenever `HRUZ > 0` or
  `RDF(N,KK) = 0`, which is the common case, so the branch skips a division far
  more often than it mispredicts.

---

## 3. Memory and variable management

### M1 — `ERUZ` should be cell-major — *high*

`ERUZ` is declared `(total_no_elements, top_cell_no)` (`AL_C.F90:159`,
allocated at `:190`). It is the only element-major array in the ET→VSS data
path; `DELTAZ`, `VSPSI`, `VSTHE` and `CQ` are all cell-major.

**Producer side.** `ET:592` writes `ERUZ(IEL, II)` inside the `KK` loop. The
inner index is `II`, the *second* dimension, so consecutive iterations are
`total_no_elements × 8` bytes apart. For 10⁴ elements that is an 80 KB stride;
for 10⁵, 800 KB. Each of the `K` (≈ 5–20) writes therefore touches its own cache
line and, at those strides, its own page — so a loop that modifies `K × 8` bytes
of useful data pulls in `K × 64` bytes and burns `K` TLB entries. Write-allocate
makes it worse: each store fetches a line it will overwrite only 1/8 of.

**Consumer side.** `VSmod.f90:4373-4375`:

```fortran
DO ICL = ICDUM, ICTOP
   CQ (ICL, IEL) = -ERUZ (IEL, ICL) * CA0
END DO
```

`CQ` is `(LLEE, NELEE)` (`VSmod.f90:4243`) — cell-major. So this is a
full transpose-shaped access: the destination walks contiguously while the
source strides by `total_no_elements`. One of the two sides is guaranteed
cache-hostile *no matter what*, given the current layouts. Three further
consumer sites have the same source stride: `VSmod.f90:2998` (VSS mass
balance), `rest.f90:286` (water balance), and `CMmod.f90:1572`
(`TRAN1(NAQU:NCETOP) = ERUZ(NCL, NAQU:NCETOP)`, a strided gather).

**Fix.** Change the declaration and allocation to
`ERUZ(top_cell_no, total_no_elements)` and swap the subscripts at the five
call sites (`ETmod.f90:592`, `VSmod.f90:2998`, `:4374`, `rest.f90:286`,
`CMmod.f90:1572`). After the swap:

- `ET`'s column write becomes contiguous — `K × 8` bytes over 1–2 cache lines,
- `VSmod:4374` becomes a contiguous-to-contiguous copy that the compiler can
  vectorise or turn into a scaled `memcpy`,
- `CMmod:1572` becomes a contiguous slice copy,
- `rest.f90:286` becomes a contiguous reduction.

Every consumer improves; none regresses. This is the single highest-value change
in this document, and it is mechanical.

(Separately: `CQ(LLEE, NELEE)` at `VSmod.f90:4243` is a ~100 MB routine-local
array. That is VSmod's problem, not ETmod's, but it is worth flagging while the
`ERUZ` change is being made, since the two are touched together.)

### M2 — `DEL` is deliberately over-sized to paper over a domain confusion — *low*

`INITIALISE_ETMOD:176` allocates `DEL(MAX(NV, NM, NRAIN))`, and the header at
`:157-160` explains this as intentional: "`METIN` writes it by
meteorological-site index, while the three active counts can differ."

Checking `METIN` directly (`rest.f90:852`, `:866`, `:903`, `:916`), every write
to `DEL` is `DEL(I)` inside a `DO I = 1, NM` loop. `ET:385` and `:525`/`:554`
read `DEL(MS)` where `MS = NMC(IEL)`, the meteorological-site index. So `DEL` is
written and read purely over `1:NM`; `NV` and `NRAIN` never index it.

The `MAX` is therefore not "sizing for the largest active legacy index domain" —
it is defensive padding for an indexing confusion that does not exist in the
current code. The 2026-05-03 history entry ("Expanded `DEL` to `MAX(NV,NM,NRAIN)`
to avoid undersizing the meteorological domain") suggests a real bug was
observed; if so, it was somewhere other than `METIN`'s `DEL` writes, and the
padding is masking rather than fixing it.

**Recommendation.** Size it `DEL(NM)` and let a bounds-checked build (`-fcheck=bounds`)
say whether anything actually indexes it outside `1:NM`. If something does, that
is the bug to fix. Keeping the `MAX` guarantees the symptom stays hidden. The
memory itself is irrelevant (a few hundred KB at most); the concealment is the
cost.

### M3 — module-level scalars used as per-element scratch — *design, high leverage*

The following are single scalars in `AL_D` carrying one element's state at a
time: `PE`, `EINT`, `PNET`, `DRAIN`, `ERZ`, `ESOIL`, `AE`, `CSTOLD`, `CPLAI`,
`HRUZ`, `NSMT`. ETmod adds `RA(N)`, `RC(N)`, `PSI4(:)`, `UZALFA(:)` and `msg`.

Three concrete consequences, all already evidenced above:

1. **Every "stale value" bug in §1 is a direct consequence** — C1 (`DRAIN`,
   `PNET`), C2 (`ESOIL`, `AE`), C3 (`FE`, though that one is loop-local), C7
   (`PNET`). With per-element locals none of them could occur; the compiler
   would flag the use of an unset variable.
2. **The element loop cannot be parallelised.** `ETSIM:891-916` is otherwise a
   textbook `DO CONCURRENT` candidate: independent elements, no reductions
   except `TIMEUZ` (hoisted), no I/O on the fast path. The shared scalars are
   the only obstacle. Given that ET is called once per timestep for every land
   element and each element's work is a few hundred flops plus a handful of
   transcendentals, this is a real missed opportunity.
3. **Cross-module coupling is invisible.** `ETIN` depends on `SMET` assigning
   particular scalars, and there is no signature to check that against — which is
   exactly how C1 survived.

**Recommendation.** Not a one-commit change, but the incremental path is clear:
introduce a derived type holding the per-element ET state, make `ET` and `ETIN`
take it as `INTENT(INOUT)`, and keep the `AL_D` scalars as a thin write-back at
the end of `ETIN` for the modules that still read them. Doing this for `PNET`
and `DRAIN` alone would retire C1 permanently rather than by the defensive
zeroing suggested there.

### M4 — `msg` is module-scope shared state — *low*

`ETmod.f90:131`, `CHARACTER(132) :: msg`, used only in `ET` (`:393`, `:506`).
It is documented as a "shared private warning/fatal diagnostic buffer", but
there is no reason for it to outlive the routine, and its module scope makes
`ET` non-reentrant on the warning path (relevant to M3's parallelisation goal).

**Fix.** Make it a local in `ET`. After P1 removes the `:506` write, only the
fatal path at `:393` remains, which executes at most once per run.

### M5 — `NVEE`-sized static arrays — *low impact, worth doing for consistency*

Eleven of ETmod's module arrays are still fixed at `NVEE = 250 000`:

| Array | Type | Static size |
|---|---|---|
| `BAR` | `LOGICAL` | 1 MB |
| `MODE`, `NF`, `MEASPE`, `MODECS`, `MODEPL`, `MODECL`, `MODEVH`, `NCTCST`, `NCTPLA`, `NCTCLA`, `NCTVHT` | `INTEGER` | 11 × 1 MB |

≈ 12 MB of `.bss` for arrays whose used extent is `NV` (typically well under
100) or `NM`. The other 24 arrays were already converted to run-sized
allocatables by `INITIALISE_ETMOD`; these eleven were not.

Be honest about the cost: untouched `.bss` pages are never faulted in, so the
resident-set impact is near zero, and the *used* entries (indices `1:NV`) stay
resident in L1 across the whole element loop. The real arguments for converting
them are consistency with the rest of the module, and the fact that `MEASPE` is
indexed by meteorological site while its neighbours are indexed by vegetation
type — a domain confusion that a correctly-sized allocation would expose.

Note also that `MEASPE(NVEE)` is declared `INTEGER :: MEASPE(NVEE) !! ... by
meteorological site` and read at `:386` as `MEASPE(MS)` — correct — but it sits
in a block otherwise indexed by `N`. Sizing it `NM` and the rest `NV` documents
the distinction in the code rather than in a comment.

### M6 — `INITIALISE_ETMOD` has no `STAT=` and no deallocator — *low*

`:175-208`. Twenty-four unconditional `ALLOCATE`s in ten statements, no `STAT=`,
no matching `FINALISE_ETMOD`, and a second call would abort in the runtime.
The header documents this at `:162-165`.

For a single-run batch executable this is defensible. It becomes a defect the
moment the code is used as a library, driven from a test harness that runs more
than one catchment in a process, or wrapped for calibration/ensemble runs — all
of which are plausible for this codebase. A guard costs three lines:

```fortran
IF (ALLOCATED(RA)) RETURN     ! or: CALL FINALISE_ETMOD()
```

The `PS1`/`FET`/`RCF` allocations are `NV × NUZTAB` = `NV × 20`; note that
`INET` reads `N1 = NF(I)` rows (`FRmod.f90:5005`) with no check that
`NF(I) <= NUZTAB`. `ET:534`/`:561` then index `PS1(N, KF)` with `KF = NF(N)`.
ETmod trusts that bound; it is `INET`'s to enforce.

### M7 — dead code: `ETCHK2` and `RDL` — *cleanup*

`ETCHK2` (`:645-661`) is `PRIVATE` and has no caller anywhere in the tree. Its
`ALCHK` call tests `RDL == 0` exactly, which — as the header notes at `:625-630`
— rejects every value the manual defines as meaningful for record `ET8`.

`RDL` itself (`AL_C.F90:128`) is read by `INET` (`FRmod.f90:4883`), echoed to
the print file, written to the restart file (`FRmod.f90:3177`), and **never used
in any calculation**. The physics it was meant to control — the share of bank
transpiration drawn from the channel rather than the soil — is instead expressed
through `UZALFA`'s `1/(1 + ALFA)` weighting at `ET:588`, which *reduces* soil
uptake for cells at or below the bed without adding the complement anywhere. The
result is that bank vegetation near the channel simply transpires less than its
potential demand, with no stream abstraction to match.

Whether that is intended physics or a half-removed feature I cannot determine
from the source. Mass balance still closes (`ERZ` accumulates the same reduced
`DUM` that `ERUZ` receives), so this is not a conservation error — but it is
worth a deliberate decision rather than continued drift. Either way, `ETCHK2`
should be deleted; it cannot be called and would reject valid input if it were.

### M8 — `RA` and `RC` are input parameters used as scratch — *low*

`ET:377`/`:379` writes `RA(N)`; `ET:536`-`:548` writes `RC(N)`. Both are read
from record `ET8` by `INET` (`FRmod.f90:4882`) and both are documented as
per-vegetation *parameters*.

I checked `FRmod.f90:3140-3200`: neither is written to the restart file, so
there is no hot-start correctness consequence, and within a single element the
read-after-write ordering is consistent. So this is not a bug today. It is,
however:

- a silent corruption of the input record in memory (any later diagnostic that
  echoes `RA`/`RC` reports the last element's interpolated value),
- a blocker for M3's parallelisation, and
- in `RC`'s case, a measurable cost inside the hot loop (see P4).

The `RC` fix in P4 (write a local scalar) resolves both. For `RA`, computing
`ra_cell = RTOP(N) / U(MS)` into a local and using it throughout `ET` does the
same at zero cost.

---

## 4. Priority summary

| # | Finding | Class | Severity | Effort | Risk |
|---|---|---|---|---|---|
| C1 | `DRAIN`/`PNET` leak between elements on the snow path | correctness | high | 2 lines | very low |
| M1 | `ERUZ` element-major; transpose to `(cell, element)` | memory/perf | high | 5 call sites | low, mechanical |
| P2 | `S` written on every root cell, never read | perf | medium | delete 6 lines | very low |
| P1 | `WRITE(msg,...)` outside the `first` guard | perf | medium | move 1 line | none |
| C2 | `NRD = 0` leaves `ESOIL`/`AE` stale | correctness | medium | hoist 1 line | low |
| P4 | mode dispatch + `RC(N)` store inside the cell loop | perf | medium | ~20 lines | low |
| C6 | `EXP` overflow/cancellation in the `Q > 0` canopy branch | correctness | medium | rewrite 4 lines | medium — changes results in the tails |
| P6 | eliminate the `PSI4` staging copy (also fixes C4) | perf/memory | low–medium | 5 call sites | low |
| P5 | `UZALFA` re-zeroed for every ordinary element | perf | low | ~6 lines | low |
| C8 | `RA` guard does not cover the measured-`PE` path | correctness | low | hoist the check | very low |
| P7 | `ICMREF`/`NHBED` looked up twice per bank element | perf | low | change `ET`'s signature | very low |
| C5 | division evaluated before the zero-capacity repair | robustness | low | reorder | none |
| C3 | mode 3 `<` should be `<=` (undefined `FE` at `NF = 1`) | correctness | low | 1 char | very low |
| C7 | `PNET = zero` after `PNETTO` is written | correctness | low | needs a decision | — |
| M2 | `DEL(MAX(NV,NM,NRAIN))` masks a non-existent domain issue | memory | low | 1 line + a bounds-checked run | low |
| M8 | `RA`/`RC` used as scratch | design | low | folded into P4 | very low |
| M4 | `msg` at module scope | design | low | 1 line | none |
| M5 | eleven `NVEE`-sized static arrays | memory | low | ~15 lines | low |
| M6 | no `STAT=`, no deallocator, no re-entry guard | robustness | low | 3 lines | none |
| M7 | `ETCHK2` and `RDL` are dead | cleanup | low | delete | none |
| M3 | module scalars as per-element scratch | design | high leverage | incremental | medium |

A reasonable first commit is **C1 + P1 + P2 + C2 + C3 + C5**: all six are
small, independent, and low-risk, and together they close two mass-balance leaks
and remove two dead computations from the hot path. **M1** deserves its own
commit because it touches four other modules. **C6** should be validated against
reference output before merging, since it deliberately changes results in the
numerical tails.

---

## 5. Checked and found correct

Recorded so these do not get re-audited.

- **Loop bounds.** `ETSIM:891` iterates `NGDBGN : total_no_elements`, i.e. land
  elements only; channel links (`1 : NGDBGN-1`) are correctly excluded, and
  `ET`'s bank handling reads `ICMREF(IEL,4)` only under
  `ITYPE == 1 .OR. ITYPE == 2`.
- **`K` clipping.** `ET:504`'s `IF (top_cell_no - K < 0)` is the correct guard
  for the array lower bound: the deepest cell reached is
  `II = top_cell_no - K + 1`, so `K <= top_cell_no` keeps `II >= 1`. (It does
  *not* guard against the column base — that is C4.)
- **`RDF` beyond the root zone.** `AL_C.F90:278-279` allocates `RDF(NV,LLEE)`
  and zeroes it, and `INET` fills only `1:NRD(I)`. So the bank-element loop
  extension (`K` raised to `top_cell_no - NHBED`) reads `RDF(N,KK) = 0` for
  `KK > NRD` and correctly contributes nothing to `ERZ`. No uninitialised read.
- **`UZALFA` bounds.** `ETSIM:900-901` writes `UZALFA(1 : NHBED)` and
  `UZALFA(NHBED+1)`. Given the invariant that `NHBED` is a valid cell index with
  `NHBED + 1 <= top_cell_no` — which every other consumer assumes
  (`CMmod.f90:1225`, `:1422`, `:2287`) — both are in range, and the
  `IF (ICE <= top_cell_no)` guard at `:907` correctly handles
  `ICE = top_cell_no + 1`.
- **Mode 2's interpolation loop cannot fall through.** Its `ELSE` branch implies
  `PS1(N,1) < PSI4 <= PS1(N,KF)`, which forces `KF >= 2` and guarantees the
  `DO KL = 2, KF` search succeeds. All four mode-2 paths assign `RC`. Only
  mode 3 has the hole (C3).
- **`MODE = 4`.** `INET` (`FRmod.f90:4999`) treats `MODE` 1 and 4 alike as
  constant-`RC`, and `ET:518`'s `IF (M1 /= 2 .AND. M1 /= 3)` routes 4 to the
  mode-1 branch. Consistent, despite the manual documenting only modes 1–3.
- **Unit conversions.** The mm↔m and per-step↔per-second conversions in
  `ETIN:770-775` are dimensionally consistent with the mm/s and mm conventions
  `ET` establishes, and the 2026-04 change to strict `1000.0D0` literals removed
  the single-precision constants correctly.
- **`DCOPY` semantics.** With `incx = incy = 1` it takes the
  `dy(1:n) = dx(1:n)` path (`utilsmod.f90:77-78`), and the `INTENT(INOUT)` on
  `dy` is correct for the strided case. The `ETSIM` call passes a contiguous
  column slice, so no copy-in/copy-out temporary is created.
- **`ESOIL` half-factor.** `ET:602`'s `0.5D0 * AE * (1 - CPLAI)` matches the
  2015-05-27 history entry and the `AL_D` unit documentation (`ESOIL` in mm/s).
- **The `NOTZERO(DUM)` branch at `:594`** is correct and beneficial (see P8).
- **`ETSIM`'s time advance.** `DTUZ = UZNEXT * 3600` and
  `TIMEUZ = TIMEUZ + UZNEXT` are consistent with `UZNEXT` in hours and `DTUZ`
  in seconds as used throughout `ET`/`ETIN`.
