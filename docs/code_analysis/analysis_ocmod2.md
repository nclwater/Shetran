# Logical analysis: `OCmod2` — correctness, memory/stride, accuracy

## Scope and method

Source-only assessment of `src/modules/OCmod2.f90` (1919 lines). No profile was
taken and no timings were measured. Every claim is derived from reading the
module together with:

- its only hot caller, `src/modules/OCQDQMOD.F90` (`OCQDQ`, lines `:150-345`),
- `src/modules/OCmod.f90` (`OCSIM` state advance and the `OCFIX` call site,
  `:2213-2252`; the `XSTAB` builder `OCXS`, `:2455-2505`),
- the array declarations and comparison helpers in
  `src/parameters/sglobal.f90`,
- `src/modules/FRmod.f90:291-379` for the `DXQQ`/`DYQQ`/`CWIDTH` geometry,
- the compiler flags in `CMakeLists.txt`,
- `git show 87a18ff`, `13d3d71`, `12d3945` for the behaviour that changed in the
  version under investigation.

Where a claim depends on compiler behaviour rather than on the source alone,
that is stated.

This document deliberately does **not** re-report the two findings that
`analysis_ocmod.md` already owns from the caller side
(`M1`, the `XSTAB` size; `S6`, the OC-state staging round trips, since fixed by
`13d3d71`). It picks up the *consumption* side of `XSTAB` inside `OCCODE`, which
that document only touches in passing.

Relevant compiled extents, all from `sglobal.f90`:

| Constant | Value | Line |
|---|---|---|
| `nelee` | 250 000 | `:118` |
| `nlfee` | 20 000 | `:117` |
| `NXSCEE` | 100 000 | `:135` |
| `vsmall` (the `ISZERO` band) | `1.0e-20` | `:190` |

---

## Conclusion up front

Four findings dominate.

1. **`OCNODE`'s convergence test can never be satisfied once the residual hits
   exactly zero, and the mass-conservation correction is skipped whenever the
   iteration cap is reached.** Commit `87a18ff` (2026-05-21, in the version under
   investigation) deleted the `IF (ISZERO(FN))` early exit, raised the cap from
   50 to 200, and tightened both tolerances by 10×. The deleted early exit turns
   an exact hit into a 200-iteration stall on a fixed point; the tightened
   tolerance makes the cap far more reachable; and on the capped path the
   junction returns flows that do **not** sum to zero. Findings **C3**, **C4**,
   **P2**.

2. **`OCCODE`'s table index has no lower bound.** `I = INT((H/HFULL)*(NXSCEE-1)
   + 1)` is guarded only against `I >= NXSCEE`. A negative depth gives `I <= 0`
   and reads outside this link's table — silently, into the previous link's
   rows. `OCFIX` exists specifically because negative depths occur, so this is
   reachable, not theoretical. Finding **C1**.

3. **The face dimension of `qsazz` is the slowest-varying one.**
   `qsazz(NELEE,4)` puts the four faces of one element 2 MB apart. Every
   face-by-face loop over a single element in this module and in `OCSIM` touches
   four cache lines and four pages to read 32 bytes. Together with
   `afromICMREF(NELEE,12)`, `OCFIX`'s element loop runs **17 concurrent
   sequential streams**; transposing to `qsazz(4,NELEE)` and an
   element-contiguous `ICMREF(12,NELEE)` layout takes that to about seven.
   Finding **M2**.

4. **`NXSCEE = 100 000` buys derivative smoothness that `DZMIN = 1 mm` throws
   away.** The table resolves bank-full depth to ~50 µm while every consumer in
   the module smooths over 1 mm. Cutting `NXSCEE` by 100× moves the per-link
   table from 2.4 MB to 24 KB — from guaranteed-cold to L2-resident — with a
   piecewise-linear interpolation error still far below the smoothing floor.
   Finding **M1**.

The module is otherwise in good shape. §6 lists the things I checked and found
correct, so they do not get re-audited.

---

## 1. Correctness

### C1 — `OCCODE` table index is unguarded below (`:532-537`)

```fortran
H = Z - ZG
HFULL = AFROMXSTYPES(1, NXSCEE)
I = INT((H / HFULL) * DBLE(NXSCEE - 1) + ONE)

IF (I < NXSCEE) THEN
   DERIV = AFROMXSTYPES(3, I)
   CONV  = AFROMXSTYPES(2, I) + DERIV * DIMJE(H, AFROMXSTYPES(1, I))
```

The `IF` guards the *upper* end only. For `H < 0` the expression gives
`I <= 0` and the branch is taken, so `AFROMXSTYPES(3, I)` and
`AFROMXSTYPES(2, I)` are read out of bounds. With the column-major layout of
`XSTAB(3, NXSCEE, link)`, `XSTAB(3,0,L)` addresses one element *before*
`XSTAB(1,1,L)`, i.e. the tail of link `L-1`'s table — a plausible-looking
double, so nothing crashes and nothing is flagged in a `Release` build
(`-fcheck=bounds` is `Debug`-only, `CMakeLists.txt:694`).

Reachability: `OCFIX`'s `HSMALL` branch (`:1771`) exists precisely to repair
`HRF(iel) < ZGRUND(iel)`, and it only fires *after* `OCQDQ` has already run with
whatever state the solve produced. `OCCODE` is called from `OCQBC` (`:741`),
`OCQLNK` (`:1228`) and `OCQMLN` (`:1365`, `:1385`) with `Z` taken straight from
`GETHRF`. A single element that goes slightly negative inside a timestep is
enough.

Fix: `I = MAX(1, INT(...))`, or hoist the whole index computation into
`MIN(NXSCEE, MAX(1, ...))` and drop the branch's dependence on `I`'s sign.

Secondary, and much less likely: `INT()` of `(H/HFULL)*99999 + 1` overflows
default `INTEGER` once `H > ~21 475 * HFULL`. Not worth code, worth knowing.

### C2 — `CONVEYAN` returns undefined output for `ty` outside `{0,1,2}` (`:1454-1487`)

The three `IF`/`ELSE IF` arms have no `ELSE`. `CONV` and `DERIV` are
`INTENT(OUT)`, so an unrecognised `ty` returns whatever was in the caller's
variables. All present call sites pass a literal `0`, `1` or `2`
(`OCmod.f90:2498`, `OCmod2.f90:541`, `:739`, `:881`, `:1056`), so this is
latent. `CONVEYAN` is `PUBLIC` (`:85`), which is what makes it worth closing.

Related, and the reason a default arm matters more than usual here: `xa` and
`extra` are `OPTIONAL` and used without `PRESENT()` tests (the routine's own
`@warning` at `:1429-1432` says so). A wrong `ty` therefore does not just return
garbage, it can dereference an absent argument.

### C3 — `OCNODE` skips the mass-conservation correction on the capped path (`:319-352`)

```fortran
IF (ABS(FN) <= SIGMAQ * 1.0D-3 .AND. ABS(B - A) <= 1.0D-4) THEN
   JMAJOR = 0
   DO J = 1, 3
      IF (ABS(QJ(J)) > ABS(QJ(JMAJOR))) JMAJOR = J
   END DO
   QJ(JMAJOR) = QJ(JMAJOR) - FN     ! <-- only here
   FAILED = .FALSE.
   EXIT iteration_loop
END IF
```

`QJ(JMAJOR) = QJ(JMAJOR) - FN` is the 1999 fix (`SB 4.27`, history line `:259`)
that makes the returned branch flows sum to exactly zero. It runs **only** on the
converged exit. When the loop runs out at `NC = 200`, `QJ` is left as whatever
`FNODE` produced at the last trial elevation, and `SUM(QJ) = FN ≠ 0`: the
confluence creates or destroys water at a rate of `FN`, and warnings 1027/1028
are issued (`:348`, `:350`).

That was equally true before `87a18ff`, but the tolerances that decide how often
this path is taken were tightened by 10× in that commit:

| | residual test | bracket test | cap |
|---|---|---|---|
| before `87a18ff` | `|F| <= 1e-2 * Σ|Q|` | `|B-A| <= 1e-3` | 50 |
| now (`:319`, `:310`) | `|F| <= 1e-3 * Σ|Q|` | `|B-A| <= 1e-4` | 200 |

So the same junction that used to converge now has four times as many
iterations to reach a criterion ten times tighter. Whether the net effect is
more or fewer capped junctions is not determinable from the source — but it
directly determines both the cost (**P2**) and how often non-conservative flows
are returned. It needs measuring, not reasoning about.

The routine also documents `Σ_i QJ_i = 0` as an exit condition of `OCQMLN`
(`:1308`), and that exit condition is simply false on this path.

### C4 — `OCNODE` stalls for the full 200 iterations when `FN` hits exactly zero (`:310-345`)

`87a18ff` deleted this from the top of the iteration body:

```fortran
IF (ISZERO(FN)) THEN   ! * Test for convergence (either exact or approximate)
    ...
ENDIF
```

Trace what now happens when `FNODE` returns `FN == 0` and `|B - A| > 1e-4`:

1. Convergence test fails on the bracket half (`:319`).
2. `TEST = GTZERO(FN * FNM1)` = `GTZERO(0)` = `.FALSE.` — no halving.
3. `FN * FA >= 0` is `0 >= 0`, true, so `A = WN` and `FA = 0`.
4. Next iteration: `WN = (A*FB - B*FA)/(FB - FA)` = `A*FB/FB` = `A`, exactly.
5. `FNODE(A, ...)` returns `0` again. Go to 1.

`A`, `B`, `FA` and `FB` are now all fixed points. The loop grinds out its
remaining iterations re-evaluating `FNODE` at the identical elevation, exits
with `FAILED = .TRUE.`, and issues warning 1027 — and 1028 as well if
`|B-A| > 1e-3`, which by construction it may well be. The flows themselves are
fine (`FN = 0`, so the skipped correction in **C3** is a no-op here), but the
run pays 200 `FNODE` calls and reports a spurious convergence failure.

The two `RETURN`s at `:299` and `:302` only cover `F` being zero *at the bracket
ends*, not at an interior trial point.

Fix: restore an interior `IF (ISZERO(FN)) EXIT` — with `FAILED = .FALSE.`, since
the old code's `failed = .TRUE.` on that path was itself wrong.

### C5 — `OCNODE` depends on an undocumented `branch 0 is always active` invariant (`:286-287`, `:1358-1367`)

```fortran
A = ZI(0)
B = A
init_loop: DO J = 1, 3
   IF (ISZERO(ROOTLI(J))) THEN ...
```

Branch 0 is seeded into the bracket unconditionally; only branches 1-3 are
tested against the `ROOTLI == 0` absent-branch flag. The argument documentation
states the invariant (`:271`, "branch `J=0` is never absent") but nothing
enforces it.

In `OCQMLN` this becomes a live hazard on the *perturbed* solves. `ZJ(J)` is
assigned only inside the `JEL2(J) > 0` arm (`:1364`); the perturbed call passes
`ZJ`, not `ZI` (`:1388`). If `JEL2(0)` were ever non-positive, `A = ZJ(0)` reads
an uninitialized local and the bracket is seeded from garbage. `CI(0)`/`DI(0)`
are in the same position but are protected in practice by `FNODE`'s `ROOTLI`
guard (`:428`).

The caller always sets `JEL2(0) = ielu` (`OCQDQMOD.F90:314`) with `ielu >= 1`,
so this holds today. It is one line to assert.

### C6 — `OCFIX` can read `PEL0`/`PFACE0` uninitialized (`:1811-1833`)

```fortran
confluence_loop: DO PPP = 1, 3
   PEL = afromICMRF2 (IBR, PPP)
   IF (PEL < 1) CYCLE confluence_loop
   ...
   PEL0 = PEL
   PFACE0 = PFACE
END DO confluence_loop

IF (JEL < 0) THEN
   JEL = PEL0
   JFACE = PFACE0
END IF
```

`PEL0`/`PFACE0` are assigned only for participants with `PEL >= 1`. If a branch
`IBR` has no such participant, the fallback assigns undefined values to
`JEL`/`JFACE`. If `TEST` is also true (`QSMALL` or `HSMALL`, `:1798`), the
adjustment block at `:1837` then indexes `cellarea(JEL)`, `HRFZZ(JEL)` and
`QSAZZ(JEL, JFACE)` with them, and writes back at `:1863-1864`.

The routine's own entry-requirements table already says "at least one
participant must exist" (`:1688`), so this is a documented precondition with no
check. Initialising `PEL0 = 0` before the loop and keeping the `JEL > 0` guards
converts a silent memory scribble into a no-op.

### C7 — `OCQBNK` can take `SQRT` of a negative argument (`:873-874`)

```fortran
IF (ZG >= ZB) THEN
   DZ = SIG * DZ + MIN(DZL, ZERO)
   ROOTDZ = SQRT(DZ)
```

With `DZL = ZI(LO) - ZB < 0`, the expression collapses to
`DZ = ZI(HI) - ZBG(0)` — negative whenever the *higher* of the two water levels
is itself below channel bank-full.

Under the branch's own conditions this cannot happen: `ZG >= ZB` here, and if
the bank element is the higher side then `ZI(1) >= ZBG(1) = ZG >= ZB`; if the
channel is the higher side then `DZL = ZI(1) - ZB >= 0` and the `MIN` is zero.
So the guard is `ZI(1) >= ZBG(1)`, i.e. non-negative depth on the bank element —
the exact condition **C1** notes is not always true. A NaN here propagates
silently into `Q`, `DQ`, the OC matrix row and then the whole solve, with
`-fp-model=precise` and no trapping enabled (`CMakeLists.txt:679-686`).

`ROOTDZ = SQRT(MAX(ZERO, DZ))` costs one instruction.

### C8 — `JFACE` undefined on the `JEL == 0` path (`:1804-1805`)

```fortran
ELSE IF (JEL == 0) THEN
   FAIL = .FALSE.
```

`JFACE` is not set. Every subsequent use is inside `IF (JEL > 0)` (`:1840`,
`:1859`), so it is currently unreachable — but the safety is by inspection
across 60 lines rather than by construction. Noted for completeness, not for
action.

---

## 2. Accuracy

### A1 — The module's own smoothing formula is written backwards in four places

`DIMJE` is the positive-difference function (`sglobal.f90:571-579`):

```fortran
IF (x>y) THEN
   dimje = x-y
ELSE
   dimje = zero
```

so `DIMJE(DZMIN, DZ) = MAX(DZMIN - |Δz|, 0)`. The code is right, and matches the
source comment at `:1052` ("`DZMIN` in `CONVMM` prevents small `DQ` when `DZ` is
small"): the correction is active only for `|Δz| < DZMIN` and switches off above
it.

The FORD documentation says the opposite. At `:605`, `:978` and `:1128` the
formula is given as

\[ C^* = C + C_H \max(DZMIN, |\Delta z|) \]

which is a term that *grows without bound* as the head difference grows, rather
than a floor that vanishes. The correct statement is

\[ C^* = C + C_H \max(DZMIN - |\Delta z|,\ 0). \]

The error ripples into the `DQ` expressions quoted immediately below each
(`:617-621`, `:985-987`, `:1133-1134`), which are otherwise correct. Four
documentation sites, no code change.

`OCCODE`'s use at `:537` — `DIMJE(H, AFROMXSTYPES(1,I))` — and the module header
at `:478` are consistent with each other and with `DIM` semantics; only the
`DIMJE(DZMIN, DZ)` sites are mis-transcribed.

### A2 — `CONVEYAN`'s `ty=0` derivative is not the derivative of its conveyance (`:1464-1466`)

```fortran
HM23 = H**F23
CONV = STR * XA * HM23      ! NOTE IS XA FOR CASE 0 BUT H FOR CASE 1
DERIV = STR * HM23 * F53
```

For `ty=1`, `C = str·h^{5/3}` and `dC/dh = (5/3)·str·h^{2/3}` — the returned
`DERIV` is exact. For `ty=0`, `C = str·xa·h^{2/3}`, whose derivative is
`str·(dxa/dh)·h^{2/3} + (2/3)·str·xa·h^{-1/3}`. The returned value is the `ty=1`
derivative, which coincides with the truth only if `xa ≡ h`. The comment on the
`CONV` line acknowledges the `xa`/`h` swap but the `DERIV` line below it does
not follow.

No live consumer is affected: the only `ty=0` call site is `OCXS`
(`OCmod.f90:2498`), which discards `DERIV` into `adumy` and builds the table
slope as a proper secant `(XCJ - XCJM1)/STEPH` (`:2502`). The contract is still
inconsistent, and `CONVEYAN` is `PUBLIC`.

While here — `ty=2`'s derivative *is* correct, which answers the
`! is f23 correct here?` comment at `:1486`:

\[ \frac{d}{dh}\left[str\,A(h)\,h^{2/3}\right]
 = str\,W\,h^{2/3} + \tfrac{2}{3}\,str\,A\,h^{-1/3}
 = C\left(\frac{W}{A} + \frac{2}{3h}\right), \]

with `dA/dh = W = extra`, exactly as coded. The comment can be deleted.

### A3 — The near-zero smoothing is sound (verified, no action)

For `10⁻⁹ <= h < 10⁻³` both `ty=0` and `ty=1` use

\[ p(h) = \tfrac{10}{3}h^2(4 - 1000h),\qquad p'(h) = \tfrac{10}{3}h(8 - 3000h). \]

Checked:

- `p'` is exactly `dp/dh`. ✓
- Value continuity at the `h = 10⁻³` join: `p(10⁻³) = (10/3)(10⁻⁶)(3) = 10⁻⁵`,
  and `h^{5/3} = 10⁻⁵`. ✓
- Slope continuity: `p'(10⁻³) = (10/3)(10⁻³)(5) = 1.667×10⁻²`, and
  `(5/3)h^{2/3} = (5/3)(10⁻²) = 1.667×10⁻²`. ✓
- Monotone on the smoothed range: `p' > 0` for `h < 8/3000 = 2.67×10⁻³`. ✓
- `ty=0`'s variant `str·p(h)·xa/h` also matches `str·xa·h^{2/3}` at the join. ✓

So the polynomial is the correct C¹ Hermite blend, not an ad-hoc fudge. The only
discontinuity is at `h = 10⁻⁹`, where `DERIV` steps from 0 to `2.7×10⁻⁸·str` —
negligible.

Worth recording because the `TAKE CARE valid only for threshold of 1 mm`
comments (`:1460`, `:1462`, `:1475`, `:1476`) read like a warning that something
is unverified. It is verified, and the comments are right that the coefficients
`4`, `1000`, `8`, `3000` are hard-wired to `DZMIN = 10⁻³`: changing `DZMIN`
without recomputing them breaks both continuity properties above.

### A4 — `OCQMLN`'s finite-difference Jacobian is limited by `OCNODE`'s tolerance (`:1373-1397`)

```fortran
ZINC = MAX(WLMIN, (ZSAVE - ZGI(J)) * ONEPC)   ! WLMIN = 1e-3, ONEPC = 1e-2
...
CALL OCNODE(IELB, ZJ, CI, DI, ROOTLI, QDUM2)
DO I = 0, 3
   DQIJ(I, J) = (QDUM2(I) - QJ(I)) / ZINC
END DO
```

`OCNODE` accepts `|F| <= 10⁻³·Σ|Q|` and then dumps the whole residual onto the
single largest branch (`:324`). Each individual `Q_i` therefore carries an
uncertainty of order `10⁻³·Σ|Q|`, and — this is the part that does not cancel —
**`JMAJOR` can differ between the base solve and the perturbed solve.** When it
does, one branch flow jumps by the full residual for reasons that have nothing
to do with the perturbation.

With a shallow branch, `ZINC` bottoms out at `WLMIN = 10⁻³` m, so the difference
quotient amplifies that jump by 1000×. The resulting derivative noise floor is
`O(Σ|Q|)` — the same order as the flows themselves.

This is an upper bound: when `JMAJOR` is stable and both solves stop at similar
residuals the errors are strongly correlated and mostly cancel. But the bound is
not small, and the consequence is a polluted OC Jacobian, which shows up as
extra outer work and timestep rejections in `OCSIM` rather than as time inside
`OCmod2` — i.e. exactly the kind of slowdown that does not appear where you look
for it.

Two mitigations, in order of cost:

1. Make the perturbed solve deterministic relative to the base solve: reuse the
   base solve's converged node elevation as the perturbed solve's starting
   bracket, and pin `JMAJOR` to the base solve's choice.
2. Differentiate the node balance analytically. `DI` is already available for
   every branch, so `∂F/∂z_n` is closed-form and `∂Q_i/∂z_j` follows by the
   implicit function theorem — replacing four extra confluence solves with a
   4×4 arithmetic step. This also removes the dominant term in **P2**.

### A5 — `OCQBNK`'s `DQ(LO,LO)` cut-off is `DZMIN` away from where the flow changes (`:889-891`)

```fortran
IF (DZL < -DZMIN) DUM = ZERO
DQ(LO, LO) = -DUM / ROOTL
```

`Δz*` becomes independent of `ZI(LO)` as soon as `DZL < 0` (see **C7**), so the
true `∂Q/∂z_LO` is zero throughout `DZL < 0`. The code only zeroes it below
`-DZMIN`. In the 1 mm band `DZL ∈ (-DZMIN, 0)` the returned derivative is
`-DUM/√L` where the truth is `0`.

This is consistent with the `DZMIN` smoothing convention used everywhere else in
the module and is very likely deliberate — an abrupt Jacobian step is worse for
the solve than a 1 mm-wide inconsistency. Recorded so it is not mistaken for a
bug later. Note that `DQ(LO,HI)` correctly keeps the un-zeroed `DUM`, because
`∂Δz*/∂z_HI = 1` on both sides of the switch.

### A6 — The `NTYPE=12` ZQ derivative is a hard-coded constant (`:1205-1216`)

```fortran
Q(LO) = GET_ZQTABLE_VALUE(ZQTABLEREF, ZI(HI))
WEIRSILL = ZQWEIRSILL(ZQTABLEREF)
DZU = DIMJE(ZI(HI), WEIRSILL)
DQ(LO, HI) = 50.0D0 * 1.5D0 * SQRT(DZU)
DQ(LO, LO) = 0.0D0
```

The routine's own `@warning` (`:1109-1114`) already flags that `75·√Δz` is not
the slope of the rating curve and was tuned for Crummock. Two things to add:

- `DQ(LO,LO) = 0` is *correct* here, not an oversight: `Q` depends only on
  `ZI(HI)`, so the pair `(DQ(LO,HI), DQ(LO,LO)) = (something, 0)` is
  structurally right and the antisymmetric copy at `:1239-1241` preserves it.
  Only the magnitude of `DQ(LO,HI)` is arbitrary.
- **Worth verifying separately:** `HI`/`LO` are chosen by which water level is
  higher (`:1186-1189`), so if the downstream level ever exceeds the reservoir
  level, the table is evaluated at the *downstream* stage and the flow reverses.
  A stage-discharge rating curve is normally defined for one physical direction
  only. Whether that reversal is intended, or whether the table's upstream side
  should be pinned by link identity, is not determinable from this module.

`get_ZQTable_value` is reached through the module-scope `ZQTableRef`, which
`OCQDQ` sets as a side channel inside its face loop (`OCQDQMOD.F90:265-271`).
That is not re-entrant and would need attention before any threading of the face
loop.

### A7 — `XSTAB`'s last row is never written (`OCmod.f90:2480-2503`)

`OCXS`'s table loop runs `J = 2, NXSCEE` and stores into `XSTAB(2, J-1, ·)` and
`XSTAB(3, J-1, ·)`, so `XSTAB(2, NXSCEE, ·)` and `XSTAB(3, NXSCEE, ·)` are never
assigned. `XSTAB(1, NXSCEE, ·)` *is* assigned (`:2500`) and is the one `OCCODE`
reads as `HFULL` (`:530`).

`OCCODE`'s `I < NXSCEE` guard means the two unwritten entries are never read, so
there is no live defect — but they will show as uninitialized under valgrind or
`-finit-real=snan`, and the module's entry-requirement list (`:451-459`,
`:1268`) implies the whole table is valid. One line in `OCXS` closes it.

---

## 3. Memory allocation and stride

### M1 — `NXSCEE = 100 000` over-resolves the table by two orders of magnitude (`:80-82`, `:177`, `:507-544`)

`ALLOCATE(xstab(3, nxscee, total_no_links))` is `3 × 100 000 × 8 = 2.4 MB per
channel link`. `analysis_ocmod.md` finding M1 covers the total (0.2-2.4 GiB on
the shipped examples, 109 M `CONVEYAN` calls at initialisation). The consumption
side is what matters here:

Every `OCCODE` call, i.e. every conveyance evaluation on every channel face on
every timestep, does two loads into that table:

```fortran
HFULL = AFROMXSTYPES(1, NXSCEE)          ! the far end of a 2.4 MB object
I = INT((H / HFULL) * DBLE(NXSCEE - 1) + ONE)
DERIV = AFROMXSTYPES(3, I)               ! somewhere in the middle
CONV  = AFROMXSTYPES(2, I) + DERIV * DIMJE(H, AFROMXSTYPES(1, I))
```

The three row entries `(1,I)`, `(2,I)`, `(3,I)` are adjacent — the `(3, NXSCEE,
link)` ordering is the right one and gets all three from one 24-byte load. That
part is well done. The problem is the two accesses 2.4 MB apart, and the fact
that the table is far too large to stay resident across links.

**How much resolution is actually needed.** The table stores a piecewise-linear
reconstruction, error `O(C''·Δh²/8)`. With `NXSCEE = 100 000` and a 5 m
bank-full depth, `Δh = 50 µm`. Every consumer of `CONV` in this module then
smooths the result over `DZMIN = 1 mm` — twenty times coarser. At
`NXSCEE = 1000`, `Δh = 5 mm`, still comfortably inside the smoothing floor, and
the table drops to **24 KB per link**: L1/L2-resident, and the `HFULL` probe
lands in the same page as the interpolation.

**Two further reductions, independent of that.** Row 1 stores a uniform ramp,
`XSTAB(1,j,·) = STEPH·(j-1)` (`OCmod.f90:2476`, `:2500`) — a third of the memory
reproducing what the index already encodes. `OCCODE` reads it only to form
`H - XSTAB(1,I)`, which is `H - STEPH*(I-1)`. Caching `STEPH` and `HFULL` in
small per-link side arrays would:

- drop the table to `(2, NXSCEE, nlinks)` — another third off;
- remove the far-end `HFULL` load entirely, so `OCCODE` touches exactly one
  cache line per call.

Combined with the resolution cut that is `2.4 MB → 16 KB` per link.

Risk: low. The reconstruction is continuous across nodes and into the `ty=2`
branch either way (verified in §6), and `OCXS`'s build cost falls by the same
factor.

### M2 — The face dimension of `qsazz` is the slowest-varying one (`:77-78`)

```fortran
DOUBLEPRECISION, DIMENSION(NELEE)   :: HRFZZ
DOUBLEPRECISION, DIMENSION(NELEE,4) :: qsazz
```

`NELEE = 250 000`, so `qsazz(iel,1)` and `qsazz(iel,2)` are **2 MB apart**.
Every loop in the codebase that processes the four faces of one element reads 32
bytes spread over four cache lines and four pages:

| Site | Loop |
|---|---|
| `OCmod2.f90:1779-1783` | `OCFIX` `HSMALL` pre-scan |
| `OCmod2.f90:1790-1883` | `OCFIX` face loop, up to `NPASS = 100` times |
| `OCmod.f90:2219-2240` | `OCSIM` state advance, `GETQSA`/`SETQSA` per face |
| `OCQDQMOD.F90:280-284`, `:338` | `OCQDQ` flux scatter |

`afromICMREF(NELEE, 12)` has the same shape problem and `OCFIX` reads eight of
its twelve columns — `IFACE+4` and `IFACE+8` for four faces — each 1 MB from the
next (`:1800-1802`).

Counting the concurrent sequential streams in `OCFIX`'s element loop:

```
HRFZZ, cellarea, DXQQ, DYQQ, ZGRUND            5
qsazz columns 1..4                             4
afromICMREF columns 5..12                      8
                                              ---
                                              17
```

That is at or past what a typical L2 stream prefetcher tracks. Note the honest
framing: **within one pass each column is swept sequentially in `ielc`**, so
this is prefetcher pressure and lines-touched-per-element, not random access.
The fix is a layout transpose:

- `qsazz(4, NELEE)` — the four faces of an element become one contiguous 32-byte
  block inside a single cache line;
- an element-contiguous `ICMREF(12,NELEE)` layout for the
  neighbour/reciprocal-face fields (the eight fields `OCFIX` needs), likewise.

That takes 17 streams to about 7 and places each element's face/topology data
in two compact blocks. The payoff scales with how many of `OCFIX`'s up-to-100
passes actually execute, which `analysis_ocmod.md` finding C4 flags as
unmeasured — so measure the pass histogram first.

The transpose is not local to this module: `qsazz` is `PUBLIC` (`:86`) and
`OCmod.f90:2265-2268` copies it into `QOC(NELEE,4)`. Transposing `qsazz` alone
would turn those four contiguous column copies into strided reads. The coherent
cross-module change is therefore to transpose `QOC` to `(4,NELEE)` at the same
time; its face-inner consumers in `SYmod`, `CMmod`, `FRmod`, and `rest` then
benefit too, while `OCSIM` can copy one contiguous four-face block per element.
The tradeoff is that face-outer restart/diagnostic sweeps such as
`FRmod.f90:1356` and `SYmod.f90:2653` become stride-4, so they belong in the
benchmark even though they are not the per-element hot path.

### M3 — `HRFZZ` and `qsazz` are ~99 % padding

2 MB and 8 MB of static BSS sized by `NELEE = 250 000`, while the touched extent
is `total_no_elements` — one to two orders of magnitude smaller on every shipped
example. Untouched pages never fault in, so the resident cost is small; the cost
is that the *live* data is spread across an 8 MB address range instead of being
compact, which is the same issue as **M2** seen from the allocation side.

Making both `ALLOCATABLE` at `total_no_elements` (as `OCSIM_WORKSPACE` already
does for its solver arrays, `OCmod.f90:200-240`) would compact the working set.
`initialise_ocmod` (`:172-180`) is already the natural home and already runs
after the element count is known.

### M4 — What commit `13d3d71` actually traded

`13d3d71` removed the `inhrf`/`GGGETHRF`/`inqsa`/`GGGETQSA` arguments and made
`OCFIX` correct `HRFZZ`/`QSAZZ` in place. Two observations:

**It is behaviour-preserving.** `GGGETHRF` was seeded from `inhrf` before the
pass loop and then both read and written inside it, so the pass loop already had
Gauss-Seidel semantics — element `n` sees updates written by elements `1..n-1`
of the same pass. Substituting the module arrays changes nothing about that
ordering. The saving of three round trips of the OC state per timestep is real.

**It also removed the last compact copy of the data.** The buffers were
dimensioned `(nel)` and `(nel,4)`, so inside the pass loop the face columns were
`nel` apart, not `NELEE` apart. For `nel = 5000` the bytes touched are identical
either way and each column is still swept sequentially, so this is **not** a
regression in practice. But it does mean **M2**'s transpose is now the only
route to getting one element's four faces onto one cache line.

The AD note the commit added (`:1737-1756`) is correct and worth keeping: a
`QSAZZ(1:nel,:)` actual argument is non-contiguous and would be copied in and
out. Transposing to `qsazz(4, NELEE)` would make `QSAZZ(:,1:nel)` contiguous and
remove that objection to the argument-passed form as well.

### M5 — No hidden array temporaries anywhere in the module (verified)

Checked every call site where Fortran silently materialises a copy:

| Site | Actual | Dummy | Verdict |
|---|---|---|---|
| `:741`, `:1228`, `:1365`, `:1385` | `XSTAB(:,:,LINK)` | `AFROMXSTYPES(3, NXSCEE)` | Contiguous whole-plane slice of a rank-3 array against an explicit-shape dummy — address passed, no copy. ✓ |
| `OCQDQMOD.F90:197` | `COCBCD(1, IBC)` | `AFROMCOCBCD(5)` | `COCBCD(5, NOCTAB)` (`OCQDQMOD.F90:55`) — sequence association gives exactly 5 contiguous elements, in range. ✓ |
| `OCQDQMOD.F90:276` | `COCBCD(1, itemp)` | `AFROMCOCBCD(3)` | First 3 of the same 5. ✓ |
| `OCQDQMOD.F90:242`, `:260`, `:275` | `QJ(0:3)`, `DQ(0:1,0:1)` | `Q(0:1)`, `DQ(0:1,0:1)` | Larger actual against smaller dummy — legal sequence association, no copy. ✓ |

The `PERF FIX` comments at `OCQDQMOD.F90:195`, `:239`, `:257`, `:274` claiming
these avoid dope-vector overhead are correct in effect, though the mechanism is
sequence association rather than descriptor avoidance.

---

## 4. Performance, beyond memory layout

### P1 — `H**F23` is a `pow()` call on every conveyance evaluation (`:1464`, `:1478`, `:1484`)

`F23 = 2.0D0/3.0D0` is a real exponent, so `H**F23` compiles to
`exp(F23*log(H))` via libm — roughly 50-100 cycles, and `-fp-model=precise`
(`CMakeLists.txt:679-686`) prevents any strength reduction. This sits on the
per-face path through `OCQGRD`, `OCQBNK`, `OCQBC` and `OCCODE`'s above-table
branch, plus 100 000 calls per link at initialisation from `OCXS`.

`h^{2/3} = cbrt(h)²`, and `cbrt` is typically 2-4× cheaper than a general `pow`.
The value is already computed once and reused for both `CONV` and `DERIV`
(`HM23`), which is the right structure — only the primitive is expensive.

### P2 — The `OCNODE` iteration cap is a 4× worst-case multiplier on confluence cost

`OCQMLN` calls `OCNODE` once for the base solve (`:1370`) plus once per active
branch for the finite-difference columns (`:1388`) — up to five times per
confluence per `OCQDQ` call. Each `OCNODE` is now capped at 200 `FNODE`
evaluations instead of 50, and each `FNODE` is four `SQRT`s and four divides
over branches `0:3` (`:427-435`).

Worst case per confluence per timestep: **1000 `FNODE` calls, against 250 before
`87a18ff`.**

Whether that worst case is approached is exactly what is unmeasured. Three
things make it more likely than it was: the 10× tighter residual test, the 10×
tighter bracket test, and the `FN == 0` stall in **C4**.

Recommended order:

1. **Instrument the `OCNODE` iteration histogram.** One counter, one run. It
   either exonerates `87a18ff` or identifies it as the regression, and nothing
   else in this list should be done before it.
2. Fix **C4** — one line, removes an entire class of 200-iteration stalls.
3. If the histogram is bad, replace the Illinois iteration with a Newton step on
   the node balance. `DI` is already passed in, so `∂F/∂z_n = Σ_j ∂Q_j/∂z_n` is
   closed-form; expect single-digit iterations instead of tens. This is the same
   change as **A4**'s mitigation 2 and fixes both.

`FNODE` also recomputes `SIG`, `CJ` and the `SQRT` for all four branches on
every call even though only the trial elevation moved; a Newton formulation
removes most of those evaluations rather than optimising them.

### P3 — `OCFIX` divides per element per pass (`:1765`, `:1841`)

```fortran
DZE = DTOC / cellarea (ielc)        ! per element, per pass
...
DZA = DTOC / cellarea (JEL)         ! per corrected face, per pass
```

Up to `NPASS × nel × 5` double-precision divisions (~20 cycles each, not
pipelined like multiplies) recomputing a quantity that changes only when `DTOC`
changes. One `rdze(1:nel) = dtoc / cellarea(1:nel)` before `pass_loop` — a
single vectorisable pass — removes all of them and turns both sites into loads.

### P4 — Formatted internal writes in the innermost loop (`:1878`, `:1898`)

```fortran
IF ((ABS (DHQ) > HERROR) .OR. (passs == npass)) THEN
   ...
   WRITE (MSG, 91030) rdum4(1), rdum4(2), idum, rdum4(4)
   CALL ERROR(WWWARN, 1030, PPPRI, ielc, 0, MSG)
```

On the final pass the `passs == npass` disjunct makes this **unconditional for
every corrected face**. A formatted internal write into a `CHARACTER(132)` costs
on the order of a microsecond; `CALL ERROR` then writes it out. A run that
regularly reaches the pass cap pays that per corrected face, per timestep — and
by construction it only happens in runs that are already struggling, i.e. the
slow ones.

Cheapest fix: count the offending faces during the final pass and emit one
summary record per element, or gate the `passs == npass` disjunct behind a
diagnostic flag while keeping the `ABS(DHQ) > HERROR` test unconditional.

`HERROR = 10⁻⁵` m of depth created is not a rare threshold, so the first
disjunct alone can fire often; that one is genuinely diagnostic and should stay.

Minor, same lines: `rdum4` and `idum` are pure indirection — `rdum4(3)` is never
set in the 91030 branch, and the values could go straight into the `WRITE` list.

### P5 — The accessors (`:94-148`)

`GETHRF`/`SETHRF`/`GETQSA`/`SETQSA` are one-line module procedures, and IPO is
enabled (`CMakeLists.txt:73`), so they should inline. `gethrf`/`getqsa` are
`PURE`; `sethrf`/`setqsa` cannot be. `OCSIM`'s state-advance loop calls them
`10·n` times per timestep (`OCmod.f90:2213-2240`).

Nothing in this module obstructs inlining, and `analysis_ocmod.md` finding S6 /
P1 already owns the caller-side reduction. Recorded only so the accessor pattern
is not blamed twice.

---

## 5. Priority table

| ID | Action | Findings | Expected effect | Risk |
|---|---|---|---|---|
| **P0** | Instrument the `OCNODE` iteration histogram and the `OCFIX` pass count | C3, C4, P2 | None directly — decides whether `87a18ff` is the regression | None |
| **1** | Restore the interior `IF (ISZERO(FN)) EXIT` in `OCNODE` | C4 | Removes 200-iteration stalls on exact hits, and the spurious 1027/1028 warnings they raise | None — strictly fewer iterations, same result |
| **2** | Clamp `OCCODE`'s index: `I = MAX(1, INT(...))` | C1 | Closes a silent out-of-bounds read | None |
| **3** | `ROOTDZ = SQRT(MAX(ZERO, DZ))` in `OCQBNK`; initialise `PEL0 = 0` in `OCFIX`; `ELSE` arm in `CONVEYAN` | C7, C6, C2 | Closes three latent paths | None |
| **4** | Correct the `DIMJE(DZMIN, DZ)` formula in the FORD blocks at `:605`, `:978`, `:1128`; drop the answered `! is f23 correct here?` at `:1486`; note the unwritten `XSTAB` tail rows | A1, A2, A7 | Documentation only | None |
| **5** | Hoist `dtoc/cellarea` out of `OCFIX`'s pass loop | P3 | Removes up to `100·nel·5` divisions per timestep | None — bitwise identical |
| **6** | Gate the `passs == npass` write disjunct; emit one summary record | P4 | Removes per-face formatted I/O from the worst-case path | Low — changes diagnostic granularity |
| **7** | Cut `NXSCEE` to ~1000; store `STEPH`/`HFULL` per link; drop `XSTAB` row 1 | M1 | 2.4 MB → ~16 KB per link; `OCCODE` touches one cache line instead of two distant ones; `OCXS` init cost falls 100× | Low — interpolation error stays far below `DZMIN` |
| **8** | Apply mass conservation on `OCNODE`'s capped path, or reject the timestep | C3 | Removes an unbounded conservation error on non-converged junctions | Medium — changes results where it fires, which is the point |
| **9** | Transpose `qsazz` and `QOC` to `(4, NELEE)`, and `ICMREF` to an element-contiguous `(12,NELEE)` layout | M2, M4 | 17 concurrent `OCFIX` streams → ~7; removes the face-wise layout mismatch across OC and its consumers | Medium — coordinated change across OC, SY, CM, FR, `rest`, and shared declarations |
| **10** | Replace `OCQMLN`'s finite-difference columns with the analytic Jacobian | A4, P2 | Removes up to four confluence solves per junction per timestep and the derivative noise floor | Medium-high — changes the Jacobian, needs regression testing |
| **11** | `cbrt`-based `h^{2/3}` | P1 | 2-4× on the conveyance primitive | Low — verify against `-fp-model=precise` expectations |

---

## 6. Verified correct — do not re-audit

Checked in detail and found sound. Recorded so the next pass over this file does
not spend time on them.

**Paired-flux antisymmetry and derivative signs** (`OCQGRD:1056-1068`,
`OCQBNK:881-906`, `OCQLNK:1228-1241`). With `Q_{LO} = C\sqrt{\Delta z}/\sqrt{L}`
and `Δz = z_{HI} - z_{LO}`:

\[ \frac{\partial Q_{LO}}{\partial z_{HI}}
   = \frac{C_H\sqrt{\Delta z} + \tfrac{1}{2}C/\sqrt{\Delta z}}{\sqrt{L}},
   \qquad
   \frac{\partial Q_{LO}}{\partial z_{LO}}
   = -\frac{\tfrac{1}{2}C/\sqrt{\Delta z}}{\sqrt{L}} \]

— the `C_H` term is absent from the second because `H_m = z_{HI} - z_{G,HI}`
depends on the higher side only. All three routines match this, and all three
enforce `Q(HI) = -Q(LO)`, `DQ(HI,·) = -DQ(LO,·)` by construction rather than by
recomputation, so the face pair is conservative to the last bit.

**`OCFIX`'s face-length selection** (`:1797`). `DXY(MOD(IFACE,2))` with
`DXY(0) = DXQQ`, `DXY(1) = DYQQ` gives faces 1,3 → `DYQQ` and faces 2,4 →
`DXQQ`, which is exactly `OCQDQMOD:fdqq` (`:394-405`). Consistent.

**The width passed to `OCQBC` really is the channel width for channel
boundaries.** `OCQDQ` passes `W = FDQQ(ielu, IFACE)` (`:189`), which becomes
`AFROMCWIDTH` in `OCCODE`'s above-table area extension (`:540`) — where
`CWIDTH(link)` is wanted, and where the sibling paths `OCQLNK`/`OCQMLN` do pass
`CWIDTH` explicitly (`OCQDQMOD.F90:231`). It resolves correctly: `FRmod.f90:303-308`
sets `DXQQ = CWIDTH` for a north-south link and `DYQQ = CWIDTH` for an east-west
one, and the face carrying the channel flow is precisely the face whose `fdqq`
returns that dimension. Correct, but obtained indirectly enough to be worth
writing down.

**`OCQBC`'s weir sign handling** (`:710-718`). `QWEIR` is called with the higher
and lower of the local and external levels; when `ZI >= ZX` the local element is
upstream, so `dQ/dZI` is `DQU` and the flux is negated to follow the
into-element convention; otherwise the local element is downstream, `dQ/dZI` is
`DQL` — which is already in `FROMDQ` from the call — and the flux is positive.
Both orientations are right.

**`OCNODE`'s bracket invariant.** `A = min_j ZI_j` gives `Δz_j <= 0` for every
active branch, hence `FA <= 0`; symmetrically `FB >= 0`. Exact zeros at either
end return early (`:299`, `:302`). Inside the loop, `FN * FA >= 0` retains the
sign of the replaced endpoint and the Illinois halving only scales magnitudes,
so `FA <= 0 <= FB` holds throughout and `FB - FA` cannot vanish. The division at
`:312` is safe. (The `FN == 0` case is **C4** — a stall, not a division fault.)

**`OCFIX`'s `HSMALL` sign algebra** (`:1774-1785`, `:1846-1847`). For `H < 0`:
`DQE0 > 0`, `SGN = +1`, flagged faces are outflows (`QE < 0`), `Qasum < 0`,
`FDQE ∈ [-1, 0)`, and `DQE = FDQE·QE > 0` adds inflow. For `0 < H < HCRIT`:
`DQE0 < 0`, `SGN = -1`, flagged faces are inflows, `FDQE ∈ [-1, 0)`, and
`DQE < 0` removes inflow. Both directions correct, and the `MAX(-ONE, ·)` caps
the reduction at 100 % of each flagged flow.

**`OCFIX`'s "depth created" diagnostic** (`:1855`, `:1862`, `:1868`). `Qasum`
accumulates `DQE + DQA` across the local and neighbour corrections and is scaled
by the *local* `DZE`. That looked dimensionally wrong at first read; it is not.
The volume created is `DTOC·(DQE + DQA)`, so `(DQE + DQA)·DZE` is that volume
expressed as a depth at the local element — the correct normalisation for a
per-element diagnostic.

**`XSTAB`'s piecewise-linear reconstruction is continuous**, both across table
nodes and into `CONVEYAN`'s `ty=2` branch at the top of the table. `OCXS` stores
`XSTAB(2,j) = C(h_j)` and `XSTAB(3,j) = (C(h_{j+1}) - C(h_j))/\Delta h`
(`OCmod.f90:2500-2502`), so `XSTAB(2,I) + XSTAB(3,I)(h - XSTAB(1,I))` reproduces
`C(h_{I+1})` exactly at the right end of every interval, and at `h = HFULL` both
branches evaluate `str·A·h^{2/3}`.

**The near-zero smoothing polynomial** — see **A3**. C¹ at the `1 mm` join,
monotone, and `p'` is exactly `dp/dh`.

**No hidden array temporaries** — see **M5**.

---

## 7. What could not be determined from the source

- **How often `OCNODE` reaches its 200-iteration cap**, and how that compares
  with the pre-`87a18ff` behaviour at 50. This decides whether **C3**, **P2**
  and the 2026-05 tolerance change are the slowdown or a red herring. It is one
  counter and one run (**P0**).
- **How many of `OCFIX`'s 100 passes actually execute.** Everything in **M2**,
  **P3** and **P4** scales with it. `analysis_ocmod.md` finding C4 raises the
  same question; it is still open.
- **Whether the `NTYPE=12` `HI`/`LO` selection can reverse a ZQ rating curve**
  (**A6**). Needs the rating-table semantics from `ZQmod` and a reservoir
  dataset, not this module.
- **Whether `jxswork` can reach `OCQMLN` stale.** `OCQDQ` declares it
  uninitialized (`OCQDQMOD.F90:155`) and assigns it only for participants with
  `KEL <= total_no_links` (`:232`, `:313`). If a confluence participant were ever
  a land element, `XSTAB(:,:,JXSWORK(J))` at `:1365`/`:1385` would index with a
  stale or undefined link number. Confluences are channel junctions, so this
  should not arise — but it depends on `FRmod`'s `ICMRF2` construction, which is
  outside this module's scope.
