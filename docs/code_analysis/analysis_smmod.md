# Logical analysis: `SMmod` — correctness, performance, memory/variable management

## Scope and method

Source-only assessment of `src/modules/SMmod.f90` (746 lines). No profile was
taken and no timings were measured. Every claim below is derived from reading
the module together with:

- its only caller, `src/modules/ETmod.f90:755-763` (`ETIN`, which calls `SMIN`
  twice per element per ET step),
- `src/modules/FRmod.f90:5560-5645` (`INSM`, which fills `DDF`, `RHOS`,
  `RHODEF`, `TSIN`, `NSD`, `MSM`, `ZOS`/`ZDS`/`ZUS`, `IMET`, `SD`, `RHOSAR`,
  and zeroes `NSMC`),
- `src/modules/FRmod.f90:1296-1370` (`FRINIT`, the hotstart reader that fills
  `SMELT`/`TMELT`),
- `src/modules/run_sim.f90:348-375` (the hotstart writer),
- the declarations in `src/parameters/AL_C.F90`, `src/parameters/AL_D.f90` and
  `src/parameters/sglobal.f90`,
- the compiler configuration in `CMakeLists.txt`.

Where a claim depends on compiler behaviour rather than on the source alone,
that is stated. Where a claim depends on a physical intent I could not recover
from the source, it is presented as an inconsistency with the numeric evidence
attached, not as a settled defect.

Relevant compiled extents:

| Constant | Value | Source |
|---|---|---|
| `max_no_snowmelt_slugs` | 400 | `sglobal.f90:134` |
| `NELEE` (element capacity) | 250 000 | `sglobal.f90:118` |
| `NVEE` (vegetation/met capacity) | 250 000 | `sglobal.f90:121` |
| `vsmall` (the `ISZERO` band) | `1.0e-20` | `sglobal.f90:190` |

Relevant units, since almost every finding below turns on one:

| Quantity | Unit | Source |
|---|---|---|
| `DTUZ` | seconds | `AL_C.F90:172` |
| `TIMEUZ`, `TMELT` | hours | `AL_D.f90:155`, `SMmod.f90:57` |
| `SD`, `SF` | mm of snow | `AL_D.f90:222-224` |
| `SMELT`, `PNSNOW` | mm of water | `SMmod.f90:56`, `:72` |
| `PNET` | mm/s (scalar, not per element) | `AL_D.f90:145` |
| `ESAT`, `ESATA` | mb (see C3) | `SMmod.f90:370`, `:376` |
| `PO` | Pa (see C3) | `SMmod.f90:372` |

Array layouts (both are slug-major, which is the correct choice — see §2):

| Array | Declared shape | Declared at |
|---|---|---|
| `SMELT` | `(max_no_snowmelt_slugs, total_no_elements)` | `SMmod.f90:56`, `:111` |
| `TMELT` | `(max_no_snowmelt_slugs, total_no_elements)` | `SMmod.f90:57`, `:110` |

---

## Conclusion up front

The module is small and its control flow is now clean, but four findings are
serious and three of them are in the energy-budget branch, which is the branch
the module exists for.

1. **The meltwater-slug compaction assumes slugs are released in creation
   order, and they are not.** When a pack is exhausted in a single step the new
   slug's travel time is zero, so it releases immediately while older slugs are
   still in flight. The compaction then shifts the array down by the *count* of
   released slugs rather than by *which* slugs released: one slug's water is
   silently deleted from the model and another is delivered to the ground
   twice. This fires at the end of essentially every melt season, on every
   element that still has an in-flight slug. **§C1, high.**

2. **The saturation-vapour-pressure polynomial is a fit about +15 °C that goes
   negative below about -7 °C.** It is evaluated at the *snow surface*
   temperature `TS` (which is ≤ 0 by construction) and at air temperature `TA`
   in a snow module. At 0 °C it is 19 % low; at -5 °C, 65 % low; at -10 °C it
   returns -2.40 mb. **§C2, high.**

3. **`ESAT` is in mb while `PO` is in Pa**, so the specific-humidity formula on
   lines 373 and 379 underestimates `Q` and `QA` by a factor of ~100. Latent
   heat `HFE` and sublimation depth `ESM` are consequently ~1 % of their
   intended magnitude — the model effectively has no sublimation. The `*100.0d0`
   on line 372 is the likely single culprit. **§C3, high.**

4. **`SMELT` and `TMELT` are allocated lazily from `SMIN`, but `FRINIT` reads
   into them during initialisation, before `SMIN` has ever run.** A hotstart
   file carrying any non-zero `NSMC` writes through a null dope vector.
   **§C4, medium-high, latent crash.**

On performance, the module is not a hot spot in the way `OCmod`/`VSmod` are,
but three costs are free to remove: a `LOG` of three run-constant scalars
evaluated per element per timestep (**§P1**), an elevation-to-pressure
conversion recomputed per element per timestep from data that never changes
(**§P2**), and a one-time initialiser invoked twice per element per timestep
(**§P3**). The single-pass rewrite that fixes C1 is also strictly cheaper than
the current two-pass version (**§P4**).

On memory, `SMELT`+`TMELT` cost **6.4 kB per element** (400 slugs × 8 B × 2)
for a high-water mark that is realistically single digits — 320 MB at 50 000
elements, 1.6 GB at the `NELEE` cap — while *simultaneously* being too small to
survive a deep pack on a fine timestep, in which case the module calls `STOP`
(**§M1**, **§C5**). A further 1 MB of `IMET` is written by `INSM` and never
read by anything (**§M5**).

Two source comments claiming performance wins (`SMmod.f90:368`, `:495`) do not
describe real effects; see **§P6**.

---

## 1. Correctness

### C1 — slug compaction assumes release in creation order — *high*

`SMmod.f90:476-505`:

```fortran
DO KL = 1, NSMC(IEL)
   IF (TIMEUZ >= tmelt(KL, IEL)) THEN
      pnsnow = SMELT(KL, IEL) + pnsnow
      NCC = NCC + 1
   END IF
END DO

IF (NCC > 0) THEN
   NSMC(IEL) = NSMC(IEL) - NCC
   KK = NSMC(IEL)
   IF (KK > 0) THEN
      DO KL = 1, KK
         KKK = KL + NCC
         tmelt(KL, IEL) = tmelt(KKK, IEL)
         SMELT(KL, IEL) = SMELT(KKK, IEL)
      END DO
   END IF
END IF
```

The scan is order-independent: it sums *any* slug whose release time has
passed, wherever it sits. The compaction is not: it unconditionally shifts the
tail down by `NCC`, which is only correct if the released slugs were exactly
slots `1..NCC`. That holds only if `TMELT` is non-decreasing in slot order.

It is not. The release time assigned at `SMmod.f90:469` is

```fortran
tmelt(NNC, IEL) = (0.7448d0 * SD(IEL) / 1000.0d0 + 1.429d0) * SD(IEL) / 1000.0d0 + TIMEUZ
```

so slug *k* releases at `TIMEUZ_k + f(SD_k)` with `f(s) = (0.7448 s + 1.429) s`
hours, `s = SD/1000`. `f` is monotone increasing in `SD`, and `SD` is written
*before* this line, at `SMmod.f90:430-440`, by the melt/evaporation deduction.
A later slug therefore has an earlier release time whenever the pack shrinks by
more than `ΔTIMEUZ` worth of travel time between two steps.

**The guaranteed trigger is pack exhaustion.** When line 437 sets
`SD(IEL) = zero`, `f(0) = 0`, so the slug created in that step has
`TMELT = TIMEUZ` and satisfies `TIMEUZ >= TMELT` in the very same call, while
every older slug — created when the pack still had depth — is still pending.

Worked failure, with `DTUZ = 900 s` (0.25 h):

| Step | `TIMEUZ` | `SD` after deduction | slug created | `TMELT` |
|---|---|---|---|---|
| 1 | 0.00 h | 400 mm | slot 1 | `f(0.4) + 0 = 0.691 h` |
| 2 | 0.25 h | 0 mm (pack exhausted) | slot 2 | `f(0) + 0.25 = 0.25 h` |

At step 2 the scan visits slot 1 (`0.25 >= 0.691` false) and slot 2
(`0.25 >= 0.25` true), so `pnsnow += SMELT(2)` and `NCC = 1`. The compaction
then sets `NSMC = 1`, `KK = 1`, and copies slot 2 over slot 1. Two things go
wrong at once:

- **Slot 1's water is deleted.** It was never added to `pnsnow`, never reached
  the ground, and is now overwritten. It is gone from the water balance.
- **Slot 2's water is retained.** It has already been paid out into `pnsnow`,
  and its `TMELT` of 0.25 h is now permanently in the past, so at step 3 it is
  released *again*.

Net effect: one slug's mass lost, another's doubled. The two errors do not
cancel — the amounts are unrelated — and both are invisible to any check in
this module.

The precondition is only `NSMC >= 2` at the moment of exhaustion, i.e. that the
previous step's slug has not yet drained: `f(SD_prev) > DTUZ/3600`. For
`SD_prev = 400 mm` and a 15-minute `DTUZ` that is `0.691 > 0.25` — comfortably
true. Since every snowpack that forms is eventually exhausted, this fires at
least once per element per snow season, and again on every intermittent
melt-out during the season.

A partial melt (rather than full exhaustion) triggers it too, but needs a
larger drop: `f(SD_1) - f(SD_2) > DTUZ/3600`, and since `df/ds ≈ 4.41 h/m` at
`s = 2`, that is roughly `ΔSD > 227 · DTUZ[h]` mm. At hourly steps that is an
implausible 227 mm/step; at sub-hourly steps it becomes reachable. The
exhaustion case above is the one that matters, and it is unconditional.

**Fix.** Compact by *retention*, in one pass, which removes `NCC` entirely and
is also cheaper than the current two-pass form:

```fortran
KK = 0
DO KL = 1, NSMC(IEL)
   IF (TIMEUZ >= tmelt(KL, IEL)) THEN
      pnsnow = pnsnow + SMELT(KL, IEL)
   ELSE
      KK = KK + 1
      tmelt(KK, IEL) = tmelt(KL, IEL)
      SMELT(KK, IEL) = SMELT(KL, IEL)
   END IF
END DO
NSMC(IEL) = KK
```

This is stable, correct for any release order, touches each slot once, and
needs no `IF (NSMC(IEL) > 0)` guard (a zero-trip loop leaves `KK = 0`).

Note that this changes results, so it needs a regression baseline: any run with
snow will shift. That is the point — the current results are wrong — but it
should not be slipped in as a silent refactor.

### C2 — the vapour-pressure polynomial is a +15 °C fit used almost entirely below 0 °C — *high*

`SMmod.f90:369-376`:

```fortran
TEMP_RATIO = (TS(IEL) / five) - three
ESAT = (17.044d0 + TEMP_RATIO * (5.487d0 + TEMP_RATIO * (0.776d0 + &
        TEMP_RATIO * (0.1063d0 + TEMP_RATIO * 0.003d0))))
```

The scaled variable is `r = T/5 - 3`, so `r = 0` at `T = 15 °C`, where the
polynomial returns its leading coefficient, 17.044 mb. Saturation vapour
pressure at 15 °C is 17.02 mb, so this is a quartic expansion centred on
+15 °C. Evaluated away from that centre it degrades fast, and then diverges:

| `T` | `r` | polynomial (mb) | reference `e_s` (mb) | error |
|---|---|---|---|---|
| 15 | 0 | 17.044 | 17.02 | +0.1 % |
| 10 | -1 | 12.230 | 12.26 | -0.2 % |
| 5 | -2 | 8.372 | 8.72 | -4 % |
| 0 | -3 | 4.940 | 6.11 | **-19 %** |
| -5 | -4 | 1.477 | 4.22 (water) / 4.02 (ice) | **-65 %** |
| -10 | -5 | **-2.404** | 2.86 (water) / 2.60 (ice) | **sign inverted** |

(Reference values from the Magnus form `6.112 exp(17.62 T / (243.12 + T))`.)

The root is at `r ≈ -4.4`, i.e. **`T ≈ -7.0 °C`**: below that the routine
returns a negative saturation vapour pressure, and hence via line 373 a
negative specific humidity.

This matters more here than it would elsewhere, because of *which* temperatures
are substituted:

- Line 369 uses `TS(IEL)`, the snow-surface temperature. `TS` is written only
  at `SMmod.f90:415` from `TS2`, which is either `zero` (melting branch) or
  strictly negative (`LTZERO(TS2)` branch), floored at -50 °C by line 404. So
  `TS <= 0` always after the first step, and the polynomial is *never*
  evaluated inside its accurate range for the snow surface.
- Line 375 uses `TA(MS)`. The energy-budget branch runs whenever there is a
  pack or sub-freezing air, so `TA` too is routinely below the divergence point.

There is one mitigating structural detail worth stating precisely, because it
bounds the damage. The evaporation driver is a *difference*:

```
E = RHOA * DN * (Q - QA)
```

and with the denominator effectively constant (see C3), `Q - QA ∝ ESAT - EA =
e_s(TS) - e_s(TA) + VPD`. So the polynomial's *offset* error largely cancels and
what survives is the error in its *slope*. That is not a rescue: `d/dT` of the
polynomial at -10 °C is 0.840 mb/°C, against a true `de_s/dT` over ice of about
0.19-0.24 mb/°C. The gradient is roughly **3.5-4× too steep** exactly where the
module spends its time, so sublimation and condensation are over-driven by the
temperature difference even where the offsets cancel.

I cannot tell from the source whether this polynomial was ever intended for
sub-zero use or was inherited from the ET side of the model and reused. The
numbers above are unambiguous about its behaviour, though. Replacing it with a
Magnus/Tetens form over ice for `T < 0` and over water otherwise is a small,
self-contained change and removes both the divergence and the slope error.

### C3 — `ESAT` (mb) and `PO` (Pa) are mixed in the humidity formula — *high*

`SMmod.f90:372-379`:

```fortran
PO = 1012.0d0 * (one - 0.0065d0 * ZGRUND(IEL) / 288.0d0) * 100.0d0
Q  = (0.62197d0 * ESAT) / ((PO / 1.0045d0) - (0.37803d0 * ESAT))
...
QA = (0.62197d0 * EA)   / ((PO / 1.0045d0) - (0.37803d0 * EA))
```

The standard specific-humidity relation `q = 0.622 e / (p - 0.378 e)` requires
`e` and `p` in the *same* units. They are not here:

- `1012.0d0` is sea-level pressure in **mb**; the barometric factor is
  dimensionless; the trailing `* 100.0d0` converts to **Pa**. So `PO ≈ 101 200`
  at sea level.
- `ESAT` is in **mb** (17.044 at 15 °C — see C2).

Numeric check at 15 °C, sea level, as written:

```
Q = 0.62197 * 17.044 / (101200/1.0045 - 0.37803*17.044)
  = 10.601 / (100746 - 6.44)
  = 1.052e-4
```

Correct `q_sat(15 °C, 1012 mb)` is about **1.06e-2** (10.6 g/kg). The result is
low by a factor of ~100 — precisely the mb→Pa factor. Making the units
consistent either way reproduces the right answer:

```
ESAT in Pa: 0.62197*1704.4 / (100746 - 644.3) = 1.0589e-2   ✓
PO   in mb: 0.62197*17.044 / (1007.5 - 6.44)  = 1.0590e-2   ✓
```

`PO` has no other reader in the codebase (`SMmod.f90:373` and `:379` are its
only uses), so the trailing `* 100.0d0` on line 372 is the most likely single
defect; dropping it is the minimal fix.

Two consequences follow:

- **Sublimation is ~1 % of its intended magnitude.** `E = RHOA*DN*(Q-QA)`
  inherits the factor directly, and `E` drives both the latent heat
  `HFE = (LVW + LWI - CPI*TS)*E*DTUZ` (line 385, and thence `HFT` and the melt)
  and the depth loss `ESM = E*DTUZ/RHOS` (line 426). In effect the energy-budget
  snowpack neither sublimates nor gains from deposition to any meaningful
  degree, and `HFT` is a convection + radiation + ground budget only.
- **The non-linearity of the humidity formula is inert.** With `|ESAT| ≈ 17`
  against `PO/1.0045 ≈ 100 746`, the `0.37803*ESAT` term contributes 6 parts in
  100 000. `Q` is a straight linear function of `ESAT` for all practical
  purposes, which is why the offset cancellation described in C2 holds so
  cleanly. Fixing the units restores the non-linearity and will change results
  by more than the naive 100× on `E` alone.

C2 and C3 should be fixed together and validated together; each changes the
magnitude of the same term, and fixing only C3 amplifies C2's slope error by
two orders of magnitude.

### C4 — `SMELT`/`TMELT` are unallocated when the hotstart reader fills them — *medium-high, latent crash*

`initialise_smmod` (`SMmod.f90:107-114`) is called from exactly one place:
`SMIN`, at `SMmod.f90:701`. `SMIN` is called only from `ETIN`
(`ETmod.f90:757`, `:762`), i.e. only once the simulation loop is running.

But `FRINIT` reads into both arrays during initialisation
(`FRmod.f90:1362-1364`):

```fortran
(NSMC (IEL), IEL = NGDBGN, total_no_elements), atemp, &
((SMELT (K, IEL), K = 1, NSMC (IEL)), IEL = NGDBGN, total_no_elements), atemp, &
((tmelt(K, IEL), K = 1, NSMC (IEL)), IEL = NGDBGN, total_no_elements), atemp, &
```

and the ordering in `FRINIT` is `IF (BEXSM) CALL INSM` at line 1300, then the
hotstart block at line 1351. `INSM` does not allocate — it only zeroes `NSMC`
(`FRmod.f90:5636`). So at line 1363 both allocatables are still unallocated.

Whether this crashes depends on the file contents, which is what makes it
latent: `NSMC(IEL)` is read from the hotstart file *earlier in the same*
`READ`, so if every element's slug count is zero the implied-`DO` has zero
trip count and nothing is dereferenced. A hotstart taken mid-melt-season —
exactly when you would want one — has non-zero counts and writes through a
null base address.

`-fcheck=bounds` is set only for `Debug` (`CMakeLists.txt:694`) and does not
cover allocation status; `-fcheck=pointer` is not set in any configuration. So
this is not caught even in a debug build; it presents as a raw segfault in the
`READ`.

The mirror-image case exists on the write side. `run_sim.f90:372-373` writes
`(SMELT(K,IEL), K=1,NSMC(IEL))` guarded only by `BHOTPR`, not by `BEXSM`. With
snow disabled, `INSM` never runs, so `NSMC` — a static
`INTEGER :: NSMC(NELEE)` with no initialiser (`AL_D.f90:194`) — is never
zeroed. In practice gfortran and ifx place it in `.bss`, which the loader
zeroes, so the trip count is zero and it works; but that is a platform
guarantee, not a language one, and the module is relying on it.

**Fix.** Call `initialise_smmod` from `FRINIT` before the hotstart block
(and unconditionally, not under `BEXSM`, since `run_sim`'s writer is
unconditional), and make the routine idempotent as described in M2. That also
removes the per-element call in `SMIN` — see P3.

### C5 — the 400-slug cap terminates the process — *medium*

`SMmod.f90:451-454`:

```fortran
IF (NSMC(IEL) > max_no_snowmelt_slugs) THEN
   WRITE (6, 30) NSMC(IEL), IEL
   STOP
END IF
```

The bound check itself is correct and correctly placed: `NSMC` is incremented
at line 447 and the check precedes the first write at line 460, so no
out-of-bounds store occurs. The source already carries a comment
(`SMmod.f90:450`) proposing an error flag instead. Two things make this worth
acting on rather than leaving as a note.

First, the cap is reachable under ordinary configurations. A slug's residence
is `f(SD) = (0.7448 s + 1.429) s` hours with `s = SD/1000`, and slugs are
created once per `DTUZ`, so the steady-state count is about
`f(SD) * 3600 / DTUZ`:

| `SD` | `f(SD)` | at `DTUZ` = 3600 s | 900 s | 300 s | 60 s |
|---|---|---|---|---|---|
| 500 mm | 0.90 h | 1 | 4 | 11 | 54 |
| 1 000 mm | 2.17 h | 3 | 9 | 27 | 131 |
| 2 000 mm | 5.84 h | 6 | 24 | 71 | 351 |
| 3 000 mm | 11.0 h | 11 | 44 | 132 | **660 → STOP** |

`f` is quadratic in depth, so alpine or high-latitude catchments on a
sub-5-minute unsaturated-zone timestep hit the wall. The failure mode is a
`STOP` deep inside a long run, after the hotstart interval has passed, with the
message going to unit 6 rather than to `PPPRI`.

Second, `STOP` from a library-style module is the wrong exit for a code that is
also driven through the visualisation interface. Raising an error flag to the
host, as the comment suggests, lets the run terminate through the normal
shutdown path and flush its output files.

Sizing the array from an actual runtime bound rather than a compile-time
constant would remove the failure entirely; see M1.

### C6 — `HFE` uses the pre-clamp `E`, `ESM` uses the post-clamp `E`; `EE` is dead — *low to medium*

`SMmod.f90:385` computes latent heat from `E` as returned by line 382. Then at
`SMmod.f90:419-421`:

```fortran
EE = E
! EVAPORATION CHANGES DEPTH BUT CONDENSATION DOES NOT
IF (LTZERO(E) .AND. ISZERO(TS(IEL))) E = zero
```

and `ESM = E * DTUZ / RHOS` at line 426 uses the clamped value.

So for a melting pack (`TS == 0`) with condensation (`E < 0`), the energy
budget has already credited the pack with the latent heat of that condensation
via `HFE`, but the mass never arrives: `ESM` is forced to zero and the
condensed water is added to neither `SD` nor `SMELT`. Energy in, mass nowhere.
The quantity is small in absolute terms, and C3 currently suppresses it by a
further two orders of magnitude, but it becomes visible once C3 is fixed.

`EE` is assigned and never read anywhere in the module. Its placement — capturing
`E` immediately *before* the clamp — strongly suggests it was the unclamped
value kept for exactly this reconciliation, or for reporting actual
evaporation, and that the consumer was lost. It should either be given back its
purpose or deleted; leaving a dead capture of a value that is about to be
destroyed is a trap for the next reader.

Separately, the guard's condition does not match its comment. The comment says
condensation does not change depth; the code suppresses it *only* when
`TS == 0`. For a sub-freezing pack, `E < 0` gives `ESM < 0` and line 439
therefore *increases* `SD`. That may well be intended as rime/deposition — it
is physically defensible, and converting the deposited water to snow depth via
`RHOS` is the right sort of thing to do — but the comment says the opposite, so
one of the two is wrong and the source does not say which. This needs a physics
decision, not a code change chosen by guesswork.

### C7 — `SD → 0` denominators in the energy-budget branch — *low, currently unreachable*

`SMmod.f90:392-403`:

```fortran
IF ((SD(IEL) <= 100.0d0) .AND. (LTZERO(HFT))) THEN
   HFT = (TA(MS) - TS(IEL)) * (CPI * RHOS * SD(IEL))
END IF
...
TS2 = (HFT / (CPI * RHOS * SD(IEL))) + TS(IEL)
```

With `SD == 0` exactly, line 403 divides by zero: `0/0 → NaN` if the guard on
392 fired, `HFT/0 → ±Inf` otherwise. I traced the dispatch and believe this is
currently unreachable:

- From `SMET` (`SMmod.f90:651`), `SM` runs if `precip > 0` or `SD > 0`. In the
  `precip > 0, SD == 0` case, `SMET` is only entered at all when `SD > 0` or
  `TA <= 0` (`SMmod.f90:729`), and `TA <= 0` makes line 324 add the new
  snowfall to `SD` first, so `SD > 0` by line 403.
- From `SMIN`'s `NSMT == 1` path (`SMmod.f90:711`), `SM` is called only under
  `GTZERO(SD(IEL))`.

So the invariant holds, but it holds by a three-way argument across two
routines and depends on `GTZERO`'s strict `a > 0` semantics
(`sglobal.f90:264-267`) — not on anything local to `SM`. It is worth a cheap
explicit guard at line 392, both to make the invariant local and because the
*near*-zero case is genuinely ill-conditioned: a pack of 1e-12 mm produces a
`TS2` of order 1e12, and although the -50 °C floor on line 404 catches the
negative side, there is no upper clamp. The positive side is then rescued only
downstream, by the `TSM > SD` cap at line 430 bounding the melt to the
available depth. That is a lot of load-bearing weight on a cap three blocks
away.

### C8 — unguarded `U(MS)` and an unbounded Richardson correction — *low*

`SMmod.f90:343-353`:

```fortran
DN = ((0.4d0 / LOG((ZUS - ZDS) / ZOS))**2) * U(MS)
RICH = 9.81d0 * (ZUS - EFFDEP / 1000.0d0 - ZDS) * (TA(MS) - TS(IEL)) &
     / ((TA(MS) + 273.0d0) * U(MS) * U(MS))
IF (TA(MS) > TS(IEL)) THEN
   DN = DN / (1.0d0 + 10.0d0 * RICH)
ELSE
   DN = DN * (1.0d0 - 10.0d0 * RICH)
END IF
```

Three notes, in decreasing order of concern:

- **`U(MS) == 0` divides by zero in `RICH`.** Calm conditions are not exotic in
  a snow-covered catchment. `DN` itself would correctly be zero, but `RICH`
  becomes `±Inf` (or `NaN` if the numerator is also zero) before that matters,
  and `DN * (1 - 10*Inf)` propagates. A wind-speed floor, or a short-circuit
  `IF (ISZERO(U(MS))) DN = zero` ahead of the correction, closes this.
- **The stability correction is unbounded on the unstable side.** The sign
  handling is right — `TA > TS` is stable, `RICH > 0`, and `DN` is divided
  down; `TA < TS` is unstable, `RICH < 0`, and `DN * (1 - 10*RICH) > DN`
  amplifies — but there is no cap on the amplification. For strongly unstable
  conditions over a cold pack the exchange coefficient can be multiplied
  arbitrarily. The stable branch is safe (`1 + 10*RICH > 1` whenever
  `RICH > 0`), so this is one-sided. A conventional `|RICH| <= 0.2`-style clamp
  would bound it.
- **`EFFDEP` is a constant zero.** It is set at line 306 and never written
  again, so `- EFFDEP / 1000.0d0` on line 346 is dead arithmetic. The 1996
  history entry (`SMmod.f90:43`) records that initialising it *was* the fix for
  a prior undefined-variable bug, and the 1992 comment on line 341 explains it
  was deliberately removed from `DN` to avoid `LOG` of a non-positive number.
  It is now vestigial and should be deleted from line 346 as well, so the
  source stops implying a snow-depth dependence that does not exist.

### C9 — line 411 assumes `TS <= 0` — *low*

```fortran
HFT = HFT - ((-TS(IEL)) * CPI * RHOS * SD(IEL))
```

This subtracts the heat needed to warm the pack to 0 °C, which is correct only
for `TS < 0`; for `TS > 0` it *adds* heat. After the first energy-budget step
`TS` is always `<= 0` (line 415 writes either `zero` or a negative `TS2`), so
the only exposure is the initial value: `INSM` sets `TS(IEL) = TSIN` read from
record SM4 (`FRmod.f90:5581`, `:5638`), and forces `TSIN = 0` only for `MSM = 1`
(`FRmod.f90:5586`) — i.e. only in the branch that never reaches this line. A
positive `TSIN` with `MSM = 2` is not rejected at read time and would inflate
the first step's melt. Clamping `TSIN` to `<= 0` in `INSM` is the natural place
to fix it.

### C10 — the -50 °C floor destroys energy — *informational*

`SMmod.f90:404` clamps `TS2` and does not adjust `HFT` to match, so the heat
implied by the clamped-off temperature range simply vanishes from the budget.
This is a deliberate stability guard and is documented as such in the header
(`SMmod.f90:227-230`), so I list it only for completeness: if a closed energy
budget is ever wanted for validation, this is one of the two places where it is
not closed (C6 being the other).

---

## 2. Performance

Context for the numbers below: `SM` runs per element with snow, per unsaturated-zone
timestep; `SMIN` runs *twice* per element per timestep (`ETmod.f90:757`, `:762`).
None of these are inner-loop-over-cells routines like `VSmod`'s column solve, so
absolute savings are modest. All four leading items are, however, free —
no result change, no restructuring.

### P1 — `LOG` of three run-constant scalars, per element, per timestep — *low to medium*

`SMmod.f90:343`:

```fortran
DN = ((0.4d0 / LOG((ZUS - ZDS) / ZOS))**2) * U(MS)
```

`ZUS`, `ZDS` and `ZOS` are module scalars written once by `INSM`
(`FRmod.f90:5590-5600`) and never modified afterwards. The entire
`(0.4/LOG((ZUS-ZDS)/ZOS))**2` factor is therefore a run constant, recomputed
for every snow-covered element on every timestep.

A `LOG` is roughly 20-40 cycles on current hardware and is not vectorisable
here, and it sits at the top of the energy-budget branch, so it is on the
critical path for every element. Hoisting it into a module-level saved value
computed at the end of `INSM` (or lazily, guarded like the other one-time
state) reduces line 343 to a multiply.

This is the clearest single win in the module. It will not show up as a large
fraction of total runtime — `SMmod` is not where the time goes — but it is a
transcendental function evaluated `N_snow_elements × N_steps` times to produce
the same number every time.

### P2 — `PO` recomputed per element per timestep from immutable data — *low*

`SMmod.f90:372`:

```fortran
PO = 1012.0d0 * (one - 0.0065d0 * ZGRUND(IEL) / 288.0d0) * 100.0d0
```

`ZGRUND` is ground-surface elevation (`sglobal.f90:159`), fixed for the whole
run. `PO` is a pure function of it, and is used only on lines 373 and 379. So
this is a load from a 250 000-element static array plus four flops, per element
per timestep, to recompute a per-element constant.

Two options. A precomputed `PO` table costs `8 × total_no_elements` bytes and
turns the whole thing into one load — but it adds an array where `SMmod`'s
memory story (§M1) is already the weak part. Given `E` and hence `PO` only
matter to the sublimation term, and that term is currently near-inert (C3), the
cheaper move is to fold the constants (`1012 * 100 / 288 * 0.0065` collapses to
a single coefficient) and leave the elevation lookup:

```fortran
PO = 101200.0d0 - 2.2847222d0 * ZGRUND(IEL)   ! == 1012*(1 - 0.0065*Z/288)*100
```

Note this must be revisited alongside C3 — if the `* 100.0d0` is removed, the
folded constant changes accordingly.

`(PO / 1.0045d0)` appears on both lines 373 and 379; since `PO` is unchanged
between them, any compiler at `-O2` will common-subexpression it, so that
particular repetition costs nothing. It is worth hoisting anyway for
readability, not for speed.

### P3 — `initialise_smmod` is invoked twice per element per timestep — *low*

`SMmod.f90:701` calls it unconditionally at the top of `SMIN`, before the
`NSMT` dispatch, so it runs even for elements that do no snow work at all. With
`SMIN` itself called twice per element per ET step, that is `2 × N_elements ×
N_steps` invocations of a routine whose entire job was finished on the first
one.

I want to be accurate about the cost: the routine is tiny, in the same module,
and `-O2` will almost certainly inline it down to a load-and-test on the saved
`FIRST` flag with a perfectly predicted branch. At 50 000 elements and 20 000
steps that is 2×10⁹ near-free operations — call it a couple of seconds if it is
*not* inlined, and essentially nothing if it is. So this is not a real hot spot.

It is worth fixing regardless, because the fix is the same one C4 needs: move
the call to `FRINIT`, ahead of the hotstart read. That removes the per-element
cost and the latent crash in one edit, and it makes the allocation lifetime
legible instead of "whenever the first element with snow happens to run".

### P4 — the slug scan and compaction should be one pass — *low, and it fixes C1*

The current code (`SMmod.f90:479-503`) walks `1..NSMC` to accumulate and count,
then walks `1..KK` again to shift. The single-pass retention form given in C1
touches each slot exactly once, drops the `NCC` bookkeeping and the two nested
`IF` guards, and is correct for arbitrary release order.

The counts are small — typically 1 to 10 slugs (see the table in C5) — so the
throughput difference is negligible. Take this change for C1; the performance
improvement is incidental.

### P5 — repeated `RHOS` divisions and global reloads — *low*

`RHOS` is divided into things at lines 313 and 426 and multiplied at 393, 411,
413, 460 and 462. Division is ~4× the latency of multiplication; hoisting
`RRHOS = one / RHOS` once at the top would convert both divisions. Similarly,
`TA(MS)` is loaded around ten times and `TS(IEL)` about eight, each through a
250 000-element static array.

Realistically the compiler already handles the reloads: `SM` contains no
procedure calls, so nothing can alias the module arrays across the body and
gfortran/ifx will keep the values in registers. The division hoist is a genuine
but tiny win. I list this mainly as a readability point — caching `TAM`,
`TSI`, `SDI` in locals would make the routine much easier to read than its
current density of `(MS)` and `(IEL)` subscripts, at no cost.

There is also a pointless round trip at lines 313 and 462: `SF = pnsnow / RHOS`
followed later by `SMELT = (USM + SF) * RHOS`, which reconstitutes `pnsnow`
through a division and a multiplication and loses a couple of ulps doing it.
`SMELT = USM * RHOS + pnsnow_saved` is both exact and cheaper — though note
`pnsnow` is zeroed at line 315, so this needs the value captured (`TOPNET`
already holds it, and is otherwise dead — see M4).

### P6 — two source comments claim performance effects that do not exist — *cleanup*

Both are recent additions and both are misleading to a future reader:

`SMmod.f90:368`:

```fortran
! High-Performance Fix: Pre-calculate the temperature ratio to avoid repeated division/subtraction
TEMP_RATIO = (TS(IEL) / five) - three
```

The expression it replaces appeared inside a Horner-form polynomial, where
`(T/5 - 3)` was already a single common subexpression that every optimising
compiler eliminates. The rewrite is a readability improvement and nothing more;
the generated code at `-O2` is the same. Calling it a "High-Performance Fix"
invites someone to preserve it as load-bearing.

`SMmod.f90:495`:

```fortran
! Performance Reversion: Explicit DO loop is faster for micro-arrays
! than building F90 array-slice dope vectors.
```

The claim does not hold for this access pattern. `tmelt(KL, IEL)` with `KL`
varying and `IEL` fixed is contiguous in column-major storage, so
`tmelt(1:KK, IEL) = tmelt(1+NCC:KK+NCC, IEL)` is a contiguous same-rank
assignment with statically known stride — gfortran and ifx both lower it to an
inline copy or a `memmove`, with no descriptor constructed. The general worry
about dope vectors applies to non-contiguous slices and to array arguments
passed to assumed-shape dummies, neither of which is what this line does.

In practice `KK` is 0-10, so neither form matters and the loop is fine to keep.
The comment should go, or be corrected to say the loop was kept for clarity.

### P7 — minor items

- `MS = NMC(IEL)` is computed in `SMIN` (line 703), again in `SMET` (line 592),
  and again in `SM` (line 309), for the same `IEL` within one call chain. Three
  loads of the same value from a static array. Trivial, but passing `MS` down
  would be clearer as well as cheaper.
- `SMET`'s zeroing loop (`SMmod.f90:646-648`) writes `S(1:K)` one element at a
  time. `S(1:K) = zero` is contiguous and vectorises; `K = NRD(N)` is a root
  depth in cells, so at most `LLEE`. Marginal, but the explicit loop buys
  nothing here. (Note the ETmod analysis's §P2 finding that `S` is written and
  never read at all — if that is confirmed, this loop should be deleted rather
  than optimised.)
- `SMET`'s `ISZERO(SNDEP)` branch (`SMmod.f90:603-605`) is an empty `CONTINUE`
  used as a structural placeholder. It is harmless and arguably documents the
  three-way split, but `.NOT. ISZERO(SNDEP) .AND. SNDEP >= VHT(N)` restructured
  as a two-branch `IF` would express the same logic without a no-op arm.

---

## 3. Memory and variable management

### M1 — `SMELT` + `TMELT` cost 6.4 kB per element for a single-digit high-water mark — *high*

`SMmod.f90:110-111`:

```fortran
ALLOCATE (TMELT(max_no_snowmelt_slugs, total_no_elements))
ALLOCATE (SMELT(max_no_snowmelt_slugs, total_no_elements))
```

With `max_no_snowmelt_slugs = 400` (`sglobal.f90:134`) and 8-byte doubles, that
is 3 200 B per element per array, **6 400 B per element** in total:

| `total_no_elements` | `SMELT` + `TMELT` |
|---|---|
| 10 000 | 64 MB |
| 50 000 | 320 MB |
| 100 000 | 640 MB |
| 250 000 (`NELEE` cap) | 1.6 GB |

Against that, the actual occupancy from the C5 table is typically 1-10 slugs
and reaches the low hundreds only for deep packs on fine timesteps. So the
arrays are roughly two orders of magnitude over-provisioned for the common case
— and, as C5 shows, *still* not large enough for the uncommon one, at which
point the module aborts the run. That is the worst of both: it pays for
capacity it does not use and fails anyway when it needs more.

The dimension order is correct and should stay: slug-major means
`SMELT(1:NSMC, IEL)` is contiguous, so a typical element's slugs sit in one or
two cache lines and both the scan and the compaction are sequential. The
per-element stride is 3 200 B, so each element touches a fresh region, but that
is one cache line per element per array either way — the layout is not the
problem, the extent is.

Two directions, in order of preference:

1. **Size it from a runtime bound.** The maximum residence time is
   `f(SD_max)` and slugs arrive once per `DTUZ`, so
   `n_max ≈ f(SD_max) * 3600 / DTUZ_min + 2`. `SD_max` is not known a priori,
   but a generous configured ceiling (say 10 m of snow) with the actual minimum
   `DTUZ` gives a defensible bound, computed once in `INSM` and used for the
   `ALLOCATE`. This shrinks the common case by ~100× and lets deep-snow runs
   succeed instead of hitting `STOP`.
2. **Reduce `max_no_snowmelt_slugs` and add graceful overflow.** Less
   satisfying, but much less invasive: drop the constant to something like 32,
   and on overflow merge the two oldest slugs (sum the `SMELT`, take the later
   `TMELT`) rather than aborting. That bounds memory at 512 B/element and turns
   a fatal error into a bounded accuracy loss.

Either way this should be settled together with C5, since they are the same
sizing decision seen from opposite ends.

### M2 — the allocation guard is a saved flag, with no `STAT=` and no deallocator — *medium*

```fortran
SUBROUTINE initialise_smmod
   LOGICAL :: first=.TRUE.
   if (FIRST) then
      ALLOCATE (TMELT(max_no_snowmelt_slugs,total_no_elements))
      ALLOCATE (SMELT(max_no_snowmelt_slugs,total_no_elements))
      FIRST = .FALSE.
   endif
END SUBROUTINE initialise_smmod
```

Four issues, all cheap to fix:

- **The guard is not self-consistent with the state it guards.** `first` gets an
  implicit `SAVE` from its initialiser (which the header at `SMmod.f90:96-99`
  correctly documents), but it tracks *whether the routine has run*, not
  *whether the arrays are allocated*. If anything ever deallocates them, a
  later call is a silent no-op and the next access goes through a null
  descriptor. `IF (.NOT. ALLOCATED(SMELT))` asks the actual question, is
  idempotent by construction, and needs no saved state at all.
- **No `STAT=`.** Per M1 this allocation can legitimately be 1.6 GB. Failure
  currently produces the runtime's generic abort with no indication of which
  array or what size was requested. A `STAT=`/`ERRMSG=` pair reporting
  `max_no_snowmelt_slugs`, `total_no_elements` and the byte count would turn an
  opaque crash into a one-line diagnosis.
- **No deallocator.** Nothing ever frees these arrays. For the standalone
  executable that is harmless — the process exits — but `SMmod` is reachable
  through the visualisation interface, and any embedding that runs more than
  one simulation in a process leaks 6.4 kB/element per run and, worse, silently
  reuses the *previous* run's slug contents because the saved `first` suppresses
  reallocation. A `finalise_smmod` mirroring the pattern used elsewhere in the
  codebase closes both.
- **`total_no_elements` is not validated.** It is initialised to `-1`
  (`sglobal.f90:140`) and set by `FRIND`. Since the call currently happens from
  `SMIN` — deep into the run — it is certainly positive by then, but moving the
  call to `FRINIT` (C4/P3) puts it much closer to the initialisation, so an
  explicit `IF (total_no_elements <= 0)` guard becomes worth having.

Note that the arrays are also never initialised after allocation. That is safe
only while `NSMC` starts at zero everywhere, which `INSM` guarantees when
`BEXSM` is true and `.bss` zeroing guarantees otherwise (see C4). Since the
allocation is happening anyway, `SMELT = zero` / `TMELT = zero` removes the
dependence on both — at the cost of first-touching the whole 6.4 kB/element,
which argues for fixing M1 first.

### M3 — per-element working state lives in module scalars — *design, high leverage*

`USM`, `ESM`, `RHOS`, `TOPNET`, `PNSNOW` (`SMmod.f90:59-72`) are module-level
scalars used as scratch for whichever element is currently being processed, and
`RHOS` in particular is written in `SMET` (lines 597-598) and *read* in `SM`
(lines 313, 393, 411, 413, 426, 460, 462) — an implicit cross-routine contract
with nothing enforcing it.

I traced whether that contract can be violated, and it currently cannot:

- `SM` is called from `SMET:653`, after `SMET:597-598` has set `RHOS` for this
  element.
- `SM` is called from `SMIN:721` only under `NSMT == 1`, and `NSMT` becomes 1
  either at `SMET:624` (after `RHOS` was set at 597 for this same element) or at
  `SMIN:737` (in which case `SD == 0` and `SMIN:711` blocks the `SM` call).

So `RHOS` is correct at every reachable `SM` entry. But that is a four-step
argument across three routines, re-derived from scratch by every reader, and
one new call site breaks it silently — `SF(IEL) = pnsnow / RHOS` at line 313
would just use the previous element's snow density, which for `NSD = 1`
(spatially variable `RHOSAR`) is a plausible-looking wrong number rather than a
crash.

This is also the reason `SMmod` blocks any future parallelisation of the ET
element loop. `ETmod`'s per-element loop is the natural place to add OpenMP, and
every one of these scalars — plus `PNET` and `NSMT` from `AL_D` — is a shared
write. That is worth stating explicitly, because the surrounding modules are
being worked on for exactly that kind of improvement, and this module is
currently a hard barrier to it.

The fix is mechanical: make them locals of `SM`, and pass `RHOS` from `SMET` as
an argument (or compute it in `SM`, since it depends only on `NSD`,
`RHOSAR(IEL)` and `RHODEF`, all of which `SM` can already see). `RHOS` must stay
`PUBLIC` for `FRmod:5581` to read the input value into, but that role — the
configured default — is distinct from its role as per-element scratch, and the
two should not share a variable.

### M4 — dead module state shadowed by locals — *cleanup*

`SMmod.f90:63-66` declares module-level `HFC`, `HFR`, `HFE`, `HFT`, and
`SMmod.f90:301` declares locals with exactly the same four names inside `SM`:

```fortran
DOUBLE PRECISION :: hfc, hfr, hfe, hft
```

The locals shadow the module variables completely, so the module copies are
never written and never read. They are not in the `PUBLIC` list
(`SMmod.f90:87`), so no other module can observe them either. Four dead
doubles, and — more importantly — a shadowing pattern that makes the source
read as though `SM` were publishing its heat-flux terms as module state when it
is not. Delete lines 63-66.

`TOPNET` (`SMmod.f90:71`) is assigned once, at line 314, and never read
anywhere in the codebase. It is `PRIVATE`. Either delete it or put it to work
as the saved input depth for the round-trip fix in P5 — it already holds
exactly the right value.

### M5 — `IMET(NVEE)` is 1 MB of write-only state — *low impact, easy*

`SMmod.f90:74` declares `INTEGER :: IMET(NVEE)` with `NVEE = 250 000`, i.e.
1 MB of static storage. `INSM` reads it from record SM6b
(`FRmod.f90:5600`) and echoes it (`FRmod.f90:5605`). Nothing else in the
codebase reads it — I grepped the whole tree; the only hits are the
declaration, the `PUBLIC` list, the two `FRmod` lines, and documentation.

The module header (`SMmod.f90:26`) and `FRmod`'s (`FRmod.f90:5544`) both
describe it as the meteorological-station element locations used for the
energy-budget wind-speed correction, but `SM` uses `MS = NMC(IEL)`
(`SMmod.f90:309`) for every meteorological lookup, including `U(MS)` at line
343. So the documented purpose is real but unimplemented — the correction it
was meant to feed does not exist.

Two things follow. The storage should shrink to whatever `NM` actually needs
(it is read as `(IMET(N), N = 1, NM)`, so it is a per-met-station array, not a
per-element one, and `NVEE`-sizing it is the same over-allocation pattern noted
in the ETmod analysis's §M5). And the documentation should stop describing a
correction the code does not apply, or the correction should be implemented —
but that is a modelling decision, not a cleanup.

### M6 — `HEAD` is a `DOUBLEPRECISION` array read with an `A4` descriptor — *low, portability*

`SMmod.f90:76` declares `DOUBLEPRECISION :: HEAD(20)`, and `FRmod.f90:5606`
reads into it:

```fortran
READ(SMD, '(20A4)') HEAD
```

The `A` edit descriptor applied to a non-character list item is not standard
Fortran — F2018 requires a character-type item — and this is pure Hollerith
legacy, packing four characters into each 8-byte double. gfortran and ifx both
accept it as an extension, which is why it has survived, but it is exactly the
kind of construct that breaks on a compiler change, and the module has already
had one Hollerith descriptor removed in this modernisation pass
(`SMmod.f90:287`).

`HEAD` is only ever used to consume and echo a title line. `CHARACTER(LEN=80)`
expresses that directly, is standard, and drops the array. It is `PUBLIC` and
used by `FRmod`, so the change touches both files.

### M7 — dead locals — *cleanup*

In `SM` (`SMmod.f90:299-302`):

- `MR = NRAINC(IEL)` (line 308) — assigned, never read.
- `N = NVC(IEL)` (line 310) — assigned, never read.
- `EE = E` (line 419) — assigned, never read; see C6, this one may indicate lost
  functionality rather than simple dead code, so decide before deleting.
- `EFFDEP` — set to zero and used only in the dead subtraction at line 346; see
  C8.

In `SMET` (`SMmod.f90:587`):

- `MR = NRAINC(IEL)` (line 593) — assigned, never read.

`-Wunused-variable` is already enabled for `Debug` builds
(`CMakeLists.txt:696`), but it does not catch assigned-then-unused variables,
which is why these have survived. gfortran's `-Wunused-but-set-variable` would
catch all five; it is worth adding to the `Debug` configuration.

### M8 — `SF` is used as both a depth and a rate within one routine — *low, but a real trap*

`SF(IEL)` is a snow *depth* in mm from line 313 through line 462, and is then
converted in place to a *rate* in mm/hr at line 508:

```fortran
SF(IEL) = (SF(IEL) / DTUZ) * 3600.0d0
```

The declared meaning in `AL_D.f90:224` is "Current snowfall depth by element
(mm of snow)", which matches the first two thirds of the routine and not the
value the routine leaves behind. The array is also read elsewhere (`ETmod.f90:73`
imports it), so a consumer has to know which side of line 508 it is on.

Worse, `SF(IEL)` is only assigned when `SM` runs for that element. An element
that takes any of the paths where `SM` is skipped keeps the rate written on some
earlier step — a stale value with no marker distinguishing it from a fresh zero.
That is the same class of leak the ETmod analysis documents for `DRAIN` and
`PNET` in its §C1, and for the same structural reason.

Using a separate output variable for the rate, or converting at the point of
consumption rather than in place, removes the ambiguity. Correcting
`AL_D.f90:224`'s comment is the minimum.

### M9 — `USE SGLOBAL` without an `ONLY` clause — *low*

`SMmod.f90:49` imports all of `SGLOBAL` unqualified, with a commented-out
`ONLY` clause on the very next line showing the intent was there:

```fortran
USE SGLOBAL
!USE SGLOBAL, ONLY : NVEE
```

The module actually needs `NVEE`, `NELEE`-derived extents,
`max_no_snowmelt_slugs`, `total_no_elements`, `ZGRUND`, the numeric constants
`zero`/`one`/`two`/`three`/`five`, and the comparison helpers `ISZERO`,
`GTZERO`, `LTZERO`, `LEZERO`. That is a short, writable list.

`ZGRUND` in particular (used at line 372) is currently invisible at the top of
the file — a reader has to grep `sglobal.f90` to discover where it comes from.
The `Debug` build already sets `-Wuse-without-only` (`CMakeLists.txt:697`), so
this is a warning the project has chosen to enable and is not acting on.

---

## 4. Priority summary

| # | Finding | Severity | Effort | Changes results? |
|---|---|---|---|---|
| C1 | Slug compaction assumes ordered release; mass lost and double-counted at every pack exhaustion | high | low | **yes** |
| C3 | `ESAT` (mb) vs `PO` (Pa): specific humidity ~100× low, sublimation near-inert | high | trivial | **yes, large** |
| C2 | SVP polynomial is a +15 °C fit; negative below -7 °C, slope 4× too steep | high | medium | **yes** |
| C4 | `SMELT`/`TMELT` unallocated during the hotstart read | medium-high | low | no (fixes a crash) |
| C5 | 400-slug cap calls `STOP` mid-run | medium | medium | no |
| M1 | 6.4 kB/element for a single-digit high-water mark | medium | medium | no |
| M3 | Per-element scratch in module scalars; blocks ET-loop parallelism | medium | medium | no |
| M2 | Saved-flag guard, no `STAT=`, no deallocator | medium | low | no |
| C6 | `HFE` uses pre-clamp `E`, `ESM` post-clamp; `EE` dead; comment contradicts code | low-med | low | yes, small |
| P1 | `LOG` of run-constant scalars per element per step | low-med | trivial | no |
| C8 | `U(MS) == 0` divides by zero; unbounded unstable correction | low | low | edge cases only |
| C7 | `SD → 0` denominators (currently unreachable, ill-conditioned near zero) | low | trivial | no |
| C9 | Line 411 assumes `TS <= 0`; positive `TSIN` unvalidated for `MSM=2` | low | trivial | first step only |
| P2 | `PO` recomputed per element per step | low | trivial | no |
| P3 | `initialise_smmod` called 2N times per step | low | trivial | no |
| M5 | `IMET` — 1 MB written, never read; documented feature unimplemented | low | low | no |
| M4 | Shadowed dead module `HFC`/`HFR`/`HFE`/`HFT`; dead `TOPNET` | cleanup | trivial | no |
| M6 | `HEAD` is `DOUBLEPRECISION` read with `A4` (non-standard) | low | low | no |
| M7 | Dead locals `MR`, `N`, `EE`, `EFFDEP` | cleanup | trivial | no |
| M8 | `SF` is a depth then a rate; stale between steps | low | low | no |
| M9 | `USE SGLOBAL` without `ONLY` | low | low | no |
| P6 | Two misleading performance comments | cleanup | trivial | no |

**Suggested order.** C4 + P3 first: one edit (move the `initialise_smmod` call
into `FRINIT`), no result change, removes a latent crash. Then M2 and M4/M7/P6
as a no-risk cleanup pass. Then C1 on its own, with a regression baseline
captured first — it changes every snow run and should be attributable. Then C3
and C2 **together**, also with a baseline; fixing C3 alone amplifies C2's slope
error by two orders of magnitude, which would look like a regression. C5 and M1
are the same sizing decision and should be taken as one piece of work. M3 is
worth doing before any attempt at parallelising the ET element loop, and not
especially urgent otherwise.

---

## 5. Checked and found correct

Listed so the next reader does not re-derive them.

- **Unit consistency of the heat budget.** Every term in
  `HFT = HFC + HFR - HFE + (HFG + RN)*DTUZ` checks out as J/m²:
  `HFC = RHOA·CPA·DN·ΔT·DTUZ` is `kg/m³ · J/kg/C · m/s · C · s`; `HFR` uses
  `SF·RHOS` (mm snow → mm water) `· RHOW/1000` to reach kg/m² before the
  specific heat; `HFE = (LVW + LWI - CPI·TS)·E·DTUZ` is `J/kg · kg/m²/s · s`.
  The `Q`/`QA` factor inside `E` is the exception — see C3.
- **The `SD·RHOS` shorthand.** `TS2 = HFT/(CPI·RHOS·SD) + TS` and
  `USM = HFT/(LWI·RHOS)` both rely on `(SD/1000 m)·(RHOS·1000 kg/m³) = SD·RHOS
  kg/m²`, which the comment at `SMmod.f90:400-402` states and which is right.
  `USM` comes out in mm of snow, as documented.
- **The Richardson-number sign convention.** `TA > TS` is stable, gives
  `RICH > 0`, and the code divides `DN` down; `TA < TS` is unstable, gives
  `RICH < 0`, and the code multiplies up. Both directions are physically
  correct. The stable branch's denominator `1 + 10·RICH` cannot reach zero,
  since that branch is taken only when `RICH > 0`. Only the unbounded
  amplification and the `U == 0` case are problems (C8).
- **The slug-array bounds check.** `NSMC` is incremented at line 447, checked
  against `max_no_snowmelt_slugs` at line 451, and first used as a subscript at
  line 460. No out-of-bounds store is possible. The compaction's
  `KKK = KL + NCC` reaches at most `KK + NCC = NSMC_old`, also in bounds.
- **`USM` is clamped before it reaches `SMELT`.** The `TSM > SD` cap at lines
  430-440 runs before the `SMELT` assignment at lines 460-462, so the routed
  slug can never exceed the available pack. `USM` is also floored at zero
  (line 423) and `RHOS > 0` is ensured by `SMET:598`, so `SMELT >= 0` always,
  which is what the `GTZERO` test at line 466 assumes.
- **`RHOS` is correct at every reachable `SM` entry.** The four-step argument is
  in M3. It holds today; it is the fragility, not a present defect.
- **Array layout.** Slug-major `(max_no_snowmelt_slugs, total_no_elements)` is
  the right choice for column-major Fortran: the scan and compaction both walk
  a contiguous run. Do not "fix" this to element-major.
- **`TMELT`/`TIMEUZ` units.** Both hours; `f(SD)` yields hours; the comparison
  at line 480 is consistent. `DTUZ` is seconds, and the two places that bridge
  the two — `pnsnow = precip_m_per_s·1000·DTUZ` (line 637) and
  `SF/DTUZ·3600` (line 508) — both use the right factor.
- **The degree-day threshold is +2 °C, not 0 °C.** `USM = DDF·(TA - 2)·DTUZ`
  with `USM = 0` below 2 °C (lines 332-333). This is unusual but deliberate, and
  the header already flags it (`SMmod.f90:30-33`).
- **`TSIN` is forced to zero for `MSM = 1`** (`FRmod.f90:5586`), so the
  degree-day branch never reads a meaningful `TS`. `ISZERO(TS(IEL))` at line 421
  is therefore always true in that branch — but `E` is zero there too
  (line 335), so the guard is inert rather than wrong.
- **Control flow after the 2026-04 modernisation.** I re-derived the branch
  structure of all three routines against the documented `GOTO` removal. `SMIN`'s
  four-way dispatch, `SMET`'s three-way snow-depth split, and `SM`'s
  degree-day/energy-budget selection all reach the same states the header tables
  describe. The `CONTINUE` placeholder at line 605 is a no-op, not a fall-through
  bug.
