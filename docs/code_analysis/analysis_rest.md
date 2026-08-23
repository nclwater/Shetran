# Logical performance assessment: `rest`

## Scope and method

This is a **logical, source-only** assessment. Nothing was compiled, profiled
or timed. Every claim is derived from reading `src/modules/rest.f90` in full,
together with the declarations it depends on (`src/parameters/sglobal.f90`,
`src/parameters/AL_C.F90`, `src/parameters/AL_D.f90`,
`src/parameters/mod_parameters.f90`), the callees it uses
(`src/modules/utilsmod.f90`, `src/modules/OCmod2.f90`), the single caller
(`src/modules/run_sim.f90`) and the compiler flags in `CMakeLists.txt`.
Where a claim depends on compiler behaviour rather than on the standard,
that is stated explicitly.

The module contains five procedures:

| Procedure | Call site | Frequency |
|---|---|---|
| `EXTRA_OUTPUT` (`:110`) | end of run | once |
| `BALWAT` (`:238`) | `run_sim.f90:331` | **every timestep** |
| `READ_DATED_RECORD` (`:362`) | `METIN` | per met record |
| `RESIZE_MET_RECORD` (`:448`) | `READ_DATED_RECORD` | rare |
| `METIN` (`:557`) | `TMSTEP` only (`:1331`, `:1363`) | **1–n times per timestep** |
| `TMSTEP` (`:1056`) | `run_sim.f90:255` | **every timestep** |

`EXTRA_OUTPUT` runs once and is not performance-relevant; it is not discussed
further except where noted in §1.11.

## Conclusion up front

The module is **not** a hot spot in the arithmetic sense. Its per-timestep work
is `BALWAT` (one sweep of the two largest 2-D/3-D state arrays in the model) and
two small `O(total_no_elements)` scans in `TMSTEP`. Everything else is I/O
bound and amortised over the meteorological record interval.

The findings that matter are, in order:

1. **`METIN` depends on implicit `SAVE` of 2 MB local arrays** (§1.1). `PEIN`,
   `TAHIGH` and `TALOW` are read on one call and used on a later call. Nothing
   in the source makes them persist. It works today only because both supported
   compilers, with the flags currently in `CMakeLists.txt`, put large
   constant-bound locals in static storage. Automatic/recursive modes would
   break that accidental persistence silently, with no diagnostic and no crash,
   just wrong evaporation; Intel's `/heap-arrays` option is not such a mode
   because it governs automatic arrays and expression temporaries.
2. **`TA` is overwritten unconditionally from arrays that are only ever defined
   when `ISTA` is true** (§1.2). In `BMETAL` runs without max/min temperature
   files, air temperature is forced to zero on every timestep, which disables
   snowmelt in `SMmod`.
3. **`BALWAT` is a pure diagnostic** whose only consumer is legacy output type
   43 (`FRmod.f90:3524`) and one visualisation getter, yet it runs a full
   element × cell sweep every timestep (§3.1, §3.2). Gating it is the single
   largest available saving in this module.
4. **`BALWAT`'s inner loops fight the array layout** (§3.1): `ERUZ` is
   `(element, cell)` and is indexed cell-inner, and `QVSH` is traversed twice
   where one pass would do.
5. **14 MB of `NVEE`-sized locals in `METIN`** (§2.1) for data that is at most
   `NM` or `NRAIN` long.

Items 1 and 2 should be fixed before any optimisation work, because a
correctness fix to `METIN`'s storage class (§1.1) is also the fix for the
memory waste (§2.1) — they are the same change.

---

## Part 1 — Correctness

### 1.1 `METIN` relies on implicit `SAVE` for `PEIN`, `TAHIGH` and `TALOW`

`METIN` declares (`:566-567`):

```fortran
DOUBLE PRECISION :: PA(NVEE), PEIN(NVEE), PETOT(NVEE), PER(NRAIN)
DOUBLE PRECISION :: TAHIGHT(NVEE), TALOWT(NVEE), TAHIGH(NVEE), TALOW(NVEE)
```

None of these has `SAVE` and none has an initialiser. Yet the algorithm
requires three of them to survive between calls:

* `:737-738` — `PETOT(1:NM) = (TEND - UZNOW) * PEIN(1:NM)` runs **before** any
  read in the current call. The `PEIN` used here is whatever the previous
  `METIN(2)` call read. This is the normal path: `EPTIME` only falls below
  `UZNOW + UZNEXT` once per PE record, so on most timesteps `pet_read_loop`
  (`:743`) does not execute at all and `PEIN` is *never* assigned in the call
  that uses it.
* `:825` — `TA(1:NM) = (TAHIGH(1:NM) + TALOW(1:NM)) / 2` has the same
  structure: on timesteps where no record is read, both operands come from an
  earlier call.

Under the Fortran standard, a local variable without `SAVE` becomes undefined
when the procedure returns. The code is therefore non-conforming, and its
correctness rests on a compiler default:

* **ifx / ifort** leave non-allocatable local arrays in static storage under
  the default local-storage mode. `PEIN(NVEE)` is therefore static in the
  configured build and its bytes persist in practice.
* **gfortran** moves constant-bound local arrays above `-fmax-stack-var-size`
  into static storage; 2 MB is far above any default threshold, so it also
  persists today.

Both behaviours are switchable, and one switch is already sitting in the build
file:

| Change | Effect on `METIN` |
|---|---|
| ifx `/Qauto` or `-auto`; Intel OpenMP/recursive mode | arrays become automatic — `PEIN` is undefined on entry |
| gfortran `-frecursive`, `-fopenmp`, `-fmax-stack-var-size=0` | arrays become automatic — same failure |
| gfortran with F2018 "all procedures recursive by default" semantics | same failure |
| any future compiler with different defaults | same failure |

The failure mode is the worst kind: no crash, no diagnostic. Static storage is
zero-filled, so today the very first call also reads zeros rather than garbage,
which is why the current behaviour looks correct. Under heap or stack
allocation the run would silently produce arbitrary potential evaporation and
air temperature.

**Fix.** Move the persistent state to module scope and size it to the data, not
to `NVEE`:

```fortran
DOUBLE PRECISION, ALLOCATABLE, SAVE :: PEIN(:), TAHIGH(:), TALOW(:)
```

allocated once to `MAX(NM, NRAIN)` after the met configuration is known.
`PETOT` and `PA` are genuinely call-local and can stay local (but should be
sized to `NM`, see §2.1). `PER`, `TAHIGHT` and `TALOWT` are never referenced
and should be deleted (§1.9).

This single change fixes the standards violation, removes the reliance on
compiler defaults, and eliminates roughly 14 MB of static storage.

### 1.2 `TA` is overwritten from arrays that are only defined when `ISTA` is true

`:825` executes on **every** `METIN(2)` call in the `BMETAL` branch:

```fortran
OBSPE(1:NM) = PETOT(1:NM) / UZNEXT / 3600.0d0
TA(1:NM) = (TAHIGH(1:NM) + TALOW(1:NM)) / 2.0d0
```

But `TAHIGH` and `TALOW` are only ever assigned inside `IF (ISTA)` blocks
(`:667`, `:679`, `:714`, `:720`, `:761`, `:770`, `:800`, `:806`). `ISTA` is
initialised `.TRUE.` at `FRmod.f90:1846` and cleared at `FRmod.f90:1890` when
file 45 or 46 is absent from the run-data file — a supported configuration.

When `BMETAL` is true and `ISTA` is false, `TAHIGH` and `TALOW` are never
written. Combined with §1.1 they sit in zeroed static storage, so `:825`
assigns `TA(1:NM) = 0.0` on every timestep of the run.

`TA` is not a local diagnostic. It is consumed by the snowmelt module at
`SMmod.f90:324, 332-333, 346-349, 356-364, 375, 393, 459, 617, 729`, where the
predicates are `TA <= 0` (accumulate snow / ice-phase heat flux) and `TA > 0`
(melt). Forcing `TA = 0` selects the accumulate branch permanently: **snow
never melts** in a `BMETAL` run without temperature files. It also overwrites
the fixed 10 °C that `MNmod.f90:671` deliberately installs for the MN
contaminant path.

Whether the run is affected depends on `BEXSM`, so this is not universal, but
where it applies the result is a silently wrong water balance rather than an
error.

**Fix.** Guard the assignment:

```fortran
IF (ISTA) TA(1:NM) = (TAHIGH(1:NM) + TALOW(1:NM)) / 2.0d0
```

Note that the `ISTA` guards inside the read loops are already correct; only the
consumer at `:825` is unguarded. The docstring at `:494` already documents the
intended behaviour ("from max/min temperature average **when `ISTA` is
enabled**"), so the code and the documentation disagree, not the code and the
design.

### 1.3 Fatal read errors exit with status 0

The module uses two termination forms inconsistently:

| Form | Lines |
|---|---|
| `ERROR STOP` | `:406`, `:603`, `:620`, `:656`, `:1131`, `:1143`, `:1155`, `:1164`, `:1176`, `:1185`, `:1197`, `:1206`, `:1224`, `:1248`, `:1270`, `:1292` |
| `STOP` / `STOP '…'` | `:672`, `:674`, `:684`, `:686`, `:703`, `:716`, `:722`, `:749`, `:750`, `:764`, `:765`, `:773`, `:774`, `:790`, `:802`, `:808` |

`ERROR STOP` sets a processor-dependent **non-zero** exit status. Plain `STOP`,
with or without a character stop code, terminates with status **0** on both
gfortran and ifx. So sixteen fatal met-input error paths — every failure to
read the PE file or either temperature file outside the hot-start block —
report success to the shell.

For a model that is normally driven from batch scripts and calibration
harnesses, a fatal error that looks like a clean run is a real defect: failed
members are silently accepted into ensembles.

The 2026-05-10 history entry (`:24`, `:554`) says the interactive prompts were
"replaced with `ERROR STOP`". That conversion was applied to the precipitation
paths and to `TMSTEP`, but the PE and temperature paths were left on `STOP`.

**Fix.** Convert all sixteen to `ERROR STOP`, and while doing so give the four
bare `STOP`s at `:672`, `:684`, `:703` and the `STOP 'Error reading …'` forms a
message consistent with the rest of the module (currently `:749-750`,
`:764-765`, `:773-774` produce terse messages such as `'Error reading PET file'`
that omit the station count, unlike their hot-start equivalents at `:651-654`).

### 1.4 Non-dated `MED` reads ignore positive `IOSTAT`

In the `BMETAL = .FALSE.` branch, every read tests only for end of file:

```fortran
READ (MED, 9030, IOSTAT=ios) ISITE, NN, PINP(I), RN(I), U(I), PA(I), TA(I), DEL(I), VPD(I), IDATA
IF (ios < 0) THEN            ! :854 — and likewise :877, :905, :927, :944
```

A malformed record (`ios > 0`) falls straight through. The affected elements of
`PINP`, `RN`, `U`, `TA`, `DEL`, `VPD` and `OBSPE` keep whatever the previous
record left in them (or, for the first record, zero), `METIME` has already been
advanced at `:848-849`, and the run continues with silently stale forcing.

This is the opposite policy to the `BMETAL` branch, which treats a malformed
record as fatal (§1.3). Format `9030` (`:980`) is a two-line `G12.6` read, so a
column-misaligned or short file is a realistic input error.

**Fix.** Add `IF (ios > 0)` handling to all five sites, matching the `BMETAL`
branch: report the record and `ERROR STOP`.

### 1.5 Dated records are parsed, validated and then discarded

`READ_DATED_RECORD` computes `DATEHOUR = HOUR_FROM_DATE(...)` at `:413` for
every record of every dated file. The callers pass `prddate`, `epddate`,
`tahdate`, `taldate` (`:592`, `:647`, `:668`, `:680`, `:746`, `:762`, `:771`),
which are initialised at `:576-579` and then **never read**. The docstring is
explicit about this (`:540-544`: "the parsed dates are used for start-file
checks and initial positioning in `TMSTEP`. Within this routine the active
record windows are still advanced by `DTMET2` and `DTMET3`").

Two consequences:

* **Correctness.** After the initial positioning in `TMSTEP` §2b (`:1214-1302`),
  the record timing is assumed to be exactly `DTMET2`/`DTMET3` for ever. A
  dated file with a gap, a duplicate timestamp, a daylight-saving discontinuity
  or an interval that differs from `DTMET2` will be consumed in file order with
  no check, and the forcing will drift out of phase with the model clock by an
  amount that grows monotonically. The dates give the *appearance* of a
  self-describing file format without any of the safety it implies.
* **Cost.** See §3.5 — this is not a cheap discard.

**Fix (cheap).** Compare each parsed date against the window the model expects
and fail (or resynchronise) on mismatch:

```fortran
IF (ABS(prddate - MELAST) > TOLERANCE) CALL ERROR(FFFATAL, …)
```

**Fix (cheaper still, if validation is not wanted).** Make the timestamp parse
optional via an argument so `HOUR_FROM_DATE` is not called at all on the
steady-state path.

### 1.6 A trailing blank line in a dated file is a fatal date error, not end of data

`READ_DATED_RECORD` reads with format `9000` (`:432`), a formatted read. Under
the default `PAD='YES'`, a short or blank record is blank-padded rather than
producing an error: `I4` and `I2` edit descriptors over blanks yield zero. So a
trailing blank line — extremely common in hand-edited or tool-generated time
series — produces `IOS = 0` and `YEAR = MONTH = DAY = 0`.

`HOUR_FROM_DATE` (`utilsmod.f90:379`) then indexes `DAYS_TO_START_MONTH(0, 0)`
and runs its round-trip check at `utilsmod.f90:409-418`, which fails and calls
`ERROR STOP` with "There is a problem with a date that has been entered".

The equivalent non-dated path handles the same situation gracefully: the
list-directed read at `:615` / `:699` returns `ios < 0`, the "Finish of prd
data" notice is written to `.pri`, and remaining values are zeroed
(`:606-612`, `:623-629`).

So switching a file to the dated format converts a benign end-of-data into a
fatal, misleadingly-worded error.

**Fix.** After the record read, treat an all-blank record as end of file:

```fortran
IF (LEN_TRIM(MET_RECORD) == 0 .AND. YEAR == 0) THEN
   IOS = -1
   IOSTAGE = IOSTAGE_RECORD
   RETURN
END IF
```

### 1.7 Non-`BMETAL` runs skip the time-varying vegetation update

The early return at `:840`:

```fortran
ELSE
   ! READ ALL MET. DATA IN FIXED TIME INTERVAL (USUALLY HOURLY) FORMAT
   IF (IFLAG == 2) RETURN
```

sits *before* the time-varying parameter block at `:962-972`, which calls
`TERPO1` for `CSTCAP`, `PLAI`, `CLAI` and `VHT`.

`TMSTEP` calls `METIN(2)` on every timestep (`:1362-1363`) but calls `METIN(1)`
only when a record boundary falls inside the candidate step (`:1329`).
Therefore:

| Mode | Vegetation parameters updated |
|---|---|
| `BMETAL = .TRUE.` | every timestep (via `METIN(2)`), plus again on every record boundary (via `METIN(1)`) |
| `BMETAL = .FALSE.` | only on `MED` record boundaries — i.e. at most once per `DTMET` |

In the fixed-interval mode the canopy storage capacity, leaf area indices and
vegetation height therefore lag the model clock by up to `DTMET`, and are
stepped rather than interpolated. This looks like long-standing legacy
behaviour rather than a recent regression, but it is an undocumented asymmetry
between two modes that are meant to differ only in input format.

**Fix.** Replace the `RETURN` at `:840` with a branch that skips the `MED` read
but falls through to `:962`, so both modes update vegetation on every call. The
redundant second update per boundary in `BMETAL` mode should be removed at the
same time (§3.6).

### 1.8 The `BMETP` diagnostic mixes two index spaces and prints stale data

```fortran
IF (BMETP) THEN                      ! :830
   WRITE(PPPRI, 9130) METIME
   DO I = 1, NM
      WRITE(PPPRI, 9140) I, PINP(I), PEIN(I)
   END DO
END IF
```

* `PINP` is indexed by **rainfall station** (`1:NRAIN`), `PEIN` by
  **meteorological site** (`1:NM`). The loop uses `NM` for both. When
  `NM /= NRAIN` — the case the module explicitly supports at `:842`/`:893` —
  the printed pairing is meaningless, and when `NM > NRAIN` the `PINP` values
  printed are whatever lies beyond the active stations (in bounds, since `pinp`
  is `nvee+10` at `:50`, but not data).
* The block runs for **both** `IFLAG` values. On an `IFLAG == 1` call `PEIN`
  has not been updated; on an `IFLAG == 2` call `PINP` has not been. One of the
  two columns is always one record stale.

Diagnostic only, but it is the diagnostic a user would reach for when
investigating exactly the mis-synchronisation described in §1.5.

### 1.9 Dead locals and dead stores

| Item | Line | Status |
|---|---|---|
| `PER(NRAIN)` | `:566` | declared, never referenced. An *automatic* array (runtime bound), so it costs a real stack allocation on every call. |
| `TAHIGHT(NVEE)`, `TALOWT(NVEE)` | `:567` | declared, never referenced. 4 MB of static storage. |
| `PA(NVEE)` | `:566` | written at `:852`, `:864`, `:903`, `:914`; never read. 2 MB to hold discarded atmospheric pressure. |
| `EPLAST` | `:692`, `:727` | assigned in `hotstart_epd_loop`, never read there (only the `pet_read_loop` copies at `:780`/`:813` are used). Dead store. |
| `IDATA`, `NN`, `ISITE` | `:564` | read from `MED`, never used except `ISITE` in the `BMETP` echo. Documented at `:485`, `:498`. |
| `DEPTHS` | `:269` | pure copy of `asum`; `DELSTO`/`STORW_balwat` could use `asum` directly. |
| `car` | `:143` | copy of `carea` used for exactly one of eleven identical divisions (`:145`); the other ten use `carea`. Vestigial. |

The `NVEE`-sized entries here are the bulk of §2.1.

### 1.10 `TMSTEP` division guards — currently safe, but only by construction

Two divisions by `PINP(I)` have no explicit guard:

* `:1319` — `UZNEXT = MIN(UZNEXT, PMAX / PINP(I))`, reached only when
  `PTOT(I) > PMAX`. Since `PTOT(I) = (TEND - UZNOW) * PINP(I)` with
  `TEND >= UZNOW`, and `PMAX > 0`, this implies `PINP(I) > 0`. Safe.
* `:1336` — `UZTEST = MELAST - UZNOW + (PMAX - PTOT(I)) / PINP(I)`, reached
  only when `PTOT(I) + (METIME - MELAST) * PINP(I) > PMAX`. At the top of every
  `meteorological_loop` iteration `PTOT(I) <= PMAX` holds (if it did not, the
  previous iteration would have set `EXITT` and exited), so the predicate
  requires `PINP(I) > 0`. Safe.

Both are safe **only** because `PMAX > 0` and because the loop invariant
`PTOT <= PMAX` is maintained. Neither is asserted anywhere. Worth an explicit
`PMAX > 0` check at input time rather than a comment.

Related: `:1337` computes `UZNEXT = MIN(UZNEXT, UZTEST)` and `UZTEST >=
MELAST - UZNOW` always holds, so the subsequent `TEND - MELAST` at `:1344`
cannot go negative. Also correct by construction, also unasserted.

One genuine hazard remains: `precip_read_loop` (`:590`) and the `MED` loops
advance `METIME` by `dtmet2` / `DTMET` per iteration and terminate only when
`METIME >= BHOTTI` or `METIME >= UZNOW + UZNEXT`. A zero or negative
`dtmet2`/`dtmet3`/`DTMET` gives an infinite loop that reads to end of file and
then spins forever writing nothing. Validation belongs at input.

### 1.11 Minor

* `READ_DATED_RECORD`'s docstring (`:347-348`) states that `VALUES` is "left
  unchanged when the read fails". For the `IOSTAGE_VALUES` path this is not
  true: a list-directed internal read that fails partway may have already
  assigned the leading elements. Harmless (all callers treat it as fatal) but
  the contract as written is wrong.
* `IOS_SHORT_RECORD = 1` (`:60`) is a synthesised positive `IOSTAT` value in the
  same numeric space as compiler-generated error codes. Callers only test
  `> 0`, so no collision occurs today; a caller that ever switches on specific
  codes would be misled.
* `MET_RECORD` is never deallocated at end of run. Cosmetic.
* `EXTRA_OUTPUT` prints `FLERRC`/`SYERRC`/`CMERRC` counts (`:114-122`) that, as
  its own `@note` at `:99-103` records, no routine assigns. Three loops and a
  header block that always produce an empty section.
* `:1358` uses the single-precision literal `3.6E6`. The value is exactly
  representable in binary32 so no accuracy is lost, but it is inconsistent with
  the `D0`-suffixed literals used everywhere else in the module and would
  become a silent precision bug if the constant were ever edited.

---

## Part 2 — Memory and variable management

### 2.1 `METIN` declares 14 MB of locals for at most `NM` values

`NVEE = 250000` (`sglobal.f90:121`). The declarations at `:566-567` are:

| Array | Bytes | Used extent | Referenced? |
|---|---|---|---|
| `PA` | 2 000 000 | `1:NM` | written, never read |
| `PEIN` | 2 000 000 | `1:NM` | yes |
| `PETOT` | 2 000 000 | `1:NM` | yes |
| `TAHIGHT` | 2 000 000 | — | never |
| `TALOWT` | 2 000 000 | — | never |
| `TAHIGH` | 2 000 000 | `1:NM` | yes |
| `TALOW` | 2 000 000 | `1:NM` | yes |
| **total** | **14 000 000** | | |

`NM` is the number of meteorological sites — realistically single or double
digits. `NVEE` is a worst-case capacity constant.

Because these are constant-bound arrays, current compilers place them in static
storage (§1.1), so this is a 14 MB BSS/virtual-address reservation rather than
14 MB of stack traffic per call. The resident cost is smaller — pages beyond
the touched prefixes are generally never faulted in. The cost is address-space
footprint and, far more importantly, the standards violation in §1.1 that this
storage class is silently papering over.

Note the interaction: if this is "fixed" naively by enabling automatic or
recursive local storage, §1.1 turns into a live wrong-answer bug. Storage-class
and persistence fixes must therefore be addressed together; `-heap-arrays`
alone does not change these fixed-bound arrays.

`TA(NVEE)` (`AL_D.f90:228`), `MEASPE(NVEE)` (`ETmod.f90:96`) and
`pinp(nvee+10)` (`:50`) have the same shape and the same reasoning, but they are
module-scope by design and outside this module's remit.

### 2.2 `pinp` is over-dimensioned and its `+10` is unexplained

```fortran
DOUBLEPRECISION :: pinp(nvee+10)=zero      ! :50
```

`pinp` is only ever accessed over `1:NRAIN` (`:592`, `:611`, `:615`, `:628`,
`:632`, `:1311`, `:1319`, `:1334`, `:1344`, `:1352`) and, incorrectly, over
`1:NM` in the `BMETP` echo (§1.8). The `+10` slack has no comment and no
reader; it looks like defensive padding against exactly the `NM`-vs-`NRAIN`
confusion in §1.8. Either the padding is load-bearing — in which case the
overrun it protects against is a bug to fix — or it is not, and should go.

Two megabytes of the four this array occupies would disappear if it were made
allocatable and sized to `NRAIN` once the met configuration is read.

### 2.3 `MET_RECORD` is shared across four files, so it converges to the widest

The buffer strategy in `READ_DATED_RECORD` (`:381-388`, `:423-427`) is sound in
outline: start at `LENGTH_LINEVERYLONG` (262 144, `mod_parameters.f90:71`),
shrink to fit after the first successful data line, grow monotonically
thereafter, never thrash. The growth path with `BACKSPACE` and re-read
(`:400-411`) correctly handles a record wider than `NVALUES * LENGTH_TEXT_R8P`.

The weakness is that one buffer serves `PRD`, `EPD`, `TAH` and `TAL`, which have
independent widths (`NRAIN` vs `NM`). The buffer converges to
`MAX` over all four, and every read of the *narrowest* file then pays for the
widest:

* the formatted read at `:391` blank-pads the entire buffer;
* `LEN_TRIM` at `:398` scans it.

Both are `O(LEN(MET_RECORD))`, not `O(record)`. For a catchment with `NM = 500`
PE sites and `NRAIN = 2` rain gauges, every precipitation record costs a 13 kB
memset plus a 13 kB backward scan to move 60 characters of data. Over an hourly
30-year run that is a few gigabytes of pointless memory traffic — small next to
the solver, but it is pure overhead and trivially removable.

**Fix.** Four buffers instead of one, or a small derived type holding
`(buffer, sized)` per unit. Each then sizes itself to its own file and stays
there.

A second, smaller point: `MET_RECORD_SIZED` is only set after a *successful*
value parse (`:423-427`). If the first dated read hits end of file or a parse
error, the buffer stays at 262 144 characters for the remainder of the run,
and every subsequent record pays a 256 kB pad-and-scan.

### 2.4 Blanket `USE SGLOBAL`

`:28` is an unqualified `USE SGLOBAL`, with the `ONLY` list commented out
immediately below at `:29`. Every other `USE` in the module is qualified
(`:30-44`). The unqualified import is what supplies `ZERO`, `ONE`, `UZNOW`,
`PPPRI`, `ZGRUND`, `cellarea`, `total_no_elements`, `total_no_links`,
`top_cell_no`, `ISERROR`, `ISERROR2`, `FFFATAL` and `ERROR` — none of which is
locally visible as such.

The Debug build sets `-Wuse-without-only` (`CMakeLists.txt:697`), so this
already emits a warning. Restoring the `ONLY` list would document the module's
actual dependency surface, which matters here because `rest` reaches into
almost every global state module in the model.

---

## Part 3 — Performance

### 3.1 `BALWAT`: three sweeps where one would do, and one transposed array

`BALWAT` (`:238-320`) runs once per timestep over all elements. Per element it
executes three separate loops over the same cell range `NLYRBT(IEL,1) :
top_cell_no`:

```fortran
DO CELL = NLYRBT(IEL,1), top_cell_no                 ! :265 — storage
   asum = asum + DELTAZ(CELL,IEL) * VSTHE(CELL,IEL)
END DO
...
DO CELL = NLYRBT(IEL,1), top_cell_no                 ! :285 — root extraction
   asum = asum - ERUZ(IEL,CELL)
END DO
...
DO JDUM = 1, 2                                       ! :296 — lateral flux
   DO CELL = NLYRBT(IEL,1), top_cell_no              ! :298
      asumQ = asumQ + QVSH(JDUM,CELL,IEL) + QVSH(JDUM+2,CELL,IEL)
   END DO
END DO
```

Three problems, in decreasing severity:

**`ERUZ` is indexed against its layout.** `ERUZ` is
`(total_no_elements, top_cell_no)` (`AL_C.F90:159`, `:190`). The loop at `:285`
varies `CELL`, i.e. the *second* subscript, so consecutive iterations are
`total_no_elements * 8` bytes apart. At 2000 elements that is a 16 kB stride:
one cache line and probably one TLB entry consumed per cell, with zero spatial
locality, no vectorisation, and the same cache lines re-fetched by the next
element's iteration only after the whole array has been swept.

Note the 1997 history entry at `:235`: "Swapped array subscripts for `QVSH`,
`DELTAZ`, and `VSTHE`". `ERUZ` was not included in that pass and has been the
odd one out ever since. `DELTAZ` and `VSTHE` at `:266` are `(cell, element)`
and stride correctly — the fix that was applied to them is exactly what `ERUZ`
still needs. `ERUZ` is allocatable, but transposing it is a coordinated
cross-module change: `ETmod` produces it and `VSmod`, `CMmod`, and this routine
consume it.

**`QVSH` is traversed twice.** `QVSH` is `(4, top_cell_no, total_no_elements)`
(`AL_C.F90:160`, `:188`). All four face values for a given cell are contiguous —
32 bytes, half a cache line. The `JDUM` loop makes two passes over the same
memory to pick up `(1,3)` then `(2,4)`. One pass summing all four is
arithmetically identical (the sum is unordered), halves the traffic, and lets
the compiler contract the four loads into a single vector reduction:

```fortran
DO CELL = NLYRBT(IEL,1), top_cell_no
   asumQ = asumQ + QVSH(1,CELL,IEL) + QVSH(2,CELL,IEL) &
                 + QVSH(3,CELL,IEL) + QVSH(4,CELL,IEL)
END DO
```

with the `QOC` terms — only four values, unrelated to `CELL` — hoisted out of
the cell loop entirely:

```fortran
asumQ = asumQ - QOC(IEL,1) - QOC(IEL,2) + QOC(IEL,3) + QOC(IEL,4)
```

**The three loops can be fused.** Once `ERUZ` strides correctly, all three
loops walk the same cell range and can be one loop, so `DELTAZ`, `VSTHE`,
`ERUZ` and `QVSH` are read once per cell. This does not reduce the bytes read
(different arrays) but it removes two loop set-ups per element and improves
the prefetcher's job.

Two further micro-points:

* `IF (.NOT. FIRST_balwat)` at `:281` is loop-invariant across a
  `total_no_elements`-long loop. Splitting the first call into its own short
  loop removes the branch and makes the steady-state loop body straight-line.
* `GETHRF(IEL)` at `:261` is an external `PURE` function
  (`OCmod2.f90:94-102`) called once per element. CMake requires IPO support and
  enables it for optimized builds (`CMakeLists.txt:72-84`), so the one-line
  accessor is eligible for inlining; the actual compiler decision should be
  confirmed in generated code if this loop profiles hot.

### 3.2 `BALWAT` is a diagnostic that always runs

`WBERR`, the only output of `BALWAT`, has exactly two consumers in the whole
tree:

* `FRmod.f90:3524` — `BUFFER(J) = WBERR(IEL)`, legacy result-set data type 43;
* `visualisation_interface_left.f90:171` — a getter that narrows it to default
  real.

If neither output type 43 nor that visualisation item is selected for a run,
every cycle spent in `BALWAT` is discarded. Given §3.1 — a full sweep of
`DELTAZ`, `VSTHE`, `ERUZ` and `QVSH`, the four largest per-cell arrays in the
water model, plus `GETHRF` per element — this is likely the single largest
avoidable cost in the module, and possibly a measurable fraction of the
non-solver time per timestep.

**Fix.** Gate the `run_sim.f90:331` call on a flag set at start-up from the
selected output sets:

```fortran
IF (want_wberr) CALL BALWAT
```

**Caveat.** `WBERR` and `STORW_balwat` are cumulative, so this is all-or-nothing
per run: the flag must be decided before the first timestep and must not
change. If output type 43 can be enabled partway through a run, the gate must
be `.TRUE.` whenever it is enabled *anywhere* in the output plan, not merely
active now.

### 3.3 `TMSTEP` scans every element for snow on every timestep

```fortran
IF (BEXSM) THEN                                      ! :1087
   SMFLAG = .FALSE.
   DO I = 1, NM
      IF (TA(I) > 0.0d0) SMFLAG = .TRUE.             ! :1090
   END DO
   IF (SMFLAG) THEN
      snowmelt_check: DO IEL = total_no_links + 1, total_no_elements
         IF (SD(IEL) > 0.0d0) THEN                   ! :1095
            TSNOW = 0.5d0
            EXIT snowmelt_check
         END IF
      END DO snowmelt_check
   END IF
END IF
```

The `EXIT` makes this cheap when snow *is* present — it stops at the first
snow-covered element. The expensive case is the common one: a snow-enabled run
in a warm period, where `SMFLAG` is true and no element has snow, so the loop
runs to completion. That is a full `O(total_no_elements)` scan of `SD` on every
timestep for the whole snow-free part of the year, to produce `.FALSE.`

Two independent improvements:

* The `TA` loop at `:1089-1091` has no early exit. `IF (ANY(TA(1:NM) > 0.0d0))`
  expresses the same thing and short-circuits. Minor — `NM` is small.
* The `SD` scan should be replaced by a flag maintained where `SD` is written.
  `SMmod` already knows when a pack appears or disappears; a module-level
  `snow_present` counter or logical, updated at the point of change, reduces
  this to an `O(1)` test. Failing that, `ANY(SD(total_no_links+1 :
  total_no_elements) > 0.0d0)` is at least vectorisable, which the current
  scalar loop with its branch and `EXIT` is not.

Note also that this whole block computes `TSNOW` which is then used once, at
`:1104`, in a `MIN` — and that when `BEXSM` is false, `TSNOW = TMAX` makes the
`MIN` a no-op. The structure is fine; only the scan needs attention.

### 3.4 `precip_m_per_s` does two divisions per element per timestep

```fortran
DO IEL = 1, total_no_elements                        ! :1357
   precip_m_per_s(IEL) = PTOT(NRAINC(IEL)) / UZNEXT / 3.6E6
END DO
```

`UZNEXT` and `3.6E6` are loop-invariant, but IEEE semantics forbid the compiler
from turning `x/UZNEXT/3.6E6` into a multiply by a precomputed reciprocal —
and `-fp-model=precise` / `-fno-fast-math` (`CMakeLists.txt:682, 702`) are set
explicitly, so it definitely will not. Every element therefore pays two true
double-precision divisions, each ~15–40 cycles and poorly pipelined, every
timestep.

`NRAIN` is typically a handful; `total_no_elements` is thousands. Scale the
`NRAIN` values once and gather:

```fortran
PSCALE(1:NRAIN) = PTOT(1:NRAIN) / (UZNEXT * 3.6D6)
DO IEL = 1, total_no_elements
   precip_m_per_s(IEL) = PSCALE(NRAINC(IEL))
END DO
```

This replaces `2 * total_no_elements` divisions with `NRAIN` divisions and one
multiply, and leaves a pure indexed gather that vectorises. It is also
arithmetically *not* bit-identical to the original (one rounding instead of
two), which is worth stating in the change note even though the difference is
at the last ulp.

Note `3.6E6` → `3.6D6` while touching this line (§1.11).

### 3.5 `HOUR_FROM_DATE` is expensive and its result is thrown away

`READ_DATED_RECORD:413` calls `HOUR_FROM_DATE` for every record of every dated
file. That function (`utilsmod.f90:379-420`) is not a cheap arithmetic
conversion:

* `DAYS_IN_YEARS_SINCE_1950` (`utilsmod.f90:428-453`) loops over every leap-year
  candidate from 1952 to `y-1` in steps of 4 — about 19 iterations for a 2026
  date;
* it then calls `DATE_FROM_HOUR` (`utilsmod.f90:517`) purely to validate the
  round trip, and that function calls `DAYS_IN_YEARS_SINCE_1950` again inside a
  `DO WHILE` loop (`utilsmod.f90:535-537`), plus twice more at `:539` and
  `:548`.

So one timestamp parse costs on the order of a hundred loop iterations plus
several integer divisions — to produce a value that §1.5 shows nobody reads.
With `PRD`, `EPD`, `TAH` and `TAL` all dated and hourly, that is four such
parses per simulated hour for the life of the run.

The same cost also dominates the start-up positioning loops in `TMSTEP` §2b
(`:1214-1302`), which call `HOUR_FROM_DATE` once per skipped record. A run
starting twenty years into a long hourly series pays it ~175 000 times before
the first timestep. That is a one-off, but it is a visible start-up delay and it
is easy to remove: the skip loop only needs a *monotone* comparison, so it can
compare `(year, month, day, hour, minute)` tuples directly against the
decomposed `tih` and call `HOUR_FROM_DATE` exactly once, on the record it
finally accepts.

**Fixes, in order of value:**

1. Make the timestamp parse optional in `READ_DATED_RECORD` (skip it entirely
   on the steady-state path), *or* — better — keep it and actually validate it
   (§1.5), which converts a wasted cost into a real check.
2. Replace the `HOUR_FROM_DATE`-per-record calls in the four `TMSTEP` skip
   loops with integer tuple comparison.
3. Independently, `DAYS_IN_YEARS_SINCE_1950` should be closed-form
   (`(y-1950)*365 + leaps(y-1) - leaps(1949)` using `y/4 - y/100 + y/400`)
   rather than a loop. That is a `utilsmod` change and benefits every caller.

### 3.6 The vegetation-parameter block runs twice per timestep in `BMETAL` mode

`:962-972` runs on every `METIN` call that reaches it. In `BMETAL` mode both
`METIN(1)` (`:1331`) and `METIN(2)` (`:1363`) reach it, so on any timestep that
crosses a precipitation record boundary the four `TERPO1` interpolations run
twice for all `NV` vegetation types, producing identical results — `TCURR =
TIMEUZ` does not change between the two calls.

`TERPO1` (`utilsmod.f90:694-740`) is small and `PURE`, and `NV` is the number of
vegetation types rather than elements, so the absolute cost is low. It is listed
here for completeness and because the fix falls out of §1.7: move the block out
of `METIN` and call it once per timestep from `TMSTEP`, after the `METIN(2)`
call. That makes both modes consistent (§1.7) *and* removes the duplicate.

### 3.7 Structural duplication in `METIN` blocks all of the above

`METIN` contains four near-identical copies of the "read PE record, handle
error, handle EOF, optionally read `TAH`, optionally read `TAL`, divide by
`dtmet3`, advance `EPTIME`" sequence:

| Copy | Lines | Variant |
|---|---|---|
| hot-start, dated | `:646-695` | `READ_DATED_RECORD` |
| hot-start, undated | `:698-730` | list-directed |
| steady, dated | `:745-785` | `READ_DATED_RECORD` |
| steady, undated | `:788-818` | list-directed |

They differ only in whether they accumulate into `PETOT` and in the wording of
their error messages — and the messages have already drifted apart (§1.3), which
is the usual first symptom.

Worse, the `IF (BMETDATES)` test at `:646` and `:745` is *inside* the loop, so a
loop-invariant condition is re-evaluated on every record, and the loop exit
condition is duplicated in both arms (`:695` and `:730`; `:785` and `:818`).

This is not itself a performance problem — it is what makes every fix in §1 and
§3.5 a four-place edit with four chances to miss one. An internal subroutine
taking `(unit, nvalues, dated, values, label)` collapses all four to one, hoists
the `BMETDATES` test out of the loop, and gives the error messages a single
definition.

The precipitation path (`:589-637`) has the same shape in two copies rather than
four.

---

## Part 4 — Ranked recommendations

| # | Change | Kind | Effort | Expected effect |
|---|---|---|---|---|
| 1 | Move `PEIN`/`TAHIGH`/`TALOW` to module-scope allocatables sized to `NM` (§1.1, §2.1) | correctness | small | removes reliance on compiler storage-class defaults; frees ~14 MB |
| 2 | Guard `TA(1:NM) = …` with `IF (ISTA)` (§1.2) | correctness | trivial | restores snowmelt in `BMETAL` runs without temperature files |
| 3 | Convert the sixteen `STOP`s to `ERROR STOP` (§1.3) | correctness | trivial | fatal met errors stop reporting success to the shell |
| 4 | Gate `BALWAT` on whether `WBERR` is output (§3.2) | performance | small | removes a full element × cell sweep per timestep when unused |
| 5 | Transpose `ERUZ` to `(cell, element)`; fuse `BALWAT`'s three cell loops; single-pass `QVSH`; hoist `QOC` (§3.1) | performance | medium | removes the only large-stride access in the module and halves `QVSH` traffic |
| 6 | Handle `ios > 0` on the five `MED` reads (§1.4) | correctness | small | malformed fixed-format met data stops being silently ignored |
| 7 | Hoist the division out of the `precip_m_per_s` loop (§3.4) | performance | trivial | `2 × total_no_elements` divisions → `NRAIN` per timestep |
| 8 | Validate dated timestamps against the expected window, or stop computing them (§1.5, §3.5) | correctness + performance | small | catches gapped/mis-intervalled dated files; removes ~100 loop iterations per record |
| 9 | Replace the `SD` scan with a maintained snow-present flag (§3.3) | performance | medium | `O(total_no_elements)` → `O(1)` per timestep in snow-free periods |
| 10 | Collapse the four duplicated PE read blocks into one internal routine (§3.7) | maintainability | medium | prerequisite for keeping 3, 6 and 8 correct |
| 11 | Per-file `MET_RECORD` buffers (§2.3) | performance | small | narrow dated files stop paying the widest file's pad-and-scan |
| 12 | Move the `TERPO1` block to `TMSTEP` (§1.7, §3.6) | correctness + performance | small | makes both met modes update vegetation identically; removes the duplicate pass |
| 13 | Delete `PER`, `TAHIGHT`, `TALOWT`, `PA`, `DEPTHS`, `car`; restore `USE SGLOBAL, ONLY` (§1.9, §2.4) | hygiene | trivial | −6 MB, and `-Wuse-without-only` goes quiet |
| 14 | Treat a blank dated record as end of data (§1.6) | robustness | trivial | trailing newline stops being a fatal error |

Items 1–3 are independent of each other and of everything else, and should go
first. Item 10 should precede 3, 6 and 8 if all four are being done in one pass.
Item 5 is the only one that changes a shared array layout. It must update
`ETmod` (producer), `VSmod` and `CMmod` (other consumers), as well as this
routine, and should be validated under the `Debug` build's bounds checking.
