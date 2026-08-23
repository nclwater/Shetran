# Logical performance assessment: `FRmod`

## Scope and method

This is a **logical, source-only** assessment. No profile was taken and no
timings were measured. Every claim below is derived from reading
`src/modules/FRmod.f90` in full, together with the call site in
`src/modules/run_sim.f90`, the dimension parameters in
`src/parameters/sglobal.f90`, `src/parameters/AL_C.F90`, `src/parameters/AL_D.f90`
and `src/parameters/AL_G.F90`, and the compiler flags in `CMakeLists.txt`.
Where a claim depends on compiler behaviour rather than on the source alone,
that is stated explicitly.

The assessment was requested for `FRmod.f90` only. Callers and callees outside
this module were read where needed to establish call frequency or an interface,
but are not themselves assessed.

`FRmod` is orchestration code, so the first thing to establish is which of its
twenty-odd routines actually carry per-timestep cost. From `run_sim:254-392`
the main loop calls, on **every** timestep:

```text
FRSORT                                   (:294)
FRMB      → FRRESP  when a day boundary is crossed   (:334 → :1731)
FRRESP    when BSTORE                                (:346)
FROUTPUT('main ')                                    (:384)
```

Everything else — `FROPEN`, `INFR`, `FRIND`, `FRDIM`, `FRINIT`, `INET`, `INSM`,
`INBK`, `INPL`, `FRRESC`, `FRLTL`, `MUERR2` — runs once, and `INCM` runs once on
the first active contaminant step. Four routines therefore carry the runtime
cost, and `FRSORT` is the only one that does real work proportional to the mesh
on every step regardless of output settings.

## Conclusion up front

`FRmod` has **no arithmetic hot spot**. Its runtime cost is entirely (a) memory
traffic in `FRSORT`, (b) two whole-domain sweeps in `FRRESP` that run before the
routine knows whether any output is due, and (c) a set of locals dimensioned at
compile-time capacity rather than at the active mesh size.

The dominant structural problem is that every element-indexed 2-D array the
module touches is **element-major** — `ICMREF(NELEE,12)`, `DHF(NELEE,4)`,
`QOC(NELEE,4)`, `ELEV(NELEE,2)` — while every loop reads a *row*. With
`NELEE = 250000` (`sglobal.f90:118`) the column stride of `ICMREF` is 1 MB.
`FRSORT` walks elements in `ISORT` order, i.e. a **random permutation**, and
reads five `ICMREF` columns per element: five randomly-placed 1 MB-strided
loads per element per timestep, with no spatial locality and no reuse.

`FRSORT` also declares 6 MB of fixed-capacity local storage
(`ELEV(NELEE,2)` + `ISTEMP(NELEE,2)`, `:3580-3581`) in a routine entered once
per timestep, and `FRRESP` another 2 MB (`BUFFER(NELEE)`, `:3327`).

Before any of that, five correctness defects should be fixed. Two of them
(`FRIND`'s stale `ITYPE`, `INBK`'s unused `found_adjacent`) silently produce
wrong topology or read element 0; one (`FRIND`'s one-past-the-edge code-grid
probes) blocks bounds-checked validation of everything else, since `Debug` uses
`-fcheck=bounds` / `/check:bounds` (`CMakeLists.txt:651,694`).

The cheapest substantial win is `FRRESP`: two whole-domain sweeps per timestep
feeding a single output id that most runs never select.

---

## Part 1 — Correctness defects

These are reported first because two of them corrupt topology, two read outside
array bounds, and the bounds-checked build cannot be used to validate any later
change while they remain.

### 1.1 `FRIND` uses a stale `ITYPE` when setting `NBFACE`

`ITYPE` is assigned at `:717`, inside the *neighbour-assignment* loop that ends
at `:1113`. The consistency-check loop that follows (`:1129-1175`) never
assigns it, but reads it at `:1172`:

```fortran
ICMREF (INDEX, I + 8) = I
IF (ITYPE < 3 .AND. NBFACE (INDEX) == 0) NBFACE (INDEX) = I
```

`ITYPE` there holds the type of the **last** element processed by the previous
loop, not the type of `INDEX`. Two consequences, and which one you get depends
only on how the mesh happens to be numbered:

| Last element's type | Effect |
|---|---|
| 0 (grid) — the normal case, since grid elements are created last (`:698-707`) | `ITYPE < 3` is true for every `INDEX`, so `NBFACE` is set for **channel links too**, which the comment at `:1120-1123` says it should not be. |
| 3 (a link, i.e. a link-only mesh) | `ITYPE < 3` is false for every `INDEX`, so `NBFACE` stays zero everywhere. |

`NBFACE` is passed straight into `SYMAIN` (`run_sim:302`) and written to the
result header (`FRRESC:3156`). The fix is to reload
`ITYPE = ICMREF(INDEX, 1)` at the top of `element_check`.

### 1.2 `INBK` computes `found_adjacent` and never tests it

`INBK`'s `INTYPE == 1` branch searches for an adjacent grid element (`:3875-3883`)
and, failing that, an adjacent bank (`:3886-3894`), setting `found_adjacent`
in both. The flag is then used only to decide whether to run the second search
(`:3885`). It is **never tested before the value is used**:

```fortran
! * set value
DZG = ZGRUND (IEL) - ZGRUND (JEL)          ! :3898
SELECT CASE (IDATA)
 CASE (2); NMC (IEL) = NMC (JEL)           ! :3905
 ...
```

When neither search succeeds, `JEL` holds whatever `ICMREF(IEL, 8)` left in it —
`0` for a boundary face, or a **negative** multi-link node pointer (`:583`,
`:1143`). `ZGRUND(0)`, `NMC(0)`, `NRAINC(0)`, `NVC(0)`, `STRXX(0)`, `SD(0)`,
`RHOSAR(0)`, `ZVSPSL(0)` and `GETHRF(0)` are all then read, and the results are
written into a real bank element.

`found_adjacent` was clearly added to guard this block; the guard was never
applied. `IF (found_adjacent)` around `:3897-3922` closes it.

### 1.3 `FRIND` probes the code grids one index outside their bounds

`INGRID`, `LCODEX` and `LCODEY` are all declared `(NXEE, NYEE)` with lower bound
1 (`AL_D.f90:177,180,183`). `FRIND` indexes them at `I-1`, `J-1`, `I+1` and
`J+1` without any guard:

| Site | Expression | Out of range when |
|---|---|---|
| `:765` | `INGRID (I - 1, J)` | `I = 1` |
| `:778` | `INGRID (I, J - 1)` | `J = 1` |
| `:730`, `:794`, `:988` | `LCODEX (I + 1, J)` | `I = NXEE` |
| `:743`, `:870`, `:1021` | `LCODEY (I, J + 1)` | `J = NYEE` |
| `:890`, `:939`, `:957` | `LCODEX (I, J - 1)` | `J = 1` |
| `:842`, `:891`, `:940`, `:966` | `LCODEY (I - 1, J)` | `I = 1` |

In column-major storage `INGRID(0, J)` aliases `INGRID(NXEE, J-1)` — the last
column of the previous row — so a catchment that touches the west edge of the
grid silently connects to whatever is at the east edge one row down. For
`J = 1` the access is genuinely before the start of the array.

This works today only because catchments are conventionally padded with an
inactive border. Nothing in `INFR` or `FRIND` enforces that. Either declare the
three grids `(0:NXEE+1, 0:NYEE+1)` and zero the halo, or guard each probe.

### 1.4 `FRRESP` is called with `RESNOW = 0` and resets the cumulative-erosion time base

`FRRESP` maintains a running time integral of the erosion rate at `:3345-3347`:

```fortran
DO J = 1, total_no_elements
   GNUCUM(J) = GNUCUM(J) + GNU(J) * (RESNOW - PREVTM) * 3600.0D0 * 1000.0D0
END DO
```

and sets `PREVTM = RESNOW` on exit (`:3541`). `run_sim:321` calls it once, on
the first active contaminant step, with `RESNOW = ZERO`:

```fortran
IF (BSTORE) CALL FRRESP (AIOSTO, ZERO, .FALSE.)
```

At that point `PREVTM` is the current simulation time. The increment is
therefore `-GNU * PREVTM * 3.6e6` — the whole run so far, subtracted — and
`PREVTM` is then reset to 0, so the next call at `UZNOW` adds
`+GNU * UZNOW * 3.6e6`.

The two errors partially cancel, and cancel exactly if `GNU` is identical at the
two calls. In general the residual is `(GNU_after - GNU_before) * PREVTM`, i.e.
proportional to elapsed simulation time — unbounded. The correct argument is
the current time, as at `run_sim:346`.

### 1.5 `PREVTM` and `GNUCUM` have no defined initial value

Recorded in the module header (`:42-47`), but stated here because 1.4 makes it
reachable. `PREVTM` (`:148`) and `GNUCUM` (`:152`) are module variables with no
declaration initialiser and no assignment before `:3346` reads them. Both are
static storage, so gfortran and ifx will in practice zero them at load, but
nothing in the language guarantees it and nothing in the module documents a
contract. `PREVTM = zero` and `GNUCUM = zero` at declaration cost nothing.

### 1.6 `FROUTPUT`'s local `SAVE` copies defeat the AD export contract

`run_sim.f90:100-101` imports `qoctot, uzold, next_hour, icounter2` from `FRmod`
"for AD only". `FROUTPUT` declares its own `SAVE` copies of three of the four
(`:2229-2234`), shadowing the module variables:

| Module variable | Declared | Working copy | Public value during a run |
|---|---|---|---|
| `qoctot` | `:140`, `0.0d0` | `:2231` | stays `0.0d0` |
| `uzold` | `:141`, `0.0d0` | `:2229` | stays `0.0d0` |
| `next_hour` | `:137`, `INTEGER = 1` | `:2230`, **`DOUBLE PRECISION`** | stays `1` |
| `sedtot`/`sedfinetot`/`contamtot` | `:143-145` | `:2232-2234` | stay `0.0d0` |
| `icounter2` | `:138` | — (not shadowed) | correct |

The private module `hour_now` (`:139`), `uznowt` (`:142`) and `qoctotextra`
(`:146`) are likewise shadowed by `:2221`, `:2224` and the local allocatable,
and are dead. Note also the type change on `next_hour`.

`FROUTPUT` additionally declares `filnam` as `CHARACTER(256)` at `:2217`,
shadowing `SGLOBAL::filnam` (`sglobal.f90:149`, `LENGTH_FILEPATH`), which
`FROPEN` uses as its rundata record buffer; and local `ZERO`/`ONE` parameters
(`:2212-2213`) shadowing `SGLOBAL::zero`.

Either remove the module-level duplicates and export the working values, or
delete the export. The current arrangement is the only one that costs storage
and produces constants.

### 1.7 `FRRESP` CASE(30) writes `DUMO` where `DUM0` was meant

`:3474-3487`:

```fortran
DO SED = SFSED1, SFSED2
   IF (QOC (IEL, K) > ZERO) THEN
      DUM0 = DUM0 + QSED (IEL, SED, K) / QOC (IEL, K)
   ELSE
      DUMO = ZERO                                 ! :3482 — letter O, not digit 0
   END IF
END DO
```

`DUMO` and `DUM0` are both declared at `:3328`. The zero-discharge branch is
therefore a no-op and `DUMO` is never read anywhere in the routine. As it
happens "contribute nothing" is the defensible behaviour and `DUM0 = ZERO`
would have been worse (it would discard earlier fractions), so the current
output is probably the intended one — but the branch is dead, the intent is
unrecorded, and `DUMO` should be deleted along with it.

### 1.8 `FRRESP` CASE(50) subscripts `BALANC` with the buffer index

`:3530`:

```fortran
CASE (50)
   BUFFER(J) = BALANC(J) * 1000.0D0 / CAREA
```

`J` is the output-buffer index running `1..NOUT`, where
`NOUT = ICLNUM(ICLASS)` (`:3367`) — the number of elements in the selected
output class. `BALANC` is `DOUBLE PRECISION :: BALANC(20)` (`AL_D.f90:238`).
Any class with more than twenty members reads past `BALANC`. The mapping
"buffer slot *j* holds balance term *j*" also only makes sense for the
`FRMB:1731` call, which selects data id 50 alone.

### 1.9 Two different guards for the same outlet, neither complete

`FROUTPUT` guards on `mblink == 0 .AND. mbface == 0` (`:2608`, `:2635`);
`FRMB` guards on `MBLINK /= 0` (`:1684`). `find_mass_balance_outlet` (`:2469`)
sets

```fortran
mblink = L
mbface = NOCBCD (NOCBCC (L), 2)
```

with no validation of the second value. If `NOCBCD(...,2)` is 0 — or outside
1:4 — then `mblink /= 0` and `FROUTPUT`'s `.AND.` guard passes, and
`qoc(mblink, 0)` is indexed at `:2641` and `:2611`. `QOC` is `(NELEE,4)`
(`AL_C.F90:153`), so index 0 aliases `QOC(NELEE, face-1)`.

Two further points in the same routine:

- `:2472-2473` clears `MBLINK`/`MBFACE` **unconditionally**, although the header
  at `:2182-2185` says the scan applies only "when result-file output has not
  provided `MBLINK`/`MBFACE`". Any value established by `INFR` is discarded.
- The `DO iface = 1, 4` loop at `:2476` uses `iface` only in the
  `ICMREF(L, 4 + iface) == 0` test; the assignments do not depend on it. A link
  with two external faces performs the same assignment twice.

### 1.10 `FRSORT` merges the two lists on inconsistent keys

Column 1 is built and sorted on `GETHRF` (`:3621`); column 2 on `ZVSPSL`
(`:3625`). The merge at `:3738` compares

```fortran
IF (NS2 == 0 .OR. ZVSPSL (ISTEMP (I1, 1)) > ELEV (I2, 2)) THEN
```

— that is, the *phreatic* level of the next ponded element against the stored
phreatic key of the next dry element. The surface-water key that column 1 was
sorted by is never used in the merge.

The header at `:3562-3566` records this as the implemented behaviour, so it is
presumably deliberate. Two consequences are worth stating anyway:

- `ISORT` is not monotone in either key. `ISORT` is not a local concern: `VSSIM`
  iterates columns in `ISORT` order (`VSmod.f90:4410`) and `CMmod` sweeps in
  `ISORT` order (`CMmod.f90:723`), so this ordering feeds the nonlinear solve.
- The whole `NSTART`/`NEND` pre-scan (`:3643-3693`) exists to exploit the
  previous timestep's ordering being nearly sorted. Merging on a key the list
  was not sorted by weakens exactly that precondition, so the Shell sort at
  `:3696-3725` does more work than the design intends. This is the performance
  cost of the inconsistency, and it recurs every timestep.

Two smaller items in the same routine:

- `.OR.` is **not** short-circuit in Fortran. `ELEV(I2, 2)` at `:3738` may be
  evaluated when `NS2 == 0`. It is in range (`I2 = 1`) so this is benign, but it
  is an uninitialised read.
- The Shell-sort inner loop backs up with `K = K - JUMP; IF (K > 0) CYCLE`
  (`:3718-3719`) rather than `K >= NSTART`, so it can compare and swap elements
  below `NSTART`. Safe only because the pre-scan guarantees everything before
  `NSTART` is `>= ZHIGH`; the loop bound should say so.

### 1.11 `FRDIM` reads `DXIN(0)`/`DYIN(0)` on a degenerate grid

`:272` and `:278`:

```fortran
DX (NX) = DXIN (NXM1)
DY (NY) = DYIN (NYM1)
```

With `NX == 1` (or `NY == 1`) `NXM1` is 0. A one-column model is a legitimate
1-D configuration — `FROUTPUT:2607` explicitly comments "A 1-D run may have no
configured outlet face" — so this is reachable.

---

## Part 2 — Array capacity, layout and locality

### 2.1 Six routines size local storage at capacity, not at the active mesh

`NLFEE = 20000` and `NELEE = 250000` (`sglobal.f90:117-118`).

| Site | Declaration | Bytes | Called |
|---|---|---|---|
| `FRSORT:3580` | `ELEV (NELEE, 2)` | 4.0 MB | **every timestep** |
| `FRSORT:3581` | `ISTEMP (NELEE, 2)` | 2.0 MB | **every timestep** |
| `FRRESP:3327` | `BUFFER (NELEE)` | 2.0 MB | **every timestep** |
| `FRINIT:1280` | `rdd (NELEE)`, `rddq (NELEE, 4)` | 10.0 MB | once, only used when `BHOTRD` |
| `INBK:3828-3829` | `IVALUE`, `IELEM`, `VALUE` at `NLFEE*2` | 0.6 MB | once |
| module `:152-153` | `GNUCUM (NELEE)`, `DLSSRT (NELEE)` | 4.0 MB | resident for the whole run |

With the current supported compiler defaults, this storage is static:

- gfortran moves constant-bound locals above `-fmax-stack-var-size` into static
  memory. Nothing in `CMakeLists.txt` sets that flag, so the default applies and
  `FRSORT`'s 6 MB becomes permanently resident `.bss`.
- ifx/ifort place non-allocatable local arrays in static storage by default;
  the current build does not enable `/Qauto`, `-auto`, recursion, or OpenMP.
  Options that make locals automatic would instead turn `FRSORT` into a 6 MB
  stack frame, but that is a latent configuration risk, not the current build.

The fix is the same either way: size from `total_no_elements`, or make them
module allocatables allocated once in `FRINIT`. `GNUCUM`/`DLSSRT` should also be
allocated only when some output set actually selects data id 44 (see 3.1).

### 2.2 `ELEV(NELEE,2)` splits the two sort lists 2 MB apart

The two columns of `ELEV` are `NELEE` doubles apart. The merge loop
(`:3736-3764`) alternates between `ISTEMP(I1,1)`, `ELEV(I2,2)` and a random
gather `ZVSPSL(ISTEMP(I1,1))` — three streams, two of them 2 MB apart, plus a
scattered load.

Two arrays of length `total_no_elements` each (or one array of length
`2*total_no_elements`) reduce the working set from 6 MB to
`24 * total_no_elements` bytes and put both lists in the same page neighbourhood.
For a 2000-element catchment that is 48 kB instead of 6 MB.

### 2.3 The module reads element-major arrays row-wise, everywhere

| Array | Declaration | Column stride |
|---|---|---|
| `ICMREF` | `(NELEE, 12)` `AL_G.F90:46` | 1.0 MB |
| `DHF` | `(NELEE, 4)` `AL_C.F90:105` | 2.0 MB |
| `QOC` | `(NELEE, 4)` `AL_C.F90:153` | 2.0 MB |
| `ICLIST` | `(NELEE, 14)` `AL_D.f90:188` | 1.0 MB |

Every element loop in the module reads a *row*, i.e. one element from each of
several columns:

| Site | Columns touched per element |
|---|---|
| `FRDIM:285-288`, `:321-324`, `:400-403` | `ICMREF(:,1..4)`, then `(:,5..8)` |
| `FRIND:717-720`, `:1131` | `ICMREF(:,1..4)`, `(:,5..8)` |
| **`FRSORT:3599`, `:3605-3618`** | `ICMREF(:,1)`, `(:,4)`, `(:,5..8)` |
| `INBK:3869`, `:3876`, `:3887` | `ICMREF(:,1)`, `(:,5..8)` |
| `INCM:4369-4370`, `:4415`, `:4523` | `ICMREF(:,5..8)`, `(:,9..12)` |
| `FRDIM:509-513` | `DHF(:,1..4)` |

`FRSORT` is the per-timestep one and it is the worst case, because its element
loop is driven by `ISORT` (`:3598`) — a permutation that changes every timestep.
So each iteration issues up to five randomly-addressed loads, each into a
different 1 MB-strided column, plus `ZVSPSL`, `ZGRUND` and `GETHRF`. That is
seven or eight cache lines, and very likely seven or eight TLB entries, consumed
per element per timestep, with no reuse between iterations.

A transposed `ICMREF(12, NELEE)` puts a whole element row in one or two cache
lines and turns `FRSORT`'s inner body into a single gather. **This cannot be
done in `FRmod` alone**: `ICMREF` is passed as `ICMREF(1,5)` to `SYMAIN`
(`run_sim:302`) and `CMRD` (`INCM:4248`), and declared `(NELEE,12)` in `OCFIX`
(`OCmod2.f90:1714`). It is nevertheless the single largest structural change
available. The same `ICMREF(12,NELEE)` change is identified from the OC side in
`analysis_ocmod2.md` M2 and `analysis_ocqdqmod.md` P1; `analysis_mnmod.md` §2.1
applies the same layout principle to MN's separate `(element,cell)` arrays.

A cheaper partial measure, local to `FRmod`: `FRSORT` needs only
`ICMREF(:,1)` and `ICMREF(:,4..8)`. Building a compact per-element record once
in `FRIND` — or simply hoisting `ICMREF(IEL,1)` and `LINKNS(IEL)` into a packed
`LOGICAL`/`INTEGER(1)` side array — removes the type test and the link-face
lookups from the per-timestep path.

### 2.4 `FRIND` zeroes 3.25 M integers, strided, and 1 MB of it is dead

`:639-643`:

```fortran
DO I = 1, NELEE
   NGRID (I) = 0
   NBFACE (I) = 0
   ICMREF (I, 1:12) = 0
END DO
```

Three problems in five lines:

- The loop runs to the **capacity** `NELEE = 250000`, not to
  `total_no_elements`. At that point `total_no_elements` is not yet known, so
  this is defensible — but `ICMREF = 0; NGRID = 0; NBFACE = 0` as three
  whole-array assignments does the same job contiguously and lets the compiler
  emit `memset`.
- `ICMREF(I, 1:12)` inside an `I` loop is **twelve stores 1 MB apart** per
  iteration: 3 M strided integer stores, touching 12 MB of pages in an
  interleaved pattern.
- `NGRID` is never read again. `AL_D.f90:176` documents it as "zeroed by
  `FRIND` and not subsequently read", and a grep over `src/` confirms it: the
  only occurrences are the `USE` list (`:80`) and this loop. That is 1 MB of
  permanent static memory and 250 000 stores for nothing.

Startup cost only, but it is 12 MB of first-touch page faults before the first
timestep.

---

## Part 3 — Redundant per-timestep work

### 3.1 `FRRESP` performs two whole-domain sweeps before it knows whether anything is due

`:3338-3347`, ahead of `output_loop`:

```fortran
IF (.NOT. SEDSRT) THEN
   DO J = 1, total_no_elements
      IF (NOTZERO(DLS(J))) SEDSRT = .TRUE.
      DLSSRT(J) = DLS(J)
   END DO
END IF

DO J = 1, total_no_elements
   GNUCUM(J) = GNUCUM(J) + GNU(J) * (RESNOW - PREVTM) * 3600.0D0 * 1000.0D0
END DO
```

`GNUCUM` has exactly one reader in the whole module: `CASE (44)` at `:3526`.
`DLSSRT` likewise. So:

- If no output set has `IODATA(ISET) == 44` — the common case; data id 44 is
  cumulative soil loss, only meaningful with the sediment component — the second
  loop is a whole-domain read-modify-write plus a multiply, on **every
  timestep**, producing a value nothing reads.
- Even when a set does select id 44, the loop covers `total_no_elements` while
  the set writes at most `ICLNUM(ICLASS)` values.
- `FRRESP` is entered twice on reporting steps: from `run_sim:346` and from
  `FRMB:1731`. `FRMB` passes an `AIOSTO` that is blank except for position 50
  (`:1728-1729`), so on that call id 44 cannot possibly be selected, and the
  sweep is unconditionally wasted.

The fix is a flag computed once — `ANY(IODATA(1:NSET) == 44)` — gating both
loops. This is the cheapest substantial change in the file and it cannot alter
any other output.

The first loop has a second issue: it captures the `DLSSRT` baseline on the
first call at which *any* element has nonzero `DLS`, and captures it for all
elements at that moment. Elements whose `DLS` becomes nonzero later never get a
baseline. That is a modelling question, not a performance one, but the two are
entangled in the same loop.

### 3.2 `FRMB` loads a dead array element per element per timestep

`:1672`:

```fortran
IPSTN = NRAINC (IEL)
```

`IPSTN` is never read. The header records this at `:1516-1518` ("still set for
the legacy rainfall-station pathway but is not used"), but it is still executed:
one load from a 1 MB `INTEGER(NELEE)` array per element per timestep, on the
one loop in `FRMB` that runs unconditionally.

### 3.3 `FRMB`'s `BALANC` update is written as an obfuscated two-pass loop

`:1694-1702`:

```fortran
DO I = 0, 6, 6
   BALANC (I + 1) = BALANC (I + 1) + PRECM
   ...
   BALANC (18 + I / 6) = BALANC (18 + I / 6) + BFLOW
END DO
```

Fourteen read-modify-writes on a module array, with a division in a subscript.
Written out as fourteen explicit statements the compiler can hold `BALANC(1:6)`,
`(7:12)`, `(18)` and `(19)` in registers; as written it may not be able to prove
the subscript arithmetic. Trivial in absolute terms — this runs once per
timestep, not once per element — but it is free to fix and it is the clearest
part of the routine to get wrong.

### 3.4 `FRMB` accumulates into module array elements inside the cell loop

`:1711-1721`:

```fortran
DO IEL = total_no_links + 1, total_no_elements
   ...
   BALANC (13) = BALANC (13) + CSTORE (IEL) * AREAEM
   BALANC (14) = BALANC (14) + SD (IEL) * RHOSAR (IEL) * AREAEM
   BALANC (16) = BALANC (16) + (GETHRF (IEL) - ZGRUND (IEL)) * AREAE

   DO ICL = NLYRBT (IEL, 1), top_cell_no
      BALANC (15) = BALANC (15) + VSTHE (ICL, IEL) * DELTAZ (ICL, IEL) * AREAE
   END DO
END DO
```

`BALANC(15)` is a read-modify-write of a module array element in the **innermost**
loop; the compiler must reload it each iteration unless it can prove `BALANC`
does not alias `VSTHE` or `DELTAZ`. Four scalar accumulators stored once after
the loops removes the dependency entirely. This runs only on reporting steps
(daily, per `run_sim:333`), so it is a minor item — but it is the only
`nel × ncell` sweep in `FRMB`.

Note the inner loop reads `VSTHE(ICL, IEL)` and `DELTAZ(ICL, IEL)` — cell-major,
unit-stride. That part is already right.

### 3.5 `restart_accumulators` does the same work twice

`FROUTPUT:2770-2775`:

```fortran
IF (ISextradis) THEN
   qoctotextra(1:disextrapoints) = qocavextra(1:disextrapoints) * dt
   do i = 1, disextrapoints
      qoctotextra(i) = qocavextra(i) * dt
   end do
END IF
```

The array assignment and the loop are identical. Delete the loop.

### 3.6 `write_main_output`'s trailing block is dead

`FROUTPUT:2607-2612`:

```fortran
IF (mblink == 0 .AND. mbface == 0) THEN
   qocav = ZERO
ELSE
   qocold = qoc(mblink, mbface)
END IF
```

`qocav` is overwritten by `sample_current_values` at the top of the next call
(`:2587`) and is not read again in this one. `qocold` is declared at `:2222` and
assigned here; grep confirms it has **no reader anywhere in the module**.

Removing the block also removes the last unguarded `qoc(mblink, mbface)`
reference discussed in 1.9.

### 3.7 `write_regular_outputs` reallocates a `SAVE`d buffer on every call

`:2804-2809`:

```fortran
CHARACTER(len=32), DIMENSION(:), allocatable :: buf
SAVE buf
IF (ALLOCATED(buf)) DEALLOCATE(buf)
ALLOCATE   (buf(disextrapoints))
buf = ''
```

`buf` is `SAVE`d specifically so it can persist, then freed and reallocated to
the same size on every call — and allocated as a zero-size array when
`ISextradis` is false, in which case it is never used. Allocate once, in
`allocate_extra_discharge` (`:2382`), alongside the other station arrays.

This runs once per `TOUTPUT` interval, so the cost is small; it is listed
because it is heap traffic in a routine whose only other work is formatting a
line.

### 3.8 `FRDIM` makes three passes over the same elements to re-derive the same indices

`:284-312` (`dim_loop`), `:320-375` (`overlap_loop`) and `:399-515`
(`node_space_loop`) each open with the identical four loads:

```fortran
ITYPE = ICMREF (IEL, 1)
IX    = ICMREF (IEL, 2)
IY    = ICMREF (IEL, 3)
IL    = ICMREF (IEL, 4)
```

Three passes over `total_no_elements`, twelve strided `ICMREF` loads per element
(see 2.3). The second and third loops cannot be merged — `overlap_loop` writes
`DXQQ`/`DYQQ` of *neighbouring* elements (`:356-364`), which `node_space_loop`
then reads — but the first two can, and `IL` is loaded in `node_space_loop`
(`:403`) and never used there.

`FRDIM` runs once, so this is a readability item, not a performance one.

---

## Part 4 — Layout mismatches in one-off setup

### 4.1 `INCM` builds two element-major scratch arrays from cell-major sources

`:4239`:

```fortran
ALLOCATE(KSPDUM(total_no_elements, top_cell_no + 1), &
         DUMMYCONC(total_no_elements, top_cell_no))
```

`KSPDUM` is filled at `:4356-4361` from `DELTAZ(NCE, NCL)`, which is cell-major
(`AL_C.F90`, `(top_cell_no, total_no_elements)`). The nest is element-outer,
cell-inner, so it reads unit-stride and writes with stride
`total_no_elements` — a full-domain transposing scatter. `DUMMYCONC` is then
read back the same way at `:4702-4710` and copied into
`CCCC(NCL, NCE, NCONT)`, which is also element-major.

`INCM` runs once, so the cost is a startup cost. It is listed because it is the
same layout mismatch that `analysis_mnmod.md` §2.1-2.2 identifies as the
dominant per-timestep problem in `MNmod`, and because `CCCC`/`SSSS` being
element-major is what forces `MNmod`'s hand there. Any future transposition of
the contaminant state has to change these two nests as well.

`NCATTY` is allocated `(NELEE, NCONEE)` at `:4240` — 3 MB at capacity — although
only `total_no_links+1 : total_no_elements` is ever written. All five
allocatables are local, so they are released on return; there is no leak.

### 4.2 `INCM`'s `ROH` workspace is conditionally initialised and unconditionally read

`ROH(LLEE)` (`:4215`) is filled only inside `IF (ITYPE /= 0)` (`:4632-4656`),
and read only inside the matching `ELSE` of a second test at `:4680-4689`. The
two tests agree, so this is correct — but the two branches are 50 lines apart
with an unrelated 15-line loop between them, and `ROH` is not initialised at
`:4646` for indices below `NLYRBT(NCL,1) - 1`. Worth a comment at minimum.

---

## Part 5 — Dead code and vestigial interfaces

Not performance-relevant, but they obscure the analysis above.

**`write_dis` has no caller.** `:3013-3023`. Only `write_dis2` is used
(`FROUTPUT:2602`). `write_dis` writes a bare `F20.8` to unit `dis` — the
regular-interval CSV that `write_regular_outputs` formats — so calling it would
corrupt that file's structure. It is listed as an active output routine in the
module header table (`:23`).

**`NGRID`** — see 2.4. Zeroed, never read.

**Dead locals:** `FRSORT:3618` `IL = ICMREF(IEL, 4)` unused; `FRSORT:3769-3771`
`FORMAT 1000`/`1010` unreferenced; `FRRESP:3328` `DUMO` write-only (1.7);
`FROUTPUT:2222` `qocold` write-only (3.6); `FRMB:1637` `IPSTN` write-only (3.2);
`FRDIM:403` `IL` unused in `node_space_loop`.

**Stale documentation:** `FRRESP`'s header (`:3284-3291`) describes `SFSED1` and
`SFSED2` as statement functions; they are plain `INTEGER` scalars assigned once
per output set (`:3324`, `:3372-3373`). `find_mass_balance_outlet`'s
header (`:2182-2185`) describes a conditional scan; the code is unconditional
(1.9). `FRMB`'s header (`:1500-1507`) asserts `2 <= NLYRBT(e,1)`, which nothing
checks and which `VSmod:1430` only guarantees for links.

**Redundant expression:** `FRRESP:3383` re-reads `IODATA(ISET)` in the
`SELECT CASE` selector although `IDATA` was set from it at `:3358` and has
already been validated at `:3359-3360`.

**Type mixing:** `FROUTPUT:2894` increments the `INTEGER` module variable
`icounter2` (`:138`) by the double literal `24.0D0`, and `:2879` compares it
against the double `uznow`. It works; it is not what the declaration says.

**Six copies of the same block:** `FROPEN`'s EOF handling
(`:1869-1876`, `:1880-1887`, `:1926-1932`, `:1936-1942`, `:1955-1960`,
`:1964-1969`, `:1983-1987`, `:1991-1995`) repeats
"disable the remaining optional groups, `CLOSE(2)`, `RETURN`" eight times with
different subsets of flags. One helper taking the first-disabled group index
would remove all eight.

**Unenforced capacities.** `INFR` prints every hard-coded capacity to the print
file (`:5158-5180`) and then checks none of them: `NX`/`NY` are read at `:5189`
without testing `NXEE`/`NYEE`; `FRIND` writes `ICMREF(INDEX, ...)` up to
`total_no_elements` without testing `NELEE` (`:701`, `:710`); `INBK` sets
`NVALUE = 2 * total_no_links` (`:3969`) and reads into `IELEM(NLFEE*2)` without
testing `NLFEE`. `MNERR0` in `MNmod` does exactly these checks for its own
capacities; `FRmod`, which is where the counts are established, does none.

**Format-descriptor mismatch:** `write_regular_outputs:2824` uses
`'(A,A1,F0.3,*(A1,A))'` with no items for the unlimited group. Legal, but the
two branches at `:2822` and `:2824` differ only in the trailing implied-do and
should share one descriptor.

---

## Recommended order of work

| Priority | Change | Expected benefit | Numerical risk |
|---|---|---|---|
| P0 | Reload `ITYPE` in `FRIND`'s consistency loop (1.1) | Correctness: `NBFACE` is currently wrong for either links or everything | **Changes `NBFACE`, and therefore `SYMAIN` input** — must be validated, not assumed bitwise |
| P0 | Guard `INBK`'s type-1 branch with `found_adjacent` (1.2) | Correctness; stops reads of element 0 / negative indices | None on meshes where every bank has a neighbour; changes results where one does not |
| P0 | Give the code grids a zeroed halo, or guard the six probe sites (1.3) | Correctness; **unblocks bounds-checked validation of everything below** | None on padded catchments |
| P0 | Pass the current time, not `ZERO`, at `run_sim:321` (1.4); initialise `PREVTM`/`GNUCUM` (1.5) | Correctness of output id 44 | Changes id 44 output only |
| P1 | Gate `FRRESP`'s two pre-loop sweeps on "id 44 selected" (3.1) | **High** — removes two whole-domain sweeps per timestep from every run that does not use id 44, and from every `FRMB`-triggered call regardless | **None — the values are currently unread in those cases** |
| P1 | Size `ELEV`/`ISTEMP`/`BUFFER`/`rdd`/`rddq` from `total_no_elements` (2.1), and split `ELEV` into two arrays (2.2) | **High** — 6 MB → tens of kB working set in the one routine that runs unconditionally every timestep; removes the latent large-frame exposure under automatic compiler modes | None — pure storage change |
| P1 | Delete the dead blocks: `restart_accumulators` duplicate (3.5), `write_main_output` tail (3.6), `IPSTN` (3.2), `qocold`, `DUMO` (1.7) | Small but free; 3.6 also closes the `qoc(mblink,0)` path | None |
| P2 | Validate `mbface` in `find_mass_balance_outlet`; unify the two outlet guards; drop the redundant `iface` loop (1.9) | Correctness on meshes with an odd boundary record | None on well-formed input |
| P2 | Bound `NOUT` against 20 in `FRRESP` CASE(50) (1.8); whole-array zeroing in `FRIND` (2.4); allocate `buf` once (3.7) | Correctness + 12 MB of strided startup stores | None |
| P2 | Resolve the `FROUTPUT` shadowing: export the working values or delete the module duplicates (1.6) | Makes the AD export contract true; removes six dead module variables | None to the simulation; **changes what AD consumers see** |
| P3 | Add capacity checks to `INFR`/`FRIND`/`INBK` (Part 5) | Turns three silent overruns into diagnostics | None |
| P3 | Transpose `ICMREF` to `(12, NELEE)` (2.3) | **Largest payoff** — `FRSORT`'s random per-element gather collapses from ~5 strided loads to 1-2 cache lines; also benefits `SYMAIN`, `OCFIX`, `CMRD` | Reordering only, but **cross-module**: `run_sim:302`, `INCM:4248`, `OCmod2.f90:1714` all declare or slice it |
| P4 | Decide whether the `FRSORT` merge key is intentional (1.10); scalarise `FRMB`'s `BALANC` accumulators (3.4); merge `FRDIM`'s first two passes (3.8); delete `write_dis` and `NGRID` (Part 5) | Ordering quality feeds `VSSIM`/`CMSIM`; the rest is tidying | 1.10 changes `ISORT` and therefore solver sweep order — **not** bitwise |

For P0 items 1.3 and 1.5, P1 (all), P2 and P3 the appropriate acceptance test is
**bitwise-identical output** with an unchanged sequence of accepted timesteps.

The items that can legitimately change results are 1.1, 1.2, 1.4 and 1.10 —
each because the current behaviour is wrong, not because the fix is
approximate. 1.1 and 1.10 additionally change input to other components
(`SYMAIN` and `VSSIM` respectively), so they should be validated on a catchment
with banks and a multi-link junction rather than on a V-catchment test.
