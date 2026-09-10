# Review of the V4.5.2 → V4.5.3 slowdown analysis

> **Status: applied.** The corrections and additions below have been folded into
> the three documents they review. This file is retained as the audit trail —
> what was checked, against which source, and why each change was made. Nothing
> here is outstanding work.

This document reviews `changes_v4.5.2_vsv4.5.3.md`, `impact_v4.5.2_vsv4.5.3.md` and
`optimise_v4.5.2_vsv4.5.3.md`. It records what was re-verified against the tagged
source, corrects a few points, adds findings the three documents miss, and proposes
extra diagnostics.

Method: source inspection of tags `V4.5.2` (`19e95cc`) and `V4.5.3` (`937175f`),
main `src` tree only. `Linux/src` excluded as instructed. Nothing was built or run.

---

## Part 1 — What re-checks out

The following claims were confirmed directly in the source and need no change.

| Claim | Verified at |
|---|---|
| `OCSIM` work arrays became local allocatables, allocated + explicitly zeroed + auto-deallocated on every call | `src/modules/OCmod.f90:1578-1650` |
| `ijedum(NELEE,4,2:3)` / `ijedum2(NLFEE,3,2)` are zeroed then fully repacked every step; volumes `8*NELEE + 6*NLFEE` ints, 1.20 MB → 8.48 MB | `OCmod.f90:1581-1584`, repack at `~1810` |
| Clear volume `16*NX²*(NY+5) + 8*NX*NY + 12*NX + 10*NEL` doubles | arithmetic checks out for the listed shapes |
| Capacity table (`NXEE` 250→1000, `NELEE` 30000→250000, `NXOCEE` 500→`4*NXEE`, …) | `src/parameters/sglobal.f90:83-127` |
| `mnplant` contains `do i=1,npltee; claimx(i)=2` and `do I=1,nel; NPLTYP(i,2)=1` nested inside `do nelm=nlf+1,nel` | `MNmod.f90:3625-3675` |
| `sedav` accumulates without reset in `FROUTPUT('main')` for the normal (outlet present) case; only the `mblink==0 .and. mbface==0` branch resets it | `FRmod.f90:2041, 2063-2069` |
| `CHARACTER(LEN=1000000) :: tmp` used for every dated met record, up to 4× per met boundary | `rest.f90:303, 326/401/436/457, 568/602/623` |
| `MNmod - Copy.f90` defines the same module and procedures as `MNmod.f90` | both files, 4488 lines each |
| Negative-Strickler `zi` adjustment commented out in both branches | `OCQDQMOD.F90:148-160, 233-240` |

The overall diagnosis — that `OCSIM` workspace churn is the leading candidate for a
general, catchment-dependent slowdown — holds up.

---

## Part 2 — Corrections

### 2.1 The V4.5.2 baseline arrays were almost certainly static, not stack

This matters because it changes *why* V4.5.3 is slow.

In V4.5.2, `EE(NXOCEE, NXOCEE, NYEE)` = `EE(500,500,250)` = **500 MB**, plus ~12 MB
for the other matrices. A 512 MB local array cannot live on a default stack. With
Intel Fortran's default `-auto-scalar` policy, fixed-size local arrays are placed in
**static storage**, so in V4.5.2 these arrays were `.bss`: mapped once at load,
zero-filled by the OS, pages faulted in lazily and only for the regions the solver
actually touched, and **retained across calls**. That is why V4.5.2 had no per-step
cost at all despite the enormous declarations.

Neither tag contains a build project (no `.vfproj`, no `.sln`, no makefile), so this
should be confirmed against whatever build settings were actually used. But the
conclusion is robust either way: V4.5.2 did no per-call initialisation, and that is
the whole of the regression.

Two consequences the documents get slightly wrong:

- **Memory footprint is not an argument for the V4.5.3 design.** `impact` §1 implies
  V4.5.3 is heavier; in fact for any model with `NX < 1000` the V4.5.3 *allocation*
  is smaller than V4.5.2's static block. The regression is purely per-step cost, and
  `optimise` should not sell the fix on memory savings.
- **`inhrf`, `GGGETHRF`, `inqsa`, `GGGETQSA` were already per-call allocations in
  V4.5.2.** They were declared `DIMENSION(total_no_elements)`, and
  `total_no_elements` is a module *variable* (`sglobal.f90:86`), not a parameter — so
  these were automatic arrays, allocated on every call in V4.5.2 too. Converting them
  to `ALLOCATABLE` changed nothing; the only new cost is the four `= 0.0d0`
  statements. `changes` §1 lists them among the new allocations. Minor, but it means
  they are the *least* interesting of the four groups.

### 2.2 The dominant cost is fresh-mapping, not memset bandwidth

`impact` §1 models the cost as bytes written per step and derives a lower bound. That
understates it substantially, and the understatement is the key to prioritising the
fix.

Multi-megabyte `allocate` goes over `MMAP_THRESHOLD`, so each timestep the allocator
gets **freshly mmap'd, unfaulted** pages. The subsequent `EE = 0.0d0` therefore costs
a minor page fault plus a kernel page-zero per 4 KB page — roughly an order of
magnitude more than a memset over already-resident memory. `deallocate` then munmaps
the region and returns it to the OS, so the next timestep repeats the whole cycle.
On top of that, every step's `EE` lands at a different address, so nothing stays warm
in cache or TLB between steps.

This changes the recommended ordering. `optimise`'s implementation series puts
"remove the clears" first and "allocate once" second. It should be the other way
round:

1. **Allocate once (move to module scope / `OCINI`), keep the clears.** This alone
   removes the mmap/munmap cycle and the page-fault storm, converting the remaining
   cost into a plain memset over resident memory. It is guaranteed **bitwise
   identical** — the arrays are still fully zeroed on entry — so it needs no
   numerical argument at all and can be merged on a pure timing result.
2. **Then remove the clears.** This is the change that requires proving every read
   is preceded by a write, so it should carry the risk on its own, after the safe
   win is already banked.

This also explains why the post-tag commit `0feebe7` (module-level allocation,
clears retained) was reported as effective despite not touching the clears.

### 2.3 The `mnplant` quadratic loop is a startup cost, not a per-step cost

`impact` §3 lists it under "Nitrate repeated initialisation" and `optimise` gives it
P1 alongside per-step work. It is guarded by `if (pass==1)` (`MNmod.f90:3577`), so it
runs exactly once. The magnitude is still worth fixing — `(NEL-NLF) * (NPLTEE + NEL)`
assignments, i.e. ~2.6×10⁹ for `NEL=10000` with `NPLTEE=NVEE=250000` — but it shows
up as a one-off startup stall, not as a per-step slope. That distinction matters for
attribution: a nitrate run that is slow from the first step has a different cause
from one that takes a minute to start.

### 2.4 The negative-Strickler change has a *direction*, not just an effect

`impact` §6 and `optimise` §9 say the change "could" alter the timestep count. The
sign is predictable.

In V4.5.2, once surface depth exceeded the storage depth, `zi(j) = GETHRF(kel) +
strxx(kel)/1000` with `strxx < 0` **lowered** the driving head by the storage depth,
so routed flow went smoothly to zero as depth approached the storage threshold —
i.e. the configured depth behaved as dead storage. V4.5.3 removes that, so flow is
now driven by the full head including the dead storage. Expect **larger overland
flows and steeper gradients at shallow depths**, hence more OC flow-correction
retries and **more accepted timesteps**, not fewer.

So this is not only a physics question parked for later: it is a second, independent
mechanism that can raise `runtime = NSTEP × cost_per_step` through the `NSTEP` term.
It should be measured in the same experiment as `OCSIM`, precisely so the two can be
separated. `impact` §6's suggested first diagnostic (compare final `NSTEP`) is the
right one; it just deserves a stated prediction to test against.

Note also that the commented-out line reads `0.95*strxx(kel)/1000`, not the V4.5.2
`strxx(kel)/1000`. Someone was mid-experiment with a 0.95 factor when it was
disabled. Whoever resolves the physics should know there were three candidate
behaviours in play, not two.

---

## Part 3 — New findings

These are not in any of the three documents. The first two are consequences of
exactly the change under investigation, which makes them directly relevant.

### 3.1 `GG` is allocated one row short — probable heap overflow (high severity)

In `OCSIM`, the row loop ends with an **unguarded** write
(`OCmod.f90:~1749`):

```fortran
IF (IROW.NE.NROWL) THEN
    ee(1:nsv,1:ncr,irsv) = ...      ! guarded, so EE only needs up to NROWL
ENDIF
gg(1:ncr,irsv) = JEMATMUL_VM(...)   ! NOT guarded; irsv = irow+1
```

On the final iteration `IROW = NROWL`, so this writes `GG(:, NROWL+1)`. That value is
then genuinely consumed immediately after the loop:

```fortran
IROW = NROWL
DD(1:ncr,IROW) = GG(1:ncr,IRSV)     ! IRSV = NROWL+1
```

But V4.5.3 allocates `GG(NX*4, NY)`. `NROWL` is set in `OCIND` by
`IF (ICOUNT.GT.0) NROWL = J` over `J = 1, NY`, so `NROWL = NY` whenever the last grid
row contains any active element — which is the normal case for a catchment whose
bounding box is the grid. The write is then one full column (`4*NX` doubles) past the
end of the allocation.

V4.5.2 was safe by accident: `GG(NXOCEE, NYEE)` with `NY < NYEE = 250` left slack
rows. Sizing exactly to `NY` removed that slack. `NROWST` is already declared
`NROWST(NY+1)` in `OCIND`, which is the same `+1` convention — `GG` should be
`(4*NX, NY+1)`, and any `optimise` §3 re-sizing must preserve that.

Symptoms would be intermittent: for small `NX` the overrun often lands in malloc
slack and is silent; for larger allocations that end on a page boundary it is a
segfault. Either way it is undefined behaviour introduced in this range, and it is
worth checking before attributing any V4.5.3 instability to the physics changes.

`optimise` §3 does warn abstractly that "arrays carrying information between rows can
access the next-row slot", but presents it as a caution for future work rather than
as a defect already present. It should be reclassified as a bug fix and hoisted to
P0 — it is the one item here that can corrupt results rather than merely slow them.

### 3.2 The `NXOC` capacity guard no longer guards the allocation

`OCIND` computes the maximum active row width and validates it
(`OCmod.f90:995-998`):

```fortran
IF (NXOC.GT.NXOCEE) THEN
    CALL ERROR(FFFATAL, 1006, PPPRI, 0, 0, 'ARRAY DIMENSION OF NXOC TOO SMALL')
ENDIF
```

In V4.5.2 this was correct: the arrays were `NXOCEE = 500` wide and the check tested
against 500. In V4.5.3 the arrays are `4*NX` wide but the check still tests against
`NXOCEE = 4*NXEE = 4000`. **Allocation and validation have decoupled.** A model with
`4*NX < NXOC ≤ 4000` now silently overruns instead of failing cleanly.

Is `4*NX` actually a safe bound? Counting `OCIND`'s inner loop, each grid column `I`
in a row can contribute, with bank elements enabled (`BEXBK`): 3 elements at the west
face (bank, link, bank), 1 grid square, and 3 at the south face — **up to `7*NX` per
row**, not `4*NX`. `4*NX` is safe only when `BEXBK` is false. It is generous for
typical sparse channel networks, but nothing in the code enforces it.

This strengthens `optimise` §3 considerably. That section recommends sizing from the
maximum active row width as a *performance* measure; it is also the *correctness*
fix, because it makes the allocation and the guard the same quantity. Two
implementation notes for whoever does it:

- `NXOC` is declared as a plain local in `OCIND` (`INTEGER :: ... NXOC`) and is
  discarded on return — it used to live in `SPEC.OC`. It must be promoted to an
  `INTENT(OUT)` argument or module variable, or recomputed as
  `MAXVAL(NROWST(2:NROWL+1) - NROWST(1:NROWL))`.
- Once sized from `NXOC`, change the guard to test against the value actually
  allocated, and keep it as a fatal error.

### 3.3 `OCABC`'s dummy arguments over-declare their actuals

`OCABC` declares `DOUBLEPRECISION, INTENT(OUT) :: AA(NXOCEE), BB(NCR), CC(NXOCEE)`
(`OCmod.f90:212`), and `OCSIM` passes `AA(:,IND)` and `CC(:,IND)`, whose extent is
`4*NX`. Whenever `NX < 1000`, the dummy claims more storage than the actual has.

In practice `OCABC` only touches `AA(1:NSV)` and `CC(1:NPR)`, so no overrun occurs
today — but the declaration is invalid under the standard, and it will trip
`-check bounds` immediately. Since `optimise` §2 proposes validating the removal of
the clears by running with bounds checking and FP traps, this needs fixing *first*,
or the diagnostic run will drown in false positives from this and 3.1. Change the
dummies to `AA(NSV)`/`CC(NPR)` or assumed-shape.

### 3.4 Nitrate: `cdi`/`cdit` are written to the wrong row (correctness)

In `mnplant`'s initialisation (`MNmod.f90:3583-3592`):

```fortran
do i = 1,nv
    call alredi ( 0,mnpl,mnoutpl,':MNP10',1,1,idum )
    nvalue(i) = idum(1)
    ...
    do ntb = 1,idum(1)
        cdi(nv,ntb)  = dummy(2*ntb-1)     ! nv, not i
        cdit(nv,ntb) = dummy(2*ntb)
    enddo
enddo
```

The loop index is `i` but the store index is `nv`. Every vegetation type's canopy
density index table is written into row `nv`, overwriting the previous one. The data
is read back as `cdi(jplty,i)` / `cdit(jplty,i)` (`MNmod.f90:3667-3671`), so for every
plant type except `jplty == nv` the interpolation reads a `save`d array that was never
assigned.

This is a plain typo, but it silently corrupts plant uptake for all but one
vegetation type. It belongs in the nitrate section of `optimise` §5 as a correctness
item ahead of the performance items — there is no point optimising a kernel that is
reading uninitialised data.

While there: `cdi` and `cdit` are declared `(npltee, nvalee)` = `(250000, 30)`, i.e.
60 MB each as `save`d locals, 120 MB of `.bss` for tables whose real extent is
`(NV, 30)`. The `NVEE` 30000→250000 bump inflated these 8.3×. Mostly harmless as
untouched `.bss`, but it is the same `NPLTEE`-for-`NV` confusion that causes the
`claimx(1:npltee)=2` sweep, and both should be fixed together — which is the concrete
instance of the general advice in `optimise` §8.

### 3.5 `INITIALISE_AL_C` was moved earlier — a behaviour change, not a refactor

`VSmod.f90` moves `CALL INITIALISE_VSMOD()` / `CALL INITIALISE_AL_C()` from after
`VSCONC` (now commented out at `VSmod.f90:2058-2059`) to the end of the loop-1200
block at `VSmod.f90:1130-1133`, and adds a new `INITIALISE_AL_C2()` at
`VSmod.f90:2012`.

These routines both allocate **and zero** (`AL_C.F90:204-234`). Moving a zeroing
initialiser earlier is not neutral: anything written to `QVSH`, `QVSV`, `JVSACN`,
`JVSDEL` and friends between the old and new call sites was previously wiped and now
survives. That could be a deliberate fix, or it could quietly resurrect stale values
that the old ordering was masking.

None of the three documents mentions this file at all. It should at least be audited,
because it changes initial state for the VSS component and therefore sits upstream of
any "is the timestep sequence identical?" comparison. If the `UZNEXT` sequences turn
out to differ between the tags, this is a candidate cause alongside the
negative-Strickler change, and it would be easy to misattribute.

### 3.6 Six modified `src` files are not covered by the analysis

`changes` says "24 files under `src`" but discusses about 18. Not covered:
`OCmod2.f90`, `VSmod.f90`, `run_sim.f90`, `utilsmod.f90`, `AL_D.f90`, `is_cc.f90`.
Most are benign, but two are worth a line each:

- **`utilsmod.f90:249-255`** — `HOUR_FROM_DATE`'s date-validation failure path now
  does `write(...) 'paused, type [enter] to continue'` followed by `read (*,*)`
  before `stop`. The same interactive pause was added to several I/O error paths in
  `FRmod.f90` (e.g. `FRmod.f90:2033-2038`). **In a batch or HPC run these hang
  forever instead of exiting.** For anyone timing these versions, a "very slow run"
  may in fact be a process blocked on stdin. Worth checking before profiling, and
  worth guarding on an interactive-terminal test.
- **`run_sim.f90:187`** — `call deallocate_colm_cg()` frees `JKZCOL`, `JOLFN`, `NOL`,
  `NOLCE` after the first contaminant step. I checked this: those four are referenced
  only in `FRmod`'s `INCM` (initialisation) and in comment headers in `CMmod`; the
  per-step `COLMSM` path uses only `NOLBT` and `NOLCEA`, which are retained. **This
  one is correct** — recording it so nobody has to re-derive it.

---

## Part 4 — Additional suggestions

### 4.1 Diagnostics that need no build

The stated constraint is that these tags have no CMake setup. Several of the
hypotheses can still be discriminated using the **existing binaries and existing run
logs**, which is worth doing before investing in a legacy build environment.

**(a) Pad the grid.** `NXEE` is now 1000, so a model can be re-gridded with extra
empty columns/rows without recompiling. `OCSIM`'s clear cost scales as `NX²·NY` while
the useful solver work scales with the active channel network. If runtime rises
roughly quadratically in `NX` when only inactive padding is added, that is a decisive
confirmation of the `OCSIM` matrices. Under V4.5.2 the same padding should be nearly
free, because the arrays were fixed-size and only the touched regions cost anything.

**(b) Run a trivially small model for many timesteps.** For `NEL` of a few tens, the
matrix clears are negligible and the per-step cost is dominated by the
capacity-sized topology adapters. Predicted per-step adapter traffic: V4.5.2 ≈ 1.2 MB
(copy only), V4.5.3 ≈ 17 MB (8.48 MB zero + 8.48 MB copy). So a tiny model should
show a **large** V4.5.3/V4.5.2 ratio if `ijedum` dominates, and only a small one if
the `NX²·NY` matrices dominate. Combined with (a), this separates finding 1 from
finding 2 in `impact`'s ranking without a single line of instrumentation.

**(c) Correlate existing slowdown factors against `NX²·NY / NEL`.** If per-model
slowdown factors have already been collected across several catchments, plotting them
against `NX²·NY` (matrix hypothesis), against a constant (adapter hypothesis), and
against `NSTEP` ratio (physics hypothesis) discriminates all three from data already
in hand.

**(d) Compare VSZ against RSS.** V4.5.2 should show a very large virtual size with
modest resident size — the 512 MB `.bss` is mostly never touched. V4.5.3 should show
a smaller virtual size but a resident size that reflects the whole workspace, because
every page is written by the clears each step. `/usr/bin/time -v`, or Task Manager's
working set on Windows, is enough. This tests §2.1 and §2.2 at once, and it needs
only that both binaries run.

**(e) Read `NSTEP` out of the existing run summaries.** `run_sim.f90:9900` already
prints the step count on normal completion. If old logs exist for both versions on
the same input, the per-step-versus-more-steps question may already be answered.

### 4.2 Give the A/B sequence a stop rule

`impact`'s confirmation sequence and `optimise`'s implementation series both list the
steps but not what constitutes success at each one. Suggest making the acceptance
explicit, because the risk profile differs sharply across the list:

| Step | Acceptance |
|---|---|
| Fix `GG` bound (3.1), `OCABC` dummies (3.3) | Bitwise identical output; enables meaningful bounds-checked runs |
| Allocate `OCSIM` workspace once, clears retained | **Bitwise identical by construction** — timing-only decision |
| Remove the clears | Bitwise identical + clean run under bounds checking and FP traps with poisoned workspace |
| Size from `NXOC` | Bitwise identical; plus a deliberate over-capacity model that must hit the (now correct) fatal error |
| Topology adapters to active size | Bitwise identical; needs a small model *and* one near the limits |
| Negative-Strickler | **Not** bitwise identical — this is the one step where output changes are expected, so it must be last and separately reviewed |

The value of stating this is that everything except the last row has an objective,
automatable pass/fail that does not require anyone to judge whether a hydrograph
difference is acceptable.

### 4.3 Note the two-tree divergence is now a physics divergence

`changes` §7 correctly warns that `src` and `Linux/src` are not equivalent and that
"V4.5.3" does not uniquely identify an executable. It is worth stating the sharpest
instance: the negative-Strickler head adjustment is **off** in `src` and **on with a
0.95 factor** in `Linux/src`. That is not a packaging difference, it is two different
models under one tag. Any timing or output comparison must record the tree, and the
divergence should be resolved before, not after, the performance work — otherwise
half the A/B results will not be portable between platforms.

### 4.4 Guard against the duplicate nitrate module at build time

`optimise` §5 recommends removing `MNmod - Copy.f90`. Worth adding: whichever build
system is eventually used should **fail** on two files defining the same module, not
pick one. The failure mode here is silent — the two variants differ in dummy-array
bounds and in whether hard-coded nine-cell output is enabled — so a wildcard glob can
change both results and runtime depending on directory ordering. A duplicate-module
check is cheap and prevents the class, not just this instance.

### 4.5 Order the nitrate work by correctness first

`optimise` §5 is ordered by cost. Suggest reordering so that 3.4 (`cdi`/`cdit`
mis-indexing) comes first, followed by the `NPLTEE`-versus-`NV` sizing confusion,
then the performance items. Profiling and optimising `mnplant` while it interpolates
from uninitialised tables would produce timings that describe code nobody wants to
keep.

---

## Summary of proposed changes to the three documents

| Where | Change |
|---|---|
| `impact` §1 | Add the fresh-mmap / page-fault mechanism; drop the implication that V4.5.3 uses more memory than V4.5.2 |
| `impact` §1, `optimise` series | Swap the order: allocate-once (bitwise safe) before remove-clears (needs proof) |
| `impact` §3, `optimise` §5 | Mark the `mnplant` quadratic loop as one-time startup, not per-step |
| `impact` §6, `optimise` §9 | State the predicted direction (more `NSTEP`) for the negative-Strickler change |
| `optimise` §3 | Promote to P0 and reframe as a bug fix: `GG` needs `NY+1`; the `NXOC` guard must test the allocated size; `NXOC` must be exported from `OCIND` |
| `optimise` §5 | Add the `cdi`/`cdit` indexing bug ahead of the performance items |
| `changes` | Cover `VSmod.f90` (initialiser moved), `run_sim.f90`, `utilsmod.f90`; flag the interactive `read (*,*)` pauses on error paths |
| all | Add the buildless diagnostics in §4.1 as the first experiments, given the CMake constraint |
