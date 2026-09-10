# Optimising the V4.5.3 changes without losing their intent

## Objectives

The optimisations should retain the useful changes made between V4.5.2 and
V4.5.3:

- models should no longer be constrained unnecessarily by the old small
  compile-time limits;
- the nitrate model, dated meteorological input, and new outputs should remain
  available;
- the open-channel solver should produce the same numerical result for a given
  timestep; and
- hydrological behaviour changes, such as the treatment of negative Strickler
  values, should be decided separately from implementation-performance work.

For changes intended to affect performance only, the safest acceptance test is
an unchanged sequence of accepted timesteps and bitwise-identical output where
practical. A documented numerical tolerance is appropriate only where a
deliberate change in evaluation order makes bitwise identity impossible.

## Recommended order of work

| Priority | Change | Expected benefit | Numerical risk |
|---|---|---|---|
| P0 | Fix the `GG` allocation bound and the `OCABC` dummy declarations | Correctness; unblocks bounds-checked validation of everything below | None — pure defect fix |
| P0 | Allocate the `OCSIM` workspace once, outside the timestep loop | Very high; removes the per-step mapping and page-fault cycle | **None — bitwise identical by construction** |
| P0 | Remove the full-array initialisation in `OCSIM` | High for medium and large channel grids | Low, after checking active-slice coverage |
| P0 | Stop copying and clearing fixed-capacity topology arrays on every timestep | High where `NELEE` is much larger than `NEL` | Low to medium, depending on interface chosen |
| P1 | Size open-channel matrices from the maximum active row width rather than `4*NX`, and make the `NXOC` guard test that value | High memory-bandwidth saving, and closes a silent-overrun path | Medium because row bounds must be audited carefully |
| P1 | Remove the remaining `OCFIX` state adapters or restrict them to active slices | Moderate | Medium; automatic-differentiation interfaces need checking |
| P1 | Fix the `cdi`/`cdit` mis-indexing in `mnplant` | Correctness for all but one vegetation type | None — pure defect fix |
| P1 | Remove avoidable full-grid work in the nitrate model | High for nitrate simulations only | Medium |
| P2 | Replace the million-character dated-input record buffer | Moderate for dated-input-heavy runs | Low |
| P2 | Reduce and correct optional-output aggregation | Low to moderate, configuration-dependent | Low |

The P0 work directly addresses costs that occur in every ordinary simulation.
The nitrate work is important but cannot explain the slowdown in a run where
the nitrate option is disabled.

Two notes on ordering, which differs from earlier drafts of this document.

Allocating the workspace once now comes **before** removing the clears. The
arrays remain fully zeroed on entry, so that change cannot alter results and
can be accepted on a timing measurement alone. It is also likely to be the
larger share of the benefit: the V4.5.3 allocations are big enough to be served
by fresh memory mappings, so the clears are currently paying a page fault per
4 KB rather than streaming over resident memory. Removing the clears is the
change that requires proving every read is preceded by a write, and it should
carry that risk by itself once the safe win is banked.

The two correctness fixes are listed as P0 and P1 rather than being deferred,
because both sit inside the code being optimised. The `GG` and `OCABC` defects
will fire under the bounds checking that section 2 relies on for validation, and
optimising `mnplant` before fixing its indexing would characterise behaviour
nobody intends to keep.

## 0. Fix the defects introduced with the workspace conversion

Three problems came in with the `OCSIM` conversion itself. They are cheap to fix
and they gate the validation strategy used by the rest of this document, so they
go first.

**`GG` needs a second dimension of `NY + 1`.** The row loop ends with an
unguarded `gg(1:ncr,irsv) = ...` at `irow == NROWL`, and `GG(:, NROWL+1)` is read
immediately afterwards by `DD(1:ncr,IROW) = GG(1:ncr,IRSV)`. V4.5.3 allocates
`GG(NX*4, NY)`, and `NROWL = NY` whenever the last grid row holds any active
element — the normal case. V4.5.2 was safe only through the spare rows left by
`GG(NXOCEE, NYEE)`; sizing exactly to `NY` removed that margin. Use `NY + 1`,
matching the `NROWST(NY+1)` convention already present in `OCIND`. `EE` does not
need the extra row, because its next-row write is guarded by
`IF (IROW.NE.NROWL)`, but state that reasoning in a comment rather than leaving
the asymmetry unexplained.

**`OCABC` must not over-declare its dummies.** It declares `AA(NXOCEE)` and
`CC(NXOCEE)` while `OCSIM` passes actuals of extent `4*NX`. No overrun occurs
today, since only `AA(1:NSV)` and `CC(1:NPR)` are touched, but the declaration is
invalid whenever `NX < 1000`. Change to `AA(NSV)` and `CC(NPR)`, or to
assumed-shape.

Both of these will trip `-check bounds`. Section 2 proposes validating the
removal of the clears by running with bounds checking and floating-point traps;
that diagnostic is unusable until these are fixed.

**Re-couple the `NXOC` guard to the allocation.** This is described in section 3,
because the fix is the same as the sizing change.

## 1. Make the `OCSIM` workspace persistent

The V4.5.3 design correctly moved several arrays away from fixed declarations,
but it allocates them in `OCSIM`, which is called once per main timestep. Move
the workspace to module state, or preferably to a small workspace derived type,
and allocate it once during open-channel initialisation.

Do this **before** removing the clears, not after. Keeping the clears in place
makes the change bitwise identical by construction, so it needs no numerical
argument and can be merged on a timing result. It is also where most of the
benefit is: the V4.5.3 allocations exceed the allocator's mmap threshold, so
every timestep currently receives freshly mapped, unfaulted pages and the clears
pay a minor page fault plus a kernel page-zero for each 4 KB, rather than
streaming over resident memory. The matching deallocation unmaps the region
again, and the next step's workspace lands at a different address, so nothing
stays warm in cache or TLB. Allocating once collapses all of that to a plain
`memset`.

Note also what this change is *not* for. The V4.5.2 arrays were fixed-size
locals in static storage — `EE(500,500,250)` alone is 500 MB — mapped once at
load and faulted in lazily. For any model with `NX < 1000` the V4.5.3 allocation
is therefore *smaller* than the V4.5.2 block. Do not justify this work as a
memory saving; the case rests entirely on per-timestep cost.

`inhrf`, `GGGETHRF`, `inqsa` and `GGGETQSA` deserve less attention than the
others here: they were declared `DIMENSION(total_no_elements)` in V4.5.2 with
`total_no_elements` a module variable, so they were already automatic arrays
allocated on every call. Only their zeroing is new.

The initialisation should:

1. construct the channel row information with `OCIND`;
2. derive the required active dimensions from that information;
3. allocate the workspace once; and
4. retain a finalisation or dimension-change path for programs that run more
   than one model in the same process.

The current post-V4.5.3 source already contains a partial implementation of
this idea in commit `0feebe7` ("made some arrays in hot loops module level
allocated ones"). That is useful corroboration, but it does not complete the
optimisation: the large arrays are still blanket-cleared in each `OCSIM` call,
and their first dimension remains the conservative `4*NX`.

If that later implementation is used as the starting point, the call ordering
also needs attention. The active row widths can only be used to size the
workspace after `OCIND` has populated `NROWST` and the related row metadata.

Allocation failures should be checked with `STAT=` and reported with the model
dimensions. This makes an invalid model or insufficient memory fail at
initialisation instead of in the timestep loop.

## 2. Remove redundant full-array clearing

V4.5.3 assigns zero to every newly allocated open-channel work array before
calling the solver. Once section 1 is in place this is what remains of the new
per-step cost, and unlike section 1 it is a change that can alter results if the
active-slice analysis is wrong. Do it second, and benchmark it separately so its
contribution is known rather than assumed. The
solver already initialises the portions it uses:

- `OCABC` initialises the active lengths of `AA`, `BB`, `CC`, and `FF`;
- the active portions of `TM1`, `TM2`, `TV1`, `TV2`, `EE`, `GG`, and `DD` are
  assigned before they are consumed; and
- the `OCFIX` adapters are filled from the corresponding model arrays before
  the differentiated routine is called.

Remove the whole-array assignments rather than replacing them with another
full-array mechanism. If a compiler or debug runtime happens to initialise
allocated memory, that is not a substitute for proving the active slices are
defined.

A safe way to validate the change is to add a debug-only mode that fills the
workspace with signalling NaNs or a recognisable integer poison value at the
start of a timestep, then run with bounds checking and floating-point exception
traps. This diagnostic mode should not be enabled in production builds. It can
expose an overlooked read while the normal implementation avoids all blanket
initialisation.

## 3. Size matrices from the channel network, not the grid width

The work matrices are declared with a leading dimension of `4*NX`. This is
usually much larger than the number of variables in any active channel row.
After `OCIND`, calculate the maximum active row width from the row start
information, for example from the relevant differences of successive `NROWST`
entries, and allocate the leading dimensions to that value.

This is not only a performance change. `4*NX` is not a *proven* upper bound, and
V4.5.3 no longer checks it against anything. `OCIND` still validates the
computed maximum row width against the compile-time capacity:

```fortran
IF (NXOC.GT.NXOCEE) THEN
    CALL ERROR(FFFATAL, 1006, PPPRI, 0, 0, 'ARRAY DIMENSION OF NXOC TOO SMALL')
ENDIF
```

In V4.5.2 that was consistent, because the arrays were `NXOCEE = 500` wide and
the test used 500. In V4.5.3 the arrays are `4*NX` wide while the test still uses
`NXOCEE = 4*NXEE = 4000`, so allocation and validation now measure different
things and a model between the two bounds overruns silently instead of failing
cleanly. Counting `OCIND`'s inner loop, one grid column can contribute three
elements at the west face, one grid square, and three at the south face when
bank elements are enabled — up to `7*NX` per row. `4*NX` is provably safe only
when `BEXBK` is false.

Sizing from the actual maximum row width therefore fixes the guard as well as
the cost, because the allocation and the validated quantity become the same
number. Two implementation notes:

- `NXOC` is a plain local in `OCIND` and is discarded on return; it used to live
  in `SPEC.OC`. Promote it to an `INTENT(OUT)` argument or module variable, or
  recompute it as `MAXVAL(NROWST(2:NROWL+1) - NROWST(1:NROWL))`.
- Keep the guard fatal, and test it against the value actually allocated.

The audit must include all row indices used by the solver. In particular,
arrays carrying information between rows can access the next-row slot, so their
second dimension should have explicit bounds that include every value from the
first active row through `NROWL+1`. **V4.5.3 already violates this** for `GG`;
see section 0. Explicit lower bounds are preferable to relying on an oversized
`NY` allocation whose unused prefix happens to make the indices valid.

A ragged allocation per row could reduce memory further, but it would add
indirection in the hot solver. A single dense workspace sized to the maximum
active row is the better first implementation.

## 4. Eliminate fixed-capacity topology adapters

V4.5.3 copies the connectivity arrays into `ijedum` and `ijedum2` on every
timestep. Their sizes are based on the enlarged maxima `NELEE` and `NLFEE`, not
the active element count, and the destination arrays are also cleared first.

Preferred options, in order, are:

1. change `OCFIX` to accept the existing connectivity arrays directly using
   explicit or assumed-shape interfaces;
2. if an adapter is required by the automatic-differentiation boundary, pack
   only the active entries once during initialisation because channel topology
   is static during a simulation; or
3. as a minimal patch, copy only the active sections and never clear the unused
   capacity.

The adapter should be sized from `NEL` and the maximum active confluence index,
not from `NELEE=250000` and `NLFEE=20000`. Tests should include a model near the
new limits as well as a small model, because this change removes reliance on
unused capacity being zero.

The same principle applies to `inhrf`, `inqsa`, `GGGETHRF`, and `GGGETQSA`.
The immediate safe improvement is to allocate them once and fill only their
active ranges. A later change can let `OCFIX` operate directly on the model's
head and discharge arrays, provided the differentiated interface and
input/output intent are verified.

## 5. Optimise nitrate computations without changing the model

Nitrate simulations necessarily perform substantially more work than V4.5.2,
because the physics is new. Several implementation costs are nevertheless
avoidable. Take the correctness item first: there is no value in profiling a
kernel that reads uninitialised data.

### Fix the canopy density index indexing (correctness, do first)

`mnplant`'s initialisation reads the canopy density index tables with the wrong
store index:

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

The loop index is `i` but the store index is `nv`, so each vegetation type's
table overwrites the previous one in row `nv`. The tables are read back as
`cdi(jplty,i)` and `cdit(jplty,i)`, so every plant type except `jplty == nv`
interpolates from a `save`d array that was never assigned. Fix the index, then
add a check that every row read back was actually populated.

### Fix repeated large initialisation

In `mnplant`, the loop over land elements contains another loop that assigns
`NPLTYP(1:NEL,2)=1`. This turns a simple initialisation into approximately
`NEL**2` assignments. Move that initialisation outside the enclosing element
loop, or use one whole active-slice assignment before the loop. The same outer
loop also repeats `CLAIMX(1:NPLTEE)=2`; move this assignment out as well and
restrict it to the active plant types. Since `NPLTEE` is the enlarged
`NVEE=250000` capacity, this second mistake is the larger of the two.

Note that the whole block is guarded by `if (pass==1)`, so it runs exactly once.
Its cost — roughly `(NEL-NLF) * (NPLTEE + NEL)` assignments, about 2.6 × 10⁹ for
`NEL = 10000` — appears as a one-off stall before the first nitrate step, not as
a per-step slope. Worth fixing, but time it separately from the per-step kernels
so the two are not reported as a single ratio.

The same `NPLTEE`-for-`NV` confusion also sizes the lookup tables themselves:
`cdi` and `cdit` are declared `(npltee, nvalee)` = `(250000, 30)`, i.e. 60 MB
each as `save`d locals, where `(NV, 30)` is meant. Fix the sizing at the same
time as the loop bounds; they are the same mistake.

### Use Fortran-contiguous storage and loop order

Many nitrate arrays are allocated as `(NEL,NCETOP)` but traversed with the cell
or layer index in the inner loop. In column-major Fortran this accesses memory
with a stride of `NEL`. For nitrate-internal arrays, prefer
`(NCETOP,NEL)` and keep the first index in the inner loop. Where external
interfaces require the existing orientation, either change the loop order when
dependencies allow it or convert once at the interface rather than repeatedly.

### Avoid validation copies

`mnerr3` copies eleven complete active fields through `dummy4` and scans
them every timestep; `mnerr4` adds another validation pass. Preserve useful
finite-value and range checks, but do not require full-field copies:

- inspect the source arrays directly;
- fuse checks into an existing pass where this does not obscure error
  reporting; and
- make exhaustive diagnostics a clearly named strict/debug option if they are
  too expensive for production runs.

Errors should still report the element, layer, variable, and offending value.

### Do output work only when output is due

`mnout` updates 15 per-cell cumulative arrays and performs an additional
full-grid aggregation every timestep, although it writes only at the output
interval. Accumulate the required domain totals directly as scalars where the
published output is aggregate-only. If future cell-level cumulative output is
required, make those arrays conditional on that option.

The final domain scan of current pool state should be inside the output-due
branch. This must not discard the accumulation between output times: update
cumulative domain scalars each timestep and format them only at the defined
interval. Preserve their cumulative-from-start semantics unless interval totals
are an explicit new requirement.

### Reduce repeated setup and passes

- Initialise `TA(1:NV)=10` once if 10 is genuinely a constant; otherwise use
  the intended meteorological or model input.
- Precompute static soil, vegetation, and element metadata used by multiple
  kernels.
- Fuse compatible element/layer passes only after documenting their data
  dependencies.
- Reconcile the differences in `MNmod - Copy.f90`, retain one authoritative
  nitrate module, and remove the second module definition from the maintained
  source set. This prevents wildcard builds from selecting the maximum-capacity
  variant or compiling the same module twice.

The nonlinear solvers allow up to 20 iterations with a tolerance of `1e-12`.
Record iteration counts before changing them. Skipping provably inactive or
already-converged cells can be considered, but relaxing the tolerance should
not be the first performance fix because it changes the model solution.

After serial memory access and redundant passes have been fixed, independent
element loops are candidates for OpenMP. Reductions and output totals must use
a reproducible scheme if deterministic results are required.

## 6. Make dated meteorological parsing proportional to the record

The dated-input path declares a one-million-character temporary record and
uses it for each dated line. Replace this with one of:

- direct formatted parsing of the timestamp and values;
- a reusable dynamically sized record whose capacity grows only when needed;
  or
- a bounded line buffer based on the actual supported maximum number of
  stations and fields.

Parse the timestamp once per record, validate ordering at the point it is read,
and avoid repeated character-to-date conversions. Retain the undated fast path
so installations that do not request dated data incur no new parsing work.

## 7. Keep optional output out of the hot path

For new water-table, sediment, and contaminant outputs:

- compute component totals only when the corresponding output is enabled;
- buffer formatted output where practical;
- keep accumulation separate from file formatting; and
- avoid repeated date conversion when several outputs share the same model
  time.

The sediment-output value `sedav` must be reset to zero at the start of every
`FROUTPUT('main')` sediment calculation, immediately before summing the
fractions. In V4.5.3 it is initialised only in the `'start'` branch
(`FRmod.f90:2041`) and is not declared `SAVE`. In `'main'` the accumulation
`sedav = sedav + QSED(...)` (`FRmod.f90:2069`) has no preceding reset on the
normal path; only the one-dimensional `mblink == 0 .and. mbface == 0` branch
zeroes it. The outlet case therefore reads an undefined value, and compilers
that retain the local storage make the supposedly instantaneous value grow
monotonically across timesteps. This is principally a correctness problem, but
it must be fixed to make output and performance comparisons meaningful.

Output code should also explicitly handle one-dimensional or no-outlet cases
instead of depending on inactive array entries.

## 8. Continue active-size allocation carefully

The conversion of vertical, vegetation, soil, and contaminant work arrays to
active dimensions is sound in principle. Continue that work for remaining
large fixed-capacity arrays, but distinguish between:

- persistent model state, allocated once per simulation;
- reusable solver workspace, allocated once per set of dimensions; and
- genuinely small procedure-local temporaries.

Do not allocate or deallocate the first two categories in routines reached on
every timestep. Avoid reusing a single maximum parameter for unrelated concepts
such as vegetation types and meteorological stations; separate bounds make both
validation and allocation sizes clearer.

## 9. Treat the negative-Strickler change as a physics decision

The main-source implementation in V4.5.3 stops adjusting effective water level
by the negative Strickler value while retaining the roughness switch. That
removes a few arithmetic operations and is not a direct slowdown, but it alters
discharges, convergence, and the number of accepted timesteps.

The direction is predictable, which makes this testable rather than merely
uncertain. In V4.5.2 the routed head fell smoothly to zero as depth approached
the storage threshold, so the configured depth acted as dead storage; V4.5.3
drives flow with the full head including that storage. Expect larger flows and
steeper gradients at shallow depths, more flow-correction retries, and therefore
**more** accepted timesteps. This is a second, independent route to a slowdown,
acting on `NSTEP` rather than on cost per step, and it should be measured in the
same experiment as `OCSIM` so the two can be separated.

Note that three behaviours are in circulation, not two. The commented-out line
reads `0.95*strxx(kel)/1000`, not the V4.5.2 `strxx(kel)/1000`, and the 0.95
variant is left *active* in the `Linux/src` tree. So: full adjustment (V4.5.2
`src`), none (V4.5.3 `src`), and 0.95 (V4.5.3 `Linux/src`). Resolve that
divergence before the performance work, or A/B results will not carry between
platforms.

Do not combine a change to this behaviour with the workspace optimisation.
First establish the intended representation of surface storage:

- restore the previous effective-head adjustment if the negative value encodes
  a storage depth;
- adopt the 0.95 variant if that experiment had a rationale worth recovering; or
- implement the storage-volume/flux relationship explicitly if the old
  adjustment was only an approximation.

Whichever interpretation is selected needs a targeted regression test. Compare
the complete `UZNEXT` sequence and accepted timestep count, not just the final
hydrograph. This will separate an indirect increase in simulation steps from
the per-step regression in `OCSIM`.

If the `UZNEXT` sequences do differ between the tags, this is not the only
candidate cause. `VSmod.f90` moved `INITIALISE_VSMOD()` and `INITIALISE_AL_C()`
earlier and added `INITIALISE_AL_C2()`; since those routines zero as well as
allocate, anything written to the affected arrays between the old and new call
sites was previously wiped and now survives. Audit that before attributing a
timestep-sequence difference to the Strickler change.

## Verification plan

Use a small set of short deterministic cases covering:

| Case | Purpose |
|---|---|
| Hydrology only, ordinary channel parameters | Measures the universal `OCSIM` cost |
| Negative-Strickler surface storage | Detects changes in timestep sequence and routing behaviour |
| Sediment enabled | Exercises new sediment output and `sedav` reset |
| Contaminant enabled, nitrate disabled | Separates general contaminant/output overhead from nitrate |
| Nitrate enabled | Measures new physics and nitrate-specific optimisations |
| Dated and undated meteorological input | Measures parser overhead and confirms compatibility |
| One-dimensional/no-outlet case | Checks output guards and unusual bounds |
| Water-table and ZQ options | Covers the remaining modified paths |

For each case record:

- wall-clock and CPU time, preferably split into initialisation, solver, and
  output sections;
- peak resident memory;
- number of calls and inclusive time for `OCSIM`, `OCFIX`, and the nitrate
  driver;
- accepted timestep count and the `UZNEXT` sequence;
- nonlinear nitrate iteration counts; and
- water and constituent balance errors plus numerical output differences.

Before collecting any wall-clock figure, confirm the run is not blocked on an
interactive pause. Error paths added in this range — `utilsmod.f90`'s date
validation and several `FRmod.f90` file-open and write failures — now do
`read (*,*)` before `stop`. In a batch or scheduled run these wait on standard
input indefinitely, so an apparently pathological runtime may be a hung process
rather than a slow one.

Even though these tags do not provide the modern CMake build, lightweight
timing counters can be added around the identified routines when a supported
legacy build environment is available. A sampling profiler is preferable to
instrumenting every small nitrate routine, because fine-grained timers can
distort short kernels.

### Measurements that need no rebuild

Several hypotheses can be separated using the existing executables, which is
worth doing before investing in a legacy build environment:

- **Pad the grid.** `NXEE` is now 1000, so a model can be re-gridded with extra
  inactive columns and rows without recompiling. The clear cost scales as
  `NX**2 * NY` while the useful solver work scales with the active channel
  network, so a roughly quadratic rise in runtime from inactive padding confirms
  the block-solver matrices. The same padding should be nearly free in V4.5.2.
- **Run a tiny model for many steps.** At `NEL` of a few tens the matrix clears
  are negligible and per-step cost is dominated by the capacity-sized topology
  adapters (about 1.20 MB per step in V4.5.2 against about 17 MB in V4.5.3). A
  large ratio on a tiny model points to `ijedum`; a small one points to the
  matrices. Together with the padding test this separates the two leading
  hypotheses without instrumentation.
- **Compare virtual size against resident size.** V4.5.2 should show a large
  virtual size with a modest resident set, because most of its static workspace
  is never touched; V4.5.3 should show the reverse profile because the clears
  write every page every step.
- **Read `NSTEP` from existing logs.** `run_sim.f90` already prints the step
  count on normal completion, so the per-step-versus-more-steps question may
  already be answerable from runs that have been done.

### Acceptance criteria

Each step has an objective pass condition, and they are not all the same. Making
this explicit matters because only the last row requires anyone to judge whether
a hydrological difference is acceptable.

| Step | Acceptance |
|---|---|
| Fix `GG` bound and `OCABC` dummies | Bitwise identical output; enables meaningful bounds-checked runs |
| Allocate `OCSIM` workspace once, clears retained | **Bitwise identical by construction** — a timing-only decision |
| Remove the clears | Bitwise identical, plus a clean run under bounds checking and FP traps with a poisoned workspace |
| Size from `NXOC` | Bitwise identical, plus a deliberately over-capacity model that must hit the corrected fatal error |
| Topology adapters to active size | Bitwise identical, on both a small model and one near the new limits |
| Nitrate indexing and sizing fixes | Output *changes*, and should: previously uninitialised tables now carry data. Needs scientific review |
| Negative-Strickler | Output changes by design. Last, and separately reviewed |

## Suggested implementation series

Keep the changes reviewable and benchmark each one independently:

1. fix the `GG` allocation bound and the `OCABC` dummy declarations, so the
   remaining steps can be validated under bounds checking;
2. move `OCSIM` allocation to initialisation, keeping the clears, and add
   dimension-change cleanup — the bitwise-safe change, and probably the largest
   single gain;
3. remove redundant `OCSIM` clearing, with debug poisoning and regression
   coverage;
4. remove or active-size the topology and state adapters;
5. size solver matrices from active row widths and re-couple the `NXOC` guard to
   the allocated size;
6. fix the nitrate `cdi`/`cdit` indexing, then the repeated large initialisation,
   table sizing, and memory layout;
7. remove nitrate validation/output scans and other repeated setup;
8. replace the dated-input buffer and streamline optional output; and
9. resolve the negative-Strickler semantics — across both source trees — in a
   separate, explicitly numerical change.

Steps 2 and 3 are deliberately in this order. Earlier drafts put the clearing
first; allocating once is both the safer change and, because the per-step
allocations are large enough to be served by fresh memory mappings, likely the
more valuable one.

This sequence should recover most of the general V4.5.2-to-V4.5.3 performance
loss before touching model equations. Nitrate-enabled runs will remain slower
than V4.5.2 because they perform additional physical calculations, but their
overhead should then scale with active elements and cells rather than with
compile-time maxima or accidental quadratic work.
