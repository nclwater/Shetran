# Runtime impact assessment: V4.5.2 versus V4.5.3

## Conclusion

The strongest code-level explanation for the general V4.5.3 slowdown is the new workspace management in `OCSIM`.

V4.5.3 allocates, clears, uses, and automatically deallocates a large overland-flow workspace once per simulation timestep. The largest clear scales as `NX**2*NY` (cubic when both horizontal dimensions grow together), and additional topology arrays are sized by the newly enlarged compile-time capacities rather than by the active catchment. This work is unconditional and is consistent with a catchment-dependent slowdown of roughly two to four times.

The corresponding V4.5.2 arrays were fixed-size locals held in static storage, mapped once and retained between calls, so V4.5.2 paid nothing per timestep. Because the V4.5.3 allocations are large enough to be served by fresh memory mappings, the per-step cost is dominated by page faulting rather than by memory bandwidth, which makes it materially larger than a byte count suggests.

For nitrate-enabled simulations there is a second, intentional source of substantial work: the new nitrate model. Its current implementation also contains avoidable full-grid validation, output aggregation, and poor memory traversal on every step, plus a quadratic initialisation loop that runs once at startup.

A third mechanism acts through the number of timesteps rather than the cost of each: the negative-Strickler change removes a dead-storage head reduction, which should increase flows at shallow depths and hence the number of accepted steps.

Dated meteorological input and new component outputs add smaller conditional costs.

This is a logical assessment, not a measured attribution. A compiler/runtime profile is still required to quantify percentages — though several of the competing hypotheses can be separated without rebuilding at all, as described under "Diagnostics that need no rebuild".

## Where the new cost enters the timestep

The main loop in `run_sim.f90` has this effective structure:

```text
each accepted timestep
  TMSTEP
  ETSIM
  VSSIM
  OCSIM                         always
    allocate all solver work   new in V4.5.3
    clear all solver work      new in V4.5.3
    solve overland flow
    copy/fix surface state
    automatic deallocation     new in V4.5.3
  SYMAIN                        if sediment is active
  CMSIM                         if contaminants are active
    MNCONT                      if nitrate is active; new in V4.5.3
  balances and output
```

Total runtime can be considered as:

```text
runtime = number_of_timesteps * cost_per_timestep + initialization + output
```

V4.5.3 can increase both terms: `OCSIM` and optional nitrate increase cost per timestep, while the negative-Strickler behavior change may change the number of timesteps through altered numerical behavior.

## Ranked findings

| Rank | Change | Scope | Expected impact | Confidence |
|---:|---|---|---|---|
| 1 | Per-timestep allocation and whole-array clearing in `OCSIM` | All simulations | Very high | High |
| 2 | Full-capacity `ijedum`/`ijedum2` work after `NELEE`/`NLFEE` increases | All simulations | Medium to high, especially for small/medium models | High |
| 3 | New nitrate process kernels and iterations | Nitrate simulations | High, partly inherent in the new physics | High |
| 4 | Nitrate validation and balance/output scans on every step | Nitrate simulations | Medium to high and avoidable | High |
| 4a | Nitrate quadratic/capacity-sized initialisation in `mnplant` | Nitrate simulations | High but **one-time**, at first nitrate step only | High |
| 5 | One-million-character dated-met record buffer and internal I/O | `BMETDATES=.TRUE.` | Medium when met records are frequent | Medium |
| 6 | New sediment, contaminant, and water-table output | Corresponding options | Low to medium; output-frequency dependent | High |
| 7 | Changed negative-Strickler head treatment | Models using negative Strickler | Indirect; could alter timestep/retry count | Medium |
| 8 | One-time active-size allocations and initialization | Startup/component activation | Low for long runs | High |
| 9 | ZQ cleanup, format widths, examples, docs | Limited or non-executable | Negligible general impact | High |

Two items fall outside this ranking because they are not performance findings, but they must be handled before or alongside the work above:

- the `GG` allocation defect, the decoupled `NXOC` guard, and the `OCABC` dummy declarations (section 1) are correctness problems in the very code being optimised, and two of the three will fire under the bounds checking that the optimisation work needs;
- the interactive `read (*,*)` pauses added to error paths in `utilsmod.f90` and `FRmod.f90` block on standard input in batch runs. A run that appears pathologically slow may be a hung process, so this should be ruled out before any timing is collected.

## 1. Primary hotspot: `OCSIM` workspace churn

### What changed

In V4.5.2, `OCSIM` declares fixed-size local work arrays. There are no source-level `ALLOCATE` statements and no blanket zeroing of these arrays at subroutine entry.

Those declarations were enormous — `EE(NXOCEE, NXOCEE, NYEE)` is `EE(500,500,250)`, i.e. 500 MB, plus about 12 MB for the other matrices — and cannot have been on a default stack. With Intel Fortran's default `-auto-scalar` policy, fixed-size local arrays go to static storage, so in V4.5.2 this workspace was `.bss`: mapped once at load, zero-filled by the operating system, faulted in lazily and only over the regions the solver actually touched, and retained across calls. This is the mechanism by which V4.5.2 paid nothing per timestep. Because the tags contain no build project, it should be confirmed against the build settings actually used, but the conclusion is unaffected either way: V4.5.2 contains no initialisation statements at all.

One implication is worth stating so it is not used as an argument in the wrong direction: for any model with `NX < 1000`, the V4.5.3 *allocation* is smaller than the V4.5.2 static block. V4.5.3 is not the more memory-hungry version, and the case for changing it rests entirely on per-timestep cost.

A second qualification: of the four groups listed below, `inhrf`, `GGGETHRF`, `inqsa` and `GGGETQSA` were already automatic arrays in V4.5.2, because they were declared `DIMENSION(total_no_elements)` and `total_no_elements` is a module variable (`sglobal.f90:86`), not a parameter. They were allocated and deallocated on every call in V4.5.2 too. Only their four `= 0.0d0` statements are new, which makes them the least significant of the four groups.

In V4.5.3, `OCSIM` declares the same work as local allocatables and executes all of the following on every call:

```fortran
allocate(ijedum(...), ijedum2(...))
ijedum = 0
ijedum2 = 0

allocate(AA(...), DD(...), ...)
...
AA = 0.0d0
DD = 0.0d0
...
EE = 0.0d0
...

allocate(inhrf(...), GGGETHRF(...), inqsa(...), GGGETQSA(...))
inhrf = 0.0d0
...
```

Because these are unsaved local allocatables, Fortran automatically deallocates them when `OCSIM` returns. The next timestep repeats the allocation and first-touch work.

### Amount of forced clearing

Let `NX` and `NY` be the active grid dimensions and `NEL` the active element count. The explicit double-precision clears write approximately:

```text
16 * NX^2 * (NY + 5) + 8 * NX * NY + 12 * NX + 10 * NEL
```

double values per timestep.

The terms include:

- `EE(4*NX, 4*NX, NY)`: `16*NX^2*NY` doubles;
- five square matrices (`AA`, `BB`, `CC`, `TM1`, `TM2`): `80*NX^2` doubles;
- `DD` and `GG`: `8*NX*NY` doubles;
- three vectors: `12*NX` doubles;
- four active state adapters: `10*NEL` doubles.

Illustrative write volumes for the double arrays alone are:

| Hypothetical `NX × NY` | Assumed `NEL` | Double values cleared per step | Bytes written per step |
|---:|---:|---:|---:|
| 20 × 20 | 400 | 167,440 | 1.34 MB |
| 50 × 50 | 2,500 | 2,245,600 | 17.96 MB |
| 100 × 100 | 10,000 | 16,981,200 | 135.85 MB |
| 200 × 200 | 40,000 | 131,922,400 | 1.06 GB |

These are lower-bound traffic figures. They omit allocator work, deallocation, page management, the integer arrays, source reads, later solver reads/writes, and cache eviction of useful state.

### Why the byte count understates the cost

Treating this as a memory-bandwidth problem substantially understates it, and the understatement is what determines the right order of fixes.

Allocations of this size exceed the allocator's mmap threshold, so on every timestep the workspace is handed back as **freshly mapped, unfaulted** pages. The subsequent `EE = 0.0d0` therefore costs a minor page fault plus a kernel page-zero for every 4 KB touched — roughly an order of magnitude more than a `memset` over memory that is already resident. The matching `deallocate` then unmaps the region and returns it to the operating system, so the next timestep repeats the entire cycle. On top of that, each step's workspace lands at a different address, so nothing stays warm in cache or TLB between steps.

This means the two P0 remedies are not interchangeable, and the safer one is also the more valuable one:

1. **Allocating the workspace once, with the clears retained,** removes the mmap/munmap cycle and the page-fault storm outright, leaving only a plain `memset` over resident memory. It is **bitwise identical by construction**, because the arrays are still fully zeroed on entry, so it can be accepted on a timing result alone with no numerical argument.
2. **Removing the clears** is the change that requires proving every read is preceded by a write. It should carry that risk on its own, after the safe win is already banked.

This also explains why the post-tag commit `0feebe7`, which moves the allocatables to module scope but leaves the clears in place, was effective despite not addressing the clearing at all.

### Why the clears are redundant

The historical comments immediately above `OCABC` state that only useful parts of `AA`, `BB`, `CC`, and `FF` are initialized there rather than in `OCSIM`. The code still does that:

- `AA(1:NSV)` is cleared for each active column;
- `BB(1:NCR)` is cleared;
- `CC(1:NPR)` is cleared;
- `FF` is assigned;
- active slices of `TM1`, `TM2`, `TV1`, `TV2`, `EE`, `GG`, and `DD` are assigned before they are read.

The V4.5.3 whole-array assignments therefore undo an earlier deliberate optimization and touch large unused regions. V4.5.2 demonstrates that those blanket clears are not required by the algorithm.

### Why the observed factor is plausible

The overland-flow solver already works with dense row-block matrices. V4.5.3 adds a large streaming memory pass immediately before the useful solver work, evicting data and consuming memory bandwidth. The allocation dimensions use the conservative `4*NX` row bound even when actual row widths are much smaller. The overhead consequently varies strongly with catchment shape and grid dimensions, which matches a reported slowdown that varies between models rather than being a fixed offset.

### Correctness defects in the same code

Two of the safety margins that the fixed-size declarations used to provide were removed along with them. These are documented in the changes note; they are repeated here because they bear on how this workspace can be measured and modified.

- **`GG` is allocated one row short.** The row loop writes `gg(:,irsv)` unguarded at `irow == NROWL`, and that column is genuinely consumed immediately afterwards, but `GG` is allocated `(4*NX, NY)` while `NROWL = NY` in the normal case. V4.5.2 was safe only through the spare rows in `GG(NXOCEE, NYEE)`. This is undefined behaviour that is present today, and it should be fixed before any V4.5.3 instability is attributed to the physics changes.
- **The `NXOC` guard tests `NXOCEE = 4000` while the arrays are `4*NX` wide.** Allocation and validation have decoupled, and `4*NX` is provably safe only when bank elements are disabled (`OCIND` can produce up to `7*NX` elements in a row otherwise).
- **`OCABC` declares `AA(NXOCEE)` and `CC(NXOCEE)` for actuals of extent `4*NX`.** No overrun occurs in practice, but this and the `GG` defect will both fire under `-check bounds`, so they must be fixed before bounds checking can be used to validate removal of the clears.

## 2. Enlarged full-capacity topology adapters

`OCSIM` repacks connectivity into `ijedum` and `ijedum2` every timestep. The loops use whole first-dimension slices, so their work is based on capacity rather than active element/link counts.

The number of integer values copied is:

```text
8 * NELEE + 6 * NLFEE
```

With the tag constants:

- V4.5.2 copies about 1.20 MB of integer destinations per timestep;
- V4.5.3 copies about 8.48 MB per timestep;
- V4.5.3 first writes another 8.48 MB by zeroing the same arrays.

The copy itself existed in V4.5.2, but the capacity increase makes it about seven times larger in byte volume, and the pre-zero is new. This is especially important for small models: the topology-adapter cost remains tied to the 250,000-element executable capacity even if only a few hundred or thousand elements are active.

The zeroing is certainly redundant because every plane of both arrays is immediately overwritten. The capacity-sized repacking is also unnecessary because `OCFIX` loops only over the active `NEL` and active confluence references.

## 3. Nitrate runtime cost

Nitrate is gated by `ISMN`, so it cannot explain a slowdown in a run where the nitrate rundata entry is absent. Where enabled, a large increase over V4.5.2 is expected because V4.5.2 did not perform this physics at all.

### Inherent work

Each nitrate contaminant step invokes multiple active-cell kernels for:

- plant growth and uptake;
- temperature, moisture, pH, and denitrification factors;
- manure, litter, humus, ammonium, and nitrate pools;
- mineralisation, immobilisation, nitrification, volatilisation, and source/sink terms.

Four pool routines contain nonlinear cell loops with a maximum of 20 iterations and a squared relative tolerance of `1e-12`. This cost is scientifically motivated, though convergence behavior should be profiled.

### Avoidable work

Several implementation details add work without changing the process equations:

- `mnerr3` copies eleven complete `(NEL, NCETOP)` fields through `dummy4` and scans them on every nitrate step;
- `mnerr4` performs another time-varying input validation pass;
- `mnout` first updates 15 per-cell cumulative arrays, then performs another full active-cell balance summation on every step even though it writes only every 24 hours;
- most internal state arrays are `(NEL, NCETOP)` while loops vary `NCL` in the inner loop, producing stride-`NEL` access in Fortran column-major storage;
- `MNCONT` rewrites all `TA(1:NV)` values to 10 on every call.

For nitrate models, these are credible additional contributors beyond the unavoidable new equations.

### One-time startup cost, distinct from per-step cost

`mnplant`'s initialisation block contains two capacity-by-active-element loops:

- `DO I=1,NEL; NPLTYP(I,2)=1` inside an outer `DO NELM=NLF+1,NEL`, making that assignment quadratic in `NEL`;
- the same outer loop repeatedly assigning `CLAIMX(1:NPLTEE)=2`; with `NPLTEE=NVEE=250000` this is the larger of the two.

Together these are roughly `(NEL-NLF) * (NPLTEE + NEL)` assignments, about 2.6 × 10⁹ for `NEL = 10000`.

However, the whole block is guarded by `if (pass==1)` (`MNmod.f90:3577`), so it executes exactly once. It presents as a one-off stall before the first nitrate step, not as a per-step slope. That distinction matters for attribution: a nitrate run that is slow from the first step onwards has a different cause from one that takes a minute to start. Timing should record the two separately rather than reporting a single ratio.

### A correctness defect that precedes optimisation

`mnplant` reads the canopy density index tables with the wrong store index — `cdi(nv,ntb)` and `cdit(nv,ntb)` inside a `do i = 1,nv` loop — while reading them back as `cdi(jplty,i)`. Every vegetation type except `jplty == nv` therefore interpolates from a `save`d array that was never assigned. Profiling or optimising this kernel before fixing the typo would characterise code that nobody intends to keep.

## 4. Dated meteorological input

The new dated path is optional. The undated path does not read or parse the large temporary buffer.

When `BMETDATES` is true, each applicable meteorological record is read with an `A` edit descriptor into `CHARACTER(LEN=1000000) :: tmp`, followed by an internal list-directed read. Assigning a shorter record to a fixed-length Fortran character value blank-pads the remainder, so the implementation can touch roughly one megabyte per record before parsing the actual values. Potential evaporation plus optional high/low temperature can repeat this three times at a meteorological boundary.

This is unlikely to explain a uniform all-model slowdown, but it can be visible for dated examples with short meteorological intervals or relatively cheap hydrological timesteps.

## 5. Output additions

The regular outlet-discharge write on every timestep predates V4.5.3. V4.5.3 changes its date format but does not introduce that write.

New conditional work includes:

- a loop across sediment fractions on every sediment timestep;
- sediment and contaminant aggregation and regular CSV writes;
- selected water-table-depth writes at the daily mass-balance interval;
- extra date formatting at regular output boundaries.

This work should scale with enabled components, number of selected points, and `TOUTPUT`. It is secondary to `OCSIM` for ordinary runs, but output-heavy configurations should still be timed separately.

There is also a correctness issue in the V4.5.3 sediment output path: `sedav` is set to zero during the separate `FROUTPUT('start')` invocation, is not declared `SAVE`, and is then read before assignment while accumulating fractions in `FROUTPUT('main')`. Its value is therefore undefined by the Fortran standard; with storage-retaining compiler behaviour it grows across timesteps. Fixing this does not materially affect speed but should accompany output work.

## 6. Numerical behavior and timestep count

The negative-Strickler branches in `OCQDQMOD.F90` no longer lower `ZI` by the configured storage depth after the storage threshold is exceeded. This changes the head used for flow calculations while retaining the roughness switch.

The direct operation count is slightly lower, not higher. Nevertheless, changed flow magnitudes can affect:

- VSS error flags used by `TMSTEP` to reduce the next timestep;
- occurrence of OC flow correction warnings;
- stability and the number of accepted timesteps;
- the sequence of component/output calls.

The direction is predictable, and stating it gives the diagnostic something to test against. In V4.5.2, `zi(j) = GETHRF(KEL) + strxx(kel)/1000` with `strxx < 0` lowered the driving head by the storage depth, so routed flow decayed smoothly to zero as depth approached the storage threshold — the configured depth behaved as dead storage. V4.5.3 drives flow with the full head including that storage. The expectation is therefore **larger overland flows and steeper gradients at shallow depths, more flow-correction retries, and more accepted timesteps**, not fewer.

This makes the change a second independent mechanism for a slowdown, acting through the `NSTEP` term rather than through cost per step. It is not merely a physics question to be parked; it belongs in the same experiment as `OCSIM`, precisely so the two can be separated.

The first diagnostic remains to compare final `NSTEP` between the two versions for the same input. Equal `NSTEP` points to per-step cost; a materially larger V4.5.3 `NSTEP` shows that numerical behavior is also involved, and the prediction above is what a larger `NSTEP` should be checked against.

Note that `VSmod.f90`'s relocation of `INITIALISE_AL_C()` (which zeroes as well as allocates) is a second candidate cause if the `UZNEXT` sequences differ. The two should not be conflated.

## 7. Changes unlikely to cause the reported slowdown

The following changes are one-time, inactive by default, or computationally negligible relative to the timestep solvers:

- active-size initialization of ET, VS geometry, and contaminant arrays;
- reducing `ALINTP` to active dimensions;
- the ZQ subroutine-to-function refactor;
- the max snowmelt-slug capacity increase, unless snowmelt code explicitly scans the maximum rather than `NSMC`;
- wider output formats and improved error messages;
- version resources, examples, and documentation.

## Post-tag evidence

Current `master` contains commit `0feebe7` (“made some arrays in hot loops module level allocated ones”), which moves the `OCSIM` allocatables to module scope and allocates them once from `OCINI`. This independently supports the identification of repeated allocation as a hotspot, and it is the same step recommended as the first P0 change above — which is consistent with the page-fault account, since eliminating the per-step mapping is where most of the benefit lies even with the clears left in.

That later change is only a partial remedy: current `OCSIM` still clears every full work array on every call, still uses `4*NX` rather than the actual maximum row width, and still clears/copies topology arrays sized by `NELEE`/`NLFEE`. It also inherits the `GG` bound defect, since moving the allocation does not change its shape. The optimization proposal therefore goes beyond simply adopting `0feebe7`.

## Diagnostics that need no rebuild

These tags predate the CMake build setup, so rebuilding them is itself a project. Several of the competing hypotheses can nevertheless be discriminated using the **existing executables and existing run logs**, and this is worth doing before investing in a legacy build environment.

**Pad the grid.** `NXEE` is now 1000, so a model can be re-gridded with extra inactive columns and rows without recompiling anything. The `OCSIM` clear cost scales as `NX**2 * NY`, whereas the useful solver work scales with the active channel network. If runtime rises roughly quadratically in `NX` when only inactive padding is added, that confirms the block-solver matrices. The same padding should be close to free under V4.5.2, where the arrays were fixed-size and only touched regions cost anything.

**Run a trivially small model for many timesteps.** At `NEL` of a few tens the matrix clears are negligible and per-step cost is dominated by the capacity-sized topology adapters. Predicted adapter traffic is about 1.20 MB per step for V4.5.2 (copy only) against about 17 MB for V4.5.3 (8.48 MB zero plus 8.48 MB copy). A tiny model should therefore show a *large* V4.5.3/V4.5.2 ratio if `ijedum` dominates and only a small one if the `NX**2 * NY` matrices dominate. Combined with the padding test, this separates finding 1 from finding 2 without any instrumentation.

**Correlate existing slowdown factors.** If per-model slowdown factors have already been collected across several catchments, plotting them against `NX**2 * NY` (matrix hypothesis), against a constant (adapter hypothesis), and against the `NSTEP` ratio (physics hypothesis) discriminates all three from data already in hand.

**Compare virtual size against resident size.** V4.5.2 should show a very large virtual size with a modest resident set, because most of the 500 MB static workspace is never touched. V4.5.3 should show a smaller virtual size but a resident set reflecting the whole workspace, because the clears write every page every step. `/usr/bin/time -v`, or the working set in Task Manager, is sufficient. This tests the static-storage account and the page-fault account at once, and needs only that both executables run.

**Read `NSTEP` from existing summaries.** `run_sim.f90` already prints the step count on normal completion. If logs exist for both versions on the same input, the per-step-versus-more-steps question may already be answered.

## Suggested confirmation sequence when comparable builds are available

1. Rule out a process blocked on an interactive error-path pause before trusting any wall-clock figure.
2. Compare `NSTEP` and the sequence of accepted `UZNEXT` values for identical inputs.
3. Time `TMSTEP`, `ETSIM`, `VSSIM`, `OCSIM`, `SYMAIN`, `CMSIM`, and output separately with a monotonic clock.
4. In `OCSIM`, separately time workspace preparation, `OCQDQ`, the row solve, and `OCFIX`.
5. A/B test these minimal changes in order:
   - fix the `GG` bound and the `OCABC` dummy declarations, so that later steps can be validated under bounds checking;
   - allocate the workspace once, keeping the clears — bitwise identical by construction, and the step that removes the mmap and page-fault cycle;
   - remove the redundant full-array clears;
   - restrict topology adapters to active sizes or remove them;
   - allocate by actual maximum row width, and make the `NXOC` guard test that same value.
6. For nitrate runs, time the one-time `mnplant` initialisation separately from the per-step kernels, then `mnerr3`, `mnerr4`, and `mnout`.
7. Compare scientific outputs after every A/B step, not just total runtime.

Note that step 5 reverses the order given in earlier drafts. Allocating once is both the safer change — it cannot alter results, since the arrays are still fully zeroed — and, because of the page-fault mechanism described in section 1, probably the larger share of the win.

The expected signature of the primary diagnosis is a large reduction in `OCSIM` time with unchanged `NSTEP` and numerically identical solver outputs.
