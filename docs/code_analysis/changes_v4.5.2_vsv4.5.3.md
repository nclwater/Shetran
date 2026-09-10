# Code changes between V4.5.2 and V4.5.3

## Scope and method

This is a source-level comparison of Git tags `V4.5.2` and `V4.5.3`. No executable was built and no timings were collected. The comparison used both the ordinary diff and a whitespace-insensitive diff, because part of the tag range consists of formatting and documentation work.

- `V4.5.2`: `19e95ccf975d988fb413b434a9f6ef91fadc1f9d` (23 September 2022)
- `V4.5.3`: `937175f782b090b79ff109306f6aab7e56931dbf` (19 March 2026)
- Whole-tree diff: 127 files, 59,611 insertions, 745 deletions
- File distribution: 91 example files, 24 files under `src`, four files under `Linux`, four documentation files, and four top-level metadata/build files

The very large insertion count is therefore misleading as a measure of executable change. It is dominated by new examples, time series, binary documentation, and two 4,488-line variants of the nitrate module. The principal runtime-relevant changes are concentrated in `src/modules/OCmod.f90`, `src/modules/MNmod.f90`, `src/modules/CMmod.f90`, `src/modules/rest.f90`, `src/modules/FRmod.f90`, and several parameter modules.

The range also contains the history of a previously separate ZQ cleanup branch. Some commits have 2020 author dates even though they only became part of this tag-to-tag range through later merges.

Neither tag contains a build project: there is no `.vfproj`, no `.sln`, and no makefile. Compiler options are therefore not recoverable from the tags themselves, which matters for interpreting how V4.5.2 stored its large fixed-size local arrays (see section 1).

The 24 modified files under `src` are, in full:

| File | Nature of change |
|---|---|
| `modules/OCmod.f90` | `OCSIM` workspace conversion (section 1) |
| `modules/MNmod.f90`, `modules/MNmod - Copy.f90` | new nitrate component (section 2) |
| `modules/CMmod.f90` | nitrate hook into contaminant solve (section 2) |
| `modules/rest.f90` | dated meteorological input (section 3) |
| `modules/FRmod.f90` | output, error handling, active-size point lists (section 4) |
| `modules/OCQDQMOD.F90` | negative-Strickler surface storage (section 5) |
| `modules/ZQmod.f90`, `modules/OCmod2.f90` | ZQ cleanup and call-site update (section 5) |
| `modules/VSmod.f90` | initialiser call sites moved (section 5) |
| `modules/run_sim.f90` | console dates, contaminant array initialisation (section 5) |
| `modules/utilsmod.f90` | date-validation error path (section 5) |
| `modules/SYmod.f90` | active-size dummy declarations (section 1) |
| `parameters/sglobal.f90` | capacity constants (section 1) |
| `parameters/AL_C.F90`, `parameters/CONT_CC.F90`, `parameters/colm_cg.f90`, `parameters/colm_co.f90` | active-size conversions (section 1) |
| `parameters/AL_D.f90`, `parameters/is_cc.f90` | new `pslextra` unit, `isextrapsl` and `ISMN` flags |
| `util/mod_load_filedata.f90` | `ALINTP` active dimensions, larger buffers (section 1) |
| `visualisation/visualisation_metadata.f90` | closes and deletes parsed plan input (section 4) |
| `resource/Resource1.rc` | version string (section 6) |

## Executive overview

V4.5.3 combines several different ideas:

1. add a nitrate-cycle component and connect it to contaminant transport;
2. support dated meteorological input and date-bearing CSV output;
3. support additional water-table, sediment, and contaminant output;
4. allow materially larger catchments while converting selected fixed-size arrays to active-size allocatable arrays;
5. improve one-dimensional/no-outlet handling and make several input/error paths more robust;
6. clean up the reservoir/ZQ implementation and alter the treatment of negative-Strickler surface storage.

Most of those goals are either optional or initialization-only. One implementation detail is not: V4.5.3 changes the overland-flow workspace in `OCSIM` from fixed local arrays to local allocatable arrays, then allocates and clears the entire workspace on every model timestep. That change is treated in detail in the accompanying impact assessment.

## 1. Capacity and allocation changes

### Compile-time capacity increases

`src/parameters/sglobal.f90` changes the principal capacity constants as follows:

| Constant | V4.5.2 | V4.5.3 | Ratio |
|---|---:|---:|---:|
| `NXEE` | 250 | 1,000 | 4.0 |
| `NYEE` | 250 | 1,000 | 4.0 |
| `NLFEE` | 10,000 | 20,000 | 2.0 |
| `NELEE` | 30,000 | 250,000 | 8.33 |
| `NVEE` | 30,000 | 250,000 | 8.33 |
| `NXOCEE` | 500 | `4*NXEE` = 4,000 | 8.0 |
| `max_no_snowmelt_slugs` | 200 | 400 | 2.0 |

The purpose is to accommodate larger grids and more vegetation/meteorological categories. These constants still dimension many module arrays, so the executable's virtual/static memory footprint grows even where the active model is small.

### Useful active-size conversions

Several formerly maximum-capacity arrays become allocatable and are initialized once after the active dimensions are known:

- `AL_C.F90`: vertical connectivity, vertical geometry, soil-layer data, and root-density arrays;
- `ETmod.f90`: vegetation and evapotranspiration tables;
- `CONT_CC.F90`, `colm_cg.f90`, and `colm_co.f90`: contaminant state, overlap, and previous-step work arrays;
- `mod_load_filedata.f90`: `ALINTP` now exposes an active `(NEL, NCETOP)` result rather than `(NELEE, LLEE)`;
- `SYmod.f90`: selected dummy-array declarations use active `NEL` bounds.

These changes support the larger model limits without multiplying every large three-dimensional allocation by the new maxima. Their initialization and clearing occur once, so they are not a plausible cause of a whole-run factor-of-two-to-four regression by themselves.

### Problematic `OCSIM` conversion

In `src/modules/OCmod.f90`, the fixed local solver work arrays in `OCSIM` become local allocatables. On each call, V4.5.3:

- allocates topology adapters `ijedum` and `ijedum2` using the full `NELEE` and `NLFEE` capacities;
- allocates the block-solver matrices using `4*NX` and `NY`;
- allocates active element state adapters;
- explicitly fills every allocation with zero;
- lets all local allocatables be automatically deallocated on return.

`OCSIM` is called unconditionally once per main simulation timestep. This differs from the other active-size conversions, which allocate once during component initialization.

Two details of the V4.5.2 baseline are needed to read this change correctly.

First, the V4.5.2 declarations were very large: `EE(NXOCEE, NXOCEE, NYEE)` is `EE(500,500,250)`, i.e. 500 MB, plus roughly 12 MB for the other matrices. A local array of that size cannot live on a default stack, so V4.5.2 must have placed it in static storage — which is the default behaviour of Intel Fortran's `-auto-scalar` policy for fixed-size local arrays. In static storage the block is mapped once at load, zero-filled by the operating system, faulted in lazily and only for the regions the solver actually touches, and **retained between calls**. That is why V4.5.2 performed no per-call initialisation at all. Because the tags carry no build project, this should be confirmed against the build settings that were actually used; the conclusion does not depend on it, since V4.5.2 contains no initialisation statements either way.

A consequence worth stating plainly: for any model with `NX < 1000`, the V4.5.3 allocation is *smaller* than the V4.5.2 static block. V4.5.3 is not the more memory-hungry version. The regression is entirely per-timestep cost.

Second, not all four groups of arrays are equally new. `inhrf`, `GGGETHRF`, `inqsa` and `GGGETQSA` were declared `DIMENSION(total_no_elements)` in V4.5.2, and `total_no_elements` is a module *variable* (`sglobal.f90:86`), not a parameter. They were therefore already automatic arrays, allocated and deallocated on every call in V4.5.2. Converting them to `ALLOCATABLE` changed nothing; only their four `= 0.0d0` statements are new. The genuinely new per-call work is the block-solver matrices and the topology adapters.

### Defects introduced by the `OCSIM` conversion

The conversion also removed two safety margins that the fixed-size declarations had provided implicitly.

**`GG` is allocated one row short.** The row loop in `OCSIM` ends with an unguarded write:

```fortran
IF (IROW.NE.NROWL) THEN
    ee(1:nsv,1:ncr,irsv) = ...      ! guarded: EE only ever needs up to NROWL
ENDIF
gg(1:ncr,irsv) = JEMATMUL_VM(...)   ! not guarded; irsv = irow+1
```

On the final iteration `IROW = NROWL`, so this writes `GG(:, NROWL+1)`, and that column is read immediately after the loop by `DD(1:ncr,IROW) = GG(1:ncr,IRSV)`. But V4.5.3 allocates `GG(NX*4, NY)`. `OCIND` sets `NROWL` by `IF (ICOUNT.GT.0) NROWL = J` over `J = 1, NY`, so `NROWL = NY` whenever the last grid row contains any active element — the normal case for a catchment whose bounding box is the grid. V4.5.2 was safe only by accident, because `GG(NXOCEE, NYEE)` left spare rows whenever `NY < NYEE = 250`. `GG` needs a second dimension of `NY + 1`, matching the `NROWST(NY+1)` convention already used in `OCIND`.

**The `NXOC` capacity guard no longer guards the allocation.** `OCIND` computes the maximum active row width and validates it against `NXOCEE`:

```fortran
IF (NXOC.GT.NXOCEE) THEN
    CALL ERROR(FFFATAL, 1006, PPPRI, 0, 0, 'ARRAY DIMENSION OF NXOC TOO SMALL')
ENDIF
```

In V4.5.2 this was consistent: the arrays were `NXOCEE = 500` wide and the test used 500. In V4.5.3 the arrays are `4*NX` wide but the test still uses `NXOCEE = 4*NXEE = 4000`, so allocation and validation now measure different things. Counting `OCIND`'s inner loop, one grid column can contribute three elements at the west face, one grid square, and three at the south face when bank elements are enabled — up to `7*NX` per row, not `4*NX`. `4*NX` is provably safe only when `BEXBK` is false; it is generous for typical sparse channel networks, but nothing enforces it, and a model between the two bounds now overruns silently instead of failing cleanly. `NXOC` itself is a plain local in `OCIND` and is discarded on return; it used to live in `SPEC.OC`.

**`OCABC` dummy arguments over-declare their actuals.** `OCABC` declares `AA(NXOCEE)` and `CC(NXOCEE)` while `OCSIM` now passes `AA(:,IND)` and `CC(:,IND)` of extent `4*NX`. Whenever `NX < 1000` the dummy claims more storage than the actual has. No overrun occurs in practice, because `OCABC` touches only `AA(1:NSV)` and `CC(1:NPR)`, but the declaration is invalid and will trip `-check bounds` immediately — which matters because bounds checking is the natural way to validate any later change to this workspace.

## 2. Nitrate component

V4.5.3 adds `src/modules/MNmod.f90` and a near-duplicate `src/modules/MNmod - Copy.f90`. The latter retains maximum-capacity dummy-array declarations in places where `MNmod.f90` uses active dimensions, and it enables hard-coded output for nine cells that is commented out in `MNmod.f90`. Both define the same module and procedures, so the extra file is a build/maintenance hazard if source discovery is based on wildcards; which variant is compiled can also change array-bound behaviour and output.

The nitrate implementation adds:

- ammonium, nitrate, litter, manure, humus, plant-uptake, mineralisation, immobilisation, nitrification, denitrification, volatilisation, temperature, pH, and moisture calculations;
- persistent two-dimensional state arrays allocated using active element and cell counts;
- eight new nitrate-related input/output units, 53 through 60;
- `ISMN` as the runtime feature flag;
- two contaminant source/sink arrays, `SSS1` and `SSS2`;
- nitrate-specific initialization, validation, time-varying input, balance accumulation, and output.

`CMmod.f90` calls `MNCONT` from `CMSIM` when `ISMN` is true. The resulting `SSS1`/`SSS2` values replace the normal plant source/sink terms in the contaminant column solve. For the first contaminant, the surface additions are disabled in `CMmod` because they are handled by the nitrate component.

The nitrate work is intentionally substantial. Its runtime effect is confined to contaminant simulations with nitrate enabled, but within that scope it runs numerous full active-cell passes and several nonlinear loops allowing up to 20 iterations per cell.

### Nitrate defects noted during inspection

`mnplant`'s initialisation block reads the canopy density index tables with the wrong store index:

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

The loop index is `i` but the store index is `nv`, so every vegetation type's table is written into row `nv`, overwriting the previous one. The data is read back as `cdi(jplty,i)` and `cdit(jplty,i)`, so for every plant type except `jplty == nv` the interpolation reads a `save`d array that was never assigned. This is a plain typo, but it silently affects plant uptake for all but one vegetation type.

The new module also reuses `NPLTEE = NVEE = 250000` where the active `NV` is meant. `cdi` and `cdit` are declared `(npltee, nvalee)` = `(250000, 30)`, i.e. 60 MB each as `save`d locals, for tables whose real extent is `(NV, 30)`. The `NVEE` increase from 30,000 inflated these 8.3 times. The same confusion produces the `claimx(1:npltee) = 2` sweep discussed in the impact assessment.

## 3. Dated meteorological input

`ETmod.f90` adds the `BMETDATES` flag. `FRmod.f90` reads this optional fourth logical from ET input, defaulting it to false for backward compatibility.

When enabled, `rest.f90` now:

- reads ISO-like year/month/day/hour/minute/second fields from precipitation, potential-evaporation, and optional maximum/minimum-temperature records;
- validates that data start early enough for the simulation;
- skips records before the requested simulation start;
- converts dates with `HOUR_FROM_DATE`;
- reads the remainder of each record through an internal character buffer.

The undated path remains available. Some explicit loops over meteorological stations are replaced by array-slice expressions.

The implementation declares `CHARACTER(LEN=1000000) :: tmp` in `METIN`. A dated record is read into this one-million-character buffer and then parsed by an internal formatted read. This is runtime-relevant only for dated inputs, but it is unnecessarily expensive for ordinary records.

## 4. Output changes

### Extra water-table output

Commits `c4d2eae` and `b0be2b7` add an optional rundata entry and water-table-depth output. The final V4.5.3 form writes selected element depths to one CSV at the daily mass-balance interval. When the feature is absent, normal runs take only the flag checks.

### Date-bearing and component output

`FRmod.f90` adds or changes:

- ISO-style dates in outlet discharge output;
- dynamic allocation for extra discharge and water-table point lists;
- regular sediment output for all and fine sediment when sediment is enabled;
- regular contaminant-1 output when contaminants are enabled;
- more detailed I/O error handling;
- start and end dates in console output;
- safe zero discharge/concentration handling for one-dimensional simulations with no outlet.

The every-timestep outlet-discharge file already existed in V4.5.2 and already performed `DATE_FROM_HOUR`; changing its string format is not a new per-step category of work. The sediment and contaminant accumulation/output paths are new and conditional on those components.

`visualisation_metadata.f90` now closes and deletes the parsed visualization-plan input. This is not in a timestep loop.

## 5. Hydrological and numerical changes

### Negative-Strickler surface storage

`src/modules/OCQDQMOD.F90` changes both relevant surface-flow branches. In V4.5.2, a negative Strickler value both selected special roughness and lowered the effective surface elevation by the magnitude of the configured storage depth:

```fortran
zi(j) = GETHRF(KEL) + strxx(kel)/1000     ! strxx < 0, so this lowers the head
```

In V4.5.3, the elevation adjustment is commented out while the roughness switching remains.

Note that the commented-out line reads `0.95*strxx(kel)/1000`, not the V4.5.2 `strxx(kel)/1000`. Someone was mid-experiment with a 0.95 factor when the line was disabled, and the 0.95 variant is the one left *active* in the separate `Linux/src` tree. There are therefore three candidate behaviours in circulation under this tag range, not two: full adjustment (V4.5.2 `src`), no adjustment (V4.5.3 `src`), and 0.95 adjustment (V4.5.3 `Linux/src`).

This is a numerical/physical behavior change rather than a direct increase in instruction count, but its direction is predictable rather than merely uncertain. In V4.5.2 the routed head fell smoothly to zero as depth approached the storage threshold, so the configured depth behaved as dead storage; in V4.5.3 flow is driven by the full head including that storage. The expected result is larger overland flows and steeper gradients at shallow depths, hence more flow-correction retries and *more* accepted timesteps. It therefore needs separate output and timestep-count comparison even though it is not the primary per-step hotspot.

### Reservoir/ZQ cleanup

The ZQ branch changes include:

- renaming the lookup subroutine to the function `get_ZQTable_value`;
- moving local parsing variables out of module state;
- use of kind parameters and `newunit`;
- closing the ZQ log explicitly;
- documentation and visibility cleanup;
- a small change to the negative-Strickler elevation factor in the separate `Linux/src` tree.

The lookup algorithm remains a linear scan of the selected ZQ table. These changes affect reservoir boundary calls only and do not explain a general slowdown.

### Moved initialisation in `VSmod.f90`

`VSmod.f90` moves `CALL INITIALISE_VSMOD()` and `CALL INITIALISE_AL_C()` from after `VSCONC` (now commented out around `VSmod.f90:2058`) to the end of the loop-1200 block at `VSmod.f90:1133`, and adds a new `INITIALISE_AL_C2()` at `VSmod.f90:2012`.

This is not a neutral refactor. Both routines allocate **and zero** their arrays (`AL_C.F90:204-234`), so moving a zeroing initialiser earlier changes program state: anything written to `QVSH`, `QVSV`, `JVSACN`, `JVSDEL` and the associated geometry between the old and new call sites was previously wiped and now survives. That may be a deliberate fix or it may resurrect stale values that the old ordering masked, but either way it alters the initial state handed to the VSS component. It consequently sits upstream of any "is the timestep sequence identical?" comparison between the tags, and is a candidate explanation alongside the negative-Strickler change if the `UZNEXT` sequences differ.

### Interactive pauses on error paths

Several error paths added in this range now block on standard input before stopping. `utilsmod.f90:249-255` gives `HOUR_FROM_DATE`'s date-validation failure this treatment, and `FRmod.f90` adds the same pattern to a number of file-open and write failures:

```fortran
write(*,'(''paused, type [enter] to continue'')')
read (*,*)
stop
```

This is helpful when a user double-clicks the executable, but in a batch, scheduled or HPC run these hang indefinitely rather than exiting. Anyone timing these versions should rule this out first: a run that appears extremely slow may in fact be a process blocked on stdin.

### Contaminant array lifecycle in `run_sim.f90`

`run_sim.f90` now calls `initialise_cont_cc()`, `initialise_colm_cg()` and `initialise_colm_co()` when contaminants are active, and calls `deallocate_colm_cg()` after the first contaminant step. The deallocation frees `JKZCOL`, `JOLFN`, `NOL` and `NOLCE`. This was checked and is correct: those four are referenced only in `FRmod`'s `INCM` initialisation and in comment headers within `CMmod`, while the per-step `COLMSM` path uses only `NOLBT` and `NOLCEA`, which are retained. Recorded here so it does not need re-deriving.

### Other robustness changes

- `FRmod.f90` guards outlet accesses when a one-dimensional model has no outlet.
- `FRmod.f90` forces an undefined `NSOBED` to soil type 1 when sediment and solute are combined.
- input-header and filename buffers are enlarged in `mod_load_filedata.f90`.
- selected numeric output fields widen from `G12.6` to `G14.6`.
- `run_sim.f90` prints simulation start and end dates to the console before the main loop.

## 6. Examples, documentation, and packaging

The tag adds 91 example files, principally:

- dated and simple Aire at Kildwick Bridge examples;
- a Cobres extra-water-table-output example;
- one- and three-dimensional Slapton nitrate examples.

It also adds legacy Word/PDF documentation, updates the changelog and compiling notes, ignores `.mod` files, and changes the Windows resource version from 4.5.2 to 4.5.3. None of these changes affects simulation runtime.

## 7. Source-tree caveat

The tag contains both `src` and `Linux/src`, but they are not equivalent. The main `src` tree receives the nitrate, date, active-allocation, and output changes. The `Linux/src` tree receives only selected ZQ, storage, and constant updates. Any runtime comparison must record which tree and compiler project supplied the executable; otherwise the label “V4.5.3” does not uniquely identify the executable code path.

The sharpest instance is the negative-Strickler head adjustment described in section 5: it is **off** in `src` and **on with a 0.95 factor** in `Linux/src`. That is not a packaging difference but two different hydrological models under one tag, so the divergence should be resolved before performance work rather than after. Otherwise half of any A/B result will not carry between platforms.

## Reproducible inspection commands

```sh
git diff --stat V4.5.2..V4.5.3
git diff --ignore-all-space V4.5.2..V4.5.3 -- src
git log --reverse --oneline V4.5.2..V4.5.3
git show V4.5.3:src/modules/OCmod.f90
git show V4.5.3:src/modules/MNmod.f90
```
