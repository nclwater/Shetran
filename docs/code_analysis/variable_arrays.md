# Replacing hard-coded array dimensions with allocatable storage

Status: source-audited analysis and implementation plan. No Fortran code changed yet.
Scope: the 20 array-dimension and sizing declarations in
[`src/parameters/sglobal.f90:117-137`](../../src/parameters/sglobal.f90).
Baseline commit: `08317e0` (master), version 4.7 development.

---

## 1. Why

Most of these parameters are compile-time capacities rather than problem sizes.
`NXSCEE` is instead a numerical-resolution setting, `NOLEE` is a derived alias,
and `NSETEE` belongs to a dead legacy path; they require different treatment.
Every module-level array dimensioned by a capacity is allocated in static storage
at program start, regardless of catchment size. Measured on the current release
build:

```
$ size build/release/bin/shetran
   text     data      bss      dec
5272391  4080936  1335481544  1344834871
```

**1.24 GiB (1.34 GB) of `.bss`** is reserved before the first input record is
read. Broken down by owning module (`nm -S`, aggregated):

| Static bytes | Owner | Largest contributors |
|-------------:|:------|:---------------------|
| 493.7 MiB | procedure-scope statics | `VSSIM` locals `CQ`, `VSPSIN`, `VSTHEN` — `(LLEE,NELEE)` = 95.4 MiB each ([`VSmod.f90:4243-4245`](../../src/modules/VSmod.f90)); `MNPLANT` `CDI`/`CDIT` — `(NPLTEE,30)` = 57.2 MiB each; `VSCONC` `LRENUM(NELEE,NLYREE)` = 19.1 MiB |
| 208.9 MiB | `plant_cc` | `PDZF3(NELEE,NPELEE,LLEE)` = 190.7 MiB alone |
| 198.1 MiB | `vsmod` | `JVSALN(NELEE,NLYREE,4)` = 76.3 MiB; `ICSOILSV`, `IVSSTO` `(LLEE,NELEE)` = 47.7 MiB each |
| 91.5 MiB | `sed_cs` | `QSED(NELEE,NSEDEE,4)` = 53.4 MiB; `FDEL`, `FBETA` `(NELEE,NSEDEE)` = 13.4 MiB each |
| 61.5 MiB | `al_d` | 22 `NELEE` arrays, 8 `NVEE` arrays |
| 57.8 MiB | `al_c` | 26 `NELEE` arrays, `DHF`/`QOC` `(NELEE,4)` = 7.6 MiB each |
| 41.5 MiB | `cont_cc` | `RSW`, `RSWC`, `RSWT`, `CCCCW`, `CCAPR` `(NELEE,NCONEE)` = 5.7 MiB each |
| 32.7 MiB | `sed_co` | `FBETAO`, `FDELO` `(NELEE,NSEDEE)` = 13.4 MiB each |
| 15.8 MiB | `symod` | |
| 15.3 MiB | `al_g` | `ICMREF(NELEE,12)` = 11.4 MiB |
| 11.5 / 11.1 / 9.5 MiB | `etmod` / `ocmod` / `ocmod2` | |
| 7.6 MiB | `sglobal` | `cellarea`, `DXQQ`, `DYQQ`, `ZGRUND` `(NELEE)` = 1.9 MiB each |

The release build enables IPO/LTO ([`CMakeLists.txt:87-96`](../../CMakeLists.txt)),
so module arrays with no surviving reference are already absent from the binary
(`NGRID`, `BCPAA`/`BCPBB`, and the whole `IOCORS`/`IODATA`/… result family have
no symbol). The figures above are therefore the *referenced* static footprint,
and deleting an unreferenced array does not reduce them.

A typical catchment uses a few thousand elements, not 250 000. The static
footprint is therefore roughly two to three orders of magnitude larger than the
working set, which:

* forces `mcmodel=large`-style link configurations and blocks 32-bit targets
  (`/mcmodel:large` and `/heap-arrays:0` are present but commented out at
  [`CMakeLists.txt:771-772`](../../CMakeLists.txt));
* destroys cache locality (arrays are strided by the *capacity*, not the size —
  e.g. `DELTAZ(LLEE,total_no_elements)` wastes 4/5 of every cache line when a
  column has 10 active cells);
* makes capacity overflow a silent memory corruption rather than an error
  (see §7.1);
* forces a recompile to run a larger catchment.

The end state is: true capacities become run-time sizes derived from input, and
their arrays are sized to the active problem. Numerical-resolution constants
such as `NXSCEE` remain constants unless changed in a separate accuracy feature.

---

## 2. Conversion technique

Four syntactic categories exist, with different conversion rules. Current counts
across `src/` (arrays, not source lines):

| Category | Count | Conversion |
|:---------|------:|:-----------|
| Module-level array declarations | 395 | Must become `ALLOCATABLE` + an `ALLOCATE` in an initialiser |
| Dummy-argument explicit shapes | 411 | Usually no syntactic change — a specification expression may reference a host/use-associated run-time value; audit sequence association and lower bounds at each interface |
| Procedure-local array declarations | 85 | Become automatic arrays; convert large or persistent ones to `ALLOCATABLE` storage and recreate any initial value explicitly |
| `ALLOCATE` statements that pass a capacity | 24 statements | Already allocatable; only the `ALLOCATE` argument changes. Invisible to the `.bss` measurements above — this storage is capacity-sized *heap* |

The fourth category is easy to miss because those arrays look converted already:
`DELTAZ`/`ZVSNOD(LLEE,…)`, `NLYRBT`/`NTSOIL`/`ZLYRBT(…,NLYREE)`, `RDF(NV,LLEE)`
([`AL_C.F90:246-248,278`](../../src/parameters/AL_C.F90)),
`IVSDUM_VSREAD`/`RVSDUM_VSREAD(NELEE,NLYREE)`
([`VSmod.f90:323-324`](../../src/modules/VSmod.f90) — 60 MB of heap on its own),
`MN_WORK`/`CELEM`-family `(NELEE)` and `IDUM1X(NELEE+3)`
([`MNmod.f90:462-463,526-529`](../../src/modules/MNmod.f90)),
`SYmod`'s `(NLFEE,NSEDEE)` block ([`SYmod.f90:226`](../../src/modules/SYmod.f90)),
`RDUM(NXEE*NYEE)` ([`SYmod.f90:2175`](../../src/modules/SYmod.f90)),
`NCATTY(NELEE,NCONEE)` and `TABLE_*(NOCTAB,NOCTAB,NCONEE)`
([`FRmod.f90:4240-4241`](../../src/modules/FRmod.f90)),
`TMELT`/`SMELT` ([`SMmod.f90:110-111`](../../src/modules/SMmod.f90)), and
`XSTAB` ([`OCmod2.f90:177`](../../src/modules/OCmod2.f90)).

The key enabler: changing

```fortran
INTEGER(KIND=I_P), PARAMETER :: nelee = 250000
```

to

```fortran
INTEGER(KIND=I_P), PROTECTED :: nelee = -1   ! set once during setup
```

leaves declarations such as `DOUBLE PRECISION, INTENT(IN) ::
DCBSED(NLFEE, NSED)` legal and turns ordinary local declarations into automatic
arrays. It does **not** make every non-module use safe or legal. Constant-only
contexts must be rewritten, including `PARAMETER` expressions, `DATA` repeat
counts such as `DATA BDONE / NELEE*.FALSE. /`, declaration-initialised locals,
and explicit-shape derived-type components. Large automatic arrays would move
from static storage to the stack. The declaration counts above are therefore a
search inventory, not a claim that only the 395 module declarations need work.

`PROTECTED` prevents accidental assignment outside `sglobal`; if Form A is used,
set all related values through one validated setup-size initialisation routine
that refuses a second call. Per-parameter setters would make inconsistent
partially initialised size state too easy.

### 2.1 Interim vs final form

Two forms are available, and they can be applied in either order:

* **Form A (variable capacity).** Keep the name and change `PARAMETER` to a
  value set once during setup. Dummy bounds can usually keep their spelling, but
  module arrays, type components, constant expressions, `DATA` statements,
  declaration initialisers, and large/persistent locals still need conversion.
  This removes the *waste* while retaining a global run-time capacity.
* **Form B (active size).** Delete the capacity entirely and dimension arrays by
  the existing active-size variable (`total_no_elements`, `total_no_links`,
  `top_cell_no`, `NX`, `NY`, `NV`, `NS`, `NSED`, `NCON`, …).

Form B is the goal. Form A is a useful intermediate only where a safe upper bound
is known before a writer but the final active size is known later. `NLYREE` does
not require Form A once its structured input is pre-scanned. `LLEE` may use Form
A for the mesh-construction workspace, but that capacity must be distinct from
the final active cell count and from other `LLEE`-sized domains (§4.4).

### 2.2 Where allocation goes

Follow the pattern already established in the tree:

* `initialise_al_c` / `initialise_al_c2` / `initialise_al_c3`
  ([`AL_C.F90:204-281`](../../src/parameters/AL_C.F90))
* `initialise_cont_cc`, `initialise_colm_cg`, `initialise_colm_co`
  ([`run_sim.f90:248-250`](../../src/modules/run_sim.f90))
* `INITIALISE_ETMOD` ([`ETmod.f90:175-184`](../../src/modules/ETmod.f90))
* `MNALLOCATE(NEL, NCETOP)` ([`MNmod.f90:436`](../../src/modules/MNmod.f90))

Allocation follows **size barriers**, not merely module ownership. Several
modules (`AL_C`, `AL_D`, `VSmod`, `ETmod`) own arrays whose dimensions become
known at different times, so they need barrier-specific allocation phases (or
small allocators grouped by dimension). A single all-purpose module initialiser
would either run too early or allocate some arrays twice.

Every conversion must also reproduce declaration initialisation explicitly after
allocation. Examples include `plant_cc` defaults (`BCPAA=BCPBB=0`,
`DELONE=0.5`, `NPLTYP=1`), VSS time/state defaults, and saved workspaces. Add
matching `finalise_<module>` routines where repeated setup in one process is a
supported goal; deallocation is not required merely to let the operating system
reclaim storage at program exit.

### 2.3 Pre-scan pattern for sizes buried in input files

Where a count is only available part-way through a data file, the tree already
has a working idiom: `GET_NCON_EARLY` / `GET_NSED_EARLY`
([`visualisation_interface_left.f90:482-566`](../../src/visualisation/visualisation_interface_left.f90))
scan for a record tag (`:CM3`, `:SY11`), read the count, and `REWIND`. Generalise
this into a small helper in `mod_load_filedata`:

```fortran
SUBROUTINE scan_for_count(unit, tag, value, context)
```

Use that helper only for a scalar value following a uniquely tagged record.
Structured cases (`ETD` breakpoints, OC cross sections, VSS layer records, and
hotstart payloads) need dedicated count-only readers that share record parsing
with the fill pass. A generic tag scanner must not guess record layout or skip
validation. Every discovery pass must rewind to a defined position and report
EOF/malformed input distinctly.

---

## 3. When each size becomes known — the setup barriers

Call order, from [`Shetran.f90:122-147`](../../src/Shetran.f90) and
[`FRmod.f90:1273-1399`](../../src/modules/FRmod.f90):

```
GET_DIR_AND_CATCH → FROPEN → SIMULATION → FRINIT →
    INFR → INITIALISE_AL_C3 → INITIALISE_ETMOD → INET → INSM →
    OCINI → FRDIM → INBK → VSIN → ReadZQTable
  ... then back in SIMULATION: GET_NSED_EARLY / GET_NCON_EARLY,
      initialise_cont_cc / colm_cg / colm_co, main loop
```

Within `INFR` ([`FRmod.f90:5120-5432`](../../src/modules/FRmod.f90)) the record
order is what makes this tractable:

| Line | Record | Establishes |
|-----:|:-------|:------------|
| 5198 | `:FR2` | **`NX`, `NY`** |
| 5225/5229 | `:FR8`,`:FR10` | `DXIN(1:NX-1)`, `DYIN(1:NY-1)` |
| 5258 | `:FR24` | `BEXSM`, **`BEXBK`**, `BEXSY`, `BEXCM` |
| 5326 | `:FR28` | **`NM`, `NRAIN`, `NV`, `NS`**, `NLYRCT` |
| 5363 | `:FR34` | `INGRID(1:NX,1:NY)` |
| 5388/5390 | `:FR35a/c` | `LCODEX(1:NX+1,1:NY)`, `LCODEY(1:NX,1:NY+1)` |
| 5393 | — | `CALL FRIND` → **`total_no_links`, `total_no_elements`** |
| 5399+ | `:FR37`… | `ZGRUND`, `NMC`, `NRAINC`, `NVC` — first element-indexed writes |

This yields five allocation/discovery barriers:

**Barrier A — after `:FR2` (line 5198).** `NX`, `NY` known.
Allocate all grid-shaped storage: `INGRID`, `LCODEX`, `LCODEY` (`AL_D`),
`ICMXY` (`AL_G`), `DXIN`, `DYIN`, and the `NXEE*NYEE` workspaces `IDUM` (`AL_C`)
and `RDUM` (`SYmod`).

**Barrier B — after `:FR35c` (line 5390), before `FRIND`.**
`total_no_links` and `total_no_elements` are fully determined by `INGRID`,
`LCODEX`, `LCODEY` and `BEXBK`, all of which are now read. `FRIND`'s first pass
([`FRmod.f90:653-710`](../../src/modules/FRmod.f90)) already computes exactly
these counts. Split that counting out into a new `FRCOUNT` called before the
allocation, then let `FRIND` fill the arrays as it does today.

> This is the finding that changes the expected ordering. `NELEE` and `NLFEE`
> are *not* hard because their values are unknown late — they are known before
> any element-indexed array is written. They are last in this plan purely because
> of blast radius: ~700 declaration sites depend on them.

**Barrier C — component discovery before component writers.** `NV`/`NM`/
`NRAIN`/`NS` are already known at `:FR28`. `NSED` from `:SY11`, `NCON` from
`:CM3`, ET table lengths, the six independent `NOCTAB` successors (§4.9), and
the three `NVSEE` successors must be discovered by pre-scan or a count-only
reader before allocating their arrays. "Allocate at the end of `OCINI`" is too
late: several OC arrays are filled while `OCINI` is still deriving `NOCBC` and
the cross-section counts.

**Barrier D — vertical-mesh construction and finalisation in `VSCONC`
([`VSmod.f90:1280+`](../../src/modules/VSmod.f90)).** A validated construction
capacity must exist before the first write to `DELTAZ`/`ZVSNOD`. The final
`top_cell_no` becomes known only after the renumbering retry loop succeeds. Only
then may final-cell arrays be allocated. The current retry loop calls
`INITIALISE_VSMOD` and `INITIALISE_AL_C` on every pass, so it must be restructured
before those routines allocate final-size arrays (§4.4 and Stage 7).

**Barrier E — hotstart restore.** Fresh setup must finish first. Read the stored
vertical-cell count into a separate scalar and require it to equal the freshly
built `top_cell_no`; do not overwrite the live mesh dimension. Snow-slug storage
must be allocated or grown to the largest stored `NSMC` before reading stored
`SMELT`/`TMELT` values (§4.13). This requires a header/count pass or a staged
hotstart reader; allocation after the single existing list-directed `READ` is too
late.

---

## 4. Parameter-by-parameter trace

For each parameter: where a dimensioned array is first written, whether the
active size is obtainable from input, and what blocks conversion.

### Summary table

| Parameter | Value | Active size | Source of active size | Barrier | Decls (mod/local) | Difficulty |
|:----------|------:|:------------|:----------------------|:--------|:------------------|:-----------|
| `NSETEE` | 45 | `NSET` | **never assigned** — dead | — | 8 / 0 | trivial (delete) |
| `NUZTAB` | 20 | `max(NF(1:NV))` | `:ET7` field `NF`, per veg type | C | 0 / 0 (3 allocs) | easy |
| `NVBP` | 140 | `max(JJJ)` | `:ET11(1..4/4)`, per veg type | C | 0 / 0 (8 allocs) | easy |
| `max_no_snowmelt_slugs` | 400 | `max(NSMC)` | hotstart input and run-time growth | E/run time | 0 / 0 (2 allocs) | medium (staged restore + grow) |
| `NXSCEE` | 100000 | — | **not a capacity**: numerical table resolution | — | 0 / 0 (1 alloc) | out of allocation scope |
| `NOLEE` | `2*LLEE` | none | inert alias; overlap arrays already use `2*top_cell_no+1` | — | 0 / 0 | delete |
| `NPELEE` | 2 | 1 or 2 active slots | derivable from `PLAI(NVC(element))` before `INPL` writes | C | 16 / 0 | easy after shape fix |
| `NPLTEE` | `NVEE` | plant types | `NV` at `:FR28`; `INPL` later sets `NPLT=NV` | C | 14 / 0 | easy |
| `NVSEE` | 20 | *three* distinct | see §4.10 | C | 33 / 6 | medium (split first) |
| `NOCTAB` | 20 | *six* distinct | see §4.9 | C | 10 / 6 | medium (split first) |
| `NSEDEE` | 7 | `NSED` or contaminant fallback 3 | `:SY11`; otherwise `INCM` uses 3 when contaminants run without sediment | C | 22 / 28 | medium |
| `NCONEE` | 3 | `NCON` | `:CM3` | C | 34 / 6 | medium |
| `NSEE` | 1000 | `NS` | `:FR28` **and** `:VS03` | B or C | 33 / 3 | medium |
| `NVEE` | 250000 | *three* distinct | `NV`, `NM`, `NRAIN` at `:FR28` | B | 31 / 14 | medium (split first) |
| `NLYREE` | 20 | `max(NLYR)+1` | `:VS08a`/`:VS08d`, terminator-scanned | C | 9 / 9 | medium-hard |
| `LLEE` | 50 | several distinct vertical domains | mesh construction/final mesh, root-density rows, and `+1` workspaces | D | 47 / 62 | hard (split first) |
| `NXEE`,`NYEE` | 1000 | `NX`, `NY` | `:FR2` | A | 12 / 4 | easy value, halo care |
| `NLFEE` | 20000 | `total_no_links` | countable at Barrier B | B | 55 / 3 | easy value, wide reach |
| `NELEE` | 250000 | `total_no_elements` | countable at Barrier B | B | 105 / 32 | easy value, widest reach |

### 4.1 `NELEE` = 250000 — total elements

First written: `FRIND` zeroes `NGRID`, `NBFACE`, `ICMREF(:,1:12)` over `1:NELEE`
([`FRmod.f90:639-643`](../../src/modules/FRmod.f90)), then writes `ICMREF`,
`LINKNS`, `ICMBK`, and `ICMXY` while it is still computing `INDEX`. A check made
only after `total_no_elements = INDEX` is therefore already too late: overflow
has happened. Stage 0 must either introduce `FRCOUNT` first or add a guard before
every increment-and-write operation.

Active size: `total_no_elements = INDEX` at
[`FRmod.f90:710`](../../src/modules/FRmod.f90), where
`INDEX = n_links + (2*n_links if BEXBK) + n_active_grid_cells`. All three terms
derive from `LCODEX`, `LCODEY`, `INGRID` and `BEXBK`.

Owners of `NELEE`-dimensioned module arrays: `AL_D` (22), `AL_C` (26), `SED_CS`
(6), `CONT_CC` (9), `plant_cc` (8), `sed_co` (4), `SYmod` (5), `MNmod` (7),
`VSmod` (12), `sglobal` (4: `cellarea`, `DXQQ`, `DYQQ`, `ZGRUND`), `OCmod` (2),
`OCmod2` (2), `OCQDQMOD` (2), `AL_G` (1), `colm_cg` (3), `FRmod` (2), `rest` (1).

The element count itself has no late-discovery blocker. The ordering above and
the separate `ICMRF2` record count (§4.2) are the technical prerequisites; the
rest is volume.

Note `DELTAP(0:NELEE)` ([`VSmod.f90:4243`](../../src/modules/VSmod.f90)) — the
zero lower bound is deliberate and must be preserved.

### 4.2 `NLFEE` = 20000 — channel links

First written: `ICMBK(L, IBANK)` at
[`FRmod.f90:690`](../../src/modules/FRmod.f90), inside the same pass that sets
`total_no_links`. Active size `total_no_links = INDEX` at
[`FRmod.f90:677`](../../src/modules/FRmod.f90).

As for `NELEE`, checking `total_no_links` after line 677 cannot protect the
earlier channel-link writes. `FRCOUNT` or pre-write guards are required.

`ICMRF2` is not proved to have `total_no_links` active rows. `FRIND` increments a
separate `INDEX2` for multi-link branch records and stores the final value in
`NEL2` ([`FRmod.f90:1115`](../../src/modules/FRmod.f90)). The count-only pass
must compute this third size as well, and `ICMRF2` must be allocated from it (or
from a separately proved upper bound), not by assumption from
`total_no_links`.

Owners: `AL_C` (13), `SED_CS` (11), `AL_D` (7), `link_cw` (6), `bk_cw` (5),
`OCmod` (4), `SYmod` (4), `CONT_CC` (3), `sed_co` (2), `VSmod` (2), `OCQDQMOD` (1).

Already handled dynamically: `xstab(3,nxscee,total_no_links)`
([`OCmod2.f90:177`](../../src/modules/OCmod2.f90)), `ghrf(total_no_links)`
([`OCmod.f90:1729`](../../src/modules/OCmod.f90)), `FCPBKO`/`GCPBKO`
([`CONT_CC.F90:108-109`](../../src/parameters/CONT_CC.F90)).

### 4.3 `NXEE`, `NYEE` = 1000 — basic grid extent

Active sizes `NX`, `NY` are read at
[`FRmod.f90:5198`](../../src/modules/FRmod.f90), long before any grid array is
touched. Arrays: `INGRID`, `LCODEX`, `LCODEY` (`AL_D`), `ICMXY` (`AL_G`),
`DXIN(NXEE)`, `DYIN(NYEE)` (`AL_D`), `NROWST(NYEE+1)` (`OCmod`),
`IDUM(NXEE*NYEE)` (`AL_C`), `RDUM(NXEE*NYEE)` ([`SYmod.f90:2175`](../../src/modules/SYmod.f90)).

**Halo requirement.** `FRIND` reads one cell outside the active grid in every
direction, relying on the oversized static arrays to absorb it:

```
FRmod.f90:765   IF (INGRID (I - 1, J) >= 0)        ! I=1  → INGRID(0,J)
FRmod.f90:778   IF (INGRID (I, J - 1) >= 0)        ! J=1  → INGRID(I,0)
FRmod.f90:890   IF (LCODEX (I, J - 1) >= 4)        ! J=1  → LCODEX(I,0)
FRmod.f90:994   IF (LCODEX (I + 1, J - 1) >= 4)    ! I=NX,J=1 → LCODEX(NX+1,0)
FRmod.f90:842   IF (LCODEY (I - 1, J + 1) >= 4)    ! I=1  → LCODEY(0,J+1)
```

With column-major storage, `INGRID(0,J)` currently aliases
`INGRID(NXEE,J-1)`. For the usual `NX < NXEE` case that aliased location is an
unused zero-initialised `.bss` slot; only when the active extent reaches the
capacity can it contain the preceding column's populated edge. The current
result is therefore accidental, but a regression difference is **not** an
expected consequence for ordinary models. Allocate an explicit halo and set
each halo value to the boundary semantics required by `FRIND`:

```fortran
ALLOCATE(INGRID(0:NX+1, 0:NY+1), LCODEX(0:NX+1, 0:NY+1), LCODEY(0:NX+1, 0:NY+1))
INGRID = -1        ! outside the catchment
LCODEX = 0 ; LCODEY = 0
ALLOCATE(ICMXY(0:NX+1, 0:NY+1)) ; ICMXY = 0
```

This also closes a latent edge-at-capacity bug. Treat any output difference as a
regression until the individual boundary decision has been traced; do not update
reference output merely because the new arrays have explicit halos.

Preserving the lower bound at call boundaries needs explicit attention. Passing
a whole `0:` allocatable to an explicit-shape dummy normally rebases the dummy's
first element. Pass the intended interior slice (for example
`INGRID(1:NX,1:NY)`) or change the interface to assumed shape and declare/use
the lower bounds deliberately.

Removing `NXEE` also exposes a separate file-format limit. `INFR` and utility
readers use formats such as `(I7,1X,500I1)`
([`FRmod.f90:5363`](../../src/modules/FRmod.f90)); allocation beyond 500 columns
does not make that input readable. Replace every fixed `500I1` read/write with a
dynamic parser/formatter (or a format whose repeat count is derived from `NX`)
in the same stage.

`mod_load_filedata`'s readers already take extents as arguments — `ALALLF`
receives `NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE`
([`mod_load_filedata.f90:125-127`](../../src/util/mod_load_filedata.f90)) and
`ALREAD` takes `N1`/`N2`
([`mod_load_filedata.f90:1134`](../../src/util/mod_load_filedata.f90)), so those
call sites need only their actual arguments changed. `utilsmod`'s `AREADI`/
`AREADR` ([`utilsmod.f90:1166,1351`](../../src/modules/utilsmod.f90)) take no
extents at all and read their bounds from host state. Either way the fixed
formats and any halo-bearing actual arguments still require the interface audit
above.

### 4.4 `LLEE` = 50 — vertical cells per column

`LLEE` does not have one active replacement. It currently conflates at least
four domains:

| Domain | Required extent |
|:-------|:----------------|
| Mesh-construction columns (`DELTAZ`, `ZVSNOD`) | capacity needed while `VSCONC` is still discovering and renumbering the mesh |
| Final cell-indexed state | final `top_cell_no` after the last successful retry |
| Root-density input `RDF` | maximum `NRD` read for any vegetation type, which is filled before the final mesh exists |
| Cell/layer workspaces | final active cells plus required sentinels; `DCSNOD(NCSZON+1)`, `DCRNOD(NCRBED+1)`, and `NIDUM(NITOT+1)`/`NJDUM(NJTOT+1)` need explicit `+1` storage |

The first-pass cell count in a non-link column starts at one (`ICL=1`), then
adds, for every retained aquifer layer,
`MAX(1,INT(DZLYR/VSZMAX)+1)`, not `CEILING(DZLYR/VSZMAX)`. It then adds up to
`NCSZON` soil-zone cells. A link copies part of a bank column and adds up to
`NCRBED` river-bed cells. These terms are all required in any initial
construction estimate.

That estimate is not a proved final upper bound. The connectivity pass can set
larger `LRENUM(element,layer)` values and retry mesh construction. In addition,
the present code calls `INITIALISE_VSMOD()` and `INITIALISE_AL_C()`
unconditionally inside every retry at
[`VSmod.f90:1460-1461`](../../src/modules/VSmod.f90); a second pass attempts to
allocate already allocated arrays. Consequently, "allocate once from the simple
formula and run `VSCONC` unchanged" is not viable.

The safe design is to separate construction from final storage. Run the
renumbering/count algorithm using growable per-column construction buffers (or a
count-only representation that records `LRENUM`) until it stabilises. Do not
allocate final `top_cell_no` arrays inside that retry loop. Then allocate final
cell state once, replay/populate the accepted geometry, and initialise the
solver arrays. If a single precomputed construction bound is preferred instead,
it must include the initial cell, aquifer formula above, soil and river-bed
cells, all sentinel slots, and a mathematically proved maximum produced by the
`LRENUM` retry algorithm; the current source does not provide that proof.

`NOLEE = 2*LLEE` ([`sglobal.f90:133`](../../src/parameters/sglobal.f90)) is used
only in `colm_cg`'s `USE` list and is otherwise inert; the overlap arrays are
already allocated as `2*top_cell_no+1`
([`colm_cg.f90:105-108`](../../src/parameters/colm_cg.f90)). `NOLEE` can be
deleted independently, in Stage 1.

### 4.5 `NVEE` = 250000 — **three different things**

`NVEE` simultaneously bounds three unrelated domains:

| Domain | Active size | Arrays |
|:-------|:------------|:-------|
| Vegetation types | `NV` (`:FR28`) | `CLAI`, `PLAI`, `NRD`, `RDL` (`AL_C`); `VHT`, `EPOTR`, `LROOT` (`AL_D`); `BAR`, `MODE`, `NF`, `MODECS`, `MODEPL`, `MODECL`, `MODEVH`, `NCTCST`, `NCTPLA`, `NCTCLA`, `NCTVHT` (`ETmod`); `DRDRIP`, `FDRIP`, `XDRIP` (`SYmod`) |
| Meteorological sites | `NM` (`:FR28`) | `OBSPE`, `TA`, `U`, `VPD`, `RN` (`AL_D`); `MEASPE` ([`ETmod.f90:96`](../../src/modules/ETmod.f90), filled `1:NM` at [`FRmod.f90:4869`](../../src/modules/FRmod.f90)); `IMET` ([`SMmod.f90:74`](../../src/modules/SMmod.f90), filled `1:NM` at [`FRmod.f90:5609`](../../src/modules/FRmod.f90)) |
| Rainfall stations | `NRAIN` (`:FR28`) | precipitation forcing indexed by `NRAINC(iel)` |

The doc comment on `MEASPE` already says "by meteorological site" while the
declaration says `NVEE` — the current code is only correct because
`NVEE ≥ max(NV, NM, NRAIN)`. `DEL(MAX(NV,NM,NRAIN))`
([`ETmod.f90:176`](../../src/modules/ETmod.f90)) is the one place that already
acknowledges the three-way split.

Split `NVEE` into `NVEGEE`/`NMETEE`/`NRAINEE` (or go straight to `NV`/`NM`/
`NRAIN`) **before** touching allocation. All three are known at `:FR28`, i.e.
before Barrier B.

### 4.6 `NSEE` = 1000 — soil types

Active size `NS`, read **twice**: at `:FR28`
([`FRmod.f90:5326`](../../src/modules/FRmod.f90)) and again at `:VS03`
([`VSmod.f90:3384`](../../src/modules/VSmod.f90), `NS = IDUM(1)`), where the
second read silently overwrites the first. Add a consistency check and a fatal
error on mismatch as part of this conversion; then allocate at Barrier B using
the `:FR28` value.

Arrays: `VSmod` (19: `VSK3D`, `IVSFLG`, `IVSNTB`, `VSTRES`, `VSVGN`, `VSALPH`,
`VSPSS`, `VSPPOR`, `VSPTHE`, `VSPKR`, `VSPETA`, `VSPDTH`, `VSPDKR`, `VSPDET`,
`TBPSI`, `TBTHE`, `TBKR`, `TBTHEC`, `TBKRC`), `SYmod` (6), `CONT_CC` (3),
`AL_C` (2: `THSAT`, `VSPOR`), `SED_CS` (2), `MNmod` (1).

### 4.7 `NLYREE` = 20 — soil layers per element

Active size: `max(NLYR(1:total_no_elements)) + 1`. `NLYR` is derived in `VSREAD`
by scanning `IVSDUM_VSREAD(ICAT, ICOUNT+1) /= 0` until a zero terminator
([`VSmod.f90:3588-3590`](../../src/modules/VSmod.f90)) — i.e. the layer count is
encoded as trailing zeros in a `NELEE × NLYREE` buffer read by
`ALREAD(6, VSD, PPPRI, ':VS08a', NELEE, NLYREE, …)`
([`VSmod.f90:3549,3647`](../../src/modules/VSmod.f90)). Getting the size
therefore requires a pre-scan of `:VS08a`/`:VS08d` counting fields per record.

**Encoding coupling.** `NLYREE` is not only a bound: `NMOD = NLYREE + 1` is used
as a radix to pack a `(lmin,lmax)` layer pair into one integer in `JVSALN`
([`VSmod.f90:1791`](../../src/modules/VSmod.f90)). It is spelled twice, in two
routines and two different ways: `VSCONC` declares it at
[`VSmod.f90:1278`](../../src/modules/VSmod.f90) and assigns it at
[`VSmod.f90:1299`](../../src/modules/VSmod.f90), while `VSCONL` makes it a
`PARAMETER` at [`VSmod.f90:1847-1849`](../../src/modules/VSmod.f90). (There is no
routine named `VSALN`; the encoder is `VSCONL`, `VSmod.f90:1839-2082`.) Both must
derive from the same run-time value, or `JVSALN` decodes to garbage. Convert
`NMOD` to a module variable set once alongside the layer capacity.

### 4.8 `NSEDEE` = 7 / `NCONEE` = 3

Active sizes `NSED` (`:SY11`) and `NCON` (`:CM3`), both already obtainable via
the existing `GET_NSED_EARLY` / `GET_NCON_EARLY` pre-scans
([`run_sim.f90:244-247`](../../src/modules/run_sim.f90)), which are called
*after* `FRINIT` today. `SYmod` and `CMmod` already pass `NSED`/`NCON` and the
capacities as dummy arguments throughout (157 and 29 dummy declarations
respectively), so almost all of the work is in `SED_CS`, `SED_CO`, `CONT_CC`,
`plant_cc` and the `AL_D` mean-accumulator arrays.

Move both pre-scans earlier (into `FROPEN` or the top of `FRINIT`) so `NSED` and
`NCON` are available at Barrier B/C rather than after setup completes.

`NSED` has a second source that the allocation rule must preserve. If
contaminants are enabled but sediment is not (`BEXCM` and not `BEXSY`), `INCM`
sets `NSED=3` and immediately initialises three sediment slots
([`FRmod.f90:4262-4267`](../../src/modules/FRmod.f90)). Therefore use:

```text
if sediment is active:       component_sediment_count = pre-scanned NSED
else if contaminant active:  component_sediment_count = 3
else:                        component_sediment_count = 0 (or no allocation)
```

The zero-component policy must be consistent across both compilers: either use
legal zero-sized allocations everywhere or leave arrays unallocated and guard
all accesses. Do not allocate solely from `:SY11`.

### 4.9 `NOCTAB` = 20 — **six different dimensions**

| Role | Active count | Read at | Arrays |
|:-----|:-------------|:--------|:-------|
| OC boundary-condition records | `NOCBC` | `OCBC` input, checked at [`OCmod.f90:785-789`](../../src/modules/OCmod.f90) | `NOCBCD(NOCTAB,4)` (`AL_D`), `HOCPRV`, `QOCFIN`, `HOCNXV` (`OCmod`), `COCBCD`, `HOCNOW`, `QOCF` (`OCQDQMOD`) |
| Roughness categories | `NCATR` | `:OC4`, checked at [`OCmod.f90:1841`](../../src/modules/OCmod.f90) | `CATR(NOCTAB)` |
| Default cross-section categories | `NDEFCT` | [`OCmod.f90:1535`](../../src/modules/OCmod.f90), checked at 1537 | category dimension of `XDEFH`, `XDEFW`; `NXDEF` |
| Width/depth pairs per cross-section | `max(maxval(NXDEF),maxval(NXSECT))` | [`OCmod.f90:1555`](../../src/modules/OCmod.f90), checked at 1557 | pair dimension of `XDEFH`, `XDEFW`, `XINH`, `XINW`, `XAREA` |
| Contaminant category types | maximum `NUM_CATEGORIES_TYPES(contaminant)` | contaminant table input via `CMRD` | first dimension of `NTAB`, `TABLE_CONCENTRATION`, `TABLE_WATER_DEPTH` |
| Contaminant data pairs per category | maximum `NTAB(category,contaminant)` | contaminant table input via `CMRD` | second dimension of `TABLE_CONCENTRATION`, `TABLE_WATER_DEPTH` |

The last two are independent even though `INCM` currently assigns `NOCTAB` to
both `MAX_NUM_CATEGORY_TYPES` and `MAX_NUM_DATA_PAIRS`
([`FRmod.f90:4236-4241`](../../src/modules/FRmod.f90)). They require two
successors, not one shared contaminant-table capacity.

Some existing paths range-check `NOCTAB`, but this does not solve allocation
timing. `OCINI` fills boundary and cross-section arrays while discovering their
counts, so allocating all four OC domains at the end of `OCINI` is too late.
Use count-only/pre-scan passes, then allocate, rewind, and perform the existing
fill pass. Split into six clearly named run-time sizes before allocation:
`oc_boundary_count`, `roughness_category_count`,
`default_cross_section_count`, `max_cross_section_pair_count`,
`max_contaminant_category_count`, and `max_contaminant_pair_count`.

### 4.10 `NVSEE` = 20 — **three different things**

| Role | Active count | Arrays |
|:-----|:-------------|:-------|
| Soil-characteristic table rows | `IVSNTB(is)` per soil, `:VS05a` | `TBPSI`, `TBTHE`, `TBKR`, `TBTHEC`, `TBKRC` `(NVSEE,NSEE)`; locals `XDUM`, `YDUM`, `Y2DUM`, `UDUM` ([`VSmod.f90:3356`](../../src/modules/VSmod.f90)) |
| VSS boundary categories | `NVSWL`, `NVSLF`, `NVSLH`, `NVSLG`, `NVSBF`, `NVSBH` | `RWELIN`, `RLFPRV`, `RLHPRV`/`RLHNXT`, `RLGPRV`/`RLGNXT`, `RBFPRV`, `RBHPRV`/`RBHNXT`, `RLFDUM`, `RLHDUM`, `RLGDUM`, `WLNOW`, `RLFNOW`, `RLHNOW`, `RLGNOW`, `RBFNOW`, `RBHNOW`, `VSZWLB`, `VSZWLT`, `NVSLF*`, `NVSLH*`, `NVSLG*` |
| Aquifer-connectivity records | `NAQCON`, `:VS10` | `IAQCON(4,NVSEE)` ([`VSmod.f90:2386`](../../src/modules/VSmod.f90)) |

Split into `NSOILTABEE`, `NVSBCEE`, `NAQCONEE`.

### 4.11 `NSETEE` = 45 — dead

The legacy binary result-set family (`IOCORS`, `IODATA`, `IOELEM`, `IORES`,
`IOSTA`, `IOSTEP`, `IOEND`, `IOTIME` in `AL_D`) is gated on `NSET`. `NSET` is
**never assigned** anywhere in the tree: its only writer would be `INRES`, whose
call is commented out at
[`FRmod.f90:1343`](../../src/modules/FRmod.f90). `FRRESC` — the routine that
would open the result units — has no call site at all. `FRRESP` is still called
under `BSTORE` ([`run_sim.f90:321,346`](../../src/modules/run_sim.f90)) and loops
`DO ISET = 1, NSET` over an undefined variable, indexing `IOTIME`, `IOEND`,
`IODATA`, `IOELEM`, and `IOCORS` inside it
([`FRmod.f90:3349-3369`](../../src/modules/FRmod.f90)).

This is already visible in the binary: none of those eight arrays has a symbol in
the LTO release build, because the compiler is entitled to treat the undefined
`NSET` however it likes and has elided the loop. The current behaviour of
`BSTORE` runs is therefore whatever the optimiser chose, not what the source
reads.

Action: delete the `NSETEE` family and the `FRRESC`/`FRRESP` legacy-result path,
or (if it must be retained for a future `INRES`) initialise `NSET = 0` explicitly
and allocate the arrays at size `NSET`. Either way this is a correctness fix, not
just a memory one — reading an undefined loop bound is undefined behaviour today.

### 4.12 `NXSCEE` = 100000 — not a capacity

`NXSCEE` is the *resolution* of the generated conveyance table, not a bound on
input: `STEPH = XINH(ielr,N)/(NXSCEE - 1.0d0)` and `DO J = 2, NXSCEE`
([`OCmod.f90:2476-2480`](../../src/modules/OCmod.f90)), with lookup
`I = INT((H/HFULL)*DBLE(NXSCEE-1) + ONE)`
([`OCmod2.f90:532`](../../src/modules/OCmod2.f90)). The array is already
allocatable ([`OCmod2.f90:177`](../../src/modules/OCmod2.f90)); at
`3 × 100000 × total_no_links × 8` bytes it costs **2.4 MB per link**, or
**2.4 GB per 1000 links**. This makes it an important memory consumer, but its
second dimension still expresses numerical accuracy rather than input capacity.

Action for this project: keep the default constant at 100000 and leave numerical
resolution outside the capacity-removal commits. A separate, explicitly tested
feature may later make it configurable or replace the dense per-link tables with
adaptive/shared storage. Changing the value changes results, so it must not be
mixed into this refactor.

### 4.13 `max_no_snowmelt_slugs` = 400

`TMELT`/`SMELT(max_no_snowmelt_slugs, total_no_elements)` are already allocatable
([`SMmod.f90:110-111`](../../src/modules/SMmod.f90)); the count `NSMC(IEL)` grows
at run time and is capacity-checked at
[`SMmod.f90:451`](../../src/modules/SMmod.f90). No input value bounds it.

The normal allocation currently occurs only on the first later call to `SM`, but
the hotstart reader in `FRINIT` reads `NSMC`, `SMELT`, and `TMELT` before that
call. A hotstart can therefore access unallocated arrays, and its stored `NSMC`
may exceed 400.

Action: allocate snow storage before hotstart restore. Use a staged hotstart
reader to discover the largest stored `NSMC`, validate non-negative counts, grow
to that extent, and only then read the slug values. During the simulation keep a
small initial capacity and grow the first dimension geometrically via
`MOVE_ALLOC`, preserving all existing values. The capacity variable belongs to
`SMmod`; it is not a compile-time `sglobal` problem after this conversion.

### 4.14 `NUZTAB` = 20, `NVBP` = 140

Both are per-vegetation table lengths read *inline while the tables are being
filled*, in `INET`:

* `NF(I)` is read in the same record as the parameters
  ([`FRmod.f90:4881-4883`](../../src/modules/FRmod.f90)) and then used as the
  `PS1`/`RCF`/`FET` row count at
  [`FRmod.f90:5004-5005`](../../src/modules/FRmod.f90).
* `JJJ` is read immediately before each `RELCST`/`RELPLA`/`RELCLA`/`RELVHT`
  breakpoint loop ([`FRmod.f90:4916-4924`](../../src/modules/FRmod.f90)).

But `INITIALISE_ETMOD` allocates all of them *before* `INET` runs
([`FRmod.f90:1295-1297`](../../src/modules/FRmod.f90),
[`ETmod.f90:175-184`](../../src/modules/ETmod.f90)). Options:

1. Pre-scan the `ETD` file for `max(NF)` and `max(JJJ)` (§2.3) — preferred,
   matches the existing idiom.
2. Move `INITIALISE_ETMOD` to after the parameter block of `INET` and allocate in
   two phases.

**Also a latent bug:** `JJJ` is never checked against `NVBP` and `NF(I)` is never
checked against `NUZTAB`. An ET file with 200 breakpoints overwrites past the
allocation today. Add the checks in Stage 0 regardless of when the conversion
happens.

### 4.15 `NPLTEE` = `NVEE`, `NPELEE` = 2

`NPLTEE` inherits the 250 000 from `NVEE` purely by aliasing. Its cost is
`MNPLANT`'s `CDI`/`CDIT(NPLTEE,30)` at 57.2 MiB each,
`PKMAX(NPLTEE,NCONEE)` at 5.7 MiB, and `PMASS`/`PF2MAX`/`DELONE(NPLTEE)` at
1.9 MiB each — about 129 MiB in total. Active plant-type count comes from `INPL`
([`FRmod.f90:5478`](../../src/modules/FRmod.f90)), which sets `NPLT=NV`, so it is
known from `:FR28`; it need not wait for `INPL`.

Note what `NPLTEE` does *not* explain. `PDZF3(NELEE,NPELEE,LLEE)`
([`plant_cc.f90:104`](../../src/parameters/plant_cc.f90)) is the single largest
static object in the program at 190.7 MiB, but it carries no `NPLTEE` dimension:
its size is `NELEE × LLEE`, so it is released by Stage 7 and Stage 9, not by the
`NPLTEE` work here. Correcting its index order (below) does not change its size
at all, since the product of the three extents is unchanged.

`NPELEE` is the per-element plant-slot count. `INPL` chooses one or two slots
from `PLAI(NVC(element))`, but writes slot 2 while making that choice. Compute
the run-wide slot count before allocating plant state using the same predicate
(`2` if any active column has `PLAI(NVC)<0.99`, otherwise `1`), then have `INPL`
validate rather than discover the bound.

There is an existing shape defect that must be fixed in the same semantic
preparation stage. `PDZF3` is declared
`(NELEE,NPELEE,LLEE)` but all active accesses, including
[`FRmod.f90:5520`](../../src/modules/FRmod.f90), use
`(element,cell,plant_slot)`. Its replacement shape is therefore
`(total_no_elements,top_cell_no,active_plant_slots)`, not a mechanical
substitution into the declared order. Recreate the `plant_cc` declaration
defaults explicitly after allocation, especially `BCPAA=BCPBB=0`,
`DELONE=0.5`, and `NPLTYP=1`.

Decoupling `NPLTEE` from `NVEE` remains a high-return change, but it is not
low-risk until the `PDZF3` shape and initialisation semantics above are fixed and
covered by a bounds-checked contaminant/plant test.

---

## 5. Implementation stages

The order below separates existing correctness defects from mechanical storage
conversion. Each stage is independently mergeable and verifiable. All discovered
sizes should be collected in one setup-size record (or equivalent module state),
validated once, and passed to barrier-specific allocators; do not scatter
independent assignments to replacement globals throughout the readers.

### Stage 0 — guard rails (no capacity changes)

Prerequisite for everything else. Purpose: make silent overflow impossible
*before* changing any bound, so that a later regression can be attributed.

1. Extract the count-only part of `FRIND` into `FRCOUNT` now. It must derive
   `total_no_links`, `total_no_elements`, and the separate `ICMRF2` row count
   without writing `ICMREF`, `ICMBK`, `ICMXY`, `LINKNS`, or `ICMRF2`. Check all
   three capacities before calling the existing fill pass. A check after
   [`FRmod.f90:677`](../../src/modules/FRmod.f90) or
   [`FRmod.f90:710`](../../src/modules/FRmod.f90) is too late because those arrays
   have already been written.
2. Add the remaining pre-write capacity checks:
   * `INFR`: assert `NX <= NXEE`, `NY <= NYEE` after
     [`FRmod.f90:5198`](../../src/modules/FRmod.f90).
   * After `:FR28`, validate non-negative `NM`, `NRAIN`, `NV`, and `NS` and
     their present capacities before any domain-indexed read or write.
   * Validate the early `NSED`/`NCON` discoveries and the contaminant-only
     sediment fallback before component setup.
   * `INET`: assert `NF(I) <= NUZTAB` and `JJJ <= NVBP`
     ([`FRmod.f90:4882`, `4916`](../../src/modules/FRmod.f90)). Neither name is
     currently mentioned anywhere except its `ALLOCATE` and the `INFR` banner.
   * `VSREAD`: assert `IVSNTB(IS) <= NVSEE` and `NAQCON <= NVSEE`.
     Count/validate each `:VS08` layer list before copying it into a fixed-width
     buffer; a later `NLYR < NLYREE` test is too late.
   * `VSCONC`: guard every increment before writing a cell, including link-bed
     cells and `NITOT+1`/`NJTOT+1` sentinels. A final
     `top_cell_no <= LLEE` assertion cannot protect earlier writes.
   * Audit the remaining scoped dimensions with the same rule: the check must
     precede the first indexed write, even where a later fatal range check
     already exists.
3. Fix `NSET`: initialise it to 0 if the legacy result path is retained
   temporarily (see §4.11).
4. Establish the regression baseline by running every model under `examples/`
   with the current release build and recording both compiler comparison CSVs.
   Do **not** regenerate `output_should/` as part of baseline creation; first
   explain any pre-existing mismatch.
5. Add a `Debug` CI configuration run over at least three examples with
   `-fcheck=bounds` / `/check:bounds` (already wired at
   [`CMakeLists.txt:779`](../../CMakeLists.txt) for ifx and
   [`822`](../../CMakeLists.txt) for gfortran). Bounds checking is the
   primary safety net for every later stage.
6. Add shared validation used by every later allocation: reject negative counts,
   define the zero-size/disabled-component policy, check integer multiplication
   before forming products, and report allocation failure with `STAT=` and
   `ERRMSG=`. The current tree's allocators do not provide this protection.

Expected memory change: none. Valid, defined input paths should remain
bit-identical. An existing overflow must now fail cleanly, and initialising
`NSET=0` deliberately replaces undefined legacy behaviour.

### Stage 1 — delete dead capacity

* Remove the `NSETEE` family and the unreachable `FRRESC` path (§4.11). This
  removes undefined behaviour, not memory.
* Remove `NOLEE` (§4.4) — inert.
* Remove the unused `PARAMETER` expressions built from capacities:
  `NTEMP1`/`NTEMP2` ([`plant_cc.f90:70-71`](../../src/parameters/plant_cc.f90))
  and `JVSDUM` ([`VSmod.f90:1277`](../../src/modules/VSmod.f90)), all documented
  as legacy and unread. They are constant contexts that would otherwise have to
  be rewritten in Stages 6 and 9 (§7.4).
* Remove the arrays documented as inactive with no consumer: `NGRID` (zeroed by
  `FRIND` at [`FRmod.f90:640`](../../src/modules/FRmod.f90) and never read),
  `NEXPO`, `WIDTF`, `ZBED`, `HFLBED`, `ZFBED`, `DZFBED`, `HFLBNK`, `LROOT`,
  `EPOTR` (`AL_D`); `NHSAT` (`AL_C`); `montec` (`sglobal`).
  Verify each with a fresh search before deleting — the FORD comments claim they
  are inactive, and that claim should be re-checked, not trusted.

Expected memory change: **none**. LTO already drops every one of these arrays
(§1), so none of them has a symbol in the release binary today. The value of
this stage is correctness and a smaller Stage 6/9 surface (~12 fewer declaration
sites), not bytes. Risk: low.

### Stage 2 — self-contained leaf parameters

First isolate the semantic corrections that would otherwise be hidden inside a
large allocation diff, then convert the leaf dimensions.

| Sub-step | Parameters | Files |
|:---------|:-----------|:------|
| 2a | Correct `PDZF3` order to `(element,cell,plant_slot)` and add a bounds-checked plant/contaminant regression | `plant_cc`, `FRmod:INPL`, consumers |
| 2b | `NPLTEE` → `NV`; precompute the active plant-slot count before `INPL`; explicitly restore declaration defaults after allocation | `plant_cc`, `MNmod`, `FRmod:INPL` |
| 2c | `NUZTAB`, `NVBP` via an `ETD` count pass, followed by allocate/rewind/fill | `ETmod`, `FRmod:INET` |
| 2d | Snow-slug storage → allocated before hotstart, staged hotstart restore, growable during `SM` | `SMmod`, `FRmod` |

Keep `NXSCEE=100000` unchanged in this project (§4.12). Expected static-memory
change is roughly **−129 MiB** after 2b: `CDI` and `CDIT` at 57.2 MiB each,
`PKMAX` at 5.7 MiB, and `PMASS`/`PF2MAX`/`DELONE` at 1.9 MiB each. `PDZF3` is
*not* included — it has no `NPLTEE` dimension and 2a does not change its size
(§4.15). Exact heap use remains model-dependent.

### Stage 3 — split the overloaded parameters

Pure renaming; no allocation yet, no behaviour change. Each new parameter keeps
the old numeric value initially, so the build is bit-identical.

* `NVEE` → `NVEGEE`, `NMETEE`, `NRAINEE` (§4.5)
* `NOCTAB` → six domain names: OC boundary records, roughness categories,
  default cross sections, cross-section pairs, contaminant categories, and
  contaminant pairs (§4.9)
* `NVSEE` → `NSOILTABEE`, `NVSBCEE`, `NAQCONEE` (§4.10)
* `LLEE` uses → construction cells, final cells, root-density rows, soil-zone
  nodes, river-bed nodes, and sentinel workspaces (§4.4); keep their initial
  numeric values where necessary during this rename-only stage

Verify by building and confirming byte-identical regression output. Splitting
before allocating is what makes Stages 4–6 mechanical instead of speculative.

### Stage 4 — component-count parameters

* `NSEDEE` → the component sediment count from §4.8; `NCONEE` → `NCON`. Move
  `GET_NSED_EARLY`/`GET_NCON_EARLY` ahead of the first component allocation and
  implement the contaminant-only fallback of three sediment slots. Add
  dimension-specific allocation phases for `sed_cs`, `sed_co`, `cont_cc`, and
  `plant_cc`.
* `NOCTAB`'s six successors → their active counts. Use count-only/pre-scan
  passes for OC and contaminant input, allocate, rewind, then fill. Do not wait
  until the end of `OCINI`.
* `NVSEE`'s three successors → their active counts, allocated in `VSREAD`
  (soil tables and boundary categories are read before first use; `IAQCON` needs
  a pre-scan of `:VS10` or a two-phase read). Where a reader currently writes
  while counting, perform discovery before allocation rather than relying on a
  post-write check.

Expected memory change: ~−98 MiB, from 166 MiB of `sed_cs` + `sed_co` +
`cont_cc` static that shrinks by the `NSED`/`NCON` ratio rather than vanishing.

### Stage 5 — `NSEE` and the `NVEE` successors

Both `NS` and `NV`/`NM`/`NRAIN` are known at `:FR28`, i.e. before Barrier B.
Add the `NS` double-read consistency check (§4.6). Allocate `VSmod`'s soil
property block, `AL_C`'s `THSAT`/`VSPOR`, `SYmod`'s soil arrays, `ETmod`'s
per-vegetation block, `SMmod`'s `IMET`, `AL_D`'s met arrays.

Expected memory change: ~−58 MiB, almost all of it the `NVEE` successors
(`ETmod` and `AL_D`'s per-site arrays); `NSEE` contributes about 10 MiB.

### Stage 6 — `NLYREE`

Pre-scan `:VS08a`/`:VS08d` for the maximum layer count. Convert `NMOD` in both
`VSCONC` and `VSCONL` to the matching run-time value (§4.7). Allocate `JVSALN`,
`NLYRBT`, `NTSOIL`, `ZLYRBT`, `LRENUM`, and `VSmod`'s layer-selection arrays.
Preserve the layer-end sentinel explicitly and initialise saved `LRENUM` state
at the start of each setup rather than relying on a declaration initialiser.

Expected memory change: ~−72 MiB, from the 95.4 MiB held by `JVSALN`
(76.3 MiB) and `LRENUM` (19.1 MiB); both keep their `NELEE` extent until
Stage 9.

### Stage 7 — `LLEE`

This stage must implement the domain split in §4.4, not replace every `LLEE`
token with `top_cell_no`.

1. Allocate `RDF` from the pre-scanned maximum `NRD`. Allocate soil-zone and
   river-bed node workspaces from `NCSZON+1` and `NCRBED+1`.
2. Refactor `VSCONC` so its retry/count phase uses growable per-column temporary
   geometry (or an equivalent count representation). Move
   `INITIALISE_VSMOD`/`INITIALISE_AL_C` out of the retry loop.
3. When `LRENUM` has stabilised, set `top_cell_no`, allocate final geometry and
   solver arrays exactly once, and populate them. Give `NIDUM`/`NJDUM` and any
   similar work array `top_cell_no+1` where the algorithm writes a sentinel.
4. Read hotstart's stored vertical count into a separate variable and require
   equality with the freshly built count before reading `VSPSI` (§7.8).
5. Convert the 62 procedure-local `LLEE` arrays. In particular, make `VSSIM`'s
   `CQ`, `VSPSIN`, and `VSTHEN` reusable allocatable workspaces at
   `(top_cell_no,total_no_elements)`. Do not move hundreds of megabytes to the
   stack.

Expected memory change: **~−458 MiB**, the largest single reduction in the plan.
`LLEE` carries 572 MiB of static storage: `VSSIM`'s `CQ`, `VSPSIN`, `VSTHEN` at
95.4 MiB each, `PDZF3` at 190.7 MiB, and `VSmod`'s `ICSOILSV`/`IVSSTO` at
47.7 MiB each. All of these keep an `NELEE` extent until Stage 9, so the residual
after this stage is still substantial.

### Stage 8 — `NXEE`, `NYEE`

Allocate at Barrier A with the halo of §4.3. Replace all fixed `500I1` grid
formats, and audit every call that passes a halo-bearing whole array to an
explicit-shape dummy; pass interior slices or use an assumed-shape interface.
No behaviour change is expected. Investigate any diff, especially on
edge-touching catchments (`Cobres`, `foston100m`,
`38014-100m-SurfaceErrors`), before accepting it.

Expected memory change: ~−30 MiB.

### Stage 9 — `NLFEE`, `NELEE`

The largest mechanical change: ~160 module declarations plus 35 locals across 20
files.

1. Use the Stage-0 `FRCOUNT` result at Barrier B. Recheck that it computes
   `total_no_links`, `total_no_elements`, and the `ICMRF2` active row count from
   `INGRID`/`LCODEX`/`LCODEY`/`BEXBK` without writing fill arrays.
2. Invoke barrier/dimension-specific allocation phases for element, link, and
   `ICMRF2` storage. Do not require one monolithic initialiser per owning module.
3. Convert module declarations file by file, in dependency order:
   `sglobal` → `AL_G` → `AL_D` → `AL_C` → `SED_CS`/`SED_CO` → `CONT_CC` →
   `plant_cc` → `link_cw`/`bk_cw`/`colm_cg` → `OCmod`/`OCmod2`/`OCQDQMOD` →
   `VSmod`/`SYmod`/`MNmod`/`FRmod`/`rest`.
4. Convert the 32 `NELEE`-sized procedure locals to allocatables (same stack
   argument as Stage 7): notably `FRINIT`'s `rdd(NELEE)`/`rddq(NELEE,4)`
   ([`FRmod.f90:1280`](../../src/modules/FRmod.f90)) and `VSSIM`'s
   `DELTAP(0:NELEE)`, `CDNET`, `OK`.
5. Preserve `DELTAP`'s `0:` lower bound. `NROWST`'s `NY+1` extent belongs to
   Stage 8. Replace every declaration initialiser and `DATA` repeat count that
   depended on `NELEE` with explicit setup code.

Expected memory change: the remainder — target total `.bss` under 10 MiB.

### Stage 10 — cleanup

* Delete `NXE`/`NYE`/`NXEP1`/`NYEP1` aliases in `AL_D`
  ([`AL_D.f90:108-113`](../../src/parameters/AL_D.f90)) once `NXEE`/`NYEE` are
  gone.
* Update the `INFR` capacity banner
  ([`FRmod.f90:5167-5189`](../../src/modules/FRmod.f90)) to print *active* sizes.
* Update the `sglobal` module header table and the manual's array-size table.
* Remove transitional run-time capacity aliases and setters after all consumers
  use semantic active sizes.
* Add `finalise_*` deallocation if multi-run operation in one process is a
  supported requirement; test a second setup before claiming that capability.
* Consider the analogous parameters left out of scope: `NSOLEE=200`
  ([`VSmod.f90:232`](../../src/modules/VSmod.f90)), `NSYCEE=10`
  ([`SYmod.f90:124`](../../src/modules/SYmod.f90)), `NSYBEE`,
  `MN_PLANT_NVALEE=30` ([`MNmod.f90:121`](../../src/modules/MNmod.f90)),
  `NCLASS=14` ([`AL_D.f90:68`](../../src/parameters/AL_D.f90)).

---

## 6. Cumulative effect

| After stage | Approx. static `.bss` | Cumulative reduction | Dominant release |
|:------------|----------------------:|---------------------:|:-----------------|
| baseline | 1274 MiB | — | |
| 1 | 1274 MiB | 0 % | none — LTO already elides the deleted arrays |
| 2 | 1145 MiB | 10 % | `CDI`, `CDIT` |
| 3 | 1145 MiB | 10 % | rename only; bit-identical |
| 4 | 1047 MiB | 18 % | `sed_cs`, `sed_co`, `cont_cc` |
| 5 | 989 MiB | 22 % | `ETmod`, `AL_D` per-site arrays |
| 6 | 917 MiB | 28 % | `JVSALN`, `LRENUM` |
| 7 | 459 MiB | 64 % | `PDZF3`, `VSSIM` locals, `ICSOILSV`, `IVSSTO` |
| 8 | 429 MiB | 66 % | grid workspaces |
| 9 | < 10 MiB (target) | > 99 % | everything `NELEE`/`NLFEE`-shaped |

Figures are derived from the per-symbol `nm` measurements in §1 by replacing each
capacity with a representative active size (`total_no_elements` 2000,
`total_no_links` 200, `top_cell_no` 10, `NX`/`NY` 100, `NLYR` 5, `NS` 10,
`NV`/`NM`/`NRAIN` 5, `NSED` 3, `NCON` 1) and are estimates for planning only.
Two effects dominate the shape of the table: most large arrays carry *two*
capacities, so they are not released until the later of the two stages; and
`NPELEE=2` is already an active-sized extent, so converting it frees nothing.

They measure static `.bss`, not total resident or heap memory. Capacity-sized
heap allocations (§2, fourth category) are not counted anywhere here and survive
every stage until their own `ALLOCATE` argument is changed. In particular, the
already-allocatable `XSTAB` remains about 2.4 MB per channel link at the
unchanged `NXSCEE=100000`; large channel networks can therefore use gigabytes
even after the static-capacity work is complete (§4.12).

---

## 7. Cross-cutting hazards

**7.1 Silent overflow becomes a hard error.** Today, `total_no_elements >
NELEE` writes past the end of `ICMREF` into whatever module follows it in `.bss`,
with no diagnostic. Any catchment that currently "works" while exceeding a
capacity will start failing after conversion. Stage 0 exists to surface these
*before* the bounds change, so a failure can be attributed to the model rather
than to the refactor. A useful guard must run before the first write, not after a
combined count/fill pass has returned.

**7.2 Out-of-bounds reads currently absorbed by oversizing.** §4.3 documents the
`INGRID`/`LCODEX`/`LCODEY` halo reads. Search for others as each stage lands, by
running `Debug` with bounds checking. `DELTAP(0:NELEE)` and `NROWST(NYEE+1)` are
known non-zero-based/off-by-one shapes that must be preserved deliberately.
`DCSNOD(NCSZON+1)`, `DCRNOD(NCRBED+1)`, and
`NIDUM(NITOT+1)`/`NJDUM(NJTOT+1)` are additional sentinel cases.

**7.3 Automatic arrays and the stack.** Converting a `PARAMETER` to a variable
silently turns 85 local declarations into automatic arrays. Several are hundreds
of megabytes (§1). They must be converted to `ALLOCATABLE` in the same commit
that changes the parameter, not left for later. Watch for `SAVE` semantics:
`LRENUM(NELEE,NLYREE) = 0` ([`VSmod.f90:1294`](../../src/modules/VSmod.f90)) has
an initialiser and is therefore implicitly `SAVE`d — an allocatable replacement
must make its setup lifetime explicit. The current one-run persistence is not
automatically the desired multi-run behaviour.

**7.4 Derived-type components cannot use variable bounds.** `MN_CONFIG_TYPE`
(`KDDSOL(NSEE)`) and `MN_PLANT_STATE_TYPE` (`NVALUE(NPLTEE)`, `CDI(NPLTEE,…)`,
`NPLTYP(NELEE,NPELEE)`, …) at
[`MNmod.f90:122-149`](../../src/modules/MNmod.f90) declare components with the
capacity parameters. A type component's bound must be constant, so these must
become `ALLOCATABLE` components — the type already mixes both styles
(`MN_WORKSPACE_TYPE` is fully allocatable), so follow that model.

The same constant-context rule applies outside types. The concrete sites are
`NTEMP1 = 2*NELEE*NPELEE*NCONEE` and `NTEMP2 = NPLTEE*NCONEE`
([`plant_cc.f90:70-71`](../../src/parameters/plant_cc.f90)),
`JVSDUM = NELEE*NLYREE` ([`VSmod.f90:1277`](../../src/modules/VSmod.f90)),
`NXE = NXEE` / `NYE = NYEE`
([`AL_D.f90:112-113`](../../src/parameters/AL_D.f90)), the `NMOD` `PARAMETER` in
`VSCONL` (§4.7), and `DATA BDONE / NELEE*.FALSE. /`
([`VSmod.f90:1860`](../../src/modules/VSmod.f90)). The first three are unread
legacy and should simply be deleted in Stage 1; the rest need rewriting as
run-time setup.

**7.5 Encoded values.** `NMOD = NLYREE + 1` is a radix, not a bound (§4.7).
`NPLTEE = NVEE` is an alias, not an independent capacity (§4.15).
`NOLEE = 2*LLEE` is derived (§4.4). Search for arithmetic on a capacity before
changing it.

**7.6 Initial values are data.** Allocatable arrays do not inherit a removed
declaration initialiser. Each allocation phase needs an explicit initial-value
ledger and assignments after successful allocation. This includes numeric and
logical defaults, saved retry state, and partially initialised arrays whose
unused entries are nevertheless read by legacy code.

**7.7 Allocation phases and retries.** `AL_C`, `AL_D`, `VSmod`, and `ETmod` each
own arrays from multiple size barriers. Allocation entry points must be
dimension-specific or safely idempotent. In particular, no final-size allocator
may remain inside a `VSCONC` retry. Every `ALLOCATE` should use `STAT=` and
`ERRMSG=` and report the requested shape.

**7.8 Hotstart is a sizing input, not an allocation barrier after the fact.**
The existing reader overwrites `top_cell_no` and then uses it to bound `VSPSI`;
it also reads arbitrary `NSMC` values and their `SMELT`/`TMELT` payloads. Read the
stored mesh count separately and validate equality with the fresh mesh. Discover
or grow snow capacity before reading its payload. "Allocate after the hotstart
read" cannot work with the existing single list-directed `READ`, because that
same statement already dereferences the arrays.

**7.9 Lower bounds and sequence association.** Whole-array actual arguments with
`0:` lower bounds are rebased when associated with many explicit-shape dummies.
Use explicit interior slices or assumed-shape interfaces. Preserve intentional
sequence association only where the storage order and required extent have been
proved.

**7.10 File-format capacities.** Removing an array bound does not remove literal
repeat counts such as `(I7,1X,500I1)`. Audit input and output formats alongside
each dimension so the file reader can represent the newly accepted size.

**7.11 Two compilers.** The suite is validated against both gfortran and ifx
(`examples/comparison_overview_gfortran.csv`, `_ifx.csv`). Allocation and
automatic-array behaviour differ between them (default stack limits, static
promotion of large locals). Every stage must be verified on both.

**7.12 Visualisation is already decoupled.** None of the `src/visualisation/*`
files reference any of the 20 parameters; they go through accessor functions.
No work is needed there, and that boundary should be preserved.

---

## 8. Verification protocol

Applied identically at every stage:

1. **Build** `Release` and `Debug` with gfortran and ifx. `Debug` carries
   `-fcheck=bounds` / `/check:bounds`, which is the primary detector for this
   class of change.
2. **Regression.** Run every model under `examples/` and compare with
   `python examples/check_results_consistency.py` against `output_should/`.
   Bit-identical output is required for every storage-only commit, including
   Stage 8. Existing-bug fixes (`PDZF3`, malformed hotstart handling, `NSET`)
   should be separate commits with focused tests; accept a numerical difference
   only after tracing it to that specific correction. Never update
   `output_should/` merely because an array shape changed.
3. **Footprint.** Record `size build/release/bin/shetran` and the per-module
   `nm -S` aggregation (command in §9) in the stage's commit message. This is the
   objective measure that the stage did what it claimed.
4. **Unit tests.** `test/oc_row_width` and `test/visualisation_read` build under
   `SHETRAN_BUILD_TESTS`; keep them green.
5. **Shape and disabled-component tests.** Cover zero/disabled sediment and
   contaminant modes, the contaminant-only three-sediment fallback, one- and
   two-slot plants, maximum root-density input, a `VSCONC` renumbering retry,
   grid-edge models, and a grid wider than 500 columns. Assert discovered sizes
   and final allocation bounds in Debug builds.
6. **Hotstart tests.** Restore a normal hotstart, reject a mismatched stored
   `top_cell_no`, restore more than 400 stored snow slugs, and reject negative or
   truncated counts without touching unallocated storage.
7. **Former-capacity probes.** Run at least one generated/minimal model beyond
   each old limit where practical, and malformed-count inputs that must fail
   cleanly. Once a capacity is removed, do not emulate the test by setting a new
   artificial capacity too small.
8. **Allocation diagnostics.** Exercise validation failures and, where feasible,
   an allocation-failure path; require a shape-bearing fatal diagnostic rather
   than a compiler/runtime abort.

---

## 9. Measurement commands

```bash
# Total static footprint
size build/release/bin/shetran

# Static bytes per Fortran module, largest first
nm -S build/release/bin/shetran \
  | awk '($3=="B"||$3=="b"){ sz=strtonum("0x"$2); n=$4;
      if (match(n,/^__[a-z0-9_]+_MOD_/)) m=substr(n,3,RLENGTH-7);
      else m="<procedure-local static>";
      tot[m]+=sz; all+=sz }
    END { for (m in tot) printf "%14.2f MiB  %s\n", tot[m]/1048576, m
          printf "%14.2f MiB  == TOTAL ==\n", all/1048576 }' \
  | sort -rn

# Largest individual static objects
nm -S --size-sort build/release/bin/shetran | tail -40

# Declaration sites still using a capacity
rg -n -w 'NELEE' src -g '!parameters/sglobal.f90'
```

---

## 10. Open questions

The corrected plan makes three decisions: semantic active sizes are the end
state (Form B), `NXSCEE` remains a constant numerical-resolution setting, and a
hotstart mesh-count mismatch is fatal before state is read. The remaining policy
questions are:

1. **Legacy binary results.** Stage 1 proposes deleting the `FRRESC`/`NSETEE`
   path as unreachable. Is `INRES` intended to return, or can the whole legacy
   result-file mechanism go?
2. **Hotstart compatibility.** Can the hotstart format gain an explicit header
   containing per-record mesh and maximum-snow-slug counts, or must the staged
   reader remain byte-compatible with existing list-directed files?
3. **Multi-run capability.** Adding and testing `finalise_*` deallocation is only worthwhile
   if a future driver runs several catchments in one process. Is that on the
   roadmap?
