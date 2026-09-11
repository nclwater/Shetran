# The size of the `IDUM` and `DUMMY` input workspaces

**Deviation:** the `input_workspace` follow-up in
[deviations.md](../plan/deviations.md) — "`IDUM`/`DUMMY` keep oversized
declarations".

**Background:** `src/io/input_workspace.f90` held two module-level scratch
arrays, `IDUM(NXEE*NYEE)` and `DUMMY(NELEE)`, shared by every input reader.
That module was deleted and each reader was given its own local copy with the
same extents and an explicit `SAVE`. This note records what each copy actually
needs, so the extents can be trimmed deliberately rather than by guesswork.

**Answer in one line:** of the sixteen locals, **five are bound by a callee's
explicit-shape dummy and must keep their present size**, three are already
correctly sized, and **eight are oversized** — one of them by a factor of 5000.
Trimming the eight would return **about 22 MB** of static storage. Nothing here
is a correctness bug; every array is at least as large as it needs to be.

## What the change cost

`IDUM` is `NXEE*NYEE` = 1000 × 1000 default integers = 4,000,000 B.
`DUMMY` is `NELEE` = 250,000 doubles = 2,000,000 B.
There are now eight of each, one per procedure, all `SAVE`d into static
storage. Measured on the Debug binary:

```
$ nm --size-sort -S build/debug/bin/shetran | grep -E '\b(idum|dummy)\.[0-9]+'
  ... 16 symbols ...
48000000 bytes (45.8 MiB) in 16 symbols
```

The single module-level pair it replaced was 6,000,000 B, so the localisation
cost **42 MB** of `.bss`. For scale, the Debug binary's total `.bss` is
1.66 GB, so this is roughly 2.5% of an already very large static footprint —
which is why the change was not noticeable in the example-model runs.

## Site-by-site requirement

`NGDBGN..total_no_elements` is an element index, bounded by `NELEE` (250,000).
A grid index is bounded by `NXEE*NYEE` (1,000,000). A column cell index is
bounded by `LLEE` (50). A link index is bounded by `NLFEE` (20,000).

| Procedure | Array | Declared | Needs | Why |
|:----------|:------|:---------|:------|:----|
| [[cm_input:INCM]] | `IDUM` | `NXEE*NYEE` | `NXEE*NYEE` | passed to `CMRD`, whose dummy is explicit-shape `IDUM(NXEE*NYEE)` |
| [[cm_input:INCM]] | `DUMMY` | `NELEE` | `NELEE` | `CMRD` dummy is `DUMMY(NELEE)` |
| [[simulation_driver:SIMULATION]] | `IDUM` | `NXEE*NYEE` | `NXEE*NYEE` | passed to `SYMAIN`, dummy `IDUM(NXEE*NYEE)` |
| [[simulation_driver:SIMULATION]] | `DUMMY` | `NELEE` | `NELEE` | `SYMAIN` dummy is `DUMMY(NELEE)` |
| [[vs_input:VSREAD]] | `IDUM` | `NXEE*NYEE` | `NXEE*NYEE` | `ALREAD` mode 4 with `IDATA(N1,N2)` called as `(NX, NY)` |
| [[vs_input:VSREAD]] | `DUMMY` | `NELEE` | `NELEE` | see the note below — formally `NX*NY`, but never referenced |
| [[bank_setup:INBK]] | `IDUM` | `NXEE*NYEE` | **`NELEE`** | only ever `IDUM(IEL)` over `NGDBGN..total_no_elements` |
| [[bank_setup:INBK]] | `DUMMY` | `NELEE` | `NELEE` | correct as declared |
| [[vs_connectivity:VSCONC]] | `IDUM` | `NXEE*NYEE` | **`NELEE`** | subscripts are `IEL`, `ILINK`, `IBANK2` — all element indices |
| [[oc_validation:OCCHK1]] | `IDUM` | `NXEE*NYEE` | **`NXEE`** | `IDUM(X)` for `X = 1..NX`, one grid row at a time, then `IDUM(1:NX)` |
| [[oc_input:JEOCBC]] | `IDUM` | `NXEE*NYEE` | **`NELEE`** | filled by `AREADI`, which writes `IAOUT(1:total_no_elements)`; read at `IDUM(IELY)` |
| [[oc_input:OCREAD]] | `IDUM` | `NXEE*NYEE` | **`NELEE`** | `AREADI` as above; also passed to `OCPLF`, whose dummy is `NXDEF(NOCTAB)` = 20 |
| [[oc_input:OCREAD]] | `DUMMY` | `NELEE` | `NELEE` | `AREADR` / element loop |
| [[oc_driver:OCINI]] | `DUMMY` | `NELEE` | **`NLFEE`** | `OCCHK2` writes `DDUM1A(IELw)` for `IELw = 1..total_no_links`, and `DDUM1A(1:N)` with `N = NXSECT(IELw) - 1 <= NOCTAB` |
| [[cm_column:COLMSM]] | `DUMMY` | `NELEE` | **`LLEE`** | subscript is `NCE`, a column cell index, over `NCEBOT..NCETOP` |
| [[cm_column:COLMW]] | `DUMMY` | `NELEE` | **`LLEE + 1`** | the single statement `DUMMY(NCEBOT-1:NCETOP+1) = zero` — and it is a dead write |

The two `cm_column` entries are the extreme case: 2,000,000 B reserved to hold
at most 51 values. `COLMW`'s copy exists only to be zeroed and is never read,
so it could be deleted outright rather than resized.

## What trimming would save

| Change | Saving |
|:-------|-------:|
| Four `IDUM` from `NXEE*NYEE` to `NELEE` (`INBK`, `VSCONC`, `JEOCBC`, `OCREAD`) | 12.0 MB |
| `OCCHK1`'s `IDUM` from `NXEE*NYEE` to `NXEE` | 4.0 MB |
| `OCINI`'s `DUMMY` from `NELEE` to `NLFEE` | 1.8 MB |
| `COLMSM`'s `DUMMY` from `NELEE` to `LLEE` | 2.0 MB |
| Delete `COLMW`'s `DUMMY` and its dead write | 2.0 MB |
| **Total** | **21.8 MB** |

The five interface-bound locals account for 16 MB of the remainder (three
`IDUM` at 4 MB, two `DUMMY` at 2 MB). They cannot shrink without changing
`CMRD`, `SYMAIN` and `ALREAD` to take assumed-shape dummies, which is a much
larger change and is not proposed here. Of the remaining 10 MB, 6 MB is in the
three locals that are already the right size (`VSREAD`, `INBK` and `OCREAD`'s
`DUMMY`, 2 MB each) and about 4 MB is what the trimmed eight would still
occupy, almost all of it the four `IDUM` at `NELEE`.

## A latent non-conformance found on the way

[[vs_input:VSREAD]] line 723 calls

```fortran
CALL ALREAD(4, VSD, FID_logfile, ':VS08c', NX, NY, NUM_CATEGORIES_TYPES, CDUM, IDUM, DUMMY)
```

`ALREAD`'s real destination is the explicit-shape dummy `RDATA(N1, N2)`, so
here it is `RDATA(NX, NY)`. Sequence association requires the actual argument
to have at least `NX*NY` elements, but `DUMMY` has `NELEE` = 250,000, which is
smaller than the worst-case `NX*NY` = 1,000,000. A grid with more than 250,000
cells would make the call formally non-conforming.

In practice nothing happens: mode 4 reads only into `IDATA` and never
references `RDATA`, so no out-of-bounds access occurs. The defect is also
**pre-existing** — the module-level `DUMMY` was `NELEE` too, so localisation
neither introduced nor worsened it. It is recorded here because the same
reasoning that sizes the locals is what surfaced it. The clean fix is to make
`ALREAD`'s `IDATA`/`RDATA` assumed-shape, which would remove the constraint at
every call site at once.

## Recommendation

Trim the oversized locals to the "Needs" column and delete `COLMW`'s dead
`DUMMY`. That is a mechanical, per-procedure change with no cross-procedure
coupling, because every one of these arrays is scratch used and consumed inside
a single procedure — which is exactly what the localisation established. Each
new extent must be `USE`d from [[array_limits]] where it is not already
imported.

Leave the five interface-bound locals alone unless `CMRD`, `SYMAIN` and `ALREAD`
are reworked to take assumed-shape dummies; at that point all sixteen could
become automatic arrays of the true problem size and the `SAVE` attributes
could go.
