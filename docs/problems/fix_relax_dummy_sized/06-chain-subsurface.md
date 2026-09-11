# Phase 6 — Subsurface chain

**Depends on:** phase 3, and **phase 1 should land first** — it removes the
dead `RDATA` argument that currently pins `VSREAD`'s real workspace.
**Independent of** phases 4, 5, 7.
**Chain:** `VSREAD`, `VSCONC` (both in `src/subsurface/`).

## Current state

`src/subsurface/vs_input.f90`, in `VSREAD`:

```fortran
INTEGER, DIMENSION(NXEE*NYEE), SAVE :: IDUM      ! 4,000,000 B
DOUBLEPRECISION, DIMENSION(NELEE), SAVE :: DUMMY ! 2,000,000 B
```

`src/subsurface/vs_connectivity.f90`, in `VSCONC`:

```fortran
INTEGER, DIMENSION(NELEE), SAVE :: IDUM          ! 1,000,000 B (trimmed in e09f465)
```

This module already uses allocatable work buffers for the same job —
`IVSDUM_VSREAD`, `RVSDUM_VSREAD`, `ISDUM_VSREAD`, `RSDUM_VSREAD` — so the
pattern is established here and should be followed, including moving those
into the new module if it tidies the file. Their `_VSREAD` suffix is a
rename-era artefact; new arrays do not need it.

## True requirement

**`VSREAD` integer.** Live at 17 `ALREAD` sites. Four of them are mode 4 grid
reads at `(NX, NY)` (`:VS08c`, `:VS14`, `:VS15`, `:VS17`, `:VS18`). So the
requirement is **`NX*NY`** — a genuine grid buffer.

**`VSREAD` real.** After phase 1, live at only three sites:

| Record | Shape | Requirement |
|:-------|:------|:------------|
| `:VS04` | `(5, 1)` | 5 |
| `:VS12b` | `(2, NW)` | `2*NW` |
| `:VS13b` | `(3, NSP)` | `3*NSP` |

That is, `MAX(5, 2*NW, 3*NSP)` — nothing like `NELEE`. The current `NELEE`
extent exists only because the buffer was also passed as a dead placeholder at
the mode-4 calls, one of which (`:VS08c`) is the documented non-conformance.
Phase 1 deletes those placeholders; this phase collects the resulting freedom.

Check what bounds `NW` and `NSP` — they come from `:VS11`/`:VS12`/`:VS13` and
are validated against `NVSEE` or similar. If no check exists, add one rather
than growing the buffer from unvalidated input.

**`VSCONC` integer.** Subscripted by `IEL` (element), `ILINK = ICMREF(IEL,4)`
(link element number) and `IBANK2 = ICMBK(ILINK,2)` (bank element number), and
read back as `NCL = IDUM(IEL)` over `ICOL1..total_no_elements`. All are element
numbers, so **`total_no_elements`**.

## Target

Create `src/subsurface/vs_workspace.f90`:

```fortran
INTEGER, ALLOCATABLE :: vs_grid_codes(:)
   !! Flattened grid of VSS category/type codes read by ALREAD mode 4.
   !! Extent is NX*NY.
DOUBLE PRECISION, ALLOCATABLE :: vs_record_values(:)
   !! Record value buffer for VSREAD's list reads (:VS04, :VS12b, :VS13b).
   !! Extent is MAX(5, 2*NW, 3*NSP).
INTEGER, ALLOCATABLE :: vs_column_cell_counts(:)
   !! Number of VSS cells assigned to each element column, built by VSCONC.
   !! Extent is total_no_elements.
```

`vs_column_cell_counts` is a better name than a generic scratch one: in
`VSCONC` the array genuinely carries meaning between the two loops (it is
written per element/link/bank and read back as `NCL`). Confirm that reading of
the code before settling on the name.

## Work

1. Create the module; allocate via `ensure_capacity`.
2. `VSREAD`: `NX`/`NY` are available on entry (set by `INFR`, which runs first
   in `FRINIT`). Size `vs_grid_codes` there. Size `vs_record_values` after
   `:VS11`/`:VS12`/`:VS13` have supplied `NW` and `NSP`.
3. `VSCONC`: size `vs_column_cell_counts` on entry from `total_no_elements`.
4. Replace the `SAVE`d locals with `USE vs_workspace`.
5. Consider folding `IVSDUM_VSREAD` and friends into the same module with
   proper names. This is optional and can be a follow-up; if you do it, do it
   as a separate commit so the storage change stays reviewable.

## Pitfalls

- `VSCONC` also declares `INTEGER :: LRENUM(NELEE, NLYREE) = 0`, which is
  19,531 KB of `.bss` and carries a default initialiser that makes it
  implicitly `SAVE`. It is a much bigger object than anything in this plan and
  is **out of scope** — note it for a follow-up, do not touch it here.
- `VSREAD` is long and has several `SAVE`d module-level buffers with
  interlocking lifetimes. Change one array at a time and rebuild between each.
- The `:VS08c` call must lose its `RDATA` argument in phase 1. If phase 1 has
  not landed, sizing `vs_record_values` below `NX*NY` reintroduces the
  non-conformance as a real out-of-bounds write rather than a latent one. Check
  that the placeholder is gone before shrinking.

## Expected result

- `VSREAD`: 6,000,000 B replaced by `NX*NY` integers and a handful of doubles.
- `VSCONC`: 1,000,000 B replaced by `total_no_elements` integers.
- The `:VS08c` non-conformance provably gone, not merely documented.
