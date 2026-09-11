# Phase 7 — Overland / channel chain

**Depends on:** phase 3, and **phase 2 should land first** — it removes the
`LDUM1(NELEE)` mask and the `SZLOG` plumbing from the same routines, and doing
both at once makes the diff hard to review.
**Independent of** phases 4, 5, 6.
**Chain:** `OCINI` → `OCCHK2`; `OCREAD`; `JEOCBC`; `INBK`.

## Current state, after the `e09f465` trim

| Routine | File | Array | Extent | Bytes |
|:--------|:-----|:------|:-------|------:|
| `OCREAD` | `oc_input.f90` | `IDUM` | `NELEE` | 1,000,000 |
| `OCREAD` | `oc_input.f90` | `DUMMY` | `NELEE` | 2,000,000 |
| `JEOCBC` | `oc_input.f90` | `IDUM` | `NELEE` | 1,000,000 |
| `INBK` | `bank_setup.f90` | `IDUM` | `NELEE` | 1,000,000 |
| `INBK` | `bank_setup.f90` | `DUMMY` | `NELEE` | 2,000,000 |
| `OCINI` | `oc_driver.f90` | `DUMMY` | `NLFEE` | 160,000 |

All six are already at the right *class* of extent. What remains is to make
them runtime-sized and stop them being `SAVE`d statics.

## True requirement

All of these are filled by `AREADI`/`AREADR`, whose destination dummy is
`IAOUT(:)` / `AOUT(:)` — **assumed shape**, already. `AREADI` writes
`IAOUT(1:total_no_elements)` (and `IAOUT(NGDBGN:total_no_elements)` in the
fill mode). So:

| Array | Requirement |
|:------|:------------|
| `OCREAD` integer | `total_no_elements`; also passed to `OCPLF` as `NXDEF(NOCTAB)`, so at least `NOCTAB` |
| `OCREAD` real | `total_no_elements` (`AREADR`, then `DUMMY(ielt)`) |
| `JEOCBC` integer | `total_no_elements` (`AREADI`, read at `IDUM(IELY)` over `NGDBGN..total_no_elements`) |
| `INBK` integer | `total_no_elements` (`IDUM(IEL)` over `NGDBGN..total_no_elements`) |
| `INBK` real | `total_no_elements` (same loop; read back into `ZGRUND`, `STRXX`, `SD`, …) |
| `OCINI` real | `MAX(total_no_links, NOCTAB)` — `OCCHK2` writes `DDUM1A(IELw)` over `1..total_no_links` and `DDUM1A(1:N)` with `N = NXSECT(IELw)-1 <= NOCTAB` |

`total_no_elements` is established by `INFR`, which is the first call in
`FRINIT` — well before `OCINI`, `INBK` and `VSIN`. Verify the ordering in
`src/frame/frame_setup.f90` before relying on it.

## Target

Create `src/overland_channel/oc_workspace.f90`. One array per distinct need —
do not share one buffer between `OCREAD`, `JEOCBC` and `INBK` merely because
their extents coincide:

```fortran
INTEGER, ALLOCATABLE :: oc_element_codes(:)
   !! Per-element category codes read by AREADI for the OC boundary records.
DOUBLE PRECISION, ALLOCATABLE :: oc_element_values(:)
   !! Per-element values read by AREADR for the OC boundary records.
INTEGER, ALLOCATABLE :: oc_boundary_categories(:)
   !! Per-element boundary-category codes read by JEOCBC.
DOUBLE PRECISION, ALLOCATABLE :: oc_link_section_widths(:)
   !! Per-link cross-section scratch written by OCCHK2.
INTEGER, ALLOCATABLE :: bk_element_codes(:)
   !! Per-element integer codes read by INBK for the bank records.
DOUBLE PRECISION, ALLOCATABLE :: bk_element_values(:)
   !! Per-element values read by INBK for the bank records.
```

Rename freely if reading the code suggests better names — these are proposals,
and the rule is that the name says what the array holds. `bank_setup` is its
own module but lives in the same directory; giving it its own arrays inside
`oc_workspace` is acceptable, as is a separate `bank_workspace`. Pick one and
be consistent.

## Work

1. Create the module; allocate each array via `ensure_capacity` on entry to its
   owning routine.
2. Replace the six `SAVE`d declarations with `USE` of the new module.
3. `OCINI` passes its buffer to `OCCHK2`, whose dummy is `DDUM1A(:)` —
   assumed shape, so it already accepts any size. Either keep the argument
   (simplest, and the data flow stays visible) or drop it in favour of module
   access. Prefer keeping it: `OCCHK2` has exactly one caller and the argument
   costs nothing now that the size constraint is gone.
4. `OCREAD` passes its integer buffer to `OCPLF` as `NXDEF(NOCTAB)`. That is an
   explicit-shape dummy needing `NOCTAB` elements; `total_no_elements` exceeds
   it for any real model, but add the `MAX(..., NOCTAB)` to the sizing so the
   relationship is stated rather than assumed.
5. Delete `nelee`/`nlfee` `USE` imports that become unused, as `e09f465` did.

## Pitfalls

- `INBK`'s real buffer is **not** write-only scratch. It is filled across a
  13-iteration record loop and read back into `ZGRUND`, `STRXX`, `STRYY`, `SD`,
  `RHOSAR`, `ZVSPSL` and `SETHRF`. Its contents must survive the whole routine.
  `ensure_capacity` does not preserve contents across a grow — size it once on
  entry, before the loop, not inside it.
- `SETHRF(IEL, ZGRUND(IEL) + DUMMY(IEL))` appears in both `INBK` and `OCREAD`
  and passes an expression, not the array. No sequence-association concern, but
  do not disturb the argument order.
- `AREADI` has a fill mode (`KON == 3`) that writes only
  `NGDBGN:total_no_elements`, leaving `1:NGDBGN-1` undefined. Anything reading
  below `NGDBGN` was reading undefined memory before and still will. Do not
  "fix" this by zeroing — it would change results if any consumer depends on
  the stale values. Note it for a separate investigation.
- `oc_validation.f90` is also touched by phase 2. Land phase 2 first.

## Expected result

- ~7,160,000 B moved from `.bss` to right-sized heap.
- Six `SAVE` attributes removed.
