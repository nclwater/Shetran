# Phase 1 — `ALREAD`'s data arguments become `OPTIONAL`

**Depends on:** nothing. **Blocks:** nothing (phases 4–7 are easier after it).
**Target:** `src/io/record_readers.f90`, plus 52 call sites in three files.

## Why

`ALREAD(FLAG, IUNIT, OUNIT, LINE, N1, N2, NUM_CATEGORIES_TYPES, CDATA, IDATA, RDATA)`
dispatches on `FLAG`, and each mode touches at most two of the three data
arguments. Every call site must nevertheless supply all three. Across the 52
sites, **103 of the 156 data arguments are placeholders the callee never
references**.

This is also where a latent standard non-conformance lives. At
`src/subsurface/vs_input.f90` the `:VS08c` call passes `DUMMY(NELEE)` as
`RDATA(NX, NY)`; sequence association requires the actual to have at least
`NX*NY` elements, which `NELEE` does not guarantee. Mode 4 never references
`RDATA`, so nothing happens at run time — and once `RDATA` is optional, that
call simply omits it and the defect is gone rather than documented.

## Which modes use which argument

Derived from the `SELECT CASE (FLAG)` block. Confirm against the source before
relying on it.

| `FLAG` | `CDATA` | `IDATA` | `RDATA` | Sites |
|-------:|:-------:|:-------:|:-------:|------:|
| -1, 0 | — | — | — | 2 |
| 1 | yes | — | — | 3 |
| 2 | — | yes | — | 21 |
| 3 | — | — | yes | 16 |
| 4 | — | yes | — | 6 |
| 5 | — | — | yes | 1 |
| 6 | — | yes | yes | 2 |
| 7 | — | yes | yes | 1 |

## Work

### 1. Change the declarations

```fortran
CHARACTER(LEN=*),  INTENT(OUT), OPTIONAL :: CDATA
INTEGER(kind=I_P), INTENT(OUT), OPTIONAL :: IDATA(N1, N2)
REAL(kind=R8P),    INTENT(OUT), OPTIONAL :: RDATA(N1, N2)
```

Keep the explicit shape `(N1, N2)`. It is correct, it documents the record
layout, and it is what makes the reads work. Do **not** change it to assumed
shape — see the impact assessment for why that breaks every rank-1 call site.

### 2. Guard every reference

Each `CASE` must check `PRESENT()` for the arguments it uses and raise a fatal
diagnostic if a required one is missing, rather than reading into nothing. Add
a contained helper next to the existing `throw_fatal`:

```fortran
SUBROUTINE require(ok, what)
   LOGICAL, INTENT(IN) :: ok
   CHARACTER(LEN=*), INTENT(IN) :: what
   IF (.NOT. ok) THEN
      WRITE (MSG, 9820) what, FLAG
      CALL throw_fatal(17, MSG)
   END IF
END SUBROUTINE
```

with a new format, e.g.
`9820 FORMAT('ALREAD called without its ', A, ' argument for FLAG=', I3)`.
Pick an error number not already used in this file — check the existing
`throw_fatal` calls (currently 3, 4, 5, 6, 7, 10, 11, 14, 16).

Then at the head of each case: `CALL require(PRESENT(IDATA), 'IDATA')`, and so
on for each argument that case uses.

### 3. Delete the placeholders at the call sites

Convert every call to keyword form for the data arguments it actually needs, so
the mapping is visible at the call site and cannot drift:

```fortran
! before
CALL ALREAD(2, VSD, FID_logfile, ':VS03', 4, 1, 0, CDUM, IDUM, DUMMY)
! after
CALL ALREAD(2, VSD, FID_logfile, ':VS03', 4, 1, 0, IDATA=IDUM)
```

Do this for all 52 sites. The table above tells you which argument survives at
each; regenerate the classification yourself rather than trusting it:

```bash
grep -rn --include="*.f90" "CALL ALREAD(" src
```

The three files are `src/subsurface/vs_input.f90` (30 sites),
`src/sediment/sy_input.f90` (16) and `src/io/spatial_fields.f90` (6).

### 4. Remove the placeholder variables that are now unreferenced

`CDUM` is passed at 48 sites and used at 3. Once the placeholders are gone,
several local declarations become dead — most importantly the `CHARACTER :: CDUM`
one-character placeholders. Delete every local that no longer has a reader.
The compiler will not tell you; grep each name in its host procedure.

Do **not** in this phase touch the `IDUM`/`DUMMY` arrays themselves even where
they become unreferenced at some sites — they are still live at others, and
resizing them is phases 4–7. Note in the commit message which ones became
unused, as input to those phases.

## Pitfalls

- `CDATA` is `CHARACTER(LEN=*)`. An absent optional assumed-length character
  dummy is fine, but do not attempt to query its length when absent.
- The epilogue `HEAD0_alread = HEAD` runs for every mode and must stay outside
  any `PRESENT()` guard.
- `src/io/spatial_fields.f90` passes `AEL(1, I2)` — an array *element* — as
  `RDATA`. That is legal sequence association and must stay legal. Do not
  "helpfully" change it to `AEL(:, I2)`; the dummy is still explicit-shape and
  the element form is what makes the offset work.
- Mode 2's and mode 3's reads are whole-array (`READ (IUNIT,*) IDATA`). They
  stay whole-array — the dummy remains explicit-shape, so this is correct.

## Expected result

- 103 arguments deleted across 52 call sites.
- The `:VS08c` non-conformance eliminated.
- No change in static storage yet (that is phases 4–7).
- No numerical difference on any of the 13 baselines.
