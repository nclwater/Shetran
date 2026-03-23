# Heap/Stack Memory Analysis: Cobres (backup_input)

## 1. Context & Basis
This note estimates heap and stack memory for the Cobres example model using input files under [examples/Cobres/backup_input](examples/Cobres/backup_input).

**Parsed model sizes (Cobres):**
- `nx=17`, `ny=29` from [examples/Cobres/backup_input/input_cob_frd.txt](examples/Cobres/backup_input/input_cob_frd.txt)
- Active elements `nel=176` from the computational grid mask in the same file
- `ncetop=35` from `:VS03 NCSZON` in [examples/Cobres/backup_input/input_cob_vsd.txt](examples/Cobres/backup_input/input_cob_vsd.txt)

Assumptions:
- `DOUBLEPRECISION = 8 bytes`, `INTEGER = 4 bytes`
- Estimated sizes are per allocation, not total process memory

---

## 2. Heap Allocations (Local ALLOCATABLE) — Ranked by Size
These are local, dynamically allocated arrays (heap) that are large enough to matter for Cobres.

### 2.1 `MNmod::mncont`
Allocates ~50 arrays of shape `(nel, ncetop)`.
- **Estimated size:** ~2.35 MB
- **Location:** [src/modules/MNmod.f90](src/modules/MNmod.f90#L330)
- **Notes:** A single call allocates many arrays; total footprint grows as $O(nel \cdot ncetop)$.

### 2.2 `OCmod::OCSIM`
Allocates several large matrices sized by $NX*4$ and $NY$.
- **Estimated size:** ~1.24 MB
- **Dominant term:** `EE(NX*4, NX*4, NY)` ~1.02 MB
- **Location:** [src/modules/OCmod.f90](src/modules/OCmod.f90#L1617), [src/modules/OCmod.f90](src/modules/OCmod.f90#L1622)

### 2.3 `MNmod::mnedth`
Allocates ~15 arrays of shape `(nel, ncetop)`.
- **Estimated size:** ~0.70 MB
- **Location:** [src/modules/MNmod.f90](src/modules/MNmod.f90#L3319)

### 2.4 `visualisation_interface_right::send_pass`
Allocates `iel(nel)` and `dum(nx,ny)` or `dum(nel,4)`.
- **Estimated size:** < 0.01 MB
- **Location:** [src/visualisation/visualisation_interface_right.f90](src/visualisation/visualisation_interface_right.f90#L321)

### 2.5 `visualisation_hdf5::write_mn` (conditional)
The local `surf_elv` is only allocated if the item name is `surf_elv`.
- **Cobres plan:** `surf_elv` is not in [examples/Cobres/backup_input/input_cob_visualisation_plan.txt](examples/Cobres/backup_input/input_cob_visualisation_plan.txt), so this is **not allocated** for Cobres.
- **Potentially huge:** if enabled, it is a 6D grid-sized buffer.
- **Location:** [src/visualisation/visualisation_hdf5.f90](src/visualisation/visualisation_hdf5.f90#L294)

---

## 3. Stack-Resident Arrays (Automatic/Static Locals)
These can land on the stack (or static storage if the compiler decides), and are worth tracking for stack overflow risk.

### 3.1 `visualisation_metadata::read_mask::mask_write`
`cc` is an automatic local array sized to the mask dimensions.
- **Location:** [src/visualisation/visualisation_metadata.f90](src/visualisation/visualisation_metadata.f90#L1169)
- **Cobres mask size:** 29 x 17 = 493 characters (~0.5 KB)
- **Risk:** small for Cobres, but can be large for bigger masks because it scales with `SIZE(m%ma)`

### 3.2 Grid-sized `IDUM(NXEE*NYEE)` locals
These appear in multiple parsing routines.
- **Examples:** [src/util/mod_load_filedata.f90](src/util/mod_load_filedata.f90#L87), [src/modules/CMmod.f90](src/modules/CMmod.f90#L105), [src/modules/SYmod.f90](src/modules/SYmod.f90#L1203)
- **Cobres size:** `NXEE*NYEE = 17*29 = 493` integers (~2 KB)
- **Risk:** low for Cobres; higher for large grids

### 3.3 `run_sim::simulation` local `hrf(nelee)`
- **Location:** [src/modules/run_sim.f90](src/modules/run_sim.f90#L76)
- **Cobres size:** `nelee` ~ `nel` -> ~1.4 KB (DOUBLEPRECISION)

---

## 4. Why a Memory Exception at `read_mask::mask_write` Line 1170?
The reported crash location matches the stack-based automatic array `cc` in `mask_write`:
- **Line:** [src/visualisation/visualisation_metadata.f90](src/visualisation/visualisation_metadata.f90#L1169)
- **Statement:** `CHARACTER, DIMENSION(SIZE(m%ma,DIM=1),SIZE(m%ma,DIM=2)) :: cc`

### Most plausible memory-exception pathways
1. **Stack overflow from huge mask dimensions**
   - `cc` is an automatic array placed on the stack by default.
   - If `m%ilow:m%ihigh` and `m%jlow:m%jhigh` are very large, `cc` becomes huge and can overflow the stack.
   - This is consistent with an access violation on Windows and a crash in `mask_write`.

2. **Incorrect mask bounds leading to massive allocation**
   - `read_mask` uses `ALLOCATE(m%ma(m%ilow:m%ihigh, m%jlow:m%jhigh))` based on file input.
   - If the mask bounds are corrupted or parsed incorrectly, `m%ma` can be massive.
   - Even if heap allocation succeeds (or partially succeeds), the follow-up automatic stack array `cc` will overflow.

3. **Memory exhaustion in HDF5/visualisation path**
   - The visualisation metadata is read early, before many runtime buffers are freed.
   - A large mask can stack with existing heap allocations (e.g., HDF5 buffers), increasing the chance of an access violation.

### Why Cobres might still trigger it
Cobres itself uses small masks (29x17), so a crash at `mask_write` suggests:
- The **mask bounds were not read correctly**, resulting in a wildly oversized `m%ma`.
- Or **stack is already corrupted** before `mask_write` and this line is just the first place the runtime catches it.

---

## 5. Observations & Practical Checks
- For Cobres, **heap allocations are moderate** (< 3 MB for the largest locals).
- **Stack risk** in Cobres is low for correct input, but the `mask_write` automatic array becomes dangerous if bounds are wrong.
- If this crash is reproducible, focus on validating mask bounds on input and confirming that `read_mask` sees the expected `jlow/jhigh/ilow/ihigh` values.

---

## 6. Suggested Debug Targets (Aligned with `analysis_crash.md`)
1. **Log mask bounds** immediately before the `ALLOCATE(m%ma...)` call.
2. **Disable or guard `mask_write`** for very large masks to avoid stack overflow.
3. **Confirm `/heap-arrays0`** or equivalent flags to move large automatic arrays off the stack.
4. **Validate visualisation plan files** to ensure mask lines are not malformed.

If you want, I can add a small guard in `read_mask` to avoid allocating `cc` on the stack for large masks, or refactor `mask_write` to stream output without a temporary full-sized array.
