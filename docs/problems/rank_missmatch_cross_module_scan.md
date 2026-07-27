# Cross-module rank mismatch risk scan (quick)

Date: 2026-04-03
Scope: src/modules only (active code path), no edits
Related note: docs/problems/rank_missmatch_mnmod.md

## Executive summary

The argument-association pattern seen in MNmod is present in several other modules. The same portability risk exists where:

- compute code passes scalars or base elements to utility dummies declared as arrays
- compute code passes 1D storage to utility dummies declared rank-2
- compute code relies on sequence association instead of explicit array sections

Most affected modules in src/modules are:

1. src/modules/MNmod.f90
2. src/modules/CMmod.f90
3. src/modules/SYmod.f90
4. src/modules/OCmod.f90
5. src/modules/FRmod.f90

## Quick impacted-file map

| Priority | File | Why impacted | Example lines |
|---|---|---|---|
| P1 | src/modules/MNmod.f90 | Heavy use of ALREDI/ALREDF/ALREDC, ALALLI, ALINTP plus ALCHK/ALCHKI with scalar/base-element actuals | src/modules/MNmod.f90:1531, src/modules/MNmod.f90:3867, src/modules/MNmod.f90:2019 |
| P1 | src/modules/CMmod.f90 | Similar ALRED*/ALALLI usage patterns (rank-2 utility dummies fed scalar/1D forms) | src/modules/CMmod.f90:263, src/modules/CMmod.f90:282 |
| P1 | src/modules/SYmod.f90 | Extensive ALCHK/ALCHKI calls over ranges, relying on array storage association conventions | src/modules/SYmod.f90:1434, src/modules/SYmod.f90:1438 |
| P2 | src/modules/OCmod.f90 | ALCHK/ALCHKI range checks in same style as SY/MN | src/modules/OCmod.f90:811, src/modules/OCmod.f90:827 |
| P2 | src/modules/FRmod.f90 | ALINTP call path with base-element style arguments | src/modules/FRmod.f90:4047 |

## Utility routines that are central to the issue

These are the highest leverage subroutines because many modules call them:

- src/util/mod_load_filedata.f90:445 (ALCHK)
- src/util/mod_load_filedata.f90:608 (ALCHKI)
- src/util/mod_load_filedata.f90:1173 (ALREDC)
- src/util/mod_load_filedata.f90:1244 (ALREDF)
- src/util/mod_load_filedata.f90:1330 (ALREDI)
- src/util/mod_load_filedata.f90:267 (ALALLI)
- src/util/mod_load_filedata.f90:807 (ALINTP)

## Where to fix: utility side vs compute side

### Option A: fix utility side first (recommended first step)

What this means:

- keep existing compute call sites mostly unchanged
- add compatibility wrappers / generic interfaces in utility module for scalar, rank-1, and rank-2 forms
- route wrappers to canonical implementations

Pros:

- fastest broad risk reduction across many modules
- lower churn in scientific kernels
- easier to validate because behavior remains centralized

Cons:

- preserves legacy calling style in compute modules
- less explicit interfaces at call sites
- may defer full standards-cleanup work

### Option B: fix compute side first

What this means:

- update each call site to explicit rank-correct arguments
- replace scalar and base-element passing with explicit temporary arrays or explicit sections

Pros:

- strongest long-term standards compliance and readability
- clearest intent at each scientific call site

Cons:

- much larger patch surface
- higher short-term regression risk
- slower to complete and review

### Best overall strategy

Use a staged hybrid:

1. Utility-first compatibility hardening (A) to quickly remove compiler portability blockers.
2. Then incremental compute-side cleanup (B) per module (MN -> CM/SY -> OC/FR) to converge on fully explicit, standard-conforming calls.

This gives rapid cross-platform stability now, while still moving toward cleaner code.

## Minimal-risk refactor patterns

1. Scalar checker input to ALCHK/ALCHKI:
- use a local 1-element array and pass that array.

2. Vector checker calls using base element:
- pass explicit sections, e.g. arr(i0:i1), when dummy expects a vector range.

3. ALREDI/ALREDF/ALREDC single-value reads:
- read into local (1,1) temporary rank-2 arrays, then assign to scalar or rank-1 storage.

4. ALALLI/ALINTP calls with arr(nlf+1):
- pass explicit slices matching declared dummy bounds.

## Suggested rollout order

1. Utility wrappers in src/util/mod_load_filedata.f90
2. MNmod cleanup (highest concentration of patterns)
3. CMmod and SYmod
4. OCmod and FRmod
5. Optional: remove wrappers once all call sites are explicit and validated
