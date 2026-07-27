# Rank mismatch analysis for MNmod

Date: 2026-04-03
File analyzed: src/modules/MNmod.f90
Status: no code changes made

## Summary

The rank mismatch behavior is consistent with legacy argument association patterns in MNmod calls to utility routines in mod_load_filedata.

Why this can pass with ifx (Windows) but fail with gfortran (Linux):

- The code uses sequence-association style calls that pass base elements or scalar variables to dummies declared as arrays.
- With explicit module interfaces visible, gfortran tends to enforce rank rules more strictly.
- ifx is often more permissive for legacy forms in default builds, especially when warnings are not promoted to errors.
- The project configuration differs by platform/compiler, so diagnostics are not equivalent between Windows ifx and Linux gfortran.

Relevant build configuration:

- Linux defaults to gfortran: CMakeLists.txt:4
- Windows is restricted to Intel ifx: CMakeLists.txt:34
- gfortran debug flags include stricter warnings: CMakeLists.txt:393

## No-edit mapping table

This table maps suspicious call sites in MNmod to callee signatures and portability risk.

| ID | Caller site | Actual argument pattern | Callee signature | Why this is fragile | Portability risk |
|---|---|---|---|---|---|
| A1 | src/modules/MNmod.f90:1531 | `call alchk(..., uznow, nerr, ldum)` where `N0=1, N1=1` | `ALCHK(..., SUBJ(N0:N1), ...)` in src/util/mod_load_filedata.f90:445 | Scalar passed to rank-1 dummy. Some compilers accept legacy association, others reject rank mismatch with explicit interface. | High |
| A2 | src/modules/MNmod.f90:979 | `call alchk(..., dxqq(icol1), nerr, ldum)` where vector check spans `icol1:nel` | `ALCHK(..., SUBJ(N0:N1), ...)` in src/util/mod_load_filedata.f90:445 | Base-element sequence association to emulate a slice. Legacy but non-obvious; can be rejected depending on strictness/options. | Medium-High |
| A3 | src/modules/MNmod.f90:981 | `call alchk(..., dyqq(icol1), nerr, ldum)` | `ALCHK(..., SUBJ(N0:N1), ...)` in src/util/mod_load_filedata.f90:445 | Same pattern as A2. | Medium-High |
| A4 | src/modules/MNmod.f90:3859 | `call alredi(..., 1, 1, idum)` where `idum` is 1D workspace | `ALREDI(..., IDATA(N1,N2))` in src/util/mod_load_filedata.f90:1330 | 1D actual to rank-2 dummy. Storage-compatible in some legacy cases, but explicit interface may diagnose mismatch. | High |
| A5 | src/modules/MNmod.f90:3880 | `call alredf(..., ndata, 1, dummy)` where `dummy` is 1D workspace | `ALREDF(..., FDATA(N1,N2))` in src/util/mod_load_filedata.f90:1244 | Same as A4 but for real data. | High |
| A6 | src/modules/MNmod.f90:3583 | `call alredc(..., 1, 1, cdum)` where `cdum` is scalar/1-element usage | `ALREDC(..., CDATA(N1,N2))` in src/util/mod_load_filedata.f90:1173 | Character array rank expectations are strict with explicit interfaces. | High |
| A7 | src/modules/MNmod.f90:3867 | `call alalli(..., khelem(nlf+1), idum)` | `ALALLI(..., CATTYP(NLF+1:NEL), IDUM(:))` in src/util/mod_load_filedata.f90:267 | Passes element as base address to expected rank-1 section. Works in legacy sequence-association style but is compiler-sensitive. | Medium-High |
| A8 | src/modules/MNmod.f90:2019 | `call alintp(..., ncolmb(nlf+1), ..., cconc, cdpth, ..., cahum)` | `ALINTP(..., NCOLMB(NLF+1:NEL), ...)` in src/util/mod_load_filedata.f90:807 | Base-element passing for NCOLMB expected as vector segment. Usually accepted, still legacy-style and brittle with strict checks. | Medium |

Notes:

- Similar patterns repeat at other calls in MNmod for ALALLI/ALREDI/ALREDF/ALINTP.
- mntemp call to TRIDAG in MNmod appears rank-consistent with the TRIDAG assumed-shape interface in utilsmod.

## What is best to change (without changing functionality)

Recommended order is from lowest behavioral risk to highest confidence in portability.

1. Replace scalar-to-array checker calls with explicit 1-element work arrays.
- Example strategy: for `uznow`, assign `dums(1)=uznow` then call `alchk(..., dums, ...)`.
- This preserves logic exactly and removes rank ambiguity.

2. Replace base-element actuals with explicit array sections when callee expects vectors.
- Use `dxqq(icol1:nel)` instead of `dxqq(icol1)` where possible.
- Use `dyqq(icol1:nel)` similarly.
- This keeps values and index range the same, but is explicit and standard-conforming.

3. Add thin wrapper overloads in mod_load_filedata for scalar/singleton forms.
- Keep existing routines intact.
- Add wrapper entry points or generic interfaces that accept scalar or rank-1/rank-2 forms and forward to canonical implementations.
- This avoids touching many scientific call sites while making interface intent explicit.

4. For ALREDI/ALREDF/ALREDC one-value reads, use explicit rank-2 temporary arrays of shape (1,1).
- Read into temp(1,1), then assign to scalar/1D workspace.
- Same data path, safer rank semantics.

5. For ALALLI/ALINTP calls that pass `arr(nlf+1)`, use explicit slices for full expected ranges.
- Example: pass `khelem(nlf+1:nel)` and `ncolmb(nlf+1:nel)` where dummy expects those bounds.
- This documents intent and removes dependence on legacy sequence association.

## Suggested validation matrix after refactor

To confirm no functional change:

1. Build and run one baseline test case with ifx (Windows) before and after.
2. Build and run the same case with gfortran (Linux) before and after.
3. Compare key outputs (time series and summary files) with tolerance-based checks.
4. Enable stricter diagnostics in debug builds to catch residual interface issues early.

## Final conclusion

MNmod appears to rely on legacy, storage-sequence style argument association in several utility calls. That can be tolerated by ifx and rejected by gfortran depending on strictness and explicit interface checking. Converting those calls to explicit, rank-correct arguments is the safest portability fix and should not alter model functionality when done as above.
