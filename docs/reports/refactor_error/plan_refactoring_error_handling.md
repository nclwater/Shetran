# Error Handling & I/O Status Refactoring Plan (src/)

## Summary

This document analyzes the usage of central error functions, warnings, and I/O status handling in `src/` and proposes a phased plan to unify and modernize them.

### Key references

- Central implementation and globals: `src/parameters/sglobal.f90` (`sglobal.ERROR`, `sglobal.ALSTOP`, `sglobal.ERRC`, `sglobal.ERRTOT`, `sglobal.ISERROR`, `sglobal.ISERROR2`)
- I/O and refactor examples:
  - `docs/reports/refactor_goto/report_goto_rest_refactored.md` (IOSTAT replacement of `END=`)
  - `docs/reports/refactor_goto/report_goto_getdirqq.md`
  - `docs/reports/refactor_io/plan_io_refactor.md`
- Higher-level guidance:
  - `docs/reports/modernisation/FORTRAN_MODERNIZATION_REPORT.md`
  - `docs/reports/plan_refactoring.md`

---


## Current state

The project currently uses a mix of legacy and partially-modernized error and I/O patterns concentrated around a single, monolithic error routine in the `sglobal` module. The summary below expands on the previous high-level points and documents specific behaviours, code patterns, and pain points to guide the refactor.

- Central error facility (sglobal)
  - A single subroutine, `ERROR`, in `src/parameters/sglobal.f90` provides formatted messages, error counting, optional display of help files, and termination for fatal errors. `ALSTOP` is used to stop the program on fatal conditions.
  - Error bookkeeping (arrays `ERRC`, counter `ERRTOT`) and summary printing are implemented inside `sglobal`, coupling counting, formatting, help lookup and termination behaviour together.
  - `ERROR` accepts an integer `ETYPE` (conventionally `FFFATAL`, `EEERR`, `WWWARN`) rather than a typed severity. This integer-based API is widely used across `src/`.

- Typical error handling patterns in the codebase
  - Direct calls to `ERROR(...)` and `ALSTOP(...)` are pervasive; many modules `USE sglobal` to access these.
  - Some modules use ad-hoc `WRITE` followed by `STOP` instead of centralized reporting.
  - Conditional GOTOs and labelled error-handling are still present in legacy files; refactored files progressively replace those with `IOSTAT` checks and explicit calls to `ERROR`.

- I/O and IOSTAT usage
  - Mixed approaches across files:
    - Newer, refactored code uses `IOSTAT=` with conditional handling.
    - Older code frequently uses `END=`/`ERR=` with label-based control flow.
    - Some reads/writes assume fixed unit numbers and do not always check IOSTAT thoroughly.
  - `NEWUNIT` is not used consistently: several OPEN statements still pass hard-coded unit numbers, creating potential for unit clashes or platform issues.

- Help-file and path handling
  - `ERROR` contains logic to construct help-file paths, open help files and optionally prompt the user (press Enter to continue). This blends user I/O interaction with error reporting.
  - Hard-coded help path fragments appear (for example `'/helpmessages'`); lookup code uses string concatenation with `rootdir` and `dirqq` in place.
  - There is no single configurable canonical help directory API; modules replicate variations of path logic.

- Behavioural special cases
  - Certain error numbers set global flags (e.g., `ISERROR` for errors 1024/1030 and `ISERROR2` for 1060) which trigger timestep reduction logic elsewhere. This couples specific numeric error codes to simulation control flow.
  - `ERROR` prints a final "asummary" and attempts to show help messages when `ETYPE==FFFATAL` or `ERRNUM==0`, which can perform file I/O during error handling — risky in constrained or parallel environments.

- Testability and reproducibility concerns
  - Tight coupling of diagnostics, I/O and termination makes unit testing of error reporting difficult.
  - Changing `ERROR` behaviour risks altering simulation outputs (e.g., when `ALSTOP` is invoked), so refactor must preserve semantics.

- Concurrency and robustness
  - The codebase is not designed for concurrent or reentrant error handling: global counters and I/O during error handling are not thread-safe.
  - Lack of consistent use of `IOSTAT` and `NEWUNIT` increases the chance of resource leaks or unclosed files after errors.

- Summary of pain points to address
  - Coupling: formatting, counting, help lookup, and stopping logic are mixed and widely imported.
  - Inconsistent I/O patterns: `END=`/`ERR=`/label GOTOs still present alongside `IOSTAT` usage.
  - Fixed unit numbers: manual management of units is error-prone.
  - Semantic coupling of numeric error codes to non-reporting behaviour (e.g., timestep reduction) complicates refactor.
  - Help-file access and user prompting during errors is implemented in ad-hoc ways and duplicated in places.

This more detailed view should drive the refactor goals: separate concerns (reporting vs. counters vs. termination), provide robust I/O helpers, centralize help-path configuration, and preserve existing behaviours (including side-effects tied to particular error codes) while enabling incremental migration and improved testability.

---

## Problems & risks (detailed and nuanced)

- Centralisation vs. legitimate globals
  - sglobal currently contains a mix of concerns: numerical/formatting constants (marker999, zero, one, izero1, etc.), small elemental utility functions (iszero, isone, idimje, dimje), temporary buffers (EARRAY, text32), and the monolithic ERROR/ALSTOP logic. Some of these globals (constants and small pure/elemental utils) are legitimately shared and reduce duplication; treating every `USE sglobal` as harmful would be incorrect.
  - Recommendation: preserve the constants and pure/utilities in a small, stable "constants/utils" module while extracting stateful/error-reporting and I/O behaviour into separate modules. This minimises churn for callers that only need numeric constants or pure helpers.

- Coupling and dependency surface
  - Current coupling arises when many modules `USE sglobal` to access ERROR or flags but also pull in unrelated symbols. This can mask the true dependency graph and make targeted changes harder.
  - Impact: refactors that change ERROR semantics or add I/O behaviour require broad recompilation and careful regression testing.
  - Mitigation: introduce fine-grained modules (e.g., sglobal_constants, sglobal_utils, error_handling) and provide compatibility shims so modules that only need constants do not depend on the error subsystem.

- Behavioural coupling (error codes → control flow)
  - Specific numeric error codes in ERROR set global flags (ISERROR, ISERROR2) and cause side-effects (timestep reduction). This implicit coupling between numeric codes and runtime control flow is brittle and hard to track.
  - Mitigation: make these side-effects explicit in a dedicated control API (e.g., notify_simulation_control(code)), or map special codes to enumerated events so code paths depending on them can be refactored with tests.

- I/O during error handling and help-file logic
  - ERROR performs file I/O (help lookup, printing help messages) and user prompts; doing I/O while handling other I/O failures or in parallel/daemonized runs is risky.
  - Impact: deadlocks, cascading failures, non-deterministic outputs in parallel runs, and difficulty testing.
  - Mitigation: separate message formatting and side-effecting help-file display; provide a configurable policy (log-only, interactive, panic) and a pluggable backend for help-message retrieval.

- Hard-coded paths and platform assumptions
  - Hard-coded strings like '/helpmessages' and PATH1 = '/shetran/' appear in the codebase. These assume POSIX-style separators and locations.
  - Mitigation: centralise path configuration and use platform-aware utilities; avoid hard-coded roots in error/reporting modules.

- Unit and I/O management inconsistency
  - Fixed unit numbers and mixed use of IOSTAT vs END=/ERR= create risks of unit clashes, leaked handles, and unhandled I/O errors.
  - Mitigation: promote NEWUNIT usage and safe_open wrappers; replace END=/ERR= patterns with explicit IOSTAT handling.

- Testability and reproducibility
  - Monolithic ERROR with side-effects makes unit testing and deterministic regression testing difficult. Fatal behaviour (ALSTOP) can abort test runs unless wrapped.
  - Mitigation: make ERROR behaviour injectable (logger backend, fatal handler) and provide a test-mode that records messages without stopping execution.

- Concurrency and reentrancy
  - Global counters, shared temp buffers (EARRAY), and file I/O in ERROR are not thread-safe or reentrant.
  - Impact: unsafe in future parallelised or multi-threaded code.
  - Mitigation: design the new error module to avoid mutable globals where possible; protect shared state or require callers to serialize access; prefer returning status objects over global counters.

- Migration and regression risk
  - Any change to ERROR, ALSTOP, or the semantics of special error codes can alter program control flow or outputs.
  - Mitigation: preserve public signatures via thin shims, add comprehensive unit and integration tests, and validate using canonical example runs after each migration step.

- Operational and UX risks
  - Interactive prompts during error handling are inappropriate for batch/CI runs.
  - Mitigation: allow non-interactive mode controlled by environment or configuration.

- Summary of concrete short-term actions to reduce risk
  1. Audit all `USE sglobal` sites and classify whether they need constants, pure utils, or ERROR/ALSTOP.
  2. Introduce separate modules for constants/pure utils and for error handling; implement shims in sglobal to preserve compatibility.
  3. Extract help-file lookup and interactive behaviour out of ERROR into a pluggable helper with configurable policy.
  4. Replace hard-coded paths with a central configuration interface.
  5. Replace fixed-unit I/O with NEWUNIT + IOSTAT wrappers incrementally.
  6. Add tests that assert: message formatting, counter bookkeeping, special-code side-effects, and that fatal behaviour is observable but testable (e.g., via injected fatal handler).

---

## Goals

1. Centralize error reporting and logging in a dedicated module with a small, documented API.
2. Standardize severity levels and the error-code schema.
3. Provide robust I/O helpers using `NEWUNIT` + `IOSTAT`.
4. Preserve current behavior (no algorithmic changes) and enable incremental migration.
5. Improve testability (formatting, help lookup, I/O helpers).
6. Provide backward-compatible shims for gradual transition.

---

## Proposed modules

- `src/error/error_handling.f90`
  - API examples:
    - `report_error(severity, code, component, context, message)` — record, print, optionally fatal
    - `record_error(code, module_idx, count_only)` — update counters
    - `print_error_summary(unit)`
    - `set_help_path(path)`
    - `load_help_message(code)` → string or status
    - `fatal_abort(code)` — termination shim
  - Constants: severity `PARAMETER`s (e.g., `ERR_FATAL`, `ERR_ERROR`, `ERR_WARN`)
  - Data: counters, help path, configurable output unit
  - Backwards-compatible wrappers:
    - `ERROR(...)` and `ALSTOP(...)` that delegate to the new API

- `src/io/io_utils.f90`
  - Helpers:
    - `safe_open(file, newunit, iomsg, action)` — uses `NEWUNIT` + `IOSTAT`, returns unit
    - `safe_read(unit, fmt, iostat, ...)` — standardized read pattern
    - `check_iostat(iostat, context, code)` — map `IOSTAT` to `report_error`
    - Path utilities (`join`, `exists`) using `LENGTH_FILEPATH`
  - Ensure all I/O uses `IOSTAT` (no `END=`/`ERR=` jumps)

---

## Migration plan (phased)

### Phase 0 — Design & tests (1–2 weeks)

- Design new modules and APIs; document in FORD style.
- Add unit tests for formatting, counters, help-file loading and `safe_open`.
- Add CMake test targets.

### Phase 1 — Create modules & shims (≈2 weeks)

- Implement `src/error/error_handling.f90` and `src/io/io_utils.f90`.
- Implement wrapper subroutines matching `sglobal.ERROR` and `sglobal.ALSTOP` signatures that delegate to the new modules.
- Update build order (CMake) so new modules compile early.

### Phase 2 — Migrate I/O patterns (incremental, per module) (4–8 weeks)

For each module (prioritise I/O-heavy first):

1. Replace `OPEN`/`READ` sites with `safe_open` and `READ(..., IOSTAT=iost)`.
2. Replace `END=`/`ERR=` clauses and GOTOs with `if (iost /= 0) then ...` or calls to `report_error`.
3. Replace direct `WRITE`/`STOP` flows with `report_error(ERR_FATAL, ...)` or `fatal_abort(...)`.

Keep wrappers to preserve behavior until fully migrated. Run compilation and regressions after each module migration.

### Phase 3 — Centralize help-file handling & summaries (≈2 weeks)

- Move help-file lookup and display into `error_handling`.
- Replace direct file reads in modules with `load_help_message(code)`.

### Phase 4 — Remove legacy code and harden (2–4 weeks)

- Once coverage is complete, remove the monolithic implementation from `sglobal` or convert it to a thin shim that `USE`s `error_handling`.
- Enforce `IMPLICIT NONE` and add static checks.
- Replace hard-coded units with `NEWUNIT` wrappers.

### Phase 5 — Tests, docs & roll-out (ongoing)

- Add unit and integration tests per `docs/reports/plan_refactoring.md` Phase 10.
- Update FORD docs.
- Validate outputs against canonical examples to ensure no regressions.

---

## Concrete examples

- Legacy read with `END=`:

Before:
```fortran
READ (TAH, *, END = 383) (tahigh(I), I = 1, NM)
```

After:
```fortran
READ (TAH, *, IOSTAT=iost) (tahigh(I), I = 1, NM)
IF (iost < 0) THEN
  CALL report_error(ERR_WARN, 1234, 'METIN', 'TAH EOF -> using defaults')
  DO I=1,NM
    tahigh(I) = 10.0
  END DO
END IF
```

- Legacy error label jump:

Before:
```fortran
IF (cond) GOTO 8110
...
8110 WRITE(...); CALL ERROR(...)
```

After:
```fortran
IF (cond) CALL handle_invalid_nsed(NSED, NSEDEE, SPR)
! or
IF (cond) CALL report_error(ERR_FATAL, 2006, 'COMP', 'Invalid NSED')
```

---

## Acceptance criteria

- Examples in `examples/` build and produce identical outputs (where expected) after migration.
- No remaining uses of legacy `END=`/`ERR=` + labels in `src/` (except compatibility stubs).
- All file opens use `NEWUNIT`-based `safe_open` or have justification.
- Error summary and help-file display preserved and centralized.
- New module has unit tests covering formatting, counters, help lookup and I/O error mapping.

---

## Backout / rollback

- Keep `sglobal` error function as a compatibility shim until migration completes.
- Use feature branches per migration chunk for safe rollback.

---

## References

- `src/parameters/sglobal.f90`
- `docs/reports/refactor_goto/report_goto_rest_refactored.md`
- `docs/reports/refactor_goto/report_goto_rest.md`
- `docs/reports/refactor_goto/report_goto_getdirqq.md`
- `docs/reports/refactor_goto/report_goto_symod.md`
- `docs/reports/refactor_io/plan_io_refactor.md`
- `docs/reports/modernisation/FORTRAN_MODERNIZATION_REPORT.md`
- `docs/reports/plan_refactoring.md`

---

## Next actionable steps

1. Create `src/error/error_handling.f90` and `src/io/io_utils.f90` prototypes with a minimal API.
2. Implement unit tests for help-file lookup and report formatting.
3. Implement `ERROR` and `ALSTOP` wrappers in `sglobal` delegating to the new module.
4. Migrate 2–3 modules with known I/O patterns (e.g., meteorological input / rest) to validate patterns and tests.

## Notes

- Maintain strict "no algorithmic changes" during migration; only change error/I/O handling patterns.
- Keep the migration incremental and validated by compiling and running the project's example simulations after each module change (see testing plan: [docs/reports/plan_refactoring.md](docs/reports/plan_refactoring.md) Phase 10).

---

