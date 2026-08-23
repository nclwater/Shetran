# Logical analysis: `OCQDQMOD` — correctness, performance, memory/variable management

## Scope and method

Source-only assessment of `src/modules/OCQDQMOD.F90` (407 lines, of which the
working routine `OCQDQ` is `:149-359`). No profile was taken and no timings were
measured. Every claim is derived from reading the module together with:

- its callees' dummy-argument declarations in `src/modules/OCmod2.f90`
  (`OCQBC:636`, `OCQBNK:837`, `OCQGRD:1002`, `OCQLNK:1157`, `OCQMLN:1330`,
  `OCNODE:262`, `FNODE:403`, the `gethrf`/`setqsa` accessors `:94-148`,
  `OCFIX:1709`),
- its only caller, `OCmod.f90:2122` (`OCSIM`), and the two `DQIST2` consumers
  `OCmod.f90:499-509` and `:2227-2237`,
- the topology construction in `src/modules/FRmod.f90:780-980` (the `ICMRF2`
  branch records) and `:1129-1170` (the reciprocal-face fixup),
- the array declarations in `src/parameters/AL_C.F90`, `AL_D.f90`, `AL_G.F90`
  and `sglobal.f90`.

Where a claim depends on compiler behaviour rather than on the source alone,
that is stated.

This document does **not** re-report findings already owned by
`analysis_ocmod2.md` from the callee side. Where a
finding here is the caller-side half of one of those, it says so and stops
there. In particular `analysis_ocmod2.md` §M5 already verified every
sequence-association site in this file (`COCBCD(1,IBC)`, the `(0:3)`-actual /
`(0:1)`-dummy passes); those are not re-audited.

Relevant compiled extents:

| Constant | Value | Source |
|---|---|---|
| `NELEE` | 250 000 | `sglobal.f90:118` |
| `NLFEE` | 20 000 | `sglobal.f90:117` |
| `NOCTAB` | 20 | `sglobal.f90:128` |

Element numbering, used throughout below: elements `1 .. total_no_links` are
channel links and `NGDBGN = total_no_links + 1` onwards are grid and bank
elements (`FRmod.f90:709`, `OCmod.f90:1815`). So `KEL <= total_no_links` is
exactly the "is a link" test the routine uses.

---

## Conclusion up front

`OCQDQ` is a dispatcher. It performs essentially no floating-point arithmetic of
its own — one division and a handful of comparisons per participant — and hands
all the physics to `OCmod2`. Its cost is therefore **address generation and
cache/TLB traffic**, not flops, and there is exactly one performance lever with
real magnitude: the `(element, face)` array layout. Micro-optimising the body
would be measuring the wrong thing.

Four findings dominate.

1. **The surface-storage override is missing on the external-boundary path.**
   The negative-`STRXX` marker is honoured in `single_data_loop` (`:223-229`)
   and `multi_data_loop` (`:316-322`) but not at `:190`, where the boundary
   branch passes `FSTR(ielu, IFACE)` straight through. A flagged element with a
   prescribed-head or weir-plus-river boundary on face 1 or 3 therefore passes a
   **negative Strickler coefficient** into `OCQBC`, which reverses the sign of
   the boundary flow and corrupts the diagonal Jacobian entry — silently, at a
   physically plausible magnitude. Finding **C1**. This is the only finding here
   that is a live defect rather than a latent one.

2. **The `(element, face)` layout costs ~31 cache lines per element to move ~248
   bytes.** `DHF`, `DQ0ST`, `DQIST`, `qsazz` and eight `ICMREF` columns are all
   dimensioned `(NELEE, ·)`, so the four faces of one element are 1-2 MB apart
   in every one of them. `OCQDQ`'s element loop runs **≈31 concurrent sequential
   streams**, past what an L2 stream prefetcher tracks, at roughly 12 % cache
   line utilisation. Transposing to `(4, NELEE)` and making `ICMREF`
   element-contiguous takes that to ~12 lines and ~12 streams. Finding **P1** —
   the same coordinated face-layout change `analysis_ocmod2.md` **M2**
   recommends, and this module is the strongest argument for it because it is
   the one place all five array objects are indexed by `(element, face)` in the
   same inner loop.

3. **`jxswork` is never initialised and is conditionally assigned.** Declared at
   `:156` with no initialiser, written only for participants with
   `KEL <= total_no_links` (`:234`, `:327`), and read unconditionally by
   `OCQMLN` as `XSTAB(:, :, JXSWORK(J))` for every active branch. The invariant
   that holds it up — confluence participants are always links — is real
   (`FRmod` fills `ICMRF2` only from `LINKNO`) but is nowhere stated or checked,
   and the failure mode is an unbounded index into a rank-3 array. Finding
   **C3**.

4. **The two gather loops are byte-identical duplicates** (`:212-235` vs
   `:305-328`, differing only in `JMAX`). That duplication is not a style
   complaint: it is the direct cause of **C1**, where the same eight lines were
   needed in a third place and were not written. Finding **P4**.

The `MOD(J+JJ,4)` confluence-derivative scatter at `:348` — by far the most
opaque line in the file — was traced through `FRmod`'s four branch-record blocks
and is **correct**. §5 records the proof so it is not re-derived.

---

## 1. Correctness

### C1 — The surface-storage override is missing on the external-boundary path (`:187-203`)

This is a live defect.

`single_data_loop` and `multi_data_loop` both reinterpret a negative `STRXX` as
a millimetre-scale ponding-depth marker and substitute a fixed effective
roughness:

```fortran
STR(J) = FSTR(KEL, KFACE)

! surface storage (sb 1905022)
IF (STRXX(KEL) < 0.0d0) THEN
   IF ((GETHRF(KEL) - ZGRUND(KEL)) < (-STRXX(KEL) / 1000.0d0)) THEN
      STR(J) = 0.5d0
   ELSE
      STR(J) = 2.0d0
   END IF
END IF
```

The boundary branch has no equivalent:

```fortran
IF (NFACE == IFACE) THEN
   W = FDQQ(ielu, IFACE)
   STR(0) = FSTR(ielu, IFACE)        ! <-- no marker test
   ...
   CALL OCQBC(NTYPE, DHF(ielu, IFACE), ZGRUND(ielu), STR(0), W, ...)
```

`FSTR` returns `STRXX(jel)` for faces 1 and 3 (`:377-381`). So for an element
carrying the marker, `STR(0)` is the raw negative value.

**Where it lands.** `OCQBC` uses `STR` in Part 2, gated on
`MTYPE == 3 .OR. NTYPE == 8` (`OCmod2.f90:727`), i.e. boundary types 3 (grid
prescribed head), 9 (channel prescribed head) and 8 (weir with river in
parallel). Type 3 takes `STRW = STR * W; CALL CONVEYAN(STRW, HM, CONVM, DERIVM, 1)`;
types 8 and 9 take `CALL OCCODE(ZGI, STR, W, ...)`. Both are linear in `STR`, so
a negative `STR` gives `CONVM < 0` and `DERIVM < 0`, and then
(`OCmod2.f90:746-747`):

```fortran
FROMQ  = FROMQ  + SIG * CONVM  * ROOTDZ / ROOTL
FROMDQ = FROMDQ + (SIG * DERIVM * ROOTDZ - DUM) / ROOTL
```

For `MTYPE == 3`, Part 1 leaves `FROMQ = ZERO`, so the whole boundary flow is
this term and it is **exactly sign-reversed**: the boundary drives water toward
the higher head instead of away from it. `FROMDQ` is worse than reversed — with
`DERIVM < 0` and `DUM < 0` the two contributions have opposite signs, so the
diagonal entry `DQ0ST(ielu, IFACE)` can come out positive, which is the wrong
sign for the tridiagonal OC solve to be diagonally dominant.

**Why it is silent.** The marker is a depth in millimetres, so a 50 mm storage
threshold is `STRXX = -50` and `|STR| = 50` — squarely inside the ordinary
Strickler range of 20-80. Nothing is out of scale, nothing NaNs, no warning
fires. The flow is of entirely plausible magnitude and points the wrong way.

**Reachability.** Needs an element with the negative-`STRXX` marker that also
carries an OC boundary condition of type 3, 8 or 9, on face 1 or 3 — faces 2
and 4 read `STRYY`, which is not used as a marker, so they are unaffected. A
ponding cell at a domain edge with a prescribed stage is an ordinary
configuration.

**Fix.** Extract the override into one place and call it from all three paths:

```fortran
PURE FUNCTION eff_str(kel, kface, z) RESULT(r)
   INTEGER, INTENT(IN) :: kel, kface
   DOUBLE PRECISION, INTENT(IN) :: z   ! water level, already loaded
   DOUBLE PRECISION :: r
   IF (strxx(kel) < zero) THEN
      r = MERGE(0.5d0, 2.0d0, (z - zgrund(kel)) * 1000.0d0 < -strxx(kel))
   ELSE
      r = fstr(kel, kface)
   END IF
END FUNCTION
```

That is also **P2** and **P4** below: it removes the redundant `GETHRF`, the
division, and the duplication in one change.

### C2 — `multi_scatter_loop` guards on `== 0` where every other guard uses `< 1` (`:335`)

```fortran
multi_data_loop: DO J = 0, JMAX
   KEL = JEL2(J)
   IF (KEL < 1) CYCLE multi_data_loop      ! :307
...
multi_scatter_loop: DO J = 0, JMAX
   KEL = JEL2(J)
   IF (KEL == 0) CYCLE multi_scatter_loop  ! :335
```

`OCQMLN` uses `IF (JEL2(J) <= 0)` for the same test (`OCmod2.f90:1358`,
`:1375`). The scatter loop is the odd one out. If `ICMRF2` ever held a negative
participant, `multi_data_loop` and `OCQMLN` would both treat it as inactive
while `multi_scatter_loop` would treat it as active and execute

```fortran
CALL SETQSA(KEL, KFACE, QJ(J))     ! qsazz(negative, stale) = ...
DQ0ST(KEL, KFACE) = DQIJ(J, J)
```

— two out-of-bounds writes with a stale `KFACE` (see **C4**).

Not reachable today: `FRmod:809-971` only ever writes `LINKNO` results into
`ICMRF2`, and unwritten slots stay at the BSS zero. So this is a one-character
divergence from the two guards that bracket it, not a bug. Close it anyway; it
costs nothing and it is the kind of asymmetry that a later change to the
absent-branch encoding would turn into a silent memory scribble.

### C3 — `jxswork` is uninitialised and conditionally assigned (`:156`, `:231-234`, `:324-327`)

```fortran
INTEGER :: jxswork(0:3)                  ! :156 — no initialiser
...
IF (KEL > total_no_links) CYCLE multi_data_loop
CW(J) = CWIDTH(KEL)
XA(J) = XAFULL(KEL)
jxswork(J) = KEL                          ! :327 — links only
```

`OCQMLN` then reads it for every active branch, with no guard
(`OCmod2.f90:1365`, `:1385`):

```fortran
CALL OCCODE(ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, JXSWORK(J)), ZJ(J), CI(J), DI(J))
```

Three separate exposures, in increasing order of severity:

- `CW(J)` and `XA(J)` are stale from an earlier face if the participant is not a
  link;
- `jxswork(J)` is stale in the same case, and `XSTAB` is `ALLOCATE`d
  `(3, nxscee, total_no_links)`, so a stale link number is a wrong-but-in-range
  read of another link's rating table;
- on the *first* confluence encountered in a run, `jxswork` has never been
  written at all, so `JXSWORK(J)` is whatever was on the stack — an unbounded
  index into a rank-3 array.

The invariant that holds this up is that confluence participants are always
links, which is true by construction in `FRmod` but is stated nowhere and
checked nowhere. `analysis_ocmod2.md` §7 raises the same question from the
callee side and leaves it open; this is the caller-side answer, plus the
"never initialised" half, which that document does not cover.

Fix, in order of preference: add `jxswork(J) = KEL` unconditionally with the
link test moved to an explicit `IF` block (see **P3**), or at minimum
`jxswork = 1` before `element_loop` so the uninitialised case degrades to a
wrong answer rather than a wild read.

The same reasoning does **not** apply to the single-face path: `:231` fires
routinely for land elements, but `CW`/`XA`/`jxswork` are only consumed by
`OCQLNK`, which is reached only when both `ielu` and `JEL` are links
(`:238`, `:257`, `:261`). Verified in §5.

### C4 — `JFACE2(J)` is stale for inactive confluence participants (`:292-300`)

```fortran
multi_setup_loop: DO J = 1, JMAX
   KEL = ICMRF2(IBR, J)
   IF (KEL > 0) THEN
      IF (KEL < ielu) CYCLE face_loop
      JFACE2(J) = ICMRF2(IBR, J + 3)      ! only assigned when KEL > 0
   END IF
   JEL2(J) = KEL
END DO multi_setup_loop
```

`JFACE2(J)` retains whatever the last face that touched slot `J` left there, and
is entirely undefined on the first junction of a run. Guarded today by `:307`
and `:335`, so it is only exploitable through **C2**. Recorded because it is the
second half of that hazard and both are closed by the same one-line change.

Note also that `CYCLE face_loop` at `:296` fires *before* `JEL2(J) = KEL` for
that `J`, leaving `JEL2` partially updated. Harmless — the next junction
rewrites every slot it reads — but it means neither `JEL2` nor `JFACE2` can be
assumed consistent outside the block that fills them.

### C5 — `LINK = MAX(1, MIN(ielu, total_no_links))` silently substitutes the last link (`:193`)

```fortran
LINK = MAX(1, MIN(ielu, total_no_links))
CALL OCQBC(NTYPE, ..., XAFULL(LINK), LINK, ...)
```

Two clamps, two different failure modes:

- **`MIN`**: for a *grid* element with a boundary condition, `ielu > total_no_links`
  and `LINK` becomes `total_no_links` — the last link in the domain. `OCQBC`
  passes it on as `XAFULL(LINK)` and `XSTAB(:, :, LINK)` (`OCmod2.f90:741`),
  which is an unrelated channel's bank-full area and rating table. Only reached
  on the `NTYPE == 8` (weir plus river in parallel) path, which is presumably
  channel-only, so the value is discarded — but nothing enforces that, and if a
  grid boundary were ever configured as type 8 the run would use the last link's
  cross-section without complaint.
- **`MAX`**: in a pure-grid run (`total_no_links == 0`) the `MIN` gives 0 and the
  `MAX` lifts it to 1. `XAFULL(1)` is then an unwritten element of a static
  array (BSS zero, so harmless), but `XSTAB` was allocated with a zero-length
  third dimension, so `XSTAB(:, :, 1)` inside `OCQBC` is a genuine out-of-bounds
  slice. Again gated on `NTYPE == 8`, so latent.

The clamp is doing the job of a precondition. Replacing it with an explicit
channel test — and a `FFFATAL` if a type-8 boundary is declared on a grid
element — converts two silent wrong answers into a configuration error at the
point where it can still be fixed.

### C6 — The ZQ table scan has no early exit and overrides an internal boundary type (`:262-276`)

```fortran
itemp = MAX(1, NBC)

DO i = 1, NoZQTables
   IF (((ielu == ZQTableLink(i)) .AND. (IFACE == ZQTableFace(i))) .OR. &
       ((JEL  == ZQTableLink(i)) .AND. (JFACE == ZQTableFace(i)))) THEN
      ZQTableRef = i
      NTYPE = 12
   END IF
END DO
```

Three observations, none fatal:

- **No `EXIT`.** If two table entries match the same face — a configuration
  error — the highest-numbered one silently wins, and the loop runs to
  completion either way.
- **The override is unconditional over `NTYPE`.** `NTYPE` was just set from
  `NOCBCD(NBC, 3)` at `:254`, so a face that is both a configured internal weir
  (`NTYPE = 7`) and a ZQ table becomes a ZQ table with no diagnostic. That
  precedence may well be intended; it is not documented anywhere.
- **`ZQTableRef` is a module-scope side channel** in `AL_D`, written here and
  read inside `OCQLNK` via `GET_ZQTABLE_VALUE`. It is never reset when no table
  matches, which is safe only because `NTYPE` is not 12 on that path.
  `analysis_ocmod2.md` **A6** already flags that this blocks any threading of
  `face_loop`; the caller-side note is that it also makes the correctness of
  `OCQLNK` depend on control flow 900 lines away.

`itemp` is computed before the override and `COCBCD(1, itemp)` is still passed
at `:276`. Harmless — `NTYPE == 12` does not read `AFROMCOCBCD` — but it means
`itemp`'s `MAX(1, ...)` is load-bearing for a path that no longer uses it.

### C7 — `DQIJ` is partially undefined and not pre-zeroed (`:159`, `:331`)

`QJ` and `DQ` are zeroed once before `element_loop` (`:165-166`); `DQIJ` is not.
`OCQMLN` declares `DQIJ` `INTENT(OUT)` and writes only the columns of active
branches (`OCmod2.f90:1374-1395`), so after the call `DQIJ(:, J)` is undefined
for every inactive `J`. Every read in `multi_scatter_loop` is guarded — `:339`
by `KEL /= 0`, `:342` by `J > 0` plus the same, `:349` by
`ICMRF2(JBR, JJ) > 0` — so this is not a live defect.

It is worth one statement anyway: under `-finit-real=snan` or valgrind the
uninitialised columns will show, and the guard at `:349` depends on the
`ICMREF`/`ICMRF2` consistency the module header already lists as an unchecked
assumption (`:135-137`). `DQIJ = zero` alongside `:166` makes the failure mode
"a zero derivative" rather than "whatever was on the stack".

Note the asymmetry, incidentally: the pre-zeroing at `:165-166` that *is* there
is dead. Every callee declares `Q`/`DQ` `INTENT(OUT)` and assigns every element
it declares — `OCQGRD:1029-1030` for the impermeable case, `:1064-1069`
otherwise; `OCQBNK:903-906`; `OCQLNK:1239-1241` — and the boundary path assigns
`DQ(0,1)` explicitly at `:199`. So the two statements that exist are unnecessary
and the one that is needed is absent.

---

## 2. Performance

### P1 — The `(element, face)` layout costs ~31 cache lines per element (dominant)

Every array this routine indexes by face is dimensioned with the element extent
first:

| Array | Declaration | Column stride |
|---|---|---|
| `ICMREF` | `INTEGER (NELEE, 12)` (`AL_G.F90:46`) | 1 MB |
| `DHF` | `DOUBLEPRECISION (NELEE, 4)` (`AL_C.F90:105`) | 2 MB |
| `DQ0ST` | `DOUBLEPRECISION (NELEE, 4)` (`AL_D.f90:234`) | 2 MB |
| `DQIST` | `DOUBLEPRECISION (NELEE, 4)` (`AL_D.f90:235`) | 2 MB |
| `qsazz` | `DOUBLEPRECISION (NELEE, 4)` (`OCmod2.f90:78`) | 2 MB |

Counting the distinct cache lines touched to process **one element's four
faces**, self side only:

```
ICMREF(ielu, 5:8)    neighbour refs                4
ICMREF(ielu, 9:12)   reciprocal faces              4
DHF(ielu, 1:4)                                     4
DQ0ST(ielu, 1:4)                                   4
DQIST(ielu, 1:4)                                   4
qsazz(ielu, 1:4)     via SETQSA                    4
NOCBCC, ZGRUND, STRXX, STRYY, DXQQ, DYQQ, hrfzz    7
                                                  ---
                                                   31
```

The payload is about 248 bytes; the lines fetched are about 1 984. That is
**~12 % cache line utilisation**, and 31 concurrent sequential streams — at or
past what a typical L2 stream prefetcher tracks. The *neighbour* accesses
(`GETHRF(JEL)`, `DHF(JEL, JFACE)`, `SETQSA(JEL, JFACE, ·)`,
`DQ0ST(JEL, JFACE)`) land at effectively random offsets inside those same
arrays, so they are additional misses on top.

Honest framing: within one `element_loop` pass each column is swept
sequentially in `ielu`, so this is prefetcher pressure, TLB pressure and lines
touched per element — not random access on the self side. The neighbour side
genuinely is random.

The fix is the transpose `analysis_ocmod2.md` **M2** proposes for `qsazz`,
extended to the other four arrays: `qsazz(4, NELEE)`, `DHF(4, NELEE)`,
`DQ0ST(4, NELEE)`, `DQIST(4, NELEE)`, and an element-contiguous
`ICMREF(12,NELEE)` layout. Then one element's four faces are a single contiguous
32-byte block in each array:

```
ICMREF topology  8 ints  = 32 B                    1
DHF              4 dbl   = 32 B                    1
DQ0ST            4 dbl   = 32 B                    1
DQIST            4 dbl   = 32 B                    1
qsazz            4 dbl   = 32 B                    1
scalars                                            7
                                                  ---
                                                   12
```

~31 lines → ~12, ~31 streams → ~12, and utilisation from 12 % to roughly 50 %.
`DQ0ST` and `DQIST` are always read and written together and could be
interleaved into one `(2, 4, NELEE)` array for another line.

This is not a local change — `qsazz` is `PUBLIC` and `OCmod.f90:2265-2268`
currently copies it column-wise into `QOC`. Transpose `QOC` in the same project
so that handoff does not become strided. The full layout change should be
costed across `OCQDQ`, `OCSIM`, `OCFIX`, and the downstream `QOC` consumers,
rather than as separate array edits. `OCQDQ` is the site that makes the case:
it is the only routine that indexes all five current arrays by `(element,face)`
inside the same inner loop.

### P2 — The surface-storage test redundantly recomputes state it already has (`:220-229`, `:313-322`)

```fortran
ZI(J)  = GETHRF(KEL)                                     ! :217
LI(J)  = DHF(KEL, KFACE)
ZGI(J) = ZGRUND(KEL)                                     ! :219
STR(J) = FSTR(KEL, KFACE)                                ! :220

IF (STRXX(KEL) < 0.0d0) THEN
   IF ((GETHRF(KEL) - ZGRUND(KEL)) < (-STRXX(KEL) / 1000.0d0)) THEN
```

Per participant, per face, per timestep:

- `GETHRF(KEL)` is called a second time for a value already in `ZI(J)`, and
  `ZGRUND(KEL)` reloaded for one already in `ZGI(J)`. `gethrf` is `PURE` and
  one line, so IPO should fold the call (`analysis_ocmod2.md` **P5**), but the
  reload is a second reference to an 2 MB static array;
- `-STRXX(KEL) / 1000.0d0` is a **double-precision division** — ~20 cycles,
  not pipelined — where `(ZI(J) - ZGI(J)) * 1000.0d0 < -STRXX(KEL)` is a
  multiply;
- `FSTR(KEL, KFACE)` is evaluated and then discarded whenever the marker fires.

Rewritten (and this is the same edit as **C1**'s fix):

```fortran
IF (strxx(KEL) < zero) THEN
   STR(J) = MERGE(0.5d0, 2.0d0, (ZI(J) - ZGI(J)) * 1000.0d0 < -strxx(KEL))
ELSE
   STR(J) = fstr(KEL, KFACE)
END IF
```

Separately and larger: `STRXX(KEL)` is loaded for **every participant of every
face** whether or not any element in the run carries a marker. That is a 2 MB
static array streamed once per element pass for a feature most runs do not use.
A module-level `LOGICAL :: any_surface_storage`, set once where `STRXX` is
read, would remove the array from the working set entirely and turn the branch
into a loop-invariant test the compiler can hoist.

### P3 — `CYCLE` used as "skip the tail of the body" (`:231`, `:324`)

```fortran
IF (KEL > total_no_links) CYCLE single_data_loop
CW(J) = CWIDTH(KEL)
XA(J) = XAFULL(KEL)
jxswork(J) = KEL
END DO single_data_loop
```

The `CYCLE` is not a loop-control statement here; it is an `IF` block written
inside out, and it happens to be the last three statements of the body. Anything
appended after `:234` would silently apply only to links. Combined with **C3**
(the conditional assignment is exactly what makes `jxswork` stale) and **C1**
(the same pattern of "the marker test is only in the places that happened to
need it"), this is a structural rather than cosmetic point. An explicit

```fortran
IF (KEL <= total_no_links) THEN
   CW(J) = CWIDTH(KEL); XA(J) = XAFULL(KEL); jxswork(J) = KEL
END IF
```

costs nothing and removes the trap.

### P4 — `single_data_loop` and `multi_data_loop` are identical (`:212-235`, `:305-328`)

The two loop bodies are byte-for-byte the same; only `JMAX` differs (1 vs 3),
and `JMAX` is already a variable. The single-face path additionally pays for
staging `ielu`/`IFACE` and `JEL`/`JFACE` into `JEL2(0:1)`/`JFACE2(0:1)` and then
running a two-trip loop over them — so it carries the cost of sharing a loop
body with the multi path *and* the duplication.

This is not primarily a performance finding. **C1** is a direct consequence: the
gather logic was needed in a third place and was not written there. Factoring
the body into one internal subroutine

```fortran
SUBROUTINE gather(J, KEL, KFACE)   ! fills ZI, LI, ZGI, STR, CW, XA, jxswork
```

collapses `:212-235`, `:305-328` and the missing `:190` case into one site, and
lets the single path drop the staging arrays entirely.

### P5 — The ZQ scan is static topology evaluated in the innermost loop (`:265-271`)

Four integer comparisons per table entry, per link-link face, per timestep, with
no early exit. For a handful of tables this is noise; the point is that the
predicate depends only on `(ielu, IFACE)` and `(JEL, JFACE)`, all of which are
fixed topology. It could be resolved once at initialisation into a sparse
per-link map (or a `ZQFaceRef` column alongside the link arrays), making the hot
path a single load and a compare against zero. Worth doing only if `NoZQTables`
is ever more than a few — but add the `EXIT` regardless (**C6**).

### P6 — `SETQSA` where `DQ0ST` is a direct store (`:200`, `:280-286`, `:338`)

```fortran
CALL SETQSA(JEL, JFACE, QJ(1))
DQ0ST(JEL, JFACE) = DQ(1, 1)
DQIST(JEL, JFACE) = DQ(1, 0)
```

Two direct stores and one subroutine call, into three arrays of identical shape,
two lines apart. `setqsa` is one assignment (`OCmod2.f90:139-148`) and cannot be
`PURE`, so it is an optimisation barrier in a block the compiler would otherwise
fuse — though with IPO on (`CMakeLists.txt:73`) it should inline and the barrier
should vanish.

The asymmetry exists only because `qsazz` is private to `OCmod2` while `DQ0ST`
is public in `AL_D`. Not worth changing on its own; worth noting that if **P1**'s
transpose happens, this is the natural moment to make the three stores
consistent. Recorded so the accessor pattern is not blamed twice —
`analysis_ocmod2.md` **P5** already owns it.

### P7 — Minor, no action

- `DQ(0, 1) = zero` followed by `DQIST(ielu, IFACE) = DQ(0, 1)` (`:199`, `:202`)
  is a round trip through a local for a constant store.
- `IF (JEL < ielu .AND. SINGLE)` (`:182`) — Fortran does not guarantee
  short-circuit evaluation, but both operands are already-loaded registers.
  The module header (`:76`) documents the operands in the opposite order; the
  code is correct either way.
- `LINK` (`:193`) is recomputed per boundary face rather than hoisted. There is
  at most one boundary face per element.
- `NFACE` **is** correctly hoisted out of `face_loop` (`:170-175`). Good as-is.

---

## 3. Memory and variable management

### M1 — 4 MB of static BSS for two per-element arrays (`:58-59`)

```fortran
DOUBLEPRECISION :: STRXX(NELEE)      ! 2 MB
DOUBLEPRECISION :: STRYY(NELEE)      ! 2 MB
DOUBLEPRECISION :: XAFULL(NLFEE)     ! 160 KB
```

`NELEE = 250 000` against a touched extent of `total_no_elements`, typically one
to two orders of magnitude smaller on the shipped examples. Untouched pages
never fault in, so the resident cost is small; the cost is that the live data is
spread across a 2 MB address range per array instead of being compact, which is
**P1** seen from the allocation side. Same pattern as
`analysis_ocmod2.md` **M3**.

Making all three `ALLOCATABLE` at `total_no_elements` / `total_no_links` would
compact them. There is no `initialise_ocqdqmod` to host that today; the natural
home is wherever `STRXX` is currently filled from input.

### M2 — `COCBCD`, `HOCNOW`, `QOCF` are correctly sized and correctly oriented (`:55-57`)

`NOCTAB = 20`, so `COCBCD(5, 20)` is 800 bytes and `HOCNOW`/`QOCF` are 160 bytes
each — all L1-resident, and `COCBCD`'s coefficient index is the fastest-varying
one, which is what makes the `COCBCD(1, IBC)` sequence-association pass at
`:197` and `:276` give five (respectively three) contiguous doubles.
`analysis_ocmod2.md` **M5** verified the bounds: `COCBCD(1, 20)` leaves exactly
five elements. Correct as-is; recorded so it is not "fixed".

### M3 — Public mutable module state with no write barrier (`:54-64`)

```fortran
PUBLIC :: OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD
```

Six arrays are public and writable with no setter. The module header
(`:25-28`) already records that `run_sim` imports `HOCNOW`, `QOCF` and `XAFULL`
without referencing them — dead imports that widen the write surface for no
benefit. Removing them from `run_sim`'s import list is zero-risk and narrows the
set of files that can perturb `OCQDQ`'s inputs from six to whatever actually
fills them.

Same for `ICMXY` at `:49`, imported and never referenced, as the header notes.
An unused `USE ... ONLY` entry costs nothing at runtime but keeps a module
dependency in the build graph.

### M4 — Local storage is trivial and correctly scoped

`OCQDQ`'s locals (`:154-161`) total roughly 400 bytes: 22 scalars, four
`(0:3)` real arrays, two `(0:3)` integer arrays, `DQ(0:1,0:1)` and
`DQIJ(0:3,0:3)`. All stack-resident, no allocation, no `SAVE`, and — verified
in `analysis_ocmod2.md` **M5** — no hidden array temporaries at any call site,
because every actual is either a whole explicit-shape array or a base element
address, never a non-contiguous section. That part of the 2026-04-06 rework
(`632f254`, history line `:44`) did what it claims.

The one variable-management defect is `jxswork` (**C3**), and the one
readability cost is that `KEL` is reused as the inner-loop induction variable
inside `multi_scatter_loop` (`:346`) after it has been read at `:337-343` — safe,
because `JBR` is taken before the inner loop, but it means two different
elements share a name inside twelve lines.

---

## 4. Priority table

| ID | Action | Findings | Expected effect | Risk |
|---|---|---|---|---|
| **1** | Apply the surface-storage override on the boundary path; factor it into one helper | C1, P2, P4 | Fixes a sign-reversed boundary flow and a corrupted Jacobian diagonal; removes a division and a redundant `GETHRF` per participant | Low — changes results only where the bug fires, which is the point |
| **2** | Initialise `jxswork`; make the link-only assignments an explicit `IF` block | C3, P3 | Closes an unbounded `XSTAB` index on the first confluence | None |
| **3** | `IF (KEL < 1)` in `multi_scatter_loop`; `DQIJ = zero` beside `:166` | C2, C7 | Closes two latent out-of-bounds writes and an uninitialised read | None |
| **4** | `EXIT` the ZQ scan on match; document the `NTYPE = 12` precedence over internal-boundary types | C6 | Removes silent last-match-wins behaviour | None |
| **5** | Replace `LINK`'s clamp with an explicit channel test plus a fatal on a type-8 grid boundary | C5 | Turns two silent wrong answers into a configuration error | Low — may reject inputs that previously ran |
| **6** | Factor the two gather loops into one internal subroutine; drop the single-path staging | P4 | Removes the duplication that caused C1 | Low |
| **7** | Cache `any_surface_storage` and skip the `STRXX` load when false | P2 | Removes a 2 MB static array from the per-element working set on runs that do not use the feature | None |
| **8** | Transpose `DHF`, `DQ0ST`, `DQIST`, `qsazz`, and `QOC` to `(4, NELEE)`, and `ICMREF` to an element-contiguous `(12,NELEE)` layout | P1 | ~31 lines/element → ~12, ~31 streams → ~12, utilisation 12 % → ~50 %; avoids making the `qsazz`→`QOC` handoff strided | Medium — coordinated cross-module layout change |
| **9** | Precompute the ZQ face map at initialisation | P5 | Only if `NoZQTables` grows | Low |
| **10** | Drop the unused `ICMXY` import and the dead `run_sim` imports | M3 | Narrows the write surface on public state | None |

Items 1-4 are one afternoon and carry essentially no regression risk. Item 8 is
the only one with real performance magnitude and should be costed together with
`analysis_ocmod2.md` **M2**, not separately.

---

## 5. Verified correct — do not re-audit

### The `MOD(J+JJ, 4)` confluence scatter is correct (`:341-351`)

This is the one line in the file worth proving, because it is a bare modular
arithmetic expression indexing a global derivative array with no comment.

```fortran
DQIST2(IBR, J) = DQIJ(0, J)
JBR = -ICMREF(KEL, KFACE + 4)

DO JJ = 1, 3
   KEL = ICMRF2(JBR, JJ)
   IF (KEL > 0) THEN
      JJJ = MOD(J + JJ, 4)
      DQIST2(JBR, JJ) = DQIJ(J, JJJ)
   END IF
END DO
```

**Structure.** `FRmod:797-801` increments `INDEX2` once **per element per
multi-way face**, so each of the up to four members of a junction owns a
*distinct* branch record listing the other three from its own viewpoint. `IBR`
and `JBR` are therefore always different rows, and there is no clobbering
between `:342` and the inner loop.

**The claim.** Local index 0 is `ielu` and local index `k` is
`ICMRF2(IBR, k)`. The line asserts that slot `JJ` of member `J`'s record is
local index `MOD(J + JJ, 4)` — i.e. that all four members enumerate the node's
arms in the same cyclic order, starting from themselves.

**Proof.** Reading the four branch-record blocks in `FRmod` and labelling each
arm by compass direction:

| Block | Self arm | Slot 1 | Slot 2 | Slot 3 | Source |
|---|---|---|---|---|---|
| Face 1 (east) of an EW link at `(I,J)` | W | N: `NSOUTH(I+1,J)` | E: `EWEST(I+1,J)` | S: `NSOUTH(I+1,J-1)` | `:803-828` |
| Face 2 (north) of a NS link at `(I,J)` | S | W: `EWEST(I-1,J+1)` | N: `NSOUTH(I,J+1)` | E: `EWEST(I,J+1)` | `:852-877` |
| Face 3 (west) of an EW link at `(I,J)` | E | S: `NSOUTH(I,J-1)` | W: `EWEST(I-1,J)` | N: `NSOUTH(I,J)` | `:897-923` |
| Face 4 (south) of a NS link at `(I,J)` | N | E: `EWEST(I,J)` | S: `NSOUTH(I,J-1)` | W: `EWEST(I-1,J)` | `:944-970` |

Each row reads `W,N,E,S` / `S,W,N,E` / `E,S,W,N` / `N,E,S,W` — the **same
clockwise cycle**, rotated to start at the self arm. That is exactly the
invariant, so `MOD(J + JJ, 4)` is the right index. ✓

**Three-way junctions preserve it**: `FRmod` assigns slots by geometric position
(which `LCODE` test fires), not by compaction, so an absent arm leaves a zero
*in place* and the remaining slots keep their cyclic offsets. `:307`, `:335` and
`OCmod2:1358` all key off `JEL2(J)`, which is `ICMRF2(IBR, J)`, so local index
and slot index stay aligned throughout. ✓

**The consumers agree**: `OCmod.f90:499-505` and `:2228-2234` both read
`DQIST2(IBR, J)` as ∂Q(owner of `IBR`)/∂z(`ICMRF2(IBR, J)`), guarded by
`ICMRF2(IBR, J) > 0` — matching what `:342` and `:349` write and matching the
write guards. ✓

**Coverage is complete**: the junction is processed exactly once, from its
lowest-numbered member, so the inner loop is the *only* mechanism by which the
other three members' rows get filled. It is necessary, not redundant. ✓

### The `JEL < ielu` skip is load-bearing for correctness, not just for work (`:182`)

`:238` dispatches to `OCQBNK` when `ielu <= total_no_links .AND. JEL > total_no_links`,
and the `ELSE` at `:257` sends `ielu > total_no_links` to `OCQGRD` — the
land-land routine. So a link-bank face reached from the *bank* side would call
`OCQGRD` on a channel.

It cannot be reached from the bank side: links are elements `1 .. total_no_links`
and everything else is above (`NGDBGN = total_no_links + 1`, `FRmod:709`), so for
a bank/link pair `JEL < ielu` always holds when `ielu` is the bank, and `:182`
cycles. The skip is therefore what *guarantees* `ielu` is the link on every
link-land face — not merely what avoids processing the face twice.

Worth writing down because it is invisible at the point of the skip, and it
means the ordering condition cannot be relaxed or reordered without breaking
`:238`.

### `CW`/`XA`/`jxswork` are always valid where `OCQLNK` reads them

`OCQLNK` is reached only through `:261`, which requires `ielu <= total_no_links`
(`:238` false on its first conjunct is impossible there) **and**
`JEL <= total_no_links` (`:238` false on its second conjunct). Both participants
are links, so `:231`'s `CYCLE` does not fire for either and `CW(0:1)`, `XA(0:1)`,
`jxswork(0:1)` are all freshly written. ✓ The staleness in **C3** is confined to
the confluence path.

### `NBC` selection cannot pick up a zero boundary index (`:245-254`)

```fortran
IF (IFACE == NFACE) THEN
   NBC = IBC
```

`NFACE` is 0 whenever `IBC <= 0` (`:170-175`) and `IFACE` runs 1..4, so the
branch is unreachable with `IBC == 0`. `NBC > 0` is therefore guaranteed
wherever `NOCBCD(NBC, 3)` is read at `:254`. ✓

### Unspecified external faces correctly stay at zero flow

If an element's boundary face is external but `NFACE /= IFACE` — or if it has no
boundary condition at all — the `eexternal` branch writes nothing, so
`qsazz(ielu, IFACE)`, `DQ0ST` and `DQIST` are never assigned for that face. That
looks like a stale-value hazard; it is not. All three are static arrays in BSS
(zero at startup), nothing else ever writes them for such a face — `OCFIX`'s
face loop cycles immediately on `QE < ZERO` being false for `QE == 0`
(`OCmod2.f90:1791-1795`) — and zero is the physically correct answer for an
unspecified external boundary. ✓

### No hidden array temporaries at any call site

Verified in `analysis_ocmod2.md` **M5** and not repeated: `COCBCD(1, IBC)` and
`COCBCD(1, itemp)` are in-range sequence associations against `AFROMCOCBCD(5)`
and `(3)`; the `(0:3)`-actual against `(0:1)`-dummy passes at `:242`, `:260`,
`:275` are legal sequence association with the address passed, not copied. The
`PERF FIX` comments at `:195`, `:241`, `:259`, `:274`, `:330` are correct in
effect. ✓

---

## 6. What could not be determined from the source

- **Whether any shipped or field dataset actually places a surface-storage
  marker on an element with a type 3/8/9 boundary condition on face 1 or 3.**
  That decides whether **C1** is a latent defect or one that has been silently
  producing reversed boundary flows. It is a grep over the input decks for
  negative `STRXX` values cross-referenced against `NOCBCD`, not a source
  question.
- **The intended precedence when a face is both an internal weir and a ZQ
  table** (**C6**). Needs the ZQ configuration semantics, not this module.
- **Whether `NoZQTables` is ever more than a handful.** Decides whether **P5** is
  worth doing at all.
- **The measured cost split between this routine and its `OCmod2` callees.**
  `OCQDQ` does almost no arithmetic, so on the argument above its share should
  be nearly all memory stalls — but that is a prediction, and the transpose in
  **P1** should be justified by a cache-miss count on `element_loop`, not by the
  line-count arithmetic in this document.
