# Logical performance assessment: `CMmod`

## Scope and method

This is a **logical, source-only** assessment. No profile was taken and no
timings were measured. Every claim below is derived from reading
`src/modules/CMmod.f90` in full, together with the call site in
`src/modules/run_sim.f90`, the state modules it shares
(`src/parameters/CONT_CC.F90`, `colm_c1.f90`, `colm_c2.f90`, `colm_cc.f90`,
`colm_cc1.f90`, `colm_cg.f90`, `colm_co.f90`, `link_cc.f90`, `link_cc1.f90`,
`link_cw.f90`, `bk_cw.f90`, `plant_cc.f90`, `is_cc.f90`, `SED_CS.F90`,
`sed_co.f90`, `AL_C.F90`, `AL_G.F90`), the dimension parameters in
`sglobal.f90`, `TRIDAG` in `utilsmod.f90`, the producers in `FRmod.f90`, and the
compiler flags in `CMakeLists.txt`. Where a claim depends on compiler behaviour
rather than on the source alone, that is stated explicitly.

Line numbers refer to `CMmod.f90` at commit `e0aef0b` (3 386 lines).

The assessment was requested for `CMmod.f90` only. Callers and callees outside
this module were read where needed to establish an interface, a call frequency
or a producer, but are not themselves assessed.

`CMmod` has two entry points. `CMRD` (`:146`) runs **once**, from
`FRmod:INCM` (`FRmod.f90:4246`). `CMSIM` (`:662`) runs **once per VSS
timestep** whenever the contaminant component is active
(`run_sim.f90:324`, guarded by `BCM` at `:291`). Everything reachable from
`CMSIM` therefore carries per-timestep cost:

```text
CMSIM
 ├─ MNINITIALISE (first call) / MNCONT (later calls)   when ISMN   (:702-716)
 ├─ PLPREP                                             when ISPLT  (:719)
 ├─ per element, in ISORT order:                                   (:723-732)
 │    ├─ COLMW → COLMSM → { PLCOLM → PLANT }, RET ×2, COLM → SLVCLM → TRIDAG
 │    └─ LINKW → LINKSM → FRET ×6, LINK → SNL3
 └─ CCCC/SSSS → CCCCO/SSSSO save sweep                             (:736-754)
```

All seventeen procedures were assessed. Coverage:

| Procedure | Lines | Per-timestep? | Principal findings |
|---|---|---|---|
| `CMRD` | 146–620 | no (startup) | 1.14 |
| `CMSIM` | 662–756 | yes | 2.2, 1.15 |
| `COLM` | 801–1055 | yes, per cell | 1.2, 1.9, 3.6, 4.1, 4.5 |
| `COLMSM` | 1102–1437 | yes, per cell | 1.3, 1.4, 1.6, 3.1, 3.5, 5.4 |
| `COLMW` | 1488–1830 | yes, per cell | 1.1, 1.7, 2.1, 2.3, 3.2, 3.7 |
| `DISP` | 1843–1860 | yes | 3.1 (constant placeholder) |
| `LINKSM` | 1905–2169 | yes, per link | 1.3, 1.12, 1.13 |
| `LINKW` | 2215–2484 | yes, per link | 1.10, 1.11, 1.13, 3.4 |
| `LINK` | 2517–2691 | yes, per link | 3.3, 5.2 |
| `PHI` | 2704–2723 | yes | 3.1 (constant placeholder) |
| `PLCOLM` | 2760–2880 | `ISPLT` only | 1.5, 1.8, 4.3 |
| `SLVCLM` | 2905–2951 | yes, per column | 4.6 |
| `RET` | 2978–3028 | yes, ×2 per column | 5.5 |
| `SNL3` | 3064–3174 | yes, per link | 4.2, 4.4, 5.2 |
| `FRET` | 3202–3266 | yes, ×6 per link | 5.5 |
| `PLANT` | 3290–3356 | `ISPLT` only | 1.8 (guards) |
| `PLPREP` | 3358–3384 | `ISPLT` only | 1.8 |

## Conclusion up front

`CMmod` has, unusually for this codebase, **both** a memory-layout problem and a
genuine arithmetic hot spot, and the arithmetic one is probably the larger.

- **Arithmetic.** `COLM:880` and `:890` evaluate `COLCAP(NC)**GGNMON` and
  `SOLCAP(NC)**GGNMON` with a *runtime real* exponent, i.e. two `pow()` calls,
  **per cell, per contaminant, per element, per timestep**. `GGNMON` is
  `GNN(NCONT) - 1`, constant for the whole run. `SNL3:3095` then runs a
  hard-coded **103 fixed-point iterations with three divisions each** per link
  per contaminant per timestep, with no convergence test and no early exit.
  These two sites dominate; neither is a consequence of the physics.

- **Layout.** Every CM-owned state array that the module carries across
  timesteps is **element-major with cell as the second subscript** — `CCCC`, `CCCCO`,
  `SSSS`, `SSSSO`, `SSS1`, `SSS2` (`CONT_CC.F90:44-51`), `QQO`, `UUAJPO`,
  `VSTHEO`, `GGAMMO` (`colm_co.f90`) — and the shared, ET-produced `ERUZ`
  (`AL_C.F90:159`) has the same layout. Every loop that touches them is
  element-fixed, cell-inner. Each inner iteration
  therefore strides by `total_no_elements` doubles. Everything arriving from VS
  (`VSTHE`, `QVSV`, `DELTAZ`, `ZVSNOD`) is already cell-major and is read
  unit-stride, so the two conventions are mixed inside single loop bodies
  (`COLMW:1585-1610` is the clearest example). This is the same finding as
  `analysis_mnmod.md` §2.1, and CM is where those arrays are owned.

- **`CMSIM`'s save sweep** (`:736-754`) is the worst single instance: a
  `NCON × NEL × ncells` copy of two arrays with the innermost subscript in the
  slowest-varying position. It moves roughly eight cache lines for every one
  line of useful data.

Before any of that, a set of correctness defects should be settled. **Four
module variables are read on the unconditional hot path and are never assigned
anywhere in `src/`** (`XXI`, `ESSCAP`, `ESSCPC`, `ESSCPT` — §1.1, §1.2); three
more control flags are likewise unassigned and silently disable the `CM5` and
`CM13` inputs (§1.7). Three further defects are order-dependent
(`ret_setup_loop`, `IIICFO`/`CCAPIO`, `CDUM`) and make the answer depend on
contaminant index or on `ISORT`. One is an out-of-bounds middle subscript
(`PDZF3`) that aliases two plant slots onto each other. Two more (`find_jal`,
`NCETOP+1` in `LLEE` arrays) block bounds-checked validation of everything
else, since `Debug` uses `-fcheck=bounds` / `/check:bounds`
(`CMakeLists.txt:651,694`).

The cheapest substantial win is special-casing the linear-adsorption case in
`COLM` so the two `pow()` calls disappear. The largest is transposing the
`CONT_CC` and `COLM_CO` state to cell-major.

---

## Part 1 — Correctness defects

### 1.1 `XXI` is never assigned, and it scales a term on the unconditional main path

`plant_cc.f90:106` declares

```fortran
DOUBLEPRECISION XXI !! Uninitialized weighting applied with the mobile-water fraction ...
```

A grep over `src/` finds **no assignment to `XXI` anywhere**. There are exactly
two readers, and the first is not on the plant path at all:

```fortran
! COLMW:1615 — inside cell_loop, every land column, every timestep
GGAMM1(NCE) = ((one - XXI * PHIDUM) * ROH(NCE) * TRAN1(NCE) / (KSP(NCE) * Z2)) &
            + (((one - PHIDUM) * TTHET1(NCE) - (one - PPHI(NCE)) * TTHET(NCE)) / DTUZ)
```

`GGAMM1` is stored to `GGAMMO(NCL,NCE)` (`:1617`), read back as `GGAMM`
(`:1614`) on the next step, and consumed by `COLM:876-877` and `:900-901`:

```fortran
BCAP   = Z2SQOD * (AALPH + half * ABS(GGAMM(NC)))
GMCAP  = Z2SQOD * GGAMM(NC) / two
```

i.e. it sets the dynamic/dead-space exchange rate for **every cell of every
column**. This is not gated by `ISPLT`, `ISMN`, `ISBK` or anything else.

Both gfortran and ifx place module data in static storage and zero it at load,
so in practice `XXI == 0` and the first term degenerates to the *unweighted*
root-extraction rate `ROH·TRAN1/(KSP·Z2)`. Nothing in the language guarantees
that, `-finit-real=snan` would expose it immediately, and the mobile-water
weighting the expression was written for is simply absent.

The second reader is `PLCOLM:2835`:

```fortran
XDUM = XXI * PPHI(NCE)
CDUM = XDUM * COLCAP(NCE)              ! mobile-region contribution
SDUM = (ONE - XDUM) * SOLCAP(NCE)      ! dead-space contribution
```

With `XXI == 0`, `CDUM` is identically zero and `EDCAP` (`:2843`) receives
nothing: **plant uptake draws entirely from the dead-space region and not at
all from the mobile region**, which is backwards. Reachable only when `ISPLT`
is true, which today it never is (1.7).

`XXI` needs a producer, or the weighting needs to be removed deliberately. It
is listed first because it is the only unassigned variable in this module that
affects the answer on every run.

### 1.2 `ESSCAP`, `ESSCPC` and `ESSCPT` are never assigned and are read in `COLM`

`colm_cc.f90:89-91` declares all three, each annotated "uninitialized". A grep
over `src/` finds no assignment. All three are read in `COLM`, in the block
that adds the surface-water and sediment terms to the top-cell balance:

```fortran
:987   MCAP  = MCAP  + (VCAP * (...) - ESSCAP - ICAP - QCAP + CST2 * SUM1) / KSP(NC)
:992   MCAPT = MCAPT + (... - ESSCPT - ICAPT - QCAPT + CST2 * SUM2 / TSE) / KSP(NC)
:995   MCAPC = MCAPC + (VCAP * (...) - ESSCPC - ICAPC - QCAPC + CST2 * SUM3) / KSP(NC)
```

`MCAP`, `MCAPT` and `MCAPC` go straight into `SLT` (`:1018`) and `ELT`
(`:1015`) for the top cell, so these are live terms in the solved system on
every column on every timestep.

The name and the `colm_cc.f90` comment identify them as the top-cell
surface-water/loose-sediment plant-uptake terms — the surface counterpart of
`EDCAP`/`ESCAP`, which `PLCOLM` does populate. The plant path was evidently
never finished. As with `XXI`, static zeroing makes the omission invisible.

Either give them a producer in `PLCOLM`, or replace them with
`ZERO` parameters and a comment recording that surface plant uptake is not
modelled. Both are honest; the present state is neither.

### 1.3 `COLMSM` and `LINKSM` age their history arrays inside the contaminant loop

`COLMSM`'s `ret_setup_loop` (`:1261-1269`) is **inside** `cont_loop`:

```fortran
FBO(JSED)         = FBETAO(NCL, JSED)   ! old
FB(JSED)          = FBETA(NCL, JSED)    ! new
FBETAO(NCL, JSED) = FB(JSED)            ! shift
FDLO(JSED)        = FDELO(NCL, JSED)
FDL(JSED)         = FDEL(NCL, JSED)
FDELO(NCL, JSED)  = FDL(JSED)
```

After the pass for contaminant 1, `FBETAO(NCL,:) == FBETA(NCL,:)`. Contaminants
2..`NCON` therefore read `FBO == FB` and `FDLO == FDL`, and the two `RET` calls
at `:1272` and `:1276` compute

```
RT = (SUMN/TH - SUMO/THO)/DT
```

with `SUMN == SUMO`. **Contaminant 1 sees the sediment-composition change over
the timestep; contaminants 2 and 3 see none.** `RRRLST` and `RRRSWT` feed
`RRRSAT` and thence `MCAPT` in `COLM:869` and `:992`, so this is a difference
in the solved equation, not in a diagnostic.

`LINKSM:2080-2092` has the identical defect for `FBBEDO`, `FDELO` and
`FBTSDO`.

The shift must be hoisted out of `cont_loop` — read the old values once before
the loop, write the new values once after it.

### 1.4 `COLMSM` ages the rainfall and dry-deposition history inside the element sweep

`:1285-1286` and `:1301`:

```fortran
ICAP          = -Z2OD * IIICFO(NCONT)
IIICFO(NCONT) = IIICF(NCONT)
...
CCAPIO(NCONT) = CCAPI(NCONT)
```

`IIICFO` and `CCAPIO` are `(NCONEE)` module arrays — *per contaminant*, not per
element. `COLMSM` is called once per land element. The **first** element in
`ISORT` order shifts them; every subsequent element in the same timestep reads
the already-shifted value. So element 1 uses the previous timestep's rate and
elements 2..N use the current one.

Today this is benign in effect: `CCAPI` and `IIICF` are written only by `CMRD`
(`:401`, `:479`) and are seeded into `CCAPIO`/`IIICFO` by `FRmod:4563-4564`, so
after the first timestep old == new and the distinction vanishes. It is
nevertheless a per-element loop mutating per-contaminant global state, it makes
the first timestep order-dependent, and it silently blocks any future change
that makes rainfall concentration time-varying — which is exactly what the
`CCAPIO`/`QCAPT` machinery exists for. Move both shifts to `CMSIM`, after
`update_loop`.

### 1.5 `PDZF3` is indexed out of bounds in its middle subscript, aliasing the two plant slots

`plant_cc.f90:104` declares

```fortran
DOUBLEPRECISION PDZF3 (NELEE, NPELEE, LLEE)
```

with `NPELEE = 2` (`sglobal.f90:133`) and `LLEE = 50` (`:119`). Both the
producer (`FRmod:5511`) and the consumer (`PLCOLM:2839`, `:2862`) index it as

```fortran
PDZF3(NCL, NCE, JPLANT)
```

— element, **cell**, plant. `NCE` runs `NRBOT..NCETOP`, i.e. up to
`top_cell_no`, against an extent of 2.

The declaration is simply transposed relative to every use. The array is large
enough that no access leaves it, so the effect is aliasing rather than
corruption. In column-major order the linear offset is
`(NCE-1) + NPELEE*(JPLANT-1)` in units of `NELEE`:

| Slot | Offsets written by `FRmod:5511` (`NCE = 2..NCETOP`) |
|---|---|
| `JPLANT = 1` | 1 .. `NCETOP-1` |
| `JPLANT = 2` | 3 .. `NCETOP+1` |

Offsets 3..`NCETOP-1` are written twice. Since `FRmod:5493-5495` sets
`NPL(NCL) = 2` and plant slot 2 is written last, **plant slot 1's root
distribution for cells 4..`NCETOP` is overwritten by plant slot 2's**. When the
two slots have different `NPLTYP`, and therefore different `RDF`, the uptake in
`PLCOLM:2839` is wrong for one of them.

`-fcheck=bounds` traps this immediately. The fix is to declare
`PDZF3(NELEE, LLEE, NPELEE)`; no use site changes.

Reachable only when `ISPLT` is true, which today it never is (1.7). It should
still be fixed before the plant path is re-enabled.

### 1.6 `COLMSM`'s `CDUM` is an implicitly-`SAVE`d local written for every column

`:1133`:

```fortran
DOUBLE PRECISION :: CDUM = 0.0D0
```

A declaration initialiser gives a local the `SAVE` attribute. `CDUM` is
assigned only inside `IF (ISBK)` (`:1326`), but is *stored unconditionally* at
`:1409-1410`:

```fortran
CCCCO(NCL, 1, NCONT) = CDUM
CCCC (NCL, 1, NCONT) = CDUM
```

For an ordinary land column, `CDUM` holds whatever the last **bank** column
processed left there — a value belonging to a different element and, because
`CDUM` is not subscripted by contaminant, to a different contaminant as well.
Cell 1 of `CCCCO` is the designated slot for "effective concentration in the
flow entering the stream via the bed" and is read back by `LINKSM:1947` as
`CCCCO(NBK(JBK),1,NCONT)`. That read is guarded to bank elements, so the
garbage written into land columns is not consumed — but nothing enforces that,
and `CMSIM:748` will overwrite the slot for any element whose
`NLYRBT(NELM,1)` is 1.

Two smaller points in the same three lines:

- The write to `CCCC` at `:1410` is **dead**: `save_conc_loop` at `:1413`
  immediately overwrites `CCCC(NCL,1,NCONT)` with `MAX(1.0D-16, CCAP(1))`.
- Because the write happens inside `cont_loop`, a land column stores the same
  stale `CDUM` into all `NCON` contaminant slices.

Guard the block with `IF (ISBK)` and delete `:1410`.

### 1.7 Three control flags are never assigned

`IS_CC` declares `ISADNL`, `ISFLXB` and `ISPLT` (`is_cc.f90:3,5,6`). A grep
over `src/` finds **no assignment to any of them**: the only writes are
`CMRD:253` and `CMRD:305`, which target *local* declarations (`:221-222`) that
shadow the module flags.

These are not cosmetic. Each selects a different set of equations:

| Flag | Read at | What it selects |
|---|---|---|
| `ISFLXB` | `COLM:1023`, `COLMSM:1241` | Flux vs. prescribed-concentration lower boundary — a *different bottom equation* |
| `ISADNL` | `SLVCLM:2931`, `COLMSM:1272,1276,1421`, `LINKSM` (12 `FRET`/`RET` calls) | Linear vs. Freundlich adsorption, and whether `SLVCLM` runs its 10 Picard iterations at all |
| `ISPLT` | `CMSIM:719`, `COLMSM:1341` | Whether the plant-uptake path runs |

All three are static module storage, so gfortran and ifx will in practice zero
them at load and every run takes the `.FALSE.` branch — which happens to be the
common configuration. Nothing in the language guarantees it, and the `CM5` and
`CM13` records the manual documents have **no effect whatsoever** today. The
module header records this at `:38-43` and `:1087-1090`; it is repeated here
because it is a precondition for reasoning about anything in Parts 3 and 4.

Fix: delete the local declarations in `CMRD` and let the assignments reach the
module flags; give `ISPLT` a producer or delete the path.

### 1.8 The plant path has three further unguarded reads

All are `ISPLT`-gated and therefore dormant, but they should be settled with
1.5 and 1.7 rather than after them.

**`FLEFT` is never assigned.** `plant_cc.f90:90` declares it "currently
uninitialized"; the sole reader is `PLPREP:3379`:

```fortran
IF (NOTZERO(PFTWO(JPLTY))) THEN
   DELFOU(JPLTY) = ONE
ELSE
   DELFOU(JPLTY) = FLEFT(JPLTY)     ! never written
END IF
```

`DELFOU` is `1.0` by declaration initialiser (`plant_cc.f90:88`), so `PLPREP`
*downgrades* it to an undefined value the moment a canopy disappears —
i.e. exactly when the residual-fraction path matters. `DELFOU` then scales the
compartment-B recycling term in `PLCOLM:2861`.

**`NRBOT` has no lower bound.** `PLCOLM:2813`:

```fortran
NRBOT = NCETOP - NRD(JPLTY)
```

`NRD` is the root-zone cell count by vegetation type (`AL_C.F90:127`) and is
read from input. If `NRD(JPLTY) >= NCETOP`, `NRBOT <= 0` and
`rooted_cell_loop` (`:2834`) reads `PPHI(0)`, `COLCAP(0)`, `KSP(0)` and
`PDZF3(NCL,0,JPLANT)`. `ETmod:492` and `SMmod:644` use the same `NRD` without
a bound either, so the constraint belongs upstream — but `MAX(1, ...)` here
costs nothing.

**Two unguarded denominators**, both recorded in the header at `:2756-2758`:
`QDUM = SUM / (PMDUM * (GMCPAA + D3DUM*GMCPBB))` (`:2851`) and
`F2DUM = PFTWO(JPLTY) / PF2MAX(JPLTY)` (`:2822`). `PMASS` and `PF2MAX` have no
declaration initialiser and are set by `FRmod:INPL`; `PF2MAX` is also divided
by at `FRmod:5516`.

### 1.9 `COLM` reads stale halo values and can divide 0/0

`COLMW`'s `cell_loop` (`:1579`) sets `TTHET`, `PPHI` and friends only up to
`NCETOP`. `COLM:842` reads `PPHI(NC+1)` and `TTHET(NC+1)` at `NC = NCETOP` —
values left over from the **previous column** in the sweep.

Numerically this is neutralised today because `COLMSM:1168` sets
`DDOD(NCETOP+1) = zero`, which zeroes the numerator of the harmonic mean at
`:906-908`:

```fortran
OCAPP = two * PPHITH*DDOD(NC) * PPHTHP*DDOD(NC+1) * KSP(NC)*KSP(NC+1) /   &
        (PPHITH*DDOD(NC)*KSP(NC+1) + PPHTHP*DDOD(NC+1)*KSP(NC))
```

— *unless the denominator is also zero*. With `DDOD(NC+1) = 0` the denominator
reduces to `PPHITH*DDOD(NC)*KSP(NC+1)`, and `PPHITH = PPHI(NC)*TTHET(NC)`. A
top cell with `VSTHEO == 0` gives `0/0`. The same applies to any interior pair
of cells both at zero water content. There is no guard, and no `NOTZERO` test,
on either `OCAPP` or `OCAPP1`.

Two further unguarded divisions in the same loop body: `FCAPC` divides by
`COLCAP(NC)` (`:882`) and `GCAPS` by `SOLCAP(NC)` (`:892`). Those are safe only
because of the `MAX(1.0D-16, ...)` floor at `:1415-1416` — which is itself
flagged "temporary" in the source and is a mass-balance defect in its own
right, since it silently creates contaminant whenever the solved concentration
goes negative.

### 1.10 `COLMW`'s `find_jal` is an unbounded search

`:1534-1538`:

```fortran
JAL = 0
find_jal: DO
   JAL = JAL + 1
   IF (ICMREF(NLINKA, JAL + 4) == NCL) EXIT find_jal
END DO find_jal
```

`ICMREF` is `(NELEE, 12)` (`AL_G.F90:46`); the valid face columns are 5:8, i.e.
`JAL` in 1:4. There is no bound. If element `NCL` does not appear in
`ICMREF(NLINKA, 5:8)` — a bank whose link's neighbour list is inconsistent —
the loop walks column 9, 10, 11, 12 and then off the end of the array, reading
whatever follows `ICMREF` in memory until it happens to match `NCL` or the
process faults. `JFLINK = ICMREF(NLINKA, JAL+8)` at `:1540` then reads further
out still.

`DO JAL = 1, 4 ... END DO` followed by an `ERROR` call costs nothing and turns
a silent walk-off into a diagnostic.

### 1.11 The module writes index `NCETOP+1` into `LLEE`-sized arrays

`VSmod`'s documented precondition is `LL <= LLEE` (`VSmod.f90:4168`), i.e.
`top_cell_no` may legitimately equal `LLEE = 50`. `CMmod` requires
`top_cell_no <= LLEE - 1`:

| Site | Access | Array extent |
|---|---|---|
| `COLMW:1622` | `KSP(NCETOP + 1)` | `KSP(LLEE)` `colm_c2.f90` |
| `COLMSM:1151` | `COLCAP(NCETOP+1)`, `SOLCAP(NCETOP+1)` | `(LLEE)` `colm_cc.f90` |
| `COLMSM:1168-1169` | `DDOD(NCETOP+1)`, `DDOD1(NCETOP+1)` | `(LLEE)` |
| `COLMSM:1206` | `CCAPA(NCEPSF+1, JA)`, `CCAPAT(...)` | `(LLEE,4)` |
| `COLMW:1709-1711` | `QQ(NCETOP+1,JA)`, `QQ1(...)`, `DUMMY(NCETOP+1)` | `(LLEE,4)` |
| `COLM:842` | `PPHI(NC+1)`, `TTHET(NC+1)` at `NC = NCETOP` | `(LLEE)` |

Either the shared precondition should be tightened to `top_cell_no <= LLEE-1`
and enforced where `top_cell_no` is established, or these arrays should be
declared `(LLEE+1)`. As it stands the bounds-checked build cannot be used to
validate any other change in this module on a 50-cell configuration.

### 1.12 The link path indexes element 0 when a link has no bank

`LINKW:2285-2286` sets `NBK(JBK) = NBANK(NLINK, JBK)`. `NBANK` is written only
at `FRmod:4423`, inside `IF (ITYPEA == 1 .OR. ITYPEA == 2)` — i.e. only where
an adjacent element actually is a bank, which requires `BEXBK`
(`FRmod:682`). `NBANK` is a plain module array (`bk_cw.f90:36`) with no
initialiser, so for a link with no bank it holds 0.

Nothing in `CMmod` tests that. `LINKSM:1947` then evaluates

```fortran
CCPBK(JBK, 1) = CCCCO(NBK(JBK), 1, NCONT)
```

with `NBK = 0`, reading before the start of an allocatable whose lower bound is
1. `LINKSM:1952-1953`, `LINKW:2437` (`QVSH(JFDUMB, NCE, NBK(JBK))`),
`LINKW:2445` (`NVC(NBK(JBK))`), `LINKW:2450`, `:2464`, `:2469` and
`LINK:2563` all do the same.

`LINKW:2433` has the matching problem on the face index: `JFDUMB =
ICMREF(NLINK, JFDUM+8)` is 0 for an external face, and `QVSH(0, NCE, ...)` at
`:2437` is out of range in the first subscript.

"Contaminant transport requires `BEXBK`" may well be the intended contract, but
it is not stated in the module header and not checked anywhere. One test in
`LINKW`, calling `ERROR`, closes all seven sites.

### 1.13 `LINKW` integrates both banks from bank 2's base cell

`:2446` sets `NDUM = NCEBD(NLINK, JBK) + 1` inside `banks_loop`. After the loop
`NDUM` holds bank **2**'s value. `bed_cells_loop` (`:2460`) then starts *both*
banks from it:

```fortran
bed_cells_loop: DO JBK = 1, 2
   NCE  = NDUM                                    ! bank 2's base, for both
   DUMK = (ONE - FNCEBD(NLINK, JBK)) * KSPBK(JBK, NCE)
```

When `NCEBD(NLINK,1) /= NCEBD(NLINK,2)`, bank 1 is integrated from the wrong
cell, and — because `kspbk_loop` (`:2449`) only fills `KSPBK(JBK, NCE)` for
`NCE >= NCEBD(NLINK,JBK)+1` — it may read a `KSPBK(1, ·)` entry left over from
the previous link. The result is `THBED(NLINK)` (`:2479`), the bed moisture
content passed to four `FRET` calls in `LINKSM`.

The header records this at `:2204-2208`. The fix is to recompute
`NDUM = NCEBD(NLINK, JBK) + 1` inside `bed_cells_loop`.

`:2479` also divides `SUM / SUMK` with no zero guard; `SUMK` is zero whenever
both banks have `FNCEBD == 1` and `NHBED < NDUM`.

### 1.14 `LINKSM`'s `FCPSW1` indexes a cell array with a contaminant number

`:2116-2117`:

```fortran
FCPSW1(JBK) = RSW(NA, NCONT) + RSWT(NA, NCONT) * TSE + &
              RSWC(NA, NCONT) * (CCCC(NA, NCETOP, NCONT) - CCPBK(JBK, NCONT))
```

`CCPBK` is `(2, LLEE)` (`link_cc.f90`), indexed by bank side and **cell**. The
other three terms all refer to the ground-surface cell `NCETOP`, so the
subtraction is plainly meant to be `CCPBK(JBK, NCETOP)`. With `NCONT <= 3` the
access is in range but selects cell 1, 2 or 3 — for cell 1 that is the
bank-inflow slot from 1.6, not a concentration at all.

`FCPSW1` feeds `DUMP7` in `LINK:2645` and thence the stream-water right-hand
side. The header records the defect at `:1896-1899` as retained behaviour.

### 1.15 `LINKSM` reads `NWELL` and `QQQDUM` before they are defined

`:2005-2006`:

```fortran
QCDUM = (QQQSL1 - QQQDUM) * CCAPI(NCONT)
IF (NWELL /= 0) QCDUM = QCDUM + QQQDUM * CCCCW(NWELL, NCONT)
```

`NWELL` and `QQQDUM` are module variables (`:90`, `:92`) that `LINKW`
**redeclares as locals** (`:2237-2238`) and assigns at `:2418-2423`. The module
versions are never written. `QQQSL1` is *not* shadowed, so that one arrives
correctly.

In practice static zeroing makes `QQQDUM = 0` and `NWELL = 0`, which reduces
the expression to the commented-out legacy line at `:2009`
(`QCP1 = QQQSL1*CCAPI/(D0*Z2*KS)`). The irrigation hand-off the code was
written for therefore never happens. Deleting the two local declarations in
`LINKW` fixes it — and **changes results** on any model with a link-targeted
well. Recorded in the header at `:46-49` and `:1890-1894`.

### 1.16 `CMRD` does not validate `NCOLMB`

`:295`:

```fortran
NCOLMB (IEL) = IDUM (INDX + 1)
```

The element index `IEL` is checked (`:291`); the cell number is not. `NCOLMB`
becomes `NCEBOT` in `COLMW:1515`, and the solver requires `2 <= NCEBOT <=
NCETOP`:

- `COLMW:1624` writes `KSPP(NCEBOT - 1)`;
- `COLM:927-928` reads `KSPP(NC-1)` and `:954` reads `COLCAP(NC-1)` at
  `NC = NCEBOT`;
- `COLMSM:1151` reads `CCCCO(NCL, NCEBOT-1, NCONT)`.

`NCOLMB = 1` gives index 0 in four `LLEE` arrays; `NCOLMB = 0` or a negative
value gives worse. A value above `NCETOP` makes `main_loop` a zero-trip loop
and leaves `MCAP`, `FCAP`, `GCAP`, `HLT`, `DUMMY` and `PCAPM` undefined when
`COLM:986` reads them.

The default path is safe (`NCED == -1` selects `NLYRBE`, `:281`); only the
`CM11` exception list is unchecked. `IF (NCOLMB(IEL) < 2 .OR. NCOLMB(IEL) >
top_cell_no)` with an `ERROR` call is one line.

Three related gaps in the same routine, all cheap to close:

- `NCON` (`:249`) is never checked against `NCONEE`. It is caught indirectly by
  the epilogue test at `:596` (`NCONCM < NCON` must fail if `NCON > 3`), but
  only after the whole file has been read.
- `NUM_CATEGORIES_TYPES`, `NTAB`, `TABLE_*` and `ISCNSV` are `INTENT(OUT)` and
  are assigned only for `I = 1..NCONCM` and only inside `IF (ISCNSV(I))`. The
  remaining elements are undefined on return.
- The `:CM11` workspace check at `:270` tests `NREQ > NELEE`, but the data is
  read into `IDUM`, which is `(NXEE*NYEE)` = 10⁶. The check is conservative,
  and therefore safe, but it names the wrong array. The `:CM31`/`:CM37` checks
  (`:411`, `:453`) do target `DUMMY(NELEE)` correctly.

### 1.17 The whole sweep is order-dependent, and the order changes every timestep

This is design, not defect, but it constrains everything else so it is worth
stating precisely.

`update_loop` (`:723`) walks `ISORT`, and `run_sim:294` calls `FRSORT`
unconditionally on **every** timestep, so `ISORT` is a fresh permutation each
step. Within the sweep, elements read each other's **new** values:

| Site | Reads | Of |
|---|---|---|
| `COLMSM:1196` | `CCCC(NWORK(JA), NCETOP, NCONT)` | a neighbouring column |
| `COLMSM:1227` | `CCCC(NLINKA, NCETOP, NCONT)` | the adjacent link |
| `COLMSM:1319` | `CCCC(NLINKA, NCETOP-2, NCONT)` | the adjacent link's deep bed |
| `LINKSM:1956` | `CCCC(NBK(JBK), NCETOP, NCONT)` | an adjacent bank |
| `LINKSM:1976` | `CCCC(LA, NCETOP, NCONT)` | an adjacent link |
| `LINKSM:2117` | `CCCC(NA, NCETOP, NCONT)` | an adjacent bank |

Each is paired with the corresponding `CCCCO` read to form a *time derivative*
(`CSWAT` at `:1196`, `FCSFA1` at `:1977`). For a neighbour not yet processed,
`CCCC == CCCCO` and the derivative is zero; for one already processed it is the
real change. This is Gauss–Seidel on an arbitrary, timestep-varying ordering.

Consequences:

- Results are not reproducible under any change that perturbs water levels
  enough to reorder `ISORT`, including a change that is otherwise
  bitwise-neutral. This must be accounted for when validating any of the
  changes below.
- The sweep cannot be parallelised element-wise, and the `COLM_*`/`LINK_*` work
  arrays are single-column scratch (module header `:13-16`), so it cannot be
  parallelised at all without duplicating that state.

`analysis_frmod.md` §1.10 separately questions whether `FRSORT`'s merge key is
what was intended; that ordering is this loop's input.

---

## Part 2 — Array layout, capacity and locality

### 2.1 Every CM-owned state array is element-major; shared `ERUZ` has the same mismatch

`total_no_elements` is the *first* subscript of all the state CM owns and
carries across timesteps. The ET-produced `ERUZ` used by CM follows the same
layout:

| Array | Declaration | Inner-loop stride |
|---|---|---|
| `CCCC`, `CCCCO`, `SSSS`, `SSSSO` | `(total_no_elements, top_cell_no+1, NCON)` `CONT_CC.F90:44-47` | `nel` doubles |
| `SSS1`, `SSS2` | same | `nel` doubles |
| `QQO` | `(total_no_elements, top_cell_no+1, 4)` `colm_co.f90` | `nel` doubles |
| `UUAJPO`, `VSTHEO`, `GGAMMO` | `(total_no_elements, top_cell_no+1)` | `nel` doubles |
| `ERUZ` | `(total_no_elements, top_cell_no)` `AL_C.F90:159` | `nel` doubles |
| `FCPBKO`, `GCPBKO` | `(total_no_links, 2, top_cell_no+1, NCON)` | `2·nlf` doubles |

Everything arriving from VS is the other way round and is read correctly:
`VSTHE(NCE,NCL)`, `QVSV(NCE,NCL)`, `DELTAZ(NCE,NCL)`, `ZVSNOD(NCE,NCL)` are all
`(cell, element)` and unit-stride.

`COLMW`'s `cell_loop` (`:1579-1618`) mixes both conventions in one body:

```fortran
KSP(NCE)    = DELTAZ(NCE, NCL) / Z2       ! unit stride
TTHET(NCE)  = VSTHEO(NCL, NCE)            ! stride nel
UUAJP(NCE)  = UUAJPO(NCL, NCE)            ! stride nel
TTHET1(NCE) = VSTHE(NCE, NCL)             ! unit stride
...
VSTHEO(NCL, NCE) = TTHET1(NCE)            ! stride nel
UUAJPO(NCL, NCE) = UUAJP1(NCE)            ! stride nel
GGAMMO(NCL, NCE) = GGAMM1(NCE)            ! stride nel
```

Five strided streams per cell, each consuming a full cache line — and, once
`nel · 8` exceeds the page size, a separate TLB entry — to deliver eight bytes.

The loops **cannot** be interchanged: the cell range is per-element
(`NLYRBT(NCL,JLYR)`, `NCOLMB(NCL)`), so the element loop must stay outer. The
fix is to transpose the arrays to `(top_cell_no+1, total_no_elements)`, which
also aligns them with VS.

Scope of the change: all six `CONT_CC` arrays and all of `COLM_CO` are
`ALLOCATABLE` and allocated in exactly one place each
(`CONT_CC:initialise_cont_cc`, `colm_co.f90:initialise_colm_co`), so the
declarations are trivial. The use sites are:

| File | `CCCC`/`SSSS`-family references |
|---|---|
| `CMmod.f90` | 41 |
| `FRmod.f90` | 21 |
| `MNmod.f90` | 20 |

`COLM_CO` is referenced by the same three files. `ERUZ` additionally reaches
`ETmod`, `VSmod` and `rest.f90` and should be treated separately.

`analysis_mnmod.md` §2.1 declares `CCCC`/`SSSS` out of scope for MN precisely
because CM owns them; this is where that decision has to be made.

### 2.2 `CMSIM`'s save sweep is the single worst-ordered loop in the module

`:736-754`:

```fortran
contaminant_loop: DO NCONT = 1, NCON
   link_save_loop: DO NELM = 1, NLF
      DO NCE = NCETOP - 2, NCETOP
         CCCCO(NELM, NCE, NCONT) = CCCC(NELM, NCE, NCONT)
      END DO
   END DO
   column_save_loop: DO NELM = NLF + 1, NEL
      RSZWLO(NELM) = QVSWEL(NELM)
      DO NCE = NLYRBT(NELM, 1), NCETOP
         CCCCO(NELM, NCE, NCONT) = CCCC(NELM, NCE, NCONT)
         SSSSO(NELM, NCE, NCONT) = SSSS(NELM, NCE, NCONT)
      END DO
   END DO
END DO contaminant_loop
```

The innermost subscript is the **slowest-varying** one. Every iteration reads
one double from each of two arrays and writes one to each of two more, each at
stride `nel · 8` bytes — four cache lines pulled, four doubles used.

For an illustrative 5 000-element, 25-cell, 3-contaminant model that is
`3 × 5000 × 25 × 4 = 1.5 M` cache lines (96 MB of line traffic) to move 3 MB of
data, every timestep. The comment at `:735` ("High-Performance Fix … vectorized
array slices") describes a change that made the loop *look* tidier without
changing its access pattern.

Two independent fixes, both available today:

- **Interchange with a mask.** Hoist a `nlyrbt1(:) = NLYRBT(:,1)` vector once
  and write

  ```fortran
  DO NCE = min_base, NCETOP
     DO NELM = NLF+1, NEL
        IF (NCE >= nlyrbt1(NELM)) THEN
           CCCCO(NELM,NCE,NCONT) = CCCC(NELM,NCE,NCONT)
           SSSSO(NELM,NCE,NCONT) = SSSS(NELM,NCE,NCONT)
        END IF
     END DO
  END DO
  ```

  Unit stride, at the cost of a predicated inner body. Bitwise-identical.
- **Transpose per 2.1**, after which the *current* loop order is already unit
  stride and no restructuring is needed.

Two further points in the same loop:

- `RSZWLO(NELM) = QVSWEL(NELM)` (`:746`) is inside `contaminant_loop` and is
  independent of `NCONT`. It executes `NCON` times per element with the same
  value. The header records this at `:640-642`. Hoist it into its own loop —
  or, better, one whole-array assignment.
- The save range is `NLYRBT(NELM,1)..NCETOP`, but `COLMSM:1151` *consumes*
  `CCCCO` from `NCEBOT-1 = NCOLMB(NELM)-1`. Where `CM11` sets `NCOLMB` below
  `NLYRBT(:,1)`, cells `NCOLMB..NLYRBT-1` of `CCCCO` are never refreshed and
  the solver reads values frozen at initialisation. The two ranges should be
  derived from the same source.

### 2.3 `ERUZ` is the one shared ET/VSS array that is element-major, and `COLMW` gathers a whole column from it

`:1572`:

```fortran
TRAN1(NAQU : NCETOP) = ERUZ(NCL, NAQU : NCETOP)
```

`ERUZ` is `(total_no_elements, top_cell_no)` (`AL_C.F90:159`, allocated at
`:189`) while every other VS array in the same routine is `(cell, element)`.
This one line is a `ncells`-long strided gather per column per timestep — about
25 cache lines to deliver 200 bytes.

`TRAN1` is then read at `:1615` and `:1815`. `ERUZ`'s producer is `ETmod`;
transposing it is a cross-component change and belongs with 2.1, not before it.

### 2.4 `AL_C::DUMMY` — a 2 MB shared input workspace — is used as per-column solver scratch

`COLMW:1711` and `COLMSM:1304-1306, 1322, 1378` index `DUMMY(NCE)` for
`NCE = NCEBOT-1 .. NCETOP+1`. There is no local declaration; this is
`AL_C.F90:143`:

```fortran
DOUBLEPRECISION, DIMENSION(NELEE) :: DUMMY  !! Floating-point workspace for spatial input and validation.
```

Three problems:

- It is 2 MB of permanently resident static storage of which this module uses
  the first ~50 doubles.
- It is the same array `run_sim:308` hands to `SYMAIN` as scratch on the same
  timestep, and the same array `CMRD` receives as its `DUMMY` argument. Nothing
  documents that CM may clobber it mid-timestep. It happens to be safe because
  `SYMAIN` runs to completion before `CMSIM` starts and CM re-zeroes what it
  uses, but the coupling is invisible and one reordering away from a bug.
- `COLM:822` declares a **local scalar** also called `DUMMY`, shadowing the
  array within that one routine. Three distinct `DUMMY`s in one 3 386-line
  module.

A local `DOUBLE PRECISION :: SRC(LLEE)` in `COLMSM` — 400 bytes — replaces the
array use entirely, and lets the `COLMW` use be deleted (see 3.2).

### 2.5 Roughly 260 MB of static storage backs this component, most of it dead

Sizes at compile-time capacity (`NELEE = 250000`, `NLFEE = 20000`,
`NCONEE = 3`, `NSEDEE = 7`, `LLEE = 50`, `NPELEE = 2`):

| Array | Declaration | Bytes | Status |
|---|---|---|---|
| `PDZF3` | `(NELEE,NPELEE,LLEE)` `plant_cc.f90:104` | **200 MB** | live only when `ISPLT` (never — 1.7) |
| `QQRVO` | `(NLFEE,LLEE,2)` `bk_cw.f90:40` | 16 MB | **zero references outside its declaration** |
| `BCPAA`, `BCPBB` | `(NELEE,NPELEE,NCONEE)` | 12 MB each | live only when `ISPLT` |
| `CCPBO`, `CCAPRO` | `(NELEE,NCONEE)` `CONT_CC.F90:33,38` | 6 MB each | **zero references** |
| `CCAPB`, `CCAPE`, `CCAPR`, `CCCCW` | `(NELEE,NCONEE)` | 6 MB each | live |
| `RSW`, `RSWC`, `RSWT` | `(NELEE,NCONEE)` | 6 MB each | live |
| `GMCBBO`, `PFONE` | `(NELEE,NPELEE)` | 4 MB each | `ISPLT` only |
| `ZCOLMB` | `(NELEE)` | 2 MB | live |
| `JBTLYR` | `(NELEE)` `colm_cg.f90` | 1 MB | **zero references** |

`QQRVO`, `CCPBO`, `CCAPRO` and `JBTLYR` are 29 MB of `.bss` that no line of
source reads or writes; a grep over `src/` outside the declaring modules
returns nothing for any of them. Deleting them is free and cannot change
behaviour.

The `ISPLT`-only arrays are another 232 MB that is allocated, first-touched by
the loader, and never used. Whether they should be made allocatable or deleted
depends on 1.7.

Where the live fixed-size arrays go is compiler-dependent, but they are module
data rather than procedure locals, so both gfortran and ifx place them in
static storage; there is no stack-overflow exposure here of the kind
that `analysis_frmod.md` §2.1 notes can arise for procedure locals under
automatic compiler modes.

The local arrays in this module are all small and bounded by `LLEE` or
`NSEDEE`: `COLM`'s `WORKA`/`WORKB` (800 B), `SLVCLM`'s `ELTE`/`PLTE`/`RHTD`
(1.2 kB), `COLMW`'s `Q1`/`TRAN1`/`EMULT` (1.2 kB), `LINKSM`'s nine `NSEDEE`
arrays (500 B), `TRIDAG`'s automatic `GAM(N)`. None warrants attention.

### 2.6 `CCAPA` is indexed cell-outer in `COLM` and face-outer in `COLMSM`

`CCAPA`/`CCAPAT` are `(LLEE,4)` (`colm_cc.f90`). `COLMSM`'s `face_loop`
(`:1171`) is face-outer, cell-inner — unit stride, correct. `COLM`'s
`main_loop`/`face_loop` (`:836`/`:855`) is cell-outer, face-inner, so the four
faces of one cell are 400 bytes apart.

That gives `COLM`'s inner body sixteen concurrent 8-byte-per-outer-iteration
streams (`CCAPA`, `CCAPAT`, `QQ`, `QQ1` × 4 faces), which is at the edge of
what a hardware prefetcher tracks. Transposing to `(4, LLEE)` makes the inner
face loop contiguous and costs four line changes in `COLMSM` and two in `COLM`.
Modest, and much less important than 2.1.

---

## Part 3 — Redundant per-timestep work

### 3.1 `COLMSM`'s `disp_loop` recomputes six per-cell arrays that are almost all constant

`:1159-1166`, executed per cell, per contaminant, per element, per timestep:

```fortran
DDOD(NCE)   = OODO * DISP(NCONT, JSOL(NCE), TTHET(NCE),  UUAJP(NCE-1),  UUAJP(NCE))
DDOD1(NCE)  = OODO * DISP(NCONT, JSOL(NCE), TTHET1(NCE), UUAJP1(NCE-1), UUAJP1(NCE))
AALPSO(NCE) = ALPHA(JSOL(NCE), NCONT)
FFSO(NCE)   = FADS(JSOL(NCE), NCONT)
GGNNSO(NCE) = GNN(NCONT)
KKDSO(NCE)  = KDDSOL(JSOL(NCE), NCONT)
```

`DISP` (`:1843`) ignores all five arguments and returns `3.0D-8`. So `DDOD` and
`DDOD1` are the **same constant** `OODO * 3.0D-8` in every cell of every column
for the whole run, and `GGNNSO` is `GNN(NCONT)` repeated `ncells` times. Of the
six, only three genuinely vary, and those vary by *soil layer*, not by cell —
`JSOL(NCE)` is constant within a layer (`COLMW:1580`).

`DISP` and `PHI` are `PURE` and module-contained, so a good compiler should
inline and fold them; whether it also eliminates the dead `TTHET`/`UUAJP` loads
is not something the source can guarantee, and `-O2` with
`-fp-model=precise` / `-fno-fast-math` (`CMakeLists.txt:681,701`) is
conservative.

This is the placeholder that `CM57`/`CM59`/`CM61` were meant to replace
(`:114`, `:1840-1842`), so the loop should not simply be deleted. But
`GGNNSO(NCEBOT:NCETOP) = GNN(NCONT)` as one slice assignment, and hoisting the
three table lookups to the layer loop, are both correct under the current *and*
the intended `DISP`.

### 3.2 `COLMW`'s face loop zeroes arrays it immediately overwrites

`:1708-1745`:

```fortran
main_face_loop: DO JA = 1, 4
   QQ   (NCEBOT-1 : NCETOP+1, JA) = zero
   QQ1  (NCEBOT-1 : NCETOP+1, JA) = zero
   DUMMY(NCEBOT-1 : NCETOP+1)     = zero
   ...
   DO NCE = NCEBOT, NCETOP
      QQ1(NCE, JA) = Q1(NCE) * (ZONE1 * ROH(NCE) / KSP(NCE))
      QQ (NCE, JA) = QQO(NCL, NCE, JA)
      QQO(NCL, NCE, JA) = QQ1(NCE, JA)
   END DO
END DO
```

- The `QQ`/`QQ1` zeroing covers `NCEBOT..NCETOP`, which the loop at `:1741`
  then overwrites in full. Only the two halo entries `NCEBOT-1` and `NCETOP+1`
  survive — and `COLM` reads `QQ(NC,J)` only for `NC = NCEBOT..NCETOP`
  (`:836`, `:855`), so **even the halo is never read**. All three slice
  assignments are dead.
- `DUMMY` is not read anywhere else in `COLMW`, and `COLMSM:1304` re-zeroes it
  before use. `:1711` is dead, and it is executed four times.

That is `4 × 3 × (ncells+2)` stores per column per timestep for nothing —
around 300 stores per column, or 1.5 M stores per timestep on a
5 000-element model.

### 3.3 `LINK` computes the entire stream-water equation for dry links and discards it

`LINK` builds all twenty coefficients unconditionally and only branches on
wetness at the very end (`:2674`):

```fortran
IF (USCP < HALF) THEN
   CALL SNL3(ONE, ZERO, ZERO, ZERO, ZERO, -DLT-DLTDA, ..., ZERO, QLT+QLTDA, SLT+SLTDA, ...)
ELSE
   CALL SNL3(ALT-DLTDA, ALTSTR, -BLT+ELTDA+HLTDA, -BLTSTR, ZERO, ..., PLT-QLTDA-SLTDA, ...)
END IF
```

The dry call passes literal `ONE`/`ZERO` for `A`, `AS`, `B`, `BS` and `P`. So
`ALT`, `ALTSTR`, `BLT`, `BLTSTR` and `PLT` are **never used** on the dry path.
Tracing their inputs, the following are dead whenever `USCP < HALF`:

| Dead value | Produced at | Feeds only |
|---|---|---|
| `DUMA1`, `DUMA2` | `:2547-2548` | `ALT`, `ALTSTR`, `PLT` |
| `DUMA5` (`SUM4`), `DUMP6` (`SUM5`), `DUMP5` (`SUM3`) | `:2570-2572` | `ALT`, `PLT` |
| `DUMA6` | `:2573` | `ALT`, `ALTSTR`, `PLT` |
| `DUMP1` | `:2615` | `PLT` |
| `DUMP4` (`DSUM`) | `:2641` | `PLT` |
| `DUMP7` | `:2647` | `PLT` |

Because `SUM3`, `SUM4` and `SUM5` are all dead, **the whole `bank_loop`
(`:2556-2568`) is dead work on a dry link** — two banks × up to `NCETOP-NCEBK+1`
cells, reading six arrays (`PCPBK1`, `CCPBK`, `FCPBK`, `GCPBK`, `SCPBK`,
`KSPBK`) per cell. So are `upstream_loop` (`:2623`), `downstream_loop`
(`:2634`) and `bank_sum_loop` (`:2644`), together with the two divisions at
`:2627` and `:2638`.

Dry links are not an edge case — headwaters and ephemeral channels are dry for
much of a run, and `LINKW:2266` sets `USCP = ZERO` whenever
`ARXL(NLINK)/Z2SQ < 1.0D-20`.

A compiler *could* sink these computations into the `ELSE` branch, since they
are all register-resident locals with no side effects. Whether it does depends
on how aggressively it performs partial dead-code elimination across a
100-line straight-line region that also loads module data. Hoisting the block
into `IF (USCP >= HALF)` makes it explicit and is bitwise-neutral.

### 3.4 `LINKW` loads `QDEFF` to multiply it by zero

`:2379-2380`:

```fortran
DUM = ZERO
PCSFA1(JLA) = DUMX * (-QLINK(LA, LENDA(JLA)) - QDEFF(LA, LENDA(JLA)) * DUM) / ACSFA1(JLA)
! NB: CONVECTION WITH DISPERSED SEDIMENTS NEGLECTED
```

and `:2407-2409`:

```fortran
DUMA = ZERO
PCSFM1 = DUM * (QLINK(NLINK, 1) + DUMA * QDEFF(NLINK, 1))
PCSFP1 = DUM * (QLINK(NLINK, 2) + DUMA * QDEFF(NLINK, 2))
```

Four loads from a `(NLFEE,2)` array per link per timestep, each multiplied by a
literal zero. The compiler may fold them; `QDEFF` is module data and cannot
alias the locals, so it probably will. The point is the intent: if dispersed-
sediment convection is to stay disabled, the terms should be deleted with a
comment; if it is to be re-enabled, `DUM`/`DUMA` need a producer. Leaving a
disabled physics term as a multiply by a local zero is the worst of both.

### 3.5 `COLMSM`'s bank source loop always runs one dead iteration

`:1314`:

```fortran
bank_src_loop: DO NCE = NCEAB(NLINKA, JBK), NHBED(NLINKA, JBK) + 1
```

`FRmod:4556` sets `NCEAB(NLINK,JBK) = NHBED(NLINK,JBK)`, so the loop is exactly
two iterations. `COLMW:1791-1793` zeroes `QQRV` and then sets **only**
`QQRV(NCEAB(NLINKA,JBK))`. The second iteration therefore has
`QQRV(NCE) = 0`, giving `DUM1 = DUM2 = DUM3 = QCDUM = 0` and contributing
nothing to `SUMQ`, `SUM` or `DUMMY(NCE)`.

Trivial in cost. It is listed because the loop bound and the initialisation
disagree about how many bank cells exchange with the stream, and one of the two
is wrong.

### 3.6 `COLM` computes four coefficients for the top cell and then discards them

`main_loop` (`:836-960`) runs to `NC = NCETOP` and assigns all ten coefficient
arrays. The block at `:1012-1019` then recomputes `DLT`, `ELT`, `ELTSTR` and
`SLT` for that same `NCADJ`. The loop's versions are dead.

More importantly, the code after the loop depends on **fifteen** loop-carried
scalars (`MCAP`, `MCAPT`, `MCAPC`, `FCAP`, `FCAPT`, `FCAPC`, `GCAP`, `GCAPT`,
`HLT`, `HLT1`, `BPGSG`, `BMGSG`, `PCAPM`, `PCAPM1`, `DUMMY`) surviving from the
last iteration. That is legal and intentional, but it means an empty column
(`NCEBOT > NCETOP`, reachable per 1.16) reads fifteen undefined values, and it
makes the routine impossible to restructure safely without first naming the
dependency.

The comments at `:1021` and `:1040` are also swapped: `:1021` says "top cell"
above `NC = NCEBOT`, and `:1040` says "bottom cell" above the row-count
calculation.

### 3.7 `COLMW` reloads a neighbour index it already has

`convection_loop` (`:1685-1700`) computes `NELMA = ICMREF(NCL, JA+4)` for each
face and stores the resolved neighbour in `NWORK(JA)`. `main_face_loop:1726`
then re-reads the same element:

```fortran
NCLA = ICMREF(NCL, JA + 4)
```

`ICMREF` is `(NELEE,12)` — column stride 1 MB — and `NCL` follows `ISORT`, so
this is four extra randomly-addressed loads per column per timestep. Keeping
`NELMA` in a 4-element local removes them.

---

## Part 4 — Arithmetic hot spots

This is the part of the module where the cost is genuinely in the FPU rather
than in the memory system, and it is where the largest single win is.

### 4.1 `COLM` calls `pow()` twice per cell per contaminant per element per timestep

`:880` and `:890`:

```fortran
FCAP = PPHITH + FFKD * COLCAP(NC)**GGNMON
GCAP = TTHT - PPHITH + (KKD - FFKD) * SOLCAP(NC)**GGNMON
```

`GGNMON = GGNNSO(NC) - one` (`:846`) — and `GGNNSO(NC)` is `GNN(NCONT)` for
every cell (3.1). `GNN` is read once by `CMRD:509` and never changes.

A `**` with a `DOUBLE PRECISION` exponent compiles to a `pow()` library call —
roughly 50–100 cycles, not vectorisable, and a call boundary that blocks
optimisation of the surrounding loop body. Two of them, in the innermost loop
of the column path.

For an illustrative 5 000-element, 25-cell, 3-contaminant model that is
**750 000 `pow()` calls per timestep**, or on the order of 50–75 M cycles —
around 20 ms per timestep at 3 GHz, before any other work.

The linear-adsorption case makes both terms vanish:

```fortran
GNN == 1  ⇒  GGNMON == 0  ⇒  x**0 == 1
          ⇒  FCAP  = PPHITH + FFKD
             FCAPC = GGNMON * (FCAP - PPHITH) / COLCAP(NC) = 0
```

so hoisting a `LOGICAL :: linear = ISZERO(GGNMON)` out of `main_loop` (it is
invariant over the whole timestep, not just the loop) and branching gives:

```fortran
IF (linear) THEN
   FCAP  = PPHITH + FFKD
   FCAPC = zero
   GCAP  = TTHT - PPHITH + (KKD - FFKD)
   GCAPS = zero
ELSE
   ... existing expressions ...
END IF
```

This is **bitwise-identical** in the linear case (`x**0.0` is exactly 1.0 for
any finite `x`, and `0.0 * y` is exactly 0.0 for finite `y`), and unchanged in
the nonlinear case. Given `ISADNL` is never true (1.7), and that a Freundlich
exponent of 1 is the ordinary configuration, this branch is expected to be
taken essentially always.

`COLMSM:1429-1430` has the same pattern (`COLCAP(NCE)**GNDUM`,
`SOLCAP(NCE)**GNDUM`) in `fcpbk_loop2`, but that loop is already guarded by
`ISADNL` and covers only exposed bank cells.

`RET:3019` and `FRET:3256` each evaluate `C**(GN - TWO)`, but both are inside
`IF (ISNL)` branches.

### 4.2 `SNL3` performs 103 fixed-point iterations with no convergence test

`:3095-3099`:

```fortran
iteration_loop: DO NJ = 1, 100
   X1 = (P + (B + BS*X2)*X2 + C*X3) / (A + AS*X1)
   X2 = (Q + (D + DS*X1)*X1 + (F + FS*X3)*X3) / (E + ES*X2)
   X3 = (S + (H + HS*X2)*X2) / (AY + AYS*X3)
END DO iteration_loop
```

followed by `stability_loop` (`:3130`), three more of the same. **103
iterations × 3 divisions = 309 divisions per link, per contaminant, per
timestep**, plus about 1 700 other flops. There is no residual test inside the
loop and no early exit.

For 500 links and 3 contaminants that is 150 000 divisions per timestep — at
~20 cycles each, roughly 3 M cycles. Smaller than 4.1, but it is pure waste:
a Gauss–Seidel fixed-point iteration on a 3×3 system that converges at all
converges in single-digit iterations, and once it reaches its fixed point the
remaining iterations reproduce the same value to the last bit.

The safe form is to keep the 100-iteration cap and add an exit on a *relative*
change below a few ulps:

```fortran
DO NJ = 1, 100
   X1OLD = X1;  X2OLD = X2;  X3OLD = X3
   ... three updates ...
   XREF = ABS(X1) + ABS(X2) + ABS(X3)
   IF (NOTZERO(XREF) .AND. &
       (ABS(X1-X1OLD)+ABS(X2-X2OLD)+ABS(X3-X3OLD)) <= 1.0D-14 * XREF) EXIT
END DO
```

This is **not** bitwise-neutral in general — a fixed-point iteration can
oscillate in the last bit without settling — but it is neutral wherever the
iteration has actually converged, which is the case the routine's own
`stability_loop` already tests for. It should be validated by comparing
against the unmodified routine on a full run, not assumed.

Note also that `stability_loop` runs its three extra iterations
*unconditionally* and compares against `X1OLD`/`X2OLD`/`X3OLD` captured once
before the loop, printing the diagnostic on each of the three passes. That is
a debug artefact costing 3/103 of the routine on every call.

### 4.3 `PLCOLM` divides by a factor that cancels algebraically

`:2838-2844`:

```fortran
TDUM  = CDUM + SDUM
DUM   = Z2DUM * PDZF3(NCL, NCE, JPLANT) * TDUM
SUM   = SUM + DUM

EDDUM = DUM * F1DUM / (TDUM * (Z2 * KSP(NCE)))
EDCAP(NCE) = EDCAP(NCE) + CDUM * EDDUM
ESCAP(NCE) = ESCAP(NCE) + SDUM * EDDUM
```

Substituting `DUM`, the `TDUM` factor cancels exactly:

```
EDDUM = Z2DUM · PDZF3 · F1DUM / (Z2 · KSP(NCE))
```

`DUM` itself is still needed for `SUM`, but `EDDUM` should be written without
`TDUM`. That removes one division per rooted cell per contaminant per element
per timestep **and** removes the `0/0` the header warns about at `:2756-2757`:
when `XXI = 0` and `SOLCAP(NCE) = 0` — or, once `XXI` is given a value, when
both regions are at zero concentration — `TDUM` is zero and `EDDUM` is `NaN`,
where the algebraic limit is finite.

Reachable only when `ISPLT` is true (1.7), but it is the clearest example in
the module of a removable singularity and it costs one line.

### 4.4 `SNL3`'s two unsuppressed diagnostics can turn the run I/O-bound

`:3121-3122` and `:3139-3140` are bare `PRINT` statements. Only diagnostic 3
(`:3165-3172`) has a suppression counter. A badly conditioned model emits up to
**two lines of unbuffered stdout per link per contaminant per timestep** from
diagnostics 1 and 2 — on a 500-link, 3-contaminant model, 3 000 lines per
timestep.

The header at `:3059-3063` already records that the word `FATAL` in these
messages is inaccurate (the routine neither stops nor calls `ERROR`). Whatever
is decided about the messages themselves, they need the same counter
diagnostic 3 has.

The saved `COUNT` (`:3086`) also makes `SNL3` stateful and non-reentrant, which
matters if the sweep is ever parallelised.

### 4.5 `COLM`'s inner loop divides by loop-invariant quantities

`main_loop` performs, per cell:

| Site | Division | Invariance |
|---|---|---|
| `:881` | `(PPHIT1 - PPHITH) / TSE` | `TSE` fixed for the whole timestep (`CMSIM:699`) |
| `:891` | `(TTHT1 - PPHIT1 - TTHT + PPHITH) / TSE` | same |
| `:882` | `/ COLCAP(NC)` | varies |
| `:892` | `/ SOLCAP(NC)` | varies |
| `:906-908` | harmonic mean | varies |
| `:911-913` | harmonic mean | varies |
| `:923` | `one / KSP(NC)` | varies |

Seven divisions per cell, of which two are by `TSE`. `TSE` is set once per
timestep in `CMSIM:699` and is a module variable, so the compiler cannot hoist
`1/TSE` out of the loop without proving no aliasing store to `COLM_C1::TSE` —
which it cannot, since `SLVCLM` and `PLCOLM` are called from the same call
tree. A local `OOTSE = one / TSE` at the top of `COLM` turns two divisions per
cell into two multiplies. `TSE` is also divided by at `:862`, `:993`, `:1001`,
`:1030`, `:1037`, `:1196`, `:1227`, `:1300`, and in `SLVCLM`'s callers.

Note that replacing `x/TSE` by `x*(1/TSE)` is **not** bitwise-neutral unless
`TSE` is a power of two. The safe version of this change is to hoist the
*loads* and keep the divisions, or to accept and validate the reassociation.
Either way it is second-order next to 4.1: it saves perhaps 40 cycles per cell
against `pow()`'s 100–200.

### 4.6 `SLVCLM`'s Picard loop has a fixed count and no convergence test

`:2933-2948` runs exactly ten iterations, each a full `TRIDAG` solve plus three
`n`-length vector expressions with `n` divisions each. Guarded by `ISADNL`, so
it is dead today (1.7). If nonlinear adsorption is ever enabled, this is
`10 × (4n divisions + a Thomas solve)` per cell-column per contaminant per
element per timestep, and it needs the same treatment as 4.2 — a convergence
test — before it is turned on. The header at `:2897-2900` records the absence
of the test.

`TRIDAG` itself (`utilsmod.f90:769`) is clean: `PURE`, explicit-shape dummies,
one reciprocal per row, no copy-in/copy-out, and it does not modify its inputs.
Its `GAM(N)` automatic array is at most `LLEE` doubles. It is not a concern.

---

## Part 5 — Dead code, shadowing and vestigial interfaces

Not performance-relevant, but they obscure the analysis above.

### 5.1 Dead declarations and duplicate imports

**Dead module variable.** `count` (`:87`) is declared, initialised to 0, and
never referenced. `SNL3` has its own separate saved `COUNT` (`:3086`). The
comment on `:87` already says so.

**Duplicate `USE` statements.** `USE IS_CC` appears at `:67` and `:69`;
`USE UTILSMOD` at `:68` (`TRIDAG`) and `:71` (`DCOPY`). Both are legal; both
should be single statements. `USE AL_C` (`:65`) and `USE AL_G` (`:66`) have no
`ONLY` clause, which is what makes `AL_C::DUMMY` reachable by accident (2.4) —
and the Debug build sets `-Wuse-without-only` (`CMakeLists.txt:699`).

**Dead locals.** `LINKSM:1922` declares `LFONE`, `LDUM` and `LA`; only `LA` is
used. `LINKSM:1930` declares `DUMX`, never used — `LINKW` has its own.
`CMRD:218` declares `rubbish(1,1)`, the only rank-2 buffer passed to `ALREDI`
where every other call passes `IDUM`.

**Dead writes.** `COLMSM:1410` (see 1.6). `COLMSM:1206`'s `bdy_loop` runs to
`NCEPSF+1 = NCETOP+1`, one index past anything `COLM` reads (`:836` bounds `NC`
at `NCETOP`), while the non-boundary branch at `:1178` correctly stops at
`MIN(NDUM, NCETOP)`. The two branches should agree.

### 5.2 `SNL3`'s `C` parameter is dead, and two `LINK` coefficients are identical

`SNL3` is `PRIVATE` (only `CMSIM` and `CMRD` are public) and has exactly two
call sites, `LINK:2676` and `LINK:2680`. **Both pass `ZERO` for `C`.** The
`+ C*X3` term in `iteration_loop` (`:3096`), in `stability_loop` (`:3131`) and
in the `PERR` residual (`:3149`) is therefore always zero, and the first
equation never couples directly to `X3`. Either the parameter should be
removed, or a caller that uses it should be written.

In the same routine, `FLTDA` (`:2607`) and `HLTDA` (`:2612`) are both
`-TSE * ACSBD1`, and `GYLTDA` (`:2671`) is its negation. Three names for one
quantity.

`LINK`'s `bank_loop` also carries redundant accumulators: `SUM1` and `SUM2` are
reset per bank (`:2557-2558`) and then immediately summed into `SUM4`/`SUM5`
(`:2566-2567`), so two accumulators would do the work of four.

### 5.3 `PLCOLM` and `COLMSM` zero the same arrays over three different ranges

| Site | Range | Arrays |
|---|---|---|
| `COLMSM:1140` `init_loop` | `1 .. LLEE` | `GNERD`, `GNDSE`, `GND2`, `GNDSE2` |
| `COLMSM:1344` `zero_edcap_loop` | `NCEBOT .. NCETOP` | `EDCAP`, `EDCAPC`, `EDCAPT`, `ESCAP`, `ESCAPS`, `ESCAPT` |
| `PLCOLM:2797` `init_uptake_loop` | `1 .. NCETOP` | the same six |

The two branches of `IF (ISPLT)` at `COLMSM:1341` therefore clear different
ranges of the same six arrays, and `COLMSM:1356`'s `mn_loop` overwrites all six
again over a fourth range when `ISMN` is true. Only `NCEBOT..NCETOP` is ever
read (`COLM:836`). Nothing is wrong, but the inconsistency is a standing
invitation to a stale-value bug and costs `ncells` redundant stores per
contaminant per element per timestep on the `ISPLT` path.

### 5.4 `COLMSM`'s lateral weighting cancels to a plain arithmetic mean

`layer_loop` (`:1183-1188`):

```fortran
DO NOLP = NOLDUM, NOLBT(NCL, NCE+1, JA) - 1
   JCEA  = NOLCEA(NCL, NOLP, JA)
   QDUM  = QQ1(NCE, JA)                       ! invariant in NOLP
   SUMQ  = SUMQ + QDUM
   SUMQC = SUMQC + QDUM * CCCCO(NWORK(JA), JCEA, NCONT)
END DO
IF (NOTZERO(SUMQ)) SUMQ = SUMQC / SUMQ
```

`QDUM` does not depend on the loop variable, so `SUMQC/SUMQ` is the plain
**arithmetic mean** of the adjacent cell concentrations — the flow weighting the
header describes at `:1063-1065` cancels exactly. `JOLFN` (`colm_cg.f90`), the
per-overlap area share, is computed by `FRmod:4401` and is presumably what the
weight was meant to be. Either way this is an expensive way to compute a mean:
one strided `CCCCO` gather and one `NOLCEA` gather per overlap record.

Two related mismatches:

- `CCAPA` is built from `QQ1` (the **new** flow) but `COLM`'s upwind test at
  `:856` uses `QQ` (the **old** flow). Where the two have opposite sign, the
  upwind direction and the averaged concentration disagree.
- Where `QQ1(NCE,JA)` is exactly zero, `SUMQ` stays zero and `CCAPA` is set to
  **zero concentration** rather than to the neighbour's value — which `COLM`
  then convects with the nonzero old flow.

### 5.5 `RET` and `FRET` diverge on the zero-concentration case

`FRET:3233` opens with `IF (ISZERO(C))` and returns storage alone. `RET`
(`:2978`) has no such branch and evaluates `C**(GN - TWO)` directly (`:3019`).
For `GN < 2` and small `C` that overflows; for `C = 0` it is a domain error.

In practice `RET` is only ever called with `COLCAP(NCETOP)` (`COLMSM:1272`,
`:1276`), which the `MAX(1.0D-16, ...)` floor (1.9) keeps strictly positive —
so the divergence is currently masked by a defect elsewhere. With `GN = 0.5`,
`C = 1e-16` gives `C**(-1.5) ≈ 1e24`, which is finite but meaningless.

The two routines are otherwise the same calculation with an extra porosity
correction in `FRET`; they should share the guard. Both were correctly
converted to `DOT_PRODUCT` over explicit-shape `(NSED)` dummies
(`:3009-3010`, `:3243-3244`) and are otherwise efficient.

### 5.6 Stale documentation and unenforced preconditions

**Stale documentation.** The "High-Performance Fix" comment at `:735` claims a
vectorisation that the code does not contain (2.2). `COLM`'s comments at
`:1021` and `:1040` are swapped (3.6). `COLMSM:1115-1122` retains eight lines of
commented-out `COMMON` declarations. `LINKSM:1909` carries a
`! USE CONT_CC ! (Duplicate removed)` comment for a line that is gone.

**Unenforced preconditions.** Beyond 1.16: nothing checks `NCETOP >= 3` before
`CMSIM:739` indexes `NCETOP-2`; nothing checks `NCON <= NCONEE`; nothing checks
`NSED <= NSEDEE` before `LINKSM:2016` and `COLMSM:1261` loop to `NSED`;
nothing checks that `top_cell_no < LLEE` (1.11). `FRmod:MNERR0` performs
exactly these checks for MN's capacities.

---

## Recommended order of work

| Priority | Change | Expected benefit | Numerical risk |
|---|---|---|---|
| P0 | Give `XXI` a producer, or remove the weighting deliberately (1.1) | Correctness: an unassigned variable currently scales a term in **every cell of every column on every timestep** | **Changes results** as soon as `XXI /= 0`; none if the deliberate choice is `XXI = 0` |
| P0 | Give `ESSCAP`/`ESSCPC`/`ESSCPT` a producer, or make them `ZERO` parameters (1.2) | Correctness: three unassigned values are live terms in the top-cell balance | None if replaced by zero; changes results if given a producer |
| P0 | Declare the six `LLEE` arrays `(LLEE+1)`, or tighten `top_cell_no` to `<= LLEE-1` (1.11) | Correctness; **unblocks bounds-checked validation of everything below** | None on models with `top_cell_no < LLEE` |
| P0 | Fix `PDZF3`'s declaration to `(NELEE, LLEE, NPELEE)` (1.5) | Correctness: plant slot 2 currently overwrites slot 1 | None today (`ISPLT` false); **changes results** once the plant path is enabled |
| P0 | Bound `find_jal` and call `ERROR` on failure (1.10) | Turns an out-of-array walk into a diagnostic | None on well-formed meshes |
| P0 | Hoist the sediment-fraction shift out of `cont_loop` in `COLMSM:1261` and `LINKSM:2080` (1.3) | Correctness: contaminants 2..`NCON` currently see no sediment-composition change | **Changes results** for `NCON > 1` — the current values are wrong, not approximate |
| P0 | Move the `IIICFO`/`CCAPIO` shift from `COLMSM` to `CMSIM` (1.4) | Correctness: removes an `ISORT`-order dependency | None while `CCAPI`/`IIICF` are constant; changes the first timestep only |
| P1 | **Branch `COLM:880,890` on `GGNMON == 0`** (4.1) | **Highest** — removes two `pow()` calls per cell per contaminant per element per timestep from every linear-adsorption run | **None** — `x**0.0` is exactly 1.0 |
| P1 | Restructure `CMSIM:736-754` for unit stride, and hoist `RSZWLO` (2.2) | **High** — the module's largest single memory-traffic item, ~8× line-to-datum waste | None — pure reordering, bitwise-identical |
| P1 | Guard `LINK`'s stream-water block with `IF (USCP >= HALF)` (3.3) | High on catchments with many dry links — removes a two-bank cell sweep and three sums per dry link per contaminant per timestep | None — the values are provably unused on that path |
| P1 | Add a convergence exit to `SNL3` (4.2); add a suppression counter to diagnostics 1 and 2 (4.4) | High on link-heavy models; removes an unbounded stdout path | **Not bitwise** — must be validated against the unmodified routine on a full run |
| P1 | Delete the dead zeroing in `COLMW:1709-1711` (3.2); replace `AL_C::DUMMY` with a local (2.4) | ~300 stores per column per timestep; removes an invisible coupling to `SYMAIN` | None |
| P2 | Guard `COLMSM:1409` with `IF (ISBK)` and delete `:1410` (1.6) | Correctness of the `CCCCO(:,1,:)` bank slot; removes a dead store | None where `NLYRBT(:,1) >= 2` |
| P2 | Validate `NCOLMB` in `CMRD:295`; add the missing capacity checks (1.16, 5.6) | Turns four silent overruns into diagnostics | None on valid input |
| P2 | Recompute `NDUM` inside `LINKW`'s `bed_cells_loop`; guard `SUM/SUMK` (1.13) | Correctness of `THBED` on links with asymmetric banks | **Changes results** on such links |
| P2 | Test for `NBK == 0` / `JFDUMB == 0` in `LINKW` (1.12) | Closes seven index-0 reads | None where `BEXBK` |
| P2 | Hoist the invariants out of `disp_loop` (3.1); keep `NELMA` in `COLMW` (3.7); hoist the `TSE` loads in `COLM` (4.5) | Moderate, cheap, local | None if the divisions are kept; reassociating `x/TSE` to `x*(1/TSE)` is **not** bitwise-neutral |
| P2 | Delete `QQRVO`, `CCPBO`, `CCAPRO`, `JBTLYR` (2.5); delete `count`, the duplicate `USE`s, the dead locals, and `SNL3`'s `C` parameter (5.1, 5.2) | 29 MB of `.bss`; removes noise | None — zero references |
| P3 | Resolve `ISFLXB`/`ISADNL`/`ISPLT`: assign the module flags from `CMRD`, or delete the paths (1.7) | Makes `CM5`/`CM13` mean something | **Changes results** on any model that sets them — that is the point |
| P3 | With 1.7: fix `PLCOLM`'s `TDUM` division (4.3), bound `NRBOT`, and give `FLEFT` a producer (1.8) | Prerequisites for re-enabling the plant path | N/A until `ISPLT` is live |
| P3 | Fix `FCPSW1`'s `CCPBK(JBK, NCONT)` → `CCPBK(JBK, NCETOP)` (1.14); delete `LINKW`'s local `NWELL`/`QQQDUM` (1.15) | Correctness of two link source terms | **Changes results** — both current values are wrong |
| P3 | Decide what `layer_loop`'s weighting should be (5.4); reconcile `QQ` vs `QQ1` in the upwind test; give `RET` the `ISZERO(C)` guard `FRET` has (5.5) | Correctness of lateral coupling and of surface retardation | **Changes results** |
| P4 | **Transpose `CONT_CC` and `COLM_CO` to `(cell, element)`** (2.1) | **Largest structural payoff** — makes the entire column path unit-stride, and unblocks `analysis_mnmod.md` §2.1 | Reordering only, but **cross-module**: ~82 sites in `CMmod`, `FRmod`, `MNmod` |
| P4 | Transpose `ERUZ` (2.3) and `CCAPA`/`CCAPAT` (2.6) | Removes the last strided gathers in `COLMW`/`COLM` | `ERUZ` is produced by `ETmod` and also consumed by `VSmod` and `rest.f90`; all four modules must change together |
| P4 | Add a convergence test to `SLVCLM`'s Picard loop before nonlinear adsorption is enabled (4.6); make `SNL3` stateless (4.4) | Prerequisite for 1.7 and for any future parallelisation | N/A until `ISADNL` is live |

For P0 item 1.11, P1 items 4.1 / 2.2 / 3.3 / 3.2 / 2.4, and the P2 deletions,
the appropriate acceptance test is **bitwise-identical output** with an
unchanged sequence of accepted timesteps.

The items that can legitimately change results are 1.1, 1.2, 1.3, 1.4, 1.5,
1.13, 1.14, 1.15, 4.2 and the 5.4/5.5 decisions — each because the current
behaviour is wrong or is an unconverged approximation, not because the fix is
approximate.

One caveat applies to **all** validation of this module. Per 1.17, `CMSIM`
sweeps in `ISORT` order and elements read each other's partially-updated
values, while `run_sim:294` recomputes `ISORT` on every timestep. Any change
that perturbs a water level enough to reorder two elements will change the
answer even when the change is otherwise exact. Bitwise comparison is
meaningful for changes confined to `CMmod` — which is all of the above — but
not for anything that touches VS or OC upstream of it.
