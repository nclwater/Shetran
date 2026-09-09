# Physical constants: duplicates with disagreeing values

Part of the `src/` reorganisation proposal (see `proposal.md`). The agreed
approach is to **move every constant into `mod_parameters` under a distinct
name, preserving its current value exactly**, so that the reorganisation stays
numerically neutral. This document records the disagreements that the move makes
visible, so that unifying them can be decided later on its own merits.

Nothing in this document has been changed in the source. It is a decision aid.

## Summary

| Quantity | Current names and values | Verdict |
|:---------|:-------------------------|:--------|
| Gravitational acceleration | `GRAVTY` = 9.80665, `ROOT2G` = 4.42944 (implies 9.80997), literal `9.81d0` in `SMmod` | Rounding difference only |
| Water density | `RHOWAT` = 998.0, `RHOW` = 1000.0 | Different reference temperature |
| Air density | `RHO` = 1.2, `RHOA` = 1.29 | Different reference temperature |
| Specific heat of air | `CP` = 1003.0, `CPA` = 1003.0 | **Identical — safe to unify** |
| Latent heat of vaporisation | `LAMDA` = 2465000, `LVW` = 2500000 | Different reference temperature |

## The central observation

The two sets are not arbitrarily inconsistent. The evapotranspiration constants
are the values of their quantities at roughly 15–20 °C, and the snow constants
are the values at 0 °C:

| Quantity | ET value | Textbook value at 20 °C | Snow value | Textbook value at 0 °C |
|:---------|---------:|------------------------:|-----------:|-----------------------:|
| Air density (kg/m³) | 1.2 | 1.204 | 1.29 | 1.293 |
| Water density (kg/m³) | (998.0, sediment) | 998.2 | 1000.0 | 999.84 |
| Latent heat of vaporisation (J/kg) | 2465000 | 2454000 (2465000 ≈ 15 °C) | 2500000 | 2501000 |

That is a coherent modelling choice, not an accident: evapotranspiration is
computed for ambient air, and snowmelt for a pack at the melting point. The
sediment module's `RHOWAT` = 998.0 likewise matches water at ~20 °C, which suits
a warm-season transport calculation.

**Recommendation:** do not unify the air-density, water-density or
latent-heat values without a hydrological decision. Treat the differences as
intentional until someone with the science context says otherwise. The
distinct names proposed below make the intent explicit rather than accidental.

## Detail

### 1. Gravitational acceleration — three spellings

| Where | Name | Value | Note |
|:------|:-----|:------|:-----|
| `src/sediment/CONST_SY.F90:18` | `GRAVTY` | `9.80665d0` | Standard gravity. Used throughout `SYmod`. |
| `src/overland_channel/OCmod2.f90:81` | `ROOT2G` | `4.42944d0` | Pre-computed √(2g). 4.42944² / 2 = 9.80997, i.e. it encodes g = 9.81. |
| `src/snow/SMmod.f90:360` | *(literal)* | `9.81d0` | Inside the Richardson-number expression; not a named constant. |

This is the one group where unification is close to free. √(2 × 9.80665) =
4.4286906, against the stored 4.42944 — a relative difference of 1.7 × 10⁻⁴,
which would perturb weir discharge slightly. The `SMmod` literal affects only a
stability correction.

**Proposed:** `GRAVITY = 9.80665d0` in `mod_parameters`, with `SQRT_TWO_G =
4.42944d0` retained separately and documented as encoding 9.81. Replacing
`SQRT_TWO_G` with `SQRT(2*GRAVITY)` and the `SMmod` literal with `GRAVITY` are
two small, independent follow-up changes, each of which shifts results.

### 2. Water density

| Where | Name | Value | Consumers |
|:------|:-----|:------|:----------|
| `src/sediment/CONST_SY.F90:20` | `RHOWAT` | `998.0d0` | `SYmod` transport capacity, critical shear, settling velocity |
| `src/snow/SMmod.f90:83` | `RHOW` | `1000.0d0` | `SMmod` energy-budget melt |

`RHOWAT` enters the sediment relations as ratios and differences against
`RHOSED` = 2650, so changing it to 1000.0 would move `K1_syovtr`,
`K3_syovtr`, `KG_syengh` and `K2_sycrit` and therefore every sediment yield.

**Proposed:** `RHO_WATER_SEDIMENT = 998.0d0`, `RHO_WATER_SNOW = 1000.0d0`.

### 3. Air density

| Where | Name | Value | Consumers |
|:------|:-----|:------|:----------|
| `src/evapotranspiration/ETmod.f90:90` | `RHO` | `1.2` | Penman aerodynamic term |
| `src/snow/SMmod.f90:82` | `RHOA` | `1.29d0` | Turbulent heat exchange over snow |

A 7.5 % difference. Both appear in linear flux terms, so unifying would shift
evaporation or snowmelt by a comparable fraction.

**Proposed:** `RHO_AIR_ET = 1.2d0`, `RHO_AIR_SNOW = 1.29d0`.

Note `RHO` is declared as a default-real literal `1.2` in a `DOUBLEPRECISION`
parameter, so it carries only single-precision digits (1.2000000476837158…).
Writing it as `1.2d0` would itself be a tiny numerical change. This is worth
fixing but is not free.

### 4. Specific heat of air — the one safe merge

| Where | Name | Value |
|:------|:-----|:------|
| `src/evapotranspiration/ETmod.f90:91` | `CP` | `1003.` |
| `src/snow/SMmod.f90:84` | `CPA` | `1003.0d0` |

Numerically identical apart from the literal kind (`1003.` is default real,
`1003.0d0` is double precision; both are exactly representable, so the values
agree bit for bit).

**Proposed:** a single `CP_AIR = 1003.0d0`. This is the only merge in this
document that changes nothing. It is listed in `variables.csv` as two distinct
names (`CP_AIR_ET`, `CP_AIR_SNOW`) so that the mechanical move stays uniform;
collapsing them to one is a safe one-line follow-up.

### 5. Latent heat of vaporisation

| Where | Name | Value | Consumers |
|:------|:-----|:------|:----------|
| `src/evapotranspiration/ETmod.f90:88` | `LAMDA` | `2465000.` | Penman equation |
| `src/snow/SMmod.f90:88` | `LVW` | `2500000.0d0` | Snow sublimation/condensation term |

A 1.4 % difference, consistent with ~15 °C versus 0 °C.

**Proposed:** `L_VAPORISATION_ET = 2465000.0d0`,
`L_VAPORISATION_SNOW = 2500000.0d0`.

## Constants with no conflict

Moved to `mod_parameters` under clearer names; values unchanged.

| Current | Proposed | Value | Source |
|:--------|:---------|:------|:-------|
| `RHOSED` | `RHO_SEDIMENT` | 2650.0 | `CONST_SY` |
| `VISCOS` | `NU_WATER` | 1.0e-6 | `CONST_SY` |
| `GAMMA` | `PSYCHROMETRIC_CONSTANT` | 0.659 | `ETmod` |
| `CPW` | `CP_WATER` | 4187.0 | `SMmod` |
| `CPI` | `CP_ICE` | 2093.0 | `SMmod` |
| `LWI` | `L_FUSION` | 334000.0 | `SMmod` |
| `HFG` | `GROUND_HEAT_FLUX_SNOW` | 2.0 | `SMmod` |
| `F23` | `TWO_THIRDS` | 2/3 | `OCmod2` |
| `F53` | `FIVE_THIRDS` | 5/3 | `OCmod2` |

`HFG` is a calibration value rather than a physical constant, but it is
declared alongside the others and has a single fixed value; the `_SNOW` suffix
keeps that visible.

`GAMMA` has the same defect as `RHO` in §3: it is written as the default-real
literal `0.659` in a `DOUBLEPRECISION` parameter, so it stores
0.6589999794960022. `ETmod`'s other two default-real literals, `CP = 1003.` and
`LAMDA = 2465000.`, are exactly representable and so carry no error. Writing
`GAMMA` as `0.659d0` is a tiny numerical change, on the same footing as the
`RHO` fix.

## Constants deliberately *not* moved

These are numerical tolerances or component sizing values, not physical
constants, and stay with the code that defines their meaning:

| Name | Stays in | Why |
|:-----|:---------|:----|
| `DZMIN`, `RDZMIN`, `H23MIN` | `oc_conveyance` | Overland/channel depth thresholds. They sit with `CONVEYAN` rather than in `oc_discharge` because `OCFIX`, in `oc_node_solver`, reads `DZMIN`; see the cycle table in `proposal.md`. |
| `eps` | `linear_algebra` | Matrix-singularity tolerance |
| `errcntallowed` | `vs_column_solver` | Convergence-warning limit, read only by `VSCOLM` |
| `K1_syovtr`, `K3_syovtr`, `K4_syovtr` | `sy_transport_capacity` | Derived transport coefficients |

`vsmall` is a comparison tolerance rather than a physical constant, but it is
genuinely shared: it moves from `sglobal` to `mod_parameters`, where
`float_compare` and the components can all see it.
