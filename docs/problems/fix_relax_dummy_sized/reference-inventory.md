# Reference inventory

Measured at commit `e09f465`, Debug binary, gfortran 16.2.1. Consult this; do
not edit it as work proceeds — record new measurements in commit messages and
in each phase's own file.

## The fifteen remaining `IDUM`/`DUMMY` workspaces

Reproduce the symbol list with:

```bash
nm --size-sort -S build/debug/bin/shetran | grep -E '\b(idum|dummy)\.[0-9]+'
```

| Owner | File | Array | Extent | Bytes | Phase | True requirement |
|:------|:-----|:------|:-------|------:|------:|:-----------------|
| `INCM` | `contaminant/cm_input.f90` | `IDUM` | `NXEE*NYEE` | 4,000,000 | 5 | `NX*NY` |
| `INCM` | `contaminant/cm_input.f90` | `DUMMY` | `NELEE` | 2,000,000 | 5 | largest `NREQ` in `CMRD` |
| `SIMULATION` | `driver/simulation_driver.f90` | `IDUM` | `NXEE*NYEE` | 4,000,000 | 4 | `MAX(total_no_elements, NLFEE)` |
| `SIMULATION` | `driver/simulation_driver.f90` | `DUMMY` | `NELEE` | 2,000,000 | 4 | `MAX(total_no_elements, NREQ_syread)` |
| `VSREAD` | `subsurface/vs_input.f90` | `IDUM` | `NXEE*NYEE` | 4,000,000 | 6 | `NX*NY` |
| `VSREAD` | `subsurface/vs_input.f90` | `DUMMY` | `NELEE` | 2,000,000 | 6 | `MAX(5, 2*NW, 3*NSP)` after phase 1 |
| `INBK` | `overland_channel/bank_setup.f90` | `IDUM` | `NELEE` | 1,000,000 | 7 | `total_no_elements` |
| `INBK` | `overland_channel/bank_setup.f90` | `DUMMY` | `NELEE` | 2,000,000 | 7 | `total_no_elements` |
| `VSCONC` | `subsurface/vs_connectivity.f90` | `IDUM` | `NELEE` | 1,000,000 | 6 | `total_no_elements` |
| `JEOCBC` | `overland_channel/oc_input.f90` | `IDUM` | `NELEE` | 1,000,000 | 7 | `total_no_elements` |
| `OCREAD` | `overland_channel/oc_input.f90` | `IDUM` | `NELEE` | 1,000,000 | 7 | `MAX(total_no_elements, NOCTAB)` |
| `OCREAD` | `overland_channel/oc_input.f90` | `DUMMY` | `NELEE` | 2,000,000 | 7 | `total_no_elements` |
| `OCINI` | `overland_channel/oc_driver.f90` | `DUMMY` | `NLFEE` | 160,000 | 7 | `MAX(total_no_links, NOCTAB)` |
| `OCCHK1` | `overland_channel/oc_validation.f90` | `IDUM` | `NXEE` | 4,000 | 8 | `NXEE`, local |
| `COLMSM` | `contaminant/cm_column.f90` | `DUMMY` | `LLEE` | 400 | 8 | `LLEE`, local |
| | | | **total** | **26,164,400** | | |

## Logical check-mask scratch

| Owner | File | Array | Extent | Bytes | Phase |
|:------|:-----|:------|:-------|------:|------:|
| `OCINI` | `overland_channel/oc_driver.f90` | `LDUM1` | `NELEE` | 1,000,000 | 2 |
| `sy_workspace` | `sediment/sy_workspace.f90` | `LDUM` | allocatable | 64 | 2 |

Plus a dozen one-element `LDUM(1)` / `LDUM1(1)` placeholders across
`cm_input`, `mn_input`, `mn_validation`, `sy_validation`, and `LDUM2(LLEE)` in
`mn_driver`.

## Placeholder-argument counts

| Callee | Sites | Data args | Dead | Phase |
|:-------|------:|----------:|-----:|------:|
| `ALREAD` | 52 | 156 | 103 | 1 |
| `ALCHK` | 137 | 137 | 137 | 2 |
| `ALCHKI` | 97 | 97 | 97 | 2 |
| | | | **337** | |

`ALREAD` sites by file: `subsurface/vs_input.f90` 30,
`sediment/sy_input.f90` 16, `io/spatial_fields.f90` 6.

`ALREAD` sites by `FLAG`, with the arguments each mode actually uses:

| `FLAG` | Sites | Uses |
|-------:|------:|:-----|
| -1 | 1 | none |
| 0 | 1 | none |
| 1 | 3 | `CDATA` |
| 2 | 21 | `IDATA` |
| 3 | 16 | `RDATA` |
| 4 | 6 | `IDATA` |
| 5 | 1 | `RDATA` |
| 6 | 2 | `IDATA`, `RDATA` |
| 7 | 1 | `IDATA`, `RDATA` |

## Related routines and their workspace dummies

| Routine | File | Workspace dummy | Note |
|:--------|:-----|:----------------|:-----|
| `ALREAD` | `io/record_readers.f90` | `IDATA(N1,N2)`, `RDATA(N1,N2)` | explicit shape; correct, keep |
| `ALREDI` | `io/record_readers.f90` | `IDATA(N1,N2)` | one data arg, no placeholder problem |
| `ALREDF` | `io/record_readers.f90` | `FDATA(N1,N2)` | as above |
| `ALALLI` | `io/spatial_fields.f90` | `IDUM(*)` | already assumed-size — the model to follow |
| `AREADI` | `io/grid_arrays.f90` | `IAOUT(:)` | already assumed-shape |
| `AREADR` | `io/grid_arrays.f90` | `AOUT(:)` | already assumed-shape |
| `ALCHK` | `io/input_validation.f90` | `OBJ(N0:*)`, `SUBJ(N0:N1)`, `NOTOK(N0:N1)` | `NOTOK` never read by any caller |
| `ALCHKI` | `io/input_validation.f90` | as above, integer | as above |
| `DCOPY` | `util/linear_algebra.f90` | `dx(*)`, `dy(*)` | 48 sites; 24 take an array element of a workspace |

## Out of scope, noted for follow-up

Larger static objects than anything in this plan, none of them touched:

| Symbol | Size |
|:-------|-----:|
| `cm_plant_state:PDZF3` | 191 MB |
| `mn_state:MN_PLANT_STATE` (and its `__def_init` twin) | 137 MB each |
| `vsthen` / `vspsin` / `cq` (saved locals) | 95 MB each |
| `vs_config:JVSALN` | 76 MB |
| `sy_state:QSED` | 53 MB |
| `vs_connectivity:LRENUM` | 19 MB, implicitly `SAVE` via initialiser |

Total `.bss` at `e09f465` is 1,637,961,200 B. This plan addresses about 1.6% of
it.
