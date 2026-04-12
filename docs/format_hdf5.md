# SHETran HDF5 output format

This document describes the HDF5 structure written by the visualisation pipeline in this repository.
It is based on the current implementation in:

- src/visualisation/visualisation_hdf5.f90
- src/visualisation/visualisation_metadata.f90
- src/visualisation/visualisation_structure.f90
- src/visualisation/visualisation_interface_centre.f90

## 1. Top-level structure

SHETran creates one HDF5 file (path provided via `hdf5filename`) and creates these top-level groups:

- CONSTANTS (required)
- VARIABLES (required)
- CATCHMENT_MAPS (conditionally created on first map write)
- CATCHMENT_SPREADSHEETS (conditionally created on first spreadsheet write)

In normal runs, both optional groups are expected to appear because static variables `surf_elv` and `number` trigger their creation.

## 2. Required groups and their content

## 2.1 CONSTANTS group

Purpose:

- static (time-invariant) outputs.

Children:

- one dataset per static variable
- dataset name is the variable `name` (for example `surf_elv`, `number`, `centroid`)

No per-variable subgroup is used for constants.

## 2.2 VARIABLES group

Purpose:

- dynamic (time-varying) outputs.

Children:

- one subgroup per configured dynamic item in visualisation_plan.

Dynamic subgroup naming:

- base format: `"<users_number width 3> <name>"`
- if variable varies with sediment: append sediment number to name (example pattern: `s_dis 3`)
- if variable varies with contaminant: append contaminant number to name

Inside each dynamic subgroup:

- `value` dataset (required)
- `time` dataset (required)

`time` dataset:

- datatype: float
- shape: 1D extensible
- units attribute: `hours`

`value` dataset:

- datatype: integer or float depending on variable type
- first dimension is extensible time
- other dimensions are fixed by item metadata

## 3. Optional visualisation helper groups

## 3.1 CATCHMENT_MAPS

Purpose:

- image-like indexed map products.

Created when:

- `surf_elv` is written.

Typical dataset name:

- `SV<ver>_elevation`

Palette dataset:

- `palette1` (indexed color palette)

Image metadata attributes are written using HDF5 Lite helpers:

- CLASS = IMAGE
- IMAGE_VERSION = 1.2
- IMAGE_SUBCLASS = IMAGE_INDEXED

## 3.2 CATCHMENT_SPREADSHEETS

Purpose:

- magnified integer spreadsheet-like map products.

Created when:

- `number` is written.

Typical dataset name:

- `SV<ver>_numbering`

Dataset attributes:

- title
- magnification

## 4. Dataset attributes on CONSTANTS and VARIABLES/value

Each main variable dataset gets these attributes:

- title (string)
- units (string)
- basis (string)
- scope (string)
- names of dimensions (string array)
- database type (single character code)

Additional conditional attributes (only when dimension exists):

- time: string note `has its own dataset`
- column limits: integer pair [ilow, ihigh]
- row limits: integer pair [jlow, jhigh]
- element nos.: 2 x N integer table
  - row 1: local list index (1..N)
  - row 2: element number
- element types: string array of member labels
- extra: string array for extra dimension labels

## 5. Dimension model and shape semantics

Internal canonical dimension keys are:

1. time
2. extra
3. layer
4. el_typ
5. column or el-lst
6. row

Which of these appear depends on item configuration:

- time: present for dynamic variables only
- extra: present when extra_dimensions has more than one value
- layer: present when variable varies with elevation (layer range > 0)
- el_typ: present when member count > 1 (compound or edge-based outputs)
- column/row: for grid basis
- el-lst: for list basis

Important reduction rules used by the writer:

- `extra_dimensions = '-'` means one conceptual entry, but dimension is dropped from stored rank.
- single-member `el_typ` is dropped from stored rank.

### 5.1 Grid per timepoint versus point/list per timepoint

SHETran supports both output styles through item BASIS in visualisation_plan:

- grid_as_grid:
  - time series of 2D grid slices (plus optional layer/extra/member axes)
  - this is the "2d-array grid per time point" style.

- grid_as_list or list_as_list:
  - time series over element list index (`el-lst`) rather than full grid.
  - this is the "point/list based time series" style.

So shape is not only variable-dependent; it is also plan-dependent.

## 6. Type code system

Output type family (stored as `database type` first character) resolves to one of:

- B: real banks
- E: integer banks
- F: integer rivers
- G: real middle + edges (compound)
- I: integer middle
- L: real rivers
- M: real middle
- N: integer middle + edges (compound)

Member count by resolved type:

- B, E, F, L: 4 members (N/E/S/W)
- G, N: 9 members (square + 4 banks + 4 links)
- I, M: 1 member

Element type labels used in attributes:

- 1-member: square
- 4-member bank set: N-bank, E-bank, S-bank, W-bank
- 4-member river set: N-link, E-link, S-link, W-link
- 9-member compound: square + bank set + link set

## 7. Extra-dimension label vocabulary

Allowed extra_dimensions values and labels:

- `-` -> none
- `faces` -> North, East, South, West
- `left_right` -> left, right
- `X_Y` -> x, y

## 8. Complete possible variable catalog

This is the full registry from visualisation_interface_centre.f90 (outtype).
"Implemented" reflects the code flag used by dynamic registration checks.

## 8.1 Static variables (CONSTANTS)

| number | name     | units | extra_dimensions | varies_with_elevation | implemented |
| -----: | -------- | ----- | ---------------- | --------------------- | ----------- |
|     -7 | spatial1 | -     | -                | no                    | yes         |
|     -6 | soil_typ | -     | -                | yes                   | yes         |
|     -5 | surf_elv | m     | -                | no                    | yes         |
|     -4 | vert_thk | m     | -                | yes                   | yes         |
|     -3 | r_span   | m     | faces            | no                    | yes         |
|     -2 | number   | -     | -                | no                    | yes         |
|     -1 | centroid | m     | X_Y              | no                    | yes         |
|      0 | grid_dxy | m     | X_Y              | no                    | yes         |

## 8.2 Dynamic variables (VARIABLES)

| number | name     | units   | extra_dimensions | varies_with_elevation | varies_with_sediment_no | varies_with_contaminant_no | implemented |
| -----: | -------- | ------- | ---------------- | --------------------- | ----------------------- | -------------------------- | ----------- |
|      1 | net_rain | mm/hour | -                | no                    | no                      | no                         | yes         |
|      2 | pot_evap | mm/hour | -                | no                    | no                      | no                         | yes         |
|      3 | trnsp    | mm/hour | -                | no                    | no                      | no                         | yes         |
|      4 | srf_evap | mm/hour | -                | no                    | no                      | no                         | yes         |
|      5 | int_evap | mm/hour | -                | no                    | no                      | no                         | yes         |
|      6 | drainage | mm/hour | -                | no                    | no                      | no                         | yes         |
|      7 | can_stor | mm      | -                | no                    | no                      | no                         | yes         |
|      8 | infilt   | mm/hour | -                | no                    | no                      | no                         | no          |
|      9 | v_flow   | m/s     | -                | yes                   | no                      | no                         | yes         |
|     10 | snow_dep | m       | -                | no                    | no                      | no                         | yes         |
|     11 | snow_tmp | deg C   | -                | no                    | no                      | no                         | no          |
|     12 | ph_depth | m       | -                | no                    | no                      | no                         | yes         |
|     13 | lat_flow | m3/s    | faces            | yes                   | no                      | no                         | no          |
|     14 | ovr_flow | m3/s    | faces            | no                    | no                      | no                         | yes         |
|     15 | srf_dep  | m       | -                | no                    | no                      | no                         | yes         |
|     16 | recharge | m/s     | -                | yes                   | no                      | no                         | no          |
|     17 | st_aq_fl | m3/s    | -                | no                    | no                      | no                         | no          |
|     18 | sp_dis   | m3/s    | -                | no                    | no                      | no                         | no          |
|     19 | psi      | m       | -                | yes                   | no                      | no                         | yes         |
|     20 | theta    | m3/m3   | -                | yes                   | no                      | no                         | yes         |
|     21 | s_t_dp   | mm      | -                | no                    | no                      | no                         | yes         |
|     22 | s_p_dp   | mm      | -                | no                    | yes                     | no                         | no          |
|     23 | s_in_d   | kg/m2/s | -                | no                    | yes                     | no                         | no          |
|     24 | s_if_s   | kg/m2/s | -                | no                    | yes                     | no                         | no          |
|     25 | s_v_er   | mm/day  | -                | no                    | no                      | no                         | yes         |
|     26 | s_l_er   | m/s     | left_right       | no                    | no                      | no                         | no          |
|     27 | s_dis    | kg/s    | faces            | no                    | yes                     | no                         | yes         |
|     28 | s_n_di   | kg/s    | -                | no                    | yes                     | no                         | no          |
|     29 | s_dena   | -       | -                | no                    | yes                     | no                         | no          |
|     30 | s_conc   | mg/l    | faces            | no                    | yes                     | no                         | no          |
|     31 | s_x_dp   | m2      | -                | no                    | no                      | no                         | no          |
|     32 | c_c_dr   | -       | -                | yes                   | no                      | yes                        | yes         |
|     33 | c_c_ds   | -       | -                | yes                   | no                      | yes                        | yes         |
|     34 | c_c_sw   | -       | -                | no                    | no                      | yes                        | no          |
|     35 | c_c_sl   | -       | -                | no                    | no                      | yes                        | no          |
|     36 | c_c_dl   | -       | -                | no                    | no                      | yes                        | no          |
|     37 | c_c_bs   | -       | -                | no                    | no                      | yes                        | no          |
|     38 | c_c_we   | -       | -                | no                    | no                      | yes                        | no          |
|     39 | c_c_pp   | -       | -                | no                    | no                      | yes                        | no          |
|     40 | c_c_tp   | -       | -                | no                    | no                      | yes                        | no          |
|     41 | well_t_a | m3/s    | -                | no                    | no                      | no                         | no          |
|     42 | well_a_s | m3/s    | -                | yes                   | no                      | no                         | no          |
|     43 | bal_err  | m       | -                | no                    | no                      | no                         | yes         |
|     44 | sd_loss  | mm      | -                | no                    | no                      | no                         | no          |

## 9. Optionality summary

Required per file:

- group CONSTANTS
- group VARIABLES

Required per dynamic item subgroup:

- dataset value
- dataset time

Required per static item:

- one dataset in CONSTANTS

Optional by configuration:

- dynamic subgroup existence itself (depends on visualisation_plan items)
- extra, layer, el_typ dimensions (depends on item/type metadata)
- CATCHMENT_MAPS and CATCHMENT_SPREADSHEETS groups (created on demand)

## 10. Practical parsing guidance

For robust downstream readers:

- Do not hardcode rank; inspect `names of dimensions` and dataset shape.
- Treat dynamic values as time-major arrays where time is the first stored axis.
- Use attributes (`basis`, `scope`, limits, element nos., element types, extra) to reconstruct geometry.
- Handle both full-grid and list-based outputs.
- Expect variable-specific suffixing for sediment or contaminant selections in subgroup names.

## 11. Notes and caveats

- Static variables are registered from a fixed internal list, while dynamic variables are selected through visualisation_plan and validated against implementation flags.
- Some catalog entries are documented but marked not implemented; requesting these dynamic variables triggers validation errors.
- Name formatting for dynamic subgroup numeric prefixes is Fortran width-3 integer formatting, so readers should allow leading spaces.
