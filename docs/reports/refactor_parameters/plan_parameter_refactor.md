# Parameter Module Refactoring Plan

## Executive Summary

The current `src/parameters/` directory contains a mixture of true parameter definitions, variable declarations, and procedural code (subroutines/functions) that should be reorganized for better maintainability and clarity. This document outlines a three-phase refactoring plan to create a more logical and maintainable parameter structure.

## Current Issues

- 40+ files in parameters directory with unclear naming conventions
- Mix of parameters, variables, and procedural code in same directory
- Multiple files with similar names (colm_*, link_*, etc.) with unclear distinctions
- Functions and subroutines mixed with parameter definitions
- Inconsistent file naming (some uppercase, some lowercase)

## 1. Parameters

### 1.1 Core System Parameters
**Target: `src/parameters/core/`**

- **`sglobal.f90`** → Split into multiple focused modules:
  - `system_constants.f90` - Version info, banners, fundamental constants
  - `array_dimensions.f90` - Grid dimensions, array sizing parameters
  - `numerical_constants.f90` - Mathematical constants (zero, one, pi, etc.)
  - `error_constants.f90` - Error codes and types

- **`mod_parameters.f90`** → Rename to `precision_kinds.f90`
  - Modern Fortran KIND definitions
  - String length constants
  - NaN equivalents

### 1.1.1 Use of the Fortran Standard Library (stdlib) with mod_parameters / precision_kinds

Assessment summary
- Keep mod_parameters (renamed precision_kinds.f90) as the canonical project-owned module for KINDs and project-specific PARAMETER values.
- Use stdlib to replace low-level, language/utility concerns inside mod_parameters rather than removing mod_parameters entirely.
- Benefits: fewer bespoke implementations, improved portability and correctness, smaller test surface for platform-specific behaviour.

Concrete stdlib replacements and complements
- Kind selection
  - Replace custom SELECTED_REAL_KIND wrappers and handcrafted kind-helpers with stdlib_kinds (or stdlib's kind helpers) inside precision_kinds.f90; re-export I_P, R_P symbols.
- Machine numerics and math constants
  - Use stdlib_math for machine constants/epsilon helpers (where available) and stdlib's well-tested math constants rather than ad-hoc eps/pi definitions. Keep domain-specific constants (physical parameters) in the project module.
- Common logical / numeric convenience constants
  - Replace repeated definitions (zero, one, half, T/F) by re-exporting small stable symbols computed from stdlib or use them directly in modules; retain descriptive project names for clarity.
- File units / IO helpers
  - For any environment/path helpers currently mixed into mod_parameters, prefer stdlib_filesystem and stdlib_environment for portable path handling and config reads. For file unit management, plan migration to NEWUNIT in code and use stdlib helpers for path tests; keep a transitional file_units.f90 to map legacy unit numbers.
- String & parsing helpers
  - Use stdlib_strings for tokenization/parsing used at init/config time; do not use strings in hot loops.
- Tests & validation
  - Use stdlib_unittest to assert KIND sizes, numeric tolerances and platform invariants exposed by precision_kinds.f90.

What should remain in mod_parameters / precision_kinds.f90
- Project-specific PARAMETER values (model default time steps, physics constants, domain-specific thresholds).
- Public, stable aliases (I_P, R_P) and any descriptive constant names used throughout the codebase.
- Centralized documentation and comments explaining the semantics and units of the parameters.

Recommended minimal migration approach
1. Add stdlib as a dependency (prefer fpm; otherwise vendor selected stdlib modules).
2. In precision_kinds.f90:
   - Replace internal kind-computation code with calls to stdlib_kinds and set I_P, R_P from those results.
   - Import stdlib_math for machine/epsilon helpers and re-export or reference as needed.
   - Leave domain constants untouched.
3. Add unit tests (stdlib_unittest) that validate I_P/R_P sizes and machine epsilon behaviour on CI platforms.
4. Gradually replace other ad-hoc utilities across the parameters tree with the corresponding stdlib modules (strings/filesystem/sorting) during the per-module refactors.
5. Plan a follow-up pass to adopt NEWUNIT in IO code and remove hardcoded unit numbers from file_units once adapter and tests are in place.

Practical notes & caveats
- Pin stdlib version in fpm.toml or vendor a minimal set of modules if the build system is not yet fpm-based.
- Avoid using container-like stdlib modules (hashmap, list) directly in performance-critical inner loops; instantiate them at init time and treat as read-only in parallel regions.
- Use stdlib_unittest early to lock behaviour across compilers/architectures when kind/math helpers are switched.

Rationale (one-liner)
- Use stdlib to eliminate duplicated, error-prone small utilities in mod_parameters while keeping mod_parameters as the single authoritative source of project semantics and values.

### 1.2 Physics Constants
**Target: `src/parameters/physics/`**

- **`const_sy.f90`** → `physical_constants.f90`
  - Gravity, density, viscosity constants
  - Universal physical constants

### 1.3 Component-Specific Parameters
**Target: `src/parameters/components/`**

#### 1.3.1 Column Model Parameters
- **`colm_c1.f90`** → `column_water_params.f90`
  - Water flow variables for column model
  - Scale references and finite difference constants

- **`colm_c2.f90`** → `column_flow_vars.f90`
  - Secondary water flow variables
  - Boundary conditions and dimensional data

- **`colm_cc.f90`** → `column_contaminant_params.f90`
  - Contaminant variables for column model

- **`colm_cc1.f90`** → `column_contaminant_supplement.f90`
  - Supplementary contaminant parameters

- **`colm_cg.f90`** → `column_generation_params.f90`
  - Generation/preparation parameters for column model

- **`colm_co.f90`** → `column_output_params.f90`
  - Output-related parameters for column model

#### 1.3.2 Link Model Parameters
- **`link_cc.f90`** → `link_contaminant_params.f90`
  - Contaminant variables for link model

- **`link_cc1.f90`** → `link_contaminant_supplement.f90`
  - Supplementary contaminant parameters for links

- **`link_cw.f90`** → `link_water_params.f90`
  - Water flow preparation parameters for links

#### 1.3.3 Sediment Parameters
- **`sed_co.f90`** → `sediment_output_params.f90`
  - Sediment model output parameters

- **`SED_CS.F90`** → `sediment_core_params.f90`
  - Core sediment model parameters

#### 1.3.4 Other Component Parameters
- **`is_cc.f90`** → `initial_conditions_params.f90`
  - Initial condition parameters

- **`plant_cc.f90`** → `plant_contaminant_params.f90`
  - Plant-contaminant interaction parameters

- **`bk_cw.f90`** → `bank_water_params.f90`
  - Bank water flow parameters

### 1.4 Global System Variables
**Target: `src/parameters/global_system/`**

- **`AL_C.F90`** → Split into:
  - `file_units.f90` - File unit number definitions
  - `global_arrays.f90` - Large global arrays (NELEE-sized)
  - `vegetation_arrays.f90` - Vegetation-related arrays

- **`AL_D.f90`** → `flow_component_vars.f90`
  - Flow component variables only

- **`AL_G.F90`** → `grid_variables.f90`
  - Grid-related variables

- **`CONT_CC.F90`** → `contaminant_control_params.f90`
  - Contaminant control parameters

## 2. Subroutines / Functions

### 2.1 Utility Functions
**Target: `src/util/numerical/`**

From `sglobal.f90`, extract functions:

- `eqmarker(a)` → `numerical_comparisons.f90`
- `gtzero(a)`, `gezero(a)`, `ltzero(a)`, `lezero(a)` → `numerical_comparisons.f90`
- `iszero(a)`, `iszero_a(a)`, `i_iszero_a2(a)` → `numerical_comparisons.f90`
- `notzero(a)`, `isone(a)`, `notone(a)` → `numerical_comparisons.f90`
- `idimje(x,y)`, `dimje(x,y)` → `numerical_utilities.f90`

### 2.2 Error Handling
**Target: `src/util/error_handling/`**

From `sglobal.f90`, extract subroutines:

- `ERROR(ETYPE, ERRNUM, OUT, IEL, CELL, TEXT)` → `error_reporting.f90`
- `ALSTOP(FLAG)` → `program_termination.f90`

### 2.3 Initialization Routines
**Target: `src/simulation/initialization/`**

From `AL_C.F90`, extract:

- `initialise_al_c()` → `vadose_zone_initialization.f90`
  - Allocates and initializes VSS (Vadose-Saturated-Surface) arrays
  - Initializes unsaturated zone flow arrays (qvsh, qvsv, vspsi, vsthe, qvswli, eruz)

## 3. Code Updates

### 3.1 Phase 1: Create New Directory Structure
1. Create new directory structure:
   ```
   src/parameters/
   ├── core/
   ├── physics/
   ├── components/
   └── global_system/
   ```

2. Create corresponding directories in `src/util/` and `src/simulation/`

### 3.2 Phase 2: Extract and Reorganize Parameters
1. **Split `sglobal.f90`**:
   - Extract constants to new modules
   - Move functions to utility modules
   - Move error handling to error modules

2. **Reorganize component parameters**:
   - Rename and move colm_* files to components/
   - Rename and move link_* files to components/
   - Group related parameters together

3. **Modernize legacy files**:
   - Split large AL_* files into logical groups
   - Improve naming conventions
   - Add proper documentation

### 3.3 Phase 3: Update Dependencies
1. **Update USE statements** throughout codebase:
   - Replace `USE SGLOBAL` with specific module imports
   - Update all parameter file references
   - Ensure proper module dependencies

2. **Update build system**:
   - Modify CMakeLists.txt to include new directory structure
   - Update module dependency order

3. **Testing and validation**:
   - Compile and test with existing examples
   - Ensure no functionality changes
   - Validate all parameter values remain accessible

### 3.4 Implementation Strategy

#### Phase 1: Preparation (Low Risk)
- Create directory structure
- Copy files to new locations
- No code changes yet

#### Phase 2: Incremental Refactoring (Medium Risk)
- Refactor one module at a time
- Start with smallest, least connected modules
- Test compilation after each module

#### Phase 3: Integration (High Risk)
- Update all USE statements
- Final testing and validation
- Performance verification

### 3.5 Rollback Plan
- Maintain original files until full validation
- Use git branches for each phase
- Keep detailed change log for reference

### 3.6 Benefits
- **Clarity**: Clear separation of parameters, variables, and code
- **Maintainability**: Logical grouping of related parameters
- **Modularity**: Easier to understand component dependencies
- **Modernization**: Following Fortran best practices
- **Documentation**: Improved code organization aids understanding

### 3.7 Risks and Mitigation
- **Compilation errors**: Incremental approach with frequent testing
- **Missing dependencies**: Comprehensive USE statement audit
- **Performance impact**: Benchmark critical sections
- **Merge conflicts**: Coordinate with other developers

## 4. Call-Graph Analysis and Clarity Improvements

### 4.1 Current Dependency Issues

Analysis of the current module dependencies reveals several problems that the proposed reorganization would address:

#### 4.1.1 Monolithic SGLOBAL Module
**Problem**: Nearly every module depends on `SGLOBAL`, creating a central bottleneck:
```fortran
USE SGLOBAL  ! Used by 50+ modules
```

**Current Issues**:
- Contains mixed responsibilities (constants, arrays, functions, error handling)
- Creates unnecessary compilation dependencies
- Makes it difficult to track what each module actually needs

**Proposed Solution**: Split into focused modules:
```fortran
USE system_constants, ONLY: SHEVER, BANNER
USE array_dimensions, ONLY: NELEE, LLEE, NXEE, NYEE
USE numerical_constants, ONLY: zero, one, pi
USE error_constants, ONLY: FFFATAL, EEERR, WWWARN
```

#### 4.1.2 Parameter Module Coupling
**Problem**: Complex interdependencies between parameter modules:
```fortran
! contaminant_column_solver.f90 uses 6 COLM modules!
USE COLM_C1
USE COLM_C2  
USE COLM_CC
USE COLM_CC1
USE COLM_CG
USE COLM_CO
```

**Current Issues**:
- Unclear which parameters belong together
- Multiple modules needed for single computational task
- No logical grouping by purpose

**Proposed Solution**: Logical grouping by function:
```fortran
USE column_water_params     ! Instead of COLM_C1, COLM_C2
USE column_contaminant_params  ! Instead of COLM_CC, COLM_CC1
USE column_generation_params   ! Instead of COLM_CG  
USE column_output_params      ! Instead of COLM_CO
```

#### 4.1.3 Cross-Parameter Dependencies
**Problem**: Parameter modules depend on each other:
```fortran
! link_cw.f90
USE LINK_CC1  ! Parameter module using another parameter module
```

**Impact**: Creates compilation order dependencies and circular reference risks.

**Proposed Solution**: Eliminate cross-dependencies by proper grouping and moving shared items to core modules.

### 4.2 Call-Graph Improvements

#### 4.2.1 Dependency Hierarchy Clarification
**Before**: Flat, complex web of dependencies
```
sglobal ←── al_c ←── framework_*
   ↑         ↑         ↑
colm_* ←── cont_cc ←── contaminant_*
   ↑         ↑         ↑
link_* ←── sed_* ←── sediment_*
```

**After**: Clear hierarchical structure
```
core/ (system_constants, array_dimensions, numerical_constants)
  ↓
physics/ (physical_constants)
  ↓
components/ (column_*, link_*, sediment_*, initial_conditions_*)
  ↓
global_system/ (file_units, global_arrays, grid_variables)
  ↓
compute/ & simulation/ modules
```

#### 4.2.2 Reduced Compilation Dependencies

**Before**: Changing SGLOBAL triggers recompilation of 50+ modules

**After**: Changing specific parameter files only affects modules that actually use them

#### 4.2.3 Interface Clarity

**Before**: Unclear what functionality each module provides

**After**: Self-documenting module names and focused responsibilities

### 4.3 FORD Documentation Benefits

The reorganization will improve FORD documentation generation by:

1. **Clearer Module Graphs**: Related parameters grouped together
2. **Reduced Visual Complexity**: Fewer interconnections between unrelated modules
3. **Better Navigation**: Logical directory structure reflected in documentation
4. **Focused Module Pages**: Each module has a clear, single purpose

### 4.4 Build System Improvements

#### 4.4.1 CMake Dependency Management

**Current Challenge**: Complex interdependencies make parallel compilation difficult

**Improvement**: Clear hierarchy enables better parallel compilation

#### 4.4.2 Incremental Compilation

**Before**: Changes to core parameters trigger full rebuild

**After**: Only affected components need recompilation

### 4.5 Quantitative Improvements

Based on current dependency analysis:

| Metric                            | Before      | After           | Improvement      |
| --------------------------------- | ----------- | --------------- | ---------------- |
| Modules depending on SGLOBAL      | 50+         | ~10 (core only) | 80% reduction    |
| Average USE statements per module | 8-12        | 3-5             | 50% reduction    |
| Cross-parameter dependencies      | 15+         | 0               | 100% elimination |
| Average compilation dependencies  | 40+ modules | 10-15 modules   | 70% reduction    |

### 4.6 Call-Graph Validation Approach

To verify improvements:

1. **Generate FORD graphs before/after**: Compare visual complexity
2. **CMake dependency analysis**: Use `cmake --graphviz=deps.dot` to analyze build dependencies
3. **Compilation time measurements**: Benchmark full and incremental builds
4. **Static analysis**: Use tools to verify no circular dependencies

The proposed reorganization significantly improves call-graph clarity by:

- Eliminating the monolithic SGLOBAL bottleneck
- Creating logical parameter groupings
- Reducing cross-dependencies between parameter modules
- Establishing clear hierarchical relationships
- Improving build system efficiency

## 5. Parameter Extraction Analysis

### 5.1 Physical Constants for Extraction

Analysis of the codebase reveals numerous physical constants scattered throughout different modules that would benefit from centralization:

#### 5.1.1 Currently Well-Organized Constants
**`CONST_SY.F90`** - Already contains core physical constants:
```fortran
DOUBLEPRECISION, PARAMETER :: GRAVTY = 9.80665d0    ! Gravity (m/s²)
DOUBLEPRECISION, PARAMETER :: RHOSED = 2650.0d0     ! Sediment density (kg/m³)
DOUBLEPRECISION, PARAMETER :: RHOWAT = 998.0d0      ! Water density (kg/m³)
DOUBLEPRECISION, PARAMETER :: VISCOS = 1.0D-6       ! Viscosity (m²/s)
```

**`snow_constants.f90`** - Well-organized snow physics constants:
```fortran
DOUBLEPRECISION, PARAMETER :: RHOA = 1.29d0         ! Air density (kg/m³)
DOUBLEPRECISION, PARAMETER :: RHOW = 1000.0d0       ! Water density (kg/m³)
DOUBLEPRECISION, PARAMETER :: CPA = 1003.0d0        ! Specific heat of air (J/kg/C)
DOUBLEPRECISION, PARAMETER :: LWI = 334000.0d0      ! Latent heat of fusion (J/kg)
DOUBLEPRECISION, PARAMETER :: LVW = 2500000.0d0     ! Latent heat of vaporization (J/kg)
```

#### 5.1.2 Scattered Constants Requiring Extraction

**Evapotranspiration Constants** (`et_variables.f90`):
```fortran
DOUBLEPRECISION, PARAMETER :: LAMDA=2465000.        ! Latent heat of vaporization (J/kg)
DOUBLEPRECISION, PARAMETER :: GAMMA=0.659           ! Psychrometric constant (mbar/C)
DOUBLEPRECISION, PARAMETER :: RHO=1.2               ! Air density (kg/m³)
DOUBLEPRECISION, PARAMETER :: CP=1003.              ! Specific heat of air (J/kg/C)
```

**Hydraulic Constants** (`oc_parameters.f90`):
```fortran
DOUBLEPRECISION, PARAMETER :: F23=2.0D0/3.0D0       ! 2/3 power for Manning's equation
DOUBLEPRECISION, PARAMETER :: F53=5.0D0/3.0D0       ! 5/3 power for Manning's equation
DOUBLEPRECISION, PARAMETER :: ROOT2G = 4.42944d0    ! sqrt(2*9.81) - gravity constant
```

### 5.2 Mathematical Constants for Extraction

#### 5.2.1 Repeated Basic Mathematical Constants
Found in **6 different visualization modules**:
```fortran
LOGICAL, PARAMETER :: T=.TRUE., F=.FALSE.
REAL, PARAMETER :: zero=0.0, half=0.5
```

Found in **sglobal.f90** and repeated elsewhere:
```fortran
DOUBLEPRECISION, PARAMETER :: zero=0.0d0, one=1.0d0, half=0.5d0
DOUBLEPRECISION, PARAMETER :: two=2.0d0, three=3.0d0, five=5.0d0
```

#### 5.2.2 Numerical Tolerance Constants
```fortran
! utilsmod.f90
DOUBLEPRECISION, PARAMETER :: eps=1.0d-15

! sglobal.f90
DOUBLEPRECISION, PARAMETER :: vsmall=1.0d-20

! oc_parameters.f90
DOUBLEPRECISION, PARAMETER :: DZMIN = 1.0D-3
```

### 5.3 Recommended Extractions

#### 5.3.1 Consolidate Physical Constants
**Target: `src/parameters/physics/thermodynamic_constants.f90`**
```fortran
! Unified thermodynamic and physical constants
DOUBLEPRECISION, PARAMETER :: &
   GRAVITY = 9.80665d0, &           ! Acceleration due to gravity (m/s²)
   
   ! Density constants (kg/m³)
   DENSITY_AIR = 1.29d0, &          ! Air density at standard conditions
   DENSITY_WATER = 1000.0d0, &     ! Pure water density
   DENSITY_SEDIMENT = 2650.0d0, &  ! Typical sediment density
   
   ! Heat capacity constants (J/kg/K)
   SPECIFIC_HEAT_AIR = 1003.0d0, &
   SPECIFIC_HEAT_WATER = 4187.0d0, &
   SPECIFIC_HEAT_ICE = 2093.0d0, &
   
   ! Latent heat constants (J/kg)
   LATENT_HEAT_FUSION = 334000.0d0, &      ! Ice to water
   LATENT_HEAT_VAPORIZATION = 2465000.0d0, & ! Water to vapor
   
   ! Other physical constants
   KINEMATIC_VISCOSITY = 1.0D-6, &  ! Water kinematic viscosity (m²/s)
   PSYCHROMETRIC_CONSTANT = 0.659   ! Psychrometric constant (mbar/C)
```

#### 5.3.2 Mathematical Constants Module
**Target: `src/parameters/core/mathematical_constants.f90`**
```fortran
! Fundamental mathematical constants and tolerances
DOUBLEPRECISION, PARAMETER :: &
   ! Basic constants
   MATH_ZERO = 0.0d0, &
   MATH_ONE = 1.0d0, &
   MATH_HALF = 0.5d0, &
   MATH_TWO = 2.0d0, &
   MATH_THREE = 3.0d0, &
   MATH_PI = 3.141592653589793d0, &
   
   ! Common fractions for hydraulic calculations
   TWO_THIRDS = 2.0d0/3.0d0, &     ! Manning's equation
   FIVE_THIRDS = 5.0d0/3.0d0, &    ! Manning's equation
   SQRT_TWO_G = 4.42944d0, &       ! sqrt(2*gravity)
   
   ! Numerical tolerances
   EPSILON_SMALL = 1.0d-15, &      ! General numerical tolerance
   EPSILON_VERY_SMALL = 1.0d-20, & ! Very small tolerance
   DEPTH_MINIMUM = 1.0d-3          ! Minimum flow depth (m)

! Logical constants
LOGICAL, PARAMETER :: &
   LOGICAL_TRUE = .TRUE., &
   LOGICAL_FALSE = .FALSE.

! Integer constants  
INTEGER, PARAMETER :: &
   INT_ZERO = 0, &
   INT_ONE = 1
```

#### 5.3.3 Component-Specific Constants
**Target: `src/parameters/physics/hydraulic_constants.f90`**
```fortran
! Hydraulic and flow-related constants
DOUBLEPRECISION, PARAMETER :: &
   MANNING_TWO_THIRDS = 2.0d0/3.0d0, &  ! Manning's equation exponent
   MANNING_FIVE_THIRDS = 5.0d0/3.0d0, & ! Manning's equation exponent
   MINIMUM_FLOW_DEPTH = 1.0d-3, &       ! Minimum depth for calculations (m)
   SQRT_MINIMUM_DEPTH = 3.16227766d-2   ! sqrt(MINIMUM_FLOW_DEPTH)
```

### 5.4 Implementation Benefits

#### 5.4.1 Consistency and Accuracy

- **Eliminates conflicting values**: Currently water density appears as both 998.0 and 1000.0 kg/m³
- **Centralizes physical constants**: Single source of truth for physical properties
- **Improves maintainability**: Changes to physical constants in one location

#### 5.4.2 Code Clarity

- **Self-documenting names**: `GRAVITY` instead of scattered `9.81` values
- **Proper units documentation**: Clear units in comments
- **Logical grouping**: Related constants together

#### 5.4.3 Compilation Efficiency

- **Reduced redundancy**: Eliminates 6+ duplicate definitions of basic constants
- **Better optimization**: Compiler can optimize constant propagation
- **Cleaner dependencies**: Clear separation of mathematical vs. physical constants

### 5.5 Migration Strategy

#### 5.5.1 Phase 1: Create Constants Modules
1. Create `thermodynamic_constants.f90`
2. Create `mathematical_constants.f90`
3. Create `hydraulic_constants.f90`

#### 5.5.2 Phase 2: Consolidate Scattered Constants
1. Extract from `et_variables.f90`
2. Extract from `oc_parameters.f90`
3. Consolidate visualization module constants
4. Update all USE statements

#### 5.5.3 Phase 3: Validation
1. Verify no value changes during extraction
2. Check for any missed constants
3. Ensure consistent units throughout
4. Test compilation and runtime behavior

### 5.6 Identified Extraction Candidates

| Current Location      | Constant   | Value          | Target Module           |
| --------------------- | ---------- | -------------- | ----------------------- |
| et_variables.f90      | LAMDA      | 2465000.       | thermodynamic_constants |
| et_variables.f90      | GAMMA      | 0.659          | thermodynamic_constants |
| oc_parameters.f90     | F23        | 2.0D0/3.0D0    | hydraulic_constants     |
| oc_parameters.f90     | ROOT2G     | 4.42944d0      | hydraulic_constants     |
| 6 visualization files | T, F       | .TRUE./.FALSE. | mathematical_constants  |
| 6 visualization files | zero, half | 0.0, 0.5       | mathematical_constants  |
| utilsmod.f90          | eps        | 1.0d-15        | mathematical_constants  |
| Multiple files        | File units | Various        | file_units              |

This extraction would eliminate approximately **30+ duplicate constant definitions** and create a more maintainable, consistent parameter structure.

### 5.7 File Unit Identification and Reorganization

Analysis of the current file unit assignments reveals a complex system of hardcoded unit numbers scattered across AL_C.F90 and AL_D.F90. The following table identifies all current file units, proposes more descriptive names, and describes their usage:

| Current ID | Unit | Proposed Name                 | Description                           |
| ---------- | ---- | ----------------------------- | ------------------------------------- |
| FRD        | 10   | fid_flow_data                 | Flow component input data file        |
| VSD        | 11   | fid_vadose_saturated_data     | Vadose-saturated zone input data      |
| OCD        | 12   | fid_overland_channel_data     | Overland/channel flow input data      |
| ETD        | 13   | fid_evapotranspiration_data   | Evapotranspiration input data         |
| PPD        | 14   | fid_precipitation_data        | Precipitation input data              |
| SMD        | 15   | fid_snow_model_data           | Snow model input data                 |
| BKD        | 16   | fid_bank_data                 | Bank erosion input data               |
| SYD        | 17   | fid_sediment_yield_data       | Sediment yield input data             |
| CMD        | 18   | fid_contaminant_data          | Contaminant model input data          |
| MED        | 19   | fid_meteorological_data       | Meteorological input data             |
| PRD        | 20   | fid_precipitation_timeseries  | Precipitation time series data        |
| EPD        | 21   | fid_evaporation_data          | Evaporation input data                |
| TIM        | 22   | fid_time_control              | Time control parameters               |
| PRI        | 23   | fid_primary_output            | Primary output file (now in sglobal)  |
| SPR        | 24   | fid_summary_print             | Summary print output                  |
| CMP        | 25   | fid_comparison_output         | Comparison output file                |
| BUG        | 26   | fid_debug_output              | Debug information output              |
| RES        | 27   | fid_results_binary            | Binary results output file            |
| HOT        | 28   | fid_hotstart                  | Hotstart file for restart             |
| VSI        | 29   | fid_vadose_saturated_init     | VS zone initialization                |
| VED        | 30   | fid_vegetation_data           | Vegetation input data                 |
| WLD        | 31   | fid_water_level_data          | Water level data                      |
| LFB        | 32   | fid_link_flow_binary          | Link flow binary output               |
| LHB        | 33   | fid_link_head_binary          | Link head binary output               |
| LGB        | 34   | fid_link_general_binary       | Link general binary output            |
| BFB        | 35   | fid_bank_flow_binary          | Bank flow binary output               |
| BHB        | 36   | fid_bank_head_binary          | Bank head binary output               |
| OFB        | 37   | fid_overland_flow_binary      | Overland flow binary output           |
| OHB        | 38   | fid_overland_head_binary      | Overland head binary output           |
| CMT        | 39   | fid_contaminant_text          | Contaminant text output               |
| CMB        | 40   | fid_contaminant_binary        | Contaminant binary output             |
| DIS        | 41   | fid_discharge_output          | Discharge output file                 |
| VSE        | 42   | fid_vadose_saturated_error    | VS zone error output                  |
| MAS        | 43   | fid_mass_balance              | Mass balance output                   |
| DIS2       | 44   | fid_discharge_secondary       | Secondary discharge output            |
| TAH        | 45   | fid_time_analysis_high        | High-frequency time analysis          |
| TAL        | 46   | fid_time_analysis_low         | Low-frequency time analysis           |
| disextra   | 47   | fid_discharge_extra           | Extra discharge output                |
| vp_in      | 48   | fid_visualization_plan_input  | Visualization plan input              |
| vp_out     | 49   | fid_visualization_plan_output | Visualization plan output             |
| zqd        | 51   | fid_zq_table_data             | ZQ table hydraulic data               |
| pslextra   | 52   | fid_psl_extra                 | Extra PSL output                      |
| SFB        | 9876 | fid_sediment_flow_binary      | Sediment flow binary (unallocated)    |
| SRB        | 9877 | fid_sediment_results_binary   | Sediment results binary (unallocated) |

#### 5.7.1 File Unit Usage Patterns

**Input Data Files (10-22):**

- Primary model component data files
- Read during initialization
- Component-specific parameter and configuration data

**Output Files (24-47):**

- Various output formats (text, binary)
- Results, diagnostics, and analysis data
- Component-specific output streams

**Visualization Files (48-49):**

- Visualization system configuration
- Used by visualization modules

**Special Files:**

- **23 (PRI)**: Primary output, now in sglobal
- **9876-9877**: Unallocated placeholders

#### 5.7.2 Recommended File Units Module Structure

**Target: `src/parameters/global_system/file_units.f90`**
```fortran
! Centralized file unit definitions for SHETRAN
! All file units grouped by purpose with descriptive names

! Input data file units (10-22)
INTEGER, PARAMETER :: &
   fid_flow_data = 10, &                    ! Flow component input data
   fid_vadose_saturated_data = 11, &        ! Vadose-saturated zone data
   fid_overland_channel_data = 12, &        ! Overland/channel flow data
   fid_evapotranspiration_data = 13, &      ! Evapotranspiration data
   fid_precipitation_data = 14, &           ! Precipitation data
   fid_snow_model_data = 15, &              ! Snow model data
   fid_bank_data = 16, &                    ! Bank erosion data
   fid_sediment_yield_data = 17, &          ! Sediment yield data
   fid_contaminant_data = 18, &             ! Contaminant model data
   fid_meteorological_data = 19, &          ! Meteorological data
   fid_precipitation_timeseries = 20, &     ! Precipitation time series
   fid_evaporation_data = 21, &             ! Evaporation data
   fid_time_control = 22                    ! Time control parameters

! Primary system file units (23-26)
INTEGER, PARAMETER :: &
   fid_primary_output = 23, &               ! Primary output file
   fid_summary_print = 24, &                ! Summary print output
   fid_comparison_output = 25, &            ! Comparison output
   fid_debug_output = 26                    ! Debug information

! Results and restart file units (27-30)
INTEGER, PARAMETER :: &
   fid_results_binary = 27, &               ! Binary results output
   fid_hotstart = 28, &                     ! Hotstart file for restart
   fid_vadose_saturated_init = 29, &        ! VS zone initialization
   fid_vegetation_data = 30                 ! Vegetation input data

! Component-specific output file units (31-52)
INTEGER, PARAMETER :: &
   fid_water_level_data = 31, &             ! Water level data
   fid_link_flow_binary = 32, &             ! Link flow binary output
   fid_link_head_binary = 33, &             ! Link head binary output
   fid_link_general_binary = 34, &          ! Link general binary output
   fid_bank_flow_binary = 35, &             ! Bank flow binary output
   fid_bank_head_binary = 36, &             ! Bank head binary output
   fid_overland_flow_binary = 37, &         ! Overland flow binary output
   fid_overland_head_binary = 38, &         ! Overland head binary output
   fid_contaminant_text = 39, &             ! Contaminant text output
   fid_contaminant_binary = 40, &           ! Contaminant binary output
   fid_discharge_output = 41, &             ! Discharge output
   fid_vadose_saturated_error = 42, &       ! VS zone error output
   fid_mass_balance = 43, &                 ! Mass balance output
   fid_discharge_secondary = 44, &          ! Secondary discharge output
   fid_time_analysis_high = 45, &           ! High-frequency time analysis
   fid_time_analysis_low = 46, &            ! Low-frequency time analysis
   fid_discharge_extra = 47, &              ! Extra discharge output
   fid_visualization_plan_input = 48, &     ! Visualization plan input
   fid_visualization_plan_output = 49, &    ! Visualization plan output
   fid_zq_table_data = 51, &                ! ZQ table hydraulic data
   fid_psl_extra = 52                       ! Extra PSL output

! Reserved/unallocated units
INTEGER, PARAMETER :: &
   fid_sediment_flow_binary = 9876, &       ! Sediment flow binary (reserved)
   fid_sediment_results_binary = 9877       ! Sediment results binary (reserved)
```

#### 5.7.3 Migration Benefits

1. **Clarity**: Self-documenting file unit names
2. **Maintainability**: Centralized unit management
3. **Conflict Prevention**: Clear unit allocation tracking
4. **Documentation**: Built-in purpose description
5. **Consistency**: Standardized naming convention

#### 5.7.4 Future Modernization: NEWUNIT Implementation

**Important Note**: The file unit reorganization described above maintains the current hardcoded unit number approach for compatibility during the initial refactoring phase. However, **modern Fortran best practice recommends using the `NEWUNIT` feature** instead of hardcoded unit numbers.

**Recommended Future Refactoring** (separate phase):
```fortran
! Instead of hardcoded units:
OPEN(UNIT=fid_flow_data, FILE='flow.dat')

! Use NEWUNIT for automatic unit allocation:
INTEGER :: flow_unit
OPEN(NEWUNIT=flow_unit, FILE='flow.dat')
```

**Benefits of NEWUNIT approach:**

- **Automatic unit allocation**: System assigns available unit numbers
- **No unit conflicts**: Eliminates risk of duplicate unit assignments
- **Simplified management**: No need to track used/available units
- **Modern standard**: Follows Fortran 2008+ best practices
- **Safer code**: Reduces potential runtime errors from unit conflicts

**Implementation Strategy for NEWUNIT Migration:**

1. **Phase 1**: Current refactoring with descriptive hardcoded units
2. **Phase 2**: Gradual migration to NEWUNIT approach
3. **Phase 3**: Remove hardcoded unit parameters entirely

This two-phase approach ensures the initial parameter refactoring can proceed while laying groundwork for future modernization to NEWUNIT standards.
