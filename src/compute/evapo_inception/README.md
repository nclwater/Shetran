# Evapotranspiration Module Refactoring

This directory contains the refactored evapotranspiration (ET) module components.

## Original Structure
The original `ETmod.f90` was a monolithic module containing all ET-related functionality.

## New Structure

### Main Interface
- `src/compute/ETmod.f90` - Main interface module that provides the same public interface as the original, ensuring no changes are needed in other files that use ETmod.

### Implementation Modules (in `src/compute/evapo_inception/`)

1. **`et_variables.f90`** - Module-wide variables and constants
   - Physical constants (LAMDA, GAMMA, RHO, CP)
   - Control variables (BAR, BMETP, BINETP, BMETAL)
   - Arrays for vegetation properties and modes
   - All module-level data storage

2. **`et_core.f90`** - Core evapotranspiration calculations
   - `ET` subroutine - main ET calculation routine
   - Implements 3 calculation modes (Penman-Monteith variants)
   - Handles interception and canopy water balance

3. **`et_integration.f90`** - Integration with the broader model
   - `ETIN` subroutine - integrates ET with other model components
   - Handles snowmelt interactions
   - Manages result storage and unit conversions
   - Handles surface water vs soil evaporation partitioning

4. **`et_validation.f90`** - Input data validation
   - `ETCHK2` subroutine - validates ET input parameters

5. **`et_main.f90`** - Main controller
   - `ETSIM` subroutine - main entry point for ET calculations
   - Loops over all elements and orchestrates calculations

## Design Principles
- **Minimal Interface Changes**: The public interface remains identical to ensure compatibility
- **Private by Default**: All implementation details are private unless specifically needed elsewhere
- **Logical Grouping**: Related functionality is grouped together in separate modules
- **Clear Dependencies**: Each module has well-defined dependencies

## Usage
Other modules should continue to `USE ETmod` exactly as before. The refactoring is transparent to the rest of the codebase.

## Files
- Original: `src/modules/ETmod.f90.sav` (backed up)
- New interface: `src/compute/ETmod.f90`
- Implementation: `src/compute/evapo_inception/*.f90`
