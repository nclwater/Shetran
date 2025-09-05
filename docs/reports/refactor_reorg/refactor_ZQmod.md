# ZQmod Refactoring Documentation

## Overview

This document describes the refactoring of the ZQmod.f90 module that was performed on September 5, 2025. The refactoring extracts functionality from a monolithic module into logical, focused modules following separation of concerns principles.

## Original Structure

The original `ZQmod.f90.sav` file contained:

- **Data structures**: Module-level variables for storing ZQ table data, dimensions, and metadata
- **File I/O functionality**: `ReadZQTable` subroutine for reading and parsing ZQ table files
- **Interpolation functionality**: `get_ZQTable_value` function for discharge value lookup

## Refactored Structure

The functionality has been separated into the following modules:

### 1. Data Types Module: `zq_data_types.f90`

**Location**: `src/compute/hydraulic_structures/zq_data_types.f90`

**Purpose**: Contains all shared data structures and variables used by the ZQ table system.

**Key Variables**:
```fortran
! Dimension arrays
INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC :: nZQcols, nZQrows, zcol
! ZQ table data
REAL(kind=R8P), DIMENSION(:,:,:), ALLOCATABLE, PUBLIC :: ZQ
REAL(kind=R8P), DIMENSION(:,:), ALLOCATABLE, PUBLIC :: headerRealArray
! Operation parameters  
INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC :: ZQTableOpHour
INTEGER(kind=I_P), PUBLIC :: ZQTableRef
```

**Code Citation**: Lines 35-44 from original `ZQmod.f90.sav`

### 2. Table Reader Module: `zq_table_reader.f90`

**Location**: `src/compute/hydraulic_structures/zq_table_reader.f90`

**Purpose**: Handles reading ZQ table files and parsing metadata.

**Key Functionality**:
- Reads ZQ table file format with headers and data
- Parses stage threshold headers (e.g., 'ZQ>96.8' → 96.80)
- Allocates and populates data arrays
- Writes diagnostic output to log files

**Code Citation**: Lines 69-200 from original `ZQmod.f90.sav` (`ReadZQTable` subroutine)

### 3. Interpolator Module: `zq_interpolator.f90`

**Location**: `src/compute/hydraulic_structures/zq_interpolator.f90`

**Purpose**: Performs discharge value interpolation from ZQ lookup tables.

**Key Functionality**:
- Handles sluice operation timing based on `ZQTableOpHour`
- Selects appropriate weir equation based on stage thresholds
- Interpolates discharge values from ZQ arrays
- Returns downstream discharge for given upstream stage

**Code Citation**: Lines 218-273 from original `ZQmod.f90.sav` (`get_ZQTable_value` function)

### 4. Interface Module: `ZQmod.f90`

**Location**: `src/compute/ZQmod.f90`

**Purpose**: Provides clean public interface without implementation details.

**Key Features**:
- Contains no variable declarations
- Contains no implementation code
- Re-exports public interfaces from implementation modules
- Maintains backward compatibility for existing code

**Code Citation**: Public interface declarations from lines 42-43 of original `ZQmod.f90.sav`

## Benefits of Refactoring

### 1. Separation of Concerns
- **Data management** separated from **business logic**
- **File I/O operations** isolated from **computational algorithms**
- **Interface definition** separated from **implementation details**

### 2. Improved Maintainability
- Smaller, focused modules are easier to understand and modify
- Changes to file format handling don't affect interpolation logic
- Data structure changes are localized to one module

### 3. Better Testing
- Individual components can be unit tested in isolation
- Mock data can be easily injected for testing interpolation algorithms
- File I/O can be tested separately from computational logic

### 4. Enhanced Reusability
- Interpolation algorithms could be reused for other lookup table scenarios
- File reading patterns could be adapted for other table-based inputs
- Data structures provide clear interface for future extensions

## Migration Notes

### For Existing Code
The refactoring maintains full backward compatibility. Existing code that uses:
```fortran
USE ZQmod, ONLY: ReadZQTable, get_ZQTable_value
```

Will continue to work without modification.

### For New Development
New code can optionally use the more specific modules directly:
```fortran
USE zq_table_reader, ONLY: ReadZQTable
USE zq_interpolator, ONLY: get_ZQTable_value
```

### Build System
The build system will need to compile the new modules in dependency order:
1. `zq_data_types.f90`
2. `zq_table_reader.f90` 
3. `zq_interpolator.f90`
4. `ZQmod.f90`

## Future Enhancements

The refactored structure enables several potential improvements:

1. **Error Handling**: Better error management could be added to the file reader
2. **Validation**: Input validation could be enhanced in the data types module
3. **Performance**: Interpolation algorithms could be optimized without affecting other components
4. **Flexibility**: Alternative file formats could be supported by adding new reader modules

## Related Files

- **Original**: `/local/sonst/work/other/Shetran/src/modules/ZQmod.f90.sav`
- **Documentation**: Referenced in `docs/ZQ module.md`
- **Dependencies**: Uses data from `AL_D`, `AL_C`, `sglobal`, and `mod_parameters` modules

## Author Information

- **Refactoring**: GitHub Copilot (September 2025)
- **Original Authors**: 
  - Daryl Hughes, Newcastle University
  - Stephen Birkinshaw, Newcastle University  
  - Sven Berendsen, Newcastle University
