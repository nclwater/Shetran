# SHETRAN I/O Consolidation Plan

**Project:** SHETRAN Hydrological Model  
**Document Version:** 1.0  
**Date:** 5 September 2025  
**Status:** Rough Proposal - Phase 8 Planning Document  

---

## Notice

**This is a rough proposal** for Phase 8: I/O Consolidation of the SHETRAN refactoring project. The detailed implementation strategy, file movements, and organizational structure outlined in this document are preliminary and subject to revision during actual implementation.

This document serves as a comprehensive planning reference extracted from the main refactoring plan to provide detailed guidance for the I/O consolidation phase while keeping the main plan document concise and consistent with other phase descriptions.

---

## Overview

Consolidation of all file handling operations from across the codebase into the `src/io/` directory, excluding visualization-specific I/O which remains in `src/visualisation/`. This phase creates a centralized, well-organized file I/O system with clear responsibilities and consistent error handling.

---

## Current I/O Distribution Analysis

### Existing I/O Structure
```
src/io/                          # Current limited I/O directory
├── meteorological_input.f90     # Meteorological data reading
└── simulation_output.f90        # Basic simulation output

src/util/                        # General file operations
├── mod_load_filedata.f90        # Major file data loading utilities (ALREAD, etc.)
└── getdirqq.f90                 # Command-line and file argument processing

src/compute/                     # Scattered I/O operations
├── subsurface_flow/
│   └── subsurface_io.f90        # Subsurface data reading (VSREAD)
├── hydraulic_structures/
│   └── zq_table_reader.f90      # ZQ table file reading
└── execution_control/
    └── framework_output_manager.f90  # File opening and management (FROPEN, FRRESP)

src/visualisation/               # Visualization I/O (EXCLUDED from consolidation)
├── visualisation_read.f90       # Visualization file parsing
├── visualisation_hdf5.f90       # HDF5 output operations
└── [other visualization I/O]    # Remains in place
```

---

## Proposed Consolidated Structure

### Target I/O Organization
```
src/io/
├── input/                       # Input file handling
│   ├── meteorological_input.f90      # ← Already in place
│   ├── filedata_loader.f90           # ← From util/mod_load_filedata.f90
│   ├── subsurface_data_reader.f90    # ← From compute/subsurface_flow/subsurface_io.f90
│   ├── hydraulic_data_reader.f90     # ← From compute/hydraulic_structures/zq_table_reader.f90
│   └── command_line_interface.f90    # ← From util/getdirqq.f90
├── output/                      # Output file handling
│   ├── simulation_output.f90         # ← Already in place
│   ├── framework_output.f90          # ← From compute/execution_control/framework_output_manager.f90
│   └── result_writers.f90            # ← New module for consolidated result writing
├── common/                      # Shared I/O functionality
│   ├── file_utilities.f90            # Common file operations, error handling
│   ├── data_validation.f90           # Input data validation routines
│   └── io_constants.f90              # File units, format specifications
└── interfaces/                  # I/O interface definitions
    ├── data_reader_interface.f90     # Abstract interfaces for data readers
    └── output_writer_interface.f90   # Abstract interfaces for output writers
```

---

## Refactoring Strategy

### Phase 8.1: File Data Loading Consolidation
**Scope**: Move and refactor `util/mod_load_filedata.f90`

- **Target**: `src/io/input/filedata_loader.f90`
- **Key Functions**: ALREAD, ALINIT, ALCHK, ALCHKI, ALINTP, ALREDC, ALREDF, ALREDI, etc.
- **Dependencies**: 20+ modules currently use mod_load_filedata
- **Approach**: Maintain public interface, improve internal organization

### Phase 8.2: Specialized Data Readers
**Scope**: Consolidate domain-specific file readers

- **Subsurface I/O**: Move `compute/subsurface_flow/subsurface_io.f90` → `io/input/subsurface_data_reader.f90`
- **Hydraulic Structures**: Move `compute/hydraulic_structures/zq_table_reader.f90` → `io/input/hydraulic_data_reader.f90`
- **Command Line**: Move `util/getdirqq.f90` → `io/input/command_line_interface.f90`

### Phase 8.3: Output Consolidation
**Scope**: Centralize output file management

- **Framework Output**: Extract I/O portions of `framework_output_manager.f90`
- **Result Writers**: Create unified result writing interfaces
- **File Management**: Centralize file unit management and opening/closing operations

### Phase 8.4: I/O Infrastructure
**Scope**: Create shared I/O infrastructure

- **Error Handling**: Standardized I/O error handling and reporting
- **File Utilities**: Common file operations (existence checking, path handling)
- **Data Validation**: Input data validation and consistency checking
- **Interface Standards**: Abstract interfaces for extensible I/O operations

---

## Implementation Details

### Module Migration Strategy
```fortran
! Example: filedata_loader.f90 (migrated from mod_load_filedata.f90)
MODULE filedata_loader
   ! Maintains all existing public interfaces for backward compatibility
   
   USE io_constants, ONLY : DEFAULT_UNITS, ERROR_CODES
   USE file_utilities, ONLY : check_file_exists, safe_file_open
   USE data_validation, ONLY : validate_grid_data, validate_numeric_ranges
   
   IMPLICIT NONE
   
   ! Re-export all existing public interfaces
   PUBLIC :: ALREAD, ALINIT, ALCHK, ALCHKI, ALINTP
   PUBLIC :: ALREDC, ALREDF, ALREDI, ALREDL, ALRED2
   PUBLIC :: ALALLI, ALALLF
   
   ! New enhanced interfaces (optional)
   PUBLIC :: enhanced_grid_reader, validated_data_loader
   
CONTAINS
   ! All existing subroutines maintained with improved error handling
   ! Internal reorganization for better maintainability
END MODULE filedata_loader
```

### Backward Compatibility Approach

- **Interface Preservation**: All existing public interfaces remain unchanged
- **USE Statement Updates**: Systematic update of USE statements across codebase
- **Gradual Migration**: Phase implementation allows for incremental testing
- **Deprecation Warnings**: Optional warnings for deprecated patterns

---

## File Movement Plan

### High Priority Migrations
1. **mod_load_filedata.f90** → **filedata_loader.f90**

   - Most critical: Used by 20+ modules
   - Impact: Core data loading functionality
   - Dependencies: Requires careful USE statement updates

2. **framework_output_manager.f90** (I/O portions) → **framework_output.f90**

   - Critical: File opening and management (FROPEN, FRRESP)
   - Impact: Core simulation file management
   - Note: Computational portions remain in execution_control

### Medium Priority Migrations
3. **subsurface_io.f90** → **subsurface_data_reader.f90**

   - Scope: VSREAD and related subsurface file reading
   - Impact: Subsurface flow component
   - Dependencies: Limited to subsurface modules

4. **zq_table_reader.f90** → **hydraulic_data_reader.f90**

   - Scope: ZQ table file parsing
   - Impact: Hydraulic structures component
   - Dependencies: Limited scope

### Lower Priority Migrations
5. **getdirqq.f90** → **command_line_interface.f90**

   - Scope: Command-line argument processing
   - Impact: Program startup
   - Dependencies: Minimal, self-contained

---

## Testing and Validation Strategy

### Regression Testing Requirements

- **File Reading Validation**: All existing input files must parse identically
- **Output Comparison**: Generated outputs must match exactly
- **Error Handling**: Improved error messages without functional changes
- **Performance Testing**: No performance degradation from reorganization

### Integration Testing

- **Build System**: CMake integration with new directory structure
- **Module Dependencies**: Validation of all USE statement updates
- **Cross-platform**: Consistent behavior across supported platforms
- **Example Cases**: All existing examples must run without modification

---

## Benefits and Outcomes

### Immediate Benefits

- **Centralized I/O**: All file operations in logical, discoverable locations
- **Reduced Coupling**: Clear separation between I/O and computational logic
- **Improved Maintainability**: Easier to modify, debug, and extend I/O operations
- **Error Handling**: Consistent, comprehensive error handling across all I/O

### Long-term Benefits

- **Extensibility**: Clear patterns for adding new input/output formats
- **Testing**: Isolated I/O operations easier to unit test
- **Documentation**: Centralized I/O documentation and examples
- **Performance**: Opportunities for I/O optimization and caching

## Dependencies

- Completion of Phase 7 (Utilities Refactoring)
- Stable parameter interfaces from Phase 6
- Coordination with visualization team (exclusion of visualization I/O)

## Success Metrics

- **Functional Correctness**: Zero regression in file reading/writing behavior
- **Code Organization**: All non-visualization I/O consolidated in `src/io/`
- **Maintainability**: Reduced coupling between I/O and computational modules
- **Documentation**: Complete FORD documentation for all I/O modules
- **Error Handling**: Comprehensive, consistent error reporting

## Implementation Notes

### Coordination Points

- **Visualization Team**: Ensure clear boundaries between general I/O and visualization-specific file operations
- **Build System**: CMake updates required for new directory structure
- **Testing Team**: Integration with testing framework development

### Risk Mitigation

- **Interface Stability**: Maintain all existing public interfaces during migration
- **Incremental Implementation**: Sub-phases allow for gradual migration and testing
- **Rollback Strategy**: Ability to revert individual migrations if issues arise

### Future Considerations

- **HDF5 Integration**: Potential future integration of non-visualization HDF5 operations
- **Performance Optimization**: Opportunities for I/O caching and buffering
- **Format Extensions**: Framework for supporting additional input/output formats

---

*This document represents preliminary planning for Phase 8 of the SHETRAN refactoring project. Implementation details may be refined during execution based on practical considerations and stakeholder feedback.*
