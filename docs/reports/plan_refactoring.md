# SHETRAN Codebase Refactoring Plan

**Project:** SHETRAN Hydrological Model  
**Document Version:** 1.0  
**Date:** 5 September 2025  
**Status:** Phases 1-4 Complete, Phases 5 & 6 In Progress  

---

## Executive Summary

This document outlines the comprehensive refactoring plan for the SHETRAN hydrological model codebase, aimed at modernizing the software architecture, improving maintainability, and establishing sustainable development practices. The refactoring effort encompasses build system modernization, code structure reorganization, documentation standardization, and quality assurance improvements.

### Project Goals
- **Modernization**: Replace legacy build systems and deprecated language constructs
- **Maintainability**: Improve code organization and documentation
- **Cross-platform Support**: Ensure consistent builds across Linux/Windows platforms
- **Code Quality**: Eliminate problematic constructs (GOTO statements) and establish testing frameworks
- **Documentation**: Implement comprehensive, automatically generated documentation

---

## Refactoring Phases Overview

| Phase | Component                                               | Status                    | Key Outcomes                                      |
| ----- | ------------------------------------------------------- | ------------------------- | ------------------------------------------------- |
| 1     | [CMake Build System](#phase-1-cmake-build-system)       | ✅ Complete                | Cross-platform builds, dependency management      |
| 2     | [HDF5 Integration](#phase-2-hdf5-modernization)         | ✅ Complete                | System library support, flexible compilation      |
| 3     | [Module Disaggregation](#phase-3-module-disaggregation) | ✅ Complete                | Clearer file organization, removed `src/modules/` |
| 4     | [GOTO Elimination](#phase-4-goto-elimination)           | ✅ Complete                | Modern control flow throughout codebase           |
| 5     | [FORD Documentation](#phase-5-ford-documentation)       | 🚧 Infrastructure Complete | Automated documentation generation                |
| 6     | [Parameter Refactoring](#phase-6-parameter-refactoring) | 🚧 In Progress             | Systematic parameter organization                 |
| 7     | [Utilities Refactoring](#phase-7-utilities-refactoring) | 📋 Planned                 | Utility module reorganization                     |
| 8     | [I/O Consolidation](#phase-8-io-consolidation)          | 📋 Planned                 | Centralized file handling operations              |
| 9     | [Architecture Review](#phase-9-architecture-review)     | 📋 Planned                 | Global architecture optimization                  |
| 10    | [Testing Framework](#phase-10-testing-framework)        | 📋 Planned                 | Unit and integration testing                      |

---

## Phase 1: CMake Build System ✅ **COMPLETE**

### Overview
Replaced the legacy build system with a modern CMake-based solution supporting multiple compilers and platforms.

### Key Achievements
- **Cross-platform Support**: Unified build system for Linux/Windows
- **Automatic Dependency Resolution**: Intelligent source file discovery and ordering
- **Multi-compiler Support**: Intel Fortran (ifort/ifx), GNU Fortran (gfortran)
- **Build Scripts**: Automated build scripts (`build.sh`/`build.bat`) with dependency checking

### Technical Implementation
- **Automatic Source Discovery**: Recursively finds all Fortran files in `src/`
- **Dependency Ordering**: 11-group pattern-based ordering system for compilation
- **Compiler-Specific Flags**: Optimized flags for each supported compiler
- **Build Presets**: CMakePresets.json for quick configuration

### Documentation
- 📋 [CMake Implementation Summary](cmake/IMPLEMENTATION_SUMMARY.md)
- 📋 [Build Adjustments](cmake/CMAKE_BUILD_ADJUSTMENTS_OCmod2.md)

### Impact
- Eliminated Windows/Intel Fortran-only dependencies
- Modernized deprecated Fortran constructs
- Established consistent build environment across platforms

---

## Phase 2: HDF5 Modernization ✅ **COMPLETE**

### Overview
Replaced bundled HDF5 mod files with flexible system that supports both system libraries and fresh compilation.

### Key Achievements
- **System Integration**: Automatic detection of system HDF5 libraries
- **Flexible Compilation**: Option to download and compile HDF5 from source
- **Cross-platform**: Consistent HDF5 handling across operating systems
- **Build Integration**: Seamless CMake integration with dependency management

### Technical Details
- **Linux/Unix**: Uses `find_package(HDF5)` for system library detection
- **Windows**: Supports provided libraries in `external/` directory
- **Source Compilation**: Automated download and compilation when system libraries unavailable
- **Library Types**: Supports both shared and static HDF5 libraries

### Impact
- Eliminated dependency on outdated bundled libraries
- Improved compatibility with modern HDF5 versions
- Reduced maintenance burden for library updates

---

## Phase 3: Module Disaggregation ✅ **COMPLETE**

### Overview
Disaggregated large monolithic modules into smaller, focused files organized in logical directory structures.

### Key Achievements
- **Directory Restructuring**: Organized source files into functional directories
- **Module Separation**: Split large modules into focused, single-purpose files
- **Eliminated `src/modules/`**: Distributed modules to appropriate functional directories
- **Improved Clarity**: More descriptive filenames and clearer responsibilities

### Major Refactoring Examples
- **VSmod.f90** → Multiple visualization modules in `src/visualisation/`
- **SYmod.f90** → System modules in `src/simulation/`
- **SMmod.f90** → Sediment modules in appropriate directories
- **OCmod.f90** → Overland flow modules in `src/compute/`

### Technical Implementation
- Preserved public interfaces during module separation
- Maintained compilation dependencies through careful ordering
- Updated CMake dependency resolution for new file structure

### Documentation
- 📋 [Refactoring Reports](refactor_reorg/) - Individual module refactoring details

### Impact
- Improved code navigability and maintainability
- Clearer module responsibilities and dependencies
- Easier debugging and development workflow

---

## Phase 4: GOTO Elimination ✅ **COMPLETE**

### Overview
Systematically eliminated all GOTO statements throughout the codebase, replacing them with modern Fortran control structures.

### Key Achievements
- **Complete GOTO Removal**: Eliminated all GOTO statements from the codebase
- **Modern Control Flow**: Replaced with DO loops, IF-THEN-ELSE, and structured programming
- **Error Handling Modernization**: Improved error handling patterns
- **Code Clarity**: More readable and maintainable control flow

### Files Refactored
- `src/util/getdirqq_winIntel.f90` - Error handling improvements
- `src/visualisation/visualisation_interface_left.f90` - Control flow modernization
- `src/modules/utilsmod.f90` - Loop and conditional restructuring
- `src/modules/OCmod.f90` - Flow control improvements
- Multiple other modules with GOTO usage

### Technical Approach
- **Structured Replacement**: Each GOTO replaced with appropriate control structure
- **Loop Optimization**: Converted GOTO-based loops to proper DO loops
- **Error Handling**: Improved error handling with early returns and proper status checking
- **Code Validation**: Ensured functional equivalence after refactoring

### Documentation
- 📋 [GOTO Usage Report](refactor_goto/report_goto_usage.md)
- 📋 [Individual Refactoring Reports](refactor_goto/) - Per-file refactoring details

### Impact
- Improved code readability and maintainability
- Eliminated potential control flow confusion
- Aligned with modern Fortran best practices

---

## Phase 5: FORD Documentation ✅ **INFRASTRUCTURE COMPLETE**

### Overview
Implemented comprehensive documentation generation using FORD (Fortran Documenter) tool with automated build integration.

### Key Achievements
- **Infrastructure Complete**: Full FORD setup with build system integration
- **Baseline Coverage**: 12% of files (14/114) with FORD-compliant documentation
- **Automated Generation**: CMake integration for documentation builds
- **Quality Tools**: Documentation coverage analysis and template generation
- **Cross-platform**: Works on both Linux and Windows development environments

### Technical Implementation
- **FORD Configuration**: `ford_project.md` with project-wide settings
- **Build Integration**: CMake targets for documentation generation
- **Template System**: Automated template generation for undocumented files
- **Coverage Monitoring**: Tools to track documentation progress
- **VS Code Integration**: Development environment setup for efficient documentation

### Current Status
- **✅ Infrastructure**: Complete and operational
- **✅ Exemplary Documentation**: `src/Shetran.f90` serves as documentation model
- **🚧 Implementation**: Ready for systematic documentation of remaining 100+ files

### Documentation
- 📋 [FORD Documentation Plan](refactor_docs/plan_FORD_docs.md)

### Impact
- Established foundation for comprehensive documentation
- Automated documentation generation and deployment
- Improved developer onboarding and code understanding

---

## Phase 6: Parameter Refactoring 🚧 **IN PROGRESS**

### Overview
Systematic reorganization of the `src/parameters/` directory to separate true parameters from variables and procedural code.

### Planned Achievements
- **Clear Separation**: Distinguish parameters, variables, and procedures
- **Logical Organization**: Group related parameters by function and scope
- **Consistent Naming**: Implement clear, descriptive naming conventions
- **Documentation**: Full FORD documentation for all parameter modules

### Proposed Structure
```
src/parameters/
├── core/                    # System-wide constants and dimensions
│   ├── system_constants.f90 # Version info, banners, fundamental constants
│   ├── array_dimensions.f90 # Grid dimensions, array sizing
│   ├── numerical_constants.f90 # Mathematical constants
│   └── error_constants.f90  # Error codes and types
├── physics/                 # Physical constants
│   └── physical_constants.f90 # Gravity, density, viscosity
├── components/              # Component-specific parameters
│   ├── column_*.f90         # Column model parameters
│   ├── link_*.f90           # Link model parameters
│   ├── sediment_*.f90       # Sediment parameters
│   └── plant_*.f90          # Plant parameters
└── global_system/           # Global system variables
    ├── file_units.f90       # File unit definitions
    ├── global_arrays.f90    # Large global arrays
    └── grid_variables.f90   # Grid-related variables
```

### Variable Renaming Strategy
- **Systematic Renaming**: Comprehensive variable renaming for clarity
- **Mapping Documentation**: Complete mapping of old to new variable names
- **Automated Tools**: Scripts to assist with renaming process
- **Validation**: Ensure functional equivalence after renaming

### Current Progress
- **Planning Complete**: Detailed refactoring plan documented
- **Renaming Map**: CSV file with proposed variable name changes
- **Ready for Implementation**: Infrastructure and planning in place

### Documentation
- 📋 [Parameter Refactoring Plan](refactor_parameters/plan_parameter_refactor.md)
- 📋 [Variable Renaming Map](refactor_parameters/plan_parameter_renaming.csv)

---

## Phase 7: Utilities Refactoring 📋 **PLANNED**

### Overview
Reorganization of the `src/util/` directory to improve utility module structure and eliminate remaining legacy code patterns.

### Planned Activities
- **Module Separation**: Break up large utility modules into focused components
- **Function Organization**: Group related utility functions logically
- **Cross-platform Consistency**: Unify platform-specific utility implementations
- **Documentation**: Complete FORD documentation for all utility modules
- **Error Handling and Messages**: Standartise error messages and centralise their handling

### Target Areas
- File I/O utilities
- Mathematical and numerical utilities
- String processing utilities
- System interface utilities
- Data validation utilities
- Error handling utilities

### Dependencies
- Completion of Phase 6 (Parameter Refactoring)
- Updated parameter references throughout utility modules

---

## Phase 8: I/O Consolidation 📋 **PLANNED**

### Overview
Consolidation of all file handling operations from across the codebase into the `src/io/` directory, excluding visualization-specific I/O which remains in `src/visualisation/`. This phase creates a centralized, well-organized file I/O system with clear responsibilities and consistent error handling.

### Planned Activities
- **File Movement**: Relocate scattered I/O modules to centralized `src/io/` structure
- **Interface Standardization**: Create consistent interfaces for input/output operations
- **Error Handling**: Implement comprehensive, standardized I/O error handling
- **Documentation**: Complete FORD documentation for all I/O modules

### Target Areas
- Core file data loading utilities (`mod_load_filedata.f90`)
- Specialized data readers (subsurface, hydraulic structures)
- Framework output management
- Command-line interface processing
- Common I/O utilities and validation

### Proposed Structure
```
src/io/
├── input/                       # Input file handling modules
├── output/                      # Output file handling modules
├── common/                      # Shared I/O functionality
└── interfaces/                  # I/O interface definitions
```

### Key Migrations
- `util/mod_load_filedata.f90` → `io/input/filedata_loader.f90`
- `compute/subsurface_flow/subsurface_io.f90` → `io/input/subsurface_data_reader.f90`
- `compute/hydraulic_structures/zq_table_reader.f90` → `io/input/hydraulic_data_reader.f90`
- `util/getdirqq.f90` → `io/input/command_line_interface.f90`
- I/O portions of `framework_output_manager.f90` → `io/output/framework_output.f90`

### Benefits
- **Centralized I/O**: All file operations in logical, discoverable locations
- **Reduced Coupling**: Clear separation between I/O and computational logic
- **Improved Maintainability**: Easier to modify, debug, and extend I/O operations
- **Consistent Error Handling**: Standardized error reporting across all I/O operations

### Documentation
- 📋 [Detailed I/O Consolidation Plan](docs/reports/refactor_io/plan_io_refactor.md)

### Dependencies
- Completion of Phase 7 (Utilities Refactoring)
- Stable parameter interfaces from Phase 6
- Coordination with visualization team (exclusion of visualization I/O)

### Success Metrics
- **Functional Correctness**: Zero regression in file reading/writing behavior
- **Code Organization**: All non-visualization I/O consolidated in `src/io/`
- **Maintainability**: Reduced coupling between I/O and computational modules
- **Documentation**: Complete FORD documentation for all I/O modules
- **Error Handling**: Comprehensive, consistent error reporting

---

## Phase 9: Architecture Review 📋 **PLANNED**

### Overview
Comprehensive review of the overall codebase architecture using call-graph analysis and the refactored parameters/utilities to identify further optimization opportunities.

### Planned Activities
- **Call-Graph Analysis**: Generate and analyze complete call dependency graph
- **Module Dependencies**: Review inter-module dependencies for optimization
- **Interface Design**: Evaluate and improve module interfaces
- **Performance Analysis**: Identify architectural bottlenecks
- **Further Reorganization**: Implement additional structural improvements based on analysis

### Tools and Methods
- **Dependency Analysis**: Automated tools to generate call graphs
- **Architecture Visualization**: Graphical representation of module relationships
- **Performance Profiling**: Identify hot paths and optimization opportunities
- **Code Metrics**: Quantitative analysis of code complexity and coupling

### Dependencies
- Completion of Phases 6-8 (Parameter Refactoring, Utilities Refactoring, and I/O Consolidation)
- Stable codebase with comprehensive documentation

---

## Phase 10: Testing Framework 📋 **PLANNED**

### Overview
Establishment of comprehensive testing infrastructure including both unit testing and integration testing capabilities.

📋 **[Detailed Testing Plan](docs/reports/testing/plan_testing.md)** - Complete implementation strategy and framework evaluation

### Testing Strategy

#### 10.1 Unit Testing Framework
- **Testing Library**: Selection and integration of Fortran unit testing framework
  - Options: FRUIT, FUnit, or pFUnit
- **Module Testing**: Individual module and subroutine testing
- **Test Organization**: Parallel test directory structure mirroring source organization
- **Automated Execution**: Integration with CMake build system for automated test runs

#### 10.2 Integration Testing
- **End-to-End Testing**: Complete simulation workflow testing
- **Example Validation**: Automated validation against existing example cases
- **Performance Regression**: Monitoring for performance degradations
- **Cross-platform Validation**: Ensure consistent results across platforms

#### 10.3 Test Infrastructure
- **Test Data Management**: Version-controlled test datasets and expected outputs
- **Continuous Integration**: Automated testing on code changes
- **Coverage Analysis**: Code coverage reporting and monitoring
- **Performance Benchmarking**: Automated performance testing and reporting

### Proposed Test Structure
```
tests/
├── unit/                    # Unit tests mirroring src/ structure
│   ├── compute/
│   ├── io/
│   ├── parameters/
│   ├── simulation/
│   └── util/
├── integration/             # Integration tests
│   ├── examples/            # Tests based on existing examples
│   ├── workflows/           # Complete simulation workflows
│   └── performance/         # Performance regression tests
├── data/                    # Test datasets and expected outputs
└── tools/                   # Testing utilities and scripts
```

### Implementation Phases
1. **Framework Selection**: Evaluate and select appropriate testing framework
2. **Infrastructure Setup**: CMake integration and basic test structure
3. **Critical Module Testing**: Start with core computational modules
4. **Integration Test Implementation**: Validate existing examples
5. **Comprehensive Coverage**: Systematic testing of all modules
6. **CI/CD Integration**: Automated testing pipeline

### Success Metrics
- **Unit Test Coverage**: Target >80% code coverage for critical modules
- **Integration Test Coverage**: All existing examples pass automated validation
- **Regression Prevention**: Zero tolerance for functionality regressions
- **Performance Monitoring**: Automated detection of performance degradations

### Dependencies
- Completion of architectural refactoring phases (6-9)
- Stable module interfaces and clear module responsibilities
- Comprehensive documentation for test development guidance

---

## Cross-Cutting Concerns

### Version Control and Git Management
- **Branch Strategy**: Feature branches for each refactoring phase
- **Commit Standards**: Descriptive commit messages with phase references
- **Documentation**: Comprehensive commit history documenting all changes
- **Release Tags**: Version tagging for major refactoring milestones

### Quality Assurance
- **Code Reviews**: Peer review process for all refactoring changes
- **Functional Validation**: Ensure all changes maintain computational correctness
- **Performance Monitoring**: Track performance impact of refactoring changes
- **Documentation Standards**: Maintain high documentation quality throughout

### Developer Workflow
- **VS Code Integration**: Optimized development environment setup
- **Build Automation**: Streamlined build and testing processes
- **Documentation Tools**: Efficient tools for maintaining documentation
- **Debugging Support**: Improved debugging capabilities through clearer code structure

---

## Success Metrics and Validation

### Quantitative Metrics
- **Build Time**: Reduction in compilation time through improved dependency management
- **Documentation Coverage**: Target >90% FORD documentation coverage
- **Code Quality**: Elimination of legacy constructs (GOTO, deprecated syntax)
- **Test Coverage**: Comprehensive test coverage for critical functionality
- **Cross-platform Consistency**: Identical results across supported platforms

### Qualitative Improvements
- **Developer Experience**: Improved ease of development and debugging
- **Code Maintainability**: Clearer code organization and documentation
- **New Developer Onboarding**: Reduced time for new developers to become productive
- **Research Productivity**: Easier modification and extension of the model

### Validation Methods
- **Example Verification**: All existing examples produce identical results
- **Performance Benchmarking**: No performance degradation from refactoring
- **User Acceptance**: Positive feedback from research community users
- **Long-term Maintenance**: Reduced effort required for ongoing maintenance

---

## Risk Management

### Technical Risks
- **Functional Regression**: Risk of introducing computational errors during refactoring
  - *Mitigation*: Comprehensive testing against existing examples
- **Performance Degradation**: Risk of performance loss during reorganization
  - *Mitigation*: Continuous performance monitoring and benchmarking
- **Compilation Issues**: Risk of breaking builds during major changes
  - *Mitigation*: Incremental changes with frequent build validation

### Project Risks
- **Resource Allocation**: Risk of insufficient time/resources for comprehensive refactoring
  - *Mitigation*: Phased approach allowing for priority-based implementation
- **Knowledge Transfer**: Risk of losing institutional knowledge during changes
  - *Mitigation*: Comprehensive documentation of all changes and decisions
- **User Disruption**: Risk of disrupting ongoing research during refactoring
  - *Mitigation*: Maintaining stable release branches during development

---

## Timeline and Dependencies

### Critical Path Dependencies
1. Parameter Refactoring → Utilities Refactoring → I/O Consolidation → Architecture Review
2. FORD Documentation → Comprehensive Testing
3. Architecture Review → Performance Optimization  
4. All phases → Final validation and release

---

## Conclusion

The SHETRAN refactoring project represents a comprehensive modernization effort that has already achieved significant milestones in build system modernization, code organization, and documentation infrastructure. With five major phases complete and a clear roadmap for the remaining work, the project is well-positioned to deliver a maintainable, well-documented, and thoroughly tested codebase that will serve the hydrological modeling community for years to come.

The systematic approach, with its emphasis on maintaining functional correctness while improving code quality, ensures that the refactoring effort enhances rather than disrupts the valuable scientific computing capabilities that SHETRAN provides to researchers worldwide.

---

## References and Related Documents

### Phase Documentation
- 📋 [CMake Implementation](docs/reports/cmake/)
- 📋 [Module Refactoring](docs/reports/refactor_reorg/)
- 📋 [GOTO Elimination](docs/reports/refactor_goto/)
- 📋 [FORD Documentation](docs/reports/refactor_docs/)
- 📋 [Parameter Refactoring](docs/reports/refactor_parameters/)
- 📋 [I/O Consolidation](docs/reports/refactor_io/plan_io_refactor.md)
- 📋 [Testing Strategy](docs/reports/testing/plan_testing.md)

### Build and Development
- 📋 [CMake Build Instructions](CMAKE_BUILD.md)
- 📋 [Compilation Guide](COMPILING.md)
- 📋 [Development Setup](docs/reports/cmake/)

### Project Management
- 📋 [Changelog](CHANGELOG.md)
- 📋 [Bug Reports](BUGS.md)
- 📋 [TODO Items](TODO.md)

---

*This document is maintained as part of the SHETRAN refactoring project and should be updated as phases progress and new insights are gained.*
