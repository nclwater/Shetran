# SHETRAN Testing Strategy and Implementation Plan

**Project:** SHETRAN Hydrological Model  
**Document Version:** 1.0  
**Date:** 5 September 2025  
**Status:** Planning Phase  

---

## Executive Summary

This document outlines a comprehensive testing strategy for the SHETRAN hydrological model, designed to ensure code reliability, prevent regressions, and support ongoing refactoring efforts. The testing framework addresses both module-level unit testing and system-level integration testing, with particular emphasis on leveraging existing example datasets for validation.

### Key Objectives
- **Quality Assurance**: Ensure computational accuracy and numerical stability
- **Regression Prevention**: Detect unintended changes during refactoring
- **Refactoring Support**: Enable safe code modernization with confidence
- **Documentation**: Provide executable specifications of expected behavior
- **Continuous Integration**: Automated testing for sustainable development

---

## Testing Architecture Overview

### Two-Tier Testing Strategy

The SHETRAN testing framework employs a dual-level approach that balances comprehensive coverage with practical implementation:

#### **Tier 1: Integration Testing** (Priority 1 - Immediate Implementation)
- **Primary Focus**: End-to-end simulation validation using existing examples
- **Coverage**: Complete simulation workflows from input to output
- **Data Source**: Existing `examples/` directory (12 test cases)
- **Validation Method**: Output comparison against established baselines

#### **Tier 2: Unit Testing** (Priority 2 - Post-Refactoring Implementation)
- **Primary Focus**: Individual module and subroutine validation
- **Coverage**: Core computational modules and utility functions
- **Validation Method**: Isolated testing of mathematical functions and algorithms
- **Integration Point**: Aligns with ongoing refactoring phases 6-9

---

## Integration Testing Implementation

### 1. Example-Based Test Suite

The SHETRAN project contains a rich collection of example cases that serve as comprehensive integration tests:

#### Available Test Cases
```
examples/
├── 38014-100m-SurfaceErrors/    # Surface error handling
├── Cobres/                      # Water flow and sediment (2-month simulation)
├── Cobres-ExtraOutputDischargePoints/  # Discharge point output testing
├── Cobres-ExtraOutputWaterTable/       # Water table output testing
├── Cobres1D/                    # 1D simulation variant
├── Slapton/                     # Slapton catchment case study
├── dano100m/                    # 100m resolution validation
├── dunsop/                      # Dunsop catchment baseline
├── dunsop-hot1/                 # Hotstart functionality test 1
├── dunsop-hot2/                 # Hotstart functionality test 2
├── foston100m/                  # Foston 100m resolution case
└── reservoir-ZQmodule-example/  # ZQ module validation
```

#### Test Categories by Functionality

**Core Hydrological Processes**

- `Cobres/` - Water flow and sediment transport
- `Slapton/` - Comprehensive catchment simulation
- `dunsop/` - Standard hydrological validation

**Spatial Resolution Validation**

- `dano100m/`, `foston100m/` - 100m grid resolution testing
- `38014-100m-SurfaceErrors/` - Surface error handling at resolution

**Advanced Features**

- `dunsop-hot1/`, `dunsop-hot2/` - Hotstart/restart functionality
- `reservoir-ZQmodule-example/` - Specialized ZQ module testing
- `Cobres1D/` - Reduced-dimension simulation

**Output Validation**

- `Cobres-ExtraOutputDischargePoints/` - Discharge point data extraction
- `Cobres-ExtraOutputWaterTable/` - Water table output verification

### 2. Test Infrastructure Design

#### Automated Test Execution Framework
```bash
tests/
├── integration/
│   ├── run_all_examples.sh           # Master test execution script
│   ├── compare_outputs.py            # Output comparison utilities
│   ├── baseline_data/                # Reference output datasets
│   │   ├── cobres_baseline/
│   │   ├── slapton_baseline/
│   │   └── [other_examples]/
│   ├── tolerance_config.yaml         # Numerical tolerance specifications
│   └── test_reports/                 # Test execution reports
├── data/
│   ├── minimal_test_cases/           # Lightweight test datasets
│   └── regression_datasets/          # Historical regression test data
└── tools/
    ├── baseline_generator.sh         # Generate new baselines
    ├── performance_monitor.py        # Performance regression detection
    └── test_result_analyzer.py       # Test result analysis
```

#### CMake Integration
```cmake
# Add to CMakeLists.txt
enable_testing()

# Integration tests
add_test(NAME cobres_integration 
         COMMAND ${CMAKE_BINARY_DIR}/bin/shetran 
         WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/examples/Cobres)

add_test(NAME slapton_integration
         COMMAND ${CMAKE_BINARY_DIR}/bin/shetran
         WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/examples/Slapton)

# Performance regression tests
add_test(NAME performance_regression
         COMMAND python3 ${CMAKE_SOURCE_DIR}/tests/tools/performance_monitor.py)
```

### 3. Output Validation Strategy

#### Multi-Level Comparison Framework

**Level 1: Critical Results (Strict Tolerance)**

- Water balance totals (±0.01%)
- Peak discharge values (±0.1%)
- Total sediment yields (±0.1%)

**Level 2: Detailed Time Series (Moderate Tolerance)**

- Hourly discharge rates (±1.0%)
- Water table elevations (±0.5%)
- Soil moisture distributions (±2.0%)

**Level 3: Diagnostic Outputs (Relaxed Tolerance)**

- Visualization data (±5.0%)
- Intermediate calculation steps (±2.0%)
- Debug/logging information (text comparison)

#### Baseline Management

- **Initial Baseline Generation**: Use current stable build outputs as references
- **Baseline Updates**: Controlled updates only after thorough review
- **Version Control**: Baseline data stored in Git LFS for efficient handling
- **Platform Variants**: Separate baselines for different compiler/platform combinations

---

## Unit Testing Implementation

### 1. Strategic Module Selection

Given the ongoing refactoring phases, unit testing implementation should be coordinated with module stabilization:

#### Phase 1: Core Mathematical Modules (Immediate Priority)
```
src/compute/
├── execution_control/
│   ├── framework_mass_balance.f90      # Critical: Mass balance calculations
│   ├── framework_spatial_setup.f90     # Critical: Spatial discretization
│   └── framework_element_sorting.f90   # Important: Element ordering
├── flow_calculator.f90                 # Critical: Flow computations
└── hydraulic_helpers.f90               # Important: Hydraulic calculations
```

#### Phase 2: I/O and Data Management (Secondary Priority)
```
src/io/
├── contaminant_data_reader.f90         # Data input validation
├── meteorological_input.f90            # Met data processing
└── mod_load_filedata.f90               # File data loading
```

#### Phase 3: Utility and Supporting Modules (Final Priority)
```
src/util/
├── increment_utilities.f90             # Mathematical utilities
└── contaminant_utilities.f90           # Contaminant calculations
```

### 2. Unit Test Structure

#### Test Organization Pattern
```
tests/unit/
├── compute/
│   ├── test_framework_mass_balance.f90
│   ├── test_flow_calculator.f90
│   └── test_hydraulic_helpers.f90
├── io/
│   ├── test_meteorological_input.f90
│   └── test_mod_load_filedata.f90
├── util/
│   └── test_increment_utilities.f90
└── common/
    ├── test_utilities.f90              # Shared testing utilities
    └── assertion_framework.f90         # Custom assertion macros
```

#### Test Methodology per Module Type

**Mathematical/Computational Modules**
- **Function Testing**: Individual mathematical functions with known inputs/outputs
- **Boundary Testing**: Edge cases and limit conditions
- **Numerical Stability**: Precision and convergence testing
- **Algorithm Validation**: Comparison with analytical solutions where available

**I/O and Data Modules**
- **File Format Validation**: Correct parsing of input file formats
- **Error Handling**: Graceful handling of malformed inputs
- **Data Integrity**: Validation of data transformations and conversions
- **Memory Management**: Proper allocation and deallocation testing

### 3. Testing Framework Selection

Based on Fortran ecosystem evaluation (see [Appendix A](#appendix-a-fortran-testing-frameworks)), the recommended framework progression:

#### Primary Choice: **FRUIT** (Fortran Unit Test Framework)
- **Advantages**: Lightweight, minimal dependencies, easy CMake integration
- **Suitable for**: Initial unit test implementation and core module testing
- **Integration**: Simple `include` directives, compatible with legacy Fortran

#### Secondary Choice: **pFUnit** (NASA's parallel unit testing framework)
- **Advantages**: Advanced features, parallel testing support, comprehensive assertions
- **Suitable for**: Advanced testing scenarios and large-scale module testing
- **Migration Path**: Upgrade from FRUIT as testing sophistication increases

---

## Implementation Timeline and Coordination with Refactoring

### Phase Coordination Strategy

The testing implementation is designed to complement and support the ongoing refactoring phases:

#### **Phase 6 (Current): Parameter Refactoring** 🚧
**Testing Actions:**
- Begin integration testing infrastructure setup
- Establish baseline outputs from current examples
- Implement basic CMake test integration

**Rationale:** Parameter refactoring provides stable interfaces for test development

#### **Phase 7 (Planned): Utilities Refactoring** 📋
**Testing Actions:**
- Implement unit tests for utility modules as they are refactored
- Validate refactored utilities against original implementations
- Establish unit testing patterns and conventions

**Rationale:** Utility modules are typically self-contained and ideal for initial unit testing

#### **Phase 8 (Planned): I/O Consolidation** 📋
**Testing Actions:**
- Implement integration testing infrastructure setup
- Establish baseline outputs from current examples
- Implement basic CMake test integration

**Rationale:** I/O consolidation provides stable file handling interfaces for test development

#### **Phase 9 (Planned): Architecture Review** 📋  
**Testing Actions:**
- Complete integration testing implementation
- Implement unit tests for core computational modules
- Establish performance baseline and regression testing

**Rationale:** Architectural stability enables comprehensive test suite implementation

#### **Phase 10 (Planned): Testing Framework** 📋
**Testing Actions:**
- Full unit test coverage for all refactored modules
- Automated CI/CD pipeline integration
- Comprehensive documentation and testing guidelines

**Rationale:** Dedicated testing phase ensures complete coverage and integration

### Implementation Milestones

| Milestone                           | Timeline            | Dependencies           | Deliverables                             |
| ----------------------------------- | ------------------- | ---------------------- | ---------------------------------------- |
| **M1: Integration Testing Setup**   | Phase 6 completion  | Stable build system    | Automated example testing, baseline data |
| **M2: Unit Testing Infrastructure** | Phase 8 start       | Refactored utilities   | Testing framework, first unit tests      |
| **M3: Core Module Unit Tests**      | Phase 9 start       | Stable architecture    | Comprehensive unit test coverage         |
| **M4: Complete Testing Framework**  | Phase 10 completion | All refactoring phases | Full automation, CI integration          |

### Risk Mitigation

#### **Risk: Interface Changes During Refactoring**

**Mitigation:** Focus on integration testing initially; defer detailed unit testing until interfaces stabilize

#### **Risk: Baseline Drift**

**Mitigation:** Careful baseline management with explicit approval processes for updates

#### **Risk: Performance Impact**

**Mitigation:** Performance monitoring integrated into testing framework; performance regression alerts

#### **Risk: Testing Overhead**

**Mitigation:** Automated execution integrated into build process; minimal developer overhead

---

## Success Metrics and Quality Gates

### Integration Testing Metrics

#### **Functional Correctness**

- **Target**: 100% of example cases pass automated testing
- **Measurement**: Binary pass/fail for each example case
- **Quality Gate**: Zero tolerance for example case failures

#### **Numerical Accuracy**

- **Target**: All critical outputs within ±0.01% tolerance
- **Measurement**: Statistical analysis of output deviations
- **Quality Gate**: 95% of outputs within strict tolerance bounds

#### **Performance Stability**

- **Target**: <5% performance variation between versions
- **Measurement**: Execution time monitoring across all examples
- **Quality Gate**: No performance regressions >10% without explicit approval

### Unit Testing Metrics

#### **Code Coverage**

- **Target**: >80% line coverage for critical computational modules
- **Measurement**: Automated coverage analysis during builds
- **Quality Gate**: No decrease in coverage without justification

#### **Test Quality**

- **Target**: >95% test pass rate across all platforms
- **Measurement**: Automated test execution reporting
- **Quality Gate**: Zero tolerance for test failures in release builds

### Long-term Quality Indicators

#### **Defect Detection**

- **Target**: >90% of bugs caught by automated testing before manual discovery
- **Measurement**: Bug tracking and test failure correlation analysis
- **Review Period**: Quarterly assessment and testing improvement

#### **Development Velocity**

- **Target**: Maintained or improved development speed despite testing overhead
- **Measurement**: Feature delivery timeline tracking
- **Review Period**: Monthly development velocity assessment

---

## Resource Requirements and Dependencies

### Infrastructure Requirements

#### **Computational Resources**

- **Build Server**: Dedicated CI/CD server for automated testing
- **Storage**: ~10GB for baseline data and test artifacts
- **Network**: Reliable connection for Git LFS baseline data access

#### **Software Dependencies**

- **CMake**: Enhanced with testing configurations
- **Python 3.x**: For test utilities and output analysis
- **Git LFS**: For large baseline dataset management
- **Fortran Testing Framework**: FRUIT initially, potential pFUnit upgrade

### Training and Documentation

#### **Developer Training**

- **Testing Best Practices**: Team training on unit testing methodology
- **Framework Usage**: Training on selected testing framework utilization
- **Baseline Management**: Procedures for baseline updates and management

#### **Documentation Requirements**

- **Testing Guidelines**: Comprehensive guide for test development
- **CI/CD Documentation**: Build and deployment pipeline documentation
- **Troubleshooting Guide**: Common testing issues and resolutions

---

## Appendix A: Fortran Testing Frameworks

### Overview of Available Frameworks

The Fortran ecosystem offers several testing frameworks, each with distinct advantages and limitations. This evaluation focuses on frameworks suitable for large-scale scientific computing projects like SHETRAN.

### 1. FRUIT (Fortran Unit Test Framework)

#### **Characteristics**

- **Type**: Lightweight unit testing framework
- **Dependencies**: None (pure Fortran)
- **Language Support**: Fortran 90/95/2003/2008
- **Learning Curve**: Minimal

#### **Advantages**

- **Simplicity**: Easy to learn and implement
- **Zero Dependencies**: No external libraries required
- **Legacy Compatibility**: Works with older Fortran standards
- **CMake Integration**: Straightforward build system integration
- **Minimal Overhead**: Low impact on build times

#### **Limitations**

- **Basic Assertions**: Limited assertion capabilities compared to modern frameworks
- **No Parallel Support**: Sequential testing only
- **Limited Reporting**: Basic test result reporting
- **Manual Test Discovery**: Tests must be manually registered

#### **Example Usage**
```fortran
program test_mass_balance
  use fruit
  implicit none
  
  call init_fruit
  call test_water_balance
  call test_mass_conservation
  call fruit_summary
end program

subroutine test_water_balance
  use fruit
  use framework_mass_balance
  implicit none
  
  real(8) :: input_flux, output_flux, expected_balance
  
  input_flux = 100.0d0
  output_flux = 95.0d0
  expected_balance = 5.0d0
  
  call assert_equals(expected_balance, input_flux - output_flux, &
                     "Water balance calculation")
end subroutine
```

#### **Suitability for SHETRAN**

- **Excellent for**: Initial unit testing implementation
- **Good for**: Testing mathematical functions and simple algorithms
- **Adequate for**: Basic integration with existing build system

### 2. pFUnit (NASA's Parallel Fortran Unit Testing Framework)

#### **Characteristics**

- **Type**: Advanced unit testing framework with parallel support
- **Dependencies**: Python (for code generation), MPI (for parallel tests)
- **Language Support**: Modern Fortran (2008+)
- **Learning Curve**: Moderate to steep

#### **Advantages**

- **Advanced Assertions**: Comprehensive assertion library
- **Parallel Testing**: MPI-based parallel test execution
- **Code Generation**: Automatic test runner generation
- **Modern Design**: Object-oriented testing architecture
- **Comprehensive Reporting**: Detailed test result reporting including XML output

#### **Limitations**

- **Complex Setup**: Requires Python and potentially MPI
- **Modern Fortran Only**: Requires Fortran 2008+ features
- **Build Complexity**: More complex CMake integration
- **Learning Curve**: Requires significant investment to learn effectively

#### **Example Usage**
```fortran
@test
subroutine test_hydraulic_calculations()
  use pfunit_mod
  use hydraulic_helpers
  implicit none
  
  real(8) :: depth, velocity, expected_discharge, actual_discharge
  
  depth = 2.0d0
  velocity = 1.5d0
  expected_discharge = 3.0d0
  
  actual_discharge = calculate_discharge(depth, velocity)
  
  @assertEqual(expected_discharge, actual_discharge, tolerance=1.0d-10)
end subroutine
```

#### **Suitability for SHETRAN**

- **Excellent for**: Long-term advanced testing implementation
- **Good for**: Large-scale module testing with parallel capabilities
- **Challenging for**: Immediate implementation due to setup complexity

### 3. FUnit

#### **Characteristics**

- **Type**: Ruby-based Fortran testing framework
- **Dependencies**: Ruby interpreter
- **Language Support**: Fortran 90/95/2003
- **Learning Curve**: Moderate

#### **Advantages**

- **Test Generation**: Ruby-based test generation and execution
- **Good Documentation**: Well-documented with examples
- **Flexible Structure**: Flexible test organization
- **Assertion Library**: Reasonable assertion capabilities

#### **Limitations**

- **Ruby Dependency**: Requires Ruby interpreter
- **Limited Adoption**: Smaller user community
- **Platform Limitations**: May have platform-specific issues
- **Maintenance Concerns**: Less active development

#### **Suitability for SHETRAN**

- **Adequate for**: Projects already using Ruby toolchain
- **Poor for**: SHETRAN due to additional dependency requirements

### 4. FORTLS (Fortran Test Library Suite)

#### **Characteristics**

- **Type**: Minimalist testing library
- **Dependencies**: None
- **Language Support**: Fortran 90+
- **Learning Curve**: Minimal

#### **Advantages**

- **Ultra-lightweight**: Minimal code footprint
- **Self-contained**: Single module implementation
- **Easy Integration**: Simple include and use

#### **Limitations**

- **Very Basic**: Extremely limited functionality
- **No Framework**: Lacks structured testing framework
- **Manual Everything**: All test management manual
- **Limited Assertions**: Basic assertion capabilities only

#### **Suitability for SHETRAN**

- **Adequate for**: Quick and dirty testing
- **Poor for**: Comprehensive testing strategy

### Framework Recommendation Matrix

| Framework  | Setup Complexity | Learning Curve | Feature Richness | SHETRAN Suitability | Recommended Phase                   |
| ---------- | ---------------- | -------------- | ---------------- | ------------------- | ----------------------------------- |
| **FRUIT**  | Low              | Low            | Medium           | High                | Phase 8-9 (Initial Implementation)  |
| **pFUnit** | High             | High           | High             | Medium              | Phase 10+ (Advanced Implementation) |
| **FUnit**  | Medium           | Medium         | Medium           | Low                 | Not Recommended                     |
| **FORTLS** | Low              | Low            | Low              | Low                 | Emergency Only                      |

### Implementation Strategy Recommendation

#### **Phase 1: FRUIT Implementation (Immediate)**
1. **Quick Start**: Implement basic unit tests using FRUIT
2. **Prove Concept**: Validate testing approach with core modules
3. **Build Momentum**: Establish testing culture and practices
4. **Infrastructure**: Set up basic CMake integration and CI

#### **Phase 2: Evaluate pFUnit Migration (Future)**
1. **Assessment**: Evaluate pFUnit benefits vs. implementation cost
2. **Pilot Project**: Test pFUnit with a subset of modules
3. **Migration Plan**: Develop migration strategy if benefits justify cost
4. **Advanced Features**: Leverage parallel testing and advanced assertions

### Technical Integration Considerations

#### **CMake Integration Pattern**
```cmake
# Basic FRUIT integration
find_path(FRUIT_DIR fruit.f90 PATHS ${CMAKE_SOURCE_DIR}/external/fruit)
if(FRUIT_DIR)
    add_subdirectory(${FRUIT_DIR})
    set(TESTING_ENABLED ON)
else()
    message(STATUS "FRUIT not found - testing disabled")
    set(TESTING_ENABLED OFF)
endif()

if(TESTING_ENABLED)
    add_subdirectory(tests)
endif()
```

#### **Directory Structure Integration**
```
external/                    # Third-party dependencies
├── fruit/                   # FRUIT framework (git submodule)
│   ├── fruit.f90
│   └── CMakeLists.txt
tests/                       # Test suite
├── unit/                    # Unit tests
│   ├── CMakeLists.txt
│   └── [test modules]
├── integration/             # Integration tests
└── CMakeLists.txt           # Master test configuration
```

This comprehensive framework evaluation supports the strategic choice of FRUIT for initial implementation, with a clear migration path to pFUnit as the testing infrastructure matures and the potential benefits of advanced features justify the additional complexity.

---

## Conclusion

The SHETRAN testing strategy provides a pragmatic, phased approach to establishing comprehensive test coverage while supporting ongoing refactoring efforts. By prioritizing integration testing using existing examples and coordinating unit testing implementation with module stabilization, this plan ensures both immediate value and long-term sustainability.

The dual-tier approach acknowledges the practical constraints of legacy scientific code while establishing a foundation for modern testing practices. The coordination with refactoring phases ensures that testing implementation supports rather than impedes the ongoing modernization effort.

Success depends on consistent execution, careful baseline management, and maintaining focus on the primary objectives: computational accuracy, regression prevention, and refactoring support. The proposed metrics and quality gates provide clear targets for measuring progress and ensuring the testing framework delivers its intended value.
