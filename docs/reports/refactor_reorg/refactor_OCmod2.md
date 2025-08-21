# Overland Channel Flow Module Refactoring

This directory contains the refactored components of the original `OCmod2.f90` module, which handles overland and channel flow calculations in the SHETRAN hydrological modeling system.

## Original Module

The original `OCmod2.f90` was a large monolithic module containing all overland channel flow functionality. It has been broken down into focused, maintainable modules.

## Refactored Structure

### 1. **oc_parameters.f90**
Contains mathematical and physical constants used in flow calculations:
- Manning's equation coefficients (F23, F53)
- Minimum depth and flow thresholds (DZMIN, RDZMIN, H23MIN)
- Gravity-related constants (ROOT2G)

### 2. **oc_data_management.f90** 
Manages data arrays and provides access functions:
- Water surface elevations (`HRFZZ`)
- Discharge values (`qsazz`)
- Cross-section lookup tables (`xstab`)
- Getter/setter functions (`GETHRF`, `SETHRF`, `GETQSA`, `SETQSA`, etc.)

### 3. **oc_node_flows.f90**
Handles flow calculations at network nodes:
- `OCNODE`: Flow calculations at confluences and junctions
- `FNODE`: Function evaluation for node flow iteration
- Mass conservation at channel junctions
- Iterative solution methods for complex flow networks

### 4. **oc_hydraulic_calculations.f90**
Core hydraulic calculations:
- `OCCODE`: Conveyance and derivative calculations for channel links
- `CONVEYAN`: Unified conveyance calculation for different channel types
- `QWEIR`: Weir flow calculations (drowned and undrowned conditions)

### 5. **oc_channel_flow_types.f90**
Different types of channel flow calculations:
- `OCQBC`: External boundary conditions
- `OCQBNK`: Channel bank flows
- `OCQGRD`: Overland flow between land elements
- `OCQLNK`: Flow between two channel links
- `OCQMLN`: Flow between multiple channel links
- Integration with ZQ module for stage-discharge relationships

### 6. **oc_flow_control.f90**
Flow control and mass conservation:
- `OCFIX`: Ensures flow consistency and mass conservation
- Handles small flow corrections and depth adjustments
- Iterative flow adjustment algorithms
- Error handling and reporting

### 7. **OCmod2.f90** (Main Interface)
Provides backward compatibility by re-exporting all public interfaces from the individual modules. This ensures existing code continues to work without modification.

## Benefits of Refactoring

1. **Modularity**: Each module has a clear, focused responsibility
2. **Maintainability**: Easier to understand, debug, and modify specific functionality
3. **Reusability**: Individual components can be reused in other parts of the system
4. **Testing**: Each module can be tested independently
5. **Backward Compatibility**: Existing code using `OCmod2` continues to work unchanged

## Dependencies

The modules maintain the original dependencies on:
- `SGLOBAL`: Global SHETRAN variables and constants
- `ZQmod`: Stage-discharge relationship module
- `AL_D`: Algebraic data structures

## Usage

To use the refactored modules, simply continue using:
```fortran
USE OCmod2
```

The main interface module will automatically include all necessary functionality.

For new development, you can import specific modules as needed:
```fortran
USE oc_hydraulic_calculations, ONLY: CONVEYAN, QWEIR
USE oc_node_flows, ONLY: OCNODE
```

## Migration

No changes are required to existing code that uses `OCmod2`. The refactoring maintains complete backward compatibility through the main interface module.
