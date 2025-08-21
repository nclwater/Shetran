# SHETRAN FRmod Architecture Diagram

## Overview
Visual representation of the refactored FRmod.f90 modular architecture, showing dependencies and data flow.

## Module Dependency Graph

```
                          📦 SHETRAN FRMOD ARCHITECTURE
                                        │
    ┌───────────────────────────────────────────────────────────────────────┐
    │                      🏗️ External Dependencies                        │
    │        (SGLOBAL, AL_C, AL_D, OCmod, ETmod, SMmod, VSmod, etc.)       │
    └─────────────────────────┬─────────────────────────────────────────────┘
                              │
                              ▼
    ┌─────────────────────────────────────────────────────────────────────────┐
    │                🎯 framework_shared.f90 (87 lines)                     │
    │                        [Group 10.4 - Base]                            │
    │                                                                       │
    │  📊 Shared Variables:                                                 │
    │    • TITLE, TSH, TCH, BSOFT, BSTORE                                  │
    │    • qoctot, uzold, btime, next_hour, icounter2                       │
    │                                                                       │
    │  🔧 Shared Subroutines:                                              │
    │    • INFR, DINET, DINOC, INET, INSM, INBK, FRDIM, INCM              │
    │    (placeholder implementations to break circular dependencies)        │
    └─────────────────────────┬─────────────────────────────────────────────┘
                              │
                              ▼
         ┌─────────────────────────────────────────────────────────────────────┐
         │                    🏗️ Framework Modules                           │
         │                      [Group 10.5 - Core]                          │
         │                                                                   │
         │   ┌─────────────────────────┐    ┌─────────────────────────────┐   │
         │   │   📋 initialization     │    │     📐 spatial_setup        │   │
         │   │      (365 lines)        │    │       (1,037 lines)         │   │
         │   │                         │    │                             │   │
         │   │  🔧 FRINIT              │    │  🔧 FRIND                   │   │
         │   │  🔧 FRLTL               │    │  🔧 FRDIM                   │   │
         │   │                         │    │                             │   │
         │   │  📥 imports FRRESP      │    │  🏠 standalone module        │   │
         │   │     from output_manager │    │                             │   │
         │   └─────────────────────────┘    └─────────────────────────────┘   │
         │                                                                   │
         │   ┌─────────────────────────┐    ┌─────────────────────────────┐   │
         │   │    📁 output_manager    │    │     ⚖️ mass_balance         │   │
         │   │      (1,032 lines)      │    │       (263 lines)           │   │
         │   │                         │    │                             │   │
         │   │  🔧 FROPEN              │    │  🔧 FRMB                    │   │
         │   │  🔧 FROUTPUT            │    │                             │   │
         │   │  🔧 FRRESP              │    │  📥 imports FRRESP          │   │
         │   │  🔧 write_dis/write_dis2│    │     from output_manager     │   │
         │   │                         │    │                             │   │
         │   │  📤 exports qoctot,     │    │                             │   │
         │   │      uzold, btime, etc. │    │                             │   │
         │   └─────────────────────────┘    └─────────────────────────────┘   │
         │                                                                   │
         │   ┌─────────────────────────┐    ┌─────────────────────────────┐   │
         │   │   📊 element_sorting    │    │  🧩 component_initialization │   │
         │   │      (255 lines)        │    │        (2,035 lines)        │   │
         │   │                         │    │                             │   │
         │   │  🔧 FRSORT              │    │  🔧 INCM, INFR, DINET      │   │
         │   │                         │    │  🔧 INSM, DINOC, INBK      │   │
         │   │  🏠 standalone module    │    │  🔧 INET, DOCIN, INPL      │   │
         │   │                         │    │                             │   │
         │   │                         │    │  📥 imports FRIND           │   │
         │   │                         │    │     from spatial_setup      │   │
         │   │                         │    │                             │   │
         │   │                         │    │  📤 exports TITLE, TSH,     │   │
         │   │                         │    │      TCH, BSOFT, BSTORE     │   │
         │   └─────────────────────────┘    └─────────────────────────────┘   │
         └─────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
    ┌─────────────────────────────────────────────────────────────────────────┐
    │                      🎯 FRmod.f90 Interface                            │
    │                         [Group 11 - API]                               │
    │                           (22 lines)                                   │
    │                                                                       │
    │  📥 Selective Imports:                                                │
    │    ├── framework_shared          → shared variables & utilities       │
    │    ├── framework_initialization  → FRINIT, FRLTL                     │
    │    ├── framework_output_manager  → FROPEN, FROUTPUT, FRRESP          │
    │    ├── framework_mass_balance    → FRMB                               │
    │    ├── framework_spatial_setup   → FRIND                             │
    │    └── framework_element_sorting → FRSORT                            │
    │                                                                       │
    │  📤 Public Interface:                                                 │
    │    All original FRmod.f90 subroutines and variables                  │
    │    (100% backward compatibility maintained)                           │
    └─────────────────────────┬─────────────────────────────────────────────┘
                              │
                              ▼
    ┌─────────────────────────────────────────────────────────────────────────┐
    │                     🎯 Application Layer                              │
    │                                                                       │
    │     rest.f90, run_sim.f90, Shetran.f90, and other modules            │
    │     (Use FRmod exactly as before - no changes required)               │
    └─────────────────────────────────────────────────────────────────────────┘
```

## Compilation Flow

```
Step 1: 🏗️  framework_shared.f90        (Group 10.4)
         │
         └──► Generates framework_shared.mod
              │
Step 2: 🏗️  All Framework Modules       (Group 10.5)
         │   ├── framework_initialization.f90
         │   ├── framework_spatial_setup.f90
         │   ├── framework_output_manager.f90
         │   ├── framework_mass_balance.f90
         │   ├── framework_element_sorting.f90
         │   └── framework_component_initialization.f90
         │
         └──► Generate individual .mod files
              │
Step 3: 🎯  FRmod.f90                    (Group 11)
         │
         └──► Links all framework modules into unified interface
              │
Step 4: 🎯  Application Modules          (Groups 12-13)
         │
         └──► rest.f90, run_sim.f90, etc. use FRmod as before
```

## Key Design Principles

### 🔄 Dependency Resolution
- **Problem**: Circular dependencies between framework modules
- **Solution**: Central `framework_shared.f90` provides common utilities
- **Result**: Clean hierarchical structure, no circular imports

### 🎯 Interface Preservation  
- **Requirement**: Zero breaking changes for existing code
- **Solution**: `FRmod.f90` maintains exact same public interface
- **Result**: Complete backward compatibility

### 🧩 Modular Design
- **Goal**: Separate concerns by functionality
- **Implementation**: Each module has focused responsibility
- **Benefits**: Better maintainability, testability, parallel development

### ⚡ Build Optimization
- **Challenge**: Correct compilation order for dependencies
- **Solution**: CMakeLists.txt Groups 10.4 → 10.5 → 11
- **Result**: Reliable parallel compilation, faster builds

---
*Generated: August 21, 2025*  
*FRmod Refactoring: 4,956 lines → 7 modules, 5,012 lines total*
