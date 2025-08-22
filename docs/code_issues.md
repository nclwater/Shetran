# SHETRAN Code Standardization Issues Assessment

## Overview

This document provides a comprehensive analysis of non-standard Fortran features and portability issues in the SHETRAN hydrological model codebase. The assessment is based on analysis of the `src/` directory which contains the current, up-to-date code.

## Executive Summary

The SHETRAN codebase contains several categories of non-standard Fortran features that prevent compilation with standard-compliant compilers like GNU gfortran. The issues range from critical architectural problems (Cray pointers) to minor portability concerns (deprecated statements).

## Critical Issues (Must Fix)

### 1. Cray Pointer Usage ⚠️ **HIGH PRIORITY**
**Impact**: Complete compilation failure on standard compilers
**Locations**: Primarily `src/visualisation/visualisation_structure.f90`

**Issues**:

- Non-standard `POINTER(integer_var, fortran_array)` syntax (lines 171-178)
- `INTEGER(INT_PTR_KIND())` usage for pointer arithmetic
- `LOC()` intrinsic function calls for memory address manipulation
- Affects entire visualization subsystem architecture

**Examples**:
```fortran
INTEGER(INT_PTR_KIND())       :: llistend
TYPE(BS), TARGET :: llistend_b ; POINTER(llistend, llistend_b)
TYPE(ES), TARGET :: llistend_e ; POINTER(llistend, llistend_e)
...
IF(ASSOCIATED(pb%next)) THEN ; first = LOC(pb%next) ; ELSE ; first=0 ; ENDIF
```

**Solution Required**: Complete architectural redesign using standard Fortran pointers or derived types with allocatable components.

### 2. Intel Compiler Directives (!DEC$) ⚠️ **HIGH PRIORITY**
**Impact**: Prevents portability to other compilers
**Locations**: Multiple visualization files

**Issues**:

- `!DEC$ REAL:4` - Forces 4-byte real precision
- `!DEC$ ATTRIBUTES DLLEXPORT` - Windows DLL export declarations
- `!DEC$ DEFINE` and `!DEC$ IF/ENDIF` - Conditional compilation blocks
- `!DEC$ ATTRIBUTES DLLEXPORT :: subroutine_name`

**Files Affected**:

- `src/visualisation/visualisation_structure.f90` (lines 3, 504, 520, 610)
- `src/visualisation/visualisation_pass.f90` (lines 10-14, 36, 50-72)
- `src/visualisation/visualisation_read.f90`
- `src/visualisation/visualisation_map.f90`
- `src/visualisation/visualisation_interface_*.f90`

### 3. Windows-Specific Dependencies ⚠️ **HIGH PRIORITY**
**Impact**: Linux compilation failure
**Locations**: Platform-specific modules

**Issues**:

- `USE IFQWIN` - Intel QuickWin graphics library
- `QWIN$FRAMEWINDOW`, `GETHWNDQQ` - Windows GUI functions
- Windows file system dependencies

**Files Affected**:

- `src/util/getdirqq.f90` (line 21)
- `src/parameters/sglobal.f90` (commented DFLIB usage)

## Important Issues (Should Fix)

### 4. SEQUENCE Statements in Derived Types
**Impact**: Memory layout compatibility issues
**Locations**: `src/visualisation/visualisation_structure.f90`

**Issues**:

- `SEQUENCE` statement in `real_middle_and_edges` type (line 83)
- `SEQUENCE` statement in `GS` type (line 135)
- Can cause alignment issues with modern compilers

### 5. Deprecated Language Features
**Impact**: Warning messages, potential future incompatibility

**PAUSE Statements**:

- `src/parameters/sglobal.f90` (lines 427, 481)
- `PAUSE` is obsolescent in modern Fortran

**Issues**:
```fortran
pause
PAUSE  
```

### 6. Non-Standard File Inclusion
**Impact**: Not following modern Fortran practices
**Locations**: `src/visualisation/visualisation_metadata.f90`

**Issues**:

- `INCLUDE 'include_increment.f90'` (lines 1176, 1183, 1189, 1195)
- Should use modules instead of file inclusion

## Minor Issues (Nice to Fix)

### 7. Already Addressed Dependencies
**Status**: Currently commented out (good!)

**Intel DFLIB Dependencies**:

- `src/parameters/sglobal.f90` (line 272)
- `!""USE DFLIB, ONLY:FULLPATHQQ, GETDRIVEDIRQQ, GETCHARQQ`
- Already properly commented out

### 8. Legacy I/O Patterns
**Impact**: Generally work but could be modernized

**Standard I/O Usage**:

- `REWIND`, `BACKSPACE` statements are standard but usage patterns could be improved
- Found in various modules (`src/modules/run_sim.f90`, `src/modules/ZQmod.f90`)

### 9. Obsolescent Features (Noted but Not Used)
**Status**: Comments indicate awareness

**ENTRY Statements**:

- `src/util/mod_load_filedata.f90` contains comments about ENTRY statements
- Actual ENTRY usage was removed (good practice)

## File-by-File Priority Assessment

### Critical Files (Require Immediate Attention)

1. `src/visualisation/visualisation_structure.f90` - **Cray pointers + DEC$ directives**
2. `src/visualisation/visualisation_pass.f90` - **DEC$ conditional compilation**
3. `src/util/getdirqq.f90` - **Windows dependencies**

### Important Files (Should Be Addressed)

4. `src/visualisation/visualisation_*.f90` - **DEC$ REAL:4 directives**
5. `src/parameters/sglobal.f90` - **PAUSE statements**
6. `src/visualisation/visualisation_metadata.f90` - **INCLUDE statements**

### Minor Files (Low Priority)

7. Various modules with legacy I/O patterns
8. Files with already-commented problematic dependencies

## Recommended Approach

### Phase 1: Critical Infrastructure (Visualization System)

1. **Redesign Cray pointer architecture** in `visualisation_structure.f90`

   - Replace Cray pointers with standard Fortran allocatable components
   - Implement proper linked list management using standard pointers
   - Remove `INT_PTR_KIND()` and `LOC()` dependencies

2. **Remove Intel compiler dependencies**

   - Replace `!DEC$` directives with portable alternatives
   - Use compiler-independent conditional compilation (CPP)
   - Remove Windows-specific GUI dependencies

### Phase 2: Language Modernization

1. **Replace SEQUENCE statements** with portable alternatives
2. **Remove PAUSE statements** with modern error handling
3. **Convert INCLUDE to module usage**

### Phase 3: Code Quality Improvements

1. **Modernize I/O patterns** where beneficial
2. **Standardize coding practices** across modules
3. **Add portable build configuration**

## Testing Strategy

### Compiler Compatibility Testing

- **GNU gfortran** (primary Linux target)
- **Intel ifx** (modern Intel compiler)
- **NAG Fortran** (strict standard compliance)

### Functional Testing

- Maintain mathematical equivalence of all calculations
- Preserve I/O compatibility with existing input/output files
- Ensure visualization functionality can be disabled for headless builds

## Risk Assessment

### High Risk

- Cray pointer replacement may affect numerical results if not implemented carefully
- Visualization system redesign could introduce subtle bugs

### Medium Risk  

- Conditional compilation changes may affect feature availability
- Memory layout changes could affect binary I/O compatibility

### Low Risk

- Most language modernization changes are mechanical
- Already-commented dependencies pose no risk

## Success Criteria

### Compilation Success

- Clean compilation with `gfortran -std=f2008`
- No non-standard extension warnings
- Successful linking with HDF5 libraries

### Functional Preservation

- All numerical results identical to within machine precision
- All input file formats preserved
- Core simulation functionality unaffected

### Portability Achievement

- Code compiles on multiple Linux distributions
- No Windows-specific dependencies in core simulation
- Optional visualization features clearly separated

---

**Document Version**: 1.0  
**Assessment Date**: August 11, 2025  
**Scope**: `src/` directory analysis  
**Next Review**: After Phase 1 completion
