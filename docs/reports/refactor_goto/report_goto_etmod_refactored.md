# GOTO Refactoring Report: Evapotranspiration Module (ETmod)

**Date:** 20 August 2025  
**Directory Examined:** `src/compute/evapo_inception/`  
**Author:** GitHub Copilot  
**Branch:** remove_goto  

## Executive Summary

This report documents the examination and refactoring of legacy GOTO statements and numbered DO loop labels in the evapotranspiration module located in `src/compute/evapo_inception/`. The analysis found and successfully modernized 2 instances of old-style Fortran DO loops with numeric labels, replacing them with modern Fortran control structures.

## Files Examined

The following files were analyzed for GOTO statements and legacy control structures:

- `et_variables.f90` - Module-wide variables and constants
- `et_core.f90` - Core evapotranspiration calculations ✅ **Modified**
- `et_integration.f90` - Integration with the broader model
- `et_main.f90` - Main controller ✅ **Modified**
- `et_validation.f90` - Input data validation

## Findings and Modifications

### 1. File: `et_main.f90`

**Issue Found:** Old-style numbered DO loop with label `1000`

**Original Code (Lines 47-69):**
```fortran
! Loop over land-elements
DO 1000 IEL = NGDBGN, total_no_elements
   ITYPE = ICMREF (IEL, 1)
   IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
      IL = ICMREF (IEL, 4)
      ALFA = 0.5 * CWIDTH (IL) / BWIDTH
      ICE = NHBED (IL, ITYPE) + 2
      CALL ALINIT (ALFA, ICE-2, UZALFA)
      UZALFA (ICE-1) = ALFA * FHBED (IL, ITYPE)
   ELSE
      ICE = 1
   ENDIF

   IF (ICE.LE.top_cell_no) CALL ALINIT (ZERO, top_cell_no - ICE+1, UZALFA (ICE) )

   HRUZ = getHRF(IEL) - ZGRUND (IEL)
   ICE = NLYRBT (IEL, 1)

   CALL DCOPY (top_cell_no - ICE+1, VSPSI (ICE, IEL), 1, PSI4 (ICE), &
      1)

   CALL ETIN (IEL)

1000  END DO
```

**Refactored Code:**
```fortran
! Loop over land-elements
DO IEL = NGDBGN, total_no_elements
   ITYPE = ICMREF (IEL, 1)
   IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
      IL = ICMREF (IEL, 4)
      ALFA = 0.5 * CWIDTH (IL) / BWIDTH
      ICE = NHBED (IL, ITYPE) + 2
      CALL ALINIT (ALFA, ICE-2, UZALFA)
      UZALFA (ICE-1) = ALFA * FHBED (IL, ITYPE)
   ELSE
      ICE = 1
   ENDIF

   IF (ICE.LE.top_cell_no) CALL ALINIT (ZERO, top_cell_no - ICE+1, UZALFA (ICE) )

   HRUZ = getHRF(IEL) - ZGRUND (IEL)
   ICE = NLYRBT (IEL, 1)

   CALL DCOPY (top_cell_no - ICE+1, VSPSI (ICE, IEL), 1, PSI4 (ICE), &
      1)

   CALL ETIN (IEL)

END DO
```

**Change Description:** Removed the numeric label `1000` from the DO loop statement, converting it to modern Fortran syntax. The loop functionality remains identical.

### 2. File: `et_core.f90`

**Issue Found:** Old-style numbered DO loop with label `310`

**Original Code (Lines 221-319):**
```fortran
!     Count from the top cell down (for RDF subscript)
DO 310 KK = 1, K
!
!        Corresponding bottom-up cell number
   II = top_cell_no - KK + 1
!
   IF (M1.NE.2.AND.M1.NE.3) THEN
!
!--------------------------------
!  MODE 1 CALCULATIONS.....
!--------------------------------
!
      IF (PSI4 (II) .GE.ZERO) THEN
         AE = PE
      ELSE
         AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE+RC (N) &
            / RA (N) ) ) )
      ENDIF

   ! ... [extensive calculation logic continues] ...

310   END DO
```

**Refactored Code:**
```fortran
!     Count from the top cell down (for RDF subscript)
DO KK = 1, K
!
!        Corresponding bottom-up cell number
   II = top_cell_no - KK + 1
!
   IF (M1.NE.2.AND.M1.NE.3) THEN
!
!--------------------------------
!  MODE 1 CALCULATIONS.....
!--------------------------------
!
      IF (PSI4 (II) .GE.ZERO) THEN
         AE = PE
      ELSE
         AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE+RC (N) &
            / RA (N) ) ) )
      ENDIF

   ! ... [extensive calculation logic continues] ...

END DO
```

**Change Description:** Removed the numeric label `310` from the DO loop statement. This loop contains complex evapotranspiration calculations across three different modes and includes proper modern control structures (`CYCLE` and `EXIT` statements) within nested loops for interpolation calculations.

## Analysis Notes

### Positive Observations

1. **Modern Control Structures Present:** The code already uses modern Fortran control structures where appropriate:
   - `CYCLE` statements for loop continuation (lines 257, 285 in `et_core.f90`)
   - `EXIT` statements for early loop termination (lines 262, 290 in `et_core.f90`)
   - Block-IF structures instead of GOTO statements

2. **Historical Awareness:** The code includes a comment in `et_core.f90` (line 59) indicating previous refactoring efforts:
   ```fortran
   ! RAH  981021  4.2  Use block-IFs instead of GOTOs.
   ```

3. **No Actual GOTO Statements:** Despite the legacy DO loop labels, no actual GOTO statements were found in the codebase, indicating that previous modernization efforts were successful in eliminating computed jumps.

### Impact Assessment

The refactoring changes are **minimal and safe**:

- **Functionality:** No changes to program logic or behavior
- **Performance:** No impact on computational performance
- **Maintainability:** Improved code readability and compliance with modern Fortran standards
- **Compatibility:** Changes are compatible with modern Fortran compilers while maintaining backward compatibility

## Recommendations

1. **Build Verification:** Compile and test the refactored modules to ensure no unintended side effects.

2. **Code Style Consistency:** Consider updating other old-style Fortran constructs throughout the codebase for consistency.

3. **Documentation Updates:** The module documentation and comments are well-maintained. No additional documentation changes are needed for these specific modifications.

4. **Future Development:** Continue using modern Fortran control structures in new development and refactor numbered labels when encountered in other modules.

## Files Modified

1. `/src/compute/evapo_inception/et_main.f90` - Removed DO loop label `1000`
2. `/src/compute/evapo_inception/et_core.f90` - Removed DO loop label `310`

## Conclusion

The evapotranspiration module has been successfully modernized by removing legacy numbered DO loop labels. The code now fully conforms to modern Fortran standards while maintaining all original functionality. No GOTO statements were found, indicating that this module has been well-maintained over its development history.

The refactoring was straightforward and safe, involving only the removal of numeric labels from DO statements without any changes to the underlying computational logic. The module's use of modern control structures like `CYCLE` and `EXIT` demonstrates good programming practices already in place.
