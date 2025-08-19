# In-depth Report on the Refactoring of OCmod2.f90

## 1. Introduction

This document provides a detailed explanation of the refactoring applied to the `OCmod2.f90` module, with a specific focus on the subroutines responsible for calculating flow and their derivatives: `OCQGRD`, `OCQBNK`, and `OCQLNK`. The primary goal of this refactoring was to eliminate `GOTO` statements, improve code structure, and centralize physical calculations for better maintainability and stability.

## 2. General Refactoring Patterns

Before diving into specific subroutines, it's important to note two key patterns applied throughout the refactoring process.

### 2.1. Elimination of `GOTO` with Structured Flow Direction

A common pattern in the original code was using `GOTO` to handle bidirectional flow between two elements. This often resulted in complex, hard-to-follow code paths. For example, a calculation block might have looked like this:

```fortran
C     Original GOTO-based structure
      DZ = ZI(1) - ZI(0)
      IF (DZ .GT. 0.0D0) GOTO 100

C     Flow is from element 0 to 1
      H0 = ZI(0) - ZG(0)
      CONV0 = STR(0) * H0**1.6667
      ...
      GOTO 200

C     Flow is from element 1 to 0
100   CONTINUE
      HI = ZI(1) - ZG(1)
      CONVI = STR(1) * HI**1.6667
      ...

C     End of flow calculation
200   CONTINUE
```

The refactored code consistently replaces this with a clean, structured approach to determine the "high" (upstream) and "low" (downstream) elements first:

```fortran
! Determine flow direction based on water surface elevation difference (DZ)
DZ = ZI (1) - ZI (0)
SIG = SIGN (ONE, DZ)
! HI is 1 (upstream), LO is 0 (downstream) if flow is from 1 to 0
! HI is 0 (upstream), LO is 1 (downstream) if flow is from 0 to 1
HI = (1 + NINT (SIG) ) / 2
LO = 1 - HI
```

This snippet elegantly sets the indices for the upstream (`HI`) and downstream (`LO`) elements, allowing the rest of the subroutine to perform calculations without conditional branching on flow direction.

### 2.2. Centralization of Physical Calculations

Complex physical formulas that were previously duplicated across multiple subroutines have been extracted into dedicated utility functions. This is a significant improvement for maintainability and consistency. Before this refactoring, the calculations were performed inline within each flow-calculating subroutine (like `OCQGRD`, `OCQBNK`, and `OCQLNK`).

-   **`conveyan`**: This subroutine centralizes the calculation of conveyance and its derivative based on the Strickler-Manning equation. Previously, this logic was repeated. For instance, in the original `OCQGRD`, the conveyance was calculated directly:

    ```fortran
    ! Original inline conveyance calculation in OCQGRD
    HM = ZI (HI) - ZGI (HI)
    HM23 = zero
    IF (GTZERO(HM)) HM23 = HM**F23
    CONVM = STRW * HM23 * HM
    DERIVM = STRW * MAX (H23MIN, HM23) * F53
    ```
    This calculation is now handled by a single call: `CALL CONVEYAN(strw, hm, convm, derivm, 1)`. The new `conveyan` subroutine also includes improved handling for very low water depths to improve numerical stability, which was a critical part of the refactoring.

-   **`QWEIR`**: This subroutine encapsulates the logic for calculating flow over a horizontal-crested weir, correctly handling both drowned and undrowned flow conditions. This logic was previously embedded within subroutines like `OCQBNK`. The original code for weir flow was more complex and harder to maintain:

    ```fortran
    ! Original inline weir logic in OCQBNK
    IF (DZL .GT. SUBRIO*DZU) THEN
        ! Drowned weir
        Q(LO) = COEFF(1) * DZL * SQRT(ZU-ZL)
        ...
    ELSE
        ! Undrowned weir
        Q(LO) = COEFF(2) * DZU**1.5
        ...
    ENDIF
    ```
    This is now replaced by a clean, single call: `CALL QWEIR(ZI(HI), ZB, ZI(LO), COEFF, F23, Q(LO), DQ(LO,HI), rdum)`.

This centralization makes the code easier to read, debug, and maintain, as the core physics is no longer scattered across multiple locations.

## 3. Analysis of `OCQGRD` and `OCQBNK`

### 3.1. The Refactoring Anomaly: Code Duplication

A critical observation during the analysis is that the subroutines `OCQGRD` (flow between two land elements) and `OCQBNK` (flow at a channel bank) are **identical**.

The entire body of `OCQGRD` was copied into `OCQBNK`. While this successfully removed `GOTO` statements from `OCQBNK`, it introduced a significant code smell and a potential bug. The physics of flow between two overland grid cells may not be identical to the physics of flow between a channel and its bank. This duplication creates a maintenance burden, as any change to the logic must be manually applied in both places.

### 3.2. Deconstruction of the Logic

The logic within both `OCQGRD` and `OCQBNK` follows these steps:

1.  **Determine Flow Direction**: The `HI`/`LO` pattern described in section 2.1 is used.

2.  **Select Physical Law**: The code decides whether to model the flow using a resistance equation or a weir equation based on the relative elevations of the channel bank and the adjacent ground.

    ```fortran
    ! ZG is ground elevation, ZB is bank elevation
    IF (ZG.GE.ZB) THEN
        ! Channel bank is lower or equal to ground: use resistance equation
        ...
    ELSE
        ! Channel bank is higher than ground: use flat-crested weir equation
        ...
    ENDIF
    ```

3.  **Calculate Flow and Derivatives**:
    -   **Resistance Case**: The code calls the new `conveyan` subroutine to get the conveyance (`CONVM`) and its derivative (`DERIVM`). These values are then used in the standard resistance equation to calculate the flow (`Q`) and its derivatives with respect to the upstream and downstream heads (`DQ`).

        ```fortran
        ! Call the centralized conveyance calculation
        CALL CONVEYAN(strw, hm, convm, derivm, 1)
        ...
        ! Calculate flow and derivatives
        DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
        Q (LO) = CONVM * ROOTDZ / ROOTL
        DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
        DQ (LO, LO) = - DUM / ROOTL
        ```

    -   **Weir Case**: The code calls the `QWEIR` subroutine, passing it the appropriate coefficients and water levels.

        ```fortran
        ! Call the centralized weir calculation
        CALL QWEIR(ZI(HI), ZB, ZI(LO), COEFF, F23, Q(LO), DQ(LO,HI), rdum)
        DQ(LO,LO) = rdum
        ```

4.  **Ensure Conservation**: Finally, the flow and derivatives for the `HI` element are set as the negative of the `LO` element's values to ensure mass conservation.

    ```fortran
    Q (HI) = - Q (LO)
    DQ (HI, HI) = - DQ (LO, HI)
    DQ (HI, LO) = - DQ (LO, LO)
    ```

## 4. Analysis of `OCQLNK`

The `OCQLNK` subroutine, which calculates flow between two channel links, was refactored following a similar, but more sophisticated, pattern.

1.  **Data-Driven Logic**: Unlike `OCQGRD`, `OCQLNK` uses the `NTYPE` variable to select from multiple physical models. This makes the subroutine more flexible and extensible.

    ```fortran
    IF (NTYPE.EQ.7) THEN
        ! Internal weir
        CALL QWEIR(...)
    ELSEIF (NTYPE.EQ.12) THEN
        ! ZQ Table lookup (for reservoirs/structures)
        Q(LO) = get_ZQTable_value(...)
        ...
    ELSE
        ! Standard resistance-based flow
        ...
    ENDIF
    ```

2.  **Resistance Calculation**: The `ELSE` block for standard flow follows the same pattern as `OCQGRD`: it determines the `HI`/`LO` links, calls `OCCODE` (which in turn calls `conveyan`) to get the conveyance of the upstream link, and then calculates the flow and its derivatives.

## 5. Conclusion and Recommendations

The refactoring of these subroutines has successfully modernized the code by removing `GOTO` statements and introducing structured, centralized calculations. The use of helper functions like `conveyan` and `QWEIR` is a significant improvement.

However, the **identical nature of `OCQGRD` and `OCQBNK` is a major concern**. It is strongly recommended that this duplication be addressed. The logic within `OCQBNK` should be reviewed to ensure it correctly models the physics of channel-bank interaction, and it should be refactored into its own distinct implementation if necessary. If the logic is indeed identical, one subroutine should call the other, or the common logic should be extracted into a third, private subroutine.
