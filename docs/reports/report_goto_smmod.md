# In-depth Report on the Refactoring of SMmod.f90

## 1. Introduction

This document provides a detailed analysis of the refactoring applied to the `SMmod.f90` module, which handles the snowmelt processes within the model. The primary objective of this work was to modernize the codebase by migrating it to a Fortran 90 module structure and systematically eliminating `GOTO` statements. These changes significantly improve the code's readability, structure, and maintainability.

## 2. General Refactoring Patterns

The refactoring of `SMmod.f90` followed two main principles: replacing outdated control flow with modern structures and encapsulating logic within a module.

### 2.1. Elimination of `GOTO` with Structured Logic

The original `SM.f` file was characterized by a complex web of `GOTO` statements that made the program flow difficult to trace. The refactoring replaced this "spaghetti code" with structured `IF-THEN-ELSE` blocks and `DO` loops.

A clear example is the logic for calculating the final snow depth (`SD`). The original code used multiple `GOTO` statements to handle different scenarios, such as whether the total snow loss was greater than the available snow depth.

```fortran
C     Original GOTO-based structure for updating snow depth
      TSM = USM + ESM
      IF (TSM.GT.SD (IEL) ) GOTO 24
      SD (IEL) = SD (IEL) - TSM
      GOTO 5
   24 IF (ESM.GT.SD (IEL) ) GOTO 25
      SD (IEL) = SD (IEL) - ESM
      USM = SD (IEL)
      GOTO 26
   25 ESM = SD (IEL)
      USM = zero
   26 SD (IEL) = zero
    5 CONTINUE
C     ... continue to meltwater routing
```

This confusing sequence of jumps was refactored into a clear, nested `IF-THEN-ELSE` structure that is much easier to understand and debug.

```fortran
! Refactored, structured logic for updating snow depth
      TSM = USM + ESM
      IF (TSM > SD (IEL)) THEN
          IF (ESM > SD(IEL)) THEN
              ESM = SD(IEL)
              USM = 0.0D0
              SD(IEL) = 0.0D0
          ELSE
              SD(IEL) = SD(IEL) - ESM
              USM = SD(IEL)
              SD(IEL) = 0.0D0
          ENDIF
      ELSE
          SD(IEL) = SD(IEL) - TSM
      ENDIF
! ... continue to meltwater routing
```

### 2.2. Code Encapsulation in a Fortran 90 Module

The original implementation consisted of separate subroutines that likely relied on shared state through `COMMON` blocks or other global data. The refactoring encapsulated all snow-related logic and data into a single, cohesive `MODULE SMmod`.

This provides several key advantages:
-   **Data Hiding**: The module explicitly controls which data and procedures are `PUBLIC` (accessible by other parts of the code) and which are `PRIVATE`. This prevents accidental modification of internal state variables.
-   **Clear Interfaces**: Subroutine arguments are clearly defined with `INTENT` attributes, making their purpose explicit.
-   **Improved Organization**: The code is logically grouped. The new module contains three main subroutines: `SMIN`, `SMET`, and `SM`, which clearly separate the different stages of the snow calculation process.

## 3. Analysis of Subroutines

The logic of the original file was broken down into three distinct, more manageable subroutines.

### 3.1. `SMIN`: The Control Subroutine

`SMIN` acts as the primary entry point and controller for the snowmelt module. Its main responsibility is to determine which calculation path to follow based on the current conditions.

-   **Before**: In the original code, this control logic was mixed with the main calculations, using `GOTO` statements to jump to the correct section based on temperature and the existence of a snowpack.
-   **After**: `SMIN` now contains a clean set of conditional checks. It first determines if evapotranspiration (ET) calculations have already run. Based on this and the air temperature, it decides whether to:
    1.  Call `SMET` to handle ET calculations in the presence of snow or freezing temperatures.
    2.  Call `SM` directly if ET has already been handled and a snowpack exists.
    3.  Bypass snow calculations entirely if conditions do not warrant them.

This separation of concerns makes the overall process flow much clearer.

### 3.2. `SMET`: Snow and Evapotranspiration

The `SMET` subroutine is responsible for handling the interaction between the snowpack and the vegetation canopy, specifically for evapotranspiration (ET) and interception calculations.

-   **Before**: This logic was part of a large, monolithic block of code. It used `GOTO`s to handle different cases, such as whether the snow depth was greater than the vegetation height.
-   **After**: `SMET` now cleanly handles these specific physical scenarios:
    -   If the snow covers the vegetation, it correctly sets ET and interception to zero and calls `SM` to calculate the snowmelt.
    -   If the snow is below the vegetation, it adjusts the leaf area index (`CPLAI`) and then determines the next step based on air temperature.
    -   If the temperature is below freezing, it assumes no ET or interception and calls `SM`.
    -   If the temperature is above freezing, it sets a flag (`NSMT = 1`) to indicate that the main `ET` routine should run before the final snowmelt calculation is performed by `SM`.

### 3.3. `SM`: The Core Snowmelt Calculation

`SM` is the heart of the module, performing the detailed energy budget or degree-day calculations to determine the amount of snowmelt.

-   **Before**: This subroutine was a long and complex sequence of calculations interwoven with `GOTO` statements for control flow. This made it extremely difficult to follow the logic for the energy balance, snow depth update, and meltwater routing.
-   **After**: The refactored `SM` subroutine is now a linear, step-by-step procedure:
    1.  It first calculates the net snowfall and adds it to the snowpack if the temperature is below freezing.
    2.  It chooses between the **degree-day** and **energy budget** methods based on the `MSM` flag. The energy budget calculation, though still complex, is now a single, self-contained block of code without `GOTO` interruptions.
    3.  It calculates the final snow depth using the structured `IF-THEN-ELSE` block shown in section 2.1.
    4.  Finally, it routes the calculated snowmelt and any rainfall through the snowpack. The original `GOTO` loop for this routing process has been replaced with a standard `DO` loop, which is more readable and robust.

## 4. Conclusion

The refactoring of `SMmod.f90` represents a significant step forward in modernizing the Shetran codebase. By eliminating `GOTO` statements and adopting a modular, structured design, the code is now substantially easier to read, understand, and maintain. This reduces the risk of introducing bugs during future development and makes the complex physics of the snowmelt process more transparent and verifiable.
