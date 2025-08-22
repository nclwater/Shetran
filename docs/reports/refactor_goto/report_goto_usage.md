# GOTO Usage Report

This report details the usage of GOTO statements within the Shetran codebase.

## `src/util/getdirqq_winIntel.f90`

- ~~**Line 137: `IF(message/='') GOTO 1000`**~~
  - **Purpose:** Error handling. If an error message has been set during command-line argument parsing, it jumps to the error handling block at label `1000`.

- ~~**Line 146: `GOTO 1000`**~~
  - **Purpose:** Error handling. If the specified rundata file does not exist, it jumps to the error handling block at label `1000`.

## `src/visualisation/visualisation_interface_left.f90`

- ~~**Line 253: `GOTO 1000`**~~
  - **Purpose:** Error handling. In `get_ncon_early`, if the marker `:CM3` is not found in the contaminant data file, it jumps to the error handling block at label `1000`.

- ~~**Line 271: `GOTO 1000`**~~
  - **Purpose:** Error handling. In `get_nsed_early`, if the marker `:SY11` is not found in the sediment data file, it jumps to the error handling block at label `1000`.

## `src/modules/utilsmod.f90`

This module contains several `GOTO` statements, some of which are part of complex control flows.

### `SUBROUTINE FINPUT`

- ~~**Line 118: `GOTO 1000`**~~
  - **Purpose:** Early exit. If the input time is beyond the current simulation step end (`INTIME.GE.SIMEND`), it copies the next data point into the output array and jumps directly to the `RETURN` statement at label `1000`. This avoids further processing when the data for the current step is already known.

- ~~**Line 139: `GOTO 20`**~~
  - **Purpose:** Loop for data aggregation. This statement is inside a conditional block. It jumps back to the `READ` statement at label `20`, creating a loop. This loop reads and accumulates time-series data until the read time (`INTIME`) surpasses the simulation step end time (`SIMEND`).

### `SUBROUTINE HINPUT`

The control flow in this subroutine is confusing due to what appears to be a partial refactoring of older `GOTO`-based logic into a `DO` loop.

- ~~**Line 219: `GOTO 223`**~~
  - **Purpose:** Unconditional jump. This `GOTO` jumps over the error-handling line (`9999 INTIME = marker999`) to a `CONTINUE` statement. The subsequent `IF` statement then determines whether to `EXIT` the surrounding `DO` loop. This structure is a convoluted way to manage the loop and its error condition.

- **Commented-out GOTOs:**
  - The file also contains several commented-out `GOTO` statements (e.g., lines 715, 725, 726, 731, 861, 869), indicating an ongoing effort to refactor and remove them.

## `src/modules/OCmod.f90`

This module also contains a mix of active and commented-out `GOTO` statements.

### `SUBROUTINE OCABC`

- **Line 253: `iscycle = .TRUE. !GOTO 20`**
  - **Purpose:** Loop control. This line is inside a `DO` loop. The original code likely used `GOTO 20` to jump to the end of the loop iteration. The refactored code uses a boolean flag `iscycle` and a `CYCLE` statement, but the `GOTO` remains in a comment.

### `SUBROUTINE OCCHK2`

- **Line 779: `IF (NONEED) GOTO 110`**
  - **Purpose:** Skip unnecessary checks. If a boundary condition file is not needed (`NOCHB.EQ.0` or `NOCFB.EQ.0`), this `GOTO` skips the `INQUIRE` block for that file.

- **Line 790: `GOTO 110`**
  - **Purpose:** Conditional jump. Similar to the one in `OCCHK0`, this skips the error handling block if the file unit is valid.

### `SUBROUTINE OCLTL`

- **Line 1051: `IF (K.NE.I) THEN !GOTO 100`**
  - **Purpose:** Error handling. The original code likely used `GOTO 100` to jump to an error message and stop execution if an incorrect coordinate was found. The refactored code sets a flag and uses `CYCLE` to exit the loop, but the `GOTO` is still present in a comment.

- **Line 1064: `iscycle=.TRUE. !GOTO 70`**
  - **Purpose:** Loop control. Similar to the one in `OCABC`, this was likely used to exit an inner loop. The refactored code uses a boolean flag and a `CYCLE` statement.

### `SUBROUTINE OCPLF`

- **Line 1382: `IF ((NDEFCT.GT.NOCTAB).OR.(NDEFCT.LT.0)) THEN !GOTO 8054`**
  - **Purpose:** Error handling. The original code used a `GOTO` to jump to an error handling block if the number of default cross-section categories was out of range. This has been replaced with a structured `IF` block.
- **Line 1408: `READ (OCD, *, ERR = 8300, END = 8300) I, ZG, WDEPTH, STR, IDEFX`**
  - **Purpose:** Error and end-of-file handling. The `ERR=` and `END=` specifiers jump to label `8300` on a read error or end of file. This has been refactored to use modern error handling.
- **Line 1413: `IF (I.NE.ielm) THEN`**
  - **Purpose:** Error handling. The original code used a boolean flag and `CYCLE` to emulate a `GOTO` jump if the element number was incorrect. This has been replaced with a direct `EXIT` from the loop.
- **Line 1423: `IF (TEST) THEN`**
  - **Purpose:** Error handling. The original code used a boolean flag and `CYCLE` to emulate a `GOTO` jump. This has been replaced with a direct `EXIT` from the loop.

### `SUBROUTINE OCREAD`

- **Line 1700: `IF ((NCATR.GT.NOCTAB).OR.(NCATR.LT.0)) GOTO 8047`**
  - **Purpose:** Error handling. Jumps to an error block if the number of roughness categories is out of range. This has been replaced with a structured `IF` block.

## `src/modules/SMmod.f90`

### `SUBROUTINE SM`

- **GOTO statements:**
  - The subroutine contained several `GOTO` statements (e.g., `GOTO 30`, `GOTO 100`, `GOTO 200`, `GOTO 300`, `GOTO 400`, `GOTO 500`) that were part of the original logic for handling snowmelt, evaporation, and routing meltwater through the snowpack. These have been removed and the logic replaced with modern `IF-THEN-ELSE` blocks and `DO` loops with `EXIT` statements.

### `SUBROUTINE SMET`

- **GOTO statements:**
  - The subroutine contained `GOTO` statements (e.g., `GOTO 100`, `GOTO 200`, `GOTO 300`) to manage control flow based on whether the snowpack covered vegetation and whether the temperature was above or below freezing. These have been replaced with structured `IF-THEN-ELSE` blocks.

### `SUBROUTINE SMIN`

- **GOTO statements:**
  - The subroutine used `GOTO` statements (e.g., `GOTO 100`, `GOTO 200`, `GOTO 300`) to decide whether to call the snowmelt (`SM`) or snow evapotranspiration (`SMET`) routines based on the state of the snowpack and previous calculations. This logic has been refactored into a clearer, nested `IF-THEN-ELSE` structure.

## `src/modules/OCmod2.f90`

### `SUBROUTINE OCFIX`

- **Commented-out GOTOs:**
  - The subroutine contained several commented-out `GOTO` statements (e.g., `GOTO 400`, `GOTO 300`, `GOTO 200`, `GOTO 900`) that were part of the original loop control and error handling logic. These have been removed and the logic replaced with modern `DO` loops with `CYCLE` and `EXIT` statements.

## `src/parameters/sglobal.f90`

- ~~**Line 432: `GOTO 5`**~~
  - **Purpose:** File reading loop. This statement creates a loop to read and print lines from a help file. It jumps back to the `READ` statement at label `5`. The loop is expected to terminate when an end-of-file or error condition occurs, which transfers control to the statement at label `7`.

## `src/modules/ETmod.f90`

This module contains `GOTO` statements for interpolation and control flow.

### `SUBROUTINE ET`

- ~~**Line 432: `IF (PSI4 (II) .GT.PS1 (N, KL) ) GOTO 170`**~~
  - **Purpose:** Loop control for interpolation. This is inside a `DO` loop that searches for the correct interval for linear interpolation. If the current value `PSI4(II)` is greater than the grid point `PS1(N, KL)`, it skips to the next iteration using `GOTO 170`, which is the label for the `END DO` statement.

- ~~**Line 437: `GOTO 200`**~~
  - **Purpose:** Exit interpolation loop. After a successful interpolation, this jumps out of the `DO` loop to the `CONTINUE` statement at label `200`.

### `SUBROUTINE ETIN`

- ~~**Line 586: `IF (NSMT.EQ.0.AND. (BEXSM) ) GOTO 10`**~~
  - **Purpose:** Conditional execution. If snowmelt calculations are not required (`NSMT.EQ.0`), this `GOTO` skips the call to the `ET` subroutine and jumps directly to the `CONTINUE` statement at label `10`.

## `src/modules/rest.f90`

This module is a collection of routines and contains a large number of `GOTO` statements, reflecting an older style of Fortran programming. They are used for control flow, error handling, and looping, often in complex ways.

### `SUBROUTINE BALWAT`

- ~~**Line 178: `IF (FIRST_balwat) GOTO 400`**~~
  - **Purpose:** Initialization. On the first call to this subroutine, it skips the main balance calculation and jumps to the end of the element loop (label `400`). This is used to initialize the `WBERR` array to zero without performing a balance check.

### `SUBROUTINE METIN`

This subroutine is responsible for reading meteorological data and is particularly dense with `GOTO` statements. They direct the flow based on the data format (`BMETAL`), handle hot starts (`BHOTRD`), and manage the reading of different data files (`PRD`, `EPD`, `MED`, etc.).

- ~~**Line 308: `IF (.NOT.BMETAL) GOTO 40`**~~: Skips the breakpoint file reading section if not applicable.
- ~~**Line 323, 370, 390, 399, 430, 450, 459, 538, 573, 616, 649, 678`**~~: These `GOTO` statements jump from a `READ` statement to the corresponding processing block (e.g., from `READ (PRD, ...)` to label `282`). This is a common pattern to handle the `END=` specifier for end-of-file detection.
- ~~**Line 358, 413, 597, 700`**~~: These create loops for hot-start runs, repeatedly reading data until the simulation's start time (`BHOTTI`) is reached.
- ~~**Line 486: `GOTO 12`**~~: Creates a loop to read potential evapotranspiration data until the current computational timestep is filled.
- ~~**Line 511, 599`**~~: These jump to the end of the subroutine (label `190`) after a major processing block is complete.
- ~~**Line 519: `IF (NRAIN.NE.NM) GOTO 100`**~~: Directs control flow to a different section if the number of rainfall and meteorological stations are unequal.
- ~~**Line 566, 643`**~~: These skip reading measured potential evaporation for a station if it's not required.

## `src/modules/OCmod2.f90`

### `SUBROUTINE OCFIX`

- **Commented-out GOTOs:**
  - The subroutine contained several commented-out `GOTO` statements (e.g., `GOTO 400`, `GOTO 300`, `GOTO 200`, `GOTO 900`) that were part of the original loop control and error handling logic. These have been removed and the logic replaced with modern `DO` loops with `CYCLE` and `EXIT` statements.

