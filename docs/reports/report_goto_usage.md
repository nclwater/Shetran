# GOTO Usage Report

This report details the usage of GOTO statements within the Shetran codebase.

## `src/modules/getdirqq_winIntel.f90`

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

- **Line 118: `GOTO 1000`**
  - **Purpose:** Early exit. If the input time is beyond the current simulation step end (`INTIME.GE.SIMEND`), it copies the next data point into the output array and jumps directly to the `RETURN` statement at label `1000`. This avoids further processing when the data for the current step is already known.

- **Line 139: `GOTO 20`**
  - **Purpose:** Loop for data aggregation. This statement is inside a conditional block. It jumps back to the `READ` statement at label `20`, creating a loop. This loop reads and accumulates time-series data until the read time (`INTIME`) surpasses the simulation step end time (`SIMEND`).

### `SUBROUTINE HINPUT`

The control flow in this subroutine is confusing due to what appears to be a partial refactoring of older `GOTO`-based logic into a `DO` loop.

- **Line 219: `GOTO 223`**
  - **Purpose:** Unconditional jump. This `GOTO` jumps over the error-handling line (`9999 INTIME = marker999`) to a `CONTINUE` statement. The subsequent `IF` statement then determines whether to `EXIT` the surrounding `DO` loop. This structure is a convoluted way to manage the loop and its error condition.

- **Commented-out GOTOs:**
  - The file also contains several commented-out `GOTO` statements (e.g., lines 715, 725, 726, 731, 861, 869), indicating an ongoing effort to refactor and remove them.

## `src/modules/OCmod.f90`

This module also contains a mix of active and commented-out `GOTO` statements.

### `SUBROUTINE OCABC`

- **Line 253: `iscycle = .TRUE. !GOTO 20`**
  - **Purpose:** Loop control. This line is inside a `DO` loop. The original code likely used `GOTO 20` to jump to the end of the loop iteration. The refactored code uses a boolean flag `iscycle` and a `CYCLE` statement, but the `GOTO` remains in a comment.

### `SUBROUTINE OCCHK0`

- **Line 617: `GOTO 110`**
  - **Purpose:** Conditional jump. If a file unit is correctly opened and formatted, this `GOTO` skips the error handling block for that unit and proceeds to check the next one.

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

## `src/parameters/sglobal.f90`

- **Line 432: `GOTO 5`**
  - **Purpose:** File reading loop. This statement creates a loop to read and print lines from a help file. It jumps back to the `READ` statement at label `5`. The loop is expected to terminate when an end-of-file or error condition occurs, which transfers control to the statement at label `7`.

## `src/modules/ETmod.f90`

This module contains `GOTO` statements for interpolation and control flow.

### `SUBROUTINE ET`

- **Line 432: `IF (PSI4 (II) .GT.PS1 (N, KL) ) GOTO 170`**
  - **Purpose:** Loop control for interpolation. This is inside a `DO` loop that searches for the correct interval for linear interpolation. If the current value `PSI4(II)` is greater than the grid point `PS1(N, KL)`, it skips to the next iteration using `GOTO 170`, which is the label for the `END DO` statement.

- **Line 437: `GOTO 200`**
  - **Purpose:** Exit interpolation loop. After a successful interpolation, this jumps out of the `DO` loop to the `CONTINUE` statement at label `200`.

- **Line 461: `IF (PSI4 (II) .GT.PS1 (N, KL) ) GOTO 240`**
  - **Purpose:** Loop control for interpolation. Similar to the one at line 432, this is used to find the correct interval for interpolation in a different mode (`M1.EQ.3`).

- **Line 466: `GOTO 270`**
  - **Purpose:** Exit interpolation loop. Similar to the one at line 437, this jumps out of the interpolation loop to the `CONTINUE` statement at label `270`.

### `SUBROUTINE ETIN`

- **Line 586: `IF (NSMT.EQ.0.AND. (BEXSM) ) GOTO 10`**
  - **Purpose:** Conditional execution. If snowmelt calculations are not required (`NSMT.EQ.0`), this `GOTO` skips the call to the `ET` subroutine and jumps directly to the `CONTINUE` statement at label `10`.

## `src/modules/rest.f90`

This module is a collection of routines and contains a large number of `GOTO` statements, reflecting an older style of Fortran programming. They are used for control flow, error handling, and looping, often in complex ways.

### `SUBROUTINE BALWAT`

- **Line 178: `IF (FIRST_balwat) GOTO 400`**
  - **Purpose:** Initialization. On the first call to this subroutine, it skips the main balance calculation and jumps to the end of the element loop (label `400`). This is used to initialize the `WBERR` array to zero without performing a balance check.

### `SUBROUTINE METIN`

This subroutine is responsible for reading meteorological data and is particularly dense with `GOTO` statements. They direct the flow based on the data format (`BMETAL`), handle hot starts (`BHOTRD`), and manage the reading of different data files (`PRD`, `EPD`, `MED`, etc.).

- **Line 308: `IF (.NOT.BMETAL) GOTO 40`**: Skips the breakpoint file reading section if not applicable.
- **Line 323, 370, 390, 399, 430, 450, 459, 538, 573, 616, 649, 678**: These `GOTO` statements jump from a `READ` statement to the corresponding processing block (e.g., from `READ (PRD, ...)` to label `282`). This is a common pattern to handle the `END=` specifier for end-of-file detection.
- **Line 358, 413, 597, 700**: These create loops for hot-start runs, repeatedly reading data until the simulation's start time (`BHOTTI`) is reached.
- **Line 486: `GOTO 12`**: Creates a loop to read potential evapotranspiration data until the current computational timestep is filled.
- **Line 511, 599**: These jump to the end of the subroutine (label `190`) after a major processing block is complete.
- **Line 519: `IF (NRAIN.NE.NM) GOTO 100`**: Directs control flow to a different section if the number of rainfall and meteorological stations are unequal.
- **Line 566, 643**: These skip reading measured potential evaporation for a station if it's not required.

### `SUBROUTINE TMSTEP`

This subroutine computes the next timestep and also contains several `GOTO`s, some of which have been refactored into logical flags but remain as comments.

- **Line 790: `iscycle = .TRUE. ! GOTO 8`**: A refactored `GOTO`. The comment indicates the original logic was to jump to the end of a `DO` loop (label `8`).
- **Line 823: `jumpto45=.TRUE. !GOTO 45`**: A refactored `GOTO`. The original logic would jump to the error handling section at label `45` if the timestep became invalid.
- **Line 835, 855**: Commented-out `GOTO`s that were part of the original complex loop structure for reading precipitation data and adjusting the timestep.

## `src/visualisation/visualisation_read.f90`

This module handles reading data for visualization and contains numerous `GOTO` statements, primarily for error handling and low-level character parsing.

### `SUBROUTINE r_ii` (Read an integer)

- **Line 66: `IF(.NOT.ANY(c==di)) GOTO 95`**
  - **Purpose:** Error handling. If a character read is not a valid digit, jumps to the error handler at label `95`.

- **Line 68: `IF(i>szb) GOTO 95`**
  - **Purpose:** Error handling. If the number of digits exceeds the buffer size, jumps to the error handler.

- **Line 75: `GOTO 100`**
  - **Purpose:** Error handling. After formatting an error message for a read error (`ERR=90`), jumps to the main error routine at label `100`.

- **Line 77: `GOTO 100`**
  - **Purpose:** Error handling. After formatting an error message for an invalid character, jumps to the main error routine.

### `SUBROUTINE find_first_character`

- **Line 99: `GOTO 100`**
  - **Purpose:** Error handling. Jumps to the main error routine if a read error occurs.

### `SUBROUTINE r_rr` (Read a real)

- **Line 118: `IF(.NOT.ANY(c==dr)) GOTO 95`**
  - **Purpose:** Error handling. If a character is not valid for a real number (digit, '.', '-'), jumps to the error handler at `95`.

- **Line 120: `IF(i>szb) GOTO 95`**
  - **Purpose:** Error handling. Jumps to error handler if the buffer is full.

- **Line 127: `GOTO 100`**
  - **Purpose:** Error handling. Jumps to the main error routine after a read error.

- **Line 128: `GOTO 100`**
  - **Purpose:** Error handling. Jumps to the main error routine after an invalid character error.

### `SUBROUTINE strip`

- **Line 238: `IF(dum/=checktitle) GOTO 900`**
  - **Purpose:** Error handling. If the file title doesn't match the expected title, jumps to the error handler at `900`.

## `src/modules/OCmod2.f90`

This module contains refactored `GOTO` statements, primarily within the `OCFIX` subroutine. The original `GOTO`s have been replaced by `CYCLE` statements with labels, but the `GOTO`s themselves remain in comments.

### `SUBROUTINE OCFIX`

- **Line 1038: `IF (.NOT.TEST) CYCLE out300 !GOTO 300`**
  - **Purpose:** Loop control. The original code used `GOTO 300` to skip to the next iteration of the `IFACE` loop. The refactored code uses `CYCLE out300`.

- **Line 1059: `IF (PEL.LT.1) CYCLE out200 !GOTO 200`**
  - **Purpose:** Loop control. The original code used `GOTO 200` to skip to the next iteration of the `PPP` loop. The refactored code uses `CYCLE out200`.

- **Line 1139: `!IF (AOK) EXIT out900 !GOTO 901`**
  - **Purpose:** Early exit. This commented-out line shows that the original logic was to jump out of the main `passs` loop (labeled `out900`) if the `AOK` flag was true. The modern equivalent would be an `EXIT` statement.

## `src/modules/SMmod.f90`

This module, for snowmelt (`SM`) calculations, is another example of classic Fortran style, with `GOTO` statements managing the main logic flow.

### `SUBROUTINE SM`

This subroutine contains the core snowmelt logic and is heavily reliant on `GOTO`.

- **Control Flow based on `MSM` (Melt Model)**:
  - **Line 168: `IF (MSM.EQ.2) GOTO 10`**: If the energy budget model is selected, it jumps to the corresponding code block at label `10`.
  - **Line 180: `goto 27`**: After the degree-day calculation, it jumps to the evaporation loss calculation.

- **Energy Budget Logic (`MSM.EQ.2`)**:
  - **Line 196: `IF (TA (MS) .GT.TS (IEL) ) GOTO 20`**: Jumps to a different Richardson number correction based on air vs. snow temperature.
  - **Line 198: `GOTO 21`**: Skips the alternative correction.
  - **Line 265: `IF (LTZERO(TS2)) GOTO 22`**: If the hypothetical new snow temperature is below zero, it jumps to the "no melting" block.
  - **Line 272: `GOTO 23`**: After calculating melt, jumps to set the final snow temperature.

- **Snow Depth and Loss Calculations**:
  - **Line 289: `IF (TSM.GT.SD (IEL) ) GOTO 24`**: If total snow loss exceeds snow depth, jumps to a block that handles the complete melting of the snowpack.
  - **Line 291: `GOTO 5`**: After adjusting snow depth, jumps to the start of the meltwater routing section.
  - **Line 292: `IF (ESM.GT.SD (IEL) ) GOTO 25`**: If evaporation loss is greater than the remaining snow depth, jumps to handle that specific case.
  - **Line 295: `GOTO 26`**: Jumps to the final snow depth adjustment.

- **Meltwater Routing**:
  - **Line 308: `IF (NSMC (IEL) .LE.max_no_snowmelt_slugs) GOTO 34`**: Checks if there is space for a new meltwater slug; if not, it stops.
  - **Line 317: `IF (LEZERO(TA(MS))) GOTO 32`**: If the air temperature is below freezing, it jumps to a block that treats precipitation as snow.
  - **Line 320: `GOTO 33`**: Skips the snow block.
  - **Line 323: `IF (GTZERO(SMELT(NNC,IEL))) GOTO 35`**: If there is melt, jumps to calculate the time it will take to reach the bottom of the pack.
  - **Line 326: `GOTO 31`**: If there's no new melt, jumps to check the existing slugs.
  - **Line 335: `IF (TIMEUZ.LT.tmelt(KL, IEL) ) GOTO 11`**: In a loop, if a slug has not yet reached the bottom, it skips to the next slug.
  - **Line 344, 348, 354: `GOTO 3`**: After processing slugs, jumps to the final conversion step.
  - **Line 357: `IF (NSMC (IEL) .LE.0) GOTO 3`**: If no slugs are left, jumps to the end.
  - **Line 359: `GOTO 14`**: Jumps back to the start of the slug processing loop.

### `SUBROUTINE SMET`

- **Line 408: `IF (ISZERO(SNDEP)) GOTO 309`**: If there is no snow depth, jumps to the "no snow" logic.
- **Line 409: `IF (SNDEP.LT.VHT (N) ) GOTO 302`**: If snow depth is less than vegetation height, jumps to a different calculation block.
- **Line 432: `IF (GTZERO(TA(MS))) GOTO 308`**: If temperature is above freezing, jumps to the block that sets up for ET calculations.
- **Line 458, 459: `GOTO 306`**: If it's snowing or a snowpack exists, jumps to call the main `SM` routine.

### `SUBROUTINE SMIN`

- **Line 510: `IF (NSMT.EQ.1) GOTO 11`**: If ET calculations have already been done, jumps to the snowmelt block.
- **Line 514: `IF (GTZERO(SD(IEL))) GOTO 10`**: If a snowpack exists, jumps to call the snow ET routine.
- **Line 518: `IF (LEZERO(TA(MS))) GOTO 10`**: If the temperature is below freezing, jumps to call the snow ET routine.

