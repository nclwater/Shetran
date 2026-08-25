!> @brief Runs one SHETRAN simulation from command-line selection through final output.
!>
!> This is the sole program unit and the entry point of the `SHETRAN`
!> executable assembled by `CMakeLists.txt`. It owns no local data. Instead it
!> establishes the initial shared state, delegates setup and timestepping to
!> the model modules, requests final reports, and returns to the Fortran run
!> time. The hydrological process ordering itself belongs to
!> [[run_sim:SIMULATION]].
!>
!> @author Stephen Birkinshaw, Newcastle University
!> @author Sven Berendsen, Newcastle University
!>
!> ### Command-line selection
!>
!> [[getdirqq:get_dir_and_catch]] validates the selected rundata file and sets
!> the shared filenames before any model file is opened:
!>
!> | Invocation | Current behavior |
!> |:-----------|:-----------------|
!> | `shetran -f <path>` | Uses the named rundata file directly. This is the normal GFortran invocation. |
!> | `shetran -c [name]` | Reads alternating name/path records from `catchments.txt`; an omitted name selects `default`. |
!> | no args or `-a` | Opens a dialog only with Intel QuickWin on Windows; other builds stop with usage text. |
!> | trailing `-error` | Sets shared `error_mode`; no current routine reads that flag, so termination behavior is unchanged. |
!>
!> `catchments.txt` is resolved relative to the launch working directory. A
!> successful selection sets `FILNAM` to the validated path, `DIRQQ` to its
!> directory (or `.`), `CNAM` to its final filename stem with an exact lowercase
!> `rundata_` prefix removed, and `rootdir` to the launch working directory.
!> The historical `RUNFIL` argument is passed but is not read by the current
!> selector.
!>
!> @warning
!> The current non-QuickWin behavior differs from the user manual: a bare
!> filename is not accepted without `-f`, and no-argument GFortran execution
!> does not open a dialog. The manual also describes `-error` as suppressing an
!> interactive wait, but current fatal termination is already noninteractive
!> and independent of that flag.
!> @endwarning
!>
!> ### Execution sequence
!>
!> | Step | Operation | Current effect |
!> |:-----|:----------|:---------------|
!> | 1 | `ERROR(-999,...)` | Initializes the shared error flags and retained help-path state. |
!> | 2 | `GET_DIR_AND_CATCH` | Resolves and validates the rundata path and catchment identity. |
!> | 3 | `ALTRAP` | Calls the retained floating-point-trap hook; the current implementation enables no traps. |
!> | 4 | `NSTEP=0`; `UZNOW=ZERO` | Initializes the timestep count and elapsed simulation time in hours. |
!> | 5 | `FROPEN` | Opens the rundata-controlled inputs and outputs and reads their filename records. |
!> | 6 | `SIMULATION` | Initializes the model, advances timesteps, and writes scheduled outputs. |
!> | 7 | `FROUTPUT('end  ')` | Writes final phreatic-surface and pressure-head data for a future VSI file. |
!> | 8 | `EXTRA_OUTPUT` | Writes the error-count summary, completion record, and final water-balance totals. |
!> | 9 | `RECORD_VISUALISATION_DATA(...,'end')` | Records any due final state and closes visualisation/HDF5 resources. |
!>
!> The final visualisation time is converted from double precision to
!> default-real kind. Remaining connected Fortran units and process-lifetime
!> allocations are left to normal program termination; this entry point does
!> not explicitly close or deallocate the complete model state.
!>
!> Command-line failures stop before [[frmod:FROPEN]]. Fatal setup or simulation
!> errors may stop in their owning routines, so the final three calls are made
!> only after [[run_sim:SIMULATION]] returns normally.
!>
!> @note
!> [[mod_load_filedata:ALTRAP]] is retained for interface compatibility. Its
!> platform-specific IEEE handler has been disabled since version 4g-pc; it
!> currently forces a local status to zero and returns.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2019-12-10 | SB | 4.4.6 | Added hotstart capability to the orchestrated simulation. |
!> | 2020-03-05 | SvB | - | Cleaned and modernized the legacy entry source. |
!> | 2020-04-22 | SB | - | Added the current `src/Shetran.f90` entry file during repository reorganization. |
!> | 2026-03-28 | SvB | - | Added the first FORD program header and explanatory call-site comments. |
!> | 2026-05-11 | SB | - | Restored the `ALTRAP` import for the retained call during the current-code rebase. |
!> | 2026-05-13 | SvB | - | Removed the final Intel-specific `sleepqq` call from the portable entry point. |
!> | 2026-06-19 | SB | 4.6.4 | Updated cross-platform command-line selection, including conditional QuickWin support. |
!> | 2024-06-05 | SB | v4.7.0 |  remove inital call to error subroutine |
!> @endhistory
PROGRAM SHETRAN

   ! Shared filenames, time, constants, and error service.
   USE SGLOBAL

   ! Shared timestep number.
   USE AL_D, ONLY: nstep

   ! Retained no-op floating-point-trap hook.
   USE mod_load_filedata, ONLY : ALTRAP

   ! Cross-platform command-line and directory handling.
   USE GETDIRQQ, ONLY: GET_DIR_AND_CATCH

   ! Rundata-controlled file setup and framework output.
   USE FRmod, ONLY: FROPEN, &
      FROUTPUT

   ! Visualisation recording and final cleanup.
   USE VISUALISATION_INTERFACE_RIGHT, ONLY: RECORD_VISUALISATION_DATA

   ! Completion and water-balance reporting.
   USE REST, ONLY: extra_output

   ! Model initialization and timestep driver.
   USE RUN_SIM, ONLY: SIMULATION

   ! Explicit release of the persistent open-channel solver workspace.
   USE OCmod, ONLY: FINALISE_OCSIM_WORKSPACE

   ! Cross-platform millisecond sleep used before automatic console closure.
   USE stdlib_system, ONLY: sleep

   IMPLICIT NONE

   ! Local variables (none needed for main program)

   ! Main program execution

   ! Parse command line arguments and determine input files
   ! Processes command line to get rundata file and directory paths
   CALL GET_DIR_AND_CATCH(runfil, filnam, cnam, dirqq, rootdir)

   ! Retain the legacy startup call; the current hook enables no traps.
   CALL ALTRAP

   ! Initialize the timestep count and elapsed simulation time [h].
   nstep = 0
   uznow = zero

   ! Open the rundata-controlled model inputs and outputs.
   CALL FROPEN

   ! Initialize the model and run its timestep loop.
   CALL SIMULATION

   ! Write final state suitable for a later VSI initial-condition file.
   CALL FROUTPUT('end  ')

   ! Write completion, error-count, and water-balance summaries.
   CALL extra_output()

   ! Record any due final visualisation data and close its resources.
   CALL RECORD_VISUALISATION_DATA(REAL(uznow, KIND=4), 'end')

   ! Release the model-lifetime open-channel solver workspace.
   CALL FINALISE_OCSIM_WORKSPACE()

   ! Program completion
   ! added a delay to allow users to see the final output before the console window closes
   if (casemode == '-a') CALL sleep(5000)

END PROGRAM SHETRAN
