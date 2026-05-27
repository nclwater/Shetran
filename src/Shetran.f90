!> summary: SHETRAN main program.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University; Newcastle University Water Group
!>
!> Main executable entry point for the SHETRAN hydrological modelling system.
!>
!> `SHETRAN` resolves the run-data file and catchment paths, enables the optional
!> floating-point trap, initializes the run clock, opens model files, runs the
!> simulation driver, writes final framework and extra outputs, records the final
!> visualisation state, and then pauses briefly before exit.
!>
!> Command-line selection is delegated to [[get_dir_and_catch]]:
!>
!> | Mode | Effect |
!> |:-----|:-------|
!> | no argument or `-a` | Open a file-selection dialog for a run-data file. |
!> | `-f <file>` | Use the supplied run-data filename directly. |
!> | `-c` | Look up `default` in `catchments.txt`; named lookup is documented in [[get_dir_and_catch]]. |
!> | trailing `-error` | Do not wait for Enter after fatal errors. |
!>
!> Execution sequence:
!>
!> | Step | Call | Purpose |
!> |:-----|:-----|:--------|
!> | 1 | `GET_DIR_AND_CATCH` | Resolve run-data filename, catchment name, input directory, and root directory. |
!> | 2 | `ALTRAP` | Enable the project's floating-point exception trap if configured. |
!> | 3 | `FROPEN` | Open/read run-data-controlled input and output files. |
!> | 4 | `SIMULATION` | Run the main timestep driver. |
!> | 5 | `FROUTPUT`, `EXTRA_OUTPUT`, `RECORD_VISUALISATION_DATA` | Write final reports and visualisation data. |
!>
!> Required input files are those named by the selected run-data file.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2019-12-10 | SteveB | 4.4.6 | Added hotstart capability. |
!> | 2020-03-05 | SvenB | 4.4.7 | Code cleanups and modernization. |
!> | 2026-03-19 | SteveB | 4.5.3 | Added datum in time series, allocatable arrays, and nitrate module. |
!> @endhistory
PROGRAM SHETRAN

   ! ============================================================================
   ! Module imports with explicit interface declarations
   ! ============================================================================

   ! Global constants and shared variables
   USE SGLOBAL

   ! Main data arrays and simulation parameters
   USE AL_D, ONLY: nstep  !< Current simulation time step number
   
   ! Testing trap of floating point exceptions
   ! Is it still necessary? Default is _off_
   USE mod_load_filedata, ONLY : ALTRAP


   ! Cross-platform command line and directory utilities
   USE GETDIRQQ, ONLY: GET_DIR_AND_CATCH  !< Parse command line arguments

   ! Framework for file operations and mass balance
   USE FRmod, ONLY: FROPEN, &  !< Open all data files
      FROUTPUT       !< Generate framework output

   ! Visualization data recording interface
   USE VISUALISATION_INTERFACE_RIGHT, ONLY: RECORD_VISUALISATION_DATA  !< Record viz data

   ! Additional simulation output utilities
   USE REST, ONLY: extra_output  !< Generate extra output files

   ! Main simulation execution controller
   USE RUN_SIM, ONLY: SIMULATION  !< Execute main simulation loop.

   IMPLICIT NONE
    
   ! Resolve run-data file, catchment name, input directory, and root directory.
   CALL GET_DIR_AND_CATCH(runfil, filnam, cnam, dirqq, rootdir)
    
   ! Testing trap of floating point exceptions
   ! Is it still necessary? Default is _off_
   CALL ALTRAP

   ! Initialize the timestep counter and model clock before file setup.
   nstep = 0        !< Initialize step counter
   uznow = zero     !< Initialize time variable

   ! Open input and output files specified through the run-data file.
   CALL FROPEN

   ! Execute the main timestep loop.
   CALL SIMULATION

   ! Write final framework output and close framework-managed files.
   CALL FROUTPUT('end  ')

   ! Write supplementary output files.
   CALL EXTRA_OUTPUT()

   ! Record final state for post-processing visualisation.
   CALL RECORD_VISUALISATION_DATA(REAL(uznow, KIND=4), 'end')

   ! ============================================================================
   ! Program completion
   ! ============================================================================

   ! Keep the console visible briefly for interactive Windows runs.
   CALL sleepqq(5000) 

END PROGRAM SHETRAN
