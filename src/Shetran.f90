!> summary: SHETRAN main program.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University; Newcastle University Water Group
!>
!> Main program entry point for the SHETRAN hydrological modelling system.
!> This program coordinates command-line/file selection, input opening,
!> initialization, simulation execution, final output, and visualisation output.
!>
!> ### Program Responsibilities:
!> - Parse command line arguments and configuration
!> - Initialize all model components and data structures
!> - Execute the main simulation time-stepping loop
!> - Generate final outputs and cleanup resources
!>
!> ### Program Flow:
!> 1. **Initialization**: Parse command line, read configuration
!> 2. **Setup**: Initialize all model components and data structures
!> 3. **Simulation**: Execute main time-stepping loop
!> 4. **Finalization**: Write final outputs and cleanup
!>
!> ### Related Components:
!> The executable coordinates the SHETRAN components described in the user
!> manual:
!>
!> - **FRmod**: frame setup, input/output control, indexing, bank setup, and mass balance.
!> - **ETmod**: evapotranspiration, interception, and vegetation controls.
!> - **OCmod/OCmod2**: overland and channel flow routing.
!> - **VSmod**: variably saturated subsurface flow.
!> - **SMmod**: optional snowmelt calculations.
!> - **SYmod**: optional sediment erosion and transport.
!> - **CMmod**: optional contaminant transport; requires bank elements when enabled.
!> - **MNmod**: optional mineral nitrogen/nitrate calculations called through the contaminant component.
!> - **ZQmod**: optional reservoir stage-discharge table support.
!> - **RUN_SIM**: main simulation time-step driver.
!> - **REST**: meteorological input, timestep control, extra output, and water-balance utilities.
!> - **GETDIRQQ**: cross-platform command-line and directory handling.
!>
!> @note This is the main entry point for all SHETRAN simulations
!> @endnote
!>
!> @warning Ensure all input files are properly formatted and accessible
!> @endwarning
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
   USE RUN_SIM, ONLY: SIMULATION  !< Execute main simulation loop   IMPLICIT NONE

   IMPLICIT NONE
    
   ! Parse command line arguments and determine input files
   ! Processes command line to get rundata file and directory paths
   ! Note: Uses cross-platform implementation for Linux/Windows compatibility
   CALL GET_DIR_AND_CATCH(runfil, filnam, cnam, dirqq, rootdir)
    
   ! Testing trap of floating point exceptions
   ! Is it still necessary? Default is _off_
   CALL ALTRAP

   ! Initialize simulation state variables
   ! Set initial conditions for time stepping and model state
   nstep = 0        !< Initialize step counter
   uznow = zero     !< Initialize time variable

   ! Open all input and output data files
   ! Opens data files based on configuration and validates formats
   ! Note: File handles are managed by the framework system
   CALL FROPEN

   ! Execute main simulation time-stepping loop
   ! This is the core computational engine that advances the simulation
   ! through time, solving the governing equations at each time step
   ! See RUN_SIM module for detailed simulation algorithm
   CALL SIMULATION

   ! Generate final framework output and close files
   ! Writes final mass balance, summary statistics, and closes file handles
   CALL FROUTPUT('end  ')

   ! Generate additional output files
   ! Creates supplementary output files for specialized analysis
   CALL EXTRA_OUTPUT()

   ! Record final visualization data
   ! Writes final state data for post-processing visualization
   ! Note: Converts time to single precision for visualization system
   CALL RECORD_VISUALISATION_DATA(REAL(uznow, KIND=4), 'end')

   ! ============================================================================
   ! Program completion
   ! ============================================================================

   ! The program now terminates cleanly
   CALL sleepqq(5000) 

END PROGRAM SHETRAN
