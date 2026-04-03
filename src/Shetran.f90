!> SHETRAN - Main Program
!>
!> Main program entry point for the SHETRAN hydrological modeling system.
!> This program coordinates the initialization, execution, and finalization
!> of all SHETRAN simulation components.
!>
!> ### Authors:
!> - **Stephen Birkinshaw**, Newcastle University (Original development)
!> - **Sven Berendsen**, Newcastle University (Modernization)
!> - **Newcastle University Water Group** (Ongoing development)
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
!> This program uses the following SHETRAN modules:
!>
!> - **ETmod**: Evapotranspiration processes
!> - **FRmod**: Framework for file operations and mass balance
!> - **Mnmod**: Nitrate transport module.
!> - **OCmod**: Overland channel flow calculations
!> - **SMmod**: Snow model calculations
!> - **SYmod**: Sediment yield and transport
!> - **RUN_SIM**: Main simulation execution controller
!> - **simulation_output**: Additional output utilities
!> - **GETDIRQQ**: Cross-platform directory utilities
!>
!> @note This is the main entry point for all SHETRAN simulations
!>
!> @warning Ensure all input files are properly formatted and accessible
!>
!> @history
!> | Version | Date | Author | Description |
!> |---------|------|--------|-------------|
!> | v4.4.6 | 2019-12-10 | SteveB | Added Hotstart ability |
!> | v4.4.7 | 2020-03-05 | SvenB | Code cleanups and modernization |
!> | v4.5.3 | 2026-03-19 | SteveB | Datum in time series, allocatable arrays and nitrate module |
!>
!-------------------------------------------------------------------------------
PROGRAM SHETRAN

   ! ============================================================================
   ! Module imports with explicit interface declarations
   ! ============================================================================

   ! Global constants and shared variables
   USE SGLOBAL

   ! Main data arrays and simulation parameters
   USE AL_D, ONLY: nstep  !< Current simulation time step number

   ! File data loading and preprocessing utilities
   USE mod_load_filedata, ONLY: ALTRAP  !< Initialize data loading system

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

   ! ============================================================================
   ! Local variables (none needed for main program)
   ! ============================================================================

   ! ============================================================================
   ! Main program execution
   ! ============================================================================

   ! Initialize error handling system
   ! Sets up error message handling with initial message buffer
   CALL ERROR(-999, 0, 0, 0, 0, 'Initialise error messages')

   ! Parse command line arguments and determine input files
   ! Processes command line to get rundata file and directory paths
   ! Note: Uses cross-platform implementation for Linux/Windows compatibility
   CALL GET_DIR_AND_CATCH(runfil, filnam, cnam, dirqq, rootdir)

   ! Initialize data loading and preprocessing system
   ! Sets up all data structures and performs initial data validation
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
   CALL extra_output()

   ! Record final visualization data
   ! Writes final state data for post-processing visualization
   ! Note: Converts time to single precision for visualization system
   CALL RECORD_VISUALISATION_DATA(REAL(uznow, KIND=4), 'end')

   ! ============================================================================
   ! Program completion
   ! ============================================================================

   ! The program now terminates cleanly

END PROGRAM SHETRAN
