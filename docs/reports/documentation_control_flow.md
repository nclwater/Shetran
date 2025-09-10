# SHETRAN — Program Control Flow Documentation

This document summarises the overall program control flow of SHETRAN as executed by the refactored codebase and the decisions recorded in project reports. It is a high-level, implementation-oriented map intended to help developers navigate execution, testing, and refactoring work.

## 1. High-level overview
- Program entry: main driver reads configuration and example/run control files, initialises global state, then enters the simulation control loop.
- Phases:
  1. Startup / initialization
  2. Preprocessing and mesh/grid setup
  3. Time-stepping main loop (meteorology → surface → subsurface → sediment/contaminant → output)
  4. Finalisation and cleanup

References: design & phase overview in [`plan_refactoring.md`](docs/reports/plan_refactoring.md) and modernization rationale in [`FORTRAN_MODERNIZATION_REPORT.md`](docs/reports/modernisation/FORTRAN_MODERNIZATION_REPORT.md).

## 2. Initialization sequence
- Read command-line / rundata: command-line handling and file lookup (see I/O consolidation plan: [`plan_io_refactor.md`](docs/reports/refactor_io/plan_io_refactor.md)).
- Load parameter sets, file-units and global arrays (parameter refactor guidance in [`plan_refactoring.md`](docs/reports/plan_refactoring.md)).
- Construct mesh, allocate arrays, and run consistency checks (reported in SYmod refactor: [`refactor_SYmod.md`](docs/reports/refactor_reorg/refactor_SYmod.md)).

## 3. Main simulation control loop
- Advance simulation time in discrete timesteps (outer time loop).
- Per timestep sequence:
  1. Update boundary / meteorological inputs
  2. Overland flow processing (routing, infiltration)
  3. Subsurface flow solver(s)
  4. Sediment transport & erosion modules
  5. Contaminant transport modules
  6. Write outputs (time-slice & diagnostics)
- Each component is implemented as modular subroutines with clear interfaces to facilitate unit testing and replacement (testing plan: [`plan_testing.md`](docs/reports/testing/plan_testing.md)).

## 4. Restart and error-control patterns
- Legacy GOTO-based restarts were replaced by structured DO WHILE / EXIT loops and logical flags; complex restart mechanisms are encapsulated in explicit retry loops (see GOTO modernization reports: [`report_goto_vsmod_refactored.md`](docs/reports/refactor_goto/report_goto_vsmod_refactored.md) and [`report_goto_symod.md`](docs/reports/refactor_goto/report_goto_symod.md)).
- Error handling uses centralized error/reporting calls with early returns rather than global jump labels (documented in refactor reports).

## 5. I/O and output control
- I/O is being consolidated into `src/io/` per the I/O plan; all file open/close and unit management are standardised to use consistent error checking and IOSTAT handling ([`plan_io_refactor.md`](docs/reports/refactor_io/plan_io_refactor.md)).
- FORD documentation templates standardize module headers and make control points discoverable ([`plan_FORD_docs.md`](docs/reports/refactor_docs/plan_FORD_docs.md)).

## 6. Testing and validation hooks
- Unit tests should mirror src/ structure (tests/unit/*) and integration tests should use example datasets to validate end-to-end behavior (testing plan: [`plan_testing.md`](docs/reports/testing/plan_testing.md)).
- Control-flow sensitive areas (state machines, restart loops, connectivity setup) are explicitly listed in the GOTO reports and must be covered by unit + integration tests.

## 7. Developer guidance / next steps
- Use the documented module interfaces and centralized I/O to add targeted unit tests.
- When modifying control flow, consult the GOTO modernization reports for equivalent structured patterns: [`report_goto_vsmod_refactored.md`](docs/reports/refactor_goto/report_goto_vsmod_refactored.md), [`report_goto_symod.md`](docs/reports/refactor_goto/report_goto_symod.md).
- Validate changes against existing example runs and the functional differences analysis: [`functional_differences.md`](docs/reports/modernisation/functional_differences.md).

## References
- Refactoring plan: [`plan_refactoring.md`](docs/reports/plan_refactoring.md)  
- Testing plan: [`plan_testing.md`](docs/reports/testing/plan_testing.md)  
- I/O consolidation: [`plan_io_refactor.md`](docs/reports/refactor_io/plan_io_refactor.md)  
- FORTRAN modernization: [`FORTRAN_MODERNIZATION_REPORT.md`](docs/reports/modernisation/FORTRAN_MODERNIZATION_REPORT.md)  
- GOTO modernization: [`report_goto_vsmod_refactored.md`](docs/reports/refactor_goto/report_goto_vsmod_refactored.md), [`report_goto_symod.md`](docs/reports/refactor_goto/report_goto_symod.md)

## Appendix A — Precise initialization locations and order (extracted from current Fortran sources)

The ordered list below was reconstructed by scanning src/ and follows the call/initialization responsibilities present in the current Fortran files. For each step: routine / call target — file path — one-line purpose.

1. program Shetran — src/Shetran.f90  
   - Main program entry; parses command-line and hands control to the simulation driver.

2. subroutine simulation — src/simulation/run_sim.f90  
   - Top-level driver that orchestrates startup, component initialization and then the main time-stepping loop.

3. get_dir_and_catch (variants) — src/util/getdirqq.f90, src/util/getdirqq_portable.f90, src/util/getdirqq_winIntel.f90  
   - Resolve runfile path, working directory and catchment root; initial file/location setup.

4. mod_load_filedata — src/util/mod_load_filedata.f90  
   - Load runfile-driven lists and auxiliary file metadata used by subsequent initialization.

5. INPL (component input parsing) — src/simulation/framework/framework_component_initialization.f90 (SUBROUTINE INPL)  
   - Read and distribute component-specific input blocks into framework structures.

6. FRINIT (framework initialization) — src/simulation/framework/framework_initialization.f90 (SUBROUTINE FRINIT)  
   - Initialize global framework state and allocate core framework arrays.

7. INFR (framework shared helpers) — src/simulation/framework/framework_shared.f90 (SUBROUTINE INFR)  
   - Initialize shared framework data referenced by multiple components.

8. FRIND (spatial setup / indexing) — src/simulation/framework/framework_spatial_setup.f90 (SUBROUTINE FRIND)  
   - Build spatial indices, element ordering and mappings for solvers and I/O.

9. OCINI / initialise_ocmod — src/simulation/overland_channel/oc_initialization.f90 (SUBROUTINE OCINI) and src/compute/OCmod2.f90 (SUBROUTINE initialise_ocmod)  
   - Initialize overland/channel state, routing matrices and module parameters.

10. VSIN / initialise_vsmod — src/compute/subsurface_flow/subsurface_initialization.f90 (SUBROUTINE VSIN) and src/compute/subsurface_flow/subsurface_variables.f90 (SUBROUTINE initialise_vsmod)  
    - Initialize subsurface arrays, soil-layer structures and solver defaults.

11. initialise_smmod — src/compute/snow/snow_initialization.f90 (SUBROUTINE initialise_smmod)  
    - Initialize snow-melt module state and arrays.

12. Sediment initialization helpers — src/compute/sediment/sediment_initialization.f90 (e.g., workspace handling subroutines)  
    - Prepare sediment module workspace and validate resources for sediment routines.

13. Contaminant data reader — src/compute/contaminant/contaminant_data_reader.f90 (CMRD)  
    - Read contaminant-specific input datasets required by contaminant initialization.

14. Visualization initialization — src/visualisation/visualisation_hdf5.f90 (SUBROUTINE initialise) and related visualisation modules  
    - Prepare HDF5/visualisation output structures and metadata.

15. Framework output manager / output registration — src/simulation/framework/framework_output_manager.f90 (SUBROUTINE FRRESC) and src/io/simulation_output.f90 (SUBROUTINE extra_output)  
    - Register/open output channels and configure diagnostics.

16. Time-step controller preparation — src/simulation/timestep_control.f90 (SUBROUTINE TMSTEP) and time-control setup in run_sim.f90  
    - Initialize time-stepping parameters and counters prior to entering main loop.

17. Return to simulation driver and enter main time-stepping loop — src/simulation/run_sim.f90  
    - Initialized modules are invoked per timestep according to the main control flow.

### Notes
- This ordering is the initialization sequence reconstructed from the source tree. The canonical root call sequence is in src/Shetran.f90 → src/simulation/run_sim.f90; exact runtime order depends on the CALL sequence implemented there and any conditional branches.
- Some initialization occurs implicitly via module-level initializers and variables defined in src/parameters/*; those are not always explicit CALLs but affect runtime startup.
- If you want literal call-site line numbers and the exact CALL ordering, I can parse src/Shetran.f90 and src/simulation/run_sim.f90 and expand each step with the precise call-site lines.

## Appendix B — Main simulation loop: detailed sequence and parallelisation assessment

Summary of per-timestep sequence (implementation locations):

- Time-step controller / driver
  - TMSTEP / run_sim driver — src/simulation/timestep_control.f90 (TMSTEP) and src/simulation/run_sim.f90  
  - Orchestrates timestep increment, convergence checks and per-component calls.

- Meteorological / boundary updates
  - METIN — src/io/meteorological_input.f90  
  - Reads/applies boundary conditions and meteorological forcings for the current timestep.

- Overland / channel processing
  - OCSIM / OC* routines — src/simulation/overland_channel/oc_time_stepping.f90, oc_hydraulic_calculations.f90, OCmod2.f90, etc.  
  - Computes overland routing, infiltration and channel routing. Contains both local element computations and global routing solves (connectivity / matrix assembly).

- Subsurface flow
  - VSSIM / VSCOLM / VSIN / initialise_vsmod — src/compute/subsurface_flow/subsurface_simulation.f90, subsurface_column_solver.f90, subsurface_initialization.f90, subsurface_variables.f90  
  - Solves per-column subsurface flow (column solvers) and any cross-column coupling (if present).

- Sediment processes
  - SYMAIN / SYCOLM / sediment modules — src/compute/sediment/*.f90  
  - Erosion, transport capacity and bed-change calculations; may include element-local and link-coupled operations.

- Contaminant transport
  - CM_SIMULATE_TIMESTEP / COLM / LINKSM / PLCOLM — src/compute/contaminant/*.f90  
  - Column and link solvers for contaminants, plant uptake subroutines — mostly per-column/link computations.

- Mass-balance & diagnostics
  - FRMB / framework mass-balance and checks — src/simulation/framework/framework_mass_balance.f90

- Output (time-slice & diagnostics)
  - extra_output / FRRESC / visualisation initialise — src/io/simulation_output.f90, src/simulation/framework/framework_output_manager.f90, src/visualisation/visualisation_hdf5.f90  
  - Collects state and writes outputs (HDF5/other formats).

Parallelisation assessment and recommendations
- Best near-term gains (low refactor cost / high benefit)
  1. Column-level solvers (subsurface VSCOLM, contaminant COLM, sediment SYCOLM)
     - These routines operate per vertical column / element and are good targets for shared-memory parallelism (OpenMP) by looping over columns. Minimal data coupling between columns in many implementations.
  2. Per-element local computations in overland and sediment routines
     - Local flux calculations, source/sink updates and property evaluations can be parallelised across elements/links where no immediate global dependency exists.
  3. I/O overlap / asynchronous output and parallel HDF5
     - Move heavy output to a separate thread/process or use parallel HDF5 to avoid blocking timestepping.

- Higher-effort / high-reward
  1. Domain decomposition (MPI) for very large catchments
     - Partition mesh and run distributed-memory solves. Requires refactor of global arrays, halo exchange and routing solver interfaces.
  2. Parallel routing / global solves
     - Channel/overland routing often uses global matrices or ordered sweeps; refactor to iterative solvers or partitioned solves for parallelism.

- Cautions and prerequisites
  - Module-level globals and implicit shared state: audit and remove hidden shared mutable state before threading. Convert globals to explicit state passed into routines where practical.
  - Synchronisation points: routing and any global aggregation (mass-balance, diagnostics) require barriers / reductions.
  - Numerical reproducibility: parallel reductions and asynchronous I/O can change rounding / ordering — add regression tests.
  - I/O concurrency: ensure the chosen file format and HDF5 build support parallel writes; otherwise use a dedicated I/O rank or buffered output.

Suggested incremental approach
1. Add OpenMP pragmas to iterate over independent element/column loops (VSCOLM, COLM, SYCOLM, many overland local loops). Start with read-only data + independent writes to per-thread buffers.
2. Introduce an asynchronous output thread / producer-consumer queue to decouple heavy writes.
3. After stabilizing OpenMP speedups and correctness, evaluate MPI domain decomposition for runs that exceed single-node memory or compute.

References (implementation pointers)
- Files with per-timestep entry points: src/simulation/run_sim.f90, src/simulation/timestep_control.f90
- Overland: src/simulation/overland_channel/*.f90, src/compute/OCmod2.f90
- Subsurface: src/compute/subsurface_flow/*.f90 (VSCOLM, VSIN, initialise_vsmod)
- Sediment: src/compute/sediment/*.f90 (SYMAIN, SYCOLM, others)
- Contaminant: src/compute/contaminant/*.f90 (CM_SIMULATE_TIMESTEP, COLM, LINKSM)

## Appendix C — Persistent OpenMP region and avoiding per-iteration setup costs

Short checklist
- Create one persistent parallel region that encloses the main timestep loop to avoid repeated thread fork/join.
- Pre-allocate per-thread scratch buffers once (indexed by omp_get_thread_num) and reuse them every timestep.
- Use worksharing ( !$omp do ) inside the parallel region for column/element loops; prefer schedule(static).
- Use !$omp single or !$omp master for serial I/O and global reductions; avoid ending the parallel region for these.
- Avoid allocations, deallocations or large firstprivate copies inside the timestep loop.
- Use nowait on independent worksharing regions when safe to reduce implicit barriers.
- Test numerical reproducibility (reductions can change ordering).

Minimal Fortran pattern (persistent region + per-thread scratch)
```fortran
program shetran_parallel
  use omp_lib
  integer :: i, ncols, tid, nthreads
  integer, parameter :: scratch_size = 1000
  real, allocatable :: scratch(:,:)

  ncols = ...  ! number of column solves

  ! allocate per-thread scratch once
  nthreads = max(1, omp_get_max_threads())
  allocate(scratch(nthreads, scratch_size))

  !$omp parallel default(shared) private(i, tid)
    tid = omp_get_thread_num() + 1

    ! Threads persist for the whole run; timestepping inside
    do while (time < time_end)

      !$omp do schedule(static)
      do i = 1, ncols
        call solve_column(i, scratch(tid, :))
      end do
      !$omp end do

      !$omp single
        call write_outputs_if_needed()
      !$omp end single

    end do
  !$omp end parallel

contains
  subroutine solve_column(col, local_scratch)
    integer, intent(in) :: col
    real, intent(inout) :: local_scratch(:)
    ! column solve using preallocated local_scratch (no allocs)
  end subroutine
end program
```

Notes
- Start small: parallelize independent column/element loops first, then address synchronization and I/O.
- Audit module-level globals and convert hidden shared state to explicit per-thread or per-call state before threading.