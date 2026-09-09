#!/usr/bin/env python3
"""Generate the SHETRAN source-reorganisation proposal CSVs.

Reads the inventories produced by `inventory_units.py` and the usage map from
`analyse_usage.py`, applies the target layout defined below, and writes:

  docs/rename/functions.csv  - one row per procedure, with its documentation
                               block, current location and proposed target
  docs/rename/variables.csv  - one row per module-level variable, likewise
  docs/rename/types.csv      - one row per derived-type definition, likewise;
                               a type moves as a block, so it carries a line
                               range rather than a single line

The reference columns come from `analyse_usage.py` and are split in two:
`referenced_by` lists files that demonstrably import the name (an ONLY list
names it), `possibly_referenced_by` lists files that could only reach it
through a whole-module USE, where the token may equally be a local.

Out of scope, per the request: src/visualisation, src/resource, src/Shetran.f90.
"""

import csv
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "docs" / "rename"

# ---------------------------------------------------------------------------
# Target layout: new module name -> (target file, one-line purpose)
# ---------------------------------------------------------------------------
TARGETS = {
    # --- core: genuinely cross-cutting only -------------------------------
    "mod_parameters":       ("src/core/mod_parameters.f90", "Numeric kinds, buffer lengths, sentinels, mathematical and physical constants."),
    "array_limits":         ("src/core/array_limits.f90", "Compile-time capacity bounds shared by more than one component."),
    "build_info":           ("src/core/build_info.f90", "Version number, development flag and banner text."),
    "file_units":           ("src/core/file_units.f90", "Central table of every Fortran unit number; keeps uniqueness checkable in one place."),
    "run_context":          ("src/core/run_context.f90", "Selected rundata paths, catchment name and command-line run options."),
    "simulation_clock":     ("src/core/simulation_clock.f90", "Simulation time, timestep number and the coupled ET/VSS step lengths."),
    "runtime_flags":        ("src/core/runtime_flags.f90", "Timestep-reduction requests written by error reporting and read by timestep control."),
    "grid_topology":        ("src/core/grid_topology.f90", "Active grid extent, element/coordinate lookup and face connectivity."),
    "element_geometry":     ("src/core/element_geometry.f90", "Element counts, plan dimensions, areas, ground elevation and processing order."),
    "legacy_retained":      ("src/core/legacy_retained.f90", "Retained state documented as having no current producer or consumer."),

    # --- util: domain-independent helpers ---------------------------------
    "error_reporting":      ("src/util/error/error_reporting.f90", "Error/warning reporting, severity levels, counters and termination."),
    "error_status":         ("src/util/error/error_status.f90", "IOSTAT/STAT wrappers for open, close, read, write, rewind and (de)allocate."),
    "datetime":             ("src/util/datetime.f90", "Calendar-date to simulation-hour conversion and leap-year helpers."),
    "interpolation":        ("src/util/interpolation.f90", "Table and time-series interpolation."),
    "linear_algebra":       ("src/util/linear_algebra.f90", "Matrix and vector primitives: copy, multiply, tridiagonal solve, LU inversion."),
    "random_numbers":       ("src/util/random_numbers.f90", "Uniform pseudo-random number generation."),
    "float_compare":        ("src/util/float_compare.f90", "Tolerance-based comparisons against zero, one and the end-of-input marker."),
    "platform_traps":       ("src/util/platform_traps.f90", "Retained floating-point-trap hook."),

    # --- io: SHETRAN input-file format layer ------------------------------
    "record_readers":       ("src/io/record_readers.f90", "Heading-and-value record readers for the component data files."),
    "spatial_fields":       ("src/io/spatial_fields.f90", "Reads and spreads per-element, per-layer and per-bank spatial fields."),
    "input_validation":     ("src/io/input_validation.f90", "Range and consistency checks applied to freshly read input."),
    "grid_arrays":          ("src/io/grid_arrays.f90", "Reads whole integer and real arrays laid out on the model grid."),
    "input_workspace":      ("src/io/input_workspace.f90", "Shared integer and real scratch buffers used while reading spatial input."),
    "timeseries_input":     ("src/io/timeseries_input.f90", "Reads the next dated value from a forcing or boundary time series."),

    # --- meteorology ------------------------------------------------------
    "met_input":            ("src/meteorology/met_input.f90", "Reads combined and separate meteorological forcing records."),
    "met_forcing":          ("src/meteorology/met_forcing.f90", "Current meteorological forcing values, station mapping and input intervals."),

    # --- driver -----------------------------------------------------------
    "command_line":         ("src/driver/command_line.f90", "Command-line parsing and rundata-file selection."),
    "simulation_driver":    ("src/driver/simulation_driver.f90", "Model initialisation and the coupled timestep loop."),
    "timestep_control":     ("src/driver/timestep_control.f90", "Chooses the next coupled timestep from forcing and error feedback."),
    "water_balance":        ("src/driver/water_balance.f90", "Catchment water-balance accumulation and reporting schedule."),
    "run_summary":          ("src/driver/run_summary.f90", "End-of-run error counts, completion record and balance totals."),

    # --- frame ------------------------------------------------------------
    "frame_setup":          ("src/frame/frame_setup.f90", "Opens the rundata-controlled files and drives whole-model initialisation."),
    "frame_geometry":       ("src/frame/frame_geometry.f90", "Builds element, link and bank indexing, areas and face lengths."),
    "bank_setup":           ("src/overland_channel/bank_setup.f90", "Reads the optional explicit-bank element data and bankfull geometry."),
    "frame_output":         ("src/frame/frame_output.f90", "Scheduled text and CSV output, plus the final-state file."),
    "mass_balance_report":  ("src/frame/mass_balance_report.f90", "Periodic mass-balance reporting."),
    "legacy_result_files":  ("src/frame/legacy_result_files.f90", "Legacy unformatted result-set metadata and writers."),
    "run_control":          ("src/frame/run_control.f90", "Run title, component-enable switches, hotstart settings and print flags."),

    # --- evapotranspiration ----------------------------------------------
    "et_process":           ("src/evapotranspiration/et_process.f90", "Penman-Monteith evaporation, interception and transpiration."),
    "et_input":             ("src/evapotranspiration/et_input.f90", "Reads ET parameters and the time-varying vegetation tables."),
    "et_state":             ("src/evapotranspiration/et_state.f90", "Canopy, vegetation and evaporation state shared with other components."),

    # --- snow -------------------------------------------------------------
    "snowmelt":             ("src/snow/snowmelt.f90", "Degree-day and energy-budget snowmelt and meltwater routing."),
    "snow_input":           ("src/snow/snow_input.f90", "Reads the snowmelt data file."),
    "snow_state":           ("src/snow/snow_state.f90", "Snowpack depth, temperature, density and meltwater-slug state."),

    # --- overland and channel flow ---------------------------------------
    "oc_driver":            ("src/overland_channel/oc_driver.f90", "Overland/channel setup and the implicit row solver."),
    "oc_input":             ("src/overland_channel/oc_input.f90", "Reads the overland/channel data file and its boundary records."),
    "oc_indexing":          ("src/overland_channel/oc_indexing.f90", "Row-solver ordering, row widths and link-number lookup."),
    "oc_validation":        ("src/overland_channel/oc_validation.f90", "Consistency checks on overland/channel input and geometry."),
    "oc_boundaries":        ("src/overland_channel/oc_boundaries.f90", "Time-varying stage and flow boundary conditions."),
    "oc_node_solver":       ("src/overland_channel/oc_node_solver.f90", "Per-node water-level solution and its accessors."),
    "oc_discharge":         ("src/overland_channel/oc_discharge.f90", "Face discharge for grid, link, bank, confluence and weir flow."),
    "oc_stage_discharge":   ("src/overland_channel/oc_stage_discharge.f90", "Stage-discharge relations and their derivatives."),
    "oc_cross_sections":    ("src/overland_channel/oc_cross_sections.f90", "Channel width-depth cross-section and conveyance tables."),
    "zq_tables":            ("src/overland_channel/zq_tables.f90", "Reservoir and weir stage-discharge lookup tables."),
    "channel_geometry":     ("src/overland_channel/channel_geometry.f90", "Channel link length, width, bed elevation and bank-element mapping."),
    "oc_state":             ("src/overland_channel/oc_state.f90", "Water levels, face discharges, roughness and flow derivatives."),

    # --- variably saturated subsurface -----------------------------------
    "vs_driver":            ("src/subsurface/vs_driver.f90", "Iterates the coupled subsurface columns and checks their mass balance."),
    "vs_column_solver":     ("src/subsurface/vs_column_solver.f90", "Assembles and solves the tridiagonal pressure-head correction."),
    "vs_sources":           ("src/subsurface/vs_sources.f90", "Well, spring, boundary and interception source terms."),
    "vs_connectivity":      ("src/subsurface/vs_connectivity.f90", "Builds the cell, layer and link connectivity of the subsurface mesh."),
    "vs_input":             ("src/subsurface/vs_input.f90", "Reads the subsurface data and initial-condition files."),
    "vs_boundaries":        ("src/subsurface/vs_boundaries.f90", "Time-varying lateral, base and well boundary series."),
    "vs_soil_tables":       ("src/subsurface/vs_soil_tables.f90", "Generates and stores the soil hydraulic-property lookup tables."),
    "vs_config":            ("src/subsurface/vs_config.f90", "Soil parameters, initial-condition options and setup switches."),
    "vs_state":             ("src/subsurface/vs_state.f90", "Pressure head, water content and the subsurface flux fields."),

    # --- sediment ---------------------------------------------------------
    "sy_driver":            ("src/sediment/sy_driver.f90", "Sediment timestep driver and workspace allocation."),
    "sy_input":             ("src/sediment/sy_input.f90", "Reads and initialises the sediment data groups."),
    "sy_validation":        ("src/sediment/sy_validation.f90", "Consistency checks on sediment input and state."),
    "sy_transport_capacity":("src/sediment/sy_transport_capacity.f90", "Transport-capacity, critical-shear and settling-velocity relations."),
    "sy_hillslope":         ("src/sediment/sy_hillslope.f90", "Hillslope detachment, overland routing and fine-sediment exchange."),
    "sy_channel":           ("src/sediment/sy_channel.f90", "Channel routing, bed layers and bank erosion."),
    "sy_config":            ("src/sediment/sy_config.f90", "Sediment options, thresholds and per-class or per-soil parameters."),
    "sy_workspace":         ("src/sediment/sy_workspace.f90", "Per-timestep sediment work arrays shared between the sediment routines."),
    "sy_state":             ("src/sediment/sy_state.f90", "Mobile, loose and bed sediment state shared with other components."),

    # --- contaminant ------------------------------------------------------
    "cm_driver":            ("src/contaminant/cm_driver.f90", "Advances every active contaminant for one subsurface timestep."),
    "cm_input":             ("src/contaminant/cm_input.f90", "Reads the contaminant data file."),
    "cm_column":            ("src/contaminant/cm_column.f90", "Column advection-dispersion-reaction assembly and solution."),
    "cm_channel":           ("src/contaminant/cm_channel.f90", "Channel-link advection-dispersion-reaction assembly and solution."),
    "cm_sorption":          ("src/contaminant/cm_sorption.f90", "Soil and sediment sorption retardation factors."),
    "cm_plant":             ("src/contaminant/cm_plant.f90", "Two-compartment plant uptake path."),
    "cm_parameters":        ("src/contaminant/cm_parameters.f90", "Contaminant properties and shared transport state."),
    "cm_solver_flags":      ("src/contaminant/cm_solver_flags.f90", "Logical switches selecting the contaminant and nitrate calculation paths."),
    "cm_plant_state":       ("src/contaminant/cm_plant_state.f90", "Plant-uptake state and parameters shared with the nitrate component."),
    "cm_sediment_previous": ("src/contaminant/cm_sediment_previous.f90", "Previous-timestep sediment state used by the transport equations."),
    "cm_bank_geometry":     ("src/contaminant/cm_bank_geometry.f90", "Bank geometry and exchange indices used by contaminant transport."),
    "cm_column_scaling":    ("src/contaminant/column/cm_column_scaling.f90", "Scaling factors and active-cell range for the column solver."),
    "cm_column_water":      ("src/contaminant/column/cm_column_water.f90", "Per-column water state used by the transport solver."),
    "cm_column_state":      ("src/contaminant/column/cm_column_state.f90", "Per-column concentrations and source/sink terms."),
    "cm_column_equations":  ("src/contaminant/column/cm_column_equations.f90", "Coupled equation workspace for the column solver."),
    "cm_column_geometry":   ("src/contaminant/column/cm_column_geometry.f90", "Column-base, face-overlap and well-flow geometry."),
    "cm_column_previous":   ("src/contaminant/column/cm_column_previous.f90", "Previous-timestep column water state."),
    "cm_link_state":        ("src/contaminant/link/cm_link_state.f90", "Three-compartment workspace for one stream link."),
    "cm_link_scaling":      ("src/contaminant/link/cm_link_scaling.f90", "Nondimensional link length and bank-cell thickness."),
    "cm_link_water":        ("src/contaminant/link/cm_link_water.f90", "Bed geometry and retained water state for link preparation."),

    # --- nitrate ----------------------------------------------------------
    "mn_driver":            ("src/nitrate/mn_driver.f90", "Allocation, initialisation and the per-timestep nitrate sequence."),
    "mn_input":             ("src/nitrate/mn_input.f90", "Reads static nitrate data and the scheduled N and C additions."),
    "mn_validation":        ("src/nitrate/mn_validation.f90", "Consistency checks on nitrate input and state."),
    "mn_organic_matter":    ("src/nitrate/mn_organic_matter.f90", "Litter, humus and manure carbon turnover and CO2 production."),
    "mn_nitrogen":          ("src/nitrate/mn_nitrogen.f90", "Ammonium, nitrification, mineralisation and litter-nitrogen pools."),
    "mn_environment":       ("src/nitrate/mn_environment.f90", "Soil temperature and the temperature/moisture reduction factors."),
    "mn_plant":             ("src/nitrate/mn_plant.f90", "Plant nitrogen uptake."),
    "mn_output":            ("src/nitrate/mn_output.f90", "Nitrate and carbon budget output files."),
    "mn_state":             ("src/nitrate/mn_state.f90", "Carbon and nitrogen pools, rates and per-cell nitrate state."),
}

# ---------------------------------------------------------------------------
# Procedure placement: source module -> {procedure (lower) -> new module}
# ---------------------------------------------------------------------------
PROCS = {
    "FRmod": {
        "fropen": "frame_setup", "read_rundata_record": "frame_setup",
        "unit_context": "frame_setup", "stop_eof_error": "frame_setup",
        "stop_rundata_open_error": "frame_setup", "frinit": "frame_setup",
        "infr": "frame_setup", "docin": "frame_setup", "dinet": "frame_setup",
        "dinoc": "frame_setup",
        "frdim": "frame_geometry", "frind": "frame_geometry",
        "frltl": "frame_geometry", "frsort": "frame_geometry",
        # Component setup follows its component, not the frame that calls it.
        "inet": "et_input", "insm": "snow_input", "inbk": "bank_setup",
        "incm": "cm_input", "inpl": "cm_plant", "muerr2": "cm_input",
        "frmb": "mass_balance_report",
        "froutput": "frame_output",
        "initialise_output": "frame_output",
        "initialise_extra_discharge_points": "frame_output",
        "allocate_extra_discharge": "frame_output",
        "initialise_extra_water_table_output": "frame_output",
        "find_mass_balance_outlet": "frame_output",
        "write_discharge_header": "frame_output",
        "initialise_sediment_output": "frame_output",
        "initialise_contaminant_output": "frame_output",
        "write_main_output": "frame_output",
        "sample_current_values": "frame_output",
        "accumulate_interval": "frame_output",
        "write_completed_regular_outputs": "frame_output",
        "restart_accumulators": "frame_output",
        "write_regular_outputs": "frame_output",
        "timestamp_from_output_hour": "frame_output",
        "write_periodic_mass_balance": "frame_output",
        "write_final_state": "frame_output",
        "write_checked": "frame_output",
        "stop_on_io_error": "frame_output",
        "fatal_on_io_error": "frame_output",
        "frresc": "legacy_result_files", "frresp": "legacy_result_files",
        "res_write_check": "legacy_result_files",
        "write_dis": "frame_output", "write_dis2": "frame_output",
    },
    "VSmod": {
        "vssim": "vs_driver", "vsprep": "vs_driver", "vsmb": "vs_driver",
        "initialise_vsmod": "vs_driver",
        "vscolm": "vs_column_solver", "vscoef": "vs_column_solver",
        "vsfunc": "vs_column_solver", "vsbc": "vs_column_solver",
        "vswell": "vs_sources", "vsspr": "vs_sources", "vslowr": "vs_sources",
        "vsuppr": "vs_sources", "vsintc": "vs_sources", "vssai": "vs_sources",
        "vsconc": "vs_connectivity", "vsconl": "vs_connectivity",
        "fncell": "vs_connectivity",
        "vsread": "vs_input", "vsin": "vs_input", "abort_vsin": "vs_input",
        "initialise_vsread_buffers": "vs_input",
        "vssoil": "vs_soil_tables",
    },
    "SYmod": {
        "symain": "sy_driver", "initialise_symain_workspace": "sy_driver",
        "balsed": "sy_driver",
        "syread": "sy_input", "syinit": "sy_input", "sybc": "sy_input",
        "syerr0": "sy_validation", "syerr1": "sy_validation",
        "syerr2": "sy_validation", "syerr3": "sy_validation",
        "fnqout": "sy_validation",
        "syackw": "sy_transport_capacity", "fdgr": "sy_transport_capacity",
        "fa": "sy_transport_capacity", "syengh": "sy_transport_capacity",
        "syovtr": "sy_transport_capacity", "sycltr": "sy_transport_capacity",
        "sycrit": "sy_transport_capacity", "sydr": "sy_transport_capacity",
        "syover": "sy_hillslope", "sycolm": "sy_hillslope",
        "syfine": "sy_hillslope",
        "sylink": "sy_channel", "sybed": "sy_channel", "sybker": "sy_channel",
        "sywat": "sy_channel", "fqout": "sy_channel",
    },
    "CMmod": {
        "cmsim": "cm_driver", "cmrd": "cm_input",
        "colm": "cm_column", "colmsm": "cm_column", "colmw": "cm_column",
        "slvclm": "cm_column", "disp": "cm_column", "phi": "cm_column",
        "link": "cm_channel", "linksm": "cm_channel", "linkw": "cm_channel",
        "snl3": "cm_channel",
        "ret": "cm_sorption", "fret": "cm_sorption",
        "plcolm": "cm_plant", "plant": "cm_plant", "plprep": "cm_plant",
    },
    "MNmod": {
        "mnmain": "mn_driver", "mncont": "mn_driver",
        "mninitialise": "mn_driver", "mnallocate": "mn_driver",
        "mninit": "mn_driver", "mnisinitialised": "mn_driver",
        "mnred1": "mn_input", "mnred2": "mn_input", "mnint2": "mn_input",
        "mnerr0": "mn_validation", "mnerr1": "mn_validation",
        "mnerr2": "mn_validation", "mnerr3": "mn_validation",
        "mnerr4": "mn_validation",
        "mnco2": "mn_organic_matter", "mnlthm": "mn_organic_matter",
        "mnman": "mn_organic_matter",
        "mnamm": "mn_nitrogen", "mnnit": "mn_nitrogen",
        "mnltn": "mn_nitrogen", "mngam": "mn_nitrogen",
        "mntemp": "mn_environment", "mnedth": "mn_environment",
        "mnemph": "mn_environment", "mnemt": "mn_environment",
        "mnenph": "mn_environment", "mnent": "mn_environment",
        "mnplantinitialise": "mn_plant", "mnplant": "mn_plant",
        "mnout": "mn_output",
    },
    "OCmod": {
        "ocsim": "oc_driver", "ocini": "oc_driver",
        "initialise_ocsim_workspace": "oc_driver",
        "finalise_ocsim_workspace": "oc_driver",
        "ocsim_workspace_has_allocations": "oc_driver",
        "ocread": "oc_input", "ocplf": "oc_input", "jeocbc": "oc_input",
        "ocxs": "oc_cross_sections",
        "ocind": "oc_indexing", "linkno": "oc_indexing",
        "occhk0": "oc_validation", "occhk1": "oc_validation",
        "occhk2": "oc_validation", "ocltl": "oc_validation",
        "ocabc": "oc_boundaries", "ocext": "oc_boundaries",
        "ocpri": "oc_boundaries",
    },
    "OCmod2": {
        "ocnode": "oc_node_solver", "fnode": "oc_node_solver",
        "ocfix": "oc_node_solver", "occode": "oc_node_solver",
        "gethrf": "oc_node_solver", "sethrf": "oc_node_solver",
        "getqsa": "oc_node_solver", "setqsa": "oc_node_solver",
        "initialise_ocmod": "oc_node_solver",
        "ocqbc": "oc_discharge", "ocqbnk": "oc_discharge",
        "ocqgrd": "oc_discharge", "ocqlnk": "oc_discharge",
        "ocqmln": "oc_discharge", "conveyan": "oc_discharge",
        "qweir": "oc_discharge",
    },
    "ocqdqmod": {"ocqdq": "oc_stage_discharge", "fstr": "oc_stage_discharge",
                 "fdqq": "oc_stage_discharge"},
    "ZQmod": {"readzqtable": "zq_tables", "get_zqtable_value": "zq_tables"},
    "OC_ROW_WIDTH": {"max_active_row_width": "oc_indexing"},
    "ETmod": {
        "et": "et_process", "etchk2": "et_process", "etsim": "et_process",
        "initialise_etmod": "et_process", "etin": "et_input",
    },
    "SMmod": {
        "sm": "snowmelt", "smet": "snowmelt", "initialise_smmod": "snowmelt",
        "smin": "snow_input",
    },
    "rest": {
        "tmstep": "timestep_control", "balwat": "water_balance",
        "extra_output": "run_summary", "metin": "met_input",
        "read_dated_record": "met_input", "resize_met_record": "met_input",
    },
    "GETDIRQQ": {
        "get_dir_and_catch": "command_line",
        "derive_catch_from_filename": "command_line",
        "print_usage_and_stop": "command_line",
        "set_error_mode_from_arguments": "command_line",
        "get_current_dir": "command_line",
        "handle_command_line_error": "command_line",
        "comdlger": "command_line",
    },
    "run_sim": {"simulation": "simulation_driver"},
    "mod_error": {
        "raise_error": "error_reporting", "err_stop": "error_reporting",
        "err_set_wait_on_exit": "error_reporting",
        "errstat_fileopen": "error_status", "errstat_fileclose": "error_status",
        "errstat_alloc": "error_status", "errstat_dealloc": "error_status",
        "errstat_read": "error_status", "errstat_write": "error_status",
        "errstat_rewind": "error_status",
    },
    "mod_load_filedata": {
        "alread": "record_readers", "throw_fatal": "record_readers",
        "alred2": "record_readers", "alredc": "record_readers",
        "alredf": "record_readers", "alredi": "record_readers",
        "alredl": "record_readers",
        "alallf": "spatial_fields", "alalli": "spatial_fields",
        "albank": "spatial_fields", "alsprd": "spatial_fields",
        "alchk": "input_validation", "alchki": "input_validation",
        "alintp": "interpolation",
        "altrap": "platform_traps",
    },
    "utilsmod": {
        "hour_from_date": "datetime", "date_from_hour": "datetime",
        "days_in_years_since_1950": "datetime", "is_leap": "datetime",
        "days_to_start_month": "datetime",
        "finput": "timeseries_input", "hinput": "timeseries_input",
        "dcopy": "linear_algebra", "jematmul_mm": "linear_algebra",
        "jematmul_vm": "linear_algebra", "tridag": "linear_algebra",
        "invertmat": "linear_algebra", "lubksb": "linear_algebra",
        "ludcmp": "linear_algebra",
        "terpo1": "interpolation",
        "areadi": "grid_arrays", "areadr": "grid_arrays",
        "ran2": "random_numbers",
    },
    "tolerance_testing": {},          # whole module -> float_compare
    "AL_C": {
        # The three allocators follow the arrays they allocate.
        "initialise_al_c": "vs_state", "initialise_al_c2": "vs_state",
        "initialise_al_c3": "et_state",
    },
    "COLM_CG": {}, "COLM_CO": {}, "CONT_CC": {},
    "mod_parameters": {},
}

# Modules that move wholesale; every remaining procedure and variable lands here.
WHOLE_MODULE = {
    "tolerance_testing": "float_compare",
    "CONT_CC": "cm_parameters",
    "IS_CC": "cm_solver_flags",
    "PLANT_CC": "cm_plant_state",
    "SED_CO": "cm_sediment_previous",
    "BK_CW": "cm_bank_geometry",
    "COLM_C1": "cm_column_scaling",
    "COLM_C2": "cm_column_water",
    "COLM_CC": "cm_column_state",
    "COLM_CC1": "cm_column_equations",
    "COLM_CG": "cm_column_geometry",
    "COLM_CO": "cm_column_previous",
    "LINK_CC": "cm_link_state",
    "LINK_CC1": "cm_link_scaling",
    "LINK_CW": "cm_link_water",
    "sed_cs": "sy_state",
    "AL_G": "grid_topology",
    "MNmod": "mn_state",
    "ZQmod": "zq_tables",
    "const_sy": "mod_parameters",
    "mod_parameters": "mod_parameters",   # stays put
}

# ---------------------------------------------------------------------------
# Variable placement
# ---------------------------------------------------------------------------
# Renamed constants gathered into mod_parameters. Values are preserved exactly;
# the duplicates that disagree numerically are listed in constants_review.md.
CONST_RENAME = {
    ("const_sy", "GRAVTY"): "GRAVITY",
    ("const_sy", "RHOSED"): "RHO_SEDIMENT",
    ("const_sy", "RHOWAT"): "RHO_WATER_SEDIMENT",
    ("const_sy", "VISCOS"): "NU_WATER",
    ("ETmod", "LAMDA"): "L_VAPORISATION_ET",
    ("ETmod", "GAMMA"): "PSYCHROMETRIC_CONSTANT",
    ("ETmod", "RHO"): "RHO_AIR_ET",
    ("ETmod", "CP"): "CP_AIR_ET",
    ("SMmod", "RHOA"): "RHO_AIR_SNOW",
    ("SMmod", "RHOW"): "RHO_WATER_SNOW",
    ("SMmod", "CPA"): "CP_AIR_SNOW",
    ("SMmod", "CPW"): "CP_WATER",
    ("SMmod", "CPI"): "CP_ICE",
    ("SMmod", "LWI"): "L_FUSION",
    ("SMmod", "LVW"): "L_VAPORISATION_SNOW",
    ("SMmod", "HFG"): "GROUND_HEAT_FLUX_SNOW",
    ("OCmod2", "F23"): "TWO_THIRDS",
    ("OCmod2", "F53"): "FIVE_THIRDS",
    ("OCmod2", "ROOT2G"): "SQRT_TWO_G",
}

# Renames forced by a name collision between two modules being merged. These do
# NOT imply a move to mod_parameters.
DISAMBIGUATE = {
    # AL_D's ZQTableRef (the table selected for the current link face, written
    # by OCQDQ) and ZQmod's PRIVATE ZQTableRef (the reference number read while
    # loading each table) both land in zq_tables. Rename the private one; the
    # shared one keeps its name.
    ("ZQmod", "ZQTableRef"): "ZQTableRefRead",
}

VARS = {
    "sglobal": {
        "shever": "build_info", "bdever": "build_info", "banner": "build_info",
        "runfil": "build_info",
        "dirqq": "run_context", "filnam": "run_context", "cnam": "run_context",
        "rootdir": "run_context", "hdf5filename": "run_context",
        "visualisation_plan_filename": "run_context",
        "visualisation_check_filename": "run_context",
        "error_mode": "run_context",
        "uznow": "simulation_clock",
        "total_no_elements": "element_geometry",
        "total_no_links": "element_geometry",
        "top_cell_no": "element_geometry", "cellarea": "element_geometry",
        "dxqq": "element_geometry", "dyqq": "element_geometry",
        "zgrund": "element_geometry",
        "flag_runtime_reduction_errors": "runtime_flags",
        "flag_runtime_reduction_e1060": "runtime_flags",
        "szmonte": "legacy_retained", "ran2monte1": "legacy_retained",
        "ran2monte2": "legacy_retained", "pcmonte": "legacy_retained",
        "montec": "legacy_retained", "earray": "legacy_retained",
        "text32": "legacy_retained",
        # Mathematical constants and comparison tolerances.
        "marker999": "mod_parameters", "imarker": "mod_parameters",
        "izero": "mod_parameters", "ione": "mod_parameters",
        "izero1": "mod_parameters", "ione1": "mod_parameters",
        "zero": "mod_parameters", "half": "mod_parameters",
        "one": "mod_parameters", "two": "mod_parameters",
        "three": "mod_parameters", "five": "mod_parameters",
        "vsmall": "mod_parameters", "zero1": "mod_parameters",
        "one1": "mod_parameters",
        # Everything else left in sglobal is an array capacity bound.
        "__default__": "array_limits",
        "__constants__": "array_limits",
    },
    "AL_G": {"__default__": "grid_topology"},
    "AL_C": {
        "icmrf2": "grid_topology",
        "tih": "simulation_clock", "dtuz": "simulation_clock",
        "uznext": "simulation_clock",
        "nbface": "element_geometry", "dhf": "element_geometry",
        "isort": "element_geometry",
        "idum": "input_workspace", "dummy": "input_workspace",
        "ispack": "snow_state",
        "nhsat": "legacy_retained",
        "sberr": "sy_state",
        "icmbk": "channel_geometry", "nhbed": "channel_geometry",
        "fhbed": "channel_geometry", "bexbk": "channel_geometry",
        "clenth": "channel_geometry", "cwidth": "channel_geometry",
        "zbeff": "channel_geometry", "zbfull": "channel_geometry",
        "linkns": "channel_geometry",
        "arxl": "oc_state", "qoc": "oc_state",
        "nvc": "et_state", "nv": "et_state", "nrd": "et_state",
        "rdl": "et_state", "rdf": "et_state", "clai": "et_state",
        "plai": "et_state", "draina": "et_state", "esoila": "et_state",
        "eevap": "et_state", "pnetto": "et_state", "eruz": "et_state",
        "__default__": "vs_state",
        "__constants__": "file_units",
    },
    "AL_D": {
        "ingrid": "grid_topology",
        "carea": "element_geometry", "dxin": "element_geometry",
        "dyin": "element_geometry", "bwidth": "element_geometry",
        "nxp1": "element_geometry", "nyp1": "element_geometry",
        "nxm1": "element_geometry", "nym1": "element_geometry",
        "nxep1": "element_geometry", "nyep1": "element_geometry",
        "nstep": "simulation_clock", "tth": "simulation_clock",
        "timeuz": "simulation_clock", "uzval": "simulation_clock",
        "nm": "met_forcing", "nrain": "met_forcing", "nmc": "met_forcing",
        "nrainc": "met_forcing", "dtmet": "met_forcing",
        "dtmet2": "met_forcing", "dtmet3": "met_forcing",
        "ista": "met_forcing", "ta": "met_forcing", "u": "met_forcing",
        "vpd": "met_forcing", "rn": "met_forcing", "obspe": "met_forcing",
        "precip_m_per_s": "met_forcing",
        "pmax": "timestep_control", "palfa": "timestep_control",
        "tmax": "timestep_control", "prest": "timestep_control",
        "nset": "legacy_result_files", "iocors": "legacy_result_files",
        "iodata": "legacy_result_files", "ioelem": "legacy_result_files",
        "iores": "legacy_result_files", "iclist": "legacy_result_files",
        "iclnum": "legacy_result_files", "iosta": "legacy_result_files",
        "iostep": "legacy_result_files", "ioend": "legacy_result_files",
        "iotime": "legacy_result_files", "resfil": "legacy_result_files",
        "pstart": "legacy_result_files",
        "mblink": "water_balance", "mbface": "water_balance",
        "mbflag": "water_balance", "mbyear": "water_balance",
        "mbmon": "water_balance", "mbday": "water_balance",
        "balanc": "water_balance",
        "qmax": "oc_state", "lcodex": "oc_state", "lcodey": "oc_state",
        "ocnow": "oc_state", "ocnext": "oc_state", "dq0st": "oc_state",
        "dqist": "oc_state", "dqist2": "oc_state",
        "nocbcc": "oc_boundaries", "nocbcd": "oc_boundaries",
        "nozqtables": "zq_tables", "zqtableref": "zq_tables",
        "zqtablelink": "zq_tables", "zqtableface": "zq_tables",
        "zqweirsill": "zq_tables", "iszq": "zq_tables",
        "msm": "snow_state", "nsmt": "snow_state", "nsmc": "snow_state",
        "sd": "snow_state", "ts": "snow_state", "sf": "snow_state",
        "rhosar": "snow_state",
        "cstore": "et_state", "erza": "et_state", "epot": "et_state",
        "einta": "et_state", "eswa": "et_state", "s": "et_state",
        "hruz": "et_state", "pnet": "et_state", "pe": "et_state",
        "eint": "et_state", "erz": "et_state", "drain": "et_state",
        "esoil": "et_state", "ae": "et_state", "cstold": "et_state",
        "cplai": "et_state", "vht": "et_state",
        "ngrid": "legacy_retained", "nexpo": "legacy_retained",
        "widtf": "legacy_retained", "zbed": "legacy_retained",
        "hflbed": "legacy_retained", "zfbed": "legacy_retained",
        "dzfbed": "legacy_retained", "lroot": "legacy_retained",
        "hflbnk": "legacy_retained", "epotr": "legacy_retained",
        "cmean": "legacy_retained", "smean": "legacy_retained",
        "admean": "legacy_retained", "flerrc": "legacy_retained",
        "syerrc": "legacy_retained", "cmerrc": "legacy_retained",
        "bexts1": "legacy_retained", "nrpd": "legacy_retained",
        "__default__": "run_control",   # BEX*, hotstart, isextra*, TOUTPUT
        "__constants__": "file_units",  # unit numbers; NXE/NYE/NCLASS below
    },
    "mod_error": {
        "errcode_fileopen": "error_status", "errcode_fileclose": "error_status",
        "errcode_allocate": "error_status", "errcode_deallocate": "error_status",
        "errcode_read": "error_status", "errcode_write": "error_status",
        "errcode_rewind": "error_status",
        "fid_logfile": "file_units",
        "__default__": "error_reporting",
    },
    "FRmod": {
        "iaout": "legacy_result_files", "allout": "legacy_result_files",
        "dtao": "legacy_result_files", "bstore": "legacy_result_files",
        "btime": "legacy_result_files", "sedsrt": "legacy_result_files",
        "gnucum": "legacy_result_files", "dlssrt": "legacy_result_files",
        "prevtm": "mass_balance_report", "timb": "mass_balance_report",
        "first_frmb": "mass_balance_report",
        "next_hour": "frame_output", "icounter2": "frame_output",
        "hour_now": "frame_output", "qoctot": "frame_output",
        "uzold": "frame_output", "uznowt": "frame_output",
        "sedtot": "frame_output", "sedfinetot": "frame_output",
        "contamtot": "frame_output", "qoctotextra": "frame_output",
        "bppnet": "frame_output", "bpepot": "frame_output",
        "bpqoc": "frame_output", "bpdep": "frame_output",
        "bpqf": "frame_output", "bpqh": "frame_output",
        "bpqsz": "frame_output", "bphsz": "frame_output",
        "bpbal": "frame_output", "bpsd": "frame_output",
        "__default__": "run_control",
    },
    "rest": {
        "first_balwat": "water_balance", "storw_balwat": "water_balance",
        "__default__": "met_input",
    },
    "ETmod": {
        "__default__": "et_input",
        "__constants__": "mod_parameters",
    },
    "SMmod": {
        "smelt": "snow_state", "tmelt": "snow_state", "rhosar": "snow_state",
        "__default__": "snow_input",
        "__constants__": "mod_parameters",
    },
    "OCmod": {
        "xinh": "oc_cross_sections", "xinw": "oc_cross_sections",
        "xarea": "oc_cross_sections", "nxsect": "oc_cross_sections",
        "hoclst": "oc_boundaries", "hocnxt": "oc_boundaries",
        "qflast": "oc_boundaries", "qfnext": "oc_boundaries",
        "hocprv": "oc_boundaries", "qocfin": "oc_boundaries",
        "hocnxv": "oc_boundaries", "nochb": "oc_boundaries",
        "nocfb": "oc_boundaries",
        "tdc": "oc_driver", "tfc": "oc_driver", "dtoc": "oc_driver",
        "ocsim_workspace": "oc_driver",
        "ocsim_workspace_type": "oc_driver",  # travels with its instance
        "__default__": "oc_indexing",
    },
    "OCmod2": {
        "dzmin": "oc_discharge", "rdzmin": "oc_discharge",
        "h23min": "oc_discharge",
        "__default__": "oc_state",
        "__constants__": "mod_parameters",
    },
    "ocqdqmod": {
        "xafull": "oc_cross_sections",
        "cocbcd": "oc_boundaries", "hocnow": "oc_boundaries",
        "qocf": "oc_boundaries",
        "__default__": "oc_state",
    },
    "SYmod": {
        "first_syackw": "sy_transport_capacity",
        "first_syfine": "sy_transport_capacity",
        "wsed_syfine": "sy_transport_capacity",
        "k1_syovtr": "sy_transport_capacity",
        "k3_syovtr": "sy_transport_capacity",
        "k4_syovtr": "sy_transport_capacity",
        "__default__": "sy_config",
        "__linerange__": [(172, 196, "sy_workspace")],
    },
    "VSmod": {
        "__default__": "vs_config",
        "__linerange__": [
            (124, 128, "vs_state"),      # cached legacy solver state
            (129, 137, "vs_input"),      # VSREAD read buffers
            (138, 164, "vs_boundaries"), # boundary-series times and values
            (165, 234, "vs_config"),     # legacy VSCOM1 configuration
            (235, 253, "vs_soil_tables"),
            (254, 285, "vs_config"),     # legacy VSINIT configuration
        ],
    },
    "CMmod": {
        "lwork": "cm_channel", "nbk": "cm_channel", "nwell": "cm_channel",
        "islk": "cm_channel", "qqqdum": "cm_channel", "qqqsl1": "cm_channel",
        "__default__": "cm_column",
    },
    "mod_load_filedata": {"__default__": "record_readers"},
    "utilsmod": {"__default__": "linear_algebra"},
    "GETDIRQQ": {"__default__": "command_line"},
    "sed_cs": {"__default__": "sy_state"},
    "MNmod": {"__default__": "mn_state"},
    "ZQmod": {"__default__": "zq_tables"},
}

# Capacity parameters that stay shared even though they came from AL_D.
ARRAY_LIMIT_NAMES = {"nxe", "nye", "nclass"}
# Constants that are sizing bounds of a single component and stay with it.
COMPONENT_LIMITS = {
    ("SYmod", "nsybee"): "sy_config", ("SYmod", "nsycee"): "sy_config",
    ("VSmod", "nsolee"): "vs_soil_tables",
    ("MNmod", "mn_plant_nvalee"): "mn_state",
    ("VSmod", "errcntallowed"): "vs_driver",
    ("rest", "record_headroom"): "met_input",
    ("rest", "iostage_none"): "met_input", ("rest", "iostage_record"): "met_input",
    ("rest", "iostage_values"): "met_input",
    ("rest", "ios_short_record"): "met_input",
    ("utilsmod", "eps"): "linear_algebra",
    ("PLANT_CC", "ntemp1"): "cm_plant_state",
    ("PLANT_CC", "ntemp2"): "cm_plant_state",
    ("mod_error", "errlvl_fatal"): "error_reporting",
    ("mod_error", "errlvl_error"): "error_reporting",
    ("mod_error", "errlvl_warn"): "error_reporting",
    ("mod_error", "err_limit_error_codes"): "error_reporting",
}


def target_for_var(row):
    mod, name = row["module"], row["name"].lower()
    is_param = row["is_parameter"] == "True"

    if (mod, name) in COMPONENT_LIMITS:
        return COMPONENT_LIMITS[(mod, name)]
    if (mod, row["name"]) in CONST_RENAME:
        return "mod_parameters"
    if mod in WHOLE_MODULE and mod not in VARS:
        return WHOLE_MODULE[mod]

    rules = VARS.get(mod)
    if rules is None:
        return WHOLE_MODULE.get(mod, "")
    if name in rules:
        return rules[name]
    if is_param:
        if mod == "AL_D" and name in ARRAY_LIMIT_NAMES:
            return "array_limits"
        if "__constants__" in rules:
            return rules["__constants__"]
    for lo, hi, tgt in rules.get("__linerange__", []):
        if lo <= int(row["line"]) <= hi:
            return tgt
    return rules.get("__default__", "")


def target_for_type(row):
    """A derived type follows the same per-module map as a variable.

    It is not a PARAMETER and has no line-range special case, so the shared
    rules apply unchanged; only `OCSIM_WORKSPACE_TYPE` needs a named entry,
    to keep it with its instance rather than with `OCmod`'s default target.
    """
    return target_for_var({
        "module": row["module"], "name": row["name"],
        "is_parameter": "False", "line": row["start"],
    })


def target_for_proc(row):
    mod, name = row["module"], row["name"].lower()
    rules = PROCS.get(mod, {})
    if name in rules:
        return rules[name]
    return WHOLE_MODULE.get(mod, "")


def ref_columns(use):
    """Reference columns for one entity, confirmed set first."""
    return [
        use.get("n_refs_confirmed", ""), use.get("refs_confirmed", ""),
        use.get("n_refs_possible", ""), use.get("refs_possible", ""),
    ]


def doc_block(lines, doc_start, unit_start):
    if not doc_start:
        return ""
    return "\n".join(lines[int(doc_start) - 1:int(unit_start) - 1]).strip()


def main():
    units = list(csv.DictReader((OUT / "_inventory_units.csv").open()))
    varrows = list(csv.DictReader((OUT / "_inventory_vars.csv").open()))
    uunits = {(r["name"], r["module"], r["start"]): r
              for r in csv.DictReader((OUT / "_usage_units.csv").open())}
    uvars = {(r["name"], r["module"], r["line"]): r
             for r in csv.DictReader((OUT / "_usage_vars.csv").open())}
    typerows = list(csv.DictReader((OUT / "_inventory_types.csv").open()))
    utypes = {(r["name"], r["module"], r["start"]): r
              for r in csv.DictReader((OUT / "_usage_types.csv").open())}

    filecache = {}

    def lines_of(f):
        if f not in filecache:
            filecache[f] = (ROOT / f).read_text(errors="replace").splitlines()
        return filecache[f]

    unresolved = []

    # ---------------- functions.csv ----------------
    with (OUT / "functions.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow([
            "function_name", "kind", "source_file", "source_module",
            "starting_line", "ending_line", "doc_block_start_line",
            "move_block_start_line", "nested_in_parent", "target_file",
            "target_module", "target_module_purpose", "referenced_by_n_files",
            "referenced_by", "possibly_referenced_by_n_files",
            "possibly_referenced_by", "documentation",
        ])
        for r in sorted(units, key=lambda x: (x["file"], int(x["start"]))):
            tgt = target_for_proc(r)
            if not tgt:
                unresolved.append(("proc", r["module"], r["name"]))
            path, purpose = TARGETS.get(tgt, ("", ""))
            lines = lines_of(r["file"])
            use = uunits.get((r["name"], r["module"], r["start"]), {})
            w.writerow([
                r["name"], r["kind"], r["file"], r["module"],
                r["start"], r["end"], r["doc_start"] or "",
                r["doc_start"] or r["start"], r["nested"],
                path, tgt, purpose,
                *ref_columns(use),
                doc_block(lines, r["doc_start"], r["start"]),
            ])

    # ---------------- variables.csv ----------------
    with (OUT / "variables.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow([
            "variable_name", "source_file", "source_module", "line",
            "declared_type", "attributes", "dimensions", "is_parameter",
            "target_file", "target_module", "target_module_purpose",
            "proposed_rename", "referenced_by_n_files", "referenced_by",
            "possibly_referenced_by_n_files", "possibly_referenced_by",
            "documentation",
        ])
        for r in sorted(varrows, key=lambda x: (x["file"], int(x["line"]))):
            tgt = target_for_var(r)
            if not tgt:
                unresolved.append(("var", r["module"], r["name"]))
            path, purpose = TARGETS.get(tgt, ("", ""))
            use = uvars.get((r["name"], r["module"], r["line"]), {})
            w.writerow([
                r["name"], r["file"], r["module"], r["line"],
                r["type"], r["attrs"], r["dims"], r["is_parameter"],
                path, tgt, purpose,
                CONST_RENAME.get((r["module"], r["name"]))
                or DISAMBIGUATE.get((r["module"], r["name"]), ""),
                *ref_columns(use),
                r["doc"],
            ])

    # ---------------- types.csv ----------------
    with (OUT / "types.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow([
            "type_name", "source_file", "source_module", "starting_line",
            "ending_line", "doc_block_start_line", "move_block_start_line",
            "n_components", "target_file", "target_module",
            "target_module_purpose", "referenced_by_n_files", "referenced_by",
            "possibly_referenced_by_n_files", "possibly_referenced_by",
            "documentation",
        ])
        for r in sorted(typerows, key=lambda x: (x["file"], int(x["start"]))):
            tgt = target_for_type(r)
            if not tgt:
                unresolved.append(("type", r["module"], r["name"]))
            path, purpose = TARGETS.get(tgt, ("", ""))
            lines = lines_of(r["file"])
            use = utypes.get((r["name"], r["module"], r["start"]), {})
            w.writerow([
                r["name"], r["file"], r["module"], r["start"], r["end"],
                r["doc_start"] or "", r["doc_start"] or r["start"],
                r["n_components"], path, tgt, purpose,
                *ref_columns(use),
                doc_block(lines, r["doc_start"], r["start"]),
            ])

    print(f"functions.csv: {len(units)} rows")
    print(f"variables.csv: {len(varrows)} rows")
    print(f"types.csv: {len(typerows)} rows")
    if unresolved:
        print(f"UNRESOLVED ({len(unresolved)}):")
        for kind, mod, name in unresolved[:40]:
            print(f"  {kind:4} {mod}::{name}")
    else:
        print("all entities assigned a target")

    # summary of file sizes implied by the split
    import collections
    per = collections.Counter()
    for r in units:
        t = target_for_proc(r)
        if t and r["nested"] == "False":
            per[t] += int(r["end"]) - int(r["start"]) + 1
    print("\nApproximate procedure lines per target module:")
    for t, n in sorted(per.items(), key=lambda x: -x[1]):
        print(f"  {n:6d}  {t}")


if __name__ == "__main__":
    main()
