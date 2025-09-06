---
project: SHETRAN
version: 4.4.7
license: Custom License
author: Newcastle University Water Group
author_description: Hydrological modeling research group
email: stephen.birkinshaw@ncl.ac.uk
src_dir: src
output_dir: docs/ford
project_github: https://github.com/nclwater/Shetran
project_website: https://research.ncl.ac.uk/shetran/
summary: SHETRAN is a physically-based, distributed hydrological model for simulating water flow, sediment transport, and contaminant migration in catchments.
graph: true
search: true
display: public
         protected
         private
preprocess: false
exclude_dir: src/legacy
            src/backup
exclude: src/util/getdirqq_winIntel.f90
         src/util/getdirqq.f90
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
           iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
copy_subdir: examples
            docs/reports
md_extensions: markdown.extensions.toc
              markdown.extensions.smarty
---

SHETRAN is a physically-based, distributed hydrological model developed at Newcastle University for simulating integrated water, sediment, and contaminant processes in river catchments.

## Model Capabilities

SHETRAN provides comprehensive simulation of:

- **Hydrological processes**: Surface and subsurface water flow, evapotranspiration, snow dynamics
- **Sediment transport**: Erosion, deposition, and sediment routing through catchment systems  
- **Contaminant migration**: Solute transport, adsorption, and chemical transformations
- **Hydraulic structures**: Weirs, reservoirs, and flow control mechanisms

## Development History

SHETRAN has evolved through several major development phases:

### Original Development (1980s-1990s)
- Developed at the Institute of Hydrology (now CEH Wallingford)
- Initial implementation in FORTRAN 77
- Focus on distributed hydrological modeling concepts

### Newcastle Era (2000s-2010s)
- Transferred to Newcastle University Water Resources Group
- Enhanced with sediment transport and contaminant modeling
- Extended for larger-scale catchment applications

### Recent Modernization (2020s)
- **Cross-platform compatibility**: Eliminated Windows-specific dependencies and DFWIN calls
- **Memory management**: Improved dynamic allocation and deallocation practices
- **Code organization**: Refactored monolithic modules into focused, maintainable components
- **Build system**: Modern CMake-based build with automated dependency handling
- **Standards compliance**: Migration from FORTRAN 77/90 to modern Fortran standards
- **Documentation**: Comprehensive documentation using FORD system
- **Performance optimization**: Enhanced computational efficiency and reduced memory footprint

## Getting Started

For build instructions, see [CMAKE_BUILD.md](CMAKE_BUILD.md).
For usage examples, explore the `examples/` directory.
For detailed API documentation, browse the modules and procedures sections below.

## Project Structure

The SHETRAN codebase is organized into focused modules:

- **Framework**: Core simulation engine and mass balance calculations
- **Overland Channel**: Surface water flow and routing
- **Sediment**: Erosion and transport processes
- **Evapotranspiration**: ET calculations and plant processes
- **Snow Model**: Snow accumulation and melt dynamics
- **I/O Systems**: Data input/output and file management

## Development Standards

SHETRAN follows modern software engineering practices:

- **Modular architecture**: Clear separation of concerns with well-defined interfaces
- **Comprehensive documentation**: FORD-compliant documentation for all public interfaces
- **Cross-platform compatibility**: Tested on Linux, Windows, and Unix systems
- **Version control**: Git-based development with systematic change tracking
- **Code review**: Peer review process for all major changes

## Research Applications

SHETRAN has been applied to numerous research areas:

- Climate change impact assessment
- Flood risk modeling and management
- Water quality and contamination studies
- Sediment yield prediction
- Land use change impact analysis
- Extreme event simulation
