# SHETRAN CMake Build System

This directory contains a CMake-based build system for SHETRAN that supports multiple compilers and platforms with automatic source file discovery and dependency ordering.

## Key Features

- **Automatic Source Discovery**: Automatically finds all Fortran source files in the `src/` directory
- **Intelligent Dependency Ordering**: Orders source files based on module dependencies and patterns
- **Cross-platform Support**: Works on Linux, Unix, and Windows  
- **Multi-compiler Support**: Intel Fortran (ifort, ifx) and GNU Fortran (gfortran)
- **HDF5 Integration**: Automatic HDF5 detection on Linux, provided libraries on Windows

## Quick Start

### Linux/Unix

1. **Using the build script (recommended):**
   ```bash
   # Auto-detect compiler and build
   ./build.sh
   
   # Build with specific compiler
   ./build.sh -c ifort      # Intel Fortran (Classic)
   ./build.sh -c ifx        # Intel Fortran (LLVM)
   ./build.sh -c gfortran   # GNU Fortran
   
   # Debug build
   ./build.sh -t Debug
   
   # Clean build
   ./build.sh --clean
   
   # Build and install
   ./build.sh --install -p /usr/local
   ```

2. **Using CMake directly:**
   ```bash
   mkdir build && cd build
   cmake -DCMAKE_BUILD_TYPE=Release ..
   make -j$(nproc)
   ```

### Windows

1. **Using the build script:**
   ```cmd
   REM Build with auto-detected Intel Fortran compiler
   build.bat
   
   REM Build with ifx
   build.bat -c ifx
   
   REM Debug build
   build.bat -t Debug
   
   REM Install
   build.bat --install -p "C:\SHETRAN"
   ```

2. **Using CMake directly:**
   ```cmd
   mkdir build && cd build
   cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release ..
   nmake
   ```

## Requirements

### Linux
- CMake 3.12 or higher
- One of the following Fortran compilers:
  - Intel Fortran Compiler (ifort or ifx)
  - GNU Fortran (gfortran)
- HDF5 development packages:
  - Ubuntu/Debian: `sudo apt install libhdf5-dev`
  - CentOS/RHEL: `sudo yum install hdf5-devel`
  - Fedora: `sudo dnf install hdf5-devel`

### Windows
- CMake 3.12 or higher
- Intel Fortran Compiler (ifort or ifx)
- HDF5 libraries (provided in `external/` directory)
- Visual Studio Build Tools or Visual Studio

## Compiler Auto-Detection

When using the build script with auto-detection (default behavior), the compiler selection follows platform-specific priorities:

- **Linux**: Prefers GNU Fortran (gfortran) first, then Intel Fortran compilers (ifort, ifx)
- **Windows**: Prefers Intel Fortran compilers (ifort, ifx) first, then GNU Fortran (gfortran)

You can override this behavior by explicitly specifying a compiler with the `-c` option:
```bash
./build.sh -c gfortran   # Force GNU Fortran on any platform
./build.sh -c ifort      # Force Intel Fortran (Classic) on any platform
./build.sh -c ifx        # Force Intel Fortran (LLVM) on any platform
```

## Compiler-Specific Settings

### Intel Fortran (ifort)
- Uses `-mcmodel=large` for large memory models (configurable)
- Uses `-heap-arrays <size>` for large array handling (default: 100000000)
- Uses `-fpp` for preprocessing.
- Optimized with `-O3` for release builds
- Suppresses G-format descriptor remark (`/Qdiag-disable:8291` on Windows, `-diag-disable 8291` on Linux).

### Intel Fortran (ifx)
- **Linux**:
    - Uses `-mcmodel=large` for large memory models (configurable).
    - Uses `-fpp` for preprocessing.
    - Optimized with `-O3 -xHost` for release builds.
    - Suppresses G-format descriptor remark (`-diag-disable 8291`).
- **Windows**:
    - Uses `/fpp` for preprocessing.
    - Optimized with `/O3 /QxHost` for release builds.
    - Suppresses G-format descriptor remark (`/Qdiag-disable:8291`).
    - Sets linker flag `/link /STACK:500000000` for large stack arrays.
    - Note: `-mcmodel=large` and `-fstack-arrays` are not applicable/used.

### GNU Fortran (gfortran)
- Uses `-mcmodel=large` for large memory models (configurable)
- Uses `-fmax-stack-var-size=<size>` for large arrays (default: 100000000)
- Uses `-cpp` for preprocessing.
- Optimized with `-O3` for release builds

## Build Options

### Automatic Source Discovery
The build system automatically discovers all Fortran source files in the `src/` directory and its subdirectories. You no longer need to manually maintain lists of source files in CMakeLists.txt.

### Dependency Ordering Options
- **Automatic dependency analysis** (default, recommended): Analyzes actual module `USE` statements to determine the correct compilation order.
- **Pattern-based ordering** (fallback): Uses filename patterns as a fallback if automatic analysis is disabled.

```bash
# The build scripts use automatic dependency analysis by default.

# To disable automatic analysis and use the pattern-based fallback:
cmake -DENABLE_DEPENDENCY_ANALYSIS=OFF ..

# To enable verbose dependency output during analysis:
cmake -DVERBOSE_DEPENDENCY_OUTPUT=ON ..
```

### Memory Configuration Options
The build system provides configurable memory settings:

```bash
# Use default large memory settings
./build.sh

# Disable large memory model 
./build.sh --disable-large-memory

# Custom heap array size for Intel ifort
./build.sh --heap-size 50000000

# Custom stack variable size for gfortran  
./build.sh --stack-size 50000000

# Using CMake directly
cmake -DENABLE_LARGE_MEMORY_MODEL=OFF ..
cmake -DHEAP_ARRAY_SIZE=50000000 -DSTACK_VAR_SIZE=50000000 ..
```

**Default Values:**
- Large memory model: **Enabled** (`-mcmodel=large`)
- Intel ifort heap arrays: **100000000** (`-heap-arrays 100000000`)
- GNU gfortran stack variables: **100000000** (`-fmax-stack-var-size=100000000`)
- Intel ifx: Uses `-fstack-arrays` (heap-arrays not available)

### Adding New Source Files
Simply add your new `.f90` files anywhere in the `src/` directory tree. The build system will automatically discover and include them in the next build.

## Build Configurations

### Build Types
- **Release**: Optimized build for production use
- **Debug**: Debug symbols, runtime checks, no optimization
- **RelWithDebInfo**: Optimized with debug symbols

### CMake Presets
You can use CMake presets for common configurations:

```bash
# Configure with preset
cmake --preset=ifort

# Build with preset
cmake --build --preset=ifort

# Available presets
cmake --list-presets
```

## Directory Structure

```
SHETRAN/
├── CMakeLists.txt           # Main CMake configuration
├── CMakePresets.json        # CMake presets for common configurations
├── build.sh                 # Linux build script
├── build.bat                # Windows build script
├── src/                     # Source code
│   ├── Shetran.f90          # Main program
│   ├── compute/
│   │   ├── CMmod.f90
│   │   ├── ETmod.f90
│   │   ├── FRmod.f90
│   │   ├── OCmod.f90
│   │   ├── OCmod2.f90
│   │   ├── OCQDQMOD.f90
│   │   ├── SMmod.f90
│   │   ├── contaminant/
│   │   │   ├── contaminant_column_solver.f90
│   │   │   ├── contaminant_common.f90
│   │   │   ├── contaminant_data_reader.f90
│   │   │   ├── contaminant_link_solver.f90
│   │   │   ├── contaminant_plant.f90
│   │   │   ├── contaminant_simulation.f90
│   │   │   └── contaminant_utilities.f90
│   │   ├── evapotranspiration_inception/
│   │   │   ├── et_core.f90
│   │   │   ├── et_integration.f90
│   │   │   ├── et_main.f90
│   │   │   ├── et_validation.f90
│   │   │   └── et_variables.f90
│   │   ├── execution_control/
│   │   │   ├── framework_component_initialization.f90
│   │   │   ├── framework_element_sorting.f90
│   │   │   ├── framework_initialization.f90
│   │   │   ├── framework_mass_balance.f90
│   │   │   ├── framework_output_manager.f90
│   │   │   ├── framework_shared.f90
│   │   │   └── framework_spatial_setup.f90
│   │   ├── hydraulic_flow/
│   │   │   ├── flow_calculator.f90
│   │   │   ├── hydraulic_helpers.f90
│   │   │   └── hydraulic_variables.f90
│   │   ├── overland_channel/
│   │   │   ├── oc_channel_flow_types.f90
│   │   │   ├── oc_common_data.f90
│   │   │   ├── oc_data_management.f90
│   │   │   ├── oc_flow_control.f90
│   │   │   ├── oc_hydraulic_calculations.f90
│   │   │   ├── oc_initialization.f90
│   │   │   ├── oc_input.f90
│   │   │   ├── oc_matrix_coefficients.f90
│   │   │   ├── oc_node_flows.f90
│   │   │   ├── oc_output.f90
│   │   │   ├── oc_parameters.f90
│   │   │   ├── oc_time_stepping.f90
│   │   │   ├── oc_utils.f90
│   │   │   └── oc_validation.f90
│   │   ├── snow/
│   │   │   ├── snowmelt_calculation.f90
│   │   │   ├── snow_constants.f90
│   │   │   ├── snow_evapotranspiration.f90
│   │   │   ├── snow_initialization.f90
│   │   │   ├── snow_interface.f90
│   │   │   └── snow_variables.f90
│   │   └── water_balance/
│   │       └── water_balance.f90
│   ├── io/
│   │   ├── meteorological_input.f90
│   │   └── simulation_output.f90
│   ├── modules/
│   │   ├── SYmod.f90
│   │   ├── VSmod.f90
│   │   └── ZQmod.f90
│   ├── parameters/
│   │   ├── AL_C.F90, AL_D.f90, AL_G.F90, ... (and 18 other parameter files)
│   │   └── sglobal.f90
│   ├── resource/
│   │   ├── resource.h
│   │   └── Resource1.rc
│   ├── simulation/
│   │   ├── run_sim.f90
│   │   └── timestep_control.f90
│   ├── util/
│   │   ├── getdirqq.f90, getdirqq_portable.f90, ... (and 2 other utility files)
│   │   └── utilsmod.f90
│   └── visualisation/
│       ├── increment_utilities.f90
│       └── ... (and 11 other visualisation files)
└── external/               # External dependencies (Windows)
    ├── Include/            # HDF5 headers and modules
    └── library-files/      # HDF5 libraries
```

## Advanced Usage

### Dependency Analysis Options
The build system provides two methods for ordering Fortran source files:

1. **Automatic dependency analysis** (default, recommended):
   - Analyzes actual module USE/PROVIDES relationships  
   - Automatically handles refactored module dependencies
   - Uses topological sorting for optimal build order
   - Supports complex dependency graphs including the new OC module structure
   
2. **Pattern-based ordering** (fallback):
   - Uses filename patterns and directory structure
   - Handles most dependency cases correctly
   - Updated to support refactored OC modules

The build system automatically detects the refactored overland channel (OC) modules in `src/flow/overland_channel/` and ensures they are built in the correct dependency order:
- `oc_parameters.f90` → `oc_data_management.f90` → `oc_hydraulic_calculations.f90` → `oc_node_flows.f90` → `oc_channel_flow_types.f90` → `oc_flow_control.f90` → `OCmod2.f90`

```bash
# Disable dependency analysis (use pattern-based fallback)
cmake -DENABLE_DEPENDENCY_ANALYSIS=OFF ..

# Enable advanced analysis with verbose output
cmake -DENABLE_DEPENDENCY_ANALYSIS=ON -DVERBOSE_DEPENDENCY_OUTPUT=ON ..
```

### Custom Installation
```bash
# Install to custom location
./build.sh --install -p /opt/shetran

# On Windows
build.bat --install -p "C:\Program Files\SHETRAN"
```

### Parallel Builds
```bash
# Use 8 parallel jobs
./build.sh -j 8
```

**On Windows**, the default `NMake Makefiles` generator used by `build.bat` is single-threaded and does not support parallel builds. The `-j` flag will be ignored. For parallel builds on Windows, consider using the `Ninja` generator with CMake directly:
```cmd
cmake -G "Ninja" ..
ninja
```
### Environment Setup

#### Intel Fortran
If using Intel compilers, make sure to source the Intel environment:

**Linux:**
```bash
source /opt/intel/oneapi/setvars.sh
```

**Windows:**
```cmd
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
```

### Troubleshooting

#### HDF5 Not Found (Linux)
If CMake cannot find HDF5:
1. Install HDF5 development packages
2. Set `HDF5_ROOT` environment variable:
   ```bash
   export HDF5_ROOT=/usr/local
   ./build.sh
   ```

#### Compiler Not Found
If the build script cannot find your compiler:
1. Make sure the compiler is in your PATH
2. Source the compiler environment (Intel compilers)
3. Specify the compiler explicitly:
   ```bash
   ./build.sh -c /path/to/your/compiler
   ```

#### Large Memory Issues
The CMake configuration automatically sets appropriate flags for large memory models. If you still encounter memory issues:

1. **Linux**: Increase stack size:
   ```bash
   ulimit -s unlimited
   ```

2. **Check system limits**: Ensure your system has sufficient memory and virtual memory.

## Running SHETRAN

After building, run SHETRAN with:

```bash
# Linux
./build/bin/shetran -f rundata_file.txt

# Windows  
.\build\bin\shetran.exe -f rundata_file.txt
```

## Examples

The `examples/` directory contains sample input files. Try running:

```bash
cd examples/Cobres
../../build/bin/shetran -f rundata_cob.txt
```

## Support

For build issues, check:
1. Compiler and CMake versions
2. HDF5 installation
3. Environment variables
4. System memory and disk space

For SHETRAN usage, refer to the main documentation and examples.
