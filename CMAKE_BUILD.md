# SHETRAN CMake Build System

This directory contains a CMake-based build system for SHETRAN that supports multiple compilers and platforms with automatic source file discovery and dependency ordering.

## Key Features

- **Automatic Source Discovery**: Automatically finds all Fortran source files in the `src/` directory
- **Intelligent Dependency Ordering**: Orders source files based on module dependencies
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
   
   # Generate documentation
   ford ford_project.md
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
The build system uses **automatic dependency analysis** to determine the correct compilation order. It analyzes the `MODULE` and `USE` statements in your Fortran source files to create a dependency graph and ensure files are compiled in the correct sequence. This process is fully automatic and is a required part of the build.

You can enable verbose output during the analysis to see the discovered dependencies:

```bash
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

## HDF5 Dependency Management

The build system includes sophisticated HDF5 handling that automatically adapts based on your platform and compiler:

### HDF5 Strategy by Platform and Compiler

**Linux with gfortran**:
- **Default behavior**: Try system HDF5 first, then download and build if not found
- **Rationale**: System HDF5 is typically well-integrated with gfortran on Linux distributions

**Linux with Intel Fortran (ifort/ifx)**:
- **Default behavior**: Always download and build HDF5 from source
- **Rationale**: Ensures compatibility between Intel Fortran and HDF5

**Windows (any compiler)**:
- **Default behavior**: Use provided libraries if available, otherwise download and build
- **Rationale**: Windows typically lacks package managers for easy HDF5 installation

### HDF5 Build Options

You can customize the HDF5 handling using these options:

```bash
# Force building HDF5 from source (Linux with gfortran only)
./build.sh --force-build-hdf5

# Don't try system HDF5, build from source (Linux with gfortran only)
./build.sh --no-system-hdf5

# Using CMake directly
cmake -DFORCE_BUILD_HDF5=ON ..
cmake -DHDF5_USE_SYSTEM_FIRST=OFF ..
```

### Automatic C Compiler Selection

When building HDF5 from source, the build system automatically selects an appropriate C compiler to match your Fortran compiler:

- **gfortran** → **gcc** (preferred) or **clang**
- **Intel ifort/ifx on Linux** → **icc/icx** (preferred) or **clang**
- **Intel ifort/ifx on Windows** → **icl/clang-cl** (preferred)
- **Other compilers** → **clang** (fallback)

### HDF5 Features

- **Version**: HDF5 1.14.3 (when built from source)
- **Components**: Fortran interface, High-Level (HL) library
- **H5IM Palette Support**: Automatically detected and enabled when available

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

## Advanced Usage

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

## Documentation System

SHETRAN uses FORD (FORtran Documenter) to generate comprehensive API documentation from source code comments. The documentation system creates browsable HTML documentation with module dependency graphs, procedure listings, and cross-references.

### Requirements

To generate documentation, you need:

1. **FORD and Graphviz**: Install both in the same Python environment
   ```bash
   pip install ford graphviz
   ```

   This installs both the FORD documentation generator and the Python Graphviz package, which provides the necessary tools for generating dependency graphs and diagrams.

### Generating Documentation

#### Using VS Code Tasks
The easiest way to generate documentation is using the pre-configured VS Code task:

1. Open the Command Palette (`Ctrl+Shift+P` or `Cmd+Shift+P`)
2. Run `Tasks: Run Task`
3. Select `Generate FORD Documentation`

#### Using the Command Line
```bash
# Generate documentation from the project root
ford ford_project.md
```

The generated documentation will be available in `docs/ford/index.html`.

#### Using the Build Script
The documentation task is also available as a build target:
```bash
# Linux/macOS
./build.sh --docs

# Or use the task runner directly
```

### Documentation Configuration

The documentation is configured through `ford_project.md`, which includes:

- **Project metadata**: Version, author, license information
- **Source directories**: Automatically scans `src/` directory
- **Output location**: `docs/ford/`
- **Graph generation**: Enabled for module dependency visualization
- **Search functionality**: Full-text search across documentation
- **Extensions**: Enhanced with table of contents and smart typography

### Documentation Coverage

SHETRAN includes tools to monitor documentation quality:

```bash
# Check documentation coverage
./tools/check_doc_coverage.sh

# Generate documentation template for new modules
./tools/generate_doc_template.sh ModuleName > src/path/ModuleName.f90
```

The project targets >95% documentation coverage for all public interfaces.

### Documentation Standards

All SHETRAN modules follow FORD documentation standards:

- **Module headers**: Brief description, author, date, version
- **Detailed descriptions**: Purpose, algorithms, usage patterns
- **Parameter documentation**: All public variables and procedures
- **Usage examples**: Where appropriate
- **Cross-references**: Links to related modules and procedures

Example documentation format:
```fortran
!> @file module_name.f90
!> @brief Brief description of module functionality
!> @author Author Name, Institution  
!> @date 2025-09-05
!> @version 1.0
!>
!> @details
!! Detailed description of the module.
!! Include purpose, algorithms, and usage patterns.
!!
!! This module provides:
!! - Feature 1: Description
!! - Feature 2: Description
!>
!> @note Implementation notes
!> @warning Usage warnings
!> @see Related modules

module module_name
   implicit none
   private

   real, parameter :: CONSTANT = 1.0  !< Description of constant
```

### Accessing Documentation

After generation, the documentation provides:

- **Module listings**: All modules with dependency relationships
- **Procedure index**: Searchable list of all public procedures
- **Type definitions**: User-defined types and their components
- **Dependency graphs**: Visual representation of module relationships
- **Source code browser**: Syntax-highlighted source with cross-links
- **Search functionality**: Full-text search across all documentation

Open `docs/ford/index.html` in a web browser to browse the generated documentation.

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

#### HDF5 Linking Issues
If you encounter problems linking SHETRAN against system HDF5 libraries (especially with Intel compilers), try building HDF5 locally:

```bash
# Force building HDF5 from source instead of using system libraries
./build.sh --force-build-hdf5

# Or using CMake directly
cmake -DFORCE_BUILD_HDF5=ON ..
```

This ensures HDF5 is compiled with the same compiler as SHETRAN, avoiding compatibility issues.

For documentation generation issues:
1. Ensure FORD is installed (`pip install ford`)
2. Ensure Graphviz is installed for dependency graphs
3. Check that `ford_project.md` configuration is valid
4. Verify source files contain proper FORD documentation comments

For SHETRAN usage, refer to the main documentation and examples.
