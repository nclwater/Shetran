# SHETRAN CMake Build System

This directory contains a CMake-based build system for SHETRAN that supports multiple compilers and platforms.

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
   REM Build with Intel Fortran (default)
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

## Compiler-Specific Settings

### Intel Fortran (ifort)
- Uses `-mcmodel=large` for large memory models
- Uses `-heap-arrays 100000000` for large array handling
- Optimized with `-O3` for release builds

### Intel Fortran (ifx)
- Uses `-mcmodel=large` for large memory models  
- Uses `-fstack-arrays` instead of `-heap-arrays` (not available in ifx)
- Optimized with `-O3` for release builds

### GNU Fortran (gfortran)
- Uses `-mcmodel=large` for large memory models
- Uses `-fmax-stack-var-size=100000000` for large arrays
- Optimized with `-O3` for release builds

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
├── build.bat               # Windows build script
├── src/                    # Source code
│   ├── Shetran.f90         # Main program
│   ├── modules/            # Fortran modules
│   ├── parameters/         # Parameter modules
│   ├── util/               # Utility modules
│   └── visualisation/      # Visualization modules
└── external/               # External dependencies (Windows)
    ├── Include/            # HDF5 headers and modules
    └── library-files/      # HDF5 libraries
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

# On Windows
build.bat -j 8
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
