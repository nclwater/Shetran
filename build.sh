#!/bin/bash

# SHETRAN Build Script
# This script helps configure and build SHETRAN with different compilers and options

set -e  # Exit on any error

# Default values
BUILD_TYPE="Release"
COMPILER="auto"
BUILD_DIR=""  # Will be set based on build type
INSTALL_PREFIX=""
CLEAN_BUILD=false
VERBOSE=false

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -c, --compiler COMPILER    Specify compiler: ifort, ifx, gfortran, or auto (default: auto)"
    echo "  -t, --type TYPE           Build type: Debug, Release, RelWithDebInfo (default: Release)"
    echo "  -d, --build-dir DIR       Build directory (default: build/<build-type>)"
    echo "  -p, --prefix PREFIX       Installation prefix"
    echo "  --clean                   Clean build directory before building"
    echo "  -v, --verbose             Verbose build output"
    echo "  -j, --jobs N              Number of parallel build jobs (default: number of CPU cores)"
    echo "  --test                    Run tests after building"
    echo "  --install                 Install after building"
    echo "  --disable-large-memory    Disable large memory model (mcmodel=large)"
    echo "  --heap-size SIZE          Set heap array size for Intel ifort (default: 100000000)"
    echo "  --stack-size SIZE         Set stack variable size for gfortran (default: 100000000)"
    echo "  --use-windows-intel-getdirqq  Use Windows Intel-specific getdirqq (Windows+Intel only)"
    echo "  --force-build-hdf5        Force building HDF5 from source (Linux with gfortran only)"
    echo "  --no-system-hdf5          Don't try system HDF5 first, build from source (Linux with gfortran only)"
    echo "  -h, --help                Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                        # Build with auto-detected compiler"
    echo "  $0 -c ifort -t Debug      # Build with Intel ifort compiler in debug mode"
    echo "  $0 -c gfortran --clean    # Clean build with gfortran"
    echo "  $0 --install -p /usr/local # Build and install to /usr/local"
    echo "  $0 --disable-large-memory  # Build without large memory model"
    echo "  $0 --heap-size 50000000   # Build with smaller heap arrays (Intel)"
    echo "  $0 --stack-size 50000000  # Build with smaller stack variables (GNU)"
    echo "  $0 --use-windows-intel-getdirqq  # Use Windows-specific version (Windows+Intel only)"
    echo "  $0 --force-build-hdf5     # Force building HDF5 from source"
    echo "  $0 --no-system-hdf5       # Build HDF5 from source instead of using system version"
}

log_info() {
    echo -e "${BLUE}INFO:${NC} $1"
}

log_success() {
    echo -e "${GREEN}SUCCESS:${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}WARNING:${NC} $1"
}

log_error() {
    echo -e "${RED}ERROR:${NC} $1"
}

log_info_detail() {
    echo -e "${BLUE}INFO:${NC} $1"
}

# Parse command line arguments
JOBS=$(nproc)
RUN_TESTS=false
DO_INSTALL=false
ENABLE_LARGE_MEMORY=true
HEAP_SIZE=""
STACK_SIZE=""
USE_WINDOWS_INTEL_GETDIRQQ=false
FORCE_BUILD_HDF5=false
HDF5_USE_SYSTEM_FIRST=true

while [[ $# -gt 0 ]]; do
    case $1 in
        -c|--compiler)
            COMPILER="$2"
            shift 2
            ;;
        -t|--type)
            BUILD_TYPE="$2"
            shift 2
            ;;
        -d|--build-dir)
            BUILD_DIR="$2"
            shift 2
            ;;
        -p|--prefix)
            INSTALL_PREFIX="$2"
            shift 2
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -j|--jobs)
            JOBS="$2"
            shift 2
            ;;
        --test)
            RUN_TESTS=true
            shift
            ;;
        --install)
            DO_INSTALL=true
            shift
            ;;
        --disable-large-memory)
            ENABLE_LARGE_MEMORY=false
            shift
            ;;
        --heap-size)
            HEAP_SIZE="$2"
            shift 2
            ;;
        --stack-size)
            STACK_SIZE="$2"
            shift 2
            ;;
        --use-windows-intel-getdirqq)
            USE_WINDOWS_INTEL_GETDIRQQ=true
            shift
            ;;
        --force-build-hdf5)
            FORCE_BUILD_HDF5=true
            shift
            ;;
        --no-system-hdf5)
            HDF5_USE_SYSTEM_FIRST=false
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Detect available compilers
detect_compiler() {
    if [[ "$COMPILER" != "auto" ]]; then
        return
    fi
    
    log_info "Auto-detecting Fortran compiler..."
    
    # On Linux, prefer gfortran; on Windows/other platforms, prefer Intel compilers
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        # Linux: prefer gfortran first, then Intel compilers
        if command -v gfortran >/dev/null 2>&1; then
            COMPILER="gfortran"
            log_info "Found GNU Fortran Compiler: gfortran (preferred on Linux)"
        elif command -v ifort >/dev/null 2>&1; then
            COMPILER="ifort"
            log_info "Found Intel Fortran Compiler (Classic): ifort"
        elif command -v ifx >/dev/null 2>&1; then
            COMPILER="ifx"
            log_info "Found Intel Fortran Compiler (LLVM): ifx"
        else
            log_error "No supported Fortran compiler found!"
            log_error "Please install one of: gfortran, ifort, or ifx"
            exit 1
        fi
    else
        # Windows/other platforms: prefer Intel compilers first, then gfortran
        if command -v ifort >/dev/null 2>&1; then
            COMPILER="ifort"
            log_info "Found Intel Fortran Compiler (Classic): ifort"
        elif command -v ifx >/dev/null 2>&1; then
            COMPILER="ifx"
            log_info "Found Intel Fortran Compiler (LLVM): ifx"
        elif command -v gfortran >/dev/null 2>&1; then
            COMPILER="gfortran"
            log_info "Found GNU Fortran Compiler: gfortran"
        else
            log_error "No supported Fortran compiler found!"
            log_error "Please install one of: ifort, ifx, or gfortran"
            exit 1
        fi
    fi
}

# Check for required dependencies
check_dependencies() {
    log_info "Checking dependencies..."
    
    # Check for CMake
    if ! command -v cmake >/dev/null 2>&1; then
        log_error "CMake not found! Please install CMake (version 3.12 or higher)"
        exit 1
    fi
    
    CMAKE_VERSION=$(cmake --version | head -n1 | cut -d' ' -f3)
    log_info "Found CMake version: $CMAKE_VERSION"
    
    # Check for HDF5 on Linux
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if ! pkg-config --exists hdf5; then
            log_warning "HDF5 development packages not found via pkg-config"
            log_warning "Please install HDF5 development packages:"
            log_warning "  Ubuntu/Debian: sudo apt install libhdf5-dev"
            log_warning "  CentOS/RHEL:   sudo yum install hdf5-devel"
            log_warning "  Fedora:        sudo dnf install hdf5-devel"
        else
            HDF5_VERSION=$(pkg-config --modversion hdf5)
            log_info "Found HDF5 version: $HDF5_VERSION"
        fi
    fi
}

# Set up compiler environment
setup_compiler() {
    case $COMPILER in
        ifort|ifx)
            log_info "Setting up Intel Fortran compiler environment..."
            # Try to source Intel environment if available
            if [[ -f /opt/intel/oneapi/setvars.sh ]]; then
                source /opt/intel/oneapi/setvars.sh intel64 > /dev/null 2>&1 || true
            fi
            ;;
        gfortran)
            log_info "Using GNU Fortran compiler..."
            ;;
        *)
            log_error "Unsupported compiler: $COMPILER"
            exit 1
            ;;
    esac
}

# Main build function
build_shetran() {
    log_info "Starting SHETRAN build process..."
    
    # Set BUILD_DIR based on build type if not explicitly set
    if [[ -z "$BUILD_DIR" ]]; then
        case "$BUILD_TYPE" in
            Debug)
                BUILD_DIR="build/debug"
                ;;
            Release)
                BUILD_DIR="build/release"
                ;;
            RelWithDebInfo)
                BUILD_DIR="build/relwithdebinfo"
                ;;
            MinSizeRel)
                BUILD_DIR="build/minsizerel"
                ;;
            *)
                BUILD_DIR="build/$(echo "$BUILD_TYPE" | tr '[:upper:]' '[:lower:]')"
                ;;
        esac
        log_info "Using build directory: $BUILD_DIR"
    fi
    
    # Clean build directory if requested
    if [[ "$CLEAN_BUILD" == "true" ]]; then
        log_info "Cleaning build directory: $BUILD_DIR"
        rm -rf "$BUILD_DIR"
    fi
    
    # Create build directory and remember source path
    SOURCE_DIR="$(pwd)"
    mkdir -p "$BUILD_DIR"
    cd "$BUILD_DIR"
    
    # Calculate relative path back to source directory
    # Count directory levels to determine how many "../" we need
    LEVEL_COUNT=$(echo "$BUILD_DIR" | grep -o "/" | wc -l)
    if [[ "$BUILD_DIR" == */* ]]; then
        # BUILD_DIR has subdirectories, calculate relative path
        SOURCE_PATH=""
        for ((i=0; i<LEVEL_COUNT+1; i++)); do
            SOURCE_PATH="../$SOURCE_PATH"
        done
        # Remove trailing slash
        SOURCE_PATH="${SOURCE_PATH%/}"
    else
        # BUILD_DIR is at same level as source
        SOURCE_PATH=".."
    fi
    
    # Prepare CMake arguments
    CMAKE_ARGS="-DCMAKE_BUILD_TYPE=$BUILD_TYPE"
    
    if [[ -n "$INSTALL_PREFIX" ]]; then
        CMAKE_ARGS="$CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX"
    fi
    
    # Memory configuration options
    if [[ "$ENABLE_LARGE_MEMORY" == "false" ]]; then
        CMAKE_ARGS="$CMAKE_ARGS -DENABLE_LARGE_MEMORY_MODEL=OFF"
    fi
    
    # Getdirqq version selection
    if [[ "$USE_WINDOWS_INTEL_GETDIRQQ" == "true" ]]; then
        CMAKE_ARGS="$CMAKE_ARGS -DUSE_WINDOWS_INTEL_GETDIRQQ=ON"
        log_info "Using Windows Intel-specific getdirqq version"
    else
        log_info "Using portable getdirqq version (cross-platform)"
    fi
    
    if [[ -n "$HEAP_SIZE" ]]; then
        # Validate heap size is numeric
        if [[ "$HEAP_SIZE" =~ ^[0-9]+$ ]]; then
            CMAKE_ARGS="$CMAKE_ARGS -DHEAP_ARRAY_SIZE=$HEAP_SIZE"
        else
            log_error "Heap size must be a positive integer: $HEAP_SIZE"
            exit 1
        fi
    fi
    
    if [[ -n "$STACK_SIZE" ]]; then
        # Validate stack size is numeric
        if [[ "$STACK_SIZE" =~ ^[0-9]+$ ]]; then
            CMAKE_ARGS="$CMAKE_ARGS -DSTACK_VAR_SIZE=$STACK_SIZE"
        else
            log_error "Stack size must be a positive integer: $STACK_SIZE"
            exit 1
        fi
    fi
    
    # Handle HDF5 options
    if [[ "$FORCE_BUILD_HDF5" == "true" ]]; then
        CMAKE_ARGS="$CMAKE_ARGS -DFORCE_BUILD_HDF5=ON"
        log_info "Forcing HDF5 build from source"
    fi
    
    if [[ "$HDF5_USE_SYSTEM_FIRST" == "false" ]]; then
        CMAKE_ARGS="$CMAKE_ARGS -DHDF5_USE_SYSTEM_FIRST=OFF"
        log_info "Disabling system HDF5 search"
    fi
    
    # Set Fortran compiler
    case $COMPILER in
        ifort)
            CMAKE_ARGS="$CMAKE_ARGS -DCMAKE_Fortran_COMPILER=ifort"
            ;;
        ifx)
            CMAKE_ARGS="$CMAKE_ARGS -DCMAKE_Fortran_COMPILER=ifx"
            ;;
        gfortran)
            CMAKE_ARGS="$CMAKE_ARGS -DCMAKE_Fortran_COMPILER=gfortran"
            ;;
    esac
    
    # Configure
    log_info "Configuring with CMake..."
    log_info "CMake arguments: $CMAKE_ARGS"
    log_info_detail "Source files will be automatically discovered from src/ directory"
    log_info_detail "Dependency ordering: Pattern-based (default) or advanced analysis"
    cmake $CMAKE_ARGS "$SOURCE_PATH"
    
    # Build
    log_info "Building SHETRAN with $JOBS parallel jobs..."
    if [[ "$VERBOSE" == "true" ]]; then
        make -j$JOBS VERBOSE=1
    else
        make -j$JOBS
    fi
    
    log_success "Build completed successfully!"
    
    # Run tests if requested
    if [[ "$RUN_TESTS" == "true" ]]; then
        log_info "Running tests..."
        ctest --output-on-failure
    fi
    
    # Install if requested
    if [[ "$DO_INSTALL" == "true" ]]; then
        log_info "Installing SHETRAN..."
        make install
        log_success "Installation completed!"
    fi
    
    # Show build summary
    echo ""
    log_success "SHETRAN build summary:"
    echo "  Compiler:     $COMPILER"
    echo "  Build type:   $BUILD_TYPE"
    echo "  Build dir:    $BUILD_DIR"
    if [[ -n "$INSTALL_PREFIX" ]]; then
        echo "  Install dir:  $INSTALL_PREFIX"
    fi
    echo "  Executable:   $BUILD_DIR/bin/shetran"
    echo ""
    echo "To run SHETRAN:"
    echo "  $BUILD_DIR/bin/shetran -f <rundata_file>"
}

# Main execution
main() {
    log_info "SHETRAN Build Script"
    log_info "==================="
    
    detect_compiler
    check_dependencies
    setup_compiler
    build_shetran
    
    log_success "All done!"
}

# Run main function
main
