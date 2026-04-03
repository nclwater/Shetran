#!/usr/bin/env bash
# SHETRAN Build Script for Linux
# Usage: ./build.sh [OPTIONS]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Default values
BUILD_TYPE="Release"
CLEAN_BUILD=false
CLEAN_APP_ONLY=false
VERBOSE=false
JOBS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
GENERATE_FORD=false
DOCS_ONLY=false
COMPILER="gfortran"

show_usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Options:
  -t, --type TYPE         Build type: Debug, Release, or ReleaseNative (default: Release)
  -c, --compiler COMPILER Compiler: gfortran or ifx (default: gfortran)
  --clean                 Clean build directory before building
  --clean-app             Clean and rebuild SHETRAN only (keep external libraries)
  -v, --verbose           Verbose build output
  -j, --jobs N            Number of parallel jobs (default: ${JOBS})
  --ford                  Generate FORD documentation after a successful build
  --docs-only             Generate FORD documentation only (no compile)
  -h, --help              Show this help message

Examples:
  $(basename "$0")
  $(basename "$0") -t Debug
  $(basename "$0") -t ReleaseNative -c ifx --clean
  $(basename "$0") -t Release --clean-app
  $(basename "$0") --ford
  $(basename "$0") --docs-only

Build Directory Structure:
  Debug builds:    build/debug/
  Release builds:  build/release/
  ReleaseNative:   build/release-native/
  Executable:      build/<type>/bin/shetran
EOF
}

require_command() {
    local cmd="$1"
    local msg="$2"
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "ERROR: ${msg}" >&2
        exit 1
    fi
}

setup_ifx_environment_if_needed() {
    if command -v ifx >/dev/null 2>&1; then
        return 0
    fi

    local candidates=(
        "/opt/intel/oneapi/setvars.sh"
        "$HOME/intel/oneapi/setvars.sh"
    )

    for candidate in "${candidates[@]}"; do
        if [[ -f "$candidate" ]]; then
            # shellcheck disable=SC1090
            source "$candidate" >/dev/null 2>&1 || true
            if command -v ifx >/dev/null 2>&1; then
                return 0
            fi
        fi
    done

    return 1
}

generate_ford_docs() {
    echo "INFO: Generating FORD documentation..."
    require_command "ford" "ford command not found in PATH! Install FORD first (for example: pip install ford)."

    (cd "$SCRIPT_DIR" && ford -o docs/ford ./ford_project.md)
    echo "INFO: FORD documentation generated at docs/ford/index.html"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -t|--type)
            [[ $# -ge 2 ]] || { echo "ERROR: Missing value for $1" >&2; show_usage; exit 1; }
            BUILD_TYPE="$2"
            shift 2
            ;;
        -c|--compiler)
            [[ $# -ge 2 ]] || { echo "ERROR: Missing value for $1" >&2; show_usage; exit 1; }
            COMPILER="$2"
            shift 2
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --clean-app)
            CLEAN_APP_ONLY=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -j|--jobs)
            [[ $# -ge 2 ]] || { echo "ERROR: Missing value for $1" >&2; show_usage; exit 1; }
            JOBS="$2"
            shift 2
            ;;
        --ford)
            GENERATE_FORD=true
            shift
            ;;
        --docs-only)
            DOCS_ONLY=true
            GENERATE_FORD=true
            shift
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        *)
            echo "ERROR: Unknown option: $1" >&2
            show_usage
            exit 1
            ;;
    esac
done

echo "SHETRAN Build Script for Linux"
echo "================================"

cd "$SCRIPT_DIR"

# Validate build type
case "$BUILD_TYPE" in
    Debug|Release|ReleaseNative)
        ;;
    *)
        echo "ERROR: Invalid build type: $BUILD_TYPE" >&2
        echo "ERROR: Must be Debug, Release, or ReleaseNative" >&2
        exit 1
        ;;
esac

# Validate compiler
case "$COMPILER" in
    gfortran|ifx)
        ;;
    *)
        echo "ERROR: Invalid compiler: $COMPILER" >&2
        echo "ERROR: Must be gfortran or ifx" >&2
        exit 1
        ;;
esac

if $CLEAN_BUILD && $CLEAN_APP_ONLY; then
    echo "ERROR: --clean and --clean-app are mutually exclusive" >&2
    exit 1
fi

if $DOCS_ONLY; then
    echo "INFO: Running in documentation-only mode"
    generate_ford_docs
    exit 0
fi

# Check for requested compiler
if [[ "$COMPILER" == "ifx" ]]; then
    echo "INFO: Checking for ifx compiler..."
    if ! setup_ifx_environment_if_needed; then
        echo "ERROR: ifx compiler not found in PATH." >&2
        echo "ERROR: Install Intel oneAPI and ensure ifx is available (or run setvars.sh)." >&2
        exit 1
    fi
    echo "INFO: Found ifx compiler"
else
    echo "INFO: Checking for gfortran compiler..."
    require_command "gfortran" "gfortran compiler not found in PATH!"
    echo "INFO: Found gfortran compiler"
fi

# Check for CMake
require_command "cmake" "CMake not found! Please install CMake and add it to your PATH."
echo "INFO: Found CMake version: $(cmake --version | head -n 1 | awk '{print $3}')"

# Determine build directory
case "$BUILD_TYPE" in
    Debug)
        BUILD_DIR="build/debug"
        ;;
    ReleaseNative)
        BUILD_DIR="build/release-native"
        ;;
    *)
        BUILD_DIR="build/release"
        ;;
esac

echo "INFO: Build type:      $BUILD_TYPE"
echo "INFO: Compiler:        $COMPILER"
echo "INFO: Build directory: $BUILD_DIR"

# Clean build directory if requested
if $CLEAN_BUILD; then
    echo "INFO: Cleaning build directory: $BUILD_DIR"
    rm -rf "$BUILD_DIR"
fi

# Clean only SHETRAN build artifacts if requested
if $CLEAN_APP_ONLY; then
    if [[ -d "$BUILD_DIR" ]]; then
        echo "INFO: Cleaning SHETRAN artifacts in: $BUILD_DIR"
        rm -f "$BUILD_DIR/bin/shetran"
        rm -rf "$BUILD_DIR/CMakeFiles/SHETRAN.dir"
        find "$BUILD_DIR" -maxdepth 1 -type f \( -name "*.mod" -o -name "*.smod" \) -delete
    else
        echo "INFO: Build directory does not exist yet. --clean-app has nothing to clean."
    fi
fi

mkdir -p "$BUILD_DIR"
pushd "$BUILD_DIR" >/dev/null

SOURCE_PATH="../.."

echo "INFO: Configuring with CMake..."
CMAKE_ARGS=(
    -DCMAKE_BUILD_TYPE="$BUILD_TYPE"
    -DCMAKE_Fortran_COMPILER="$COMPILER"
)
echo "INFO: CMake arguments: ${CMAKE_ARGS[*]}"
cmake "${CMAKE_ARGS[@]}" "$SOURCE_PATH"

echo "INFO: Building SHETRAN..."
BUILD_ARGS=(--build . --target SHETRAN --parallel "$JOBS")
if $VERBOSE; then
    BUILD_ARGS+=(--verbose)
fi
cmake "${BUILD_ARGS[@]}"

popd >/dev/null

echo
echo "SUCCESS: Build completed successfully!"
echo
echo "  Compiler:     $COMPILER"
echo "  Build type:   $BUILD_TYPE"
echo "  Build dir:    $BUILD_DIR"
echo "  Executable:   $BUILD_DIR/bin/shetran"
echo

if $GENERATE_FORD; then
    generate_ford_docs
fi
