cmake_minimum_required(VERSION 3.20)

# Default to ifx on Linux (fallback: gfortran) unless user explicitly selected a compiler.
if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux" AND NOT DEFINED CMAKE_Fortran_COMPILER)
    find_program(DEFAULT_IFX ifx)
    if(DEFAULT_IFX)
        set(CMAKE_Fortran_COMPILER "${DEFAULT_IFX}" CACHE FILEPATH "Fortran compiler" FORCE)
        message(STATUS "Defaulting Fortran compiler to ifx on Linux: ${CMAKE_Fortran_COMPILER}")
    else()
        find_program(DEFAULT_GFORTRAN gfortran)
        if(DEFAULT_GFORTRAN)
            set(CMAKE_Fortran_COMPILER "${DEFAULT_GFORTRAN}" CACHE FILEPATH "Fortran compiler" FORCE)
            message(STATUS "ifx not found; defaulting Fortran compiler to gfortran on Linux: ${CMAKE_Fortran_COMPILER}")
        else()
            message(WARNING "Neither ifx nor gfortran found on PATH; CMake will use its default Fortran compiler selection.")
        endif()
    endif()
endif()

project(SHETRAN VERSION 4.6.4.0 LANGUAGES Fortran C)

# Windows executable metadata is compiled from a generated resource script.
# Keep RC Windows-only so Linux builds do not need a resource compiler.
if(WIN32)
    enable_language(RC)
endif()

# =============================================================================
# Build type
# =============================================================================
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build (Debug or Release)" FORCE)
endif()

set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release ReleaseNative)

# Expose custom configuration to multi-config generators (e.g. Visual Studio)
if(CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_CONFIGURATION_TYPES "Debug;Release;ReleaseNative" CACHE STRING "Available build types" FORCE)
endif()

# =============================================================================
# Compiler support and platform defaults
# =============================================================================
if(WIN32)
    if(NOT (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" OR CMAKE_Fortran_COMPILER_ID STREQUAL "Intel"))
        message(FATAL_ERROR
            "Unsupported Fortran compiler on Windows: ${CMAKE_Fortran_COMPILER_ID}\n"
            "Currently only Intel ifx is supported on Windows.")
    endif()
elseif(UNIX AND NOT APPLE)
    if(NOT (
        CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" OR
        CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR
        CMAKE_Fortran_COMPILER_ID STREQUAL "GNU"))
        message(FATAL_ERROR
            "Unsupported Fortran compiler on Linux: ${CMAKE_Fortran_COMPILER_ID}\n"
            "Supported compilers: ifx (IntelLLVM/Intel) and gfortran (GNU).")
    endif()
else()
    message(FATAL_ERROR "Unsupported platform: this project currently supports Windows and Linux.")
endif()

set(IS_INTEL_FORTRAN OFF)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" OR CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(IS_INTEL_FORTRAN ON)
endif()

# Enable link-time optimization for every target in Release builds.  Checking
# both project languages ensures that this works for the mixed Fortran/C
# targets used by SHETRAN and fortran_stdlib on all supported toolchains.
include(CheckIPOSupported)
check_ipo_supported(
    RESULT SHETRAN_IPO_SUPPORTED
    OUTPUT SHETRAN_IPO_ERROR
    LANGUAGES Fortran C
)
if(NOT SHETRAN_IPO_SUPPORTED)
    message(FATAL_ERROR
        "Link-time optimization is required for Release builds but is not supported:\n"
        "${SHETRAN_IPO_ERROR}")
endif()
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE ON)
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASENATIVE ON)

# Define flags used by the custom ReleaseNative build type.
set(CMAKE_Fortran_FLAGS_RELEASENATIVE "" CACHE STRING "Flags used by the ReleaseNative build type" FORCE)

if(IS_INTEL_FORTRAN)
    # G-format descriptor warning suppression (ifx warning #8291)
    if(WIN32)
        set(INTEL_G_FORMAT_WARN_SUPPRESS_FLAG "/Qdiag-disable:8291")
    else()
        set(INTEL_G_FORMAT_WARN_SUPPRESS_FLAG "-diag-disable=8291")
    endif()

    if(WIN32)
        # Remove the default /O2 that CMake adds for Release to avoid duplicate /O2
        # in Release and conflicts with /O3 in ReleaseNative.
        string(REPLACE "/O2" "" CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}")
    endif()
else()
    set(INTEL_G_FORMAT_WARN_SUPPRESS_FLAG "")
endif()

set(CMAKE_C_STANDARD 99)

# =============================================================================
# Build options
# =============================================================================
option(ENABLE_DEPENDENCY_ANALYSIS "Enable automatic Fortran module dependency analysis" ON)
option(VERBOSE_DEPENDENCY_OUTPUT "Enable verbose output during dependency analysis" OFF)
option(STATIC_RUNTIME "Link Intel and MSVC runtimes statically (portable .exe, no oneAPI on PATH required)" ON)
option(ENABLE_FORD_DOCS "Enable FORD documentation target (ford_docs)" OFF)
option(ENABLE_QUICKWIN "Enable Intel QuickWin file picker for no-argument and -a Windows runs" ON)
option(SHETRAN_BUILD_TESTS "Build SHETRAN parser tests" OFF)

set(SHETRAN_ENABLE_QUICKWIN OFF)
if(ENABLE_QUICKWIN AND WIN32 AND IS_INTEL_FORTRAN)
    set(SHETRAN_ENABLE_QUICKWIN ON)
elseif(ENABLE_QUICKWIN)
    message(WARNING "ENABLE_QUICKWIN requires Intel Fortran on Windows; disabling QuickWin support.")
endif()

# =============================================================================
# Runtime linking (Windows/MSVC only)
# =============================================================================
if(WIN32)
    if(STATIC_RUNTIME)
        # /MT — statically link MSVC C runtime; applied automatically to all targets
        # via CMAKE_MSVC_RUNTIME_LIBRARY (CMake 3.15+)
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreadedDebug" CACHE STRING "" FORCE)
            set(HDF5_MSVC_RUNTIME "MultiThreadedDebug")
        else()
            set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded" CACHE STRING "" FORCE)
            set(HDF5_MSVC_RUNTIME "MultiThreaded")
        endif()
        message(STATUS "Runtime linking: Static (/libs:static + /MT) — portable executable")
    else()
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            set(HDF5_MSVC_RUNTIME "MultiThreadedDebugDLL")
        else()
            set(HDF5_MSVC_RUNTIME "MultiThreadedDLL")
        endif()
        message(STATUS "Runtime linking: Dynamic — requires Intel oneAPI on PATH to run")
    endif()
endif()

# =============================================================================
# Optional FORD documentation target
# =============================================================================
if(ENABLE_FORD_DOCS)
    find_program(FORD_EXECUTABLE NAMES ford)
    if(NOT FORD_EXECUTABLE)
        message(FATAL_ERROR
            "ENABLE_FORD_DOCS is ON, but 'ford' was not found in PATH.\n"
            "Install FORD or configure with -DENABLE_FORD_DOCS=OFF.")
    endif()

    add_custom_target(ford_docs
        COMMAND ${FORD_EXECUTABLE} -o ${CMAKE_SOURCE_DIR}/docs/ford ${CMAKE_SOURCE_DIR}/ford_project.md
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Generating FORD documentation"
        VERBATIM
    )
endif()

# =============================================================================
# HDF5 from source
# =============================================================================
include(ExternalProject)

if(WIN32)
    set(STATIC_LIBRARY_EXT ".lib")
else()
    set(STATIC_LIBRARY_EXT ".a")
endif()

set(HDF5_LOCAL_TARBALL "${CMAKE_SOURCE_DIR}/external/tarballs/hdf5-1.14.6.tar.gz")
if(EXISTS ${HDF5_LOCAL_TARBALL})
    set(HDF5_URL "file://${HDF5_LOCAL_TARBALL}")
    message(STATUS "Using local HDF5 tarball: ${HDF5_LOCAL_TARBALL}")
else()
    message(FATAL_ERROR
        "HDF5 tarball not found at: ${HDF5_LOCAL_TARBALL}\n"
        "Please place hdf5-1.14.6.tar.gz in external/tarballs/")
endif()

# HDF5 debug static libraries are platform-specific:
# - Windows builds typically use _D
# - Linux/macOS builds use _debug
if(CMAKE_CONFIGURATION_TYPES)
    # Multi-config generators choose the build type at build time, so
    # CMAKE_BUILD_TYPE cannot be used to select one imported library here.
    if(WIN32)
        set(HDF5_DEBUG_LIB_POSTFIX "_D")
    else()
        set(HDF5_DEBUG_LIB_POSTFIX "_debug")
    endif()
    set(HDF5_LIB_POSTFIX "")
    set(HDF5_CMAKE_BUILD_TYPE "Release")
elseif(CMAKE_BUILD_TYPE STREQUAL "Debug")
    if(WIN32)
        set(HDF5_LIB_POSTFIX "_D")
    else()
        set(HDF5_LIB_POSTFIX "_debug")
    endif()
    set(HDF5_CMAKE_BUILD_TYPE "Debug")
else()
    set(HDF5_LIB_POSTFIX "")
    # HDF5 only uses standard build types; map non-Debug SHETRAN builds to Release.
    set(HDF5_CMAKE_BUILD_TYPE "Release")
endif()

set(HDF5_STATIC_EXT "${STATIC_LIBRARY_EXT}")

# Build the pinned zlib-ng source in zlib-compatibility mode before HDF5 so
# that DEFLATE support is available consistently on Windows and Linux without
# relying on a system installation.
set(ZLIB_NG_LOCAL_TARBALL "${CMAKE_SOURCE_DIR}/external/tarballs/zlib-ng-2.3.3.tar.gz")
if(EXISTS ${ZLIB_NG_LOCAL_TARBALL})
    set(ZLIB_NG_URL "file://${ZLIB_NG_LOCAL_TARBALL}")
    message(STATUS "Using local zlib-ng tarball: ${ZLIB_NG_LOCAL_TARBALL}")
else()
    message(FATAL_ERROR
        "zlib-ng tarball not found at: ${ZLIB_NG_LOCAL_TARBALL}\n"
        "Please place zlib-ng-2.3.3.tar.gz in external/tarballs/")
endif()

set(ZLIB_NG_STAGE_DIR "${CMAKE_BINARY_DIR}/zlib-ng-install")

if(WIN32)
    set(ZLIB_NG_LIB_NAME "zlibstatic")
    set(ZLIB_NG_DEBUG_LIB_NAME "zlibstaticd")
    set(ZLIB_NG_LIB_PREFIX "")
else()
    set(ZLIB_NG_LIB_NAME "z")
    set(ZLIB_NG_DEBUG_LIB_NAME "z")
    set(ZLIB_NG_LIB_PREFIX "lib")
endif()

set(ZLIB_NG_CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${HDF5_CMAKE_BUILD_TYPE}
    -DBUILD_SHARED_LIBS=OFF
    -DBUILD_TESTING=OFF
    -DZLIB_COMPAT=ON
    -DWITH_GTEST=OFF
    -DWITH_FUZZERS=OFF
    -DWITH_BENCHMARKS=OFF
    -DWITH_BENCHMARK_APPS=OFF
    -DINSTALL_UTILS=OFF
    -DWITH_NATIVE_INSTRUCTIONS=OFF
    -DWITH_RUNTIME_CPU_DETECTION=ON
    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
)

if(WIN32)
    list(APPEND ZLIB_NG_CMAKE_ARGS -DCMAKE_MSVC_RUNTIME_LIBRARY=${HDF5_MSVC_RUNTIME})
endif()

ExternalProject_Add(ZLIB_NG_external
    URL ${ZLIB_NG_URL}
    URL_HASH SHA256=f9c65aa9c852eb8255b636fd9f07ce1c406f061ec19a2e7d508b318ca0c907d1
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    INSTALL_DIR ${ZLIB_NG_STAGE_DIR}
    CMAKE_CACHE_ARGS
        -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        -DCMAKE_INSTALL_LIBDIR:PATH=lib
        -DCMAKE_INSTALL_BINDIR:PATH=bin
        -DCMAKE_INSTALL_INCLUDEDIR:PATH=include
    CMAKE_ARGS
        ${ZLIB_NG_CMAKE_ARGS}
    INSTALL_COMMAND
        ${CMAKE_COMMAND} --install <BINARY_DIR> --prefix <INSTALL_DIR> --config $<CONFIG>
    BUILD_BYPRODUCTS
        ${ZLIB_NG_STAGE_DIR}/lib/${ZLIB_NG_LIB_PREFIX}${ZLIB_NG_LIB_NAME}${HDF5_STATIC_EXT}
)

# Always stage HDF5 into the project build tree; never rely on system install paths.
set(HDF5_STAGE_DIR "${CMAKE_BINARY_DIR}/hdf5-install")

set(HDF5_CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${HDF5_CMAKE_BUILD_TYPE}
    -DHDF5_BUILD_FORTRAN=ON
    -DHDF5_BUILD_HL_LIB=ON
    -DHDF5_BUILD_EXAMPLES=OFF
    -DHDF5_BUILD_TESTING=OFF
    -DHDF5_BUILD_TOOLS=OFF
    -DBUILD_TESTING=OFF
    -DHDF5_ENABLE_TESTS=OFF
    -DHDF5_ENABLE_EXAMPLES=OFF
    -DHDF5_TEST_FORTRAN=OFF
    -DHDF5_BUILD_FORTRAN_TESTS=OFF
    -DHDF5_ENABLE_SHARED_LIB=OFF
    -DHDF5_ENABLE_STATIC_LIB=ON
    -DBUILD_SHARED_LIBS=OFF
    -DHDF5_ENABLE_SZIP_SUPPORT=OFF
    -DHDF5_ENABLE_Z_LIB_SUPPORT=ON
    -DHDF5_USE_ZLIB_STATIC=ON
    -DZLIB_USE_STATIC_LIBS=ON
    -DZLIB_ROOT=${ZLIB_NG_STAGE_DIR}
    -DCMAKE_PREFIX_PATH=${ZLIB_NG_STAGE_DIR}
    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
    -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
)

if(WIN32)
    list(APPEND HDF5_CMAKE_ARGS -DCMAKE_MSVC_RUNTIME_LIBRARY=${HDF5_MSVC_RUNTIME})
endif()

ExternalProject_Add(HDF5_external
    DEPENDS ZLIB_NG_external
    URL ${HDF5_URL}
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    INSTALL_DIR ${HDF5_STAGE_DIR}
    CMAKE_CACHE_ARGS
        -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        -DCMAKE_INSTALL_LIBDIR:PATH=lib
        -DCMAKE_INSTALL_BINDIR:PATH=bin
        -DCMAKE_INSTALL_INCLUDEDIR:PATH=include
    CMAKE_ARGS
        ${HDF5_CMAKE_ARGS}
    INSTALL_COMMAND
        ${CMAKE_COMMAND} --install <BINARY_DIR> --prefix <INSTALL_DIR> --config $<CONFIG>
    BUILD_BYPRODUCTS
        ${HDF5_STAGE_DIR}/lib/libhdf5${HDF5_LIB_POSTFIX}${HDF5_STATIC_EXT}
        ${HDF5_STAGE_DIR}/lib/libhdf5_f90cstub${HDF5_LIB_POSTFIX}${HDF5_STATIC_EXT}
        ${HDF5_STAGE_DIR}/lib/libhdf5_fortran${HDF5_LIB_POSTFIX}${HDF5_STATIC_EXT}
        ${HDF5_STAGE_DIR}/lib/libhdf5_hl${HDF5_LIB_POSTFIX}${HDF5_STATIC_EXT}
        ${HDF5_STAGE_DIR}/lib/libhdf5_hl_f90cstub${HDF5_LIB_POSTFIX}${HDF5_STATIC_EXT}
        ${HDF5_STAGE_DIR}/lib/libhdf5_hl_fortran${HDF5_LIB_POSTFIX}${HDF5_STATIC_EXT}
)

set(HDF5_INCLUDE_DIRS "${HDF5_STAGE_DIR}/include")
set(HDF5_LIB_DIR "${HDF5_STAGE_DIR}/lib")

add_library(zlib_ng::zlib STATIC IMPORTED)
if(CMAKE_CONFIGURATION_TYPES)
    set_target_properties(zlib_ng::zlib PROPERTIES
        IMPORTED_CONFIGURATIONS "Debug;Release;ReleaseNative"
        IMPORTED_LOCATION_DEBUG
            "${ZLIB_NG_STAGE_DIR}/lib/${ZLIB_NG_LIB_PREFIX}${ZLIB_NG_DEBUG_LIB_NAME}${HDF5_STATIC_EXT}"
        IMPORTED_LOCATION_RELEASE
            "${ZLIB_NG_STAGE_DIR}/lib/${ZLIB_NG_LIB_PREFIX}${ZLIB_NG_LIB_NAME}${HDF5_STATIC_EXT}"
        IMPORTED_LOCATION_RELEASENATIVE
            "${ZLIB_NG_STAGE_DIR}/lib/${ZLIB_NG_LIB_PREFIX}${ZLIB_NG_LIB_NAME}${HDF5_STATIC_EXT}"
    )
else()
    set_target_properties(zlib_ng::zlib PROPERTIES
        IMPORTED_LOCATION
            "${ZLIB_NG_STAGE_DIR}/lib/${ZLIB_NG_LIB_PREFIX}${ZLIB_NG_LIB_NAME}${HDF5_STATIC_EXT}"
    )
endif()

# Create IMPORTED targets for each HDF5 library so that nmake does not try to
# treat the .lib file paths as makefile targets to build.
foreach(_hdf5_lib
    libhdf5_hl_fortran
    libhdf5_hl_f90cstub
    libhdf5_hl
    libhdf5_fortran
    libhdf5_f90cstub
    libhdf5)
    add_library(hdf5::${_hdf5_lib} STATIC IMPORTED)
    if(CMAKE_CONFIGURATION_TYPES)
        set_target_properties(hdf5::${_hdf5_lib} PROPERTIES
            IMPORTED_CONFIGURATIONS "Debug;Release;ReleaseNative"
            IMPORTED_LOCATION_DEBUG
                "${HDF5_LIB_DIR}/${_hdf5_lib}${HDF5_DEBUG_LIB_POSTFIX}${HDF5_STATIC_EXT}"
            IMPORTED_LOCATION_RELEASE
                "${HDF5_LIB_DIR}/${_hdf5_lib}${HDF5_STATIC_EXT}"
            IMPORTED_LOCATION_RELEASENATIVE
                "${HDF5_LIB_DIR}/${_hdf5_lib}${HDF5_STATIC_EXT}"
        )
    else()
        set_target_properties(hdf5::${_hdf5_lib} PROPERTIES
            IMPORTED_LOCATION
                "${HDF5_LIB_DIR}/${_hdf5_lib}${HDF5_LIB_POSTFIX}${HDF5_STATIC_EXT}"
        )
    endif()
endforeach()
# add_dependencies on IMPORTED targets is not supported; the dependency is
# established via add_dependencies(SHETRAN HDF5_external) further below.

set(HDF5_LIBRARIES
    hdf5::libhdf5_hl_fortran
    hdf5::libhdf5_hl_f90cstub
    hdf5::libhdf5_hl
    hdf5::libhdf5_fortran
    hdf5::libhdf5_f90cstub
    hdf5::libhdf5
    zlib_ng::zlib
)

if(WIN32)
    list(APPEND HDF5_LIBRARIES shlwapi)
endif()

# =============================================================================
# Fortran stdlib from source
# =============================================================================
include(FetchContent)

set(STDLIB_LOCAL_TARBALL "${CMAKE_SOURCE_DIR}/external/tarballs/stdlib-0.8.1.tar.gz")
if(EXISTS ${STDLIB_LOCAL_TARBALL})
    set(STDLIB_URL "file://${STDLIB_LOCAL_TARBALL}")
    message(STATUS "Using local Fortran stdlib tarball: ${STDLIB_LOCAL_TARBALL}")
else()
    message(FATAL_ERROR
        "Fortran stdlib tarball not found at: ${STDLIB_LOCAL_TARBALL}\n"
        "Please place stdlib-0.8.1.tar.gz in external/tarballs/")
endif()

set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build shared libraries" FORCE)
set(BUILD_TESTING OFF CACHE BOOL "Build tests" FORCE)
set(FIND_BLAS OFF CACHE BOOL "Find external BLAS and LAPACK" FORCE)

set(STDLIB_ANSI OFF CACHE BOOL "Compile STDLIB ANSI" FORCE)
set(STDLIB_BITSETS OFF CACHE BOOL "Compile STDLIB BITSETS" FORCE)
set(STDLIB_HASHMAPS OFF CACHE BOOL "Compile STDLIB HASHMAPS" FORCE)
set(STDLIB_IO OFF CACHE BOOL "Compile STDLIB IO" FORCE)
set(STDLIB_LINALG_ITERATIVE OFF CACHE BOOL "Compile STDLIB LINALG_ITERATIVE" FORCE)
set(STDLIB_LOGGER OFF CACHE BOOL "Compile STDLIB LOGGER" FORCE)
set(STDLIB_QUADRATURE OFF CACHE BOOL "Compile STDLIB QUADRATURE" FORCE)
set(STDLIB_SPECIALMATRICES OFF CACHE BOOL "Compile STDLIB SPECIALMATRICES" FORCE)
set(STDLIB_STRINGLIST OFF CACHE BOOL "Compile STDLIB STRINGLIST" FORCE)
set(STDLIB_STATS OFF CACHE BOOL "Compile STDLIB STATS" FORCE)
set(STDLIB_SYSTEM ON CACHE BOOL "Compile STDLIB SYSTEM" FORCE)

# Visual Studio normally starts without an activated Conda environment.  Find
# fypp in the environment used for SHETRAN and cache its absolute path so the
# generated custom-build commands work from both the IDE and the command line.
set(SHETRAN_CONDA_ENV "shetran" CACHE STRING
    "Conda environment from which to use the fypp preprocessor")

if(NOT FYPP)
    set(_conda_environment_roots)

    if(DEFINED ENV{CONDA_PREFIX})
        get_filename_component(_active_conda_name "$ENV{CONDA_PREFIX}" NAME)
        if(_active_conda_name STREQUAL SHETRAN_CONDA_ENV)
            list(APPEND _conda_environment_roots "$ENV{CONDA_PREFIX}")
        endif()
        list(APPEND _conda_environment_roots
            "$ENV{CONDA_PREFIX}/envs/${SHETRAN_CONDA_ENV}")
    endif()

    if(DEFINED ENV{CONDA_EXE})
        get_filename_component(_conda_scripts_dir "$ENV{CONDA_EXE}" DIRECTORY)
        get_filename_component(_conda_base_dir "${_conda_scripts_dir}" DIRECTORY)
        list(APPEND _conda_environment_roots
            "${_conda_base_dir}/envs/${SHETRAN_CONDA_ENV}")
    endif()

    if(DEFINED ENV{USERPROFILE})
        set(_conda_environments_file "$ENV{USERPROFILE}/.conda/environments.txt")
        if(EXISTS "${_conda_environments_file}")
            file(STRINGS "${_conda_environments_file}" _known_conda_environments)
            foreach(_conda_environment IN LISTS _known_conda_environments)
                get_filename_component(_conda_environment_name
                    "${_conda_environment}" NAME)
                if(_conda_environment_name STREQUAL SHETRAN_CONDA_ENV)
                    list(APPEND _conda_environment_roots "${_conda_environment}")
                endif()
            endforeach()
        endif()

        list(APPEND _conda_environment_roots
            "$ENV{USERPROFILE}/miniforge3/envs/${SHETRAN_CONDA_ENV}"
            "$ENV{USERPROFILE}/miniconda3/envs/${SHETRAN_CONDA_ENV}"
            "$ENV{USERPROFILE}/anaconda3/envs/${SHETRAN_CONDA_ENV}"
        )
    endif()

    list(REMOVE_DUPLICATES _conda_environment_roots)
    find_program(FYPP
        NAMES fypp fypp.exe
        HINTS ${_conda_environment_roots}
        PATH_SUFFIXES Scripts bin
        NO_DEFAULT_PATH
        DOC "Path to the fypp preprocessor"
    )
    if(NOT FYPP)
        find_program(FYPP NAMES fypp fypp.exe
            DOC "Path to the fypp preprocessor")
    endif()

    unset(_active_conda_name)
    unset(_conda_scripts_dir)
    unset(_conda_base_dir)
    unset(_conda_environments_file)
    unset(_known_conda_environments)
    unset(_conda_environment)
    unset(_conda_environment_name)
    unset(_conda_environment_roots)
endif()

if(NOT FYPP)
    message(FATAL_ERROR
        "fypp was not found in Conda environment '${SHETRAN_CONDA_ENV}'.\n"
        "Install it in that environment or configure with "
        "-DFYPP=<absolute-path-to-fypp>.")
endif()
message(STATUS "Using fypp: ${FYPP}")

FetchContent_Declare(fortran_stdlib
    URL ${STDLIB_URL}
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    PATCH_COMMAND
        ${CMAKE_COMMAND}
        -DSTDLIB_SOURCE_DIR=<SOURCE_DIR>
        -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/PatchFortranStdlib.cmake
)

FetchContent_MakeAvailable(fortran_stdlib)

# A Visual Studio project can use either the C/C++ project system or Intel's
# Fortran project system, but not both.  stdlib 0.8.1 puts C helpers and
# Fortran modules in one target, causing CMake to generate a .vcxproj in which
# the Fortran sources are non-compiling items.  Build the C helpers as an
# object library and archive those objects into the Fortran target instead.
if(CMAKE_GENERATOR MATCHES "^Visual Studio" AND TARGET fortran_stdlib_system)
    set(_stdlib_system_source_dir "${fortran_stdlib_SOURCE_DIR}/src/system")

    set_property(TARGET fortran_stdlib_system PROPERTY SOURCES
        "${_stdlib_system_source_dir}/stdlib_system.F90"
        "${_stdlib_system_source_dir}/stdlib_system_subprocess.F90"
        "${_stdlib_system_source_dir}/stdlib_system_path.f90"
    )
    set_property(TARGET fortran_stdlib_system PROPERTY LINKER_LANGUAGE Fortran)

    add_library(fortran_stdlib_system_c OBJECT
        "${_stdlib_system_source_dir}/stdlib_system.c"
        "${_stdlib_system_source_dir}/stdlib_system_subprocess.c"
    )
    set_target_properties(fortran_stdlib_system_c PROPERTIES
        POSITION_INDEPENDENT_CODE ON
    )
    target_sources(fortran_stdlib_system PRIVATE
        $<TARGET_OBJECTS:fortran_stdlib_system_c>
    )

    unset(_stdlib_system_source_dir)
endif()

# Multi-config generators append the configuration name to
# Fortran_MODULE_DIRECTORY (for example, ``.../mod_files/target/Debug``).
# stdlib 0.8.1 creates only the base module directory, while ifx expects the
# complete output directory to exist before it writes the first .mod file.
# Create those per-configuration directories during generation so a clean
# Visual Studio build does not fail with ifx error #7001.
if(CMAKE_CONFIGURATION_TYPES)
    function(collect_buildsystem_targets DIRECTORY_PATH OUTPUT_VAR)
        get_property(_targets DIRECTORY "${DIRECTORY_PATH}" PROPERTY BUILDSYSTEM_TARGETS)
        get_property(_subdirectories DIRECTORY "${DIRECTORY_PATH}" PROPERTY SUBDIRECTORIES)

        foreach(_subdirectory IN LISTS _subdirectories)
            collect_buildsystem_targets("${_subdirectory}" _subdirectory_targets)
            list(APPEND _targets ${_subdirectory_targets})
        endforeach()

        set(${OUTPUT_VAR} "${_targets}" PARENT_SCOPE)
    endfunction()

    collect_buildsystem_targets("${fortran_stdlib_SOURCE_DIR}" _stdlib_targets)
    foreach(_target IN LISTS _stdlib_targets)
        get_target_property(_module_directory "${_target}" Fortran_MODULE_DIRECTORY)
        if(_module_directory AND NOT _module_directory MATCHES "-NOTFOUND$")
            foreach(_configuration IN LISTS CMAKE_CONFIGURATION_TYPES)
                file(MAKE_DIRECTORY "${_module_directory}/${_configuration}")
            endforeach()
        endif()
    endforeach()

    unset(_stdlib_targets)
    unset(_target)
    unset(_module_directory)
    unset(_configuration)
endif()

# =============================================================================
# Include Fortran dependency analysis
# =============================================================================
include(cmake/FortranDependencies.cmake)

# =============================================================================
# Source file discovery
# =============================================================================
function(discover_fortran_sources SOURCE_DIR OUTPUT_VAR)
    file(GLOB_RECURSE FORTRAN_FILES
        "${SOURCE_DIR}/*.f90"
        "${SOURCE_DIR}/*.F90"
    )

    # Filter out files that should be excluded
    set(FILTERED_FILES)
    foreach(FILE ${FORTRAN_FILES})
        get_filename_component(DIR ${FILE} DIRECTORY)
        get_filename_component(NAME ${FILE} NAME)

        set(SHOULD_EXCLUDE FALSE)

        # Exclude build directories and CMake files
        if(DIR MATCHES ".*/build.*" OR DIR MATCHES ".*/CMakeFiles.*")
            set(SHOULD_EXCLUDE TRUE)
        endif()

        # Exclude backup and temporary files
        if(NAME MATCHES ".*~$" OR NAME MATCHES ".*\\.bak$" OR NAME MATCHES ".*\\.tmp$" OR NAME MATCHES ".*\\.sav$")
            set(SHOULD_EXCLUDE TRUE)
        endif()

        # Exclude textual include files (not standalone compilation units)
        if(NAME MATCHES "^include_.*\\.f90$" OR NAME MATCHES "^include_.*\\.F90$")
            set(SHOULD_EXCLUDE TRUE)
        endif()

        if(NOT SHOULD_EXCLUDE)
            list(APPEND FILTERED_FILES ${FILE})
        endif()
    endforeach()

    set(${OUTPUT_VAR} ${FILTERED_FILES} PARENT_SCOPE)
endfunction()

# =============================================================================
# Discover and sort sources
# =============================================================================
message(STATUS "Discovering Fortran source files...")
discover_fortran_sources("${CMAKE_SOURCE_DIR}/src" DISCOVERED_SOURCES)

# Separate main program from module sources
set(MAIN_PROGRAM)
set(MODULE_SOURCES)

foreach(FILE ${DISCOVERED_SOURCES})
    get_filename_component(FILENAME ${FILE} NAME)
    if(FILENAME STREQUAL "Shetran.f90")
        set(MAIN_PROGRAM ${FILE})
        message(STATUS "Found main program: ${FILE}")
    else()
        list(APPEND MODULE_SOURCES ${FILE})
    endif()
endforeach()

if(NOT MAIN_PROGRAM)
    message(FATAL_ERROR "Main program src/Shetran.f90 not found!")
endif()

# Topologically sort module sources
if(ENABLE_DEPENDENCY_ANALYSIS)
    message(STATUS "Running automatic Fortran module dependency analysis...")
    topological_sort_fortran(MODULE_SOURCES ORDERED_MODULE_SOURCES)

    if(VERBOSE_DEPENDENCY_OUTPUT)
        validate_source_order(ORDERED_MODULE_SOURCES)
    endif()
else()
    message(FATAL_ERROR "ENABLE_DEPENDENCY_ANALYSIS must be ON. Automatic dependency analysis is required.")
endif()

# Combine: sorted modules first, then main program
set(SHETRAN_SOURCES ${ORDERED_MODULE_SOURCES} ${MAIN_PROGRAM})

list(LENGTH SHETRAN_SOURCES SOURCES_COUNT)
message(STATUS "Total sources found: ${SOURCES_COUNT}")

# =============================================================================
# Create executable
# =============================================================================
add_executable(SHETRAN ${SHETRAN_SOURCES})

if(WIN32)
    set(SHETRAN_GENERATED_RESOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/generated")
    set(SHETRAN_GENERATED_RESOURCE "${SHETRAN_GENERATED_RESOURCE_DIR}/shetran.rc")

    file(MAKE_DIRECTORY "${SHETRAN_GENERATED_RESOURCE_DIR}")
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/src/resource/shetran.rc.in"
        "${SHETRAN_GENERATED_RESOURCE}"
        @ONLY
    )
    target_sources(SHETRAN PRIVATE "${SHETRAN_GENERATED_RESOURCE}")
    set_source_files_properties("${SHETRAN_GENERATED_RESOURCE}" PROPERTIES
        COMPILE_DEFINITIONS "$<$<CONFIG:Debug>:_DEBUG>"
    )
endif()

add_dependencies(SHETRAN HDF5_external)

set_target_properties(SHETRAN PROPERTIES
    OUTPUT_NAME "shetran"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# =============================================================================
# Include directories
# =============================================================================
target_include_directories(SHETRAN PRIVATE
    ${HDF5_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# =============================================================================
# Link libraries
# =============================================================================
target_link_libraries(SHETRAN PRIVATE
    ${HDF5_LIBRARIES}
    fortran_stdlib::fortran_stdlib_system
)

target_compile_definitions(SHETRAN PRIVATE
    SHETRAN_HAVE_STDLIB_SYSTEM
    $<$<BOOL:${WIN32}>:SHETRAN_WINDOWS>
    $<$<BOOL:${IS_INTEL_FORTRAN}>:SHETRAN_INTEL_FORTRAN>
    $<$<BOOL:${SHETRAN_ENABLE_QUICKWIN}>:SHETRAN_HAVE_QUICKWIN>
    $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},GNU>:SHETRAN_GNU_FORTRAN>
)

# =============================================================================
# Compiler flags
# =============================================================================
function(apply_shetran_fortran_options target_name)
if(WIN32)
    # ifx on Windows uses MSVC-style options.
    target_compile_options(${target_name} PRIVATE
        /fpp
        # "/mcmodel:large"
        # "/heap-arrays:0"
        ${INTEL_G_FORMAT_WARN_SUPPRESS_FLAG}

        # Debug flags
        $<$<CONFIG:Debug>:/Od> # Use /Od for "disable optimization" with ifx
        $<$<CONFIG:Debug>:/debug:full>
        $<$<CONFIG:Debug>:/traceback>
        $<$<CONFIG:Debug>:/check:bounds>
        $<$<CONFIG:Debug>:/stand>
        $<$<CONFIG:Debug>:/warn:all>
        $<$<CONFIG:Debug>:/fp:precise>

        # Release: safer numerics, portable across CPU generations
        $<$<CONFIG:Release>:/O2>
        $<$<CONFIG:Release>:/fp:precise>

        # ReleaseNative: maximum local performance, non-portable ISA tuning
        $<$<CONFIG:ReleaseNative>:/O3>
        $<$<CONFIG:ReleaseNative>:/QxHost>
        $<$<CONFIG:ReleaseNative>:/fp:precise>
    )

elseif(UNIX AND NOT APPLE)
    if(IS_INTEL_FORTRAN)
        # ifx on Linux uses GNU-style options.
        target_compile_options(${target_name} PRIVATE
            -fpp
            ${INTEL_G_FORMAT_WARN_SUPPRESS_FLAG}
            $<$<CONFIG:Debug>:-O0>
            $<$<CONFIG:Debug>:-g>
            $<$<CONFIG:Debug>:-traceback>
            $<$<CONFIG:Debug>:-check>
            $<$<CONFIG:Debug>:bounds>
            $<$<CONFIG:Debug>:-stand>
            $<$<CONFIG:Debug>:-warn>
            $<$<CONFIG:Debug>:-fp-model=precise>

            $<$<CONFIG:Release>:-O2>
            $<$<CONFIG:Release>:-fp-model=precise>

            $<$<CONFIG:ReleaseNative>:-O3>
            $<$<CONFIG:ReleaseNative>:-xHost>
            $<$<CONFIG:Release>:-fp-model=precise>
        )
    elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        target_compile_options(${target_name} PRIVATE
            -cpp
            $<$<CONFIG:Debug>:-O0>
            $<$<CONFIG:Debug>:-g>
            $<$<CONFIG:Debug>:-fbacktrace>
            $<$<CONFIG:Debug>:-fcheck=bounds>
            $<$<CONFIG:Debug>:-Wall>
            $<$<CONFIG:Debug>:-Wextra>
            $<$<CONFIG:Debug>:-Wunused-variable>
            $<$<CONFIG:Debug>:-Wuse-without-only>
            $<$<CONFIG:Debug>:-fno-fast-math>

            $<$<CONFIG:Release>:-O2>
            $<$<CONFIG:Release>:-fno-fast-math>

            $<$<CONFIG:ReleaseNative>:-O3>
            $<$<CONFIG:ReleaseNative>:-march=native>
            $<$<CONFIG:ReleaseNative>:-fno-fast-math>
        )
    endif()
endif()
endfunction()

apply_shetran_fortran_options(SHETRAN)

# /libs:static — statically link Intel Fortran/C runtime libraries
# so the .exe runs without Intel oneAPI on PATH
if(WIN32 AND STATIC_RUNTIME)
    target_link_options(SHETRAN PRIVATE /libs:static)
endif()

if(SHETRAN_ENABLE_QUICKWIN)
    target_link_options(SHETRAN PRIVATE
        /libs:qwin
    )
endif()

# =============================================================================
# Tests
# =============================================================================
if(SHETRAN_BUILD_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()

# =============================================================================
# Configuration summary
# =============================================================================
message(STATUS "")
message(STATUS "SHETRAN Configuration Summary:")
message(STATUS "  Build type:       ${CMAKE_BUILD_TYPE}")
message(STATUS "  Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "  Compiler ID:      ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "  HDF5 source:      ${HDF5_LOCAL_TARBALL}")
message(STATUS "  zlib-ng source:   ${ZLIB_NG_LOCAL_TARBALL}")
message(STATUS "  stdlib source:    ${STDLIB_LOCAL_TARBALL}")
message(STATUS "  Parser tests:     ${SHETRAN_BUILD_TESTS}")
if(WIN32)
        message(STATUS "  Static runtime:   ${STATIC_RUNTIME}")
        message(STATUS "  QuickWin dialog:  ${SHETRAN_ENABLE_QUICKWIN}")
endif()
message(STATUS "  Output directory: ${CMAKE_BINARY_DIR}/bin")
message(STATUS "")
