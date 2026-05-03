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

project(SHETRAN LANGUAGES Fortran C)

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
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
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

if(WIN32)
    set(HDF5_STATIC_EXT ".lib")
else()
    set(HDF5_STATIC_EXT ".a")
endif()

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
    -DHDF5_ENABLE_Z_LIB_SUPPORT=OFF
    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
    -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
)

if(WIN32)
    list(APPEND HDF5_CMAKE_ARGS -DCMAKE_MSVC_RUNTIME_LIBRARY=${HDF5_MSVC_RUNTIME})
endif()

ExternalProject_Add(HDF5_external
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
        ${CMAKE_COMMAND} --install <BINARY_DIR> --prefix <INSTALL_DIR>
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
    set_target_properties(hdf5::${_hdf5_lib} PROPERTIES
        IMPORTED_LOCATION "${HDF5_LIB_DIR}/${_hdf5_lib}${HDF5_LIB_POSTFIX}${HDF5_STATIC_EXT}"
    )
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
)

if(WIN32)
    list(APPEND HDF5_LIBRARIES shlwapi)
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
target_link_libraries(SHETRAN PRIVATE ${HDF5_LIBRARIES})

# =============================================================================
# Compiler flags
# =============================================================================
if(WIN32)
    # ifx on Windows uses MSVC-style options.
    target_compile_options(SHETRAN PRIVATE
        # "/fpp"
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
        # (intentionally no interprocedural optimization flags such as /Qipo)
        # because they break the build
        $<$<CONFIG:ReleaseNative>:/O3>
        $<$<CONFIG:ReleaseNative>:/QxHost>
        $<$<CONFIG:ReleaseNative>:/fp:precise>
    )

    # /libs:static — statically link Intel Fortran/C runtime libraries
    # so the .exe runs without Intel oneAPI on PATH
    if(STATIC_RUNTIME)
        target_link_options(SHETRAN PRIVATE /libs:static)
    endif()
elseif(UNIX AND NOT APPLE)
    if(IS_INTEL_FORTRAN)
        # ifx on Linux uses GNU-style options.
        target_compile_options(SHETRAN PRIVATE
            ${INTEL_G_FORMAT_WARN_SUPPRESS_FLAG}
            $<$<CONFIG:Debug>:-O0>
            $<$<CONFIG:Debug>:-g>
            $<$<CONFIG:Debug>:-traceback>
            $<$<CONFIG:Debug>:-check=bounds>
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
        target_compile_options(SHETRAN PRIVATE
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

# =============================================================================
# Configuration summary
# =============================================================================
message(STATUS "")
message(STATUS "SHETRAN Configuration Summary:")
message(STATUS "  Build type:       ${CMAKE_BUILD_TYPE}")
message(STATUS "  Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "  Compiler ID:      ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "  HDF5 source:      ${HDF5_LOCAL_TARBALL}")
if(WIN32)
        message(STATUS "  Static runtime:   ${STATIC_RUNTIME}")
endif()
message(STATUS "  Output directory: ${CMAKE_BINARY_DIR}/bin")
message(STATUS "")
