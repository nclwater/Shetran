cmake_minimum_required(VERSION 3.20)

project(SHETRAN LANGUAGES Fortran C)

# =============================================================================
# Build type
# =============================================================================
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build (Debug or Release)" FORCE)
endif()

# =============================================================================
# Compiler check — only ifx is supported for now
# =============================================================================
if(NOT (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" OR CMAKE_Fortran_COMPILER_ID STREQUAL "Intel"))
    message(FATAL_ERROR
        "Unsupported Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}\n"
        "Currently only Intel ifx is supported on Windows.")
endif()

# =============================================================================
# Compiler flags for ifx on Windows
# =============================================================================
# G-format descriptor warning suppression
set(INTEL_G_FORMAT_WARN_SUPPRESS_FLAG "/Qdiag-disable:8291")

# Remove the default /O2 that CMake adds for Release to avoid conflict with /O3
string(REPLACE "/O2" "" CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}")

set(CMAKE_C_STANDARD 99)

# =============================================================================
# Build options
# =============================================================================
option(ENABLE_DEPENDENCY_ANALYSIS "Enable automatic Fortran module dependency analysis" ON)
option(VERBOSE_DEPENDENCY_OUTPUT "Enable verbose output during dependency analysis" OFF)
option(STATIC_RUNTIME "Link Intel and MSVC runtimes statically (portable .exe, no oneAPI on PATH required)" ON)

# =============================================================================
# Runtime linking
# =============================================================================
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

# HDF5 appends _D to library names in Debug builds by default
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(HDF5_LIB_POSTFIX "_D")
else()
    set(HDF5_LIB_POSTFIX "")
endif()

ExternalProject_Add(HDF5_external
    URL ${HDF5_URL}
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/hdf5-install
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DCMAKE_MSVC_RUNTIME_LIBRARY=${HDF5_MSVC_RUNTIME}
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
    BUILD_BYPRODUCTS
        ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5${HDF5_LIB_POSTFIX}.lib
        ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_f90cstub${HDF5_LIB_POSTFIX}.lib
        ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_fortran${HDF5_LIB_POSTFIX}.lib
        ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl${HDF5_LIB_POSTFIX}.lib
        ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_f90cstub${HDF5_LIB_POSTFIX}.lib
        ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_fortran${HDF5_LIB_POSTFIX}.lib
)

set(HDF5_INCLUDE_DIRS "${CMAKE_BINARY_DIR}/hdf5-install/include")
set(HDF5_LIB_DIR "${CMAKE_BINARY_DIR}/hdf5-install/lib")

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
        IMPORTED_LOCATION "${HDF5_LIB_DIR}/${_hdf5_lib}${HDF5_LIB_POSTFIX}.lib"
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
    shlwapi
)

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
# Compiler flags (ifx on Windows, MSVC-style)
# =============================================================================
target_compile_options(SHETRAN PRIVATE
    # "/fpp"
    # "/mcmodel:large"
    # "/heap-arrays:0"
    "${INTEL_G_FORMAT_WARN_SUPPRESS_FLAG}"

    # Debug flags
    $<$<CONFIG:Debug>:/Od> # Use /Od for "disable optimization" with ifx
    $<$<CONFIG:Debug>:/debug:full>
    $<$<CONFIG:Debug>:/traceback>
    $<$<CONFIG:Debug>:/check:bounds>
    $<$<CONFIG:Debug>:/stand>

    # Release flags
    $<$<CONFIG:Release>:/O3>
    $<$<CONFIG:Release>:/QxHost>
)
# /libs:static — statically link Intel Fortran/C runtime libraries
# so the .exe runs without Intel oneAPI on PATH
if(STATIC_RUNTIME)
    target_link_options(SHETRAN PRIVATE /libs:static)
endif()

# =============================================================================
# Configuration summary
# =============================================================================
message(STATUS "")
message(STATUS "SHETRAN Configuration Summary:")
message(STATUS "  Build type:       ${CMAKE_BUILD_TYPE}")
message(STATUS "  Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "  HDF5 source:      ${HDF5_LOCAL_TARBALL}")
  message(STATUS "  Static runtime:   ${STATIC_RUNTIME}")
  message(STATUS "  Output directory: ${CMAKE_BINARY_DIR}/bin")
message(STATUS "")
