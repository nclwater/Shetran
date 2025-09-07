cmake_minimum_required(VERSION 3.12)

project(SHETRAN LANGUAGES Fortran C)

# Find and link zlib for compression support (Linux/Unix)
if(NOT WIN32)
    find_package(ZLIB REQUIRED)
    if(NOT ZLIB_FOUND)
        message(FATAL_ERROR "ZLIB not found. Please install zlib development package (e.g. zlib1g-dev or zlib-devel).")
    endif()

    # Find SZIP library
    find_library(SZIP_LIBRARY sz)
    if(NOT SZIP_LIBRARY)
        message(FATAL_ERROR "SZIP library not found. Please install szip development package (e.g. libsz2-dev or szip-devel).")
    endif()
endif()

# Set C standard for HDF5 compatibility

# Compiler-specific settings
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    # Define the warning suppression flag for G-format descriptors based on platform
    if(WIN32)
        set(INTEL_G_FORMAT_WARN_SUPPRESS_FLAG "/Qdiag-disable:8291")
    else()
        set(INTEL_G_FORMAT_WARN_SUPPRESS_FLAG "-diag-disable 8291")
    endif()
endif()

set(CMAKE_C_STANDARD 99)

# Enable testing
enable_testing()

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build" FORCE)
endif()

# HDF5 build options (must be defined before HDF5 handling logic)
option(FORCE_BUILD_HDF5 "Force building HDF5 from source even if system version is available (Linux with gfortran only)" OFF)
option(HDF5_USE_SYSTEM_FIRST "Try system HDF5 first before building from source (Linux with gfortran only)" ON)

# Compiler-specific settings
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    if(CMAKE_Fortran_COMPILER MATCHES "ifort")
        # Intel Classic Fortran Compiler
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mcmodel=large -heap-arrays")
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${INTEL_G_FORMAT_WARN_SUPPRESS_FLAG}") # Suppress G-format remark
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")  # Enable preprocessor
        set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -traceback -check bounds -check uninit")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
    elseif(CMAKE_Fortran_COMPILER MATCHES "ifx")
        # Intel OneAPI Fortran Compiler  
        # On Windows with MSVC-like command lines, CMake defaults to /O2 for Release
        # builds. We add /O3 later, causing an override warning. This removes the
        # default /O2 flag to ensure a clean build.
        if(WIN32)
            string(REPLACE "/O2" "" CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}")
        endif()
        # Flags for ifx are set later using target_compile_options for better control,
        # especially on Windows to handle MSVC-like command lines correctly.
    endif()

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # GNU Fortran Compiler
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mcmodel=large")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fmax-stack-var-size=100000000")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")  # Enable preprocessor
    set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -fbacktrace -fbounds-check")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
else()
    # This 'else' block is for compilers other than Intel or GNU
    message(WARNING "Unsupported Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif()

# Enhanced HDF5 handling with automatic download and build
include(ExternalProject)
include(FetchContent)

# Function to determine the appropriate C compiler for the Fortran compiler
function(determine_c_compiler_for_fortran)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        # For gfortran, prefer gcc
        find_program(GCC_COMPILER gcc)
        if(GCC_COMPILER)
            set(CMAKE_C_COMPILER ${GCC_COMPILER} PARENT_SCOPE)
            message(STATUS "Using GCC as C compiler to match gfortran")
        else()
            find_program(CLANG_COMPILER clang)
            if(CLANG_COMPILER)
                set(CMAKE_C_COMPILER ${CLANG_COMPILER} PARENT_SCOPE)
                message(STATUS "Using Clang as C compiler (GCC not found)")
            endif()
        endif()
    elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
        # For Intel Fortran, prefer Intel C compiler, then clang, then gcc
        if(WIN32)
            find_program(ICL_COMPILER icl)
            if(ICL_COMPILER)
                set(CMAKE_C_COMPILER ${ICL_COMPILER} PARENT_SCOPE)
                message(STATUS "Using Intel C compiler (icl) to match Intel Fortran")
            else()
                find_program(CLANG_CL_COMPILER clang-cl)
                if(CLANG_CL_COMPILER)
                    set(CMAKE_C_COMPILER ${CLANG_CL_COMPILER} PARENT_SCOPE)
                    message(STATUS "Using Clang-cl as C compiler")
                endif()
            endif()
        else()
            find_program(ICC_COMPILER icc)
            if(ICC_COMPILER)
                set(CMAKE_C_COMPILER ${ICC_COMPILER} PARENT_SCOPE)
                message(STATUS "Using Intel C compiler (icc) to match Intel Fortran")
            else()
                find_program(ICX_COMPILER icx)
                if(ICX_COMPILER)
                    set(CMAKE_C_COMPILER ${ICX_COMPILER} PARENT_SCOPE)
                    message(STATUS "Using Intel oneAPI C compiler (icx) to match Intel Fortran")
                else()
                    find_program(CLANG_COMPILER clang)
                    if(CLANG_COMPILER)
                        set(CMAKE_C_COMPILER ${CLANG_COMPILER} PARENT_SCOPE)
                        message(STATUS "Using Clang as C compiler")
                    endif()
                endif()
            endif()
        endif()
    else()
        # For other compilers, use system default or clang
        find_program(CLANG_COMPILER clang)
        if(CLANG_COMPILER)
            set(CMAKE_C_COMPILER ${CLANG_COMPILER} PARENT_SCOPE)
            message(STATUS "Using Clang as C compiler for ${CMAKE_Fortran_COMPILER_ID}")
        endif()
    endif()
endfunction()

# Platform-specific HDF5 handling
if(WIN32)
    # Windows: Always use provided or download and build HDF5
    if(EXISTS "${CMAKE_SOURCE_DIR}/external/library-files/hdf5.lib")
        # Use existing provided libraries
        set(HDF5_ROOT "${CMAKE_SOURCE_DIR}/external")
        set(HDF5_INCLUDE_DIRS "${CMAKE_SOURCE_DIR}/external/Include")
        set(HDF5_LIBRARIES 
            "${CMAKE_SOURCE_DIR}/external/library-files/hdf5_hl_fortran.lib"
            "${CMAKE_SOURCE_DIR}/external/library-files/hdf5_hl.lib" 
            "${CMAKE_SOURCE_DIR}/external/library-files/hdf5_fortran.lib"
            "${CMAKE_SOURCE_DIR}/external/library-files/hdf5.lib"
        )
        set(HDF5_FOUND TRUE)
        message(STATUS "Using provided HDF5 libraries for Windows")
    else()
        # Download and build HDF5 for Windows
        message(STATUS "Downloading and building HDF5 for Windows")
        determine_c_compiler_for_fortran()
        
            ExternalProject_Add(HDF5_external
            URL https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.14/hdf5-1.14.3/src/hdf5-1.14.3.tar.gz
            URL_HASH SHA256=09cdb287aa7a89148c1638dd20891fdbae08102cf433ef128fd345338aa237c7
            DOWNLOAD_EXTRACT_TIMESTAMP TRUE
            CMAKE_ARGS
                -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/hdf5-install
                -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                -DHDF5_BUILD_FORTRAN=ON
                -DHDF5_BUILD_HL_LIB=ON
                -DHDF5_BUILD_EXAMPLES=OFF
                -DHDF5_BUILD_TESTING=ON
                -DHDF5_BUILD_TOOLS=OFF
                -DBUILD_TESTING=OFF
                -DHDF5_ENABLE_TESTS=ON
                -DHDF5_ENABLE_EXAMPLES=OFF
                -DHDF5_TEST_FORTRAN=OFF
                -DHDF5_BUILD_FORTRAN_TESTS=OFF
                -DHDF5_ENABLE_SHARED_LIB=OFF
                -DHDF5_ENABLE_STATIC_LIB=ON
                -DBUILD_SHARED_LIBS=OFF
                -DHDF5_ENABLE_SZIP_SUPPORT=OFF
                -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
            BUILD_BYPRODUCTS
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5.lib
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_f90cstub.lib
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_fortran.lib
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl.lib
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_f90cstub.lib
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_fortran.lib
        )
        
        set(HDF5_ROOT "${CMAKE_BINARY_DIR}/hdf5-install")
        set(HDF5_INCLUDE_DIRS "${CMAKE_BINARY_DIR}/hdf5-install/include")
        set(HDF5_LIBRARIES 
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_fortran.lib"
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_f90cstub.lib"
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl.lib"
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_fortran.lib"
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_f90cstub.lib"
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5.lib"
            "shlwapi"
        )
        set(HDF5_FOUND TRUE)
        set(HDF5_IS_EXTERNAL TRUE)
    endif()
else()
    # Linux/Unix: Strategy depends on compiler
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        # For gfortran on Linux: Try system first (if enabled), then download and build
        if(HDF5_USE_SYSTEM_FIRST AND NOT FORCE_BUILD_HDF5)
            find_package(HDF5 QUIET COMPONENTS Fortran HL)
            if(HDF5_FOUND)
                message(STATUS "Found system HDF5: ${HDF5_VERSION} (using with gfortran on Linux)")
                # Ensure we have all the high-level Fortran libraries for the system HDF5
                # Use pkg-config to get the proper library list
                find_package(PkgConfig QUIET)
                if(PKG_CONFIG_FOUND)
                    pkg_check_modules(HDF5_FORTRAN QUIET hdf5_fortran)
                    pkg_check_modules(HDF5_HL_FORTRAN QUIET hdf5_hl_fortran)
                    if(HDF5_FORTRAN_FOUND AND HDF5_HL_FORTRAN_FOUND)
                        # Combine all the necessary libraries
                        set(HDF5_LIBRARIES ${HDF5_HL_FORTRAN_LIBRARIES} ${HDF5_FORTRAN_LIBRARIES} ${HDF5_LIBRARIES})
                        message(STATUS "HDF5 Fortran libraries: ${HDF5_LIBRARIES}")
                    endif()
                endif()
            else()
                message(STATUS "System HDF5 not found. Downloading and building HDF5 for gfortran on Linux")
                set(BUILD_HDF5_FROM_SOURCE TRUE)
            endif()
        else()
            if(FORCE_BUILD_HDF5)
                message(STATUS "FORCE_BUILD_HDF5 enabled. Downloading and building HDF5 for gfortran on Linux")
            else()
                message(STATUS "HDF5_USE_SYSTEM_FIRST disabled. Downloading and building HDF5 for gfortran on Linux")
            endif()
            set(BUILD_HDF5_FROM_SOURCE TRUE)
        endif()
        
        if(BUILD_HDF5_FROM_SOURCE)
            determine_c_compiler_for_fortran()
            
                    ExternalProject_Add(HDF5_external
                URL https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.14/hdf5-1.14.3/src/hdf5-1.14.3.tar.gz
                URL_HASH SHA256=09cdb287aa7a89148c1638dd20891fdbae08102cf433ef128fd345338aa237c7
                DOWNLOAD_EXTRACT_TIMESTAMP TRUE
                CMAKE_ARGS
                    -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/hdf5-install
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DHDF5_BUILD_FORTRAN=ON
                    -DHDF5_BUILD_HL_LIB=ON
                    -DHDF5_BUILD_EXAMPLES=OFF
                    -DHDF5_BUILD_TESTING=ON
                    -DHDF5_BUILD_TOOLS=OFF
                    -DBUILD_TESTING=OFF
                    -DHDF5_ENABLE_TESTS=ON
                    -DHDF5_ENABLE_EXAMPLES=OFF
                    -DHDF5_TEST_FORTRAN=OFF
                    -DHDF5_BUILD_FORTRAN_TESTS=OFF
                    -DHDF5_ENABLE_SHARED_LIB=OFF
                    -DHDF5_ENABLE_STATIC_LIB=ON
                    -DBUILD_SHARED_LIBS=OFF
                    -DHDF5_ENABLE_SZIP_SUPPORT=OFF
                    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                    -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
                BUILD_BYPRODUCTS
                    ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5.a
                    ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_fortran.a
                    ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl.a
                    ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_fortran.a
            )
            
            set(HDF5_ROOT "${CMAKE_BINARY_DIR}/hdf5-install")
            set(HDF5_INCLUDE_DIRS "${CMAKE_BINARY_DIR}/hdf5-install/include")
            set(HDF5_LIBRARIES 
                "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_fortran.a"
                "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl.a"
                "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_fortran.a"
                "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5.a"
            )
            set(HDF5_FOUND TRUE)
            set(HDF5_IS_EXTERNAL TRUE)
        endif()
    else()
        # For non-gfortran on Linux: Always download and build
        message(STATUS "Using non-gfortran compiler on Linux. Downloading and building HDF5")
        determine_c_compiler_for_fortran()
        
        ExternalProject_Add(HDF5_external
            URL https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.14/hdf5-1.14.3/src/hdf5-1.14.3.tar.gz
            URL_HASH SHA256=09cdb287aa7a89148c1638dd20891fdbae08102cf433ef128fd345338aa237c7
            DOWNLOAD_EXTRACT_TIMESTAMP TRUE
            CMAKE_ARGS
                -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/hdf5-install
                -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                -DHDF5_BUILD_FORTRAN=ON
                -DHDF5_BUILD_HL_LIB=ON
                -DHDF5_BUILD_EXAMPLES=OFF
                -DHDF5_BUILD_TESTING=ON
                -DHDF5_BUILD_TOOLS=OFF
                -DBUILD_TESTING=OFF
                -DHDF5_ENABLE_TESTS=ON
                -DHDF5_ENABLE_EXAMPLES=OFF
                -DHDF5_TEST_FORTRAN=OFF
                -DHDF5_BUILD_FORTRAN_TESTS=OFF
                -DHDF5_ENABLE_SHARED_LIB=OFF
                -DHDF5_ENABLE_STATIC_LIB=ON
                -DBUILD_SHARED_LIBS=OFF
                -DHDF5_ENABLE_SZIP_SUPPORT=OFF
                -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
            BUILD_BYPRODUCTS
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5.a
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_fortran.a
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl.a
                ${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_fortran.a
        )
        
        set(HDF5_ROOT "${CMAKE_BINARY_DIR}/hdf5-install")
        set(HDF5_INCLUDE_DIRS "${CMAKE_BINARY_DIR}/hdf5-install/include")
        set(HDF5_LIBRARIES 
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl_fortran.a"
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_hl.a"
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5_fortran.a"
            "${CMAKE_BINARY_DIR}/hdf5-install/lib/libhdf5.a"
        )
        set(HDF5_FOUND TRUE)
        set(HDF5_IS_EXTERNAL TRUE)
    endif()
endif()

# Test for H5IM palette functions availability (only if using system HDF5)
if(HDF5_FOUND AND NOT HDF5_IS_EXTERNAL)
    include(CheckFortranSourceCompiles)
    set(CMAKE_REQUIRED_INCLUDES ${HDF5_INCLUDE_DIRS})
    set(CMAKE_REQUIRED_LIBRARIES ${HDF5_LIBRARIES})
    
    check_fortran_source_compiles("
        program test_h5im_palette
            use hdf5
            use h5im
            implicit none
            integer(hid_t) :: file_id, pal_id
            integer :: error
            integer(hsize_t), dimension(2) :: dims = [256, 3]
            integer, dimension(768) :: pal_data
            
            call h5open_f(error)
            call h5fcreate_f('test.h5', H5F_ACC_TRUNC_F, file_id, error)
            call h5immake_palette_f(file_id, 'palette', dims, pal_data, error)
            call h5fclose_f(file_id, error)
            call h5close_f(error)
        end program
    " HAVE_H5IM_PALETTE)
    
    if(HAVE_H5IM_PALETTE)
        message(STATUS "H5IM palette functions: Available")
        add_compile_definitions(HAVE_H5IM_PALETTE)
    else()
        message(STATUS "H5IM palette functions: Not available (using basic HDF5)")
    endif()
else()
    # For external HDF5 builds, assume H5IM palette functions are available
    if(HDF5_IS_EXTERNAL)
        message(STATUS "H5IM palette functions: Assumed available (external HDF5 build)")
        add_compile_definitions(HAVE_H5IM_PALETTE)
    endif()
endif()

if(NOT HDF5_FOUND)
    message(FATAL_ERROR "HDF5 configuration failed. Unable to find or build HDF5.")
endif()

# Build options
option(ENABLE_DEPENDENCY_ANALYSIS "Enable automatic Fortran module dependency analysis for optimal source ordering (recommended)" ON)
option(VERBOSE_DEPENDENCY_OUTPUT "Enable verbose output during dependency analysis and validation" OFF)
option(USE_WINDOWS_INTEL_GETDIRQQ "Use Windows Intel-specific getdirqq (only available on Windows with Intel compiler)" OFF)

# Handle getdirqq version selection and ensure the generated file is included
if(USE_WINDOWS_INTEL_GETDIRQQ)
    if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
        set(GETDIRQQ_SOURCE "${CMAKE_SOURCE_DIR}/src/util/getdirqq_winIntel.f90")
        message(STATUS "Using Windows Intel-specific getdirqq version")
    else()
        if(NOT WIN32)
            message(FATAL_ERROR "Windows Intel getdirqq requested but platform is not Windows. Use portable version instead.")
        elseif(NOT CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
            message(FATAL_ERROR "Windows Intel getdirqq requested but compiler is not Intel (${CMAKE_Fortran_COMPILER_ID}). Use portable version instead.")
        endif()
    endif()
else()
    set(GETDIRQQ_SOURCE "${CMAKE_SOURCE_DIR}/src/util/getdirqq_portable.f90")
    message(STATUS "Using portable getdirqq version (cross-platform)")
endif()

# Copy selected version to the active getdirqq.f90 (this happens at configure time)
set(GETDIRQQ_TARGET "${CMAKE_SOURCE_DIR}/src/util/getdirqq.f90")
configure_file("${GETDIRQQ_SOURCE}" "${GETDIRQQ_TARGET}" COPYONLY)

# Include advanced Fortran dependency analysis
include(cmake/FortranDependencies.cmake)

# Enhanced function to automatically discover Fortran source files
function(discover_fortran_sources SOURCE_DIR OUTPUT_VAR)
    # Find all Fortran source files
    file(GLOB_RECURSE FORTRAN_FILES
        "${SOURCE_DIR}/*.f90" 
        "${SOURCE_DIR}/*.F90"
        "${SOURCE_DIR}/*.f95"
        "${SOURCE_DIR}/*.F95"
        "${SOURCE_DIR}/*.f03"
        "${SOURCE_DIR}/*.F03"
        "${SOURCE_DIR}/*.f08"
        "${SOURCE_DIR}/*.F08"
    )
    
    # Filter out files that should be excluded
    set(FILTERED_FILES)
    foreach(FILE ${FORTRAN_FILES})
        get_filename_component(DIR ${FILE} DIRECTORY)
        get_filename_component(NAME ${FILE} NAME)
        
        # Exclude build directories, CMake files, and template files
        set(SHOULD_EXCLUDE FALSE)
        
        if(DIR MATCHES ".*/build.*" OR DIR MATCHES ".*/CMakeFiles.*")
            set(SHOULD_EXCLUDE TRUE)
        endif()
        
        # Exclude template files (but include the generated getdirqq.f90)
        if(NAME MATCHES ".*_portable\\.f90$" OR NAME MATCHES ".*_winIntel\\.f90$")
            set(SHOULD_EXCLUDE TRUE)
        endif()
        
        # Exclude backup files and temporary files
        if(NAME MATCHES ".*~$" OR NAME MATCHES ".*\\.bak$" OR NAME MATCHES ".*\\.tmp$" OR NAME MATCHES ".*\\.sav$")
            set(SHOULD_EXCLUDE TRUE)
        endif()
        
        if(NOT SHOULD_EXCLUDE)
            list(APPEND FILTERED_FILES ${FILE})
        endif()
    endforeach()
    
    # Ensure the generated getdirqq.f90 is included if it exists
    if(EXISTS "${GETDIRQQ_TARGET}")
        list(FIND FILTERED_FILES "${GETDIRQQ_TARGET}" GETDIRQQ_INDEX)
        if(GETDIRQQ_INDEX EQUAL -1)
            list(APPEND FILTERED_FILES "${GETDIRQQ_TARGET}")
        endif()
    endif()
    
    set(${OUTPUT_VAR} ${FILTERED_FILES} PARENT_SCOPE)
endfunction()

# Function to get automatic dependency ordering using Fortran module analysis
function(automatic_dependency_sort SOURCES_LIST OUTPUT_VAR)
    set(ORDERED_SOURCES)
    set(REMAINING_SOURCES ${${SOURCES_LIST}})
    
    message(STATUS "Using automatic Fortran module dependency analysis...")
    
    # Use the advanced topological sorting from FortranDependencies.cmake
    topological_sort_fortran(REMAINING_SOURCES ORDERED_SOURCES)
    
    # Validate the ordering
    if(VERBOSE_DEPENDENCY_OUTPUT)
        validate_source_order(ORDERED_SOURCES)
    endif()
    
    set(${OUTPUT_VAR} ${ORDERED_SOURCES} PARENT_SCOPE)
endfunction()

# Discover all Fortran source files automatically
message(STATUS "Discovering Fortran source files...")
discover_fortran_sources("${CMAKE_SOURCE_DIR}/src" DISCOVERED_SOURCES)

# Separate main program from modules
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

# Automatically sort module sources using the best available method
if(ENABLE_DEPENDENCY_ANALYSIS)
    message(STATUS "Using automatic Fortran module dependency analysis...")
    automatic_dependency_sort(MODULE_SOURCES ORDERED_MODULE_SOURCES)
endif() # ENABLE_DEPENDENCY_ANALYSIS is now required

# Validate source ordering if requested
if(ENABLE_DEPENDENCY_ANALYSIS AND VERBOSE_DEPENDENCY_OUTPUT)
    validate_source_order(ORDERED_MODULE_SOURCES)
endif()

# Combine all sources in correct order (modules first, then main program)
set(SHETRAN_SOURCES ${ORDERED_MODULE_SOURCES})
if(MAIN_PROGRAM)
    list(APPEND SHETRAN_SOURCES ${MAIN_PROGRAM})
endif()

# Print summary of discovered sources
message(STATUS "")
list(LENGTH SHETRAN_SOURCES SOURCES_COUNT)
message(STATUS "Total sources found: ${SOURCES_COUNT}")

# Provide information about the dependency analysis
if(ENABLE_DEPENDENCY_ANALYSIS)
    message(STATUS "Source ordering: Automatic dependency analysis (recommended)")
else()
    message(FATAL_ERROR "ENABLE_DEPENDENCY_ANALYSIS is now required. The pattern-based fallback has been removed.")
endif()
message(STATUS "")

# Create the executable
add_executable(SHETRAN ${SHETRAN_SOURCES})

# Add dependency on external HDF5 if it's being built
if(HDF5_IS_EXTERNAL)
    add_dependencies(SHETRAN HDF5_external)
endif()

# Set target properties
set_target_properties(SHETRAN PROPERTIES
    OUTPUT_NAME "shetran"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Include directories
target_include_directories(SHETRAN PRIVATE ${HDF5_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}/src)

# Link libraries
target_link_libraries(SHETRAN PRIVATE ${HDF5_LIBRARIES})

# Apply compiler-specific flags using target_compile_options for better control
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    if(CMAKE_Fortran_COMPILER MATCHES "ifort")
        # ifort flags are already set via CMAKE_Fortran_FLAGS
    elseif(CMAKE_Fortran_COMPILER MATCHES "ifx")
        # For ifx, we must be careful with flags, especially on Windows where
        # CMake may convert '-' to '/' for MSVC-like command lines.
        # We use modern generator expressions for configuration-specific flags.
        if(WIN32)
            # On Windows, use MSVC-style flags. /QxHost is the equivalent of -xHost.
            # There is no equivalent for -mcmodel=large or -fstack-arrays.
            # We increase the stack size via a linker flag.
            target_compile_options(SHETRAN PRIVATE
                "/fpp" # Enable preprocessor
                "${INTEL_G_FORMAT_WARN_SUPPRESS_FLAG}" # Suppress G-format remark
            )
            target_compile_options(SHETRAN PRIVATE "$<$<CONFIG:Debug>:/O0;/Zi;/traceback;/check:bounds;/check:uninit>")
            target_compile_options(SHETRAN PRIVATE "$<$<CONFIG:Release,RelWithDebInfo>:/O3;/QxHost>")
        else()
            # On Linux, standard hyphenated flags work as expected.
            # -fstack-arrays is a gfortran flag, not valid for ifx.
            # Users should increase stack limit via `ulimit -s` if needed for large stack arrays.
            target_compile_options(SHETRAN PRIVATE
                "-fpp"
                "-mcmodel=large"
                "${INTEL_G_FORMAT_WARN_SUPPRESS_FLAG}" # Suppress G-format remark
            )
            target_compile_options(SHETRAN PRIVATE "$<$<CONFIG:Debug>:-O0;-g;-traceback;-check;bounds;-check;uninit>")
            target_compile_options(SHETRAN PRIVATE "$<$<CONFIG:Release,RelWithDebInfo>:-O3;-xHost>")
        endif()
    endif()
endif()

# Add high level HDF5 Fortran library explicitly for Linux/Unix
if(NOT WIN32)
    # Link all required HDF5 Fortran stub libraries, main HDF5 C library, and zlib for Intel ifx and other compilers
    target_include_directories(SHETRAN PRIVATE ${ZLIB_INCLUDE_DIRS})
    
    # Only use hardcoded paths if we built HDF5 from source (HDF5_IS_EXTERNAL is set)
    if(HDF5_IS_EXTERNAL)
        # Explicitly link the stub library file if it exists for externally built HDF5
        set(HDF5_LIBDIR "${CMAKE_BINARY_DIR}/hdf5-install/lib")
        set(HDF5_HL_FORTRAN "${HDF5_LIBDIR}/libhdf5_hl_fortran.a")
        set(HDF5_FORTRAN "${HDF5_LIBDIR}/libhdf5_fortran.a")
        set(HDF5_HL "${HDF5_LIBDIR}/libhdf5_hl.a")
        set(HDF5_C "${HDF5_LIBDIR}/libhdf5.a")
        set(HDF5_F90CSTUB "${HDF5_LIBDIR}/libhdf5_f90cstub.a")
        set(HDF5_HL_F90CSTUB "${HDF5_LIBDIR}/libhdf5_hl_f90cstub.a")
        target_link_libraries(SHETRAN PRIVATE "${HDF5_HL_FORTRAN}" "${HDF5_FORTRAN}" "${HDF5_HL}" "${HDF5_C}" ${ZLIB_LIBRARIES} "${HDF5_F90CSTUB}" "${HDF5_HL_F90CSTUB}" "${HDF5_HL}" "${HDF5_C}")
    else()
        # For system HDF5, just link zlib as the HDF5 libraries are already linked above
        target_link_libraries(SHETRAN PRIVATE ${ZLIB_LIBRARIES})
    endif()
endif()

# Additional linker flags for large memory model
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    # -mcmodel=large is a compiler flag, not a linker flag. The compiler handles the linker part.
    if(WIN32 AND CMAKE_Fortran_COMPILER MATCHES "ifx")
        # For ifx on Windows, there is no -mcmodel=large or equivalent.
        # We increase the stack size to handle large arrays.
        set_target_properties(SHETRAN PROPERTIES LINK_FLAGS "/link /STACK:500000000") # 500 MB stack
    endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # For gfortran, -mcmodel=large is also a compiler flag.
    # If specific linker flags are needed for gfortran, they should be added here.
endif()

# Installation
install(TARGETS SHETRAN
    RUNTIME DESTINATION bin
    COMPONENT Runtime
)

# Install examples
install(DIRECTORY examples/
    DESTINATION share/shetran/examples
    COMPONENT Examples
    PATTERN "*.txt" PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
)

# Install documentation
install(FILES 
    README.md
    CHANGELOG.md
    COMPILING.md
    LICENSE.md
    DESTINATION share/doc/shetran
    COMPONENT Documentation
)

# CPack configuration for packaging
set(CPACK_PACKAGE_NAME "SHETRAN")
set(CPACK_PACKAGE_VENDOR "Newcastle University")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "SHETran - Hydrological modelling package")
set(CPACK_PACKAGE_VERSION_MAJOR 4)
set(CPACK_PACKAGE_VERSION_MINOR 4)
set(CPACK_PACKAGE_VERSION_PATCH 6)
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/LICENSE.md")
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.md")

if(WIN32)
    set(CPACK_GENERATOR "NSIS")
else()
    set(CPACK_GENERATOR "TGZ;DEB")
    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Newcastle University Water Group")
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "libhdf5-dev")
endif()

include(CPack)

# FORD Documentation Target
find_program(FORD_EXECUTABLE ford)
if(FORD_EXECUTABLE)
    # Generate source file list for FORD dependencies
    file(GLOB_RECURSE FORTRAN_SOURCES "src/*.f90" "src/*.F90")
    
    add_custom_target(docs
        COMMAND ${FORD_EXECUTABLE} ${CMAKE_SOURCE_DIR}/ford_project.md
        DEPENDS ${FORTRAN_SOURCES} ${CMAKE_SOURCE_DIR}/ford_project.md
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Generating FORD documentation"
        VERBATIM
    )
    
    # Optional: Install documentation
    install(DIRECTORY ${CMAKE_SOURCE_DIR}/docs/ford/
            DESTINATION share/doc/shetran
            OPTIONAL)
            
    message(STATUS "FORD found: ${FORD_EXECUTABLE}")
    message(STATUS "Documentation target 'docs' available")
else()
    message(STATUS "FORD not found - documentation target unavailable")
    message(STATUS "Install FORD with: pip install ford")
endif()

# Print configuration summary
message(STATUS "")
message(STATUS "SHETRAN Configuration Summary:")
message(STATUS "  Build type:     ${CMAKE_BUILD_TYPE}")
message(STATUS "  Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "  Fortran flags:    ${CMAKE_Fortran_FLAGS}")
message(STATUS "  HDF5 version:     ${HDF5_VERSION}")
message(STATUS "  HDF5 includes:    ${HDF5_INCLUDE_DIRS}")
message(STATUS "  Install prefix:   ${CMAKE_INSTALL_PREFIX}")
message(STATUS "")
