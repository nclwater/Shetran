cmake_minimum_required(VERSION 3.12)
project(SHETRAN LANGUAGES Fortran C)

# Set C standard for HDF5 compatibility
set(CMAKE_C_STANDARD 99)

# Enable testing
enable_testing()

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build" FORCE)
endif()

# Compiler-specific settings
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    if(CMAKE_Fortran_COMPILER MATCHES "ifort")
        # Intel Classic Fortran Compiler
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mcmodel=large -heap-arrays")
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")  # Enable preprocessor
        set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -traceback -check bounds -check uninit")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
    elseif(CMAKE_Fortran_COMPILER MATCHES "ifx")
        # Intel OneAPI Fortran Compiler  
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mcmodel=large")
        # Note: heap-arrays is not available in ifx, use alternative approach
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fstack-arrays")
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")  # Enable preprocessor
        set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -traceback -check bounds -check uninit")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
    endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # GNU Fortran Compiler
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mcmodel=large")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fmax-stack-var-size=100000000")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")  # Enable preprocessor
    set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -fbacktrace -fbounds-check")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
else()
    message(WARNING "Unsupported Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif()

# Platform-specific HDF5 handling
if(WIN32)
    # Use provided HDF5 libraries for Windows
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
    # Find system HDF5 for Linux/Unix
    find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
    if(HDF5_FOUND)
        message(STATUS "Found HDF5: ${HDF5_VERSION}")
        message(STATUS "HDF5 Include dirs: ${HDF5_INCLUDE_DIRS}")
        message(STATUS "HDF5 Libraries: ${HDF5_LIBRARIES}")
        
        # Test for H5IM palette functions availability
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
        message(FATAL_ERROR "HDF5 not found. Please install HDF5 development packages.")
    endif()
endif()

# Build options
option(ENABLE_DEPENDENCY_ANALYSIS "Enable automatic Fortran module dependency analysis for optimal source ordering (recommended)" ON)
option(VERBOSE_DEPENDENCY_OUTPUT "Enable verbose output during dependency analysis and validation" OFF)
option(USE_WINDOWS_INTEL_GETDIRQQ "Use Windows Intel-specific getdirqq (only available on Windows with Intel compiler)" OFF)

# Handle getdirqq version selection and ensure the generated file is included
if(USE_WINDOWS_INTEL_GETDIRQQ)
    if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
        set(GETDIRQQ_SOURCE "${CMAKE_SOURCE_DIR}/src/modules/getdirqq_winIntel.f90")
        message(STATUS "Using Windows Intel-specific getdirqq version")
    else()
        if(NOT WIN32)
            message(FATAL_ERROR "Windows Intel getdirqq requested but platform is not Windows. Use portable version instead.")
        elseif(NOT CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
            message(FATAL_ERROR "Windows Intel getdirqq requested but compiler is not Intel (${CMAKE_Fortran_COMPILER_ID}). Use portable version instead.")
        endif()
    endif()
else()
    set(GETDIRQQ_SOURCE "${CMAKE_SOURCE_DIR}/src/modules/getdirqq_portable.f90")
    message(STATUS "Using portable getdirqq version (cross-platform)")
endif()

# Copy selected version to the active getdirqq.f90 (this happens at configure time)
set(GETDIRQQ_TARGET "${CMAKE_SOURCE_DIR}/src/modules/getdirqq.f90")
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
        if(NAME MATCHES ".*~$" OR NAME MATCHES ".*\\.bak$" OR NAME MATCHES ".*\\.tmp$")
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
            message(STATUS "Added generated file: getdirqq.f90")
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

# Fallback function for pattern-based ordering (kept as backup)
function(pattern_based_dependency_sort SOURCES_LIST OUTPUT_VAR)
    set(ORDERED_SOURCES)
    set(REMAINING_SOURCES ${${SOURCES_LIST}})
    
    message(STATUS "Using pattern-based ordering as fallback...")
    
    # Define ordering groups with patterns (updated for refactored structure)
    set(ORDER_GROUPS
        # Group 1: Global parameters and constants
        "sglobal\\.f90$;CONST_SY\\.F90$;mod_parameters\\.f90$"
        
        # Group 2: Basic parameter modules  
        "AL_[GCD]\\.f?90$;CONT_CC\\.F90$"
        
        # Group 3: Utility and support modules (including generated getdirqq)
        "utilsmod\\.f90$;getdirqq\\.f90$;mod_load_filedata\\.f90$"
        
        # Group 4: Basic computation modules
        "is_cc\\.f90$;link_cc.*\\.f90$;colm_c.*\\.f90$;bk_cw\\.f90$;sed_co\\.f90$"
        
        # Group 5: Core modules (basic dependencies)
        "SED_CS\\.F90$;ZQmod\\.f90$;SMmod\\.f90$;plant_cc\\.f90$"
        
        # Group 6: Refactored OC modules (ordered by dependency)
        "oc_parameters\\.f90$;oc_data_management\\.f90$;oc_hydraulic_calculations\\.f90$"
        
        # Group 7: More OC modules  
        "oc_node_flows\\.f90$;oc_channel_flow_types\\.f90$;oc_flow_control\\.f90$"
        
        # Group 8: Contaminant supporting modules (must come before CMmod)
        "contaminant_common\\.f90$;contaminant_utilities\\.f90$;contaminant_data_reader\\.f90$"

        # Group 8.5: Plant module (must come before column solver)
        "contaminant_plant\\.f90$"
        
        # Group 9: More contaminant modules
        "contaminant_simulation\\.f90$;contaminant_column_solver\\.f90$;contaminant_link_solver\\.f90$"
        
        # Group 9.5: ET modules (refactored structure - order by dependency)
        "et_variables\\.f90$;et_validation\\.f90$;et_core\\.f90$;et_integration\\.f90$;et_main\\.f90$"
        
        # Group 10: OC interface and physics modules including refactored ETmod and CMmod
        "OCmod2\\.f90$;OCQDQMOD\\.F90$;ETmod\\.f90$;CMmod\\.f90$"
        
        # Group 11: Advanced modules
        "VSmod\\.f90$;OCmod\\.f90$;SYmod\\.f90$;FRmod\\.f90$"
        
        # Group 12: Visualization and control (flexible ordering)
        "visualisation_.*\\.f90$;increment_utilities\\.f90$"
        
        # Group 13: Simulation control
        "rest\\.f90$;run_sim\\.f90$"
    )
    
    # Process each group
    foreach(GROUP_PATTERNS ${ORDER_GROUPS})
        string(REPLACE ";" "|" PATTERN_REGEX "${GROUP_PATTERNS}")
        set(NEW_REMAINING)
        
        foreach(FILE ${REMAINING_SOURCES})
            get_filename_component(FILENAME ${FILE} NAME)
            if(FILENAME MATCHES "(${PATTERN_REGEX})")
                list(APPEND ORDERED_SOURCES ${FILE})
                message(STATUS "  Ordered: ${FILENAME}")
            else()
                list(APPEND NEW_REMAINING ${FILE})
            endif()
        endforeach()
        
        set(REMAINING_SOURCES ${NEW_REMAINING})
    endforeach()
    
    # Add any remaining files (new files will automatically be included here)
    foreach(FILE ${REMAINING_SOURCES})
        get_filename_component(FILENAME ${FILE} NAME)
        list(APPEND ORDERED_SOURCES ${FILE})
        message(STATUS "  Auto-added: ${FILENAME}")
    endforeach()
    
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
else()
    message(STATUS "Using pattern-based ordering (dependency analysis disabled)...")
    pattern_based_dependency_sort(MODULE_SOURCES ORDERED_MODULE_SOURCES)
endif()

# Validate source ordering if requested
if(ENABLE_DEPENDENCY_ANALYSIS AND VERBOSE_DEPENDENCY_OUTPUT)
    message(STATUS "Validating dependency resolution...")
    validate_source_order(ORDERED_MODULE_SOURCES)
endif()

# Combine all sources in correct order (modules first, then main program)
set(SHETRAN_SOURCES ${ORDERED_MODULE_SOURCES})
if(MAIN_PROGRAM)
    list(APPEND SHETRAN_SOURCES ${MAIN_PROGRAM})
endif()

# Print discovered sources for verification
message(STATUS "")
message(STATUS "Final source file build order:")
set(SOURCE_INDEX 1)
foreach(SOURCE ${SHETRAN_SOURCES})
    file(RELATIVE_PATH REL_PATH "${CMAKE_SOURCE_DIR}" "${SOURCE}")
    message(STATUS "  ${SOURCE_INDEX}. ${REL_PATH}")
    math(EXPR SOURCE_INDEX "${SOURCE_INDEX} + 1")
endforeach()
list(LENGTH SHETRAN_SOURCES SOURCES_COUNT)
message(STATUS "Total sources found: ${SOURCES_COUNT}")

# Provide information about the dependency analysis
if(ENABLE_DEPENDENCY_ANALYSIS)
    message(STATUS "Source ordering: Automatic dependency analysis (recommended)")
else()
    message(STATUS "Source ordering: Pattern-based fallback")
    message(STATUS "  Note: Enable ENABLE_DEPENDENCY_ANALYSIS for better automatic ordering")
endif()
message(STATUS "")

# Create the executable
add_executable(SHETRAN ${SHETRAN_SOURCES})

# Set target properties
set_target_properties(SHETRAN PROPERTIES
    OUTPUT_NAME "shetran"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Include directories
target_include_directories(SHETRAN PRIVATE ${HDF5_INCLUDE_DIRS})

# Link libraries
target_link_libraries(SHETRAN PRIVATE ${HDF5_LIBRARIES})

# Add high level HDF5 Fortran library explicitly for Linux/Unix
if(NOT WIN32)
    target_link_libraries(SHETRAN PRIVATE hdf5_hl_fortran hdf5_hl_f90cstub)
endif()

# Additional linker flags for large memory model
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    if(CMAKE_Fortran_COMPILER MATCHES "ifort")
        set_target_properties(SHETRAN PROPERTIES LINK_FLAGS "-mcmodel=large")
    elseif(CMAKE_Fortran_COMPILER MATCHES "ifx")
        set_target_properties(SHETRAN PROPERTIES LINK_FLAGS "-mcmodel=large")
    endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set_target_properties(SHETRAN PROPERTIES LINK_FLAGS "-mcmodel=large")
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
