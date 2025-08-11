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
        # Intel Fortran Compiler (Classic)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mcmodel=large -heap-arrays 100000000")
        set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -traceback -check bounds -check uninit")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
    elseif(CMAKE_Fortran_COMPILER MATCHES "ifx")
        # Intel Fortran Compiler (LLVM-based)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mcmodel=large")
        # Note: heap-arrays is not available in ifx, use alternative approach
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fstack-arrays")
        set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -traceback -check bounds -check uninit")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
    endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # GNU Fortran Compiler
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mcmodel=large")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fmax-stack-var-size=100000000")
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
    else()
        message(FATAL_ERROR "HDF5 not found. Please install HDF5 development packages.")
    endif()
endif()

# Source file organization
set(SHETRAN_SOURCES
    # Main program
    src/Shetran.f90
    
    # Parameters
    src/parameters/sglobal.f90
    src/parameters/mod_parameters.f90
    src/parameters/CONST_SY.F90
    src/parameters/AL_G.F90
    src/parameters/AL_C.F90
    src/parameters/AL_D.f90
    src/parameters/CONT_CC.F90
    src/parameters/is_cc.f90
    src/parameters/link_cc.f90
    src/parameters/link_cc1.f90
    src/parameters/link_cw.f90
    src/parameters/colm_c1.f90
    src/parameters/colm_c2.f90
    src/parameters/colm_cc.f90
    src/parameters/colm_cc1.f90
    src/parameters/colm_cg.f90
    src/parameters/colm_co.f90
    
    # Utility modules
    src/util/mod_load_filedata.f90
    
    # Main modules
    src/modules/utilsmod.f90
    src/modules/getdirqq.f90
    src/modules/CMmod.f90
    src/modules/ETmod.f90
    src/modules/FRmod.f90
    src/modules/OCmod.f90
    src/modules/OCmod2.f90
    src/modules/OCQDQMOD.F90
    src/modules/SED_CS.F90
    src/modules/SMmod.f90
    src/modules/SYmod.f90
    src/modules/VSmod.f90
    src/modules/ZQmod.f90
    src/modules/bk_cw.f90
    src/modules/plant_cc.f90
    src/modules/rest.f90
    src/modules/run_sim.f90
    src/modules/sed_co.f90
    
    # Visualization modules
    src/visualisation/visualisation_extras.f90
    src/visualisation/visualisation_structure.f90
    src/visualisation/visualisation_pass.f90
    src/visualisation/visualisation_read.f90
    src/visualisation/visualisation_metadata.f90
    src/visualisation/visualisation_map.f90
    src/visualisation/visualisation_hdf5.f90
    src/visualisation/visualisation_interface_centre.f90
    src/visualisation/visualisation_interface_left.f90
    src/visualisation/visualisation_interface_right.f90
    src/visualisation/visualisation_interface_far_right.f90
)

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
