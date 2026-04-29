# Advanced Fortran dependency analysis for CMake
# This file provides functions to analyze Fortran module dependencies
# and perform topological sorting of source files.
#
# Adapted from the Shetran reference implementation.

# Function to extract module names from a Fortran source file
function(extract_fortran_modules SOURCE_FILE PROVIDES_VAR REQUIRES_VAR)
    set(PROVIDES_MODULES)
    set(REQUIRES_MODULES)

    if(EXISTS ${SOURCE_FILE})
        file(READ ${SOURCE_FILE} FILE_CONTENT)

        # Split content into lines for better parsing
        string(REPLACE "\n" ";" FILE_LINES "${FILE_CONTENT}")

        foreach(LINE ${FILE_LINES})
            # Skip comment lines (lines starting with ! or C) and empty lines
            string(STRIP "${LINE}" STRIPPED_LINE)
            if(NOT STRIPPED_LINE MATCHES "^[!Cc]" AND NOT STRIPPED_LINE STREQUAL "")
                string(TOUPPER "${STRIPPED_LINE}" LINE_UPPER)

                # Extract provided modules (MODULE statements at start of line)
                if(LINE_UPPER MATCHES "^[ \t]*MODULE[ \t]+([A-Z_][A-Z0-9_]*)")
                    string(REGEX REPLACE "^[ \t]*MODULE[ \t]+([A-Z_][A-Z0-9_]*).*" "\\1" MODULE_NAME "${LINE_UPPER}")
                    # Exclude procedure modules and other non-standard modules
                    if(NOT MODULE_NAME MATCHES "PROCEDURE" AND
                       NOT MODULE_NAME MATCHES "SUBROUTINE" AND
                       NOT MODULE_NAME MATCHES "FUNCTION")
                        list(APPEND PROVIDES_MODULES ${MODULE_NAME})
                    endif()
                endif()

                # Extract required modules (USE statements at start of line)
                if(LINE_UPPER MATCHES "^[ \t]*USE[ \t]+([A-Z_][A-Z0-9_]*)")
                    string(REGEX REPLACE "^[ \t]*USE[ \t]+([A-Z_][A-Z0-9_]*).*" "\\1" MODULE_NAME "${LINE_UPPER}")
                    # Filter out intrinsic and external modules that we don't need to track
                    if(NOT MODULE_NAME MATCHES "^(ISO_|IEEE_|OMP_|MPI)" AND
                       NOT MODULE_NAME MATCHES "^(IFPORT|IFCORE|KERNEL32)" AND
                       NOT MODULE_NAME MATCHES "^(HDF5|H5|NETCDF)" AND
                       NOT MODULE_NAME MATCHES "^(BLAS|LAPACK)")
                        list(APPEND REQUIRES_MODULES ${MODULE_NAME})
                    endif()
                endif()
            endif()
        endforeach()

        # Remove duplicates
        if(PROVIDES_MODULES)
            list(REMOVE_DUPLICATES PROVIDES_MODULES)
        endif()
        if(REQUIRES_MODULES)
            list(REMOVE_DUPLICATES REQUIRES_MODULES)
        endif()
    endif()

    set(${PROVIDES_VAR} ${PROVIDES_MODULES} PARENT_SCOPE)
    set(${REQUIRES_VAR} ${REQUIRES_MODULES} PARENT_SCOPE)
endfunction()

# Function to perform topological sort of Fortran sources based on module dependencies
function(topological_sort_fortran SOURCES_LIST OUTPUT_VAR)
    set(RESULT)
    set(REMAINING_SOURCES ${${SOURCES_LIST}})

    # Build dependency map
    set(PROVIDES_MAP)
    set(REQUIRES_MAP)
    set(SOURCE_TO_PROVIDES_MAP)
    set(SOURCE_TO_REQUIRES_MAP)

    message(STATUS "Analyzing module dependencies...")

    foreach(SOURCE ${REMAINING_SOURCES})
        extract_fortran_modules(${SOURCE} PROVIDES REQUIRES)

        get_filename_component(SOURCE_NAME ${SOURCE} NAME)

        if(PROVIDES)
            set(SOURCE_TO_PROVIDES_MAP_${SOURCE_NAME} ${PROVIDES})
            foreach(MODULE ${PROVIDES})
                set(PROVIDES_MAP_${MODULE} ${SOURCE})
                message(STATUS "  ${SOURCE_NAME} provides: ${MODULE}")
            endforeach()
        endif()

        if(REQUIRES)
            set(SOURCE_TO_REQUIRES_MAP_${SOURCE_NAME} ${REQUIRES})
            message(STATUS "  ${SOURCE_NAME} requires: ${REQUIRES}")
        endif()
    endforeach()

    message(STATUS "Performing topological sort...")

    # Topological sort implementation
    set(MAX_ITERATIONS 100)
    set(ITERATION 0)

    while(REMAINING_SOURCES AND ITERATION LESS MAX_ITERATIONS)
        set(MADE_PROGRESS FALSE)
        set(NEW_REMAINING)

        foreach(SOURCE ${REMAINING_SOURCES})
            get_filename_component(SOURCE_NAME ${SOURCE} NAME)
            set(CAN_ADD TRUE)

            # Check if all required modules are already provided by sources in RESULT
            if(DEFINED SOURCE_TO_REQUIRES_MAP_${SOURCE_NAME})
                foreach(REQUIRED_MODULE IN LISTS SOURCE_TO_REQUIRES_MAP_${SOURCE_NAME})
                    set(MODULE_FOUND FALSE)

                    # Check if this module is provided by a source already in RESULT
                    foreach(ADDED_SOURCE ${RESULT})
                        get_filename_component(ADDED_SOURCE_NAME ${ADDED_SOURCE} NAME)
                        if(DEFINED SOURCE_TO_PROVIDES_MAP_${ADDED_SOURCE_NAME})
                            list(FIND SOURCE_TO_PROVIDES_MAP_${ADDED_SOURCE_NAME} ${REQUIRED_MODULE} MODULE_INDEX)
                            if(NOT MODULE_INDEX EQUAL -1)
                                set(MODULE_FOUND TRUE)
                                break()
                            endif()
                        endif()
                    endforeach()

                    # Also check if module is provided by same source (internal modules)
                    if(NOT MODULE_FOUND AND DEFINED SOURCE_TO_PROVIDES_MAP_${SOURCE_NAME})
                        list(FIND SOURCE_TO_PROVIDES_MAP_${SOURCE_NAME} ${REQUIRED_MODULE} MODULE_INDEX)
                        if(NOT MODULE_INDEX EQUAL -1)
                            set(MODULE_FOUND TRUE)
                        endif()
                    endif()

                    # Check if it's a system/external module that we don't need to worry about
                    if(NOT MODULE_FOUND)
                        if(REQUIRED_MODULE MATCHES "^(HDF5|H5|NETCDF|MPI_|ISO_|IEEE_|OMP_)" OR
                           REQUIRED_MODULE MATCHES "^(IFPORT|IFCORE|KERNEL32|BLAS|LAPACK)" OR
                           NOT DEFINED PROVIDES_MAP_${REQUIRED_MODULE})
                            set(MODULE_FOUND TRUE)
                        endif()
                    endif()

                    if(NOT MODULE_FOUND)
                        set(CAN_ADD FALSE)
                        break()
                    endif()
                endforeach()
            endif()

            if(CAN_ADD)
                list(APPEND RESULT ${SOURCE})
                set(MADE_PROGRESS TRUE)
                message(STATUS "  Added: ${SOURCE_NAME}")
            else()
                list(APPEND NEW_REMAINING ${SOURCE})
            endif()
        endforeach()

        set(REMAINING_SOURCES ${NEW_REMAINING})
        math(EXPR ITERATION "${ITERATION} + 1")

        if(NOT MADE_PROGRESS AND REMAINING_SOURCES)
            message(WARNING "Circular dependency or unresolved dependencies detected in ${ITERATION} iterations.")
            message(STATUS "Remaining sources to be resolved:")
            foreach(SOURCE ${REMAINING_SOURCES})
                get_filename_component(SOURCE_NAME ${SOURCE} NAME)
                message(STATUS "  - ${SOURCE_NAME}")
                if(DEFINED SOURCE_TO_REQUIRES_MAP_${SOURCE_NAME})
                    message(STATUS "    requires: ${SOURCE_TO_REQUIRES_MAP_${SOURCE_NAME}}")
                endif()
            endforeach()
            message(STATUS "Adding remaining files in alphabetical order...")

            # Sort remaining files alphabetically for consistent builds
            list(SORT REMAINING_SOURCES)

            foreach(SOURCE ${REMAINING_SOURCES})
                get_filename_component(SOURCE_NAME ${SOURCE} NAME)
                message(STATUS "  Force added: ${SOURCE_NAME}")
                list(APPEND RESULT ${SOURCE})
            endforeach()
            break()
        endif()
    endwhile()

    if(ITERATION EQUAL MAX_ITERATIONS)
        message(WARNING "Maximum iterations reached during dependency resolution. Some files may be out of order.")
    endif()

    set(${OUTPUT_VAR} ${RESULT} PARENT_SCOPE)
endfunction()

# Function to validate Fortran source order by checking for common issues
function(validate_source_order SOURCES_LIST)
    message(STATUS "Validating source file order...")

    set(SEEN_MODULES)
    foreach(SOURCE ${${SOURCES_LIST}})
        extract_fortran_modules(${SOURCE} PROVIDES REQUIRES)

        get_filename_component(SOURCE_NAME ${SOURCE} NAME)

        # Check if required modules have been seen
        if(REQUIRES)
            foreach(REQUIRED IN LISTS REQUIRES)
                list(FIND SEEN_MODULES ${REQUIRED} MODULE_INDEX)
                if(MODULE_INDEX EQUAL -1 AND DEFINED PROVIDES_MAP_${REQUIRED})
                    message(WARNING "Module ${REQUIRED} required by ${SOURCE_NAME} but not yet provided")
                endif()
            endforeach()
        endif()

        # Add provided modules to seen list
        if(PROVIDES)
            foreach(PROVIDED IN LISTS PROVIDES)
                list(APPEND SEEN_MODULES ${PROVIDED})
            endforeach()
        endif()
    endforeach()

    message(STATUS "Source order validation complete")
endfunction()
