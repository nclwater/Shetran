# Shared assertion support: a core owning the counters and pass/fail decision,
# one module per asserted type, and a facade the test programs use. Every test
# target compiles these; they are small and carry no state between targets.
set(SHETRAN_TEST_SUPPORT_SOURCES
    "${CMAKE_CURRENT_SOURCE_DIR}/support/test_support_core.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/support/test_support_integer.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/support/test_support_character.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/support/test_support_real.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/support/shetran_test_support.f90"
)

set(VISUALISATION_TEST_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}/modules")
set(VISUALISATION_TEST_DATA_DIR "${CMAKE_CURRENT_BINARY_DIR}/data")
set(VISUALISATION_TEST_SCRATCH_DIR "${CMAKE_CURRENT_BINARY_DIR}/scratch")

file(MAKE_DIRECTORY
    "${VISUALISATION_TEST_MODULE_DIR}"
    "${VISUALISATION_TEST_DATA_DIR}"
    "${VISUALISATION_TEST_SCRATCH_DIR}"
)

foreach(fixture
    records_basic.txt
    strip_input.txt
    strip_expected.txt
)
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/visualisation_read/data/${fixture}"
        "${VISUALISATION_TEST_DATA_DIR}/${fixture}"
        COPYONLY
    )
endforeach()

# CMake writes this fixture without a trailing record terminator.
file(WRITE "${VISUALISATION_TEST_DATA_DIR}/records_no_final_newline.txt" "final")

add_executable(visualisation_read_tests
    "${CMAKE_SOURCE_DIR}/src/parameters/mod_parameters.f90"
    "${CMAKE_SOURCE_DIR}/src/visualisation/visualisation_read_parser.f90"
    "${CMAKE_SOURCE_DIR}/src/visualisation/visualisation_read.f90"
    ${SHETRAN_TEST_SUPPORT_SOURCES}
    "${CMAKE_CURRENT_SOURCE_DIR}/visualisation_read/test_visualisation_read.f90"
)

set_target_properties(visualisation_read_tests PROPERTIES
    Fortran_MODULE_DIRECTORY "${VISUALISATION_TEST_MODULE_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/bin"
)

target_include_directories(visualisation_read_tests PRIVATE
    "${VISUALISATION_TEST_MODULE_DIR}"
)

apply_shetran_fortran_options(visualisation_read_tests)

add_test(
    NAME visualisation_read.unit
    COMMAND $<TARGET_FILE:visualisation_read_tests> unit "${VISUALISATION_TEST_DATA_DIR}"
)
set_tests_properties(visualisation_read.unit PROPERTIES
    WORKING_DIRECTORY "${VISUALISATION_TEST_SCRATCH_DIR}"
    LABELS "visualisation;unit"
)

# The OC row-width derivation is deliberately dependency-free, so this target
# compiles only that module and the shared assertion support.
set(OC_ROW_WIDTH_TEST_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}/modules_oc_row_width")
file(MAKE_DIRECTORY "${OC_ROW_WIDTH_TEST_MODULE_DIR}")

add_executable(oc_row_width_tests
    "${CMAKE_SOURCE_DIR}/src/modules/oc_row_width.f90"
    ${SHETRAN_TEST_SUPPORT_SOURCES}
    "${CMAKE_CURRENT_SOURCE_DIR}/oc_row_width/test_oc_row_width.f90"
)

set_target_properties(oc_row_width_tests PROPERTIES
    Fortran_MODULE_DIRECTORY "${OC_ROW_WIDTH_TEST_MODULE_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/bin"
)

target_include_directories(oc_row_width_tests PRIVATE
    "${OC_ROW_WIDTH_TEST_MODULE_DIR}"
)

apply_shetran_fortran_options(oc_row_width_tests)

add_test(
    NAME oc_row_width.unit
    COMMAND $<TARGET_FILE:oc_row_width_tests>
)
set_tests_properties(oc_row_width.unit PROPERTIES
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    LABELS "oc;unit"
)

file(GLOB VISUALISATION_PLAN_SOURCES
    LIST_DIRECTORIES FALSE
    "${CMAKE_SOURCE_DIR}/examples/*/model/*visualisation_plan*.txt"
)
list(SORT VISUALISATION_PLAN_SOURCES)

set(VISUALISATION_PLAN_FIXTURES)
foreach(plan IN LISTS VISUALISATION_PLAN_SOURCES)
    file(RELATIVE_PATH relative_plan "${CMAKE_SOURCE_DIR}" "${plan}")
    if(relative_plan MATCHES "^examples/_")
        continue()
    endif()

    string(REGEX MATCH "^examples/([^/]+)/model/(.+)$" matched_plan "${relative_plan}")
    if(NOT matched_plan)
        message(FATAL_ERROR "Unexpected visualisation plan path: ${relative_plan}")
    endif()

    set(example_name "${CMAKE_MATCH_1}")
    set(plan_name "${CMAKE_MATCH_2}")
    set(fixture_directory
        "${CMAKE_CURRENT_BINARY_DIR}/fixtures/visualisation_plans/${example_name}/model")
    set(fixture_path "${fixture_directory}/${plan_name}")
    file(MAKE_DIRECTORY "${fixture_directory}")
    configure_file("${plan}" "${fixture_path}" COPYONLY)
    list(APPEND VISUALISATION_PLAN_FIXTURES "${fixture_path}")
endforeach()

list(LENGTH VISUALISATION_PLAN_FIXTURES VISUALISATION_PLAN_COUNT)
if(VISUALISATION_PLAN_COUNT EQUAL 0)
    message(FATAL_ERROR "No non-underscore example visualisation plans were found")
endif()
message(STATUS "Visualisation parser corpus: ${VISUALISATION_PLAN_COUNT} plans")

add_test(
    NAME visualisation_read.examples
    COMMAND $<TARGET_FILE:visualisation_read_tests> examples ${VISUALISATION_PLAN_FIXTURES}
)
set_tests_properties(visualisation_read.examples PROPERTIES
    WORKING_DIRECTORY "${VISUALISATION_TEST_SCRATCH_DIR}"
    LABELS "visualisation;examples"
)
