# Work around CMake 4.4's Windows-IntelLLVM link-rule ordering with ifx IPO.
#
# CMake policy CMP0197 adds the architecture's wrapped -machine option at the
# end of the compiler-driver rule, after /link. Intel ifx's /Qipo option selects
# lld-link, so the linker sees /Qoption,link,-machine:<arch> as an input file.
# Move that compiler-driver option before <LINK_FLAGS>, which CMake emits before
# /link. Remove this override once CMake itself generates that ordering.

if(WIN32
        AND CMAKE_VERSION VERSION_GREATER_EQUAL 4.1
        AND CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM"
        AND NOT CMAKE_GENERATOR MATCHES "Visual Studio")
    foreach(_link_rule IN ITEMS
            CMAKE_Fortran_LINK_EXECUTABLE
            CMAKE_Fortran_CREATE_SHARED_LIBRARY
            CMAKE_Fortran_CREATE_SHARED_MODULE)
        string(REGEX MATCH
            " /Qoption,link,[-/]machine:[^ \r\n]+"
            _machine_option
            "${${_link_rule}}")

        if(_machine_option)
            string(FIND "${${_link_rule}}" "link /out:<TARGET>" _link_position)
            string(FIND "${${_link_rule}}" "${_machine_option}" _machine_position)

            if(_link_position GREATER_EQUAL 0 AND _machine_position GREATER _link_position)
                string(REPLACE "${_machine_option}" "" ${_link_rule} "${${_link_rule}}")
                string(REPLACE
                    " <LINK_FLAGS>"
                    "${_machine_option} <LINK_FLAGS>"
                    ${_link_rule}
                    "${${_link_rule}}")
            endif()
        endif()
    endforeach()

    unset(_link_rule)
    unset(_machine_option)
    unset(_link_position)
    unset(_machine_position)
endif()
