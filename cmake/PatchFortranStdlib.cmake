if(NOT DEFINED STDLIB_SOURCE_DIR)
    message(FATAL_ERROR "STDLIB_SOURCE_DIR was not supplied")
endif()

set(_stdlib_helper "${STDLIB_SOURCE_DIR}/cmake/stdlib.cmake")
if(NOT EXISTS "${_stdlib_helper}")
    message(FATAL_ERROR "Fortran stdlib helper not found: ${_stdlib_helper}")
endif()

file(READ "${_stdlib_helper}" _stdlib_helper_contents)
set(_patch_marker "SHETRAN_VISUAL_STUDIO_PREGENERATE_FYPP_V2")

if(NOT _stdlib_helper_contents MATCHES "${_patch_marker}")
    file(APPEND "${_stdlib_helper}" [=[

# SHETRAN_VISUAL_STUDIO_PREGENERATE_FYPP_V2
# CMake's Intel Visual Studio generator needs generated Fortran sources to
# exist while the solution is generated so it can discover module and
# submodule dependencies.  Retain the normal custom command for incremental
# rebuilds, but also produce each fypp output during initial configuration.
function(preprocess preproc preprocopts srcext trgext srcfiles trgfiles)
  set(_trgfiles)
  foreach(srcfile IN LISTS srcfiles)
    get_filename_component(filename ${srcfile} NAME)
    string(REGEX REPLACE "\\.${srcext}$" ".${trgext}" trgfile ${filename})
    set(_source "${CMAKE_CURRENT_SOURCE_DIR}/${srcfile}")
    set(_output "${CMAKE_CURRENT_BINARY_DIR}/${trgfile}")

    if(CMAKE_GENERATOR MATCHES "^Visual Studio")
      # stdlib expresses configure-known feature booleans as generator
      # expressions for its build-time command.  Evaluate those simple BOOL
      # expressions before invoking fypp during configuration.
      set(_configure_preprocopts)
      foreach(_preprocopt IN LISTS preprocopts)
        if(_preprocopt MATCHES "^(.*)\\$<BOOL:([^>]*)>(.*)$")
          set(_bool_prefix "${CMAKE_MATCH_1}")
          set(_bool_argument "${CMAKE_MATCH_2}")
          set(_bool_suffix "${CMAKE_MATCH_3}")
          if(_bool_argument)
            set(_bool_result 1)
          else()
            set(_bool_result 0)
          endif()
          list(APPEND _configure_preprocopts
            "${_bool_prefix}${_bool_result}${_bool_suffix}")
        else()
          list(APPEND _configure_preprocopts "${_preprocopt}")
        endif()
      endforeach()

      execute_process(
        COMMAND "${preproc}" ${_configure_preprocopts} "${_source}" "${_output}"
        RESULT_VARIABLE _preprocess_result
        OUTPUT_VARIABLE _preprocess_stdout
        ERROR_VARIABLE _preprocess_stderr
      )
      if(NOT _preprocess_result EQUAL 0)
        message(FATAL_ERROR
          "fypp failed while generating ${_output}\n"
          "${_preprocess_stdout}${_preprocess_stderr}")
      endif()
    endif()

    add_custom_command(
      OUTPUT "${_output}"
      COMMAND "${preproc}" ${preprocopts} "${_source}" "${_output}"
      MAIN_DEPENDENCY "${_source}"
      VERBATIM
    )
    list(APPEND _trgfiles "${_output}")
  endforeach()
  set(${trgfiles} ${_trgfiles} PARENT_SCOPE)
endfunction()
]=])
endif()
