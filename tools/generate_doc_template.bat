@echo off
REM tools/generate_doc_template.bat
REM SHETRAN FORD Documentation Template Generator for Windows
REM 
REM Usage: generate_doc_template.bat ModuleName > template.f90

if "%1"=="" (
    echo Usage: %0 ModuleName
    echo.
    echo Generates a FORD-compliant Fortran module template with proper documentation.
    echo Output should be redirected to a file:
    echo   %0 MyModule ^> src\compute\MyModule.f90
    exit /b 1
)

set module_name=%1
set current_date=%date:~10,4%-%date:~4,2%-%date:~7,2%

echo !^> @file %module_name%.f90
echo !^> @brief Brief description of %module_name% functionality
echo !^> @author Author Name, Institution
echo !^> @date %current_date%
echo !^> @version 1.0
echo !^>
echo !^> @details
echo !! Detailed description of the %module_name% module.
echo !! Include purpose, algorithms, and usage patterns.
echo !!
echo !! This module provides:
echo !! - Feature 1: Description
echo !! - Feature 2: Description  
echo !! - Feature 3: Description
echo !^>
echo !^> @note Implementation notes and important considerations
echo !^> @warning Usage warnings or limitations
echo !^> @todo Future enhancements or known issues
echo !^> @see Related modules or external references
echo !^>
echo !^> **Historical Information:**
echo !! @note Creation date: %current_date%
echo !! @note Initial implementation by: Author Name
echo.
echo MODULE %module_name%
echo    IMPLICIT NONE
echo    PRIVATE
echo.
echo    ! Module parameters
echo    integer, parameter :: MODULE_VERSION = 1
echo.
echo    ! Module variables
echo    logical :: is_initialized = .false.  !^< Module initialization status
echo.
echo    ! Public interfaces
set lower_name=%module_name%
for %%i in (a b c d e f g h i j k l m n o p q r s t u v w x y z) do call set lower_name=%%lower_name:%%i=%%i%%
echo    PUBLIC :: initialize_%lower_name%
echo    PUBLIC :: finalize_%lower_name%
echo    PUBLIC :: procedure_name
echo.
echo CONTAINS
echo.
echo    !^> @brief Initialize the %module_name% module
echo    !^> @details
echo    !! Performs module initialization including:
echo    !! - Parameter validation
echo    !! - Memory allocation
echo    !! - Initial state setup
echo    !^>
echo    !^> @param[out] success Initialization success flag
echo    !^> @note Must be called before using other module procedures
echo    SUBROUTINE initialize_%lower_name%^(success^)
echo       LOGICAL, INTENT^(OUT^) :: success
echo       
echo       ! Implementation
echo       is_initialized = .true.
echo       success = .true.
echo    END SUBROUTINE initialize_%lower_name%
echo.
echo    !^> @brief Finalize the %module_name% module
echo    !^> @details
echo    !! Performs cleanup including:
echo    !! - Memory deallocation
echo    !! - Resource release
echo    !! - State reset
echo    !^>
echo    !^> @note Should be called when module is no longer needed
echo    SUBROUTINE finalize_%lower_name%^(^)
echo       ! Implementation
echo       is_initialized = .false.
echo    END SUBROUTINE finalize_%lower_name%
echo.
echo    !^> @brief Brief description of main procedure
echo    !^> @details
echo    !! Detailed description of the algorithm, implementation approach,
echo    !! and any special considerations.
echo    !^>
echo    !^> @param[in] input_param Description of input parameter
echo    !^> @param[out] output_param Description of output parameter  
echo    !^> @param[inout] inout_param Description of input/output parameter
echo    !^> @return Description of return value ^(if function^)
echo    !^>
echo    !^> @note Implementation notes
echo    !^> @warning Usage warnings
echo    !^> @see Related procedures or references
echo    !^>
echo    !^> @author Author name
echo    !^> @date %current_date%
echo    SUBROUTINE procedure_name^(input_param, output_param, inout_param^)
echo       ! Parameter declarations with proper documentation
echo       INTEGER, INTENT^(IN^)    :: input_param   !^< Description of input parameter
echo       REAL,    INTENT^(OUT^)   :: output_param  !^< Description of output parameter
echo       LOGICAL, INTENT^(INOUT^) :: inout_param   !^< Description of input/output parameter
echo       
echo       ! Local variables
echo       integer :: local_var  !^< Description of local variable
echo       
echo       ! Procedure implementation
echo       if ^(.not. is_initialized^) then
echo          ! Handle uninitialized module
echo          return
echo       end if
echo       
echo       ! Main algorithm implementation here
echo       
echo    END SUBROUTINE procedure_name
echo.
echo END MODULE %module_name%
