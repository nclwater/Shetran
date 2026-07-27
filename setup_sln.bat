@echo off
REM Generate CMake Visual Studio solutions for every supported VS version found.
REM Requirements: CMake 4.4+, Intel ifx, and fypp in the current environment.

setlocal EnableExtensions EnableDelayedExpansion
pushd "%~dp0"

set "MIN_CMAKE_MAJOR=4"
set "MIN_CMAKE_MINOR=4"
set "VS_COUNT=0"
set "FAILURES=0"

echo SHETRAN Visual Studio Solution Setup
echo ====================================

REM CMake 4.4 introduced the Visual Studio 18 2026 generator used by this
REM project and is the minimum supported version for this script.
where cmake >nul 2>&1
if errorlevel 1 (
    echo ERROR: CMake was not found in PATH.
    goto failed
)

set "CMAKE_VERSION="
for /f "tokens=3" %%V in ('cmake --version 2^>^&1 ^| findstr /b /c:"cmake version"') do set "CMAKE_VERSION=%%V"
if not defined CMAKE_VERSION (
    echo ERROR: Could not determine the installed CMake version.
    goto failed
)

for /f "tokens=1,2 delims=." %%A in ("%CMAKE_VERSION%") do (
    set "CMAKE_MAJOR=%%A"
    set "CMAKE_MINOR=%%B"
)

if %CMAKE_MAJOR% LSS %MIN_CMAKE_MAJOR% (
    echo ERROR: CMake %CMAKE_VERSION% is too old. CMake 4.4 or newer is required.
    goto failed
)
if %CMAKE_MAJOR% EQU %MIN_CMAKE_MAJOR% if %CMAKE_MINOR% LSS %MIN_CMAKE_MINOR% (
    echo ERROR: CMake %CMAKE_VERSION% is too old. CMake 4.4 or newer is required.
    goto failed
)
echo INFO: Found CMake %CMAKE_VERSION%.

REM fypp must come from the environment in which this script is run. Passing
REM its absolute path to CMake makes the generated projects independent of the
REM environment from which Visual Studio is subsequently launched.
set "FYPP_EXE="
for /f "delims=" %%F in ('where fypp 2^>nul') do if not defined FYPP_EXE set "FYPP_EXE=%%~fF"
if not defined FYPP_EXE (
    echo ERROR: fypp was not found in the current environment.
    echo ERROR: Activate the SHETRAN Python environment first, for example:
    echo ERROR:     conda activate shetran
    goto failed
)
echo INFO: Found fypp: %FYPP_EXE%

REM The Intel compiler is normally available after opening a oneAPI prompt.
REM Try the standard oneAPI setup scripts when it is not already on PATH.
where ifx >nul 2>&1
if errorlevel 1 (
    if exist "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" (
        call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 >nul 2>&1
    ) else if exist "C:\Program Files\Intel\oneAPI\setvars.bat" (
        call "C:\Program Files\Intel\oneAPI\setvars.bat" intel64 >nul 2>&1
    )
)
where ifx >nul 2>&1
if errorlevel 1 (
    echo ERROR: Intel ifx was not found. Install the oneAPI HPC Toolkit or run
    echo ERROR: this script from an Intel oneAPI command prompt.
    goto failed
)
echo INFO: Found Intel ifx.

set "VSWHERE=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
if not exist "%VSWHERE%" set "VSWHERE=%ProgramFiles%\Microsoft Visual Studio\Installer\vswhere.exe"
if not exist "%VSWHERE%" (
    echo ERROR: vswhere.exe was not found. Install Visual Studio with the
    echo ERROR: Desktop development tools and try again.
    goto failed
)
for %%I in ("%VSWHERE%") do set "VSWHERE_DIR=%%~dpI"

REM Check each Visual Studio generation supported by current Intel ifx/CMake.
REM Each installed major version receives an independent CMake cache.
call :generate_solution 18 19 2026 "Visual Studio 18 2026"
call :generate_solution 17 18 2022 "Visual Studio 17 2022"
call :generate_solution 16 17 2019 "Visual Studio 16 2019"

if "%VS_COUNT%"=="0" (
    echo ERROR: No supported Visual Studio installation was found.
    echo ERROR: Supported versions are Visual Studio 2019, 2022, and 2026.
    goto failed
)

echo.
if not "%FAILURES%"=="0" (
    echo ERROR: %FAILURES% Visual Studio solution^(s^) could not be generated.
    popd
    exit /b 1
)

echo SUCCESS: Generated %VS_COUNT% Visual Studio solution^(s^).
echo Open the SHETRAN.sln or SHETRAN.slnx file in each build\vs_VERSION directory.
popd
exit /b 0

:generate_solution
set "VS_MAJOR=%~1"
set "VS_NEXT_MAJOR=%~2"
set "VS_YEAR=%~3"
set "VS_GENERATOR=%~4"
set "VS_INSTANCE="

pushd "%VSWHERE_DIR%" >nul
for /f "delims=" %%I in ('vswhere.exe -latest -products * -version "[%VS_MAJOR%.0,%VS_NEXT_MAJOR%.0)" -requires Microsoft.Component.MSBuild Microsoft.VisualStudio.Component.CoreEditor -property installationPath') do set "VS_INSTANCE=%%I"
popd
if not defined VS_INSTANCE exit /b 0

set /a VS_COUNT+=1
set "BUILD_DIR=build\vs_%VS_YEAR%"

echo.
echo INFO: Found Visual Studio %VS_YEAR%: %VS_INSTANCE%
echo INFO: Generating %BUILD_DIR% using "%VS_GENERATOR%"...

REM Always start with a clean generator-specific build tree. BUILD_DIR is
REM constructed above from a fixed prefix and a known Visual Studio year.
if exist "%BUILD_DIR%" (
    echo INFO: Removing existing directory: %BUILD_DIR%
    rmdir /s /q "%BUILD_DIR%"
    if exist "%BUILD_DIR%" (
        echo ERROR: Could not remove %BUILD_DIR%.
        echo ERROR: Close Visual Studio and any process using that directory,
        echo ERROR: then run this script again.
        set /a FAILURES+=1
        exit /b 0
    )
)

cmake -S . -B "%BUILD_DIR%" ^
    -G "%VS_GENERATOR%" ^
    -A x64 ^
    -T "fortran=ifx" ^
    "-DFYPP:FILEPATH=%FYPP_EXE%" ^
    "-DCMAKE_GENERATOR_INSTANCE=%VS_INSTANCE%"

if errorlevel 1 (
    echo ERROR: Generation failed for Visual Studio %VS_YEAR%.
    set /a FAILURES+=1
) else (
    if exist "%BUILD_DIR%\SHETRAN.slnx" (
        echo INFO: Generated %BUILD_DIR%\SHETRAN.slnx
    ) else if exist "%BUILD_DIR%\SHETRAN.sln" (
        echo INFO: Generated %BUILD_DIR%\SHETRAN.sln
    ) else (
        echo INFO: Generated Visual Studio build tree: %BUILD_DIR%
    )
)
exit /b 0

:failed
popd
exit /b 1
