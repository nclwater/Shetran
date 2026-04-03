@echo off
REM SHETRAN Build Script for Windows (ifx only)
REM Usage: build.bat [OPTIONS]

setlocal enabledelayedexpansion

REM Default values
set BUILD_TYPE=Release
set CLEAN_BUILD=false
set CLEAN_APP_ONLY=false
set VERBOSE=false
set JOBS=%NUMBER_OF_PROCESSORS%
set GENERATE_FORD=false
set DOCS_ONLY=false

REM Parse command line arguments
:parse_args
if "%~1"=="" goto end_parse
if "%~1"=="-t" (
    set BUILD_TYPE=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--type" (
    set BUILD_TYPE=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--clean" (
    set CLEAN_BUILD=true
    shift
    goto parse_args
)
if "%~1"=="--clean-app" (
    set CLEAN_APP_ONLY=true
    shift
    goto parse_args
)
if "%~1"=="-v" (
    set VERBOSE=true
    shift
    goto parse_args
)
if "%~1"=="--verbose" (
    set VERBOSE=true
    shift
    goto parse_args
)
if "%~1"=="-j" (
    set JOBS=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--jobs" (
    set JOBS=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--ford" (
    set GENERATE_FORD=true
    shift
    goto parse_args
)
if "%~1"=="--docs-only" (
    set DOCS_ONLY=true
    set GENERATE_FORD=true
    shift
    goto parse_args
)
if "%~1"=="-h" goto show_usage
if "%~1"=="--help" goto show_usage

echo ERROR: Unknown option: %~1
goto show_usage

:end_parse

echo SHETRAN Build Script for Windows
echo =================================

cd "%~dp0"

REM Validate build type
if /i not "%BUILD_TYPE%"=="Debug" if /i not "%BUILD_TYPE%"=="Release" if /i not "%BUILD_TYPE%"=="ReleaseNative" (
    echo ERROR: Invalid build type: %BUILD_TYPE%
    echo ERROR: Must be Debug, Release, or ReleaseNative
    exit /b 1
)

if "%CLEAN_BUILD%"=="true" if "%CLEAN_APP_ONLY%"=="true" (
    echo ERROR: --clean and --clean-app are mutually exclusive
    exit /b 1
)

if "%DOCS_ONLY%"=="true" goto generate_ford_docs

REM Auto-detect ifx compiler
echo INFO: Checking for ifx compiler...
where ifx >nul 2>&1
if errorlevel 1 (
    echo ERROR: ifx compiler not found in PATH!
    echo ERROR: Please make sure Intel oneAPI is installed and ifx is available.
    echo INFO: Attempting to set up Intel oneAPI environment...

    if exist "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" (
        call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 > nul 2>&1
        goto check_ifx_again
    )
    if exist "C:\Program Files\Intel\oneAPI\setvars.bat" (
        call "C:\Program Files\Intel\oneAPI\setvars.bat" intel64 > nul 2>&1
        goto check_ifx_again
    )

    echo ERROR: Intel oneAPI environment not found. Please install Intel oneAPI HPC Toolkit.
    exit /b 1
)
goto ifx_found

:check_ifx_again
where ifx >nul 2>&1
if errorlevel 1 (
    echo ERROR: ifx still not found after setting up Intel oneAPI environment.
    exit /b 1
)

:ifx_found
echo INFO: Found ifx compiler

REM Check for CMake
where cmake >nul 2>&1
if errorlevel 1 (
    echo ERROR: CMake not found! Please install CMake and add it to your PATH.
    exit /b 1
)
for /f "tokens=3" %%v in ('cmake --version 2^>^&1 ^| findstr /i "version"') do (
    echo INFO: Found CMake version: %%v
)

REM Determine build directory
if /i "%BUILD_TYPE%"=="Debug" (
    set BUILD_DIR=build\debug
) else if /i "%BUILD_TYPE%"=="ReleaseNative" (
    set BUILD_DIR=build\release-native
) else (
    set BUILD_DIR=build\release
)

echo INFO: Build type:      %BUILD_TYPE%
echo INFO: Build directory:  %BUILD_DIR%

REM Clean build directory if requested
if "%CLEAN_BUILD%"=="true" (
    echo INFO: Cleaning build directory: %BUILD_DIR%
    if exist "%BUILD_DIR%" rmdir /s /q "%BUILD_DIR%"
)

REM Clean only SHETRAN build artifacts if requested
if "%CLEAN_APP_ONLY%"=="true" (
    if exist "%BUILD_DIR%" (
        echo INFO: Cleaning SHETRAN artifacts in: %BUILD_DIR%

        if exist "%BUILD_DIR%\bin\shetran.exe" del /q "%BUILD_DIR%\bin\shetran.exe"
        if exist "%BUILD_DIR%\CMakeFiles\SHETRAN.dir" rmdir /s /q "%BUILD_DIR%\CMakeFiles\SHETRAN.dir"

        for %%f in ("%BUILD_DIR%\*.mod" "%BUILD_DIR%\*.smod") do (
            if exist "%%~f" del /q "%%~f"
        )
    ) else (
        echo INFO: Build directory does not exist yet. --clean-app has nothing to clean.
    )
)

REM Create build directory
if not exist "%BUILD_DIR%" mkdir "%BUILD_DIR%"
cd "%BUILD_DIR%"

REM Calculate source path (we are now two levels deep: build/<type>)
set SOURCE_PATH=..\..

REM Configure
echo INFO: Configuring with CMake...
set CMAKE_ARGS=-DCMAKE_BUILD_TYPE=%BUILD_TYPE% -DCMAKE_Fortran_COMPILER=ifx -G "NMake Makefiles"
echo INFO: CMake arguments: %CMAKE_ARGS%
cmake %CMAKE_ARGS% %SOURCE_PATH%
if errorlevel 1 (
    echo ERROR: CMake configuration failed!
    cd "%~dp0"
    exit /b 1
)

REM Build
echo INFO: Building SHETRAN...
if "%VERBOSE%"=="true" (
    nmake SHETRAN VERBOSE=1
) else (
    nmake SHETRAN
)
if errorlevel 1 (
    echo ERROR: Build failed!
    cd "%~dp0"
    exit /b 1
)

echo.
echo SUCCESS: Build completed successfully!
echo.
echo   Compiler:     ifx
echo   Build type:   %BUILD_TYPE%
echo   Build dir:    %BUILD_DIR%
echo   Executable:   %BUILD_DIR%\bin\shetran.exe
echo.

REM Return to source directory
cd "%~dp0"

if "%GENERATE_FORD%"=="true" call :generate_ford_docs_impl

goto end

:generate_ford_docs
echo INFO: Running in documentation-only mode
call :generate_ford_docs_impl
if errorlevel 1 exit /b 1
goto end

:generate_ford_docs_impl
echo INFO: Generating FORD documentation...
where ford >nul 2>&1
if errorlevel 1 (
    echo ERROR: ford command not found in PATH!
    echo ERROR: Install FORD first (for example: pip install ford)
    exit /b 1
)

ford -o docs\ford .\ford_project.md
if errorlevel 1 (
    echo ERROR: FORD documentation generation failed!
    exit /b 1
)

echo INFO: FORD documentation generated at docs\ford\index.html
exit /b 0

:show_usage
echo Usage: %~nx0 [OPTIONS]
echo.
echo Options:
echo   -t, --type TYPE     Build type: Debug, Release, or ReleaseNative (default: Release)
echo   --clean             Clean build directory before building
echo   --clean-app         Clean and rebuild SHETRAN only (keep external libraries)
echo   -v, --verbose       Verbose build output
echo   -j, --jobs N        Number of parallel jobs (default: %NUMBER_OF_PROCESSORS%)
echo   --ford              Generate FORD documentation after a successful build
echo   --docs-only         Generate FORD documentation only ^(no compile^)
echo   -h, --help          Show this help message
echo.
echo Examples:
echo   %~nx0                        Build Release (default)
echo   %~nx0 -t Debug               Build Debug
echo   %~nx0 -t Debug --clean       Clean Debug build
echo   %~nx0 -t Release --clean-app Rebuild SHETRAN only, keep external libs
echo   %~nx0 -t Release --verbose   Verbose Release build
echo   %~nx0 -t ReleaseNative       Maximum local optimization build
echo   %~nx0 --ford                 Build and generate FORD documentation
echo   %~nx0 --docs-only            Generate FORD documentation only
echo.
echo Build Directory Structure:
echo   Debug builds:    build\debug\
echo   Release builds:  build\release\
echo   ReleaseNative:   build\release-native\
echo   Executable:      build\^<type^>\bin\shetran.exe

:end
endlocal
