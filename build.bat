@echo off
REM SHETRAN Build Script for Windows
REM This script helps configure and build SHETRAN on Windows

setlocal enabledelayedexpansion

REM Default values
set BUILD_TYPE=Release
set COMPILER=auto
set BUILD_DIR=
set INSTALL_PREFIX=
set CLEAN_BUILD=false
set VERBOSE=false
set JOBS=%NUMBER_OF_PROCESSORS%
set RUN_TESTS=false
set DO_INSTALL=false
set GENERATE_DOCS=false
set DOCS_ONLY=false
set ENABLE_LARGE_MEMORY=true
set HEAP_SIZE=
set STACK_SIZE=
set USE_WINDOWS_INTEL_GETDIRQQ=false

REM Parse command line arguments
:parse_args
if "%~1"=="" goto end_parse
if "%~1"=="-c" (
    set COMPILER=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--compiler" (
    set COMPILER=%~2
    shift
    shift
    goto parse_args
)
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
if "%~1"=="-d" (
    set BUILD_DIR=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--build-dir" (
    set BUILD_DIR=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="-p" (
    set INSTALL_PREFIX=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--prefix" (
    set INSTALL_PREFIX=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--clean" (
    set CLEAN_BUILD=true
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
if "%~1"=="--test" (
    set RUN_TESTS=true
    shift
    goto parse_args
)
if "%~1"=="--install" (
    set DO_INSTALL=true
    shift
    goto parse_args
)
if "%~1"=="--docs" (
    set DOCS_ONLY=true
    shift
    goto parse_args
)
if "%~1"=="--docs-with-build" (
    set GENERATE_DOCS=true
    shift
    goto parse_args
)
if "%~1"=="--disable-large-memory" (
    set ENABLE_LARGE_MEMORY=false
    shift
    goto parse_args
)
if "%~1"=="--heap-size" (
    set HEAP_SIZE=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="--use-windows-intel-getdirqq" (
    set USE_WINDOWS_INTEL_GETDIRQQ=ON
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

REM Auto-detect compiler if not specified
if not "%COMPILER%"=="auto" goto :compiler_detection_done

echo INFO: Auto-detecting Fortran compiler...

where ifort >nul 2>&1
if not errorlevel 1 goto :found_ifort

where ifx >nul 2>&1
if not errorlevel 1 goto :found_ifx

echo ERROR: No supported Intel Fortran compiler found in PATH!
echo ERROR: Please make sure ifort or ifx is available.
exit /b 1

:found_ifort
set COMPILER=ifort
echo INFO: Found Intel Fortran Compiler (Classic): ifort
goto :compiler_detection_done

:found_ifx
set COMPILER=ifx
echo INFO: Found Intel Fortran Compiler (LLVM): ifx
goto :compiler_detection_done

:compiler_detection_done

REM Check for Intel compiler environment
echo INFO: Setting up Intel Fortran compiler environment...
if exist "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" goto :setup_intel_x86
if exist "C:\Program Files\Intel\oneAPI\setvars.bat" goto :setup_intel_x64
goto :setup_intel_notfound

:setup_intel_x86
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 > nul 2>&1
goto :setup_intel_done

:setup_intel_x64
call "C:\Program Files\Intel\oneAPI\setvars.bat" intel64 > nul 2>&1
goto :setup_intel_done

:setup_intel_notfound
echo WARNING: Intel oneAPI environment not found in default locations
echo WARNING: Please make sure Intel Fortran compiler is in your PATH
:setup_intel_done

REM Check for CMake
where cmake >nul 2>&1
if errorlevel 1 (
    echo ERROR: CMake not found! Please install CMake and add it to your PATH
    exit /b 1
)

REM Get CMake version safely
set "CMAKE_VERSION=unknown"
for /f "tokens=2,*" %%a in ('cmake --version') do (
    if /i "%%a"=="version" set "CMAKE_VERSION=%%b"
    if /i "%%a"=="version:" set "CMAKE_VERSION=%%b"
)
echo INFO: Found CMake version: %CMAKE_VERSION%

REM Check for FORD
call :check_ford

REM Handle documentation-only mode
if "%DOCS_ONLY%"=="true" (
    echo INFO: Documentation generation mode
    call :generate_docs
    if %errorlevel% neq 0 exit /b %errorlevel%
    echo SUCCESS: Documentation generation complete!
    goto :eof
)

REM Set BUILD_DIR based on build type if not explicitly set
if "%BUILD_DIR%"=="" (
    if /i "%BUILD_TYPE%"=="Debug" (
        set BUILD_DIR=build\debug
    ) else if /i "%BUILD_TYPE%"=="Release" (
        set BUILD_DIR=build\release
    ) else if /i "%BUILD_TYPE%"=="RelWithDebInfo" (
        set BUILD_DIR=build\relwithdebinfo
    ) else if /i "%BUILD_TYPE%"=="MinSizeRel" (
        set BUILD_DIR=build\minsizerel
    ) else (
        set BUILD_DIR=build\%BUILD_TYPE%
    )
    echo INFO: Using build directory: %BUILD_DIR%
)

REM Clean build directory if requested
if "%CLEAN_BUILD%"=="true" (
    echo INFO: Cleaning build directory: %BUILD_DIR%
    if exist "%BUILD_DIR%" rmdir /s /q "%BUILD_DIR%"
)

REM Create build directory
if not exist "%BUILD_DIR%" mkdir "%BUILD_DIR%"
cd "%BUILD_DIR%"

REM Calculate source path based on build directory depth
set SOURCE_PATH=..
echo "%BUILD_DIR%" | findstr "\" >nul
if not errorlevel 1 (
    set SOURCE_PATH=..\..
)

REM Prepare CMake arguments
set CMAKE_ARGS=-DCMAKE_BUILD_TYPE=%BUILD_TYPE%

if not "%INSTALL_PREFIX%"=="" (
    set CMAKE_ARGS=%CMAKE_ARGS% -DCMAKE_INSTALL_PREFIX=%INSTALL_PREFIX%
)

REM Memory configuration options
if "%ENABLE_LARGE_MEMORY%"=="false" (
    set CMAKE_ARGS=%CMAKE_ARGS% -DENABLE_LARGE_MEMORY_MODEL=OFF
)

if not "%HEAP_SIZE%"=="" (
    set CMAKE_ARGS=%CMAKE_ARGS% -DHEAP_ARRAY_SIZE=%HEAP_SIZE%
)

REM getdirqq version selection
if "%USE_WINDOWS_INTEL_GETDIRQQ%"=="ON" (
    set CMAKE_ARGS=%CMAKE_ARGS% -DUSE_WINDOWS_INTEL_GETDIRQQ=ON
)

REM Set compiler
if "%COMPILER%"=="ifort" (
    set CMAKE_ARGS=%CMAKE_ARGS% -DCMAKE_Fortran_COMPILER=ifort
) else if "%COMPILER%"=="ifx" (
    set CMAKE_ARGS=%CMAKE_ARGS% -DCMAKE_Fortran_COMPILER=ifx
) else (
    echo ERROR: Unsupported compiler: %COMPILER%
    echo ERROR: Only ifort and ifx are supported on Windows
    exit /b 1
)

REM Configure
echo INFO: Configuring with CMake...
echo INFO: CMake arguments: %CMAKE_ARGS%
cmake %CMAKE_ARGS% -G "NMake Makefiles" %SOURCE_PATH%
if errorlevel 1 (
    echo ERROR: CMake configuration failed!
    exit /b 1
)

REM Build
echo INFO: Building SHETRAN...
if "%VERBOSE%"=="true" (
    nmake VERBOSE=1
) else (
    nmake
)
if errorlevel 1 (
    echo ERROR: Build failed!
    exit /b 1
)

echo SUCCESS: Build completed successfully!

REM Run tests if requested
if "%RUN_TESTS%"=="true" (
    echo INFO: Running tests...
    ctest --output-on-failure
)

REM Install if requested
if "%DO_INSTALL%"=="true" (
    echo INFO: Installing SHETRAN...
    nmake install
    echo SUCCESS: Installation completed!
)

REM Generate documentation if requested
if "%GENERATE_DOCS%"=="true" (
    call :generate_docs
    if %errorlevel% neq 0 echo WARNING: Documentation generation failed, but build succeeded
)

REM Show build summary
echo.
echo SUCCESS: SHETRAN build summary:
echo   Compiler:     %COMPILER%
echo   Build type:   %BUILD_TYPE%
echo   Build dir:    %BUILD_DIR%
if not "%INSTALL_PREFIX%"=="" echo   Install dir:  %INSTALL_PREFIX%
echo   Executable:   %BUILD_DIR%\bin\shetran.exe
echo.
echo To run SHETRAN:
echo   %BUILD_DIR%\bin\shetran.exe -f ^<rundata_file^>

goto end

:show_usage
echo Usage: %0 [OPTIONS]
echo.
echo Options:
echo   -c, --compiler COMPILER    Specify compiler: ifort, ifx, or auto (default: auto)
echo   -t, --type TYPE           Build type: Debug, Release, RelWithDebInfo (default: Release)
echo   -d, --build-dir DIR       Build directory (default: build\^<build-type^>)
echo   -p, --prefix PREFIX       Installation prefix
echo   --clean                   Clean build directory before building
echo   -v, --verbose             Verbose build output
echo   -j, --jobs N              Number of parallel build jobs (default: number of CPU cores)
echo   --test                    Run tests after building
echo   --install                 Install after building
echo   --docs                    Generate FORD documentation only (no build)
echo   --docs-with-build         Generate documentation after building
echo   --disable-large-memory    Disable large memory model (mcmodel=large)
echo   --heap-size SIZE          Set heap array size for Intel ifort (default: 100000000)
echo   --use-windows-intel-getdirqq  Use Windows Intel-specific getdirqq (default on Windows)
echo   -h, --help                Show this help message
echo.
echo Build Directory Structure:
echo   Build outputs are organized by build type:
echo   - Debug builds:        build\debug\
echo   - Release builds:      build\release\
echo   - RelWithDebInfo:      build\relwithdebinfo\
echo   - Custom build types:  build\^<type^>\
echo.
echo HDF5 Dependency:
echo   On Windows, HDF5 is automatically downloaded and built from source
echo   using the official HDF Group distribution. The build system handles
echo   compiler selection automatically for optimal compatibility.
echo.
echo Examples:
echo   %0                        # Build with auto-detected compiler
echo   %0 -c ifx -t Debug        # Build with Intel ifx compiler in debug mode
echo   %0 --clean                # Clean build with auto-detected compiler
echo   %0 --install -p C:\SHETRAN # Build and install to C:\SHETRAN
echo   %0 --docs                 # Generate FORD documentation only
echo   %0 --docs-with-build      # Build and generate documentation
echo   %0 --disable-large-memory  # Build without large memory model
echo   %0 --heap-size 50000000   # Build with smaller heap arrays

goto :end

:check_ford
where ford >nul 2>&1
if %errorlevel% == 0 (
    for /f "tokens=*" %%i in ('ford --version 2^>nul') do set FORD_VERSION=%%i
    if not defined FORD_VERSION set FORD_VERSION=unknown
    echo INFO: Found FORD version: %FORD_VERSION%
    echo INFO: Documentation generation available with 'nmake docs'
) else (
    echo WARNING: FORD not found. Install with: pip install ford
    echo WARNING: Documentation generation will be unavailable
)
goto :eof

:generate_docs
echo INFO: Generating FORD documentation...

REM Check if FORD is available
where ford >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: FORD not found! Please install with: pip install ford
    exit /b 1
)

REM Save current directory
set CURRENT_DIR=%cd%

REM Find the source root directory (where ford_project.md should be)
if exist "ford_project.md" (
    set SOURCE_ROOT=%cd%
) else if exist "..\ford_project.md" (
    cd ..
    set SOURCE_ROOT=%cd%
) else if exist "..\..\ford_project.md" (
    cd ..\..
    set SOURCE_ROOT=%cd%
) else (
    REM Search upward for ford_project.md
    set FOUND_CONFIG=false
    set TEST_DIR=%cd%
    :search_loop
    if exist "%TEST_DIR%\ford_project.md" (
        set SOURCE_ROOT=%TEST_DIR%
        set FOUND_CONFIG=true
        goto end_search
    )
    for %%i in ("%TEST_DIR%") do set PARENT_DIR=%%~dpi
    if "%PARENT_DIR%"=="%TEST_DIR%\" goto end_search
    set TEST_DIR=%PARENT_DIR:~0,-1%
    goto search_loop
    :end_search
    
    if "%FOUND_CONFIG%"=="false" (
        echo ERROR: FORD configuration file 'ford_project.md' not found!
        echo ERROR: Searched from current directory up to filesystem root
        cd /d "%CURRENT_DIR%"
        exit /b 1
    )
)

echo INFO: Found FORD configuration in: %SOURCE_ROOT%
cd /d "%SOURCE_ROOT%"

REM Run FORD to generate documentation
echo INFO: Running FORD documentation generator...
ford ford_project.md
if %errorlevel% == 0 (
    echo SUCCESS: Documentation generated successfully!
    echo INFO: Documentation available in: %SOURCE_ROOT%\docs\ford\
    echo INFO: Open docs\ford\index.html in a web browser to view
) else (
    echo ERROR: Documentation generation failed!
    cd /d "%CURRENT_DIR%"
    exit /b 1
)

REM Return to original directory
cd /d "%CURRENT_DIR%"
goto :eof

:end
REM Return to source directory if we're in a build directory
if not "%CD%"=="%~dp0" (
    cd "%~dp0"
)
endlocal
