@echo off
REM SHETRAN Build Script for Windows
REM This script helps configure and build SHETRAN on Windows

setlocal enabledelayedexpansion

REM Default values
set BUILD_TYPE=Release
set COMPILER=auto
set BUILD_DIR=build
set INSTALL_PREFIX=
set CLEAN_BUILD=false
set VERBOSE=false
set JOBS=%NUMBER_OF_PROCESSORS%
set RUN_TESTS=false
set DO_INSTALL=false
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

REM Clean build directory if requested
if "%CLEAN_BUILD%"=="true" (
    echo INFO: Cleaning build directory: %BUILD_DIR%
    if exist "%BUILD_DIR%" rmdir /s /q "%BUILD_DIR%"
)

REM Create build directory
if not exist "%BUILD_DIR%" mkdir "%BUILD_DIR%"
cd "%BUILD_DIR%"

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
cmake %CMAKE_ARGS% -G "NMake Makefiles" ..
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
echo   -d, --build-dir DIR       Build directory (default: build)
echo   -p, --prefix PREFIX       Installation prefix
echo   --clean                   Clean build directory before building
echo   -v, --verbose             Verbose build output
echo   -j, --jobs N              Number of parallel build jobs (default: number of CPU cores)
echo   --test                    Run tests after building
echo   --install                 Install after building
echo   --disable-large-memory    Disable large memory model (mcmodel=large)
echo   --heap-size SIZE          Set heap array size for Intel ifort (default: 100000000)
echo   --use-windows-intel-getdirqq  Use Windows Intel-specific getdirqq (default on Windows)
echo   -h, --help                Show this help message
echo.
echo Examples:
echo   %0                        # Build with auto-detected compiler
echo   %0 -c ifx -t Debug        # Build with Intel ifx compiler in debug mode
echo   %0 --clean                # Clean build with ifort
echo   %0 --install -p C:\SHETRAN # Build and install to C:\SHETRAN
echo   %0 --disable-large-memory  # Build without large memory model
echo   %0 --heap-size 50000000   # Build with smaller heap arrays

:end
cd ..
endlocal
