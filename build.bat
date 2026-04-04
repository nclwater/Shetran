@echo off
REM SHETRAN Build Script for Windows (ifx only)
REM Usage: build.bat [OPTIONS]

setlocal enabledelayedexpansion

REM Default values
set BUILD_TYPE=Release
set COMPILER=ifx
set CLEAN_BUILD=false
set CLEAN_APP_ONLY=false
set VERBOSE=false
set JOBS=1
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

REM Validate compiler
if /i not "%COMPILER%"=="ifx" if /i not "%COMPILER%"=="flang" if /i not "%COMPILER%"=="lfortran" (
    echo ERROR: Invalid compiler: %COMPILER%
    echo ERROR: Must be ifx, flang, or lfortran
    exit /b 1
)

if "%CLEAN_BUILD%"=="true" if "%CLEAN_APP_ONLY%"=="true" (
    echo ERROR: --clean and --clean-app are mutually exclusive
    exit /b 1
)

if "%DOCS_ONLY%"=="true" goto generate_ford_docs

REM Check for requested compiler and its dependencies
if /i "%COMPILER%"=="ifx" (
    echo INFO: Checking for ifx compiler...
    where ifx >nul 2>&1
    if errorlevel 1 (
        echo ERROR: ifx compiler not found in PATH!
        echo ERROR: Please make sure Intel oneAPI is installed and ifx is available.
        echo INFO: Attempting to set up Intel oneAPI environment...

        if exist "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" (
            call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 > nul 2>&1
            goto check_ifx_again_in_block
        )
        if exist "C:\Program Files\Intel\oneAPI\setvars.bat" (
            call "C:\Program Files\Intel\oneAPI\setvars.bat" intel64 > nul 2>&1
            goto check_ifx_again_in_block
        )

        echo ERROR: Intel oneAPI environment not found. Please install Intel oneAPI HPC Toolkit.
        exit /b 1
    )
    goto ifx_found_in_block

    :check_ifx_again_in_block
    where ifx >nul 2>&1
    if errorlevel 1 (
        echo ERROR: ifx still not found after setting up Intel oneAPI environment.
        exit /b 1
    )

    :ifx_found_in_block
    echo INFO: Found ifx compiler.
    set CMAKE_C_COMPILER_ARG=
    set CMAKE_RUNTIME_FLAGS=
    set CMAKE_GENERATOR="NMake Makefiles"
    set BUILD_TOOL=nmake
) else if /i "%COMPILER%"=="flang" (
    echo INFO: Checking for flang toolchain...
    where flang >nul 2>&1
    if errorlevel 1 (
        echo ERROR: flang compiler not found in PATH. Please add it to your environment.
        exit /b 1
    )

    set CONDA_ENV_ROOT=%CONDA_PREFIX%
    if not defined CONDA_ENV_ROOT (
        set FLANG_EXE=
        for /f "delims=" %%I in ('where flang 2^>nul') do (
            if not defined FLANG_EXE set "FLANG_EXE=%%~fI"
        )
        if defined FLANG_EXE (
            for %%I in ("!FLANG_EXE!") do set "FLANG_BIN_DIR=%%~dpI"
            for %%I in ("!FLANG_BIN_DIR!..\..") do set "CONDA_ENV_ROOT=%%~fI"
        )
    )

    where clang-cl >nul 2>&1
    if errorlevel 1 (
        where clang >nul 2>&1
        if errorlevel 1 (
            echo ERROR: Neither clang-cl nor clang were found in PATH. One is required for flang.
            exit /b 1
        )
        set C_COMPILER=clang
    ) else (
        set C_COMPILER=clang-cl
    )

    set LLVM_NM=
    for /f "delims=" %%I in ('where llvm-nm 2^>nul') do (
        if not defined LLVM_NM set "LLVM_NM=%%~fI"
    )
    if not defined LLVM_NM (
        echo ERROR: llvm-nm not found in PATH. It is required for Fortran/C symbol mangling checks.
        echo ERROR: Please ensure LLVM bin tools are installed in the selected conda environment.
        exit /b 1
    )

    set FLANG_RUNTIME_FOUND=false
    set FLANG_RT_LIBDIR=
    for /f "delims=" %%F in ('where /r "!CONDA_ENV_ROOT!\Library\lib\clang" flang_rt.runtime.dynamic.lib 2^>nul') do (
        if not defined FLANG_RT_LIBDIR (
            for %%I in ("%%~fF") do set "FLANG_RT_LIBDIR=%%~dpI"
            if "!FLANG_RT_LIBDIR:~-1!"=="\" set "FLANG_RT_LIBDIR=!FLANG_RT_LIBDIR:~0,-1!"
            set FLANG_RUNTIME_FOUND=true
        )
    )

    if "!FLANG_RUNTIME_FOUND!"=="false" (
        echo ERROR: Flang runtime libraries were not found in this environment.
        echo ERROR: Missing file pattern: !CONDA_ENV_ROOT!\Library\lib\clang\*\lib\x86_64-pc-windows-msvc\flang_rt.runtime.dynamic*.lib
        echo ERROR: Install with: conda install -n shetran -c conda-forge flang-rt_win-64=22.1.0
        exit /b 1
    )

    if defined LIB (
        set "LIB=!FLANG_RT_LIBDIR!;!LIB!"
    ) else (
        set "LIB=!FLANG_RT_LIBDIR!"
    )
    echo INFO: Using flang runtime library directory: !FLANG_RT_LIBDIR!

    echo INFO: Found flang with C compiler !C_COMPILER! and llvm-nm.
    set CMAKE_C_COMPILER_ARG=-DCMAKE_C_COMPILER=!C_COMPILER!
    set CMAKE_RUNTIME_FLAGS=-DCMAKE_NM=!LLVM_NM! -DCMAKE_TRY_COMPILE_CONFIGURATION=Release
    set CMAKE_GENERATOR="Ninja"
    set BUILD_TOOL=ninja
) else if /i "%COMPILER%"=="lfortran" (
    echo INFO: Checking for lfortran compiler...
    where lfortran >nul 2>&1
    if errorlevel 1 (
        echo ERROR: lfortran compiler not found in PATH. Please add it to your environment.
        exit /b 1
    )
    echo INFO: Found lfortran compiler.
    set CMAKE_C_COMPILER_ARG=
    set CMAKE_RUNTIME_FLAGS=
    set CMAKE_GENERATOR="Ninja"
    set BUILD_TOOL=ninja
)

REM Check for CMake
where cmake >nul 2>&1
if errorlevel 1 (
    echo ERROR: CMake not found! Please install CMake and add it to your PATH.
    exit /b 1
)
for /f "tokens=3" %%v in ('cmake --version 2^>^&1 ^| findstr /i "version"') do (
    echo INFO: Found CMake version: %%v
)

REM Check for Ninja if selected
if "%BUILD_TOOL%"=="ninja" (
    where ninja >nul 2>&1
    if errorlevel 1 (
        echo ERROR: Ninja build tool not found in PATH!
        echo ERROR: It is required when building with %COMPILER%.
        echo INFO: You can install it via 'pip install ninja' or 'conda install ninja'.
        exit /b 1
    )
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
echo INFO: Compiler:        %COMPILER%
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
set CMAKE_ARGS=-DCMAKE_BUILD_TYPE=%BUILD_TYPE% -DCMAKE_Fortran_COMPILER=%COMPILER% %CMAKE_C_COMPILER_ARG% %CMAKE_RUNTIME_FLAGS% -G %CMAKE_GENERATOR%
echo INFO: CMake arguments: !CMAKE_ARGS!
cmake !CMAKE_ARGS! %SOURCE_PATH%
if errorlevel 1 (
    echo ERROR: CMake configuration failed!
    cd "%~dp0"
    exit /b 1
)

REM Build
echo INFO: Building SHETRAN...
if "%VERBOSE%"=="true" (
    if "%BUILD_TOOL%"=="nmake" (
        nmake SHETRAN VERBOSE=1
    ) else (
        ninja -v -j %JOBS% SHETRAN
    )
) else (
    if "%BUILD_TOOL%"=="nmake" (
        nmake SHETRAN
    ) else (
        ninja -j %JOBS% SHETRAN
    )
)
if errorlevel 1 (
    echo ERROR: Build failed!
    cd "%~dp0"
    exit /b 1
)

echo.
echo SUCCESS: Build completed successfully!
echo.
echo   Compiler:     %COMPILER%
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
echo   -c, --compiler COMPILER Compiler: ifx, flang, or lfortran (default: ifx)
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
echo   %~nx0 -c flang               Build with flang/clang
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
