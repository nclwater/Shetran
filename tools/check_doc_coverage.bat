@echo off
REM tools/check_doc_coverage.bat
REM SHETRAN Documentation Coverage Checker for Windows
REM 
REM This script analyzes the FORD documentation coverage across all Fortran source files

echo SHETRAN Documentation Coverage Report
echo ====================================

set undocumented=0
set total=0

REM Find all Fortran files and check for FORD documentation markers
for /r src %%f in (*.f90 *.F90) do (
    set /a total+=1
    findstr /m "!>" "%%f" >nul
    if errorlevel 1 (
        echo UNDOCUMENTED: %%f
        set /a undocumented+=1
    )
)

set /a documented=total-undocumented
set /a coverage=documented*100/total

echo.
echo Summary:
echo ======================================
echo Total files: %total%
echo Documented: %documented%
echo Undocumented: %undocumented%
echo Coverage: %coverage%%%
echo.

if %coverage% lss 95 (
    echo WARNING: Documentation coverage is below 95%%
    echo Target: ^>95%% coverage for production ready documentation
    exit /b 1
) else (
    echo SUCCESS: Documentation coverage meets target ^(^>95%%^)
    exit /b 0
)
