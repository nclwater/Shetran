#!/bin/bash
# tools/check_doc_coverage.sh
# SHETRAN Documentation Coverage Checker
# 
# This script analyzes the FORD documentation coverage across all Fortran source files

echo "SHETRAN Documentation Coverage Report"
echo "===================================="

undocumented=0
total=0

# Find all Fortran files and check for FORD documentation markers
for file in $(find src -name "*.f90" -o -name "*.F90" | sort); do
    total=$((total + 1))
    if ! grep -q "!>" "$file"; then
        echo "UNDOCUMENTED: $file"
        undocumented=$((undocumented + 1))
    fi
done

documented=$((total - undocumented))

# Check if we found any files to avoid division by zero
if [ $total -eq 0 ]; then
    coverage=0
else
    coverage=$((documented * 100 / total))
fi

echo ""
echo "Summary:"
echo "======================================"
echo "Total files: $total"
echo "Documented: $documented"
echo "Undocumented: $undocumented"
echo "Coverage: $coverage%"
echo ""

if [ $coverage -lt 95 ]; then
    echo "WARNING: Documentation coverage is below 95%"
    echo "Target: >95% coverage for production ready documentation"
    exit 1
else
    echo "SUCCESS: Documentation coverage meets target (>95%)"
    exit 0
fi
