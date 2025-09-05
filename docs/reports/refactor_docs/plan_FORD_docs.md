# FORD Documentation Standardization Plan

**Report Date:** 5 September 2025  
**Project:** SHETRAN Hydrological Model  
**Phase:** Phase 1 & 2 Complete, Phase 3 In Progress  
**Status:** ✅ Infrastructure Complete, 🚧 Implementation Phase Active  

---

## Executive Summary ✅ **UPDATED**

This plan outlined the complete standardization of SHETRAN's Fortran documentation using the FORD (Fortran Documenter) tool. **Phase 1 (Infrastructure Setup) and Phase 2 (Documentation Standards) are now complete.** The FORD documentation system is fully operational with 12% baseline coverage (14/114 files) established. Phase 3 (Implementation) is in progress with the main program file serving as an exemplary documentation model.

**✅ COMPLETED ACHIEVEMENTS:**
- FORD infrastructure fully implemented and tested
- Cross-platform build system integration (Linux/Windows)
- Documentation coverage and template generation tools
- Exemplary documentation created for `src/Shetran.f90`
- VS Code integration and automated workflows established

**🚧 CURRENT STATUS:**
- Ready for systematic documentation of remaining 100+ Fortran files
- All tools and infrastructure in place for efficient Phase 3 execution
- Template generators and coverage monitoring operational

---

## Current Documentation Assessment ✅ **UPDATED**

### Existing Documentation Styles
1. **Modern FORD-style**: `src/util/getdirqq.f90` (complete with `!>`, `@brief`, `@param`) ✅
2. **Legacy header blocks**: `src/parameters/AL_C.F90` (extensive version history, inconsistent format)
3. **Minimal documentation**: Many modules lack comprehensive documentation
4. **Mixed patterns**: Combination of old-style comments and partial modernization

### Documentation Coverage Analysis ✅ **UPDATED - Current Status**
- **Total Fortran files**: 114 (verified by FORD scan)
- **FORD-compliant files**: 14 (~12% coverage baseline established)
- **Well-documented legacy**: ~20-30 files
- **Minimal documentation**: ~100+ files
- **Exemplary documentation**: `src/Shetran.f90` (main program) ✅
- **Infrastructure**: Fully operational FORD system ✅

---

## Implementation Strategy

### Phase 1: Infrastructure Setup (Week 1-2) ✅ **COMPLETED**

#### 1.1 FORD Installation and Configuration ✅ **COMPLETED**
**Deliverables:**

- [x] `ford_project.md` - Main FORD configuration file ✅
- [x] `docs/ford/` - Documentation output directory ✅
- [x] CMake integration for automated doc generation ✅
- [x] `.gitignore` updates for FORD output ✅

**Technical Details:**
```bash
# FORD installation (system-wide or virtual environment)
pip install ford

# Project structure
ford_project.md          # Main FORD configuration
docs/
├── ford/                # FORD output directory (ignored by git)
│   ├── index.html      # Generated documentation homepage
│   ├── modules/        # Module documentation
│   ├── procedures/     # Procedure documentation
│   └── search/         # Search functionality
└── ford_config/        # FORD configuration templates
    ├── templates/      # Custom HTML templates (optional)
    └── stylesheets/    # Custom CSS (optional)
```

#### 1.2 Build System Integration ✅ **COMPLETED**
**CMakeLists.txt modifications:** ✅
```cmake
# Add FORD documentation target
find_program(FORD_EXECUTABLE ford)
if(FORD_EXECUTABLE)
    add_custom_target(docs
        COMMAND ${FORD_EXECUTABLE} ${CMAKE_SOURCE_DIR}/ford_project.md
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Generating FORD documentation"
        VERBATIM
    )
    message(STATUS "FORD found: ${FORD_EXECUTABLE}")
    message(STATUS "Documentation target 'docs' available")
else()
    message(STATUS "FORD not found - documentation target unavailable")
endif()
```

**Build script integration:** ✅
```bash
# In build.sh (Linux/Unix)
if command -v ford >/dev/null 2>&1; then
    log_info "FORD available - can generate documentation with 'make docs'"
else
    log_warning "FORD not found - install with 'pip install ford' for documentation generation"
fi
```

**Windows build.bat integration:** ✅
```batch
REM In build.bat (Windows)
where ford >nul 2>&1
if %errorlevel% == 0 (
    echo FORD available - can generate documentation with 'nmake docs'
) else (
    echo WARNING: FORD not found - install with 'pip install ford' for documentation generation
)
```

#### 1.3 Version Control Configuration ✅ **COMPLETED**
**.gitignore additions:** ✅
```gitignore
# FORD documentation output
docs/ford/
*.ford_output
ford_project.html

# Python virtual environment (if used for FORD)
venv/
.venv/
__pycache__/
*.pyc
```

#### 1.4 VS Code Integration ✅ **COMPLETED**
**.vscode/tasks.json addition:** ✅
```json
{
    "label": "Generate FORD Documentation",
    "type": "shell",
    "command": "ford",
    "args": ["ford_project.md"],
    "group": "build",
    "presentation": {
        "echo": true,
        "reveal": "always",
        "focus": false,
        "panel": "shared"
    },
    "problemMatcher": []
}
```

**Dependencies check addition to build.sh:** ✅
```bash
check_ford() {
    if command -v ford >/dev/null 2>&1; then
        FORD_VERSION=$(ford --version 2>/dev/null || echo "unknown")
        log_info "Found FORD version: $FORD_VERSION"
    else
        log_warning "FORD not found. Install with: pip install ford"
        log_warning "Documentation generation will be unavailable"
    fi
}
```

**Dependencies check addition to build.bat:** ✅
```batch
:check_ford
where ford >nul 2>&1
if %errorlevel% == 0 (
    for /f "tokens=*" %%i in ('ford --version 2^>nul') do set FORD_VERSION=%%i
    if not defined FORD_VERSION set FORD_VERSION=unknown
    echo Found FORD version: %FORD_VERSION%
) else (
    echo WARNING: FORD not found. Install with: pip install ford
    echo WARNING: Documentation generation will be unavailable
)
goto :eof
```

### Phase 2: Documentation Standards and Templates (Week 2-3) ✅ **COMPLETED**

#### 2.1 Documentation Style Guide ✅ **COMPLETED**
**Module-level documentation template:**
```fortran
!> @file module_name.f90
!> @brief One-line description of module purpose
!> @author Author Name, Institution
!> @date Creation date (YYYY-MM-DD)
!> @version Version number
!>
!> @details
!! Extended description of module functionality.
!! Include algorithm details, usage patterns, and relationships
!! to other modules.
!>
!> @note Important implementation notes
!> @warning Usage warnings or limitations
!> @todo Future enhancements or known issues
!>
!> @see Related modules or external references
!>
!> **Historical Information:**
!! @note Legacy version history preserved from original headers
!! @note Refactoring history and rationale
```

**Procedure documentation template:**
```fortran
!> @brief Brief description of procedure purpose and functionality
!>
!> @details
!! Detailed description of algorithm, implementation approach,
!! and any special considerations.
!>
!> @param[in] input_param Description of input parameter
!> @param[out] output_param Description of output parameter  
!> @param[inout] inout_param Description of input/output parameter
!> @return Description of return value (functions only)
!>
!> @note Implementation notes
!> @warning Usage warnings
!> @see Related procedures or references
!>
!> @author Author name
!> @date Implementation date
```

**Variable documentation:**
```fortran
integer :: element_count        !< Number of computational elements in the model
real(dp) :: time_step          !< Current simulation time step [seconds]
logical :: is_initialized      !< Flag indicating module initialization status
```

#### 2.2 FORD Configuration File ✅ **COMPLETED**
**ford_project.md structure:** ✅
```markdown
---
title: SHETRAN Hydrological Model
project: SHETRAN
version: 4.4.7
license: Custom License
author: Newcastle University Water Group
author_description: Hydrological modeling research group
email: stephen.birkinshaw@ncl.ac.uk
source: src
output_dir: docs/ford
project_github: https://github.com/nclwater/Shetran
project_website: https://research.ncl.ac.uk/shetran/
summary: |
    SHETRAN is a physically-based, distributed hydrological model for 
    simulating water flow, sediment transport, and contaminant migration 
    in catchments.

media_dir: docs/media
favicon: docs/media/favicon.ico
theme: ford

graph: true
search: true
macro: TEST
       LOGIC=.true.
preprocessor: -DFORD_DOC

src_dir: src
exclude_dir: src/legacy
            src/backup

extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
           iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html

copy_subdir: examples
            docs/reports

md_extensions: markdown.extensions.toc
              markdown.extensions.smarty

page_dir: docs/pages
---

# SHETRAN Documentation

SHETRAN is a comprehensive hydrological modeling system...
```

### Phase 3: Systematic Documentation Implementation (Week 4-8) 🚧 **IN PROGRESS**

#### 3.1 Priority-based Implementation Schedule

**Week 4: Core System (Priority 1)** 🚧 **IN PROGRESS**
- [x] `src/Shetran.f90` (main program) ✅ **COMPLETED - Exemplary FORD documentation**
- [ ] `src/parameters/` (parameter modules)
- [ ] `src/simulation/` (simulation control)
- [ ] Interface modules from recent refactoring

**Week 5: Computation Modules (Priority 2)**  
- [ ] `src/compute/FRmod.f90` (framework)
- [ ] `src/compute/OCmod.f90` (overland channel)
- [ ] `src/compute/SYmod.f90` (sediment yield)
- [ ] `src/compute/ETmod.f90` (evapotranspiration)
- [ ] `src/compute/SMmod.f90` (snow model)

**Week 6: I/O and Utilities (Priority 3)**
- [ ] `src/io/` (input/output modules)
- [ ] `src/util/` (utility functions)
- [x] Already documented: `src/util/getdirqq.f90` (example standard) ✅ **ALREADY COMPLIANT**

**Week 7: Visualization and Advanced Features (Priority 4)**
- [ ] `src/visualisation/` (visualization interfaces)
- [ ] `src/compute/ZQmod.f90` (reservoir module)
- [ ] Specialized computation modules

**Week 8: Legacy Integration and Cleanup (Priority 5)**
- [ ] Convert remaining legacy headers
- [ ] Cross-reference validation
- [ ] Documentation completeness audit

#### 3.2 Documentation Conversion Workflow

**For each module:**
1. **Backup original**: Create `.original` backup if significant changes needed
2. **Analyze existing documentation**: Extract useful information from legacy headers
3. **Apply FORD templates**: Use standardized templates for consistency
4. **Preserve historical info**: Convert version history to `@note` sections  
5. **Add modern documentation**: Fill gaps with proper FORD markup
6. **Cross-reference**: Add `@see` links to related modules
7. **Validate**: Check FORD parsing and output quality

**Quality control checklist per module:**
- [ ] All public procedures documented with `@brief` and `@param`
- [ ] Module-level documentation complete
- [ ] Important variables have inline comments
- [ ] Legacy information preserved in `@note` sections
- [ ] Cross-references to related modules added
- [ ] FORD parsing validation passed

### Phase 4: Integration and Automation (Week 9-10) ✅ **COMPLETED**

#### 4.1 Build System Final Integration ✅ **COMPLETED**

**CMake enhancements:**
```cmake
# Enhanced documentation target with dependency checking
if(FORD_EXECUTABLE)
    # Generate source file list for FORD dependencies
    file(GLOB_RECURSE FORTRAN_SOURCES "src/*.f90" "src/*.F90")
    
    add_custom_target(docs
        COMMAND ${FORD_EXECUTABLE} ${CMAKE_SOURCE_DIR}/ford_project.md
        DEPENDS ${FORTRAN_SOURCES} ${CMAKE_SOURCE_DIR}/ford_project.md
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Generating FORD documentation"
        VERBATIM
    )
    
    # Optional: Install documentation
    install(DIRECTORY ${CMAKE_SOURCE_DIR}/docs/ford/
            DESTINATION share/doc/shetran
            OPTIONAL)
endif()
```

**CI/CD integration preparation:**
```yaml
# GitHub Actions workflow for documentation
name: Documentation
on: [push, pull_request]
jobs:
  docs:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.x'
      - name: Install FORD
        run: pip install ford
      - name: Generate documentation
        run: ford ford_project.md
      - name: Deploy to GitHub Pages
        if: github.ref == 'refs/heads/main'
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./docs/ford
```

#### 4.2 Documentation Maintenance Tools ✅ **COMPLETED**

**Documentation coverage script:** ✅ **IMPLEMENTED**
```bash
#!/bin/bash
# tools/check_doc_coverage.sh
echo "SHETRAN Documentation Coverage Report"
echo "===================================="

undocumented=0
total=0

for file in $(find src -name "*.f90" -o -name "*.F90"); do
    total=$((total + 1))
    if ! grep -q "!>" "$file"; then
        echo "UNDOCUMENTED: $file"
        undocumented=$((undocumented + 1))
    fi
done

documented=$((total - undocumented))
coverage=$((documented * 100 / total))

echo ""
echo "Summary:"
echo "Total files: $total"
echo "Documented: $documented"
echo "Undocumented: $undocumented"
echo "Coverage: $coverage%"
```

**Windows documentation coverage script:** ✅ **IMPLEMENTED**
```batch
@echo off
REM tools/check_doc_coverage.bat
echo SHETRAN Documentation Coverage Report
echo ====================================

set undocumented=0
set total=0

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
echo Total files: %total%
echo Documented: %documented%
echo Undocumented: %undocumented%
echo Coverage: %coverage%%%
```

**Template generator script:** ✅ **IMPLEMENTED**
```bash
#!/bin/bash
# tools/generate_doc_template.sh
# Usage: ./generate_doc_template.sh ModuleName > template.f90

module_name="$1"
if [[ -z "$module_name" ]]; then
    echo "Usage: $0 ModuleName"
    exit 1
fi

cat << EOF
!> @file ${module_name}.f90
!> @brief Brief description of ${module_name} functionality
!> @author Author Name, Institution
!> @date $(date +%Y-%m-%d)
!> @version 1.0
!>
!> @details
!! Detailed description of the ${module_name} module.
!! Include purpose, algorithms, and usage patterns.
!>
!> @note Implementation notes
!> @see Related modules

MODULE ${module_name}
   IMPLICIT NONE
   PRIVATE

   ! Public interfaces
   PUBLIC :: procedure_name

CONTAINS

   !> @brief Brief description of procedure
   !> @param[in] param_name Parameter description
   !> @return Return value description (if function)
   SUBROUTINE procedure_name(param_name)
      ! Implementation
   END SUBROUTINE procedure_name

END MODULE ${module_name}
EOF
```

**Windows template generator script:** ✅ **IMPLEMENTED**
```batch
@echo off
REM tools/generate_doc_template.bat
REM Usage: generate_doc_template.bat ModuleName > template.f90

if "%1"=="" (
    echo Usage: %0 ModuleName
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
echo !^>
echo !^> @note Implementation notes
echo !^> @see Related modules
echo.
echo MODULE %module_name%
echo    IMPLICIT NONE
echo    PRIVATE
echo.
echo    ! Public interfaces
echo    PUBLIC :: procedure_name
echo.
echo CONTAINS
echo.
echo    !^> @brief Brief description of procedure
echo    !^> @param[in] param_name Parameter description
echo    !^> @return Return value description (if function^)
echo    SUBROUTINE procedure_name(param_name^)
echo       ! Implementation
echo    END SUBROUTINE procedure_name
echo.
echo END MODULE %module_name%
```

### Phase 5: Validation and Deployment (Week 11-12)

#### 5.1 Quality Assurance

**Documentation validation checklist:**
- [ ] All 216+ Fortran files have FORD-compliant documentation
- [ ] No FORD parsing errors or warnings
- [ ] Cross-references work correctly
- [ ] Search functionality operational
- [ ] Historical information preserved
- [ ] Build system integration functional
- [ ] Documentation coverage >95%

**Testing scenarios:**
```bash
# Test documentation generation (Linux/Unix)
ford ford_project.md

# Test build system integration (Linux/Unix)
mkdir -p build/test && cd build/test
cmake ../..
make docs

# Test coverage (Linux/Unix)
./tools/check_doc_coverage.sh

# Windows testing scenarios
# Test documentation generation (Windows)
ford ford_project.md

# Test build system integration (Windows)
mkdir build\test
cd build\test
cmake ..\..
nmake docs

# Test coverage (Windows)
.\tools\check_doc_coverage.bat

# Validate HTML output (both platforms)
# Check for broken links, missing content
```

#### 5.2 Documentation Deployment

**Local development:**
```bash
# Generate and serve documentation locally (Linux/Unix)
ford ford_project.md
cd docs/ford && python -m http.server 8000
# Access at http://localhost:8000
```

**Windows local development:**
```batch
REM Generate and serve documentation locally (Windows)
ford ford_project.md
cd docs\ford
python -m http.server 8000
REM Access at http://localhost:8000
```

**Integration with existing workflow:**
- Add documentation generation to release process
- Include in `build.sh` and `build.bat` optional targets
- VS Code task for quick documentation updates
- CMake target for automated builds on both platforms

---

## Resource Requirements

### Dependencies
**System requirements:**
- Python 3.6+ (for FORD)
- pip (for FORD installation)
- Modern web browser (for viewing documentation)

**FORD installation:**
```bash
# System-wide installation
pip install ford

# Or user installation
pip install --user ford

# Or virtual environment (recommended for development)
python -m venv venv
source venv/bin/activate  # Linux/Mac
pip install ford
```

### Time Estimates
- **Phase 1** (Infrastructure): 16-24 hours
- **Phase 2** (Standards): 12-16 hours  
- **Phase 3** (Implementation): 40-60 hours
- **Phase 4** (Integration): 12-16 hours
- **Phase 5** (Validation): 8-12 hours
- **Total**: 88-128 hours (11-16 working days)

### Skills Required
- Fortran knowledge (for understanding existing code)
- FORD markup language (learnable in 2-4 hours)
- Basic HTML/CSS (for customization, optional)
- CMake (for build integration)
- Git (for version control workflow)

---

## Risk Assessment and Mitigation

### Technical Risks
**Risk**: FORD parsing errors with legacy Fortran constructs  
**Mitigation**: Test FORD compatibility early, use preprocessing if needed

**Risk**: Large documentation generation time  
**Mitigation**: Implement incremental builds, optimize FORD configuration

**Risk**: Documentation drift from code changes  
**Mitigation**: Integrate into build process, add CI/CD checks

### Process Risks
**Risk**: Inconsistent documentation quality across contributors  
**Mitigation**: Clear style guide, templates, review process

**Risk**: Loss of historical information during conversion  
**Mitigation**: Systematic backup strategy, preserve in `@note` sections

---

## Success Metrics

### Quantitative Goals
- [ ] **Coverage**: >95% of Fortran files have comprehensive FORD documentation
- [ ] **Build Integration**: Documentation generation included in standard build
- [ ] **Performance**: Documentation generation completes in <5 minutes
- [ ] **Accessibility**: Searchable, cross-referenced HTML documentation

### Qualitative Goals
- [ ] **Consistency**: Uniform documentation style across all modules
- [ ] **Maintainability**: Easy to update documentation with code changes
- [ ] **Usability**: New developers can understand codebase from documentation
- [ ] **Preservation**: Historical information maintained and accessible

---

## Future Enhancements

### Phase 6 (Future): Advanced Features
- Custom HTML themes matching SHETRAN branding
- Integration with GitHub Pages for public documentation
- API change tracking and versioning
- Interactive code examples and tutorials
- Automated link checking and validation

### Continuous Improvement
- Regular documentation quality audits
- Contributor training materials
- Documentation review as part of code review process
- Metrics tracking for documentation usage and effectiveness

---

## Conclusion

This comprehensive plan provides a systematic approach to standardizing SHETRAN's documentation using FORD. The phased implementation ensures minimal disruption to ongoing development while establishing a robust foundation for long-term documentation maintainability.

The integration with existing build systems, version control workflows, and development tools ensures that the documentation system becomes a natural part of the development process rather than an additional burden.

Upon completion, SHETRAN will have professional-grade, searchable, and maintainable API documentation that significantly improves code accessibility for both current and future developers.
