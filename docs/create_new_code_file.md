# Creating New SHETRAN Code Files

This guide explains how to create new Fortran modules and files in SHETRAN with proper FORD documentation from the start.

## Quick Start

### Using the Template Generator

SHETRAN provides template generators for both Linux/Unix and Windows that create FORD-compliant Fortran modules:

**Linux/Unix:**
```bash
./tools/generate_doc_template.sh ModuleName > src/compute/ModuleName.f90
```

**Windows:**
```batch
.\tools\generate_doc_template.bat ModuleName > src\compute\ModuleName.f90
```

### Example Usage

Create a new water quality module:

```bash
# Generate the template
./tools/generate_doc_template.sh WaterQuality > src/compute/WaterQuality.f90

# Edit the generated file to implement your functionality
# The template includes all required FORD documentation
```

## Template Generator Features

### Generated Structure

The template generator creates a complete Fortran module with:

- **FORD-compliant documentation headers**
- **Module metadata** (file info, author, date, version)
- **Proper visibility control** (PRIVATE by default)
- **Standard module structure** with initialization/finalization
- **Documented procedures** with parameter descriptions
- **Inline variable documentation**
- **Example implementation patterns**

### What Gets Generated

```fortran
!> @file ModuleName.f90
!> @brief Brief description of ModuleName functionality
!> @author Author Name, Institution
!> @date 2025-09-05
!> @version 1.0
!>
!> @details
!! Detailed description of the ModuleName module.
!! Include purpose, algorithms, and usage patterns.
!!
!! This module provides:
!! - Feature 1: Description
!! - Feature 2: Description
!! - Feature 3: Description
!>
!> @note Implementation notes and important considerations
!> @warning Usage warnings or limitations
!> @todo Future enhancements or known issues
!> @see Related modules or external references

MODULE ModuleName
   IMPLICIT NONE
   PRIVATE

   ! Module parameters
   integer, parameter :: MODULE_VERSION = 1

   ! Module variables
   logical :: is_initialized = .false.  !< Module initialization status

   ! Public interfaces
   PUBLIC :: initialize_modulename
   PUBLIC :: finalize_modulename
   PUBLIC :: procedure_name

CONTAINS
   ! ... fully documented procedures ...
END MODULE ModuleName
```

## Customization After Generation

### 1. Update Module Metadata

Edit the generated file header to match your module:

```fortran
!> @file WaterQuality.f90
!> @brief Water quality modeling and chemical transport calculations
!> @author Jane Smith, Newcastle University
!> @date 2025-09-05
!> @version 1.0
!>
!> @details
!! This module implements water quality modeling including:
!! - Chemical species transport
!! - Reaction kinetics
!! - Source/sink calculations
!! - Concentration tracking
```

### 2. Add Your Module's Public Interface

Replace the template procedures with your actual functionality:

```fortran
   ! Public interfaces - replace these with your actual procedures
   PUBLIC :: initialize_water_quality
   PUBLIC :: finalize_water_quality
   PUBLIC :: calculate_chemical_transport
   PUBLIC :: update_concentrations
   PUBLIC :: apply_reactions
```

### 3. Implement Your Procedures

Replace the template `procedure_name` with your actual procedures:

```fortran
   !> @brief Calculate chemical transport for a time step
   !> @details
   !! Computes advective and diffusive transport of chemical species
   !! using finite difference methods on the computational grid.
   !>
   !> @param[in] time_step Simulation time step [seconds]
   !> @param[in] velocity_field Water velocity vectors [m/s]
   !> @param[inout] concentrations Chemical concentrations [mg/L]
   !> @param[out] transport_flux Mass transport fluxes [kg/s]
   !>
   !> @note Requires module to be initialized before calling
   !> @see update_concentrations, apply_reactions
   SUBROUTINE calculate_chemical_transport(time_step, velocity_field, concentrations, transport_flux)
      REAL, INTENT(IN)    :: time_step
      REAL, INTENT(IN)    :: velocity_field(:,:)
      REAL, INTENT(INOUT) :: concentrations(:,:)
      REAL, INTENT(OUT)   :: transport_flux(:,:)
      
      ! Your implementation here
      
   END SUBROUTINE calculate_chemical_transport
```

## File Organization Guidelines

### Directory Structure

Place new modules in appropriate directories:

```
src/
├── compute/              # Computational modules
│   ├── water_quality/    # Water quality related modules
│   ├── sediment/        # Sediment transport modules
│   ├── hydraulic_flow/  # Flow calculation modules
│   └── ...
├── io/                  # Input/output modules
├── parameters/          # Parameter and data modules
├── util/               # Utility functions
└── visualisation/      # Visualization modules
```

### Naming Conventions

- **Module files**: Use descriptive names, e.g., `water_quality_chemistry.f90`
- **Module names**: Match the filename, e.g., `MODULE water_quality_chemistry`
- **Procedure names**: Use clear, descriptive names with consistent prefixes

### Example Placements

```bash
# Computational modules
./tools/generate_doc_template.sh WaterQualityChemistry > src/compute/water_quality/water_quality_chemistry.f90

# I/O modules  
./tools/generate_doc_template.sh ChemicalDataReader > src/io/chemical_data_reader.f90

# Utility modules
./tools/generate_doc_template.sh ChemicalUtils > src/util/chemical_utils.f90
```

## Integration with Build System

### Automatic Discovery

SHETRAN's CMake build system automatically discovers new Fortran files:

1. **Add your file** to the appropriate `src/` subdirectory
2. **Build system** automatically detects and includes it
3. **Module dependencies** are analyzed automatically
4. **Documentation** is included in FORD generation

### Testing Your Module

After creating a new module:

```bash
# Test that it compiles
./build.sh -t Debug

# Check documentation coverage
./tools/check_doc_coverage.sh

# Generate updated documentation
make docs   # or cmake --build . --target docs
```

## Documentation Standards

### Required Documentation Elements

Every new module must include:

- **File header** with `@file`, `@brief`, `@author`, `@date`
- **Module description** with `@details` section
- **Procedure documentation** with `@brief`, `@param`, `@return`
- **Variable documentation** with inline comments using `!<`
- **Cross-references** using `@see` tags where appropriate

### Quality Checklist

Before submitting new code, verify:

- [ ] All public procedures have `@brief` and `@param` documentation
- [ ] Module has comprehensive `@details` section
- [ ] Important variables have inline documentation
- [ ] Cross-references to related modules added
- [ ] FORD parsing passes without errors
- [ ] Documentation coverage target maintained

### Documentation Tips

**Good documentation:**
```fortran
!> @brief Calculate sediment erosion rate using USLE equation
!> @details
!! Implements the Universal Soil Loss Equation (USLE) to calculate
!! soil erosion rates based on rainfall intensity, soil properties,
!! slope characteristics, and land use factors.
!>
!> @param[in] rainfall_intensity Rainfall intensity [mm/hr]
!> @param[in] soil_erodibility Soil erodibility factor (K-factor)
!> @param[in] slope_factor Topographic slope factor
!> @return Erosion rate [tonnes/hectare/year]
```

**Avoid minimal documentation:**
```fortran
!> Calculate erosion
FUNCTION calc_erosion() RESULT(erosion)
```

## Advanced Usage

### Custom Template Modifications

You can modify the template generators in `tools/` to:

- **Add company/institution-specific headers**
- **Include additional metadata fields**
- **Customize default procedure patterns**
- **Add project-specific documentation sections**

### Batch File Creation

Create multiple related modules at once:

```bash
# Create a suite of water quality modules
for module in WaterQualityChemistry WaterQualityBiology WaterQualityPhysics; do
    ./tools/generate_doc_template.sh $module > src/compute/water_quality/${module,,}.f90
done
```

## Integration with Version Control

### Git Workflow

```bash
# Create feature branch for new module
git checkout -b feature/water-quality-module

# Generate and implement module
./tools/generate_doc_template.sh WaterQuality > src/compute/WaterQuality.f90
# ... edit and implement ...

# Test and document
./tools/check_doc_coverage.sh
make docs

# Commit with descriptive message
git add src/compute/WaterQuality.f90
git commit -m "Add WaterQuality module with chemical transport calculations

- Implements advective and diffusive transport
- Includes reaction kinetics framework
- Full FORD documentation included
- Integration tests passing"
```

### Code Review Checklist

When reviewing new modules:

- [ ] Template generator was used as starting point
- [ ] All template placeholders replaced with actual content
- [ ] Documentation is comprehensive and accurate
- [ ] Module follows SHETRAN coding standards
- [ ] Build system integration works correctly
- [ ] Documentation coverage maintained

## Troubleshooting

### Common Issues

**Template generator not executable:**
```bash
chmod +x tools/generate_doc_template.sh
```

**FORD parsing errors:**

- Check for proper `!>` comment syntax
- Verify all `@param` tags match actual parameters
- Ensure module and procedure names are consistent

**Build system not finding module:**

- Verify file is in `src/` subdirectory
- Check that module name matches filename
- Ensure proper Fortran syntax

### Getting Help

- Check existing modules for examples: `src/util/getdirqq.f90`
- Review FORD documentation: [FORD GitHub](https://github.com/Fortran-FOSS-Programmers/FORD)
- Consult SHETRAN development team for project-specific questions

---

This documentation system ensures that all new SHETRAN code maintains high documentation standards and integrates seamlessly with the project's build and documentation infrastructure.
