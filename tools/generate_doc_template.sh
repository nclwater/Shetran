#!/bin/bash
# tools/generate_doc_template.sh
# SHETRAN FORD Documentation Template Generator
# 
# Usage: ./generate_doc_template.sh ModuleName > template.f90

module_name="$1"
if [[ -z "$module_name" ]]; then
    echo "Usage: $0 ModuleName"
    echo ""
    echo "Generates a FORD-compliant Fortran module template with proper documentation."
    echo "Output should be redirected to a file:"
    echo "  $0 MyModule > src/compute/MyModule.f90"
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
!>
!> **Historical Information:**
!! @note Creation date: $(date +%Y-%m-%d)
!! @note Initial implementation by: Author Name

MODULE ${module_name}
   IMPLICIT NONE
   PRIVATE

   ! Module parameters
   integer, parameter :: MODULE_VERSION = 1

   ! Module variables
   logical :: is_initialized = .false.  !< Module initialization status

   ! Public interfaces
   PUBLIC :: initialize_${module_name,,}
   PUBLIC :: finalize_${module_name,,}
   PUBLIC :: procedure_name

CONTAINS

   !> @brief Initialize the ${module_name} module
   !> @details
   !! Performs module initialization including:
   !! - Parameter validation
   !! - Memory allocation
   !! - Initial state setup
   !>
   !> @param[out] success Initialization success flag
   !> @note Must be called before using other module procedures
   SUBROUTINE initialize_${module_name,,}(success)
      LOGICAL, INTENT(OUT) :: success
      
      ! Implementation
      is_initialized = .true.
      success = .true.
   END SUBROUTINE initialize_${module_name,,}

   !> @brief Finalize the ${module_name} module
   !> @details
   !! Performs cleanup including:
   !! - Memory deallocation
   !! - Resource release
   !! - State reset
   !>
   !> @note Should be called when module is no longer needed
   SUBROUTINE finalize_${module_name,,}()
      ! Implementation
      is_initialized = .false.
   END SUBROUTINE finalize_${module_name,,}

   !> @brief Brief description of main procedure
   !> @details
   !! Detailed description of the algorithm, implementation approach,
   !! and any special considerations.
   !>
   !> @param[in] input_param Description of input parameter
   !> @param[out] output_param Description of output parameter  
   !> @param[inout] inout_param Description of input/output parameter
   !> @return Description of return value (if function)
   !>
   !> @note Implementation notes
   !> @warning Usage warnings
   !> @see Related procedures or references
   !>
   !> @author Author name
   !> @date $(date +%Y-%m-%d)
   SUBROUTINE procedure_name(input_param, output_param, inout_param)
      ! Parameter declarations with proper documentation
      INTEGER, INTENT(IN)    :: input_param   !< Description of input parameter
      REAL,    INTENT(OUT)   :: output_param  !< Description of output parameter
      LOGICAL, INTENT(INOUT) :: inout_param   !< Description of input/output parameter
      
      ! Local variables
      integer :: local_var  !< Description of local variable
      
      ! Procedure implementation
      if (.not. is_initialized) then
         ! Handle uninitialized module
         return
      end if
      
      ! Main algorithm implementation here
      
   END SUBROUTINE procedure_name

END MODULE ${module_name}
EOF
