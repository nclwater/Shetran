!MMMMMM MODULE ocqdqmod
MODULE ocqdqmod
! Public interface module for hydraulic flow calculations
! JE  1/09   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactored: 2025-08-22 - Split into multiple files for better organization
!
! This module serves as the public interface to the hydraulic flow calculation
! system, which was previously contained entirely within OCQDQMOD.F90.
! The implementation has been split into logical components:
!   - hydraulic_variables.f90: Common variables and arrays
!   - hydraulic_helpers.f90: Helper functions (fstr, fdqq)
!   - flow_calculator.f90: Main OCQDQ subroutine implementation

   ! Import all components from the reorganized modules
   USE hydraulic_variables, ONLY : XAFULL, COCBCD, HOCNOW, QOCF, STRXX, STRYY
   USE hydraulic_helpers,   ONLY : fstr, fdqq
   USE flow_calculator,     ONLY : OCQDQ

   IMPLICIT NONE

   ! Re-export all public interfaces to maintain compatibility
   PUBLIC :: OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD

END MODULE ocqdqmod
