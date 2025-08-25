MODULE SMmod
! Public interface for snow model - refactored from original SMmod.f90
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the SM.F files
! Refactor 2025: Reorganized into modular structure with separate concerns

   ! Import variables and procedures from specialized modules
   USE snow_constants
   USE snow_variables
   USE snow_initialization, ONLY : initialise_smmod
   USE snow_interface, ONLY : SMIN

   IMPLICIT NONE

   ! Keep same public interface as original module
   PRIVATE
   PUBLIC :: SMIN, rhos, head, binsmp, ddf, zos, zds, zus, nsd, rhodef, imet, smelt, tmelt, initialise_smmod

   ! Re-export physical constants for backward compatibility
   PUBLIC :: RHOA, RHOW, CPA, CPW, CPI, LWI, LVW, HFG

END MODULE SMmod
