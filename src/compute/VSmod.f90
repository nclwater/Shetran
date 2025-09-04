MODULE VSmod
!----------------------------------------------------------------------*
! Interface module for Variably Saturated Subsurface (VSS) flow
! This module maintains backward compatibility with the original VSmod
!----------------------------------------------------------------------*
! Refactored from monolithic VSmod.f90.sav
! Date: 2025-01-04
! Original size: 4,511 lines
! New architecture: 8 focused modules + interface
!----------------------------------------------------------------------*

   ! Import all the specialized modules
   USE subsurface_variables
   USE subsurface_initialization
   USE subsurface_simulation
   USE subsurface_column_solver
   USE subsurface_boundary_conditions
   USE subsurface_soil_properties
   USE subsurface_io
   USE subsurface_utilities

   IMPLICIT NONE

   ! Re-export all public interfaces for backward compatibility

   ! Main entry points
   PUBLIC :: VSIN, VSSIM, initialise_vsmod

   ! Column solving routines
   PUBLIC :: VSCOLM, VSCOEF, VSINTC

   ! Boundary condition routines
   PUBLIC :: VSBC, VSSAI, VSLOWR, VSUPPR, VSWELL, VSPREP

   ! Soil property routines
   PUBLIC :: VSFUNC, VSSOIL, VSSPR

   ! I/O routines
   PUBLIC :: VSREAD

   ! Utility routines
   PUBLIC :: VSMB, fncell

   ! Additional initialization routines
   PUBLIC :: VSCONC, VSCONL

END MODULE VSmod
