MODULE CMmod
!----------------------------------------------------------------------*
!
! Main interface module for contaminant transport calculations
! This module provides the same interface as the original CMmod.f90
! but with refactored implementation in separate modules
!
! Refactored from original CMmod.f90 - maintains same public interface
!----------------------------------------------------------------------*
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the CM COLM and LINK .F files
! SvenB  20200305 removed complete SGLOBAL include
! [REFACTORING] 19/08/2025 - Extracted from original CMmod.f90 as interface module
!                           Provides public interface to contaminant transport system
! [REFACTORING] 05/09/2025 - Converted to pure interface module following ETmod pattern
!                           All subroutine implementations moved to specialized modules

   ! Import all contaminant modules and functionality
   USE contaminant_common
   USE contaminant_data_reader
   USE contaminant_simulation
   USE contaminant_column_solver
   USE contaminant_link_solver
   USE contaminant_utilities
   USE contaminant_interface

   IMPLICIT NONE

   ! Make everything private by default
   PRIVATE

   ! Re-export the same public interface as the original module
   ! This ensures other files that USE CMmod continue to work unchanged
   PUBLIC :: CMSIM, CMFIN, CMRD

END MODULE CMmod
