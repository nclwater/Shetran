module zq_data_types

!-------------------------------------------------------------------------------
!
!> @file zq_data_types.f90
!
!> @author Refactored from ZQmod.f90 by GitHub Copilot
!> @author Original: Daryl Hughes, Newcastle University
!> @author Original: Stephen Birkinshaw, Newcastle University
!> @author Original: Sven Berendsen, Newcastle University
!
!> @brief
!! Data types and common variables for ZQ table (reservoir) functionality.
!! This module contains all the data structures used to store elevation-discharge
!! relationships for hydraulic structures like reservoirs and weirs.
!
!> @details
!! Contains allocatable arrays and variables that store:
!! - ZQ table dimensions and structure
!! - Header information including stage thresholds
!! - Operation parameters for sluices and weirs
!! - The actual ZQ lookup tables
!
! REVISION HISTORY:
! 2025-09-05 - Refactored from original ZQmod.f90 into separate data types module
!
!-------------------------------------------------------------------------------

   USE mod_parameters                                                          ! general parameters

   IMPLICIT NONE

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Variables

   ! set everything to private by default
   PRIVATE

   ! module variables - all public for use by other ZQ modules
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC    :: nZQcols                  !< use to dimension allocatable arrays
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC    :: nZQrows                  !< use to dimension allocatable arrays
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC    :: zcol                     !< use to dimension allocatable arrays
   REAL(kind=R8P), DIMENSION(:,:), ALLOCATABLE, PUBLIC     :: headerRealArray          !< real array to store weirEq stage thresholds
   REAL(kind=R8P), DIMENSION(:,:,:), ALLOCATABLE, PUBLIC   :: ZQ                       !< ZQ = 3D array (nZQrows, nZQcols, NoZQTables)
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC    :: ZQTableOpHour            !< the hour at which sluices are operated
   INTEGER(kind=I_P), PUBLIC                               :: ZQTableRef               !< the reference number of the ZQtable

END MODULE zq_data_types
