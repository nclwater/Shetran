!> summary: Data types and common variables for ZQ table (reservoir) functionality.
!> author: Daryl Hughes (Newcastle University), Stephen Birkinshaw (Newcastle University),
!>         Sven Berendsen (Newcastle University), Refactored by GitHub Copilot
!> date: 2025-09-05
!>
!> This module contains all the data structures used to store elevation-discharge
!> relationships for hydraulic structures like reservoirs and weirs.
!>
!> It contains allocatable arrays and variables that store:
!>
!> - ZQ table dimensions and structure.
!> - Header information including stage thresholds.
!> - Operation parameters for sluices and weirs.
!> - The actual ZQ lookup tables.
!>
!> @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | ? | DH, SB, SvenB | Original versions of ZQmod |
!> | 2025-09-05 | AI | Refactored from original ZQmod.f90 into this module |
!>
module zq_data_types

   USE mod_parameters                                                          ! general parameters

   IMPLICIT NONE

   ! ===========================================================================
   ! Variables

   ! set everything to private by default
   PRIVATE

   ! module variables - all public for use by other ZQ modules
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC    :: nZQcols                  !! Number of columns in each ZQ table
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC    :: nZQrows                  !! Number of rows in each ZQ table
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC    :: zcol                     !! Currently active column index for each ZQ table
   REAL(kind=R8P), DIMENSION(:,:), ALLOCATABLE, PUBLIC     :: headerRealArray          !! Real array to store weir stage thresholds [m]
   REAL(kind=R8P), DIMENSION(:,:,:), ALLOCATABLE, PUBLIC   :: ZQ                       !! 3D array storing elevation-discharge tables (rows, cols, tables)
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE, PUBLIC    :: ZQTableOpHour            !! The hour of the day at which sluices are operated for each table
   INTEGER(kind=I_P), PUBLIC                               :: ZQTableRef               !! The reference number of the currently processed ZQ table

END MODULE zq_data_types
