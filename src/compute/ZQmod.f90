!> summary: Public interface module for ZQ table (reservoir) functionality.
!> author: Refactored by GitHub Copilot, Original: Daryl Hughes (Newcastle University),
!>         Stephen Birkinshaw (Newcastle University), Sven Berendsen (Newcastle University)
!> date: 2025-09-05
!>
!> This module provides a clean interface to the ZQ table system without
!> containing any implementation details or variable declarations.
!>
!> This is the main interface module for the ZQ table system that models
!> elevation-discharge relationships for hydraulic structures like reservoirs
!> and weirs. The actual implementation has been refactored into separate
!> modules within src/compute/hydraulic_structures/:
!>
!> - zq_data_types.f90: Common data structures and variables
!> - zq_table_reader.f90: File reading and parsing functionality
!> - zq_interpolator.f90: Value interpolation and lookup functionality
!>
!> For models which contain reservoirs, it outputs downstream discharge as a
!> function of upstream water elevation. The user must create an elevation-discharge
!> (ZQ) set up file and reference this in the RunDatafile (module 51).
!> This file may contain multiple ZQ tables i.e. for multiple channel links
!> These can be created in Excel etc., and saved as .txt. The file should be
!> space-delimited, so may require replacement of tabs with spaces
!> The ZQtables require a Z column (first), followed by at least one discharge column.
!> These should have names along the format 'ZQ>##.##' i.e. discharge at elevations
!> above this threshold
!> The number of rows and the interval between Zs is arbitrary (for example, 0.01m
!> intervals would be suitable)
!>
!> @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | ? | DH | Initial version |
!> | ? | SB | Reworked for inclusion in SHETRAN4.4.6.Res2 |
!> | 2025-09-05 | AI | Refactored into interface module with implementation in separate files |
!>
module ZQmod

   ! Import the public interfaces from the implementation modules
   USE zq_table_reader,    ONLY: ReadZQTable                                  ! ZQ file reading functionality
   USE zq_interpolator,    ONLY: get_ZQTable_value                            ! ZQ value interpolation functionality

   IMPLICIT NONE

   ! set everything to private by default
   PRIVATE

   ! Re-export the public interfaces - this is the only public content of this module
   PUBLIC                                          :: ReadZQTable              ! subroutine for reading ZQ tables
   PUBLIC                                          :: get_ZQTable_value        ! function for interpolating discharge values

   ! Note: No variables are declared in this interface module.
   ! All data structures are contained in zq_data_types.f90
   ! All implementation is contained in the respective specialized modules.

END MODULE ZQmod
