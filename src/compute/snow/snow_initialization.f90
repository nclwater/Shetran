MODULE snow_initialization
! Snow model initialization routines extracted from SMmod.f90
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactor 2025: Extracted initialization into separate module

   USE SGLOBAL
   USE snow_variables, ONLY : smelt, tmelt
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: initialise_smmod

CONTAINS

!SSSSSS SUBROUTINE initialise_smmod
   SUBROUTINE initialise_smmod
      LOGICAL         :: first=.TRUE.
      if (FIRST) then
         ALLOCATE (TMELT(max_no_snowmelt_slugs,total_no_elements))
         ALLOCATE (SMELT(max_no_snowmelt_slugs,total_no_elements))
         FIRST = .FALSE.
      endif
   END SUBROUTINE initialise_smmod

END MODULE snow_initialization
