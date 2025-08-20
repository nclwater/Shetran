MODULE ETmod
!----------------------------------------------------------------------*
!
! Main interface module for evapotranspiration calculations
! This module provides the same interface as the original ETmod.f90
! but with refactored implementation in separate modules
!
! Refactored from original ETmod.f90 - maintains same public interface
!----------------------------------------------------------------------*
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the ET .F files
! Refactored 2025: Split into multiple modules for better organization

   ! Import all the variables and main functionality from the sub-modules
   USE et_variables
   USE et_main
   USE et_validation

   IMPLICIT NONE

   ! Make everything private by default
   PRIVATE

   ! Re-export the same public interface as the original module
   ! This ensures other files that USE ETmod continue to work unchanged
   PUBLIC :: ETSIM, BMETP, BINETP, BMETAL, MEASPE, CSTCAP, RC, BAR, RA, MODE, &
      NF, CK, CB, MODECS, MODEPL, MODECL, MODEVH, NCTCST, CSTCA1, RELCST, TIMCST, &
      NCTPLA, PLAI1, RELPLA, TIMPLA, NCTCLA, CLAI1, NCTVHT, VHT1, RELVHT, TIMVHT, &
      PS1, RCF, FET, RTOP, RELCLA, TIMCLA, del, &
      psi4, uzalfa  ! THESE NEEDED ONLY FOR AD

END MODULE ETmod
