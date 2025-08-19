MODULE oc_parameters
! Module containing parameters and constants for overland channel flow calculations
! Extracted from OCmod2.f90 as part of refactoring
! Original: JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90

   IMPLICIT NONE

! Mathematical and physical constants for flow calculations
   DOUBLEPRECISION, PARAMETER   :: F23=2.0D0/3.0D0,      & ! 2/3 power for Manning's equation
      F53=5.0D0/3.0D0,      & ! 5/3 power for Manning's equation
      DZMIN = 1.0D-3,       & ! Minimum depth difference
      RDZMIN=3.16227766d-2, & ! sqrt(DZMIN)
      H23MIN=1.0d-2,        & ! DZMIN^(2/3)
      ROOT2G = 4.42944d0      ! sqrt(2*9.81) - gravity constant

END MODULE oc_parameters
