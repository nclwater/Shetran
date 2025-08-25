MODULE snow_variables
! Snow model variables and data structures extracted from SMmod.f90
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the SM.F files
! Refactor 2025: Extracted variables into separate module

   USE SGLOBAL
   IMPLICIT NONE

   ! Dynamic arrays for snowmelt calculations
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: smelt, tmelt

   ! Snow model variables
   DOUBLEPRECISION :: USM, DDF, RHOS, ESM, HFC, HFR, HFE, HFT, ZUS, ZDS, ZOS
   DOUBLEPRECISION :: RHODEF, TOPNET, PNSNOW
   LOGICAL         :: BINSMP
   INTEGER         :: IMET(NVEE), NSD
   DOUBLEPRECISION :: HEAD(20)

   ! Public interface for all variables
   PUBLIC :: smelt, tmelt
   PUBLIC :: USM, DDF, RHOS, ESM, HFC, HFR, HFE, HFT, ZUS, ZDS, ZOS
   PUBLIC :: RHODEF, TOPNET, PNSNOW, BINSMP, IMET, NSD, HEAD

END MODULE snow_variables
