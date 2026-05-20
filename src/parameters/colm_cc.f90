!> summary: Column contaminant transport state.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the legacy `COLM.CC` common blocks. It stores
!> contaminant concentration, lateral convection, uptake, dispersion,
!> generation, and soil-property arrays used by the `COLM` column contaminant
!> calculations.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked, no changes. |
!> | 1991-06-16 | JE | 3.1 | Removed references to `CCPRV`. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE COLM_CC
!------------------------------- Start of COLM.CC ---------------------*
!                      INCLUDE FILE FOR CONTAMINANT VARIABLES USED IN
!                      SUBROUTINE COLM
!----------------------------------------------------------------------*
! Version:  /SHETRAN/INCLUDE/COLM.CC/4.1
! Modifications:
!                          JE     26/4/91   3.0     WRITTEN
!                          JE     13/6/91   3.1     CHECKED, NO CHANGES
!                          JE     16/6/91   3.1     REFERENCES TO CCPRV
!                                                   REMOVED
! RAH  970313  4.1  Explicit typing.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Imported constants
!                      LLEE
! Commons
   USE SGLOBAL, ONLY : LLEE
   IMPLICIT NONE
   DOUBLEPRECISION GCAPLA

!COMMON / GENCAP / GCAPLA
   DOUBLEPRECISION CSWA (4), CSWAT (4), RRRSWA (4), RRRSAT (4)
   DOUBLEPRECISION RRRLS, RRRLSC, RRRLST, RRRSW, RRRSWC, RRRSWT

!COMMON / CLSURC / CSWA, CSWAT, RRRSWA, RRRSAT, RRRLS, RRRLSC, &
   !RRRLST, RRRSW, RRRSWC, RRRSWT
!                            DATA FOR LATERAL CONVECTION AT SURFACE
   DOUBLEPRECISION CCAPA (LLEE, 4), CCAPAT (LLEE, 4)

!COMMON / CLCOV / CCAPA, CCAPAT
!                            DATA FOR CONVECTION CONC. IN THE SAT. ZONE
   DOUBLEPRECISION CCAP (LLEE), COLCAP (LLEE), SCAP (LLEE), SOLCAP ( &
      LLEE)

!COMMON / CLCONC / CCAP, COLCAP, SCAP, SOLCAP
!                            CONCENTRATIONS IN COLUMN
   DOUBLEPRECISION EDCAP (LLEE), EDCAPC (LLEE), EDCAPT (LLEE)
   DOUBLEPRECISION ESCAP (LLEE), ESCAPS (LLEE), ESCAPT (LLEE)
   DOUBLEPRECISION ESSCAP, ESSCPC, ESSCPT

!COMMON / CLPLT / EDCAP, EDCAPC, EDCAPT, ESCAP, ESCAPS, ESCAPT, &
   !ESSCAP, ESSCPC, ESSCPT
!                            RATES OF PLANT UPTAKE FROM THE COLUMN
   DOUBLEPRECISION DDOD (LLEE), DDOD1 (LLEE)

!COMMON / CLDSP / DDOD, DDOD1
!                            DISEPERSION COEFFICIENTS FOR COLUMN
   DOUBLEPRECISION GNERD (LLEE), GNDSE (LLEE), GND2 (LLEE), GNDSE2 ( &
      LLEE)

!COMMON / GENER / GNERD, GNDSE, GND2, GNDSE2
!                            GENERATION DATA TO BE CARRIED
!                            TO DAUGHTER CONTAMINANT
   DOUBLEPRECISION AALPSO (LLEE), FFSO (LLEE), GGNNSO (LLEE), &
      KKDSO (LLEE)

!COMMON / SOILD / AALPSO, FFSO, GGNNSO, KKDSO
!                            SOIL PROPERTY DATA
   DOUBLEPRECISION CCPRF, CCPRFT
!PRIVATE :: LLEE
END MODULE COLM_CC
