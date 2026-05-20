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
USE SGLOBAL, ONLY : LLEE
IMPLICIT NONE
DOUBLEPRECISION GCAPLA  !! General contaminant capacity scaling value.

!COMMON / GENCAP / GCAPLA  
DOUBLEPRECISION CSWA (4), CSWAT (4), RRRSWA (4), RRRSAT (4)  !! Lateral surface contaminant convection state.
DOUBLEPRECISION RRRLS, RRRLSC, RRRLST, RRRSW, RRRSWC, RRRSWT  !! Lateral loose-sediment and surface-water contaminant terms.

!COMMON / CLSURC / CSWA, CSWAT, RRRSWA, RRRSAT, RRRLS, RRRLSC, &
 !RRRLST, RRRSW, RRRSWC, RRRSWT
!                            DATA FOR LATERAL CONVECTION AT SURFACE
DOUBLEPRECISION CCAPA (LLEE, 4), CCAPAT (LLEE, 4)  !! Convection concentration arrays in the saturated zone.

!COMMON / CLCOV / CCAPA, CCAPAT  
!                            DATA FOR CONVECTION CONC. IN THE SAT. ZONE
DOUBLEPRECISION CCAP (LLEE), COLCAP (LLEE), SCAP (LLEE), SOLCAP ( &
 LLEE)  !! Contaminant concentrations in the column.

!COMMON / CLCONC / CCAP, COLCAP, SCAP, SOLCAP  
!                            CONCENTRATIONS IN COLUMN
DOUBLEPRECISION EDCAP (LLEE), EDCAPC (LLEE), EDCAPT (LLEE)  !! Plant uptake terms from column cells.
DOUBLEPRECISION ESCAP (LLEE), ESCAPS (LLEE), ESCAPT (LLEE)  !! Soil/solution uptake or exchange terms from column cells.
DOUBLEPRECISION ESSCAP, ESSCPC, ESSCPT  !! Aggregate plant uptake or exchange terms.

!COMMON / CLPLT / EDCAP, EDCAPC, EDCAPT, ESCAP, ESCAPS, ESCAPT, &
 !ESSCAP, ESSCPC, ESSCPT
!                            RATES OF PLANT UPTAKE FROM THE COLUMN
DOUBLEPRECISION DDOD (LLEE), DDOD1 (LLEE)  !! Current and previous dispersion coefficients for the column.

!COMMON / CLDSP / DDOD, DDOD1  
!                            DISEPERSION COEFFICIENTS FOR COLUMN
DOUBLEPRECISION GNERD (LLEE), GNDSE (LLEE), GND2 (LLEE), GNDSE2 ( &
 LLEE)  !! Generation data carried to daughter contaminants.

!COMMON / GENER / GNERD, GNDSE, GND2, GNDSE2  
!                            GENERATION DATA TO BE CARRIED
!                            TO DAUGHTER CONTAMINANT
DOUBLEPRECISION AALPSO (LLEE), FFSO (LLEE), GGNNSO (LLEE), &
 KKDSO (LLEE)  !! Soil property data for contaminant calculations.

!COMMON / SOILD / AALPSO, FFSO, GGNNSO, KKDSO  
!                            SOIL PROPERTY DATA
DOUBLEPRECISION CCPRF, CCPRFT  !! Contaminant column reference/work values.
!PRIVATE :: LLEE
END MODULE COLM_CC
