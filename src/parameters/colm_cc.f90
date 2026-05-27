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
DOUBLEPRECISION :: GCAPLA       !! Scaled contaminant generation coefficient for the active contaminant.

DOUBLEPRECISION :: CSWA(4)      !! Previous surface-water concentration used on each surface face.
DOUBLEPRECISION :: CSWAT(4)     !! Time derivative of `CSWA`.
DOUBLEPRECISION :: RRRSWA(4)    !! Previous surface-water retardation factor for each surface face.
DOUBLEPRECISION :: RRRSAT(4)    !! Time derivative of `RRRSWA`.
DOUBLEPRECISION :: RRRLS        !! Loose-sediment retardation factor.
DOUBLEPRECISION :: RRRLSC       !! Concentration derivative of `RRRLS`.
DOUBLEPRECISION :: RRRLST       !! Time derivative of `RRRLS`.
DOUBLEPRECISION :: RRRSW        !! Surface-water retardation factor for the column.
DOUBLEPRECISION :: RRRSWC       !! Concentration derivative of `RRRSW`.
DOUBLEPRECISION :: RRRSWT       !! Time derivative of `RRRSW`.

DOUBLEPRECISION :: CCAPA(LLEE,4)  !! Previous advective concentration for each cell face.
DOUBLEPRECISION :: CCAPAT(LLEE,4) !! Time derivative of `CCAPA`.

DOUBLEPRECISION :: CCAP(LLEE)   !! Solved mobile-water concentration for each column cell.
DOUBLEPRECISION :: COLCAP(LLEE) !! Previous mobile-water concentration for each column cell.
DOUBLEPRECISION :: SCAP(LLEE)   !! Solved sorbed/solid concentration for each column cell.
DOUBLEPRECISION :: SOLCAP(LLEE) !! Previous sorbed/solid concentration for each column cell.

DOUBLEPRECISION :: EDCAP(LLEE)  !! Mobile-phase sink/source rate for each column cell.
DOUBLEPRECISION :: EDCAPC(LLEE) !! Concentration derivative of `EDCAP`.
DOUBLEPRECISION :: EDCAPT(LLEE) !! Time derivative of `EDCAP`.
DOUBLEPRECISION :: ESCAP(LLEE)  !! Sorbed-phase sink/source rate for each column cell.
DOUBLEPRECISION :: ESCAPS(LLEE) !! Sorbed-concentration derivative of `ESCAP`.
DOUBLEPRECISION :: ESCAPT(LLEE) !! Time derivative of `ESCAP`.
DOUBLEPRECISION :: ESSCAP       !! Aggregate surface-cell sink/source rate.
DOUBLEPRECISION :: ESSCPC       !! Concentration derivative of `ESSCAP`.
DOUBLEPRECISION :: ESSCPT       !! Time derivative of `ESSCAP`.

DOUBLEPRECISION :: DDOD(LLEE)   !! Previous scaled dispersion coefficient for each column cell.
DOUBLEPRECISION :: DDOD1(LLEE)  !! Current scaled dispersion coefficient for each column cell.

DOUBLEPRECISION :: GNERD(LLEE)  !! Mobile-phase generation term passed to daughter contaminants.
DOUBLEPRECISION :: GNDSE(LLEE)  !! Sorbed-phase generation term passed to daughter contaminants.
DOUBLEPRECISION :: GND2(LLEE)   !! Time-derivative component of `GNERD`.
DOUBLEPRECISION :: GNDSE2(LLEE) !! Time-derivative component of `GNDSE`.

DOUBLEPRECISION :: AALPSO(LLEE) !! Longitudinal dispersivity for each column cell.
DOUBLEPRECISION :: FFSO(LLEE)   !! Fraction of sorption sites assigned to equilibrium adsorption.
DOUBLEPRECISION :: GGNNSO(LLEE) !! Freundlich exponent for each column cell.
DOUBLEPRECISION :: KKDSO(LLEE)  !! Distribution coefficient for each column cell.

DOUBLEPRECISION :: CCPRF        !! Lower-boundary reference concentration.
DOUBLEPRECISION :: CCPRFT       !! Time derivative of `CCPRF`.
!PRIVATE :: LLEE
END MODULE COLM_CC
