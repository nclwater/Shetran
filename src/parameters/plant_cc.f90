!> summary: Plant contaminant uptake data and crop property arrays.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the legacy `PLANT.CC` common blocks and the associated
!> `PLDAT` block-data initialization. It stores plant contaminant concentrations,
!> crop property factors, plant type indices, uptake coefficients, plant mass,
!> and root-distribution factors used by contaminant plant uptake routines.
!>
!> Legacy `PLDAT` defaults and initialization notes:
!>
!> | Symbol | Default | Meaning |
!> |:-------|:--------|:--------|
!> | `BCPAA`, `BCPBB` | `0.0` | Initial plant relative concentrations. |
!> | `DELONE` | `0.5` | Initial plant compartment-A/B partition factor. |
!> | `DELTWO` | `0.9` | Initial root-zone uptake partition factor. |
!> | `DELTHR` | `1.0` | Initial compartment-B active-mass scaling factor. |
!> | `FLEFT` | `1.0` in legacy `PLDAT`; no current initializer | Residual plant fraction used when canopy leaf area is zero. |
!> | `RHOPL` | `500.0` | Plant material density, used for scaling only. |
!> | `NPLTYP(:,2)` | `1` | Initial second plant type on each soil column. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked, no changes. |
!> | 1993-03-16 | JE | 3.4 | Full implementation. |
!> | 1997-02-24 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE PLANT_CC
   USE SGLOBAL, ONLY : NELEE, NLFEE, LLEE, NPELEE, NCONEE, NPLTEE
   IMPLICIT NONE

   INTEGER, PARAMETER :: NTEMP1=2*NELEE*NPELEE*NCONEE !! Former `PLDAT` initializer count for `BCPAA` and `BCPBB`.
   INTEGER, PARAMETER :: NTEMP2=NPLTEE*NCONEE        !! Former `PLDAT` initializer count for plant-type contaminant data.


   DOUBLEPRECISION :: GENAA (NPELEE) !! Plant compartment-A generation/decay term by plant slot.
   DOUBLEPRECISION :: GENBB (NPELEE) !! Plant compartment-B generation/decay term by plant slot.
   DOUBLEPRECISION :: GCPL           !! Current contaminant decay/generation coefficient for plant uptake calculations.
   DOUBLEPRECISION :: GMCPAA         !! Current scaled plant mass for compartment A.
   DOUBLEPRECISION :: GMCPBB         !! Current scaled plant mass for compartment B.
   DOUBLEPRECISION :: GMCBBD         !! Time derivative of the scaled plant compartment-B mass.
   DOUBLEPRECISION :: QCPAA          !! Plant uptake rate assigned to compartment A.
   DOUBLEPRECISION :: QCPBB          !! Plant uptake rate assigned to compartment B.
   DOUBLEPRECISION :: RHOPL=500.0d0  !! Plant material density used for concentration scaling.

!COMMON / ALOCAL / GENAA, GENBB

!COMMON / VLOCAL / GCPL, GMCPAA, GMCPBB, GMCBBD, QCPAA, QCPBB, &
   !RHOPL
!                       Arrays and variables used only in plant routines
   DOUBLEPRECISION :: BCPAA (NELEE, NPELEE, NCONEE)=0.0d0 !! Relative concentration in plant compartment A.
   DOUBLEPRECISION :: BCPBB (NELEE, NPELEE, NCONEE)=0.0d0 !! Relative concentration in plant compartment B.

!COMMON / BCON / BCPAA, BCPBB
   DOUBLEPRECISION :: DELONE (NPLTEE)=0.5 !! Plant compartment-A/B partition factor by plant type.
   DOUBLEPRECISION :: DELTWO (NPLTEE)=0.9 !! Root-zone uptake partition factor by plant type.
   DOUBLEPRECISION :: DELTHR (NPLTEE)=1.0 !! Compartment-B active-mass scaling factor by plant type.
   DOUBLEPRECISION :: DELFOU (NPLTEE)=1.0 !! Current live/residual plant factor by plant type.
   DOUBLEPRECISION :: FLEFT (NPLTEE)      !! Residual plant factor used when canopy leaf area is zero.

!COMMON / DELTA / DELONE, DELTWO, DELTHR, DELFOU, FLEFT
!                 Plant and cropping property data
   DOUBLEPRECISION :: GMCBBO (NELEE, NPELEE) !! Previous time-step scaled mass for plant compartment B.

!COMMON / GMOLD / GMCBBO
!                 Old values for masses in compartment b
   INTEGER :: NPL (NELEE)             !! Number of plant slots active on each soil column.
   INTEGER :: NPLTYP (NELEE, NPELEE)=1 !! Plant type number for each soil-column plant slot.
   INTEGER :: NPLT                    !! Total number of plant types.

!COMMON / NUMPL / NPL, NPLTYP, NPLT
!                 Total number of plants, and their type numbers, on
!                 each soil column
   DOUBLEPRECISION :: PKMAX (NPLTEE, NCONEE) !! Maximum contaminant uptake coefficient by plant type and contaminant.
   DOUBLEPRECISION :: PMASS (NPLTEE)         !! Maximum plant material mass per unit area by plant type.

!COMMON / MASSP / PKMAX, PMASS
!                 Contaminant uptake coefficient, and maximum mass of
!                 plant material per unit area
   DOUBLEPRECISION :: PFONE (NELEE, NPELEE)       !! Soil-column plant area fraction by plant slot.
   DOUBLEPRECISION :: PFTWO (NPLTEE)              !! Current canopy leaf area index by plant type.
   DOUBLEPRECISION :: PF2MAX (NPLTEE)             !! Maximum canopy leaf area index by plant type.
   DOUBLEPRECISION :: PDZF3 (NELEE, NPELEE, LLEE) !! Root distribution fraction by soil column, plant slot, and layer.

!COMMON / PF123 / PFONE, PFTWO, PF2MAX, PDZF3
!                 nb  PFTWO and PF2MAX are specified for each plant type
!                 PLAI, CLAI, and RDF for use in contaminant plant
!                 uptake routines
   DOUBLEPRECISION :: XXI !! Dissolved/solid uptake weighting factor used in plant contaminant uptake.
!PRIVATE :: NELEE, NLFEE, LLEE, NPELEE, NCONEE, NPLTEE
end MODULE PLANT_CC

!      BLOCK DATA PLDAT
!*           Plant data
!
!      USE SGLOBAL
!      USE AL_C
!      USE COLM_CC
!
!      USE PLANT_CC
!      PARAMETER(NTEMP1=2*NELEE*NPELEE*NCONEE, NTEMP2=NPLTEE*NCONEE)
!
!
!      DATA BCPAA,BCPBB / NTEMP1*0.0D0 /
!*                 Initialise plant relative concentrations
!
!      DATA DELONE / NPLTEE*half / DELTWO / NPLTEE*0.9D0 /
!     &     DELTHR / NPLTEE*one / FLEFT  / NPLTEE*one /
!*                 Plant and cropping property data
!
!*     DATA ESSCAP,ESSCPC,ESSCPT / 3*0.0D0 /
!*                 Ensures there is no uptake from surface water
!*                 and sediments
!
!      DATA RHOPL / 500.0D0 /
!*                 Density of plant material (used in scaling only)
!
!      DATA (NPLTYP(I,2),I=1,NELEE) / NELEE*1 /
!*                 Second plant type on each soil column
!
!      END
