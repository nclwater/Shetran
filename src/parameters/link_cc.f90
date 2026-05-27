!> summary: Link contaminant transport work variables.
!> author: JE, Newcastle University
!>
!> This module replaces the legacy `LINK.CC` common blocks. It stores
!> cross-sectional areas, adjacent-column concentrations, erosion rates, fluxes,
!> retardation variables, Peclet numbers, plant uptake rates, and transfer
!> coefficients used by contaminant calculations in the `LINK` routine.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-20 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1991-07-16 | JE | 3.1 | Removed `AL.P` include and `NCETOP` definition; replaced `FCPBK1`/`GCPBK1` with `FCPBK`/`GCPBK`. |
!> | 1991-07-16 | JE | 3.1 | Renamed `CONC` to `CONCL` to avoid a name clash. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE LINK_CC
   USE SGLOBAL, ONLY : LLEE
   IMPLICIT NONE

   DOUBLEPRECISION :: ACPBD1   !! Current deeper-bed/deposited-material cross-sectional area.
   DOUBLEPRECISION :: ACPBDT   !! Time derivative of `ACPBD1`.
   DOUBLEPRECISION :: ACPBS    !! Bed-surface cross-sectional area.
   DOUBLEPRECISION :: ACSFA1(6) !! Surface-water cross-sectional areas for adjacent surface compartments.
   DOUBLEPRECISION :: ACPSF1   !! Current stream-water cross-sectional area.
   DOUBLEPRECISION :: ACPSFT   !! Time derivative of `ACPSF1`.

   DOUBLEPRECISION :: CCBD1Q      !! Updated deeper-bed concentration used in post-solve flux terms.
   DOUBLEPRECISION :: CCPBK(2,LLEE) !! Adjacent bank-column dynamic-region concentrations.
   DOUBLEPRECISION :: CCPGS1(2)   !! Adjacent bank ground-surface concentrations.
   DOUBLEPRECISION :: CCBS1Q      !! Updated bed-surface concentration used in post-solve flux terms.
   DOUBLEPRECISION :: CCSFA1(6)   !! Adjacent surface-flow concentrations.
   DOUBLEPRECISION :: CCSF1Q      !! Updated stream-water concentration used in post-solve flux terms.
   DOUBLEPRECISION :: SCPBK(2,LLEE) !! Adjacent bank-column dead-space concentrations.

   DOUBLEPRECISION :: WCPBD1 !! Deeper-bed erosion/deposition rate.
   DOUBLEPRECISION :: VCPBK1 !! Bank erosion rate contribution.

   DOUBLEPRECISION :: ICP1   !! Imposed/dry-deposition flux term for stream water.
   DOUBLEPRECISION :: ICPSBD !! Infiltration/source flux to deeper bed.
   DOUBLEPRECISION :: ICSBDC !! Concentration derivative of `ICPSBD`.
   DOUBLEPRECISION :: ICSBDT !! Time derivative of `ICPSBD`.
   DOUBLEPRECISION :: ICPSBS !! Infiltration/source flux to bed surface.
   DOUBLEPRECISION :: ICSBSC !! Concentration derivative of `ICPSBS`.
   DOUBLEPRECISION :: ICSBST !! Time derivative of `ICPSBS`.
   DOUBLEPRECISION :: QCP1   !! Rainfall, well, or imposed-flow concentration flux.

   DOUBLEPRECISION :: FCPBD      !! Retardation factor for deeper-bed concentration.
   DOUBLEPRECISION :: FCPBDC     !! Concentration derivative of `FCPBD`.
   DOUBLEPRECISION :: FCBD1Q     !! Updated deeper-bed retardation factor.
   DOUBLEPRECISION :: FCPBDT     !! Time derivative of `FCPBD`.
   DOUBLEPRECISION :: FCPBK(2,LLEE) !! Bank mobile-region retardation factors.
   DOUBLEPRECISION :: FCPBS      !! Retardation factor for bed-surface concentration.
   DOUBLEPRECISION :: FCPBSC     !! Concentration derivative of `FCPBS`.
   DOUBLEPRECISION :: FCBS1Q     !! Updated bed-surface retardation factor.
   DOUBLEPRECISION :: FCPBST     !! Time derivative of `FCPBS`.
   DOUBLEPRECISION :: FCPSW1(2)  !! Updated stream-water retardation factors at adjacent banks.
   DOUBLEPRECISION :: FCPSD      !! Retardation factor for deposited/deep-bed exchange.
   DOUBLEPRECISION :: FCPSDC     !! Concentration derivative of `FCPSD`.
   DOUBLEPRECISION :: FCPSDT     !! Time derivative of `FCPSD`.
   DOUBLEPRECISION :: FCPSF      !! Retardation factor for stream-water concentration.
   DOUBLEPRECISION :: FCSFA1(6)  !! Adjacent surface-flow retardation factors.
   DOUBLEPRECISION :: FCPSFC     !! Concentration derivative of `FCPSF`.
   DOUBLEPRECISION :: FCSF1Q     !! Updated stream-water retardation factor.
   DOUBLEPRECISION :: FCPSFT     !! Time derivative of `FCPSF`.
   DOUBLEPRECISION :: GCPBK(2,LLEE) !! Bank dead-space retardation factors.

   INTEGER :: NCEBK(2) !! First exposed bank cell number for each bank side.

   DOUBLEPRECISION :: PCPBK1(2,LLEE) !! Peclet numbers for bank-column exchange.
   DOUBLEPRECISION :: PCPSB1(2)      !! Peclet numbers for stream-bed exchange at each bank.
   DOUBLEPRECISION :: PCSFA1(6)      !! Peclet numbers for adjacent surface-flow exchange.
   DOUBLEPRECISION :: PCPSW1(2)      !! Peclet numbers for stream-water exchange at each bank.
   DOUBLEPRECISION :: PCSFM1         !! Peclet number for downstream stream-water exchange.
   DOUBLEPRECISION :: PCSFP1         !! Peclet number for upstream stream-water exchange.

   DOUBLEPRECISION :: ECPBD  !! Plant uptake or biological sink in the deeper-bed compartment.
   DOUBLEPRECISION :: ECPBDC !! Concentration derivative of `ECPBD`.
   DOUBLEPRECISION :: ECPBDT !! Time derivative of `ECPBD`.
   DOUBLEPRECISION :: ECPBS  !! Plant uptake or biological sink in the bed-surface compartment.
   DOUBLEPRECISION :: ECPBSC !! Concentration derivative of `ECPBS`.
   DOUBLEPRECISION :: ECPBST !! Time derivative of `ECPBS`.
   DOUBLEPRECISION :: ECPSF  !! Plant uptake or biological sink in the stream-water compartment.
   DOUBLEPRECISION :: ECPSFC !! Concentration derivative of `ECPSF`.
   DOUBLEPRECISION :: ECPSFT !! Time derivative of `ECPSF`.

   DOUBLEPRECISION :: USCP !! Stream-water presence switch: wet links solve stream-water storage.

   DOUBLEPRECISION :: ACSBD1 !! Transfer coefficient between deeper bed and bed surface.
   DOUBLEPRECISION :: ACSBS1 !! Transfer coefficient between bed surface and stream water.
   DOUBLEPRECISION :: GCPLAL !! Linear decay/generation coefficient for link compartments.
   DOUBLEPRECISION :: GCPLAQ !! Quadratic/flow-scaled decay or generation coefficient.
!PRIVATE :: LLEE
END MODULE LINK_CC
