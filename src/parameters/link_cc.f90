!> summary: Provides contaminant transport variables for the LINK subroutine.
!> author: J. Ewen
!> date: 2008-08-12
!>
!> This module contains variables related to contaminant transport processes
!> within the river link component of the model. It includes variables for
!> concentrations, fluxes, retardation, erosion, and plant uptake.
!>
!> @history
!> | Date       | Author | Version  | Description                                                              |
!> |:-----------|:-------|:---------|:-------------------------------------------------------------------------|
!> | 1991-05-20 | JE     | 3.0      | Original `INCLUDE` file written.                                         |
!> | 1991-06-13 | JE     | 3.1      | Completed.                                                               |
!> | 1991-07-16 | JE     | 3.1      | Removed AL.P, NCETOP. Replaced FCPBK1, GCPBK1 with FCPBK, GCPBK.           |
!> | 1991-07-16 | JE     | 3.1      | Changed CONC to CONCL to avoid name clash.                               |
!> | 2008-08-12 | JE     | 4.3.5F90 | Converted to Fortran 90.                                                 |
!> | 2025-08-11 | AI     | -        | Added KIND parameters and FORD docs.                                     |
MODULE LINK_CC
   USE SGLOBAL, ONLY : LLEE
   USE MOD_PARAMETERS, ONLY: R8P, I_P
   IMPLICIT NONE

   ! Cross-sectional areas of link cells
   REAL(KIND=R8P) :: ACPBD1     !! Area coefficient for bed type 1
   REAL(KIND=R8P) :: ACPBDT     !! Transient area coefficient for bed
   REAL(KIND=R8P) :: ACPBS      !! Steady area coefficient for bed
   REAL(KIND=R8P) :: ACSFA1(6)  !! Area coefficients for surface flow (6 elements)
   REAL(KIND=R8P) :: ACPSF1     !! Area coefficient for surface type 1
   REAL(KIND=R8P) :: ACPSFT     !! Transient area coefficient for surface

   ! Concentrations in adjacent columns and links, and concentrations of parent contaminants
   REAL(KIND=R8P) :: CCBD1Q     !! Concentration coefficient for bed type 1
   REAL(KIND=R8P) :: CCPBK(2,LLEE) !! Concentration coefficient for bank
   REAL(KIND=R8P) :: CCPGS1(2)  !! Concentration coefficient for groundwater type 1 (2 elements)
   REAL(KIND=R8P) :: CCBS1Q     !! Steady concentration coefficient for bed type 1
   REAL(KIND=R8P) :: CCSFA1(6)  !! Concentration coefficients for surface (6 elements)
   REAL(KIND=R8P) :: CCSF1Q     !! Concentration coefficient for surface type 1
   REAL(KIND=R8P) :: SCPBK(2,LLEE) !! Parent contaminant concentration in bank

   ! Erosion rates
   REAL(KIND=R8P) :: WCPBD1     !! Water coefficient for bed type 1
   REAL(KIND=R8P) :: VCPBK1     !! Velocity coefficient for bank type 1

   ! Fluxes
   REAL(KIND=R8P) :: ICP1       !! Infiltration coefficient type 1
   REAL(KIND=R8P) :: ICPSBD     !! Infiltration coefficient for bed
   REAL(KIND=R8P) :: ICSBDC     !! Infiltration coefficient for bed concentration
   REAL(KIND=R8P) :: ICSBDT     !! Transient infiltration coefficient for bed
   REAL(KIND=R8P) :: ICPSBS     !! Steady infiltration coefficient for bed
   REAL(KIND=R8P) :: ICSBSC     !! Steady infiltration coefficient for bed concentration
   REAL(KIND=R8P) :: ICSBST     !! Steady transient infiltration coefficient for bed
   REAL(KIND=R8P) :: QCP1       !! Flow coefficient type 1

   ! Retardation variables
   REAL(KIND=R8P) :: FCPBD      !! Retardation factor for bed
   REAL(KIND=R8P) :: FCPBDC     !! Retardation factor for bed concentration
   REAL(KIND=R8P) :: FCBD1Q     !! Retardation factor for bed type 1
   REAL(KIND=R8P) :: FCPBDT     !! Transient retardation factor for bed
   REAL(KIND=R8P) :: FCPBK(2,LLEE) !! Retardation factor for bank
   REAL(KIND=R8P) :: FCPBS      !! Steady retardation factor for bed
   REAL(KIND=R8P) :: FCPBSC     !! Steady retardation factor for bed concentration
   REAL(KIND=R8P) :: FCBS1Q     !! Steady retardation factor for bed type 1
   REAL(KIND=R8P) :: FCPBST     !! Steady transient retardation factor for bed
   REAL(KIND=R8P) :: FCPSW1(2)  !! Retardation factor for surface water (2 elements)
   REAL(KIND=R8P) :: FCPSD      !! Retardation factor for sediment
   REAL(KIND=R8P) :: FCPSDC     !! Retardation factor for sediment concentration
   REAL(KIND=R8P) :: FCPSDT     !! Transient retardation factor for sediment
   REAL(KIND=R8P) :: FCPSF      !! Retardation factor for surface
   REAL(KIND=R8P) :: FCSFA1(6)  !! Retardation factors for surface (6 elements)
   REAL(KIND=R8P) :: FCPSFC     !! Retardation factor for surface concentration
   REAL(KIND=R8P) :: FCSF1Q     !! Retardation factor for surface type 1
   REAL(KIND=R8P) :: FCPSFT     !! Transient retardation factor for surface
   REAL(KIND=R8P) :: GCPBK(2,LLEE) !! Growth coefficient for bank

   ! Node numbers in the stream banks
   INTEGER(KIND=I_P) :: NCEBK(2) !! Bank element numbers (2 elements)

   ! Peclet numbers
   REAL(KIND=R8P) :: PCPBK1(2,LLEE) !! Peclet numbers for bank
   REAL(KIND=R8P) :: PCPSB1(2)  !! Peclet numbers for bank surface (2 elements)
   REAL(KIND=R8P) :: PCSFA1(6)  !! Peclet numbers for surface (6 elements)
   REAL(KIND=R8P) :: PCPSW1(2)  !! Peclet numbers for surface water (2 elements)
   REAL(KIND=R8P) :: PCSFM1     !! Mean Peclet number for surface
   REAL(KIND=R8P) :: PCSFP1     !! Plus Peclet number for surface

   ! Plant uptake rates
   REAL(KIND=R8P) :: ECPBD      !! Plant uptake rate for bed
   REAL(KIND=R8P) :: ECPBDC     !! Plant uptake rate for bed concentration
   REAL(KIND=R8P) :: ECPBDT     !! Transient plant uptake rate for bed
   REAL(KIND=R8P) :: ECPBS      !! Steady plant uptake rate for bed
   REAL(KIND=R8P) :: ECPBSC     !! Steady plant uptake rate for bed concentration
   REAL(KIND=R8P) :: ECPBST     !! Steady transient plant uptake rate for bed
   REAL(KIND=R8P) :: ECPSF      !! Plant uptake rate for surface
   REAL(KIND=R8P) :: ECPSFC     !! Plant uptake rate for surface concentration
   REAL(KIND=R8P) :: ECPSFT     !! Transient plant uptake rate for surface

   ! Switch for presence or otherwise of stream water
   REAL(KIND=R8P) :: USCP       !! Switch for stream water presence

   ! Transfer coefficients and decay constants
   REAL(KIND=R8P) :: ACSBD1     !! Area coefficient for sediment bed type 1
   REAL(KIND=R8P) :: ACSBS1     !! Steady area coefficient for sediment bed type 1
   REAL(KIND=R8P) :: GCPLAL     !! Growth coefficient for plant algae
   REAL(KIND=R8P) :: GCPLAQ     !! Growth coefficient for aquatic plants

END MODULE LINK_CC
