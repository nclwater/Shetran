!> summary: Defines parameters and variables for plant contaminant data.
!> author: J. Ewen
!> date: 2008-08-12
!>
!> This module contains variables related to contaminant uptake and concentration
!> in plants. It includes plant properties, contaminant coefficients, and arrays
!> for tracking masses and concentrations within different plant components.
!> It was converted from a legacy `INCLUDE` file and its associated `BLOCK DATA`.
!>
!> @history
!> | Date       | Author | Version  | Description                               |
!> |:-----------|:-------|:---------|:------------------------------------------|
!> | 1991-04-?? | JE     | 3.0      | Original `INCLUDE` file written.          |
!> | 1991-06-13 | JE     | 3.1      | Checked, no changes.                      |
!> | 1993-03-16 | JE     | 3.4      | Full implementation.                      |
!> | 1997-02-24 | RAH    | 4.1      | Explicit typing.                          |
!> | 2008-08-12 | JE     | 4.3.5F90 | Converted to Fortran 90 module.           |
!> | 2025-08-11 | AI     | -        | Added FORD docs, modernized declarations. |
MODULE PLANT_CC

   USE SGLOBAL, ONLY : NELEE, NLFEE, LLEE, NPELEE, NCONEE, NPLTEE
   USE MOD_PARAMETERS, ONLY: R8P, I_P
   IMPLICIT NONE

   ! The legacy COMMON block /ALOCAL/ was replaced by these module variables.
   REAL(KIND=R8P) :: GENAA(NPELEE)
   REAL(KIND=R8P) :: GENBB(NPELEE)

   ! The legacy COMMON block /VLOCAL/ was replaced by these module variables.
   ! These are used only in plant routines.
   REAL(KIND=R8P) :: GCPL
   REAL(KIND=R8P) :: GMCPAA
   REAL(KIND=R8P) :: GMCPBB
   REAL(KIND=R8P) :: GMCBBD
   REAL(KIND=R8P) :: QCPAA
   REAL(KIND=R8P) :: QCPBB
   REAL(KIND=R8P) :: RHOPL = 500.0_R8P !! Density of plant material (used in scaling only).

   ! The legacy COMMON block /BCON/ was replaced by these module variables.
   REAL(KIND=R8P) :: BCPAA(NELEE, NPELEE, NCONEE) = 0.0_R8P !! Initial plant relative concentrations.
   REAL(KIND=R8P) :: BCPBB(NELEE, NPELEE, NCONEE) = 0.0_R8P !! Initial plant relative concentrations.

   ! The legacy COMMON block /DELTA/ was replaced by these module variables.
   ! These hold plant and cropping property data.
   REAL(KIND=R8P) :: DELONE(NPLTEE) = 0.5_R8P
   REAL(KIND=R8P) :: DELTWO(NPLTEE) = 0.9_R8P
   REAL(KIND=R8P) :: DELTHR(NPLTEE) = 1.0_R8P
   REAL(KIND=R8P) :: DELFOU(NPLTEE) = 1.0_R8P
   REAL(KIND=R8P) :: FLEFT(NPLTEE)

   ! The legacy COMMON block /GMOLD/ was replaced by this module variable.
   REAL(KIND=R8P) :: GMCBBO(NELEE, NPELEE) !! Old values for masses in compartment b.

   ! The legacy COMMON block /NUMPL/ was replaced by these module variables.
   INTEGER(KIND=I_P) :: NPL(NELEE)      !! Total number of plants on each soil column.
   INTEGER(KIND=I_P) :: NPLTYP(NELEE, NPELEE) = 1 !! Plant type numbers on each soil column.
   INTEGER(KIND=I_P) :: NPLT            !! Total number of plant types.

   ! The legacy COMMON block /MASSP/ was replaced by these module variables.
   REAL(KIND=R8P) :: PKMAX(NPLTEE, NCONEE) !! Contaminant uptake coefficient.
   REAL(KIND=R8P) :: PMASS(NPLTEE)         !! Maximum mass of plant material per unit area.

   ! The legacy COMMON block /PF123/ was replaced by these module variables.
   ! PFTWO and PF2MAX are specified for each plant type.
   ! PLAI, CLAI, and RDF for use in contaminant plant uptake routines.
   REAL(KIND=R8P) :: PFONE(NELEE, NPELEE)
   REAL(KIND=R8P) :: PFTWO(NPLTEE)
   REAL(KIND=R8P) :: PF2MAX(NPLTEE)
   REAL(KIND=R8P) :: PDZF3(NELEE, NPELEE, LLEE)

   REAL(KIND=R8P) :: XXI

end MODULE PLANT_CC
