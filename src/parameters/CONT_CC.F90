!> summary: Defines global variables and data for contaminant transport.
!> author: J. Ewen, R.A.H.
!> date: 2004-11-01
!>
!> This module provides shared variables for contaminant transport across the entire catchment.
!> It includes boundary conditions, concentrations, decay rates, soil properties, and retardation factors.
!> It was converted from a legacy Fortran 77 INCLUDE file.
!>
!> @history
!> | Date       | Author | Version | Description                               |
!> |:-----------|:-------|:--------|:------------------------------------------|
!> | 1991-04-26 | JE     | 3.1     | Original `INCLUDE` file written.          |
!> | 1991-06-13 | JE     | 3.1     | Completed.                                |
!> | 1991-06-18 | JE     | 3.1     | BLOCK WELC added.                         |
!> | 1997-02-24 | RAH    | 4.1     | Explicit typing.                          |
!> | 2004-11-01 | JE     | -       | Converted to Fortran 95.                  |
!> | 2025-08-11 | AI     | -       | Added KIND parameters and FORD docs.      |
MODULE CONT_CC

   USE SGLOBAL, ONLY : NELEE, NCONEE, LLEE, NSEE, NSEDEE, NLFEE
   USE MOD_PARAMETERS, ONLY: R8P, I_P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: CCAPB, CCPBO, CCAPE, CCAPI, CCAPIO, CCAPR, CCAPRO, IIICF, IIICFO
   PUBLIC :: CCCCW
   PUBLIC :: CCCC, CCCCO, SSSS, SSSSO
   PUBLIC :: GCPLA, GGLMSO
   PUBLIC :: CCAPIN
   PUBLIC :: ALPHA, FADS, GNN, KDDLS, KDDSOL
   PUBLIC :: NCON
   PUBLIC :: FCPBKO, GCPBKO, FSF, FSFC, FSFT, RSW, RSWC, RSWT
   PUBLIC :: ALPHBD, ALPHBS

   ! Code =====================================================================

   ! Contaminant concentration and flux boundary conditions
   REAL(KIND=R8P) :: CCAPB(NELEE,NCONEE)    !! Boundary contaminant concentration
   REAL(KIND=R8P) :: CCPBO(NELEE,NCONEE)    !! Boundary contaminant concentration at previous time step
   REAL(KIND=R8P) :: CCAPE(NELEE,NCONEE)    !! Boundary contaminant concentration (end value)
   REAL(KIND=R8P) :: CCAPI(NCONEE)          !! Initial boundary contaminant concentration
   REAL(KIND=R8P) :: CCAPIO(NCONEE)         !! Initial boundary contaminant concentration at previous time step
   REAL(KIND=R8P) :: CCAPR(NELEE,NCONEE)    !! Rainfall contaminant concentration
   REAL(KIND=R8P) :: CCAPRO(NELEE,NCONEE)   !! Rainfall contaminant concentration at previous time step
   REAL(KIND=R8P) :: IIICF(NCONEE)          !! Inflow contaminant flux
   REAL(KIND=R8P) :: IIICFO(NCONEE)         !! Inflow contaminant flux at previous time step

   ! Contaminant concentration in well water
   REAL(KIND=R8P) :: CCCCW(NELEE,NCONEE)    !! Contaminant concentration in well water

   ! Concentrations within catchment
   REAL(KIND=R8P) :: CCCC(NELEE,LLEE,NCONEE)  !! Aqueous phase concentration
   REAL(KIND=R8P) :: CCCCO(NELEE,LLEE,NCONEE) !! Aqueous phase concentration at previous time step
   REAL(KIND=R8P) :: SSSS(NELEE,LLEE,NCONEE)  !! Sorbed phase concentration
   REAL(KIND=R8P) :: SSSSO(NELEE,LLEE,NCONEE) !! Sorbed phase concentration at previous time step

   ! Contaminant decay rates
   REAL(KIND=R8P) :: GCPLA(NCONEE)          !! General contaminant decay rate parameter
   REAL(KIND=R8P) :: GGLMSO(NCONEE)         !! Contaminant decay rate in soil

   ! Initial concentration
   REAL(KIND=R8P) :: CCAPIN(NCONEE)         !! Initial contaminant concentration

   ! Contaminant properties for soil and sediment
   REAL(KIND=R8P) :: ALPHA(NSEE,NCONEE)     !! Sorption/desorption rate coefficient
   REAL(KIND=R8P) :: FADS(NSEE,NCONEE)      !! Fraction of adsorption sites
   REAL(KIND=R8P) :: GNN(NCONEE)            !! Soil bulk density (kg/m3)
   REAL(KIND=R8P) :: KDDLS(NSEDEE,NCONEE)   !! Partition coefficient for loose sediment (m3/kg)
   REAL(KIND=R8P) :: KDDSOL(NSEE,NCONEE)    !! Partition coefficient for soil (m3/kg)

   ! Number of contaminants
   INTEGER(KIND=I_P) :: NCON               !! Total number of contaminants being simulated

   ! Retardation variables for lateral coupling (bank erosion, surface flow)
   REAL(KIND=R8P) :: FCPBKO(NLFEE,2,LLEE,NCONEE) !! Retardation factor component
   REAL(KIND=R8P) :: GCPBKO(NLFEE,2,LLEE,NCONEE) !! Retardation factor component
   REAL(KIND=R8P) :: FSF(NLFEE,NCONEE)           !! Retardation factor for surface flow
   REAL(KIND=R8P) :: FSFC(NLFEE,NCONEE)          !! Retardation factor constant for surface flow
   REAL(KIND=R8P) :: FSFT(NLFEE,NCONEE)          !! Retardation factor for surface flow at previous time step
   REAL(KIND=R8P) :: RSW(NELEE,NCONEE)           !! Retardation factor for surface water
   REAL(KIND=R8P) :: RSWC(NELEE,NCONEE)          !! Retardation factor constant for surface water
   REAL(KIND=R8P) :: RSWT(NELEE,NCONEE)          !! Retardation factor for surface water at previous time step

   ! Coefficients for exchange between cells of a link
   REAL(KIND=R8P) :: ALPHBD(NCONEE)         !! Exchange coefficient (dissolved phase)
   REAL(KIND=R8P) :: ALPHBS(NCONEE)         !! Exchange coefficient (sorbed phase)

END MODULE CONT_CC
