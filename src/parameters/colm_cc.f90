!> summary: Defines common variables for contaminant transport in the `COLM` subroutine.
!> author: J. Ewen, R.A.H.
!> date: 2008-12-01
!>
!> This module provides shared variables for contaminant-related processes,
!> such as concentrations, plant uptake, and soil properties, used within the `COLM` subroutine.
!> It was converted from a legacy Fortran 77 INCLUDE file.
!>
!> @history
!> | Date       | Author | Version  | Description                               |
!> |:-----------|:-------|:---------|:------------------------------------------|
!> | 1991-04-26 | JE     | 3.0      | Original `INCLUDE` file written.          |
!> | 1991-06-13 | JE     | 3.1      | Checked, no changes.                      |
!> | 1991-06-16 | JE     | 3.1      | References to CCPRV removed.              |
!> | 1997-03-13 | RAH    | 4.1      | Explicit typing.                          |
!> | 2008-12-01 | JE     | 4.3.5F90 | Converted to Fortran 90.                  |
!> | 2025-08-11 | AI     | -        | Added KIND parameters and FORD docs.      |
MODULE COLM_CC

   USE SGLOBAL, ONLY : LLEE
   USE MOD_PARAMETERS, ONLY: R8P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: GCAPLA
   PUBLIC :: CSWA, CSWAT, RRRSWA, RRRSAT, RRRLS, RRRLSC, RRRLST, RRRSW, RRRSWC, RRRSWT
   PUBLIC :: CCAPA, CCAPAT
   PUBLIC :: CCAP, COLCAP, SCAP, SOLCAP
   PUBLIC :: EDCAP, EDCAPC, EDCAPT, ESCAP, ESCAPS, ESCAPT, ESSCAP, ESSCPC, ESSCPT
   PUBLIC :: DDOD, DDOD1
   PUBLIC :: GNERD, GNDSE, GND2, GNDSE2
   PUBLIC :: AALPSO, FFSO, GGNNSO, KKDSO
   PUBLIC :: CCPRF, CCPRFT

   ! Code =====================================================================

   REAL(KIND=R8P) :: GCAPLA   !! General capacity parameter

   ! Data for lateral convection at surface
   REAL(KIND=R8P) :: CSWA(4)      !! Surface water contaminant concentration
   REAL(KIND=R8P) :: CSWAT(4)     !! Surface water contaminant concentration at previous time step
   REAL(KIND=R8P) :: RRRSWA(4)    !! Lateral surface water contaminant flux
   REAL(KIND=R8P) :: RRRSAT(4)    !! Lateral surface water contaminant flux at previous time step
   REAL(KIND=R8P) :: RRRLS        !! Lateral loose sediment contaminant flux
   REAL(KIND=R8P) :: RRRLSC       !! Lateral loose sediment contaminant flux constant
   REAL(KIND=R8P) :: RRRLST       !! Lateral loose sediment contaminant flux at previous time step
   REAL(KIND=R8P) :: RRRSW        !! Lateral surface water contaminant flux
   REAL(KIND=R8P) :: RRRSWC       !! Lateral surface water contaminant flux constant
   REAL(KIND=R8P) :: RRRSWT       !! Lateral surface water contaminant flux at previous time step

   ! Data for convection concentration in the saturated zone
   REAL(KIND=R8P) :: CCAPA(LLEE, 4)   !! Convective contaminant concentration
   REAL(KIND=R8P) :: CCAPAT(LLEE, 4)  !! Convective contaminant concentration at previous time step

   ! Concentrations in column
   REAL(KIND=R8P) :: CCAP(LLEE)       !! Aqueous phase concentration (kg/m3)
   REAL(KIND=R8P) :: COLCAP(LLEE)     !! Total concentration in the column (kg/m3)
   REAL(KIND=R8P) :: SCAP(LLEE)       !! Sorbed phase concentration (kg/kg)
   REAL(KIND=R8P) :: SOLCAP(LLEE)     !! Solid phase concentration (kg/m3)

   ! Rates of plant uptake from the column
   REAL(KIND=R8P) :: EDCAP(LLEE)      !! Plant uptake rate from dissolved phase
   REAL(KIND=R8P) :: EDCAPC(LLEE)     !! Plant uptake rate constant from dissolved phase
   REAL(KIND=R8P) :: EDCAPT(LLEE)     !! Plant uptake rate from dissolved phase at previous time step
   REAL(KIND=R8P) :: ESCAP(LLEE)      !! Plant uptake rate from sorbed phase
   REAL(KIND=R8P) :: ESCAPS(LLEE)     !! Plant uptake rate from sorbed phase (scaled)
   REAL(KIND=R8P) :: ESCAPT(LLEE)     !! Plant uptake rate from sorbed phase at previous time step
   REAL(KIND=R8P) :: ESSCAP           !! Total plant uptake rate from sorbed phase
   REAL(KIND=R8P) :: ESSCPC           !! Total plant uptake rate constant from sorbed phase
   REAL(KIND=R8P) :: ESSCPT           !! Total plant uptake rate from sorbed phase at previous time step

   ! Dispersion coefficients for column
   REAL(KIND=R8P) :: DDOD(LLEE)       !! Dispersion coefficient
   REAL(KIND=R8P) :: DDOD1(LLEE)      !! Dispersion coefficient at previous time step

   ! Generation data to be carried to daughter contaminant
   REAL(KIND=R8P) :: GNERD(LLEE)      !! Generation rate from dissolved phase
   REAL(KIND=R8P) :: GNDSE(LLEE)      !! Generation rate from sorbed phase
   REAL(KIND=R8P) :: GND2(LLEE)       !! Generation rate parameter
   REAL(KIND=R8P) :: GNDSE2(LLEE)     !! Generation rate parameter for sorbed phase

   ! Soil property data
   REAL(KIND=R8P) :: AALPSO(LLEE)     !! Soil property related to sorption/desorption
   REAL(KIND=R8P) :: FFSO(LLEE)       !! Fraction of organic matter in soil
   REAL(KIND=R8P) :: GGNNSO(LLEE)     !! Soil bulk density (kg/m3)
   REAL(KIND=R8P) :: KKDSO(LLEE)      !! Soil-water partition coefficient (m3/kg)

   ! Not part of a common block in the original F77 code
   REAL(KIND=R8P) :: CCPRF            !! Reference contaminant concentration
   REAL(KIND=R8P) :: CCPRFT           !! Reference contaminant concentration at previous time step

END MODULE COLM_CC
