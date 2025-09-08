!> summary: Defines additional common variables for water flow calculations in soil columns.
!> author: J. Ewen, R.A.H.
!> date: 2008-12-01
!>
!> This module provides a second set of shared variables used in the `COLM` subroutine,
!> primarily related to water flow data. It was converted from a legacy Fortran 77 INCLUDE file.
!>
!> @history
!> | Date       | Author | Version  | Description                               |
!> |:-----------|:-------|:---------|:------------------------------------------|
!> | 1991-04-22 | JE     | 3.1      | Original `INCLUDE` file written.          |
!> | 1991-06-13 | JE     | 3.1      | Completed.                                |
!> | 1997-03-13 | RAH    | 4.1      | Explicit typing.                          |
!> | 2008-12-01 | JE     | 4.3.5F90 | Converted to Fortran 90.                  |
!> | 2025-08-11 | AI     | -        | Added KIND parameters and FORD docs.      |
MODULE COLM_C2

   USE SGLOBAL, ONLY: LLEE
   USE MOD_PARAMETERS, ONLY: R8P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: ICAP, ICAPT, ICAPC, QCAP, QCAPT, QCAPC
   PUBLIC :: DDA, DDB, DDDLS, DDDLS1, DDDSW, DDDSW1, GGGNU, GGGNU1, KSP, KSPP, ZONE, ZONE1
   PUBLIC :: TTTLSE
   PUBLIC :: QQQSW, QQQSW1
   PUBLIC :: GGAMM, GGAMM1, PPHI, PPHI1, QQ, QQ1, TTHET, TTHET1, UUAJP, UUAJP1
   PUBLIC :: QQRF, QQRF1
   PUBLIC :: QI, QI1

   ! Code =====================================================================

   ! For compatibility with variable names in WRSRU/TR's
   REAL(KIND=R8P) :: ICAP     !! Interception capacity (m)
   REAL(KIND=R8P) :: ICAPT    !! Interception capacity at previous time step (m)
   REAL(KIND=R8P) :: ICAPC    !! Interception capacity constant (m)
   REAL(KIND=R8P) :: QCAP     !! Interception storage (m)
   REAL(KIND=R8P) :: QCAPT    !! Interception storage at previous time step (m)
   REAL(KIND=R8P) :: QCAPC    !! Interception storage constant (m)

   ! Numbering and size data for column
   REAL(KIND=R8P) :: DDA      !! Vertical grid spacing (m)
   REAL(KIND=R8P) :: DDB      !! Vertical grid spacing related parameter
   REAL(KIND=R8P) :: DDDLS    !! Depth of loose sediment (m)
   REAL(KIND=R8P) :: DDDLS1   !! Depth of loose sediment at previous time step (m)
   REAL(KIND=R8P) :: DDDSW    !! Depth of surface water (m)
   REAL(KIND=R8P) :: DDDSW1   !! Depth of surface water at previous time step (m)
   REAL(KIND=R8P) :: GGGNU    !! Soil porosity
   REAL(KIND=R8P) :: GGGNU1   !! Soil porosity at previous time step
   REAL(KIND=R8P) :: KSP(LLEE) !! Saturated hydraulic conductivity (m/s)
   REAL(KIND=R8P) :: KSPP(LLEE) !! Saturated hydraulic conductivity at previous time step (m/s)
   REAL(KIND=R8P) :: ZONE     !! Soil zone parameter
   REAL(KIND=R8P) :: ZONE1    !! Soil zone parameter at previous time step

   ! Constant for loose sediment
   REAL(KIND=R8P) :: TTTLSE   !! Loose sediment parameter

   ! Data for lateral flows at surface
   REAL(KIND=R8P) :: QQQSW(4) !! Lateral surface water flow rates (m3/s)
   REAL(KIND=R8P) :: QQQSW1(4) !! Lateral surface water flow rates at previous time step (m3/s)

   ! Water flow data for column
   REAL(KIND=R8P) :: GGAMM(LLEE)  !! Soil water retention parameter
   REAL(KIND=R8P) :: GGAMM1(LLEE) !! Soil water retention parameter at previous time step
   REAL(KIND=R8P) :: PPHI(LLEE)   !! Soil water pressure head (m)
   REAL(KIND=R8P) :: PPHI1(LLEE)  !! Soil water pressure head at previous time step (m)
   REAL(KIND=R8P) :: QQ(LLEE, 4)  !! Inter-cell water fluxes (m/s)
   REAL(KIND=R8P) :: QQ1(LLEE, 4) !! Inter-cell water fluxes at previous time step (m/s)
   REAL(KIND=R8P) :: TTHET(LLEE)  !! Volumetric soil moisture content
   REAL(KIND=R8P) :: TTHET1(LLEE) !! Volumetric soil moisture content at previous time step
   REAL(KIND=R8P) :: UUAJP(LLEE)  !! Adjusted vertical water flux (m/s)
   REAL(KIND=R8P) :: UUAJP1(LLEE) !! Adjusted vertical water flux at previous time step (m/s)

   ! Flows into bottom cell
   REAL(KIND=R8P) :: QQRF     !! Flow into bottom cell (m/s)
   REAL(KIND=R8P) :: QQRF1    !! Flow into bottom cell at previous time step (m/s)

   ! Not part of a common block in the original F77 code
   REAL(KIND=R8P) :: QI       !! Inflow rate (m/s)
   REAL(KIND=R8P) :: QI1      !! Inflow rate at previous time step (m/s)

END MODULE COLM_C2
