!> summary: Defines variables for the sediment/contaminant interface.
!> author: J. Ewen
!> date: 2004-11-01
!>
!> This module contains variables that interface between sediment and contaminant
!> transport calculations. It includes data on sediment depths, infiltration rates,
!> erosion, densities, porosities, and particle flow rates. It was converted
!> from a legacy `INCLUDE` file.
!>
!> @history
!> | Date       | Author | Version | Description                                                              |
!> |:-----------|:-------|:--------|:-------------------------------------------------------------------------|
!> | 1991-04-26 | JE     | 3.0     | Original `INCLUDE` file written.                                         |
!> | 1991-06-16 | JE     | 3.1     | Completed.                                                               |
!> | 1993-02-08 | GP     | 3.4     | Brought QLINK, QDEFF from LINK.CW. Added SEDDIA. Renamed SDPOR to SDEPOR. |
!> | 1994-10-02 | RAH    | 3.4.1   | Added QSED to /SDFLO/; removed redundant SEDDIA from /NUMSED/.            |
!> | 1997-02-20 | RAH    | 4.1     | Separated /SDEPOI/ from mixed-type /SDEPOR/.                             |
!> | 1998-03-08 | RAH    | 4.2     | Removed PSD.                                                             |
!> | 1999-01-27 | SB     | 4.27    | Added DCBED and DCBSED as in 3.4.2.                                      |
!> | 2004-11-01 | JE     | -       | Converted to Fortran 95.                                                 |
!> | 2025-08-11 | AI     | -       | Added FORD docs, modernized declarations.                                |
MODULE sed_cs

   USE SGLOBAL, ONLY : NELEE, NLFEE, NSEDEE, NSEE
   USE MOD_PARAMETERS, ONLY: R8P, I_P
   IMPLICIT NONE

   ! The legacy COMMON block /SDDEP/ was replaced by these module variables.
   REAL(KIND=R8P) :: ARBDEP(NLFEE)         !! Accumulated cross-sectional area of deposited sediments
   REAL(KIND=R8P) :: DLS(NELEE)            !! Depth of loose sediment (mm)
   REAL(KIND=R8P) :: GINFD(NLFEE, NSEDEE)  !! Rate of infiltration of sediments into deep bed layer (kg/m²/s)
   REAL(KIND=R8P) :: GINFS(NLFEE, NSEDEE)  !! Rate of infiltration of sediments into surface bed layer (kg/m²/s)
   REAL(KIND=R8P) :: GNU(NELEE)            !! Rate of ground surface erosion (mm/day)
   REAL(KIND=R8P) :: GNUBK(NLFEE)          !! Rate of lateral bank erosion (mm/day)
   REAL(KIND=R8P) :: DCBED(NLFEE)          !! Depth of sediment in bed top layer (mm)
   REAL(KIND=R8P) :: DCBSED(NLFEE, NSEDEE) !! Depth of sediment in bed top layer by size class (mm)

   ! The legacy COMMON block /SDDEN/ was replaced by this module variable.
   REAL(KIND=R8P) :: FDEL(NELEE, NSEDEE)   !! Relative density of suspended sediments (dimensionless)

   ! The legacy COMMON block /SDFRA/ was replaced by these module variables.
   REAL(KIND=R8P) :: FBETA(NELEE, NSEDEE)  !! Volume fractions of loose sediments by size class (dimensionless)
   REAL(KIND=R8P) :: FBTSD(NLFEE, NSEDEE)  !! Volume fractions of newly deposited sediments by size class (dimensionless)

   ! The legacy COMMON block /SDEPOR/ was replaced by these module variables.
   REAL(KIND=R8P) :: PBSED(NLFEE)          !! Porosity of stream bed sediments (dimensionless)
   REAL(KIND=R8P) :: PLS(NELEE)            !! Porosity of loose surface sediments (dimensionless)
   REAL(KIND=R8P) :: SOSDFN(NSEE, NSEDEE)  !! Fraction of soil in each sediment particle size class (dimensionless)
   REAL(KIND=R8P) :: SOFN(NSEE, NSEDEE)    !! Backup soil fractions used when sediment code is inactive (dimensionless)

   ! The legacy COMMON block /SDEPOI/ was replaced by this module variable.
   INTEGER(KIND=I_P) :: NSOBED(NLFEE)      !! Soil type index for parent material at stream bed

   ! The legacy COMMON block /NUMSED/ was replaced by this module variable.
   INTEGER(KIND=I_P) :: NSED               !! Number of active sediment size classes

   ! The legacy COMMON block /SDFLO/ was replaced by these module variables.
   REAL(KIND=R8P) :: QLINK(NLFEE, 2)       !! Water discharge rates through channel links (m³/s)
   REAL(KIND=R8P) :: QDEFF(NLFEE, 2)       !! Effective water discharge rates for sediment transport (m³/s)
   REAL(KIND=R8P) :: QSED(NELEE, NSEDEE, 4) !! Sediment discharge rates through element faces (kg/s)

END MODULE sed_cs
