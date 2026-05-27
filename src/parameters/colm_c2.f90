!> summary: Column water-flow state arrays.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the second legacy `COLM.C2` common-block include. It
!> stores column water-flow variables used by `COLM`, including capacity terms,
!> numbering/size data, loose-sediment constants, lateral surface flows, water
!> contents, potentials, and previous-step equivalents.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-22 | JE | 3.1 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE COLM_C2
   USE SGLOBAL, ONLY : LLEE
   IMPLICIT NONE
   DOUBLEPRECISION :: ICAP        !! Previous scaled internal contaminant input term.
   DOUBLEPRECISION :: ICAPT       !! Time derivative of the internal contaminant input term.
   DOUBLEPRECISION :: ICAPC       !! Concentration derivative of the internal contaminant input term.
   DOUBLEPRECISION :: QCAP        !! Previous scaled rainfall, well, and imposed-flow input term.
   DOUBLEPRECISION :: QCAPT       !! Time derivative of the rainfall, well, and imposed-flow input term.
   DOUBLEPRECISION :: QCAPC       !! Concentration derivative of the rainfall, well, and imposed-flow input term.
   DOUBLEPRECISION :: DDA         !! Column y-width from `DYQQ(NCL)`.
   DOUBLEPRECISION :: DDB         !! Column x-width from `DXQQ(NCL)`.
   DOUBLEPRECISION :: DDDLS       !! Previous loose-sediment depth for the column.
   DOUBLEPRECISION :: DDDLS1      !! Current loose-sediment depth for the column.
   DOUBLEPRECISION :: DDDSW       !! Previous surface-water depth above the column.
   DOUBLEPRECISION :: DDDSW1      !! Current surface-water depth above the column.
   DOUBLEPRECISION :: GGGNU       !! Previous surface-water exchange coefficient for the column.
   DOUBLEPRECISION :: GGGNU1      !! Current surface-water exchange coefficient for the column.
   DOUBLEPRECISION :: KSP(LLEE)   !! Nondimensional cell thickness, `DELTAZ/Z2`.
   DOUBLEPRECISION :: KSPP(LLEE)  !! Nondimensional node spacing used between cell centres.
   DOUBLEPRECISION :: ZONE        !! Previous nondimensional saturated column depth.
   DOUBLEPRECISION :: ZONE1       !! Current nondimensional saturated column depth.

   DOUBLEPRECISION :: TTTLSE      !! Loose-sediment scaling constant used in surface-cell exchange.

   DOUBLEPRECISION :: QQQSW(4)    !! Previous lateral surface-water flow for each face.
   DOUBLEPRECISION :: QQQSW1(4)   !! Current lateral surface-water flow for each face.

   DOUBLEPRECISION :: GGAMM(LLEE)  !! Previous cell water-change coefficient.
   DOUBLEPRECISION :: GGAMM1(LLEE) !! Current cell water-change coefficient.
   DOUBLEPRECISION :: PPHI(LLEE)   !! Previous mobile-water fraction for each cell.
   DOUBLEPRECISION :: PPHI1(LLEE)  !! Current mobile-water fraction for each cell.
   DOUBLEPRECISION :: QQ(LLEE,4)   !! Previous lateral water flow for each cell face.
   DOUBLEPRECISION :: QQ1(LLEE,4)  !! Current lateral water flow for each cell face.
   DOUBLEPRECISION :: TTHET(LLEE)  !! Previous volumetric water content for each cell.
   DOUBLEPRECISION :: TTHET1(LLEE) !! Current volumetric water content for each cell.
   DOUBLEPRECISION :: UUAJP(LLEE)  !! Previous vertical water flux for each cell interface.
   DOUBLEPRECISION :: UUAJP1(LLEE) !! Current vertical water flux for each cell interface.

   DOUBLEPRECISION :: QQRF        !! Previous flow into the bottom cell.
   DOUBLEPRECISION :: QQRF1       !! Current flow into the bottom cell.
   DOUBLEPRECISION :: QI          !! Previous net rainfall input over the column area.
   DOUBLEPRECISION :: QI1         !! Current net rainfall input over the column area.
!PRIVATE :: LLEE

END MODULE COLM_C2
