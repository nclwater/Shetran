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
!
! Commons
DOUBLEPRECISION ICAP, ICAPT, ICAPC, QCAP, QCAPT, QCAPC  
DOUBLEPRECISION DDA, DDB, DDDLS, DDDLS1, DDDSW, DDDSW1  
DOUBLEPRECISION GGGNU, GGGNU1, KSP (LLEE), KSPP (LLEE), ZONE, &
 ZONE1
!                             FOR COMPATIBILITY WITH VARIABLE NAMES
!                             IN WRSRU/TR'S
!COMMON / CLBC / ICAP, ICAPT, ICAPC, QCAP, QCAPT, QCAPC  

!COMMON / CLDIM / DDA, DDB, DDDLS, DDDLS1, DDDSW, DDDSW1, GGGNU, &
! GGGNU1, KSP, KSPP, ZONE, ZONE1
!                            NUMBERING AND SIZE DATA FOR COLUMN
DOUBLEPRECISION TTTLSE  

!COMMON / CSEDAT / TTTLSE  
!                            CONSTANT FOR LOOSE SEDIMENT
DOUBLEPRECISION QQQSW (4), QQQSW1 (4)  

!COMMON / CLSURW / QQQSW, QQQSW1  
!                            DATA FOR LATERAL FLOWS AT SURFACE
DOUBLEPRECISION GGAMM (LLEE), GGAMM1 (LLEE), PPHI (LLEE), PPHI1 ( &
 LLEE)
DOUBLEPRECISION QQ (LLEE, 4), QQ1 (LLEE, 4), TTHET (LLEE), &
 TTHET1 (LLEE)
DOUBLEPRECISION UUAJP (LLEE), UUAJP1 (LLEE)  

!COMMON / CLWAT / GGAMM, GGAMM1, PPHI, PPHI1, QQ, QQ1, TTHET, &
 !TTHET1, UUAJP, UUAJP1
!                            WATER FLOW DATA FOR COLUMN
DOUBLEPRECISION QQRF, QQRF1  

!COMMON / STMBKW / QQRF, QQRF1  
!                            FLOWS INTO BOTTOM CELL
DOUBLEPRECISION QI, QI1  
!PRIVATE :: LLEE

END MODULE COLM_C2
