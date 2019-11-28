MODULE COLM_C2
!---------------------------- Start of COLM.C2 ------------------------*
!
!                      SECOND INCLUDE FILE FOR WATER VARIABLES USED IN
!                      SUBROUTINE COLM
!
!----------------------------------------------------------------------*
! Version:  /SHETRAN/INCLUDE/COLM.C2/4.1
! Modifications:
!                          JE     22/4/91   3.1     WRITTEN
!                          JE     13/6/91   3.1     COMPLETED
! RAH  970313  4.1  Explicit typing.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Other commons and constants
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
