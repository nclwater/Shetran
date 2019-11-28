MODULE COLM_C1
!---------------------------- Start of COLM.C1 ------------------------*
!
!                      INCLUDE FILE FOR WATER VARIABLES USED IN
!                      SUBROUTINE COLM
!                      NB: THIS FILE 'INCLUDES' AL.P, SO CARRIES
!                      THE EFFECTS OF THE PARAMETER AND IMPLICIT
!                      STATEMENTS SET UP IN THE WATER FLOW CODE THROUGH
!                      INTO THE CONTAMINANT CODE FOR COLUMNS
!
!----------------------------------------------------------------------*
! Version:  /SHETRAN/INCLUDE/COLM.C1/4.1
! Modifications:
!                          JE     26/4/91   3.1     WRITTEN
!                          JE     13/6/91   3.1     COMPLETED
!                          JE     16/6/91   3.1     REFERENCES TO LNCONT
!                                                   AND LNSOIL REMOVED
!                          JE     17/7/91   3.1     REORDERED NAMES IN
!                                                   CLNUM
!                          JE     26/8/91   3.1     MOVED PARAMETER
!                                                   NCETOP TO BLOCKCLNUM
! RAH  970313  4.1  Explicit typing.  Split mixed-type /CLNUM/.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Imported constants
!USE SGLOBAL
IMPLICIT NONE
!
! Commons
DOUBLEPRECISION D0, Z2, Z2SQ, Z2OD, Z2SQOD  

!COMMON / CLREF / D0, Z2, Z2SQ, Z2OD, Z2SQOD  
!                            SCALE REFERENCES
DOUBLEPRECISION CST1, CST2, CST3, SGMA, SGSQ, SGTSE, SGSTSE, &
 OMSGMA
DOUBLEPRECISION OPSGL, OPSGSL, TSE  

!COMMON / FD / CST1, CST2, CST3, SGMA, SGSQ, SGTSE, SGSTSE, OMSGMA, &
! OPSGL, OPSGSL, TSE
!                             FINITE DIFFERENCE AND SCALING CONSTANTS
DOUBLEPRECISION FNCPSF  

!COMMON / CLNUM / FNCPSF  
!                             CELL FRACTION FOR HIGHEST CELL BELOW
!                             PHREATIC SURFACE
INTEGER :: NCEBOT, NCETOP, NCEPSF  

END MODULE COLM_C1
