MODULE LINK_CC
!                                         LINK.CC
!
!                      INCLUDE FILE FOR CONTAMINANT VARIABLES USED IN
!                      SUBROUTINE LINK
!
!                                 PROGRAM AMENDMENT HISTORY
!
!                      AMENDED BY  DATE   VERSION   REASON FOR AMENDMENT
!                      ----------  ----   -------   --------------------
!                          JE     20/5/91   3.0     WRITTEN
!                          JE     13/6/91   3.1     COMPLETED
!                          JE     16/7/91   3.1     INCLUDE AL.P AND
!                                                   NCETOP DEFINITION
!                                                   REMOVED; FCPBK1,
!                                                   GCPBK1 REPLACED BY
!                                                   FCPBK AND GCPBK
!                          JE     16/7/91   3.1     CONC CHANGED TO
!                                                   CONCL TO AVOID CLASH
!                                                   OF NAMES
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!-----------------------------------------------------------------------
USE SGLOBAL, ONLY : LLEE
IMPLICIT NONE
DOUBLEPRECISION :: ACPBD1, ACPBDT, ACPBS, ACSFA1(6), ACPSF1, ACPSFT
!COMMON / AREAS / ACPBD1, ACPBDT, ACPBS, ACSFA1 (6), ACPSF1, &
! ACPSFT
!                             CROSS-SECTIONAL AREAS OF LINK CELLS
DOUBLEPRECISION :: CCBD1Q, CCPBK(2,LLEE), CCPGS1(2), CCBS1Q, CCSFA1(6), CCSF1Q, SCPBK(2,LLEE)
!COMMON / CONCL / CCBD1Q, CCPBK (2, LLEE), CCPGS1 (2), CCBS1Q, &
! CCSFA1 (6), CCSF1Q, SCPBK (2, LLEE)
!                             CONCENTRATIONS IN ADJACENT COLUMNS AND
!                             LINKS, AND CONCENTRATIONS OF PARENT
!                             CONTAMINANTS
DOUBLEPRECISION :: WCPBD1, VCPBK1
!COMMON / EROS / WCPBD1, VCPBK1  
!                             EROSION RATES
DOUBLEPRECISION :: ICP1, ICPSBD, ICSBDC, ICSBDT, ICPSBS, ICSBSC, ICSBST
DOUBLEPRECISION :: QCP1
!COMMON / FLX / ICP1, ICPSBD, ICSBDC, ICSBDT, ICPSBS, ICSBSC, &
! ICSBST, QCP1
!                             FLUXES
DOUBLEPRECISION :: FCPBD, FCPBDC, FCBD1Q, FCPBDT, FCPBK(2,LLEE), &
 FCPBS, FCPBSC, FCBS1Q, FCPBST, FCPSW1 (2), FCPSD, FCPSDC, FCPSDT, &
 FCPSF, FCSFA1(6), FCPSFC, FCSF1Q, FCPSFT, GCPBK(2,LLEE)
!COMMON / FRETS / FCPBD, FCPBDC, FCBD1Q, FCPBDT, FCPBK (2, LLEE), &
! FCPBS, FCPBSC, FCBS1Q, FCPBST, FCPSW1 (2), FCPSD, FCPSDC, FCPSDT, &
! FCPSF, FCSFA1 (6), FCPSFC, FCSF1Q, FCPSFT, GCPBK (2, LLEE)
!                             RETARDATION VARIABLES
INTEGER :: NCEBK(2)
!COMMON / NUMB / NCEBK (2)  
!                             NODE NUMBERS IN THE STREAM BANKS
DOUBLEPRECISION :: PCPBK1(2,LLEE), PCPSB1(2), PCSFA1(6), PCPSW1(2), PCSFM1, PCSFP1
!COMMON / PECLE / PCPBK1 (2, LLEE), PCPSB1 (2), PCSFA1 (6), &
! PCPSW1 (2), PCSFM1, PCSFP1
!                             PECLET NUMBERS
DOUBLEPRECISION ::ECPBD, ECPBDC, ECPBDT, ECPBS, ECPBSC, ECPBST, ECPSF, ECPSFC, ECPSFT
!COMMON / SPLANT / ECPBD, ECPBDC, ECPBDT, ECPBS, ECPBSC, ECPBST, &
! ECPSF, ECPSFC, ECPSFT
!                             PLANT UPTAKE RATES
DOUBLEPRECISION :: USCP
!COMMON / SWITCH / USCP  
!                             SWITCH FOR PRESENCE OR OTHERWISE
!                             OF STREAM WATER
DOUBLEPRECISION ::ACSBD1, ACSBS1, GCPLAL, GCPLAQ  
!COMMON / TRAN / ACSBD1, ACSBS1, GCPLAL, GCPLAQ  
!                             TRANSFER COEFFICIENTS AND DECAY CONTANTS
!PRIVATE :: LLEE
END MODULE LINK_CC
