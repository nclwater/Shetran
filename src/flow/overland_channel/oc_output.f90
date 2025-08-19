MODULE oc_output
! Output routines for overland channel calculations
! Contains OCPRI and other output-related subroutines
! Extracted from OCmod.f90

   USE SGLOBAL
   USE AL_C ,     ONLY : ARXL, QOC
   USE OCmod2,    ONLY : GETHRF
   USE oc_common_data

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: OCPRI

CONTAINS

   SUBROUTINE OCPRI (OCNOW, ARXL, QOC)
!----------------------------------------------------------------------*
!
!  Print results of OC simulation
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCPRI/4.2
! Modifications:
! RAH  980226  4.2  New.
!----------------------------------------------------------------------*
! Entry requirements:
!  NELEE.ge.NEL    NEL.ge.[1,NLF]    NLF.ge.0    NLF.le.size_of_ARXL
!  PRI.ge.0        PRI open for F output
!----------------------------------------------------------------------*
      DOUBLEPRECISION, INTENT(IN) :: OCNOW, ARXL(:), QOC(:,:)
      DOUBLEPRECISION             :: ghrf(total_no_links)
      INTEGER                     :: FACE, ielmm
!----------------------------------------------------------------------*
      WRITE(PPPRI, 9100) 'AFTER', OCNOW, ' HOURS ----'
      WRITE(PPPRI, 9200) 'iel', ('QOC(iel,', FACE, ')', FACE = 1, 4) , 'HRF', 'ARXL'
      DO ielmm=1,total_no_links
         ghrf(ielmm) = GETHRF(ielmm)
      ENDDO
      WRITE(PPPRI, 9210) (ielmm, (QOC(ielmm,FACE), FACE=1,4), ghrf(ielmm), ARXL (ielmm), ielmm = 1, total_no_links)
      DO ielmm = total_no_links + 1,total_no_elements
         WRITE(PPPRI, 9210) ielmm, (QOC (ielmm, FACE), FACE = 1, 4), GETHRF (ielmm)
      END DO

      WRITE(PPPRI, 9100) 'END ----'
9100  FORMAT (//'---- OC MODULE  RESULTS ',A:F10.2,A//)
9200  FORMAT (4X,A4,4(2X,A8,I1,A1),2A12/)

9210  FORMAT (4X,I4,SP,4F12.3,S,2F12.3)
   END SUBROUTINE OCPRI

END MODULE oc_output
