MODULE et_validation
!----------------------------------------------------------------------*
!
! Input data validation for evapotranspiration calculations
! Contains ETCHK2 subroutine
!
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE mod_load_filedata, ONLY : ALCHK

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: ETCHK2

CONTAINS

!SSSSSS SUBROUTINE ETCHK2 (PRI, NV, RDL, LDUM1)
   SUBROUTINE ETCHK2 (PRI, NV, RDL, LDUM1)
!----------------------------------------------------------------------*
!
!  Check ET input data
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/ET/ETCHK2/4.2
! Modifications:
! RAH  981103  4.2  New (from OCCHK2).
!----------------------------------------------------------------------*
! Entry requirements:
!  NV.ge.1    PRI open for F output
!----------------------------------------------------------------------*
      INTEGER :: PRI, NV
      DOUBLEPRECISION RDL (NV)
! Workspace arguments
      LOGICAL :: LDUM1 (NV)
! Locals, etc
      INTEGER :: FATAL, ERR
      PARAMETER (FATAL = 1, ERR = 2)
      INTEGER :: IUNDEF, NERR
      DATA NERR / 0 /
!----------------------------------------------------------------------*
! 1. Vegetation Properties
! ------------------------
!RDL

      CALL ALCHK (ERR, 1062, PRI, 1, NV, IUNDEF, IUNDEF, 'RDL(veg)', &
         'EQ', ZERO1, ZERO , RDL, NERR, LDUM1)
! 2. Finish
! ---------
!
      IF (NERR.GT.0) CALL ERROR(FFFATAL, 1000, PRI, 0, 0, 'Error(s) detected while checking ET input data')
   END SUBROUTINE ETCHK2

END MODULE et_validation
