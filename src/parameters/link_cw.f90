!> summary: Defines water-related variables for preparing to run the LINK subroutine.
!> author: J. Ewen
!> date: 2008-08-12
!>
!> This module contains variables used for water flow calculations in the
!> channel network, specifically for the LINK subroutine. It includes
!> cross-sectional areas, bed layer thicknesses, and pointers for link
!> connectivity. It was converted from a legacy `INCLUDE` file.
!>
!> @history
!> | Date       | Author | Version  | Description                                                              |
!> |:-----------|:-------|:---------|:-------------------------------------------------------------------------|
!> | 1991-05-20 | JE     | 3.0      | Original `INCLUDE` file written.                                         |
!> | 1991-06-13 | JE     | 3.1      | Completed.                                                               |
!> | 1991-06-16 | JE     | 3.1      | Added ACPBSG, ACPSFO, OLDB, QSTRM; Renamed DBDM to DBDI.                   |
!> | 1991-06-18 | JE     | 3.1      | LINK.CC1 included.                                                       |
!> | 1991-08-26 | JE     | 3.1      | BLOCK OLOL removed.                                                      |
!> | 1993-02-08 | GP     | 3.4      | Moved QLINK, QDEFF to SED.CS.                                            |
!> | 1997-03-13 | RAH    | 4.1      | Explicit typing.                                                         |
!> | 1998-03-08 | RAH    | 4.2      | Amended comment.                                                         |
!> | 2008-08-12 | JE     | 4.3.5F90 | Converted to Fortran 90 module.                                          |
!> | 2025-08-11 | AI     | -        | Added FORD-compliant documentation and modernized declarations.          |
MODULE LINK_CW

   USE SGLOBAL, ONLY : NLFEE
   USE LINK_CC1
   USE MOD_PARAMETERS, ONLY: R8P, I_P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: ACPBDO, ACPBSG, ACPBI, ACPSFO
   PUBLIC :: DBS, DBDI
   PUBLIC :: LENDA
   PUBLIC :: THBED, THBEDO

   ! Code =====================================================================

   ! The legacy COMMON block /AREAO/ was replaced by these module variables.
   ! It contained cross-sectional areas.
   REAL(KIND=R8P) :: ACPBDO(NLFEE) !! Cross-sectional area of bed deep layer old.
   REAL(KIND=R8P) :: ACPBSG(NLFEE) !! Cross-sectional area of bed surface layer gross.
   REAL(KIND=R8P) :: ACPBI(NLFEE)  !! Cross-sectional area of bed deep layer immobile.
   REAL(KIND=R8P) :: ACPSFO(NLFEE) !! Cross-sectional area of bed surface layer fine old.

   ! The legacy COMMON block /DBED/ was replaced by these module variables.
   REAL(KIND=R8P) :: DBS         !! Thickness of bed surface layer.
   REAL(KIND=R8P) :: DBDI        !! Minimum allowable thickness of the combined bed surface and deep layers.

   ! The legacy COMMON block /POINT/ was replaced by this module variable.
   INTEGER(KIND=I_P) :: LENDA(6)  !! Pointers for the number for the end of the links which can be attached to a given link.

   REAL(KIND=R8P) :: THBED(NLFEE)  !! Thickness of the bed.
   REAL(KIND=R8P) :: THBEDO(NLFEE) !! Old thickness of the bed.

END MODULE LINK_CW
