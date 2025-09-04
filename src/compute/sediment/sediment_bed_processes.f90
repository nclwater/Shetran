MODULE sediment_bed_processes
   USE SGLOBAL
   USE sediment_common
   USE CONST_SY
   USE sediment_transport_capacity, ONLY : SYCRIT
   USE UTILSMOD

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: SYBED, SYFINE

CONTAINS

!SSSSSS SUBROUTINE SYBED (DCBEDO, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, &
   SUBROUTINE SYBED (DCBEDO, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, &
      DDIPRM, ARBDEP, DLS, FBETA, DCBSED, DDBSED, DCBED)
!
!----------------------------------------------------------------------*
!
! Update stream-bed state variables for each link.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR65
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  RAH  23.5.94  Version 3.4.1 by AB/RAH. File creation date 5.4.94.
!----------------------------------------------------------------------*
! Input arguments
      INTEGER :: NELEE, NLF, NLFEE, NSED
      DOUBLEPRECISION DCBEDO, CWIDTH (NLF)
      DOUBLEPRECISION DCIPRM (NLFEE, NSED), DDIPRM (NLFEE, NSED)

!
! Input/output arguments
      DOUBLEPRECISION ARBDEP (NLF), DLS (NLF), FBETA (NELEE, NSED)
!
! Output arguments
      DOUBLEPRECISION DCBSED (NLFEE, NSED), DDBSED (NLFEE, NSED), &
         DCBED (NLF)
!
! Locals, etc
      DOUBLEPRECISION AC, AD, DCBEDZ, DCC, DCNEW, DDBEDZ, DLSNEW, &
         DLSOLD
      DOUBLEPRECISION DCIPP, DDIPP, DCINEW, SUMSED
      INTEGER :: LINK, SED
!
!
!----------------------------------------------------------------------*
!
!
!     * Loop over links
      DO 300 LINK = 1, NLF
!
!
!        * Calculate interim bed layer thicknesses
         DCBEDZ = 0
         DDBEDZ = 0
         DO 100 SED = 1, NSED
            DCBEDZ = DCBEDZ + DCIPRM (LINK, SED)
            DDBEDZ = DDBEDZ + DDIPRM (LINK, SED)
100      END DO
!
!        * Reset variables that are independent of size group
         DLSOLD = DLS (LINK)
         DLSNEW = DCBEDZ + DDBEDZ
         DLS (LINK) = DLSNEW
         ARBDEP (LINK) = ARBDEP (LINK) + CWIDTH (LINK) * (DLSNEW - &
            DLSOLD)
         DCNEW = MIN (DLSNEW, DCBEDO)
         DCBED (LINK) = DCNEW
!
!        * What fraction of the interim top layer remains in the top
!        *  layer, and what fraction of the interim bottom layer becomes
!        *  part of the top?
         DCC = MIN (DCBEDZ, DCNEW)
         AC = 0
         AD = 0
         IF (DCBEDZ.GT.0) AC = DCC / DCBEDZ
         IF (DDBEDZ.GT.0) AD = (DCNEW - DCC) / DDBEDZ
!
!        * Loop over sediment size groups
         DO 200 SED = 1, NSED
!
!           * Interim layer depths
            DCIPP = DCIPRM (LINK, SED)
            DDIPP = DDIPRM (LINK, SED)
!
!           * Total depth (for this size group)
            SUMSED = DCIPP + DDIPP
!
!           * New top layer depth
            DCINEW = AC * DCIPP + AD * DDIPP
            DCBSED (LINK, SED) = DCINEW
!
!           * New bottom layer depth
            DDBSED (LINK, SED) = SUMSED-DCINEW
!
!           * Composition of both layers together
            IF (DLSNEW.GT.0) FBETA (LINK, SED) = (SUMSED / DLSNEW)
!
!        * Next sediment type
200      END DO
!
!
!     * Next link
300   END DO
!
!
   END SUBROUTINE SYBED
!SSSSSS SUBROUTINE SYFINE (DRSEDF, FBIC, FICRIT, NLF, ALPHA, DTSY, AREA, &
   SUBROUTINE SYFINE (DRSEDF, FBIC, FICRIT, NLF, ALPHA, DTSY, AREA, &
      DCBF, FBETAF, FDELF, PBSED, TAUK, VCFMAX, VINFMX, BARM)
!
!----------------------------------------------------------------------*
!
! Evaluate quantities specific to fine sediment particles, associated
!  with settling, infiltration and armouring.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR70
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  RAH  15.07.94  Version 3.4.1 by AB/RAH. File created 28.3.94.
!----------------------------------------------------------------------*
      USE CONST_SY
!
! Commons and distributed constants
!
! Constants referenced
!     CONST.SY:  GRAVTY, RHOSED, RHOWAT, VISCOS
!
! Input arguments
      INTEGER :: NLF
      DOUBLEPRECISION DRSEDF, FBIC, FICRIT, ALPHA, DTSY, AREA (NLF), &
         DCBF (NLF)
      DOUBLEPRECISION PBSED (NLF), FBETAF (NLF), FDELF (NLF), TAUK (NLF)
!
! Output arguments
      DOUBLEPRECISION VCFMAX (NLF), VINFMX (NLF)
      LOGICAL :: BARM (NLF)
!
! Locals, etc
      DOUBLEPRECISION DUM, TAUEC, VMAX
      DOUBLEPRECISION ADOUBLEPRECISION, DCFMXL, FDELFL, TAUKL
      INTEGER :: LINK
!
!----------------------------------------------------------------------*
!
!
!     * Calculate settling velocity for fines ( first call only )
      IF (FIRST_syfine) THEN
         FIRST_syfine = .FALSE.
         WSED_syfine = DRSEDF**2 * GRAVTY * (RHOSED-RHOWAT) / (18 * RHOWAT * &
            VISCOS)
      ENDIF
!
!     * Loop over channel links
      DO 100 LINK = 1, NLF
!
         TAUKL = TAUK (LINK)
         ADOUBLEPRECISION = AREA (LINK)
         FDELFL = FDELF (LINK)
!
!        * Calculate critical shear stress for fines
         CALL SYCRIT (0, DRSEDF, TAUKL, DUM, TAUEC)
!
!        * Calculate potential fines in upper layer
!        *  (existing fines + settling)
         DUM = ALPHA * TAUEC
         IF (DUM.GT.0) DUM = DIMJE(DUM, TAUKL) / DUM
         DCFMXL = DCBF (LINK) + FDELFL * WSED_syfine * DUM * DTSY
         VCFMAX (LINK) = DCFMXL * ADOUBLEPRECISION
!
!        * Can fines be armoured ?
         BARM (LINK) = TAUKL.LE.TAUEC
!
!        * Calculate potential infiltration rate
         VMAX = 0
         IF (FBETAF (LINK) .LT.FBIC) VMAX = WSED_syfine * ADOUBLEPRECISION * DIMJE(FDELFL, &
            FICRIT / (1 - PBSED (LINK) ) ) * DTSY
         VINFMX (LINK) = VMAX
!
!     * End of link loop
100   END DO
!
   END SUBROUTINE SYFINE

END MODULE sediment_bed_processes
