MODULE water_balance
! Extracted from rest.f90 - Water balance calculation functionality
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactored: 2025-08-22 - Split from rest module for better organization

   USE SGLOBAL
   USE AL_G,    ONLY : icmref
   USE AL_C,    ONLY : ARXL, CWIDTH, DELTAZ, DTUZ, EEVAP, ERUZ, &
      NLYRBT, PNETTO, QVSBF, QVSWEL, QBKF, QOC, QVSH, VSTHE, WBERR
   USE mod_load_filedata, ONLY : ALINIT
   USE OCmod2,  ONLY : GETHRF

   IMPLICIT NONE

   LOGICAL :: FIRST_balwat=.TRUE.
   DOUBLEPRECISION :: STORW_balwat(NELEE)=zero

   PRIVATE
   PUBLIC :: BALWAT

CONTAINS

!SSSSSS SUBROUTINE BALWAT
   SUBROUTINE BALWAT
!----------------------------------------------------------------------*
!           Returns WBERR(column or link no.)
!           the balance error for water depth. This is the
!           extra depth, in metres, of water created during the
!           timestep.
!----------------------------------------------------------------------*
! Version:  SHETRAN/FR/BALWAT/4.1
! Modifications:
!  RAH  03.10.94  Version 3.4.1 from version 3.4 Aug 94: std header;
!                  declare everything; extra comments.
!                 Initialize STORW; set WBERR=0 on first pass.
!  GP  20.02.95  updated for SHETRAN V4.0 (finished 15/1/96)
!                   Mods for new VSS module: one loop for all elements;
!                   scrap AMULT,JBK,NLINKA,NLKSA; asumQ for advection;
!                   replace DDZ,TH3,RSZAQ,RSZWEL,QHSZ with DELTAZ,
!                   VSTHE,QVSBF,QVSWEL,QVSH (note change in sign); QBK*
!                   implicit in QVSH (except QBKF for link elements).
! RAH  970217  4.1  Swap subscripts: QVSH,DELTAZ,VSTHE (see AL.C).
!      970606       Rename locals NCE,NCL as CELL,IEL.
! Refactored: 2025-08-22 - Split from rest module for better organization
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P:            LLEE,NELEE,NLFEE
! Input common
!     AL.C:            LL,NLYRBT(NEL,1),AREA(NEL),CWIDTH(NLFEE)
!                      DELTAZ(LLEE,NEL),ZGRUND(NLF+1,NEL)
!                      DTUZ,ARXL(NLFEE),EEVAP(NEL),ERUZ(NELEE,LLEE)
!                      HRF(NLF+1:NEL),PNETTO(NEL),QBKF(NLFEE,2)
!                      QOC(NELEE,4),QVSBF(NEL),QVSH(4,LLEE,NEL)
!                      QVSWEL(NEL),VSTHE(LLEE,NEL),
!     AL.G:            NEL,ICMREF(NEL,1)
! In+out common
!     AL.C:            WBERR(NEL)
      DOUBLEPRECISION DELSTO, DEPTHI, DEPTHS, asum, asumQ
      INTEGER :: ITYPE, JDUM, CELL, IEL

!----------------------------------------------------------------------*
! Initialization
! --------------

      IF (FIRST_balwat) CALL ALINIT (ZERO, total_no_elements, WBERR)
! Loop Over Columns
! -----------------
      DO IEL = 1, total_no_elements
         ITYPE = ICMREF (IEL, 1)
!        Calculate depth of water stored and change since previous step
!        --------------------------------------------------------------
!        * surface
         IF (ITYPE.EQ.3) THEN
            asum = ARXL (IEL) / CWIDTH (IEL)
         ELSE
            asum = GETHRF (IEL) - ZGRUND (IEL)

         ENDIF
!        * sub-surface
         DO CELL = NLYRBT (IEL, 1), top_cell_no
            asum = asum + DELTAZ (CELL, IEL) * VSTHE (CELL, IEL)
         END DO

         DEPTHS = asum
!        * net increase this timestep

         DELSTO = DEPTHS - STORW_balwat (IEL)
!        * save new value for use next timestep



         STORW_balwat (IEL) = DEPTHS
!        Calculate net depth of water supplied over the previous step
!        ------------------------------------------------------------
!        * ... but only if we have a bona fide value for DELSTO

         IF (.not. FIRST_balwat) THEN
!                     >>>>>>>>
!        * sources and sinks
            asum = PNETTO (IEL) - EEVAP (IEL) + QVSBF (IEL) - QVSWEL (IEL)
            DO CELL = NLYRBT (IEL, 1), top_cell_no
               asum = asum - ERUZ (IEL, CELL)

            END DO
!        * advection
            IF (ITYPE.EQ.3) THEN
               asumQ = - QBKF (IEL, 1) - QBKF (IEL, 2)
            ELSE
               asumQ = zero
            ENDIF
            DO JDUM = 1, 2
               asumQ = asumQ - QOC (IEL, JDUM) + QOC (IEL, JDUM + 2)
               DO CELL = NLYRBT (IEL, 1), top_cell_no
                  asumQ = asumQ + QVSH (JDUM, CELL, IEL) + QVSH (JDUM + 2, &
                     CELL, IEL)
               END DO
            END DO

            asum = asum + asumQ / cellarea (IEL)
!        * convert from rate to depth


            DEPTHI = asum * DTUZ
!        Update the cumulative water balance error as a depth
!        ----------------------------------------------------

            WBERR (IEL) = WBERR (IEL) + DELSTO - DEPTHI

         ENDIF

      END DO
! Epilogue
! --------
      FIRST_balwat = .FALSE.
   END subroutine BALWAT

END MODULE water_balance
