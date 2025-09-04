MODULE subsurface_utilities
!----------------------------------------------------------------------*
! Utility functions and mass balance calculations
! Contains VSMB, fncell - utility functions
!----------------------------------------------------------------------*
! Extracted from VSmod.f90 as part of refactoring
! Date: 2025-09-04
! Source: VSmod.f90.sav (lines 4365-4511)
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE subsurface_variables
   USE AL_G, ONLY : NX, NY, ICMXY, ICMREF
   USE AL_C, ONLY : deltaz, QVSH, jvsacn, QVSV, QVSWLI, VSTHE, QVSSPR, QVSBF, &
      DTUZ, NLYRBT, LINKNS, NVSWLI, ERUZ, ESOILA, JVSDEL
   IMPLICIT NONE

   PRIVATE
   ! Public utility routines
   PUBLIC :: VSMB, fncell

CONTAINS

!SSSSSS SUBROUTINE VSMB (VSTHEN)
   SUBROUTINE VSMB (VSTHEN)
!
!----------------------------------------------------------------------*
! Updates flows to ensure mass conservation
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSMB/4.1
! Modifications:
!  GP  08.03.95  written (v4.0 finished 17.07.96)
! RAH  961228  4.1  Remove variable ILINK.  No leading comments.
!      970214       Reverse DELTAZ,QVSH indices (AL.C). Declare JCL,JFA.
!                   mv VSTHEN from VSCOM1.INC to arg list, reverse subs.
!      970118       Swap subscripts: JVSACN,QVSV,QVSWLI,VSTHE (AL.C);
!                   also fix error in QVSWLI index: use IW not IEL.
!                   Remove temporary code (to set VSSTMP).  DBLE locals.
!                   Don't include VSCOM1.INC.
!      970509       Scrap output QVSBF (set in VSSIM).  Order labels.
!                   Remove redundant local BDONE.  Trap JVSDEL.ne.0.
!----------------------------------------------------------------------*
! Commons and distributed constants
! Imported constants
!     AL.P.VSS:        LLEE,NELEE
! Input common
!     AL.C:            LL,  NLYRBT(NEL,1),JVSACN(4,LLEE,NEL)
!                           NVSWLI(NEL),  JVSDEL(4,LLEE,NEL)
!                             AREA(NEL),    DELTAZ(LLEE,NEL)
!                           LINKNS(*)
!                      DTUZ,ESOILA(NEL),      ERUZ(NELEE,LL)
!                       VSTHE(LLEE,NEL), QVSV(LLEE,NEL),QVSWLI(LLEE,*)
!     AL.G:            NEL, ICMREF(NELEE,12)
! In+out common
!     AL.C:            QVSH(4,LLEE,NEL)
! Input arguments

      DOUBLEPRECISION VSTHEN (LLEE, total_no_elements)
! Locals, etc
      INTEGER :: NFACES, IFACES (4)
      INTEGER :: IEL, J, ITYPE, IFA, JEL, ICL, JFA, JCL, IW, MCL
      DOUBLEPRECISION AREAE, CMBE, F, Qasum
      LOGICAL :: iscycle
!----------------------------------------------------------------------*
! --- loop over all elements
      iscycle=.FALSE.
      DO 2900 IEL = 1, total_no_elements
         IF(iscycle) CYCLE
         ITYPE = ICMREF (IEL, 1)
         ! Choose faces to adjust (ie set NFACES and IFACES)
         ! grids - do nothing!
         IF (ITYPE.EQ.0) THEN
            NFACES = 0
            ! banks - update only 'outer' face adjacent to grid (if there is one)
         ELSEIF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
            NFACES = 0
            DO 920 IFA = 1, 4
               IF(iscycle) CYCLE
               JEL = ICMREF (IEL, IFA + 4)
               IF (JEL.GT.0) THEN
                  IF (ICMREF (JEL, 1) .EQ.0) THEN
                     IFACES (1) = IFA
                     NFACES = 1
                     iscycle = .TRUE. !GOTO 930  !                       >>>>>>>>
                  ENDIF
               ENDIF
920         ENDDO
            iscycle=.FALSE.! 930 CONTINUE
            ! links - update faces adjacent to banks only
         ELSE
            NFACES = 2
            IF (LINKNS (IEL) ) THEN
               IFACES (1) = 1
               IFACES (2) = 3
            ELSE
               IFACES (1) = 2
               IFACES (2) = 4
            ENDIF
         ENDIF
         ! Loop over column cells if required (top to bottom for QVSV's benefit)
         IF (NFACES.GT.0) THEN
            IW = NVSWLI (IEL)
            AREAE = cellarea (IEL)
            DO 990 ICL = top_cell_no, NLYRBT (IEL, 1), - 1
               ! calculate mass balance error (m**3/s)
               MCL = ICL - 1
               CMBE = - QVSV (MCL, IEL) + QVSV (ICL, IEL) + ERUZ (IEL, &
                  ICL) + DELTAZ (ICL, IEL) * (VSTHE (ICL, IEL) - VSTHEN ( &
                  ICL, IEL) ) / DTUZ
               IF (IW.GT.0) CMBE = CMBE+QVSWLI (ICL, IW)
               IF (ICL.EQ.top_cell_no) CMBE = CMBE+ESOILA (IEL)
               CMBE = CMBE * AREAE
               DO 950 IFA = 1, 4
                  CMBE = CMBE-QVSH (IFA, ICL, IEL)
950            ENDDO
               ! adjust lateral flows (unless Qasum=0)
               Qasum = zero
               DO 955 J = 1, NFACES
                  IFA = IFACES (J)
                  Qasum = Qasum + QVSH (IFA, ICL, IEL)
955            ENDDO
               IF (NOTZERO(Qasum)) THEN
                  F = one + CMBE / Qasum
                  DO 960 J = 1, NFACES
                     IFA = IFACES (J)
                     QVSH (IFA, ICL, IEL) = QVSH (IFA, ICL, IEL) * F
960               ENDDO
               ENDIF
990         ENDDO
         ENDIF
         ! Update flows for adjacent element
         DO 2800 IFA = 1, 4
            IF(iscycle) CYCLE
            JEL = ICMREF (IEL, IFA + 4)
            IF (JEL.GT.0) THEN
               JFA = ICMREF (IEL, IFA + 8)
               DO 1820 ICL = NLYRBT (IEL, 1), top_cell_no
                  IF(iscycle) CYCLE
                  !970509            (catch JEL next time around)
                  IF (JVSDEL (IFA, ICL, IEL) .NE.0) THEN
                     iscycle=.TRUE.  !GOTO 8820
                     CYCLE
                  ENDIF
                  JCL = JVSACN (IFA, ICL, IEL)
                  IF (JCL.GT.0) QVSH (JFA, JCL, JEL) = - QVSH (IFA, ICL, IEL)
1820           ENDDO
            ENDIF
2800     END DO
2900  END DO
      IF(.NOT.iscycle) RETURN
8820  STOP 'UNFINISHED CODE FOR SPLIT CELLS IN SUBROUTINE VSMB!'
   END SUBROUTINE VSMB

!FFFFFF INTEGER FUNCTION fncell
   INTEGER FUNCTION fncell(I, IEL, ITOP)
      INTEGER, INTENT(IN) :: I, IEL, ITOP
      fncell = IDIMJE(MIN(NLYRBT (IEL, I + 1), ITOP + 1), NLYRBT (IEL, I) )
   END FUNCTION fncell


END MODULE
