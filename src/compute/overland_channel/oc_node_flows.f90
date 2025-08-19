MODULE oc_node_flows
! Module for node flow calculations in overland channel networks
! Handles flow calculations at confluences and junctions
! Extracted from OCmod2.f90 as part of refactoring

   USE SGLOBAL
   USE oc_parameters
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: OCNODE, FNODE

CONTAINS

!SSSSSS SUBROUTINE OCNODE
   SUBROUTINE OCNODE (iela, ZI, CI, DI, ROOTLI, QJ)
!----------------------------------------------------------------------*
! CALCULATES FLOWS OUT OF NODE AS FUNCTION OF ADJACENT WATER ELEVATIONS
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCNODE/4.27
! Modifications:
!  GP          3.4  Call ERROR & terminate iterations if nc.eq.50.
!                   Add argument ZNODE (see OCQMLN).
! RAH  980212  4.2  Supply missing PRI,FATAL,WARN, to pass to ERROR;
!                   also remove argument ZNODE (see OCQMLN).
!                   Explicit typing.  Locals: scrap TESTZ; add TEST.
!                   Generic intrinsics.  Description: OUT OF, not INTO.
!                   RETURN immediately if FA=0 (don't overwrite QJ).
!                   Test NC BEFORE updating A or B.
!                   Set QJ at absent branches (was in OCQMLN).
!      980220       Add argument IEL to pass to ERROR (see OCQMLN).
!      980318       Add argument DI to pass to FNODE (see OCQMLN).
! SB   990204 4.27  Problem with conserving mass at junctions
!                   Adjust QJ so the largest absolute value at a
!                   junction is modified so that the asum = 0
!----------------------------------------------------------------------*

      INTEGER, INTENT(IN)         :: IELa
      DOUBLEPRECISION, INTENT(IN) :: CI (0:3), DI (0:3), ZI (0:3), ROOTLI (0:3)
! NB:
!         ROOTLI(J)   is zero for any absent branches J.gt.0
!                     Note: branch J=0 is never absent.
!   DI(J),CI(J),ZI(J) are undefined for absent branches
      DOUBLEPRECISION, INTENT(OUT) :: QJ (0:3)
      INTEGER                      :: J, NC
      DOUBLEPRECISION              :: A, B, FA, FB, FN, FNM1, SIGMAQ, WN
      LOGICAL                      :: TEST
!^^^^RAH/SB 4/2/99 CONSERVE MASS AT JUNCTIONS ^^^^^^^^^^^^^
      INTEGER                      :: JMAJOR
      LOGICAL :: iscycle, failed
!----------------------------------------------------------------------*
!
! FIRST GUESSES (CHOOSE VALUES A,B SUCH THAT F(A)*F(B) .le. 0 )
! (USE MIN AND MAX OF VALID ELEVATIONS); also, set QJ at absent branches
!
      A = ZI (0)
      B = A
      DO J = 1, 3
         IF(ISZERO(ROOTLI(J))) THEN
            QJ (J) = zero
         ELSE
            A = MIN (ZI (J), A)
            B = MAX (ZI (J), B)
         ENDIF
      ENDDO
      CALL FNODE(A, DI, CI, ZI, ROOTLI, QJ, FA)
      IF (ISZERO(FA)) RETURN
      CALL FNODE(B, DI, CI, ZI, ROOTLI, QJ, FB)
      IF (ISZERO(FB)) RETURN
!
! Iterate to convergence, using successive linear interpolation
!
      FN = FA
      NC = 0
      failed =.FALSE.
      iscycle=.FALSE.
!     * Start of iteration loop: set new point WN and calculate FN
      DO nc=1,50
         IF(iscycle) CYCLE
         WN   = (A*FB - B*FA) / (FB-FA)
         FNM1 = FN
         CALL FNODE(WN, DI, CI, ZI, ROOTLI, QJ, FN)
         IF (ISZERO(FN)) THEN   !        * Test for convergence (either exact or approximate)
            failed =.TRUE.
            iscycle=.TRUE.
            CYCLE
         ENDIF
         SIGMAQ = ABS(QJ(0) ) + ABS(QJ(1) ) + ABS(QJ(2) ) + ABS(QJ(3) )
         IF (ABS(FN) .LE. SIGMAQ*1.0D-2 .AND. ABS(B-A) .LE. 1.0D-3) THEN
            JMAJOR = 0
            DO J = 1, 3
               IF (ABS(QJ(J)) .GT. ABS(QJ(JMAJOR))) JMAJOR = J
            ENDDO
            QJ(JMAJOR) = QJ(JMAJOR) - FN
            iscycle=.TRUE.
            CYCLE
         ENDIF
         !            * ... carry on: replace either A or B with WN; and
         !            * adjust interpolation factor if sign of F didn't change
         TEST = GTZERO(FN * FNM1)  !TAKE CARE - PRECEDENCE
         IF (FN * FA.GE.0D0) THEN
            A = WN
            FA = FN
            IF (TEST) FB = FB * half
         ELSE
            B = WN
            FB = FN
            IF (TEST) FA = FA * half
         ENDIF
      ENDDO
      IF(failed) THEN
         IF (ABS (FN) .LT.SIGMAQ * 1D-1.AND.ABS (B - A) .LT.5D-2) THEN
            !CALL ERROR(WWWARN, 1027, PPPRI, iela, 0, 'maximum iterations exceeded for OC confluence')
         ELSE
            !CALL ERROR(FFFATAL, 1028, PPPRI, iela, 0, 'iteration failure for OC confluence')
         ENDIF
      ENDIF
   END SUBROUTINE OCNODE

!SSSSSS SUBROUTINE FNODE
   SUBROUTINE FNODE (ZNODE, DI, CI, ZI, ROOTLI, QJ, resfnode)
!----------------------------------------------------------------------*
! CALCULATES THE FUNCTION FNODE(ZNODE) = sum OF FLOWS OUT OF A NODE
! NB. ROOTLI IS USED AS A FLAG FOR NON-EXISTENT LINKS
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/FNODE/4.2
! Modifications:
! RAH  980211  4.2  Explicit typing.
!                   Generic intrinsics.  Replace ci2 array with CMAX.
!                   Integer DELTA.  New locals CJ,DJ,Q,Qasum.
!      980212       Scrap local array CI3.  Extend IF block in loop 100.
!      980318       Set downstream CJ using ZNODE (had (CI(J)+CMAX)/2);
!                   add argument DI (see OCNODE).
!----------------------------------------------------------------------*
      DOUBLEPRECISION, INTENT(IN) ::  ZNODE, DI (0:3), CI (0:3), ZI (0:3), ROOTLI (0:3)
      DOUBLEPRECISION, INTENT(OUT) ::  QJ (0:3), resfnode
! NB:
!         QJ(J) is output, but only for those J with ROOTLI(J).ne.0
! Locals, etc
      INTEGER         :: J
      DOUBLEPRECISION :: CJ, DZ, Qasum, SIG
!----------------------------------------------------------------------*
      Qasum = zero
      qj = zero
      DO J = 0, 3
         IF (ISZERO(ROOTLI(J))) CYCLE
!                            >>>>>>>>
         DZ = ZNODE-ZI (J)
         SIG = SIGN (ONE, DZ)
         CJ = CI (J) + DI (J) * MAX (ZERO, DZ)
         QJ (J) = SIG * CJ * SQRT (SIG * DZ) / ROOTLI (J)
         Qasum = QJ (J) + Qasum
      ENDDO
      resfnode = Qasum
   END SUBROUTINE FNODE

END MODULE oc_node_flows
