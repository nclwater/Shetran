MODULE oc_flow_control
! Module for flow control and mass conservation in overland channel networks
! Handles mass conservation, flow corrections, and depth adjustments
! Extracted from OCmod2.f90 as part of refactoring

   USE SGLOBAL
   USE oc_parameters
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: OCFIX

CONTAINS

!SSSSSS SUBROUTINE OCFIX
   SUBROUTINE OCFIX(afromICMREF, afromICMRF2, nel, dtoc, inhrf, GGGETHRF, inqsa, GGGETQSA)
!----------------------------------------------------------------------*
!  Ensure that discharges and elevations/depths satisfy requirements.
!
!  The following conditions are treated:
!     i flow in direction of non-negative surface elevation gradient;
!    ii flow  rate  less than a pre-defined minimum meaningful value;
!   iii water depth less than a pre-defined minimum meaningful value.
!
!  Treatment comprises a reduction of existing flow rates, with
!  corresponding (ie conservative) adjustments to surface elevations.
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCFIX/4.27
! Modifications:
! RAH  08.10.94  Version 3.4.1. Created 03.10.94 from part of OCSIM.
!                  Repeat IEL loop up to NPASS times.  UHCRIT was 0.
!                  HRF adjustment (corresponding to flow correction)
!                   had "*DTOC" missing.
!                  Adjust confluence flows too (were disregarded).
! RAH  980115  4.2  Add INTRINSIC.
!      980617      Treat cases i & ii (above) for discharges only.
!                  Replace "positive" with "non-negative" in case i.
!                  New test criteria at confluences (were unreliable).
!                  Less severe flow adjustments (were just set to 0).
!      980618       Give details, in MSG, of any mass created or lost.
!      980623      Merge flow & depth loops, with depth as priority.
!      980624      Make depth adjustments conservative.
!                  Scrap locals KEL,KFACE (KEL wasn't set when JEL=0).
!                   Swap sign of HERROR; use in ERROR call criteria.
!      980625      Adjust HRF(IEL) once only: use ZE in the interim.
!      980729      NPASS 50 was 4; also introduce new error 1060.
!                   Replace statement function FNDXY with array DXY.
! SB   990204 4.27  Problem with small flows from a lower to a higher
!                   element. Modify DQE0
! SB   990208 4.27  Problem with small flows from a lower to a higher
!                   element. Add AOK = FALSE in final depth adjustment
!----------------------------------------------------------------------*
! Entry requirements:
!  NEL.ge.1    NELEE.ge.NEL         DTOC.gt.0
!  PRI.ge.0    NLFEE.ge.1    AREA(1:NEL).gt.0    PRI open for F output
!  for iel in 1:NEL  for iface in 1:4
!      let jel=ICMREF(iel,iface,2)    let ibr=-jel
!          jel.le.NEL                     ibr.le.NLFEE
!      {jel.lt.1} .or. {1.le.ICMREF(iel,iface,3).le.4}
!      {ibr.lt.1} .or. {  for p in 1:3  let pel=ICMRF2(ibr,p,1)
!                             pel.le.NEL
!                            {pel.lt.1} .or. {1.le.ICMRF2(ibr,p,2).le.4}
!                         max{pel}.ge.1  }
!----------------------------------------------------------------------*
      INTEGER, INTENT(IN) :: nel, afromICMREF (NELEE, 4, 2:3), afromICMRF2 (NLFEE, 3, 2)
      DOUBLEPRECISION, INTENT(IN) :: dtoc

! Parameters for flow control
      INTEGER         :: NPASS
      PARAMETER (NPASS = 100)
      DOUBLEPRECISION :: UHCRIT, HCRIT, HERROR
      PARAMETER (UHCRIT = 1D-7, HCRIT = 1D-7, HERROR = 1D-5)

! Input/Output arrays
      DOUBLEPRECISION, DIMENSION(nel), INTENT(IN)    :: inhrf
      DOUBLEPRECISION, DIMENSION(nel), INTENT(OUT)   :: GGGETHRF
      DOUBLEPRECISION, DIMENSION(nel,4), INTENT(IN)  :: inqsa
      DOUBLEPRECISION, DIMENSION(nel,4), INTENT(OUT) :: GGGETQSA

! Local variables
      INTEGER          :: IELc, IFACE, IBR, idum
      INTEGER          :: JEL, JFACE, PPP, PASSS, PEL, PEL0, PFACE, PFACE0
      DOUBLEPRECISION  :: DQE, DZE, QE, ZE, DHQ, DHH, DDZ, DQE0, FDQE, H
      DOUBLEPRECISION  :: DQA, DZA, QA, ZA, QQ, QQMIN, Qasum, SGN, ZG, DXY (0:1), rdum4(4)
      LOGICAL          :: AOK, QSMALL, HSMALL, FAIL, FAILP, TEST, FLAG (4)
      CHARACTER(132)  :: MSG

! Initialize arrays
      GGGETHRF = inhrf
      GGGETQSA = inqsa
      aok = .FALSE.

! Control Loop
      out900 : DO PASSS = 1, NPASS
         IF(aok) THEN
            CYCLE out900  ! Entry into loop problem
         ELSE
            AOK = .TRUE.
         ENDIF

         out400 : DO ielc = 1, NEL
            ZE = GGGETHRF (ielc)
            DZE = DTOC / cellarea (ielc)
            DXY (0) = DXQQ (ielc)
            DXY (1) = DYQQ (ielc)

            ! Depth Criterion: flag outflow (D<0) or inflow (D>0) faces
            ZG = ZGRUND (ielc)
            H = ZE-ZG
            HSMALL = (H.LT.HCRIT).AND.NOTZERO(H)
            FDQE = ZERO

            IF (HSMALL) THEN
               DQE0 = - H / DZE
               SGN = SIGN (ONE, DQE0)
               Qasum = ZERO
               DO IFACE = 1, 4
                  QE = GGGETQSA (ielc, IFACE)
                  FLAG (IFACE) = QE * SGN.LT.ZERO
                  IF (FLAG (IFACE) ) Qasum = Qasum + QE
               ENDDO
               IF (NOTZERO(Qasum)) FDQE = MAX ( - ONE, DQE0 / Qasum)
            ENDIF

            ! Face Loop
            Qasum = ZERO
            out300 : DO IFACE = 1, 4
               QE = GGGETQSA (ielc, IFACE)
               ! apply flow criteria to discharges only
               TEST = QE.LT.ZERO
               IF (HSMALL) TEST = FLAG (IFACE)
               IF (.NOT.TEST) CYCLE out300

               QSMALL = - QE.LT.DXY (MOD (IFACE, 2) ) * UHCRIT
               TEST = QSMALL.OR.HSMALL

               ! Gradient Criterion & Neighbour Location
               JEL = afromICMREF (ielc, IFACE, 2)
               IF (JEL.GT.0) THEN
                  ! regular face
                  JFACE = afromICMREF (ielc, IFACE, 3)
                  FAIL = GGGETHRF (JEL) .GE.ZE
               ELSEIF (JEL.EQ.0) THEN
                  ! external boundary
                  FAIL = .FALSE.
               ELSE
                  ! confluence: choose branch with largest flow
                  IBR = - JEL
                  QQMIN = ZERO
                  FAIL = .FALSE.
                  out200 : DO PPP = 1, 3
                     PEL = afromICMRF2 (IBR, PPP, 1)
                     IF (PEL.LT.1) CYCLE out200
                     PFACE = afromICMRF2 (IBR, PPP, 2)
                     QQ = GGGETQSA (PEL, PFACE) * QE
                     FAILP = (GGGETHRF (PEL) .GE.ZE).AND.(QQ.LT.ZERO)
                     IF ( (FAILP.OR.TEST) .AND.QQ.LT.QQMIN) THEN
                        JEL = PEL
                        JFACE = PFACE
                        QQMIN = QQ
                     ENDIF
                     FAIL = FAIL.OR.FAILP
                     PEL0 = PEL
                     PFACE0 = PFACE
                  ENDDO out200
                  IF (JEL.LT.0) THEN
                     JEL = PEL0
                     JFACE = PFACE0
                  ENDIF
               ENDIF

               ! Adjustments
               IF (FAIL.OR.TEST) THEN
                  AOK = .FALSE.
                  IF (JEL.GT.0) THEN
                     DZA = DTOC / cellarea (JEL)
                     ZA = GGGETHRF (JEL)
                     QA = GGGETQSA (JEL, JFACE)
                  ENDIF
                  IF (HSMALL) THEN
                     DQE = FDQE * QE
                  ELSEIF (QSMALL) THEN
                     DQE = - QE
                  ELSE
                     DDZ = DZMIN + ZA - ZE
                     DQE = MIN ( + QA, - QE, DDZ / (DZA + DZE) )
                  ENDIF
                  Qasum = Qasum + DQE
                  GGGETQSA(ielc, IFACE) = QE+DQE
                  ZE = ZE+DQE * DZE
                  IF (JEL.GT.0) THEN
                     SGN = SIGN (ONE, DQE)
                     DQA = - SGN * MIN (SGN * DQE, SGN * QA)
                     Qasum = Qasum + DQA
                     GGGETQSA(JEL, JFACE) = QA + DQA
                     GGGETHRF(JEL) = ZA + DQA * DZA
                  ENDIF
                  IF (.NOT.HSMALL) THEN
                     DHQ = Qasum * DZE
                     Qasum = ZERO
                     ! Error message always produced if pass.eq.npass
                     IF ((ABS (DHQ) .GT.HERROR) .or.(passs.eq.npass)) THEN
                        rdum4(1)= - QE ; rdum4(2)=- 1D2 * DQE / QE ; idum=IFACE ; rdum4(4)=DHQ
                        WRITE (MSG, 91030) rdum4(1:2),idum,rdum4(4:4)
                        CALL ERROR(WWWARN, 1030, PPPRI, ielc, 0, MSG)
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO out300

            ! Final Depth Adjustment
            IF (HSMALL) THEN
               AOK = .FALSE.
               DHQ = Qasum * DZE
               DHH = ZG - ZE
               ZE = ZG
               ! Error message always produced if pass.eq.npass
               IF ((ABS (DHQ) + ABS (DHH) .GT.HERROR) .or.(passs.eq.npass)) THEN
                  rdum4(1)=H ; rdum4(2)=DHQ ; rdum4(3)=DHH
                  WRITE (MSG, 91024) rdum4(1:3)
                  CALL ERROR(WWWARN, 1024, PPPRI, ielc, 0, MSG)
               ENDIF
            ENDIF
            GGGETHRF(ielc) =ZE

         ENDDO out400
      ENDDO out900

      IF(.not.aok) CALL ERROR(WWWARN, 1060, PPPRI, 0, 0, 'OC flow criteria could not be met')

! Format statements
91024 FORMAT( 'Surface water depth adjusted from',SP,1PG15.7,' to zero',         ': depth created =',2G15.7 )
91030 FORMAT( 'Surface water discharge rate',1PG14.7,' reduced by', &
         0PF7.2,'% at face',I4,': depth created =',SP,1PG15.7 )
   END SUBROUTINE OCFIX

END MODULE oc_flow_control
