MODULE oc_channel_flow_types
! Module for different types of channel flow calculations
! Handles boundary conditions, channel links, banks, overland flow, and multi-links
! Extracted from OCmod2.f90 as part of refactoring

   USE SGLOBAL
   USE oc_parameters
   USE oc_data_management, ONLY: XSTAB
   USE oc_hydraulic_calculations, ONLY: OCCODE, QWEIR, CONVEYAN
   USE oc_node_flows, ONLY: OCNODE
!!***ZQ Module integration
   USE ZQmod, ONLY : get_ZQTable_value
   USE AL_D,  ONLY : ZQweirsill,ZQTableRef
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: OCQBC, OCQBNK, OCQGRD, OCQLNK, OCQMLN

CONTAINS

!SSSSSS SUBROUTINE OCQBC
   SUBROUTINE OCQBC(NTYPE, LI, ZGI, STR, W, afromXAFULL, link, afromCOCBCD, ZI, afromHOCNOW, afromQOCF, fromQ, fromDQ)
!----------------------------------------------------------------------*
!  CALCULATE FLOW AND DERIVATIVE AT AN EXTERNAL BOUNDARY
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCQBC/4.2
! Entry requirements:
!  NXSCEE.ge.2    LI.gt.0    if {7.le.NTYPE.le.8} COCBCD(1:2).ge.0
!  if {NTYPE.eq.8.or.MOD(NTYPE,6).eq.3.and.NTYPE.ne.3}
!      ZI.ge.ZGI    [STR,W,XAFULL,XSTAB(1,NXSCEE)].gt.0
!      for i in 1:NXSCEE-1   XSTAB(1,i)=XSTAB(1,NXSCEE)*(i-1)/(NXSCEE-1)
!                            XSTAB(2,i).ge.0    XSTAB(3,i).gt.0
!----------------------------------------------------------------------*
      INTEGER, INTENT(IN)         :: NTYPE, LINK
      DOUBLEPRECISION, INTENT(IN) ::  LI, ZGI, STR, W, afromXAFULL, ZI, afromHOCNOW, afromQOCF, &
         afromCOCBCD(5)
      DOUBLEPRECISION, INTENT(OUT) :: fromQ, fromDQ
      INTEGER                      :: MTYPE
      DOUBLEPRECISION              :: AH, B, C, CONVM, CONVMM, D, DERIVM, DHH, DQU, DUM, DZ, E
      DOUBLEPRECISION              :: H, HM, ROOTDZ, ROOTL
      DOUBLEPRECISION              :: SIG, STRW, SUBRIO, ZSILL, ZL, ZU, ZX, COEFF (2)

! Prologue
      MTYPE = MOD (NTYPE, 6)

! Part 1 - Different boundary condition types
      IF (MTYPE.EQ.3) THEN
         ! Prescribed time-varying head - grid (3) or channel (9)
         ZX = afromHOCNOW
         fromQ = zero
         fromDQ = zero
      ELSEIF (MTYPE.EQ.4) THEN
         ! Prescribed time-varying flow - grid (4) or channel (10)
         fromQ = afromQOCF
         fromDQ = zero
      ELSEIF (MTYPE.EQ.5) THEN
         ! Flow a polynomial function of head - grid (5) or channel (11)
         H = ZI - ZGI
         AH = afromCOCBCD (1) * H
         B = afromCOCBCD (2)
         C = afromCOCBCD (3)
         D = afromCOCBCD (4)
         E = afromCOCBCD (5)
         fromQ = - ( ( ( (AH + B) * H + C) * H + D) * H + E)
         fromDQ = - ( ( (4D0 * AH + 3D0 * B) * H + 2D0 * C) * H + D)
      ELSEIF (NTYPE.EQ.7.OR.NTYPE.EQ.8) THEN
         ! Weir (7) ... with river in parallel (8)
         COEFF (1) = afromCOCBCD (1)
         SUBRIO = afromCOCBCD (2)
         ZSILL = afromCOCBCD (3)
         ZX = afromCOCBCD (4)
         COEFF (2) = COEFF (1)
         ZU = MAX (ZX, ZI)
         ZL = MIN (ZX, ZI)
         CALL QWEIR (ZU, ZSILL, ZL, COEFF, SUBRIO, fromQ, DQU, fromDQ)
         IF (ZI.GE.ZX) THEN
            fromQ = - fromQ
            fromDQ = - DQU
         ENDIF
      ENDIF

! Part 2 - Head, or river-part of river+weir
      IF (MTYPE.EQ.3.OR.NTYPE.EQ.8) THEN
         DZ = ZX - ZI
         SIG = SIGN (ONE, DZ)
         DZ = SIG * DZ
         ROOTDZ = SQRT (DZ)
         DHH = LI * DBLE(4 - MTYPE)
         ROOTL = SQRT (DHH)

         IF (NTYPE.EQ.3) THEN
            HM = ZI - ZGI
            STRW = STR * W
            CALL CONVEYAN(strw, hm, convm, derivm, 1)
         ELSE
            CALL OCCODE (ZGI, STR, W, afromXAFULL, XSTAB(:,:,link), ZI, CONVM, DERIVM)
         ENDIF

         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
         fromQ = fromQ + SIG * CONVM * ROOTDZ / ROOTL
         fromDQ = fromDQ + (SIG * DERIVM * ROOTDZ - DUM) / ROOTL
      ENDIF
   END SUBROUTINE OCQBC

!SSSSSS SUBROUTINE OCQBNK
   SUBROUTINE OCQBNK (W, LI, ZBG, STR, ZI, Q, DQ)
!----------------------------------------------------------------------*
!  CALCULATE FLOW AND DERIVATIVES AT A BANK OF A CHANNEL LINK
!----------------------------------------------------------------------*
! Note: Subscript 0 refers to the link, 1 to the land element
      DOUBLEPRECISION, INTENT(IN)  :: W, LI (0:1), ZBG (0:1), STR (0:1), ZI (0:1)
      DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)
      INTEGER                      :: HI, LO
      DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
      DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, STRW
      DOUBLEPRECISION              :: DZL, ZB, ZG, COEFF (2), rdum

      DZ = ZI (1) - ZI (0)
      SIG = SIGN (ONE, DZ)
      HI = (1 + NINT (SIG) ) / 2
      LO = 1 - HI
      ZB = ZBG (0)
      ZG = ZBG (1)
      DZL = ZI (LO) - ZB

! Channel bank-full lower than adjacent ground: resistance equation
      IF (ZG.GE.ZB) THEN
         DZ = SIG * DZ + MIN (DZL, ZERO)
         ROOTDZ = SQRT (DZ)
         HM = ZI (HI) - ZBG (HI)
         DHH = LI (0) + LI (1)
         STRW = W * (STR (0) * LI (0) + STR (1) * LI (1) ) / DHH
         ROOTL = SQRT (DHH)
         CALL CONVEYAN(strw, hm, convm, derivm, 1)
         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
         Q (LO) = CONVM * ROOTDZ / ROOTL
         DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
         IF (DZL.LT. - DZMIN) DUM = ZERO
         DQ (LO, LO) = - DUM / ROOTL

! Channel bank-full higher than adjacent ground: flat-crested weir eqn
      ELSE
         COEFF (1) = ROOT2G * W
         COEFF (2) = 386D-3 * COEFF (1)
         CALL QWEIR(ZI(HI), ZB, ZI(LO), COEFF, F23, Q(LO), DQ(LO,HI), rdum)
         DQ(LO,LO) = rdum
      ENDIF

! Copy LO to HI
      Q (HI) = - Q (LO)
      DQ (HI, HI) = - DQ (LO, HI)
      DQ (HI, LO) = - DQ (LO, LO)
   END SUBROUTINE OCQBNK

!SSSSSS SUBROUTINE OCQGRD
   SUBROUTINE OCQGRD (NTYPE, LI, ZGI, STR, W, ZI, Q, DQ)
!----------------------------------------------------------------------*
!  CALCULATE FLOW AND DERIVATIVES BETWEEN TWO LAND ELEMENTS
!----------------------------------------------------------------------*
      INTEGER, INTENT(IN)          :: NTYPE
      DOUBLEPRECISION, INTENT(IN)  :: W, LI (0:1), ZGI (0:1), STR (0:1), ZI (0:1)
      DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)
      INTEGER                      :: HI, LO, I
      DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ, HM
      DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, STRW

! INTERNAL IMPERMEABLE BOUNDARY
      IF (NTYPE.EQ.1) THEN
         DO I = 0, 1
            Q (I) = zero
            DQ (I, 0) = zero
            DQ (I, 1) = zero
         ENDDO
         RETURN
      ENDIF

! Set up local variables
      DZ = ZI (1) - ZI (0)
      SIG = SIGN (ONE, DZ)
      HI = (1 + NINT (SIG) ) / 2
      LO = 1 - HI
      DZ = SIG * DZ
      ROOTDZ = SQRT (DZ)
      HM = ZI (HI) - ZGI (HI)
      DHH = LI (0) + LI (1)
      STRW = W * (STR (0) * LI (0) + STR (1) * LI (1) ) / DHH
      ROOTL = SQRT (DHH)

! CALCULATE FLOW AND DERIVATIVES
      CALL CONVEYAN(strw, hm, convm, derivm, 1)
      CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
      DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
      Q (LO) = CONVM * ROOTDZ / ROOTL
      DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
      DQ (LO, LO) = - DUM / ROOTL
      Q (HI) = - Q (LO)
      DQ (HI, HI) = - DQ (LO, HI)
      DQ (HI, LO) = - DQ (LO, LO)
   END SUBROUTINE OCQGRD

!SSSSSS SUBROUTINE OCQLNK
   SUBROUTINE OCQLNK(NTYPE, LI, ZGI, STR, CW, XA, jXSwork, afromCOCBCD, ZI, Q, DQ)
!----------------------------------------------------------------------*
!  CALCULATE FLOW AND DERIVATIVES BETWEEN TWO CHANNEL LINKS
!----------------------------------------------------------------------*
      INTEGER, INTENT(IN)          :: NTYPE
      DOUBLEPRECISION, INTENT(IN)  :: LI (0:1), ZGI (0:1), CW (0:1), afromCOCBCD(3)
      DOUBLEPRECISION, INTENT(IN)  ::  ZI (0:1), STR (0:1), XA (0:1)
      INTEGER, INTENT(IN)          :: JXSWORK(0:3)
      DOUBLEPRECISION, INTENT(OUT) :: Q (0:1), DQ (0:1, 0:1)
      INTEGER                      :: HI, LO
      DOUBLEPRECISION              :: CONVM, CONVMM, DERIVM, DHH, DUM, DZ
      DOUBLEPRECISION              :: ROOTDZ, ROOTL, SIG, SUBRIO, ZSILL
      DOUBLEPRECISION              :: COEFF (2), rdum
!!***ZQ Module 200520
      DOUBLEPRECISION              :: dzu, weirsill

! Set up local variables - part 1
      DZ = ZI (1) - ZI (0)
      SIG = SIGN (ONE, DZ)
      HI = (1 + NINT (SIG) ) / 2
      LO = 1 - HI

! Internal weir
      IF (NTYPE.EQ.7) THEN
         COEFF (1) = afromCOCBCD (1)
         SUBRIO = afromCOCBCD (2)
         ZSILL = afromCOCBCD (3)
         COEFF (2) = COEFF (1)
         CALL QWEIR(ZI(HI), ZSILL, ZI(LO), COEFF, SUBRIO, Q(LO), DQ(LO,HI), rdum)
         DQ(LO,LO)=rdum

!!***ZQ Module 200520
      ELSEIF (NTYPE.EQ.12) THEN
         Q(LO)     = get_ZQTable_value(ZQTableRef,ZI(HI))
         weirsill  = ZQWeirSill(ZQTableRef)
         DZU       = DIMJE(ZI(HI), weirsill)
         DQ(LO,HI) = 50.0*1.5*sqrt(dzu)  ! This works for Crummock. Stability should be tested
         DQ(LO,LO) = 0
!!***ZQ Module 200520 end

      ELSE
         ! Set up local variables - part 2
         DZ = SIG * DZ
         ROOTDZ = SQRT (DZ)
         DHH = LI (0) + LI (1)
         ROOTL = SQRT (DHH)

         ! CALCULATE FLOW AND DERIVATIVES
         CALL OCCODE (ZGI(HI), STR(HI), CW(HI), XA(HI), XSTAB(:, :, jxswork(HI)), ZI(HI), CONVM, DERIVM)
         CONVMM = CONVM + DERIVM * DIMJE(DZMIN, DZ)
         DUM = half * CONVMM / MAX (RDZMIN, ROOTDZ)
         Q (LO) = CONVM * ROOTDZ / ROOTL
         DQ (LO, HI) = (DERIVM * ROOTDZ + DUM) / ROOTL
         DQ (LO, LO) = - DUM / ROOTL
      ENDIF

      Q (HI) = - Q (LO)
      DQ (HI, HI) = - DQ (LO, HI)
      DQ (HI, LO) = - DQ (LO, LO)
   END SUBROUTINE OCQLNK

!SSSSSS SUBROUTINE OCQMLN
   SUBROUTINE OCQMLN(ielb, JEL2, LI, ZGI, STR, CW, XA,  ZI, QJ, DQIJ, JXSwork)
!----------------------------------------------------------------------*
!  CALCULATE FLOW AND DERIVATIVES BETWEEN MULTIPLE CHANNEL LINKS
!----------------------------------------------------------------------*
      INTEGER, INTENT(IN)          :: IELb, JEL2 (0:3)
      DOUBLEPRECISION, INTENT(IN)  :: LI (0:3), ZGI (0:3), STR (0:3)
      DOUBLEPRECISION, INTENT(IN)  ::  CW (0:3), XA (0:3), ZI (0:3)
      INTEGER, INTENT(IN)          :: jxswork(0:3)
      DOUBLEPRECISION, INTENT(OUT) ::  QJ (0:3), DQIJ (0:3, 0:3)

      DOUBLEPRECISION             :: ONEPC, WLMIN
      PARAMETER (ONEPC = 1D-2, WLMIN = 1D-3)
      INTEGER                     :: I, J
      DOUBLEPRECISION             :: CSAVE, DSAVE, CI (0:3), DI (0:3), QDUM2 (0:3)
      DOUBLEPRECISION             :: ZINC, ZSAVE, ROOTLI (0:3), ZJ (0:3)

! Calculate conveyance & its derivative (both.ge.0), & set local arrays
      DO J = 0, 3
         IF (JEL2 (J) .LE.0) THEN
            ROOTLI (J) = zero  ! OCNODE uses ROOTLI as a flag
         ELSE
            ROOTLI (J) = SQRT (LI (J) )
            ZJ (J) = ZI (J)
            CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSTAB(:, :, jxswork(J)), ZJ(J), CI(J), DI(J))
         ENDIF
      ENDDO

! Find flows out of node
      CALL OCNODE (ielb, ZI, CI, DI, ROOTLI, QJ)

! CALC. DQi/DHj
      DO J = 0, 3
         IF (JEL2 (J) .LE.0) CYCLE
         ! temporarily increase ZJ and recalculate CI,DI
         ZSAVE  = ZJ(J)
         CSAVE  = CI(J)
         DSAVE  = DI(J)
         ZINC   = MAX(WLMIN, (ZSAVE-ZGI(J))*ONEPC)
         ZJ (J) = ZSAVE+ZINC
         CALL OCCODE (ZGI(J), STR(J), CW(J), XA(J), XSTAB(1, 1, JXSWORK(J)), ZJ(J), CI(J), DI(J) )

         ! calculate resultant flows & evaluate derivative
         CALL OCNODE (ielb, ZJ, CI, DI, ROOTLI, QDUM2)
         DO I = 0, 3
            DQIJ (I, J) = (QDUM2 (I) - QJ (I) ) / ZINC
         ENDDO
         ZJ(J) = ZSAVE
         CI(J) = CSAVE
         DI(J) = DSAVE
      ENDDO
   END SUBROUTINE OCQMLN

END MODULE oc_channel_flow_types
