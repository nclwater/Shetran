MODULE oc_compute
! Computation routines for overland channel calculations
! Contains OCABC and other computational subroutines
! Extracted from OCmod.f90

   USE SGLOBAL
   USE AL_C ,     ONLY : IDUM, NBFACE, CWIDTH, ZBFULL, &
      DUMMY, ZBEFF, ICMBK, BEXBK, QBKB, QBKF, ICMRF2, &
      TIH, DHF, CLENTH, CLENTH, PNETTO, QH, QOC, LINKNS, ARXL
   USE AL_D ,     ONLY : DQ0ST, DQIST, DQIST2, OCNOW, OCNEXT, OCD, ESWA, QMAX, NOCBCC, &
      NOCBCD, LCODEX, LCODEY, NOCTAB, OHB, OFB
   USE AL_G ,     ONLY : NGDBGN, NX, NY, ICMREF, ICMXY
   USE UTILSMOD , ONLY : HINPUT, FINPUT, AREADR, AREADI, JEMATMUL_VM, JEMATMUL_MM, INVERTMAT
   USE mod_load_filedata ,    ONLY : ALCHK, ALCHKI, ALINIT
   USE OCmod2 ,   ONLY : GETHRF, GETQSA, GETQSA_ALL, SETHRF, SETQSA, CONVEYAN, OCFIX, XSTAB, &
      HRFZZ, qsazz, INITIALISE_OCMOD  !these needed only for ad
   USE OCQDQMOD,  ONLY : OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD !, &  !REST NNEDED ONLY FOR AD
   USE oc_common_data

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: OCABC

CONTAINS

!SSSSSS SUBROUTINE OCABC
   SUBROUTINE OCABC(IND, IROW, ielz, NSV, NCR, NPR, IBC, N, AREAE, &
      ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW, AA, BB, CC, FF)
!----------------------------------------------------------------------*
! CALCULATION OF MATRIX COEFFICIENTS, GIVEN FLOWS AND DERIVATIVES
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCABC/4.2
! [Version history as in original...]
!----------------------------------------------------------------------*
      INTEGER, INTENT(IN)         :: IND, IROW, IELz, NSV, NCR, NPR, IBC, N
      DOUBLEPRECISION, INTENT(IN) :: AREAE, ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW
      DOUBLEPRECISION, INTENT(OUT) :: AA(NXOCEE), BB (NCR), CC (NXOCEE), FF
      INTEGER :: I, IBR, IFACE, IM
      INTEGER :: J, JEL, JFACE, JND, JROW
      DOUBLEPRECISION AR, BKDUM, DQ0, DQI, H, HI, HM, PDUM, Q, QHDUM, WI, WM
      LOGICAL :: BLINK, TEST, iscycle
!
! ----- INITIALIZE OUTPUT ARRAYS & GET WATER DEPTH
!
      IF (NSV.GT.0) CALL ALINIT (ZERO, NSV, AA)
      CALL ALINIT (ZERO, NCR, BB)
      IF (NPR.GT.0) CALL ALINIT (ZERO, NPR, CC)
      H = Z - ZG
!
! ----- HEAD BOUNDARY
!
      IF ((IBC.EQ.3).OR.(IBC.EQ.9)) THEN
         BB (IND) = one
         FF = HNOW - H
         RETURN
      ENDIF
!
! ----- IS THE CURRENT ELEMENT A LINK?
!
      BLINK = ICMREF (ielz, 1) .EQ.3
!
! ----- PUT STORAGE TERM INTO CENTRAL COEFFICIENT FOR CURRENT ELEMENT
!
      TEST = BLINK
      IF (TEST) TEST = Z.LT.ZBF
      IF (TEST) THEN
         !         * note requirements: XINH(IEL,1)=0; XINH(IEL,N).GE.ZBF-ZG
         iscycle=.FALSE.
         DO I = 2, N
            IF(iscycle) CYCLE
            HI = XINH (ielz, I)
            IF (H.LT.HI) THEN
               IM = I - 1
               HM = XINH (ielz, IM)
               WM = XINW (ielz, IM)
               WI = XINW (ielz, I)
               AR = CL * (WM + (WI - WM) * ( (H - HM) / (HI - HM) ) )
               iscycle = .TRUE.
               CYCLE
            ENDIF
         ENDDO
      ELSE
         AR = AREAE
      ENDIF
      BB (IND) = - AR / DTOC
!
! ----- PUT PRECIPITATION, EVAPORATION AND EXCHANGE FLOWS INTO RHS
!
      PDUM = PNETT
      IF (BLINK) THEN
         IF (H.LT.1D-8) PDUM = ZERO
         BKDUM = QBKB (ielz, 1) + QBKF (ielz, 1) + QBKB (ielz, 2) + QBKF ( &
            ielz, 2)
         QHDUM = ZERO
      ELSE
         BKDUM = ZERO
         QHDUM = QHE
      ENDIF
      FF = - AREAE * (PDUM + QHDUM - ESWAE) + BKDUM
!
! ----- LOOP OVER ADJACENT ELEMENTS
!
      DO 500 IFACE = 1, 4
         JEL = ICMREF (ielz, IFACE+4)
         JFACE = ICMREF (ielz, IFACE+8)
!
! --- GET FLOW AND DERIVATIVE (+VE INTO ELEMENT)
!
         Q = GETQSA (ielz, IFACE)
         DQ0 = DQ0ST (ielz, IFACE)
!
! --- ADD INTO COEFFICIENTS FOR CURRENT ELEMENT
!
         BB (IND) = BB (IND) + DQ0
         FF = FF - Q
!
! --- TEST FOR SINGLE ADJACENT ELEMENT
!
         IF (JEL.GT.0) THEN
!
            JROW = ICMREF (JEL, 3)
            JND = NELIND (JEL)
            DQI = DQIST (ielz, IFACE)
!
!        ADD DERIVATIVE TO COEFFICIENT FOR ADJACENT ELEMENT
!
            IF (JROW.EQ.IROW) THEN
               BB (JND) = BB (JND) - DQI
            ELSE
               IBR = JROW - IROW
               IF (IBR.EQ.1) THEN
                  CC (JND) = CC (JND) - DQI
               ELSEIF (IBR.EQ.-1) THEN
                  AA (JND) = AA (JND) - DQI
               ENDIF
            ENDIF
         ENDIF
500   END DO
   END SUBROUTINE OCABC

END MODULE oc_compute
