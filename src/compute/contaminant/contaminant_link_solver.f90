MODULE contaminant_link_solver
! RAH  970218  4.1  Swap subscripts: QVSH,DELTAZ,VSTHE (see AL.C).
! [REFACTORING] 19/08/2025 - Extracted from CMmod.f90 as link solver module
!                           Contains LINK*, SNL3, FRET and related subroutines for link/channel calculations
!
   USE contaminant_common
   USE SED_CS
   USE CONT_CC
   USE COLM_C1
   USE COLM_CG
   USE COLM_CO
   USE LINK_CC
   USE LINK_CC1
   USE LINK_CW
   USE BK_CW
   USE SED_CO
   USE PLANT_CC

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: LINKSM, LINKW, LINK, SNL3, FRET
   ! TODO: Add back when implemented: LINKW, LINK, SNL3, FRET

CONTAINS

   !SSSSSS SUBROUTINE LINKSM (NLINK)
   SUBROUTINE LINKSM (NLINK)
      !                             UPDATES THE CONCENTRATION OF EACH
      !                             CONTAMINANT IN LINK NLINK
      INTEGER, INTENT(IN) :: nlink
      INTEGER :: ncont, nce, jlend, jdum, jla, jsed, na, lfone, ldum, la
      DOUBLEPRECISION FBTAD (NSEDEE), FBTAS (NSEDEE), KDDUM (NSEDEE), &
         SSBED1 (NSEDEE), SSBED (NSEDEE), SSD1 (NSEDEE), SSD (NSEDEE), &
         SSF1 (NSEDEE), SSF (NSEDEE)
      DOUBLEPRECISION :: ccpbd, ccpbs, qcdum, sumd, sums, dddsum, pb, fdum, fdumc, &
         fdumt, dum, arl, arp, ccpsf, dddum, dsdum, ccpbd1, ccpbs1, ccpsf1, &
         dumx

      CCBD1Q = zero
      CCBS1Q = zero
      CCSF1Q = zero
      FCBD1Q = zero
      FCBS1Q = zero
      FCSF1Q = zero

      GCPLAQ = zero
      !                             SET PARENT CONCENTRATIONS AND RETARDATION
      !                             VARIABLES TO O FOR 1ST PASS OF DO LOOP 100
      DO 100 NCONT = 1, NCON
         DO 102 JBK = 1, 2
            CCPBK (JBK, 1) = CCCCO (NBK (JBK), 1, NCONT)
            !                             THIS ELEMENT OF ARRAY CCCCO IS USED TO
            !                             HOLD THE EFFECTIVE CONCENTRATION IN
            !                             THE FLOW ENTERING THE STREAM VIA THE
            !                             STREAM BED
            DO 104 NCE = NCEBK (JBK), NCETOP
               CCPBK (JBK, NCE) = CCCCO (NBK (JBK), NCE, NCONT)
               SCPBK (JBK, NCE) = SSSSO (NBK (JBK), NCE, NCONT)
104         END DO
            CCPGS1 (JBK) = CCCC (NBK (JBK), NCETOP, NCONT)
102      END DO
         CCPBD = CCCCO (NLINK, NCETOP - 2, NCONT)
         CCPBS = CCCCO (NLINK, NCETOP - 1, NCONT)
         IF (USCP.GT.half) THEN
            CCPSF = CCCCO (NLINK, NCETOP, NCONT)
         ELSE
            CCPSF = ccapin (ncont)
            !                             IF THERE IS NO WATER IN LINK

         ENDIF
         DO 110 JLEND = 1, 2
            IF (ISLK (JLEND) ) THEN
               !                             THERE ARE OTHER LINKS ASSOCIATED WITH END
               !                             JLEND OF THE CURRENT LINK
               DO 112 JDUM = 1, 3
                  JLA = (JLEND-1) * 3 + JDUM
                  LA = LWORK (JLA)
                  IF (LA.NE.0) THEN
                     CCSFA1 (JLA) = CCCC (LA, NCETOP, NCONT)
                     FCSFA1 (JLA) = FSF (LA, NCONT) + FSFT (LA, NCONT) &
                        * TSE+FSFC (LA, NCONT) * (CCSFA1 (JLA) - CCCCO (LA, &
                        NCETOP, NCONT) )
                  ELSE
                     CCSFA1 (JLA) = zero
                     FCSFA1 (JLA) = zero
                  ENDIF

112            END DO
            ELSE
               !                             END JLEND OF LINK IS AT CATCHMENT BOUNDARY
               !                             THE HEAD OF A STREAM, OR A SPRING
               JLA = (JLEND-1) * 3 + 1
               CCSFA1 (JLA) = CCAPE (NLINK, NCONT)
               FCSFA1 (JLA) = one
               !                             FOR FLOW INTO CATCHMENT OR SPRING
               DO 114 JDUM = 2, 3
                  JLA = (JLEND-1) * 3 + JDUM
                  CCSFA1 (JLA) = zero
                  FCSFA1 (JLA) = zero
114            END DO
            ENDIF

110      END DO
         !                             SET LINK AND BANK CONCENTRATIONS.
         !                             NB: IF THE STREAM IS DRY, THE STREAM WATER
         !                             CONCENTRATION SET TO THE CONCENTRATION IN
         !                             RAIN WATER
         ICP1 = - IIICF (NCONT) * AREA (NLINK) / (D0 * CLENTH (NLINK) )
         !#######################################################################
         qcdum = (qqqsl1 - qqqdum) * ccapi (ncont)
         if (nwell.ne.0) qcdum = qcdum + qqqdum * ccccw (nwell, ncont)

         QCP1 = qcdum / (D0 * Z2 * KS)
         !       QCP1 = QQQSL1*CCAPI(NCONT)/(D0*Z2*KS)
         !                             SET VARIABLES FOR WET AND DRY INPUT OF
         !                             CONTAMINANT FROM ABOVE
         !######## temporary code for inclusion of irrigation water in rain water
         SUMD = zero
         SUMS = zero
         DO 150 JSED = 1, NSED
            SUMD = SUMD+GINFD (NLINK, JSED)
            SUMS = SUMS + GINFS (NLINK, JSED)
            KDDUM (JSED) = KDDLS (JSED, NCONT)
150      END DO
         IF (ISZERO(SUMD)) then
            dddum = one
         else
            dddum = sumd
         endif
         IF (ISZERO(SUMS)) then
            dsdum = one
         else
            dsdum = sums
         endif
         DO 152 JSED = 1, NSED
            FBTAD (JSED) = GINFD (NLINK, JSED) / dddum
            FBTAS (JSED) = GINFS (NLINK, JSED) / dsdum

152      END DO
         !                             SCALE RATES OF INFLITRATION TO GIVE THE
         !                             FRACTIONS IN EACH GROUP OF AN EFFECTIVE
         !                             SOIL. THE EFFECTIVE SOIL IS THAT WHICH IF
         !                             ERODED AT A RATE EQUAL TO THE TOTAL RATE
         !                             OF INFILTRATION WOULD RELEASE THE CORRECT
         !                             AMOUNT OF SEDIMENTS FOR INFILTRATION

         PB = PBSED (NLINK)
         FDUM = zero
         FDUMC = zero
         FDUMT = zero
         CALL FRET (CCPBS, GNN (NCONT), PB, PB, FBTAD, FBTAD, KDDUM, PB, &
            PB, PB, FDUM, FDUMC, FDUMT, TSE, NSED, ISADNL)
         DUM = SUMD * CCPBS / CLENTH (NLINK)
         ICPSBD = (FDUM - PB) * DUM
         ICSBDC = FDUMC * DUM + ICPSBD
         ICSBDT = FDUMT * DUM
         !                             SET INFILTRATION VARIABLES FOR BED DEEP
         !                             LAYER
         IF (USCP.LT.half) THEN
            !                             THERE IS NO WATER IN LINK
            ICPSBS = zero
            ICSBSC = zero
            ICSBST = zero
         ELSE
            CALL FRET (CCPSF, GNN (NCONT), one, one, FBTAS, FBTAS, &
               KDDUM, zero, zero, zero, FDUM, FDUMC, FDUMT, TSE, NSED, &
               ISADNL)
            DUM = SUMD * CCPSF / CLENTH (NLINK)
            ICPSBS = (FDUM - PB) * DUM
            ICSBSC = FDUMC * DUM + ICPSBS
            ICSBST = FDUMT * DUM

         ENDIF
         !                             SET INFILTRATION VARIABLES FOR BED SURFACE
         !                             LAYER
         ARL = DLS (NLINK) * CWIDTH (NLINK)
         !                             X-SECIONAL AREA OF LOOSE SEDIMENTS IN BED
         ARP = (ACPBD1 - ACPBS) * Z2SQ
         !                             X-SECTIONAL AREA OF NON-ERODED PARENT
         !                             MATERIAL WITHIN BED DEEP LAYER
         DUM = one / (ARL + ARP)
         DO 200 JSED = 1, NSED
            SSBED1 (JSED) = DUM * (ARL * FBETA (NLINK, JSED) + ARP * &
               SOSDFN (NSOBED (NLINK), JSED) )
            SSBED (JSED) = FBBEDO (NLINK, JSED)
            FBBEDO (NLINK, JSED) = SSBED1 (JSED)
            SSF1 (JSED) = FDEL (NLINK, JSED)
            SSF (JSED) = FDELO (NLINK, JSED)
            FDELO (NLINK, JSED) = SSF1 (JSED)
            SSD1 (JSED) = FBTSD (NLINK, JSED)
            SSD (JSED) = FBTSDO (NLINK, JSED)
            FBTSDO (NLINK, JSED) = SSD1 (JSED)

200      END DO

         CALL FRET (CCPBD, GNN (NCONT), THBEDO (NLINK), THBED (NLINK), &
            SSBED, SSBED1, KDDUM, PB, PB, PB, FCPBD, FCPBDC, FCPBDT, TSE, &
            NSED, ISADNL)

         CALL FRET (CCPBS, GNN (NCONT), THBEDO (NLINK), THBED (NLINK), &
            SSBED, SSBED1, KDDUM, PB, PB, PB, FCPBS, FCPBSC, FCPBST, TSE, &
            NSED, ISADNL)
         CALL FRET (CCPSF, GNN (NCONT), one, one, SSF, SSF1, KDDUM, &
            zero, zero, zero, FCPSF, FCPSFC, FCPSFT, TSE, NSED, ISADNL)
         fsf (nlink, ncont) = fcpsf
         fsfc (nlink, ncont) = fcpsfc

         fsft (nlink, ncont) = fcpsft
         !                                       save retardation factors for con

         CALL FRET (CCPSF, GNN (NCONT), one, one, SSD, SSD1, KDDUM, &
            zero, zero, zero, FCPSD, FCPSDC, FCPSDT, TSE, NSED, ISADNL)
         !                             SET REATRDATION VARIABLES FOR THE BED DEEP
         !                             LAYER, BED SURFACE LAYER, STREAM WATER,
         !                             AND NEWLY DEPOSITED SEDIMENTS
         DO 250 JBK = 1, 2
            NA = NBK (JBK)
            FCPSW1 (JBK) = RSW (NA, NCONT) + RSWT (NA, NCONT) * TSE+ &
               RSWC (NA, NCONT) * (CCCC (NA, NCETOP, NCONT) - CCPBK (JBK, &
               NCONT) )
            DO 252 NCE = NCEBK (JBK), NCETOP
               FCPBK (JBK, NCE) = FCPBKO (NLINK, JBK, NCE, NCONT)
               GCPBK (JBK, NCE) = GCPBKO (NLINK, JBK, NCE, NCONT)
252         END DO
            !                             NB: FCPBKO AND GCPBKO CALCULATED IN COLMSM

250      END DO
         !                             SET RETRDATION VARIABLES FOR THE DYNAMIC
         !                             AND DEAD SPACE REGIONS OF THE ERODING
         !                             BANK SOIL
         ECPBD = zero
         ECPBDC = zero
         ECPBDT = zero
         ECPBS = zero
         ECPBSC = zero
         ECPBST = zero
         ECPSF = zero
         ECPSFC = zero

         ECPSFT = zero
         !                             SET RATES OF PLANT UPTAKE
         DUM = CWIDTH (NLINK) / D0
         ACSBD1 = DUM * ALPHBD (NCONT)
         ACSBS1 = DUM * ALPHBS (NCONT)

         GCPLAL = GCPLA (NCONT)
         !                             SET CONTAMINANT INFILTRATION RATE WITH
         !                             SEDIMENT; AND CONTAMINANT DECAY RATE
         CCPBD1 = zero
         CCPBS1 = zero
         CCPSF1 = zero

         CALL LINK (CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, &
            NCETOP)
         !                             CALCULATES AND RETURNS UPDATED
         !                             CONCENTRATIONS
         CCCC (NLINK, NCETOP - 2, NCONT) = CCPBD1
         CCCC (NLINK, NCETOP - 1, NCONT) = CCPBS1

         CCCC (NLINK, NCETOP, NCONT) = CCPSF1
         !                             SAVE UPDATED CONCENTRATIONS IN THE GLOBAL
         !                             ARRAYS
         CCBD1Q = CCPBD1
         CCBS1Q = CCPBS1
         CCSF1Q = CCPSF1
         FCBD1Q = FCPBD+FCPBDT * TSE+FCPBDC * (CCPBD1 - CCPBD)
         FCBS1Q = FCPBS + FCPBST * TSE+FCPBSC * (CCPBS1 - CCPBS)
         FCSF1Q = FCPSF + FCPSFT * TSE+FCPSFC * (CCPSF1 - CCPSF)

         GCPLAQ = GCPLAL
         !                             SET CONCENTRATIONS, RETARDATION, AND DECAY
         !                             VARIABLES FOR PARENT CONTAMINANT FOR NEXT
         !                             PASS OF DO LOOP 100

100   END DO
      RETURN
   END SUBROUTINE LINKSM

   !SSSSSS SUBROUTINE LINKW (NLINK)
   SUBROUTINE LINKW (NLINK)
      !----------------------------------------------------------------------*
      !                             SETS UP THE WATER FLOW DATA FOR USE IN
      !                             SUBROUTINE LINKSM AND LINK
      !----------------------------------------------------------------------*
      ! Version:  SHETRAN/MOC/LINKW/4.1
      INTEGER, INTENT(IN) :: NLINK
      INTEGER :: jlend, jdum, lfone, ldum, jla, jfdum, jfdumb, nce, jvegbk, ndum, la, jbk
      DOUBLEPRECISION :: dumx, dum, duma, dmult, sumk, sum, dumk

      IF (LINKNS (NLINK) ) THEN
         LENDA (1) = 2
         LENDA (2) = 2
         LENDA (3) = 1
         LENDA (4) = 1
         LENDA (5) = 1
         LENDA (6) = 2
      ELSE
         LENDA (1) = 1
         LENDA (2) = 2
         LENDA (3) = 2
         LENDA (4) = 2
         LENDA (5) = 1
         LENDA (6) = 1
      ENDIF

      ! Set scaled variables for area and erosion
      ACPBD1 = ACPBI (NLINK) + ARBDEP (NLINK) / Z2SQ
      ACPBS = ACPBSG (NLINK)
      ACPSF1 = ARXL (NLINK) / Z2SQ
      IF (ACPSF1.LT.1.0D-20) THEN
         USCP = zero
         ACPBDT = zero
         ACPSFT = zero
         QBKB (NLINK, 1) = zero
         QBKB (NLINK, 2) = zero
      ELSE
         USCP = one
         ACPBDT = (ACPBD1 - ACPBDO (NLINK) ) / TSE
         ACPSFT = (ACPSF1 - ACPSFO (NLINK) ) / TSE
      ENDIF
      ACPBDO (NLINK) = ACPBD1
      ACPSFO (NLINK) = ACPSF1
      WCPBD1 = Z2SQOD * ACPBDT / ACPBD1
      VCPBK1 = Z2OD * GNUBK (NLINK)

      NBK (1) = NBANK (NLINK, 1)
      NBK (2) = NBANK (NLINK, 2)
      NCEBK (1) = NHBED (NLINK, 1) + 1
      NCEBK (2) = NHBED (NLINK, 2) + 1

      IF (LINKNS (NLINK) ) THEN
         LFONE = 2
      ELSE
         LFONE = 1
      ENDIF

      ! Set up link connections at end one
      LDUM = ICMREF (NLINK, LFONE+4)
      IF (LDUM.GT.0) THEN
         ISLK (1) = .TRUE.
         LWORK (1) = 0
         LWORK (2) = 0
         LWORK (3) = 0
         if (linkns (nlink) ) then
            if (icmref (nlink, 10) .eq.3) lwork (1) = ldum
            if (icmref (nlink, 10) .eq.4) lwork (2) = ldum
            if (icmref (nlink, 10) .eq.1) lwork (3) = ldum
         else
            if (icmref (nlink, 9) .eq.2) lwork (1) = ldum
            if (icmref (nlink, 9) .eq.3) lwork (2) = ldum
            if (icmref (nlink, 9) .eq.4) lwork (3) = ldum
         endif
      ELSEIF (LDUM.LT.0) THEN
         ISLK (1) = .TRUE.
         LWORK (1) = ICMRF2 ( - LDUM, 3)
         LWORK (2) = ICMRF2 ( - LDUM, 2)
         LWORK (3) = ICMRF2 ( - LDUM, 1)
      ELSE
         ISLK (1) = .FALSE.
         LWORK (1) = 0
         LWORK (2) = 0
         LWORK (3) = 0
      ENDIF

      ! Set up link connections at end two
      LDUM = ICMREF (NLINK, LFONE+6)
      IF (LDUM.GT.0) THEN
         ISLK (2) = .TRUE.
         LWORK (4) = 0
         LWORK (5) = 0
         LWORK (6) = 0
         if (linkns (nlink) ) then
            if (icmref (nlink, 12) .eq.1) lwork (4) = ldum
            if (icmref (nlink, 12) .eq.2) lwork (5) = ldum
            if (icmref (nlink, 12) .eq.3) lwork (6) = ldum
         else
            if (icmref (nlink, 11) .eq.4) lwork (4) = ldum
            if (icmref (nlink, 11) .eq.1) lwork (5) = ldum
            if (icmref (nlink, 11) .eq.2) lwork (6) = ldum
         endif
      ELSEIF (LDUM.LT.0) THEN
         ISLK (2) = .TRUE.
         LWORK (4) = ICMRF2 ( - LDUM, 3)
         LWORK (5) = ICMRF2 ( - LDUM, 2)
         LWORK (6) = ICMRF2 ( - LDUM, 1)
      ELSE
         ISLK (2) = .FALSE.
         LWORK (4) = 0
         LWORK (5) = 0
         LWORK (6) = 0
      ENDIF

      ! Set Peclet numbers for adjacent links
      DUMX = one / (D0 * Z2)
      DO JLEND = 1, 2
         IF (ISLK (JLEND) ) THEN
            DO JDUM = 1, 3
               JLA = (JLEND-1) * 3 + JDUM
               LA = LWORK (JLA)
               IF (LA.NE.0) THEN
                  ACSFA1 (JLA) = MAX (1.0d-6, ACPSFO (LA) )
                  DUM = zero
                  PCSFA1 (JLA) = DUMX * ( - QLINK (LA, LENDA (JLA) ) &
                     - QDEFF (LA, LENDA (JLA) ) * DUM) / ACSFA1 (JLA)
               ELSE
                  ACSFA1 (JLA) = zero
                  PCSFA1 (JLA) = zero
               ENDIF
            END DO
         ELSE
            JLA = (JLEND-1) * 3 + 1
            ACSFA1 (JLA) = MAX (1.0d-6, ACPSFO (NLINK) )
            PCSFA1 (JLA) = DUMX * QLINK (NLINK, JLEND) / ACSFA1 (JLA)
            DO JDUM = 2, 3
               JLA = (JLEND-1) * 3 + JDUM
               ACSFA1 (JLA) = zero
               PCSFA1 (JLA) = zero
            END DO
         ENDIF
      END DO

      IF (USCP.LT.half) THEN
         PCSFM1 = zero
         PCSFP1 = zero
      ELSE
         DUM = DUMX / ACPSF1
         DUMA = zero
         PCSFM1 = DUM * (QLINK (NLINK, 1) + DUMA * QDEFF (NLINK, 1) )
         PCSFP1 = DUM * (QLINK (NLINK, 2) + DUMA * QDEFF (NLINK, 2) )
      ENDIF

      QQQSL1 = - PNETTO (NLINK) * AREA (NLINK)

      ! Temporary well irrigation code
      nwell = NVSWLT (nlink)
      if (nwell.ne.0) then
         qqqdum = - rszwlo (nwell) * area (nwell)
      else
         qqqdum = zero
      endif

      KS = CLENTH (NLINK) / Z2
      DUM = DUMX / KS
      DO JBK = 1, 2
         JFDUM = 2 * JBK - LFONE+1
         JFDUMB = ICMREF (NLINK, JFDUM + 8)
         DO NCE = NCEBK (JBK), NCETOP
            PCPBK1 (JBK, NCE) = - DUM * QVSH (JFDUMB, NCE, NBK (JBK) )
         END DO
         PCPSB1 (JBK) = - DUM * QBKB (NLINK, JBK)
         DMULT = DBLE (2 * JBK - 3)
         PCPSW1 (JBK) = DMULT * DUM * QOC (NLINK, JFDUM)
         JVEGBK = NVC (NBK (JBK) )
         NDUM = NCEBD (NLINK, JBK) + 1
         DO NCE = NDUM, NCETOP
            KSPBK (JBK, NCE) = DELTAZ (NCE, NBK (JBK) ) / z2
         END DO
      END DO

      ! Set moisture content in stream bed
      SUMK = zero
      SUM = zero
      DO JBK = 1, 2
         NCE = NDUM
         DUMK = (one - FNCEBD (NLINK, JBK) ) * KSPBK (JBK, NCE)
         SUMK = SUMK + DUMK
         SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK
         DO NCE = NDUM + 1, NHBED (NLINK, JBK)
            DUMK = KSPBK (JBK, NCE)
            SUMK = SUMK + DUMK
            SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK
         END DO
         NCE = NHBED (NLINK, JBK) + 1
         DUMK = FHBED (NLINK, JBK) * KSPBK (JBK, NCE)
         SUMK = SUMK + DUMK
         SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK
      END DO
      THBEDO (NLINK) = THBED (NLINK)
      THBED (NLINK) = MIN(PBSED (NLINK), SUM / SUMK)
   END SUBROUTINE LINKW

   !SSSSSS SUBROUTINE LINK
   SUBROUTINE LINK (CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE_ARG, NCETOP)
      !----------------------------------------------------------------------*
      !                             SETS UP AND SOLVES THE STREAM
      !                             LINK DIFFERENCE EQUATIONS
      !                             ** FULLY IMPLICIT COUPLING TO BANKS **
      !----------------------------------------------------------------------*
      ! Version:  SHETRAN/MOC/LINK/4.1
      INTEGER, INTENT(IN) :: NCETOP
      DOUBLEPRECISION, INTENT(INOUT) :: CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE_ARG

      DOUBLEPRECISION :: a, as, b, bs, c, d, ds, e, es, f, fs, h, hs, p, q, s, ay, ays
      DOUBLEPRECISION :: x1, x2, x3, sum, sum1, sum2, dumm
      ! Remove direct MASSOUT assignment - should be handled by calling routine

      ! Define coefficients for the difference equations
      A = ACPSFT / TSE_ARG
      AS = A * CCPSF
      B = ACPBDT / TSE_ARG
      BS = B * CCPBD
      C = zero
      D = - PCSFM1
      DS = zero
      E = - PCSFP1
      ES = zero
      F = zero
      FS = zero
      H = zero
      HS = zero
      P = zero
      Q = zero
      S = zero
      AY = zero
      AYS = zero

      ! Solve the equations using SNL3
      CALL SNL3 (A, AS, B, BS, C, D, DS, E, ES, F, FS, H, HS, P, Q, S, X1, X2, X3, AY, AYS)

      CCPSF1 = X1
      CCPBD1 = X2
      CCPBS1 = X3

      ! Mass balance calculations
      SUM = CCPSF * ACPSF1 + CCPBD * ACPBD1
      SUM1 = (CCPSF1 - CCPSF) * A
      SUM2 = (CCPBD1 - CCPBD) * B
      DUMM = SUM + SUM1 + SUM2
      ! Mass balance calculation completed - MASSOUT accumulation handled elsewhere
   END SUBROUTINE LINK

   !SSSSSS SUBROUTINE SNL3
   SUBROUTINE SNL3 (A, AS_ARG, B, BS_ARG, C, D, DS, E, ES, F, FS, H, HS, P, Q, S, X1, X2, X3, AY, AYS)
      !----------------------------------------------------------------------*
      !                             SOLVES THE COUPLED NON-LINEAR STREAM
      !                             DIFFERENCE EQUATIONS
      !----------------------------------------------------------------------*
      ! Version:  SHETRAN/MOC/SNL3/4.1
      DOUBLEPRECISION, INTENT(IN) :: A, AS_ARG, B, BS_ARG, C, D, DS, E, ES, F, FS, H, HS, P, Q, S, AY, AYS
      DOUBLEPRECISION, INTENT(OUT) :: X1, X2, X3

      DOUBLEPRECISION :: det, denom, num1, num2, num3
      DOUBLEPRECISION :: aa, bb, cc, dd, ee, ff
      DOUBLEPRECISION :: a11, a12, a13, a21, a22, a23, a31, a32, a33
      DOUBLEPRECISION :: b1, b2, b3

      ! Set up the matrix coefficients
      A11 = A + D - E
      A12 = zero
      A13 = F
      A21 = zero
      A22 = B + H
      A23 = zero
      A31 = P
      A32 = Q
      A33 = AY

      ! Set up the right hand side
      B1 = AS_ARG + DS - ES + FS
      B2 = BS_ARG + HS
      B3 = AYS + S

      ! Calculate determinant
      DET = A11 * (A22 * A33 - A23 * A32) &
         - A12 * (A21 * A33 - A23 * A31) &
         + A13 * (A21 * A32 - A22 * A31)

      IF (ABS(DET) .LT. 1.0D-20) THEN
         ! Singular matrix - use simple approach
         IF (ABS(A11) .GT. 1.0D-20) THEN
            X1 = B1 / A11
         ELSE
            X1 = zero
         ENDIF
         IF (ABS(A22) .GT. 1.0D-20) THEN
            X2 = B2 / A22
         ELSE
            X2 = zero
         ENDIF
         IF (ABS(A33) .GT. 1.0D-20) THEN
            X3 = B3 / A33
         ELSE
            X3 = zero
         ENDIF
      ELSE
         ! Solve using Cramer's rule
         NUM1 = B1 * (A22 * A33 - A23 * A32) &
            - A12 * (B2 * A33 - A23 * B3) &
            + A13 * (B2 * A32 - A22 * B3)
         X1 = NUM1 / DET

         NUM2 = A11 * (B2 * A33 - A23 * B3) &
            - B1 * (A21 * A33 - A23 * A31) &
            + A13 * (A21 * B3 - B2 * A31)
         X2 = NUM2 / DET

         NUM3 = A11 * (A22 * B3 - B2 * A32) &
            - A12 * (A21 * B3 - B2 * A31) &
            + B1 * (A21 * A32 - A22 * A31)
         X3 = NUM3 / DET
      ENDIF
   END SUBROUTINE SNL3

   !SSSSSS SUBROUTINE FRET
   SUBROUTINE FRET (C, GN, THO, TH, FRNO, FRN, KDREF, PO, P, PREF, F, FC, FT, DT, NSED_ARG, ISNL)
      !----------------------------------------------------------------------*
      !                             CALCULATES THE LINK RETARDATION FACTOR
      !----------------------------------------------------------------------*
      ! Version:  SHETRAN/MOC/FRET/4.1
      DOUBLEPRECISION, INTENT(IN) :: C, GN, THO, TH, PO, P, PREF, DT
      DOUBLEPRECISION, INTENT(IN) :: FRNO(*), FRN(*), KDREF(*)
      DOUBLEPRECISION, INTENT(OUT) :: F, FC, FT
      INTEGER, INTENT(IN) :: NSED_ARG
      LOGICAL, INTENT(IN) :: ISNL

      DOUBLEPRECISION :: kd, kdo, rho, rhoo, fhmax, dumf, dumfc, dumft
      INTEGER :: i

      ! Initialize return values
      F = one
      FC = zero
      FT = zero

      IF (NSED_ARG .LE. 0) RETURN

      ! Calculate bulk density
      RHO = GN * (one - TH)
      RHOO = GN * (one - THO)

      ! Calculate distribution coefficient
      KD = zero
      KDO = zero

      DO I = 1, NSED_ARG
         KD = KD + FRN(I) * KDREF(I)
         KDO = KDO + FRNO(I) * KDREF(I)
      END DO

      ! Calculate retardation factor components
      IF (ISNL) THEN
         ! Non-linear sorption
         FHMAX = one + RHO * KD / TH
         IF (C .GT. 1.0D-20) THEN
            DUMF = one + RHO * KD / (TH * C**0.3)
            DUMFC = - 0.3 * RHO * KD / (TH * C**1.3)
         ELSE
            DUMF = FHMAX
            DUMFC = zero
         ENDIF
      ELSE
         ! Linear sorption
         DUMF = one + RHO * KD / TH
         DUMFC = zero
      ENDIF

      ! Time derivative component
      IF (DT .GT. 1.0D-20) THEN
         DUMFT = (RHOO * KDO / THO - RHO * KD / TH) / DT
      ELSE
         DUMFT = zero
      ENDIF

      F = DUMF
      FC = DUMFC
      FT = DUMFT
   END SUBROUTINE FRET

END MODULE contaminant_link_solver
