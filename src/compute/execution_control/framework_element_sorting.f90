MODULE framework_element_sorting
! Module for framework element sorting functionality
! Extracted from FRmod.f90 as part of refactoring

   USE SGLOBAL
   USE CONT_CC, ONLY :    CCAPE, CCAPR, CCAPB, GNN, alphbd, alphbs, alpha, fads
   USE AL_G, ONLY :     NX, NY, ICMREF, ICMXY, NGDBGN
   USE AL_C, ONLY :     ARXL, BEXBK, BFB, BHB, BUG, CWIDTH, CLENTH, CMD, CMP, CMT, CMB,  clai, &
      DELTAZ, DRAINA, dhf, DUMMY, DTUZ, EEVAP, ESOILA, &
      FHBED, ISORT, IDUM, ICMRF2, ICMBK, JVSACN, JVSDEL, LINKNS, LFB, LHB, LGB, &
      NBFACE, NV, NLYRBT, NRD, NLYR, NHBED, NTSOIL, NVC, NVSSPC, NVSSPT, NVSWLI, NVSWLT, NWELBT, NS, NWELTP, &
      plai, PNETTO, &
      QH, QVSH, QVSSPR, QVSWEL, QVSWLI, QVSV, QOC, QBKB, QBKF, &
      RDL, RDF, SYD, SPR, &
      TIH, UZNEXT, VSPSI, VSD, VSTHE, VSI, VSPOR, WLD, WBERR, ZBEFF, ZBFULL, ZLYRBT, ZVSNOD, &
      ZVSPSL
   USE AL_D,    ONLY :  BALANC, BEXSZ, BEXEX, BEXSY, BEXCM, BEXSM, BEXOC, BEXET, BEXUZ, BKD, BHOTRD, BWIDTH, BHOTST, BHOTTI, BHOTPR,&
      CAREA, CSTORE, DIS, DIS2, DISEXTRA, DXIN, DYIN, DQ0ST, DQIST, DQIST2, DTMET3, EINTA, DTMET, DTMET2, ERZA, ETD, EPOT, &
      EPD, FRD, HOTIME, HOT, TAH, TAL, ISTA,isextradis,iszq,isextrapsl,pslextra, &
      IOCORS, ICLNUM, NCLASS, ICLIST, IODATA, IOELEM, IOSTA, IOSTEP, IOEND, IORES, IOTIME, INGRID, &
      LCODEY, LCODEX, MBLINK, MBFACE, MBFLAG, MBYEAR, MSM, MAS, MED, MBMON, MBDAY, &
      NXM1, NYM1, NRAINC, NMC, NM, NSET, NXP1, NYP1, NXE, NYE, NSMC, NGRID, NOCBCC, NOCBCD, NRAIN, NXEP1, NYEP1, &
      OCD, OFB, OHB, OCNOW, precip_m_per_s, PSTART, PRD, PPD, PMAX, PALFA, PREST, QMAX, RES, RHOSAR, RESFIL, &
      SF, SMD, SD, TIMEUZ, TS, TIM, TMAX, TTH, UZVAL, VHT, VED, VSE,TOUTPUT,zqd
   USE OCmod,    ONLY : LINKNO, OCLTL
   USE OCQDQMOD, ONLY : STRXX, STRYY
   USE UTILSMOD, ONLY : AREADR, AREADI, HOUR_FROM_DATE, DATE_FROM_HOUR
   USE mod_load_filedata,    ONLY : ALINIT, ALINTP, ALCHK, ALCHKI
   USE SMmod,    ONLY : head, binsmp, ddf, rhos, zos, zds, zus, nsd, rhodef, imet, smelt, tmelt
   USE ETmod,    ONLY : BAR, BMETP, BINETP, BMETAL, CSTCAP, CSTCA1, CK, CB, CLAI1, FET, &
      MEASPE, MODE, MODECS, MODEVH, MODEPL, MODECL, NCTCLA, NCTVHT,NCTCST, NF, NCTPLA, &
      PS1, PLAI1, RELPLA, RELCST, RA, RC, RCF, RELCLA, RELVHT, RTOP, TIMCST, TIMPLA, TIMVHT, TIMCLA,  VHT1
   USE VSmod,    ONLY : VSIN, VSPTHE, NVSSOL, VSPKR, VSPETA, VSPDTH, VSPDKR, VSPDET, VSPPSI
   USE OCmod,    ONLY : OCINI
   USE OCmod2,   ONLY : GETHRF, SETHRF, SETQSA
   USE CONST_SY, ONLY : RHOSED
   USE SED_CS,   ONLY : DLS, GNU, FBETA, FDEL, PLS, GINFD, GINFS, GNUBK, QSED, DCBED, DCBSED, ARBDEP, &
      nsed, FBTSD, QDEFF, NSOBED, PBSED, SOSDFN, sofn
   USE SED_CO,   ONLY : DLSO, GNUO, FBBEDO, FDELO, FBTSDO
   USE COLM_CG,  ONLY : ZCOLMB, NOLCE, NOLCEA, NOLBT, JOLFN, NOL, NCOLMB, JKZCOL, SCL, OODO
   USE CONT_CC,  ONLY : CCCCo, CCCC, CCCCW, SSSS, SSSSO, IIICF, CCAPIN, KDDSOL, KDDLS, GGLMSO, NCON, GCPLA, CCAPIO, CCAPI, IIICFO
   USE COLM_C1,  ONLY : Z2, D0, Z2SQ, Z2OD, Z2SQOD, SGMA, SGSQ, OMSGMA, NCETOP
   USE COLM_CO,  ONLY : DSWO, QIO, QQRFO, RSZWLO, ZONEO, QQQSWO, GGAMMO, QQO, VSTHEO, UUAJPO
   USE BK_CW,    ONLY : NBANK, NCEBD, FNCEBD, NCEAB
   USE IS_CC,    ONLY : ISPLT
   USE LINK_CW,  ONLY : DBDI, ACPBSG, DBS, ACPBI, ACPSFO, ACPBDO, THBEDO, THBED
   USE PLANT_CC, ONLY : PMASS, PF2MAX, PKMAX, NPLT, PFONE, NPLTYP, PDZF3, DELONE, NPL, GMCBBO
   USE ZQmod,    ONLY : ReadZQTable

   IMPLICIT NONE

   PUBLIC :: FRSORT

CONTAINS

   SUBROUTINE FRSORT
!
! SORT OF ALL ELEMENTS ON WATER ELEVATION (HIGHEST ELEVATION FIRST)
! OR WATER TABLE ELEVATION IF NO SURFACE WATER IS PRESENT IN A GRID SQUA
!   BANK ELEMENT
! OR CHANNEL BED ELEVATION IF NO SURFACE WATER IS PRESENT IN A CHANNEL L
!
! SURFACE WATER ELEVATIONS AND INDICES STORED IN COLUMN 1 OF ELEV AND IS
! WATER TABLE ELEVATIONS AND INDICES STORED IN COLUMN 2 OF ELEV AND ISTE
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!

!
      DOUBLEPRECISION ELEV (NELEE, 2)
      INTEGER :: ISTEMP (NELEE, 2), NSORT (2)
      INTEGER :: ns1, ns2, i, iel, itype, jel, il, L, ndum, nstart, nend, &
         jump, m, k, n, itemp, i1, i2, is
      DOUBLEPRECISION :: hsz1, hsz2, zhigh, zlow, temp
      LOGICAL :: iscycle

      IF (total_no_elements.EQ.1) RETURN
      NS1 = 0
      NS2 = 0
!
! PUT ELEVATIONS INTO LOCAL ARRAYS, DIVIDED INTO SURFACE AND WATER TABLE
!   ELEMENTS (NB. 'GHOST' PHREATIC SURFACE LEVELS ARE SET UP FOR THE CHA
!   EQUAL TO THE MAX. PHREATIC ELEVATION OF THE NEIGHBOURING ELEMENTS)
!
      DO 100 I = 1, total_no_elements
!
         IEL = ISORT (I)
         ITYPE = ICMREF (IEL, 1)
         IF (ITYPE.EQ.3) THEN
            HSZ1 = zero
            HSZ2 = zero
            IF (LINKNS (IEL) ) THEN
               JEL = ICMREF (IEL, 5)
               IF (JEL.GT.0) HSZ1 = ZVSPSL (JEL)
               JEL = ICMREF (IEL, 7)
               IF (JEL.GT.0) HSZ2 = ZVSPSL (JEL)
            ELSE
               JEL = ICMREF (IEL, 6)
               IF (JEL.GT.0) HSZ1 = ZVSPSL (JEL)
               JEL = ICMREF (IEL, 8)
               IF (JEL.GT.0) HSZ2 = ZVSPSL (JEL)
            ENDIF
            ZVSPSL (IEL) = MAX (HSZ1, HSZ2)
         ENDIF
!
         IL = ICMREF (IEL, 4)
         IF (GETHRF (IEL) - ZGRUND (IEL) .GT.1.0E-8) THEN
            NS1 = NS1 + 1
            ELEV (NS1, 1) = GETHRF (IEL)
            ISTEMP (NS1, 1) = IEL
         ELSE
            NS2 = NS2 + 1
            ELEV (NS2, 2) = ZVSPSL (IEL)
            ISTEMP (NS2, 2) = IEL
         ENDIF
100   END DO
!
      NSORT (1) = NS1
      NSORT (2) = NS2
!
! --- SORT ON WATER SURFACE ELEVATIONS, THEN WATER TABLE ELEVATIONS
!
      DO 500 L = 1, 2
         NDUM = NSORT (L)
         !
         ! - CHECK FOR START AND END OF ARRAY TO BE SORTED
         !
         ! PASS ONE (HIGHEST TO LOWEST)
         ! - FIND FIRST POINT (IF ANY) WHERE ELEVATIONS START INCREASING
         iscycle=.FALSE.
         DO I = 1, NDUM - 1
            IF(iscycle) CYCLE
            IF (ELEV (I + 1, L) .GT.ELEV (I, L) ) THEN
               NSTART = I
               iscycle=.TRUE. !GOTO 220
            ENDIF
         ENDDO
         !
         ! - IF NO INCREASING ELEVATIONS FOUND, THE ARRAY IS ALREADY SORTED
         !
         IF(.NOT.iscycle) CYCLE !GOTO 500
         !
         ! - FIND HIGHEST POINT IN REST OF ARRAY
         !
220      ZHIGH = zero
         DO 240 I = NSTART + 1, NSORT (L)
            IF (ELEV (I, L) .GT.ZHIGH) ZHIGH = ELEV (I, L)
240      ENDDO
         !
         ! - FIND POSITION IN SORTED SECTION OF ARRAY OF ELEVATION 'HIGH'
         iscycle=.FALSE.
         DO I = 1, NSTART
            IF(iscycle) CYCLE
            IF (ELEV (I, L) .LT.ZHIGH) THEN
               NSTART = I
               iscycle=.TRUE. !GOTO 300
            ENDIF
         ENDDO
         !
         ! PASS TWO (LOWEST TO HIGHEST)
         ! - FIND FIRST POINT (IF ANY) WHERE ELEVATIONS START DECREASING
         !
         !300
         iscycle=.FALSE.
         DO I = NDUM, 2, - 1
            IF(iscycle) CYCLE
            IF (ELEV (I - 1, L) .LT.ELEV (I, L) ) THEN
               NEND = I
               iscycle=.TRUE. !GOTO 320
            ENDIF
         ENDDO
         !
         ! - IF NO DECREASING ELEVATIONS FOUND, THE ARRAY IS ALREADY SORTED
         ! (NB THIS SHOULD NEVER HAPPEN, AS IT SHOULD BE CHECKED IN PASS ONE)
         !
         IF(.NOT.iscycle) CYCLE !GOTO 500
         !
         ! - FIND LOWEST POINT IN REST OF ARRAY
         !
320      ZLOW = 1.0E10
         DO I = NEND-1, 1, - 1
            IF (ELEV (I, L) .LT.ZLOW) ZLOW = ELEV (I, L)
         ENDDO
         !
         ! - FIND POSITION IN SORTED SECTION OF ARRAY OF ELEVATION 'ZLOW'
         !
         iscycle=.FALSE.
         DO I = NDUM, NEND, - 1
            IF(iscycle) CYCLE
            IF (ELEV (I, L) .GT.ZLOW) THEN
               NEND = I
               iscycle=.TRUE. !GOTO 400
            ENDIF
         ENDDO
         !
         ! --- SORT ON ARRAY BETWEEN NSTART AND NEND
         !
400      JUMP = NEND-NSTART + 1
410      JUMP = JUMP / 2
         IF (JUMP.NE.0) THEN
            DO M = NSTART, NEND-JUMP
               K = M
               DO
420               N = K + JUMP
                  IF (ELEV (K, L) .LT.ELEV (N, L) ) THEN
                     ITEMP = ISTEMP (K, L)
                     ISTEMP (K, L) = ISTEMP (N, L)
                     ISTEMP (N, L) = ITEMP
                     TEMP = ELEV (K, L)
                     ELEV (K, L) = ELEV (N, L)
                     ELEV (N, L) = TEMP
                     K = K - JUMP
                     !IF (K.GT.0) GOTO 420
                     IF(.NOT.(K.GT.0)) EXIT
                  ENDIF
                  EXIT
               ENDDO
            ENDDO
            ! Continue with next jump size
         ENDIF
         !
         ! --- ARRAY ISTEMP IS SORTED
         !
500   END DO
!
! --- REASSEMBLE ISORT ARRAY
!
      I1 = 1
      I2 = 1
      IS = 1
!
! --- MERGE TWO SORTED ARRAYS
      DO WHILE (I1.LE.NS1 .AND. I2.LE.NS2)
         IF (NS1.GT.0) THEN
            IF (NS2.EQ.0.OR.ZVSPSL (ISTEMP (I1, 1) ) .GT.ELEV (I2, 2) ) &
               THEN
               ISORT (IS) = ISTEMP (I1, 1)
               I1 = I1 + 1
               IS = IS + 1
            ELSE
               ISORT (IS) = ISTEMP (I2, 2)
               I2 = I2 + 1
               IS = IS + 1
            ENDIF
         ENDIF
      END DO
!
! --- COPY REMAINING ELEMENTS FROM FIRST ARRAY
      IF (I2.GT.NS2) THEN
         DO I = IS, total_no_elements
            ISORT (I) = ISTEMP (I1, 1)
            I1 = I1 + 1
         END DO
! --- COPY REMAINING ELEMENTS FROM SECOND ARRAY
      ELSEIF (I1.GT.NS1) THEN
         DO I = IS, total_no_elements
            ISORT (I) = ISTEMP (I2, 2)
            I2 = I2 + 1
         END DO
      ENDIF
!
700   CONTINUE
!
      RETURN
!
1000  FORMAT(' total_no_elements= ',I4,'  NS1= ',I4,' NS2= ',I4,' SFCMAX(*)= ',F7.1, &
      &       ' sfcmin=',f7.1,' SZMAX(+)= ',F7.1,' szmin=',f7.1)
1010  FORMAT(' ',I4,' ',I4,' |',A68)
!
   END SUBROUTINE FRSORT

END MODULE framework_element_sorting
