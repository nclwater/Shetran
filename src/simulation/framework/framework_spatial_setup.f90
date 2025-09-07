MODULE framework_spatial_setup
! Module for framework spatial setup functionality
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

   PUBLIC :: FRDIM, FRIND

CONTAINS

   SUBROUTINE FRIND (BINFRP)
!----------------------------------------------------------------------*
!
! SUBROUTINE TO SET UP INDEX ARRAY FOR CONTAMINANT MIGRATION
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/FR/FRIND/4.2
! Modifications:
! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.
! RAH  970223  4.1  Explicit typing.
!      970523       Amend header.
! RAH  980713  4.2  Don't INCLUDE SPEC.OC.
!      980717       (Amend Version.)
!----------------------------------------------------------------------*
! Commons and constants




! Imported constants
!     SPEC.AL:         NELEE
! Input common
!     SPEC.AL:         NX,NY
!                      INGRID(NXEE,NY),LCODEX(NXEE,NY),LCODEY(NXEE,NY)
!                      BEXBK,BEXOC
! Output common
!     SPEC.AL:         NEL,NGDBGN,NLF
!                      ICMBK(NLFEE,2),ICMREF(NELEE,12),ICMRF2(NLFEE,6)
!                      ICMXY(NXEE,NY),NBFACE(NELEE),   NGRID(NELEE)
!                      LINKNS(NLFEE)
! Input arguments

      LOGICAL :: BINFRP
! Locals, etc
      LOGICAL :: NSOUTH, EWEST
      PARAMETER (NSOUTH = .TRUE., EWEST = .FALSE.)
      INTEGER :: I, IBANK, ICOUNT, IM1, IN1, INDEX, INDEX2, INEXT1, IP1
      INTEGER :: ITYPE, J, J1, J2, JM1, JN2, JNEXT1, JP1, K, L, L1
      INTEGER :: NEL2, NNODE3, NNODE4
      LOGICAL :: SINGLE
      LOGICAL :: found_match, found_inner_match  ! For GOTO replacement

      CHARACTER (LEN=2) :: PDIRN
!----------------------------------------------------------------------*
!
! ^^^^^^^^^^^^ INITIALISE ARRAY AND INDEX NUMBER
!
      DO I = 1, NELEE
         NGRID (I) = 0
         NBFACE (I) = 0
         DO K = 1, 12
            ICMREF (I, K) = 0
         END DO
      END DO
!
      INDEX = 0
      INDEX2 = 0
!
! ^^^^^^^^^^^^ SET UP INDEX NUMBERS
!
! --- CHANNEL LINKS
!
      DO J = 1, NY
!
         DO I = 1, NX
            IF (LCODEY (I, J) .GE.4) THEN
               INDEX = INDEX + 1
               ICMREF (INDEX, 1) = 3
               ICMREF (INDEX, 2) = I
               ICMREF (INDEX, 3) = J
               ICMREF (INDEX, 4) = INDEX
               LINKNS (INDEX) = .FALSE.
            ENDIF
         END DO
!
         DO I = 1, NX
            IF (LCODEX (I, J) .GE.4) THEN
               INDEX = INDEX + 1
               ICMREF (INDEX, 1) = 3
               ICMREF (INDEX, 2) = I
               ICMREF (INDEX, 3) = J
               ICMREF (INDEX, 4) = INDEX
               LINKNS (INDEX) = .TRUE.
            ENDIF
         END DO
!
      END DO
!
      total_no_links = INDEX
!
! --- BANK ELEMENTS
!
      IF (BEXBK.AND.total_no_links.GT.0) THEN
!
         DO 230 IBANK = 1, 2
            DO 220 L = 1, total_no_links
!
               INDEX = INDEX + 1
               ICMREF (INDEX, 1) = IBANK
               ICMREF (INDEX, 2) = ICMREF (L, 2)
               ICMREF (INDEX, 3) = ICMREF (L, 3)
               ICMREF (INDEX, 4) = L
               ICMBK (L, IBANK) = INDEX
!
220         END DO
230      END DO
!
      ENDIF
!
! --- GRID CODES
!
250   DO J = 1, NY
         DO I = 1, NX
            IF (INGRID (I, J) .GE.0) THEN
               INDEX = INDEX + 1
               ICMREF (INDEX, 2) = I
               ICMREF (INDEX, 3) = J
               ICMXY (I, J) = INDEX
            ENDIF
         END DO
      END DO
!
      NGDBGN = total_no_links + 1
      total_no_elements = INDEX
!
! ^^^^^^^^^^^^ SET UP ADJACENT NODES
!
      DO 600 INDEX = 1, total_no_elements
!
         ITYPE = ICMREF (INDEX, 1)
         I = ICMREF (INDEX, 2)
         J = ICMREF (INDEX, 3)
         L = ICMREF (INDEX, 4)
         IP1 = I + 1
         JP1 = J + 1
         IM1 = I - 1
         JM1 = J - 1
!
! --- GRID SQUARE
!
         IF (ITYPE.EQ.0) THEN
!
! FACE 1 (EAST)
!
            IF (BEXOC.AND.LCODEX (I + 1, J) .GE.4) THEN
               L = LINKNO (IP1, J, NSOUTH)
               IF (BEXBK) THEN
                  ICMREF (INDEX, 5) = ICMBK (L, 2)
               ELSE
                  ICMREF (INDEX, 5) = L
                  ICMREF (INDEX, 4) = 9999
               ENDIF
            ELSE
               IF (INGRID (I + 1, J) .GE.0) ICMREF (INDEX, 5) = ICMXY ( &
                  I + 1, J)
            ENDIF
!
! FACE 2 (NORTH)
!
            IF (BEXOC.AND.LCODEY (I, J + 1) .GE.4) THEN
               L = LINKNO (I, JP1, EWEST)
               IF (BEXBK) THEN
                  ICMREF (INDEX, 6) = ICMBK (L, 2)
               ELSE
                  ICMREF (INDEX, 6) = L
                  ICMREF (INDEX, 4) = 9999
               ENDIF
            ELSE
               IF (INGRID (I, J + 1) .GE.0) ICMREF (INDEX, 6) = ICMXY ( &
                  I, J + 1)
            ENDIF
!
! FACE 3 (WEST)
!
            IF (BEXOC.AND.LCODEX (I, J) .GE.4) THEN
               L = LINKNO (I, J, NSOUTH)
               IF (BEXBK) THEN
                  ICMREF (INDEX, 7) = ICMBK (L, 1)
               ELSE
                  ICMREF (INDEX, 7) = L
                  ICMREF (INDEX, 4) = 9999
               ENDIF
            ELSE
               IF (INGRID (I - 1, J) .GE.0) ICMREF (INDEX, 7) = ICMXY ( &
                  I - 1, J)
            ENDIF
!
! FACE 4 (SOUTH)
!
            IF (BEXOC.AND.LCODEY (I, J) .GE.4) THEN
               L = LINKNO (I, J, EWEST)
               IF (BEXBK) THEN
                  ICMREF (INDEX, 8) = ICMBK (L, 1)
               ELSE
                  ICMREF (INDEX, 8) = L
                  ICMREF (INDEX, 4) = 9999
               ENDIF
            ELSE
               IF (INGRID (I, J - 1) .GE.0) ICMREF (INDEX, 8) = ICMXY ( &
                  I, J - 1)
            ENDIF
!
! --- CHANNEL LINK
!
         ELSEIF (ITYPE.EQ.3) THEN
!
! FACE 1 (EAST)
!
            IF (LINKNS (L) ) THEN
               IF (BEXBK) THEN
                  ICMREF (INDEX, 5) = ICMBK (L, 1)
               ELSE
                  IF (INGRID (I, J) .GE.0) ICMREF (INDEX, 5) = ICMXY (I, &
                     J)
               ENDIF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEX (I + 1, J) .GE.4) ICOUNT = ICOUNT + 1
               IF (LCODEY (I + 1, J) .GE.4) ICOUNT = ICOUNT + 1
               IF (LCODEX (I + 1, J - 1) .GE.4) ICOUNT = ICOUNT + 1
               IF (ICOUNT.GT.1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF (INDEX, 5) = - INDEX2
               ENDIF
               IF (LCODEX (I + 1, J) .GE.4) THEN
                  L1 = LINKNO (IP1, J, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 5) = L1
                  ELSE
                     ICMRF2 (INDEX2, 1) = L1
                  ENDIF
               ENDIF
               IF (LCODEY (I + 1, J) .GE.4) THEN
                  L1 = LINKNO (IP1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 5) = L1
                  ELSE
                     ICMRF2 (INDEX2, 2) = L1
                  ENDIF
               ENDIF
               IF (LCODEX (I + 1, J - 1) .GE.4) THEN
                  L1 = LINKNO (IP1, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 5) = L1
                  ELSE
                     ICMRF2 (INDEX2, 3) = L1
                  ENDIF
               ENDIF
            ENDIF
!
! FACE 2 (NORTH)
!
            IF (.NOT.LINKNS (L) ) THEN
               IF (BEXBK) THEN
                  ICMREF (INDEX, 6) = ICMBK (L, 1)
               ELSE
                  IF (INGRID (I, J) .GE.0) ICMREF (INDEX, 6) = ICMXY (I, &
                     J)
               ENDIF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEY (I - 1, J + 1) .GE.4) ICOUNT = ICOUNT + 1
               IF (LCODEX (I, J + 1) .GE.4) ICOUNT = ICOUNT + 1
               IF (LCODEY (I, J + 1) .GE.4) ICOUNT = ICOUNT + 1
               IF (ICOUNT.GT.1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF (INDEX, 6) = - INDEX2
               ENDIF
               IF (LCODEY (I - 1, J + 1) .GE.4) THEN
                  L1 = LINKNO (IM1, JP1, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 6) = L1
                  ELSE
                     ICMRF2 (INDEX2, 1) = L1
                  ENDIF
               ENDIF
               IF (LCODEX (I, J + 1) .GE.4) THEN
                  L1 = LINKNO (I, JP1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 6) = L1
                  ELSE
                     ICMRF2 (INDEX2, 2) = L1
                  ENDIF
               ENDIF
               IF (LCODEY (I, J + 1) .GE.4) THEN
                  L1 = LINKNO (I, JP1, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 6) = L1
                  ELSE
                     ICMRF2 (INDEX2, 3) = L1
                  ENDIF
               ENDIF
            ENDIF
!
! FACE 3 (WEST)
!
            IF (LINKNS (L) ) THEN
               IF (BEXBK) THEN
                  ICMREF (INDEX, 7) = ICMBK (L, 2)
               ELSE
                  IF (INGRID (I - 1, J) .GE.0) ICMREF (INDEX, 7) &
                     = ICMXY (I - 1, J)
               ENDIF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEX (I, J - 1) .GE.4) ICOUNT = ICOUNT + 1
               IF (LCODEY (I - 1, J) .GE.4) ICOUNT = ICOUNT + 1
               IF (LCODEX (I, J) .GE.4) ICOUNT = ICOUNT + 1
               IF (ICOUNT.GT.1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF (INDEX, 7) = - INDEX2
               ENDIF
               IF (LCODEX (I, J - 1) .GE.4) THEN
                  L1 = LINKNO (I, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 7) = L1
                  ELSE
                     ICMRF2 (INDEX2, 1) = L1
                  ENDIF
               ENDIF
               IF (LCODEY (I - 1, J) .GE.4) THEN
                  L1 = LINKNO (IM1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 7) = L1
                  ELSE
                     ICMRF2 (INDEX2, 2) = L1
                  ENDIF
               ENDIF
               IF (LCODEX (I, J) .GE.4) THEN
                  L1 = LINKNO (I, J, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 7) = L1
                  ELSE
                     ICMRF2 (INDEX2, 3) = L1
                  ENDIF
               ENDIF
            ENDIF
!
! FACE 4 (SOUTH)
!
            IF (.NOT.LINKNS (L) ) THEN
               IF (BEXBK) THEN
                  ICMREF (INDEX, 8) = ICMBK (L, 2)
               ELSE
                  IF (INGRID (I, J - 1) .GE.0) ICMREF (INDEX, 8) &
                     = ICMXY (I, J - 1)
               ENDIF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEY (I, J) .GE.4) ICOUNT = ICOUNT + 1
               IF (LCODEX (I, J - 1) .GE.4) ICOUNT = ICOUNT + 1
               IF (LCODEY (I - 1, J) .GE.4) ICOUNT = ICOUNT + 1
               IF (ICOUNT.GT.1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF (INDEX, 8) = - INDEX2
               ENDIF
               IF (LCODEY (I, J) .GE.4) THEN
                  L1 = LINKNO (I, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 8) = L1
                  ELSE
                     ICMRF2 (INDEX2, 1) = L1
                  ENDIF
               ENDIF
               IF (LCODEX (I, J - 1) .GE.4) THEN
                  L1 = LINKNO (I, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 8) = L1
                  ELSE
                     ICMRF2 (INDEX2, 2) = L1
                  ENDIF
               ENDIF
               IF (LCODEY (I - 1, J) .GE.4) THEN
                  L1 = LINKNO (IM1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 8) = L1
                  ELSE
                     ICMRF2 (INDEX2, 3) = L1
                  ENDIF
               ENDIF
            ENDIF
!
! --- BANK ELEMENT
!
         ELSE
!
! FACE 1 (EAST)
!
            IF (LINKNS (L) ) THEN
               IF (ITYPE.EQ.1) THEN
                  IF (INGRID (I, J) .GE.0) ICMREF (INDEX, 5) = ICMXY (I, &
                     J)
               ELSE
                  ICMREF (INDEX, 5) = L
               ENDIF
            ELSE
               IF (ITYPE.EQ.1) THEN
                  IF (LCODEX (I + 1, J) .GE.4) THEN
                     L1 = LINKNO (IP1, J, NSOUTH)
                     ICMREF (INDEX, 5) = ICMBK (L1, 2)
                  ELSEIF (LCODEY (I + 1, J) .GE.4) THEN
                     L1 = LINKNO (IP1, J, EWEST)
                     ICMREF (INDEX, 5) = ICMBK (L1, 1)
                  ELSEIF (LCODEX (I + 1, J - 1) .GE.4) THEN
                     L1 = LINKNO (IP1, JM1, NSOUTH)
                     ICMREF (INDEX, 5) = ICMBK (L1, 1)
                  ENDIF
               ELSE
                  IF (LCODEX (I + 1, J - 1) .GE.4) THEN
                     L1 = LINKNO (IP1, JM1, NSOUTH)
                     ICMREF (INDEX, 5) = ICMBK (L1, 2)
                  ELSEIF (LCODEY (I + 1, J) .GE.4) THEN
                     L1 = LINKNO (IP1, J, EWEST)
                     ICMREF (INDEX, 5) = ICMBK (L1, 2)
                  ELSEIF (LCODEX (I + 1, J) .GE.4) THEN
                     L1 = LINKNO (IP1, J, NSOUTH)
                     ICMREF (INDEX, 5) = ICMBK (L1, 1)
                  ENDIF
               ENDIF
            ENDIF
!
! FACE 2 (NORTH)
!
            IF (.NOT.LINKNS (L) ) THEN
               IF (ITYPE.EQ.1) THEN
                  IF (INGRID (I, J) .GE.0) ICMREF (INDEX, 6) = ICMXY (I, &
                     J)
               ELSE
                  ICMREF (INDEX, 6) = L
               ENDIF
            ELSE
               IF (ITYPE.EQ.1) THEN
                  IF (LCODEY (I, J + 1) .GE.4) THEN
                     L1 = LINKNO (I, JP1, EWEST)
                     ICMREF (INDEX, 6) = ICMBK (L1, 2)
                  ELSEIF (LCODEX (I, J + 1) .GE.4) THEN
                     L1 = LINKNO (I, JP1, NSOUTH)
                     ICMREF (INDEX, 6) = ICMBK (L1, 1)
                  ELSEIF (LCODEY (I - 1, J + 1) .GE.4) THEN
                     L1 = LINKNO (IM1, JP1, EWEST)
                     ICMREF (INDEX, 6) = ICMBK (L1, 1)
                  ENDIF
               ELSE
                  IF (LCODEY (I - 1, J + 1) .GE.4) THEN
                     L1 = LINKNO (IM1, JP1, EWEST)
                     ICMREF (INDEX, 6) = ICMBK (L1, 2)
                  ELSEIF (LCODEX (I, J + 1) .GE.4) THEN
                     L1 = LINKNO (I, JP1, NSOUTH)
                     ICMREF (INDEX, 6) = ICMBK (L1, 2)
                  ELSEIF (LCODEY (I, J + 1) .GE.4) THEN
                     L1 = LINKNO (I, JP1, EWEST)
                     ICMREF (INDEX, 6) = ICMBK (L1, 1)
                  ENDIF
               ENDIF
            ENDIF
!
! FACE 3 (WEST)
!
            IF (LINKNS (L) ) THEN
               IF (ITYPE.EQ.1) THEN
                  ICMREF (INDEX, 7) = L
               ELSE
                  IF (INGRID (I - 1, J) .GE.0) ICMREF (INDEX, 7) &
                     = ICMXY (I - 1, J)
               ENDIF
            ELSE
               IF (ITYPE.EQ.1) THEN
                  IF (LCODEX (I, J) .GE.4) THEN
                     L1 = LINKNO (I, J, NSOUTH)
                     ICMREF (INDEX, 7) = ICMBK (L1, 1)
                  ELSEIF (LCODEY (I - 1, J) .GE.4) THEN
                     L1 = LINKNO (IM1, J, EWEST)
                     ICMREF (INDEX, 7) = ICMBK (L1, 1)
                  ELSEIF (LCODEX (I, J - 1) .GE.4) THEN
                     L1 = LINKNO (I, JM1, NSOUTH)
                     ICMREF (INDEX, 7) = ICMBK (L1, 2)
                  ENDIF
               ELSE
                  IF (LCODEX (I, J - 1) .GE.4) THEN
                     L1 = LINKNO (I, JM1, NSOUTH)
                     ICMREF (INDEX, 7) = ICMBK (L1, 1)
                  ELSEIF (LCODEY (I - 1, J) .GE.4) THEN
                     L1 = LINKNO (IM1, J, EWEST)
                     ICMREF (INDEX, 7) = ICMBK (L1, 2)
                  ELSEIF (LCODEX (I, J) .GE.4) THEN
                     L1 = LINKNO (I, J, NSOUTH)
                     ICMREF (INDEX, 7) = ICMBK (L1, 2)
                  ENDIF
               ENDIF
            ENDIF
!
! FACE 4 (SOUTH)
!
            IF (.NOT.LINKNS (L) ) THEN
               IF (ITYPE.EQ.1) THEN
                  ICMREF (INDEX, 8) = L
               ELSE
                  IF (INGRID (I, J - 1) .GE.0) ICMREF (INDEX, 8) &
                     = ICMXY (I, J - 1)
               ENDIF
            ELSE
               IF (ITYPE.EQ.1) THEN
                  IF (LCODEY (I, J) .GE.4) THEN
                     L1 = LINKNO (I, J, EWEST)
                     ICMREF (INDEX, 8) = ICMBK (L1, 1)
                  ELSEIF (LCODEX (I, J - 1) .GE.4) THEN
                     L1 = LINKNO (I, JM1, NSOUTH)
                     ICMREF (INDEX, 8) = ICMBK (L1, 1)
                  ELSEIF (LCODEY (I - 1, J) .GE.4) THEN
                     L1 = LINKNO (IM1, J, EWEST)
                     ICMREF (INDEX, 8) = ICMBK (L1, 2)
                  ENDIF
               ELSE
                  IF (LCODEY (I - 1, J) .GE.4) THEN
                     L1 = LINKNO (IM1, J, EWEST)
                     ICMREF (INDEX, 8) = ICMBK (L1, 1)
                  ELSEIF (LCODEX (I, J - 1) .GE.4) THEN
                     L1 = LINKNO (I, JM1, NSOUTH)
                     ICMREF (INDEX, 8) = ICMBK (L1, 2)
                  ELSEIF (LCODEY (I, J) .GE.4) THEN
                     L1 = LINKNO (I, J, EWEST)
                     ICMREF (INDEX, 8) = ICMBK (L1, 2)
                  ENDIF
               ENDIF
            ENDIF
!
         ENDIF
!
600   END DO
!
      NEL2 = INDEX2
!
! ^^^^^^^^^^^^ CHECK INDEX ARRAY FOR CONSISTENCY, AND SET UP
!              ADJACENT FACES (ICMREF(9-12))
! (FOR NORMAL ELEMENTS, CHECK THAT THE ADJACENT ELEMENT POINTS BACK
!  TO THE CURRENT ELEMENT.
!  FOR MULTIPLE CHANNEL LINKS AT A NODE, CHECK THAT EACH LINK
!  POINTS BACK TO THE CURRENT ELEMENT)
!
      ICOUNT = 0
      NNODE3 = 0
      NNODE4 = 0
      DO 700 INDEX = 1, total_no_elements
!
         DO 650 I = 1, 4
            INEXT1 = ICMREF (INDEX, I + 4)
            IF (INEXT1.GT.0) THEN
               found_match = .FALSE.
               DO J = 1, 4
                  IF (ICMREF (INEXT1, J + 4) .EQ.INDEX) THEN
                     ICMREF (INDEX, I + 8) = J
                     found_match = .TRUE.
                     EXIT
                  ENDIF
               END DO
               IF (.NOT. found_match) THEN
                  WRITE(PPPRI, 1100) INDEX, I
                  ICOUNT = ICOUNT + 1
               END IF
            ELSEIF (INEXT1.LT.0) THEN
               IF (ICMRF2 ( - INEXT1, 1) .EQ.0.OR.ICMRF2 ( - INEXT1, 2) &
                  .EQ.0.OR.ICMRF2 ( - INEXT1, 3) .EQ.0) THEN
                  NNODE3 = NNODE3 + 1
               ELSE
                  NNODE4 = NNODE4 + 1
               ENDIF
               DO 640 J1 = 1, 3
                  IN1 = ICMRF2 ( - INEXT1, J1)
                  IF (IN1.GT.0) THEN
                     found_inner_match = .FALSE.
                     DO J = 1, 4
                        JNEXT1 = ICMREF (IN1, J + 4)
                        IF (JNEXT1.LT.0) THEN
                           DO J2 = 1, 3
                              JN2 = ICMRF2 ( - JNEXT1, J2)
                              IF (JN2.EQ.INDEX) THEN
                                 ICMRF2 ( - INEXT1, J1 + 3) = J
                                 found_inner_match = .TRUE.
                                 EXIT
                              ENDIF
                           END DO
                           IF (found_inner_match) EXIT
                        ENDIF
                     END DO
                     IF (.NOT. found_inner_match) THEN
                        WRITE(PPPRI, 1100) INDEX, I
                        ICOUNT = ICOUNT + 1
                     END IF
                  ENDIF
640            END DO
            ELSE
               ICMREF (INDEX, I + 8) = I
!
               IF (ITYPE.LT.3.AND.NBFACE (INDEX) .EQ.0) NBFACE (INDEX) &
                  = I
!
            ENDIF
650      END DO
!
700   END DO
!
      IF (ICOUNT.GT.0) WRITE(PPPRI, 1200) ICOUNT
!
! ^^^^^^^^^^^^ WRITE OUT INDEX ARRAY, IF REQUIRED
!
      IF (BINFRP) THEN
!
         WRITE(PPPRI, 1300) total_no_elements
         DO 800 INDEX = 1, total_no_elements
            PDIRN = ' '
            ITYPE = ICMREF (INDEX, 1)
            IF (ITYPE.GT.0) THEN
               L = ICMREF (INDEX, 4)
               IF (LINKNS (L) ) THEN
                  PDIRN = 'NS'
               ELSE
                  PDIRN = 'EW'
               ENDIF
            ENDIF
            WRITE(PPPRI, 1400) INDEX, (ICMREF (INDEX, K), K = 1, 4), &
               PDIRN, (ICMREF (INDEX, K), K = 5, 8)
800      END DO
!
         IF (NEL2.GT.0) THEN
            WRITE(PPPRI, 1500) NNODE3 / 3, NNODE4 / 4, NEL2
            DO 900 INDEX2 = 1, NEL2
               WRITE(PPPRI, 1600) INDEX2, (ICMRF2 (INDEX2, I), I = 1, 3)
900         END DO
         ENDIF
!
      ENDIF
!
! FORMAT STATEMENTS
!
1100  FORMAT(' INCONSISTENCY FOUND AT INDEX:',I4,' FACE:',I2)
!
1200  FORMAT(/  I4,' INCONSISTENCIES FOUND IN INDEX ARRAY' /)
!
1300  FORMAT(' ', / 'INDEX ARRAY: NO. OF ELEMENTS = ',I6, // &
      &       ' ','     INDEX      TYPE         X         Y      LINK   ', &
      &       '  FACE1     FACE2     FACE3     FACE4' / &
      &       ' ','     -----      ----         -         -      ----   ', &
      &       '  -----     -----     -----     -----' )
!
1400  FORMAT(' ',5(4X,I6),1X,A2,1X,I6,3(4X,I6))
!
1500  FORMAT(' '/'AUXILIARY INDEX ARRAY FOR CHANNEL NODES: ',/ &
      &           'NO. OF NODES WITH 3 BRANCHES = ',I4,/ &
      &           'NO. OF NODES WITH 4 BRANCHES = ',I4,/ &
      &           'TOTAL NO. OF INDICES         = ',I4 // &
      &       ' ','   INDEX  LINK 1  LINK 2  LINK 3' / &
      &       ' ','   -----  ------  ------  ------' )
!
1600  FORMAT(' ',5(4X,I4))
!
   END SUBROUTINE FRIND

   SUBROUTINE FRDIM (BINFRP)
!----------------------------------------------------------------------*
!
! SET UP ELEMENT DIMENSIONS AND AREAS, AND TOTAL CATCHMENT AREA
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/FR/FRDIM/4.1
! Modifications:
! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.
! RAH  970223  4.1  Explicit typing.
!----------------------------------------------------------------------*
! Commons and constants
!
! Imported constants
!     SPEC.AL:         NELEE,NLFEE,NXEE,NYEE
!
! Input common
!     SPEC.AL:         NEL,NX,NXM1,NY,NYM1,PRI,ICMREF(NELEE,8)
!                      CWIDTH(NLFEE),DXIN(NXM1),DY(NYM1)
!                      LINKNS(NLFEE)
!
! Output common
!     SPEC.AL:         CAREA,AREA(NEL),DHF(NELEE,4),DXQQ(NEL),DYQQ(NEL)
!                      BWIDTH
!
! Input arguments
      LOGICAL :: BINFRP
!
! Locals, etc
      INTEGER :: I1, I2, IEL, IFACE, IL, IL1, IL2, INEXT1, INEXT2, &
         ITYPE
      INTEGER :: IX, IY, J, JEL, JL, JTYPE, K
      DOUBLEPRECISION CATEST, DIFF, DX (NXEE), DY (NYEE)
!
!----------------------------------------------------------------------*
!
! SET VALUE FOR BANK ELEMENT WIDTH
! (CURRENTLY HARD-CODED AS A FIXED WIDTH)
!
      BWIDTH = 10.0
!
! --- CALCULATE DX AND DY FROM DXIN AND DYIN
!
      DX (1) = DXIN (1)
      DX (NX) = DXIN (NXM1)
      DO J = 2, NXM1
         DX (J) = (DXIN (J - 1) + DXIN (J) ) * 0.5
      END DO
      DY (1) = DYIN (1)
      DY (NY) = DYIN (NYM1)
      DO K = 2, NYM1
         DY (K) = (DYIN (K - 1) + DYIN (K) ) * 0.5
      END DO
!
! --- SET UP BASIC DIMENSIONS OF EACH ELEMENT
!
      DO 900 IEL = 1,total_no_elements
!
         ITYPE = ICMREF (IEL, 1)
         IX = ICMREF (IEL, 2)
         IY = ICMREF (IEL, 3)
         IL = ICMREF (IEL, 4)
!
         IF (ITYPE.EQ.0) THEN
            DXQQ (IEL) = DX (IX)
            DYQQ (IEL) = DY (IY)
         ELSEIF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
            IF (LINKNS (IL) ) THEN
               DXQQ (IEL) = BWIDTH
               DYQQ (IEL) = DY (IY)
            ELSE
               DXQQ (IEL) = DX (IX)
               DYQQ (IEL) = BWIDTH
            ENDIF
         ELSEIF (ITYPE.EQ.3) THEN
            IF (LINKNS (IEL) ) THEN
               DXQQ (IEL) = CWIDTH (IL)
               DYQQ (IEL) = DY (IY)
               CLENTH (IL) = DY (IY)
            ELSE
               DXQQ (IEL) = DX (IX)
               DYQQ (IEL) = CWIDTH (IL)
               CLENTH (IL) = DX (IX)
            ENDIF
         ENDIF
900   END DO
!
! --- CORRECT FOR OVERLAPPING ELEMENTS (NB: CHANNEL LINK OVERLAPS NOT IN
! --- AND CALCULATE ELEMENT AND CATCHMENT AREA
!
      CAREA = zero
      CATEST = zero
!
      DO 950 IEL = 1, total_no_elements
!
         ITYPE = ICMREF (IEL, 1)
         IX = ICMREF (IEL, 2)
         IY = ICMREF (IEL, 3)
         IL = ICMREF (IEL, 4)
!
         IF (ITYPE.EQ.0) THEN
!
            DO 920 I1 = 5, 8
!
! GRID ELEMENTS (REMOVE WIDTHS OF CHANNEL LINKS, AND POSSIBLY BANK ELEME
!
               INEXT1 = ICMREF (IEL, I1)
               IF (INEXT1.GT.0) THEN
                  DIFF = zero
                  IF (ICMREF (INEXT1, 1) .GT.0) THEN
                     IL = ICMREF (INEXT1, 4)
                     DIFF = DIFF + 0.5 * CWIDTH (IL)
                     IF (ICMREF (INEXT1, 1) .LT.3) DIFF = DIFF + BWIDTH
                  ENDIF
                  IF (I1.EQ.5.OR.I1.EQ.7) DXQQ (IEL) = DXQQ (IEL) &
                     - DIFF
                  IF (I1.EQ.6.OR.I1.EQ.8) DYQQ (IEL) = DYQQ (IEL) &
                     - DIFF
               ENDIF
!
! BANK ELEMENTS (REMOVE OVERLAP OF BANKS/BANKS AND BANK/CHANNEL FOR EACH
! CORNER OF EACH GRID ELEMENT)
!
               I2 = I1 + 1
               IF (I2.EQ.9) I2 = 5
               INEXT2 = ICMREF (IEL, I2)
               IF (INEXT1.GT.0.AND.INEXT2.GT.0) THEN
                  IF ( (ICMREF (INEXT1, 1) .EQ.1.OR.ICMREF (INEXT1, 1) &
                     .EQ.2) .AND. (ICMREF (INEXT2, 1) .EQ.1.OR.ICMREF ( &
                     INEXT2, 1) .EQ.2) ) THEN
                     IL1 = ICMREF (INEXT1, 4)
                     IL2 = ICMREF (INEXT2, 4)
                     IF (LINKNS (IL1) ) THEN
                        DYQQ (INEXT1) = DYQQ (INEXT1) - BWIDTH - 0.5 * &
                           CWIDTH (IL2)
                     ELSE
                        DXQQ (INEXT1) = DXQQ (INEXT1) - BWIDTH - 0.5 * &
                           CWIDTH (IL2)
                     ENDIF
                     IF (LINKNS (IL2) ) THEN
                        DYQQ (INEXT2) = DYQQ (INEXT2) - BWIDTH - 0.5 * &
                           CWIDTH (IL1)
                     ELSE
                        DXQQ (INEXT2) = DXQQ (INEXT2) - BWIDTH - 0.5 * &
                           CWIDTH (IL1)
                     ENDIF
                  ENDIF
               ENDIF
!
920         END DO
!
         ENDIF
!
! CALCULATE CATCHMENT AREA BY asumMING ALL BASIC GRID SIZES
! AND CATCHMENT AREA OBTAINED BY asumMING ALL ELEMENT AREAS (INCLUDES OVE
!
         IF (ITYPE.EQ.0) CATEST = CATEST + DX (IX) * DY (IY)
!
950   END DO
!
! --- CALCULATE AREA OF EACH ELEMENT
!
      DO 955 IEL = 1, total_no_elements
         cellarea (IEL) = DXQQ (IEL) * DYQQ (IEL)
         CAREA = CAREA + cellarea (IEL)
955   END DO
!
! --- PRINT OUT ELEMENT AREA, TOTAL CATCHMENT AREA, AND PERCENTAGE ERROR
!
      IF (BINFRP) THEN
         WRITE(PPPRI, 1500)
         DO 960 IEL = 1, total_no_elements
            WRITE(PPPRI, 1600) IEL, DXQQ (IEL), DYQQ (IEL), cellarea (IEL)
960      END DO
!
         DIFF = (CAREA - CATEST) * 100.0d0 / CAREA
         IF (CAREA.LT.1.0D6) THEN
            WRITE(PPPRI, 1700) CAREA, CATEST, DIFF
         ELSE
            WRITE(PPPRI, 1750) CAREA / 1.0D6, CATEST / 1.0D6, DIFF
         ENDIF
      ENDIF
!
! ----- SET UP SPACINGS DHF BETWEEN COMPUTATIONAL NODES AND EDGE OF ELEM
!
      DO 980 IEL = 1, total_no_elements
         ITYPE = ICMREF (IEL, 1)
         IX = ICMREF (IEL, 2)
         IY = ICMREF (IEL, 3)
         IL = ICMREF (IEL, 4)
!
! WEST FACE (FACE 3)
!
         IFACE = 3
         JEL = ICMREF (IEL, IFACE+4)
!
         IF (JEL.EQ.0) THEN
            IF (ITYPE.EQ.0) THEN
               DHF (IEL, IFACE) = 0.5 * DXIN (IX - 1)
            ELSEIF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
               DHF (IEL, IFACE) = 0.5 * BWIDTH
            ELSE
               IF (LINKNS (IEL) ) THEN
                  DHF (IEL, IFACE) = 0.5 * CWIDTH (IEL)
               ELSE
                  DHF (IEL, IFACE) = 0.5 * DXIN (IX - 1)
               ENDIF
            ENDIF
!
         ELSEIF (JEL.GT.0) THEN
            JTYPE = ICMREF (JEL, 1)
            JL = ICMREF (JEL, 4)
!
            IF (ITYPE.EQ.0) THEN
               IF (JTYPE.EQ.0) THEN
                  DHF (IEL, IFACE) = 0.5 * DXIN (IX - 1)
               ELSEIF (JTYPE.EQ.1) THEN
                  DHF (IEL, IFACE) = 0.5 * (DXIN (IX - 1) - 2 * BWIDTH - &
                     CWIDTH (JL) )
               ELSEIF (JTYPE.EQ.3) THEN
                  DHF (IEL, IFACE) = 0.5 * (DXIN (IX - 1) - CWIDTH (JL) &
                     )
               ENDIF
!
            ELSEIF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
               IF (JTYPE.EQ.0) THEN
                  DHF (IEL, IFACE) = 0.5 * BWIDTH
               ELSEIF (JTYPE.EQ.1.OR.JTYPE.EQ.2) THEN
                  DHF (IEL, IFACE) = 0.5 * DXQQ (IEL)
               ELSE
                  DHF (IEL, IFACE) = 0.5 * BWIDTH
               ENDIF
!
            ELSE
               IF (LINKNS (IEL) ) THEN
                  DHF (IEL, IFACE) = 0.5 * CWIDTH (IEL)
               ELSE
                  DHF (IEL, IFACE) = 0.5 * DXIN (IX - 1)
               ENDIF
!
            ENDIF
!
         ELSEIF (JEL.LT.0) THEN
            IF (LINKNS (IEL) ) THEN
               DHF (IEL, IFACE) = 0.5 * CWIDTH (IEL)
            ELSE
               DHF (IEL, IFACE) = 0.5 * DXIN (IX - 1)
            ENDIF
!
         ENDIF
!
! SOUTH FACE (FACE 4)
!
         IFACE = 4
         JEL = ICMREF (IEL, IFACE+4)
!
         IF (JEL.EQ.0) THEN
            IF (ITYPE.EQ.0) THEN
               DHF (IEL, IFACE) = 0.5 * DYIN (IY - 1)
            ELSEIF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
               DHF (IEL, IFACE) = 0.5 * BWIDTH
            ELSE
               IF (LINKNS (IEL) ) THEN
                  DHF (IEL, IFACE) = 0.5 * DYIN (IY - 1)
               ELSE
                  DHF (IEL, IFACE) = 0.5 * CWIDTH (IEL)
               ENDIF
            ENDIF
!
         ELSEIF (JEL.GT.0) THEN
            JTYPE = ICMREF (JEL, 1)
            JL = ICMREF (JEL, 4)
!
            IF (ITYPE.EQ.0) THEN
               IF (JTYPE.EQ.0) THEN
                  DHF (IEL, IFACE) = 0.5 * DYIN (IY - 1)
               ELSEIF (JTYPE.EQ.1) THEN
                  DHF (IEL, IFACE) = 0.5 * (DYIN (IY - 1) - 2 * BWIDTH - &
                     CWIDTH (JL) )
               ELSEIF (JTYPE.EQ.3) THEN
                  DHF (IEL, IFACE) = 0.5 * (DYIN (IY - 1) - CWIDTH (JL) &
                     )
               ENDIF
!
            ELSEIF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
               IF (JTYPE.EQ.0) THEN
                  DHF (IEL, IFACE) = 0.5 * BWIDTH
               ELSEIF (JTYPE.EQ.1.OR.JTYPE.EQ.2) THEN
                  DHF (IEL, IFACE) = 0.5 * DYQQ (IEL)
               ELSE
                  DHF (IEL, IFACE) = 0.5 * BWIDTH
               ENDIF
!
            ELSE
               IF (LINKNS (IEL) ) THEN
                  DHF (IEL, IFACE) = 0.5 * DYIN (IY - 1)
               ELSE
                  DHF (IEL, IFACE) = 0.5 * CWIDTH (IEL)
               ENDIF
!
            ENDIF
!
         ELSEIF (JEL.LT.0) THEN
            IF (LINKNS (IEL) ) THEN
               DHF (IEL, IFACE) = 0.5 * DYIN (IY - 1)
            ELSE
               DHF (IEL, IFACE) = 0.5 * CWIDTH (IEL)
            ENDIF
!
         ENDIF
!
! EAST FACE (FACE 1)
!
         IFACE = 1
         DHF (IEL, IFACE) = DXQQ (IEL) - DHF (IEL, 3)
!
! NORTH FACE (FACE 2)
!
         IFACE = 2
         DHF (IEL, IFACE) = DYQQ (IEL) - DHF (IEL, 4)
!
980   END DO
!
      RETURN
!
! ^^^^^^^^^^^^ FORMAT STATEMENTS
!
1500  FORMAT(/ '   INDEX   DXQQ (M)   DYQQ (M)     AREA (M^^2)' /)
!
1600  FORMAT(' ',4X,I6,4X,F7.2,4X,F7.2,4X,F12.2)
!
1700  FORMAT(/ ' TOTAL CATCHMENT AREA = ',F12.3,' SQ. METRES. ' / &
      &         ' BASIC CATCHMENT AREA = ',F12.3,' SQ. METRES. ' / &
      &   ' DIFFERENCE INTRODUCED BY CHANNEL SYSTEM AND BANKS = ', &
      &   F12.3,' %' /)
!
1750  FORMAT(/ ' TOTAL CATCHMENT AREA = ',F12.3,' SQ. KM. ' / &
      &         ' BASIC CATCHMENT AREA = ',F12.3,' SQ. KM. ' / &
      &   ' DIFFERENCE INTRODUCED BY CHANNEL SYSTEM AND BANKS = ', &
      &   F12.3,' %' /)
!

   END SUBROUTINE FRDIM

END MODULE framework_spatial_setup
