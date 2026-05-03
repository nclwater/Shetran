MODULE FRmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the FR .F files
!***ZQ Module 200520 new variables iszq,zqd
   USE SGLOBAL
   USE CONT_CC, ONLY :    CCAPE, CCAPR, CCAPB, GNN, alphbd, alphbs, alpha, fads
!USE SGLOBAL, ONLY :     NELEE, nlfee, noctab, NXEE, NYEE, NVEE, BDEVER, SHEVER, BANNER, FILNAM, DIRQQ, &
!                     VISUALISATION_PLAN_FILENAME, VISUALISATION_CHECK_FILENAME, HDF5FILENAME, CNAM, NCONEE, LLEE, NSEE
   USE AL_G, ONLY :     NX, NY, ICMREF, ICMXY, NGDBGN
   USE AL_C, ONLY :     ARXL, BEXBK, BFB, BHB, BUG, CWIDTH, CLENTH, CMD, CMP, CMT, CMB,  clai, &
      DELTAZ, DRAINA, dhf, DUMMY, DTUZ, EEVAP, ESOILA, &
      FHBED, ISORT, IDUM, ICMRF2, ICMBK, JVSACN, JVSDEL, LINKNS, LFB, LHB, LGB, &
      NBFACE, NV, NLYRBT, NRD, NLYR, NHBED, NTSOIL, NVC, NVSSPC, NVSSPT, NVSWLI, NVSWLT, NWELBT, NS, NWELTP, &
      plai, PNETTO, &
      QH, QVSH, QVSSPR, QVSWEL, QVSWLI, QVSV, QOC, QBKB, QBKF, &
      RDL, RDF, SYD, SPR, &
      TIH, UZNEXT, VSPSI, VSD, VSTHE, VSI, VSPOR, WLD, WBERR, ZBEFF, ZBFULL, ZLYRBT, ZVSNOD, &
      ZVSPSL, MND,MNFC,MNFN,MNPL,MNPR,MNOUT1,MNOUT2,MNOUTPL,INITIALISE_AL_C3
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
   USE mod_load_filedata,    ONLY : ALINTP, ALCHK, ALCHKI
   USE SMmod,    ONLY : head, binsmp, ddf, rhos, zos, zds, zus, nsd, rhodef, imet, smelt, tmelt
   USE ETmod,    ONLY : BAR, BMETP, BINETP, BMETAL, BMETDATES, CSTCAP, CSTCA1, CK, CB, CLAI1, FET, &
      MEASPE, MODE, MODECS, MODEVH, MODEPL, MODECL, NCTCLA, NCTVHT,NCTCST, NF, NCTPLA, &
!                     PS1, PLAI1, RELPLA, RELCST, RA, RC, RCF, RELCLA, RELVHT, RTOP, TIMCST, TIMPLA, TIMVHT, TIMCLA,  VHT1
      PS1, PLAI1, RELPLA, RELCST, RA, RC, RCF, RELCLA, RELVHT, RTOP, TIMCST, TIMPLA, TIMVHT, TIMCLA,  VHT1, &
      INITIALISE_ETMOD
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
   USE IS_CC,    ONLY : ISPLT,ISMN
   USE LINK_CW,  ONLY : DBDI, ACPBSG, DBS, ACPBI, ACPSFO, ACPBDO, THBEDO, THBED
   USE PLANT_CC, ONLY : PMASS, PF2MAX, PKMAX, NPLT, PFONE, NPLTYP, PDZF3, DELONE, NPL, GMCBBO

   USE ZQmod,    ONLY : ReadZQTable


   IMPLICIT NONE
!FROM SPEC_FR
!MODULE SPEC_FR
!------------------- Start of SPEC.FR ---------------------------------*
!
! ^^^ COMMON FILE OF SPECIFICATIONS OF FRAME VARIABLES.
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/SPEC.FR/4.2
! Modifications:
!   GP        FEB 89    2.0  'SHE88' IMPLEMENTATION ON NEWCASTLE AMDAHL
!   GP        AUG 89    2.2  ADD DEFAULT IMPERMEABLE BED LEVEL VARIABLES
!   GP        MAR 90    3.0     SHETRANUK AMENDMENTS
!  GP          3.4  Add BSOFT.
! RAH  03.10.94  Version 3.4.1 from version 3.4: standard format;
!                 no INTEGER*2; declare everything; no long lines.
!  GP  950117  4.0  Move TIM to AL.D.
! RAH  970221  4.1  Remove II,KK,L1,L,NSTRAT,NUZ,INIBDE,ZIBDE,WSI,WS,
!                   WEXSY,WSYEX,WSER1,WSER2,UZOLD,SZOLD,OCOLD,EXOLD,NOW,
!                   N1,T,QOCFT,DUMH,BCAL,B1,BIBDE (redundant).
! RAH  980308  4.2  Remove BUZCAL,BSZCAL,BOCCAL,BEXCAL.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
! SB  Mar 26  4.6  Allocate the following arrays:
!                  ADD BMETDATES so met data has the option of including dates
!----------------------------------------------------------------------*
!
! ^^^ INTEGER VARIABLES.
!IMPLICIT NONE
   INTEGER :: IAOUT
!COMMON / FRCB1 / IAOUT
!
! ^^^ FLOATING-POINT VARIABLES AND ARRAYS.
!
   DOUBLEPRECISION ALLOUT, DTAO, TSH, TCH !, TITLE (20)
!COMMON / FRCB2 / ALLOUT, DTAO, TSH, TCH, TITLE
!
! ^^^ LOGICAL VARIABLES.
!
   LOGICAL :: BFRTS1, BFRTS2, BINFRP, BTIME, BSOFT
   LOGICAL :: BSTORE, BPPNET, BPEPOT
   LOGICAL :: BPQOC, BPDEP, BPQF, BPQH, BPQSZ, BPHSZ, BPBAL, BPSD
!END MODULE SPEC_FR

   CHARACTER (LEN=80) :: TITLE
   CHARACTER(256)     :: msg



!SAVEd variables put here for AD
   INTEGER, SAVE   :: next_hour = 1, icounter2 = 0
   INTEGER         :: hour_now
   DOUBLEPRECISION :: qoctot = 0.0d0, uzold = 0.0d0, uznowt
   DOUBLEPRECISION :: sedtot = 0.0d0, sedfinetot = 0.0d0, contamtot = 0.0d0
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE               :: qoctotextra
   DOUBLEPRECISION :: PREVTM
   DOUBLEPRECISION :: TIMB=zero
   LOGICAL         :: FIRST_frmb=.TRUE.
   LOGICAL         :: SEDSRT=.FALSE.
   DOUBLEPRECISION :: GNUCUM (NELEE), DLSSRT (NELEE)

   PRIVATE

   PUBLIC :: FROPEN, FRINIT, FRSORT, FROUTPUT, FRMB, FRRESP, FRIND, FRLTL, INCM, & !REST NEEDED FOR AD ONLY
      qoctot, uzold, bsoft, tsh, tch, bstore, btime, next_hour, icounter2, DATE_FROM_HOUR, &
      sedtot,sedfinetot,contamtot

   CONTAINS


   !SSSSSS SUBROUTINE FRDIM
   SUBROUTINE FRDIM(BINFRP)
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

      ! Assumed global variables provided via host module(s) (e.g., SPEC.AL):
      ! NXEE, NYEE, NX, NXM1, NY, NYM1, total_no_elements, PPPRI
      ! DXIN, DYIN, ICMREF, LINKNS, CWIDTH, CLENTH, DXQQ, DYQQ, CAREA, cellarea, DHF

      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: BINFRP

      ! Locals, etc
      INTEGER :: I1, I2, IEL, IFACE, IL, IL1, IL2, INEXT1, INEXT2, ITYPE
      INTEGER :: IX, IY, J, JEL, JL, JTYPE, K
      DOUBLE PRECISION :: CATEST, DIFF, DX(NXEE), DY(NYEE)

   !----------------------------------------------------------------------*

      ! SET VALUE FOR BANK ELEMENT WIDTH
      ! (CURRENTLY HARD-CODED AS A FIXED WIDTH)
      BWIDTH = 10.0D0

      ! --- CALCULATE DX AND DY FROM DXIN AND DYIN
      DX(1)  = DXIN(1)
      DX(NX) = DXIN(NXM1)
      DO J = 2, NXM1
         DX(J) = (DXIN(J - 1) + DXIN(J)) * 0.5D0
      END DO

      DY(1)  = DYIN(1)
      DY(NY) = DYIN(NYM1)
      DO K = 2, NYM1
         DY(K) = (DYIN(K - 1) + DYIN(K)) * 0.5D0
      END DO

      ! --- SET UP BASIC DIMENSIONS OF EACH ELEMENT
      dim_loop: DO IEL = 1, total_no_elements
         ITYPE = ICMREF(IEL, 1)
         IX    = ICMREF(IEL, 2)
         IY    = ICMREF(IEL, 3)
         IL    = ICMREF(IEL, 4)

         IF (ITYPE == 0) THEN
            DXQQ(IEL) = DX(IX)
            DYQQ(IEL) = DY(IY)
         ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
            IF (LINKNS(IL)) THEN
               DXQQ(IEL) = BWIDTH
               DYQQ(IEL) = DY(IY)
            ELSE
               DXQQ(IEL) = DX(IX)
               DYQQ(IEL) = BWIDTH
            END IF
         ELSE IF (ITYPE == 3) THEN
            IF (LINKNS(IEL)) THEN
               DXQQ(IEL)  = CWIDTH(IL)
               DYQQ(IEL)  = DY(IY)
               CLENTH(IL) = DY(IY)
            ELSE
               DXQQ(IEL)  = DX(IX)
               DYQQ(IEL)  = CWIDTH(IL)
               CLENTH(IL) = DX(IX)
            END IF
         END IF
      END DO dim_loop

      ! --- CORRECT FOR OVERLAPPING ELEMENTS (NB: CHANNEL LINK OVERLAPS NOT IN)
      ! --- AND CALCULATE ELEMENT AND CATCHMENT AREA

      CAREA  = ZERO
      CATEST = ZERO

      overlap_loop: DO IEL = 1, total_no_elements
         ITYPE = ICMREF(IEL, 1)
         IX    = ICMREF(IEL, 2)
         IY    = ICMREF(IEL, 3)
         IL    = ICMREF(IEL, 4)

         IF (ITYPE == 0) THEN
            corner_loop: DO I1 = 5, 8
               ! GRID ELEMENTS (REMOVE WIDTHS OF CHANNEL LINKS, AND POSSIBLY BANK ELEME)
               INEXT1 = ICMREF(IEL, I1)

               IF (INEXT1 > 0) THEN
                  DIFF = ZERO
                  IF (ICMREF(INEXT1, 1) > 0) THEN
                     IL = ICMREF(INEXT1, 4)
                     DIFF = DIFF + 0.5D0 * CWIDTH(IL)
                     IF (ICMREF(INEXT1, 1) < 3) DIFF = DIFF + BWIDTH
                  END IF
                  IF (I1 == 5 .OR. I1 == 7) DXQQ(IEL) = DXQQ(IEL) - DIFF
                  IF (I1 == 6 .OR. I1 == 8) DYQQ(IEL) = DYQQ(IEL) - DIFF
               END IF

               ! BANK ELEMENTS (REMOVE OVERLAP OF BANKS/BANKS AND BANK/CHANNEL FOR EACH
               ! CORNER OF EACH GRID ELEMENT)
               I2 = I1 + 1
               IF (I2 == 9) I2 = 5
               INEXT2 = ICMREF(IEL, I2)

               IF (INEXT1 > 0 .AND. INEXT2 > 0) THEN
                  IF ((ICMREF(INEXT1, 1) == 1 .OR. ICMREF(INEXT1, 1) == 2) .AND. &
                      (ICMREF(INEXT2, 1) == 1 .OR. ICMREF(INEXT2, 1) == 2)) THEN

                     IL1 = ICMREF(INEXT1, 4)
                     IL2 = ICMREF(INEXT2, 4)

                     IF (LINKNS(IL1)) THEN
                        DYQQ(INEXT1) = DYQQ(INEXT1) - BWIDTH - 0.5D0 * CWIDTH(IL2)
                     ELSE
                        DXQQ(INEXT1) = DXQQ(INEXT1) - BWIDTH - 0.5D0 * CWIDTH(IL2)
                     END IF

                     IF (LINKNS(IL2)) THEN
                        DYQQ(INEXT2) = DYQQ(INEXT2) - BWIDTH - 0.5D0 * CWIDTH(IL1)
                     ELSE
                        DXQQ(INEXT2) = DXQQ(INEXT2) - BWIDTH - 0.5D0 * CWIDTH(IL1)
                     END IF
                  END IF
               END IF
            END DO corner_loop
         END IF

         ! CALCULATE CATCHMENT AREA BY SUMMING ALL BASIC GRID SIZES
         ! AND CATCHMENT AREA OBTAINED BY SUMMING ALL ELEMENT AREAS (INCLUDES OVERLAP)
         IF (ITYPE == 0) CATEST = CATEST + DX(IX) * DY(IY)

      END DO overlap_loop

      ! --- CALCULATE AREA OF EACH ELEMENT
      area_loop: DO IEL = 1, total_no_elements
         cellarea(IEL) = DXQQ(IEL) * DYQQ(IEL)
         CAREA = CAREA + cellarea(IEL)
      END DO area_loop

      ! --- PRINT OUT ELEMENT AREA, TOTAL CATCHMENT AREA, AND PERCENTAGE ERROR
      IF (BINFRP) THEN
         WRITE(PPPRI, 1500)
         DO IEL = 1, total_no_elements
            WRITE(PPPRI, 1600) IEL, DXQQ(IEL), DYQQ(IEL), cellarea(IEL)
         END DO

         DIFF = (CAREA - CATEST) * 100.0D0 / CAREA
         IF (CAREA < 1.0D6) THEN
            WRITE(PPPRI, 1700) CAREA, CATEST, DIFF
         ELSE
            WRITE(PPPRI, 1750) CAREA / 1.0D6, CATEST / 1.0D6, DIFF
         END IF
      END IF

      ! ----- SET UP SPACINGS DHF BETWEEN COMPUTATIONAL NODES AND EDGE OF ELEM
      node_space_loop: DO IEL = 1, total_no_elements
         ITYPE = ICMREF(IEL, 1)
         IX    = ICMREF(IEL, 2)
         IY    = ICMREF(IEL, 3)
         IL    = ICMREF(IEL, 4)

         ! WEST FACE (FACE 3)
         IFACE = 3
         JEL   = ICMREF(IEL, IFACE + 4)

         IF (JEL == 0) THEN
            IF (ITYPE == 0) THEN
               DHF(IEL, IFACE) = 0.5D0 * DXIN(IX - 1)
            ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               DHF(IEL, IFACE) = 0.5D0 * BWIDTH
            ELSE
               IF (LINKNS(IEL)) THEN
                  DHF(IEL, IFACE) = 0.5D0 * CWIDTH(IEL)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0 * DXIN(IX - 1)
               END IF
            END IF
         ELSE IF (JEL > 0) THEN
            JTYPE = ICMREF(JEL, 1)
            JL    = ICMREF(JEL, 4)

            IF (ITYPE == 0) THEN
               IF (JTYPE == 0) THEN
                  DHF(IEL, IFACE) = 0.5D0 * DXIN(IX - 1)
               ELSE IF (JTYPE == 1) THEN
                  DHF(IEL, IFACE) = 0.5D0 * (DXIN(IX - 1) - 2.0D0 * BWIDTH - CWIDTH(JL))
               ELSE IF (JTYPE == 3) THEN
                  DHF(IEL, IFACE) = 0.5D0 * (DXIN(IX - 1) - CWIDTH(JL))
               END IF
            ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               IF (JTYPE == 0) THEN
                  DHF(IEL, IFACE) = 0.5D0 * BWIDTH
               ELSE IF (JTYPE == 1 .OR. JTYPE == 2) THEN
                  DHF(IEL, IFACE) = 0.5D0 * DXQQ(IEL)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0 * BWIDTH
               END IF
            ELSE
               IF (LINKNS(IEL)) THEN
                  DHF(IEL, IFACE) = 0.5D0 * CWIDTH(IEL)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0 * DXIN(IX - 1)
               END IF
            END IF
         ELSE IF (JEL < 0) THEN
            IF (LINKNS(IEL)) THEN
               DHF(IEL, IFACE) = 0.5D0 * CWIDTH(IEL)
            ELSE
               DHF(IEL, IFACE) = 0.5D0 * DXIN(IX - 1)
            END IF
         END IF

         ! SOUTH FACE (FACE 4)
         IFACE = 4
         JEL   = ICMREF(IEL, IFACE + 4)

         IF (JEL == 0) THEN
            IF (ITYPE == 0) THEN
               DHF(IEL, IFACE) = 0.5D0 * DYIN(IY - 1)
            ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               DHF(IEL, IFACE) = 0.5D0 * BWIDTH
            ELSE
               IF (LINKNS(IEL)) THEN
                  DHF(IEL, IFACE) = 0.5D0 * DYIN(IY - 1)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0 * CWIDTH(IEL)
               END IF
            END IF
         ELSE IF (JEL > 0) THEN
            JTYPE = ICMREF(JEL, 1)
            JL    = ICMREF(JEL, 4)

            IF (ITYPE == 0) THEN
               IF (JTYPE == 0) THEN
                  DHF(IEL, IFACE) = 0.5D0 * DYIN(IY - 1)
               ELSE IF (JTYPE == 1) THEN
                  DHF(IEL, IFACE) = 0.5D0 * (DYIN(IY - 1) - 2.0D0 * BWIDTH - CWIDTH(JL))
               ELSE IF (JTYPE == 3) THEN
                  DHF(IEL, IFACE) = 0.5D0 * (DYIN(IY - 1) - CWIDTH(JL))
               END IF
            ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               IF (JTYPE == 0) THEN
                  DHF(IEL, IFACE) = 0.5D0 * BWIDTH
               ELSE IF (JTYPE == 1 .OR. JTYPE == 2) THEN
                  DHF(IEL, IFACE) = 0.5D0 * DYQQ(IEL)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0 * BWIDTH
               END IF
            ELSE
               IF (LINKNS(IEL)) THEN
                  DHF(IEL, IFACE) = 0.5D0 * DYIN(IY - 1)
               ELSE
                  DHF(IEL, IFACE) = 0.5D0 * CWIDTH(IEL)
               END IF
            END IF
         ELSE IF (JEL < 0) THEN
            IF (LINKNS(IEL)) THEN
               DHF(IEL, IFACE) = 0.5D0 * DYIN(IY - 1)
            ELSE
               DHF(IEL, IFACE) = 0.5D0 * CWIDTH(IEL)
            END IF
         END IF

         ! EAST FACE (FACE 1)
         IFACE = 1
         DHF(IEL, IFACE) = DXQQ(IEL) - DHF(IEL, 3)

         ! NORTH FACE (FACE 2)
         IFACE = 2
         DHF(IEL, IFACE) = DYQQ(IEL) - DHF(IEL, 4)

      END DO node_space_loop

   ! ^^^^^^^^^^^^ FORMAT STATEMENTS
1500  FORMAT(/ '   INDEX   DXQQ (M)   DYQQ (M)     AREA (M^^2)' /)
1600  FORMAT(' ',4X,I6,4X,F7.2,4X,F7.2,4X,F12.2)
1700  FORMAT(/ ' TOTAL CATCHMENT AREA = ',F12.3,' SQ. METRES. ' / &
      &        ' BASIC CATCHMENT AREA = ',F12.3,' SQ. METRES. ' / &
      &  ' DIFFERENCE INTRODUCED BY CHANNEL SYSTEM AND BANKS = ', &
      &  F12.3,' %' /)
1750  FORMAT(/ ' TOTAL CATCHMENT AREA = ',F12.3,' SQ. KM. ' / &
      &        ' BASIC CATCHMENT AREA = ',F12.3,' SQ. KM. ' / &
      &  ' DIFFERENCE INTRODUCED BY CHANNEL SYSTEM AND BANKS = ', &
      &  F12.3,' %' /)

   END SUBROUTINE FRDIM



   !SSSSSS SUBROUTINE FRIND (BINFRP)
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

   ! Assumed external module dependencies providing global variables:
   ! NELEE, NX, NY, NXEE, INGRID, LCODEX, LCODEY, BEXBK, BEXOC, NEL, NGDBGN, NLF,
   ! ICMBK, ICMREF, ICMRF2, ICMXY, NBFACE, NGRID, LINKNS, LINKNO, PPPRI,
   ! total_no_links, total_no_elements

      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: BINFRP

      ! Locals, etc
      LOGICAL, PARAMETER  :: NSOUTH = .TRUE., EWEST = .FALSE.
      INTEGER :: I, IBANK, ICOUNT, IM1, IN1, INDEX, INDEX2, INEXT1, IP1
      INTEGER :: ITYPE, J, J1, J2, JM1, JN2, JNEXT1, JP1, K, L, L1
      INTEGER :: NEL2, NNODE3, NNODE4
      LOGICAL :: SINGLE

      CHARACTER(LEN=2) :: PDIRN

      !----------------------------------------------------------------------*
      !
      ! ^^^^^^^^^^^^ INITIALISE ARRAY AND INDEX NUMBER
      !
      DO I = 1, NELEE
         NGRID (I) = 0
         NBFACE (I) = 0
         ICMREF (I, 1:12) = 0
      END DO

      INDEX = 0
      INDEX2 = 0

      !
      ! ^^^^^^^^^^^^ SET UP INDEX NUMBERS
      !
      ! --- CHANNEL LINKS
      !
      DO J = 1, NY
         DO I = 1, NX
            IF (LCODEY (I, J) >= 4) THEN
               INDEX = INDEX + 1
               ICMREF (INDEX, 1) = 3
               ICMREF (INDEX, 2) = I
               ICMREF (INDEX, 3) = J
               ICMREF (INDEX, 4) = INDEX
               LINKNS (INDEX) = .FALSE.
            END IF
         END DO

         DO I = 1, NX
            IF (LCODEX (I, J) >= 4) THEN
               INDEX = INDEX + 1
               ICMREF (INDEX, 1) = 3
               ICMREF (INDEX, 2) = I
               ICMREF (INDEX, 3) = J
               ICMREF (INDEX, 4) = INDEX
               LINKNS (INDEX) = .TRUE.
            END IF
         END DO
      END DO

      total_no_links = INDEX

      !
      ! --- BANK ELEMENTS
      !
      IF (BEXBK .AND. total_no_links > 0) THEN
         DO IBANK = 1, 2
            DO L = 1, total_no_links
               INDEX = INDEX + 1
               ICMREF (INDEX, 1) = IBANK
               ICMREF (INDEX, 2) = ICMREF (L, 2)
               ICMREF (INDEX, 3) = ICMREF (L, 3)
               ICMREF (INDEX, 4) = L
               ICMBK (L, IBANK) = INDEX
            END DO
         END DO
      END IF

      !
      ! --- GRID CODES
      !
      DO J = 1, NY
         DO I = 1, NX
            IF (INGRID (I, J) >= 0) THEN
               INDEX = INDEX + 1
               ICMREF (INDEX, 2) = I
               ICMREF (INDEX, 3) = J
               ICMXY (I, J) = INDEX
            END IF
         END DO
      END DO

      NGDBGN = total_no_links + 1
      total_no_elements = INDEX

      !
      ! ^^^^^^^^^^^^ SET UP ADJACENT NODES
      !
      DO INDEX = 1, total_no_elements

         ITYPE = ICMREF (INDEX, 1)
         I = ICMREF (INDEX, 2)
         J = ICMREF (INDEX, 3)
         L = ICMREF (INDEX, 4)
         IP1 = I + 1
         JP1 = J + 1
         IM1 = I - 1
         JM1 = J - 1

         ! --- GRID SQUARE
         IF (ITYPE == 0) THEN

            ! FACE 1 (EAST)
            IF (BEXOC .AND. LCODEX (I + 1, J) >= 4) THEN
               L = LINKNO (IP1, J, NSOUTH)
               IF (BEXBK) THEN
                  ICMREF (INDEX, 5) = ICMBK (L, 2)
               ELSE
                  ICMREF (INDEX, 5) = L
                  ICMREF (INDEX, 4) = 9999
               END IF
            ELSE
               IF (INGRID (I + 1, J) >= 0) ICMREF (INDEX, 5) = ICMXY (I + 1, J)
            END IF

            ! FACE 2 (NORTH)
            IF (BEXOC .AND. LCODEY (I, J + 1) >= 4) THEN
               L = LINKNO (I, JP1, EWEST)
               IF (BEXBK) THEN
                  ICMREF (INDEX, 6) = ICMBK (L, 2)
               ELSE
                  ICMREF (INDEX, 6) = L
                  ICMREF (INDEX, 4) = 9999
               END IF
            ELSE
               IF (INGRID (I, J + 1) >= 0) ICMREF (INDEX, 6) = ICMXY (I, J + 1)
            END IF

            ! FACE 3 (WEST)
            IF (BEXOC .AND. LCODEX (I, J) >= 4) THEN
               L = LINKNO (I, J, NSOUTH)
               IF (BEXBK) THEN
                  ICMREF (INDEX, 7) = ICMBK (L, 1)
               ELSE
                  ICMREF (INDEX, 7) = L
                  ICMREF (INDEX, 4) = 9999
               END IF
            ELSE
               IF (INGRID (I - 1, J) >= 0) ICMREF (INDEX, 7) = ICMXY (I - 1, J)
            END IF

            ! FACE 4 (SOUTH)
            IF (BEXOC .AND. LCODEY (I, J) >= 4) THEN
               L = LINKNO (I, J, EWEST)
               IF (BEXBK) THEN
                  ICMREF (INDEX, 8) = ICMBK (L, 1)
               ELSE
                  ICMREF (INDEX, 8) = L
                  ICMREF (INDEX, 4) = 9999
               END IF
            ELSE
               IF (INGRID (I, J - 1) >= 0) ICMREF (INDEX, 8) = ICMXY (I, J - 1)
            END IF

         ! --- CHANNEL LINK
         ELSE IF (ITYPE == 3) THEN

            ! FACE 1 (EAST)
            IF (LINKNS (L)) THEN
               IF (BEXBK) THEN
                  ICMREF (INDEX, 5) = ICMBK (L, 1)
               ELSE
                  IF (INGRID (I, J) >= 0) ICMREF (INDEX, 5) = ICMXY (I, J)
               END IF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEX (I + 1, J) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEY (I + 1, J) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEX (I + 1, J - 1) >= 4) ICOUNT = ICOUNT + 1

               IF (ICOUNT > 1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF (INDEX, 5) = -INDEX2
               END IF

               IF (LCODEX (I + 1, J) >= 4) THEN
                  L1 = LINKNO (IP1, J, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 5) = L1
                  ELSE
                     ICMRF2 (INDEX2, 1) = L1
                  END IF
               END IF

               IF (LCODEY (I + 1, J) >= 4) THEN
                  L1 = LINKNO (IP1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 5) = L1
                  ELSE
                     ICMRF2 (INDEX2, 2) = L1
                  END IF
               END IF

               IF (LCODEX (I + 1, J - 1) >= 4) THEN
                  L1 = LINKNO (IP1, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 5) = L1
                  ELSE
                     ICMRF2 (INDEX2, 3) = L1
                  END IF
               END IF
            END IF

            ! FACE 2 (NORTH)
            IF (.NOT. LINKNS (L)) THEN
               IF (BEXBK) THEN
                  ICMREF (INDEX, 6) = ICMBK (L, 1)
               ELSE
                  IF (INGRID (I, J) >= 0) ICMREF (INDEX, 6) = ICMXY (I, J)
               END IF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEY (I - 1, J + 1) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEX (I, J + 1) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEY (I, J + 1) >= 4) ICOUNT = ICOUNT + 1

               IF (ICOUNT > 1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF (INDEX, 6) = -INDEX2
               END IF

               IF (LCODEY (I - 1, J + 1) >= 4) THEN
                  L1 = LINKNO (IM1, JP1, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 6) = L1
                  ELSE
                     ICMRF2 (INDEX2, 1) = L1
                  END IF
               END IF

               IF (LCODEX (I, J + 1) >= 4) THEN
                  L1 = LINKNO (I, JP1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 6) = L1
                  ELSE
                     ICMRF2 (INDEX2, 2) = L1
                  END IF
               END IF

               IF (LCODEY (I, J + 1) >= 4) THEN
                  L1 = LINKNO (I, JP1, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 6) = L1
                  ELSE
                     ICMRF2 (INDEX2, 3) = L1
                  END IF
               END IF
            END IF

            ! FACE 3 (WEST)
            IF (LINKNS (L)) THEN
               IF (BEXBK) THEN
                  ICMREF (INDEX, 7) = ICMBK (L, 2)
               ELSE
                  IF (INGRID (I - 1, J) >= 0) ICMREF (INDEX, 7) = ICMXY (I - 1, J)
               END IF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEX (I, J - 1) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEY (I - 1, J) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEX (I, J) >= 4) ICOUNT = ICOUNT + 1

               IF (ICOUNT > 1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF (INDEX, 7) = -INDEX2
               END IF

               IF (LCODEX (I, J - 1) >= 4) THEN
                  L1 = LINKNO (I, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 7) = L1
                  ELSE
                     ICMRF2 (INDEX2, 1) = L1
                  END IF
               END IF

               IF (LCODEY (I - 1, J) >= 4) THEN
                  L1 = LINKNO (IM1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 7) = L1
                  ELSE
                     ICMRF2 (INDEX2, 2) = L1
                  END IF
               END IF

               IF (LCODEX (I, J) >= 4) THEN
                  L1 = LINKNO (I, J, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 7) = L1
                  ELSE
                     ICMRF2 (INDEX2, 3) = L1
                  END IF
               END IF
            END IF

            ! FACE 4 (SOUTH)
            IF (.NOT. LINKNS (L)) THEN
               IF (BEXBK) THEN
                  ICMREF (INDEX, 8) = ICMBK (L, 2)
               ELSE
                  IF (INGRID (I, J - 1) >= 0) ICMREF (INDEX, 8) = ICMXY (I, J - 1)
               END IF
            ELSE
               SINGLE = .TRUE.
               ICOUNT = 0
               IF (LCODEY (I, J) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEX (I, J - 1) >= 4) ICOUNT = ICOUNT + 1
               IF (LCODEY (I - 1, J) >= 4) ICOUNT = ICOUNT + 1

               IF (ICOUNT > 1) THEN
                  SINGLE = .FALSE.
                  INDEX2 = INDEX2 + 1
                  ICMREF (INDEX, 8) = -INDEX2
               END IF

               IF (LCODEY (I, J) >= 4) THEN
                  L1 = LINKNO (I, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 8) = L1
                  ELSE
                     ICMRF2 (INDEX2, 1) = L1
                  END IF
               END IF

               IF (LCODEX (I, J - 1) >= 4) THEN
                  L1 = LINKNO (I, JM1, NSOUTH)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 8) = L1
                  ELSE
                     ICMRF2 (INDEX2, 2) = L1
                  END IF
               END IF

               IF (LCODEY (I - 1, J) >= 4) THEN
                  L1 = LINKNO (IM1, J, EWEST)
                  IF (SINGLE) THEN
                     ICMREF (INDEX, 8) = L1
                  ELSE
                     ICMRF2 (INDEX2, 3) = L1
                  END IF
               END IF
            END IF

         ! --- BANK ELEMENT
         ELSE

            ! FACE 1 (EAST)
            IF (LINKNS (L)) THEN
               IF (ITYPE == 1) THEN
                  IF (INGRID (I, J) >= 0) ICMREF (INDEX, 5) = ICMXY (I, J)
               ELSE
                  ICMREF (INDEX, 5) = L
               END IF
            ELSE
               IF (ITYPE == 1) THEN
                  IF (LCODEX (I + 1, J) >= 4) THEN
                     L1 = LINKNO (IP1, J, NSOUTH)
                     ICMREF (INDEX, 5) = ICMBK (L1, 2)
                  ELSE IF (LCODEY (I + 1, J) >= 4) THEN
                     L1 = LINKNO (IP1, J, EWEST)
                     ICMREF (INDEX, 5) = ICMBK (L1, 1)
                  ELSE IF (LCODEX (I + 1, J - 1) >= 4) THEN
                     L1 = LINKNO (IP1, JM1, NSOUTH)
                     ICMREF (INDEX, 5) = ICMBK (L1, 1)
                  END IF
               ELSE
                  IF (LCODEX (I + 1, J - 1) >= 4) THEN
                     L1 = LINKNO (IP1, JM1, NSOUTH)
                     ICMREF (INDEX, 5) = ICMBK (L1, 2)
                  ELSE IF (LCODEY (I + 1, J) >= 4) THEN
                     L1 = LINKNO (IP1, J, EWEST)
                     ICMREF (INDEX, 5) = ICMBK (L1, 2)
                  ELSE IF (LCODEX (I + 1, J) >= 4) THEN
                     L1 = LINKNO (IP1, J, NSOUTH)
                     ICMREF (INDEX, 5) = ICMBK (L1, 1)
                  END IF
               END IF
            END IF

            ! FACE 2 (NORTH)
            IF (.NOT. LINKNS (L)) THEN
               IF (ITYPE == 1) THEN
                  IF (INGRID (I, J) >= 0) ICMREF (INDEX, 6) = ICMXY (I, J)
               ELSE
                  ICMREF (INDEX, 6) = L
               END IF
            ELSE
               IF (ITYPE == 1) THEN
                  IF (LCODEY (I, J + 1) >= 4) THEN
                     L1 = LINKNO (I, JP1, EWEST)
                     ICMREF (INDEX, 6) = ICMBK (L1, 2)
                  ELSE IF (LCODEX (I, J + 1) >= 4) THEN
                     L1 = LINKNO (I, JP1, NSOUTH)
                     ICMREF (INDEX, 6) = ICMBK (L1, 1)
                  ELSE IF (LCODEY (I - 1, J + 1) >= 4) THEN
                     L1 = LINKNO (IM1, JP1, EWEST)
                     ICMREF (INDEX, 6) = ICMBK (L1, 1)
                  END IF
               ELSE
                  IF (LCODEY (I - 1, J + 1) >= 4) THEN
                     L1 = LINKNO (IM1, JP1, EWEST)
                     ICMREF (INDEX, 6) = ICMBK (L1, 2)
                  ELSE IF (LCODEX (I, J + 1) >= 4) THEN
                     L1 = LINKNO (I, JP1, NSOUTH)
                     ICMREF (INDEX, 6) = ICMBK (L1, 2)
                  ELSE IF (LCODEY (I, J + 1) >= 4) THEN
                     L1 = LINKNO (I, JP1, EWEST)
                     ICMREF (INDEX, 6) = ICMBK (L1, 1)
                  END IF
               END IF
            END IF

            ! FACE 3 (WEST)
            IF (LINKNS (L)) THEN
               IF (ITYPE == 1) THEN
                  ICMREF (INDEX, 7) = L
               ELSE
                  IF (INGRID (I - 1, J) >= 0) ICMREF (INDEX, 7) = ICMXY (I - 1, J)
               END IF
            ELSE
               IF (ITYPE == 1) THEN
                  IF (LCODEX (I, J) >= 4) THEN
                     L1 = LINKNO (I, J, NSOUTH)
                     ICMREF (INDEX, 7) = ICMBK (L1, 1)
                  ELSE IF (LCODEY (I - 1, J) >= 4) THEN
                     L1 = LINKNO (IM1, J, EWEST)
                     ICMREF (INDEX, 7) = ICMBK (L1, 1)
                  ELSE IF (LCODEX (I, J - 1) >= 4) THEN
                     L1 = LINKNO (I, JM1, NSOUTH)
                     ICMREF (INDEX, 7) = ICMBK (L1, 2)
                  END IF
               ELSE
                  IF (LCODEX (I, J - 1) >= 4) THEN
                     L1 = LINKNO (I, JM1, NSOUTH)
                     ICMREF (INDEX, 7) = ICMBK (L1, 1)
                  ELSE IF (LCODEY (I - 1, J) >= 4) THEN
                     L1 = LINKNO (IM1, J, EWEST)
                     ICMREF (INDEX, 7) = ICMBK (L1, 2)
                  ELSE IF (LCODEX (I, J) >= 4) THEN
                     L1 = LINKNO (I, J, NSOUTH)
                     ICMREF (INDEX, 7) = ICMBK (L1, 2)
                  END IF
               END IF
            END IF

            ! FACE 4 (SOUTH)
            IF (.NOT. LINKNS (L)) THEN
               IF (ITYPE == 1) THEN
                  ICMREF (INDEX, 8) = L
               ELSE
                  IF (INGRID (I, J - 1) >= 0) ICMREF (INDEX, 8) = ICMXY (I, J - 1)
               END IF
            ELSE
               IF (ITYPE == 1) THEN
                  IF (LCODEY (I, J) >= 4) THEN
                     L1 = LINKNO (I, J, EWEST)
                     ICMREF (INDEX, 8) = ICMBK (L1, 1)
                  ELSE IF (LCODEX (I, J - 1) >= 4) THEN
                     L1 = LINKNO (I, JM1, NSOUTH)
                     ICMREF (INDEX, 8) = ICMBK (L1, 1)
                  ELSE IF (LCODEY (I - 1, J) >= 4) THEN
                     L1 = LINKNO (IM1, J, EWEST)
                     ICMREF (INDEX, 8) = ICMBK (L1, 2)
                  END IF
               ELSE
                  IF (LCODEY (I - 1, J) >= 4) THEN
                     L1 = LINKNO (IM1, J, EWEST)
                     ICMREF (INDEX, 8) = ICMBK (L1, 1)
                  ELSE IF (LCODEX (I, J - 1) >= 4) THEN
                     L1 = LINKNO (I, JM1, NSOUTH)
                     ICMREF (INDEX, 8) = ICMBK (L1, 2)
                  ELSE IF (LCODEY (I, J) >= 4) THEN
                     L1 = LINKNO (I, J, EWEST)
                     ICMREF (INDEX, 8) = ICMBK (L1, 2)
                  END IF
               END IF
            END IF

         END IF

      END DO

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

      element_check: DO INDEX = 1, total_no_elements
         face_loop: DO I = 1, 4
            INEXT1 = ICMREF (INDEX, I + 4)

            IF (INEXT1 > 0) THEN
               DO J = 1, 4
                  IF (ICMREF (INEXT1, J + 4) == INDEX) THEN
                     ICMREF (INDEX, I + 8) = J
                     CYCLE face_loop
                  END IF
               END DO
               WRITE(PPPRI, 1100) INDEX, I
               ICOUNT = ICOUNT + 1

            ELSE IF (INEXT1 < 0) THEN
               IF (ICMRF2 (-INEXT1, 1) == 0 .OR. ICMRF2 (-INEXT1, 2) == 0 .OR. ICMRF2 (-INEXT1, 3) == 0) THEN
                  NNODE3 = NNODE3 + 1
               ELSE
                  NNODE4 = NNODE4 + 1
               END IF

               branch_loop: DO J1 = 1, 3
                  IN1 = ICMRF2 (-INEXT1, J1)
                  IF (IN1 > 0) THEN
                     DO J = 1, 4
                        JNEXT1 = ICMREF (IN1, J + 4)
                        IF (JNEXT1 < 0) THEN
                           DO J2 = 1, 3
                              JN2 = ICMRF2 (-JNEXT1, J2)
                              IF (JN2 == INDEX) THEN
                                 ICMRF2 (-INEXT1, J1 + 3) = J
                                 CYCLE branch_loop
                              END IF
                           END DO
                        END IF
                     END DO
                     WRITE(PPPRI, 1100) INDEX, I
                     ICOUNT = ICOUNT + 1
                  END IF
               END DO branch_loop

            ELSE
               ICMREF (INDEX, I + 8) = I
               IF (ITYPE < 3 .AND. NBFACE (INDEX) == 0) NBFACE (INDEX) = I
            END IF
         END DO face_loop
      END DO element_check

      IF (ICOUNT > 0) WRITE(PPPRI, 1200) ICOUNT

      !
      ! ^^^^^^^^^^^^ WRITE OUT INDEX ARRAY, IF REQUIRED
      !
      IF (BINFRP) THEN

         WRITE(PPPRI, 1300) total_no_elements
         DO INDEX = 1, total_no_elements
            PDIRN = ' '
            ITYPE = ICMREF (INDEX, 1)
            IF (ITYPE > 0) THEN
               L = ICMREF (INDEX, 4)
               IF (LINKNS (L)) THEN
                  PDIRN = 'NS'
               ELSE
                  PDIRN = 'EW'
               END IF
            END IF
            WRITE(PPPRI, 1400) INDEX, (ICMREF (INDEX, K), K = 1, 4), &
               PDIRN, (ICMREF (INDEX, K), K = 5, 8)
         END DO

         IF (NEL2 > 0) THEN
            WRITE(PPPRI, 1500) NNODE3 / 3, NNODE4 / 4, NEL2
            DO INDEX2 = 1, NEL2
               WRITE(PPPRI, 1600) INDEX2, (ICMRF2 (INDEX2, I), I = 1, 3)
            END DO
         END IF

      END IF

      ! FORMAT STATEMENTS
      !
1100  FORMAT(' INCONSISTENCY FOUND AT INDEX:', I4, ' FACE:', I2)
1200  FORMAT(/ I4, ' INCONSISTENCIES FOUND IN INDEX ARRAY' /)
1300  FORMAT(' ', / 'INDEX ARRAY: NO. OF ELEMENTS = ', I6, // &
             ' ', '     INDEX      TYPE         X         Y      LINK   ', &
             '  FACE1     FACE2     FACE3     FACE4' / &
             ' ', '     -----      ----         -         -      ----   ', &
             '  -----     -----     -----     -----' )
1400  FORMAT(' ', 5(4X, I6), 1X, A2, 1X, I6, 3(4X, I6))
1500  FORMAT(' '/'AUXILIARY INDEX ARRAY FOR CHANNEL NODES: ', / &
             'NO. OF NODES WITH 3 BRANCHES = ', I4, / &
             'NO. OF NODES WITH 4 BRANCHES = ', I4, / &
             'TOTAL NO. OF INDICES         = ', I4 // &
             ' ', '   INDEX  LINK 1  LINK 2  LINK 3' / &
             ' ', '   -----  ------  ------  ------' )
1600  FORMAT(' ', 5(4X, I4))

   END SUBROUTINE FRIND



   !SSSSSS SUBROUTINE FRINIT
   SUBROUTINE FRINIT()
   !----------------------------------------------------------------------*
   !
   ! INITIALISATION PHASE
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/FR/FRINIT/4.2
   ! Modifications:
   !  GP         3.4  Set CMT,CMP,CMB.  Special treatment for DBOT(LL).
   !                  Replace L1 with L1+1 in assignment of NLYRBT,ZLYRBT.
   !                  Print table of soil horizon depths.
   !                  Hot-start: initialize HOTIME; read HOT file;
   !                  set TIMEUZ,BHOTTI; call FRRESP.
   ! RAH  941005 3.4.1 Bring IMPLICIT from AL.P.  Pass UZNOW to FRRESP.
   !                  Call ERROR if NLYRBT can't be set.
   !  GP  960724  4.0  New module VSS replaces UZ,SZ,EX ...
   !                  Reassign unit nos; replace UZD,SZD,AQD,SZB,HBD with
   !                  VSD,VSI,LFB,LHB,LGB,BFB,BHB; scrap CPR (keep CMP).
   !                  Call VSIN, instead of INUZ,INSZ,INUZ2 (or DINUZ,
   !                  DINSZ), and after INBK, not before.
   !                  Move adjustment of ZLYRBT & setting of
   !                  NLYRBT,NTSOIL,NHBED,FHBED to VSREAD,VSCONC.
   !                  Scrap ZBED (see INCM), RSZ,QUZR,QSZR (see SHETRN)
   !                  and EPSZA,QSZOC,QSZO,WSI.  Add FRRESP arg .FALSE..
   !                  Close FR,WAT input (except boundary data) files.
   !                  Remove PSI3,PSI33,HSZ,EPS,QSZUZ,WATC3 from HOT read.
   ! RAH  970223  4.1  Explicit typing.  Scrap BUZCAL,BSZCAL,BOCCAL,BEXCAL,
   !                  DTSTOC,DTSTSZ,DTSTUZ,EXNEXT,EXNOW,EXVAL,OCUNT,OCVAL,
   !                  PNETOC,POC,SZNOW,SZUNT,SZVAL,TSTOOC,TSTOSZ,TSTOUZ,
   !                  UZUNT,WSUZI,WSSZI,WSOCI,WOCLI (AL.D), FINC,NUMBER.
   !      970524      AIOSTO size 20 (was 50); & use DATA.
   !      970525      Pass BINFRP to INRES.
   !      970530      (Amend DATA statement.)
   ! RAH  980317  4.2  (Amend specification comments below.)
   ! SB   010307  4g-pc changed data statement for AIOSTO
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! NELEE, BEXET, BEXSM, MSM, BEXOC, BEXBK, BINFRP, total_no_links, NMC,
      ! NRAINC, ICMREF, iszq, FRD, VSD, OCD, ETD, SMD, BKD, VSI, PPD, HOTIME,
      ! zero, BHOTRD, HOT, UZNEXT, top_cell_no, CSTORE, NGDBGN, total_no_elements,
      ! QOC, DQ0ST, DQIST, DQIST2, SD, TS, NSMC, SMELT, tmelt, VSPSI, BHOTTI,
      ! PPPRI, ALLOUT, DTAO, UZNOW, OCNOW, UZVAL, TIMEUZ

      IMPLICIT NONE

      ! Locals, etc
      INTEGER :: IEL, IFACE, JEL, K, ios
      DOUBLE PRECISION :: rdd(NELEE), rddq(NELEE, 4)
      CHARACTER(LEN=20) :: AIOSTO
      CHARACTER(LEN=10) :: atemp

      DATA AIOSTO / '11111111111111111111' /

      !----------------------------------------------------------------------*

      ! OPEN I/O DATA FILES
      ! CALL FROPEN  !moded to main routine
      !
      ! CALL INITIALIZATION SUBROUTINES.
      ! NOTE: THE ORDER IN WHICH THE SUBROUTINES ARE CALLED IS IMPORTANT.

      CALL INFR
      CALL INITIALISE_AL_C3
      CALL INITIALISE_ETMOD

      IF (BEXET) CALL INET
      IF (.NOT. BEXET) CALL DINET

      IF (BEXSM) CALL INSM
      IF (.NOT. BEXSM) MSM = 0

      IF (BEXOC) CALL OCINI()
      IF (.NOT. BEXOC) CALL DINOC

      ! CALCULATE ELEMENT DIMENSIONS AND AREAS
      ! (MUST BE CALLED AFTER OCINI AND BEFORE VSIN)
      CALL FRDIM (BINFRP)

      IF (BEXBK) CALL INBK

      CALL VSIN

      ! SET UP MET. & RAINFALL STATIONS FOR THE CHANNEL (= ADJACENT BANK/GRID)
      link_loop: DO IEL = 1, total_no_links
         NMC (IEL) = 1
         NRAINC (IEL) = 1

         DO IFACE = 1, 4
            JEL = ICMREF (IEL, 4 + IFACE)
            IF (JEL > 0) THEN
               IF (ICMREF (JEL, 1) /= 3 .AND. NMC (JEL) > 0 .AND. NRAINC (JEL) > 0) THEN
                  NMC (IEL) = NMC (JEL)
                  NRAINC (IEL) = NRAINC (JEL)
                  CYCLE link_loop
               END IF
            END IF
         END DO
      END DO link_loop

      !***ZQ Module 200520
      IF (iszq) CALL ReadZQTable

      ! close data input file units
      REWIND(FRD) !!CLOSE (FRD)    !AD
      REWIND(VSD) !!CLOSE (VSD)    !AD
      REWIND(OCD) !!CLOSE (OCD)    !AD
      REWIND(ETD) !!CLOSE (ETD)    !AD
      REWIND(SMD) !!CLOSE (SMD)    !AD
      REWIND(BKD) !!CLOSE (BKD)    !AD
      REWIND(VSI) !!CLOSE (VSI)    !AD
      ! CALL RES FILE INPUT ROUTINE, IF REQUIRED
      ! IF (BSTORE) CALL INRES(BINFRP)
      REWIND(PPD) !!CLOSE (PPD)    !AD

      ! UPDATE HOTSTART TIME AND READ FROM FILE IF BHOTRD = TRUE
      HOTIME = zero

      IF (BHOTRD) THEN

         hotstart_read: DO
            READ (HOT, *, IOSTAT=ios) atemp, HOTIME, UZNEXT, top_cell_no, atemp, &
                  (CSTORE (IEL), IEL = NGDBGN, total_no_elements), atemp, &
                  (rdd(IEL), IEL = 1, total_no_elements), atemp, &
                  ((rddq (IEL, K), IEL = 1, total_no_elements), K = 1, 4), atemp, &
                  ((QOC (IEL, K), IEL = 1, total_no_elements), K = 1, 4), atemp, &
                  ((DQ0ST (IEL, K), IEL = 1, total_no_elements), K = 1, 4), atemp, &
                  ((DQIST (IEL, K), IEL = 1, total_no_elements), K = 1, 4), atemp, &
                  ((DQIST2 (IEL, K), IEL = 1, NGDBGN - 1), K = 1, 3), atemp, &
                  (SD (IEL), IEL = NGDBGN, total_no_elements), atemp, &
                  (TS (IEL), IEL = NGDBGN, total_no_elements), atemp, &
                  (NSMC (IEL), IEL = NGDBGN, total_no_elements), atemp, &
                  ((SMELT (K, IEL), K = 1, NSMC (IEL)), IEL = NGDBGN, total_no_elements), atemp, &
                  ((tmelt(K, IEL), K = 1, NSMC (IEL)), IEL = NGDBGN, total_no_elements), atemp, &
                  ((VSPSI (k, iel), k = 1, top_cell_no), IEL = 1, total_no_elements)

            ! Gracefully exit if end of hotstart file is reached
            IF (ios /= 0) THEN
               WRITE(PPPRI, '(/ A)') ' WARNING: END OF HOTSTART FILE REACHED'
               EXIT hotstart_read
            END IF

            DO IEL = 1, total_no_elements
               CALL SETHRF(IEL, rdd(IEL))
               DO K = 1, 4
                  CALL SETQSA(IEL, K, rddq(IEL, K))
               END DO
            END DO

            ! Keep reading lines if HOTIME is less than the target BHOTTI
            IF (HOTIME >= BHOTTI) EXIT hotstart_read

         END DO hotstart_read

         WRITE(PPPRI, '(// A, F10.2, A /)') ' ^^^ HOTSTART OF SIMULATION AT TIME ', HOTIME, ' ^^^'

         ALLOUT = HOTIME + DTAO
         UZNOW  = HOTIME
         OCNOW  = HOTIME
         UZVAL  = UZNOW + UZNEXT
         TIMEUZ = HOTIME
         BHOTTI = HOTIME

         ! --- WRITE SET OF DATA TO RES FILES AT HOTSTART TIME
         CALL FRRESP (AIOSTO, UZNOW, .FALSE.)

      END IF

   END SUBROUTINE FRINIT



   !SSSSSS SUBROUTINE FRLTL (NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)
   SUBROUTINE FRLTL (NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)
   !----------------------------------------------------------------------*
   !
   ! READ IN ARRAY OF NUMERIC CODES (FOR OUTPUT CLASS DEFINITION)
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/FR/FRLTL/4.0
   ! Modifications:
   ! RAH  941002 3.4.1 Remove IMPLICIT INTEGER*2.  IARR INTEGER (was *2).
   ! RAH  970223  4.1  Explicit typing.
   !----------------------------------------------------------------------*
   ! INPUT PARAMETERS:
   !   NNX     X DIMENSION OF GRID
   !   NNY     Y DIMENSION OF GRID
   !   NXE     X DIMENSION OF ARRAY
   !   NYE     Y DIMENSION OF ARRAY
   !   INF     INOUT FILE UNIT NUMBER
   !   IOF     OUTPUT FILE UNIT NUMBER
   !   BPCNTL  LOGICAL PRINT CONTROL
   !
   ! OUTPUT PARAMETERS:
   !   IARR    ARRAY OF CODES READ IN FROM FILE
   !
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NNX, NNY, NXE, NYE, INF, IOF
      LOGICAL, INTENT(IN) :: BPCNTL

      ! Output arguments
      INTEGER, INTENT(OUT) :: IARR (NXE, NYE)

      ! Locals, etc
      INTEGER :: I, J, K, L, M
      ! CHARACTER(LEN=80) :: TITLE
      CHARACTER(LEN=1)  :: A1LINE (200)

      ! Modern array constructor replacing the legacy DATA statement
      CHARACTER(LEN=1), PARAMETER :: NMERIC(9) = ['1', '2', '3', '4', '5', '6', '7', '8', '9']

      !----------------------------------------------------------------------*

      READ (INF, '(A80)') TITLE
      IF (BPCNTL) WRITE (IOF, '(A80)') TITLE

      ! Vectorized array initialization replacing nested DO loops
      IARR(1:NNX, 1:NNY) = 0

      I = NNY
      DO J = 1, NNY
         READ (INF, '(I7, 1X, 500A1)') K, (A1LINE (L), L = 1, NNX)
         IF (BPCNTL) WRITE (IOF, '(I7, 1X, 500A1)') K, (A1LINE (L), L = 1, NNX)

         ! Inline error handling replacing GOTO 100
         IF (K /= I) THEN
            IF (BPCNTL) WRITE (IOF, '("   ^^^   INCORRECT COORDINATE")')
            STOP 1
         END IF

         I = I - 1

         outer_loop: DO L = 1, NNX
            DO M = 1, 9
               IF (A1LINE(L) == NMERIC(M)) THEN
                  IARR(L, K) = M
                  CYCLE outer_loop
               END IF
            END DO
         END DO outer_loop

      END DO

   END SUBROUTINE FRLTL



   !----------------------------------------------------------------------*
   ! subroutine to calculate monthly mass balance
   ! all variables are calculated in cubic metres
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/FR/FRMB/4.1
   ! Modifications:
   !  GP  29.06.95  written (v4.0 finished 16/7/96)
   ! RAH  970307  4.1  Replace ineffectual (& illegal) COMMON with SAVE.
   !                   Swap indices: DELTAZ,QVSV,VSTHE (see AL.C).
   !                   DATA statements AFTER specs.  Explicit typing.
   !      970310       Use local variables for repeated sub-expressions.
   !                   Define constant SS.  Remove redundant local IMSTN.
   !                   Initialize locals to 0, not BALANC.
   !                   Consistent indices.  Generic intrinsics.
   !                   Replace NGDBGN with NLF+1.  Scrap some locals.
   !      970311       Loop I=0,6,6 for BALANC update.  Labels in order.
   !      970312       Change name from MB to FRMB.  Remove temporary code.
   !                   Use ALINIT.  Simplify test (asasume UZNOW.ge.0).
   !                   Replace UZNEXT*3600 with DTUZ.
   !                   Fix errors in: setting of MBDAY (case MBFLAG=1); and
   !                   initialization of BALANC (was omitted).
   !      970524       Add FRRESP argument UZNOW.
   !----------------------------------------------------------------------*
   ! Limited ranges:
   !      dimensions of size NLFEE:  for link  in            1:NLF
   ! DELTAZ(cell,e), VSTHE(cell,e):  for cell  in NLYRBT(e,1):LL
   !                       P(ipstn):  for ipstn in    NRAINC(1:NEL)
   !                  QVSV(cell,e):  for cell  == NLYRBT(e,1)
   !----------------------------------------------------------------------*
   ! Entry conditions:
   ! 1 <=  LL <=  LLEE
   ! 1 <= NEL <= NELEE
   ! 0 <= NLF <= NLFEE >= 1
   ! for each e in 1:NEL:
   !     2 <= NLYRBT(e,1) <= LLEE
   !     1 <= NRAINC(e)   <= NVEE (size of P)
   !----------------------------------------------------------------------*
   ! Commons and imported constants
   ! Imported constants
   !     SPEC.AL:         LLEE,NELEE,NLFEE
   ! Input common
   !     SPEC.AL:         LL,MBFACE,MBFLAG,MBLINK,NEL,NLF
   !                           NRAINC(NEL),NLYRBT(NEL,1)
   !                      TIH,   AREA(NEL),RHOSAR(NEL),CLENTH(NLFEE)
   !                      DELTAZ(LLEE,NEL),ZGRUND(NEL)
   !                      DTUZ,QOC(MBLINK:MBLINK,MBFACE:MBFACE)
   !                      ARXL(NLFEE), CSTORE(NEL),    P(*)
   !                      QBKB(NLFEE,2),EINTA(NEL),HRF(NEL), QVSV(LLEE,NEL)
   !                      QBKF(NLFEE,2),EEVAP(NEL), SD(NEL),VSTHE(LLEE,NEL)
   ! In+out common
   !     SPEC.AL:         MBDAY,MBMON,MBYEAR
   ! Out+in common
   !     SPEC.AL:         BALANC(19)
   ! Locals, etc
   !INTRINSIC ABS, MOD
   SUBROUTINE FRMB

      ! Assumed external module dependencies providing global arrays:
      ! BALANC, total_no_elements, precip_m_per_s, EINTA, EEVAP, ERZA, QVSV
      ! QOC, QBKB, QBKF, CSTORE, SD, RHOSAR, GETHRF, ZGRUND, VSTHE, DELTAZ
      ! ARXL, CLENTH, NRAINC, NLYRBT, cellarea, HOUR_FROM_DATE
      ! ZERO, FIRST_frmb, DTUZ, MBLINK, MBFACE, total_no_links, UZNOW, TIMB
      ! MBFLAG, MBMON, MBDAY, MBYEAR, TIH

      IMPLICIT NONE

      INTEGER, PARAMETER :: MBHOUR = 0, MBMIN = 0
      DOUBLE PRECISION, PARAMETER :: MPMM = 1.0D-3

      ! Modernized DATA statement into parameter array initialization
      INTEGER, PARAMETER :: MONEND (12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      INTEGER :: IEL, IPSTN, ICBOTM, IL, I, ICL, LYEAR
      DOUBLE PRECISION :: AT, QBK, AREAE, AREAEM
      DOUBLE PRECISION :: PRECM, CEVAPM, SEVAPM, TRANSM, AQFLXM, DISCHM, BFLOW
      CHARACTER (LEN=50) :: AIOSTO
      LOGICAL :: r

      ! Water flow mass bal variables (BALANC) are (time integrals of):
      ! 1    precipitation
      ! 2    canopy evaporation
      ! 3    evaporation from soil or surface water
      ! 4    transpiration
      ! 5    regional aquifer upflow (flow through the model base)
      ! 6    outlet discharge
      ! 7-12 cumulative totals for variables 1-6
      ! 13   storage in canopy
      ! 14      "    in snowpack
      ! 15      "    in subsurface
      ! 16      "    in surface water
      ! 17      "    in channels
      ! 18   aquifer-channel flow (through channel bed and sides)
      ! 19   cumulative aquifer-channel flow
      !----------------------------------------------------------------------*

      ! Initialization
      ! Replaced ALINIT with array slice
      IF (FIRST_frmb) BALANC(1:19) = ZERO
      FIRST_frmb = .FALSE.

      ! Calculate water volumes based on flow rates
      !     * variables 1-5 (and 7-11)
      PRECM = ZERO
      CEVAPM = ZERO
      SEVAPM = ZERO
      TRANSM = ZERO
      AQFLXM = ZERO

      DO IEL = 1, total_no_elements
         IPSTN = NRAINC (IEL)
         ICBOTM = NLYRBT (IEL, 1) - 1
         AT = cellarea (IEL) * DTUZ
         PRECM = PRECM + precip_m_per_s(IEL) * AT
         CEVAPM = CEVAPM + EINTA (IEL) * AT
         SEVAPM = SEVAPM + EEVAP (IEL) * AT
         TRANSM = TRANSM + ERZA (IEL) * AT
         AQFLXM = AQFLXM + QVSV (ICBOTM, IEL) * AT
      END DO

      !     * variable 6 (and 12)
      DISCHM = ZERO
      IF (MBLINK /= 0) DISCHM = ABS (QOC (MBLINK, MBFACE) * DTUZ)

      !     * variable 18 (and 19)
      BFLOW = ZERO
      DO IL = 1, total_no_links
         QBK = QBKB (IL, 1) + QBKB (IL, 2) + QBKF (IL, 1) + QBKF (IL, 2)
         BFLOW = BFLOW + QBK * DTUZ
      END DO

      ! Update BALANC (note: elements 1:6 & 18 may be reset to zero below)
      DO I = 0, 6, 6
         BALANC (I + 1) = BALANC (I + 1) + PRECM
         BALANC (I + 2) = BALANC (I + 2) + CEVAPM
         BALANC (I + 3) = BALANC (I + 3) + SEVAPM
         BALANC (I + 4) = BALANC (I + 4) + TRANSM
         BALANC (I + 5) = BALANC (I + 5) + AQFLXM
         BALANC (I + 6) = BALANC (I + 6) + DISCHM
         BALANC (18 + I / 6) = BALANC (18 + I / 6) + BFLOW
      END DO

      ! -------------- Proceed only if output is required now -------------- *

      IF (UZNOW < TIMB) RETURN

      ! Calculate water volumes based on storage
      ! Replaced ALINIT with array slice (13 through 17 is 5 elements)
      BALANC(13:17) = ZERO

      DO IEL = total_no_links + 1, total_no_elements
         AREAE = cellarea (IEL)
         AREAEM = AREAE * MPMM
         BALANC (13) = BALANC (13) + CSTORE (IEL) * AREAEM
         BALANC (14) = BALANC (14) + SD (IEL) * RHOSAR (IEL) * AREAEM
         BALANC (16) = BALANC (16) + (GETHRF (IEL) - ZGRUND (IEL)) * AREAE

         DO ICL = NLYRBT (IEL, 1), top_cell_no
            BALANC (15) = BALANC (15) + VSTHE (ICL, IEL) * DELTAZ (ICL, IEL) * AREAE
         END DO
      END DO

      DO IL = 1, total_no_links
         BALANC (17) = BALANC (17) + ARXL (IL) * CLENTH (IL)
      END DO

      ! Output the data
      AIOSTO (:49) = ' '
      AIOSTO (50:) = '1'

      CALL FRRESP (AIOSTO, UZNOW, .TRUE.)

      ! Calculate the next output time
      IF (MBFLAG == 1) THEN
         ! * next day
         LYEAR = 0

         IF (MOD(MBYEAR, 4) == 0) THEN
            IF (MOD(MBYEAR, 100) == 0) THEN
               r = MOD(MBYEAR, 400) == 0
            ELSE
               r = .TRUE.
            END IF
         ELSE
            r = .FALSE.
         END IF

         IF (r .AND. MBMON == 2) LYEAR = 1
         MBDAY = MOD (MBDAY, MONEND (MBMON) + LYEAR) + 1
      ELSE
         ! * next month
         MBDAY = 1
      END IF

      IF (MBDAY == 1) THEN
         MBMON = MOD (MBMON, 12) + 1
         IF (MBMON == 1) MBYEAR = MBYEAR + 1
      END IF

      TIMB = HOUR_FROM_DATE(MBYEAR, MBMON, MBDAY, MBHOUR, MBMIN) - TIH

      ! Initialise all short period flow data
      ! Replaced ALINIT with array slice
      BALANC(1:6) = ZERO
      BALANC (18) = ZERO

   END SUBROUTINE FRMB



   !SSSSSS SUBROUTINE FROPEN
   SUBROUTINE FROPEN
   !----------------------------------------------------------------------*
   !
   ! OPEN I/O DATA FILES
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/FR/FROPEN/4.0
   ! Modifications:
   ! RAH  941003 3.4.1 Amend writes 1030,1050: append CNAM to RUNFIL.
   ! RAH  970223  4.0  Specify STATUS on OPEN(2).  Write to, & rewind, TIM.
   !                   Units 27,28,22,14 were 18,19,36,18 (see FRINIT).
   !----------------------------------------------------------------------*
   ! Commons and constants

      ! Assumed external module dependencies providing global variables:
      ! BTIME, BANNER, DIRQQ, FILNAM, CNAM, ista, isextradis, iszq,
      ! isextrapsl, ismn, visualisation_plan_filename, visualisation_check_filename,
      ! hdf5filename, RESFIL, TIM

      IMPLICIT NONE

      INTEGER :: I, ios
      CHARACTER (LEN=200) :: FILNAM2

      !----------------------------------------------------------------------*
      !
      BTIME = .FALSE.

      ! WRITE BANNER HEADER TO SCREEN
      WRITE (*,*)
      WRITE (*,*) '**************************'
      WRITE (*,*) BANNER
      WRITE (*,*) '**************************'
      WRITE (*,*)

      ! ****sb 161213
      ista = .TRUE.
      ! ****sb 220415
      isextradis = .TRUE.
      !***ZQ Module 200520
      iszq = .TRUE.
      !sb 110324
      isextrapsl = .TRUE.
      !sb 230925
      ismn = .TRUE.

      OPEN (2, FILE = FILNAM, STATUS = 'OLD', IOSTAT = ios)
      IF (ios /= 0) CALL stop_rundata_error(CNAM)

      FILNAM2 = TRIM(DIRQQ) // 'info_' // TRIM(CNAM) // '_SHETRAN_log.txt'

      OPEN (61, FILE = FILNAM2, IOSTAT = ios)
      IF (ios /= 0) CALL stop_open_error(FILNAM2)

      READ (2, '(A)', IOSTAT = ios) FILNAM
      IF (ios /= 0) CALL stop_rundata_error(CNAM)

      !***ZQ Module 200520 change log file to unit 52 and read DO 100 I = 10, 51 (was 50)
      !***extra psl 110324 change log file to unit 53 and read DO 100 I = 10, 52 (was 50). see extra lines at the end
      !***nitrate 230925 change log file to unit 61 and read DO 100 I = 10, 60 (was 50). see extra lines at the end
      WRITE (61, '(A)') FILNAM
      WRITE (61, *)

      ! Main file reading loop
      DO I = 10, 50
         READ (2, '(A)', IOSTAT = ios) FILNAM

         IF (ios < 0) THEN
            IF (I < 14) CALL stop_eof_error(CNAM)
            iszq = .FALSE.
            isextrapsl = .FALSE.
            ismn = .FALSE.
            CLOSE (2)
            RETURN
         END IF

         WRITE (61, '(A)') FILNAM
         READ (2, '(A)', IOSTAT = ios) FILNAM

         IF (ios < 0) THEN
            IF (I < 14) CALL stop_eof_error(CNAM)
            iszq = .FALSE.
            isextrapsl = .FALSE.
            ismn = .FALSE.
            CLOSE (2)
            RETURN
         END IF

         IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
            !***Sb 161213
            IF (I == 45 .OR. I == 46) ista = .FALSE.
            !***Sb 220415
            IF (I == 47) isextradis = .FALSE.

            WRITE (61, '("- NOT USED")')
         ELSE
            FILNAM = TRIM(DIRQQ) // TRIM(FILNAM)
            IF (I == 48) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               visualisation_plan_filename = FILNAM
            ELSE IF (I == 49) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               visualisation_check_filename = FILNAM
            ELSE IF (I == 50) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               hdf5filename = FILNAM
            ELSE
               WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') I, FILNAM

               OPEN (I, FILE = FILNAM, IOSTAT = ios)
               IF (ios /= 0) THEN
                  WRITE (*,'(A,A)') ' Error opening the file ', TRIM(FILNAM)
                  WRITE (*,'("paused, type [enter] to continue")')
                  READ (*,*)
                  STOP
               END IF

               IF (I == 27) RESFIL = FILNAM
               IF (I == 22) THEN
                  BTIME = .TRUE.
                  WRITE (TIM, *) 'Reading data sets ...'
                  REWIND (TIM)
               END IF
            END IF
         END IF
      END DO

      !***ZQ Module 200520
      READ (2, '(A)', IOSTAT = ios) FILNAM
      IF (ios < 0) THEN
         iszq = .FALSE.
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      READ (2, '(A)', IOSTAT = ios) FILNAM

      IF (ios < 0) THEN
         iszq = .FALSE.
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         iszq = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         OPEN (51, FILE = FILNAM, IOSTAT = ios)
         IF (ios /= 0) CALL stop_open_error(FILNAM)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 51, FILNAM
      END IF

      !extra psl 110324
      READ (2, '(A)', IOSTAT = ios) FILNAM
      IF (ios < 0) THEN
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      READ (2, '(A)', IOSTAT = ios) FILNAM

      IF (ios < 0) THEN
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         isextrapsl = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         FILNAM2 = TRIM(DIRQQ) // FILNAM
         OPEN (52, FILE = FILNAM2, IOSTAT = ios)
         IF (ios /= 0) CALL stop_open_error(FILNAM2)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 52, FILNAM2
      END IF

      !nitrate component 230925
      READ (2, '(A)', IOSTAT = ios) FILNAM
      IF (ios < 0) THEN
         ismn = .FALSE.
         CLOSE (2)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      READ (2, '(A)', IOSTAT = ios) FILNAM

      IF (ios < 0) THEN
         ismn = .FALSE.
         CLOSE (2)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         ismn = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         FILNAM = TRIM(DIRQQ) // FILNAM
         OPEN (53, FILE = FILNAM, IOSTAT = ios)
         IF (ios /= 0) CALL stop_open_error(FILNAM)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 53, FILNAM
      END IF

      ! Remaining nitrate files
      DO I = 54, 60
         READ (2, '(A)', IOSTAT = ios) FILNAM
         IF (ios < 0) EXIT

         WRITE (61, '(A)') FILNAM
         READ (2, '(A)', IOSTAT = ios) FILNAM
         IF (ios < 0) EXIT

         IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
            WRITE (61, '("- NOT USED")')
         ELSE
            FILNAM = TRIM(DIRQQ) // FILNAM
            OPEN (I, FILE = FILNAM, IOSTAT = ios)
            IF (ios /= 0) CALL stop_open_error(FILNAM)
            WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') I, FILNAM
         END IF
      END DO

      CLOSE (2)

      RETURN

   CONTAINS

      ! Internal helpers to cleanly exit without jumping to bottom labels
      SUBROUTINE stop_eof_error(c_name)
         CHARACTER(LEN=*), INTENT(IN) :: c_name
         WRITE (*, '("UNEXPECTED -EOF- ON FILE ",A)') c_name
         STOP 'ABNORMAL END'
      END SUBROUTINE stop_eof_error

      SUBROUTINE stop_rundata_error(c_name)
         CHARACTER(LEN=*), INTENT(IN) :: c_name
         WRITE (*, '("ERROR OPENING RUNDATA FILE ",A)') c_name
         STOP 'ABNORMAL END'
      END SUBROUTINE stop_rundata_error

      SUBROUTINE stop_open_error(f_name)
         CHARACTER(LEN=*), INTENT(IN) :: f_name
         WRITE (*, '("ERROR OPENING FILE ",A)') f_name
         STOP 'ABNORMAL END'
      END SUBROUTINE stop_open_error

   END SUBROUTINE FROPEN



   !SSSSSS SUBROUTINE FROUTPUT
      SUBROUTINE FROUTPUT(SIMPOS)
      !----------------------------------------------------------------------*
      ! Generates output files for discharge, sediment, contaminants,
      ! water tables, and mass balances at various simulation stages.
      !
      ! SIMPOS == 'start' : initialise output files and persistent state
      ! SIMPOS == 'main*' : accumulate and write timestep/hourly outputs
      ! otherwise         : write final-state output for restart/initialisation
      !
      ! Assumed global variables provided via host module(s):
      ! ISextradis, disextra, total_no_links, ISextrapsl, pslextra,
      ! total_no_elements, DIRQQ, cnam, dis2, mas, dis, toutput,
      ! ICMREF, NOCBCC, NOCBCD, uznow, BHOTRD, bhotti, bexsy, bexcm,
      ! QSED, RHOSED, CCCC, top_cell_no, qoc, tih, icounter2, balanc,
      ! carea, zgrund, zvspsl, vse, bexbk, VSPSI, nlyrbt, FFFATAL, PPPRI
      !----------------------------------------------------------------------*

         IMPLICIT NONE

         ! Dummy arguments
         CHARACTER(LEN=5), INTENT(IN) :: SIMPOS

         ! Parameters
         INTEGER, PARAMETER :: SEDALLUNIT  = 681
         INTEGER, PARAMETER :: SEDFINEUNIT = 682
         INTEGER, PARAMETER :: PSLFILEUNIT = 683
         INTEGER, PARAMETER :: CONTAMUNIT  = 684

         DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0
         DOUBLE PRECISION, PARAMETER :: ONE  = 1.0D0

         ! Locals: strings
         CHARACTER(LEN=20)  :: disextratext, pslextratext
         CHARACTER(LEN=256) :: filnam

         ! Locals: scalars
         INTEGER :: L, iface, nminel, i, j, iel, ios
         INTEGER :: hour_now
         DOUBLE PRECISION :: qocav, qocold
         DOUBLE PRECISION :: sedav, sedfineav, contamav
         DOUBLE PRECISION :: uznowt

         ! Persistent state between calls
         INTEGER, SAVE :: disextrapoints = 0
         INTEGER, SAVE :: pslextrapoints = 0
         DOUBLE PRECISION, SAVE :: uzold = ZERO
         DOUBLE PRECISION, SAVE :: next_hour = ZERO
         DOUBLE PRECISION, SAVE :: qoctot = ZERO
         DOUBLE PRECISION, SAVE :: sedtot = ZERO
         DOUBLE PRECISION, SAVE :: sedfinetot = ZERO
         DOUBLE PRECISION, SAVE :: contamtot = ZERO

         ! Persistent optional-output metadata/state
         INTEGER, ALLOCATABLE, SAVE :: pslextraelement(:)
         INTEGER, ALLOCATABLE, SAVE :: disextraelement(:), disextraface(:)
         DOUBLE PRECISION, ALLOCATABLE, SAVE :: qocavextra(:)

      !----------------------------------------------------------------------*

         SELECT CASE (SIMPOS)

         CASE ('start')
            CALL initialise_output()

         CASE DEFAULT
            IF (SIMPOS(1:4) == 'main') THEN
               CALL write_main_output()
            ELSE
               CALL write_final_state()
            END IF

         END SELECT

      CONTAINS


         SUBROUTINE initialise_output()
         !----------------------------------------------------------------------*
         ! Initialise regular and optional output streams.  The optional point
         ! lists are compacted in-place: invalid element/link IDs are skipped and
         ! the retained count is written back to disextrapoints/pslextrapoints.
         !----------------------------------------------------------------------*

            IF (ISextradis) CALL initialise_extra_discharge_points()
            IF (ISextrapsl) CALL initialise_extra_water_table_output()

            CALL write_checked(dis2, &
                 'Simulated discharge at the outlet at every model timestep.', &
                 'Error writing to the discharge every timestep at the catchment outlet file ' // &
                 '(unit 41 in the rundata file)')

            WRITE(dis2, '(A)', IOSTAT=ios) &
                 'Date_yyyy-mm-dd HH:MM:SS,Time(hours),Outlet_Discharge(m3/s)'

            CALL write_checked(mas, &
                 'Spatially Averaged Totals (mm) over the simulation', &
                 'Error writing to the the mass balance data file (unit 43 in the rundata file)')

            WRITE(mas, '(A)') &
                 'Time(Hours),' // &
                 'Cumulative_Precipitation,' // &
                 'Cumulative_Canopy_Evaporation,' // &
                 'Cumulative_Soil_Evaporation,' // &
                 'Cumulative_Transpiration,' // &
                 'Cumulative_Aquifer_Flow,' // &
                 'Cumulative_Discharge,' // &
                 'Canopy_Storage,' // &
                 'Snow_Storage,' // &
                 'Subsurface_Storage,' // &
                 'Land_Surface_Storage,' // &
                 'Channel_Storage'

            WRITE(dis, '(A,F8.2,A)', IOSTAT=ios) &
                 'Simulated discharge(m3/s) at the outlet - regular timestep ', &
                 TOUTPUT, &
                 ' hours. Simulated discharge is the mean value over the timestep ' // &
                 'with the date at the start of the timestep'
            CALL stop_on_io_error(ios, &
                 'Error writing to the regular discharge at the catchment outlet file ' // &
                 '(unit 44 in the rundata file)')

            CALL find_mass_balance_outlet()
            CALL write_discharge_header()

            uznowt    = uznow / TOUTPUT
            next_hour = DBLE(INT(uznowt)) + ONE

            ! Hotstart first time is correct.
            IF (BHOTRD) uzold = DBLE(INT(bhotti / TOUTPUT))

            IF (bexsy) CALL initialise_sediment_output()
            IF (bexcm) CALL initialise_contaminant_output()

         END SUBROUTINE initialise_output


         SUBROUTINE initialise_extra_discharge_points()
            READ(disextra, *, IOSTAT=ios)
            CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

            READ(disextra, *, IOSTAT=ios) disextratext, disextrapoints
            CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

            CALL allocate_extra_discharge(disextrapoints)

            j = 0
            DO i = 1, disextrapoints
               READ(disextra, *, IOSTAT=ios) L, iface
               CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

               ! Silently ignore discharge requests beyond the link range, matching
               ! the original behaviour while keeping the retained list compact.
               IF (L <= total_no_links) THEN
                  j = j + 1
                  disextraelement(j) = L
                  disextraface(j)    = iface
               END IF
            END DO

            disextrapoints = j
         END SUBROUTINE initialise_extra_discharge_points


         SUBROUTINE allocate_extra_discharge(n)
            INTEGER, INTENT(IN) :: n

            IF (ALLOCATED(disextraelement)) DEALLOCATE(disextraelement)
            IF (ALLOCATED(disextraface))    DEALLOCATE(disextraface)
            IF (ALLOCATED(qocavextra))      DEALLOCATE(qocavextra)
            IF (ALLOCATED(qoctotextra))     DEALLOCATE(qoctotextra)

            ALLOCATE(disextraelement(n), disextraface(n), qocavextra(n), qoctotextra(n))

            disextraelement = 0
            disextraface    = 0
            qocavextra      = ZERO
            qoctotextra     = ZERO
         END SUBROUTINE allocate_extra_discharge


         SUBROUTINE initialise_extra_water_table_output()
            READ(pslextra, *, IOSTAT=ios)
            CALL fatal_on_io_error(ios, 1069, &
                 'no or incorrect data in input_CATCH_water_table_depth file')

            READ(pslextra, *, IOSTAT=ios) pslextratext, pslextrapoints
            CALL fatal_on_io_error(ios, 1069, &
                 'no or incorrect data in input_CATCH_water_table_depth file')

            IF (ALLOCATED(pslextraelement)) DEALLOCATE(pslextraelement)
            ALLOCATE(pslextraelement(pslextrapoints))
            pslextraelement = 0

            j = 0
            DO i = 1, pslextrapoints
               READ(pslextra, *, IOSTAT=ios) iel
               CALL fatal_on_io_error(ios, 1069, &
                    'no or incorrect data in input_CATCH_water_table_depth file')

               ! Silently ignore water-table requests beyond the element range,
               ! preserving the original compaction behaviour.
               IF (iel <= total_no_elements) THEN
                  j = j + 1
                  pslextraelement(j) = iel
               END IF
            END DO

            pslextrapoints = j

            filnam = TRIM(DIRQQ) // 'output_' // TRIM(cnam) // '_water_table_depth.csv'
            OPEN(PSLFILEUNIT, FILE=filnam, IOSTAT=ios)
            CALL fatal_on_io_error(ios, 1069, 'Error opening water table depth file')

            WRITE(PSLFILEUNIT, '(A)') &
                 'Water_Table_depth(m_below_ground). A negative number ' // &
                 'means there is surface water with the absolute value ' // &
                 'the depth of surface water'
            WRITE(PSLFILEUNIT, '(A,*(A,I0))') 'Time(hours)', &
                 (', Element-', pslextraelement(j), j = 1, pslextrapoints)
         END SUBROUTINE initialise_extra_water_table_output


         SUBROUTINE find_mass_balance_outlet()
         !----------------------------------------------------------------------*
         ! Find outlet link for mass-balance output when no reservoir files exist.
         ! The outlet must be a weir boundary condition, type 7.
         !----------------------------------------------------------------------*
            mblink = 0
            mbface = 0

            DO L = 1, total_no_links
               DO iface = 1, 4
                  IF (ICMREF(L, 4 + iface) == 0 .AND. NOCBCC(L) > 0) THEN
                     IF (NOCBCD(NOCBCC(L), 3) == 7) THEN
                        mblink = L
                        mbface = NOCBCD(NOCBCC(L), 2)
                     END IF
                  END IF
               END DO
            END DO
         END SUBROUTINE find_mass_balance_outlet


         SUBROUTINE write_discharge_header()
            IF (ISextradis) THEN
               WRITE(dis, '(*(A,I0))') &
                    'Date_yyyy-mm-dd HH:MM:SS,Time(hours),Outlet-', &
                    mblink, (',Channel-', disextraelement(j), j = 1, disextrapoints)
            ELSE
               WRITE(dis, '(A)') &
                    'Date_yyyy-mm-dd HH:MM:SS,Time(hours),Outlet-Discharge'
            END IF
         END SUBROUTINE write_discharge_header


         SUBROUTINE initialise_sediment_output()
            filnam = TRIM(DIRQQ) // 'output_' // TRIM(cnam) // '_sediment_all.csv'
            OPEN(SEDALLUNIT, FILE=filnam)

            filnam = TRIM(DIRQQ) // 'output_' // TRIM(cnam) // '_sediment_fine.csv'
            OPEN(SEDFINEUNIT, FILE=filnam)

            WRITE(SEDALLUNIT, '(A)', IOSTAT=ios) &
                 'Sediment discharge at the outlet - All Sediments. ' // &
                 'This is the mean value over the timestep with the date at the start of the timestep'
            CALL stop_on_io_error(ios, 'Error writing to the sed-all-daily-output.csv file')
            WRITE(SEDALLUNIT, '(A)') &
                 'Date_yyyy-mm-dd HH:MM:SS,Time(hours),Outlet-Discharge(kg/s)'

            WRITE(SEDFINEUNIT, '(A)', IOSTAT=ios) &
                 'Sediment discharge at the outlet - Fine Sediments. ' // &
                 'This is the mean value over the timestep with the date at the start of the timestep'
            CALL stop_on_io_error(ios, 'Error writing to the sed-fine-daily-output.csv file')
            WRITE(SEDFINEUNIT, '(A)') &
                 'Date_yyyy-mm-dd HH:MM:SS,Time(hours),Outlet-Discharge(kg/s)'

            sedav = ZERO
         END SUBROUTINE initialise_sediment_output


         SUBROUTINE initialise_contaminant_output()
            filnam = TRIM(DIRQQ) // 'output_' // TRIM(cnam) // '_contaminant.csv'
            OPEN(CONTAMUNIT, FILE=filnam)

            WRITE(CONTAMUNIT, '(A)', IOSTAT=ios) &
                 'Contaminant Relative Concentration (contaminant 1) at the outlet. ' // &
                 'This is the mean value over the timestep with the date at the start of the timestep.'
            CALL stop_on_io_error(ios, 'Error writing to the contaminant.csv file')
            WRITE(CONTAMUNIT, '(A)') &
                 'Date_yyyy-mm-dd HH:MM:SS,Time(hours),Relative_concentration'
         END SUBROUTINE initialise_contaminant_output


         SUBROUTINE write_main_output()
         !----------------------------------------------------------------------*
         ! Accumulate mean values in normalised output-time units.  When the
         ! current model time crosses one or more regular output boundaries, write
         ! one row for the just-completed interval and fill any skipped regular
         ! intervals with the current timestep average.
         !----------------------------------------------------------------------*

            CALL sample_current_values(qocav, sedav, sedfineav, contamav)

            uznowt   = uznow / TOUTPUT
            hour_now = INT(uznowt)

            IF (hour_now < INT(next_hour)) THEN
               CALL accumulate_interval(uznowt - uzold, qocav, sedav, sedfineav, contamav)
            ELSE
               CALL accumulate_interval(next_hour - uzold, qocav, sedav, sedfineav, contamav)
               CALL write_completed_regular_outputs(hour_now, qocav, sedav, sedfineav, contamav)
               CALL restart_accumulators(uznowt - next_hour, qocav, sedav, sedfineav, contamav)

               next_hour = next_hour + ONE
            END IF

            CALL WRITE_DIS2(mbface, qocav, uznow)
            CALL write_periodic_mass_balance()

            uzold = uznowt

            ! temp sb 250925 for when doing 1d simulations
            IF (mblink == 0 .AND. mbface == 0) THEN
               qocav = ZERO
            ELSE
               qocold = qoc(mblink, mbface)
            END IF

         END SUBROUTINE write_main_output


         SUBROUTINE sample_current_values(q_out, sed_out, sedfine_out, contam_out)
            DOUBLE PRECISION, INTENT(OUT) :: q_out
            DOUBLE PRECISION, INTENT(OUT) :: sed_out
            DOUBLE PRECISION, INTENT(OUT) :: sedfine_out
            DOUBLE PRECISION, INTENT(OUT) :: contam_out

            IF (mblink == 0 .AND. mbface == 0) THEN
               q_out       = ZERO
               sed_out     = ZERO
               sedfine_out = ZERO
               contam_out  = ZERO
            ELSE
               q_out = qoc(mblink, mbface)

               IF (bexsy) THEN
                  sed_out = ZERO
                  DO i = 1, nsed
                     sed_out = sed_out + QSED(mblink, i, mbface) * RHOSED
                  END DO
                  sedfine_out = QSED(mblink, 1, mbface) * RHOSED
               ELSE
                  sed_out     = ZERO
                  sedfine_out = ZERO
               END IF

               IF (bexcm) THEN
                  contam_out = CCCC(mblink, top_cell_no, 1)
               ELSE
                  contam_out = ZERO
               END IF
            END IF

            IF (ISextradis) THEN
               DO i = 1, disextrapoints
                  qocavextra(i) = qoc(disextraelement(i), disextraface(i))
               END DO
            END IF
         END SUBROUTINE sample_current_values


         SUBROUTINE accumulate_interval(dt, q_mean, sed_mean, sedfine_mean, contam_mean)
            DOUBLE PRECISION, INTENT(IN) :: dt
            DOUBLE PRECISION, INTENT(IN) :: q_mean
            DOUBLE PRECISION, INTENT(IN) :: sed_mean
            DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
            DOUBLE PRECISION, INTENT(IN) :: contam_mean

            qoctot = qoctot + q_mean * dt

            IF (bexsy) THEN
               sedtot     = sedtot     + sed_mean     * dt
               sedfinetot = sedfinetot + sedfine_mean * dt
            END IF

            IF (bexcm) contamtot = contamtot + contam_mean * dt

            IF (ISextradis) THEN
               do i = 1, disextrapoints
                   qoctotextra(i) = qoctotextra(i) + qocavextra(i) * dt
               end do
            END IF
         END SUBROUTINE accumulate_interval


         SUBROUTINE write_completed_regular_outputs(hour_now, q_mean, sed_mean, sedfine_mean, contam_mean)
            INTEGER, INTENT(IN) :: hour_now
            DOUBLE PRECISION, INTENT(IN) :: q_mean
            DOUBLE PRECISION, INTENT(IN) :: sed_mean
            DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
            DOUBLE PRECISION, INTENT(IN) :: contam_mean

            DOUBLE PRECISION :: output_hour

            ! output_hour = next_hour - 1.0D0:
            !     mean value over the regular timestep, timestamped at the start
            !     of the timestep.
            ! output_hour = next_hour:
            !     equivalent mean value timestamped at the end of the timestep.
            output_hour = next_hour - ONE

            CALL write_regular_outputs(output_hour, ABS(qoctot), qoctotextra, &
                                       sedtot, sedfinetot, contamtot)

            DO i = INT(next_hour) + 1, hour_now
               next_hour   = DBLE(i)
               output_hour = next_hour - ONE

               CALL write_regular_outputs(output_hour, ABS(q_mean), qocavextra, &
                                          sed_mean, sedfine_mean, contam_mean)
            END DO
         END SUBROUTINE write_completed_regular_outputs


         SUBROUTINE restart_accumulators(dt, q_mean, sed_mean, sedfine_mean, contam_mean)
            DOUBLE PRECISION, INTENT(IN) :: dt
            DOUBLE PRECISION, INTENT(IN) :: q_mean
            DOUBLE PRECISION, INTENT(IN) :: sed_mean
            DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
            DOUBLE PRECISION, INTENT(IN) :: contam_mean

            qoctot = q_mean * dt

            IF (bexsy) THEN
               sedtot     = sed_mean     * dt
               sedfinetot = sedfine_mean * dt
            END IF

            IF (bexcm) contamtot = contam_mean * dt

            IF (ISextradis) THEN
               qoctotextra(1:disextrapoints) = qocavextra(1:disextrapoints) * dt
               do i = 1, disextrapoints
                   qoctotextra(i) = qocavextra(i) * dt
               end do
            END IF
         END SUBROUTINE restart_accumulators


         SUBROUTINE write_regular_outputs(output_hour, discharge, discharge_extra, sediment, sediment_fine, contaminant)
            DOUBLE PRECISION, INTENT(IN) :: output_hour
            DOUBLE PRECISION, INTENT(IN) :: discharge
            DOUBLE PRECISION, INTENT(IN) :: discharge_extra(:)
            DOUBLE PRECISION, INTENT(IN) :: sediment
            DOUBLE PRECISION, INTENT(IN) :: sediment_fine
            DOUBLE PRECISION, INTENT(IN) :: contaminant

            CHARACTER(LEN=32) :: stamp
            DOUBLE PRECISION  :: elapsed

            elapsed = output_hour * TOUTPUT
            stamp   = timestamp_from_output_hour(output_hour)

            IF (ISextradis) THEN
               WRITE(dis, '(A,'','',F0.3,'','',F0.5,*( '','',F0.5 ))') &
                    TRIM(stamp), elapsed, discharge, &
                    (ABS(discharge_extra(j)), j = 1, disextrapoints)
            ELSE
               WRITE(dis, '(A,'','',F0.3,'','',F0.5)') &
                    TRIM(stamp), elapsed, discharge
            END IF

            IF (bexsy) THEN
               WRITE(SEDALLUNIT,  '(A,'','',F0.3,'','',F0.5)') TRIM(stamp), elapsed, sediment
               WRITE(SEDFINEUNIT, '(A,'','',F0.3,'','',F0.5)') TRIM(stamp), elapsed, sediment_fine
            END IF

            IF (bexcm) THEN
               WRITE(CONTAMUNIT, '(A,'','',F0.3,'','',F0.5)') TRIM(stamp), elapsed, contaminant
            END IF
         END SUBROUTINE write_regular_outputs


         FUNCTION timestamp_from_output_hour(output_hour) RESULT(stamp)
            DOUBLE PRECISION, INTENT(IN) :: output_hour
            CHARACTER(LEN=32) :: stamp
            INTEGER :: c(6)

            c = DATE_FROM_HOUR(tih + output_hour * TOUTPUT)

            WRITE(stamp, '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
                 c(1), c(2), c(3), c(4), c(5), c(6)
         END FUNCTION timestamp_from_output_hour


         SUBROUTINE write_periodic_mass_balance()
            IF (uznow <= icounter2) RETURN

            WRITE(mas, '(F16.3,11('','',F16.3))') uznow, &
                 balanc(7)  * 1000.0D0 / carea, &
                 balanc(8)  * 1000.0D0 / carea, &
                 balanc(9)  * 1000.0D0 / carea, &
                 balanc(10) * 1000.0D0 / carea, &
                 balanc(11) * 1000.0D0 / carea, &
                 balanc(12) * 1000.0D0 / carea, &
                 balanc(13) * 1000.0D0 / carea, &
                 balanc(14) * 1000.0D0 / carea, &
                 balanc(15) * 1000.0D0 / carea, &
                 balanc(16) * 1000.0D0 / carea, &
                 balanc(17) * 1000.0D0 / carea

            icounter2 = icounter2 + 24.0D0

            IF (ISextrapsl) THEN
               WRITE(PSLFILEUNIT, '(F10.2,*(1A,F10.2))') uznow, &
                    (',', zgrund(pslextraelement(i)) - zvspsl(pslextraelement(i)), &
                     i = 1, pslextrapoints)
            END IF
         END SUBROUTINE write_periodic_mass_balance


         SUBROUTINE write_final_state()
            WRITE(vse, *) 'Output at end of simulation for use as initial conditions in vsi file'
            WRITE(vse, *) 'This output is by element number'
            WRITE(vse, *)
            WRITE(vse, *) 'phreatic surface level '

            IF (bexbk) THEN
               nminel = 1
            ELSE
               nminel = total_no_links + 1
            END IF

            WRITE(vse, '(10(1X,F9.3))') (zvspsl(j), j = nminel, total_no_elements)
            WRITE(vse, *)
            WRITE(vse, *) 'Heads at end of simulation'

            DO iel = 1, total_no_elements
               IF (bexbk .OR. iel > total_no_links) THEN
                  WRITE(vse, '(I7)') iel
                  WRITE(vse, '(10(1X,F9.3))') &
                       (VSPSI(j, iel), j = nlyrbt(iel, 1), top_cell_no)
               END IF
            END DO
         END SUBROUTINE write_final_state


         SUBROUTINE write_checked(unit, line, error_message)
            INTEGER,          INTENT(IN) :: unit
            CHARACTER(LEN=*), INTENT(IN) :: line
            CHARACTER(LEN=*), INTENT(IN) :: error_message

            WRITE(unit, '(A)', IOSTAT=ios) line
            CALL stop_on_io_error(ios, error_message)
         END SUBROUTINE write_checked


         SUBROUTINE stop_on_io_error(io_status, message)
            INTEGER,          INTENT(IN) :: io_status
            CHARACTER(LEN=*), INTENT(IN) :: message

            IF (io_status == 0) RETURN

            WRITE(*, '(A)') message
            WRITE(*, '(A)') 'Check it is not open in other software (e.g. Excel)'
            WRITE(*, '(''paused, type [enter] to continue'')')
            READ(*, *)
            STOP
         END SUBROUTINE stop_on_io_error


         SUBROUTINE fatal_on_io_error(io_status, error_code, message)
            INTEGER,          INTENT(IN) :: io_status
            INTEGER,          INTENT(IN) :: error_code
            CHARACTER(LEN=*), INTENT(IN) :: message

            IF (io_status /= 0) CALL ERROR(FFFATAL, error_code, PPPRI, 0, 0, message)
         END SUBROUTINE fatal_on_io_error

      END SUBROUTINE FROUTPUT



   !SSSSSS SUBROUTINE write_dis
   SUBROUTINE write_dis(mbface, qoo)
      INTEGER, INTENT(IN)            :: mbface
      DOUBLEPRECISION, INTENT(IN)    :: qoo
      DOUBLEPRECISION                :: qd
      IF((mbface==1) .OR. (mbface==2)) THEN
         qd = qoo
      ELSE
         qd = -qoo
      ENDIF
      WRITE(dis,'(F20.8)') qd
   END SUBROUTINE write_dis



   !SSSSSS SUBROUTINE write_dis2
   SUBROUTINE write_dis2(mbface, qoo, tme)
      INTEGER, INTENT(IN)            :: mbface
      INTEGER                        :: c(6)
      DOUBLEPRECISION, INTENT(IN)    :: qoo, tme
      DOUBLEPRECISION                :: qd
      CHARACTER(128)                 :: dum
      IF((mbface==1) .OR. (mbface==2)) THEN
         qd = qoo
      ELSE
         qd = -qoo
      ENDIF
      c = DATE_FROM_HOUR(tih + tme)
      WRITE(dum,'(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') c(1),'-',c(2),'-',c(3),' ', c(4),':',c(5),':',c(6)
!WRITE(dum,'(2(I2.2,A),I4.4,3(A,I2.2))') c(1),'-',c(2),'-',c(3),' ', c(4),':',c(5),':',c(6)
      WRITE(dis2,'(A,A1,F0.5,A1,F0.5)') TRIM(dum), ',',tme, ',',qd
   END SUBROUTINE write_dis2



   !SSSSSS SUBROUTINE FRRESC
   SUBROUTINE FRRESC
   !----------------------------------------------------------------------*
   !
   ! OUTPUT CONTROL (HEADER) DATA TO UNFORMATTED 'RES' FILE.
   ! OPEN UNFORMATTED RESULTS FILES TO HOLD ACTUAL DATA.
   !
   ! NOTE: THE ORDER OF ARRAYS DOESN'T NECESSARILY MATCH THAT OF THE COMMON
   ! BLOCKS, SINCE THE ORDER OF READING THE DATA IS IMPORTANT IN SOME CASES
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/FR/FRRESC/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Explicit typing.
   ! RAH  970223  4.0  Combine DFILE with CFILE (& replace CPR with CMP).
   !                   Add sections IVEG,VEG,ALCB1A.
   !                   Replace NWC,DCONX,DCONY,DDZ,THSAT,NW,CCB,DB,BLOWP,
   !                   CATUZ,NLYRC,EFFSAT,THFC,THWILT with JVS*,NVS*,
   !                   DELTAZ,RDL,VSP*,ZBFULL,ZVSNOD.  See also AL.C,AL.D.
   !                   Amend NLYRBT,NTSOIL loops (J inside, not I).
   !                   Size of ICL* is 14 (was 13).
   ! RAH  970223  4.1  Swap indices: DELTAZ,JVS*,ZVSNOD (see AL.C).
   ! RAH  980713  4.2  Replace [I,N]CATUZ (redundant) with 0.
   !      980722       ICL[IST,NUM] size NCLASS was 14 (see AL.D).
   !                   Replace BEX[UZ,EX,SZ,TS1] with LDUM0 (see AL.D).
   !                   Repace NGRID,NEXPO (also 0) with IDUM0 (see AL.D).
   !                   Replace WIDTF,ZBED,HFLBED,ZFBED,DZFBED,LROOT,HFLBNK,
   !                   PSTART with FDUM0 (see AL.D).  Use I2.2 for ANUM.
   !                   Replace IORES with IDUM0 (was written while unset).
   !                   Rewrite loop 280, & use intrinsic LEN & FORMAT 9300.
   !                   Scrap local FILNAM (then no length limit).
   !----------------------------------------------------------------------*
   ! Commons and constants

      ! Assumed external module dependencies providing global variables:
      ! SHEVER, RES, NX, NY, NGDBGN, total_no_elements, ICMREF, ICMXY,
      ! FRD, VSD, OCD, ETD, PPD, SMD, BKD, SYD, CMD, MED, PRD, EPD, TIM,
      ! PPPRI, SPR, CMP, BUG, HOT, VSI, VED, WLD, LFB, LHB, LGB, BFB, BHB,
      ! OFB, OHB, CMT, CMB, top_cell_no, total_no_links, NS, NV, WWWARN,
      ! EEERR, FFFATAL, NRD, RDF, CAREA, TIH, LINKNS, BEXBK, ICMBK, ICMRF2,
      ! JVSACN, JVSDEL, NLYR, NLYRBT, NBFACE, NHBED, NTSOIL, NVC, NVSSPC,
      ! NVSSPT, NVSWLI, NVSWLT, NWELBT, NWELTP, cellarea, CLENTH, CWIDTH,
      ! DELTAZ, DHF, DXQQ, DYQQ, FHBED, RDL, VSPOR, ZBEFF, ZBFULL, ZGRUND,
      ! ZLYRBT, ZVSNOD, MSM, NM, NRAIN, NSET, NXP1, NYP1, NXM1, NYM1, NXE,
      ! NYE, NXEP1, NYEP1, DTMET, QMAX, BHOTTI, BHOTST, PMAX, PALFA, TMAX,
      ! BWIDTH, TTH, BEXET, BEXOC, BEXSM, BHOTPR, BHOTRD, BEXSY, BEXCM,
      ! NMC, INGRID, NRAINC, IOCORS, ICLNUM, NCLASS, ICLIST, IODATA, IOELEM,
      ! LCODEX, LCODEY, DXIN, DYIN, IOSTA, IOSTEP, IOEND, NVSSOL, VSPPSI,
      ! VSPTHE, VSPKR, VSPETA, VSPDTH, VSPDKR, VSPDET, RESFIL, IORES

      IMPLICIT NONE

      ! Locals, etc
      INTEGER, PARAMETER :: IDUM0 = 0
      DOUBLE PRECISION, PARAMETER :: FDUM0 = 0.0D0
      LOGICAL, PARAMETER :: LDUM0 = .TRUE.

      INTEGER :: I, ICHAR, ISET, J, K, L
      CHARACTER(2) :: ANUM
      CHARACTER(128) :: fname

      !----------------------------------------------------------------------*

      ! WRITE SHETRAN VERSION
      !1
      WRITE (RES) SHEVER

      ! ALGCB1
      !2
      WRITE (RES) NX, NY, NGDBGN, total_no_elements

      ! ALGCB2
      !3-4
      WRITE (RES) ((ICMREF (I, J), I = 1, total_no_elements), J = 1, 12)
      WRITE (RES) ((ICMXY (I, J), I = 1, NX), J = 1, NY)

      ! CFILE + DFILE (except SFB,SRB)
      !5
      WRITE (RES) FRD, VSD, OCD, ETD, PPD, SMD, BKD, SYD, CMD, MED, PRD, &
         EPD, TIM, PPPRI, SPR, CMP, BUG, RES, HOT, VSI, VED, WLD, LFB, LHB, &
         LGB, BFB, BHB, OFB, OHB, CMT, CMB

      ! ALCCB1
      !6
      WRITE (RES) top_cell_no, total_no_links, NS, NV, WWWARN, EEERR, FFFATAL

      ! IVEG
      !7
      WRITE (RES) (NRD (I), I = 1, NV)

      ! VEG
      !8
      WRITE (RES) ((RDF (I, J), J = 1, NRD (I)), I = 1, NV)

      ! CAREA (ALDCB3 - see also below) + ALCB1A
      !9
      WRITE (RES) CAREA, TIH

      ! ALCCB3
      !10-11
      WRITE (RES) (LINKNS (L), L = 1, total_no_links)
      WRITE (RES) BEXBK

      ! ALCCB5
      !12-27
      WRITE (RES) ((ICMBK (I, J), I = 1, total_no_links), J = 1, 2)
      WRITE (RES) ((ICMRF2 (I, J), I = 1, total_no_links), J = 1, 6)
      WRITE (RES) (((JVSACN (K, J, I), K = 1, 4), J = 1, top_cell_no), I = 1, total_no_elements)
      WRITE (RES) (((JVSDEL (K, J, I), K = 1, 4), J = 1, top_cell_no), I = 1, total_no_elements)
      WRITE (RES) (NLYR (I), I = 1, total_no_elements)
      WRITE (RES) ((NLYRBT (I, J), J = 1, NLYR (I)), I = 1, total_no_elements)
      WRITE (RES) (NBFACE (I), I = 1, total_no_elements)
      WRITE (RES) ((NHBED (I, J), I = 1, total_no_links), J = 1, 2)
      WRITE (RES) ((NTSOIL (I, J), J = 1, NLYR (I)), I = 1, total_no_elements)
      WRITE (RES) (NVC (I), I = 1, total_no_elements)
      WRITE (RES) (NVSSPC (I), I = 1, total_no_elements)
      WRITE (RES) (NVSSPT (I), I = 1, total_no_elements)
      WRITE (RES) (NVSWLI (I), I = 1, total_no_elements)
      WRITE (RES) (NVSWLT (I), I = 1, total_no_elements)
      WRITE (RES) (NWELBT (I), I = 1, total_no_elements)
      WRITE (RES) (NWELTP (I), I = 1, total_no_elements)

      ! ALCCB7 (except THSAT)
      !28-42
      WRITE (RES) (cellarea (I), I = 1, total_no_elements)
      WRITE (RES) (CLENTH (I), I = 1, total_no_links)
      WRITE (RES) (CWIDTH (I), I = 1, total_no_links)
      WRITE (RES) ((DELTAZ (J, I), J = 1, top_cell_no), I = 1, total_no_elements)
      WRITE (RES) ((DHF (I, J), I = 1, total_no_elements), J = 1, 4)
      WRITE (RES) (DXQQ (I), I = 1, total_no_elements)
      WRITE (RES) (DYQQ (I), I = 1, total_no_elements)
      WRITE (RES) ((FHBED (I, J), I = 1, total_no_links), J = 1, 2)
      WRITE (RES) (RDL (I), I = 1, NV)
      WRITE (RES) (VSPOR (I), I = 1, NS)
      WRITE (RES) (ZBEFF (I), I = 1, total_no_links)
      WRITE (RES) (ZBFULL (I), I = 1, total_no_links)
      WRITE (RES) (ZGRUND (I), I = 1, total_no_elements)
      WRITE (RES) ((ZLYRBT (I, J), J = 1, NLYR (I)), I = 1, total_no_elements)
      WRITE (RES) ((ZVSNOD (J, I), J = 1, top_cell_no), I = 1, total_no_elements)

      ! ALDCB1 (except MBLINK,MBFACE,MBFLAG)
      !43
      WRITE (RES) MSM, IDUM0, NM, NRAIN, NSET, NXP1, NYP1, NXM1, NYM1, &
         NXE, NYE, NXEP1, NYEP1

      ! ALDCB3 (except CAREA - see above)
      !44
      WRITE (RES) FDUM0, DTMET, QMAX, BHOTTI, BHOTST, PMAX, PALFA, TMAX, BWIDTH, TTH

      ! ALDCB5
      !45
      WRITE (RES) BEXET, LDUM0, LDUM0, BEXOC, LDUM0, BEXSM, LDUM0, &
         BHOTPR, BHOTRD, BEXSY, BEXCM

      ! ALDCB6 (except NOCBCC, NOCBCD)
      !46-59
      WRITE (RES) (NMC (I), I = 1, total_no_elements)
      WRITE (RES) ((INGRID (I, J), I = 1, NX), J = 1, NY)
      WRITE (RES) (NRAINC (I), I = 1, total_no_elements)
      WRITE (RES) (IOCORS (I), I = 1, NSET)
      WRITE (RES) (ICLNUM (I), I = 1, NCLASS)
      WRITE (RES) ((ICLIST (I, J), I = 1, total_no_elements), J = 1, NCLASS)
      WRITE (RES) (IODATA (I), I = 1, NSET)
      WRITE (RES) (IOELEM (I), I = 1, NSET)
      WRITE (RES) ((LCODEX (I, J), I = 1, NX), J = 1, NY)
      WRITE (RES) ((LCODEY (I, J), I = 1, NX), J = 1, NY)

      ! ALDCB8 (except RHOSAR)
      !60-71
      WRITE (RES) (DXIN (I), I = 1, NX)
      WRITE (RES) (DYIN (I), I = 1, NY)
      WRITE (RES) (IOSTA (I), I = 1, NSET)
      WRITE (RES) (IOSTEP (I), I = 1, NSET)
      WRITE (RES) (IOEND (I), I = 1, NSET)

      ! VSSOLI/VSSOLR (except VSPSS, VSPPOR)
      !72-79
      WRITE (RES) NVSSOL
      WRITE (RES) (VSPPSI (I), I = 1, NVSSOL)
      WRITE (RES) ((VSPTHE (I, J), I = 1, NVSSOL), J = 1, NS)
      WRITE (RES) ((VSPKR (I, J), I = 1, NVSSOL), J = 1, NS)
      WRITE (RES) ((VSPETA (I, J), I = 1, NVSSOL), J = 1, NS)
      WRITE (RES) ((VSPDTH (I, J), I = 1, NVSSOL), J = 1, NS)
      WRITE (RES) ((VSPDKR (I, J), I = 1, NVSSOL), J = 1, NS)
      WRITE (RES) ((VSPDET (I, J), I = 1, NVSSOL), J = 1, NS)

      ! CLOSE RES FILE, SO THAT RESULTS CAN BE INSPECTED USING SHEGRAPH BEFORE
      ! SIMULATION HAS TERMINATED
      !
      CLOSE (RES)

      ! OPEN OUTPUT DATA FILES ON FILE UNITS 50 ONWARDS
      !
      IF (NSET > 0) THEN
         ! Modernized: Find the actual length of the filename string
         ICHAR = LEN_TRIM(RESFIL)

         DO ISET = 1, NSET
            IORES (ISET) = 50 + ISET
            WRITE (ANUM, '(I2.2)') ISET
            fname = RESFIL(:ICHAR) // ANUM
            OPEN(IORES(ISET), FILE = TRIM(fname), FORM = 'UNFORMATTED')
            WRITE (*, '(" OPENING FILE UNIT",I3," TO FILE ",2A)') IORES(ISET), RESFIL(:ICHAR), ANUM
         END DO
      END IF

   END SUBROUTINE FRRESC



   !SSSSSS SUBROUTINE FRRESP (AIOSTO, RESNOW, NOW)
   SUBROUTINE FRRESP (AIOSTO, RESNOW, NOW)
   !----------------------------------------------------------------------*
   !
   ! WRITE DATA TO UNFORMATTED RESULT FILES.
   !
   ! DATA IS ONLY WRITTEN (ON A PARTICULAR CALL TO THIS SUBROUTINE) FOR THE
   !   DATA TYPES INDICATED BY A '1' IN ARGUMENT AIOSTO. THIS ROUTINE CAN
   !   THEREFORE BE CALLED SELECTIVELY FROM DIFFERENT PARTS OF SHETRAN.
   !
   ! DATA OUTPUT IS DETERMINED BY USER-DEFINED OUTPUT SETS & DATA CLASSES.
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/FR/FRRESP/4.2

      ! Assumed external module dependencies providing global variables:
      ! SEDSRT, total_no_elements, DLS, DLSSRT, GNUCUM, GNU, PREVTM, NSET,
      ! IOTIME, IOEND, IODATA, IOELEM, ICLNUM, IOCORS, NSED, ICLIST, PNETTO,
      ! EPOT, ERZA, ESOILA, EINTA, DRAINA, CSTORE, QH, IORES, QVSV, top_cell_no,
      ! SD, TS, ZVSPSL, ZGRUND, QVSH, QOC, GETHRF, QBKB, QBKF, QVSSPR, VSPSI,
      ! VSTHE, FBETA, FDEL, RHOSED, PLS, GINFD, GINFS, GNUBK, QSED, DCBED,
      ! DCBSED, ZERO, ARBDEP, CCCC, SSSS, NCOLMB, CCCCW, QVSWEL, cellarea,
      ! NVSWLI, QVSWLI, WBERR, BALANC, CAREA, NELEE, LLEE

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: RESNOW
      LOGICAL, INTENT(IN)          :: NOW
      CHARACTER(LEN=*), INTENT(IN) :: AIOSTO

      ! Locals
      DOUBLE PRECISION, PARAMETER  :: UNDEF = 999.999D0
      INTEGER :: SFSED1, SFSED2
      DOUBLE PRECISION :: DUM1 (4)
      INTEGER :: ICLASS, ICORS, IDATA, IEL, ISET, IW, J, K, KK, NOUT
      DOUBLE PRECISION :: BUFFER (NELEE), COLBUF (LLEE)
      DOUBLE PRECISION :: DUMO, DUM0

      LOGICAL :: COLUMN
      INTEGER :: SED

      !----------------------------------------------------------------------*

      ! --- LOOP OVER ALL OUTPUT SETS
      !
      !^^^^ sb 4/2/99
      !^^^^ cummulative soil loss data type 44
      IF (.NOT. SEDSRT) THEN
         DO J = 1, total_no_elements
            IF (NOTZERO(DLS(J))) SEDSRT = .TRUE.
            DLSSRT(J) = DLS(J)
         END DO
      END IF

      DO J = 1, total_no_elements
         GNUCUM(J) = GNUCUM(J) + GNU(J) * (RESNOW - PREVTM) * 3600.0D0 * 1000.0D0
      END DO

      output_loop: DO ISET = 1, NSET
         COLUMN = .FALSE.

         ! CHECK IF DATA FOR THIS SET IS TO BE OUTPUT NOW.
         IF (.NOT. NOW) THEN
            IF (RESNOW < IOTIME(ISET) - 1.0D-6) CYCLE output_loop
            IF (IOTIME(ISET) >= IOEND(ISET)) CYCLE output_loop
         END IF

         IDATA = IODATA(ISET)
         IF (IDATA < 1 .OR. IDATA > MIN(LEN(AIOSTO), 50)) CYCLE output_loop
         IF (AIOSTO(IDATA:IDATA) /= '1') CYCLE output_loop

         ! SET UP NUMBER OF DATA ITEMS TO BE WRITTEN
         IF (IOELEM(ISET) > 0) THEN
            NOUT = 1
         ELSE
            ICLASS = -IOELEM(ISET)
            NOUT = ICLNUM(ICLASS)
         END IF
         ICORS = IOCORS(ISET)

         ! Array limits for sediment loops
         SFSED1 = MAX(1, ICORS)
         SFSED2 = MAX(NSED * (1 - ICORS), ICORS)

         ! ASSEMBLE OUTPUT BUFFER
         DO J = 1, NOUT
            IF (IOELEM(ISET) > 0) THEN
               IEL = IOELEM(ISET)
            ELSE
               IEL = ICLIST(J, ICLASS)
            END IF

            SELECT CASE (IODATA(ISET))
            CASE (1)
               BUFFER(J) = PNETTO(IEL) * 3600000.0D0
            CASE (2)
               BUFFER(J) = EPOT(IEL) * 3600000.0D0
            CASE (3)
               BUFFER(J) = ERZA(IEL) * 3600000.0D0
            CASE (4)
               BUFFER(J) = ESOILA(IEL) * 3600000.0D0
            CASE (5)
               BUFFER(J) = EINTA(IEL) * 3600000.0D0
            CASE (6)
               BUFFER(J) = DRAINA(IEL) * 3600000.0D0
            CASE (7)
               BUFFER(J) = CSTORE(IEL)
            CASE (8)
               BUFFER(J) = QH(IEL) * 3600000.0D0
            CASE (9)
               COLUMN = .TRUE.
               WRITE (IORES(ISET)) RESNOW, (QVSV(K, IEL), K = 1, top_cell_no)
            CASE (10)
               BUFFER(J) = SD(IEL)
            CASE (11)
               BUFFER(J) = TS(IEL)
            CASE (12)
               BUFFER(J) = ZVSPSL(IEL) - ZGRUND(IEL)
            CASE (13)
               COLUMN = .TRUE.
               WRITE (IORES(ISET)) RESNOW, (((QVSH(KK, K, IEL)), K = 1, top_cell_no), KK = 1, 4)
            CASE (14)
               COLUMN = .TRUE.
               WRITE (IORES(ISET)) RESNOW, (QOC(IEL, K), K = 1, 4)
            CASE (15)
               BUFFER(J) = GETHRF(IEL) - ZGRUND(IEL)
            CASE (16)
               BUFFER(J) = UNDEF
            CASE (17)
               IF (IEL <= total_no_links) THEN
                  BUFFER(J) = QBKB(IEL, 1) + QBKB(IEL, 2) + QBKF(IEL, 1) + QBKF(IEL, 2)
               ELSE
                  BUFFER(J) = UNDEF
               END IF
            CASE (18)
               BUFFER(J) = QVSSPR(IEL)
            CASE (19)
               COLUMN = .TRUE.
               WRITE (IORES(ISET)) RESNOW, (VSPSI(K, IEL), K = 1, top_cell_no)
            CASE (20)
               COLUMN = .TRUE.
               WRITE (IORES(ISET)) RESNOW, (VSTHE(K, IEL), K = 1, top_cell_no)
            CASE (21)
               DUM0 = DLS(IEL)
               IF (ICORS > 0) DUM0 = DUM0 * FBETA(IEL, ICORS)
               BUFFER(J) = 1.0D3 * DUM0
            CASE (22)
               DUM0 = 0.0D0
               DO SED = SFSED1, SFSED2
                  DUM0 = DUM0 + FDEL(IEL, SED)
               END DO
               BUFFER(J) = 1.0D3 * RHOSED * (1.0D0 - PLS(IEL)) * DUM0
            CASE (23)
               BUFFER(J) = GINFD(IEL, ICORS)
            CASE (24)
               BUFFER(J) = GINFS(IEL, ICORS)
            CASE (25)
               BUFFER(J) = 1000.0D0 * 24.0D0 * 3600.0D0 * GNU(IEL)
            CASE (26)
               BUFFER(J) = 1000.0D0 * 24.0D0 * 3600.0D0 * GNUBK(IEL)
            CASE (27)
               COLUMN = .TRUE.
               DO K = 1, 4
                  DUM0 = 0.0D0
                  DO SED = SFSED1, SFSED2
                     DUM0 = DUM0 + QSED(IEL, SED, K)
                  END DO
                  DUM1(K) = DUM0 * RHOSED
               END DO
               WRITE (IORES(ISET)) RESNOW, DUM1
            CASE (28)
               DUM0 = 0.0D0
               DO SED = SFSED1, SFSED2
                  DUM0 = DUM0 + QSED(IEL, SED, 1) + QSED(IEL, SED, 2) + &
                                QSED(IEL, SED, 3) + QSED(IEL, SED, 4)
               END DO
               BUFFER(J) = DUM0 * RHOSED
            CASE (29)
               IF (DCBED(IEL) > 0.0D0) THEN
                  BUFFER(J) = DCBSED(IEL, ICORS) / DCBED(IEL)
               ELSE
                  BUFFER(J) = ZERO
               END IF
            CASE (30)
               COLUMN = .TRUE.
               DO K = 1, 4
                  DUM0 = 0.0D0
                  DO SED = SFSED1, SFSED2
                     IF (QOC(IEL, K) > ZERO) THEN
                        DUM0 = DUM0 + QSED(IEL, SED, K) / QOC(IEL, K)
                     ELSE
                        DUMO = ZERO
                     END IF
                  END DO
                  DUM1(K) = 1.0D3 * DUM0 * RHOSED
               END DO
               WRITE (IORES(ISET)) RESNOW, (DUM1(K), K = 1, 4)
            CASE (31)
               BUFFER(J) = ARBDEP(IEL)
            CASE (32)
               COLUMN = .TRUE.
               WRITE (IORES(ISET)) RESNOW, (CCCC(IEL, K, ICORS), K = 1, top_cell_no)
            CASE (33)
               COLUMN = .TRUE.
               WRITE (IORES(ISET)) RESNOW, (SSSS(IEL, K, ICORS), K = 1, top_cell_no)
            CASE (34)
               BUFFER(J) = CCCC(IEL, top_cell_no, ICORS)
            CASE (35)
               BUFFER(J) = CCCC(IEL, top_cell_no - 1, ICORS)
            CASE (36)
               BUFFER(J) = CCCC(IEL, top_cell_no - 2, ICORS)
            CASE (37)
               BUFFER(J) = CCCC(IEL, NCOLMB(IEL), ICORS)
            CASE (38)
               BUFFER(J) = CCCCW(IEL, ICORS)
            CASE (39:40)
               BUFFER(J) = UNDEF
            CASE (41)
               BUFFER(J) = QVSWEL(IEL) * cellarea(IEL)
            CASE (42)
               COLUMN = .TRUE.
               IW = NVSWLI(IEL)
               IF (IW > 0) THEN
                  DO K = 1, top_cell_no
                     COLBUF(K) = QVSWLI(K, IW) * cellarea(IEL)
                  END DO
               ELSE
                  DO K = 1, top_cell_no
                     COLBUF(K) = 0.0D0
                  END DO
               END IF
               WRITE (IORES(ISET)) RESNOW, (COLBUF(K), K = 1, top_cell_no)
            CASE (43)
               BUFFER(J) = WBERR(IEL)
            CASE (44)
               BUFFER(J) = GNUCUM(IEL) - (DLS(IEL) - DLSSRT(IEL)) * 1000.0D0
            CASE (45:49)
               BUFFER(J) = UNDEF
            CASE (50)
               BUFFER(J) = BALANC(J) * 1000.0D0 / CAREA
            END SELECT

         END DO

         IF (.NOT. COLUMN) WRITE (IORES(ISET)) RESNOW, (BUFFER(J), J = 1, NOUT)

         IOTIME(ISET) = RESNOW + IOSTEP(ISET)

      END DO output_loop

      PREVTM = RESNOW
   END SUBROUTINE FRRESP



   ! 14/3/95
   !
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   !SSSSSS SUBROUTINE FRSORT
   SUBROUTINE FRSORT
   !
   ! SORT OF ALL ELEMENTS ON WATER ELEVATION (HIGHEST ELEVATION FIRST)
   ! OR WATER TABLE ELEVATION IF NO SURFACE WATER IS PRESENT IN A GRID SQUARE
   !   BANK ELEMENT
   ! OR CHANNEL BED ELEVATION IF NO SURFACE WATER IS PRESENT IN A CHANNEL LINK
   !
   ! SURFACE WATER ELEVATIONS AND INDICES STORED IN COLUMN 1 OF ELEV AND ISTEMP
   ! WATER TABLE ELEVATIONS AND INDICES STORED IN COLUMN 2 OF ELEV AND ISTEMP
   !
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   !

      ! Assumed external module dependencies providing global variables:
      ! NELEE, total_no_elements, ISORT, ICMREF, LINKNS, ZVSPSL, GETHRF,
      ! ZGRUND, zero

      IMPLICIT NONE

      ! Locals, etc
      DOUBLE PRECISION :: ELEV (NELEE, 2)
      INTEGER :: ISTEMP (NELEE, 2), NSORT (2)
      INTEGER :: NS1, NS2, I, IEL, ITYPE, JEL, IL, L, NDUM, NSTART, NEND, &
                 JUMP, M, K, N, ITEMP, I1, I2, IS
      DOUBLE PRECISION :: HSZ1, HSZ2, ZHIGH, ZLOW, TEMP

      !----------------------------------------------------------------------*

      IF (total_no_elements == 1) RETURN

      NS1 = 0
      NS2 = 0

      ! PUT ELEVATIONS INTO LOCAL ARRAYS, DIVIDED INTO SURFACE AND WATER TABLE
      !   ELEMENTS (NB. 'GHOST' PHREATIC SURFACE LEVELS ARE SET UP FOR THE CHANNELS
      !   EQUAL TO THE MAX. PHREATIC ELEVATION OF THE NEIGHBOURING ELEMENTS)
      !
      DO I = 1, total_no_elements

         IEL = ISORT (I)
         ITYPE = ICMREF (IEL, 1)

         IF (ITYPE == 3) THEN
            HSZ1 = zero
            HSZ2 = zero
            IF (LINKNS (IEL)) THEN
               JEL = ICMREF (IEL, 5)
               IF (JEL > 0) HSZ1 = ZVSPSL (JEL)
               JEL = ICMREF (IEL, 7)
               IF (JEL > 0) HSZ2 = ZVSPSL (JEL)
            ELSE
               JEL = ICMREF (IEL, 6)
               IF (JEL > 0) HSZ1 = ZVSPSL (JEL)
               JEL = ICMREF (IEL, 8)
               IF (JEL > 0) HSZ2 = ZVSPSL (JEL)
            END IF
            ZVSPSL (IEL) = MAX (HSZ1, HSZ2)
         END IF

         IL = ICMREF (IEL, 4)
         IF (GETHRF (IEL) - ZGRUND (IEL) > 1.0E-8) THEN
            NS1 = NS1 + 1
            ELEV (NS1, 1) = GETHRF (IEL)
            ISTEMP (NS1, 1) = IEL
         ELSE
            NS2 = NS2 + 1
            ELEV (NS2, 2) = ZVSPSL (IEL)
            ISTEMP (NS2, 2) = IEL
         END IF

      END DO

      NSORT (1) = NS1
      NSORT (2) = NS2

      ! --- SORT ON WATER SURFACE ELEVATIONS, THEN WATER TABLE ELEVATIONS
      !
      column_loop: DO L = 1, 2
         NDUM = NSORT (L)

         ! - CHECK FOR START AND END OF ARRAY TO BE SORTED
         !
         ! PASS ONE (HIGHEST TO LOWEST)
         ! - FIND FIRST POINT (IF ANY) WHERE ELEVATIONS START INCREASING
         NSTART = 0
         DO I = 1, NDUM - 1
            IF (ELEV (I + 1, L) > ELEV (I, L)) THEN
               NSTART = I
               EXIT
            END IF
         END DO

         ! - IF NO INCREASING ELEVATIONS FOUND, THE ARRAY IS ALREADY SORTED
         IF (NSTART == 0) CYCLE column_loop

         ! - FIND HIGHEST POINT IN REST OF ARRAY
         ZHIGH = zero
         DO I = NSTART + 1, NSORT (L)
            IF (ELEV (I, L) > ZHIGH) ZHIGH = ELEV (I, L)
         END DO

         ! - FIND POSITION IN SORTED SECTION OF ARRAY OF ELEVATION 'HIGH'
         DO I = 1, NSTART
            IF (ELEV (I, L) < ZHIGH) THEN
               NSTART = I
               EXIT
            END IF
         END DO

         ! PASS TWO (LOWEST TO HIGHEST)
         ! - FIND FIRST POINT (IF ANY) WHERE ELEVATIONS START DECREASING
         NEND = 0
         DO I = NDUM, 2, -1
            IF (ELEV (I - 1, L) < ELEV (I, L)) THEN
               NEND = I
               EXIT
            END IF
         END DO

         ! - IF NO DECREASING ELEVATIONS FOUND, THE ARRAY IS ALREADY SORTED
         IF (NEND == 0) CYCLE column_loop

         ! - FIND LOWEST POINT IN REST OF ARRAY
         ZLOW = 1.0E10
         DO I = NEND - 1, 1, -1
            IF (ELEV (I, L) < ZLOW) ZLOW = ELEV (I, L)
         END DO

         ! - FIND POSITION IN SORTED SECTION OF ARRAY OF ELEVATION 'ZLOW'
         DO I = NDUM, NEND, -1
            IF (ELEV (I, L) > ZLOW) THEN
               NEND = I
               EXIT
            END IF
         END DO

         ! --- SORT ON ARRAY BETWEEN NSTART AND NEND (Shell Sort)
         JUMP = NEND - NSTART + 1

         gap_loop: DO
            JUMP = JUMP / 2
            IF (JUMP == 0) EXIT gap_loop

            DO M = NSTART, NEND - JUMP
               K = M

               inner_sort_loop: DO
                  N = K + JUMP
                  IF (ELEV (K, L) < ELEV (N, L)) THEN
                     ! Swap indices
                     ITEMP = ISTEMP (K, L)
                     ISTEMP (K, L) = ISTEMP (N, L)
                     ISTEMP (N, L) = ITEMP

                     ! Swap elevations
                     TEMP = ELEV (K, L)
                     ELEV (K, L) = ELEV (N, L)
                     ELEV (N, L) = TEMP

                     K = K - JUMP
                     IF (K > 0) CYCLE inner_sort_loop
                  END IF
                  EXIT inner_sort_loop
               END DO inner_sort_loop

            END DO
         END DO gap_loop

         ! --- ARRAY ISTEMP IS SORTED
      END DO column_loop

      ! --- REASSEMBLE ISORT ARRAY
      !
      I1 = 1
      I2 = 1
      IS = 1

      reassemble_loop: DO
         IF (NS1 > 0) THEN
            IF (NS2 == 0 .OR. ZVSPSL (ISTEMP (I1, 1)) > ELEV (I2, 2)) THEN
               ISORT (IS) = ISTEMP (I1, 1)
               I1 = I1 + 1
               IS = IS + 1
            ELSE
               ISORT (IS) = ISTEMP (I2, 2)
               I2 = I2 + 1
               IS = IS + 1
            END IF
         END IF

         IF (I1 > NS1) THEN
            DO I = IS, total_no_elements
               ISORT (I) = ISTEMP (I2, 2)
               I2 = I2 + 1
            END DO
            EXIT reassemble_loop
         END IF

         IF (I2 > NS2) THEN
            DO I = IS, total_no_elements
               ISORT (I) = ISTEMP (I1, 1)
               I1 = I1 + 1
            END DO
            EXIT reassemble_loop
         END IF
      END DO reassemble_loop

      RETURN

      ! FORMAT STATEMENTS
1000  FORMAT(' total_no_elements= ', I4, '  NS1= ', I4, ' NS2= ', I4, ' SFCMAX(*)= ', F7.1, &
             ' sfcmin=', f7.1, ' SZMAX(+)= ', F7.1, ' szmin=', f7.1)
1010  FORMAT(' ', I4, ' ', I4, ' |', A68)

   END SUBROUTINE FRSORT




   !SSSSSS SUBROUTINE INBK
   SUBROUTINE INBK
   !----------------------------------------------------------------------*
   !
   ! SUBROUTINE TO READ IN INPUT DATA FOR BANK COMPONENT
   !
   ! INPUT METHODS ARE:
   !  1  SET VALUE = ADJACENT GRID VALUE
   !                 (OR ADJACENT BANK-FULL ELEV. FOR G. LEVEL)
   !  2  SET VALUE = GIVEN DEFAULT VALUE
   !  3  VALUE GIVEN FOR EACH DATA CLASS (SEE OUTPUT DEFINITION FILE)
   !  4  VALUE GIVEN FOR EACH BANK ELEMENT
   !
   ! Note that bank widths are not set here.
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/BK/INBK/4.2
   ! Modifications:
   ! RAH  941001 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL (AL.P).
   !  GP  940816  4.0  Don't set NLYRC,I/CATUZ (see AL.D,FRRESC), N/ICTUZR
   !                   (see AL.D), NLYR,NTSOIL,ZLYRBT (see VSREAD,VSCONC).
   !                   Replace HSZ with ZVSPSL (see AL.C).
   ! RAH  980713  4.2  Explicit typing.
   !      980730       Don't support INTYPE=3 (was incorrect - see BR/__).
   !                   New local DZG.
   ! JE   JAN 2009     Loop restructure for AD
   !----------------------------------------------------------------------*
   ! Commons and constants

      ! Assumed external module dependencies providing global variables:
      ! BKD, TITLE, PPPRI, NGDBGN, total_no_elements, IDUM, DUMMY, zero,
      ! ICMREF, ZGRUND, ZBFULL, NMC, NRAINC, NVC, STRXX, STRYY, SD,
      ! RHOSAR, ZVSPSL, GETHRF, SETHRF, total_no_links, FFFATAL, ERROR

      IMPLICIT NONE

      ! Locals, etc
      INTEGER :: I, IEL, ICOUNT, IDATA, IFAULT, IL, INTYPE, ITYPE
      INTEGER :: J, JEL, NVALUE
      INTEGER :: IVALUE (NLFEE * 2), IELEM (NLFEE * 2)
      DOUBLE PRECISION :: DFAULT, DZG, VALUE (NLFEE * 2)
      LOGICAL :: BINBKD, found_adjacent

      ! Modern array constructor replacing the legacy DATA statement
      LOGICAL, PARAMETER :: INTEGR(13) = [.FALSE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., &
         .FALSE., .TRUE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE.]

      !----------------------------------------------------------------------*
      !
      ! READ TITLE, FLAG FOR PRINTING INITIALISATION DATA
      ! :BK1
      READ (BKD, '(A)') TITLE
      READ (BKD, '(L7)') BINBKD

      ! ----- LOOP OVER INPUT DATA TYPES
      !
      out500: DO IDATA = 1, 13
         ! INITIALISE DUMMY ARRAYS
         DO IEL = NGDBGN, total_no_elements
            IDUM (IEL) = 0
            DUMMY (IEL) = zero
         END DO

         ! READ TITLE, INPUT METHOD, NUMBER OF FOLLOWING VALUES
         ! :BK3
         READ (BKD, '(A)') TITLE
         IF (BINBKD) WRITE(PPPRI, '(A)') TITLE
         READ (BKD, '(10I7)') INTYPE, NVALUE

         !
         ! TYPE 1: SET VALUE = VALUE AT ADJACENT GRID
         ! ++++++++++++++++++++++++++++++++++++++++++
         !
         ! (except ZGRUND     = ZBFULL(il)
         !     and ZVSPSL,HRF = value + ZGRUND - ZGRUND(jel) )
         !
         ! NB. CATCHMENT IS SCANNED TWICE. THE 2nd TIME THROUGH, ANY BANKS WITH
         !     NO ADJACENT GRID ARE GIVEN THE VALUE OF THE 1st ADJACENT BANK FOUND
         !
         IF (INTYPE == 1) THEN
            out95: DO ICOUNT = 1, 2
               out90: DO IEL = NGDBGN, total_no_elements
                  ITYPE = ICMREF (IEL, 1)
                  IF (ITYPE /= 1 .AND. ITYPE /= 2) CYCLE out90

                  ! * find adjacent element
                  found_adjacent = .FALSE.

                  out60: DO J = 1, 4
                     JEL = ICMREF (IEL, 4 + J)
                     IF (JEL > 0) THEN
                        IF (ICMREF (JEL, 1) == 0) THEN
                           found_adjacent = .TRUE.
                           EXIT out60
                        END IF
                     END IF
                  END DO out60

                  IF (.NOT. found_adjacent) THEN
                     out65: DO J = 1, 4
                        JEL = ICMREF (IEL, J + 4)
                        IF (JEL > 0) THEN
                           IF (ICMREF (JEL, 1) == 1 .OR. ICMREF (JEL, 1) == 2) THEN
                              found_adjacent = .TRUE.
                              EXIT out65
                           END IF
                        END IF
                     END DO out65
                  END IF

                  ! * set value
                  DZG = ZGRUND (IEL) - ZGRUND (JEL)

                  SELECT CASE (IDATA)
                  CASE (1)
                     IL = ICMREF (IEL, 4)
                     ZGRUND (IEL) = ZBFULL (IL)
                  CASE (2)
                     NMC (IEL) = NMC (JEL)
                  CASE (3)
                     NRAINC (IEL) = NRAINC (JEL)
                  CASE (4)
                     NVC (IEL) = NVC (JEL)
                  CASE (6)
                     STRXX (IEL) = STRXX (JEL)
                  CASE (7)
                     STRYY (IEL) = STRYY (JEL)
                  CASE (10)
                     SD (IEL) = SD (JEL)
                  CASE (11)
                     RHOSAR (IEL) = RHOSAR (JEL)
                  CASE (12)
                     ZVSPSL (IEL) = ZVSPSL (JEL) + DZG
                  CASE (13)
                     CALL SETHRF(IEL, GETHRF (JEL) + DZG)
                  END SELECT
               END DO out90
            END DO out95

            CYCLE out500
            !
            ! TYPE 2: READ SINGLE DEFAULT VALUE
            ! +++++++++++++++++++++++++++++++++
            !
         ELSE IF (INTYPE == 2) THEN
            ! :BK5
            IF (INTEGR (IDATA)) THEN
               READ (BKD, '(10I7)') IFAULT
               IF (BINBKD) WRITE(PPPRI, 1300) IFAULT

               DO IEL = NGDBGN, total_no_elements
                  ITYPE = ICMREF (IEL, 1)
                  IF (ITYPE == 1 .OR. ITYPE == 2) IDUM (IEL) = IFAULT
               END DO
               ! :BK6
            ELSE
               READ (BKD, '(10F7.0)') DFAULT
               IF (BINBKD) WRITE(PPPRI, 1500) DFAULT

               DO IEL = NGDBGN, total_no_elements
                  ITYPE = ICMREF (IEL, 1)
                  ! amended by GP 18/7/94 to be consistent with DSATE code
                  IF (ITYPE == 1 .OR. ITYPE == 2) THEN
                     IF (IDATA == 1) THEN
                        IL = ICMREF (IEL, 4)
                        DUMMY (IEL) = ZBFULL (IL) + DFAULT
                     ELSE
                        DUMMY (IEL) = DFAULT
                     END IF
                  END IF
               END DO
            END IF

            ! TYPE 3: READ PAIRS OF (DATA CLASS, VALUE)
            ! +++++++++++++++++++++++++++++++++++++++++
         ELSE IF (INTYPE == 3) THEN
            ! :BK7-8
            CALL ERROR(FFFATAL, 1061, PPPRI, 0, 0, 'BKD input type 3 (data class, value) not supported')

            ! TYPE 4: READ PAIRS OF (BANK ELEMENT NUMBER, VALUE)
            ! ++++++++++++++++++++++++++++++++++++++++++++++++++
         ELSE IF (INTYPE == 4) THEN
            NVALUE = 2 * total_no_links
            ! 980713
            IF (INTEGR (IDATA)) THEN
               READ (BKD, '(10I7)') (IELEM (I), IVALUE (I), I = 1, NVALUE)
               IF (BINBKD) WRITE(PPPRI, 2000)
               IF (BINBKD) WRITE(PPPRI, 2050) (IELEM (I), IVALUE (I), I = 1, NVALUE)

               DO I = 1, NVALUE
                  IEL = IELEM (I)
                  ITYPE = ICMREF (IEL, 1)
                  IF (ITYPE == 1 .OR. ITYPE == 2) IDUM (IEL) = IVALUE (I)
               END DO
            ELSE
               READ (BKD, '(5(I7,F7.0))') (IELEM (I), VALUE (I), I = 1, NVALUE)
               IF (BINBKD) WRITE(PPPRI, 2100)
               IF (BINBKD) WRITE(PPPRI, 2150) (IELEM (I), VALUE (I), I = 1, NVALUE)

               DO I = 1, NVALUE
                  IEL = IELEM (I)
                  ITYPE = ICMREF (IEL, 1)
                  IF (ITYPE == 1 .OR. ITYPE == 2) DUMMY (IEL) = VALUE (I)
               END DO
            END IF
         END IF

         ! MOVE DATA FROM DUMMY ARRAYS INTO ACTUAL DATA ARRAYS
         DO IEL = NGDBGN, total_no_elements
            ITYPE = ICMREF (IEL, 1)
            IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               SELECT CASE (IDATA)
               CASE (1)
                  ZGRUND (IEL) = DUMMY (IEL)
               CASE (2)
                  NMC (IEL) = IDUM (IEL)
               CASE (3)
                  NRAINC (IEL) = IDUM (IEL)
               CASE (4)
                  NVC (IEL) = IDUM (IEL)
               CASE (6)
                  STRXX (IEL) = DUMMY (IEL)
               CASE (7)
                  STRYY(IEL) = DUMMY (IEL)
               CASE (10)
                  SD (IEL) = DUMMY (IEL)
               CASE (11)
                  RHOSAR (IEL) = DUMMY (IEL)
               CASE (12)
                  ZVSPSL (IEL) = ZGRUND (IEL) - DUMMY (IEL)
               CASE (13)
                  CALL SETHRF(IEL, ZGRUND (IEL) + DUMMY (IEL))
               END SELECT
            END IF
         END DO

      END DO out500

      ! FORMAT STATEMENTS
      !
1300  FORMAT(' DEFAULT VALUE ', I7, ' USED IN ALL BANK ELEMENTS'/)
1500  FORMAT(' DEFAULT VALUE ', F12.3, ' USED IN ALL BANK ELEMENTS'/)
2000  FORMAT(' VALUES ALLOCATED TO EACH ELEMENT:'/ 3('       ELEMENT   VALUE'))
2050  FORMAT(3(I7, 2X, I7, 6X))
2100  FORMAT(' VALUES ALLOCATED TO EACH ELEMENT:'/ 3('       ELEMENT     VALUE'))
2150  FORMAT(3(I7, F12.3, 6X))

   END SUBROUTINE INBK



   !SSSSSS SUBROUTINE INCM
   SUBROUTINE INCM (ISSDON)
   !----------------------------------------------------------------------*
   !
   !  INITIALISATION SUBROUTINE FOR CONTAMINANT COMPONENT
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/MUZ/INCM/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.  No INTEGER*2.
   !  GP  960124  4.0  Scrap local variables K1,K2 & arrays JAQBT,JFACE,
   !                   JSOOL,JSOOLA,NWORK,DOL,TDUMMY,ZDEL,ZHATP; add ROH.
   !                   Replace: THSAT with VSPOR; KSPPE,KSPE with KSPDUM.
   !                   Set ZCOLMB from ZVSNOD (was ZGRUND & DDZ).
   !                   ...
   ! RAH  970108  4.1  No long lines or non-std chars.  Generic intrinsics.
   !      970218       Swap subscripts: QVSH,DELTAZ,JVSACN,JVSDEL,ZVSNOD,
   !                   QVSV,VSTHE (see AL.C).
   !      970521       Scrap outputs PLS,PSD (SED.CS), JBTLYR (COLM.CG) &
   !                   WELDRO (COLM.CO).  Explicit typing.
   !                   Don't admit BEXBK=F (setting JEL in loop 24).
   ! RAH  980308  4.2  Scrap output OLBD (BK.CW).
   !      981103       Scrap output ERUZO (COLM.CO).
   !----------------------------------------------------------------------*
   ! Commons and constants

      USE CMmod, ONLY: CMRD   !"JE"

      ! Assumed external module dependencies providing global variables:
      ! CMD, CMP, NCONEE, NELEE, total_no_elements, total_no_links, NLFEE, NSEE, NS, NSEDEE,
      ! NSED, NOCTAB, NX, NXEE, NYEE, NY, NLYRBT, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS,
      ! NCOLMB, DBS, DBDI, CCAPI, CCAPE, CCAPR, CCAPB, IIICF, SOFN, GNN, GGLMSO, ALPHBD,
      ! ALPHBS, KDDLS, ALPHA, FADS, MUERR2, ARBDEP, zero, DLS, DLSO, FBETA, FBTSD, FDEL,
      ! GINFD, GINFS, GNUBK, QDEFF, NHBED, NTSOIL, NSOBED, PBSED, VSPOR, GNU, GNUO,
      ! SOSDFN, one, Z2, D0, Z2SQ, Z2OD, Z2SQOD, SGMA, SGSQ, OMSGMA, top_cell_no, NCON,
      ! GCPLA, KDDSOL, ZCOLMB, ZVSNOD, KSPDUM, DELTAZ, JVSACN, JVSDEL, NOLCE, NOLCEA,
      ! NOLBT, JOLFN, NOL, FHBED, JFCE, NCEBD, FNCEBD, JOLDUM, FOLDUM, JKZCOL, cellarea,
      ! CLENTH, CWIDTH, half, ACPBSG, ACPBI, NCEAB, CCAPIO, IIICFO, ACPSFO, ARXL, ACPBDO,
      ! CCCCO, CCAPIN, CCCC, VSTHE, THBEDO, THBED, FBBEDO, FDELO, FBTSDO, DSWO, GETHRF,
      ! ZGRUND, QIO, PNETTO, QQRFO, QVSV, RSZWLO, ZONEO, QQQSWO, QOC, GGAMMO, QQO, QVSH,
      ! SSSSO, SSSS, VSTHEO, UUAJPO, ISCNSV, ALINTP, ISPLT, INPL

      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: ISSDON

      ! Locals, etc
      INTEGER :: ICL, IDEL, IEL, IFA, ITYPE, ITYPEA
      INTEGER :: JA, JAL, JBK, JBKU, JCL, JDEL, JDUM, JEL, JFA, JFLINK
      INTEGER :: JLYR, JSED, JSOIL, LDUM
      INTEGER :: NBKU, NCDUM, NCE, NCE1, NCE2, NCEA, NCL, NCONT
      INTEGER :: NDIFF, NDUM, NDUMA, NELMA, NLINK, NLINKA, NLINKU
      INTEGER :: NOL1, NOL2, NOLBD, NOLDUM, NOLP, NOLX
      INTEGER :: JFCE (2), JOLDUM (2), NBK (2), NCEDUM (2)
      DOUBLE PRECISION :: ARL, ARP, DBK, DKBED, DMULT, DUM, DUM1, DUM2, DUM3, DUMK
      DOUBLE PRECISION :: FNOLBD, asum, asumK

      DOUBLE PRECISION :: FNDUM (2), FOLDUM (2), ROH (LLEE)
      DOUBLE PRECISION, ALLOCATABLE :: KSPDUM(:, :)

      ! Added by SB
      INTEGER :: MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS
      INTEGER :: NUM_CATEGORIES_TYPES (NCONEE), NTAB (NOCTAB, NCONEE)
      INTEGER, ALLOCATABLE :: NCATTY(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: TABLE_CONCENTRATION(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: TABLE_WATER_DEPTH(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: DUMMYCONC(:, :)

      LOGICAL :: LDUM1(1), ISCNSV (NCONEE)

      !----------------------------------------------------------------------*
      ! New by SB 18/11/04
      ! contam.f removed. z2 and d0 (scaling variables) needed here
      ! -----------------------------------------------------------------
      Z2 = 50.0D0
      D0 = 1.0D-3

      !----------------------------------------------------------------------*
      ! New by SB
      ! Parameter values for spatially variable initial contaminant conc.
      ! -----------------------------------------------------------------
      !
      MAX_NUM_CATEGORY_TYPES = NOCTAB
      MAX_NUM_DATA_PAIRS = NOCTAB

      ALLOCATE(KSPDUM(total_no_elements, top_cell_no + 1), DUMMYCONC(total_no_elements, top_cell_no))
      ALLOCATE(NCATTY(NELEE, NCONEE))
      ALLOCATE(TABLE_CONCENTRATION(NOCTAB, NOCTAB, NCONEE), TABLE_WATER_DEPTH(NOCTAB, NOCTAB, NCONEE))

      ! Read main CM input data file
      ! ----------------------------
      ! Modified by SB

      CALL CMRD (CMD, CMP, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, total_no_elements, total_no_links, NLFEE, NSEE, &
         NS, NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY, NLYRBT (total_no_links + 1, 1), &
         ICMXY, ICMBK, ICMREF (1, 5), BEXBK, LINKNS, NUM_CATEGORIES_TYPES, NCATTY, NCON, &
         NCOLMB (total_no_links + 1), NTAB, DBS, DBDI, CCAPI, CCAPE, CCAPR, CCAPB, &
         TABLE_CONCENTRATION, TABLE_WATER_DEPTH, IIICF, SOFN, GNN, GGLMSO, ALPHBD, ALPHBS, KDDLS, &
         ALPHA, FADS, ISCNSV, IDUM, DUMMY)
         ! Checks the data used to calculate spatially variable
         ! concentrations in the grid and bank elements is OK

      CALL MUERR2 (CMP, total_no_elements, NELEE, total_no_links, MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCON, NCONEE, &
         NUM_CATEGORIES_TYPES, NTAB, NCATTY, ISCNSV, TABLE_CONCENTRATION, TABLE_WATER_DEPTH, LDUM1)

      !----------------------------------------------------------------------*
      DO NCL = total_no_links + 1, total_no_elements
         NCOLMB (NCL) = NLYRBT (NCL, 1)
      END DO

      IF (.NOT. ISSDON) THEN
         ! ssssss INITIALISE SEDIMENT VARIABLES sssss
         NSED = 3
         DO NLINK = 1, total_no_links
            ARBDEP (NLINK) = zero
            DLS (NLINK) = zero
            DLSO (NLINK) = zero

            FBETA (NLINK, 1:3) = [one, zero, zero]
            FBTSD (NLINK, 1:3) = [one, zero, zero]
            FDEL (NLINK, 1:3)  = [zero, zero, zero]
            GINFD (NLINK, 1:3) = [zero, zero, zero]
            GINFS (NLINK, 1:3) = [zero, zero, zero]

            GNUBK (NLINK) = zero
            QDEFF (NLINK, 1:2) = zero

            DO JA = 1, 4
               NELMA = ICMREF (NLINK, JA + 4)
               IF (NELMA > 0) THEN
                  ITYPEA = ICMREF (NELMA, 1)
                  IF (ITYPEA == 1) THEN
                     NBK (1) = NELMA
                  ELSE IF (ITYPEA == 2) THEN
                     NBK (2) = NELMA
                  END IF
               END IF
            END DO

            JLYR = 0
            search_lyr_loop: DO
               JLYR = JLYR + 1
               IF (NLYRBT (NBK (1), JLYR) >= NHBED (NLINK, 1)) EXIT search_lyr_loop
            END DO search_lyr_loop

            NSOBED (NLINK) = NTSOIL (NBK (1), JLYR - 1)
            PBSED (NLINK) = VSPOR (NSOBED (NLINK))
            ! SET BED SOIL TYPE AND POROSITY, BASED ON THE SOIL AT THE
            ! BOTTOM OF THE EXPOSED FACE OF BANK 1
         END DO

         DO NCL = total_no_links + 1, total_no_elements
            DLS (NCL) = zero
            DLSO (NCL) = zero
            FDEL (NCL, 1:3) = [zero, zero, zero]
            FBETA (NCL, 1:3) = [one, zero, zero]
            GNU (NCL) = zero
            GNUO (NCL) = zero
         END DO

         DO JSOIL = 1, NSEE
            SOSDFN (JSOIL, 1:3) = SOFN (JSOIL, 1:3)
         END DO
         ! SET SEDIMENT FRACTIONS FOR SOIL TYPES

      END IF
      ! IF THE SEDIMENT CODE IS NOT ACTIVE, THE SEDIMENT VARIABLES ARE SET TO APPROPRIATE VALUES
      ! ccccccccccccc SET CONSTANTS cccccccccccccc

      SCL = one / 32500.0D0
      OODO = one / D0

      ! SCALING FACTORS
      Z2SQ = Z2 * Z2
      Z2OD = OODO * Z2
      Z2SQOD = OODO * Z2SQ

      ! SCALING VALUES
      SGMA = one
      SGSQ = SGMA * SGMA
      OMSGMA = one - SGMA

      ! FINITE DIFFERENCE IMPLICIT WEIGHTING
      NCETOP = top_cell_no

      DO NCONT = 1, NCON
         ! SET CONSTANTS WHICH DEPEND ON CONTAMINANT NUMBER
         GCPLA (NCONT) = GGLMSO (NCONT) * Z2SQOD
         ! SET DECAY CONSTANTS FOR CONTAMINANTS

         DO JSOIL = 1, NS
            asum = SUM(SOSDFN (JSOIL, 1:NSED) * KDDLS (1:NSED, NCONT))
            KDDSOL (JSOIL, NCONT) = asum
         END DO
         ! SET REFERENCE DISTRIBUTION COEFFICIENT FOR SOIL TO MATCH THAT SPECIFIED FOR THE
         ! SEDIMENT PARTICLE SIZE GROUPS
      END DO

      DO NCL = total_no_links + 1, total_no_elements
         ZCOLMB (NCL) = ZVSNOD (NCOLMB (NCL), NCL)
      END DO
      ! SET ELEVATION OF BOTTOM CELLS IN SOIL COLUMNS

      ! set up temporary array for use until full vss coding completed
      DO NCL = 1, total_no_elements
         DO NCE = NLYRBT (NCL, 1), top_cell_no
            KSPDUM (NCL, NCE) = DELTAZ (NCE, NCL) / Z2
         END DO
         KSPDUM (NCL, top_cell_no + 1) = KSPDUM (NCL, top_cell_no)
      END DO

      !---------------------------------------------------------------
      ! Set up NOL, NOLBT, NOLCE, NOLCEA, JOLFN using VSS arrays JVSACN,
      ! JVSDEL and DELTAZ
      ! NB. NOLBT and JOLFN are overwritten during the loop over a column

      DO IEL = total_no_links + 1, total_no_elements
         DO IFA = 1, 4
            JEL = ICMREF (IEL, IFA + 4)
            JFA = ICMREF (IEL, IFA + 8)
            IF (JEL == 0) THEN
               JEL = IEL
               JFA = IFA
            ELSE IF (ICMREF (JEL, 1) == 3) THEN
               JEL = ICMREF (JEL, IFA + 4)
            END IF

            NOLP = 0
            DO ICL = NLYRBT (IEL, 1), top_cell_no
               IF (JVSACN (IFA, ICL, IEL) > 0) THEN
                  JCL = JVSACN (IFA, ICL, IEL)
                  IDEL = JVSDEL (IFA, ICL, IEL)
                  JDEL = JVSDEL (JFA, JCL, JEL)

                  NOLP = NOLP + 1
                  NOLCE (IEL, NOLP, IFA) = ICL
                  NOLCEA (IEL, NOLP, IFA) = JCL
                  NOLBT (IEL, ICL, IFA) = NOLP

                  IF (IDEL == 1) THEN
                     JOLFN (IEL, NOLP, IFA) = INT (32500.0D0 * DELTAZ (ICL, IEL) / (DELTAZ (ICL, IEL) + DELTAZ (ICL + 1, IEL)))
                     NOLP = NOLP + 1
                     NOLCE (IEL, NOLP, IFA) = ICL + 1
                     NOLCEA (IEL, NOLP, IFA) = JCL
                     JOLFN (IEL, NOLP, IFA) = INT (32500.0D0 * DELTAZ (ICL + 1, IEL) / (DELTAZ (ICL, IEL) + DELTAZ (ICL + 1, IEL)))
                  ELSE IF (JDEL == 1) THEN
                     NOLP = NOLP + 1
                     NOLCE (IEL, NOLP, IFA) = ICL
                     NOLCEA (IEL, NOLP, IFA) = JCL + 1
                  ELSE
                     JOLFN (IEL, NOLP, IFA) = 32500
                  END IF
               END IF
            END DO

            NOL (IEL, IFA) = NOLP
            NOLBT (IEL, top_cell_no + 1, IFA) = NOLP + 1
         END DO
      END DO

      DKBED = DBDI / Z2
      DO NLINK = 1, total_no_links
         ! ^^^^^^^^^ SET CONSTANTS FOR LINKS ^^^^^^^^
         DO JA = 1, 4
            NDUMA = ICMREF (NLINK, JA + 4)
            IF (NDUMA > 0) THEN
               ITYPEA = ICMREF (NDUMA, 1)
               IF (ITYPEA == 1 .OR. ITYPEA == 2) THEN
                  ! ADJACENT ELEMENT IS A BANK
                  JBK = ITYPEA
                  NBK (JBK) = NDUMA
                  ! USED ONLY IN THIS ROUTINE
                  NBANK (NLINK, JBK) = NDUMA
                  ! SAVED FOR USE IN OTHER SUBROUTINES

                  asum = FHBED (NLINK, JBK) * KSPDUM (NBK (JBK), NHBED (NLINK, JBK) + 1)
                  IF (asum >= DKBED) THEN
                     NCEDUM (JBK) = NHBED (NLINK, JBK)
                     FNDUM (JBK) = (asum - DKBED) / KSPDUM (NBK (JBK), NHBED (NLINK, JBK) + 1)
                  ELSE
                     NCE = NHBED (NLINK, JBK)

                     bed_depth_loop: DO
                        NCE = NCE - 1
                        asum = asum + KSPDUM (NBK (JBK), NCE + 1)
                        IF (asum > DKBED) EXIT bed_depth_loop
                     END DO bed_depth_loop

                     NCEDUM (JBK) = NCE
                     FNDUM (JBK) = (asum - DKBED) / KSPDUM (NBK (JBK), NCE + 1)
                  END IF

                  ! NCEDUM AND FNDUM ARE THE 1ST ESTIMATES FOR NCEBD AND FNCEBD.
                  ! THEY ARE THE CORRECT VALUES FOR A TOTAL BED THICKNESS OF DBDI METRES.
                  ! CHANGES ARE MADE LATER SO THAT A SINGLE OVERLAP NUMBER AND FRACTION
                  ! (NOLBD AND FNOLBD) CAN BE ASSOCIATED WITH THE REGION BELOW THE DEEP BED.

                  asum = zero
                  JFCE(JBK) = JA + SIGN(2, 2 - JA)
                  NOLP = NOLBT (NBK (JBK), NCEDUM (JBK) + 1, JFCE (JBK)) - 1

                  fraction_loop: DO
                     NOLP = NOLP + 1
                     DUM1 = SCL * JOLFN (NBK (JBK), NOLP, JFCE (JBK))
                     asum = asum + DUM1
                     IF (asum > FNDUM (JBK)) EXIT fraction_loop
                  END DO fraction_loop

                  JOLDUM (JBK) = NOLP - 1
                  FOLDUM (JBK) = (FNDUM (JBK) - asum + DUM1) / DUM1
                  ! OVERLAP NUMBERS AND FRACTIONS ASSOCIATED WITH THE 1ST ESTIMATES
               END IF
            END IF
         END DO

         DUM1 = DBLE (JOLDUM (1)) + FNDUM (1)
         DUM2 = DBLE (JOLDUM (2)) + FNDUM (2)
         IF (DUM1 <= DUM2) THEN
            NOLBD = JOLDUM (1)
            FNOLBD = FNDUM (1)
            NCEBD (NLINK, 1) = NCEDUM (1)
            FNCEBD (NLINK, 1) = FNDUM (1)
            LDUM = 2
         ELSE
            NOLBD = JOLDUM (2)
            FNOLBD = FNDUM (2)
            NCEBD (NLINK, 2) = NCEDUM (2)
            FNCEBD (NLINK, 2) = FNDUM (2)
            LDUM = 1
         END IF

         NCDUM = NOLCE (NBK (LDUM), NOLBD, JFCE (LDUM))
         NOLDUM = NOLBT (NBK (LDUM), NCDUM + 1, JFCE (LDUM)) - 1
         ! HIGHEST OVERLAP ASSOC. WITH NCDUM

         DUM3 = FNOLBD * SCL * DBLE (JOLFN (NBK (LDUM), NOLBD + 1, JFCE (LDUM)))
         ! FRACTION OF NEXT HIGHEST CELL COVERED BY FRACTION OF OVERLAP

         IF (NOLDUM == NOLBD) THEN
            NCEBD (NLINK, LDUM) = NCDUM
            FNCEBD (NLINK, LDUM) = DUM3
         ELSE
            NCEBD (NLINK, LDUM) = NCDUM - 1
            asum = DUM3
            DO NOLP = NOLBT (NBK (LDUM), NCDUM, JFCE (LDUM)), NOLBD
               asum = asum + SCL * DBLE (JOLFN (NBK (LDUM), NOLP, JFCE (LDUM)))
            END DO
            FNCEBD (NLINK, LDUM) = asum
         END IF
         ! SET FINAL VALUES FOR THE OVERLAP NUMBERS NOLBD AND FRACTIONS FNOLBD
         ! FOR THE REGION BELOW THE DEEP BED; AND SET THE CELL NUMBERS NCEBD
         ! AND FRACTIONS FNCEBD ACCORDINGLY

         asum = zero
         DO JBK = 1, 2
            DO NCE = NCEBD (NLINK, JBK) + 1, NHBED (NLINK, JBK) + 1
               asum = asum + KSPDUM (NBK (JBK), NCE)
            END DO
            asum = asum - FNCEBD (NLINK, JBK) * KSPDUM (NBK (JBK), NCEBD (NLINK, JBK) + 1)
            asum = asum - (one - FHBED (NLINK, JBK)) * KSPDUM (NBK (JBK), NHBED (NLINK, JBK) + 1)
         END DO

         ACPBSG (NLINK) = DBS * CWIDTH (NLINK) / Z2SQ
         ACPBI (NLINK) = (half * asum * CWIDTH (NLINK) / Z2) - ACPBSG (NLINK)
         ! SET BED SURFACE LAYER THICKNESS TO DBS METRES, AND THE COMBINED AREA OF THE
         ! BED SURFACE AND DEEP LAYERS TO THE AREA ABOVE OVERLAP NOLBD AND FRACTION FNOLBD

         DO JBK = 1, 2
            ! uuuuuuu ADJUST TRANSMISIVITIES FOR uuuuuuu
            ! UPSTREAM AND DOWNSTREAM SUBSURFACE FLOW IN BANKS
            NCE1 = NHBED (NLINK, JBK)
            DO JA = 1, 4
               NDUMA = ICMREF (NBK (JBK), JA + 4)
               IF (NDUMA /= 0) THEN
                  ITYPEA = ICMREF (NDUMA, 1)
                  IF (ITYPEA == 1 .OR. ITYPEA == 2) THEN
                     ! THE ELEMENT UPSTREAM OR DOWNSTREAM FROM BANK JBK OF LINK NLINK IS ITSELF A BANK
                     NOL1 = NOLBT (NBK (JBK), NCE1 + 1, JA) - 1
                     NBKU = NDUMA
                     NLINKU = ICMREF (NBKU, 4)

                     IF (ICMBK (NLINKU, 1) == NBKU) THEN
                        JBKU = 1
                     ELSE
                        JBKU = 2
                     END IF

                     NCE2 = NHBED (NLINKU, JBKU)
                     NOL2 = NOLBT (NBKU, NCE2 + 1, ICMREF (NBK (JBK), JA + 8)) - 1
                     ! USE ICMREF SO CORRECT FACE IS FOUND EVEN IF THE UPSTREAM OR DOWNSTREAM BANK IS ROUND A CORNER

                     NOLX = MIN (NOL1, NOL2)
                     DUM1 = cellarea (NBK (JBK)) / CLENTH (NLINK) + cellarea (NBKU) / CLENTH (NLINKU)
                     DUM2 = half * (cellarea (NLINK) / CLENTH (NLINK) + cellarea (NLINKU) / CLENTH (NLINKU))
                     DMULT = DUM1 / (DUM1 + DUM2)

                     DO NOLP = NOLX + 1, NOL (NBK (JBK), JA)
                        JKZCOL (NBK (JBK), NOLP, JA) = MAX (1, INT (DMULT * JKZCOL (NBK (JBK), NOLP, JA)))
                     END DO
                  END IF
               END IF
            END DO
         END DO

         DO JBK = 1, 2
            NCEAB (NLINK, JBK) = NHBED (NLINK, JBK)
         END DO

      END DO

      DO NCONT = 1, NCON
         ! xxxxxxx INITIALISE VARIABLES WHICH DEPEND ON CONTAMINANT NUMBER xxxxxxx
         CCAPIO (NCONT) = CCAPI (NCONT)
         IIICFO (NCONT) = IIICF (NCONT)
      END DO

      DO NLINK = 1, total_no_links
         ! ooooooo INITIALISE LINK VARIABLES oooooooo
         ACPSFO (NLINK) = ARXL (NLINK) / Z2SQ
         ACPBDO (NLINK) = ACPBI (NLINK)

         DO NCONT = 1, NCON
            CCCCO (NLINK, NCETOP - 2:NCETOP, NCONT) = CCAPIN (NCONT)
            CCCC (NLINK, NCETOP - 2:NCETOP, NCONT)  = CCAPIN (NCONT)
         END DO

         asumK = zero
         asum = zero
         DO JBK = 1, 2
            NDUM = NCEBD (NLINK, JBK) + 1
            NCE = NDUM
            DUMK = (one - FNCEBD (NLINK, JBK)) * KSPDUM (ICMBK (NLINK, JBK), NCE)
            asumK = asumK + DUMK
            asum = asum + VSTHE (NCE, NBK (JBK)) * DUMK

            DO NCE = NDUM + 1, NHBED (NLINK, JBK)
               DUMK = KSPDUM (ICMBK (NLINK, JBK), NCE)
               asumK = asumK + DUMK
               asum = asum + VSTHE (NCE, NBK (JBK)) * DUMK
            END DO

            NCE = NHBED (NLINK, JBK) + 1
            DUMK = FHBED (NLINK, JBK) * KSPDUM (ICMBK (NLINK, JBK), NCE)
            asumK = asumK + DUMK
            asum = asum + VSTHE (NCE, NBK (JBK)) * DUMK
         END DO

         THBEDO (NLINK) = MIN (PBSED (NLINK), asum / asumK)
         THBED (NLINK) = THBEDO (NLINK)

         ARL = DLS (NLINK) * CWIDTH (NLINK)
         ARP = (ACPBI (NLINK) - ACPBSG (NLINK)) * Z2SQ
         DUM = one / (ARL + ARP)

         DO JSED = 1, NSED
            ! sb temp fix 09022026: NSOBED fallback
            IF (NSOBED (NLINK) == 0) NSOBED (NLINK) = 1

            FBBEDO (NLINK, JSED) = DUM * (ARL * FBETA (NLINK, JSED) + ARP * SOSDFN (NSOBED (NLINK), JSED))
            FDELO (NLINK, JSED) = FDEL (NLINK, JSED)
            FBTSDO (NLINK, JSED) = FBTSD (NLINK, JSED)
         END DO
      END DO

      DO NCL = total_no_links + 1, total_no_elements
         ! iiiiii INITIALISE COLUMN VARIABLES iiiiiii
         DLSO (NCL) = DLS (NCL)
         DSWO (NCL) = GETHRF (NCL) - ZGRUND (NCL)
         GNUO (NCL) = GNU (NCL)
         QIO (NCL) = -PNETTO (NCL) * cellarea (NCL)
         QQRFO (NCL) = QVSV (NCOLMB (NCL), NCL) * cellarea (NCL)
         RSZWLO (NCL) = zero
         ZONEO (NCL) = (ZGRUND (NCL) - ZCOLMB (NCL)) / Z2

         DO JDUM = 1, 2
            QQQSWO (NCL, JDUM) = -QOC (NCL, JDUM)
            QQQSWO (NCL, JDUM + 2) = QOC (NCL, JDUM + 2)
         END DO

         ! set up variables for l-shaped bank calculations, if required
         ITYPE = ICMREF (NCL, 1)
         IF (ITYPE /= 0) THEN
            JBK = ITYPE
            NLINKA = ICMREF (NCL, 4)
            JAL = 0

            link_face_loop: DO
               JAL = JAL + 1
               IF (ICMREF (NLINKA, JAL + 4) == NCL) EXIT link_face_loop
            END DO link_face_loop

            JFLINK = ICMREF (NLINKA, JAL + 8)
            DBK = cellarea (NCL) / CLENTH (NLINKA)
            DMULT = DBK / (DBK + half * CWIDTH (NLINKA))

            DO NCE = NLYRBT (NCL, 1) - 1, NCEBD (NLINKA, JBK)
               ROH (NCE) = DMULT
            END DO

            NCE = NCEBD (NLINKA, JBK) + 1
            ROH (NCE) = one - (one - DMULT) * FNCEBD (NLINKA, JBK)

            DO NCE = NCEBD (NLINKA, JBK) + 2, LLEE
               ROH (NCE) = one
            END DO
         END IF

         DO NCE = 1, top_cell_no  !LLEE  !JE
            GGAMMO (NCL, NCE) = zero
            DO JA = 1, 4
               QQO (NCL, NCE, JA) = QVSH (JA, NCE, NCL)
            END DO

            DO NCONT = 1, NCON
               CCCCO (NCL, NCE, NCONT) = CCAPIN (NCONT)
               SSSSO (NCL, NCE, NCONT) = CCAPIN (NCONT)
               CCCC (NCL, NCE, NCONT) = CCAPIN (NCONT)
               SSSS (NCL, NCE, NCONT) = CCAPIN (NCONT)
            END DO
         END DO

         ! calculate theta and vert vel for L-shaped bank, if required
         IF (ITYPE == 0) THEN
            DO NCE = NLYRBT (NCL, 1) - 1, top_cell_no
               VSTHEO (NCL, NCE) = VSTHE (NCE, NCL)
               UUAJPO (NCL, NCE) = QVSV (NCE, NCL)
            END DO
         ELSE
            NDIFF = NLYRBT (NLINKA, 1) - NLYRBT (NCL, 1)
            DO NCE = NLYRBT (NCL, 1) - 1, top_cell_no
               NCEA = NCE + NDIFF
               IF (NCEA <= top_cell_no) THEN
                  VSTHEO (NCL, NCE) = ((one - ROH (NCE)) * VSTHE (NCEA, NLINKA) + ROH (NCE) * VSTHE (NCE, NCL))
                  UUAJPO (NCL, NCE) = ((one - ROH (NCE)) * QVSV (NCEA, NLINKA) + ROH (NCE) * QVSV (NCE, NCL)) / ROH (NCE)
               ELSE
                  VSTHEO (NCL, NCE) = VSTHE (NCE, NCL)
                  UUAJPO (NCL, NCE) = QVSV (NCE, NCL)
               END IF
            END DO
         END IF

      END DO

      ! New code by SB --------------
      DO NCONT = 1, NCON
         IF (ISCNSV (NCONT)) THEN
            CALL ALINTP (LLEE, NCETOP, total_no_elements, NELEE, total_no_links, NUM_CATEGORIES_TYPES (NCONT), &
               MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCATTY (total_no_links + 1, NCONT), NCOLMB (total_no_links + 1), &
               NTAB (1, NCONT), TABLE_CONCENTRATION (1, 1, NCONT), TABLE_WATER_DEPTH (1, 1, NCONT), &
               DELTAZ, ZVSNOD, DUMMYCONC)

            DO NCL = total_no_links + 1, total_no_elements
               DO NCE = NCOLMB (NCL), NCETOP
                  CCCC (NCL, NCE, NCONT) = DUMMYCONC (NCL, NCE)
                  SSSS (NCL, NCE, NCONT) = CCCC (NCL, NCE, NCONT)
                  ! ADDED SB 6/3/00
                  SSSSO (NCL, NCE, NCONT) = CCCC (NCL, NCE, NCONT)
                  CCCCO (NCL, NCE, NCONT) = CCCC (NCL, NCE, NCONT)
               END DO
            END DO
         END IF
      END DO
      ! End of new code by SB -------

      IF (ISPLT) CALL INPL

   END SUBROUTINE INCM



   !SSSSSS SUBROUTINE INET
   SUBROUTINE INET
   !----------------------------------------------------------------------*
   ! THIS SUBROUTINE READS IN PARAMETERS REQUIRED FOR THE ET COMPONENT
   ! AND CARRIES OUT INITIALISATION CALCULATIONS
   ! IT IS ASSUMED THAT MET SITE CODES AND VEGETATION CODES HAVE BEEN
   ! READ IN THE GLOBAL INITIALISATION ROUTINES
   ! VARIABLE NAMES ARE AS SPECIFIED IN IH SHE REPORT 8, MAY 1978
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/ET/INET/4.2
   ! Modifications since v3.3:
   !  GP       3.4  Don't call METIN (see TMSTEP).
   ! RAH  941001 3.4.1 Add IMPLICIT DOUBLEPRECISION (see AL.P).
   ! RAH  970516  4.1  Scrap WEP,WETEX,WETOCE,WEXET,WSET,WSETER,WSETI
   !                   (AL.D), DWETER (SPEC.ET) & AKKEP,AKKEA,AKKP (local).
   !                   Bring HEAD,ZU,ZD,ZO from SPEC.ET.
   !                   HEAD is type CHAR (was DBLE).  Explicit typing.
   !                   Scrap SPEC.ET output arrays NUMCST,NUMPLA,NUMCLA,
   !                   NUMVHT: use local JJJ.
   ! RAH  981021  4.2  Scrap AL.D outputs CSTOLD,EPOTR.
   !                   Replace VK^2 with constant VKSQ.
   !----------------------------------------------------------------------*

      ! Assumed global variables provided via host module(s):
      ! NV, BHOTRD, NGDBGN, total_no_elements, NRAIN, ETD, PPPRI, NM
      ! PRD, EPD, MED, TAH, TAL, ISTA, FFFATAL
      ! CSTCAP, RC, BAR, MODE, CSTORE, precip_m_per_s, TIMEUZ, BMETDATES
      ! BMETP, BINETP, BMETAL, DTMET, DTMET2, DTMET3, MEASPE, RA, ZU, ZD, ZO
      ! NF, PLAI, CK, CB, NRD, CLAI, VHT, RDL, MODECS, MODEPL, MODECL, MODEVH
      ! NCTCST, CSTCA1, RELCST, TIMCST, NCTPLA, PLAI1, RELPLA, TIMPLA
      ! NCTCLA, CLAI1, RELCLA, TIMCLA, NCTVHT, VHT1, RELVHT, TIMVHT
      ! PS1, RCF, FET, RDF, RTOP

      IMPLICIT NONE

      ! --- LOCAL VARIABLES ---

      ! Scalars
      INTEGER          :: I, IEL, IIMEAS, J, JJ, JJJ, N1, N2, ios, N
      DOUBLE PRECISION :: DEPTH, ASUM
      CHARACTER(LEN=80):: HEAD

      ! Missing local arrays used for Energy Budget calculations
      ! Defined with NVEE size as per the common block logic
      DOUBLE PRECISION :: ZU(NVEE), ZD(NVEE), ZO(NVEE)

      ! Constants
      DOUBLE PRECISION, PARAMETER :: VKSQ = 0.1681D0 ! (0.41^2)

   !----------------------------------------------------------------------*

      ! INITIAL VALUES
      init_veg_loop: DO I = 1, NV
         CSTCAP(I) = 0.0D0
         RC(I)     = 0.0D0
         BAR(I)    = .FALSE.
         MODE(I)   = 0
      END DO init_veg_loop

      ! CHECK IF HOTSTART
      IF (.NOT. BHOTRD) THEN
         init_store_loop: DO IEL = NGDBGN, total_no_elements
            CSTORE(IEL) = 0.0D0
         END DO init_store_loop
      END IF

      precip_m_per_s = 0.0D0
      TIMEUZ = 0.0D0

      !-----READ PRINTCONTROL PARAMETERS
      !:ET1
      READ(ETD, '(A)') HEAD

      ! new code 10202026 BMETDATES added
      ! if true then the prd, epd and temperature files contain dates in the first column
      ! for backwards compatibility the default is false and BMETDATES will not be present in line ET1
      BMETDATES = .FALSE.
      READ(ETD, '(A)') HEAD
      READ(HEAD, '(4L7)', IOSTAT=ios) BMETP, BINETP, BMETAL, BMETDATES
      IF (ios /= 0) THEN
         READ(HEAD, '(3L7)', IOSTAT=ios) BMETP, BINETP, BMETAL
         BMETDATES = .FALSE.
      END IF

      !-----READ TIMESTEP FOR INPUT OF MET AND RAINDATA,
      !     TIMECONSTANT FOR RAINFALL DISTRIBUTION
      !:ET3
      READ(ETD, '(A)') HEAD
      ! sb 300407 convert breakpoint data to regularly spaced data
      READ(ETD, *) DTMET, DTMET2, DTMET3

      !-----READ WHETHER POTENTIAL EVAP IS MEASURED AND THEREFORE TO
      !     BE READ IN DIRECTLY FOR EACH MET STATION IN TURN.
      !     MEASPE = 0 : POTENTIAL EVAP NOT MEASURED
      !            = 1 : POTENTIAL EVAP MEASURED
      !:ET5
      READ(ETD, '(A)') HEAD
      READ(ETD, '(10I7)') (MEASPE(IIMEAS), IIMEAS = 1, NM)

      !---------------------------------
      !  LOOP ON VEGETATION TYPES....
      !---------------------------------
      veg_type_loop: DO I = 1, NV

         IF (BINETP) WRITE(PPPRI, "('0'//1X, 'VEGETATION TYPE', I6/1X, 22('*'))") I

         !:ET7
         READ(ETD, '(A)') HEAD
         IF (BINETP) WRITE(PPPRI, "('0'//1X, A)") TRIM(HEAD)

         !-------------------------------------
         !  READ PARAMETER DATA
         !-------------------------------------
         READ(ETD, '(L7, 5F7.0, I7/I7, 4F7.0, I7, 3F7.0)') &
              BAR(I), RA(I), ZU(I), ZD(I), ZO(I), RC(I), MODE(I), NF(I), &
              PLAI(I), CSTCAP(I), CK(I), CB(I), NRD(I), CLAI(I), VHT(I), RDL(I)

         IF (BINETP) WRITE(PPPRI, "('0', 1X, 'ET COMPONENT WITH MODE', I6, 2X, 'OPERATION')") MODE(I)

         !-----WRITE PARAMETER DATA
         IF (BINETP) WRITE(PPPRI, "('0', 'PARAMETERS'/1X, 10('*')//10X, 'PLAI', F15.8/10X, " // &
                                  "'CSTCAP', F13.8/10X, 'CK', F17.8/10X, 'CB', F17.8/10X, " // &
                                  "'CLAI', F15.8/10X, 'VHT', F16.8/10X, 'RDL', F16.8)") &
                                  PLAI(I), CSTCAP(I), CK(I), CB(I), CLAI(I), VHT(I), RDL(I)

         IF (BAR(I) .AND. BINETP) WRITE(PPPRI, "(' ', 10X, 'VARIABLE RA WITH'/10X, 'ZO', F17.4/10X, " // &
                                               "'ZD', F18.4/10X, 'ZU', F17.4)") ZO(I), ZD(I), ZU(I)

         IF (.NOT. BAR(I) .AND. BINETP) WRITE(PPPRI, "(' ', 10X, 'CONSTANT RA =', F10.4)") RA(I)

         !--------------------------------------------------------
         !    READ TABULAR VARIATION OF TIME-VARYING PARAMETERS
         !--------------------------------------------------------
         !:ET9
         READ(ETD, '(A)') HEAD

         !-----READ MODE: 0=CONSTANT; 1=TIME-VARYING
         READ(ETD, '(4I7)') MODECS(I), MODEPL(I), MODECL(I), MODEVH(I)

         !-----CHECK MODE FOR TIME-VARYING CSTCAP
         IF (BINETP) WRITE(PPPRI, "('0', 1X, 'MODE FOR CSTCAP FOR VEGETATION', I3, ' IS', I3, 3X, " // &
                                  "'(0=CONSTANT; 1=TIME-VARYING)')") I, MODECS(I)

         IF (MODECS(I) /= 0) THEN
            NCTCST(I) = 1
            CSTCA1(I) = CSTCAP(I)

            !-----READ NUMBER OF VALUES IN CSTCAP VARIATION TABLE
            !:ET11(1/4)
            READ(ETD, '(A)') HEAD
            READ(ETD, '(I7)') JJJ
            !:ET13(1/4)
            READ(ETD, '(A)') HEAD
            IF (BINETP) WRITE(PPPRI, "('0'//1X, A)") TRIM(HEAD)

            !-----READ TIME-VARYING CSTCAP VALUES
            cstcap_loop: DO JJ = 1, JJJ
               READ(ETD, *) RELCST(I, JJ), TIMCST(I, JJ)
               IF (BINETP) WRITE(PPPRI, "(2G10.3)") RELCST(I, JJ), TIMCST(I, JJ)
            END DO cstcap_loop
         END IF

         !-----CHECK MODE FOR TIME-VARYING PLAI
         IF (BINETP) WRITE(PPPRI, "('0', 1X, 'MODE FOR PLAI FOR VEGETATION', I3, ' IS', I3, 3X, " // &
                                  "'(0=CONSTANT; 1=TIME-VARYING)')") I, MODEPL(I)

         IF (MODEPL(I) /= 0) THEN
            NCTPLA(I) = 1
            PLAI1(I)  = PLAI(I)

            !-----READ NUMBER OF VALUES IN PLAI VARIATION TABLE
            !:ET11(2/4)
            READ(ETD, '(A)') HEAD
            READ(ETD, '(I7)') JJJ
            !:ET13(2/4)
            READ(ETD, '(A)') HEAD
            IF (BINETP) WRITE(PPPRI, "('0'//1X, A)") TRIM(HEAD)

            !-----READ TIME-VARYING PLAI VALUES
            plai_loop: DO JJ = 1, JJJ
               READ(ETD, *) RELPLA(I, JJ), TIMPLA(I, JJ)
               IF (BINETP) WRITE(PPPRI, "(2G10.3)") RELPLA(I, JJ), TIMPLA(I, JJ)
            END DO plai_loop
         END IF

         !-----CHECK MODE FOR TIME-VARYING CLAI
         IF (BINETP) WRITE(PPPRI, "('0', 1X, 'MODE FOR CLAI FOR VEGETATION', I3, ' IS', I3, 3X, " // &
                                  "'(0=CONSTANT; 1=TIME-VARYING)')") I, MODECL(I)

         IF (MODECL(I) /= 0) THEN
            NCTCLA(I) = 1
            CLAI1(I)  = CLAI(I)

            !-----READ NUMBER OF VALUES IN CLAI VARIATION TABLE
            !:ET11(3/4)
            READ(ETD, '(A)') HEAD
            READ(ETD, '(I7)') JJJ
            !:ET13(3/4)
            READ(ETD, '(A)') HEAD
            IF (BINETP) WRITE(PPPRI, "('0'//1X, A)") TRIM(HEAD)

            !-----READ TIME-VARYING CLAI VALUES
            clai_loop: DO JJ = 1, JJJ
               READ(ETD, *) RELCLA(I, JJ), TIMCLA(I, JJ)
               IF (BINETP) WRITE(PPPRI, "(2G10.3)") RELCLA(I, JJ), TIMCLA(I, JJ)
            END DO clai_loop
         END IF

         !-----CHECK MODE FOR TIME-VARYING VHT
         IF (BINETP) WRITE(PPPRI, "('0', 1X, 'MODE FOR VHT FOR VEGETATION', I3, ' IS', I3, 3X, " // &
                                  "'(0=CONSTANT; 1=TIME-VARYING)')") I, MODEVH(I)

         IF (MODEVH(I) /= 0) THEN
            NCTVHT(I) = 1
            VHT1(I)   = VHT(I)

            !-----READ NUMBER OF VALUES IN VHT VARIATION TABLE
            !:ET11(4/4)
            READ(ETD, '(A)') HEAD
            READ(ETD, '(I7)') JJJ
            !:ET13(4/4)
            READ(ETD, '(A)') HEAD
            IF (BINETP) WRITE(PPPRI, "('0'//1X, A)") TRIM(HEAD)

            !-----READ TIME-VARYING VHT VALUES
            vht_loop: DO JJ = 1, JJJ
               READ(ETD, *) RELVHT(I, JJ), TIMVHT(I, JJ)
               IF (BINETP) WRITE(PPPRI, "(2G10.3)") RELVHT(I, JJ), TIMVHT(I, JJ)
            END DO vht_loop
         END IF

         !--------------------------------------------------
         !    END OF READING TIME-VARYING PARAMETERS
         !--------------------------------------------------

         !-----CHECK MODE FOR EVAPOTRANSPIRATION CALCULATIONS
         IF (MODE(I) /= 1 .AND. MODE(I) /= 4) THEN
            !---------------------------------------------
            !  READ AND WRITE PSI/RCF/FET FUNCTION DATA.
            !---------------------------------------------
            !:ET15
            READ(ETD, '(A)') HEAD
            N1 = NF(I)
            READ(ETD, '(3F7.2)') (PS1(I, J), RCF(I, J), FET(I, J), J = 1, N1)

            IF (BINETP) WRITE(PPPRI, "('0'//1X, A)") TRIM(HEAD)
            IF (BINETP) WRITE(PPPRI, "(' ', 3F10.2)") (PS1(I, J), RCF(I, J), FET(I, J), J = 1, N1)
         ELSE
            WRITE(PPPRI, "(' ', 10X, 'CONSTANT RC =', F10.4)") RC(I)
         END IF

         !-----READ AND WRITE ROOT DENSITY FUNCTION DATA
         !:ET17
         READ(ETD, '(A)') HEAD
         ! --------------------------------------------------------
         !  NOTE THAT IT IS ASSUMED HERE THAT DEPTHS CORRESPOND
         !  TO THE NODE DEPTHS FOR THE UZ SOLUTION, SO THAT
         !  EACH NODE IN THE ROOT ZONE HAS A CORRESPONDING RDF
         !  VALUE.  THE VALUES SHOULD BE INPUT FROM THE SURFACE
         !  DOWNWARDS.
         !---------------------------------------------------------
         IF (BINETP) WRITE(PPPRI, "('0'//1X, A)") TRIM(HEAD)

         ASUM = 0.0D0
         N2 = NRD(I)

         rdf_loop: DO J = 1, N2
            READ(ETD, '(2F7.4)') DEPTH, RDF(I, J)
            IF (BINETP) WRITE(PPPRI, "(' ', 2F15.6)") DEPTH, RDF(I, J)
            ASUM = ASUM + RDF(I, J)
         END DO rdf_loop

         IF (BINETP) WRITE(PPPRI, "('0', 1X, 'SUM OF RDF VALUES IS', F10.4)") ASUM

         IF (BAR(I)) RTOP(I) = LOG((ZU(I) - ZD(I)) / ZO(I))**2 / VKSQ

      END DO veg_type_loop
      !-----END OF VEGETATION LOOP

      !-----------------------------------
      !    READ IN METEOROLOGICAL DATA
      !-----------------------------------
      IF (BMETAL) THEN
         ! Modernization Fix: Replaced GOTO traps with strict IOSTAT handling
         READ(PRD, *, IOSTAT=ios)
         IF (ios /= 0) CALL ERROR(FFFATAL, 1063, PPPRI, 0, 0, 'no data in prd file')

         READ(EPD, *, IOSTAT=ios)
         IF (ios /= 0) CALL ERROR(FFFATAL, 1064, PPPRI, 0, 0, 'no data in epd file')
      ELSE
         READ(MED, *, IOSTAT=ios)
         IF (ios /= 0) CALL ERROR(FFFATAL, 1065, PPPRI, 0, 0, 'no data in med file')
      END IF

      IF (ISTA) THEN
         READ(TAH, *, IOSTAT=ios)
         IF (ios /= 0) CALL ERROR(FFFATAL, 1066, PPPRI, 0, 0, 'no data in air temp - high file')

         READ(TAL, *, IOSTAT=ios)
         IF (ios /= 0) CALL ERROR(FFFATAL, 1067, PPPRI, 0, 0, 'no data in air temp - low file')
      END IF

   END SUBROUTINE INET



   !SSSSSS SUBROUTINE INFR
   SUBROUTINE INFR
   !-------------------------
   !
   !
   !     READ AND INITIALIZE DATA WHICH IS COMMON TO TWO OR MORE COMPONENTS
   !         - ORGANISATION AND FILE NOS.        FRD,MED,ETD,UZD,OCD,SZD,
   !                                             SMD,PRI,RES,HOT,SED
   !         - JOB TITLE.
   !         - MODEL SIZE.                       NX,NY
   !          - START TIME OF SIMULATION
   ! ISYEAR,ISMTH,ISDAY,ISHOUR,ISMIN
   !          - END   TIME OF SIMULATION
   ! IEYEAR,IEMTH,IEDAY,IEHOUR,IEMIN
   !         - H-H GRID SIZES IN X-DIRECTION.    DXIN
   !         - H-H GRID SIZES IN Y-DIRECTION.    DYIN
   !         - PRINTING CONTROL.                 DTAO,IAOUT,BINFRP,
   !                                             BFRTS1,BFRTS2,BSTORE
   !         - CONTROLS FOR SELECTION OF         BPPNET,BPEPOT,BPQOC,
   !             RESULTS TO BE PRINTED           BPDEP,BPQF,BPQH,BPQSZ,
   !                                             BPHSZ,BPBAL
   !         - COMPONENT EXECUTION CONTROL.      BEXET,BEXUZ,BEXOC,BEXSZ,
   !                                             BEXSM
   !         - NO. OF METEOROLOGICAL SITES,
   !             RAINFALL STATIONS,
   !             VEGETATION TYPES AND SOIL
   !             TYPES.                          NM,NRAIN,NV,NS
   !         - RIVER LINING PARAMETERS.          BLOWP,DB,CCB
   !         - DEFAULT MET- VEG- AND SOILCODES   IDMC,IDRA,IDVE,IDS1,IDS2
   !         - GROUND SURFACE LEVEL.             ZGRUND
   !         - IMPERMEABLE BED LEVEL.            ZBED
   !         - METEOROLOGICAL SITE CODES.        NMC
   !         - RAINFALL STATION CODES.           NRAINC
   !         - VEGETATION CODES.                 NVC
   !         - SOIL CODES - UNDER ROOT ZONE.     NSC1
   !         - SOIL CODES - ROOT ZONE.           NSC2
   !         - GRID CODE FOR UZ, SZ AND FRAME    INGRID

      ! Assumed external module dependencies providing global variables:
      ! PPPRI, BDEVER, SHEVER, BANNER, FRD, TITLE, nxee, nyee, nlfee, nelee,
      ! llee, nvee, nsee, NVSEE, NVBP, NUZTAB, NLYREE, NXOCEE, NOCTAB, NSEDEE,
      ! NCONEE, NOLEE, NPLTEE, NPELEE, max_no_snowmelt_slugs, NXSCEE, NX, NY,
      ! NXM1, NYM1, NXP1, NYP1, DXIN, DYIN, DTAO, IAOUT, BINFRP, BFRTS1,
      ! BFRTS2, BSTORE, PSTART, PMAX, PALFA, QMAX, TMAX, BSOFT, PREST, two,
      ! BPPNET, BPEPOT, BPQOC, BPDEP, BPQF, BPQH, BPQSZ, BPHSZ, BPBAL, BPSD,
      ! BEXSM, BEXBK, BEXSY, BEXCM, BEXET, BEXUZ, BEXOC, BEXSZ, BEXEX, BHOTRD,
      ! BHOTPR, BHOTTI, BHOTST, HOUR_FROM_DATE, mbyear, mbmon, mbday, TSH, TCH,
      ! NM, NRAIN, NV, NS, INGRID, LCODEX, LCODEY, NXE, NYE, ZGRUND, NMC, NRAINC,
      ! NVC, TOUTPUT, ALLOUT, NXEP1, NYEP1, ISORT, total_no_elements, OCLTL,
      ! FRIND, AREADR, AREADI

      IMPLICIT NONE

      INTEGER :: nxplus, isyear, ismth, isday, ishour, ismin, ieyear, iemth, ieday, iehour, iemin, &
         jsyear, jsmth, jsday, jshour, jsmin, jcyear, jcmth, jcday, jchour, jcmin, j, k, &
         nlyrct, ipr, idmc, idra, idve, idlyr, i1, i2, i, ipflg, iel, ios
      DOUBLE PRECISION :: tthx

      !----------------------------------------------------------------------*

      WRITE(PPPRI, 10)
10    FORMAT ('1', // T10, '                                E'/T10, &
              ' EUROPEAN HYDROLOGIC SYSTEM  S  H  E  SYSTEME HYDROLOGIQUE EUROPEEN'/T10, &
              '                                S' /)

      ! PRINT THE CURRENT VERSION NUMBER
      IF (BDEVER) THEN
         WRITE(PPPRI, 16) SHEVER
      ELSE
         WRITE(PPPRI, 15) SHEVER
      END IF
16    FORMAT (/ 'SHETRAN VERSION NUMBER: ', F5.1, ' ')
15    FORMAT (/ 'SHETRAN VERSION NUMBER: ', F5.1)

      WRITE(PPPRI, 17) BANNER
17    FORMAT(/A80/)

      ! READ AND PRINT JOB TITLE.
      ! :FR1
      WRITE(PPPRI, '(A)') 'Catchment Name '
      WRITE(PPPRI, '(A)') '************** '
      READ (FRD, '(A)') TITLE

      WRITE(PPPRI, '(A)') TITLE
      WRITE(PPPRI, '(/ 20A4, //, 100("="))') TITLE

      WRITE(PPPRI, *)
      WRITE(PPPRI, '(A)') 'Fixed array sizes in this version of SHETRAN '
      WRITE(PPPRI, '(A)') '******************************************** '
      WRITE(PPPRI, '(A)') 'Grid points in x,y directions, river links, total no of elements. THESE ARE THE MOST IMPROTANT ONES'
      WRITE(PPPRI, '(4(A,I0))') ' NXEE = ', nxee, '  NYEE = ', nyee, '  NLFEE = ', nlfee, '  NELEE = ', nelee
      WRITE(PPPRI, *)
      WRITE(PPPRI, '(A)') 'Grid points in vertical'
      WRITE(PPPRI, '(1(A,I0))') ' LLEE = ', llee
      WRITE(PPPRI, *)
      WRITE(PPPRI, '(A)') 'Vegetation types, soil typess (NVEE also used for number of precipitation and pet stations)'
      WRITE(PPPRI, '(2(A,I0))') ' NVEE = ', nvee, '  NSEE = ', nsee
      WRITE(PPPRI, *)
      WRITE(PPPRI, '(A)') 'Tables in the VSS component, time varying veg breakpoints, Tables in the ET component (max number of PSI/RCF/FET values, Maximum number of ssoi layers'
      WRITE(PPPRI, '(4(A,I0))') ' NVSEE = ', NVSEE, '  NVBP = ', NVBP, '  NUZTAB = ', NUZTAB, '  NLYREE = ', NLYREE
      WRITE(PPPRI, *)
      WRITE(PPPRI, '(A)') 'Maximum number of elements(Grids,banks and links) in a row, Tables used in OC component, sediment sze fractions'
      WRITE(PPPRI, '(3(A,I0))') ' NXOCEE = ', NXOCEE, '  NOCTAB = ', NOCTAB, '  NSEDEE = ', NSEDEE
      WRITE(PPPRI, *)
      WRITE(PPPRI, '(A)') 'Number of contaminants, number of overlaps, number of plants in an element, total number of plants for contaminants'
      WRITE(PPPRI, '(4(A,I0))') ' NCONEE = ', NCONEE, '  NOLEE = ', NOLEE, '  NPLTEE = ', NPLTEE, '  NPELEE = ', NPELEE
      WRITE(PPPRI, *)
      WRITE(PPPRI, '(A)') 'Number of snow meltwater slugs, Size of internal tables for channel conveyance'
      WRITE(PPPRI, '(2(A,I0))') ' max_no_snowmelt_slugs = ', max_no_snowmelt_slugs, '  NXSCEE = ', NXSCEE
      WRITE(PPPRI, *)

      WRITE(PPPRI, 20)
20    FORMAT (/ ' ^^^ ENTER INFR ^^^')

      ! READ AND PRINT MODEL SIZE, TOTAL SIMULATION TIME, GRID SIZES AND
      ! PRINTING CONTROL.
      ! :FR2
      READ (FRD, *)
      READ (FRD, *) NX, NY
      NXPLUS = 0

      ! :FR4
      READ (FRD, *)
      READ (FRD, *) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN

      ! :FR6
      READ (FRD, *)
      READ (FRD, *) IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN

      ! READ START TIMES FOR SEDIMENT AND CONTAMINANT COMPONENTS
      ! :FR7a
      READ (FRD, *)
      READ (FRD, *) JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN

      ! :FR7c
      READ (FRD, *)
      READ (FRD, *) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN

      NXM1 = NX - 1
      NYM1 = NY - 1
      NXP1 = NX + 1
      NYP1 = NY + 1

      ! :FR8
      READ (FRD, '(20A4)') TITLE
      READ (FRD, '(10F7.0)') (DXIN (J), J = 1, NXM1)

      ! :FR10
      READ (FRD, '(20A4)') TITLE
      READ (FRD, '(10F7.0)') (DYIN (K), K = 1, NYM1)

      ! :FR12
      READ (FRD, '(20A4)') TITLE
      READ (FRD, '(F7.0, I7, 4L7, F7.0)') DTAO, IAOUT, BINFRP, BFRTS1, BFRTS2, BSTORE, PSTART

      ! :FR20
      READ (FRD, '(20A4)') TITLE
      READ (FRD, '(4F7.0,L7)') PMAX, PALFA, QMAX, TMAX, BSOFT

      ! PMAX = one
      ! PALFA = 0.15D0
      IF (TMAX > 2.0D0) THEN
         WRITE(PPPRI, *) '^^^ TIMESTEP LIMITED TO 2 HOURS ^^^'
         TMAX = 2.0D0
      END IF

      PREST = (1.0D0 + PALFA)

      IF (IAOUT == 2) THEN
         ! :FR22
         READ (FRD, '(20A4)') TITLE
         READ (FRD, '(10L7)') BPPNET, BPEPOT, BPQOC, BPDEP, BPQF, BPQH, BPQSZ, BPHSZ, BPBAL, BPSD
      END IF

      ! ---- BEX** = TRUE FOR EXECUTION AND FALSE FOR NO EXECUTION
      !      NOTE: COMPONENTS FR,ET,UZ,OC,SZ,EX ARE ALWAYS INCLUDED
      ! :FR24
      READ (FRD, '(20A4)') TITLE
      READ (FRD, '(10L7)') BEXSM, BEXBK, BEXSY, BEXCM
      BEXET = .TRUE.
      BEXUZ = .TRUE.
      BEXOC = .TRUE.
      BEXSZ = .TRUE.
      BEXEX = .TRUE.

      ! LOGICAL PARAMETERS FOR HOT START
      ! :FR26
      READ (FRD, '(20A4)') TITLE
      READ (FRD, '(2L7, 2F7.2)') BHOTRD, BHOTPR, BHOTTI, BHOTST

      ! PRINT INITIALISATION DATA
      WRITE(PPPRI, 150) NX, NY
150   FORMAT ('0'//, ' GRID SPECIFICATION'/80('*')//, ' NX = ', I4, 21X, 'NY = ', I4)
      WRITE(PPPRI, 160) (DXIN (J), J = 1, NXM1)
160   FORMAT ('0', 'H-H GRID SIZES (METERS) IN X-DIRECTION', /, (1X, 10G11.4))
      WRITE(PPPRI, 170) (DYIN (K), K = 1, NYM1)
170   FORMAT ('0', 'H-H GRID SIZES (METERS) IN Y-DIRECTION', /, (1X, 10G11.4))
      WRITE(PPPRI, 200)
200   FORMAT (' ', 80('*'))

      ! CONVERT STARTTIME AND ENDTIME TO HOURS.
      TIH = HOUR_FROM_DATE(ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN)
      TTH = HOUR_FROM_DATE(IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN)
      TTHX = TTH - TIH

      WRITE(PPPRI, 210) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN, IEYEAR, &
         IEMTH, IEDAY, IEHOUR, IEMIN, TTHX
210   FORMAT ('0'//, ' START OF SIMULATION  : ', 5I6, /, &
              ' END OF SIMULATION    : ', 5I6, /, &
              ' LENGTH OF SIMULATION : ', F10.2, ' HOURS.')

      ! store start time for mass balance
      mbyear = isyear
      mbmon = ismth
      mbday = isday

      IF (BEXSY) THEN
         TSH = HOUR_FROM_DATE(JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN)
         WRITE(PPPRI, 211) JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN, (TSH - TIH)
211      FORMAT (// ' START OF SEDIMENT SIMULATION  : ', 5I6, / &
                  '           AT SIMULATION HOUR  : ', F8.2)
      END IF

      IF (BEXCM) THEN
         TCH = HOUR_FROM_DATE(JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN)
         WRITE(PPPRI, 212) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN, (TCH - TIH)
212      FORMAT (// ' START OF CONTAMINANT SIMULATION  : ', 5I6, / &
                  '               AT SIMULATION HOUR  : ', F8.2)
      END IF

      WRITE(PPPRI, 215) TMAX
215   FORMAT ('0', //, ' BASIC TIMESTEP (HOURS) :', F8.3)

      WRITE(PPPRI, 220) DTAO
220   FORMAT ('0'//, ' PRINTING CONTROL - ALL RESULTS PRINTED AT', &
              ' INTERVALS OF DTAO = ', F7.2, ' HOURS.')

      IF (.NOT. BSTORE) WRITE(PPPRI, 230)
230   FORMAT ('0'//, ' RESULTS NOT REQUIRED ON FILE STORE.')

      IF (BSTORE) WRITE(PPPRI, 240)
240   FORMAT ('0'//, ' RESULTS RECORDED ON FILE STORE.')

      ! READ AND PRINT NM,NRAIN,NV AND NS.
      ! :FR28
      READ (FRD, '(20A4)') TITLE
      READ (FRD, '(5I7)') NM, NRAIN, NV, NS, NLYRCT
      WRITE(PPPRI, 260) NM, NRAIN, NV, NS, NLYRCT
260   FORMAT ('0'//, ' NO. OF METEOROLOGICAL SITES = ', I3, /, &
              ' NO. OF RAINFALL STATIONS = ', I3, /, &
              ' NO. OF VEGETATION TYPES = ', I3, /, &
              ' NO. OF SOIL TYPES = ', I3, /, &
              ' NO. OF SOIL HORIZON CATEGORIES = ', I3)

      ! READ RIVER LINING PARAMETERS.  BLOWP,DB,CCB,BEXTS1
      ! :FR30
      READ (FRD, '(20A4)') TITLE
      READ (FRD, *)

      ! SET PRINTING CONTROL FOR SUBROUTINES AREADR AND AREADI.
      IPR = 0
      IF (BINFRP) IPR = 1

      ! READ DEFAULT VALUES FOR MET,RAIN,VEG,SOIL-CODES. APPLIED WHEN > 0
      ! :FR32
      READ (FRD, '(20A4)') TITLE
      READ (FRD, '(6I7)') IDMC, IDRA, IDVE, IDLYR
      WRITE(PPPRI, 300) IDMC, IDRA, IDVE, IDLYR
300   FORMAT ('0', /, ' DEFAULT METEOROLOGICAL STATION CODE =', I3, /, &
              1X, 'DEFAULT RAINFALL STATION CODE       =', I3, /, &
              1X, 'DEFAULT VEGETATION GRID CODE        =', I3, /, &
              1X, 'DEFAULT SOIL HORIZON CATEGORY CODE  =', I3)

      ! READ IN MAIN CATCHMENT DEFINITION ARRAY, INGRID
      ! (NB. THIS IS NOT READ IN USING AREAD ROUTINES, AS THE
      ! INDEX ARRAY ICMREF HASN'T BEEN SET UP YET)
      !
      ! :FR34
      READ (FRD, '(20A4)') TITLE
      IF (BINFRP) WRITE(PPPRI, '( / 20A4)') TITLE

      DO I1 = 1, NY
         K = NY + 1 - I1
         READ (FRD, '(I7, 1X, 500I1)') I2, (INGRID (J, K), J = 1, NX)
         IF (BINFRP) WRITE(PPPRI, '(I7, 1X, 500I1)') I2, (INGRID (J, K), J = 1, NX)

         ! Catchment array definition check
         IF (I2 /= K) THEN
            WRITE(PPPRI, 314) TITLE, I2
314         FORMAT (//2X, 'ERROR IN DATA ', 20A4, //2X, 'IN THE VICINITY OF ', &
                    'LINE K= ', I5)
            STOP
         END IF
      END DO

      ! SET INGRID TO BE ITS INTERNAL VALUES FOR SHE (=0 IN CATCHMENT, -1 OTHER)
      DO I = 1, NX
         DO J = 1, NY
            IF (INGRID (I, J) == 1) THEN
               INGRID (I, J) = 0
            ELSE
               INGRID (I, J) = -1
            END IF
         END DO
      END DO

      ! READ THE CODES FOR OVERLAND/CHANNEL FLOW GRID BOUNDARIES
      ! :FR35a
      CALL OCLTL (NXP1, NY, LCODEX, NXE, NYE, FRD, PPPRI, BINFRP)
      ! :FR35c
      CALL OCLTL (NX, NYP1, LCODEY, NXE, NYE, FRD, PPPRI, BINFRP)

      ! INITIALISE GLOBAL INDEX ARRAY
      CALL FRIND (BINFRP)

      ! READ / PRINT ARRAYS ZGRUND, NMC, NRAIN, NVC.
      ! SET EQUAL TO DEFAULT VALUES IF THESE ARE TO BE USED.
      !
      ! :FR37
      CALL AREADR (ZGRUND, IPR, FRD, PPPRI)

      IPFLG = 3
      ! :FR43
      IF (IDMC > 0) CALL AREADI (NMC, IPFLG, IDMC, PPPRI, NM)
      IF (IDMC <= 0) CALL AREADI (NMC, IPR, FRD, PPPRI, NM)
      ! :FR46
      IF (IDRA > 0) CALL AREADI (NRAINC, IPFLG, IDRA, PPPRI, NRAIN)
      IF (IDRA <= 0) CALL AREADI (NRAINC, IPR, FRD, PPPRI, NRAIN)
      ! :FR49
      IF (IDVE > 0) CALL AREADI (NVC, IPFLG, IDVE, PPPRI, NV)
      IF (IDVE <= 0) CALL AREADI (NVC, IPR, FRD, PPPRI, NV)

      ! :FR52
      READ (FRD, '(20A4)', IOSTAT = ios) TITLE
      IF (ios == 0) READ (FRD, *, IOSTAT = ios) TOUTPUT

      ! Check if the optional outputs read successfully
      IF (ios /= 0) TOUTPUT = 24.0D0

      ! INITIALIZATION OF SOME PARAMETERS.
      ALLOUT = DTAO + PSTART
      NXEP1 = NXE + 1
      NYEP1 = NYE + 1

      ! INITIALISATION OF ISORT ARRAY
      DO IEL = 1, total_no_elements
         ISORT (IEL) = IEL
      END DO

      WRITE(PPPRI, 430)
430   FORMAT ('0'//, ' EXIT INFR')

   END SUBROUTINE INFR



   !SSSSSS SUBROUTINE INPL
   SUBROUTINE INPL
   !----------------------------------------------------------------------*
   !           Initialisation subroutine for contaminant plant uptake
   !----------------------------------------------------------------------*

      ! Assumed global variables provided via host module(s):
      ! NPLT, NV, pmass, pf2max, pkmax, total_no_links, total_no_elements
      ! NPLTYP, NVC, PFONE, PLAI, NPL, NCETOP, PDZF3, RDF, DELONE
      ! CLAI, GMCBBO
      USE PLANT_CC
      USE COLM_C1

      IMPLICIT NONE

      ! Locals
      INTEGER :: NCL, JPLANT, JPLTY, NCE, NDUM
      DOUBLE PRECISION :: D1DUM, RDUM

   !----------------------------------------------------------------------*

      NPLT = NV
      ! Number of top cell in column, and number of plant types

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ gp 30/3/93
      pmass(1) = TWO
      pmass(2) = 3.0D0
      pmass(3) = 20.0D0

      pf2max(1) = TWO
      pf2max(2) = 6.0D0
      pf2max(3) = 10.0D0

      pkmax(1, 1) = 1.5D-8
      pkmax(2, 1) = 3.0D-8
      pkmax(3, 1) = 3.0D-8
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ temp. for dsatd2

      column_loop: DO NCL = total_no_links + 1, total_no_elements

         NPLTYP(NCL, 1) = NVC(NCL)
         PFONE(NCL, 1)  = PLAI(NPLTYP(NCL, 1))

         IF (PFONE(NCL, 1) >= 0.99D0) THEN
            NPL(NCL) = 1
         ELSE
            PFONE(NCL, 2) = ONE - PFONE(NCL, 1)
            NPL(NCL) = 2
         END IF

         ! ^^^^^^^^^^^^^^^ TEMPORARY
         ! Set number of plant types on each column
         ! Temporarily, only two plant types are allowed on each
         ! column and the total PLAI is one
         ! Second plant type number is set in BLOCK DATA

         plant_loop: DO JPLANT = 1, NPL(NCL)

            JPLTY = NPLTYP(NCL, JPLANT)
            ! Plant type number

            root_density_loop: DO NCE = NCETOP, 2, -1
               NDUM = NCETOP - NCE + 1
               PDZF3(NCL, NCE, JPLANT) = RDF(JPLTY, NDUM)
            END DO root_density_loop
            ! Set root density function

            D1DUM = DELONE(JPLTY)
            RDUM  = CLAI(JPLTY) / PF2MAX(JPLTY)

            GMCBBO(NCL, JPLANT) = RDUM * D1DUM
            ! Initialise old value for mass in compartment b

         END DO plant_loop

      END DO column_loop

   END SUBROUTINE INPL



   !SSSSSS SUBROUTINE INSM
   SUBROUTINE INSM
   !----------------------------------------------------------------------*
   !  THIS SUBROUTINE READS IN THE PARAMETERS REQUIRED FOR THE
   !  SNOWMELT COMPONENT AND CARRIES OUT INITIALISATION
   !  CALCULATIONS.
   !----------------------------------------------------------------------*
   !  VARIABLE AND UNIT SPECIFICATION
   !  UNIFSD- SNOWDEPTH IF UNIFORM (MM OF SNOW)           MM
   !  SD    - SNOWDEPTH (MM OF SNOW)                      MM
   !  DDF   - DEGREE DAY FACTOR                           MM/S/C
   !  RHOS  - SPECIFIC GRAVITY OF SNOW                    --
   !  TSIN  - INITIAL TEMPERATURE OF SNOW                 C
   !  TS    - TEMPERATURE OF SNOW                         C
   !  NSMC  - COUNTER USED IN ROUTING MELTWATER
   !          THROUGH SNOWPACK. EQUALS NUMBER OF
   !          SLUGS OF MELTWATER MOVING THROUGH SNOWPACK  --
   !  MSM   - EQUALS 1 FOR DEGREE DAY
   !                 2 FOR ENERGY BUDGET                  --
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! SMD, BINSMP, PPPRI, HEAD, DDF, RHOS, NSD, MSM, RHODEF
      ! ZOS, ZDS, ZUS, NM, IMET, ngdbgn, total_no_elements, rhosar
      ! SD, NSMC, TS, SF

      IMPLICIT NONE

      ! Locals
      INTEGER :: N, IEL, I
      DOUBLE PRECISION :: TSIN, UNIFSD

   !----------------------------------------------------------------------*

      ! READ PRINT CONTROL PARAMETERS
      READ(SMD, '(20A4)') HEAD
      READ(SMD, '(L7)') BINSMP
      IF (BINSMP) WRITE(PPPRI, '(///1X, 20A4)') HEAD

      ! READ SNOWMELT DATA
      READ(SMD, '(20A4)') HEAD
      READ(SMD, '(2F7.5,F7.2,2I7)') DDF, RHOS, TSIN, NSD, MSM
      RHODEF = RHOS

      ! Added by spa, 05/11/92.  Snowpack temp no longer needed
      ! for degree day method.  Therefore if msm=1, tsin=0.
      IF (MSM == 1) TSIN = ZERO

      IF (BINSMP) WRITE(PPPRI, 801) DDF, RHOS, TSIN, MSM

      ! Execute Energy Budget specific reads if MSM > 1
      IF (MSM /= 1) THEN
         ! READ ENERGY BUDGET DATA
         READ(SMD, '(20A4)') HEAD
         READ(SMD, '(3F7.5)') ZOS, ZDS, ZUS

         IF (BINSMP) WRITE(PPPRI, 803) ZOS, ZDS, ZUS

         ! METEOROLOGICAL (WINDSPEED) DATA LOCATION
         READ(SMD, '(20A4)') HEAD
         READ(SMD, '(10I7)') (IMET(N), N = 1, NM)

         IF (BINSMP) THEN
            WRITE(PPPRI, 715)
            station_loop: DO N = 1, NM
               WRITE(PPPRI, '(3X, I4, 10X, I4)') N, IMET(N)
            END DO station_loop
         END IF
      END IF

      ! IS SNOWDEPTH UNIFORM?
      IF (NSD == 0) THEN
         uniform_rho_loop: DO IEL = ngdbgn, total_no_elements
            rhosar(IEL) = RHODEF
         END DO uniform_rho_loop

         ! UNIFORM SNOWDEPTH (MM OF SNOW)
         READ(SMD, '(20A4)') HEAD
         READ(SMD, '(F7.1)') UNIFSD

         uniform_sd_loop: DO IEL = ngdbgn, total_no_elements
            SD(IEL) = UNIFSD
         END DO uniform_sd_loop

         IF (BINSMP) WRITE(PPPRI, '(/, 1X, "INITIAL SNOWPACK HAS UNIFORM THICKNESS =", F7.1, 1X, "MM")') UNIFSD
      ELSE
         ! NONUNIFORM SNOWDEPTH (MM OF SNOW)
         I = 0
         IF (BINSMP) I = 1
         CALL AREADR(SD, I, SMD, PPPRI)
         CALL AREADR(rhosar, I, SMD, PPPRI)
      END IF

      ! Epilogue Element Processing
      epilogue_loop: DO IEL = ngdbgn, total_no_elements
         ! SET COUNTER FOR SNOWMELT ROUTINE
         NSMC(IEL) = 0
         ! SET SNOW TEMPERATURES
         TS(IEL) = TSIN
         ! SET SNOWFALL
         SF(IEL) = ZERO
      END DO epilogue_loop

   !----------------------------------------------------------------------*
   ! FORMAT STATEMENTS
   !----------------------------------------------------------------------*

801   FORMAT(/, 'DEGREE DAY FACTOR DDF =', F7.5, 1X, 'MM/S/C', &
             5X, 'SNOW SPECIFIC GRAVITY RHOS =', F7.5 / &
             5X, 'INITIAL SNOW TEMPERATURE =', F7.2, 1X, 'C' / &
             5X, 'SNOWMELT CALCULATED BY DEGREE DAY IF MSM IS 1', &
             ' AND BY ENERGY BUDGET IF MSM IS 2', 5X, 'MSM =', I3)

803   FORMAT(/, 'ENERGY BUDGET DATA', 3X, 'ROUGHNESS ZOS =', F7.5, 1X, 'M' / &
             21X, 'ZERO PLANE DISPLACEMENT ZDS =', F7.5, 1X, 'M' / &
             21X, 'HEIGHT OF ANEMOMETER ZUS =', F7.5, 1X, 'M')

715   FORMAT(/' LOCATION OF MET. STATIONS: ' / &
              ' STATION NO.   ELEMENT NO.')

   END SUBROUTINE INSM



   !SSSSSS SUBROUTINE DINET
   SUBROUTINE DINET
   !----------------------------------------------------------------------*
   ! DUMMY COMPONENT INITIALISATION (ET)
   !----------------------------------------------------------------------*

      ! Assumed global variable provided via host module:
      ! BMETAL

      IMPLICIT NONE

   !----------------------------------------------------------------------*

      WRITE(*, '(/, /, "ENTER DINET")')
      BMETAL = .TRUE.

      ! PNET=0.0003
      ! PE=0.0
      ! EINT=0.0
      ! ERZ=0.0
      ! DRAIN=0.0
      ! ESOIL=0.0

   END SUBROUTINE DINET



   !SSSSSS SUBROUTINE DINOC
   SUBROUTINE DINOC
   !----------------------------------------------------------------------*
   ! DUMMY COMPONENT INITIALISATION (OC)
   !----------------------------------------------------------------------*

      IMPLICIT NONE

   !----------------------------------------------------------------------*

      WRITE(*, '(/, /, "ENTER DINOC")')

   END SUBROUTINE DINOC



   !SSSSSS SUBROUTINE DOCIN
   PURE SUBROUTINE DOCIN
   !----------------------------------------------------------------------*
   ! DUMMY COMPONENT (OC)
   !----------------------------------------------------------------------*

      IMPLICIT NONE

   END SUBROUTINE DOCIN



   !SSSSSS SUBROUTINE MUERR2
   SUBROUTINE MUERR2(CPR, total_no_elements, NELEE, total_no_links, MAX_NUM_CATEGORY_TYPES, &
                     MAX_NUM_DATA_PAIRS, NCON, NCONEE, NUM_CATEGORIES_TYPES, NTAB, NCATTY,  &
                     ISCNSV, TABLE_CONCENTRATION, TABLE_WATER_DEPTH, LDUM)
   !--------------------------------------------------------------------*
   ! Checks data that is used to calculate the spatially variable
   ! contaminant concentrations for grid and bank elements
   !--------------------------------------------------------------------*
   ! Version: 4.2                 Notes:
   ! Module: CM                 Program: SHETRAN
   ! Modifications
   ! Notes: The checking works. However, it is done in a poor way.
   ! In future this should be changed
   !--------------------------------------------------------------------*

      ! Assumed global variables provided via host module(s):
      ! EEERR, FFFATAL

      IMPLICIT NONE

      ! --- Dummy Arguments ---
      INTEGER, INTENT(IN) :: CPR, total_no_elements, NELEE, total_no_links
      INTEGER, INTENT(IN) :: MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS
      INTEGER, INTENT(IN) :: NCON, NCONEE

      INTEGER, INTENT(IN) :: NUM_CATEGORIES_TYPES(NCONEE)
      INTEGER, INTENT(IN) :: NTAB(MAX_NUM_CATEGORY_TYPES, NCONEE)
      INTEGER, INTENT(INOUT) :: NCATTY(NELEE, NCONEE)
      LOGICAL, INTENT(IN) :: ISCNSV(NCONEE)

      DOUBLE PRECISION, INTENT(INOUT) :: TABLE_CONCENTRATION(MAX_NUM_CATEGORY_TYPES, &
                                  MAX_NUM_DATA_PAIRS, NCONEE)
      DOUBLE PRECISION, INTENT(INOUT) :: TABLE_WATER_DEPTH(MAX_NUM_CATEGORY_TYPES, &
                                   MAX_NUM_DATA_PAIRS, NCONEE)

      LOGICAL, INTENT(INOUT) :: LDUM(1)  !! Workspace/Flag

      ! --- Local Variables ---
      INTEGER :: ICOL1, NERR, NELMTY, NTBL, I, J
      INTEGER :: IUNDEF            !! Standard undefined integer flag
      DOUBLE PRECISION :: PREVDP    !! Previous depth for monotonicity check

      ! Constant arrays required by ALCHKI/ALCHK interfaces
      INTEGER :: IZERO(1)
      DOUBLE PRECISION :: ZERO1(1)


   !--------------------------------------------------------------------*

      ! 0. Preliminaries
      ! --- Data Initialisation ---
      IZERO  = (/ 0 /)
      ! ZERO1  = (/ 0.0D0 /)
      ! IUNDEF = -999
      NERR   = 0
      ICOL1  = total_no_links + 1

      ! 1. Check the data used to calculate the spatially variable
      ! contaminant concentrations
      ! -------------------------------------------------------

      contam_loop: DO I = 1, NCON

         IF (ISCNSV(I)) THEN

            ! *NCATTY
            ncatty_loop: DO J = ICOL1, total_no_elements
               CALL ALCHKI(EEERR, 2103, CPR, J, J, IUNDEF, IUNDEF, &
                           'NCATTY(iel)', 'GT', IZERO, NCATTY(J:J, I), NERR, LDUM(1:1))
            END DO ncatty_loop

            ! *TABLE_WATER_DEPTH
            ! The table of depths must have a first depth equal to zero,
            ! thereafter the depth must increase
            category_loop1: DO NELMTY = 1, NUM_CATEGORIES_TYPES(I)

               CALL ALCHK(EEERR, 2104, CPR, NELMTY, NELMTY, 1, IUNDEF, &
                          'TABLE_WATER_DEPTH[NUM_CATEGORIES_TYPES,1]', 'EQ', ZERO1, ZERO, &
                          TABLE_WATER_DEPTH(NELMTY:NELMTY, 1, I), NERR, LDUM(1:1))

               table_depth_loop: DO NTBL = 2, NTAB(NELMTY, I)
                  PREVDP = TABLE_WATER_DEPTH(NELMTY, NTBL - 1, I)
                  CALL ALCHK(EEERR, 2105, CPR, NELMTY, NELMTY, NTBL, IUNDEF, &
                             'TABLE_WATER_DEPTH[NUM_CATEGORIES_TYPES,ntab]', 'GT', (/PREVDP/), &
                             ZERO, TABLE_WATER_DEPTH(NELMTY:NELMTY, NTBL, I), NERR, LDUM(1:1))
               END DO table_depth_loop

            END DO category_loop1

            ! *TABLE_CONCENTRATION
            ! Each value in the table of concentrations must be >= 0
            category_loop2: DO NELMTY = 1, NUM_CATEGORIES_TYPES(I)
               table_conc_loop: DO NTBL = 1, NTAB(NELMTY, I)
                  CALL ALCHK(EEERR, 2106, CPR, NELMTY, NELMTY, NTBL, IUNDEF, &
                             'TABLE_CONCENTRATION[nmne,ntab]', 'GE', ZERO1, ZERO, &
                             TABLE_CONCENTRATION(NELMTY:NELMTY, NTBL, I), NERR, LDUM(1:1))
               END DO table_conc_loop
            END DO category_loop2

         END IF

      END DO contam_loop

      ! 2. Epilogue
      ! -----------
      IF (NERR > 0) THEN
         CALL ERROR(FFFATAL, 2107, CPR, 0, 0, 'Error(s) detected while checking static/initial interface')
      END IF

   END SUBROUTINE MUERR2

END MODULE FRmod
