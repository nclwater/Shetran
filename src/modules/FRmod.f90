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
DOUBLEPRECISION :: qoctot = 0.0d0, uzold = 0.0d0, uznowt, qoctotextra(100) = 0.0d0
DOUBLEPRECISION :: PREVTM
DOUBLEPRECISION :: TIMB=zero
LOGICAL         :: FIRST_frmb=.TRUE.  
LOGICAL         :: SEDSRT=.FALSE.
DOUBLEPRECISION :: GNUCUM (NELEE), DLSSRT (NELEE)  

PRIVATE

PUBLIC :: FROPEN, FRINIT, FRSORT, FROUTPUT, FRMB, FRRESP, FRIND, FRLTL, INCM, & !REST NEEDED FOR AD ONLY
          qoctot, uzold, bsoft, tsh, tch, bstore, btime, next_hour, icounter2
CONTAINS


!SSSSSS SUBROUTINE FRDIM (BINFRP)  
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
DO 860 J = 2, NXM1  
  860 DX (J) = (DXIN (J - 1) + DXIN (J) ) * 0.5  
DY (1) = DYIN (1)  
DY (NY) = DYIN (NYM1)  
DO 870 K = 2, NYM1  
  870 DY (K) = (DYIN (K - 1) + DYIN (K) ) * 0.5  
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
  900 END DO  
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
  920       END DO  
!
   ENDIF  
!
! CALCULATE CATCHMENT AREA BY asumMING ALL BASIC GRID SIZES
! AND CATCHMENT AREA OBTAINED BY asumMING ALL ELEMENT AREAS (INCLUDES OVE
!
   IF (ITYPE.EQ.0) CATEST = CATEST + DX (IX) * DY (IY)  
!
  950 END DO  
!
! --- CALCULATE AREA OF EACH ELEMENT
!
DO 955 IEL = 1, total_no_elements  
   cellarea (IEL) = DXQQ (IEL) * DYQQ (IEL)  
   CAREA = CAREA + cellarea (IEL)  
  955 END DO  
!
! --- PRINT OUT ELEMENT AREA, TOTAL CATCHMENT AREA, AND PERCENTAGE ERROR
!
IF (BINFRP) THEN  
   WRITE(PPPRI, 1500)  
   DO 960 IEL = 1, total_no_elements  
      WRITE(PPPRI, 1600) IEL, DXQQ (IEL), DYQQ (IEL), cellarea (IEL)  
  960    END DO  
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
  980 END DO  
!
RETURN  
!
! ^^^^^^^^^^^^ FORMAT STATEMENTS
!
 1500 FORMAT(/ '   INDEX   DXQQ (M)   DYQQ (M)     AREA (M^^2)' /)  
!
 1600 FORMAT(' ',4X,I6,4X,F7.2,4X,F7.2,4X,F12.2)  
!
 1700 FORMAT(/ ' TOTAL CATCHMENT AREA = ',F12.3,' SQ. METRES. ' / &
&         ' BASIC CATCHMENT AREA = ',F12.3,' SQ. METRES. ' / &
&   ' DIFFERENCE INTRODUCED BY CHANNEL SYSTEM AND BANKS = ', &
&   F12.3,' %' /)
!
 1750 FORMAT(/ ' TOTAL CATCHMENT AREA = ',F12.3,' SQ. KM. ' / &
&         ' BASIC CATCHMENT AREA = ',F12.3,' SQ. KM. ' / &
&   ' DIFFERENCE INTRODUCED BY CHANNEL SYSTEM AND BANKS = ', &
&   F12.3,' %' /)
!

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

CHARACTER (LEN=2) :: PDIRN  
!----------------------------------------------------------------------*
!
! ^^^^^^^^^^^^ INITIALISE ARRAY AND INDEX NUMBER
!
DO 100 I = 1, NELEE  
   NGRID (I) = 0  
   NBFACE (I) = 0  
   DO 100 K = 1, 12  
      ICMREF (I, K) = 0  
  100 CONTINUE  
!
INDEX = 0  
INDEX2 = 0  
!
! ^^^^^^^^^^^^ SET UP INDEX NUMBERS
!
! --- CHANNEL LINKS
!
DO 50 J = 1, NY  
!
   DO 30 I = 1, NX  
      IF (LCODEY (I, J) .GE.4) THEN  
         INDEX = INDEX + 1  
         ICMREF (INDEX, 1) = 3  
         ICMREF (INDEX, 2) = I  
         ICMREF (INDEX, 3) = J  
         ICMREF (INDEX, 4) = INDEX  
         LINKNS (INDEX) = .FALSE.  
      ENDIF  
   30    END DO  
!
   DO 40 I = 1, NX  
      IF (LCODEX (I, J) .GE.4) THEN  
         INDEX = INDEX + 1  
         ICMREF (INDEX, 1) = 3  
         ICMREF (INDEX, 2) = I  
         ICMREF (INDEX, 3) = J  
         ICMREF (INDEX, 4) = INDEX  
         LINKNS (INDEX) = .TRUE.  
      ENDIF  
   40    END DO  
!
   50 END DO  
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
  220       END DO  
  230    END DO  
!
ENDIF  
!
! --- GRID CODES
!
  250 DO 300 J = 1, NY  
   DO 300 I = 1, NX  
      IF (INGRID (I, J) .GE.0) THEN  
         INDEX = INDEX + 1  
         ICMREF (INDEX, 2) = I  
         ICMREF (INDEX, 3) = J  
         ICMXY (I, J) = INDEX  
      ENDIF  
  300 CONTINUE  
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
  600 END DO  
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
         DO 630 J = 1, 4  
            IF (ICMREF (INEXT1, J + 4) .EQ.INDEX) THEN  
               ICMREF (INDEX, I + 8) = J  
               GOTO 650  
            ENDIF  
  630          END DO  
         WRITE(PPPRI, 1100) INDEX, I  
         ICOUNT = ICOUNT + 1  
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
               DO 635 J = 1, 4  
                  JNEXT1 = ICMREF (IN1, J + 4)  
                  IF (JNEXT1.LT.0) THEN  
                     DO 632 J2 = 1, 3  
                        JN2 = ICMRF2 ( - JNEXT1, J2)  
                        IF (JN2.EQ.INDEX) THEN  
                           ICMRF2 ( - INEXT1, J1 + 3) = J  
                           GOTO 640  
                        ENDIF  
  632                      END DO  
                  ENDIF  
  635                END DO  
               WRITE(PPPRI, 1100) INDEX, I  
               ICOUNT = ICOUNT + 1  
            ENDIF  
  640          END DO  
      ELSE  
         ICMREF (INDEX, I + 8) = I  
!
         IF (ITYPE.LT.3.AND.NBFACE (INDEX) .EQ.0) NBFACE (INDEX) &
          = I
!
      ENDIF  
  650    END DO  
!
  700 END DO  
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
  800    END DO  
!
   IF (NEL2.GT.0) THEN  
      WRITE(PPPRI, 1500) NNODE3 / 3, NNODE4 / 4, NEL2  
      DO 900 INDEX2 = 1, NEL2  
         WRITE(PPPRI, 1600) INDEX2, (ICMRF2 (INDEX2, I), I = 1, 3)  
  900       END DO  
   ENDIF  
!
ENDIF  
!
! FORMAT STATEMENTS
!
 1100 FORMAT(' INCONSISTENCY FOUND AT INDEX:',I4,' FACE:',I2)  
!
 1200 FORMAT(/  I4,' INCONSISTENCIES FOUND IN INDEX ARRAY' /)  
!
 1300 FORMAT(' ', / 'INDEX ARRAY: NO. OF ELEMENTS = ',I6, // &
&       ' ','     INDEX      TYPE         X         Y      LINK   ', &
&       '  FACE1     FACE2     FACE3     FACE4' / &
&       ' ','     -----      ----         -         -      ----   ', &
&       '  -----     -----     -----     -----' )
!
 1400 FORMAT(' ',5(4X,I6),1X,A2,1X,I6,3(4X,I6))  
!
 1500 FORMAT(' '/'AUXILIARY INDEX ARRAY FOR CHANNEL NODES: ',/ &
&           'NO. OF NODES WITH 3 BRANCHES = ',I4,/ &
&           'NO. OF NODES WITH 4 BRANCHES = ',I4,/ &
&           'TOTAL NO. OF INDICES         = ',I4 // &
&       ' ','   INDEX  LINK 1  LINK 2  LINK 3' / &
&       ' ','   -----  ------  ------  ------' )
!
 1600 FORMAT(' ',5(4X,I4))  
!
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
!  GP          3.4  Set CMT,CMP,CMB.  Special treatment for DBOT(LL).
!                   Replace L1 with L1+1 in assignment of NLYRBT,ZLYRBT.
!                   Print table of soil horizon depths.
!                   Hot-start: initialize HOTIME; read HOT file;
!                   set TIMEUZ,BHOTTI; call FRRESP.
! RAH  941005 3.4.1 Bring IMPLICIT from AL.P.  Pass UZNOW to FRRESP.
!                   Call ERROR if NLYRBT can't be set.
!  GP  960724  4.0  New module VSS replaces UZ,SZ,EX ...
!                   Reassign unit nos; replace UZD,SZD,AQD,SZB,HBD with
!                   VSD,VSI,LFB,LHB,LGB,BFB,BHB; scrap CPR (keep CMP).
!                   Call VSIN, instead of INUZ,INSZ,INUZ2 (or DINUZ,
!                   DINSZ), and after INBK, not before.
!                   Move adjustment of ZLYRBT & setting of
!                   NLYRBT,NTSOIL,NHBED,FHBED to VSREAD,VSCONC.
!                   Scrap ZBED (see INCM), RSZ,QUZR,QSZR (see SHETRN)
!                   and EPSZA,QSZOC,QSZO,WSI.  Add FRRESP arg .FALSE..
!                   Close FR,WAT input (except boundary data) files.
!                   Remove PSI3,PSI33,HSZ,EPS,QSZUZ,WATC3 from HOT read.
! RAH  970223  4.1  Explicit typing.  Scrap BUZCAL,BSZCAL,BOCCAL,BEXCAL,
!                   DTSTOC,DTSTSZ,DTSTUZ,EXNEXT,EXNOW,EXVAL,OCUNT,OCVAL,
!                   PNETOC,POC,SZNOW,SZUNT,SZVAL,TSTOOC,TSTOSZ,TSTOUZ,
!                   UZUNT,WSUZI,WSSZI,WSOCI,WOCLI (AL.D), FINC,NUMBER.
!      970524       AIOSTO size 20 (was 50); & use DATA.
!      970525       Pass BINFRP to INRES.
!      970530       (Amend DATA statement.)
! RAH  980317  4.2  (Amend specification comments below.)
! SB   010307  4g-pc changed data statement for AIOSTO
!----------------------------------------------------------------------*
! Commons and constants
 
! Imported constants
!     SPEC.AL          NELEE
! Input common
!     SPEC.AL:         LL,NEL,NGDBGN,NLF,ICMREF(NELEE,8)
!                      UZNEXT,CSTORE(NEL)
!                      BEXBK,BEXET,BEXOC,BEXSM
!     SPEC.FR:         DTAO
!                      BHOTRD,BINFRP,BSTORE
! In|out common
!     SPEC.AL:         NMC(NEL),NRAINC(NEL)
! Output common
!     SPEC.AL:         BFB,BHB,BKD,BUG,CMB,CMD,CMP,CMT,EPD,ETD,FRD,HOT
!                      LFB,LGB,LHB,MED,NXE,NYE,OCD,OFB,OHB,PPD,PRD,PRI
!                      RES,SMD,SPR,SYD,TIM,VED,VSD,VSI,WLD
!                      ERR,FATAL,WARN,MSM
!                      BHOTTI,HOTIME,OCNOW,TIMEUZ,UZNEXT,UZNOW,
!     SPEC.FR:         ALLOUT


! Locals, etc
INTEGER :: IEL, IFACE, JEL, K
DOUBLEPRECISION :: rdd(nelee), rddq(nelee,4)
CHARACTER (LEN=20) :: AIOSTO  
character*10 :: atemp


DATA AIOSTO / '11111111111111111111' /  
!----------------------------------------------------------------------*
!NXE = NXEE  
!NYE = NYEE  
!
! file unit numbers
!
!FRD = 10  
!VSD = 11  
!OCD = 12  
!ETD = 13  
!PPD = 14  
!SMD = 15  
!BKD = 16  
!SYD = 17  
!CMD = 18  
!MED = 19  
!PRD = 20  
!EPD = 21  
!TIM = 22  
!PRI = 23  
!SPR = 24  
!CMP = 25  
!BUG = 26  
!RES = 27  
!HOT = 28  
!VSI = 29  
!VED = 30  
!WLD = 31  
!LFB = 32  
!LHB = 33  
!LGB = 34  
!BFB = 35  
!BHB = 36  
!OFB = 37  
!OHB = 38  
!CMT = 39  
!CMB = 40  
!DIS = 41  
!VSE = 42  
!MAS = 43  
!
!FATAL = 1  
!ERR = 2  
!WARN = 3  
!
! OPEN I/O DATA FILES
!
! CALL FROPEN  !moded to main routine
!
!     CALL INITIALIZATION SUBROUTINES.
!        NOTE: THE ORDER IN WHICH THE SUBROUTINES ARE CALLED IS
!              IMPORTANT.
!
CALL INFR  
!
IF (BEXET) CALL INET  
IF (.NOT.BEXET) CALL DINET  
!
IF (BEXSM) CALL INSM  
IF (.NOT.BEXSM) MSM = 0  
!
IF (BEXOC) CALL OCINI()
IF (.NOT.BEXOC) CALL DINOC
!
! CALCULATE ELEMENT DIMENSIONS AND AREAS
! (MUST BE CALLED AFTER OCINI AND BEFORE VSIN)
!
CALL FRDIM (BINFRP)  
!
IF (BEXBK) CALL INBK  
!
CALL VSIN  
!
! SET UP MET. & RAINFALL STATIONS FOR THE CHANNEL (= ADJACENT BANK/GRID)
!
DO 30 IEL = 1, total_no_links  
   NMC (IEL) = 1  
   NRAINC (IEL) = 1  
   DO 20 IFACE = 1, 4  
      JEL = ICMREF (IEL, 4 + IFACE)  
      IF (JEL.GT.0) THEN  
         IF (ICMREF (JEL, 1) .NE.3.AND.NMC (JEL) .GT.0.AND.NRAINC &
          (JEL) .GT.0) THEN
            NMC (IEL) = NMC (JEL)  
            NRAINC (IEL) = NRAINC (JEL)  
            GOTO 30  
         ENDIF  
      ENDIF  
   20    END DO  

   30 END DO  
!
!***ZQ Module 200520
if (iszq) call ReadZQTable

! close data input file units
REWIND(FRD) !!CLOSE (FRD)    !AD
REWIND(VSD) !!CLOSE (VSD)    !AD  
REWIND(OCD) !!CLOSE (OCD)    !AD
REWIND(ETD) !!CLOSE (ETD)    !AD
REWIND(SMD) !!CLOSE (SMD)    !AD
REWIND(BKD) !!CLOSE (BKD)    !AD
REWIND(VSI) !!CLOSE (VSI)    !AD 
!
! CALL RES FILE INPUT ROUTINE, IF REQUIRED
!
!      IF (BSTORE) CALL INRES(BINFRP)
REWIND(PPD) !!CLOSE (PPD)    !AD
!
!     UPDATE HOTSTART TIME AND READ FROM FILE IF BHOTRD = TRUE
!
HOTIME = zero  
!
IF (BHOTRD) THEN  
!

115    READ (HOT, *, END = 120) atemp, HOTIME, UZNEXT, top_cell_no,atemp, (CSTORE (IEL), &
    IEL = NGDBGN, total_no_elements),atemp, (rdd(IEL), IEL = 1, total_no_elements),atemp, ( (rddq (IEL, K), &
    IEL = 1, total_no_elements), K = 1, 4),atemp, ( (QOC (IEL, K), IEL = 1, total_no_elements), &
    K = 1, 4),atemp, ( (DQ0ST (IEL, K), IEL = 1, total_no_elements), K = 1, 4), &
    atemp,( (DQIST (IEL, K), IEL = 1, total_no_elements), K = 1, 4),atemp, ( (DQIST2 (IEL, K) &
    , IEL = 1, NGDBGN - 1), K = 1, 3),atemp, (SD (IEL), IEL = NGDBGN, &
    total_no_elements),atemp, (TS (IEL), IEL = NGDBGN, total_no_elements),atemp, (NSMC (IEL), IEL = NGDBGN, &
    total_no_elements),atemp, ( (SMELT (K, IEL), K = 1, NSMC (IEL) ), IEL = NGDBGN, &
    total_no_elements),atemp, ( (tmelt(K, IEL), K = 1, NSMC (IEL) ), IEL = NGDBGN, total_no_elements), &
    atemp, ( (VSPSI (k, iel), k = 1, top_cell_no), IEL = 1, total_no_elements)
    DO iel=1,total_no_elements
        CALL SETHRF(iel,rdd(iel))
        DO k=1,4
            CALL SETQSA(iel,k,rddq(iel,K))
        ENDDO
    ENDDO
   IF (HOTIME.GE.BHOTTI) GOTO 125  
   GOTO 115  
!
  120    WRITE(PPPRI, 122)  
  122 FORMAT  ( / ' WARNING: END OF HOTSTART FILE REACHED')  
!
  125    WRITE(PPPRI, 127) HOTIME  
  127 FORMAT  (// ' ^^^ HOTSTART OF SIMULATION AT TIME ',F10.2,' ^^^' /)  
!
!
   ALLOUT = HOTIME+DTAO  
   UZNOW = HOTIME  
   OCNOW = HOTIME  
   UZVAL = UZNOW + UZNEXT  
   TIMEUZ = HOTIME  
   BHOTTI = HOTIME  
!
! --- WRITE SET OF DATA TO RES FILES AT HOTSTART TIME
!
   CALL FRRESP (AIOSTO, UZNOW, .FALSE.)  
!
ENDIF  
!
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
! Input arguments
INTEGER :: NNX, NNY, NXE, NYE, INF, IOF  
LOGICAL :: BPCNTL  
!
! Output arguments
INTEGER :: IARR (NXE, NYE)  
!
! Locals, etc
INTEGER :: I, J, K, L, M  
!CHARACTER (LEN=80) :: TITLE  
CHARACTER (LEN=1) :: NMERIC (9), A1LINE (200)  
DATA NMERIC / '1', '2', '3', '4', '5', '6', '7', '8', '9' /  
!
!----------------------------------------------------------------------*
!
READ (INF, 10) TITLE  
   10 FORMAT (A80)  
IF (BPCNTL) WRITE (IOF, 20) TITLE  
   20 FORMAT (A80)  
!
DO 30 J = 1, NNY  
   DO 30 I = 1, NNX  
   30 IARR (I, J) = 0  
!
I = NNY  
DO 40 J = 1, NNY  
   READ (INF, 50) K, (A1LINE (L), L = 1, NNX)  
   50 FORMAT   (I7, 1X, 500A1)  
   IF (BPCNTL) WRITE (IOF, 50) K, (A1LINE (L), L = 1, NNX)  
!
   IF (K.NE.I) GOTO 100  
   I = I - 1  
!
   DO 70 L = 1, NNX  
      DO 60 M = 1, 9  
         IF (A1LINE (L) .EQ.NMERIC (M) ) THEN  
            IARR (L, K) = M  
            GOTO 70  
         ENDIF  
   60       END DO  
   70    END DO  
!
   40 END DO  
RETURN  
!
  100 IF (BPCNTL) WRITE (IOF, 110)  
  110 FORMAT ('  ^^^   INCORRECT COORDINATE')  
STOP  
END SUBROUTINE FRLTL





!SSSSSS SUBROUTINE FRMB  
SUBROUTINE FRMB  
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
!      dimensions of size NLFEE:  for link  in           1:NLF
! DELTAZ(cell,e), VSTHE(cell,e):  for cell  in NLYRBT(e,1):LL
!                      P(ipstn):  for ipstn in    NRAINC(1:NEL)
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
INTEGER :: MBHOUR, MBMIN  
DOUBLEPRECISION MPMM
PARAMETER (MBHOUR = 0, MBMIN = 0, MPMM = 1D-3)  
INTEGER :: MONEND (12), IEL, IPSTN, ICBOTM, IL, I, ICL, LYEAR  
DOUBLEPRECISION AT, QBK, AREAE, AREAEM  
DOUBLEPRECISION PRECM, CEVAPM, SEVAPM, TRANSM, AQFLXM, DISCHM, &
 BFLOW
CHARACTER (LEN=50) :: AIOSTO  
DATA MONEND / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /  
LOGICAL  :: r
! Water flow mass bal variables (BALANC) are (time integrals of):
! 1     precipitation
! 2     canopy evaporation
! 3     evaporation from soil or surface water
! 4     transpiration
! 5     regional aquifer upflow (flow through the model base)
! 6     outlet discharge
! 7-12  cumulative totals for variables 1-6
! 13    storage in canopy
! 14       "    in snowpack
! 15       "    in subsurface
! 16       "    in surface water
! 17       "    in channels
! 18    aquifer-channel flow (through channel bed and sides)
! 19    cumulative aquifer-channel flow
!----------------------------------------------------------------------*
! Initialization
IF (FIRST_frmb) CALL ALINIT (ZERO, 19, BALANC)  
FIRST_frmb = .FALSE.  
! Calculate water volumes based on flow rates
!     * variables 1-5 (and 7-11)
PRECM = ZERO  
CEVAPM = ZERO  
SEVAPM = ZERO  
TRANSM = ZERO  
AQFLXM = ZERO  
DO 100 IEL = 1, total_no_elements  
   IPSTN = NRAINC (IEL)  
   ICBOTM = NLYRBT (IEL, 1) - 1  
   AT = cellarea (IEL) * DTUZ  
   PRECM = PRECM + precip_m_per_s(iel) * AT  
   CEVAPM = CEVAPM + EINTA (IEL) * AT  
   SEVAPM = SEVAPM + EEVAP (IEL) * AT  
   TRANSM = TRANSM + ERZA (IEL) * AT  
   AQFLXM = AQFLXM + QVSV (ICBOTM, IEL) * AT  

  100 END DO  
!     * variable 6 (and 12)
DISCHM = ZERO  

IF (MBLINK.NE.0) DISCHM = ABS (QOC (MBLINK, MBFACE) * DTUZ)  
!     * variable 18 (and 19)
BFLOW = ZERO  
DO 120 IL = 1, total_no_links  
   QBK = QBKB (IL, 1) + QBKB (IL, 2) + QBKF (IL, 1) + QBKF (IL, 2)  
   BFLOW = BFLOW + QBK * DTUZ  


  120 END DO  
! Update BALANC (note: elements 1:6 & 18 may be reset to zero below)
DO 150 I = 0, 6, 6  
   BALANC (I + 1) = BALANC (I + 1) + PRECM  
   BALANC (I + 2) = BALANC (I + 2) + CEVAPM  
   BALANC (I + 3) = BALANC (I + 3) + SEVAPM  
   BALANC (I + 4) = BALANC (I + 4) + TRANSM  
   BALANC (I + 5) = BALANC (I + 5) + AQFLXM  
   BALANC (I + 6) = BALANC (I + 6) + DISCHM  
   BALANC (18 + I / 6) = BALANC (18 + I / 6) + BFLOW  



  150 END DO  
! -------------- Proceed only if output is required now -------------- *


IF (UZNOW.LT.TIMB) RETURN  
!                        !!!!!!
! Calculate water volumes based on storage
CALL ALINIT (ZERO, 5, BALANC (13) )  
DO 215 IEL = total_no_links + 1,total_no_elements  
   AREAE = cellarea (IEL)  
   AREAEM = AREAE * MPMM  
   BALANC (13) = BALANC (13) + CSTORE (IEL) * AREAEM  
   BALANC (14) = BALANC (14) + SD (IEL) * RHOSAR (IEL) * AREAEM  
   BALANC (16) = BALANC (16) + (GETHRF (IEL) - ZGRUND (IEL) ) &
    * AREAE
   DO 210 ICL = NLYRBT (IEL, 1), top_cell_no  
      BALANC (15) = BALANC (15) + VSTHE (ICL, IEL) * DELTAZ (ICL, &
       IEL) * AREAE
  210    END DO  
  215 END DO  
DO 220 IL = 1, total_no_links  
   BALANC (17) = BALANC (17) + ARXL (IL) * CLENTH (IL)  


  220 END DO  
! Output the data
AIOSTO (:49) = ' '  
AIOSTO (50:) = '1'  


CALL FRRESP (AIOSTO, UZNOW, .TRUE.)  
! Calculate the next output time

IF (MBFLAG.EQ.1) THEN  
!         * next day
   LYEAR = 0  
   
   
   IF(MOD(mbyear,4)==0) THEN
    IF(MOD(mbyear,100)==0) THEN
        r = MOD(mbyear,400)==0
    ELSE
        r = .TRUE.
    ENDIF
   ELSE
    r = .FALSE.
   ENDIF
   
   
   IF (r.AND.MBMON.EQ.2) LYEAR = 1  
   MBDAY = MOD (MBDAY, MONEND (MBMON) + LYEAR) + 1  
ELSE  
!         * next month
   MBDAY = 1  
ENDIF  
IF (MBDAY.EQ.1) THEN  
   MBMON = MOD (MBMON, 12) + 1  
   IF (MBMON.EQ.1) MBYEAR = MBYEAR + 1  

ENDIF  


TIMB = HOUR_FROM_DATE(MBYEAR, MBMON, MBDAY, MBHOUR, MBMIN) - TIH  
! Initialise all short period flow data
CALL ALINIT (ZERO, 6, BALANC)  

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
  
INTEGER :: i, io  
CHARACTER (LEN=200) :: FILNAM2
!----------------------------------------------------------------------*
!
BTIME = .FALSE.  
!
! WRITE BANNER HEADER TO SCREEN
!
!IF (BDEVER) THEN  
!   WRITE ( *, 16) SHEVER  
!ELSE  
!   WRITE ( *, 15) SHEVER  
!ENDIF  
   16 FORMAT (// 'SHETRAN', F4.1 , ' ' )  
   15 FORMAT (// 'SHETRAN', F4.1 )  
!
WRITE ( *, * ) BANNER  
Write (*,*) '***************************************************************'


WRITE ( *, * )  
!     OPEN FILE RUNDATA TO OBTAIN FILENAMES:
!        WRITE(*,*)
!        WRITE(*,'(A)') ' ENTER CATCHMENT DIRECTORY NAME'
!        READ (*,FMT='(A)') dirqq  !e.g. 'E:\sv4\cobres\'
!               io = 1 ; io1=0
!      DO WHILE(io/=0 .AND. io1<5)
!          io1 = io1 + 1!
!          IF(io1>1) THEN
!              PRINT*, 'failed to open file '//TRIM(filnam)
!              PRINT*, 'press enter to try again'
!              READ(*,*)
!          ENDIF
!          CALL GET_DIR_AND_CATCH(RUNFIL, FILNAM, CNAM, DIRQQ)
!!      WRITE(*,'(A)') ' Enter catchment name: '  !e.g. 'excobpine'
!!      READ(*,1000) CNAM
!          FILNAM = TRIM(DIRQQ)//RUNFIL//CNAM

! ****sb 161213
ista=.true.
! ****sb 220415
isextradis=.true.
!***ZQ Module 200520
iszq=.true.
!sb 110324
isextrapsl=.true.

OPEN (2, FILE = FILNAM, STATUS = 'OLD', IOSTAT = io)  
!      ENDDO
filnam2=TRIM (DIRQQ) //'output_'//trim(cnam)//'_log.txt'
OPEN (53, FILE = FILNAM2, ERR = 400)  



READ (2, 1000, ERR = 300) FILNAM  



!***ZQ Module 200520 change log file to unit 52 and read DO 100 I = 10, 51 (was 50)
!***extra psl 110324 change log file to unit 53 and read DO 100 I = 10, 52 (was 50). see extra lines at the end
WRITE (53, 1000) FILNAM  
WRITE ( 53, * )  
DO 100 I = 10, 50  
   READ (2, 1000, END = 200) FILNAM  
   WRITE ( 53, 1000) FILNAM  
   READ (2, 1000, END = 200) FILNAM  
   IF (FILNAM.EQ.' '.OR.FILNAM.EQ.'0') THEN  

!***Sb 161213
      if (I.eq.45.or.i.eq.46) then 
         ista=.false.
      endif
!***Sb 220415
      if (I.eq.47) then 
         isextradis=.false.
      endif

      WRITE ( 53, 1010)  
   ELSE  
      filnam = TRIM (DIRQQ) //TRIM (FILNAM)  
      IF (I == 48) THEN  
         WRITE ( 53, 1021) I, FILNAM  
         visualisation_plan_filename = filnam  
      ELSEIF (I == 49) THEN  
         WRITE ( 53, 1021) I, FILNAM  
         visualisation_check_filename = filnam  
      ELSEIF (I == 50) THEN  
         WRITE ( 53, 1021) I, FILNAM  
         hdf5filename = filnam  
      ELSE  
         WRITE ( 53, 1020) I, FILNAM  
! make  hot file formattedsteve birkinshaw 13092017        
!        IF (I.EQ.27.OR.I.EQ.28) THEN  
!            OPEN (I, FILE = FILNAM, FORM = 'UNFORMATTED', ERR = &
!             400)
!         ELSE  
            OPEN (I, FILE = FILNAM, ERR = 400)  
!         ENDIF  
         IF (I.EQ.27) RESFIL = FILNAM  
         IF (I.EQ.22) THEN  
            BTIME = .TRUE.  
            WRITE (TIM, * ) 'Reading data sets ...'  
            REWIND (TIM)  
         ENDIF  
      ENDIF  
   ENDIF  
100 END DO  

!***ZQ Module 200520
READ (2, 1000, END = 190) FILNAM  
WRITE ( 53, 1000) FILNAM  
READ (2, 1000, END = 190) FILNAM  
IF (FILNAM.EQ.' '.OR.FILNAM.EQ.'0') THEN  
         iszq=.false.
else
    OPEN (51, FILE = FILNAM, ERR = 400) 
endif

 !extra psl 110324
READ (2, 1000, END = 195) FILNAM  
WRITE ( 53, 1000) FILNAM  
READ (2, 1000, END = 195) FILNAM  
IF (FILNAM.EQ.' '.OR.FILNAM.EQ.'0') THEN  
         isextrapsl=.false.
else
    filnam2=TRIM (DIRQQ) //filnam
    OPEN (52, FILE = FILNAM2, ERR = 400) 
endif

    
    
    
CLOSE (2)  




GOTO 900  
!

190 iszq=.false.
    isextrapsl=.false.
    goto 900
    
195 isextrapsl=.false.
    goto 900


200 IF (I.LT.14) THEN  
   WRITE ( *, 1030) CNAM  
   STOP 'ABNORMAL END'  
ENDIF  
GOTO 900  
!
  300 WRITE ( *, 1050) CNAM  
STOP 'ABNORMAL END'  
!
  400 WRITE ( *, 1040) FILNAM  
STOP 'ABNORMAL END'  
!
  900 RETURN  
!
 1000 FORMAT(A)  
!
 1010 FORMAT('- NOT USED')  
!
 1020 FORMAT('OPENING FILE UNIT ',I3,' TO FILE ',A)  
 1021 FORMAT('FILE ',I3,' IS ',A)  
!
 1030 FORMAT('UNEXPECTED -EOF- ON FILE ',A)  
!
 1040 FORMAT('ERROR OPENING FILE ',A)  
!
 1050 FORMAT('ERROR OPENING RUNDATA FILE ',A)  
!
END SUBROUTINE FROPEN



!SSSSSS SUBROUTINE FROUTPUT
SUBROUTINE FROUTPUT(SIMPOS)  
integer :: L, iface,disextrapoints,disextraelement(100),disextraface(100),pslextraelement(100),pslextrapoints,ifile
CHARACTER (LEN=20) :: disextratext,pslextratext,celem
CHARACTER (LEN=5) :: SIMPOS  
CHARACTER(256)     :: filnam
DOUBLEPRECISION qocav, qocold,qocavextra(100)
save disextrapoints,disextraelement,disextraface,pslextrapoints,pslextraelement
 
INTEGER :: nminel, i, j, iel
1000 format(i7)            !PUT HERE FOR AD PROBLEM
1100 format(10(x,f9.3))
IF (SIMPOS.EQ.'start') THEN  
    
    if (ISextradis) then 
      read(disextra,*,err=580,end=580) 
      read(disextra,*,err=580,end=580) disextratext,disextrapoints
      do i=1,disextrapoints
         read(disextra,*,err=580,end=580) disextraelement(i),disextraface(i)
!         print*,disextraelement(i),disextraface(i)
      enddo
    
    endif

!sb 110324 extra water table output    
    if (ISextrapsl) then 
      read(pslextra,*,err=581,end=581) 
      read(pslextra,*,err=581,end=581) pslextratext,pslextrapoints
      do i=1,pslextrapoints
         read(pslextra,*,err=581,end=581) pslextraelement(i)
!         print*,disextraelement(i),disextraface(i)
         ifile=80+I
         write (celem,'(I)') pslextraelement(i)
         FILNAM = TRIM (DIRQQ) //'output_WaterTable_Element'//trim(adjustl(celem))//'.txt'
         open(ifile, FILE = FILNAM, ERR = 581)
         write(ifile,'(A)') 'Time(hours), Water Table depth (m below ground)'
      enddo
    
    endif

    
    !^^^^^^ sb 08/03/06
    write (dis2, '(A58)') 'Date_dd/mm/yyyy_hours  Time(hours)  Outlet_Discharge(m3/s)'
    write (mas, '(A60)') 'Spatially Averaged Totals (mm) over the simulation'
    write (mas, '(12(a16,1a))') 'Time(Hours)', ',', &
                                 'Cum Prec.', ',', &
                                 'Cum. Can. Evap.', ',', &
                                 'Cum. Soil Evap.', ',', &
                                 'Cum. Trans', ',', &
                                 'Cum. Aqu. Flow', ',', &
                                 'Cum. Discharge', ',', &
                                 'Canopy Storage', ',', &
                                 'Snow Storage', ',', &
                                 'Subsurface Stor.', ',', &
                                 'Land Surf Stor.', ',', &
                                 'Channel Stor.'
    write (dis,'(A,f8.2,A)' ) 'discharge at the outlet - regular timestep', toutput, ' hours'
    !^^^^^ SB 25/01/05 find outlet link when no res files - mass balnce outp
    !sb 120514 oulet must be a weir
    DO L = 1, total_no_links  
        DO IFACE = 1, 4  
            IF (ICMREF (L, 4 + IFACE) .EQ.0.AND.NOCBCC (L) .GT.0) THEN  
                  IF (NOCBCD(NOCBCC(L),3).EQ.7) THEN
                  ! if boundary conditions has type7 which is a weir
                  MBLINK = L  
                  MBFACE = NOCBCD (NOCBCC (L), 2)  
                  ENDIF
            ENDIF  
        ENDDO  
    ENDDO
    
    if (ISextradis) then 
       WRITE(dis,'(A14,i6,a14,i6,100(i20,1x))') '      Outlet =',mblink,' Extra points:',(disextraelement(j),j=1,disextrapoints)
    endif
    uznowt=uznow*(1/TOUTPUT)
    next_hour = INT(uznowt) + 1.0
! sb hotstart first time is correct
    if (BHOTRD) uzold=int(bhotti/TOUTPUT)
    
ELSEIF (SIMPOS (1:4) .EQ.'main') THEN  
    !sb 02/05/07 outlet discharge sent to discharge.txt file
    !      write(492,*) uznow,qoc(mblink,mbface)
    ! asasume the average discharge over a timestep is QOC
    uznowt=uznow*(1/TOUTPUT)

    qocav     = qoc (mblink, mbface)
    if (ISextradis) then 
       do i=1,disextrapoints
          qocavextra(i)= qoc(disextraelement(i),disextraface(i))
       enddo
    endif
    hour_now  = INT(uznowt)
    
    IF(hour_now<next_hour) THEN  ! not new hour
        qoctot = qoctot + qocav*(uznowt-uzold)  
            if (ISextradis) then 
               do i=1,disextrapoints
                  qoctotextra(i) = qoctotextra(i) + qocavextra(i)*(uznowt-uzold) 
               enddo
            endif
    ELSE
        qoctot = qoctot + qocav*(next_hour-uzold)
           if (ISextradis) then 
              do i=1,disextrapoints
                 qoctotextra(i) = qoctotextra(i) + qocavextra(i)*(next_hour-uzold) 
              enddo
           endif
              
        if (ISextradis) then 
           WRITE(dis,'(100(F20.8,1x))') abs(qoctot),(abs(qoctotextra(j)),j=1,disextrapoints)
        else
           WRITE(dis,'(F20.8)') abs(qoctot)
        endif
!        CALL WRITE_DIS(mbface, qoctot)
        DO i = next_hour+1, hour_now
            next_hour = i
            if (ISextradis) then 
               WRITE(dis,'(100(F20.8,1x))') abs(qocav),(abs(qocavextra(j)),j=1,disextrapoints)
           else
              WRITE(dis,'(F20.8)') abs(qocav)
           endif
!            CALL WRITE_DIS(mbface, qocav)
        ENDDO
        !!!IF(testmax) qmmm = MAX(qmmm,ABS(qocav)) 
        qoctot    = qocav * (uznowt-next_hour)
        if (ISextradis) then 
          do i=1,disextrapoints
              qoctotextra(i) = qocavextra(i) * (uznowt-next_hour)
          enddo
        endif
          
        next_hour = next_hour + 1.0 
    ENDIF
    CALL WRITE_DIS2(mbface, qocav, uznow)
   	!write (494,'(4(f10.6))') (qsed(mblink,1,i), i=1,4)
 
      
    IF(uznow > icounter2) then  
        write (mas, '(12(f16.3,1a))') uznow, ',', &
                                      balanc (7) * 1000 / carea, ',', &
                                      balanc (8) * 1000 / carea, ',', &
                                      balanc (9) * 1000 / carea, ',', &
                                      balanc (10) * 1000 / carea, ',', &
                                      balanc (11) * 1000 / carea, ',', &
                                      balanc (12) * 1000 / carea, ',', &
                                      balanc (13) * 1000 / carea, ',', &
                                      balanc (14) * 1000 / carea, ',', &
                                      balanc (15) * 1000 / carea, ',', &
                                      balanc (16) * 1000 / carea, ',', &
                                      balanc (17) * 1000 / carea
        icounter2 = icounter2 + 24  
        do i=1,pslextrapoints
            ifile=80+I
            write(ifile,'(2(f10.2,1a))') uznow,',',zgrund(pslextraelement(i))-zvspsl (pslextraelement(i))
        enddo
   endif  
    uzold = uznowt 
    qocold = qoc (mblink, mbface)  
    ! end of sb
ELSE  
    write (vse,  * ) 'Output at end of simulation for use as', ' initial conditions in vsi file'
    write (vse, * ) 'This output is by element number'  
    write (vse, * )  
    write (vse, * ) 'phreatic surface level '  
    if (bexbk) then  
        nminel = 1  
    else  
        nminel = total_no_links + 1  
    endif  
    write (vse, 1100) (zvspsl (j), j = nminel, total_no_elements)  
    write (vse, * )  
    write (vse, * ) 'Heads at end of simulation'  
    do iel = 1, total_no_elements  
        if (bexbk.or.iel.gt.total_no_links) then  
            write (vse, 1000) iel  
            write (vse, 1100) (VSPSI (J, IEL), j = nlyrbt (iel, 1), &
            top_cell_no)
        endif  
    enddo      
ENDIF

RETURN

580  CALL ERROR(FFFATAL,1068,PPPRI,0,0,   'no or incorrect data in extra discharge points file')
581  CALL ERROR(FFFATAL,1069,PPPRI,0,0,   'no or incorrect data in water level data file')


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
WRITE(dum,'(2(I2.2,A),I4.4,3(A,I2.2))') c(3),'/',c(2),'/',c(1),'_', c(4),':',c(5),':',c(6)
WRITE(dis2,'(A,2F20.8,4X)') TRIM(dum), tme, qd 
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
  




  
! Imported constants
!     SPEC.AL          LLEE,NCLASS,NELEE,NLFEE,NVEE,NXEE
!                      RESFIL
!                      SHEVER
!     VSSOIL.INC       NSOLEE
! Input common
!     SPEC.AL          BFB,BHB,BKD,BUG,CMB,CMD,CMP,CMT,EPD,ERR,ETD
!                      FATAL,FRD,HOT,LFB,LGB,LHB,LL,MED,MSM
!                      NEL,NGDBGN,NLF,NM,NRAIN,NS,NSET,NV
!                      NX,NXE,NXEP1,NXM1,NXP1,NY,NYE,NYEP1,NYM1,NYP1
!                      OCD,OFB,OHB,PPD,PRD,PRI
!                      RES,SMD,SPR,SYD,TIM,VED,VSD,VSI,WARN,WLD
!                      ICLIST(NELEE,NCLASS),ICLNUM(NEL)
!                      ICMBK(NLFEE,2),ICMREF(NELEE,12),ICMRF2(NLFEE,6)
!                      ICMXY(NXEE,NY),INGRID(NXEE,NY)
!                      IOCORS(*),IODATA(*),IOELEM(*),IOEND(*)
!                      IOSTA(*),IOSTEP(*)
!                      JVSACN(4,LLEE,NEL),JVSDEL(4,LLEE,NEL)
!                      LCODEX(NXEE,NY),LCODEY(NXEE,NY)
!                      NBFACE(NEL),NHBED(NLFEE,2)
!                      NLYR(NEL),NLYRBT(NELEE,*),NMC(NEL)
!                      NRAINC(NEL),NRD(NV),NTSOIL(NELEE,*)
!                      NVC(NEL),NVSSPC(NEL),NVSSPT(NEL)
!                      NVSWLI(NEL),NVSWLT(NEL),NWELBT(NEL),NWELTP(NEL)
!
!                      BHOTST,BHOTTI,BWIDTH,CAREA,DTMET
!                      PALFA,PMAX,QMAX,TIH,TMAX,TTH
!                      AREA(NEL),CLENTH(NLFEE),CWIDTH(NLFEE)
!                      DELTAZ(LLEE,NEL),DHF(NELEE,4),DXQQ(NEL),DYQQ(NEL)
!                      FHBED(NLFEE,2),RDF(NVEE,LLEE),RDL(NV),VSPOR(NS)
!                      ZBEFF(NLFEE),ZBFULL(NLFEE),ZGRUND(NEL)
!                      ZLYRBT(NELEE,*),ZVSNOD(LLEE,NEL)
!
!                      BEXBK, LINKNS(NLFEE)
!
!     VSSOIL.INC       NVSSOL
!                      VSPDET(NSOLEE,NS),VSPDKR(NSOLEE,NS)
!                      VSPDTH(NSOLEE,NS),VSPETA(NSOLEE,NS)
!                      VSPKR(NSOLEE,NS),VSPPSI(NVSSOL),VSPTHE(NSOLEE,NS)
! Output common
!     SPEC.AL          IORES(*)
! Locals, etc
!INTRINSIC LEN  
INTEGER :: IDUM0  
DOUBLEPRECISION FDUM0  
LOGICAL :: LDUM0  
PARAMETER (IDUM0 = 0, FDUM0 = 0.0D0, LDUM0 = .TRUE.)  
INTEGER :: I, ICHAR, ISET, J, K, L  
CHARACTER (2) :: ANUM
CHARACTER(128) :: fname
!----------------------------------------------------------------------*
!
! WRITE SHETRAN VERSION
!1
WRITE (RES) SHEVER  
!
! ALGCB1
!2
WRITE (RES) NX, NY, NGDBGN, total_no_elements  
!
! ALGCB2
!3-4
WRITE (RES) ( (ICMREF (I, J), I = 1, total_no_elements), J = 1, 12)  
WRITE (RES) ( (ICMXY (I, J), I = 1, NX), J = 1, NY)  
!
! CFILE + DFILE (except SFB,SRB)
!5
WRITE (RES) FRD, VSD, OCD, ETD, PPD, SMD, BKD, SYD, CMD, MED, PRD, &
 EPD, TIM, PPPRI, SPR, CMP, BUG, RES, HOT, VSI, VED, WLD, LFB, LHB, &
 LGB, BFB, BHB, OFB, OHB, CMT, CMB
!
! ALCCB1
!6
WRITE (RES) top_cell_no, total_no_links, NS, NV, WWWARN, EEERR, FFFATAL  
!
! IVEG
!7
WRITE (RES) (NRD (I), I = 1, NV)  
!
! VEG
!8
WRITE (RES) ( (RDF (I, J), J = 1, NRD (I) ), I = 1, NV)  
!
! CAREA (ALDCB3 - see also below) + ALCB1A
!9
WRITE (RES) CAREA, TIH  
!
! ALCCB3
!10-11
WRITE (RES) (LINKNS (L), L = 1, total_no_links)  
WRITE (RES) BEXBK  
!
! ALCCB5
!12-27
WRITE (RES) ( (ICMBK (I, J), I = 1, total_no_links), J = 1, 2)  
WRITE (RES) ( (ICMRF2 (I, J), I = 1, total_no_links), J = 1, 6)  
WRITE (RES) ( ( (JVSACN (K, J, I), K = 1, 4), J = 1, top_cell_no), I = 1, &
 total_no_elements)
WRITE (RES) ( ( (JVSDEL (K, J, I), K = 1, 4), J = 1, top_cell_no), I = 1, &
 total_no_elements)
WRITE (RES) (NLYR (I), I = 1, total_no_elements)  
WRITE (RES) ( (NLYRBT (I, J), J = 1, NLYR (I) ), I = 1, total_no_elements)  
WRITE (RES) (NBFACE (I), I = 1, total_no_elements)  
WRITE (RES) ( (NHBED (I, J), I = 1, total_no_links), J = 1, 2)  
WRITE (RES) ( (NTSOIL (I, J), J = 1, NLYR (I) ), I = 1, total_no_elements)  
WRITE (RES) (NVC (I), I = 1, total_no_elements)  
WRITE (RES) (NVSSPC (I), I = 1, total_no_elements)  
WRITE (RES) (NVSSPT (I), I = 1, total_no_elements)  
WRITE (RES) (NVSWLI (I), I = 1, total_no_elements)  
WRITE (RES) (NVSWLT (I), I = 1, total_no_elements)  
WRITE (RES) (NWELBT (I), I = 1, total_no_elements)  
WRITE (RES) (NWELTP (I), I = 1, total_no_elements)  
!
! ALCCB7 (except THSAT)
!28-42
WRITE (RES) (cellarea (I), I = 1, total_no_elements)  
WRITE (RES) (CLENTH (I), I = 1, total_no_links)  
WRITE (RES) (CWIDTH (I), I = 1, total_no_links)  
WRITE (RES) ( (DELTAZ (J, I), J = 1, top_cell_no), I = 1, total_no_elements)  
WRITE (RES) ( (DHF (I, J), I = 1, total_no_elements), J = 1, 4)  
WRITE (RES) (DXQQ (I), I = 1, total_no_elements)  
WRITE (RES) (DYQQ (I), I = 1, total_no_elements)  
WRITE (RES) ( (FHBED (I, J), I = 1, total_no_links), J = 1, 2)  
WRITE (RES) (RDL (I), I = 1, NV)  
WRITE (RES) (VSPOR (I), I = 1, NS)  
WRITE (RES) (ZBEFF (I), I = 1, total_no_links)  
WRITE (RES) (ZBFULL (I), I = 1, total_no_links)  
WRITE (RES) (ZGRUND (I), I = 1, total_no_elements)  
WRITE (RES) ( (ZLYRBT (I, J), J = 1, NLYR (I) ), I = 1, total_no_elements)  
WRITE (RES) ( (ZVSNOD (J, I), J = 1, top_cell_no), I = 1, total_no_elements)  
!
! ALDCB1 (except MBLINK,MBFACE,MBFLAG)
!43
WRITE (RES) MSM, IDUM0, NM, NRAIN, NSET, NXP1, NYP1, NXM1, NYM1, &
 NXE, NYE, NXEP1, NYEP1
!
! ALDCB3 (except CAREA - see above)
!44
WRITE (RES) FDUM0, DTMET, QMAX, BHOTTI, BHOTST, PMAX, PALFA, TMAX, &
 BWIDTH, TTH
!
! ALDCB5
!45
WRITE (RES) BEXET, LDUM0, LDUM0, BEXOC, LDUM0, BEXSM, LDUM0, &
 BHOTPR, BHOTRD, BEXSY, BEXCM
!
! ALDCB6 (except NOCBCC, NOCBCD)
!46-59
WRITE (RES) (NMC (I), I = 1, total_no_elements)  
!      WRITE (RES) (IDUM0,I=1,total_no_elements)
WRITE (RES) ( (INGRID (I, J), I = 1, NX), J = 1, NY)  
WRITE (RES) (NRAINC (I), I = 1, total_no_elements)  
WRITE (RES) (IOCORS (I), I = 1, NSET)  
WRITE (RES) (ICLNUM (I), I = 1, NCLASS)  
WRITE (RES) ( (ICLIST (I, J), I = 1, total_no_elements), J = 1, NCLASS)  
WRITE (RES) (IODATA (I), I = 1, NSET)  
WRITE (RES) (IOELEM (I), I = 1, NSET)  
!      WRITE (RES) (IDUM0,I=1,NSET)
WRITE (RES) ( (LCODEX (I, J), I = 1, NX), J = 1, NY)  
WRITE (RES) ( (LCODEY (I, J), I = 1, NX), J = 1, NY)  
!      WRITE (RES) (IDUM0,I=1,total_no_elements)
!      WRITE (RES) ((IDUM0,I=1,NLF),J=1,2)
!
! ALDCB8 (except RHOSAR)
!60-71
WRITE (RES) (DXIN (I), I = 1, NX)  
WRITE (RES) (DYIN (I), I = 1, NY)  
!      WRITE (RES) (FDUM0,I=1,NLF)
!      WRITE (RES) (FDUM0,I=1,NEL)
!      WRITE (RES) (FDUM0,I=1,NLF)
!      WRITE (RES) (FDUM0,I=1,NLF)
!      WRITE (RES) (FDUM0,I=1,NLF)
!      WRITE (RES) (FDUM0,I=1,NV)
!      WRITE (RES) (FDUM0,I=1,NLF)
WRITE (RES) (IOSTA (I), I = 1, NSET)  
WRITE (RES) (IOSTEP (I), I = 1, NSET)  
WRITE (RES) (IOEND (I), I = 1, NSET)  
!
! VSSOLI/VSSOLR (except VSPSS, VSPPOR)
!72-79
WRITE (RES) NVSSOL  
WRITE (RES) (VSPPSI (I), I = 1, NVSSOL)  
WRITE (RES) ( (VSPTHE (I, J), I = 1, NVSSOL), J = 1, NS)  
WRITE (RES) ( (VSPKR (I, J), I = 1, NVSSOL), J = 1, NS)  
WRITE (RES) ( (VSPETA (I, J), I = 1, NVSSOL), J = 1, NS)  
WRITE (RES) ( (VSPDTH (I, J), I = 1, NVSSOL), J = 1, NS)  
WRITE (RES) ( (VSPDKR (I, J), I = 1, NVSSOL), J = 1, NS)  
WRITE (RES) ( (VSPDET (I, J), I = 1, NVSSOL), J = 1, NS)  
!
! CLOSE RES FILE, SO THAT RESULTS CAN BE INSPECTED USING SHEGRAPH BEFORE
! SIMULATION HAS TERMINATED
!
CLOSE (RES)
!
! OPEN OUTPUT DATA FILES ON FILE UNITS 50 ONWARDS
!
IF (NSET.GT.0) THEN  
   DO 280 ICHAR = 2, LEN (RESFIL)  
      IF (RESFIL (ICHAR:) .EQ.' ') GOTO 290  
  280    END DO  
  290    ICHAR = ICHAR - 1  
   DO 300 ISET = 1, NSET  
      IORES (ISET) = 50 + ISET  
      WRITE (ANUM, '(I2.2)') ISET
      fname = RESFIL(:ICHAR)//ANUM
      OPEN(IORES(ISET),FILE=TRIM(fname),FORM='UNFORMATTED')
      WRITE ( *, 9300) IORES (ISET), RESFIL (:ICHAR), ANUM  
  300    END DO  
ENDIF  
 9300 FORMAT(' OPENING FILE UNIT',I3,' TO FILE ',2A)  
!
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
! Modifications:
! RAH  941005 3.4.1 Add argument RESNOW: use instead of UZNOW (from
!                   SPEC.AL) - see SHETRN; see also FRINIT,INRES.
!                   Bring IMPLICIT from SPEC.AL.  Don't include SPEC.SY.
!                   Replace CSED,SEDTRN with asum_over_faces(QSED),QSED.
!  GP  960628  4.0  All floating-point output in single precision:
!                   use DOUBLEPRECISION, & locals BUFFER,COLBUF.
!                   Add argument NOW to allow IOTIME override
!                   (see MB; also FRINIT,INRES,SHETRN).
!                   New VSS module: replace THUZ,HSZ,QHSZ,QBKI(*,1+2),
!                   PSI3,TH3,UUAJPO with QVSV(column),ZVSPSL,QVSH,
!                   QVSSPR,VSPSI,VSTHE,QVSWEL*AREA.
!                   QBKB (was type 16): add to QBKF (17); 16 undefined.
!                   Types 45-49 (BCPAA,BCPBB,EPS) now undefined.
!                   Type 50 (new): BALANC*1D3/CAREA.
! RAH  970224  4.1  Swap indices: QVSH,QVSV,QVSWLI,VSPSI,VSTHE (AL.C).
!                   Scrap locals IX,IY,IL (redundant) & ITYPE
!                   (use IEL.LE.NLF).  Explicit typing.
!                   Set BUFFER=999.999 if otherwise undefined.
!      970524       Don't include COLM.CO or PLANT.CC.
!                   AIOSTO asasumed size (was 50).
! RAH  980309  4.2  Replace CCSTOR,CSSTOR,CMEAN,SMEAN,ADMEAN,999.999
!                   with UNDEF.
!  SB  990127 4.27  incorporate 3.4.2 sediment code into 4.27
!                   include SED.CS, CONST.SY
!                   integers SFSED1 and SFSED2,DOUBLEPRECISION DUM1(4)
!                   data types 21 - 30
!  SB  010320 4g-pc dum1(4) to real
!----------------------------------------------------------------------*
! Entry conditions:
! 1       <= NELEE
! 1, NSET <= NSETEE
! for each set in 1:NSET:
!       1 <= IODATA(set) <=  50 ( size_of_AIOSTO)
!     NEL >= IOELEM(set) >= -14 (-size_of_ICLNUM)
!     IOELEM(set) <= 0  ==>:
!         IODATA(set)=50  ==>  ICLNUM(class) <= 20 (size_of_BALANC)
!         IODATA(set)<50  ==>:
!                         1 <= ICLNUM(class)   <= NELEE
!              for each j in 1:ICLNUM(class):
!                         1 <= ICLIST(j,class) <= NEL
!     where class = -IOELEM(set)
!     IODATA(set) in 21:38 or 44:44  ==>  1 <= IOCORS(set) <= NCON
!      IORES(set) is connected for unformatted output
!----------------------------------------------------------------------*
! Commons and constants

!
! Input common
!     CONT.CC:         NCON
!
! Input arguments
DOUBLEPRECISION RESNOW  
LOGICAL :: NOW  
CHARACTER (LEN=*) :: AIOSTO  
!
! Locals, etc
INTEGER :: SFSED1, SFSED2  
DOUBLEPRECISION :: DUM1 (4)  
DOUBLEPRECISION :: UNDEF  
PARAMETER (UNDEF = 999.999)  
INTEGER :: ICLASS, ICORS, IDATA, IEL, ISET, IW, J, K, KK, NOUT
DOUBLEPRECISION :: BUFFER (NELEE), COLBUF (LLEE)
DOUBLEPRECISION :: dumo, dum0

LOGICAL :: COLUMN  
!^^^^ sb 4/2/99
!^^^^ cummulative erosion output data type 44
!^^^^ gnucum = mm
!^^^^ gnu m/s
INTEGER :: sed  
!     * DO loop variable limits
SFSED1 (ICORS) = MAX (1, ICORS)  

SFSED2 (ICORS) = MAX (NSED * (1 - ICORS), ICORS)  
!
!----------------------------------------------------------------------*
!
! --- LOOP OVER ALL OUTPUT SETS
!
!^^^^ sb 4/2/99
!^^^^ cummulative soil loss data type 44
IF (.NOT.SEDSRT) THEN  
   DO 653 J = 1, total_no_elements  
      IF (NOTZERO(DLS (J))) SEDSRT = .TRUE.  
      DLSSRT (J) = DLS (J)  
  653    END DO  
ENDIF  
DO 654 J = 1, total_no_elements  
   GNUCUM (J) = GNUCUM (J) + GNU (J) * (RESNOW - PREVTM) * 3600 * &
    1000


  654 END DO  

DO 100 ISET = 1, NSET  
   COLUMN = .FALSE.  
!
! CHECK IF DATA FOR THIS SET IS TO BE OUTPUT NOW.
! REJECT DATA IF COMPUTATIONAL TIME HAS NOT REACHED NEXT OUTPUT TIME or
! if no further output is required (unless input argument NOW is .TRUE.)
! OR DATA IS NOT REQUESTED TO BE OUTPUT ON THIS CALL TO THIS ROUTINE.
!
   IF (.NOT.NOW) THEN  
      IF (RESNOW.LT.IOTIME (ISET) - 1.0E-6) GOTO 100  
      IF (IOTIME (ISET) .GE.IOEND (ISET) ) GOTO 100  
   ENDIF  
!
   IDATA = IODATA (ISET)  
   IF (IDATA.LT.1.OR.IDATA.GT.MIN (LEN (AIOSTO), 50) ) GOTO 100  
   IF (AIOSTO (IDATA:IDATA) .NE.'1') GOTO 100  
!
! SET UP NUMBER OF DATA ITEMS TO BE WRITTEN
!
   IF (IOELEM (ISET) .GT.0) THEN  
      NOUT = 1  
   ELSE  
      ICLASS = - IOELEM (ISET)  
      NOUT = ICLNUM (ICLASS)  
   ENDIF  
   ICORS = IOCORS (ISET)  
!
! ASSEMBLE OUTPUT BUFFER
!

   DO 50 J = 1, NOUT  
      IF (IOELEM (ISET) .GT.0) THEN  
         IEL = IOELEM (ISET)  
      ELSE  
         IEL = ICLIST (J, ICLASS)  
      ENDIF  
!
      IF (IODATA (ISET) .EQ.1) THEN  
         BUFFER (J) = PNETTO (IEL) * 3600000.  
      ELSEIF (IODATA (ISET) .EQ.2) THEN  
         BUFFER (J) = EPOT (IEL) * 3600000.  
      ELSEIF (IODATA (ISET) .EQ.3) THEN  
         BUFFER (J) = ERZA (IEL) * 3600000.  
      ELSEIF (IODATA (ISET) .EQ.4) THEN  
         BUFFER (J) = ESOILA (IEL) * 3600000.  
      ELSEIF (IODATA (ISET) .EQ.5) THEN  
         BUFFER (J) = EINTA (IEL) * 3600000.  
      ELSEIF (IODATA (ISET) .EQ.6) THEN  
         BUFFER (J) = DRAINA (IEL) * 3600000.  
      ELSEIF (IODATA (ISET) .EQ.7) THEN  
         BUFFER (J) = CSTORE (IEL)  
      ELSEIF (IODATA (ISET) .EQ.8) THEN  
         BUFFER (J) = QH (IEL) * 3600000.  
      ELSEIF (IODATA (ISET) .EQ.9) THEN  
         COLUMN = .TRUE.  
         WRITE (IORES (ISET) ) RESNOW, ((QVSV (K, IEL) ), &
          K = 1, top_cell_no)
      ELSEIF (IODATA (ISET) .EQ.10) THEN  
         BUFFER (J) = SD (IEL)  
      ELSEIF (IODATA (ISET) .EQ.11) THEN  
         BUFFER (J) = TS (IEL)  
      ELSEIF (IODATA (ISET) .EQ.12) THEN  
         BUFFER (J) = ZVSPSL (IEL) - ZGRUND (IEL)  
      ELSEIF (IODATA (ISET) .EQ.13) THEN  
         COLUMN = .TRUE.  
         WRITE (IORES (ISET) ) RESNOW, ( ((QVSH (KK, K, IEL) &
          ), K = 1, top_cell_no), KK = 1, 4)
      ELSEIF (IODATA (ISET) .EQ.14) THEN  
         COLUMN = .TRUE.  
         WRITE (IORES (ISET) ) RESNOW, ((QOC (IEL, K) ), &
          K = 1, 4)
      ELSEIF (IODATA (ISET) .EQ.15) THEN  
         BUFFER (J) = GETHRF (IEL) - ZGRUND (IEL)  
      ELSEIF (IODATA (ISET) .EQ.16) THEN  
         BUFFER (J) = UNDEF  
      ELSEIF (IODATA (ISET) .EQ.17) THEN  
         IF (IEL.LE.total_no_links) THEN  
            BUFFER (J) = QBKB (IEL, 1) + QBKB (IEL, 2) + QBKF ( &
             IEL, 1) + QBKF (IEL, 2)
         ELSE  
            BUFFER (J) = UNDEF  
         ENDIF  
      ELSEIF (IODATA (ISET) .EQ.18) THEN  
         BUFFER (J) = QVSSPR (IEL)  
      ELSEIF (IODATA (ISET) .EQ.19) THEN  
         COLUMN = .TRUE.  
         WRITE (IORES (ISET) ) RESNOW, ((VSPSI (K, IEL) ), &
          K = 1, top_cell_no)
      ELSEIF (IODATA (ISET) .EQ.20) THEN  
         COLUMN = .TRUE.  
         WRITE (IORES (ISET) ) RESNOW, ((VSTHE (K, IEL) ), &
          K = 1, top_cell_no)
      ELSEIF (IODATA (ISET) .EQ.21) THEN  
         DUM0 = DLS (IEL)  
         IF (ICORS.GT.0) DUM0 = DUM0 * FBETA (IEL, ICORS)  
         BUFFER (J) = 1D3 * DUM0  
      ELSEIF (IODATA (ISET) .EQ.22) THEN  
         DUM0 = 0  
         DO 220 SED = SFSED1 (ICORS), SFSED2 (ICORS)  
            DUM0 = DUM0 + FDEL (IEL, SED)  
  220          END DO  
         BUFFER (J) = 1D3 * RHOSED * (1 - PLS (IEL) ) * DUM0  
      ELSEIF (IODATA (ISET) .EQ.23) THEN  
         BUFFER (J) = GINFD (IEL, ICORS)  
      ELSEIF (IODATA (ISET) .EQ.24) THEN  
         BUFFER (J) = GINFS (IEL, ICORS)  
      ELSEIF (IODATA (ISET) .EQ.25) THEN  
         BUFFER (J) = 1000.0 * 24.0D0 * 3600.0D0 * GNU (IEL)  
      ELSEIF (IODATA (ISET) .EQ.26) THEN  
         BUFFER (J) = 1000.0 * 24.0D0 * 3600.0D0 * GNUBK (IEL)  
      ELSEIF (IODATA (ISET) .EQ.27) THEN  
         COLUMN = .TRUE.  
         DO 274 K = 1, 4  
            DUM0 = 0  
            DO 270 SED = SFSED1 (ICORS), SFSED2 (ICORS)  
               DUM0 = DUM0 + QSED (IEL, SED, K)  
  270             END DO  
            DUM1 (K) = DUM0 * RHOSED  
  274          END DO  
         WRITE (IORES (ISET) ) RESNOW, DUM1  
      ELSEIF (IODATA (ISET) .EQ.28) THEN  
         DUM0 = 0  
         DO 280 SED = SFSED1 (ICORS), SFSED2 (ICORS)  
            DUM0 = DUM0 + QSED (IEL, SED, 1) + QSED (IEL, SED, 2) &
             + QSED (IEL, SED, 3) + QSED (IEL, SED, 4)
  280          END DO  
         BUFFER (J) = DUM0 * RHOSED  
      ELSEIF (IODATA (ISET) .EQ.29) THEN  
         IF (DCBED (IEL) .gt.0) then  
            BUFFER (J) = DCBSED (IEL, ICORS) / DCBED (IEL)  
         ELSE  
            BUFFER (J) = ZERO  
         ENDIF  
      ELSEIF (IODATA (ISET) .EQ.30) THEN  
         COLUMN = .TRUE.  
         DO 304 K = 1, 4  
            DUM0 = 0  
            DO 300 SED = SFSED1 (ICORS), SFSED2 (ICORS)  
               if (QOC (IEL, K) .gt.ZERO) then  
                  DUM0 = DUM0 + QSED (IEL, SED, K) / QOC (IEL, K)  
               else  
                  DUMO = ZERO  
               endif  
  300             END DO  
            DUM1 (K) = 1D3 * DUM0 * RHOSED  
  304          END DO  
         WRITE (IORES (ISET) ) RESNOW, (DUM1 (K), K = 1, 4)  
      ELSEIF (IODATA (ISET) .EQ.31) THEN  
         BUFFER (J) = ARBDEP (IEL)  
      ELSEIF (IODATA (ISET) .EQ.32) THEN  
         COLUMN = .TRUE.  
         WRITE (IORES (ISET) ) RESNOW, ((CCCC (IEL, K, ICORS) &
          ), K = 1, top_cell_no)
      ELSEIF (IODATA (ISET) .EQ.33) THEN  
         COLUMN = .TRUE.  
         WRITE (IORES (ISET) ) RESNOW, ((SSSS (IEL, K, ICORS) &
          ), K = 1, top_cell_no)
      ELSEIF (IODATA (ISET) .EQ.34) THEN  
         BUFFER (J) = CCCC (IEL, top_cell_no, ICORS)  
      ELSEIF (IODATA (ISET) .EQ.35) THEN  
         BUFFER (J) = CCCC (IEL, top_cell_no - 1, ICORS)  
      ELSEIF (IODATA (ISET) .EQ.36) THEN  
         BUFFER (J) = CCCC (IEL, top_cell_no - 2, ICORS)  
      ELSEIF (IODATA (ISET) .EQ.37) THEN  
         BUFFER (J) = CCCC (IEL, NCOLMB (IEL), ICORS)  
      ELSEIF (IODATA (ISET) .EQ.38) THEN  
         BUFFER (J) = CCCCW (IEL, ICORS)  
      ELSEIF (IODATA (ISET) .EQ.39) THEN  
         BUFFER (J) = UNDEF  
      ELSEIF (IODATA (ISET) .EQ.40) THEN  
         BUFFER (J) = UNDEF  
      ELSEIF (IODATA (ISET) .EQ.41) THEN  

         BUFFER (J) = QVSWEL (IEL) * cellarea (IEL)  
!>>> well abstraction rates - TEMPORARY! 960628
      ELSEIF (IODATA (ISET) .EQ.42) THEN  
         COLUMN = .TRUE.  
         IW = NVSWLI (IEL)  
         IF (IW.GT.0) THEN  
            DO 340 K = 1, top_cell_no  
               COLBUF (K) = QVSWLI (K, IW) * cellarea (IEL)  
  340             END DO  
         ELSE  
            DO 345 K = 1, top_cell_no  
               COLBUF (K) = zero  
  345             END DO  
         ENDIF  

         WRITE (IORES (ISET) ) RESNOW, (COLBUF (K), K = 1, top_cell_no)  
!<<<
      ELSEIF (IODATA (ISET) .EQ.43) THEN  
         BUFFER (J) = WBERR (IEL)  
      ELSEIF (IODATA (ISET) .EQ.44) THEN  
!^^^^ sb 4/2/99
!^^^^ cummulative soil loss output data type 44
!^^^^^ cumulative erosion - depth of loose soil
         BUFFER (J) = GNUCUM (IEL) - (DLS (IEL) - DLSSRT (IEL) ) &
          * 1000
      ELSEIF (IODATA (ISET) .EQ.45) THEN  
         BUFFER (J) = UNDEF  
      ELSEIF (IODATA (ISET) .EQ.46) THEN  
         BUFFER (J) = UNDEF  
      ELSEIF (IODATA (ISET) .EQ.47) THEN  
         BUFFER (J) = UNDEF  
      ELSEIF (IODATA (ISET) .EQ.48) THEN  
         BUFFER (J) = UNDEF  
      ELSEIF (IODATA (ISET) .EQ.49) THEN  
         BUFFER (J) = UNDEF  
      ELSEIF (IODATA (ISET) .EQ.50) THEN  
         BUFFER (J) = BALANC (J) * 1000.0D0 / CAREA  
      ENDIF  
!
   50    END DO  
!

   IF (.NOT.COLUMN) WRITE (IORES (ISET) ) RESNOW, (BUFFER (J), &
    J = 1, NOUT)
!
   IOTIME (ISET) = RESNOW + IOSTEP (ISET)  
!
  100 END DO  
!
!^^^^ sb 4/2/99
!^^^^ cummulative erosion output data type 44

PREVTM = RESNOW  
END SUBROUTINE FRRESP
! 14/3/95
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



!SSSSSS SUBROUTINE FRSORT  
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
  100 END DO  
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
    220    ZHIGH = zero  
    DO 240 I = NSTART + 1, NSORT (L)  
        IF (ELEV (I, L) .GT.ZHIGH) ZHIGH = ELEV (I, L)  
    240 ENDDO  
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
    320 ZLOW = 1.0E10  
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
    400 JUMP = NEND-NSTART + 1  
    410 JUMP = JUMP / 2  
    IF (JUMP.NE.0) THEN  
        DO M = NSTART, NEND-JUMP  
            K = M
            DO  
                420 N = K + JUMP  
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
      GOTO 410  
   ENDIF  
    !
    ! --- ARRAY ISTEMP IS SORTED
    !
500 END DO  
!
! --- REASSEMBLE ISORT ARRAY
!
I1 = 1  
I2 = 1  
IS = 1  
!
  600 IF (NS1.GT.0) THEN  
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
!
IF (I1.GT.NS1) THEN  
   DO 520 I = IS, total_no_elements  
      ISORT (I) = ISTEMP (I2, 2)  
      I2 = I2 + 1  
  520    END DO  
   GOTO 700  
ENDIF  
!
IF (I2.GT.NS2) THEN  
   DO 540 I = IS, total_no_elements  
      ISORT (I) = ISTEMP (I1, 1)  
      I1 = I1 + 1  
  540    END DO  
   GOTO 700  
ENDIF  
!
GOTO 600  
!
  700 CONTINUE  
!
RETURN  
!
 1000 FORMAT(' total_no_elements= ',I4,'  NS1= ',I4,' NS2= ',I4,' SFCMAX(*)= ',F7.1, &
&       ' sfcmin=',f7.1,' SZMAX(+)= ',F7.1,' szmin=',f7.1)
 1010 FORMAT(' ',I4,' ',I4,' |',A68)  
!
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
!JE   JAN 2009      Loop restructure for AD
!----------------------------------------------------------------------*
! Commons and constants
  
 
! Imported constants
!     SPEC.AL          NCLASS,NELEE,NLFEE
! Input common
!     SPEC.AL          BKD,FATAL,NEL,NGDBGN,NLF,PRI
!                      ICLIST(NELEE,NCLASS),ICLNUM(NCLASS)
!                      ICMREF(NELEE,8)
!                      ZBFULL(NLFEE)
! In|out common
!     SPEC.AL             NMC(NEL),NRAINC(NEL),   NVC(NEL)
!                      RHOSAR(NEL),ZGRUND(NEL)
!                         HRF(NEL),    SD(NEL),ZVSPSL(NEL)
! Workspace common
!     SPEC.AL           IDUM(NGDBGN:NEL)
!                      DUMMY(NGDBGN:NEL)
! Locals, etc
INTEGER :: I, IEL, ICOUNT, IDATA, IFAULT, IL, INTYPE, ITYPE  
INTEGER :: J, JEL, NVALUE  
INTEGER :: IVALUE (NLFEE * 2), IELEM (NLFEE * 2)  
DOUBLEPRECISION DFAULT, DZG, VALUE (NLFEE * 2)  
!CHARACTER (LEN=80) :: TITLE  
LOGICAL :: BINBKD, INTEGR (13), g70
!

DATA INTEGR / .FALSE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., &
 .FALSE., .TRUE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE. /
!----------------------------------------------------------------------*
!
! READ TITLE, FLAG FOR PRINTING INITIALISATION DATA
!:BK1
READ (BKD, 1000) TITLE  
READ (BKD, 1100) BINBKD  
!
! ----- LOOP OVER INPUT DATA TYPES
!
out500 : DO IDATA = 1, 13  
    !     INITIALISE DUMMY ARRAYS
    DO IEL = NGDBGN, total_no_elements  
        IDUM (IEL) = 0  
        DUMMY (IEL) = zero  
    ENDDO  
    !     READ TITLE, INPUT METHOD, NUMBER OF FOLLOWING VALUES
    !:BK3
    READ (BKD, 1000) TITLE  
    IF (BINBKD) WRITE(PPPRI, 1000) TITLE  
    READ (BKD, 1200) INTYPE, NVALUE  
    !
    !
    !        TYPE 1: SET VALUE = VALUE AT ADJACENT GRID
    !        ++++++++++++++++++++++++++++++++++++++++++
    !
    !         (except ZGRUND     = ZBFULL(il)
    !             and ZVSPSL,HRF = value + ZGRUND - ZGRUND(jel) )
    !
    ! NB. CATCHMENT IS SCANNED TWICE. THE 2nd TIME THROUGH, ANY BANKS WITH
    !    NO ADJACENT GRID ARE GIVEN THE VALUE OF THE 1st ADJACENT BANK FOUND
    !
    IF (INTYPE.EQ.1) THEN  
        out95 : DO ICOUNT = 1, 2
            out90 : DO IEL = NGDBGN, total_no_elements
                ITYPE = ICMREF (IEL, 1)  
                IF (ITYPE.NE.1.AND.ITYPE.NE.2) CYCLE out90 !GOTO 90  
                !                                                 >>>>>>>
                !                  * find adjacent element
                g70 = .FALSE.
                out60 : DO J = 1, 4
                    IF(g70) CYCLE out60 
                    JEL = ICMREF (IEL, 4 + J)  
                    IF (JEL.GT.0) THEN  
                        IF (ICMREF (JEL, 1) .EQ.0) g70=.TRUE. !GOTO 70  
                    ENDIF  
                ENDDO out60
                IF(.NOT. g70) THEN
                    out65 : DO J = 1, 4
                        IF(g70) CYCLE out65 
                        JEL = ICMREF (IEL, J + 4)  
                        IF (JEL.GT.0) THEN  
                            IF (ICMREF (JEL, 1) .EQ.1.OR.ICMREF (JEL, 1) .EQ.2) g70=.TRUE. !GOTO 70
                        ENDIF  
                    ENDDO out65
                ENDIF
                !70  CONTINUE  
                !                  * set value
                DZG = ZGRUND (IEL) - ZGRUND (JEL)  
                IF (IDATA.EQ.1) THEN  
                    IL = ICMREF (IEL, 4)  
                    ZGRUND (IEL) = ZBFULL (IL)  
                ELSEIF (IDATA.EQ.2) THEN  
                    NMC (IEL) = NMC (JEL)  
                ELSEIF (IDATA.EQ.3) THEN  
                    NRAINC (IEL) = NRAINC (JEL)  
                ELSEIF (IDATA.EQ.4) THEN  
                    NVC (IEL) = NVC (JEL)  
                ELSEIF (IDATA.EQ.6) THEN  
                    STRXX (IEL) = STRXX (JEL)  
                ELSEIF (IDATA.EQ.7) THEN  
                    STRYY (IEL) = STRYY (JEL)  
                ELSEIF (IDATA.EQ.10) THEN  
                    SD (IEL) = SD (JEL)  
                ELSEIF (IDATA.EQ.11) THEN  
                    RHOSAR (IEL) = RHOSAR (JEL)  
                ELSEIF (IDATA.EQ.12) THEN  
                    ZVSPSL (IEL) = ZVSPSL (JEL) + DZG  
                ELSEIF (IDATA.EQ.13) THEN  
                    CALL SETHRF(IEL, GETHRF (JEL) + DZG)  
                ENDIF  
            ENDDO out90
        ENDDO out95 
        CYCLE OUT500 !GOTO 500  
    !            >>>>>>>>
    !
    !
    !        TYPE 2: READ SINGLE DEFAULT VALUE
    !        +++++++++++++++++++++++++++++++++
    !
    ELSEIF (INTYPE.EQ.2) THEN  
        !:BK5
        IF (INTEGR (IDATA) ) THEN  
            READ (BKD, 1200) IFAULT  
            IF (BINBKD) WRITE(PPPRI, 1300) IFAULT  
            DO IEL = NGDBGN, total_no_elements  
                ITYPE = ICMREF (IEL, 1)  
                IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) IDUM (IEL) = IFAULT  
            ENDDO  
                !:BK6
        ELSE  
            READ (BKD, 1400) DFAULT  
            IF (BINBKD) WRITE(PPPRI, 1500) DFAULT  
            DO IEL = NGDBGN, total_no_elements  
                ITYPE = ICMREF (IEL, 1)  
                ! amended by GP 18/7/94 to be consistent with DSATE code
                IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN  
                    IF (IDATA.EQ.1) THEN  
                        IL = ICMREF (IEL, 4)  
                        DUMMY (IEL) = ZBFULL (IL) + DFAULT  
                    ELSE  
                        DUMMY (IEL) = DFAULT  
                    ENDIF  
                ENDIF  
                !--------------------------------------------------------
            ENDDO  
        ENDIF  
        !        TYPE 3: READ PAIRS OF (DATA CLASS, VALUE)
        !        +++++++++++++++++++++++++++++++++++++++++
    ELSEIF (INTYPE.EQ.3) THEN  
        !:BK7-8
        CALL ERROR(FFFATAL, 1061, PPPRI, 0, 0, 'BKD input type 3 (data class, value) not supported')
        !        TYPE 4: READ PAIRS OF (BANK ELEMENT NUMBER, VALUE)
        !        ++++++++++++++++++++++++++++++++++++++++++++++++++
    ELSEIF (INTYPE.EQ.4) THEN  
        !
        NVALUE = 2 * total_no_links  
        !980713
        IF (INTEGR (IDATA) ) THEN  
            READ (BKD, 1200) (IELEM (I), IVALUE (I), I = 1, NVALUE)  
            IF (BINBKD) WRITE(PPPRI, 2000)  
            IF (BINBKD) WRITE(PPPRI, 2050) (IELEM (I), IVALUE (I), I = 1, NVALUE)
            DO I = 1, NVALUE  
                IEL = IELEM (I)  
                ITYPE = ICMREF (IEL, 1)  
                IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) IDUM (IEL) = IVALUE (I)  
            ENDDO  
        ELSE  
            READ (BKD, 1800) (IELEM (I), VALUE (I), I = 1, NVALUE)  
            IF (BINBKD) WRITE(PPPRI, 2100)  
            IF (BINBKD) WRITE(PPPRI, 2150) (IELEM (I), VALUE (I), I = 1, NVALUE)
            DO I = 1, NVALUE  
                IEL = IELEM (I)  
                ITYPE = ICMREF (IEL, 1)  
                IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) DUMMY (IEL) = VALUE (I)  
            ENDDO  
        ENDIF  
    ENDIF  
    !
    ! MOVE DATA FROM DUMMY ARRAYS INTO ACTUAL DATA ARRAYS
    !
    DO IEL = NGDBGN, total_no_elements  
        ITYPE = ICMREF (IEL, 1)  
        IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN  
            IF (IDATA.EQ.1) THEN  
                ZGRUND (IEL) = DUMMY (IEL)  
            ELSEIF (IDATA.EQ.2) THEN  
                NMC (IEL) = IDUM (IEL)  
            ELSEIF (IDATA.EQ.3) THEN  
                NRAINC (IEL) = IDUM (IEL)  
            ELSEIF (IDATA.EQ.4) THEN  
                NVC (IEL) = IDUM (IEL)  
            ELSEIF (IDATA.EQ.6) THEN  
                STRXX (IEL) = DUMMY (IEL)  
            ELSEIF (IDATA.EQ.7) THEN  
                STRYY(IEL) = DUMMY (IEL)  
            ELSEIF (IDATA.EQ.10) THEN  
                SD (IEL) = DUMMY (IEL)  
            ELSEIF (IDATA.EQ.11) THEN  
                RHOSAR (IEL) = DUMMY (IEL)  
            ELSEIF (IDATA.EQ.12) THEN  
                ZVSPSL (IEL) = ZGRUND (IEL) - DUMMY (IEL)  
            ELSEIF (IDATA.EQ.13) THEN  
                CALL SETHRF(IEL, ZGRUND (IEL) + DUMMY (IEL))
            ENDIF  
        ENDIF  
    ENDDO
ENDDO out500  
!
! FORMAT STATEMENTS
!
 1000 FORMAT(A)  
!
 1100 FORMAT(L7)  
!
 1200 FORMAT(10I7)  
!
 1300 FORMAT(' DEFAULT VALUE ',I7,' USED IN ALL BANK ELEMENTS'/)  
!
 1400 FORMAT(10F7.0)  
!
 1500 FORMAT(' DEFAULT VALUE ',F12.3,' USED IN ALL BANK ELEMENTS'/)  
!
 1800 FORMAT(5(I7,F7.0))  
!
 2000 FORMAT(' VALUES ALLOCATED TO EACH ELEMENT:'/        3('       ELEMENT   VALUE'))
!
 2050 FORMAT(3(I7,2X,I7,6X))  
!
 2100 FORMAT(' VALUES ALLOCATED TO EACH ELEMENT:'/        3('       ELEMENT     VALUE'))
!
 2150 FORMAT(3(I7,F12.3,6X))  
!
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

USE CMmod, ONLY:CMRD   !"JE"
! Input common
!     ...
! Input arguments

LOGICAL :: ISSDON  
!                             ANSWER TO: IS SEDIMENT CODE ACTIVE?
! Locals, etc
!INTRINSIC DBLE, INT, MAX, MIN  
INTEGER :: ICL, IDEL, IEL, IFA, ITYPE, ITYPEA  
INTEGER :: JA, JAL, JBK, JBKU, JCL, JDEL, JDUM, JEL, JFA, JFLINK  
INTEGER :: JLYR, JSED, JSOIL, LDUM  
INTEGER :: NBKU, NCDUM, NCE, NCE1, NCE2, NCEA, NCL, NCONT  
INTEGER :: NDIFF, NDUM, NDUMA, NELMA, NLINK, NLINKA, NLINKU  
INTEGER :: NOL1, NOL2, NOLBD, NOLDUM, NOLP, NOLX  
INTEGER :: JFCE (2), JOLDUM (2), NBK (2), NCEDUM (2)  
DOUBLEPRECISION ARL, ARP, DBK, DKBED, DMULT, DUM, DUM1, DUM2, &
 DUM3, DUMK
DOUBLEPRECISION FNOLBD, asum, asumK  


DOUBLEPRECISION FNDUM (2), FOLDUM (2), KSPDUM (NELEE, LLEE), &
 ROH (LLEE)
! changes by sb 28/2/00 make phidat,difdat and sispdt local
! Output arguments
!
!
! Added by SB
INTEGER :: MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS  
INTEGER :: NUM_CATEGORIES_TYPES (NCONEE), NTAB (NOCTAB, NCONEE)  
INTEGER :: NCATTY (NELEE, NCONEE)  
DOUBLEPRECISION TABLE_CONCENTRATION (NOCTAB, NOCTAB, NCONEE)  
DOUBLEPRECISION TABLE_WATER_DEPTH (NOCTAB, NOCTAB, NCONEE)  


LOGICAL :: LDUM1, ISCNSV (NCONEE)  
!
!
!----------------------------------------------------------------------*
!
! New by SB 18/11/04
! contam.f removed. z2 and d0 (scaling variables) needed here
! -----------------------------------------------------------------
Z2 = 50.0d0  
D0 = 1.0D-3  
!----------------------------------------------------------------------*
!
! New by SB
! Parameter values for spatially variable initial contaminant conc.
! -----------------------------------------------------------------
!
MAX_NUM_CATEGORY_TYPES = NOCTAB  

MAX_NUM_DATA_PAIRS = NOCTAB  
!
! Read main CM input data file
! ----------------------------
!
!     Modified by SB

CALL CMRD (CMD, CMP, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, total_no_elements, total_no_links, NLFEE, NSEE, &  !"JE"
 NS, NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY, NLYRBT (total_no_links + 1, 1), &  !"JE"
 ICMXY, ICMBK, ICMREF (1, 5), BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  NCATTY, NCON, &  !"JE"
 NCOLMB (total_no_links + 1), NTAB, DBS, DBDI, CCAPI, CCAPE, CCAPR, CCAPB, &  !"JE"
TABLE_CONCENTRATION, TABLE_WATER_DEPTH, IIICF, SOFN, GNN, GGLMSO, ALPHBD, ALPHBS, KDDLS, &  !"JE"
 ALPHA, FADS, ISCNSV, IDUM, &  !"JE"
 DUMMY)  !"JE"
!           Checks the data used to calculate spatially variable
!           concentrations in the grid and bank elements is OK



CALL MUERR2 (CMP, total_no_elements, NELEE, total_no_links, MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCON, NCONEE, &
 NUM_CATEGORIES_TYPES,  NTAB, NCATTY, ISCNSV, TABLE_CONCENTRATION, TABLE_WATER_DEPTH, LDUM1)
!----------------------------------------------------------------------*
DO 9876 NCL = total_no_links + 1, total_no_elements  
   NCOLMB (NCL) = NLYRBT (NCL, 1)  

 9876 END DO  
IF (.NOT.ISSDON) THEN  
!                             ssssssssssssssssssssssssssssssssssssssssss
!                             ssssss INITIALISE SEDIMENT VARIABLES sssss
   NSED = 3  
   DO 1 NLINK = 1, total_no_links  
      ARBDEP (NLINK) = zero  
      DLS (NLINK) = zero  

      DLSO (NLINK) = zero  
      FBETA (NLINK, 1) = one  
      FBETA (NLINK, 2) = zero  

      FBETA (NLINK, 3) = zero  
      FBTSD (NLINK, 1) = one  
      FBTSD (NLINK, 2) = zero  

      FBTSD (NLINK, 3) = zero  
      FDEL (NLINK, 1) = zero  
      FDEL (NLINK, 2) = zero  

      FDEL (NLINK, 3) = zero  
      GINFD (NLINK, 1) = zero  
      GINFD (NLINK, 2) = zero  
      GINFD (NLINK, 3) = zero  
      GINFS (NLINK, 1) = zero  
      GINFS (NLINK, 2) = zero  

      GINFS (NLINK, 3) = zero  
      GNUBK (NLINK) = zero  
      QDEFF (NLINK, 1) = zero  

      QDEFF (NLINK, 2) = zero  
      DO 2 JA = 1, 4  
         NELMA = ICMREF (NLINK, JA + 4)  
         IF (NELMA.GT.0) THEN  
            ITYPEA = ICMREF (NELMA, 1)  
            IF (ITYPEA.EQ.1) THEN  
               NBK (1) = NELMA  
            ELSEIF (ITYPEA.EQ.2) THEN  
               NBK (2) = NELMA  
            ENDIF  
         ENDIF  
    2       END DO  
      JLYR = 0  
    4       JLYR = JLYR + 1  
      IF (NLYRBT (NBK (1), JLYR) .LT.NHBED (NLINK, 1) ) GOTO 4  
      NSOBED (NLINK) = NTSOIL (NBK (1), JLYR - 1)  
      PBSED (NLINK) = VSPOR (NSOBED (NLINK) )  
!                             SET BED SOIL TYPE AND POROSITY, BASED ON
!                             THE SOIL AT THE BOTTOM OF THE EXPOSED FACE
!                             OF BANK 1

    1    END DO  
   DO 5 NCL = total_no_links + 1, total_no_elements  
      DLS (NCL) = zero  

      DLSO (NCL) = zero  
      FDEL (NCL, 1) = zero  
      FDEL (NCL, 2) = zero  

      FDEL (NCL, 3) = zero  
      FBETA (NCL, 1) = one  
      FBETA (NCL, 2) = zero  

      FBETA (NCL, 3) = zero  
      GNU (NCL) = zero  
      GNUO (NCL) = zero  

    5    END DO  
   DO 6 JSOIL = 1, NSEE  
      SOSDFN (JSOIL, 1) = SOFN (JSOIL, 1)  
      SOSDFN (JSOIL, 2) = SOFN (JSOIL, 2)  
      SOSDFN (JSOIL, 3) = SOFN (JSOIL, 3)  

    6    END DO  
!                             SET SEDIMENT FRACTIONS FOR SOIL TYPES




ENDIF  
!                             IF THE SEDIMENT CODE IS NOT ACTIVE, THE
!                             SEDIMENT VARIABLES ARE SET TO APPROPRIATE
!                             VALUES
!                             ssssssssssssssssssssssssssssssssssssssssss
!                             cccccccccccccccccccccccccccccccccccccccccc
!                             ccccccccccccc SET CONSTANTS cccccccccccccc
SCL = one / 32500.0D0  

OODO = one / D0  
!                             SCALING FACTORS
Z2SQ = Z2 * Z2  
Z2OD = OODO * Z2  

Z2SQOD = OODO * Z2SQ  
!                            SCALING VALUES
SGMA = one  
SGSQ = SGMA * SGMA  

OMSGMA = one - SGMA  
!                            FINITE DIFFERENCE IMPLICIT WEIGHTING


NCETOP = top_cell_no  
!                            TOP CELL NUMBER FOR COLUMNS

DO 9 NCONT = 1, NCON  
!                             SET CONSTANTS WHICH DEPEND
!                             ON CONTAMINANT NUMBER

   GCPLA (NCONT) = GGLMSO (NCONT) * Z2SQOD  
!                            SET DECAY CONSTANTS FOR CONTAMINANTS
   DO 71 JSOIL = 1, NS  
      asum = zero  
      DO 72 JSED = 1, NSED  
         asum = asum + SOSDFN (JSOIL, JSED) * KDDLS (JSED, NCONT)  
   72       END DO  
      KDDSOL (JSOIL, NCONT) = asum  
   71    END DO  
!                             SET REFERENCE DISTRIBUTION COEFFICIENT FOR
!                             SOIL TO MATCH THAT SPECIFIED FOR THE
!                             SEDIMENT PARTICLE SIZE GROUPS


    9 END DO  
DO 10 NCL = total_no_links + 1, total_no_elements  
   ZCOLMB (NCL) = ZVSNOD (NCOLMB (NCL), NCL)  




   10 END DO  
!                             SET ELEVATION OF
!                             BOTTOM CELLS IN SOIL COLUMNS
! set up temporary array for use until full vss coding completed
DO 13 NCL = 1, total_no_elements  
   DO 14 NCE = NLYRBT (NCL, 1), top_cell_no  
      KSPDUM (NCL, NCE) = DELTAZ (NCE, NCL) / Z2  
   14    END DO  
   KSPDUM (NCL, top_cell_no + 1) = KSPDUM (NCL, top_cell_no)  





   13 END DO  
!---------------------------------------------------------------
! Set up NOL, NOLBT, NOLCE, NOLCEA, JOLFN using VSS arrays JVSACN,
! JVSDEL and DELTAZ
! NB. NOLBT and JOLFN are overwritten during the loop over a column

DO 20 IEL = total_no_links + 1, total_no_elements  

   DO 24 IFA = 1, 4  
      JEL = ICMREF (IEL, IFA + 4)  
      JFA = ICMREF (IEL, IFA + 8)  
      IF (JEL.EQ.0) THEN  
         JEL = IEL  
         JFA = IFA  
      ELSEIF (ICMREF (JEL, 1) .EQ.3) THEN  
         JEL = ICMREF (JEL, IFA + 4)  

      ENDIF  
      NOLP = 0  
      DO 26 ICL = NLYRBT (IEL, 1), top_cell_no  
         IF (JVSACN (IFA, ICL, IEL) .GT.0) THEN  
            JCL = JVSACN (IFA, ICL, IEL)  
            IDEL = JVSDEL (IFA, ICL, IEL)  

            JDEL = JVSDEL (JFA, JCL, JEL)  
            NOLP = NOLP + 1  
            NOLCE (IEL, NOLP, IFA) = ICL  
            NOLCEA (IEL, NOLP, IFA) = JCL  

            NOLBT (IEL, ICL, IFA) = NOLP  
            IF (IDEL.EQ.1) THEN  
               JOLFN (IEL, NOLP, IFA) = INT (32500.0D0 * DELTAZ ( &
                ICL, IEL) / (DELTAZ (ICL, IEL) + DELTAZ (ICL + 1, &
                IEL) ) )
               NOLP = NOLP + 1  
               NOLCE (IEL, NOLP, IFA) = ICL + 1  
               NOLCEA (IEL, NOLP, IFA) = JCL  
               JOLFN (IEL, NOLP, IFA) = INT (32500.0D0 * DELTAZ ( &
                ICL + 1, IEL) / (DELTAZ (ICL, IEL) + DELTAZ (ICL + &
                1, IEL) ) )
            ELSEIF (JDEL.EQ.1) THEN  
               NOLP = NOLP + 1  
               NOLCE (IEL, NOLP, IFA) = ICL  
               NOLCEA (IEL, NOLP, IFA) = JCL + 1  
            ELSE  
               JOLFN (IEL, NOLP, IFA) = 32500  
            ENDIF  

         ENDIF  

   26       END DO  
      NOL (IEL, IFA) = NOLP  

      NOLBT (IEL, top_cell_no + 1, IFA) = NOLP + 1  

   24    END DO  


   20 END DO  
DKBED = DBDI / Z2  
DO 100 NLINK = 1, total_no_links  
!                             ^^^^^^^^^ SET CONSTANTS FOR LINKS ^^^^^^^^
   DO 102 JA = 1, 4  
      NDUMA = ICMREF (NLINK, JA + 4)  
      IF (NDUMA.GT.0) THEN  
         ITYPEA = ICMREF (NDUMA, 1)  
         IF ( (ITYPEA.EQ.1) .OR. (ITYPEA.EQ.2) ) THEN  
!                             ADJACENT ELEMENT IS A BANK
            JBK = ITYPEA  
            NBK (JBK) = NDUMA  
!                             USED ONLY IN THIS ROUTINE
            NBANK (NLINK, JBK) = NDUMA  
!                             SAVED FOR USE IN OTHER SUBROUTINES
            asum = FHBED (NLINK, JBK) * KSPDUM (NBK (JBK), NHBED ( &
             NLINK, JBK) + 1)
            IF (asum.GE.DKBED) THEN  
               NCEDUM (JBK) = NHBED (NLINK, JBK)  
               FNDUM (JBK) = (asum - DKBED) / KSPDUM (NBK (JBK), &
                NHBED (NLINK, JBK) + 1)
            ELSE  
               NCE = NHBED (NLINK, JBK)  
  104                NCE = NCE-1  
               asum = asum + KSPDUM (NBK (JBK), NCE+1)  
               IF (asum.LE.DKBED) GOTO 104  
               NCEDUM (JBK) = NCE  
               FNDUM (JBK) = (asum - DKBED) / KSPDUM (NBK (JBK), &
                NCE+1)
            ENDIF  
!                             NCEDUM AND FNDUM ARE THE 1ST ESTIMATES
!                             FOR NCEBD AND FNCEBD. THEY ARE THE CORRECT
!                             VALUES FOR A TOTAL BED THICKNESS OF
!                             DBDI METRES. CHANGES ARE MADE LATER SO
!                             THAT A SINGLE OVERLAP NUMBER
!                             AND FRACTION (NOLBD AND FNOLBD) CAN BE
!                             ASSOCIATED WITH THE REGION BELOW THE DEEP
!                             BED.
!                             NB: THIS LONG WINDED APPROACH IS NEEDED
!                             IF ALL THE SOIL IN THE BANKS IS TO BE
!                             ACCOUNTED FOR IN THE CONTAMINANT
!                             CALCULATIONS, SINCE THE SAME ELEVATION IN
!                             ADJACENT BANKS DOES NOT CORRESPOND TO THE
!                             SAME SCALED HEIGHT AT THEIR COMMON FACE
            asum = zero  
            JFCE(JBK) = JA + SIGN(2, 2-JA)  
            NOLP = NOLBT (NBK (JBK), NCEDUM (JBK) + 1, JFCE (JBK) &
             ) - 1
  106             NOLP = NOLP + 1  
            DUM1 = SCL * JOLFN (NBK (JBK), NOLP, JFCE (JBK) )  
            asum = asum + DUM1  
            IF (asum.LE.FNDUM (JBK) ) GOTO 106  
            JOLDUM (JBK) = NOLP - 1  
            FOLDUM (JBK) = (FNDUM (JBK) - asum + DUM1) / DUM1  
!                             OVERLAP NUMBERS AND FRACTIONS ASSOCIATED
!                             WITH THE 1ST ESTIMATES
         ENDIF  
      ENDIF  

  102    END DO  
   DUM1 = DBLE (JOLDUM (1) ) + FNDUM (1)  
   DUM2 = DBLE (JOLDUM (2) ) + FNDUM (2)  
   IF (DUM1.LE.DUM2) THEN  
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
   ENDIF  
   NCDUM = NOLCE (NBK (LDUM), NOLBD, JFCE (LDUM) )  
   NOLDUM = NOLBT (NBK (LDUM), NCDUM + 1, JFCE (LDUM) ) - 1  
!                             HIGHEST OVERLAP ASSOC. WITH NCDUM
   DUM3 = FNOLBD * SCL * DBLE (JOLFN (NBK (LDUM), NOLBD+1, JFCE ( &
    LDUM) ) )
!                             FRACTION OF NEXT HIGHEST CELL COVERED
!                             BY FRACTION OF OVERLAP
   IF (NOLDUM.EQ.NOLBD) THEN  
      NCEBD (NLINK, LDUM) = NCDUM  
      FNCEBD (NLINK, LDUM) = DUM3  
   ELSE  
      NCEBD (NLINK, LDUM) = NCDUM - 1  
      asum = DUM3  
      DO 107 NOLP = NOLBT (NBK (LDUM), NCDUM, JFCE (LDUM) ), &
       NOLBD
         asum = asum + SCL * DBLE (JOLFN (NBK (LDUM), NOLP, JFCE ( &
          LDUM) ) )
  107       END DO  
      FNCEBD (NLINK, LDUM) = asum  

   ENDIF  
!                             SET FINAL VALUES FOR THE OVERLAP NUMBERS
!                             NOLBD AND FRACTIONS FNOLBD FOR THE REGION
!                             BELOW THE DEEP BED; AND SET THE CELL
!                             NUMBERS NCEBD AND FRACTIONS FNCEBD
!                             ACCORDINGLY
   asum = zero  
   DO 108 JBK = 1, 2  
      DO 110 NCE = NCEBD (NLINK, JBK) + 1, NHBED (NLINK, JBK) &
       + 1
         asum = asum + KSPDUM (NBK (JBK), NCE)  
  110       END DO  
      asum = asum - FNCEBD (NLINK, JBK) * KSPDUM (NBK (JBK), &
       NCEBD (NLINK, JBK) + 1)
      asum = asum - (one - FHBED (NLINK, JBK) ) * KSPDUM (NBK ( &
       JBK), NHBED (NLINK, JBK) + 1)
  108    END DO  
   ACPBSG (NLINK) = DBS * CWIDTH (NLINK) / Z2SQ  


   ACPBI (NLINK) = (half * asum * CWIDTH (NLINK) / Z2) - ACPBSG ( &
    NLINK)
!                             SET BED SURFACE LAYER THICKNESS TO DBS
!                             METRES, AND THE COMBINED AREA OF THE
!                             BED SURFACE AND DEEP LAYERS TO THE AREA
!                             ABOVE OVERLAP NOLBD AND FRACTION FNOLBD
!                             (ALL THE BANK SOIL IS ACCOUNTED FOR WITH
!                             THIS APPROACH)
   DO 120 JBK = 1, 2  
!                             uuuuuuu ADJUST TRANSMISIVITIES FOR uuuuuuu
!                             UPSTREAM AND DOWNSTREAM SUBSURFACE FLOW IN
!                             BANKS
      NCE1 = NHBED (NLINK, JBK)  
      DO 122 JA = 1, 4  
         NDUMA = ICMREF (NBK (JBK), JA + 4)  
         IF (NDUMA.NE.0) THEN  
            ITYPEA = ICMREF (NDUMA, 1)  
            IF ( (ITYPEA.EQ.1) .OR. (ITYPEA.EQ.2) ) THEN  
!                             THE ELEMENT UPSTREAM OR DOWNSTREAM FROM
!                             BANK JBK OF LINK NLINK IS ITSELF A BANK:
!                             BANK NUMBER NBKU, WHICH IS BANK JBKU OF
!                             LINK NLINKU
               NOL1 = NOLBT (NBK (JBK), NCE1 + 1, JA) - 1  
               NBKU = NDUMA  
               NLINKU = ICMREF (NBKU, 4)  
               IF (ICMBK (NLINKU, 1) .EQ.NBKU) THEN  
                  JBKU = 1  
               ELSE  
                  JBKU = 2  
               ENDIF  
               NCE2 = NHBED (NLINKU, JBKU)  
               NOL2 = NOLBT (NBKU, NCE2 + 1, ICMREF (NBK (JBK), &
                JA + 8) ) - 1
!                             USE ICMREF SO CORRECT FACE IS FOUND EVEN
!                             IF THE UPSTREAM OR DOWNSTREAM BANK IS
!                             ROUND A CORNER
               NOLX = MIN (NOL1, NOL2)  
!                             NOLX IS THE HIGHEST OVERLAP FOR WHICH THE
!                             LONGITUDINAL TRANSMISIVITY OF THE REGION
!                             UNDER THE CHANNEL SHOULD BE TAKEN INTO
!                             ACCOUNT IN ARRAY JKZCOL
               DUM1 = cellarea (NBK (JBK) ) / CLENTH (NLINK) + cellarea ( &
                NBKU) / CLENTH (NLINKU)
               DUM2 = half * (cellarea (NLINK) / CLENTH (NLINK) &
                + cellarea (NLINKU) / CLENTH (NLINKU) )
               DMULT = DUM1 / (DUM1 + DUM2)  
               DO 126 NOLP = NOLX + 1, NOL (NBK (JBK), JA)  
                  JKZCOL (NBK (JBK), NOLP, JA) = MAX (1, INT ( &
                   DMULT * JKZCOL (NBK (JBK), NOLP, JA) ) )
  126                END DO  
            ENDIF  
         ENDIF  
!                             ADJUST SCALED TRANSMISIVITIES FOR BANKS TO
!                             INCLUDE THE PATHS FOR FLOW BELOW CHANNEL,
!                             IN THE DIRECTION OF CHANNEL
  122       END DO  


  120    END DO  
!                             uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu
   DO 130 JBK = 1, 2  
      NCEAB (NLINK, JBK) = NHBED (NLINK, JBK)  

  130    END DO  
!                             SET THE NUMBER, NCEAB, FOR THE LOWEST
!                             CELL WHICH EXCHANGES GROUND WATER WITH
!                             STREAM WATER





  100 END DO  
!                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
DO 15 NCONT = 1, NCON  
!                             xxxxxxx INITIALISE VARIABLES WHICH xxxxxxx
!                             xxxxxx DEPEND ON CONTAMINANT NUMBER xxxxxx
   CCAPIO (NCONT) = CCAPI (NCONT)  
   IIICFO (NCONT) = IIICF (NCONT)  
!                             SET INITIAL VALUES IN 'OLD' ARRAYS

   15 END DO  
!                             xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
DO 16 NLINK = 1, total_no_links  
!                             ooooooo INITIALISE LINK VARIABLES oooooooo
   ACPSFO (NLINK) = ARXL (NLINK) / Z2SQ  
   ACPBDO (NLINK) = ACPBI (NLINK)  
   DO 17 NCONT = 1, NCON  
      CCCCO (NLINK, NCETOP - 2, NCONT) = CCAPIN (NCONT)  
      CCCCO (NLINK, NCETOP - 1, NCONT) = CCAPIN (NCONT)  
      CCCCO (NLINK, NCETOP, NCONT) = CCAPIN (NCONT)  
      CCCC (NLINK, NCETOP - 2, NCONT) = CCAPIN (NCONT)  
      CCCC (NLINK, NCETOP - 1, NCONT) = CCAPIN (NCONT)  


      CCCC (NLINK, NCETOP, NCONT) = CCAPIN (NCONT)  

   17    END DO  
!                             LINK RELATIVE CONCENTRATIONS ARE STORED IN
!                             CCCC AND CCCCO
   asumK = zero  
   asum = zero  
   DO 160 JBK = 1, 2  
      NDUM = NCEBD (NLINK, JBK) + 1  
      NCE = NDUM  
      DUMK = (one - FNCEBD (NLINK, JBK) ) * KSPDUM (ICMBK ( &
       NLINK, JBK), NCE)
      asumK = asumK + DUMK  
      asum = asum + VSTHE (NCE, NBK (JBK) ) * DUMK  
      DO 162 NCE = NDUM + 1, NHBED (NLINK, JBK)  
         DUMK = KSPDUM (ICMBK (NLINK, JBK), NCE)  
         asumK = asumK + DUMK  
         asum = asum + VSTHE (NCE, NBK (JBK) ) * DUMK  
  162       END DO  
      NCE = NHBED (NLINK, JBK) + 1  
      DUMK = FHBED (NLINK, JBK) * KSPDUM (ICMBK (NLINK, JBK), &
       NCE)
      asumK = asumK + DUMK  
      asum = asum + VSTHE (NCE, NBK (JBK) ) * DUMK  
  160    END DO  
   THBEDO (NLINK) = MIN (PBSED (NLINK), asum / asumK)  

   THBED (NLINK) = THBEDO (NLINK)  
!                             INITIALISE MOISTURE CONTENT IN STREAM BED,
!                             AS THE WEIGHTED AVERAGE FOR THE CELLS, OF
!                             BOTH BANKS, LYING WITHIN THE BED SURFACE
!                             AND BED DEEP LAYER
   ARL = DLS (NLINK) * CWIDTH (NLINK)  
!                             X-SECIONAL AREA OF LOOSE SEDIMENTS IN BED
   ARP = (ACPBI (NLINK) - ACPBSG (NLINK) ) * Z2SQ  
!                             X-SECTIONAL AREA OF NON-ERODED PARENT
!                             MATERIAL WITHIN BED DEEP LAYER
   DUM = one / (ARL + ARP)  
   DO 200 JSED = 1, NSED  
      FBBEDO (NLINK, JSED) = DUM * (ARL * FBETA (NLINK, JSED) &
       + ARP * SOSDFN (NSOBED (NLINK), JSED) )
      FDELO (NLINK, JSED) = FDEL (NLINK, JSED)  
      FBTSDO (NLINK, JSED) = FBTSD (NLINK, JSED)  

  200    END DO  
!                             SET INITIAL VALUES FOR THE PARTICLE SIZE
!                             FRACTIONS IN THE STREAM AND BED



   16 END DO  
!                             oooooooooooooooooooooooooooooooooooooooooo
DO 50 NCL = total_no_links + 1, total_no_elements  
!                             iiiiii INITIALISE COLUMN VARIABLES iiiiiii
   DLSO (NCL) = DLS (NCL)  
   DSWO (NCL) = GETHRF (NCL) - ZGRUND (NCL)  
   GNUO (NCL) = GNU (NCL)  
   QIO (NCL) = - PNETTO (NCL) * cellarea (NCL)  
   QQRFO (NCL) = QVSV (NCOLMB (NCL), NCL) * cellarea (NCL)  
   RSZWLO (NCL) = zero  
!                             MUST BE SET TO 0


   ZONEO (NCL) = (ZGRUND (NCL) - ZCOLMB (NCL) ) / Z2  
   DO 51 JDUM = 1, 2  
      QQQSWO (NCL, JDUM) = - QOC (NCL, JDUM)  
      QQQSWO (NCL, JDUM + 2) = QOC (NCL, JDUM + 2)  


   51    END DO  
!                             NB: INWARDS POSITIVE CONVENTION USED HERE
!                             WHILE X AND Y POSITIVE CONVENTION USED IN
!                             WATER FLOW COMPONENTS
! set up variables for l-shaped bank calculations, if required
   ITYPE = ICMREF (NCL, 1)  
   IF (ITYPE.NE.0) THEN  
!                             ELEMENT IS A BANK
      JBK = ITYPE  
      NLINKA = ICMREF (NCL, 4)  
!                             NUMBER FOR ASSOCIATED LINK
      JAL = 0  
   55       JAL = JAL + 1  
      IF (ICMREF (NLINKA, JAL + 4) .NE.NCL) GOTO 55  
      JFLINK = ICMREF (NLINKA, JAL + 8)  
!                             NUMBER FOR FACE ASSOCIATED WITH LINK
      DBK = cellarea (NCL) / CLENTH (NLINKA)  
      DMULT = DBK / (DBK + half * CWIDTH (NLINKA) )  
      DO 56 NCE = NLYRBT (NCL, 1) - 1, NCEBD (NLINKA, JBK)  
         ROH (NCE) = DMULT  
   56       END DO  
      NCE = NCEBD (NLINKA, JBK) + 1  
      ROH (NCE) = one - (one - DMULT) * FNCEBD (NLINKA, JBK)  
      DO 57 NCE = NCEBD (NLINKA, JBK) + 2, LLEE  
         ROH (NCE) = one  

   57       END DO  

   ENDIF  

   DO 52 NCE = 1, top_cell_no  !LLEE  !JE
      GGAMMO (NCL, NCE) = zero  
      DO 53 JA = 1, 4  
         QQO (NCL, NCE, JA) = QVSH (JA, NCE, NCL)  
   53       END DO  
      DO 54 NCONT = 1, NCON  
         CCCCO (NCL, NCE, NCONT) = CCAPIN (NCONT)  
         SSSSO (NCL, NCE, NCONT) = CCAPIN (NCONT)  
         CCCC (NCL, NCE, NCONT) = CCAPIN (NCONT)  
         SSSS (NCL, NCE, NCONT) = CCAPIN (NCONT)  

   54       END DO  


   52    END DO  
! calculate theta and vert vel for L-shaped bank, if required
   IF (ITYPE.EQ.0) THEN  
      DO 58 NCE = NLYRBT (NCL, 1) - 1, top_cell_no  
         VSTHEO (NCL, NCE) = VSTHE (NCE, NCL)  
         UUAJPO (NCL, NCE) = QVSV (NCE, NCL)  
   58       END DO  
   ELSE  
      NDIFF = NLYRBT (NLINKA, 1) - NLYRBT (NCL, 1)  
      DO 59 NCE = NLYRBT (NCL, 1) - 1, top_cell_no  
         NCEA = NCE+NDIFF  
         IF (NCEA.LE.top_cell_no) THEN  
            VSTHEO (NCL, NCE) = ( (one - ROH (NCE) ) * VSTHE ( &
             NCEA, NLINKA) + ROH (NCE) * VSTHE (NCE, NCL) )
            UUAJPO (NCL, NCE) = ( (one - ROH (NCE) ) * QVSV ( &
             NCEA, NLINKA) + ROH (NCE) * QVSV (NCE, NCL) ) / ROH ( &
             NCE)
         ELSE  
            VSTHEO (NCL, NCE) = VSTHE (NCE, NCL)  
            UUAJPO (NCL, NCE) = QVSV (NCE, NCL)  
         ENDIF  
   59       END DO  


   ENDIF  

   50 END DO  
!     New code by SB
!     --------------
DO 380 NCONT = 1, NCON  
!
   IF (ISCNSV (NCONT) ) THEN  
!
!     Concentrations are spatially variable and the concentration
!     in each cell is calculated by linearly interpolating
!     between values in the depth/conc. tables
      CALL ALINTP (LLEE, NCETOP, total_no_elements, NELEE, total_no_links, NUM_CATEGORIES_TYPES (NCONT), &
       MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCATTY (total_no_links + 1, NCONT), NCOLMB (total_no_links + 1), &
       NTAB (1, NCONT),TABLE_CONCENTRATION (1, 1, NCONT), TABLE_WATER_DEPTH (1, 1, NCONT), &
       DELTAZ, ZVSNOD, CCCC (1, 1, NCONT) )
      DO 385 NCL = total_no_links + 1, total_no_elements  
         DO 390 NCE = NCOLMB (NCL), NCETOP  
            SSSS (NCL, NCE, NCONT) = CCCC (NCL, NCE, NCONT)  
!     ADDED SB 6/3/00
            SSSSO (NCL, NCE, NCONT) = CCCC (NCL, NCE, NCONT)  
            CCCCO (NCL, NCE, NCONT) = CCCC (NCL, NCE, NCONT)  
  390          END DO  
  385       END DO  
   ENDIF  
!

  380 END DO  
!
!     End of new code by SB
!     ---------------------



IF (ISPLT) CALL INPL  
!                       Initialise plant uptake routines
!                                   iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
END SUBROUTINE INCM





!SSSSSS SUBROUTINE INET  
SUBROUTINE INET  
!----------------------------------------------------------------------*
!
!  THIS SUBROUTINE READS IN PARAMETERS REQUIRED FOR THE ET COMPONENT
!  AND CARRIES OUT INITIALISATION CALCULATIONS
!  IT IS ASasumED THAT MET SITE CODES AND VEGETATION CODES HAVE BEEN
!  READ IN THE GLOBAL INITIALISATION ROUTINES
!  VARAIBLE NAMES ARE AS SPECIFIED IN IH SHE REPORT 8, MAY 1978
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/ET/INET/4.2
! Modifications since v3.3:
!  GP          3.4  Don't call METIN (see TMSTEP).
! RAH  941001 3.4.1 Add IMPLICIT DOUBLEPRECISION (see AL.P).
! RAH  970516  4.1  Scrap WEP,WETEX,WETOCE,WEXET,WSET,WSETER,WSETI
!                   (AL.D), DWETER (SPEC.ET) & AKKEP,AKKEA,AKKP (local).
!                   Bring HEAD,ZU,ZD,ZO from SPEC.ET.
!                   HEAD is type CHAR (was DBLE).  Explicit typing.
!                   Scrap SPEC.ET output arrays NUMCST,NUMPLA,NUMCLA,
!                   NUMVHT: use local JJJ.
! RAH  981021  4.2  Scrap AL.D outputs CSTOLD,EPOTR.
!                   Replace VK^^2 with constant VKSQ.
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     SPEC.AL          NUZTAB,NVBP,NVEE
! Input common
!     SPEC.AL          EPD,ETD,MED,NEL,NGDBGN,NM,NRAIN,NV,PRD,PRI
!                      BHOTRD
! Output common
!     SPEC.AL          NRD(NV)
!                      DTMET,EPTIME,METIME,TIMEUZ
!                      CLAI(NV),RDL(NV),   P(NRAIN),CSTORE(NGDBGN:NEL)
!                      PLAI(NV),VHT(NV),PINP(NRAIN),RDF(NVEE,LLEE)
!     SPEC.ET          MEASPE(NM),MODE(NVEE),NF(NV)
!                      MODECS(NV),MODEPL(NV),MODECL(NV),MODEVH(NV)
!                      NCTCST(NV),NCTPLA(NV),NCTCLA(NV),NCTVHT(NV)
!                      CB(NV),CSTCA1(NV),  RA(NV),PLAI1(NV),VHT1(NV)
!                      CK(NV),CSTCAP(NVEE),RC(NVEE),CLAI1(NV),RTOP(NV)
!                      FET(NVEE,NUZTAB),PS1(NVEE,NUZTAB)
!                      RCF(NVEE,NUZTAB)
!                      RELCST(NVEE,NVBP),TIMCST(NVEE,NVBP)
!                      RELPLA(NVEE,NVBP),TIMPLA(NVEE,NVBP)
!                      RELCLA(NVEE,NVBP),TIMCLA(NVEE,NVBP)
!                      RELVHT(NVEE,NVBP),TIMVHT(NVEE,NVBP)
!                      BINETP,BMETAL,BMETP,BAR(NVEE)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     VARIABLE AND UNIT SPECIFICATION
!
!     RA    - AERODYNAMIC RESISTANCE                  SEC/M
!     RC    - STOMATAL RESISTANCE                     SEC/M
!     CSTCAP- CANOPY STORAGE CAPACITY                 MM
!     CSTORE- CANPOY STORAGE                          MM
!     CK    - DRAINAGE PARAMETER                      MM/SEC
!     CB    - DRAINAGE PARAMETER                      1/MM
!     ZO    - ZERO PLANE DISPLACEMENT                 M
!     ZD    - ROUGHNESS HEIGHT                        M
!     ZU    - HEIGHT OF ANEMOMETER                    M
!     PS1   - AVERAGE TENSION                         M
!     RCF   - CORRESPONDING RC                        SEC/M
!     FET   -       -       EA/EP                     NON-DIM
!     RDF   - ROOT DISTRIBUTION FUNCTION              NON-DIM
!     PLAI  - GROUND COVER INDEX                      NON-DIM
!     CPLAI - CANOPY LEAF AREA INDEX                  NON-DIM
!     VHT   - CANOPY HEIGHT                           M
!     MEASPE- = 0 IF POTENTIAL EVAP NOT MEASURED
!             = 1 IF POTENTIAL EVAP IS MEASURED
!     DTMET - TIMESTEP FOR INPUT OF MED DATA          HR
!     DTMET2 - TIMESTEP FOR INPUT OF PRD DATA         HR
!     DTMET3 - TIMESTEP FOR INPUT OF EPD DATA         HR
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! Locals, etc
!INTRINSIC LOG  
DOUBLEPRECISION VKSQ  
PARAMETER (VKSQ = .41D0**2)  
INTEGER :: I, IEL, IIMEAS, J, JJ, JJJ, N1, N2, c5(5), c6(6), spinup, length, hours
DOUBLEPRECISION DEPTH, asum, ZU (NVEE), ZD (NVEE), ZO (NVEE), step, dum, fred(100)

CHARACTER (LEN=80) :: HEAD, cdum
CHARACTER(256)     :: msg2
!----------------------------------------------------------------------*
!
!  INITIAL VALUES
!
DO 10 I = 1, NVEE  
   CSTCAP (I) = 0.  
   RC (I) = 0.  
   BAR (I) = .FALSE.  
   10 MODE (I) = 0
!
!     CHECK IF HOTSTART
!
IF (.NOT.BHOTRD) THEN  
   DO 20 IEL = NGDBGN, total_no_elements  
      CSTORE (IEL) = 0.  
   20    END DO  
ENDIF  
!
DO 40 I = 1, NRAIN  
   !PINP (I) = 0.  
   40 ENDDO !precip_m_per_s(I) = 0.  
precip_m_per_s = 0.
TIMEUZ = 0.  
!
!-----READ PRINTCONTROL PARAMETERS
!:ET1
READ (ETD, 100) HEAD  
READ (ETD, 60) BMETP, BINETP, BMETAL  
   60 FORMAT (3L7)  
!
!-----READ TIMESTEP FOR INPUT OF MET AND RAINDATA,
!          TIMECONSTANT FOR RAINFALL DISTRIBUTION
!:ET3
READ (ETD, 100) HEAD  
!      READ (ETD,70) DTMET,DTMET2,DTMET3
!   70 FORMAT (F7.1)
! sb 300407 convert breakpoint data to regularly spaced data
READ (ETD, * ) DTMET, DTMET2, DTMET3  
!
!-----READ WHETHER POTENTIAL EVAP IS MEASURED AND THEREFORE TO
!        BE READ IN DIRECTLY FOR EACH MET STATION IN TURN.
!        MEASPE = 0 : POTENTIAL EVAP NOT MEASURED
!               = 1 : POTENTIAL EVAP MEASURED
!:ET5
READ (ETD, 100) HEAD  
READ (ETD, 80) (MEASPE (IIMEAS), IIMEAS = 1, NM)  
   80 FORMAT (10I7)  
!
!---------------------------------
!  LOOP ON VEGETATION TYPES....
!---------------------------------
!
DO 430 I = 1, NV  
!
   IF (BINETP) WRITE(PPPRI, 90) I  
   90 FORMAT   ('0'//1X, 'VEGETATION TYPE', I6/1X, 22('*'))  
!:ET7
   READ (ETD, 100) HEAD  
  100 FORMAT   (A)  
   IF (BINETP) WRITE(PPPRI, 110) HEAD  
  110 FORMAT   ('0'//1X, A)  
!-------------------------------------
!  READ PARAMETER DATA
!-------------------------------------
   READ (ETD, 120) BAR (I), RA (I), ZU (I), ZD (I), ZO (I), &
    RC (I), MODE (I), NF (I), PLAI (I), CSTCAP (I), CK (I), &
    CB (I), NRD (I), CLAI (I), VHT (I), RDL (I)
  120 FORMAT   (L7, 5F7.0, I7/I7, 4F7.0, I7, 3F7.0)  
   IF (BINETP) WRITE(PPPRI, 130) MODE (I)  
  130 FORMAT   ('0', 1X, 'ET COMPONENT WITH MODE', I6, 2X, 'OPERATION')  
!
!-----WRITE PARAMETER DATA
   IF (BINETP) WRITE(PPPRI, 140) PLAI (I), CSTCAP (I), CK (I), &
    CB (I), CLAI (I), VHT (I), RDL (I)
  140 FORMAT   ('0', 'PARAMETERS'/1X, 10('*')//10X, 'PLAI', F15.8/10X, &
&         'CSTCAP', F13.8/10X, 'CK', F17.8/10X, 'CB', F17.8/10X, &
&         'CLAI', F15.8/10X, 'VHT', F16.8/10X, 'RDL', F16.8)
  150    IF (BAR (I) .AND.BINETP) WRITE(PPPRI, 160) ZO (I), ZD (I), &
    ZU (I)
  160 FORMAT   (' ', 10X, 'VARIABLE RA WITH'/10X, 'ZO', F17.4/10X, 'ZD', &
&         F18.4/10X, 'ZU', F17.4)
   IF (.NOT.BAR (I) .AND.BINETP) WRITE(PPPRI, 170) RA (I)  
  170 FORMAT   (' ', 10X, 'CONSTANT RA =', F10.4)  
!--------------------------------------------------------
!     READ TABULAR VARIATION OF TIME-VARYING PARAMETERS
!--------------------------------------------------------
!:ET9
   READ (ETD, 100) HEAD  
!
!-----READ MODE: 0=CONSTANT; 1=TIME-VARYING
   READ (ETD, 180) MODECS (I), MODEPL (I), MODECL (I), MODEVH (I)  
  180 FORMAT   (4I7)  
!
!-----CHECK MODE FOR TIME-VARYING CSTCAP
   IF (BINETP) WRITE(PPPRI, 190) I, MODECS (I)  
  190 FORMAT   ('0', 1X, 'MODE FOR CSTCAP FOR VEGETATION', I3, ' IS', &
&         I3, 3X, ' (0=CONSTANT; 1=TIME-VARYING)')
   IF (MODECS (I) .NE.0) THEN  
      NCTCST (I) = 1  
      CSTCA1 (I) = CSTCAP (I)  
!-----READ NUMBER OF VALUES IN CSTCAP VARIATION TABLE
!:ET11(1/4)
      READ (ETD, 100) HEAD  
      READ (ETD, 200) JJJ  
  200 FORMAT        (I7)  
!:ET13(1/4)
      READ (ETD, 100) HEAD  
      IF (BINETP) WRITE(PPPRI, 110) HEAD  
!-----READ TIME-VARYING CSTCAP VALUES
      DO 230 JJ = 1, JJJ  
         READ (ETD, 210) RELCST (I, JJ), TIMCST (I, JJ)  
  210 FORMAT           (2G7.3)  
         IF (BINETP) WRITE(PPPRI, 220) RELCST (I, JJ), TIMCST (I, &
          JJ)
  220 FORMAT           (2G10.3)  
  230       END DO  
   ENDIF  
!
!-----CHECK MODE FOR TIME-VARYING PLAI
   IF (BINETP) WRITE(PPPRI, 250) I, MODEPL (I)  
  250 FORMAT   ('0', 1X, 'MODE FOR PLAI FOR VEGETATION', I3, ' IS', I3, &
&         3X, ' (0=CONSTANT; 1=TIME-VARYING)')
   IF (MODEPL (I) .NE.0) THEN  
      NCTPLA (I) = 1  
      PLAI1 (I) = PLAI (I)  
!
!-----READ NUMBER OF VALUES IN PLAI VARIATION TABLE
!:ET11(2/4)
      READ (ETD, 100) HEAD  
      READ (ETD, 200) JJJ  
!:ET13(2/4)
      READ (ETD, 100) HEAD  
      IF (BINETP) WRITE(PPPRI, 110) HEAD  
!
!-----READ TIME-VARYING PLAI VALUES
      DO 260 JJ = 1, JJJ  
         READ (ETD, 210) RELPLA (I, JJ), TIMPLA (I, JJ)  
         IF (BINETP) WRITE(PPPRI, 220) RELPLA (I, JJ), TIMPLA (I, &
          JJ)
  260       END DO  
   ENDIF  
!
!-----CHECK MODE FOR TIME-VARYING CLAI
   IF (BINETP) WRITE(PPPRI, 280) I, MODECL (I)  
  280 FORMAT   ('0', 1X, 'MODE FOR CLAI FOR VEGETATION', I3, ' IS', I3, &
&         3X, ' (0=CONSTANT; 1=TIME-VARYING)')
   IF (MODECL (I) .NE.0) THEN  
      NCTCLA (I) = 1  
      CLAI1 (I) = CLAI (I)  
!
!-----READ NUMBER OF VALUES IN CLAI VARIATION TABLE
!:ET11(3/4)
      READ (ETD, 100) HEAD  
      READ (ETD, 200) JJJ  
!:ET13(3/4)
      READ (ETD, 100) HEAD  
      IF (BINETP) WRITE(PPPRI, 110) HEAD  
!
!-----READ TIME-VARYING CLAI VALUES
      DO 290 JJ = 1, JJJ  
         READ (ETD, 210) RELCLA (I, JJ), TIMCLA (I, JJ)  
         IF (BINETP) WRITE(PPPRI, 220) RELCLA (I, JJ), TIMCLA (I, &
          JJ)
  290       END DO  
   ENDIF  
!
!-----CHECK MODE FOR TIME-VARYING VHT
   IF (BINETP) WRITE(PPPRI, 310) I, MODEVH (I)  
  310 FORMAT   ('0', 1X, 'MODE FOR VHT FOR VEGETATION', I3, ' IS', I3, &
&         3X, ' (0=CONSTANT; 1=TIME-VARYING)')
   IF (MODEVH (I) .NE.0) THEN  
      NCTVHT (I) = 1  
      VHT1 (I) = VHT (I)  
!
!-----READ NUMBER OF VALUES IN VHT VARIATION TABLE
!:ET11(4/4)
      READ (ETD, 100) HEAD  
      READ (ETD, 200) JJJ  
!:ET13(4/4)
      READ (ETD, 100) HEAD  
      IF (BINETP) WRITE(PPPRI, 110) HEAD  
!
!-----READ TIME-VARYING VHT VALUES
      DO 320 JJ = 1, JJJ  
         READ (ETD, 210) RELVHT (I, JJ), TIMVHT (I, JJ)  
         IF (BINETP) WRITE(PPPRI, 220) RELVHT (I, JJ), TIMVHT (I, &
          JJ)
  320       END DO  


   ENDIF  
!--------------------------------------------------
!     END OF READING TIME-VARYING PARAMETERS
!--------------------------------------------------
!
!-----CHECK MODE FOR EVAPOTRANSPIRATION CALCULATIONS

   IF (MODE (I) .NE.1.AND.MODE (I) .NE.4) THEN  
!---------------------------------------------
!     READ AND WRITE PSI/RCF/FET FUNCTION DATA.
!---------------------------------------------
!:ET15
      READ (ETD, 100) HEAD  
      N1 = NF (I)  
      READ (ETD, 340) (PS1 (I, J), RCF (I, J), FET (I, J), &
       J = 1, N1)
  340 FORMAT        (3F7.2)  
      IF (BINETP) WRITE(PPPRI, 110) HEAD  
      IF (BINETP) WRITE(PPPRI, 350) (PS1 (I, J), RCF (I, J), &
       FET (I, J), J = 1, N1)
  350 FORMAT        (' ', 3F10.2)  
   ELSE  
      WRITE(PPPRI, 370) RC (I)  
  370 FORMAT        (' ', 10X, 'CONSTANT RC =', F10.4)  
   ENDIF  
!
!-----READ AND WRITE ROOT DENSITY FUNCTION DATA
!:ET17
   READ (ETD, 100) HEAD  
! --------------------------------------------------------
!  NOTE THAT IT IS ASasumED HERE THAT DEPTHS CORRESPOND
!  TO THE NODE DEPTHS FOR THE UZ SOLUTION, SO THAT
!  EACH NODE IN THE ROOT ZONE HAS A CORRESPONDING RDF
!  VALUE.  THE VALUES SHOULD BE INPUT FROM THE SURFACE
!  DOWNWARDS.
!---------------------------------------------------------
   IF (BINETP) WRITE(PPPRI, 110) HEAD  
   asum = 0.  
   N2 = NRD (I)  
   DO 400 J = 1, N2  
      READ (ETD, 390) DEPTH, RDF (I, J)  
  390 FORMAT     (2F7.4)  
      IF (BINETP) WRITE(PPPRI, 410) DEPTH, RDF (I, J)  
      asum = asum + RDF (I, J)  
  400    END DO  
   IF (BINETP) WRITE(PPPRI, 420) asum  
  410 FORMAT   (' ', 2F15.6)  
  420 FORMAT   ('0', 1X, 'asum OF RDF VALUES IS', F10.4)  
   IF (BAR (I) ) RTOP (I) = LOG ( (ZU (I) - ZD (I) ) / ZO (I) ) ** &
    2 / VKSQ
!
!-----END OF VEGETATION LOOP
!


  430 END DO  
!-----------------------------------
!     READ IN METEOROLOGICAL DATA
!-----------------------------------
      IF (BMETAL) THEN
        READ (PRD,*,err=567,end=567)
        READ (EPD,*,err=568,end=568)
      ELSE
        READ (MED,*,err=569,end=569)
      ENDIF
      !METIME = 0.0
      !EPTIME = 0.0
      if (ISTA) then 
         READ (TAH,*,err=570,end=570)
         READ (TAL,*,err=571,end=571)
      endif


      RETURN

 567  CALL ERROR(FFFATAL,1063,PPPRI,0,0,  'no data in prd file')
 568  CALL ERROR(FFFATAL,1064,PPPRI,0,0,  'no data in epd file')
 569  CALL ERROR(FFFATAL,1065,PPPRI,0,0,   'no data in med file')
 570  CALL ERROR(FFFATAL,1066,PPPRI,0,0,   'no data in air temp - high file')
 571  CALL ERROR(FFFATAL,1067,PPPRI,0,0,   'no data in air temp - low file')
END SUBROUTINE INET
! 17/7/96
!
!-------------------------



!SSSSSS SUBROUTINE INFR  
SUBROUTINE INFR  
!-------------------------
!
!
!     READ AND INITIALIZE DATA WHICH IS COMMON TO TWO OR MORE COMPONENTS
!        - ORGANISATION AND FILE NOS.        FRD,MED,ETD,UZD,OCD,SZD,
!                                            SMD,PRI,RES,HOT,SED
!        - JOB TITLE.
!        - MODEL SIZE.                       NX,NY
!         - START TIME OF SIMULATION
! ISYEAR,ISMTH,ISDAY,ISHOUR,ISMIN
!         - END   TIME OF SIMULATION
! IEYEAR,IEMTH,IEDAY,IEHOUR,IEMIN
!        - H-H GRID SIZES IN X-DIRECTION.    DXIN
!        - H-H GRID SIZES IN Y-DIRECTION.    DYIN
!        - PRINTING CONTROL.                 DTAO,IAOUT,BINFRP,
!                                            BFRTS1,BFRTS2,BSTORE
!        - CONTROLS FOR SELECTION OF         BPPNET,BPEPOT,BPQOC,
!            RESULTS TO BE PRINTED           BPDEP,BPQF,BPQH,BPQSZ,
!                                            BPHSZ,BPBAL
!        - COMPONENT EXECUTION CONTROL.      BEXET,BEXUZ,BEXOC,BEXSZ,
!                                            BEXSM
!        - NO. OF METEOROLOGICAL SITES,
!             RAINFALL STATIONS,
!             VEGETATION TYPES AND SOIL
!             TYPES.                         NM,NRAIN,NV,NS
!        - RIVER LINING PARAMETERS.          BLOWP,DB,CCB
!        - DEFAULT MET- VEG- AND SOILCODES   IDMC,IDRA,IDVE,IDS1,IDS2
!        - GROUND SURFACE LEVEL.             ZGRUND
!        - IMPERMEABLE BED LEVEL.            ZBED
!        - METEOROLOGICAL SITE CODES.        NMC
!        - RAINFALL STATION CODES.           NRAINC
!        - VEGETATION CODES.                 NVC
!        - SOIL CODES - UNDER ROOT ZONE.     NSC1
!        - SOIL CODES - ROOT ZONE.           NSC2
!        - GRID CODE FOR UZ, SZ AND FRAME    INGRID

INTEGER :: nxplus, isyear, ismth, isday, ishour, ismin, ieyear, iemth, ieday, iehour, iemin, &
           jsyear, jsmth, jsday, jshour, jsmin, jcyear, jcmth, jcday, jchour, jcmin, j, k, &
           nlyrct, ipr, idmc, idra, idve, idlyr, i1, i2, i, ipflg, iel
DOUBLEPRECISION :: tthx
WRITE(PPPRI, 10)  
   10 FORMAT ('1',// T10, '                                E'/T10, &
& &
' EUROPEAN HYDROLOGIC SYSTEM  S  H  E  SYSTEME HYDROLOGIQUE EUROPEEN'/T10, '                                S' /)
!
! PRINT THE CURRENT VERSION NUMBER
!
IF (BDEVER) THEN  
   WRITE(PPPRI, 16) SHEVER  
ELSE  
   WRITE(PPPRI, 15) SHEVER  
ENDIF  
   16 FORMAT (/  'SHETRAN VERSION NUMBER: ', F5.1 , &
&        ' ' )
   15 FORMAT (/  'SHETRAN VERSION NUMBER: ', F5.1 )  
WRITE(PPPRI, 17) BANNER  
   17 FORMAT(/A80/)  
!
!     READ AND PRINT JOB TITLE.
!:FR1
READ (FRD, 30) TITLE  
   30 FORMAT (20A4)  
WRITE(PPPRI, 40) TITLE  
   40 FORMAT (/  20A4, //, 100('='))  
!
WRITE(PPPRI, 20)  
   20 FORMAT (/ ' ^^^ ENTER INFR ^^^')  
!
!     READ AND PRINT MODEL SIZE, TOTAL SIMULATION TIME, GRID SIZES AND
!        PRINTING CONTROL.
!:FR2
READ (FRD, * )  
READ (FRD, * ) NX, NY  
NXPLUS = 0  
!:FR4
READ (FRD, * )  
READ (FRD, * ) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN  
!:FR6
READ (FRD, * )  
READ (FRD, * ) IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN  
!
! READ START TIMES FOR SEDIMENT AND CONTAMINANT COMPONENTS
!
!:FR7a
READ (FRD, * )  
READ (FRD, * ) JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN  
!:FR7c
READ (FRD, * )  
READ (FRD, * ) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN  
!
NXM1 = NX - 1  
NYM1 = NY - 1  
NXP1 = NX + 1  
NYP1 = NY + 1  
!:FR8
READ (FRD, 30) TITLE  
READ (FRD, 50) (DXIN (J), J = 1, NXM1)  
!:FR10
READ (FRD, 30) TITLE  
READ (FRD, 50) (DYIN (K), K = 1, NYM1)  
   50 FORMAT (10F7.0)  
!
!:FR12
READ (FRD, 30) TITLE  
READ (FRD, 80) DTAO, IAOUT, BINFRP, BFRTS1, BFRTS2, BSTORE, &
 PSTART
   80 FORMAT (F7.0, I7, 4L7, F7.0)  
!:FR20
READ (FRD, 30) TITLE  
READ (FRD,85) PMAX, PALFA, QMAX, TMAX, BSOFT 

   85 FORMAT(4F7.0,L7)  
!PMAX = one  
!PALFA = 0.15D0  
IF (TMAX.GT.two) THEN  
   WRITE(PPPRI, * ) '^^^ TIMESTEP LIMITED TO 2 HOURS ^^^'  
   TMAX = two  
ENDIF  
!
PREST = (one + PALFA)  
!
IF (IAOUT.EQ.2) THEN  
!:FR22
   READ (FRD, 30) TITLE  
   READ (FRD, 100) BPPNET, BPEPOT, BPQOC, BPDEP, BPQF, BPQH, &
    BPQSZ, BPHSZ, BPBAL, BPSD
  100 FORMAT     (10L7)  
ENDIF  
!
!---- BEX** = TRUE FOR EXECUTION AND FALSE FOR NO EXECUTION
!     NOTE: COMPONENTS FR,ET,UZ,OC,SZ,EX ARE ALWAYS INCLUDED
!
!:FR24
READ (FRD, 30) TITLE  
READ (FRD, 130) BEXSM, BEXBK, BEXSY, BEXCM  
BEXET = .TRUE.  
BEXUZ = .TRUE.  
BEXOC = .TRUE.  
BEXSZ = .TRUE.  
BEXEX = .TRUE.  
!
!     LOGICAL PARAMETERS FOR HOT START
!
!:FR26
READ (FRD, 30) TITLE  
READ (FRD, 140) BHOTRD, BHOTPR, BHOTTI, BHOTST  
!
! PRINT INITIALISATION DATA
!
  130 FORMAT (10L7)  
  140 FORMAT (2L7, 2F7.2)  
WRITE(PPPRI, 150) NX, NY  
  150 FORMAT ('0'//, ' GRID SPECIFICATION'/80('*')//, ' NX = ', I4, &
&       21X, 'NY = ', I4)
WRITE(PPPRI, 160) (DXIN (J), J = 1, NXM1)  
  160 FORMAT ('0', 'H-H GRID SIZES (METERS) IN X-DIRECTION', /, &
&       (1X,10G11.4))
WRITE(PPPRI, 170) (DYIN (K), K = 1, NYM1)  
  170 FORMAT ('0', 'H-H GRID SIZES (METERS) IN Y-DIRECTION', /, &
&       (1X,10G11.4))
WRITE(PPPRI, 200)  
  200 FORMAT (' ', 80('*'))  
!
!     CONVERT STARTTIME AND ENDTIME TO HOURS.
TIH = HOUR_FROM_DATE(ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN)  
TTH = HOUR_FROM_DATE(IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN)  
TTHX = TTH - TIH  
WRITE(PPPRI, 210) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN, IEYEAR, &
 IEMTH, IEDAY, IEHOUR, IEMIN, TTHX
  210 FORMAT ('0'//, ' START OF SIMULATION  : ', 5I6, /, &
&               ' END OF SIMULATION    : ', 5I6, /, &
&       ' LENGTH OF SIMULATION : ', F10.2, ' HOURS.')
!
! store start time for mass balance
mbyear = isyear  
mbmon = ismth  

mbday = isday  
IF (BEXSY) THEN  
   TSH = HOUR_FROM_DATE(JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN)  
   WRITE(PPPRI, 211) JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN, (TSH - &
    TIH)
  211 FORMAT  (// ' START OF SEDIMENT SIMULATION  : ',5I6, / &
&            '           AT SIMULATION HOUR  : ',F8.2)
ENDIF  
IF (BEXCM) THEN  
   TCH = HOUR_FROM_DATE(JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN)  
   WRITE(PPPRI, 212) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN, (TCH - &
    TIH)
  212 FORMAT  (// ' START OF CONTAMINANT SIMULATION  : ',5I6, / &
&            '              AT SIMULATION HOUR  : ',F8.2)
ENDIF  
!
WRITE(PPPRI, 215) TMAX  
  215 FORMAT ('0',//, ' BASIC TIMESTEP (HOURS) :', F8.3)  
!
WRITE(PPPRI, 220) DTAO  
  220 FORMAT ('0'//, ' PRINTING CONTROL - ALL RESULTS PRINTED AT', &
&       ' INTERVALS OF DTAO = ', F7.2, ' HOURS.')
!
IF (.NOT.BSTORE) WRITE(PPPRI, 230)  
  230 FORMAT ('0'//, ' RESULTS NOT REQUIRED ON FILE STORE.')  
!
IF (BSTORE) WRITE(PPPRI, 240)  
  240 FORMAT ('0'//, ' RESULTS RECORDED ON FILE STORE.')  
!
!     READ AND PRINT NM,NRAIN,NV AND NS.
!:FR28
READ (FRD, 30) TITLE  
READ (FRD, 250) NM, NRAIN, NV, NS, NLYRCT  
  250 FORMAT (5I7)  
WRITE(PPPRI, 260) NM, NRAIN, NV, NS, NLYRCT  
  260 FORMAT ('0'//, ' NO. OF METEOROLOGICAL SITES = ', I3, /, &
&       ' NO. OF RAINFALL STATIONS = ', I3, /, &
&       ' NO. OF VEGETATION TYPES = ', I3, /, &
&       ' NO. OF SOIL TYPES = ', I3, /, &
&       ' NO. OF SOIL HORIZON CATEGORIES = ', I3)
!
!     READ RIVER LINING PARAMETERS.  BLOWP,DB,CCB,BEXTS1
!:FR30
READ (FRD, 30) TITLE  
read (frd, * )  
!c      READ (FRD,270) BLOWP, DB, CCB, BEXTS1
!c  270 FORMAT (L7, 2F7.0, L7)
!c      WRITE(PPPRI,280) BLOWP, DB, CCB, BEXTS1
!c  280 FORMAT ('0'//, ' RIVER LOW PERMEABILITY LINING PARAMETERS.', /,
!c     1       ' BLOWP=', L1, 5X, 'LINING THICKNESS (DB) =', F7.2,
!c     2     ' METERS.', 5X, 'PERMEABILITY (CCB) =', E13.6, ' M/DAY.', /
!c     3       , ' BEXTS1=', L7)
!     CONVERT CCB TO M/SEC
!c      CCB = CCB / 86400.
!
!     SET PRINTING CONTROL FOR SUBROUTINES AREADR AND AREADI.
IPR = 0  
IF (BINFRP) IPR = 1  
!
!     READ DEFAULT VALUES FOR MET,RAIN,VEG,SOIL-CODES. APPLIED WHEN > 0
!:FR32
READ (FRD, 30) TITLE  
READ (FRD, 290) IDMC, IDRA, IDVE, IDLYR  
  290 FORMAT (6I7)  
WRITE(PPPRI, 300) IDMC, IDRA, IDVE, IDLYR  
  300 FORMAT ('0', /, ' DEFAULT METEOROLOGICAL STATION CODE =', I3, /, &
&       1X, 'DEFAULT RAINFALL STATION CODE       =', I3, /, &
&       1X, 'DEFAULT VEGETATION GRID CODE        =', I3, /, &
&       1X, 'DEFAULT SOIL HORIZON CATEGORY CODE  =', I3)
!
! READ IN MAIN CATCHMENT DEFINITION ARRAY, INGRID
! (NB. THIS IS NOT READ IN USING AREAD ROUTINES, AS THE
! INDEX ARRAY ICMREF HASN'T BEEN SET UP YET)
!
!:FR34
READ (FRD, 30) TITLE  
IF (BINFRP) WRITE(PPPRI, 303) TITLE  
  303 FORMAT(/ 20A4)  
!
DO 310 I1 = 1, NY  
   K = NY + 1 - I1  
   READ (FRD, 306) I2, (INGRID (J, K), J = 1, NX)  
   IF (BINFRP) WRITE(PPPRI, 306) I2, (INGRID (J, K), J = 1, NX)  
  306 FORMAT  (I7, 1X, 500I1)  
   IF (I2.NE.K) GOTO 312  
  310 END DO  
GOTO 316  
!
!^^^^^^ERROR IN DATA
!
  312 CONTINUE  
WRITE(PPPRI, 314) TITLE, I2  
  314 FORMAT (//2X, 'ERROR IN DATA ', 20A4, //2X, 'IN THE VICINITY OF ', &
&       'LINE K= ', I5)
STOP  
!
! SET INGRID TO BE ITS INTERNAL VALUES FOR SHE (=0 IN CATCHMENT, -1 OTHE
!
  316 DO 320 I = 1, NX  
   DO 320 J = 1, NY  
      IF (INGRID (I, J) .EQ.1) THEN  
         INGRID (I, J) = 0  
      ELSE  
         INGRID (I, J) = - 1  
      ENDIF  
  320 CONTINUE  
!
! READ THE CODES FOR OVERLAND/CHANNEL FLOW GRID BOUNDARIES
!
!:FR35a
CALL OCLTL (NXP1, NY, LCODEX, NXE, NYE, FRD, PPPRI, BINFRP)  
!:FR35c
CALL OCLTL (NX, NYP1, LCODEY, NXE, NYE, FRD, PPPRI, BINFRP)  
!
! INITIALISE GLOBAL INDEX ARRAY
!
CALL FRIND (BINFRP)  
!
!     READ / PRINT ARRAYS ZGRUND, NMC, NRAIN, NVC.
!     SET EQUAL TO DEFAULT VALUES IF THESE ARE TO BE USED.
!
!:FR37
CALL AREADR (ZGRUND, IPR, FRD, PPPRI)  
!
IPFLG = 3  
!:FR43
IF (IDMC.GT.0) CALL AREADI (NMC, IPFLG, IDMC, PPPRI, NM)  
IF (IDMC.LE.0) CALL AREADI (NMC, IPR, FRD, PPPRI, NM)  
!:FR46
IF (IDRA.GT.0) CALL AREADI (NRAINC, IPFLG, IDRA, PPPRI, NRAIN)  
IF (IDRA.LE.0) CALL AREADI (NRAINC, IPR, FRD, PPPRI, NRAIN)  
!:FR49
IF (IDVE.GT.0) CALL AREADI (NVC, IPFLG, IDVE, PPPRI, NV)  

IF (IDVE.LE.0) CALL AREADI (NVC, IPR, FRD, PPPRI, NV)  

!:FR52
READ (FRD, 30,err=958,end=958) TITLE  
READ (FRD, *,err=958,end=958) TOUTPUT

goto 959
    
958 toutput=24.0
!     INITIALIZATION OF SOME PARAMETERS.
!
959 ALLOUT = DTAO + PSTART  
NXEP1 = NXE+1  
NYEP1 = NYE+1  
!
! INITIALISATION OF ISORT ARRAY
!
DO 425 IEL = 1, total_no_elements  
  425 ISORT (IEL) = IEL  
!
WRITE(PPPRI, 430)  
  430 FORMAT ('0'//, ' EXIT INFR')  
!
RETURN  

END SUBROUTINE INFR
! ######################################################################
! #                                                                    #
! #                                S                                   #
! # EUROPEAN HYDROLOGIC SYSTEM  S  H  E  SYSTEME HYDROLOGIQUE EUROPEEN #
! #                                S                                   #
! #                                                                    #
! #                            SHETRAN-UK                              #
! #           PLANT COMTAMINANT MIGRATION COMPONENT (MPL)              #
! #                                                                    #
! ######################################################################
!
!
!----------------PROGRAM AMENDMENT HISTORY----------------------------
!
! AMENDED BY   DATE   VERSION   REASON FOR AMENDMENT
! ----------  ------  -------   --------------------
!    JE       18/3/93  3.4      IMPLEMENTATION
!---------------------------------------------------------------------
!




!SSSSSS SUBROUTINE INPL  
SUBROUTINE INPL  
!                 Initialisation subroutine for contaminant plant uptake

INTEGER :: ncl, jplant, jplty, nce, ndum
DOUBLEPRECISION :: d1dum, rdum
!                 Include parameter statements, water/contaminant
!                 interface COMMON blocks, and plant COMMON blocks
NPLT = NV  
!                 Number of top cell in column, and number of plant
!                 types
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ gp 30/3/93
pmass (1) = two  
pmass (2) = 3.0d0  
pmass (3) = 20.0d0  
pf2max (1) = two  
pf2max (2) = 6.0d0  
pf2max (3) = 10.0d0  
pkmax (1, 1) = 1.5d-8  
pkmax (2, 1) = 3.0d-8  


pkmax (3, 1) = 3.0d-8  
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ temp. for dsatd2


DO 100 NCL = total_no_links + 1, total_no_elements  
   NPLTYP (NCL, 1) = NVC (NCL)  
   PFONE (NCL, 1) = PLAI (NPLTYP (NCL, 1) )  
   IF (PFONE (NCL, 1) .GE.0.99) THEN  
      NPL (NCL) = 1  
   ELSE  
      PFONE (NCL, 2) = one - PFONE (NCL, 1)  
      NPL (NCL) = 2  


   ENDIF  
! ^^^^^^^^^^^^^^^ TEMPORARY
!                 Set number of plant types on each column
!                 Temporarily, only two plant types are allowed on each
!                 column and the total PLAI is one
!                 Second plant type number is set in BLOCK DATA
   DO 200 JPLANT = 1, NPL (NCL)  

      JPLTY = NPLTYP (NCL, JPLANT)  
!                 Plant type number
      DO 210 NCE = NCETOP, 2, - 1  
         NDUM = NCETOP - NCE+1  
         PDZF3 (NCL, NCE, JPLANT) = RDF (JPLTY, NDUM)  

  210       END DO  
!                 Set root density function
      D1DUM = DELONE (JPLTY)  
      RDUM = CLAI (JPLTY) / PF2MAX (JPLTY)  



      GMCBBO (NCL, JPLANT) = RDUM * D1DUM  
!                 Initialise old value for mass in compartment b

  200    END DO  

  100 END DO  
RETURN  
END SUBROUTINE INPL


! 30/1/96
! ######################################################################
! #
! #                                 E
! # EUROPEAN HYDROLOGIC SYSTEM   S  H  E   SYSTEME HYDROLOGIQUE EUROPEEN
! #                                 S
! #
! #                             SHETRAN-UK
! #                       SNOWMELT COMPONENT (SM)
! #
! ######################################################################
!
!     CREATED:  MARCH 1981 BY JCB
!
!----------------PROGRAM AMENDMENT HISTORY----------------------------
!
! AMENDED BY   DATE   VERSION   REASON FOR AMENDMENT
! ----------  ------  -------   --------------------
!   GP        SEP 89    2.1     'SHE88' IMPLEMENTATION ON NEWCASTLE AMDA
!   GP        JUN 90    2.2     AMENDMENTS FOR VARIABLE SNOWPACK
!                               + CORRECTION FOR LOW TEMPS. AND SHALLOW
!                               + REMOVE NAMELISTS
!                               + SHETRAN AMENDMENTS
!   SPA       NOV 92    3.?     1) REMOVE INCORRECT SNOWPACK TEMPERATURE
!                               CONTROL WHEN USING DEGREE DAY METHOD
!                               2) FURTHER CORRECTION FOR LOW TEMPS. AND
!                               SHALLOW PACK
!                               3) REMOVAL OF SNOWPACK DEPTH FROM CALC O
!                               PROFILE TO PREVENT LN OF ZERO OR -VE NO.
!
!---------------------------------------------------------------------
!
!



!SSSSSS SUBROUTINE INSM  
SUBROUTINE INSM  
!
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!  THIS SUBROUTINE READS IN THE PARAMETERS REQUIRED FOR THE
!  SNOWMELT COMPONENT AND CARRIES OUT INITIALISATION
!  CALCULATIONS.
!
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
 
INTEGER :: n, iel, i
DOUBLEPRECISION :: tsin, unifsd
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     VARIABLE AND UNIT SPECIFICATION
!     UNIFSD- SNOWDEPTH IF UNIFORM (MM OF SNOW)           MM
!     SD    - SNOWDEPTH (MM OF SNOW)                      MM
!     DDF   - DEGREE DAY FACTOR                           MM/S/C
!     RHOS  - SPECIFIC GRAVITY OF SNOW                    --
!     TSIN  - INITIAL TEMPERATURE OF SNOW                 C
!     TS    - TEMPERATURE OF SNOW                         C
!     NSMC  - COUNTER USED IN ROUTING MELTWATER
!               THROUGH SNOWPACK. EQUALS NUMBER OF
!               SLUGS OF MELTWATER MOVING THROUGH SNOWPACK--
!     MSM   - EQUALS 1 FOR DEGREE DAY
!                    2 FOR ENERGY BUDGET                  --
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!
!         READ PRINT CONTROL PARAMETERS
READ (SMD, 700) HEAD  
  700 FORMAT(20A4)  
READ (SMD, 708) BINSMP  
  708 FORMAT(L7)  
IF (BINSMP) WRITE(PPPRI, 800) HEAD  
  800 FORMAT(1H0//1X,20A4)  
!
!         READ SNOWMELT DATA
READ (SMD, 700) HEAD  
READ (SMD, 701) DDF, RHOS, TSIN, NSD, MSM  
  701 FORMAT(2F7.5,F7.2,2I7)  
RHODEF = RHOS  
!         Added by spa, 05/11/92.  Snowpack temp no longer needed
!         for degree day method.  Therefore if msm=1, tsin=0.
if (msm.eq.1) tsin = zero  
IF (BINSMP) WRITE(PPPRI, 801) DDF, RHOS, TSIN, MSM  
  801 FORMAT(1H0,'DEGREE DAY FACTOR DDF =',F7.5,1X,'MM/S/C', &
& 5X,'SNOW SPECIFIC GRAVITY RHOS =',F7.5/ &
& 5X,'INITIAL SNOW TEMPERATURE =',F7.2,1X,'C'/ &
& 5X,'SNOWMELT CALCULATED BY DEGREE DAY IF MSM IS 1', &
& ' AND BY ENERGY BUDGET IF MSM IS 2',5X,'MSM =',I3)
!
IF (MSM.EQ.1) GOTO 710  
!        READ ENERGY BUDGET DATA
READ (SMD, 700) HEAD  
READ (SMD, 709) ZOS, ZDS, ZUS  
  709 FORMAT(3F7.5)  
IF (BINSMP) WRITE(PPPRI, 803) ZOS, ZDS, ZUS  
  803 FORMAT(1H0,'ENERGY BUDGET DATA',3X,'ROUGHNESS ZOS =',F7.5,1X,'M'/ &
&    21X,'ZERO PLANE DISPLACEMENT ZDS =',F7.5,1X,'M'/ &
&    21X,'HEIGHT OF ANEMOMETER ZUS =',F7.5,1X,'M')
!
!         METEOROLOGICAL (WINDSPEED) DATA LOCATION
!
READ (SMD, 700) HEAD  
READ (SMD, 720) (IMET (N), N = 1, NM)  
  720 FORMAT(10I7)  
IF (BINSMP) THEN  
   WRITE(PPPRI, 715)  
  715 FORMAT  (/' LOCATION OF MET. STATIONS: ' / &
&    ' STATION NO.    ELEMENT NO.')
   DO 730 N = 1, NM  
      WRITE(PPPRI, 735) N, IMET (N)  
  735 FORMAT    (3X,I4,10X,I4)  
  730    END DO  
ENDIF  
!
!         IS SNOWDEPTH UNIFORM?
!
  710 IF (NSD.EQ.0) then  
   do 712 iel = ngdbgn, total_no_elements  
      rhosar (iel) = rhodef  
  712    end do  
   GOTO 703  
endif  
!
!         NONUNIFORM SNOWDEPTH (MM OF SNOW)
I = 0  
IF (BINSMP) I = 1  
CALL AREADR (SD, I, SMD, PPPRI)  
CALL AREADR (RHOSAR, I, SMD, PPPRI)  
GOTO 704  
!
!         UNIFORM SNOWDEPTH (MM OF SNOW)
  703 READ (SMD, 700) HEAD  
READ (SMD, 705) UNIFSD  
  705 FORMAT(F7.1)  
DO 706 IEL = NGDBGN, total_no_elements  
   SD (IEL) = UNIFSD  
  706 END DO  
IF (BINSMP) WRITE(PPPRI, 802) UNIFSD  
  802 FORMAT(1H0,1X,'INITIAL SNOWPACK HAS UNIFORM THICKNESS =', &
& F7.1,1X,'MM')
  704 DO 707 IEL = NGDBGN, total_no_elements  
!                  SET COUNTER FOR SNOWMELT ROUTINE
   NSMC (IEL) = 0  
!                  SET SNOW TEMPERATURES
   TS (IEL) = TSIN  
!                  SET SNOWFALL
   SF (IEL) = zero  
  707 END DO  
RETURN  
END SUBROUTINE INSM




!SSSSSS SUBROUTINE DINET  
SUBROUTINE DINET  
!
!
WRITE ( *, 1)  
    1 FORMAT(// 'ENTER DINET')  
BMETAL = .TRUE.  
!     PNET=0.0003
!     PE=0.0
!     EINT=0.0
!     ERZ=0.0
!     DRAIN=0.0
!     ESOIL=0.0
RETURN  
END SUBROUTINE DINET
! ######################################################################
! #
! #                                 S
! # EUROPEAN HYDROLOGIC SYSTEM   S  H  E   SYSTEME HYDROLOGIQUE EUROPEEN
! #                                 E
! #
! #                             SHETRAN-UK
! #                        DUMMY COMPONENTS (DUM)
! #
! ######################################################################
!
!  CONTAINS DUMMY VERSIONS OF OC-ET-UZ-SZ-EXSZOC  (NOT CURRENTLY USED)
!
!----------------PROGRAM AMENDMENT HISTORY----------------------------
!
! AMENDED BY   DATE   VERSION   REASON FOR AMENDMENT
! ----------  ------  -------   --------------------
!   GP        JUN 89    0.1     USE WITH V-CATCHMENT TESTS
!   GP        DEC 91    0.2     MINIMAL VERSIONS (NOT CURRENTLY USED)
!
!---------------------------------------------------------------------
!



!SSSSSS SUBROUTINE DINOC  
SUBROUTINE DINOC  
!
  
!
WRITE ( *, 1)  
    1 FORMAT(// 'ENTER DINOC')  
RETURN  
END SUBROUTINE DINOC
!



!SSSSSS SUBROUTINE DOCIN  
SUBROUTINE DOCIN  
!
  
!
RETURN  
END SUBROUTINE DOCIN




!SSSSSS SUBROUTINE MUERR2 (CPR, NEL, NELEE, NLF, MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCON, &
SUBROUTINE MUERR2 (CPR, total_no_elements, NELEE, total_no_links,      &
    MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCON, NCONEE, NUM_CATEGORIES_TYPES,  NTAB, NCATTY, ISCNSV,          &
    TABLE_CONCENTRATION, TABLE_WATER_DEPTH, LDUM)
!
!--------------------------------------------------------------------*
!
! Checks data that is used to calculate the spatially variable
! contaminant concentrations for grid and bank elements
!
!--------------------------------------------------------------------*
! Version: 4.2                   Notes:
! Module: CM                 Program: SHETRAN
! Modifications
! Notes: The checking works. However, it is done in a poor way.
! In future this should be changed
!--------------------------------------------------------------------*
!
INTEGER :: CPR, total_no_elements, NELEE, total_no_links  
INTEGER :: MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS  
INTEGER :: NCON, NCONEE  
INTEGER :: NUM_CATEGORIES_TYPES (NCONEE), NTAB (MAX_NUM_CATEGORY_TYPES, NCONEE)  
INTEGER :: NCATTY (NELEE, NCONEE)  
LOGICAL :: ISCNSV (NCONEE)  
DOUBLEPRECISION TABLE_CONCENTRATION (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCONEE)  

DOUBLEPRECISION TABLE_WATER_DEPTH (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCONEE)  
! WORKSPACE ARGUMENTS

LOGICAL :: LDUM  
!
! LOCALS ETC.
INTEGER :: ICOL1, IUNDEF, NERR, NELMTY, NTBL, I, J  
INTEGER :: IZERO (1)  
DOUBLEPRECISION PREVDP  
!

DATA IZERO / 0 /  
!
!
! 0. Preliminaries
! ----------------
!
!  Initialize local counter
NERR = 0  
!  Position of 1st column element


ICOL1 = total_no_links + 1  
! 1. Check the data used to calculate the spatially variable
! contamianant concentrations
! -------------------------------------------------------
!

DO 100 I = 1, NCON  

   IF (ISCNSV (I) ) THEN  
      DO 110 J = ICOL1, total_no_elements  
!       *NCATTY
         CALL ALCHKI(EEERR, 2103, CPR, J, J, IUNDEF, IUNDEF, &
          'NCATTY(iel)', 'GT', IZERO, NCATTY (J, I) , NERR, LDUM)
  110       END DO  
!
!       *TABLE_WATER_DEPTH
!       The table of depths must have a first depth equal to zero,
!       thereafter the depth must increase
!
      DO 160 NELMTY = 1, NUM_CATEGORIES_TYPES (I)  
         CALL ALCHK(EEERR, 2104, CPR, NELMTY, NELMTY, 1, IUNDEF, &
          'TABLE_WATER_DEPTH[NUM_CATEGORIES_TYPES,1]', 'EQ', ZERO1, ZERO , TABLE_WATER_DEPTH (NELMTY, 1, &
          I) , NERR, LDUM)
         DO 170 NTBL = 2, NTAB (NELMTY, I)  
            PREVDP = TABLE_WATER_DEPTH (NELMTY, NTBL - 1, I)  
            CALL ALCHK(EEERR, 2105, CPR, NELMTY, NELMTY, NTBL, &
             IUNDEF, 'TABLE_WATER_DEPTH[NUM_CATEGORIES_TYPES,ntab]', 'GT', (/PREVDP/) , &
             ZERO , TABLE_WATER_DEPTH (NELMTY, NTBL, I) , NERR, LDUM)
  170          END DO  
  160       END DO  
!
!       *TABLE_CONCENTRATION
!       Each value in the table of concentrations must be >= 0
!
      DO 260 NELMTY = 1, NUM_CATEGORIES_TYPES (I)  
         DO 270 NTBL = 1, NTAB (NELMTY, I)  
            CALL ALCHK(EEERR, 2106, CPR, NELMTY, NELMTY, NTBL, &
             IUNDEF, 'TABLE_CONCENTRATION[nmne,ntab]', 'GE', zero1, zero , &
            TABLE_CONCENTRATION (NELMTY, NTBL, I) , NERR, LDUM)
  270          END DO  

  260       END DO  
   ENDIF  
!

  100 END DO  
! 2. Epilogue
! -----------
!
IF (NERR.GT.0) CALL ERROR(FFFATAL, 2107, CPR, 0, 0, 'Error(s) detected while checking static/initial interface')
!






END SUBROUTINE MUERR2
END MODULE FRmod