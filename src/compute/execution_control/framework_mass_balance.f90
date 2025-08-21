MODULE framework_mass_balance
! Module for framework mass balance functionality
! Extracted from FRmod.f90 as part of refactoring

   USE framework_output_manager, ONLY : FRRESP
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

   ! Module variables for mass balance functionality
   DOUBLEPRECISION :: TIMB=zero
   LOGICAL         :: FIRST_frmb=.TRUE.

   PUBLIC :: FRMB

CONTAINS

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

100   END DO
!     * variable 6 (and 12)
      DISCHM = ZERO

      IF (MBLINK.NE.0) DISCHM = ABS (QOC (MBLINK, MBFACE) * DTUZ)
!     * variable 18 (and 19)
      BFLOW = ZERO
      DO 120 IL = 1, total_no_links
         QBK = QBKB (IL, 1) + QBKB (IL, 2) + QBKF (IL, 1) + QBKF (IL, 2)
         BFLOW = BFLOW + QBK * DTUZ


120   END DO
! Update BALANC (note: elements 1:6 & 18 may be reset to zero below)
      DO 150 I = 0, 6, 6
         BALANC (I + 1) = BALANC (I + 1) + PRECM
         BALANC (I + 2) = BALANC (I + 2) + CEVAPM
         BALANC (I + 3) = BALANC (I + 3) + SEVAPM
         BALANC (I + 4) = BALANC (I + 4) + TRANSM
         BALANC (I + 5) = BALANC (I + 5) + AQFLXM
         BALANC (I + 6) = BALANC (I + 6) + DISCHM
         BALANC (18 + I / 6) = BALANC (18 + I / 6) + BFLOW



150   END DO
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
210      END DO
215   END DO
      DO 220 IL = 1, total_no_links
         BALANC (17) = BALANC (17) + ARXL (IL) * CLENTH (IL)


220   END DO
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

END MODULE framework_mass_balance
