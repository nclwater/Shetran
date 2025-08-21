MODULE framework_output_manager
! Module for framework output manager functionality
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

   ! Module variables for output manager functionality
   INTEGER, SAVE   :: next_hour = 1, icounter2 = 0
   INTEGER         :: hour_now
   DOUBLEPRECISION :: qoctot = 0.0d0, uzold = 0.0d0, uznowt, qoctotextra(100) = 0.0d0
   DOUBLEPRECISION :: PREVTM
   LOGICAL         :: SEDSRT=.FALSE.
   LOGICAL         :: BTIME = .FALSE.
   DOUBLEPRECISION :: GNUCUM (NELEE), DLSSRT (NELEE)

   PUBLIC :: FROPEN, FROUTPUT, FRRESP, write_dis, write_dis2
   PUBLIC :: qoctot, uzold, next_hour, icounter2, btime  ! Export variables needed by other modules

CONTAINS

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
280      END DO
290      ICHAR = ICHAR - 1
         DO 300 ISET = 1, NSET
            IORES (ISET) = 50 + ISET
            WRITE (ANUM, '(I2.2)') ISET
            fname = RESFIL(:ICHAR)//ANUM
            OPEN(IORES(ISET),FILE=TRIM(fname),FORM='UNFORMATTED')
            WRITE ( *, 9300) IORES (ISET), RESFIL (:ICHAR), ANUM
300      END DO
      ENDIF
9300  FORMAT(' OPENING FILE UNIT',I3,' TO FILE ',2A)
!
   END SUBROUTINE FRRESC

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
653      END DO
      ENDIF
      DO 654 J = 1, total_no_elements
         GNUCUM (J) = GNUCUM (J) + GNU (J) * (RESNOW - PREVTM) * 3600 * &
            1000


654   END DO

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
220            END DO
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
270               END DO
                  DUM1 (K) = DUM0 * RHOSED
274            END DO
               WRITE (IORES (ISET) ) RESNOW, DUM1
            ELSEIF (IODATA (ISET) .EQ.28) THEN
               DUM0 = 0
               DO 280 SED = SFSED1 (ICORS), SFSED2 (ICORS)
                  DUM0 = DUM0 + QSED (IEL, SED, 1) + QSED (IEL, SED, 2) &
                     + QSED (IEL, SED, 3) + QSED (IEL, SED, 4)
280            END DO
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
300               END DO
                  DUM1 (K) = 1D3 * DUM0 * RHOSED
304            END DO
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
340               END DO
               ELSE
                  DO 345 K = 1, top_cell_no
                     COLBUF (K) = zero
345               END DO
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
50       END DO
!

         IF (.NOT.COLUMN) WRITE (IORES (ISET) ) RESNOW, (BUFFER (J), &
            J = 1, NOUT)
!
         IOTIME (ISET) = RESNOW + IOSTEP (ISET)
!
100   END DO
!
!^^^^ sb 4/2/99
!^^^^ cummulative erosion output data type 44

      PREVTM = RESNOW
   END SUBROUTINE FRRESP

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

   SUBROUTINE FROUTPUT(SIMPOS)
      integer :: L, iface,disextrapoints,disextraelement(100),disextraface(100),pslextraelement(100),pslextrapoints,ifile
      CHARACTER (LEN=20) :: disextratext,pslextratext,celem
      CHARACTER (LEN=5) :: SIMPOS
      CHARACTER(256)     :: filnam
      DOUBLEPRECISION qocav, qocold,qocavextra(100)
      save disextrapoints,disextraelement,disextraface,pslextrapoints,pslextraelement

      INTEGER :: nminel, i, j, iel
1000  format(i7)            !PUT HERE FOR AD PROBLEM
1100  format(10(x,f9.3))
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
               write (celem,'(I0)') pslextraelement(i)
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

580   CALL ERROR(FFFATAL,1068,PPPRI,0,0,   'no or incorrect data in extra discharge points file')
581   CALL ERROR(FFFATAL,1069,PPPRI,0,0,   'no or incorrect data in water level data file')


   END SUBROUTINE FROUTPUT

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
16    FORMAT (// 'SHETRAN', F4.1 , ' ' )
15    FORMAT (// 'SHETRAN', F4.1 )
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
100   END DO

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

190   iszq=.false.
      isextrapsl=.false.
      goto 900

195   isextrapsl=.false.
      goto 900


200   IF (I.LT.14) THEN
         WRITE ( *, 1030) CNAM
         STOP 'ABNORMAL END'
      ENDIF
      GOTO 900
!
300   WRITE ( *, 1050) CNAM
      STOP 'ABNORMAL END'
!
400   WRITE ( *, 1040) FILNAM
      STOP 'ABNORMAL END'
!
900   RETURN
!
1000  FORMAT(A)
!
1010  FORMAT('- NOT USED')
!
1020  FORMAT('OPENING FILE UNIT ',I3,' TO FILE ',A)
1021  FORMAT('FILE ',I3,' IS ',A)
!
1030  FORMAT('UNEXPECTED -EOF- ON FILE ',A)
!
1040  FORMAT('ERROR OPENING FILE ',A)
!
1050  FORMAT('ERROR OPENING RUNDATA FILE ',A)
!
   END SUBROUTINE FROPEN

END MODULE framework_output_manager
