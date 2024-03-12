MODULE AL_D
USE SGLOBAL, ONLY : NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, NOCTAB
IMPLICIT NONE
!-------------------- START OF AL.D -----------------------------------!
!
! INCLUDE FILE FOR COMMON VARIABLES FOR FLOW COMPONENTS ONLY
!
!----------------------------------------------------------------------!
! Version:  AL_D.F95/4.3
! Modifications:
!   GP        MAR 91    3.0     WRITTEN
!   GP        JUN 91    3.1     NEW VARIABLES ADDED (NEXPO)
!   GP        FEB 92    3.3     SOIL LAYER ARRAYS ADDED
!   GP        JUN 92    3.4 VARIABLES MOVED TO AL.C FOR HOTSTART
!                           (TIH,UZNOW & array NRD).  Add arrays
!                           NSMC,FLERRC,SYERRC,CMERRC,KSAT,QSA,DQ0ST,
!                           DQIST,DQIST2,SMELT,TM,WATC3,ESWA,QEX,QEXDH.
!                           Scrap constants NS801,NS501.  NSTEP I*4.
! RAH  28.09.94  Version 3.4.1 from version 3.4: standard header;
!                declare everything; no INTEGER*2 (/DFILE/,/ALDCB2/);
!                remove FLERRC, SYERRC, CMERRC from /ALDCB7/.
!  GP  960103  4.0  Move BEXBK,ESOILA,NBFACE,PRI,QH,UZNEXT,WLD to AL.C.
!                   Move PSI4,UZALFA to SPEC.ET.
!                   Remove DFRLYR,IFRLYR,NFRLYR,NLYRC (see INBK),
!                   EXBETA (see OCPLF), QSZO,QSZR,QUZR (see SHETRN),
!                   and THWILT (see ET).  Also remove
!                   AQD,CONX,CONY,HBD,NW,SZB,SZD (SZ), DNPRUZ,EFFSAT,
!                   EPS,EPSZA,HPSI0,PSI3,PSI33,TH1,TH33,UZD,WATC3 (UZ),
!                   BLOWP,CCB,DB (EX), QSZUZ (SZ,UZ), KSAT (EX,UZ),
!                   QSZOC,RSZ (FR,SZ), THUZ (FR,UZ), CATUZ (BK,UZ),
!                   PRUZN,THFC (ET,UZ), and QBOU,THBOT (FR,SZ,UZ).
!                   Increase size of ICLNUM,ICLIST to 14 (see INRES).
!                   Bring NOCBC* from SPEC.OC (for INRES).
!                   New variables MB* (for INRES,MB), and arrays
!                   BALANC,*STOR,*MEAN (for FRRESP,MB).
!                   Bring TIM from SPEC.FR (for FRRESC).
! RAH  970212  4.1  Remove "derived dimensions" (redundant).
!      970213       Remove WSZEX,WSSZER,WSZR,WOCSZS,WUZSZS (SZ),
!                   EPSZ,WSUZ,WUZEV,WUZTR,WUZSZU,WSUZER,WSUZT,WSUZTI,
!                   WOCUZU (UZ), NCTUZR,ICTUZR (BK,UZ), SZNEXT (FR,SZ),
!                   WSSZ (SZ,UZ), QEX,QEXDH (EX,SZ), DRYH (EX,SZ,UZ),
!                   and EXUNT,QSZL,WOCSZO,WOCUZO (redundant).
! RAH  980119  4.2  Remove WSOC,WSOCI,WSOCER (see OCINI) and
!                   WOCEV,WOCLI,WOCR (redundant).
!      980306       Remove EXNOW,EXVAL,EXNEXT,UZUNT,OCVAL,OCUNT,SZNOW,
!                   SZVAL,SZUNT,WSETI,WSET,WEXET,WETEX,WETOCE,WSETER,
!                   DWEXET,DWETEX,DWETOC,WEP,DWEP,DWSET,WSSZI,WSEPS,
!                   TSTOUZ,TSTOOC,TSTOSZ,DTSTUZ,DTSTSZ,DTSTOC,PNETOC,
!                   POC,CCSTOR,CSSTOR,HFL,HFI,VOCX,VOCY,EPOTOC.
!                   RHOSAR is static.
!      980307       Reduce size of NOCBCD from 5 to 4.
!      980713       Remove I/NCATUZ (see FRRESC).
!      980716       Define NCLASS (see INRES,FRRESC).
!      981103       Remove NSOIL (see ETSIM,ET).
!  JE  JULY 04 ---- Convert to FORTRAN 95
!***ZQ Module 200520
! new variables     zqd,NoZQTables,ZQTableRef,iszq,ZQTableLink,ZQTableFace,ZQweirSill!----------------------------------------------------------------------*

! ----- Constants

      INTEGER, PARAMETER :: NCLASS=14

! ----- File unit numbers

!      INTEGER          FRD,MED,ETD,OCD,SMD,TIM,RES,HOT,EPD, &
!                      PRD,VED,PPD,BKD,OFB,OHB,DIS,MAS,VSE, DIS2
                       
INTEGER,PARAMETER :: &    !VALUES USED TO BE SET IN FRINIT 
FRD = 10, &
!VSD = 11 , &
OCD = 12 , &
ETD = 13 , &
PPD = 14 , &
SMD = 15 , &
BKD = 16 , &
!SYD = 17 , &
!CMD = 18 , &
MED = 19 , &
PRD = 20 , &
EPD = 21 , &
TIM = 22 , &
!PRI = 23 , &
!SPR = 24 , &
!CMP = 25 , &
!BUG = 26 , &
RES = 27 , &
HOT = 28 , &
!VSI = 29 , &
VED = 30 , &
!WLD = 31 , &
!LFB = 32 , &
!LHB = 33 , &
!LGB = 34 , &
!BFB = 35 , &
!BHB = 36 , &
OFB = 37 , &
OHB = 38 , &
!CMT = 39 , &
!CMB = 40 , &
DIS = 41 , &
VSE = 42 , &
MAS = 43 , &
DIS2 = 44, &
TAH = 45, &
TAL = 46, &
disextra = 47, &
zqd = 51, &
pslextra = 52

! ----- Static integer variables

      INTEGER ::          MSM,NM,NRAIN,NSET,MBLINK,MBFACE,MBFLAG, &
                      NXP1,NYP1,NXM1,NYM1,NXEP1,NYEP1,NoZQTables,ZQTableRef
INTEGER, PARAMETER :: &
NXE = NXEE, &  
NYE = NYEE 

! ----- Time-dependent integer variables

      INTEGER          NSTEP,NRPD,NSMT,MBYEAR,MBMON,MBDAY

! ----- Static floating-point variables

      DOUBLEPRECISION PSTART,DTMET,QMAX,BHOTTI,BHOTST,PMAX, &
                      PALFA,TMAX,CAREA,BWIDTH,TTH,DTMET2,DTMET3,TOUTPUT

! ----- Time-dependent floating-point variables

      DOUBLEPRECISION UZVAL,OCNOW,OCNEXT,HRUZ, &
                      PNET,PE,EINT,ERZ,DRAIN,ESOIL,AE,CSTOLD,CPLAI, &
                      !METIME,MELAST,PINMAX,EPTIME,PREST,TIMEUZ,HOTIME
                      PREST,TIMEUZ,HOTIME

! ----- Static logical variables

      LOGICAL          BEXET,BEXUZ,BEXEX,BEXOC,BEXSZ,BEXSM, &
                      BEXTS1,BHOTPR,BHOTRD,BEXSY,BEXCM, ISTA,isextradis,iszq,isextrapsl

! ----- Static integer arrays
!
      INTEGER           NGRID(NELEE),INGRID(NXEE,NYEE),IOCORS(NSETEE)
      INTEGER             NMC(NELEE),LCODEX(NXEE,NYEE),IODATA(NSETEE)
      INTEGER          NRAINC(NELEE),LCODEY(NXEE,NYEE),IOELEM(NSETEE)
      INTEGER          NOCBCC(NELEE),NOCBCD(NOCTAB,4),  IORES(NSETEE)
      INTEGER          ICLIST(NELEE,NCLASS),            NEXPO(NLFEE,2)
      INTEGER          ICLNUM(NCLASS)
      INTEGER, DIMENSION(:), ALLOCATABLE               :: ZQTableLink,ZQTableFace ! These store the metadata for a single ZQtable in the ZQ file

! ----- Time-dependent integer arrays

      INTEGER          NSMC(NELEE)

!970212 TEMPORARY!
      INTEGER          FLERRC(0:100),SYERRC(0:100),CMERRC(0:100)

! ----- Static floating-point arrays
!
      DOUBLEPRECISION   DXIN(NXEE),DYIN(NYEE),WIDTF(NLFEE),ZBED(NELEE), &
                      HFLBED(NLFEE), ZFBED(NLFEE),       DZFBED(NLFEE), &
                       LROOT(NVEE), HFLBNK(NLFEE),       IOSTA(NSETEE), &
                      IOSTEP(NSETEE),IOEND(NSETEE),      RHOSAR(NELEE)
      DOUBLEPRECISION,    DIMENSION(:), ALLOCATABLE              :: ZQweirSill 

! ----- Time-dependent floating-point arrays
!
      DOUBLEPRECISION CSTORE(NELEE), ERZA(NELEE), &
                        EPOT(NELEE),EINTA(NELEE),EPOTR(NVEE), &
                          SD(NELEE),   TS(NELEE),   SF(NELEE), &
                           S(LLEE),     precip_m_per_s(nelee), &
                       OBSPE(NVEE),    TA(NVEE),     U(NVEE),VPD(NVEE), &
                          RN(NVEE),VHT(NVEE),IOTIME(NSETEE), &
                         DQ0ST(NELEE,4),DQIST(NELEE,4), &
                      DQIST2(NLFEE,3), &
                        ESWA(NELEE), BALANC(20),CMEAN(NELEE,2,NCONEE), &
                       SMEAN(NELEE,2,NCONEE),  ADMEAN(NELEE,2,NCONEE)

! ----- Static character variables
!
      CHARACTER*200     RESFIL
      
!PRIVATE :: NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, NOCTAB
END MODULE AL_D