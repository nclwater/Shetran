!> summary: Shared flow-component state and file-unit constants.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> `AL_D` stores common state used primarily by the SHETRAN flow components.
!> It includes file-unit constants, model-size and timing controls, process
!> activation flags, input/output scheduling arrays, hotstart state, snow state,
!> overland/channel flow storage, meteorological forcing arrays, and reservoir
!> ZQ-table metadata.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-03 | GP | 3.0 | Original version written. |
!> | 1991-06 | GP | 3.1 | Added new variables including `NEXPO`. |
!> | 1992-02 | GP | 3.3 | Added soil-layer arrays. |
!> | 1992-06 | GP | 3.4 | Moved selected variables to `AL_C`, added flow/snow/storage arrays, and removed obsolete constants. |
!> | 1994-09-28 | RAH | 3.4.1 | Applied standard header, declared variables, and removed `INTEGER*2`. |
!> | 1996-01-03 | GP | 4.0 | Moved selected VSS variables to `AL_C`, removed redundant SZ/UZ/EX variables, and added mass-balance arrays. |
!> | 1997-02 | RAH | 4.1 | Removed redundant derived dimensions and obsolete state variables. |
!> | 1998-01-19 | RAH | 4.2 | Removed obsolete OC and storage variables; defined `NCLASS`. |
!> | 2004-07 | JE | - | Converted to Fortran 95. |
!> | 2020-05-20 | SB | - | Added ZQ table file unit and reservoir table metadata variables. |
!> @endhistory
MODULE AL_D
USE SGLOBAL, ONLY : NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, NOCTAB
IMPLICIT NONE

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
