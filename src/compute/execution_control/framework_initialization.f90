MODULE framework_initialization
! Module for framework initialization functionality
! Extracted from FRmod.f90 as part of refactoring

   USE framework_shared, ONLY : INFR, DINET, DINOC, INET, INSM, INBK, FRDIM
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

   ! Module variables from original FRmod for initialization functionality
   DOUBLEPRECISION :: ALLOUT, DTAO
   LOGICAL :: BFRTS1, BFRTS2, BINFRP
   LOGICAL :: BSTORE, BPPNET, BPEPOT
   LOGICAL :: BPQOC, BPDEP, BPQF, BPQH, BPQSZ, BPHSZ, BPBAL, BPSD
   CHARACTER (LEN=80) :: TITLE
   CHARACTER(256) :: msg
   INTEGER :: IAOUT

   PUBLIC :: FRINIT, FRLTL
   PUBLIC :: bstore  ! Export variables needed by other modules

CONTAINS

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
         DO IFACE = 1, 4
            JEL = ICMREF (IEL, 4 + IFACE)
            IF (JEL.GT.0) THEN
               IF (ICMREF (JEL, 1) .NE.3.AND.NMC (JEL) .GT.0.AND.NRAINC &
                  (JEL) .GT.0) THEN
                  NMC (IEL) = NMC (JEL)
                  NRAINC (IEL) = NRAINC (JEL)
                  EXIT  ! Exit inner loop, continue with next element
               ENDIF
            ENDIF
         END DO

30    END DO
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
         ! Read hotstart data until time condition met
         hotstart_reading: DO
            READ (HOT, *, END = 120) atemp, HOTIME, UZNEXT, top_cell_no,atemp, (CSTORE (IEL), &
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
            IF (HOTIME.GE.BHOTTI) THEN
               EXIT hotstart_reading  ! Time condition met
            END IF
            ! Continue reading next record
         END DO hotstart_reading
!
120      WRITE(PPPRI, 122)
122      FORMAT  ( / ' WARNING: END OF HOTSTART FILE REACHED')
!
125      WRITE(PPPRI, 127) HOTIME
127      FORMAT  (// ' ^^^ HOTSTART OF SIMULATION AT TIME ',F10.2,' ^^^' /)
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
10    FORMAT (A80)
      IF (BPCNTL) WRITE (IOF, 20) TITLE
20    FORMAT (A80)
!
      DO J = 1, NNY
         DO I = 1, NNX
            IARR (I, J) = 0
         END DO
      END DO
!
      I = NNY
      DO 40 J = 1, NNY
         READ (INF, 50) K, (A1LINE (L), L = 1, NNX)
50       FORMAT   (I7, 1X, 500A1)
         IF (BPCNTL) WRITE (IOF, 50) K, (A1LINE (L), L = 1, NNX)
!
         IF (K.NE.I) GOTO 100
         I = I - 1
!
         DO L = 1, NNX
            DO M = 1, 9
               IF (A1LINE (L) .EQ.NMERIC (M) ) THEN
                  IARR (L, K) = M
                  EXIT  ! Exit inner loop, continue to next L
               ENDIF
            END DO
         END DO
!
40    END DO
      RETURN
!
100   IF (BPCNTL) WRITE (IOF, 110)
110   FORMAT ('  ^^^   INCORRECT COORDINATE')
      STOP
   END SUBROUTINE FRLTL

END MODULE framework_initialization
