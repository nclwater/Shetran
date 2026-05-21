!> summary: SHETRAN frame initialisation, file I/O, output, and component setup.
!>
!> This module contains the legacy FR "frame" routines that connect SHETRAN's
!> component models to run data, mesh indexing, output control, hot-start/restart
!> files, and component initialisation. It reads the global frame input, opens
!> data files, constructs element/link/bank indexing, calculates element areas
!> and face lengths, initialises ET, snow, overland/channel, VSS, bank, sediment,
!> contaminant, plant, and ZQ-table options, and writes both legacy result output
!> and newer text time series.
!>
!> The routines here are primarily orchestration and data-marshalling code rather
!> than a separate hydrological process formulation. They provide the common
!> bookkeeping that allows the water-flow, sediment, contaminant, snow, ET, and
!> reservoir table modules to share geometry, file units, time controls, and
!> output definitions.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed and standardised the FR frame, including impermeable-bed defaults, `BSOFT`, `TIM` migration to `AL_D`, result output, and hot-start/rescue handling. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the FR `.F` files into this Fortran 90 module. |
!> | 2020-05 | SB | 4.5 | Added ZQ-module variables and support. |
!> | 2026-03 | SB | 4.6 | Added allocation-based initialisation through `INITIALISE_AL_C3` and `INITIALISE_ETMOD`, date-aware meteorological input through `BMETDATES`, outlet sediment/contaminant text series, water-table and virtual-discharge text output, improved diagnostics, and `.pri` reporting of hard-coded array sizes. |
MODULE FRmod
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
USE mod_load_filedata,    ONLY : ALINIT, ALINTP, ALCHK, ALCHKI
USE SMmod,    ONLY : head, binsmp, ddf, rhos, zos, zds, zus, nsd, rhodef, imet, smelt, tmelt
USE ETmod,    ONLY : BAR, BMETP, BINETP, BMETAL, BMETDATES, CSTCAP, CSTCA1, CK, CB, CLAI1, FET, &
                     MEASPE, MODE, MODECS, MODEVH, MODEPL, MODECL, NCTCLA, NCTVHT,NCTCST, NF, NCTPLA, &
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
! Legacy SPEC.FR frame variables retained as module state.
!IMPLICIT NONE
INTEGER :: IAOUT
!COMMON / FRCB1 / IAOUT
! Floating-point frame variables and arrays.
DOUBLEPRECISION ALLOUT, DTAO, TSH, TCH !, TITLE (20)
!COMMON / FRCB2 / ALLOUT, DTAO, TSH, TCH, TITLE
! Logical frame-control flags.
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


!> Calculates element dimensions, face lengths, and total catchment area.
!>
!> `FRDIM` derives grid-cell dimensions from half-grid spacing, assigns areas
!> for channel links, banks, and land elements, computes face lengths `DHF`,
!> and accumulates `CAREA`. These geometry terms are used throughout water,
!> sediment, and contaminant calculations.
!>
!> Inputs are the active model dimensions and grid/link geometry from the legacy
!> `SPEC.AL` state: `NEL`, `NX`, `NY`, `NXM1`, `NYM1`, `ICMREF`, `CWIDTH`,
!> `DXIN`, `DYIN`, and `LINKNS`. Outputs are `CAREA`, `AREA`, `DHF`, `DXQQ`,
!> `DYQQ`, and the fixed bank-element width `BWIDTH`.
!>
!> The routine first converts half-grid spacings to full cell dimensions:
!>
!> \[
!> DX_1=DXIN_1,\qquad DX_{NX}=DXIN_{NX-1},\qquad
!> DX_i=\frac{DXIN_{i-1}+DXIN_i}{2},
!> \]
!>
!> with the same construction for `DY`. The bank width is currently fixed as
!> `BWIDTH = 10 m`.
!>
!> Initial element dimensions are assigned from element type `ICMREF(IEL,1)`.
!> Grid elements use the full grid dimensions,
!>
!> \[
!> DXQQ=DX(IX),\qquad DYQQ=DY(IY).
!> \]
!>
!> Bank elements use `BWIDTH` across the bank and the grid spacing along the
!> associated link: north-south links use `DXQQ=BWIDTH`, `DYQQ=DY(IY)`;
!> east-west links use `DXQQ=DX(IX)`, `DYQQ=BWIDTH`. Channel links use channel
!> width across the channel and grid spacing along the link:
!>
!> \[
!> \begin{array}{ll}
!> DXQQ=CWIDTH,\ DYQQ=DY,\ CLENTH=DY, & \text{north-south link},\\
!> DXQQ=DX,\ DYQQ=CWIDTH,\ CLENTH=DX, & \text{east-west link}.
!> \end{array}
!> \]
!>
!> The dimensions of grid and bank elements are then reduced to remove overlap
!> with adjacent channels and banks. For a grid face adjacent to a channel or
!> bank, the removed width is
!>
!> \[
!> \Delta = 0.5\,CWIDTH + \begin{cases}
!> BWIDTH, & \text{adjacent element is a bank},\\
!> 0, & \text{adjacent element is a channel link}.
!> \end{cases}
!> \]
!>
!> Bank-bank corner overlaps are also removed by subtracting
!> `BWIDTH + 0.5*CWIDTH` from the along-bank dimension of the paired bank
!> elements. The final element area and total catchment area are then
!>
!> \[
!> AREA_i = DXQQ_i\,DYQQ_i,\qquad CAREA=\sum_i AREA_i.
!> \]
!>
!> `CATEST` is the uncorrected sum of basic grid-square areas,
!> \(\sum DX(IX)DY(IY)\), used only for optional printed diagnostics comparing
!> the basic catchment area with the element-area sum after channel and bank
!> corrections.
!>
!> Finally, `DHF(IEL,face)` stores the distance from the element computational
!> node to each face. West and south distances are calculated from the neighbour
!> element type and local overlap corrections; east and north distances are the
!> remaining parts of the corrected element dimensions:
!>
!> \[
!> DHF_{east}=DXQQ-DHF_{west},\qquad
!> DHF_{north}=DYQQ-DHF_{south}.
!> \]
SUBROUTINE FRDIM (BINFRP)
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



!> Builds element, bank, link, grid, and neighbour index arrays.
!>
!> The routine converts grid/link/bank code maps into compact SHETRAN element
!> numbering, including the index arrays needed by contaminant migration. Inputs
!> are the grid dimensions and code maps `NX`, `NY`, `INGRID`, `LCODEX`,
!> `LCODEY`, plus the bank/OC flags `BEXBK` and `BEXOC`. It sets `NEL`,
!> `NGDBGN`, and `NLF`, and fills `ICMREF`, `ICMRF2`, `ICMBK`, `ICMXY`,
!> `NBFACE`, `NGRID`, and `LINKNS`, defining the topology later used by OC,
!> VSS, sediment, and contaminant routines.
!>
!> Element numbers are assigned in a fixed order. Channel links are created
!> first from the link-code grids: `LCODEY >= 4` creates east-west links
!> (`LINKNS=.FALSE.`), then `LCODEX >= 4` creates north-south links
!> (`LINKNS=.TRUE.`). Each link has `ICMREF(:,1)=3`, stores its grid location in
!> `ICMREF(:,2:3)`, and stores its own link number in `ICMREF(:,4)`.
!> `total_no_links` is the last link index.
!>
!> If the bank component is active, two bank elements are then created for each
!> link. Bank element type is `1` or `2`, `ICMREF(:,4)` points back to the
!> associated link, and `ICMBK(link,bank)` maps from a link and bank side to the
!> bank element number. Grid elements are added last for every non-negative
!> `INGRID` cell; `ICMXY(i,j)` maps a grid coordinate back to the grid-element
!> number. Consequently
!>
!> \[
!> NGDBGN = total\_no\_links + 1,
!> \]
!>
!> so active land/bank/grid elements begin immediately after the channel links.
!>
!> `ICMREF` columns 5:8 hold the neighbours across faces 1:4
!> (east, north, west, south). For grid elements the neighbour is either the
!> adjacent grid cell, an intervening bank element when banks are enabled, or
!> the channel link itself when OC links exist without banks. In the latter case
!> `ICMREF(:,4)=9999` marks that the grid element is adjacent to a channel
!> system rather than an ordinary soil-only element.
!>
!> Channel-link faces either point to their adjacent banks/grid cells or to
!> other channel links at link nodes. A single connected link is stored directly
!> in `ICMREF(:,5:8)`. If a node has multiple connected links, `FRIND` creates an
!> auxiliary `ICMRF2` entry, stores the connected link numbers in
!> `ICMRF2(idx,1:3)`, and stores `-idx` in the relevant `ICMREF` face column.
!> This negative pointer is used later by routing and contaminant routines to
!> expand multi-link junctions.
!>
!> Bank-element face neighbours are assigned according to the associated link
!> orientation and bank side: one face connects to the channel link, one or more
!> faces may connect to neighbouring bank elements around junctions, and the
!> outer face connects to the adjacent grid cell where present.
!>
!> After all forward neighbours are assigned, `FRIND` checks that each neighbour
!> points back to the current element. For ordinary neighbours it records the
!> reciprocal face in `ICMREF(:,9:12)`. For multi-link nodes it records the
!> reciprocal faces in `ICMRF2(:,4:6)`. Boundary faces keep their own face index
!> in `ICMREF(:,9:12)` and the first boundary face for non-link elements is
!> stored in `NBFACE`.
SUBROUTINE FRIND (BINFRP)
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


!> Runs the main frame initialisation sequence.
!>
!> `FRINIT` calls the common frame reader, component-specific input routines,
!> geometry/index setup, VSS initialisation, bank/sediment/contaminant setup,
!> hot-start reading, allocation routines, and output header preparation before
!> the first timestep is executed.
!>
!> The initialisation uses shared model dimensions and flags including `LL`,
!> `NEL`, `NGDBGN`, `NLF`, `ICMREF`, `UZNEXT`, `CSTORE`, `BEXBK`, `BEXET`,
!> `BEXOC`, `BEXSM`, `DTAO`, `BHOTRD`, `BINFRP`, and `BSTORE`. It updates
!> meteorological/rainfall category arrays `NMC` and `NRAINC`, opens or sets the
!> component input/output units (`BFB`, `BHB`, `BKD`, `CMB`, `CMD`, `CMP`, `CMT`,
!> `EPD`, `ETD`, `FRD`, `HOT`, `LFB`, `LGB`, `LHB`, `MED`, `OCD`, `OFB`, `OHB`,
!> `PPD`, `PRD`, `PRI`, `RES`, `SMD`, `SPR`, `SYD`, `TIM`, `VED`, `VSD`, `VSI`,
!> `WLD`), and initialises run state such as `BHOTTI`, `HOTIME`, `OCNOW`,
!> `TIMEUZ`, `UZNEXT`, `UZNOW`, `MSM`, and `ALLOUT`.
SUBROUTINE FRINIT()
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

call INITIALISE_AL_C3
call INITIALISE_ETMOD
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



!> Reads a gridded numeric-code map used for output class definitions.
!>
!> `NNX` and `NNY` are the grid dimensions to read, while `NXE` and `NYE` are the
!> declared dimensions of output array `IARR`. `INF` is the input file unit,
!> `IOF` is the output/echo file unit, and `BPCNTL` controls whether the read
!> code map is printed. The numeric codes read from `INF` are returned in
!> `IARR`.
SUBROUTINE FRLTL (NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)
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



!> Calculates and writes monthly water-balance accumulators.
!>
!> `FRMB` accumulates precipitation, evapotranspiration, discharge, storage,
!> subsurface, snow, and balance terms in cubic metres, resets monthly totals
!> when required, and triggers result output through [[frresp]].
!>
!> All accumulated quantities are in cubic metres. The routine uses the
!> following limited index ranges:
!>
!> | Quantity | Limited range used |
!> |:---------|:-------------------|
!> | Link-indexed arrays with declared size `NLFEE` | `link = 1:NLF` |
!> | `DELTAZ(cell,e)` and `VSTHE(cell,e)` | `cell = NLYRBT(e,1):LL` |
!> | `P(ipstn)` | `ipstn` values taken from `NRAINC(1:NEL)` |
!> | `QVSV(cell,e)` | `cell == NLYRBT(e,1)` |
!>
!> Entry conditions are `1 <= LL <= LLEE`, `1 <= NEL <= NELEE`,
!> `0 <= NLF <= NLFEE` with `NLFEE >= 1`, and for each element `e`,
!> `2 <= NLYRBT(e,1) <= LLEE` and `1 <= NRAINC(e) <= NVEE`.
!>
!> Inputs include monthly-balance controls `MBFACE`, `MBFLAG`, `MBLINK`, model
!> dimensions `LL`, `NEL`, `NLF`, geometry/storage arrays `AREA`, `CLENTH`,
!> `DELTAZ`, `ZGRUND`, `ARXL`, `CSTORE`, `HRF`, `SD`, `VSTHE`, flow terms `QOC`,
!> `QBKB`, `QBKF`, `QVSV`, rainfall and ET terms `P`, `EINTA`, `EEVAP`, and time
!> controls `TIH` and `DTUZ`. It updates `MBDAY`, `MBMON`, `MBYEAR`, and the
!> `BALANC(19)` accumulator.
!>
!> `BALANC` stores both short-period and cumulative water-balance terms:
!>
!> | Index | Meaning |
!> |:------|:--------|
!> | 1:6 | Current reporting-period precipitation, canopy evaporation, soil/surface-water evaporation, transpiration, regional aquifer flux through the model base, and outlet discharge. |
!> | 7:12 | Cumulative totals of the same six flow terms. |
!> | 13 | Canopy storage. |
!> | 14 | Snowpack water-equivalent storage. |
!> | 15 | Subsurface water storage. |
!> | 16 | Surface-water storage on land elements. |
!> | 17 | Channel water storage. |
!> | 18 | Current reporting-period aquifer-channel exchange through channel bed and sides. |
!> | 19 | Cumulative aquifer-channel exchange. |
!>
!> On each timestep, rates are converted to volumes with
!>
!> \[
!> A_t(e)=AREA_e\,DTUZ.
!> \]
!>
!> The timestep contributions are
!>
!> \[
!> P_m = \sum_e precip_e A_t(e),\qquad
!> E_{can,m} = \sum_e EINTA_e A_t(e),
!> \]
!>
!> \[
!> E_{soil,m} = \sum_e EEVAP_e A_t(e),\qquad
!> T_m = \sum_e ERZA_e A_t(e),
!> \]
!>
!> \[
!> Q_{base,m} = \sum_e QVSV_{NLYRBT(e,1)-1,e} A_t(e).
!> \]
!>
!> Outlet discharge is taken from the configured monthly-balance link and face:
!>
!> \[
!> Q_{out,m} =
!> \begin{cases}
!> |QOC(MBLINK,MBFACE)|\,DTUZ, & MBLINK \ne 0,\\
!> 0, & MBLINK = 0.
!> \end{cases}
!> \]
!>
!> Aquifer-channel exchange is accumulated over all links from bank-bed and
!> bank-face flows:
!>
!> \[
!> Q_{bank,m} =
!> \sum_l \left(QBKB_{l,1}+QBKB_{l,2}+QBKF_{l,1}+QBKF_{l,2}\right)DTUZ.
!> \]
!>
!> These timestep values are added to both `BALANC(1:6)` and `BALANC(7:12)`,
!> while `Q_bank,m` is added to `BALANC(18)` and `BALANC(19)`.
!>
!> Storage terms are recomputed only when output is due (`UZNOW >= TIMB`).
!> Canopy and snow storages convert millimetres over element area to cubic
!> metres with `MPMM = 1D-3`:
!>
!> \[
!> BALANC_{13}=\sum_e CSTORE_e\,AREA_e\,10^{-3},
!> \]
!>
!> \[
!> BALANC_{14}=\sum_e SD_e\,RHOSAR_e\,AREA_e\,10^{-3}.
!> \]
!>
!> Subsurface, land-surface, and channel storages are
!>
!> \[
!> BALANC_{15}=\sum_e\sum_{k=NLYRBT(e,1)}^{top}
!> VSTHE_{k,e}\,DELTAZ_{k,e}\,AREA_e,
!> \]
!>
!> \[
!> BALANC_{16}=\sum_e (HRF_e-ZGRUND_e)AREA_e,\qquad
!> BALANC_{17}=\sum_l ARXL_l\,CLENTH_l.
!> \]
!>
!> The routine writes these values through [[frresp]] using output-data selector
!> 50. It then advances the next reporting date by one day when `MBFLAG=1`, or
!> to the first day of the next month otherwise, including Gregorian leap-year
!> handling for February. After output, the short-period flow terms
!> `BALANC(1:6)` and `BALANC(18)` are reset to zero; cumulative totals are
!> retained.
SUBROUTINE FRMB
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



!> Opens the run-data controlled input and output files.
!>
!> `FROPEN` prints the SHETRAN banner, opens the frame/run files, reads file
!> names and unit assignments, and prepares legacy output streams used by
!> initialisation and runtime reporting.
SUBROUTINE FROPEN

! Commons and constants
INTEGER :: i, io
integer :: ios
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
Write (*,*)
Write (*,*) '**************************'
WRITE ( *, * ) BANNER
Write (*,*) '**************************'


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
!       WRITE(*,'(A)') ' Enter catchment name: '  !e.g. 'excobpine'
!       READ(*,1000) CNAM
!          FILNAM = TRIM(DIRQQ)//RUNFIL//CNAM

! ****sb 161213
ista=.true.
! ****sb 220415
isextradis=.true.
!***ZQ Module 200520
iszq=.true.
!sb 110324
isextrapsl=.true.
!sb 230925
ismn=.true.


OPEN (2, FILE = FILNAM, STATUS = 'OLD', IOSTAT = io)
if (io /= 0) then
    write (*,'(A,A)') ' Error opening the rundata file ',trim(FILNAM)
    write(*,'(''paused, type [enter] to continue'')')
    read (*,*)
    stop
endif
filnam2=TRIM (DIRQQ) //'info_'//trim(cnam)//'_SHETRAN_log.txt'
OPEN (61, FILE = FILNAM2, ERR = 400)



READ (2, 1000, ERR = 300) FILNAM



!***ZQ Module 200520 change log file to unit 52 and read DO 100 I = 10, 51 (was 50)
!***extra psl 110324 change log file to unit 53 and read DO 100 I = 10, 52 (was 50). see extra lines at the end
!***nitrate 230925 change log file to unit 61 and read DO 100 I = 10, 60 (was 50). see extra lines at the end
WRITE (61, 1000) FILNAM
WRITE ( 61, * )
DO 100 I = 10, 50
   READ (2, 1000, END = 200) FILNAM
   WRITE ( 61, 1000) FILNAM
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

      WRITE ( 61, 1010)
   ELSE
      filnam = TRIM (DIRQQ) //TRIM (FILNAM)
      IF (I == 48) THEN
         WRITE ( 61, 1021) I, FILNAM
         visualisation_plan_filename = filnam
      ELSEIF (I == 49) THEN
         WRITE ( 61, 1021) I, FILNAM
         visualisation_check_filename = filnam
      ELSEIF (I == 50) THEN
         WRITE ( 61, 1021) I, FILNAM
         hdf5filename = filnam
      ELSE
         WRITE ( 61, 1020) I, FILNAM
! make  hot file formattedsteve birkinshaw 13092017
!        IF (I.EQ.27.OR.I.EQ.28) THEN
!            OPEN (I, FILE = FILNAM, FORM = 'UNFORMATTED', ERR = &
!             400)
!         ELSE
            OPEN (I, FILE = FILNAM, iostat=ios)
            if (ios/=0) then
                write (*,'(A,A)') ' Error opening the file ',trim(FILNAM)
                write(*,'(''paused, type [enter] to continue'')')
               read (*,*)
                stop
            endif
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
WRITE ( 61, 1000) FILNAM
READ (2, 1000, END = 190) FILNAM
IF (FILNAM.EQ.' '.OR.FILNAM.EQ.'0') THEN
         iszq=.false.
      WRITE ( 61, 1010)
else
    OPEN (51, FILE = FILNAM, ERR = 400)
    WRITE ( 61, 1020) 51, FILNAM
endif

 !extra psl 110324
READ (2, 1000, END = 195) FILNAM
WRITE ( 61, 1000) FILNAM
READ (2, 1000, END = 195) FILNAM
IF (FILNAM.EQ.' '.OR.FILNAM.EQ.'0') THEN
         isextrapsl=.false.
      WRITE ( 61, 1010)
else
    filnam2=TRIM (DIRQQ) //filnam
    OPEN (52, FILE = FILNAM2, ERR = 400)
    WRITE ( 61, 1020) 52, FILNAM2
endif

  !nitrate component 230925
READ (2, 1000, END = 196) FILNAM
WRITE ( 61, 1000) FILNAM
READ (2, 1000, END = 196) FILNAM
IF (FILNAM.EQ.' '.OR.FILNAM.EQ.'0') THEN
         ismn=.false.
      WRITE ( 61, 1010)
else
    filnam=TRIM (DIRQQ) //filnam
    OPEN (53, FILE = FILNAM, ERR = 400)
    WRITE ( 61, 1020) 53, FILNAM
endif
DO  I = 54, 60
    READ (2, 1000, END = 196) FILNAM
   WRITE ( 61, 1000) FILNAM
   READ (2, 1000, END = 196) FILNAM
   IF (FILNAM.EQ.' '.OR.FILNAM.EQ.'0') THEN
       WRITE ( 61, 1010)
   else
       filnam=TRIM (DIRQQ) //filnam
       OPEN (I, FILE = FILNAM, ERR = 400)
       WRITE ( 61, 1020) I, FILNAM
   endif
enddo

CLOSE (2)

GOTO 900
!

190 iszq=.false.
    isextrapsl=.false.
    ismn=.false.
    goto 900

195 isextrapsl=.false.
    ismn=.false.
    goto 900


196 ismn=.false.
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



!> Manages additional text time-series output.
!>
!> The routine handles start, timestep, and close phases for CSV-style discharge,
!> extra discharge stations, water-table depth, sediment, fine sediment, and
!> contaminant outlet series. It keeps running totals between calls and formats
!> time using `DATE_FROM_HOUR`.
SUBROUTINE FROUTPUT(SIMPOS)
integer :: L, iface,disextrapoints,pslextrapoints,ifile
CHARACTER (LEN=20) :: disextratext,pslextratext,celem
CHARACTER (LEN=5) :: SIMPOS
CHARACTER(256)     :: filnam
CHARACTER(128)    :: dum
integer, parameter :: SEDALLUNIT = 681
integer, parameter :: SEDFINEUNIT = 682
integer, parameter :: PSLFILEUNIT = 683
integer, parameter :: CONTAMUNIT = 684
INTEGER, DIMENSION(:), ALLOCATABLE               :: pslextraelement
INTEGER, DIMENSION(:), ALLOCATABLE               :: disextraelement,disextraface
DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE               :: qocavextra

DOUBLEPRECISION qocav, qocold
DOUBLEPRECISION sedav,sedfineav,contamav
save disextrapoints,disextraelement,disextraface,pslextrapoints,pslextraelement,qocavextra
DOUBLEPRECISION outputhour

INTEGER :: nminel, i, j, iel, ios
INTEGER  :: c(6)
character(len=32), DIMENSION(:),allocatable :: buf
character(len=32) :: bufmb(17)
character(len=32) :: bufdis
save buf

1000 format(i7)            !PUT HERE FOR AD PROBLEM
1100 format(10(x,f9.3))
IF (SIMPOS.EQ.'start') THEN

    if (ISextradis) then
      read(disextra,*,err=580,end=580)
      read(disextra,*,err=580,end=580) disextratext,disextrapoints
      allocate   (disextraelement(disextrapoints))
      allocate   (disextraface(disextrapoints))
      allocate   (qocavextra(disextrapoints))
      allocate   (qoctotextra(disextrapoints))
      allocate   (buf(disextrapoints))
      disextraelement=0
      disextraface=0
      qocavextra=0.0d0
      qoctotextra=0.0d0
      buf = ''

      j=0
      do i=1,disextrapoints
         j=j+1
         read(disextra,*,err=580,end=580) disextraelement(j),disextraface(j)
         !remove the output if the element number is too big
         if (disextraelement(j) .GT. total_no_links) then
            disextraelement(j)=0
            disextraface(j)=0
            j=j-1
         endif
      enddo
      disextrapoints=j

    endif

!sb 110324 extra water table output
    if (ISextrapsl) then
      read(pslextra,*,err=581,end=581)
      read(pslextra,*,err=581,end=581) pslextratext,pslextrapoints
      allocate   (pslextraelement(pslextrapoints))
      pslextraelement=0
      j=0
      do i=1,pslextrapoints
         j=j+1
         read(pslextra,*,err=581,end=581) pslextraelement(j)
         !remove the output if the element number is too big
         if (pslextraelement(j) .GT. total_no_elements) then
            pslextraelement(j)=0
            j=j-1
         endif
!         print*,disextraelement(i),disextraface(i)
      enddo
      pslextrapoints=j
!         write (celem,'(I)') pslextraelement(i)
         FILNAM = TRIM (DIRQQ) //'output_'//trim(cnam)//'_water_table_depth.csv'
         open(PSLFILEUNIT, FILE = FILNAM, ERR = 581)
         write(PSLFILEUNIT,'(A)') 'Water_Table_depth(m_below_ground). A negative number means there is surface water with the absolute value the depth of surface water'
         write(PSLFILEUNIT,'(A,*(A,I0))') 'Time(hours)', (', Element-',pslextraelement(j),j=1,pslextrapoints)

    endif


    !^^^^^^ sb 08/03/06
    write (dis2, '(A)',iostat=ios ) 'Simulated discharge at the outlet at every model timestep.'
     if (ios/=0) then
        write(*,'(A)') 'Error writing to the  discharge every timestep at the catchment outlet file  (unit 41 in the rundata file)'
        write(*,'(A)') 'Check it is not open in other software (e.g. Excel)'
               write(*,'(''paused, type [enter] to continue'')')
               read (*,*)
                stop
    endif
   write (dis2, '(A)',iostat=ios ) 'Date_yyyy-mm-dd_hours(iso8601format),Time(hours),Outlet_Discharge(m3/s)'
   write (mas, '(A)',iostat=ios ) 'Spatially Averaged Totals (mm) over the simulation'
    if (ios/=0) then
        write(*,'(A)') 'Error writing to the the mass balance data file  (unit 43 in the rundata file)'
        write(*,'(A)') 'Check it is not open in other software (e.g. Excel)'
               write(*,'(''paused, type [enter] to continue'')')
               read (*,*)
                stop
    endif
    write (mas, '(12(A,1A))') 'Time(Hours)', ',', &
                                 'Cumulative_Precipitation', ',', &
                                 'Cumulative_Canopy_Evaporation', ',', &
                                 'Cumulative_Soil_Evaporation', ',', &
                                 'Cumulative_Transpiration', ',', &
                                 'Cumulative_Aquifer_Flow', ',', &
                                 'Cumulative_Discharge', ',', &
                                 'Canopy_Storage', ',', &
                                 'Snow_Storage', ',', &
                                 'Subsurface_Storage', ',', &
                                 'Land_Surface_Storage', ',', &
                                 'Channel_Storage'
!    write (dis,'(A,f8.2,A)',iostat=ios ) 'Simulated discharge(m3/s) at the outlet - regular timestep', toutput, ' hours. Simulated discharge is the mean value over the timestep with the date at the end of the timestep'
    write (dis,'(A,f8.2,A)',iostat=ios ) 'Simulated discharge(m3/s) at the outlet - regular timestep', toutput, ' hours. Simulated discharge is the mean value over the timestep with the date at the start of the timestep'
    if (ios/=0) then
        write(*,'(A)') 'Error writing to the regular discharge at the catchment outlet file (unit 44 in the rundata file)'
        write(*,'(A)') 'Check it is not open in other software (e.g. Excel)'
               write(*,'(''paused, type [enter] to continue'')')
               read (*,*)
                stop
    endif
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
       WRITE(dis,'(*(A,I0))') 'Date_yyyy/mm/dd_hours(iso8601format),Time(hours),Outlet-',mblink,(',Channel-',disextraelement(j),j=1,disextrapoints)
    else
        WRITE(dis,'(A)')  'Date_yyyy/mm/dd_hours(iso8601format),Time(hours),Outlet-Discharge'
    endif
    uznowt=uznow*(1/TOUTPUT)
    next_hour = INT(uznowt) + 1.0
! sb hotstart first time is correct
    if (BHOTRD) uzold=int(bhotti/TOUTPUT)

    if (bexsy) then

        FILNAM = TRIM (DIRQQ) //'output_'//trim(cnam)//'_sediment_all.csv'
        open (SEDALLUNIT, file=FILNAM)
        FILNAM = TRIM (DIRQQ) //'output_'//trim(cnam)//'_sediment_fine.csv'
        open (SEDFINEUNIT, file=FILNAM)
        write (SEDALLUNIT,'(A)',iostat=ios) 'Sediment discharge at the outlet - All Sediments. This is the mean value over the timestep with the date at the start of the timestep'
        if (ios/=0) then
            write(*,'(A)') 'Error writing to the sed-all-daily-output.csv file'
            write(*,'(A)') 'Check it is not open in other software (e.g. Excel)'
                   write(*,'(''paused, type [enter] to continue'')')
                   read (*,*)
                    stop
        endif
        write (SEDALLUNIT,'(A)') 'Date_yyyy/mm/dd_hours(iso8601format),Time(hours),Outlet-Discharge(kg/s)'
	    write (SEDFINEUNIT,'(A)',iostat=ios) 'Sediment discharge at the outlet - Fine Sediments. This is the mean value over the timestep with the date at the start of the timestep'
        if (ios/=0) then
            write(*,'(A)') 'Error writing to the sed-fine-daily-output.csv file'
            write(*,'(A)') 'Check it is not open in other software (e.g. Excel)'
                   write(*,'(''paused, type [enter] to continue'')')
                   read (*,*)
                    stop
        endif
        write (SEDFINEUNIT,'(A)') 'Date_yyyy/mm/dd_hours(iso8601format),Time(hours),Outlet-Discharge(kg/s)'
        sedav=0.0
    endif

    if (bexcm) then

        FILNAM = TRIM (DIRQQ) //'output_'//trim(cnam)//'_contaminant.csv'
        open (CONTAMUNIT, file=FILNAM)
        write (CONTAMUNIT,'(A)',iostat=ios) 'Contaminant Relative Concentration (contaminant 1) at the outlet. This is the mean value over the timestep with the date at the start of the timestep'
        if (ios/=0) then
            write(*,'(A)') 'Error writing to the contaminant.csv file'
            write(*,'(A)') 'Check it is not open in other software (e.g. Excel)'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
        endif
        write (CONTAMUNIT,'(A)') 'Date_yyyy/mm/dd_hours(iso8601format),Time(hours),Relative_concentration'
    endif


ELSEIF (SIMPOS (1:4) .EQ.'main') THEN

    if (bexsy) then

!*** 1d simulations
        do i=1,nsed
            if ((mblink.eq.0).and.(mbface.eq.0)) then
                sedav=0
            else
                sedav=sedav+QSED(mblink,i,mbface)*RHOSED
            endif
        enddo
        if ((mblink.eq.0).and.(mbface.eq.0)) then
            sedfineav=0
        else
            sedfineav=QSED(mblink,1,mbface)*RHOSED
        endif
    endif
    if (bexcm) then
        if ((mblink.eq.0).and.(mbface.eq.0)) then
            contamav=0
        else
            contamav=CCCC(mblink,top_cell_no,1)
        endif
    endif

    !sb 02/05/07 outlet discharge sent to discharge.txt file
    ! asasume the average discharge over a timestep is QOC
    uznowt=uznow*(1/TOUTPUT)

!***  1d simulations
    if ((mblink.eq.0).and.(mbface.eq.0)) then
        qocav=0
    else
        qocav     = qoc (mblink, mbface)
    endif
    if (ISextradis) then
        do i=1,disextrapoints
            qocavextra(i)= qoc(disextraelement(i),disextraface(i))
        enddo
    endif
    hour_now  = INT(uznowt)

    IF(hour_now<next_hour) THEN  ! not new hour
        qoctot = qoctot + qocav*(uznowt-uzold)
        if (bexsy) then
            sedtot = sedtot + sedav*(uznowt-uzold)
            sedfinetot = sedfinetot + sedfineav*(uznowt-uzold)
        endif
        if (bexcm) then
            contamtot = contamtot + contamav*(uznowt-uzold)
        endif
        if (ISextradis) then
            do i=1,disextrapoints
                qoctotextra(i) = qoctotextra(i) + qocavextra(i)*(uznowt-uzold)
            enddo
        endif
    ELSE
        qoctot = qoctot + qocav*(next_hour-uzold)
        if (bexsy) then
            sedtot = sedtot + sedav*(next_hour-uzold)
            sedfinetot = sedfinetot + sedfineav*(next_hour-uzold)
        endif
        if (bexcm) then
            contamtot = contamtot + contamav*(next_hour-uzold)
        endif

        if (ISextradis) then
            do i=1,disextrapoints
                qoctotextra(i) = qoctotextra(i) + qocavextra(i)*(next_hour-uzold)
            enddo
        endif

        ! if outputhour = next_hour-1.0 it is the mean value over the timestep with the date at the start of the timestep.
        ! if outputhour = next_hour it is the mean value over the timestep with the date at the end of the timestep.
        outputhour = next_hour-1.0

        c = DATE_FROM_HOUR(tih+outputhour*TOUTPUT)
        WRITE(dum,'(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') c(1),'-',c(2),'-',c(3),' ', c(4),':',c(5),':',c(6)
        write(bufdis,'(F20.5)') abs(qoctot)
        bufdis = adjustl(bufdis)
        if (ISextradis) then
            do j=1,disextrapoints
                write(buf(j),'(F20.5)') abs(qoctotextra(j))
                buf(j) = adjustl(buf(j))
            enddo
            WRITE(dis,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',',trim(bufdis),(',',trim(buf(j)),j=1,disextrapoints)
        else
            WRITE(dis,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',',trim(bufdis)
        endif
        if (bexsy) then
                write(bufdis,'(F20.5)') sedtot
             bufdis = adjustl(bufdis)
            write(SEDALLUNIT,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',', trim(bufdis)
                write(bufdis,'(F20.5)') sedfinetot
             bufdis = adjustl(bufdis)
            write(SEDFINEUNIT,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',', trim(bufdis)
        endif
        if (bexcm) then
                 write(bufdis,'(F20.5)') contamtot
             bufdis = adjustl(bufdis)
           write(CONTAMUNIT,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',', trim(bufdis)
        endif
        DO i = next_hour+1, hour_now
            next_hour = i
           outputhour = next_hour-1
           c = DATE_FROM_HOUR(tih+outputhour*TOUTPUT)
            WRITE(dum,'(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') c(1),'-',c(2),'-',c(3),' ', c(4),':',c(5),':',c(6)
            write(bufdis,'(F20.5)') abs(qocav)
            bufdis = adjustl(bufdis)
            if (ISextradis) then
                do j=1,disextrapoints
                    write(buf(j),'(F20.5)') abs(qocavextra(j))
                    buf(j) = adjustl(buf(j))
                enddo
                WRITE(dis,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',',trim(bufdis),(',',trim(buf(j)),j=1,disextrapoints)
            else
                WRITE(dis,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',',trim(bufdis)
            endif
            if (bexsy) then
                write(bufdis,'(F20.5)') sedav
                bufdis = adjustl(bufdis)
                write(SEDALLUNIT,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',',trim(bufdis)
                write(bufdis,'(F20.5)') sedfineav
                bufdis = adjustl(bufdis)
                write(SEDFINEUNIT,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',',trim(bufdis)
            endif
            if (bexcm) then
                write(bufdis,'(F20.5)') contamav
                bufdis = adjustl(bufdis)
                write(CONTAMUNIT,'(A,A1,F0.3,*(A1,A))') trim(dum),',',outputhour*TOUTPUT,',',trim(bufdis)
            endif
        ENDDO
        qoctot    = qocav * (uznowt-next_hour)
        if (bexsy) then
            sedtot    = sedav * (uznowt-next_hour)
            sedfinetot    = sedfineav * (uznowt-next_hour)
        endif
        if (bexcm) then
            contamtot    = contamav * (uznowt-next_hour)
        endif
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
        ! quite complicated so it outputs nicely.
        write(bufmb(6),'(F12.3)') uznow
        bufmb(6) = adjustl(bufmb(6))
        do i=7,17
            write(bufmb(I),'(F12.3)') balanc(I)* 1000 / carea
            bufmb(I) = adjustl(bufmb(I))
        enddo
        write (mas, '(11(A,1a),A)') (trim(bufmb(i)), ',', i=6,16),trim(bufmb(17))
        icounter2 = icounter2 + 24
        if (ISextrapsl) then
            write(PSLFILEUNIT,'(f10.2,*(1a,f10.2))') uznow,  (',', zgrund(pslextraelement(i)) - zvspsl(pslextraelement(i)), i=1, pslextrapoints)
        endif
    endif
    uzold = uznowt


!*** temp sb 250925 for when doing 1d simulations
    if ((mblink.eq.0).and.(mbface.eq.0)) then
        qocav=0
    else
           qocold     = qoc (mblink, mbface)
    endif

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
581  CALL ERROR(FFFATAL,1069,PPPRI,0,0,   'no or incorrect data in input_CATCH_water_table_depth file')


END SUBROUTINE FROUTPUT



!> Writes one discharge value using the configured mass-balance face sign convention.
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



!> Writes one timestamped discharge record using the configured face sign convention.
SUBROUTINE write_dis2(mbface, qoo, tme)
INTEGER, INTENT(IN)            :: mbface
INTEGER                        :: c(6)
DOUBLEPRECISION, INTENT(IN)    :: qoo, tme
DOUBLEPRECISION                :: qd
CHARACTER(128)                 :: dum
character(len=32) :: bufdis2

IF((mbface==1) .OR. (mbface==2)) THEN
    qd = qoo
ELSE
    qd = -qoo
ENDIF
c = DATE_FROM_HOUR(tih + tme)
WRITE(dum,'(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') c(1),'-',c(2),'-',c(3),' ', c(4),':',c(5),':',c(6)
!WRITE(dum,'(2(I2.2,A),I4.4,3(A,I2.2))') c(1),'-',c(2),'-',c(3),'T', c(4),':',c(5),':',c(6)
write(bufdis2,'(F20.5)') qd
bufdis2 = adjustl(bufdis2)
WRITE(dis2,'(A,A1,F0.5,A1,A)') TRIM(dum), ',',tme, ',',TRIM(bufdis2)
END SUBROUTINE write_dis2



!> Writes result-file control headers and opens unformatted result datasets.
!>
!> `FRRESC` serialises output class definitions and common model metadata to the
!> legacy results file, then opens the unformatted result files used by
!> [[frresp]] for selected output sets and data classes.
!>
!> The header contains the SHETRAN version, result filename stem, model
!> dimensions, component file units, output set/class definitions, element/grid
!> topology, VSS connectivity, soil and vegetation tables, bed/channel geometry,
!> boundary and component-enable flags, and VSS soil-property tables. The write
!> order intentionally does not always follow the old COMMON-block ordering
!> because some arrays must be read back in a specific order. `IORES` is filled
!> with the unformatted result-file units opened for the selected output data.
SUBROUTINE FRRESC
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



!> Writes selected results to legacy result files.
!>
!> Output is controlled by user-defined output sets, output classes, and output
!> times. The routine assembles the requested water-flow and component data into
!> output buffers and writes only the records due on the current call.
!>
!> On each call, data are written only for data types marked with `1` in
!> `AIOSTO`, allowing different SHETRAN components to call `FRRESP`
!> selectively. Entry conditions require `NELEE >= 1`, `1 <= NSET <= NSETEE`,
!> each `IODATA(set)` within the `AIOSTO` range, `IOELEM(set)` either a valid
!> element or a valid output class selector, class lists `ICLNUM`/`ICLIST`
!> within element bounds, contaminant-oriented data types `21:38` and `44`
!> using `1 <= IOCORS(set) <= NCON`, and each `IORES(set)` connected for
!> unformatted output.
!
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
SUBROUTINE FRRESP (AIOSTO, RESNOW, NOW)
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
! well abstraction rates - temporary legacy note, 1996-06-28
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



!> Sorts active elements for component execution and output ordering.
!>
!> Elements are sorted by descending surface-water elevation. If no surface
!> water is present in a grid square or bank element, the water-table elevation
!> is used instead; if no surface water is present in a channel link, the channel
!> bed elevation is used. Surface-water elevations and element indices are stored
!> in column 1 of `ELEV` and `ISTEMP`; water-table elevations and indices are
!> stored in column 2.
SUBROUTINE FRSORT

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



!> Reads and initialises bank water-level/depth data.
!>
!> `INBK` reads bank-component input data and sets bank water-surface elevations
!> and related bank state used by OC, VSS, sediment, and contaminant routines.
!> The `INTYPE` input methods are:
!>
!> | `INTYPE` | Meaning |
!> |:---------|:--------|
!> | 1 | Set each value from the adjacent grid value, or from the adjacent bank-full elevation for ground level. |
!> | 2 | Set all values from the supplied default value. |
!> | 3 | Read one value for each data class defined in the output definition file. |
!> | 4 | Read one value for each bank element. |
!>
!> Bank widths are not set here. The routine uses bank input unit `BKD`, class
!> lists `ICLIST`/`ICLNUM`, element references `ICMREF`, and bank-full elevations
!> `ZBFULL`; it updates `NMC`, `NRAINC`, `NVC`, `RHOSAR`, `ZGRUND`, `HRF`, `SD`,
!> and `ZVSPSL`, using `IDUM` and `DUMMY` as workspace.
SUBROUTINE INBK
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



!> Initialises the contaminant component and contaminant interface arrays.
!>
!> The routine reads contaminant data via [[cmmod:cmrd]], checks tabulated
!> spatially variable concentrations, builds column/link geometry terms, sets
!> contaminant storage coefficients, interpolates initial column concentrations,
!> and initialises plant uptake data when enabled.
!>
!> `INCM` sets contaminant scaling constants before any solve-time coefficients
!> are assembled:
!>
!> \[
!> Z2 = 50,\qquad D0 = 10^{-3},\qquad OODO=1/D0,
!> \]
!>
!> \[
!> Z2SQ=Z2^2,\qquad Z2OD=Z2/D0,\qquad Z2SQOD=Z2^2/D0.
!> \]
!>
!> The finite-difference weighting is initialised as fully implicit through
!> `SGMA=1`, `SGSQ=SGMA**2`, and `OMSGMA=1-SGMA`. Contaminant decay is scaled
!> for the solver as
!>
!> \[
!> GCPLA_c = GGLMSO_c\,Z2SQOD.
!> \]
!>
!> For each soil type and contaminant, the soil reference distribution
!> coefficient is reconstructed from sediment particle fractions and
!> particle-size distribution coefficients:
!>
!> \[
!> KDDSOL_{s,c} = \sum_j SOSDFN_{s,j}\,KDDLS_{j,c}.
!> \]
!>
!> If the sediment component is inactive, `INCM` creates a neutral sediment
!> interface: three sediment fractions, no loose/deposited sediment mass, first
!> fraction equal to one, zero sediment fluxes, and bed soil/porosity inferred
!> from the bank soil at the exposed channel bed. This gives the contaminant
!> component consistent sediment arrays without running sediment transport.
!>
!> Column geometry is prepared from VSS layering. `NCOLMB` is set to each
!> column's bottom active layer, `ZCOLMB` stores the corresponding node
!> elevation, and the scaled cell thickness workspace is
!>
!> \[
!> KSP_{e,k}=DELTAZ_{e,k}/Z2.
!> \]
!>
!> Lateral overlap arrays `NOL`, `NOLBT`, `NOLCE`, `NOLCEA`, and `JOLFN` are
!> built from `JVSACN`, `JVSDEL`, and `DELTAZ`. Where an overlap spans two
!> cells, `JOLFN` stores the fractional contribution on the legacy integer scale
!> 32500, for example
!>
!> \[
!> JOLFN =
!> \left\lfloor
!> 32500\,\frac{DELTAZ_k}{DELTAZ_k+DELTAZ_{k+1}}
!> \right\rfloor .
!> \]
!>
!> For each channel link, the routine derives the bed-deep cell numbers and
!> fractional coverage (`NCEBD`, `FNCEBD`) on both adjacent banks from the
!> specified deep-bed thickness `DBDI/Z2` and reconciles the two bank overlap
!> systems so all bank soil below the channel is accounted for. It then sets the
!> bed-surface and bed-deep storage coefficients:
!>
!> \[
!> ACPBSG_l = DBS\,CWIDTH_l/Z2^2,
!> \]
!>
!> \[
!> ACPBI_l =
!> \frac{1}{2}\left(\sum \Delta z^\*_{bank}\right)CWIDTH_l/Z2
!> - ACPBSG_l,
!> \]
!>
!> where the summed scaled bank thickness excludes the parts outside the
!> bed-surface/deep-bed region.
!>
!> Link initial concentrations are set to the incoming concentration `CCAPIN`
!> in the deep-bed, bed-surface, and stream-water cells. Initial stream-bed
!> moisture is the thickness-weighted average over the two adjacent bank regions
!> participating in the bed layers, capped by bed porosity:
!>
!> \[
!> THBED_l =
!> \min\left(PBSED_l,\frac{\sum_k VSTHE_k w_k}{\sum_k w_k}\right).
!> \]
!>
!> Initial bed particle fractions combine loose sediment and parent bed
!> material:
!>
!> \[
!> FBBEDO_{l,j} =
!> \frac{DLS_l\,CWIDTH_l\,FBETA_{l,j}
!>       +(ACPBI_l-ACPBSG_l)Z2^2\,SOSDFN_{NSOBED_l,j}}
!>      {DLS_l\,CWIDTH_l +(ACPBI_l-ACPBSG_l)Z2^2}.
!> \]
!>
!> For soil and bank columns, old-state flow and concentration arrays are
!> initialised from current water-flow state. Surface input and bottom flux use
!>
!> \[
!> QIO_e=-PNETTO_e\,AREA_e,\qquad
!> QQRFO_e=QVSV_{NCOLMB(e),e}\,AREA_e,
!> \]
!>
!> and surface-water depth is stored as `DSWO = HRF - ZGRUND`. Bank columns use
!> an L-shaped correction factor
!>
!> \[
!> \rho = \frac{AREA_{bank}/CLENTH_l}
!>             {AREA_{bank}/CLENTH_l + 0.5\,CWIDTH_l},
!> \]
!>
!> to blend bank and associated-link water contents and vertical velocities
!> where the contaminant column represents both bank soil and channel-underflow
!> geometry.
!>
!> If `CMRD` marked an initial concentration as spatially variable, `INCM`
!> calls `ALINTP` to interpolate the category-specific concentration/depth table
!> onto every active column cell and copies the result into both current and old
!> mobile/dead-space concentration arrays (`CCCC`, `SSSS`, `CCCCO`, `SSSSO`).
!> Finally, plant uptake data are initialised through [[inpl]] when `ISPLT` is
!> enabled.
SUBROUTINE INCM (ISSDON)
!----------------------------------------------------------------------*
!
!  INITIALISATION SUBROUTINE FOR CONTAMINANT COMPONENT
!
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


!DOUBLEPRECISION FNDUM (2), FOLDUM (2), KSPDUM (NELEE, LLEE), ROH (LLEE)
DOUBLEPRECISION FNDUM (2), FOLDUM (2), KSPDUM (total_no_elements,top_cell_no+1), ROH (LLEE)
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
DOUBLEPRECISION DUMMYCONC(total_no_elements,top_cell_no)


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
! sb temp fix 09022026
! this is crashing out as nsobed is sometimes undefined.
! if it is undefined set it equal to soil type 1
!  this seems to be a problem only when both sediment and solute coponents are running.
       if (NSOBED (NLINK)==0) NSOBED (NLINK)=1

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
       DELTAZ, ZVSNOD, DUMMYCONC )
      DO 385 NCL = total_no_links + 1, total_no_elements
         DO 390 NCE = NCOLMB (NCL), NCETOP
            CCCC (NCL, NCE, NCONT)= DUMMYCONC (NCL,NCE)
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



!> Reads evapotranspiration input and initialises ET state.
!>
!> `INET` reads meteorological/vegetation mode flags, canopy and aerodynamic
!> parameters, time-varying vegetation/canopy/root-resistance tables, and root
!> density functions used by [[etmod]].
!>
!> It assumes meteorological-site codes and vegetation codes have already been
!> read by the global initialisation routines. Variable names follow the IH SHE
!> Report 8 convention used by the legacy manual and code.
!>
!> Shared inputs are:
!>
!> | Group | Variables |
!> |:------|:----------|
!> | ET and meteorological file units | `EPD`, `ETD`, `MED`, `PRD`, `PRI` |
!> | Run dimensions | `NEL`, `NGDBGN`, `NM`, `NRAIN`, `NV` |
!> | Restart control | `BHOTRD` |
!> | Imported ET dimensions | `NUZTAB`, `NVBP`, `NVEE` |
!>
!> Initialised shared state is:
!>
!> | Group | Variables |
!> |:------|:----------|
!> | ET timing | `DTMET`, `EPTIME`, `METIME`, `TIMEUZ` |
!> | Vegetation/root state | `NRD`, `CLAI`, `RDL`, `PLAI`, `VHT`, `RDF` |
!> | Rainfall and canopy storage | `P`, `PINP`, `CSTORE` |
!> | ET mode/control flags | `MEASPE`, `MODE`, `NF`, `BMETP`, `BINETP`, `BMETAL`, `BAR` |
!> | Time-varying parameter controls | `MODECS`, `MODEPL`, `MODECL`, `MODEVH`, `NCTCST`, `NCTPLA`, `NCTCLA`, `NCTVHT` |
!> | Canopy/aerodynamic/resistance tables | `CB`, `CK`, `CSTCAP`, `CSTCA1`, `RA`, `RC`, `RTOP`, `PLAI1`, `CLAI1`, `VHT1` |
!> | Soil-moisture-tension tables | `PS1`, `RCF`, `FET` |
!> | Time-varying ratio/time tables | `RELCST`/`TIMCST`, `RELPLA`/`TIMPLA`, `RELCLA`/`TIMCLA`, `RELVHT`/`TIMVHT` |
!>
!> Key ET variables and units are:
!>
!> | Variable | Meaning | Units |
!> |:---------|:--------|:------|
!> | `RA` | Aerodynamic resistance. | s/m |
!> | `RC` | Stomatal/canopy resistance. | s/m |
!> | `CSTCAP` | Canopy storage capacity. | mm |
!> | `CSTORE` | Canopy storage. | mm |
!> | `CK` | Canopy drainage parameter. | mm/s |
!> | `CB` | Canopy drainage parameter. | 1/mm |
!> | `ZO` | Zero-plane displacement. | m |
!> | `ZD` | Roughness height. | m |
!> | `ZU` | Height of anemometer. | m |
!> | `PS1` | Average soil-moisture tension. | m |
!> | `RCF` | Canopy resistance corresponding to `PS1`. | s/m |
!> | `FET` | Actual/potential evapotranspiration ratio `EA/EP`. | nondimensional |
!> | `RDF` | Root distribution function. | nondimensional |
!> | `PLAI` | Ground-cover index. | nondimensional |
!> | `CPLAI` | Canopy leaf-area index. | nondimensional |
!> | `VHT` | Canopy height. | m |
!> | `MEASPE` | `0` if potential evaporation is not measured; `1` if measured. | flag |
!> | `DTMET` | Timestep for full meteorological-data input. | hr |
!> | `DTMET2` | Timestep for precipitation-data input. | hr |
!> | `DTMET3` | Timestep for potential-evaporation-data input. | hr |
SUBROUTINE INET
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
!DO 10 I = 1, NVEE
DO 10 I = 1, NV
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

!new code 10202026 BMETDATES added
! if true then the prd, epd and temperature files contain dates in the first column
! for backwards compatibility the default is false and BMETDATES will not be opresent in line ET1
BMETDATES=.False.
READ (ETD, 60) BMETP, BINETP, BMETAL, BMETDATES
   60 FORMAT (4L7)
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
!         READ (ETD, 210) RELCST (I, JJ), TIMCST (I, JJ)
!  210 FORMAT           (2G7.3)
         READ (ETD, *) RELCST (I, JJ), TIMCST (I, JJ)
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
!         READ (ETD, 210) RELPLA (I, JJ), TIMPLA (I, JJ)
         READ (ETD, *) RELPLA (I, JJ), TIMPLA (I, JJ)
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
!         READ (ETD, 210) RELCLA (I, JJ), TIMCLA (I, JJ)
         READ (ETD, *) RELCLA (I, JJ), TIMCLA (I, JJ)
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
!         READ (ETD, 210) RELVHT (I, JJ), TIMVHT (I, JJ)
         READ (ETD, *) RELVHT (I, JJ), TIMVHT (I, JJ)
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

 567  CALL ERROR(FFFATAL,1063,PPPRI,0,0,  'no data in precipitation time series (prd) file')
 568  CALL ERROR(FFFATAL,1064,PPPRI,0,0,  'no data in potential evaporation time series (epd) file')
 569  CALL ERROR(FFFATAL,1065,PPPRI,0,0,   'no data in met data time series (med)1060 file')
 570  CALL ERROR(FFFATAL,1066,PPPRI,0,0,   'no data in air temp - high file')
 571  CALL ERROR(FFFATAL,1067,PPPRI,0,0,   'no data in air temp - low file')
END SUBROUTINE INET
! 17/7/96
!
!-------------------------



!> Reads global frame data shared by multiple components.
!>
!> This includes model size, simulation dates, grid spacing, output controls,
!> component execution flags, meteorological/vegetation/soil codes, ground
!> levels, link maps, printing controls, extra output settings, and ZQ setup-file
!> references.
!>
!> Common data read and initialised include:
!>
!> | Data group | Variables |
!> |:-----------|:----------|
!> | Organisation and file units | `FRD`, `MED`, `ETD`, `UZD`, `OCD`, `SZD`, `SMD`, `PRI`, `RES`, `HOT`, `SED` |
!> | Job title | run title text |
!> | Model size | `NX`, `NY` |
!> | Simulation start time | `ISYEAR`, `ISMTH`, `ISDAY`, `ISHOUR`, `ISMIN` |
!> | Simulation end time | `IEYEAR`, `IEMTH`, `IEDAY`, `IEHOUR`, `IEMIN` |
!> | Half-grid spacing | `DXIN` in x direction, `DYIN` in y direction |
!> | Printing control | `DTAO`, `IAOUT`, `BINFRP`, `BFRTS1`, `BFRTS2`, `BSTORE` |
!> | Printed-result selection | `BPPNET`, `BPEPOT`, `BPQOC`, `BPDEP`, `BPQF`, `BPQH`, `BPQSZ`, `BPHSZ`, `BPBAL` |
!> | Component execution control | `BEXET`, `BEXUZ`, `BEXOC`, `BEXSZ`, `BEXSM` |
!> | Counts | `NM`, `NRAIN`, `NV`, `NS` |
!> | River lining parameters | `BLOWP`, `DB`, `CCB` |
!> | Default met/rain/vegetation/soil codes | `IDMC`, `IDRA`, `IDVE`, `IDS1`, `IDS2` |
!> | Elevations | `ZGRUND`, `ZBED` |
!> | Distributed codes | `NMC`, `NRAINC`, `NVC`, `NSC1`, `NSC2`, `INGRID` |
SUBROUTINE INFR
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

write(PPPRI,*)
write(PPPRI,*)
write(PPPRI,'(A)') ' SHETRAN file folder = '
write(PPPRI,'(1X,A)') DIRQQ
write(PPPRI,'(A)') ' SHETRAN rundata name = '
write(PPPRI,'(A)') ' rundata_'//trim(cnam)//'.txt'
write(PPPRI,*)
write(PPPRI,*)
write(PPPRI,*)

!
!     READ AND PRINT JOB TITLE.
!:FR1
WRITE(PPPRI,'(A)') 'Catchment Name '
WRITE(PPPRI,'(A)') '************** '
READ (FRD,'(A)') TITLE
   30 FORMAT (20A4)
WRITE(PPPRI,'(A)') TITLE
40 FORMAT (/  20A4, //, 100('='))

WRITE(PPPRI,*)
WRITE(PPPRI,'(A)') 'Fixed array sizes in this version of SHETRAN '
WRITE(PPPRI,'(A)') '******************************************** '
WRITE(PPPRI,'(A)') 'Grid points in x,y directions, river links, total no of elements. THESE ARE THE MOST IMPROTANT ONES'
WRITE(PPPRI,'(4(A,I0))') ' NXEE = ',nxee, '  NYEE = ', nyee, '  NLFEE = ',nlfee, '  NELEE = ',nelee
WRITE(PPPRI,*)
WRITE(PPPRI,'(A)') 'Grid points in vertical'
WRITE(PPPRI,'(1(A,I0))') ' LLEE = ',llee
WRITE(PPPRI,*)
WRITE(PPPRI,'(A)') 'Vegetation types, soil typess (NVEE also used for number of precipitation and pet stations)'
WRITE(PPPRI,'(2(A,I0))') ' NVEE = ',nvee, '  NSEE = ', nsee
WRITE(PPPRI,*)
WRITE(PPPRI,'(A)') 'Tables in the VSS component, time varying veg breakpoints, Tables in the ET component (max number of PSI/RCF/FET values, Maximum number of ssoi layers'
WRITE(PPPRI,'(4(A,I0))') ' NVSEE = ',NVSEE, '  NVBP = ', NVBP, '  NUZTAB = ',NUZTAB, '  NLYREE = ',NLYREE
WRITE(PPPRI,*)
WRITE(PPPRI,'(A)') 'Maximum number of elements(Grids,banks and links) in a row, Tables used in OC component, sediment sze fractions'
WRITE(PPPRI,'(3(A,I0))') ' NXOCEE = ',NXOCEE, '  NOCTAB = ', NOCTAB, '  NSEDEE = ',NSEDEE
WRITE(PPPRI,*)
WRITE(PPPRI,'(A)') 'Number of contaminants, number of overlaps, number of plants in an element, total number of plants for contaminants'
WRITE(PPPRI,'(4(A,I0))') ' NCONEE = ',NCONEE, '  NOLEE = ', NOLEE, '  NPLTEE = ',NPLTEE, '  NPELEE = ',NPELEE
WRITE(PPPRI,*)
WRITE(PPPRI,'(A)') 'Number of snow meltwater slugs, Size of internal tables for channel conveyance'
WRITE(PPPRI,'(2(A,I0))') ' max_no_snowmelt_slugs = ',max_no_snowmelt_slugs, '  NXSCEE = ', NXSCEE
WRITE(PPPRI,*)


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



!> Initialises contaminant plant-uptake arrays.
!>
!> `INPL` initialises the SHETRAN-UK plant contaminant migration component
!> (MPL). The current implementation maps vegetation classes to plant uptake
!> compartments and root fractions, including legacy hard-coded plant parameters.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-03-18 | JE | 3.4 | Implemented the MPL plant contaminant migration component initialisation. |
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



!> Reads snowmelt component input and initialises snowpack state.
!>
!> Key snowmelt variables and units are:
!>
!> | Variable | Meaning | Units |
!> |:---------|:--------|:------|
!> | `UNIFSD` | Snow depth when a uniform initial snow depth is supplied. | mm snow |
!> | `SD` | Snow depth. | mm snow |
!> | `DDF` | Degree-day factor. | mm/s/C |
!> | `RHOS` | Specific gravity of snow. | - |
!> | `TSIN` | Initial snow temperature. | C |
!> | `TS` | Snow temperature. | C |
!> | `NSMC` | Number of meltwater slugs being routed through the snowpack. | - |
!> | `MSM` | Snowmelt method flag: `1` degree-day, `2` energy budget. | - |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1981-03 | JCB | - | Created snowmelt component (SM). |
!> | 1989-09 | GP | 2.1 | SHE88 implementation on Newcastle AMDAHL. |
!> | 1990-06 | GP | 2.2 | Variable snowpack, low-temp correction, shallow pack, SHETRAN amendments. |
!> | 1992-11 | SPA | 3.x | Removed incorrect snowpack temperature control, further low-temp correction. |
!> @endhistory
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



!> Dummy ET initialisation used when the ET component is disabled.
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



!> Dummy overland/channel initialisation used when OC is disabled.
!>
!> This routine belongs to the legacy SHETRAN-UK dummy component set (DUM),
!> which contains dummy versions of OC, ET, UZ, SZ, and EXSZOC routines. These
!> minimal dummy components are not currently used.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-06 | GP | 0.1 | Added dummy components for use with V-catchment tests. |
!> | 1991-12 | GP | 0.2 | Reduced to minimal versions, not currently used. |
SUBROUTINE DINOC
!

!
WRITE ( *, 1)
    1 FORMAT(// 'ENTER DINOC')
RETURN
END SUBROUTINE DINOC



!> Dummy OC input hook retained for legacy component structure.
SUBROUTINE DOCIN
!

!
RETURN
END SUBROUTINE DOCIN



!> Checks spatially variable contaminant concentration tables.
!>
!> `MUERR2` verifies that category counts, table lengths, water-depth breakpoints,
!> and concentration values are valid before the contaminant initialisation uses
!> them to interpolate grid and bank concentrations.
SUBROUTINE MUERR2 (CPR, total_no_elements, NELEE, total_no_links,      &
    MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCON, NCONEE, NUM_CATEGORIES_TYPES,  NTAB, NCATTY, ISCNSV,          &
    TABLE_CONCENTRATION, TABLE_WATER_DEPTH, LDUM)

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
