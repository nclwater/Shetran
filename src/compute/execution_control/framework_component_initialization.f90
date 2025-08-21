MODULE framework_component_initialization
! Module for framework component initialization functionality
! Extracted from FRmod.f90 as part of refactoring

   USE framework_spatial_setup, ONLY : FRIND
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

   ! Module variables needed by component initialization
   CHARACTER (LEN=80) :: TITLE
   DOUBLEPRECISION :: TSH, TCH
   DOUBLEPRECISION :: ALLOUT, DTAO
   LOGICAL :: BFRTS1, BFRTS2, BINFRP, BSOFT
   LOGICAL :: BSTORE, BPPNET, BPEPOT
   LOGICAL :: BPQOC, BPDEP, BPQF, BPQH, BPQSZ, BPHSZ, BPBAL, BPSD
   INTEGER :: IAOUT

   PUBLIC :: INCM, INBK, INET, INFR, INPL, INSM, DINET, DINOC, DOCIN
   PUBLIC :: TITLE, TSH, TCH, BSOFT, BSTORE  ! Export variables needed by other modules

CONTAINS

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

210         END DO
!                 Set root density function
            D1DUM = DELONE (JPLTY)
            RDUM = CLAI (JPLTY) / PF2MAX (JPLTY)



            GMCBBO (NCL, JPLANT) = RDUM * D1DUM
!                 Initialise old value for mass in compartment b

200      END DO

100   END DO
      RETURN
   END SUBROUTINE INPL

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


      LOGICAL :: LDUM1(total_no_elements), ISCNSV (NCONEE)
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

9876  END DO
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
2           END DO
            JLYR = 0
            find_layer: DO
               JLYR = JLYR + 1
               IF (NLYRBT (NBK (1), JLYR) .GE.NHBED (NLINK, 1) ) EXIT find_layer
            END DO find_layer
            NSOBED (NLINK) = NTSOIL (NBK (1), JLYR - 1)
            PBSED (NLINK) = VSPOR (NSOBED (NLINK) )
!                             SET BED SOIL TYPE AND POROSITY, BASED ON
!                             THE SOIL AT THE BOTTOM OF THE EXPOSED FACE
!                             OF BANK 1

1        END DO
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

5        END DO
         DO 6 JSOIL = 1, NSEE
            SOSDFN (JSOIL, 1) = SOFN (JSOIL, 1)
            SOSDFN (JSOIL, 2) = SOFN (JSOIL, 2)
            SOSDFN (JSOIL, 3) = SOFN (JSOIL, 3)

6        END DO
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
72          END DO
            KDDSOL (JSOIL, NCONT) = asum
71       END DO
!                             SET REFERENCE DISTRIBUTION COEFFICIENT FOR
!                             SOIL TO MATCH THAT SPECIFIED FOR THE
!                             SEDIMENT PARTICLE SIZE GROUPS


9     END DO
      DO 10 NCL = total_no_links + 1, total_no_elements
         ZCOLMB (NCL) = ZVSNOD (NCOLMB (NCL), NCL)




10    END DO
!                             SET ELEVATION OF
!                             BOTTOM CELLS IN SOIL COLUMNS
! set up temporary array for use until full vss coding completed
      DO 13 NCL = 1, total_no_elements
         DO 14 NCE = NLYRBT (NCL, 1), top_cell_no
            KSPDUM (NCL, NCE) = DELTAZ (NCE, NCL) / Z2
14       END DO
         KSPDUM (NCL, top_cell_no + 1) = KSPDUM (NCL, top_cell_no)





13    END DO
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

26          END DO
            NOL (IEL, IFA) = NOLP

            NOLBT (IEL, top_cell_no + 1, IFA) = NOLP + 1

24       END DO


20    END DO
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
                     find_soil_layer: DO
                        NCE = NCE-1
                        asum = asum + KSPDUM (NBK (JBK), NCE+1)
                        IF (asum.GT.DKBED) EXIT find_soil_layer
                     END DO find_soil_layer
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
                  find_overlap: DO
                     NOLP = NOLP + 1
                     DUM1 = SCL * JOLFN (NBK (JBK), NOLP, JFCE (JBK) )
                     asum = asum + DUM1
                     IF (asum.GT.FNDUM (JBK) ) EXIT find_overlap
                  END DO find_overlap
                  JOLDUM (JBK) = NOLP - 1
                  FOLDUM (JBK) = (FNDUM (JBK) - asum + DUM1) / DUM1
!                             OVERLAP NUMBERS AND FRACTIONS ASSOCIATED
!                             WITH THE 1ST ESTIMATES
               ENDIF
            ENDIF

102      END DO
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
107         END DO
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
110         END DO
            asum = asum - FNCEBD (NLINK, JBK) * KSPDUM (NBK (JBK), &
               NCEBD (NLINK, JBK) + 1)
            asum = asum - (one - FHBED (NLINK, JBK) ) * KSPDUM (NBK ( &
               JBK), NHBED (NLINK, JBK) + 1)
108      END DO
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
126                  END DO
                  ENDIF
               ENDIF
!                             ADJUST SCALED TRANSMISIVITIES FOR BANKS TO
!                             INCLUDE THE PATHS FOR FLOW BELOW CHANNEL,
!                             IN THE DIRECTION OF CHANNEL
122         END DO


120      END DO
!                             uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu
         DO 130 JBK = 1, 2
            NCEAB (NLINK, JBK) = NHBED (NLINK, JBK)

130      END DO
!                             SET THE NUMBER, NCEAB, FOR THE LOWEST
!                             CELL WHICH EXCHANGES GROUND WATER WITH
!                             STREAM WATER





100   END DO
!                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      DO 15 NCONT = 1, NCON
!                             xxxxxxx INITIALISE VARIABLES WHICH xxxxxxx
!                             xxxxxx DEPEND ON CONTAMINANT NUMBER xxxxxx
         CCAPIO (NCONT) = CCAPI (NCONT)
         IIICFO (NCONT) = IIICF (NCONT)
!                             SET INITIAL VALUES IN 'OLD' ARRAYS

15    END DO
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

17       END DO
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
162         END DO
            NCE = NHBED (NLINK, JBK) + 1
            DUMK = FHBED (NLINK, JBK) * KSPDUM (ICMBK (NLINK, JBK), &
               NCE)
            asumK = asumK + DUMK
            asum = asum + VSTHE (NCE, NBK (JBK) ) * DUMK
160      END DO
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

200      END DO
!                             SET INITIAL VALUES FOR THE PARTICLE SIZE
!                             FRACTIONS IN THE STREAM AND BED



16    END DO
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


51       END DO
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
55          JAL = JAL + 1
            IF (ICMREF (NLINKA, JAL + 4) .NE.NCL) GOTO 55
            JFLINK = ICMREF (NLINKA, JAL + 8)
!                             NUMBER FOR FACE ASSOCIATED WITH LINK
            DBK = cellarea (NCL) / CLENTH (NLINKA)
            DMULT = DBK / (DBK + half * CWIDTH (NLINKA) )
            DO 56 NCE = NLYRBT (NCL, 1) - 1, NCEBD (NLINKA, JBK)
               ROH (NCE) = DMULT
56          END DO
            NCE = NCEBD (NLINKA, JBK) + 1
            ROH (NCE) = one - (one - DMULT) * FNCEBD (NLINKA, JBK)
            DO 57 NCE = NCEBD (NLINKA, JBK) + 2, LLEE
               ROH (NCE) = one

57          END DO

         ENDIF

         DO 52 NCE = 1, top_cell_no  !LLEE  !JE
            GGAMMO (NCL, NCE) = zero
            DO 53 JA = 1, 4
               QQO (NCL, NCE, JA) = QVSH (JA, NCE, NCL)
53          END DO
            DO 54 NCONT = 1, NCON
               CCCCO (NCL, NCE, NCONT) = CCAPIN (NCONT)
               SSSSO (NCL, NCE, NCONT) = CCAPIN (NCONT)
               CCCC (NCL, NCE, NCONT) = CCAPIN (NCONT)
               SSSS (NCL, NCE, NCONT) = CCAPIN (NCONT)

54          END DO


52       END DO
! calculate theta and vert vel for L-shaped bank, if required
         IF (ITYPE.EQ.0) THEN
            DO 58 NCE = NLYRBT (NCL, 1) - 1, top_cell_no
               VSTHEO (NCL, NCE) = VSTHE (NCE, NCL)
               UUAJPO (NCL, NCE) = QVSV (NCE, NCL)
58          END DO
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
59          END DO


         ENDIF

50    END DO
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
390            END DO
385         END DO
         ENDIF
!

380   END DO
!
!     End of new code by SB
!     ---------------------



      IF (ISPLT) CALL INPL
!                       Initialise plant uptake routines
!                                   iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
   END SUBROUTINE INCM

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
10    FORMAT ('1',// T10, '                                E'/T10, &
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
16    FORMAT (/  'SHETRAN VERSION NUMBER: ', F5.1 , &
      &        ' ' )
15    FORMAT (/  'SHETRAN VERSION NUMBER: ', F5.1 )
      WRITE(PPPRI, 17) BANNER
17    FORMAT(/A80/)
!
!     READ AND PRINT JOB TITLE.
!:FR1
      READ (FRD, 30) TITLE
30    FORMAT (20A4)
      WRITE(PPPRI, 40) TITLE
40    FORMAT (/  20A4, //, 100('='))
!
      WRITE(PPPRI, 20)
20    FORMAT (/ ' ^^^ ENTER INFR ^^^')
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
50    FORMAT (10F7.0)
!
!:FR12
      READ (FRD, 30) TITLE
      READ (FRD, 80) DTAO, IAOUT, BINFRP, BFRTS1, BFRTS2, BSTORE, &
         PSTART
80    FORMAT (F7.0, I7, 4L7, F7.0)
!:FR20
      READ (FRD, 30) TITLE
      READ (FRD,85) PMAX, PALFA, QMAX, TMAX, BSOFT

85    FORMAT(4F7.0,L7)
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
100      FORMAT     (10L7)
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
130   FORMAT (10L7)
140   FORMAT (2L7, 2F7.2)
      WRITE(PPPRI, 150) NX, NY
150   FORMAT ('0'//, ' GRID SPECIFICATION'/80('*')//, ' NX = ', I4, &
      &       21X, 'NY = ', I4)
      WRITE(PPPRI, 160) (DXIN (J), J = 1, NXM1)
160   FORMAT ('0', 'H-H GRID SIZES (METERS) IN X-DIRECTION', /, &
      &       (1X,10G11.4))
      WRITE(PPPRI, 170) (DYIN (K), K = 1, NYM1)
170   FORMAT ('0', 'H-H GRID SIZES (METERS) IN Y-DIRECTION', /, &
      &       (1X,10G11.4))
      WRITE(PPPRI, 200)
200   FORMAT (' ', 80('*'))
!
!     CONVERT STARTTIME AND ENDTIME TO HOURS.
      TIH = HOUR_FROM_DATE(ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN)
      TTH = HOUR_FROM_DATE(IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN)
      TTHX = TTH - TIH
      WRITE(PPPRI, 210) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN, IEYEAR, &
         IEMTH, IEDAY, IEHOUR, IEMIN, TTHX
210   FORMAT ('0'//, ' START OF SIMULATION  : ', 5I6, /, &
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
211      FORMAT  (// ' START OF SEDIMENT SIMULATION  : ',5I6, / &
         &            '           AT SIMULATION HOUR  : ',F8.2)
      ENDIF
      IF (BEXCM) THEN
         TCH = HOUR_FROM_DATE(JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN)
         WRITE(PPPRI, 212) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN, (TCH - &
            TIH)
212      FORMAT  (// ' START OF CONTAMINANT SIMULATION  : ',5I6, / &
         &            '              AT SIMULATION HOUR  : ',F8.2)
      ENDIF
!
      WRITE(PPPRI, 215) TMAX
215   FORMAT ('0',//, ' BASIC TIMESTEP (HOURS) :', F8.3)
!
      WRITE(PPPRI, 220) DTAO
220   FORMAT ('0'//, ' PRINTING CONTROL - ALL RESULTS PRINTED AT', &
      &       ' INTERVALS OF DTAO = ', F7.2, ' HOURS.')
!
      IF (.NOT.BSTORE) WRITE(PPPRI, 230)
230   FORMAT ('0'//, ' RESULTS NOT REQUIRED ON FILE STORE.')
!
      IF (BSTORE) WRITE(PPPRI, 240)
240   FORMAT ('0'//, ' RESULTS RECORDED ON FILE STORE.')
!
!     READ AND PRINT NM,NRAIN,NV AND NS.
!:FR28
      READ (FRD, 30) TITLE
      READ (FRD, 250) NM, NRAIN, NV, NS, NLYRCT
250   FORMAT (5I7)
      WRITE(PPPRI, 260) NM, NRAIN, NV, NS, NLYRCT
260   FORMAT ('0'//, ' NO. OF METEOROLOGICAL SITES = ', I3, /, &
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
290   FORMAT (6I7)
      WRITE(PPPRI, 300) IDMC, IDRA, IDVE, IDLYR
300   FORMAT ('0', /, ' DEFAULT METEOROLOGICAL STATION CODE =', I3, /, &
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
303   FORMAT(/ 20A4)
!
      DO 310 I1 = 1, NY
         K = NY + 1 - I1
         READ (FRD, 306) I2, (INGRID (J, K), J = 1, NX)
         IF (BINFRP) WRITE(PPPRI, 306) I2, (INGRID (J, K), J = 1, NX)
306      FORMAT  (I7, 1X, 500I1)
         IF (I2.NE.K) GOTO 312
310   END DO
      GOTO 316
!
!^^^^^^ERROR IN DATA
!
312   CONTINUE
      WRITE(PPPRI, 314) TITLE, I2
314   FORMAT (//2X, 'ERROR IN DATA ', 20A4, //2X, 'IN THE VICINITY OF ', &
      &       'LINE K= ', I5)
      STOP
!
! SET INGRID TO BE ITS INTERNAL VALUES FOR SHE (=0 IN CATCHMENT, -1 OTHE
!
316   DO I = 1, NX
         DO J = 1, NY
            IF (INGRID (I, J) .EQ.1) THEN
               INGRID (I, J) = 0
            ELSE
               INGRID (I, J) = - 1
            ENDIF
         END DO
      END DO
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

958   toutput=24.0
!     INITIALIZATION OF SOME PARAMETERS.
!
959   ALLOUT = DTAO + PSTART
      NXEP1 = NXE+1
      NYEP1 = NYE+1
!
! INITIALISATION OF ISORT ARRAY
!
      DO IEL = 1, total_no_elements
         ISORT (IEL) = IEL
      END DO
!
      WRITE(PPPRI, 430)
430   FORMAT ('0'//, ' EXIT INFR')
!
      RETURN

   END SUBROUTINE INFR

   SUBROUTINE DINET
!
!
      WRITE ( *, 1)
1     FORMAT(// 'ENTER DINET')
      BMETAL = .TRUE.
!     PNET=0.0003
!     PE=0.0
!     EINT=0.0
!     ERZ=0.0
!     DRAIN=0.0
!     ESOIL=0.0
      RETURN
   END SUBROUTINE DINET

   SUBROUTINE DOCIN
!

!
      RETURN
   END SUBROUTINE DOCIN

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
700   FORMAT(20A4)
      READ (SMD, 708) BINSMP
708   FORMAT(L7)
      IF (BINSMP) WRITE(PPPRI, 800) HEAD
800   FORMAT(1H0//1X,20A4)
!
!         READ SNOWMELT DATA
      READ (SMD, 700) HEAD
      READ (SMD, 701) DDF, RHOS, TSIN, NSD, MSM
701   FORMAT(2F7.5,F7.2,2I7)
      RHODEF = RHOS
!         Added by spa, 05/11/92.  Snowpack temp no longer needed
!         for degree day method.  Therefore if msm=1, tsin=0.
      if (msm.eq.1) tsin = zero
      IF (BINSMP) WRITE(PPPRI, 801) DDF, RHOS, TSIN, MSM
801   FORMAT(1H0,'DEGREE DAY FACTOR DDF =',F7.5,1X,'MM/S/C', &
      & 5X,'SNOW SPECIFIC GRAVITY RHOS =',F7.5/ &
      & 5X,'INITIAL SNOW TEMPERATURE =',F7.2,1X,'C'/ &
      & 5X,'SNOWMELT CALCULATED BY DEGREE DAY IF MSM IS 1', &
      & ' AND BY ENERGY BUDGET IF MSM IS 2',5X,'MSM =',I3)
!
      IF (MSM.EQ.1) GOTO 710
!        READ ENERGY BUDGET DATA
      READ (SMD, 700) HEAD
      READ (SMD, 709) ZOS, ZDS, ZUS
709   FORMAT(3F7.5)
      IF (BINSMP) WRITE(PPPRI, 803) ZOS, ZDS, ZUS
803   FORMAT(1H0,'ENERGY BUDGET DATA',3X,'ROUGHNESS ZOS =',F7.5,1X,'M'/ &
      &    21X,'ZERO PLANE DISPLACEMENT ZDS =',F7.5,1X,'M'/ &
      &    21X,'HEIGHT OF ANEMOMETER ZUS =',F7.5,1X,'M')
!
!         METEOROLOGICAL (WINDSPEED) DATA LOCATION
!
      READ (SMD, 700) HEAD
      READ (SMD, 720) (IMET (N), N = 1, NM)
720   FORMAT(10I7)
      IF (BINSMP) THEN
         WRITE(PPPRI, 715)
715      FORMAT  (/' LOCATION OF MET. STATIONS: ' / &
         &    ' STATION NO.    ELEMENT NO.')
         DO 730 N = 1, NM
            WRITE(PPPRI, 735) N, IMET (N)
735         FORMAT    (3X,I4,10X,I4)
730      END DO
      ENDIF
!
!         IS SNOWDEPTH UNIFORM?
!
710   IF (NSD.EQ.0) then
         do 712 iel = ngdbgn, total_no_elements
            rhosar (iel) = rhodef
712      end do
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
703   READ (SMD, 700) HEAD
      READ (SMD, 705) UNIFSD
705   FORMAT(F7.1)
      DO 706 IEL = NGDBGN, total_no_elements
         SD (IEL) = UNIFSD
706   END DO
      IF (BINSMP) WRITE(PPPRI, 802) UNIFSD
802   FORMAT(1H0,1X,'INITIAL SNOWPACK HAS UNIFORM THICKNESS =', &
      & F7.1,1X,'MM')
704   DO 707 IEL = NGDBGN, total_no_elements
!                  SET COUNTER FOR SNOWMELT ROUTINE
         NSMC (IEL) = 0
!                  SET SNOW TEMPERATURES
         TS (IEL) = TSIN
!                  SET SNOWFALL
         SF (IEL) = zero
707   END DO
      RETURN
   END SUBROUTINE INSM

   SUBROUTINE DINOC
!

!
      WRITE ( *, 1)
1     FORMAT(// 'ENTER DINOC')
      RETURN
   END SUBROUTINE DINOC

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
1000  FORMAT(A)
!
1100  FORMAT(L7)
!
1200  FORMAT(10I7)
!
1300  FORMAT(' DEFAULT VALUE ',I7,' USED IN ALL BANK ELEMENTS'/)
!
1400  FORMAT(10F7.0)
!
1500  FORMAT(' DEFAULT VALUE ',F12.3,' USED IN ALL BANK ELEMENTS'/)
!
1800  FORMAT(5(I7,F7.0))
!
2000  FORMAT(' VALUES ALLOCATED TO EACH ELEMENT:'/        3('       ELEMENT   VALUE'))
!
2050  FORMAT(3(I7,2X,I7,6X))
!
2100  FORMAT(' VALUES ALLOCATED TO EACH ELEMENT:'/        3('       ELEMENT     VALUE'))
!
2150  FORMAT(3(I7,F12.3,6X))
!
   END SUBROUTINE INBK

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
      DO I = 1, NVEE
         CSTCAP (I) = 0.
         RC (I) = 0.
         BAR (I) = .FALSE.
         MODE (I) = 0
      END DO
!
!     CHECK IF HOTSTART
!
      IF (.NOT.BHOTRD) THEN
         DO 20 IEL = NGDBGN, total_no_elements
            CSTORE (IEL) = 0.
20       END DO
      ENDIF
!
      DO 40 I = 1, NRAIN
         !PINP (I) = 0.
40    ENDDO !precip_m_per_s(I) = 0.
      precip_m_per_s = 0.
      TIMEUZ = 0.
!
!-----READ PRINTCONTROL PARAMETERS
!:ET1
      READ (ETD, 100) HEAD
      READ (ETD, 60) BMETP, BINETP, BMETAL
60    FORMAT (3L7)
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
80    FORMAT (10I7)
!
!---------------------------------
!  LOOP ON VEGETATION TYPES....
!---------------------------------
!
      DO 430 I = 1, NV
!
         IF (BINETP) WRITE(PPPRI, 90) I
90       FORMAT   ('0'//1X, 'VEGETATION TYPE', I6/1X, 22('*'))
!:ET7
         READ (ETD, 100) HEAD
100      FORMAT   (A)
         IF (BINETP) WRITE(PPPRI, 110) HEAD
110      FORMAT   ('0'//1X, A)
!-------------------------------------
!  READ PARAMETER DATA
!-------------------------------------
         READ (ETD, 120) BAR (I), RA (I), ZU (I), ZD (I), ZO (I), &
            RC (I), MODE (I), NF (I), PLAI (I), CSTCAP (I), CK (I), &
            CB (I), NRD (I), CLAI (I), VHT (I), RDL (I)
120      FORMAT   (L7, 5F7.0, I7/I7, 4F7.0, I7, 3F7.0)
         IF (BINETP) WRITE(PPPRI, 130) MODE (I)
130      FORMAT   ('0', 1X, 'ET COMPONENT WITH MODE', I6, 2X, 'OPERATION')
!
!-----WRITE PARAMETER DATA
         IF (BINETP) WRITE(PPPRI, 140) PLAI (I), CSTCAP (I), CK (I), &
            CB (I), CLAI (I), VHT (I), RDL (I)
140      FORMAT   ('0', 'PARAMETERS'/1X, 10('*')//10X, 'PLAI', F15.8/10X, &
         &         'CSTCAP', F13.8/10X, 'CK', F17.8/10X, 'CB', F17.8/10X, &
         &         'CLAI', F15.8/10X, 'VHT', F16.8/10X, 'RDL', F16.8)
150      IF (BAR (I) .AND.BINETP) WRITE(PPPRI, 160) ZO (I), ZD (I), &
            ZU (I)
160      FORMAT   (' ', 10X, 'VARIABLE RA WITH'/10X, 'ZO', F17.4/10X, 'ZD', &
         &         F18.4/10X, 'ZU', F17.4)
         IF (.NOT.BAR (I) .AND.BINETP) WRITE(PPPRI, 170) RA (I)
170      FORMAT   (' ', 10X, 'CONSTANT RA =', F10.4)
!--------------------------------------------------------
!     READ TABULAR VARIATION OF TIME-VARYING PARAMETERS
!--------------------------------------------------------
!:ET9
         READ (ETD, 100) HEAD
!
!-----READ MODE: 0=CONSTANT; 1=TIME-VARYING
         READ (ETD, 180) MODECS (I), MODEPL (I), MODECL (I), MODEVH (I)
180      FORMAT   (4I7)
!
!-----CHECK MODE FOR TIME-VARYING CSTCAP
         IF (BINETP) WRITE(PPPRI, 190) I, MODECS (I)
190      FORMAT   ('0', 1X, 'MODE FOR CSTCAP FOR VEGETATION', I3, ' IS', &
         &         I3, 3X, ' (0=CONSTANT; 1=TIME-VARYING)')
         IF (MODECS (I) .NE.0) THEN
            NCTCST (I) = 1
            CSTCA1 (I) = CSTCAP (I)
!-----READ NUMBER OF VALUES IN CSTCAP VARIATION TABLE
!:ET11(1/4)
            READ (ETD, 100) HEAD
            READ (ETD, 200) JJJ
200         FORMAT        (I7)
!:ET13(1/4)
            READ (ETD, 100) HEAD
            IF (BINETP) WRITE(PPPRI, 110) HEAD
!-----READ TIME-VARYING CSTCAP VALUES
            DO 230 JJ = 1, JJJ
               READ (ETD, 210) RELCST (I, JJ), TIMCST (I, JJ)
210            FORMAT           (2G7.3)
               IF (BINETP) WRITE(PPPRI, 220) RELCST (I, JJ), TIMCST (I, &
                  JJ)
220            FORMAT           (2G10.3)
230         END DO
         ENDIF
!
!-----CHECK MODE FOR TIME-VARYING PLAI
         IF (BINETP) WRITE(PPPRI, 250) I, MODEPL (I)
250      FORMAT   ('0', 1X, 'MODE FOR PLAI FOR VEGETATION', I3, ' IS', I3, &
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
260         END DO
         ENDIF
!
!-----CHECK MODE FOR TIME-VARYING CLAI
         IF (BINETP) WRITE(PPPRI, 280) I, MODECL (I)
280      FORMAT   ('0', 1X, 'MODE FOR CLAI FOR VEGETATION', I3, ' IS', I3, &
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
290         END DO
         ENDIF
!
!-----CHECK MODE FOR TIME-VARYING VHT
         IF (BINETP) WRITE(PPPRI, 310) I, MODEVH (I)
310      FORMAT   ('0', 1X, 'MODE FOR VHT FOR VEGETATION', I3, ' IS', I3, &
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
320         END DO


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
340         FORMAT        (3F7.2)
            IF (BINETP) WRITE(PPPRI, 110) HEAD
            IF (BINETP) WRITE(PPPRI, 350) (PS1 (I, J), RCF (I, J), &
               FET (I, J), J = 1, N1)
350         FORMAT        (' ', 3F10.2)
         ELSE
            WRITE(PPPRI, 370) RC (I)
370         FORMAT        (' ', 10X, 'CONSTANT RC =', F10.4)
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
390         FORMAT     (2F7.4)
            IF (BINETP) WRITE(PPPRI, 410) DEPTH, RDF (I, J)
            asum = asum + RDF (I, J)
400      END DO
         IF (BINETP) WRITE(PPPRI, 420) asum
410      FORMAT   (' ', 2F15.6)
420      FORMAT   ('0', 1X, 'asum OF RDF VALUES IS', F10.4)
         IF (BAR (I) ) RTOP (I) = LOG ( (ZU (I) - ZD (I) ) / ZO (I) ) ** &
            2 / VKSQ
!
!-----END OF VEGETATION LOOP
!


430   END DO
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

567   CALL ERROR(FFFATAL,1063,PPPRI,0,0,  'no data in prd file')
568   CALL ERROR(FFFATAL,1064,PPPRI,0,0,  'no data in epd file')
569   CALL ERROR(FFFATAL,1065,PPPRI,0,0,   'no data in med file')
570   CALL ERROR(FFFATAL,1066,PPPRI,0,0,   'no data in air temp - high file')
571   CALL ERROR(FFFATAL,1067,PPPRI,0,0,   'no data in air temp - low file')
   END SUBROUTINE INET

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

      LOGICAL :: LDUM(total_no_elements)
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
                  'NCATTY(iel)', 'GT', IZERO, NCATTY (J, I) , NERR, LDUM(J:J))
110         END DO
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
170            END DO
160         END DO
!
!       *TABLE_CONCENTRATION
!       Each value in the table of concentrations must be >= 0
!
            DO 260 NELMTY = 1, NUM_CATEGORIES_TYPES (I)
               DO 270 NTBL = 1, NTAB (NELMTY, I)
                  CALL ALCHK(EEERR, 2106, CPR, NELMTY, NELMTY, NTBL, &
                     IUNDEF, 'TABLE_CONCENTRATION[nmne,ntab]', 'GE', zero1, zero , &
                     TABLE_CONCENTRATION (NELMTY, NTBL, I) , NERR, LDUM)
270            END DO

260         END DO
         ENDIF
!

100   END DO
! 2. Epilogue
! -----------
!
      IF (NERR.GT.0) CALL ERROR(FFFATAL, 2107, CPR, 0, 0, 'Error(s) detected while checking static/initial interface')
!






   END SUBROUTINE MUERR2

END MODULE framework_component_initialization
