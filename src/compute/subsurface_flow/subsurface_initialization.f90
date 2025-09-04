MODULE subsurface_initialization
!----------------------------------------------------------------------*
! VSS component initialization and setup
! Contains VSIN, VSCONC, VSCONL - the main initialization routines
!----------------------------------------------------------------------*
! Extracted from VSmod.f90 as part of refactoring
! Date: 2025-09-04
! Source: VSmod.f90.sav (lines 1963-2191, 830-1447, 1457-1785)
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE subsurface_variables
   USE subsurface_io, ONLY : VSREAD
   USE subsurface_soil_properties, ONLY : VSSOIL, VSFUNC
   USE subsurface_utilities, ONLY : fncell
   USE AL_G, ONLY : ICMREF, NX, NY, NGDBGN
   USE AL_C, ONLY : BHB, BFB, bexbk, DTUZ, deltaz, dummy, DHF, ESOILA, ERUZ, EEVAP, &
      FHBED, ISORT, jvsacn, JVSDEL, idum, icmbk, LFB, LHB, LINKNS, lgb, &
      NWELBT, NWELTP, NVSSPC, NVSWLI, NTSOIL, nhbed, NVC, NRD, nlyrbt, NVSWLT, NVSSPT, NBFACE, NS, nlyr, &
      PNETTO, QVSSPR, QVSBF, QH, QVSWEL, QBKF, QBKB, QVSV, QVSWLI, QVSH, QBKI, &
      tih, UZNEXT, &
      vsd, VSI, VSPSI, VSTHE, VSPOR, WLD, ZVSPSL, zlyrbt, zvsnod, zbeff, INITIALISE_AL_C, TIH
   USE AL_D, ONLY : TTH
   USE UTILSMOD, ONLY : DCOPY
   USE OCmod2, ONLY : GETHRF
   USE mod_load_filedata, ONLY : ALINIT, ALSPRD
   IMPLICIT NONE

   PRIVATE
   ! Public initialization routines
   PUBLIC :: VSIN, VSCONC, VSCONL

CONTAINS

!SSSSSS SUBROUTINE VSIN ()
   SUBROUTINE VSIN ()
!----------------------------------------------------------------------*
! Controls initialisation of VSS component data
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSIN/4.1
! Modifications:
!  GP  20.07.94  written (v4.0 finished 21/10/96)
! RAH  970122  4.1  No long/leading comments or lower-case code.
!                   Amend externals list.  Extend VSFUNC argument list.
!      970512       Swap IVSSTO & VSKR indices (VSCOM1.INC), and scrap
!                   local arrays ICSDUM & CKRDUM.  Similarly, swap
!                   DELTAZ, ZVSNOD & VSPSI (AL.C), and scrap CPSDUM.
!                   Scrap outputs VSETAN & VSKRN (VSCOM1.INC).
!                   Rationalize loops 800 & 950, and initialize.
!                   Generic intrinsics.  Use ISTART more.  Order labels.
!      970522       NWELTP default 1.
!                   Use GOTO for errors; fix error in message 1041.
!      970630       Bring NAQCON,IAQCON from VSINIT.INC; swap indices;
!                   pass to VSREAD,VSCONL.  Use format 9010 for 9020.
!                   Replace NGDBGN with NLF+1.
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P.VSS         LLEE,NVSEE
! Input common
!     ...
! Output common
!     AL.C             NWELBT(NEL),NWELTP(NEL),NVSSPC(NEL)
!                      VSPSI(LLEE,NEL),ZVSPSL(NEL)
!     VSCOM1.INC:      IVSSTO(LLEE,NEL)
!                      VSKR(LLEE,NEL)
!     VSINIT.INC:      NVSERR
      CHARACTER(132) :: MSG
      INTEGER :: IEL, ICL, ILYR, ICBOT, ICTOP, IW, IELIN, ISTART, &
         NAQCON
      INTEGER :: IAQCON (4, NVSEE), ISDUM (LLEE)
      DOUBLEPRECISION DZ, RDUM, ZGI, ZMIN


      DOUBLEPRECISION CDUM1 (LLEE), CDUM2 (LLEE), CDUM3 (LLEE), CDUM4 ( &
         LLEE)
!----------------------------------------------------------------------*

      WRITE(PPPRI, 9010) 'Start', ' '

      NVSERR = 0
      IF (BEXBK) THEN
         ISTART = 1
      ELSE
         ISTART = total_no_links + 1


      ENDIF
! call VSREAD to read from input data file
      CALL VSREAD (NAQCON, IAQCON)


      IF (NVSERR.GT.0) GOTO 8900
! read first lines of time-varying files
      IF (NVSWL.GT.0) READ (WLD, * )
      IF (NVSLF.GT.0) READ (LFB, * )
      IF (NVSLH.GT.0) READ (LHB, * )
      IF (NVSBF.GT.0) READ (BFB, * )



      IF (NVSBH.GT.0) READ (BHB, * )
! call VSCONL and VSCONC to set up connectivity arrays for ...
! ... layers


      CALL VSCONL (NAQCON, IAQCON)
! ... cells


      CALL VSCONC

!no_of_hours_run = INT(TTH - TIH + 1.0d0)
!OPEN(unit=8798, file=TRIM(size_file), action='WRITE')
!WRITE(8798,'(4I10,A)') max_no_snowmelt_slugs, total_no_elements, total_no_links, top_cell_no, &
!              '     max_no_snowmelt_slugs, total_no_elements, total_no_links, top_cell_no'
!WRITE(8798,'(4I10,A)') szmonte, pcmonte, ran2monte1, ran2monte2, '     szmonte, pcmonte, ran2monte1, ran2monte2'
!DO iii=1,szmonte
!    WRITE(8798,'(<SIZE(montec,DIM=2)>I1)') montec(iii,:)
!ENDDO
!CLOSE(8789)
      CALL INITIALISE_VSMOD()
      CALL INITIALISE_AL_C()
! set up cell numbers for wells and springs
!     set defaults
      DO 700 IEL = 1, total_no_elements
         NWELBT (IEL) = 1
         NWELTP (IEL) = 1
         NVSSPC (IEL) = 0

700   END DO
      DO 890 IEL = total_no_links + 1, total_no_elements
         ICBOT = NLYRBT (IEL, 1)

         ZGI = ZGRUND (IEL)
         IW = NVSWLI (IEL)

         IF (IW.GT.0) THEN
            RDUM = ZGI - VSZWLB (IW)
            DO ICL = ICBOT, top_cell_no
               IF (RDUM.LE.ZVSNOD (ICL, IEL) ) GOTO 770
            END DO

770         NWELBT (IEL) = ICL
            RDUM = ZGI - VSZWLT (IW)
            DO ICL = top_cell_no, ICBOT, - 1
               IF (RDUM.GE.ZVSNOD (ICL, IEL) ) GOTO 790
            END DO

790         NWELTP (IEL) = ICL

         ENDIF
         RDUM = VSSPD (IEL)
         IF (GTZERO(RDUM)) THEN
            RDUM = ZGI - RDUM
            DO 820 ICL = ICBOT, top_cell_no
               DZ = ABS (ZVSNOD (ICL, IEL) - RDUM)
               IF (DZ.LE.half * DELTAZ (ICL, IEL) ) GOTO 860
820         END DO
860         NVSSPC (IEL) = ICL

         ENDIF


890   END DO
! call VSSOIL to set up soil property tables



      CALL VSSOIL
! set up initial conditions (read from file unit VSI, if required)
! type 1 - uniform phreatic surface depth, equilibrium psi profile

      IF (INITYP.EQ.1) THEN
         DO 900 IEL = 1, total_no_elements
            ZVSPSL (IEL) = MAX (ZLYRBT (IEL, 1), ZGRUND (IEL) - VSIPSD)


900      END DO
! type 2 - varying phreatic surface level, equilibrium psi profile

      ELSEIF (INITYP.EQ.2) THEN
         READ (VSI, '(A)')


         READ (VSI, * ) (ZVSPSL (IEL), IEL = ISTART, total_no_elements)
! type 3 - 3-dimensional field of psi values (+ init. psl for output)

      ELSE
         READ (VSI, '(A)')

         DO 950 IEL = ISTART, total_no_elements
            READ (VSI, * ) IELIN
            IF (IELIN.NE.IEL) GOTO 8041
            ICBOT = NLYRBT (IEL, 1)
            ICTOP = top_cell_no

            READ (VSI, * ) (VSPSI (ICL, IEL), ICL = ICBOT, ICTOP)
            ZMIN = ZVSNOD (ICBOT, IEL) - half * DELTAZ (ICBOT, IEL)
            DO ICL = ICBOT, ICTOP
               IF (LTZERO(VSPSI(ICL,IEL))) GOTO 940
            END DO
940         ICL = MAX (ICBOT, ICL - 1)

            ZVSPSL (IEL) = MAX (ZMIN, ZVSNOD (ICL, IEL) + VSPSI (ICL, &
               IEL) )

950      END DO


      ENDIF
! set up equilibrium psi profile for types 1 or 2
      IF (INITYP.EQ.1.OR.INITYP.EQ.2) THEN
         DO 1200 IEL = 1, total_no_elements
            DO 1140 ICL = NLYRBT (IEL, 1), top_cell_no
               VSPSI (ICL, IEL) = ZVSPSL (IEL) - ZVSNOD (ICL, IEL)
1140        END DO
1200     END DO


      ENDIF
! set up initial relative conductivities for all elements

      DO 1400 IEL = ISTART, total_no_elements
         DO 1270 ILYR = 1, NLYR (IEL)
            DO 1250 ICL = NLYRBT (IEL, ILYR), NLYRBT (IEL, ILYR + 1) &
               - 1
               ISDUM (ICL) = NTSOIL (IEL, ILYR)
               IVSSTO (ICL, IEL) = 0
1250        END DO

1270     END DO
         ICBOT = NLYRBT (IEL, 1)
         ICTOP = top_cell_no

         CALL VSFUNC ( NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
            VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ISDUM (ICBOT), &
            VSPSI (ICBOT, IEL), IVSSTO (ICBOT, IEL), CDUM1, CDUM2, VSKR ( &
            ICBOT, IEL), CDUM3, CDUM4)

1400  END DO
      WRITE(PPPRI, 9010) 'End', '   '


      GOTO 8900
! Error handling
8041  NVSERR = NVSERR + 1
      WRITE (MSG, 9040) IEL

      CALL ERROR (EEERR, 1041, PPPRI, 0, 0, MSG)
8900  IF (NVSERR.LT.1) RETURN
      WRITE (MSG, 9030) NVSERR

      CALL ERROR(FFFATAL, 1040, PPPRI, 0, 0, MSG)

9010  FORMAT( / '!!',78('#') / 1X,A,' of VSS data ',A,60('#') / 80('#'))

9030  FORMAT(I4,' Errors have occurred in VSS data reading ', &
      &          'or initialisation.')

9040  FORMAT('Error reading VSS initial conditions for element ', &
      &       I4, '.')
   END SUBROUTINE VSIN

!SSSSSS SUBROUTINE VSCONC ()
   SUBROUTINE VSCONC ()
!----------------------------------------------------------------------*
! Sets up cell sizes and connectivity matrix
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSCONC/4.1
! Modifications:
!  GP  20.07.94  written (4.0 completed 960117)
! RAH  970326  4.1  Generic intrinsics.  Move ERROR calls to the end.
!                   New locals ZAQTOP,ZLBOT,ZNODE,ICOL1,ICL0,NCL.
!                   Scrap local ZDUM1.  Automatic type conversion.
!                   Rename locals NDUM,ZDUM2,ZDUM3,Zasum,Zasum1.
!                   Scrap label & GOTO 970 (use MAX(ZERO,VSZMIN)).
!                   Swap subscripts: DELTAZ,ZVSNOD (AL.C).  Move IBANK2.
!                   Move block-IF outside loop 974; execute always.
!                   Labels in order.  Define DELTAZ,ZVSNOD for ICL=1.
!                   Do loop 1100 only if ICL0>0, call ALINIT, & rm loop
!                   1170 (zero sub-cells).  Initialize NRENUM in DATA.
!                   Start at ICL0+1 in search for NLYRBT & don't test
!                   DELTAZ>0.
!      970402       Start at ICOL1 in loop 1600 (instead of GOTO).
!                   Rationalize tests in loop 1500.  Swap subscripts:
!                   JVSACN,JVSDEL, & initialize to 0 (was IUNDEF first).
!                   Declare NCELL,NACELL,ZDIFF.
!      970422       Initialize LRENUM to 0 (was IUNDEF) & test NCLYR<=0.
!      970423       Start at NLF+1 (was test type=3) in loop 1000.
!                   Rename ZAQTOP ZSZBOT.
!      970522       Remove "unfinished code" message.  Simplify test.
!      970523       Set ZVSNOD(1,IEL) less than ZLYRBT(IEL,1).
!      970612       Simplify loop 1120.  (Cancel above: 2 mods.)
!      970718       ZAQBOT was ZLBOT.  Labels in order.
!                   Use IEL.LE.NLF etc for ITYPE.EQ.3.
!                   GOTO 1585 instead of ELSE.  Fix error in setting of
!                   ITOP, JTOP for links (was LL-NCSZON).
!                   Use NMOD instead of 100, & merge layer IF-blocks.
!                   Rationalize tests for skipping loop 1590.  Rename
!                   I|JALDUM J|IRANGE.  Scrap inconsistency error 1049.
!                   Fix error in aquifer zone: skip if EITHER, not BOTH.
!      970728       Scrap local IUNDEF & arrays LIDUM,LJDUM. Fix errors:
!                   in message 1037 print I|J not LI|JDUM(I|J) (=1); at
!                   top of aquifer zone goto 1585 not 1590 (for BDONE).
!      970730       Refine split cell treatment: don't straddle null
!                   cells.  Flag warnings 1037 & 1053 once only.
!                   Scrap inconsistency error 1050.  Complete IEL loop
!                   before renumbering (don't jump out straightaway).
!      970801       Complete split cells: spread foregone splits
!                   (was ill-specified). Reduce MSG size. Simplify test.
!                   Don't connect (ends of) river bed cells.
!      970806       Add some entry conditions.
!      970811       (Amend PAIR logic: use MISS.)
!----------------------------------------------------------------------*
! Entry conditions:
! {   LLEE, NLYREE, VSZMAX } >  0              LLEE >= NCSZON
! { NCRBED, NCSZON, VSZMIN } >= 0            NLYREE >  NLYR(NLF+1:NEL)
!              calls_per_run <= 1             NELEE >= NEL > { NLF, 0 }
!           DCSZON(1:NCSZON) >= VSZMIN        NLFEE >=       { NLF, 1 }
! for each e in NLF+1:NEL
!     ICMREF(e,1).ne.2 ==> for each layer in 1:NLYR(e)
!                          ZLYRBT(e,layer) <= ZLYRBT(e,layer+1)
!                   either ZLYRBT(e,layer) <  ZGRUND(e)-DCSTOT
!                       or ZLYRBT(e,layer) =~ ZGRUND(e)-asum(DCSZON(1:i))
!                                                 for some i in 1:NCSZON
!     ICMREF(e,1).eq.1  ==>  1 <= ICMREF(e,4) <= NLF
! for each e in ICOL1:NEL                       ! ICOL1 is defined below
!     for each face in 1:4      jel == ICMREF(e,4+face) <= NEL
!              jel.ge.ICOL1  ==>  1 <= ICMREF(e,8+face) <= 4
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!            AL.P.VSS: LLEE,NELEE,NLFEE,NLYREE
! Input common
!                AL.C: FATAL,NLF,PRI,WARN,ICMBK(NLFEE,2),NLYR(NLF+1:NEL)
!                      ZBEFF(NLFEE),ZGRUND(NLF+1:NEL)
!                      ZLYRBT(NELEE,NLYREE)
!                      BEXBK
!                AL.G: NEL,ICMREF(NELEE,12)
!          VSCOM1.INC: NCRBED,NCSZON,JVSALN(NELEE,NLYREE,4)
!                      DCRTOT,DCSTOT,VSZMAX,VSZMIN,DCSZON(LLEE)
! Output common
!                AL.C: LL,JVSACN(4,LLEE,NELEE),NLYRBT(NELEE,NLYREE)
!                         JVSDEL(4,LLEE,NELEE),          NHBED(NLFEE,2)
!                      DELTAZ(LLEE,NEL),ZVSNOD(LLEE,NEL),FHBED(NLFEE,2)
! Workspace common
!                AL.C: IDUM(NEL)
! Locals, etc
!INTRINSIC DIM, INT, MAX, MIN, MOD
      INTEGER :: JVSDUM, NMOD
      PARAMETER (JVSDUM = NELEE * NLYREE, NMOD = NLYREE+1)
      INTEGER :: I, IRANGE, IBOT, IBOTL, ICL, IEL, IFA, ILYR, ITOP
      INTEGER :: J, JRANGE, JBOT, JBOTL, JCL, JEL, JFA, JLYR, JTOP
      INTEGER :: IDEL, IDEL0, IL, ILMAX, ILMIN, NITOT, NIMIN
      INTEGER :: JDEL, JDEL0, JL, JLMAX, JLMIN, NJTOT, NJMIN
      INTEGER :: IAQTOP, IBANK2, IBK, ICL0, ICL1, ICOL1, ILINK, ITYPE
      INTEGER :: DEL, JDIF, K, K2, K20, K2MOD, LCON, LTOP, &
         NRENUM
      INTEGER :: NACELL, NCELL, NCL, NCLYR, NDUM, NEXTRA, NODD, NUM2
      INTEGER :: LRENUM (NELEE, NLYREE), NIDUM (LLEE), NJDUM (LLEE), jedumdum
      DOUBLEPRECISION DZLYR, ZCBOT, ZDEPTH, ZBDBOT, ZCTOP, ZDUM
      DOUBLEPRECISION ZAQBOT, ZSZBOT, ZDIFF, ZLBOT, ZNODE
      LOGICAL :: BRENUM, BWARN, MISS, PAIR, BDONE (NELEE, 4)
      CHARACTER (LEN=57) :: MSG

      DATA LRENUM / JVSDUM * 0 /, NRENUM / 0 /


!FNCELL (I, IEL, ITOP) = IDIMJE(MIN (NLYRBT (IEL, I + 1), ITOP + 1), & !statement function replaced
! NLYRBT (IEL, I) )
!----------------------------------------------------------------------*
!>>> return to here if cells have to be re-numbered
210   NRENUM = NRENUM + 1
!>>>
      IF (NRENUM.GT.NELEE) GOTO 8048
      BWARN = NRENUM.EQ.NELEE





      BRENUM = .FALSE.
! Set initial indices, dimensions & positions of cells
!______________________________________________________*
! set values as follows:
!        for element e:     IDUM(e)  the number of cells
!                       DELTAZ(c,e)  the size of cell c
!                       ZVSNOD(c,e)  the nodal elevation of cell c
!        also: LL  the maximum value of IDUM over all elements
! --- loop over elements
      top_cell_no = 0
      DO 1000 IEL = total_no_links + 1, total_no_elements
         ITYPE = ICMREF (IEL, 1)
!           * process only grid and bank-1 elements here
!           * (links & bank-2's are treated in the bank-1 pass)


         IF (ITYPE.EQ.2) GOTO 1000
!                           >>>>>>>>>
! --- loop over layers in aquifer zone (start from bottom of column)
         ZSZBOT = ZGRUND (IEL) - DCSTOT
!           NB: ICL used as a counter in loops below; cell 1 is a dummy
         ICL = 1
         DELTAZ (ICL, IEL) = ZERO
!970612            DELTAZ(ICL,IEL) =                 VSZMIN
!970612            ZVSNOD(ICL,IEL) = ZLYRBT(IEL,1) - VSZMIN*half
         ZVSNOD (ICL, IEL) = ZERO
!^^^^^^


         DO 950 ILYR = 1, NLYR (IEL)
!              * divide each layer into equal sized cells
            ZLBOT = ZLYRBT (IEL, ILYR)
            DZLYR = MIN (ZLYRBT (IEL, ILYR + 1), ZSZBOT) - ZLBOT
!              skip if layer is thinner than minimum cell size
!970422        NB  if ZLBOT lies in aquifer zone this will leave a gap!
            IF (DZLYR.LT.VSZMIN) GOTO 950
!                                       >>>>>>>>
!              if no other plan make cells as large as poss but < VSZMAX
            NCLYR = LRENUM (IEL, ILYR)
            IF (NCLYR.LE.0) NCLYR = MAX (1, INT (DZLYR / VSZMAX) &
               + 1)
            ZDEPTH = DZLYR / NCLYR
            DO 920 I = 1, NCLYR
               ICL = ICL + 1
               DELTAZ (ICL, IEL) = ZDEPTH
               ZVSNOD (ICL, IEL) = ZDEPTH * (I - half) + ZLBOT

920         END DO


950      END DO
! --- set up data for soil zone (note DCSZON index is from the top down)
         ZAQBOT = ZLYRBT (IEL, 1)
         ZCBOT = ZSZBOT

         DO 960 I = NCSZON, 1, - 1
            ZDEPTH = DCSZON (I)
            ZNODE = ZCBOT + ZDEPTH * half
            IF (ZNODE.GT.ZAQBOT) THEN
               ICL = ICL + 1
               DELTAZ (ICL, IEL) = ZDEPTH
               ZVSNOD (ICL, IEL) = ZNODE
            ENDIF

            ZCBOT = ZCBOT + ZDEPTH


960      END DO
! --- update LL & store number of cells for this column
         top_cell_no = MAX (top_cell_no, ICL)


         IDUM (IEL) = ICL
! --- process link and opposite bank elements, if IEL is bank type 1


         IF (ITYPE.NE.1) GOTO 1000
!                           >>>>>>>>>
!           * set up link cells up to bottom of link bed
         ILINK = ICMREF (IEL, 4)
         ZBDBOT = ZBEFF (ILINK) - DCRTOT

         ZCBOT = ZLYRBT (IEL, 1)

         DO 974 ICL1 = 1, ICL
            ZDEPTH = DELTAZ (ICL1, IEL)
            ZCTOP = ZCBOT + ZDEPTH
            IF (ZCTOP.GT.ZBDBOT) GOTO 976
!                                   >>>>>>>>
            DELTAZ (ICL1, ILINK) = ZDEPTH
            ZVSNOD (ICL1, ILINK) = ZVSNOD (ICL1, IEL)

            ZCBOT = ZCTOP


974      END DO
!>         < this point won't be traversed unless bank is below bed.
!           cell just below link bed: smaller than bank, unless ...
976      ZDEPTH = ZBDBOT - ZCBOT
         IF (ZDEPTH.LT.VSZMIN) THEN
!               ... remainder is small: add it to the cell below
            ICL1 = ICL1 - 1
            ZDEPTH = ZDEPTH + DELTAZ (ICL1, ILINK)
         ENDIF
         DELTAZ (ICL1, ILINK) = ZDEPTH

         ZVSNOD (ICL1, ILINK) = ZBDBOT - half * ZDEPTH
!           set up link bed cells (note DCRBED index is top-down)
         ZCBOT = ZBDBOT
         DO 980 I = NCRBED, 1, - 1
            ZDEPTH = DCRBED (I)
            ICL1 = ICL1 + 1
            DELTAZ (ICL1, ILINK) = ZDEPTH
            ZVSNOD (ICL1, ILINK) = ZCBOT + ZDEPTH * half
            ZCBOT = ZCBOT + ZDEPTH

980      END DO
!           update LL & store number of cells for the link
         top_cell_no = MAX (top_cell_no, ICL1)

         IDUM (ILINK) = ICL1
!           set up opposite bank cells
         IBANK2 = ICMBK (ILINK, 2)
         DO 985 I = 1, ICL
            DELTAZ (I, IBANK2) = DELTAZ (I, IEL)
            ZVSNOD (I, IBANK2) = ZVSNOD (I, IEL)
985      END DO

         IDUM (IBANK2) = ICL




1000  END DO
! Renumber cells (so that the top cell number is LL) & set up NLYRBT
!____________________________________________________________________*
! --- set number of first column element
      IF (BEXBK) THEN
         ICOL1 = 1
      ELSE
         ICOL1 = total_no_links + 1
!!!
!            * temporary measure to avoid out-of-bounds errors, etc
         DO ILINK = 1, total_no_links
            NLYRBT (ILINK, 1) = top_cell_no
         END DO
!!!


      ENDIF
! --- loop over column elements

      DO 1200 IEL = ICOL1, total_no_elements
!           * shuffle values in DELTAZ & ZVSNOD, and zero remainder
         NCL = IDUM (IEL)
         ICL0 = top_cell_no - NCL
         IF (ICL0.GT.0) THEN
            DO 1100 I = NCL, 1, - 1
               ICL = ICL0 + I
               DELTAZ (ICL, IEL) = DELTAZ (I, IEL)
               ZVSNOD (ICL, IEL) = ZVSNOD (I, IEL)
1100        END DO
            CALL ALINIT (ZERO, ICL0, DELTAZ (1, IEL) )
            CALL ALINIT (ZERO, ICL0, ZVSNOD (1, IEL) )

         ENDIF
!           * find bottom cell in each layer
!970612
         ICL0 = ICL0 + 1
!^^^^^^
         DO 1150 ILYR = 1, NLYR (IEL)
            DO ICL = ICL0 + 1, top_cell_no
               IF (ZVSNOD (ICL, IEL) .GT.ZLYRBT (IEL, ILYR) ) GOTO 1130
            END DO
1130        NLYRBT (IEL, ILYR) = ICL
            ICL0 = ICL - 1
1150     END DO

         NLYRBT (IEL, ILYR) = top_cell_no + 1





1200  END DO
! Set up cell connectivities (JVSACN, JVSDEL)
!_____________________________________________*
! --- initialise arrays first
      DO 1260 IEL = 1, total_no_elements
         IBOT = NLYRBT (IEL, 1)
         DO 1240 IFA = 1, 4
            BDONE (IEL, IFA) = .FALSE.
            DO 1220 ICL = IBOT, top_cell_no
               JVSACN (IFA, ICL, IEL) = 0
               JVSDEL (IFA, ICL, IEL) = 0
1220        END DO
1240     END DO



1260  END DO
! ----- start of loop over (faces of) elements
      LTOP = top_cell_no - NCRBED
      IAQTOP = top_cell_no - NCSZON

      DO 1600 IEL = ICOL1, total_no_elements
         ITYPE = ICMREF (IEL, 1)
         IBOT = NLYRBT (IEL, 1)
         IF (IEL.LE.total_no_links) THEN
            IBK = ICMBK (IEL, 1)
            ITOP = MIN (IAQTOP + IBOT - NLYRBT (IBK, 1), LTOP)
         ELSE
            ITOP = IAQTOP

         ENDIF

         DO 1590 IFA = 1, 4
            JEL = ICMREF (IEL, IFA + 4)
            IF (JEL.LT.ICOL1) GOTO 1590
!                                >>>>>>>>>
            JFA = ICMREF (IEL, IFA + 8)
            IF (BDONE (JEL, JFA) ) GOTO 1590
!                                >>>>>>>>>
            JBOT = NLYRBT (JEL, 1)


            JDIF = JBOT - IBOT
! --- channel link-bank face: cells below river bed explicitly matched
!              * NB: layer connectivity & soil zone are disregarded here
            IF (IEL.LE.total_no_links.AND.JEL.GT.total_no_links) THEN
               DO 1280 ICL = IBOT, LTOP
                  JCL = ICL + JDIF
                  JVSACN (IFA, ICL, IEL) = JCL
                  JVSACN (JFA, JCL, JEL) = ICL
1280           END DO
               GOTO 1585
!                  >>>>>>>>>


            ENDIF
! --- other elements (grid-grid, grid-bank, or end-to-end banks/links)
            IF (JEL.LE.total_no_links) THEN
               IBK = ICMBK (JEL, 1)
               JTOP = MIN (IAQTOP + JBOT - NLYRBT (IBK, 1), LTOP)
               LCON = LTOP
            ELSE
               JTOP = IAQTOP
               LCON = top_cell_no


            ENDIF
! ----- soil zone processing
!              * one-to-one for all active (except river-bed) cells
            jedumdum = MAX (IBOT, JBOT)
            jedumdum = MAX (jedumdum, ITOP + 1, JTOP + 1)
            !""AD DO 1322 ICL = MAX (IBOT, JBOT, ITOP + 1, JTOP + 1), LCON
            DO 1322 ICL = jedumdum , LCON
               JCL = ICL
               JVSACN (IFA, ICL, IEL) = JCL
               JVSACN (JFA, JCL, JEL) = ICL


1322        END DO
! ----- aquifer zone processing
!              * loop over layers, starting at the bottom
            ILYR = 1
            JLYR = 1

1410        CONTINUE
            IBOTL = NLYRBT (IEL, ILYR)
            JBOTL = NLYRBT (JEL, JLYR)
            IF (IBOTL.GT.ITOP.OR.JBOTL.GT.JTOP) GOTO 1585
!                                                         >>>>>>>>>
            JRANGE = JVSALN (IEL, ILYR, IFA)

            IRANGE = JVSALN (JEL, JLYR, JFA)
            IF (JRANGE.EQ.0) THEN
               ILYR = ILYR + 1
               GOTO 1410
!                     <<<<<<<<<
            ELSEIF (IRANGE.EQ.0) THEN
               JLYR = JLYR + 1
               GOTO 1410
!                     <<<<<<<<<

            ENDIF
!                 * range of layers to process on this pass
            ILMIN = IRANGE / NMOD
            ILMAX = MOD (IRANGE, NMOD)
            JLMIN = JRANGE / NMOD

            JLMAX = MOD (JRANGE, NMOD)
!                 * count cells in column IEL, & no. required in JEL
            NITOT = 0
            NJMIN = 0
            NODD = 0
            DO 1470 IL = ILMIN, ILMAX
               NCELL = FNCELL (IL, IEL, ITOP)
               IF (JVSALN (IEL, IL, IFA) .NE.0) THEN
                  DO 1460 I = 0, NCELL - 1
                     NITOT = 1 + NITOT
                     NIDUM (NITOT) = I + NLYRBT (IEL, IL)
1460              END DO
                  NCELL = NCELL - NODD
                  NJMIN = (NCELL + 1) / 2 + NJMIN
                  NODD = MOD (NCELL, 2)
               ELSEIF (NCELL.GT.0) THEN
                  NODD = 0
               ENDIF
1470        END DO

            NIDUM (NITOT + 1) = 0
!                 * count cells in column JEL, & no. required in IEL
            NJTOT = 0
            NIMIN = 0
            NODD = 0
            DO 1570 JL = JLMIN, JLMAX
               NCELL = FNCELL (JL, JEL, JTOP)
               IF (JVSALN (JEL, JL, JFA) .NE.0) THEN
                  DO 1560 J = 0, NCELL - 1
                     NJTOT = 1 + NJTOT
                     NJDUM (NJTOT) = J + NLYRBT (JEL, JL)
1560              END DO
                  NCELL = NCELL - NODD
                  NIMIN = (NCELL + 1) / 2 + NIMIN
                  NODD = MOD (NCELL, 2)
               ELSEIF (NCELL.GT.0) THEN
                  NODD = 0
               ENDIF
1570        END DO

            NJDUM (NJTOT + 1) = 0

            IF (NITOT.EQ.0.AND.NJTOT.GT.0) THEN
!                     * I-layers are empty
               WRITE (MSG, 9200) JFA, JLYR

               IF (NRENUM.EQ.1) CALL ERROR(WWWARN, 1053, PPPRI, JEL, 0, &
                  MSG)

            ELSEIF (NJTOT.EQ.0.AND.NITOT.GT.0) THEN
!                     * J-layers are empty
               WRITE (MSG, 9200) IFA, ILYR

               IF (NRENUM.EQ.1) CALL ERROR(WWWARN, 1053, PPPRI, IEL, 0, &
                  MSG)

            ELSEIF (NJTOT.LT.NJMIN) THEN
!                     * need more J-cells
               BRENUM = .TRUE.
               NEXTRA = 0
               DO 1572 JL = JLMIN, JLMAX
                  IF (JVSALN (JEL, JL, JFA) .NE.0) THEN
                     IF (BWARN) THEN
                        WRITE (MSG, 9300) JFA, JL
                        CALL ERROR(WWWARN, 1037, PPPRI, JEL, 0, MSG)
                     ENDIF
                     NCELL = FNCELL (JL, JEL, JTOP)
                     NDUM = NCELL * NJMIN + NEXTRA + NJTOT / 2
                     LRENUM (JEL, JL) = NDUM / NJTOT
                     NEXTRA = MOD (NDUM, NJTOT) - NJTOT / 2
                  ENDIF

1572           END DO

            ELSEIF (NITOT.LT.NIMIN) THEN
!                     * need more I-cells
               BRENUM = .TRUE.
               NEXTRA = 0
               DO 1574 IL = ILMIN, ILMAX
                  IF (JVSALN (IEL, IL, IFA) .NE.0) THEN
                     IF (BWARN) THEN
                        WRITE (MSG, 9300) IFA, IL
                        CALL ERROR(WWWARN, 1037, PPPRI, IEL, 0, MSG)
                     ENDIF
                     NCELL = FNCELL (IL, IEL, ITOP)
                     NDUM = NCELL * NIMIN + NEXTRA + NITOT / 2
                     LRENUM (IEL, IL) = NDUM / NITOT
                     NEXTRA = MOD (NDUM, NITOT) - NITOT / 2
                  ENDIF

1574           END DO

            ELSE
!                     * how many splits possible, & how many to forego
               IF (NITOT.GE.NJTOT) THEN
                  IDEL0 = 1
                  NUM2 = NITOT - NJMIN
                  NEXTRA = NJTOT - NJMIN
               ELSE
                  IDEL0 = 0
                  NUM2 = NJTOT - NIMIN
                  NEXTRA = NITOT - NIMIN
               ENDIF
               JDEL0 = 1 - IDEL0

               CALL ALSPRD (NEXTRA, NUM2, K20, K2MOD)
!                     * loop over all cells found
               MISS = .FALSE.
               K2 = - K20
               I = 1
               J = 1

1575           IF (I.LE.NITOT.AND.J.LE.NJTOT) THEN
                  PAIR = NIDUM (I + IDEL0) .EQ.NIDUM (I) + 1
                  PAIR = NJDUM (J + JDEL0) .EQ.NJDUM (J) + 1.OR.PAIR
                  PAIR = .NOT.MISS.AND.PAIR
                  IF (PAIR) THEN
                     K2 = K2 + 1
                     MISS = K2.GE.0.AND.MOD (K2, K2MOD) .EQ.0
                     MISS = K2.LE. (NEXTRA - 1) * K2MOD.AND.MISS
                     PAIR = .NOT.MISS
                  ELSE
                     MISS = .FALSE.
                  ENDIF
                  DEL = 0
                  IF (PAIR) DEL = 1
                  IDEL = IDEL0 * DEL
                  JDEL = JDEL0 * DEL
                  DO 1580 K = 0, DEL
                     ICL = NIDUM (I)
                     JCL = NJDUM (J)
                     IF (IDEL.GE.K) JVSACN (IFA, ICL, IEL) = JCL
                     IF (JDEL.GE.K) JVSACN (JFA, JCL, JEL) = ICL
                     JVSDEL (IFA, ICL, IEL) = IDEL * (1 - 2 * K)
                     JVSDEL (JFA, JCL, JEL) = JDEL * (1 - 2 * K)
                     I = I + IDIMJE(IDEL, K)
                     J = J + IDIMJE(JDEL, K)
1580              END DO
                  I = I + 1
                  J = J + 1

                  GOTO 1575
!                         <<<<<<<<<

               ENDIF

            ENDIF
!                 * move on to next layers
            ILYR = ILMAX + 1
            JLYR = JLMAX + 1

            GOTO 1410
!              <<<<<<<<<

1585        BDONE (IEL, IFA) = .TRUE.

1590     END DO



1600  END DO
! Repeat the whole thing if necessary
!_____________________________________*
!                 <<<<<<<<



      IF (BRENUM) GOTO 210
!                 <<<<<<<<
! Finish off
!____________*

      WRITE (PPPRI, 9000) top_cell_no


      DO 2100 IEL = ICOL1, total_no_links
!        * adjust elevations for link cells (to make room for river-bed)
         IBK = ICMBK (IEL, 1)
         NACELL = LTOP + NLYRBT (IBK, 1) - NLYRBT (IEL, 1)
         ZDUM = DELTAZ (NACELL, IBK)

         ZDIFF = ZDUM - DELTAZ (LTOP, IEL)

         DELTAZ (LTOP, IEL) = ZDUM
         DO ICL = NLYRBT (IEL, 1), LTOP - 1
            ZVSNOD (ICL, IEL) = ZVSNOD (ICL, IEL) - ZDIFF
         END DO
         ZVSNOD (ICL, IEL) = ZVSNOD (ICL, IEL) - ZDIFF * half
         DO ILYR = 1, NLYR (IEL)
            ZLYRBT (IEL, ILYR) = ZLYRBT (IEL, ILYR) - ZDIFF
         END DO
!        * NB. banks 1 and 2 are identical
         NHBED (IEL, 1) = NACELL
         NHBED (IEL, 2) = NACELL
         FHBED (IEL, 1) = ZERO

         FHBED (IEL, 2) = ZERO

2100  END DO

      RETURN

8048  CALL ERROR(FFFATAL, 1048, PPPRI, 0, 0, 'Attempts to renumber cells have failed.')
9000  FORMAT(/ 'Number of top cell in all columns (LL) = ',I3)
9200  FORMAT('Null cell connectivity being set up for face ',I1, &
      &       ' layer ',I2)
9300  FORMAT(  'Not possible to connect all cells for face ',I1, &
      &       ' layer ',I2)
   END SUBROUTINE VSCONC

!SSSSSS SUBROUTINE VSCONL (NAQCON, IAQCON)
   SUBROUTINE VSCONL (NAQCON, IAQCON)
!----------------------------------------------------------------------*
! Sets up layer connectivity matrix
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSCONL/4.1
! Modifications:
!  GP  20.07.94  written (v4.0 finished 8/8/95)
! RAH  970508  4.1  New locals ICOL1, JTYPE, LYR, NLYRI.
!                   Simplify test & amend comment for null connectivity.
!                   Generic intrinsics.  No illegal DATA for JVSALN.
!      970522       Fix error setting JLMAX: use JLMIN not ILMIN.
!                   Scrap "null connectivity" message (error 1047).
!      970630       Move NAQCON,IAQCON from VSINIT.INC to arg-list, &
!                   swap indices (see VSREAD).
!      970703       Initialize to 0 (was IUNDEF), once & for all, but
!                   only for active iel, & only up to NLYR(iel)+1.
!      970710       Redefine IUNDEF (was 9999).  Use NMOD, not 100.
!                   More detail in ERROR MSG.  Labels in order.
!                   Rewrite loop 110, & fix error: multiply JLYR by
!                   NMOD+1 on first assignment; also trap invalid JLYR.
!      970711       Local ZSMALL.  Rewrite loop 200, & fix errors: set
!                   JVSALN BOTH sides for user-defined; correct express-
!                   ions for ILMIN, etc; also generalize/amend default
!                   strategy (was: check/set single embedded layer -
!                   although some were missed; else connect matching
!                   soils; else move down a layer).  Use -1 for IUNDEF.
!      970714       Leave bank-link faces at zero (never used anyway).
!                   Criterion for loop 200 at start (was at end).
!                   Set ISOILP=0 for ILYR=NLYRI; also use JSOILP.
!      970721       JVSALN=0 or NMOD*imin+imax always.
!      970813       Don't give up on face if no match for I|JLYR.
!----------------------------------------------------------------------*
! Entry conditions:  not more than 1 call per run
! NAQCON <= nvsee (size of IAQCON)
! NELEE >= NEL >= 1 ;  NLF >= 0 ;  NLYREE >= 1, NLYR(1:NEL)
! for e in ICOL1:NEL : for face in 1:4 :  ea = ICMREF(e,4+face) <= NEL ;
!                       ea >= ICOL1  ==>  1 <= ICMREF(e,8+face) <= 4
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P.VSS:        NELEE,NLYREE
! Input common
!     AL.C:            ERR,NLF,PRI,NLYR(NEL),NTSOIL(NELEE,NLYREE)
!                                ZGRUND(NEL),ZLYRBT(NELEE,NLYREE)
!                      BEXBK
!     AL.G:            NEL,ICMREF(NELEE,5:12)
!     VSCOM1.INC:      DCSTOT
! In+out common
!     VSINIT.INC:      NVSERR
! Output common
!     VSCOM1.INC:      JVSALN(NELEE,NLYREE,4)
! Input arguments

      INTEGER :: NAQCON, IAQCON (4, * )
! Locals, etc
!INTRINSIC MAX, MIN, MOD
      INTEGER :: NMOD
      DOUBLEPRECISION ZSMALL
      PARAMETER (NMOD = NLYREE+1, ZSMALL = 1D-6)
      INTEGER :: I, J, ILYR, JLYR, IEL, JEL, IFA, JFA, NLYRI, NLYRJ
      INTEGER :: ILMIN, ILMAX, JLMIN, JLMAX, IRANGE, JRANGE, ISOIL, &
         JSOIL
      INTEGER :: ISKIP, JSKIP, ISOILP, JSOILP, I1, I2, ICOL1, K, KEL
      INTEGER :: ILDUM (NLYREE), JLDUM (NLYREE)
      DOUBLEPRECISION ZSZBOT
      LOGICAL :: IOK, MOVEJ, TEST1, BDONE (NELEE)
      CHARACTER (LEN=132) :: MSG


      DATA BDONE / NELEE * .FALSE. /
!----------------------------------------------------------------------*
      IF (BEXBK) THEN
         ICOL1 = 1
      ELSE
         ICOL1 = total_no_links + 1


      ENDIF
! ----- default is null connectivity
      DO IFA = 1, 4
         DO IEL = 1, total_no_elements
            DO ILYR = 1, NLYR (IEL) + 1
               JVSALN (IEL, ILYR, IFA) = 0
            END DO
         END DO
      END DO
! Main loop over (faces of) column elements
!___________________________________________*

      DO 500 IEL = ICOL1, total_no_elements

         NLYRI = NLYR (IEL)

         DO 400 IFA = 1, 4


            JEL = ICMREF (IEL, IFA + 4)
! null connectivity for boundary faces, branched channels & link flanks


            IF (JEL.LT.ICOL1.OR. (IEL.LE.total_no_links.AND.JEL.GT.total_no_links) ) GOTO 400
!                                                            >>>>>>>>
! skip rest of loop if face already processed ...


            IF (BDONE (JEL) ) GOTO 400
!                           >>>>>>>>
! ... else process BOTH sides of face
            NLYRJ = NLYR (JEL)



            JFA = ICMREF (IEL, IFA + 8)
! check for user-defined layer connectivity for this pair of elements
! ILDUM(ilyr) is the layer in column JEL connected to layer 'ilyr'
! if more than one layer is connected ILDUM = NMOD*min.layer + max.layer
! a value of zero specifies null connectivity
! NB this code also verifies the input data in IAQCON
            DO I = 1, NLYRI
               ILDUM (I) = - 1
            END DO
            DO J = 1, NLYRJ
               JLDUM (J) = - 1
            END DO

            DO 110 I = 1, NAQCON
               I1 = IAQCON (1, I)

               I2 = IAQCON (3, I)
!              * does entry I belong to the current pair of elements?
               IF (IEL.EQ.I1.AND.JEL.EQ.I2) THEN
                  K = 2
               ELSEIF (IEL.EQ.I2.AND.JEL.EQ.I1) THEN
                  K = 4
               ELSE
                  GOTO 110
!                  >>>>>>>>

               ENDIF
               ILYR = IAQCON (K, I)

               JLYR = IAQCON (6 - K, I)
               MSG = ' '

               IF (ILYR.LT.0.OR.ILYR.GT.NLYRI) THEN
!                  * ILYR out of range
                  KEL = IEL

                  WRITE (MSG, 9381) ILYR, I, IEL, NLYRI

               ELSEIF (JLYR.LT.0.OR.JLYR.GT.NLYRJ) THEN
!                  * JLYR out of range
                  KEL = JEL

                  WRITE (MSG, 9381) JLYR, I, JEL, NLYRJ

               ELSE
                  IF (ILYR.GT.0) THEN
                     JRANGE = ILDUM (ILYR)
                     TEST1 = JLYR.EQ.0.AND.JRANGE.GT.0
                     IF (JRANGE.EQ.0.OR.TEST1) THEN
!                          * invalid
                        KEL = IEL
                        JRANGE = MOD (JLYR + JRANGE, NMOD)
                        WRITE (MSG, 9382) IEL, ILYR, JRANGE, JEL, I
                     ELSE
                        IF (JRANGE.LT.0) JRANGE = NMOD * NLYRJ + 1
                        JLMIN = MIN (JLYR, JRANGE / NMOD)
                        JLMAX = MAX (JLYR, MOD (JRANGE, NMOD) )
                        ILDUM (ILYR) = NMOD * JLMIN + JLMAX
                     ENDIF

                  ENDIF
                  IF (JLYR.GT.0) THEN
                     IRANGE = JLDUM (JLYR)
                     TEST1 = ILYR.EQ.0.AND.IRANGE.GT.0
                     IF (IRANGE.EQ.0.OR.TEST1) THEN
!                          * invalid
                        KEL = JEL
                        IRANGE = MOD (ILYR + IRANGE, NMOD)
                        WRITE (MSG, 9382) JEL, JLYR, IRANGE, IEL, I
                     ELSE
                        IF (IRANGE.LT.0) IRANGE = NMOD * NLYRI + 1
                        ILMIN = MIN (ILYR, IRANGE / NMOD)
                        ILMAX = MAX (ILYR, MOD (IRANGE, NMOD) )
                        JLDUM (JLYR) = NMOD * ILMIN + ILMAX
                     ENDIF

                  ENDIF

               ENDIF
!              * note: MSG for ILYR>0.and.JRANGE=0 is lost
!              *       if also JLYR>0.and.IRANGE=0
               IF (MSG.NE.' ') THEN
                  CALL ERROR (EEERR, 1038, PPPRI, KEL, 0, MSG)
                  NVSERR = NVSERR + 1

               ENDIF


110         END DO
! set ILYR & JLYR to numbers of layers immediately below soil zone
!           ZSMALL is added to avoid rounding errors if the bottom
!           of a layer coincides with the bottom of the soil zone
!970711     ! expression for ZSZBOT is wrong for link elements
            ZSZBOT = ZGRUND (IEL) - DCSTOT - ZSMALL
            DO ILYR = NLYRI, 1, - 1
               IF (ZLYRBT (IEL, ILYR) .LT.ZSZBOT) GOTO 125
            END DO
125         ZSZBOT = ZGRUND (JEL) - DCSTOT - ZSMALL
            DO JLYR = NLYRJ, 1, - 1
               IF (ZLYRBT (JEL, JLYR) .LT.ZSZBOT) GOTO 200
            END DO

200         IF (ILYR.EQ.0.OR.JLYR.EQ.0) GOTO 400
!                                           >>>>>>>>
            ISOIL = NTSOIL (IEL, ILYR)

            JSOIL = NTSOIL (JEL, JLYR)
            JRANGE = ILDUM (ILYR)

            IRANGE = JLDUM (JLYR)

            IF (JRANGE.EQ.0.OR. (IRANGE.GT.0.AND.JRANGE.LT.0) ) THEN
!                  * null

               ILYR = ILYR - 1

            ELSEIF (IRANGE.EQ.0.OR. (JRANGE.GT.0.AND.IRANGE.LT.0) ) &
               THEN
!                  * null

               JLYR = JLYR - 1

            ELSEIF (JRANGE.GT.0) THEN
!                  * user-specified
               JLMIN = JRANGE / NMOD

               ILMIN = IRANGE / NMOD
!                  * repeat until the whole connected range is processed
210            CONTINUE
!                     *
               ILMAX = ILYR
               DO 220 ILYR = ILMAX, ILMIN, - 1
                  JRANGE = ILDUM (ILYR)
                  JVSALN (IEL, ILYR, IFA) = MAX (0, JRANGE)
                  IF (JRANGE.GT.0) JLMIN = MIN (JLMIN, JRANGE / NMOD)
220            END DO
!                     *
               JLMAX = JLYR
               DO 240 JLYR = JLMAX, JLMIN, - 1
                  IRANGE = JLDUM (JLYR)
                  JVSALN (JEL, JLYR, JFA) = MAX (0, IRANGE)
                  IF (IRANGE.GT.0) ILMIN = MIN (ILMIN, IRANGE / NMOD)
240            END DO
!                     *

               IF (ILMIN.LE.ILYR) GOTO 210

            ELSEIF (ISOIL.EQ.JSOIL) THEN
!                  * matching soils
               JVSALN (IEL, ILYR, IFA) = JLYR * NMOD+JLYR
               JVSALN (JEL, JLYR, JFA) = ILYR * NMOD+ILYR
               ILYR = ILYR - 1

               JLYR = JLYR - 1


            ELSE
!                  * decide whether to move down column IEL or JEL:
!                  * set type of soil above
               ISOILP = 0
               IF (ILYR.LT.NLYRI) ISOILP = NTSOIL (IEL, ILYR + 1)
               JSOILP = 0

               IF (JLYR.LT.NLYRJ) JSOILP = NTSOIL (JEL, JLYR + 1)
!                  * look for next matching soil or user-specification
               DO I = ILYR - 1, 1, - 1
                  IF (NTSOIL (IEL, I) .EQ.JSOIL.OR.ILDUM (I) .GE.0) GOTO 265
               END DO
265            ISKIP = ILYR - I
               DO J = JLYR - 1, 1, - 1
                  IF (NTSOIL (JEL, J) .EQ.ISOIL.OR.JLDUM (J) .GE.0) GOTO 285
               END DO

285            JSKIP = JLYR - J
!                  * choose smallest skip; or preserve soil continuity
               MOVEJ = ISOIL.EQ.ISOILP.OR.JSOIL.NE.JSOILP
               MOVEJ = JSKIP.LT.ISKIP.OR.JSKIP.EQ.ISKIP.AND.MOVEJ
               MOVEJ = J.GT.0.AND.MOVEJ

               IF (MOVEJ) MOVEJ = JLDUM (J) .LT.0
!                  * would there be any point moving down IEL?
               IOK = I.GT.0

               IF (IOK) IOK = ILDUM (I) .LT.0
!                  * the choice is made
               IF (MOVEJ) THEN
                  JLYR = J
               ELSEIF (IOK) THEN
                  ILYR = I
               ELSE
                  ILYR = ILYR - 1
                  JLYR = JLYR - 1

               ENDIF

            ENDIF
!           * process next pair of layers

            GOTO 200

400      END DO

         BDONE (IEL) = .TRUE.



500   END DO
! Formats
!_________*
9381  FORMAT('Layer',I3,' out of range, IAQCON entry',I3, &
      &      ' (element',I5,' has',I3,' layers)')

9382  FORMAT('Invalid null connection, element',I5,':', &
      &      ' layer',I3,' already connected to layer',I3,', element',I5, &
      &      ' (see IAQCON entry',I3,')')
   END SUBROUTINE VSCONL


END MODULE
