MODULE oc_input
! Input routines for overland channel calculations
! Contains OCREAD, JEOCBC, OCPLF and other input-related subroutines
! Extracted from OCmod.f90

   USE SGLOBAL, ONLY: pppri, FFFATAL, ERROR, UZNOW, nelee, nxee, nyee, nlfee, marker999, zero, one, two, half, &
      total_no_elements, total_no_links, ZGRUND, NOCTAB, EEERR, ISZERO, NOTZERO
   USE AL_C ,     ONLY : IDUM, NBFACE, CWIDTH, ZBFULL, &
      DUMMY, ZBEFF, ICMBK, BEXBK, QBKB, QBKF, ICMRF2, &
      TIH, DHF, CLENTH, CLENTH, PNETTO, QH, QOC, LINKNS, ARXL
   USE AL_D ,     ONLY : DQ0ST, DQIST, DQIST2, OCNOW, OCNEXT, OCD, ESWA, QMAX, NOCBCC, &
      NOCBCD, LCODEX, LCODEY, OHB, OFB
   USE AL_G ,     ONLY : NGDBGN, NX, NY, ICMREF, ICMXY
   USE UTILSMOD , ONLY : HINPUT, FINPUT, AREADR, AREADI, JEMATMUL_VM, JEMATMUL_MM, INVERTMAT
   USE mod_load_filedata ,    ONLY : ALCHK, ALCHKI, ALINIT
   USE OCQDQMOD,  ONLY : STRXX, STRYY, COCBCD
   USE OCmod2, ONLY: SETHRF
   USE oc_common_data, ONLY : NELIND, NROWF, NROWL, NOCHB, NOCFB, &
      NROWEL, NROWST, NXSECT, HOCLST, HOCNXT, QFLAST, QFNEXT, &
      HOCPRV, QOCFIN, HOCNXV, XINH, XINW, XAREA, dtoc
   USE oc_utils, ONLY : LINKNO

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: OCREAD, JEOCBC, OCPLF

CONTAINS


   SUBROUTINE OCREAD(KONT, TDC, TFC, CATR, DDUM2)
!----------------------------------------------------------------------*
!  Control the reading of the OC input data file
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCREAD/4.2
! Modifications:
!  GP          3.4  Call OCBC always, not only when NLF.ne.0.
! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
! RAH  980120  4.2  Explicit typing.  Remove local FINI.
!                   Call ALINIT if .not.BIOWAT.  Use KKON for I (=1).
!                   Move KONT from SPEC.OC to arg-list (see OCINI).
!                   Implement missing option NCATR.gt.0 (see BR/48).
!      980130       Bring (initial) read section from OCINI.
!                   Don't print NT (now redundant) or DET,TDC,TFC,SMIN.
!                   Replace GOTOs with block-IFs.  List-directed input.
!                   Test NCATR>NOCTAB not NCATRE (SPEC.OC variable).
!                   Renumber labels, move FORMATs to end and tidy.
!                   Print CDRS only if non-zero.  Call ERROR on errors.
!                   Bring NCATR,CDRS,CATR,BIOWAT from SPEC.OC.
!      980202       KKON=0 if KONT even, not just 0.
!                   Trap NCATR.lt.0.  Move OCIND call to OCINI.
!                   Use ALINIT for STRX,STRY.  Full output if BOUT.
!      980203       Use NGDBGN for NLF+1.
!      980204       Full argument lists for OCBC, OCPLF.  Close OCD.
!      980205       Full argument list, no INCLUDEs (see OCINI).
!                   Bring IXER from SPEC.OC.  FATAL local.
!      980218       Bring NGDBGN(=NLF+1) from argument list (see OCINI).
!                   Call OCPLF only if IXER.eq.0.
!      980220       Spelling.
!      980225       Swap COCBCD subscripts (see SPEC.OC)
!      980226       Move TDC,TFC to argument list; overwrite if KONT<2.
!                   Don't print KONT.
!----------------------------------------------------------------------*
! Entry requirements:
!  NELEE.ge.[NEL,NOCTAB*NOCTAB]    NEL.gt.NLF    NLF.ge.0    NOCTAB.ge.1
!  NLFEE.ge.NLF    OCD open for F input      PRI open for F output
!----------------------------------------------------------------------*
      INTEGER, INTENT(OUT) :: KONT
      DOUBLEPRECISION, INTENT(INOUT) :: TDC, TFC
      DOUBLEPRECISION, INTENT(OUT) :: CATR (NOCTAB), DDUM2 (NOCTAB, NOCTAB)
      INTEGER              :: I, IBC, ICAT, IELt, IXER, KKON, TYPEE
      INTEGER              :: NCATR, NLAND, NOCBC, NT, NC (11)
      DOUBLEPRECISION      :: DET, SMIN
      DOUBLEPRECISION      :: CDRS
      LOGICAL              :: BIOWAT, BOUT
      CHARACTER(81) :: MSG
      CHARACTER(11)  :: CTYPE (11)
      ICAT (ielt) = MAX (1, MIN (IDUM (ielt), NCATR) )
      DATA NC / 4 * 0, 5, 0, 2 * 4, 2 * 0, 5 /

      DATA CTYPE / 'impermeable', '  grid-grid', '       head', ' flux', ' polynomial', ' river_link', '       weir', ' river+weir', &
      & '       head', '       flux', ' polynomial' /
!----------------------------------------------------------------------*
!
!               Initialization
!
      IXER = 0
      NLAND = total_no_elements - total_no_links
      NGDBGN = total_no_links + 1
!
!               Integer & logical variables
!:OC1
      READ (OCD, * )
      READ (OCD, * ) NT, NCATR, KONT, BIOWAT
      KKON = MOD (KONT, 2)
      BOUT = KKON.EQ.1
      IF (BOUT) WRITE(PPPRI, 9080) ' ', NCATR
!
!               OC time-step data
!:OC2
!     (was (PT(I),TEMPS(I),I=1,NT))
      READ (OCD, * )
      READ (OCD, * )
!
!               Default roughness parameters & floating-point variables
!:OC3
      READ (OCD, * )
      READ (OCD, * ) SMIN, CDRS, TDC, TFC, DET
      IF (KONT.LT.2) TDC = TFC + one
!:OC4
      IF (ISZERO(CDRS)) THEN
         IF ((NCATR.GT.NOCTAB).OR.(NCATR.LT.0)) GOTO 8047
         IF (NCATR.GT.0) THEN
            read (OCD, * ) (CATR (I), I = 1, NCATR)
            IF (BOUT) THEN
               WRITE(PPPRI, 9084) (CATR (I), I = 1, NCATR)
               WRITE(PPPRI, * )
            ENDIF
         ENDIF
      ELSEIF (BOUT) THEN
         WRITE(PPPRI, 9082) CDRS
      ENDIF
!
!               INITIAL OVERLAND FLOW ELEVATIONS
!:OC5
      IF (BIOWAT) THEN
         CALL AREADR (DUMMY, KKON, OCD, PPPRI)
      ELSE
         CALL ALINIT (ZERO, NLAND, DUMMY (NGDBGN) )
         IF (BOUT) WRITE(PPPRI, 9085) 'zero'
      ENDIF
      DO 4 ielt = NGDBGN, total_no_elements
         CALL SETHRF(ielt, ZGRUND (ielt) + DUMMY (ielt))

4     END DO
!
!               ROUGHNESS PARAMETERS FOR OVERLAND FLOW
!:OC14
!:OC17
      IF (NOTZERO(CDRS)) THEN
         CALL ALINIT(CDRS, NLAND, STRXX(NGDBGN) )
         CALL ALINIT(CDRS, NLAND, STRYY(NGDBGN) )
      ELSEIF (NCATR.EQ.0) THEN
         CALL AREADR(STRXX, KKON, OCD, PPPRI)
         CALL AREADR(STRYY, KKON, OCD, PPPRI)
      ELSE
         CALL AREADI (IDUM(1:nelee), KKON, OCD, PPPRI, NCATR)
         DO ielt = NGDBGN, total_no_elements
            STRXX(ielt) = CATR (ICAT (ielt) )
         END DO
         CALL AREADI (IDUM(1:nelee), KKON, OCD, PPPRI, NCATR)
         DO ielt = NGDBGN,total_no_elements
            STRYY(ielt) = CATR (ICAT (ielt) )
         END DO

      ENDIF
!               BOUNDARY CONDITIONS
!
      CALL JEOCBC(IXER, NOCBC)
!
!               PARAMETERS OF RIVER LINKS
!
      IF ((total_no_links.GT.0).AND.(IXER.EQ.0)) THEN
         CALL OCPLF(BOUT, IXER, NOCBCD(:, 2:4), IDUM(1:noctab), DDUM2)
      ENDIF
!
!               FINISH
!
      REWIND(OCD) !CLOSE (OCD)    !AD
      IF (IXER.NE.0) THEN
         WRITE (MSG, 9412) IXER
         CALL ERROR(FFFATAL, 1049, PPPRI, 0, 0, MSG)
      ELSEIF (BOUT) THEN
         WRITE(PPPRI, 9500) 'no-flow'
         IF (NOCBC.GT.0) WRITE(PPPRI, 9600) 'Index', 'Element', 'Face', &
            'Type', 'Category', 'Coefficients'
         DO 2000 IBC = 1, NOCBC
            TYPEE = NOCBCD (IBC, 3)
            WRITE(PPPRI, 9610) IBC, (NOCBCD (IBC, I), I = 1, 2), CTYPE ( &
               TYPEE), NOCBCD (IBC, 4), (COCBCD (I, IBC), I = 1, NC (TYPEE) )
2000     END DO
         WRITE(PPPRI, 9080) ' END OF '
      ENDIF

      RETURN
8047  WRITE (MSG, 9004) NCATR, NOCTAB

      CALL ERROR(FFFATAL, 1047, PPPRI, 0, 0, MSG)

9004  FORMAT('Number of roughness categories NCATR =',I4,2X, &
      &       'lies outside range 0:NOCTAB = 0 :',I4)

9080  FORMAT (///'---- OC MODULE ',A,'INPUT DATA PROCESSING ----'///: &
      &          5X,'NUMBER OF DIFFERENT OVERLAND FLOW ROUGHNESS', &
      &             ' CATEGORIES   NCATR = ',I4 )

9082  FORMAT (/5X,'DEFAULT VALUE OF OVERLAND FLOW ROUGHNESS ', &
      &             'COEFFICIENT     CDRS = ', F8.2)

9084  FORMAT (/4X,' ROUGHNESS COEFFICIENTS  CATR  ATTACHED TO', &
      &            ' EACH OF THE NCATR CATEGORIES' / (10F10.2))

9085  FORMAT (/5X,'Initial overland water depth is ',A)

9412  FORMAT (I5,' ERROR(S) FOUND DURING OC INPUT DATA PROCESSING')

9500  FORMAT (/5X,'Default OC B.C. is ',A,' at catchment boundaries ', &
      &            'and at channel/bank dead-ends')

9600  FORMAT (/5X,'OC Boundary Conditions:'//5X,3A8,A12,A10,A14)

9610  FORMAT (5X,3I8,A12,I10,1P,5G14.6)
   END SUBROUTINE OCREAD

   SUBROUTINE JEOCBC(IXER, NOCBC)
!----------------------------------------------------------------------*
!
!  Set up boundary data (except for some channel link details to follow)
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCBC/4.2
! [Version history as in original...]
!----------------------------------------------------------------------*
      INTEGER, INTENT(OUT)         :: IXER
      INTEGER, INTENT(OUT)         :: NOCBC
      INTEGER                      :: BANK, I, IBANK, IBC, IBC0, IBK, ICAT, IELy, IFACE
      INTEGER                      :: J, JBANK, JBC, JEL, K, KFACE, NOCPB, TYPEE
      DOUBLEPRECISION              :: ADUM (5)
      LOGICAL                      :: TEST
      CHARACTER (LEN=77)           :: MSG
!----------------------------------------------------------------------*
!
! NUMBER OF CATEGORIES FOR EACH TYPE
!:OC20
      READ (OCD, * )
      read (OCD, * ) NOCHB,NOCFB,NOCPB
!
! INITIALIZATION
!
      NOCBC = 0
      DO 10 iely = 1,total_no_elements
         NOCBCC (iely) = 0
10    END DO
!
! HEAD BOUNDARY (TYPE 3)
!:OC22
      IF (NOCHB.GT.0) THEN
         MSG = 'ERROR IN OC HEAD BOUNDARY GRID'
         CALL AREADI (IDUM, 0, OCD, PPPRI, NOCHB)
         DO iely = NGDBGN,total_no_elements
            ICAT = IDUM (iely)
            IF ((ICAT.LT.0).OR.(ICAT.GT.NOCHB)) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1020, PPPRI, iely, 0, MSG)
            ELSEIF (ICAT.GT.0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC.GT.NOCTAB) CYCLE
               NOCBCC (iely) = NOCBC
               NOCBCD (NOCBC, 1) = iely
               NOCBCD (NOCBC, 2) = 0
               NOCBCD (NOCBC, 3) = 3
               NOCBCD (NOCBC, 4) = ICAT
            ENDIF
         ENDDO
      ENDIF
!
! FLUX BOUNDARY (TYPE 4)
!:OC24
      IF (NOCFB.GT.0) THEN
         MSG = 'ERROR IN OC FLUX BOUNDARY GRID'
         CALL AREADI (IDUM, 0, OCD, PPPRI, NOCFB)
         DO 40 iely = NGDBGN,total_no_elements
            ICAT = IDUM (iely)
            IF ((ICAT.LT.0).OR.(ICAT.GT.NOCFB)) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1021, PPPRI, iely, 0, MSG)
            ELSEIF (ICAT.GT.0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC.GT.NOCTAB) CYCLE
               NOCBCC (iely) = NOCBC
               NOCBCD (NOCBC, 1) = iely
               NOCBCD (NOCBC, 2) = NBFACE (iely)
               NOCBCD (NOCBC, 3) = 4
               NOCBCD (NOCBC, 4) = ICAT
            ENDIF
40       END DO
      ENDIF
!
! POLYNOMIAL FUNCTION BOUNDARY (TYPE 5)
!:OC26
      IF (NOCPB.GT.0) THEN
         IBC0 = NOCBC
         MSG = 'ERROR IN OC POLYNOMIAL FUNCTION BOUNDARY GRID'
         CALL AREADI (IDUM, 0, OCD, PPPRI, NOCPB)
         DO 60 iely = NGDBGN,total_no_elements
            ICAT = IDUM (iely)
            IF ((ICAT.LT.0).OR.(ICAT.GT.NOCPB)) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1022, PPPRI, iely, 0, MSG)
            ELSEIF (ICAT.GT.0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC.GT.NOCTAB) CYCLE
               NOCBCC (iely) = NOCBC
               NOCBCD (NOCBC, 1) = iely
               NOCBCD (NOCBC, 2) = NBFACE (iely)
               NOCBCD (NOCBC, 3) = 5
               NOCBCD (NOCBC, 4) = ICAT
            ENDIF
60       END DO
!:OC28
         MSG = 'Error reading polynomial function data in OC'
         read (OCD, * )
         DO 80 I = 1, NOCPB
            read (OCD, * ) ICAT, ADUM
            IF (ICAT.NE.I) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1031, PPPRI, iely, 0, MSG)
            ELSE
               DO 70 IBC = IBC0 + 1, MIN (NOCBC, NOCTAB)
                  TEST = NOCBCD (IBC, 4) .EQ.I
                  !IF (TEST) CALL DCOPY(5, ADUM, 1, COCBCD(1:5,IBC), 1)
                  IF (TEST) COCBCD(1:5,IBC) = adum
70             END DO
            ENDIF
80       END DO
      ENDIF
!
! SET CHANNEL LINK BOUNDARY TYPES (other data will follow)
!
      DO I = 1, NX
         DO J = 1, NY
            DO 90 K = 0, 1
               TYPEE = LCODEX (I, J) * (1 - K) + LCODEY (I, J) * K
               IF ((TYPEE.GE.7).AND.(TYPEE.LE.11)) THEN
                  iely = LINKNO (I, J, K.EQ.0)
                  NOCBC = NOCBC + 1
                  IF (NOCBC.LE.NOCTAB) THEN
                     NOCBCC (iely) = NOCBC
                     NOCBCD (NOCBC, 1) = iely
                     NOCBCD (NOCBC, 3) = TYPEE
                     IF (TYPEE.EQ.9) NOCHB = NOCHB + 1
                     IF (TYPEE.EQ.10) NOCFB = NOCFB + 1
                  ENDIF
               ENDIF
90          END DO
         END DO
      END DO
!
! SET INTERNAL IMPERMEABLE GRID BOUNDARY CONDITIONS (TYPE 1)
!
! NB Impermeability extended across ends of any adjacent bank elements
!
      IBC0 = NOCBC
      DO I = 1, NX
         DO J = 1, NY
            DO 107 IFACE = 3, 4
               TYPEE = LCODEX (I, J) * (4 - IFACE) + LCODEY (I, J) &
                  * (IFACE-3)
               IF (TYPEE.EQ.1) THEN
                  iely = ICMXY (I, J)
                  JEL = 0
                  IF (iely.GT.0) JEL = ICMREF (iely, 4 + IFACE)
                  IF (JEL.GT.0) THEN
                     NOCBC = NOCBC + 1
                     IF (NOCBC.LE.NOCTAB) THEN
                        NOCBCC (iely) = NOCBC
                        NOCBCD (NOCBC, 1) = iely
                        NOCBCD (NOCBC, 2) = IFACE
                     ENDIF
                     NOCBC = NOCBC + 1
                     IF (NOCBC.LE.NOCTAB) THEN
                        NOCBCC (JEL) = NOCBC
                        NOCBCD (NOCBC, 1) = JEL
                        NOCBCD (NOCBC, 2) = ICMREF (iely, 8 + IFACE)
                     ENDIF
                     DO 104 BANK = 2, 1, - 1
                        KFACE = 9 - IFACE-2 * BANK
                        IBANK = ICMREF (iely, 4 + KFACE)
                        IBK = 0
                        IF (IBANK.GT.0) IBK = ICMREF (IBANK, 1)
                        IF (IBK.EQ.BANK) THEN
                           NOCBC = NOCBC + 1
                           IF (NOCBC.LE.NOCTAB) THEN
                              NOCBCC (IBANK) = NOCBC
                              NOCBCD (NOCBC, 1) = IBANK
                              NOCBCD (NOCBC, 2) = IFACE
                           ENDIF
                           NOCBC = NOCBC + 1
                           IF (NOCBC.GT.NOCTAB) CYCLE
                           JBANK = ICMREF (IBANK, 4 + IFACE)
                           NOCBCC (JBANK) = NOCBC
                           NOCBCD (NOCBC, 1) = JBANK
                           NOCBCD (NOCBC, 2) = ICMREF (IBANK, 8 + IFACE)
                        ENDIF
104                  END DO
                  ENDIF
               ENDIF
107         END DO
         END DO
      END DO
      DO 120 IBC = IBC0 + 1, MIN (NOCBC, NOCTAB)
         NOCBCD (IBC, 3) = 1
         NOCBCD (IBC, 4) = 1
120   END DO
!
! CHECK
!
      IF (NOCBC.GT.NOCTAB) THEN
         IXER = IXER + 1
         WRITE (MSG, 9050) NOCBC, NOCTAB
         CALL ERROR(EEERR, 1050, PPPRI, 0, 0, MSG)
      ENDIF
      DO 1000 IBC = 1, MIN (NOCBC, NOCTAB)
         iely = NOCBCD (IBC, 1)
         JBC = NOCBCC (iely)
         IF (JBC.NE.IBC) THEN
            IXER = IXER + 1
            WRITE (MSG, 9100) NOCBCD (IBC, 3), NOCBCD (JBC, 3)
            CALL ERROR(EEERR, 1059, PPPRI, iely, 0, MSG)
         ENDIF
1000  END DO
!
! FINISH
!
9050  FORMAT ('Number of OC boundary conditions NOCBC =',I4,2X, &
      &        'exceeds array size NOCTAB =',I4)

9100  FORMAT ('Element has multiple OC boundary conditions ', &
      &        '(types',I2,' and',I2,')')
   END SUBROUTINE JEOCBC

   SUBROUTINE OCPLF(BOUT, IXER, fromNOCBCD, NXDEF, XDEFW)
!----------------------------------------------------------------------*
!
!  READ IN DATA FOR EACH CHANNEL LINK
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCPLF/4.2
! [Version history as in original...]
!----------------------------------------------------------------------*
      LOGICAL, INTENT(INOUT) :: BOUT
      INTEGER, INTENT(INOUT) :: IXER
      INTEGER, INTENT(INOUT) :: fromNOCBCD(NOCTAB, 2:4)
      INTEGER, INTENT(OUT)   :: NXDEF (NOCTAB)
      DOUBLEPRECISION        :: XDEFH (NOCTAB, NOCTAB), XDEFW (NOCTAB, NOCTAB)
      INTEGER                :: I, IBC, IDEF, IDEFX, ielm, J, N, NDEFCT, TYPEE
      DOUBLEPRECISION        :: STR, WDEPTH, ZG
      LOGICAL                :: TEST, g8055, g8013, g8300, greturn
      CHARACTER(102)         :: MSG
!----------------------------------------------------------------------*
!
! READ DEFAULT CHANNEL CROSS-SECTIONS
!:OC30
      read (OCD, * )
      read (OCD, * ) NDEFCT
      IF ((NDEFCT.GT.NOCTAB).OR.(NDEFCT.LT.0)) THEN
         WRITE (MSG, 9054) NDEFCT, NOCTAB
         CALL ERROR(EEERR, 1054, PPPRI, 0, 0, MSG)
         IXER = IXER + 1
      ENDIF

      g8013=.FALSE.
      g8055=.FALSE.
      g8300=.FALSE.
      greturn=.FALSE.

!:OC32
      IF (NDEFCT.GT.0) THEN
         read (OCD, * )
         IF (BOUT) WRITE(PPPRI, 9032) 'Category', 'Width', 'Height'
         out100 : DO IDEF = 1, NDEFCT
            IF(g8055) CYCLE out100
            read (OCD, * ) N
            IF ((N.GT.NOCTAB).OR.(N.LT.2)) THEN
               g8055=.TRUE.
               CYCLE out100
            ENDIF
            NXDEF (IDEF) = N
            read (OCD, * ) (XDEFW (IDEF, J), XDEFH (IDEF, J), J = 1, N)
            IF (BOUT) WRITE(PPPRI, 9034) IDEF, (XDEFW (IDEF, J), XDEFH ( IDEF, J), J = 1, N)
         ENDDO out100
      ENDIF
!
! READ DATA FOR EACH LINK
!:OC35
      IF(g8055) THEN
         WRITE (MSG, 9055) IDEF, N, NOCTAB
         CALL ERROR(EEERR, 1055, PPPRI, 0, 0, MSG)
         IXER = IXER + 1
      ELSE
         read (OCD, * )
         IF (BOUT) WRITE(PPPRI, 9035) 'Element', 'Elevation', 'Init.Depth', 'Strickler', 'Width', 'Height'
         out500 : DO ielm = 1, total_no_links
            IF(g8013 .OR. g8300 .OR. greturn) CYCLE out500

            ! Try to read the input data - handle errors inline
            read (OCD, *, ERR = 8300, END = 8300) I, ZG, WDEPTH, STR, IDEFX

            ! Check if we read the expected element number
            IF (I.NE.ielm) THEN
               g8013=.TRUE.
               CYCLE out500
            ENDIF

            ZGRUND (ielm) = ZG
            CALL SETHRF(ielm, ZG + WDEPTH)
            STRXX(ielm) = STR
            STRYY(ielm) = STR
            !:OC37
            TEST = (IDEFX.EQ.1).OR.(IDEFX.GT.NOCTAB)
            IF ((IDEFX.EQ.0).OR.(IDEFX.LT.-NDEFCT).OR.TEST) THEN
               WRITE (MSG, 9012) IDEFX, - NDEFCT, NOCTAB
               CALL ERROR(EEERR, 1012, PPPRI, ielm, 0, MSG)
               IXER = IXER + 1
               IF (TEST) THEN
                  greturn=.TRUE.
                  CYCLE out500
               ENDIF
               ! Continue to boundary condition processing without setting cross-section data
            ELSEIF (IDEFX.GT.0) THEN
               N = IDEFX
               read (OCD, * ) (XINW (ielm, J), XINH (ielm, J), J = 1, N)
               IF (BOUT) WRITE(PPPRI, 9037) ielm, ZG, WDEPTH, STR, (XINW ( &
                  ielm, J), XINH (ielm, J), J = 1, N)
               NXSECT (ielm) = N
               CWIDTH (ielm) = XINW (ielm, N)
               ZBFULL (ielm) = XINH (ielm, N) + ZG
            ELSE
               IDEF = - IDEFX
               N = NXDEF (IDEF)
               !CALL DCOPY(N, XDEFH(IDEF,1), NOCTAB, XINH(ielm,1), NLFEE)
               !CALL DCOPY(N, XDEFW(IDEF,1), NOCTAB, XINW(ielm,1), NLFEE)
               XINH(ielm,1:n) = XDEFH(IDEF,1:n)
               XINW(ielm,1:n) = XDEFW(IDEF,1:n)
               IF (BOUT) WRITE(PPPRI, 9137) ielm, ZG, WDEPTH, STR, IDEF
               NXSECT (ielm) = N
               CWIDTH (ielm) = XINW (ielm, N)
               ZBFULL (ielm) = XINH (ielm, N) + ZG
            ENDIF

            !
            ! read IN ADDITIONAL DATA FOR BOUNDARY CONDITIONS
            !:OC38-41
            IBC = NOCBCC (ielm)
            IF (IBC.GT.0) THEN
               TYPEE = fromNOCBCD (IBC, 3)
               IF((TYPEE.EQ.7).OR.(TYPEE.EQ.8)) THEN
                  read (OCD, * ) fromNOCBCD (IBC, 2), (COCBCD (J, IBC), J = 1, 4)
                  fromNOCBCD (IBC, 4) = 1
               ELSEIF (TYPEE.EQ.9) THEN
                  fromNOCBCD (IBC, 2) = 0
                  read (OCD, * ) fromNOCBCD (IBC, 4)
               ELSEIF (TYPEE.EQ.10) THEN
                  read (OCD, * ) (fromNOCBCD (IBC, J), J = 2, 4, 2)
               ELSEIF (TYPEE.EQ.11) THEN
                  read (OCD, * ) fromNOCBCD (IBC, 2), (COCBCD (J, IBC), J = 1, 5)
                  fromNOCBCD (IBC, 4) = 1
               ENDIF
            ENDIF
            CYCLE out500

8300        g8300=.TRUE.
            CYCLE out500
         ENDDO out500
      ENDIF
      IF(greturn) THEN
         RETURN
      ELSEIF(g8013) THEN
         WRITE (MSG, 9013) ielm, I
         CALL ERROR(EEERR, 1013, PPPRI, ielm, 0, MSG)
         IXER = IXER + 1
      ELSEIF(g8300) THEN
         MSG = 'Channel input data is missing or has incorrect format'
         CALL ERROR(EEERR, 1019, PPPRI, ielm, 0, MSG)
         IXER = IXER + 1
      ENDIF
9012  FORMAT ('Cross-section number IDEFX =',I4,' lies outside ranges', &
      &        ' -NDEFCT:-1 =',I4,' : -1  and  2:NOCTAB = 2 :',I4)

9013  FORMAT ('Expected element number,',I5,', but found',I5,', ', &
      &        'while reading channel data')

9032  FORMAT (/5X,'Default Channel Cross-sections:'//5X,3A10/)

9034  FORMAT (5X,I10,(T16,2F10.3))

9035  FORMAT (/5X,'Link Element Data:'//5X,6A11/)

9037  FORMAT (5X,I11,3F11.3,(T50,2F11.3))

9054  FORMAT ('Number of default channel cross-section categories ', &
      &        'NDEFCT =',I4,2X,'lies outside range 0:NOCTAB = 0 :',I4)

9055  FORMAT ('Number of width/elevation pairs NXDEF(',I3,') =',I4,2X, &
      &        'lies outside range 2:NOCTAB = 2:',I4)

9137  FORMAT (5X,I11,3F11.3,3X,'default category',I3)
   END SUBROUTINE OCPLF

END MODULE oc_input
