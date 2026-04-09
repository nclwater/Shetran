MODULE OCmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces part of the OC .F files
   USE SGLOBAL
   USE AL_C ,     ONLY : IDUM, NBFACE, CWIDTH, ZBFULL, &
      DUMMY, ZBEFF, ICMBK, BEXBK, QBKB, QBKF, ICMRF2, &
      TIH, DHF, CLENTH, CLENTH, PNETTO, QH, QOC, LINKNS, ARXL
   USE AL_D ,     ONLY : DQ0ST, DQIST, DQIST2, OCNOW, OCNEXT, OCD, ESWA, QMAX, NOCBCC, &
      NOCBCD, LCODEX, LCODEY, NOCTAB, OHB, OFB
   USE AL_G ,     ONLY : NGDBGN, NX, NY, ICMREF, ICMXY
   USE UTILSMOD , ONLY : HINPUT, FINPUT, AREADR, AREADI, JEMATMUL_VM, JEMATMUL_MM, INVERTMAT
   USE mod_load_filedata ,    ONLY : ALCHK, ALCHKI
   USE OCmod2 ,   ONLY : GETHRF, GETQSA, GETQSA_ALL, SETHRF, SETQSA, CONVEYAN, OCFIX, XSTAB, &
      HRFZZ, qsazz, INITIALISE_OCMOD  !these needed only for ad
   USE OCQDQMOD,  ONLY : OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD !, &  !REST NNEDED ONLY FOR AD

!                     firstocqdq
!FROM SPEC_OC
!-------------------------- Start of SPEC.OC --------------------------*
!
! ^^^ COMMON FILE OF SPECIFICATIONS OF OC COMPONENT VARIABLES.
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/SPEC.OC/4.2
! Modifications:
!   GP        FEB 89    2.0   'SHE88' IMPLEMENTATION ON NEWCASTLE AMDAHL
!   GP        AUG 89    2.1     ADD LOGICAL BIOWAT
!   GP        NOV 89    2.2     ADD VARIABLES FOR NEW IMPLICIT OC
!   GP        APR 91    3.0     SHETRAN REWRITE
!                               + INCLUDING IMPLICIT SCHEME AND BANKS
!   GP        JAN 92    3.2     ADD WLMIN, DQIST2(NLFEE,3)
!   GP        JUN 92    3.4     VARIABLES MOVED TO AL.D FOR HOTSTART
!                               (arrays QSA,DQ0ST,DQIST,DQIST2).
!  GP  960103  4.0  Move NOCBCC,NOCBCD to AL.D.
! RAH  970221  4.1  Correct spelling: NCATR was NACTR.  Explicit typing.
!                   Remove CFDEB,SURFS,SURFZ,DDDZST (redundant).
! RAH  971215  4.2  Remove LONT (see OCSIM).  Move DD,EE,GG to OCSIM.
!      980107       Move AA,BB,CC,FF to OCSIM (see also OCABC).
!      980119       Amend mod note above (3.4 was 3.3).
!                   Remove PT,TEMPS (see OCINI).  Move DET to OCINI.
!      980120       Remove NCATRE (see OCINI,OCBLOC - deleted).
!                   Move KONT,NCATR,CDRS,CATR,BIOWAT to OCINI.
!      980121       Move NOCBC,NOCPB to OCBC.
!                   Move NDEFCT,NXDEF,XDEFH,XDEFW to OCPLF.
!      980205       Move IXER to OCREAD.
!      980210       Move NXOC to OCIND.
!      980212       Remove NROWFN (see OCSIM,OCIND). Mv WLMIN to OCQMLN.
!                   Increase NROWST size by 1, reduce XCONV,XDERIV by 1.
!      980225       Swap COCBCD subscripts - also in
!                   OCINI,OCREAD,OCBC,OCPLF,OCQLNK,OCQBC.
!      980226       Move DTOC to OCSIM.  Add TDC,TFC (see OCINI,OCSIM).
!      980408       Move ROOT2G to OCQBNK.
!      980424       Merge XSECTH,XCONV,XDERIV into XSTAB
!                   (see OCINI,OCXS,OCSIM).
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Requirements:
!  NXSCEE.ge.2
!----------------------------------------------------------------------*
   IMPLICIT NONE
   INTEGER            :: NELIND (NELEE)
   INTEGER            :: NROWF, NROWL, NOCHB, NOCFB
   INTEGER            :: NROWEL (NELEE), NROWST (NYEE+1), NXSECT (NLFEE)
   DOUBLEPRECISION    :: HOCLST, HOCNXT, QFLAST, QFNEXT, TDC, TFC
   DOUBLEPRECISION    :: HOCPRV (NOCTAB), QOCFIN (NOCTAB), HOCNXV (NOCTAB)
   DOUBLEPRECISION    :: XINH (NLFEE, NOCTAB)
   DOUBLEPRECISION    :: XINW (NLFEE, NOCTAB)
   DOUBLEPRECISION    :: XAREA (NLFEE, NOCTAB)
   DOUBLEPRECISION     :: dtoc

   PRIVATE

   PUBLIC :: OCINI, OCSIM, OCLTL, LINKNO, & !REST ARE PUBLIC FOR AD ONLY
      qfnext, hoclst, hocprv, qocfin, hocnxt, hocnxv

CONTAINS



   !SSSSSS SUBROUTINE OCINI
   SUBROUTINE OCINI()
   !----------------------------------------------------------------------*
   !  Control OC initialization
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCINI/4.2
   ! Modifications:
   !  GP       3.4  Don't set OCNOW,OCVAL,OCNEXT (see also FRINIT,SHE).
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   ! RAH  961228  4.1  Remove variables T & TF.
   ! RAH  980119  4.2  Explicit typing.
   !                   Scrap SPEC.AL variables WSOC,WSOCI,WSOCER, SPEC.OC
   !                   arrays PT,TEMPS, & local variables DT,IDT,TITRE,VTP.
   !      980120       Use SQRT not DSQRT.
   !                   Bring KONT from SPEC.OC & pass to OCREAD.
   !      980130       Call OCCHK0 (new).  Move read section to OCREAD.
   !      980202       Write NXSCEE.  Bring OCIND call from OCREAD.
   !      980203       Pass NGDBGN to OCCHK0, but not OHB,OFB.
   !                   Call OCCHK1, OCCHK2 and OCXS (new).
   !                   Bring OHB,OFB initial read from OCBC.
   !      980205       Pass LDUM1 to OCCHK1. Full argument list for OCREAD.
   !      980210       Full argument list for OCIND.
   !      980212       Move WLMIN (SPEC.OC) to OCQMLN.
   !      980218       (Remove NGDBGN from OCREAD argument list.)
   !      980226       Get TDC,TFC from OCREAD.
   !      980408       Move ROOT2G (SPEC.OC) to OCQBNK.
   !      980424       Merge XSECTH,XCONV,XDERIV into XSTAB (SPEC.OC).
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  [NELEE,NLFEE,NXEE,NY,NOCTAB ].ge.1    NXSCEE.ge.2    NEL.ge.NGDBGN
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! NOCTAB, NELEE, total_no_links, NXSCEE, PPPRI, BEXBK, NROWF, NROWL, 
      ! NROWST, NELIND, NROWEL, NOCHB, OHB, NOCFB, OFB, DUMMY

      IMPLICIT NONE

      ! Locals
      INTEGER :: KONT
      DOUBLE PRECISION :: DDUM1 (NOCTAB), DDUM2 (NOCTAB, NOCTAB)
      DOUBLE PRECISION :: TDC, TFC
      LOGICAL :: LDUM1 (NELEE)

   !----------------------------------------------------------------------*

      CALL OCCHK0()
      
      ! Call to check constraints using AD-aliasing safe interface
      CALL OCCHK1(SIZE(LDUM1), LDUM1) 

      ! Input data & associated requirements
      CALL OCREAD(KONT, TDC, TFC, DDUM1, DDUM2)
      CALL OCCHK2(DUMMY, DDUM1, NELEE, LDUM1)

      ! Boundary data files
      ! Read title lines if applicable
      IF (NOCHB > 0) READ (OHB, *)
      IF (NOCFB > 0) READ (OFB, *)

      CALL INITIALISE_OCMOD()

      ! Cross-section tables & effective bed elevations
      IF (total_no_links > 0) THEN
         IF (MOD(KONT, 2) == 1) WRITE(PPPRI, 9100) NXSCEE
         CALL OCXS()
      END IF

      ! Indicies for Thomas algorithm
      CALL OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)

      RETURN

      ! FORMAT statements
9100  FORMAT (/5X, 'Size of internal tables for channel conveyance, etc', '  NXSCEE =', I6)

   END SUBROUTINE OCINI



   !SSSSSS SUBROUTINE OCABC
   SUBROUTINE OCABC(IND, IROW, IELZ, NSV, NCR, NPR, IBC, N, AREAE, &
         ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW, AA, BB, CC, FF)
   !----------------------------------------------------------------------*
   ! CALCULATION OF MATRIX COEFFICIENTS, GIVEN FLOWS AND DERIVATIVES
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCABC/4.2
   ! Modifications:
   ! GP   930326  3.4  Don't set EEVAP(IEL) (see ETIN).
   !                   Replace PNETTO(IEL)-EPDUM (part of FF(IND)) with
   !                   PDUM-ESWA(IEL).
   !                   Don't subtract QBKI(IEL,IBK) from BKDUM.
   ! RAH  941003 3.4.1 Bring IMPLICIT from SPEC.AL.
   ! GP   960115  4.0  Replace QUZR(IEL)+QSZR(IEL) (part of FF) with QHDUM.
   ! RAH  980107  4.2  Explicit typing.  Amend description in header.
   !                   Add arguments IND,IEL,N,AREAE,CL,ZBF,ZG,Z;
   !                   remove ICOUNT; also, move AA,BB,CC,FF from SPEC.OC
   !                   to arg-list & reduce dimensions by one (see OCSIM).
   !                   Ensure AR defined (for links) when Z=ZBF.
   !                   New locals HI,HM,IM,WI,WM.
   !      980108       Scrap JFACE2.  New local IBR.  Use BLINK not ITYPE.
   !                   Initialize AA,BB,CC,FF here, not in OCSIM.
   !                   Unroll loop, and use ELSE, for BKDUM and QHDUM.
   !                   Add arguments NSV,NCR,NPR,PNETT,QHE,ESWAE.
   !      980115       Set head boundaries (were in OCSIM, after solver).
   !      980226       Move DTOC from SPEC.OC to arg-list (see OCSIM).
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! NXOCEE, DTOC, ZERO, ONE, ICMREF, ICMRF2, DQ0ST, DQIST2, QBKB, DQIST,
      ! QSA, QBKF, NELIND, XINH, XINW

      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)          :: IND, IROW, IELZ, NSV, NCR, NPR, IBC, N
      DOUBLE PRECISION, INTENT(IN) :: AREAE, ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW
      DOUBLE PRECISION, INTENT(OUT):: AA(NXOCEE), BB(NCR), CC(NXOCEE), FF

      ! Local Variables
      INTEGER                      :: I, IBR, IFACE, IM, J, JEL, JFACE, JND, JROW
      DOUBLE PRECISION             :: AR, BKDUM, DQ0, DQI, H, HI, HM, PDUM, Q
      DOUBLE PRECISION             :: QHDUM, WI, WM
      LOGICAL                      :: BLINK, TEST

   !----------------------------------------------------------------------*

   ! ----- INITIALIZE OUTPUT ARRAYS & GET WATER DEPTH
      ! Performance Rollback: Explicit DO loops bypass dope-vector overhead for micro-arrays
      IF (NSV > 0) THEN
         DO I = 1, NSV
            AA(I) = ZERO
         END DO
      END IF

      DO I = 1, NCR
         BB(I) = ZERO
      END DO

      IF (NPR > 0) THEN
         DO I = 1, NPR
            CC(I) = ZERO
         END DO
      END IF

      H = Z - ZG

   ! ----- HEAD BOUNDARY
      IF (IBC == 3 .OR. IBC == 9) THEN
         BB(IND) = ONE
         FF = HNOW - H
         RETURN
      END IF

   ! ----- IS THE CURRENT ELEMENT A LINK?
      BLINK = (ICMREF(IELZ, 1) == 3)

   ! ----- PUT STORAGE TERM INTO CENTRAL COEFFICIENT FOR CURRENT ELEMENT
      TEST = BLINK
      IF (TEST) TEST = (Z < ZBF)

      IF (TEST) THEN
         ! * note requirements: XINH(IEL,1)=0; XINH(IEL,N).GE.ZBF-ZG
         search_loop: DO I = 2, N
            HI = XINH(IELZ, I)
            IF (H < HI) THEN
               IM = I - 1
               HM = XINH(IELZ, IM)
               WM = XINW(IELZ, IM)
               WI = XINW(IELZ, I)
               AR = CL * (WM + (WI - WM) * ((H - HM) / (HI - HM)))
               EXIT search_loop
            END IF
         END DO search_loop
      ELSE
         AR = AREAE
      END IF

      BB(IND) = -AR / DTOC

   ! ----- PUT PRECIPITATION, EVAPORATION AND EXCHANGE FLOWS INTO RHS
      PDUM = PNETT
      IF (BLINK) THEN
         IF (H < 1.0D-8) PDUM = ZERO
         BKDUM = QBKB(IELZ, 1) + QBKF(IELZ, 1) + QBKB(IELZ, 2) + QBKF(IELZ, 2)
         QHDUM = ZERO
      ELSE
         BKDUM = ZERO
         QHDUM = QHE
      END IF

      FF = -AREAE * (PDUM + QHDUM - ESWAE) + BKDUM

   ! ----- LOOP OVER ADJACENT ELEMENTS
      face_loop: DO IFACE = 1, 4
         JEL = ICMREF(IELZ, IFACE + 4)
         JFACE = ICMREF(IELZ, IFACE + 8)

   ! --- GET FLOW AND DERIVATIVE (+VE INTO ELEMENT)
         Q = GETQSA (ielz, IFACE)  
         DQ0 = DQ0ST(IELZ, IFACE)

   ! --- ADD INTO COEFFICIENTS FOR CURRENT ELEMENT
         BB(IND) = BB(IND) + DQ0
         FF = FF - Q

   ! --- TEST FOR SINGLE ADJACENT ELEMENT
         IF (JEL > 0) THEN
            JROW = ICMREF(JEL, 3)
            JND = NELIND(JEL)
            DQI = DQIST(IELZ, IFACE)

   !        ADD DERIVATIVE TO COEFFICIENT FOR ADJACENT ELEMENT
            IF (JROW == IROW) BB(JND) = BB(JND) + DQI
            IF (JROW > IROW)  AA(JND) = AA(JND) + DQI
            IF (JROW < IROW)  CC(JND) = CC(JND) + DQI

   ! --- SIMILARLY FOR MULTIPLE ADJACENT LINKS
         ELSE IF (JEL < 0) THEN
            IBR = -JEL
            DO J = 1, 3
               JEL = ICMRF2(IBR, J)
               IF (JEL > 0) THEN
                  JROW = ICMREF(JEL, 3)
                  JND = NELIND(JEL)
                  DQI = DQIST2(IBR, J)
                  
                  IF (JROW == IROW) BB(JND) = BB(JND) + DQI
                  IF (JROW > IROW)  AA(JND) = AA(JND) + DQI
                  IF (JROW < IROW)  CC(JND) = CC(JND) + DQI
               END IF
            END DO
         END IF
      END DO face_loop

   END SUBROUTINE OCABC



   !SSSSSS SUBROUTINE JEOCBC
   SUBROUTINE JEOCBC(IXER, NOCBC)
   !----------------------------------------------------------------------*
   !
   !  Set up boundary data (except for some channel link details to follow)
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCBC/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Bring IMPLICIT from SPEC.AL.
   ! GP   970207  4.2  Add missing code for polynomial coeffs (see BR/50).
   ! RAH  971218       Explicit typing.  List-directed reads.
   !                   Initialize NOCBCC, but not NOCBCD(link) (use OCPLF).
   !                   Use local TYPE.  Loop 90 instead of duplicate.
   !                   Fix error in use of IBANK (using new loop 107).
   !      980115       Trap ICAT<0 and NOCBC>NOCTAB.  Pass IEL to ERROR.
   !                   Update IXER.  Replace ADUM,...,EDUM with ADUM(5).
   !                   Amend COCBCD index, & replace STOP with call ERROR.
   !                   Use IEL,TYPE for L,LC.
   !      980121       Ensure NOCBCD(*,4).ge.1.  Use locals MSG,TEST.
   !                   Bring NOCBC,NOCPB from SPEC.OC.
   !      980203       Move OHB,OFB initial read to OCINI.
   !      980204       Full argument list (no INCLUDEs) (see OCREAD);
   !                   ERR local.  Move NOCBC to argument list.
   !      980206       Check for elements with multiple BCs.
   !      980218       (Fix error in loop 120 start value).
   !      980225       Swap COCBCD subscripts (see SPEC.OC)
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! OCD, NOCHB, NOCFB, total_no_elements, NOCBCC, PPPRI, EEERR, NOCTAB, 
      ! NOCBCD, NBFACE, COCBCD, NX, NY, LCODEX, LCODEY, LINKNO, ICMXY, ICMREF
      ! *CRITICAL*: Ensure 'IDUM' is available via host module

      IMPLICIT NONE 

      ! Arguments
      INTEGER, INTENT(OUT)         :: IXER
      INTEGER, INTENT(OUT)         :: NOCBC

      ! Local Variables
      INTEGER                      :: BANK, I, IBANK, IBC, IBC0, IBK, ICAT
      INTEGER                      :: IELY, IFACE, J, JBANK, JBC, JEL, K
      INTEGER                      :: KFACE, NOCPB, TYPEE
      DOUBLE PRECISION             :: ADUM(5)
      LOGICAL                      :: TEST
      CHARACTER(LEN=77)            :: MSG

   !----------------------------------------------------------------------*

   ! NUMBER OF CATEGORIES FOR EACH TYPE
      READ (OCD, *)
      READ (OCD, *) NOCHB, NOCFB, NOCPB

   ! INITIALIZATION
      NOCBC = 0
      
      ! Vectorized zeroing for large array
      NOCBCC(1:total_no_elements) = 0

   ! HEAD BOUNDARY (TYPE 3)
      IF (NOCHB > 0) THEN
         MSG = 'ERROR IN OC HEAD BOUNDARY GRID'
         CALL AREADI(IDUM, 0, OCD, PPPRI, NOCHB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCHB) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1020, PPPRI, IELY, 0, MSG)
            ELSE IF (ICAT > 0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC > NOCTAB) CYCLE
               NOCBCC(IELY) = NOCBC
               NOCBCD(NOCBC, 1) = IELY
               NOCBCD(NOCBC, 2) = 0
               NOCBCD(NOCBC, 3) = 3
               NOCBCD(NOCBC, 4) = ICAT
            END IF
         END DO
      END IF

   ! FLUX BOUNDARY (TYPE 4)
      IF (NOCFB > 0) THEN
         MSG = 'ERROR IN OC FLUX BOUNDARY GRID'
         CALL AREADI(IDUM, 0, OCD, PPPRI, NOCFB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCFB) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1021, PPPRI, IELY, 0, MSG)
            ELSE IF (ICAT > 0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC > NOCTAB) CYCLE
               NOCBCC(IELY) = NOCBC
               NOCBCD(NOCBC, 1) = IELY
               NOCBCD(NOCBC, 2) = NBFACE(IELY)
               NOCBCD(NOCBC, 3) = 4
               NOCBCD(NOCBC, 4) = ICAT
            END IF
         END DO
      END IF

   ! POLYNOMIAL FUNCTION BOUNDARY (TYPE 5)
      IF (NOCPB > 0) THEN
         IBC0 = NOCBC
         MSG = 'ERROR IN OC POLYNOMIAL FUNCTION BOUNDARY GRID'
         CALL AREADI(IDUM, 0, OCD, PPPRI, NOCPB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCPB) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1022, PPPRI, IELY, 0, MSG)
            ELSE IF (ICAT > 0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC > NOCTAB) CYCLE
               NOCBCC(IELY) = NOCBC
               NOCBCD(NOCBC, 1) = IELY
               NOCBCD(NOCBC, 2) = NBFACE(IELY)
               NOCBCD(NOCBC, 3) = 5
               NOCBCD(NOCBC, 4) = ICAT
            END IF
         END DO

         MSG = 'Error reading polynomial function data in OC'
         READ (OCD, *)

         DO I = 1, NOCPB
            READ (OCD, *) ICAT, ADUM
            IF (ICAT /= I) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1031, PPPRI, IELY, 0, MSG)
            ELSE
               DO IBC = IBC0 + 1, MIN(NOCBC, NOCTAB)
                  TEST = (NOCBCD(IBC, 4) == I)
                  IF (TEST) COCBCD(1:5, IBC) = ADUM
               END DO
            END IF
         END DO
      END IF

   ! SET CHANNEL LINK BOUNDARY TYPES (other data will follow)
      x_link_loop: DO I = 1, NX
         y_link_loop: DO J = 1, NY
            DO K = 0, 1
               TYPEE = LCODEX(I, J) * (1 - K) + LCODEY(I, J) * K
               IF (TYPEE >= 7 .AND. TYPEE <= 11) THEN
                  IELY = LINKNO(I, J, K == 0)
                  NOCBC = NOCBC + 1
                  IF (NOCBC <= NOCTAB) THEN
                     NOCBCC(IELY) = NOCBC
                     NOCBCD(NOCBC, 1) = IELY
                     NOCBCD(NOCBC, 3) = TYPEE
                     IF (TYPEE == 9) NOCHB = NOCHB + 1
                     IF (TYPEE == 10) NOCFB = NOCFB + 1
                  END IF
               END IF
            END DO
         END DO y_link_loop
      END DO x_link_loop

   ! SET INTERNAL IMPERMEABLE GRID BOUNDARY CONDITIONS (TYPE 1)
   ! NB Impermeability extended across ends of any adjacent bank elements
      IBC0 = NOCBC
      x_grid_loop: DO I = 1, NX
         y_grid_loop: DO J = 1, NY
            DO IFACE = 3, 4
               TYPEE = LCODEX(I, J) * (4 - IFACE) + LCODEY(I, J) * (IFACE - 3)
               IF (TYPEE == 1) THEN
                  IELY = ICMXY(I, J)
                  JEL = 0
                  IF (IELY > 0) JEL = ICMREF(IELY, 4 + IFACE)

                  IF (JEL > 0) THEN
                     NOCBC = NOCBC + 1
                     IF (NOCBC <= NOCTAB) THEN
                        NOCBCC(IELY) = NOCBC
                        NOCBCD(NOCBC, 1) = IELY
                        NOCBCD(NOCBC, 2) = IFACE
                     END IF

                     NOCBC = NOCBC + 1
                     IF (NOCBC <= NOCTAB) THEN
                        NOCBCC(JEL) = NOCBC
                        NOCBCD(NOCBC, 1) = JEL
                        NOCBCD(NOCBC, 2) = ICMREF(IELY, 8 + IFACE)
                     END IF

                     DO BANK = 2, 1, -1
                        KFACE = 9 - IFACE - 2 * BANK
                        IBANK = ICMREF(IELY, 4 + KFACE)
                        IBK = 0
                        IF (IBANK > 0) IBK = ICMREF(IBANK, 1)

                        IF (IBK == BANK) THEN
                           NOCBC = NOCBC + 1
                           IF (NOCBC <= NOCTAB) THEN
                              NOCBCC(IBANK) = NOCBC
                              NOCBCD(NOCBC, 1) = IBANK
                              NOCBCD(NOCBC, 2) = IFACE
                           END IF

                           NOCBC = NOCBC + 1
                           IF (NOCBC > NOCTAB) CYCLE

                           JBANK = ICMREF(IBANK, 4 + IFACE)
                           NOCBCC(JBANK) = NOCBC
                           NOCBCD(NOCBC, 1) = JBANK
                           NOCBCD(NOCBC, 2) = ICMREF(IBANK, 8 + IFACE)
                        END IF
                     END DO
                  END IF
               END IF
            END DO
         END DO y_grid_loop
      END DO x_grid_loop

      ! Vectorized setting types and categories
      IF (NOCBC > IBC0) THEN
         NOCBCD(IBC0 + 1 : MIN(NOCBC, NOCTAB), 3) = 1
         NOCBCD(IBC0 + 1 : MIN(NOCBC, NOCTAB), 4) = 1
      END IF

   ! CHECK
      IF (NOCBC > NOCTAB) THEN
         IXER = IXER + 1
         WRITE (MSG, "('Number of OC boundary conditions NOCBC =',I4,2X,'exceeds array size NOCTAB =',I4)") NOCBC, NOCTAB
         CALL ERROR(EEERR, 1050, PPPRI, 0, 0, MSG)
      END IF

      DO IBC = 1, MIN(NOCBC, NOCTAB)
         IELY = NOCBCD(IBC, 1)
         JBC = NOCBCC(IELY)
         IF (JBC /= IBC) THEN
            IXER = IXER + 1
            WRITE (MSG, "('Element has multiple OC boundary conditions (types',I2,' and',I2,')')") NOCBCD(IBC, 3), NOCBCD(JBC, 3)
            CALL ERROR(EEERR, 1059, PPPRI, IELY, 0, MSG)
         END IF
      END DO

   END SUBROUTINE JEOCBC



   !SSSSSS SUBROUTINE OCCHK0
   SUBROUTINE OCCHK0()
   !----------------------------------------------------------------------*
   !
   !  Check static variables & constants input to the OC
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCCHK0/4.2
   ! Modifications:
   ! RAH  980130  4.2  New.
   !      980203       Add argument NGDBGN; remove OHB,OFB.  NELEE.ge.NX.
   !                   INQUIRE first, and use OUNIT for messages.
   !      980206       NELEE.ge.NOCTAB*NOCTAB.
   ! JE Jan 2009       Above restriction removed
   !----------------------------------------------------------------------*
      INTEGER       :: ERRNUM, I, IUNDEF, IUNIT, NERR, OUNIT
      INTEGER       :: IDUMS (1), IDUMO (1)
      LOGICAL       :: BOPEN, LDUM1 (1)
      CHARACTER(47) :: MSG
      CHARACTER(11) :: FORM
      CHARACTER(3)  :: NAME
      NERR = 0
   !----------------------------------------------------------------------*
   ! 1. Unit Numbers
   ! ---------------
   ! PRI, OCD
      OUNIT = PPPRI
      IUNIT = PPPRI
      NAME = 'PRI'
      
      DO I = 0, 1
         INQUIRE (IUNIT, OPENED = BOPEN, FORM = FORM)
         
         IF (.NOT. BOPEN) THEN
            WRITE (MSG, '("File unit ",A," =",I4,1X,A)') NAME, IUNIT, 'is not connected to a file'
            ERRNUM = 1008
            IF (I == 0) OUNIT = 0
            CALL ERROR(EEERR, ERRNUM, OUNIT, 0, 0, MSG)
            NERR = NERR + 1
         ELSE IF (FORM /= 'FORMATTED') THEN
            WRITE (MSG, '("File unit ",A," =",I4,1X,A,1X,A)') NAME, IUNIT, 'has format type', FORM
            ERRNUM = 1009
            IF (I == 0) OUNIT = 0
            CALL ERROR(EEERR, ERRNUM, OUNIT, 0, 0, MSG)
            NERR = NERR + 1
         END IF
         
         ! Setup for the next iteration (I=1)
         IUNIT = OCD
         NAME = 'OCD'
      END DO
      
      IDUMS (1) = MIN (PPPRI, OCD)

      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ PRI, OCD ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)
      
   ! 2. Array Sizes
   ! --------------
   ! NELEE
      IDUMS (1) = NELEE
      IDUMO (1) = MAX (NX, total_no_elements)! , NOCTAB*NOCTAB)
      CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
   ! NLFEE
      IDUMS (1) = NLFEE
      IDUMO (1) = MAX (1, total_no_links)
      CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
   ! NXEE
      IDUMS (1) = NXEE
      IDUMO (1) = NX
      CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
   ! NOCTAB
      IDUMS (1) = NOCTAB
      CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NOCTAB', 'GE', IONE1, IDUMS, NERR, LDUM1)
   ! NXSCEE
      IDUMS (1) = NXSCEE

      CALL ALCHKI (EEERR, 1002, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXSCEE', 'GT', IONE1, IDUMS, NERR, LDUM1)
      
   ! 3. Number of Entities
   ! ---------------------
   ! NLF
      IDUMS (1) = total_no_links
      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO1, IDUMS, NERR, LDUM1)
      IDUMO (1) = total_no_elements
      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)
   ! NX, NY
      IDUMS (1) = MIN (NX, NY)
      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ NX, NY ]', 'GE', IONE1, IDUMS, NERR, LDUM1)
   ! NGDBGN
      IDUMS (1) = NGDBGN
      IDUMO (1) = total_no_links + 1

      CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NGDBGN', 'EQ', IDUMO, IDUMS, NERR, LDUM1)
      
   ! 4. Finish
   ! ---------
      IF (NERR > 0) THEN
         CALL ERROR(FFFATAL, 1000, OUNIT, 0, 0, 'Error(s) detected while checking OC input variables & constants')
      END IF
      
   END SUBROUTINE OCCHK0



   !SSSSSS SUBROUTINE OCCHK1
   SUBROUTINE OCCHK1(SZLOG, LDUM1)
   !----------------------------------------------------------------------*
   !
   !  Check static OC input arrays
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCCHK1/4.2
   ! Modifications:
   ! RAH  980203  4.2  New.
   !      980205       Add argument LDUM1.
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  [ NEL, NX, NY ].ge.1     NELEE.ge.NEL    NXEE.ge.NX
   !  PRI open for F output    size_of_LDUM1.ge.[NX,NEL]
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! total_no_elements, EEERR, PPPRI, NX, NY, NGDBGN, ICMREF, ICMXY, 
      ! LCODEX, LCODEY, LINKNO, IDUM, IZERO1, FFFATAL

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)  :: SZLOG
      LOGICAL, INTENT(OUT) :: LDUM1(SZLOG)
      
      ! Locals
      INTEGER :: CODE, FACE, I, IELx, X, Y, TYPEE
      INTEGER :: NERR, IUNDEF
      INTEGER :: IDUMO(1)
      
      CHARACTER(LEN=23) :: NAME
      CHARACTER, PARAMETER :: XY(0:1) = ['X', 'Y']
      
   !----------------------------------------------------------------------*
   
   ! Initialize local variables in the executable block to avoid implicit SAVE bugs
      NERR = 0
      IUNDEF = 0
      NAME = 'validity_of_LCODE?(x,y)'

   ! 1. Index Arrays
   ! ---------------
      IDUMO(1) = total_no_elements

   ! ICMREF
      face_loop: DO FACE = 1, 4
         CALL ALCHKI (EEERR, 1057, PPPRI, 1, total_no_elements, FACE, 2, 'ICMREF(iel,face,2)', &
                      'LE', IDUMO, ICMREF(1:total_no_elements, 4+FACE), NERR, LDUM1(1:total_no_elements))
      END DO face_loop

   ! ICMXY
      y_icmxy_loop: DO Y = 1, NY
         ! Modernized: Passing explicit array slice ICMXY(1:NX, Y) instead of scalar start point
         CALL ALCHKI (EEERR, 1057, PPPRI, 1, NX, Y, IUNDEF, 'ICMXY(x,y)', &
                      'LE', IDUMO, ICMXY(1:NX, Y), NERR, LDUM1(1:NX))
      END DO y_icmxy_loop

   ! 2. Channel Definition Arrays
   ! ----------------------------
   ! LCODEX, LCODEY
      xy_loop: DO I = 0, 1
         
         ! Inject 'X' or 'Y' into the 18th character of the string
         NAME(18:18) = XY(I)
         
         y_lcode_loop: DO Y = 1, NY
            x_lcode_loop: DO X = 1, NX
               CODE = 0
               TYPEE = LCODEX(X, Y) * (1 - I) + LCODEY(X, Y) * I
               
               IF (TYPEE >= 7 .AND. TYPEE <= 11) THEN
                  IELx = LINKNO(X, Y, I == 0)
                  IF (IELx <= 0 .OR. IELx >= NGDBGN) CODE = TYPEE
               END IF
               
               IDUM(X) = CODE
            END DO x_lcode_loop
            
            ! Modernized: Explicit array slice for IDUM
            CALL ALCHKI (EEERR, 1058, PPPRI, 1, NX, Y, IUNDEF, NAME, 'EQ', &
                         IZERO1, IDUM(1:NX), NERR, LDUM1(1:NX))
         END DO y_lcode_loop
         
      END DO xy_loop

   ! 3. Finish
   ! ---------
      IF (NERR > 0) THEN
         CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking static OC input arrays')
      END IF

   END SUBROUTINE OCCHK1



   !SSSSSS SUBROUTINE OCCHK2
   SUBROUTINE OCCHK2 (DDUM1A, DDUM1B, SZLOG, LDUM1)
   !----------------------------------------------------------------------*
   !
   !  Check OC input data
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCCHK2/4.2
   ! Modifications:
   ! RAH  980203  4.2  New: partly from part of OCPLF.
   !      980206       Check unit numbers.
   !      980218       Don't check units if NONEED.
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  NEL.ge.[NLF,1]    NLFEE.ge.[NLF,1]    PRI open for F output
   !----------------------------------------------------------------------*

      USE CONST_SY ! Assuming this provides global variables like total_no_links, etc.

      IMPLICIT NONE

      INTEGER, INTENT(IN)           :: SZLOG
      DOUBLE PRECISION, INTENT(OUT) :: DDUM1A (:), DDUM1B(:)
      
      ! Changed from INTENT(IN) to INTENT(INOUT) to allow ALCHK/ALCHKI to write to it
      LOGICAL, INTENT(INOUT)        :: LDUM1(SZLOG)  !LDUM1 ( * )
      
      INTEGER :: ERRNUM, I, IELw, IUNDEF, IUNIT, N, NERR
      INTEGER :: IDUMS (1)
      LOGICAL :: BOPEN, NONEED
      CHARACTER (47) :: MSG
      CHARACTER(11)  :: FORM
      CHARACTER(3)   :: NAME
      CHARACTER(19)  :: SUBJ

      ! Assumed external module dependencies providing global variables:
      ! OHB, OFB, NOCHB, NOCFB, EEERR, PPPRI, IZERO1, ZERO1, ZERO, STRXX, STRYY
      ! total_no_elements, total_no_links, XINH, XINW, NXSECT, WWWARN

      !----------------------------------------------------------------------*
      
      NERR = 0
      IUNDEF = 0

      ! 1. Unit Numbers
      ! ---------------
      ! OHB, OFB
      IDUMS (1) = 0
      IUNIT = OHB
      NAME = 'OHB'
      NONEED = NOCHB == 0

      DO I = 0, 1
         IF (.NOT. NONEED) THEN
            IDUMS (1) = MIN (IUNIT, IDUMS (1))
            INQUIRE (IUNIT, OPENED = BOPEN, FORM = FORM)
            
            IF (.NOT. BOPEN) THEN
               WRITE (MSG, 9100) NAME, IUNIT, 'is not connected to a file'
               ERRNUM = 1008
               CALL ERROR(EEERR, ERRNUM, PPPRI, 0, 0, MSG)
               NERR = NERR + 1
            ELSE IF (FORM /= 'FORMATTED') THEN
               WRITE (MSG, 9100) NAME, IUNIT, 'has format type', FORM
               ERRNUM = 1009
               CALL ERROR(EEERR, ERRNUM, PPPRI, 0, 0, MSG)
               NERR = NERR + 1
            END IF
         END IF

         ! Setup for OFB on the second pass
         IUNIT = OFB
         NAME = 'OFB'
         NONEED = NOCFB == 0
      END DO
      
      CALL ALCHKI (EEERR, 1003, PPPRI, 1, 1, IUNDEF, IUNDEF, '[ OHB, OFB ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)

      ! 2. Element Properties
      ! ---------------------
      ! STRX
      CALL ALCHK(EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRX(iel)', 'GT', ZERO1, ZERO, STRXX, NERR, LDUM1)
      ! STRY
      CALL ALCHK (EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRY(iel)', 'GT', ZERO1, ZERO, STRYY, NERR, LDUM1)


      ! 3. Cross-section Tables
      ! -----------------------
      !
      IF (total_no_links > 0) THEN
         ! XINH
         CALL ALCHK (EEERR, 1016, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINH(link)[j=1]', 'EQ', ZERO1, ZERO, XINH, NERR, LDUM1)
         
         DO IELw = 1, total_no_links
            N = NXSECT (IELw) - 1
            WRITE (SUBJ, 9310) IELw
            
            DDUM1A(1:N) = XINH(IELw, 1:N)
            DDUM1B(1:N) = XINH(IELw, 2:N+1)
            CALL ALCHK (EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, 'GTa', DDUM1A, ZERO, DDUM1B, NERR, LDUM1)

            ! XINW
            SUBJ (4:4) = 'W'
            DDUM1A(1:N) = XINW(IELw, 1:N)
            DDUM1B(1:N) = XINW(IELw, 2:N+1)
            CALL ALCHK (EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, 'GEa', DDUM1A, ZERO, DDUM1B, NERR, LDUM1)
         END DO
         
         DO IELw = 1, total_no_links
            DDUM1A (IELw) = XINW (IELw, NXSECT (IELw))
         END DO
         
         CALL ALCHK (EEERR, 1056, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINW[link,NXSECT(link)]', 'GT', ZERO1, ZERO, &
                     DDUM1A, NERR, LDUM1)
      END IF

      IF (NERR > 0) THEN
         !!!! sb 190522 negative strickler for surface storage
         CALL ERROR(WWWARN, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
         ! CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
      END IF

      ! Format Statements
      ! -----------------
9100  FORMAT ('File unit ', A, ' =', I4, 1X, A: 1X, A)
9310  FORMAT ('XINH[ link =', I3, '](j)')

   END SUBROUTINE OCCHK2



   !SSSSSS SUBROUTINE OCEXT
   SUBROUTINE OCEXT
   !----------------------------------------------------------------------*
   ! READ IN TIME-VARYING BOUNDARY CONDITION DATA
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! NOCHB, OHB, TIH, OCNOW, OCNEXT, HOCLST, HOCNXT, HOCPRV, HOCNXV, HOCNOW
      ! NOCFB, OFB, QFLAST, QFNEXT, QOCFIN, QOCF
      ! FFFATAL, PPPRI

      IMPLICIT NONE

   !----------------------------------------------------------------------*

   ! --- HEAD BOUNDARY ---
      IF (NOCHB > 0) THEN
         CALL HINPUT (OHB, TIH, OCNOW, OCNEXT, HOCLST, HOCNXT, &
                      HOCPRV(1:NOCHB), HOCNXV(1:NOCHB), NOCHB, HOCNOW(1:NOCHB))
      END IF

      IF (EQMARKER(HOCNXT)) THEN
         CALL ERROR(FFFATAL, 1007, PPPRI, 0, 0, 'END OF OC HEAD BOUNDARY DATA')
      END IF

   ! --- FLUX BOUNDARY ---
      IF (NOCFB > 0) THEN
         CALL FINPUT (OFB, TIH, OCNOW, OCNEXT, QFLAST, QFNEXT, &
                      QOCFIN(1:NOCFB), NOCFB, QOCF(1:NOCFB))
      END IF

      IF (EQMARKER(QFNEXT)) THEN
         CALL ERROR(FFFATAL, 1023, PPPRI, 0, 0, 'END OF OC FLUX BOUNDARY DATA')
      END IF

   END SUBROUTINE OCEXT



   !SSSSSS SUBROUTINE OCIND
   SUBROUTINE OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)
   !----------------------------------------------------------------------*
   !
   ! SET UP INDEXING SYSTEM FOR THOMAS ALGORITHM
   !
   !   THE CATCHMENT IS SPLIT INTO A NUMBER OF ROWS, EACH INCLUDING ALL
   !   THE ELEMENTS WITH THE SAME Y CO-ORDINATE (AS IN ICMXY(X,Y))
   !   NOTE: E-W BANKS/LINKS ARE INCLUDED WITH THE ROW ABOVE.
   !
   !   NROWF         - NO. OF FIRST (LOWEST) ROW
   !   NROWL         - NO. OF LAST (HIGHEST) ROW
   !   NROWST(J)     - POINTER TO POSITION IN NROWEL OF START OF ROW J
   !   NROWEL(1:NEL) - Contiguous list of elements in row order: thus,
   !                   the number of the Ith element in row J is
   !                       IEL = NROWEL(K) where K = NROWST(J)+I-1
   !   NELIND(IEL)   - INDEX NO. (POSITION IN ROW) OF ELEMENT NO. IEL
   !
   !   NB. NELIND IS THE PARTIAL INVERSE OF NROWEL:
   !                                  NELIND(NROWEL(NROWST(J)+I-1)) = I
   !   NB. Row no. of element ICMXY(x,y) (also of any associated link/bank
   !       elements) is y
   !
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! NY, NX, LINKNO, ICMBK, ICMXY, NXOCEE, FFFATAL, PPPRI

      IMPLICIT NONE

      ! Arguments
      LOGICAL, INTENT(IN)  :: BEXBK
      INTEGER, INTENT(OUT) :: NROWF, NROWL, NROWST(NY + 1), NELIND(:), NROWEL(:)
      
      ! Locals
      INTEGER :: BANK, FACE, I, ICOUNT, IELv, J, K, LINK, NXOC

   !----------------------------------------------------------------------*

   ! Initialize counters
      NXOC = 0
      K = 0

   ! LOOP OVER BASIC GRID SYSTEM
   ! - LOOP OVER EACH ROW

      row_loop: DO J = 1, NY
         NROWST(J) = K + 1
         IF (K == 0) NROWF = J

   ! ---- LOOP OVER EACH GRID SQUARE IN ROW
         ICOUNT = 0
         
         col_loop: DO I = 1, NX
            
   ! ------- Loop over west & south faces
            face_loop: DO FACE = 3, 4

   ! ---------- Test for link at face of grid
               LINK = LINKNO(I, J, FACE == 3)
               
               IF (LINK > 0) THEN
                  IF (BEXBK) THEN
                     BANK = ICMBK(LINK, 5 - FACE)
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = BANK
                     NELIND(BANK) = ICOUNT
                  END IF
                  
                  K = K + 1
                  ICOUNT = ICOUNT + 1
                  NROWEL(K) = LINK
                  NELIND(LINK) = ICOUNT
                  
                  IF (BEXBK) THEN
                     BANK = ICMBK(LINK, FACE - 2)
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = BANK
                     NELIND(BANK) = ICOUNT
                  END IF
               END IF

   ! ---------- Test for active grid square
               IF (FACE == 3) THEN
                  IELv = ICMXY(I, J)
                  IF (IELv > 0) THEN
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = IELv
                     NELIND(IELv) = ICOUNT
                  END IF
               END IF

            END DO face_loop
         END DO col_loop

   ! ---- Next row
         NXOC = MAX(NXOC, K + 1 - NROWST(J))
         IF (ICOUNT > 0) NROWL = J

      END DO row_loop

   ! - This marks the end of the last row (+1)
   ! Modern Fix: Explicitly use NY + 1 instead of relying on the leaked loop variable 'J'
      NROWST(NY + 1) = K + 1

   ! CHECK ARRAY DIMENSIONS
      IF (NXOC > NXOCEE) THEN
         CALL ERROR(FFFATAL, 1006, PPPRI, 0, 0, 'ARRAY DIMENSION OF NXOC TOO SMALL')
      END IF

   END SUBROUTINE OCIND


   
   ! 12/8/94
   !SSSSSS SUBROUTINE OCLTL
   SUBROUTINE OCLTL (NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)
!
! READ IN ARRAY OF ALPHANUMERIC CODES FOR CHANNEL DEFINITION
!
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
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      IMPLICIT NONE ! Always good practice in modern Fortran

      ! Dummy Arguments
      INTEGER, INTENT(IN)  :: NNX, NNY, NXE, NYE, INF, IOF
      INTEGER, INTENT(OUT) :: IARR(NXE, NYE)
      LOGICAL, INTENT(IN)  :: BPCNTL

      ! Local Variables
      CHARACTER(LEN=80)    :: TITLE
      CHARACTER(LEN=1)     :: A1LINE(500)
      INTEGER              :: I, J, K, L, M

      ! 1. Replaced DATA with modern PARAMETER array initialization
      CHARACTER(LEN=1), PARAMETER :: CODES(11) = &
         ['I', '.', ' ', ' ', ' ', 'R', 'W', 'A', 'H', 'F', 'P']

! Code =================================================================

      ! 2. Inline format strings eliminate the need for numbered FORMAT labels
      READ (INF, '(A80)') TITLE
      IF (BPCNTL) WRITE (IOF, '(A80)') TITLE

      ! 3. Whole-array slicing replaces the DO 30 loops
      IARR(1:NNX, 1:NNY) = 0

      I = NNY

      read_loop: DO J = 1, NNY
         ! 4. Replaced implied DO loop with array slicing: A1LINE(1:NNX)
         READ (INF, '(I7, 1X, 500A1)') K, A1LINE(1:NNX)
         IF (BPCNTL) WRITE (IOF, '(I7, 1X, 500A1)') K, A1LINE(1:NNX)

         ! 5. Handled the error immediately instead of using the iscycle40 flag
         IF (K /= I) THEN
            IF (BPCNTL) WRITE (IOF, "('  ^^^   INCORRECT COORDINATE')")
            STOP 'INCORRECT COORDINATE'
         END IF

         I = I - 1

         line_loop: DO L = 1, NNX
            search_code: DO M = 1, 11
               IF (A1LINE(L) == CODES(M) .AND. CODES(M) /= ' ') THEN
                  IARR(L, K) = M
                  ! 6. Replaced the "iscycle" flag with a clean EXIT
                  EXIT search_code
               END IF
            END DO search_code
         END DO line_loop

      END DO read_loop

   END SUBROUTINE OCLTL



!SSSSSS SUBROUTINE OCPLF
   SUBROUTINE OCPLF(BOUT, IXER, fromNOCBCD, NXDEF, XDEFW)
   !----------------------------------------------------------------------*
   !
   !  READ IN DATA FOR EACH CHANNEL LINK
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCPLF/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   !  GP  940808  4.0  Remove redundant array EXBETA (SPEC.AL).
   ! RAH  980121  4.2  Fix error in COCBCD 2nd subscript (see BR/50).
   !                   Explicit typing.
   !                   List-directed input, & scrap local TITRE.  No FLOAT.
   !                   Bring NDEFCT,NXDEF,XDEFH,XDEFW from SPEC.OC.
   !                   Trap large NDEFCT.  Scrap EARRAY.  New local TEST.
   !      980127       Use locals MSG,N more.  New local ZG.
   !                   Test STR.le.0, not .eq.0.  Require IEL in order.
   !                   Combine IDEFX tests.  Use IDEF: don't redfine IDEFX.
   !                   Split expression for XAREA.  Use HJ for DPNOW.
   !                   Rearrange loop 200.  Rename INDEX,ICODE as IBC,TYPE.
   !                   Read NOCBCD,COCBCD directly, & set NOCBCD(IBC,2&4).
   !      980128       Don't set XCONV or XDERIV for J=NXSCEE.  Trap N=1.
   !                   Replace FATAL errors with ERR, & increase IXER.
   !                   Require XDEFH,XINH strictly increasing, and
   !                   XDEFW,XINW .gt.0 for J=N.
   !      980129       Use IEL for loop variable ICT.  Trap NDEFCT<0.
   !      980203       Move cross-section table set-up to OCXS (new).
   !                   Move STRX,STRY,XINH,XINW checks to OCCHK2 (new).
   !      980204       Full argument list (no INCLUDEs) (see OCREAD);
   !                   ERR local.  Add argument BOUT: write to PRI if true.
   !      980206       Call DCOPY for XINH,XINW.  More info in MSG.
   !      980218       Adjust formats.
   !      980220       Adjust formats.
   !      980225       Swap COCBCD subscripts (see SPEC.OC)
   ! JE   JAN 2009     Loop restructure for AD
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  NLFEE.ge.NLF    [NLF,NOCTAB].ge.1    NOCBCC(1:NLF).le.NOCTAB
   !  OCD open for F input                 PRI open for F output
   !----------------------------------------------------------------------*
   ! Exit conditions:
   ! IXER(out).ge.IXER(in)
   ! IXER(out).eq.IXER(in) ==> 2.le.NXSECT(1:NLF).le.NOCTAB
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! NOCTAB, XDEFH, OCD, EEERR, PPPRI, total_no_links, ZGRUND, SETHRF, STRXX
      ! STRYY, XINW, XINH, NXSECT, CWIDTH, ZBFULL, NOCBCC, COCBCD, ERROR

      IMPLICIT NONE

      LOGICAL, INTENT(INOUT) :: BOUT
      INTEGER, INTENT(INOUT) :: IXER
      INTEGER, INTENT(INOUT) :: fromNOCBCD(NOCTAB, 2:4)
      INTEGER, INTENT(OUT)   :: NXDEF (NOCTAB)
      DOUBLE PRECISION       :: XDEFH (NOCTAB, NOCTAB), XDEFW (NOCTAB, NOCTAB)
      
      INTEGER :: I, IBC, IDEF, IDEFX, ielm, J, N, NDEFCT, TYPEE, ios
      DOUBLE PRECISION :: STR, WDEPTH, ZG
      LOGICAL :: TEST, g8055, g8013, g8300, greturn
      CHARACTER(102) :: MSG

      !----------------------------------------------------------------------*
      !
      ! READ DEFAULT CHANNEL CROSS-SECTIONS
      ! :OC30
      
      READ (OCD, *)
      READ (OCD, *) NDEFCT
      
      IF ((NDEFCT > NOCTAB) .OR. (NDEFCT < 0)) THEN 
         WRITE (MSG, 9054) NDEFCT, NOCTAB
         CALL ERROR(EEERR, 1054, PPPRI, 0, 0, MSG)
         IXER = IXER + 1
      END IF

      g8013 = .FALSE.
      g8055 = .FALSE.
      g8300 = .FALSE.
      greturn = .FALSE.

      ! :OC32
      IF (NDEFCT > 0) THEN
         READ (OCD, *)
         IF (BOUT) WRITE(PPPRI, 9032) 'Category', 'Width', 'Height'
         
         out100: DO IDEF = 1, NDEFCT
            IF (g8055) CYCLE out100
            READ (OCD, *) N
            
            IF ((N > NOCTAB) .OR. (N < 2)) THEN
               g8055 = .TRUE.
               CYCLE out100
            END IF
            
            NXDEF (IDEF) = N
            READ (OCD, *) (XDEFW (IDEF, J), XDEFH (IDEF, J), J = 1, N)
            
            IF (BOUT) WRITE(PPPRI, 9034) IDEF, (XDEFW (IDEF, J), XDEFH (IDEF, J), J = 1, N)
         END DO out100
      END IF

      !
      ! READ DATA FOR EACH LINK
      ! :OC35
      IF (g8055) THEN
         WRITE (MSG, 9055) IDEF, N, NOCTAB
         CALL ERROR(EEERR, 1055, PPPRI, 0, 0, MSG)
         IXER = IXER + 1
      ELSE
         READ (OCD, *)
         IF (BOUT) WRITE(PPPRI, 9035) 'Element', 'Elevation', 'Init.Depth', 'Strickler', 'Width', 'Height'
         
         out500: DO ielm = 1, total_no_links
            IF (g8013 .OR. g8300 .OR. greturn) CYCLE out500
            
            ! Modernized with IOSTAT check
            READ (OCD, *, IOSTAT=ios) I, ZG, WDEPTH, STR, IDEFX

            IF (ios /= 0) THEN
               g8300 = .TRUE.
               CYCLE out500
            END IF

            IF (I /= ielm) THEN
               g8013 = .TRUE.
               CYCLE out500
            END IF

            ZGRUND (ielm) = ZG
            CALL SETHRF(ielm, ZG + WDEPTH)
            STRXX(ielm) = STR
            STRYY(ielm) = STR
            
            ! :OC37
            TEST = (IDEFX == 1) .OR. (IDEFX > NOCTAB)
            
            IF ((IDEFX == 0) .OR. (IDEFX < -NDEFCT) .OR. TEST) THEN
               WRITE (MSG, 9012) IDEFX, -NDEFCT, NOCTAB
               CALL ERROR(EEERR, 1012, PPPRI, ielm, 0, MSG)
               IXER = IXER + 1
               
               IF (TEST) THEN
                  greturn = .TRUE.
                  CYCLE out500
               END IF
               
            ELSE
               IF (IDEFX > 0) THEN
                  N = IDEFX
                  READ (OCD, *) (XINW (ielm, J), XINH (ielm, J), J = 1, N)
                  IF (BOUT) WRITE(PPPRI, 9037) ielm, ZG, WDEPTH, STR, (XINW (ielm, J), XINH (ielm, J), J = 1, N)
               ELSE
                  IDEF = -IDEFX
                  N = NXDEF (IDEF)
                  ! Native Fortran array slice copying N elements
                  XINH(ielm, 1:N) = XDEFH(IDEF, 1:N)
                  XINW(ielm, 1:N) = XDEFW(IDEF, 1:N)
                  IF (BOUT) WRITE(PPPRI, 9137) ielm, ZG, WDEPTH, STR, IDEF
               END IF

               NXSECT (ielm) = N
               
               ! CHANNEL BANK-FULL WIDTH & ELEVATION
               CWIDTH (ielm) = XINW (ielm, N)
               ZBFULL (ielm) = XINH (ielm, N) + ZG
            END IF

            ! READ IN ADDITIONAL DATA FOR BOUNDARY CONDITIONS
            ! :OC38-41
            IBC = NOCBCC (ielm)
            
            IF (IBC > 0) THEN
               TYPEE = fromNOCBCD (IBC, 3)
               
               IF ((TYPEE == 7) .OR. (TYPEE == 8)) THEN
                  READ (OCD, *) fromNOCBCD (IBC, 2), (COCBCD (J, IBC), J = 1, 4)
                  fromNOCBCD (IBC, 4) = 1
               ELSE IF (TYPEE == 9) THEN
                  fromNOCBCD (IBC, 2) = 0
                  READ (OCD, *) fromNOCBCD (IBC, 4)
               ELSE IF (TYPEE == 10) THEN
                  READ (OCD, *) (fromNOCBCD (IBC, J), J = 2, 4, 2)
               ELSE IF (TYPEE == 11) THEN
                  READ (OCD, *) fromNOCBCD (IBC, 2), (COCBCD (J, IBC), J = 1, 5)
                  fromNOCBCD (IBC, 4) = 1
               END IF
            END IF
            
         END DO out500
      END IF

      ! Epilogue Error Catching
      IF (greturn) THEN
         RETURN
      ELSE IF (g8013) THEN
         WRITE (MSG, 9013) ielm, I
         CALL ERROR(EEERR, 1013, PPPRI, ielm, 0, MSG)
         IXER = IXER + 1
      ELSE IF (g8300) THEN
         MSG = 'Channel input data is missing or has incorrect format'
         CALL ERROR(EEERR, 1019, PPPRI, ielm, 0, MSG)
         IXER = IXER + 1
      END IF

      ! Format Statements
9012  FORMAT ('Cross-section number IDEFX =', I4, ' lies outside ranges', &
               ' -NDEFCT:-1 =', I4, ' : -1  and  2:NOCTAB = 2 :', I4)

9013  FORMAT ('Expected element number,', I5, ', but found', I5, ', ', &
               'while reading channel data')

9032  FORMAT (/5X, 'Default Channel Cross-sections:'//5X, 3A10/)

9034  FORMAT (5X, I10, (T16, 2F10.3))

9035  FORMAT (/5X, 'Link Element Data:'//5X, 6A11/)

9037  FORMAT (5X, I11, 3F11.3, (T50, 2F11.3))

9054  FORMAT ('Number of default channel cross-section categories ', &
               'NDEFCT =', I4, 2X, 'lies outside range 0:NOCTAB = 0 :', I4)

9055  FORMAT ('Number of width/elevation pairs NXDEF(', I3, ') =', I4, 2X, &
               'lies outside range 2:NOCTAB = 2:', I4)

9137  FORMAT (5X, I11, 3F11.3, 3X, 'default category', I3)

   END SUBROUTINE OCPLF



!SSSSSS SUBROUTINE OCPRI
   SUBROUTINE OCPRI (OCNOW, ARXL, QOC)
!----------------------------------------------------------------------*
!
!  Print results of OC simulation
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCPRI/4.2
! Modifications:
! RAH  980226  4.2  New.
!----------------------------------------------------------------------*
! Entry requirements:
!  NELEE.ge.NEL    NEL.ge.[1,NLF]    NLF.ge.0    NLF.le.size_of_ARXL
!  PRI.ge.0        PRI open for F output
!----------------------------------------------------------------------*
      DOUBLEPRECISION, INTENT(IN) :: OCNOW, ARXL(:), QOC(NELEE, 4)
      DOUBLEPRECISION             :: ghrf(total_no_links)
      INTEGER                     :: FACE, ielmm
!----------------------------------------------------------------------*
      WRITE(PPPRI, 9100) 'AFTER', OCNOW, ' HOURS ----'
      WRITE(PPPRI, 9200) 'iel', ('QOC(iel,', FACE, ')', FACE = 1, 4) , 'HRF', 'ARXL'
      DO ielmm=1,total_no_links
         ghrf(ielmm) = GETHRF(ielmm)
      ENDDO
      WRITE(PPPRI, 9210) (ielmm, (QOC(ielmm,FACE), FACE=1,4), ghrf(ielmm), ARXL (ielmm), ielmm = 1, total_no_links)
      DO ielmm = total_no_links + 1,total_no_elements
         WRITE(PPPRI, 9210) ielmm, (QOC (ielmm, FACE), FACE = 1, 4), GETHRF (ielmm)
      END DO

      WRITE(PPPRI, 9100) 'END ----'
9100  FORMAT (//'---- OC MODULE  RESULTS ',A:F10.2,A//)
9200  FORMAT (4X,A4,4(2X,A8,I1,A1),2A12/)

9210  FORMAT (4X,I4,SP,4F12.3,S,2F12.3)
   END SUBROUTINE OCPRI



   !SSSSSS SUBROUTINE OCREAD
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
   ! SvB  260402       Ran through G:Gemini for mondernization
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  NELEE.ge.[NEL,NOCTAB*NOCTAB]    NEL.gt.NLF    NLF.ge.0    NOCTAB.ge.1
   !  NLFEE.ge.NLF    OCD open for F input      PRI open for F output
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCREAD/4.2

      ! Assumed external module dependencies providing global variables:
      ! NOCTAB, IDUM, total_no_elements, total_no_links, NGDBGN, OCD, PPPRI,
      ! one, ISZERO, ZERO, DUMMY, ZGRUND, SETHRF, STRXX, STRYY, NOTZERO,
      ! nelee, JEOCBC, OCPLF, NOCBCD, COCBCD, FFFATAL, ERROR, AREADR, AREADI

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(OUT)          :: KONT
      DOUBLE PRECISION, INTENT(OUT) :: TDC, TFC
      DOUBLE PRECISION, INTENT(OUT) :: CATR(NOCTAB), DDUM2(NOCTAB, NOCTAB)
      
      ! Locals
      INTEGER          :: I, IBC, ICAT, ielt, IXER, KKON, TYPEE
      INTEGER          :: NCATR, NLAND, NOCBC, NT
      DOUBLE PRECISION :: DET, SMIN, CDRS
      LOGICAL          :: BIOWAT, BOUT
      CHARACTER(81)    :: MSG

      INTEGER, PARAMETER :: NC(11) = [0, 0, 0, 0, 5, 0, 4, 4, 0, 0, 5]
      CHARACTER(11), PARAMETER :: CTYPE(11) = ['impermeable', '  grid-grid', '       head', ' flux      ', &
         ' polynomial', ' river_link', '       weir', ' river+weir', '       head', '       flux', ' polynomial']

      !----------------------------------------------------------------------*
      !              Initialization
      !
      IXER = 0
      NLAND = total_no_elements - total_no_links
      NGDBGN = total_no_links + 1

      !              Integer & logical variables
      ! :OC1
      READ (OCD, *)
      READ (OCD, *) NT, NCATR, KONT, BIOWAT

      KKON = MOD(KONT, 2)
      BOUT = (KKON == 1)

      IF (BOUT) WRITE(PPPRI, 9080) ' ', NCATR

      !              OC time-step data
      ! :OC2
      READ (OCD, *)
      READ (OCD, *)

      !              Default roughness parameters & floating-point variables
      ! :OC3
      READ (OCD, *)
      READ (OCD, *) SMIN, CDRS, TDC, TFC, DET

      IF (KONT < 2) TDC = TFC + one

      ! :OC4
      IF (ISZERO(CDRS)) THEN
         IF (NCATR > NOCTAB .OR. NCATR < 0) THEN
            WRITE (MSG, '("Number of roughness categories NCATR =",I4,2X, &
                         "lies outside range 0:NOCTAB = 0 :",I4)') NCATR, NOCTAB
            CALL ERROR(FFFATAL, 1047, PPPRI, 0, 0, MSG)
         END IF

         IF (NCATR > 0) THEN
            ! PERF FIX: Implied DO loop instead of array slice
            READ (OCD, *) (CATR(I), I = 1, NCATR)
            IF (BOUT) THEN
               WRITE(PPPRI, 9084) (CATR(I), I = 1, NCATR)
               WRITE(PPPRI, *)
            END IF
         END IF
      ELSE IF (BOUT) THEN
         WRITE(PPPRI, 9082) CDRS
      END IF

      !              INITIAL OVERLAND FLOW ELEVATIONS
      ! :OC5
      IF (BIOWAT) THEN
         CALL AREADR(DUMMY, KKON, OCD, PPPRI)
      ELSE
         ! PERF FIX: Explicit DO loop instead of array slice assignment
         DO ielt = NGDBGN, total_no_elements
            DUMMY(ielt) = ZERO
         END DO
         IF (BOUT) WRITE(PPPRI, 9085) 'zero'
      END IF

      elevation_loop: DO ielt = NGDBGN, total_no_elements
         CALL SETHRF(ielt, ZGRUND(ielt) + DUMMY(ielt))
      END DO elevation_loop

      !              ROUGHNESS PARAMETERS FOR OVERLAND FLOW
      ! :OC14
      ! :OC17
      IF (NOTZERO(CDRS)) THEN
         ! PERF FIX: Explicit DO loops instead of array slice assignment
         DO ielt = NGDBGN, total_no_elements
            STRXX(ielt) = CDRS
            STRYY(ielt) = CDRS
         END DO
      ELSE IF (NCATR == 0) THEN
         CALL AREADR(STRXX, KKON, OCD, PPPRI)
         CALL AREADR(STRYY, KKON, OCD, PPPRI)
      ELSE
         ! Pass base memory address IDUM
         CALL AREADI(IDUM, KKON, OCD, PPPRI, NCATR)

         roughness_x_loop: DO ielt = NGDBGN, total_no_elements
            ICAT = MAX(1, MIN(IDUM(ielt), NCATR))
            STRXX(ielt) = CATR(ICAT)
         END DO roughness_x_loop

         CALL AREADI(IDUM, KKON, OCD, PPPRI, NCATR)

         roughness_y_loop: DO ielt = NGDBGN, total_no_elements
            ICAT = MAX(1, MIN(IDUM(ielt), NCATR))
            STRYY(ielt) = CATR(ICAT)
         END DO roughness_y_loop
      END IF

      !              BOUNDARY CONDITIONS
      CALL JEOCBC(IXER, NOCBC)

      !              PARAMETERS OF RIVER LINKS
      IF (total_no_links > 0 .AND. IXER == 0) THEN
         ! PERF FIX: Pass base memory address NOCBCD(1, 2) instead of 2D slice
         CALL OCPLF(BOUT, IXER, NOCBCD(1, 2), IDUM, DDUM2)
      END IF

      !              FINISH
      REWIND(OCD)

      IF (IXER /= 0) THEN
         WRITE (MSG, 9412) IXER
         CALL ERROR(FFFATAL, 1049, PPPRI, 0, 0, MSG)
      ELSE IF (BOUT) THEN
         WRITE(PPPRI, 9500) 'no-flow'
         IF (NOCBC > 0) WRITE(PPPRI, 9600) 'Index', 'Element', 'Face', &
            'Type', 'Category', 'Coefficients'

         print_bc_loop: DO IBC = 1, NOCBC
            TYPEE = NOCBCD(IBC, 3)

            ! PERF FIX: Explicit indexing and Implied DO loop instead of slices
            WRITE(PPPRI, 9610) IBC, NOCBCD(IBC, 1), NOCBCD(IBC, 2), CTYPE(TYPEE), &
               NOCBCD(IBC, 4), (COCBCD(I, IBC), I = 1, NC(TYPEE))
         END DO print_bc_loop

         WRITE(PPPRI, 9080) ' END OF '
      END IF

      RETURN

      ! FORMAT STATEMENTS
9080  FORMAT (///'---- OC MODULE ',A,'INPUT DATA PROCESSING ----'///: &
              5X,'NUMBER OF DIFFERENT OVERLAND FLOW ROUGHNESS', &
                 ' CATEGORIES   NCATR = ',I4)

9082  FORMAT (/5X,'DEFAULT VALUE OF OVERLAND FLOW ROUGHNESS ', &
                  'COEFFICIENT     CDRS = ', F8.2)

9084  FORMAT (/4X,' ROUGHNESS COEFFICIENTS  CATR  ATTACHED TO', &
                  ' EACH OF THE NCATR CATEGORIES' / (10F10.2))

9085  FORMAT (/5X,'Initial overland water depth is ',A)

9412  FORMAT (I5,' ERROR(S) FOUND DURING OC INPUT DATA PROCESSING')

9500  FORMAT (/5X,'Default OC B.C. is ',A,' at catchment boundaries ', &
                  'and at channel/bank dead-ends')

9600  FORMAT (/5X,'OC Boundary Conditions:'//5X,3A8,A12,A10,A14)

9610  FORMAT (5X,3I8,A12,I10,1P,5G14.6)
   END SUBROUTINE OCREAD



   !SSSSSS SUBROUTINE OCSIM
   SUBROUTINE OCSIM
   !----------------------------------------------------------------------*
   !
   !  MAIN OVERLAND/CHANNEL SIMULATION ROUTINE
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCSIM/4.2
   ! Modifications:
   !  GP         3.4  Set HRF at head boundaries.  Disallow flow against
   !                  surface gradient (except boundaries & confluences),
   !                  and move trap for small depths & setting of ARXL to
   !                  after this point; also, replace WLMIN with 1D-5.
   !                  Trap large flows (ABS(QOC)>QMAX) in channels.
   ! RAH  941008 3.4.1 Bring IMPLICIT from SPEC.AL.  Move traps (except
   !                  QMAX) to new subroutine OCFIX (confluences too).
   ! RAH  961228  4.1  Remove variables TF & LONT.
   ! RAH  971215  4.2  Explicit typing.  Bring DD,EE,GG from SPEC.OC.
   !                  Remove DOCEV,DLIOC,DETOCO,DOCUZO,DOCSZO.
   !                  Set NCR,NPR,NSV (& NDUM) only where necessary.
   !                  Merge OCQDQ loops.  Don't call OCMAS.
   !      971216      Move first row initialization nearer to OCABC.
   !                  Call OCQDQ always (not only 1st step) at the start,
   !                  and not at the end of a step.  Scrap tests NLF.GT.0.
   !                  Use IROW as subscript not NROWF; also use IRSV.
   !                  Cut duplicate code in main loop over rows.
   !      971217      Merge EE,GG code for first row into loop over rows.
   !                  Initialize only useful elements of AA,BB,CC,FF.
   !                  Separate treatment for DD of last row.
   !      980106      Call ERROR if ICOD=1 after PMINVM.
   !                  Next IROW if NCR=0 in main loop.  Scrap JFACE.
   !                  Renumber labels 4,190,200 as 44,250,255.
   !                  Amend head boundary implementation, & move to OCABC.
   !                  New locals DDI,DH,DQ,DW,HI,HM,IBR,IM,WI,WM,Z.
   !                  Use DCOPY & CHSGN to set QOC.  Use ABS not DABS.
   !      980107      OCABC arguments: remove ICOUNT;
   !                  add IEL,NXSECT,ZBFULL,ZGRUND,HRF,AA,BB,CC,FF.
   !                  Bring AA,BB,CC,FF from SPEC.OC.
   !      980108      OCABC arguments: add NSV,NCR,NPR,PNETTO,QH,ESWA.
   !                  Move initialization of AA,BB,CC,FF to OCABC.
   !      980109      OCABC arguments: add IBC,HOCNOW.
   !      980212      Scrap NROWFN - use NROWST(IROW+1)-1.
   !      980226      Call OCPRI.  Bring DTOC from SPEC.OC; pass to OCABC.
   !                  Call OCQDQ once only, and pass COCBCD,QOCF.
   !      980327      Pass XAFULL (new local) to OCQDQ.  Scrap local K.
   !                  Pass OCTIME, not OCNOW, to OCPRI, & call before QMAX
   !                  test, not after.
   !      980331      Pass STRX,STRY to OCQDQ.
   !      980409      Pass HOCNOW to OCQDQ.
   !      980424      Pass NXSCEE,XSTAB to OCQDQ.
   !      980427      Pass final local FWRK to OCQDQ.
   ! JE   JAN 2009    Loop restructure for AD
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables
      IMPLICIT NONE

      INTEGER :: I, IELs, IND, IROW, IBC, IBR, ICOD, IFACE, IHB, IM, IRSV
      INTEGER :: J, JEL, JND, JROW, K0, LINK, N, NCR, NPR, NSV, face
      INTEGER :: kk, ll, vv

      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ijedum, ijedum2
      
      DOUBLE PRECISION :: DDI, DH, DQ, DW, H, HI, HM, OCTIME, WI, WM, Z
      
      DOUBLE PRECISION, DIMENSION(:,:),   ALLOCATABLE :: AA, DD, BB, GG, CC, TM1, TM2, inqsa, GGGETQSA
      DOUBLE PRECISION, DIMENSION(:),     ALLOCATABLE :: FF, TV1, TV2, inhrf, GGGETHRF
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: EE

      LOGICAL :: first = .TRUE., found_level, channel_blowup
      CHARACTER(36) :: MSG

      !----------------------------------------------------------------------*
      !
      ! ----- Initialize
      ! needs to be nelee and nlfee here as it reads from arrays that are still set to these sizes
      ALLOCATE (ijedum(nelee, 4, 2:3), ijedum2(nlfee, 3, 2))
      ijedum  = 0
      ijedum2 = 0

      ALLOCATE (AA(NX*4, NX*4), DD(NX*4, NY))
      ALLOCATE (FF(NX*4))
      ALLOCATE (BB(NX*4, NX*4), GG(NX*4, NY))
      ALLOCATE (CC(NX*4, NX*4))
      ALLOCATE (EE(NX*4, NX*4, NY))
      ALLOCATE (TM1(NX*4, NX*4), TM2(NX*4, NX*4))
      ALLOCATE (TV1(NX*4), TV2(NX*4))
      
      AA = 0.0D0; DD = 0.0D0; FF = 0.0D0; BB = 0.0D0; GG = 0.0D0
      CC = 0.0D0; EE = 0.0D0; TM1 = 0.0D0; TM2 = 0.0D0; TV1 = 0.0D0; TV2 = 0.0D0

      ALLOCATE (inhrf(total_no_elements))
      ALLOCATE (GGGETHRF(total_no_elements))
      ALLOCATE (inqsa(total_no_elements, 4))
      ALLOCATE (GGGETQSA(total_no_elements, 4))
      
      inhrf = 0.0D0; GGGETHRF = 0.0D0; inqsa = 0.0D0; GGGETQSA = 0.0D0

      !
      DTOC = OCNEXT * 3600.0D0
      
      IF (first) THEN
         first = .FALSE.
         DO LINK = 1, total_no_links
            XAFULL (LINK) = XAREA (LINK, NXSECT (LINK))
         END DO
      END IF

      ! ----- GET PRESCRIBED BOUNDARY VALUES HOCNOW & QOCF
      CALL OCEXT

      ! ----- CALCULATE FLOWS QSA & DERIVATIVES DQ0ST,DQIST,DQIST2
      CALL OCQDQ ()

      

      ! ----- LOOP OVER ROWS, CALCULATING EE & GG
      NCR = 0
      
      row_loop: DO IROW = NROWF, NROWL
         IRSV = IROW + 1
         !
         ! NCR : NUMBER OF ELEMENTS IN THE CURRENT ROW
         ! NPR : NUMBER OF ELEMENTS IN THE PREVIOUS ROW
         ! NSV : NUMBER OF ELEMENTS IN THE NEXT (SUIVANT) ROW
         !
         NPR = NCR
         K0 = NROWST (IROW) - 1
         NCR = NROWST (IRSV) - 1 - K0
         
         IF (NCR == 0) CYCLE row_loop
         
         NSV = NROWST (MIN (IRSV, NROWL) + 1) - NROWST (IRSV)
         
         ! CALCULATE MATRICES AA, BB, CC, FF
         DO IND = 1, NCR
            iels = NROWEL (IND + K0)
            LINK = MAX (1, MIN (iels, total_no_links))
            IBC = NOCBCC (iels)
            
            IF (IBC > 0) THEN
               IHB = NOCBCD (IBC, 4)
               IBC = NOCBCD (IBC, 3)
            ELSE
               IHB = 1
            END IF
            
            CALL OCABC (IND, IROW, iels, NSV, NCR, NPR, IBC, NXSECT (LINK), cellarea (iels), &
                        ZGRUND (iels), CLENTH (LINK), ZBFULL (LINK), GETHRF (iels),          &
                        PNETTO (iels), QH (iels), ESWA (iels), HOCNOW (IHB), AA(:,IND),      &
                        BB (1:ncr,IND), CC(:,IND), FF (IND))
         END DO
         
         ! CALCULATE MATRIX TM2 (inverse of CC.EE+BB) AND VECTOR TV2 (FF-CC.GG)
         IF (IROW == NROWF) THEN
            DO IND = 1, NCR
               TM2(1:ncr, IND) = BB(1:ncr, IND)
            END DO
            TV2(1:ncr) = FF(1:ncr)
         ELSE
            tm1(1:ncr, 1:ncr) = JEMATMUL_MM(cc(1:npr, 1:ncr), ee(1:ncr, 1:npr, irow), ncr, npr, ncr)
            tm2(1:ncr, 1:ncr) = bb(1:ncr, 1:ncr) + tm1(1:ncr, 1:ncr)
            tv1(1:ncr)        = JEMATMUL_VM(cc(1:npr, 1:ncr), gg(1:npr, irow), ncr, npr)
            TV2(1:ncr)        = FF(1:ncr) - TV1(1:ncr)
         END IF
         
         CALL INVERTMAT(TM2(1:ncr, 1:ncr), NCR, ICOD)
         
         ! Catch singular matrix inversion failure
         IF (ICOD == 1) THEN
            WRITE (MSG, '(A,I4)') 'Singular matrix at row', IROW
            CALL ERROR(FFFATAL, 1018, PPPRI, NROWEL(NROWST(IROW)), 0, MSG)
            RETURN
         END IF
         
         ! CALCULATE MATRIX EE(IROW+1)
         IF (IROW /= NROWL) THEN
            ee(1:nsv, 1:ncr, irsv) = JEMATMUL_MM(tm2(1:ncr, 1:ncr), aa(1:nsv, 1:ncr), ncr, ncr, nsv)
            ee(1:nsv, 1:ncr, irsv) = -ee(1:nsv, 1:ncr, irsv)
         END IF
         
         ! CALCULATE VECTOR GG(IROW+1)
         gg(1:ncr, irsv) = JEMATMUL_VM(tm2(1:ncr, 1:ncr), tv2(1:ncr), ncr, ncr)
         
      END DO row_loop


      ! ----- DOWNWARDS SWEEP, CALCULATION OF DD
      !
      ! * last row first (use NCR,IRSV from loop above)
      IROW = NROWL
      DD(1:ncr, IROW) = GG(1:ncr, IRSV)
      
      ! * loop over remaining rows
      DO IROW = NROWL - 1, NROWF, -1
         IRSV = IROW + 1
         NSV = NCR
         NCR = NROWST (IRSV) - NROWST (IROW)
         
         tv1(1:ncr) = JEMATMUL_VM(ee(1:nsv, 1:ncr, irsv), dd(1:nsv, irsv), ncr, nsv)
         dd(1:ncr, irow) = tv1(1:ncr) + gg(1:ncr, irsv)
      END DO

      ! ----- ADVANCE WATER LEVELS AND FLOWS TO TIME LEVEL N+1,
      !       USING FIRST ORDER DERIVATIVES OF FLOWS AT TIME LEVEL N
      DO iels = 1, total_no_elements
         IND = NELIND (iels)
         IROW = ICMREF (iels, 3)
         DDI = DD (IND, IROW)
         CALL SETHRF(iels, GETHRF (iels) + DDI)
         
         DO IFACE = 1, 4
            DQ = DQ0ST (iels, IFACE) * DDI
            JEL = ICMREF (iels, IFACE + 4)
            
            IF (JEL > 0) THEN
               JND = NELIND (JEL)
               JROW = ICMREF (JEL, 3)
               DQ = DQIST (iels, IFACE) * DD (JND, JROW) + DQ
               
            ELSE IF (JEL < 0) THEN
               IBR = -JEL
               DO J = 1, 3
                  JEL = ICMRF2 (IBR, J)
                  IF (JEL > 0) THEN
                     JND = NELIND (JEL)
                     JROW = ICMREF (JEL, 3)
                     DQ = DQIST2 (IBR, J) * DD (JND, JROW) + DQ
                  END IF
               END DO
            END IF
            
            CALL SETQSA(iels, IFACE, GETQSA(iels, IFACE) + DQ)
         END DO
      END DO

      ! CHECK FOR SPURIOUS NEGATIVE FLOWS, AND RECALCULATE WATER LEVELS
      ! IF REQUIRED.  NB. DOES NOT CHECK BOUNDARY FLOWS
      vv = 5
      DO LL = 2, 3
         DO kk = 1, 4
            ijedum(:, kk, LL) = icmref(:, vv)
            vv = vv + 1
         END DO
      END DO
      
      vv = 1
      DO LL = 1, 2
         DO kk = 1, 3
            ijedum2(:, kk, LL) = icmrf2(:, vv)
            vv = vv + 1
         END DO
      END DO

      ! untidy mess for debugging of tangent
      DO vv = 1, total_no_elements
         inhrf(vv) = GETHRF(vv)
         DO face = 1, 4
            inqsa(vv, face) = GETQSA(vv, face)
         END DO
      END DO

      CALL OCFIX(ijedum, ijedum2, total_no_elements, dtoc, inhrf, GGGETHRF, inqsa, GGGETQSA)

      DO vv = 1, total_no_elements
         CALL SETHRF(vv, GGGETHRF(vv))
         DO face = 1, 4
            CALL SETQSA(vv, face, GGGETQSA(vv, face))
         END DO
      END DO

      ! SET FLOWS QOC (POSITIVE X,Y) FOR USE BY OTHER COMPONENTS
      QOC(1:total_no_elements, :) = GETQSA_ALL(total_no_elements)
      qoc(1:total_no_elements, 1:2) = -qoc(1:total_no_elements, 1:2)

      ! ----- CALCULATE CROSS-SECTIONAL AREA OF CHANNEL WATER
      link_loop: DO iels = 1, total_no_links
         Z = GETHRF (iels)
         H = Z - ZGRUND (iels)
         N = NXSECT (iels)
         found_level = .FALSE.
         
         sect_loop: DO I = 2, N
            HI = XINH (iels, I)
            IF (H < HI) THEN
               IM = I - 1
               HM = XINH (iels, IM)
               WM = XINW (iels, IM)
               WI = XINW (iels, I)
               DH = H - HM
               DW = (WI - WM) * (DH / (HI - HM))
               ARXL (iels) = XAREA (iels, IM) + (WM + 0.5D0 * DW) * DH
               found_level = .TRUE.
               EXIT sect_loop
            END IF
         END DO sect_loop
         
         IF (.NOT. found_level) THEN
            ARXL (iels) = XAREA (iels, N) + (Z - ZBFULL (iels)) * CWIDTH (iels)
         END IF
      END DO link_loop

      ! ----- Print results
      OCTIME = OCNOW + OCNEXT
      IF ((OCTIME >= TDC) .AND. (OCTIME <= TFC)) CALL OCPRI (OCTIME, ARXL, QOC)

      ! ----- CHECK FOR CHANNEL BLOW-UP
      channel_blowup = .FALSE.
      IF (GTZERO(QMAX)) THEN
         blowup_loop: DO iels = 1, total_no_links
            DO IFACE = 1, 4
               IF (ABS (QOC (iels, IFACE)) > QMAX) THEN
                  channel_blowup = .TRUE.
                  EXIT blowup_loop
               END IF
            END DO
         END DO blowup_loop
      END IF
      
      IF (channel_blowup) THEN
         MSG = 'CHANNEL FLOWS EXCEED MAXIMUM ALLOWED'
         CALL ERROR(FFFATAL, 1029, PPPRI, iels, 0, MSG)
      END IF

   END SUBROUTINE OCSIM



   !SSSSSS SUBROUTINE OCXS
   SUBROUTINE OCXS ()
   !----------------------------------------------------------------------*
   !
   !  Set up channel cross-section tables & effective bed elevations
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCXS/4.2
   ! Modifications:
   ! RAH  980203  4.2  New: taken from part of OCPLF.
   !      980317      ! Fix inaccuracy in XAJ (was linear in H); set XDERIV
   !                   to give continuous XCONV.  Use XAJ in loop 180.
   !      980424       Merge XSECTH,XCONV,XDERIV into XSTAB (see SPEC.OC).
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !  NLFEE.ge.NLF    NLF.ge.1    NXSCEE.ge.2    CWIDTH(1:NLF).gt.0
   !  NXSECT(1:NLF).le.size_of_[XINH,XINW,XAREA]
   !  for iel in 1:NLF                XINH(iel,NXSECT(iel)).gt.0
   !      for i in 1:NXSECT(iel)-1    XINH(iel,i).lt.XINH(iel,i+1)
   ! Exit conditions:
   !  ...
   ! Note:
   !       XSTAB(i,j,iel) is, for i=1,2,3: water depth, conveyance, and
   !                                       derivative of conveyance, respectively
   ! NB:
   !       XSTAB(2:3,j,1:NLF) not defined for j=NXSCEE
   !
      ! Assumed external module dependencies providing global variables:
      ! total_no_links, NXSECT, STRXX, XAREA, zero, XINW, XINH, half,
      ! ZBEFF, ZBFULL, CWIDTH, NXSCEE, XSTAB, CONVEYAN

      IMPLICIT NONE

      INTEGER         :: I, IELr, J, N
      DOUBLE PRECISION :: ALPHA, DH, HI, HIP1, HJ, STEPH, STR, W2, XAJ, XCJ, XCJM1, adumy
      
   !----------------------------------------------------------------------*

      link_loop: DO ielr = 1, total_no_links
         !
         ! LOCAL VARIABLES
         !
         N = NXSECT(ielr)
         STR = STRXX(ielr)
         
         !
         ! SET UP CROSS-SECTIONAL AREAS FOR EACH OF THE INPUT LEVELS
         !
         XAREA(ielr, 1) = zero
         
         area_loop: DO J = 2, N
            W2 = XINW(ielr, J) + XINW(ielr, J - 1)
            DH = XINH(ielr, J) - XINH(ielr, J - 1)
            XAREA(ielr, J) = XAREA(ielr, J - 1) + W2 * DH * half
         END DO area_loop
         
         !
         ! EFFECTIVE BED ELEVATION
         !
         ZBEFF(ielr) = ZBFULL(ielr) - XAREA(ielr, N) / CWIDTH(ielr)
         
         !
         ! SET UP FULL CROSS-SECTION TABLES OF HEIGHT, CONVEYANCE & DERIVATIVE
         !
         ! NOTE: The formulation is such that
         !             XSTAB(2,j,ielr) + XSTAB(3,j,ielr)*( h - XSTAB(1,j,ielr) )
         !       is a continuous (piecewise linear) function of h
         !
         I = 1
         HI = XINH(ielr, I)
         STEPH = XINH(ielr, N) / (NXSCEE - 1.0d0)
         XCJ = zero
         XSTAB(1, 1, ielr) = zero
         
         table_loop: DO J = 2, NXSCEE
            XCJM1 = XCJ
            HJ = STEPH * (J - 1)

            ! Advance index I until we bracket the target height HJ
            search_loop: DO
               HIP1 = XINH(ielr, I + 1)
               IF (I >= N - 1 .OR. HIP1 >= HJ) EXIT search_loop
               I = I + 1
               HI = HIP1
            END DO search_loop

            DH = HJ - HI
            ALPHA = DH / (HIP1 - HI)
            W2 = (2.0d0 - ALPHA) * XINW(ielr, I) + ALPHA * XINW(ielr, I + 1)
            XAJ = XAREA(ielr, I) + W2 * DH * half

            ! XCJ = STR * XAJ * HJ**F23
            CALL CONVEYAN(str, hj, xcj, adumy, 0, xaj)
            
            XSTAB(1, J, ielr) = HJ
            XSTAB(2, J - 1, ielr) = XCJM1
            XSTAB(3, J - 1, ielr) = (XCJ - XCJM1) / STEPH
         END DO table_loop

      END DO link_loop

   END SUBROUTINE OCXS



   !FFFFFF INTEGER FUNCTION LINKNO
   PURE INTEGER FUNCTION LINKNO (I, J, NSOUTH)
   !----------------------------------------------------------------------*
   ! GET LINK NUMBER, GIVEN X, Y CO-ORDINATES AND ORIENTATION
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! total_no_links, LINKNS, ICMREF

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: I, J
      LOGICAL, INTENT(IN) :: NSOUTH

      ! Locals
      INTEGER :: L

   !----------------------------------------------------------------------*

      LINKNO = 0
      
      IF (total_no_links == 0) RETURN

      ! High-Performance Fix: Replaced 'iscycle' AD-hack with a direct EXIT
      ! to immediately terminate the loop once the correct link is found.
      search_loop: DO L = 1, total_no_links
         
         ! Integer comparison first for fast short-circuiting
         IF (ICMREF(L, 2) == I .AND. ICMREF(L, 3) == J) THEN
            
            ! Logical equivalence check
            IF (NSOUTH .EQV. LINKNS(L)) THEN
               LINKNO = L
               EXIT search_loop
            END IF
            
         END IF
         
      END DO search_loop

   END FUNCTION LINKNO

END MODULE OCmod
