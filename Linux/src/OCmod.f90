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
USE mod_load_filedata ,    ONLY : ALCHK, ALCHKI, ALINIT
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
!
!  Control OC initialization
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCINI/4.2
! Modifications:
!  GP          3.4  Don't set OCNOW,OCVAL,OCNEXT (see also FRINIT,SHE).
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
! Commons and constants
! Imported constants
!     SPEC.AL          NELEE,NLFEE,NXEE,NOCTAB,NXOCEE
!     SPEC.OC          NXSCEE
! Input common
!     SPEC.AL          NEL,NGDBGN,NLF,NX,NY,OCD,OHB,OFB,PRI
!                      LCODEX(NXEE,NY),ICMREF(NELEE,12),ICMBK(NLFEE,2)
!                      LCODEY(NXEE,NY),ICMXY(NXEE,NY),NBFACE(NGDBGN:NEL)
!                      BEXBK
! In|out common
!     SPEC.AL          ZGRUND(NEL)
!                Note: Input ZGRUND(NLF+1:NEL); output ZGRUND(1:NLF)
! Output common
!     SPEC.AL          NOCBCC(NEL),NOCBCD(NOCTAB,4)
!                         HRF(NEL),CWIDTH(NLFEE),ZBEFF(NLFEE)
!                                  ZBFULL(NLFEE)
!     SPEC.OC          NELIND(NEL),NXSECT(NLFEE),NROWF,NOCHB
!                      NROWEL(NEL),NROWST(NY+1), NROWL,NOCFB
!                        XINH(NLFEE,NOCTAB),STRX(NELEE),TDC,TFC
!                        XINW(NLFEE,NOCTAB),STRY(NELEE),COCBCD(5,NOCTAB)
!                       XAREA(NLFEE,NOCTAB),XSTAB(3,NXSCEE,NLFEE)
! Workspace common
!     SPEC.AL           IDUM(NELEE)
!                      DUMMY(NELEE)
! Locals, etc
!INTRINSIC MOD  
INTEGER :: KONT
DOUBLEPRECISION DDUM1 (NOCTAB), DDUM2 (NOCTAB, NOCTAB)  
LOGICAL :: LDUM1 (NELEE)  
!
CALL OCCHK0()
!CALL OCCHK1 (ICMREF (1, 5), &
!CALL OCCHK1(ICMREF (1:4, 5), SIZE(ldum1), LDUM1) !AD aliasing
CALL OCCHK1(SIZE(ldum1), LDUM1) !AD aliasing
!
! Input data & associated requirements
!
CALL OCREAD(KONT, TDC, TFC, DDUM1, DDUM2)
CALL OCCHK2 (DUMMY, DDUM1, nelee, LDUM1)
!
! Boundary data files
!
!     Read title lines
IF (NOCHB.GT.0) READ (OHB, * )  
IF (NOCFB.GT.0) READ (OFB, * )  


CALL INITIALISE_OCMOD()

!
! Cross-section tables & effective bed elevations
!
IF (total_no_links.GT.0) THEN  
   IF (MOD (KONT, 2) .EQ.1) WRITE(PPPRI, 9100) NXSCEE  
   CALL OCXS()
ENDIF  
!
! Indicies for Thomas algorithm
!
CALL OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)
 9100 FORMAT (/5X,'Size of internal tables for channel conveyance, etc', '  NXSCEE =',I6)
END SUBROUTINE OCINI


!SSSSSS SUBROUTINE OCABC
SUBROUTINE OCABC(IND, IROW, ielz, NSV, NCR, NPR, IBC, N, AREAE, &
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
! Commons and constants
! Imported constants
!     SPEC.AL          NELEE,NLFEE
! Input common
!     SPEC.AL          ICMREF(NELEE,12),ICMRF2(NLFEE,3)
!                       DQ0ST(NELEE,4), DQIST2(NLFEE,3),QBKB(NLFEE,2)
!                       DQIST(NELEE,4),    QSA(NELEE,4),QBKF(NLFEE,2)
!     SPEC.OC          NELIND(NELEE)
!                        XINH(NLFEE,*),   XINW(NLFEE,*)
INTEGER, INTENT(IN)         :: IND, IROW, IELz, NSV, NCR, NPR, IBC, N  
DOUBLEPRECISION, INTENT(IN) :: AREAE, ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW
DOUBLEPRECISION, INTENT(OUT) :: AA(NXOCEE), BB (NCR), CC (NXOCEE), FF  
INTEGER :: I, IBR, IFACE, IM  
INTEGER :: J, JEL, JFACE, JND, JROW  
DOUBLEPRECISION AR, BKDUM, DQ0, DQI, H, HI, HM, PDUM, Q, QHDUM, WI, WM
LOGICAL :: BLINK, TEST, iscycle
!
! ----- INITIALIZE OUTPUT ARRAYS & GET WATER DEPTH
!
IF (NSV.GT.0) CALL ALINIT (ZERO, NSV, AA)  
CALL ALINIT (ZERO, NCR, BB)  
IF (NPR.GT.0) CALL ALINIT (ZERO, NPR, CC)  
H = Z - ZG  
!
! ----- HEAD BOUNDARY
!
IF ((IBC.EQ.3).OR.(IBC.EQ.9)) THEN  
   BB (IND) = one  
   FF = HNOW - H  
   RETURN  
ENDIF  
!
! ----- IS THE CURRENT ELEMENT A LINK?
!
BLINK = ICMREF (ielz, 1) .EQ.3  
!
! ----- PUT STORAGE TERM INTO CENTRAL COEFFICIENT FOR CURRENT ELEMENT
!
TEST = BLINK  
IF (TEST) TEST = Z.LT.ZBF  
IF (TEST) THEN  
    !         * note requirements: XINH(IEL,1)=0; XINH(IEL,N).GE.ZBF-ZG
    iscycle=.FALSE.
    DO I = 2, N
        IF(iscycle) CYCLE  
        HI = XINH (ielz, I)  
        IF (H.LT.HI) THEN  
            IM = I - 1  
            HM = XINH (ielz, IM)  
            WM = XINW (ielz, IM)  
            WI = XINW (ielz, I)  
            AR = CL * (WM + (WI - WM) * ( (H - HM) / (HI - HM) ) )  
            iscycle = .TRUE. !GOTO 20
            CYCLE  
        ENDIF  
    ENDDO  
ELSE  
    AR = AREAE  
ENDIF  
BB (IND) = - AR / DTOC  
!
! ----- PUT PRECIPITATION, EVAPORATION AND EXCHANGE FLOWS INTO RHS
!
PDUM = PNETT  
IF (BLINK) THEN  
   IF (H.LT.1D-8) PDUM = ZERO  
   BKDUM = QBKB (ielz, 1) + QBKF (ielz, 1) + QBKB (ielz, 2) + QBKF ( &
    ielz, 2)
   QHDUM = ZERO  
ELSE  
   BKDUM = ZERO  
   QHDUM = QHE  
ENDIF  
FF = - AREAE * (PDUM + QHDUM - ESWAE) + BKDUM  
!
! ----- LOOP OVER ADJACENT ELEMENTS
!
DO 500 IFACE = 1, 4  
   JEL = ICMREF (ielz, IFACE+4)  
   JFACE = ICMREF (ielz, IFACE+8)  
!
! --- GET FLOW AND DERIVATIVE (+VE INTO ELEMENT)
!
   Q = GETQSA (ielz, IFACE)  
   DQ0 = DQ0ST (ielz, IFACE)  
!
! --- ADD INTO COEFFICIENTS FOR CURRENT ELEMENT
!
   BB (IND) = BB (IND) + DQ0  
   FF = FF - Q  
!
! --- TEST FOR SINGLE ADJACENT ELEMENT
!
   IF (JEL.GT.0) THEN  
!
      JROW = ICMREF (JEL, 3)  
      JND = NELIND (JEL)  
      DQI = DQIST (ielz, IFACE)  
!
!        ADD DERIVATIVE TO COEFFICIENT FOR ADJACENT ELEMENT
!
      IF (JROW.EQ.IROW) BB (JND) = BB (JND) + DQI  
      IF (JROW.GT.IROW) AA (JND) = AA (JND) + DQI  
      IF (JROW.LT.IROW) CC (JND) = CC (JND) + DQI  
!
! --- SIMILARLY FOR MULTIPLE ADJACENT LINKS
!
   ELSEIF (JEL.LT.0) THEN  
!
      IBR = - JEL  
      DO 200 J = 1, 3  
         JEL = ICMRF2 (IBR, J)  
         IF (JEL.GT.0) THEN  
            JROW = ICMREF (JEL, 3)  
            JND = NELIND (JEL)  
            DQI = DQIST2 (IBR, J)  
            IF (JROW.EQ.IROW) BB (JND) = BB (JND) + DQI  
            IF (JROW.GT.IROW) AA (JND) = AA (JND) + DQI  
            IF (JROW.LT.IROW) CC (JND) = CC (JND) + DQI  
         ENDIF  
  200       END DO  
!
   ENDIF  
  500 END DO  
END SUBROUTINE OCABC

!SSSSSS SUBROUTINE OCBC
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
! Entry requirements:
!  NELEE.ge.NEL    NXEE.ge.[ NX, 1 ]    [ NY, NGDBGN, NOCTAB ].ge.1
!  [ NGDBGN, ICMXY(1:NX,1:NY), ICMREF(1:NEL,5:8) ].le.NEL
!  for x in 1:NX : for y in 1:NY :
!      7.le.LCODEX(x,y).le.11 ==> 0.lt.LINKNO(x,y,T).lt.NGDBGN
!      7.le.LCODEY(x,y).le.11 ==> 0.lt.LINKNO(x,y,F).lt.NGDBGN
!  OCD open for F input    PRI open for F output
!----------------------------------------------------------------------*
! Exit conditions:
!  IXER[out].ge.IXER[in]     0.le.NOCBCC(1:NEL).le.NOCTAB
!  IXER[out].eq.IXER[in] ==> 1.le.NOCBCD(1:NOCBC,1).le.NEL
!                            1.le.NOCBCD(1:NOCBC,3).le.11
!                                 NOCBC.le.NOCTAB
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
READ (OCD, * ) NOCHB,NOCFB,NOCPB  
!
! INITIALIZATION
!
NOCBC = 0  
DO 10 iely = 1,total_no_elements  
   NOCBCC (iely) = 0  
   10 END DO  
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
   40    END DO  
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
   60    END DO  
!:OC28
   MSG = 'Error reading polynomial function data in OC'  
   READ (OCD, * )  
   DO 80 I = 1, NOCPB  
      READ (OCD, * ) ICAT, ADUM  
      IF (ICAT.NE.I) THEN  
         IXER = IXER + 1  
         CALL ERROR(EEERR, 1031, PPPRI, iely, 0, MSG)  
      ELSE  
         DO 70 IBC = IBC0 + 1, MIN (NOCBC, NOCTAB)  
            TEST = NOCBCD (IBC, 4) .EQ.I  
            !IF (TEST) CALL DCOPY(5, ADUM, 1, COCBCD(1:5,IBC), 1)
            IF (TEST) COCBCD(1:5,IBC) = adum
   70          END DO  
      ENDIF  
   80    END DO  
ENDIF  
!
! SET CHANNEL LINK BOUNDARY TYPES (other data will follow)
!
DO 100 I = 1, NX  
   DO 100 J = 1, NY  
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
   90       END DO  
  100 CONTINUE  
!
! SET INTERNAL IMPERMEABLE GRID BOUNDARY CONDITIONS (TYPE 1)
!
! NB Impermeability extended across ends of any adjacent bank elements
!
IBC0 = NOCBC  
DO 110 I = 1, NX  
   DO 110 J = 1, NY  
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
  104                END DO  
            ENDIF  
         ENDIF  
  107       END DO  
  110 CONTINUE  
DO 120 IBC = IBC0 + 1, MIN (NOCBC, NOCTAB)  
   NOCBCD (IBC, 3) = 1  
   NOCBCD (IBC, 4) = 1  
  120 END DO  
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
 1000 END DO  
!
! FINISH
!
 9050 FORMAT ('Number of OC boundary conditions NOCBC =',I4,2X, &
&        'exceeds array size NOCTAB =',I4)

 9100 FORMAT ('Element has multiple OC boundary conditions ', &
&        '(types',I2,' and',I2,')')
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
!JE Jan 2009        Above restriction removed
!----------------------------------------------------------------------*
INTEGER       :: ERRNUM, I, IUNDEF, IUNIT, NERR, OUNIT  
INTEGER       :: IDUMS (1), IDUMO (1)  
LOGICAL       :: BOPEN, LDUM1 (1)  
CHARACTER(47) :: MSG
CHARACTER(11) :: FORM
CHARACTER(3)  :: NAME
nerr = 0
!----------------------------------------------------------------------*
! 1. Unit Numbers
! ---------------
!PRI, OCD
OUNIT = PPPRI  
IUNIT = PPPRI  
NAME = 'PRI'  
DO  I = 0, 1  
   INQUIRE (IUNIT, OPENED = BOPEN, FORM = FORM)  
   IF (.NOT.BOPEN) THEN  
      WRITE (MSG, 9100) NAME, IUNIT, 'is not connected to a file'  
      ERRNUM = 1008  
   ELSEIF (FORM.NE.'FORMATTED') THEN  
      WRITE (MSG, 9100) NAME, IUNIT, 'has format type', FORM  
      ERRNUM = 1009  
   ELSE  
      GOTO 110  
   ENDIF  
   IF (I.EQ.0) OUNIT = 0  
   CALL ERROR(EEERR, ERRNUM, OUNIT, 0, 0, MSG)  
   NERR = NERR + 1  
  110    IUNIT = OCD  
   NAME = 'OCD'  
ENDDO  
IDUMS (1) = MIN (PPPRI, OCD)  

CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ PRI, OCD ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)
! 2. Array Sizes
! --------------
!NELEE
IDUMS (1) = NELEE  
IDUMO (1) = MAX (NX, total_no_elements)! , NOCTAB*NOCTAB)  
CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NLFEE
IDUMS (1) = NLFEE  
IDUMO (1) = MAX (1, total_no_links)  
CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NXEE
IDUMS (1) = NXEE  
IDUMO (1) = NX  
CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NOCTAB
IDUMS (1) = NOCTAB  
CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NOCTAB', 'GE', IONE1, IDUMS, NERR, LDUM1)
!NXSCEE
IDUMS (1) = NXSCEE  

CALL ALCHKI (EEERR, 1002, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXSCEE', 'GT', IONE1, IDUMS, NERR, LDUM1)
! 3. Number of Entities
! ---------------------
!NLF
IDUMS (1) = total_no_links 
CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO1, IDUMS, NERR, LDUM1)
IDUMO (1) = total_no_elements  
CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)
!NX, NY
IDUMS (1) = MIN (NX, NY)  
CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ NX, NY ]', 'GE', IONE1, IDUMS, NERR, LDUM1)
!NGDBGN
IDUMS (1) = NGDBGN  
IDUMO (1) = total_no_links + 1  


CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NGDBGN', 'EQ', IDUMO, IDUMS, NERR, LDUM1)
! 4. Finish
! ---------
IF (NERR.GT.0) THEN
    CALL ERROR(FFFATAL, 1000, OUNIT, 0, 0, 'Error(s) detected while checking OC input variables & constants')
ENDIF
 9100 FORMAT ('File unit ',A,' =',I4,1X,A:1X,A)  
END SUBROUTINE OCCHK0


!SSSSSS SUBROUTINE OCCHK1
!SUBROUTINE OCCHK1(afromICMREF, SZLOG, LDUM1)
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
INTEGER, INTENT(IN) :: szlog
!INTEGER, INTENT(inout) :: afromICMREF(4)  !AD aliasing
LOGICAL, INTENT(OUT):: LDUM1 (szlog)  !LDUM1 ( * )  
INTEGER             :: CODE, FACE, I, IELx, IUNDEF, NERR, TYPEE, X, Y  
INTEGER             :: IDUMO (1)
CHARACTER(23)       :: NAME
CHARACTER           :: XY (0:1)  
DATA NERR / 0 / , XY / 'X', 'Y' /  
!----------------------------------------------------------------------*
! 1. Index Arrays
! ---------------
IDUMO (1) = total_no_elements  
!ICMREF
DO 110 FACE = 1, 4  
CALL ALCHKI (EEERR, 1057, PPPRI, 1, total_no_elements, FACE, 2, 'ICMREF(iel,face,2)' &
!&, 'LE', IDUMO, ICMREF (1, FACE, 2) , NERR, LDUM1)
!&, 'LE', IDUMO, afromICMREF(FACE) , NERR, LDUM1(1:NEL))
&, 'LE', IDUMO, ICMREF(1:total_no_elements, 4+FACE) , NERR, LDUM1(1:total_no_elements))
  110 END DO  
!ICMXY
DO 120 Y = 1, NY  
   CALL ALCHKI (EEERR, 1057, PPPRI, 1, NX, Y, IUNDEF, 'ICMXY(x,y)', &
    'LE', IDUMO, ICMXY (1, Y) , NERR, LDUM1(1:NX))
  120 END DO  
! 2. Channel Definition Arrays
! ----------------------------
!LCODEX,LCODEY
NAME = 'validity_of_LCODE?(x,y)'  
DO 230 I = 0, 1  
   NAME (18:18) = XY (I)  
   DO 220 Y = 1, NY  
      DO 210 X = 1, NX  
         CODE = 0  
         TYPEE = LCODEX (X, Y) * (1 - I) + LCODEY (X, Y) * I  
         IF ((TYPEE.GE.7).AND.(TYPEE.LE.11)) THEN  
            ielx = LINKNO (X, Y, I.EQ.0)  
            IF (ielx.LE.0.OR.ielx.GE.NGDBGN) CODE = TYPEE  
         ENDIF  
         IDUM (X) = CODE  
  210       END DO  
      CALL ALCHKI (EEERR, 1058, PPPRI, 1, NX, Y, IUNDEF, NAME, 'EQ', &
       IZERO1, IDUM, NERR, LDUM1(1:NX))
  220    END DO  
  230 END DO  
! 3. Finish
! ---------

IF (NERR.GT.0) CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking static OC input arrays')
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
INTEGER, INTENT(IN)           :: szlog
DOUBLEPRECISION, INTENT(OUT) :: DDUM1A (:), DDUM1B(:)  
LOGICAL, INTENT(IN)          :: LDUM1(szlog)  !LDUM1 ( * )  
INTEGER                      :: ERRNUM, I, IELw, IUNDEF, IUNIT, N, NERR  
INTEGER                      :: IDUMS (1)
LOGICAL                      :: BOPEN, NONEED  
CHARACTER (47)               :: MSG
CHARACTER(11)                :: FORM
CHARACTER(3)                 :: NAME
CHARACTER(19)                :: SUBJ  
DATA NERR / 0 /
!----------------------------------------------------------------------*
! 1. Unit Numbers
! ---------------
!OHB,OFB
IDUMS (1) = 0  
IUNIT = OHB  
NAME = 'OHB'  
NONEED = NOCHB.EQ.0  

DO I = 0, 1  
   IF (NONEED) GOTO 110  

   IDUMS (1) = MIN (IUNIT, IDUMS (1) )  
   INQUIRE (IUNIT, OPENED = BOPEN, FORM = FORM)  
   IF (.NOT.BOPEN) THEN  
      WRITE (MSG, 9100) NAME, IUNIT, 'is not connected to a file'  
      ERRNUM = 1008  
   ELSEIF (FORM.NE.'FORMATTED') THEN  
      WRITE (MSG, 9100) NAME, IUNIT, 'has format type', FORM  
      ERRNUM = 1009  
   ELSE  
      GOTO 110  
   ENDIF  
   CALL ERROR(EEERR, ERRNUM, PPPRI, 0, 0, MSG)  

   NERR = NERR + 1  
  110    IUNIT = OFB  
   NAME = 'OFB'  

   NONEED = NOCFB.EQ.0  
END DO  
CALL ALCHKI (EEERR, 1003, PPPRI, 1, 1, IUNDEF, IUNDEF, '[ OHB, OFB ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)
! 2. Element Properties
! ---------------------
!STRX

CALL ALCHK(EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRX(iel)', 'GT', ZERO1, ZERO, STRXX, NERR, LDUM1)
!STRY
CALL ALCHK (EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRY(iel)', 'GT', zero1, zero, STRYY, NERR, LDUM1)
! 3. Cross-section Tables
! -----------------------
!
IF (total_no_links.GT.0) THEN  
!XINH
CALL ALCHK (EEERR, 1016, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINH(link)[j=1]', 'EQ', zero1, zero , XINH, NERR, LDUM1)
   DO 310 ielw = 1, total_no_links
      N = NXSECT (ielw) - 1  
      WRITE (SUBJ, 9310) ielw  
      !CALL DCOPY(N, XINH(IEL,1), NLFEE, DDUM1A(1:n), 1)  
      !CALL DCOPY(N, XINH(IEL, 2), NLFEE, DDUM1B(1:n), 1)
      DDUM1A(1:n) = XINH(IELw,1:n)
      DDUM1B(1:n) = XINH(ielw, 2:n+1)
      CALL ALCHK (EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, 'GTa', DDUM1A, zero , DDUM1B, NERR, LDUM1)
!XINW
      SUBJ (4:4) = 'W'  
      !CALL DCOPY(N, XINW(IEL,1), NLFEE, DDUM1A(1:n), 1)  
      !CALL DCOPY(N, XINW(IEL,2), NLFEE, DDUM1B(1:n), 1)
      DDUM1A(1:n) = XINW(ielw,1:n)
      DDUM1B(1:n) = XINW(ielw,2:n+1)
      CALL ALCHK (EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, &
       'GEa', DDUM1A, zero , DDUM1B, NERR, LDUM1)
  310    END DO  
   DO 320 ielw = 1, total_no_links
      DDUM1A (ielw) = XINW (ielw, NXSECT (ielw) )  
  320    END DO  
CALL ALCHK (EEERR, 1056, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINW[link,NXSECT(link)]', 'GT', zero1, zero , &
                   DDUM1A, NERR, LDUM1)
ENDIF  

IF (NERR.GT.0) then
!!!! sb 190522 negative strickler for surface storage    
    CALL ERROR(WWWARN, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
!    CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
ENDIF
 9100 FORMAT ('File unit ',A,' =',I4,1X,A:1X,A)  

 9310 FORMAT ('XINH[ link =',I3,'](j)')  
END SUBROUTINE OCCHK2


!SSSSSS SUBROUTINE OCEXT  
SUBROUTINE OCEXT  
!----------------------------------------------------------------------
!
! READ IN TIME-VARYING BOUNDARY CONDITION DATA
!----------------------------------------------------------------------
!
! HEAD BOUNDARY
!
IF (NOCHB.GT.0) CALL HINPUT (OHB, TIH, OCNOW, OCNEXT, HOCLST, HOCNXT, HOCPRV(1:nochb), HOCNXV(1:nochb), NOCHB, HOCNOW(1:nochb))
IF (EQMARKER(HOCNXT)) CALL ERROR(FFFATAL, 1007, PPPRI, 0, 0, 'END OF OC HEAD BOUNDARY DATA')
!
! FLUX BOUNDARY
!
IF (NOCFB.GT.0) CALL FINPUT (OFB, TIH, OCNOW, OCNEXT, QFLAST, QFNEXT, QOCFIN(1:nocfb), NOCFB, QOCF(1:nocfb))
IF (EQMARKER(QFNEXT)) CALL ERROR(FFFATAL, 1023, PPPRI, 0, 0, 'END OF OC FLUX BOUNDARY DATA')
!
RETURN  
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
!                                 NELIND(NROWEL(NROWST(J)+I-1)) = I
!   NB. Row no. of element ICMXY(x,y) (also of any associated link/bank
!       elements) is y
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCIND/4.2
! Modifications:
! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
! RAH  980210  4.2  Scrap SPEC.OC output NROWFN (see OCSIM).
!                   Restructure/clarify: 1 line each for NROWF,NROWL;
!                   2 lines for NROWST; loop 5 for N-S & E-W links;
!                   local LINK; MAX for NXOC.  Scrap EARRAY (SPEC.AL).
!                   Explicit typing.  Bring NXOC from SPEC.OC.
!                   Full argument list (no INCLUDEs).  Local FATAL.
!----------------------------------------------------------------------*
! Entry requirements:
!  NLFEE.ge.[nlf,1]    NXEE.ge.[NX,1]    NY.ge.1
!  LINKNO(1:NX,1:NY,{T,F}).le.nlf (defined extent of ICMBK)
!    1.le.ICMBK(1:nlf,1:2).le.nel (defined extent of NELIND,NROWEL)
!  {ICMXY(1:NX,1:NY).gt.0} + {link} + {bank} = {1:nel}
!      where {link} = {LINKNO(1:NX,1:NY,{T,F}).gt.0}
!      and   {bank} = {ICMBK({link},1:2)}  if BEXBK
!                     {empty}              otherwise
!----------------------------------------------------------------------*
LOGICAL, INTENT(IN)  :: BEXBK  
INTEGER, INTENT(OUT) :: NROWF, NROWL, NROWST (NY + 1), NELIND(:), NROWEL(:)
INTEGER              :: BANK, FACE, I, ICOUNT, IELv, J, K, LINK, NXOC  
!----------------------------------------------------------------------*
!
! Initialize counters
!
NXOC = 0  
K = 0  
!
! LOOP OVER BASIC GRID SYSTEM
!
! - LOOP OVER EACH ROW
!

DO 20 J = 1, NY  
   NROWST (J) = K + 1  
   IF (K.EQ.0) NROWF = J  
!
! ---- LOOP OVER EACH GRID SQUARE IN ROW
!
   ICOUNT = 0  
   DO 10 I = 1, NX  
!
! ------- Loop over west & south faces
!
      DO 5 FACE = 3, 4  
!
! ---------- Test for link at face of grid
!
         LINK = LINKNO (I, J, FACE.EQ.3)  
         IF (LINK.GT.0) THEN  
            IF (BEXBK) THEN  
               BANK = ICMBK (LINK, 5 - FACE)  
               K = K + 1  
               ICOUNT = ICOUNT + 1  
               NROWEL (K) = BANK  
               NELIND (BANK) = ICOUNT  
            ENDIF  
            K = K + 1  
            ICOUNT = ICOUNT + 1  
            NROWEL (K) = LINK  
            NELIND (LINK) = ICOUNT  
            IF (BEXBK) THEN  
               BANK = ICMBK (LINK, FACE-2)  
               K = K + 1  
               ICOUNT = ICOUNT + 1  
               NROWEL (K) = BANK  
               NELIND (BANK) = ICOUNT  
            ENDIF  
         ENDIF  
!
! ---------- Test for active grid square
!
         IF (FACE.EQ.3) THEN  
            ielv = ICMXY (I, J)  
            IF (ielv.GT.0) THEN  
               K = K + 1  
               ICOUNT = ICOUNT + 1  
               NROWEL (K) = ielv  
               NELIND (ielv) = ICOUNT  
            ENDIF  
         ENDIF  
!
! ---------- Next face
!
    5       END DO  
!
! ------- Next square
!
   10    END DO  
!
! ---- Next row
!
   NXOC = MAX (NXOC, K + 1 - NROWST (J) )  
   IF (ICOUNT.GT.0) NROWL = J  
!
   20 END DO  
!
! - This marks the end of the last row (+1)
!
NROWST (J) = K + 1  
!
! CHECK ARRAY DIMENSIONS
!
IF (NXOC.GT.NXOCEE) THEN  
CALL ERROR(FFFATAL, 1006, PPPRI, 0, 0, 'ARRAY DIMENSION OF NXOC TOO SMALL')
ENDIF  
!
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
CHARACTER (LEN=80)  :: TITLE  
CHARACTER (LEN=1)   :: CODES (11), A1LINE (500)
INTEGER, INTENT(IN) ::  NNX, NNY, NXE, NYE, INF, IOF
INTEGER             :: IARR (NXE, NYE), i, j, k, L, m
LOGICAL, INTENT(IN) :: BPCNTL
LOGICAL             :: iscycle, iscycle40
DATA CODES / 'I', '.', ' ', ' ', ' ', 'R', 'W', 'A', 'H', 'F', 'P' /
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
iscycle40 = .FALSE.
DO 40 J = 1, NNY  
    IF(iscycle40) CYCLE
    READ (INF, 50) K, (A1LINE (L), L = 1, NNX)  
    50 FORMAT   (I7, 1X, 500A1)  
    IF (BPCNTL) WRITE (IOF, 50) K, (A1LINE (L), L = 1, NNX)  
    !
    IF (K.NE.I) THEN !GOTO 100
        iscycle40=.TRUE.
        CYCLE  
    ENDIF
    I = I - 1  
    
    DO L = 1, NNX   !70
        iscycle=.FALSE.
        DO M = 1, 11
            IF(iscycle) CYCLE 
            !AD? IF (A1LINE (L) .EQ.CODES (M) .AND.CODES (M) .NE.' ') THEN
            IF ((A1LINE (L)==CODES(M)) .AND. (CODES(M)/=' ')) THEN
                IARR (L, K) = M  
                iscycle=.TRUE. !GOTO 70  
            ENDIF  
        ENDDO  
    ENDDO  !70  
    !
40 END DO
IF(.NOT. iscycle40) RETURN  
!
  100 IF (BPCNTL) WRITE (IOF, 110)  
  110 FORMAT ('  ^^^   INCORRECT COORDINATE')  
STOP  
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
!JE    JAN 2009     Loop restructure for AD
!----------------------------------------------------------------------*
! Entry requirements:
!  NLFEE.ge.NLF    [NLF,NOCTAB].ge.1    NOCBCC(1:NLF).le.NOCTAB
!  OCD open for F input                 PRI open for F output
!----------------------------------------------------------------------*
! Exit conditions:
! IXER(out).ge.IXER(in)
! IXER(out).eq.IXER(in) ==> 2.le.NXSECT(1:NLF).le.NOCTAB
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
READ (OCD, * )  
READ (OCD, * ) NDEFCT  
IF ((NDEFCT.GT.NOCTAB).OR.(NDEFCT.LT.0)) THEN !GOTO 8054  
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
    READ (OCD, * )  
    IF (BOUT) WRITE(PPPRI, 9032) 'Category', 'Width', 'Height'  
    out100 : DO IDEF = 1, NDEFCT 
        IF(g8055) CYCLE out100 
        READ (OCD, * ) N  
        IF ((N.GT.NOCTAB).OR.(N.LT.2)) THEN
            g8055=.TRUE.
            CYCLE out100
        ENDIF
        NXDEF (IDEF) = N  
        READ (OCD, * ) (XDEFW (IDEF, J), XDEFH (IDEF, J), J = 1, N)  
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
    READ (OCD, * )  
    IF (BOUT) WRITE(PPPRI, 9035) 'Element', 'Elevation', 'Init.Depth', 'Strickler', 'Width', 'Height'
    out500 : DO ielm = 1, total_no_links
        IF(g8013 .OR. g8300 .OR. greturn) CYCLE out500 
        READ (OCD, *, ERR = 8300, END = 8300) I, ZG, WDEPTH, STR, IDEFX
        
        GOTO 877
        8300 g8300=.TRUE.
        CYCLE out500
    
        877 IF (I.NE.ielm) THEN
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
            GOTO 300  
        ELSEIF (IDEFX.GT.0) THEN  
            N = IDEFX  
            READ (OCD, * ) (XINW (ielm, J), XINH (ielm, J), J = 1, N)  
            IF (BOUT) WRITE(PPPRI, 9037) ielm, ZG, WDEPTH, STR, (XINW ( &
            ielm, J), XINH (ielm, J), J = 1, N)
        ELSE  
            IDEF = - IDEFX  
            N = NXDEF (IDEF)  
            !CALL DCOPY(N, XDEFH(IDEF,1), NOCTAB, XINH(ielm,1), NLFEE)
            !CALL DCOPY(N, XDEFW(IDEF,1), NOCTAB, XINW(ielm,1), NLFEE)
            XINH(ielm,1:n) = XDEFH(IDEF,1:n)
            XINW(ielm,1:n) = XDEFW(IDEF,1:n)
            IF (BOUT) WRITE(PPPRI, 9137) ielm, ZG, WDEPTH, STR, IDEF  
        ENDIF  
    
        NXSECT (ielm) = N  
        !
        ! CHANNEL BANK-FULL WIDTH & ELEVATION
        !
        CWIDTH (ielm) = XINW (ielm, N)  
    
        ZBFULL (ielm) = XINH (ielm, N) + ZG  
        !
        ! READ IN ADDITIONAL DATA FOR BOUNDARY CONDITIONS
        !:OC38-41
        300    IBC = NOCBCC (ielm)  
        IF (IBC.GT.0) THEN  
            TYPEE = fromNOCBCD (IBC, 3)  
            IF((TYPEE.EQ.7).OR.(TYPEE.EQ.8)) THEN  
                READ (OCD, * ) fromNOCBCD (IBC, 2), (COCBCD (J, IBC), J = 1, 4)
                fromNOCBCD (IBC, 4) = 1  
            ELSEIF (TYPEE.EQ.9) THEN  
                fromNOCBCD (IBC, 2) = 0  
                READ (OCD, * ) fromNOCBCD (IBC, 4)  
            ELSEIF (TYPEE.EQ.10) THEN  
                READ (OCD, * ) (fromNOCBCD (IBC, J), J = 2, 4, 2)  
            ELSEIF (TYPEE.EQ.11) THEN  
                READ (OCD, * ) fromNOCBCD (IBC, 2), (COCBCD (J, IBC), J = 1, 5)
                fromNOCBCD (IBC, 4) = 1  
            ENDIF  
        ENDIF  
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
 9012 FORMAT ('Cross-section number IDEFX =',I4,' lies outside ranges', &
&        ' -NDEFCT:-1 =',I4,' : -1  and  2:NOCTAB = 2 :',I4)

 9013 FORMAT ('Expected element number,'I5,', but found',I5,', ', &
&        'while reading channel data')

 9032 FORMAT (/5X,'Default Channel Cross-sections:'//5X,3A10/)  

 9034 FORMAT (5X,I10,(T16,2F10.3))  

 9035 FORMAT (/5X,'Link Element Data:'//5X,6A11/)  

 9037 FORMAT (5X,I11,3F11.3,(T50,2F11.3))  

 9054 FORMAT ('Number of default channel cross-section categories ', &
&        'NDEFCT =',I4,2X,'lies outside range 0:NOCTAB = 0 :',I4)

 9055 FORMAT ('Number of width/elevation pairs NXDEF(',I3,') =',I4,2X, &
&        'lies outside range 2:NOCTAB = 2:',I4)

 9137 FORMAT (5X,I11,3F11.3,3X,'default category',I3)  
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
DO 210 ielmm = total_no_links + 1,total_no_elements  
  210 WRITE(PPPRI, 9210) ielmm, (QOC (ielmm, FACE), FACE = 1, 4), GETHRF (ielmm)  

WRITE(PPPRI, 9100) 'END ----'  
 9100 FORMAT (//'---- OC MODULE  RESULTS ',A:F10.2,A//)  
 9200 FORMAT (4X,A4,4(2X,A8,I1,A1),2A12/)  

 9210 FORMAT (4X,I4,SP,4F12.3,S,2F12.3)  
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
!----------------------------------------------------------------------*
! Entry requirements:
!  NELEE.ge.[NEL,NOCTAB*NOCTAB]    NEL.gt.NLF    NLF.ge.0    NOCTAB.ge.1
!  NLFEE.ge.NLF    OCD open for F input      PRI open for F output
!----------------------------------------------------------------------*
INTEGER, INTENT(OUT) :: KONT
DOUBLEPRECISION      :: TDC, TFC
DOUBLEPRECISION      :: CATR (NOCTAB), DDUM2 (NOCTAB, NOCTAB)
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
      READ (OCD, * ) (CATR (I), I = 1, NCATR)  
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

    4 END DO  
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
   DO 210 ielt = NGDBGN, total_no_elements  
  210    STRXX(ielt) = CATR (ICAT (ielt) )  
   CALL AREADI (IDUM(1:nelee), KKON, OCD, PPPRI, NCATR)  
   DO 220 ielt = NGDBGN,total_no_elements  
  220    STRYY(ielt) = CATR (ICAT (ielt) )  

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
 2000    END DO  
   WRITE(PPPRI, 9080) ' END OF '  
ENDIF  

RETURN  
 8047 WRITE (MSG, 9004) NCATR, NOCTAB  

CALL ERROR(FFFATAL, 1047, PPPRI, 0, 0, MSG)  

 9004 FORMAT('Number of roughness categories NCATR =',I4,2X, &
&       'lies outside range 0:NOCTAB = 0 :',I4)

 9080 FORMAT (///'---- OC MODULE ',A,'INPUT DATA PROCESSING ----'///: &
&          5X,'NUMBER OF DIFFERENT OVERLAND FLOW ROUGHNESS', &
&             ' CATEGORIES   NCATR = ',I4 )

 9082 FORMAT (/5X,'DEFAULT VALUE OF OVERLAND FLOW ROUGHNESS ', &
&             'COEFFICIENT     CDRS = ', F8.2)

 9084 FORMAT (/4X,' ROUGHNESS COEFFICIENTS  CATR  ATTACHED TO', &
&            ' EACH OF THE NCATR CATEGORIES' / (10F10.2))

 9085 FORMAT (/5X,'Initial overland water depth is ',A)  

 9412 FORMAT (I5,' ERROR(S) FOUND DURING OC INPUT DATA PROCESSING')  

 9500 FORMAT (/5X,'Default OC B.C. is ',A,' at catchment boundaries ', &
&            'and at channel/bank dead-ends')

 9600 FORMAT (/5X,'OC Boundary Conditions:'//5X,3A8,A12,A10,A14)  

 9610 FORMAT (5X,3I8,A12,I10,1P,5G14.6)  
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
!  GP          3.4  Set HRF at head boundaries.  Disallow flow against
!                   surface gradient (except boundaries & confluences),
!                   and move trap for small depths & setting of ARXL to
!                   after this point; also, replace WLMIN with 1D-5.
!                   Trap large flows (ABS(QOC)>QMAX) in channels.
! RAH  941008 3.4.1 Bring IMPLICIT from SPEC.AL.  Move traps (except
!                   QMAX) to new subroutine OCFIX (confluences too).
! RAH  961228  4.1  Remove variables TF & LONT.
! RAH  971215  4.2  Explicit typing.  Bring DD,EE,GG from SPEC.OC.
!                   Remove DOCEV,DLIOC,DETOCO,DOCUZO,DOCSZO.
!                   Set NCR,NPR,NSV (& NDUM) only where necessary.
!                   Merge OCQDQ loops.  Don't call OCMAS.
!      971216       Move first row initialization nearer to OCABC.
!                   Call OCQDQ always (not only 1st step) at the start,
!                   and not at the end of a step.  Scrap tests NLF.GT.0.
!                   Use IROW as subscript not NROWF; also use IRSV.
!                   Cut duplicate code in main loop over rows.
!      971217       Merge EE,GG code for first row into loop over rows.
!                   Initialize only useful elements of AA,BB,CC,FF.
!                   Separate treatment for DD of last row.
!      980106       Call ERROR if ICOD=1 after PMINVM.
!                   Next IROW if NCR=0 in main loop.  Scrap JFACE.
!                   Renumber labels 4,190,200 as 44,250,255.
!                   Amend head boundary implementation, & move to OCABC.
!                   New locals DDI,DH,DQ,DW,HI,HM,IBR,IM,WI,WM,Z.
!                   Use DCOPY & CHSGN to set QOC.  Use ABS not DABS.
!      980107       OCABC arguments: remove ICOUNT;
!                   add IEL,NXSECT,ZBFULL,ZGRUND,HRF,AA,BB,CC,FF.
!                   Bring AA,BB,CC,FF from SPEC.OC.
!      980108       OCABC arguments: add NSV,NCR,NPR,PNETTO,QH,ESWA.
!                   Move initialization of AA,BB,CC,FF to OCABC.
!      980109       OCABC arguments: add IBC,HOCNOW.
!      980212       Scrap NROWFN - use NROWST(IROW+1)-1.
!      980226       Call OCPRI.  Bring DTOC from SPEC.OC; pass to OCABC.
!                   Call OCQDQ once only, and pass COCBCD,QOCF.
!      980327       Pass XAFULL (new local) to OCQDQ.  Scrap local K.
!                   Pass OCTIME, not OCNOW, to OCPRI, & call before QMAX
!                   test, not after.
!      980331       Pass STRX,STRY to OCQDQ.
!      980409       Pass HOCNOW to OCQDQ.
!      980424       Pass NXSCEE,XSTAB to OCQDQ.
!      980427       Pass new local FWRK to OCQDQ.
!JE    JAN 2009     Loop restructure for AD
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     SPEC.AL          NELEE,NLFEE,NOCTAB,NXOCEE,NYEE
!     SPEC.OC          NXSCEE
! Input common
!     SPEC.AL          FATAL,NEL,NLF,NSTEP,PRI
!                      ICMREF(NELEE,3:12),NOCBCC(NEL)
!                      ICMRF2(NLFEE,6),   NOCBCD(NOCTAB,2:4)
!                      QMAX
!                        AREA(NEL), CLENTH(NLFEE),DXQQ(NEL),DHF(NELEE,4)
!                      ZGRUND(NEL), CWIDTH(NLFEE),DYQQ(NEL)
!                                   ZBFULL(NLFEE)
!                        ESWA(NEL), PNETTO(NEL),    QH(NEL),OCNEXT,OCNOW
!     SPEC.OC          NELIND(NEL), NROWEL(NEL),NROWF,NROWL
!                      NXSECT(NLFEE),    NROWST(NROWF:NROWL+1)
!                       XAREA(NLFEE,NOCTAB),STRX(NEL),  QOCF(NOCTAB)
!                        XINH(NLFEE,NOCTAB),STRY(NEL),HOCNOW(NOCTAB)
!                        XINW(NLFEE,NOCTAB),TDC,TFC,COCBCD(5,NOCTAB)
!                       XSTAB(3,NXSCEE,NLFEE)
! In+out common
!     SPEC.AL          HRF(NEL)
! Output common
!     SPEC.AL          QOC(NELEE,4),DQ0ST(NELEE,4),  ARXL(NLFEE)
!                      QSA(NELEE,4),DQIST(NELEE,4),DQIST2(NLFEE,3)
INTEGER         :: I, IELs, IND, IROW, IBC, IBR, ICOD, IFACE, IHB, IM, IRSV
INTEGER         :: J, JEL, JND, JROW, K0, LINK, N, NCR, NPR, NSV  , face
!INTEGER         :: LCdum (NXOCEE, 2)
INTEGER         :: ijedum(nelee,4,2:3), ijedum2(nlfee,3,2), kk, ll, vv  !afromICMREF (NELEE, 4, 2:3), afromICMRF2 (NLFEE, 3, 2)
DOUBLEPRECISION :: DDI, DH, DQ, DW, H, HI, HM, OCTIME, WI, WM, Z
DOUBLEPRECISION :: AA (NXOCEE, NXOCEE), DD (NXOCEE, NYEE), FF(NXOCEE)
DOUBLEPRECISION :: BB (NXOCEE, NXOCEE), GG (NXOCEE, NYEE)  
DOUBLEPRECISION :: CC (NXOCEE, NXOCEE), EE (NXOCEE, NXOCEE, NYEE)  
DOUBLEPRECISION :: TM1 (NXOCEE, NXOCEE), TV1 (NXOCEE) !, FWRK (NXSCEE * 12)
DOUBLEPRECISION :: TM2 (NXOCEE, NXOCEE), TV2 (NXOCEE)  
DOUBLEPRECISION, DIMENSION(total_no_elements)    :: inhrf
DOUBLEPRECISION, DIMENSION(total_no_elements)    :: GGGETHRF
DOUBLEPRECISION, DIMENSION(total_no_elements,4)  :: inqsa
DOUBLEPRECISION, DIMENSION(total_no_elements,4)  :: GGGETQSA
LOGICAL         :: g8018, g8029, cycle255
LOGICAL         :: first=.TRUE.
CHARACTER(36)   :: MSG  
!----------------------------------------------------------------------*
!
! ----- Initialize
!
DTOC = OCNEXT * 3600.D0  
IF (FIRST) THEN
    first = .FALSE.  
    DO LINK = 1, total_no_links  
        XAFULL (LINK) = XAREA (LINK, NXSECT (LINK) )
    ENDDO 
ENDIF  
!
! ----- GET PRESCRIBED BOUNDARY VALUES HOCNOW & QOCF
! sb 120514 put this line back in
!
CALL OCEXT  
!
! ----- CALCULATE FLOWS QSA & DERIVATIVES DQ0ST,DQIST,DQIST2
!

CALL OCQDQ ()
!
! ----- LOOP OVER ROWS, CALCULATING EE & GG
!
NCR = 0
g8018=.FALSE.
g8029=.FALSE.
out44 : DO IROW = NROWF, NROWL
    IF(g8018) CYCLE out44
    IRSV = IROW + 1  
    !
    !        NCR    : NUMBER OF ELEMENTS IN THE CURRENT ROW
    !        NPR    : NUMBER OF ELEMENTS IN THE PREVIOUS ROW
    !        NSV    : NUMBER OF ELEMENTS IN THE NEXT (SUIVANT) ROW
    !
    NPR = NCR  
    K0 = NROWST (IROW) - 1  
    NCR = NROWST (IRSV) - 1 - K0  
    IF (NCR.EQ.0) CYCLE out44 !GOTO 44  
    NSV = NROWST (MIN (IRSV, NROWL) + 1) - NROWST (IRSV)  
    !
    ! CALCULATE MATRICES AA, BB, CC, FF
    !
    DO IND = 1, NCR  
        iels = NROWEL (IND+K0)  
        LINK = MAX (1, MIN (iels, total_no_links) )  
        IBC = NOCBCC (iels)  
        IF (IBC.GT.0) THEN  
            IHB = NOCBCD (IBC, 4)  
            IBC = NOCBCD (IBC, 3)  
        ELSE  
            IHB = 1  
        ENDIF  
        CALL OCABC (IND, IROW, iels, NSV, NCR, NPR, IBC, NXSECT ( &
                    LINK), cellarea (iels), ZGRUND (iels), CLENTH (LINK), ZBFULL ( &
                    LINK), GETHRF (iels), PNETTO (iels), QH (iels), ESWA (iels), &
                    HOCNOW (IHB), AA(:,IND), BB (1:ncr,IND), CC(:,IND), &
                    FF (IND) )
    ENDDO  
    !
    ! CALCULATE MATRIX TM2 (inverse of CC.EE+BB) AND VECTOR TV2 (FF-CC.GG)
    !
    IF (IROW.EQ.NROWF) THEN  
        DO IND = 1, NCR
            TM2(1:ncr,IND) = BB(1:ncr,IND)
            !CALL DCOPY (NCR, BB(1:ncr,IND), 1, TM2(1:ncr,IND), 1)  
        ENDDO  
         TV2(1:ncr) = FF(1:ncr) 
        !CALL DCOPY(NCR, FF(1:ncr), 1, TV2(1:ncr), 1)  
    ELSE  
        !CALL MULMM (TM1, CC, EE (1, 1, IROW), NCR, NPR, NCR, NXOCEE)
        tm1(1:ncr,1:ncr) = JEMATMUL_MM(cc(1:npr,1:ncr), ee(1:ncr,1:npr,irow), ncr, npr, ncr)
        
        !CALL ADDMM (TM2, BB, TM1, NCR, NCR, NXOCEE)
        tm2(1:ncr,1:ncr) = bb(1:ncr,1:ncr) + tm1(1:ncr,1:ncr)
        
        !CALL MULMV (TV1, CC, GG (1, IROW), NCR, NPR, NXOCEE)
        tv1(1:ncr) = JEMATMUL_VM(cc(1:npr,1:ncr), gg(1:npr,irow), ncr, npr)
        
        !CALL DIFVV (TV2, FF, TV1, NCR, NXOCEE) 
         TV2(1:ncr) = FF(1:ncr) - TV1(1:ncr)
    ENDIF  
    !CALL PMINVM (TM2(1:ncr,1:ncr), NCR, ICOD)  
    CALL INVERTMAT(TM2(1:ncr,1:ncr), NCR, ICOD)  
    IF (ICOD.EQ.1) THEN
        g8018=.TRUE.
        CYCLE out44
    ENDIF
    !
    ! CALCULATE MATRIX EE(IROW+1)
    !
    IF (IROW.NE.NROWL) THEN  
        !CALL MULMM (EE (1, 1, IRSV), TM2, AA, NCR, NCR, NSV, NXOCEE)
        ee(1:nsv,1:ncr,irsv) = JEMATMUL_MM(tm2(1:ncr,1:ncr), aa(1:nsv,1:ncr), ncr, ncr, nsv) 
        
        !CALL CHSGN (EE (1, 1, IRSV), NCR, NSV, NXOCEE)
        ee(1:nsv,1:ncr,irsv) = - ee(1:nsv,1:ncr,irsv)
    ENDIF  
    !
    ! CALCULATE VECTOR GG(IROW+1)
    !!
    !CALL MULMV (GG (1, IRSV), TM2, TV2, NCR, NCR, NXOCEE)
    gg(1:ncr,irsv) = JEMATMUL_VM(tm2(1:ncr,1:ncr), tv2(1:ncr), ncr, ncr)
    !
ENDDO out44
IF(g8018) THEN
    !AD WRITE (MSG, '(A,I4)') 'Singular matrix at row', IROW  
    CALL ERROR(FFFATAL, 1018, PPPRI, NROWEL (NROWST (IROW) ), 0, MSG)
ELSE
    !
    ! ----- DOWNWARDS SWEEP, CALCULATION OF DD
    !
    !     * last row first (use NCR,IRSV from loop above)
    IROW = NROWL  
    DD(1:ncr,IROW) = GG(1:ncr,IRSV)
    !ALL DCOPY(NCR, GG(1:ncr,IRSV), 1, DD(1:ncr,IROW), 1)  
    !     * loop over remaining rows
    DO IROW = NROWL - 1, NROWF, - 1  
        IRSV = IROW + 1  
        NSV = NCR  
        NCR = NROWST (IRSV) - NROWST (IROW)  
        !CALL MULMV (TV1, EE (1, 1, IRSV), DD (1, IRSV), NCR, NSV, NXOCEE)
        tv1(1:ncr) = JEMATMUL_VM(ee(1:nsv,1:ncr,irsv), dd(1:nsv,irsv), ncr, nsv)
        
        !CALL ADDVV (DD (1, IROW), TV1, GG (1, IRSV), NCR)
        dd(1:ncr,irow) = tv1(1:ncr) + gg(1:ncr,irsv) 
    ENDDO  
    !
    ! ----- ADVANCE WATER LEVELS AND FLOWS TO TIME LEVEL N+1,
    !       USING FIRST ORDER DERIVATIVES OF FLOWS AT TIME LEVEL N
    !
    DO iels = 1,total_no_elements  
        IND = NELIND (iels)  
        IROW = ICMREF (iels, 3)  
        DDI = DD (IND, IROW)  
        CALL SETHRF(iels, GETHRF (iels) + DDI)
        DO IFACE = 1, 4  
            DQ = DQ0ST (iels, IFACE) * DDI  
            JEL = ICMREF (iels, IFACE+4)  
            IF (JEL.GT.0) THEN  
                JND = NELIND (JEL)  
                JROW = ICMREF (JEL, 3)  
                DQ = DQIST (iels, IFACE) * DD (JND, JROW) + DQ  
            ELSEIF (JEL.LT.0) THEN  
                IBR = - JEL  
                DO J = 1, 3  
                    JEL = ICMRF2 (IBR, J)  
                    IF (JEL.GT.0) THEN  
                        JND = NELIND (JEL)  
                        JROW = ICMREF (JEL, 3)  
                        DQ = DQIST2 (IBR, J) * DD (JND, JROW) + DQ  
                    ENDIF  
                ENDDO  
            ENDIF  
            CALL SETQSA(iels, IFACE, GETQSA(iels, IFACE) + DQ)  
        ENDDO  
    ENDDO  
    !
    ! CHECK FOR SPURIOUS NEGATIVE FLOWS, AND RECALCULATE WATER LEVELS
    ! IF REQUIRED.  NB. DOES NOT CHECK BOUNDARY FLOWS
    !
    ! called routine has afromICMREF (NELEE, 4, 2:3), afromICMRF2 (NLFEE, 3, 2)
    vv=5
    DO LL=2,3       !AD PASSING PROBLEMS (use local dummies)
        DO kk=1,4   !array order
            ijedum(:,kk,LL) = icmref(:,vv)
            vv = vv + 1
        ENDDO
    ENDDO
    vv = 1
    DO LL=1,2
        DO kk=1,3  !array order
            ijedum2(:,kk,LL) = icmrf2(:,vv)
            vv = vv + 1
        ENDDO
    ENDDO
    !CALL OCFIX (ICMREF (1, 5), ICMRF2, AREA, DTOC, QSA)
    !CALL OCFIX(ijedum, ijedum2, total_no_elements, dtoc)  !AD PASSING PROBLEMS (use local dummies)
    
    !ad this untidy mess is for debugging of tangent
    DO vv=1,total_no_elements
        inhrf(vv) = GETHRF(vv)
        DO face=1,4
            inqsa(vv,face) = GETQSA(vv,face)
        ENDDO
    ENDDO


CALL OCFIX(ijedum, ijedum2, total_no_elements, dtoc, inhrf, GGGETHRF, inqsa, GGGETQSA)  !AD PASSING PROBLEMS (use local dummies)!




    DO vv=1,total_no_elements
        CALL SETHRF(vv, GGGETHRF(vv))
        DO face=1,4
            CALL SETQSA(vv, face, GGGETQSA(vv,face))
        ENDDO
    ENDDO
    
    
    
    
    ! SET FLOWS QOC (POSITIVE X,Y) FOR USE BY OTHER COMPONENTS
    !
    !DO IFACE = 1, 4  
        QOC(1:total_no_elements,:) = GETQSA_ALL(total_no_elements)
        !CALL DCOPY(NEL, QSA(1:nel,IFACE), 1, QOC(1:nel,IFACE), 1)  
    !ENDDO  
    !CALL CHSGN (QOC, 2, NEL, NELEE)
    qoc(1:total_no_elements,1:2) = - qoc(1:total_no_elements,1:2)
    !
    ! ----- CALCULATE CROSS-SECTIONAL AREA OF CHANNEL WATER
    !
    out255 : DO iels = 1, total_no_links
        cycle255=.FALSE.  
        Z = GETHRF (iels)  
        H = Z - ZGRUND (iels)  
        N = NXSECT (iels)  
        out250 : DO I = 2, N
            IF(cycle255) CYCLE out250 
            HI = XINH (iels, I)  
            IF (H.LT.HI) THEN  
                IM = I - 1  
                HM = XINH (iels, IM)  
                WM = XINW (iels, IM)  
                WI = XINW (iels, I)  
                DH = H - HM  
                DW = (WI - WM) * (DH / (HI - HM) )  
                ARXL (iels) = XAREA (iels, IM) + (WM + half * DW) * DH  
                cycle255 = .TRUE.
            ENDIF  
        ENDDO out250
        IF(cycle255) CYCLE out255  
        ARXL (iels) = XAREA (iels, N) + (Z - ZBFULL (iels) ) * CWIDTH ( iels)
    ENDDO out255  
    !
    ! ----- Print results
    !
    OCTIME = OCNOW + OCNEXT  
    IF ((OCTIME.GE.TDC).AND.(OCTIME.LE.TFC)) CALL OCPRI (OCTIME, ARXL, QOC)
    !
    ! ----- CHECK FOR CHANNEL BLOW-UP
    !
    IF (GTZERO(QMAX)) THEN  
        out270 : DO iels = 1, total_no_links
            IF(g8029) CYCLE out270
            out260 : DO IFACE = 1, 4  
                IF(g8029) CYCLE out260
                IF (ABS (QOC (iels, IFACE) ) .GT.QMAX) g8029 = .TRUE.
            ENDDO out260  
        ENDDO out270
    ENDIF
ENDIF
IF(g8029) THEN
     MSG = 'CHANNEL FLOWS EXCEED MAXIMUM ALLOWED'  
    CALL ERROR(FFFATAL, 1029, PPPRI, iels, 0, MSG)
ENDIF
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
!      980317     ! Fix inaccuracy in XAJ (was linear in H); set XDERIV
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
!                             derivative of conveyance, respectively
! NB:
!       XSTAB(2:3,j,1:NLF) not defined for j=NXSCEE
!
INTEGER         :: I, IELr, J, N  
DOUBLEPRECISION :: ALPHA, DH, HI, HIP1, HJ, STEPH, STR, W2, XAJ, XCJ, XCJM1, adumy
!----------------------------------------------------------------------*

DO 500 ielr = 1, total_no_links  
!
! LOCAL VARIABLES
!
   N = NXSECT (ielr)  

   STR = STRXX(ielr)  
!
! SET UP CROSS-SECTIONAL AREAS FOR EACH OF THE INPUT LEVELS
!
   XAREA (ielr, 1) = zero  
   DO 180 J = 2, N  
      W2 = XINW (ielr, J) + XINW (ielr, J - 1)  
      DH = XINH (ielr, J) - XINH (ielr, J - 1)  
      XAREA (ielr, J) = XAREA (ielr, J - 1) + W2 * DH * half  

  180    END DO  
!
! EFFECTIVE BED ELEVATION
!

   ZBEFF (ielr) = ZBFULL (ielr) - XAREA (ielr, N) / CWIDTH (ielr)  
!
! SET UP FULL CROSS-SECTION TABLES OF HEIGHT, CONVEYANCE & DERIVATIVE
!
! NOTE: The formulation is such that
!             XSTAB(2,j,ielr) + XSTAB(3,j,ielr)*( h - XSTAB(1,j,ielr) )
!       is a continuous (piecewise linear) function of h
!
   I = 1  
   HI = XINH (ielr, I)  
   STEPH = XINH (ielr, N) / (NXSCEE-1)  
   XCJ = zero  
   XSTAB (1, 1, ielr) = zero  
   DO 200 J = 2, NXSCEE  
      XCJM1 = XCJ  
      HJ = STEPH * (J - 1)
      
      
  !orig    
  !190       HIP1  = XINH(ielr,I+1)
  !          IF ( I.LT.N-1 .AND. HIP1.LT.HJ ) THEN
  !               I = I + 1
  !              HI = HIP1
  !              GOTO 190
  !          ENDIF
        
        
        DO
            HIP1  = XINH(ielr,I+1)
            IF (.NOT.( (I.LT.N-1) .AND. (HIP1.LT.HJ) )) EXIT
                 I = I + 1
                HI = HIP1
        ENDDO
  
  
  
  
            
  !    iscycle=.FALSE.  !NOT QUITE RIGHT
  !    DO I=I,N-1
  !      IF(iscycle) CYCLE
  !      HIP1 = XINH (ielr,I+1)  
  !      IF (HIP1<HJ) THEN
  !           HI = HIP1
  !       ELSE
  !         iscycle=.true.
  !      ENDIF  
  !    ENDDO  


      DH = HJ - HI  
      ALPHA = DH / (HIP1 - HI)  
      W2 = (2D0 - ALPHA) * XINW (ielr, I) + ALPHA * XINW (ielr, I + &
       1)
      XAJ = XAREA (ielr, I) + W2 * DH * half  

      !XCJ = STR * XAJ * HJ**F23
      CALL CONVEYAN(str, hj, xcj, adumy, 0, xaj) 
      XSTAB (1, J, ielr) = HJ  
      XSTAB (2, J - 1, ielr) = XCJM1  
      XSTAB (3, J - 1, ielr) = (XCJ - XCJM1) / STEPH  

  200    END DO  

  500 END DO  
END SUBROUTINE OCXS



!FFFFFF INTEGER FUNCTION LINKNO
INTEGER FUNCTION LINKNO (I, J, NSOUTH)  
! GET LINK NUMBER, GIVEN X, Y CO-ORDINATES AND ORIENTATION
LOGICAL, INTENT(IN) :: NSOUTH
INTEGER, INTENT(IN) :: i, j
INTEGER             :: L
LOGICAL             :: jedum, iscycle !NEEDED FOR AD
LINKNO = 0  
IF (total_no_links.EQ.0) RETURN  
iscycle=.FALSE.
DO L = 1, total_no_links
    IF(iscycle) CYCLE
    jedum = NSOUTH.EQV.LINKNS (L)
   IF ((ICMREF (L, 2) .EQ.I) .AND. (ICMREF (L, 3) .EQ.J) .AND. jedum ) THEN
      LINKNO = L  
      iscycle=.TRUE.  
   ENDIF  
ENDDO  
END FUNCTION LINKNO
END MODULE OCmod