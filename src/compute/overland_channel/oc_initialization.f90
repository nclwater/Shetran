MODULE oc_initialization
! Core initialization routines for overland channel calculations
! Data validation functions moved to oc_validation.f90 module
! Extracted from OCmod.f90

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
   USE oc_common_data
   USE oc_validation, ONLY: OCCHK0, OCCHK1, OCCHK2
   USE oc_input, ONLY: OCREAD, JEOCBC, OCPLF
   USE oc_utils, ONLY: LINKNO

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: OCINI, OCIND, OCXS

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
9100  FORMAT (/5X,'Size of internal tables for channel conveyance, etc', '  NXSCEE =',I6)
   END SUBROUTINE OCINI

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
! [Version history as in original...]
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
5           END DO
!
! ------- Next square
!
10       END DO
!
! ---- Next row
!
         NXOC = MAX (NXOC, K + 1 - NROWST (J) )
         IF (ICOUNT.GT.0) NROWL = J
!
20    END DO
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

!SSSSSS SUBROUTINE OCXS
   SUBROUTINE OCXS ()
!----------------------------------------------------------------------*
!
!  Set up channel cross-section tables & effective bed elevations
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/OC/OCXS/4.2
! [Version history as in original...]
!----------------------------------------------------------------------*
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

180      END DO
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
            !          ENDIF


            DO
               HIP1  = XINH(ielr,I+1)
               IF (.NOT.( (I.LT.N-1) .AND. (HIP1.LT.HJ) )) EXIT
               I = I + 1
               HI = HIP1
            ENDDO

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

200      END DO

500   END DO
   END SUBROUTINE OCXS

END MODULE oc_initialization
