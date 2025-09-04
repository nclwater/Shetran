MODULE subsurface_io
!----------------------------------------------------------------------*
! Input/output operations and file reading
! Contains VSREAD - VSS data file reader
!----------------------------------------------------------------------*
! Extracted from VSmod.f90 as part of refactoring
! Date: 2025-09-04
! Source: VSmod.f90.sav (lines 3700-4005)
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE subsurface_variables
   USE AL_G, ONLY : ICMREF, NX, NY, ICMXY, NGDBGN
   USE AL_C, ONLY : NTSOIL, NS, BEXBK, VSD, NVSWLI, IDUM, DUMMY, NLYR, ZLYRBT, &
      ICMBK, ZBEFF, NVSWLT, NVSSPT, VSPOR, VSI
   USE UTILSMOD, ONLY : FINPUT, HINPUT
   USE mod_load_filedata, ONLY : ALREAD
   IMPLICIT NONE

   PRIVATE
   ! Public I/O routines
   PUBLIC :: VSREAD

CONTAINS

!SSSSSS SUBROUTINE VSREAD (NAQCON, IAQCON)
   SUBROUTINE VSREAD (NAQCON, IAQCON)
!----------------------------------------------------------------------*
! Reads in all data from VSS input data file
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSREAD/4.1
! Modifications:
!  GP  20.07.94  written (v4.0 finished 31/1/96)
! RAH  970213  4.1  Initialize NLBTYP,NLBCAT,NVSWLC,NBBTYP,NBBCAT.
!                   Reverse subscripts NVSLFL,NVSLHL,NVSLGL (see VSSIM).
!      970522       Initialize NVSWLI.  Fix errors: use TBKR not TBTHE
!                   in loop 21 (IVSFLG=2); add -1 to NVSLHT & NVSLGT.
!      970630       Bring NAQCON,IAQCON from VSINIT.INC to arg-list, &
!                   swap indices to fix error in ALREAD call.
!                   Fix ALREAD call VS08b: only if NLF>0.
!      970805       Ensure {NLBCAT,NBBCAT,NVSWLC}.ge.1.
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P.VSS         LLEE,NELEE,NLYREE,NSEE,NVSEE
      INTEGER :: NAQCON, IAQCON (4, NVSEE)
! Locals, etc
      INTEGER :: I, I0, IBK, ICAT, IEL, ILYR, IS, ISP, IW, IWT, IX, &
         IXY0, IY
      INTEGER :: ICOUNT, LCOUNT
      INTEGER :: IVSDUM (NELEE, NLYREE), IVSCAT (NELEE), ISDUM (NSEE, 8)
      INTEGER :: NUM_CATEGORIES_TYPES,  NELEM, NCOUNT, NDUM, NSP, NW
      INTEGER :: ILB, NLB, ITYP, NLDUM, ISDUM1, IDUM1(1)
      DOUBLEPRECISION RVSDUM (NELEE, NLYREE), RSDUM (NSEE, 8), DCSDUM ( &
         0:LLEE), DCSNOD (LLEE), DCRDUM (0:LLEE), DCRNOD (LLEE), SIG, PDUM
      DOUBLEPRECISION XDUM (NVSEE), YDUM (NVSEE), Y2DUM (NVSEE), &
         UDUM (NVSEE)
      CHARACTER (LEN=80) :: CDUM, MSG * 132
      LOGICAL :: BDONE (NELEE)

      DATA BDONE / NELEE * .FALSE. /
!
!----------------------------------------------------------------------*
!
! Initialization
      DO 6 IEL = 1, total_no_elements
         NVSWLI (IEL) = 0
         NLBTYP (IEL) = 0
         NBBTYP (IEL) = 0
         NVSWLC (IEL) = 1
         NLBCAT (IEL) = 1
         NBBCAT (IEL) = 1


6     END DO
! VS01 ----- main data file title
      CALL ALREAD (1, VSD, PPPRI, ':VS01', 1, 1, 0, CDUM, IDUM, DUMMY)


      WRITE(PPPRI, '(/ X,A /)') CDUM
! VS02 ----- logical flags
      READ (VSD, '(A)') CDUM


      READ (VSD, * ) BFAST, BSOILP, BHELEV
! VS03 ----- integer variables
      CALL ALREAD (2, VSD, PPPRI, ':VS03', 4, 1, 0, CDUM, IDUM, DUMMY)
      NS = IDUM (1)
      NCSZON = IDUM (2)
      NCRBED = IDUM (3)


      INITYP = IDUM (4)
! VS04 ----- real variables
      CALL ALREAD (3, VSD, PPPRI, ':VS04', 5, 1, 0, CDUM, IDUM, DUMMY)
      VSIPSD = DUMMY (1)
      VSZMIN = DUMMY (2)
      VSZMAX = DUMMY (3) + 1.0D-6
      VSWV = DUMMY (4)


      VSWL = DUMMY (5)
! VS05 ----- physical property data
      CALL ALREAD (7, VSD, PPPRI, ':VS05', NSEE, 8, NS, CDUM, ISDUM, &
         RSDUM)
      DO 10 IS = 1, NS
         IVSFLG (IS) = ISDUM (IS, 2)
         IVSNTB (IS) = ISDUM (IS, 3)
         VSK3D (IS, 1) = RSDUM (IS, 1) / (3600.0D0 * 24.0D0)
         VSK3D (IS, 2) = RSDUM (IS, 2) / (3600.0D0 * 24.0D0)
         VSK3D (IS, 3) = RSDUM (IS, 3) / (3600.0D0 * 24.0D0)
         VSPOR (IS) = RSDUM (IS, 4)
         VSTRES (IS) = RSDUM (IS, 5)
         VSPSS (IS) = RSDUM (IS, 6)
         VSVGN (IS) = RSDUM (IS, 7)

         VSALPH (IS) = RSDUM (IS, 8)

         VSPPOR (IS) = VSPOR (IS)


10    END DO
! VS05a ---- soil characteristic function tabulated data
      DO 15 IS = 1, NS

         IF (IVSFLG (IS) .EQ.2.OR.IVSFLG (IS) .EQ.4) THEN
            READ (VSD, * ) ISDUM1
            IF (IS.NE.ISDUM1) THEN
               WRITE (MSG, 9030) IS
               CALL ERROR(FFFATAL, 1051, PPPRI, 0, 0, MSG)

            ENDIF
            DO 14 I = 1, IVSNTB (IS)
               READ (VSD, * ) TBPSI (I, IS), TBTHE (I, IS), TBKR (I, IS)


14          END DO
! set up cubic spline coefficients for theta, using log(psi)
! based on routines 'spline' and 'splint' in NUMERICAL RECIPES
! FOR FORTRAN (..UNFINISHED), pp 109 and 110
! NB asasumes 'natural' boundary conditions (ie zero 2nd derivatives)
            DO 16 I = 1, IVSNTB (IS)
               XDUM (I) = DLOG10 ( - TBPSI (I, IS) )
               YDUM (I) = TBTHE (I, IS)
16          END DO

            NDUM = IVSNTB (IS)
            Y2DUM (1) = zero
            UDUM (1) = zero
            Y2DUM (NDUM) = zero
            DO 17 I = 2, NDUM - 1
               SIG = (XDUM (I) - XDUM (I - 1) ) / (XDUM (I + 1) - XDUM ( &
                  I - 1) )
               PDUM = SIG * Y2DUM (I - 1) + two
               Y2DUM (I) = (SIG - one) / PDUM
               UDUM (I) = (6.0D0 * ( (YDUM (I + 1) - YDUM (I) ) / &
                  (XDUM (I + 1) - XDUM (I) ) - (YDUM (I) - YDUM (I - 1) ) &
                  / (XDUM (I) - XDUM (I - 1) ) ) / (XDUM (I + 1) - XDUM (I &
                  - 1) ) - SIG * UDUM (I - 1) ) / PDUM
17          END DO
            DO 18 I = NDUM - 1, 1, - 1
               Y2DUM (I) = Y2DUM (I) * Y2DUM (I + 1) + UDUM (I)
18          END DO
            DO 19 I = 1, NDUM
               TBTHEC (I, IS) = Y2DUM (I)


19          END DO
! if required, set up cubic spline coefficients for Kr similarly

            IF (IVSFLG (IS) .EQ.2) THEN
               DO 21 I = 1, IVSNTB (IS)
                  YDUM (I) = TBKR (I, IS)

21             END DO
               Y2DUM (1) = zero
               UDUM (1) = zero
               Y2DUM (NDUM) = zero
               DO 22 I = 2, NDUM - 1
                  SIG = (XDUM (I) - XDUM (I - 1) ) / (XDUM (I + 1) &
                     - XDUM (I - 1) )
                  PDUM = SIG * Y2DUM (I - 1) + two
                  Y2DUM (I) = (SIG - one) / PDUM
                  UDUM (I) = (6.0D0 * ( (YDUM (I + 1) - YDUM (I) ) &
                     / (XDUM (I + 1) - XDUM (I) ) - (YDUM (I) - YDUM (I - &
                     1) ) / (XDUM (I) - XDUM (I - 1) ) ) / (XDUM (I + 1) &
                     - XDUM (I - 1) ) - SIG * UDUM (I - 1) ) / PDUM
22             END DO
               DO 23 I = NDUM - 1, 1, - 1
                  Y2DUM (I) = Y2DUM (I) * Y2DUM (I + 1) + UDUM (I)
23             END DO
               DO 24 I = 1, NDUM
                  TBKRC (I, IS) = Y2DUM (I)

24             END DO

            ENDIF

         ENDIF



15    END DO
! VS06 ----- soil zone cell sizes (start at the ground surface)
! NB. dcsnod(ncszon+1) is set to the BOTTOM of the (fictional) cell
! immediately below the soil zone, rather than at the node, to ensure
! that no layer can exist in the aquifer zone with thickness < vszmin
! (see loop 530)
      IF (NCSZON.GT.0) CALL ALREAD (3, VSD, PPPRI, ':VS06', NCSZON, 1, 0, &
         CDUM, IDUM, DCSZON)
      WRITE(PPPRI, * ) 'DCSZON: ', (DCSZON (I) , I = 1, NCSZON)
      DCSTOT = zero
      DCSDUM (0) = zero
      DO 30 I = 1, NCSZON
         DCSTOT = DCSTOT + DCSZON (I)
         DCSDUM (I) = DCSTOT
         DCSNOD (I) = half * (DCSDUM (I) + DCSDUM (I - 1) )
30    END DO



      DCSNOD (NCSZON + 1) = DCSTOT + VSZMIN
! VS07 ----- river bed cell sizes (start at the bed surface)
! NB. dcrnod(ncrbed+1) is set to the BOTTOM of the (fictional) cell
! immediately below the river bed soil zone (see VS06 comment above)
! (see loop 730)
      IF (NCRBED.GT.0) CALL ALREAD (3, VSD, PPPRI, ':VS07', NCRBED, 1, 0, &
         CDUM, IDUM, DCRBED)
      WRITE(PPPRI, * ) 'DCRBED: ', (DCRBED (I) , I = 1, NCRBED)
      DCRTOT = zero
      DCRDUM (0) = zero
      DO 40 I = 1, NCRBED
         DCRTOT = DCRTOT + DCRBED (I)
         DCRDUM (I) = DCRTOT
         DCRNOD (I) = half * (DCRDUM (I) + DCRDUM (I - 1) )
40    END DO



      DCRNOD (NCRBED+1) = DCRTOT + VSZMIN
! VS08 ----- soil/lithology layer definition data
! --- read no. of categories and elements
      CALL ALREAD (2, VSD, PPPRI, ':VS08', 2, 1, 0, CDUM, IDUM, DUMMY)
      NUM_CATEGORIES_TYPES = IDUM (1)


      NELEM = IDUM (2)
! --- category data


      IF (NUM_CATEGORIES_TYPES .EQ.0) THEN
! expect all elements to be input individually
! (all grids plus 1 set of data for each link if BEXBK=.true.
!  all grids if BEXBK = .false.)
         IF (BEXBK) THEN
            NCOUNT = total_no_elements - 2 * total_no_links
         ELSE
            NCOUNT = total_no_elements - total_no_links


         ENDIF


      ELSE
! initialise arrays
         DO IEL = 1, NELEE
            DO ILYR = 1, NLYREE
               IVSDUM (IEL, ILYR) = 0
               RVSDUM (IEL, ILYR) = zero
            END DO
         END DO
! read layer data


         CALL ALREAD (6, VSD, PPPRI, ':VS08a', NELEE, NLYREE, NUM_CATEGORIES_TYPES,  CDUM, &
            IVSDUM, RVSDUM)
! for NUM_CATEGORIES_TYPES = 1, set all elements = category 1
         IF (NUM_CATEGORIES_TYPES == 1) THEN
            DO 100 IEL = 1, total_no_elements
               IVSCAT (IEL) = 1


100         END DO
! for > 1 category read in categories for links (if required) and grids
         ELSE
            IF (BEXBK.AND.total_no_links.GT.0) THEN
               CALL ALREAD (2, VSD, PPPRI, ':VS08b', total_no_links, 1, NUM_CATEGORIES_TYPES,  CDUM, &
                  IVSCAT, DUMMY)
            ENDIF
            CALL ALREAD (4, VSD, PPPRI, ':VS08c', NX, NY, NUM_CATEGORIES_TYPES,  CDUM, &
               IDUM, DUMMY)
            DO 300 IY = 1, NY
               IXY0 = (IY - 1) * NX
               DO 200 IX = 1, NX
                  IEL = ICMXY (IX, IY)
                  IF (IEL.NE.0) IVSCAT (IEL) = IDUM (IXY0 + IX)
200            END DO
300         END DO


         ENDIF
! move layer data into elements for ...
         NCOUNT = 0

         DO 400 IEL = 1, total_no_elements
            IF (.NOT.(ICMREF (IEL, 1) .EQ.1.OR.ICMREF (IEL, 1) .EQ.2.OR. ( &
               .NOT.BEXBK.AND.ICMREF (IEL, 1) .EQ.3) )) THEN
               IF (IVSCAT (IEL) .EQ.0) THEN

                  NCOUNT = NCOUNT + 1
               ELSE

                  BDONE (IEL) = .TRUE.
                  ICAT = IVSCAT (IEL)
                  ICOUNT = 0
                  DO WHILE (IVSDUM (ICAT, ICOUNT + 1) .NE.0)
                     ICOUNT = ICOUNT + 1
                  END DO


! ...grids
                  IF (ICMREF (IEL, 1) .EQ.0) THEN
                     NLYR (IEL) = ICOUNT
                     DO 360 ILYR = 1, NLYR (IEL)
                        NTSOIL (IEL, ILYR) = IVSDUM (ICAT, ILYR)
                        ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - RVSDUM (ICAT, &
                           ILYR)


360                  END DO
! ...banks
                  ELSE
                     DO 380 I = 1, 2
                        IBK = ICMBK (IEL, I)
                        BDONE (IBK) = .TRUE.
                        NLYR (IBK) = ICOUNT
                        DO 370 ILYR = 1, NLYR (IBK)
                           NTSOIL (IBK, ILYR) = IVSDUM (ICAT, ILYR)
                           ZLYRBT (IBK, ILYR) = ZGRUND (IBK) - RVSDUM ( &
                              ICAT, ILYR)
370                     END DO


380                  END DO
! ...links
!    (NB uses data from bank 2, which is identical to bank 1)
                     LCOUNT = 0
                     DO WHILE (RVSDUM (ICAT, LCOUNT + 1) .GE.ZGRUND (IBK) &
                        - ZBEFF (IEL) + VSZMIN)
                        LCOUNT = LCOUNT + 1
                     END DO
                     NLYR (IEL) = LCOUNT
                     DO 397 ILYR = 1, NLYR (IEL)
                        NTSOIL (IEL, ILYR) = NTSOIL (IBK, ILYR)
                        ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)
397                  END DO

                  ENDIF

               ENDIF
            END IF

400      END DO


      ENDIF
! check no. of category elements consistent with no. of individual eleme
      IF (NCOUNT.NE.NELEM) THEN
         WRITE (MSG, 9000) NCOUNT
         CALL ERROR(FFFATAL, 1032, PPPRI, 0, 0, MSG)


      ENDIF
! --- element data


      IF (NELEM.NE.0) THEN
! initialise variables
         DO IEL = 1, NELEE
            DO ILYR = 1, NLYREE
               IVSDUM (IEL, ILYR) = 0
               RVSDUM (IEL, ILYR) = zero
            END DO
         END DO
! read layer data

         CALL ALREAD (6, VSD, PPPRI, ':VS08d', NELEE, NLYREE, NELEM, CDUM, &
            IVSDUM, RVSDUM)


         DO 500 IEL = 1, total_no_elements
! ignore banks, links (if no banks), and elements already processed

            IF (.NOT.(BDONE (IEL) .OR.ICMREF (IEL, 1) .EQ.1.OR.ICMREF (IEL, 1) &
               .EQ.2.OR. (.NOT.BEXBK.AND.ICMREF (IEL, 1) .EQ.3) )) THEN
! move layer data into elements for ...

               BDONE (IEL) = .TRUE.
               ICOUNT = 0
               DO WHILE (IVSDUM (IEL, ICOUNT + 1) .NE.0)
                  ICOUNT = ICOUNT + 1
               END DO
! ...grids
               IF (ICMREF (IEL, 1) .EQ.0) THEN
                  NLYR (IEL) = ICOUNT
                  DO 460 ILYR = 1, NLYR (IEL)
                     NTSOIL (IEL, ILYR) = IVSDUM (IEL, ILYR)
                     ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - RVSDUM (IEL, ILYR)


460               END DO
! ...banks
               ELSE
                  DO 480 I = 1, 2
                     IBK = ICMBK (IEL, I)
                     BDONE (IBK) = .TRUE.
                     NLYR (IBK) = ICOUNT
                     DO 470 ILYR = 1, NLYR (IBK)
                        NTSOIL (IBK, ILYR) = IVSDUM (IEL, ILYR)
                        ZLYRBT (IBK, ILYR) = ZGRUND (IBK) - RVSDUM (IEL, &
                           ILYR)
470                  END DO


480               END DO
! ...links
!    (NB uses data from bank 2, which is identical to bank 1)
                  LCOUNT = 0
                  DO WHILE (RVSDUM (IEL, LCOUNT + 1) .GE.ZGRUND (IBK) - ZBEFF ( &
                     IEL) + VSZMIN)
                     LCOUNT = LCOUNT + 1
                  END DO
                  NLYR (IEL) = LCOUNT
                  DO 497 ILYR = 1, NLYR (IEL)
                     NTSOIL (IEL, ILYR) = NTSOIL (IBK, ILYR)
                     ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)
497               END DO

               ENDIF
            END IF

500      END DO


      ENDIF
! adjust horizon boundaries in soil zone to match computational mesh
! and set up ZLYRBT for ground surface
      DO 550 IEL = NGDBGN, total_no_elements
         DO 540 ILYR = NLYR (IEL), 1, - 1
            IF (ZGRUND (IEL) - ZLYRBT (IEL, ILYR) .LE.DCSTOT + VSZMIN) THEN
               DO 530 I = 1, NCSZON + 1
                  IF (DCSNOD (I) .GT.ZGRUND (IEL) - ZLYRBT (IEL, ILYR) ) &
                     THEN
                     ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - DCSDUM (I - 1)
                     EXIT
                  ENDIF
530            END DO
            END IF
540      END DO
         ZLYRBT (IEL, NLYR (IEL) + 1) = ZGRUND (IEL)

550   END DO
      IF (BEXBK) THEN
         DO 560 IEL = 1, total_no_links
            IBK = ICMBK (IEL, 1)
            DO 555 ILYR = 1, NLYR (IEL)
               ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)
555         END DO
560      END DO


      ENDIF
! check that all elements have been set up
      DO 650 IEL = 1, total_no_elements
         IF (BEXBK.OR.ICMREF (IEL, 1) .EQ.0) THEN
            IF (.NOT.BDONE (IEL) ) THEN
               NVSERR = NVSERR + 1
               WRITE (MSG, 9020) IEL
               CALL ERROR (EEERR, 1033, PPPRI, 0, 0, MSG)
            ENDIF
         END IF


650   END DO
! VS09 ----- channel bed layer


      IF (total_no_links.GT.0.AND.BEXBK) THEN
! read soil types for each link


         CALL ALREAD (2, VSD, PPPRI, ':VS09', total_no_links, 1, 1, CDUM, ISRBED, &
            DUMMY)
! read bed depths for each link


         CALL ALREAD (3, VSD, PPPRI, ':VS09a', total_no_links, 1, 1, CDUM, IDUM, &
            DRBED)
! set up channel bed layer for each link
         DO 700 IEL = 1, total_no_links
            IF (DRBED (IEL) .GT.VSZMIN) THEN
               NLYR (IEL) = NLYR (IEL) + 1
               NTSOIL (IEL, NLYR (IEL) ) = ISRBED (IEL)
               ZLYRBT (IEL, NLYR (IEL) ) = ZBEFF (IEL) - DRBED (IEL)
               IF (ZLYRBT (IEL, NLYR (IEL) ) .LT.ZLYRBT (IEL, NLYR (IEL) &
                  - 1) + VSZMIN) THEN
                  NLYR (IEL) = NLYR (IEL) - 1
                  NTSOIL (IEL, NLYR (IEL) ) = ISRBED (IEL)
               ENDIF
            ENDIF


700      END DO
! adjust horizon boundaries in river bed to match computational mesh
! and set up ZLYRBT for river bed surface
         DO 750 IEL = 1, total_no_links
            DO 740 ILYR = NLYR (IEL), 1, - 1
               IF (ZGRUND (IEL) - ZLYRBT (IEL, ILYR) .LE.DCRTOT + &
                  VSZMIN) THEN
                  DO 730 I = 1, NCRBED+1
                     IF (DCRNOD (I) .GT.ZGRUND (IEL) - ZLYRBT (IEL, ILYR) ) &
                        THEN
                        ZLYRBT (IEL, ILYR) = ZBEFF (IEL) - DCRDUM (I - 1)
                        EXIT
                     ENDIF
730               END DO
               END IF
740         END DO
            ZLYRBT (IEL, NLYR (IEL) + 1) = ZBEFF (IEL)

750      END DO


      ENDIF
! VS10 ----- aquifer zone user-defined connectivities

      CALL ALREAD (2, VSD, PPPRI, ':VS10', 1, 1, 0, CDUM, IDUM1, DUMMY)
      NAQCON = IDUM1(1)

      IF (NAQCON.GT.0) CALL ALREAD (2, VSD, PPPRI, ':VS10a', 4, NAQCON, 0, &
         CDUM, IAQCON, DUMMY)
! VS11 ----- no. of categories for boundary conditions
      CALL ALREAD (2, VSD, PPPRI, ':VS11', 8, 1, 0, CDUM, IDUM, DUMMY)
      NVSWL = IDUM (1)
      NVSSP = IDUM (2)
      NVSLF = IDUM (3)
      NVSLH = IDUM (4)
      NVSLG = IDUM (5)
      NVSBF = IDUM (6)
      NVSBH = IDUM (7)


      NVSBD = IDUM (8)
! wells -----------------------------------------------
! VS12 ----- no. of wells

      IF (NVSWL.GT.0) THEN
         CALL ALREAD (2, VSD, PPPRI, ':VS12', 1, 1, 0, CDUM, IDUM, DUMMY)


         NW = IDUM (1)
! VS12a ---- element, category number, and target element
         CALL ALREAD (2, VSD, PPPRI, ':VS12a', 3, NW, 0, CDUM, IDUM, &
            DUMMY)
         DO 800 IW = 1, NW
            I0 = 3 * (IW - 1)
            IEL = IDUM (I0 + 1)
            NVSWLC (IEL) = MAX (1, IDUM (I0 + 2) )
            IWT = IDUM (I0 + 3)
            IF (IWT.GT.0) NVSWLT (IWT) = IEL
            NVSWLI (IEL) = IW


800      END DO
! VS12b ---- depth below ground of bottom and top of well screen
         CALL ALREAD (3, VSD, PPPRI, ':VS12b', 2, NW, 0, CDUM, IDUM, &
            DUMMY)
         DO 810 IW = 1, NW
            VSZWLB (IW) = DUMMY (2 * (IW - 1) + 1)

            VSZWLT (IW) = DUMMY (2 * (IW - 1) + 2)

810      END DO


      ENDIF
! springs ---------------------------------------------
! VS13 ----- no. of springs


      IF (NVSSP.GT.0) THEN
!c        CALL ALREAD(2, VSD, PRI, ':VS13', 1, 1, 0,
!c     -              CDUM, IDUM, DUMMY)
!c        NSP = IDUM(1)


         NSP = NVSSP
! VS13a ---- element and target element
         CALL ALREAD (2, VSD, PPPRI, ':VS13a', 2, NSP, 0, CDUM, IDUM, &
            DUMMY)
         DO 860 ISP = 1, NSP
            IEL = IDUM (2 * (ISP - 1) + 1)
            IF (IDUM (2 * (ISP - 1) + 2) .GT.0) NVSSPT (IDUM (2 * &
               (ISP - 1) + 2) ) = IEL


860      END DO
! VS13b ---- depth of spring source below ground, elevation of
!            discharge point, spring coefficient
         CALL ALREAD (3, VSD, PPPRI, ':VS13b', 3, NSP, 0, CDUM, IDUM1, DUMMY)
         DO 865 ISP = 1, NSP
            IEL = IDUM (2 * (ISP - 1) + 1)
            VSSPD (IEL) = DUMMY (3 * (ISP - 1) + 1)
            VSSPZ (IEL) = DUMMY (3 * (ISP - 1) + 2)
            VSSPCO (IEL) = DUMMY (3 * (ISP - 1) + 3)

865      END DO


      ENDIF
! lateral boundary conditions -------------------------
! VS14 ----- grid of codes (types)
      NDUM = MAX(NVSLF, NVSLH, NVSLG)

      IF (NDUM.GT.0) THEN
         CALL ALREAD (4, VSD, PPPRI, ':VS14', NX, NY, NDUM, CDUM, IDUM, &
            DUMMY)
         DO 920 IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO 910 IX = 1, NX
               IEL = ICMXY (IX, IY)
               IF (IEL.NE.0) NLBTYP (IEL) = IDUM (IXY0 + IX)
910         END DO


920      END DO
! VS15 ----- grid of category numbers
         CALL ALREAD (4, VSD, PPPRI, ':VS15', NX, NY, NDUM, CDUM, IDUM, &
            DUMMY)
         DO 940 IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO 930 IX = 1, NX
               IEL = ICMXY (IX, IY)
               IF (IEL.NE.0) NLBCAT (IEL) = MAX (1, IDUM (IXY0 + IX) )
930         END DO



940      END DO
! VS16 ----- No. of lateral boundary categories (flow, head, and head gr
! with b.c/s set only on selected layers
! initialise arrays to default values for reading in time-series data
         DO 840 ICAT = 1, NDUM
            NVSLFN (ICAT) = 0
            NVSLHN (ICAT) = 0
            NVSLGN (ICAT) = 0

840      END DO
         NVSLFT = NVSLF
         NVSLHT = NVSLH

         NVSLGT = NVSLG
         CALL ALREAD (2, VSD, PPPRI, ':VS16', 1, 1, 0, CDUM, IDUM, DUMMY)

         NLB = IDUM (1)


         DO 880 ILB = 1, NLB
! VS16a ---- b.c. type, category, no. of layers
            CALL ALREAD (2, VSD, PPPRI, ':VS16a', 3, 1, 0, CDUM, IDUM, &
               DUMMY)
            ITYP = IDUM (1)
            ICAT = IDUM (2)


            NLDUM = IDUM (3)
! VS16b ---- layer numbers
            CALL ALREAD (2, VSD, PPPRI, ':VS16b', NLDUM, 1, 0, CDUM, IDUM, &
               DUMMY)
            IF (ITYP.EQ.3) THEN
               NVSLFN (ICAT) = NLDUM
               NVSLFT = NVSLFT + NLDUM - 1
               DO 862 I = 1, NLDUM
                  NVSLFL (I, ICAT) = IDUM (I)
862            END DO
            ENDIF
            IF (ITYP.EQ.4) THEN
               NVSLHN (ICAT) = NLDUM
               NVSLHT = NVSLHT + NLDUM - 1
               DO 864 I = 1, NLDUM
                  NVSLHL (I, ICAT) = IDUM (I)
864            END DO
            ENDIF
            IF (ITYP.EQ.5) THEN
               NVSLGN (ICAT) = NLDUM
               NVSLGT = NVSLGT + NLDUM - 1
               DO 866 I = 1, NLDUM
                  NVSLGL (I, ICAT) = IDUM (I)
866            END DO

            ENDIF

880      END DO


      ENDIF
! bottom boundary conditions --------------------------
! VS17 ----- grid of codes (types)
      NDUM = MAX(NVSBF, NVSBH, NVSBD)

      IF (NDUM.GT.0) THEN

         IF (total_no_links.GT.0.AND.BEXBK) THEN
            CALL ALREAD (2, VSD, PPPRI, ':VS17', total_no_links, 1, 1, CDUM, IDUM, &
               DUMMY)
            DO 945 IEL = 1, total_no_links
               NBBTYP (IEL) = IDUM (IEL)
               NBBTYP (total_no_links + IEL) = IDUM (IEL)
               NBBTYP (2 * total_no_links + IEL) = IDUM (IEL)

945         END DO

         ENDIF
         CALL ALREAD (4, VSD, PPPRI, ':VS17', NX, NY, NDUM, CDUM, IDUM, &
            DUMMY)
         DO 960 IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO 950 IX = 1, NX
               IEL = ICMXY (IX, IY)
               IF (IEL.NE.0) NBBTYP (IEL) = IDUM (IXY0 + IX)
950         END DO


960      END DO
! VS18 ----- grid of category numbers

         IF (total_no_links.GT.0.AND.BEXBK) THEN
            CALL ALREAD (2, VSD, PPPRI, ':VS18', total_no_links, 1, 1, CDUM, IDUM, &
               DUMMY)
            DO 965 IEL = 1, total_no_links
               ICAT = MAX (1, IDUM (IEL) )
               NBBCAT (IEL) = ICAT
               NBBCAT (total_no_links + IEL) = ICAT
               NBBCAT (2 * total_no_links + IEL) = ICAT

965         END DO

         ENDIF
         CALL ALREAD (4, VSD, PPPRI, ':VS18', NX, NY, NDUM, CDUM, IDUM, &
            DUMMY)
         DO 980 IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO 970 IX = 1, NX
               IEL = ICMXY (IX, IY)
               IF (IEL.NE.0) NBBCAT (IEL) = MAX (1, IDUM (IXY0 + IX) )
970         END DO

980      END DO


      ENDIF
! FORMAT statements

9000  FORMAT('Error in number of VSS layer elements. ', &
      &       'NELEM should be ',I4)

9020  FORMAT('Error reading VSS layers for element ',I4, '.')

9030  FORMAT('Soil type ',I4,' not expected for soil property tables.')
9999  RETURN
!----------------------------------------------------------------------*
! Exit conditions:
! for each e in 1:NEL: 0<= NLBTYP(e), NBBTYP(e)
!                      1<= NLBCAT(e), NBBCAT(e), NVSWLC(e)
!----------------------------------------------------------------------*
   END SUBROUTINE VSREAD


END MODULE
