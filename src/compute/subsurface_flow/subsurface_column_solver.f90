MODULE subsurface_column_solver
!----------------------------------------------------------------------*
! Individual column flow calculations and matrix setup
! Contains VSCOLM, VSCOEF, VSINTC - column flow equation solvers
!----------------------------------------------------------------------*
! Extracted from VSmod.f90 as part of refactoring
! Date: 2025-09-04
! Source: VSmod.f90.sav (lines 540-824, 316-534, 2197-2320)
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE subsurface_variables
   USE subsurface_soil_properties, ONLY : VSFUNC, VSSPR
   USE subsurface_boundary_conditions, ONLY : VSUPPR, VSWELL, VSBC, VSSAI, VSLOWR
   USE AL_G, ONLY : ICMREF
   USE AL_C, ONLY : BHB, BFB, bexbk, DTUZ, deltaz, dummy, DHF, ESOILA, ERUZ, EEVAP, &
      FHBED, ISORT, jvsacn, JVSDEL, idum, icmbk, LFB, LHB, LINKNS, lgb, &
      NWELBT, NWELTP, NVSSPC, NVSWLI, NTSOIL, nhbed, NVC, NRD, nlyrbt, NVSWLT, NVSSPT, NBFACE, NS, nlyr, &
      PNETTO, QVSSPR, QVSBF, QH, QVSWEL, QBKF, QBKB, QVSV, QVSWLI, QVSH, QBKI, &
      tih, UZNEXT, &
      vsd, VSI, VSPSI, VSTHE, VSPOR, WLD, ZVSPSL, zlyrbt, zvsnod, zbeff, INITIALISE_AL_C, TIH
   USE AL_D, ONLY : TTH
   USE UTILSMOD, ONLY : TRIDAG, DCOPY
   IMPLICIT NONE

   PRIVATE
   ! Public column solver routines
   PUBLIC :: VSCOLM, VSCOEF, VSINTC

CONTAINS

!SSSSSS SUBROUTINE VSCOLM
   SUBROUTINE VSCOLM (EESN, CWV, CWL, VSK3D, BCHELE, ELEVEL, &
      IEL, ICBOT, ICTOP, ICBED, ICLYRB, ICSOIL, JCBC, JCDEL1, JELDUM, &
      JCACN, JCDEL, ICSPCE, ICLFN, ICLFL, ICWLBT, ICLHN, ICLHL, ICWLTP, &
      ICLGN, ICLGL, CA0, CZG, CZSP, CCS, CDELZ, CZ, CDELL, CAIJ, CAIJ1, &
      CDELL1, CZ1, DT, CDNET, CPSIN, CQ, CZS, CPSI1, CPSIN1, CKIJ1, &
      CQWIN, CLF, CLH, CLG, CBF, CBH, ICSTOR, CPSI, CKR, CTHETA, CQH, &
      CQV, CQWI, CQSP, CPSL, depadj)
!!!!!!! extra variable depadj passed for mods to vssai.f
! SPA, 03/11.98
!----------------------------------------------------------------------*
! Solves flow equations for a single colm
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSCOLM/4.2
! Modifications:
!  GP  29.07.94  written (v4.0 finished 17.07.96)
! RAH  961220  4.1  Remove commented lines.
!      961228       Arguments: add CWV,CWL; remove BUG.  IFA is local.
!                   Remove common CCCOLM, and CETAO, CKRO lines.
!                   VSCOEF arguments: remove IEL,NIT; add CWV,CWL.
!      970121       CEPSMX,NITMAX are constants.  Use DO 500 not GOTO.
!                   Use generic intrinsics.  Remove redundant ICPSL.
!                   Extend arg-list (& remove redundancies) for VSFUNC.
!      970122       Extend VSCOEF argument list.
!      970123       Make VSCOEF output arguments, and CETA,CDETA,CDKR,
!                   local; also eliminate some arguments (see VSCOEF),
!                   including CBETP (use CBETM(ICL+1) instead).
!      970126       Full argument list for VSINTC, and make CA,CC local.
!      970127       Full argument lists for VSUPPR,VSWELL,VSSPR,VSBC.
!      970131       Full argument list for VSLOWR, & unconditional call.
!                   Remove redundant I1.  Amend some comments.
!      970203       Full argument list for VSSAI, and reposition call.
!                   Replace input CV with CA0,CDELZ.  CDPSI,CB,CR local.
!                   Replace output CQINF with CQV(ICTOP).
!                   Pass CA0 to VSWELL (see VSWELL). Simplify CPSL code.
!                   Add CGAM2 term to CQH unconditionally.
!      970207       Remove VSWELL output CQW.
!      970210       Remove output argument NITC and common CQBK*.
!                   Move inputs BCHELE,CA0,CZG,DT,CPSIN and outputs
!                   CQSP,CPSL from VSCOLM.INC to argument list.
!                   Move input SIGMA to VSINTC.  Initialize CQH.
!      970513       Use VSK3D(ICSOIL(ICL),?) for CKIJS(ICL,?),CKZS(ICL).
!                   Swap indices: CAIJ, CQH, JCACN, & JCDEL.
!                   Use arguments in place of VSCOLM.INC.
!      970514       VSBC args: remove DWORK2; add IFA - also to VSSAI.
!                   VSUPPR args: replace CDW,CEW,CQP with CDNET.
!                   VSWELL args: reorder.  Don't initialize CQH.
!                   New local DPSI.  No block-IF in setting CPSL.
!      970515       Re-order arguments.
! RAH  980402  4.2  Replace local ERR with new arg ELEVEL (see VSSIM).
!JE   JAN 2009      Loop restructure for AD
!----------------------------------------------------------------------*
! Entry conditions:
! 1 <= ICBOT <= ICSPCE, ICWLBT, ICWLTP <= ICTOP < LLEE
!     ICWLBT <= ICWLTP
! for each j in 1:4: JCBC(j)  =   0,3,4,5,9 or 10
!               3 <= JCBC(j) <= 5  ==>  JELDUM(j) = 0
!                    JCBC(j)  = 9  ==>  JCACN(j,ICBOT:ICTOP) = 0
!                    JCBC(j)  = 10 ==>  JCACN(j,ICBED+1:ICTOP) = 0
! the following are static functions of IEL:
!     ICBED,ICBOT,ICLFL,ICLFN,ICLHL,ICLHN,ICLYRB,ICTOP,JCACN,JCBC,JELDUM
!----------------------------------------------------------------------*
! Limited ranges:
!              CQWI, CQWIN: only if JCBC(5)=1
! ICSPCE, CCS, CQSP,  CZSP: only if JCBC(5)=2
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P.VSS:        LLEE,NLYREE,NSEE
!     VSSOIL.INC:      NSOLEE
! Input common
!     VSSOIL.INC:      NVSSOL
!                      VSPPSI(NVSSOL),   VSPTHE(NSOLEE,NS)
!                       VSPKR(NSOLEE,NS),VSPETA(NSOLEE,NS)
!                      VSPDKR(NSOLEE,NS),VSPDET(NSOLEE,NS)
! Input arguments
      INTEGER :: EESN, ELEVEL, IEL, ICBOT, ICTOP, ICBED
      INTEGER :: ICSPCE, ICWLBT, ICWLTP, ICLFN, ICLHN, ICLGN
      INTEGER :: ICLYRB (NLYREE), ICSOIL (ICBOT:ICTOP), JCBC (0:5)
      INTEGER :: ICLFL (NLYREE), JCACN (4, ICBOT:ICTOP), JELDUM (4)
      INTEGER :: ICLHL (NLYREE), JCDEL (4, ICBOT:ICTOP)
      INTEGER :: ICLGL (NLYREE), JCDEL1 (LLEE, 4)
      DOUBLEPRECISION CWV, CWL, CA0, CZG, CZSP, CCS
      DOUBLEPRECISION VSK3D (NSEE, 3), CDELZ (ICBOT:ICTOP), CDELL (4)
      DOUBLEPRECISION CAIJ1 (LLEE, 4), CZ (ICBOT:ICTOP), CDELL1 (4)
      DOUBLEPRECISION CZ1 (LLEE, 4), CAIJ (4, ICBOT:ICTOP)
      DOUBLEPRECISION DT, CDNET, CQWIN, CBF, CBH
      DOUBLEPRECISION CPSI1 (LLEE, 4), CPSIN (ICBOT:ICTOP), CLF (NLYREE)
      DOUBLEPRECISION CPSIN1 (LLEE, 4), CQ (ICBOT:ICTOP), CLH (NLYREE)
      DOUBLEPRECISION CKIJ1 (LLEE, 4), CZS (4), CLG (NLYREE)
!!!!! depadj for fix to vssai, SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      DOUBLEPRECISION depadj (4)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      LOGICAL :: BCHELE
! In+out arguments
      INTEGER :: ICSTOR (ICBOT:ICTOP)

      DOUBLEPRECISION CPSI (ICBOT:ICTOP)
! Output arguments
      DOUBLEPRECISION CTHETA (ICBOT:ICTOP), CQV (ICBOT - 1:ICTOP)
      DOUBLEPRECISION CKR (ICBOT:ICTOP), CQH (4, ICBOT:ICTOP)

      DOUBLEPRECISION CQWI (ICWLBT:ICWLTP), CQSP, CPSL
! Locals, etc
!INTRINSIC ABS, MAX
      INTEGER :: NITMAX
      DOUBLEPRECISION CEPSMX
      PARAMETER (NITMAX = 100, CEPSMX = 1D-4)
      INTEGER :: BTYPE, I, ICL, IFA, J, K, K1, NDUM, NIT, PCL, SOIL
      DOUBLEPRECISION CPSMIN, DPSI, DPSIMX, H0, H1, H2
      DOUBLEPRECISION DWORK1 (1 + LLEE+NLYREE), DWORK2 (LLEE)
      DOUBLEPRECISION CETA (LLEE), CDETA (LLEE), CDKR (LLEE)
      DOUBLEPRECISION CBETM (LLEE), CDBETM (LLEE), CDBTMM (LLEE)
      DOUBLEPRECISION CF (LLEE), CDF (LLEE), CKIJ (LLEE, 4), CDKIJ ( LLEE, 4)
      DOUBLEPRECISION CGAM1 (LLEE, 4), CDGAM1 (LLEE, 4)
      DOUBLEPRECISION CGAM2 (LLEE, 4), CDGAM2 (LLEE, 4)
      DOUBLEPRECISION CA (LLEE), CB (LLEE), CC (LLEE), CR (LLEE), CDPSI (LLEE)
      LOGICAL :: g510
      integer,save :: errorcount=0
!----------------------------------------------------------------------*
! Initialization
!________________*


      NDUM = ICTOP - ICBOT + 1
! Main iteration loop (calculations within depend upon CPSI)
!____________________________________________________________*

      g510=.FALSE.
      OUT500 : DO NIT = 1, NITMAX
         IF(g510) CYCLE
! update soil properties from previous iteration


         CALL VSFUNC (NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
            VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ICSOIL, CPSI, &
            ICSTOR, CTHETA, CETA (ICBOT), CKR, CDETA (ICBOT), CDKR (ICBOT) &
            )
! set up intermediate coefficients


         CALL VSCOEF (LLEE, NSEE, CWV, CWL, VSK3D, ICBOT, ICTOP, JELDUM, &
            JCBC (1), ICSOIL, JCACN, JCDEL, JCDEL1, CA0, CDELL, CDELL1, &
            CDELZ, CAIJ, CAIJ1, CKR, CDKR (ICBOT), CKIJ1, CBETM (ICBOT), &
            CDBETM (ICBOT), CDBTMM (ICBOT), CF (ICBOT), CDF (ICBOT), &
            CKIJ, CDKIJ, CGAM1, CGAM2, CDGAM1, CDGAM2, DWORK1, DWORK2)
! prepare basic coefficients for tri-diagonal solver ("internal" cells)


         CALL VSINTC (LLEE, ICBOT, ICTOP, JELDUM, JCBC (1), JCACN, &
            JCDEL1, CA0, CDELZ, CZ, CZ1, DT, CETA (ICBOT), CDETA (ICBOT), &
            CQ, CPSI, CPSIN, CF (ICBOT), CDF (ICBOT), CBETM (ICBOT), &
            CDBETM (ICBOT), CDBTMM (ICBOT), CPSI1, CPSIN1, CGAM1, CGAM2, &
            CDGAM1, CDGAM2, CA (ICBOT), CB (ICBOT), CC (ICBOT), CR (ICBOT), &
            DWORK1)
! add top boundary condition
         SOIL = ICSOIL (ICTOP)


         CALL VSUPPR (CA0, CDELZ (ICTOP), VSK3D (SOIL, 3), DT, CDNET, &
            CPSI (ICTOP), CB (ICTOP), CR (ICTOP), CQV (ICTOP) )
! add well abstraction (type 1)
         BTYPE = JCBC (5)
         IF (BTYPE.EQ.1) THEN


            CALL VSWELL (NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL (ICWLBT), &
               CA0, CDELZ (ICWLBT), CQWIN, CPSI (ICWLBT), CR (ICWLBT), &
               CQWI, DWORK1)
! add spring discharge (type 2)
         ELSEIF (BTYPE.EQ.2) THEN
            CALL VSSPR (CZ (ICSPCE), CZSP, CCS, CPSI (ICSPCE), CKR ( &
               ICSPCE), CDKR (ICSPCE), CB (ICSPCE), CR (ICSPCE), CQSP)


         ENDIF
! add user-defined lateral boundary conditions (types 3-5)
         DO 20 IFA = 1, 4
            BTYPE = JCBC (IFA)
            IF (BTYPE.GE.3.AND.BTYPE.LE.5) THEN


               CALL VSBC (BCHELE, IFA, ICBOT, ICTOP, JCBC (IFA), &
                  ICLYRB, ICLFN, ICLFL, ICLHN, ICLHL, CZG, CDELL (IFA), &
                  CDELZ, CZ, CAIJ, CLF, CLH, CPSI, CKIJ (ICBOT, IFA), &
                  CDKIJ (ICBOT, IFA), CB (ICBOT), CR (ICBOT), CQH, DWORK1)
! add stream-aquifer interaction (types 9 and 10)
            ELSEIF (BTYPE.EQ.9.OR.BTYPE.EQ.10) THEN
               CALL VSSAI (IFA, JCBC (IFA), ICBOT, ICTOP, ICBED, CDELL ( &
                  IFA), CZ, CAIJ, CZS (IFA), CPSI, CKIJ (ICBOT, IFA), &
                  CDKIJ (ICBOT, IFA), CB (ICBOT), CR (ICBOT), CQH, depadj ( &
                  ifa), cdelz)
!!!!!! added depadj to call to vssai, SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            ENDIF


20       END DO
! add lower boundary condition (types 6-8)
         SOIL = ICSOIL (ICBOT)
!         CALL VSLOWR(JCBC(0),CA0,CZ,CDELZ,VSK3D(SOIL,3),
!     $               CBF,CBH,CPSI,CKR(ICBOT),CDKR(ICBOT),
!     $            CB(ICBOT),CR(ICBOT), CQV)



         CALL VSLOWR (JCBC (0), CA0, CZ (icbot), CDELZ (icbot), VSK3D ( &
            SOIL, 3), CBF, CBH, CPSI (icbot), CKR (ICBOT), CDKR (ICBOT), &
            CB (ICBOT), CR (ICBOT), CQV (icbot - 1) )
! solve linear equations


         CALL TRIDAG (CA (ICBOT:ICTOP), CB (ICBOT:ICTOP), CC (ICBOT:ICTOP), CR (ICBOT:ICTOP), &
            CDPSI (ICBOT:ICTOP), NDUM)
! update PSI array and check for convergence
         DPSIMX = ZERO
         DO 100 ICL = ICBOT, ICTOP
            DPSI = CDPSI (ICL)
            CPSI (ICL) = CPSI (ICL) + DPSI
            DPSIMX = MAX (DPSIMX, ABS (DPSI) )
100      END DO

         IF (DPSIMX.LE.CEPSMX) g510=.TRUE. !GOTO 510
!                              >>>>>>>>
      ENDDO out500

!    write (789,*), uznow, cqwin,cqwi(2),cr(2)


      IF (ELEVEL.GT.0) then
         errorcount=errorcount+1
         if (errorcount.lt.errcntallowed) then
            CALL ERROR (ELEVEL, 1036, PPPRI, IEL, 0, 'Maximum iterations in VSS column solver')
         elseif (errorcount.eq.errcntallowed) then
            CALL ERROR (ELEVEL, 1036, PPPRI, IEL, 0, '**** Last printout of the error message - maximum iterations error in VSS column solver *****')
         endif
      endif





510   CONTINUE
! Calculate final values of output variables
!____________________________________________*
! flows
      DO 600 ICL = ICBOT, ICTOP - 1
         PCL = ICL + 1
         CQV (ICL) = CBETM (PCL) * (CZ (ICL) + CPSI (ICL) - CZ (PCL) &
            - CPSI (PCL) ) / CA0

600   END DO
      DO 650 J = 1, 4
         IF (JELDUM (J) .LT.1) GOTO 650
!                            >>>>>>>>

         DO 640 I = ICBOT, ICTOP
            K = JCACN (J, I)
            IF (K.LT.1) GOTO 640
!                       >>>>>>>>
            K1 = K + JCDEL1 (K, J)
            H0 = CZ (I) + CPSI (I)
            H1 = CZ1 (K, J) + CPSI1 (K, J)

            H2 = CZ1 (K1, J) + CPSI1 (K1, J)

            CQH (J, I) = CGAM1 (I, J) * (H1 - H0) + CGAM2 (I, J) &
               * (H2 - H0)
640      END DO


650   END DO
! phreatic surface level
      CPSMIN = CZ (ICBOT) - half * CDELZ (ICBOT)
      DO ICL = ICBOT, ICTOP
         IF (CPSI (ICL) .LT.ZERO) GOTO 940
      END DO
940   ICL = MAX (ICBOT, ICL - 1)

      CPSL = MAX (CPSMIN, CZ (ICL) + CPSI (ICL) )
   END SUBROUTINE VSCOLM

!SSSSSS SUBROUTINE VSCOEF (LLEE, NSEE, CWV, CWL, VSK3D, ICBOT, ICTOP, &
   SUBROUTINE VSCOEF (LLEE, NSEE, CWV, CWL, VSK3D, ICBOT, ICTOP, &
      JELDUM, JCBC, ICSOIL, JCACN, JCDEL, JCDEL1, CA0, CDELL, CDELL1, &
      CDELZ, CAIJ, CAIJ1, CKR, CDKR, CKIJ1, CBETM, CDBETM, CDBTMM, CF, &
      CDF, CKIJ, CDKIJ, CGAM1, CGAM2, CDGAM1, CDGAM2, C, D)
!----------------------------------------------------------------------*
! Sets up coefficients for column internal cells
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSCOEF/4.1
! Modifications:
!  GP  22.08.94  written (v4.0 finished 20.12.95)
! RAH  961228  4.1  No leading comments.  Remove arguments IEL & NIT.
!                   Add arguments CWV,CWL (were in VSCOLM.INC).
!      970115       Dispense with VSCOLM.INC arrays CKZ,CDKZ.
!                   Rewrite "vertical" sections: use fewer operations;
!                   use DCOPY; don't overwrite CDELZ.
!      970116       Rewrite "lateral" sections similarly, and fix error
!                   in CDGAM* when CWL.NE.1.  No lower-case code.
!      970122       Use arguments, not COMMON.
!      970123       Scrap output CBETP,CDBETP,CDBTPP,CDFM,CDFP,CG,CDG.
!      970513       Swap indices: JCACN, JCDEL & CAIJ. Rename local DUM.
!                   New args NSEE,ICSOIL,VSK3D in place of CKZS,CKIJS.
!----------------------------------------------------------------------*
! Entry conditions:
! 1 <= ICBOT <= ICTOP <= LLEE
! 0 <  CWL, CA0, NSEE
! for each i in ICBOT:ICTOP:
!    1 <= ICSOIL(i) <= NSEE
!    0 <   CDELZ(i), CKR(i), VSK3D(ICSOIL(i),1:3)
!    for each j in 1:4 such that
!                      JELDUM(j)>0 and JCACN(j,i).ne.0 and JCBC(j).ne.9:
!       1 <= k, k1 <= LLEE
!       1 >= |JCDEL(j,i)|, |JCDEL1(k,j)|
!       0 <  CAIJ(j,i), CAIJ1(k,j), CAIJ1(k1,j), CKIJ1(k,j), CKIJ1(k1,j)
!    where k=JCACN(j,i), and k1=k+JCDEL1(k,j)
! for each j in 1:4: 0 < CDELL(j) + CDELL1(j)
!----------------------------------------------------------------------*
! Input arguments
      INTEGER :: LLEE, NSEE, ICBOT, ICTOP, JELDUM (4), JCBC (4)
      INTEGER :: ICSOIL (ICBOT:ICTOP), JCACN (4, ICBOT:ICTOP)
      INTEGER :: JCDEL1 (LLEE, 4), JCDEL (4, ICBOT:ICTOP)
      DOUBLEPRECISION CWV, CWL, VSK3D (NSEE, 3)
      DOUBLEPRECISION CA0, CDELL (4), CDELZ (ICBOT:ICTOP)
      DOUBLEPRECISION CDELL1 (4), CAIJ (4, ICBOT:ICTOP), CAIJ1 (LLEE, 4)

      DOUBLEPRECISION CKR (ICBOT:ICTOP), CDKR (ICBOT:ICTOP), CKIJ1 ( &
         LLEE, 4)
! Output arguments
      DOUBLEPRECISION CBETM (ICBOT:ICTOP + 1)
      DOUBLEPRECISION CDBETM (ICBOT:ICTOP + 1), CDBTMM (ICBOT:ICTOP + 1)
      DOUBLEPRECISION CF (ICBOT:ICTOP), CDF (ICBOT:ICTOP)
      DOUBLEPRECISION CKIJ (LLEE, 4), CDKIJ (LLEE, 4), CGAM1 (LLEE, 4)

      DOUBLEPRECISION CDGAM1 (LLEE, 4), CGAM2 (LLEE, 4), CDGAM2 (LLEE, &
         4)
! Workspace arguments

      DOUBLEPRECISION C (ICBOT:ICTOP), D (ICBOT:ICTOP)
! Locals, etc
!INTRINSIC ABS, MOD
      INTEGER :: DELKJ, I, J, K, K1, M, NIJ, NKJ, NKJM1, P
      DOUBLEPRECISION AIJDUM, AREA2, C1, C2, CAVE, CI, CIJ, CKJ, CK1J, &
         CM, Casum
      DOUBLEPRECISION D1, D2, DIJ, AODZ, KSAODZ, DXDUM, RCI, RCM, WI, &
         WIM1, WO2DX
      DOUBLEPRECISION KIJ, DKIJ, GAM1, GAM2, DGAM1, DGAM2, CKIJS, CKZS



      LOGICAL :: TEST
!----------------------------------------------------------------------*
! vertical conductivity terms (CBETM,CDB*)
      CBETM (ICBOT) = zero
      CDBETM (ICBOT) = zero

      CDBTMM (ICBOT) = zero

      IF (ISZERO(CWV)) THEN
!        ! Special case: weighted harmonic mean
         AREA2 = CA0 * 2D0
         DO 100 I = ICBOT, ICTOP
            CKZS = VSK3D (ICSOIL (I), 3)
            KSAODZ = CKZS * AREA2 / CDELZ (I)
            C (I) = CKR (I) * KSAODZ
            D (I) = CDKR (I) * KSAODZ
100      END DO
         DO 200 I = ICBOT + 1, ICTOP
            M = I - 1
            CM = C (M)
            CI = C (I)
            Casum = CM + CI
            RCM = CM / Casum
            RCI = CI / Casum
            CBETM (I) = CI * RCM
            CDBETM (I) = D (I) * RCM**2
            CDBTMM (I) = D (M) * RCI**2

200      END DO

      ELSEIF (ISONE(CWV)) THEN
!        * Arithmetic mean
         DO 203 I = ICBOT, ICTOP
            CKZS = VSK3D (ICSOIL (I), 3)
            C (I) = CKR (I) * CKZS
            D (I) = CDKR (I) * CKZS
203      END DO
         DO 205 I = ICBOT + 1, ICTOP
            M = I - 1
            AODZ = CA0 / (CDELZ (M) + CDELZ (I) )
            CBETM (I) = AODZ * (C (M) + C (I) )
            CDBETM (I) = AODZ * D (I)
            CDBTMM (I) = AODZ * D (M)

205      END DO

      ELSE
!        * General w-mean
         WI = one / CWV
         WIM1 = (one - CWV) / CWV
         DO 208 I = ICBOT, ICTOP
            CKZS = VSK3D (ICSOIL (I), 3)
            C (I) = (CKR (I) * CKZS) **CWV
            D (I) = CDKR (I) * CKZS
208      END DO
         DO 210 I = ICBOT + 1, ICTOP
            M = I - 1
            CM = C (M)
            CI = C (I)
            CAVE = .5D0 * (CM + CI)
            AODZ = CA0 / (CDELZ (M) + CDELZ (I) )
            CBETM (I) = AODZ * CAVE**WI * 2D0
            CDBETM (I) = AODZ * (CAVE / CI) **WIM1 * D (I)
            CDBTMM (I) = AODZ * (CAVE / CM) **WIM1 * D (M)

210      END DO

      ENDIF
      I = ICTOP + 1
      CBETM (I) = zero
      CDBETM (I) = zero


      CDBTMM (I) = zero
! vertical components of coefficients  NB lateral components added later
      DO 220 I = ICBOT, ICTOP
         P = I + 1
         CF (I) = CBETM (I) + CBETM (P)
         CDF (I) = CDBETM (I) + CDBTMM (P)


220   END DO
! loop over each face
      WI = one / CWL
      WIM1 = (one - CWL) / CWL

      DO 400 J = 1, 4
         M = 1 + MOD (J - 1, 2)
         TEST = JELDUM (J) .LT.1.OR.JCBC (J) .EQ.9
         DXDUM = CDELL (J) + CDELL1 (J)
         WO2DX = half * CWL / DXDUM


         DO 300 I = ICBOT, ICTOP
! lateral conductivity terms
            CKIJS = VSK3D (ICSOIL (I), M)
            KIJ = CKR (I) * CKIJS
            DKIJ = CDKR (I) * CKIJS
            CKIJ (I, J) = KIJ


            CDKIJ (I, J) = DKIJ
! lateral components of all coefficients
            K = JCACN (J, I)
            IF (K.EQ.0.OR.TEST) GOTO 300
!                                   >>>>>>>>
            NIJ = ABS (JCDEL (J, I) ) + 1
            DELKJ = JCDEL1 (K, J)
            K1 = K + DELKJ
            NKJM1 = ABS (DELKJ)

            NKJ = NKJM1 + 1
            CKJ = CKIJ1 (K, J) * CAIJ1 (K, J) / NIJ
            CK1J = CKIJ1 (K1, J) * CAIJ1 (K1, J) / NIJ
            AIJDUM = CAIJ (J, I) / NKJ
            DIJ = DKIJ * AIJDUM * WO2DX

            CIJ = KIJ * AIJDUM
            C1 = half * (CIJ + CKJ)
            C2 = half * (CIJ + CK1J)
            D1 = one

            D2 = one
            IF (NOTONE(CWL)) THEN
               CIJ = CIJ**CWL
               CKJ = CKJ**CWL
               CK1J = CK1J**CWL
               D1 = (C1 / CIJ) **WIM1
               D2 = (C2 / CIJ) **WIM1
               C1 = C1**WI
               C2 = C2**WI

            ENDIF
            GAM1 = C1 / DXDUM
            GAM2 = C2 / DXDUM * NKJM1
            DGAM1 = D1 * DIJ

            DGAM2 = D2 * DIJ * NKJM1
            CGAM1 (I, J) = GAM1
            CGAM2 (I, J) = GAM2
            CDGAM1 (I, J) = DGAM1
            CDGAM2 (I, J) = DGAM2
            CF (I) = CF (I) + GAM1 + GAM2

            CDF (I) = CDF (I) + DGAM1 + DGAM2

300      END DO

400   END DO
   END SUBROUTINE VSCOEF

!SSSSSS SUBROUTINE VSINTC (LLEE, ICBOT, ICTOP, JELDUM, JCBC, JCACN, &
   SUBROUTINE VSINTC (LLEE, ICBOT, ICTOP, JELDUM, JCBC, JCACN, &
      JCDEL1, CA0, CDELZ, CZ, CZ1, DT, CETA, CDETA, CQ, CPSI, CPSIN, CF, &
      CDF, CBETM, CDBETM, CDBTMM, CPSI1, CPSIN1, CGAM1, CGAM2, CDGAM1, &
      CDGAM2, CA, CB, CC, CR, H)
!----------------------------------------------------------------------*
! Sets up coefficients for column internal cells
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSINTC/4.1
! Modifications:
!  GP  20.08.94  written (v4.0 finished 22.06.95)
! RAH  970120  4.1  Rewrite with fewer operations, and without
!                   overwriting input arrays.
!      970126       Dispense with input IEL,CB*P,CD*P,CDFM,C*G.
!                   Use arguments, not COMMON.
!      970203       Replace input CV with CA0,CDELZ.
!      970210       Make input SIGMA local.
!      970514       CQ is now pre-multiplied by CA0*CDELZ (see VSSIM).
!                   Swap JCACN indices.
!----------------------------------------------------------------------*
! Entry conditions:
! 1 <= ICBOT <= ICTOP <= LLEE
! 0 <  DT
! for each j such that JELDUM(j)>0 and JCBC(j).ne.9:
!    for each i such that JCACN(j,i).ne.0: 1 <= k, k1 <= LLEE
!    where k=JCACN(j,i) and k1=k+JCDEL1(k,j)
!----------------------------------------------------------------------*
! Input arguments
      INTEGER :: LLEE, ICBOT, ICTOP, JELDUM (4), JCBC (4)
      INTEGER :: JCACN (4, ICBOT:ICTOP), JCDEL1 (LLEE, 4)
      DOUBLEPRECISION CA0, CZ1 (LLEE, 4)
      DOUBLEPRECISION CDELZ (ICBOT:ICTOP), CZ (ICBOT:ICTOP)
      DOUBLEPRECISION CETA (ICBOT:ICTOP), DT, CDETA (ICBOT:ICTOP)
      DOUBLEPRECISION CPSI (ICBOT:ICTOP), CPSIN (ICBOT:ICTOP)
      DOUBLEPRECISION CF (ICBOT:ICTOP), CDF (ICBOT:ICTOP)
      DOUBLEPRECISION CQ (ICBOT:ICTOP), CBETM (ICBOT:ICTOP + 1)
      DOUBLEPRECISION CDBETM (ICBOT:ICTOP + 1), CDBTMM (ICBOT:ICTOP + 1)
      DOUBLEPRECISION CPSI1 (LLEE, 4), CPSIN1 (LLEE, 4), CGAM1 (LLEE, 4)

      DOUBLEPRECISION CDGAM1 (LLEE, 4), CDGAM2 (LLEE, 4), CGAM2 (LLEE, &
         4)
! Output arguments
      DOUBLEPRECISION CA (ICBOT:ICTOP), CB (ICBOT:ICTOP), CC (ICBOT: &
         ICTOP)

      DOUBLEPRECISION CR (ICBOT:ICTOP)
! Workspace arguments

      DOUBLEPRECISION H (ICBOT - 1:ICTOP + 1)
! Locals, etc
      DOUBLEPRECISION SIGMA, OMSIG
      PARAMETER (SIGMA = 1D0, OMSIG = 1D0 - SIGMA)
      INTEGER :: I, J, K, K1, P
      DOUBLEPRECISION CBETMI, CBETPI, CDBETP, CDBMMI, CDBTPP, CDFM, &
         CDFP, CDG



      DOUBLEPRECISION CFI, CGI, DPSI, HI, HK, HK1, HM, HP, VODT
!----------------------------------------------------------------------*
! Prepare effective hydraulic heads
      I = ICBOT - 1
      H (I) = zero
      DO 100 I = ICBOT, ICTOP
         H (I) = SIGMA * CPSI (I) + OMSIG * CPSIN (I) + CZ (I)
100   END DO


      H (I) = zero
! Set coefficients, omitting lateral terms
      DO 200 I = ICBOT, ICTOP

         P = I + 1
         HM = H (I - 1)
         HI = H (I)
         HP = H (P)
         CFI = CF (I)
         CBETMI = CBETM (I)
         CBETPI = CBETM (P)
         CDBTPP = CDBETM (P)
         CDBMMI = CDBTMM (I)
         CDBETP = CDBTMM (P)
         CDFM = CDBMMI

         CDFP = CDBTPP
         VODT = CDELZ (I) * CA0 / DT
         CGI = CETA (I) * VODT
         CDG = CDETA (I) * VODT

         DPSI = CPSI (I) - CPSIN (I)
         CA (I) = SIGMA * CBETMI - HI * CDFM + HM * CDBMMI
         CC (I) = SIGMA * CBETPI - HI * CDFP + HP * CDBTPP
         CB (I) = HM * CDBETM (I) - HI * CDF (I) + HP * CDBETP - &
            (SIGMA * CFI + DPSI * CDG + CGI)

         CR (I) = - (HM * CBETMI - HI * CFI + HP * CBETPI - DPSI * CGI + &
            CQ (I) )


200   END DO
! Add lateral terms

      DO 400 J = 1, 4

         IF (JELDUM (J) .LT.1.OR.JCBC (J) .EQ.9) GOTO 400

         DO 300 I = ICBOT, ICTOP
            K = JCACN (J, I)
            IF (K.EQ.0) GOTO 300

            K1 = JCDEL1 (K, J) + K
            HK = SIGMA * CPSI1 (K, J) + OMSIG * CPSIN1 (K, J) + CZ1 (K, &
               J)

            HK1 = SIGMA * CPSI1 (K1, J) + OMSIG * CPSIN1 (K1, J) &
               + CZ1 (K1, J)
            CB (I) = CB (I) + HK * CDGAM1 (I, J) + HK1 * CDGAM2 (I, J)

            CR (I) = CR (I) - HK * CGAM1 (I, J) - HK1 * CGAM2 (I, J)

300      END DO

400   END DO
   END SUBROUTINE VSINTC


END MODULE
