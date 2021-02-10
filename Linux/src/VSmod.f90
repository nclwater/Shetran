MODULE VSmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the VS .F files
USE SGLOBAL
USE mod_load_filedata, ONLY : ALINIT, ALSPRD, ALREAD
!USE SGLOBAL,  ONLY : 
USE AL_G, ONLY : ICMREF, NX, NY, ICMXY, NGDBGN
USE AL_C, ONLY : BHB, BFB, bexbk, DTUZ, deltaz, dummy, DHF, ESOILA, ERUZ, EEVAP, &
                 FHBED, ISORT, jvsacn, JVSDEL, idum, icmbk, LFB, LHB, LINKNS, lgb, &
                 NWELBT, NWELTP, NVSSPC, NVSWLI, NTSOIL, nhbed, NVC, NRD, nlyrbt, NVSWLT, NVSSPT, NBFACE, NS, nlyr, &
                 PNETTO, QVSSPR, QVSBF, QH, QVSWEL, QBKF, QBKB, QVSV, QVSWLI, QVSH, QBKI, &
                 tih, UZNEXT, &
                 vsd, VSI, VSPSI, VSTHE, VSPOR, WLD, ZVSPSL, zlyrbt, zvsnod, zbeff, INITIALISE_AL_C, TIH
USE AL_D, ONLY : TTH
!USE VSINIT_INC  
!USE VSCOM1_INC  
!USE VSSOIL_INC  
USE UTILSMOD, ONLY : TRIDAG, FINPUT, HINPUT, DCOPY
USE OCmod2,   ONLY : GETHRF
IMPLICIT NONE
!ALL THESE WERE SAVE VARAIABLES - MOVED HERE FOR AD
INTEGER         :: ICSOILsv(LLEE,NELEE), JCBCsv(0:5,NELEE)
!DOUBLEPRECISION :: VSAIJsv(4,LLEE,NELEE)
DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: VSAIJsv

DOUBLEPRECISION :: WLLAST=zero, WLTIME=zero, RWELIN(NVSEE)=zero  
DOUBLEPRECISION :: RLFLST=zero, RLFTIM=zero, RLFPRV(NVSEE)=zero  
DOUBLEPRECISION :: RLHLST=zero, RLHTIM=zero, RLHPRV(NVSEE)=zero, RLHNXT(NVSEE)=zero  
DOUBLEPRECISION :: RLGLST=zero, RLGTIM=zero, RLGPRV(NVSEE)=zero, RLGNXT(NVSEE)=zero
DOUBLEPRECISION :: RBFLST=zero, RBFTIM=zero, RBFPRV(NVSEE)=zero
DOUBLEPRECISION :: RBHLST=zero, RBHTIM=zero, RBHPRV(NVSEE)=zero, RBHNXT(NVSEE)=zero  
DOUBLEPRECISION :: RLFDUM(NVSEE)=zero, RLHDUM(NVSEE)=zero, RLGDUM(NVSEE)=zero
LOGICAL :: FIRSTvssim=.TRUE.
integer,parameter :: errcntallowed=1000

!IMPORTED FROM  MODULE vscom1_inc
!MODULE vscom1_inc
!------------------- Start of VSCOM1.INC ------------------------------*
!
!  common block for global VSS variables
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/VSCOM1.INC/4.2
! Modifications:
!  GP  15.04.94  Created.  v4.0 completed 950808.
! RAH  970210  4.1  Remove VSPSIN,VSTHEN (VSSIM).
!      970213       Reverse subscripts: NVSLFL,NVSLHL,NVSLGL,RLFNOW,
!                   RLHNOW,RLGNOW (see VSSIM; also VSPREP,VSREAD).
!      970218       Swap IVSSTO,VSKR subscripts, and remove VSETAN,VSKRN
!                   (see VSIN,VSSIM).
! RAH  980308  4.2  Amend history.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*

! Imported constants
!                      LLEE,NELEE,NLFEE,NLYREE,NSEE,NVSEE

! logical variables, initialisation
!USE SGLOBAL, ONLY : NELEE, NLFEE, NLYREE, NVSEE, LLEE, NSEE
!IMPLICIT NONE
LOGICAL :: BLOWP, BHELEV  

!COMMON / VSC1LI / BLOWP, BHELEV  
! integer variables, initialisation
INTEGER :: NCSZON, NCRBED  
INTEGER :: JVSALN (NELEE, NLYREE, 4), ISRBED (NLFEE)  
INTEGER :: NVSWL, NVSSP, NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD  
INTEGER :: NVSWLC (NELEE), NLBTYP (NELEE)  
INTEGER :: NLBCAT (NELEE), NBBTYP (NELEE), NBBCAT (NELEE)  
INTEGER :: NVSLFT, NVSLFL (NLYREE, NVSEE), NVSLFN (NVSEE)  
INTEGER :: NVSLHT, NVSLHL (NLYREE, NVSEE), NVSLHN (NVSEE)  
INTEGER :: NVSLGT, NVSLGL (NLYREE, NVSEE), NVSLGN (NVSEE)  

!COMMON / VSC1II / NCSZON, NCRBED, JVSALN, ISRBED, NVSWL, NVSSP, &
 !NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD, NVSWLC, NLBTYP, NLBCAT, &
 !NBBTYP, NBBCAT, NVSLFT, NVSLFL, NVSLFN, NVSLHT, NVSLHL, NVSLHN, &
 !NVSLGT, NVSLGL, NVSLGN
! integer variables, time-varying
INTEGER :: IVSSTO (LLEE, NELEE)  

!COMMON / VSC1IT / IVSSTO  
! floating-point variables and arrays, initialisation
DOUBLEPRECISION DCSZON (LLEE), DCRBED (LLEE), DCSTOT, DCRTOT, &
 VSZMIN, VSZMAX, VSK3D (NSEE, 3), DRBED (NLFEE), VSSPZ (NELEE), &
 VSSPCO (NELEE), VSWV, VSWL

!COMMON / VSC1RI / DCSZON, DCRBED, DCSTOT, DCRTOT, VSZMIN, VSZMAX, &
 !VSK3D, DRBED, VSSPZ, VSSPCO, VSWV, VSWL
! floating-point arrays, time-varying
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VSKR !(LLEE, NELEE)
DOUBLEPRECISION WLNOW (NVSEE), RLFNOW (NLYREE, &
 NVSEE), RLHNOW (NLYREE, NVSEE), RLGNOW (NLYREE, NVSEE), RBFNOW ( &
 NVSEE), RBHNOW (NVSEE)
!PRIVATE :: NELEE, NLFEE, NLYREE, NVSEE, LLEE, NSEE
!end MODULE vscom1_inc

!IMPORTED FROM MODULE vssoil_inc
!MODULE vssoil_inc 
! 19/9/94
!------------------- Start of VSSOIL.INC ------------------------------*
! common block for soil parameter tables
!----------------------------------------------------------------------*
! Modules:       VSS (0.0)
! Program:       SHETRAN (4.0)
! Includers:     ??
! Modifications:
!  GP  15.04.94  Created
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
!
!USE SGLOBAL, ONLY : NSEE
!IMPLICIT NONE
INTEGER :: NSOLEE  

PARAMETER (NSOLEE = 200)  
DOUBLEPRECISION VSPPSI (NSOLEE), VSPTHE (NSOLEE, NSEE), VSPKR ( &
 NSOLEE, NSEE), VSPETA (NSOLEE, NSEE)
DOUBLEPRECISION VSPDTH (NSOLEE, NSEE), VSPDKR (NSOLEE, NSEE), &
 VSPDET (NSOLEE, NSEE)

DOUBLEPRECISION VSPSS (NSEE), VSPPOR (NSEE)  

INTEGER :: NVSSOL  
!PRIVATE :: NSEE
!END MODULE vssoil_inc

!IMPORTED FROM MODULE VSINIT_INC
!MODULE VSINIT_INC
!------------------- Start of VSINIT.INC ------------------------------*
! common block for VSS variables used in initialisation only
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/VSINIT.INC/4.1
! Modifications:
!  GP  12.09.94  Created (v4.0 finished 9/8/95)
! RAH  970630  4.1  Move NAQCON,IAQCON to VSIN; see also VSREAD,VSCONL.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*

! logical variables
!USE SGLOBAL, ONLY : NELEE, NSEE, NVSEE
!IMPLICIT NONE

LOGICAL :: BFAST, BSOILP  


!COMMON / VSINIL / BFAST, BSOILP  
! integer arrays & variables
INTEGER :: IVSFLG (NSEE), IVSNTB (NSEE), NVSERR, INITYP  


!COMMON / VSINII / IVSFLG, IVSNTB, NVSERR, INITYP  
! floating-point arrays & variables

DOUBLEPRECISION VSTRES (NSEE), VSVGN (NSEE), VSALPH (NSEE), &
 VSIPSD, VSZWLB (NVSEE), VSZWLT (NVSEE), TBPSI (NVSEE, NSEE), &
 TBTHE (NVSEE, NSEE), TBKR (NVSEE, NSEE), TBTHEC (NVSEE, NSEE), &
 TBKRC (NVSEE, NSEE), VSSPD (NELEE)
!PRIVATE :: NELEE, NSEE, NVSEE
!end MODULE VSINIT_INC


PRIVATE
PUBLIC :: VSIN, VSSIM, & !REST ARE PUBLIC ONLY FOR AD
          rlfdum, rlgnxt, firstvssim, rbhlst, rlhlst, vsaijsv, jcbcsv, rbhprv, rlglst, rlhprv, rbfprv, rlgprv, &
         rlfprv, rwelin, rbhtim, wltime, rlhdum, rbhnxt, rlhtim, rlgdum, rlhnxt, rbftim, rlgtim, &
         VSPTHE, NVSSOL, VSPKR, VSPETA, VSPDTH, VSPDKR, VSPDET, VSPPSI, &
         wlnow, vskr, rlfnow, rbfnow, ivssto, rlhnow, rbhnow, INITIALISE_VSMOD, &
         RLFTIM, icsoilsv !THESE NEEDED ONLY FOR AD
CONTAINS


!SSSSSS SUBROUTINE initialise_vsmod
SUBROUTINE initialise_vsmod()

ALLOCATE(vsaijsv(4,top_cell_no,total_no_elements), vskr(top_cell_no,total_no_elements))
END SUBROUTINE initialise_vsmod

!SSSSSS SUBROUTINE VSBC (BCHELE, FACE, ICBOT, ICTOP, JCBC, ICLYRB, ICLFN, &
SUBROUTINE VSBC (BCHELE, FACE, ICBOT, ICTOP, JCBC, ICLYRB, ICLFN, &
 ICLFL, ICLHN, ICLHL, CZG, CDELL, CDELZ, CZ, CAIJ, CLF, CLH, CPSI, &
 CKIJ, CDKIJ, CB, CR, CQH, DUM)
!----------------------------------------------------------------------*
! Sets up coefficients for column user-defined boundary conditions
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSBC/4.1
! Modifications:
!  GP  22.08.94  written (v4.0 finished 8.8.95)
! RAH  970120  4.1  No leading comments.  No lower-case code.
!                   Combine IF-blocks.  Use generic intrinsics.
!      970127       Use arguments, not INCLUDE.  Use DUM for TDUM,HDUM.
!      970514       Scrap workspace arg CDQH; set CB & CR directly.
!                   Don't initialize CQH (see VSCOLM).  New local QTOT.
!                   Add arg FACE (=1:4) & 1st dim to arrays CAIJ & CQH.
!      970813       Amend CLF & DUM subscripts: use I not ILYR.
!----------------------------------------------------------------------*
! Entry conditions:
! 1 <= FACE  <= 4
! 1 <= ICBOT <= ICTOP <=   LLEE (size of DUM)
! 0 <= ICLFN          <= NLYREE (size of ICLFL,CLF)
! 0 <= ICLHN          <= NLYREE (size of ICLHL,CLH,DUM)
! 0 <  CDELL
! for each i in 1:ICLFN:
!            1 <= ICLFL(i) < NLYREE (size of ICLYRB)
!        ICBOT <= ICLYRB(y)
!    1 + ICTOP >= ICLYRB(y+1)
! where y=ICLFL(i)
! for each i in 1:ICLHN:
!            1 <= ICLHL(i) < NLYREE (size of ICLYRB)
!        ICBOT <= ICLYRB(y)
!    1 + ICTOP >= ICLYRB(y+1)
! where y=ICLHL(i)
! for each c in ICBOT:ICTOP: 0 < CDELZ(c), CKIJ(c)
!----------------------------------------------------------------------*
! Input arguments
LOGICAL :: BCHELE  
INTEGER :: FACE, ICBOT, ICTOP, JCBC, ICLYRB ( * )  
INTEGER :: ICLFN, ICLFL ( * ), ICLHN, ICLHL ( * )  
DOUBLEPRECISION CDELZ (ICBOT:ICTOP), CZ (ICBOT:ICTOP), CZG, CDELL  
DOUBLEPRECISION CAIJ (4, ICBOT:ICTOP), CPSI (ICBOT:ICTOP), &
 CLF ( * )

DOUBLEPRECISION CKIJ (ICBOT:ICTOP), CDKIJ (ICBOT:ICTOP), CLH ( * )  
! In+out arguments

DOUBLEPRECISION CB (ICBOT:ICTOP), CR (ICBOT:ICTOP)  
! Output arguments

DOUBLEPRECISION CQH (4, ICBOT:ICTOP)  
! Workspace arguments

DOUBLEPRECISION DUM ( * )  
! Locals, etc
!INTRINSIC MAX  
INTEGER :: ICL, I, ILYR, ICL1, ICL2, IDUM, SGN  


DOUBLEPRECISION ADHOL, AOL, KDUM, Q, QTOT, TICL, TTOT, ZDUM  
!----------------------------------------------------------------------*
! flow (type 3)

IF (JCBC.EQ.3) THEN  
   DO 200 I = 1, MAX (1, ICLFN)  
      IF (ICLFN.EQ.0) THEN  
         ICL1 = ICBOT  
         ICL2 = ICTOP  
      ELSE  
         ILYR = ICLFL (I)  
         ICL1 = ICLYRB (ILYR)  
         ICL2 = ICLYRB (ILYR + 1) - 1  
      ENDIF  
      TTOT = zero  
      DO 160 ICL = ICL1, ICL2  
         TICL = CKIJ (ICL) * CDELZ (ICL)  
         DUM (ICL) = TICL  
         TTOT = TTOT + TICL  
  160       END DO  
      QTOT = CLF (I)  
      DO 180 ICL = ICL1, ICL2  
         Q = (DUM (ICL) / TTOT) * QTOT  
         CR (ICL) = CR (ICL) - Q  
         CQH (FACE, ICL) = Q  
  180       END DO  

  200    END DO  
! head (type 4)
! NB. If BCHELE=.false., head b.c.'s are depths below ground surface

ELSEIF (JCBC.EQ.4) THEN  
   IF (BCHELE) THEN  
      ZDUM = zero  
      SGN = + 1  
   ELSE  
      ZDUM = CZG  
      SGN = - 1  
   ENDIF  
   IDUM = MAX (ICLHN, 1)  
   DO 210 I = 1, IDUM  
      DUM (I) = ZDUM + SGN * CLH (I)  

  210    END DO  
   DO 260 I = 1, IDUM  
      IF (ICLHN.EQ.0) THEN  
         ICL1 = ICBOT  
         ICL2 = ICTOP  
      ELSE  
         ILYR = ICLHL (I)  
         ICL1 = ICLYRB (ILYR)  
         ICL2 = ICLYRB (ILYR + 1) - 1  
      ENDIF  
      DO 240 ICL = ICL1, ICL2  
         AOL = CAIJ (FACE, ICL) / CDELL  
         ADHOL = (DUM (I) - CZ (ICL) - CPSI (ICL) ) * AOL  
         KDUM = CKIJ (ICL)  
         Q = KDUM * ADHOL  
         CB (ICL) = CB (ICL) + CDKIJ (ICL) * ADHOL + KDUM * AOL  
         CR (ICL) = CR (ICL) - Q  
         CQH (FACE, ICL) = Q  
  240       END DO  


  260    END DO  
! head gradient (type 5)

ELSEIF (JCBC.EQ.5) THEN  

   !STOP 'unfinished code for boundary type 5 - head gradients'  
   print*,  'unfinished code for boundary type 5 - head gradients'  

ENDIF  
END SUBROUTINE VSBC





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
  100    END DO  
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

  200    END DO  

ELSEIF (ISONE(CWV)) THEN  
!        * Arithmetic mean
   DO 203 I = ICBOT, ICTOP  
      CKZS = VSK3D (ICSOIL (I), 3)  
      C (I) = CKR (I) * CKZS  
      D (I) = CDKR (I) * CKZS  
  203    END DO  
   DO 205 I = ICBOT + 1, ICTOP  
      M = I - 1  
      AODZ = CA0 / (CDELZ (M) + CDELZ (I) )  
      CBETM (I) = AODZ * (C (M) + C (I) )  
      CDBETM (I) = AODZ * D (I)  
      CDBTMM (I) = AODZ * D (M)  

  205    END DO  

ELSE  
!        * General w-mean
   WI = one / CWV  
   WIM1 = (one - CWV) / CWV  
   DO 208 I = ICBOT, ICTOP  
      CKZS = VSK3D (ICSOIL (I), 3)  
      C (I) = (CKR (I) * CKZS) **CWV  
      D (I) = CDKR (I) * CKZS  
  208    END DO  
   DO 210 I = ICBOT + 1, ICTOP  
      M = I - 1  
      CM = C (M)  
      CI = C (I)  
      CAVE = .5D0 * (CM + CI)  
      AODZ = CA0 / (CDELZ (M) + CDELZ (I) )  
      CBETM (I) = AODZ * CAVE**WI * 2D0  
      CDBETM (I) = AODZ * (CAVE / CI) **WIM1 * D (I)  
      CDBTMM (I) = AODZ * (CAVE / CM) **WIM1 * D (M)  

  210    END DO  

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


  220 END DO  
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

  300    END DO  

  400 END DO  
END SUBROUTINE VSCOEF





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


   20    END DO  
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
  100    END DO  

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
    




  510 CONTINUE  
! Calculate final values of output variables
!____________________________________________*
! flows
DO 600 ICL = ICBOT, ICTOP - 1  
   PCL = ICL + 1  
   CQV (ICL) = CBETM (PCL) * (CZ (ICL) + CPSI (ICL) - CZ (PCL) &
    - CPSI (PCL) ) / CA0

  600 END DO  
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
  640    END DO  


  650 END DO  
! phreatic surface level
CPSMIN = CZ (ICBOT) - half * CDELZ (ICBOT)  
DO 920 ICL = ICBOT, ICTOP  
  920 IF (CPSI (ICL) .LT.ZERO) GOTO 940  
  940 ICL = MAX (ICBOT, ICL - 1)  

CPSL = MAX (CPSMIN, CZ (ICL) + CPSI (ICL) )  
END SUBROUTINE VSCOLM





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
  210 NRENUM = NRENUM + 1  
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

  920       END DO  


  950    END DO  
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


  960    END DO  
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


  974    END DO  
!>         < this point won't be traversed unless bank is below bed.
!           cell just below link bed: smaller than bank, unless ...
  976    ZDEPTH = ZBDBOT - ZCBOT  
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

  980    END DO  
!           update LL & store number of cells for the link
   top_cell_no = MAX (top_cell_no, ICL1)  

   IDUM (ILINK) = ICL1  
!           set up opposite bank cells
   IBANK2 = ICMBK (ILINK, 2)  
   DO 985 I = 1, ICL  
      DELTAZ (I, IBANK2) = DELTAZ (I, IEL)  
      ZVSNOD (I, IBANK2) = ZVSNOD (I, IEL)  
  985    END DO  

   IDUM (IBANK2) = ICL  




 1000 END DO  
! Renumber cells (so that the top cell number is LL) & set up NLYRBT
!____________________________________________________________________*
! --- set number of first column element
IF (BEXBK) THEN  
   ICOL1 = 1  
ELSE  
   ICOL1 = total_no_links + 1  
!!!
!            * temporary measure to avoid out-of-bounds errors, etc
   DO 1080 ILINK = 1, total_no_links  
 1080    NLYRBT (ILINK, 1) = top_cell_no  
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
 1100       END DO  
      CALL ALINIT (ZERO, ICL0, DELTAZ (1, IEL) )  
      CALL ALINIT (ZERO, ICL0, ZVSNOD (1, IEL) )  

   ENDIF  
!           * find bottom cell in each layer
!970612
   ICL0 = ICL0 + 1  
!^^^^^^
   DO 1150 ILYR = 1, NLYR (IEL)  
      DO 1120 ICL = ICL0 + 1, top_cell_no  
 1120       IF (ZVSNOD (ICL, IEL) .GT.ZLYRBT (IEL, ILYR) ) GOTO 1130  
 1130       NLYRBT (IEL, ILYR) = ICL  
      ICL0 = ICL - 1  
 1150    END DO  

   NLYRBT (IEL, ILYR) = top_cell_no + 1  





 1200 END DO  
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
 1220       END DO  
 1240    END DO  



 1260 END DO  
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
 1280          END DO  
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


 1322       END DO  
! ----- aquifer zone processing
!              * loop over layers, starting at the bottom
      ILYR = 1  
      JLYR = 1  

 1410       CONTINUE  
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
 1460             END DO  
            NCELL = NCELL - NODD  
            NJMIN = (NCELL + 1) / 2 + NJMIN  
            NODD = MOD (NCELL, 2)  
         ELSEIF (NCELL.GT.0) THEN  
            NODD = 0  
         ENDIF  
 1470       END DO  

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
 1560             END DO  
            NCELL = NCELL - NODD  
            NIMIN = (NCELL + 1) / 2 + NIMIN  
            NODD = MOD (NCELL, 2)  
         ELSEIF (NCELL.GT.0) THEN  
            NODD = 0  
         ENDIF  
 1570       END DO  

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

 1572          END DO  

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

 1574          END DO  

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

 1575          IF (I.LE.NITOT.AND.J.LE.NJTOT) THEN  
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
 1580             END DO  
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

 1585       BDONE (IEL, IFA) = .TRUE.  

 1590    END DO  



 1600 END DO  
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
   DO 2050 ICL = NLYRBT (IEL, 1), LTOP - 1  
 2050    ZVSNOD (ICL, IEL) = ZVSNOD (ICL, IEL) - ZDIFF  
   ZVSNOD (ICL, IEL) = ZVSNOD (ICL, IEL) - ZDIFF * half  
   DO 2060 ILYR = 1, NLYR (IEL)  

 2060    ZLYRBT (IEL, ILYR) = ZLYRBT (IEL, ILYR) - ZDIFF  
!        * NB. banks 1 and 2 are identical
   NHBED (IEL, 1) = NACELL  
   NHBED (IEL, 2) = NACELL  
   FHBED (IEL, 1) = ZERO  

   FHBED (IEL, 2) = ZERO  

 2100 END DO  

RETURN  

 8048 CALL ERROR(FFFATAL, 1048, PPPRI, 0, 0, 'Attempts to renumber cells have failed.')
 9000 FORMAT(/ 'Number of top cell in all columns (LL) = ',I3)  
 9200 FORMAT('Null cell connectivity being set up for face ',I1, &
&       ' layer ',I2)
 9300 FORMAT(  'Not possible to connect all cells for face ',I1, &
&       ' layer ',I2)
END SUBROUTINE VSCONC

!FFFFFF INTEGER FUNCTION fncell
INTEGER FUNCTION fncell(I, IEL, ITOP)
INTEGER, INTENT(IN) :: I, IEL, ITOP
fncell = IDIMJE(MIN(NLYRBT (IEL, I + 1), ITOP + 1), NLYRBT (IEL, I) )
 END FUNCTION fncell



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
DO 50 IFA = 1, 4  
   DO 50 IEL = 1, total_no_elements  
      DO 50 ILYR = 1, NLYR (IEL) + 1  
         JVSALN (IEL, ILYR, IFA) = 0  



   50 CONTINUE  
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
      DO 102 I = 1, NLYRI  
  102       ILDUM (I) = - 1  
      DO 104 J = 1, NLYRJ  

  104       JLDUM (J) = - 1  

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


  110       END DO  
! set ILYR & JLYR to numbers of layers immediately below soil zone
!           ZSMALL is added to avoid rounding errors if the bottom
!           of a layer coincides with the bottom of the soil zone
!970711     ! expression for ZSZBOT is wrong for link elements
      ZSZBOT = ZGRUND (IEL) - DCSTOT - ZSMALL  
      DO 120 ILYR = NLYRI, 1, - 1  
  120       IF (ZLYRBT (IEL, ILYR) .LT.ZSZBOT) GOTO 125  
  125       ZSZBOT = ZGRUND (JEL) - DCSTOT - ZSMALL  
      DO 140 JLYR = NLYRJ, 1, - 1  


  140       IF (ZLYRBT (JEL, JLYR) .LT.ZSZBOT) GOTO 200  
! --- start of loop over layers (downwards from top of aquifer zone)

  200       IF (ILYR.EQ.0.OR.JLYR.EQ.0) GOTO 400  
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
  210          CONTINUE  
!                     *
         ILMAX = ILYR  
         DO 220 ILYR = ILMAX, ILMIN, - 1  
            JRANGE = ILDUM (ILYR)  
            JVSALN (IEL, ILYR, IFA) = MAX (0, JRANGE)  
            IF (JRANGE.GT.0) JLMIN = MIN (JLMIN, JRANGE / NMOD)  
  220          END DO  
!                     *
         JLMAX = JLYR  
         DO 240 JLYR = JLMAX, JLMIN, - 1  
            IRANGE = JLDUM (JLYR)  
            JVSALN (JEL, JLYR, JFA) = MAX (0, IRANGE)  
            IF (IRANGE.GT.0) ILMIN = MIN (ILMIN, IRANGE / NMOD)  
  240          END DO  
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
         DO 260 I = ILYR - 1, 1, - 1  
  260          IF (NTSOIL (IEL, I) .EQ.JSOIL.OR.ILDUM (I) .GE.0) GOTO &
          265
  265          ISKIP = ILYR - I  
         DO 280 J = JLYR - 1, 1, - 1  
  280          IF (NTSOIL (JEL, J) .EQ.ISOIL.OR.JLDUM (J) .GE.0) GOTO &
          285

  285          JSKIP = JLYR - J  
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

  400    END DO  

   BDONE (IEL) = .TRUE.  



  500 END DO  
! Formats
!_________*
 9381 FORMAT('Layer',I3,' out of range, IAQCON entry',I3, &
&      ' (element',I5,' has',I3,' layers)')

 9382 FORMAT('Invalid null connection, element',I5,':', &
&      ' layer',I3,' already connected to layer',I3,', element',I5, &
&      ' (see IAQCON entry',I3,')')
END SUBROUTINE VSCONL



!SSSSSS SUBROUTINE VSFUNC
SUBROUTINE VSFUNC (NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
 VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ICSOIL, CPSI, ICSTOR, &
 CTHETA, CETA, CKR, CDETA, CDKR)
!
!----------------------------------------------------------------------*
! Calculates moisture content, storage coefficient, and relative
! hydraulic conductivity for a column, given soil water potentials
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSFUNC/4.1
! Modifications:
!  GP  18.8.94  written
! RAH  961220  4.1  No long/leading comments.  Declare externals.
!                   Explicit sizes where possible.  ICSTOR is in+out.
!                   No redundant execution or lower-case code.
!      970121       Use arguments, not COMMON.  Allow end-point cases.
!                   Remove redundant arguments and commented code.
!      970122       Amend entry conditions.  Use branch for ERROR call.
!JE  JAN 2009       Loop restructure for AD
!----------------------------------------------------------------------*
! Returns:
!         moisture content (CTHETA),
!         storage co-efficient (CETA),
!         relative hydraulic conductivity (CKR),
!         derivative of storage coefficient(CDETA), and
!         derivative of relative conductivity (CDKR)
! for all cells in a column, given pressure potential (CPSI).
!
! Based on subroutine HUNT of 'Numerical Recipes in FORTRAN: The Art of
!   Scientific Computing (2nd Ed.)', Press et al. (1992), p112
!----------------------------------------------------------------------*
! Entry conditions:
!     PRI is connected for formatted output
! 1 < NVSSOL <= NSOLEE
!     VSPPSI is monotonic strictly decreasing
!      ICBOT <= ICTOP
! 0 < ICSOIL(ICBOT:ICTOP) <= NS (size of 2nd dimension of VSPTHE, etc)
!----------------------------------------------------------------------*
!
! Input arguments
INTEGER :: NVSSOL, NSOLEE  
DOUBLEPRECISION VSPPSI (NVSSOL), VSPTHE (NSOLEE, * )  
DOUBLEPRECISION VSPKR (NSOLEE, * ), VSPETA (NSOLEE, * )  
DOUBLEPRECISION VSPDKR (NSOLEE, * ), VSPDET (NSOLEE, * )  
INTEGER :: IEL, ICBOT, ICTOP, ICSOIL (ICBOT:ICTOP)  
DOUBLEPRECISION CPSI (ICBOT:ICTOP)  
!
! In+out arguments
INTEGER :: ICSTOR (ICBOT:ICTOP)  
!
! Output arguments
DOUBLEPRECISION CTHETA (ICBOT:ICTOP)  
DOUBLEPRECISION CETA (ICBOT:ICTOP), CKR (ICBOT:ICTOP)  
DOUBLEPRECISION CDETA (ICBOT:ICTOP), CDKR (ICBOT:ICTOP)  
!
! Locals, etc
!INTRINSIC MAX, MIN, NINT  
CHARACTER (LEN=5) :: WETDRY (0:1)  
DOUBLEPRECISION P, PDUM, VLO  
INTEGER :: ICL, INC, JHI, JLO, JM, IS, DRY 
LOGICAL :: g8100 

DATA WETDRY / '(wet)', '(dry)' /  
!
!----------------------------------------------------------------------*
!
! ----- loop over all cells in column
G8100=.FALSE.
OUT100 : DO ICL = ICBOT, ICTOP
    IF(g8100) CYCLE  
   P = CPSI (ICL)  
   JLO = ICSTOR (ICL)  



   IS = ICSOIL (ICL)  
! --- find location in table of current psi value
! test for initial guess
   IF (JLO.LE.0.OR.JLO.GT.NVSSOL) THEN  
      JLO = 0  
      JHI = NVSSOL + 1  
      GOTO 30  


   ENDIF  
! set initial hunt increment, and hunt up the table
   INC = 1  

   IF (P.LE.VSPPSI (JLO) ) THEN  
   10       JHI = JLO + INC  
      IF (JHI.GT.NVSSOL) THEN  
         JHI = NVSSOL + 1  
      ELSEIF (P.LE.VSPPSI (JHI) ) THEN  
         JLO = JHI  
         INC = INC + INC  
         GOTO 10  


      ENDIF  
! hunt down the table

   ELSE  
      JHI = JLO  
   20       JLO = JHI - INC  
      IF (JLO.LT.1) THEN  
         JLO = 0  
      ELSEIF (P.GT.VSPPSI (JLO) ) THEN  
         JHI = JLO  
         INC = INC + INC  
         GOTO 20  

      ENDIF  



   ENDIF  
! hunt completed, begin bisection
!       At this point: { VSPPSI(JLO)>=P or JLO=0        } and
!                      { VSPPSI(JHI)< P or JHI=NVSSOL+1 }
   30    IF (JHI - JLO.EQ.1) GOTO 50  
   JM = (JHI + JLO) / 2  
   IF (P.LT.VSPPSI (JM) ) THEN  
      JLO = JM  
   ELSE  
      JHI = JM  
   ENDIF  
   GOTO 30  
   50    CONTINUE  
   JLO = MAX (1, MIN (JLO, NVSSOL - 1) )  

   JHI = JLO + 1  


   ICSTOR (ICL) = JLO  
! --- interpolate between values for return variables
   VLO = VSPPSI (JLO)  
   PDUM = (P - VLO) / (VSPPSI (JHI) - VLO)  

   IF (PDUM.LT.ZERO.OR.PDUM.GT.ONE) THEN  !GOTO 8100
    g8100=.TRUE. 
    CYCLE out100
   ENDIF
   VLO = VSPTHE (JLO, IS)  
   CTHETA (ICL) = (VSPTHE (JHI, IS) - VLO) * PDUM + VLO  
   CETA (ICL) = VSPETA (JHI, IS)  
   VLO = VSPDKR (JLO, IS)  
   CDKR (ICL) = (VSPDKR (JHI, IS) - VLO) * PDUM + VLO  
   VLO = VSPKR (JLO, IS)  
   CKR (ICL) = (VSPKR (JHI, IS) - VLO) * PDUM + VLO  
   VLO = VSPDET (JLO, IS)  

   CDETA (ICL) = (VSPDET (JHI, IS) - VLO) * PDUM + VLO  
ENDDO out100
!----------------------------------------------------------------------*
! Exit conditions:
! for each c in ICBOT:ICTOP:
!             0 <  ICSTOR(c) <  NVSSOL
!    VSPPSI(j)  <=   CPSI(c) <= VSPPSI(j+1)
!    VSPTHE(j,s)<= CTHETA(c) <= VSPTHE(j+1,s)
!    VSPETA(j,s)<=   CETA(c) <= VSPETA(j+1,s)
!     VSPKR(j,s)<=    CKR(c) <=  VSPKR(j+1,s)
!    VSPDET(j,s)<=  CDETA(c) <= VSPDET(j+1,s)
!    VSPDKR(j,s)<=   CDKR(c) <= VSPDKR(j+1,s)
! where j=ICSTOR(c) and s=ICSOIL(c)
!----------------------------------------------------------------------*
!RETURN
IF(g8100) THEN 
    DRY = NINT (MAX (ZERO, MIN (PDUM, ONE) ) )  !8100
    CALL ERROR(FFFATAL, 1034 + DRY, PPPRI, IEL, ICL, 'soil property interpolation out of range '//WETDRY (DRY) )
ENDIF
END SUBROUTINE VSFUNC



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

  700 END DO  
DO 890 IEL = total_no_links + 1, total_no_elements  
   ICBOT = NLYRBT (IEL, 1)  

   ZGI = ZGRUND (IEL)  
   IW = NVSWLI (IEL)  

   IF (IW.GT.0) THEN  
      RDUM = ZGI - VSZWLB (IW)  
      DO 760 ICL = ICBOT, top_cell_no  
  760       IF (RDUM.LE.ZVSNOD (ICL, IEL) ) GOTO 770  

  770       NWELBT (IEL) = ICL  
      RDUM = ZGI - VSZWLT (IW)  
      DO 780 ICL = top_cell_no, ICBOT, - 1  
  780       IF (RDUM.GE.ZVSNOD (ICL, IEL) ) GOTO 790  

  790       NWELTP (IEL) = ICL  

   ENDIF  
   RDUM = VSSPD (IEL)  
   IF (GTZERO(RDUM)) THEN  
      RDUM = ZGI - RDUM  
      DO 820 ICL = ICBOT, top_cell_no  
         DZ = ABS (ZVSNOD (ICL, IEL) - RDUM)  
         IF (DZ.LE.half * DELTAZ (ICL, IEL) ) GOTO 860  
  820       END DO  
  860       NVSSPC (IEL) = ICL  

   ENDIF  


  890 END DO  
! call VSSOIL to set up soil property tables



CALL VSSOIL  
! set up initial conditions (read from file unit VSI, if required)
! type 1 - uniform phreatic surface depth, equilibrium psi profile

IF (INITYP.EQ.1) THEN  
   DO 900 IEL = 1, total_no_elements  
      ZVSPSL (IEL) = MAX (ZLYRBT (IEL, 1), ZGRUND (IEL) - VSIPSD)  


  900    END DO  
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
      DO 920 ICL = ICBOT, ICTOP  
  920       IF (LTZERO(VSPSI(ICL,IEL))) GOTO 940  
  940       ICL = MAX (ICBOT, ICL - 1)  

      ZVSPSL (IEL) = MAX (ZMIN, ZVSNOD (ICL, IEL) + VSPSI (ICL, &
       IEL) )

  950    END DO  


ENDIF  
! set up equilibrium psi profile for types 1 or 2
IF (INITYP.EQ.1.OR.INITYP.EQ.2) THEN  
   DO 1200 IEL = 1, total_no_elements  
      DO 1140 ICL = NLYRBT (IEL, 1), top_cell_no  
         VSPSI (ICL, IEL) = ZVSPSL (IEL) - ZVSNOD (ICL, IEL)  
 1140       END DO  
 1200    END DO  


ENDIF  
! set up initial relative conductivities for all elements

DO 1400 IEL = ISTART, total_no_elements  
   DO 1270 ILYR = 1, NLYR (IEL)  
      DO 1250 ICL = NLYRBT (IEL, ILYR), NLYRBT (IEL, ILYR + 1) &
       - 1
         ISDUM (ICL) = NTSOIL (IEL, ILYR)  
         IVSSTO (ICL, IEL) = 0  
 1250       END DO  

 1270    END DO  
   ICBOT = NLYRBT (IEL, 1)  
   ICTOP = top_cell_no  

   CALL VSFUNC ( NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
    VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ISDUM (ICBOT), &
    VSPSI (ICBOT, IEL), IVSSTO (ICBOT, IEL), CDUM1, CDUM2, VSKR ( &
    ICBOT, IEL), CDUM3, CDUM4)

 1400 END DO  
WRITE(PPPRI, 9010) 'End', '   '  


GOTO 8900  
! Error handling
 8041 NVSERR = NVSERR + 1  
WRITE (MSG, 9040) IEL  

CALL ERROR (EEERR, 1041, PPPRI, 0, 0, MSG)  
 8900 IF (NVSERR.LT.1) RETURN  
WRITE (MSG, 9030) NVSERR  

CALL ERROR(FFFATAL, 1040, PPPRI, 0, 0, MSG)  

 9010 FORMAT( / '!!',78('#') / 1X,A,' of VSS data ',A,60('#') / 80('#'))  

 9030 FORMAT(I4,' Errors have occurred in VSS data reading ', &
&          'or initialisation.')

 9040 FORMAT('Error reading VSS initial conditions for element ', &
&       I4, '.')
END SUBROUTINE VSIN





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
  100 END DO  


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


  200 END DO  
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

  300    END DO  

  400 END DO  
END SUBROUTINE VSINTC



!SSSSSS SUBROUTINE VSLOWR (JCBC, CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, &
SUBROUTINE VSLOWR (JCBC, CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, &
 CKR, CDKR, CB, CR, CQV)
!
!----------------------------------------------------------------------*
! Sets up coefficients for column lower boundary condition
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSLOWR/4.1
! Modifications:
!  GP  22.08.94  written
! RAH  970120  4.1  No leading comments.  No lower-case code.
!                   Combine IF-blocks.  Use local CQVDUM.
!      970131       Use arguments, not INCLUDE.  CDQDUM DBLE, not DOUBLEPRECISION.
!----------------------------------------------------------------------*
! Entry conditions:
! 0 < CDELZ
!----------------------------------------------------------------------*
!
! Input arguments
INTEGER :: JCBC  
DOUBLEPRECISION CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, CKR, CDKR  
!
! In+out arguments
DOUBLEPRECISION CB, CR  
!
! Output arguments
DOUBLEPRECISION CQV  
!
! Locals, etc
DOUBLEPRECISION CDQDUM, CQVDUM, DH, KSODZ  
!
!----------------------------------------------------------------------*
!
! column base flow (type 6)
IF (JCBC.EQ.6) THEN  
   CQVDUM = CBF  

   CDQDUM = zero  
! column base head (type 7)
ELSEIF (JCBC.EQ.7) THEN  
   DH = CBH - CZ - CPSI  
   KSODZ = CKZS / (half * CDELZ)  
   CQVDUM = KSODZ * CKR * DH  

   CDQDUM = KSODZ * (CDKR * DH - CKR)  
! no flow (970131: Check column base free drainage (type 8)!)
ELSE  
   CQVDUM = zero  

   CDQDUM = zero  

ENDIF  
CQV = CQVDUM  
CB = CB + CA0 * CDQDUM  

CR = CR - CA0 * CQVDUM  
END SUBROUTINE VSLOWR




!SSSSSS SUBROUTINE VSMB (VSTHEN)  
SUBROUTINE VSMB (VSTHEN)  
!
!----------------------------------------------------------------------*
! Updates flows to ensure mass conservation
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSMB/4.1
! Modifications:
!  GP  08.03.95  written (v4.0 finished 17.07.96)
! RAH  961228  4.1  Remove variable ILINK.  No leading comments.
!      970214       Reverse DELTAZ,QVSH indices (AL.C). Declare JCL,JFA.
!                   mv VSTHEN from VSCOM1.INC to arg list, reverse subs.
!      970118       Swap subscripts: JVSACN,QVSV,QVSWLI,VSTHE (AL.C);
!                   also fix error in QVSWLI index: use IW not IEL.
!                   Remove temporary code (to set VSSTMP).  DBLE locals.
!                   Don't include VSCOM1.INC.
!      970509       Scrap output QVSBF (set in VSSIM).  Order labels.
!                   Remove redundant local BDONE.  Trap JVSDEL.ne.0.
!----------------------------------------------------------------------*
! Commons and distributed constants
! Imported constants
!     AL.P.VSS:        LLEE,NELEE
! Input common
!     AL.C:            LL,  NLYRBT(NEL,1),JVSACN(4,LLEE,NEL)
!                           NVSWLI(NEL),  JVSDEL(4,LLEE,NEL)
!                             AREA(NEL),    DELTAZ(LLEE,NEL)
!                           LINKNS(*)
!                      DTUZ,ESOILA(NEL),      ERUZ(NELEE,LL)
!                       VSTHE(LLEE,NEL), QVSV(LLEE,NEL),QVSWLI(LLEE,*)
!     AL.G:            NEL, ICMREF(NELEE,12)
! In+out common
!     AL.C:            QVSH(4,LLEE,NEL)
! Input arguments

DOUBLEPRECISION VSTHEN (LLEE, total_no_elements)  
! Locals, etc
INTEGER :: NFACES, IFACES (4)  
INTEGER :: IEL, J, ITYPE, IFA, JEL, ICL, JFA, JCL, IW, MCL  
DOUBLEPRECISION AREAE, CMBE, F, Qasum
LOGICAL :: iscycle
!----------------------------------------------------------------------*
! --- loop over all elements
iscycle=.FALSE.
DO 2900 IEL = 1, total_no_elements
    IF(iscycle) CYCLE  
    ITYPE = ICMREF (IEL, 1)  
    ! Choose faces to adjust (ie set NFACES and IFACES)
    ! grids - do nothing!
    IF (ITYPE.EQ.0) THEN  
        NFACES = 0  
        ! banks - update only 'outer' face adjacent to grid (if there is one)
    ELSEIF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN  
        NFACES = 0
        DO 920 IFA = 1, 4
            IF(iscycle) CYCLE 
            JEL = ICMREF (IEL, IFA + 4)  
            IF (JEL.GT.0) THEN  
                IF (ICMREF (JEL, 1) .EQ.0) THEN  
                    IFACES (1) = IFA  
                    NFACES = 1  
                    iscycle = .TRUE. !GOTO 930  !                       >>>>>>>>
                ENDIF  
            ENDIF  
        920 ENDDO  
        iscycle=.FALSE.! 930 CONTINUE  
        ! links - update faces adjacent to banks only
    ELSE  
        NFACES = 2  
        IF (LINKNS (IEL) ) THEN  
            IFACES (1) = 1  
            IFACES (2) = 3  
        ELSE  
            IFACES (1) = 2  
            IFACES (2) = 4  
        ENDIF  
    ENDIF  
    ! Loop over column cells if required (top to bottom for QVSV's benefit)
    IF (NFACES.GT.0) THEN  
        IW = NVSWLI (IEL)  
        AREAE = cellarea (IEL)  
        DO 990 ICL = top_cell_no, NLYRBT (IEL, 1), - 1  
            ! calculate mass balance error (m**3/s)
            MCL = ICL - 1  
            CMBE = - QVSV (MCL, IEL) + QVSV (ICL, IEL) + ERUZ (IEL, &
                    ICL) + DELTAZ (ICL, IEL) * (VSTHE (ICL, IEL) - VSTHEN ( &
                    ICL, IEL) ) / DTUZ
             IF (IW.GT.0) CMBE = CMBE+QVSWLI (ICL, IW)  
            IF (ICL.EQ.top_cell_no) CMBE = CMBE+ESOILA (IEL)  
            CMBE = CMBE * AREAE  
            DO 950 IFA = 1, 4  
                   CMBE = CMBE-QVSH (IFA, ICL, IEL)  
            950  ENDDO  
            ! adjust lateral flows (unless Qasum=0)
            Qasum = zero  
            DO 955 J = 1, NFACES  
                IFA = IFACES (J)  
                Qasum = Qasum + QVSH (IFA, ICL, IEL)  
            955 ENDDO  
            IF (NOTZERO(Qasum)) THEN  
                F = one + CMBE / Qasum  
                DO 960 J = 1, NFACES  
                    IFA = IFACES (J)  
                    QVSH (IFA, ICL, IEL) = QVSH (IFA, ICL, IEL) * F
                960 ENDDO  
            ENDIF  
        990  ENDDO  
    ENDIF  
    ! Update flows for adjacent element
    DO 2800 IFA = 1, 4
        IF(iscycle) CYCLE  
        JEL = ICMREF (IEL, IFA + 4)  
        IF (JEL.GT.0) THEN  
            JFA = ICMREF (IEL, IFA + 8)  
            DO 1820 ICL = NLYRBT (IEL, 1), top_cell_no
                IF(iscycle) CYCLE
                !970509            (catch JEL next time around)
                IF (JVSDEL (IFA, ICL, IEL) .NE.0) THEN
                    iscycle=.TRUE.  !GOTO 8820
                    CYCLE
                ENDIF 
                JCL = JVSACN (IFA, ICL, IEL)  
                IF (JCL.GT.0) QVSH (JFA, JCL, JEL) = - QVSH (IFA, ICL, IEL)
            1820 ENDDO  
        ENDIF  
    2800    END DO  
2900 END DO  
IF(.NOT.iscycle) RETURN  
 8820 STOP 'UNFINISHED CODE FOR SPLIT CELLS IN SUBROUTINE VSMB!'  
END SUBROUTINE VSMB





!SSSSSS SUBROUTINE VSPREP ()  
SUBROUTINE VSPREP ()  
!----------------------------------------------------------------------*
! Prepares catchment, and controls reading of time-varying boundary
! conditions
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSPREP/4.1
! Modifications:
!  GP  29.07.94  written (v4.0 finished 3/5/95)
! RAH  961228  4.1  Remove variables IEL,ICL.  No leading comments.
!                   Declare ERROR external.  No lower-case code.
!                   Use SAVE instead of ineffectual COMMON.
!      970213       Reverse subscripts RLFNOW,RLHNOW,RLGNOW (see VSSIM).
!      970522       Initialize saved locals.
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P:            NVSEE
! Input common
!     ...
! Output common
!     VSCOM1.INC:      ...
INTEGER :: NDATA  
PARAMETER (NDATA = 4 + 3 * NVSEE)  
INTEGER :: I, II, III, NDUM  
!DOUBLEPRECISION WLLAST, WLTIME, RWELIN (NVSEE)  
!DOUBLEPRECISION RLFLST, RLFTIM, RLFPRV (NVSEE)  
!DOUBLEPRECISION RLHLST, RLHTIM, RLHPRV (NVSEE), RLHNXT (NVSEE)  
!DOUBLEPRECISION RLGLST, RLGTIM, RLGPRV (NVSEE), RLGNXT (NVSEE)  
!DOUBLEPRECISION RBFLST, RBFTIM, RBFPRV (NVSEE)  
!DOUBLEPRECISION RBHLST, RBHTIM, RBHPRV (NVSEE), RBHNXT (NVSEE)

!DOUBLEPRECISION RLFDUM (NVSEE), RLHDUM (NVSEE), RLGDUM (NVSEE)  
!SAVE WLLAST, WLTIME, RWELIN, RLHLST, RLHTIM, RLHPRV, RLHNXT  
!SAVE RLFLST, RLFTIM, RLFPRV, RLGLST, RLGTIM, RLGPRV, RLGNXT  
!SAVE RBFLST, RBFTIM, RBFPRV, RBHLST, RBHTIM, RBHPRV, RBHNXT  
!DATA WLLAST, WLTIME, RWELIN, RLHLST, RLHTIM, RLHPRV, RLHNXT / &
! NDATA * 0.0D0 /
!DATA RLFLST, RLFTIM, RLFPRV, RLGLST, RLGTIM, RLGPRV, RLGNXT / &
! NDATA * 0.0D0 /
!DATA RBFLST, RBFTIM, RBFPRV, RBHLST, RBHTIM, RBHPRV, RBHNXT / &
! NDATA * 0.0D0 /
!----------------------------------------------------------------------*
! wells

IF (NVSWL.GT.0) THEN  
   CALL FINPUT (WLD, TIH, UZNOW, UZNEXT, WLLAST, WLTIME, RWELIN, &
    NVSWL, WLNOW)

   IF (EQMARKER(WLTIME)) CALL ERROR(FFFATAL, 1042, PPPRI, 0, 0, &
    'End of well abstraction file (WLD)')



ENDIF  
! lateral flow boundary condition

IF (NVSLF.GT.0) THEN  
   CALL FINPUT (LFB, TIH, UZNOW, UZNEXT, RLFLST, RLFTIM, RLFPRV, &
    NVSLFT, RLFDUM)

   IF (EQMARKER(RLFTIM)) CALL ERROR(FFFATAL, 1043, PPPRI, 0, 0, &
    'End of lateral flow boundary condition file (LFB)')
   III = 1  
   DO 20 I = 1, NVSLF  
      NDUM = NVSLFN (I)  
      IF (NDUM.EQ.0) NDUM = 1  
      DO 10 II = 1, NDUM  
         RLFNOW (II, I) = RLFDUM (III)  
         III = III + 1  
   10       END DO  

   20    END DO  



ENDIF  
! lateral head boundary condition

IF (NVSLH.GT.0) THEN  
   CALL HINPUT (LHB, TIH, UZNOW, UZNEXT, RLHLST, RLHTIM, RLHPRV, &
    RLHNXT, NVSLHT, RLHDUM)

   IF (EQMARKER(RLHTIM)) CALL ERROR(FFFATAL, 1044, PPPRI, 0, 0, &
    'End of lateral head boundary condition file (LHB)')
   III = 1  
   DO 40 I = 1, NVSLH  
      NDUM = NVSLHN (I)  
      IF (NDUM.EQ.0) NDUM = 1  
      DO 30 II = 1, NDUM  
         RLHNOW (II, I) = RLHDUM (III)  
         III = III + 1  
   30       END DO  

   40    END DO  



ENDIF  
! lateral head gradient boundary condition

IF (NVSLG.GT.0) THEN  
   CALL HINPUT (LGB, TIH, UZNOW, UZNEXT, RLGLST, RLGTIM, RLGPRV, &
    RLGNXT, NVSLGT, RLGDUM)

   IF (EQMARKER(RLGTIM)) CALL ERROR(FFFATAL, 1052, PPPRI, 0, 0, &
    'End of lateral head gradient boundary condition file (LGB)')
   III = 1  
   DO 60 I = 1, NVSLG  
      NDUM = NVSLGN (I)  
      IF (NDUM.EQ.0) NDUM = 1  
      DO 50 II = 1, NDUM  
         RLGNOW (II, I) = RLGDUM (III)  
         III = III + 1  
   50       END DO  

   60    END DO  



ENDIF  
! column base flow boundary condition

IF (NVSBF.GT.0) THEN  
   CALL FINPUT (BFB, TIH, UZNOW, UZNEXT, RBFLST, RBFTIM, RBFPRV, &
    NVSBF, RBFNOW)

   IF (EQMARKER(RBFTIM)) CALL ERROR(FFFATAL, 1045, PPPRI, 0, 0, &
    'End of column base flow boundary condition file (BFB)')



ENDIF  
! column base head boundary condition

IF (NVSBH.GT.0) THEN  
   CALL HINPUT (BHB, TIH, UZNOW, UZNEXT, RBHLST, RBHTIM, RBHPRV, &
    RBHNXT, NVSBH, RBHNOW)

   IF (EQMARKER(RBHTIM)) CALL ERROR(FFFATAL, 1046, PPPRI, 0, 0, &
    'End of column base head boundary condition file (BHB)')


ENDIF  
END SUBROUTINE VSPREP





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


    6 END DO  
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


   10 END DO  
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


   14       END DO  
! set up cubic spline coefficients for theta, using log(psi)
! based on routines 'spline' and 'splint' in NUMERICAL RECIPES
! FOR FORTRAN (..UNFINISHED), pp 109 and 110
! NB asasumes 'natural' boundary conditions (ie zero 2nd derivatives)
      DO 16 I = 1, IVSNTB (IS)  
         XDUM (I) = DLOG10 ( - TBPSI (I, IS) )  
         YDUM (I) = TBTHE (I, IS)  
   16       END DO  

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
   17       END DO  
      DO 18 I = NDUM - 1, 1, - 1  
         Y2DUM (I) = Y2DUM (I) * Y2DUM (I + 1) + UDUM (I)  
   18       END DO  
      DO 19 I = 1, NDUM  
         TBTHEC (I, IS) = Y2DUM (I)  


   19       END DO  
! if required, set up cubic spline coefficients for Kr similarly

      IF (IVSFLG (IS) .EQ.2) THEN  
         DO 21 I = 1, IVSNTB (IS)  
            YDUM (I) = TBKR (I, IS)  

   21          END DO  
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
   22          END DO  
         DO 23 I = NDUM - 1, 1, - 1  
            Y2DUM (I) = Y2DUM (I) * Y2DUM (I + 1) + UDUM (I)  
   23          END DO  
         DO 24 I = 1, NDUM  
            TBKRC (I, IS) = Y2DUM (I)  

   24          END DO  

      ENDIF  

   ENDIF  



   15 END DO  
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
   30 END DO  



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
   40 END DO  



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
   DO 50 IEL = 1, NELEE  
      DO 50 ILYR = 1, NLYREE  
         IVSDUM (IEL, ILYR) = 0  
         RVSDUM (IEL, ILYR) = zero  


   50    CONTINUE  
! read layer data


   CALL ALREAD (6, VSD, PPPRI, ':VS08a', NELEE, NLYREE, NUM_CATEGORIES_TYPES,  CDUM, &
    IVSDUM, RVSDUM)
! for NUM_CATEGORIES_TYPES = 1, set all elements = category 1
   IF (NUM_CATEGORIES_TYPES == 1) THEN  
      DO 100 IEL = 1, total_no_elements  
         IVSCAT (IEL) = 1  


  100       END DO  
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
  200          END DO  
  300       END DO  


   ENDIF  
! move layer data into elements for ...
   NCOUNT = 0  

   DO 400 IEL = 1, total_no_elements  

      IF (ICMREF (IEL, 1) .EQ.1.OR.ICMREF (IEL, 1) .EQ.2.OR. ( &
       .NOT.BEXBK.AND.ICMREF (IEL, 1) .EQ.3) ) GOTO 400
      IF (IVSCAT (IEL) .EQ.0) THEN  

         NCOUNT = NCOUNT + 1  
      ELSE  

         BDONE (IEL) = .TRUE.  
         ICAT = IVSCAT (IEL)  
         ICOUNT = 0  
  350          IF (IVSDUM (ICAT, ICOUNT + 1) .EQ.0) GOTO 355  
         ICOUNT = ICOUNT + 1  
         GOTO 350  


  355          CONTINUE  
! ...grids
         IF (ICMREF (IEL, 1) .EQ.0) THEN  
            NLYR (IEL) = ICOUNT  
            DO 360 ILYR = 1, NLYR (IEL)  
               NTSOIL (IEL, ILYR) = IVSDUM (ICAT, ILYR)  
               ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - RVSDUM (ICAT, &
                ILYR)


  360             END DO  
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
  370                END DO  


  380             END DO  
! ...links
!    (NB uses data from bank 2, which is identical to bank 1)
            LCOUNT = 0  
  390             IF (RVSDUM (ICAT, LCOUNT + 1) .LT.ZGRUND (IBK) &
             - ZBEFF (IEL) + VSZMIN) GOTO 395
            LCOUNT = LCOUNT + 1  

            GOTO 390  
  395             NLYR (IEL) = LCOUNT  
            DO 397 ILYR = 1, NLYR (IEL)  
               NTSOIL (IEL, ILYR) = NTSOIL (IBK, ILYR)  
               ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)  
  397             END DO  

         ENDIF  

      ENDIF  

  400    END DO  


ENDIF  
! check no. of category elements consistent with no. of individual eleme
IF (NCOUNT.NE.NELEM) THEN  
   WRITE (MSG, 9000) NCOUNT  
   CALL ERROR(FFFATAL, 1032, PPPRI, 0, 0, MSG)  


ENDIF  
! --- element data


IF (NELEM.NE.0) THEN  
! initialise variables
   DO 420 IEL = 1, NELEE  
      DO 420 ILYR = 1, NLYREE  
         IVSDUM (IEL, ILYR) = 0  
         RVSDUM (IEL, ILYR) = zero  


  420    CONTINUE  
! read layer data

   CALL ALREAD (6, VSD, PPPRI, ':VS08d', NELEE, NLYREE, NELEM, CDUM, &
    IVSDUM, RVSDUM)


   DO 500 IEL = 1, total_no_elements  
! ignore banks, links (if no banks), and elements already processed


      IF (BDONE (IEL) .OR.ICMREF (IEL, 1) .EQ.1.OR.ICMREF (IEL, 1) &
       .EQ.2.OR. (.NOT.BEXBK.AND.ICMREF (IEL, 1) .EQ.3) ) GOTO 500
! move layer data into elements for ...

      BDONE (IEL) = .TRUE.  
      ICOUNT = 0  
  450       IF (IVSDUM (IEL, ICOUNT + 1) .EQ.0) GOTO 455  
      ICOUNT = ICOUNT + 1  
      GOTO 450  


  455       CONTINUE  
! ...grids
      IF (ICMREF (IEL, 1) .EQ.0) THEN  
         NLYR (IEL) = ICOUNT  
         DO 460 ILYR = 1, NLYR (IEL)  
            NTSOIL (IEL, ILYR) = IVSDUM (IEL, ILYR)  
            ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - RVSDUM (IEL, ILYR)  


  460          END DO  
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
  470             END DO  


  480          END DO  
! ...links
!    (NB uses data from bank 2, which is identical to bank 1)
         LCOUNT = 0  
  490          IF (RVSDUM (IEL, LCOUNT + 1) .LT.ZGRUND (IBK) - ZBEFF ( &
          IEL) + VSZMIN) GOTO 495
         LCOUNT = LCOUNT + 1  

         GOTO 490  
  495          NLYR (IEL) = LCOUNT  
         DO 497 ILYR = 1, NLYR (IEL)  
            NTSOIL (IEL, ILYR) = NTSOIL (IBK, ILYR)  
            ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)  
  497          END DO  

      ENDIF  

  500    END DO  


ENDIF  
! adjust horizon boundaries in soil zone to match computational mesh
! and set up ZLYRBT for ground surface
DO 550 IEL = NGDBGN, total_no_elements  
   DO 540 ILYR = NLYR (IEL), 1, - 1  
      IF (ZGRUND (IEL) - ZLYRBT (IEL, ILYR) .GT.DCSTOT + VSZMIN) &
       GOTO 545
      DO 530 I = 1, NCSZON + 1  
         IF (DCSNOD (I) .GT.ZGRUND (IEL) - ZLYRBT (IEL, ILYR) ) &
          THEN
            ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - DCSDUM (I - 1)  
            GOTO 540  
         ENDIF  
  530       END DO  
  540    END DO  
  545    ZLYRBT (IEL, NLYR (IEL) + 1) = ZGRUND (IEL)  

  550 END DO  
IF (BEXBK) THEN  
   DO 560 IEL = 1, total_no_links 
      IBK = ICMBK (IEL, 1)  
      DO 555 ILYR = 1, NLYR (IEL)  
         ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)  
  555       END DO  
  560    END DO  


ENDIF  
! check that all elements have been set up
DO 650 IEL = 1, total_no_elements  
   IF (.NOT.BEXBK.AND.ICMREF (IEL, 1) .NE.0) GOTO 650  
   IF (.NOT.BDONE (IEL) ) THEN  
      NVSERR = NVSERR + 1  
      WRITE (MSG, 9020) IEL  
      CALL ERROR (EEERR, 1033, PPPRI, 0, 0, MSG)  
   ENDIF  


  650 END DO  
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


  700    END DO  
! adjust horizon boundaries in river bed to match computational mesh
! and set up ZLYRBT for river bed surface
   DO 750 IEL = 1, total_no_links
      DO 740 ILYR = NLYR (IEL), 1, - 1  
         IF (ZGRUND (IEL) - ZLYRBT (IEL, ILYR) .GT.DCRTOT + &
          VSZMIN) GOTO 745
         DO 730 I = 1, NCRBED+1  
            IF (DCRNOD (I) .GT.ZGRUND (IEL) - ZLYRBT (IEL, ILYR) ) &
             THEN
               ZLYRBT (IEL, ILYR) = ZBEFF (IEL) - DCRDUM (I - 1)  
               GOTO 740  
            ENDIF  
  730          END DO  
  740       END DO  
  745       ZLYRBT (IEL, NLYR (IEL) + 1) = ZBEFF (IEL)  

  750    END DO  


ENDIF  
! VS10 ----- aquifer zone user-defined connectivities

CALL ALREAD (2, VSD, PPPRI, ':VS10', 1, 1, 0, CDUM, NAQCON, DUMMY)  


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


  800    END DO  
! VS12b ---- depth below ground of bottom and top of well screen
   CALL ALREAD (3, VSD, PPPRI, ':VS12b', 2, NW, 0, CDUM, IDUM, &
    DUMMY)
   DO 810 IW = 1, NW  
      VSZWLB (IW) = DUMMY (2 * (IW - 1) + 1)  

      VSZWLT (IW) = DUMMY (2 * (IW - 1) + 2)  

  810    END DO  


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


  860    END DO  
! VS13b ---- depth of spring source below ground, elevation of
!            discharge point, spring coefficient
   CALL ALREAD (3, VSD, PPPRI, ':VS13b', 3, NSP, 0, CDUM, IDUM1, DUMMY)
   DO 865 ISP = 1, NSP  
      IEL = IDUM (2 * (ISP - 1) + 1)  
      VSSPD (IEL) = DUMMY (3 * (ISP - 1) + 1)  
      VSSPZ (IEL) = DUMMY (3 * (ISP - 1) + 2)  
      VSSPCO (IEL) = DUMMY (3 * (ISP - 1) + 3)  

  865    END DO  


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
  910       END DO  


  920    END DO  
! VS15 ----- grid of category numbers
   CALL ALREAD (4, VSD, PPPRI, ':VS15', NX, NY, NDUM, CDUM, IDUM, &
    DUMMY)
   DO 940 IY = 1, NY  
      IXY0 = (IY - 1) * NX  
      DO 930 IX = 1, NX  
         IEL = ICMXY (IX, IY)  
         IF (IEL.NE.0) NLBCAT (IEL) = MAX (1, IDUM (IXY0 + IX) )  
  930       END DO  



  940    END DO  
! VS16 ----- No. of lateral boundary categories (flow, head, and head gr
! with b.c/s set only on selected layers
! initialise arrays to default values for reading in time-series data
   DO 840 ICAT = 1, NDUM  
      NVSLFN (ICAT) = 0  
      NVSLHN (ICAT) = 0  
      NVSLGN (ICAT) = 0  

  840    END DO  
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
  862          END DO  
      ENDIF  
      IF (ITYP.EQ.4) THEN  
         NVSLHN (ICAT) = NLDUM  
         NVSLHT = NVSLHT + NLDUM - 1  
         DO 864 I = 1, NLDUM  
            NVSLHL (I, ICAT) = IDUM (I)  
  864          END DO  
      ENDIF  
      IF (ITYP.EQ.5) THEN  
         NVSLGN (ICAT) = NLDUM  
         NVSLGT = NVSLGT + NLDUM - 1  
         DO 866 I = 1, NLDUM  
            NVSLGL (I, ICAT) = IDUM (I)  
  866          END DO  

      ENDIF  

  880    END DO  


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

  945       END DO  

   ENDIF  
   CALL ALREAD (4, VSD, PPPRI, ':VS17', NX, NY, NDUM, CDUM, IDUM, &
    DUMMY)
   DO 960 IY = 1, NY  
      IXY0 = (IY - 1) * NX  
      DO 950 IX = 1, NX  
         IEL = ICMXY (IX, IY)  
         IF (IEL.NE.0) NBBTYP (IEL) = IDUM (IXY0 + IX)  
  950       END DO  


  960    END DO  
! VS18 ----- grid of category numbers

   IF (total_no_links.GT.0.AND.BEXBK) THEN  
      CALL ALREAD (2, VSD, PPPRI, ':VS18', total_no_links, 1, 1, CDUM, IDUM, &
       DUMMY)
      DO 965 IEL = 1, total_no_links  
         ICAT = MAX (1, IDUM (IEL) )  
         NBBCAT (IEL) = ICAT  
         NBBCAT (total_no_links + IEL) = ICAT  
         NBBCAT (2 * total_no_links + IEL) = ICAT  

  965       END DO  

   ENDIF  
   CALL ALREAD (4, VSD, PPPRI, ':VS18', NX, NY, NDUM, CDUM, IDUM, &
    DUMMY)
   DO 980 IY = 1, NY  
      IXY0 = (IY - 1) * NX  
      DO 970 IX = 1, NX  
         IEL = ICMXY (IX, IY)  
         IF (IEL.NE.0) NBBCAT (IEL) = MAX (1, IDUM (IXY0 + IX) )  
  970       END DO  

  980    END DO  


ENDIF  
! FORMAT statements

 9000 FORMAT('Error in number of VSS layer elements. ', &
&       'NELEM should be ',I4)

 9020 FORMAT('Error reading VSS layers for element ',I4, '.')  

 9030 FORMAT('Soil type ',I4,' not expected for soil property tables.')  
 9999 RETURN  
!----------------------------------------------------------------------*
! Exit conditions:
! for each e in 1:NEL: 0<= NLBTYP(e), NBBTYP(e)
!                      1<= NLBCAT(e), NBBCAT(e), NVSWLC(e)
!----------------------------------------------------------------------*
END SUBROUTINE VSREAD



!SSSSSS SUBROUTINE VSSAI (FACE, JCBC, ICBOT, ICTOP, ICBED, CDELL, CZ, &
SUBROUTINE VSSAI (FACE, JCBC, ICBOT, ICTOP, ICBED, CDELL, CZ, &
 CAIJ, CZS, CPSI, CKIJ, CDKIJ, CB, CR, CQH, depadj, cdelz)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!!!! depadj added, SPA, 03/11/98
!----------------------------------------------------------------------*
! Sets up coefficients for column stream-aquifer interaction
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSSAI/4.1
! Modifications:
!  GP  22.08.94  written (v4.0 finished 15.01.96)
! RAH  970121  4.1  IDUM is INTEGER not DOUBLEPRECISION.
!                   Use AOL,DH,KIJ to reduce number of operations.
!      970203       Use arguments, not INCLUDE.  Add some comments.
!      970211       Remove outputs CQBKB,CQBKF (see VSSIM).
!      970514       Add arg FACE & 1st dim to arrays CAIJ & CQH.
!----------------------------------------------------------------------*
! Entry conditions:
!     1 <= FACE <= 4
! ICBOT <= ICBED+1, ICTOP
!     0 <  CDELL
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: FACE, JCBC, ICBOT, ICTOP, ICBED  
DOUBLEPRECISION CDELL, CZ (ICBOT:ICTOP), CAIJ (4, ICBOT:ICTOP)  
DOUBLEPRECISION CZS, CPSI (ICBOT:ICTOP)  
DOUBLEPRECISION CKIJ (ICBOT:ICTOP), CDKIJ (ICBOT:ICTOP)  
!!!!!! SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

DOUBLEPRECISION depadj  
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! In+out arguments

DOUBLEPRECISION CB (ICBOT:ICTOP), CR (ICBOT:ICTOP)  
! Output arguments

DOUBLEPRECISION CQH (4, ICBOT:ICTOP)  
! Locals, etc
INTEGER :: ICL, IDUM  
DOUBLEPRECISION QDUM, DQDUM, AOL, DH, KIJ  
! !!!! SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^



DOUBLEPRECISION ddum, cdelz (icbot:ictop)  
!^^^^^^^^^^^^^^^^^^^^^^^^^^
!----------------------------------------------------------------------*
! set lowest cell in exposed bank face
IF (JCBC.EQ.9) THEN  
!        * in effect stream bed is at base of current land element
   IDUM = ICBOT  
ELSE  
!        * stream-aquifer interaction with banks
   IDUM = ICBED+1  


ENDIF  
! loop over appropriate cells

DO 200 ICL = IDUM, ICTOP  

   DH = CZS - CZ (ICL) - CPSI (ICL)  
! !!!!! change to calculation of AOL for flow out of channel
! limits flows if depth of water in channel is low, or zero
! SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   ddum = 1.0  
   if (GTZERO(dh)) ddum = min (one, depadj / cdelz (icl) )  

   AOL = (ddum * CAIJ (FACE, ICL) ) / CDELL  
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   KIJ = CKIJ (ICL)  
! !!!! SPA, 03/11/98.  Change definition of flow derivative
!        DQDUM =   ( CDKIJ(ICL)*DH - KIJ ) * AOL
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   dqdum = - kij * aol  
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   QDUM = KIJ * DH * AOL  
   CQH (FACE, ICL) = QDUM  
   CB (ICL) = CB (ICL) + DQDUM  

   CR (ICL) = CR (ICL) - QDUM  

  200 END DO  
END SUBROUTINE VSSAI




!SSSSSS SUBROUTINE VSSIM ()  
SUBROUTINE VSSIM ()  
!----------------------------------------------------------------------*
! VSS Controlling routine for a single timestep
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSSIM/4.2
! Modifications:
!  GP  29.07.94  written (v4.0 finished 17.07.96)
! RAH  961228  4.1  Remove temporary debug code.  DPSIEL,DPSIMX >=0.
!                   Bring CWV,CWL from VSCOLM.INC, and pass to VSCOLM.
!      970207       Dispense with CNOW,CTHEN,CV,CWV,CWL,VSPOR1,VSSTMP.
!                   Replace CQINF with CQV(ICTOP).  Use DO 660 not GOTO.
!                   CQWI is redefined - see VSWELL.  asum QVSWEL locally.
!                   Use OK to simplify convergence test.
!      970210       Remove CETAN,CKRN.CPSIM,NVSCIT. Make PSIM 1D not 2D.
!                   Dispense with BCHELE,CA0,CPSIN,CPSL,CQSP,CZG,DT.
!                   Use ALINIT and DCOPY.  Bring VSPSIN,VSTHEN from
!                   VSCOM1.INC and reverse indices.  If JEL.le.0 set
!                   JCACN=0, and don't set JCDEL* or C*IJ1,CZ1,CPSI*1.
!                   Move CQH initialization to VSCOLM.  Move SIGMA from
!                   VSCOLM.INC to VSINTC.  Set ICTOP,QH,QVSBF,QBK* once.
!      970211       Replace CES,CDW,CEW,CQP with CDNET, bring CQ from
!                   VSCOLM.INC (with ICBED,ICLYRB,ICSOIL), add dim, set
!                   once. Scrap ICWL*,ICSP*,CZSP,CCS.Initialize QH,QVSH.
!      970213       VSCOLM.INC: bring JCBC,ICWCAT,ICLBCT,ICBBCT,CZS, and
!                   scrap CQWIN,CLF,ICLFL,ICLFN,CLH,ICLHL,ICLHN,CLG,
!                   ICLGL,ICLGN,CBF,CBH. Include VSSOIL.INC. rm NVSSPT,
!                   NVSWLT (AL.C).  JCBC: add dimension; define once.
!                   Swap subscripts on NVSL*L,RL*NOW (in VSCOM1.INC).
!      970214       Bring from VSCOLM.INC: CDELL,CDELL1,CAIJ,CAIJ1.
!                   Replace CAIJ with VSAIJ: set once; use for CAIJ1.
!                   Reverse DELTAZ,QVSH subscripts (in AL.C); pass to
!                   VSCOLM; scrap CDELZ,CQH.
!      970217       Swap subscripts: JVSACN,JVSDEL,ZVSNOD,QVSV,QVSWLI,
!                   VSPSI,VSTHE (see AL.C), & IVSSTO,VSKR (VSCOM1.INC)
!                   (also fixes error whereby ICSTOR not initialized).
!                   VSCOLM.INC: scrap JCACN,JCDEL,CZ,CQV,CQWI,CPSI,
!                   ICSTOR,CTHETA,CKR; bring remainder (CPSI1,CPSIN1,
!                   CZ1,CKIJ1,JCDEL1).  Add dimension to ICSOIL: set
!                   once; scrap CKZS,CKIJS (use VSK3D); use for CKIJ1.
!                   Redefine CQ: multiply by AREA*DELTAZ (see VSINTC).
!                   Move QVSWEL outside loop.  VSMB straight after loop.
!      970515       Re-order VSCOLM arguments.
!      970522       Don't need MAX for ICWLBT,etc.
!      970618       Don't call VSCOLP.  DO 285 if JEL.GE.ISTART (was 1).
! RAH  980402  4.2  Pass new local ELEVEL to VSCOLM.
!JE   JAN 2009      Loop restructure for AD
!----------------------------------------------------------------------*
! Entry conditions:
!   1 <= LLEE,  NEL,   NLFEE
!  LL <= LLEE;  NEL <= NELEE;  0 <= NLF <= NLFEE
! for each e in 1:NEL:    LLEE >= LL =NLYRBT(e,NLYR(e)+1)
!     for each layer in 1:NLYR(e): 1<=NLYRBT(e,layer)<=NLYRBT(e,layer+1)
! for each link in 1:NLF: for each face in 1:4:
!    BEXBK  ==>  1 <= ICMREF(jel,1) <= 3,  where jel=ICMREF(link,face+4)
! for each e in istart:NEL: 0 <= NLBTYP(e), NBBTYP(e)
!                           1 <= NLBCAT(e), NBBCAT(e), NVSWLC(e)
!                           1 <= NWELBT(e) <= NWELTP(e) <= LLEE
!                           4 >= NBFACE(e)
!                       NVSEE >= NVSWLI(e)
!         NLBTYP(e)>0  ==>  0 <  NBFACE(e)
!                           0  = NVSWLI(e)*NVSSPC(e)
!     for each face in 1:4:
!         ICMREF(e,4+face)<istart ==> JVSACN(face,NLYRBT(e,1):LL,e) = 0,
!  where istart=1 if BEXBK, NLF+1 otherwise
! for each e in NLF+1:NEL:  1 <= NVC(e) <= NV (size of NRD array)
!            LL-NLYRBT(e,1)+1 >= NRD(veg),
!                              where veg=NVC(e)
! ...
!----------------------------------------------------------------------*
! Limited ranges:
!                    range (e,1:LLEE):  (e,NLYRBT(e,1):LL) only
!                    range    1:NLFEE:               1:NLF only
! JVSACN(face,cell,e), NLYR(e), NTSOIL(e,layer), NVSWLI(e):
!              for e in istart:NEL only, where istart is defined above
! VSKR(e,cell): input for any e having a neighbour earlier in ISORT list
!              output for e in istart:NEL
! ...
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P.VSS:        LLEE,NELEE,NLFEE,NLYREE,NSEE,NVSEE
!     VSSOIL.INC:      NSOLEE
! Input common
!     AL.C:            BFB,BHB,LFB,LGB,LHB,LL,NEL,NLF,NS,PRI,WLD,ERR
!                      JVSACN(4,LLEE,NEL),JVSDEL(4,LLEE,NEL)
!                      NHBED(NLFEE,2),NLYR(NEL),NLYRBT(NELEE,NLYREE)
!                      NRD(*),NTSOIL(NELEE,NLYREE),NVC(NLF+1:NEL)
!                      NVSSPC(NEL),NVSWLI(NEL),NWELBT(NEL),NWELTP(NEL)
!                      TIH,AREA(NEL),DELTAZ(LLEE,NEL),DHF(NELEE,4)
!                      VSPOR(NS),ZGRUND(NEL),ZVSNOD(LLEE,NEL)
!                      BEXBK,LINKNS(NLFEE)
!                      ISORT(NEL)
!                      DTUZ,UZNEXT,UZNOW,EEVAP(NEL)
!                      ERUZ(NELEE,LL),ESOILA(NEL),HRF(NEL),PNETTO(NEL)
!     AL.G:            ICMREF(NELEE,12)
!     VSCOM1.INC:      NVSBF,NVSBH, NLBCAT(NEL),  NLBTYP(NEL)
!                      NVSLF,NVSLFT,NVSLFN(NVSEE),NVSLFL(NLYREE,NVSEE)
!                      NVSLG,NVSLGT,NVSLGN(NVSEE),NVSLGL(NLYREE,NVSEE)
!                      NVSLH,NVSLHT,NVSLHN(NVSEE),NVSLHL(NLYREE,NVSEE)
!                      NVSWL,NVSWLC(NEL),         NBBTYP(NEL)
!                      VSWL,VSWV,VSK3D(NSEE,3),VSSPZ(NEL),VSSPCO(NEL)
!                      BHELEV
!     VSSOIL.INC:      NVSSOL
!                      VSPDET(NSOLEE,NS),VSPDKR(NSOLEE,NS)
!                      VSPETA(NSOLEE,NS), VSPKR(NSOLEE,NS)
!                      VSPPSI(NVSSOL),   VSPTHE(NSOLEE,NS)
! In+out common
!     AL.C:             VSPSI(LLEE,NEL),VSTHE(LLEE,NEL)
!     VSCOM1.INC:      IVSSTO(LLEE,NEL), VSKR(LLEE,NEL)
! Output common
!     AL.C:            QBKB(NLFEE,2),    QH(NEL),ZVSPSL(NEL)
!                      QBKF(NLFEE,2), QVSBF(NEL),QVSWLI(LLEE,NVSEE)
!                      QBKI(NLFEE,2),QVSSPR(NEL),  QVSV(LLEE,NEL)
!                                    QVSWEL(NEL),  QVSH(4,LLEE,NELEE)
! Workspace common
!     VSCOM1.INC:      RBFNOW(NVSEE),RLFNOW(NLYREE,NVSEE)
!                      RBHNOW(NVSEE),RLGNOW(NLYREE,NVSEE)
!                       WLNOW(NVSEE),RLHNOW(NLYREE,NVSEE)
INTEGER :: NITMAX, NITMIN  
DOUBLEPRECISION GEPSMX, DRYH
PARAMETER (NITMAX = 10, NITMIN = 2, GEPSMX = 1D-4, DRYH = 1D-8)  
INTEGER :: N, IFDUM1, IFDUM2, NIT, NCELL, WET, ICDUM, K, ELEVEL  
INTEGER :: I, II, IEL, IFA, ICL, ILYR, IW, ITYPE, IBK, ISTART, &
 IBANK
INTEGER :: JEL, JFA, JCL, JCBED, JELDUM (4), JCDEL1 (LLEE, 4)  
INTEGER :: ICBOT, ICTOP, ICWCAT, ICLBCT, ICBBCT, ICBED, ICWLBT  
INTEGER :: ICLYRB (NLYREE)
DOUBLEPRECISION DPSIEL, DPSIMX, DELTAP (0:NELEE)  
DOUBLEPRECISION CDW, CES, CQW, CDNET (NELEE), CQ (LLEE, NELEE), &
 QBK, QI
DOUBLEPRECISION CA0, CDELL (4), CDELL1 (4), CAIJ1 (LLEE, 4), &
 CZ1 (LLEE, 4)
DOUBLEPRECISION DXYDUM 
DOUBLEPRECISION PSIM (LLEE), VSPSIN (LLEE, NELEE), VSTHEN (LLEE, &
 NELEE)
DOUBLEPRECISION CPSI1 (LLEE, 4), CPSIN1 (LLEE, 4), CKIJ1 (LLEE, 4) &
 , CZS (4)
integer,save :: errorcount2=0
!!!!!! Extra array: depadj - depth of surface water for adjacent
! elements - added for channel aquifer flows fix, SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
DOUBLEPRECISION depadj (4)  
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
LOGICAL :: TEST, OK (NELEE), g670
!----------------------------------------------------------------------*
! Initialization
!________________*
IF (BEXBK) THEN  
   IBANK = 1  
   ISTART = 1  
ELSE  
   IBANK = 0  
   ISTART = total_no_links + 1  
ENDIF  

ICTOP = top_cell_no  
IF (FIRSTvssim) THEN  

   FIRSTvssim = .FALSE.  
!         * set outputs & locals for non-column elements
   IF (ISTART.GT.1) CALL ALINIT (ZERO, ISTART - 1, QH)  
   DO 4 IEL = 1, ISTART - 1  
      ICBOT = NLYRBT (IEL, 1)  
      N = 4 * (ICTOP - ICBOT + 1)  
      CALL ALINIT (ZERO, N, QVSH (1, ICBOT, IEL) )  
      CALL ALINIT (ZERO, N, VSAIJsv (1, ICBOT, IEL) )  
      DO 2 ICL = ICBOT, ICTOP  
    2       ICSOILsv (ICL, IEL) = 1  

    4    END DO  
!        * set static locals for column elements
   DO 95 IEL = ISTART, total_no_elements  
! JCBC contains boundary condition types:
! 0 - bottom boundary; 1-4 - faces; 5 - well/spring
! boundary condition types are:
! 0     internal face or no-flow boundary condition
! 1     wells
! 2     springs
! 3     lateral flow
! 4     lateral head
! 5     lateral head gradient
! 6     column base flow
! 7     column base head
! 8     column base free drainage
! 9     stream-aquifer interaction (without banks)
! 10    stream-aquifer interaction (with banks)
      DO 20 II = 1, 5  
   20       JCBCsv (II, IEL) = 0  
      JCBCsv (0, IEL) = NBBTYP (IEL)  
      IFA = MAX (1, NBFACE (IEL) )  
      JCBCsv (IFA, IEL) = NLBTYP (IEL)  
      IF (NVSWLI (IEL) .GT.0) JCBCsv (5, IEL) = 1  
      IF (NVSSPC (IEL) .GT.0) JCBCsv (5, IEL) = 2  
      DO 90 IFA = 1, 4  
         JEL = ICMREF (IEL, IFA + 4)  
         TEST = IEL.GT.total_no_links.AND.JEL.GE.1.AND.JEL.LE.total_no_links  
         IF (TEST) JCBCsv (IFA, IEL) = 9 + IBANK  
! VSAIJ contains cell-face areas for lateral flow (note face 1=3, 2=4)
         IFDUM1 = MOD (IFA, 4) + 1  
         IFDUM2 = MOD (IFA + 2, 4) + 1  
         DXYDUM = DHF (IEL, IFDUM1) + DHF (IEL, IFDUM2)  
         DO 50 ICL = NLYRBT (IEL, 1), ICTOP  
   50          VSAIJsv (IFA, ICL, IEL) = DELTAZ (ICL, IEL) * DXYDUM  
   90       END DO  
! ICSOIL contains soil types for each cell
      DO 93 ILYR = 1, NLYR (IEL)  
         N = NTSOIL (IEL, ILYR)  
         DO 92 ICL = NLYRBT (IEL, ILYR), NLYRBT (IEL, ILYR + 1) &
          - 1
   92          ICSOILsv (ICL, IEL) = N  
   93       END DO  

   95    END DO  



ENDIF  
! prepare catchment boundary condition data

CALL VSPREP  
!!!!!! Calc. depth of water for channel links, even if no banks
! n.b. rainfall and evap terms neglected, as these are calculated for
! channels after VSS is called.
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if (.not.bexbk) then  
   do 107 iel = 1, total_no_links  
      cdnet (iel) = GEThrf (iel) - zgrund (iel)  
  107    end do  

endif  
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

DO 108 IEL = ISTART, total_no_elements  

   CES = ESOILA (IEL)  
   CDW = GETHRF (IEL) - ZGRUND (IEL)  

   CDNET (IEL) = (PNETTO (IEL) - (EEVAP (IEL) - CES) ) * DTUZ + &
    CDW
   CA0 = cellarea (IEL)  
   ICBOT = NLYRBT (IEL, 1)  
   ICDUM = ICTOP + 1  
   IF (IEL.GT.total_no_links) ICDUM = ICDUM - NRD (NVC (IEL) )  
   IF (ICDUM.GT.ICBOT) CALL ALINIT (ZERO, ICDUM - ICBOT, CQ ( &
    ICBOT, IEL) )

! stop crash if rooting zone is below base of aquifer sb 020211
    icdum=max(1,icdum)

   DO 106 ICL = ICDUM, ICTOP  
      CQ (ICL, IEL) = - ERUZ (IEL, ICL) * CA0  
  106    END DO  

   CQ (ICTOP, IEL) = CQ (ICTOP, IEL) - CES * CA0  



  108 END DO  
! save psi values at time level N
DO 212 IEL = 1, total_no_elements  
   ICBOT = NLYRBT (IEL, 1)  
   NCELL = ICTOP - ICBOT + 1  
   CALL DCOPY (NCELL, VSPSI (ICBOT, IEL), 1, VSPSIN (ICBOT, IEL), &
    1)
   CALL DCOPY (NCELL, VSTHE (ICBOT, IEL), 1, VSTHEN (ICBOT, IEL), &
    1)



  212 END DO  
! initialize convergence indicators
CALL ALINIT (ZERO, ISTART, DELTAP)  
DO 214 IEL = 1, ISTART - 1  
  214 OK (IEL) = .TRUE.  
DO 216 IEL = ISTART, total_no_elements  





  216 OK (IEL) = .FALSE.  
! start of main iteration loop
!______________________________*
ELEVEL = 0  

g670=.FALSE.
out660 : DO NIT = 1, NITMAX
    IF(g670) CYCLE  
   IF (NIT.EQ.NITMAX) ELEVEL = EEERR  
   DPSIMX = ZERO  

   DO 500 I = 1, total_no_elements  
      IEL = ISORT (I)  
      IF (OK (IEL) ) GOTO 500  
!                        >>>>>>>>
      ICBOT = NLYRBT (IEL, 1)  
      ITYPE = ICMREF (IEL, 1)  


      NCELL = ICTOP - ICBOT + 1  
! save psi at iteration level m


      CALL DCOPY (NCELL, VSPSI (ICBOT, IEL), 1, PSIM (ICBOT), &
       1)
! set up column arrays using global arrays
      DO 250 ILYR = 1, NLYR (IEL) + 1  

  250       ICLYRB (ILYR) = NLYRBT (IEL, ILYR)  

      IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) ICBED = NHBED (ICMREF (IEL, 4) &
       , ITYPE)

      DO 300 IFA = 1, 4  
         CDELL (IFA) = DHF (IEL, IFA)  
         JEL = ICMREF (IEL, IFA + 4)  

         JELDUM (IFA) = JEL  
         IF (JEL.LT.1) THEN  
            DXYDUM = ZERO  
         ELSE  

            CZS (IFA) = GETHRF (JEL)  
! !!!!! fix for channel aquifer flows, SPA, 03/11/98
! Pass depth of water in adjacent elements to vscolm
! as well as elevation of water surface
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            depadj (ifa) = cdnet (jel)  
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            JFA = ICMREF (IEL, IFA + 8)  
            DXYDUM = DHF (JEL, JFA)  
         ENDIF  
         CDELL1 (IFA) = DXYDUM  

         IF (JEL.LT.ISTART) GOTO 300  
!                                 >>>>>>>>
!              NB: VSPSI, VSKR may hold values from previous iteration
         K = MOD (JFA - 1, 2) + 1  
         DO 285 JCL = NLYRBT (JEL, 1), top_cell_no  
            JCDEL1 (JCL, IFA) = JVSDEL (JFA, JCL, JEL)  
            CAIJ1 (JCL, IFA) = VSAIJsv (JFA, JCL, JEL)  
            CZ1 (JCL, IFA) = ZVSNOD (JCL, JEL)  
            CPSI1 (JCL, IFA) = VSPSI (JCL, JEL)  
            CPSIN1 (JCL, IFA) = VSPSIN (JCL, JEL)  
            N = ICSOILsv (JCL, JEL)  
            CKIJ1 (JCL, IFA) = VSKR (JCL, JEL) * VSK3D (N, K)  

  285          END DO  


  300       END DO  
! boundary condition indices
      IW = MAX (1, NVSWLI (IEL) )  
      ICWLBT = NWELBT (IEL)  
      ICWCAT = NVSWLC (IEL)  
      ICLBCT = NLBCAT (IEL)  


      ICBBCT = NBBCAT (IEL)  
! calculate new potentials and flow rates



      CALL VSCOLM (NSEE, VSWV, VSWL, VSK3D, BHELEV, ELEVEL, &
       IEL, ICBOT, ICTOP, ICBED, ICLYRB, ICSOILsv (ICBOT, IEL), &
       JCBCsv (0, IEL), JCDEL1, JELDUM, JVSACN (1, ICBOT, IEL), &
       JVSDEL (1, ICBOT, IEL), NVSSPC (IEL), NVSLFN (ICLBCT), &
       NVSLFL (1, ICLBCT), NWELBT (IEL), NVSLHN (ICLBCT), NVSLHL ( &
       1, ICLBCT), NWELTP (IEL), NVSLGN (ICLBCT), NVSLGL (1, &
       ICLBCT), cellarea (IEL), ZGRUND (IEL), VSSPZ (IEL), VSSPCO (IEL) &
       , DELTAZ (ICBOT, IEL), ZVSNOD (ICBOT, IEL), CDELL, VSAIJsv (1, &
       ICBOT, IEL), CAIJ1, CDELL1, CZ1, DTUZ, CDNET (IEL), VSPSIN ( &
       ICBOT, IEL), CQ (ICBOT, IEL), CZS, CPSI1, CPSIN1, CKIJ1, &
       WLNOW (ICWCAT), RLFNOW (1, ICLBCT), RLHNOW (1, ICLBCT), &
       RLGNOW (1, ICLBCT), RBFNOW (ICBBCT), RBHNOW (ICBBCT), &
       IVSSTO (ICBOT, IEL), VSPSI (ICBOT, IEL), VSKR (ICBOT, IEL), &
       VSTHE (ICBOT, IEL), QVSH (1, ICBOT, IEL), QVSV (ICBOT - 1, &
       IEL), QVSWLI (ICWLBT, IW), QVSSPR (IEL), ZVSPSL (IEL), &
       depadj)
!!!!!! extra argument depadj added for channel-aquifer flows fix
! SPA, 03/11/98
! record largest change for this iteration
      DPSIEL = ZERO  
      DO 400 ICL = ICBOT, ICTOP  
  400       DPSIEL = MAX (DPSIEL, ABS (VSPSI (ICL, IEL) - PSIM (ICL) ) )  
      DELTAP (IEL) = DPSIEL  



      DPSIMX = MAX (DPSIMX, DPSIEL)  
! end of element loop: check for convergence or maximum iterations
  500    END DO  
!970214  At present the criterion on DPSIMX overrides that on NIT
   IF (DPSIMX.LE.GEPSMX) THEN
        g670 =.TRUE.
        CYCLE out660
   ENDIF
   IF (NIT.GE.NITMIN) THEN  
      DO 650 IEL = ISTART, total_no_elements  
         DPSIEL = DELTAP (IEL)  
         DO 640 IFA = 1, 4  
            JEL = MAX (0, ICMREF (IEL, IFA + 4) )  
            DPSIEL = MAX (DPSIEL, DELTAP (JEL) )  
  640          END DO  
         OK (IEL) = DPSIEL.LT.GEPSMX  
  650       END DO  



   ENDIF  
! end of iteration loop
ENDDO out660 
IF(.NOT.g670) then
    errorcount2=errorcount2+1
    if (errorcount2.lt.errcntallowed) then    
      CALL ERROR(EEERR, 1039, PPPRI, 0, 0, 'Maximum iterations in VSS global solver')
    elseif (errorcount2.eq.errcntallowed) then
      CALL ERROR (EEERR, 1039, PPPRI, 0, 0, '**** Last printout of the error message - maximum iterations in VSS global solver *****')
    endif    
endif




  670 CONTINUE  
! main solution is complete: tidy up
!____________________________________*
! update flows to ensure mass conservation


CALL VSMB (VSTHEN)  
! set auxiliary output arrays
DO 700 IEL = ISTART, total_no_elements  
   ICBOT = NLYRBT (IEL, 1)  
   QVSBF (IEL) = QVSV (ICBOT - 1, IEL)  
   QH (IEL) = QVSV (ICTOP, IEL)  
   IW = NVSWLI (IEL)  
   IF (IW.LT.1) GOTO 700  
!                     >>>>>>>>
   CQW = ZERO  
   DO 690 ICL = NWELBT (IEL), NWELTP (IEL)  
  690    CQW = QVSWLI (ICL, IW) + CQW  
   QVSWEL (IEL) = CQW  


  700 END DO  
! calculate QBKB, QBKF, QBKI for all cases:
!     bank elements or not, including dry channels
DO 780 IBK = 1, 2  

   DO 760 IEL = 1, total_no_links  
      QI = - HALF * cellarea (IEL) * QH (IEL)  

      WET = NINT (HALF + SIGN (HALF, GETHRF (IEL) - ZGRUND (IEL) &
       - DRYH) )
      IFA = 2 * IBK  

      IF (LINKNS (IEL) ) IFA = IFA - 1  
      JEL = ICMREF (IEL, IFA + 4)  

      JFA = ICMREF (IEL, IFA + 8)  
      JCBED = top_cell_no  
      IF (JEL.GT.0) JCBED = NLYRBT (JEL, 1) - 1  

      IF (BEXBK) JCBED = NHBED (IEL, IBK)  
      QBK = ZERO  
      DO 740 JCL = JCBED+1, top_cell_no  
         QBK = QBK + QVSH (JFA, JCL, JEL)  

  740       END DO  
! !!! mod.s to make definition of exchange flows consistent with balwat
! SPA, 04/11/98
      QBKF (IEL, IBK) = QBK  
      QBKB (IEL, IBK) = QI * IBANK * WET  

      QBKI (IEL, IBK) = QI * IBANK * (1 - WET)  
  760    END DO  

  780 END DO  

END SUBROUTINE VSSIM
! 26/1/96




!SSSSSS SUBROUTINE VSSOIL ()  
SUBROUTINE VSSOIL ()  
!
!----------------------------------------------------------------------*
! Sets up soil property tables for VSS
!----------------------------------------------------------------------*
! Module:        VSS (0.0)
! Program:       SHETRAN (4.0)
! Callers:       VSIN
! Modifications:
!  GP  20.07.94  written
!----------------------------------------------------------------------*
INTEGER :: I, IS, NTBPOS (NSEE), NDUM  
DOUBLEPRECISION RVSSOL, PSI, EDUM, EEDUM, DDDUM  
DOUBLEPRECISION DDTSAT, DDTRES, DDA, DDN, DDM, DD1M1, DDTSMR, &
 DDAP, DDAPN, DDAPN1, DDAPM, DDAPM1, DDAPM2, DDTCAP, DDTC, DDTCM, &
 DDTCM1, DDTCM2, DDDTCP

DOUBLEPRECISION PLOG, PLOGLO, PLOGHI, ADUM, BDUM, HDUM, rkrdum  

PARAMETER (EDUM = 2.718281828D0)  


DATA NTBPOS / NSEE * 1 /  
!----------------------------------------------------------------------*
! soil flags:
!       1       van Genuchten
!       2       tabulated theta(psi) and Kr(psi)
!       3       exponential
!       4       tabulated theta(psi), Averjanov Kr (compatible with V3.4
!----------------------------------------------------------------------*
!
! set up size of internal look-up tables
IF (BFAST) THEN  
   NVSSOL = MIN0 (100, NSOLEE)  
ELSE  
   NVSSOL = MIN0 (500, NSOLEE)  
ENDIF  


RVSSOL = DBLE (NVSSOL)  
! loop over NVSSOL divisions of the soil property tables
! (NB. low values of I correspond to wet soils)
! psi ranges from -(10**-2) to -(10**4)

DO 500 I = 5, NVSSOL - 1  
   PSI = - 10.D0** ( - two + DBLE (6 * (I - 5) ) / RVSSOL)  


   VSPPSI (I) = PSI  
! set up property data for each soil type, using method ...


   DO 400 IS = 1, NS  
! ... 1 (Van Genuchten)

      IF (IVSFLG (IS) .EQ.1) THEN  
         DDTSAT = VSPOR (IS)  
         DDTRES = VSTRES (IS)  
         DDA = VSALPH (IS) * 100.0d0  

         DDN = VSVGN (IS)  
         DDM = one - (one / DDN)  

         DD1M1 = (one / DDM) - one  

         DDTSMR = DDTSAT - DDTRES  
         DDAP = - DDA * PSI  
         DDAPN = DDAP**DDN  
         DDAPN1 = DDAP** (DDN - one)  
         DDAPM = (one + DDAPN) **DDM  
         DDAPM1 = (one + DDAPN) ** (DDM + one)  

         DDAPM2 = (one + DDAPN) ** (DDM + two)  

         DDDTCP = DDA * DDM * DDN * DDAPN1 / DDAPM1  

         VSPTHE (I, IS) = DDTRES + DDTSMR / DDAPM  

         VSPDTH (I, IS) = DDTSMR * DDDTCP  
         DDTCAP = MAX (1.0d-10, (VSPTHE (I, IS) - DDTRES) &
          / DDTSMR)
         DDTC = one - (DDTCAP** (one / DDM) )  
         DDTCM = DDTC**DDM  
         DDTCM1 = DDTC** (DDM - one)  

         DDTCM2 = (one - DDTCM) **two  


         VSPKR (I, IS) = DSQRT (DDTCAP) * DDTCM2  
!            VSPDKR(I,IS) = DSQRT(DDTCAP)*(one-DDTCM)*
!     -        (half*(one-DDTCM)/DDTCAP +
!     -         two*DDTCM1*DDTCAP**DD1M1) * DDDTCP

         DDDUM = (DDA * DDA * DDM * DDN * DDTSMR * DDAPN1 / &
          DDAPM2) * ( (DDN - one) * (one + DDAPN) + (DDM + &
          one) * DDN * DDAPN1)
         VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
          + VSPDTH (I, IS)
!cc            VSPDET(I,IS) = VSPDTH(I,IS)*VSPSS(IS)/VSPOR(IS) +
!cc     -                     DDDUM


         vspdet (i, is) = zero  
! ... 2 (tabulated theta and Kr)


      ELSEIF (IVSFLG (IS) .EQ.2) THEN  
!               check for correct location in input table
!               (interpolate between positions NTBPOS(IS) and NTBPOS(IS+
         IF (PSI.LT.TBPSI (NTBPOS (IS) + 1, IS) ) NTBPOS (IS) &
          = NTBPOS (IS) + 1


         NDUM = NTBPOS (IS)  
!               evaluate cubic spline polynomial for theta and Kr
         PLOG = DLOG10 ( - PSI)  
         PLOGHI = DLOG10 ( - TBPSI (NDUM + 1, IS) )  
         PLOGLO = DLOG10 ( - TBPSI (NDUM, IS) )  
         HDUM = PLOGHI - PLOGLO  
         ADUM = (PLOGHI - PLOG) / HDUM  

         BDUM = (PLOG - PLOGLO) / HDUM  
         VSPTHE (I, IS) = ADUM * TBTHE (NDUM, IS) + BDUM * TBTHE ( &
          NDUM + 1, IS) + ( (ADUM**three - ADUM) * TBTHEC (NDUM, &
          IS) + (BDUM**three - BDUM) * TBTHEC (NDUM + 1, IS) ) &
          * (HDUM**two) / 6.0D0

         VSPTHE (I, IS) = VSPOR (IS) * VSPTHE (I, IS)  


         VSPKR (I, IS) = ADUM * TBKR (NDUM, IS) + BDUM * TBKR ( &
          NDUM + 1, IS) + ( (ADUM**three - ADUM) * TBKRC (NDUM, IS) &
          + (BDUM**three - BDUM) * TBKRC (NDUM + 1, IS) ) * &
          (HDUM**two) / 6.0D0
! ... 3 (exponential)

      ELSEIF (IVSFLG (IS) .EQ.3) THEN  

         EEDUM = EDUM** (VSALPH (IS) * PSI)  
         VSPTHE (I, IS) = VSTRES (IS) + (VSPOR (IS) - VSTRES (IS) &
          ) * EEDUM
         VSPDTH (I, IS) = (VSPOR (IS) - VSTRES (IS) ) * VSALPH ( &
          IS) * EEDUM
         DDDUM = VSPDTH (I, IS) * VSALPH (IS)  
         VSPKR (I, IS) = EEDUM  

         VSPDKR (I, IS) = VSALPH (IS) * EEDUM  
         VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
          + VSPDTH (I, IS)


         VSPDET (I, IS) = VSPDTH (I, IS) * VSPSS (IS) / VSPOR (IS) &
          + DDDUM
! ... 2/4 (tabulated theta and Kr / tabulated theta and Averjanov Kr)

      ELSEIF (IVSFLG (IS) .EQ.4) THEN  

         stop 'UNFINISHED code for soil properties type 4'  

      ENDIF  

  400    END DO  


  500 END DO  
! set up property data for extreme dry conditions

VSPPSI (NVSSOL) = - 1.0D6  
DO 700 IS = 1, NS  
   VSPTHE (NVSSOL, IS) = VSTRES (IS)  
   VSPKR (NVSSOL, IS) = zero  
   VSPETA (NVSSOL, IS) = zero  
   VSPDTH (NVSSOL, IS) = zero  
   VSPDKR (NVSSOL, IS) = zero  
   VSPDET (NVSSOL, IS) = zero  


  700 END DO  
! set up storage term for tabulated data
DO 540 I = 5, NVSSOL - 1  
   DO 520 IS = 1, NS  
      IF (IVSFLG (IS) .EQ.2.OR.IVSFLG (IS) .EQ.4) VSPDTH (I, IS) &
       = (VSPTHE (I + 1, IS) - VSPTHE (I, IS) ) / (VSPPSI (I + 1) &
       - VSPPSI (I) )
      VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
       + VSPDTH (I, IS)
  520    END DO  

  540 END DO  
DO 560 I = 5, NVSSOL - 1  
   DO 550 IS = 1, NS  
      IF (IVSFLG (IS) .EQ.2.OR.IVSFLG (IS) .EQ.4) VSPDET (I, IS) &
       = VSPDTH (I, IS) * VSPSS (IS) / VSPOR (IS) + (VSPDTH (I + 1, &
       IS) - VSPDTH (I, IS) ) / (VSPPSI (I + 1) - VSPPSI (I) )
  550    END DO  


  560 END DO  
! set up property data for extreme wet conditions
VSPPSI (4) = zero  
VSPPSI (3) = 2.5d-1  
VSPPSI (2) = 5.0D-1  

VSPPSI (1) = 1.0D6  
DO 600 IS = 1, NS  
   VSPKR (4, IS) = one  
   VSPKR (3, IS) = one  
   VSPKR (2, IS) = one  
   VSPKR (1, IS) = one  
   VSPETA (4, IS) = vspeta (5, is)  
   VSPETA (3, IS) = vspeta (4, is)  
   VSPETA (2, IS) = VSPSS (IS)  
   VSPETA (1, IS) = VSPSS (IS)  
   VSPDTH (4, IS) = vspdth (5, is)  
   VSPTHE (4, IS) = vspor (is)  
   VSPTHE (3, IS) = vspor (is) + vspeta (4, is) * (vsppsi (3) &
    - vsppsi (4) )
   VSPTHE (2, IS) = vspthe (3, is) + vspeta (3, is) * (vsppsi (2) &
    - vsppsi (3) )
   VSPTHE (1, IS) = vspthe (2, is) + vspss (is) * (vsppsi (1) &
    - vsppsi (2) )
   VSPDTH (3, IS) = zero  
   VSPDTH (2, IS) = zero  
   VSPDTH (1, IS) = zero  
   VSPDKR (4, IS) = vspdkr (5, is)  
   VSPDKR (3, IS) = zero  
   VSPDKR (2, IS) = zero  
   VSPDKR (1, IS) = zero  
!        VSPDET(3,IS) = vspdet(4,is)
   VSPDET (4, IS) = zero  
   VSPDET (3, IS) = zero  
   VSPDET (2, IS) = zero  
   VSPDET (1, IS) = zero  





  600 END DO  
! adjust theta for specific storage in the unsaturated zone
!      delpsi=0.0
!      do 610 i=nvssol-1,3,-1
!        delpsi = delpsi+vspthe(i,is)*(vsppsi(i)-vsppsi(i+1))
!        do 605 is=1,ns
!          vspthe(i,is) = vspthe(i,is) *
!     -      (one + vspss(is)*delpsi/vspor(is))
! 605    continue
! 610  continue
! add increment to eta, for stability near water table
!      DO 660 IS=1,NS
!        ETAMAX = 0.0D0
!        DO 620 I=1,NVSSOL
!          ETAMAX = MAX(ETAMAX,VSPETA(I,IS))
! 620    CONTINUE
!        DO 640 I=1,NVSSOL
!          VSPETA(I,IS) = VSPETA(I,IS) +
!     -      0.1d0*ETAMAX*MAX( (1.0D0-DABS(VSPPSI(I))), 0.0D0)
! 640    CONTINUE
! 660  CONTINUE
! DSATG-specific code - adjust relative conductivity curves so that
! Kr approaches unity at saturation (for values of VG-n less than 2,
! the value of Kr drops rapidly and unphysically less than one near satu
do 680 is = 1, ns  
   rkrdum = vspor (is) - vstres (is)  
   do 670 i = 5, nvssol  
      vspkr (i, is) = ( (vspthe (i, is) - vstres (is) ) / rkrdum) &
       **two
  670    end do  



  680 end do  
! write soil property tables to PRI file

IF (BSOILP) THEN  

   WRITE(PPPRI, 905) NS, NVSSOL  
   DO 800 IS = 1, NS  
      WRITE(PPPRI, 910) IS  
      DO 820 I = 1, NVSSOL  
         WRITE(PPPRI, 920) I, VSPPSI (I), VSPTHE (I, IS), VSPETA ( &
          I, IS), VSPKR (I, IS), VSPDTH (I, IS), VSPDET (I, IS), &
          VSPDKR (I, IS)
  820       END DO  

  800    END DO  

ENDIF  

  905 FORMAT(/ 'VSS physical soil/lithology property data' / &
&         '=========================================' / &
&         I3, ' soils' / &
&         I3, ' values in soil property tables' )

  910 FORMAT(/ &
& 3X,'  Soil property tables for soil/lithology type: ',I3 / &
& 3X,'  -------------------------------------------------' // &
& 3X,'      psi         theta          eta            Kr      ', &
& ' d(the)/d(psi) d(eta)/d(psi)  d(Kr)/d(psi)' / &
& 3X,'   (VSPPSI)      (VSPTHE)      (VSPETA)       (VSPKR)   ', &
& '   (VSPDTH)      (VSPDET)       (VSPDKR)  ' / &
& 3X,'  ------------  ------------  ------------  ------------', &
& '  ------------  ------------  ------------' )

  920 FORMAT(I3,7(2X,G12.6))  
RETURN  
END SUBROUTINE VSSOIL



!SSSSSS SUBROUTINE VSSPR (CZ, CZSP, CCS, CPSI, CKR, CDKR, CB, CR, CQSP)  
SUBROUTINE VSSPR (CZ, CZSP, CCS, CPSI, CKR, CDKR, CB, CR, CQSP)  
!
!----------------------------------------------------------------------*
! Sets up coefficients for column spring discharge
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSSPR/4.1
! Modifications:
!  GP  22.08.94  written
! RAH  970120  4.1  No leading comments.  Use local DHDUM.
!      970127       Use arguments, not INCLUDE.
!----------------------------------------------------------------------*
!
! Input arguments
DOUBLEPRECISION CZ, CZSP, CCS, CPSI, CKR, CDKR  
!
! In+out arguments
DOUBLEPRECISION CB, CR  
!
! Output arguments
DOUBLEPRECISION CQSP  
!
! Locals, etc
DOUBLEPRECISION DHDUM  
!
!----------------------------------------------------------------------*
!

DHDUM = CPSI + CZ - CZSP  

IF (GEZERO(DHDUM)) THEN  

   CQSP = CCS * CKR * DHDUM  
   CR = CR + CQSP  

   CB = CB - CCS * CDKR  

ELSE  

   CQSP = zero  

ENDIF  
END SUBROUTINE VSSPR





!SSSSSS SUBROUTINE VSUPPR (CA0, CDELZ, CKZS, DT, CDNET, CPSI, CB, CR, &
SUBROUTINE VSUPPR (CA0, CDELZ, CKZS, DT, CDNET, CPSI, CB, CR, &
 CQINF)
!----------------------------------------------------------------------*
! Sets up coefficients for column upper boundary condition
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSUPPR/4.2
! Modifications:
!  GP  22.08.94  written (v4.0 finished 20.12.95)
! RAH  970120  4.1  No leading or long comments.  No lower-case code.
!                   Use MAX not MAX.  Rearrange expressions.
!                   Don't INCLUDE AL.G.
!      970127       Use arguments, not COMMON.
!      970514       Replace CDW + (CQP - CEW)*DT with CDNET (see VSSIM).
! RAH  981104  4.2  Rename locals DUM? as QIN,etc.
!----------------------------------------------------------------------*
! Entry conditions:
! 0 < CDELZ, DT
!----------------------------------------------------------------------*
! Input arguments
DOUBLEPRECISION CA0, CDELZ, CKZS, DT, CDNET, CPSI  
! In+out arguments

DOUBLEPRECISION CB, CR  
! Output arguments

DOUBLEPRECISION CQINF  
! Locals, etc
!INTRINSIC MAX  


DOUBLEPRECISION QIN, QOUT, CDQINF, DZO2  
!----------------------------------------------------------------------*
! CDNET = total net available depth of surface water after evaporation
! QIN   = infiltration rate which would exhaust CDNET
! QOUT  = exfiltration rate based on transport (-ve for infiltration)
! CQINF = calculated exfiltration rate
!         (+ve upwards, to be consistent with the global array, QH)
!----------------------------------------------------------------------*
DZO2 = half * CDELZ  
QIN = CDNET / DT  
CDQINF = CKZS / DZO2  


QOUT = CDQINF * (CPSI - (MAX (CDNET, ZERO) + DZO2) )  
! infiltration (limited by available water) or evaporation

IF (QIN.LT. - QOUT) THEN  
   CQINF = - QIN  


   CDQINF = ZERO  
! infiltration (limited by soil properties) or exfiltration

ELSE  

   CQINF = QOUT  


ENDIF  
! add into right-hand-side of column tridiagonal system
CB = CB - CDQINF * CA0  

CR = CR + CQINF * CA0  
END SUBROUTINE VSUPPR



!SSSSSS SUBROUTINE VSWELL (NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL, CA0, &
SUBROUTINE VSWELL (NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL, CA0, &
 CDELZ, CQWIN, CPSI, CR, CQWI, RKZDUM)
!----------------------------------------------------------------------*
! Sets up coefficients for column well abstraction
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSWELL/4.1
! Modifications:
!  GP  22.08.94  written (v4.0 finished 28.02.95)
! RAH  970120  4.1  Use generic intrinsics.  Use local QDUM.
!      970127       Use arguments, not INCLUDE.
!      970207       Redefine CQWI: divide by CA0.  Remove output CQW.
!      970514       New args NSEE,ICSOIL,VSK3D in place of LLEE,CKIJS.
!                   Rearrange QDUM expression.
!----------------------------------------------------------------------*
! Entry conditions:
! ICWLBT <= ICWLTP
!      1 <= ICSOIL(ICWLBT:ICWLTP) <= NSEE
!      0 < CA0, CDELZ(ICWLBT:ICWLTP+1), VSK3D(ICSOIL(ICWLBT:ICWLTP),1:2)
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: NSEE, ICWLBT, ICWLTP, ICSOIL (ICWLBT:ICWLTP)  
DOUBLEPRECISION CDELZ (ICWLBT:ICWLTP + 1), VSK3D (NSEE, 2), &
 CA0

DOUBLEPRECISION CPSI (ICWLBT:ICWLTP), CQWIN  
! In+out arguments

DOUBLEPRECISION CR (ICWLBT:ICWLTP)  
! Output arguments

DOUBLEPRECISION CQWI (ICWLBT:ICWLTP)  
! Workspace arguments

DOUBLEPRECISION RKZDUM (ICWLBT:ICWLTP)  
! Locals, etc
!INTRINSIC MAX, MIN  
INTEGER :: ICL, SOIL  




DOUBLEPRECISION RKZTOT, DZDUM, PDUM, QDUM, RKZ  
!----------------------------------------------------------------------*
! The value of CQWIN is the prescribed abstraction rate (m3/s).
! The actual abstraction rate CQWI (m/s) may be less than this if some
! of the aquifer around the well screen becomes unsaturated
! (ie if CPSI(ICL) < DZDUM below).
! calculate product of mean lateral hydraulic conductivity & cell depth
RKZTOT = ZERO  
DO 50 ICL = ICWLBT, ICWLTP  
   SOIL = ICSOIL (ICL)  
   RKZ = half * (VSK3D (SOIL, 1) + VSK3D (SOIL, 2) ) * CDELZ ( &
    ICL)
   RKZDUM (ICL) = RKZ  
   RKZTOT = RKZ + RKZTOT  


   50 END DO  
! calculate flow into well for each cell, & add into matrix coefficients

DO 100 ICL = ICWLBT, ICWLTP  
   DZDUM = half * (CDELZ (ICL) + CDELZ (ICL + 1) )  
   PDUM = MIN (DZDUM, MAX (CPSI (ICL), ZERO) )  

   QDUM = CQWIN * (RKZDUM (ICL) / RKZTOT) * (PDUM / DZDUM)  
   CQWI (ICL) = QDUM / CA0  

   CR (ICL) = QDUM + CR (ICL)  
   

  100 END DO  
END SUBROUTINE VSWELL
END MODULE VSmod