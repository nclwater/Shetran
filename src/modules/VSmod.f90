MODULE VSmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the VS .F files
   USE SGLOBAL
   USE mod_load_filedata, ONLY : ALSPRD, ALREAD
!USE SGLOBAL,  ONLY :
   USE AL_G, ONLY : ICMREF, NX, NY, ICMXY, NGDBGN
   USE AL_C, ONLY : BHB, BFB, bexbk, DTUZ, deltaz, dummy, DHF, ESOILA, ERUZ, EEVAP, &
      FHBED, ISORT, jvsacn, JVSDEL, idum, icmbk, LFB, LHB, LINKNS, lgb, &
      NWELBT, NWELTP, NVSSPC, NVSWLI, NTSOIL, nhbed, NVC, NRD, nlyrbt, NVSWLT, NVSSPT, NBFACE, NS, nlyr, &
      PNETTO, QVSSPR, QVSBF, QH, QVSWEL, QBKF, QBKB, QVSV, QVSWLI, QVSH, QBKI, &
      tih, UZNEXT, &
      vsd, VSI, VSPSI, VSTHE, VSPOR, WLD, ZVSPSL, zlyrbt, zvsnod, zbeff, INITIALISE_AL_C, INITIALISE_AL_C2, TIH
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
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: IVSDUM_VSREAD
   INTEGER, DIMENSION(:), ALLOCATABLE :: IVSCAT_VSREAD
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISDUM_VSREAD
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RVSDUM_VSREAD
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RSDUM_VSREAD
   LOGICAL, DIMENSION(:), ALLOCATABLE :: BDONE_VSREAD

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


   !SSSSSS SUBROUTINE initialise_vsread_buffers
   SUBROUTINE initialise_vsread_buffers()

      IF (.NOT. ALLOCATED(IVSDUM_VSREAD)) THEN
         ALLOCATE(IVSDUM_VSREAD(NELEE, NLYREE), IVSCAT_VSREAD(NELEE), ISDUM_VSREAD(NSEE, 8))
         ALLOCATE(RVSDUM_VSREAD(NELEE, NLYREE), RSDUM_VSREAD(NSEE, 8), BDONE_VSREAD(NELEE))
      END IF

      IVSDUM_VSREAD = 0
      IVSCAT_VSREAD = 0
      ISDUM_VSREAD = 0
      RVSDUM_VSREAD = zero
      RSDUM_VSREAD = zero
      BDONE_VSREAD = .FALSE.
   END SUBROUTINE initialise_vsread_buffers



   !SSSSSS SUBROUTINE VSBC
   SUBROUTINE VSBC(BCHELE, FACE, ICBOT, ICTOP, JCBC, ICLYRB, ICLFN, &
                   ICLFL, ICLHL, ICLHN, CZG, CDELL, CDELZ, CZ, CAIJ, CLF, CLH, CPSI, &
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
   ! 0 <= ICLFN           <= NLYREE (size of ICLFL,CLF)
   ! 0 <= ICLHN           <= NLYREE (size of ICLHL,CLH,DUM)
   ! 0 <  CDELL
   ! for each i in 1:ICLFN:
   !             1 <= ICLFL(i) < NLYREE (size of ICLYRB)
   !         ICBOT <= ICLYRB(y)
   !     1 + ICTOP >= ICLYRB(y+1)
   ! where y=ICLFL(i)
   ! for each i in 1:ICLHN:
   !             1 <= ICLHL(i) < NLYREE (size of ICLYRB)
   !         ICBOT <= ICLYRB(y)
   !     1 + ICTOP >= ICLYRB(y+1)
   ! where y=ICLHL(i)
   ! for each c in ICBOT:ICTOP: 0 < CDELZ(c), CKIJ(c)
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: BCHELE
      INTEGER, INTENT(IN) :: FACE, ICBOT, ICTOP, JCBC, ICLFN, ICLHN
      INTEGER, INTENT(IN) :: ICLYRB(*), ICLFL(*), ICLHL(*)
      DOUBLE PRECISION, INTENT(IN) :: CZG, CDELL
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP), CZ(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CAIJ(4, ICBOT:ICTOP), CPSI(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CKIJ(ICBOT:ICTOP), CDKIJ(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CLF(*), CLH(*)

      ! In+out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: CB(ICBOT:ICTOP), CR(ICBOT:ICTOP)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT)   :: CQH(4, ICBOT:ICTOP)

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: DUM(*)

      ! Locals
      INTEGER :: ICL, I, ILYR, ICL1, ICL2, IDUM, SGN
      DOUBLE PRECISION :: ADHOL, AOL, KDUM, Q, QTOT, TICL, TTOT, ZDUM

   !----------------------------------------------------------------------*

      ! flow (type 3)
      IF (JCBC == 3) THEN
         flow_loop: DO I = 1, MAX(1, ICLFN)
            IF (ICLFN == 0) THEN
               ICL1 = ICBOT
               ICL2 = ICTOP
            ELSE
               ILYR = ICLFL(I)
               ICL1 = ICLYRB(ILYR)
               ICL2 = ICLYRB(ILYR + 1) - 1
            END IF

            TTOT = 0.0D0

            calc_ttot_loop: DO ICL = ICL1, ICL2
               TICL = CKIJ(ICL) * CDELZ(ICL)
               DUM(ICL) = TICL
               TTOT = TTOT + TICL
            END DO calc_ttot_loop

            QTOT = CLF(I)

            distribute_flow_loop: DO ICL = ICL1, ICL2
               Q = (DUM(ICL) / TTOT) * QTOT
               CR(ICL) = CR(ICL) - Q
               CQH(FACE, ICL) = Q
            END DO distribute_flow_loop

         END DO flow_loop

      ! head (type 4)
      ! NB. If BCHELE=.false., head b.c.'s are depths below ground surface
      ELSE IF (JCBC == 4) THEN
         IF (BCHELE) THEN
            ZDUM = ZERO
            SGN = 1
         ELSE
            ZDUM = CZG
            SGN = -1
         END IF

         IDUM = MAX(ICLHN, 1)

         head_init_loop: DO I = 1, IDUM
            DUM(I) = ZDUM + DBLE(SGN) * CLH(I)
         END DO head_init_loop

         head_calc_loop: DO I = 1, IDUM
            IF (ICLHN == 0) THEN
               ICL1 = ICBOT
               ICL2 = ICTOP
            ELSE
               ILYR = ICLHL(I)
               ICL1 = ICLYRB(ILYR)
               ICL2 = ICLYRB(ILYR + 1) - 1
            END IF

            apply_head_loop: DO ICL = ICL1, ICL2
               AOL   = CAIJ(FACE, ICL) / CDELL
               ADHOL = (DUM(I) - CZ(ICL) - CPSI(ICL)) * AOL
               KDUM  = CKIJ(ICL)
               Q     = KDUM * ADHOL

               CB(ICL) = CB(ICL) + CDKIJ(ICL) * ADHOL + KDUM * AOL
               CR(ICL) = CR(ICL) - Q
               CQH(FACE, ICL) = Q
            END DO apply_head_loop
         END DO head_calc_loop

      ! head gradient (type 5)
      ELSE IF (JCBC == 5) THEN
         !STOP 'unfinished code for boundary type 5 - head gradients'
         PRINT *, 'unfinished code for boundary type 5 - head gradients'
      END IF

   END SUBROUTINE VSBC



   !SSSSSS SUBROUTINE VSCOEF
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
   !                       JELDUM(j)>0 and JCACN(j,i).ne.0 and JCBC(j).ne.9:
   !       1 <= k, k1 <= LLEE
   !       1 >= |JCDEL(j,i)|, |JCDEL1(k,j)|
   !       0 <  CAIJ(j,i), CAIJ1(k,j), CAIJ1(k1,j), CKIJ1(k,j), CKIJ1(k1,j)
   !    where k=JCACN(j,i), and k1=k+JCDEL1(k,j)
   ! for each j in 1:4: 0 < CDELL(j) + CDELL1(j)
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! zero, one, half, ISZERO, ISONE, NOTONE

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN)          :: LLEE, NSEE, ICBOT, ICTOP
      INTEGER, INTENT(IN)          :: JELDUM(4), JCBC(4)
      INTEGER, INTENT(IN)          :: ICSOIL(ICBOT:ICTOP), JCACN(4, ICBOT:ICTOP)
      INTEGER, INTENT(IN)          :: JCDEL1(LLEE, 4), JCDEL(4, ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CWV, CWL, VSK3D(NSEE, 3)
      DOUBLE PRECISION, INTENT(IN) :: CA0, CDELL(4), CDELZ(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CDELL1(4), CAIJ(4, ICBOT:ICTOP), CAIJ1(LLEE, 4)
      DOUBLE PRECISION, INTENT(IN) :: CKR(ICBOT:ICTOP), CDKR(ICBOT:ICTOP), CKIJ1(LLEE, 4)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CBETM(ICBOT:ICTOP + 1)
      DOUBLE PRECISION, INTENT(OUT) :: CDBETM(ICBOT:ICTOP + 1), CDBTMM(ICBOT:ICTOP + 1)
      DOUBLE PRECISION, INTENT(OUT) :: CF(ICBOT:ICTOP), CDF(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(OUT) :: CKIJ(LLEE, 4), CDKIJ(LLEE, 4), CGAM1(LLEE, 4)
      DOUBLE PRECISION, INTENT(OUT) :: CDGAM1(LLEE, 4), CGAM2(LLEE, 4), CDGAM2(LLEE, 4)

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(OUT) :: C(ICBOT:ICTOP), D(ICBOT:ICTOP)

      ! Locals
      INTEGER :: DELKJ, I, J, K, K1, M, NIJ, NKJ, NKJM1, P
      DOUBLE PRECISION :: AIJDUM, AREA2, C1, C2, CAVE, CI, CIJ, CKJ, CK1J, CM, Casum
      DOUBLE PRECISION :: D1, D2, DIJ, AODZ, KSAODZ, DXDUM, RCI, RCM, WI, WIM1, WO2DX
      DOUBLE PRECISION :: KIJ, DKIJ, GAM1, GAM2, DGAM1, DGAM2, CKIJS, CKZS
      LOGICAL :: TEST

   !----------------------------------------------------------------------*

      ! vertical conductivity terms (CBETM,CDB*)
      CBETM(ICBOT) = zero
      CDBETM(ICBOT) = zero
      CDBTMM(ICBOT) = zero

      IF (ISZERO(CWV)) THEN
         ! Special case: weighted harmonic mean
         AREA2 = CA0 * 2.0d0
         DO I = ICBOT, ICTOP
            CKZS = VSK3D(ICSOIL(I), 3)
            KSAODZ = CKZS * AREA2 / CDELZ(I)
            C(I) = CKR(I) * KSAODZ
            D(I) = CDKR(I) * KSAODZ
         END DO

         DO I = ICBOT + 1, ICTOP
            M = I - 1
            CM = C(M)
            CI = C(I)
            Casum = CM + CI
            RCM = CM / Casum
            RCI = CI / Casum
            CBETM(I) = CI * RCM
            CDBETM(I) = D(I) * RCM**2
            CDBTMM(I) = D(M) * RCI**2
         END DO

      ELSE IF (ISONE(CWV)) THEN
         ! Arithmetic mean
         DO I = ICBOT, ICTOP
            CKZS = VSK3D(ICSOIL(I), 3)
            C(I) = CKR(I) * CKZS
            D(I) = CDKR(I) * CKZS
         END DO

         DO I = ICBOT + 1, ICTOP
            M = I - 1
            AODZ = CA0 / (CDELZ(M) + CDELZ(I))
            CBETM(I) = AODZ * (C(M) + C(I))
            CDBETM(I) = AODZ * D(I)
            CDBTMM(I) = AODZ * D(M)
         END DO

      ELSE
         ! General w-mean
         WI = one / CWV
         WIM1 = (one - CWV) / CWV

         DO I = ICBOT, ICTOP
            CKZS = VSK3D(ICSOIL(I), 3)
            C(I) = (CKR(I) * CKZS)**CWV
            D(I) = CDKR(I) * CKZS
         END DO

         DO I = ICBOT + 1, ICTOP
            M = I - 1
            CM = C(M)
            CI = C(I)
            CAVE = 0.5d0 * (CM + CI)
            AODZ = CA0 / (CDELZ(M) + CDELZ(I))
            CBETM(I) = AODZ * CAVE**WI * 2.0d0
            CDBETM(I) = AODZ * (CAVE / CI)**WIM1 * D(I)
            CDBTMM(I) = AODZ * (CAVE / CM)**WIM1 * D(M)
         END DO

      END IF

      I = ICTOP + 1
      CBETM(I) = zero
      CDBETM(I) = zero
      CDBTMM(I) = zero

      ! vertical components of coefficients  NB lateral components added later
      DO I = ICBOT, ICTOP
         P = I + 1
         CF(I) = CBETM(I) + CBETM(P)
         CDF(I) = CDBETM(I) + CDBTMM(P)
      END DO

      ! loop over each face
      WI = one / CWL
      WIM1 = (one - CWL) / CWL

      face_loop: DO J = 1, 4
         M = 1 + MOD(J - 1, 2)
         TEST = (JELDUM(J) < 1) .OR. (JCBC(J) == 9)
         DXDUM = CDELL(J) + CDELL1(J)
         WO2DX = half * CWL / DXDUM

         internal_cell_loop: DO I = ICBOT, ICTOP
            ! lateral conductivity terms
            CKIJS = VSK3D(ICSOIL(I), M)
            KIJ = CKR(I) * CKIJS
            DKIJ = CDKR(I) * CKIJS
            CKIJ(I, J) = KIJ
            CDKIJ(I, J) = DKIJ

            ! lateral components of all coefficients
            K = JCACN(J, I)

            ! Cycle directly replaces GOTO 300
            IF (K == 0 .OR. TEST) CYCLE internal_cell_loop

            NIJ = ABS(JCDEL(J, I)) + 1
            DELKJ = JCDEL1(K, J)
            K1 = K + DELKJ
            NKJM1 = ABS(DELKJ)
            NKJ = NKJM1 + 1

            CKJ = CKIJ1(K, J) * CAIJ1(K, J) / DBLE(NIJ)
            CK1J = CKIJ1(K1, J) * CAIJ1(K1, J) / DBLE(NIJ)
            AIJDUM = CAIJ(J, I) / DBLE(NKJ)
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
               D1 = (C1 / CIJ)**WIM1
               D2 = (C2 / CIJ)**WIM1
               C1 = C1**WI
               C2 = C2**WI
            END IF

            GAM1 = C1 / DXDUM
            GAM2 = C2 / DXDUM * DBLE(NKJM1)
            DGAM1 = D1 * DIJ
            DGAM2 = D2 * DIJ * DBLE(NKJM1)

            CGAM1(I, J) = GAM1
            CGAM2(I, J) = GAM2
            CDGAM1(I, J) = DGAM1
            CDGAM2(I, J) = DGAM2

            CF(I) = CF(I) + GAM1 + GAM2
            CDF(I) = CDF(I) + DGAM1 + DGAM2

         END DO internal_cell_loop
      END DO face_loop

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
   ! JE   JAN 2009      Loop restructure for AD
   !----------------------------------------------------------------------*
   ! Entry conditions:
   ! 1 <= ICBOT <= ICSPCE, ICWLBT, ICWLTP <= ICTOP < LLEE
   !      ICWLBT <= ICWLTP
   ! for each j in 1:4: JCBC(j)  =   0,3,4,5,9 or 10
   !              3 <= JCBC(j) <= 5  ==>  JELDUM(j) = 0
   !                   JCBC(j)  = 9  ==>  JCACN(j,ICBOT:ICTOP) = 0
   !                   JCBC(j)  = 10 ==>  JCACN(j,ICBED+1:ICTOP) = 0
   ! the following are static functions of IEL:
   !     ICBED,ICBOT,ICLFL,ICLFN,ICLHL,ICLHN,ICLYRB,ICTOP,JCACN,JCBC,JELDUM
   !----------------------------------------------------------------------*
   ! Limited ranges:
   !              CQWI, CQWIN: only if JCBC(5)=1
   ! ICSPCE, CCS, CQSP,  CZSP: only if JCBC(5)=2
   !----------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! LLEE, NLYREE, NSEE, NSOLEE, NVSSOL, VSPPSI, VSPTHE, VSPKR, VSPETA,
      ! VSPDKR, VSPDET, PPPRI, ERROR, errcntallowed, ZERO, half

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: EESN, ELEVEL, IEL, ICBOT, ICTOP, ICBED
      INTEGER, INTENT(IN) :: ICSPCE, ICWLBT, ICWLTP, ICLFN, ICLHN, ICLGN
      INTEGER, INTENT(IN) :: ICLYRB (NLYREE), ICSOIL (ICBOT:ICTOP), JCBC (0:5)
      INTEGER, INTENT(IN) :: ICLFL (NLYREE), JCACN (4, ICBOT:ICTOP), JELDUM (4)
      INTEGER, INTENT(IN) :: ICLHL (NLYREE), JCDEL (4, ICBOT:ICTOP)
      INTEGER, INTENT(IN) :: ICLGL (NLYREE), JCDEL1 (LLEE, 4)
      DOUBLE PRECISION, INTENT(IN) :: CWV, CWL, CA0, CZG, CZSP, CCS
      DOUBLE PRECISION, INTENT(IN) :: VSK3D (NSEE, 3), CDELZ (ICBOT:ICTOP), CDELL (4)
      DOUBLE PRECISION, INTENT(IN) :: CAIJ1 (LLEE, 4), CZ (ICBOT:ICTOP), CDELL1 (4)
      DOUBLE PRECISION, INTENT(IN) :: CZ1 (LLEE, 4), CAIJ (4, ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: DT, CDNET, CQWIN, CBF, CBH
      DOUBLE PRECISION, INTENT(IN) :: CPSI1 (LLEE, 4), CPSIN (ICBOT:ICTOP), CLF (NLYREE)
      DOUBLE PRECISION, INTENT(IN) :: CPSIN1 (LLEE, 4), CQ (ICBOT:ICTOP), CLH (NLYREE)
      DOUBLE PRECISION, INTENT(IN) :: CKIJ1 (LLEE, 4), CZS (4), CLG (NLYREE)
      DOUBLE PRECISION, INTENT(IN) :: depadj (4)
      LOGICAL, INTENT(IN) :: BCHELE

      ! In+out arguments
      INTEGER, INTENT(INOUT) :: ICSTOR (ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(INOUT) :: CPSI (ICBOT:ICTOP)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CTHETA (ICBOT:ICTOP), CQV (ICBOT - 1:ICTOP)
      DOUBLE PRECISION, INTENT(OUT) :: CKR (ICBOT:ICTOP), CQH (4, ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(OUT) :: CQWI (ICWLBT:ICWLTP), CQSP, CPSL

      ! Locals, etc
      INTEGER, PARAMETER :: NITMAX = 100
      DOUBLE PRECISION, PARAMETER :: CEPSMX = 1.0D-4
      INTEGER :: BTYPE, I, ICL, IFA, J, K, K1, NDUM, NIT, PCL, SOIL
      DOUBLE PRECISION :: CPSMIN, DPSI, DPSIMX, H0, H1, H2
      DOUBLE PRECISION :: DWORK1 (1 + LLEE+NLYREE), DWORK2 (LLEE)
      DOUBLE PRECISION :: CETA (LLEE), CDETA (LLEE), CDKR (LLEE)
      DOUBLE PRECISION :: CBETM (LLEE), CDBETM (LLEE), CDBTMM (LLEE)
      DOUBLE PRECISION :: CF (LLEE), CDF (LLEE), CKIJ (LLEE, 4), CDKIJ ( LLEE, 4)
      DOUBLE PRECISION :: CGAM1 (LLEE, 4), CDGAM1 (LLEE, 4)
      DOUBLE PRECISION :: CGAM2 (LLEE, 4), CDGAM2 (LLEE, 4)
      DOUBLE PRECISION :: CA (LLEE), CB (LLEE), CC (LLEE), CR (LLEE), CDPSI (LLEE)

      INTEGER, SAVE :: errorcount = 0

   !----------------------------------------------------------------------*
   ! Initialization
   !________________*

      NDUM = ICTOP - ICBOT + 1

   ! Main iteration loop (calculations within depend upon CPSI)
   !____________________________________________________________*

      OUT500 : DO NIT = 1, NITMAX

         ! update soil properties from previous iteration
         CALL VSFUNC (NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
            VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ICSOIL, CPSI, &
            ICSTOR, CTHETA, CETA (ICBOT), CKR, CDETA (ICBOT), CDKR (ICBOT))

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
            CPSI (ICTOP), CB (ICTOP), CR (ICTOP), CQV (ICTOP))

         ! add well abstraction (type 1)
         BTYPE = JCBC (5)
         IF (BTYPE == 1) THEN
            CALL VSWELL (NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL (ICWLBT), &
               CA0, CDELZ (ICWLBT), CQWIN, CPSI (ICWLBT), CR (ICWLBT), &
               CQWI, DWORK1)
         ! add spring discharge (type 2)
         ELSE IF (BTYPE == 2) THEN
            CALL VSSPR (CZ (ICSPCE), CZSP, CCS, CPSI (ICSPCE), CKR ( &
               ICSPCE), CDKR (ICSPCE), CB (ICSPCE), CR (ICSPCE), CQSP)
         END IF

         ! add user-defined lateral boundary conditions (types 3-5)
         DO IFA = 1, 4
            BTYPE = JCBC (IFA)
            IF (BTYPE >= 3 .AND. BTYPE <= 5) THEN
               CALL VSBC (BCHELE, IFA, ICBOT, ICTOP, JCBC (IFA), &
                  ICLYRB, ICLFN, ICLFL, ICLHL, ICLHN, CZG, CDELL (IFA), &
                  CDELZ, CZ, CAIJ, CLF, CLH, CPSI, CKIJ (ICBOT, IFA), &
                  CDKIJ (ICBOT, IFA), CB (ICBOT), CR (ICBOT), CQH, DWORK1)

            ! add stream-aquifer interaction (types 9 and 10)
            ELSE IF (BTYPE == 9 .OR. BTYPE == 10) THEN
               CALL VSSAI (IFA, JCBC (IFA), ICBOT, ICTOP, ICBED, CDELL ( &
                  IFA), CZ, CAIJ, CZS (IFA), CPSI, CKIJ (ICBOT, IFA), &
                  CDKIJ (ICBOT, IFA), CB (ICBOT), CR (ICBOT), CQH, depadj ( &
                  IFA), cdelz)
            END IF
         END DO

         ! add lower boundary condition (types 6-8)
         SOIL = ICSOIL (ICBOT)
         CALL VSLOWR (JCBC (0), CA0, CZ (ICBOT), CDELZ (ICBOT), VSK3D ( &
            SOIL, 3), CBF, CBH, CPSI (ICBOT), CKR (ICBOT), CDKR (ICBOT), &
            CB (ICBOT), CR (ICBOT), CQV (ICBOT - 1))

         ! solve linear equations (Preserving required assumed-shape array slices)
         CALL TRIDAG (CA(ICBOT), CB(ICBOT), CC(ICBOT), CR(ICBOT), CDPSI(ICBOT), NDUM)

         ! update PSI array and check for convergence
         DPSIMX = ZERO
         DO ICL = ICBOT, ICTOP
            DPSI = CDPSI (ICL)
            CPSI (ICL) = CPSI (ICL) + DPSI
            DPSIMX = MAX (DPSIMX, ABS (DPSI))
         END DO

         ! PERFECT EXIT: Immediately break loop if convergence is met
         IF (DPSIMX <= CEPSMX) EXIT OUT500

      END DO OUT500

      ! Handle non-convergence error reporting safely
      IF (NIT > NITMAX .AND. ELEVEL > 0) THEN
         errorcount = errorcount + 1
         IF (errorcount < errcntallowed) THEN
            CALL ERROR (ELEVEL, 1036, PPPRI, IEL, 0, 'Maximum iterations in VSS column solver')
         ELSE IF (errorcount == errcntallowed) THEN
            CALL ERROR (ELEVEL, 1036, PPPRI, IEL, 0, '**** Last printout of the error message - maximum iterations error in VSS column solver *****')
         END IF
      END IF

   ! Calculate final values of output variables
   !____________________________________________*
   ! flows
      DO ICL = ICBOT, ICTOP - 1
         PCL = ICL + 1
         CQV (ICL) = CBETM (PCL) * (CZ (ICL) + CPSI (ICL) - CZ (PCL) - CPSI (PCL)) / CA0
      END DO

      face_loop: DO J = 1, 4
         IF (JELDUM (J) < 1) CYCLE face_loop

         cell_loop: DO I = ICBOT, ICTOP
            K = JCACN (J, I)
            IF (K < 1) CYCLE cell_loop

            K1 = K + JCDEL1 (K, J)
            H0 = CZ (I) + CPSI (I)
            H1 = CZ1 (K, J) + CPSI1 (K, J)
            H2 = CZ1 (K1, J) + CPSI1 (K1, J)

            CQH (J, I) = CGAM1 (I, J) * (H1 - H0) + CGAM2 (I, J) * (H2 - H0)
         END DO cell_loop
      END DO face_loop

   ! phreatic surface level
      CPSMIN = CZ (ICBOT) - half * CDELZ (ICBOT)

      search_loop: DO ICL = ICBOT, ICTOP
         IF (CPSI(ICL) < ZERO) EXIT search_loop
      END DO search_loop

      ! Adjust ICL only if we actually found a value or finished the loop
      ICL = MAX(ICBOT, ICL - 1)

      CPSL = MAX (CPSMIN, CZ (ICL) + CPSI (ICL))

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

      ! Assumed external module dependencies providing global variables:
      ! NELEE, NLYREE, LLEE, NLFEE, total_no_links, total_no_elements,
      ! top_cell_no, VSZMIN, VSZMAX, ZERO, half, DCSTOT, DCSZON, NCSZON,
      ! DCRBED, NCRBED, ZGRUND, ZLYRBT, DELTAZ, ZVSNOD, JVSACN, JVSDEL,
      ! JVSALN, NHBED, FHBED, NLYR, NLYRBT, ICMREF, ICMBK, ZBEFF, DCRTOT,
      ! INITIALISE_VSMOD, INITIALISE_AL_C, ALSPRD, ERROR, FFFATAL, WWWARN, PPPRI

      IMPLICIT NONE

      ! Locals
      INTEGER, PARAMETER :: JVSDUM = NELEE * NLYREE
      INTEGER :: NMOD
      INTEGER :: I, IRANGE, IBOT, IBOTL, ICL, IEL, IFA, ILYR, ITOP
      INTEGER :: J, JRANGE, JBOT, JBOTL, JCL, JEL, JFA, JLYR, JTOP
      INTEGER :: IDEL, IDEL0, IL, ILMAX, ILMIN, NITOT, NIMIN
      INTEGER :: JDEL, JDEL0, JL, JLMAX, JLMIN, NJTOT, NJMIN
      INTEGER :: IAQTOP, IBANK2, IBK, ICL0, ICL1, ICOL1, ILINK, ITYPE
      INTEGER :: DEL, JDIF, K, K2, K20, K2MOD, LCON, LTOP
      INTEGER :: NACELL, NCELL, NCL, NCLYR, NDUM, NEXTRA, NODD, NUM2
      INTEGER :: NIDUM(LLEE), NJDUM(LLEE), MAX_BOT_TOP
      DOUBLE PRECISION :: DZLYR, ZCBOT, ZDEPTH, ZBDBOT, ZCTOP, ZDUM
      DOUBLE PRECISION :: ZAQBOT, ZSZBOT, ZDIFF, ZLBOT, ZNODE
      LOGICAL :: BRENUM, BWARN, MISS, PAIR, BDONE(NELEE, 4)
      CHARACTER(LEN=57) :: MSG
      INTEGER :: nlyrmax

      ! Modern Initialization replacing DATA blocks
      INTEGER :: LRENUM(NELEE, NLYREE) = 0
      INTEGER, SAVE :: NRENUM = 0

   !----------------------------------------------------------------------*

      NMOD = NLYREE + 1

      renumbering_loop: DO
         NRENUM = NRENUM + 1

         ! Safe inline error trap replaces GOTO 8048
         IF (NRENUM > NELEE) THEN
            CALL ERROR(FFFATAL, 1048, PPPRI, 0, 0, 'Attempts to renumber cells have failed.')
            RETURN
         END IF

         BWARN = (NRENUM == NELEE)
         BRENUM = .FALSE.

         ! Set initial indices, dimensions & positions of cells
         !______________________________________________________*
         top_cell_no = 0

         element_loop: DO IEL = total_no_links + 1, total_no_elements
            ITYPE = ICMREF(IEL, 1)

            ! * process only grid and bank-1 elements here
            IF (ITYPE == 2) CYCLE element_loop

            ! --- loop over layers in aquifer zone (start from bottom of column)
            ZSZBOT = ZGRUND(IEL) - DCSTOT
            ICL = 1
            DELTAZ(ICL, IEL) = ZERO
            ZVSNOD(ICL, IEL) = ZERO

            layer_loop: DO ILYR = 1, NLYR(IEL)
               ! * divide each layer into equal sized cells
               ZLBOT = ZLYRBT(IEL, ILYR)
               DZLYR = MIN(ZLYRBT(IEL, ILYR + 1), ZSZBOT) - ZLBOT

               ! skip if layer is thinner than minimum cell size
               IF (DZLYR < VSZMIN) CYCLE layer_loop

               ! if no other plan make cells as large as poss but < VSZMAX
               NCLYR = LRENUM(IEL, ILYR)
               IF (NCLYR <= 0) NCLYR = MAX(1, INT(DZLYR / VSZMAX) + 1)

               ZDEPTH = DZLYR / DBLE(NCLYR)

               DO I = 1, NCLYR
                  ICL = ICL + 1
                  DELTAZ(ICL, IEL) = ZDEPTH
                  ZVSNOD(ICL, IEL) = ZDEPTH * (DBLE(I) - half) + ZLBOT
               END DO
            END DO layer_loop

            ! --- set up data for soil zone
            ZAQBOT = ZLYRBT(IEL, 1)
            ZCBOT = ZSZBOT

            DO I = NCSZON, 1, -1
               ZDEPTH = DCSZON(I)
               ZNODE = ZCBOT + ZDEPTH * half
               IF (ZNODE > ZAQBOT) THEN
                  ICL = ICL + 1
                  DELTAZ(ICL, IEL) = ZDEPTH
                  ZVSNOD(ICL, IEL) = ZNODE
               END IF
               ZCBOT = ZCBOT + ZDEPTH
            END DO

            ! --- update LL & store number of cells for this column
            top_cell_no = MAX(top_cell_no, ICL)
            IDUM(IEL) = ICL

            ! --- process link and opposite bank elements, if IEL is bank type 1
            IF (ITYPE /= 1) CYCLE element_loop

            ! * set up link cells up to bottom of link bed
            ILINK = ICMREF(IEL, 4)
            ZBDBOT = ZBEFF(ILINK) - DCRTOT
            ZCBOT = ZLYRBT(IEL, 1)

            link_cells: DO ICL1 = 1, ICL
               ZDEPTH = DELTAZ(ICL1, IEL)
               ZCTOP = ZCBOT + ZDEPTH

               IF (ZCTOP > ZBDBOT) EXIT link_cells

               DELTAZ(ICL1, ILINK) = ZDEPTH
               ZVSNOD(ICL1, ILINK) = ZVSNOD(ICL1, IEL)
               ZCBOT = ZCTOP
            END DO link_cells

            ! cell just below link bed: smaller than bank, unless ...
            ZDEPTH = ZBDBOT - ZCBOT
            IF (ZDEPTH < VSZMIN) THEN
               ! ... remainder is small: add it to the cell below
               ICL1 = ICL1 - 1
               ZDEPTH = ZDEPTH + DELTAZ(ICL1, ILINK)
            END IF

            DELTAZ(ICL1, ILINK) = ZDEPTH
            ZVSNOD(ICL1, ILINK) = ZBDBOT - half * ZDEPTH

            ! set up link bed cells
            ZCBOT = ZBDBOT
            DO I = NCRBED, 1, -1
               ZDEPTH = DCRBED(I)
               ICL1 = ICL1 + 1
               DELTAZ(ICL1, ILINK) = ZDEPTH
               ZVSNOD(ICL1, ILINK) = ZCBOT + ZDEPTH * half
               ZCBOT = ZCBOT + ZDEPTH
            END DO

            ! update LL & store number of cells for the link
            top_cell_no = MAX(top_cell_no, ICL1)
            IDUM(ILINK) = ICL1

            ! set up opposite bank cells
            IBANK2 = ICMBK(ILINK, 2)

            ! Exploit F90 array slicing
            DELTAZ(1:ICL, IBANK2) = DELTAZ(1:ICL, IEL)
            ZVSNOD(1:ICL, IBANK2) = ZVSNOD(1:ICL, IEL)

            IDUM(IBANK2) = ICL

         END DO element_loop

         ! Renumber cells & set up NLYRBT
         !____________________________________________________________________*
         IF (BEXBK) THEN
            ICOL1 = 1
         ELSE
            ICOL1 = total_no_links + 1
            NLYRBT(1:total_no_links, 1) = top_cell_no
         END IF

         ! --- loop over column elements
         DO IEL = ICOL1, total_no_elements
            NCL = IDUM(IEL)
            ICL0 = top_cell_no - NCL

            IF (ICL0 > 0) THEN
               DELTAZ(ICL0 + 1 : ICL0 + NCL, IEL) = DELTAZ(1 : NCL, IEL)
               ZVSNOD(ICL0 + 1 : ICL0 + NCL, IEL) = ZVSNOD(1 : NCL, IEL)

               DELTAZ(1 : ICL0, IEL) = ZERO
               ZVSNOD(1 : ICL0, IEL) = ZERO
            END IF

            ICL0 = ICL0 + 1

            DO ILYR = 1, NLYR(IEL)
               search_icl: DO ICL = ICL0 + 1, top_cell_no
                  IF (ZVSNOD(ICL, IEL) > ZLYRBT(IEL, ILYR)) EXIT search_icl
               END DO search_icl

               NLYRBT(IEL, ILYR) = ICL
               ICL0 = ICL - 1
            END DO

            NLYRBT(IEL, ILYR) = top_cell_no + 1
         END DO

         CALL INITIALISE_VSMOD()
         CALL INITIALISE_AL_C()

         ! Set up cell connectivities (JVSACN, JVSDEL)
         !_____________________________________________*
         DO IEL = 1, total_no_elements
            IBOT = NLYRBT(IEL, 1)
            BDONE(IEL, 1:4) = .FALSE.
            JVSACN(1:4, IBOT:top_cell_no, IEL) = 0
            JVSDEL(1:4, IBOT:top_cell_no, IEL) = 0
         END DO

         LTOP = top_cell_no - NCRBED
         IAQTOP = top_cell_no - NCSZON

         face_setup_loop: DO IEL = ICOL1, total_no_elements
            ITYPE = ICMREF(IEL, 1)
            IBOT = NLYRBT(IEL, 1)

            IF (IEL <= total_no_links) THEN
               IBK = ICMBK(IEL, 1)
               ITOP = MIN(IAQTOP + IBOT - NLYRBT(IBK, 1), LTOP)
            ELSE
               ITOP = IAQTOP
            END IF

            face_loop: DO IFA = 1, 4
               JEL = ICMREF(IEL, IFA + 4)
               IF (JEL < ICOL1) CYCLE face_loop

               JFA = ICMREF(IEL, IFA + 8)
               IF (BDONE(JEL, JFA)) CYCLE face_loop

               JBOT = NLYRBT(JEL, 1)
               JDIF = JBOT - IBOT

               ! --- channel link-bank face
               IF (IEL <= total_no_links .AND. JEL > total_no_links) THEN
                  DO ICL = IBOT, LTOP
                     JCL = ICL + JDIF
                     JVSACN(IFA, ICL, IEL) = JCL
                     JVSACN(JFA, JCL, JEL) = ICL
                  END DO

                  BDONE(IEL, IFA) = .TRUE.
                  CYCLE face_loop
               END IF

               ! --- other elements
               IF (JEL <= total_no_links) THEN
                  IBK = ICMBK(JEL, 1)
                  JTOP = MIN(IAQTOP + JBOT - NLYRBT(IBK, 1), LTOP)
                  LCON = LTOP
               ELSE
                  JTOP = IAQTOP
                  LCON = top_cell_no
               END IF

               ! ----- soil zone processing
               MAX_BOT_TOP = MAX(IBOT, JBOT)
               MAX_BOT_TOP = MAX(MAX_BOT_TOP, ITOP + 1, JTOP + 1)

               DO ICL = MAX_BOT_TOP, LCON
                  JCL = ICL
                  JVSACN(IFA, ICL, IEL) = JCL
                  JVSACN(JFA, JCL, JEL) = ICL
               END DO

               ! ----- aquifer zone processing
               ILYR = 1
               JLYR = 1

               layer_match_loop: DO WHILE (.TRUE.)
                  IBOTL = NLYRBT(IEL, ILYR)
                  JBOTL = NLYRBT(JEL, JLYR)

                  IF (IBOTL > ITOP .OR. JBOTL > JTOP) THEN
                     BDONE(IEL, IFA) = .TRUE.
                     CYCLE face_loop
                  END IF

                  JRANGE = JVSALN(IEL, ILYR, IFA)
                  IRANGE = JVSALN(JEL, JLYR, JFA)

                  IF (JRANGE == 0) THEN
                     ILYR = ILYR + 1
                     CYCLE layer_match_loop
                  ELSE IF (IRANGE == 0) THEN
                     JLYR = JLYR + 1
                     CYCLE layer_match_loop
                  END IF

                  ILMIN = IRANGE / NMOD
                  ILMAX = MOD(IRANGE, NMOD)
                  JLMIN = JRANGE / NMOD
                  JLMAX = MOD(JRANGE, NMOD)

                  ! count cells in column IEL, & no. required in JEL
                  NITOT = 0
                  NJMIN = 0
                  NODD = 0

                  DO IL = ILMIN, ILMAX
                     NCELL = FNCELL(IL, IEL, ITOP)
                     IF (JVSALN(IEL, IL, IFA) /= 0) THEN
                        DO I = 0, NCELL - 1
                           NITOT = 1 + NITOT
                           NIDUM(NITOT) = I + NLYRBT(IEL, IL)
                        END DO
                        NCELL = NCELL - NODD
                        NJMIN = (NCELL + 1) / 2 + NJMIN
                        NODD = MOD(NCELL, 2)
                     ELSE IF (NCELL > 0) THEN
                        NODD = 0
                     END IF
                  END DO
                  NIDUM(NITOT + 1) = 0

                  ! count cells in column JEL, & no. required in IEL
                  NJTOT = 0
                  NIMIN = 0
                  NODD = 0

                  DO JL = JLMIN, JLMAX
                     NCELL = FNCELL(JL, JEL, JTOP)
                     IF (JVSALN(JEL, JL, JFA) /= 0) THEN
                        DO J = 0, NCELL - 1
                           NJTOT = 1 + NJTOT
                           NJDUM(NJTOT) = J + NLYRBT(JEL, JL)
                        END DO
                        NCELL = NCELL - NODD
                        NIMIN = (NCELL + 1) / 2 + NIMIN
                        NODD = MOD(NCELL, 2)
                     ELSE IF (NCELL > 0) THEN
                        NODD = 0
                     END IF
                  END DO
                  NJDUM(NJTOT + 1) = 0

                  ! Checking conditions and splitting cells
                  IF (NITOT == 0 .AND. NJTOT > 0) THEN
                     WRITE (MSG, 9200) JFA, JLYR
                     IF (NRENUM == 1) CALL ERROR(WWWARN, 1053, PPPRI, JEL, 0, MSG)

                  ELSE IF (NJTOT == 0 .AND. NITOT > 0) THEN
                     WRITE (MSG, 9200) IFA, ILYR
                     IF (NRENUM == 1) CALL ERROR(WWWARN, 1053, PPPRI, IEL, 0, MSG)

                  ELSE IF (NJTOT < NJMIN) THEN
                     BRENUM = .TRUE.
                     NEXTRA = 0
                     DO JL = JLMIN, JLMAX
                        IF (JVSALN(JEL, JL, JFA) /= 0) THEN
                           IF (BWARN) THEN
                              WRITE (MSG, 9300) JFA, JL
                              CALL ERROR(WWWARN, 1037, PPPRI, JEL, 0, MSG)
                           END IF
                           NCELL = FNCELL(JL, JEL, JTOP)
                           NDUM = NCELL * NJMIN + NEXTRA + NJTOT / 2
                           LRENUM(JEL, JL) = NDUM / NJTOT
                           NEXTRA = MOD(NDUM, NJTOT) - NJTOT / 2
                        END IF
                     END DO

                  ELSE IF (NITOT < NIMIN) THEN
                     BRENUM = .TRUE.
                     NEXTRA = 0
                     DO IL = ILMIN, ILMAX
                        IF (JVSALN(IEL, IL, IFA) /= 0) THEN
                           IF (BWARN) THEN
                              WRITE (MSG, 9300) IFA, IL
                              CALL ERROR(WWWARN, 1037, PPPRI, IEL, 0, MSG)
                           END IF
                           NCELL = FNCELL(IL, IEL, ITOP)
                           NDUM = NCELL * NIMIN + NEXTRA + NITOT / 2
                           LRENUM(IEL, IL) = NDUM / NITOT
                           NEXTRA = MOD(NDUM, NITOT) - NITOT / 2
                        END IF
                     END DO

                  ELSE
                     ! how many splits possible, & how many to forego
                     IF (NITOT >= NJTOT) THEN
                        IDEL0 = 1
                        NUM2 = NITOT - NJMIN
                        NEXTRA = NJTOT - NJMIN
                     ELSE
                        IDEL0 = 0
                        NUM2 = NJTOT - NIMIN
                        NEXTRA = NITOT - NIMIN
                     END IF
                     JDEL0 = 1 - IDEL0

                     CALL ALSPRD(NEXTRA, NUM2, K20, K2MOD)

                     MISS = .FALSE.
                     K2 = -K20
                     I = 1
                     J = 1

                     pair_search: DO WHILE (I <= NITOT .AND. J <= NJTOT)
                        PAIR = (NIDUM(I + IDEL0) == NIDUM(I) + 1)
                        PAIR = (NJDUM(J + JDEL0) == NJDUM(J) + 1) .OR. PAIR
                        PAIR = .NOT. MISS .AND. PAIR

                        IF (PAIR) THEN
                           K2 = K2 + 1
                           MISS = (K2 >= 0 .AND. MOD(K2, K2MOD) == 0)
                           MISS = (K2 <= (NEXTRA - 1) * K2MOD .AND. MISS)
                           PAIR = .NOT. MISS
                        ELSE
                           MISS = .FALSE.
                        END IF

                        DEL = 0
                        IF (PAIR) DEL = 1

                        IDEL = IDEL0 * DEL
                        JDEL = JDEL0 * DEL

                        DO K = 0, DEL
                           ICL = NIDUM(I)
                           JCL = NJDUM(J)
                           IF (IDEL >= K) JVSACN(IFA, ICL, IEL) = JCL
                           IF (JDEL >= K) JVSACN(JFA, JCL, JEL) = ICL
                           JVSDEL(IFA, ICL, IEL) = IDEL * (1 - 2 * K)
                           JVSDEL(JFA, JCL, JEL) = JDEL * (1 - 2 * K)

                           ! Replaced non-standard IDIMJE with standard MAX implementation
                           I = I + MAX(0, IDEL - K)
                           J = J + MAX(0, JDEL - K)
                        END DO

                        I = I + 1
                        J = J + 1
                     END DO pair_search

                  END IF

                  ! move on to next layers
                  ILYR = ILMAX + 1
                  JLYR = JLMAX + 1

               END DO layer_match_loop

            END DO face_loop
         END DO face_setup_loop

         ! Repeat the whole thing if BRENUM was flagged
         IF (.NOT. BRENUM) EXIT renumbering_loop

      END DO renumbering_loop

   ! Finish off
   !____________*
      WRITE (PPPRI, 9000) top_cell_no

      finish_loop: DO IEL = ICOL1, total_no_links
         IBK = ICMBK(IEL, 1)
         NACELL = LTOP + NLYRBT(IBK, 1) - NLYRBT(IEL, 1)
         ZDUM = DELTAZ(NACELL, IBK)
         ZDIFF = ZDUM - DELTAZ(LTOP, IEL)

         DELTAZ(LTOP, IEL) = ZDUM

         IF (NLYRBT(IEL, 1) <= LTOP - 1) THEN
            ZVSNOD(NLYRBT(IEL, 1):LTOP - 1, IEL) = ZVSNOD(NLYRBT(IEL, 1):LTOP - 1, IEL) - ZDIFF
         END IF

         ZVSNOD(ICL, IEL) = ZVSNOD(ICL, IEL) - ZDIFF * half

         IF (NLYR(IEL) >= 1) THEN
            ZLYRBT(IEL, 1:NLYR(IEL)) = ZLYRBT(IEL, 1:NLYR(IEL)) - ZDIFF
         END IF

         ! NB. banks 1 and 2 are identical
         NHBED(IEL, 1) = NACELL
         NHBED(IEL, 2) = NACELL
         FHBED(IEL, 1) = ZERO
         FHBED(IEL, 2) = ZERO

      END DO finish_loop

      RETURN

      ! FORMAT STATEMENTS
9000  FORMAT(/ 'Number of top cell in all columns (LL) = ',I3)
9200  FORMAT('Null cell connectivity being set up for face ',I1, ' layer ',I2)
9300  FORMAT('Not possible to connect all cells for face ',I1, ' layer ',I2)

   CONTAINS

      ! Replaces the obsolete Statement Function FNCELL
      PURE INTEGER FUNCTION FNCELL(IDX, ELEM, TOP)
         INTEGER, INTENT(IN) :: IDX, ELEM, TOP
         ! Calculates number of cells handling boundary constraints
         FNCELL = MAX(0, MIN(NLYRBT(ELEM, IDX + 1), TOP + 1) - NLYRBT(ELEM, IDX))
      END FUNCTION FNCELL

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

      element_loop: DO IEL = ICOL1, total_no_elements
         NLYRI = NLYR(IEL)

         face_loop: DO IFA = 1, 4
            JEL = ICMREF(IEL, IFA + 4)
            ! null connectivity for boundary faces, branched channels & link flanks

            ! 1. Skip rest of loop if face already processed using CYCLE
            IF (JEL < ICOL1 .OR. (IEL <= total_no_links .AND. JEL > total_no_links)) CYCLE face_loop
            IF (BDONE(JEL)) CYCLE face_loop

            ! ... else process BOTH sides of face
            NLYRJ = NLYR(JEL)
            JFA = ICMREF(IEL, IFA + 8)

            ! 2. Replaced the 102 and 104 loops with array slicing
            ILDUM(1:NLYRI) = -1
            JLDUM(1:NLYRJ) = -1

            aqcon_loop: DO I = 1, NAQCON
               I1 = IAQCON(1, I)
               I2 = IAQCON(3, I)

               ! * does entry I belong to the current pair of elements?
               IF (IEL == I1 .AND. JEL == I2) THEN
                  K = 2
               ELSEIF (IEL == I2 .AND. JEL == I1) THEN
                  K = 4
               ELSE
                  ! 3. Replaced GOTO 110 with CYCLE
                  CYCLE aqcon_loop
               END IF

               ILYR = IAQCON(K, I)
               JLYR = IAQCON(6 - K, I)
               MSG = ' '

               IF (ILYR < 0 .OR. ILYR > NLYRI) THEN
                  ! * ILYR out of range
                  KEL = IEL
                  WRITE (MSG, 9381) ILYR, I, IEL, NLYRI
               ELSEIF (JLYR < 0 .OR. JLYR > NLYRJ) THEN
                  ! * JLYR out of range
                  KEL = JEL
                  WRITE (MSG, 9381) JLYR, I, JEL, NLYRJ
               ELSE
                  IF (ILYR > 0) THEN
                     JRANGE = ILDUM(ILYR)
                     TEST1 = JLYR == 0 .AND. JRANGE > 0
                     IF (JRANGE == 0 .OR. TEST1) THEN
                        ! * invalid
                        KEL = IEL
                        JRANGE = MOD(JLYR + JRANGE, NMOD)
                        WRITE (MSG, 9382) IEL, ILYR, JRANGE, JEL, I
                     ELSE
                        IF (JRANGE < 0) JRANGE = NMOD * NLYRJ + 1
                        JLMIN = MIN(JLYR, JRANGE / NMOD)
                        JLMAX = MAX(JLYR, MOD(JRANGE, NMOD))
                        ILDUM(ILYR) = NMOD * JLMIN + JLMAX
                     END IF
                  END IF

                  IF (JLYR > 0) THEN
                     IRANGE = JLDUM(JLYR)
                     TEST1 = ILYR == 0 .AND. IRANGE > 0
                     IF (IRANGE == 0 .OR. TEST1) THEN
                        ! * invalid
                        KEL = JEL
                        IRANGE = MOD(ILYR + IRANGE, NMOD)
                        WRITE (MSG, 9382) JEL, JLYR, IRANGE, IEL, I
                     ELSE
                        IF (IRANGE < 0) IRANGE = NMOD * NLYRI + 1
                        ILMIN = MIN(ILYR, IRANGE / NMOD)
                        ILMAX = MAX(ILYR, MOD(IRANGE, NMOD))
                        JLDUM(JLYR) = NMOD * ILMIN + ILMAX
                     END IF
                  END IF
               END IF

               ! * note: MSG for ILYR>0.and.JRANGE=0 is lost
               ! * if also JLYR>0.and.IRANGE=0
               IF (MSG /= ' ') THEN
                  CALL ERROR(EEERR, 1038, PPPRI, KEL, 0, MSG)
                  NVSERR = NVSERR + 1
               END IF
            END DO aqcon_loop

            ! set ILYR & JLYR to numbers of layers immediately below soil zone
            ZSZBOT = ZGRUND(IEL) - DCSTOT - ZSMALL

            ! 4. Replaced 120 and 140 loops with EXIT searches
            find_ilyr: DO ILYR = NLYRI, 1, -1
               IF (ZLYRBT(IEL, ILYR) < ZSZBOT) EXIT find_ilyr
            END DO find_ilyr

            ZSZBOT = ZGRUND(JEL) - DCSTOT - ZSMALL

            find_jlyr: DO JLYR = NLYRJ, 1, -1
               IF (ZLYRBT(JEL, JLYR) < ZSZBOT) EXIT find_jlyr
            END DO find_jlyr

            ! --- start of loop over layers (downwards from top of aquifer zone)
            ! 5. Replaced the massive 200 GOTO loop with a DO WHILE
            layer_matching: DO WHILE (ILYR > 0 .AND. JLYR > 0)
               ISOIL = NTSOIL(IEL, ILYR)
               JSOIL = NTSOIL(JEL, JLYR)
               JRANGE = ILDUM(ILYR)
               IRANGE = JLDUM(JLYR)

               IF (JRANGE == 0 .OR. (IRANGE > 0 .AND. JRANGE < 0)) THEN
                  ! * null
                  ILYR = ILYR - 1
               ELSEIF (IRANGE == 0 .OR. (JRANGE > 0 .AND. IRANGE < 0)) THEN
                  ! * null
                  JLYR = JLYR - 1
               ELSEIF (JRANGE > 0) THEN
                  ! * user-specified
                  JLMIN = JRANGE / NMOD
                  ILMIN = IRANGE / NMOD

                  ! 6. Replaced the 210 GOTO jump with another DO WHILE
                  ! * repeat until the whole connected range is processed
                  process_range: DO WHILE (ILMIN <= ILYR)
                     ILMAX = ILYR
                     DO ILYR = ILMAX, ILMIN, -1
                        JRANGE = ILDUM(ILYR)
                        JVSALN(IEL, ILYR, IFA) = MAX(0, JRANGE)
                        IF (JRANGE > 0) JLMIN = MIN(JLMIN, JRANGE / NMOD)
                     END DO

                     JLMAX = JLYR
                     DO JLYR = JLMAX, JLMIN, -1
                        IRANGE = JLDUM(JLYR)
                        JVSALN(JEL, JLYR, JFA) = MAX(0, IRANGE)
                        IF (IRANGE > 0) ILMIN = MIN(ILMIN, IRANGE / NMOD)
                     END DO
                  END DO process_range

               ELSEIF (ISOIL == JSOIL) THEN
                  ! * matching soils
                  JVSALN(IEL, ILYR, IFA) = JLYR * NMOD + JLYR
                  JVSALN(JEL, JLYR, JFA) = ILYR * NMOD + ILYR
                  ILYR = ILYR - 1
                  JLYR = JLYR - 1
               ELSE
                  ! * decide whether to move down column IEL or JEL:
                  ! * set type of soil above
                  ISOILP = 0
                  IF (ILYR < NLYRI) ISOILP = NTSOIL(IEL, ILYR + 1)
                  JSOILP = 0
                  IF (JLYR < NLYRJ) JSOILP = NTSOIL(JEL, JLYR + 1)

                  ! * look for next matching soil or user-specification
                  search_i: DO I = ILYR - 1, 1, -1
                     IF (NTSOIL(IEL, I) == JSOIL .OR. ILDUM(I) >= 0) EXIT
                  END DO search_i
                  ISKIP = ILYR - I

                  search_j: DO J = JLYR - 1, 1, -1
                     IF (NTSOIL(JEL, J) == ISOIL .OR. JLDUM(J) >= 0) EXIT
                  END DO search_j
                  JSKIP = JLYR - J

                  ! * choose smallest skip; or preserve soil continuity
                  MOVEJ = (ISOIL == ISOILP) .OR. (JSOIL /= JSOILP)
                  MOVEJ = (JSKIP < ISKIP) .OR. (JSKIP == ISKIP .AND. MOVEJ)
                  MOVEJ = (J > 0) .AND. MOVEJ

                  IF (MOVEJ) MOVEJ = JLDUM(J) < 0

                  ! * would there be any point moving down IEL?
                  IOK = I > 0
                  IF (IOK) IOK = ILDUM(I) < 0

                  ! * the choice is made
                  IF (MOVEJ) THEN
                     JLYR = J
                  ELSEIF (IOK) THEN
                     ILYR = I
                  ELSE
                     ILYR = ILYR - 1
                     JLYR = JLYR - 1
                  END IF
               END IF
            END DO layer_matching
            ! * process next pair of layers happens naturally by looping the WHILE

         END DO face_loop

         BDONE(IEL) = .TRUE.

      END DO element_loop

! Formats
!_________*
9381  FORMAT('Layer',I3,' out of range, IAQCON entry',I3, &
      &      ' (element',I5,' has',I3,' layers)')

9382  FORMAT('Invalid null connection, element',I5,':', &
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
      ! JE   JAN 2009       Loop restructure for AD
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

      ! Assumed external module dependencies providing global variables:
      ! ZERO, ONE, FFFATAL, PPPRI, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NVSSOL, NSOLEE
      DOUBLE PRECISION, INTENT(IN) :: VSPPSI(NVSSOL), VSPTHE(NSOLEE, *)
      DOUBLE PRECISION, INTENT(IN) :: VSPKR(NSOLEE, *), VSPETA(NSOLEE, *)
      DOUBLE PRECISION, INTENT(IN) :: VSPDKR(NSOLEE, *), VSPDET(NSOLEE, *)
      INTEGER, INTENT(IN) :: IEL, ICBOT, ICTOP, ICSOIL(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP)

      ! In+out arguments
      INTEGER, INTENT(INOUT) :: ICSTOR(ICBOT:ICTOP)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CTHETA(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(OUT) :: CETA(ICBOT:ICTOP), CKR(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(OUT) :: CDETA(ICBOT:ICTOP), CDKR(ICBOT:ICTOP)

      ! Locals
      CHARACTER(LEN=5) :: WETDRY(0:1) = ['(wet)', '(dry)']
      DOUBLE PRECISION :: P, PDUM, VLO
      INTEGER :: ICL, INC, JHI, JLO, JM, IS, DRY
      LOGICAL :: IS_ERROR

   !----------------------------------------------------------------------*

      IS_ERROR = .FALSE.

      ! ----- loop over all cells in column
      OUT100: DO ICL = ICBOT, ICTOP

         P = CPSI(ICL)
         JLO = ICSTOR(ICL)
         IS = ICSOIL(ICL)

         ! --- find location in table of current psi value
         ! test for initial guess
         IF (JLO <= 0 .OR. JLO > NVSSOL) THEN
            JLO = 0
            JHI = NVSSOL + 1
         ELSE
            ! set initial hunt increment
            INC = 1

            ! hunt up the table
            IF (P <= VSPPSI(JLO)) THEN
               hunt_up: DO WHILE (.TRUE.)
                  JHI = JLO + INC
                  IF (JHI > NVSSOL) THEN
                     JHI = NVSSOL + 1
                     EXIT hunt_up
                  ELSE IF (P <= VSPPSI(JHI)) THEN
                     JLO = JHI
                     INC = INC + INC
                  ELSE
                     EXIT hunt_up
                  END IF
               END DO hunt_up

            ! hunt down the table
            ELSE
               JHI = JLO
               hunt_down: DO WHILE (.TRUE.)
                  JLO = JHI - INC
                  IF (JLO < 1) THEN
                     JLO = 0
                     EXIT hunt_down
                  ELSE IF (P > VSPPSI(JLO)) THEN
                     JHI = JLO
                     INC = INC + INC
                  ELSE
                     EXIT hunt_down
                  END IF
               END DO hunt_down
            END IF
         END IF

         ! hunt completed, begin bisection
         ! At this point: { VSPPSI(JLO)>=P or JLO=0        } and
         !                { VSPPSI(JHI)< P or JHI=NVSSOL+1 }

         bisection: DO WHILE (JHI - JLO > 1)
            JM = (JHI + JLO) / 2
            IF (P < VSPPSI(JM)) THEN
               JLO = JM
            ELSE
               JHI = JM
            END IF
         END DO bisection

         JLO = MAX(1, MIN(JLO, NVSSOL - 1))
         JHI = JLO + 1

         ICSTOR(ICL) = JLO

         ! --- interpolate between values for return variables
         VLO = VSPPSI(JLO)
         PDUM = (P - VLO) / (VSPPSI(JHI) - VLO)

         ! Error trap replaced the g8100 CYCLE
         IF (PDUM < ZERO .OR. PDUM > ONE) THEN
            IS_ERROR = .TRUE.
            EXIT OUT100
         END IF

         VLO = VSPTHE(JLO, IS)
         CTHETA(ICL) = (VSPTHE(JHI, IS) - VLO) * PDUM + VLO

         CETA(ICL) = VSPETA(JHI, IS)

         VLO = VSPDKR(JLO, IS)
         CDKR(ICL) = (VSPDKR(JHI, IS) - VLO) * PDUM + VLO

         VLO = VSPKR(JLO, IS)
         CKR(ICL) = (VSPKR(JHI, IS) - VLO) * PDUM + VLO

         VLO = VSPDET(JLO, IS)
         CDETA(ICL) = (VSPDET(JHI, IS) - VLO) * PDUM + VLO

      END DO OUT100

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

      IF (IS_ERROR) THEN
         DRY = NINT(MAX(ZERO, MIN(PDUM, ONE)))
         CALL ERROR(FFFATAL, 1034 + DRY, PPPRI, IEL, ICL, 'soil property interpolation out of range '//WETDRY(DRY))
      END IF

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

      ! Assumed external module dependencies providing global variables:
      ! LLEE, NVSEE, total_no_elements, total_no_links, top_cell_no, BEXBK,
      ! NVSERR, NVSWL, NVSLF, NVSLH, NVSBF, NVSBH, WLD, LFB, LHB, BFB, BHB,
      ! NWELBT, NWELTP, NVSSPC, NLYRBT, ZGRUND, NVSWLI, VSZWLB, VSZWLT, ZVSNOD,
      ! VSSPD, DELTAZ, INITYP, ZVSPSL, ZLYRBT, VSIPSD, VSI, VSPSI, NLYR, NTSOIL,
      ! IVSSTO, NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, VSPETA, VSPDKR, VSPDET,
      ! VSKR, EEERR, FFFATAL, PPPRI, ERROR, INITIALISE_AL_C2, VSREAD, VSCONL,
      ! VSCONC, VSSOIL, VSFUNC, half, GTZERO, LTZERO

      IMPLICIT NONE

      ! Locals
      CHARACTER(132) :: MSG
      INTEGER :: IEL, ICL, ILYR, ICBOT, ICTOP, IW, IELIN, ISTART, NAQCON
      INTEGER :: IAQCON(4, NVSEE), ISDUM(LLEE)
      DOUBLE PRECISION :: DZ, RDUM, ZGI, ZMIN
      DOUBLE PRECISION :: CDUM1(LLEE), CDUM2(LLEE), CDUM3(LLEE), CDUM4(LLEE)

   !----------------------------------------------------------------------*

      ! top_cell_no is unknown at this point. But the code to caculate top_cell_no
      ! uses DELTAZ and ZVSNOD so these use llee
      CALL INITIALISE_AL_C2()

      WRITE(PPPRI, 9010) 'Start', ' '

      NVSERR = 0
      IF (BEXBK) THEN
         ISTART = 1
      ELSE
         ISTART = total_no_links + 1
      END IF

      ! call VSREAD to read from input data file
      CALL VSREAD (NAQCON, IAQCON)

      ! Trap configuration errors immediately
      IF (NVSERR > 0) THEN
         CALL ABORT_VSIN()
         RETURN
      END IF

      ! read first lines of time-varying files
      IF (NVSWL > 0) READ (WLD, *)
      IF (NVSLF > 0) READ (LFB, *)
      IF (NVSLH > 0) READ (LHB, *)
      IF (NVSBF > 0) READ (BFB, *)
      IF (NVSBH > 0) READ (BHB, *)

      ! call VSCONL and VSCONC to set up connectivity arrays for layers and cells
      CALL VSCONL (NAQCON, IAQCON)
      CALL VSCONC()

      ! set up cell numbers for wells and springs
      ! set defaults
      DO IEL = 1, total_no_elements
         NWELBT(IEL) = 1
         NWELTP(IEL) = 1
         NVSSPC(IEL) = 0
      END DO

      element_loop_wells_springs: DO IEL = total_no_links + 1, total_no_elements
         ICBOT = NLYRBT(IEL, 1)
         ZGI = ZGRUND(IEL)
         IW  = NVSWLI(IEL)

         IF (IW > 0) THEN
            ! Find bottom well node
            RDUM = ZGI - VSZWLB(IW)
            find_bottom: DO ICL = ICBOT, top_cell_no
               IF (RDUM <= ZVSNOD(ICL, IEL)) EXIT find_bottom
            END DO find_bottom
            NWELBT(IEL) = ICL

            ! Find top well node (looping backwards)
            RDUM = ZGI - VSZWLT(IW)
            find_top: DO ICL = top_cell_no, ICBOT, -1
               IF (RDUM >= ZVSNOD(ICL, IEL)) EXIT find_top
            END DO find_top
            NWELTP(IEL) = ICL
         END IF

         RDUM = VSSPD(IEL)

         IF (GTZERO(RDUM)) THEN
            RDUM = ZGI - RDUM

            ! Find specific node based on delta Z
            find_spc: DO ICL = ICBOT, top_cell_no
               DZ = ABS(ZVSNOD(ICL, IEL) - RDUM)
               IF (DZ <= half * DELTAZ(ICL, IEL)) EXIT find_spc
            END DO find_spc
            NVSSPC(IEL) = ICL
         END IF

      END DO element_loop_wells_springs

      ! call VSSOIL to set up soil property tables
      CALL VSSOIL()

      ! set up initial conditions (read from file unit VSI, if required)
      ! type 1 - uniform phreatic surface depth, equilibrium psi profile
      IF (INITYP == 1) THEN
         DO IEL = 1, total_no_elements
            ZVSPSL(IEL) = MAX(ZLYRBT(IEL, 1), ZGRUND(IEL) - VSIPSD)
         END DO

      ! type 2 - varying phreatic surface level, equilibrium psi profile
      ELSE IF (INITYP == 2) THEN
         READ (VSI, '(A)')
         READ (VSI, *) (ZVSPSL(IEL), IEL = ISTART, total_no_elements)

      ! type 3 - 3-dimensional field of psi values (+ init. psl for output)
      ELSE
         READ (VSI, '(A)')

         element_loop_vsi: DO IEL = ISTART, total_no_elements
            READ (VSI, *) IELIN

            IF (IELIN /= IEL) THEN
               NVSERR = NVSERR + 1
               WRITE (MSG, 9040) IEL
               CALL ERROR (EEERR, 1041, PPPRI, 0, 0, MSG)
               CALL ABORT_VSIN()
               RETURN
            END IF

            ICBOT = NLYRBT(IEL, 1)
            ICTOP = top_cell_no

            READ (VSI, *) VSPSI(ICBOT:ICTOP, IEL)

            ZMIN = ZVSNOD(ICBOT, IEL) - half * DELTAZ(ICBOT, IEL)

            search_loop: DO ICL = ICBOT, ICTOP
               IF (LTZERO(VSPSI(ICL, IEL))) EXIT search_loop
            END DO search_loop

            ICL = MAX(ICBOT, ICL - 1)
            ZVSPSL(IEL) = MAX(ZMIN, ZVSNOD(ICL, IEL) + VSPSI(ICL, IEL))

         END DO element_loop_vsi

      END IF

      ! set up equilibrium psi profile for types 1 or 2
      IF (INITYP == 1 .OR. INITYP == 2) THEN
         equilibrium_profile_loop: DO IEL = 1, total_no_elements
            DO ICL = NLYRBT(IEL, 1), top_cell_no
               VSPSI(ICL, IEL) = ZVSPSL(IEL) - ZVSNOD(ICL, IEL)
            END DO
         END DO equilibrium_profile_loop
      END IF

      ! set up initial relative conductivities for all elements
      init_cond_loop: DO IEL = ISTART, total_no_elements

         DO ILYR = 1, NLYR(IEL)
            DO ICL = NLYRBT(IEL, ILYR), NLYRBT(IEL, ILYR + 1) - 1
               ISDUM(ICL) = NTSOIL(IEL, ILYR)
               IVSSTO(ICL, IEL) = 0
            END DO
         END DO

         ICBOT = NLYRBT(IEL, 1)
         ICTOP = top_cell_no

         CALL VSFUNC(NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
            VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ISDUM(ICBOT), &
            VSPSI(ICBOT, IEL), IVSSTO(ICBOT, IEL), CDUM1, CDUM2, VSKR(ICBOT, IEL), &
            CDUM3, CDUM4)

      END DO init_cond_loop

      WRITE(PPPRI, 9010) 'End', '   '

      RETURN

      ! FORMAT STATEMENTS for the host subroutine
9010  FORMAT( / '!!',78('#') / 1X,A,' of VSS data ',A,60('#') / 80('#'))
9040  FORMAT('Error reading VSS initial conditions for element ', I4, '.')

   CONTAINS

      ! Internal subroutine to handle fatal data initialisation failures cleanly
      SUBROUTINE ABORT_VSIN()
         WRITE (MSG, 9030) NVSERR
         CALL ERROR(FFFATAL, 1040, PPPRI, 0, 0, MSG)

         ! Format statement scoped correctly to the internal subroutine
9030     FORMAT(I4,' Errors have occurred in VSS data reading ', 'or initialisation.')
      END SUBROUTINE ABORT_VSIN

   END SUBROUTINE VSIN



   !SSSSSS SUBROUTINE VSINTC
   PURE SUBROUTINE VSINTC (LLEE, ICBOT, ICTOP, JELDUM, JCBC, JCACN, &
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

      ! Assumed external module dependencies providing global variables:
      ! zero

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE, ICBOT, ICTOP, JELDUM (4), JCBC (4)
      INTEGER, INTENT(IN) :: JCACN (4, ICBOT:ICTOP), JCDEL1 (LLEE, 4)
      DOUBLE PRECISION, INTENT(IN) :: CA0, CZ1 (LLEE, 4)
      DOUBLE PRECISION, INTENT(IN) :: CDELZ (ICBOT:ICTOP), CZ (ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CETA (ICBOT:ICTOP), DT, CDETA (ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CPSI (ICBOT:ICTOP), CPSIN (ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CF (ICBOT:ICTOP), CDF (ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CQ (ICBOT:ICTOP), CBETM (ICBOT:ICTOP + 1)
      DOUBLE PRECISION, INTENT(IN) :: CDBETM (ICBOT:ICTOP + 1), CDBTMM (ICBOT:ICTOP + 1)
      DOUBLE PRECISION, INTENT(IN) :: CPSI1 (LLEE, 4), CPSIN1 (LLEE, 4), CGAM1 (LLEE, 4)
      DOUBLE PRECISION, INTENT(IN) :: CDGAM1 (LLEE, 4), CDGAM2 (LLEE, 4), CGAM2 (LLEE, 4)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CA (ICBOT:ICTOP), CB (ICBOT:ICTOP), CC (ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(OUT) :: CR (ICBOT:ICTOP)

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(OUT) :: H (ICBOT - 1:ICTOP + 1)

      ! Locals
      DOUBLE PRECISION, PARAMETER :: SIGMA = 1.0D0, OMSIG = 1.0D0 - SIGMA
      INTEGER :: I, J, K, K1, P
      DOUBLE PRECISION :: CBETMI, CBETPI, CDBETP, CDBMMI, CDBTPP, CDFM, CDFP, CDG
      DOUBLE PRECISION :: CFI, CGI, DPSI, HI, HK, HK1, HM, HP, VODT

   !----------------------------------------------------------------------*

      ! Prepare effective hydraulic heads
      I = ICBOT - 1
      H(I) = zero

      DO I = ICBOT, ICTOP
         H(I) = SIGMA * CPSI(I) + OMSIG * CPSIN(I) + CZ(I)
      END DO

      I = ICTOP + 1
      H(I) = zero

      ! Set coefficients, omitting lateral terms
      DO I = ICBOT, ICTOP
         P = I + 1
         HM = H(I - 1)
         HI = H(I)
         HP = H(P)
         CFI = CF(I)
         CBETMI = CBETM(I)
         CBETPI = CBETM(P)
         CDBTPP = CDBETM(P)
         CDBMMI = CDBTMM(I)
         CDBETP = CDBTMM(P)
         CDFM = CDBMMI
         CDFP = CDBTPP
         VODT = CDELZ(I) * CA0 / DT
         CGI = CETA(I) * VODT
         CDG = CDETA(I) * VODT
         DPSI = CPSI(I) - CPSIN(I)

         CA(I) = SIGMA * CBETMI - HI * CDFM + HM * CDBMMI
         CC(I) = SIGMA * CBETPI - HI * CDFP + HP * CDBTPP
         CB(I) = HM * CDBETM(I) - HI * CDF(I) + HP * CDBETP - &
                 (SIGMA * CFI + DPSI * CDG + CGI)
         CR(I) = - (HM * CBETMI - HI * CFI + HP * CBETPI - DPSI * CGI + CQ(I))
      END DO

      ! Add lateral terms
      face_loop: DO J = 1, 4

         IF (JELDUM(J) < 1 .OR. JCBC(J) == 9) CYCLE face_loop

         internal_cell_loop: DO I = ICBOT, ICTOP
            K = JCACN(J, I)
            IF (K == 0) CYCLE internal_cell_loop

            K1 = JCDEL1(K, J) + K
            HK = SIGMA * CPSI1(K, J) + OMSIG * CPSIN1(K, J) + CZ1(K, J)
            HK1 = SIGMA * CPSI1(K1, J) + OMSIG * CPSIN1(K1, J) + CZ1(K1, J)

            CB(I) = CB(I) + HK * CDGAM1(I, J) + HK1 * CDGAM2(I, J)
            CR(I) = CR(I) - HK * CGAM1(I, J) - HK1 * CGAM2(I, J)

         END DO internal_cell_loop

      END DO face_loop

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

      ! Assumed external module dependencies providing global variables:
      ! LLEE, total_no_elements, ICMREF, LINKNS, NVSWLI, cellarea, top_cell_no,
      ! NLYRBT, QVSV, ERUZ, DELTAZ, VSTHE, DTUZ, QVSWLI, ESOILA, QVSH, zero,
      ! one, NOTZERO, JVSDEL, JVSACN

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: VSTHEN (LLEE, total_no_elements)

      ! Locals
      INTEGER :: NFACES, IFACES (4)
      INTEGER :: IEL, J, ITYPE, IFA, JEL, ICL, JFA, JCL, IW, MCL
      DOUBLE PRECISION :: AREAE, CMBE, F, Qasum

   !----------------------------------------------------------------------*

      ! --- loop over all elements
      element_loop: DO IEL = 1, total_no_elements

         ITYPE = ICMREF(IEL, 1)

         ! Choose faces to adjust (ie set NFACES and IFACES)
         IF (ITYPE == 0) THEN
            ! grids - do nothing!
            NFACES = 0

         ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
            ! banks - update only 'outer' face adjacent to grid (if there is one)
            NFACES = 0

            search_faces: DO IFA = 1, 4
               JEL = ICMREF(IEL, IFA + 4)
               IF (JEL > 0) THEN
                  IF (ICMREF(JEL, 1) == 0) THEN
                     IFACES(1) = IFA
                     NFACES = 1
                     EXIT search_faces  ! Cleanly replaces the iscycle hack and GOTO 930
                  END IF
               END IF
            END DO search_faces

         ELSE
            ! links - update faces adjacent to banks only
            NFACES = 2
            IF (LINKNS(IEL)) THEN
               IFACES(1) = 1
               IFACES(2) = 3
            ELSE
               IFACES(1) = 2
               IFACES(2) = 4
            END IF
         END IF

         ! Loop over column cells if required (top to bottom for QVSV's benefit)
         IF (NFACES > 0) THEN
            IW = NVSWLI(IEL)
            AREAE = cellarea(IEL)

            cell_balance_loop: DO ICL = top_cell_no, NLYRBT(IEL, 1), -1
               ! calculate mass balance error (m**3/s)
               MCL = ICL - 1
               CMBE = -QVSV(MCL, IEL) + QVSV(ICL, IEL) + ERUZ(IEL, ICL) + &
                      DELTAZ(ICL, IEL) * (VSTHE(ICL, IEL) - VSTHEN(ICL, IEL)) / DTUZ

               IF (IW > 0) CMBE = CMBE + QVSWLI(ICL, IW)
               IF (ICL == top_cell_no) CMBE = CMBE + ESOILA(IEL)

               CMBE = CMBE * AREAE

               DO IFA = 1, 4
                  CMBE = CMBE - QVSH(IFA, ICL, IEL)
               END DO

               ! adjust lateral flows (unless Qasum=0)
               Qasum = zero
               DO J = 1, NFACES
                  IFA = IFACES(J)
                  Qasum = Qasum + QVSH(IFA, ICL, IEL)
               END DO

               IF (NOTZERO(Qasum)) THEN
                  F = one + CMBE / Qasum
                  DO J = 1, NFACES
                     IFA = IFACES(J)
                     QVSH(IFA, ICL, IEL) = QVSH(IFA, ICL, IEL) * F
                  END DO
               END IF
            END DO cell_balance_loop
         END IF

         ! Update flows for adjacent element
         adjacent_update_loop: DO IFA = 1, 4
            JEL = ICMREF(IEL, IFA + 4)

            IF (JEL > 0) THEN
               JFA = ICMREF(IEL, IFA + 8)

               layer_update_loop: DO ICL = NLYRBT(IEL, 1), top_cell_no

                  ! 970509 (catch JEL next time around)
                  ! Immediately crash if split cells are encountered (Replacing GOTO 8820)
                  IF (JVSDEL(IFA, ICL, IEL) /= 0) THEN
                     STOP 'UNFINISHED CODE FOR SPLIT CELLS IN SUBROUTINE VSMB!'
                  END IF

                  JCL = JVSACN(IFA, ICL, IEL)
                  IF (JCL > 0) QVSH(JFA, JCL, JEL) = -QVSH(IFA, ICL, IEL)

               END DO layer_update_loop
            END IF

         END DO adjacent_update_loop

      END DO element_loop

   END SUBROUTINE VSMB



   !SSSSSS SUBROUTINE VSPREP
   SUBROUTINE VSPREP()
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

      ! Assumed global variables provided via host module(s):
      ! NVSEE, NVSWL, WLD, TIH, UZNOW, UZNEXT, WLNOW
      ! NVSLF, LFB, NVSLFT, NVSLFN, RLFNOW
      ! NVSLH, LHB, NVSLHT, NVSLHN, RLHNOW
      ! NVSLG, LGB, NVSLGT, NVSLGN, RLGNOW
      ! NVSBF, BFB, RBFNOW, NVSBH, BHB, RBHNOW
      ! FFFATAL, PPPRI

      IMPLICIT NONE

      ! Locals
      INTEGER :: I, II, III, NDUM

      ! Modernization Fix: Resurrected the saved state variables from the comments!
      ! These must be SAVED to track time-series interpolation across timesteps.
      ! DOUBLE PRECISION, SAVE :: WLLAST = 0.0D0, WLTIME = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RWELIN(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RLFLST = 0.0D0, RLFTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RLFPRV(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RLHLST = 0.0D0, RLHTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RLHPRV(NVSEE) = 0.0D0, RLHNXT(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RLGLST = 0.0D0, RLGTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RLGPRV(NVSEE) = 0.0D0, RLGNXT(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RBFLST = 0.0D0, RBFTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RBFPRV(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RBHLST = 0.0D0, RBHTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RBHPRV(NVSEE) = 0.0D0, RBHNXT(NVSEE) = 0.0D0

      ! Workspace arrays for boundary data reads
      ! DOUBLE PRECISION :: RLFDUM(NVSEE), RLHDUM(NVSEE), RLGDUM(NVSEE)


   !----------------------------------------------------------------------*

      ! wells
      IF (NVSWL > 0) THEN
         CALL FINPUT(WLD, TIH, UZNOW, UZNEXT, WLLAST, WLTIME, RWELIN, NVSWL, WLNOW)

         IF (EQMARKER(WLTIME)) THEN
            CALL ERROR(FFFATAL, 1042, PPPRI, 0, 0, 'End of well abstraction file (WLD)')
         END IF
      END IF

      ! lateral flow boundary condition
      IF (NVSLF > 0) THEN
         CALL FINPUT(LFB, TIH, UZNOW, UZNEXT, RLFLST, RLFTIM, RLFPRV, NVSLFT, RLFDUM)

         IF (EQMARKER(RLFTIM)) THEN
            CALL ERROR(FFFATAL, 1043, PPPRI, 0, 0, 'End of lateral flow boundary condition file (LFB)')
         END IF

         III = 1
         lf_main_loop: DO I = 1, NVSLF
            NDUM = NVSLFN(I)
            IF (NDUM == 0) NDUM = 1

            lf_sub_loop: DO II = 1, NDUM
               RLFNOW(II, I) = RLFDUM(III)
               III = III + 1
            END DO lf_sub_loop
         END DO lf_main_loop
      END IF

      ! lateral head boundary condition
      IF (NVSLH > 0) THEN
         CALL HINPUT(LHB, TIH, UZNOW, UZNEXT, RLHLST, RLHTIM, RLHPRV, &
                     RLHNXT, NVSLHT, RLHDUM)

         IF (EQMARKER(RLHTIM)) THEN
            CALL ERROR(FFFATAL, 1044, PPPRI, 0, 0, 'End of lateral head boundary condition file (LHB)')
         END IF

         III = 1
         lh_main_loop: DO I = 1, NVSLH
            NDUM = NVSLHN(I)
            IF (NDUM == 0) NDUM = 1

            lh_sub_loop: DO II = 1, NDUM
               RLHNOW(II, I) = RLHDUM(III)
               III = III + 1
            END DO lh_sub_loop
         END DO lh_main_loop
      END IF

      ! lateral head gradient boundary condition
      IF (NVSLG > 0) THEN
         CALL HINPUT(LGB, TIH, UZNOW, UZNEXT, RLGLST, RLGTIM, RLGPRV, &
                     RLGNXT, NVSLGT, RLGDUM)

         IF (EQMARKER(RLGTIM)) THEN
            CALL ERROR(FFFATAL, 1052, PPPRI, 0, 0, 'End of lateral head gradient boundary condition file (LGB)')
         END IF

         III = 1
         lg_main_loop: DO I = 1, NVSLG
            NDUM = NVSLGN(I)
            IF (NDUM == 0) NDUM = 1

            lg_sub_loop: DO II = 1, NDUM
               RLGNOW(II, I) = RLGDUM(III)
               III = III + 1
            END DO lg_sub_loop
         END DO lg_main_loop
      END IF

      ! column base flow boundary condition
      IF (NVSBF > 0) THEN
         CALL FINPUT(BFB, TIH, UZNOW, UZNEXT, RBFLST, RBFTIM, RBFPRV, &
                     NVSBF, RBFNOW)

         IF (EQMARKER(RBFTIM)) THEN
            CALL ERROR(FFFATAL, 1045, PPPRI, 0, 0, 'End of column base flow boundary condition file (BFB)')
         END IF
      END IF

      ! column base head boundary condition
      IF (NVSBH > 0) THEN
         CALL HINPUT(BHB, TIH, UZNOW, UZNEXT, RBHLST, RBHTIM, RBHPRV, &
                     RBHNXT, NVSBH, RBHNOW)

         IF (EQMARKER(RBHTIM)) THEN
            CALL ERROR(FFFATAL, 1046, PPPRI, 0, 0, 'End of column base head boundary condition file (BHB)')
         END IF
      END IF

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

      ! Assumed external module dependencies providing global variables:
      ! LLEE, NELEE, NLYREE, NSEE, NVSEE, total_no_elements, NVSWLI, NLBTYP,
      ! NBBTYP, NVSWLC, NLBCAT, NBBCAT, ALREAD, VSD, PPPRI, IDUM, DUMMY, BFAST,
      ! BSOILP, BHELEV, NS, NCSZON, NCRBED, INITYP, VSIPSD, VSZMIN, VSZMAX,
      ! VSWV, VSWL, IVSFLG, IVSNTB, VSK3D, VSPOR, VSTRES, VSPSS, VSVGN, VSALPH,
      ! VSPPOR, ERROR, FFFATAL, TBPSI, TBTHE, TBKR, TBTHEC, TBKRC, zero, two, one,
      ! DCSZON, DCSTOT, DCRBED, DCRTOT, BEXBK, total_no_links, NX, NY, ICMXY,
      ! ICMREF, NLYR, NTSOIL, ZLYRBT, ZGRUND, ICMBK, ZBEFF, NGDBGN, EEERR,
      ! ISRBED, DRBED, NVSWL, NVSSP, NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD,
      ! NVSLFN, NVSLHN, NVSLGN, NVSLFT, NVSLHT, NVSLGT, NVSLFL, NVSLHL, NVSLGL,
      ! NVSWLT, VSZWLB, VSZWLT, NVSSPT, VSSPD, VSSPZ, VSSPCO

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(INOUT) :: NAQCON
      INTEGER, INTENT(INOUT) :: IAQCON(4, NVSEE)

      ! Locals
      INTEGER :: I, I0, IBK, ICAT, IEL, ILYR, IS, ISP, IW, IWT, IX, IXY0, IY
      INTEGER :: ICOUNT, LCOUNT
      INTEGER :: NUM_CATEGORIES_TYPES, NELEM, NCOUNT, NDUM, NSP, NW
      INTEGER :: ILB, NLB, ITYP, NLDUM, ISDUM1, IDUM1(1)
      DOUBLE PRECISION :: DCSDUM(0:LLEE)
      DOUBLE PRECISION :: DCSNOD(LLEE), DCRDUM(0:LLEE), DCRNOD(LLEE), SIG, PDUM
      DOUBLE PRECISION :: XDUM(NVSEE), YDUM(NVSEE), Y2DUM(NVSEE), UDUM(NVSEE)
      CHARACTER(LEN=80)  :: CDUM
      CHARACTER(LEN=132) :: MSG

   !----------------------------------------------------------------------*
      ! Initialization

      CALL initialise_vsread_buffers()

      DO IEL = 1, total_no_elements
         NVSWLI(IEL) = 0
         NLBTYP(IEL) = 0
         NBBTYP(IEL) = 0
         NVSWLC(IEL) = 1
         NLBCAT(IEL) = 1
         NBBCAT(IEL) = 1
      END DO

      ! VS01 ----- main data file title
      CALL ALREAD (1, VSD, PPPRI, ':VS01', 1, 1, 0, CDUM, IDUM, DUMMY)
      WRITE(PPPRI, '(/, 1X, A, /)') TRIM(CDUM)

      ! VS02 ----- logical flags
      READ (VSD, '(A)') CDUM
      READ (VSD, *) BFAST, BSOILP, BHELEV

      ! VS03 ----- integer variables
      CALL ALREAD (2, VSD, PPPRI, ':VS03', 4, 1, 0, CDUM, IDUM, DUMMY)
      NS = IDUM(1)
      NCSZON = IDUM(2)
      NCRBED = IDUM(3)
      INITYP = IDUM(4)

      ! VS04 ----- real variables
      CALL ALREAD (3, VSD, PPPRI, ':VS04', 5, 1, 0, CDUM, IDUM, DUMMY)
      VSIPSD = DUMMY(1)
      VSZMIN = DUMMY(2)
      VSZMAX = DUMMY(3) + 1.0D-6
      VSWV   = DUMMY(4)
      VSWL   = DUMMY(5)

      ! VS05 ----- physical property data
      CALL ALREAD (7, VSD, PPPRI, ':VS05', NSEE, 8, NS, CDUM, ISDUM_VSREAD, RSDUM_VSREAD)

      DO IS = 1, NS
         IVSFLG(IS) = ISDUM_VSREAD(IS, 2)
         IVSNTB(IS) = ISDUM_VSREAD(IS, 3)
         VSK3D(IS, 1) = RSDUM_VSREAD(IS, 1) / (3600.0D0 * 24.0D0)
         VSK3D(IS, 2) = RSDUM_VSREAD(IS, 2) / (3600.0D0 * 24.0D0)
         VSK3D(IS, 3) = RSDUM_VSREAD(IS, 3) / (3600.0D0 * 24.0D0)
         VSPOR(IS) = RSDUM_VSREAD(IS, 4)
         VSTRES(IS) = RSDUM_VSREAD(IS, 5)
         VSPSS(IS) = RSDUM_VSREAD(IS, 6)
         VSVGN(IS) = RSDUM_VSREAD(IS, 7)
         VSALPH(IS) = RSDUM_VSREAD(IS, 8)
         VSPPOR(IS) = VSPOR(IS)
      END DO

      ! VS05a ---- soil characteristic function tabulated data
      DO IS = 1, NS
         IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
            READ (VSD, *) ISDUM1
            IF (IS /= ISDUM1) THEN
               WRITE (MSG, 9030) IS
               CALL ERROR(FFFATAL, 1051, PPPRI, 0, 0, MSG)
            END IF

            DO I = 1, IVSNTB(IS)
               READ (VSD, *) TBPSI(I, IS), TBTHE(I, IS), TBKR(I, IS)
            END DO

            ! set up cubic spline coefficients for theta, using log(psi)
            ! based on routines 'spline' and 'splint' in NUMERICAL RECIPES
            ! FOR FORTRAN (..UNFINISHED), pp 109 and 110
            ! NB assumes 'natural' boundary conditions (ie zero 2nd derivatives)
            DO I = 1, IVSNTB(IS)
               XDUM(I) = LOG10(-TBPSI(I, IS))
               YDUM(I) = TBTHE(I, IS)
            END DO

            NDUM = IVSNTB(IS)
            Y2DUM(1) = zero
            UDUM(1) = zero
            Y2DUM(NDUM) = zero

            DO I = 2, NDUM - 1
               SIG = (XDUM(I) - XDUM(I - 1)) / (XDUM(I + 1) - XDUM(I - 1))
               PDUM = SIG * Y2DUM(I - 1) + two
               Y2DUM(I) = (SIG - one) / PDUM
               UDUM(I) = (6.0D0 * ((YDUM(I + 1) - YDUM(I)) / &
                  (XDUM(I + 1) - XDUM(I)) - (YDUM(I) - YDUM(I - 1)) &
                  / (XDUM(I) - XDUM(I - 1))) / (XDUM(I + 1) - XDUM(I - 1)) &
                  - SIG * UDUM(I - 1)) / PDUM
            END DO

            DO I = NDUM - 1, 1, -1
               Y2DUM(I) = Y2DUM(I) * Y2DUM(I + 1) + UDUM(I)
            END DO

            DO I = 1, NDUM
               TBTHEC(I, IS) = Y2DUM(I)
            END DO

            ! if required, set up cubic spline coefficients for Kr similarly
            IF (IVSFLG(IS) == 2) THEN
               DO I = 1, IVSNTB(IS)
                  YDUM(I) = TBKR(I, IS)
               END DO

               Y2DUM(1) = zero
               UDUM(1) = zero
               Y2DUM(NDUM) = zero

               DO I = 2, NDUM - 1
                  SIG = (XDUM(I) - XDUM(I - 1)) / (XDUM(I + 1) - XDUM(I - 1))
                  PDUM = SIG * Y2DUM(I - 1) + two
                  Y2DUM(I) = (SIG - one) / PDUM
                  UDUM(I) = (6.0D0 * ((YDUM(I + 1) - YDUM(I)) / &
                     (XDUM(I + 1) - XDUM(I)) - (YDUM(I) - YDUM(I - 1)) / &
                     (XDUM(I) - XDUM(I - 1))) / (XDUM(I + 1) - XDUM(I - 1)) &
                     - SIG * UDUM(I - 1)) / PDUM
               END DO

               DO I = NDUM - 1, 1, -1
                  Y2DUM(I) = Y2DUM(I) * Y2DUM(I + 1) + UDUM(I)
               END DO

               DO I = 1, NDUM
                  TBKRC(I, IS) = Y2DUM(I)
               END DO
            END IF
         END IF
      END DO

      ! VS06 ----- soil zone cell sizes (start at the ground surface)
      IF (NCSZON > 0) THEN
         CALL ALREAD (3, VSD, PPPRI, ':VS06', NCSZON, 1, 0, CDUM, IDUM, DCSZON)
      END IF
      WRITE(PPPRI, *) 'DCSZON: ', (DCSZON(I), I = 1, NCSZON)

      DCSTOT = zero
      DCSDUM(0) = zero

      DO I = 1, NCSZON
         DCSTOT = DCSTOT + DCSZON(I)
         DCSDUM(I) = DCSTOT
         DCSNOD(I) = half * (DCSDUM(I) + DCSDUM(I - 1))
      END DO

      DCSNOD(NCSZON + 1) = DCSTOT + VSZMIN

      ! VS07 ----- river bed cell sizes (start at the bed surface)
      IF (NCRBED > 0) THEN
         CALL ALREAD (3, VSD, PPPRI, ':VS07', NCRBED, 1, 0, CDUM, IDUM, DCRBED)
      END IF
      WRITE(PPPRI, *) 'DCRBED: ', (DCRBED(I), I = 1, NCRBED)

      DCRTOT = zero
      DCRDUM(0) = zero

      DO I = 1, NCRBED
         DCRTOT = DCRTOT + DCRBED(I)
         DCRDUM(I) = DCRTOT
         DCRNOD(I) = half * (DCRDUM(I) + DCRDUM(I - 1))
      END DO

      DCRNOD(NCRBED + 1) = DCRTOT + VSZMIN

      ! VS08 ----- soil/lithology layer definition data
      ! --- read no. of categories and elements
      CALL ALREAD (2, VSD, PPPRI, ':VS08', 2, 1, 0, CDUM, IDUM, DUMMY)
      NUM_CATEGORIES_TYPES = IDUM(1)
      NELEM = IDUM(2)

      ! --- category data
      IF (NUM_CATEGORIES_TYPES == 0) THEN
         ! expect all elements to be input individually
         IF (BEXBK) THEN
            NCOUNT = total_no_elements - 2 * total_no_links
         ELSE
            NCOUNT = total_no_elements - total_no_links
         END IF

      ELSE
         ! initialise arrays
         DO IEL = 1, NELEE
            DO ILYR = 1, NLYREE
               IVSDUM_VSREAD(IEL, ILYR) = 0
               RVSDUM_VSREAD(IEL, ILYR) = zero
            END DO
         END DO

         ! read layer data
         CALL ALREAD (6, VSD, PPPRI, ':VS08a', NELEE, NLYREE, NUM_CATEGORIES_TYPES, CDUM, IVSDUM_VSREAD, RVSDUM_VSREAD)

         ! for NUM_CATEGORIES_TYPES = 1, set all elements = category 1
         IF (NUM_CATEGORIES_TYPES == 1) THEN
            DO IEL = 1, total_no_elements
               IVSCAT_VSREAD(IEL) = 1
            END DO

         ! for > 1 category read in categories for links (if required) and grids
         ELSE
            IF (BEXBK .AND. total_no_links > 0) THEN
               CALL ALREAD (2, VSD, PPPRI, ':VS08b', total_no_links, 1, NUM_CATEGORIES_TYPES, CDUM, IVSCAT_VSREAD, DUMMY)
            END IF

            CALL ALREAD (4, VSD, PPPRI, ':VS08c', NX, NY, NUM_CATEGORIES_TYPES, CDUM, IDUM, DUMMY)

            DO IY = 1, NY
               IXY0 = (IY - 1) * NX
               DO IX = 1, NX
                  IEL = ICMXY(IX, IY)
                  IF (IEL /= 0) IVSCAT_VSREAD(IEL) = IDUM(IXY0 + IX)
               END DO
            END DO
         END IF

         ! move layer data into elements for ...
         NCOUNT = 0
         element_category_loop: DO IEL = 1, total_no_elements
            IF (ICMREF(IEL, 1) == 1 .OR. ICMREF(IEL, 1) == 2 .OR. &
               (.NOT. BEXBK .AND. ICMREF(IEL, 1) == 3)) CYCLE element_category_loop

            IF (IVSCAT_VSREAD(IEL) == 0) THEN
               NCOUNT = NCOUNT + 1
            ELSE
               BDONE_VSREAD(IEL) = .TRUE.
               ICAT = IVSCAT_VSREAD(IEL)
               ICOUNT = 0

               ! Modern DO WHILE replacing GOTO 350 / 355
               DO WHILE (IVSDUM_VSREAD(ICAT, ICOUNT + 1) /= 0)
                  ICOUNT = ICOUNT + 1
               END DO

               ! ...grids
               IF (ICMREF(IEL, 1) == 0) THEN
                  NLYR(IEL) = ICOUNT
                  DO ILYR = 1, NLYR(IEL)
                     NTSOIL(IEL, ILYR) = IVSDUM_VSREAD(ICAT, ILYR)
                     ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - RVSDUM_VSREAD(ICAT, ILYR)
                  END DO

               ! ...banks
               ELSE
                  DO I = 1, 2
                     IBK = ICMBK(IEL, I)
                     BDONE_VSREAD(IBK) = .TRUE.
                     NLYR(IBK) = ICOUNT
                     DO ILYR = 1, NLYR(IBK)
                        NTSOIL(IBK, ILYR) = IVSDUM_VSREAD(ICAT, ILYR)
                        ZLYRBT(IBK, ILYR) = ZGRUND(IBK) - RVSDUM_VSREAD(ICAT, ILYR)
                     END DO
                  END DO

                  ! ...links (NB uses data from bank 2, which is identical to bank 1)
                  LCOUNT = 0

                  ! Modern DO WHILE replacing GOTO 390 / 395
                  DO WHILE (RVSDUM_VSREAD(ICAT, LCOUNT + 1) >= ZGRUND(IBK) - ZBEFF(IEL) + VSZMIN)
                     LCOUNT = LCOUNT + 1
                  END DO

                  NLYR(IEL) = LCOUNT
                  DO ILYR = 1, NLYR(IEL)
                     NTSOIL(IEL, ILYR) = NTSOIL(IBK, ILYR)
                     ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
                  END DO
               END IF
            END IF
         END DO element_category_loop
      END IF

      ! check no. of category elements consistent with no. of individual elements
      IF (NCOUNT /= NELEM) THEN
         WRITE (MSG, 9000) NCOUNT
         CALL ERROR(FFFATAL, 1032, PPPRI, 0, 0, MSG)
      END IF

      ! --- element data
      IF (NELEM /= 0) THEN
         ! initialise variables
         DO IEL = 1, NELEE
            DO ILYR = 1, NLYREE
               IVSDUM_VSREAD(IEL, ILYR) = 0
               RVSDUM_VSREAD(IEL, ILYR) = zero
            END DO
         END DO

         ! read layer data
         CALL ALREAD (6, VSD, PPPRI, ':VS08d', NELEE, NLYREE, NELEM, CDUM, IVSDUM_VSREAD, RVSDUM_VSREAD)

         element_data_loop: DO IEL = 1, total_no_elements
            ! ignore banks, links (if no banks), and elements already processed
            IF (BDONE_VSREAD(IEL) .OR. ICMREF(IEL, 1) == 1 .OR. ICMREF(IEL, 1) == 2 .OR. &
               (.NOT. BEXBK .AND. ICMREF(IEL, 1) == 3)) CYCLE element_data_loop

            BDONE_VSREAD(IEL) = .TRUE.
            ICOUNT = 0

            DO WHILE (IVSDUM_VSREAD(IEL, ICOUNT + 1) /= 0)
               ICOUNT = ICOUNT + 1
            END DO

            ! ...grids
            IF (ICMREF(IEL, 1) == 0) THEN
               NLYR(IEL) = ICOUNT
               DO ILYR = 1, NLYR(IEL)
                  NTSOIL(IEL, ILYR) = IVSDUM_VSREAD(IEL, ILYR)
                  ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - RVSDUM_VSREAD(IEL, ILYR)
               END DO

            ! ...banks
            ELSE
               DO I = 1, 2
                  IBK = ICMBK(IEL, I)
                  BDONE_VSREAD(IBK) = .TRUE.
                  NLYR(IBK) = ICOUNT
                  DO ILYR = 1, NLYR(IBK)
                     NTSOIL(IBK, ILYR) = IVSDUM_VSREAD(IEL, ILYR)
                     ZLYRBT(IBK, ILYR) = ZGRUND(IBK) - RVSDUM_VSREAD(IEL, ILYR)
                  END DO
               END DO

               ! ...links
               LCOUNT = 0
               DO WHILE (RVSDUM_VSREAD(IEL, LCOUNT + 1) >= ZGRUND(IBK) - ZBEFF(IEL) + VSZMIN)
                  LCOUNT = LCOUNT + 1
               END DO

               NLYR(IEL) = LCOUNT
               DO ILYR = 1, NLYR(IEL)
                  NTSOIL(IEL, ILYR) = NTSOIL(IBK, ILYR)
                  ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
               END DO
            END IF
         END DO element_data_loop
      END IF

      ! adjust horizon boundaries in soil zone to match computational mesh
      ! and set up ZLYRBT for ground surface
      adjust_horizon_loop: DO IEL = NGDBGN, total_no_elements
         layer_adjust_loop: DO ILYR = NLYR(IEL), 1, -1
            IF (ZGRUND(IEL) - ZLYRBT(IEL, ILYR) > DCSTOT + VSZMIN) EXIT layer_adjust_loop

            search_zone_loop: DO I = 1, NCSZON + 1
               IF (DCSNOD(I) > ZGRUND(IEL) - ZLYRBT(IEL, ILYR)) THEN
                  ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - DCSDUM(I - 1)
                  CYCLE layer_adjust_loop
               END IF
            END DO search_zone_loop
         END DO layer_adjust_loop

         ZLYRBT(IEL, NLYR(IEL) + 1) = ZGRUND(IEL)
      END DO adjust_horizon_loop

      IF (BEXBK) THEN
         DO IEL = 1, total_no_links
            IBK = ICMBK(IEL, 1)
            DO ILYR = 1, NLYR(IEL)
               ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
            END DO
         END DO
      END IF

      ! check that all elements have been set up
      check_done_loop: DO IEL = 1, total_no_elements
         IF (.NOT. BEXBK .AND. ICMREF(IEL, 1) /= 0) CYCLE check_done_loop
          IF (.NOT. BDONE_VSREAD(IEL)) THEN
            WRITE (MSG, 9020) IEL
            CALL ERROR (EEERR, 1033, PPPRI, 0, 0, MSG)
         END IF
      END DO check_done_loop

      ! VS09 ----- channel bed layer
      IF (total_no_links > 0 .AND. BEXBK) THEN
         ! read soil types for each link
         CALL ALREAD (2, VSD, PPPRI, ':VS09', total_no_links, 1, 1, CDUM, ISRBED, DUMMY)

         ! read bed depths for each link
         CALL ALREAD (3, VSD, PPPRI, ':VS09a', total_no_links, 1, 1, CDUM, IDUM, DRBED)

         ! set up channel bed layer for each link
         DO IEL = 1, total_no_links
            IF (DRBED(IEL) > VSZMIN) THEN
               NLYR(IEL) = NLYR(IEL) + 1
               NTSOIL(IEL, NLYR(IEL)) = ISRBED(IEL)
               ZLYRBT(IEL, NLYR(IEL)) = ZBEFF(IEL) - DRBED(IEL)

               IF (ZLYRBT(IEL, NLYR(IEL)) < ZLYRBT(IEL, NLYR(IEL) - 1) + VSZMIN) THEN
                  NLYR(IEL) = NLYR(IEL) - 1
                  NTSOIL(IEL, NLYR(IEL)) = ISRBED(IEL)
               END IF
            END IF
         END DO

         ! adjust horizon boundaries in river bed to match computational mesh
         ! and set up ZLYRBT for river bed surface
         bed_adjust_loop: DO IEL = 1, total_no_links
            layer_bed_loop: DO ILYR = NLYR(IEL), 1, -1
               IF (ZGRUND(IEL) - ZLYRBT(IEL, ILYR) > DCRTOT + VSZMIN) EXIT layer_bed_loop

               search_bed_loop: DO I = 1, NCRBED + 1
                  IF (DCRNOD(I) > ZGRUND(IEL) - ZLYRBT(IEL, ILYR)) THEN
                     ZLYRBT(IEL, ILYR) = ZBEFF(IEL) - DCRDUM(I - 1)
                     CYCLE layer_bed_loop
                  END IF
               END DO search_bed_loop
            END DO layer_bed_loop

            ZLYRBT(IEL, NLYR(IEL) + 1) = ZBEFF(IEL)
         END DO bed_adjust_loop
      END IF

      ! VS10 ----- aquifer zone user-defined connectivities
      ! FIX: Read into the IDUM array first to satisfy strict array-interface
      ! requirements, then assign the value to the scalar NAQCON.
      CALL ALREAD (2, VSD, PPPRI, ':VS10', 1, 1, 0, CDUM, IDUM, DUMMY)
      NAQCON = IDUM(1)

      IF (NAQCON > 0) THEN
         CALL ALREAD (2, VSD, PPPRI, ':VS10a', 4, NAQCON, 0, CDUM, IAQCON, DUMMY)
      END IF

      ! VS11 ----- no. of categories for boundary conditions
      CALL ALREAD (2, VSD, PPPRI, ':VS11', 8, 1, 0, CDUM, IDUM, DUMMY)
      NVSWL = IDUM(1)
      NVSSP = IDUM(2)
      NVSLF = IDUM(3)
      NVSLH = IDUM(4)
      NVSLG = IDUM(5)
      NVSBF = IDUM(6)
      NVSBH = IDUM(7)
      NVSBD = IDUM(8)

      ! wells -----------------------------------------------
      ! VS12 ----- no. of wells
      IF (NVSWL > 0) THEN
         CALL ALREAD (2, VSD, PPPRI, ':VS12', 1, 1, 0, CDUM, IDUM, DUMMY)
         NW = IDUM(1)

         ! VS12a ---- element, category number, and target element
         CALL ALREAD (2, VSD, PPPRI, ':VS12a', 3, NW, 0, CDUM, IDUM, DUMMY)
         DO IW = 1, NW
            I0 = 3 * (IW - 1)
            IEL = IDUM(I0 + 1)
            NVSWLC(IEL) = MAX(1, IDUM(I0 + 2))
            IWT = IDUM(I0 + 3)
            IF (IWT > 0) NVSWLT(IWT) = IEL
            NVSWLI(IEL) = IW
         END DO

         ! VS12b ---- depth below ground of bottom and top of well screen
         CALL ALREAD (3, VSD, PPPRI, ':VS12b', 2, NW, 0, CDUM, IDUM, DUMMY)
         DO IW = 1, NW
            VSZWLB(IW) = DUMMY(2 * (IW - 1) + 1)
            VSZWLT(IW) = DUMMY(2 * (IW - 1) + 2)
         END DO
      END IF

      ! springs ---------------------------------------------
      ! VS13 ----- no. of springs
      IF (NVSSP > 0) THEN
         NSP = NVSSP
         ! VS13a ---- element and target element
         CALL ALREAD (2, VSD, PPPRI, ':VS13a', 2, NSP, 0, CDUM, IDUM, DUMMY)
         DO ISP = 1, NSP
            IEL = IDUM(2 * (ISP - 1) + 1)
            IF (IDUM(2 * (ISP - 1) + 2) > 0) NVSSPT(IDUM(2 * (ISP - 1) + 2)) = IEL
         END DO

         ! VS13b ---- depth of spring source below ground, elevation of
         !            discharge point, spring coefficient
         CALL ALREAD (3, VSD, PPPRI, ':VS13b', 3, NSP, 0, CDUM, IDUM1, DUMMY)
         DO ISP = 1, NSP
            IEL = IDUM(2 * (ISP - 1) + 1)
            VSSPD(IEL) = DUMMY(3 * (ISP - 1) + 1)
            VSSPZ(IEL) = DUMMY(3 * (ISP - 1) + 2)
            VSSPCO(IEL) = DUMMY(3 * (ISP - 1) + 3)
         END DO
      END IF

      ! lateral boundary conditions -------------------------
      ! VS14 ----- grid of codes (types)
      NDUM = MAX(NVSLF, NVSLH, NVSLG)

      IF (NDUM > 0) THEN
         CALL ALREAD (4, VSD, PPPRI, ':VS14', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NLBTYP(IEL) = IDUM(IXY0 + IX)
            END DO
         END DO

         ! VS15 ----- grid of category numbers
         CALL ALREAD (4, VSD, PPPRI, ':VS15', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NLBCAT(IEL) = MAX(1, IDUM(IXY0 + IX))
            END DO
         END DO

         ! VS16 ----- No. of lateral boundary categories (flow, head, and head gr
         ! with b.c/s set only on selected layers
         ! initialise arrays to default values for reading in time-series data
         DO ICAT = 1, NDUM
            NVSLFN(ICAT) = 0
            NVSLHN(ICAT) = 0
            NVSLGN(ICAT) = 0
         END DO

         NVSLFT = NVSLF
         NVSLHT = NVSLH
         NVSLGT = NVSLG

         CALL ALREAD (2, VSD, PPPRI, ':VS16', 1, 1, 0, CDUM, IDUM, DUMMY)
         NLB = IDUM(1)

         DO ILB = 1, NLB
            ! VS16a ---- b.c. type, category, no. of layers
            CALL ALREAD (2, VSD, PPPRI, ':VS16a', 3, 1, 0, CDUM, IDUM, DUMMY)
            ITYP = IDUM(1)
            ICAT = IDUM(2)
            NLDUM = IDUM(3)

            ! VS16b ---- layer numbers
            CALL ALREAD (2, VSD, PPPRI, ':VS16b', NLDUM, 1, 0, CDUM, IDUM, DUMMY)

            IF (ITYP == 3) THEN
               NVSLFN(ICAT) = NLDUM
               NVSLFT = NVSLFT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLFL(I, ICAT) = IDUM(I)
               END DO
            END IF

            IF (ITYP == 4) THEN
               NVSLHN(ICAT) = NLDUM
               NVSLHT = NVSLHT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLHL(I, ICAT) = IDUM(I)
               END DO
            END IF

            IF (ITYP == 5) THEN
               NVSLGN(ICAT) = NLDUM
               NVSLGT = NVSLGT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLGL(I, ICAT) = IDUM(I)
               END DO
            END IF
         END DO
      END IF

      ! bottom boundary conditions --------------------------
      ! VS17 ----- grid of codes (types)
      NDUM = MAX(NVSBF, NVSBH, NVSBD)

      IF (NDUM > 0) THEN
         IF (total_no_links > 0 .AND. BEXBK) THEN
            CALL ALREAD (2, VSD, PPPRI, ':VS17', total_no_links, 1, 1, CDUM, IDUM, DUMMY)
            DO IEL = 1, total_no_links
               NBBTYP(IEL) = IDUM(IEL)
               NBBTYP(total_no_links + IEL) = IDUM(IEL)
               NBBTYP(2 * total_no_links + IEL) = IDUM(IEL)
            END DO
         END IF

         CALL ALREAD (4, VSD, PPPRI, ':VS17', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NBBTYP(IEL) = IDUM(IXY0 + IX)
            END DO
         END DO

         ! VS18 ----- grid of category numbers
         IF (total_no_links > 0 .AND. BEXBK) THEN
            CALL ALREAD (2, VSD, PPPRI, ':VS18', total_no_links, 1, 1, CDUM, IDUM, DUMMY)
            DO IEL = 1, total_no_links
               ICAT = MAX(1, IDUM(IEL))
               NBBCAT(IEL) = ICAT
               NBBCAT(total_no_links + IEL) = ICAT
               NBBCAT(2 * total_no_links + IEL) = ICAT
            END DO
         END IF

         CALL ALREAD (4, VSD, PPPRI, ':VS18', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NBBCAT(IEL) = MAX(1, IDUM(IXY0 + IX))
            END DO
         END DO
      END IF

      RETURN

      ! FORMAT statements
9000  FORMAT('Error in number of VSS layer elements. NELEM should be ',I4)
9020  FORMAT('Error reading VSS layers for element ',I4, '.')
9030  FORMAT('Soil type ',I4,' not expected for soil property tables.')

   END SUBROUTINE VSREAD



   !SSSSSS SUBROUTINE VSSAI
   PURE SUBROUTINE VSSAI(FACE, JCBC, ICBOT, ICTOP, ICBED, CDELL, CZ, &
                         CAIJ, CZS, CPSI, CKIJ, CDKIJ, CB, CR, CQH, depadj, cdelz)
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
   !      SPA, 03/11/98 depadj added
   !----------------------------------------------------------------------*
   ! Entry conditions:
   !     1 <= FACE <= 4
   ! ICBOT <= ICBED+1, ICTOP
   !     0 <  CDELL
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: FACE, JCBC, ICBOT, ICTOP, ICBED
      DOUBLE PRECISION, INTENT(IN) :: CDELL, CZS, depadj
      DOUBLE PRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP), CPSI(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CAIJ(4, ICBOT:ICTOP), cdelz(ICBOT:ICTOP)
      DOUBLE PRECISION, INTENT(IN) :: CKIJ(ICBOT:ICTOP), CDKIJ(ICBOT:ICTOP)

      ! In+out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: CB(ICBOT:ICTOP), CR(ICBOT:ICTOP)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CQH(4, ICBOT:ICTOP)

      ! Locals
      INTEGER :: ICL, IDUM
      DOUBLE PRECISION :: QDUM, DQDUM, AOL, DH, KIJ, DDUM

   !----------------------------------------------------------------------*

      ! set lowest cell in exposed bank face
      IF (JCBC == 9) THEN
         ! in effect stream bed is at base of current land element
         IDUM = ICBOT
      ELSE
         ! stream-aquifer interaction with banks
         IDUM = ICBED + 1
      END IF

      ! loop over appropriate cells
      cell_loop: DO ICL = IDUM, ICTOP

         DH = CZS - CZ(ICL) - CPSI(ICL)

         ! !!!!! change to calculation of AOL for flow out of channel
         ! limits flows if depth of water in channel is low, or zero
         ! SPA, 03/11/98
         DDUM = 1.0D0
         IF (GTZERO(DH)) DDUM = MIN(ONE, depadj / cdelz(ICL))

         AOL = (DDUM * CAIJ(FACE, ICL)) / CDELL
         KIJ = CKIJ(ICL)

         ! !!!! SPA, 03/11/98.  Change definition of flow derivative
         ! DQDUM =   ( CDKIJ(ICL)*DH - KIJ ) * AOL
         DQDUM = -KIJ * AOL

         QDUM = KIJ * DH * AOL
         CQH(FACE, ICL) = QDUM

         CB(ICL) = CB(ICL) + DQDUM
         CR(ICL) = CR(ICL) - QDUM

      END DO cell_loop

   END SUBROUTINE VSSAI



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
   ! JE   JAN 2009      Loop restructure for AD
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
   !         LL-NLYRBT(e,1)+1 >= NRD(veg),
   !                              where veg=NVC(e)
   ! ...
   !----------------------------------------------------------------------*
   ! Limited ranges:
   !                    range (e,1:LLEE):  (e,NLYRBT(e,1):LL) only
   !                    range   1:NLFEE:               1:NLF only
   ! JVSACN(face,cell,e), NLYR(e), NTSOIL(e,layer), NVSWLI(e):
   !              for e in istart:NEL only, where istart is defined above
   ! VSKR(e,cell): input for any e having a neighbour earlier in ISORT list
   !              output for e in istart:NEL
   ! ...
   !----------------------------------------------------------------------*
   SUBROUTINE VSSIM ()

      IMPLICIT NONE

      ! Locals, etc
      INTEGER, PARAMETER :: NITMAX = 10, NITMIN = 2
      DOUBLE PRECISION, PARAMETER :: GEPSMX = 1.0D-4, DRYH = 1.0D-8

      INTEGER :: N, IFDUM1, IFDUM2, NIT, NCELL, WET, ICDUM, K, ELEVEL
      INTEGER :: I, II, IEL, IFA, ICL, ILYR, IW, ITYPE, IBK, ISTART, IBANK
      INTEGER :: JEL, JFA, JCL, JCBED, JELDUM (4)
      INTEGER :: ICBOT, ICTOP, ICWCAT, ICLBCT, ICBBCT, ICBED, ICWLBT

      DOUBLE PRECISION :: DPSIEL, DPSIMX
      DOUBLE PRECISION :: CDW, CES, CQW, QBK, QI
      DOUBLE PRECISION :: CA0, DXYDUM

      INTEGER, SAVE :: errorcount2 = 0
      LOGICAL :: TEST, g670

      ! Note: Variables mapped from implicit context (LLEE, NELEE, NLYREE, etc.)
      ! are retained here strictly according to user rules.
      INTEGER :: JCDEL1 (LLEE, 4), ICLYRB (NLYREE)
      DOUBLE PRECISION :: DELTAP (0:NELEE), CDNET (NELEE), CQ (LLEE, NELEE)
      DOUBLE PRECISION :: CDELL (4), CDELL1 (4), CAIJ1 (LLEE, 4), CZ1 (LLEE, 4)
      DOUBLE PRECISION :: PSIM (LLEE), VSPSIN (LLEE, NELEE), VSTHEN (LLEE, NELEE)
      DOUBLE PRECISION :: CPSI1 (LLEE, 4), CPSIN1 (LLEE, 4), CKIJ1 (LLEE, 4), CZS (4)

      !!!!!! Extra array: depadj - depth of surface water for adjacent
      ! elements - added for channel aquifer flows fix, SPA, 03/11/98
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      DOUBLE PRECISION :: depadj (4)
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      LOGICAL :: OK (NELEE)

      !----------------------------------------------------------------------*
      ! Initialization
      !________________*
      IF (BEXBK) THEN
         IBANK = 1
         ISTART = 1
      ELSE
         IBANK = 0
         ISTART = total_no_links + 1
      END IF

      ICTOP = top_cell_no

      IF (FIRSTvssim) THEN

         FIRSTvssim = .FALSE.

         ! * set outputs & locals for non-column elements
         ! Replaced ALINIT with array slices
         IF (ISTART > 1) QH(1 : ISTART - 1) = ZERO

         DO IEL = 1, ISTART - 1
            ICBOT = NLYRBT (IEL, 1)
            QVSH(1:4, ICBOT:ICTOP, IEL) = ZERO
            VSAIJsv(1:4, ICBOT:ICTOP, IEL) = ZERO
            DO ICL = ICBOT, ICTOP
               ICSOILsv (ICL, IEL) = 1
            END DO
         END DO

         ! * set static locals for column elements
         DO IEL = ISTART, total_no_elements
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
            DO II = 1, 5
               JCBCsv (II, IEL) = 0
            END DO

            JCBCsv (0, IEL) = NBBTYP (IEL)
            IFA = MAX (1, NBFACE (IEL))
            JCBCsv (IFA, IEL) = NLBTYP (IEL)

            IF (NVSWLI (IEL) > 0) JCBCsv (5, IEL) = 1
            IF (NVSSPC (IEL) > 0) JCBCsv (5, IEL) = 2

            DO IFA = 1, 4
               JEL = ICMREF (IEL, IFA + 4)
               TEST = IEL > total_no_links .AND. JEL >= 1 .AND. JEL <= total_no_links
               IF (TEST) JCBCsv (IFA, IEL) = 9 + IBANK

               ! VSAIJ contains cell-face areas for lateral flow (note face 1=3, 2=4)
               IFDUM1 = MOD (IFA, 4) + 1
               IFDUM2 = MOD (IFA + 2, 4) + 1
               DXYDUM = DHF (IEL, IFDUM1) + DHF (IEL, IFDUM2)

               DO ICL = NLYRBT (IEL, 1), ICTOP
                  VSAIJsv (IFA, ICL, IEL) = DELTAZ (ICL, IEL) * DXYDUM
               END DO
            END DO

            ! ICSOIL contains soil types for each cell
            DO ILYR = 1, NLYR (IEL)
               N = NTSOIL (IEL, ILYR)
               DO ICL = NLYRBT (IEL, ILYR), NLYRBT (IEL, ILYR + 1) - 1
                  ICSOILsv (ICL, IEL) = N
               END DO
            END DO

         END DO
      END IF


      ! prepare catchment boundary condition data
      CALL VSPREP


      !!!!!! Calc. depth of water for channel links, even if no banks
      ! n.b. rainfall and evap terms neglected, as these are calculated for
      ! channels after VSS is called.
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      IF (.NOT. bexbk) THEN
         DO IEL = 1, total_no_links
            CDNET (IEL) = GEThrf (IEL) - zgrund (IEL)
         END DO
      END IF
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      DO IEL = ISTART, total_no_elements

         CES = ESOILA (IEL)
         CDW = GETHRF (IEL) - ZGRUND (IEL)

         CDNET (IEL) = (PNETTO (IEL) - (EEVAP (IEL) - CES)) * DTUZ + CDW
         CA0 = cellarea (IEL)
         ICBOT = NLYRBT (IEL, 1)
         ICDUM = ICTOP + 1

         IF (IEL > total_no_links) ICDUM = ICDUM - NRD (NVC (IEL))

         ! Replaced ALINIT with array slice
         IF (ICDUM > ICBOT) CQ(ICBOT : ICDUM - 1, IEL) = ZERO

         ! stop crash if rooting zone is below base of aquifer sb 020211
         ICDUM = MAX (1, ICDUM)

         DO ICL = ICDUM, ICTOP
            CQ (ICL, IEL) = -ERUZ (IEL, ICL) * CA0
         END DO

         CQ (ICTOP, IEL) = CQ (ICTOP, IEL) - CES * CA0

      END DO

      ! save psi values at time level N
      DO IEL = 1, total_no_elements
         ICBOT = NLYRBT (IEL, 1)
         NCELL = ICTOP - ICBOT + 1
         CALL DCOPY (NCELL, VSPSI (ICBOT, IEL), 1, VSPSIN (ICBOT, IEL), 1)
         CALL DCOPY (NCELL, VSTHE (ICBOT, IEL), 1, VSTHEN (ICBOT, IEL), 1)
      END DO

      ! initialize convergence indicators (Replaced ALINIT with array slice)
      DELTAP(0 : ISTART - 1) = ZERO

      DO IEL = 1, ISTART - 1
         OK (IEL) = .TRUE.
      END DO

      DO IEL = ISTART, total_no_elements
         OK (IEL) = .FALSE.
      END DO

      ! start of main iteration loop
      !______________________________*
      ELEVEL = 0
      g670 = .FALSE.

      DO NIT = 1, NITMAX

         IF (NIT == NITMAX) ELEVEL = EEERR
         DPSIMX = ZERO

         DO I = 1, total_no_elements
            IEL = ISORT (I)

            IF (OK (IEL)) CYCLE

            ICBOT = NLYRBT (IEL, 1)
            ITYPE = ICMREF (IEL, 1)

            NCELL = ICTOP - ICBOT + 1

            ! save psi at iteration level m
            CALL DCOPY (NCELL, VSPSI (ICBOT, IEL), 1, PSIM (ICBOT), 1)

            ! set up column arrays using global arrays
            DO ILYR = 1, NLYR (IEL) + 1
               ICLYRB (ILYR) = NLYRBT (IEL, ILYR)
            END DO

            IF (ITYPE == 1 .OR. ITYPE == 2) ICBED = NHBED (ICMREF (IEL, 4), ITYPE)

            DO IFA = 1, 4
               CDELL (IFA) = DHF (IEL, IFA)
               JEL = ICMREF (IEL, IFA + 4)
               JELDUM (IFA) = JEL

               IF (JEL < 1) THEN
                  DXYDUM = ZERO
               ELSE
                  CZS (IFA) = GETHRF (JEL)

                  ! !!!!! fix for channel aquifer flows, SPA, 03/11/98
                  ! Pass depth of water in adjacent elements to vscolm
                  ! as well as elevation of water surface
                  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  depadj (IFA) = cdnet (JEL)
                  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  JFA = ICMREF (IEL, IFA + 8)
                  DXYDUM = DHF (JEL, JFA)
               END IF

               CDELL1 (IFA) = DXYDUM

               IF (JEL < ISTART) CYCLE

               ! NB: VSPSI, VSKR may hold values from previous iteration
               K = MOD (JFA - 1, 2) + 1
               DO JCL = NLYRBT (JEL, 1), top_cell_no
                  JCDEL1 (JCL, IFA) = JVSDEL (JFA, JCL, JEL)
                  CAIJ1 (JCL, IFA) = VSAIJsv (JFA, JCL, JEL)
                  CZ1 (JCL, IFA) = ZVSNOD (JCL, JEL)
                  CPSI1 (JCL, IFA) = VSPSI (JCL, JEL)
                  CPSIN1 (JCL, IFA) = VSPSIN (JCL, JEL)
                  N = ICSOILsv (JCL, JEL)
                  CKIJ1 (JCL, IFA) = VSKR (JCL, JEL) * VSK3D (N, K)
               END DO

            END DO

            ! boundary condition indices
            IW = MAX (1, NVSWLI (IEL))
            ICWLBT = NWELBT (IEL)
            ICWCAT = NVSWLC (IEL)
            ICLBCT = NLBCAT (IEL)
            ICBBCT = NBBCAT (IEL)

            ! calculate new potentials and flow rates
            CALL VSCOLM (NSEE, VSWV, VSWL, VSK3D, BHELEV, ELEVEL, IEL, ICBOT, ICTOP, ICBED,       &
                         ICLYRB, ICSOILsv (ICBOT, IEL), JCBCsv (0, IEL), JCDEL1, JELDUM,          &
                         JVSACN (1, ICBOT, IEL), JVSDEL (1, ICBOT, IEL), NVSSPC (IEL),            &
                         NVSLFN (ICLBCT), NVSLFL (1, ICLBCT), NWELBT (IEL), NVSLHN (ICLBCT),      &
                         NVSLHL (1, ICLBCT), NWELTP (IEL), NVSLGN (ICLBCT), NVSLGL (1, ICLBCT),   &
                         cellarea (IEL), ZGRUND (IEL), VSSPZ (IEL), VSSPCO (IEL),                 &
                         DELTAZ (ICBOT, IEL), ZVSNOD (ICBOT, IEL), CDELL, VSAIJsv (1, ICBOT, IEL),&
                         CAIJ1, CDELL1, CZ1, DTUZ, CDNET (IEL), VSPSIN (ICBOT, IEL),              &
                         CQ (ICBOT, IEL), CZS, CPSI1, CPSIN1, CKIJ1, WLNOW (ICWCAT),              &
                         RLFNOW (1, ICLBCT), RLHNOW (1, ICLBCT), RLGNOW (1, ICLBCT),              &
                         RBFNOW (ICBBCT), RBHNOW (ICBBCT), IVSSTO (ICBOT, IEL),                   &
                         VSPSI (ICBOT, IEL), VSKR (ICBOT, IEL), VSTHE (ICBOT, IEL),               &
                         QVSH (1, ICBOT, IEL), QVSV (ICBOT - 1, IEL), QVSWLI (ICWLBT, IW),        &
                         QVSSPR (IEL), ZVSPSL (IEL), depadj)

            !!!!!! extra argument depadj added for channel-aquifer flows fix
            ! SPA, 03/11/98

            ! record largest change for this iteration
            DPSIEL = ZERO
            DO ICL = ICBOT, ICTOP
               DPSIEL = MAX (DPSIEL, ABS (VSPSI (ICL, IEL) - PSIM (ICL)))
            END DO

            DELTAP (IEL) = DPSIEL
            DPSIMX = MAX (DPSIMX, DPSIEL)

         ! end of element loop: check for convergence or maximum iterations
         END DO

         ! 970214  At present the criterion on DPSIMX overrides that on NIT
         IF (DPSIMX <= GEPSMX) THEN
            g670 = .TRUE.
            EXIT
         END IF

         IF (NIT >= NITMIN) THEN
            DO IEL = ISTART, total_no_elements
               DPSIEL = DELTAP (IEL)
               DO IFA = 1, 4
                  JEL = MAX (0, ICMREF (IEL, IFA + 4))
                  DPSIEL = MAX (DPSIEL, DELTAP (JEL))
               END DO
               OK (IEL) = DPSIEL < GEPSMX
            END DO
         END IF

      ! end of iteration loop
      END DO

      IF (.NOT. g670) THEN
         errorcount2 = errorcount2 + 1
         IF (errorcount2 < errcntallowed) THEN
            CALL ERROR(EEERR, 1039, PPPRI, 0, 0, 'Maximum iterations in VSS global solver')
         ELSE IF (errorcount2 == errcntallowed) THEN
            CALL ERROR (EEERR, 1039, PPPRI, 0, 0, '**** Last printout of the error message - maximum iterations in VSS global solver *****')
         END IF
      END IF

      ! main solution is complete: tidy up
      !____________________________________*
      ! update flows to ensure mass conservation

      CALL VSMB (VSTHEN)

      ! set auxiliary output arrays
      DO IEL = ISTART, total_no_elements
         ICBOT = NLYRBT (IEL, 1)
         QVSBF (IEL) = QVSV (ICBOT - 1, IEL)
         QH (IEL) = QVSV (ICTOP, IEL)
         IW = NVSWLI (IEL)

         IF (IW < 1) CYCLE

         CQW = ZERO
         DO ICL = NWELBT (IEL), NWELTP (IEL)
            CQW = QVSWLI (ICL, IW) + CQW
         END DO

         QVSWEL (IEL) = CQW
      END DO

      ! calculate QBKB, QBKF, QBKI for all cases:
      !    bank elements or not, including dry channels
      DO IBK = 1, 2

         DO IEL = 1, total_no_links
            QI = -HALF * cellarea (IEL) * QH (IEL)
            WET = NINT (HALF + SIGN (HALF, GETHRF (IEL) - ZGRUND (IEL) - DRYH))
            IFA = 2 * IBK

            IF (LINKNS (IEL)) IFA = IFA - 1
            JEL = ICMREF (IEL, IFA + 4)
            JFA = ICMREF (IEL, IFA + 8)

            JCBED = top_cell_no
            IF (JEL > 0) JCBED = NLYRBT (JEL, 1) - 1
            IF (BEXBK) JCBED = NHBED (IEL, IBK)

            QBK = ZERO
            DO JCL = JCBED + 1, top_cell_no
               QBK = QBK + QVSH (JFA, JCL, JEL)
            END DO

            ! !!! mod.s to make definition of exchange flows consistent with balwat
            ! SPA, 04/11/98
            QBKF (IEL, IBK) = QBK
            QBKB (IEL, IBK) = QI * IBANK * WET
            QBKI (IEL, IBK) = QI * IBANK * (1 - WET)
         END DO

      END DO

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

      ! Assumed external module dependencies providing global variables:
      ! NSEE, NSOLEE, BFAST, NVSSOL, VSPPSI, NS, IVSFLG, VSPOR, VSTRES,
      ! VSALPH, VSVGN, VSPTHE, VSPDTH, VSPKR, VSPDKR, VSPETA, VSPDET, VSPSS,
      ! TBPSI, TBTHE, TBTHEC, TBKR, TBKRC, BSOILP, PPPRI, zero, one, two, three

      IMPLICIT NONE

      ! Locals
      INTEGER :: I, IS, NDUM
      INTEGER :: NTBPOS(NSEE) = 1
      DOUBLE PRECISION :: RVSSOL, PSI, DDDUM
      DOUBLE PRECISION :: DDTSAT, DDTRES, DDA, DDN, DDM, DD1M1, DDTSMR
      DOUBLE PRECISION :: DDAP, DDAPN, DDAPN1, DDAPM, DDAPM1, DDAPM2, DDTCAP
      DOUBLE PRECISION :: DDTC, DDTCM, DDTCM1, DDTCM2, DDDTCP
      DOUBLE PRECISION :: PLOG, PLOGLO, PLOGHI, ADUM, BDUM, HDUM, RKRDUM

   !----------------------------------------------------------------------*
   ! soil flags:
   !       1       van Genuchten
   !       2       tabulated theta(psi) and Kr(psi)
   !       3       exponential
   !       4       tabulated theta(psi), Averjanov Kr (compatible with V3.4
   !----------------------------------------------------------------------*

      ! set up size of internal look-up tables
      IF (BFAST) THEN
         NVSSOL = MIN (100, NSOLEE)
      ELSE
         NVSSOL = MIN (500, NSOLEE)
      END IF

      RVSSOL = DBLE(NVSSOL)

      ! loop over NVSSOL divisions of the soil property tables
      ! (NB. low values of I correspond to wet soils)
      ! psi ranges from -(10**-2) to -(10**4)
      psi_loop: DO I = 5, NVSSOL - 1

         PSI = -(10.0D0**(-two + 6.0D0 * DBLE(I - 5) / RVSSOL))
         VSPPSI(I) = PSI

         ! set up property data for each soil type
         soil_loop: DO IS = 1, NS

            ! ... 1 (Van Genuchten)
            IF (IVSFLG(IS) == 1) THEN
               DDTSAT = VSPOR(IS)
               DDTRES = VSTRES(IS)
               DDA = VSALPH(IS) * 100.0D0
               DDN = VSVGN(IS)
               DDM = one - (one / DDN)
               DD1M1 = (one / DDM) - one
               DDTSMR = DDTSAT - DDTRES
               DDAP = -DDA * PSI
               DDAPN = DDAP**DDN
               DDAPN1 = DDAP**(DDN - one)
               DDAPM = (one + DDAPN)**DDM
               DDAPM1 = (one + DDAPN)**(DDM + one)
               DDAPM2 = (one + DDAPN)**(DDM + two)
               DDDTCP = DDA * DDM * DDN * DDAPN1 / DDAPM1

               VSPTHE(I, IS) = DDTRES + DDTSMR / DDAPM
               VSPDTH(I, IS) = DDTSMR * DDDTCP

               DDTCAP = MAX(1.0D-10, (VSPTHE(I, IS) - DDTRES) / DDTSMR)
               DDTC = one - (DDTCAP**(one / DDM))
               DDTCM = DDTC**DDM
               DDTCM1 = DDTC**(DDM - one)
               DDTCM2 = (one - DDTCM)**two

               VSPKR(I, IS) = SQRT(DDTCAP) * DDTCM2

               ! Commented out legacy derivative code maintained for reference
               ! VSPDKR(I,IS) = DSQRT(DDTCAP)*(one-DDTCM)*
               !  (half*(one-DDTCM)/DDTCAP + two*DDTCM1*DDTCAP**DD1M1) * DDDTCP

               DDDUM = (DDA * DDA * DDM * DDN * DDTSMR * DDAPN1 / DDAPM2) * &
                       ((DDN - one) * (one + DDAPN) + (DDM + one) * DDN * DDAPN1)
               VSPETA(I, IS) = VSPTHE(I, IS) * VSPSS(IS) / VSPOR(IS) + VSPDTH(I, IS)

               ! VSPDET(I,IS) = VSPDTH(I,IS)*VSPSS(IS)/VSPOR(IS) + DDDUM
               VSPDET(I, IS) = zero

            ! ... 2 (tabulated theta and Kr)
            ELSE IF (IVSFLG(IS) == 2) THEN

               ! check for correct location in input table
               ! Safely bounds check using DO WHILE instead of simple IF
               DO WHILE (PSI < TBPSI(NTBPOS(IS) + 1, IS))
                  NTBPOS(IS) = NTBPOS(IS) + 1
               END DO

               NDUM = NTBPOS(IS)

               ! evaluate cubic spline polynomial for theta and Kr
               PLOG = LOG10(-PSI)
               PLOGHI = LOG10(-TBPSI(NDUM + 1, IS))
               PLOGLO = LOG10(-TBPSI(NDUM, IS))
               HDUM = PLOGHI - PLOGLO
               ADUM = (PLOGHI - PLOG) / HDUM
               BDUM = (PLOG - PLOGLO) / HDUM

               VSPTHE(I, IS) = ADUM * TBTHE(NDUM, IS) + BDUM * TBTHE(NDUM + 1, IS) + &
                               ((ADUM**three - ADUM) * TBTHEC(NDUM, IS) + &
                               (BDUM**three - BDUM) * TBTHEC(NDUM + 1, IS)) * &
                               (HDUM**two) / 6.0D0

               VSPTHE(I, IS) = VSPOR(IS) * VSPTHE(I, IS)

               VSPKR(I, IS) = ADUM * TBKR(NDUM, IS) + BDUM * TBKR(NDUM + 1, IS) + &
                              ((ADUM**three - ADUM) * TBKRC(NDUM, IS) + &
                              (BDUM**three - BDUM) * TBKRC(NDUM + 1, IS)) * &
                              (HDUM**two) / 6.0D0

            ! ... 3 (exponential)
            ELSE IF (IVSFLG(IS) == 3) THEN

               ! Replaced EDUM**(VSALPH * PSI) hack with precise EXP intrinsic
               DDDUM = EXP(VSALPH(IS) * PSI)
               VSPTHE(I, IS) = VSTRES(IS) + (VSPOR(IS) - VSTRES(IS)) * DDDUM
               VSPDTH(I, IS) = (VSPOR(IS) - VSTRES(IS)) * VSALPH(IS) * DDDUM

               VSPKR(I, IS)  = DDDUM
               VSPDKR(I, IS) = VSALPH(IS) * DDDUM

               VSPETA(I, IS) = VSPTHE(I, IS) * VSPSS(IS) / VSPOR(IS) + VSPDTH(I, IS)
               VSPDET(I, IS) = VSPDTH(I, IS) * VSPSS(IS) / VSPOR(IS) + VSPDTH(I, IS) * VSALPH(IS)

            ! ... 4 (tabulated theta and Averjanov Kr)
            ELSE IF (IVSFLG(IS) == 4) THEN
               STOP 'UNFINISHED code for soil properties type 4'
            END IF

         END DO soil_loop
      END DO psi_loop

      ! set up property data for extreme dry conditions
      VSPPSI(NVSSOL) = -1.0D6
      DO IS = 1, NS
         VSPTHE(NVSSOL, IS) = VSTRES(IS)
         VSPKR(NVSSOL, IS)  = zero
         VSPETA(NVSSOL, IS) = zero
         VSPDTH(NVSSOL, IS) = zero
         VSPDKR(NVSSOL, IS) = zero
         VSPDET(NVSSOL, IS) = zero
      END DO

      ! set up storage term for tabulated data
      DO I = 5, NVSSOL - 1
         DO IS = 1, NS
            IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
               VSPDTH(I, IS) = (VSPTHE(I + 1, IS) - VSPTHE(I, IS)) / (VSPPSI(I + 1) - VSPPSI(I))
               VSPETA(I, IS) = VSPTHE(I, IS) * VSPSS(IS) / VSPOR(IS) + VSPDTH(I, IS)
            END IF
         END DO
      END DO

      DO I = 5, NVSSOL - 1
         DO IS = 1, NS
            IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
               VSPDET(I, IS) = VSPDTH(I, IS) * VSPSS(IS) / VSPOR(IS) + &
                               (VSPDTH(I + 1, IS) - VSPDTH(I, IS)) / (VSPPSI(I + 1) - VSPPSI(I))
            END IF
         END DO
      END DO

      ! set up property data for extreme wet conditions
      VSPPSI(4) = zero
      VSPPSI(3) = 2.5D-1
      VSPPSI(2) = 5.0D-1
      VSPPSI(1) = 1.0D6

      wet_conditions_loop: DO IS = 1, NS

         ! Converted line-by-line assignments into high-performance array slices
         VSPKR(1:4, IS) = one
         VSPETA(3:4, IS) = VSPETA(5, IS)
         VSPETA(1:2, IS) = VSPSS(IS)
         VSPDTH(4, IS) = VSPDTH(5, IS)

         VSPTHE(4, IS) = VSPOR(IS)
         VSPTHE(3, IS) = VSPTHE(4, IS) + VSPETA(4, IS) * (VSPPSI(3) - VSPPSI(4))
         VSPTHE(2, IS) = VSPTHE(3, IS) + VSPETA(3, IS) * (VSPPSI(2) - VSPPSI(3))
         VSPTHE(1, IS) = VSPTHE(2, IS) + VSPSS(IS) * (VSPPSI(1) - VSPPSI(2))

         VSPDTH(1:3, IS) = zero
         VSPDKR(4, IS) = VSPDKR(5, IS)
         VSPDKR(1:3, IS) = zero
         VSPDET(1:4, IS) = zero

      END DO wet_conditions_loop

      ! DSATG-specific code - adjust relative conductivity curves so that
      ! Kr approaches unity at saturation (for values of VG-n less than 2,
      ! the value of Kr drops rapidly and unphysically less than one near satu...)
      dsatg_loop: DO IS = 1, NS
         RKRDUM = VSPOR(IS) - VSTRES(IS)
         ! Replace inner loop with high-performance array operation
         VSPKR(5:NVSSOL, IS) = ((VSPTHE(5:NVSSOL, IS) - VSTRES(IS)) / RKRDUM)**two
      END DO dsatg_loop

      ! write soil property tables to PRI file
      IF (BSOILP) THEN
         WRITE(PPPRI, 905) NS, NVSSOL
         DO IS = 1, NS
            WRITE(PPPRI, 910) IS
            DO I = 1, NVSSOL
               WRITE(PPPRI, 920) I, VSPPSI(I), VSPTHE(I, IS), VSPETA(I, IS), VSPKR(I, IS), &
                                 VSPDTH(I, IS), VSPDET(I, IS), VSPDKR(I, IS)
            END DO
         END DO
      END IF

      RETURN

      ! FORMAT STATEMENTS
905   FORMAT(/ 'VSS physical soil/lithology property data' / &
             '=========================================' / &
             I3, ' soils' / &
             I3, ' values in soil property tables' )

910   FORMAT(/ &
             3X,'  Soil property tables for soil/lithology type: ',I3 / &
             3X,'  -------------------------------------------------' // &
             3X,'      psi         theta          eta            Kr      ', &
             ' d(the)/d(psi) d(eta)/d(psi)  d(Kr)/d(psi)' / &
             3X,'   (VSPPSI)      (VSPTHE)      (VSPETA)       (VSPKR)   ', &
             '   (VSPDTH)      (VSPDET)       (VSPDKR)  ' / &
             3X,'  ------------  ------------  ------------  ------------', &
             '  ------------  ------------  ------------' )

920   FORMAT(I3,7(2X,G14.6))

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



   !SSSSSS SUBROUTINE VSWELL
   PURE SUBROUTINE VSWELL(NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL, CA0, &
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

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NSEE, ICWLBT, ICWLTP
      INTEGER, INTENT(IN) :: ICSOIL(ICWLBT:ICWLTP)
      DOUBLE PRECISION, INTENT(IN) :: CA0, CQWIN
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICWLBT:ICWLTP + 1)
      DOUBLE PRECISION, INTENT(IN) :: VSK3D(NSEE, 2)
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICWLBT:ICWLTP)

      ! In+out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: CR(ICWLBT:ICWLTP)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT)   :: CQWI(ICWLBT:ICWLTP)

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: RKZDUM(ICWLBT:ICWLTP)

      ! Locals
      INTEGER :: ICL, SOIL
      DOUBLE PRECISION :: RKZTOT, DZDUM, PDUM, QDUM, RKZ

   !----------------------------------------------------------------------*

      ! The value of CQWIN is the prescribed abstraction rate (m3/s).
      ! The actual abstraction rate CQWI (m/s) may be less than this if some
      ! of the aquifer around the well screen becomes unsaturated
      ! (ie if CPSI(ICL) < DZDUM below).

      ! Calculate product of mean lateral hydraulic conductivity & cell depth
      ! Kept as scalar DO loop to maximize performance on small cell slices
      RKZTOT = ZERO

      rkz_loop: DO ICL = ICWLBT, ICWLTP
         SOIL = ICSOIL(ICL)
         RKZ = HALF * (VSK3D(SOIL, 1) + VSK3D(SOIL, 2)) * CDELZ(ICL)
         RKZDUM(ICL) = RKZ
         RKZTOT = RKZ + RKZTOT
      END DO rkz_loop

      ! Calculate flow into well for each cell, & add into matrix coefficients
      well_flow_loop: DO ICL = ICWLBT, ICWLTP
         DZDUM = HALF * (CDELZ(ICL) + CDELZ(ICL + 1))
         PDUM  = MIN(DZDUM, MAX(CPSI(ICL), ZERO))

         QDUM = CQWIN * (RKZDUM(ICL) / RKZTOT) * (PDUM / DZDUM)
         CQWI(ICL) = QDUM / CA0

         CR(ICL) = QDUM + CR(ICL)
      END DO well_flow_loop

   END SUBROUTINE VSWELL

END MODULE VSmod
