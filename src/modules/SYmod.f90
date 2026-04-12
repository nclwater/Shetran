MODULE SYmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the SY .F files
   USE SGLOBAL
!USE AL_P
   USE mod_load_filedata, ONLY : ALCHKI, ALCHK, ALALLF, ALREAD  !, HELPPATH
   USE UTILSMOD, ONLY : DCOPY
   USE CONST_SY

   IMPLICIT NONE

   LOGICAL         :: FIRST_syackw=.TRUE.

   LOGICAL         :: FIRST_syfine=.TRUE.
   DOUBLEPRECISION :: WSED_syfine


   INTEGER, PARAMETER  :: NSYBEE= 40, NSYCEE=10
   INTEGER          :: ISACKW_symain, ISGSED_symain, ISSYOK_symain, ISTEC_symain, ISUSED_symain, NEPS_symain
   INTEGER          :: NFINE_symain, NSYB_symain
   INTEGER          :: NSYBCD_symain(NSYBEE,3), NSYC_symain(4), NTSOBK_symain(NLFEE), PASS_symain=0, NTSOTP_symain(NELEE)
   DOUBLEPRECISION  :: ALPHA_symain, CONCOB_symain, DCBEDO_symain, FBIC_symain, FICRIT_symain, FPCRIT_symain, SYNOW_symain
   DOUBLEPRECISION  :: DLSMAX_symain, DDBSED_symain(NLFEE, NSEDEE)
   DOUBLEPRECISION  :: ABC_symain(NSEDEE, NSYCEE), ACKW_symain(5, NSEDEE), ARXLOL_symain(NLFEE), BBC_symain(NSEDEE, NSYCEE)
   DOUBLEPRECISION  :: BKB_symain(NSEE), DBFULL_symain(NLFEE)
   DOUBLEPRECISION  :: DRDRIP_symain(NVEE), DRSED_symain(NSEDEE), DRSO50_symain(NSEE), DWATOL_symain(NELEE), FCG_symain(NELEE)
   DOUBLEPRECISION  :: FCROCK_symain(NELEE), FDRIP_symain(NVEE), FETA_symain(NELEE), FPCLAY_symain(NSEE)
   DOUBLEPRECISION  :: GBC_symain(NSEDEE, NSYCEE), GKF_symain(NSEE), GKR_symain(NSEE), RHOSO_symain(NSEE), XDRIP_symain(NVEE)

   DOUBLE PRECISION, PARAMETER :: K1_syovtr = 0.05D0 * RHOWAT**2 / ((RHOSED - RHOWAT)**2 * SQRT(GRAVTY))
   DOUBLE PRECISION, PARAMETER :: K3_syovtr = 2.45D0 * (RHOSED / RHOWAT)**(-0.4D0) / SQRT((RHOSED - RHOWAT) * GRAVTY)
   DOUBLE PRECISION, PARAMETER :: K4_syovtr = 0.635D0 / SQRT(RHOWAT)

   PRIVATE

   PUBLIC :: SYMAIN, issyok_symain

CONTAINS


   !SSSSSS SUBROUTINE SYACKW
   PURE SUBROUTINE SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, DRSED, ARXL, DCBSED, DWAT1, &
                           QOC, TAUJ, ACKW, GSED)

      IMPLICIT NONE

      ! Commons and distributed constants
      ! Constants referenced
      !  CONST.SY: GRAVTY, RHOSED, RHOWAT, VISCOS
      !  (Assumed from CONST_SY: ONE, ZERO, FIRST_syackw, K2_syackw, DGRMAX_syackw, ROOT32_syackw)

      ! Input arguments
      INTEGER, INTENT(IN)          :: ISACKW, NFINE, NLF, NLFEE, NELEE, NSED
      LOGICAL, INTENT(IN)          :: LINKNS (NLF)
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NFINE + 1:NSED), ARXL (NLF)
      DOUBLE PRECISION, INTENT(IN) :: DWAT1 (NLF), QOC (NELEE, 4)
      DOUBLE PRECISION, INTENT(IN) :: DCBSED (NLFEE, NFINE + 1:NSED), TAUJ (NELEE, 4)

      ! In/Out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: ACKW (5, NFINE + 1:NSED)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT)   :: GSED (NLF, NFINE + 1:NSED)

      ! Locals
      DOUBLE PRECISION, PARAMETER :: DGRSML = 1.0D-4
      DOUBLE PRECISION, PARAMETER :: F16 = 0.16D0, F50 = 0.5D0, F56 = 0.56D0, F84 = 0.84D0
      DOUBLE PRECISION, PARAMETER :: THIRD = 1.0D0 / 3.0D0
      
      DOUBLE PRECISION, PARAMETER :: KRHO = RHOSED / RHOWAT - 1.0D0
      DOUBLE PRECISION, PARAMETER :: K2_syackw = (GRAVTY * KRHO / VISCOS**2)**THIRD
      DOUBLE PRECISION, PARAMETER :: DGRMAX_syackw = 10.0D0**(ONE / F56) + DGRSML
      DOUBLE PRECISION, PARAMETER :: ROOT32_syackw = SQRT(32.0D0)
      DOUBLE PRECISION :: AAW, ARXLE, CAW, DAAA, DBED16, DBED50, DBED84, DGR
      DOUBLE PRECISION :: DSED, DWAT1E, FGR, G, H10, LGR, MAW
      DOUBLE PRECISION :: NAW, QK, UGR, USTR, UK, BASE
      INTEGER          :: FACE, IEND, LINK, NFP1, NNF, SED, SGN

      !----------------------------------------------------------------------*

      ! Initialization
      NNF = NSED - NFINE
      NFP1 = NFINE + 1

      DO SED = NFP1, NSED
         DGR = FDGR (DRSED (SED))
         LGR = LOG10 (DGR)
         ACKW (1, SED) = MAX (ZERO, ONE - F56 * LGR) ! Replaced DIMJE with standard intrinsic
         IF (ISACKW == 1) ACKW (2, SED) = FA (DGR)
         ACKW (3, SED) = 1.34D0 + 9.66D0 / DGR
         ACKW (4, SED) = 10.0D0**( (2.86D0 - LGR) * LGR - 3.53D0)
         ACKW (5, SED) = ONE / SQRT (GRAVTY * KRHO * DRSED (SED))
      END DO

      ! Zero GSED array slice
      GSED(:, :) = ZERO

      ! Loop over ends of each link
      DO IEND = 1, 3, 2
         SGN = 2 - IEND

         ! Loop over all channel links
         DO LINK = 1, NLF

            ! Determine face equivalent to this end, and flow rate there
            FACE = IEND
            IF (LINKNS (LINK)) FACE = FACE + 1
            QK = SGN * QOC (LINK, FACE)

            ! Check that this end is outflowing
            IF (QK > ZERO) THEN

               ! Copy array elements to local variables
               ARXLE = ARXL (LINK)
               DWAT1E = DWAT1 (LINK)
               H10 = 10.0D0 * DWAT1E

               ! Determine shear velocity and water flow velocity
               USTR = SQRT (TAUJ (LINK, FACE) / RHOWAT)
               UK = ZERO
               IF (ARXLE > ZERO) UK = QK / ARXLE

               ! Set A-W parameters for the Day modification if needed
               IF (ISACKW == 2) THEN

                  DBED84 = SYDR (F84, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))

                  IF (DBED84 > ZERO) THEN
                     DBED50 = SYDR (F50, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))
                     DBED16 = SYDR (F16, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))
                     DAAA = 1.62D0 * DBED50 * (DBED16 / DBED84)**0.28D0
                  ELSE
                     DAAA = ZERO
                  END IF

                  DGR = FDGR (DAAA)
                  AAW = FA (DGR)
                  
                  DO SED = NFP1, NSED
                     ACKW (2, SED) = AAW * (0.6D0 + 0.4D0 * SQRT (DAAA / DRSED (SED)))
                  END DO

               END IF

               ! Loop over sediment types
               DO SED = NFP1, NSED

                  ! Set A-W parameters for this Sediment size group
                  NAW = ACKW (1, SED)
                  AAW = ACKW (2, SED)
                  MAW = ACKW (3, SED)
                  CAW = ACKW (4, SED)
                  DSED = DRSED (SED)

                  ! Calculate particle mobility
                  UGR = ZERO
                  IF (DSED < H10) UGR = UK / (ROOT32_syackw * LOG10 (H10 / DSED))
                  FGR = ACKW (5, SED)
                  IF (NAW > ZERO) FGR = FGR * USTR**NAW
                  IF (NAW < ONE) FGR = FGR * UGR**(ONE - NAW)

                  ! Determine discharge capacity for this end
                  ! High-Performance Fix: Do not perform exponentiation (0.0**MAW) if base is zero or less.
                  IF (DWAT1E > ZERO) THEN
                     BASE = (FGR / AAW) - ONE
                     IF (BASE > ZERO) THEN
                        G = DSED * (QK / DWAT1E) * CAW * (BASE**MAW)
                        IF (NAW > ZERO) G = G * (UK / USTR)**NAW
                        
                        ! Determine the total discharge capacity of both ends
                        GSED (LINK, SED) = GSED (LINK, SED) + G
                     END IF
                  END IF

               END DO

            END IF

         END DO
      END DO

   CONTAINS

      ! ----------------------------------------------------------------------*
      ! Internal Functions
      ! ----------------------------------------------------------------------*

      ELEMENTAL FUNCTION FDGR(DUM_VAL) RESULT(RES)
         DOUBLE PRECISION, INTENT(IN) :: DUM_VAL
         DOUBLE PRECISION :: RES
         RES = MAX (ONE, MIN (K2_syackw * DUM_VAL, DGRMAX_syackw))
      END FUNCTION FDGR

      ELEMENTAL FUNCTION FA(DUM_VAL) RESULT(RES)
         DOUBLE PRECISION, INTENT(IN) :: DUM_VAL
         DOUBLE PRECISION :: RES
         RES = 0.14D0 + 0.23D0 / SQRT (DUM_VAL)
      END FUNCTION FA

   END SUBROUTINE SYACKW



!SSSSSS SUBROUTINE SYBC
   SUBROUTINE SYBC
!!!!STOP ' FATAL ERROR!!  Sediment boundary flows not yet implemented'
   END SUBROUTINE SYBC



   !SSSSSS SUBROUTINE SYBED
   !----------------------------------------------------------------------*
   ! Update stream-bed state variables for each link.
   !----------------------------------------------------------------------*
   PURE SUBROUTINE SYBED(DCBEDO, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, &
                         DDIPRM, ARBDEP, DLS, FBETA, DCBSED, DDBSED, DCBED)
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR65
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  23.5.94  Version 3.4.1 by AB/RAH. File creation date 5.4.94.
   !----------------------------------------------------------------------*
   
      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NELEE, NLF, NLFEE, NSED
      DOUBLE PRECISION, INTENT(IN) :: DCBEDO
      DOUBLE PRECISION, INTENT(IN) :: CWIDTH(NLF)
      DOUBLE PRECISION, INTENT(IN) :: DCIPRM(NLFEE, NSED), DDIPRM(NLFEE, NSED)

      ! Input/output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: ARBDEP(NLF), DLS(NLF)
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA(NELEE, NSED)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: DCBSED(NLFEE, NSED), DDBSED(NLFEE, NSED)
      DOUBLE PRECISION, INTENT(OUT) :: DCBED(NLF)

      ! Locals, etc
      INTEGER :: LINK, SED
      DOUBLE PRECISION :: AC, AD, DCBEDZ, DCC, DCNEW, DDBEDZ, DLSNEW, DLSOLD
      DOUBLE PRECISION :: DCIPP, DDIPP, DCINEW, SUMSED

   !----------------------------------------------------------------------*

      ! * Loop over links
      link_loop: DO LINK = 1, NLF

         ! * Calculate interim bed layer thicknesses
         DCBEDZ = 0.0D0
         DDBEDZ = 0.0D0
         
         sum_loop: DO SED = 1, NSED
            DCBEDZ = DCBEDZ + DCIPRM(LINK, SED)
            DDBEDZ = DDBEDZ + DDIPRM(LINK, SED)
         END DO sum_loop

         ! * Reset variables that are independent of size group
         DLSOLD = DLS(LINK)
         DLSNEW = DCBEDZ + DDBEDZ
         DLS(LINK) = DLSNEW
         
         ARBDEP(LINK) = ARBDEP(LINK) + CWIDTH(LINK) * (DLSNEW - DLSOLD)
         DCNEW = MIN(DLSNEW, DCBEDO)
         DCBED(LINK) = DCNEW

         ! * What fraction of the interim top layer remains in the top
         ! * layer, and what fraction of the interim bottom layer becomes
         ! * part of the top?
         DCC = MIN(DCBEDZ, DCNEW)
         AC = 0.0D0
         AD = 0.0D0
         
         IF (DCBEDZ > 0.0D0) AC = DCC / DCBEDZ
         IF (DDBEDZ > 0.0D0) AD = (DCNEW - DCC) / DDBEDZ

         ! * Loop over sediment size groups
         sed_loop: DO SED = 1, NSED

            ! * Interim layer depths
            DCIPP = DCIPRM(LINK, SED)
            DDIPP = DDIPRM(LINK, SED)

            ! * Total depth (for this size group)
            SUMSED = DCIPP + DDIPP

            ! * New top layer depth
            DCINEW = AC * DCIPP + AD * DDIPP
            DCBSED(LINK, SED) = DCINEW

            ! * New bottom layer depth
            DDBSED(LINK, SED) = SUMSED - DCINEW

            ! * Composition of both layers together
            IF (DLSNEW > 0.0D0) FBETA(LINK, SED) = (SUMSED / DLSNEW)

         END DO sed_loop

      END DO link_loop

   END SUBROUTINE SYBED



   !SSSSSS SUBROUTINE SYBKER
   !----------------------------------------------------------------------*
   ! Calculate the rate of lateral erosion of stream banks for each link.
   !----------------------------------------------------------------------*
   PURE SUBROUTINE SYBKER(ISTEC, NLF, NS, FPCLAY, RHOSO, DRSO50, TAUK, &
                          CWIDTH, DWAT1, BKB, NTSOBK, FETA, CLENTH, DBFULL, EPSB, GNUBK)
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR66
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  16.06.94  Version 3.4.1 by AB/RAH. File creation date 28.3.94.
   !----------------------------------------------------------------------*
   
      ! Assumed external module dependencies providing global variables/functions:
      ! USE UTILSMOD, ONLY : DIMJE
      ! USE OC_MODULE, ONLY : ONE, TWO

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISTEC, NLF, NS
      INTEGER, INTENT(IN) :: NTSOBK(NLF)
      DOUBLE PRECISION, INTENT(IN) :: FPCLAY(NS), RHOSO(NS), DRSO50(NS), BKB(NS)
      DOUBLE PRECISION, INTENT(IN) :: TAUK(NLF), CWIDTH(NLF), DWAT1(NLF)
      DOUBLE PRECISION, INTENT(IN) :: FETA(NLF), CLENTH(NLF), DBFULL(NLF)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: EPSB(NLF), GNUBK(NLF)

      ! Locals, etc
      DOUBLE PRECISION, PARAMETER :: A1 = 0.05D0, B1 = 0.41D0, B2 = 0.22D0, B3 = 0.035D0
      DOUBLE PRECISION, PARAMETER :: QUART = 1.0D0 / 4.0D0
      
      INTEGER :: BKSOIL, LINK
      DOUBLE PRECISION :: DWAT1E, GNUBKE, K, TAUEC, TAUKE, X

   !----------------------------------------------------------------------*

      ! * Loop over channel links
      link_loop: DO LINK = 1, NLF
         
         BKSOIL = NTSOBK(LINK)
         DWAT1E = DWAT1(LINK)
         TAUKE  = TAUK(LINK)

         ! * Calculate aspect ratio coefficient ( see Notes )
         X = ONE / MAX(QUART, DWAT1E / CWIDTH(LINK))
         K = A1 + B1 * MIN(X, ONE) + B2 * MIN(DIMJE(X, ONE), ONE) &
             + B3 * DIMJE(X, TWO)

         ! * Obtain critical shear stress for bank erosion
         CALL SYCRIT(ISTEC, DRSO50(BKSOIL), TAUKE, FPCLAY(BKSOIL), TAUEC)

         ! * Calculate bank erosion rate
         GNUBKE = BKB(BKSOIL) * DIMJE(K * TAUKE, TAUEC) / (TAUEC * RHOSO(BKSOIL))
         GNUBK(LINK) = GNUBKE

         ! * Calculate rate of release of sediments for each link
         EPSB(LINK) = TWO * FETA(LINK) * CLENTH(LINK) * GNUBKE * &
                      MIN(DWAT1E, DBFULL(LINK))

      END DO link_loop

   END SUBROUTINE SYBKER



   !----------------------------------------------------------------------*
   !
   ! To determine the sediment transport capacity of flow in channels, and
   !  set the sediment advection coefficients.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR59
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  15.07.94  Version 3.4.1 by AB/RAH. File creation date 1.4.94.
   !  BTL  25.04.95  Version 3.4.1 correction in calculation of QSW
   !----------------------------------------------------------------------*
   SUBROUTINE SYCLTR (CONCOB, FPCRIT, ISACKW, ISUSED, NELEE, NFINE, NLF, NLFEE, NSED, NSEDEE,     &
                      DRSED, ARXL, CWIDTH, DCBED, LINKNS, DWAT1, QOC, SLOPEJ, DCBSED, FDEL, TAUJ, &
                      ACKW, CONCI, QSDWAT, GSED, QSWSUM)

      IMPLICIT NONE

      ! Commons and distributed constants
      !
      ! Constants referenced
      !   CONST.SY:  RHOWAT
      !   (Assumed from CONST_SY: FIRST_sycltr, K1_sycltr, ZERO, ONE, GTZERO, ISZERO, DIMJE)

      ! Input arguments
      INTEGER, INTENT(IN)          :: ISACKW, ISUSED, NELEE, NFINE, NLF, NLFEE, NSED, NSEDEE
      DOUBLE PRECISION, INTENT(IN) :: CONCOB, FPCRIT
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NFINE + 1:NSED), ARXL (NLF), CWIDTH (NLF), DCBED (NLF)
      DOUBLE PRECISION, INTENT(IN) :: DWAT1 (NLF), QOC (NELEE, 4), SLOPEJ (NELEE, 4)
      DOUBLE PRECISION, INTENT(IN) :: DCBSED (NLFEE, NFINE + 1:NSED), FDEL (NELEE, NFINE + 1:NSED)
      DOUBLE PRECISION, INTENT(IN) :: TAUJ (NELEE, 4)
      LOGICAL, INTENT(IN)          :: LINKNS (NLF)

      ! Input/output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: ACKW (5, NFINE + 1:NSED)

      ! Output arguments
      ! NB: QSDWAT defined for outflow faces only
      DOUBLE PRECISION, INTENT(OUT)   :: CONCI (NLFEE, NSED), QSDWAT (NLFEE, NSEDEE, 4)

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: GSED (NLF, NFINE + 1:NSED), QSWSUM (NLF, NSED)

      ! Locals, etc
      DOUBLE PRECISION, PARAMETER :: ZZ5 = 0.05D0
      DOUBLE PRECISION, PARAMETER :: k1_sycltr = 8.5D0 / SQRT (RHOWAT)

      INTEGER :: FACE, IEND, ISIDE, LINK, NFP1, NSDWAT, SED, SGN
      DOUBLE PRECISION :: CONCID, DCSUM, DUM, FDSUM, FRACT, KQ, QK
      DOUBLE PRECISION :: TAUEC, TAUD, QSW, FRACT1, FRACT2
      LOGICAL :: BODD

      ! External/Module functions implicitly referenced
      ! DOUBLE PRECISION :: DIMJE
      ! LOGICAL :: GTZERO, ISZERO

      !----------------------------------------------------------------------*
      
      NFP1 = NFINE + 1
      
      ! Replaced ALINIT with array slice operation
      QSWSUM(1:NLF, 1:NSED) = ZERO

      ! Streamwise capacity discharge rates ...
      ! ---------------------------------------
      !
      !     ... using specified method
      !
      IF (ISACKW == 1 .OR. ISACKW == 2) THEN
         CALL SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, DRSED, ARXL, DCBSED, DWAT1, &
                      QOC, TAUJ, ACKW, GSED)
      ELSE
         CALL SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, QOC, LINKNS, SLOPEJ, GSED)
      END IF


      ! Advection Coefficients (outflow faces only) Part 1 ...
      ! ------------------------------------------------------
      !
      !     ... for size groups which move with water velocity
      !
      NSDWAT = NFINE
      IF (ISUSED == 0) NSDWAT = NSED
      
      IF (NSDWAT > 0) THEN
         ! * All faces (both ends and sides)
         DO FACE = 1, 4
            SGN = SIGN (1, 2 - FACE)
            BODD = MOD (FACE, 2) == 1

            ! * All links (but skip over non-outflow faces)
            DO LINK = 1, NLF
               QK = SGN * QOC (LINK, FACE)
               IF (GTZERO(QK)) THEN

                  ! * Set QSWSUM increment for link ends only
                  QSW = ZERO
                  IF (BODD .NEQV. LINKNS (LINK)) QSW = QK

                  ! * Fines only, or all size groups, as appropriate
                  DO SED = 1, NSDWAT
                     QSDWAT (LINK, SED, FACE) = QK
                     ! * Don't actually need QSWSUM for fines, but ...
                     QSWSUM (LINK, SED) = QSWSUM (LINK, SED) + QSW
                  END DO

               END IF
            END DO
         END DO
      END IF


      ! Advection Coefficients (outflow faces only)  Part 2 ...
      ! -------------------------------------------------------
      !
      !     ... at link ends for each size group which moves with an
      !         independent velocity.
      !
      IF (ISUSED == 1) THEN

         ! * Loop over both ends ( of every link )
         DO IEND = 1, 3, 2
            SGN = 2 - IEND

            ! * Loop over every link (but skip over non-outflow faces)
            DO LINK = 1, NLF
               FACE = IEND
               IF (LINKNS (LINK)) FACE = FACE + 1
               QK = SGN * QOC (LINK, FACE)
               
               IF (GTZERO(QK)) THEN

                  TAUD = TAUJ (LINK, FACE)
                  KQ = K1_sycltr * ARXL (LINK)

                  ! * Loop over non-fine size groups
                  DO SED = NFP1, NSED
                     CALL SYCRIT (0, DRSED (SED), TAUD, DUM, TAUEC)
                     QSW = MIN (KQ * SQRT (DIMJE(TAUD, SQRT (TAUD * TAUEC))), QK)
                     QSDWAT (LINK, SED, FACE) = QSW
                     QSWSUM (LINK, SED) = QSWSUM (LINK, SED) + QSW
                  END DO

               END IF

            ! * Next link
            END DO

         ! * Next end
         END DO

      END IF


      ! Determine notional particle concentrations at flow capacity
      ! -----------------------------------------------------------
      !
      ! * Loop over fines (Replaced ALINIT loop with single slice assignment)
      ! Note: Assuming FPCRIT is properly assigned to the entire NLF dimension for each fine
      CONCI(1:NLF, 1:NFINE) = FPCRIT

      ! * Loop over links
      DO LINK = 1, NLF

         ! * Determine denominators for scaling factors
         FDSUM = ZERO
         DO SED = NFP1, NSED
            FDSUM = FDSUM + FDEL (LINK, SED)
         END DO
         IF (ISZERO(FDSUM)) FDSUM = ONE
         
         DCSUM = DCBED (LINK)
         IF (ISZERO(DCSUM)) DCSUM = ONE

         ! * Loop over non-fines
         DO SED = NFP1, NSED
            QSW = QSWSUM (LINK, SED)
            IF (GTZERO(QSW)) THEN
               FRACT1 = FDEL (LINK, SED) / FDSUM
               FRACT2 = DCBSED (LINK, SED) / DCSUM
               FRACT = MAX (ZZ5, FRACT1, FRACT2)
               CONCID = MIN (FPCRIT, FRACT * GSED (LINK, SED) / QSW)
            ELSE
               CONCID = ZERO
            END IF
            CONCI (LINK, SED) = CONCID
         END DO

      END DO


      ! Advection Coefficients (outflow faces only) Part 3 ...
      ! ------------------------------------------------------
      !
      !     ... at link sides, for each size group which moves at an
      !         independent velocity.
      !
      IF (ISUSED == 1) THEN

         ! * Loop over both sides
         DO ISIDE = 2, 4, 2
            SGN = 3 - ISIDE

            ! * Loop over every link (but skip over non-outflow sides)
            DO LINK = 1, NLF
               FACE = ISIDE
               IF (LINKNS (LINK)) FACE = ISIDE - 1
               QK = SGN * QOC (LINK, FACE)
               
               IF (GTZERO(QK)) THEN

                  ! * Loop over non-fine size groups
                  DO SED = NFP1, NSED
                     DUM = CONCI (LINK, SED)
                     IF (GTZERO(DUM)) DUM = QK * DIMJE(DUM, CONCOB) / DUM
                     QSDWAT (LINK, SED, FACE) = DUM
                  END DO

               END IF

            ! * Next link
            END DO

         ! * Next side
         END DO

      END IF

   END SUBROUTINE SYCLTR



   !----------------------------------------------------------------------*
   !
   ! Solve the transport equation for a given column element, for
   !  sediment in overland flow.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR68
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  23.5.94  Version 3.4.1 by AB/RAH. File creation date 22.11.93.
   !----------------------------------------------------------------------*
   PURE SUBROUTINE SYCOLM (AREAE, DTSY, DWAT1E, DWATOE, DXQQE, DYQQE, FETAE, GNUE, ISGSED, NSED,       &
                      FPCRIT, PLSE, NSEDEE, DRSED, QWAT, SLOPEE, SOSDFE, TAUJE, DLSE, FBETAE,     &
                      FDELE, QSEDE, Q, VDSED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN)          :: ISGSED, NSED, NSEDEE
      DOUBLE PRECISION, INTENT(IN) :: AREAE, DTSY, DWAT1E, DWATOE, DXQQE, DYQQE
      DOUBLE PRECISION, INTENT(IN) :: FETAE, GNUE, FPCRIT, PLSE
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NSED), QWAT (4), SLOPEE (4)
      DOUBLE PRECISION, INTENT(IN) :: SOSDFE (NSED), TAUJE (4)

      ! Input/output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: DLSE, FBETAE (NSED), FDELE (NSED)
      DOUBLE PRECISION, INTENT(INOUT) :: QSEDE (NSEDEE, 4)

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: Q (NSED), VDSED (NSED)

      ! Locals, etc
      INTEGER :: FACE, J (4), JLC, NOUT, SED
      DOUBLE PRECISION :: A1, A2, A3, B1, B2, DBETA, DDLS, FD, FLS, G
      DOUBLE PRECISION :: GJSUM, GSUM, QK, QWSUM, VD, VDSUM, VDWAT
      
      ! ! Functions
      ! LOGICAL :: GTZERO

      !----------------------------------------------------------------------*

      ! Initialization
      ! --------------
      !
      QWSUM = ZERO
      VDSUM = ZERO
      FLS = ONE - PLSE
      
      ! Replaced ALINIT with Fortran array slice
      Q(1:NSED) = ZERO

      ! Water & Sediment Budgets
      ! ------------------------
      !
      !     * Calculate water discharge & particulate supply rates
      !     * ( both non-negative ), and make a list of outflow faces
      NOUT = 0
      DO FACE = 1, 4
         QK = QWAT (FACE)
         IF (QK > ZERO) THEN
            ! * Outflow face
            QWSUM = QWSUM + QK
            NOUT = NOUT + 1
            J (NOUT) = FACE
         ELSE
            ! * Inflow or no-flow face
            DO SED = 1, NSED
               Q (SED) = Q (SED) - QSEDE (SED, FACE) / FLS
            END DO
         END IF
      END DO

      !     * Calculate volume of water + volume of discharged water
      VDWAT = DWAT1E * AREAE + QWSUM * DTSY

      !     * Calculate volume of stored sediment plus volume of
      !     * discharged sediment for each fraction ( must be non-negative )
      DDLS = FETAE * GNUE * DTSY
      DO SED = 1, NSED
         DBETA = DLSE * FBETAE (SED) + DDLS * SOSDFE (SED)
         VD = (FDELE (SED) * DWATOE + DBETA) * AREAE + Q (SED) * DTSY
         VDSUM = VDSUM + VD
         VDSED (SED) = VD
      END DO


      ! Sediment Discharge
      ! ------------------
      !
      !     Note: The only outputs from this section are the coefficients
      !           A1 and B1 required by the next section.
      !
      !     * Discharge rate based upon SUPPLY, assuming unlimited capacity
      GSUM = ZERO
      IF (GTZERO(VDWAT)) GSUM = FLS * VDSUM * (QWSUM / VDWAT)

      !     * Is discharge possible?
      IF (GTZERO(GSUM)) THEN

         ! * Yes ( implies VDSUM > 0 )
         !
         ! * Discharge rate based upon flow CAPACITY ...
         CALL SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, DRSED, QWAT, SLOPEE, TAUJE, GJSUM)

         ! ... with additional upper limit based on total suspended load
         G = MIN (GJSUM, QWSUM * FPCRIT)

         ! * Transport is governed by the lower of the two rates
         !   (take MIN before dividing, in case G>>GSUM)
         A1 = MIN (G, GSUM) / GSUM
         B1 = VDWAT

      ELSE

         ! * Either no sediment available, or no water to carry it
         !
         ! * Zero discharge case ( any sediment is deposited )
         A1 = ZERO
         B1 = ONE

      END IF


      ! Define Output Variables
      ! -----------------------
      !
      !     * Update depth of loose sediments
      DLSE = (ONE - A1) * VDSUM / AREAE

      !     * Evaluate coefficients for FBETAE
      IF (GTZERO(DLSE)) THEN
         ! * Composition of loose sediment is given by VDSED
         A2 = ONE
         B2 = VDSUM
         A3 = ZERO
      ELSE
         ! * No loose sediment left: adopt composition of surface soil
         A2 = ZERO
         B2 = ONE
         A3 = ONE
      END IF

      !     * Update compositions of suspended and loose sediments, and set
      !     * sediment flow rates for each outflow face.
      !     * ( don't pre-invert B1 or B2: they may be small! )
      DO SED = 1, NSED
         VD = VDSED (SED)
         FD = (A1 * VD) / B1
         FDELE (SED) = FD
         FBETAE (SED) = A2 * VD / B2 + A3 * SOSDFE (SED)
         
         DO JLC = 1, NOUT
            FACE = J (JLC)
            QSEDE (SED, FACE) = FLS * QWAT (FACE) * FD
         END DO
      END DO

   END SUBROUTINE SYCOLM



   !SSSSSS SUBROUTINE SYCRIT (FLAG, DRX50, TAUX, FPCLAE, TAUEC)
   PURE SUBROUTINE SYCRIT (FLAG, DRX50, TAUX, FPCLAE, TAUEC)
   !
   !----------------------------------------------------------------------*
   !
   ! Calculates critical shear stress for sediment particle transport.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR60
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH 15.07.94  Version 3.4.1 by AB/RAH. File created 05.10.93
   !----------------------------------------------------------------------*
   
      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: FLAG
      DOUBLE PRECISION, INTENT(IN) :: DRX50, TAUX, FPCLAE
      
      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: TAUEC

      ! Locals
      DOUBLE PRECISION, PARAMETER :: R0 = 3.0D-2, R1 = 1.0D0
      DOUBLE PRECISION, PARAMETER :: R2 = 6.0D0, R3 = 30.0D0, R4 = 135.0D0, R5 = 400.0D0
      
      DOUBLE PRECISION, PARAMETER :: AEC(5) = [0.1D0, 0.1D0, 0.033D0, 0.013D0, 0.03D0]
      DOUBLE PRECISION, PARAMETER :: BEC(5) = [-0.3D0, -0.62D0, 0.0D0, 0.28D0, 0.1D0]

      ! High-Performance Fix: Compile-time evaluation of constants 
      ! (Completely replaces the runtime FIRST_sycrit block)
      DOUBLE PRECISION, PARAMETER :: K1_sycrit = 1.0D0 / (SQRT(RHOWAT) * VISCOS)
      DOUBLE PRECISION, PARAMETER :: K2_sycrit = (RHOSED - RHOWAT) * GRAVTY
      DOUBLE PRECISION, PARAMETER :: K3_sycrit = 1.83D0 * LOG(10.0D0)

      INTEGER :: IS
      DOUBLE PRECISION :: RSTR
      
      ! Legacy branchless statement function
      DOUBLE PRECISION :: SF, RSTR_DUM, R_DUM
      SF(RSTR_DUM, R_DUM) = HALF - SIGN(HALF, R_DUM - RSTR_DUM)

   !----------------------------------------------------------------------*

      IF (FLAG == 1) THEN
         ! Quick method
         TAUEC = 0.493D0 * EXP(K3_sycrit * FPCLAE)
      ELSE
         ! Shields method
         RSTR = MAX(R0, MIN(DRX50 * SQRT(TAUX) * K1_sycrit, R5))

         ! Performance Reversion: Branchless execution 
         IS = NINT(ONE + SF(RSTR, R1) + SF(RSTR, R2) + SF(RSTR, R3) + SF(RSTR, R4))

         TAUEC = AEC(IS) * K2_sycrit * DRX50 * (RSTR**BEC(IS))
      END IF

   END SUBROUTINE SYCRIT



   !SSSSSS FUNCTION SYDR
   PURE DOUBLE PRECISION FUNCTION SYDR (FSED, INCF, N, F, D)
   !
   !----------------------------------------------------------------------*
   !
   ! To calculate a selected percentile based upon the discrete
   !  distribution of sediment particle diameters.
   !
   ! Note: Relevant elements of F consist of the first and then only every
   !        INCF'th element within the array. Relevant elements of F are
   !        paired in order with all elements of D to define the input
   !        distribution.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR57
   !  Module:  SY          Program:  SHETRAN
   ! Modifications:
   !  RAH  21.5.94  Version 3.4.1 by AB/RAH. File creation date 01.11.93.
   !----------------------------------------------------------------------*
   
      ! Assumed external module dependencies providing global variables:
      ! ISZERO

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN)          :: INCF, N
      DOUBLE PRECISION, INTENT(IN) :: FSED, F(1 + (N - 1) * INCF), D(N)

      ! Locals
      DOUBLE PRECISION, PARAMETER  :: ALMOST = 0.9999D0
      DOUBLE PRECISION :: DR, DRHI, DRLO, F02, FLO, FHI, FSUM2, FTOT
      INTEGER :: FRPTR, SED

   !----------------------------------------------------------------------*

      ! * Initialize local variables
      FHI = 0.0d0
      DRHI = 0.0d0
      FSUM2 = 0.0d0
      FTOT = 0.0d0
      FRPTR = 1

      ! * Double the selected 'percentile' (actually a fraction 0-1)
      ! * and scale it relative to the sum of distribution ratios
      DO SED = 1, N
         FTOT = FTOT + F(FRPTR)
         FRPTR = FRPTR + INCF
      END DO
      
      F02 = 2.0d0 * FSED * FTOT

      IF (ISZERO(F02)) THEN
         ! * Zeroth percentile or null distribution
         DR = 0.0d0
         
      ELSE
         ! * Reset fraction pointer
         FRPTR = 1

         ! * Loop over sediment types until target percentile surpassed
         search_loop: DO SED = 1, N
            
            ! * Calculate midpoint of cumulative fraction (doubled)
            FLO = FHI
            DRLO = DRHI
            FHI = F(FRPTR)
            DRHI = D(SED)
            FSUM2 = FSUM2 + FLO + FHI

            ! * Break out of loop if target percentile has been reached
            ! * (allowing for rounding error)
            IF (FSUM2 >= F02 * ALMOST) EXIT search_loop

            ! * Increment fraction pointer
            FRPTR = FRPTR + INCF
            
         END DO search_loop

         ! * Interpolate between last two Fraction/Diameter pairs to find
         ! * target percentile.
         ! * Note :- Combination of precondition FSED<1 and use of ALMOST
         ! * should ensure (FLO+FHI) > 0
         DR = DRHI - (DRHI - DRLO) * (FSUM2 - F02) / (FLO + FHI)
         
      END IF

      SYDR = DR

   END FUNCTION SYDR



   !----------------------------------------------------------------------*
   !
   ! To calculate streamwise capacity for particulate discharge for each
   !  non-fine size group, for each link, according to the Engelund-Hansen
   !  formula.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR58
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  15.07.94  Version 3.4.1 by AB/RAH. File creation date 12.4.94.
   !----------------------------------------------------------------------*
   PURE SUBROUTINE SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, QOC, LINKNS, SLOPEJ, GSED)

      IMPLICIT NONE

      ! Commons and distributed constants
      !
      ! Constants referenced
      !   CONST.SY:  RHOWAT, RHOSED, GRAVTY
      !   (Assumed from CONST_SY: FIRST_syengh, KG_syengh, ZERO, GTZERO)

      ! Input arguments
      INTEGER, INTENT(IN)          :: NFINE, NLF, NSED, NELEE
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NFINE + 1:NSED), CWIDTH (NLF), DWAT1 (NLF)
      DOUBLE PRECISION, INTENT(IN) :: QOC (NELEE, 4), SLOPEJ (NELEE, 4)
      LOGICAL, INTENT(IN)          :: LINKNS (NLF)

      ! Output arguments
      ! Defined locally
      DOUBLE PRECISION, INTENT(OUT) :: GSED (NLF, NFINE + 1:NSED)

      ! Locals, etc
      INTEGER          :: FACE, IEND, LINK, NFP1, SED, SGN
      DOUBLE PRECISION :: DWAT1E, GD, QK
      DOUBLE PRECISION, PARAMETER :: KG_syengh = 0.05D0 / (SQRT (GRAVTY) * (RHOSED / RHOWAT - 1.0D0)**2)

      ! External/Module functions implicitly referenced
      ! LOGICAL :: GTZERO

      !----------------------------------------------------------------------*

      ! * Initialization      
      NFP1 = NFINE + 1
      
      ! Replaced ALINIT with a whole-array slice assignment
      GSED(:, :) = ZERO

      ! * Loop over ends of link
      DO IEND = 1, 3, 2
         SGN = 2 - IEND

         ! * Loop over links
         DO LINK = 1, NLF

            ! * Determine current face number, outflow rate & water depth
            FACE = IEND
            IF (LINKNS (LINK)) FACE = FACE + 1
            QK = SGN * QOC (LINK, FACE)
            DWAT1E = DWAT1 (LINK)

            ! * Increment capacity rate for non-dry outflow ends only
            IF (GTZERO(DWAT1E) .AND. GTZERO(QK)) THEN

               ! * Loop invariant
               GD = QK**2 * SLOPEJ (LINK, FACE)**1.5D0 * KG_syengh / (CWIDTH (LINK) * SQRT (DWAT1E))
               
               ! * All sediment types
               DO SED = NFP1, NSED
                  GSED (LINK, SED) = GD / DRSED (SED) + GSED (LINK, SED)
               END DO

            END IF

         ! * Next link
         END DO

      ! * Next iend
      END DO

   END SUBROUTINE SYENGH



   !SSSSSS SUBROUTINE SYERR0
   !----------------------------------------------------------------------*
   !
   ! Check static variables & constants in the WAT-SY interface.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1          Notes:  SSR82
   !  Module:  SY           Program:  SHETRAN
   ! Modifications:
   !  RAH  23.09.94  Version 3.4.1 created.
   !----------------------------------------------------------------------*
   SUBROUTINE SYERR0(NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, &
                     NSEE, NV, NVEE, NX, NXEE, NY, SPR, SYD)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE
      INTEGER, INTENT(IN) :: NV, NVEE, NX, NXEE, NY, SPR, SYD

      ! Locals, etc
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      
      ! Modernization Fix: Added IZERO_ARR to replace the undeclared IZERO1
      ! and made IUNDEF a parameter to prevent passing uninitialized memory.
      INTEGER, PARAMETER :: IZERO_ARR(1) = [0]
      INTEGER, PARAMETER :: IUNDEF = 0
      
      INTEGER :: NERR, JEDUMDUM
      INTEGER :: IDUMS(1), IDUMO(1)
      LOGICAL :: LDUM1(1)

   !----------------------------------------------------------------------*

   ! 0. Preliminaries
   ! ----------------
   
      ! * Initialize local counters
      NERR = 0

   ! 1. Array Sizes
   ! --------------

   ! NELEE
      IDUMS(1) = NELEE
      IDUMO(1) = MAX(NEL, NV, NX * NY)
      CALL ALCHKI(ERR, 2054, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      
   ! NLFEE
      IDUMS(1) = NLFEE
      IDUMO(1) = MAX(1, NLF)
      CALL ALCHKI(ERR, 2055, SPR, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      
   ! NLYREE, NSEDEE
      IDUMS(1) = MIN(NLYREE, NSEDEE)
      CALL ALCHKI(ERR, 2056, SPR, 1, 1, IUNDEF, IUNDEF, '[ NLYREE, NSEDEE ]', 'GT', IZERO_ARR, IDUMS, NERR, LDUM1)
      
   ! NSEE
      IDUMS(1) = NSEE
      IDUMO(1) = NS
      CALL ALCHKI(ERR, 2057, SPR, 1, 1, IUNDEF, IUNDEF, 'NSEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      
   ! NVEE
      IDUMS(1) = NVEE
      IDUMO(1) = NV
      CALL ALCHKI(ERR, 2058, SPR, 1, 1, IUNDEF, IUNDEF, 'NVEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      
   ! NXEE
      IDUMS(1) = NXEE
      IDUMO(1) = NX
      CALL ALCHKI(ERR, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      
      IDUMO(1) = 9999
      CALL ALCHKI(ERR, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'LE', IDUMO, IDUMS, NERR, LDUM1)

   ! 2. Unit Numbers
   ! ---------------

   ! SPR, SYD
      IDUMS(1) = MIN(SPR, SYD)
      CALL ALCHKI(ERR, 2060, SPR, 1, 1, IUNDEF, IUNDEF, '[ SPR, SYD ]', 'GE', IZERO_ARR, IDUMS, NERR, LDUM1)

   ! 3. Number of Entities
   ! ---------------------

   ! NLF
      IDUMS(1) = NLF
      IDUMO(1) = NEL
      CALL ALCHKI(ERR, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO_ARR, IDUMS, NERR, LDUM1)
      CALL ALCHKI(ERR, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)
      
   ! NS, NV, NX, NY
      JEDUMDUM = MIN(NS, NV)
      IDUMS(1) = MIN(JEDUMDUM, NX, NY)
      CALL ALCHKI(ERR, 2062, SPR, 1, 1, IUNDEF, IUNDEF, '[ NS, NV, NX, NY ]', 'GT', IZERO_ARR, IDUMS, NERR, LDUM1)

   ! 4. Epilogue
   ! -----------

      IF (NERR > 0) THEN
         CALL ERROR(FATAL, 2000, SPR, 0, 0, 'Error(s) detected while checking WAT-SY interface variables')
      END IF

   END SUBROUTINE SYERR0



   !SSSSSS SUBROUTINE SYERR1
   !----------------------------------------------------------------------*
   ! Check static & initializing arrays in the WAT-SY interface.
   !----------------------------------------------------------------------*
   ! Version:  3.4.1          Notes:  SSR83
   !  Module:  SY           Program:  SHETRAN
   ! Modifications:
   !  RAH  26.09.94  Version 3.4.1.  File created 25.09.94.
   !----------------------------------------------------------------------*
   SUBROUTINE SYERR1(NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, &
                     NXEE, NYEE, NY, SPR, BEXBK, LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, &
                     NTSOIL, NVC, THSAT, CLENTH, CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, &
                     ARXL, HRF, ZGRUND, IDUM, IDUM1X, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, NXEE, NYEE, NY, SPR
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS(NLFEE)
      
      ! Read-Only Arrays (Used for reference or copied to scratchpads)
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2), ICMXY(NXEE, NY), ICMRF2(NLFEE, 3, 2)
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE)
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND(NEL)

      ! Arrays checked by ALCHK/ALCHKI (routines may use INTENT(INOUT) interfaces)
      INTEGER, INTENT(INOUT) :: ICMREF(NELEE, 4, 2:3)
      INTEGER, INTENT(INOUT) :: NLYR(NLF + 1:NEL), NVC(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: THSAT(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CLENTH(NLFEE), CWIDTH(NLFEE), ZBFULL(NLFEE)
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NLF + 1:NEL), DYQQ(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: AREA(NEL), DHF(NELEE, 4)
      DOUBLE PRECISION, INTENT(INOUT) :: ARXL(NLFEE), HRF(NLF + 1:NEL)

      ! Workspace arguments (INTENT(INOUT) as scratch space)
      INTEGER, INTENT(INOUT) :: IDUM(NXEE * NYEE)
      INTEGER, INTENT(INOUT) :: IDUM1X(-1:NEL + 1)
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)

      ! Locals, etc
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      
      ! Strict array/scalar parameters for shape matching in ALCHK
      INTEGER, PARAMETER          :: IZERO_ARR(1) = [0], IONE_ARR(1) = [1]
      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      
      INTEGER :: BANK, COUNT, FACE, FADJ, FEL
      INTEGER :: IADJ, IBR, IBRADJ, ICOL1, IEL, IELP, ILYR, IUNDEF, IX, IY
      INTEGER :: LINK, NCOL, NELP, NERR, P, PADJ
      INTEGER :: IDUM1(2)
      LOGICAL :: BKXYOK, REFOK

   !----------------------------------------------------------------------*

   ! 0. Preliminaries
   ! ----------------
      NERR = 0
      ! IUNDEF = 0
      ICOL1 = NLF + 1
      NELP = NEL + 1

   ! 1. Index Arrays
   ! ---------------

   ! ICMBK, ICMXY
      COUNT = NERR
      NCOL = 0
      
      DO IEL = 0, NLF
         IDUM1X(IEL) = 1
      END DO
      
      DO IEL = ICOL1, NELP
         IDUM1X(IEL) = 0
      END DO
      
      DO IY = 1, NY
         DO IX = 1, NX
            IEL = MAX(0, MIN(ICMXY(IX, IY), NELP))
            IDUM1X(IEL) = IDUM1X(IEL) + 1
            NCOL = NCOL + MIN(IEL, 1)
         END DO
      END DO
      
      IF (BEXBK .AND. NLF > 0) THEN
         NCOL = NCOL + 2 * NLF
         DO BANK = 1, 2
            DO LINK = 1, NLF
               IEL = MAX(0, MIN(ICMBK(LINK, BANK), NELP))
               IDUM1X(IEL) = IDUM1X(IEL) + 1
            END DO
         END DO
      END IF
      
      IDUM1(1) = NEL - NLF
      IDUM1X(0) = NCOL
      
      CALL ALCHKI(ERR, 2075, SPR, 1, 1, IUNDEF, IUNDEF, '#_column_elements', 'EQ', IDUM1, IDUM1X(0:), NERR, LDUM)
      CALL ALCHKI(ERR, 2076, SPR, 1, NEL, IUNDEF, IUNDEF, 'element_count(iel)', 'EQ', IONE_ARR, IDUM1X(1:), NERR, LDUM)
      
      BKXYOK = COUNT == NERR

   ! ICMREF part 1
      IDUM1(1) = NEL
      IDUM1(2) = -NLFEE
      REFOK = .TRUE.
      
      DO FACE = 1, 4
         COUNT = NERR
         
         CALL ALCHKI(ERR, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)', 'LE', IDUM1(1:1), ICMREF(1:, FACE, 2), NERR, LDUM)
         CALL ALCHKI(ERR, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)', 'GE', IDUM1(2:2), ICMREF(1:, FACE, 2), NERR, LDUM)
         
         IF (COUNT == NERR) THEN
            DO IEL = 1, NEL
               IADJ = ICMREF(IEL, FACE, 2)
               IF (IADJ <= 0) THEN
                  IDUM(IEL) = 0
               ELSE
                  FADJ = ICMREF(IEL, FACE, 3)
                  IF (FADJ < 1 .OR. FADJ > 4) THEN
                     IDUM(IEL) = 1
                  ELSE
                     IF (ICMREF(IADJ, FADJ, 2) /= IEL) THEN
                        IDUM(IEL) = 2
                     ELSE
                        IDUM(IEL) = 0
                        IF (ICMREF(IADJ, FADJ, 3) /= FACE) IDUM(IEL) = 3
                     END IF
                  END IF
               END IF
            END DO
            CALL ALCHKI(ERR, 2078, SPR, 1, NEL, FACE, IUNDEF, 'status_of_ICMREF(iel,face)', 'EQ', IZERO_ARR, IDUM, NERR, LDUM)
         END IF
         REFOK = REFOK .AND. COUNT == NERR
      END DO

   ! ICMREF part 2 (bank element neighbours)
      IF (NLF > 0 .AND. BEXBK .AND. BKXYOK .AND. REFOK) THEN
         IDUM1X(-1) = -2
         IDUM1X(0) = 0
         
         DO IEL = 1, NEL
            IDUM1X(IEL) = -2
         END DO
         
         DO IY = 1, NY
            DO IX = 1, NX
               IEL = MAX(0, ICMXY(IX, IY))
               IDUM1X(IEL) = MIN(IEL, 1)
            END DO
         END DO
         
         DO LINK = 1, NLF
            IDUM(LINK) = 0
         END DO
         
         DO BANK = 1, 2
            DO LINK = 1, NLF
               IEL = ICMBK(LINK, BANK)
               FACE = 2 * BANK
               IF (LINKNS(LINK)) FACE = FACE - 1
               IADJ = MAX(-1, ICMREF(IEL, FACE, 2))
               IDUM(LINK) = IDUM(LINK) + IDUM1X(IADJ)
            END DO
         END DO
         
         CALL ALCHKI(ERR, 2079, SPR, 1, NLF, IUNDEF, IUNDEF, '#_grids_neighbouring_banks(link)', 'GT', IZERO_ARR, IDUM, NERR, LDUM)
      END IF

   ! ICMRF2
      IF (REFOK) THEN
         DO IBR = 1, NLFEE
            IDUM(IBR) = -1
         END DO
         
         DO FACE = 1, 4
            DO IEL = 1, NEL
               IADJ = ICMREF(IEL, FACE, 2)
               IF (IADJ < 0) THEN
                  IBR = -IADJ
                  IF (IDUM(IBR) >= 0) THEN
                     IDUM(IBR) = IDUM(IBR) + 1
                  ELSE
                     IDUM(IBR) = 0
                     
                     DO P = 1, 3
                        IADJ = ICMRF2(IBR, P, 1)
                        IF (IADJ > NEL) THEN
                           IDUM(IBR) = IDUM(IBR) + P * 10
                        ELSE IF (IADJ > 0) THEN
                           FADJ = ICMRF2(IBR, P, 2)
                           IF (FADJ < 1 .OR. FADJ > 4) THEN
                              IDUM(IBR) = IDUM(IBR) + P * 100
                           ELSE
                              IBRADJ = -ICMREF(IADJ, FADJ, 2)
                              IF (IBRADJ < 1 .OR. IBRADJ > NLFEE) THEN
                                 IDUM(IBR) = IDUM(IBR) + P * 1000
                              ELSE
                                 
                                 search_padj: DO PADJ = 1, 3
                                    IELP = ICMRF2(IBRADJ, PADJ, 1)
                                    IF (IELP == IEL) THEN
                                       FEL = ICMRF2(IBRADJ, PADJ, 2)
                                       IF (FEL == FACE) EXIT search_padj
                                    END IF
                                 END DO search_padj
                                 
                                 IF (PADJ > 3) IDUM(IBR) = IDUM(IBR) + P * 10000
                                 
                              END IF
                           END IF
                        END IF
                     END DO
                  END IF
               END IF
            END DO
         END DO
         
         CALL ALCHKI(ERR, 2080, SPR, 1, NLFEE, IUNDEF, IUNDEF, 'status_of_ICMRF2(branch)', 'LE', IZERO_ARR, IDUM, NERR, LDUM)
      END IF

   ! 2. Soil Properties
   ! ------------------
      CALL ALCHK(ERR, 2063, SPR, 1, NS, IUNDEF, IUNDEF, 'THSAT(soil)', 'LE', ONE_ARR, ZERO_VAL, THSAT, NERR, LDUM)

   ! 3. Link Properties & Initial State
   ! ----------------------------------
      IF (NLF > 0) THEN
         CALL ALCHK(ERR, 2064, SPR, 1, NLF, IUNDEF, IUNDEF, 'CLENTH(link)', 'GE', ZERO_ARR, ZERO_VAL, CLENTH, NERR, LDUM)
         CALL ALCHK(ERR, 2065, SPR, 1, NLF, IUNDEF, IUNDEF, 'CWIDTH(link)', 'GT', ZERO_ARR, ZERO_VAL, CWIDTH, NERR, LDUM)
         CALL ALCHK(ERR, 2066, SPR, 1, NLF, IUNDEF, IUNDEF, 'ZBFULL(link)', 'GEa', ZGRUND(1:), ZERO_VAL, ZBFULL, NERR, LDUM)
         CALL ALCHK(ERR, 2067, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', 'GE', ZERO_ARR, ZERO_VAL, ARXL, NERR, LDUM)
      END IF

   ! 4. Column Properties & Initial State
   ! ------------------------------------
      CALL ALCHK(ERR, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DXQQ(iel)', 'GT', ZERO_ARR, ZERO_VAL, DXQQ(ICOL1:), NERR, LDUM)
      CALL ALCHK(ERR, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DYQQ(iel)', 'GT', ZERO_ARR, ZERO_VAL, DYQQ(ICOL1:), NERR, LDUM)
      CALL ALCHK(ERR, 2069, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', 'GEa', ZGRUND(ICOL1:), ZERO_VAL, HRF(ICOL1:), NERR, LDUM)
      
      COUNT = NERR
      IDUM1(1) = NLYREE
      CALL ALCHKI(ERR, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'GT', IZERO_ARR, NLYR(ICOL1:), NERR, LDUM)
      CALL ALCHKI(ERR, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'LE', IDUM1(1:1), NLYR(ICOL1:), NERR, LDUM)
      
      IF (COUNT == NERR) THEN
         DO IEL = ICOL1, NEL
            ILYR = NLYR(IEL)
            IDUM(IEL) = NTSOIL(IEL, ILYR)
         END DO
         IDUM1(1) = NS
         CALL ALCHKI(ERR, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NTSOIL[iel,NLYR(iel)]', 'GT', IZERO_ARR, IDUM(ICOL1:), NERR, LDUM)
         CALL ALCHKI(ERR, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NTSOIL[iel,NLYR(iel)]', 'LE', IDUM1(1:1), IDUM(ICOL1:), NERR, LDUM)
      END IF
      
      COUNT = NERR
      IDUM1(1) = NV
      CALL ALCHKI(ERR, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'GT', IZERO_ARR, NVC(ICOL1:), NERR, LDUM)
      CALL ALCHKI(ERR, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'LE', IDUM1(1:1), NVC(ICOL1:), NERR, LDUM)

   ! 5. Element Properties
   ! ---------------------
      CALL ALCHK(ERR, 2073, SPR, 1, NEL, IUNDEF, IUNDEF, 'AREA(iel)', 'GT', ZERO_ARR, ZERO_VAL, AREA, NERR, LDUM)
      DO FACE = 1, 4
         CALL ALCHK(ERR, 2074, SPR, 1, NEL, FACE, IUNDEF, 'DHF(iel,face)', 'GT', ZERO_ARR, ZERO_VAL, DHF(1:, FACE), NERR, LDUM)
      END DO

   ! 6. Epilogue
   ! -----------
      IF (NERR > 0) THEN
         CALL ERROR(FATAL, 2001, SPR, 0, 0, 'Error(s) detected while checking static/initial WAT-SY interface')
      END IF

   END SUBROUTINE SYERR1



   !----------------------------------------------------------------------*
   !
   ! Check for errors in the SY input data.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1           Notes:  SSR43
   !  Module:  SY              Program:  SHETRAN
   ! Modifications:
   !  RAH  04.10.94  Version 3.4.1 by AB/RAH. File created 3.2.94.
   !  BTL  25.05.95  Version 3.4.1 : add DLSMAX
   !----------------------------------------------------------------------*
   SUBROUTINE SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV, NSYB, NSYBEE, &
                      NSYC, NSYCEE, SPR, ICMREF, ISUSED, NEPS, NFINE, SFB, SRB, ALPHA, DCBEDO,      &
                      FPCRIT, DLSMAX, NTSOBK, NSYBCD, NBFACE, DRSED, BKB, GKF, GKR, RHOSO, SOSDFN,  &
                      DRDRIP, FDRIP, XDRIP, PBSED, FCG, FCROCK, PLS, DLS, FBETA, FDEL, ABC, BBC,    &
                      GBC, IDUM, DUMMY, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN)          :: NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV
      INTEGER, INTENT(IN)          :: NSYB, NSYBEE, NSYC (4), NSYCEE, SPR
      INTEGER, INTENT(IN)          :: ICMREF (NELEE, 4, 2:2)
      INTEGER, INTENT(IN)          :: NBFACE (NEL)
      INTEGER, INTENT(IN)          :: SFB, SRB

      ! Input/Output arguments (Variables modified via ALCHK/ALCHKI checking/casting)
      INTEGER, INTENT(INOUT)          :: ISUSED, NEPS, NFINE
      INTEGER, INTENT(INOUT)          :: NTSOBK (NLFEE), NSYBCD (NSYBEE, 3)
      DOUBLE PRECISION, INTENT(INOUT) :: ALPHA, DCBEDO, FPCRIT, DLSMAX
      DOUBLE PRECISION, INTENT(INOUT) :: DRSED (NSED), BKB (NS), GKF (NS), GKR (NS), RHOSO (NS)
      DOUBLE PRECISION, INTENT(INOUT) :: SOSDFN (NSEE, NSED), DRDRIP (NV), FDRIP (NV), XDRIP (NV)
      DOUBLE PRECISION, INTENT(INOUT) :: PBSED (NLFEE)
      DOUBLE PRECISION, INTENT(INOUT) :: FCG (NLF + 1:NEL), FCROCK (NLF + 1:NEL), PLS (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: DLS (NEL), FBETA (NELEE, NSED), FDEL (NELEE, NSED)
      DOUBLE PRECISION, INTENT(INOUT) :: ABC (NSEDEE, NSYCEE), BBC (NSEDEE, NSYCEE), GBC (NSEDEE, NSYCEE)

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT)      :: IDUM
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY
      LOGICAL, DIMENSION(NELEE), INTENT(INOUT)          :: LDUM

      ! Locals, etc
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      DOUBLE PRECISION, PARAMETER :: TOL = 1.0D-10

      INTEGER :: BB, COUNT, FACE, ICAT, IUNDEF, IEL, ITYPE, NERR
      INTEGER :: SED, SOIL, jedumdum
      INTEGER :: IDUM1 (1)
      DOUBLE PRECISION :: RDUM (NXEE*NYEE)
      
      ! Assumed external constants/functions from module/global
      ! ZERO1, ONE1, IZERO1, IONE1, IDIMJE

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------
      !     * Local counter
      NERR = 0
      ! IUNDEF = 0 ! Or however IUNDEF is defined in your environment

      ! 1. Static Variables
      ! -------------------

      ! NEPS
      IDUM (1) = NEPS
      CALL ALCHKI (ERR, 2012, SPR, 1, 1, IUNDEF, IUNDEF, 'NEPS', 'GE', IONE1, IDUM, NERR, LDUM)
      NEPS = IDUM (1)

      ! FPCRIT
      DUMMY (1) = FPCRIT
      CALL ALCHK (ERR, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'FPCRIT', 'GE', ZERO1, ZERO1 (1), DUMMY, NERR, LDUM)
      FPCRIT = DUMMY (1)

      ! DLSMAX
      DUMMY (1) = DLSMAX
      CALL ALCHK (ERR, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'DLSMAX', 'GE', ZERO1, ZERO1 (1), DUMMY, NERR, LDUM)
      DLSMAX = DUMMY (1)

      IF (NLF > 0) THEN
         ! ISUSED
         IDUM (1) = ISUSED
         CALL ALCHKI (ERR, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', 'GE', IZERO1, IDUM, NERR, LDUM)
         CALL ALCHKI (ERR, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', 'LE', IONE1, IDUM, NERR, LDUM)
         ISUSED = IDUM (1)

         ! NFINE
         IDUM (1) = NFINE
         IDUM1 (1) = MIN (1, NSED - 1)
         CALL ALCHKI (ERR, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', 'GE', IZERO1, IDUM, NERR, LDUM)
         CALL ALCHKI (ERR, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', 'LE', IDUM1, IDUM, NERR, LDUM)
         NFINE = IDUM (1)

         ! ALPHA
         IF (NFINE > 0) THEN
            DUMMY (1) = ALPHA
            CALL ALCHK (ERR, 2016, SPR, 1, 1, IUNDEF, IUNDEF, 'ALPHA', 'GE', ZERO1, ZERO1 (1), DUMMY, NERR, LDUM)
            ALPHA = DUMMY (1)
         END IF

         ! DCBEDO
         DUMMY (1) = DCBEDO
         CALL ALCHK (ERR, 2017, SPR, 1, 1, IUNDEF, IUNDEF, 'DCBEDO', 'GE', ZERO1, ZERO1 (1), DUMMY, NERR, LDUM)
         DCBEDO = DUMMY (1)
      END IF

      ! NELEE
      IDUM (1) = NXEE * NYEE
      jedumdum = IDIMJE(NSED, NFINE)
      jedumdum = jedumdum * NLF
      IDUM1(1) = MAX(NSED, jedumdum)
      
      ! * (including local workspace requirements)
      IDUM1 (1) = MAX (IDUM1 (1), NS, NSYB * 2)
      CALL ALCHKI (ERR, 2018, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUM1, IDUM, NERR, LDUM)


      ! 2. Sediment, Soil & Vegetation Properties
      ! -----------------------------------------
      !
      ! * Not enough workspace? (Converted GOTO 300 to block IF)
      IF (NELEE >= MAX (NSED, NS)) THEN

         ! DRSED
         COUNT = NERR
         CALL ALCHK (ERR, 2019, SPR, 1, 1, IUNDEF, IUNDEF, 'DRSED(sed)', 'GT', ZERO1, ZERO1 (1), DRSED (1), NERR, LDUM)

         IF (NSED > 1 .AND. NERR == COUNT) THEN
            CALL DCOPY (NSED - 1, DRSED, 1, RDUM, 1)
            IDUM(1:NSED - 1) = INT (RDUM(1:NSED - 1))
            CALL ALCHK (ERR, 2019, SPR, 2, NSED, IUNDEF, IUNDEF, 'DRSED(sed)', 'GEa', RDUM, ZERO1 (1), DRSED (2), NERR, LDUM)
         END IF

         ! GKR
         CALL ALCHK (ERR, 2020, SPR, 1, NS, IUNDEF, IUNDEF, 'GKR(soil)', 'GE', ZERO1, ZERO1 (1), GKR, NERR, LDUM)
         ! GKF
         CALL ALCHK (ERR, 2021, SPR, 1, NS, IUNDEF, IUNDEF, 'GKF(soil)', 'GE', ZERO1, ZERO1 (1), GKF, NERR, LDUM)
         ! RHOSO
         CALL ALCHK (ERR, 2022, SPR, 1, NS, IUNDEF, IUNDEF, 'RHOSO(soil)', 'GT', ZERO1, ZERO1 (1), RHOSO, NERR, LDUM)
         
         ! BKB
         IF (NLF > 0) THEN
            CALL ALCHK (ERR, 2023, SPR, 1, NS, IUNDEF, IUNDEF, 'BKB(soil)', 'GE', ZERO1, ZERO1 (1), BKB, NERR, LDUM)
         END IF

         ! SOSDFN
         DUMMY(1:NS) = ZERO1 (1)
         DO SED = 1, NSED
            DO SOIL = 1, NS
               DUMMY (SOIL) = DUMMY (SOIL) + SOSDFN (SOIL, SED)
            END DO
            CALL ALCHK (ERR, 2024, SPR, 1, NS, SED, IUNDEF, 'SOSDFN(soil,sed)', 'GE', ZERO1, ZERO1 (1), SOSDFN (1, SED), NERR, LDUM)
         END DO
         CALL ALCHK (ERR, 2024, SPR, 1, NS, IUNDEF, IUNDEF, 'SOSDFN[*][sum_over_sed](soil)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)
         
         ! XDRIP
         CALL ALCHK (ERR, 2025, SPR, 1, NV, IUNDEF, IUNDEF, 'XDRIP(veg)', 'GE', ZERO1, ZERO1 (1), XDRIP, NERR, LDUM)
         ! DRDRIP
         CALL ALCHK (ERR, 2026, SPR, 1, NV, IUNDEF, IUNDEF, 'DRDRIP(veg)', 'GT', ZERO1, ZERO1 (1), DRDRIP, NERR, LDUM)
         ! FDRIP
         CALL ALCHK (ERR, 2027, SPR, 1, NV, IUNDEF, IUNDEF, 'FDRIP(veg)', 'GE', ZERO1, ZERO1 (1), FDRIP, NERR, LDUM)

      END IF


      ! 3. Link Element Properties
      ! --------------------------
      !
      IF (NLF > 0) THEN
         ! NTSOBK
         IDUM (1) = NS
         CALL ALCHKI (ERR, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'GE', IONE1, NTSOBK, NERR, LDUM)
         CALL ALCHKI (ERR, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'LE', IDUM, NTSOBK, NERR, LDUM)
         ! PBSED
         CALL ALCHK (ERR, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', 'GE', ZERO1, ZERO1 (1), PBSED, NERR, LDUM)
         CALL ALCHK (ERR, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', 'LT', ONE1, ZERO1 (1), PBSED, NERR, LDUM)
      END IF


      ! 4. Column-element Properties
      ! ----------------------------
      !
      ! FCROCK
      CALL ALCHK (ERR, 2030, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCROCK(iel)', 'LE', ONE1, ZERO1 (1), FCROCK, NERR, LDUM)
      
      ! FCG
      DO IEL = NLF + 1, NEL
         DUMMY (IEL) = ONE1 (1) - FCROCK (IEL)
      END DO
      CALL ALCHK (ERR, 2031, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCG(iel)', 'LEa', DUMMY (NLF + 1), ZERO1 (1), FCG, NERR, LDUM)
      
      ! PLS
      CALL ALCHK (ERR, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'GE', ZERO1, ZERO1 (1), PLS, NERR, LDUM)
      CALL ALCHK (ERR, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'LT', ONE1, ZERO1 (1), PLS, NERR, LDUM)


      ! 5. All-element Initialization
      ! -----------------------------
      !
      ! DLS
      CALL ALCHK (ERR, 2033, SPR, 1, NEL, IUNDEF, IUNDEF, 'DLS(iel)', 'GE', ZERO1, ZERO1 (1), DLS, NERR, LDUM)
      
      ! FBETA
      DUMMY(1:NEL) = ZERO1 (1)
      DO SED = 1, NSED
         DO IEL = 1, NEL
            DUMMY (IEL) = DUMMY (IEL) + FBETA (IEL, SED)
         END DO
         CALL ALCHK (ERR, 2034, SPR, 1, NEL, SED, IUNDEF, 'FBETA(iel,sed)', 'GE', ZERO1, ZERO1 (1), FBETA (1, SED), NERR, LDUM)
      END DO
      CALL ALCHK (ERR, 2034, SPR, 1, NEL, IUNDEF, IUNDEF, 'FBETA[*][sum_over_sed](iel)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)
      
      ! FDEL
      DO SED = 1, NSED
         CALL ALCHK (ERR, 2035, SPR, 1, NEL, SED, IUNDEF, 'FDEL(iel,sed)', 'GE', ZERO1, ZERO1 (1), FDEL (1, SED), NERR, LDUM)
      END DO


      ! 6. Boundary Data
      ! ----------------
      !
      IF (NSYB > 0) THEN
         IF (NELEE >= NSYB * 2) THEN
            
            ! NSYCEE
            IDUM (1) = NSYCEE
            IDUM1 (1) = MAX (NSYC (1) + NSYC (2), NSYC (3) + NSYC (4))
            CALL ALCHKI (ERR, 2036, SPR, 1, 1, IUNDEF, IUNDEF, 'NSYCEE', 'GE', IDUM1, IDUM, NERR, LDUM)
            
            ! NSYBCD(BB,1)
            COUNT = NERR
            IDUM1 (1) = NEL
            CALL ALCHKI (ERR, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', 'GE', IONE1, NSYBCD, NERR, LDUM)
            CALL ALCHKI (ERR, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', 'LE', IDUM1, NSYBCD, NERR, LDUM)
            
            ! NBFACE
            IF (COUNT == NERR) THEN
               DO BB = 1, NSYB
                  IEL = NSYBCD (BB, 1)
                  IDUM (BB) = NBFACE (IEL)
               END DO
               IDUM1 (1) = 4
               CALL ALCHKI (ERR, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, 'NBFACE[NSYBCD[*][1]](bdry)', 'GE', IONE1, IDUM, NERR, LDUM)
               CALL ALCHKI (ERR, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, 'NBFACE[NSYBCD[*][1]](bdry)', 'LE', IDUM1, IDUM, NERR, LDUM)
            END IF
            
            ! ICMREF
            IF (COUNT == NERR) THEN
               DO BB = 1, NSYB
                  IEL = NSYBCD (BB, 1)
                  FACE = NBFACE (IEL)
                  IDUM (BB) = ICMREF (IEL, FACE, 2)
               END DO
               CALL ALCHKI (ERR, 2039, SPR, 1, NSYB, IUNDEF, IUNDEF, 'ICMREF[NSYBCD[*][1]][NBFACE][2](bdry)', 'EQ', IZERO1, IDUM, NERR, LDUM)
            END IF
            
            ! NSYBCD(BB,3)
            DO BB = 1, NSYB
               ITYPE = NSYBCD (BB, 2)
               IDUM (BB) = 1
               IF (MOD (ITYPE, 2) == 0) IDUM (BB) = IDUM (BB) + NSYC (ITYPE - 1)
               IDUM (NSYB + BB) = IDUM (BB) + NSYC (ITYPE)
            END DO
            CALL ALCHKI (ERR, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', 'GE', IDUM, NSYBCD (1, 3), NERR, LDUM)
            CALL ALCHKI (ERR, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', 'LE', IDUM (NSYB + 1), NSYBCD (1, 3), NERR, LDUM)
            
            ! GBC
            DO ICAT = 1, NSYC (1)
               CALL ALCHK (ERR, 2041, SPR, 1, NSED, ICAT, IUNDEF, 'GBC(sed,icat)', 'GE', ZERO1, ZERO1 (1), GBC (1, ICAT), NERR, LDUM)
            END DO
            
            ! ABC
            DO ICAT = 1, NSYC (3)
               CALL ALCHK (ERR, 2042, SPR, 1, NSED, ICAT, IUNDEF, 'ABC(sed,icat)', 'GE', ZERO1, ZERO1 (1), ABC (1, ICAT), NERR, LDUM)
            END DO
            
            ! BBC
            DO ICAT = 1, NSYC (3)
               CALL ALCHK (ERR, 2043, SPR, 1, NSED, ICAT, IUNDEF, 'BBC(sed,icat)', 'GT', ZERO1, ZERO1 (1), BBC (1, ICAT), NERR, LDUM)
            END DO
            
            ! SFB
            IF (NSYC (2) > 0) THEN
               IDUM (1) = SFB
               CALL ALCHKI (ERR, 2044, SPR, 1, 1, IUNDEF, IUNDEF, 'SFB', 'GE', IZERO1, IDUM, NERR, LDUM)
            END IF
            
            ! SRB
            IF (NSYC (2) > 0) THEN
               IDUM (1) = SRB
               CALL ALCHKI (ERR, 2045, SPR, 1, 1, IUNDEF, IUNDEF, 'SRB', 'GE', IZERO1, IDUM, NERR, LDUM)
            END IF
         END IF
      END IF


      ! 7. Epilogue
      ! -----------
      !
      IF (NERR > 0) CALL ERROR (FATAL, 2000, SPR, 0, 0, 'Error(s) detected while checking SY input data')

   END SUBROUTINE SYERR2



   !SSSSSS SUBROUTINE SYERR3
   SUBROUTINE SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, &
         ICMRF2, ISORT, DTUZ, CLAI, PLAI, ARXL, DRAINA, PNETTO, HRF, &
         ZGRUND, QOC, IQ, JMIN, JSORT, LDUM)
   !
   !----------------------------------------------------------------------*
   !
   ! Check for time-dependent errors in the WAT-SY interface.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1          Notes:  SSR81
   !  Module:  SY           Program:  SHETRAN
   ! Modifications:
   !  RAH  10.10.94  Version 3.4.1. File created 20.09.94.
   !----------------------------------------------------------------------*
   !
      ! Assumed external module dependencies providing global variables:
      ! ALCHK, ALCHKI, ERROR, zero1, ONE1, IZERO1

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN) :: NEL, NELEE, NLF, NLFEE, NV, SPR
      INTEGER, INTENT(IN) :: ICMREF (NELEE, 4, 2:3), ICMRF2 (NLFEE, 3, 2)
      INTEGER, INTENT(IN) :: ISORT (NEL)
      DOUBLE PRECISION, INTENT(IN) :: DTUZ
      DOUBLE PRECISION, INTENT(INOUT) :: CLAI (NV), PLAI (NV), ARXL (NLFEE)
      DOUBLE PRECISION, INTENT(INOUT) :: DRAINA (NLF + 1:NEL), HRF (NEL)
      DOUBLE PRECISION, INTENT(IN) :: PNETTO (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND (NEL), QOC (NELEE, 4)
   !
   ! Workspace arguments
      INTEGER :: IQ (NEL), JMIN (NEL), JSORT (0:NEL + 1)
      LOGICAL :: LDUM (NELEE)
   !
   ! Locals, etc
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      DOUBLE PRECISION, PARAMETER :: TOL = 1.0D-7
   !
      INTEGER :: FACE, FADJ, I, IADJ, IBR, IEL, IUNDEF, J, NELP, NERR, P
      DOUBLE PRECISION :: QADJ, QMIN
      DOUBLE PRECISION :: DUM1 (1)

   !----------------------------------------------------------------------*
   !
   ! 0. Preliminaries
   ! ----------------
   !
   !     * Initialize local counter
      NERR = 0
   !
   !
   ! 1. Variables
   ! ------------
   !
   ! DTUZ
      DUM1 (1) = DTUZ
      CALL ALCHK (ERR, 2046, SPR, 1, 1, IUNDEF, IUNDEF, 'DTUZ', 'GE', &
         zero1, zero1 (1), DUM1, NERR, LDUM)
   !
   !
   ! 2. Vegetative State
   ! -------------------
   !
   ! CLAI
      CALL ALCHK (ERR, 2047, SPR, 1, NV, IUNDEF, IUNDEF, 'CLAI(veg)', &
         'GE', zero1, zero1 (1), CLAI, NERR, LDUM)
   ! PLAI
      CALL ALCHK (ERR, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
         'GE', zero1, zero1 (1), PLAI, NERR, LDUM)
      CALL ALCHK (ERR, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
         'LE', ONE1, ZERO1 (1), PLAI, NERR, LDUM)
   !
   !
   ! 3. Link State
   ! -------------
   !
      IF (NLF > 0) THEN
   !
   ! ARXL
         CALL ALCHK (ERR, 2049, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
         'GE', zero1, zero1 (1), ARXL, NERR, LDUM)
   !
      END IF
   !
   !
   ! 4. Columnar State
   ! -----------------
   !
   ! DRAINA
      CALL ALCHK (ERR, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'GE', zero1, zero1 (1), DRAINA, NERR, LDUM)
   ! 10.10.94  Ought to fix WAT module so that we don't need TOL
      CALL ALCHK (ERR, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'LEa', PNETTO, TOL, DRAINA, NERR, LDUM)
   !
   !
   ! 5. Elemental State
   ! ------------------
   !
   ! HRF
      CALL ALCHK (ERR, 2051, SPR, 1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', &
         'GEa', ZGRUND, ZERO1 (1), HRF, NERR, LDUM)
   !
   !
   ! 6. Flux/Ordering
   ! ----------------
   !
   ! ISORT & QOC
   !     * Set JSORT = inverse of ISORT & initialize upper bound JMIN
   !       (note that JSORT has overspill elements )
      NELP = NEL + 1
      DO J = 0, NELP
         JSORT (J) = NELP
      END DO
      
      DO I = 1, NEL
         IEL = ISORT (I)
         J = MAX (0, MIN (IEL, NELP))
         JSORT (J) = I
         JMIN (I) = NELP
      END DO
      
   !     * At this point any element not listed in ISORT has a JSORT
   !       value of NELP, which is guaranteed to fail the test below
   !     * Update JMIN (used as object of JSORT test) & set QOC status IQ
      DO FACE = 1, 4
         
         element_loop: DO IEL = 1, NEL
   !          * innocent until proven guilty
            IQ (IEL) = 0
            
   !          * non-discharge faces are ok (Cycle directly replaces GOTO 640)
            IF (FNQOUT(IEL, FACE) <= ZERO1 (1)) CYCLE element_loop
            
            IADJ = ICMREF (IEL, FACE, 2)
            
            IF (IADJ > 0) THEN
               FADJ = ICMREF (IEL, FACE, 3)
               QADJ = FNQOUT(IADJ, FADJ)
   !             * do both elements discharge into the same face?
               IF (QADJ > ZERO1 (1)) IQ (IEL) = 1
   !             * IEL must precede IADJ in the ISORT list
               JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL))
               
            ELSE IF (IADJ < 0) THEN
               IBR = - IADJ
               QMIN = ONE1 (1)
               
               DO P = 1, 3
                  IADJ = ICMRF2 (IBR, P, 1)
                  IF (IADJ > 0) THEN
                     FADJ = ICMRF2 (IBR, P, 2)
                     QADJ = FNQOUT(IADJ, FADJ)
                     QMIN = MIN (QADJ, QMIN)
                     IF (QADJ < zero1 (1)) THEN
   !                      * IEL must precede IADJ in the ISORT list
                        JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL))
                     END IF
                  END IF
               END DO
               
   !             * discharge from IEL has nowhere to go?
               IF (QMIN >= zero1 (1)) IQ (IEL) = 2
            END IF
         END DO element_loop
         
   !        * Check QOC status at this FACE for all elements
         CALL ALCHKI (ERR, 2052, SPR, 1, NEL, FACE, IUNDEF, &
            'status_of_QOC(iel,face)', 'EQ', IZERO1, IQ, NERR, LDUM)
            
      END DO
      
   !     * Check that each donor element listed in ISORT occurs before
   !       each of its receptors, and that all elements are listed
      CALL ALCHKI (ERR, 2053, SPR, 1, NEL, IUNDEF, IUNDEF, &
         'position_in_ISORT(iel)', 'LTa', JMIN, JSORT (1), NERR, LDUM)
   !
   !
   ! 7. Epilogue
   ! -----------
   !
      IF (NERR > 0) THEN
   !
         WRITE (SPR, 9100) 'DTUZ', DTUZ
         WRITE (SPR, 9100) 'CLAI[veg=1,...,NV]', CLAI
         WRITE (SPR, 9100) 'PLAI[veg=1,...,NV]', PLAI
         WRITE (SPR, 9100) 'ARXL[link=1,...,NLF]', (ARXL (IEL), IEL = 1, NLF)
         WRITE (SPR, 9100) 'DRAINA[col=NLF+1,...,NEL]', DRAINA
         WRITE (SPR, 9100) 'PNETTO[col=NLF+1,...,NEL]', PNETTO
         WRITE (SPR, 9100) 'HRF[iel=1,...,NEL]', (HRF(IEL), IEL = 1, NEL)
         WRITE (SPR, 9100) 'ZGRUND[iel=1,...,NEL]', ZGRUND
         WRITE (SPR, 9200) 'ISORT[iel=1,...,NEL]', ISORT
         WRITE (SPR, 9200) 'position_in_ISORT[iel=1,...,NEL]', (JSORT (IEL), IEL = 1, NEL)
         
         DO FACE = 1, 4
            WRITE (SPR, 9150) 'QOC[iel=1,...,NEL][face=', FACE, ']', (QOC (IEL, FACE), IEL = 1, NEL)
         END DO
   !
         CALL ERROR (ERR, 2003, SPR, 0, 0, 'Error(s) detected while checking time-dependent WAT-SY interface')
   !
      END IF

      RETURN

      ! FORMAT STATEMENTS safely at the bottom
9100  FORMAT(1X,A,     ':'/1P,(8E10.2))
9150  FORMAT(1X,A,I1,A,':'/1P,(8E10.2))
9200  FORMAT(1X,A,     ':'/   (16I5  ))

   CONTAINS

      ! Modern replacement for the legacy Statement Function
      PURE DOUBLE PRECISION FUNCTION FNQOUT(ELEM, FCE)
         INTEGER, INTENT(IN) :: ELEM, FCE
         ! Calculates Water Discharge Rate
         FNQOUT = SIGN(1.0D0, 2.0D0 - DBLE(FCE)) * QOC(ELEM, FCE)
      END FUNCTION FNQOUT

   END SUBROUTINE SYERR3



   !SSSSSS SUBROUTINE SYFINE
   !----------------------------------------------------------------------*
   ! Evaluate quantities specific to fine sediment particles, associated
   !  with settling, infiltration and armouring.
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR70
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  15.07.94  Version 3.4.1 by AB/RAH. File created 28.3.94.
   !----------------------------------------------------------------------*
   SUBROUTINE SYFINE(DRSEDF, FBIC, FICRIT, NLF, ALPHA, DTSY, AREA, &
                     DCBF, FBETAF, FDELF, PBSED, TAUK, VCFMAX, VINFMX, BARM)

      ! Assumed module dependencies providing global variables:
      ! USE CONST_SY, ONLY : GRAVTY, RHOSED, RHOWAT, VISCOS
      ! USE SY_STATE, ONLY : FIRST_syfine, WSED_syfine
      ! USE UTILSMOD, ONLY : DIMJE

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NLF
      DOUBLE PRECISION, INTENT(IN) :: DRSEDF, FBIC, FICRIT, ALPHA, DTSY
      DOUBLE PRECISION, INTENT(IN) :: AREA(NLF), DCBF(NLF), PBSED(NLF)
      DOUBLE PRECISION, INTENT(IN) :: FBETAF(NLF), FDELF(NLF), TAUK(NLF)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: VCFMAX(NLF), VINFMX(NLF)
      LOGICAL, INTENT(OUT) :: BARM(NLF)

      ! Locals, etc
      INTEGER :: LINK
      DOUBLE PRECISION :: DUM, TAUEC, VMAX
      DOUBLE PRECISION :: AREA_L, DCFMXL, FDELFL, TAUKL

   !----------------------------------------------------------------------*

      ! * Calculate settling velocity for fines ( first call only )
      IF (FIRST_syfine) THEN
         FIRST_syfine = .FALSE.
         WSED_syfine = DRSEDF**2 * GRAVTY * (RHOSED - RHOWAT) / (18.0D0 * RHOWAT * VISCOS)
      END IF

      ! * Loop over channel links
      link_loop: DO LINK = 1, NLF

         TAUKL  = TAUK(LINK)
         AREA_L = AREA(LINK)
         FDELFL = FDELF(LINK)

         ! * Calculate critical shear stress for fines
         ! Modernization Fix: Initialize DUM to prevent passing uninitialized memory to SYCRIT INTENT(IN)
         ! DUM = 0.0D0 
         CALL SYCRIT(0, DRSEDF, TAUKL, DUM, TAUEC)

         ! * Calculate potential fines in upper layer
         ! * (existing fines + settling)
         DUM = ALPHA * TAUEC
         IF (DUM > 0.0D0) DUM = DIMJE(DUM, TAUKL) / DUM
         DCFMXL = DCBF(LINK) + FDELFL * WSED_syfine * DUM * DTSY
         VCFMAX(LINK) = DCFMXL * AREA_L

         ! * Can fines be armoured ?
         BARM(LINK) = (TAUKL <= TAUEC)

         ! * Calculate potential infiltration rate
         VMAX = 0.0D0
         IF (FBETAF(LINK) < FBIC) THEN
            VMAX = WSED_syfine * AREA_L * DIMJE(FDELFL, FICRIT / (1.0D0 - PBSED(LINK))) * DTSY
         END IF
         VINFMX(LINK) = VMAX

      END DO link_loop

   END SUBROUTINE SYFINE



   !----------------------------------------------------------------------*
   !
   !  To initialize/define, on the first SY pass, output, saved and static
   !   variables.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR61
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  24.5.94  Version 3.4.1 by AB/RAH. File creation date 23.11.93.
   !----------------------------------------------------------------------*
   SUBROUTINE SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, NTSOBK, ARXL, DCBEDO, DLS, &
                      FBETA, DRSED, HRF, PBSED, PLS, SOSDFN, THSAT, ZGRUND, NTSOTP, ZBFULL, ARBDEP, &
                      ARXLOL, DCBED, DCBSED, DDBSED, DRSO50, DWATOL, FETA, GINFD, GINFS, GNU, GNUBK, &
                      QSED, DBFULL)

      IMPLICIT NONE

      ! Commons and constants implicitly expected from USE or context
      ! (e.g. ZERO, HALF, DIMJE, SYDR)
      
      ! Input arguments
      INTEGER, INTENT(IN)          :: NEL, NELEE, NLF, NLFEE, NS, NSED, NSEE, NSEDEE
      INTEGER, INTENT(IN)          :: NTSOBK (NLFEE), NTSOTP (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: DCBEDO
      DOUBLE PRECISION, INTENT(IN) :: ARXL (NLFEE), DLS (NEL), DRSED (NSED)
      DOUBLE PRECISION, INTENT(IN) :: FBETA (NELEE, NSED), HRF (NLF + 1:NEL), PBSED (NLFEE)
      DOUBLE PRECISION, INTENT(IN) :: PLS (NLF + 1:NEL), SOSDFN (NSEE, NSED), THSAT (NS)
      DOUBLE PRECISION, INTENT(IN) :: ZBFULL (NLFEE), ZGRUND (NEL)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: ARBDEP (NLFEE), ARXLOL (NLFEE), DBFULL (NLFEE)
      DOUBLE PRECISION, INTENT(OUT) :: DCBED (NLFEE), DCBSED (NLFEE, NSED)
      DOUBLE PRECISION, INTENT(OUT) :: DDBSED (NLFEE, NSED), DRSO50 (NS), DWATOL (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(OUT) :: FETA (NEL), GINFD (NLFEE, NSED), GINFS (NLFEE, NSED)
      DOUBLE PRECISION, INTENT(OUT) :: GNU (NLF + 1:NEL), GNUBK (NLFEE)
      DOUBLE PRECISION, INTENT(OUT) :: QSED (NELEE, NSEDEE, 4)

      ! Locals, etc
      DOUBLE PRECISION :: DCBEDE, DDBEDE, DLSE, FBETAE
      INTEGER          :: IEL, LINK, SED, SOIL, FACE

      ! External functions implicitly called
      ! DOUBLE PRECISION :: DIMJE, SYDR

      !----------------------------------------------------------------------*

      ! * Initialize surface erosion rates in each column (Replaced ALINIT)
      GNU (NLF + 1 : NEL) = ZERO

      IF (NLF > 0) THEN
         ! * Initialize bank erosion rates in each link (Replaced ALINIT)
         GNUBK(1:NLF) = ZERO

         ! * Zero bed sediment accumulator (Replaced ALINIT)
         ARBDEP(1:NLF) = ZERO

         ! * Set old river c/s area equal to current river c/s area
         CALL DCOPY (NLF, ARXL, 1, ARXLOL, 1)
      END IF


      ! * Loop over sediment types
      DO SED = 1, NSED

         IF (NLF > 0) THEN
            ! * Initialize infiltration rates (Replaced ALINIT)
            GINFD(1:NLF, SED) = ZERO
            GINFS(1:NLF, SED) = ZERO
         END IF

         ! * Initialize sediment flow rates (Replaced ALINIT)
         DO FACE = 1, 4
            QSED(1:NEL, SED, FACE) = ZERO
         END DO

      ! * Next sediment type
      END DO


      ! * Loop over links
      DO LINK = 1, NLF
         DLSE = DLS (LINK)

         ! * Set ratio of bank soil to bed sediment solid volume fractions
         FETA (LINK) = (1.0D0 - THSAT (NTSOBK (LINK))) / (1.0D0 - PBSED (LINK))

         ! * Set bank full depth
         DBFULL (LINK) = ZBFULL (LINK) - ZGRUND (LINK)

         ! * Bed layer depths
         DCBEDE = MIN (DLSE, DCBEDO)
         DDBEDE = DIMJE(DLSE, DCBEDE)
         DCBED (LINK) = DCBEDE

         ! * Loop over sediment types
         DO SED = 1, NSED
            ! * Initialize sediment depths in both bed layers
            FBETAE = FBETA (LINK, SED)
            DCBSED (LINK, SED) = DCBEDE * FBETAE
            DDBSED (LINK, SED) = DDBEDE * FBETAE
         END DO

      ! * Next link
      END DO


      ! * Loop over column elements
      DO IEL = NLF + 1, NEL
         ! * Set ratio: surface soil to loose sediment solid vol fractions
         FETA (IEL) = (1.0D0 - THSAT (NTSOTP (IEL))) / (1.0D0 - PLS (IEL))

         ! * Calculate initial surface water depth
         DWATOL (IEL) = HRF (IEL) - ZGRUND (IEL)
      END DO


      ! * Calculate median particle diameter for each soil type
      DO SOIL = 1, NS
         DRSO50 (SOIL) = SYDR (HALF, NSEE, NSED, SOSDFN (SOIL, 1), DRSED)
      END DO

   END SUBROUTINE SYINIT



!SSSSSS SUBROUTINE SYLINK (NFINE, NSED, NSEDEE, DTSY, AREAE, ARXLOE, &
   SUBROUTINE SYLINK (NFINE, NSED, NSEDEE, DTSY, AREAE, ARXLOE, &
      ARXLE, CLENTE, EPSBE, PBSEDE, VINFME, BARME, VCFMAE, CONCIE, &
      DCBSEE, DDBSEE, QSDWAE, QWAT, SOSDFE, FDELE, QSEDE, DCIPRE, &
      DDIPRE, GINFDE, GINFSE)
!
!----------------------------------------------------------------------*
!
! To solve the transport equation for sediment in channel flow, for a
!  given link element.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR69
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  AB   24.5.94  Version 3.4.1 by AB/RAH. File creation date 30.3.94.
!----------------------------------------------------------------------*
!
! Input arguments
      INTEGER :: NFINE, NSED, NSEDEE
      LOGICAL :: BARME
      DOUBLEPRECISION DTSY, AREAE, ARXLOE, ARXLE, CLENTE, EPSBE, PBSEDE
      DOUBLEPRECISION CONCIE (NSED), DCBSEE (NSED), DDBSEE (NSED), &
         QWAT (4)
      DOUBLEPRECISION QSDWAE (NSEDEE, 4), SOSDFE (NSED), VCFMAE, VINFME
!
! Input/output arguments
      DOUBLEPRECISION FDELE (NSED), QSEDE (NSEDEE, 4)
!
! Output arguments
      DOUBLEPRECISION DCIPRE (NSED), DDIPRE (NSED), GINFDE (NSED)
      DOUBLEPRECISION GINFSE (NSED)
!
! Locals, etc
      INTEGER :: FACE, J (4), JI, K (4), KI, NIN, NOUT, SED
      DOUBLEPRECISION AREAEI, DCBEEE, DCIPEE, DTSYI, FDC, FDELEE, GINF
      DOUBLEPRECISION OMPB, OMPBI, QSEDIN, SUM, SUMN, SUMP
      DOUBLEPRECISION VCFS, VCARM, VDMAX, VDSEDS, VDSED, VDWAT, VINF, &
         VSTRAN
!
!
!----------------------------------------------------------------------*
!
!
! Initialization
! --------------
!
!     * Make lists of outflow and inflow faces
      NIN = 0
      NOUT = 0
      DO 100 FACE = 1, 4
         IF (QWAT (FACE) .GT.0) THEN
            NOUT = NOUT + 1
            J (NOUT) = FACE
         ELSE
            NIN = NIN + 1
            K (NIN) = FACE
         ENDIF
100   END DO
!
      SUMP = 0
      SUMN = 0
      OMPB = 1 - PBSEDE
      OMPBI = 1 / OMPB
      DTSYI = 1 / DTSY
      AREAEI = 1 / AREAE
!
!
! Loop over size groups ( largest to smallest )
! ---------------------------------------------
!
!     * Loop over sediment types ( largest to smallest )
      DO 500 SED = NSED, 1, - 1
         DCBEEE = DCBSEE (SED)
!
!
!        Water and sediment budgets
!        --------------------------
!
!        * Calculate sediment inflow rate
         SUM = 0
         DO 200 KI = 1, NIN
            SUM = SUM + QSEDE (SED, K (KI) )
200      END DO
         QSEDIN = - SUM * OMPBI
!
!        * Volume of water remaining + advective water discharge
         SUM = 0
         DO 300 JI = 1, NOUT
            SUM = SUM + QSDWAE (SED, J (JI) )
300      END DO
         VDWAT = ARXLE * CLENTE+SUM * DTSY
!
!        * Sediment available for resuspension/transport/infiltration
!        *   /armouring
         VDMAX = FDELE (SED) * ARXLOE * CLENTE+DCBEEE * AREAE+ (QSEDIN + &
            EPSBE * SOSDFE (SED) ) * DTSY
!
!
!        Infiltration and Armouring
!        --------------------------
!
!        * Sediment volumes subject to infiltration & armouring resp.
         IF (SED.GT.NFINE) THEN
!           * Non-fines
            VINF = 0
            VCARM = 0
         ELSE
!           * Fines
            VCFS = MIN (VCFMAE, VDMAX)
            VINF = MIN (VINFME, VCFS)
!           * ( SUMN/SUMP calculated below, summed over earlier passes )
            FDC = 0
            IF (BARME.AND.0.LT.SUMN) FDC = MIN (SUMN, SUMP) / SUMN
            VCARM = FDC * DIMJE(VCFS, VINF)
         ENDIF
!
!        * Volume in and above top layer after infiltration ...
         VDSEDS = DIMJE(VDMAX, VINF)
!        * ... minus armoured volume ( = SUPPLY limit for transport )
         VDSED = DIMJE(VDSEDS, VCARM)
!
!        * Infiltration rates for each layer
         GINF = VINF * DTSYI
         GINFDE (SED) = GINF
         GINFSE (SED) = GINF
!
!
!        Other output variables
!        ----------------------
!
!        * Sediment remaining in suspension + sediment discharged
!        * - limited by either SUPPLY or CAPACITY
         VSTRAN = MIN (VDSED, CONCIE (SED) * OMPBI * VDWAT)
!
!        * Concentration in suspension ('relative density')
         FDELEE = 0
         IF (VDWAT.GT.0) FDELEE = VSTRAN / VDWAT
         FDELE (SED) = FDELEE
!
!        * Interim layer depths
         DCIPEE = DIMJE(VDSEDS, VSTRAN) * AREAEI
         DCIPRE (SED) = DCIPEE
         DDIPRE (SED) = DDBSEE (SED) + VINF * AREAEI
!
!        * Particulate discharge rates at outflow faces
         DO 400 JI = 1, NOUT
            QSEDE (SED, J (JI) ) = QSDWAE (SED, J (JI) ) * FDELEE * &
               OMPB
400      END DO
!
!
!        Epilogue
!        --------
!
!        * Depth of non-fines in interim and old top layers
!        *  ( used above on final pass: definition point must be later )
!        *  ( than reference point                                     )
         SUMP = SUMP + DCIPEE
         SUMN = SUMN + DCBEEE
!
!     * Next sediment type
500   END DO
!
   END SUBROUTINE SYLINK



   !----------------------------------------------------------------------*
   !
   ! Controlling routine for the Sediment Yield module.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1          Notes:  SSR76
   !  Module:  SY             Program:  SHETRAN
   ! Modifications:
   !  RAH  04.10.94  Version 3.4.1. File created 23.12.93.
   !----------------------------------------------------------------------*
   SUBROUTINE SYMAIN (NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD, ICMBK, ICMREF, ICMRF2, ICMXY, &
                      NBFACE, NLYR, NTSOIL, NVC, AREA, CLENTH, CWIDTH, DHF, DXQQ, DYQQ, THSAT,    &
                      ZBFULL, ZGRUND, BEXBK, LINKNS, ISORT, DTUZ, TIH, UZNOW, ARXL, CLAI, DRAINA, &
                      HRF, PLAI, PNETTO, QOC, NSED, PBSED, PLS, SOSDFN, ARBDEP, DLS, FBETA, FDEL, &
                      GINFD, GINFS, GNU, GNUBK, QSED, DCBED, DCBSED, IDUM, DUMMY)

      IMPLICIT NONE

      ! Commons and distributed constants
      ! Constants referenced
      !     AL.P:  NELEE  NLFEE  NLYREE  NSEDEE  NSEE  NVEE  NXEE
      ! NB  Don't dimension arrays with NSED (undefined) or NLF (zero?)

      ! Input arguments
      INTEGER, INTENT(IN)          :: NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD
      INTEGER, INTENT(IN)          :: ICMBK (NLFEE, 2), ICMRF2 (NLFEE, 3, 2)
      INTEGER, INTENT(IN)          :: ICMXY (NXEE, NY), NBFACE (NEL)
      INTEGER, INTENT(IN)          :: NTSOIL (NEL, NLYREE)
      INTEGER, INTENT(IN)          :: ISORT (NEL)
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND (NEL)
      DOUBLE PRECISION, INTENT(IN) :: DTUZ, TIH, UZNOW
      DOUBLE PRECISION, INTENT(INOUT) :: ARXL (NLFEE), CLAI (NV), DRAINA (NLF + 1:NEL), HRF (NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: PLAI (NV)
      DOUBLE PRECISION, INTENT(IN) :: PNETTO (NLF + 1:NEL), QOC (NELEE, 4)
      LOGICAL, INTENT(IN)          :: BEXBK, LINKNS (NLFEE)

      ! Checked by SYERR1 via ALCHK/ALCHKI interfaces
      INTEGER, INTENT(INOUT)          :: ICMREF (NELEE, 4, 2:3), NLYR (NLF + 1:NEL), NVC (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: AREA (NEL), CLENTH (NLFEE), CWIDTH (NLFEE)
      DOUBLE PRECISION, INTENT(INOUT) :: DHF (NELEE, 4), DXQQ (NLF + 1:NEL), DYQQ (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: THSAT (NS), ZBFULL (NLFEE)

      ! Input/output arguments
      INTEGER, INTENT(INOUT)          :: NSED
      DOUBLE PRECISION, INTENT(INOUT) :: PBSED (NLFEE), PLS (NLF + 1:NEL), SOSDFN (NSEE, NSEDEE)
      DOUBLE PRECISION, INTENT(INOUT) :: ARBDEP (NLFEE), DLS (NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: DCBED (NLFEE), DCBSED (NLFEE, NSEDEE)
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA (NELEE, NSEDEE), FDEL (NELEE, NSEDEE)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: GINFD (NLFEE, NSEDEE), GINFS (NLFEE, NSEDEE)
      DOUBLE PRECISION, INTENT(OUT) :: GNU (NLF + 1:NEL), GNUBK (NLFEE)
      DOUBLE PRECISION, INTENT(OUT) :: QSED (NELEE, NSEDEE, 4)

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT)      :: IDUM
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY

      ! Locals, etc
      CHARACTER (LEN=*), PARAMETER :: SYVER = '4.2.7'

      INTEGER :: FACE, FADJ, I, IADJ, IB, IBR, IEL, N, P, SED, SOIL
      INTEGER :: IDUM1A (NELEE), IDUM1X (NELEE + 3)

      DOUBLE PRECISION :: DTSY
      DOUBLE PRECISION :: CONCI (NLFEE, NSEDEE), CONCIE (NSEDEE)
      DOUBLE PRECISION :: DCBSEE (NSEDEE), DCIPRE (NSEDEE)
      DOUBLE PRECISION :: DCIPRM (NLFEE, NSEDEE), DDBSEE (NSEDEE)
      DOUBLE PRECISION :: DDIPRE (NSEDEE), DDIPRM (NLFEE, NSEDEE)
      DOUBLE PRECISION :: DRDROP (NELEE), DUMSED (NLFEE * NSEDEE), DWAT1 (NELEE)
      DOUBLE PRECISION :: EPSB (NLFEE)
      DOUBLE PRECISION :: FBETAE (NSEDEE), FCC (NVEE), FDELE (NSEDEE)
      DOUBLE PRECISION :: FQCONF (NLFEE, 3), GINFDE (NSEDEE), GINFSE (NSEDEE)
      DOUBLE PRECISION :: LRAIN (NELEE)
      DOUBLE PRECISION :: QSDWAE (NSEDEE, 4), QSDWAT (NLFEE, NSEDEE, 4)
      DOUBLE PRECISION :: QSEDB (NSEDEE, NSYBEE), QSEDE (NSEDEE, 4)
      DOUBLE PRECISION :: QWAT (4), QWATB (NSYBEE)
      DOUBLE PRECISION :: SLOPEE (4), SLOPEJ (NELEE, 4), SOSDFE (NSEDEE)
      DOUBLE PRECISION :: TAUJ (NELEE, 4), TAUJE (4), TAUK (NELEE)
      DOUBLE PRECISION :: VCFMAX (NLFEE), VINFMX (NLFEE)

      LOGICAL :: DOUBT, BARM (NLFEE), LDUM (NELEE)

      !----------------------------------------------------------------------*

      PASS_symain = PASS_symain + 1
      IF (PASS_symain == 1) THEN

         ! --------------------- Initialization step ----------------------------*

         ! * Check array bounds & input variables
         CALL SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE, NV, NVEE, NX, NXEE, NY, &
                      SPR, SYD)

         ! * Check static/initializing input arrays
         CALL SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, NXEE, NYEE, NY, SPR, BEXBK,   &
                      LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, NTSOIL, NVC, THSAT, CLENTH,   &
                      CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, ARXL, HRF (NLF + 1), ZGRUND, IDUM, &
                      IDUM1X, LDUM)

         ! * Store top-layer soil type for each column element
         DO IEL = NLF + 1, NEL
            NTSOTP_symain (IEL) = NTSOIL (IEL, NLYR (IEL))
         END DO

         ! * Read SY input data file
         CALL SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, NELEE, NLF, NLFEE, NS, NSEDEE,  &
                      NSEE, NSYBEE, NSYCEE, NTSOTP_symain (NLF + 1), NV, NX, NXEE, NYEE, NY,    &
                      SPR, SYD, SYVER, ABC_symain, ALPHA_symain, BBC_symain, BKB_symain,        &
                      CONCOB_symain, DCBEDO_symain, DLS, DRDRIP_symain, DRSED_symain,           &
                      DLSMAX_symain, FBETA, FBIC_symain, FCG_symain (NLF + 1),                  &
                      FCROCK_symain (NLF + 1), FDEL, FDRIP_symain, FICRIT_symain, FPCLAY_symain,&
                      FPCRIT_symain, GBC_symain, GKF_symain, GKR_symain, ISACKW_symain,         &
                      ISGSED_symain, ISSYOK_symain, ISTEC_symain, ISUSED_symain, NEPS_symain,   &
                      NFINE_symain, NSED, NSYB_symain, NSYBCD_symain, NSYC_symain,              &
                      NTSOBK_symain, PBSED, PLS, RHOSO_symain, SOSDFN, XDRIP_symain, IDUM,      &
                      DUMMY, DUMSED)

         ! * Check SY input data
         CALL SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV,           &
                      NSYB_symain, NSYBEE, NSYC_symain, NSYCEE, SPR, ICMREF, ISUSED_symain,     &
                      NEPS_symain, NFINE_symain, SFB, SRB, ALPHA_symain, DCBEDO_symain,         &
                      FPCRIT_symain, DLSMAX_symain, NTSOBK_symain, NSYBCD_symain, NBFACE,       &
                      DRSED_symain, BKB_symain, GKF_symain, GKR_symain, RHOSO_symain, SOSDFN,   &
                      DRDRIP_symain, FDRIP_symain, XDRIP_symain, PBSED, FCG_symain (NLF + 1),   &
                      FCROCK_symain (NLF + 1), PLS, DLS, FBETA, FDEL, ABC_symain, BBC_symain,   &
                      GBC_symain, IDUM, DUMMY, LDUM)

         ! * Static variables and initialization
         CALL SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, NTSOBK_symain, ARXL,      &
                      DCBEDO_symain, DLS, FBETA, DRSED_symain, HRF (NLF + 1), PBSED, PLS,       &
                      SOSDFN, THSAT, ZGRUND, NTSOTP_symain (NLF + 1), ZBFULL, ARBDEP,           &
                      ARXLOL_symain, DCBED, DCBSED, DDBSED_symain, DRSO50_symain,               &
                      DWATOL_symain (NLF + 1), FETA_symain, GINFD, GINFS, GNU, GNUBK, QSED,     &
                      DBFULL_symain)

         !------------------- End of initialization step -----------------------*

      ELSE
         !---------------------- Simulation step -------------------------------*

         ! Check Input
         ! -----------
         ! * Check time-varying input variables
         DOUBT = ISSYOK_symain > 0
         IF (DOUBT) DOUBT = MOD (PASS_symain - 2, ISSYOK_symain) == 0
         
         IF (DOUBT) THEN
            CALL SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, ICMRF2, ISORT, DTUZ, CLAI,    &
                         PLAI, ARXL, DRAINA, PNETTO, HRF, ZGRUND, QOC, IDUM, IDUM1A, IDUM1X,    &
                         LDUM)
         END IF

         ! Quantities Independent of Sub-timestep
         ! --------------------------------------
         ! * Water-flow related variables
         CALL SYWAT (NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, DHF, DRDRIP_symain,       &
                     LINKNS, ZBFULL, ZGRUND, CLAI, DRAINA, HRF, PLAI, PNETTO, QOC,              &
                     DRDROP (NLF + 1), DWAT1, FCC, FQCONF, LRAIN (NLF + 1), SLOPEJ, TAUJ, TAUK)

         ! * Erosion rates for all column elements
         CALL SYOVER (ISTEC_symain, NEL, NLF, NS, NV, FCC, LRAIN (NLF + 1), XDRIP_symain,       &
                      DRDRIP_symain, FDRIP_symain, DRAINA, GKR_symain, DWAT1 (NLF + 1),         &
                      DRDROP (NLF + 1), FCG_symain (NLF + 1), FCROCK_symain (NLF + 1),          &
                      DRSO50_symain, TAUK (NLF + 1), FPCLAY_symain, GKF_symain, RHOSO_symain,   &
                      NTSOTP_symain (NLF + 1), NVC, GNU, DUMMY, DLS, DLSMAX_symain)

         ! * Erosion rates for all link elements
         IF (NLF > 0) THEN
            CALL SYBKER (ISTEC_symain, NLF, NS, FPCLAY_symain, RHOSO_symain, DRSO50_symain,     &
                         TAUK, CWIDTH, DWAT1, BKB_symain, NTSOBK_symain, FETA_symain, CLENTH,   &
                         DBFULL_symain, EPSB, GNUBK)
         END IF


         ! SY Sub-timestep Loop
         ! --------------------
         DTSY = DTUZ / NEPS_symain
         DO N = 1, NEPS_symain

            ! Initialization
            ! --------------
            ! Replaced ALINIT with array slices
            DO FACE = 1, 4
               DO SED = 1, NSED
                  QSED (1:NEL, SED, FACE) = ZERO
               END DO
            END DO

            ! Boundary Conditions
            ! -------------------
            
            IF (NSYB_symain > 0) THEN

               ! * Gather water "outflow" rates (should be negative)
               DO IB = 1, NSYB_symain
                  IEL = NSYBCD_symain (IB, 1)
                  FACE = NBFACE (IEL)
                  QWATB (IB) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
               END DO

               ! * Read time-varying flux data & calculate sediment flows
               CALL SYBC

               ! * Load boundary flows into QSED array
               DO IB = 1, NSYB_symain
                  IEL = NSYBCD_symain (IB, 1)
                  FACE = NBFACE (IEL)
                  CALL DCOPY (NSED, QSEDB (1, IB), 1, QSED (IEL, 1, FACE), NELEE)
               END DO

            END IF

            ! Quantities Independent of Sediment Flux
            ! ---------------------------------------
            IF (NLF > 0) THEN
               ! * Transport capacity & advection coefficients
               CALL SYCLTR (CONCOB_symain, FPCRIT_symain, ISACKW_symain, ISUSED_symain, NELEE,  &
                            NFINE_symain, NLF, NLFEE, NSED, NSEDEE,                             &
                            DRSED_symain (NFINE_symain + 1), ARXL, CWIDTH, DCBED, LINKNS, DWAT1,&
                            QOC, SLOPEJ, DCBSED (1, NFINE_symain + 1),                          &
                            FDEL (1, NFINE_symain + 1), TAUJ, ACKW_symain (1, NFINE_symain + 1),&
                            CONCI, QSDWAT, DUMMY, DUMSED)

               ! * Settling, infiltration & armouring
               IF (NFINE_symain > 0) THEN
                  CALL SYFINE (DRSED_symain (1), FBIC_symain, FICRIT_symain, NLF, ALPHA_symain, &
                               DTSY, AREA, DCBSED, FBETA, FDEL, PBSED, TAUK, VCFMAX, VINFMX,    &
                               BARM)
               END IF
            END IF

            ! One Element at a Time
            ! ---------------------
            DO I = 1, NEL
               IEL = ISORT (I)

               ! * Gather common sub-arrays
               CALL DCOPY (NSED, FDEL (IEL, 1), NELEE, FDELE, 1)
               DO FACE = 1, 4
                  QWAT (FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
                  QSEDE (1:NSED, FACE) = QSED (IEL, 1:NSED, FACE)
               END DO

               IF (IEL <= NLF) THEN
                  ! ** Link element **
                  ! * Gather link-specific sub-arrays
                  SOIL = NTSOBK_symain (IEL)
                  CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)
                  CALL DCOPY (NSED, CONCI (IEL, 1), NLFEE, CONCIE, 1)
                  CALL DCOPY (NSED, DCBSED (IEL, 1), NLFEE, DCBSEE, 1)
                  CALL DCOPY (NSED, DDBSED_symain (IEL, 1), NLFEE, DDBSEE, 1)
                  DO FACE = 1, 4
                     CALL DCOPY (NSED, QSDWAT (IEL, 1, FACE), NLFEE, QSDWAE (1, FACE), 1)
                  END DO

                  ! * Solve transport equation
                  CALL SYLINK (NFINE_symain, NSED, NSEDEE, DTSY, AREA (IEL),                    &
                               ARXLOL_symain (IEL), ARXL (IEL), CLENTH (IEL), EPSB (IEL),       &
                               PBSED (IEL), VINFMX (IEL), BARM (IEL), VCFMAX (IEL), CONCIE,     &
                               DCBSEE, DDBSEE, QSDWAE, QWAT, SOSDFE, FDELE, QSEDE, DCIPRE,      &
                               DDIPRE, GINFDE, GINFSE)

                  ! * Scatter link-specific results
                  CALL DCOPY (NSED, DCIPRE, 1, DCIPRM (IEL, 1), NLFEE)
                  CALL DCOPY (NSED, DDIPRE, 1, DDIPRM (IEL, 1), NLFEE)
                  CALL DCOPY (NSED, GINFDE, 1, GINFD (IEL, 1), NLFEE)
                  CALL DCOPY (NSED, GINFSE, 1, GINFS (IEL, 1), NLFEE)

               ELSE
                  ! ** Column element **
                  ! * Gather column-specific sub-arrays
                  SOIL = NTSOTP_symain (IEL)
                  CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)
                  CALL DCOPY (NSED, FBETA (IEL, 1), NELEE, FBETAE, 1)
                  CALL DCOPY (4, SLOPEJ (IEL, 1), NELEE, SLOPEE, 1)
                  CALL DCOPY (4, TAUJ (IEL, 1), NELEE, TAUJE, 1)

                  ! * Solve transport equation for this column element
                  CALL SYCOLM (AREA (IEL), DTSY, DWAT1 (IEL), DWATOL_symain (IEL), DXQQ (IEL),  &
                               DYQQ (IEL), FETA_symain (IEL), GNU (IEL), ISGSED_symain, NSED,   &
                               FPCRIT_symain, PLS (IEL), NSEDEE, DRSED_symain, QWAT, SLOPEE,    &
                               SOSDFE, TAUJE, DLS (IEL), FBETAE, FDELE, QSEDE, DUMMY, DUMSED)

                  ! * Scatter column-specific results
                  CALL DCOPY (NSED, FBETAE, 1, FBETA (IEL, 1), NELEE)
               END IF

               ! * Scatter common results ...
               CALL DCOPY (NSED, FDELE, 1, FDEL (IEL, 1), NELEE)
               DO FACE = 1, 4
                  CALL DCOPY (NSED, QSEDE (1, FACE), 1, QSED (IEL, 1, FACE), NELEE)

                  ! ... and propagate sediment flow rates at outflow faces
                  IF (QWAT (FACE) > ZERO) THEN
                     IADJ = ICMREF (IEL, FACE, 2)
                     
                     IF (IADJ > 0) THEN
                        ! * regular neighbour
                        FADJ = ICMREF (IEL, FACE, 3)
                        DO SED = 1, NSED
                           QSED (IADJ, SED, FADJ) = -QSEDE (SED, FACE)
                        END DO
                        
                     ELSE IF (IADJ < 0) THEN
                        ! * neighbour is a confluence node
                        IBR = -IADJ
                        DO P = 1, 3
                           IADJ = ICMRF2 (IBR, P, 1)
                           IF (IADJ > 0) THEN
                              ! * prospect is active
                              FADJ = ICMRF2 (IBR, P, 2)
                              DO SED = 1, NSED
                                 QSED (IADJ, SED, FADJ) = QSED (IADJ, SED, FADJ) - &
                                                          QSEDE (SED, FACE) * FQCONF (IBR, P)
                              END DO
                           END IF
                        END DO
                     END IF
                     
                  END IF
               END DO

            END DO

            ! Channel Bed Update
            ! ------------------
            IF (NLF > 0) THEN
               CALL SYBED (DCBEDO_symain, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, DDIPRM,      &
                           ARBDEP, DLS, FBETA, DCBSED, DDBSED_symain, DCBED)
            END IF

            ! Store Old-time Values & Update Timer
            ! ------------------------------------
            CALL DCOPY (NEL - NLF, DWAT1 (NLF + 1), 1, DWATOL_symain (NLF + 1), 1)
            IF (NLF > 0) CALL DCOPY (NLF, ARXL, 1, ARXLOL_symain, 1)
            
            SYNOW_symain = SYNOW_symain + DTSY / 3600.0D0

         END DO

         !--------------------- End of simulation step -------------------------*
      END IF

      ! Epilogue
      ! --------
      ! Ensure that current time value is exactly correct
      SYNOW_symain = UZNOW

   END SUBROUTINE SYMAIN



   !SSSSSS SUBROUTINE SYOVER
   SUBROUTINE SYOVER (ISTEC, NEL, NLF, NS, NV, FCC, LRAIN, XDRIP, &
         DRDRIP, FDRIP, DRAINA, GKR, DWAT1, DRDROP, FCG, FCROCK, DRSO50, &
         TAUK, FPCLAY, GKF, RHOSO, NTSOTP, NVC, GNU, TGMD, DLS, DLSMAX)
   !
   !----------------------------------------------------------------------*
   ! Calculate ground surface erosion for each column element.
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR64
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  04.10.94  Version 3.4.1 by AB/RAH. File created 24.09.93.
   !  BTL  25.04.95  Version 3.4.1 : DLS and DLSMAX introduced to routine
   !                       erosion rates zero if DLS>=DLSMAX
   !----------------------------------------------------------------------*
   ! Commons and distributed constants
   !
   ! Constants referenced
   !     CONST.SY:  GRAVTY, RHOWAT, ZERO, ONE
   !
      IMPLICIT NONE

   ! Input/Output arguments
      INTEGER, INTENT(IN) :: ISTEC, NEL, NLF, NS, NV
      INTEGER, INTENT(IN) :: NTSOTP (NLF + 1:NEL), NVC (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: FCC (NV), LRAIN (NLF + 1:NEL), XDRIP (NV)
      DOUBLE PRECISION, INTENT(IN) :: DRDRIP (NV), FDRIP (NV), DRAINA (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: GKR (NS), DWAT1 (NLF + 1:NEL), DRDROP (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: FCG (NLF + 1:NEL), FCROCK (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: DRSO50 (NS), TAUK (NLF + 1:NEL), FPCLAY (NS)
      DOUBLE PRECISION, INTENT(IN) :: GKF (NS), RHOSO (NS), DLS (NEL), DLSMAX
      DOUBLE PRECISION, INTENT(OUT) :: GNU (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(OUT) :: TGMD (NV)

   ! Locals
      DOUBLE PRECISION, PARAMETER :: X1 = 7.5D0, D1 = 3.3D-3, L1 = 2.78D-6, L2 = 1.39D-5
      DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979323846D0
      DOUBLE PRECISION, PARAMETER :: CLALIM = 1.0D0 / L2

      INTEGER :: ISCD, IEL, ISGMR, ISOIL, NVEG
      DOUBLE PRECISION :: CD, FCROCE, DRDRPE, DR, DF
      DOUBLE PRECISION :: LRAINE, GMD, GMR, PRSGOS, TAUEC, TAUKE, XDRIPE

      DOUBLE PRECISION, PARAMETER :: AD(4)  = [3214.9D0, 583.4D0, 133.1D0, 29.9D0]
      DOUBLE PRECISION, PARAMETER :: BD(4)  = [1.6896D0, 1.5545D0, 1.4242D0, 1.2821D0]
      DOUBLE PRECISION, PARAMETER :: ADD(4) = [0.0D0, 0.0D0, 1.93D0, 5.14D0]
      DOUBLE PRECISION, PARAMETER :: BDD(4) = [2200.0D0, 2200.0D0, 1640.0D0, 660.0D0]

      ! Legacy branchless statement function
      DOUBLE PRECISION :: SF2, SX, SY
      SF2(SX, SY) = HALF + SIGN(HALF, SX - SY)

   !----------------------------------------------------------------------*

      PRSGOS = PI * RHOWAT * RHOWAT * GRAVTY / 6.0D0

      DO NVEG = 1, NV
         XDRIPE = XDRIP(NVEG)
         DRDRPE = DRDRIP(NVEG)
         
         ! Performance Reversion: Branchless execution
         ISCD = 1 + NINT(SF2(XDRIPE, X1) + 2.0D0 * SF2(DRDRPE, D1))

         CD = ADD(ISCD) + DRDRPE * BDD(ISCD)
         TGMD(NVEG) = PRSGOS * CD * (ONE - EXP(-2.0D0 * XDRIPE / CD)) * (DRDRPE**3) * FDRIP(NVEG)
      END DO

      DO IEL = NLF + 1, NEL
         ISOIL = NTSOTP(IEL)
         NVEG = NVC(IEL)
         LRAINE = LRAIN(IEL)
         FCROCE = FCROCK(IEL)
         TAUKE = TAUK(IEL)

         ! Performance Reversion: Branchless execution
         ISGMR = MIN(4, 1 + NINT(SF2(LRAINE, L1)) + INT(LRAINE * CLALIM))

         GMR = (ONE - FCC(NVEG)) * AD(ISGMR) * (LRAINE**BD(ISGMR))
         GMD = TGMD(NVEG) * DRAINA(IEL)

         DR = GKR(ISOIL) * EXP(-MAX(ZERO, (DWAT1(IEL) / DRDROP(IEL)) - ONE)) * &
              (ONE - FCG(IEL) - FCROCE) * (GMR + GMD)

         CALL SYCRIT (ISTEC, DRSO50(ISOIL), TAUKE, FPCLAY(ISOIL), TAUEC)

         DF = GKF(ISOIL) * (ONE - FCROCE) * MAX(ZERO, TAUKE - TAUEC) / TAUEC

         IF (DLS(IEL) < DLSMAX) THEN
            GNU(IEL) = (DR + DF) / RHOSO(ISOIL)
         ELSE
            GNU(IEL) = ZERO
         END IF
      END DO

   END SUBROUTINE SYOVER



   !SSSSSS SUBROUTINE SYOVTR
   PURE SUBROUTINE SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, &
         DRSED, QWAT, SLOPEE, TAUJE, GJSUM)
   !
   !----------------------------------------------------------------------*
   !
   !  Calculate the total volumetric capacity for discharge due to
   !   overland flow for the current time step and the current element.
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1       Notes:  SSR63
   !  Module:  SY        Program:  SHETRAN
   ! Modifications:
   !  RAH  15.07.94  Version 3.4.1 by AB/RAH. File created 01.11.93.
   !----------------------------------------------------------------------*

      ! Constants referenced from CONST_SY:
      ! GRAVTY, RHOSED, RHOWAT, FIRST_syovtr, K1_syovtr, K3_syovtr, K4_syovtr, HALF, ZERO

      IMPLICIT NONE

   ! Input arguments
      INTEGER, INTENT(IN) :: ISGSED, NSED
      DOUBLE PRECISION, INTENT(IN) :: DXQQE, DYQQE, DWAT1E
      DOUBLE PRECISION, INTENT(IN) :: VDSED (NSED), DRSED (NSED)
      DOUBLE PRECISION, INTENT(IN) :: QWAT (4), SLOPEE (4), TAUJE (4)

   ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: GJSUM

   ! Locals
      DOUBLE PRECISION :: K2, AJ, DRD50, FTAU, DUM, GJ, GSUM
      DOUBLE PRECISION :: LJ, TAUEC, TAUJEE
      INTEGER :: FACE, NOUT, I, J(4)
      DOUBLE PRECISION :: FLJ_ARRAY(4)

   !----------------------------------------------------------------------*
   ! Preliminaries
   ! -------------
   !

      ! Initialize variables
      GSUM = ZERO
      
      ! High-Performance Fix: Pre-calculate face lengths into an array instead of using MOD()
      FLJ_ARRAY = [DYQQE, DXQQE, DYQQE, DXQQE]

      ! Obtain median diameter of sediment available for discharge
      DRD50 = SYDR(HALF, 1, NSED, VDSED, DRSED)

      ! Count and record faces with outflow
      NOUT = 0
      DO FACE = 1, 4
         IF (QWAT(FACE) > ZERO) THEN
            NOUT = NOUT + 1
            J(NOUT) = FACE
         END IF
      END DO

   !
   ! Transport Capacity
   ! ------------------
   !
      IF (ISGSED == 1 .AND. DWAT1E > ZERO) THEN

         ! ^^^ ENGELUND-HANSEN METHOD ^^^

         ! Precalculate constant over faces (note K2 may be very small)
         K2 = SQRT(DWAT1E) * DRD50

         ! Loop over faces with outflow
         DO I = 1, NOUT
            FACE = J(I)

            ! Discharge capacity at this face
            LJ = FLJ_ARRAY(FACE)
            GJ = (K1_syovtr * QWAT(FACE)**2 * SLOPEE(FACE)**1.5D0) / (LJ * K2)

            ! Accumulated discharge capacity for this element
            GSUM = GSUM + GJ
         END DO

      ELSE IF (ISGSED == 0) THEN

         ! ^^^^^^^ YALIN METHOD ^^^^^^^^^

         ! Loop over faces with outflow
         DO I = 1, NOUT
            FACE = J(I)

            ! Get face length
            LJ = FLJ_ARRAY(FACE)

            ! Obtain critical shear stress at the ground surface
            TAUJEE = TAUJE(FACE)
            CALL SYCRIT(0, DRD50, TAUJEE, DUM, TAUEC)

            ! Calculate discharge capacity at this face
            ! High-Performance Fixes: MAX replaces DIMJE, LOG1P replaces LOG(1+X) for precision
            FTAU = MAX(ZERO, TAUJEE - TAUEC) / TAUEC
            AJ = K3_syovtr * SQRT(TAUEC / DRD50)
            GJ = K4_syovtr * SQRT(TAUJEE) * DRD50 * LJ * (FTAU - LOG(1.0D0 + AJ * FTAU) / AJ)

            ! Accumulated capacity for this element
            GSUM = GSUM + GJ
         END DO

      ELSE
         ! ^^^ Zero capacity ^^^
      END IF

      GJSUM = GSUM

   END SUBROUTINE SYOVTR



   !----------------------------------------------------------------------*
   !
   !  Read SY data input file
   !
   !----------------------------------------------------------------------*
   ! Version:  3.4.1         Notes:  SSR75
   !  Module:  SY          Program:  SHETRAN
   ! Modifications:
   !  RAH  08.06.94  Version 3.4.1 by AB/RAH. File created 09.12.93.
   !  BTL  25.04.95  Version 3.4.1 : read in DLSMAX as second item in SY12
   !----------------------------------------------------------------------*
   !
   ! NB: Don't dimension arrays with NSED (undefined) or NLF (may be 0).
   !
   SUBROUTINE SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, &
                      NSYBEE, NSYCEE, NTSOTP, NV, NX, NXEE, NYEE, NY, SPR, SYD, SYVER, ABC, ALPHA,   &
                      BBC, BKB, CONCOB, DCBEDO, DLS, DRDRIP, DRSED, DLSMAX, FBETA, FBIC, FCG,        &
                      FCROCK, FDEL, FDRIP, FICRIT, FPCLAY, FPCRIT, GBC, GKF, GKR, ISACKW, ISGSED,    &
                      ISSYOK, ISTEC, ISUSED, NEPS, NFINE, NSED, NSYB, NSYBCD, NSYC, NTSOBK, PBSED,   &
                      PLS, RHOSO, SOSDFN, XDRIP, IDUM, DUMMY, DUMSED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NEL, NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, NSYBEE, NSYCEE
      INTEGER, INTENT(IN) :: NTSOTP (NLF + 1:NEL), NV, NX, NXEE, NYEE, NY, SYD, SPR
      INTEGER, INTENT(IN) :: ICMBK (NLFEE, 2), ICMREF (NELEE, 4, 2:2), ICMXY (NXEE, NY)
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS (NLFEE)
      CHARACTER (LEN=*), INTENT(IN) :: SYVER

      ! Output arguments
      INTEGER, INTENT(OUT) :: ISACKW, ISGSED, ISSYOK, ISTEC, ISUSED, NEPS, NFINE
      INTEGER, INTENT(OUT) :: NSED, NSYB, NSYBCD (NSYBEE, 3), NSYC (4), NTSOBK (NLFEE)
      DOUBLE PRECISION, INTENT(OUT) :: ABC (NSEDEE, NSYCEE), ALPHA, BBC (NSEDEE, NSYCEE)
      DOUBLE PRECISION, INTENT(OUT) :: BKB (NS), CONCOB, DCBEDO, DRDRIP (NV), DRSED (NSEDEE)
      DOUBLE PRECISION, INTENT(OUT) :: FBIC, FDRIP (NV), FICRIT, FPCLAY (NS), FPCRIT
      DOUBLE PRECISION, INTENT(OUT) :: GBC (NSEDEE, NSYCEE), GKF (NS), GKR (NS), PBSED (NLFEE)
      DOUBLE PRECISION, INTENT(OUT) :: RHOSO (NS), SOSDFN (NSEE, NSEDEE), XDRIP (NV), DLSMAX
      
      ! INOUT Output Arrays (modified via ALALLF slices/subroutines)
      DOUBLE PRECISION, INTENT(INOUT) :: DLS (NEL), FBETA (NELEE, NSEDEE), FCG (NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: FCROCK (NLF + 1:NEL), FDEL (NELEE, NSEDEE)
      DOUBLE PRECISION, INTENT(INOUT) :: PLS (NLF + 1:NEL)

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT) :: IDUM
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY
      DOUBLE PRECISION, DIMENSION(NLFEE * NSEDEE), INTENT(INOUT) :: DUMSED

      ! Locals, etc
      INTEGER, PARAMETER :: FATAL = 1, WARN = 3

      CHARACTER(80)  :: CDUM
      CHARACTER(132) :: MSG
      CHARACTER(8)   :: SYDVER
      INTEGER :: BB, IDUM0, I0, IEL, ICAT, ITYPE, NC, NUM_CATEGORIES_TYPES, NNN, NREQ, SED, SOIL

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------
      !
      !     * Check status of data file
      CALL ALREAD (0, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)

      !     * Print SY job title
      CALL ALREAD (1, SYD, SPR, ':SY01', 1, 1, IDUM0, CDUM, IDUM, DUMMY)
      WRITE (SPR, '(/1X,A/)') CDUM

      !     * Check & print version number
      CALL ALREAD (1, SYD, SPR, ':SY02', 1, 1, IDUM0, SYDVER, IDUM, DUMMY)
      
      !     * [miss off last character to allow eg '3.4.1' is ok in '3.4.1a' ]
      IF (INDEX (SYDVER, SYVER (:LEN (SYVER) - 1) ) == 0) THEN
         WRITE (MSG, 9011) SYVER, SYDVER
         CALL ERROR (WARN, 2011, SPR, 0, 0, MSG)
      ELSE
         WRITE (SPR, '(4X,2A/)') 'SY Module Version ', SYVER
      END IF


      ! 1. Static Variables
      ! -------------------
      !
      !     * Check workspace array size: part 1
      NREQ = 8
      IF (NELEE < NREQ) THEN
         WRITE (MSG, 9005) NELEE, NREQ
         CALL ERROR (FATAL, 2005, SPR, 0, 0, MSG)
      END IF

      !     * Integer
      NNN = 5
      IF (NLF > 0) NNN = 8
      CALL ALREAD (2, SYD, SPR, ':SY11', NNN, 1, IDUM0, CDUM, IDUM, DUMMY)
      NSED = IDUM (1)
      ISGSED = IDUM (2)
      ISTEC = IDUM (3)
      ISSYOK = IDUM (4)
      NEPS = IDUM (5)
      
      IF (NLF > 0) THEN
         ISACKW = IDUM (6)
         ISUSED = IDUM (7)
         NFINE = IDUM (8)
      END IF
      
      IF (NSED < 1 .OR. NSED > NSEDEE) THEN
         WRITE (MSG, 9006) NSED, NSEDEE
         CALL ERROR (FATAL, 2006, SPR, 0, 0, MSG)
      END IF

      !     * Floating-point
      NNN = 2
      IF (NLF > 0) NNN = 7
      CALL ALREAD (3, SYD, SPR, ':SY12', NNN, 1, IDUM0, CDUM, IDUM, DUMMY)
      FPCRIT = DUMMY (1)
      DLSMAX = DUMMY (2)
      
      IF (NLF > 0) THEN
         ALPHA = DUMMY (3)
         CONCOB = DUMMY (4)
         DCBEDO = DUMMY (5)
         FBIC = DUMMY (6)
         FICRIT = DUMMY (7)
      END IF


      ! 2. Sediment, Soil & Vegetation Properties
      ! -----------------------------------------
      !
      !     * Check workspace array size: part 2
      NREQ = MAX (MAX (5, NSED) * NS, 3 * NV)
      IF (NELEE < NREQ) THEN
         WRITE (MSG, 9005) NELEE, NREQ
         CALL ERROR (FATAL, 2005, SPR, 0, 0, MSG)
      END IF

      !     * Sediment
      CALL ALREAD (3, SYD, SPR, ':SY21', NSED, 1, IDUM0, CDUM, IDUM, DRSED)

      !     * Soil
      CALL ALREAD (3, SYD, SPR, ':SY22', 5, NS, IDUM0, CDUM, IDUM, DUMMY)
      CALL DCOPY (NS, DUMMY (1), 5, GKR, 1)
      CALL DCOPY (NS, DUMMY (2), 5, GKF, 1)
      CALL DCOPY (NS, DUMMY (3), 5, RHOSO, 1)
      CALL DCOPY (NS, DUMMY (4), 5, FPCLAY, 1)
      CALL DCOPY (NS, DUMMY (5), 5, BKB, 1)

      !     * Soil composition
      CALL ALREAD (3, SYD, SPR, ':SY23', NSED, NS, IDUM0, CDUM, IDUM, DUMMY)
      
      DO SED = 1, NSED
         CALL DCOPY (NS, DUMMY (SED), NSED, SOSDFN (1, SED), 1)
      END DO

      !     * Vegetation
      CALL ALREAD (3, SYD, SPR, ':SY24', 3, NV, IDUM0, CDUM, IDUM, DUMMY)
      CALL DCOPY (NV, DUMMY (1), 3, XDRIP, 1)
      CALL DCOPY (NV, DUMMY (2), 3, DRDRIP, 1)
      CALL DCOPY (NV, DUMMY (3), 3, FDRIP, 1)


      ! 3. Link Element Properties
      ! --------------------------
      !
      IF (NLF > 0) THEN
         ! * Bank soil type
         CALL ALREAD (2, SYD, SPR, ':SY31', NLF, 1, IDUM0, CDUM, NTSOBK, DUMMY)

         ! * Porosity of bed sediment
         CALL ALREAD (3, SYD, SPR, ':SY32', NLF, 1, IDUM0, CDUM, IDUM, PBSED)
      END IF


      ! 4. Column-element Properties
      ! ----------------------------
      !
      !     * Ground cover
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY41', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
                   ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FCG, IDUM, DUMMY)

      !     * Rock cover
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY42', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
                   ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FCROCK, IDUM, DUMMY)

      !     * Porosity of loose sediment
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY43', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
                   ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, PLS, IDUM, DUMMY)


      ! 5. All-element Initialization
      ! -----------------------------
      !
      !     * Initial depth of loose/bed sediment
      CALL ALALLF (0, 1, 0, SYD, SPR, ':SY51', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
                   ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, DLS, IDUM, DUMMY)

      !     * Initial composition of loose/bed sediment ...
      CALL ALALLF (0, NSED, - 1, SYD, SPR, ':SY52', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE,   &
                   ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FBETA, IDUM, DUMMY)

      !     ... with special option to inherit composition of soil
      IF (NUM_CATEGORIES_TYPES < 0) THEN
         DO IEL = 1, NLF
            SOIL = NTSOBK (IEL)
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), NELEE)
         END DO
         
         DO IEL = NLF + 1, NEL
            SOIL = NTSOTP (IEL)
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), NELEE)
         END DO
      END IF

      !     * Initial concentrations of suspended sediment
      CALL ALALLF (0, NSED, 0, SYD, SPR, ':SY53', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE,     &
                   ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FDEL, IDUM, DUMMY)


      ! 6. Boundary Data
      ! ----------------
      !
      !     * No of inflow boundary elements & no of categories of each type
      CALL ALREAD (2, SYD, SPR, ':SY61', 5, 1, IDUM0, CDUM, IDUM, DUMMY)
      NSYB = IDUM (1)
      DO ITYPE = 1, 4
         NSYC (ITYPE) = IDUM (1 + ITYPE)
      END DO

      IF (NSYB > 0) THEN
         IF (NSYB > NSYBEE) THEN
            WRITE (MSG, 9007) NSYB, NSYBEE
            CALL ERROR (FATAL, 2007, SPR, 0, 0, MSG)
         END IF

         ! * Check workspace array size: part 3
         NREQ = MAX (3 * NSYB, NSED * NSYC (1), NSED * 2 * NSYC (3) )
         IF (NELEE < NREQ) THEN
            WRITE (MSG, 9005) NELEE, NREQ
            CALL ERROR (FATAL, 2005, SPR, 0, 0, MSG)
         END IF

         ! * Integer boundary data
         CALL ALREAD (2, SYD, SPR, ':SY62', 3, NSYB, IDUM0, CDUM, IDUM, DUMMY)
         I0 = 0
         
         DO BB = 1, NSYB
            IEL = IDUM (I0 + 1)
            ITYPE = IDUM (I0 + 2)
            ICAT = IDUM (I0 + 3)
            
            IF (ITYPE < 1 .OR. ITYPE > 4) THEN
               WRITE (MSG, 9008) BB, ITYPE
               CALL ERROR (FATAL, 2008, SPR, 0, 0, MSG)
            END IF
            
            ! * condense 4 into 2 by adding cats 2 & 4 to lists for 1 & 3
            IF (MOD (ITYPE, 2) == 0) ICAT = ICAT + NSYC (ITYPE - 1)
            NSYBCD (BB, 1) = IEL
            NSYBCD (BB, 2) = ITYPE
            NSYBCD (BB, 3) = ICAT
            I0 = I0 + 3
         END DO

         ! * Steady flux data
         NC = NSYC (1)
         IF (NC > 0) THEN
            IF (NC > NSYCEE) THEN
               WRITE (MSG, 9009) NSYC (1), NSYCEE
               CALL ERROR (FATAL, 2009, SPR, 0, 0, MSG)
            END IF
            
            CALL ALREAD (3, SYD, SPR, ':SY63', NSED, NC, IDUM0, CDUM, IDUM, DUMMY)
            DO SED = 1, NSED
               CALL DCOPY (NC, DUMMY (SED), NSED, GBC (SED, 1), NSEDEE)
            END DO
         END IF

         ! * Steady rating curve data
         NC = NSYC (3)
         IF (NC > 0) THEN
            IF (NC > NSYCEE) THEN
               WRITE (MSG, 9010) NSYC (3), NSYCEE
               CALL ERROR (FATAL, 2010, SPR, 0, 0, MSG)
            END IF
            
            CALL ALREAD (3, SYD, SPR, ':SY64', NSED * 2, NC, IDUM0, CDUM, IDUM, DUMMY)
            DO SED = 1, NSED
               CALL DCOPY (NC, DUMMY (2 * SED - 1), 2 * NSED, ABC (SED, 1), NSEDEE)
               CALL DCOPY (NC, DUMMY (2 * SED), 2 * NSED, BBC (SED, 1), NSEDEE)
            END DO
         END IF
      END IF


      ! 7. Epilogue
      ! -----------
      !
      !     * Close the data file
      CALL ALREAD ( - 1, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)

      RETURN

      ! Format Statements ----------------------------------------------------
9003  FORMAT ( 1X,A )

9005  FORMAT ('Workspace available is NELEE = ', I5, &
             '; workspace required in subroutine SYREAD is ', I6 )

9006  FORMAT ('No. of size groups NSED=', I4, &
             ' is not in range [1,NSEDEE=', I3, ']')

9007  FORMAT ('No. of boundaries NSYB=', I5, &
             ' is greater than NSYBEE=', I4, ']')

9008  FORMAT ('Boundary type NSYBCD(', I4, ',2)=', I2, &
             ' is not is the range [1,4]')

9009  FORMAT ('No. of steady flux categories NSYC(1)=', I4, &
             ' is greater than NSYCEE=', I3, ']')

9010  FORMAT ('No. of steady rating categories NSYC(3)=', I4, &
             ' is greater than NSYCEE=', I3, ']')

9011  FORMAT ('SY module is version ', A, '; SYD data file is version ', A)

   END SUBROUTINE SYREAD



!SSSSSS SUBROUTINE SYWAT (NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, &
   SUBROUTINE SYWAT (NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, &
      DHF, DRDRIP, LINKNS, ZBFULL, ZGRUND, CLAI, DRAINA, HRF, PLAI, &
      PNETTO, QOC, DRDROP, DWAT1, FCC, FQCONF, LRAIN, SLOPEJ, TAUJ, &
      TAUK)
!
!----------------------------------------------------------------------*
!
!  Calculate variables required by the SY module which are functions
!  solely of the water flow and related quantities.
!
!----------------------------------------------------------------------*
! Version:  3.4.1      Notes:    SSR53
! Module:   SY         Program:  SHETRAN
! Modifications:
!  RAH  04.10.94  Version 3.4.1 by AB/RAH.  File created 23.11.93.
!----------------------------------------------------------------------*
!
!
! Constants referenced
!     CONST.SY:  GRAVTY  RHOWAT
!
      IMPLICIT NONE

! Input arguments
! NB: Don't use NLF as array size: it may be zero
      INTEGER, INTENT(IN) :: NEL, NELEE, NLF, NLFEE, NV
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:3), ICMRF2(NLFEE, 3, 2)
      INTEGER, INTENT(IN) :: NVC(NLF + 1 : NEL)
      DOUBLE PRECISION, INTENT(IN) :: CLAI(NV), DHF(NELEE, 4), DRAINA(NLF + 1 : NEL)
      DOUBLE PRECISION, INTENT(IN) :: DRDRIP(NV), HRF(NEL), PLAI(NV), PNETTO(NLF + 1 : NEL)
      DOUBLE PRECISION, INTENT(IN) :: QOC(NELEE, 4), ZBFULL(NLFEE), ZGRUND(NEL)
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)
!
! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: DRDROP(NLF + 1 : NEL), DWAT1(NEL), FCC(NV)
      DOUBLE PRECISION, INTENT(OUT) :: FQCONF(NLFEE, 3), LRAIN(NLF + 1 : NEL)
      DOUBLE PRECISION, INTENT(OUT) :: SLOPEJ(NELEE, 4), TAUJ(NELEE, 4), TAUK(NEL)
! NB: FQCONF defined only for branches flowing INTO a node;
!     SLOPEJ & TAUJ not defined at side faces of links.
!
! Locals, etc
      DOUBLE PRECISION, PARAMETER :: DRDMIN = 1.0D-4

      DOUBLE PRECISION :: DRAINE, DWAT1E, FCCE, HRFE, PNETTE, SLOPEE, TAUJE
      DOUBLE PRECISION :: D, DA, DE, HA, HE, L
      DOUBLE PRECISION :: Q, QABS, QMAX, QOUT, QOUTX(0:3), QSUM, TAUMAX, ZBF
      INTEGER :: FACE, IADJ, IBR, ICOL, IEL, IELP
      INTEGER :: KADJ, KEL, KELP, LINK, P, PADJ, PIN, POUT, VEG
      LOGICAL :: BSIDE
!
!----------------------------------------------------------------------*

! Loop over Vegetation Types
! --------------------------
!
!     * Calculate ground fraction sheltered from rain by canopy
      ! Replaced DO 100 loop with array slicing
      FCC(1:NV) = PLAI(1:NV) * MIN(CLAI(1:NV), ONE)

!
! Loop over Column Elements
! -------------------------
!
      column_loop: DO ICOL = NLF + 1, NEL
!
!        * Avoid multiple array references
         DRAINE = DRAINA(ICOL)
         PNETTE = PNETTO(ICOL)
         VEG = NVC(ICOL)
         FCCE = FCC(VEG)
!
!        * Calculate median raindrop/leaf-drip diameter
         D = DRDMIN
         IF (PNETTE > ZERO) THEN
            D = MAX(D, DRDRIP(VEG) * (DRAINE / PNETTE), 0.01935d0 * PNETTE**0.182d0)
         END IF
         DRDROP(ICOL) = D
!
!        * Calculate rainfall rate
         L = ZERO
         IF (FCCE < ONE) L = DIMJE(PNETTE, DRAINE) / (ONE - FCCE)
         LRAIN(ICOL) = L
!
      END DO column_loop

!
! Loop over All Elements
! ----------------------
!
      element_loop: DO IEL = 1, NEL
!
!        * Avoid multiple array references
         HRFE = HRF(IEL)
!
!        * Calculate (& store) surface water depth
         DWAT1E = DIMJE(HRFE, ZGRUND(IEL))
         DWAT1(IEL) = DWAT1E
!
!        * Initialize maximum flow & shear stress
         QMAX = ZERO
         TAUMAX = ZERO
!
!        Loop over Faces ...
!        -------------------
!
!        ... of this element, in order to set FQCONF, SLOPEJ and TAUJ,
!        and to find a value for TAUK
!
         face_loop: DO FACE = 1, 4
!
!           * Not interested in link element side faces
            BSIDE = IEL <= NLF
            IF (BSIDE) BSIDE = (MOD(FACE, 2) == 1) .EQV. LINKNS(IEL)

            ! Replaced GOTO 350 with modern CYCLE
            IF (BSIDE) CYCLE face_loop
!
!           * Discharge rate
            QOUT = FQOUT(IEL, FACE)
!
!           * No-flow faces are special case
            IF (ISZERO(QOUT)) THEN
!              * (consider weirs and branch nodes for example)
               SLOPEJ(IEL, FACE) = ZERO
               TAUJ(IEL, FACE) = ZERO

               ! Replaced GOTO 350 with modern CYCLE
               CYCLE face_loop
            END IF
!
!           * Find neighbouring element, & its face (also set FQCONF)
            KEL = FACE
            IADJ = ICMREF(IEL, KEL, 2)
            IF (IADJ == 0) THEN
!              * This is a boundary face; extrapolate from behind ...
               KEL = 1 + MOD(FACE + 1, 4)
               IADJ = ICMREF(IEL, KEL, 2)
            END IF

            IF (IADJ == 0) THEN
!              * ... unless that's a boundary too; then go for slope=0
               IADJ = IEL
               KADJ = KEL
            ELSEIF (IADJ > 0) THEN
!              * Neighbour is a regular element
               KADJ = ICMREF(IEL, KEL, 3)
            ELSE
!
!              * Extra things to do if neighbour is a confluence node
!
!              * Branch index
               IBR = -IADJ
!
!              * Initialize locals for prospect-loop:
!              - gross discharge from the node
               QSUM = ZERO
!              - prospects with maximal inflow/outflow
               PIN = 0
               POUT = 0
!              - discharge from node (let this branch be prospect 0)
               QOUTX(0) = -FQOUT(IEL, KEL)
!
!              * Loop over Prospects
               DO P = 1, 3
                  IELP = ICMRF2(IBR, P, 1)
                  IF (IELP > 0) THEN
                     KELP = ICMRF2(IBR, P, 2)
                     Q = -FQOUT(IELP, KELP)
                     QSUM = QSUM + MAX(ZERO, Q)
                     IF (Q < QOUTX(PIN)) PIN = P
                     IF (Q > QOUTX(POUT)) POUT = P
                  ELSE
                     Q = ZERO
                  END IF
                  QOUTX(P) = Q
               END DO
!
!              * Redefine neighbour as link with maximal outflow ...
               PADJ = POUT
!              * ... unless node is at inflow face for this element
               IF (QOUTX(0) > ZERO) PADJ = PIN

               IF (PADJ > 0) THEN
                  IADJ = ICMRF2(IBR, PADJ, 1)
                  KADJ = ICMRF2(IBR, PADJ, 2)
               ELSE
!                  * (no obvious candidate: go for slope=0)
                  IADJ = IEL
                  KADJ = KEL
               END IF
!
!              * Calculate node outflow fractions if appropriate
               IF (QOUT > ZERO .AND. KEL == FACE) THEN
!                  * NB: Need precondition on QOC to ensure QSUM.GT.0
                  DO P = 1, 3
                     FQCONF(IBR, P) = MAX(ZERO, QOUTX(P)) / QSUM
                  END DO
               END IF
!
            END IF
!
!           * Calculate water surface slope
            HE = HRFE
            HA = HRF(IADJ)
            DE = DHF(IEL, KEL)
            DA = DHF(IADJ, KADJ)

            IF ((IEL <= NLF) .NEQV. (IADJ <= NLF)) THEN
!              * this is a bank face; use bank-full elevation as cut-off
               LINK = MIN(IEL, IADJ)
               ZBF = ZBFULL(LINK)
               IF (HE <= ZBF) THEN
                  HE = ZBF
                  DE = ZERO
               END IF
               IF (HA <= ZBF) THEN
                  HA = ZBF
                  IF (DE > ZERO) DA = ZERO
               END IF
            END IF

            SLOPEE = ABS(HE - HA) / (DE + DA)
            SLOPEJ(IEL, FACE) = SLOPEE
!
!           * Calculate flow shear stress at the ground surface
            TAUJE = RHOWAT * GRAVTY * DWAT1E * SLOPEE
            TAUJ(IEL, FACE) = TAUJE
!
!           * Find maximum flow rate so far and TAUJ for that face
            QABS = ABS(QOUT)
            IF (QABS > QMAX) THEN
               QMAX = QABS
               TAUMAX = TAUJE
            END IF
!
!        * Next face
         END DO face_loop
!
!        * Set representative shear stress equal to maximum over faces
         TAUK(IEL) = TAUMAX
!
!     * Next element
      END DO element_loop

   CONTAINS

      ! Modern internal function replacing the obsolescent statement function
      DOUBLE PRECISION FUNCTION FQOUT(IEL, FACE)
         INTEGER, INTENT(IN) :: IEL, FACE
         FQOUT = SIGN(1, 2 - FACE) * QOC(IEL, FACE)
      END FUNCTION FQOUT

   END SUBROUTINE SYWAT

END MODULE SYmod
