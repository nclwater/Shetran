MODULE sediment_erosion
   USE SGLOBAL
   USE sediment_common
   USE CONST_SY
   USE sediment_transport_capacity, ONLY : SYCRIT
   USE UTILSMOD

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: SYBKER, SYOVER

CONTAINS

!SSSSSS SUBROUTINE SYBKER (ISTEC, NLF, NS, FPCLAY, RHOSO, DRSO50, TAUK, &
   SUBROUTINE SYBKER (ISTEC, NLF, NS, FPCLAY, RHOSO, DRSO50, TAUK, &
      CWIDTH, DWAT1, BKB, NTSOBK, FETA, CLENTH, DBFULL, EPSB, GNUBK)
!
!----------------------------------------------------------------------*
!
! Calculate the rate of lateral erosion of stream banks for each link.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR66
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  RAH  16.06.94  Version 3.4.1 by AB/RAH. File creation date 28.3.94.
!----------------------------------------------------------------------*
! Input arguments
      INTEGER :: ISTEC, NLF, NS, NTSOBK (NLF)
      DOUBLEPRECISION FPCLAY (NS), RHOSO (NS), DRSO50 (NS), TAUK (NLF)
      DOUBLEPRECISION CWIDTH (NLF), DWAT1 (NLF), BKB (NS), FETA (NLF)
      DOUBLEPRECISION CLENTH (NLF), DBFULL (NLF)
!
! Output arguments
      DOUBLEPRECISION EPSB (NLF), GNUBK (NLF)
!
! Locals, etc
      DOUBLEPRECISION A1, B1, B2, B3, QUART
      PARAMETER (A1 = 0.05D0, B1 = 0.41D0, B2 = 0.22D0, B3 = 0.035D0)
      PARAMETER (QUART = 1.0d0 / 4.0D0)
!
      INTEGER :: BKSOIL, LINK
      DOUBLEPRECISION DWAT1E, GNUBKE, K, TAUEC, TAUKE, X
!
!
!----------------------------------------------------------------------*
!
!
!     * Loop over channel links
      DO 100 LINK = 1, NLF
         BKSOIL = NTSOBK (LINK)
         DWAT1E = DWAT1 (LINK)
         TAUKE = TAUK (LINK)
!
!        * Calculate aspect ratio coefficient ( see Notes )
         X = ONE / MAX (QUART, DWAT1E / CWIDTH (LINK) )
         K = A1 + B1 * MIN (X, ONE) + B2 * MIN (DIMJE(X, ONE), ONE) &
            + B3 * DIMJE(X, TWO)
!
!        * Obtain critical shear stress for bank erosion
         CALL SYCRIT (ISTEC, DRSO50 (BKSOIL), TAUKE, FPCLAY (BKSOIL), &
            TAUEC)
!
!        * Calculate bank erosion rate
         GNUBKE = BKB (BKSOIL) * DIMJE(K * TAUKE, TAUEC) / (TAUEC * &
            RHOSO (BKSOIL) )
         GNUBK (LINK) = GNUBKE
!
!        * Calculate rate of release of sediments for each link
         EPSB (LINK) = TWO * FETA (LINK) * CLENTH (LINK) * GNUBKE * MIN &
            (DWAT1E, DBFULL (LINK) )
!
!     * Next link
100   END DO
!
   END SUBROUTINE SYBKER
!SSSSSS SUBROUTINE SYOVER (ISTEC, NEL, NLF, NS, NV, FCC, LRAIN, XDRIP, &
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
!                          erosion rates zero if DLS>=DLSMAX
!----------------------------------------------------------------------*
      USE CONST_SY
! Commons and distributed constants
!
! Constants referenced
!     CONST.SY:  GRAVTY, RHOWAT
!
! Input arguments
      INTEGER :: ISTEC, NEL, NLF, NS, NV, NTSOTP (NLF + 1:NEL), NVC ( &
         NLF + 1:NEL)
      DOUBLEPRECISION FCC (NV), LRAIN (NLF + 1:NEL), XDRIP (NV), &
         DRDRIP (NV)
      DOUBLEPRECISION FDRIP (NV), DRAINA (NLF + 1:NEL), GKR (NS)
      DOUBLEPRECISION DWAT1 (NLF + 1:NEL), DRDROP (NLF + 1:NEL), &
         FCG (NLF + 1:NEL)
      DOUBLEPRECISION FCROCK (NLF + 1:NEL), DRSO50 (NS), TAUK (NLF + 1: &
         NEL)
      DOUBLEPRECISION FPCLAY (NS), GKF (NS), RHOSO (NS)
      DOUBLEPRECISION DLS (NEL), DLSMAX
!
! Output arguments
      DOUBLEPRECISION GNU (NLF + 1:NEL)
!
! Workspace arguments
      DOUBLEPRECISION TGMD (NV)
!
! Locals, etc
      DOUBLEPRECISION CLALIM, D1, L1, L2, X1
      PARAMETER (X1 = 7.5D0, D1 = 3.3D-3, L1 = 2.78D-6, L2 = 1.39D-5)
      PARAMETER (CLALIM = 1.0d0 / L2)
!
      INTEGER :: ISCD, IEL, ISGMR, ISOIL, NVEG
      DOUBLEPRECISION AD (4), ADD (4), BD (4), BDD (4), CD, FCROCE, &
         DRDRPE, DR, DF
      DOUBLEPRECISION LRAINE, GMD, GMR, PRSGOS, TAUEC, TAUKE, XDRIPE
      DOUBLEPRECISION SF2, SX, SY
!
! Define coefficients for use in calculating GMR and CD, respectively.
      DATA AD / 3214.9, 583.4, 133.1, 29.9 /, BD / 1.6896, 1.5545, &
         1.4242, 1.2821 /
!       Class 1: 0.0  <= LRAIN < L1
!       Class 2: L1   <= LRAIN < L2
!       Class 3: L2   <= LRAIN < 2*L2
!       Class 4: 2*L2 <= LRAIN
      DATA ADD / 0.0d0, 0.0d0, 1.93d0, 5.14d0 /, BDD / 2200.0d0, 2200.0d0, 1640.0d0, &
         660.0d0 /
!       Class 1: DRDRIP <  D1    XDRIP <  X1
!       Class 2: DRDRIP <  D1    XDRIP >= X1
!       Class 3: DRDRIP >= D1    XDRIP <  X1
!       Class 4: DRDRIP >= D1    XDRIP >= X1
!
!     * Define the switch function, used in calculating CD and GMR.
      SF2 (SX, SY) = HALF + SIGN (HALF, SX - SY)
!
!----------------------------------------------------------------------*
!
!     * Initialize constant
      PRSGOS = 4.0 * ATAN (ONE) * RHOWAT * RHOWAT * GRAVTY / 6.0
!
!     * Partial evaluation of GMD for each vegetation type
      DO 100 NVEG = 1, NV
         XDRIPE = XDRIP (NVEG)
         DRDRPE = DRDRIP (NVEG)
!        * Select coefficient pair for CD equation
         ISCD = 1 + NINT (SF2 (XDRIPE, X1) + 2 * SF2 (DRDRPE, D1) )
         CD = ADD (ISCD) + DRDRPE * BDD (ISCD)
!        * Need precondition on DRDRIP to ensure CD>0
         TGMD (NVEG) = PRSGOS * CD * (ONE-EXP ( - 2.0 * XDRIPE / CD) ) &
            * DRDRPE**3 * FDRIP (NVEG)
100   END DO
!
!     * Loop over all column elements
      DO 200 IEL = NLF + 1, NEL
         ISOIL = NTSOTP (IEL)
         NVEG = NVC (IEL)
         LRAINE = LRAIN (IEL)
         FCROCE = FCROCK (IEL)
         TAUKE = TAUK (IEL)
!
!        * Select coefficient pair for GMR equation
         ISGMR = MIN (4, 1 + NINT (SF2 (LRAINE, L1) ) + INT (LRAINE * &
            CLALIM) )
!        * Evaluate sq momentum of rain drops
         GMR = (ONE-FCC (NVEG) ) * AD (ISGMR) * LRAINE**BD (ISGMR)
!
!        * Evaluate sq momentum of leaf drips
         GMD = TGMD (NVEG) * DRAINA (IEL)
!
!        * Evaluate soil detatchment rate due to drips and drops
         DR = GKR (ISOIL) * EXP ( - DIMJE(DWAT1 (IEL) / DRDROP (IEL), &
            ONE) ) * (ONE-FCG (IEL) - FCROCE) * (GMR + GMD)
!
!        * Obtain critical shear stress for current element
         CALL SYCRIT (ISTEC, DRSO50 (ISOIL), TAUKE, FPCLAY (ISOIL), &
            TAUEC)
!
!        * Evaluate soil detatchment rate due to overland flow
         DF = GKF (ISOIL) * (ONE-FCROCE) * DIMJE(TAUKE, TAUEC) / TAUEC
!
!        * Evaluate rate of erosion of ground surface
         If (DLS (IEL) .lt.DLSMAX) then
            GNU (IEL) = (DR + DF) / RHOSO (ISOIL)
         else
            GNU (IEL) = zero
         endif
!
200   END DO
!
   END SUBROUTINE SYOVER

END MODULE sediment_erosion
