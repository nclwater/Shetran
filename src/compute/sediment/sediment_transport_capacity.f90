MODULE sediment_transport_capacity
   USE SGLOBAL
   USE sediment_common
   USE CONST_SY
   USE mod_load_filedata, ONLY : ALINIT
   USE UTILSMOD

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: SYACKW, SYCLTR, SYCRIT, SYDR, SYENGH

CONTAINS

!SSSSSS SUBROUTINE SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, &
   SUBROUTINE SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, &
      DRSED, ARXL, DCBSED, DWAT1, QOC, TAUJ, ACKW, GSED)
!
!----------------------------------------------------------------------*
!
! Calculates the streamwise capacity for particulate discharge for each
!  non-fine size group, for each channel link, according to the
!  Ackers-White formulae.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR54
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  RAH  15.07.94  Version 3.4.1 by AB/RAH. File creation date 12.05.94.
!----------------------------------------------------------------------*
      USE CONST_SY
!
! Commons and distributed constants
!
! Constants referenced
!    CONST.SY: GRAVTY, RHOSED, RHOWAT, VISCOS
!
! Input arguments
      INTEGER :: ISACKW, NFINE, NLF, NLFEE, NELEE, NSED
      DOUBLEPRECISION DRSED (NFINE+1:NSED), ARXL (NLF)
      DOUBLEPRECISION DWAT1 (NLF), QOC (NELEE, 4)
      DOUBLEPRECISION DCBSED (NLFEE, NFINE+1:NSED), TAUJ (NELEE, 4)

      LOGICAL :: LINKNS (NLF)
!
! In/Out arguments
!   defined locally
      DOUBLEPRECISION ACKW (5, NFINE+1:NSED)
!
! Output arguments
      DOUBLEPRECISION GSED (NLF, NFINE+1:NSED)
!
! Locals, etc
      DOUBLEPRECISION DGRSML, F16, F50, F56, F84, KRHO, THIRD
      PARAMETER (KRHO = RHOSED / RHOWAT - 1, DGRSML = 1D-4)
      PARAMETER (F16 = 0.16D0, F50 = 0.5d0, F56 = 0.56D0, F84 = 0.84D0)
      PARAMETER (THIRD = 1 / 3D0)
!
      DOUBLEPRECISION AAW, ARXLE, CAW, DAAA, DBED16, DBED50, DBED84, &
         DGR
      DOUBLEPRECISION DSED, DUM, DWAT1E, FGR, G, H10, LGR, &
         MAW
      DOUBLEPRECISION NAW, QK, UGR, USTR, UK
      INTEGER :: FACE, IEND, LINK, NFP1, NNF, SED, SGN
!
      DOUBLEPRECISION FDGR, FA
!

!
      FDGR (DUM) = MAX (ONE, MIN (K2_syackw * DUM, DGRMAX_syackw) )
      FA (DUM) = 0.14 + 0.23 / SQRT (DUM)
!
!----------------------------------------------------------------------*
!
!
!     * Initialization
      NNF = NSED-NFINE
      NFP1 = NFINE+1
      IF (FIRST_syackw) THEN
         FIRST_syackw = .FALSE.
         K2_syackw = (GRAVTY * KRHO / VISCOS**2) **THIRD
         DGRMAX_syackw = 1D1** (ONE / F56) + DGRSML
         ROOT32_syackw = SQRT (32D0)
         DO 100 SED = NFP1, NSED
            DGR = FDGR (DRSED (SED) )
            LGR = LOG10 (DGR)
            ACKW (1, SED) = DIMJE(ONE, F56 * LGR)
            IF (ISACKW.EQ.1) ACKW (2, SED) = FA (DGR)
            ACKW (3, SED) = 1.34 + 9.66 / DGR
            ACKW (4, SED) = 10** ( (2.86 - LGR) * LGR - 3.53)
            ACKW (5, SED) = ONE / SQRT (GRAVTY * KRHO * DRSED (SED) )
100      END DO
      ENDIF
!
!
!     * Zero GSED
      CALL ALINIT (ZERO, NLF * NNF, GSED)
!
!     * Loop over ends of each link
      DO 500 IEND = 1, 3, 2
         SGN = 2 - IEND
!
!        * Loop over all channel links
         DO 400 LINK = 1, NLF
!
!           * Determine face equivalent to this end, and flow rate there
            FACE = IEND
            IF (LINKNS (LINK) ) FACE = FACE+1
            QK = SGN * QOC (LINK, FACE)
!
!           * Check that this end is outflowing
            IF (QK.GT.ZERO) THEN
!
!              * Copy array elements to local variables
               ARXLE = ARXL (LINK)
               DWAT1E = DWAT1 (LINK)
               H10 = 10 * DWAT1E
!
!              * Determine shear velocity and water flow velocity
               USTR = SQRT (TAUJ (LINK, FACE) / RHOWAT)
               UK = ZERO
               IF (ARXLE.GT.ZERO) UK = QK / ARXLE
!
!
!              * Set A-W parameters for the Day modification if needed
               IF (ISACKW.EQ.2) THEN
!
                  DBED84 = SYDR (F84, NLFEE, NNF, DCBSED (LINK, NFP1), &
                     DRSED (NFP1) )
!
                  IF (DBED84.GT.ZERO) THEN
                     DBED50 = SYDR (F50, NLFEE, NNF, DCBSED (LINK, NFP1) &
                        , DRSED (NFP1) )
                     DBED16 = SYDR (F16, NLFEE, NNF, DCBSED (LINK, NFP1) &
                        , DRSED (NFP1) )
                     DAAA = 1.62 * DBED50 * (DBED16 / DBED84) **0.28
                  ELSE
                     DAAA = ZERO
                  ENDIF
!
                  DGR = FDGR (DAAA)
                  AAW = FA (DGR)
                  DO 200 SED = NFP1, NSED
                     ACKW (2, SED) = AAW * (0.6 + 0.4 * SQRT (DAAA / &
                        DRSED (SED) ) )
200               END DO
!
               ENDIF
!
!
!              * Loop over sediment types
               DO 300 SED = NFP1, NSED
!
!                 * Set A-W parameters for this Sediment size group
                  NAW = ACKW (1, SED)
                  AAW = ACKW (2, SED)
                  MAW = ACKW (3, SED)
                  CAW = ACKW (4, SED)
                  DSED = DRSED (SED)
!
!                 * Calculate particle mobility
                  UGR = ZERO
                  IF (DSED.LT.H10) UGR = UK / (ROOT32_syackw * LOG10 (H10 / &
                     DSED) )
                  FGR = ACKW (5, SED)
                  IF (NAW.GT.ZERO) FGR = FGR * USTR**NAW
                  IF (NAW.LT.ONE) FGR = FGR * UGR** (ONE-NAW)
!
!                 * Determine discharge capacity for this end
                  G = ZERO
                  IF (DWAT1E.GT.ZERO) G = DSED * (QK / DWAT1E) * CAW * &
                     DIMJE(FGR / AAW, ONE) **MAW
                  IF (NAW.GT.ZERO.AND.G.GT.ZERO) G = G * (UK / USTR) ** &
                     NAW
!
!                 * Determine the total discharge capacity of both ends
                  GSED (LINK, SED) = GSED (LINK, SED) + G
!
!              * Next sediment type
300            END DO
!
!
!           * End of outflow check
            ENDIF
!
!        * Next link
400      END DO
!
!     * Other end of link
500   END DO
!
!
   END SUBROUTINE SYACKW
!SSSSSS SUBROUTINE SYCLTR (CONCOB, FPCRIT, ISACKW, ISUSED, NELEE, NFINE, &
   SUBROUTINE SYCLTR (CONCOB, FPCRIT, ISACKW, ISUSED, NELEE, NFINE, &
      NLF, NLFEE, NSED, NSEDEE, DRSED, ARXL, CWIDTH, DCBED, LINKNS, &
      DWAT1, QOC, SLOPEJ, DCBSED, FDEL, TAUJ, ACKW, CONCI, QSDWAT, GSED, &
      QSWSUM)
!
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
      USE CONST_SY
! Commons and distributed constants
!
! Constants referenced
!     CONST.SY:  RHOWAT
!
! Input arguments
      INTEGER :: ISACKW, ISUSED, NELEE, NFINE, NLF, NLFEE, NSED, NSEDEE
      DOUBLEPRECISION CONCOB, FPCRIT, DRSED (NFINE+1:NSED), ARXL (NLF)
      DOUBLEPRECISION CWIDTH (NLF), DCBED (NLF), DWAT1 (NLF), QOC ( &
         NELEE, 4)
      DOUBLEPRECISION SLOPEJ (NELEE, 4), DCBSED (NLFEE, NFINE+1:NSED)
      DOUBLEPRECISION FDEL (NELEE, NFINE+1:NSED), TAUJ (NELEE, 4)
      LOGICAL :: LINKNS (NLF)
!
! Input/output arguments
      DOUBLEPRECISION ACKW (5, NFINE+1:NSED)
!
! Output arguments
      DOUBLEPRECISION CONCI (NLFEE, NSED), QSDWAT (NLFEE, NSEDEE, 4)
!     NB:    QSDWAT defined for outflow faces only
!
! Workspace arguments
      DOUBLEPRECISION GSED (NLF, NFINE+1:NSED), QSWSUM (NLF, NSED)
!
! Locals, etc
      DOUBLEPRECISION ZZ5
      PARAMETER (ZZ5 = 0.05D0)
!
      INTEGER :: FACE, IEND, ISIDE, LINK, NFP1, NSDWAT, SED, SGN
      DOUBLEPRECISION CONCID, DCSUM, DUM, FDSUM, FRACT, KQ, QK

      DOUBLEPRECISION TAUEC, TAUD, QSW, FRACT1, FRACT2
      LOGICAL :: BODD

!
!----------------------------------------------------------------------*
!
!
! Initialization
! --------------
!
      IF (FIRST_sycltr) THEN
         FIRST_sycltr = .FALSE.
         K1_sycltr = 8.5 / SQRT (RHOWAT)
      ENDIF
      NFP1 = NFINE+1
      CALL ALINIT (ZERO, NSED * NLF, QSWSUM)
!
!
! Streamwise capacity discharge rates ...
! ---------------------------------------
!
!     ... using specified method
!
      IF (ISACKW.EQ.1.OR.ISACKW.EQ.2) THEN
         CALL SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, &
            DRSED, ARXL, DCBSED, DWAT1, QOC, TAUJ, ACKW, GSED)
      ELSE
         CALL SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, &
            QOC, LINKNS, SLOPEJ, GSED)
      ENDIF
!
!
! Advection Coefficients (outflow faces only) Part 1 ...
! ------------------------------------------------------
!
!     ... for size groups which move with water velocity
!
      NSDWAT = NFINE
      IF (ISUSED.EQ.0) NSDWAT = NSED
      IF (NSDWAT.GT.0) THEN
!
!        * All faces (both ends and sides)
         DO 320 FACE = 1, 4
            SGN = SIGN (1, 2 - FACE)
            BODD = MOD (FACE, 2) .EQ.1
!
!           * All links (but skip over non-outflow faces)
            DO 310 LINK = 1, NLF
               QK = SGN * QOC (LINK, FACE)
               IF (GTZERO(QK)) THEN
!
!                 * Set QSWSUM increment for link ends only
                  QSW = ZERO
                  IF (BODD.NEQV.LINKNS (LINK) ) QSW = QK
!
!                 * Fines only, or all size groups, as appropriate
                  DO 300 SED = 1, NSDWAT
                     QSDWAT (LINK, SED, FACE) = QK
!                    * Don't actually need QSWSUM for fines, but ...
                     QSWSUM (LINK, SED) = QSWSUM (LINK, SED) + QSW
300               END DO
!
               ENDIF
310         END DO
!
320      END DO
!
      ENDIF
!
!
! Advection Coefficients (outflow faces only)  Part 2 ...
! -------------------------------------------------------
!
!     ... at link ends for each size group which moves with an
!         independent velocity.
!
      IF (ISUSED.EQ.1) THEN
!
!        * Loop over both ends ( of every link )
         DO 420 IEND = 1, 3, 2
            SGN = 2 - IEND
!
!           * Loop over every link (but skip over non-outflow faces)
            DO 410 LINK = 1, NLF
               FACE = IEND
               IF (LINKNS (LINK) ) FACE = FACE+1
               QK = SGN * QOC (LINK, FACE)
               IF (GTZERO(QK)) THEN
!
                  TAUD = TAUJ (LINK, FACE)
                  KQ = K1_sycltr * ARXL (LINK)
!
!                 * Loop over non-fine size groups
                  DO 400 SED = NFP1, NSED
                     CALL SYCRIT (0, DRSED (SED), TAUD, DUM, TAUEC)
                     QSW = MIN (KQ * SQRT (DIMJE(TAUD, SQRT (TAUD * &
                        TAUEC) ) ), QK)
                     QSDWAT (LINK, SED, FACE) = QSW
                     QSWSUM (LINK, SED) = QSWSUM (LINK, SED) + QSW
400               END DO
!
               ENDIF
!
!           * Next link
410         END DO
!
!        * Next end
420      END DO
!
      ENDIF
!
!
! Determine notional particle concentrations at flow capacity
! -----------------------------------------------------------
!
!     * Loop over fines
      DO 500 SED = 1, NFINE
         CALL ALINIT (FPCRIT, NLF, CONCI (1, SED) )
500   END DO
!
!     * Loop over links
      DO 530 LINK = 1, NLF
!
!        * Determine denominators for scaling factors
         FDSUM = ZERO
         DO 510 SED = NFP1, NSED
            FDSUM = FDSUM + FDEL (LINK, SED)
510      END DO
         IF (ISZERO(FDSUM)) FDSUM = ONE
         DCSUM = DCBED (LINK)
         IF (ISZERO(DCSUM)) DCSUM = ONE
!
!        * Loop over non-fines
         DO 520 SED = NFP1, NSED
            QSW = QSWSUM (LINK, SED)
            IF (GTZERO(QSW)) THEN
               FRACT1 = FDEL (LINK, SED) / FDSUM
               FRACT2 = DCBSED (LINK, SED) / DCSUM
               FRACT = MAX (ZZ5, FRACT1, FRACT2)
               CONCID = MIN (FPCRIT, FRACT * GSED (LINK, SED) / QSW)
            ELSE
               CONCID = ZERO
            ENDIF
            CONCI (LINK, SED) = CONCID
520      END DO
!
530   END DO
!
!
! Advection Coefficients (outflow faces only) Part 3 ...
! ------------------------------------------------------
!
!     ... at link sides, for each size group which moves at an
!         independent velocity.
!
      IF (ISUSED.EQ.1) THEN
!
!        * Loop over both sides
         DO 620 ISIDE = 2, 4, 2
            SGN = 3 - ISIDE
!
!           * Loop over every link (but skip over non-outflow sides)
            DO 610 LINK = 1, NLF
               FACE = ISIDE
               IF (LINKNS (LINK) ) FACE = ISIDE-1
               QK = SGN * QOC (LINK, FACE)
               IF (GTZERO(QK)) THEN
!
!                 * Loop over non-fine size groups
                  DO 600 SED = NFP1, NSED
                     DUM = CONCI (LINK, SED)
                     IF (GTZERO(DUM)) DUM = QK * DIMJE(DUM, CONCOB) &
                        / DUM
                     QSDWAT (LINK, SED, FACE) = DUM
600               END DO
!
               ENDIF
!
!           * Next link
610         END DO
!
!        * Next side
620      END DO
!
      ENDIF
!
   END SUBROUTINE SYCLTR
!SSSSSS SUBROUTINE SYCRIT (FLAG, DRX50, TAUX, FPCLAE, TAUEC)
   SUBROUTINE SYCRIT (FLAG, DRX50, TAUX, FPCLAE, TAUEC)
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
!
! Commons and distributed constants
      USE CONST_SY
!
! Constants referenced
!     CONST.SY:  GRAVTY, RHOSED, RHOWAT, VISCOS
!
! Input arguments
      INTEGER :: FLAG
      DOUBLEPRECISION DRX50, TAUX, FPCLAE
!
! Output arguments
      DOUBLEPRECISION TAUEC
!
! Locals, etc
      DOUBLEPRECISION R0, R1, R2, R3, R4, R5
      PARAMETER (R0 = 3D-2, R1 = 1D0)
      PARAMETER (R2 = 6D0, R3 = 30D0, R4 = 135D0, R5 = 4D2)
!
      INTEGER :: IS
      DOUBLEPRECISION AEC (5), BEC (5), RSTR, R, SF
!
!     * Define constants for use in calculating TAUEC.
      DATA AEC / 0.1d0, 0.1d0, 0.033d0, 0.013d0, 0.03d0 /
      DATA BEC / - 0.3d0, - 0.62d0, 0.0d0, 0.28d0, 0.1d0 /
!
!     * Note, Classes for RSTR :-
!     *    Class i applies to R(i-1) < RSTR <= Ri
!     *    Class 1 ALSO includes RSTR = R0
!     *    RSTR is truncated to lie in the range [R0,R5]
!     *
!
!     * Define switch function, used to determine class for AEC and BEC.
      SF (RSTR, R) = HALF - SIGN (HALF, R - RSTR)
!
!----------------------------------------------------------------------*
!
!     * Calculate constants during first call to this routine
      IF (FIRST_sycrit) THEN
         K1_sycrit = 1.0 / (SQRT (RHOWAT) * VISCOS)
         K2_sycrit = (RHOSED-RHOWAT) * GRAVTY
         K3_sycrit = 1.83d0 * LOG (10.0d0)
         FIRST_sycrit = .FALSE.
      ENDIF
!
!     * Choose method of calculating TAUEC
      IF (FLAG.EQ.1) THEN
!
!
!        * Quick method
!
         TAUEC = 0.493d0 * EXP (K3_sycrit * FPCLAE)
!
      ELSE
!
!
!        * Shields method
!
!        * Calculate Particle Reynolds Number
         RSTR = MAX (R0, MIN (DRX50 * SQRT (TAUX) * K1_sycrit, R5) )
!
!        * Select coefficient pair for calculating TAUEC
         IS = NINT (ONE+SF (RSTR, R1) + SF (RSTR, R2) + SF (RSTR, R3) &
            + SF (RSTR, R4) )
!
!        * Calculate Critical Shear Stress
         TAUEC = AEC (IS) * K2_sycrit * DRX50 * RSTR**BEC (IS)
!
      ENDIF
!
!
   END SUBROUTINE SYCRIT
!FFFFFF DOUBLEPRECISION FUNCTION SYDR
   DOUBLEPRECISION FUNCTION SYDR (FSED, INCF, N, F, D)
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
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  RAH  21.5.94  Version 3.4.1 by AB/RAH. File creation date 01.11.93.
!----------------------------------------------------------------------*
!
! Input arguments
      INTEGER :: INCF, N
      DOUBLEPRECISION FSED, F (1 + (N - 1) * INCF), D (N)

!
! Locals, etc
      DOUBLEPRECISION ALMOST
      PARAMETER (ALMOST = 0.9999D0)
!
      DOUBLEPRECISION DR, DRHI, DRLO, F02, FLO, FHI, FSUM2, FTOT
      INTEGER :: FRPTR, SED
!
!----------------------------------------------------------------------*
!
!     * Initialize local variables
      FHI = 0
      DRHI = 0
      FSUM2 = 0
      FTOT = 0
      FRPTR = 1
!
!     * Double the selected 'percentile' (actually a fraction 0-1)
!     *  and scale it relative to the sum of distribution ratios
      DO 100 SED = 1, N
         FTOT = FTOT + F (FRPTR)
         FRPTR = FRPTR + INCF
100   END DO
      F02 = 2 * FSED * FTOT
!
      IF (ISZERO(F02)) THEN
!
!        * Zeroth percentile or null distribution
         DR = 0
!
      ELSE
!
!        * Reset fraction pointer
         FRPTR = 1
!
!        * Loop over sediment types until target percentile surpassed
         DO 200 SED = 1, N
!
!           * Calculate midpoint of cumulative fraction (doubled)
            FLO = FHI
            DRLO = DRHI
            FHI = F (FRPTR)
            DRHI = D (SED)
            FSUM2 = FSUM2 + FLO + FHI
!
!           * Break out of loop if target percentile has been reached
!           *  ( allowing for rounding error )
            IF (FSUM2.GE.F02 * ALMOST) GOTO 300
!                                          ^^^^^^^^
!
!           * Increment fraction pointer
            FRPTR = FRPTR + INCF
!
200      END DO
!
!        * Interpolate between last two Fraction/Diameter pairs to find
!        *  target percentile.
!        *  Note :- Combination of precondition FSED<1 and use of ALMOST
!        *          should ensure (FLO+FHI) > 0
300      DR = DRHI - (DRHI - DRLO) * (FSUM2 - F02) / (FLO + FHI)
!
      ENDIF
!
      SYDR = DR
!
   END FUNCTION SYDR
!SSSSSS SUBROUTINE SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, &
   SUBROUTINE SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, &
      QOC, LINKNS, SLOPEJ, GSED)
!
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
      USE CONST_SY

! Commons and distributed constants
!
! Constants referenced
!     CONST.SY:  RHOWAT, RHOSED, GRAVTY
!
! Input arguments
      INTEGER :: NFINE, NLF, NSED, NELEE
      DOUBLEPRECISION DRSED (NFINE+1:NSED), CWIDTH (NLF), DWAT1 (NLF)
      DOUBLEPRECISION QOC (NELEE, 4), SLOPEJ (NELEE, 4)
      LOGICAL :: LINKNS (NLF)
!
! Output arguments
!   defined locally
      DOUBLEPRECISION GSED (NLF, NFINE+1:NSED)
!
! Locals, etc
      INTEGER :: FACE, IEND, LINK, NFP1, SED, SGN
      DOUBLEPRECISION DWAT1E, GD, QK
!
!----------------------------------------------------------------------*
!
!
!     * Initialization
      IF (FIRST_syengh) THEN
         FIRST_syengh = .FALSE.
         KG_syengh = 0.05d0 / (SQRT (GRAVTY) * (RHOSED / RHOWAT - 1) **2)
      ENDIF
      NFP1 = NFINE+1
      CALL ALINIT (ZERO, NLF * (NSED-NFINE), GSED)
!
!     * Loop over ends of link
      DO 300 IEND = 1, 3, 2
         SGN = 2 - IEND
!
!        * Loop over links
         DO 200 LINK = 1, NLF
!
!           * Determine current face number, outflow rate & water depth
            FACE = IEND
            IF (LINKNS (LINK) ) FACE = FACE+1
            QK = SGN * QOC (LINK, FACE)
            DWAT1E = DWAT1 (LINK)
!
!           * Increment capacity rate for non-dry outflow ends only
            IF (GTZERO(DWAT1E).AND.GTZERO(QK)) THEN
!
!              * Loop invariant

               GD = QK**2 * SLOPEJ (LINK, FACE) **1.5 * KG_syengh / (CWIDTH ( &
                  LINK) * SQRT (DWAT1E) )
!              * All sediment types
               DO 100 SED = NFP1, NSED
                  GSED (LINK, SED) = GD / DRSED (SED) + GSED (LINK, SED)
100            END DO
!
            ENDIF
!
!        * Next link
200      END DO
!
!     * Next iend
300   END DO
!
   END SUBROUTINE SYENGH

END MODULE sediment_transport_capacity
