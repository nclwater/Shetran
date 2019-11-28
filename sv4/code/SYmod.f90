MODULE SYmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the SY .F files
USE SGLOBAL
!USE AL_P
USE ALmod, ONLY : ALINIT, ALCHKI, ALCHK, ALALLF, ALREAD  !, HELPPATH
USE ALmod, ONLY : ERROR, ERRC, ERRNEE, ERRTOT !AD NEEDS THIS
USE UTILSMOD, ONLY : DCOPY
IMPLICIT NONE

LOGICAL         :: FIRST_syackw=.TRUE.
DOUBLEPRECISION :: K2_syackw, DGRMAX_syackw, ROOT32_syackw

LOGICAL         :: FIRST_sycltr=.TRUE.
DOUBLEPRECISION :: k1_sycltr

LOGICAL          :: FIRST_sycrit=.TRUE.
DOUBLEPRECISION  :: K1_sycrit, K2_sycrit, K3_sycrit

LOGICAL         :: FIRST_syengh=.TRUE.
DOUBLEPRECISION :: KG_syengh

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

LOGICAL         :: FIRST_syovtr= .TRUE.
DOUBLEPRECISION :: K1_syovtr, K3_syovtr, K4_syovtr

PRIVATE
PUBLIC :: SYMAIN, BALSED, & !REST NEEDED ONLY FOR AD
          issyok_symain
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
  100    END DO  
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
  200             END DO  
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
  300          END DO  
!
!
!           * End of outflow check
      ENDIF  
!
!        * Next link
  400    END DO  
!
!     * Other end of link
  500 END DO  
!
!
END SUBROUTINE SYACKW



!SSSSSS SUBROUTINE SYBC  
SUBROUTINE SYBC  
!!!!STOP ' FATAL ERROR!!  Sediment boundary flows not yet implemented'  
END SUBROUTINE SYBC



!SSSSSS SUBROUTINE SYBED (DCBEDO, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, &
SUBROUTINE SYBED (DCBEDO, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, &
 DDIPRM, ARBDEP, DLS, FBETA, DCBSED, DDBSED, DCBED)
!
!----------------------------------------------------------------------*
!
! Update stream-bed state variables for each link.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR65
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  RAH  23.5.94  Version 3.4.1 by AB/RAH. File creation date 5.4.94.
!----------------------------------------------------------------------*
! Input arguments
INTEGER :: NELEE, NLF, NLFEE, NSED
DOUBLEPRECISION DCBEDO, CWIDTH (NLF)  
DOUBLEPRECISION DCIPRM (NLFEE, NSED), DDIPRM (NLFEE, NSED)  
  
!
! Input/output arguments
DOUBLEPRECISION ARBDEP (NLF), DLS (NLF), FBETA (NELEE, NSED)  
!
! Output arguments
DOUBLEPRECISION DCBSED (NLFEE, NSED), DDBSED (NLFEE, NSED), &
 DCBED (NLF)
!
! Locals, etc
DOUBLEPRECISION AC, AD, DCBEDZ, DCC, DCNEW, DDBEDZ, DLSNEW, &
 DLSOLD
DOUBLEPRECISION DCIPP, DDIPP, DCINEW, SUMSED  
INTEGER :: LINK, SED  
!
!
!----------------------------------------------------------------------*
!
!
!     * Loop over links
DO 300 LINK = 1, NLF  
!
!
!        * Calculate interim bed layer thicknesses
   DCBEDZ = 0  
   DDBEDZ = 0  
   DO 100 SED = 1, NSED  
      DCBEDZ = DCBEDZ + DCIPRM (LINK, SED)  
      DDBEDZ = DDBEDZ + DDIPRM (LINK, SED)  
  100    END DO  
!
!        * Reset variables that are independent of size group
   DLSOLD = DLS (LINK)  
   DLSNEW = DCBEDZ + DDBEDZ  
   DLS (LINK) = DLSNEW  
   ARBDEP (LINK) = ARBDEP (LINK) + CWIDTH (LINK) * (DLSNEW - &
    DLSOLD)
   DCNEW = MIN (DLSNEW, DCBEDO)  
   DCBED (LINK) = DCNEW  
!
!        * What fraction of the interim top layer remains in the top
!        *  layer, and what fraction of the interim bottom layer becomes
!        *  part of the top?
   DCC = MIN (DCBEDZ, DCNEW)  
   AC = 0  
   AD = 0  
   IF (DCBEDZ.GT.0) AC = DCC / DCBEDZ  
   IF (DDBEDZ.GT.0) AD = (DCNEW - DCC) / DDBEDZ  
!
!        * Loop over sediment size groups
   DO 200 SED = 1, NSED  
!
!           * Interim layer depths
      DCIPP = DCIPRM (LINK, SED)  
      DDIPP = DDIPRM (LINK, SED)  
!
!           * Total depth (for this size group)
      SUMSED = DCIPP + DDIPP  
!
!           * New top layer depth
      DCINEW = AC * DCIPP + AD * DDIPP  
      DCBSED (LINK, SED) = DCINEW  
!
!           * New bottom layer depth
      DDBSED (LINK, SED) = SUMSED-DCINEW  
!
!           * Composition of both layers together
      IF (DLSNEW.GT.0) FBETA (LINK, SED) = (SUMSED / DLSNEW)  
!
!        * Next sediment type
  200    END DO  
!
!
!     * Next link
  300 END DO  
!
!
END SUBROUTINE SYBED



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
  100 END DO  
!
END SUBROUTINE SYBKER



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
  300             END DO  
!
         ENDIF  
  310       END DO  
!
  320    END DO  
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
  400             END DO  
!
         ENDIF  
!
!           * Next link
  410       END DO  
!
!        * Next end
  420    END DO  
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
  500 END DO  
!
!     * Loop over links
DO 530 LINK = 1, NLF  
!
!        * Determine denominators for scaling factors
   FDSUM = ZERO  
   DO 510 SED = NFP1, NSED  
      FDSUM = FDSUM + FDEL (LINK, SED)  
  510    END DO  
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
  520    END DO  
!
  530 END DO  
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
  600             END DO  
!
         ENDIF  
!
!           * Next link
  610       END DO  
!
!        * Next side
  620    END DO  
!
ENDIF  
!
END SUBROUTINE SYCLTR



!SSSSSS SUBROUTINE SYCOLM (AREAE, DTSY, DWAT1E, DWATOE, DXQQE, DYQQE, &
SUBROUTINE SYCOLM (AREAE, DTSY, DWAT1E, DWATOE, DXQQE, DYQQE, &
 FETAE, GNUE, ISGSED, NSED, FPCRIT, PLSE, NSEDEE, DRSED, QWAT, &
 SLOPEE, SOSDFE, TAUJE, DLSE, FBETAE, FDELE, QSEDE, Q, VDSED)
!
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
! Input arguments
INTEGER :: ISGSED, NSED, NSEDEE  
DOUBLEPRECISION AREAE, DTSY, DWAT1E, DWATOE, DXQQE, DYQQE  
DOUBLEPRECISION FETAE, GNUE, FPCRIT, PLSE, DRSED (NSED)  
DOUBLEPRECISION QWAT (4), SLOPEE (4), SOSDFE (NSED), TAUJE (4)  
!
! Input/output arguments
DOUBLEPRECISION DLSE, FBETAE (NSED), FDELE (NSED), QSEDE (NSEDEE, &
 4)
!
! Workspace arguments
DOUBLEPRECISION Q (NSED), VDSED (NSED)  
!
! Locals, etc
!
INTEGER :: FACE, J (4), JLC, NOUT, SED  
DOUBLEPRECISION A1, A2, A3, B1, B2, DBETA, DDLS, FD, FLS, G  
DOUBLEPRECISION GJSUM, GSUM, QK, QWSUM, VD, VDSUM, VDWAT  
!
!
!----------------------------------------------------------------------*
!
!
! Initialization
! --------------
!
QWSUM = ZERO  
VDSUM = ZERO  
FLS = ONE-PLSE  
CALL ALINIT (ZERO, NSED, Q)  
!
!
! Water & Sediment Budgets
! ------------------------
!
!     * Calculate water discharge & particulate supply rates
!     *  ( both non-negative ), and make a list of outflow faces
NOUT = 0  
DO 200 FACE = 1, 4  
   QK = QWAT (FACE)  
   IF (QK.GT.ZERO) THEN  
!           * Outflow face
      QWSUM = QWSUM + QK  
      NOUT = NOUT + 1  
      J (NOUT) = FACE  
   ELSE  
!           * Inflow or no-flow face
      DO 100 SED = 1, NSED  
         Q (SED) = Q (SED) - QSEDE (SED, FACE) / FLS  
  100       END DO  
   ENDIF  
  200 END DO  
!
!     * Calculate volume of water + volume of discharged water
VDWAT = DWAT1E * AREAE+QWSUM * DTSY  
!
!     * Calculate volume of stored sediment plus volume of
!     *  discharged sediment for each fraction ( must be non-negative )
DDLS = FETAE * GNUE * DTSY  
DO 300 SED = 1, NSED  
   DBETA = DLSE * FBETAE (SED) + DDLS * SOSDFE (SED)  
   VD = (FDELE (SED) * DWATOE+DBETA) * AREAE+Q (SED) * DTSY  
   VDSUM = VDSUM + VD  
   VDSED (SED) = VD  
  300 END DO  
!
!
! Sediment Discharge
! ------------------
!
!     Note: The only outputs from this section are the coefficients
!           A1 and B1 required by the next section.
!
!     * Discharge rate based upon SUPPLY, assuming unlimited capacity
GSUM = ZERO  
IF (GTZERO(VDWAT)) GSUM = FLS * VDSUM * (QWSUM / VDWAT)  
!
!     * Is discharge possible?
IF (GTZERO(GSUM)) THEN  
!
!        * Yes ( implies VDSUM > 0 )
!
!        * Discharge rate based upon flow CAPACITY ...
   CALL SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, DRSED, &
    QWAT, SLOPEE, TAUJE, GJSUM)
!
!        ... with additional upper limit based on total suspended load
   G = MIN (GJSUM, QWSUM * FPCRIT)  
!
!        * Transport is governed by the lower of the two rates
!          (take MIN before dividing, in case G>>GSUM)
   A1 = MIN (G, GSUM) / GSUM  
   B1 = VDWAT  
!
ELSE  
!
!        * Either no sediment available, or no water to carry it
!
!        * Zero discharge case ( any sediment is deposited )
   A1 = ZERO  
   B1 = ONE  
!
ENDIF  
!
!
! Define Output Variables
! -----------------------
!
!     * Update depth of loose sediments
DLSE = (ONE-A1) * VDSUM / AREAE  
!
!     * Evaluate coefficients for FBETAE
IF (GTZERO(DLSE)) THEN  
!        * Composition of loose sediment is given by VDSED
   A2 = ONE  
   B2 = VDSUM  
   A3 = ZERO  
ELSE  
!        * No loose sediment left: adopt composition of surface soil
   A2 = ZERO  
   B2 = ONE  
   A3 = ONE  
ENDIF  
!
!     * Update compositions of suspended and loose sediments, and set
!     *  sediment flow rates for each outflow face.
!     *  ( don't pre-invert B1 or B2: they may be small! )
DO 500 SED = 1, NSED  
   VD = VDSED (SED)  
   FD = (A1 * VD) / B1  
   FDELE (SED) = FD  
   FBETAE (SED) = A2 * VD / B2 + A3 * SOSDFE (SED)  
   DO 400 JLC = 1, NOUT  
      FACE = J (JLC)  
      QSEDE (SED, FACE) = FLS * QWAT (FACE) * FD  
  400    END DO  
  500 END DO  
!
END SUBROUTINE SYCOLM



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
  100 END DO  
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
  200    END DO  
!
!        * Interpolate between last two Fraction/Diameter pairs to find
!        *  target percentile.
!        *  Note :- Combination of precondition FSED<1 and use of ALMOST
!        *          should ensure (FLO+FHI) > 0
  300    DR = DRHI - (DRHI - DRLO) * (FSUM2 - F02) / (FLO + FHI)  
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
  100          END DO  
!
      ENDIF  
!
!        * Next link
  200    END DO  
!
!     * Next iend
  300 END DO  
!
END SUBROUTINE SYENGH



!SSSSSS SUBROUTINE SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, &
SUBROUTINE SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, &
 NSEE, NV, NVEE, NX, NXEE, NY, SPR, SYD)
!
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
!
INTEGER :: NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE  
INTEGER :: NV, NVEE, NX, NXEE, NY, SPR, SYD  
!
! Locals, etc
INTEGER :: FATAL, ERR  
PARAMETER (FATAL = 1, ERR = 2)  
!
INTEGER :: IUNDEF, NERR, jedumdum
INTEGER :: IDUMS (1), IDUMO (1) 
LOGICAL :: LDUM1 (1)  
!
!
!----------------------------------------------------------------------*
!
! 0. Preliminaries
! ----------------
!
!     * Initialize local counter
NERR = 0  
!
!
! 1. Array Sizes
! --------------
!
!NELEE
IDUMS (1) = NELEE  
IDUMO (1) = MAX (NEL, NV, NX * NY)  
CALL ALCHKI (ERR, 2054, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
!NLFEE
IDUMS (1) = NLFEE  
IDUMO (1) = MAX (1, NLF)  
CALL ALCHKI (ERR, 2055, SPR, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
!NLYREE, NSEDEE
IDUMS (1) = MIN (NLYREE, NSEDEE)  
CALL ALCHKI (ERR, 2056, SPR, 1, 1, IUNDEF, IUNDEF, '[ NLYREE, NSEDEE ]', 'GT', IZERO1, IDUMS, NERR, LDUM1)
!NSEE
IDUMS (1) = NSEE  
IDUMO (1) = NS  
CALL ALCHKI (ERR, 2057, SPR, 1, 1, IUNDEF, IUNDEF, 'NSEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
!NVEE
IDUMS (1) = NVEE  
IDUMO (1) = NV  
CALL ALCHKI (ERR, 2058, SPR, 1, 1, IUNDEF, IUNDEF, 'NVEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
!NXEE
IDUMS (1) = NXEE  
IDUMO (1) = NX  
CALL ALCHKI (ERR, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
IDUMO (1) = 9999  
CALL ALCHKI (ERR, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'LE', &
 IDUMO, IDUMS, NERR, LDUM1)
!
!
! 2. Unit Numbers
! ---------------
!
!SPR, SYD
IDUMS (1) = MIN (SPR, SYD)  
CALL ALCHKI (ERR, 2060, SPR, 1, 1, IUNDEF, IUNDEF, '[ SPR, SYD ]', &
 'GE', IZERO1, IDUMS, NERR, LDUM1)
!
!
! 3. Number of Entities
! ---------------------
!
!NLF
IDUMS (1) = NLF  
IDUMO (1) = NEL  
CALL ALCHKI (ERR, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', &
 IZERO1, IDUMS, NERR, LDUM1)
CALL ALCHKI (ERR, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', &
 IDUMO, IDUMS, NERR, LDUM1)
!NS, NV, NX, NY
jedumdum = MIN (NS, NV)
!""AD IDUMS (1) = MIN (NS, NV, NX, NY)
IDUMS (1) = MIN (jedumdum, NX, NY)  
CALL ALCHKI (ERR, 2062, SPR, 1, 1, IUNDEF, IUNDEF, '[ NS, NV, NX, NY ]', 'GT', IZERO1, IDUMS, NERR, LDUM1)
!
!
! 4. Epilogue
! -----------
!
IF (NERR.GT.0) CALL ERROR (FATAL, 2000, SPR, 0, 0, 'Error(s) detected while checking WAT-SY interface variables')
!
END SUBROUTINE SYERR0




!SSSSSS SUBROUTINE SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, &
SUBROUTINE SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, &
 NXEE, NYEE, NY, SPR, BEXBK, LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, &
 NTSOIL, NVC, THSAT, CLENTH, CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, &
 ARXL, HRF, ZGRUND, IDUM, IDUM1X, LDUM)
!
!----------------------------------------------------------------------*
!
! Check static & initializing arrays in the WAT-SY interface.
!
!----------------------------------------------------------------------*
! Version:  3.4.1          Notes:  SSR83
!  Module:  SY           Program:  SHETRAN
! Modifications:
!  RAH  26.09.94  Version 3.4.1.  File created 25.09.94.
!----------------------------------------------------------------------*
!
INTEGER :: NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, NXEE, NYEE, NY, &
 SPR
INTEGER :: ICMBK (NLFEE, 2), ICMXY (NXEE, NY), ICMREF (NELEE, 4, &
 2:3)
INTEGER :: ICMRF2 (NLFEE, 3, 2), NLYR (NLF + 1:NEL)  
INTEGER :: NTSOIL (NELEE, NLYREE), NVC (NLF + 1:NEL)  
DOUBLEPRECISION THSAT (NS)  
DOUBLEPRECISION CLENTH (NLFEE), CWIDTH (NLFEE), ZBFULL (NLFEE)  
DOUBLEPRECISION DXQQ (NLF + 1:NEL), DYQQ (NLF + 1:NEL)  
DOUBLEPRECISION AREA (NEL), DHF (NELEE, 4)  
DOUBLEPRECISION ARXL (NLFEE), HRF (NLF + 1:NEL), ZGRUND (NEL)  
LOGICAL :: BEXBK, LINKNS (NLFEE)  
!
! Workspace arguments
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM
INTEGER :: IDUM1X ( - 1:NEL + 1)  
LOGICAL :: LDUM (NELEE)  
!
! Locals, etc
INTEGER :: FATAL, ERR  
PARAMETER (FATAL = 1, ERR = 2)  
!
INTEGER :: BANK, COUNT, FACE, FADJ, FEL  
INTEGER :: IADJ, IBR, IBRADJ, ICOL1, IEL, IELP, ILYR, IUNDEF, IX, &
 IY
INTEGER :: LINK, NCOL, NELP, NERR, P, PADJ  
INTEGER :: IDUM1 (2)
LOGICAL :: BKXYOK, REFOK  
!
!----------------------------------------------------------------------*
!
! 0. Preliminaries
! ----------------
!
!     * local counter
NERR = 0  
!     * position of 1st column element
ICOL1 = NLF + 1  
!     * number of elements plus one
NELP = NEL + 1  
!
!
! 1. Index Arrays
! ---------------
!
!ICMBK, ICMXY
COUNT = NERR  
!     * initialize column-element counter & marker array
NCOL = 0  
DO 110 IEL = 0, NLF  
   IDUM1X (IEL) = 1  
  110 END DO  
DO 115 IEL = ICOL1, NELP  
   IDUM1X (IEL) = 0  
  115 END DO  
!     * count active grid elements and mark them
DO 125 IY = 1, NY  
   DO 120 IX = 1, NX  
      IEL = MAX (0, MIN (ICMXY (IX, IY), NELP) )  
      IDUM1X (IEL) = IDUM1X (IEL) + 1  
      NCOL = NCOL + MIN (IEL, 1)  
  120    END DO  
  125 END DO  
!     * similarly for bank elements (if present all must be active)
IF (BEXBK.AND.NLF.GT.0) THEN  
   NCOL = NCOL + 2 * NLF  
   DO 135 BANK = 1, 2  
      DO 130 LINK = 1, NLF  
         IEL = MAX (0, MIN (ICMBK (LINK, BANK), NELP) )  
         IDUM1X (IEL) = IDUM1X (IEL) + 1  
  130       END DO  
  135    END DO  
ENDIF  
!     * watch out for gate-crashers
IDUM1 (1) = NEL - NLF  
IDUM1X (0) = NCOL  
CALL ALCHKI (ERR, 2075, SPR, 1, 1, IUNDEF, IUNDEF, '#_column_elements', 'EQ', IDUM1, IDUM1X (0) , NERR, LDUM)
!     * check that each element has a unique identity
CALL ALCHKI (ERR, 2076, SPR, 1, NEL, IUNDEF, IUNDEF, &
 'element_count(iel)', 'EQ', IONE1, IDUM1X (1) , NERR, LDUM)
!     * was everything ok?
BKXYOK = COUNT.EQ.NERR  
!
!ICMREF part 1
IDUM1 (1) = NEL  
IDUM1 (2) = - NLFEE  
REFOK = .TRUE.  
DO 145 FACE = 1, 4  
   COUNT = NERR  
!        * check that all neighbours are within range
CALL ALCHKI (ERR, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)' &
&, 'LE', IDUM1 (1) , ICMREF (1, FACE, 2) , NERR, LDUM)
CALL ALCHKI (ERR, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)' &
&, 'GE', IDUM1 (2) , ICMREF (1, FACE, 2) , NERR, LDUM)
!        * check regular faces for range and consistency
   IF (COUNT.EQ.NERR) THEN  
      DO 140 IEL = 1, NEL  
         IADJ = ICMREF (IEL, FACE, 2)  
         IF (IADJ.LE.0) THEN  
!                 * not a regular face
            IDUM (IEL) = 0  
         ELSE  
            FADJ = ICMREF (IEL, FACE, 3)  
            IF (FADJ.LT.1.OR.FADJ.GT.4) THEN  
!                    * bad face value
               IDUM (IEL) = 1  
            ELSE  
               IF (ICMREF (IADJ, FADJ, 2) .NE.IEL) THEN  
!                       * bad reflection
                  IDUM (IEL) = 2  
               ELSE  
                  IDUM (IEL) = 0  
!                       * faces don't match?
                  IF (ICMREF (IADJ, FADJ, 3) .NE.FACE) IDUM (IEL) &
                   = 3
               ENDIF  
            ENDIF  
         ENDIF  
  140       END DO  
      CALL ALCHKI (ERR, 2078, SPR, 1, NEL, FACE, IUNDEF, &
       'status_of_ICMREF(iel,face)', 'EQ', IZERO1, IDUM, NERR, LDUM)
   ENDIF  
!        * is everything still ok?
   REFOK = REFOK.AND.COUNT.EQ.NERR  
  145 END DO  
!
!ICMREF part 2 (bank element neighbours)
IF (NLF.GT.0.AND.BEXBK.AND.BKXYOK.AND.REFOK) THEN  
!        * set marker array (disallow non-grids other than zero)
   IDUM1X ( - 1) = - 2  
   IDUM1X (0) = 0  
   DO 150 IEL = 1, NEL  
      IDUM1X (IEL) = - 2  
  150    END DO  
   DO 165 IY = 1, NY  
      DO 160 IX = 1, NX  
         IEL = MAX (0, ICMXY (IX, IY) )  
         IDUM1X (IEL) = MIN (IEL, 1)  
  160       END DO  
  165    END DO  
!        * count number of grid neighours for each link
   DO 170 LINK = 1, NLF  
      IDUM (LINK) = 0  
  170    END DO  
   DO 185 BANK = 1, 2  
      DO 180 LINK = 1, NLF  
         IEL = ICMBK (LINK, BANK)  
         FACE = 2 * BANK  
         IF (LINKNS (LINK) ) FACE = FACE-1  
         IADJ = MAX ( - 1, ICMREF (IEL, FACE, 2) )  
         IDUM (LINK) = IDUM (LINK) + IDUM1X (IADJ)  
  180       END DO  
  185    END DO  
CALL ALCHKI (ERR, 2079, SPR, 1, NLF, IUNDEF, IUNDEF, '#_grids_neighbouring_banks(link)', 'GT', IZERO1, IDUM, NERR, LDUM)
ENDIF  
!
!ICMRF2
IF (REFOK) THEN  
!        * initialize status array
   DO 190 IBR = 1, NLFEE  
      IDUM (IBR) = - 1  
  190    END DO  
!        * check each prospect of each branch
   DO 198 FACE = 1, 4  
      DO 196 IEL = 1, NEL  
         IADJ = ICMREF (IEL, FACE, 2)  
         IF (IADJ.LT.0) THEN  
            IBR = - IADJ  
            IF (IDUM (IBR) .GE.0) THEN  
!                    * duplicate reference
               IDUM (IBR) = IDUM (IBR) + 1  
            ELSE  
!                    * initialize status
               IDUM (IBR) = 0  
               DO 194 P = 1, 3  
                  IADJ = ICMRF2 (IBR, P, 1)  
                  IF (IADJ.GT.NEL) THEN  
!                          * neighbour out of range
                     IDUM (IBR) = IDUM (IBR) + P * 10  
                  ELSEIF (IADJ.GT.0) THEN  
                     FADJ = ICMRF2 (IBR, P, 2)  
                     IF (FADJ.LT.1.OR.FADJ.GT.4) THEN  
!                             * bad face value
                        IDUM (IBR) = IDUM (IBR) + P * 100  
                     ELSE  
                        IBRADJ = - ICMREF (IADJ, FADJ, 2)  
                        IF (IBRADJ.LT.1.OR.IBRADJ.GT.NLFEE) THEN  
!                                * bad mirror branch
                           IDUM (IBR) = IDUM (IBR) + P * 1000  
                        ELSE  
                           DO 192 PADJ = 1, 3  
                              IELP = ICMRF2 (IBRADJ, PADJ, 1)  
                              IF (IELP.EQ.IEL) THEN  
                              FEL = ICMRF2 (IBRADJ, PADJ, 2)  
                              IF (FEL.EQ.FACE) GOTO 193  
                              ENDIF  
  192                            END DO  
!                                * can't find a reference in the mirror
                           IDUM (IBR) = IDUM (IBR) + P * 10000  
  193                            CONTINUE  
                        ENDIF  
                     ENDIF  
                  ENDIF  
  194                END DO  
            ENDIF  
         ENDIF  
  196       END DO  
  198    END DO  
   CALL ALCHKI (ERR, 2080, SPR, 1, NLFEE, IUNDEF, IUNDEF, &
    'status_of_ICMRF2(branch)', 'LE', IZERO1, IDUM, NERR, LDUM)
ENDIF  
!
!
! 2. Soil Properties
! ------------------
!
!THSAT
CALL ALCHK (ERR, 2063, SPR, 1, NS, IUNDEF, IUNDEF, 'THSAT(soil)', &
 'LE', ONE1, ZERO1 (1) , THSAT, NERR, LDUM)
!
!
! 3. Link Properties & Initial State
! ----------------------------------
!
IF (NLF.GT.0) THEN  
!
!CLENTH
CALL ALCHK (ERR, 2064, SPR, 1, NLF, IUNDEF, IUNDEF, 'CLENTH(link)' &
&, 'GE', ZERO1, ZERO1 (1) , CLENTH, NERR, LDUM)
!CWIDTH
CALL ALCHK (ERR, 2065, SPR, 1, NLF, IUNDEF, IUNDEF, 'CWIDTH(link)' &
&, 'GT', ZERO1, ZERO1 (1) , CWIDTH, NERR, LDUM)
!ZBFULL
CALL ALCHK (ERR, 2066, SPR, 1, NLF, IUNDEF, IUNDEF, 'ZBFULL(link)' &
&, 'GEa', ZGRUND, ZERO1 (1) , ZBFULL, NERR, LDUM)
!ARXL
CALL ALCHK (ERR, 2067, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
&'GE', ZERO1, ZERO1 (1) , ARXL, NERR, LDUM)
!
ENDIF  
!
!
! 4. Column Properties & Initial State
! ------------------------------------
!
!DXQQ
CALL ALCHK (ERR, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DXQQ(iel)', 'GT', ZERO1, ZERO1 (1) , DXQQ, NERR, LDUM)
!DYQQ
CALL ALCHK (ERR, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DYQQ(iel)', 'GT', ZERO1, ZERO1 (1) , DYQQ, NERR, LDUM)
!HRF
CALL ALCHK (ERR, 2069, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'HRF(iel)' , 'GEa', ZGRUND (ICOL1) , ZERO1 (1) , HRF, NERR, LDUM)
!NLYR
COUNT = NERR  
IDUM1 (1) = NLYREE  
CALL ALCHKI (ERR, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'GT', IZERO1, NLYR, NERR, LDUM)
CALL ALCHKI (ERR, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'LE', IDUM1, NLYR, NERR, LDUM)
!NTSOIL
IF (COUNT.EQ.NERR) THEN  
   DO 410 IEL = ICOL1, NEL  
      ILYR = NLYR (IEL)  
      IDUM (IEL) = NTSOIL (IEL, ILYR)  
  410    END DO  
   IDUM1 (1) = NS  
   CALL ALCHKI (ERR, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, &
    'NTSOIL[iel,NLYR(iel)]', 'GT', IZERO1, IDUM (ICOL1) , NERR, &
    LDUM)
   CALL ALCHKI (ERR, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, &
    'NTSOIL[iel,NLYR(iel)]', 'LE', IDUM1, IDUM (ICOL1) , NERR, &
    LDUM)
ENDIF  
!NVC
COUNT = NERR  
IDUM1 (1) = NV  
CALL ALCHKI (ERR, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'GT', IZERO1, NVC, NERR, LDUM)
CALL ALCHKI (ERR, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'LE', IDUM1, NVC, NERR, LDUM)
!
!
! 5. Element Properties
! ---------------------
!
!AREA
CALL ALCHK (ERR, 2073, SPR, 1, NEL, IUNDEF, IUNDEF, 'AREA(iel)', &
 'GT', ZERO1, ZERO1 (1) , AREA, NERR, LDUM)
!DHF
DO 510 FACE = 1, 4  
CALL ALCHK (ERR, 2074, SPR, 1, NEL, FACE, IUNDEF, 'DHF(iel,face)', &
& 'GT', ZERO1, ZERO1 (1) , DHF (1, FACE) , NERR, LDUM)
  510 END DO  
!
!
! 6. Epilogue
! -----------
!
IF (NERR.GT.0) CALL ERROR (FATAL, 2001, SPR, 0, 0, 'Error(s) detected while checking static/initial WAT-SY interface')
!
END SUBROUTINE SYERR1



!SSSSSS SUBROUTINE SYERR2 (NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, &
SUBROUTINE SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, &
 NV, NSYB, NSYBEE, NSYC, NSYCEE, SPR, ICMREF, ISUSED, NEPS, NFINE, &
 SFB, SRB, ALPHA, DCBEDO, FPCRIT, DLSMAX, NTSOBK, NSYBCD, NBFACE, &
 DRSED, BKB, GKF, GKR, RHOSO, SOSDFN, DRDRIP, FDRIP, XDRIP, PBSED, &
 FCG, FCROCK, PLS, DLS, FBETA, FDEL, ABC, BBC, GBC, IDUM, DUMMY, &
 LDUM)
!
!----------------------------------------------------------------------*
!
! Check for errors in the SY input data.
!
!----------------------------------------------------------------------*
! Version:  3.4.1          Notes:  SSR43
!  Module:  SY           Program:  SHETRAN
! Modifications:
!  RAH  04.10.94  Version 3.4.1 by AB/RAH. File created 3.2.94.
!  BTL  25.05.95  Version 3.4.1 : add DLSMAX
!----------------------------------------------------------------------*
INTEGER :: NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV  
INTEGER :: NSYB, NSYBEE, NSYC (4), NSYCEE, SPR  
INTEGER :: ICMREF (NELEE, 4, 2:2)  
INTEGER :: ISUSED, NEPS, NFINE, SFB, SRB  
INTEGER :: NTSOBK (NLFEE), NSYBCD (NSYBEE, 3), NBFACE (NEL)  
DOUBLEPRECISION ALPHA, DCBEDO, FPCRIT  
DOUBLEPRECISION DRSED (NSED), BKB (NS), GKF (NS), GKR (NS), &
 RHOSO (NS)
DOUBLEPRECISION SOSDFN (NSEE, NSED), DRDRIP (NV), FDRIP (NV), &
 XDRIP (NV)
DOUBLEPRECISION PBSED (NLFEE)  
DOUBLEPRECISION FCG (NLF + 1:NEL), FCROCK (NLF + 1:NEL), PLS (NLF &
 + 1:NEL)
DOUBLEPRECISION DLS (NEL), FBETA (NELEE, NSED), FDEL (NELEE, NSED)  
DOUBLEPRECISION ABC (NSEDEE, NSYCEE), BBC (NSEDEE, NSYCEE)  
DOUBLEPRECISION GBC (NSEDEE, NSYCEE)  
DOUBLEPRECISION DLSMAX, rdum(nxee*nyee)
!
! Workspace arguments
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM  
DOUBLEPRECISION DUMMY (NELEE)  
LOGICAL :: LDUM (NELEE)  
!
! Locals, etc
INTEGER :: FATAL, ERR  
DOUBLEPRECISION TOL  
PARAMETER (FATAL = 1, ERR = 2, TOL = 1D-10)  
!
INTEGER :: BB, COUNT, FACE, ICAT, IUNDEF, IEL, ITYPE, NERR  
INTEGER :: SED, SOIL, jedumdum
INTEGER :: IDUM1 (1) 
!
!
!
!----------------------------------------------------------------------*
!
!
! 0. Preliminaries
! ----------------
!
!     * Local counter
NERR = 0  
!
!
! 1. Static Variables
! -------------------
!
!NEPS
IDUM (1) = NEPS  
CALL ALCHKI (ERR, 2012, SPR, 1, 1, IUNDEF, IUNDEF, 'NEPS', 'GE', &
 IONE1, IDUM, NERR, LDUM)
NEPS = IDUM (1)  
!FPCRIT
DUMMY (1) = FPCRIT  
CALL ALCHK (ERR, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'FPCRIT', 'GE', &
 ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
FPCRIT = DUMMY (1)  
!DLSMAX
DUMMY (1) = DLSMAX  
CALL ALCHK (ERR, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'DLSMAX', 'GE', &
 ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
DLSMAX = DUMMY (1)  
!>>
IF (NLF.GT.0) THEN  
!>>
!ISUSED
   IDUM (1) = ISUSED  
   CALL ALCHKI (ERR, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', &
    'GE', IZERO1, IDUM, NERR, LDUM)
   CALL ALCHKI (ERR, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', &
    'LE', IONE1, IDUM, NERR, LDUM)
   ISUSED = IDUM (1)  
!NFINE
   IDUM (1) = NFINE  
   IDUM1 (1) = MIN (1, NSED-1)  
   CALL ALCHKI (ERR, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', &
    'GE', IZERO1, IDUM, NERR, LDUM)
   CALL ALCHKI (ERR, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', &
    'LE', IDUM1, IDUM, NERR, LDUM)
   NFINE = IDUM (1)  
!ALPHA
   IF (NFINE.GT.0) THEN  
      DUMMY (1) = ALPHA  
      CALL ALCHK (ERR, 2016, SPR, 1, 1, IUNDEF, IUNDEF, 'ALPHA', &
       'GE', ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
      ALPHA = DUMMY (1)  
   ENDIF  
!DCBEDO
   DUMMY (1) = DCBEDO  
   CALL ALCHK (ERR, 2017, SPR, 1, 1, IUNDEF, IUNDEF, 'DCBEDO', &
    'GE', ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
   DCBEDO = DUMMY (1)  
!<<
ENDIF  
!<<
!NELEE
IDUM (1) = NXEE*NYEE
!!!!IDUM1(1) = MAX( NSED, NLF*DIM(NSED,NFINE) )  !AD
jedumdum = IDIMJE(NSED, NFINE)
jedumdum = jedumdum * NLF
idum1(1) = MAX(nsed, jedumdum)
!     * (including local workspace requirements)
IDUM1 (1) = MAX (IDUM1 (1), NS, NSYB * 2)  
CALL ALCHKI (ERR, 2018, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', &
 IDUM1, IDUM, NERR, LDUM)
!
!
! 2. Sediment, Soil & Vegetation Properties
! -----------------------------------------
!
!     * Not enough workspace?
IF (NELEE.LT.MAX (NSED, NS) ) GOTO 300  
!
!DRSED
COUNT = NERR  
CALL ALCHK (ERR, 2019, SPR, 1, 1, IUNDEF, IUNDEF, 'DRSED(sed)', &
 'GT', ZERO1, ZERO1 (1) , DRSED (1) , NERR, LDUM)
 
 !original code
 !IF ( NSED.GT.1 .AND. NERR.EQ.COUNT ) THEN
 !        CALL DCOPY( NSED-1, DRSED, 1, IDUM, 1 )
 !        CALL ALCHK    ( ERR,2019,SPR,    2,NSED,IUNDEF,IUNDEF,
 !    $          'DRSED(sed)','GEa',IDUM ,ZERO(1),   DRSED(2),NERR,LDUM )
 !     ENDIF   
      
IF (NSED.GT.1.AND.NERR.EQ.COUNT) THEN  
         !CALL DCOPY( NSED-1, DRSED, 1, IDUM, 1 )
   CALL DCOPY (NSED-1, DRSED, 1, RDUM, 1)  
   idum(1:NSED-1) = INT (rdum(1:NSED-1))  
CALL ALCHK (ERR, 2019, SPR, 2, NSED, IUNDEF, IUNDEF, 'DRSED(sed)', &
& 'GEa', RDUM, ZERO1 (1) , DRSED (2) , NERR, LDUM)
!     $          'DRSED(sed)','GEa',IDUM ,ZERO(1),   DRSED(2),NERR,LDUM
ENDIF  
!GKR
CALL ALCHK (ERR, 2020, SPR, 1, NS, IUNDEF, IUNDEF, 'GKR(soil)', &
 'GE', zero1, zero1 (1) , GKR, NERR, LDUM)
!GKF
CALL ALCHK (ERR, 2021, SPR, 1, NS, IUNDEF, IUNDEF, 'GKF(soil)', &
 'GE', zero1, zero1 (1) , GKF, NERR, LDUM)
!RHOSO
CALL ALCHK (ERR, 2022, SPR, 1, NS, IUNDEF, IUNDEF, 'RHOSO(soil)', &
 'GT', zero1, zero1 (1) , RHOSO, NERR, LDUM)
!BKB
IF (NLF.GT.0) CALL ALCHK (ERR, 2023, SPR, 1, NS, IUNDEF, IUNDEF, &
 'BKB(soil)', 'GE', zero1, zero1 (1) , BKB, NERR, LDUM)
!SOSDFN
CALL ALINIT (ZERO1 (1), NS, DUMMY)  
DO 220 SED = 1, NSED  
   DO 210 SOIL = 1, NS  
      DUMMY (SOIL) = DUMMY (SOIL) + SOSDFN (SOIL, SED)  
  210    END DO  
CALL ALCHK (ERR, 2024, SPR, 1, NS, SED, IUNDEF, 'SOSDFN(soil,sed)' &
&, 'GE', zero1, zero1 (1) , SOSDFN (1, SED) , NERR, LDUM)
  220 END DO  
CALL ALCHK (ERR, 2024, SPR, 1, NS, IUNDEF, IUNDEF, 'SOSDFN[*][sum_over_sed](soil)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)
!XDRIP
CALL ALCHK (ERR, 2025, SPR, 1, NV, IUNDEF, IUNDEF, 'XDRIP(veg)', &
 'GE', zero1, zero1 (1) , XDRIP, NERR, LDUM)
!DRDRIP
CALL ALCHK (ERR, 2026, SPR, 1, NV, IUNDEF, IUNDEF, 'DRDRIP(veg)', &
 'GT', zero1, zero1 (1) , DRDRIP, NERR, LDUM)
!FDRIP
CALL ALCHK (ERR, 2027, SPR, 1, NV, IUNDEF, IUNDEF, 'FDRIP(veg)', &
 'GE', zero1, zero1 (1) , FDRIP, NERR, LDUM)
!
!
! 3. Link Element Properties
! --------------------------
!
  300 IF (NLF.GT.0) THEN  
!
!NTSOBK
   IDUM (1) = NS  
CALL ALCHKI (ERR, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'GE', IONE1, NTSOBK, NERR, LDUM)
CALL ALCHKI (ERR, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'LE', IDUM, NTSOBK, NERR, LDUM)
!PBSED
CALL ALCHK (ERR, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', &
& 'GE', zero1, zero1 (1) , PBSED, NERR, LDUM)
CALL ALCHK (ERR, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', &
& 'LT', ONE1, ZERO1 (1) , PBSED, NERR, LDUM)
!
ENDIF  
!
!
! 4. Column-element Properties
! ----------------------------
!
!FCROCK
CALL ALCHK (ERR, 2030, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCROCK(iel)', 'LE', ONE1, ZERO1 (1) , FCROCK, NERR, LDUM)
!FCG
DO 410 IEL = NLF + 1, NEL  
   DUMMY (IEL) = ONE1 (1) - FCROCK (IEL)  
  410 END DO  
CALL ALCHK (ERR, 2031, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCG(iel)', 'LEa', DUMMY (NLF + 1) , ZERO1 (1) , FCG, NERR, LDUM)
!PLS
CALL ALCHK (ERR, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'GE', zero1, zero1 (1) , PLS, NERR, LDUM)
CALL ALCHK (ERR, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'LT', ONE1, ZERO1 (1) , PLS, NERR, LDUM)
!
!
! 5. All-element Initialization
! -----------------------------
!
!DLS
CALL ALCHK (ERR, 2033, SPR, 1, NEL, IUNDEF, IUNDEF, 'DLS(iel)', &
 'GE', zero1, zero1 (1) , DLS, NERR, LDUM)
!FBETA
CALL ALINIT (ZERO1 (1), NEL, DUMMY)  
DO 520 SED = 1, NSED  
   DO 510 IEL = 1, NEL  
      DUMMY (IEL) = DUMMY (IEL) + FBETA (IEL, SED)  
  510    END DO  
CALL ALCHK (ERR, 2034, SPR, 1, NEL, SED, IUNDEF, 'FBETA(iel,sed)', &
& 'GE', zero1, zero1 (1) , FBETA (1, SED) , NERR, LDUM)
  520 END DO  
CALL ALCHK (ERR, 2034, SPR, 1, NEL, IUNDEF, IUNDEF, 'FBETA[*][sum_over_sed](iel)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)
!FDEL
DO 530 SED = 1, NSED  
CALL ALCHK (ERR, 2035, SPR, 1, NEL, SED, IUNDEF, 'FDEL(iel,sed)', &
&'GE', zero1, zero1 (1) , FDEL (1, SED) , NERR, LDUM)
  530 END DO  
!
!
! 6. Boundary Data
! ----------------
!
IF (NSYB.GT.0) THEN  
!Not enough workspace?
   IF (NELEE.LT.NSYB * 2) GOTO 700  
!NSYCEE
   IDUM (1) = NSYCEE  
   IDUM1 (1) = MAX (NSYC (1) + NSYC (2), NSYC (3) + NSYC (4) )  
   CALL ALCHKI (ERR, 2036, SPR, 1, 1, IUNDEF, IUNDEF, 'NSYCEE', &
    'GE', IDUM1, IDUM, NERR, LDUM)
!NSYBCD(BB,1)
   COUNT = NERR  
   IDUM1 (1) = NEL  
CALL ALCHKI (ERR, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', &
& 'GE', IONE1, NSYBCD, NERR, LDUM)
CALL ALCHKI (ERR, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', &
& 'LE', IDUM1, NSYBCD, NERR, LDUM)
!NBFACE
   IF (COUNT.EQ.NERR) THEN  
      DO 610 BB = 1, NSYB  
         IEL = NSYBCD (BB, 1)  
         IDUM (BB) = NBFACE (IEL)  
  610       END DO  
      IDUM1 (1) = 4  
      CALL ALCHKI (ERR, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, &
       'NBFACE[NSYBCD[*][1]](bdry)', 'GE', IONE1, IDUM, NERR, LDUM)
      CALL ALCHKI (ERR, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, &
       'NBFACE[NSYBCD[*][1]](bdry)', 'LE', IDUM1, IDUM, NERR, LDUM)
   ENDIF  
!ICMREF
   IF (COUNT.EQ.NERR) THEN  
      DO 620 BB = 1, NSYB  
         IEL = NSYBCD (BB, 1)  
         FACE = NBFACE (IEL)  
         IDUM (BB) = ICMREF (IEL, FACE, 2)  
  620       END DO  
      CALL ALCHKI (ERR, 2039, SPR, 1, NSYB, IUNDEF, IUNDEF, &
       'ICMREF[NSYBCD[*][1]][NBFACE][2](bdry)', 'EQ', IZERO1, IDUM, &
       NERR, LDUM)
   ENDIF  
!NSYBCD(BB,3)
   DO 630 BB = 1, NSYB  
      ITYPE = NSYBCD (BB, 2)  
      IDUM (BB) = 1  
      IF (MOD (ITYPE, 2) .EQ.0) IDUM (BB) = IDUM (BB) + NSYC ( &
       ITYPE-1)
      IDUM (NSYB + BB) = IDUM (BB) + NSYC (ITYPE)  
  630    END DO  
CALL ALCHKI (ERR, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', &
& 'GE', IDUM, NSYBCD (1, 3) , NERR, LDUM)
CALL ALCHKI (ERR, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', &
& 'LE', IDUM (NSYB + 1) , NSYBCD (1, 3) , NERR, LDUM)
!GBC
   DO 640 ICAT = 1, NSYC (1)  
CALL ALCHK (ERR, 2041, SPR, 1, NSED, ICAT, IUNDEF, 'GBC(sed,icat)' &
&, 'GE', zero1, zero1 (1) , GBC (1, ICAT) , NERR, LDUM)
  640    END DO  
!ABC
   DO 650 ICAT = 1, NSYC (3)  
CALL ALCHK (ERR, 2042, SPR, 1, NSED, ICAT, IUNDEF, 'ABC(sed,icat)' &
&, 'GE', zero1, zero1 (1) , ABC (1, ICAT) , NERR, LDUM)
  650    END DO  
!BBC
   DO 660 ICAT = 1, NSYC (3)  
CALL ALCHK (ERR, 2043, SPR, 1, NSED, ICAT, IUNDEF, 'BBC(sed,icat)' &
&, 'GT', zero1, zero1 (1) , BBC (1, ICAT) , NERR, LDUM)
  660    END DO  
!SFB
   IF (NSYC (2) .GT.0) THEN  
      IDUM (1) = SFB  
      CALL ALCHKI (ERR, 2044, SPR, 1, 1, IUNDEF, IUNDEF, 'SFB', &
       'GE', IZERO1, IDUM, NERR, LDUM)
   ENDIF  
!SRB
   IF (NSYC (2) .GT.0) THEN  
      IDUM (1) = SRB  
      CALL ALCHKI (ERR, 2045, SPR, 1, 1, IUNDEF, IUNDEF, 'SRB', &
       'GE', IZERO1, IDUM, NERR, LDUM)
   ENDIF  
ENDIF  
!
!
! 7. Epilogue
! -----------
!
  700 IF (NERR.GT.0) CALL ERROR (FATAL, 2000, SPR, 0, 0, 'Error(s) detected while checking SY input data')
!
END SUBROUTINE SYERR2



!SSSSSS SUBROUTINE SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, &
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
INTEGER :: NEL, NELEE, NLF, NLFEE, NV, SPR  
INTEGER :: ICMREF (NELEE, 4, 2:3), ICMRF2 (NLFEE, 3, 2)  
INTEGER :: ISORT (NEL)  
DOUBLEPRECISION DTUZ  
DOUBLEPRECISION CLAI (NV), PLAI (NV), ARXL (NLFEE)  
DOUBLEPRECISION DRAINA (NLF + 1:NEL), PNETTO (NLF + 1:NEL)  
DOUBLEPRECISION HRF (NEL), ZGRUND (NEL), QOC (NELEE, 4), rdum(nelee)
!
! Workspace arguments
INTEGER :: IQ (NEL), JMIN (NEL), JSORT (0:NEL + 1)  
LOGICAL :: LDUM (NELEE)  
!
! Locals, etc
INTEGER :: FATAL, ERR  
DOUBLEPRECISION TOL  
PARAMETER (FATAL = 1, ERR = 2, TOL = 1D-7)  
!
INTEGER :: FACE, FADJ, I, IADJ, IBR, IEL, IUNDEF, J, NELP, NERR, &
 P
DOUBLEPRECISION FNQOUT, QADJ, QMIN  
DOUBLEPRECISION DUM1 (1)
!
!     * Water discharge rate
FNQOUT (IEL, FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)  
!
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
!DTUZ
DUM1 (1) = DTUZ  
CALL ALCHK (ERR, 2046, SPR, 1, 1, IUNDEF, IUNDEF, 'DTUZ', 'GE', &
 zero1, zero1 (1) , DUM1, NERR, LDUM)
!
!
! 2. Vegetative State
! -------------------
!
!CLAI
CALL ALCHK (ERR, 2047, SPR, 1, NV, IUNDEF, IUNDEF, 'CLAI(veg)', &
 'GE', zero1, zero1 (1) , CLAI, NERR, LDUM)
!PLAI
CALL ALCHK (ERR, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
 'GE', zero1, zero1 (1) , PLAI, NERR, LDUM)
CALL ALCHK (ERR, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
 'LE', ONE1, ZERO1 (1) , PLAI, NERR, LDUM)
!
!
! 3. Link State
! -------------
!
IF (NLF.GT.0) THEN  
!
!ARXL
CALL ALCHK (ERR, 2049, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
&'GE', zero1, zero1 (1) , ARXL, NERR, LDUM)
!
ENDIF  
!
!
! 4. Columnar State
! -----------------
!
!DRAINA
CALL ALCHK (ERR, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'GE', zero1, zero1 (1) , DRAINA, NERR, LDUM)
! 10.10.94  Ought to fix WAT module so that we don't need TOL
CALL ALCHK (ERR, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'LEa', PNETTO, TOL, DRAINA, NERR, LDUM)
!
!
! 5. Elemental State
! ------------------
!
!HRF
CALL ALCHK (ERR, 2051, SPR, 1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', &
 'GEa', ZGRUND, ZERO1 (1) , HRF, NERR, LDUM)
!
!
! 6. Flux/Ordering
! ----------------
!
!ISORT & QOC
!     * Set JSORT = inverse of ISORT & initialize upper bound JMIN
!       (note that JSORT has overspill elements )
NELP = NEL + 1  
DO 610 J = 0, NELP  
   JSORT (J) = NELP  
  610 END DO  
DO 620 I = 1, NEL  
   IEL = ISORT (I)  
   J = MAX (0, MIN (IEL, NELP) )  
   JSORT (J) = I  
   JMIN (I) = NELP  
  620 END DO  
!     * At this point any element not listed in ISORT has a JSORT
!       value of NELP, which is guaranteed to fail the test below
!     * Update JMIN (used as object of JSORT test) & set QOC status IQ
DO 650 FACE = 1, 4  
   DO 640 IEL = 1, NEL  
!           * innocent until proven guilty
      IQ (IEL) = 0  
!           * non-discharge faces are ok
      IF (FNQOUT (IEL, FACE) .LE.ZERO1 (1) ) GOTO 640  
!                                              ^^^^^^^^
      IADJ = ICMREF (IEL, FACE, 2)  
      IF (IADJ.GT.0) THEN  
         FADJ = ICMREF (IEL, FACE, 3)  
         QADJ = FNQOUT (IADJ, FADJ)  
!              * do both elements discharge into the same face?
         IF (QADJ.GT.ZERO1 (1) ) IQ (IEL) = 1  
!              * IEL must precede IADJ in the ISORT list
         JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL) )  
      ELSEIF (IADJ.LT.0) THEN  
         IBR = - IADJ  
         QMIN = ONE1 (1)  
         DO 630 P = 1, 3  
            IADJ = ICMRF2 (IBR, P, 1)  
            IF (IADJ.GT.0) THEN  
               FADJ = ICMRF2 (IBR, P, 2)  
               QADJ = FNQOUT (IADJ, FADJ)  
               QMIN = MIN (QADJ, QMIN)  
               IF (QADJ.LT.zero1 (1) ) THEN  
!                       * IEL must precede IADJ in the ISORT list
                  JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL) )  
               ENDIF  
            ENDIF  
  630          END DO  
!              * discharge from IEL has nowhere to go?
         IF (QMIN.GE.zero1 (1) ) IQ (IEL) = 2  
      ENDIF  
  640    END DO  
!        * Check QOC status at this FACE for all elements
   CALL ALCHKI (ERR, 2052, SPR, 1, NEL, FACE, IUNDEF, &
    'status_of_QOC(iel,face)', 'EQ', IZERO1, IQ, NERR, LDUM)
  650 END DO  
!     * Check that each donor element listed in ISORT occurs before
!       each of its receptors, and that all elements are listed
CALL ALCHKI (ERR, 2053, SPR, 1, NEL, IUNDEF, IUNDEF, &
 'position_in_ISORT(iel)', 'LTa', JMIN, JSORT (1) , NERR, LDUM)
!
!
! 7. Epilogue
! -----------
!
IF (NERR.GT.0) THEN  
!
   WRITE (SPR, 9100) 'DTUZ', DTUZ  
   WRITE (SPR, 9100) 'CLAI[veg=1,...,NV]', CLAI  
   WRITE (SPR, 9100) 'PLAI[veg=1,...,NV]', PLAI
   rdum(1:nlf)=ARXL(1:nlf)  !AD
   WRITE (SPR, 9100) 'ARXL[link=1,...,NLF]', (rdum (IEL) , IEL = 1, NLF)
   WRITE (SPR, 9100) 'DRAINA[col=NLF+1,...,NEL]', DRAINA  
   WRITE (SPR, 9100) 'PNETTO[col=NLF+1,...,NEL]', PNETTO  
   rdum(1:nel)=hrf(1:nel)  !AD
   WRITE (SPR, 9100) 'HRF[iel=1,...,NEL]', rdum(1:nel)
   WRITE (SPR, 9100) 'ZGRUND[iel=1,...,NEL]', ZGRUND  
   WRITE (SPR, 9200) 'ISORT[iel=1,...,NEL]', ISORT  
   WRITE (SPR, 9200) 'position_in_ISORT[iel=1,...,NEL]', (JSORT ( &
    IEL) , IEL = 1, NEL)
   DO 710 FACE = 1, 4  
      WRITE (SPR, 9150) 'QOC[iel=1,...,NEL][face=', FACE, ']', &
       (QOC (IEL, FACE) , IEL = 1, NEL)
  710    END DO  
!
CALL ERROR (ERR, 2003, SPR, 0, 0, 'Error(s) detected'//' while checking time-dependent WAT-SY interface')
!
ENDIF  
!
 9100 FORMAT(1X,A,     ':'/1P,(8E10.2))  
 9150 FORMAT(1X,A,I1,A,':'/1P,(8E10.2))  
 9200 FORMAT(1X,A,     ':'/   (16I5  ))  
!
END SUBROUTINE SYERR3




!SSSSSS SUBROUTINE SYFINE (DRSEDF, FBIC, FICRIT, NLF, ALPHA, DTSY, AREA, &
SUBROUTINE SYFINE (DRSEDF, FBIC, FICRIT, NLF, ALPHA, DTSY, AREA, &
 DCBF, FBETAF, FDELF, PBSED, TAUK, VCFMAX, VINFMX, BARM)
!
!----------------------------------------------------------------------*
!
! Evaluate quantities specific to fine sediment particles, associated
!  with settling, infiltration and armouring.
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR70
!  Module:  SY        Program:  SHETRAN
! Modifications:
!  RAH  15.07.94  Version 3.4.1 by AB/RAH. File created 28.3.94.
!----------------------------------------------------------------------*
USE CONST_SY  
!
! Commons and distributed constants
!
! Constants referenced
!     CONST.SY:  GRAVTY, RHOSED, RHOWAT, VISCOS
!
! Input arguments
INTEGER :: NLF  
DOUBLEPRECISION DRSEDF, FBIC, FICRIT, ALPHA, DTSY, AREA (NLF), &
 DCBF (NLF)
DOUBLEPRECISION PBSED (NLF), FBETAF (NLF), FDELF (NLF), TAUK (NLF)  
!
! Output arguments
DOUBLEPRECISION VCFMAX (NLF), VINFMX (NLF)  
LOGICAL :: BARM (NLF)  
!
! Locals, etc
DOUBLEPRECISION DUM, TAUEC, VMAX 
DOUBLEPRECISION ADOUBLEPRECISION, DCFMXL, FDELFL, TAUKL  
INTEGER :: LINK  
!
!----------------------------------------------------------------------*
!
!
!     * Calculate settling velocity for fines ( first call only )
IF (FIRST_syfine) THEN  
   FIRST_syfine = .FALSE.  
   WSED_syfine = DRSEDF**2 * GRAVTY * (RHOSED-RHOWAT) / (18 * RHOWAT * &
    VISCOS)
ENDIF  
!
!     * Loop over channel links
DO 100 LINK = 1, NLF  
!
   TAUKL = TAUK (LINK)  
   ADOUBLEPRECISION = AREA (LINK)  
   FDELFL = FDELF (LINK)  
!
!        * Calculate critical shear stress for fines
   CALL SYCRIT (0, DRSEDF, TAUKL, DUM, TAUEC)  
!
!        * Calculate potential fines in upper layer
!        *  (existing fines + settling)
   DUM = ALPHA * TAUEC  
   IF (DUM.GT.0) DUM = DIMJE(DUM, TAUKL) / DUM  
   DCFMXL = DCBF (LINK) + FDELFL * WSED_syfine * DUM * DTSY  
   VCFMAX (LINK) = DCFMXL * ADOUBLEPRECISION  
!
!        * Can fines be armoured ?
   BARM (LINK) = TAUKL.LE.TAUEC  
!
!        * Calculate potential infiltration rate
   VMAX = 0  
   IF (FBETAF (LINK) .LT.FBIC) VMAX = WSED_syfine * ADOUBLEPRECISION * DIMJE(FDELFL, &
    FICRIT / (1 - PBSED (LINK) ) ) * DTSY
   VINFMX (LINK) = VMAX  
!
!     * End of link loop
  100 END DO  
!
END SUBROUTINE SYFINE



!SSSSSS SUBROUTINE SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, &
SUBROUTINE SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, &
 NTSOBK, ARXL, DCBEDO, DLS, FBETA, DRSED, HRF, PBSED, PLS, SOSDFN, &
 THSAT, ZGRUND, NTSOTP, ZBFULL, ARBDEP, ARXLOL, DCBED, DCBSED, &
 DDBSED, DRSO50, DWATOL, FETA, GINFD, GINFS, GNU, GNUBK, QSED, &
 DBFULL)
!
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
INTEGER :: NEL, NELEE, NLF, NLFEE, NS, NSED, NSEE, NSEDEE  
INTEGER :: NTSOBK (NLFEE), NTSOTP (NLF + 1:NEL)
DOUBLEPRECISION ARXL (NLFEE), DCBEDO, DLS (NEL), DRSED (NSED)  
DOUBLEPRECISION FBETA (NELEE, NSED), HRF (NLF + 1:NEL), PBSED ( &
 NLFEE)
DOUBLEPRECISION PLS (NLF + 1:NEL), SOSDFN (NSEE, NSED), THSAT (NS)  
DOUBLEPRECISION ZBFULL (NLFEE), ZGRUND (NEL)  

!
! Output arguments
DOUBLEPRECISION ARBDEP (NLFEE), ARXLOL (NLFEE), DBFULL (NLFEE)  
DOUBLEPRECISION DCBED (NLFEE), DCBSED (NLFEE, NSED)  
DOUBLEPRECISION DDBSED (NLFEE, NSED), DRSO50 (NS), DWATOL (NLF + &
 1:NEL)
DOUBLEPRECISION FETA (NEL), GINFD (NLFEE, NSED), GINFS (NLFEE, &
 NSED)
DOUBLEPRECISION GNU (NLF + 1:NEL), GNUBK (NLFEE), QSED (NELEE, &
 NSEDEE, 4)
!
! Locals, etc
!
DOUBLEPRECISION DCBEDE, DDBEDE, DLSE, FBETAE  
INTEGER :: IEL, LINK, SED, SOIL, FACE  
!
!
!----------------------------------------------------------------------*
!
!
!     * Initialize surface erosion rates in each column
CALL ALINIT (ZERO, NEL - NLF, GNU (NLF + 1) )  
!
IF (NLF.GT.0) THEN  
!
!        * Initialize bank erosion rates in each link
   CALL ALINIT (ZERO, NLF, GNUBK)  
!
!        * Zero bed sediment accumulator
   CALL ALINIT (ZERO, NLF, ARBDEP)  
!
!        * Set old river c/s area equal to current river c/s area
   CALL DCOPY (NLF, ARXL, 1, ARXLOL, 1)  
!
ENDIF  
!
!
!     * Loop over sediment types
DO 200 SED = 1, NSED  
!
   IF (NLF.GT.0) THEN  
!
!           * Initialize infiltration rates
      CALL ALINIT (ZERO, NLF, GINFD (1, SED) )  
      CALL ALINIT (ZERO, NLF, GINFS (1, SED) )  
!
   ENDIF  
!
!        * Initialize sediment flow rates
   DO 100 FACE = 1, 4  
      CALL ALINIT (ZERO, NEL, QSED (1, SED, FACE) )  
  100    END DO  
!
!     * Next sediment type
  200 END DO  
!
!
!     * Loop over links
DO 400 LINK = 1, NLF  
   DLSE = DLS (LINK)  
!
!        * Set ratio of bank soil to bed sediment solid volume fractions
   FETA (LINK) = (1 - THSAT (NTSOBK (LINK) ) ) / (1 - PBSED (LINK) &
    )
!
!        * Set bank full depth
   DBFULL (LINK) = ZBFULL (LINK) - ZGRUND (LINK)  
!
!        * Bed layer depths
   DCBEDE = MIN (DLSE, DCBEDO)  
   DDBEDE = DIMJE(DLSE, DCBEDE)  
   DCBED (LINK) = DCBEDE  
!
!        * Loop over sediment types
   DO 300 SED = 1, NSED  
!
!           * Initialize sediment depths in both bed layers
      FBETAE = FBETA (LINK, SED)  
      DCBSED (LINK, SED) = DCBEDE * FBETAE  
      DDBSED (LINK, SED) = DDBEDE * FBETAE  
!
!        * Next sediment type
  300    END DO  
!
!     * Next link
  400 END DO  
!
!
!     * Loop over column elements
DO 500 IEL = NLF + 1, NEL  
!
!        * Set ratio: surface soil to loose sediment solid vol fractions
   FETA (IEL) = (1 - THSAT (NTSOTP (IEL) ) ) / (1 - PLS (IEL) )  
!
!        * Calculate initial surface water depth
   DWATOL (IEL) = HRF (IEL) - ZGRUND (IEL)  
!
  500 END DO  
!
!
!     * Calculate median particle diameter for each soil type
DO 600 SOIL = 1, NS  
   DRSO50 (SOIL) = SYDR (HALF, NSEE, NSED, SOSDFN (SOIL, 1), &
    DRSED)
  600 END DO  
!
!
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
  100 END DO  
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
  200    END DO  
   QSEDIN = - SUM * OMPBI  
!
!        * Volume of water remaining + advective water discharge
   SUM = 0  
   DO 300 JI = 1, NOUT  
      SUM = SUM + QSDWAE (SED, J (JI) )  
  300    END DO  
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
  400    END DO  
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
  500 END DO  
!
END SUBROUTINE SYLINK



!SSSSSS SUBROUTINE SYMAIN
SUBROUTINE SYMAIN (NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD, &
 ICMBK, ICMREF, ICMRF2, ICMXY, NBFACE, NLYR, NTSOIL, NVC, AREA, &
 CLENTH, CWIDTH, DHF, DXQQ, DYQQ, THSAT, ZBFULL, ZGRUND, BEXBK, &
 LINKNS, ISORT, DTUZ, TIH, UZNOW, ARXL, CLAI, DRAINA, HRF, PLAI, &
 PNETTO, QOC, NSED, PBSED, PLS, SOSDFN, ARBDEP, DLS, FBETA, FDEL, &
 GINFD, GINFS, GNU, GNUBK, QSED, DCBED, DCBSED, IDUM, DUMMY)
!
!----------------------------------------------------------------------*
!
! Controlling routine for the Sediment Yield module.
!
!----------------------------------------------------------------------*
! Version:  3.4.1          Notes:  SSR76
!  Module:  SY           Program:  SHETRAN
! Modifications:
!  RAH  04.10.94  Version 3.4.1. File created 23.12.93.
!----------------------------------------------------------------------*
! Commons and distributed constants
! Constants referenced
!     AL.P:  NELEE  NLFEE  NLYREE  NSEDEE  NSEE  NVEE  NXEE
!
!
! NB  Don't dimension arrays with NSED (undefined) or NLF (zero?)
!
! Input arguments
INTEGER :: NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD  
INTEGER :: ICMBK (NLFEE, 2), ICMREF (NELEE, 4, 2:3), ICMRF2 ( &
 NLFEE, 3, 2)
INTEGER :: ICMXY (NXEE, NY), NBFACE (NEL), NLYR (NLF + 1:NEL)  
INTEGER :: NTSOIL (NELEE, NLYREE), NVC (NLF + 1:NEL)  
INTEGER :: ISORT (NEL)  
DOUBLEPRECISION AREA (NEL), CLENTH (NLFEE), CWIDTH (NLFEE)  
DOUBLEPRECISION DHF (NELEE, 4), DXQQ (NLF + 1:NEL), DYQQ (NLF + 1: &
 NEL)
DOUBLEPRECISION THSAT (NS), ZBFULL (NLFEE), ZGRUND (NEL)  
DOUBLEPRECISION DTUZ, TIH, UZNOW  
DOUBLEPRECISION ARXL (NLFEE), CLAI (NV), DRAINA (NLF + 1:NEL), &
 HRF (NEL)
DOUBLEPRECISION PLAI (NV), PNETTO (NLF + 1:NEL), QOC (NELEE, 4)  
LOGICAL :: BEXBK, LINKNS (NLFEE)  
!
! Input/output arguments
INTEGER :: NSED  
DOUBLEPRECISION PBSED (NLFEE), PLS (NLF + 1:NEL), SOSDFN (NSEE, &
 NSEDEE)
DOUBLEPRECISION ARBDEP (NLFEE), DLS (NEL)  
DOUBLEPRECISION DCBED (NLFEE), DCBSED (NLFEE, NSEDEE)  
DOUBLEPRECISION FBETA (NELEE, NSEDEE), FDEL (NELEE, NSEDEE)  
!
! Output arguments
DOUBLEPRECISION GINFD (NLFEE, NSEDEE), GINFS (NLFEE, NSEDEE)  
DOUBLEPRECISION GNU (NLF + 1:NEL), GNUBK (NLFEE), QSED (NELEE, &
 NSEDEE, 4)
!
! Workspace arguments
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM  
DOUBLEPRECISION DUMMY (NELEE)  
!
! Locals, etc
!
CHARACTER (LEN=*) :: SYVER  
 
!
!        -- SY module version number --
PARAMETER (SYVER = '4.2.7')  
!        ------------------------------
!
!INTEGER :: ISACKW, ISGSED, ISSYOK, ISTEC, ISUSED, NEPS, NFINE, NSYB
!INTEGER :: NSYBCD (NSYBEE, 3), NSYC (4), NTSOBK (NLFEE)  
!
!INTEGER :: PASS, NTSOTP (NELEE)  
!
INTEGER :: FACE, FADJ, I, IADJ, IB, IBR, IEL, N, P, SED, SOIL  
INTEGER :: IDUM1A (NELEE), IDUM1X (NELEE+3)  
!
!DOUBLEPRECISION ALPHA, CONCOB, DCBEDO, FBIC, FICRIT, FPCRIT, SYNOW
!DOUBLEPRECISION DLSMAX, DDBSED (NLFEE, NSEDEE)  
!DOUBLEPRECISION ABC (NSEDEE, NSYCEE), ACKW (5, NSEDEE), ARXLOL (NLFEE)
!DOUBLEPRECISION BBC (NSEDEE, NSYCEE), BKB (NSEE)  
!DOUBLEPRECISION DBFULL (NLFEE)  
!DOUBLEPRECISION DRDRIP (NVEE), DRSED (NSEDEE), DRSO50 (NSEE)  
!DOUBLEPRECISION DWATOL (NELEE)  
!DOUBLEPRECISION FCG (NELEE), FCROCK (NELEE), FDRIP (NVEE), FETA (NELEE)
!DOUBLEPRECISION FPCLAY (NSEE)  
!DOUBLEPRECISION GBC (NSEDEE, NSYCEE), GKF (NSEE), GKR (NSEE)  
!DOUBLEPRECISION RHOSO (NSEE), XDRIP (NVEE)  
!
DOUBLEPRECISION DTSY  
DOUBLEPRECISION CONCI (NLFEE, NSEDEE), CONCIE (NSEDEE)  
DOUBLEPRECISION DCBSEE (NSEDEE), DCIPRE (NSEDEE)  
DOUBLEPRECISION DCIPRM (NLFEE, NSEDEE), DDBSEE (NSEDEE)  
DOUBLEPRECISION DDIPRE (NSEDEE), DDIPRM (NLFEE, NSEDEE)  
DOUBLEPRECISION DRDROP (NELEE), DUMSED (NLFEE * NSEDEE), DWAT1 ( &
 NELEE)
DOUBLEPRECISION EPSB (NLFEE)  
DOUBLEPRECISION FBETAE (NSEDEE), FCC (NVEE), FDELE (NSEDEE)  
DOUBLEPRECISION FQCONF (NLFEE, 3), GINFDE (NSEDEE), GINFSE ( &
 NSEDEE)
DOUBLEPRECISION LRAIN (NELEE)  
DOUBLEPRECISION QSDWAE (NSEDEE, 4), QSDWAT (NLFEE, NSEDEE, 4)  
DOUBLEPRECISION QSEDB (NSEDEE, NSYBEE), QSEDE (NSEDEE, 4)  
DOUBLEPRECISION QWAT (4), QWATB (NSYBEE)  
DOUBLEPRECISION SLOPEE (4), SLOPEJ (NELEE, 4), SOSDFE (NSEDEE)  
DOUBLEPRECISION TAUJ (NELEE, 4), TAUJE (4), TAUK (NELEE)  
DOUBLEPRECISION VCFMAX (NLFEE), VINFMX (NLFEE)  
!
LOGICAL :: DOUBT, BARM (NLFEE), LDUM (NELEE)  
!
!----------------------------------------------------------------------*
!
PASS_symain = PASS_symain + 1  
IF (PASS_symain.EQ.1) THEN  
!
!                     ---------------------
!--------------------- Initialization step ----------------------------*
!                     ---------------------
!
!        * Check array bounds & input variables
   CALL SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE, &
    NV, NVEE, NX, NXEE, NY, SPR, SYD)
!
!        * Check static/initializing input arrays
   CALL SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, NXEE, NYEE, &
    NY, SPR, BEXBK, LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, &
    NTSOIL, NVC, THSAT, CLENTH, CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, &
    DHF, ARXL, HRF (NLF + 1), ZGRUND, IDUM, IDUM1X, LDUM)
!
!        * Store top-layer soil type for each column element
   DO 100 IEL = NLF + 1, NEL  
      NTSOTP_symain (IEL) = NTSOIL (IEL, NLYR (IEL) )  
  100    END DO  
!
!        * Read SY input data file
   CALL SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, NELEE, &
    NLF, NLFEE, NS, NSEDEE, NSEE, NSYBEE, NSYCEE, NTSOTP_symain (NLF + 1), &
    NV, NX, NXEE, NYEE, NY, SPR, SYD, SYVER, ABC_symain, ALPHA_symain, BBC_symain, BKB_symain, &
    CONCOB_symain, DCBEDO_symain, DLS, DRDRIP_symain, DRSED_symain, DLSMAX_symain, FBETA, FBIC_symain, FCG_symain ( &
    NLF + 1), FCROCK_symain (NLF + 1), FDEL, FDRIP_symain, FICRIT_symain, FPCLAY_symain, &
    FPCRIT_symain, GBC_symain, GKF_symain, GKR_symain, ISACKW_symain, ISGSED_symain, ISSYOK_symain, ISTEC_symain, ISUSED_symain, &
    NEPS_symain, NFINE_symain, NSED, NSYB_symain, NSYBCD_symain, NSYC_symain, NTSOBK_symain, PBSED, PLS, &
    RHOSO_symain, SOSDFN, XDRIP_symain, IDUM, DUMMY, DUMSED)
!
!        * Check SY input data
   CALL SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, &
    NV, NSYB_symain, NSYBEE, NSYC_symain, NSYCEE, SPR, ICMREF, ISUSED_symain, NEPS_symain, &
    NFINE_symain, SFB, SRB, ALPHA_symain, DCBEDO_symain, FPCRIT_symain, DLSMAX_symain, NTSOBK_symain, NSYBCD_symain, &
    NBFACE, DRSED_symain, BKB_symain, GKF_symain, GKR_symain, RHOSO_symain, SOSDFN, DRDRIP_symain, FDRIP_symain, &
    XDRIP_symain, PBSED, FCG_symain (NLF + 1), FCROCK_symain (NLF + 1), PLS, DLS, FBETA, &
    FDEL, ABC_symain, BBC_symain, GBC_symain, IDUM, DUMMY, LDUM)
!
!        * Static variables and initialization
   CALL SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, &
    NTSOBK_symain, ARXL, DCBEDO_symain, DLS, FBETA, DRSED_symain, HRF (NLF + 1), &
    PBSED, PLS, SOSDFN, THSAT, ZGRUND, NTSOTP_symain (NLF + 1), ZBFULL, &
    ARBDEP, ARXLOL_symain, DCBED, DCBSED, DDBSED_symain, DRSO50_symain, DWATOL_symain (NLF + 1) &
    , FETA_symain, GINFD, GINFS, GNU, GNUBK, QSED, DBFULL_symain)
!
!
!------------------- End of initialization step -----------------------*
!
ELSE  
!                      -----------------
!---------------------- Simulation step -------------------------------*
!                      -----------------
!
!
! Check Input
! -----------
!
!        * Check time-varying input variables
   DOUBT = ISSYOK_symain.GT.0  
   IF (DOUBT) DOUBT = MOD (PASS_symain - 2, ISSYOK_symain) .EQ.0  
   IF (DOUBT) CALL SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, &
    ICMREF, ICMRF2, ISORT, DTUZ, CLAI, PLAI, ARXL, DRAINA, PNETTO, &
    HRF, ZGRUND, QOC, IDUM, IDUM1A, IDUM1X, LDUM)
!
!
! Quantities Independent of Sub-timestep
! --------------------------------------
!
!        * Water-flow related variables

   CALL SYWAT (NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, &
    DHF, DRDRIP_symain, LINKNS, ZBFULL, ZGRUND, CLAI, DRAINA, HRF, PLAI, &
    PNETTO, QOC, DRDROP (NLF + 1), DWAT1, FCC, FQCONF, LRAIN (NLF + &
    1), SLOPEJ, TAUJ, TAUK)
!
!        * Erosion rates for all column elements

   CALL SYOVER (ISTEC_symain, NEL, NLF, NS, NV, FCC, LRAIN (NLF + 1), &
    XDRIP_symain, DRDRIP_symain, FDRIP_symain, DRAINA, GKR_symain, DWAT1 (NLF + 1), DRDROP ( &
    NLF + 1), FCG_symain (NLF + 1), FCROCK_symain (NLF + 1), DRSO50_symain, TAUK (NLF + &
    1), FPCLAY_symain, GKF_symain, RHOSO_symain, NTSOTP_symain (NLF + 1), NVC, GNU, DUMMY, DLS, &
    DLSMAX_symain)
!
!        * Erosion rates for all link elements

   IF (NLF.GT.0) CALL SYBKER (ISTEC_symain, NLF, NS, FPCLAY_symain, RHOSO_symain, &
    DRSO50_symain, TAUK, CWIDTH, DWAT1, BKB_symain, NTSOBK_symain, FETA_symain, CLENTH, DBFULL_symain, &
    EPSB, GNUBK)
!
!
!
! SY Sub-timestep Loop
! --------------------
!
   DTSY = DTUZ / NEPS_symain  
   DO 290 N = 1, NEPS_symain  
!
!
!           Initialization
!           --------------
!
      DO 150 FACE = 1, 4  
         DO 140 SED = 1, NSED  
            CALL ALINIT (ZERO, NEL, QSED (1, SED, FACE) )  
  140          END DO  
  150       END DO  
!
!
!           Boundary Conditions
!           -------------------
!
      IF (NSYB_symain.GT.0) THEN  
!
!              * Gather water "outflow" rates (should be negative)
         DO 210 IB = 1, NSYB_symain  
            IEL = NSYBCD_symain (IB, 1)  
            FACE = NBFACE (IEL)  
            QWATB (IB) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)  
  210          END DO  
!
!              * Read time-varying flux data & calculate sediment flows
         CALL SYBC  
!
!              * Load boundary flows into QSED array
         DO 220 IB = 1, NSYB_symain  
            IEL = NSYBCD_symain (IB, 1)  
            FACE = NBFACE (IEL)  
            CALL DCOPY (NSED, QSEDB (1, IB), 1, QSED (IEL, 1, &
             FACE), NELEE)
  220          END DO  
!
      ENDIF  
!
!
!           Quantities Independent of Sediment Flux
!           ---------------------------------------
!
      IF (NLF.GT.0) THEN  
!
!              * Transport capacity & advection coefficients
         CALL SYCLTR (CONCOB_symain, FPCRIT_symain, ISACKW_symain, ISUSED_symain, NELEE, &
          NFINE_symain, NLF, NLFEE, NSED, NSEDEE, DRSED_symain (NFINE_symain+1), &
          ARXL, CWIDTH, DCBED, LINKNS, DWAT1, QOC, SLOPEJ, DCBSED ( &
          1, NFINE_symain+1), FDEL (1, NFINE_symain+1), TAUJ, ACKW_symain (1, NFINE_symain+1), &
          CONCI, QSDWAT, DUMMY, DUMSED)
!
!              * Settling, infiltration & armouring
         IF (NFINE_symain.GT.0) CALL SYFINE (DRSED_symain (1), FBIC_symain, FICRIT_symain, &
          NLF, ALPHA_symain, DTSY, AREA, DCBSED, FBETA, FDEL, PBSED, TAUK, &
          VCFMAX, VINFMX, BARM)
!
      ENDIF  
!
!
!           One Element at a Time
!           ---------------------
!
      DO 270 I = 1, NEL  
         IEL = ISORT (I)  
!
!              * Gather common sub-arrays
         CALL DCOPY (NSED, FDEL (IEL, 1), NELEE, FDELE, 1)  
         DO 225 FACE = 1, 4  
            QWAT (FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)  
            CALL DCOPY (NSED, QSED (IEL, 1, FACE), NELEE, QSEDE ( &
             1, FACE), 1)
  225          END DO  
!
         IF (IEL.LE.NLF) THEN  
!
!                 ** Link element **
!
!                 * Gather link-specific sub-arrays
            SOIL = NTSOBK_symain (IEL)  
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)  
            CALL DCOPY (NSED, CONCI (IEL, 1), NLFEE, CONCIE, 1)  
            CALL DCOPY (NSED, DCBSED (IEL, 1), NLFEE, DCBSEE, 1)  
            CALL DCOPY (NSED, DDBSED_symain (IEL, 1), NLFEE, DDBSEE, 1)  
            DO 226 FACE = 1, 4  
               CALL DCOPY (NSED, QSDWAT (IEL, 1, FACE), NLFEE, &
                QSDWAE (1, FACE), 1)
  226             END DO  
!
!                 * Solve transport equation
            CALL SYLINK (NFINE_symain, NSED, NSEDEE, DTSY, AREA (IEL), &
             ARXLOL_symain (IEL), ARXL (IEL), CLENTH (IEL), EPSB (IEL), &
             PBSED (IEL), VINFMX (IEL), BARM (IEL), VCFMAX (IEL), &
             CONCIE, DCBSEE, DDBSEE, QSDWAE, QWAT, SOSDFE, FDELE, &
             QSEDE, DCIPRE, DDIPRE, GINFDE, GINFSE)
!
!                 * Scatter link-specific results
            CALL DCOPY (NSED, DCIPRE, 1, DCIPRM (IEL, 1), NLFEE)  
            CALL DCOPY (NSED, DDIPRE, 1, DDIPRM (IEL, 1), NLFEE)  
            CALL DCOPY (NSED, GINFDE, 1, GINFD (IEL, 1), NLFEE)  
            CALL DCOPY (NSED, GINFSE, 1, GINFS (IEL, 1), NLFEE)  
!
         ELSE  
!
!                 ** Column element **
!
!                 * Gather column-specific sub-arrays
            SOIL = NTSOTP_symain (IEL)  
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)  
            CALL DCOPY (NSED, FBETA (IEL, 1), NELEE, FBETAE, 1)  
            CALL DCOPY (4, SLOPEJ (IEL, 1), NELEE, SLOPEE, 1)  
            CALL DCOPY (4, TAUJ (IEL, 1), NELEE, TAUJE, 1)  
!
!                 * Solve transport equation for this column element
            CALL SYCOLM (AREA (IEL), DTSY, DWAT1 (IEL), DWATOL_symain ( &
             IEL), DXQQ (IEL), DYQQ (IEL), FETA_symain (IEL), GNU (IEL), &
             ISGSED_symain, NSED, FPCRIT_symain, PLS (IEL), NSEDEE, DRSED_symain, QWAT, &
             SLOPEE, SOSDFE, TAUJE, DLS (IEL), FBETAE, FDELE, &
             QSEDE, DUMMY, DUMSED)
!
!                 * Scatter column-specific results
            CALL DCOPY (NSED, FBETAE, 1, FBETA (IEL, 1), NELEE)  
!
         ENDIF  
!
!              * Scatter common results ...
         CALL DCOPY (NSED, FDELE, 1, FDEL (IEL, 1), NELEE)  
         DO 260 FACE = 1, 4  
            CALL DCOPY (NSED, QSEDE (1, FACE), 1, QSED (IEL, 1, &
             FACE), NELEE)
!
!                 ... and propagate sediment flow rates at outflow faces
            IF (QWAT (FACE) .GT.ZERO) THEN  
               IADJ = ICMREF (IEL, FACE, 2)  
               IF (IADJ.GT.0) THEN  
!                       * regular neighbour
                  FADJ = ICMREF (IEL, FACE, 3)  
                  DO 240 SED = 1, NSED  
                     QSED (IADJ, SED, FADJ) = - QSEDE (SED, FACE)  
  240                   END DO  
               ELSEIF (IADJ.LT.0) THEN  
!                       * neighbour is a confluence node
                  IBR = - IADJ  
                  DO 255 P = 1, 3  
                     IADJ = ICMRF2 (IBR, P, 1)  
                     IF (IADJ.GT.0) THEN  
!                             * prospect is active
                        FADJ = ICMRF2 (IBR, P, 2)  
                        DO 250 SED = 1, NSED  
                           QSED (IADJ, SED, FADJ) = QSED (IADJ, &
                            SED, FADJ) - QSEDE (SED, FACE) * &
                            FQCONF (IBR, P)
  250                         END DO  
                     ENDIF  
  255                   END DO  
               ENDIF  
            ENDIF  
!
  260          END DO  
!
  270       END DO  
!
!
!           Channel Bed Update
!           ------------------
!
      IF (NLF.GT.0) CALL SYBED (DCBEDO_symain, NELEE, NLF, NLFEE, NSED, &
       CWIDTH, DCIPRM, DDIPRM, ARBDEP, DLS, FBETA, DCBSED, DDBSED_symain, &
       DCBED)
!
!
!           Store Old-time Values & Update Timer
!           ------------------------------------
!
      CALL DCOPY (NEL - NLF, DWAT1 (NLF + 1), 1, DWATOL_symain (NLF + 1), &
       1)
      IF (NLF.GT.0) CALL DCOPY (NLF, ARXL, 1, ARXLOL_symain, 1)  
      SYNOW_symain = SYNOW_symain + DTSY / 36D2  
!
  290    END DO  
!
!
!--------------------- End of simulation step -------------------------*
!
ENDIF  
!
!
! Epilogue
! --------
!
!     Ensure that current time value is exactly correct
SYNOW_symain = UZNOW  
!
!
END SUBROUTINE SYMAIN



!SSSSSS SUBROUTINE SYOVER



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
  100 END DO  
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
  200 END DO  
!
END SUBROUTINE SYOVER



!SSSSSS SUBROUTINE SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, &
SUBROUTINE SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, &
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
USE CONST_SY  
! Commons and distributed constants
!
! Constants referenced
!    CONST.SY: GRAVTY, RHOSED, RHOWAT
!
! Input arguments
INTEGER :: ISGSED, NSED
DOUBLEPRECISION DXQQE, DYQQE, DWAT1E, VDSED (NSED), DRSED (NSED), K2 
DOUBLEPRECISION QWAT (4), SLOPEE (4), TAUJE (4)  
  
!
! Output arguments
DOUBLEPRECISION GJSUM  
!
! Locals, etc
!
DOUBLEPRECISION FLJ  
DOUBLEPRECISION AJ, DRD50, FTAU, DUM, DYMXQQ, GJ, GSUM  
DOUBLEPRECISION LJ, TAUEC, TAUJEE  
INTEGER :: FACE, NOUT, I, J (4)  

!
!     * Face length function ( DXQQE at evens, DYQQE at odds )
FLJ (FACE) = MOD (FACE, 2) * DYMXQQ + DXQQE  
!
!----------------------------------------------------------------------*
!
! Preliminaries
! -------------
!
!     * Constants
IF (FIRST_syovtr) THEN  
   K1_syovtr = 0.05d0 * RHOWAT**2 / ( (RHOSED-RHOWAT) **2 * SQRT (GRAVTY) )  
   K3_syovtr = 2.45d0 * (RHOSED / RHOWAT) ** ( - 0.4d0) / SQRT ( (RHOSED- &
    RHOWAT) * GRAVTY)
   K4_syovtr = 0.635d0 / SQRT (RHOWAT)  
   FIRST_syovtr = .FALSE.  
ENDIF  
!
!     * Initialize variables
GSUM = 0  
DYMXQQ = DYQQE-DXQQE  
!
!     * Obtain median diameter of sediment available for discharge
DRD50 = SYDR (HALF, 1, NSED, VDSED, DRSED)  
!
!     * Count and record faces with outflow
NOUT = 0  
DO 100 FACE = 1, 4  
   IF (QWAT (FACE) .GT.0) THEN  
      NOUT = NOUT + 1  
      J (NOUT) = FACE  
   ENDIF  
  100 END DO  
!
!
! Transport Capacity
! ------------------
!
IF (ISGSED.EQ.1.AND.DWAT1E.GT.0) THEN  
!
!
!        ^^^ ENGELUND-HANSEN METHOD ^^^
!
!        * Precalculate constant over faces (note K2 may be very small)
   K2 = SQRT (DWAT1E) * DRD50  
!
!        * Loop over faces with outflow
   DO 200 I = 1, NOUT  
      FACE = J (I)  
!
!           * Discharge capacity at this face
      LJ = FLJ (FACE)  
      GJ = (K1_syovtr * QWAT (FACE) **2 * SLOPEE (FACE) **1.5) / (LJ * &
       K2)
!
!           * Accumulated discharge capacity for this element
      GSUM = GSUM + GJ  
!
  200    END DO  
!
!
ELSEIF (ISGSED.EQ.0) THEN  
!
!
!        ^^^^^^^ YALIN METHOD ^^^^^^^^^
!
!        * Loop over faces with outflow
   DO 300 I = 1, NOUT  
      FACE = J (I)  
!
!           * Get face length
      LJ = FLJ (FACE)  
!
!           * Obtain critical shear stress at the ground surface
      TAUJEE = TAUJE (FACE)  
      CALL SYCRIT (0, DRD50, TAUJEE, DUM, TAUEC)  
!
!           * Calculate discharge capacity at this face
      FTAU = DIMJE(TAUJEE, TAUEC) / TAUEC  
      AJ = K3_syovtr * SQRT (TAUEC / DRD50)  
      GJ = K4_syovtr * SQRT (TAUJEE) * DRD50 * LJ * (FTAU - LOG (1 + AJ * &
       FTAU) / AJ)
!
!           * Accumulated capacity for this element
      GSUM = GSUM + GJ  
!
  300    END DO  
!
!
ELSE  
!
!        ^^^ Zero capacity ^^^
!
ENDIF  
!
GJSUM = GSUM  
!
END SUBROUTINE SYOVTR



!SSSSSS SUBROUTINE SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, &
SUBROUTINE SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, &
 NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, NSYBEE, NSYCEE, NTSOTP, NV, &
 NX, NXEE, NYEE, NY, SPR, SYD, SYVER, ABC, ALPHA, BBC, BKB, CONCOB, &
 DCBEDO, DLS, DRDRIP, DRSED, DLSMAX, FBETA, FBIC, FCG, FCROCK, &
 FDEL, FDRIP, FICRIT, FPCLAY, FPCRIT, GBC, GKF, GKR, ISACKW, &
 ISGSED, ISSYOK, ISTEC, ISUSED, NEPS, NFINE, NSED, NSYB, NSYBCD, &
 NSYC, NTSOBK, PBSED, PLS, RHOSO, SOSDFN, XDRIP, IDUM, DUMMY, &
 DUMSED)
!
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
! Input arguments
INTEGER :: NEL, NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, NSYBEE, &
 NSYCEE
 INTEGER :: NTSOTP (NLF + 1:NEL), NV, NX, NXEE, NYEE, NY, SYD, SPR
 INTEGER :: ICMBK (NLFEE, 2), ICMREF (NELEE, 4, 2:2), ICMXY (NXEE, &
 NY)
LOGICAL :: BEXBK, LINKNS (NLFEE)  
CHARACTER (LEN=*) :: SYVER  
!
! Output arguments
INTEGER :: ISACKW, ISGSED, ISSYOK, ISTEC, ISUSED, NEPS, NFINE  
INTEGER :: NSED, NSYB, NSYBCD (NSYBEE, 3), NSYC (4), NTSOBK ( &
 NLFEE)
DOUBLEPRECISION ABC (NSEDEE, NSYCEE), ALPHA, BBC (NSEDEE, NSYCEE)  
DOUBLEPRECISION BKB (NS), CONCOB, DCBEDO, DLS (NEL), DRDRIP (NV)  
DOUBLEPRECISION DRSED (NSEDEE), FBETA (NELEE, NSEDEE), FBIC  
DOUBLEPRECISION FCG (NLF + 1:NEL), FCROCK (NLF + 1:NEL)  
DOUBLEPRECISION FDEL (NELEE, NSEDEE), FDRIP (NV), FICRIT  
DOUBLEPRECISION FPCLAY (NS), FPCRIT, GBC (NSEDEE, NSYCEE), &
 GKF (NS)
DOUBLEPRECISION GKR (NS), PBSED (NLFEE), PLS (NLF + 1:NEL), &
 RHOSO (NS)
DOUBLEPRECISION SOSDFN (NSEE, NSEDEE), XDRIP (NV)  
DOUBLEPRECISION DLSMAX  
!
! Workspace arguments
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM  
DOUBLEPRECISION DUMMY (NELEE), DUMSED (NLFEE * NSEDEE)  
!
! Locals, etc
INTEGER :: FATAL, WARN  
PARAMETER (FATAL = 1, WARN = 3)  
!
CHARACTER(80)  :: CDUM 
CHARACTER(132) :: MSG
CHARACTER(8)   ::  SYDVER
INTEGER :: BB, IDUM0, I0, IEL, ICAT, ITYPE, NC, NCAT, NNN, NREQ, &
 SED, SOIL
!
!----------------------------------------------------------------------*
!
!
! 0. Preliminaries
! ----------------
!
!     * Check status of data file
CALL ALREAD (0, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)  
!
!     * Print SY job title
CALL ALREAD (1, SYD, SPR, ':SY01', 1, 1, IDUM0, CDUM, IDUM, DUMMY)  
WRITE (SPR, '(/1X,A/)') CDUM  
!
!     * Check & print version number
CALL ALREAD (1, SYD, SPR, ':SY02', 1, 1, IDUM0, SYDVER, IDUM, &
 DUMMY)
!     * [miss off last character to allow eg '3.4.1' is ok in '3.4.1a' ]
IF (INDEX (SYDVER, SYVER (:LEN (SYVER) - 1) ) .EQ.0) THEN  
   WRITE (MSG, 9011) SYVER, SYDVER  
   CALL ERROR (WARN, 2011, SPR, 0, 0, MSG)  
ELSE  
   WRITE (SPR, '(4X,2A/)') 'SY Module Version ', SYVER  
ENDIF  
!
!
! 1. Static Variables
! -------------------
!
!     * Check workspace array size: part 1
NREQ = 8  
IF (NELEE.LT.NREQ) GOTO 8000  
!
!     * Integer
NNN = 5  
IF (NLF.GT.0) NNN = 8  
CALL ALREAD (2, SYD, SPR, ':SY11', NNN, 1, IDUM0, CDUM, IDUM, &
 DUMMY)
NSED = IDUM (1)  
ISGSED = IDUM (2)  
ISTEC = IDUM (3)  
ISSYOK = IDUM (4)  
NEPS = IDUM (5)  
IF (NLF.GT.0) THEN  
   ISACKW = IDUM (6)  
   ISUSED = IDUM (7)  
   NFINE = IDUM (8)  
ENDIF  
IF (NSED.LT.1.OR.NSED.GT.NSEDEE) GOTO 8110  
!
!     * Floating-point
NNN = 2  
IF (NLF.GT.0) NNN = 7  
CALL ALREAD (3, SYD, SPR, ':SY12', NNN, 1, IDUM0, CDUM, IDUM, &
 DUMMY)
FPCRIT = DUMMY (1)  
DLSMAX = DUMMY (2)  
IF (NLF.GT.0) THEN  
   ALPHA = DUMMY (3)  
   CONCOB = DUMMY (4)  
   DCBEDO = DUMMY (5)  
   FBIC = DUMMY (6)  
   FICRIT = DUMMY (7)  
ENDIF  
!
!
! 2. Sediment, Soil & Vegetation Properties
! -----------------------------------------
!
!     * Check workspace array size: part 2
NREQ = MAX (MAX (5, NSED) * NS, 3 * NV)  
IF (NELEE.LT.NREQ) GOTO 8000  
!
!     * Sediment
CALL ALREAD (3, SYD, SPR, ':SY21', NSED, 1, IDUM0, CDUM, IDUM, &
 DRSED)
!
!     * Soil
CALL ALREAD (3, SYD, SPR, ':SY22', 5, NS, IDUM0, CDUM, IDUM, &
 DUMMY)
CALL DCOPY (NS, DUMMY (1), 5, GKR, 1)  
CALL DCOPY (NS, DUMMY (2), 5, GKF, 1)  
CALL DCOPY (NS, DUMMY (3), 5, RHOSO, 1)  
CALL DCOPY (NS, DUMMY (4), 5, FPCLAY, 1)  
CALL DCOPY (NS, DUMMY (5), 5, BKB, 1)  
!
!     * Soil composition
CALL ALREAD (3, SYD, SPR, ':SY23', NSED, NS, IDUM0, CDUM, IDUM, &
 DUMMY)
DO 200 SED = 1, NSED  
   CALL DCOPY (NS, DUMMY (SED), NSED, SOSDFN (1, SED), 1)  
  200 END DO  
!
!     * Vegetation
CALL ALREAD (3, SYD, SPR, ':SY24', 3, NV, IDUM0, CDUM, IDUM, &
 DUMMY)
CALL DCOPY (NV, DUMMY (1), 3, XDRIP, 1)  
CALL DCOPY (NV, DUMMY (2), 3, DRDRIP, 1)  
CALL DCOPY (NV, DUMMY (3), 3, FDRIP, 1)  
!
!
! 3. Link Element Properties
! --------------------------
!
IF (NLF.GT.0) THEN  
!
!        * Bank soil type
   CALL ALREAD (2, SYD, SPR, ':SY31', NLF, 1, IDUM0, CDUM, NTSOBK, &
    DUMMY)
!
!        * Porosity of bed sediment
   CALL ALREAD (3, SYD, SPR, ':SY32', NLF, 1, IDUM0, CDUM, IDUM, &
    PBSED)
!
ENDIF  
!
!
! 4. Column-element Properties
! ----------------------------
!
!     * Ground cover
CALL ALALLF (1, 1, 0, SYD, SPR, ':SY41', NEL, NLF, NX, NY, NELEE, &
 NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, FCG, IDUM, &
 DUMMY)
!
!     * Rock cover
CALL ALALLF (1, 1, 0, SYD, SPR, ':SY42', NEL, NLF, NX, NY, NELEE, &
 NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, FCROCK, &
 IDUM, DUMMY)
!
!     * Porosity of loose sediment
CALL ALALLF (1, 1, 0, SYD, SPR, ':SY43', NEL, NLF, NX, NY, NELEE, &
 NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, PLS, IDUM, &
 DUMMY)
!
!
! 5. All-element Initialization
! -----------------------------
!
!     * Initial depth of loose/bed sediment
CALL ALALLF (0, 1, 0, SYD, SPR, ':SY51', NEL, NLF, NX, NY, NELEE, &
 NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, DLS, IDUM, &
 DUMMY)
!
!     * Initial composition of loose/bed sediment ...
CALL ALALLF (0, NSED, - 1, SYD, SPR, ':SY52', NEL, NLF, NX, NY, &
 NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, &
 FBETA, IDUM, DUMMY)
!
!     ... with special option to inherit composition of soil
IF (NCAT.LT.0) THEN  
   DO 510 IEL = 1, NLF  
      SOIL = NTSOBK (IEL)  
      CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), &
       NELEE)
  510    END DO  
   DO 520 IEL = NLF + 1, NEL  
      SOIL = NTSOTP (IEL)  
      CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), &
       NELEE)
  520    END DO  
ENDIF  
!
!     * Initial concentrations of suspended sediment
CALL ALALLF (0, NSED, 0, SYD, SPR, ':SY53', NEL, NLF, NX, NY, &
 NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, &
 FDEL, IDUM, DUMMY)
!
!
! 6. Boundary Data
! ----------------
!
!     * (see workspace check above)
!
!     * No of inflow boundary elements & no of categories of each type
CALL ALREAD (2, SYD, SPR, ':SY61', 5, 1, IDUM0, CDUM, IDUM, DUMMY)  
NSYB = IDUM (1)  
DO 600 ITYPE = 1, 4  
   NSYC (ITYPE) = IDUM (1 + ITYPE)  
  600 END DO  
!
IF (NSYB.GT.0) THEN  
!
   IF (NSYB.GT.NSYBEE) GOTO 8610  
!
!        * Check workspace array size: part 3
   NREQ = MAX (3 * NSYB, NSED * NSYC (1), NSED * 2 * NSYC (3) )  
   IF (NELEE.LT.NREQ) GOTO 8000  
!
!        * Integer boundary data
   CALL ALREAD (2, SYD, SPR, ':SY62', 3, NSYB, IDUM0, CDUM, IDUM, &
    DUMMY)
   I0 = 0  
   DO 610 BB = 1, NSYB  
      IEL = IDUM (I0 + 1)  
      ITYPE = IDUM (I0 + 2)  
      ICAT = IDUM (I0 + 3)  
      IF (ITYPE.LT.1.OR.ITYPE.GT.4) GOTO 8620  
!           * condense 4 into 2 by adding cats 2 & 4 to lists for 1 & 3
      IF (MOD (ITYPE, 2) .EQ.0) ICAT = ICAT + NSYC (ITYPE-1)  
      NSYBCD (BB, 1) = IEL  
      NSYBCD (BB, 2) = ITYPE  
      NSYBCD (BB, 3) = ICAT  
      I0 = I0 + 3  
  610    END DO  
!
!        * Steady flux data
   NC = NSYC (1)  
   IF (NC.GT.0) THEN  
      IF (NC.GT.NSYCEE) GOTO 8612  
      CALL ALREAD (3, SYD, SPR, ':SY63', NSED, NC, IDUM0, CDUM, &
       IDUM, DUMMY)
      DO 620 SED = 1, NSED  
         CALL DCOPY (NC, DUMMY (SED), NSED, GBC (SED, 1), NSEDEE)  
  620       END DO  
   ENDIF  
!
!        * Steady rating curve data
   NC = NSYC (3)  
   IF (NC.GT.0) THEN  
      IF (NC.GT.NSYCEE) GOTO 8614  
      CALL ALREAD (3, SYD, SPR, ':SY64', NSED * 2, NC, IDUM0, &
       CDUM, IDUM, DUMMY)
      DO 630 SED = 1, NSED  
         CALL DCOPY (NC, DUMMY (2 * SED-1), 2 * NSED, ABC (SED, 1) &
          , NSEDEE)
         CALL DCOPY (NC, DUMMY (2 * SED), 2 * NSED, BBC (SED, 1), &
          NSEDEE)
  630       END DO  
   ENDIF  
!
ENDIF  
!
!
! 7. Epilogue
! -----------
!
!     * Close the data file
CALL ALREAD ( - 1, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, &
 DUMMY)
!
RETURN  
!
!
! Error Branches & Formats
! ------------------------
!
!     * Insufficient workspace
 8000 WRITE (MSG, 9005) NELEE, NREQ  
CALL ERROR (FATAL, 2005, SPR, 0, 0, MSG)  
!
!     * NSED not in [1,NSEDEE]
 8110 WRITE (MSG, 9006) NSED, NSEDEE  
CALL ERROR (FATAL, 2006, SPR, 0, 0, MSG)  
!
!     * NSYB > NSYBEE
 8610 WRITE (MSG, 9007) NSYB, NSYBEE  
CALL ERROR (FATAL, 2007, SPR, 0, 0, MSG)  
!
!     * NSYC(1) > NSYCEE
 8612 WRITE (MSG, 9009) NSYC (1), NSYCEE  
CALL ERROR (FATAL, 2009, SPR, 0, 0, MSG)  
!
!     * NSYC(3) > NSYCEE
 8614 WRITE (MSG, 9010) NSYC (3), NSYCEE  
CALL ERROR (FATAL, 2010, SPR, 0, 0, MSG)  
!
!     * ITYPE is not in the range [1,4]
 8620 WRITE (MSG, 9008) BB, ITYPE  
CALL ERROR (FATAL, 2008, SPR, 0, 0, MSG)  
!
!
 9003 FORMAT ( 1X,A )  
!
 9005 FORMAT ('Workspace available is NELEE = ', I5, &
&        '; workspace required in subroutine SYREAD is ',I6 )
!
 9006 FORMAT ('No. of size groups NSED=',I4, &
&        ' is not in range [1,NSEDEE=',I3,']')
!
 9007 FORMAT ('No. of boundaries NSYB=',I5, &
&        ' is greater than NSYBEE=',I4,']')
!
 9008 FORMAT ('Boundary type NSYBCD(',I4,',2)=',I2, &
&        ' is not is the range [1,4]')
!
 9009 FORMAT ('No. of steady flux categories NSYC(1)=',I4, &
&        ' is greater than NSYCEE=',I3,']')
!
 9010 FORMAT ('No. of steady rating categories NSYC(3)=',I4, &
&        ' is greater than NSYCEE=',I3,']')
!
 9011 FORMAT ('SY module is version ',A,'; SYD data file is version ',A)  
!
!
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
! Commons and distributed constants
USE CONST_SY
!
! Constants referenced
!     CONST.SY:   GRAVTY  RHOWAT
!
! Input arguments
! NB: Don't use NLF as array size: it may be zero
INTEGER :: NEL, NELEE, NLF, NLFEE, NV
INTEGER :: ICMREF (NELEE, 4, 2:3), ICMRF2 (NLFEE, 3, 2)
 INTEGER :: NVC (NLF + 1:NEL)
DOUBLEPRECISION CLAI (NV), DHF (NELEE, 4), DRAINA (NLF + 1:NEL)  
DOUBLEPRECISION DRDRIP (NV), HRF (NEL), PLAI (NV), PNETTO (NLF + &
 1:NEL)
DOUBLEPRECISION QOC (NELEE, 4), ZBFULL (NLFEE), ZGRUND (NEL)  

  
LOGICAL :: LINKNS (NLFEE)  
!
! Output arguments
DOUBLEPRECISION DRDROP (NLF + 1:NEL), DWAT1 (NEL), FCC (NV)  
DOUBLEPRECISION FQCONF (NLFEE, 3), LRAIN (NLF + 1:NEL)  
DOUBLEPRECISION SLOPEJ (NELEE, 4), TAUJ (NELEE, 4), TAUK (NEL)  
! NB: FQCONF defined only for branches flowing INTO a node;
!     SLOPEJ & TAUJ not defined at side faces of links.
!
! Locals, etc
DOUBLEPRECISION DRDMIN  
PARAMETER (DRDMIN = 1D-4)  
!
DOUBLEPRECISION DRAINE, DWAT1E, FCCE, HRFE, PNETTE, SLOPEE, TAUJE  
DOUBLEPRECISION D, DA, DE, HA, HE, L  
DOUBLEPRECISION Q, QABS, QMAX, QOUT, QOUTX (0:3), QSUM, TAUMAX, &
 ZBF
DOUBLEPRECISION FQOUT  
INTEGER :: FACE, IADJ, IBR, ICOL, IEL, IELP  
INTEGER :: KADJ, KEL, KELP, LINK, P, PADJ, PIN, POUT, VEG  
LOGICAL :: BSIDE  
!
FQOUT (IEL, FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)  
!
!----------------------------------------------------------------------*
!
! Loop over Vegetation Types
! --------------------------
!
!     * Calculate ground fraction sheltered from rain by canopy
DO 100 VEG = 1, NV  
   FCC (VEG) = PLAI (VEG) * MIN (CLAI (VEG), ONE)  
  100 END DO  
!
!
! Loop over Column Elements
! -------------------------
!
DO 200 ICOL = NLF + 1, NEL  
!
!        * Avoid multiple array references
   DRAINE = DRAINA (ICOL)  
   PNETTE = PNETTO (ICOL)  
   VEG = NVC (ICOL)  
   FCCE = FCC (VEG)  
!
!        * Calculate median raindrop/leaf-drip diameter
   D = DRDMIN  
   IF (PNETTE.GT.ZERO) D = MAX (D, DRDRIP (VEG) * (DRAINE / &
    PNETTE), 0.01935d0 * PNETTE**0.182d0)
   DRDROP (ICOL) = D  
!
!        * Calculate rainfall rate
   L = ZERO  
   IF (FCCE.LT.ONE) L = DIMJE(PNETTE, DRAINE) / (ONE-FCCE)  
   LRAIN (ICOL) = L  
!
  200 END DO  
!
!
! Loop over All Elements
! ----------------------
!
DO 390 IEL = 1, NEL  
!
!        * Avoid multiple array references
   HRFE = HRF (IEL)  
!
!        * Calculate (& store) surface water depth
   DWAT1E = DIMJE(HRFE, ZGRUND (IEL) )  
   DWAT1 (IEL) = DWAT1E  
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
   DO 350 FACE = 1, 4  
!
!           * Not interested in link element side faces
      BSIDE = IEL.LE.NLF  
      IF (BSIDE) BSIDE = MOD (FACE, 2) .EQ.1.EQV.LINKNS (IEL)  
      IF (BSIDE) GOTO 350  
!                      ^^^^^^^^
!
!           * Discharge rate
      QOUT = FQOUT (IEL, FACE)  
!
!           * No-flow faces are special case
      IF (ISZERO(QOUT)) THEN  
!              * (consider weirs and branch nodes for example)
         SLOPEJ (IEL, FACE) = ZERO  
         TAUJ (IEL, FACE) = ZERO  
         GOTO 350  
!              ^^^^^^^^
      ENDIF  
!
!           * Find neighbouring element, & its face (also set FQCONF)
      KEL = FACE  
      IADJ = ICMREF (IEL, KEL, 2)  
      IF (IADJ.EQ.0) THEN  
!              * This is a boundary face; extrapolate from behind ...
         KEL = 1 + MOD (FACE+1, 4)  
         IADJ = ICMREF (IEL, KEL, 2)  
      ENDIF  
      IF (IADJ.EQ.0) THEN  
!              * ... unless that's a boundary too; then go for slope=0
         IADJ = IEL  
         KADJ = KEL  
      ELSEIF (IADJ.GT.0) THEN  
!              * Neighbour is a regular element
         KADJ = ICMREF (IEL, KEL, 3)  
      ELSE  
!
!              * Extra things to do if neighbour is a confluence node
!
!              * Branch index
         IBR = - IADJ  
!
!              * Initialize locals for prospect-loop:
!              - gross discharge from the node
         QSUM = ZERO  
!              - prospects with maximal inflow/outflow
         PIN = 0  
         POUT = 0  
!              - discharge from node (let this branch be prospect 0)
         QOUTX (0) = - FQOUT (IEL, KEL)  
!
!              * Loop over Prospects
         DO 300 P = 1, 3  
            IELP = ICMRF2 (IBR, P, 1)  
            IF (IELP.GT.0) THEN  
               KELP = ICMRF2 (IBR, P, 2)  
               Q = - FQOUT (IELP, KELP)  
               QSUM = QSUM + MAX (ZERO, Q)  
               IF (Q.LT.QOUTX (PIN) ) PIN = P  
               IF (Q.GT.QOUTX (POUT) ) POUT = P  
            ELSE  
               Q = ZERO  
            ENDIF  
            QOUTX (P) = Q  
  300          END DO  
!
!              * Redefine neighbour as link with maximal outflow ...
         PADJ = POUT  
!              * ... unless node is at inflow face for this element
         IF (QOUTX (0) .GT.ZERO) PADJ = PIN  
         IF (PADJ.GT.0) THEN  
            IADJ = ICMRF2 (IBR, PADJ, 1)  
            KADJ = ICMRF2 (IBR, PADJ, 2)  
         ELSE  
!                 * (no obvious candidate: go for slope=0)
            IADJ = IEL  
            KADJ = KEL  
         ENDIF  
!
!              * Calculate node outflow fractions if appropriate
         IF (QOUT.GT.ZERO.AND.KEL.EQ.FACE) THEN  
!                 * NB: Need precondition on QOC to ensure QSUM.GT.0
            DO 320 P = 1, 3  
               FQCONF (IBR, P) = MAX (ZERO, QOUTX (P) ) / QSUM  
  320             END DO  
         ENDIF  
!
      ENDIF  
!
!           * Calculate water surface slope
      HE = HRFE  
      HA = HRF (IADJ)  
      DE = DHF (IEL, KEL)  
      DA = DHF (IADJ, KADJ)  
      IF (IEL.LE.NLF.NEQV.IADJ.LE.NLF) THEN  
!              * this is a bank face; use bank-full elevation as cut-off
         LINK = MIN (IEL, IADJ)  
         ZBF = ZBFULL (LINK)  
         IF (HE.LE.ZBF) THEN  
            HE = ZBF  
            DE = ZERO  
         ENDIF  
         IF (HA.LE.ZBF) THEN  
            HA = ZBF  
            IF (DE.GT.ZERO) DA = ZERO  
         ENDIF  
      ENDIF  
      SLOPEE = ABS (HE-HA) / (DE+DA)  
      SLOPEJ (IEL, FACE) = SLOPEE  
!
!           * Calculate flow shear stress at the ground surface
      TAUJE = RHOWAT * GRAVTY * DWAT1E * SLOPEE  
      TAUJ (IEL, FACE) = TAUJE  
!
!           * Find maximum flow rate so far and TAUJ for that face
      QABS = ABS (QOUT)  
      IF (QABS.GT.QMAX) THEN  
         QMAX = QABS  
         TAUMAX = TAUJE  
      ENDIF  
!
!        * Next face
  350    END DO  
!
!        * Set representative shear stress equal to maximum over faces
   TAUK (IEL) = TAUMAX  
!
!     * Next element
  390 END DO  
!
END SUBROUTINE SYWAT




!SSSSSS SUBROUTINE BALSED  
SUBROUTINE BALSED  
end subroutine BALSED

END MODULE SYmod