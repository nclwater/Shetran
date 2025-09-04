MODULE sediment_flow_dynamics
   USE SGLOBAL
   USE sediment_common
   USE CONST_SY
   USE sediment_transport_capacity, ONLY : SYCRIT, SYDR
   USE mod_load_filedata, ONLY : ALINIT
   USE UTILSMOD

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: SYCOLM, SYLINK, SYOVTR, SYWAT

CONTAINS

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
100         END DO
         ENDIF
200   END DO
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
300   END DO
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
400      END DO
500   END DO
!
   END SUBROUTINE SYCOLM
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
100   END DO
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
200      END DO
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
300      END DO
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
100   END DO
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
200   END DO
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
            IF (.NOT. BSIDE) THEN
!
!              * Discharge rate
               QOUT = FQOUT (IEL, FACE)
!
!              * No-flow faces are special case
               IF (ISZERO(QOUT)) THEN
!                 * (consider weirs and branch nodes for example)
                  SLOPEJ (IEL, FACE) = ZERO
                  TAUJ (IEL, FACE) = ZERO
               ELSE
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
300                  END DO
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
320                     END DO
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
               ENDIF  ! End of ELSE block for non-zero flow
            ENDIF     ! End of IF (.NOT. BSIDE) block
!
!        * Next face
350      END DO
!
!        * Set representative shear stress equal to maximum over faces
         TAUK (IEL) = TAUMAX
!
!     * Next element
390   END DO
!
   END SUBROUTINE SYWAT

END MODULE sediment_flow_dynamics
