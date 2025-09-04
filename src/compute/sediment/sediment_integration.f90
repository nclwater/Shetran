MODULE sediment_integration
   USE SGLOBAL
   USE sediment_common
   USE sediment_initialization
   USE sediment_transport_capacity
   USE sediment_erosion
   USE sediment_flow_dynamics
   USE sediment_bed_processes
   USE mod_load_filedata, ONLY : ALINIT
   USE UTILSMOD

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: SYMAIN, BALSED

CONTAINS

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
100      END DO
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
140            END DO
150         END DO
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
210            END DO
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
220            END DO
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
225            END DO
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
226               END DO
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
240                     END DO
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
250                           END DO
                           ENDIF
255                     END DO
                     ENDIF
                  ENDIF
!
260            END DO
!
270         END DO
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
290      END DO
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
!SSSSSS SUBROUTINE BALSED
   SUBROUTINE BALSED
   end subroutine BALSED

END MODULE sediment_integration
