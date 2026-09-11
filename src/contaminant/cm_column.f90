!> summary: Column advection--dispersion--reaction: preparation, assembly and solve.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> The soil-column path of the transport calculation. [[COLMW]] prepares one
!> column's geometry, water and boundary state; [[COLMSM]] assembles the
!> dimensionless advection--dispersion--reaction balance, substituting the
!> nitrate source and sink terms where mineral nitrogen is active; [[COLM]] and
!> [[SLVCLM]] solve it. [[DISP]] and [[PHI]] supply the dispersivity and the
!> time-weighting factor.
!>
!> The module-level variables here are one column's working values, not state
!> per element — which is why [[cm_driver:CMSIM]] has to process elements
!> serially.
!>
!> | Module-scope work state | Producer | Consumer |
!> |:------------------------|:---------|:---------|
!> | Column geometry, topology, water and boundary work state | [[COLMW]] | [[COLMSM]] and [[COLM]] |
!>
!> @warning
!> [[PHI]] returns `0.5D0` and [[DISP]] returns `3.0D-8` unconditionally: the
!> manual fields `CM57`, `CM59` and `CM61` that should set them are read into
!> local arrays in [[cm_input:CMRD]] and discarded on return.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993--1998 | GP / RAH / SB | 3.4--4.2 | Developed and reorganised the contaminant transport routines. |
!> | 2008-12 | JE | 4.3.5F90 | Created `CMmod` while converting the former CM `COLM` and `LINK` Fortran sources to Fortran 90. |
!> | 2020-03-05 | SvB | - | Replaced the complete `SGLOBAL` include with selected imports. |
!> | 2026-09-10 | SvB | - | Split out of CMmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE cm_column

   USE MOD_PARAMETERS, ONLY: half, one, two, zero
   USE array_limits, ONLY: LLEE, nelee, NSEDEE
   USE element_geometry, ONLY: area => cellarea, DXQQ, DYQQ, ZGRUND
   USE grid_topology, ONLY: ICMREF
   USE channel_geometry, ONLY: CLENTH, CWIDTH, NHBED
   USE simulation_clock, ONLY: DTUZ
   USE et_state, ONLY: EEVAP, ERUZ, PNETTO
   USE vs_state, ONLY: DELTAZ, NLYR, NLYRBT, NTSOIL, NVSWLI, NVSWLT, NWELBT, NWELTP, QBKB, &
                       QVSH, QVSV, QVSWEL, QVSWLI, VSTHE, ZVSNOD
   USE oc_state, ONLY: hrf => HRFZZ
   USE oc_state, ONLY: QOC
   USE sy_state, ONLY: DLS, FBETA, FDEL, GNU, NSED
   USE cm_parameters, ONLY: ALPHA, CCAPB, CCAPE, CCAPI, CCAPIO, CCAPR, CCCC, CCCCO, CCCCW, &
                            FADS, GCPLA, GNN, IIICF, IIICFO, KDDLS, KDDSOL, NCON, SSSS, SSSSO
   USE cm_bank_geometry, ONLY: FNCEBD, NCEAB, NCEBD
   USE cm_column_geometry, ONLY: NCOLMB, NOLBT, NOLCEA, OODO, ZCOLMB
   USE cm_column_previous, ONLY: DSWO, GGAMMO, QIO, QQO, QQQSWO, QQRFO, RSZWLO, UUAJPO, &
                                 VSTHEO, ZONEO
   USE cm_column_scaling, ONLY: D0, NCETOP, OMSGMA, SGMA, SGSQ, Z2, Z2OD, Z2SQOD
   USE cm_sediment_previous, ONLY: DLSO, FDELO, GNUO
   USE cm_solver_flags, ONLY: ISADNL, ISBK, ISFLXB, ISMN, ISPLT
   USE cm_sorption, ONLY: RET
   USE cm_plant, ONLY: PLCOLM
   USE float_compare, ONLY: notzero
   USE linear_algebra, ONLY: TRIDAG

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: COLMSM, COLMW

   INTEGER :: JBK       !! Current bank side, 1 or 2, shared by the column preparation and solve paths.
   INTEGER :: JFLINK    !! Face of the adjacent link for the current bank column.
   INTEGER :: JSOL(LLEE) !! Soil-type number for each current local column cell.
   INTEGER :: NWORK(4)  !! Adjacent element number for each face of the current column.
   INTEGER :: NLINKA    !! Link adjacent to the current bank column, or zero for an ordinary land column.
   INTEGER :: NCWELL    !! Well/spring element associated with the current column, or zero.
   DOUBLEPRECISION :: VELDUM(LLEE) !! Bank-width correction applied to current local vertical-flow terms.
   DOUBLEPRECISION :: QQQWEL       !! Current well/spring water rate used by [[colmsm]].
   DOUBLEPRECISION :: QQQWL1       !! New well/spring water rate prepared by [[colmw]].
   DOUBLEPRECISION :: QQRV(LLEE)   !! Bank-to-link lateral water rates by local cell.
   DOUBLEPRECISION :: ROH(LLEE)    !! Fraction of a bank/link composite cell assigned to the bank column.
   LOGICAL :: ISBDY(4)             !! True where the corresponding current-column face is a catchment boundary.
   INTEGER :: count = 0            !! Unused legacy module counter; [[snl3]] owns its active saved warning counter.

CONTAINS

!> @brief Assembles and solves one contaminant balance for one soil column.
!>
!> Shared state prepared by [[colmw]] and [[colmsm]] defines cells
!> `NCEBOT:NCETOP`. For each cell, `COLM` assembles vertical advection and
!> dispersion, four-face lateral transport, dynamic/dead-space exchange,
!> dissolved and sorbed storage, decay/generation, plant and nitrate terms,
!> surface-water coupling, and the selected lower boundary condition.
!>
!> With mobile concentration (C_i), dead-space concentration (S_i), water
!> content \(\theta_i\), mobile fraction \(\phi_i\), dynamic sorption fraction
!> \(f_i\), distribution coefficient \(K_{d,i}\), and Freundlich power \(n\),
!> the old-state storage factors represented by the code are
!>
!> \[
!> F_C=\phi_i\theta_i+f_iK_{d,i}C_i^{n-1},\qquad
!> F_S=(1-\phi_i)\theta_i+(1-f_i)K_{d,i}S_i^{n-1}.
!> \]
!>
!> After mapping physical cells onto local indices, the two linearised rate
!> equations passed to [[slvclm]] are
!>
!> \[
!> FLT_i\Omega_{i-1}+ELT_i\Omega_i+DLT_i\Omega_{i+1}
!>       -GLT_i\epsilon_i=SLT_i,
!> \]
!> \[
!> PLT_i\epsilon_i-TLT_i\Omega_i=QLT_i.
!> \]
!>
!> The solved rates advance `CCAP` and `SCAP` by `TSE`; `WORKA` and `WORKB`
!> then correct the generation/storage terms for the solved nonlinear rates.
!> A flux lower boundary uses the convected `CCPRF`; the alternative replaces
!> the bottom mobile equation with the prescribed `CCAPB` concentration.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought the former `AL.P` implicit declarations into the routine. |
!> | 1995-05-09 | RAH | 4.0 | Incorporated `KSP` in `OCAPP` and `OCAPP1`. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing and generic intrinsics. |
!> @endhistory
   SUBROUTINE COLM

      ! Commons and constants
      USE cm_column_scaling
      USE cm_column_water
      USE cm_column_state
      USE cm_column_equations

      IMPLICIT NONE

      ! Locals, etc
      INTEGER :: NC, J, NCADJ, NDUM
      DOUBLE PRECISION :: TTHT, TTHT1, PPHITH, PPHIT1, PPHTHP, PPHTP1
      DOUBLE PRECISION :: KKD, MCAP, MCAPC, MCAPT, WORKA(LLEE), WORKB(LLEE)
      DOUBLE PRECISION :: FFKD, GGNMON, AALPH
      DOUBLE PRECISION :: SUM1, SUM2, SUM3, CBCAPC, OMCBCC, CBCAP, CBCAPT
      DOUBLE PRECISION :: ANCAP, ANCAPT, ANCAPS, BCAP, BCAP1, BCAPSG
      DOUBLE PRECISION :: FCAP, FCAPT, FCAPC
      DOUBLE PRECISION :: GCAP, GCAPT, GCAPS, GMCAP, GMCAP1, GMCPSG
      DOUBLE PRECISION :: OCAPM, OCAPP, OCAPM1, OCAPP1
      DOUBLE PRECISION :: PCAPM, PCAPP, PCAPM1, PCAPP1
      DOUBLE PRECISION :: BPGSG, BMGSG, DUMMY
      DOUBLE PRECISION :: ALT, ALT1, HLT, HLT1, BLT, BLT1, ALTSG, HLTSG, BLTSG
      DOUBLE PRECISION :: VCAP, VCAP1
      DOUBLE PRECISION :: CBSWC, OMCBSC, CBSW, CBSWT, RRRB, RRRBT, RRRBC
      DOUBLE PRECISION :: CCPRFC, OMCRFC, CBRF, CBRFT

      !----------------------------------------------------------------------*

      OCAPP = zero
      OCAPP1 = zero
      PCAPP = zero
      PCAPP1 = zero

      ! MAIN LOOP - SETS ELEMENTS, FOR ALL CELLS, FOR VECTORS FOR DIFFERENCE EQUATIONS
      main_loop: DO NC = NCEBOT, NCETOP

         TTHT = TTHET(NC)
         TTHT1 = TTHET1(NC)
         PPHITH = PPHI(NC)*TTHT
         PPHIT1 = PPHI1(NC)*TTHT1
         PPHTHP = PPHI(NC + 1)*TTHET(NC + 1)
         PPHTP1 = PPHI1(NC + 1)*TTHET1(NC + 1)
         KKD = KKDSO(NC)
         FFKD = FFSO(NC)*KKD
         GGNMON = GGNNSO(NC) - one
         AALPH = AALPSO(NC)

         ! SET DEPTH AND SOIL DEPENDENT VARIABLES
         SUM1 = zero
         SUM2 = zero
         SUM3 = zero

         IF (NC <= (NCEPSF + 1)) THEN
            face_loop: DO J = 1, 4
               CBCAPC = half - SIGN(half, QQ(NC, J))
               OMCBCC = one - CBCAPC
               CBCAP = OMCBCC*CCAPA(NC, J) + CBCAPC*COLCAP(NC)
               CBCAPT = OMCBCC*CCAPAT(NC, J)
               SUM1 = SUM1 + QQ(NC, J)*CBCAP
               SUM2 = SUM2 + (QQ1(NC, J)/ZONE1 - QQ(NC, J)/ZONE)*CBCAP + &
                      TSE*QQ(NC, J)*CBCAPT/ZONE
               SUM3 = SUM3 + QQ(NC, J)*CBCAPC
            END DO face_loop
         END IF

         ! SUM CONVECTION TERMS OVER FOUR FACES
         MCAP = GNERD(NC) - EDCAP(NC) + CST1*SUM1
         MCAPT = GND2(NC) - EDCAPT(NC) + CST1*SUM2*ZONE/TSE
         MCAPC = -EDCAPC(NC) + CST1*SUM3

         ANCAP = GNDSE(NC) - ESCAP(NC)
         ANCAPT = GNDSE2(NC) - ESCAPT(NC)
         ANCAPS = -ESCAPS(NC)

         BCAP = Z2SQOD*(AALPH + half*ABS(GGAMM(NC)))
         BCAP1 = Z2SQOD*(AALPH + half*ABS(GGAMM1(NC)))
         BCAPSG = OMSGMA*BCAP + SGMA*BCAP1

         FCAP = PPHITH + FFKD*COLCAP(NC)**GGNMON
         FCAPT = (PPHIT1 - PPHITH)/TSE
         FCAPC = GGNMON*(FCAP - PPHITH)/COLCAP(NC)

         GND2(NC) = GCAPLA*FCAPT*COLCAP(NC)
         GNERD(NC) = GCAPLA*COLCAP(NC)*FCAP + SGTSE*GND2(NC)
         WORKA(NC) = GCAPLA*FCAP*SGTSE

         ! SET GENERATION TERMS FOR DYNAMIC REGION
         ! A FURTHER TERM WILL BE ADDED TO GENRD LATER
         GCAP = TTHT - PPHITH + (KKD - FFKD)*SOLCAP(NC)**GGNMON
         GCAPT = (TTHT1 - PPHIT1 - TTHT + PPHITH)/TSE
         GCAPS = GGNMON*(GCAP - TTHT + PPHITH)/SOLCAP(NC)

         GNDSE2(NC) = GCAPLA*GCAPT*SOLCAP(NC)
         GNDSE(NC) = GCAPLA*SOLCAP(NC)*GCAP + SGTSE*GNDSE2(NC)
         WORKB(NC) = GCAPLA*GCAP*SGTSE

         ! SET GENERATION TERMS FOR DEAD SPACE
         ! A FURTHER TERM WILL BE ADDED TO GNDSE LATER
         GMCAP = Z2SQOD*GGAMM(NC)/two
         GMCAP1 = Z2SQOD*GGAMM1(NC)/two
         GMCPSG = OMSGMA*GMCAP + SGMA*GMCAP1

         OCAPM = OCAPP
         ! WEIGHTED HARMONIC MEAN
         OCAPP = two*PPHITH*DDOD(NC)*PPHTHP*DDOD(NC + 1)* &
                 KSP(NC)*KSP(NC + 1)/(PPHITH*DDOD(NC)*KSP(NC + 1) + &
                                      PPHTHP*DDOD(NC + 1)*KSP(NC))

         OCAPM1 = OCAPP1
         OCAPP1 = two*PPHIT1*DDOD1(NC)*PPHTP1*DDOD1(NC + 1)* &
                  KSP(NC)*KSP(NC + 1)/(PPHIT1*DDOD1(NC)*KSP(NC + 1) + &
                                       PPHTP1*DDOD1(NC + 1)*KSP(NC))

         PCAPM = PCAPP
         PCAPP = Z2OD*UUAJP(NC)
         PCAPM1 = PCAPP1
         PCAPP1 = Z2OD*UUAJP1(NC)

         ! SET VALUES FOR NON-DIMENSIONED VARIABLES
         BPGSG = BCAPSG + GMCPSG
         BMGSG = BCAPSG - GMCPSG
         DUMMY = one/KSP(NC)

         ALT = DUMMY*MAX(zero, OCAPP/KSPP(NC) - half*PCAPP, -PCAPP)
         ALT1 = DUMMY*MAX(zero, OCAPP1/KSPP(NC) - half*PCAPP1, -PCAPP1)
         HLT = DUMMY*MAX(zero, OCAPM/KSPP(NC - 1) + half*PCAPM, PCAPM)
         HLT1 = DUMMY*MAX(zero, OCAPM1/KSPP(NC - 1) + half*PCAPM1, PCAPM1)

         BLT = -ALT - HLT - DUMMY*(PCAPP - PCAPM)
         BLT1 = -ALT1 - HLT1 - DUMMY*(PCAPP1 - PCAPM1)
         ALTSG = OMSGMA*ALT + SGMA*ALT1
         HLTSG = OMSGMA*HLT + SGMA*HLT1
         BLTSG = OMSGMA*BLT + SGMA*BLT1

         ! SET WORKING VALUES, AND COEFFICIENTS (A, B, AND H) FOR COMBINED
         ! CONVECTION AND DISPERSION TERM

         NCADJ = NC - NCEBOT + 1

         ! ADJUST CELL NUMBERS SO THE COEFFICIENTS BELOW ARE SET FOR NCADJ=1,2,3 ETC
         DLT(NCADJ) = -SGTSE*ALTSG
         ELT(NCADJ) = SGTSE*(-BLTSG + BPGSG) + OPSGL*(FCAP + FCAPC*COLCAP(NC)) + &
                      OPSGSL*TSE*FCAPT - SGTSE*MCAPC
         ELTSTR(NCADJ) = OPSGSL*TSE*FCAPC
         FLT(NCADJ) = -SGTSE*HLTSG
         GLT(NCADJ) = SGTSE*BMGSG
         PLT(NCADJ) = SGTSE*BMGSG + OPSGL*(GCAP + GCAPS*SOLCAP(NC)) + &
                      OPSGSL*TSE*GCAPT - SGTSE*ANCAPS
         PLTSTR(NCADJ) = OPSGSL*TSE*GCAPS
         QLT(NCADJ) = -(GCAPLA*GCAP + BMGSG + OPSGL*GCAPT)*SOLCAP(NC) + &
                      BPGSG*COLCAP(NC) + ANCAP + SGTSE*ANCAPT
         SLT(NCADJ) = ALTSG*COLCAP(NC + 1) + (BLTSG - BPGSG - GCAPLA*FCAP - &
                                              OPSGL*FCAPT)*COLCAP(NC) + HLTSG*COLCAP(NC - 1) + &
                      BMGSG*SOLCAP(NC) + MCAP + SGTSE*MCAPT
         TLT(NCADJ) = SGTSE*BPGSG

         ! SET ELEMENTS, FOR INTERNAL CELLS, OF THE VECTORS FOR THE DIFFERENCE EQUATIONS

      END DO main_loop
      ! END OF MAIN LOOP

      NC = NCETOP
      VCAP = GGGNU*Z2OD
      VCAP1 = GGGNU1*Z2OD
      SUM1 = zero
      SUM2 = zero
      SUM3 = zero

      top_face_loop: DO J = 1, 4
         CBSWC = half - SIGN(half, QQQSW(J))
         OMCBSC = one - CBSWC
         CBSW = OMCBSC*CSWA(J) + CBSWC*COLCAP(NCETOP)
         CBSWT = OMCBSC*CSWAT(J)
         RRRB = OMCBSC*RRRSWA(J) + CBSWC*RRRSW
         RRRBT = OMCBSC*RRRSAT(J) + CBSWC*RRRSWT
         RRRBC = CBSWC*RRRSWC

         SUM1 = SUM1 + QQQSW(J)*RRRB*CBSW
         SUM2 = SUM2 + (QQQSW1(J) - QQQSW(J))*RRRB*CBSW + QQQSW(J)* &
                TSE*(RRRB*CBSWT + RRRBT*CBSW)
         SUM3 = SUM3 + QQQSW(J)*(RRRB*CBSWC + RRRBC*CBSW)
      END DO top_face_loop

      ! SUM CONVECTION TERMS OVER FOUR FACES
      MCAP = MCAP + (VCAP*(FCAP*COLCAP(NC) + GCAP*SOLCAP(NC)) - &
                     ESSCAP - ICAP - QCAP + CST2*SUM1)/KSP(NC)

      ! THE GENERATION TERM FOR SOIL, SURFACE WATER, AND SEDIMENTS IS INCLUDED
      ! IN MCAP AS SET IN THE MAIN LOOP
      MCAPT = MCAPT + ((VCAP1 - VCAP)*(FCAP*COLCAP(NC) + GCAP*SOLCAP(NC))/TSE + &
                       VCAP*(FCAPT*COLCAP(NC) + GCAPT*SOLCAP(NC)) - ESSCPT - ICAPT - &
                       QCAPT + CST2*SUM2/TSE)/KSP(NC)

      MCAPC = MCAPC + (VCAP*(FCAPC*COLCAP(NC) + FCAP) - ESSCPC - ICAPC - &
                       QCAPC + CST2*SUM3)/KSP(NC)

      ! THE FOLLOWING CODE MUST COME AFTER MCAP IS OVERWRITTEN
      FCAP = FCAP + (DDDSW*RRRSW + DDDLS*TTTLSE*RRRLS)/(Z2*KSP(NC))
      FCAPT = FCAPT + (RRRSW*(DDDSW1 - DDDSW) + TTTLSE*RRRLS*(DDDLS1 - DDDLS) + &
                       TSE*(DDDSW*RRRSWT + DDDLS*TTTLSE*RRRLST))/(TSE*KSP(NC)*Z2)
      FCAPC = FCAPC + (DDDSW*RRRSWC + DDDLS*TTTLSE*RRRLSC)/(KSP(NC)*Z2)

      ! ADD EFFECT OF SURFACE WATER AND SED. TO F
      GND2(NC) = GCAPLA*FCAPT*COLCAP(NC)
      GNERD(NC) = GCAPLA*COLCAP(NC)*FCAP + SGTSE*GND2(NC)
      WORKA(NC) = GCAPLA*FCAP*SGTSE

      BLT = -HLT + DUMMY*PCAPM
      BLT1 = -HLT1 + DUMMY*PCAPM1
      BLTSG = OMSGMA*BLT + SGMA*BLT1
      NCADJ = NC - NCEBOT + 1

      DLT(NCADJ) = zero
      ELT(NCADJ) = SGTSE*(-BLTSG + BPGSG) + OPSGL*(FCAP + FCAPC*COLCAP(NC)) + &
                   OPSGSL*TSE*FCAPT - SGTSE*MCAPC
      ELTSTR(NCADJ) = OPSGSL*TSE*FCAPC
      SLT(NCADJ) = (BLTSG - BPGSG - GCAPLA*FCAP - OPSGL*FCAPT)*COLCAP(NC) + &
                   HLTSG*COLCAP(NC - 1) + BMGSG*SOLCAP(NC) + MCAP + SGTSE*MCAPT

      ! OVERWRITE VECTOR ELEMENTS FOR THE TOP CELL
      NC = NCEBOT
      IF (ISFLXB) THEN
         CCPRFC = half - SIGN(half, QQRF)
         OMCRFC = one - CCPRFC
         CBRF = OMCRFC*CCPRF + CCPRFC*COLCAP(NC)
         CBRFT = OMCRFC*CCPRFT
         ELT(1) = ELT(1) - CST3*SGTSE*QQRF*CCPRFC
         SLT(1) = SLT(1) + CST3*QQRF*CBRF
         SLT(1) = SLT(1) + CST3*SGTSE*((QQRF1 - QQRF)*CBRF + TSE*QQRF*CBRFT)/TSE
      ELSE
         DLT(1) = zero
         ELT(1) = one
         ELTSTR(1) = zero
         FLT(1) = zero
         GLT(1) = zero
         SLT(1) = (CCAP(NCEBOT) - COLCAP(NCEBOT))/TSE
      END IF

      ! OVERWRITE VECTOR ELEMENTS FOR THE BOTTOM CELL
      NDUM = NCETOP - NCEBOT + 1

      ! SOLVE THE DIFFERENCE EQUATIONS FOR THE EPSILON AND OMEGA VECTORS
      CALL SLVCLM(NDUM)

      update_loop: DO NC = NCEBOT, NCETOP
         NCADJ = NC - NCEBOT + 1
         CCAP(NC) = COLCAP(NC) + OME(NCADJ)*TSE
         SCAP(NC) = SOLCAP(NC) + EPS(NCADJ)*TSE
         GNERD(NC) = GNERD(NC) + WORKA(NC)*OME(NCADJ)
         GNDSE(NC) = GNDSE(NC) + WORKB(NC)*EPS(NCADJ)
      END DO update_loop
      ! SET ELEMENTS OF CONCENTRATION VECTORS AND GENERATION VECTORS

   END SUBROUTINE COLM

!> @brief Prepares and updates every contaminant in one land or bank column.
!>
!> For column `NCL`, the routine copies the preceding mobile/dead-space state
!> from `CCCCO`/`SSSSO`, prepares soil properties and the effective dispersion
!> \(DDOD=DISP/D0\), and constructs face concentrations. Internal faces use a
!> water-flow-weighted adjacent concentration; catchment boundaries use
!> `CCAPE`; exposed bank faces use the bank column below the bed and link water
!> above it. [[ret]] supplies loose-sediment and surface-water retardation.
!>
!> Rainfall, wells, lower-boundary flow, bank exchange, dry deposition,
!> sediment and parent-contaminant generation are combined with optional plant
!> uptake before [[colm]] is called. If `ISMN` is true, the nitrate process
!> arrays `SSS1` and `SSS2` replace the ordinary plant source terms. For
!> contaminant 1 the direct surface inputs are then suppressed because nitrate
!> inputs are already represented by the MN component.
!>
!> The result is stored with the legacy positive floor
!>
!> \[
!> CCCC=\max(10^{-16},CCAP),\qquad SSSS=\max(10^{-16},SCAP).
!> \]
!>
!> Bank columns additionally populate `FCPBKO` and `GCPBKO`, which [[link]]
!> consumes later in the ordered element sweep. Linear adsorption uses
!> \(\phi\theta+fK_d\) and \((1-\phi)\theta+(1-f)K_d\); the nonlinear branch
!> multiplies each sorption term by the corresponding old concentration raised
!> to `GNN-1`.
!>
!> @warning The module flags `ISFLXB`, `ISADNL`, and `ISPLT` used here are not
!> assigned by current [[cmrd]]. The locally initialised `CDUM` also has implicit
!> `SAVE`; only bank calls recalculate it before element-1 concentration storage.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-09-30 | GP | 3.4 | Initialised the plant uptake arrays for the inactive-plant case. |
!> | 1994-10-03 | RAH | 3.4.1 | Brought the former `AL.P` implicit declarations into the routine. |
!> | 1996-07-17 | GP | 4.0 | Revised lateral averaging and incorporated well flow in surface inputs. |
!> | 1997-03-14 | RAH | 4.1 | Added explicit typing and split the former mixed-type work common block. |
!> | 1997-05-21 | RAH | 4.1 | Removed redundant shared workspace. |
!> | 2025-09-23 | SB | 4.5.3 | Changed the source terms when the nitrate component is in use. |
!> @endhistory
   SUBROUTINE COLMSM(NCL)

      ! Commons and constants
      USE sy_state, ONLY: FBETA, FDEL, NSED
      USE cm_parameters
      USE cm_column_scaling
      USE cm_column_water
      USE cm_column_state
      USE cm_column_geometry
      USE cm_bank_geometry
      USE cm_sediment_previous

      ! Input common
      ! INTEGER :: JBK, JFLINK, JSOL (LLEE), NWORK (4), NLINKA, NCWELL
      ! DOUBLE PRECISION VELDUM (LLEE), QQQWEL, QQQWL1, QQRV (LLEE), &
      !  ROH (LLEE)
      ! LOGICAL :: ISBDY (4)
      ! COMMON / WTOCI / JBK, JFLINK, JSOL, NWORK, NLINKA, NCWELL
      ! COMMON / WTOC / VELDUM, QQQWEL, QQQWL1, QQRV, ROH
      ! COMMON / WTOCL / ISBDY
      ! VARIABLES USED ONLY IN COLMW AND COLMSM

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NCL !! Active land or bank element number.

      ! Locals, etc
      INTEGER :: NCONT, NCE, JA, NDUM, NOLDUM, NOLP, JCEA, JSED
      DOUBLE PRECISION :: CCBT, SUM, SUMQ, SUMQC, SUMW
      DOUBLE PRECISION :: DUM, DUM0, DUM1, DUM2, DUM3, DUMBED
      DOUBLE PRECISION :: CDUM = 0.0D0 !! Saved bank-to-stream concentration workspace due to declaration initialization.
      DOUBLE PRECISION :: GNDUM, QDUM, QCDUM, QCDUM1, UDUMP, UDUMM, UCDUMP, UCDUMM
      DOUBLE PRECISION :: FBO(NSEDEE), FB(NSEDEE), FDLO(NSEDEE), FDL(NSEDEE), KDDUM(NSEDEE)
      DOUBLEPRECISION, DIMENSION(NELEE), SAVE :: DUMMY !! Floating-point input workspace; scratch within this routine only. `SAVE` keeps it in static storage, as the former module variable was.

      !----------------------------------------------------------------------*

      ! SET GENERATION VARIABLES TO ZERO IN PREPARATION FOR THE 1ST PASS OF THE CONTAMINANT LOOP
      init_loop: DO NCE = 1, LLEE
         GNERD(NCE) = zero
         GNDSE(NCE) = zero
         GND2(NCE) = zero
         GNDSE2(NCE) = zero
      END DO init_loop

      ! +++++ MAIN LOOP FOR UPDATING CONCS ++++++
      cont_loop: DO NCONT = 1, NCON

         ! SET OLD CONCENTRATION VECTORS
         old_conc_loop: DO NCE = NCEBOT - 1, NCETOP + 1
            COLCAP(NCE) = CCCCO(NCL, NCE, NCONT)
            SOLCAP(NCE) = SSSSO(NCL, NCE, NCONT)
         END DO old_conc_loop

         GCAPLA = GCPLA(NCONT)

         ! SET THE EFFECTIVE DISPERSION COEFFICIENTS AND OTHER SOIL PROPERTIES
         disp_loop: DO NCE = NCEBOT, NCETOP
            DDOD(NCE) = OODO*DISP(NCONT, JSOL(NCE), TTHET(NCE), UUAJP(NCE - 1), UUAJP(NCE))
            DDOD1(NCE) = OODO*DISP(NCONT, JSOL(NCE), TTHET1(NCE), UUAJP1(NCE - 1), UUAJP1(NCE))
            AALPSO(NCE) = ALPHA(JSOL(NCE), NCONT)
            FFSO(NCE) = FADS(JSOL(NCE), NCONT)
            GGNNSO(NCE) = GNN(NCONT)
            KKDSO(NCE) = KDDSOL(JSOL(NCE), NCONT)
         END DO disp_loop

         DDOD(NCETOP + 1) = zero
         DDOD1(NCETOP + 1) = zero

         face_loop: DO JA = 1, 4

            IF (.NOT. ISBDY(JA) .AND. (JA /= JFLINK)) THEN
               ! IS NOT FACE AT CATCHMENT BOUNDARY OR THE EXPOSED FACE OF A BANK
               NDUM = NCEPSF + 1

               ! EXPLICIT (IN C) LATERAL COUPLING IN SUBSURFACE
               subsurf_loop: DO NCE = NCEBOT, MIN(NDUM, NCETOP)
                  SUMQ = zero
                  SUMQC = zero
                  NOLDUM = MAX(1, NOLBT(NCL, NCE, JA))

                  layer_loop: DO NOLP = NOLDUM, NOLBT(NCL, NCE + 1, JA) - 1
                     JCEA = NOLCEA(NCL, NOLP, JA)
                     QDUM = QQ1(NCE, JA)
                     SUMQ = SUMQ + QDUM
                     SUMQC = SUMQC + QDUM*CCCCO(NWORK(JA), JCEA, NCONT)
                  END DO layer_loop

                  IF (NOTZERO(SUMQ)) SUMQ = SUMQC/SUMQ
                  CCAPA(NCE, JA) = SUMQ
                  CCAPAT(NCE, JA) = zero
               END DO subsurf_loop

               CSWA(JA) = CCCCO(NWORK(JA), NCETOP, NCONT)
               CSWAT(JA) = (CCCC(NWORK(JA), NCETOP, NCONT) - CSWA(JA))/TSE
               RRRSWA(JA) = RSW(NWORK(JA), NCONT)
               RRRSAT(JA) = RSWT(NWORK(JA), NCONT) + RSWC(NWORK(JA), NCONT)*CSWAT(JA)

               ! IMPLICIT (IN C) LATERAL COUPLING IN SURF.
               ! NB: TIME DERIVATIVE OF R IN ADJACENT COLUMN INCLUDES THE EFFECT OF
               ! THE CHANGING CONC. IN THAT COLUMN

            ELSE IF (ISBDY(JA)) THEN
               ! IF ADJACENT COLUMN IS OUTSIDE BOUNDARY
               bdy_loop: DO NCE = NCEBOT, NCEPSF + 1
                  CCAPA(NCE, JA) = CCAPE(NCL, NCONT)
                  CCAPAT(NCE, JA) = zero
               END DO bdy_loop

               CSWA(JA) = CCAPE(NCL, NCONT)
               CSWAT(JA) = zero
               RRRSWA(JA) = one
               RRRSAT(JA) = zero
               ! NB: NO SEDIMENT FLOWS OVER BOUNDARY

            ELSE
               ! IS THE EXPOSED FACE OF A BANK COLUMN
               bank_loop: DO NCE = NCEBOT, NHBED(NLINKA, JBK)
                  CCAPA(NCE, JA) = CCCCO(NWORK(JA), NCE, NCONT)
                  CCAPAT(NCE, JA) = zero
               END DO bank_loop

               ! EXPLICIT (IN C) LATERAL COUPLING IN SUBSURFACE
               imp_bank_loop: DO NCE = NHBED(NLINKA, JBK) + 1, NCETOP
                  CCAPA(NCE, JA) = CCCCO(NLINKA, NCETOP, NCONT)
                  CCAPAT(NCE, JA) = (CCCC(NLINKA, NCETOP, NCONT) - CCAPA(NCE, JA))/TSE
               END DO imp_bank_loop
               ! IMPLICIT COUPLING WITH STREAM WATER FOR SUBSURFACE EXPOSED BANK CELLS

               CSWA(JA) = CCAPA(NCETOP, JA)
               CSWAT(JA) = CCAPAT(NCETOP, JA)
               RRRSWA(JA) = FSF(NLINKA, NCONT)
               RRRSAT(JA) = FSFT(NLINKA, NCONT) + FSFC(NLINKA, NCONT)*CSWAT(JA)
               ! NB: TIME DERIVATIVE OF F IN ADJACENT LINK INCLUDES THE EFFECT OF THE CHANGING CONC.
            END IF

         END DO face_loop

         ! SET CONCENTRATIONS AND RETARDATION FACTORS IN ADJACENT COLUMN
         IF (.NOT. ISFLXB) THEN
            CCBT = CCAPB(NCL, NCONT)
            CCAP(NCEBOT) = CCBT
            ! NB: CCAP(NCEBOT) IS USED AS THE BOUNDARY CONCENTRATION IN SUBROUTINE COLM
            CCPRF = zero
            CCPRFT = zero
         ELSE
            CCPRF = CCAPR(NCL, NCONT)
            CCPRFT = zero
            CCBT = CCPRF
         END IF

         ! SET BOTTOM CELL VARIABLES
         bot_cell_loop: DO NCE = 1, NCEBOT - 1
            COLCAP(NCE) = CCBT
            CCAP(NCE) = CCBT
            SCAP(NCE) = CCBT
         END DO bot_cell_loop

         ! SET UP ARRAYS FOR USE IN CALLS TO FUNCTION RET
         ret_setup_loop: DO JSED = 1, NSED
            KDDUM(JSED) = KDDLS(JSED, NCONT)
            FBO(JSED) = FBETAO(NCL, JSED)
            FB(JSED) = FBETA(NCL, JSED)
            FBETAO(NCL, JSED) = FB(JSED)
            FDLO(JSED) = FDELO(NCL, JSED)
            FDL(JSED) = FDEL(NCL, JSED)
            FDELO(NCL, JSED) = FDL(JSED)
         END DO ret_setup_loop

         ! SET LOOSE SEDIMENT RETARDATION VARIABLES
         CALL RET(COLCAP(NCETOP), GNN(NCONT), TTTLSE, TTTLSE, FBO, FB, &
                  KDDUM, RRRLS, RRRLSC, RRRLST, TSE, NSED, ISADNL)

         ! SET SURFACE WATER RETARDATION VARIABLES
         CALL RET(COLCAP(NCETOP), GNN(NCONT), one, one, FDLO, FDL, &
                  KDDUM, RRRSW, RRRSWC, RRRSWT, TSE, NSED, ISADNL)

         RSW(NCL, NCONT) = RRRSW
         RSWC(NCL, NCONT) = RRRSWC
         RSWT(NCL, NCONT) = RRRSWT
         ! SAVE SURFACE WATER RETARDATION VALUES FOR USE IN CALCULATING LATERAL CONVECTION RATES

         ! SET SURFACE INPUT VARIABLES
         ICAP = -Z2OD*IIICFO(NCONT)
         IIICFO(NCONT) = IIICF(NCONT)
         ICAPT = zero
         ICAPC = zero
         DUM = Z2OD/(DDA*DDB)

         QCDUM = (QI - QQQWEL)*CCAPIO(NCONT)
         QCDUM1 = (QI1 - QQQWL1)*CCAPI(NCONT)

         IF (NCWELL > 0) THEN
            QCDUM = QCDUM + QQQWEL*CCCCW(NCWELL, NCONT)
            QCDUM1 = QCDUM1 + QQQWL1*CCCCW(NCWELL, NCONT)
         END IF

         QCAP = DUM*QCDUM
         QCAPT = (DUM*QCDUM1 - QCAP)/TSE
         CCAPIO(NCONT) = CCAPI(NCONT)
         QCAPC = zero

         dummy_loop: DO NCE = NCEBOT, NCETOP
            DUMMY(NCE) = zero
         END DO dummy_loop

         IF (ISBK) THEN
            SUM = zero
            SUMQ = zero
            DUM0 = Z2OD/AREA(NCL)

            ! SET SOURCE FOR CONVECTION INTO STREAM FROM BANK
            bank_src_loop: DO NCE = NCEAB(NLINKA, JBK), NHBED(NLINKA, JBK) + 1
               SUMQ = SUMQ + QQRV(NCE)
               DUM1 = ABS(QQRV(NCE))
               DUM2 = half*(QQRV(NCE) + DUM1)
               DUM3 = half*(QQRV(NCE) - DUM1)
               QCDUM = DUM2*CCCC(NLINKA, NCETOP - 2, NCONT) + DUM3*CCCCO(NCL, NCE, NCONT)
               ! IMPLICIT COUPLING TO DEEP BED CONC.
               SUM = SUM + QCDUM
               DUMMY(NCE) = DUMMY(NCE) + ROH(NCE)*QCDUM*DUM0/KSP(NCE)
            END DO bank_src_loop

            IF (NOTZERO(SUMQ)) SUMQ = SUM/SUMQ
            CDUM = SUMQ
            ! SET EFFECTIVE CONCENTRATION IN WATER FLOW INTO STREAM FROM BANK

            NCE = NCEBD(NLINKA, JBK) + 1
            UDUMP = UUAJP1(NCE)
            UDUMM = UUAJP1(NCE - 1)
            UCDUMP = MAX(zero, UDUMP*COLCAP(NCE)) - MAX(zero, -UDUMP*COLCAP(NCE + 1))
            UCDUMM = MAX(zero, UDUMM*COLCAP(NCE - 1)) - MAX(zero, -UDUMM*COLCAP(NCE))
            DUMBED = (ROH(NCE)*VELDUM(NCE - 1) - one)*UCDUMM - (ROH(NCE)*VELDUM(NCE) - one)*UCDUMP
            DUMBED = Z2OD*DUMBED/KSP(NCE)

         ELSE
            DUMBED = zero
         END IF

         IF (ISPLT) THEN
            CALL PLCOLM(NCL, NCONT)
         ELSE
            zero_edcap_loop: DO NCE = NCEBOT, NCETOP
               EDCAP(NCE) = zero
               EDCAPC(NCE) = zero
               EDCAPT(NCE) = zero
               ESCAP(NCE) = zero
               ESCAPS(NCE) = zero
               ESCAPT(NCE) = zero
            END DO zero_edcap_loop
         END IF

         ! SB 230925 change source terms if nitrate component being used
         IF (ISMN) THEN
            mn_loop: DO NCE = NCEBOT, NCETOP
               EDCAP(NCE) = SSS1(NCL, NCE, NCONT)
               ESCAP(NCE) = SSS2(NCL, NCE, NCONT)
               EDCAPT(NCE) = zero
               EDCAPC(NCE) = zero
               ESCAPT(NCE) = zero
               ESCAPS(NCE) = zero
            END DO mn_loop

            ! The first contaminant is nitrate and surface additions are considered in the MN component
            IF (NCONT == 1) THEN
               ICAP = zero
               QCAP = zero
               QCAPT = zero
            END IF
         END IF

         ! Call contaminant plant uptake routine Sets EDCAP, ESCAP etc
         SUM = zero
         SUMW = zero

         uptake_loop: DO NCE = NCEBOT, NCETOP
            EDCAP(NCE) = EDCAP(NCE) - DUMMY(NCE) + WELDRA(NCE)*Z2OD*COLCAP(NCE)/KSP(NCE)
            ! Add stream and well uptake to plant uptake
            SUM = SUM + WELDRA(NCE)*COLCAP(NCE)
            SUMW = SUMW + WELDRA(NCE)
         END DO uptake_loop

         IF (ISBK) THEN
            NCE = NCEBD(NLINKA, JBK) + 1
            EDCAP(NCE) = EDCAP(NCE) - DUMBED
         END IF

         ! Add uptake to dry streams to plant and well uptake
         IF (NOTZERO(SUMW)) THEN
            CCCCW(NCL, NCONT) = SUM/SUMW
         ELSE
            CCCCW(NCL, NCONT) = zero
         END IF

         ! SET PLANT WELL AND STREAM UPTAKE VARIABLES; AND SET THE MIXED WELL WATER
         ! CONCENTRATION FOR USE IN PRINTOUTS. NB: WELL UPTAKE AND LOSS TO STREAM
         ! VIA BED INCLUDED IN EDCAP

         OPSGL = one + SGTSE*GCAPLA
         OPSGSL = one + SGSTSE*GCAPLA
         ! SET FACTORS AND TERMS DEPENDING ON SIGMA

         CALL COLM
         ! RETURNS UPDATED CONCENTRATIONS IN THE VECTORS CCAP AND SCAP

         ! FLOW RATE AVERAGED CONC. IN WATER FLOW FROM BANK TO STREAM STORED AS ELEMENT 1
         ! IN GLOBAL CONTAMINANT ARRAYS
         CCCCO(NCL, 1, NCONT) = CDUM
         CCCC(NCL, 1, NCONT) = CDUM

         ! SAVE THE UPDATED CONCENTRATIONS
         save_conc_loop: DO NCE = 1, NCETOP
            ! ##########################temporary MAX######################
            CCCC(NCL, NCE, NCONT) = MAX(1.0D-16, CCAP(NCE))
            SSSS(NCL, NCE, NCONT) = MAX(1.0D-16, SCAP(NCE))
            ! ##############################################################
         END DO save_conc_loop

         ! FCPBK AND GCPBK ARE USED IN THE BANK EROSION CALCULATIONS IN LINK
         IF (ISBK .AND. (.NOT. ISADNL)) THEN
            fcpbk_loop1: DO NCE = NHBED(NLINKA, JBK) + 1, NCETOP
               FCPBKO(NLINKA, JBK, NCE, NCONT) = PPHI(NCE)*TTHET(NCE) + FFSO(NCE)*KKDSO(NCE)
               GCPBKO(NLINKA, JBK, NCE, NCONT) = (one - PPHI(NCE))*TTHET(NCE) + (one - FFSO(NCE))*KKDSO(NCE)
            END DO fcpbk_loop1
         ELSE IF (ISBK .AND. ISADNL) THEN
            GNDUM = GNN(NCONT) - one
            fcpbk_loop2: DO NCE = NHBED(NLINKA, JBK) + 1, NCETOP
               FCPBKO(NLINKA, JBK, NCE, NCONT) = PPHI(NCE)*TTHET(NCE) + FFSO(NCE)*KKDSO(NCE)*COLCAP(NCE)**GNDUM
               GCPBKO(NLINKA, JBK, NCE, NCONT) = (one - PPHI(NCE))*TTHET(NCE) + (one - FFSO(NCE))*KKDSO(NCE)*SOLCAP(NCE)**GNDUM
            END DO fcpbk_loop2
         END IF

      END DO cont_loop
      ! ++++++++++++ END OF MAIN LOOP +++++++++++

   END SUBROUTINE COLMSM

!> @brief Maps current hydrology and geometry into the one-column contaminant workspace.
!>
!> `COLMW` is called immediately before [[colmsm]] for element `NCL`. It sets
!> the active interval, soil types and nondimensional cell dimensions; moves
!> current hydrological values into their old-state arrays; and prepares
!> old/new water contents, mobile fractions, vertical velocities, lateral
!> flows, surface-water flows, rain, wells, ET, plant withdrawal, and bank/link
!> exchange. The resulting module-scope and `COLM_*` work arrays are valid only
!> for this column.
!>
!> Cell geometry uses the reference length `Z2`:
!>
!> \[
!> KSP_i=\Delta z_i/Z2,\qquad
!> KSPP_i=(z_{i+1}-z_i)/Z2,\qquad
!> ZONE=(ZGRUND-ZCOLMB)/Z2.
!> \]
!>
!> For an explicit bank, `ROH` is the bank share of the bank/link composite
!> width and `VELDUM=1/ROH`; overlapping water contents and vertical flows are
!> width-weighted between bank and link. Ordinary columns use both factors as
!> one. [[phi]] supplies `PPHI` and `PPHI1`. Surface flow signs are
!> `-QOC` on faces 1--2 and `+QOC` on faces 3--4.
!>
!> The top vertical velocity is reconstructed from the surface storage change,
!> evaporation, rainfall, and lateral surface flow. A downward recurrence then
!> balances storage change, wells, root extraction, and lateral subsurface flow.
!> `EMULT` applies correction factors of 0, 0.1, 0.5, and 1 from the top five,
!> next three, next twelve, and remaining cells respectively. The prepared base
!> rate is `QQRF1=AREA(NCL)*UUAJP1(NCEBOT-1)`.
!>
!> @note `NCL` intentionally retains the current declaration without `INTENT`;
!> this documentation-only transfer does not import old-branch attributes.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought former include declarations into the routine and removed `INTEGER*2`. |
!> | 1996-07-17 | GP | 4.0 | Reworked bank, well, layer-flow, irrigation, and correction-damping setup. |
!> | 1997-02-18 | RAH | 4.1 | Swapped the principal VSS array subscripts. |
!> | 1997-03-14--1997-05-21 | RAH | 4.1 | Added explicit typing, simplified the shared workspace, and condensed well handling. |
!> | 1998-11-03 | RAH | 4.2 | Removed obsolete `ERUZO` output. |
!> | 2026-04-03 | SvB | - | Modernised the routine's loops and array operations with Gemini assistance. |
!> @endhistory
   SUBROUTINE COLMW(NCL)
! Commons and constants
      USE sy_state, ONLY: DLS, GNU
      USE cm_column_scaling
      USE cm_column_water
      USE cm_column_previous
      USE cm_column_geometry
      USE cm_bank_geometry
      USE cm_sediment_previous
      USE cm_plant_state

! Input arguments
      INTEGER :: NCL !! Active land or bank element number; read only, but intentionally without imported `INTENT`.

! Locals, etc
      INTEGER :: JAL, JSOIL, JDUM, IW, JA, JLYR, JB
      INTEGER :: NAQU, NCE, NCEA, NCLA, NDIFF, NDUM, NELMA
      DOUBLE PRECISION :: DBK, DMULT, DINV, ROHDUM, OMROH, THEDUM, QVDUM, PHIDUM
      DOUBLE PRECISION :: DUM, DUM0, DUM1, UUOLD, UUNEW, ERRDUM, UIN
      DOUBLE PRECISION :: Q1(LLEE), TRAN1(LLEE), EMULT(LLEE)
      DOUBLEPRECISION, DIMENSION(NELEE), SAVE :: DUMMY !! Floating-point input workspace; scratch within this routine only. `SAVE` keeps it in static storage, as the former module variable was.

!----------------------------------------------------------------------*
! Factors & indices
!___________________*
      SGTSE = SGMA*TSE
      SGSTSE = SGSQ*TSE
      !                             SET FACTORS DEPENDING ON SIGMA
      NCEBOT = NCOLMB(NCL)
      NAQU = NLYRBT(NCL, 1)

      !                             SET BOTTOM COLUMN CELL, AND
      !                             BOTTOM AQUIFER CELL NUMBERS
      NDUM = NCETOP - NAQU + 2
      ROH(NAQU - 1:NAQU - 1 + NDUM - 1) = ONE
      VELDUM(NAQU - 1:NAQU - 1 + NDUM - 1) = ONE

      !                             set defaults
      JBK = ICMREF(NCL, 1)
      ISBK = (JBK /= 0)

      IF (ISBK) THEN
         !                         ELEMENT IS A BANK
         NLINKA = ICMREF(NCL, 4)
         NDIFF = NLYRBT(NLINKA, 1) - NAQU

         !                         NUMBER & CELL OFFSET FOR ASSOCIATED LINK
         JAL = 0
         find_jal: DO
            JAL = JAL + 1
            IF (ICMREF(NLINKA, JAL + 4) == NCL) EXIT find_jal
         END DO find_jal

         JFLINK = ICMREF(NLINKA, JAL + 8)

         !                         NUMBER FOR FACE ASSOCIATED WITH LINK
         DBK = AREA(NCL)/CLENTH(NLINKA)
         DMULT = DBK/(DBK + half*CWIDTH(NLINKA))
         DINV = ONE/DMULT

         DO NCE = NAQU - 1, NCEBD(NLINKA, JBK)
            ROH(NCE) = DMULT
            VELDUM(NCE) = DINV
         END DO

         ROH(NCE) = ONE - (ONE - DMULT)*FNCEBD(NLINKA, JBK)
      ELSE
         !                         NOT A BANK
         JFLINK = 0
      END IF

      !                             SET ROH (& VELDUM): FOR A BANK
      !                             ROH IS THE RATIO OF THE WIDTH OF THE BANK
      !                             SOIL COLUMN TO THE SUM OF THE WIDTH OF THE
      !                             BANK SOIL COLUMN AND HALF THE WIDTH OF THE
      !                             STREAM; IT IS USED IN SUBSURFACE FLOW
      !                             CALCULATIONS TO ALLOW THE SAME CODE TO BE
      !                             USED FOR BANK AND NON-BANK COLUMNS
      !                             NB: ROH IS 1 ABOVE THE BOTTOM OF THE BED
      !                             DEEP LAYER.
      !                             FOR A NON-BANK, ROH IS 1
      !970521                       See also "temporary" section at the end

! Properties for each cell *
!__________________________*
      TRAN1(NAQU:NCETOP) = ERUZ(NCL, NAQU:NCETOP)

      !                             SET LOCAL VECTOR FOR RATE OF PLANT UPTAKE
      !                             OF WATER FOR THE FULL LENGTH OF THE COLUMN
      layer_loop: DO JLYR = 1, NLYR(NCL)
         JSOIL = NTSOIL(NCL, JLYR)

         cell_loop: DO NCE = MAX(NCEBOT, NLYRBT(NCL, JLYR)), NLYRBT(NCL, JLYR + 1) - 1
            JSOL(NCE) = JSOIL
            KSP(NCE) = DELTAZ(NCE, NCL)/Z2
            KSPP(NCE) = (ZVSNOD(NCE + 1, NCL) - ZVSNOD(NCE, NCL))/Z2

            !                     NB kspp(ncetop) is overwritten below
            TTHET(NCE) = VSTHEO(NCL, NCE)
            UUAJP(NCE) = UUAJPO(NCL, NCE)

            IF (JBK == 0) THEN
               !                 regular column element
               TTHET1(NCE) = VSTHE(NCE, NCL)
               UUAJP1(NCE) = QVSV(NCE, NCL)
            ELSE
               !                 element is (L-shaped) bank
               !                 NB uuajp1(nhbed) is overwritten below
               NCEA = NCE + NDIFF
               IF (NCEA <= NCETOP) THEN
                  ROHDUM = ROH(NAQU)
                  OMROH = one - ROHDUM
                  THEDUM = VSTHE(NCEA, NLINKA)
                  QVDUM = QVSV(NCEA, NLINKA)
                  TTHET1(NCE) = OMROH*THEDUM + ROHDUM*VSTHE(NCE, NCL)
                  UUAJP1(NCE) = OMROH*QVDUM + ROHDUM*QVSV(NCE, NCL)
               ELSE
                  TTHET1(NCE) = VSTHE(NCE, NCL)
                  UUAJP1(NCE) = QVSV(NCE, NCL)
               END IF
            END IF

            VSTHEO(NCL, NCE) = TTHET1(NCE)
            UUAJPO(NCL, NCE) = UUAJP1(NCE)
            PHIDUM = PHI(JSOIL, TTHET1(NCE))
            PPHI(NCE) = PHI(JSOIL, TTHET(NCE))
            PPHI1(NCE) = PHIDUM
            GGAMM(NCE) = GGAMMO(NCL, NCE)
            GGAMM1(NCE) = ((one - XXI*PHIDUM)*ROH(NCE)*TRAN1(NCE)/(KSP(NCE)*Z2)) &
                          + (((one - PHIDUM)*TTHET1(NCE) - (one - PPHI(NCE))*TTHET(NCE))/DTUZ)
            GGAMMO(NCL, NCE) = GGAMM1(NCE)
         END DO cell_loop
      END DO layer_loop

      !                             ordinary cells
      KSP(NCETOP + 1) = KSP(NCETOP)
      KSPP(NCETOP) = KSP(NCETOP)
      KSPP(NCEBOT - 1) = DELTAZ(NCEBOT, NCL)/Z2

      !                             special cells for KSP*
      IF (ISBK) THEN
         NCE = NHBED(NLINKA, JBK)
         UUAJP1(NCE) = QVSV(NCE, NCL)
      END IF

      !                             vert. vel. of cell below bed is in top
      !                             part of L-shaped column (over-rides above)
      NCE = NAQU - 1
      UUAJP(NCE) = UUAJPO(NCL, NCE)
      IF (JBK == 0) THEN
         UUAJP1(NCE) = QVSV(NCE, NCL)
      ELSE
         NCEA = NCE + NDIFF
         UUAJP1(NCE) = ((ONE - ROH(NCE))*QVSV(NCEA, NLINKA) + ROH(NCE)*QVSV(NCE, NCL))
      END IF

      UUAJPO(NCL, NCE) = UUAJP1(NCE)

      !                             vert vel for cell below aquifer base
      !                             SET cell properties, moisture content,
      !                             AND VERTICAL FLOW VALUES, AND
      !                             STORE 'OLD' VALUES FOR NEXT TIME STEP
      !970314                       NB See "temporary code" at end of routine

! Properties common to every cell *
!_________________________________*
      TTTLSE = 1.0D-4

      !                             SET MOISTURE CONTENT FOR LOOSE SEDIMENTS
      DDA = DYQQ(NCL)
      DDB = DXQQ(NCL)
      DDDSW = DSWO(NCL)
      DDDSW1 = HRF(NCL) - ZGRUND(NCL)
      DSWO(NCL) = DDDSW1
      DDDLS = DLSO(NCL)
      DDDLS1 = DLS(NCL)
      DLSO(NCL) = DLS(NCL)
      GGGNU = GNUO(NCL)
      GGGNU1 = GNU(NCL)
      GNUO(NCL) = GNU(NCL)
      ZONE = ZONEO(NCL)
      ZONE1 = (ZGRUND(NCL) - ZCOLMB(NCL))/Z2
      ZONEO(NCL) = ZONE1

      !                             SET WIDTHS OF COLUMN,
      !                             DEPTHS OF SURFACE WATER AND
      !                             SEDIMENTS, EROSION RATE, AND
      !                             NON-DIMENSIONED SATURATED DEPTH
      NCEPSF = NCETOP
      !                             FORMERLY (pre v4.0) THE HIGHEST CELL
      !                             NUMBER IN THE SATURATED ZONE;
      !                             now lateral transport is allowed
      !                             up to the ground surface
      CST2 = Z2/(AREA(NCL)*D0)
      CST1 = CST2/ZONE1
      CST3 = CST2/KSP(NCEBOT)

      !                             SET CONSTANTS USED IN CONVECTION TERMS
      convection_loop: DO JA = 1, 4
         NELMA = ICMREF(NCL, JA + 4)
         ISBDY(JA) = (NELMA == 0)

         IF (.NOT. ISBDY(JA)) THEN
            IF (ICMREF(NELMA, 1) == 3) THEN
               NWORK(JA) = ICMREF(NELMA, JA + 4)
            ELSE
               NWORK(JA) = NELMA
            END IF
         ELSE
            NWORK(JA) = NCL
            !                     ASSUME MIRROR IMAGE IF FACE IS AT THE
            !                     BOUNDARY OF CATCHMENT
         END IF
      END DO convection_loop

      !                             SET NWORKj TO THE NUMBER FOR THE COLUMN
      !                             ADJACENT TO FACE j

!+++++ MAIN LOOP FOR COLUMN FACES +++++*
!______________________________________*

      main_face_loop: DO JA = 1, 4
         QQ(NCEBOT - 1:NCETOP + 1, JA) = zero
         QQ1(NCEBOT - 1:NCETOP + 1, JA) = zero
         DUMMY(NCEBOT - 1:NCETOP + 1) = zero

         IF (JA == JFLINK) THEN
            !                     IS INSIDE FACE OF BANK
            DO NCE = NCEBOT, NHBED(NLINKA, JBK)
               NCEA = NCE + NDIFF
               JB = 1 + MOD(JA + 1, 4)
               Q1(NCE) = .5D0*(QVSH(JA, NCEA, NLINKA) - QVSH(JB, NCEA, NLINKA))
            END DO

            Q1(NHBED(NLINKA, JBK) + 1:NCETOP) = QVSH(JA, NHBED(NLINKA, JBK) + 1:NCETOP, NCL)
         ELSE
            !                     neighbour is a land element
            Q1(NCEBOT:NCETOP) = QVSH(JA, NCEBOT:NCETOP, NCL)

            NCLA = ICMREF(NCL, JA + 4)
            IF (ISBK .AND. NCLA > 0) THEN
               IF (ICMREF(NCLA, 1) == 1 .OR. ICMREF(NCLA, 1) == 2) THEN
                  !             add extra flow for end-to-end banks
                  DO NCE = NCEBOT, NHBED(NLINKA, JBK)
                     NCEA = NCE + NDIFF
                     Q1(NCE) = Q1(NCE) + .5D0*QVSH(JA, NCEA, NLINKA)
                  END DO
               END IF
            END IF
         END IF

         !                         SET THE LATERAL FLOW RATES Q1 FOR THE
         !                         ENTIRE DEPTH OF FACE JA OF THE
         !                         CURRENT COLUMN NCL (incl L-shaped banks)
         DO NCE = NCEBOT, NCETOP
            QQ1(NCE, JA) = Q1(NCE)*(ZONE1*ROH(NCE)/KSP(NCE))
            QQ(NCE, JA) = QQO(NCL, NCE, JA)
            QQO(NCL, NCE, JA) = QQ1(NCE, JA)
         END DO
         !                         SET THE OLD AND NEW LATERAL FLOW RATES
         !                         FOR THE SATURATED SECTIONS OF THE FACES
         !                         OF THE CURRENT COLUMN
      END DO main_face_loop
      !                             ++++++++++++ END OF MAIN LOOP ++++++++++++

!__________________________*
      DO JDUM = 1, 2
         QQQSW(JDUM) = QQQSWO(NCL, JDUM)
         QQQSW(JDUM + 2) = QQQSWO(NCL, JDUM + 2)
         QQQSW1(JDUM) = -QOC(NCL, JDUM)
         QQQSWO(NCL, JDUM) = QQQSW1(JDUM)
         QQQSW1(JDUM + 2) = QOC(NCL, JDUM + 2)
         QQQSWO(NCL, JDUM + 2) = QQQSW1(JDUM + 2)
      END DO

      !                             SET RATE OF LATERAL SURFACE WATER FLOW
      !                             INTO THE FOUR FACES OF THE COLUMN

! Boundary Conditions *
!_____________________*
      NCWELL = NVSWLT(NCL)
      IF (NCWELL /= 0) THEN
         QQQWEL = -RSZWLO(NCWELL)*AREA(NCWELL)
         QQQWL1 = -QVSWEL(NCWELL)*AREA(NCWELL)
      ELSE
         QQQWEL = zero
         QQQWL1 = zero
      END IF

      !                             irrigation onto grids
      QI = QIO(NCL)
      QI1 = -PNETTO(NCL)*AREA(NCL)
      QIO(NCL) = QI1

      !                             SET RATE OF RAIN WATER INFLOW (NEGATIVE
      !                             TO CONFORM TO POSITIVE UPWARDS CONVENTION)
      WELDRA(NAQU:NCETOP) = zero
      IW = NVSWLI(NCL)
      IF (IW /= 0) THEN
         WELDRA(NWELBT(NCL):NWELTP(NCL)) = QVSWLI(NWELBT(NCL):NWELTP(NCL), IW)
      END IF

      !                             SET THE RATE OF WELL WITHDRAWL FROM
      !                             INDIVIDUAL CELLS
      QQRV(1:NCETOP) = zero

      IF (ISBK) QQRV(NCEAB(NLINKA, JBK)) = QBKB(NLINKA, JBK)
      !                             SET RATE OF FLOW INTO BANK CELLS FROM
      !                             STREAM WATER. FLOW TAKES PLACE ONLY OVER
      !                             THE SATURATED DEPTH BETWEEN CELL NCEAB AND
      !                             THE EFFECTIVE BED OF THE CHANNEL

!################### temporary code for calc vertical vels. JE 18/9/91
! re-used by GP 24/1/96
! emult: fraction of the error correction which is removed at each cell
      EMULT(MAX(1, NCETOP - 4):NCETOP) = zero
      EMULT(MAX(1, NCETOP - 7):NCETOP - 5) = 0.1D0
      EMULT(MAX(1, NCETOP - 19):NCETOP - 8) = half
      EMULT(NCEBOT:NCETOP - 20) = ONE

      UIN = (DDDSW1 - DDDSW)/(Z2SQOD*TSE)
      DUM = SUM(QQQSW1(1:4))

      UUAJP1(NCETOP) = UIN + EEVAP(NCL) + (QI1 - DUM)/AREA(NCL)

      DO NCE = NCETOP, NCEBOT, -1
         DUM0 = KSP(NCE)/(ROH(NCE)*ZONE1)
         DUM = KSP(NCE)*(TTHET1(NCE) - TTHET(NCE))/(ROH(NCE)*Z2OD*TSE)
         DUM = DUM + WELDRA(NCE) + TRAN1(NCE)
         DUM1 = QQRV(NCE) + DUM0*(QQ1(NCE, 1) + QQ1(NCE, 2) + QQ1(NCE, 3) + QQ1(NCE, 4))
         UUOLD = UUAJP1(NCE - 1)
         UUNEW = (DUM - DUM1/AREA(NCL) + VELDUM(NCE)*UUAJP1(NCE))/VELDUM(NCE - 1)
         ERRDUM = UUNEW - UUOLD
         UUAJP1(NCE - 1) = UUNEW - ERRDUM*EMULT(NCE)
         UUAJPO(NCL, NCE - 1) = UUAJP1(NCE - 1)
      END DO
!################### end of temporary code

      QQRF = QQRFO(NCL)
      QQRF1 = AREA(NCL)*UUAJP1(NCEBOT - 1)
      QQRFO(NCL) = QQRF1
      !                             set rate of flow through base of column

   END SUBROUTINE COLMW

!> @brief Returns the effective longitudinal soil-water dispersion coefficient.
!>
!> This pure placeholder ignores contaminant `NCONT`, soil `JSOIL`, water
!> content `THETA`, and the bounding velocities `UM`/`UP`, and always returns
!> `3.0D-8` m2/s. [[colmsm]] divides the result by reference dispersion `D0`.
!>
!> @warning The manual's `CM59` diffusion and `CM61` dispersivity data are
!> currently discarded by [[cmrd]] and have no effect here.
!> @endwarning
   PURE FUNCTION DISP(NCONT, JSOIL, THETA, UM, UP) RESULT(res)

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: NCONT !! Contaminant number; currently unused.
      INTEGER, INTENT(IN) :: JSOIL !! Soil-type number; currently unused.
      DOUBLE PRECISION, INTENT(IN) :: THETA !! Volumetric water content; currently unused.
      DOUBLE PRECISION, INTENT(IN) :: UM !! Velocity below the cell; currently unused (m/s).
      DOUBLE PRECISION, INTENT(IN) :: UP !! Velocity above the cell; currently unused (m/s).

      ! Return variable
      DOUBLE PRECISION :: res !! Constant effective longitudinal dispersion (m2/s).

      ! ########## SOIL INFO NEEDED HERE #########
      res = 3.0D-8

   END FUNCTION DISP

!> @brief Returns the fraction of soil water treated as mobile.
!>
!> This pure placeholder ignores soil type `JSOIL` and water content `THETA`
!> and always returns `0.5D0`. [[colmw]] uses the result for both old and new
!> water states.
!>
!> @warning The `CM57` mobile-water fractions are read into a local array by
!> [[cmrd]] and discarded, so they do not affect this function.
!> @endwarning
   PURE FUNCTION PHI(JSOIL, THETA) RESULT(res)

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: JSOIL !! Soil-type number; currently unused.
      DOUBLE PRECISION, INTENT(IN) :: THETA !! Volumetric water content; currently unused.

      ! Return variable
      DOUBLE PRECISION :: res !! Constant mobile-water fraction.

      ! Modernization Fix: Native declaration of numeric constant
      DOUBLE PRECISION, PARAMETER :: HALF = 0.5D0

      !----------------------------------------------------------------------*

      ! ########## SOIL INFO NEEDED HERE #########
      res = HALF

   END FUNCTION PHI

!> @brief Solves the coupled mobile/dead-space equations for one column.
!>
!> [[colm]] supplies `N` rows. Eliminating the dead-space rate gives
!>
!> \[
!> \epsilon_i=(QLT_i+TLT_i\Omega_i)/PLT_i,
!> \]
!> \[
!> FLT_i\Omega_{i-1}+
!> \left(ELT_i-\frac{GLT_iTLT_i}{PLT_i}\right)\Omega_i+
!> DLT_i\Omega_{i+1}=SLT_i+\frac{GLT_iQLT_i}{PLT_i}.
!> \]
!>
!> `TRIDAG` solves this reduced system. If nonlinear adsorption is enabled,
!> ten fixed Picard-style updates replace `PLT` by
!> `PLT+PLTSTR*EPS` and add `ELTSTR*OME` to the diagonal before repeating the
!> solve. There is no convergence test or adaptive iteration count.
!>
!> @warning `ISADNL` is the currently unassigned [[cm_solver_flags]] module flag, and
!> the routine divides by `PLT`/`PLTE` without a local zero guard.
!> @endwarning
   SUBROUTINE SLVCLM(n)

      USE cm_column_equations

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: n !! Number of active column-equation rows.

      ! Locals
      INTEGER :: loop
      DOUBLE PRECISION :: ELTE(LLEE), PLTE(LLEE), RHTD(LLEE)

      !----------------------------------------------------------------------*

      ! ALLOCATE WORKSPACE
      ! Modernization Fix: Replaced DO 1 loop with high-performance array slices
      ELTE(1:n) = ELT(1:n) - GLT(1:n)*TLT(1:n)/PLT(1:n)
      RHTD(1:n) = SLT(1:n) + GLT(1:n)*QLT(1:n)/PLT(1:n)

      CALL TRIDAG(FLT, ELTE, DLT, RHTD, OME, n)

      ! ESTIMATE OMEGA AND EPSILON VECTORS
      ! Modernization Fix: Replaced DO 2 loop with array slice
      EPS(1:n) = (QLT(1:n) + TLT(1:n)*OME(1:n))/PLT(1:n)

      IF (ISADNL) THEN
         ! GO ROUND LOOP ONLY IF THERE IS NONLINEAR ADSORPTION
         picard_iteration_loop: DO loop = 1, 10

            ! SET 'NON-LINEAR' COEFFICIENTS
            ! Modernization Fix: Replaced DO 4 loop with array slices
            PLTE(1:n) = PLT(1:n) + PLTSTR(1:n)*EPS(1:n)
            ELTE(1:n) = ELT(1:n) + ELTSTR(1:n)*OME(1:n) - GLT(1:n)*TLT(1:n)/PLTE(1:n)
            RHTD(1:n) = SLT(1:n) + GLT(1:n)*QLT(1:n)/PLTE(1:n)

            ! ESTIMATE OMEGA VECTOR
            CALL TRIDAG(FLT, ELTE, DLT, RHTD, OME, n)

            ! ESTIMATE EPSILON VECTOR
            ! Modernization Fix: Replaced DO 5 loop with array slice
            EPS(1:n) = (QLT(1:n) + TLT(1:n)*OME(1:n))/PLTE(1:n)

         END DO picard_iteration_loop
      END IF

   END SUBROUTINE SLVCLM

END MODULE cm_column

