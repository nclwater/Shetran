!> summary: Channel-link assembly and solve, including the bank exchange.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> The channel path of the transport calculation. [[LINKW]] prepares one link's
!> water and bank state, [[LINKSM]] assembles its balance, and [[LINK]] solves
!> it together with [[SNL3]].
!>
!> **[[LINK]] and [[SNL3]] are mutually recursive** and must stay in the same
!> module; that pair is one of only two mutual recursions in the tree.
!>
!> As in [[cm_column]], the module-level variables are one link's working
!> values rather than state per link.
!>
!> | Module-scope work state | Producer | Consumer |
!> |:------------------------|:---------|:---------|
!> | `LWORK`, `NBK`, `ISLK`, `QQQSL1` | [[LINKW]] | [[LINKSM]] and [[LINK]] |
!> | `NWELL`, `QQQDUM` | Intended irrigation hand-off | [[LINKSM]] |
!>
!> @warning
!> `NWELL` and `QQQDUM` are intended module-scope irrigation work values, but
!> [[LINKW]] currently redeclares and assigns local variables with the same
!> names. [[LINKSM]] therefore reads the unassigned module variables.
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
MODULE cm_channel

   USE MOD_PARAMETERS, ONLY: half, one, two, zero
   USE array_limits, ONLY: NSEDEE
   USE element_geometry, ONLY: area => cellarea
   USE grid_topology, ONLY: ICMREF, ICMRF2
   USE channel_geometry, ONLY: CLENTH, CWIDTH, FHBED, LINKNS, NHBED
   USE et_state, ONLY: NVC, PNETTO
   USE vs_state, ONLY: DELTAZ, NVSWLT, QBKB, QVSH, VSTHE
   USE oc_state, ONLY: ARXL, QOC
   USE sy_state, ONLY: ARBDEP, DLS, FBETA, FBTSD, FDEL, GINFD, GINFS, GNUBK, NSED, NSOBED, &
                       PBSED, QDEFF, QLINK, SOSDFN
   USE cm_parameters, ONLY: ALPHBD, ALPHBS, CCAPE, CCAPI, CCAPIN, CCCC, CCCCO, CCCCW, GCPLA, &
                            GNN, IIICF, KDDLS, NCON, SSSSO
   USE cm_bank_geometry, ONLY: FNCEBD, NBANK, NCEBD
   USE cm_column_previous, ONLY: RSZWLO
   USE cm_column_scaling, ONLY: D0, Z2, Z2OD, Z2SQ, Z2SQOD
   USE cm_link_water, ONLY: ACPBDO, ACPBI, ACPBSG, ACPSFO, THBED, THBEDO
   USE cm_sediment_previous, ONLY: FBBEDO, FBTSDO, FDELO
   USE cm_solver_flags, ONLY: ISADNL
   USE cm_sorption, ONLY: FRET
   USE float_compare, ONLY: iszero, notzero

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: LINKSM, LINKW

   INTEGER :: LWORK(6)             !! Up to three adjacent-link entries at each end of the current link.
   INTEGER :: NBK(2)               !! Bank-column element numbers on the two sides of the current link.
   INTEGER :: nwell                !! Intended current link irrigation-well number; shadowed in [[linkw]].
   LOGICAL :: islk(2)              !! True where the corresponding end of the current link connects to another link.
   DOUBLEPRECISION :: qqqdum       !! Intended current link well inflow rate; shadowed in [[linkw]].
   DOUBLEPRECISION :: QQQSL1       !! Current effective rainfall input rate to the link, using the contaminant sign convention.

CONTAINS

!> @brief Prepares and updates every contaminant in one channel link.
!>
!> For `NLINK`, this routine loads the preceding concentrations of the deep
!> bed, bed surface, and stream water; gathers adjacent-link or boundary
!> concentrations; normalises deposited and suspended sediment fractions; and
!> calls [[fret]] for each storage compartment. A link with `USCP<=0.5` starts
!> from `CCAPIN` and omits stream/bed-surface infiltration; a wet link uses its
!> old stream concentration.
!>
!> The main scaled source terms are
!>
!> \[
!> ICP1=-IIICF\,AREA/(D0\,CLENTH),
!> \]
!> \[
!> QCP1=\frac{(QQQSL1-QQQDUM)CCAPI+QQQDUM\,C_{well}}
!>             {D0\,Z2\,KS},
!> \]
!>
!> and channel-bed exchange uses
!> \(ACSBD1=CWIDTH\,ALPHBD/D0\) and
!> \(ACSBS1=CWIDTH\,ALPHBS/D0\). [[link]] returns the three updated
!> concentrations, which are stored in cells `NCETOP-2:NCETOP`. Their
!> linearised retardation and concentration values are retained for the next
!> contaminant in the decay chain.
!>
!> @warning The `NWELL` and `QQQDUM` referenced here are unassigned
!> module-scope values because [[linkw]] shadows them with locals. The current
!> irrigation source calculation is therefore undefined under standard
!> Fortran.
!> @endwarning
!>
!> @warning `FCPSW1(JBK)` indexes `CCPBK(JBK,NCONT)` as though the contaminant
!> number were a bank-cell number. This can select the wrong cell and can exceed
!> the cell extent when `NCONT` is large; behaviour is retained unchanged.
!> @endwarning
!>
!> @note Both deep-bed and bed-surface infiltration magnitudes currently use
!> `SUMD`, the total `GINFD` rate. `SUMS` is used only to normalise `FBTAS`
!> from `GINFS`; this potentially surprising current distinction is retained.
!> @endnote
   SUBROUTINE LINKSM(NLINK)

      USE cm_parameters
      USE sy_state, ONLY: DLS, FBETA, FBTSD, FDEL, GINFD, GINFS, NSED, NSOBED, PBSED, SOSDFN
      USE cm_column_scaling
      USE cm_link_state
      USE cm_link_water
      USE cm_sediment_previous
      USE cm_plant_state

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NLINK !! Active channel-link element number.

      ! Locals, etc
      INTEGER :: NCONT, NCE, JLEND, JDUM, JLA, JSED, NA, LFONE, LDUM, LA, JBK

      DOUBLE PRECISION :: FBTAD(NSEDEE), FBTAS(NSEDEE), KDDUM(NSEDEE)
      DOUBLE PRECISION :: SSBED1(NSEDEE), SSBED(NSEDEE), SSD1(NSEDEE), SSD(NSEDEE)
      DOUBLE PRECISION :: SSF1(NSEDEE), SSF(NSEDEE)

      DOUBLE PRECISION :: CCPBD, CCPBS, QCDUM, SUMD, SUMS, DDDSUM, PB, FDUM, FDUMC
      DOUBLE PRECISION :: FDUMT, DUM, ARL, ARP, CCPSF, DDDUM, DSDUM, CCPBD1, CCPBS1
      DOUBLE PRECISION :: CCPSF1, DUMX

      !----------------------------------------------------------------------*

      CCBD1Q = ZERO
      CCBS1Q = ZERO
      CCSF1Q = ZERO
      FCBD1Q = ZERO
      FCBS1Q = ZERO
      FCSF1Q = ZERO
      GCPLAQ = ZERO

      ! SET PARENT CONCENTRATIONS AND RETARDATION
      ! VARIABLES TO O FOR 1ST PASS OF THE CONTAMINANT LOOP
      cont_loop: DO NCONT = 1, NCON

         bank_loop: DO JBK = 1, 2
            CCPBK(JBK, 1) = CCCCO(NBK(JBK), 1, NCONT)
            ! THIS ELEMENT OF ARRAY CCCCO IS USED TO HOLD THE EFFECTIVE
            ! CONCENTRATION IN THE FLOW ENTERING THE STREAM VIA THE STREAM BED

            cell_loop: DO NCE = NCEBK(JBK), NCETOP
               CCPBK(JBK, NCE) = CCCCO(NBK(JBK), NCE, NCONT)
               SCPBK(JBK, NCE) = SSSSO(NBK(JBK), NCE, NCONT)
            END DO cell_loop

            CCPGS1(JBK) = CCCC(NBK(JBK), NCETOP, NCONT)
         END DO bank_loop

         CCPBD = CCCCO(NLINK, NCETOP - 2, NCONT)
         CCPBS = CCCCO(NLINK, NCETOP - 1, NCONT)

         IF (USCP > HALF) THEN
            CCPSF = CCCCO(NLINK, NCETOP, NCONT)
         ELSE
            ! IF THERE IS NO WATER IN LINK
            CCPSF = CCAPIN(NCONT)
         END IF

         end_loop: DO JLEND = 1, 2
            IF (ISLK(JLEND)) THEN
               ! THERE ARE OTHER LINKS ASSOCIATED WITH END JLEND OF THE CURRENT LINK
               adj_loop: DO JDUM = 1, 3
                  JLA = (JLEND - 1)*3 + JDUM
                  LA = LWORK(JLA)
                  IF (LA /= 0) THEN
                     CCSFA1(JLA) = CCCC(LA, NCETOP, NCONT)
                     FCSFA1(JLA) = FSF(LA, NCONT) + FSFT(LA, NCONT)*TSE + &
                                   FSFC(LA, NCONT)*(CCSFA1(JLA) - CCCCO(LA, NCETOP, NCONT))
                  ELSE
                     CCSFA1(JLA) = ZERO
                     FCSFA1(JLA) = ZERO
                  END IF
               END DO adj_loop
            ELSE
               ! END JLEND OF LINK IS AT CATCHMENT BOUNDARY
               ! THE HEAD OF A STREAM, OR A SPRING
               JLA = (JLEND - 1)*3 + 1
               CCSFA1(JLA) = CCAPE(NLINK, NCONT)
               FCSFA1(JLA) = ONE
               ! FOR FLOW INTO CATCHMENT OR SPRING
               adj_zero_loop: DO JDUM = 2, 3
                  JLA = (JLEND - 1)*3 + JDUM
                  CCSFA1(JLA) = ZERO
                  FCSFA1(JLA) = ZERO
               END DO adj_zero_loop
            END IF
         END DO end_loop

         ! SET LINK AND BANK CONCENTRATIONS.
         ! NB: IF THE STREAM IS DRY, THE STREAM WATER CONCENTRATION
         ! SET TO THE CONCENTRATION IN RAIN WATER
         ICP1 = -IIICF(NCONT)*AREA(NLINK)/(D0*CLENTH(NLINK))

         !#######################################################################
         QCDUM = (QQQSL1 - QQQDUM)*CCAPI(NCONT)
         IF (NWELL /= 0) QCDUM = QCDUM + QQQDUM*CCCCW(NWELL, NCONT)

         QCP1 = QCDUM/(D0*Z2*KS)
         ! QCP1 = QQQSL1*CCAPI(NCONT)/(D0*Z2*KS)

         ! SET VARIABLES FOR WET AND DRY INPUT OF CONTAMINANT FROM ABOVE
         !######## temporary code for inclusion of irrigation water in rain water
         SUMD = ZERO
         SUMS = ZERO

         sum_loop: DO JSED = 1, NSED
            SUMD = SUMD + GINFD(NLINK, JSED)
            SUMS = SUMS + GINFS(NLINK, JSED)
            KDDUM(JSED) = KDDLS(JSED, NCONT)
         END DO sum_loop

         IF (ISZERO(SUMD)) THEN
            DDDUM = ONE
         ELSE
            DDDUM = SUMD
         END IF

         IF (ISZERO(SUMS)) THEN
            DSDUM = ONE
         ELSE
            DSDUM = SUMS
         END IF

         scale_loop: DO JSED = 1, NSED
            FBTAD(JSED) = GINFD(NLINK, JSED)/DDDUM
            FBTAS(JSED) = GINFS(NLINK, JSED)/DSDUM
         END DO scale_loop

         ! SCALE RATES OF INFLITRATION TO GIVE THE FRACTIONS IN EACH GROUP OF
         ! AN EFFECTIVE SOIL. THE EFFECTIVE SOIL IS THAT WHICH IF ERODED AT A
         ! RATE EQUAL TO THE TOTAL RATE OF INFILTRATION WOULD RELEASE THE CORRECT
         ! AMOUNT OF SEDIMENTS FOR INFILTRATION

         PB = PBSED(NLINK)
         FDUM = ZERO
         FDUMC = ZERO
         FDUMT = ZERO

         CALL FRET(CCPBS, GNN(NCONT), PB, PB, FBTAD, FBTAD, KDDUM, PB, PB, PB, &
                   FDUM, FDUMC, FDUMT, TSE, NSED, ISADNL)

         DUM = SUMD*CCPBS/CLENTH(NLINK)
         ICPSBD = (FDUM - PB)*DUM
         ICSBDC = FDUMC*DUM + ICPSBD
         ICSBDT = FDUMT*DUM

         ! SET INFILTRATION VARIABLES FOR BED DEEP LAYER
         IF (USCP < HALF) THEN
            ! THERE IS NO WATER IN LINK
            ICPSBS = ZERO
            ICSBSC = ZERO
            ICSBST = ZERO
         ELSE
            CALL FRET(CCPSF, GNN(NCONT), ONE, ONE, FBTAS, FBTAS, KDDUM, ZERO, ZERO, ZERO, &
                      FDUM, FDUMC, FDUMT, TSE, NSED, ISADNL)
            DUM = SUMD*CCPSF/CLENTH(NLINK)
            ICPSBS = (FDUM - PB)*DUM
            ICSBSC = FDUMC*DUM + ICPSBS
            ICSBST = FDUMT*DUM
         END IF

         ! SET INFILTRATION VARIABLES FOR BED SURFACE LAYER
         ARL = DLS(NLINK)*CWIDTH(NLINK)
         ! X-SECIONAL AREA OF LOOSE SEDIMENTS IN BED
         ARP = (ACPBD1 - ACPBS)*Z2SQ
         ! X-SECTIONAL AREA OF NON-ERODED PARENT MATERIAL WITHIN BED DEEP LAYER

         DUM = ONE/(ARL + ARP)

         bed_loop: DO JSED = 1, NSED
            SSBED1(JSED) = DUM*(ARL*FBETA(NLINK, JSED) + ARP*SOSDFN(NSOBED(NLINK), JSED))
            SSBED(JSED) = FBBEDO(NLINK, JSED)
            FBBEDO(NLINK, JSED) = SSBED1(JSED)

            SSF1(JSED) = FDEL(NLINK, JSED)
            SSF(JSED) = FDELO(NLINK, JSED)
            FDELO(NLINK, JSED) = SSF1(JSED)

            SSD1(JSED) = FBTSD(NLINK, JSED)
            SSD(JSED) = FBTSDO(NLINK, JSED)
            FBTSDO(NLINK, JSED) = SSD1(JSED)
         END DO bed_loop

         CALL FRET(CCPBD, GNN(NCONT), THBEDO(NLINK), THBED(NLINK), SSBED, SSBED1, &
                   KDDUM, PB, PB, PB, FCPBD, FCPBDC, FCPBDT, TSE, NSED, ISADNL)

         CALL FRET(CCPBS, GNN(NCONT), THBEDO(NLINK), THBED(NLINK), SSBED, SSBED1, &
                   KDDUM, PB, PB, PB, FCPBS, FCPBSC, FCPBST, TSE, NSED, ISADNL)

         CALL FRET(CCPSF, GNN(NCONT), ONE, ONE, SSF, SSF1, KDDUM, ZERO, ZERO, ZERO, &
                   FCPSF, FCPSFC, FCPSFT, TSE, NSED, ISADNL)

         FSF(NLINK, NCONT) = FCPSF
         FSFC(NLINK, NCONT) = FCPSFC
         FSFT(NLINK, NCONT) = FCPSFT
         ! save retardation factors for con

         CALL FRET(CCPSF, GNN(NCONT), ONE, ONE, SSD, SSD1, KDDUM, ZERO, ZERO, ZERO, &
                   FCPSD, FCPSDC, FCPSDT, TSE, NSED, ISADNL)

         ! SET REATRDATION VARIABLES FOR THE BED DEEP LAYER, BED SURFACE LAYER,
         ! STREAM WATER, AND NEWLY DEPOSITED SEDIMENTS

         ret_bank_loop: DO JBK = 1, 2
            NA = NBK(JBK)
            FCPSW1(JBK) = RSW(NA, NCONT) + RSWT(NA, NCONT)*TSE + &
                          RSWC(NA, NCONT)*(CCCC(NA, NCETOP, NCONT) - CCPBK(JBK, NCONT))

            ret_cell_loop: DO NCE = NCEBK(JBK), NCETOP
               FCPBK(JBK, NCE) = FCPBKO(NLINK, JBK, NCE, NCONT)
               GCPBK(JBK, NCE) = GCPBKO(NLINK, JBK, NCE, NCONT)
            END DO ret_cell_loop
            ! NB: FCPBKO AND GCPBKO CALCULATED IN COLMSM
         END DO ret_bank_loop

         ! SET RETRDATION VARIABLES FOR THE DYNAMIC AND DEAD SPACE REGIONS OF THE ERODING BANK SOIL
         ECPBD = ZERO
         ECPBDC = ZERO
         ECPBDT = ZERO
         ECPBS = ZERO
         ECPBSC = ZERO
         ECPBST = ZERO
         ECPSF = ZERO
         ECPSFC = ZERO
         ECPSFT = ZERO

         ! SET RATES OF PLANT UPTAKE
         DUM = CWIDTH(NLINK)/D0
         ACSBD1 = DUM*ALPHBD(NCONT)
         ACSBS1 = DUM*ALPHBS(NCONT)
         GCPLAL = GCPLA(NCONT)

         ! SET CONTAMINANT INFILTRATION RATE WITH SEDIMENT; AND CONTAMINANT DECAY RATE
         CCPBD1 = ZERO
         CCPBS1 = ZERO
         CCPSF1 = ZERO

         ! CALCULATES AND RETURNS UPDATED CONCENTRATIONS
         CALL LINK(CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, NCETOP)

         CCCC(NLINK, NCETOP - 2, NCONT) = CCPBD1
         CCCC(NLINK, NCETOP - 1, NCONT) = CCPBS1
         CCCC(NLINK, NCETOP, NCONT) = CCPSF1

         ! SAVE UPDATED CONCENTRATIONS IN THE GLOBAL ARRAYS
         CCBD1Q = CCPBD1
         CCBS1Q = CCPBS1
         CCSF1Q = CCPSF1
         FCBD1Q = FCPBD + FCPBDT*TSE + FCPBDC*(CCPBD1 - CCPBD)
         FCBS1Q = FCPBS + FCPBST*TSE + FCPBSC*(CCPBS1 - CCPBS)
         FCSF1Q = FCPSF + FCPSFT*TSE + FCPSFC*(CCPSF1 - CCPSF)
         GCPLAQ = GCPLAL

         ! SET CONCENTRATIONS, RETARDATION, AND DECAY VARIABLES FOR PARENT
         ! CONTAMINANT FOR NEXT PASS OF CONTAMINANT LOOP

      END DO cont_loop

   END SUBROUTINE LINKSM

!> @brief Maps hydrology, topology, banks, and sediment geometry into the link workspace.
!>
!> `LINKW` is called immediately before [[linksm]] for `NLINK`. It maps the
!> link's two ends onto up to six adjacent links, identifies the two bank
!> columns, non-dimensionalises stream and bed storage, and prepares link-end,
!> bank-subsurface, bank-surface, rainfall, and bed-exchange flows. `KS` is the
!> half-link scale `CLENTH/Z2` used by the subsequent equations.
!>
!> A link is treated as wet only when `ACPSF1=ARXL/Z2SQ` is nonzero. The wet
!> path calculates `ACPBDT`, `ACPSFT`, and `WCPBD1`; the dry path zeros those
!> derivatives, its end-flow Peclet terms, and both `QBKB` bank flows. End and
!> bank terms use the current sign/scaling conventions
!>
!> \[
!> PCSFM1=QLINK_1/(D0\,Z2\,ACPSF1),\qquad
!> PCSFP1=QLINK_2/(D0\,Z2\,ACPSF1),
!> \]
!> \[
!> PCPBK1_{b,k}=-QVSH_{b,k}/(D0\,Z2\,KS),\qquad
!> PCPSB1_b=-QBKB_b/(D0\,Z2\,KS).
!> \]
!>
!> It also recalculates `THBED` as a thickness-weighted mean of the adjacent
!> bank water contents, including fractional end cells, capped at `PBSED`.
!> `QQQSL1=-PNETTO*AREA` is the effective rainfall input using the contaminant
!> convention that upward water flow is positive.
!>
!> @warning Local `NWELL` and `QQQDUM` declarations shadow the module values
!> consumed by [[linksm]], so the intended irrigation hand-off does not occur.
!> @endwarning
!>
!> @warning During the final bed-moisture loop, both banks start from the single
!> `NDUM` left by the preceding loop (the bank-2 value). When bank base cells
!> differ, bank 1 is consequently integrated from bank 2's base. The final
!> `SUM/SUMK` also has no zero guard.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1997-02-18 | RAH | 4.1 | Swapped the `QVSH`, `DELTAZ`, and `VSTHE` subscripts. |
!> @endhistory
   SUBROUTINE LINKW(NLINK)

      USE sy_state, ONLY: ARBDEP, GNUBK, PBSED, QDEFF, QLINK
      USE cm_column_scaling
      USE cm_column_geometry
      USE cm_column_previous
      !####################temporary, for irrigation
      USE cm_link_state
      USE cm_link_water
      USE cm_bank_geometry
      USE cm_plant_state

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NLINK !! Active channel-link element number.

      ! Locals
      INTEGER :: JLEND, JDUM, LFONE, LDUM, JLA, JFDUM, JFDUMB, NCE, JVEGBK
      INTEGER :: NDUM, LA, JBK

      ! Temporary variables for irrigation logic
      INTEGER :: NWELL !! Local irrigation-well number; shadows the intended module hand-off.
      DOUBLE PRECISION :: QQQDUM !! Local irrigation rate; shadows the intended module hand-off.

      DOUBLE PRECISION :: DUMX, DUM, DUMA, DMULT, SUMK, SUM, DUMK

      !----------------------------------------------------------------------*

      IF (LINKNS(NLINK)) THEN
         LENDA(1) = 2
         LENDA(2) = 2
         LENDA(3) = 1
         LENDA(4) = 1
         LENDA(5) = 1
         LENDA(6) = 2
      ELSE
         LENDA(1) = 1
         LENDA(2) = 2
         LENDA(3) = 2
         LENDA(4) = 2
         LENDA(5) = 1
         LENDA(6) = 1
      END IF

      ! SET POINTERS FOR THE END OF THE LINKS WHICH CAN BE ATTACHED TO A GIVEN LINK

      ACPBD1 = ACPBI(NLINK) + ARBDEP(NLINK)/Z2SQ
      ACPBS = ACPBSG(NLINK)
      ACPSF1 = ARXL(NLINK)/Z2SQ

      IF (ACPSF1 < 1.0D-20) THEN
         USCP = ZERO
         ACPBDT = ZERO
         ACPSFT = ZERO
         QBKB(NLINK, 1) = ZERO
         QBKB(NLINK, 2) = ZERO
         ! ENSURES BED LAYER CALCULATIONS ARE CORRECT IF THERE IS NO WATER IN LINK
      ELSE
         USCP = ONE
         ACPBDT = (ACPBD1 - ACPBDO(NLINK))/TSE
         ACPSFT = (ACPSF1 - ACPSFO(NLINK))/TSE
      END IF

      ACPBDO(NLINK) = ACPBD1
      ACPSFO(NLINK) = ACPSF1
      WCPBD1 = Z2SQOD*ACPBDT/ACPBD1
      VCPBK1 = Z2OD*GNUBK(NLINK)

      ! SET SCALED VARIABLES FOR AREA AND EROSION
      NBK(1) = NBANK(NLINK, 1)
      NBK(2) = NBANK(NLINK, 2)
      NCEBK(1) = NHBED(NLINK, 1) + 1
      NCEBK(2) = NHBED(NLINK, 2) + 1

      ! SET LOCAL BANK NUMBERS AND NUMBERS FOR THE BOTTOM CELLS FOR THE
      ! EXPOSED PART OF THE BANK SOIL COLUMNS

      IF (LINKNS(NLINK)) THEN
         LFONE = 2
      ELSE
         LFONE = 1
      END IF

      ! SET NUMBER FOR THE FACE OF THE LINK WHICH IS AT END ONE OF THE LINK

      LDUM = ICMREF(NLINK, LFONE + 4)
      IF (LDUM > 0) THEN
         ! THERE IS ONLY ONE OTHER LINK ASSOCIATED WITH END ONE OF THE CURRENT LINK
         ISLK(1) = .TRUE.
         LWORK(1) = 0
         LWORK(2) = 0
         LWORK(3) = 0

         IF (LINKNS(NLINK)) THEN
            IF (ICMREF(NLINK, 10) == 3) LWORK(1) = LDUM
            IF (ICMREF(NLINK, 10) == 4) LWORK(2) = LDUM
            IF (ICMREF(NLINK, 10) == 1) LWORK(3) = LDUM
         ELSE
            IF (ICMREF(NLINK, 9) == 2) LWORK(1) = LDUM
            IF (ICMREF(NLINK, 9) == 3) LWORK(2) = LDUM
            IF (ICMREF(NLINK, 9) == 4) LWORK(3) = LDUM
         END IF
         ! LWORK HOLDS THE NUMBERS OF THE LINKS ASSOCIATED WITH THE CURRENT LINK

      ELSE IF (LDUM < 0) THEN
         ! THERE IS MORE THAN ONE LINK ASSOCIATED WITH END ONE OF THE CURRENT LINK
         ISLK(1) = .TRUE.
         LWORK(1) = ICMRF2(-LDUM, 3)
         LWORK(2) = ICMRF2(-LDUM, 2)
         LWORK(3) = ICMRF2(-LDUM, 1)
      ELSE
         ! THERE ARE NO LINKS ASSOCIATED WITH END ONE OF THE CURRENT LINK
         ISLK(1) = .FALSE.
         LWORK(1) = 0
         LWORK(2) = 0
         LWORK(3) = 0
      END IF

      LDUM = ICMREF(NLINK, LFONE + 6)
      IF (LDUM > 0) THEN
         ! THERE IS ONLY ONE OTHER LINK ASSOCIATED WITH END TWO OF THE CURRENT LINK
         ISLK(2) = .TRUE.
         LWORK(4) = 0
         LWORK(5) = 0
         LWORK(6) = 0

         IF (LINKNS(NLINK)) THEN
            IF (ICMREF(NLINK, 12) == 1) LWORK(4) = LDUM
            IF (ICMREF(NLINK, 12) == 2) LWORK(5) = LDUM
            IF (ICMREF(NLINK, 12) == 3) LWORK(6) = LDUM
         ELSE
            IF (ICMREF(NLINK, 11) == 4) LWORK(4) = LDUM
            IF (ICMREF(NLINK, 11) == 1) LWORK(5) = LDUM
            IF (ICMREF(NLINK, 11) == 2) LWORK(6) = LDUM
         END IF
         ! LWORK HOLDS THE NUMBERS OF THE LINKS ASSOCIATED WITH THE CURRENT LINK

      ELSE IF (LDUM < 0) THEN
         ! THERE IS MORE THAN ONE LINK ASSOCIATED WITH END TWO OF THE CURRENT LINK
         ISLK(2) = .TRUE.
         LWORK(4) = ICMRF2(-LDUM, 3)
         LWORK(5) = ICMRF2(-LDUM, 2)
         LWORK(6) = ICMRF2(-LDUM, 1)
      ELSE
         ! THERE ARE NO LINKS ASSOCIATED WITH END TWO OF THE CURRENT LINK
         ISLK(2) = .FALSE.
         LWORK(4) = 0
         LWORK(5) = 0
         LWORK(6) = 0
      END IF

      ! SET LWORK, THE ARRAY HOLDING THE NUMBERS OF THE LINKS ASSOCIATED WITH THE CURRENT LINK

      DUMX = ONE/(D0*Z2)

      end_links_loop: DO JLEND = 1, 2
         IF (ISLK(JLEND)) THEN
            ! THERE ARE OTHER LINKS ASSOCIATED WITH END JLEND OF THE CURRENT LINK
            adj_links_loop: DO JDUM = 1, 3
               JLA = (JLEND - 1)*3 + JDUM
               LA = LWORK(JLA)
               IF (LA /= 0) THEN
                  ACSFA1(JLA) = MAX(1.0D-6, ACPSFO(LA))
                  DUM = ZERO
                  PCSFA1(JLA) = DUMX*(-QLINK(LA, LENDA(JLA)) - QDEFF(LA, LENDA(JLA))*DUM)/ACSFA1(JLA)
                  ! NB: CONVECTION WITH DISPERSED SEDIMENTS NEGLECTED
               ELSE
                  ACSFA1(JLA) = ZERO
                  PCSFA1(JLA) = ZERO
               END IF
            END DO adj_links_loop
         ELSE
            ! END JLEND OF LINK IS AT CATCHMENT BOUNDARY THE HEAD OF A STREAM, OR A SPRING
            JLA = (JLEND - 1)*3 + 1
            ACSFA1(JLA) = MAX(1.0D-6, ACPSFO(NLINK))
            PCSFA1(JLA) = DUMX*QLINK(NLINK, JLEND)/ACSFA1(JLA)
            ! FOR FLOW INTO CATCHMENT OR FROM SPRING

            boundary_links_loop: DO JDUM = 2, 3
               JLA = (JLEND - 1)*3 + JDUM
               ACSFA1(JLA) = ZERO
               PCSFA1(JLA) = ZERO
            END DO boundary_links_loop
         END IF
      END DO end_links_loop

      IF (USCP < HALF) THEN
         PCSFM1 = ZERO
         PCSFP1 = ZERO
      ELSE
         DUM = DUMX/ACPSF1
         DUMA = ZERO
         PCSFM1 = DUM*(QLINK(NLINK, 1) + DUMA*QDEFF(NLINK, 1))
         PCSFP1 = DUM*(QLINK(NLINK, 2) + DUMA*QDEFF(NLINK, 2))
      END IF

      ! SET AREA AND PECLET NUMBER FOR THE LINKS ASSOCIATED WITH THE CURRENT LINKS

      QQQSL1 = -PNETTO(NLINK)*AREA(NLINK)
      ! -VE RATE OF RAIN ARRIVAL AT LINK (+VE UPWARDS TO CONFORM TO CONVENTION)

      !#######################################################################
      NWELL = NVSWLT(NLINK)
      IF (NWELL /= 0) THEN
         QQQDUM = -RSZWLO(NWELL)*AREA(NWELL)
      ELSE
         QQQDUM = ZERO
      END IF
      !###########temporary, qqqdum is rate of input of well water to stream##

      KS = CLENTH(NLINK)/Z2
      ! SET SCALED LENGTH OF LINK
      DUM = DUMX/KS

      banks_loop: DO JBK = 1, 2
         JFDUM = 2*JBK - LFONE + 1
         ! FACE NUMBER FOR LINK, ACROSS WHICH WATER ENTERS FROM BANK JBK
         JFDUMB = ICMREF(NLINK, JFDUM + 8)
         ! FACE NUMBER FOR BANK, POINTING TOWARDS L

         bank_cells_loop: DO NCE = NCEBK(JBK), NCETOP
            PCPBK1(JBK, NCE) = -DUM*QVSH(JFDUMB, NCE, NBK(JBK))
         END DO bank_cells_loop

         PCPSB1(JBK) = -DUM*QBKB(NLINK, JBK)
         DMULT = DBLE(2*JBK - 3)
         ! MULTIPLIER USED TO OBTAIN CORRECT SIGN FOR FLOWS INTO THE LINK

         PCPSW1(JBK) = DMULT*DUM*QOC(NLINK, JFDUM)
         JVEGBK = NVC(NBK(JBK))
         NDUM = NCEBD(NLINK, JBK) + 1

         ! KSPBK IS ONLY USED FOR THE CELLS AT OR ABOVE THE LEVEL OF THE BOTTOM OF THE BED DEEP LAYER
         kspbk_loop: DO NCE = NDUM, NCETOP
            KSPBK(JBK, NCE) = DELTAZ(NCE, NBK(JBK))/Z2
         END DO kspbk_loop

      END DO banks_loop

      ! SET VALUES OF VARIABLES ASSOCIATED WITH THE ADJACENT STREAM BANKS

      SUMK = ZERO
      SUM = ZERO

      bed_cells_loop: DO JBK = 1, 2
         NCE = NDUM
         DUMK = (ONE - FNCEBD(NLINK, JBK))*KSPBK(JBK, NCE)
         SUMK = SUMK + DUMK
         SUM = SUM + VSTHE(NCE, NBK(JBK))*DUMK

         inner_bed_loop: DO NCE = NDUM + 1, NHBED(NLINK, JBK)
            DUMK = KSPBK(JBK, NCE)
            SUMK = SUMK + DUMK
            SUM = SUM + VSTHE(NCE, NBK(JBK))*DUMK
         END DO inner_bed_loop

         NCE = NHBED(NLINK, JBK) + 1
         DUMK = FHBED(NLINK, JBK)*KSPBK(JBK, NCE)
         SUMK = SUMK + DUMK
         SUM = SUM + VSTHE(NCE, NBK(JBK))*DUMK
      END DO bed_cells_loop

      THBEDO(NLINK) = THBED(NLINK)
      THBED(NLINK) = MIN(PBSED(NLINK), SUM/SUMK)

      ! SET MOISTURE CONTENT IN STREAM BED, AS THE WEIGHTED AVERAGE FOR THE CELLS,
      ! OF BOTH BANKS, LYING WITHIN THE BED SURFACE AND BED DEEP LAYERS

   END SUBROUTINE LINKW

!> @brief Solves the fully implicit three-compartment contaminant balance for one link.
!>
!> Shared coefficients prepared by [[linkw]] and [[linksm]] couple stream
!> water, bed surface, deeper bed, both banks, adjacent links, sediment,
!> rainfall/well input, decay/generation, and external sinks. The unknowns are
!> rates `WMESF`, `WMEBS`, and `WMEBD`, used after the solve as
!>
!> \[
!> C_{sf}^{n+1}=C_{sf}^n+TSE\,W_{sf},\quad
!> C_{bs}^{n+1}=C_{bs}^n+TSE\,W_{bs},\quad
!> C_{bd}^{n+1}=C_{bd}^n+TSE\,W_{bd}.
!> \]
!>
!> The coefficient groups `ALT`, `BLT`, `DLT`, `ELT`, `FLT`, `HLT`, and
!> `GYLT` and their nonlinear starred forms are mapped onto [[snl3]]'s system
!>
!> \[
!> (A+A_sX_1)X_1-(B+B_sX_2)X_2-CX_3=P,
!> \]
!> \[
!> -(D+D_sX_1)X_1+(E+E_sX_2)X_2-(F+F_sX_3)X_3=Q,
!> \]
!> \[
!> -(H+H_sX_2)X_2+(Y+Y_sX_3)X_3=S.
!> \]
!>
!> For a wet link (`USCP>=0.5`) all three equations are solved. For a dry link,
!> the first row is replaced by `WMESF=0`; the bed-surface and deep-bed
!> equations remain coupled and continue to advance.
   SUBROUTINE LINK(CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, NCETOP)

      USE cm_link_state
      USE cm_link_scaling

      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN) :: NCETOP !! Top VSS cell and stream-water compartment index.
      DOUBLE PRECISION, INTENT(IN) :: CCPBD !! Old deep-bed concentration.
      DOUBLE PRECISION, INTENT(IN) :: CCPBS !! Old bed-surface concentration.
      DOUBLE PRECISION, INTENT(IN) :: CCPSF !! Old stream-water concentration.
      DOUBLE PRECISION, INTENT(IN) :: TSE !! Dimensionless contaminant timestep.
      DOUBLE PRECISION, INTENT(OUT) :: CCPBD1 !! Updated deep-bed concentration.
      DOUBLE PRECISION, INTENT(OUT) :: CCPBS1 !! Updated bed-surface concentration.
      DOUBLE PRECISION, INTENT(OUT) :: CCPSF1 !! Updated stream-water concentration.

      ! Locals
      INTEGER :: NC, NK, NJDA
      DOUBLE PRECISION :: DUMA1, DUMA2, DUMA3, DUMA4, DUMA5, DUMA6, DUMA7
      DOUBLE PRECISION :: DUMP5, DUMP6, DUMB1, DUMB2, DUMB3, DUMB3A, DUMB3B
      DOUBLE PRECISION :: DUMP1, DUMP2, DUMP3, DUMP4, DUMP7, DSUM, SUM
      DOUBLE PRECISION :: SUM1, SUM2, SUM3, SUM4, SUM5
      DOUBLE PRECISION :: ALT, ALTSTR, BLT, BLTSTR, DLT, DLTSTR, ELT, ELTSTR, ELTDA, DLTDA
      DOUBLE PRECISION :: DUMF1, FLT, FLTSTR, FLTDA, HLT, HLTSTR, HLTDA
      DOUBLE PRECISION :: PLT, DUMQ1, QLT, QLTDA, SLT, SLTDA
      DOUBLE PRECISION :: GYLT, GYLTDA, GYLTSR, WMESF, WMEBS, WMEBD

      !----------------------------------------------------------------------*

      DUMA1 = (MAX(ZERO, -PCSFP1) + MAX(ZERO, -PCSFM1))/KS
      DUMA2 = FCPSF + TSE*FCPSFT + FCPSFC*CCPSF
      DUMA3 = MAX(ZERO, WCPBD1)
      DUMA4 = FCPSD + TSE*FCPSDT + FCPSDC*CCPSF

      SUM3 = ZERO
      SUM4 = ZERO
      SUM5 = ZERO

      bank_loop: DO NK = 1, 2
         SUM1 = ZERO
         SUM2 = ZERO

         cell_loop: DO NC = NCEBK(NK), NCETOP
            SUM1 = SUM1 + MAX(ZERO, -PCPBK1(NK, NC))
            SUM2 = SUM2 + MAX(ZERO, PCPBK1(NK, NC))*CCPBK(NK, NC)
            SUM3 = SUM3 + (FCPBK(NK, NC)*CCPBK(NK, NC) + GCPBK(NK, NC)*SCPBK(NK, NC))*KSPBK(NK, NC)
         END DO cell_loop

         SUM4 = SUM4 + SUM1
         SUM5 = SUM5 + SUM2
      END DO bank_loop

      DUMA5 = SUM4
      DUMP5 = VCPBK1*SUM3
      DUMP6 = SUM5
      DUMA6 = MAX(ZERO, -PCPSW1(1)) + MAX(ZERO, -PCPSW1(2))
      DUMA7 = MAX(ZERO, -PCPSB1(1)) + MAX(ZERO, -PCPSB1(2))

      ALT = ACPSF1*(ONE + TSE*(DUMA1 + GCPLAL))*DUMA2 + TSE* &
            (DUMA3*ACPBD1*DUMA4 + ICSBSC + ECPSFC) + TSE*(DUMA5 + DUMA6*DUMA2 + DUMA7)

      ALTSTR = TSE*((ACPSF1*(ONE + TSE*(DUMA1 + GCPLAL)) + TSE*DUMA6)* &
                    FCPSFC + TSE*DUMA3*ACPBD1*FCPSDC)
      ! SET a AND a*

      DUMB1 = MAX(ZERO, -WCPBD1)
      DUMB2 = FCPBS + TSE*FCPBST + FCPBSC*CCPBS
      DUMB3A = MAX(ZERO, PCPSB1(1))
      DUMB3B = MAX(ZERO, PCPSB1(2))
      DUMB3 = DUMB3A + DUMB3B

      BLT = -TSE*(DUMB1*ACPBD1*DUMB2 + DUMB3)
      BLTSTR = -TSE*TSE*DUMB1*ACPBD1*FCPBSC
      ! SET b AND b*

      DLT = -TSE*(DUMA3*ACPBD1*DUMA4 + ICSBSC + DUMA7)
      DLTSTR = -TSE*TSE*DUMA3*ACPBD1*FCPSDC
      DLTDA = -TSE*USCP*ACSBS1
      ! SET d, d*, AND d'

      ELT = (ACPBS + TSE*(GCPLAL*ACPBS + (DUMA3 + DUMB1)*ACPBD1))* &
            DUMB2 + TSE*(ICSBDC + ECPBSC + DUMA7 + DUMB3)
      ELTSTR = TSE*(ACPBS + TSE*(GCPLAL*ACPBS + (DUMA3 + DUMB1)*ACPBD1))*FCPBSC
      ELTDA = TSE*(USCP*ACSBS1 + ACSBD1)
      ! SET e, e*, AND e'

      DUMF1 = FCPBD + TSE*FCPBDT + FCPBDC*CCPBD
      FLT = -TSE*(DUMB1*ACPBD1*DUMF1 + DUMB3)
      FLTSTR = -TSE*TSE*DUMB1*ACPBD1*FCPBDC
      FLTDA = -TSE*ACSBD1
      ! SET f, f*, AND f'

      HLT = -TSE*(DUMA3*ACPBD1*DUMB2 + DUMA7 + ICSBDC)
      HLTSTR = -TSE*TSE*DUMA3*ACPBD1*FCPBSC
      HLTDA = -TSE*ACSBD1
      ! SET h, h*, AND h'

      DUMP1 = (FCPSF + TSE*FCPSFT)*CCPSF
      DUMP2 = (FCPSD + TSE*FCPSDT)*CCPSF
      DUMP3 = (FCPBS + TSE*FCPBST)*CCPBS
      DSUM = ZERO
      SUM = ZERO
      SUM1 = ZERO

      IF (PCSFM1 > ZERO) THEN
         upstream_loop: DO NJDA = 1, 3
            SUM = SUM + ACSFA1(NJDA)*MAX(ZERO, PCSFA1(NJDA))*FCSFA1(NJDA)*CCSFA1(NJDA)
            SUM1 = SUM1 + ACSFA1(NJDA)*PCSFA1(NJDA)
         END DO upstream_loop
         IF (NOTZERO(SUM1)) DSUM = ACPSF1*PCSFM1*SUM/SUM1
      END IF

      SUM = ZERO
      SUM1 = ZERO

      IF (PCSFP1 > ZERO) THEN
         downstream_loop: DO NJDA = 4, 6
            SUM = SUM + ACSFA1(NJDA)*MAX(ZERO, PCSFA1(NJDA))*FCSFA1(NJDA)*CCSFA1(NJDA)
            SUM1 = SUM1 + ACSFA1(NJDA)*PCSFA1(NJDA)
         END DO downstream_loop
         IF (NOTZERO(SUM1)) DSUM = DSUM + ACPSF1*PCSFP1*SUM/SUM1
      END IF

      DUMP4 = DSUM/KS
      SUM = ZERO

      bank_sum_loop: DO NK = 1, 2
         SUM = SUM + MAX(ZERO, PCPSW1(NK))*FCPSW1(NK)*CCPGS1(NK)
      END DO bank_sum_loop
      DUMP7 = SUM

      PLT = -(ACPSF1*FCPSFT + ACPSFT*FCPSF)*CCPSF - ACPSF1*(DUMA1 + GCPLAL)*DUMP1 + &
            DUMP4 + DUMP5 - DUMA3*ACPBD1*DUMP2 + DUMB1*ACPBD1*DUMP3 - USCP*(QCP1 + ICP1) - &
            ICPSBS - TSE*ICSBST + GCPLAQ*ACPSF1*FCSF1Q*CCSF1Q - ECPSF - TSE*ECPSFT + &
            DUMP6 - DUMA5*CCPSF + DUMP7 - DUMA6*DUMP1 + DUMB3*CCPBS - DUMA7*CCPSF
      ! SET p

      DUMQ1 = (FCPBD + TSE*FCPBDT)*CCPBD
      QLT = -ACPBS*FCPBST*CCPBS - (GCPLAL*ACPBS + (DUMA3 + DUMB1)*ACPBD1)*DUMP3 + &
            DUMA3*ACPBD1*DUMP2 + DUMB1*ACPBD1*DUMQ1 - (ONE - USCP)*(QCP1 + ICP1) + &
            ICPSBS - ICPSBD - ECPBS + TSE*(ICSBST - ICSBDT - ECPBST) + GCPLAQ*ACPBS* &
            FCBS1Q*CCBS1Q + DUMB3*(CCPBD - CCPBS) - DUMA7*(CCPBS - CCPSF)
      QLTDA = USCP*ACSBS1*(CCPSF - CCPBS) - ACSBD1*(CCPBS - CCPBD)
      ! SET q AND q'

      SLT = -(ACPBD1*FCPBDT + ACPBDT*FCPBD)*CCPBD - ((GCPLAL + DUMB1)*DUMQ1 - DUMA3*DUMP3)* &
            ACPBD1 + ICPSBD - ECPBD + TSE*(ICSBDT - ECPBDT) + GCPLAQ*ACPBD1*FCBD1Q*CCBD1Q + &
            DUMB3A*(CCPBK(1, 1) - CCPBD) + DUMB3B*(CCPBK(2, 1) - CCPBD) - DUMA7*(CCPBD - CCPBS)
      SLTDA = ACSBD1*(CCPBS - CCPBD)
      ! SET s AND s'

      GYLT = ACPBD1*(ONE + TSE*(GCPLAL + DUMB1))*DUMF1 + TSE*(ECPBDC + DUMA7 + DUMB3)
      GYLTSR = TSE*ACPBD1*(ONE + TSE*(GCPLAL + DUMB1))*FCPBDC
      GYLTDA = TSE*ACSBD1
      ! SET y, y*, AND Y'

      IF (USCP < HALF) THEN
         ! SPECIAL CASE: NO WATER IN LINK
         CALL SNL3(ONE, ZERO, ZERO, ZERO, ZERO, -DLT - DLTDA, -DLTSTR, &
                   ELT + ELTDA, ELTSTR, -FLT - FLTDA, -FLTSTR, -HLT - HLTDA, -HLTSTR, &
                   ZERO, QLT + QLTDA, SLT + SLTDA, WMESF, WMEBS, WMEBD, GYLT + GYLTDA, GYLTSR)
      ELSE
         CALL SNL3(ALT - DLTDA, ALTSTR, -BLT + ELTDA + HLTDA, -BLTSTR, ZERO, &
                   -DLT - DLTDA, -DLTSTR, ELT + ELTDA, ELTSTR, -FLT - FLTDA, -FLTSTR, &
                   -HLT - HLTDA, -HLTSTR, PLT - QLTDA - SLTDA, QLT + QLTDA, SLT + SLTDA, &
                   WMESF, WMEBS, WMEBD, GYLT + GYLTDA, GYLTSR)
      END IF

      ! SOLVE THE DIFFERENCE EQUATIONS AND UPDATE THE CONCENTRATIONS
      CCPBD1 = CCPBD + TSE*WMEBD
      CCPBS1 = CCPBS + TSE*WMEBS
      CCPSF1 = CCPSF + TSE*WMESF

   END SUBROUTINE LINK

!> @brief Solves the three-variable nonlinear link system by fixed-point iteration.
!>
!> The equations and unknown rates are
!>
!> \[
!> (A+ASX_1)X_1-(B+BSX_2)X_2-CX_3=P,
!> \]
!> \[
!> -(D+DSX_1)X_1+(E+ESX_2)X_2-(F+FSX_3)X_3=Q,
!> \]
!> \[
!> -(H+HSX_2)X_2+(AY+AYSX_3)X_3=S.
!> \]
!>
!> Starting from zero, the routine applies exactly 100 Gauss--Seidel-style
!> fixed-point updates. It checks legacy lower bounds for the nonlinear
!> convergence region, then performs three more updates and compares their
!> cumulative change with the state after iteration 100. Finally it substitutes
!> the solution into all three equations and sums their normalised residuals.
!>
!> | Diagnostic | Trigger and effect |
!> |:-----------|:-------------------|
!> | Error 1 | A solution lies below a computed convergence-region bound; text is printed. |
!> | Error 2 | A post-solve update changes the combined state by more than `1.0D-2`; text is printed. |
!> | Error 3 | The normalised residual sum is at least `1.0D-2`; messages 1--9
!> are printed and occurrence 10 announces suppression. |
!>
!> @warning Despite the word `FATAL` in diagnostics 1 and 2, this routine does
!> not call `STOP` or `ERROR`. It also has no denominator guards and returns the
!> last iterate even after any diagnostic. The saved error-3 counter makes calls
!> stateful and non-thread-safe.
!> @endwarning
   SUBROUTINE SNL3(A, AS, B, BS, C, D, DS, E, ES, F, FS, H, HS, P, &
                   Q, S, X1, X2, X3, AY, AYS)

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: &
         A, AS, B, BS, C !! Linear/nonlinear coefficients of the first equation.
      DOUBLE PRECISION, INTENT(IN) :: &
         D, DS, E, ES, F, FS !! Linear/nonlinear coefficients of the second equation.
      DOUBLE PRECISION, INTENT(IN) :: &
         H, HS, AY, AYS !! Linear/nonlinear coefficients of the third equation.
      DOUBLE PRECISION, INTENT(IN) :: P, Q, S !! Right-hand sides of equations one, two, and three.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: X1, X2, X3 !! Returned stream, bed-surface, and deep-bed concentration rates.

      ! Locals
      INTEGER :: NJ, NJTEST
      DOUBLE PRECISION :: X1MIN, X2MIN, X3MIN, X1OLD, X2OLD, X3OLD
      DOUBLE PRECISION :: XREF, PERR, QERR, SERR

      INTEGER, SAVE :: COUNT = 0 !! Persistent count of residual failures used to suppress repeated messages.

      !----------------------------------------------------------------------*

      X1 = zero
      X2 = zero
      X3 = zero

      ! Find roots using fixed iteration
      iteration_loop: DO NJ = 1, 100
         X1 = (P + (B + BS*X2)*X2 + C*X3)/(A + AS*X1)
         X2 = (Q + (D + DS*X1)*X1 + (F + FS*X3)*X3)/(E + ES*X2)
         X3 = (S + (H + HS*X2)*X2)/(AY + AYS*X3)
      END DO iteration_loop

      ! CHECK SOLUTION IS WITHIN THE CONVERGENCE REGION
      IF (ISZERO(AS)) THEN
         X1MIN = X1
      ELSE
         X1MIN = (-A + ABS(B + two*BS*X2) + C)/(two*AS)
      END IF

      IF (ISZERO(ES)) THEN
         X2MIN = X2
      ELSE
         X2MIN = (-E + ABS(D + two*DS*X1) + ABS(F + two*FS*X3))/(two*ES)
      END IF

      IF (ISZERO(AYS)) THEN
         X3MIN = X3
      ELSE
         X3MIN = (-AY + ABS(H + two*HS*X2))/(two*AYS)
      END IF

      IF ((X1 < X1MIN) .OR. (X2 < X2MIN) .OR. (X3 < X3MIN)) THEN
         PRINT '(A40)', ' LINK: FATAL CONVERGENCE ERROR 1 IN SNL3'
         PRINT '(A33)', '       ^^^^^^^^^^^^^^^^^^^^^^^^^'
      END IF

      ! RUN THREE FURTHER ITERATION STEPS TO SEE IF THE SOLUTION IS STABLE
      X1OLD = X1
      X2OLD = X2
      X3OLD = X3

      stability_loop: DO NJTEST = 1, 3
         X1 = (P + (B + BS*X2)*X2 + C*X3)/(A + AS*X1)
         X2 = (Q + (D + DS*X1)*X1 + (F + FS*X3)*X3)/(E + ES*X2)
         X3 = (S + (H + HS*X2)*X2)/(AY + AYS*X3)

         XREF = ABS(X1) + ABS(X2) + ABS(X3)

         IF (NOTZERO(XREF)) THEN
            IF ((ABS(X1 - X1OLD) + ABS(X2 - X2OLD) + ABS(X3 - X3OLD))/XREF > 1.0D-2) THEN
               PRINT '(A40)', ' LINK: FATAL CONVERGENCE ERROR 2 IN SNL3'
               PRINT '(A33)', '       ^^^^^^^^^^^^^^^^^^^^^^^^^'
            END IF
         END IF
      END DO stability_loop

      ! CHECK THE SOLUTION IS ACCURATE (Residual calculation)
      IF (ISZERO(P)) THEN
         PERR = zero
      ELSE
         PERR = ((A + AS*X1)*X1 - (B + BS*X2)*X2 - C*X3 - P)/P
      END IF

      IF (ISZERO(Q)) THEN
         QERR = zero
      ELSE
         QERR = (-(D + DS*X1)*X1 + (E + ES*X2)*X2 - (F + FS*X3)*X3 - Q)/Q
      END IF

      IF (ISZERO(S)) THEN
         SERR = zero
      ELSE
         SERR = (-(H + HS*X2)*X2 + (AY + AYS*X3)*X3 - S)/S
      END IF

      ! Check combined fractional error
      IF ((ABS(PERR) + ABS(QERR) + ABS(SERR)) >= 1.0D-2) THEN
         COUNT = COUNT + 1
         IF (COUNT < 10) THEN
            PRINT '(A35)', ' LINK: CONVERGENCE ERROR 3 IN SNL3'
         ELSE IF (COUNT == 10) THEN
            PRINT '(A)', ' LINK: CONVERGENCE ERROR 3 IN SNL3 - MESSAGES NOW SUPPRESSED'
         END IF
      END IF

   END SUBROUTINE SNL3

END MODULE cm_channel

