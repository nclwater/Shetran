!> summary: The overland/channel boundary records, their series, and the boundary routines.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> Which elements and faces carry an imposed flow or head boundary, what that
!> boundary currently is, and the routines that maintain it: [[OCABC]] advances
!> the boundary series to the current time, [[OCEXT]] forms the external
!> exchange, and [[OCPRI]] writes the boundary diagnostics.
!>
!> `NOCBCC` maps an element to its boundary record and `NOCBCD` holds the
!> element, face, boundary type and time-series category of each record. The
!> `*LST`/`*NXT`/`*PRV` values bracket the current model time.
!>
!> The module is separate from [[oc_state]] because the boundary series are
!> read and advanced independently of the flow solution, and separate from
!> [[oc_input]] because the solver reads them every step. `dtoc` is read by
!> [[OCABC]] and so lives in [[oc_state]], not in [[oc_driver]]. Module state
!> is public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_D; see docs/rename/proposal.md. |
!> | 2026-09-11 | SvB | - | Gained OCmod's and OCQDQMOD's boundary series and the three boundary routines. |
!> @endhistory
MODULE oc_boundaries

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, one, zero
   USE array_limits, ONLY: nelee, NOCTAB
   USE element_geometry, ONLY: total_no_elements, total_no_links
   USE grid_topology, ONLY: ICMREF, ICMRF2
   USE simulation_clock, ONLY: TIH
   USE file_units, ONLY: FID_logfile, OFB, OHB
   USE oc_state, ONLY: DQ0ST, DQIST, DQIST2, dtoc, OCNEXT, OCNOW
   USE oc_indexing, ONLY: NELIND
   USE oc_cross_sections, ONLY: XINH, XINW
   USE oc_node_solver, ONLY: gethrf, getqsa
   USE vs_state, ONLY: QBKB, QBKF
   USE float_compare, ONLY: eqmarker
   USE timeseries_input, ONLY: FINPUT, HINPUT
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal
   USE error_status, ONLY: errstat_alloc

   IMPLICIT NONE

   PUBLIC :: OCABC, OCEXT, OCPRI

   INTEGER :: NOCBCC(NELEE)       !! Overland/channel boundary-condition record number by element.
   INTEGER :: NOCBCD(NOCTAB,4)    !! OC boundary records: element, face, boundary type, and time-series category.


! Boundary-series values and the current boundary state (from OCmod, OCQDQMOD).
   DOUBLEPRECISION    :: COCBCD(5, NOCTAB) !! Real-valued overland/channel boundary-condition coefficients.
   DOUBLEPRECISION    :: HOCNOW (NOCTAB)   !! Current boundary stage/head values by boundary category.
   DOUBLEPRECISION    :: QOCF (NOCTAB)     !! Current prescribed overland/channel boundary flow values by category.
   INTEGER            :: NOCHB                  !! Number of OC head-boundary categories.
   INTEGER            :: NOCFB                  !! Number of OC flow-boundary categories.

   ! Boundary-series and diagnostic-output timing state
   DOUBLEPRECISION    :: HOCLST                 !! Previous time-varying OC head-boundary time.
   DOUBLEPRECISION    :: HOCNXT                 !! Next time-varying OC head-boundary time.
   DOUBLEPRECISION    :: QFLAST                 !! Previous time-varying OC flow-boundary time.
   DOUBLEPRECISION    :: QFNEXT                 !! Next time-varying OC flow-boundary time.
   DOUBLEPRECISION    :: HOCPRV(NOCTAB)        !! Previous head-boundary values by category.
   DOUBLEPRECISION    :: QOCFIN(NOCTAB)        !! Previous flow-boundary values by category.
   DOUBLEPRECISION    :: HOCNXV(NOCTAB)        !! Next head-boundary values by category.

CONTAINS

!> @brief Assembles one element row of the implicit OC matrix.
!>
!> Given the current element, boundary type, water level, storage area,
!> rainfall, evaporation, exchange flow, and previously calculated flow
!> derivatives, `OCABC` fills the lower, central, upper, and right-hand-side
!> coefficients used by the row-wise implicit solver in [[OCSIM]].
!>
!> The routine uses the element topology in `ICMREF`, multi-link node
!> expansion in `ICMRF2`, flow derivatives `DQ0ST`, `DQIST`, and `DQIST2`,
!> bank exchange flows `QBKB` and `QBKF`, current face flows from
!> [[oc_node_solver:getqsa]], row indices `NELIND`, and channel cross-section tables
!> `XINH`/`XINW`.
!>
!> For fixed-head boundary types `IBC=3` and `IBC=9`, the assembled row simply
!> enforces the prescribed head increment:
!>
!> \[
!> \Delta H = HNOW - H,\qquad BB_{IND}=1,\qquad FF=HNOW-H.
!> \]
!>
!> Otherwise the unknown is the water-level correction for the current
!> element and its neighbours. The current water depth is
!>
!> \[
!> H = Z - ZG.
!> \]
!>
!> For land/bank elements the storage area is `AREAE`. For channel links
!> below bankfull level, the storage width is linearly interpolated from the
!> cross-section table and multiplied by link length:
!>
!> \[
!> A_s =
!> CL\left(W_m +
!> \frac{H-H_m}{H_i-H_m}(W_i-W_m)\right),
!> \]
!>
!> where \(H_m \le H < H_i\), `W_m=XINW(link,m)`, and `W_i=XINW(link,i)`.
!> The storage term contributes
!>
!> \[
!> BB_{IND} \leftarrow -A_s/DTOC.
!> \]
!>
!> Rainfall, evaporation, bank exchange, and external exchange are assembled
!> on the right-hand side as
!>
!> \[
!> FF = -AREAE\,(P_{net}+Q_H-E_{sw}) + Q_{bank},
!> \]
!>
!> with `Q_H=QHE` only for non-link elements. For links, rainfall is
!> suppressed when `H < 1D-8`, and
!>
!> \[
!> Q_{bank}=QBKB_{1}+QBKF_{1}+QBKB_{2}+QBKF_{2}.
!> \]
!>
!> Each face flow is taken positive into the current element. For face
!> \(f\), the previously calculated linearisation is applied as
!>
!> \[
!> Q_f^{n+1} \approx Q_f + DQ0ST_f\,\Delta H_i
!>                  + DQIST_f\,\Delta H_j,
!> \]
!>
!> so the current-element coefficient and residual are updated by
!>
!> \[
!> BB_{IND} \leftarrow BB_{IND}+DQ0ST_f,\qquad
!> FF \leftarrow FF-Q_f.
!> \]
!>
!> A single adjacent element receives `DQIST` in the same row (`BB`), a later
!> row (`AA`), or an earlier row (`CC`) according to its row number. For a
!> multi-link junction, `ICMREF` contains a negative pointer to `ICMRF2`; the
!> same operation is applied to each connected link using `DQIST2`.
   SUBROUTINE OCABC(IND, IROW, IELZ, NSV, NCR, NPR, IBC, N, AREAE, &
                    ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW, AA, BB, CC, FF)

      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)          :: IND    !! Row position of the current element.
      INTEGER, INTENT(IN)          :: IROW   !! Row number of the current element.
      INTEGER, INTENT(IN)          :: IELZ   !! Current element number.
      INTEGER, INTENT(IN)          :: NSV    !! Number of elements in the next (following) row.
      INTEGER, INTENT(IN)          :: NCR    !! Number of elements in the current row.
      INTEGER, INTENT(IN)          :: NPR    !! Number of elements in the previous row.
      INTEGER, INTENT(IN)          :: IBC    !! Boundary-condition type for the current element, or 0.
      INTEGER, INTENT(IN)          :: N      !! Number of cross-section table points for the current element's link.
      DOUBLE PRECISION, INTENT(IN) :: AREAE  !! Plan storage area of the current (non-link) element.
      DOUBLE PRECISION, INTENT(IN) :: ZG     !! Ground/bed elevation of the current element.
      DOUBLE PRECISION, INTENT(IN) :: CL     !! Channel-link length, used for link storage width.
      DOUBLE PRECISION, INTENT(IN) :: ZBF    !! Bankfull elevation of the current element's link.
      DOUBLE PRECISION, INTENT(IN) :: Z      !! Current water-surface elevation.
      DOUBLE PRECISION, INTENT(IN) :: PNETT  !! Net rainfall rate onto the current element.
      DOUBLE PRECISION, INTENT(IN) :: QHE    !! Exchange flow rate for the current (non-link) element.
      DOUBLE PRECISION, INTENT(IN) :: ESWAE  !! Evaporation rate from the current element's surface water.
      DOUBLE PRECISION, INTENT(IN) :: HNOW   !! Prescribed head value for a fixed-head boundary.
      DOUBLE PRECISION, INTENT(OUT):: AA(:) !! Active next-row coefficients for elements adjacent to `IELZ`.
      DOUBLE PRECISION, INTENT(OUT):: BB(:) !! Active current-row coefficients for elements adjacent to `IELZ`.
      DOUBLE PRECISION, INTENT(OUT):: CC(:) !! Active previous-row coefficients for elements adjacent to `IELZ`.
      DOUBLE PRECISION, INTENT(OUT):: FF    !! Right-hand-side residual for the current element's row equation.

      ! Local Variables
      INTEGER                      :: I, IBR, IFACE, IM, J, JEL, JFACE, JND, JROW
      DOUBLE PRECISION             :: AR, BKDUM, DQ0, DQI, H, HI, HM, PDUM, Q
      DOUBLE PRECISION             :: QHDUM, WI, WM
      LOGICAL                      :: BLINK, TEST

      !----------------------------------------------------------------------*

      ! ----- INITIALIZE OUTPUT ARRAYS & GET WATER DEPTH
      ! Performance Rollback: Explicit DO loops bypass dope-vector overhead for micro-arrays
      IF (NSV > 0) THEN
         DO I = 1, NSV
            AA(I) = ZERO
         END DO
      END IF

      DO I = 1, NCR
         BB(I) = ZERO
      END DO

      IF (NPR > 0) THEN
         DO I = 1, NPR
            CC(I) = ZERO
         END DO
      END IF

      H = Z - ZG

      ! ----- HEAD BOUNDARY
      IF (IBC == 3 .OR. IBC == 9) THEN
         BB(IND) = ONE
         FF = HNOW - H
         RETURN
      END IF

      ! ----- IS THE CURRENT ELEMENT A LINK?
      BLINK = (ICMREF(IELZ, 1) == 3)

      ! ----- PUT STORAGE TERM INTO CENTRAL COEFFICIENT FOR CURRENT ELEMENT
      TEST = BLINK
      IF (TEST) TEST = (Z < ZBF)

      IF (TEST) THEN
         ! * note requirements: XINH(IEL,1)=0; XINH(IEL,N).GE.ZBF-ZG
         search_loop: DO I = 2, N
            HI = XINH(IELZ, I)
            IF (H < HI) THEN
               IM = I - 1
               HM = XINH(IELZ, IM)
               WM = XINW(IELZ, IM)
               WI = XINW(IELZ, I)
               AR = CL*(WM + (WI - WM)*((H - HM)/(HI - HM)))
               EXIT search_loop
            END IF
         END DO search_loop
      ELSE
         AR = AREAE
      END IF

      BB(IND) = -AR/DTOC

      ! ----- PUT PRECIPITATION, EVAPORATION AND EXCHANGE FLOWS INTO RHS
      PDUM = PNETT
      IF (BLINK) THEN
         IF (H < 1.0D-8) PDUM = ZERO
         BKDUM = QBKB(IELZ, 1) + QBKF(IELZ, 1) + QBKB(IELZ, 2) + QBKF(IELZ, 2)
         QHDUM = ZERO
      ELSE
         BKDUM = ZERO
         QHDUM = QHE
      END IF

      FF = -AREAE*(PDUM + QHDUM - ESWAE) + BKDUM

      ! ----- LOOP OVER ADJACENT ELEMENTS
      face_loop: DO IFACE = 1, 4
         JEL = ICMREF(IELZ, IFACE + 4)
         JFACE = ICMREF(IELZ, IFACE + 8)

         ! --- GET FLOW AND DERIVATIVE (+VE INTO ELEMENT)
         Q = GETQSA(ielz, IFACE)
         DQ0 = DQ0ST(IELZ, IFACE)

         ! --- ADD INTO COEFFICIENTS FOR CURRENT ELEMENT
         BB(IND) = BB(IND) + DQ0
         FF = FF - Q

         ! --- TEST FOR SINGLE ADJACENT ELEMENT
         IF (JEL > 0) THEN
            JROW = ICMREF(JEL, 3)
            JND = NELIND(JEL)
            DQI = DQIST(IELZ, IFACE)

            !        ADD DERIVATIVE TO COEFFICIENT FOR ADJACENT ELEMENT
            IF (JROW == IROW) BB(JND) = BB(JND) + DQI
            IF (JROW > IROW) AA(JND) = AA(JND) + DQI
            IF (JROW < IROW) CC(JND) = CC(JND) + DQI

            ! --- SIMILARLY FOR MULTIPLE ADJACENT LINKS
         ELSE IF (JEL < 0) THEN
            IBR = -JEL
            DO J = 1, 3
               JEL = ICMRF2(IBR, J)
               IF (JEL > 0) THEN
                  JROW = ICMREF(JEL, 3)
                  JND = NELIND(JEL)
                  DQI = DQIST2(IBR, J)

                  IF (JROW == IROW) BB(JND) = BB(JND) + DQI
                  IF (JROW > IROW) AA(JND) = AA(JND) + DQI
                  IF (JROW < IROW) CC(JND) = CC(JND) + DQI
               END IF
            END DO
         END IF
      END DO face_loop

   END SUBROUTINE OCABC

!> @brief Reads time-varying head and flux boundary values for the current OC step.
!>
!> Boundary time series are advanced from the head-boundary and flux-boundary
!> files into `HOCNOW` and `QOCF`; end-of-file markers are treated as fatal
!> input errors.
!>
!> `HINPUT` interpolates or advances head values for `NOCHB` categories using
!> `TIH`, `OCNOW`, and `OCNEXT`. `FINPUT` does the same for `NOCFB` flux
!> categories. The resulting `QOCF` values are prescribed inflow rates
!> consumed by [[oc_discharge:OCQBC]].
!>
!> @warning
!> [[OCINI]] does not explicitly initialise `HOCLST`, `HOCNXT`, `QFLAST`,
!> `QFNEXT`, `HOCPRV`, `HOCNXV`, or `QOCFIN` before the first call to this
!> routine, so `HINPUT`/`FINPUT` receive processor-dependent initial state on
!> that call. This documents current behaviour; it was not repaired in this
!> transfer.
!> @endwarning
   SUBROUTINE OCEXT

      IMPLICIT NONE

      !----------------------------------------------------------------------*

      ! --- HEAD BOUNDARY ---
      IF (NOCHB > 0) THEN
         CALL HINPUT(OHB, TIH, OCNOW, OCNEXT, HOCLST, HOCNXT, &
                     HOCPRV(1:NOCHB), HOCNXV(1:NOCHB), NOCHB, HOCNOW(1:NOCHB))
      END IF

      IF (EQMARKER(HOCNXT)) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 1007, FID_logfile, 0, 0, 'END OF OC HEAD BOUNDARY DATA')
      END IF

      ! --- FLUX BOUNDARY ---
      IF (NOCFB > 0) THEN
         CALL FINPUT(OFB, TIH, OCNOW, OCNEXT, QFLAST, QFNEXT, &
                     QOCFIN(1:NOCFB), NOCFB, QOCF(1:NOCFB))
      END IF

      IF (EQMARKER(QFNEXT)) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 1023, FID_logfile, 0, 0, 'END OF OC FLUX BOUNDARY DATA')
      END IF

   END SUBROUTINE OCEXT

!> @brief Prints one OC diagnostic block to the main print file.
!>
!> The report is written only when [[OCSIM]] calls this routine for an
!> output time in the requested interval. It lists each element, the four
!> `QOC` face flows after conversion to the model x/y sign convention, and
!> the current water level `HRF`.
!>
!> | Element range | Extra field |
!> |:--------------|:------------|
!> | `1:total_no_links` | `ARXL`, the current channel wetted area. |
!> | `total_no_links+1:total_no_elements` | No channel area is printed. |
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NELEE >= total_no_elements` and `total_no_elements >= max(total_no_links,1)` | Element arrays cover active elements and links. |
!> | `total_no_links >= 0` and `total_no_links <= size(ARXL)` | Channel area values are available for printed links. |
!> | `PRI` open for formatted output | The report can be written. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-26 | RAH | 4.2 | Created this routine. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE OCPRI(OCNOW, ARXL, QOC)
      DOUBLEPRECISION, INTENT(IN) :: OCNOW      !! Simulation time being reported, in hours.
      DOUBLEPRECISION, INTENT(IN) :: ARXL(:)    !! Current channel wetted cross-sectional area, by link.
      DOUBLEPRECISION, INTENT(IN) :: QOC(NELEE, 4) !! Current face flows in the model x/y sign convention.
      DOUBLEPRECISION, ALLOCATABLE :: ghrf(:)   !! Local copy of the current water level, by link.
      INTEGER                     :: FACE, ielmm

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.

!----------------------------------------------------------------------*
      ALLOCATE (ghrf(total_no_links), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "ghrf", "OCmod:OCPRI", emsg)

      WRITE (FID_logfile, 9100) 'AFTER', OCNOW, ' HOURS ----'
      WRITE (FID_logfile, 9200) 'iel', ('QOC(iel,', FACE, ')', FACE=1, 4), 'HRF', 'ARXL'
      DO ielmm = 1, total_no_links
         ghrf(ielmm) = GETHRF(ielmm)
      END DO
      WRITE (FID_logfile, 9210) (ielmm, (QOC(ielmm, FACE), FACE=1, 4), ghrf(ielmm), ARXL(ielmm), ielmm=1, total_no_links)
      DO ielmm = total_no_links + 1, total_no_elements
         WRITE (FID_logfile, 9210) ielmm, (QOC(ielmm, FACE), FACE=1, 4), GETHRF(ielmm)
      END DO

      WRITE (FID_logfile, 9100) 'END ----'
9100  FORMAT(//'---- OC MODULE  RESULTS ', A:F10.2, A//)
9200  FORMAT(4X, A4, 4(2X, A8, I1, A1), 2A12/)

9210  FORMAT(4X, I4, SP, 4F12.3, S, 2F12.3)
   END SUBROUTINE OCPRI

END MODULE oc_boundaries

