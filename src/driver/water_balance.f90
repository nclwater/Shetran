!> summary: The per-element water balance and the catchment accumulator.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[BALWAT]] forms the water balance of one element or link over a timestep —
!> input less evaporation, transpiration and outflow, against the change in
!> canopy, surface and subsurface storage — and accumulates the catchment
!> totals into `BALANC`. It applies the face sign conventions of the producing
!> solvers, which is why it is the routine that reconciles them.
!>
!> `BALANC`'s active entries are:
!>
!> | Entries | Meaning |
!> |:--------|:--------|
!> | 1:6 | Precipitation, canopy evaporation, soil/surface evaporation, transpiration, base flow, and outlet discharge since the last report (m3). |
!> | 7:12 | Cumulative totals of entries 1:6 (m3). |
!> | 13:17 | Canopy, snow, subsurface, surface-water, and channel storage (m3). |
!> | 18:19 | Current-period and cumulative aquifer-channel exchange (m3). |
!> | 20 | Declared capacity entry; not assigned by current [[mass_balance_report:FRMB]]. |
!>
!> `MBLINK`, `MBFACE` and `MBFLAG` select the outlet link, its face and the
!> reporting schedule.
!>
!> @warning
!> No current source routine explicitly assigns `MBLINK`, `MBFACE` or
!> `MBFLAG`, although [[mass_balance_report:FRMB]] reads them every timestep.
!> Standard Fortran therefore regards these values as undefined; a compiler's
!> zero-filled static storage is not a portable initialization.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP / RAH | 2.0-4.2 | Developed the frame driver, the meteorological reader and the timestep control. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the remaining frame `.F` files to Fortran 90. |
!> | 2015-2026 | SB / SvB | 4.5-4.6 | Added the separate temperature streams, the dated meteorological reader and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of rest, AL_D; see docs/rename/proposal.md. |
!> @endhistory
MODULE water_balance

   USE MOD_PARAMETERS, ONLY: zero
   USE array_limits, ONLY: nelee
   USE element_geometry, ONLY: cellarea, top_cell_no, total_no_elements, ZGRUND
   USE grid_topology, ONLY: ICMREF
   USE channel_geometry, ONLY: CWIDTH
   USE simulation_clock, ONLY: DTUZ
   USE et_state, ONLY: EEVAP, ERUZ, PNETTO
   USE vs_state, ONLY: DELTAZ, NLYRBT, QBKF, QVSBF, QVSH, QVSWEL, VSTHE, WBERR
   USE oc_state, ONLY: ARXL, QOC
   USE oc_node_solver, ONLY: gethrf

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: BALWAT
   PUBLIC :: BALANC, MBLINK, MBFACE, MBFLAG, MBYEAR, MBMON, MBDAY

   INTEGER :: MBLINK     !! Link whose selected face supplies outlet discharge to the catchment balance.
   INTEGER :: MBFACE     !! Face of `MBLINK` used for catchment-balance discharge.
   INTEGER :: MBFLAG     !! Catchment-balance schedule: 1 daily, any other value monthly.
   INTEGER :: MBYEAR !! Calendar year of the next mass-balance report.
   INTEGER :: MBMON  !! Calendar month of the next mass-balance report.
   INTEGER :: MBDAY  !! Calendar day of the next mass-balance report.
   DOUBLEPRECISION :: BALANC(20)     !! Catchment water-volume terms described in the module table (m3).
   LOGICAL :: FIRST_balwat = .TRUE. !! `.TRUE.` until `BALWAT` has initialised `STORW_balwat` and `WBERR` on its first call.
   DOUBLEPRECISION :: STORW_balwat(NELEE) = zero !! Water storage depth for each element/link at the previous `BALWAT` call (m).

CONTAINS

!> Updates the cumulative water-balance error [[vs_state:WBERR]] for each column or link.
!>
!> The routine computes the change in stored surface/subsurface water since
!> the previous call and compares it with the net supplied depth over the
!> last timestep (precipitation, evaporation, subsurface exchange, well flow,
!> overland flow, and lateral subsurface advection). The residual is
!> accumulated in `WBERR` as a diagnostic depth in metres.
!>
!> The stored depth used by the balance is
!>
!> \[
!> S_{iel} =
!> \begin{cases}
!> ARXL_{iel}/CWIDTH_{iel}, & \text{channel links (}ICMREF(iel,1)=3\text{)},\\
!> HRF_{iel}-ZGRUND_{iel}, & \text{otherwise},
!> \end{cases}
!> + \sum_{k=NLYRBT(iel,1)}^{LL} \Delta z_{k,iel}\,\theta_{k,iel},
!> \]
!>
!> where \(\theta\) is `VSTHE` and `HRF` is read through [[oc_node_solver:gethrf]].
!> The storage change is \(\Delta S = S_{iel}-S^{old}_{iel}\), where
!> \(S^{old}\) is the previous call's `STORW_balwat`. On the first call
!> (`FIRST_balwat`) `WBERR` is initialised to zero and `STORW_balwat` is
!> primed with \(S\), but no residual is added because no previous storage
!> state is available.
!>
!> On subsequent calls the supplied rate depth before timestep conversion is
!>
!> \[
!> I_{iel} =
!> PNETTO_{iel} - EEVAP_{iel} + QVSBF_{iel} - QVSWEL_{iel}
!> - \sum_k ERUZ_{iel,k}
!> + \frac{Q_{adv}}{AREA_{iel}},
!> \]
!>
!> with channel-bank exchange \(Q_{adv} = -QBKF_{iel,1}-QBKF_{iel,2}\) for
!> channel links, and zero otherwise, before the paired face-direction terms
!> are added for \(j=1,2\):
!>
!> \[
!> Q_{adv} \leftarrow Q_{adv}
!> - QOC_{iel,j} + QOC_{iel,j+2}
!> + \sum_k \left(QVSH_{j,k,iel}+QVSH_{j+2,k,iel}\right).
!> \]
!>
!> The timestep input depth is `DEPTHI = I * DTUZ`, and the diagnostic update
!> is
!>
!> \[
!> WBERR_{iel} \leftarrow WBERR_{iel} + \Delta S - DEPTHI .
!> \]
!>
!> @note
!> This routine has no dummy arguments. It reads and updates shared grid,
!> geometry, flow, and water-level state from `SGLOBAL`, `AL_C`, `AL_D`, and
!> `AL_G`, and calls [[oc_node_solver:gethrf]] for the current surface water level.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Standard header, explicit declarations, extra comments, and first-pass storage initialisation. |
!> | 1995-02-20 | GP | 4.0 | Updated for the VSS module and revised subsurface flow variables. |
!> | 1997-02-17 | RAH | 4.1 | Swapped array subscripts for `QVSH`, `DELTAZ`, and `VSTHE`; renamed local counters. |
!> | 2026-04-05 | SvB | 4.6.1 | Replaced the `ALINIT` call with a direct `WBERR` array-slice assignment and replaced the `GOTO 400` skip on the first call with the `IF (.NOT. FIRST_balwat)` block. |
!> @endhistory
   SUBROUTINE BALWAT
      IMPLICIT NONE

      DOUBLE PRECISION :: DELSTO, DEPTHI, DEPTHS, asum, asumQ
      INTEGER          :: ITYPE, JDUM, CELL, IEL

      !----------------------------------------------------------------------*
      ! Initialization
      ! --------------

      IF (FIRST_balwat) WBERR(1:total_no_elements) = ZERO

      ! Loop Over Columns
      ! -----------------
      DO IEL = 1, total_no_elements
         ITYPE = ICMREF(IEL, 1)

         ! Calculate depth of water stored and change since previous step
         ! --------------------------------------------------------------
         ! * surface
         IF (ITYPE == 3) THEN
            asum = ARXL(IEL)/CWIDTH(IEL)
         ELSE
            asum = GETHRF(IEL) - ZGRUND(IEL)
         END IF

         ! * sub-surface
         DO CELL = NLYRBT(IEL, 1), top_cell_no
            asum = asum + DELTAZ(CELL, IEL)*VSTHE(CELL, IEL)
         END DO

         DEPTHS = asum

         ! * net increase this timestep
         DELSTO = DEPTHS - STORW_balwat(IEL)

         ! * save new value for use next timestep
         STORW_balwat(IEL) = DEPTHS

         ! Calculate net depth of water supplied over the previous step
         ! ------------------------------------------------------------
         ! * ... but only if we have a bona fide value for DELSTO

         IF (.NOT. FIRST_balwat) THEN

            ! * sources and sinks
            asum = PNETTO(IEL) - EEVAP(IEL) + QVSBF(IEL) - QVSWEL(IEL)
            DO CELL = NLYRBT(IEL, 1), top_cell_no
               asum = asum - ERUZ(IEL, CELL)
            END DO

            ! * advection
            IF (ITYPE == 3) THEN
               asumQ = -QBKF(IEL, 1) - QBKF(IEL, 2)
            ELSE
               asumQ = ZERO
            END IF

            DO JDUM = 1, 2
               asumQ = asumQ - QOC(IEL, JDUM) + QOC(IEL, JDUM + 2)
               DO CELL = NLYRBT(IEL, 1), top_cell_no
                  asumQ = asumQ + QVSH(JDUM, CELL, IEL) + QVSH(JDUM + 2, CELL, IEL)
               END DO
            END DO

            asum = asum + asumQ/cellarea(IEL)

            ! * convert from rate to depth
            DEPTHI = asum*DTUZ

            ! Update the cumulative water balance error as a depth
            ! ----------------------------------------------------
            WBERR(IEL) = WBERR(IEL) + DELSTO - DEPTHI

         END IF

      END DO

      ! Epilogue
      ! --------
      FIRST_balwat = .FALSE.

   END SUBROUTINE BALWAT

END MODULE water_balance

