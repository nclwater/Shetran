!> summary: Column iteration, the boundary update and the subsurface mass balance.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[VSSIM]] is the component's per-timestep routine: it iterates the active
!> element columns in `ISORT` order, calling
!> [[vs_column_solver:VSCOLM]] for each, until the pressure-head changes
!> converge or the iteration limit is reached. [[VSPREP]] advances the
!> time-varying boundary series first; [[VSMB]] forms the subsurface mass
!> balance afterwards. `initialise_vsmod` allocates the component's state.
!>
!> Each active element is treated as a one-dimensional vertical column, with
!> lateral coupling through the layer and cell connectivity arrays built by
!> [[vs_connectivity]]. The solved state — `VSPSI`, `VSTHE`, `VSKR` and the
!> fluxes `QVSV`, `QVSH`, `QVSBF`, `QVSSPR`, `QVSWEL`, `QBKB`, `QBKF`, `QBKI`
!> — is returned to [[vs_state]], where the rest of the model reads it.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995--1998 | GP / RAH | 4.0--4.2 | Created the VSS component and its `.INC` include groups. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS Fortran sources into a single Fortran 90 module. |
!> | 2026-03 to 2026-05 | SB / SvB | 4.6 | Modernisation pass, and moved `VSREAD`'s read buffers to allocatable module state to avoid a stack-related crash. |
!> | 2026-09-10 | SvB | - | Split out of VSmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_driver

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, half, one, zero
   USE array_limits, ONLY: LLEE, nelee, NLYREE, NSEE
   USE element_geometry, ONLY: cellarea, DHF, ISORT, NBFACE, top_cell_no, &
                              total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF
   USE channel_geometry, ONLY: BEXBK, LINKNS, NHBED
   USE simulation_clock, ONLY: DTUZ, TIH, UZNEXT, UZNOW
   USE file_units, ONLY: BFB, BHB, FID_logfile, LFB, LGB, LHB, WLD
   USE et_state, ONLY: EEVAP, ERUZ, ESOILA, NRD, NVC, PNETTO
   USE vs_state, ONLY: DELTAZ, ICSOILsv, JCBCsv, JVSACN, JVSDEL, NLYR, NLYRBT, NTSOIL, &
                       NVSSPC, NVSWLI, NWELBT, NWELTP, QBKB, QBKF, QBKI, QH, QVSBF, QVSH, &
                       QVSSPR, QVSV, QVSWEL, QVSWLI, VSAIJsv, VSPSI, VSTHE, ZVSNOD, ZVSPSL
   USE vs_config, ONLY: BHELEV, IVSSTO, NBBCAT, NBBTYP, NLBCAT, NLBTYP, NVSBF, NVSBH, &
                        NVSLF, NVSLFL, NVSLFN, NVSLFT, NVSLG, NVSLGL, NVSLGN, NVSLGT, &
                        NVSLH, NVSLHL, NVSLHN, NVSLHT, NVSWL, NVSWLC, RBFNOW, RBHNOW, &
                        RLFNOW, RLGNOW, RLHNOW, VSK3D, VSKR, VSSPCO, VSSPZ, VSWL, VSWV, WLNOW
   USE vs_boundaries, ONLY: FIRSTvssim, RBFLST, RBFPRV, RBFTIM, RBHLST, RBHNXT, RBHPRV, &
                            RBHTIM, RLFDUM, RLFLST, RLFPRV, RLFTIM, RLGDUM, RLGLST, RLGNXT, &
                            RLGPRV, RLGTIM, RLHDUM, RLHLST, RLHNXT, RLHPRV, RLHTIM, RWELIN, &
                            WLLAST, WLTIME
   USE vs_column_solver, ONLY: errcntallowed, VSCOLM
   USE float_compare, ONLY: eqmarker, notzero
   USE timeseries_input, ONLY: FINPUT, HINPUT
   USE linear_algebra, ONLY: dcopy
   USE error_reporting, ONLY: RAISE_ERROR, ERR_STOP, ERRLVL_fatal, ERRLVL_error
   USE error_status, ONLY: errstat_alloc
   USE OCmod2, ONLY: GETHRF

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: VSSIM, INITIALISE_VSMOD

CONTAINS

!> Allocates run-size VSS work arrays.
!>
!> `vsaijsv` stores lateral inter-cell conductance terms by face, cell, and
!> element, while `vskr` stores relative hydraulic conductivity by cell and
!> element. Both depend on mesh dimensions read earlier in the setup sequence.
!>
!> @note
!> This routine allocates, but does not initialise, the arrays and does not
!> guard against repeated allocation. It should therefore be called once after
!> `top_cell_no` and `total_no_elements` have been established.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
   SUBROUTINE initialise_vsmod()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VSmod:initialise_vsmod"

      ALLOCATE (vsaijsv(4, top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "vsaijsv", location, emsg)
      ALLOCATE (vskr(top_cell_no, total_no_elements), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "vskr", location, emsg)
   END SUBROUTINE initialise_vsmod

!> Applies a post-solve mass-balance correction to VSS flux arrays.
!>
!> `VSMB` adjusts reported lateral VSS fluxes after [[vssim]] has solved the
!> pressure-head and water-content fields. It uses the previous water contents
!> `VSTHEN`, current `VSTHE`, vertical fluxes `QVSV`, lateral fluxes `QVSH`,
!> root extraction `ERUZ`, soil evaporation `ESOILA`, well fluxes `QVSWLI`,
!> cell volumes (`AREA*DELTAZ`), and timestep `DTUZ` to reduce residual
!> cell-scale mass-balance error.
!>
!> The correction is applied only to selected lateral faces:
!>
!> | Element type from `ICMREF(iel,1)` | Faces adjusted |
!> |:----------------------------------|:---------------|
!> | grid (`0`) | none |
!> | bank (`1` or `2`) | the outer face adjacent to a grid element, if present |
!> | link/other | the two bank-facing side faces, selected using `LINKNS` |
!>
!> For each adjusted cell, the residual volume rate is assembled as
!>
!> \[
!>   E =
!>   AREA\left[
!>     -QVSV_{c-1}+QVSV_c+ERUZ_c
!>     + {\Delta z_c(VSTHE_c-VSTHEN_c)\over DTUZ}
!>     + QVSWLI_c + ESOILA_{top}
!>   \right]
!>   - \sum_{f=1}^4 QVSH_{f,c}.
!> \]
!>
!> Well flux is included only when `NVSWLI(iel)>0`, and `ESOILA` is included
!> only for the top cell. If the sum of the selected adjustable lateral fluxes
!> is non-zero,
!>
!> \[
!>   QVSH_{f,c} \leftarrow QVSH_{f,c}
!>   \left(1 + {E\over\sum_{adjusted}QVSH_{f,c}}\right)
!> \]
!>
!> for each selected face. The corrected flux is then copied to the adjacent
!> element with opposite sign using `JVSACN`/`ICMREF`, so paired cells report
!> equal and opposite exchange.
!>
!> @warning The split-cell branch is not implemented. If `JVSDEL` indicates a
!> split-cell lateral connection, the routine stops immediately with
!> `UNFINISHED CODE FOR SPLIT CELLS IN SUBROUTINE VSMB`.
!> @endwarning
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995-03-08 | GP | 4.0 | Written; version 4.0 completed 1996-07-17. |
!> | 1996-12-28 | RAH | 4.1 | Removed the variable `ILINK` and the leading comments. |
!> | 1997-01-18 | RAH | 4.1 | Swapped the `JVSACN`, `QVSV`, `QVSWLI`, and `VSTHE` subscripts, fixing an error in the `QVSWLI` index (use `IW`, not `IEL`); removed temporary code that set `VSSTMP`; made locals `DBLE`; stopped including `VSCOM1.INC`. |
!> | 1997-02-14 | RAH | 4.1 | Reversed the `DELTAZ` and `QVSH` indices; declared `JCL` and `JFA`; moved `VSTHEN` from `VSCOM1.INC` into the argument list, reversing its subscripts. |
!> | 1997-05-09 | RAH | 4.1 | Scrapped the output `QVSBF` (now set in [[vssim]]); put labels in order; removed the redundant local `BDONE`; added a trap for non-zero `JVSDEL`. |
!> | 2026-04-06/07 | SvB | 4.6 | Replaced the `GOTO`-driven `iscycle` deferred-stop flag with an immediate `STOP` at the point the split-cell condition is detected. Both versions terminate the run on the same condition; the current version does so without first finishing the remaining bookkeeping for the current/later elements. |
!> @endhistory
   SUBROUTINE VSMB(VSTHEN)

      ! Assumed external module dependencies providing global variables:
      ! LLEE, total_no_elements, ICMREF, LINKNS, NVSWLI, cellarea, top_cell_no,
      ! NLYRBT, QVSV, ERUZ, DELTAZ, VSTHE, DTUZ, QVSWLI, ESOILA, QVSH, zero,
      ! one, NOTZERO, JVSDEL, JVSACN

      IMPLICIT NONE

      ! Input arguments
      DOUBLE PRECISION, INTENT(IN) :: VSTHEN(LLEE, total_no_elements) !! Previous-timestep water content by cell and element.

      ! Locals
      INTEGER :: NFACES, IFACES(4)
      INTEGER :: IEL, J, ITYPE, IFA, JEL, ICL, JFA, JCL, IW, MCL
      DOUBLE PRECISION :: AREAE, CMBE, F, Qasum

      !----------------------------------------------------------------------*

      ! --- loop over all elements
      element_loop: DO IEL = 1, total_no_elements

         ITYPE = ICMREF(IEL, 1)

         ! Choose faces to adjust (ie set NFACES and IFACES)
         IF (ITYPE == 0) THEN
            ! grids - do nothing!
            NFACES = 0

         ELSE IF (ITYPE == 1 .OR. ITYPE == 2) THEN
            ! banks - update only 'outer' face adjacent to grid (if there is one)
            NFACES = 0

            search_faces: DO IFA = 1, 4
               JEL = ICMREF(IEL, IFA + 4)
               IF (JEL > 0) THEN
                  IF (ICMREF(JEL, 1) == 0) THEN
                     IFACES(1) = IFA
                     NFACES = 1
                     EXIT search_faces  ! Cleanly replaces the iscycle hack and GOTO 930
                  END IF
               END IF
            END DO search_faces

         ELSE
            ! links - update faces adjacent to banks only
            NFACES = 2
            IF (LINKNS(IEL)) THEN
               IFACES(1) = 1
               IFACES(2) = 3
            ELSE
               IFACES(1) = 2
               IFACES(2) = 4
            END IF
         END IF

         ! Loop over column cells if required (top to bottom for QVSV's benefit)
         IF (NFACES > 0) THEN
            IW = NVSWLI(IEL)
            AREAE = cellarea(IEL)

            cell_balance_loop: DO ICL = top_cell_no, NLYRBT(IEL, 1), -1
               ! calculate mass balance error (m**3/s)
               MCL = ICL - 1
               CMBE = -QVSV(MCL, IEL) + QVSV(ICL, IEL) + ERUZ(IEL, ICL) + &
                      DELTAZ(ICL, IEL)*(VSTHE(ICL, IEL) - VSTHEN(ICL, IEL))/DTUZ

               IF (IW > 0) CMBE = CMBE + QVSWLI(ICL, IW)
               IF (ICL == top_cell_no) CMBE = CMBE + ESOILA(IEL)

               CMBE = CMBE*AREAE

               DO IFA = 1, 4
                  CMBE = CMBE - QVSH(IFA, ICL, IEL)
               END DO

               ! adjust lateral flows (unless Qasum=0)
               Qasum = zero
               DO J = 1, NFACES
                  IFA = IFACES(J)
                  Qasum = Qasum + QVSH(IFA, ICL, IEL)
               END DO

               IF (NOTZERO(Qasum)) THEN
                  F = one + CMBE/Qasum
                  DO J = 1, NFACES
                     IFA = IFACES(J)
                     QVSH(IFA, ICL, IEL) = QVSH(IFA, ICL, IEL)*F
                  END DO
               END IF
            END DO cell_balance_loop
         END IF

         ! Update flows for adjacent element
         adjacent_update_loop: DO IFA = 1, 4
            JEL = ICMREF(IEL, IFA + 4)

            IF (JEL > 0) THEN
               JFA = ICMREF(IEL, IFA + 8)

               layer_update_loop: DO ICL = NLYRBT(IEL, 1), top_cell_no

                  ! 970509 (catch JEL next time around)
                  ! Immediately crash if split cells are encountered (Replacing GOTO 8820)
                  IF (JVSDEL(IFA, ICL, IEL) /= 0) THEN
                     WRITE (*, '(A)') 'ERROR: Unfinished code for split cells in subroutine VSMB. '// &
                        'Please contact the developers.'
                     CALL ERR_STOP(255)
                  END IF

                  JCL = JVSACN(IFA, ICL, IEL)
                  IF (JCL > 0) QVSH(JFA, JCL, JEL) = -QVSH(IFA, ICL, IEL)

               END DO layer_update_loop
            END IF

         END DO adjacent_update_loop

      END DO element_loop

   END SUBROUTINE VSMB

!> Reads and interpolates time-varying VSS boundary-condition series.
!>
!> `VSPREP` is the timestep preparatory reader for the VSS boundary data files
!> described in the manual's time-varying boundary-condition section. Flow
!> files are processed with `FINPUT`, which returns the timestep-averaged value
!> for a piecewise-constant input series,
!> \[
!>   \bar q(t_n,t_{n+1}) =
!>   {1 \over \Delta t}\int_{t_n}^{t_{n+1}} q_b(t)\,dt ,
!> \]
!> while head files are processed with `HINPUT`, which linearly interpolates the
!> breakpoint series to the current computational time,
!> \[
!>   h(t) = h_i + {t-t_i \over t_{i+1}-t_i}\,(h_{i+1}-h_i).
!> \]
!>
!> Boundary categories and selected-layer counts are defined by [[vsread]] from
!> `VS11` and `VS16`; this routine expands the compact time-series values back
!> into the category/layer arrays used by [[vsbc]], [[vslowr]], and [[vscolm]].
!>
!> | File/unit | Data represented | Count used | Output array |
!> |:----------|:-----------------|:-----------|:-------------|
!> | `WLD` | pumping-well abstraction, m3/s | `NVSWL` | `WLNOW` |
!> | `LFB` | lateral subsurface flow, m3/s | `NVSLFT` | `RLFNOW` |
!> | `LHB` | lateral subsurface head, m above datum | `NVSLHT` | `RLHNOW` |
!> | `LGB` | lateral head-gradient boundary | `NVSLGT` | `RLGNOW` |
!> | `BFB` | bottom flow boundary, m/s | `NVSBF` | `RBFNOW` |
!> | `BHB` | bottom head boundary, m above datum | `NVSBH` | `RBHNOW` |
!>
!> If a boundary file reaches its missing/end marker before the required model
!> time, `EQMARKER` triggers a fatal `ERROR` call (`1042`-`1046` or `1052`) so
!> the solver cannot continue with stale boundary conditions.
!>
!> @note
!> Lateral head-gradient data (`LGB`/`RLGNOW`) are still read and interpolated
!> when `NVSLG > 0`, but the downstream `JCBC=5` implementation in [[vsbc]]
!> only prints an unfinished-code message and does not apply those values to the
!> matrix.
!> @endnote
!>
!> @note
!> The saved interpolation state (`WLLAST`, `WLTIME`, `RWELIN`, and similar
!> `RL*`/`RB*` arrays for each boundary category) lives in module-level storage
!> declared near the top of `VSmod`, rather than as `SAVE` locals of this
!> routine.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-29 | GP | 4.0 | Written; version 4.0 completed 1995-05-03. |
!> | 1996-12-28 | RAH | 4.1 | Removed the variables `IEL` and `ICL` and the leading comments; declared `ERROR` external; removed lower-case code; used `SAVE` instead of an ineffectual `COMMON`. |
!> | 1997-02-13 | RAH | 4.1 | Reversed the `RLFNOW`, `RLHNOW`, and `RLGNOW` subscripts (see [[vssim]]). |
!> | 1997-05-22 | RAH | 4.1 | Initialised the saved locals. |
!> | 2026-04-03 | SvB | 4.6 | Moved the saved interpolation state out of routine `SAVE` locals into module-level storage, so it survives independently of this routine's declarations. |
!> @endhistory
   SUBROUTINE VSPREP()

      ! Assumed global variables provided via host module(s):
      ! NVSEE, NVSWL, WLD, TIH, UZNOW, UZNEXT, WLNOW
      ! NVSLF, LFB, NVSLFT, NVSLFN, RLFNOW
      ! NVSLH, LHB, NVSLHT, NVSLHN, RLHNOW
      ! NVSLG, LGB, NVSLGT, NVSLGN, RLGNOW
      ! NVSBF, BFB, RBFNOW, NVSBH, BHB, RBHNOW
      ! ERRLVL_fatal, FID_logfile

      IMPLICIT NONE

      ! Locals
      INTEGER :: I, II, III, NDUM

      ! Modernization Fix: Resurrected the saved state variables from the comments!
      ! These must be SAVED to track time-series interpolation across timesteps.
      ! DOUBLE PRECISION, SAVE :: WLLAST = 0.0D0, WLTIME = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RWELIN(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RLFLST = 0.0D0, RLFTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RLFPRV(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RLHLST = 0.0D0, RLHTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RLHPRV(NVSEE) = 0.0D0, RLHNXT(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RLGLST = 0.0D0, RLGTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RLGPRV(NVSEE) = 0.0D0, RLGNXT(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RBFLST = 0.0D0, RBFTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RBFPRV(NVSEE) = 0.0D0

      ! DOUBLE PRECISION, SAVE :: RBHLST = 0.0D0, RBHTIM = 0.0D0
      ! DOUBLE PRECISION, SAVE :: RBHPRV(NVSEE) = 0.0D0, RBHNXT(NVSEE) = 0.0D0

      ! Workspace arrays for boundary data reads
      ! DOUBLE PRECISION :: RLFDUM(NVSEE), RLHDUM(NVSEE), RLGDUM(NVSEE)

      !----------------------------------------------------------------------*

      ! wells
      IF (NVSWL > 0) THEN
         CALL FINPUT(WLD, TIH, UZNOW, UZNEXT, WLLAST, WLTIME, RWELIN, NVSWL, WLNOW)

         IF (EQMARKER(WLTIME)) THEN
            CALL RAISE_ERROR(ERRLVL_fatal, 1042, FID_logfile, 0, 0, 'End of well abstraction file (WLD)')
         END IF
      END IF

      ! lateral flow boundary condition
      IF (NVSLF > 0) THEN
         CALL FINPUT(LFB, TIH, UZNOW, UZNEXT, RLFLST, RLFTIM, RLFPRV, NVSLFT, RLFDUM)

         IF (EQMARKER(RLFTIM)) THEN
            CALL RAISE_ERROR(ERRLVL_fatal, 1043, FID_logfile, 0, 0, 'End of lateral flow boundary condition file (LFB)')
         END IF

         III = 1
         lf_main_loop: DO I = 1, NVSLF
            NDUM = NVSLFN(I)
            IF (NDUM == 0) NDUM = 1

            lf_sub_loop: DO II = 1, NDUM
               RLFNOW(II, I) = RLFDUM(III)
               III = III + 1
            END DO lf_sub_loop
         END DO lf_main_loop
      END IF

      ! lateral head boundary condition
      IF (NVSLH > 0) THEN
         CALL HINPUT(LHB, TIH, UZNOW, UZNEXT, RLHLST, RLHTIM, RLHPRV, &
                     RLHNXT, NVSLHT, RLHDUM)

         IF (EQMARKER(RLHTIM)) THEN
            CALL RAISE_ERROR(ERRLVL_fatal, 1044, FID_logfile, 0, 0, 'End of lateral head boundary condition file (LHB)')
         END IF

         III = 1
         lh_main_loop: DO I = 1, NVSLH
            NDUM = NVSLHN(I)
            IF (NDUM == 0) NDUM = 1

            lh_sub_loop: DO II = 1, NDUM
               RLHNOW(II, I) = RLHDUM(III)
               III = III + 1
            END DO lh_sub_loop
         END DO lh_main_loop
      END IF

      ! lateral head gradient boundary condition
      IF (NVSLG > 0) THEN
         CALL HINPUT(LGB, TIH, UZNOW, UZNEXT, RLGLST, RLGTIM, RLGPRV, &
                     RLGNXT, NVSLGT, RLGDUM)

         IF (EQMARKER(RLGTIM)) THEN
            CALL RAISE_ERROR(ERRLVL_fatal, 1052, FID_logfile, 0, 0, 'End of lateral head gradient boundary condition file (LGB)')
         END IF

         III = 1
         lg_main_loop: DO I = 1, NVSLG
            NDUM = NVSLGN(I)
            IF (NDUM == 0) NDUM = 1

            lg_sub_loop: DO II = 1, NDUM
               RLGNOW(II, I) = RLGDUM(III)
               III = III + 1
            END DO lg_sub_loop
         END DO lg_main_loop
      END IF

      ! column base flow boundary condition
      IF (NVSBF > 0) THEN
         CALL FINPUT(BFB, TIH, UZNOW, UZNEXT, RBFLST, RBFTIM, RBFPRV, &
                     NVSBF, RBFNOW)

         IF (EQMARKER(RBFTIM)) THEN
            CALL RAISE_ERROR(ERRLVL_fatal, 1045, FID_logfile, 0, 0, 'End of column base flow boundary condition file (BFB)')
         END IF
      END IF

      ! column base head boundary condition
      IF (NVSBH > 0) THEN
         CALL HINPUT(BHB, TIH, UZNOW, UZNEXT, RBHLST, RBHTIM, RBHPRV, &
                     RBHNXT, NVSBH, RBHNOW)

         IF (EQMARKER(RBHTIM)) THEN
            CALL RAISE_ERROR(ERRLVL_fatal, 1046, FID_logfile, 0, 0, 'End of column base head boundary condition file (BHB)')
         END IF
      END IF

   END SUBROUTINE VSPREP

!> Runs the VSS solver for one model timestep.
!>
!> `VSSIM` is the timestep controller for the variably saturated subsurface
!> component. It prepares time-varying boundary values, builds the per-element
!> column work arrays, iterates the coupled column solves, and then reconciles
!> the reported fluxes with the final water-content change.
!>
!> Main timestep sequence:
!>
!> | Stage | Work performed | Main routines/arrays |
!> |:------|:---------------|:---------------------|
!> | One-time setup | Initialise static column boundary flags, face areas, soil-type lookup, and stream-aquifer boundary types. | `JCBCsv`, `VSAIJsv`, `ICSOILsv` |
!> | Boundary preparation | Read/interpolate current VSS boundary data. | [[vsprep]], `WLNOW`, `RLFNOW`, `RLHNOW`, `RLGNOW`, `RBFNOW`, `RBHNOW` |
!> | Surface forcing | Convert rainfall, evaporation, soil evaporation, root extraction, and surface-water depth to column source terms. | `CDNET`, `CQ`, `ESOILA`, `ERUZ`, `PNETTO`, `EEVAP` |
!> | State save | Store pressure head and water content from time level \(n\). | `VSPSIN`, `VSTHEN` |
!> | Global nonlinear iteration | Visit elements in `ISORT`, assemble neighbour data, solve each active column with [[vscolm]], and track the largest pressure-head correction. | `VSPSI`, `VSTHE`, `VSKR`, `QVSH`, `QVSV` |
!> | Flux correction/output | Apply mass-balance correction and derive VSS-to-OC/bank summary fluxes. | [[vsmb]], `QVSBF`, `QH`, `QVSWEL`, `QBKB`, `QBKF`, `QBKI` |
!>
!> The surface forcing depth passed into each active column is
!> \[
!>   CDNET_e = \left(PNETTO_e - (EEVAP_e-ESOILA_e)\right)DTUZ
!>             + (h_{rf,e}-z_{g,e}),
!> \]
!> where `GETHRF(e)-ZGRUND(e)` is the current surface-water depth. Root uptake
!> and soil evaporation are assembled as source terms in `CQ`: cells in the
!> rooting zone receive \(-ERUZ(e,c)A_e\), and the top cell also receives
!> \(-ESOILA(e)A_e\). When explicit bank elements are disabled, link `CDNET`
!> values are set only to link water depth before active land columns are
!> solved; rainfall and evaporation on channels are handled later in the main
!> simulation sequence.
!>
!> The active element range starts at `ISTART = 1` when explicit bank elements
!> are enabled (`BEXBK`), so links and banks are solved as VSS columns; otherwise
!> it starts at `total_no_links + 1` and only land/grid columns are solved.
!> Stream-aquifer interaction is still accounted for without explicit banks by
!> assigning boundary type `9` on land faces adjacent to links; with banks it
!> uses type `10` on link-bank faces.
!>
!> The global iteration stops when
!> \[
!>   \max_e\max_i |\psi_i^{m+1}(e)-\psi_i^m(e)| \le 10^{-4}\ {\rm m},
!> \]
!> or after `NITMAX = 10` iterations. After `NITMIN = 2`, elements whose own
!> pressure change and neighbouring pressure changes are below the tolerance are
!> marked converged and skipped in later global iterations. If convergence is
!> not reached, warning 1039 is issued with rate-limited repeated reporting.
!> On the final global iteration `ELEVEL` is passed to [[vscolm]] as `ERRLVL_error`,
!> but the non-convergence `ERROR` call in this routine uses `ERRLVL_warn`.
!>
!> Boundary-condition flags used in the column solve:
!>
!> | `JCBC` value | Meaning |
!> |:-------------|:--------|
!> | `0` | internal face or no-flow boundary |
!> | `1` | well |
!> | `2` | spring |
!> | `3` | lateral flow boundary |
!> | `4` | lateral head boundary |
!> | `5` | lateral head-gradient boundary |
!> | `6` | column-base flow boundary |
!> | `7` | column-base head boundary |
!> | `8` | column-base free drainage |
!> | `9` | stream-aquifer interaction without explicit banks |
!> | `10` | stream-aquifer interaction with explicit banks |
!>
!> Key entry conditions carried over from the legacy interface:
!>
!> | Requirement | Purpose |
!> |:------------|:--------|
!> | `1 <= LLEE`, `LL <= LLEE`, `NEL <= NELEE`, `0 <= NLF <= NLFEE` | Global dimensions must cover the active catchment. |
!> | `LL = NLYRBT(e,NLYR(e)+1)` and ordered `NLYRBT` layer bounds | Each element must have a valid active cell range. |
!> | If `BEXBK`, link neighbours must be typed as link/bank/grid elements. | Stream-bank connectivity is required before assigning `JCBC = 10`. |
!> | For `e = ISTART:NEL`, boundary types are non-negative and categories are at least one. | `VSREAD`/`VSIN` must have assigned valid defaults. |
!> | `NLBTYP(e) > 0` implies `NBFACE(e) > 0`; wells and springs are mutually exclusive on an element. | Column boundary setup assumes one lateral-boundary face and one vertical source type. |
!> | Faces to elements earlier than `ISTART` have zero `JVSACN` connectivity. | Non-solved neighbours are represented through boundary/stream terms instead of lateral column coupling. |
!>
!> Limited ranges: element/cell arrays are used over
!> `NLYRBT(e,1):LL`; link arrays are used over `1:NLF`; and `VSKR` may be input
!> from any neighbour already visited in `ISORT`, then overwritten for active
!> elements after their column solve.
!>
!> Output summary terms after [[vsmb]]:
!>
!> | Array | Value assigned here |
!> |:------|:--------------------|
!> | `QVSBF(e)` | Bottom vertical flux `QVSV(ICBOT-1,e)`. |
!> | `QH(e)` | Top vertical flux `QVSV(ICTOP,e)`. |
!> | `QVSWEL(e)` | Sum of `QVSWLI` over the well screen when `NVSWLI(e)>0`. |
!> | `QBKF(link,bank)` | Sum of lateral VSS fluxes from the bank/grid side above the channel bed. |
!> | `QBKB(link,bank)` | Half-link surface exchange `-0.5*A_link*QH(link)` only with explicit banks and a wet link. |
!> | `QBKI(link,bank)` | Same half-link exchange only with explicit banks and a dry link. |
!>
!> @note
!> `FIRSTvssim` gates the setup of `JCBCsv`, `VSAIJsv`, and `ICSOILsv`. Changes
!> to boundary-type arrays, layer soil types, element geometry, or explicit-bank
!> mode after the first call are therefore not reflected in the cached column
!> metadata. If an element were both a well and a spring, the spring flag would
!> overwrite the well flag in `JCBCsv(5,e)`; valid input should avoid that case.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-29 | GP | 4.0 | Written; version 4.0 completed 1996-07-17. |
!> | 1996-12-28 | RAH | 4.1 | Removed temporary debug code; made `DPSIEL`/`DPSIMX` non-negative; brought `CWV`/`CWL` from `VSCOLM.INC` and passed them to [[vscolm]]. |
!> | 1997-02-07 | RAH | 4.1 | Dispensed with `CNOW`, `CTHEN`, `CV`, `CWV`, `CWL`, `VSPOR1`, `VSSTMP`; replaced `CQINF` with `CQV(ICTOP)`; used a `DO 660` loop instead of `GOTO`; redefined `CQWI` (see [[vswell]]); accumulated `QVSWEL` locally; used `OK` to simplify the convergence test. |
!> | 1997-02-10 | RAH | 4.1 | Removed `CETAN`, `CKRN`, `CPSIM`, `NVSCIT`; made `PSIM` one-dimensional; dispensed with `BCHELE`, `CA0`, `CPSIN`, `CPSL`, `CQSP`, `CZG`, `DT`; used `ALINIT` and `DCOPY`; brought `VSPSIN`/`VSTHEN` from `VSCOM1.INC` with reversed indices; set `JCACN=0` (and skipped `JCDEL*`, `C*IJ1`, `CZ1`, `CPSI*1`) when `JEL <= 0`; moved the `CQH` initialisation into [[vscolm]] and `SIGMA` into [[vsintc]]; set `ICTOP`, `QH`, `QVSBF`, and `QBK*` once. |
!> | 1997-02-11 | RAH | 4.1 | Replaced `CES`, `CDW`, `CEW`, `CQP` with `CDNET`; brought `CQ` (with `ICBED`, `ICLYRB`, `ICSOIL`) from `VSCOLM.INC`, added a dimension, and set it once; scrapped `ICWL*`, `ICSP*`, `CZSP`, `CCS`; initialised `QH` and `QVSH`. |
!> | 1997-02-13 | RAH | 4.1 | Brought `JCBC`, `ICWCAT`, `ICLBCT`, `ICBBCT`, `CZS` from `VSCOLM.INC` and scrapped `CQWIN`, `CLF`, `ICLFL`, `ICLFN`, `CLH`, `ICLHL`, `ICLHN`, `CLG`, `ICLGL`, `ICLGN`, `CBF`, `CBH`; included `VSSOIL.INC`; removed `NVSSPT`/`NVSWLT`; gave `JCBC` a dimension and defined it once; swapped the `NVSL*L`/`RL*NOW` subscripts. |
!> | 1997-02-14 | RAH | 4.1 | Brought `CDELL`, `CDELL1`, `CAIJ`, `CAIJ1` from `VSCOLM.INC`; replaced `CAIJ` with `VSAIJ`, set once and reused for `CAIJ1`; reversed the `DELTAZ`/`QVSH` subscripts and passed them to [[vscolm]]; scrapped `CDELZ` and `CQH`. |
!> | 1997-02-17 | RAH | 4.1 | Swapped the `JVSACN`, `JVSDEL`, `ZVSNOD`, `QVSV`, `QVSWLI`, `VSPSI`, `VSTHE`, `IVSSTO`, and `VSKR` subscripts, which also fixed an error whereby `ICSTOR` was left uninitialised; scrapped `JCACN`, `JCDEL`, `CZ`, `CQV`, `CQWI`, `CPSI`, `ICSTOR`, `CTHETA`, `CKR` from `VSCOLM.INC` and brought the remainder (`CPSI1`, `CPSIN1`, `CZ1`, `CKIJ1`, `JCDEL1`); added a dimension to `ICSOIL` and set it once; scrapped `CKZS`/`CKIJS` in favour of `VSK3D`, also used for `CKIJ1`; redefined `CQ` to be premultiplied by `AREA*DELTAZ` (see [[vsintc]]); moved `QVSWEL` outside the loop and placed the [[vsmb]] call straight after it. |
!> | 1997-05-15 | RAH | 4.1 | Reordered the [[vscolm]] arguments. |
!> | 1997-05-22 | RAH | 4.1 | Removed the now-unnecessary `MAX` on `ICWLBT` and similar. |
!> | 1997-06-18 | RAH | 4.1 | Stopped calling `VSCOLP`; ran loop 285 when `JEL >= ISTART` (previously `>= 1`). |
!> | 1998-04-02 | RAH | 4.2 | Passed the new local `ELEVEL` to [[vscolm]]. |
!> | 1998-11-03 | SPA | - | Passed adjacent surface-water depth (`depadj`) to [[vscolm]], as well as the adjacent water-surface elevation, for the channel-aquifer flow correction. |
!> | 1998-11-04 | SPA | - | Made reported bank exchange flows consistent with BALWAT. |
!> | 2009-01 | JE | 4.3.5F90 | Restructured loops for automatic differentiation. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote the labelled `GOTO`-driven element/face/cell loops as `DO`/`CYCLE` constructs; removed the `ALINIT` calls in favour of array-slice zero-assignment. Same convergence test and reported fluxes. |
!> @endhistory
   SUBROUTINE VSSIM()

      IMPLICIT NONE

      ! Locals, etc
      INTEGER, PARAMETER :: NITMAX = 10, NITMIN = 2
      DOUBLE PRECISION, PARAMETER :: GEPSMX = 1.0D-4, DRYH = 1.0D-8

      INTEGER :: N, IFDUM1, IFDUM2, NIT, NCELL, WET, ICDUM, K, ELEVEL
      INTEGER :: I, II, IEL, IFA, ICL, ILYR, IW, ITYPE, IBK, ISTART, IBANK
      INTEGER :: JEL, JFA, JCL, JCBED, JELDUM(4)
      INTEGER :: ICBOT, ICTOP, ICWCAT, ICLBCT, ICBBCT, ICBED, ICWLBT

      DOUBLE PRECISION :: DPSIEL, DPSIMX
      DOUBLE PRECISION :: CDW, CES, CQW, QBK, QI
      DOUBLE PRECISION :: CA0, DXYDUM

      INTEGER, SAVE :: errorcount2 = 0
      LOGICAL :: TEST, g670

      ! Note: Variables mapped from implicit context (LLEE, NELEE, NLYREE, etc.)
      ! are retained here strictly according to user rules.
      INTEGER :: JCDEL1(LLEE, 4), ICLYRB(NLYREE)
      DOUBLE PRECISION :: DELTAP(0:NELEE), CDNET(NELEE), CQ(LLEE, NELEE)
      DOUBLE PRECISION :: CDELL(4), CDELL1(4), CAIJ1(LLEE, 4), CZ1(LLEE, 4)
      DOUBLE PRECISION :: PSIM(LLEE), VSPSIN(LLEE, NELEE), VSTHEN(LLEE, NELEE)
      DOUBLE PRECISION :: CPSI1(LLEE, 4), CPSIN1(LLEE, 4), CKIJ1(LLEE, 4), CZS(4)

      ! Extra array: depadj - depth of surface water for adjacent
      ! elements - added for channel aquifer flows fix, SPA, 03/11/98
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      DOUBLE PRECISION :: depadj(4)
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      LOGICAL :: OK(NELEE)

      !----------------------------------------------------------------------*
      ! Initialization
      !________________*
      IF (BEXBK) THEN
         IBANK = 1
         ISTART = 1
      ELSE
         IBANK = 0
         ISTART = total_no_links + 1
      END IF

      ICTOP = top_cell_no

      IF (FIRSTvssim) THEN

         FIRSTvssim = .FALSE.

         ! * set outputs & locals for non-column elements
         ! Replaced ALINIT with array slices
         IF (ISTART > 1) QH(1:ISTART - 1) = ZERO

         DO IEL = 1, ISTART - 1
            ICBOT = NLYRBT(IEL, 1)
            QVSH(1:4, ICBOT:ICTOP, IEL) = ZERO
            VSAIJsv(1:4, ICBOT:ICTOP, IEL) = ZERO
            DO ICL = ICBOT, ICTOP
               ICSOILsv(ICL, IEL) = 1
            END DO
         END DO

         ! * set static locals for column elements
         DO IEL = ISTART, total_no_elements
            ! JCBC contains boundary condition types:
            ! 0 - bottom boundary; 1-4 - faces; 5 - well/spring
            ! boundary condition types are:
            ! 0     internal face or no-flow boundary condition
            ! 1     wells
            ! 2     springs
            ! 3     lateral flow
            ! 4     lateral head
            ! 5     lateral head gradient
            ! 6     column base flow
            ! 7     column base head
            ! 8     column base free drainage
            ! 9     stream-aquifer interaction (without banks)
            ! 10    stream-aquifer interaction (with banks)
            DO II = 1, 5
               JCBCsv(II, IEL) = 0
            END DO

            JCBCsv(0, IEL) = NBBTYP(IEL)
            IFA = MAX(1, NBFACE(IEL))
            JCBCsv(IFA, IEL) = NLBTYP(IEL)

            IF (NVSWLI(IEL) > 0) JCBCsv(5, IEL) = 1
            IF (NVSSPC(IEL) > 0) JCBCsv(5, IEL) = 2

            DO IFA = 1, 4
               JEL = ICMREF(IEL, IFA + 4)
               TEST = IEL > total_no_links .AND. JEL >= 1 .AND. JEL <= total_no_links
               IF (TEST) JCBCsv(IFA, IEL) = 9 + IBANK

               ! VSAIJ contains cell-face areas for lateral flow (note face 1=3, 2=4)
               IFDUM1 = MOD(IFA, 4) + 1
               IFDUM2 = MOD(IFA + 2, 4) + 1
               DXYDUM = DHF(IEL, IFDUM1) + DHF(IEL, IFDUM2)

               DO ICL = NLYRBT(IEL, 1), ICTOP
                  VSAIJsv(IFA, ICL, IEL) = DELTAZ(ICL, IEL)*DXYDUM
               END DO
            END DO

            ! ICSOIL contains soil types for each cell
            DO ILYR = 1, NLYR(IEL)
               N = NTSOIL(IEL, ILYR)
               DO ICL = NLYRBT(IEL, ILYR), NLYRBT(IEL, ILYR + 1) - 1
                  ICSOILsv(ICL, IEL) = N
               END DO
            END DO

         END DO
      END IF

      ! prepare catchment boundary condition data
      CALL VSPREP

      ! Calc. depth of water for channel links, even if no banks
      ! n.b. rainfall and evap terms neglected, as these are calculated for
      ! channels after VSS is called.
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      IF (.NOT. bexbk) THEN
         DO IEL = 1, total_no_links
            CDNET(IEL) = GEThrf(IEL) - zgrund(IEL)
         END DO
      END IF
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      DO IEL = ISTART, total_no_elements

         CES = ESOILA(IEL)
         CDW = GETHRF(IEL) - ZGRUND(IEL)

         CDNET(IEL) = (PNETTO(IEL) - (EEVAP(IEL) - CES))*DTUZ + CDW
         CA0 = cellarea(IEL)
         ICBOT = NLYRBT(IEL, 1)
         ICDUM = ICTOP + 1

         IF (IEL > total_no_links) ICDUM = ICDUM - NRD(NVC(IEL))

         ! Replaced ALINIT with array slice
         IF (ICDUM > ICBOT) CQ(ICBOT:ICDUM - 1, IEL) = ZERO

         ! stop crash if rooting zone is below base of aquifer sb 020211
         ICDUM = MAX(1, ICDUM)

         DO ICL = ICDUM, ICTOP
            CQ(ICL, IEL) = -ERUZ(IEL, ICL)*CA0
         END DO

         CQ(ICTOP, IEL) = CQ(ICTOP, IEL) - CES*CA0

      END DO

      ! save psi values at time level N
      DO IEL = 1, total_no_elements
         ICBOT = NLYRBT(IEL, 1)
         NCELL = ICTOP - ICBOT + 1
         CALL DCOPY(NCELL, VSPSI(ICBOT, IEL), 1, VSPSIN(ICBOT, IEL), 1)
         CALL DCOPY(NCELL, VSTHE(ICBOT, IEL), 1, VSTHEN(ICBOT, IEL), 1)
      END DO

      ! initialize convergence indicators (Replaced ALINIT with array slice)
      DELTAP(0:ISTART - 1) = ZERO

      DO IEL = 1, ISTART - 1
         OK(IEL) = .TRUE.
      END DO

      DO IEL = ISTART, total_no_elements
         OK(IEL) = .FALSE.
      END DO

      ! start of main iteration loop
      !______________________________*
      ELEVEL = 0
      g670 = .FALSE.

      DO NIT = 1, NITMAX

         IF (NIT == NITMAX) ELEVEL = ERRLVL_error
         DPSIMX = ZERO

         DO I = 1, total_no_elements
            IEL = ISORT(I)

            IF (OK(IEL)) CYCLE

            ICBOT = NLYRBT(IEL, 1)
            ITYPE = ICMREF(IEL, 1)

            NCELL = ICTOP - ICBOT + 1

            ! save psi at iteration level m
            CALL DCOPY(NCELL, VSPSI(ICBOT, IEL), 1, PSIM(ICBOT), 1)

            ! set up column arrays using global arrays
            DO ILYR = 1, NLYR(IEL) + 1
               ICLYRB(ILYR) = NLYRBT(IEL, ILYR)
            END DO

            IF (ITYPE == 1 .OR. ITYPE == 2) ICBED = NHBED(ICMREF(IEL, 4), ITYPE)

            DO IFA = 1, 4
               CDELL(IFA) = DHF(IEL, IFA)
               JEL = ICMREF(IEL, IFA + 4)
               JELDUM(IFA) = JEL

               IF (JEL < 1) THEN
                  DXYDUM = ZERO
               ELSE
                  CZS(IFA) = GETHRF(JEL)

                  ! !!!!! fix for channel aquifer flows, SPA, 03/11/98
                  ! Pass depth of water in adjacent elements to vscolm
                  ! as well as elevation of water surface
                  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  depadj(IFA) = cdnet(JEL)
                  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  JFA = ICMREF(IEL, IFA + 8)
                  DXYDUM = DHF(JEL, JFA)
               END IF

               CDELL1(IFA) = DXYDUM

               IF (JEL < ISTART) CYCLE

               ! NB: VSPSI, VSKR may hold values from previous iteration
               K = MOD(JFA - 1, 2) + 1
               DO JCL = NLYRBT(JEL, 1), top_cell_no
                  JCDEL1(JCL, IFA) = JVSDEL(JFA, JCL, JEL)
                  CAIJ1(JCL, IFA) = VSAIJsv(JFA, JCL, JEL)
                  CZ1(JCL, IFA) = ZVSNOD(JCL, JEL)
                  CPSI1(JCL, IFA) = VSPSI(JCL, JEL)
                  CPSIN1(JCL, IFA) = VSPSIN(JCL, JEL)
                  N = ICSOILsv(JCL, JEL)
                  CKIJ1(JCL, IFA) = VSKR(JCL, JEL)*VSK3D(N, K)
               END DO

            END DO

            ! boundary condition indices
            IW = MAX(1, NVSWLI(IEL))
            ICWLBT = NWELBT(IEL)
            ICWCAT = NVSWLC(IEL)
            ICLBCT = NLBCAT(IEL)
            ICBBCT = NBBCAT(IEL)

            ! calculate new potentials and flow rates
            CALL VSCOLM(NSEE, VSWV, VSWL, VSK3D, BHELEV, ELEVEL, IEL, ICBOT, ICTOP, ICBED, &
                        ICLYRB, ICSOILsv(ICBOT, IEL), JCBCsv(0, IEL), JCDEL1, JELDUM, &
                        JVSACN(1, ICBOT, IEL), JVSDEL(1, ICBOT, IEL), NVSSPC(IEL), &
                        NVSLFN(ICLBCT), NVSLFL(1, ICLBCT), NWELBT(IEL), NVSLHN(ICLBCT), &
                        NVSLHL(1, ICLBCT), NWELTP(IEL), NVSLGN(ICLBCT), NVSLGL(1, ICLBCT), &
                        cellarea(IEL), ZGRUND(IEL), VSSPZ(IEL), VSSPCO(IEL), &
                        DELTAZ(ICBOT, IEL), ZVSNOD(ICBOT, IEL), CDELL, VSAIJsv(1, ICBOT, IEL), &
                        CAIJ1, CDELL1, CZ1, DTUZ, CDNET(IEL), VSPSIN(ICBOT, IEL), &
                        CQ(ICBOT, IEL), CZS, CPSI1, CPSIN1, CKIJ1, WLNOW(ICWCAT), &
                        RLFNOW(1, ICLBCT), RLHNOW(1, ICLBCT), RLGNOW(1, ICLBCT), &
                        RBFNOW(ICBBCT), RBHNOW(ICBBCT), IVSSTO(ICBOT, IEL), &
                        VSPSI(ICBOT, IEL), VSKR(ICBOT, IEL), VSTHE(ICBOT, IEL), &
                        QVSH(1, ICBOT, IEL), QVSV(ICBOT - 1, IEL), QVSWLI(ICWLBT, IW), &
                        QVSSPR(IEL), ZVSPSL(IEL), depadj)

            ! extra argument depadj added for channel-aquifer flows fix
            ! SPA, 03/11/98

            ! record largest change for this iteration
            DPSIEL = ZERO
            DO ICL = ICBOT, ICTOP
               DPSIEL = MAX(DPSIEL, ABS(VSPSI(ICL, IEL) - PSIM(ICL)))
            END DO

            DELTAP(IEL) = DPSIEL
            DPSIMX = MAX(DPSIMX, DPSIEL)

            ! end of element loop: check for convergence or maximum iterations
         END DO

         ! 970214  At present the criterion on DPSIMX overrides that on NIT
         IF (DPSIMX <= GEPSMX) THEN
            g670 = .TRUE.
            EXIT
         END IF

         IF (NIT >= NITMIN) THEN
            DO IEL = ISTART, total_no_elements
               DPSIEL = DELTAP(IEL)
               DO IFA = 1, 4
                  JEL = MAX(0, ICMREF(IEL, IFA + 4))
                  DPSIEL = MAX(DPSIEL, DELTAP(JEL))
               END DO
               OK(IEL) = DPSIEL < GEPSMX
            END DO
         END IF

         ! end of iteration loop
      END DO

      IF (.NOT. g670) THEN
         errorcount2 = errorcount2 + 1
         IF (errorcount2 < errcntallowed) THEN
            CALL RAISE_ERROR(ERRLVL_error, 1039, FID_logfile, 0, 0, 'Maximum iterations in VSS global solver')
         ELSE IF (errorcount2 == errcntallowed) THEN
            CALL RAISE_ERROR (ERRLVL_error, 1039, FID_logfile, 0, 0, '**** Last printout of the error message - maximum iterations in VSS global solver *****')
         END IF
      END IF

      ! main solution is complete: tidy up
      !____________________________________*
      ! update flows to ensure mass conservation

      CALL VSMB(VSTHEN)

      ! set auxiliary output arrays
      DO IEL = ISTART, total_no_elements
         ICBOT = NLYRBT(IEL, 1)
         QVSBF(IEL) = QVSV(ICBOT - 1, IEL)
         QH(IEL) = QVSV(ICTOP, IEL)
         IW = NVSWLI(IEL)

         IF (IW < 1) CYCLE

         CQW = ZERO
         DO ICL = NWELBT(IEL), NWELTP(IEL)
            CQW = QVSWLI(ICL, IW) + CQW
         END DO

         QVSWEL(IEL) = CQW
      END DO

      ! calculate QBKB, QBKF, QBKI for all cases:
      !    bank elements or not, including dry channels
      DO IBK = 1, 2

         DO IEL = 1, total_no_links
            QI = -HALF*cellarea(IEL)*QH(IEL)
            WET = NINT(HALF + SIGN(HALF, GETHRF(IEL) - ZGRUND(IEL) - DRYH))
            IFA = 2*IBK

            IF (LINKNS(IEL)) IFA = IFA - 1
            JEL = ICMREF(IEL, IFA + 4)
            JFA = ICMREF(IEL, IFA + 8)

            JCBED = top_cell_no
            IF (JEL > 0) JCBED = NLYRBT(JEL, 1) - 1
            IF (BEXBK) JCBED = NHBED(IEL, IBK)

            QBK = ZERO
            DO JCL = JCBED + 1, top_cell_no
               QBK = QBK + QVSH(JFA, JCL, JEL)
            END DO

            ! !!! mod.s to make definition of exchange flows consistent with balwat
            ! SPA, 04/11/98
            QBKF(IEL, IBK) = QBK
            QBKB(IEL, IBK) = QI*IBANK*WET
            QBKI(IEL, IBK) = QI*IBANK*(1 - WET)
         END DO

      END DO

   END SUBROUTINE VSSIM

END MODULE vs_driver

