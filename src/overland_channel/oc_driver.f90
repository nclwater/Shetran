!> summary: Overland/channel setup and the per-timestep solve.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[OCINI]] reads and validates the input, builds the row-solver indexing and
!> the cross-section tables, and allocates the workspace; [[OCSIM]] advances
!> the overland and channel flow over one coupled model timestep, sub-stepping
!> internally. [[simulation_driver:SIMULATION]] calls `OCSIM`, frame setup
!> calls `OCINI`, and [[shetran]] calls [[FINALISE_OCSIM_WORKSPACE]] at the
!> end of the run.
!>
!> `OCSIM_WORKSPACE` holds the solver's row arrays, allocated once by
!> `INITIALISE_OCSIM_WORKSPACE` against the widest active row rather than the
!> grid capacity. `OCSIM_WORKSPACE_HAS_ALLOCATIONS` reports whether it is
!> allocated; it is deliberately left undocumented, as it was before the move.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_driver

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE array_limits, ONLY: nelee, nlfee, NOCTAB, NXSCEE
   USE element_geometry, ONLY: cellarea, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, ICMRF2, NX, NY
   USE channel_geometry, ONLY: BEXBK, CLENTH, CWIDTH, ZBFULL
   USE file_units, ONLY: FID_logfile, OFB, OHB
   USE et_state, ONLY: ESWA, PNETTO
   USE vs_state, ONLY: QH
   USE oc_state, ONLY: ARXL, DQ0ST, DQIST, DQIST2, dtoc, OCNEXT, OCNOW, QMAX, QOC, qsazz
   USE oc_indexing, ONLY: MAX_SOLVER_ROW_WIDTH, NELIND, NROWEL, NROWF, NROWL, NROWST, OCIND
   USE oc_boundaries, ONLY: HOCNOW, NOCBCC, NOCBCD, NOCFB, NOCHB, OCABC, OCEXT, OCPRI
   USE oc_cross_sections, ONLY: NXSECT, OCXS, XAREA, XINH, XINW
   USE oc_node_solver, ONLY: gethrf, getqsa, sethrf, setqsa, initialise_ocmod, OCFIX
   USE oc_stage_discharge, ONLY: OCQDQ
   USE oc_input, ONLY: OCREAD
   USE oc_validation, ONLY: OCCHK0, OCCHK1, OCCHK2
   USE linear_algebra, ONLY: invertmat, jematmul_mm, jematmul_vm
   USE float_compare, ONLY: gtzero
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal
   USE error_status, ONLY: errstat_alloc, errstat_dealloc, errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: OCINI, OCSIM, FINALISE_OCSIM_WORKSPACE

   DOUBLEPRECISION    :: TDC                    !! First time for detailed OC diagnostic output; see the [[ocini]] shadowing warning.
   DOUBLEPRECISION    :: TFC                    !! Last time for detailed OC diagnostic output; see the [[ocini]] shadowing warning.
   ! Persistent [[ocsim]] workspace, allocated once by [[initialise_ocsim_workspace]].
   TYPE :: OCSIM_WORKSPACE_TYPE
      LOGICAL :: READY = .FALSE. !! True only after every workspace component has been allocated successfully.
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: AA       !! Next-row block coefficients.
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: DD       !! Back-substituted correction by row position and row.
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: BB       !! Current-row block coefficients.
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: GG       !! Forward-elimination constant term by row position and row.
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: CC       !! Previous-row block coefficients.
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: TM1      !! Scratch matrix product.
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: TM2      !! Row system matrix, inverted in place.
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: FF          !! Current-row right-hand-side vector.
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TV1         !! Scratch vector product.
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TV2         !! Row right-hand-side vector.
      DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE :: EE    !! Forward-elimination coefficients between rows.
   END TYPE OCSIM_WORKSPACE_TYPE

   TYPE(OCSIM_WORKSPACE_TYPE) :: OCSIM_WORKSPACE

CONTAINS

!> @brief Controls OC component initialisation.
!>
!> `OCINI` checks static dimensions and topology, reads the OC input file,
!> validates roughness and cross-section data, opens boundary files,
!> initialises OC state held in [[oc_state]] and [[oc_indexing]]'s row-solver
!> workspace, builds channel cross-section tables through [[OCXS]], and
!> prepares row indices through [[OCIND]].
!>
!> Entry requirements are the OC/frame array limits being positive
!> (`NELEE`, `NLFEE`, `NXEE`, `NY`, `NOCTAB`), at least two internal
!> cross-section table rows (`NXSCEE >= 2`), and an element index range with
!> `total_no_elements >= NGDBGN`.
!>
!> The routine uses frame geometry and topology from `ICMREF`, `ICMBK`,
!> `ICMXY`, `LCODEX`, `LCODEY`, `NBFACE`, and `LINKNS`, plus OC input/output
!> units `OCD`, `OHB`, `OFB`, and `PRI`. It treats `ZGRUND` as input for
!> land/bank elements and fills the link entries from the OC/cross-section
!> setup.
!>
!> Initialised shared outputs include boundary-condition counts and codes
!> (`NOCHB`, `NOCFB`, `NOCBCC`, `NOCBCD`, `COCBCD`), hydraulic geometry
!> (`HRF`, `CWIDTH`, `ZBEFF`, `ZBFULL`, `NXSECT`, `XINH`, `XINW`, `XAREA`,
!> `XAFULL`, `XSTAB`), Strickler/roughness fields (`STRXX`, `STRYY`), timing controls
!> (`TDC`, `TFC`), and row-index arrays (`NELIND`, `NROWEL`, `NROWST`,
!> `NROWF`, `NROWL`) used by the OC implicit row solver.
!>
!> @warning
!> The local `TDC` and `TFC` declared here shadow the module-level variables
!> of the same name that [[OCSIM]] reads to gate diagnostic printing.
!> `OCREAD` fills only these local copies, so the module's `TDC`/`TFC` are
!> never explicitly assigned by the current initialisation path. This
!> documents current behaviour; it was not repaired in this transfer.
!> @endwarning
   SUBROUTINE OCINI()

      IMPLICIT NONE

      ! Locals
      INTEGER :: KONT                                !! Print/output control read by [[ocread]]; odd values enable verbose echoing.
      DOUBLE PRECISION :: DDUM1(NOCTAB), DDUM2(NOCTAB, NOCTAB) !! Discarded roughness/cross-section scratch passed to [[ocread]].
      DOUBLE PRECISION :: TDC, TFC                    !! Shadow the module-level `TDC`/`TFC`; see the routine's warning.
      LOGICAL :: LDUM1(NELEE)                        !! Discarded per-element check-result scratch passed to [[occhk1]].
      INTEGER :: ios                                 !! I/O status from a boundary-file title read.
      CHARACTER(LEN=LENGTH_LINE)  :: emsg            !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'oc_driver:OCINI' !! Location string for read-error reports.
      DOUBLEPRECISION, DIMENSION(NLFEE), SAVE :: DUMMY !! Floating-point input workspace; `OCCHK2` writes it over `1..total_no_links` (and `1:NXSECT(link)-1 <= NOCTAB`), so `NLFEE` bounds it. `SAVE` keeps it in static storage, as the former module variable was.

      !----------------------------------------------------------------------*

      CALL OCCHK0()

      ! Call to check constraints using AD-aliasing safe interface
      CALL OCCHK1(SIZE(LDUM1), LDUM1)

      ! Input data & associated requirements
      CALL OCREAD(KONT, TDC, TFC, DDUM1, DDUM2)
      CALL OCCHK2(DUMMY, DDUM1, NELEE, LDUM1)

      ! Boundary data files
      ! Read title lines if applicable
      IF (NOCHB > 0) THEN
         READ (OHB, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
      END IF
      IF (NOCFB > 0) THEN
         READ (OFB, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
      END IF

      CALL INITIALISE_OCMOD()

      ! Cross-section tables & effective bed elevations
      IF (total_no_links > 0) THEN
         IF (MOD(KONT, 2) == 1) WRITE (FID_logfile, 9100) NXSCEE
         CALL OCXS()
      END IF

      ! Indicies for Thomas algorithm
      CALL OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)
      CALL INITIALISE_OCSIM_WORKSPACE()

      RETURN

      ! FORMAT statements
9100  FORMAT(/5X, 'Size of internal tables for channel conveyance, etc', '  NXSCEE =', I6)

   END SUBROUTINE OCINI

!> @brief Allocates the persistent [[ocsim]] row-solver work arrays.
!>
!> These arrays were formerly automatic local arrays in [[ocsim]]. They are too
!> large for the stack on some compilers/runs, but allocating them on every
!> [[ocsim]] call is expensive because [[ocsim]] is called every timestep.
!> Keeping them in a module-owned workspace preserves heap storage without
!> repeated allocation in the timestep loop.
!>
!> [[ocini]] calls this routine once, after [[ocind]] has established
!> `NROWF`, `NROWL`, and `MAX_ROW_WIDTH`. The program supports one model per
!> execution; a second call without intervening finalisation is a lifecycle
!> error. Static topology is not duplicated here: [[ocsim]] passes `ICMREF`
!> and `ICMRF2` to [[oc_node_solver:OCFIX]] directly.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-05-10 | SvB | 4.6.1 | Added this allocator while moving `AA`, `DD`, `FF`, `BB`, `GG`, `CC`, `EE`, `TM1`, `TM2`, `TV1`, `TV2`, `inhrf`, `GGGETHRF`, `inqsa`, `GGGETQSA`, `ijedum`, and `ijedum2` from automatic locals in [[ocsim]] to allocatable module state. |
!> | 2026-08-20 | - | - | Removed the duplicate topology work arrays after [[oc_node_solver:OCFIX]] was changed to accept `ICMREF` and `ICMRF2` in their native layouts. |
!> | 2026-08-22 | - | - | Removed the `INHRF`, `GGGETHRF`, `INQSA`, and `GGGETQSA` state buffers after [[oc_node_solver:OCFIX]] was changed to correct `HRFZZ`/`QSAZZ` in place. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE INITIALISE_OCSIM_WORKSPACE()
      IMPLICIT NONE

      INTEGER :: ALLOC_STATUS
      CHARACTER(LEN=512) :: ALLOC_MESSAGE, MSG

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "oc_driver:INITIALISE_OCSIM_WORKSPACE"

      IF (MAX_SOLVER_ROW_WIDTH <= 0 .OR. NROWF < 1 .OR. NROWL < NROWF .OR. total_no_elements <= 0) THEN
         WRITE (MSG, '(A,6(A,I0))') 'Invalid OCSIM workspace dimensions:', &
            ' NX=', NX, ' NY=', NY, ' NROWF=', NROWF, ' NROWL=', NROWL, &
            ' MAX_SOLVER_ROW_WIDTH=', MAX_SOLVER_ROW_WIDTH, ' NEL=', total_no_elements
         CALL RAISE_ERROR(ERRLVL_fatal, 1006, FID_logfile, 0, 0, TRIM(MSG))
      END IF

      IF (OCSIM_WORKSPACE%READY .OR. OCSIM_WORKSPACE_HAS_ALLOCATIONS()) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 1006, FID_logfile, 0, 0, 'OCSIM workspace initialised more than once')
      END IF

      ! `GG` and `EE` share a `+1` storage offset: the quantity [[ocsim]] forms
      ! while eliminating row `IROW` is stored at index `IROW+1`. Their row
      ! extents differ because of what each is indexed by, not because either
      ! carries spare space. `GG` holds one vector per active row, so it spans
      ! `NROWF+1:NROWL+1`. `EE` holds one matrix per interface between
      ! consecutive active rows, and N rows have N-1 interfaces, so it spans
      ! `NROWF+1:NROWL`. The `IF (IROW /= NROWL)` guard on the `EE` write
      ! follows from that: there is no interface below the last row. Both
      ! ranges are exactly tight, so neither tolerates an off-by-one.
      ALLOCATE (OCSIM_WORKSPACE%AA(MAX_SOLVER_ROW_WIDTH, MAX_SOLVER_ROW_WIDTH), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "AA", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%DD(MAX_SOLVER_ROW_WIDTH, NROWF:NROWL), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DD", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%FF(MAX_SOLVER_ROW_WIDTH), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "FF", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%BB(MAX_SOLVER_ROW_WIDTH, MAX_SOLVER_ROW_WIDTH), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "BB", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%GG(MAX_SOLVER_ROW_WIDTH, NROWF + 1:NROWL + 1), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "GG", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%CC(MAX_SOLVER_ROW_WIDTH, MAX_SOLVER_ROW_WIDTH), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CC", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%EE(MAX_SOLVER_ROW_WIDTH, MAX_SOLVER_ROW_WIDTH, NROWF + 1:NROWL), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "EE", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%TM1(MAX_SOLVER_ROW_WIDTH, MAX_SOLVER_ROW_WIDTH), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TM1", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%TM2(MAX_SOLVER_ROW_WIDTH, MAX_SOLVER_ROW_WIDTH), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TM2", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%TV1(MAX_SOLVER_ROW_WIDTH), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TV1", location, emsg)
      ALLOCATE (OCSIM_WORKSPACE%TV2(MAX_SOLVER_ROW_WIDTH), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TV2", location, emsg)

      ! IF (ALLOC_STATUS /= 0) THEN
      !    WRITE (MSG, '(A,6(A,I0),2A)') 'Unable to allocate OCSIM workspace:', &
      !       ' NX=', NX, ' NY=', NY, ' NROWF=', NROWF, ' NROWL=', NROWL, &
      !       ' MAX_ROW_WIDTH=', MAX_SOLVER_ROW_WIDTH, ' NEL=', total_no_elements, &
      !       ' allocator: ', TRIM(ALLOC_MESSAGE)
      !    CALL FINALISE_OCSIM_WORKSPACE()
      !    CALL RAISE_ERROR(ERRLVL_fatal, 1006, FID_logfile, 0, 0, TRIM(MSG))
      ! END IF

      OCSIM_WORKSPACE%READY = .TRUE.

   END SUBROUTINE INITIALISE_OCSIM_WORKSPACE

!> @brief Releases all persistent [[ocsim]] workspace storage.
!>
!> The procedure is idempotent and also handles a partially allocated object,
!> allowing the allocation-failure path to clean up before reporting a fatal
!> error. The normal program shutdown calls it after the final possible
!> [[ocsim]] use. SHETRAN still assumes one model per process execution.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-08-21 | SvB | - | Added this finaliser alongside [[initialise_ocsim_workspace]]. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE FINALISE_OCSIM_WORKSPACE()
      IMPLICIT NONE

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "oc_driver:FINALISE_OCSIM_WORKSPACE"

      IF (ALLOCATED(OCSIM_WORKSPACE%AA)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%AA, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "AA", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%DD)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%DD, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "DD", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%FF)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%FF, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "FF", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%BB)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%BB, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "BB", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%GG)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%GG, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "GG", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%CC)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%CC, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "CC", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%EE)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%EE, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "EE", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%TM1)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%TM1, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "TM1", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%TM2)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%TM2, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "TM2", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%TV1)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%TV1, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "TV1", location, emsg)
      END IF
      IF (ALLOCATED(OCSIM_WORKSPACE%TV2)) THEN
         DEALLOCATE (OCSIM_WORKSPACE%TV2, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "TV2", location, emsg)
      END IF

      OCSIM_WORKSPACE%READY = .FALSE.
      MAX_SOLVER_ROW_WIDTH = 0

   END SUBROUTINE FINALISE_OCSIM_WORKSPACE

   LOGICAL FUNCTION OCSIM_WORKSPACE_HAS_ALLOCATIONS()
      IMPLICIT NONE

      OCSIM_WORKSPACE_HAS_ALLOCATIONS = &
         ALLOCATED(OCSIM_WORKSPACE%AA) .OR. ALLOCATED(OCSIM_WORKSPACE%DD) .OR. &
         ALLOCATED(OCSIM_WORKSPACE%FF) .OR. ALLOCATED(OCSIM_WORKSPACE%BB) .OR. &
         ALLOCATED(OCSIM_WORKSPACE%GG) .OR. ALLOCATED(OCSIM_WORKSPACE%CC) .OR. &
         ALLOCATED(OCSIM_WORKSPACE%EE) .OR. ALLOCATED(OCSIM_WORKSPACE%TM1) .OR. &
         ALLOCATED(OCSIM_WORKSPACE%TM2) .OR. ALLOCATED(OCSIM_WORKSPACE%TV1) .OR. &
         ALLOCATED(OCSIM_WORKSPACE%TV2)

   END FUNCTION OCSIM_WORKSPACE_HAS_ALLOCATIONS

!> @brief Advances the overland/channel flow solution by one OC timestep.
!>
!> `OCSIM` reads current boundary values, calls [[OCQDQ]] for nonlinear flow
!> and derivative terms, assembles the row-wise implicit matrix with
!> [[OCABC]], solves the block tridiagonal system by forward row elimination
!> and backward substitution, updates water levels and inter-element flows,
!> applies [[OCFIX]] to remove spurious negative internal flows, computes
!> channel wetted area, and optionally prints OC diagnostics.
!>
!> The routine uses frame topology and geometry (`ICMREF`, `ICMRF2`,
!> `NOCBCC`, `NOCBCD`, `cellarea`, `CLENTH`, `DHF`, `ZGRUND`, `CWIDTH`,
!> `ZBFULL`), forcing terms (`PNETTO`, `ESWA`, `QH`, `QOCF`, `HOCNOW`), OC
!> row indices (`NROWF`, `NROWL`, `NROWST`, `NROWEL`, `NELIND`),
!> cross-section and roughness tables (`NXSECT`, `XINH`, `XINW`, `XAREA`,
!> `XSTAB`, `STRXX`, `STRYY`), and timing controls (`OCNOW`, `OCNEXT`,
!> `TDC`, `TFC`). It updates `HRF` and writes `QSA`, `QOC`, `DQ0ST`,
!> `DQIST`, `DQIST2`, and `ARXL`. The per-link bankfull area `XAFULL` that
!> [[OCQDQ]] reads is a static property of the cross-section tables and is
!> built once by [[ocxs]] during initialisation, not here.
!>
!> The OC timestep is converted to seconds as
!>
!> \[
!> DTOC = 3600\,OCNEXT.
!> \]
!>
!> After [[OCQDQ]] has evaluated current flows and derivatives, [[OCABC]]
!> assembles one block row for each y-row. For row \(r\), with water-level
!> correction vector \(d_r\), the assembled equation has the
!> block-tridiagonal form
!>
!> \[
!> C_r d_{r-1} + B_r d_r + A_r d_{r+1} = F_r,
!> \]
!>
!> where `CC`, `BB`, and `AA` contain the previous-row, current-row, and
!> next-row coefficients, and `FF` contains \(F_r\).
!>
!> The forward sweep stores each eliminated row as
!>
!> \[
!> d_{r-1} = E_r d_r + G_r.
!> \]
!>
!> For the first active row,
!>
!> \[
!> M_r=B_r,\qquad v_r=F_r.
!> \]
!>
!> For later rows, the previous-row relation is substituted:
!>
!> \[
!> M_r = B_r + C_rE_r,\qquad
!> v_r = F_r - C_rG_r.
!> \]
!>
!> The row matrix is inverted and the relation for the next row is stored as
!>
!> \[
!> E_{r+1} = -M_r^{-1}A_r,\qquad
!> G_{r+1} = M_r^{-1}v_r.
!> \]
!>
!> Back substitution starts with the last row,
!>
!> \[
!> d_{NROWL}=G_{NROWL+1},
!> \]
!>
!> and proceeds upward with
!>
!> \[
!> d_r = E_{r+1}d_{r+1}+G_{r+1}.
!> \]
!>
!> The solved correction for each element is applied directly to water
!> level,
!>
!> \[
!> HRF_e^{n+1}=HRF_e^n+d_e.
!> \]
!>
!> Face flows are advanced with the same first-order linearisation used in
!> the matrix assembly:
!>
!> \[
!> Q_{e,f}^{n+1}=Q_{e,f}^n + DQ0ST_{e,f}d_e
!> + \sum_j DQIST_{e,f,j}d_j,
!> \]
!>
!> where a single neighbour uses `DQIST` and a multi-link junction expands
!> the neighbour sum through `ICMRF2` and `DQIST2`. [[OCFIX]] is then called
!> to remove spurious negative internal flows and adjust the corresponding
!> water levels. The static `ICMREF` and `ICMRF2` tables are passed in their
!> native layouts, without per-timestep reshaping or copying.
!>
!> `QOC` is copied from the internal face-flow array and converted from the
!> OC face convention to the model x/y convention by changing the sign on
!> faces 1 and 2. For each channel link, wetted area is interpolated from
!> the cross-section table. If \(H=HRF-ZGRUND\) falls between tabulated
!> depths \(H_m\) and \(H_i\),
!>
!> \[
!> \Delta H = H-H_m,\qquad
!> \Delta W = (W_i-W_m)\frac{\Delta H}{H_i-H_m},
!> \]
!>
!> \[
!> ARXL = XAREA_m + \left(W_m+\frac{1}{2}\Delta W\right)\Delta H.
!> \]
!>
!> Above the last table level, the link is extended with rectangular
!> bankfull width:
!>
!> \[
!> ARXL = XAREA_N + (HRF-ZBFULL)\,CWIDTH.
!> \]
!>
!> If `QMAX > 0`, all channel-link face flows are checked against this
!> maximum and a fatal diagnostic is issued when it is exceeded.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 3.4-4.2 | Developed the row-wise implicit solve, [[OCFIX]] flow-correction split, and current `OCABC` argument list. |
!> | 2009-01 | JE | - | Restructured the row loop for automatic differentiation. |
!> | 2026-08-20 | - | - | Passed `ICMREF` and `ICMRF2` directly to [[oc_node_solver:OCFIX]], removing the per-timestep topology staging. |
!> | 2026-08-22 | - | - | Deleted the per-timestep staging of `HRFZZ`/`QSAZZ` into and out of [[oc_node_solver:OCFIX]] buffers, which cost `10*total_no_elements` accessor calls and three round trips of the OC state. |
!> @endhistory
   SUBROUTINE OCSIM

      IMPLICIT NONE

      INTEGER :: I, IELs, IND, IROW, IBC, IBR, ICOD, IFACE, IHB, IM, IRSV
      INTEGER :: J, JEL, JND, JROW, K0, LINK, N, NCR, NPR, NSV

      DOUBLE PRECISION :: DDI, DH, DQ, DW, H, HI, HM, OCTIME, WI, WM, Z

      LOGICAL :: found_level, channel_blowup
      CHARACTER(36) :: MSG

      !----------------------------------------------------------------------*

      IF (.NOT. OCSIM_WORKSPACE%READY) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 1006, FID_logfile, 0, 0, 'OCSIM called before its workspace was initialised')
      END IF

      ASSOCIATE (AA => OCSIM_WORKSPACE%AA, DD => OCSIM_WORKSPACE%DD, &
                 FF => OCSIM_WORKSPACE%FF, BB => OCSIM_WORKSPACE%BB, &
                 GG => OCSIM_WORKSPACE%GG, CC => OCSIM_WORKSPACE%CC, &
                 EE => OCSIM_WORKSPACE%EE, TM1 => OCSIM_WORKSPACE%TM1, &
                 TM2 => OCSIM_WORKSPACE%TM2, TV1 => OCSIM_WORKSPACE%TV1, &
                 TV2 => OCSIM_WORKSPACE%TV2)
         !
         ! ----- Timestep setup
         DTOC = OCNEXT*3600.0D0

         ! ----- GET PRESCRIBED BOUNDARY VALUES HOCNOW & QOCF
         CALL OCEXT

         ! ----- CALCULATE FLOWS QSA & DERIVATIVES DQ0ST,DQIST,DQIST2
         CALL OCQDQ()

         ! ----- LOOP OVER ROWS, CALCULATING EE & GG
         NCR = 0

         row_loop: DO IROW = NROWF, NROWL
            IRSV = IROW + 1
            !
            ! NCR : NUMBER OF ELEMENTS IN THE CURRENT ROW
            ! NPR : NUMBER OF ELEMENTS IN THE PREVIOUS ROW
            ! NSV : NUMBER OF ELEMENTS IN THE NEXT (SUIVANT) ROW
            !
            NPR = NCR
            K0 = NROWST(IROW) - 1
            NCR = NROWST(IRSV) - 1 - K0

            IF (NCR == 0) CYCLE row_loop

            NSV = NROWST(MIN(IRSV, NROWL) + 1) - NROWST(IRSV)

            ! CALCULATE MATRICES AA, BB, CC, FF
            DO IND = 1, NCR
               iels = NROWEL(IND + K0)
               LINK = MAX(1, MIN(iels, total_no_links))
               IBC = NOCBCC(iels)

               IF (IBC > 0) THEN
                  IHB = NOCBCD(IBC, 4)
                  IBC = NOCBCD(IBC, 3)
               ELSE
                  IHB = 1
               END IF

               CALL OCABC(IND, IROW, iels, NSV, NCR, NPR, IBC, NXSECT(LINK), cellarea(iels), &
                          ZGRUND(iels), CLENTH(LINK), ZBFULL(LINK), GETHRF(iels), &
                          PNETTO(iels), QH(iels), ESWA(iels), HOCNOW(IHB), AA(1:nsv, IND), &
                          BB(1:ncr, IND), CC(1:npr, IND), FF(IND))
            END DO

            ! CALCULATE MATRIX TM2 (inverse of CC.EE+BB) AND VECTOR TV2 (FF-CC.GG)
            IF (IROW == NROWF) THEN
               DO IND = 1, NCR
                  TM2(1:ncr, IND) = BB(1:ncr, IND)
               END DO
               TV2(1:ncr) = FF(1:ncr)
            ELSE
               tm1(1:ncr, 1:ncr) = JEMATMUL_MM(cc(1:npr, 1:ncr), ee(1:ncr, 1:npr, irow), ncr, npr, ncr)
               tm2(1:ncr, 1:ncr) = bb(1:ncr, 1:ncr) + tm1(1:ncr, 1:ncr)
               tv1(1:ncr) = JEMATMUL_VM(cc(1:npr, 1:ncr), gg(1:npr, irow), ncr, npr)
               TV2(1:ncr) = FF(1:ncr) - TV1(1:ncr)
            END IF

            CALL INVERTMAT(TM2(1:ncr, 1:ncr), NCR, ICOD)

            ! Catch singular matrix inversion failure
            IF (ICOD == 1) THEN
               WRITE (MSG, '(A,I4)') 'Singular matrix at row', IROW
               CALL RAISE_ERROR(ERRLVL_fatal, 1018, FID_logfile, NROWEL(NROWST(IROW)), 0, MSG)
               RETURN
            END IF

            ! CALCULATE MATRIX EE(IROW+1)
            IF (IROW /= NROWL) THEN
               ee(1:nsv, 1:ncr, irsv) = JEMATMUL_MM(tm2(1:ncr, 1:ncr), aa(1:nsv, 1:ncr), ncr, ncr, nsv)
               ee(1:nsv, 1:ncr, irsv) = -ee(1:nsv, 1:ncr, irsv)
            END IF

            ! CALCULATE VECTOR GG(IROW+1)
            gg(1:ncr, irsv) = JEMATMUL_VM(tm2(1:ncr, 1:ncr), tv2(1:ncr), ncr, ncr)

         END DO row_loop

         ! ----- DOWNWARDS SWEEP, CALCULATION OF DD
         !
         ! * last row first (use NCR,IRSV from loop above)
         IROW = NROWL
         DD(1:ncr, IROW) = GG(1:ncr, IRSV)

         ! * loop over remaining rows
         DO IROW = NROWL - 1, NROWF, -1
            IRSV = IROW + 1
            NSV = NCR
            NCR = NROWST(IRSV) - NROWST(IROW)

            tv1(1:ncr) = JEMATMUL_VM(ee(1:nsv, 1:ncr, irsv), dd(1:nsv, irsv), ncr, nsv)
            dd(1:ncr, irow) = tv1(1:ncr) + gg(1:ncr, irsv)
         END DO

         ! ----- ADVANCE WATER LEVELS AND FLOWS TO TIME LEVEL N+1,
         !       USING FIRST ORDER DERIVATIVES OF FLOWS AT TIME LEVEL N
         DO iels = 1, total_no_elements
            IND = NELIND(iels)
            IROW = ICMREF(iels, 3)
            DDI = DD(IND, IROW)
            CALL SETHRF(iels, GETHRF(iels) + DDI)

            DO IFACE = 1, 4
               DQ = DQ0ST(iels, IFACE)*DDI
               JEL = ICMREF(iels, IFACE + 4)

               IF (JEL > 0) THEN
                  JND = NELIND(JEL)
                  JROW = ICMREF(JEL, 3)
                  DQ = DQIST(iels, IFACE)*DD(JND, JROW) + DQ

               ELSE IF (JEL < 0) THEN
                  IBR = -JEL
                  DO J = 1, 3
                     JEL = ICMRF2(IBR, J)
                     IF (JEL > 0) THEN
                        JND = NELIND(JEL)
                        JROW = ICMREF(JEL, 3)
                        DQ = DQIST2(IBR, J)*DD(JND, JROW) + DQ
                     END IF
                  END DO
               END IF

               CALL SETQSA(iels, IFACE, GETQSA(iels, IFACE) + DQ)
            END DO
         END DO

         ! CHECK FOR SPURIOUS NEGATIVE FLOWS, AND RECALCULATE WATER LEVELS
         ! IF REQUIRED.  NB. DOES NOT CHECK BOUNDARY FLOWS
         !
         ! [[oc_node_solver:OCFIX]] corrects `HRFZZ`/`QSAZZ` in place. The former
         ! staging of the whole OC state into `inhrf`/`inqsa` and back out of
         ! `GGGETHRF`/`GGGETQSA` through the element accessors existed only for
         ! tangent debugging; see the AD note in [[oc_node_solver:OCFIX]] for how to
         ! reinstate an argument-passed form for an AD build without paying for
         ! it here.
         CALL OCFIX(ICMREF, ICMRF2, total_no_elements, dtoc)

         ! SET FLOWS QOC (POSITIVE X,Y) FOR USE BY OTHER COMPONENTS
         !
         ! Faces 1 and 2 point in -x/-y in the OC sign convention, so they are
         ! negated here. Written column by column so that each column is a
         ! single contiguous pass over `QSAZZ` with no array temporary.
         qoc(1:total_no_elements, 1) = -qsazz(1:total_no_elements, 1)
         qoc(1:total_no_elements, 2) = -qsazz(1:total_no_elements, 2)
         qoc(1:total_no_elements, 3) = qsazz(1:total_no_elements, 3)
         qoc(1:total_no_elements, 4) = qsazz(1:total_no_elements, 4)

         ! ----- CALCULATE CROSS-SECTIONAL AREA OF CHANNEL WATER
         link_loop: DO iels = 1, total_no_links
            Z = GETHRF(iels)
            H = Z - ZGRUND(iels)
            N = NXSECT(iels)
            found_level = .FALSE.

            sect_loop: DO I = 2, N
               HI = XINH(iels, I)
               IF (H < HI) THEN
                  IM = I - 1
                  HM = XINH(iels, IM)
                  WM = XINW(iels, IM)
                  WI = XINW(iels, I)
                  DH = H - HM
                  DW = (WI - WM)*(DH/(HI - HM))
                  ARXL(iels) = XAREA(iels, IM) + (WM + 0.5D0*DW)*DH
                  found_level = .TRUE.
                  EXIT sect_loop
               END IF
            END DO sect_loop

            IF (.NOT. found_level) THEN
               ARXL(iels) = XAREA(iels, N) + (Z - ZBFULL(iels))*CWIDTH(iels)
            END IF
         END DO link_loop

         ! ----- Print results
         OCTIME = OCNOW + OCNEXT
         IF ((OCTIME >= TDC) .AND. (OCTIME <= TFC)) CALL OCPRI(OCTIME, ARXL, QOC)

         ! ----- CHECK FOR CHANNEL BLOW-UP
         channel_blowup = .FALSE.
         IF (GTZERO(QMAX)) THEN
            blowup_loop: DO iels = 1, total_no_links
               DO IFACE = 1, 4
                  IF (ABS(QOC(iels, IFACE)) > QMAX) THEN
                     channel_blowup = .TRUE.
                     EXIT blowup_loop
                  END IF
               END DO
            END DO blowup_loop
         END IF

         IF (channel_blowup) THEN
            MSG = 'CHANNEL FLOWS EXCEED MAXIMUM ALLOWED'
            CALL RAISE_ERROR(ERRLVL_fatal, 1029, FID_logfile, iels, 0, MSG)
         END IF

      END ASSOCIATE

   END SUBROUTINE OCSIM

END MODULE oc_driver

