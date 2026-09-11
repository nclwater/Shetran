!> summary: The tridiagonal pressure-head correction for one column.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[VSCOLM]] assembles the coupled problem for one element column and solves
!> it with [[linear_algebra:TRIDAG]]. [[VSCOEF]] forms the conductance
!> coefficients, [[VSBC]] applies the boundary conditions, and [[VSFUNC]]
!> interpolates the soil lookup tables of [[vs_soil_tables]]. The source terms
!> — wells, springs, interception and the boundary inflows — come from
!> [[vs_sources]].
!>
!> `errcntallowed` is the convergence-warning limit, and it is here rather than
!> in [[vs_config]] because [[VSCOLM]] is its only reader; putting it with the
!> other configuration would make [[vs_driver]] and this module depend on each
!> other. See `docs/rename/constants_review.md`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995--1998 | GP / RAH | 4.0--4.2 | Created the VSS component and its `.INC` include groups. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS Fortran sources into a single Fortran 90 module. |
!> | 2026-03 to 2026-05 | SB / SvB | 4.6 | Modernisation pass, and moved `VSREAD`'s read buffers to allocatable module state to avoid a stack-related crash. |
!> | 2026-09-10 | SvB | - | Split out of VSmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_column_solver

   USE MOD_PARAMETERS, ONLY: half, one, zero
   USE array_limits, ONLY: LLEE, NLYREE, NSEE
   USE vs_soil_tables, ONLY: NSOLEE, NVSSOL, VSPDET, VSPDKR, VSPDTH, VSPETA, VSPKR, &
                             VSPPSI, VSPTHE
   USE file_units, ONLY: FID_logfile
   USE vs_sources, ONLY: VSINTC, VSLOWR, VSSAI, VSSPR, VSUPPR, VSWELL
   USE float_compare, ONLY: isone, iszero, notone
   USE linear_algebra, ONLY: TRIDAG
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: VSCOLM, VSFUNC
   PUBLIC :: errcntallowed

   integer, parameter :: errcntallowed = 1000 !! Maximum repeated VSS convergence warnings.

CONTAINS

!> Adds user-defined lateral boundary-condition terms to a column system.
!>
!> `VSBC` applies the manual `VS14`-`VS16` lateral boundary categories to one
!> face of the column currently being assembled by [[vscolm]]. `JCBC` selects
!> the lateral boundary type:
!>
!> | `JCBC` | Manual boundary type | Implementation |
!> |:-------|:---------------------|:---------------|
!> | 3 | prescribed lateral flow | Implemented. |
!> | 4 | prescribed lateral head | Implemented. |
!> | 5 | prescribed lateral head gradient | Recognised, but only prints an unfinished-code message. |
!>
!> `FACE` must be in `1:4`, `ICBOT:ICTOP` must bound the active cells, `CDELL`
!> must be positive, and each active cell must have positive `CDELZ` and
!> `CKIJ`. If `ICLFN` or `ICLHN` is zero the corresponding boundary value is
!> applied across the full active column; otherwise `ICLFL` or `ICLHL` selects
!> model layers whose cell bounds are supplied by `ICLYRB`. The declared array
!> bounds behind those conditions are `ICTOP <= LLEE` (the size of `DUM`) and
!> `ICLFN`, `ICLHN <= NLYREE` (the sizes of `ICLFL`/`CLF` and
!> `ICLHL`/`CLH`/`DUM`). Each selected layer index must satisfy
!> `1 <= ICLFL(i) < NLYREE` (likewise `ICLHL(i)`), with the corresponding
!> `ICLYRB` bounds inside `ICBOT:ICTOP+1`.
!>
!> For a prescribed lateral flow category (`JCBC = 3`), the total input flow
!> `CLF(i)` for the selected layer interval is partitioned between cells in
!> proportion to
!>
!> \[
!>   T_c = CKIJ_c\,\Delta z_c,\qquad
!>   Q_c = {T_c\over\sum T_c}\,CLF_i .
!> \]
!>
!> The cell contribution is inserted as `CR(c) = CR(c) - Q_c` and stored in
!> `CQH(FACE,c)`.
!>
!> @note
!> The transmissive-thickness sum is used as a divisor without a zero check.
!> Active type-3 boundary intervals must therefore include at least one cell
!> with positive `CKIJ(c) * CDELZ(c)`.
!> @endnote
!>
!> For a prescribed lateral head category (`JCBC = 4`), the boundary value
!> `CLH(i)` is interpreted as an elevation when `BCHELE` is true, or as a depth
!> below ground when false:
!>
!> \[
!>   H_b =
!>   \begin{cases}
!>     CLH_i, & BCHELE,\\
!>     CZG - CLH_i, & \text{otherwise}.
!>   \end{cases}
!> \]
!>
!> For each selected cell,
!>
!> \[
!>   A/L = CAIJ(FACE,c)/CDELL,\qquad
!>   \Delta h = (H_b - CZ_c - CPSI_c)(A/L),
!> \]
!>
!> \[
!>   Q_c = CKIJ_c\,\Delta h .
!> \]
!>
!> The linearised contribution is added to the tridiagonal diagonal and
!> right-hand side as
!>
!> \[
!>   CB_c \leftarrow CB_c + CDKIJ_c\,\Delta h + CKIJ_c(A/L),\qquad
!>   CR_c \leftarrow CR_c - Q_c .
!> \]
!>
!> `CQH(FACE,c)` stores the diagnostic lateral boundary flux.
!>
!> @warning
!> `JCBC = 5` only prints `unfinished code for boundary type 5 - head
!> gradients`; it does not add matrix terms, source terms, or diagnostic fluxes.
!> @endwarning
!>
!> @note
!> The `ICLHL`/`ICLHN` argument order was swapped relative to the historical
!> `.F`-era signature during 2026 modernisation, and the call in [[vscolm]] was
!> updated to match; this is a pure reordering with no behavioural change.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written; version 4.0 completed 1995-08-08. |
!> | 1997-01-20 | RAH | 4.1 | Removed leading comments and lower-case code; combined `IF`-blocks; used generic intrinsics. |
!> | 1997-01-27 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks; reused `DUM` in place of the separate `TDUM`/`HDUM` workspaces. |
!> | 1997-05-14 | RAH | 4.1 | Scrapped the `CDQH` workspace argument and set `CB`/`CR` directly; stopped initialising `CQH` here (now the caller's job, see [[vscolm]]); added local `QTOT`; added argument `FACE` (`1:4`) and a leading dimension to `CAIJ` and `CQH`. |
!> | 1997-08-13 | RAH | 4.1 | Corrected the `CLF` and `DUM` subscripts to use `I` rather than `ILYR`. |
!> | 2026-04-06/07 | SvB | 4.6 | Swapped the `ICLHL`/`ICLHN` argument order relative to the historical `.F`-era signature, updating the call in [[vscolm]] to match; a pure reordering with no behavioural change. |
!> @endhistory
   SUBROUTINE VSBC(BCHELE, FACE, ICBOT, ICTOP, JCBC, ICLYRB, ICLFN, &
                   ICLFL, ICLHL, ICLHN, CZG, CDELL, CDELZ, CZ, CAIJ, CLF, CLH, CPSI, &
                   CKIJ, CDKIJ, CB, CR, CQH, DUM)

      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: BCHELE                    !! True when `CLH` values are elevations; false when they are depths below ground.
      INTEGER, INTENT(IN) :: FACE                      !! Boundary face number, in `1:4`.
      INTEGER, INTENT(IN) :: ICBOT                     !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                     !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: JCBC                      !! Lateral boundary type for this face.
      INTEGER, INTENT(IN) :: ICLYRB(*)                 !! Bottom-cell bounds for model-layer intervals.
      INTEGER, INTENT(IN) :: ICLFN                     !! Number of selected lateral-flow layers; zero means full active column.
      INTEGER, INTENT(IN) :: ICLFL(*)                  !! Selected model layers for type-3 lateral-flow categories.
      INTEGER, INTENT(IN) :: ICLHL(*)                  !! Selected model layers for type-4 lateral-head categories.
      INTEGER, INTENT(IN) :: ICLHN                     !! Number of selected lateral-head layers; zero means full active column.
      DOUBLE PRECISION, INTENT(IN) :: CZG               !! Ground elevation used to convert depth-style head boundaries.
      DOUBLE PRECISION, INTENT(IN) :: CDELL             !! Distance scale normal to the boundary face.
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Cell thicknesses.
      DOUBLE PRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP)   !! Cell-node elevations.
      DOUBLE PRECISION, INTENT(IN) :: CAIJ(4, ICBOT:ICTOP) !! Face areas by face and cell.
      DOUBLE PRECISION, INTENT(IN) :: CLF(*)            !! Prescribed lateral-flow boundary values.
      DOUBLE PRECISION, INTENT(IN) :: CLH(*)            !! Prescribed lateral-head or depth boundary values.
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Current pressure heads.
      DOUBLE PRECISION, INTENT(IN) :: CKIJ(ICBOT:ICTOP) !! Current lateral hydraulic conductivity terms.
      DOUBLE PRECISION, INTENT(IN) :: CDKIJ(ICBOT:ICTOP) !! Derivatives of `CKIJ` with respect to pressure head.

      ! In+out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: CB(ICBOT:ICTOP) !! Matrix diagonal terms updated with lateral boundary contributions.
      DOUBLE PRECISION, INTENT(INOUT) :: CR(ICBOT:ICTOP) !! Right-hand side terms updated with lateral boundary fluxes.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT)   :: CQH(4, ICBOT:ICTOP) !! Diagnostic lateral boundary fluxes for the selected face.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: DUM(*)         !! Workspace for transmissive-thickness weights or converted boundary heads.

      ! Locals
      INTEGER :: ICL, I, ILYR, ICL1, ICL2, IDUM, SGN
      DOUBLE PRECISION :: ADHOL, AOL, KDUM, Q, QTOT, TICL, TTOT, ZDUM

      !----------------------------------------------------------------------*

      ! flow (type 3)
      IF (JCBC == 3) THEN
         flow_loop: DO I = 1, MAX(1, ICLFN)
            IF (ICLFN == 0) THEN
               ICL1 = ICBOT
               ICL2 = ICTOP
            ELSE
               ILYR = ICLFL(I)
               ICL1 = ICLYRB(ILYR)
               ICL2 = ICLYRB(ILYR + 1) - 1
            END IF

            TTOT = 0.0D0

            calc_ttot_loop: DO ICL = ICL1, ICL2
               TICL = CKIJ(ICL)*CDELZ(ICL)
               DUM(ICL) = TICL
               TTOT = TTOT + TICL
            END DO calc_ttot_loop

            QTOT = CLF(I)

            distribute_flow_loop: DO ICL = ICL1, ICL2
               Q = (DUM(ICL)/TTOT)*QTOT
               CR(ICL) = CR(ICL) - Q
               CQH(FACE, ICL) = Q
            END DO distribute_flow_loop

         END DO flow_loop

         ! head (type 4)
         ! NB. If BCHELE=.false., head b.c.'s are depths below ground surface
      ELSE IF (JCBC == 4) THEN
         IF (BCHELE) THEN
            ZDUM = ZERO
            SGN = 1
         ELSE
            ZDUM = CZG
            SGN = -1
         END IF

         IDUM = MAX(ICLHN, 1)

         head_init_loop: DO I = 1, IDUM
            DUM(I) = ZDUM + DBLE(SGN)*CLH(I)
         END DO head_init_loop

         head_calc_loop: DO I = 1, IDUM
            IF (ICLHN == 0) THEN
               ICL1 = ICBOT
               ICL2 = ICTOP
            ELSE
               ILYR = ICLHL(I)
               ICL1 = ICLYRB(ILYR)
               ICL2 = ICLYRB(ILYR + 1) - 1
            END IF

            apply_head_loop: DO ICL = ICL1, ICL2
               AOL = CAIJ(FACE, ICL)/CDELL
               ADHOL = (DUM(I) - CZ(ICL) - CPSI(ICL))*AOL
               KDUM = CKIJ(ICL)
               Q = KDUM*ADHOL

               CB(ICL) = CB(ICL) + CDKIJ(ICL)*ADHOL + KDUM*AOL
               CR(ICL) = CR(ICL) - Q
               CQH(FACE, ICL) = Q
            END DO apply_head_loop
         END DO head_calc_loop

         ! head gradient (type 5)
      ELSE IF (JCBC == 5) THEN
         !STOP 'unfinished code for boundary type 5 - head gradients'
         PRINT *, 'unfinished code for boundary type 5 - head gradients'
      END IF

   END SUBROUTINE VSBC

!> Assembles internal vertical and lateral coefficients for a VSS column.
!>
!> `VSCOEF` builds the internal conductance terms used by [[vscolm]] when it
!> assembles the tridiagonal pressure-head correction system. It uses the
!> manual w-mean controls `VSWV` and `VSWL` (passed as `CWV` and `CWL`) to
!> average vertical and lateral hydraulic conductivity. A value of zero selects
!> the weighted harmonic vertical special case, a value of one gives an
!> arithmetic mean, and other positive values use the general w-mean.
!>
!> Required entry conditions are those established by [[vsconc]] and [[vssim]]:
!> `1 <= ICBOT <= ICTOP <= LLEE`; `CA0`, `CWL`, cell thicknesses `CDELZ`, cell
!> relative conductivities `CKR`, and saturated conductivities `VSK3D` are
!> positive; each `ICSOIL` is in `1:NSEE`; and any active lateral neighbour
!> referenced by `JCACN`/`JCDEL` has valid cell indices, face areas, distances,
!> and neighbour conductivities. `CDELL(j)+CDELL1(j)` must be positive on each
!> face.
!>
!> For vertical flow between cells \(i-1\) and \(i\), with cell area \(A\),
!> thicknesses \(\Delta z\), relative conductivity \(K_r\), saturated vertical
!> conductivity \(K_z\), and \(K_i=K_{r,i}K_{z,i}\), the stored inter-cell
!> conductance `CBETM(i)` is:
!>
!> \[
!>   \beta_i =
!>   \begin{cases}
!>     {C_{i-1}C_i\over C_{i-1}+C_i},
!>       & CWV=0,\quad C_i={2AK_i\over\Delta z_i},\\
!>     {A(K_{i-1}+K_i)\over \Delta z_{i-1}+\Delta z_i},
!>       & CWV=1,\\
!>     {2A\over\Delta z_{i-1}+\Delta z_i}
!>       \left({K_{i-1}^{CWV}+K_i^{CWV}\over2}\right)^{1/CWV},
!>       & \text{otherwise}.
!>   \end{cases}
!> \]
!>
!> `CDBETM` and `CDBTMM` store the derivatives of that conductance with
!> respect to the lower and upper cell conductivities, using `CDKR` from
!> [[vsfunc]]. The per-cell vertical contribution is
!>
!> \[
!>   CF_i = \beta_i+\beta_{i+1},\qquad
!>   CDF_i = {d\beta_i\over d\psi_i}+{d\beta_{i+1}\over d\psi_i}.
!> \]
!>
!> For lateral faces, `CKIJ(i,j)=K_rK_{sat,j}` and `CDKIJ` stores its
!> derivative. If a neighbour is active and the face is not handled as
!> stream-aquifer interaction (`JCBC(j) /= 9`), the routine constructs lateral
!> conductances `CGAM1` and, for split-cell connections, `CGAM2`, using the
!> current cell area `CAIJ`, neighbour areas `CAIJ1`, face distance
!> `CDELL+CDELL1`, and the lateral w-mean `CWL`. These lateral conductances and
!> their derivatives are added into `CF` and `CDF`; [[vscoef]] leaves the
!> boundary-specific terms to [[vsbc]], [[vssai]], and [[vslowr]].
!>
!> The lateral split factors are:
!>
!> | Quantity | Definition | Effect |
!> |:---------|:-----------|:-------|
!> | `NIJ = abs(JCDEL(j,i)) + 1` | Number of current-column pieces represented by the neighbour area term. | Divides neighbour conductance-area products. |
!> | `NKJ = abs(JCDEL1(k,j)) + 1` | Number of neighbour-column pieces represented by the current cell face. | Divides current face area. |
!> | `CGAM1` | Conductance to neighbour cell `k`. | Always present for an active lateral connection. |
!> | `CGAM2` | Conductance to neighbour cell `k + JCDEL1(k,j)`. | Zero by construction when `JCDEL1(k,j)=0`; otherwise represents the split-cell second neighbour. |
!>
!> In detail, an active lateral connection `j` of cell `i` (one with
!> `JELDUM(j) > 0`, `JCACN(j,i) /= 0`, and `JCBC(j) /= 9`) must satisfy
!> `1 <= k, k1 <= LLEE` and `|JCDEL(j,i)|, |JCDEL1(k,j)| <= 1`, with positive
!> `CAIJ(j,i)`, `CAIJ1(k,j)`, `CAIJ1(k1,j)`, `CKIJ1(k,j)`, and `CKIJ1(k1,j)`,
!> where `k = JCACN(j,i)` and `k1 = k + JCDEL1(k,j)`. `VSK3D(ICSOIL(i),1:3)`
!> must be positive for every active cell.
!>
!> @note
!> `CKIJ` and `CDKIJ` are set for every local cell and face. `CGAM1/2` and
!> `CDGAM1/2` are assigned only when `JCACN(j,i) /= 0`, `JELDUM(j) >= 1`, and
!> `JCBC(j) /= 9`; callers should only use those arrays on the same active
!> lateral-connection mask.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written; version 4.0 completed 1995-12-20. |
!> | 1996-12-28 | RAH | 4.1 | Removed leading comments; removed arguments `IEL` and `NIT`; added arguments `CWV` and `CWL` (previously in `VSCOLM.INC`). |
!> | 1997-01-15 | RAH | 4.1 | Dispensed with the `VSCOLM.INC` arrays `CKZ`/`CDKZ`; rewrote the vertical sections to use fewer operations and to stop overwriting `CDELZ`. |
!> | 1997-01-16 | RAH | 4.1 | Rewrote the lateral sections in the same style, fixing an error in `CDGAM*` when `CWL /= 1`; removed lower-case code. |
!> | 1997-01-22 | RAH | 4.1 | Passed data through arguments instead of `COMMON`. |
!> | 1997-01-23 | RAH | 4.1 | Scrapped the outputs `CBETP`, `CDBETP`, `CDBTPP`, `CDFM`, `CDFP`, `CG`, and `CDG`. |
!> | 1997-05-13 | RAH | 4.1 | Swapped the `JCACN`, `JCDEL`, and `CAIJ` indices; renamed the local `DUM`; replaced `CKZS`/`CKIJS` with the new arguments `NSEE`, `ICSOIL`, and `VSK3D`. |
!> @endhistory
   SUBROUTINE VSCOEF(LLEE, NSEE, CWV, CWL, VSK3D, ICBOT, ICTOP, &
                     JELDUM, JCBC, ICSOIL, JCACN, JCDEL, JCDEL1, CA0, CDELL, CDELL1, &
                     CDELZ, CAIJ, CAIJ1, CKR, CDKR, CKIJ1, CBETM, CDBETM, CDBTMM, CF, &
                     CDF, CKIJ, CDKIJ, CGAM1, CGAM2, CDGAM1, CDGAM2, C, D)

      ! Assumed external module dependencies providing global variables:
      ! zero, one, half, ISZERO, ISONE, NOTONE

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE                  !! Declared cell dimension for column and neighbour arrays.
      INTEGER, INTENT(IN) :: NSEE                  !! Declared soil-type dimension for conductivity arrays.
      INTEGER, INTENT(IN) :: ICBOT                 !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                 !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: JELDUM(4)             !! Adjacent element id by face; values below 1 disable lateral coupling.
      INTEGER, INTENT(IN) :: JCBC(4)               !! Boundary type by face; type 9 is handled outside regular lateral coupling.
      INTEGER, INTENT(IN) :: ICSOIL(ICBOT:ICTOP)   !! Soil type by active cell.
      INTEGER, INTENT(IN) :: JCACN(4, ICBOT:ICTOP)  !! Adjacent-cell index by face and active cell; zero means no lateral connection.
      INTEGER, INTENT(IN) :: JCDEL1(LLEE, 4)        !! Neighbour-column split offset used to find a second connected neighbour cell.
      INTEGER, INTENT(IN) :: JCDEL(4, ICBOT:ICTOP)  !! Current-column split indicator for lateral area weighting.
      DOUBLE PRECISION, INTENT(IN) :: CWV           !! Vertical hydraulic-conductivity w-mean control.
      DOUBLE PRECISION, INTENT(IN) :: CWL           !! Lateral hydraulic-conductivity w-mean control.
      DOUBLE PRECISION, INTENT(IN) :: VSK3D(NSEE, 3) !! Saturated hydraulic conductivity by soil type and x/y/z direction.
      DOUBLE PRECISION, INTENT(IN) :: CA0           !! Plan area of the current element.
      DOUBLE PRECISION, INTENT(IN) :: CDELL(4)      !! Current-element lateral distance scale by face.
      DOUBLE PRECISION, INTENT(IN) :: CDELL1(4)     !! Adjacent-element lateral distance scale by face.
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Active-cell thicknesses.
      DOUBLE PRECISION, INTENT(IN) :: CAIJ(4, ICBOT:ICTOP) !! Current-element lateral face areas.
      DOUBLE PRECISION, INTENT(IN) :: CAIJ1(LLEE, 4) !! Adjacent-element lateral face areas.
      DOUBLE PRECISION, INTENT(IN) :: CKR(ICBOT:ICTOP) !! Current relative hydraulic conductivity by active cell.
      DOUBLE PRECISION, INTENT(IN) :: CDKR(ICBOT:ICTOP) !! Derivative of `CKR` with respect to pressure head.
      DOUBLE PRECISION, INTENT(IN) :: CKIJ1(LLEE, 4) !! Adjacent-cell lateral hydraulic conductivity terms.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CBETM(ICBOT:ICTOP + 1) !! Vertical inter-cell conductance below each active cell.
      DOUBLE PRECISION, INTENT(OUT) :: CDBETM(ICBOT:ICTOP + 1) !! Derivative of `CBETM` with respect to the lower cell.
      DOUBLE PRECISION, INTENT(OUT) :: CDBTMM(ICBOT:ICTOP + 1) !! Derivative of `CBETM` with respect to the upper cell.
      DOUBLE PRECISION, INTENT(OUT) :: CF(ICBOT:ICTOP) !! Internal conductance contribution to the column diagonal.
      DOUBLE PRECISION, INTENT(OUT) :: CDF(ICBOT:ICTOP) !! Derivative of `CF` with respect to pressure head.
      DOUBLE PRECISION, INTENT(OUT) :: CKIJ(LLEE, 4)  !! Current-cell lateral hydraulic conductivity terms.
      DOUBLE PRECISION, INTENT(OUT) :: CDKIJ(LLEE, 4) !! Derivatives of `CKIJ` with respect to pressure head.
      DOUBLE PRECISION, INTENT(OUT) :: CGAM1(LLEE, 4) !! Primary lateral coupling conductance to adjacent cells.
      DOUBLE PRECISION, INTENT(OUT) :: CGAM2(LLEE, 4) !! Secondary split-cell lateral coupling conductance.
      DOUBLE PRECISION, INTENT(OUT) :: CDGAM1(LLEE, 4) !! Derivative of `CGAM1` with respect to local pressure head.
      DOUBLE PRECISION, INTENT(OUT) :: CDGAM2(LLEE, 4) !! Derivative of `CGAM2` with respect to local pressure head.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(OUT) :: C(ICBOT:ICTOP) !! Workspace for local conductivity products.
      DOUBLE PRECISION, INTENT(OUT) :: D(ICBOT:ICTOP) !! Workspace for local conductivity derivatives.

      ! Locals
      INTEGER :: DELKJ, I, J, K, K1, M, NIJ, NKJ, NKJM1, P
      DOUBLE PRECISION :: AIJDUM, AREA2, C1, C2, CAVE, CI, CIJ, CKJ, CK1J, CM, Casum
      DOUBLE PRECISION :: D1, D2, DIJ, AODZ, KSAODZ, DXDUM, RCI, RCM, WI, WIM1, WO2DX
      DOUBLE PRECISION :: KIJ, DKIJ, GAM1, GAM2, DGAM1, DGAM2, CKIJS, CKZS
      LOGICAL :: TEST

      !----------------------------------------------------------------------*

      ! vertical conductivity terms (CBETM,CDB*)
      CBETM(ICBOT) = zero
      CDBETM(ICBOT) = zero
      CDBTMM(ICBOT) = zero

      IF (ISZERO(CWV)) THEN
         ! Special case: weighted harmonic mean
         AREA2 = CA0*2.0d0
         DO I = ICBOT, ICTOP
            CKZS = VSK3D(ICSOIL(I), 3)
            KSAODZ = CKZS*AREA2/CDELZ(I)
            C(I) = CKR(I)*KSAODZ
            D(I) = CDKR(I)*KSAODZ
         END DO

         DO I = ICBOT + 1, ICTOP
            M = I - 1
            CM = C(M)
            CI = C(I)
            Casum = CM + CI
            RCM = CM/Casum
            RCI = CI/Casum
            CBETM(I) = CI*RCM
            CDBETM(I) = D(I)*RCM**2
            CDBTMM(I) = D(M)*RCI**2
         END DO

      ELSE IF (ISONE(CWV)) THEN
         ! Arithmetic mean
         DO I = ICBOT, ICTOP
            CKZS = VSK3D(ICSOIL(I), 3)
            C(I) = CKR(I)*CKZS
            D(I) = CDKR(I)*CKZS
         END DO

         DO I = ICBOT + 1, ICTOP
            M = I - 1
            AODZ = CA0/(CDELZ(M) + CDELZ(I))
            CBETM(I) = AODZ*(C(M) + C(I))
            CDBETM(I) = AODZ*D(I)
            CDBTMM(I) = AODZ*D(M)
         END DO

      ELSE
         ! General w-mean
         WI = one/CWV
         WIM1 = (one - CWV)/CWV

         DO I = ICBOT, ICTOP
            CKZS = VSK3D(ICSOIL(I), 3)
            C(I) = (CKR(I)*CKZS)**CWV
            D(I) = CDKR(I)*CKZS
         END DO

         DO I = ICBOT + 1, ICTOP
            M = I - 1
            CM = C(M)
            CI = C(I)
            CAVE = 0.5d0*(CM + CI)
            AODZ = CA0/(CDELZ(M) + CDELZ(I))
            CBETM(I) = AODZ*CAVE**WI*2.0d0
            CDBETM(I) = AODZ*(CAVE/CI)**WIM1*D(I)
            CDBTMM(I) = AODZ*(CAVE/CM)**WIM1*D(M)
         END DO

      END IF

      I = ICTOP + 1
      CBETM(I) = zero
      CDBETM(I) = zero
      CDBTMM(I) = zero

      ! vertical components of coefficients  NB lateral components added later
      DO I = ICBOT, ICTOP
         P = I + 1
         CF(I) = CBETM(I) + CBETM(P)
         CDF(I) = CDBETM(I) + CDBTMM(P)
      END DO

      ! loop over each face
      WI = one/CWL
      WIM1 = (one - CWL)/CWL

      face_loop: DO J = 1, 4
         M = 1 + MOD(J - 1, 2)
         TEST = (JELDUM(J) < 1) .OR. (JCBC(J) == 9)
         DXDUM = CDELL(J) + CDELL1(J)
         WO2DX = half*CWL/DXDUM

         internal_cell_loop: DO I = ICBOT, ICTOP
            ! lateral conductivity terms
            CKIJS = VSK3D(ICSOIL(I), M)
            KIJ = CKR(I)*CKIJS
            DKIJ = CDKR(I)*CKIJS
            CKIJ(I, J) = KIJ
            CDKIJ(I, J) = DKIJ

            ! lateral components of all coefficients
            K = JCACN(J, I)

            ! Cycle directly replaces GOTO 300
            IF (K == 0 .OR. TEST) CYCLE internal_cell_loop

            NIJ = ABS(JCDEL(J, I)) + 1
            DELKJ = JCDEL1(K, J)
            K1 = K + DELKJ
            NKJM1 = ABS(DELKJ)
            NKJ = NKJM1 + 1

            CKJ = CKIJ1(K, J)*CAIJ1(K, J)/DBLE(NIJ)
            CK1J = CKIJ1(K1, J)*CAIJ1(K1, J)/DBLE(NIJ)
            AIJDUM = CAIJ(J, I)/DBLE(NKJ)
            DIJ = DKIJ*AIJDUM*WO2DX
            CIJ = KIJ*AIJDUM

            C1 = half*(CIJ + CKJ)
            C2 = half*(CIJ + CK1J)
            D1 = one
            D2 = one

            IF (NOTONE(CWL)) THEN
               CIJ = CIJ**CWL
               CKJ = CKJ**CWL
               CK1J = CK1J**CWL
               D1 = (C1/CIJ)**WIM1
               D2 = (C2/CIJ)**WIM1
               C1 = C1**WI
               C2 = C2**WI
            END IF

            GAM1 = C1/DXDUM
            GAM2 = C2/DXDUM*DBLE(NKJM1)
            DGAM1 = D1*DIJ
            DGAM2 = D2*DIJ*DBLE(NKJM1)

            CGAM1(I, J) = GAM1
            CGAM2(I, J) = GAM2
            CDGAM1(I, J) = DGAM1
            CDGAM2(I, J) = DGAM2

            CF(I) = CF(I) + GAM1 + GAM2
            CDF(I) = CDF(I) + DGAM1 + DGAM2

         END DO internal_cell_loop
      END DO face_loop

   END SUBROUTINE VSCOEF

!> Solves the variably saturated flow equations for one element column.
!>
!> `VSCOLM` is the local nonlinear solve used by [[vssim]] for one active
!> vertical column. It updates pressure head `CPSI`, water content `CTHETA`,
!> relative conductivity `CKR`, vertical flux `CQV`, lateral flux `CQH`, well
!> flux `CQWI`, spring flux `CQSP`, and phreatic-surface level `CPSL`.
!>
!> Required entry conditions are established by [[vsconc]], [[vsconl]], and
!> [[vssim]]: `1 <= ICBOT <= ICSPCE, ICWLBT, ICWLTP <= ICTOP < LLEE`, with
!> `ICWLBT <= ICWLTP`; face boundary codes are limited to internal/no-flow
!> (`0`), lateral flow/head/gradient (`3:5`), or stream-aquifer interaction
!> (`9` or `10`); lateral boundary faces have no regular neighbour in
!> `JELDUM`; type `9` faces have no internal lateral cell connectivity; and
!> type `10` stream-aquifer faces have no connectivity above the river-bed cell
!> `ICBED`. `CQWI`/`CQWIN` are meaningful only for well columns
!> (`JCBC(5)=1`), while `ICSPCE`, `CCS`, `CQSP`, and `CZSP` are meaningful only
!> for spring columns (`JCBC(5)=2`). `ICBED`, `ICBOT`, `ICLFL`, `ICLFN`,
!> `ICLHL`, `ICLHN`, `ICLYRB`, `ICTOP`, `JCACN`, `JCBC`, and `JELDUM` are static
!> functions of `IEL`, fixed once by the setup phase and unchanged thereafter.
!>
!> For each local iteration, the routine:
!>
!> | Step | Routine/action |
!> |:-----|:---------------|
!> | Hydraulic functions | [[vsfunc]] interpolates \(\theta\), storage, \(K_r\), and derivatives from the [[vssoil]] tables. |
!> | Internal coefficients | [[vscoef]] builds vertical and lateral conductance terms. |
!> | Matrix assembly | [[vsintc]] forms the tridiagonal arrays `CA`, `CB`, `CC`, and `CR`. |
!> | Upper boundary | [[vsuppr]] applies infiltration/exfiltration from surface water. |
!> | Well or spring | [[vswell]] or [[vsspr]] adds type 1 or 2 source/sink terms. |
!> | Lateral/stream boundaries | [[vsbc]] handles manual lateral boundary types 3-5; [[vssai]] handles stream-aquifer types 9 and 10. |
!> | Lower boundary | [[vslowr]] adds bottom flow/head/free-drainage terms. |
!> | Linear solve | `TRIDAG` solves for pressure-head increments `CDPSI`. |
!>
!> A column is computationally converged when
!>
!> \[
!>   \max_c |\Delta\psi_c| \le 10^{-4}
!> \]
!>
!> within the 100 local iterations, exiting the loop immediately via `EXIT
!> OUT500`.
!>
!> After the pressure update, internal vertical fluxes are recomputed as
!>
!> \[
!>   CQV_c =
!>   {\beta_{c+1}\left[(z_c+\psi_c)-(z_{c+1}+\psi_{c+1})\right]\over CA0},
!> \]
!>
!> and regular lateral fluxes as
!>
!> \[
!>   CQH_{j,c} =
!>   \gamma_1(H_1-H_0)+\gamma_2(H_2-H_0),
!> \]
!>
!> where the \(\beta\) and \(\gamma\) conductances come from [[vscoef]]. The
!> phreatic-surface level is taken from the highest cell whose pressure head is
!> non-negative, bounded below by the bottom-cell base elevation.
!>
!> @warning
!> The error-reporting block only checks `NIT > NITMAX .AND. ELEVEL > 0`, i.e.
!> whether the loop ran to completion without converging; the severity
!> argument passed to `ERROR` is the caller-supplied `ELEVEL`, not a fixed
!> `ERRLVL_warn`. Repeated messages are limited by the saved `errorcount` and
!> `errcntallowed`.
!> @endwarning
!>
!> @note
!> `EESN`, `ICLGN`, `ICLGL`, and `CLG` are not used in this routine. Manual
!> lateral head-gradient boundary categories are therefore not applied here;
!> `JCBC=5` reaches [[vsbc]], which only prints its unfinished-code message.
!> `CQH` is not reset for all faces and cells; entries are assigned only by the
!> active boundary/stream-aquifer calls or by the final active-neighbour flux
!> loop.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-29 | GP | 4.0 | Written; version 4.0 completed 1996-07-17. |
!> | 1996-12-20 | RAH | 4.1 | Removed commented-out lines. |
!> | 1996-12-28 | RAH | 4.1 | Arguments: added `CWV`/`CWL`, removed `BUG`; made `IFA` local. Removed `COMMON /CCCOLM/` and the `CETAO`/`CKRO` lines. [[vscoef]] arguments: removed `IEL`/`NIT`, added `CWV`/`CWL`. |
!> | 1997-01-21 | RAH | 4.1 | Made `CEPSMX` and `NITMAX` constants; used a `DO 500` loop instead of `GOTO`; used generic intrinsics; removed the redundant `ICPSL`; extended (and de-duplicated) the [[vsfunc]] argument list. |
!> | 1997-01-22 | RAH | 4.1 | Extended the [[vscoef]] argument list. |
!> | 1997-01-23 | RAH | 4.1 | Made the [[vscoef]] outputs arguments and `CETA`, `CDETA`, `CDKR` local; eliminated further arguments, including `CBETP` (now `CBETM(ICL+1)`). |
!> | 1997-01-26 | RAH | 4.1 | Gave [[vsintc]] a full argument list and made `CA`/`CC` local. |
!> | 1997-01-27 | RAH | 4.1 | Gave [[vsuppr]], [[vswell]], [[vsspr]], and [[vsbc]] full argument lists. |
!> | 1997-01-31 | RAH | 4.1 | Gave [[vslowr]] a full argument list and made its call unconditional; removed the redundant `I1`. |
!> | 1997-02-03 | RAH | 4.1 | Gave [[vssai]] a full argument list and repositioned its call; replaced input `CV` with `CA0`/`CDELZ`; made `CDPSI`, `CB`, `CR` local; replaced output `CQINF` with `CQV(ICTOP)`; passed `CA0` to [[vswell]]; simplified the `CPSL` code; added the `CGAM2` term to `CQH` unconditionally. |
!> | 1997-02-07 | RAH | 4.1 | Removed the [[vswell]] output `CQW`. |
!> | 1997-02-10 | RAH | 4.1 | Removed the output argument `NITC` and the `CQBK*` commons; moved inputs `BCHELE`, `CA0`, `CZG`, `DT`, `CPSIN` and outputs `CQSP`, `CPSL` from `VSCOLM.INC` into the argument list; moved input `SIGMA` into [[vsintc]]; initialised `CQH`. |
!> | 1997-05-13 | RAH | 4.1 | Used `VSK3D(ICSOIL(ICL),?)` for `CKIJS(ICL,?)`/`CKZS(ICL)`; swapped the `CAIJ`, `CQH`, `JCACN`, and `JCDEL` indices; replaced `VSCOLM.INC` with arguments. |
!> | 1997-05-14 | RAH | 4.1 | [[vsbc]] arguments: removed `DWORK2`, added `IFA` (also to [[vssai]]). [[vsuppr]] arguments: replaced `CDW`, `CEW`, `CQP` with `CDNET`. [[vswell]] arguments: reordered; stopped initialising `CQH`. Added local `DPSI`; removed the block-`IF` when setting `CPSL`. |
!> | 1997-05-15 | RAH | 4.1 | Reordered the argument list. |
!> | 1998-04-02 | RAH | 4.2 | Replaced the local `ERR` with the new argument `ELEVEL` (see [[vssim]]). |
!> | 1998-11-03 | SPA | - | Added the `depadj` argument, carrying the adjacent channel water depth through to [[vssai]] for the channel-aquifer flow correction. |
!> | 2009-01 | JE | 4.3.5F90 | Restructured loops for automatic differentiation. |
!> | 2026-04-06/07 | SvB | 4.6 | The `GOTO`-driven `g510`/label-510 exit flag was replaced with a direct `EXIT OUT500`; the non-convergence report now checks `NIT > NITMAX` instead of a separate flag, and uses `ELEVEL` (not a fixed `ERRLVL_warn`) as the reported severity. The phreatic-surface search loop was rewritten from a labelled `DO`/`GOTO` pair to `EXIT search_loop`, with equivalent behaviour. The explicit array-section copy into `TRIDAG` was replaced with scalar-start dummy arguments (relies on sequence association). |
!> @endhistory
   SUBROUTINE VSCOLM(EESN, CWV, CWL, VSK3D, BCHELE, ELEVEL, &
                     IEL, ICBOT, ICTOP, ICBED, ICLYRB, ICSOIL, JCBC, JCDEL1, JELDUM, &
                     JCACN, JCDEL, ICSPCE, ICLFN, ICLFL, ICWLBT, ICLHN, ICLHL, ICWLTP, &
                     ICLGN, ICLGL, CA0, CZG, CZSP, CCS, CDELZ, CZ, CDELL, CAIJ, CAIJ1, &
                     CDELL1, CZ1, DT, CDNET, CPSIN, CQ, CZS, CPSI1, CPSIN1, CKIJ1, &
                     CQWIN, CLF, CLH, CLG, CBF, CBH, ICSTOR, CPSI, CKR, CTHETA, CQH, &
                     CQV, CQWI, CQSP, CPSL, depadj)

      ! Assumed external module dependencies providing global variables:
      ! LLEE, NLYREE, NSEE, NSOLEE, NVSSOL, VSPPSI, VSPTHE, VSPKR, VSPETA,
      ! VSPDKR, VSPDET, FID_logfile, ERROR, errcntallowed, ZERO, half

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: EESN                  !! Unused legacy dimension argument; current calls pass `NSEE`.
      INTEGER, INTENT(IN) :: ELEVEL                !! Positive value enables column non-convergence reporting; also used as the reported `ERROR` severity.
      INTEGER, INTENT(IN) :: IEL                   !! Element number for diagnostics and soil-function interpolation.
      INTEGER, INTENT(IN) :: ICBOT                 !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                 !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICBED                 !! River-bed cell index for stream-aquifer interaction.
      INTEGER, INTENT(IN) :: ICSPCE                !! Spring source cell; meaningful only for spring columns.
      INTEGER, INTENT(IN) :: ICWLBT                !! Bottom screened well cell; meaningful only for well columns.
      INTEGER, INTENT(IN) :: ICWLTP                !! Top screened well cell; meaningful only for well columns.
      INTEGER, INTENT(IN) :: ICLFN                 !! Number of selected lateral-flow layers; zero means full active column.
      INTEGER, INTENT(IN) :: ICLHN                 !! Number of selected lateral-head layers; zero means full active column.
      INTEGER, INTENT(IN) :: ICLGN                 !! Unused number of selected lateral-gradient layers.
      INTEGER, INTENT(IN) :: ICLYRB(NLYREE)        !! Bottom-cell bounds for model-layer intervals.
      INTEGER, INTENT(IN) :: ICSOIL(ICBOT:ICTOP)   !! Soil type by active cell.
      INTEGER, INTENT(IN) :: JCBC(0:5)             !! Boundary/source type by base, lateral face, and source slot.
      INTEGER, INTENT(IN) :: ICLFL(NLYREE)         !! Selected model layers for lateral-flow categories.
      INTEGER, INTENT(IN) :: JCACN(4, ICBOT:ICTOP)  !! Adjacent-cell index by face and active cell.
      INTEGER, INTENT(IN) :: JELDUM(4)             !! Adjacent element id by face; values below 1 disable regular lateral coupling.
      INTEGER, INTENT(IN) :: ICLHL(NLYREE)         !! Selected model layers for lateral-head categories.
      INTEGER, INTENT(IN) :: JCDEL(4, ICBOT:ICTOP)  !! Current-column split indicator for lateral coupling.
      INTEGER, INTENT(IN) :: ICLGL(NLYREE)         !! Unused selected model layers for lateral-gradient categories.
      INTEGER, INTENT(IN) :: JCDEL1(LLEE, 4)        !! Neighbour-column split offset used for second connected cells.
      DOUBLE PRECISION, INTENT(IN) :: CWV           !! Vertical hydraulic-conductivity w-mean control.
      DOUBLE PRECISION, INTENT(IN) :: CWL           !! Lateral hydraulic-conductivity w-mean control.
      DOUBLE PRECISION, INTENT(IN) :: CA0           !! Plan area of the current element.
      DOUBLE PRECISION, INTENT(IN) :: CZG           !! Ground elevation used for depth-style lateral head boundaries.
      DOUBLE PRECISION, INTENT(IN) :: CZSP          !! Spring discharge elevation; meaningful only for spring columns.
      DOUBLE PRECISION, INTENT(IN) :: CCS           !! Spring coefficient; meaningful only for spring columns.
      DOUBLE PRECISION, INTENT(IN) :: VSK3D(NSEE, 3) !! Saturated hydraulic conductivity by soil type and x/y/z direction.
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Active-cell thicknesses.
      DOUBLE PRECISION, INTENT(IN) :: CDELL(4)      !! Current-element lateral distance scale by face.
      DOUBLE PRECISION, INTENT(IN) :: CAIJ1(LLEE, 4) !! Adjacent-element lateral face areas.
      DOUBLE PRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP) !! Active-cell node elevations.
      DOUBLE PRECISION, INTENT(IN) :: CDELL1(4)     !! Adjacent-element lateral distance scale by face.
      DOUBLE PRECISION, INTENT(IN) :: CZ1(LLEE, 4)   !! Adjacent-cell node elevations by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CAIJ(4, ICBOT:ICTOP) !! Current-element lateral face areas.
      DOUBLE PRECISION, INTENT(IN) :: DT            !! Timestep length.
      DOUBLE PRECISION, INTENT(IN) :: CDNET         !! Net surface-water depth available for the upper boundary.
      DOUBLE PRECISION, INTENT(IN) :: CQWIN         !! Prescribed total well abstraction rate; meaningful only for well columns.
      DOUBLE PRECISION, INTENT(IN) :: CBF           !! Prescribed bottom-flow boundary value.
      DOUBLE PRECISION, INTENT(IN) :: CBH           !! Prescribed bottom-head boundary value.
      DOUBLE PRECISION, INTENT(IN) :: CPSI1(LLEE, 4) !! Adjacent current pressure heads by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CPSIN(ICBOT:ICTOP) !! Previous-timestep pressure heads for the current column.
      DOUBLE PRECISION, INTENT(IN) :: CLF(NLYREE)   !! Prescribed lateral-flow boundary values.
      DOUBLE PRECISION, INTENT(IN) :: CPSIN1(LLEE, 4) !! Adjacent previous-timestep pressure heads by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CQ(ICBOT:ICTOP) !! Cell source/sink terms already scaled for column assembly.
      DOUBLE PRECISION, INTENT(IN) :: CLH(NLYREE)   !! Prescribed lateral-head or depth boundary values.
      DOUBLE PRECISION, INTENT(IN) :: CKIJ1(LLEE, 4) !! Adjacent-cell lateral hydraulic conductivity terms.
      DOUBLE PRECISION, INTENT(IN) :: CZS(4)        !! Adjacent channel water-surface elevations for stream-aquifer faces.
      DOUBLE PRECISION, INTENT(IN) :: CLG(NLYREE)   !! Unused prescribed lateral-gradient boundary values.
      DOUBLE PRECISION, INTENT(IN) :: depadj(4)     !! Depth adjustment for stream-aquifer contact-area limiting.
      LOGICAL, INTENT(IN) :: BCHELE                !! True when lateral head-boundary values are elevations.

      ! In+out arguments
      INTEGER, INTENT(INOUT) :: ICSTOR(ICBOT:ICTOP) !! Soil lookup interval cache updated by [[vsfunc]].
      DOUBLE PRECISION, INTENT(INOUT) :: CPSI(ICBOT:ICTOP) !! Current pressure heads updated by the nonlinear solve.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CTHETA(ICBOT:ICTOP) !! Final volumetric water content.
      DOUBLE PRECISION, INTENT(OUT) :: CQV(ICBOT - 1:ICTOP) !! Final vertical fluxes, including lower and upper boundaries.
      DOUBLE PRECISION, INTENT(OUT) :: CKR(ICBOT:ICTOP) !! Final relative hydraulic conductivity.
      DOUBLE PRECISION, INTENT(OUT) :: CQH(4, ICBOT:ICTOP) !! Lateral and stream-aquifer fluxes assigned on active faces.
      DOUBLE PRECISION, INTENT(OUT) :: CQWI(ICWLBT:ICWLTP) !! Well abstraction rate by screened cell; meaningful only for well columns.
      DOUBLE PRECISION, INTENT(OUT) :: CQSP          !! Spring discharge; meaningful only for spring columns.
      DOUBLE PRECISION, INTENT(OUT) :: CPSL          !! Final phreatic-surface elevation for the column.

      ! Locals, etc
      INTEGER, PARAMETER :: NITMAX = 100
      DOUBLE PRECISION, PARAMETER :: CEPSMX = 1.0D-4
      INTEGER :: BTYPE, I, ICL, IFA, J, K, K1, NDUM, NIT, PCL, SOIL
      DOUBLE PRECISION :: CPSMIN, DPSI, DPSIMX, H0, H1, H2
      DOUBLE PRECISION :: DWORK1(1 + LLEE + NLYREE), DWORK2(LLEE)
      DOUBLE PRECISION :: CETA(LLEE), CDETA(LLEE), CDKR(LLEE)
      DOUBLE PRECISION :: CBETM(LLEE), CDBETM(LLEE), CDBTMM(LLEE)
      DOUBLE PRECISION :: CF(LLEE), CDF(LLEE), CKIJ(LLEE, 4), CDKIJ(LLEE, 4)
      DOUBLE PRECISION :: CGAM1(LLEE, 4), CDGAM1(LLEE, 4)
      DOUBLE PRECISION :: CGAM2(LLEE, 4), CDGAM2(LLEE, 4)
      DOUBLE PRECISION :: CA(LLEE), CB(LLEE), CC(LLEE), CR(LLEE), CDPSI(LLEE)

      INTEGER, SAVE :: errorcount = 0

      !----------------------------------------------------------------------*
      ! Initialization
      !________________*

      NDUM = ICTOP - ICBOT + 1

      ! Main iteration loop (calculations within depend upon CPSI)
      !____________________________________________________________*

      OUT500: DO NIT = 1, NITMAX

         ! update soil properties from previous iteration
         CALL VSFUNC(NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
                     VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ICSOIL, CPSI, &
                     ICSTOR, CTHETA, CETA(ICBOT), CKR, CDETA(ICBOT), CDKR(ICBOT))

         ! set up intermediate coefficients
         CALL VSCOEF(LLEE, NSEE, CWV, CWL, VSK3D, ICBOT, ICTOP, JELDUM, &
                     JCBC(1), ICSOIL, JCACN, JCDEL, JCDEL1, CA0, CDELL, CDELL1, &
                     CDELZ, CAIJ, CAIJ1, CKR, CDKR(ICBOT), CKIJ1, CBETM(ICBOT), &
                     CDBETM(ICBOT), CDBTMM(ICBOT), CF(ICBOT), CDF(ICBOT), &
                     CKIJ, CDKIJ, CGAM1, CGAM2, CDGAM1, CDGAM2, DWORK1, DWORK2)

         ! prepare basic coefficients for tri-diagonal solver ("internal" cells)
         CALL VSINTC(LLEE, ICBOT, ICTOP, JELDUM, JCBC(1), JCACN, &
                     JCDEL1, CA0, CDELZ, CZ, CZ1, DT, CETA(ICBOT), CDETA(ICBOT), &
                     CQ, CPSI, CPSIN, CF(ICBOT), CDF(ICBOT), CBETM(ICBOT), &
                     CDBETM(ICBOT), CDBTMM(ICBOT), CPSI1, CPSIN1, CGAM1, CGAM2, &
                     CDGAM1, CDGAM2, CA(ICBOT), CB(ICBOT), CC(ICBOT), CR(ICBOT), &
                     DWORK1)

         ! add top boundary condition
         SOIL = ICSOIL(ICTOP)
         CALL VSUPPR(CA0, CDELZ(ICTOP), VSK3D(SOIL, 3), DT, CDNET, &
                     CPSI(ICTOP), CB(ICTOP), CR(ICTOP), CQV(ICTOP))

         ! add well abstraction (type 1)
         BTYPE = JCBC(5)
         IF (BTYPE == 1) THEN
            CALL VSWELL(NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL(ICWLBT), &
                        CA0, CDELZ(ICWLBT), CQWIN, CPSI(ICWLBT), CR(ICWLBT), &
                        CQWI, DWORK1)
            ! add spring discharge (type 2)
         ELSE IF (BTYPE == 2) THEN
            CALL VSSPR(CZ(ICSPCE), CZSP, CCS, CPSI(ICSPCE), CKR( &
                       ICSPCE), CDKR(ICSPCE), CB(ICSPCE), CR(ICSPCE), CQSP)
         END IF

         ! add user-defined lateral boundary conditions (types 3-5)
         DO IFA = 1, 4
            BTYPE = JCBC(IFA)
            IF (BTYPE >= 3 .AND. BTYPE <= 5) THEN
               CALL VSBC(BCHELE, IFA, ICBOT, ICTOP, JCBC(IFA), &
                         ICLYRB, ICLFN, ICLFL, ICLHL, ICLHN, CZG, CDELL(IFA), &
                         CDELZ, CZ, CAIJ, CLF, CLH, CPSI, CKIJ(ICBOT, IFA), &
                         CDKIJ(ICBOT, IFA), CB(ICBOT), CR(ICBOT), CQH, DWORK1)

               ! add stream-aquifer interaction (types 9 and 10)
            ELSE IF (BTYPE == 9 .OR. BTYPE == 10) THEN
               CALL VSSAI(IFA, JCBC(IFA), ICBOT, ICTOP, ICBED, CDELL( &
                          IFA), CZ, CAIJ, CZS(IFA), CPSI, CKIJ(ICBOT, IFA), &
                          CDKIJ(ICBOT, IFA), CB(ICBOT), CR(ICBOT), CQH, depadj( &
                          IFA), cdelz)
            END IF
         END DO

         ! add lower boundary condition (types 6-8)
         SOIL = ICSOIL(ICBOT)
         CALL VSLOWR(JCBC(0), CA0, CZ(ICBOT), CDELZ(ICBOT), VSK3D( &
                     SOIL, 3), CBF, CBH, CPSI(ICBOT), CKR(ICBOT), CDKR(ICBOT), &
                     CB(ICBOT), CR(ICBOT), CQV(ICBOT - 1))

         ! solve linear equations (Preserving required assumed-shape array slices)
         CALL TRIDAG(CA(ICBOT), CB(ICBOT), CC(ICBOT), CR(ICBOT), CDPSI(ICBOT), NDUM)

         ! update PSI array and check for convergence
         DPSIMX = ZERO
         DO ICL = ICBOT, ICTOP
            DPSI = CDPSI(ICL)
            CPSI(ICL) = CPSI(ICL) + DPSI
            DPSIMX = MAX(DPSIMX, ABS(DPSI))
         END DO

         ! PERFECT EXIT: Immediately break loop if convergence is met
         IF (DPSIMX <= CEPSMX) EXIT OUT500

      END DO OUT500

      ! Handle non-convergence error reporting safely
      IF (NIT > NITMAX .AND. ELEVEL > 0) THEN
         errorcount = errorcount + 1
         IF (errorcount < errcntallowed) THEN
            CALL RAISE_ERROR(ELEVEL, 1036, FID_logfile, IEL, 0, 'Maximum iterations in VSS column solver')
         ELSE IF (errorcount == errcntallowed) THEN
            CALL RAISE_ERROR (ELEVEL, 1036, FID_logfile, IEL, 0, '**** Last printout of the error message - maximum iterations error in VSS column solver *****')
         END IF
      END IF

      ! Calculate final values of output variables
      !____________________________________________*
      ! flows
      DO ICL = ICBOT, ICTOP - 1
         PCL = ICL + 1
         CQV(ICL) = CBETM(PCL)*(CZ(ICL) + CPSI(ICL) - CZ(PCL) - CPSI(PCL))/CA0
      END DO

      face_loop: DO J = 1, 4
         IF (JELDUM(J) < 1) CYCLE face_loop

         cell_loop: DO I = ICBOT, ICTOP
            K = JCACN(J, I)
            IF (K < 1) CYCLE cell_loop

            K1 = K + JCDEL1(K, J)
            H0 = CZ(I) + CPSI(I)
            H1 = CZ1(K, J) + CPSI1(K, J)
            H2 = CZ1(K1, J) + CPSI1(K1, J)

            CQH(J, I) = CGAM1(I, J)*(H1 - H0) + CGAM2(I, J)*(H2 - H0)
         END DO cell_loop
      END DO face_loop

      ! phreatic surface level
      CPSMIN = CZ(ICBOT) - half*CDELZ(ICBOT)

      search_loop: DO ICL = ICBOT, ICTOP
         IF (CPSI(ICL) < ZERO) EXIT search_loop
      END DO search_loop

      ! Adjust ICL only if we actually found a value or finished the loop
      ICL = MAX(ICBOT, ICL - 1)

      CPSL = MAX(CPSMIN, CZ(ICL) + CPSI(ICL))

   END SUBROUTINE VSCOLM

!> Interpolates soil hydraulic functions for a column.
!>
!> `VSFUNC` evaluates the soil hydraulic functions needed by [[vscolm]] for
!> every active cell in one column, using the lookup tables prepared by
!> [[vssoil]]. Given pressure potential `CPSI`, it returns moisture content
!> `CTHETA`, storage coefficient `CETA`, relative hydraulic conductivity `CKR`,
!> derivative of storage `CDETA`, and derivative of relative conductivity
!> `CDKR`.
!>
!> Required entry conditions are: `1 < NVSSOL <= NSOLEE`; `VSPPSI` is strictly
!> decreasing; `ICBOT <= ICTOP`; `ICSOIL(ICBOT:ICTOP)` contains valid soil
!> indices, i.e. `0 < ICSOIL <= NS`, where `NS` is the size of the second
!> dimension of `VSPTHE`, `VSPKR`, `VSPETA`, `VSPDKR`, and `VSPDET`; and the
!> print/error unit is available for diagnostics.
!>
!> For each cell, the previous interval index `ICSTOR(c)` is used as the first
!> guess. The routine then hunts up or down the monotonic pressure-head table
!> with doubling increments and finishes with bisection, following the `HUNT`
!> search pattern from Press et al. (1992), *Numerical Recipes in FORTRAN: The
!> Art of Scientific Computing*, 2nd ed., p. 112. It stores the lower bracket
!> `j = ICSTOR(c)` such that, after clipping to the valid table range,
!>
!> \[
!>   VSPPSI_j \ge CPSI_c \ge VSPPSI_{j+1}.
!> \]
!>
!> The interpolation fraction is
!>
!> \[
!>   p = {CPSI_c - VSPPSI_j\over VSPPSI_{j+1}-VSPPSI_j}.
!> \]
!>
!> `CTHETA`, `CKR`, `CDKR`, and `CDETA` are linearly interpolated as
!>
!> \[
!>   X_c = X_j + p(X_{j+1}-X_j),
!> \]
!>
!> using `VSPTHE`, `VSPKR`, `VSPDKR`, and `VSPDET`, respectively. `CETA` is
!> assigned from `VSPETA(j+1,soil)` as in the legacy implementation.
!>
!> On a successful return, for each cell `c` in `ICBOT:ICTOP`, with
!> `j = ICSTOR(c)` and `s = ICSOIL(c)`, the stored interval and returned values
!> satisfy the bracketing implied by the strictly decreasing `VSPPSI` table:
!>
!> | Quantity | Exit condition |
!> |:---------|:---------------|
!> | `ICSTOR(c)` | `0 < j < NVSSOL` |
!> | `CPSI(c)` | `VSPPSI(j) >= CPSI(c) >= VSPPSI(j+1)` because `VSPPSI` is strictly decreasing. |
!> | `CTHETA(c)` | Bounded by the bracketing `VSPTHE(j,s)` and `VSPTHE(j+1,s)` values. |
!> | `CETA(c)` | Taken from `VSPETA(j+1,s)`; for monotone table segments this lies between `VSPETA(j,s)` and `VSPETA(j+1,s)`. |
!> | `CKR(c)` | Bounded by the bracketing `VSPKR(j,s)` and `VSPKR(j+1,s)` values. |
!> | `CDETA(c)` | Bounded by the bracketing `VSPDET(j,s)` and `VSPDET(j+1,s)` values. |
!> | `CDKR(c)` | Bounded by the bracketing `VSPDKR(j,s)` and `VSPDKR(j+1,s)` values. |
!>
!> If \(p\) falls outside `[0,1]`, the routine raises fatal error 1034 or 1035
!> with a wet/dry diagnostic for the offending element and cell.
!>
!> @note
!> In the current restructured loop, an out-of-range cell sets the local
!> `IS_ERROR` flag and exits the cell loop immediately (`EXIT OUT100`) before
!> the fatal `ERROR` call is made after the loop. Output values after the
!> offending cell should therefore be treated as undefined on this path.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-18 | GP | 4.0 | Written. |
!> | 1996-12-20 | RAH | 4.1 | Removed long and leading comments; declared externals; used explicit sizes where possible; made `ICSTOR` in+out; removed redundant execution and lower-case code. |
!> | 1997-01-21 | RAH | 4.1 | Passed data through arguments instead of `COMMON`; allowed the end-point cases; removed redundant arguments and commented-out code. |
!> | 1997-01-22 | RAH | 4.1 | Amended the entry conditions; used a branch for the `ERROR` call. |
!> | 2009-01 | JE | 4.3.5F90 | Restructured loops for automatic differentiation. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote the labelled `GOTO`-driven hunt/bisection search as `DO WHILE` loops with named `EXIT`s; renamed the GOTO-era `g8100` flag to `IS_ERROR`. Same search algorithm and bracketing result. |
!> @endhistory
   SUBROUTINE VSFUNC(NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
                     VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ICSOIL, CPSI, ICSTOR, &
                     CTHETA, CETA, CKR, CDETA, CDKR)

      ! Assumed external module dependencies providing global variables:
      ! ZERO, ONE, ERRLVL_fatal, FID_logfile, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NVSSOL                   !! Number of active soil lookup-table rows.
      INTEGER, INTENT(IN) :: NSOLEE                   !! Declared first dimension of the soil lookup tables.
      DOUBLE PRECISION, INTENT(IN) :: VSPPSI(NVSSOL)   !! Strictly decreasing lookup pressure-head ordinates.
      DOUBLE PRECISION, INTENT(IN) :: VSPTHE(NSOLEE, *) !! Lookup volumetric water content by row and soil type.
      DOUBLE PRECISION, INTENT(IN) :: VSPKR(NSOLEE, *)  !! Lookup relative hydraulic conductivity by row and soil type.
      DOUBLE PRECISION, INTENT(IN) :: VSPETA(NSOLEE, *) !! Lookup storage coefficient by row and soil type.
      DOUBLE PRECISION, INTENT(IN) :: VSPDKR(NSOLEE, *) !! Lookup derivative `d(K_r)/d(psi)` by row and soil type.
      DOUBLE PRECISION, INTENT(IN) :: VSPDET(NSOLEE, *) !! Lookup derivative `d(eta)/d(psi)` by row and soil type.
      INTEGER, INTENT(IN) :: IEL                      !! Element number used in diagnostics.
      INTEGER, INTENT(IN) :: ICBOT                    !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                    !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICSOIL(ICBOT:ICTOP)      !! Soil type by active cell.
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Pressure head/potential by active cell.

      ! In+out arguments
      INTEGER, INTENT(INOUT) :: ICSTOR(ICBOT:ICTOP)   !! Cached lower lookup-table interval by active cell.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CTHETA(ICBOT:ICTOP) !! Interpolated volumetric water content.
      DOUBLE PRECISION, INTENT(OUT) :: CETA(ICBOT:ICTOP) !! Interpolated storage coefficient.
      DOUBLE PRECISION, INTENT(OUT) :: CKR(ICBOT:ICTOP) !! Interpolated relative hydraulic conductivity.
      DOUBLE PRECISION, INTENT(OUT) :: CDETA(ICBOT:ICTOP) !! Interpolated derivative `d(eta)/d(psi)`.
      DOUBLE PRECISION, INTENT(OUT) :: CDKR(ICBOT:ICTOP) !! Interpolated derivative `d(K_r)/d(psi)`.

      ! Locals
      CHARACTER(LEN=5) :: WETDRY(0:1) = ['(wet)', '(dry)']
      DOUBLE PRECISION :: P, PDUM, VLO
      INTEGER :: ICL, INC, JHI, JLO, JM, IS, DRY
      LOGICAL :: IS_ERROR

      !----------------------------------------------------------------------*

      IS_ERROR = .FALSE.

      ! ----- loop over all cells in column
      OUT100: DO ICL = ICBOT, ICTOP

         P = CPSI(ICL)
         JLO = ICSTOR(ICL)
         IS = ICSOIL(ICL)

         ! --- find location in table of current psi value
         ! test for initial guess
         IF (JLO <= 0 .OR. JLO > NVSSOL) THEN
            JLO = 0
            JHI = NVSSOL + 1
         ELSE
            ! set initial hunt increment
            INC = 1

            ! hunt up the table
            IF (P <= VSPPSI(JLO)) THEN
               hunt_up: DO WHILE (.TRUE.)
                  JHI = JLO + INC
                  IF (JHI > NVSSOL) THEN
                     JHI = NVSSOL + 1
                     EXIT hunt_up
                  ELSE IF (P <= VSPPSI(JHI)) THEN
                     JLO = JHI
                     INC = INC + INC
                  ELSE
                     EXIT hunt_up
                  END IF
               END DO hunt_up

               ! hunt down the table
            ELSE
               JHI = JLO
               hunt_down: DO WHILE (.TRUE.)
                  JLO = JHI - INC
                  IF (JLO < 1) THEN
                     JLO = 0
                     EXIT hunt_down
                  ELSE IF (P > VSPPSI(JLO)) THEN
                     JHI = JLO
                     INC = INC + INC
                  ELSE
                     EXIT hunt_down
                  END IF
               END DO hunt_down
            END IF
         END IF

         ! hunt completed, begin bisection
         ! At this point: { VSPPSI(JLO)>=P or JLO=0        } and
         !                { VSPPSI(JHI)< P or JHI=NVSSOL+1 }

         bisection: DO WHILE (JHI - JLO > 1)
            JM = (JHI + JLO)/2
            IF (P < VSPPSI(JM)) THEN
               JLO = JM
            ELSE
               JHI = JM
            END IF
         END DO bisection

         JLO = MAX(1, MIN(JLO, NVSSOL - 1))
         JHI = JLO + 1

         ICSTOR(ICL) = JLO

         ! --- interpolate between values for return variables
         VLO = VSPPSI(JLO)
         PDUM = (P - VLO)/(VSPPSI(JHI) - VLO)

         ! Error trap replaced the g8100 CYCLE
         IF (PDUM < ZERO .OR. PDUM > ONE) THEN
            IS_ERROR = .TRUE.
            EXIT OUT100
         END IF

         VLO = VSPTHE(JLO, IS)
         CTHETA(ICL) = (VSPTHE(JHI, IS) - VLO)*PDUM + VLO

         CETA(ICL) = VSPETA(JHI, IS)

         VLO = VSPDKR(JLO, IS)
         CDKR(ICL) = (VSPDKR(JHI, IS) - VLO)*PDUM + VLO

         VLO = VSPKR(JLO, IS)
         CKR(ICL) = (VSPKR(JHI, IS) - VLO)*PDUM + VLO

         VLO = VSPDET(JLO, IS)
         CDETA(ICL) = (VSPDET(JHI, IS) - VLO)*PDUM + VLO

      END DO OUT100

      IF (IS_ERROR) THEN
         DRY = NINT(MAX(ZERO, MIN(PDUM, ONE)))
         CALL RAISE_ERROR(ERRLVL_fatal, 1034 + DRY, FID_logfile, IEL, ICL, 'soil property interpolation out of range '//WETDRY(DRY))
      END IF

   END SUBROUTINE VSFUNC

END MODULE vs_column_solver

