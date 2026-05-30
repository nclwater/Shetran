!> summary: Variably saturated subsurface flow.
!>
!> This module implements SHETRAN's `VS`/`VSS` component described in section
!> 2.5 of the User Guide and Data Input Manual. It reads the variably saturated
!> subsurface data file (`VSD`, `VS01`-`VS18`) and, when requested by `INITYP`,
!> the initial-conditions file (`VSI`). It constructs soil, river-bed, and
!> aquifer-zone cells; builds layer and cell connectivity; prepares
!> time-varying subsurface boundary data; solves each coupled column problem;
!> and returns surface exchange, spring, well, lateral, vertical, bank, and
!> base-flow fluxes to the shared model arrays.
!>
!> The solved state is pressure head/potential (`VSPSI`), water content
!> (`VSTHE`), relative hydraulic conductivity (`VSKR`), and fluxes (`QVSV`,
!> `QVSH`, `QVSBF`, `QVSSPR`, `QVSWEL`, `QBKB`, `QBKF`, `QBKI`). Numerically,
!> each active element is treated as a one-dimensional vertical column with
!> lateral coupling through the layer/cell connectivity arrays. [[vscolm]]
!> assembles a tridiagonal pressure-head correction system and solves it with
!> `TRIDAG` from `utilsmod`; [[vssim]] iterates columns in `ISORT` order until
!> pressure-head changes converge or the iteration limit is reached.
!>
!> Important manual controls are implemented as follows:
!>
!> | Input | Meaning in this module |
!> |:------|:-----------------------|
!> | `BFAST` | Chooses 100 or `min(500,NSOLEE)` soil lookup entries in [[vssoil]]. |
!> | `BSOILP` | Prints generated soil hydraulic lookup tables. |
!> | `BHELEV` | Interprets boundary head data as elevations rather than depths below ground. |
!> | `INITYP = 1` | Initialises an equilibrium profile from uniform phreatic-surface depth `VSIPSD`. |
!> | `INITYP = 2` | Initialises equilibrium profiles from phreatic-surface elevations in `VSI`. |
!> | `INITYP = 3` | Reads initial potentials for every cell from `VSI`. |
!> | `VSWV`, `VSWL` | Control w-mean averaging of vertical and lateral hydraulic conductivity. |
!>
!> Soil/lithology hydraulic properties are stored as lookup tables over pressure
!> head by [[vssoil]] and interpolated by [[vsfunc]]. The manual flags map to
!> code paths as follows:
!>
!> | `IVSFLG` | Manual option | Implementation status |
!> |:---------|:--------------|:----------------------|
!> | 1 | van Genuchten water retention and conductivity parameters | Implemented. |
!> | 2 | user tables for \(\theta(\psi)\) and \(K_r(\psi)\) | Implemented with spline interpolation over input tables. |
!> | 3 | exponential functions | Implemented. |
!> | 4 | user table for \(\theta(\psi)\) and Averjanov \(K(\theta)\) | Parsed as a legacy option, but stops in [[vssoil]]. |
!>
!> Boundary condition categories follow the manual `VS11`-`VS18` groups:
!> pumping wells, springs, lateral flow/head/head-gradient boundaries, and
!> bottom flow/head/free-drainage boundaries. Time-varying well and boundary
!> files are advanced by [[vsprep]] using `FINPUT`/`HINPUT`. Their terms are
!> folded into the column matrix by [[vswell]], [[vsspr]], [[vsbc]],
!> [[vslowr]], [[vsuppr]], [[vsintc]], and [[vssai]].
!>
!> Important implementation caveats found in the current code:
!>
!> | Area | Current behaviour |
!> |:-----|:------------------|
!> | Setup lifetime | Several setup routines use retained local state, and [[vssim]] caches `JCBCsv`, `VSAIJsv`, and `ICSOILsv` behind `FIRSTvssim`; the module follows the original one-initialisation workflow. |
!> | Manual boundary options | Lateral head-gradient boundaries (`JCBC=5`) are read/interpolated but not applied; bottom free drainage (`NBBTYP=8`) currently falls through to zero lower-boundary flux. |
!> | Soil-table derivatives | [[vssoil]] finally overwrites `VSPKR` with a DSATG saturation-ratio curve without recomputing `VSPDKR`, so conductivity derivatives used by [[vsfunc]] can be stale or unset. |
!> | Explicit source/sink linearisation | Spring and well terms use simplified or explicit pressure-dependent coefficients; see [[vsspr]] and [[vswell]] for the exact active forms. |
!> | Unfinished paths | `IVSFLG=4`, split-cell mass-balance correction, and lateral head-gradient boundary application are legacy unfinished paths. |
!>
!> Programmer's map:
!>
!> | Routine | Main responsibility |
!> |:--------|:--------------------|
!> | [[vsin]] | Read VSS inputs, allocate arrays, build connectivity, initialise soil tables and pressure heads. |
!> | [[vsread]] | Load `VSD` soil, zone, connectivity, well, spring, and boundary-category data. |
!> | [[vsconl]] / [[vsconc]] | Build layer and cell connectivity, cell thicknesses, node elevations, and split-cell mappings. |
!> | [[vssoil]] / [[vsfunc]] | Build and interpolate soil hydraulic property tables. |
!> | [[vssim]] | Run one VSS timestep, iterate columns, update fluxes, and call mass-balance correction. |
!> | [[vscolm]] / [[vscoef]] | Assemble and solve one nonlinear column system. |
!> | [[vsmb]] | Reconcile reported fluxes with storage change after the solve. |
!>
!> @warning
!> `IVSFLG = 4` is listed in the manual as tabulated water content with
!> Averjanov-style conductivity, but the current implementation stops if that
!> option is selected in [[vssoil]]. Split-cell mass-balance correction in
!> [[vsmb]] is also marked unfinished and stops if reached. Lateral
!> head-gradient boundary categories and bottom free-drainage categories are
!> parsed from the manual inputs, but do not currently add their advertised
!> physical boundary terms to the VSS matrix.
!> @endwarning
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-1998 | GP/RAH | 4.0-4.2 | Developed and reorganised the VSS common state, soil tables, initialisation state, connectivity, boundary handling, and column solver. |
!> | 1998-11 | SPA | - | Added the channel-aquifer flow correction: pass adjacent channel depth into [[vscolm]]/[[vssai]], limit channel-to-aquifer contact area for low channel water depth, simplify the stream-aquifer derivative, and align exchange-flow reporting with BALWAT. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS `.F` files and include blocks into this Fortran 90 module. |
!> | 2026-03 | SB | 4.6 | Moved saved arrays into allocatable module storage through `INITIALISE_AL_C2` for AD/current builds. |
!> @endhistory
MODULE VSmod
   USE SGLOBAL
   USE mod_load_filedata, ONLY : ALINIT, ALSPRD, ALREAD
!USE SGLOBAL,  ONLY :
   USE AL_G, ONLY : ICMREF, NX, NY, ICMXY, NGDBGN
   USE AL_C, ONLY : BHB, BFB, bexbk, DTUZ, deltaz, dummy, DHF, ESOILA, ERUZ, EEVAP, &
      FHBED, ISORT, jvsacn, JVSDEL, idum, icmbk, LFB, LHB, LINKNS, lgb, &
      NWELBT, NWELTP, NVSSPC, NVSWLI, NTSOIL, nhbed, NVC, NRD, nlyrbt, NVSWLT, NVSSPT, NBFACE, NS, nlyr, &
      PNETTO, QVSSPR, QVSBF, QH, QVSWEL, QBKF, QBKB, QVSV, QVSWLI, QVSH, QBKI, &
      tih, UZNEXT, &
      vsd, VSI, VSPSI, VSTHE, VSPOR, WLD, ZVSPSL, zlyrbt, zvsnod, zbeff, INITIALISE_AL_C, INITIALISE_AL_C2, TIH
   USE AL_D, ONLY : TTH
!USE VSINIT_INC
!USE VSCOM1_INC
!USE VSSOIL_INC
   USE UTILSMOD, ONLY : TRIDAG, FINPUT, HINPUT, DCOPY
   USE OCmod2,   ONLY : GETHRF
   IMPLICIT NONE
! Saved legacy state moved here for AD/current builds.
   INTEGER :: ICSOILsv(LLEE,NELEE) !! Cached VSS soil type by cell and element.
   INTEGER :: JCBCsv(0:5,NELEE)    !! Cached boundary-condition type/category metadata by face and element.
   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: VSAIJsv !! Cached lateral face area/conductance terms.

   DOUBLEPRECISION :: WLLAST=zero        !! Previous well-input record time.
   DOUBLEPRECISION :: WLTIME=zero        !! Current/next well-input record time.
   DOUBLEPRECISION :: RWELIN(NVSEE)=zero !! Current well abstraction input values.
   DOUBLEPRECISION :: RLFLST=zero        !! Previous lateral-flow boundary record time.
   DOUBLEPRECISION :: RLFTIM=zero        !! Current/next lateral-flow boundary record time.
   DOUBLEPRECISION :: RLFPRV(NVSEE)=zero !! Previous lateral-flow boundary values.
   DOUBLEPRECISION :: RLHLST=zero        !! Previous lateral-head boundary record time.
   DOUBLEPRECISION :: RLHTIM=zero        !! Current/next lateral-head boundary record time.
   DOUBLEPRECISION :: RLHPRV(NVSEE)=zero !! Previous lateral-head boundary values.
   DOUBLEPRECISION :: RLHNXT(NVSEE)=zero !! Next lateral-head boundary values.
   DOUBLEPRECISION :: RLGLST=zero        !! Previous lateral-gradient boundary record time.
   DOUBLEPRECISION :: RLGTIM=zero        !! Current/next lateral-gradient boundary record time.
   DOUBLEPRECISION :: RLGPRV(NVSEE)=zero !! Previous lateral-gradient boundary values.
   DOUBLEPRECISION :: RLGNXT(NVSEE)=zero !! Next lateral-gradient boundary values.
   DOUBLEPRECISION :: RBFLST=zero        !! Previous base-flow boundary record time.
   DOUBLEPRECISION :: RBFTIM=zero        !! Current/next base-flow boundary record time.
   DOUBLEPRECISION :: RBFPRV(NVSEE)=zero !! Previous base-flow boundary values.
   DOUBLEPRECISION :: RBHLST=zero        !! Previous base-head boundary record time.
   DOUBLEPRECISION :: RBHTIM=zero        !! Current/next base-head boundary record time.
   DOUBLEPRECISION :: RBHPRV(NVSEE)=zero !! Previous base-head boundary values.
   DOUBLEPRECISION :: RBHNXT(NVSEE)=zero !! Next base-head boundary values.
   DOUBLEPRECISION :: RLFDUM(NVSEE)=zero !! Lateral-flow interpolation workspace.
   DOUBLEPRECISION :: RLHDUM(NVSEE)=zero !! Lateral-head interpolation workspace.
   DOUBLEPRECISION :: RLGDUM(NVSEE)=zero !! Lateral-gradient interpolation workspace.
   LOGICAL :: FIRSTvssim=.TRUE.          !! True until `VSSIM` has cached column metadata.
   integer,parameter :: errcntallowed=1000 !! Maximum repeated VSS convergence warnings.

! Legacy VSCOM1.INC global VSS variables retained as module state.
!USE SGLOBAL, ONLY : NELEE, NLFEE, NLYREE, NVSEE, LLEE, NSEE
!IMPLICIT NONE
   LOGICAL :: BLOWP  !! Lower-boundary output print-control flag retained from legacy VSCOM1 state.
   LOGICAL :: BHELEV !! True when lateral boundary head inputs are elevations; false when they are depths below ground.

!COMMON / VSC1LI / BLOWP, BHELEV
! integer variables, initialisation
   INTEGER :: NCSZON                  !! Number of extra cells used to represent the soil-zone depth increments.
   INTEGER :: NCRBED                  !! Number of extra cells used to represent river-bed depth increments.
   INTEGER :: JVSALN(NELEE,NLYREE,4)  !! Aquifer-layer connectivity ranges packed as `NLYREE+1` multiples.
   INTEGER :: ISRBED(NLFEE)           !! River-bed soil type by link.
   INTEGER :: NVSWL                   !! Number of well boundary categories.
   INTEGER :: NVSSP                   !! Number of spring boundary categories.
   INTEGER :: NVSLF                   !! Number of lateral-flow boundary categories.
   INTEGER :: NVSLH                   !! Number of lateral-head boundary categories.
   INTEGER :: NVSLG                   !! Number of lateral head-gradient boundary categories.
   INTEGER :: NVSBF                   !! Number of bottom-flow boundary categories.
   INTEGER :: NVSBH                   !! Number of bottom-head boundary categories.
   INTEGER :: NVSBD                   !! Number of bottom-drainage boundary categories.
   INTEGER :: NVSWLC(NELEE)           !! Well category used by each element.
   INTEGER :: NLBTYP(NELEE)           !! Lateral boundary type by element.
   INTEGER :: NLBCAT(NELEE)           !! Lateral boundary category by element.
   INTEGER :: NBBTYP(NELEE)           !! Bottom boundary type by element.
   INTEGER :: NBBCAT(NELEE)           !! Bottom boundary category by element.
   INTEGER :: NVSLFT                  !! Expanded count of lateral-flow boundary values after selected-layer categories.
   INTEGER :: NVSLFL(NLYREE,NVSEE)    !! Selected model layers for lateral-flow categories.
   INTEGER :: NVSLFN(NVSEE)           !! Number of selected lateral-flow layers per category; zero means whole column.
   INTEGER :: NVSLHT                  !! Expanded count of lateral-head boundary values after selected-layer categories.
   INTEGER :: NVSLHL(NLYREE,NVSEE)    !! Selected model layers for lateral-head categories.
   INTEGER :: NVSLHN(NVSEE)           !! Number of selected lateral-head layers per category; zero means whole column.
   INTEGER :: NVSLGT                  !! Expanded count of lateral-gradient boundary values after selected-layer categories.
   INTEGER :: NVSLGL(NLYREE,NVSEE)    !! Selected model layers for lateral-gradient categories.
   INTEGER :: NVSLGN(NVSEE)           !! Number of selected lateral-gradient layers per category; zero means whole column.

!COMMON / VSC1II / NCSZON, NCRBED, JVSALN, ISRBED, NVSWL, NVSSP, &
   !NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD, NVSWLC, NLBTYP, NLBCAT, &
   !NBBTYP, NBBCAT, NVSLFT, NVSLFL, NVSLFN, NVSLHT, NVSLHL, NVSLHN, &
   !NVSLGT, NVSLGL, NVSLGN
! integer variables, time-varying
   INTEGER :: IVSSTO(LLEE,NELEE) !! Stored soil lookup-table interval by VSS cell and element.

!COMMON / VSC1IT / IVSSTO
! floating-point variables and arrays, initialisation
   DOUBLEPRECISION :: DCSZON(LLEE)  !! Soil-zone cell-depth increments, ordered from the ground surface downward.
   DOUBLEPRECISION :: DCRBED(LLEE)  !! River-bed cell-depth increments, ordered from the bed surface downward.
   DOUBLEPRECISION :: DCSTOT        !! Total configured soil-zone depth.
   DOUBLEPRECISION :: DCRTOT        !! Total configured river-bed depth.
   DOUBLEPRECISION :: VSZMIN        !! Minimum VSS cell thickness.
   DOUBLEPRECISION :: VSZMAX        !! Maximum VSS cell thickness, stored with the legacy small tolerance.
   DOUBLEPRECISION :: VSK3D(NSEE,3) !! Saturated hydraulic conductivity by soil type and x/y/z direction.
   DOUBLEPRECISION :: DRBED(NLFEE)  !! River-bed depth by link.
   DOUBLEPRECISION :: VSSPZ(NELEE)  !! Spring discharge elevation by element.
   DOUBLEPRECISION :: VSSPCO(NELEE) !! Spring conductance coefficient by element.
   DOUBLEPRECISION :: VSWV          !! Vertical hydraulic-conductivity w-mean control.
   DOUBLEPRECISION :: VSWL          !! Lateral hydraulic-conductivity w-mean control.

!COMMON / VSC1RI / DCSZON, DCRBED, DCSTOT, DCRTOT, VSZMIN, VSZMAX, &
   !VSK3D, DRBED, VSSPZ, VSSPCO, VSWV, VSWL
! floating-point arrays, time-varying
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VSKR !! Relative hydraulic conductivity by VSS cell and element.
   DOUBLEPRECISION :: WLNOW(NVSEE)        !! Current well abstraction values.
   DOUBLEPRECISION :: RLFNOW(NLYREE,NVSEE) !! Current lateral-flow boundary values.
   DOUBLEPRECISION :: RLHNOW(NLYREE,NVSEE) !! Current lateral-head boundary values.
   DOUBLEPRECISION :: RLGNOW(NLYREE,NVSEE) !! Current lateral-gradient boundary values.
   DOUBLEPRECISION :: RBFNOW(NVSEE)       !! Current bottom-flow boundary values.
   DOUBLEPRECISION :: RBHNOW(NVSEE)       !! Current bottom-head boundary values.
!PRIVATE :: NELEE, NLFEE, NLYREE, NVSEE, LLEE, NSEE
!end MODULE vscom1_inc

! Legacy VSSOIL.INC soil-parameter tables retained as module state.
!USE SGLOBAL, ONLY : NSEE
!IMPLICIT NONE
   INTEGER, PARAMETER :: NSOLEE = 200       !! Maximum number of generated soil lookup-table rows.
   DOUBLEPRECISION :: VSPPSI(NSOLEE)        !! Soil lookup pressure-head ordinates.
   DOUBLEPRECISION :: VSPTHE(NSOLEE,NSEE)   !! Soil lookup volumetric water content.
   DOUBLEPRECISION :: VSPKR(NSOLEE,NSEE)    !! Soil lookup relative hydraulic conductivity.
   DOUBLEPRECISION :: VSPETA(NSOLEE,NSEE)   !! Soil lookup storage coefficient.
   DOUBLEPRECISION :: VSPDTH(NSOLEE,NSEE)   !! Soil lookup derivative `d(theta)/d(psi)`.
   DOUBLEPRECISION :: VSPDKR(NSOLEE,NSEE)   !! Soil lookup derivative `d(K_r)/d(psi)`.
   DOUBLEPRECISION :: VSPDET(NSOLEE,NSEE)   !! Soil lookup derivative `d(eta)/d(psi)`.
   DOUBLEPRECISION :: VSPSS(NSEE)           !! Specific storage by soil type.
   DOUBLEPRECISION :: VSPPOR(NSEE)          !! Porosity copied from the wider soil parameter state.
   INTEGER :: NVSSOL                        !! Number of active soil lookup-table rows.
!PRIVATE :: NSEE
!END MODULE vssoil_inc

! Legacy VSINIT.INC initialisation variables retained as module state.
!USE SGLOBAL, ONLY : NELEE, NSEE, NVSEE
!IMPLICIT NONE

   LOGICAL :: BFAST  !! True to use the shorter generated soil lookup table.
   LOGICAL :: BSOILP !! True to print generated soil lookup tables.


!COMMON / VSINIL / BFAST, BSOILP
! integer arrays & variables
   INTEGER :: IVSFLG(NSEE) !! Soil hydraulic-property option by soil type.
   INTEGER :: IVSNTB(NSEE) !! Number of tabulated hydraulic-property rows by soil type.
   INTEGER :: NVSERR       !! Accumulated VSS input/setup error count.
   INTEGER :: INITYP       !! Initial pressure-head option from the VSS input file.


!COMMON / VSINII / IVSFLG, IVSNTB, NVSERR, INITYP
! floating-point arrays & variables

   DOUBLEPRECISION :: VSTRES(NSEE)      !! Residual water content by soil type.
   DOUBLEPRECISION :: VSVGN(NSEE)       !! van Genuchten `n` parameter by soil type.
   DOUBLEPRECISION :: VSALPH(NSEE)      !! Retention-curve alpha parameter by soil type.
   DOUBLEPRECISION :: VSIPSD            !! Initial uniform phreatic-surface depth for `INITYP=1`.
   DOUBLEPRECISION :: VSZWLB(NVSEE)     !! Lower screen depth for well categories.
   DOUBLEPRECISION :: VSZWLT(NVSEE)     !! Upper screen depth for well categories.
   DOUBLEPRECISION :: TBPSI(NVSEE,NSEE) !! Tabulated pressure-head values by row and soil type.
   DOUBLEPRECISION :: TBTHE(NVSEE,NSEE) !! Tabulated water-content values by row and soil type.
   DOUBLEPRECISION :: TBKR(NVSEE,NSEE)  !! Tabulated relative-conductivity values by row and soil type.
   DOUBLEPRECISION :: TBTHEC(NVSEE,NSEE) !! Cubic-spline second derivatives for tabulated water content.
   DOUBLEPRECISION :: TBKRC(NVSEE,NSEE) !! Cubic-spline second derivatives for tabulated relative conductivity.
   DOUBLEPRECISION :: VSSPD(NELEE)      !! Spring depth below ground by element.
!PRIVATE :: NELEE, NSEE, NVSEE
!end MODULE VSINIT_INC


   PRIVATE
   PUBLIC :: VSIN, VSSIM, & !REST ARE PUBLIC ONLY FOR AD
      rlfdum, rlgnxt, firstvssim, rbhlst, rlhlst, vsaijsv, jcbcsv, rbhprv, rlglst, rlhprv, rbfprv, rlgprv, &
      rlfprv, rwelin, rbhtim, wltime, rlhdum, rbhnxt, rlhtim, rlgdum, rlhnxt, rbftim, rlgtim, &
      VSPTHE, NVSSOL, VSPKR, VSPETA, VSPDTH, VSPDKR, VSPDET, VSPPSI, &
      wlnow, vskr, rlfnow, rbfnow, ivssto, rlhnow, rbhnow, INITIALISE_VSMOD, &
      RLFTIM, icsoilsv !THESE NEEDED ONLY FOR AD
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
   SUBROUTINE initialise_vsmod()

      ALLOCATE(vsaijsv(4,top_cell_no,total_no_elements), vskr(top_cell_no,total_no_elements))
   END SUBROUTINE initialise_vsmod



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
!> model layers whose cell bounds are supplied by `ICLYRB`.
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
   SUBROUTINE VSBC (BCHELE, FACE, ICBOT, ICTOP, JCBC, ICLYRB, ICLFN, &
      ICLFL, ICLHN, ICLHL, CZG, CDELL, CDELZ, CZ, CAIJ, CLF, CLH, CPSI, &
      CKIJ, CDKIJ, CB, CR, CQH, DUM)
      LOGICAL, INTENT(IN) :: BCHELE                    !! True when `CLH` values are elevations; false when they are depths below ground.
      INTEGER, INTENT(IN) :: FACE                      !! Boundary face number, in `1:4`.
      INTEGER, INTENT(IN) :: ICBOT                     !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                     !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: JCBC                      !! Lateral boundary type for this face.
      INTEGER, INTENT(IN) :: ICLYRB(*)                 !! Bottom-cell bounds for model-layer intervals.
      INTEGER, INTENT(IN) :: ICLFN                     !! Number of selected lateral-flow layers; zero means full active column.
      INTEGER, INTENT(IN) :: ICLFL(*)                  !! Selected model layers for type-3 lateral-flow categories.
      INTEGER, INTENT(IN) :: ICLHN                     !! Number of selected lateral-head layers; zero means full active column.
      INTEGER, INTENT(IN) :: ICLHL(*)                  !! Selected model layers for type-4 lateral-head categories.
      DOUBLEPRECISION, INTENT(IN) :: CZG               !! Ground elevation used to convert depth-style head boundaries.
      DOUBLEPRECISION, INTENT(IN) :: CDELL             !! Distance scale normal to the boundary face.
      DOUBLEPRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Cell thicknesses.
      DOUBLEPRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP)   !! Cell-node elevations.
      DOUBLEPRECISION, INTENT(IN) :: CAIJ(4,ICBOT:ICTOP) !! Face areas by face and cell.
      DOUBLEPRECISION, INTENT(IN) :: CLF(*)            !! Prescribed lateral-flow boundary values.
      DOUBLEPRECISION, INTENT(IN) :: CLH(*)            !! Prescribed lateral-head or depth boundary values.
      DOUBLEPRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Current pressure heads.
      DOUBLEPRECISION, INTENT(IN) :: CKIJ(ICBOT:ICTOP) !! Current lateral hydraulic conductivity terms.
      DOUBLEPRECISION, INTENT(IN) :: CDKIJ(ICBOT:ICTOP) !! Derivatives of `CKIJ` with respect to pressure head.
      DOUBLEPRECISION, INTENT(INOUT) :: CB(ICBOT:ICTOP) !! Matrix diagonal terms updated with lateral boundary contributions.
      DOUBLEPRECISION, INTENT(INOUT) :: CR(ICBOT:ICTOP) !! Right-hand side terms updated with lateral boundary fluxes.
      DOUBLEPRECISION, INTENT(INOUT) :: CQH(4,ICBOT:ICTOP) !! Diagnostic lateral boundary fluxes for the selected face.
      DOUBLEPRECISION, INTENT(INOUT) :: DUM(*)         !! Workspace for transmissive-thickness weights or converted boundary heads.
! Locals, etc
!INTRINSIC MAX
      INTEGER :: ICL, I, ILYR, ICL1, ICL2, IDUM, SGN


      DOUBLEPRECISION ADHOL, AOL, KDUM, Q, QTOT, TICL, TTOT, ZDUM
!----------------------------------------------------------------------*
! flow (type 3)

      IF (JCBC.EQ.3) THEN
         DO 200 I = 1, MAX (1, ICLFN)
            IF (ICLFN.EQ.0) THEN
               ICL1 = ICBOT
               ICL2 = ICTOP
            ELSE
               ILYR = ICLFL (I)
               ICL1 = ICLYRB (ILYR)
               ICL2 = ICLYRB (ILYR + 1) - 1
            ENDIF
            TTOT = zero
            DO 160 ICL = ICL1, ICL2
               TICL = CKIJ (ICL) * CDELZ (ICL)
               DUM (ICL) = TICL
               TTOT = TTOT + TICL
160         END DO
            QTOT = CLF (I)
            DO 180 ICL = ICL1, ICL2
               Q = (DUM (ICL) / TTOT) * QTOT
               CR (ICL) = CR (ICL) - Q
               CQH (FACE, ICL) = Q
180         END DO

200      END DO
! head (type 4)
! NB. If BCHELE=.false., head b.c.'s are depths below ground surface

      ELSEIF (JCBC.EQ.4) THEN
         IF (BCHELE) THEN
            ZDUM = zero
            SGN = + 1
         ELSE
            ZDUM = CZG
            SGN = - 1
         ENDIF
         IDUM = MAX (ICLHN, 1)
         DO 210 I = 1, IDUM
            DUM (I) = ZDUM + SGN * CLH (I)

210      END DO
         DO 260 I = 1, IDUM
            IF (ICLHN.EQ.0) THEN
               ICL1 = ICBOT
               ICL2 = ICTOP
            ELSE
               ILYR = ICLHL (I)
               ICL1 = ICLYRB (ILYR)
               ICL2 = ICLYRB (ILYR + 1) - 1
            ENDIF
            DO 240 ICL = ICL1, ICL2
               AOL = CAIJ (FACE, ICL) / CDELL
               ADHOL = (DUM (I) - CZ (ICL) - CPSI (ICL) ) * AOL
               KDUM = CKIJ (ICL)
               Q = KDUM * ADHOL
               CB (ICL) = CB (ICL) + CDKIJ (ICL) * ADHOL + KDUM * AOL
               CR (ICL) = CR (ICL) - Q
               CQH (FACE, ICL) = Q
240         END DO


260      END DO
! head gradient (type 5)

      ELSEIF (JCBC.EQ.5) THEN

         !STOP 'unfinished code for boundary type 5 - head gradients'
         print*,  'unfinished code for boundary type 5 - head gradients'

      ENDIF
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
!> @note
!> `CKIJ` and `CDKIJ` are set for every local cell and face. `CGAM1/2` and
!> `CDGAM1/2` are assigned only when `JCACN(j,i) /= 0`, `JELDUM(j) >= 1`, and
!> `JCBC(j) /= 9`; callers should only use those arrays on the same active
!> lateral-connection mask.
!> @endnote
   SUBROUTINE VSCOEF (LLEE, NSEE, CWV, CWL, VSK3D, ICBOT, ICTOP, &
      JELDUM, JCBC, ICSOIL, JCACN, JCDEL, JCDEL1, CA0, CDELL, CDELL1, &
      CDELZ, CAIJ, CAIJ1, CKR, CDKR, CKIJ1, CBETM, CDBETM, CDBTMM, CF, &
      CDF, CKIJ, CDKIJ, CGAM1, CGAM2, CDGAM1, CDGAM2, C, D)
      INTEGER, INTENT(IN) :: LLEE                  !! Declared cell dimension for column and neighbour arrays.
      INTEGER, INTENT(IN) :: NSEE                  !! Declared soil-type dimension for conductivity arrays.
      INTEGER, INTENT(IN) :: ICBOT                 !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                 !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: JELDUM(4)             !! Adjacent element id by face; values below 1 disable lateral coupling.
      INTEGER, INTENT(IN) :: JCBC(4)               !! Boundary type by face; type 9 is handled outside regular lateral coupling.
      INTEGER, INTENT(IN) :: ICSOIL(ICBOT:ICTOP)   !! Soil type by active cell.
      INTEGER, INTENT(IN) :: JCACN(4,ICBOT:ICTOP)  !! Adjacent-cell index by face and active cell; zero means no lateral connection.
      INTEGER, INTENT(IN) :: JCDEL1(LLEE,4)        !! Neighbour-column split offset used to find a second connected neighbour cell.
      INTEGER, INTENT(IN) :: JCDEL(4,ICBOT:ICTOP)  !! Current-column split indicator for lateral area weighting.
      DOUBLEPRECISION, INTENT(IN) :: CWV           !! Vertical hydraulic-conductivity w-mean control.
      DOUBLEPRECISION, INTENT(IN) :: CWL           !! Lateral hydraulic-conductivity w-mean control.
      DOUBLEPRECISION, INTENT(IN) :: VSK3D(NSEE,3) !! Saturated hydraulic conductivity by soil type and x/y/z direction.
      DOUBLEPRECISION, INTENT(IN) :: CA0           !! Plan area of the current element.
      DOUBLEPRECISION, INTENT(IN) :: CDELL(4)      !! Current-element lateral distance scale by face.
      DOUBLEPRECISION, INTENT(IN) :: CDELL1(4)     !! Adjacent-element lateral distance scale by face.
      DOUBLEPRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Active-cell thicknesses.
      DOUBLEPRECISION, INTENT(IN) :: CAIJ(4,ICBOT:ICTOP) !! Current-element lateral face areas.
      DOUBLEPRECISION, INTENT(IN) :: CAIJ1(LLEE,4) !! Adjacent-element lateral face areas.
      DOUBLEPRECISION, INTENT(IN) :: CKR(ICBOT:ICTOP) !! Current relative hydraulic conductivity by active cell.
      DOUBLEPRECISION, INTENT(IN) :: CDKR(ICBOT:ICTOP) !! Derivative of `CKR` with respect to pressure head.
      DOUBLEPRECISION, INTENT(IN) :: CKIJ1(LLEE,4) !! Adjacent-cell lateral hydraulic conductivity terms.
      DOUBLEPRECISION, INTENT(OUT) :: CBETM(ICBOT:ICTOP+1) !! Vertical inter-cell conductance below each active cell.
      DOUBLEPRECISION, INTENT(OUT) :: CDBETM(ICBOT:ICTOP+1) !! Derivative of `CBETM` with respect to the lower cell.
      DOUBLEPRECISION, INTENT(OUT) :: CDBTMM(ICBOT:ICTOP+1) !! Derivative of `CBETM` with respect to the upper cell.
      DOUBLEPRECISION, INTENT(OUT) :: CF(ICBOT:ICTOP) !! Internal conductance contribution to the column diagonal.
      DOUBLEPRECISION, INTENT(OUT) :: CDF(ICBOT:ICTOP) !! Derivative of `CF` with respect to pressure head.
      DOUBLEPRECISION, INTENT(INOUT) :: CKIJ(LLEE,4)  !! Current-cell lateral hydraulic conductivity terms.
      DOUBLEPRECISION, INTENT(INOUT) :: CDKIJ(LLEE,4) !! Derivatives of `CKIJ` with respect to pressure head.
      DOUBLEPRECISION, INTENT(INOUT) :: CGAM1(LLEE,4) !! Primary lateral coupling conductance to adjacent cells.
      DOUBLEPRECISION, INTENT(INOUT) :: CGAM2(LLEE,4) !! Secondary split-cell lateral coupling conductance.
      DOUBLEPRECISION, INTENT(INOUT) :: CDGAM1(LLEE,4) !! Derivative of `CGAM1` with respect to local pressure head.
      DOUBLEPRECISION, INTENT(INOUT) :: CDGAM2(LLEE,4) !! Derivative of `CGAM2` with respect to local pressure head.
      DOUBLEPRECISION, INTENT(OUT) :: C(ICBOT:ICTOP) !! Workspace for local conductivity products.
      DOUBLEPRECISION, INTENT(OUT) :: D(ICBOT:ICTOP) !! Workspace for local conductivity derivatives.
! Locals, etc
!INTRINSIC ABS, MOD
      INTEGER :: DELKJ, I, J, K, K1, M, NIJ, NKJ, NKJM1, P
      DOUBLEPRECISION AIJDUM, AREA2, C1, C2, CAVE, CI, CIJ, CKJ, CK1J, &
         CM, Casum
      DOUBLEPRECISION D1, D2, DIJ, AODZ, KSAODZ, DXDUM, RCI, RCM, WI, &
         WIM1, WO2DX
      DOUBLEPRECISION KIJ, DKIJ, GAM1, GAM2, DGAM1, DGAM2, CKIJS, CKZS



      LOGICAL :: TEST
!----------------------------------------------------------------------*
! vertical conductivity terms (CBETM,CDB*)
      CBETM (ICBOT) = zero
      CDBETM (ICBOT) = zero

      CDBTMM (ICBOT) = zero

      IF (ISZERO(CWV)) THEN
!        ! Special case: weighted harmonic mean
         AREA2 = CA0 * 2D0
         DO 100 I = ICBOT, ICTOP
            CKZS = VSK3D (ICSOIL (I), 3)
            KSAODZ = CKZS * AREA2 / CDELZ (I)
            C (I) = CKR (I) * KSAODZ
            D (I) = CDKR (I) * KSAODZ
100      END DO
         DO 200 I = ICBOT + 1, ICTOP
            M = I - 1
            CM = C (M)
            CI = C (I)
            Casum = CM + CI
            RCM = CM / Casum
            RCI = CI / Casum
            CBETM (I) = CI * RCM
            CDBETM (I) = D (I) * RCM**2
            CDBTMM (I) = D (M) * RCI**2

200      END DO

      ELSEIF (ISONE(CWV)) THEN
!        * Arithmetic mean
         DO 203 I = ICBOT, ICTOP
            CKZS = VSK3D (ICSOIL (I), 3)
            C (I) = CKR (I) * CKZS
            D (I) = CDKR (I) * CKZS
203      END DO
         DO 205 I = ICBOT + 1, ICTOP
            M = I - 1
            AODZ = CA0 / (CDELZ (M) + CDELZ (I) )
            CBETM (I) = AODZ * (C (M) + C (I) )
            CDBETM (I) = AODZ * D (I)
            CDBTMM (I) = AODZ * D (M)

205      END DO

      ELSE
!        * General w-mean
         WI = one / CWV
         WIM1 = (one - CWV) / CWV
         DO 208 I = ICBOT, ICTOP
            CKZS = VSK3D (ICSOIL (I), 3)
            C (I) = (CKR (I) * CKZS) **CWV
            D (I) = CDKR (I) * CKZS
208      END DO
         DO 210 I = ICBOT + 1, ICTOP
            M = I - 1
            CM = C (M)
            CI = C (I)
            CAVE = .5D0 * (CM + CI)
            AODZ = CA0 / (CDELZ (M) + CDELZ (I) )
            CBETM (I) = AODZ * CAVE**WI * 2D0
            CDBETM (I) = AODZ * (CAVE / CI) **WIM1 * D (I)
            CDBTMM (I) = AODZ * (CAVE / CM) **WIM1 * D (M)

210      END DO

      ENDIF
      I = ICTOP + 1
      CBETM (I) = zero
      CDBETM (I) = zero


      CDBTMM (I) = zero
! vertical components of coefficients  NB lateral components added later
      DO 220 I = ICBOT, ICTOP
         P = I + 1
         CF (I) = CBETM (I) + CBETM (P)
         CDF (I) = CDBETM (I) + CDBTMM (P)


220   END DO
! loop over each face
      WI = one / CWL
      WIM1 = (one - CWL) / CWL

      DO 400 J = 1, 4
         M = 1 + MOD (J - 1, 2)
         TEST = JELDUM (J) .LT.1.OR.JCBC (J) .EQ.9
         DXDUM = CDELL (J) + CDELL1 (J)
         WO2DX = half * CWL / DXDUM


         DO 300 I = ICBOT, ICTOP
! lateral conductivity terms
            CKIJS = VSK3D (ICSOIL (I), M)
            KIJ = CKR (I) * CKIJS
            DKIJ = CDKR (I) * CKIJS
            CKIJ (I, J) = KIJ


            CDKIJ (I, J) = DKIJ
! lateral components of all coefficients
            K = JCACN (J, I)
            IF (K.EQ.0.OR.TEST) GOTO 300
!                                   >>>>>>>>
            NIJ = ABS (JCDEL (J, I) ) + 1
            DELKJ = JCDEL1 (K, J)
            K1 = K + DELKJ
            NKJM1 = ABS (DELKJ)

            NKJ = NKJM1 + 1
            CKJ = CKIJ1 (K, J) * CAIJ1 (K, J) / NIJ
            CK1J = CKIJ1 (K1, J) * CAIJ1 (K1, J) / NIJ
            AIJDUM = CAIJ (J, I) / NKJ
            DIJ = DKIJ * AIJDUM * WO2DX

            CIJ = KIJ * AIJDUM
            C1 = half * (CIJ + CKJ)
            C2 = half * (CIJ + CK1J)
            D1 = one

            D2 = one
            IF (NOTONE(CWL)) THEN
               CIJ = CIJ**CWL
               CKJ = CKJ**CWL
               CK1J = CK1J**CWL
               D1 = (C1 / CIJ) **WIM1
               D2 = (C2 / CIJ) **WIM1
               C1 = C1**WI
               C2 = C2**WI

            ENDIF
            GAM1 = C1 / DXDUM
            GAM2 = C2 / DXDUM * NKJM1
            DGAM1 = D1 * DIJ

            DGAM2 = D2 * DIJ * NKJM1
            CGAM1 (I, J) = GAM1
            CGAM2 (I, J) = GAM2
            CDGAM1 (I, J) = DGAM1
            CDGAM2 (I, J) = DGAM2
            CF (I) = CF (I) + GAM1 + GAM2

            CDF (I) = CDF (I) + DGAM1 + DGAM2

300      END DO

400   END DO
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
!> for spring columns (`JCBC(5)=2`).
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
!> within the 100 local iterations. In the present loop structure this sets an
!> internal `g510` flag, so later loop passes are skipped rather than leaving
!> the `DO` loop immediately.
!>
!> @warning
!> The error-reporting block is gated only by `ELEVEL > 0`, not by the final
!> convergence flag. It therefore calls error 1036 after the loop even for
!> columns that set `g510` earlier; the severity argument passed to `ERROR` is
!> `WWWARN`. Repeated messages are limited by the saved `errorcount` and
!> `errcntallowed`.
!> @endwarning
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
!> @note
!> `EESN`, `ICLGN`, `ICLGL`, and `CLG` are not used in this routine. Manual
!> lateral head-gradient boundary categories are therefore not applied here;
!> `JCBC=5` reaches [[vsbc]], which only prints its unfinished-code message.
!> `CQH` is not reset for all faces and cells; entries are assigned only by the
!> active boundary/stream-aquifer calls or by the final active-neighbour flux
!> loop.
!> @endnote
   SUBROUTINE VSCOLM (EESN, CWV, CWL, VSK3D, BCHELE, ELEVEL, &
      IEL, ICBOT, ICTOP, ICBED, ICLYRB, ICSOIL, JCBC, JCDEL1, JELDUM, &
      JCACN, JCDEL, ICSPCE, ICLFN, ICLFL, ICWLBT, ICLHN, ICLHL, ICWLTP, &
      ICLGN, ICLGL, CA0, CZG, CZSP, CCS, CDELZ, CZ, CDELL, CAIJ, CAIJ1, &
      CDELL1, CZ1, DT, CDNET, CPSIN, CQ, CZS, CPSI1, CPSIN1, CKIJ1, &
      CQWIN, CLF, CLH, CLG, CBF, CBH, ICSTOR, CPSI, CKR, CTHETA, CQH, &
      CQV, CQWI, CQSP, CPSL, depadj)
      INTEGER, INTENT(IN) :: EESN                  !! Unused legacy dimension argument; current calls pass `NSEE`.
      INTEGER, INTENT(IN) :: ELEVEL                !! Positive value enables column non-convergence reporting.
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
      INTEGER, INTENT(IN) :: JCACN(4,ICBOT:ICTOP)  !! Adjacent-cell index by face and active cell.
      INTEGER, INTENT(IN) :: JELDUM(4)             !! Adjacent element id by face; values below 1 disable regular lateral coupling.
      INTEGER, INTENT(IN) :: ICLHL(NLYREE)         !! Selected model layers for lateral-head categories.
      INTEGER, INTENT(IN) :: JCDEL(4,ICBOT:ICTOP)  !! Current-column split indicator for lateral coupling.
      INTEGER, INTENT(IN) :: ICLGL(NLYREE)         !! Unused selected model layers for lateral-gradient categories.
      INTEGER, INTENT(IN) :: JCDEL1(LLEE,4)        !! Neighbour-column split offset used for second connected cells.
      DOUBLEPRECISION, INTENT(IN) :: CWV           !! Vertical hydraulic-conductivity w-mean control.
      DOUBLEPRECISION, INTENT(IN) :: CWL           !! Lateral hydraulic-conductivity w-mean control.
      DOUBLEPRECISION, INTENT(IN) :: CA0           !! Plan area of the current element.
      DOUBLEPRECISION, INTENT(IN) :: CZG           !! Ground elevation used for depth-style lateral head boundaries.
      DOUBLEPRECISION, INTENT(IN) :: CZSP          !! Spring discharge elevation; meaningful only for spring columns.
      DOUBLEPRECISION, INTENT(IN) :: CCS           !! Spring coefficient; meaningful only for spring columns.
      DOUBLEPRECISION, INTENT(IN) :: VSK3D(NSEE,3) !! Saturated hydraulic conductivity by soil type and x/y/z direction.
      DOUBLEPRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Active-cell thicknesses.
      DOUBLEPRECISION, INTENT(IN) :: CDELL(4)      !! Current-element lateral distance scale by face.
      DOUBLEPRECISION, INTENT(IN) :: CAIJ1(LLEE,4) !! Adjacent-element lateral face areas.
      DOUBLEPRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP) !! Active-cell node elevations.
      DOUBLEPRECISION, INTENT(IN) :: CDELL1(4)     !! Adjacent-element lateral distance scale by face.
      DOUBLEPRECISION, INTENT(IN) :: CZ1(LLEE,4)   !! Adjacent-cell node elevations by cell and face.
      DOUBLEPRECISION, INTENT(IN) :: CAIJ(4,ICBOT:ICTOP) !! Current-element lateral face areas.
      DOUBLEPRECISION, INTENT(IN) :: DT            !! Timestep length.
      DOUBLEPRECISION, INTENT(IN) :: CDNET         !! Net surface-water depth available for the upper boundary.
      DOUBLEPRECISION, INTENT(IN) :: CQWIN         !! Prescribed total well abstraction rate; meaningful only for well columns.
      DOUBLEPRECISION, INTENT(IN) :: CBF           !! Prescribed bottom-flow boundary value.
      DOUBLEPRECISION, INTENT(IN) :: CBH           !! Prescribed bottom-head boundary value.
      DOUBLEPRECISION, INTENT(IN) :: CPSI1(LLEE,4) !! Adjacent current pressure heads by cell and face.
      DOUBLEPRECISION, INTENT(IN) :: CPSIN(ICBOT:ICTOP) !! Previous-timestep pressure heads for the current column.
      DOUBLEPRECISION, INTENT(IN) :: CLF(NLYREE)   !! Prescribed lateral-flow boundary values.
      DOUBLEPRECISION, INTENT(IN) :: CPSIN1(LLEE,4) !! Adjacent previous-timestep pressure heads by cell and face.
      DOUBLEPRECISION, INTENT(IN) :: CQ(ICBOT:ICTOP) !! Cell source/sink terms already scaled for column assembly.
      DOUBLEPRECISION, INTENT(IN) :: CLH(NLYREE)   !! Prescribed lateral-head or depth boundary values.
      DOUBLEPRECISION, INTENT(IN) :: CKIJ1(LLEE,4) !! Adjacent-cell lateral hydraulic conductivity terms.
      DOUBLEPRECISION, INTENT(IN) :: CZS(4)        !! Adjacent channel water-surface elevations for stream-aquifer faces.
      DOUBLEPRECISION, INTENT(IN) :: CLG(NLYREE)   !! Unused prescribed lateral-gradient boundary values.
      DOUBLEPRECISION, INTENT(IN) :: depadj(4)     !! Depth adjustment for stream-aquifer contact-area limiting.
      LOGICAL, INTENT(IN) :: BCHELE                !! True when lateral head-boundary values are elevations.
      INTEGER, INTENT(INOUT) :: ICSTOR(ICBOT:ICTOP) !! Soil lookup interval cache updated by [[vsfunc]].
      DOUBLEPRECISION, INTENT(INOUT) :: CPSI(ICBOT:ICTOP) !! Current pressure heads updated by the nonlinear solve.
      DOUBLEPRECISION, INTENT(OUT) :: CTHETA(ICBOT:ICTOP) !! Final volumetric water content.
      DOUBLEPRECISION, INTENT(OUT) :: CQV(ICBOT-1:ICTOP) !! Final vertical fluxes, including lower and upper boundaries.
      DOUBLEPRECISION, INTENT(OUT) :: CKR(ICBOT:ICTOP) !! Final relative hydraulic conductivity.
      DOUBLEPRECISION, INTENT(INOUT) :: CQH(4,ICBOT:ICTOP) !! Lateral and stream-aquifer fluxes assigned on active faces.
      DOUBLEPRECISION, INTENT(OUT) :: CQWI(ICWLBT:ICWLTP) !! Well abstraction rate by screened cell; meaningful only for well columns.
      DOUBLEPRECISION, INTENT(OUT) :: CQSP          !! Spring discharge; meaningful only for spring columns.
      DOUBLEPRECISION, INTENT(OUT) :: CPSL          !! Final phreatic-surface elevation for the column.
! Locals, etc
!INTRINSIC ABS, MAX
      INTEGER :: NITMAX
      DOUBLEPRECISION CEPSMX
      PARAMETER (NITMAX = 100, CEPSMX = 1D-4)
      INTEGER :: BTYPE, I, ICL, IFA, J, K, K1, NDUM, NIT, PCL, SOIL
      DOUBLEPRECISION CPSMIN, DPSI, DPSIMX, H0, H1, H2
      DOUBLEPRECISION DWORK1 (1 + LLEE+NLYREE), DWORK2 (LLEE)
      DOUBLEPRECISION CETA (LLEE), CDETA (LLEE), CDKR (LLEE)
      DOUBLEPRECISION CBETM (LLEE), CDBETM (LLEE), CDBTMM (LLEE)
      DOUBLEPRECISION CF (LLEE), CDF (LLEE), CKIJ (LLEE, 4), CDKIJ ( LLEE, 4)
      DOUBLEPRECISION CGAM1 (LLEE, 4), CDGAM1 (LLEE, 4)
      DOUBLEPRECISION CGAM2 (LLEE, 4), CDGAM2 (LLEE, 4)
      DOUBLEPRECISION CA (LLEE), CB (LLEE), CC (LLEE), CR (LLEE), CDPSI (LLEE)
      LOGICAL :: g510
      integer,save :: errorcount=0
!----------------------------------------------------------------------*
! Initialization
!________________*


      NDUM = ICTOP - ICBOT + 1
! Main iteration loop (calculations within depend upon CPSI)
!____________________________________________________________*

      g510=.FALSE.
      OUT500 : DO NIT = 1, NITMAX
         IF(g510) CYCLE
! update soil properties from previous iteration


         CALL VSFUNC (NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
            VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ICSOIL, CPSI, &
            ICSTOR, CTHETA, CETA (ICBOT), CKR, CDETA (ICBOT), CDKR (ICBOT) &
            )
! set up intermediate coefficients


         CALL VSCOEF (LLEE, NSEE, CWV, CWL, VSK3D, ICBOT, ICTOP, JELDUM, &
            JCBC (1), ICSOIL, JCACN, JCDEL, JCDEL1, CA0, CDELL, CDELL1, &
            CDELZ, CAIJ, CAIJ1, CKR, CDKR (ICBOT), CKIJ1, CBETM (ICBOT), &
            CDBETM (ICBOT), CDBTMM (ICBOT), CF (ICBOT), CDF (ICBOT), &
            CKIJ, CDKIJ, CGAM1, CGAM2, CDGAM1, CDGAM2, DWORK1, DWORK2)
! prepare basic coefficients for tri-diagonal solver ("internal" cells)


         CALL VSINTC (LLEE, ICBOT, ICTOP, JELDUM, JCBC (1), JCACN, &
            JCDEL1, CA0, CDELZ, CZ, CZ1, DT, CETA (ICBOT), CDETA (ICBOT), &
            CQ, CPSI, CPSIN, CF (ICBOT), CDF (ICBOT), CBETM (ICBOT), &
            CDBETM (ICBOT), CDBTMM (ICBOT), CPSI1, CPSIN1, CGAM1, CGAM2, &
            CDGAM1, CDGAM2, CA (ICBOT), CB (ICBOT), CC (ICBOT), CR (ICBOT), &
            DWORK1)
! add top boundary condition
         SOIL = ICSOIL (ICTOP)


         CALL VSUPPR (CA0, CDELZ (ICTOP), VSK3D (SOIL, 3), DT, CDNET, &
            CPSI (ICTOP), CB (ICTOP), CR (ICTOP), CQV (ICTOP) )
! add well abstraction (type 1)
         BTYPE = JCBC (5)
         IF (BTYPE.EQ.1) THEN


            CALL VSWELL (NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL (ICWLBT), &
               CA0, CDELZ (ICWLBT), CQWIN, CPSI (ICWLBT), CR (ICWLBT), &
               CQWI, DWORK1)
! add spring discharge (type 2)
         ELSEIF (BTYPE.EQ.2) THEN
            CALL VSSPR (CZ (ICSPCE), CZSP, CCS, CPSI (ICSPCE), CKR ( &
               ICSPCE), CDKR (ICSPCE), CB (ICSPCE), CR (ICSPCE), CQSP)


         ENDIF
! add user-defined lateral boundary conditions (types 3-5)
         DO 20 IFA = 1, 4
            BTYPE = JCBC (IFA)
            IF (BTYPE.GE.3.AND.BTYPE.LE.5) THEN


               CALL VSBC (BCHELE, IFA, ICBOT, ICTOP, JCBC (IFA), &
                  ICLYRB, ICLFN, ICLFL, ICLHN, ICLHL, CZG, CDELL (IFA), &
                  CDELZ, CZ, CAIJ, CLF, CLH, CPSI, CKIJ (ICBOT, IFA), &
                  CDKIJ (ICBOT, IFA), CB (ICBOT), CR (ICBOT), CQH, DWORK1)
! add stream-aquifer interaction (types 9 and 10)
            ELSEIF (BTYPE.EQ.9.OR.BTYPE.EQ.10) THEN
               CALL VSSAI (IFA, JCBC (IFA), ICBOT, ICTOP, ICBED, CDELL ( &
                  IFA), CZ, CAIJ, CZS (IFA), CPSI, CKIJ (ICBOT, IFA), &
                  CDKIJ (ICBOT, IFA), CB (ICBOT), CR (ICBOT), CQH, depadj ( &
                  ifa), cdelz)
            ENDIF


20       END DO
! add lower boundary condition (types 6-8)
         SOIL = ICSOIL (ICBOT)
!         CALL VSLOWR(JCBC(0),CA0,CZ,CDELZ,VSK3D(SOIL,3),
!     $               CBF,CBH,CPSI,CKR(ICBOT),CDKR(ICBOT),
!     $            CB(ICBOT),CR(ICBOT), CQV)



         CALL VSLOWR (JCBC (0), CA0, CZ (icbot), CDELZ (icbot), VSK3D ( &
            SOIL, 3), CBF, CBH, CPSI (icbot), CKR (ICBOT), CDKR (ICBOT), &
            CB (ICBOT), CR (ICBOT), CQV (icbot - 1) )
! solve linear equations


         CALL TRIDAG (CA (ICBOT:ICTOP), CB (ICBOT:ICTOP), CC (ICBOT:ICTOP), CR (ICBOT:ICTOP), &
            CDPSI (ICBOT:ICTOP), NDUM)
! update PSI array and check for convergence
         DPSIMX = ZERO
         DO 100 ICL = ICBOT, ICTOP
            DPSI = CDPSI (ICL)
            CPSI (ICL) = CPSI (ICL) + DPSI
            DPSIMX = MAX (DPSIMX, ABS (DPSI) )
100      END DO

         IF (DPSIMX.LE.CEPSMX) g510=.TRUE. !GOTO 510
!                              >>>>>>>>
      ENDDO out500

!    write (789,*), uznow, cqwin,cqwi(2),cr(2)


      IF (ELEVEL.GT.0) then
         errorcount=errorcount+1
         if (errorcount.lt.errcntallowed) then
            CALL ERROR (WWWARN, 1036, PPPRI, IEL, 0, 'Maximum iterations in VSS column solver')
         elseif (errorcount.eq.errcntallowed) then
            CALL ERROR (WWWARN, 1036, PPPRI, IEL, 0, '**** Last printout of the error message - maximum iterations error in VSS column solver *****')
         endif
      endif

510   CONTINUE
! Calculate final values of output variables
!____________________________________________*
! flows
      DO 600 ICL = ICBOT, ICTOP - 1
         PCL = ICL + 1
         CQV (ICL) = CBETM (PCL) * (CZ (ICL) + CPSI (ICL) - CZ (PCL) &
            - CPSI (PCL) ) / CA0

600   END DO
      DO 650 J = 1, 4
         IF (JELDUM (J) .LT.1) GOTO 650
!                            >>>>>>>>

         DO 640 I = ICBOT, ICTOP
            K = JCACN (J, I)
            IF (K.LT.1) GOTO 640
!                       >>>>>>>>
            K1 = K + JCDEL1 (K, J)
            H0 = CZ (I) + CPSI (I)
            H1 = CZ1 (K, J) + CPSI1 (K, J)

            H2 = CZ1 (K1, J) + CPSI1 (K1, J)

            CQH (J, I) = CGAM1 (I, J) * (H1 - H0) + CGAM2 (I, J) &
               * (H2 - H0)
640      END DO


650   END DO
! phreatic surface level
      CPSMIN = CZ (ICBOT) - half * CDELZ (ICBOT)
      DO 920 ICL = ICBOT, ICTOP
920   IF (CPSI (ICL) .LT.ZERO) GOTO 940
940   ICL = MAX (ICBOT, ICL - 1)

      CPSL = MAX (CPSMIN, CZ (ICL) + CPSI (ICL) )
   END SUBROUTINE VSCOLM



!> Builds VSS cell thicknesses, node elevations, and cell connectivity.
!>
!> `VSCONC` translates the manual `VS06` soil-zone cell depths, `VS07`
!> river-bed cell depths, `VS08` aquifer-zone layer definitions, `VS09` river
!> bed geometry, and the layer connectivity prepared by [[vsconl]] into the
!> cell mesh used by the VSS solver.
!>
!> Required setup conditions include positive VSS array bounds, `VSZMAX > 0`,
!> non-negative `VSZMIN`, `NCSZON`, and `NCRBED`, `LLEE >= NCSZON`, and enough
!> model layer capacity (`NLYREE > NLYR`) for active land elements. Soil-zone
!> cell depths `DCSZON` must be at least `VSZMIN`; aquifer layer boundaries
!> must be ordered and compatible with the prescribed soil-zone depths; and
!> regular neighbour faces in `ICMREF` must reference valid elements and faces.
!> The routine is designed to be called once during VSS initialisation.
!>
!> Cell construction proceeds bottom-up. Aquifer layers are subdivided into
!> equal cells no larger than `VSZMAX`, unless later connectivity checks require
!> additional subdivision. The soil zone is then appended from the manual
!> top-down `DCSZON` depths, and link elements receive additional river-bed
!> cells from `DCRBED`. Bank elements are mirrored across each link when
!> explicit banks are enabled.
!>
!> The main outputs are:
!>
!> | Array | Meaning |
!> |:------|:--------|
!> | `DELTAZ(cell,element)` | VSS cell thickness. |
!> | `ZVSNOD(cell,element)` | Cell-node elevation. |
!> | `NLYRBT(element,layer)` | Bottom-cell index for each model layer. |
!> | `top_cell_no` / `LL` | Maximum active cell index after renumbering. |
!> | `JVSACN(face,cell,element)` | Adjacent cell connected across a face. |
!> | `JVSDEL(face,cell,element)` | Split-cell offset used when one cell connects to two neighbour cells. |
!> | `NHBED`, `FHBED` | River-bed cell index and bed fraction metadata for channel links. |
!>
!> Connectivity is first direct-matched in the soil zone and below river beds.
!> Aquifer-zone connectivity follows `JVSALN`, which encodes the layer ranges
!> allowed to exchange laterally. When two connected layer ranges have too few
!> cells to represent the required one-to-one or one-to-two split-cell
!> connections, `VSCONC` records extra layer subdivisions in `LRENUM` and
!> rebuilds the mesh. If repeated rebuilding reaches the element-count limit,
!> the routine exits through the existing fatal-error path.
!>
!> @note
!> `LRENUM` and `NRENUM` are `DATA`-initialised local variables and therefore
!> retain state between calls. The routine also calls [[initialise_vsmod]] and
!> `INITIALISE_AL_C` after each mesh-construction pass, before the final
!> `BRENUM` test can jump back for another pass. This matches the original
!> one-call setup assumption; repeated calls, or a rebuild after allocation
!> routines that do not tolerate repeated allocation, are not safe.
!> @endnote
   SUBROUTINE VSCONC ()
!INTRINSIC DIM, INT, MAX, MIN, MOD
      INTEGER :: JVSDUM, NMOD
      PARAMETER (JVSDUM = NELEE * NLYREE, NMOD = NLYREE+1)
      INTEGER :: I, IRANGE, IBOT, IBOTL, ICL, IEL, IFA, ILYR, ITOP
      INTEGER :: J, JRANGE, JBOT, JBOTL, JCL, JEL, JFA, JLYR, JTOP
      INTEGER :: IDEL, IDEL0, IL, ILMAX, ILMIN, NITOT, NIMIN
      INTEGER :: JDEL, JDEL0, JL, JLMAX, JLMIN, NJTOT, NJMIN
      INTEGER :: IAQTOP, IBANK2, IBK, ICL0, ICL1, ICOL1, ILINK, ITYPE
      INTEGER :: DEL, JDIF, K, K2, K20, K2MOD, LCON, LTOP, &
         NRENUM
      INTEGER :: NACELL, NCELL, NCL, NCLYR, NDUM, NEXTRA, NODD, NUM2
      INTEGER :: LRENUM (NELEE, NLYREE), NIDUM (LLEE), NJDUM (LLEE), jedumdum
      DOUBLEPRECISION DZLYR, ZCBOT, ZDEPTH, ZBDBOT, ZCTOP, ZDUM
      DOUBLEPRECISION ZAQBOT, ZSZBOT, ZDIFF, ZLBOT, ZNODE
      LOGICAL :: BRENUM, BWARN, MISS, PAIR, BDONE (NELEE, 4)
      CHARACTER (LEN=57) :: MSG

      DATA LRENUM / JVSDUM * 0 /, NRENUM / 0 /


!FNCELL (I, IEL, ITOP) = IDIMJE(MIN (NLYRBT (IEL, I + 1), ITOP + 1), & !statement function replaced
! NLYRBT (IEL, I) )
!----------------------------------------------------------------------*
! >>> return to here if cells have to be re-numbered
210   NRENUM = NRENUM + 1
! >>>
      IF (NRENUM.GT.NELEE) GOTO 8048
      BWARN = NRENUM.EQ.NELEE





      BRENUM = .FALSE.
! Set initial indices, dimensions & positions of cells
!______________________________________________________*
! set values as follows:
!        for element e:     IDUM(e)  the number of cells
!                       DELTAZ(c,e)  the size of cell c
!                       ZVSNOD(c,e)  the nodal elevation of cell c
!        also: LL  the maximum value of IDUM over all elements
! --- loop over elements
      top_cell_no = 0
      DO 1000 IEL = total_no_links + 1, total_no_elements
         ITYPE = ICMREF (IEL, 1)
!           * process only grid and bank-1 elements here
!           * (links & bank-2's are treated in the bank-1 pass)


         IF (ITYPE.EQ.2) GOTO 1000
!                           >>>>>>>>>
! --- loop over layers in aquifer zone (start from bottom of column)
         ZSZBOT = ZGRUND (IEL) - DCSTOT
!           NB: ICL used as a counter in loops below; cell 1 is a dummy
         ICL = 1
         DELTAZ (ICL, IEL) = ZERO
!970612            DELTAZ(ICL,IEL) =                 VSZMIN
!970612            ZVSNOD(ICL,IEL) = ZLYRBT(IEL,1) - VSZMIN*half
         ZVSNOD (ICL, IEL) = ZERO
!^^^^^^


         DO 950 ILYR = 1, NLYR (IEL)
!              * divide each layer into equal sized cells
            ZLBOT = ZLYRBT (IEL, ILYR)
            DZLYR = MIN (ZLYRBT (IEL, ILYR + 1), ZSZBOT) - ZLBOT
!              skip if layer is thinner than minimum cell size
!970422        NB  if ZLBOT lies in aquifer zone this will leave a gap!
            IF (DZLYR.LT.VSZMIN) GOTO 950
!                                       >>>>>>>>
!              if no other plan make cells as large as poss but < VSZMAX
            NCLYR = LRENUM (IEL, ILYR)
            IF (NCLYR.LE.0) NCLYR = MAX (1, INT (DZLYR / VSZMAX) &
               + 1)
            ZDEPTH = DZLYR / NCLYR
            DO 920 I = 1, NCLYR
               ICL = ICL + 1
               DELTAZ (ICL, IEL) = ZDEPTH
               ZVSNOD (ICL, IEL) = ZDEPTH * (I - half) + ZLBOT

920         END DO


950      END DO
! --- set up data for soil zone (note DCSZON index is from the top down)
         ZAQBOT = ZLYRBT (IEL, 1)
         ZCBOT = ZSZBOT

         DO 960 I = NCSZON, 1, - 1
            ZDEPTH = DCSZON (I)
            ZNODE = ZCBOT + ZDEPTH * half
            IF (ZNODE.GT.ZAQBOT) THEN
               ICL = ICL + 1
               DELTAZ (ICL, IEL) = ZDEPTH
               ZVSNOD (ICL, IEL) = ZNODE
            ENDIF

            ZCBOT = ZCBOT + ZDEPTH


960      END DO
! --- update LL & store number of cells for this column
         top_cell_no = MAX (top_cell_no, ICL)


         IDUM (IEL) = ICL
! --- process link and opposite bank elements, if IEL is bank type 1


         IF (ITYPE.NE.1) GOTO 1000
!                           >>>>>>>>>
!           * set up link cells up to bottom of link bed
         ILINK = ICMREF (IEL, 4)
         ZBDBOT = ZBEFF (ILINK) - DCRTOT

         ZCBOT = ZLYRBT (IEL, 1)

         DO 974 ICL1 = 1, ICL
            ZDEPTH = DELTAZ (ICL1, IEL)
            ZCTOP = ZCBOT + ZDEPTH
            IF (ZCTOP.GT.ZBDBOT) GOTO 976
!                                   >>>>>>>>
            DELTAZ (ICL1, ILINK) = ZDEPTH
            ZVSNOD (ICL1, ILINK) = ZVSNOD (ICL1, IEL)

            ZCBOT = ZCTOP


974      END DO
!          < this point won't be traversed unless bank is below bed.
!           cell just below link bed: smaller than bank, unless ...
976      ZDEPTH = ZBDBOT - ZCBOT
         IF (ZDEPTH.LT.VSZMIN) THEN
!               ... remainder is small: add it to the cell below
            ICL1 = ICL1 - 1
            ZDEPTH = ZDEPTH + DELTAZ (ICL1, ILINK)
         ENDIF
         DELTAZ (ICL1, ILINK) = ZDEPTH

         ZVSNOD (ICL1, ILINK) = ZBDBOT - half * ZDEPTH
!           set up link bed cells (note DCRBED index is top-down)
         ZCBOT = ZBDBOT
         DO 980 I = NCRBED, 1, - 1
            ZDEPTH = DCRBED (I)
            ICL1 = ICL1 + 1
            DELTAZ (ICL1, ILINK) = ZDEPTH
            ZVSNOD (ICL1, ILINK) = ZCBOT + ZDEPTH * half
            ZCBOT = ZCBOT + ZDEPTH

980      END DO
!           update LL & store number of cells for the link
         top_cell_no = MAX (top_cell_no, ICL1)

         IDUM (ILINK) = ICL1
!           set up opposite bank cells
         IBANK2 = ICMBK (ILINK, 2)
         DO 985 I = 1, ICL
            DELTAZ (I, IBANK2) = DELTAZ (I, IEL)
            ZVSNOD (I, IBANK2) = ZVSNOD (I, IEL)
985      END DO

         IDUM (IBANK2) = ICL




1000  END DO
! Renumber cells (so that the top cell number is LL) & set up NLYRBT
!____________________________________________________________________*
! --- set number of first column element
      IF (BEXBK) THEN
         ICOL1 = 1
      ELSE
         ICOL1 = total_no_links + 1
!!!
!            * temporary measure to avoid out-of-bounds errors, etc
         DO 1080 ILINK = 1, total_no_links
1080     NLYRBT (ILINK, 1) = top_cell_no
!!!


      ENDIF
! --- loop over column elements

      DO 1200 IEL = ICOL1, total_no_elements
!           * shuffle values in DELTAZ & ZVSNOD, and zero remainder
         NCL = IDUM (IEL)
         ICL0 = top_cell_no - NCL
         IF (ICL0.GT.0) THEN
            DO 1100 I = NCL, 1, - 1
               ICL = ICL0 + I
               DELTAZ (ICL, IEL) = DELTAZ (I, IEL)
               ZVSNOD (ICL, IEL) = ZVSNOD (I, IEL)
1100        END DO
            CALL ALINIT (ZERO, ICL0, DELTAZ (1, IEL) )
            CALL ALINIT (ZERO, ICL0, ZVSNOD (1, IEL) )

         ENDIF
!           * find bottom cell in each layer
!970612
         ICL0 = ICL0 + 1
!^^^^^^
         DO 1150 ILYR = 1, NLYR (IEL)
            DO 1120 ICL = ICL0 + 1, top_cell_no
1120        IF (ZVSNOD (ICL, IEL) .GT.ZLYRBT (IEL, ILYR) ) GOTO 1130
1130        NLYRBT (IEL, ILYR) = ICL
            ICL0 = ICL - 1
1150     END DO

         NLYRBT (IEL, ILYR) = top_cell_no + 1





1200  END DO

      CALL INITIALISE_VSMOD()
      CALL INITIALISE_AL_C()



! Set up cell connectivities (JVSACN, JVSDEL)
!_____________________________________________*
! --- initialise arrays first
      DO 1260 IEL = 1, total_no_elements
         IBOT = NLYRBT (IEL, 1)
         DO 1240 IFA = 1, 4
            BDONE (IEL, IFA) = .FALSE.
            DO 1220 ICL = IBOT, top_cell_no
               JVSACN (IFA, ICL, IEL) = 0
               JVSDEL (IFA, ICL, IEL) = 0
1220        END DO
1240     END DO



1260  END DO
! ----- start of loop over (faces of) elements
      LTOP = top_cell_no - NCRBED
      IAQTOP = top_cell_no - NCSZON

      DO 1600 IEL = ICOL1, total_no_elements
         ITYPE = ICMREF (IEL, 1)
         IBOT = NLYRBT (IEL, 1)
         IF (IEL.LE.total_no_links) THEN
            IBK = ICMBK (IEL, 1)
            ITOP = MIN (IAQTOP + IBOT - NLYRBT (IBK, 1), LTOP)
         ELSE
            ITOP = IAQTOP

         ENDIF

         DO 1590 IFA = 1, 4
            JEL = ICMREF (IEL, IFA + 4)
            IF (JEL.LT.ICOL1) GOTO 1590
!                                >>>>>>>>>
            JFA = ICMREF (IEL, IFA + 8)
            IF (BDONE (JEL, JFA) ) GOTO 1590
!                                >>>>>>>>>
            JBOT = NLYRBT (JEL, 1)


            JDIF = JBOT - IBOT
! --- channel link-bank face: cells below river bed explicitly matched
!              * NB: layer connectivity & soil zone are disregarded here
            IF (IEL.LE.total_no_links.AND.JEL.GT.total_no_links) THEN
               DO 1280 ICL = IBOT, LTOP
                  JCL = ICL + JDIF
                  JVSACN (IFA, ICL, IEL) = JCL
                  JVSACN (JFA, JCL, JEL) = ICL
1280           END DO
               GOTO 1585
!                  >>>>>>>>>


            ENDIF
! --- other elements (grid-grid, grid-bank, or end-to-end banks/links)
            IF (JEL.LE.total_no_links) THEN
               IBK = ICMBK (JEL, 1)
               JTOP = MIN (IAQTOP + JBOT - NLYRBT (IBK, 1), LTOP)
               LCON = LTOP
            ELSE
               JTOP = IAQTOP
               LCON = top_cell_no


            ENDIF
! ----- soil zone processing
!              * one-to-one for all active (except river-bed) cells
            jedumdum = MAX (IBOT, JBOT)
            jedumdum = MAX (jedumdum, ITOP + 1, JTOP + 1)
            !""AD DO 1322 ICL = MAX (IBOT, JBOT, ITOP + 1, JTOP + 1), LCON
            DO 1322 ICL = jedumdum , LCON
               JCL = ICL
               JVSACN (IFA, ICL, IEL) = JCL
               JVSACN (JFA, JCL, JEL) = ICL


1322        END DO
! ----- aquifer zone processing
!              * loop over layers, starting at the bottom
            ILYR = 1
            JLYR = 1

1410        CONTINUE
            IBOTL = NLYRBT (IEL, ILYR)
            JBOTL = NLYRBT (JEL, JLYR)
            IF (IBOTL.GT.ITOP.OR.JBOTL.GT.JTOP) GOTO 1585
!                                                         >>>>>>>>>
            JRANGE = JVSALN (IEL, ILYR, IFA)

            IRANGE = JVSALN (JEL, JLYR, JFA)
            IF (JRANGE.EQ.0) THEN
               ILYR = ILYR + 1
               GOTO 1410
!                     <<<<<<<<<
            ELSEIF (IRANGE.EQ.0) THEN
               JLYR = JLYR + 1
               GOTO 1410
!                     <<<<<<<<<

            ENDIF
!                 * range of layers to process on this pass
            ILMIN = IRANGE / NMOD
            ILMAX = MOD (IRANGE, NMOD)
            JLMIN = JRANGE / NMOD

            JLMAX = MOD (JRANGE, NMOD)
!                 * count cells in column IEL, & no. required in JEL
            NITOT = 0
            NJMIN = 0
            NODD = 0
            DO 1470 IL = ILMIN, ILMAX
               NCELL = FNCELL (IL, IEL, ITOP)
               IF (JVSALN (IEL, IL, IFA) .NE.0) THEN
                  DO 1460 I = 0, NCELL - 1
                     NITOT = 1 + NITOT
                     NIDUM (NITOT) = I + NLYRBT (IEL, IL)
1460              END DO
                  NCELL = NCELL - NODD
                  NJMIN = (NCELL + 1) / 2 + NJMIN
                  NODD = MOD (NCELL, 2)
               ELSEIF (NCELL.GT.0) THEN
                  NODD = 0
               ENDIF
1470        END DO

            NIDUM (NITOT + 1) = 0
!                 * count cells in column JEL, & no. required in IEL
            NJTOT = 0
            NIMIN = 0
            NODD = 0
            DO 1570 JL = JLMIN, JLMAX
               NCELL = FNCELL (JL, JEL, JTOP)
               IF (JVSALN (JEL, JL, JFA) .NE.0) THEN
                  DO 1560 J = 0, NCELL - 1
                     NJTOT = 1 + NJTOT
                     NJDUM (NJTOT) = J + NLYRBT (JEL, JL)
1560              END DO
                  NCELL = NCELL - NODD
                  NIMIN = (NCELL + 1) / 2 + NIMIN
                  NODD = MOD (NCELL, 2)
               ELSEIF (NCELL.GT.0) THEN
                  NODD = 0
               ENDIF
1570        END DO

            NJDUM (NJTOT + 1) = 0

            IF (NITOT.EQ.0.AND.NJTOT.GT.0) THEN
!                     * I-layers are empty
               WRITE (MSG, 9200) JFA, JLYR

               IF (NRENUM.EQ.1) CALL ERROR(WWWARN, 1053, PPPRI, JEL, 0, &
                  MSG)

            ELSEIF (NJTOT.EQ.0.AND.NITOT.GT.0) THEN
!                     * J-layers are empty
               WRITE (MSG, 9200) IFA, ILYR

               IF (NRENUM.EQ.1) CALL ERROR(WWWARN, 1053, PPPRI, IEL, 0, &
                  MSG)

            ELSEIF (NJTOT.LT.NJMIN) THEN
!                     * need more J-cells
               BRENUM = .TRUE.
               NEXTRA = 0
               DO 1572 JL = JLMIN, JLMAX
                  IF (JVSALN (JEL, JL, JFA) .NE.0) THEN
                     IF (BWARN) THEN
                        WRITE (MSG, 9300) JFA, JL
                        CALL ERROR(WWWARN, 1037, PPPRI, JEL, 0, MSG)
                     ENDIF
                     NCELL = FNCELL (JL, JEL, JTOP)
                     NDUM = NCELL * NJMIN + NEXTRA + NJTOT / 2
                     LRENUM (JEL, JL) = NDUM / NJTOT
                     NEXTRA = MOD (NDUM, NJTOT) - NJTOT / 2
                  ENDIF

1572           END DO

            ELSEIF (NITOT.LT.NIMIN) THEN
!                     * need more I-cells
               BRENUM = .TRUE.
               NEXTRA = 0
               DO 1574 IL = ILMIN, ILMAX
                  IF (JVSALN (IEL, IL, IFA) .NE.0) THEN
                     IF (BWARN) THEN
                        WRITE (MSG, 9300) IFA, IL
                        CALL ERROR(WWWARN, 1037, PPPRI, IEL, 0, MSG)
                     ENDIF
                     NCELL = FNCELL (IL, IEL, ITOP)
                     NDUM = NCELL * NIMIN + NEXTRA + NITOT / 2
                     LRENUM (IEL, IL) = NDUM / NITOT
                     NEXTRA = MOD (NDUM, NITOT) - NITOT / 2
                  ENDIF

1574           END DO

            ELSE
!                     * how many splits possible, & how many to forego
               IF (NITOT.GE.NJTOT) THEN
                  IDEL0 = 1
                  NUM2 = NITOT - NJMIN
                  NEXTRA = NJTOT - NJMIN
               ELSE
                  IDEL0 = 0
                  NUM2 = NJTOT - NIMIN
                  NEXTRA = NITOT - NIMIN
               ENDIF
               JDEL0 = 1 - IDEL0

               CALL ALSPRD (NEXTRA, NUM2, K20, K2MOD)
!                     * loop over all cells found
               MISS = .FALSE.
               K2 = - K20
               I = 1
               J = 1

1575           IF (I.LE.NITOT.AND.J.LE.NJTOT) THEN
                  PAIR = NIDUM (I + IDEL0) .EQ.NIDUM (I) + 1
                  PAIR = NJDUM (J + JDEL0) .EQ.NJDUM (J) + 1.OR.PAIR
                  PAIR = .NOT.MISS.AND.PAIR
                  IF (PAIR) THEN
                     K2 = K2 + 1
                     MISS = K2.GE.0.AND.MOD (K2, K2MOD) .EQ.0
                     MISS = K2.LE. (NEXTRA - 1) * K2MOD.AND.MISS
                     PAIR = .NOT.MISS
                  ELSE
                     MISS = .FALSE.
                  ENDIF
                  DEL = 0
                  IF (PAIR) DEL = 1
                  IDEL = IDEL0 * DEL
                  JDEL = JDEL0 * DEL
                  DO 1580 K = 0, DEL
                     ICL = NIDUM (I)
                     JCL = NJDUM (J)
                     IF (IDEL.GE.K) JVSACN (IFA, ICL, IEL) = JCL
                     IF (JDEL.GE.K) JVSACN (JFA, JCL, JEL) = ICL
                     JVSDEL (IFA, ICL, IEL) = IDEL * (1 - 2 * K)
                     JVSDEL (JFA, JCL, JEL) = JDEL * (1 - 2 * K)
                     I = I + IDIMJE(IDEL, K)
                     J = J + IDIMJE(JDEL, K)
1580              END DO
                  I = I + 1
                  J = J + 1

                  GOTO 1575
!                         <<<<<<<<<

               ENDIF

            ENDIF
!                 * move on to next layers
            ILYR = ILMAX + 1
            JLYR = JLMAX + 1

            GOTO 1410
!              <<<<<<<<<

1585        BDONE (IEL, IFA) = .TRUE.

1590     END DO



1600  END DO
! Repeat the whole thing if necessary
!_____________________________________*
!                 <<<<<<<<



      IF (BRENUM) GOTO 210
!                 <<<<<<<<
! Finish off
!____________*

      WRITE (PPPRI, 9000) top_cell_no


      DO 2100 IEL = ICOL1, total_no_links
!        * adjust elevations for link cells (to make room for river-bed)
         IBK = ICMBK (IEL, 1)
         NACELL = LTOP + NLYRBT (IBK, 1) - NLYRBT (IEL, 1)
         ZDUM = DELTAZ (NACELL, IBK)

         ZDIFF = ZDUM - DELTAZ (LTOP, IEL)

         DELTAZ (LTOP, IEL) = ZDUM
         DO 2050 ICL = NLYRBT (IEL, 1), LTOP - 1
2050     ZVSNOD (ICL, IEL) = ZVSNOD (ICL, IEL) - ZDIFF
         ZVSNOD (ICL, IEL) = ZVSNOD (ICL, IEL) - ZDIFF * half
         DO 2060 ILYR = 1, NLYR (IEL)

2060     ZLYRBT (IEL, ILYR) = ZLYRBT (IEL, ILYR) - ZDIFF
!        * NB. banks 1 and 2 are identical
         NHBED (IEL, 1) = NACELL
         NHBED (IEL, 2) = NACELL
         FHBED (IEL, 1) = ZERO

         FHBED (IEL, 2) = ZERO

2100  END DO

      RETURN

8048  CALL ERROR(FFFATAL, 1048, PPPRI, 0, 0, 'Attempts to renumber cells have failed.')
9000  FORMAT(/ 'Number of top cell in all columns (LL) = ',I3)
9200  FORMAT('Null cell connectivity being set up for face ',I1, &
      &       ' layer ',I2)
9300  FORMAT(  'Not possible to connect all cells for face ',I1, &
      &       ' layer ',I2)
   END SUBROUTINE VSCONC

!> Returns the number of VSS cells spanned by one model layer interval.
   INTEGER FUNCTION fncell(I, IEL, ITOP)
      INTEGER, INTENT(IN) :: I    !! Model-layer index.
      INTEGER, INTENT(IN) :: IEL  !! Element number.
      INTEGER, INTENT(IN) :: ITOP !! Upper active cell bound used to clip the layer top.
      fncell = IDIMJE(MIN(NLYRBT (IEL, I + 1), ITOP + 1), NLYRBT (IEL, I) )
   END FUNCTION fncell



!> Builds the layer-to-layer lateral connectivity matrix.
!>
!> `VSCONL` builds the layer-level lateral connectivity used later by
!> [[vsconc]] to create cell-level links. It combines default aquifer-zone
!> matching with the manual `VS10`/`VS10a` user-defined aquifer connectivity
!> records (`IAQCON`).
!>
!> Required setup conditions are that the routine is called at most once per
!> run; `NAQCON <= NVSEE`; `NELEE >= NEL >= 1`; `NLF >= 0`; `NLYREE` can hold
!> all `NLYR` values; and neighbour references in `ICMREF` point to valid
!> elements/faces for active column elements. If explicit banks are not present,
!> active VSS columns start at `NLF+1`; otherwise links and bank elements are
!> included from element 1.
!>
!> `JVSALN(element,layer,face)` stores the range of layers in the adjacent
!> element connected to this layer. A value of zero means no lateral connection.
!> Non-zero ranges are encoded compactly as
!>
!> \[
!>   JVSALN = NMOD\,l_{min}+l_{max},\qquad NMOD=NLYREE+1.
!> \]
!>
!> For each neighbouring element pair, `IAQCON(:,i)` records are first checked
!> for this pair. Layer numbers must be in range. Positive user records are
!> accumulated into inclusive connected-layer ranges, while conflicting
!> null/non-null records are reported as error 1038 and counted in `NVSERR`.
!>
!> Default matching starts immediately below the soil-zone depth
!> \(ZGRUND-DCSTOT\), using a small tolerance to avoid roundoff at exact layer
!> boundaries. When no user record overrides the pair, layers with the same
!> soil/lithology type are connected one-to-one. If soil types differ, the
!> routine skips downward through one or both columns until it finds the next
!> compatible soil type or user-specified connection, trying to preserve
!> continuity where possible. Boundary faces, branched channels, and link-flank
!> faces receive null connectivity.
!>
!> @warning
!> The legacy comments describe layer zero in `IAQCON` as an explicit null
!> connection, but a new one-sided zero record is not stored as a simple null
!> marker by the current range-building code. Use positive layer-pair records
!> for user-defined connectivity and do not rely on one-sided zero records to
!> block default matching.
!> @endwarning
!>
!> @note
!> The local `BDONE` array is `DATA`-initialised and retained between calls.
!> This is another reason the routine follows the original one-call setup
!> assumption.
!> @endnote
   SUBROUTINE VSCONL (NAQCON, IAQCON)
      INTEGER, INTENT(IN) :: NAQCON    !! Number of user-defined aquifer connectivity records.
      INTEGER, INTENT(IN) :: IAQCON(4,*) !! User aquifer connectivity records: element/layer pairs for adjacent columns.
! Locals, etc
!INTRINSIC MAX, MIN, MOD
      INTEGER :: NMOD
      DOUBLEPRECISION ZSMALL
      PARAMETER (NMOD = NLYREE+1, ZSMALL = 1D-6)
      INTEGER :: I, J, ILYR, JLYR, IEL, JEL, IFA, JFA, NLYRI, NLYRJ
      INTEGER :: ILMIN, ILMAX, JLMIN, JLMAX, IRANGE, JRANGE, ISOIL, &
         JSOIL
      INTEGER :: ISKIP, JSKIP, ISOILP, JSOILP, I1, I2, ICOL1, K, KEL
      INTEGER :: ILDUM (NLYREE), JLDUM (NLYREE)
      DOUBLEPRECISION ZSZBOT
      LOGICAL :: IOK, MOVEJ, TEST1, BDONE (NELEE)
      CHARACTER (LEN=132) :: MSG


      DATA BDONE / NELEE * .FALSE. /
!----------------------------------------------------------------------*
      IF (BEXBK) THEN
         ICOL1 = 1
      ELSE
         ICOL1 = total_no_links + 1


      ENDIF
! ----- default is null connectivity
      DO 50 IFA = 1, 4
         DO 50 IEL = 1, total_no_elements
            DO 50 ILYR = 1, NLYR (IEL) + 1
               JVSALN (IEL, ILYR, IFA) = 0



50    CONTINUE
! Main loop over (faces of) column elements
!___________________________________________*

      DO 500 IEL = ICOL1, total_no_elements

         NLYRI = NLYR (IEL)

         DO 400 IFA = 1, 4


            JEL = ICMREF (IEL, IFA + 4)
! null connectivity for boundary faces, branched channels & link flanks


            IF (JEL.LT.ICOL1.OR. (IEL.LE.total_no_links.AND.JEL.GT.total_no_links) ) GOTO 400
!                                                            >>>>>>>>
! skip rest of loop if face already processed ...


            IF (BDONE (JEL) ) GOTO 400
!                           >>>>>>>>
! ... else process BOTH sides of face
            NLYRJ = NLYR (JEL)



            JFA = ICMREF (IEL, IFA + 8)
! check for user-defined layer connectivity for this pair of elements
! ILDUM(ilyr) is the layer in column JEL connected to layer 'ilyr'
! if more than one layer is connected ILDUM = NMOD*min.layer + max.layer
! a value of zero specifies null connectivity
! NB this code also verifies the input data in IAQCON
            DO 102 I = 1, NLYRI
102         ILDUM (I) = - 1
            DO 104 J = 1, NLYRJ

104         JLDUM (J) = - 1

            DO 110 I = 1, NAQCON
               I1 = IAQCON (1, I)

               I2 = IAQCON (3, I)
!              * does entry I belong to the current pair of elements?
               IF (IEL.EQ.I1.AND.JEL.EQ.I2) THEN
                  K = 2
               ELSEIF (IEL.EQ.I2.AND.JEL.EQ.I1) THEN
                  K = 4
               ELSE
                  GOTO 110
!                  >>>>>>>>

               ENDIF
               ILYR = IAQCON (K, I)

               JLYR = IAQCON (6 - K, I)
               MSG = ' '

               IF (ILYR.LT.0.OR.ILYR.GT.NLYRI) THEN
!                  * ILYR out of range
                  KEL = IEL

                  WRITE (MSG, 9381) ILYR, I, IEL, NLYRI

               ELSEIF (JLYR.LT.0.OR.JLYR.GT.NLYRJ) THEN
!                  * JLYR out of range
                  KEL = JEL

                  WRITE (MSG, 9381) JLYR, I, JEL, NLYRJ

               ELSE
                  IF (ILYR.GT.0) THEN
                     JRANGE = ILDUM (ILYR)
                     TEST1 = JLYR.EQ.0.AND.JRANGE.GT.0
                     IF (JRANGE.EQ.0.OR.TEST1) THEN
!                          * invalid
                        KEL = IEL
                        JRANGE = MOD (JLYR + JRANGE, NMOD)
                        WRITE (MSG, 9382) IEL, ILYR, JRANGE, JEL, I
                     ELSE
                        IF (JRANGE.LT.0) JRANGE = NMOD * NLYRJ + 1
                        JLMIN = MIN (JLYR, JRANGE / NMOD)
                        JLMAX = MAX (JLYR, MOD (JRANGE, NMOD) )
                        ILDUM (ILYR) = NMOD * JLMIN + JLMAX
                     ENDIF

                  ENDIF
                  IF (JLYR.GT.0) THEN
                     IRANGE = JLDUM (JLYR)
                     TEST1 = ILYR.EQ.0.AND.IRANGE.GT.0
                     IF (IRANGE.EQ.0.OR.TEST1) THEN
!                          * invalid
                        KEL = JEL
                        IRANGE = MOD (ILYR + IRANGE, NMOD)
                        WRITE (MSG, 9382) JEL, JLYR, IRANGE, IEL, I
                     ELSE
                        IF (IRANGE.LT.0) IRANGE = NMOD * NLYRI + 1
                        ILMIN = MIN (ILYR, IRANGE / NMOD)
                        ILMAX = MAX (ILYR, MOD (IRANGE, NMOD) )
                        JLDUM (JLYR) = NMOD * ILMIN + ILMAX
                     ENDIF

                  ENDIF

               ENDIF
!              * note: MSG for ILYR>0.and.JRANGE=0 is lost
!              *       if also JLYR>0.and.IRANGE=0
               IF (MSG.NE.' ') THEN
                  CALL ERROR (EEERR, 1038, PPPRI, KEL, 0, MSG)
                  NVSERR = NVSERR + 1

               ENDIF


110         END DO
! set ILYR & JLYR to numbers of layers immediately below soil zone
!           ZSMALL is added to avoid rounding errors if the bottom
!           of a layer coincides with the bottom of the soil zone
!970711     ! expression for ZSZBOT is wrong for link elements
            ZSZBOT = ZGRUND (IEL) - DCSTOT - ZSMALL
            DO 120 ILYR = NLYRI, 1, - 1
120         IF (ZLYRBT (IEL, ILYR) .LT.ZSZBOT) GOTO 125
125         ZSZBOT = ZGRUND (JEL) - DCSTOT - ZSMALL
            DO 140 JLYR = NLYRJ, 1, - 1


140         IF (ZLYRBT (JEL, JLYR) .LT.ZSZBOT) GOTO 200
! --- start of loop over layers (downwards from top of aquifer zone)

200         IF (ILYR.EQ.0.OR.JLYR.EQ.0) GOTO 400
!                                           >>>>>>>>
            ISOIL = NTSOIL (IEL, ILYR)

            JSOIL = NTSOIL (JEL, JLYR)
            JRANGE = ILDUM (ILYR)

            IRANGE = JLDUM (JLYR)

            IF (JRANGE.EQ.0.OR. (IRANGE.GT.0.AND.JRANGE.LT.0) ) THEN
!                  * null

               ILYR = ILYR - 1

            ELSEIF (IRANGE.EQ.0.OR. (JRANGE.GT.0.AND.IRANGE.LT.0) ) &
               THEN
!                  * null

               JLYR = JLYR - 1

            ELSEIF (JRANGE.GT.0) THEN
!                  * user-specified
               JLMIN = JRANGE / NMOD

               ILMIN = IRANGE / NMOD
!                  * repeat until the whole connected range is processed
210            CONTINUE
!                     *
               ILMAX = ILYR
               DO 220 ILYR = ILMAX, ILMIN, - 1
                  JRANGE = ILDUM (ILYR)
                  JVSALN (IEL, ILYR, IFA) = MAX (0, JRANGE)
                  IF (JRANGE.GT.0) JLMIN = MIN (JLMIN, JRANGE / NMOD)
220            END DO
!                     *
               JLMAX = JLYR
               DO 240 JLYR = JLMAX, JLMIN, - 1
                  IRANGE = JLDUM (JLYR)
                  JVSALN (JEL, JLYR, JFA) = MAX (0, IRANGE)
                  IF (IRANGE.GT.0) ILMIN = MIN (ILMIN, IRANGE / NMOD)
240            END DO
!                     *

               IF (ILMIN.LE.ILYR) GOTO 210

            ELSEIF (ISOIL.EQ.JSOIL) THEN
!                  * matching soils
               JVSALN (IEL, ILYR, IFA) = JLYR * NMOD+JLYR
               JVSALN (JEL, JLYR, JFA) = ILYR * NMOD+ILYR
               ILYR = ILYR - 1

               JLYR = JLYR - 1


            ELSE
!                  * decide whether to move down column IEL or JEL:
!                  * set type of soil above
               ISOILP = 0
               IF (ILYR.LT.NLYRI) ISOILP = NTSOIL (IEL, ILYR + 1)
               JSOILP = 0

               IF (JLYR.LT.NLYRJ) JSOILP = NTSOIL (JEL, JLYR + 1)
!                  * look for next matching soil or user-specification
               DO 260 I = ILYR - 1, 1, - 1
260            IF (NTSOIL (IEL, I) .EQ.JSOIL.OR.ILDUM (I) .GE.0) GOTO &
                  265
265            ISKIP = ILYR - I
               DO 280 J = JLYR - 1, 1, - 1
280            IF (NTSOIL (JEL, J) .EQ.ISOIL.OR.JLDUM (J) .GE.0) GOTO &
                  285

285            JSKIP = JLYR - J
!                  * choose smallest skip; or preserve soil continuity
               MOVEJ = ISOIL.EQ.ISOILP.OR.JSOIL.NE.JSOILP
               MOVEJ = JSKIP.LT.ISKIP.OR.JSKIP.EQ.ISKIP.AND.MOVEJ
               MOVEJ = J.GT.0.AND.MOVEJ

               IF (MOVEJ) MOVEJ = JLDUM (J) .LT.0
!                  * would there be any point moving down IEL?
               IOK = I.GT.0

               IF (IOK) IOK = ILDUM (I) .LT.0
!                  * the choice is made
               IF (MOVEJ) THEN
                  JLYR = J
               ELSEIF (IOK) THEN
                  ILYR = I
               ELSE
                  ILYR = ILYR - 1
                  JLYR = JLYR - 1

               ENDIF

            ENDIF
!           * process next pair of layers

            GOTO 200

400      END DO

         BDONE (IEL) = .TRUE.



500   END DO
! Formats
!_________*
9381  FORMAT('Layer',I3,' out of range, IAQCON entry',I3, &
      &      ' (element',I5,' has',I3,' layers)')

9382  FORMAT('Invalid null connection, element',I5,':', &
      &      ' layer',I3,' already connected to layer',I3,', element',I5, &
      &      ' (see IAQCON entry',I3,')')
   END SUBROUTINE VSCONL



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
!> In the current restructured loop, an out-of-range cell sets `g8100` and
!> skips interpolation for the remainder of the column before the fatal
!> `ERROR` call is made after the loop. Output values after the offending cell
!> should therefore be treated as undefined on this path.
!> @endnote
   SUBROUTINE VSFUNC (NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
      VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ICSOIL, CPSI, ICSTOR, &
      CTHETA, CETA, CKR, CDETA, CDKR)
      INTEGER, INTENT(IN) :: NVSSOL                   !! Number of active soil lookup-table rows.
      INTEGER, INTENT(IN) :: NSOLEE                   !! Declared first dimension of the soil lookup tables.
      DOUBLEPRECISION, INTENT(IN) :: VSPPSI(NVSSOL)   !! Strictly decreasing lookup pressure-head ordinates.
      DOUBLEPRECISION, INTENT(IN) :: VSPTHE(NSOLEE,*) !! Lookup volumetric water content by row and soil type.
      DOUBLEPRECISION, INTENT(IN) :: VSPKR(NSOLEE,*)  !! Lookup relative hydraulic conductivity by row and soil type.
      DOUBLEPRECISION, INTENT(IN) :: VSPETA(NSOLEE,*) !! Lookup storage coefficient by row and soil type.
      DOUBLEPRECISION, INTENT(IN) :: VSPDKR(NSOLEE,*) !! Lookup derivative `d(K_r)/d(psi)` by row and soil type.
      DOUBLEPRECISION, INTENT(IN) :: VSPDET(NSOLEE,*) !! Lookup derivative `d(eta)/d(psi)` by row and soil type.
      INTEGER, INTENT(IN) :: IEL                      !! Element number used in diagnostics.
      INTEGER, INTENT(IN) :: ICBOT                    !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                    !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICSOIL(ICBOT:ICTOP)      !! Soil type by active cell.
      DOUBLEPRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Pressure head/potential by active cell.
      INTEGER, INTENT(INOUT) :: ICSTOR(ICBOT:ICTOP)   !! Cached lower lookup-table interval by active cell.
      DOUBLEPRECISION, INTENT(OUT) :: CTHETA(ICBOT:ICTOP) !! Interpolated volumetric water content.
      DOUBLEPRECISION, INTENT(OUT) :: CETA(ICBOT:ICTOP) !! Interpolated storage coefficient.
      DOUBLEPRECISION, INTENT(OUT) :: CKR(ICBOT:ICTOP) !! Interpolated relative hydraulic conductivity.
      DOUBLEPRECISION, INTENT(OUT) :: CDETA(ICBOT:ICTOP) !! Interpolated derivative `d(eta)/d(psi)`.
      DOUBLEPRECISION, INTENT(OUT) :: CDKR(ICBOT:ICTOP) !! Interpolated derivative `d(K_r)/d(psi)`.
! Locals, etc
!INTRINSIC MAX, MIN, NINT
      CHARACTER (LEN=5) :: WETDRY (0:1)
      DOUBLEPRECISION P, PDUM, VLO
      INTEGER :: ICL, INC, JHI, JLO, JM, IS, DRY
      LOGICAL :: g8100

      DATA WETDRY / '(wet)', '(dry)' /
!
!----------------------------------------------------------------------*
!
! ----- loop over all cells in column
      G8100=.FALSE.
      OUT100 : DO ICL = ICBOT, ICTOP
         IF(g8100) CYCLE
         P = CPSI (ICL)
         JLO = ICSTOR (ICL)



         IS = ICSOIL (ICL)
! --- find location in table of current psi value
! test for initial guess
         IF (JLO.LE.0.OR.JLO.GT.NVSSOL) THEN
            JLO = 0
            JHI = NVSSOL + 1
            GOTO 30


         ENDIF
! set initial hunt increment, and hunt up the table
         INC = 1

         IF (P.LE.VSPPSI (JLO) ) THEN
10          JHI = JLO + INC
            IF (JHI.GT.NVSSOL) THEN
               JHI = NVSSOL + 1
            ELSEIF (P.LE.VSPPSI (JHI) ) THEN
               JLO = JHI
               INC = INC + INC
               GOTO 10


            ENDIF
! hunt down the table

         ELSE
            JHI = JLO
20          JLO = JHI - INC
            IF (JLO.LT.1) THEN
               JLO = 0
            ELSEIF (P.GT.VSPPSI (JLO) ) THEN
               JHI = JLO
               INC = INC + INC
               GOTO 20

            ENDIF



         ENDIF
! hunt completed, begin bisection
!       At this point: { VSPPSI(JLO)>=P or JLO=0        } and
!                      { VSPPSI(JHI)< P or JHI=NVSSOL+1 }
30       IF (JHI - JLO.EQ.1) GOTO 50
         JM = (JHI + JLO) / 2
         IF (P.LT.VSPPSI (JM) ) THEN
            JLO = JM
         ELSE
            JHI = JM
         ENDIF
         GOTO 30
50       CONTINUE
         JLO = MAX (1, MIN (JLO, NVSSOL - 1) )

         JHI = JLO + 1


         ICSTOR (ICL) = JLO
! --- interpolate between values for return variables
         VLO = VSPPSI (JLO)
         PDUM = (P - VLO) / (VSPPSI (JHI) - VLO)

         IF (PDUM.LT.ZERO.OR.PDUM.GT.ONE) THEN  !GOTO 8100
            g8100=.TRUE.
            CYCLE out100
         ENDIF
         VLO = VSPTHE (JLO, IS)
         CTHETA (ICL) = (VSPTHE (JHI, IS) - VLO) * PDUM + VLO
         CETA (ICL) = VSPETA (JHI, IS)
         VLO = VSPDKR (JLO, IS)
         CDKR (ICL) = (VSPDKR (JHI, IS) - VLO) * PDUM + VLO
         VLO = VSPKR (JLO, IS)
         CKR (ICL) = (VSPKR (JHI, IS) - VLO) * PDUM + VLO
         VLO = VSPDET (JLO, IS)

         CDETA (ICL) = (VSPDET (JHI, IS) - VLO) * PDUM + VLO
      ENDDO out100
!RETURN
      IF(g8100) THEN
         DRY = NINT (MAX (ZERO, MIN (PDUM, ONE) ) )  !8100
         CALL ERROR(FFFATAL, 1034 + DRY, PPPRI, IEL, ICL, 'soil property interpolation out of range '//WETDRY (DRY) )
      ENDIF
   END SUBROUTINE VSFUNC



!> Initialises the VSS component.
!>
!> `VSIN` controls one-time setup of the VSS component before the first
!> timestep. It allocates shared run-size arrays, reads the manual VSS data
!> file through [[vsread]], initialises time-varying boundary input streams,
!> constructs layer and cell connectivity, builds soil hydraulic lookup tables,
!> and creates the initial pressure-head and conductivity fields.
!>
!> The setup sequence is:
!>
!> | Step | Action |
!> |:-----|:-------|
!> | Allocate shared storage | `INITIALISE_AL_C2` allocates arrays needed before `top_cell_no` is known. |
!> | Read VSS input | [[vsread]] loads `VSD` data and returns user aquifer connectivity `IAQCON`. |
!> | Prime boundary files | First records are read for well, lateral-flow/head, and bottom-flow/head files when their category counts are non-zero. |
!> | Build connectivity | [[vsconl]] creates layer connectivity and [[vsconc]] creates cells, node elevations, and cell connectivity. |
!> | Locate wells/springs | `NWELBT`, `NWELTP`, and `NVSSPC` are set from well screen depths and spring source depths. |
!> | Soil tables | [[vssoil]] builds pressure-head lookup tables for each soil/lithology. |
!> | Initial conditions | `INITYP` selects the initial pressure-head setup. |
!> | Initial conductivity | [[vsfunc]] checks/interpolates initial pressure heads and fills `VSKR`; `IVSSTO` stores lookup-table interval indices. |
!>
!> Initial-condition handling follows the manual `VS03`/`VSI` options:
!>
!> | `INITYP` | Initialisation |
!> |:---------|:---------------|
!> | 1 | Uniform phreatic-surface depth `VSIPSD`; equilibrium profile \(VSPSI=z_{psl}-z_{node}\). |
!> | 2 | Phreatic-surface elevations read from the `VSI` file; equilibrium profile. |
!> | 3 | Full cell pressure potentials read from `VSI`; `ZVSPSL` is derived from the highest non-negative pressure head. |
!>
!> Main outputs are well screen cell bounds `NWELBT`/`NWELTP`, spring source
!> cell `NVSSPC`, pressure heads `VSPSI`, phreatic-surface levels `ZVSPSL`,
!> lookup interval state `IVSSTO`, and initial relative conductivity `VSKR`.
!> Data-reading or initialisation errors accumulate in `NVSERR`; any non-zero
!> count raises fatal error 1040.
!>
!> @note
!> `ISTART` is `1` when explicit banks are present and `total_no_links+1`
!> otherwise. `INITYP=2` and `INITYP=3` therefore read `VSI` data only for
!> `ISTART:total_no_elements`, not necessarily for every manual element listed
!> in the `VSI` table. For `INITYP=3`, each element record must appear in that
!> exact increasing order; a mismatched `IEL` raises error 1041 and then the
!> accumulated fatal error 1040.
!> @endnote
!>
!> @warning
!> Well-screen depths (`VS12b`) and spring source depths (`VS13b`) are assumed
!> to fall inside the generated column cells. If a depth search fails, the code
!> falls through with the loop index beyond the searched range rather than
!> reporting a dedicated bounds error.
!> @endwarning
   SUBROUTINE VSIN ()
      CHARACTER(132) :: MSG
      INTEGER :: IEL, ICL, ILYR, ICBOT, ICTOP, IW, IELIN, ISTART, &
         NAQCON
      INTEGER :: IAQCON (4, NVSEE), ISDUM (LLEE)
      DOUBLEPRECISION DZ, RDUM, ZGI, ZMIN


      DOUBLEPRECISION CDUM1 (LLEE), CDUM2 (LLEE), CDUM3 (LLEE), CDUM4 ( &
         LLEE)
!----------------------------------------------------------------------*

!top_cell_no is unknown at this point. But the code to caculate top_cell_no uses DELTAZ and ZVSNOD so these use llee
      CALL INITIALISE_AL_C2()


      WRITE(PPPRI, 9010) 'Start', ' '

      NVSERR = 0
      IF (BEXBK) THEN
         ISTART = 1
      ELSE
         ISTART = total_no_links + 1


      ENDIF
! call VSREAD to read from input data file
      CALL VSREAD (NAQCON, IAQCON)


      IF (NVSERR.GT.0) GOTO 8900
! read first lines of time-varying files
      IF (NVSWL.GT.0) READ (WLD, * )
      IF (NVSLF.GT.0) READ (LFB, * )
      IF (NVSLH.GT.0) READ (LHB, * )
      IF (NVSBF.GT.0) READ (BFB, * )



      IF (NVSBH.GT.0) READ (BHB, * )
! call VSCONL and VSCONC to set up connectivity arrays for ...
! ... layers


      CALL VSCONL (NAQCON, IAQCON)
! ... cells


      CALL VSCONC

!no_of_hours_run = INT(TTH - TIH + 1.0d0)
!OPEN(unit=8798, file=TRIM(size_file), action='WRITE')
!WRITE(8798,'(4I10,A)') max_no_snowmelt_slugs, total_no_elements, total_no_links, top_cell_no, &
!              '     max_no_snowmelt_slugs, total_no_elements, total_no_links, top_cell_no'
!WRITE(8798,'(4I10,A)') szmonte, pcmonte, ran2monte1, ran2monte2, '     szmonte, pcmonte, ran2monte1, ran2monte2'
!DO iii=1,szmonte
!    WRITE(8798,'(<SIZE(montec,DIM=2)>I1)') montec(iii,:)
!ENDDO
!CLOSE(8789)
!CALL INITIALISE_VSMOD()
!CALL INITIALISE_AL_C()
! set up cell numbers for wells and springs
!     set defaults
      DO 700 IEL = 1, total_no_elements
         NWELBT (IEL) = 1
         NWELTP (IEL) = 1
         NVSSPC (IEL) = 0

700   END DO
      DO 890 IEL = total_no_links + 1, total_no_elements
         ICBOT = NLYRBT (IEL, 1)

         ZGI = ZGRUND (IEL)
         IW = NVSWLI (IEL)

         IF (IW.GT.0) THEN
            RDUM = ZGI - VSZWLB (IW)
            DO 760 ICL = ICBOT, top_cell_no
760         IF (RDUM.LE.ZVSNOD (ICL, IEL) ) GOTO 770

770         NWELBT (IEL) = ICL
            RDUM = ZGI - VSZWLT (IW)
            DO 780 ICL = top_cell_no, ICBOT, - 1
780         IF (RDUM.GE.ZVSNOD (ICL, IEL) ) GOTO 790

790         NWELTP (IEL) = ICL

         ENDIF
         RDUM = VSSPD (IEL)
         IF (GTZERO(RDUM)) THEN
            RDUM = ZGI - RDUM
            DO 820 ICL = ICBOT, top_cell_no
               DZ = ABS (ZVSNOD (ICL, IEL) - RDUM)
               IF (DZ.LE.half * DELTAZ (ICL, IEL) ) GOTO 860
820         END DO
860         NVSSPC (IEL) = ICL

         ENDIF


890   END DO
! call VSSOIL to set up soil property tables



      CALL VSSOIL
! set up initial conditions (read from file unit VSI, if required)
! type 1 - uniform phreatic surface depth, equilibrium psi profile

      IF (INITYP.EQ.1) THEN
         DO 900 IEL = 1, total_no_elements
            ZVSPSL (IEL) = MAX (ZLYRBT (IEL, 1), ZGRUND (IEL) - VSIPSD)


900      END DO
! type 2 - varying phreatic surface level, equilibrium psi profile

      ELSEIF (INITYP.EQ.2) THEN
         READ (VSI, '(A)')


         READ (VSI, * ) (ZVSPSL (IEL), IEL = ISTART, total_no_elements)
! type 3 - 3-dimensional field of psi values (+ init. psl for output)

      ELSE
         READ (VSI, '(A)')

         DO 950 IEL = ISTART, total_no_elements
            READ (VSI, * ) IELIN
            IF (IELIN.NE.IEL) GOTO 8041
            ICBOT = NLYRBT (IEL, 1)
            ICTOP = top_cell_no

            READ (VSI, * ) (VSPSI (ICL, IEL), ICL = ICBOT, ICTOP)
            ZMIN = ZVSNOD (ICBOT, IEL) - half * DELTAZ (ICBOT, IEL)
            DO 920 ICL = ICBOT, ICTOP
920         IF (LTZERO(VSPSI(ICL,IEL))) GOTO 940
940         ICL = MAX (ICBOT, ICL - 1)

            ZVSPSL (IEL) = MAX (ZMIN, ZVSNOD (ICL, IEL) + VSPSI (ICL, &
               IEL) )

950      END DO


      ENDIF
! set up equilibrium psi profile for types 1 or 2
      IF (INITYP.EQ.1.OR.INITYP.EQ.2) THEN
         DO 1200 IEL = 1, total_no_elements
            DO 1140 ICL = NLYRBT (IEL, 1), top_cell_no
               VSPSI (ICL, IEL) = ZVSPSL (IEL) - ZVSNOD (ICL, IEL)
1140        END DO
1200     END DO


      ENDIF
! set up initial relative conductivities for all elements

      DO 1400 IEL = ISTART, total_no_elements
         DO 1270 ILYR = 1, NLYR (IEL)
            DO 1250 ICL = NLYRBT (IEL, ILYR), NLYRBT (IEL, ILYR + 1) &
               - 1
               ISDUM (ICL) = NTSOIL (IEL, ILYR)
               IVSSTO (ICL, IEL) = 0
1250        END DO

1270     END DO
         ICBOT = NLYRBT (IEL, 1)
         ICTOP = top_cell_no

         CALL VSFUNC ( NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
            VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ISDUM (ICBOT), &
            VSPSI (ICBOT, IEL), IVSSTO (ICBOT, IEL), CDUM1, CDUM2, VSKR ( &
            ICBOT, IEL), CDUM3, CDUM4)

1400  END DO
      WRITE(PPPRI, 9010) 'End', '   '


      GOTO 8900
! Error handling
8041  NVSERR = NVSERR + 1
      WRITE (MSG, 9040) IEL

      CALL ERROR (EEERR, 1041, PPPRI, 0, 0, MSG)
8900  IF (NVSERR.LT.1) RETURN
      WRITE (MSG, 9030) NVSERR

      CALL ERROR(FFFATAL, 1040, PPPRI, 0, 0, MSG)

9010  FORMAT( / '!!',78('#') / 1X,A,' of VSS data ',A,60('#') / 80('#'))

9030  FORMAT(I4,' Errors have occurred in VSS data reading ', &
      &          'or initialisation.')

9040  FORMAT('Error reading VSS initial conditions for element ', &
      &       I4, '.')
   END SUBROUTINE VSIN



!> Adds inter-column exchange coefficients to the column system.
!>
!> `VSINTC` assembles the base tridiagonal system for one VSS column before
!> [[vscolm]] adds upper, lower, well, spring, lateral-boundary, and
!> stream-aquifer terms. It combines storage, vertical inter-cell flow, internal
!> lateral exchange to already known neighbour heads, and existing source/sink
!> terms `CQ`.
!>
!> Required entry conditions are `1 <= ICBOT <= ICTOP <= LLEE` and `DT > 0`.
!> For any face with a regular neighbour (`JELDUM(j)>0`) that is not handled as
!> stream-aquifer interaction (`JCBC(j) /= 9`), each non-zero `JCACN(j,i)` must
!> point to a valid neighbour cell `k`, and `k1 = k + JCDEL1(k,j)` must also be
!> valid. The neighbour heads and conductances supplied by [[vssim]] and
!> [[vscoef]] are assumed to be consistent with those indices.
!>
!> The scheme is currently fully implicit (`SIGMA = 1`). Effective hydraulic
!> head is formed as
!>
!> \[
!>   H_i = \sigma\psi_i + (1-\sigma)\psi_i^n + z_i,\qquad \sigma=1.
!> \]
!>
!> For each cell, the storage volume factor and linearised storage terms are
!>
!> \[
!>   V_i/\Delta t = {CDELZ_i\,CA0\over DT},\qquad
!>   G_i = CETA_i\,V_i/\Delta t,\qquad
!>   G'_i = CDETA_i\,V_i/\Delta t .
!> \]
!>
!> Using the vertical conductances `CBETM(i)` and `CBETM(i+1)` from [[vscoef]],
!> the routine fills lower diagonal `CA`, upper diagonal `CC`, diagonal `CB`,
!> and right-hand side `CR`. In compact form, the residual being linearised is
!>
!> \[
!>   R_i =
!>   H_{i-1}\beta_i - H_i CF_i + H_{i+1}\beta_{i+1}
!>   -(\psi_i-\psi_i^n)G_i + CQ_i ,
!> \]
!>
!> with derivative terms from `CDBETM`, `CDBTMM`, and `CDF` included in the
!> assembled matrix.
!>
!> Lateral neighbour contributions are then added for regular faces:
!>
!> \[
!>   CR_i \leftarrow CR_i - H_k\gamma_1 - H_{k1}\gamma_2,\qquad
!>   CB_i \leftarrow CB_i + H_k\gamma'_1 + H_{k1}\gamma'_2,
!> \]
!>
!> where `CGAM1/2` and `CDGAM1/2` are lateral conductances and derivatives from
!> [[vscoef]]. Faces with `JCBC=9` are skipped here because [[vssai]] adds those
!> stream-aquifer terms separately. Faces with `JCBC=10` are not skipped by this
!> routine; any non-zero `JCACN` entries still contribute regular lateral terms,
!> and [[vssai]] then adds the stream-aquifer contribution.
!>
!> @note
!> `CQ` is already premultiplied by the cell volume factor in [[vssim]], matching
!> the legacy `970514` change note. This routine treats it as an assembled
!> residual/source term, not as a flux density to be scaled again.
!> @endnote
   SUBROUTINE VSINTC (LLEE, ICBOT, ICTOP, JELDUM, JCBC, JCACN, &
      JCDEL1, CA0, CDELZ, CZ, CZ1, DT, CETA, CDETA, CQ, CPSI, CPSIN, CF, &
      CDF, CBETM, CDBETM, CDBTMM, CPSI1, CPSIN1, CGAM1, CGAM2, CDGAM1, &
      CDGAM2, CA, CB, CC, CR, H)
      INTEGER, INTENT(IN) :: LLEE                  !! Declared cell dimension for neighbour arrays.
      INTEGER, INTENT(IN) :: ICBOT                 !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                 !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: JELDUM(4)             !! Adjacent element id by face; values below 1 disable regular lateral coupling.
      INTEGER, INTENT(IN) :: JCBC(4)               !! Boundary type by face; type 9 is skipped here.
      INTEGER, INTENT(IN) :: JCACN(4,ICBOT:ICTOP)  !! Adjacent-cell index by face and active cell.
      INTEGER, INTENT(IN) :: JCDEL1(LLEE,4)        !! Neighbour-column split offset used for second connected cells.
      DOUBLEPRECISION, INTENT(IN) :: CA0           !! Plan area of the current element.
      DOUBLEPRECISION, INTENT(IN) :: CZ1(LLEE,4)   !! Adjacent-cell node elevations by cell and face.
      DOUBLEPRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Active-cell thicknesses.
      DOUBLEPRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP) !! Active-cell node elevations.
      DOUBLEPRECISION, INTENT(IN) :: CETA(ICBOT:ICTOP) !! Storage coefficient by active cell.
      DOUBLEPRECISION, INTENT(IN) :: DT            !! Timestep length.
      DOUBLEPRECISION, INTENT(IN) :: CDETA(ICBOT:ICTOP) !! Derivative of storage coefficient by active cell.
      DOUBLEPRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Current pressure heads.
      DOUBLEPRECISION, INTENT(IN) :: CPSIN(ICBOT:ICTOP) !! Previous-timestep pressure heads.
      DOUBLEPRECISION, INTENT(IN) :: CF(ICBOT:ICTOP) !! Internal conductance contribution to the diagonal.
      DOUBLEPRECISION, INTENT(IN) :: CDF(ICBOT:ICTOP) !! Derivative of `CF` with respect to pressure head.
      DOUBLEPRECISION, INTENT(IN) :: CQ(ICBOT:ICTOP) !! Assembled cell source/sink terms.
      DOUBLEPRECISION, INTENT(IN) :: CBETM(ICBOT:ICTOP+1) !! Vertical inter-cell conductance below each active cell.
      DOUBLEPRECISION, INTENT(IN) :: CDBETM(ICBOT:ICTOP+1) !! Derivative of `CBETM` with respect to the lower cell.
      DOUBLEPRECISION, INTENT(IN) :: CDBTMM(ICBOT:ICTOP+1) !! Derivative of `CBETM` with respect to the upper cell.
      DOUBLEPRECISION, INTENT(IN) :: CPSI1(LLEE,4) !! Adjacent current pressure heads by cell and face.
      DOUBLEPRECISION, INTENT(IN) :: CPSIN1(LLEE,4) !! Adjacent previous-timestep pressure heads by cell and face.
      DOUBLEPRECISION, INTENT(IN) :: CGAM1(LLEE,4) !! Primary lateral coupling conductance.
      DOUBLEPRECISION, INTENT(IN) :: CGAM2(LLEE,4) !! Secondary split-cell lateral coupling conductance.
      DOUBLEPRECISION, INTENT(IN) :: CDGAM1(LLEE,4) !! Derivative of `CGAM1` with respect to local pressure head.
      DOUBLEPRECISION, INTENT(IN) :: CDGAM2(LLEE,4) !! Derivative of `CGAM2` with respect to local pressure head.
      DOUBLEPRECISION, INTENT(OUT) :: CA(ICBOT:ICTOP) !! Lower diagonal for the tridiagonal column system.
      DOUBLEPRECISION, INTENT(OUT) :: CB(ICBOT:ICTOP) !! Diagonal for the tridiagonal column system.
      DOUBLEPRECISION, INTENT(OUT) :: CC(ICBOT:ICTOP) !! Upper diagonal for the tridiagonal column system.
      DOUBLEPRECISION, INTENT(OUT) :: CR(ICBOT:ICTOP) !! Right-hand side for the tridiagonal column system.
      DOUBLEPRECISION, INTENT(OUT) :: H(ICBOT-1:ICTOP+1) !! Workspace for effective hydraulic heads.
! Locals, etc
      DOUBLEPRECISION SIGMA, OMSIG
      PARAMETER (SIGMA = 1D0, OMSIG = 1D0 - SIGMA)
      INTEGER :: I, J, K, K1, P
      DOUBLEPRECISION CBETMI, CBETPI, CDBETP, CDBMMI, CDBTPP, CDFM, &
         CDFP, CDG



      DOUBLEPRECISION CFI, CGI, DPSI, HI, HK, HK1, HM, HP, VODT
!----------------------------------------------------------------------*
! Prepare effective hydraulic heads
      I = ICBOT - 1
      H (I) = zero
      DO 100 I = ICBOT, ICTOP
         H (I) = SIGMA * CPSI (I) + OMSIG * CPSIN (I) + CZ (I)
100   END DO


      H (I) = zero
! Set coefficients, omitting lateral terms
      DO 200 I = ICBOT, ICTOP

         P = I + 1
         HM = H (I - 1)
         HI = H (I)
         HP = H (P)
         CFI = CF (I)
         CBETMI = CBETM (I)
         CBETPI = CBETM (P)
         CDBTPP = CDBETM (P)
         CDBMMI = CDBTMM (I)
         CDBETP = CDBTMM (P)
         CDFM = CDBMMI

         CDFP = CDBTPP
         VODT = CDELZ (I) * CA0 / DT
         CGI = CETA (I) * VODT
         CDG = CDETA (I) * VODT

         DPSI = CPSI (I) - CPSIN (I)
         CA (I) = SIGMA * CBETMI - HI * CDFM + HM * CDBMMI
         CC (I) = SIGMA * CBETPI - HI * CDFP + HP * CDBTPP
         CB (I) = HM * CDBETM (I) - HI * CDF (I) + HP * CDBETP - &
            (SIGMA * CFI + DPSI * CDG + CGI)

         CR (I) = - (HM * CBETMI - HI * CFI + HP * CBETPI - DPSI * CGI + &
            CQ (I) )


200   END DO
! Add lateral terms

      DO 400 J = 1, 4

         IF (JELDUM (J) .LT.1.OR.JCBC (J) .EQ.9) GOTO 400

         DO 300 I = ICBOT, ICTOP
            K = JCACN (J, I)
            IF (K.EQ.0) GOTO 300

            K1 = JCDEL1 (K, J) + K
            HK = SIGMA * CPSI1 (K, J) + OMSIG * CPSIN1 (K, J) + CZ1 (K, &
               J)

            HK1 = SIGMA * CPSI1 (K1, J) + OMSIG * CPSIN1 (K1, J) &
               + CZ1 (K1, J)
            CB (I) = CB (I) + HK * CDGAM1 (I, J) + HK1 * CDGAM2 (I, J)

            CR (I) = CR (I) - HK * CGAM1 (I, J) - HK1 * CGAM2 (I, J)

300      END DO

400   END DO
   END SUBROUTINE VSINTC



!> Adds lower boundary-condition terms to the bottom VSS cell.
!>
!> `VSLOWR` applies the manual bottom boundary categories (`VS17`/`VS18`) to the
!> bottom cell of the column matrix assembled by [[vscolm]]. The required entry
!> condition is `CDELZ > 0`.
!>
!> Implemented behaviour is:
!>
!> | `JCBC` | Manual boundary type | Code behaviour |
!> |:-------|:---------------------|:---------------|
!> | 6 | prescribed column-base flow | Uses `CBF` directly. |
!> | 7 | prescribed column-base head | Applies a conductance term to head `CBH`. |
!> | 8 | free drainage | Currently falls through to zero lower-boundary flux. |
!> | other | no-flow/default | Zero lower-boundary flux. |
!>
!> For prescribed flow,
!>
!> \[
!>   q_b = CBF,\qquad {dq_b\over d\psi}=0.
!> \]
!>
!> For prescribed head, with bottom cell centre elevation `CZ`, pressure head
!> `CPSI`, saturated vertical conductivity `CKZS`, relative conductivity `CKR`,
!> and derivative `CDKR`,
!>
!> \[
!>   \Delta h = CBH - CZ - CPSI,\qquad
!>   K_{\Delta z} = {CKZS\over 0.5\,CDELZ},
!> \]
!>
!> \[
!>   q_b = K_{\Delta z}CKR\,\Delta h,\qquad
!>   {dq_b\over d\psi} = K_{\Delta z}(CDKR\,\Delta h - CKR).
!> \]
!>
!> The diagnostic/output lower flux is `CQV = q_b`. The linearised contribution
!> is inserted into the bottom-cell equation as
!>
!> \[
!>   CB \leftarrow CB + CA0\,{dq_b\over d\psi},\qquad
!>   CR \leftarrow CR - CA0\,q_b .
!> \]
   SUBROUTINE VSLOWR (JCBC, CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, &
      CKR, CDKR, CB, CR, CQV)
      INTEGER, INTENT(IN) :: JCBC           !! Bottom boundary type: 6 flow, 7 head, otherwise no-flow/free-drainage fallback.
      DOUBLEPRECISION, INTENT(IN) :: CA0    !! Plan area of the current element.
      DOUBLEPRECISION, INTENT(IN) :: CZ     !! Bottom-cell node elevation.
      DOUBLEPRECISION, INTENT(IN) :: CDELZ  !! Bottom-cell thickness.
      DOUBLEPRECISION, INTENT(IN) :: CKZS   !! Saturated vertical hydraulic conductivity for the bottom-cell soil.
      DOUBLEPRECISION, INTENT(IN) :: CBF    !! Prescribed bottom-flow boundary value.
      DOUBLEPRECISION, INTENT(IN) :: CBH    !! Prescribed bottom-head boundary value.
      DOUBLEPRECISION, INTENT(IN) :: CPSI   !! Bottom-cell pressure head.
      DOUBLEPRECISION, INTENT(IN) :: CKR    !! Bottom-cell relative hydraulic conductivity.
      DOUBLEPRECISION, INTENT(IN) :: CDKR   !! Derivative of `CKR` with respect to pressure head.
      DOUBLEPRECISION, INTENT(INOUT) :: CB  !! Bottom-cell matrix diagonal term.
      DOUBLEPRECISION, INTENT(INOUT) :: CR  !! Bottom-cell right-hand side term.
      DOUBLEPRECISION, INTENT(OUT) :: CQV   !! Bottom vertical boundary flux.
! Locals, etc
      DOUBLEPRECISION CDQDUM, CQVDUM, DH, KSODZ
!
!----------------------------------------------------------------------*
!
! column base flow (type 6)
      IF (JCBC.EQ.6) THEN
         CQVDUM = CBF

         CDQDUM = zero
! column base head (type 7)
      ELSEIF (JCBC.EQ.7) THEN
         DH = CBH - CZ - CPSI
         KSODZ = CKZS / (half * CDELZ)
         CQVDUM = KSODZ * CKR * DH

         CDQDUM = KSODZ * (CDKR * DH - CKR)
! no flow (970131: Check column base free drainage (type 8)!)
      ELSE
         CQVDUM = zero

         CDQDUM = zero

      ENDIF
      CQV = CQVDUM
      CB = CB + CA0 * CDQDUM

      CR = CR - CA0 * CQVDUM
   END SUBROUTINE VSLOWR



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
!> split-cell lateral connection, the restructured loop sets `iscycle`, skips
!> remaining work guarded by that flag, and then stops after the outer loop with
!> `UNFINISHED CODE FOR SPLIT CELLS IN SUBROUTINE VSMB`.
!> @endwarning
   SUBROUTINE VSMB (VSTHEN)
      DOUBLEPRECISION, INTENT(IN) :: VSTHEN(LLEE,total_no_elements) !! Previous-timestep water content by cell and element.
! Locals, etc
      INTEGER :: NFACES, IFACES (4)
      INTEGER :: IEL, J, ITYPE, IFA, JEL, ICL, JFA, JCL, IW, MCL
      DOUBLEPRECISION AREAE, CMBE, F, Qasum
      LOGICAL :: iscycle
!----------------------------------------------------------------------*
! --- loop over all elements
      iscycle=.FALSE.
      DO 2900 IEL = 1, total_no_elements
         IF(iscycle) CYCLE
         ITYPE = ICMREF (IEL, 1)
         ! Choose faces to adjust (ie set NFACES and IFACES)
         ! grids - do nothing!
         IF (ITYPE.EQ.0) THEN
            NFACES = 0
            ! banks - update only 'outer' face adjacent to grid (if there is one)
         ELSEIF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
            NFACES = 0
            DO 920 IFA = 1, 4
               IF(iscycle) CYCLE
               JEL = ICMREF (IEL, IFA + 4)
               IF (JEL.GT.0) THEN
                  IF (ICMREF (JEL, 1) .EQ.0) THEN
                     IFACES (1) = IFA
                     NFACES = 1
                     iscycle = .TRUE. !GOTO 930  !                       >>>>>>>>
                  ENDIF
               ENDIF
920         ENDDO
            iscycle=.FALSE.! 930 CONTINUE
            ! links - update faces adjacent to banks only
         ELSE
            NFACES = 2
            IF (LINKNS (IEL) ) THEN
               IFACES (1) = 1
               IFACES (2) = 3
            ELSE
               IFACES (1) = 2
               IFACES (2) = 4
            ENDIF
         ENDIF
         ! Loop over column cells if required (top to bottom for QVSV's benefit)
         IF (NFACES.GT.0) THEN
            IW = NVSWLI (IEL)
            AREAE = cellarea (IEL)
            DO 990 ICL = top_cell_no, NLYRBT (IEL, 1), - 1
               ! calculate mass balance error (m**3/s)
               MCL = ICL - 1
               CMBE = - QVSV (MCL, IEL) + QVSV (ICL, IEL) + ERUZ (IEL, &
                  ICL) + DELTAZ (ICL, IEL) * (VSTHE (ICL, IEL) - VSTHEN ( &
                  ICL, IEL) ) / DTUZ
               IF (IW.GT.0) CMBE = CMBE+QVSWLI (ICL, IW)
               IF (ICL.EQ.top_cell_no) CMBE = CMBE+ESOILA (IEL)
               CMBE = CMBE * AREAE
               DO 950 IFA = 1, 4
                  CMBE = CMBE-QVSH (IFA, ICL, IEL)
950            ENDDO
               ! adjust lateral flows (unless Qasum=0)
               Qasum = zero
               DO 955 J = 1, NFACES
                  IFA = IFACES (J)
                  Qasum = Qasum + QVSH (IFA, ICL, IEL)
955            ENDDO
               IF (NOTZERO(Qasum)) THEN
                  F = one + CMBE / Qasum
                  DO 960 J = 1, NFACES
                     IFA = IFACES (J)
                     QVSH (IFA, ICL, IEL) = QVSH (IFA, ICL, IEL) * F
960               ENDDO
               ENDIF
990         ENDDO
         ENDIF
         ! Update flows for adjacent element
         DO 2800 IFA = 1, 4
            IF(iscycle) CYCLE
            JEL = ICMREF (IEL, IFA + 4)
            IF (JEL.GT.0) THEN
               JFA = ICMREF (IEL, IFA + 8)
               DO 1820 ICL = NLYRBT (IEL, 1), top_cell_no
                  IF(iscycle) CYCLE
                  !970509            (catch JEL next time around)
                  IF (JVSDEL (IFA, ICL, IEL) .NE.0) THEN
                     iscycle=.TRUE.  !GOTO 8820
                     CYCLE
                  ENDIF
                  JCL = JVSACN (IFA, ICL, IEL)
                  IF (JCL.GT.0) QVSH (JFA, JCL, JEL) = - QVSH (IFA, ICL, IEL)
1820           ENDDO
            ENDIF
2800     END DO
2900  END DO
      IF(.NOT.iscycle) RETURN
8820  STOP 'UNFINISHED CODE FOR SPLIT CELLS IN SUBROUTINE VSMB!'
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
   SUBROUTINE VSPREP ()
      INTEGER :: NDATA
      PARAMETER (NDATA = 4 + 3 * NVSEE)
      INTEGER :: I, II, III, NDUM
!DOUBLEPRECISION WLLAST, WLTIME, RWELIN (NVSEE)
!DOUBLEPRECISION RLFLST, RLFTIM, RLFPRV (NVSEE)
!DOUBLEPRECISION RLHLST, RLHTIM, RLHPRV (NVSEE), RLHNXT (NVSEE)
!DOUBLEPRECISION RLGLST, RLGTIM, RLGPRV (NVSEE), RLGNXT (NVSEE)
!DOUBLEPRECISION RBFLST, RBFTIM, RBFPRV (NVSEE)
!DOUBLEPRECISION RBHLST, RBHTIM, RBHPRV (NVSEE), RBHNXT (NVSEE)

!DOUBLEPRECISION RLFDUM (NVSEE), RLHDUM (NVSEE), RLGDUM (NVSEE)
!SAVE WLLAST, WLTIME, RWELIN, RLHLST, RLHTIM, RLHPRV, RLHNXT
!SAVE RLFLST, RLFTIM, RLFPRV, RLGLST, RLGTIM, RLGPRV, RLGNXT
!SAVE RBFLST, RBFTIM, RBFPRV, RBHLST, RBHTIM, RBHPRV, RBHNXT
!DATA WLLAST, WLTIME, RWELIN, RLHLST, RLHTIM, RLHPRV, RLHNXT / &
! NDATA * 0.0D0 /
!DATA RLFLST, RLFTIM, RLFPRV, RLGLST, RLGTIM, RLGPRV, RLGNXT / &
! NDATA * 0.0D0 /
!DATA RBFLST, RBFTIM, RBFPRV, RBHLST, RBHTIM, RBHPRV, RBHNXT / &
! NDATA * 0.0D0 /
!----------------------------------------------------------------------*
! wells

      IF (NVSWL.GT.0) THEN
         CALL FINPUT (WLD, TIH, UZNOW, UZNEXT, WLLAST, WLTIME, RWELIN, &
            NVSWL, WLNOW)

         IF (EQMARKER(WLTIME)) CALL ERROR(FFFATAL, 1042, PPPRI, 0, 0, &
            'End of well abstraction file (WLD)')



      ENDIF
! lateral flow boundary condition

      IF (NVSLF.GT.0) THEN
         CALL FINPUT (LFB, TIH, UZNOW, UZNEXT, RLFLST, RLFTIM, RLFPRV, &
            NVSLFT, RLFDUM)

         IF (EQMARKER(RLFTIM)) CALL ERROR(FFFATAL, 1043, PPPRI, 0, 0, &
            'End of lateral flow boundary condition file (LFB)')
         III = 1
         DO 20 I = 1, NVSLF
            NDUM = NVSLFN (I)
            IF (NDUM.EQ.0) NDUM = 1
            DO 10 II = 1, NDUM
               RLFNOW (II, I) = RLFDUM (III)
               III = III + 1
10          END DO

20       END DO



      ENDIF
! lateral head boundary condition

      IF (NVSLH.GT.0) THEN
         CALL HINPUT (LHB, TIH, UZNOW, UZNEXT, RLHLST, RLHTIM, RLHPRV, &
            RLHNXT, NVSLHT, RLHDUM)

         IF (EQMARKER(RLHTIM)) CALL ERROR(FFFATAL, 1044, PPPRI, 0, 0, &
            'End of lateral head boundary condition file (LHB)')
         III = 1
         DO 40 I = 1, NVSLH
            NDUM = NVSLHN (I)
            IF (NDUM.EQ.0) NDUM = 1
            DO 30 II = 1, NDUM
               RLHNOW (II, I) = RLHDUM (III)
               III = III + 1
30          END DO

40       END DO



      ENDIF
! lateral head gradient boundary condition

      IF (NVSLG.GT.0) THEN
         CALL HINPUT (LGB, TIH, UZNOW, UZNEXT, RLGLST, RLGTIM, RLGPRV, &
            RLGNXT, NVSLGT, RLGDUM)

         IF (EQMARKER(RLGTIM)) CALL ERROR(FFFATAL, 1052, PPPRI, 0, 0, &
            'End of lateral head gradient boundary condition file (LGB)')
         III = 1
         DO 60 I = 1, NVSLG
            NDUM = NVSLGN (I)
            IF (NDUM.EQ.0) NDUM = 1
            DO 50 II = 1, NDUM
               RLGNOW (II, I) = RLGDUM (III)
               III = III + 1
50          END DO

60       END DO



      ENDIF
! column base flow boundary condition

      IF (NVSBF.GT.0) THEN
         CALL FINPUT (BFB, TIH, UZNOW, UZNEXT, RBFLST, RBFTIM, RBFPRV, &
            NVSBF, RBFNOW)

         IF (EQMARKER(RBFTIM)) CALL ERROR(FFFATAL, 1045, PPPRI, 0, 0, &
            'End of column base flow boundary condition file (BFB)')



      ENDIF
! column base head boundary condition

      IF (NVSBH.GT.0) THEN
         CALL HINPUT (BHB, TIH, UZNOW, UZNEXT, RBHLST, RBHTIM, RBHPRV, &
            RBHNXT, NVSBH, RBHNOW)

         IF (EQMARKER(RBHTIM)) CALL ERROR(FFFATAL, 1046, PPPRI, 0, 0, &
            'End of column base head boundary condition file (BHB)')


      ENDIF
   END SUBROUTINE VSPREP



!> Reads static VSS data from the subsurface input file.
!>
!> `VSREAD` reads the manual `VSD` groups `VS01`-`VS18` and populates the module
!> state used by [[vsconl]], [[vsconc]], [[vssoil]], [[vsin]], and the timestep
!> solver. The routine uses `ALREAD` for labelled blocks and increments
!> `NVSERR` or raises fatal errors when required layer/table data are
!> inconsistent.
!>
!> Main input groups and destinations:
!>
!> | Group | Data read | Main arrays/variables filled |
!> |:------|:----------|:-----------------------------|
!> | `VS01` | VSD title | Printed to `PPPRI`. |
!> | `VS02` | logical flags | `BFAST`, `BSOILP`, `BHELEV`. |
!> | `VS03` | counts and initialisation type | `NS`, `NCSZON`, `NCRBED`, `INITYP`. |
!> | `VS04` | initial phreatic depth and mesh/averaging controls | `VSIPSD`, `VSZMIN`, `VSZMAX`, `VSWV`, `VSWL`. |
!> | `VS05`, `VS05a` | soil/lithology hydraulic parameters and optional tables | `IVSFLG`, `IVSNTB`, `VSK3D`, `VSPOR`, `VSTRES`, `VSPSS`, `VSVGN`, `VSALPH`, `TBPSI`, `TBTHE`, `TBKR`, spline coefficients. |
!> | `VS06`, `VS07` | soil-zone and river-bed cell depths | `DCSZON`, `DCSTOT`, `DCRBED`, `DCRTOT` and helper node-depth arrays. |
!> | `VS08`-`VS08d` | aquifer-zone layer categories, grids, and individual elements | `NLYR`, `NTSOIL`, `ZLYRBT` for grids, banks, and links. |
!> | `VS09`, `VS09a` | river-bed soil type and depth | `ISRBED`, `DRBED`, link bed layers. |
!> | `VS10`, `VS10a` | user-defined aquifer connectivity | `NAQCON`, `IAQCON` for [[vsconl]]. |
!> | `VS11` | boundary category counts | `NVSWL`, `NVSSP`, `NVSLF`, `NVSLH`, `NVSLG`, `NVSBF`, `NVSBH`, `NVSBD`. |
!> | `VS12`-`VS13b` | wells and springs | `NVSWLI`, `NVSWLC`, `NVSWLT`, `VSZWLB`, `VSZWLT`, `NVSSPT`, `VSSPD`, `VSSPZ`, `VSSPCO`. |
!> | `VS14`-`VS16b` | lateral boundary type/category grids and selected-layer lists | `NLBTYP`, `NLBCAT`, `NVSLFN/HN/GN`, `NVSLFL/HL/GL`, `NVSLFT/HT/GT`. |
!> | `VS17`, `VS18` | bottom boundary type/category grids | `NBBTYP`, `NBBCAT`. |
!>
!> Conductivities from `VS05` are converted from m/day to m/s for the solver.
!> `VSZMAX` from `VS04` is stored as the input value plus `1.0e-6`, matching
!> the legacy tolerance used when deciding aquifer-zone cell subdivisions.
!> For tabulated soil options (`IVSFLG = 2` or `4`), the routine reads
!> \(\psi\), \(\theta\), and \(K_r\) tables and builds natural cubic-spline
!> second-derivative coefficients in log10(-\(\psi\)) space. For `IVSFLG = 4`,
!> the manual says entered `K_r` values are not used, but the table still has to
!> be present for input compatibility.
!>
!> Layer category data are expanded to element arrays. Category grids may cover
!> links and grid elements; individual `VS08d` records fill elements whose
!> category is zero. Soil-zone and river-bed layer boundaries are snapped to the
!> computational cell-depth sequences so later cell construction in [[vsconc]]
!> is consistent with `DCSZON` and `DCRBED`.
!>
!> `VS13` itself is treated as a dummy record in this implementation: the number
!> of spring records read from `VS13a`/`VS13b` is `NVSSP` from `VS11`. For `VS16`
!> selected-layer boundary categories, a category with `NLDUM` selected layers
!> contributes `NLDUM` time-series values, while an unlisted category contributes
!> one full-column value; this is why `NVSLFT`, `NVSLHT`, and `NVSLGT` start at
!> their category counts and add `NLDUM - 1` for each selected-layer record.
!>
!> On exit, for each element `e = 1:NEL`, the boundary type arrays are
!> non-negative and the boundary category arrays have valid defaults:
!>
!> | Array | Exit condition |
!> |:------|:---------------|
!> | `NLBTYP(e)` | `0 <= NLBTYP(e)` |
!> | `NBBTYP(e)` | `0 <= NBBTYP(e)` |
!> | `NLBCAT(e)` | `1 <= NLBCAT(e)` |
!> | `NBBCAT(e)` | `1 <= NBBCAT(e)` |
!> | `NVSWLC(e)` | `1 <= NVSWLC(e)` |
!>
!> @note
!> The local `BDONE` array is `DATA`-initialised and retained between calls.
!> `VSREAD` follows the original one-call setup assumption, like [[vsconl]] and
!> [[vsconc]].
!> @endnote
   SUBROUTINE VSREAD (NAQCON, IAQCON)
      INTEGER, INTENT(OUT) :: NAQCON       !! Number of user-defined aquifer connectivity records read from `VS10`.
      INTEGER, INTENT(OUT) :: IAQCON(4,NVSEE) !! User-defined aquifer connectivity records read from `VS10a`.
! Locals, etc
      INTEGER :: I, I0, IBK, ICAT, IEL, ILYR, IS, ISP, IW, IWT, IX, &
         IXY0, IY
      INTEGER :: ICOUNT, LCOUNT
      INTEGER :: IVSDUM (NELEE, NLYREE), IVSCAT (NELEE), ISDUM (NSEE, 8)
      INTEGER :: NUM_CATEGORIES_TYPES,  NELEM, NCOUNT, NDUM, NSP, NW
      INTEGER :: ILB, NLB, ITYP, NLDUM, ISDUM1, IDUM1(1)
      DOUBLEPRECISION RVSDUM (NELEE, NLYREE), RSDUM (NSEE, 8), DCSDUM ( &
         0:LLEE), DCSNOD (LLEE), DCRDUM (0:LLEE), DCRNOD (LLEE), SIG, PDUM
      DOUBLEPRECISION XDUM (NVSEE), YDUM (NVSEE), Y2DUM (NVSEE), &
         UDUM (NVSEE)
      CHARACTER (LEN=80) :: CDUM, MSG * 132
      LOGICAL :: BDONE (NELEE)

      DATA BDONE / NELEE * .FALSE. /
!
!----------------------------------------------------------------------*
!
! Initialization
      DO 6 IEL = 1, total_no_elements
         NVSWLI (IEL) = 0
         NLBTYP (IEL) = 0
         NBBTYP (IEL) = 0
         NVSWLC (IEL) = 1
         NLBCAT (IEL) = 1
         NBBCAT (IEL) = 1


6     END DO
! VS01 ----- main data file title
      CALL ALREAD (1, VSD, PPPRI, ':VS01', 1, 1, 0, CDUM, IDUM, DUMMY)


      WRITE(PPPRI, '(/ X,A /)') CDUM
! VS02 ----- logical flags
      READ (VSD, '(A)') CDUM


      READ (VSD, * ) BFAST, BSOILP, BHELEV
! VS03 ----- integer variables
      CALL ALREAD (2, VSD, PPPRI, ':VS03', 4, 1, 0, CDUM, IDUM, DUMMY)
      NS = IDUM (1)
      NCSZON = IDUM (2)
      NCRBED = IDUM (3)


      INITYP = IDUM (4)
! VS04 ----- real variables
      CALL ALREAD (3, VSD, PPPRI, ':VS04', 5, 1, 0, CDUM, IDUM, DUMMY)
      VSIPSD = DUMMY (1)
      VSZMIN = DUMMY (2)
      VSZMAX = DUMMY (3) + 1.0D-6
      VSWV = DUMMY (4)


      VSWL = DUMMY (5)
! VS05 ----- physical property data
      CALL ALREAD (7, VSD, PPPRI, ':VS05', NSEE, 8, NS, CDUM, ISDUM, &
         RSDUM)
      DO 10 IS = 1, NS
         IVSFLG (IS) = ISDUM (IS, 2)
         IVSNTB (IS) = ISDUM (IS, 3)
         VSK3D (IS, 1) = RSDUM (IS, 1) / (3600.0D0 * 24.0D0)
         VSK3D (IS, 2) = RSDUM (IS, 2) / (3600.0D0 * 24.0D0)
         VSK3D (IS, 3) = RSDUM (IS, 3) / (3600.0D0 * 24.0D0)
         VSPOR (IS) = RSDUM (IS, 4)
         VSTRES (IS) = RSDUM (IS, 5)
         VSPSS (IS) = RSDUM (IS, 6)
         VSVGN (IS) = RSDUM (IS, 7)

         VSALPH (IS) = RSDUM (IS, 8)

         VSPPOR (IS) = VSPOR (IS)


10    END DO
! VS05a ---- soil characteristic function tabulated data
      DO 15 IS = 1, NS

         IF (IVSFLG (IS) .EQ.2.OR.IVSFLG (IS) .EQ.4) THEN
            READ (VSD, * ) ISDUM1
            IF (IS.NE.ISDUM1) THEN
               WRITE (MSG, 9030) IS
               CALL ERROR(FFFATAL, 1051, PPPRI, 0, 0, MSG)

            ENDIF
            DO 14 I = 1, IVSNTB (IS)
               READ (VSD, * ) TBPSI (I, IS), TBTHE (I, IS), TBKR (I, IS)


14          END DO
! set up cubic spline coefficients for theta, using log(psi)
! based on routines 'spline' and 'splint' in NUMERICAL RECIPES
! FOR FORTRAN (..UNFINISHED), pp 109 and 110
! NB asasumes 'natural' boundary conditions (ie zero 2nd derivatives)
            DO 16 I = 1, IVSNTB (IS)
               XDUM (I) = DLOG10 ( - TBPSI (I, IS) )
               YDUM (I) = TBTHE (I, IS)
16          END DO

            NDUM = IVSNTB (IS)
            Y2DUM (1) = zero
            UDUM (1) = zero
            Y2DUM (NDUM) = zero
            DO 17 I = 2, NDUM - 1
               SIG = (XDUM (I) - XDUM (I - 1) ) / (XDUM (I + 1) - XDUM ( &
                  I - 1) )
               PDUM = SIG * Y2DUM (I - 1) + two
               Y2DUM (I) = (SIG - one) / PDUM
               UDUM (I) = (6.0D0 * ( (YDUM (I + 1) - YDUM (I) ) / &
                  (XDUM (I + 1) - XDUM (I) ) - (YDUM (I) - YDUM (I - 1) ) &
                  / (XDUM (I) - XDUM (I - 1) ) ) / (XDUM (I + 1) - XDUM (I &
                  - 1) ) - SIG * UDUM (I - 1) ) / PDUM
17          END DO
            DO 18 I = NDUM - 1, 1, - 1
               Y2DUM (I) = Y2DUM (I) * Y2DUM (I + 1) + UDUM (I)
18          END DO
            DO 19 I = 1, NDUM
               TBTHEC (I, IS) = Y2DUM (I)


19          END DO
! if required, set up cubic spline coefficients for Kr similarly

            IF (IVSFLG (IS) .EQ.2) THEN
               DO 21 I = 1, IVSNTB (IS)
                  YDUM (I) = TBKR (I, IS)

21             END DO
               Y2DUM (1) = zero
               UDUM (1) = zero
               Y2DUM (NDUM) = zero
               DO 22 I = 2, NDUM - 1
                  SIG = (XDUM (I) - XDUM (I - 1) ) / (XDUM (I + 1) &
                     - XDUM (I - 1) )
                  PDUM = SIG * Y2DUM (I - 1) + two
                  Y2DUM (I) = (SIG - one) / PDUM
                  UDUM (I) = (6.0D0 * ( (YDUM (I + 1) - YDUM (I) ) &
                     / (XDUM (I + 1) - XDUM (I) ) - (YDUM (I) - YDUM (I - &
                     1) ) / (XDUM (I) - XDUM (I - 1) ) ) / (XDUM (I + 1) &
                     - XDUM (I - 1) ) - SIG * UDUM (I - 1) ) / PDUM
22             END DO
               DO 23 I = NDUM - 1, 1, - 1
                  Y2DUM (I) = Y2DUM (I) * Y2DUM (I + 1) + UDUM (I)
23             END DO
               DO 24 I = 1, NDUM
                  TBKRC (I, IS) = Y2DUM (I)

24             END DO

            ENDIF

         ENDIF



15    END DO
! VS06 ----- soil zone cell sizes (start at the ground surface)
! NB. dcsnod(ncszon+1) is set to the BOTTOM of the (fictional) cell
! immediately below the soil zone, rather than at the node, to ensure
! that no layer can exist in the aquifer zone with thickness < vszmin
! (see loop 530)
      IF (NCSZON.GT.0) CALL ALREAD (3, VSD, PPPRI, ':VS06', NCSZON, 1, 0, &
         CDUM, IDUM, DCSZON)
      WRITE(PPPRI, * ) 'DCSZON: ', (DCSZON (I) , I = 1, NCSZON)
      DCSTOT = zero
      DCSDUM (0) = zero
      DO 30 I = 1, NCSZON
         DCSTOT = DCSTOT + DCSZON (I)
         DCSDUM (I) = DCSTOT
         DCSNOD (I) = half * (DCSDUM (I) + DCSDUM (I - 1) )
30    END DO



      DCSNOD (NCSZON + 1) = DCSTOT + VSZMIN
! VS07 ----- river bed cell sizes (start at the bed surface)
! NB. dcrnod(ncrbed+1) is set to the BOTTOM of the (fictional) cell
! immediately below the river bed soil zone (see VS06 comment above)
! (see loop 730)
      IF (NCRBED.GT.0) CALL ALREAD (3, VSD, PPPRI, ':VS07', NCRBED, 1, 0, &
         CDUM, IDUM, DCRBED)
      WRITE(PPPRI, * ) 'DCRBED: ', (DCRBED (I) , I = 1, NCRBED)
      DCRTOT = zero
      DCRDUM (0) = zero
      DO 40 I = 1, NCRBED
         DCRTOT = DCRTOT + DCRBED (I)
         DCRDUM (I) = DCRTOT
         DCRNOD (I) = half * (DCRDUM (I) + DCRDUM (I - 1) )
40    END DO



      DCRNOD (NCRBED+1) = DCRTOT + VSZMIN
! VS08 ----- soil/lithology layer definition data
! --- read no. of categories and elements
      CALL ALREAD (2, VSD, PPPRI, ':VS08', 2, 1, 0, CDUM, IDUM, DUMMY)
      NUM_CATEGORIES_TYPES = IDUM (1)


      NELEM = IDUM (2)
! --- category data


      IF (NUM_CATEGORIES_TYPES .EQ.0) THEN
! expect all elements to be input individually
! (all grids plus 1 set of data for each link if BEXBK=.true.
!  all grids if BEXBK = .false.)
         IF (BEXBK) THEN
            NCOUNT = total_no_elements - 2 * total_no_links
         ELSE
            NCOUNT = total_no_elements - total_no_links


         ENDIF


      ELSE
! initialise arrays
         DO 50 IEL = 1, NELEE
            DO 50 ILYR = 1, NLYREE
               IVSDUM (IEL, ILYR) = 0
               RVSDUM (IEL, ILYR) = zero


50       CONTINUE
! read layer data


         CALL ALREAD (6, VSD, PPPRI, ':VS08a', NELEE, NLYREE, NUM_CATEGORIES_TYPES,  CDUM, &
            IVSDUM, RVSDUM)
! for NUM_CATEGORIES_TYPES = 1, set all elements = category 1
         IF (NUM_CATEGORIES_TYPES == 1) THEN
            DO 100 IEL = 1, total_no_elements
               IVSCAT (IEL) = 1


100         END DO
! for > 1 category read in categories for links (if required) and grids
         ELSE
            IF (BEXBK.AND.total_no_links.GT.0) THEN
               CALL ALREAD (2, VSD, PPPRI, ':VS08b', total_no_links, 1, NUM_CATEGORIES_TYPES,  CDUM, &
                  IVSCAT, DUMMY)
            ENDIF
            CALL ALREAD (4, VSD, PPPRI, ':VS08c', NX, NY, NUM_CATEGORIES_TYPES,  CDUM, &
               IDUM, DUMMY)
            DO 300 IY = 1, NY
               IXY0 = (IY - 1) * NX
               DO 200 IX = 1, NX
                  IEL = ICMXY (IX, IY)
                  IF (IEL.NE.0) IVSCAT (IEL) = IDUM (IXY0 + IX)
200            END DO
300         END DO


         ENDIF
! move layer data into elements for ...
         NCOUNT = 0

         DO 400 IEL = 1, total_no_elements

            IF (ICMREF (IEL, 1) .EQ.1.OR.ICMREF (IEL, 1) .EQ.2.OR. ( &
               .NOT.BEXBK.AND.ICMREF (IEL, 1) .EQ.3) ) GOTO 400
            IF (IVSCAT (IEL) .EQ.0) THEN

               NCOUNT = NCOUNT + 1
            ELSE

               BDONE (IEL) = .TRUE.
               ICAT = IVSCAT (IEL)
               ICOUNT = 0
350            IF (IVSDUM (ICAT, ICOUNT + 1) .EQ.0) GOTO 355
               ICOUNT = ICOUNT + 1
               GOTO 350


355            CONTINUE
! ...grids
               IF (ICMREF (IEL, 1) .EQ.0) THEN
                  NLYR (IEL) = ICOUNT
                  DO 360 ILYR = 1, NLYR (IEL)
                     NTSOIL (IEL, ILYR) = IVSDUM (ICAT, ILYR)
                     ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - RVSDUM (ICAT, &
                        ILYR)


360               END DO
! ...banks
               ELSE
                  DO 380 I = 1, 2
                     IBK = ICMBK (IEL, I)
                     BDONE (IBK) = .TRUE.
                     NLYR (IBK) = ICOUNT
                     DO 370 ILYR = 1, NLYR (IBK)
                        NTSOIL (IBK, ILYR) = IVSDUM (ICAT, ILYR)
                        ZLYRBT (IBK, ILYR) = ZGRUND (IBK) - RVSDUM ( &
                           ICAT, ILYR)
370                  END DO


380               END DO
! ...links
!    (NB uses data from bank 2, which is identical to bank 1)
                  LCOUNT = 0
390               IF (RVSDUM (ICAT, LCOUNT + 1) .LT.ZGRUND (IBK) &
                     - ZBEFF (IEL) + VSZMIN) GOTO 395
                  LCOUNT = LCOUNT + 1

                  GOTO 390
395               NLYR (IEL) = LCOUNT
                  DO 397 ILYR = 1, NLYR (IEL)
                     NTSOIL (IEL, ILYR) = NTSOIL (IBK, ILYR)
                     ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)
397               END DO

               ENDIF

            ENDIF

400      END DO


      ENDIF
! check no. of category elements consistent with no. of individual eleme
      IF (NCOUNT.NE.NELEM) THEN
         WRITE (MSG, 9000) NCOUNT
         CALL ERROR(FFFATAL, 1032, PPPRI, 0, 0, MSG)


      ENDIF
! --- element data


      IF (NELEM.NE.0) THEN
! initialise variables
         DO 420 IEL = 1, NELEE
            DO 420 ILYR = 1, NLYREE
               IVSDUM (IEL, ILYR) = 0
               RVSDUM (IEL, ILYR) = zero


420      CONTINUE
! read layer data

         CALL ALREAD (6, VSD, PPPRI, ':VS08d', NELEE, NLYREE, NELEM, CDUM, &
            IVSDUM, RVSDUM)


         DO 500 IEL = 1, total_no_elements
! ignore banks, links (if no banks), and elements already processed


            IF (BDONE (IEL) .OR.ICMREF (IEL, 1) .EQ.1.OR.ICMREF (IEL, 1) &
               .EQ.2.OR. (.NOT.BEXBK.AND.ICMREF (IEL, 1) .EQ.3) ) GOTO 500
! move layer data into elements for ...

            BDONE (IEL) = .TRUE.
            ICOUNT = 0
450         IF (IVSDUM (IEL, ICOUNT + 1) .EQ.0) GOTO 455
            ICOUNT = ICOUNT + 1
            GOTO 450


455         CONTINUE
! ...grids
            IF (ICMREF (IEL, 1) .EQ.0) THEN
               NLYR (IEL) = ICOUNT
               DO 460 ILYR = 1, NLYR (IEL)
                  NTSOIL (IEL, ILYR) = IVSDUM (IEL, ILYR)
                  ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - RVSDUM (IEL, ILYR)


460            END DO
! ...banks
            ELSE
               DO 480 I = 1, 2
                  IBK = ICMBK (IEL, I)
                  BDONE (IBK) = .TRUE.
                  NLYR (IBK) = ICOUNT
                  DO 470 ILYR = 1, NLYR (IBK)
                     NTSOIL (IBK, ILYR) = IVSDUM (IEL, ILYR)
                     ZLYRBT (IBK, ILYR) = ZGRUND (IBK) - RVSDUM (IEL, &
                        ILYR)
470               END DO


480            END DO
! ...links
!    (NB uses data from bank 2, which is identical to bank 1)
               LCOUNT = 0
490            IF (RVSDUM (IEL, LCOUNT + 1) .LT.ZGRUND (IBK) - ZBEFF ( &
                  IEL) + VSZMIN) GOTO 495
               LCOUNT = LCOUNT + 1

               GOTO 490
495            NLYR (IEL) = LCOUNT
               DO 497 ILYR = 1, NLYR (IEL)
                  NTSOIL (IEL, ILYR) = NTSOIL (IBK, ILYR)
                  ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)
497            END DO

            ENDIF

500      END DO


      ENDIF
! adjust horizon boundaries in soil zone to match computational mesh
! and set up ZLYRBT for ground surface
      DO 550 IEL = NGDBGN, total_no_elements
         DO 540 ILYR = NLYR (IEL), 1, - 1
            IF (ZGRUND (IEL) - ZLYRBT (IEL, ILYR) .GT.DCSTOT + VSZMIN) &
               GOTO 545
            DO 530 I = 1, NCSZON + 1
               IF (DCSNOD (I) .GT.ZGRUND (IEL) - ZLYRBT (IEL, ILYR) ) &
                  THEN
                  ZLYRBT (IEL, ILYR) = ZGRUND (IEL) - DCSDUM (I - 1)
                  GOTO 540
               ENDIF
530         END DO
540      END DO
545      ZLYRBT (IEL, NLYR (IEL) + 1) = ZGRUND (IEL)

550   END DO
      IF (BEXBK) THEN
         DO 560 IEL = 1, total_no_links
            IBK = ICMBK (IEL, 1)
            DO 555 ILYR = 1, NLYR (IEL)
               ZLYRBT (IEL, ILYR) = ZLYRBT (IBK, ILYR)
555         END DO
560      END DO


      ENDIF
! check that all elements have been set up
      DO 650 IEL = 1, total_no_elements
         IF (.NOT.BEXBK.AND.ICMREF (IEL, 1) .NE.0) GOTO 650
         IF (.NOT.BDONE (IEL) ) THEN
            NVSERR = NVSERR + 1
            WRITE (MSG, 9020) IEL
            CALL ERROR (EEERR, 1033, PPPRI, 0, 0, MSG)
         ENDIF


650   END DO
! VS09 ----- channel bed layer


      IF (total_no_links.GT.0.AND.BEXBK) THEN
! read soil types for each link


         CALL ALREAD (2, VSD, PPPRI, ':VS09', total_no_links, 1, 1, CDUM, ISRBED, &
            DUMMY)
! read bed depths for each link


         CALL ALREAD (3, VSD, PPPRI, ':VS09a', total_no_links, 1, 1, CDUM, IDUM, &
            DRBED)
! set up channel bed layer for each link
         DO 700 IEL = 1, total_no_links
            IF (DRBED (IEL) .GT.VSZMIN) THEN
               NLYR (IEL) = NLYR (IEL) + 1
               NTSOIL (IEL, NLYR (IEL) ) = ISRBED (IEL)
               ZLYRBT (IEL, NLYR (IEL) ) = ZBEFF (IEL) - DRBED (IEL)
               IF (ZLYRBT (IEL, NLYR (IEL) ) .LT.ZLYRBT (IEL, NLYR (IEL) &
                  - 1) + VSZMIN) THEN
                  NLYR (IEL) = NLYR (IEL) - 1
                  NTSOIL (IEL, NLYR (IEL) ) = ISRBED (IEL)
               ENDIF
            ENDIF


700      END DO
! adjust horizon boundaries in river bed to match computational mesh
! and set up ZLYRBT for river bed surface
         DO 750 IEL = 1, total_no_links
            DO 740 ILYR = NLYR (IEL), 1, - 1
               IF (ZGRUND (IEL) - ZLYRBT (IEL, ILYR) .GT.DCRTOT + &
                  VSZMIN) GOTO 745
               DO 730 I = 1, NCRBED+1
                  IF (DCRNOD (I) .GT.ZGRUND (IEL) - ZLYRBT (IEL, ILYR) ) &
                     THEN
                     ZLYRBT (IEL, ILYR) = ZBEFF (IEL) - DCRDUM (I - 1)
                     GOTO 740
                  ENDIF
730            END DO
740         END DO
745         ZLYRBT (IEL, NLYR (IEL) + 1) = ZBEFF (IEL)

750      END DO


      ENDIF
! VS10 ----- aquifer zone user-defined connectivities

      CALL ALREAD (2, VSD, PPPRI, ':VS10', 1, 1, 0, CDUM, IDUM, DUMMY)
      NAQCON = IDUM (1)


      IF (NAQCON.GT.0) CALL ALREAD (2, VSD, PPPRI, ':VS10a', 4, NAQCON, 0, &
         CDUM, IAQCON, DUMMY)
! VS11 ----- no. of categories for boundary conditions
      CALL ALREAD (2, VSD, PPPRI, ':VS11', 8, 1, 0, CDUM, IDUM, DUMMY)
      NVSWL = IDUM (1)
      NVSSP = IDUM (2)
      NVSLF = IDUM (3)
      NVSLH = IDUM (4)
      NVSLG = IDUM (5)
      NVSBF = IDUM (6)
      NVSBH = IDUM (7)


      NVSBD = IDUM (8)
! wells -----------------------------------------------
! VS12 ----- no. of wells

      IF (NVSWL.GT.0) THEN
         CALL ALREAD (2, VSD, PPPRI, ':VS12', 1, 1, 0, CDUM, IDUM, DUMMY)


         NW = IDUM (1)
! VS12a ---- element, category number, and target element
         CALL ALREAD (2, VSD, PPPRI, ':VS12a', 3, NW, 0, CDUM, IDUM, &
            DUMMY)
         DO 800 IW = 1, NW
            I0 = 3 * (IW - 1)
            IEL = IDUM (I0 + 1)
            NVSWLC (IEL) = MAX (1, IDUM (I0 + 2) )
            IWT = IDUM (I0 + 3)
            IF (IWT.GT.0) NVSWLT (IWT) = IEL
            NVSWLI (IEL) = IW


800      END DO
! VS12b ---- depth below ground of bottom and top of well screen
         CALL ALREAD (3, VSD, PPPRI, ':VS12b', 2, NW, 0, CDUM, IDUM, &
            DUMMY)
         DO 810 IW = 1, NW
            VSZWLB (IW) = DUMMY (2 * (IW - 1) + 1)

            VSZWLT (IW) = DUMMY (2 * (IW - 1) + 2)

810      END DO


      ENDIF
! springs ---------------------------------------------
! VS13 ----- no. of springs


      IF (NVSSP.GT.0) THEN
!c        CALL ALREAD(2, VSD, PRI, ':VS13', 1, 1, 0,
!c     -              CDUM, IDUM, DUMMY)
!c        NSP = IDUM(1)


         NSP = NVSSP
! VS13a ---- element and target element
         CALL ALREAD (2, VSD, PPPRI, ':VS13a', 2, NSP, 0, CDUM, IDUM, &
            DUMMY)
         DO 860 ISP = 1, NSP
            IEL = IDUM (2 * (ISP - 1) + 1)
            IF (IDUM (2 * (ISP - 1) + 2) .GT.0) NVSSPT (IDUM (2 * &
               (ISP - 1) + 2) ) = IEL


860      END DO
! VS13b ---- depth of spring source below ground, elevation of
!            discharge point, spring coefficient
         CALL ALREAD (3, VSD, PPPRI, ':VS13b', 3, NSP, 0, CDUM, IDUM1, DUMMY)
         DO 865 ISP = 1, NSP
            IEL = IDUM (2 * (ISP - 1) + 1)
            VSSPD (IEL) = DUMMY (3 * (ISP - 1) + 1)
            VSSPZ (IEL) = DUMMY (3 * (ISP - 1) + 2)
            VSSPCO (IEL) = DUMMY (3 * (ISP - 1) + 3)

865      END DO


      ENDIF
! lateral boundary conditions -------------------------
! VS14 ----- grid of codes (types)
      NDUM = MAX(NVSLF, NVSLH, NVSLG)

      IF (NDUM.GT.0) THEN
         CALL ALREAD (4, VSD, PPPRI, ':VS14', NX, NY, NDUM, CDUM, IDUM, &
            DUMMY)
         DO 920 IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO 910 IX = 1, NX
               IEL = ICMXY (IX, IY)
               IF (IEL.NE.0) NLBTYP (IEL) = IDUM (IXY0 + IX)
910         END DO


920      END DO
! VS15 ----- grid of category numbers
         CALL ALREAD (4, VSD, PPPRI, ':VS15', NX, NY, NDUM, CDUM, IDUM, &
            DUMMY)
         DO 940 IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO 930 IX = 1, NX
               IEL = ICMXY (IX, IY)
               IF (IEL.NE.0) NLBCAT (IEL) = MAX (1, IDUM (IXY0 + IX) )
930         END DO



940      END DO
! VS16 ----- No. of lateral boundary categories (flow, head, and head gr
! with b.c/s set only on selected layers
! initialise arrays to default values for reading in time-series data
         DO 840 ICAT = 1, NDUM
            NVSLFN (ICAT) = 0
            NVSLHN (ICAT) = 0
            NVSLGN (ICAT) = 0

840      END DO
         NVSLFT = NVSLF
         NVSLHT = NVSLH

         NVSLGT = NVSLG
         CALL ALREAD (2, VSD, PPPRI, ':VS16', 1, 1, 0, CDUM, IDUM, DUMMY)

         NLB = IDUM (1)


         DO 880 ILB = 1, NLB
! VS16a ---- b.c. type, category, no. of layers
            CALL ALREAD (2, VSD, PPPRI, ':VS16a', 3, 1, 0, CDUM, IDUM, &
               DUMMY)
            ITYP = IDUM (1)
            ICAT = IDUM (2)


            NLDUM = IDUM (3)
! VS16b ---- layer numbers
            CALL ALREAD (2, VSD, PPPRI, ':VS16b', NLDUM, 1, 0, CDUM, IDUM, &
               DUMMY)
            IF (ITYP.EQ.3) THEN
               NVSLFN (ICAT) = NLDUM
               NVSLFT = NVSLFT + NLDUM - 1
               DO 862 I = 1, NLDUM
                  NVSLFL (I, ICAT) = IDUM (I)
862            END DO
            ENDIF
            IF (ITYP.EQ.4) THEN
               NVSLHN (ICAT) = NLDUM
               NVSLHT = NVSLHT + NLDUM - 1
               DO 864 I = 1, NLDUM
                  NVSLHL (I, ICAT) = IDUM (I)
864            END DO
            ENDIF
            IF (ITYP.EQ.5) THEN
               NVSLGN (ICAT) = NLDUM
               NVSLGT = NVSLGT + NLDUM - 1
               DO 866 I = 1, NLDUM
                  NVSLGL (I, ICAT) = IDUM (I)
866            END DO

            ENDIF

880      END DO


      ENDIF
! bottom boundary conditions --------------------------
! VS17 ----- grid of codes (types)
      NDUM = MAX(NVSBF, NVSBH, NVSBD)

      IF (NDUM.GT.0) THEN

         IF (total_no_links.GT.0.AND.BEXBK) THEN
            CALL ALREAD (2, VSD, PPPRI, ':VS17', total_no_links, 1, 1, CDUM, IDUM, &
               DUMMY)
            DO 945 IEL = 1, total_no_links
               NBBTYP (IEL) = IDUM (IEL)
               NBBTYP (total_no_links + IEL) = IDUM (IEL)
               NBBTYP (2 * total_no_links + IEL) = IDUM (IEL)

945         END DO

         ENDIF
         CALL ALREAD (4, VSD, PPPRI, ':VS17', NX, NY, NDUM, CDUM, IDUM, &
            DUMMY)
         DO 960 IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO 950 IX = 1, NX
               IEL = ICMXY (IX, IY)
               IF (IEL.NE.0) NBBTYP (IEL) = IDUM (IXY0 + IX)
950         END DO


960      END DO
! VS18 ----- grid of category numbers

         IF (total_no_links.GT.0.AND.BEXBK) THEN
            CALL ALREAD (2, VSD, PPPRI, ':VS18', total_no_links, 1, 1, CDUM, IDUM, &
               DUMMY)
            DO 965 IEL = 1, total_no_links
               ICAT = MAX (1, IDUM (IEL) )
               NBBCAT (IEL) = ICAT
               NBBCAT (total_no_links + IEL) = ICAT
               NBBCAT (2 * total_no_links + IEL) = ICAT

965         END DO

         ENDIF
         CALL ALREAD (4, VSD, PPPRI, ':VS18', NX, NY, NDUM, CDUM, IDUM, &
            DUMMY)
         DO 980 IY = 1, NY
            IXY0 = (IY - 1) * NX
            DO 970 IX = 1, NX
               IEL = ICMXY (IX, IY)
               IF (IEL.NE.0) NBBCAT (IEL) = MAX (1, IDUM (IXY0 + IX) )
970         END DO

980      END DO


      ENDIF
! FORMAT statements

9000  FORMAT('Error in number of VSS layer elements. ', &
      &       'NELEM should be ',I4)

9020  FORMAT('Error reading VSS layers for element ',I4, '.')

9030  FORMAT('Soil type ',I4,' not expected for soil property tables.')
9999  RETURN
   END SUBROUTINE VSREAD



!> Adds stream-aquifer interaction terms for one column face.
!>
!> `VSSAI` represents exchange between a channel water surface and the
!> neighbouring VSS cells on one face. It is called for the internally assigned
!> stream-aquifer boundary types used by [[vssim]]: `JCBC = 9` for links without
!> explicit bank elements and `JCBC = 10` for link-bank faces when bank elements
!> are present. These are not user time-varying boundary categories; they are
!> derived from the catchment/channel topology.
!>
!> The first participating cell is `ICBOT` for `JCBC = 9`, treating the stream
!> bed as the base of the current land element, and `ICBED + 1` for bank
!> interaction. For each exposed cell \(i\),
!> \[
!>   \Delta h_i = z_s - z_i - \psi_i,
!> \]
!> where `CZS` is the adjacent channel water-surface elevation, `CZ(i)` is the
!> VSS node elevation, and `CPSI(i)` is pressure head. The effective contact
!> area per unit flow length is
!> \[
!>   A_i/L =
!>   {f_i\,A_i \over L}, \qquad
!>   f_i =
!>   \begin{cases}
!>     \min(1,\mathrm{depadj}/\Delta z_i), & \Delta h_i > 0,\\
!>     1, & \Delta h_i \le 0,
!>   \end{cases}
!> \]
!> with `A_i = CAIJ(FACE,i)`, `L = CDELL`, and `depadj` the adjacent channel
!> water depth added in the 1998 channel-aquifer-flow correction.
!>
!> The exchange flux stored in `CQH(FACE,i)` is
!> \[
!>   Q_i = K_i\,\Delta h_i\,{f_i A_i \over L},
!> \]
!> using the local hydraulic conductivity `CKIJ(i)`. The routine linearises this
!> contribution with
!> \[
!>   {dQ_i \over d\psi_i} = -K_i\,{f_i A_i \over L},
!> \]
!> adding that derivative to `CB(i)` and subtracting \(Q_i\) from `CR(i)` for
!> the tridiagonal column solve in [[vscolm]]. Although `CDKIJ` is passed in and
!> an older commented line shows a conductivity-derivative form, the active code
!> does not use `CDKIJ` and does not differentiate the depth-limiting factor
!> \(f_i\).
!>
!> @note
!> For positive head difference the code reduces the contact area by
!> `depadj/CDELZ(i)` but does not bound this factor below zero; `depadj` is
!> assumed to be non-negative. For draining cells, where \(\Delta h_i \le 0\),
!> the full face area is used.
!> @endnote
!>
!> Entry conditions from the legacy block are: `1 <= FACE <= 4`,
!> `ICBOT <= ICBED + 1`, `ICBOT <= ICTOP`, and `CDELL > 0`.
   SUBROUTINE VSSAI (FACE, JCBC, ICBOT, ICTOP, ICBED, CDELL, CZ, &
      CAIJ, CZS, CPSI, CKIJ, CDKIJ, CB, CR, CQH, depadj, cdelz)
      INTEGER, INTENT(IN) :: FACE                      !! Boundary face number, in `1:4`.
      INTEGER, INTENT(IN) :: JCBC                      !! Stream-aquifer boundary type, normally 9 or 10.
      INTEGER, INTENT(IN) :: ICBOT                     !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                     !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICBED                     !! River-bed cell index used for bank interaction.
      DOUBLEPRECISION, INTENT(IN) :: CDELL             !! Distance scale normal to the stream-aquifer face.
      DOUBLEPRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP)   !! Active-cell node elevations.
      DOUBLEPRECISION, INTENT(IN) :: CAIJ(4,ICBOT:ICTOP) !! Face areas by face and active cell.
      DOUBLEPRECISION, INTENT(IN) :: CZS               !! Adjacent channel water-surface elevation.
      DOUBLEPRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Current pressure heads.
      DOUBLEPRECISION, INTENT(IN) :: CKIJ(ICBOT:ICTOP) !! Lateral hydraulic conductivity terms on this face.
      DOUBLEPRECISION, INTENT(IN) :: CDKIJ(ICBOT:ICTOP) !! Unused conductivity derivatives retained for the legacy interface.
      DOUBLEPRECISION, INTENT(IN) :: depadj            !! Channel-depth adjustment for contact-area limiting.
      DOUBLEPRECISION, INTENT(IN) :: cdelz(ICBOT:ICTOP) !! Active-cell thicknesses used in the contact-area limit.
      DOUBLEPRECISION, INTENT(INOUT) :: CB(ICBOT:ICTOP) !! Matrix diagonal terms updated by stream-aquifer exchange.
      DOUBLEPRECISION, INTENT(INOUT) :: CR(ICBOT:ICTOP) !! Right-hand side terms updated by stream-aquifer exchange.
      DOUBLEPRECISION, INTENT(INOUT) :: CQH(4,ICBOT:ICTOP) !! Diagnostic lateral fluxes on the stream-aquifer face.
! Locals, etc
      INTEGER :: ICL, IDUM
      DOUBLEPRECISION QDUM, DQDUM, AOL, DH, KIJ
      DOUBLEPRECISION ddum
!----------------------------------------------------------------------*
! set lowest cell in exposed bank face
      IF (JCBC.EQ.9) THEN
!        * in effect stream bed is at base of current land element
         IDUM = ICBOT
      ELSE
!        * stream-aquifer interaction with banks
         IDUM = ICBED+1


      ENDIF
! loop over appropriate cells

      DO 200 ICL = IDUM, ICTOP

         DH = CZS - CZ (ICL) - CPSI (ICL)
! Limit channel-to-aquifer contact area when channel water depth is low.
         ddum = 1.0
         if (GTZERO(dh)) ddum = min (one, depadj / cdelz (icl) )

         AOL = (ddum * CAIJ (FACE, ICL) ) / CDELL
         KIJ = CKIJ (ICL)
! Active derivative excludes the conductivity derivative term.
!        DQDUM =   ( CDKIJ(ICL)*DH - KIJ ) * AOL
         dqdum = - kij * aol

         QDUM = KIJ * DH * AOL
         CQH (FACE, ICL) = QDUM
         CB (ICL) = CB (ICL) + DQDUM

         CR (ICL) = CR (ICL) - QDUM

200   END DO
   END SUBROUTINE VSSAI



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
!> On the final global iteration `ELEVEL` is passed to [[vscolm]] as `EEERR`,
!> but the non-convergence `ERROR` call in this routine uses `WWWARN`.
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
!> @note
!> `FIRSTvssim` gates the setup of `JCBCsv`, `VSAIJsv`, and `ICSOILsv`. Changes
!> to boundary-type arrays, layer soil types, element geometry, or explicit-bank
!> mode after the first call are therefore not reflected in the cached column
!> metadata. If an element were both a well and a spring, the spring flag would
!> overwrite the well flag in `JCBCsv(5,e)`; valid input should avoid that case.
!> @endnote
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
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-11-03 | SPA | - | Passed adjacent surface-water depth (`depadj`) to [[vscolm]], as well as the adjacent water-surface elevation, for the channel-aquifer flow correction. |
!> | 1998-11-04 | SPA | - | Made reported bank exchange flows consistent with BALWAT. |
!> @endhistory
   SUBROUTINE VSSIM ()
      INTEGER :: NITMAX, NITMIN
      DOUBLEPRECISION GEPSMX, DRYH
      PARAMETER (NITMAX = 10, NITMIN = 2, GEPSMX = 1D-4, DRYH = 1D-8)
      INTEGER :: N, IFDUM1, IFDUM2, NIT, NCELL, WET, ICDUM, K, ELEVEL
      INTEGER :: I, II, IEL, IFA, ICL, ILYR, IW, ITYPE, IBK, ISTART, &
         IBANK
      INTEGER :: JEL, JFA, JCL, JCBED, JELDUM (4), JCDEL1 (LLEE, 4)
      INTEGER :: ICBOT, ICTOP, ICWCAT, ICLBCT, ICBBCT, ICBED, ICWLBT
      INTEGER :: ICLYRB (NLYREE)
      DOUBLEPRECISION DPSIEL, DPSIMX, DELTAP (0:NELEE)
      DOUBLEPRECISION CDW, CES, CQW, CDNET (NELEE), CQ (LLEE, NELEE), &
         QBK, QI
      DOUBLEPRECISION CA0, CDELL (4), CDELL1 (4), CAIJ1 (LLEE, 4), &
         CZ1 (LLEE, 4)
      DOUBLEPRECISION DXYDUM
      DOUBLEPRECISION PSIM (LLEE), VSPSIN (LLEE, NELEE), VSTHEN (LLEE, &
         NELEE)
      DOUBLEPRECISION CPSI1 (LLEE, 4), CPSIN1 (LLEE, 4), CKIJ1 (LLEE, 4) &
         , CZS (4)
      integer,save :: errorcount2=0
! Adjacent surface-water depth used by the channel-aquifer flow correction.
      DOUBLEPRECISION depadj (4)
      LOGICAL :: TEST, OK (NELEE), g670
!----------------------------------------------------------------------*
! Initialization
!________________*
      IF (BEXBK) THEN
         IBANK = 1
         ISTART = 1
      ELSE
         IBANK = 0
         ISTART = total_no_links + 1
      ENDIF

      ICTOP = top_cell_no
      IF (FIRSTvssim) THEN

         FIRSTvssim = .FALSE.
!         * set outputs & locals for non-column elements
         IF (ISTART.GT.1) CALL ALINIT (ZERO, ISTART - 1, QH)
         DO 4 IEL = 1, ISTART - 1
            ICBOT = NLYRBT (IEL, 1)
            N = 4 * (ICTOP - ICBOT + 1)
            CALL ALINIT (ZERO, N, QVSH (1, ICBOT, IEL) )
            CALL ALINIT (ZERO, N, VSAIJsv (1, ICBOT, IEL) )
            DO 2 ICL = ICBOT, ICTOP
2           ICSOILsv (ICL, IEL) = 1

4        END DO
!        * set static locals for column elements
         DO 95 IEL = ISTART, total_no_elements
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
            DO 20 II = 1, 5
20          JCBCsv (II, IEL) = 0
            JCBCsv (0, IEL) = NBBTYP (IEL)
            IFA = MAX (1, NBFACE (IEL) )
            JCBCsv (IFA, IEL) = NLBTYP (IEL)
            IF (NVSWLI (IEL) .GT.0) JCBCsv (5, IEL) = 1
            IF (NVSSPC (IEL) .GT.0) JCBCsv (5, IEL) = 2
            DO 90 IFA = 1, 4
               JEL = ICMREF (IEL, IFA + 4)
               TEST = IEL.GT.total_no_links.AND.JEL.GE.1.AND.JEL.LE.total_no_links
               IF (TEST) JCBCsv (IFA, IEL) = 9 + IBANK
! VSAIJ contains cell-face areas for lateral flow (note face 1=3, 2=4)
               IFDUM1 = MOD (IFA, 4) + 1
               IFDUM2 = MOD (IFA + 2, 4) + 1
               DXYDUM = DHF (IEL, IFDUM1) + DHF (IEL, IFDUM2)
               DO 50 ICL = NLYRBT (IEL, 1), ICTOP
50             VSAIJsv (IFA, ICL, IEL) = DELTAZ (ICL, IEL) * DXYDUM
90          END DO
! ICSOIL contains soil types for each cell
            DO 93 ILYR = 1, NLYR (IEL)
               N = NTSOIL (IEL, ILYR)
               DO 92 ICL = NLYRBT (IEL, ILYR), NLYRBT (IEL, ILYR + 1) &
                  - 1
92             ICSOILsv (ICL, IEL) = N
93          END DO

95       END DO



      ENDIF
! prepare catchment boundary condition data

      CALL VSPREP
!!!!!! Calc. depth of water for channel links, even if no banks
! n.b. rainfall and evap terms neglected, as these are calculated for
! channels after VSS is called.
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (.not.bexbk) then
         do 107 iel = 1, total_no_links
            cdnet (iel) = GEThrf (iel) - zgrund (iel)
107      end do

      endif
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      DO 108 IEL = ISTART, total_no_elements

         CES = ESOILA (IEL)
         CDW = GETHRF (IEL) - ZGRUND (IEL)

         CDNET (IEL) = (PNETTO (IEL) - (EEVAP (IEL) - CES) ) * DTUZ + &
            CDW
         CA0 = cellarea (IEL)
         ICBOT = NLYRBT (IEL, 1)
         ICDUM = ICTOP + 1
         IF (IEL.GT.total_no_links) ICDUM = ICDUM - NRD (NVC (IEL) )
         IF (ICDUM.GT.ICBOT) CALL ALINIT (ZERO, ICDUM - ICBOT, CQ ( &
            ICBOT, IEL) )

! stop crash if rooting zone is below base of aquifer sb 020211
         icdum=max(1,icdum)

         DO 106 ICL = ICDUM, ICTOP
            CQ (ICL, IEL) = - ERUZ (IEL, ICL) * CA0
106      END DO

         CQ (ICTOP, IEL) = CQ (ICTOP, IEL) - CES * CA0



108   END DO
! save psi values at time level N
      DO 212 IEL = 1, total_no_elements
         ICBOT = NLYRBT (IEL, 1)
         NCELL = ICTOP - ICBOT + 1
         CALL DCOPY (NCELL, VSPSI (ICBOT, IEL), 1, VSPSIN (ICBOT, IEL), &
            1)
         CALL DCOPY (NCELL, VSTHE (ICBOT, IEL), 1, VSTHEN (ICBOT, IEL), &
            1)



212   END DO
! initialize convergence indicators
      CALL ALINIT (ZERO, ISTART, DELTAP)
      DO 214 IEL = 1, ISTART - 1
214   OK (IEL) = .TRUE.
      DO 216 IEL = ISTART, total_no_elements





216   OK (IEL) = .FALSE.
! start of main iteration loop
!______________________________*
      ELEVEL = 0

      g670=.FALSE.
      out660 : DO NIT = 1, NITMAX
         IF(g670) CYCLE
         IF (NIT.EQ.NITMAX) ELEVEL = EEERR
         DPSIMX = ZERO

         DO 500 I = 1, total_no_elements
            IEL = ISORT (I)
            IF (OK (IEL) ) GOTO 500
!                        >>>>>>>>
            ICBOT = NLYRBT (IEL, 1)
            ITYPE = ICMREF (IEL, 1)


            NCELL = ICTOP - ICBOT + 1
! save psi at iteration level m


            CALL DCOPY (NCELL, VSPSI (ICBOT, IEL), 1, PSIM (ICBOT), &
               1)
! set up column arrays using global arrays
            DO 250 ILYR = 1, NLYR (IEL) + 1

250         ICLYRB (ILYR) = NLYRBT (IEL, ILYR)

            IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) ICBED = NHBED (ICMREF (IEL, 4) &
               , ITYPE)

            DO 300 IFA = 1, 4
               CDELL (IFA) = DHF (IEL, IFA)
               JEL = ICMREF (IEL, IFA + 4)

               JELDUM (IFA) = JEL
               IF (JEL.LT.1) THEN
                  DXYDUM = ZERO
               ELSE

                  CZS (IFA) = GETHRF (JEL)
! Pass adjacent water depth as well as water-surface elevation.
                  depadj (ifa) = cdnet (jel)
                  JFA = ICMREF (IEL, IFA + 8)
                  DXYDUM = DHF (JEL, JFA)
               ENDIF
               CDELL1 (IFA) = DXYDUM

               IF (JEL.LT.ISTART) GOTO 300
!                                 >>>>>>>>
!              NB: VSPSI, VSKR may hold values from previous iteration
               K = MOD (JFA - 1, 2) + 1
               DO 285 JCL = NLYRBT (JEL, 1), top_cell_no
                  JCDEL1 (JCL, IFA) = JVSDEL (JFA, JCL, JEL)
                  CAIJ1 (JCL, IFA) = VSAIJsv (JFA, JCL, JEL)
                  CZ1 (JCL, IFA) = ZVSNOD (JCL, JEL)
                  CPSI1 (JCL, IFA) = VSPSI (JCL, JEL)
                  CPSIN1 (JCL, IFA) = VSPSIN (JCL, JEL)
                  N = ICSOILsv (JCL, JEL)
                  CKIJ1 (JCL, IFA) = VSKR (JCL, JEL) * VSK3D (N, K)

285            END DO


300         END DO
! boundary condition indices
            IW = MAX (1, NVSWLI (IEL) )
            ICWLBT = NWELBT (IEL)
            ICWCAT = NVSWLC (IEL)
            ICLBCT = NLBCAT (IEL)


            ICBBCT = NBBCAT (IEL)
! calculate new potentials and flow rates



            CALL VSCOLM (NSEE, VSWV, VSWL, VSK3D, BHELEV, ELEVEL, &
               IEL, ICBOT, ICTOP, ICBED, ICLYRB, ICSOILsv (ICBOT, IEL), &
               JCBCsv (0, IEL), JCDEL1, JELDUM, JVSACN (1, ICBOT, IEL), &
               JVSDEL (1, ICBOT, IEL), NVSSPC (IEL), NVSLFN (ICLBCT), &
               NVSLFL (1, ICLBCT), NWELBT (IEL), NVSLHN (ICLBCT), NVSLHL ( &
               1, ICLBCT), NWELTP (IEL), NVSLGN (ICLBCT), NVSLGL (1, &
               ICLBCT), cellarea (IEL), ZGRUND (IEL), VSSPZ (IEL), VSSPCO (IEL) &
               , DELTAZ (ICBOT, IEL), ZVSNOD (ICBOT, IEL), CDELL, VSAIJsv (1, &
               ICBOT, IEL), CAIJ1, CDELL1, CZ1, DTUZ, CDNET (IEL), VSPSIN ( &
               ICBOT, IEL), CQ (ICBOT, IEL), CZS, CPSI1, CPSIN1, CKIJ1, &
               WLNOW (ICWCAT), RLFNOW (1, ICLBCT), RLHNOW (1, ICLBCT), &
               RLGNOW (1, ICLBCT), RBFNOW (ICBBCT), RBHNOW (ICBBCT), &
               IVSSTO (ICBOT, IEL), VSPSI (ICBOT, IEL), VSKR (ICBOT, IEL), &
               VSTHE (ICBOT, IEL), QVSH (1, ICBOT, IEL), QVSV (ICBOT - 1, &
               IEL), QVSWLI (ICWLBT, IW), QVSSPR (IEL), ZVSPSL (IEL), &
               depadj)
! record largest change for this iteration
            DPSIEL = ZERO
            DO 400 ICL = ICBOT, ICTOP
400         DPSIEL = MAX (DPSIEL, ABS (VSPSI (ICL, IEL) - PSIM (ICL) ) )
            DELTAP (IEL) = DPSIEL



            DPSIMX = MAX (DPSIMX, DPSIEL)
! end of element loop: check for convergence or maximum iterations
500      END DO
!970214  At present the criterion on DPSIMX overrides that on NIT
         IF (DPSIMX.LE.GEPSMX) THEN
            g670 =.TRUE.
            CYCLE out660
         ENDIF
         IF (NIT.GE.NITMIN) THEN
            DO 650 IEL = ISTART, total_no_elements
               DPSIEL = DELTAP (IEL)
               DO 640 IFA = 1, 4
                  JEL = MAX (0, ICMREF (IEL, IFA + 4) )
                  DPSIEL = MAX (DPSIEL, DELTAP (JEL) )
640            END DO
               OK (IEL) = DPSIEL.LT.GEPSMX
650         END DO



         ENDIF
! end of iteration loop
      ENDDO out660
      IF(.NOT.g670) then
         errorcount2=errorcount2+1
         if (errorcount2.lt.errcntallowed) then
            CALL ERROR(WWWARN, 1039, PPPRI, 0, 0, 'Maximum iterations in VSS global solver')
         elseif (errorcount2.eq.errcntallowed) then
            CALL ERROR (WWWARN, 1039, PPPRI, 0, 0, '**** Last printout of the error message - maximum iterations in VSS global solver *****')
         endif
      endif




670   CONTINUE
! main solution is complete: tidy up
!____________________________________*
! update flows to ensure mass conservation


      CALL VSMB (VSTHEN)
! set auxiliary output arrays
      DO 700 IEL = ISTART, total_no_elements
         ICBOT = NLYRBT (IEL, 1)
         QVSBF (IEL) = QVSV (ICBOT - 1, IEL)
         QH (IEL) = QVSV (ICTOP, IEL)
         IW = NVSWLI (IEL)
         IF (IW.LT.1) GOTO 700
!                     >>>>>>>>
         CQW = ZERO
         DO 690 ICL = NWELBT (IEL), NWELTP (IEL)
690      CQW = QVSWLI (ICL, IW) + CQW
         QVSWEL (IEL) = CQW


700   END DO
! calculate QBKB, QBKF, QBKI for all cases:
!     bank elements or not, including dry channels
      DO 780 IBK = 1, 2

         DO 760 IEL = 1, total_no_links
            QI = - HALF * cellarea (IEL) * QH (IEL)

            WET = NINT (HALF + SIGN (HALF, GETHRF (IEL) - ZGRUND (IEL) &
               - DRYH) )
            IFA = 2 * IBK

            IF (LINKNS (IEL) ) IFA = IFA - 1
            JEL = ICMREF (IEL, IFA + 4)

            JFA = ICMREF (IEL, IFA + 8)
            JCBED = top_cell_no
            IF (JEL.GT.0) JCBED = NLYRBT (JEL, 1) - 1

            IF (BEXBK) JCBED = NHBED (IEL, IBK)
            QBK = ZERO
            DO 740 JCL = JCBED+1, top_cell_no
               QBK = QBK + QVSH (JFA, JCL, JEL)

740         END DO
! Keep exchange-flow definitions consistent with BALWAT.
            QBKF (IEL, IBK) = QBK
            QBKB (IEL, IBK) = QI * IBANK * WET

            QBKI (IEL, IBK) = QI * IBANK * (1 - WET)
760      END DO

780   END DO

   END SUBROUTINE VSSIM

!> Builds internal soil hydraulic property lookup tables.
!>
!> `VSSOIL` converts the `VS05`/`VS05a` hydraulic-property input read by
!> [[vsread]] into the pressure-head lookup tables interpolated by [[vsfunc]].
!> It is called once during [[vsin]] initialisation.
!>
!> Table size and pressure-head grid:
!>
!> | Control | Effect |
!> |:--------|:-------|
!> | `BFAST = .TRUE.` | Use `NVSSOL = min(100, NSOLEE)` lookup entries. |
!> | `BFAST = .FALSE.` | Use `NVSSOL = min(500, NSOLEE)` lookup entries. |
!> | `I = 5:NVSSOL-1` | Main unsaturated grid, logarithmically spaced from about \(-10^{-2}\) m toward \(-10^4\) m. |
!> | `I = NVSSOL` | Extreme dry endpoint, `VSPPSI = -1.0e6`, `theta = theta_res`, `K_r = 0`. |
!> | `I = 1:4` | Saturated/positive-head extension with `VSPPSI = 1.0e6, 0.5, 0.25, 0`. |
!>
!> Stored table columns:
!>
!> | Array | Meaning |
!> |:------|:--------|
!> | `VSPPSI` | Pressure head \(\psi\). |
!> | `VSPTHE` | Volumetric water content \(\theta\). |
!> | `VSPDTH` | \(d\theta/d\psi\). |
!> | `VSPETA` | Storage coefficient \(\eta = \theta S_s/\theta_s + d\theta/d\psi\). |
!> | `VSPDET` | \(d\eta/d\psi\), where available/implemented. |
!> | `VSPKR` | Relative hydraulic conductivity \(K_r\). |
!> | `VSPDKR` | \(dK_r/d\psi\), where available/implemented. |
!>
!> Derivative-table coverage is uneven in the active implementation:
!>
!> | Soil option | `VSPDTH`/`VSPETA` | `VSPDET` | `VSPDKR` before final DSATG adjustment |
!> |:------------|:------------------|:---------|:--------------------------------------|
!> | `IVSFLG=1` | Analytic van Genuchten water-content derivative and storage. | Set to zero in the main unsaturated range. | Analytic line is commented out, so the main-range values are not assigned here. |
!> | `IVSFLG=2` | `VSPDTH` is a forward finite difference after spline interpolation of tabulated \(\theta\). | Forward finite difference of `VSPDTH`, plus the specific-storage term. | Not assigned here, although tabulated `K_r` itself is spline-interpolated before being overwritten below. |
!> | `IVSFLG=3` | Analytic exponential derivative and storage. | Analytic exponential derivative of storage. | Analytic exponential derivative, but inconsistent with the final DSATG-adjusted `VSPKR`. |
!>
!> Implemented soil-property options:
!>
!> | `IVSFLG` | Manual option | Code path |
!> |:---------|:--------------|:----------|
!> | `1` | van Genuchten water-retention/conductivity parameters | Computes \(\theta(\psi)\), \(d\theta/d\psi\), and storage from the van Genuchten form. |
!> | `2` | User tables for \(\theta(\psi)\) and \(K_r(\psi)\) | Cubic-spline interpolation in \(\log_{10}(-\psi)\); tabulated \(\theta\) is scaled by porosity. |
!> | `3` | Exponential functions | Uses \(\theta=\theta_r+(\theta_s-\theta_r)\exp(\alpha\psi)\) and \(K_r=\exp(\alpha\psi)\) before the final adjustment below. |
!> | `4` | User table for \(\theta(\psi)\) with Averjanov \(K(\theta)\) | Parsed as a legacy option, but stops in this routine. |
!>
!> For `IVSFLG = 1`, the retention curve uses
!> \[
!>   \theta = \theta_r +
!>   {\theta_s-\theta_r \over \left(1+(-\alpha\psi)^n\right)^m},
!>   \qquad m = 1 - {1 \over n},
!> \]
!> with `VSALPH` converted from cm-1 to m-1. The initial conductivity expression
!> follows the closed-form van Genuchten/Mualem style relation described by
!> van Genuchten (1980), https://doi.org/10.2136/sssaj1980.03615995004400050002x.
!>
!> After the method-specific tables are built, the current code applies a DSATG
!> adjustment over `I = 5:NVSSOL` for every soil type:
!> \[
!>   K_r = \left({\theta-\theta_r \over \theta_s-\theta_r}\right)^2 .
!> \]
!> This means the final `VSPKR` values used by [[vsfunc]] do not retain the
!> earlier method-specific conductivity curve in the main lookup range. The
!> derivative table `VSPDKR` is not recomputed by this adjustment, so `CDKR`
!> from [[vsfunc]] should be treated as an approximate or stale derivative,
!> depending on the soil option.
!>
!> The wet extension is also partly synthetic: `VSPKR(1:4,:) = 1`, `VSPTHE(4,:)`
!> is porosity, `VSPTHE(3:1,:)` is extrapolated from storage terms, and
!> `VSPDET(1:4,:)` is forced to zero.
!>
!> If `BSOILP` is enabled, the generated tables are written to `PPPRI`.
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07 to 1996-01 | GP | 4.0 | Written as the VSS soil hydraulic property table generator. |
!> @endhistory
!>
!> @warning `IVSFLG=4` is parsed as a legacy option for tabulated water content
!> with Averjanov-style relative conductivity, but this code path is unfinished
!> and stops the run.
!> @endwarning
   SUBROUTINE VSSOIL ()
      INTEGER :: I, IS, NTBPOS (NSEE), NDUM
      DOUBLEPRECISION RVSSOL, PSI, EDUM, EEDUM, DDDUM
      DOUBLEPRECISION DDTSAT, DDTRES, DDA, DDN, DDM, DD1M1, DDTSMR, &
         DDAP, DDAPN, DDAPN1, DDAPM, DDAPM1, DDAPM2, DDTCAP, DDTC, DDTCM, &
         DDTCM1, DDTCM2, DDDTCP

      DOUBLEPRECISION PLOG, PLOGLO, PLOGHI, ADUM, BDUM, HDUM, rkrdum

      PARAMETER (EDUM = 2.718281828D0)


      DATA NTBPOS / NSEE * 1 /
!----------------------------------------------------------------------*
! soil flags:
!       1       van Genuchten
!       2       tabulated theta(psi) and Kr(psi)
!       3       exponential
!       4       tabulated theta(psi), Averjanov Kr (compatible with V3.4
!----------------------------------------------------------------------*
!
! set up size of internal look-up tables
      IF (BFAST) THEN
         NVSSOL = MIN0 (100, NSOLEE)
      ELSE
         NVSSOL = MIN0 (500, NSOLEE)
      ENDIF


      RVSSOL = DBLE (NVSSOL)
! loop over NVSSOL divisions of the soil property tables
! (NB. low values of I correspond to wet soils)
! psi ranges from -(10**-2) to -(10**4)

      DO 500 I = 5, NVSSOL - 1
         PSI = - 10.D0** ( - two + DBLE (6 * (I - 5) ) / RVSSOL)


         VSPPSI (I) = PSI
! set up property data for each soil type, using method ...


         DO 400 IS = 1, NS
! ... 1 (Van Genuchten)

            IF (IVSFLG (IS) .EQ.1) THEN
               DDTSAT = VSPOR (IS)
               DDTRES = VSTRES (IS)
               DDA = VSALPH (IS) * 100.0d0

               DDN = VSVGN (IS)
               DDM = one - (one / DDN)

               DD1M1 = (one / DDM) - one

               DDTSMR = DDTSAT - DDTRES
               DDAP = - DDA * PSI
               DDAPN = DDAP**DDN
               DDAPN1 = DDAP** (DDN - one)
               DDAPM = (one + DDAPN) **DDM
               DDAPM1 = (one + DDAPN) ** (DDM + one)

               DDAPM2 = (one + DDAPN) ** (DDM + two)

               DDDTCP = DDA * DDM * DDN * DDAPN1 / DDAPM1

               VSPTHE (I, IS) = DDTRES + DDTSMR / DDAPM

               VSPDTH (I, IS) = DDTSMR * DDDTCP
               DDTCAP = MAX (1.0d-10, (VSPTHE (I, IS) - DDTRES) &
                  / DDTSMR)
               DDTC = one - (DDTCAP** (one / DDM) )
               DDTCM = DDTC**DDM
               DDTCM1 = DDTC** (DDM - one)

               DDTCM2 = (one - DDTCM) **two


               VSPKR (I, IS) = DSQRT (DDTCAP) * DDTCM2
!            VSPDKR(I,IS) = DSQRT(DDTCAP)*(one-DDTCM)*
!     -        (half*(one-DDTCM)/DDTCAP +
!     -         two*DDTCM1*DDTCAP**DD1M1) * DDDTCP

               DDDUM = (DDA * DDA * DDM * DDN * DDTSMR * DDAPN1 / &
                  DDAPM2) * ( (DDN - one) * (one + DDAPN) + (DDM + &
                  one) * DDN * DDAPN1)
               VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
                  + VSPDTH (I, IS)
!cc            VSPDET(I,IS) = VSPDTH(I,IS)*VSPSS(IS)/VSPOR(IS) +
!cc     -                     DDDUM


               vspdet (i, is) = zero
! ... 2 (tabulated theta and Kr)


            ELSEIF (IVSFLG (IS) .EQ.2) THEN
!               check for correct location in input table
!               (interpolate between positions NTBPOS(IS) and NTBPOS(IS+
               IF (PSI.LT.TBPSI (NTBPOS (IS) + 1, IS) ) NTBPOS (IS) &
                  = NTBPOS (IS) + 1


               NDUM = NTBPOS (IS)
!               evaluate cubic spline polynomial for theta and Kr
               PLOG = DLOG10 ( - PSI)
               PLOGHI = DLOG10 ( - TBPSI (NDUM + 1, IS) )
               PLOGLO = DLOG10 ( - TBPSI (NDUM, IS) )
               HDUM = PLOGHI - PLOGLO
               ADUM = (PLOGHI - PLOG) / HDUM

               BDUM = (PLOG - PLOGLO) / HDUM
               VSPTHE (I, IS) = ADUM * TBTHE (NDUM, IS) + BDUM * TBTHE ( &
                  NDUM + 1, IS) + ( (ADUM**three - ADUM) * TBTHEC (NDUM, &
                  IS) + (BDUM**three - BDUM) * TBTHEC (NDUM + 1, IS) ) &
                  * (HDUM**two) / 6.0D0

               VSPTHE (I, IS) = VSPOR (IS) * VSPTHE (I, IS)


               VSPKR (I, IS) = ADUM * TBKR (NDUM, IS) + BDUM * TBKR ( &
                  NDUM + 1, IS) + ( (ADUM**three - ADUM) * TBKRC (NDUM, IS) &
                  + (BDUM**three - BDUM) * TBKRC (NDUM + 1, IS) ) * &
                  (HDUM**two) / 6.0D0
! ... 3 (exponential)

            ELSEIF (IVSFLG (IS) .EQ.3) THEN

               EEDUM = EDUM** (VSALPH (IS) * PSI)
               VSPTHE (I, IS) = VSTRES (IS) + (VSPOR (IS) - VSTRES (IS) &
                  ) * EEDUM
               VSPDTH (I, IS) = (VSPOR (IS) - VSTRES (IS) ) * VSALPH ( &
                  IS) * EEDUM
               DDDUM = VSPDTH (I, IS) * VSALPH (IS)
               VSPKR (I, IS) = EEDUM

               VSPDKR (I, IS) = VSALPH (IS) * EEDUM
               VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
                  + VSPDTH (I, IS)


               VSPDET (I, IS) = VSPDTH (I, IS) * VSPSS (IS) / VSPOR (IS) &
                  + DDDUM
! ... 2/4 (tabulated theta and Kr / tabulated theta and Averjanov Kr)

            ELSEIF (IVSFLG (IS) .EQ.4) THEN

               stop 'UNFINISHED code for soil properties type 4'

            ENDIF

400      END DO


500   END DO
! set up property data for extreme dry conditions

      VSPPSI (NVSSOL) = - 1.0D6
      DO 700 IS = 1, NS
         VSPTHE (NVSSOL, IS) = VSTRES (IS)
         VSPKR (NVSSOL, IS) = zero
         VSPETA (NVSSOL, IS) = zero
         VSPDTH (NVSSOL, IS) = zero
         VSPDKR (NVSSOL, IS) = zero
         VSPDET (NVSSOL, IS) = zero


700   END DO
! set up storage term for tabulated data
      DO 540 I = 5, NVSSOL - 1
         DO 520 IS = 1, NS
            IF (IVSFLG (IS) .EQ.2.OR.IVSFLG (IS) .EQ.4) VSPDTH (I, IS) &
               = (VSPTHE (I + 1, IS) - VSPTHE (I, IS) ) / (VSPPSI (I + 1) &
               - VSPPSI (I) )
            VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
               + VSPDTH (I, IS)
520      END DO

540   END DO
      DO 560 I = 5, NVSSOL - 1
         DO 550 IS = 1, NS
            IF (IVSFLG (IS) .EQ.2.OR.IVSFLG (IS) .EQ.4) VSPDET (I, IS) &
               = VSPDTH (I, IS) * VSPSS (IS) / VSPOR (IS) + (VSPDTH (I + 1, &
               IS) - VSPDTH (I, IS) ) / (VSPPSI (I + 1) - VSPPSI (I) )
550      END DO


560   END DO
! set up property data for extreme wet conditions
      VSPPSI (4) = zero
      VSPPSI (3) = 2.5d-1
      VSPPSI (2) = 5.0D-1

      VSPPSI (1) = 1.0D6
      DO 600 IS = 1, NS
         VSPKR (4, IS) = one
         VSPKR (3, IS) = one
         VSPKR (2, IS) = one
         VSPKR (1, IS) = one
         VSPETA (4, IS) = vspeta (5, is)
         VSPETA (3, IS) = vspeta (4, is)
         VSPETA (2, IS) = VSPSS (IS)
         VSPETA (1, IS) = VSPSS (IS)
         VSPDTH (4, IS) = vspdth (5, is)
         VSPTHE (4, IS) = vspor (is)
         VSPTHE (3, IS) = vspor (is) + vspeta (4, is) * (vsppsi (3) &
            - vsppsi (4) )
         VSPTHE (2, IS) = vspthe (3, is) + vspeta (3, is) * (vsppsi (2) &
            - vsppsi (3) )
         VSPTHE (1, IS) = vspthe (2, is) + vspss (is) * (vsppsi (1) &
            - vsppsi (2) )
         VSPDTH (3, IS) = zero
         VSPDTH (2, IS) = zero
         VSPDTH (1, IS) = zero
         VSPDKR (4, IS) = vspdkr (5, is)
         VSPDKR (3, IS) = zero
         VSPDKR (2, IS) = zero
         VSPDKR (1, IS) = zero
!        VSPDET(3,IS) = vspdet(4,is)
         VSPDET (4, IS) = zero
         VSPDET (3, IS) = zero
         VSPDET (2, IS) = zero
         VSPDET (1, IS) = zero





600   END DO
! adjust theta for specific storage in the unsaturated zone
!      delpsi=0.0
!      do 610 i=nvssol-1,3,-1
!        delpsi = delpsi+vspthe(i,is)*(vsppsi(i)-vsppsi(i+1))
!        do 605 is=1,ns
!          vspthe(i,is) = vspthe(i,is) *
!     -      (one + vspss(is)*delpsi/vspor(is))
! 605    continue
! 610  continue
! add increment to eta, for stability near water table
!      DO 660 IS=1,NS
!        ETAMAX = 0.0D0
!        DO 620 I=1,NVSSOL
!          ETAMAX = MAX(ETAMAX,VSPETA(I,IS))
! 620    CONTINUE
!        DO 640 I=1,NVSSOL
!          VSPETA(I,IS) = VSPETA(I,IS) +
!     -      0.1d0*ETAMAX*MAX( (1.0D0-DABS(VSPPSI(I))), 0.0D0)
! 640    CONTINUE
! 660  CONTINUE
! DSATG-specific code - adjust relative conductivity curves so that
! Kr approaches unity at saturation (for values of VG-n less than 2,
! the value of Kr drops rapidly and unphysically less than one near satu
      do 680 is = 1, ns
         rkrdum = vspor (is) - vstres (is)
         do 670 i = 5, nvssol
            vspkr (i, is) = ( (vspthe (i, is) - vstres (is) ) / rkrdum) &
               **two
670      end do



680   end do
! write soil property tables to PRI file

      IF (BSOILP) THEN

         WRITE(PPPRI, 905) NS, NVSSOL
         DO 800 IS = 1, NS
            WRITE(PPPRI, 910) IS
            DO 820 I = 1, NVSSOL
               WRITE(PPPRI, 920) I, VSPPSI (I), VSPTHE (I, IS), VSPETA ( &
                  I, IS), VSPKR (I, IS), VSPDTH (I, IS), VSPDET (I, IS), &
                  VSPDKR (I, IS)
820         END DO

800      END DO

      ENDIF

905   FORMAT(/ 'VSS physical soil/lithology property data' / &
      &         '=========================================' / &
      &         I3, ' soils' / &
      &         I3, ' values in soil property tables' )

910   FORMAT(/ &
      & 3X,'  Soil property tables for soil/lithology type: ',I3 / &
      & 3X,'  -------------------------------------------------' // &
      & 3X,'      psi         theta          eta            Kr      ', &
      & ' d(the)/d(psi) d(eta)/d(psi)  d(Kr)/d(psi)' / &
      & 3X,'   (VSPPSI)      (VSPTHE)      (VSPETA)       (VSPKR)   ', &
      & '   (VSPDTH)      (VSPDET)       (VSPDKR)  ' / &
      & 3X,'  ------------  ------------  ------------  ------------', &
      & '  ------------  ------------  ------------' )

920   FORMAT(I3,7(2X,G14.6))
      RETURN
   END SUBROUTINE VSSOIL



!> Adds spring discharge terms to one VSS cell.
!>
!> `VSSPR` implements the spring boundary type (`JCBC(5) = 2`) for the single
!> cell selected by [[vsin]] from the manual `VS13b` spring source depth
!> `VSSPD`. The discharge elevation and spring coefficient are the `VS13b`
!> inputs passed here as `CZSP` and `CCS`.
!>
!> The spring is inactive while the hydraulic head in the source cell is below
!> the discharge elevation:
!> \[
!>   H - z_{\rm sp} = z_i + \psi_i - z_{\rm sp} < 0 .
!> \]
!> If the head is high enough, the routine computes the spring outflow as
!> \[
!>   Q_{\rm sp} = C_{\rm sp}\,K_r\,\left(z_i + \psi_i - z_{\rm sp}\right),
!> \]
!> where `CZ` is \(z_i\), `CPSI` is \(\psi_i\), `CKR` is the current relative
!> hydraulic conductivity from [[vsfunc]], and `CCS` is the spring coefficient.
!>
!> For an active spring, `CQSP` receives \(Q_{\rm sp}\), `CR` is increased by the
!> same flux, and `CB` is updated with the implemented linearisation term
!> `-CCS * CDKR`. For an inactive spring, `CQSP` is set to zero and the column
!> coefficients are unchanged.
!>
!> @note
!> This is not the full derivative of
!> \(C_{\rm sp}K_r(z_i+\psi_i-z_{\rm sp})\) with respect to pressure head,
!> which would include both the direct `CKR` term and the head-excess multiplier
!> on `CDKR`. The active implementation uses only `-CCS * CDKR`. Because the
!> activation test is `GEZERO`, a zero head excess gives zero spring flux but
!> still applies this coefficient term.
!> @endnote
   SUBROUTINE VSSPR (CZ, CZSP, CCS, CPSI, CKR, CDKR, CB, CR, CQSP)
      DOUBLEPRECISION, INTENT(IN) :: CZ    !! Spring-cell node elevation.
      DOUBLEPRECISION, INTENT(IN) :: CZSP  !! Spring discharge elevation.
      DOUBLEPRECISION, INTENT(IN) :: CCS   !! Spring conductance coefficient.
      DOUBLEPRECISION, INTENT(IN) :: CPSI  !! Spring-cell pressure head.
      DOUBLEPRECISION, INTENT(IN) :: CKR   !! Spring-cell relative hydraulic conductivity.
      DOUBLEPRECISION, INTENT(IN) :: CDKR  !! Derivative of `CKR` with respect to pressure head.
      DOUBLEPRECISION, INTENT(INOUT) :: CB !! Spring-cell matrix diagonal term.
      DOUBLEPRECISION, INTENT(INOUT) :: CR !! Spring-cell right-hand side term.
      DOUBLEPRECISION, INTENT(OUT) :: CQSP !! Spring discharge flux.
! Locals, etc
      DOUBLEPRECISION DHDUM
!
!----------------------------------------------------------------------*
!

      DHDUM = CPSI + CZ - CZSP

      IF (GEZERO(DHDUM)) THEN

         CQSP = CCS * CKR * DHDUM
         CR = CR + CQSP

         CB = CB - CCS * CDKR

      ELSE

         CQSP = zero

      ENDIF
   END SUBROUTINE VSSPR



!> Adds the upper infiltration/exfiltration boundary to the top VSS cell.
!>
!> `VSUPPR` forms the top-boundary contribution for one VSS column. The input
!> `CDNET` is the net surface-water depth available over the timestep after
!> evaporation has been applied by [[vssim]], and `CKZS` is the vertical
!> saturated conductivity of the top cell. The routine uses the model flux
!> convention that `CQINF > 0` is upward from the subsurface to the surface, so
!> infiltration is negative.
!>
!> The water-availability limit is
!> \[
!>   q_{\rm in} = {d_{\rm net} \over \Delta t},
!> \]
!> the rate that would exhaust the available surface depth during the timestep.
!> The hydraulic-capacity expression is
!> \[
!>   q_{\rm out} =
!>   {K_{zs} \over \Delta z/2}
!>   \left[\psi -
!>   \left(\max(d_{\rm net},0)+{\Delta z\over2}\right)\right],
!> \]
!> where `CPSI` is top-cell pressure head and `CDELZ` is top-cell thickness.
!>
!> If available water is limiting (`q_in < -q_out`), the returned flux is
!> `CQINF = -q_in` and the derivative contribution is set to zero. Otherwise
!> the boundary is hydraulic-capacity limited, or exfiltrating, and
!> `CQINF = q_out` with derivative `CKZS/(CDELZ/2)`.
!>
!> @note
!> `CKZS` is the saturated vertical conductivity passed from `VSK3D(SOIL,3)`;
!> the upper-boundary capacity does not use the current relative conductivity
!> `CKR` from [[vsfunc]]. Positive `CDNET` is treated as ponded depth in the
!> hydraulic head term. Negative `CDNET` can limit upward extraction through
!> `q_in = CDNET/DT`, but `MAX(CDNET,0)` means it does not impose a negative
!> surface-water head in `q_out`.
!> @endnote
!>
!> The column-system updates are
!> \[
!>   CB \leftarrow CB - {K_{zs}\over\Delta z/2}\,A,\qquad
!>   CR \leftarrow CR + q_{\rm inf} A,
!> \]
!> except in the water-limited case where the coefficient term is zero.
!>
!> Entry conditions: `CDELZ > 0` and `DT > 0`.
   SUBROUTINE VSUPPR (CA0, CDELZ, CKZS, DT, CDNET, CPSI, CB, CR, &
      CQINF)
      DOUBLEPRECISION, INTENT(IN) :: CA0    !! Plan area of the current element.
      DOUBLEPRECISION, INTENT(IN) :: CDELZ  !! Top-cell thickness.
      DOUBLEPRECISION, INTENT(IN) :: CKZS   !! Saturated vertical hydraulic conductivity for the top-cell soil.
      DOUBLEPRECISION, INTENT(IN) :: DT     !! Timestep length.
      DOUBLEPRECISION, INTENT(IN) :: CDNET  !! Net available surface-water depth after evaporation.
      DOUBLEPRECISION, INTENT(IN) :: CPSI   !! Top-cell pressure head.
      DOUBLEPRECISION, INTENT(INOUT) :: CB  !! Top-cell matrix diagonal term.
      DOUBLEPRECISION, INTENT(INOUT) :: CR  !! Top-cell right-hand side term.
      DOUBLEPRECISION, INTENT(OUT) :: CQINF !! Calculated upward-positive infiltration/exfiltration rate.
! Locals, etc
!INTRINSIC MAX


      DOUBLEPRECISION QIN, QOUT, CDQINF, DZO2
!----------------------------------------------------------------------*
! CDNET = total net available depth of surface water after evaporation
! QIN   = infiltration rate which would exhaust CDNET
! QOUT  = exfiltration rate based on transport (-ve for infiltration)
! CQINF = calculated exfiltration rate
!         (+ve upwards, to be consistent with the global array, QH)
!----------------------------------------------------------------------*
      DZO2 = half * CDELZ
      QIN = CDNET / DT
      CDQINF = CKZS / DZO2


      QOUT = CDQINF * (CPSI - (MAX (CDNET, ZERO) + DZO2) )
! infiltration (limited by available water) or evaporation

      IF (QIN.LT. - QOUT) THEN
         CQINF = - QIN


         CDQINF = ZERO
! infiltration (limited by soil properties) or exfiltration

      ELSE

         CQINF = QOUT


      ENDIF
! add into right-hand-side of column tridiagonal system
      CB = CB - CDQINF * CA0

      CR = CR + CQINF * CA0
   END SUBROUTINE VSUPPR



!> Distributes a prescribed well abstraction over screened VSS cells.
!>
!> `VSWELL` implements the well boundary type (`JCBC(5) = 1`) for the screen
!> interval `ICWLBT:ICWLTP`, which is derived in [[vsin]] from the manual
!> `VS12b` well-screen depths. The prescribed input `CQWIN` is the total well
!> abstraction rate in m3/s, read for the current timestep by [[vsprep]] from
!> the well data file.
!>
!> Each screened cell is first assigned a saturated lateral
!> conductivity-depth weight,
!> \[
!>   w_i = {K_{x,i}+K_{y,i} \over 2}\,\Delta z_i,\qquad
!>   W = \sum_{i=I_b}^{I_t} w_i .
!> \]
!> The available saturated thickness factor is then limited using the current
!> pressure head:
!> \[
!>   f_i =
!>   {\min\left(d_i,\max(\psi_i,0)\right) \over d_i},\qquad
!>   d_i = { \Delta z_i+\Delta z_{i+1} \over 2}.
!> \]
!>
!> The cell abstraction is
!> \[
!>   Q_i = Q_{\rm well}\,{w_i \over W}\,f_i ,
!> \]
!> so the total realised abstraction can be less than the prescribed value when
!> screened cells are partly or fully unsaturated. `CQWI(i)` stores the
!> corresponding areal rate \(Q_i/A\) in m/s, and `CR(i)` is increased by
!> \(Q_i\) for the column right-hand side.
!>
!> @note
!> The pressure-head reduction factor is evaluated explicitly. The routine does
!> not add a diagonal coefficient for the dependence of \(f_i\) on `CPSI(i)`, so
!> well abstraction changes affect the nonlinear iteration only through the next
!> column assembly. The sign convention assumes positive `CQWIN` is abstraction
!> from the VSS column.
!> @endnote
!>
!> Entry conditions: `ICWLBT <= ICWLTP`;
!> `1 <= ICSOIL(ICWLBT:ICWLTP) <= NSEE`; and positive `CA0`, screened-cell
!> thicknesses including `CDELZ(ICWLTP+1)`, and a positive total
!> conductivity-depth weight \(W\) from
!> `VSK3D(ICSOIL(ICWLBT:ICWLTP),1:2)`.
   SUBROUTINE VSWELL (NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL, CA0, &
      CDELZ, CQWIN, CPSI, CR, CQWI, RKZDUM)
      INTEGER, INTENT(IN) :: NSEE                    !! Declared soil-type dimension for conductivity arrays.
      INTEGER, INTENT(IN) :: ICWLBT                  !! Bottom screened well cell.
      INTEGER, INTENT(IN) :: ICWLTP                  !! Top screened well cell.
      INTEGER, INTENT(IN) :: ICSOIL(ICWLBT:ICWLTP)   !! Soil type by screened cell.
      DOUBLEPRECISION, INTENT(IN) :: CDELZ(ICWLBT:ICWLTP+1) !! Screened-cell thicknesses plus the cell above the screen top.
      DOUBLEPRECISION, INTENT(IN) :: VSK3D(NSEE,2)   !! Saturated x/y hydraulic conductivity by soil type.
      DOUBLEPRECISION, INTENT(IN) :: CA0             !! Plan area of the current element.
      DOUBLEPRECISION, INTENT(IN) :: CPSI(ICWLBT:ICWLTP) !! Current pressure heads in screened cells.
      DOUBLEPRECISION, INTENT(IN) :: CQWIN           !! Prescribed total well abstraction rate.
      DOUBLEPRECISION, INTENT(INOUT) :: CR(ICWLBT:ICWLTP) !! Right-hand side terms updated with realised abstraction.
      DOUBLEPRECISION, INTENT(OUT) :: CQWI(ICWLBT:ICWLTP) !! Realised well abstraction rate per cell area.
      DOUBLEPRECISION, INTENT(OUT) :: RKZDUM(ICWLBT:ICWLTP) !! Workspace for conductivity-depth weights.
! Locals, etc
!INTRINSIC MAX, MIN
      INTEGER :: ICL, SOIL




      DOUBLEPRECISION RKZTOT, DZDUM, PDUM, QDUM, RKZ
!----------------------------------------------------------------------*
! The value of CQWIN is the prescribed abstraction rate (m3/s).
! The actual abstraction rate CQWI (m/s) may be less than this if some
! of the aquifer around the well screen becomes unsaturated
! (ie if CPSI(ICL) < DZDUM below).
! calculate product of mean lateral hydraulic conductivity & cell depth
      RKZTOT = ZERO
      DO 50 ICL = ICWLBT, ICWLTP
         SOIL = ICSOIL (ICL)
         RKZ = half * (VSK3D (SOIL, 1) + VSK3D (SOIL, 2) ) * CDELZ ( &
            ICL)
         RKZDUM (ICL) = RKZ
         RKZTOT = RKZ + RKZTOT


50    END DO
! calculate flow into well for each cell, & add into matrix coefficients

      DO 100 ICL = ICWLBT, ICWLTP
         DZDUM = half * (CDELZ (ICL) + CDELZ (ICL + 1) )
         PDUM = MIN (DZDUM, MAX (CPSI (ICL), ZERO) )

         QDUM = CQWIN * (RKZDUM (ICL) / RKZTOT) * (PDUM / DZDUM)
         CQWI (ICL) = QDUM / CA0

         CR (ICL) = QDUM + CR (ICL)


100   END DO
   END SUBROUTINE VSWELL
END MODULE VSmod
