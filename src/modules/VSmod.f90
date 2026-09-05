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
!> | 2026-03-26 | SB | 4.6 | Made the VSS arrays allocatable via `INITIALISE_AL_C2` (see [[vsin]]). |
!> | 2026-04-03 | SvB | 4.6 | AI-assisted fixes and modernisation pass (formatting, count-prefixed locals, minor corrections). |
!> | 2026-04-05 | SvB | 4.6 | Removed the `ALINIT` helper; zero-initialisation calls replaced with Fortran 90 array-slice assignment. |
!> | 2026-04-06/07 | SvB | 4.6 | Expunged `GOTO`-based control flow in favour of `DO WHILE`/`CYCLE`/`EXIT` constructs; converted the `FNCELL` statement function to a contained function; removed a redundant array-section copy into `TRIDAG` (relies on sequence association); further modernisation and removal of obsolescent features. |
!> | 2026-04-10 | SvB | 4.6 | Fixed the `VSSOIL` saturation-curve initialisation: `VSPTHE(3,IS)` is now computed from the DSATG recursion instead of being copied from `VSPTHE(4,IS)`/`VSPOR`. |
!> | 2026-04-13 | SvB | 4.6 | Removed remaining labelled `DO` loops. |
!> | 2026-05-03 | SvB | 4.6 | Moved several large `VSREAD` work arrays (`IVSDUM`, `IVSCAT`, `ISDUM`, `RVSDUM`, `RSDUM`, `BDONE`) from routine-local (stack) storage into allocatable module state, allocated once by [[initialise_vsread_buffers]], to fix a stack-related crash. |
!> @endhistory
MODULE VSmod
   USE SGLOBAL
   USE mod_load_filedata, ONLY: ALSPRD, ALREAD

   USE MOD_PARAMETERS, ONLY: I_P
   USE MOD_ERROR, ONLY: errstat_alloc, RAISE_ERROR, ERRLVL_fatal, ERRLVL_error, ERRLVL_warn, FID_logfile, ERR_STOP

!USE SGLOBAL,  ONLY :
   USE AL_G, ONLY: ICMREF, NX, NY, ICMXY, NGDBGN
   USE AL_C, ONLY: BHB, BFB, bexbk, DTUZ, deltaz, dummy, DHF, ESOILA, ERUZ, EEVAP, &
                   FHBED, ISORT, jvsacn, JVSDEL, idum, icmbk, LFB, LHB, LINKNS, lgb, &
                   NWELBT, NWELTP, NVSSPC, NVSWLI, NTSOIL, nhbed, NVC, NRD, nlyrbt, NVSWLT, NVSSPT, NBFACE, NS, nlyr, &
                   PNETTO, QVSSPR, QVSBF, QH, QVSWEL, QBKF, QBKB, QVSV, QVSWLI, QVSH, QBKI, &
                   tih, UZNEXT, &
                   vsd, VSI, VSPSI, VSTHE, VSPOR, WLD, ZVSPSL, zlyrbt, zvsnod, zbeff, INITIALISE_AL_C, INITIALISE_AL_C2, TIH
   USE AL_D, ONLY: TTH
!USE VSINIT_INC
!USE VSCOM1_INC
!USE VSSOIL_INC
   USE UTILSMOD, ONLY: TRIDAG, FINPUT, HINPUT, DCOPY
   USE OCmod2, ONLY: GETHRF
   IMPLICIT NONE
! Saved legacy state moved here for AD/current builds.
   INTEGER :: ICSOILsv(LLEE, NELEE) !! Cached VSS soil type by cell and element.
   INTEGER :: JCBCsv(0:5, NELEE)    !! Cached boundary-condition type/category metadata by face and element.
   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: VSAIJsv !! Cached lateral face area/conductance terms.

   ! Read-buffer arrays for VSREAD, moved to allocatable module state (was
   ! routine-local) to avoid a stack-related crash; see initialise_vsread_buffers.
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: IVSDUM_VSREAD !! `VSREAD` work buffer: per-category layer soil-type codes.
   INTEGER, DIMENSION(:), ALLOCATABLE :: IVSCAT_VSREAD   !! `VSREAD` work buffer: layer category selected by each element.
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: ISDUM_VSREAD  !! `VSREAD` work buffer: integer fields read from `VS05`.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RVSDUM_VSREAD !! `VSREAD` work buffer: per-category layer boundary depths.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RSDUM_VSREAD  !! `VSREAD` work buffer: real-valued fields read from `VS05`.
   LOGICAL, DIMENSION(:), ALLOCATABLE :: BDONE_VSREAD    !! `VSREAD` work buffer: per-element layer-data-assigned flag.

   DOUBLEPRECISION :: WLLAST = zero        !! Previous well-input record time.
   DOUBLEPRECISION :: WLTIME = zero        !! Current/next well-input record time.
   DOUBLEPRECISION :: RWELIN(NVSEE) = zero !! Current well abstraction input values.
   DOUBLEPRECISION :: RLFLST = zero        !! Previous lateral-flow boundary record time.
   DOUBLEPRECISION :: RLFTIM = zero        !! Current/next lateral-flow boundary record time.
   DOUBLEPRECISION :: RLFPRV(NVSEE) = zero !! Previous lateral-flow boundary values.
   DOUBLEPRECISION :: RLHLST = zero        !! Previous lateral-head boundary record time.
   DOUBLEPRECISION :: RLHTIM = zero        !! Current/next lateral-head boundary record time.
   DOUBLEPRECISION :: RLHPRV(NVSEE) = zero !! Previous lateral-head boundary values.
   DOUBLEPRECISION :: RLHNXT(NVSEE) = zero !! Next lateral-head boundary values.
   DOUBLEPRECISION :: RLGLST = zero        !! Previous lateral-gradient boundary record time.
   DOUBLEPRECISION :: RLGTIM = zero        !! Current/next lateral-gradient boundary record time.
   DOUBLEPRECISION :: RLGPRV(NVSEE) = zero !! Previous lateral-gradient boundary values.
   DOUBLEPRECISION :: RLGNXT(NVSEE) = zero !! Next lateral-gradient boundary values.
   DOUBLEPRECISION :: RBFLST = zero        !! Previous base-flow boundary record time.
   DOUBLEPRECISION :: RBFTIM = zero        !! Current/next base-flow boundary record time.
   DOUBLEPRECISION :: RBFPRV(NVSEE) = zero !! Previous base-flow boundary values.
   DOUBLEPRECISION :: RBHLST = zero        !! Previous base-head boundary record time.
   DOUBLEPRECISION :: RBHTIM = zero        !! Current/next base-head boundary record time.
   DOUBLEPRECISION :: RBHPRV(NVSEE) = zero !! Previous base-head boundary values.
   DOUBLEPRECISION :: RBHNXT(NVSEE) = zero !! Next base-head boundary values.
   DOUBLEPRECISION :: RLFDUM(NVSEE) = zero !! Lateral-flow interpolation workspace.
   DOUBLEPRECISION :: RLHDUM(NVSEE) = zero !! Lateral-head interpolation workspace.
   DOUBLEPRECISION :: RLGDUM(NVSEE) = zero !! Lateral-gradient interpolation workspace.
   LOGICAL :: FIRSTvssim = .TRUE.          !! True until `VSSIM` has cached column metadata.
   integer, parameter :: errcntallowed = 1000 !! Maximum repeated VSS convergence warnings.

! Legacy VSCOM1.INC global VSS variables retained as module state.
!USE SGLOBAL, ONLY : NELEE, NLFEE, NLYREE, NVSEE, LLEE, NSEE
!IMPLICIT NONE
   LOGICAL :: BLOWP  !! Lower-boundary output print-control flag retained from legacy VSCOM1 state.
   LOGICAL :: BHELEV !! True when lateral boundary head inputs are elevations; false when they are depths below ground.

!COMMON / VSC1LI / BLOWP, BHELEV
! integer variables, initialisation
   INTEGER :: NCSZON                  !! Number of extra cells used to represent the soil-zone depth increments.
   INTEGER :: NCRBED                  !! Number of extra cells used to represent river-bed depth increments.
   INTEGER :: JVSALN(NELEE, NLYREE, 4)  !! Aquifer-layer connectivity ranges packed as `NLYREE+1` multiples.
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
   INTEGER :: NVSLFL(NLYREE, NVSEE)    !! Selected model layers for lateral-flow categories.
   INTEGER :: NVSLFN(NVSEE)           !! Number of selected lateral-flow layers per category; zero means whole column.
   INTEGER :: NVSLHT                  !! Expanded count of lateral-head boundary values after selected-layer categories.
   INTEGER :: NVSLHL(NLYREE, NVSEE)    !! Selected model layers for lateral-head categories.
   INTEGER :: NVSLHN(NVSEE)           !! Number of selected lateral-head layers per category; zero means whole column.
   INTEGER :: NVSLGT                  !! Expanded count of lateral-gradient boundary values after selected-layer categories.
   INTEGER :: NVSLGL(NLYREE, NVSEE)    !! Selected model layers for lateral-gradient categories.
   INTEGER :: NVSLGN(NVSEE)           !! Number of selected lateral-gradient layers per category; zero means whole column.

!COMMON / VSC1II / NCSZON, NCRBED, JVSALN, ISRBED, NVSWL, NVSSP, &
   !NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD, NVSWLC, NLBTYP, NLBCAT, &
   !NBBTYP, NBBCAT, NVSLFT, NVSLFL, NVSLFN, NVSLHT, NVSLHL, NVSLHN, &
   !NVSLGT, NVSLGL, NVSLGN
! integer variables, time-varying
   INTEGER :: IVSSTO(LLEE, NELEE) !! Stored soil lookup-table interval by VSS cell and element.

!COMMON / VSC1IT / IVSSTO
! floating-point variables and arrays, initialisation
   DOUBLEPRECISION :: DCSZON(LLEE)  !! Soil-zone cell-depth increments, ordered from the ground surface downward.
   DOUBLEPRECISION :: DCRBED(LLEE)  !! River-bed cell-depth increments, ordered from the bed surface downward.
   DOUBLEPRECISION :: DCSTOT        !! Total configured soil-zone depth.
   DOUBLEPRECISION :: DCRTOT        !! Total configured river-bed depth.
   DOUBLEPRECISION :: VSZMIN        !! Minimum VSS cell thickness.
   DOUBLEPRECISION :: VSZMAX        !! Maximum VSS cell thickness, stored with the legacy small tolerance.
   DOUBLEPRECISION :: VSK3D(NSEE, 3) !! Saturated hydraulic conductivity by soil type and x/y/z direction.
   DOUBLEPRECISION :: DRBED(NLFEE)  !! River-bed depth by link.
   DOUBLEPRECISION :: VSSPZ(NELEE)  !! Spring discharge elevation by element.
   DOUBLEPRECISION :: VSSPCO(NELEE) !! Spring conductance coefficient by element.
   DOUBLEPRECISION :: VSWV          !! Vertical hydraulic-conductivity w-mean control.
   DOUBLEPRECISION :: VSWL          !! Lateral hydraulic-conductivity w-mean control.

!COMMON / VSC1RI / DCSZON, DCRBED, DCSTOT, DCRTOT, VSZMIN, VSZMAX, &
   !VSK3D, DRBED, VSSPZ, VSSPCO, VSWV, VSWL
! floating-point arrays, time-varying
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: VSKR !! Relative hydraulic conductivity by VSS cell and element.
   DOUBLEPRECISION :: WLNOW(NVSEE)        !! Current well abstraction values.
   DOUBLEPRECISION :: RLFNOW(NLYREE, NVSEE) !! Current lateral-flow boundary values.
   DOUBLEPRECISION :: RLHNOW(NLYREE, NVSEE) !! Current lateral-head boundary values.
   DOUBLEPRECISION :: RLGNOW(NLYREE, NVSEE) !! Current lateral-gradient boundary values.
   DOUBLEPRECISION :: RBFNOW(NVSEE)       !! Current bottom-flow boundary values.
   DOUBLEPRECISION :: RBHNOW(NVSEE)       !! Current bottom-head boundary values.
!PRIVATE :: NELEE, NLFEE, NLYREE, NVSEE, LLEE, NSEE
!end MODULE vscom1_inc

! Legacy VSSOIL.INC soil-parameter tables retained as module state.
!USE SGLOBAL, ONLY : NSEE
!IMPLICIT NONE
   INTEGER :: NSOLEE !! Maximum number of generated soil lookup-table rows.

   PARAMETER(NSOLEE=200)
   DOUBLEPRECISION :: VSPPSI(NSOLEE)        !! Soil lookup pressure-head ordinates.
   DOUBLEPRECISION :: VSPTHE(NSOLEE, NSEE)   !! Soil lookup volumetric water content.
   DOUBLEPRECISION :: VSPKR(NSOLEE, NSEE)    !! Soil lookup relative hydraulic conductivity.
   DOUBLEPRECISION :: VSPETA(NSOLEE, NSEE)   !! Soil lookup storage coefficient.
   DOUBLEPRECISION :: VSPDTH(NSOLEE, NSEE)   !! Soil lookup derivative `d(theta)/d(psi)`.
   DOUBLEPRECISION :: VSPDKR(NSOLEE, NSEE)   !! Soil lookup derivative `d(K_r)/d(psi)`.
   DOUBLEPRECISION :: VSPDET(NSOLEE, NSEE)   !! Soil lookup derivative `d(eta)/d(psi)`.
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
   DOUBLEPRECISION :: TBPSI(NVSEE, NSEE) !! Tabulated pressure-head values by row and soil type.
   DOUBLEPRECISION :: TBTHE(NVSEE, NSEE) !! Tabulated water-content values by row and soil type.
   DOUBLEPRECISION :: TBKR(NVSEE, NSEE)  !! Tabulated relative-conductivity values by row and soil type.
   DOUBLEPRECISION :: TBTHEC(NVSEE, NSEE) !! Cubic-spline second derivatives for tabulated water content.
   DOUBLEPRECISION :: TBKRC(NVSEE, NSEE) !! Cubic-spline second derivatives for tabulated relative conductivity.
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
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
   SUBROUTINE initialise_vsmod()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "VSmod:initialise_vsmod"

      ALLOCATE (vsaijsv(4, top_cell_no, total_no_elements), STAT=ios)
      CALL errstat_alloc(ios, "vsaijsv", location)
      ALLOCATE (vskr(top_cell_no, total_no_elements), STAT=ios)
      CALL errstat_alloc(ios, "vskr", location)
   END SUBROUTINE initialise_vsmod

!> Allocates and zeroes the [[vsread]] category/layer work buffers.
!>
!> `IVSDUM_VSREAD`, `IVSCAT_VSREAD`, `ISDUM_VSREAD`, `RVSDUM_VSREAD`,
!> `RSDUM_VSREAD`, and `BDONE_VSREAD` were originally declared local to
!> [[vsread]]. They were moved into allocatable module state, allocated once
!> here, to avoid a stack-related crash from their combined size. [[vsread]]
!> calls this routine on every entry; the `ALLOCATED` guard makes repeated
!> calls safe, but the zeroing below always re-runs.
!>
!> @note
!> Unlike [[initialise_vsmod]], this routine is safe to call more than once:
!> allocation happens at most once, but the work arrays are always reset to
!> zero/false so each [[vsread]] call starts from a clean state.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
   SUBROUTINE initialise_vsread_buffers()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "VSmod:initialise_vsread_buffers"

      IF (.NOT. ALLOCATED(IVSDUM_VSREAD)) THEN
         ALLOCATE (IVSDUM_VSREAD(NELEE, NLYREE), STAT=ios)
         CALL errstat_alloc(ios, "IVSDUM_VSREAD", location)
         ALLOCATE (IVSCAT_VSREAD(NELEE), STAT=ios)
         CALL errstat_alloc(ios, "IVSCAT_VSREAD", location)
         ALLOCATE (ISDUM_VSREAD(NSEE, 8), STAT=ios)
         CALL errstat_alloc(ios, "ISDUM_VSREAD", location)
         ALLOCATE (RVSDUM_VSREAD(NELEE, NLYREE), STAT=ios)
         CALL errstat_alloc(ios, "RVSDUM_VSREAD", location)
         ALLOCATE (RSDUM_VSREAD(NSEE, 8), STAT=ios)
         CALL errstat_alloc(ios, "RSDUM_VSREAD", location)
         ALLOCATE (BDONE_VSREAD(NELEE), STAT=ios)
         CALL errstat_alloc(ios, "BDONE_VSREAD", location)
      END IF

      ! Initialise to default values
      IVSDUM_VSREAD = 0
      IVSCAT_VSREAD = 0
      ISDUM_VSREAD = 0
      RVSDUM_VSREAD = zero
      RSDUM_VSREAD = zero
      BDONE_VSREAD = .FALSE.

   END SUBROUTINE initialise_vsread_buffers

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
!> `LRENUM` and `NRENUM` are module-lifetime state (an initialised local array
!> and a `SAVE`d counter) and therefore retain state between calls. The routine
!> also calls [[initialise_vsmod]] and `INITIALISE_AL_C` after each
!> mesh-construction pass, before the final rebuild test can loop back for
!> another pass. This matches the original one-call setup assumption; repeated
!> calls, or a rebuild after allocation routines that do not tolerate repeated
!> allocation, are not safe.
!> @endnote
!>
!> @note
!> The local `nlyrmax` is declared but not used anywhere in the routine body.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written; version 4.0 completed 1996-01-17. |
!> | 1997-03-26 | RAH | 4.1 | Generic intrinsics; moved the `ERROR` calls to the end; new locals `ZAQTOP`, `ZLBOT`, `ZNODE`, `ICOL1`, `ICL0`, `NCL`; scrapped local `ZDUM1`; automatic type conversion; renamed locals `NDUM`, `ZDUM2`, `ZDUM3`, `Zasum`, `Zasum1`; replaced label/`GOTO 970` with `MAX(ZERO,VSZMIN)`; swapped the `DELTAZ`/`ZVSNOD` subscripts and moved `IBANK2`; moved a block-`IF` outside loop 974 and made it unconditional; put labels in order; defined `DELTAZ`/`ZVSNOD` for `ICL=1`; ran loop 1100 only when `ICL0>0`, called `ALINIT`, and removed loop 1170 (zeroing sub-cells); initialised `NRENUM` in `DATA`; started the `NLYRBT` search at `ICL0+1` without testing `DELTAZ>0`. |
!> | 1997-04-02 | RAH | 4.1 | Started loop 1600 at `ICOL1` instead of using a `GOTO`; rationalised the tests in loop 1500; swapped the `JVSACN`/`JVSDEL` subscripts and initialised them to `0` (previously `IUNDEF`); declared `NCELL`, `NACELL`, `ZDIFF`. |
!> | 1997-04-22 | RAH | 4.1 | Initialised `LRENUM` to `0` (previously `IUNDEF`) and tested `NCLYR<=0`. |
!> | 1997-04-23 | RAH | 4.1 | Started loop 1000 at `NLF+1` rather than testing for element type 3. |
!> | 1997-05-22 | RAH | 4.1 | Removed the "unfinished code" message and simplified a test. |
!> | 1997-05-23 | RAH | 4.1 | Set `ZVSNOD(1,IEL)` less than `ZLYRBT(IEL,1)`. |
!> | 1997-06-12 | RAH | 4.1 | Simplified loop 1120, cancelling the two preceding modifications. |
!> | 1997-07-18 | RAH | 4.1 | Renamed `ZLBOT` to `ZAQBOT`; put labels in order; used `IEL <= NLF` in place of `ITYPE == 3`; used `GOTO 1585` instead of an `ELSE`; fixed an error setting `ITOP`/`JTOP` for links (previously `LL-NCSZON`); used `NMOD` instead of `100` and merged the layer `IF`-blocks; rationalised the tests for skipping loop 1590; renamed `IALDUM`/`JALDUM` to `IRANGE`/`JRANGE`; scrapped inconsistency error 1049; fixed an aquifer-zone error (skip if either, not both). |
!> | 1997-07-28 | RAH | 4.1 | Scrapped local `IUNDEF` and arrays `LIDUM`/`LJDUM`; fixed errors in message 1037 (print `I`/`J`, not `LIDUM`/`LJDUM`, which were always 1) and at the top of the aquifer zone (`GOTO 1585`, not `1590`, for `BDONE`). |
!> | 1997-07-30 | RAH | 4.1 | Refined the split-cell treatment so splits do not straddle null cells; flagged warnings 1037 and 1053 once only; scrapped inconsistency error 1050; completed the `IEL` loop before renumbering instead of jumping out immediately. |
!> | 1997-08-01 | RAH | 4.1 | Completed the split-cell logic by spreading foregone splits (previously ill-specified); reduced the `MSG` size; simplified a test; stopped connecting the ends of river-bed cells. |
!> | 1997-08-06 | RAH | 4.1 | Added further entry conditions. |
!> | 1997-08-11 | RAH | 4.1 | Amended the `PAIR` logic to use `MISS`. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote the cell-renumbering outer loop, layer-matching search, and split-cell pairing loop from labelled `GOTO`s to `DO`/`DO WHILE` constructs with `CYCLE`/`EXIT`; replaced `CALL ALINIT` zero-initialisation with Fortran 90 array-slice assignment; converted the obsolete `FNCELL` statement function (never actually defined as a callable in the pre-modernisation source) into the contained function below; replaced the non-standard `IDIMJE` intrinsic with an equivalent `MAX(0, ...)` expression. All of these are direct control-flow/style translations with the same per-cell arithmetic. |
!> @endhistory
   SUBROUTINE VSCONC()

      ! Assumed external module dependencies providing global variables:
      ! NELEE, NLYREE, LLEE, NLFEE, total_no_links, total_no_elements,
      ! top_cell_no, VSZMIN, VSZMAX, ZERO, half, DCSTOT, DCSZON, NCSZON,
      ! DCRBED, NCRBED, ZGRUND, ZLYRBT, DELTAZ, ZVSNOD, JVSACN, JVSDEL,
      ! JVSALN, NHBED, FHBED, NLYR, NLYRBT, ICMREF, ICMBK, ZBEFF, DCRTOT,
      ! INITIALISE_VSMOD, INITIALISE_AL_C, ALSPRD, ERROR, ERRLVL_fatal, ERRLVL_warn, FID_logfile

      IMPLICIT NONE

      ! Locals
      INTEGER, PARAMETER :: JVSDUM = NELEE*NLYREE
      INTEGER :: NMOD
      INTEGER :: I, IRANGE, IBOT, IBOTL, ICL, IEL, IFA, ILYR, ITOP
      INTEGER :: J, JRANGE, JBOT, JBOTL, JCL, JEL, JFA, JLYR, JTOP
      INTEGER :: IDEL, IDEL0, IL, ILMAX, ILMIN, NITOT, NIMIN
      INTEGER :: JDEL, JDEL0, JL, JLMAX, JLMIN, NJTOT, NJMIN
      INTEGER :: IAQTOP, IBANK2, IBK, ICL0, ICL1, ICOL1, ILINK, ITYPE
      INTEGER :: DEL, JDIF, K, K2, K20, K2MOD, LCON, LTOP
      INTEGER :: NACELL, NCELL, NCL, NCLYR, NDUM, NEXTRA, NODD, NUM2
      INTEGER :: NIDUM(LLEE), NJDUM(LLEE), MAX_BOT_TOP
      DOUBLE PRECISION :: DZLYR, ZCBOT, ZDEPTH, ZBDBOT, ZCTOP, ZDUM
      DOUBLE PRECISION :: ZAQBOT, ZSZBOT, ZDIFF, ZLBOT, ZNODE
      LOGICAL :: BRENUM, BWARN, MISS, PAIR, BDONE(NELEE, 4)
      CHARACTER(LEN=57) :: MSG
      INTEGER :: nlyrmax

      ! Modern Initialization replacing DATA blocks
      INTEGER :: LRENUM(NELEE, NLYREE) = 0
      INTEGER, SAVE :: NRENUM = 0

      !----------------------------------------------------------------------*

      NMOD = NLYREE + 1

      renumbering_loop: DO
         NRENUM = NRENUM + 1

         ! Safe inline error trap replaces GOTO 8048
         IF (NRENUM > NELEE) THEN
            CALL RAISE_ERROR(ERRLVL_fatal, 1048, FID_logfile, 0, 0, 'Attempts to renumber cells have failed.')
            RETURN
         END IF

         BWARN = (NRENUM == NELEE)
         BRENUM = .FALSE.

         ! Set initial indices, dimensions & positions of cells
         !______________________________________________________*
         top_cell_no = 0

         element_loop: DO IEL = total_no_links + 1, total_no_elements
            ITYPE = ICMREF(IEL, 1)

            ! * process only grid and bank-1 elements here
            IF (ITYPE == 2) CYCLE element_loop

            ! --- loop over layers in aquifer zone (start from bottom of column)
            ZSZBOT = ZGRUND(IEL) - DCSTOT
            ICL = 1
            DELTAZ(ICL, IEL) = ZERO
            ZVSNOD(ICL, IEL) = ZERO

            layer_loop: DO ILYR = 1, NLYR(IEL)
               ! * divide each layer into equal sized cells
               ZLBOT = ZLYRBT(IEL, ILYR)
               DZLYR = MIN(ZLYRBT(IEL, ILYR + 1), ZSZBOT) - ZLBOT

               ! skip if layer is thinner than minimum cell size
               IF (DZLYR < VSZMIN) CYCLE layer_loop

               ! if no other plan make cells as large as poss but < VSZMAX
               NCLYR = LRENUM(IEL, ILYR)
               IF (NCLYR <= 0) NCLYR = MAX(1, INT(DZLYR/VSZMAX) + 1)

               ZDEPTH = DZLYR/DBLE(NCLYR)

               DO I = 1, NCLYR
                  ICL = ICL + 1
                  DELTAZ(ICL, IEL) = ZDEPTH
                  ZVSNOD(ICL, IEL) = ZDEPTH*(DBLE(I) - half) + ZLBOT
               END DO
            END DO layer_loop

            ! --- set up data for soil zone
            ZAQBOT = ZLYRBT(IEL, 1)
            ZCBOT = ZSZBOT

            DO I = NCSZON, 1, -1
               ZDEPTH = DCSZON(I)
               ZNODE = ZCBOT + ZDEPTH*half
               IF (ZNODE > ZAQBOT) THEN
                  ICL = ICL + 1
                  DELTAZ(ICL, IEL) = ZDEPTH
                  ZVSNOD(ICL, IEL) = ZNODE
               END IF
               ZCBOT = ZCBOT + ZDEPTH
            END DO

            ! --- update LL & store number of cells for this column
            top_cell_no = MAX(top_cell_no, ICL)
            IDUM(IEL) = ICL

            ! --- process link and opposite bank elements, if IEL is bank type 1
            IF (ITYPE /= 1) CYCLE element_loop

            ! * set up link cells up to bottom of link bed
            ILINK = ICMREF(IEL, 4)
            ZBDBOT = ZBEFF(ILINK) - DCRTOT
            ZCBOT = ZLYRBT(IEL, 1)

            link_cells: DO ICL1 = 1, ICL
               ZDEPTH = DELTAZ(ICL1, IEL)
               ZCTOP = ZCBOT + ZDEPTH

               IF (ZCTOP > ZBDBOT) EXIT link_cells

               DELTAZ(ICL1, ILINK) = ZDEPTH
               ZVSNOD(ICL1, ILINK) = ZVSNOD(ICL1, IEL)
               ZCBOT = ZCTOP
            END DO link_cells

            ! cell just below link bed: smaller than bank, unless ...
            ZDEPTH = ZBDBOT - ZCBOT
            IF (ZDEPTH < VSZMIN) THEN
               ! ... remainder is small: add it to the cell below
               ICL1 = ICL1 - 1
               ZDEPTH = ZDEPTH + DELTAZ(ICL1, ILINK)
            END IF

            DELTAZ(ICL1, ILINK) = ZDEPTH
            ZVSNOD(ICL1, ILINK) = ZBDBOT - half*ZDEPTH

            ! set up link bed cells
            ZCBOT = ZBDBOT
            DO I = NCRBED, 1, -1
               ZDEPTH = DCRBED(I)
               ICL1 = ICL1 + 1
               DELTAZ(ICL1, ILINK) = ZDEPTH
               ZVSNOD(ICL1, ILINK) = ZCBOT + ZDEPTH*half
               ZCBOT = ZCBOT + ZDEPTH
            END DO

            ! update LL & store number of cells for the link
            top_cell_no = MAX(top_cell_no, ICL1)
            IDUM(ILINK) = ICL1

            ! set up opposite bank cells
            IBANK2 = ICMBK(ILINK, 2)

            ! Exploit F90 array slicing
            DELTAZ(1:ICL, IBANK2) = DELTAZ(1:ICL, IEL)
            ZVSNOD(1:ICL, IBANK2) = ZVSNOD(1:ICL, IEL)

            IDUM(IBANK2) = ICL

         END DO element_loop

         ! Renumber cells & set up NLYRBT
         !____________________________________________________________________*
         IF (BEXBK) THEN
            ICOL1 = 1
         ELSE
            ICOL1 = total_no_links + 1
            NLYRBT(1:total_no_links, 1) = top_cell_no
         END IF

         ! --- loop over column elements
         DO IEL = ICOL1, total_no_elements
            NCL = IDUM(IEL)
            ICL0 = top_cell_no - NCL

            IF (ICL0 > 0) THEN
               DELTAZ(ICL0 + 1:ICL0 + NCL, IEL) = DELTAZ(1:NCL, IEL)
               ZVSNOD(ICL0 + 1:ICL0 + NCL, IEL) = ZVSNOD(1:NCL, IEL)

               DELTAZ(1:ICL0, IEL) = ZERO
               ZVSNOD(1:ICL0, IEL) = ZERO
            END IF

            ICL0 = ICL0 + 1

            DO ILYR = 1, NLYR(IEL)
               search_icl: DO ICL = ICL0 + 1, top_cell_no
                  IF (ZVSNOD(ICL, IEL) > ZLYRBT(IEL, ILYR)) EXIT search_icl
               END DO search_icl

               NLYRBT(IEL, ILYR) = ICL
               ICL0 = ICL - 1
            END DO

            NLYRBT(IEL, ILYR) = top_cell_no + 1
         END DO

         CALL INITIALISE_VSMOD()
         CALL INITIALISE_AL_C()

         ! Set up cell connectivities (JVSACN, JVSDEL)
         !_____________________________________________*
         DO IEL = 1, total_no_elements
            IBOT = NLYRBT(IEL, 1)
            BDONE(IEL, 1:4) = .FALSE.
            JVSACN(1:4, IBOT:top_cell_no, IEL) = 0
            JVSDEL(1:4, IBOT:top_cell_no, IEL) = 0
         END DO

         LTOP = top_cell_no - NCRBED
         IAQTOP = top_cell_no - NCSZON

         face_setup_loop: DO IEL = ICOL1, total_no_elements
            ITYPE = ICMREF(IEL, 1)
            IBOT = NLYRBT(IEL, 1)

            IF (IEL <= total_no_links) THEN
               IBK = ICMBK(IEL, 1)
               ITOP = MIN(IAQTOP + IBOT - NLYRBT(IBK, 1), LTOP)
            ELSE
               ITOP = IAQTOP
            END IF

            face_loop: DO IFA = 1, 4
               JEL = ICMREF(IEL, IFA + 4)
               IF (JEL < ICOL1) CYCLE face_loop

               JFA = ICMREF(IEL, IFA + 8)
               IF (BDONE(JEL, JFA)) CYCLE face_loop

               JBOT = NLYRBT(JEL, 1)
               JDIF = JBOT - IBOT

               ! --- channel link-bank face
               IF (IEL <= total_no_links .AND. JEL > total_no_links) THEN
                  DO ICL = IBOT, LTOP
                     JCL = ICL + JDIF
                     JVSACN(IFA, ICL, IEL) = JCL
                     JVSACN(JFA, JCL, JEL) = ICL
                  END DO

                  BDONE(IEL, IFA) = .TRUE.
                  CYCLE face_loop
               END IF

               ! --- other elements
               IF (JEL <= total_no_links) THEN
                  IBK = ICMBK(JEL, 1)
                  JTOP = MIN(IAQTOP + JBOT - NLYRBT(IBK, 1), LTOP)
                  LCON = LTOP
               ELSE
                  JTOP = IAQTOP
                  LCON = top_cell_no
               END IF

               ! ----- soil zone processing
               MAX_BOT_TOP = MAX(IBOT, JBOT)
               MAX_BOT_TOP = MAX(MAX_BOT_TOP, ITOP + 1, JTOP + 1)

               DO ICL = MAX_BOT_TOP, LCON
                  JCL = ICL
                  JVSACN(IFA, ICL, IEL) = JCL
                  JVSACN(JFA, JCL, JEL) = ICL
               END DO

               ! ----- aquifer zone processing
               ILYR = 1
               JLYR = 1

               layer_match_loop: DO WHILE (.TRUE.)
                  IBOTL = NLYRBT(IEL, ILYR)
                  JBOTL = NLYRBT(JEL, JLYR)

                  IF (IBOTL > ITOP .OR. JBOTL > JTOP) THEN
                     BDONE(IEL, IFA) = .TRUE.
                     CYCLE face_loop
                  END IF

                  JRANGE = JVSALN(IEL, ILYR, IFA)
                  IRANGE = JVSALN(JEL, JLYR, JFA)

                  IF (JRANGE == 0) THEN
                     ILYR = ILYR + 1
                     CYCLE layer_match_loop
                  ELSE IF (IRANGE == 0) THEN
                     JLYR = JLYR + 1
                     CYCLE layer_match_loop
                  END IF

                  ILMIN = IRANGE/NMOD
                  ILMAX = MOD(IRANGE, NMOD)
                  JLMIN = JRANGE/NMOD
                  JLMAX = MOD(JRANGE, NMOD)

                  ! count cells in column IEL, & no. required in JEL
                  NITOT = 0
                  NJMIN = 0
                  NODD = 0

                  DO IL = ILMIN, ILMAX
                     NCELL = FNCELL(IL, IEL, ITOP)
                     IF (JVSALN(IEL, IL, IFA) /= 0) THEN
                        DO I = 0, NCELL - 1
                           NITOT = 1 + NITOT
                           NIDUM(NITOT) = I + NLYRBT(IEL, IL)
                        END DO
                        NCELL = NCELL - NODD
                        NJMIN = (NCELL + 1)/2 + NJMIN
                        NODD = MOD(NCELL, 2)
                     ELSE IF (NCELL > 0) THEN
                        NODD = 0
                     END IF
                  END DO
                  NIDUM(NITOT + 1) = 0

                  ! count cells in column JEL, & no. required in IEL
                  NJTOT = 0
                  NIMIN = 0
                  NODD = 0

                  DO JL = JLMIN, JLMAX
                     NCELL = FNCELL(JL, JEL, JTOP)
                     IF (JVSALN(JEL, JL, JFA) /= 0) THEN
                        DO J = 0, NCELL - 1
                           NJTOT = 1 + NJTOT
                           NJDUM(NJTOT) = J + NLYRBT(JEL, JL)
                        END DO
                        NCELL = NCELL - NODD
                        NIMIN = (NCELL + 1)/2 + NIMIN
                        NODD = MOD(NCELL, 2)
                     ELSE IF (NCELL > 0) THEN
                        NODD = 0
                     END IF
                  END DO
                  NJDUM(NJTOT + 1) = 0

                  ! Checking conditions and splitting cells
                  IF (NITOT == 0 .AND. NJTOT > 0) THEN
                     WRITE (MSG, 9200) JFA, JLYR
                     IF (NRENUM == 1) CALL RAISE_ERROR(ERRLVL_warn, 1053, FID_logfile, JEL, 0, MSG)

                  ELSE IF (NJTOT == 0 .AND. NITOT > 0) THEN
                     WRITE (MSG, 9200) IFA, ILYR
                     IF (NRENUM == 1) CALL RAISE_ERROR(ERRLVL_warn, 1053, FID_logfile, IEL, 0, MSG)

                  ELSE IF (NJTOT < NJMIN) THEN
                     BRENUM = .TRUE.
                     NEXTRA = 0
                     DO JL = JLMIN, JLMAX
                        IF (JVSALN(JEL, JL, JFA) /= 0) THEN
                           IF (BWARN) THEN
                              WRITE (MSG, 9300) JFA, JL
                              CALL RAISE_ERROR(ERRLVL_warn, 1037, FID_logfile, JEL, 0, MSG)
                           END IF
                           NCELL = FNCELL(JL, JEL, JTOP)
                           NDUM = NCELL*NJMIN + NEXTRA + NJTOT/2
                           LRENUM(JEL, JL) = NDUM/NJTOT
                           NEXTRA = MOD(NDUM, NJTOT) - NJTOT/2
                        END IF
                     END DO

                  ELSE IF (NITOT < NIMIN) THEN
                     BRENUM = .TRUE.
                     NEXTRA = 0
                     DO IL = ILMIN, ILMAX
                        IF (JVSALN(IEL, IL, IFA) /= 0) THEN
                           IF (BWARN) THEN
                              WRITE (MSG, 9300) IFA, IL
                              CALL RAISE_ERROR(ERRLVL_warn, 1037, FID_logfile, IEL, 0, MSG)
                           END IF
                           NCELL = FNCELL(IL, IEL, ITOP)
                           NDUM = NCELL*NIMIN + NEXTRA + NITOT/2
                           LRENUM(IEL, IL) = NDUM/NITOT
                           NEXTRA = MOD(NDUM, NITOT) - NITOT/2
                        END IF
                     END DO

                  ELSE
                     ! how many splits possible, & how many to forego
                     IF (NITOT >= NJTOT) THEN
                        IDEL0 = 1
                        NUM2 = NITOT - NJMIN
                        NEXTRA = NJTOT - NJMIN
                     ELSE
                        IDEL0 = 0
                        NUM2 = NJTOT - NIMIN
                        NEXTRA = NITOT - NIMIN
                     END IF
                     JDEL0 = 1 - IDEL0

                     CALL ALSPRD(NEXTRA, NUM2, K20, K2MOD)

                     MISS = .FALSE.
                     K2 = -K20
                     I = 1
                     J = 1

                     pair_search: DO WHILE (I <= NITOT .AND. J <= NJTOT)
                        PAIR = (NIDUM(I + IDEL0) == NIDUM(I) + 1)
                        PAIR = (NJDUM(J + JDEL0) == NJDUM(J) + 1) .OR. PAIR
                        PAIR = .NOT. MISS .AND. PAIR

                        IF (PAIR) THEN
                           K2 = K2 + 1
                           MISS = (K2 >= 0 .AND. MOD(K2, K2MOD) == 0)
                           MISS = (K2 <= (NEXTRA - 1)*K2MOD .AND. MISS)
                           PAIR = .NOT. MISS
                        ELSE
                           MISS = .FALSE.
                        END IF

                        DEL = 0
                        IF (PAIR) DEL = 1

                        IDEL = IDEL0*DEL
                        JDEL = JDEL0*DEL

                        DO K = 0, DEL
                           ICL = NIDUM(I)
                           JCL = NJDUM(J)
                           IF (IDEL >= K) JVSACN(IFA, ICL, IEL) = JCL
                           IF (JDEL >= K) JVSACN(JFA, JCL, JEL) = ICL
                           JVSDEL(IFA, ICL, IEL) = IDEL*(1 - 2*K)
                           JVSDEL(JFA, JCL, JEL) = JDEL*(1 - 2*K)

                           ! Replaced non-standard IDIMJE with standard MAX implementation
                           I = I + MAX(0, IDEL - K)
                           J = J + MAX(0, JDEL - K)
                        END DO

                        I = I + 1
                        J = J + 1
                     END DO pair_search

                  END IF

                  ! move on to next layers
                  ILYR = ILMAX + 1
                  JLYR = JLMAX + 1

               END DO layer_match_loop

            END DO face_loop
         END DO face_setup_loop

         ! Repeat the whole thing if BRENUM was flagged
         IF (.NOT. BRENUM) EXIT renumbering_loop

      END DO renumbering_loop

      ! Finish off
      !____________*
      WRITE (FID_logfile, 9000) top_cell_no

      finish_loop: DO IEL = ICOL1, total_no_links
         IBK = ICMBK(IEL, 1)
         NACELL = LTOP + NLYRBT(IBK, 1) - NLYRBT(IEL, 1)
         ZDUM = DELTAZ(NACELL, IBK)
         ZDIFF = ZDUM - DELTAZ(LTOP, IEL)

         DELTAZ(LTOP, IEL) = ZDUM

         IF (NLYRBT(IEL, 1) <= LTOP - 1) THEN
            ZVSNOD(NLYRBT(IEL, 1):LTOP - 1, IEL) = ZVSNOD(NLYRBT(IEL, 1):LTOP - 1, IEL) - ZDIFF
         END IF

         ZVSNOD(ICL, IEL) = ZVSNOD(ICL, IEL) - ZDIFF*half

         IF (NLYR(IEL) >= 1) THEN
            ZLYRBT(IEL, 1:NLYR(IEL)) = ZLYRBT(IEL, 1:NLYR(IEL)) - ZDIFF
         END IF

         ! NB. banks 1 and 2 are identical
         NHBED(IEL, 1) = NACELL
         NHBED(IEL, 2) = NACELL
         FHBED(IEL, 1) = ZERO
         FHBED(IEL, 2) = ZERO

      END DO finish_loop

      RETURN

      ! FORMAT STATEMENTS
9000  FORMAT(/'Number of top cell in all columns (LL) = ', I3)
9200  FORMAT('Null cell connectivity being set up for face ', I1, ' layer ', I2)
9300  FORMAT('Not possible to connect all cells for face ', I1, ' layer ', I2)

   CONTAINS

      !> Returns the number of VSS cells spanned by one model layer interval.
      !>
      !> Replaces the obsolete Fortran statement function of the same name
      !> (the pre-modernisation source only commented out its definition, so
      !> `FNCELL` had never actually been a callable statement function; this
      !> contained function restores it with the same formula).
      PURE INTEGER FUNCTION FNCELL(IDX, ELEM, TOP)
         INTEGER, INTENT(IN) :: IDX  !! Model-layer index.
         INTEGER, INTENT(IN) :: ELEM !! Element number.
         INTEGER, INTENT(IN) :: TOP  !! Upper active cell bound used to clip the layer top.
         ! Calculates number of cells handling boundary constraints
         FNCELL = MAX(0, MIN(NLYRBT(ELEM, IDX + 1), TOP + 1) - NLYRBT(ELEM, IDX))
      END FUNCTION FNCELL

   END SUBROUTINE VSCONC

!> Builds the layer-to-layer lateral connectivity matrix.
!>
!> `VSCONL` builds the layer-level lateral connectivity used later by
!> [[vsconc]] to create cell-level links. It combines default aquifer-zone
!> matching with the manual `VS10`/`VS10a` user-defined aquifer connectivity
!> records (`IAQCON`).
!>
!> Required setup conditions are that the routine is called at most once per
!> run; `NAQCON` does not exceed `NVSEE` (the declared second dimension of
!> `IAQCON`); `1 <= NEL <= NELEE`, `NLF >= 0`, and `1 <= NLYR(1:NEL) <= NLYREE`;
!> and, for every element `e` from `ICOL1` to `NEL` and every face `1:4`, the
!> adjacent element `ea = ICMREF(e,4+face)` satisfies `ea <= NEL`, with
!> `1 <= ICMREF(e,8+face) <= 4` whenever `ea >= ICOL1`. If explicit banks are
!> not present, active VSS columns start at `NLF+1`; otherwise links and bank
!> elements are included from element 1.
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
!> assumption. Note also that `NAQCON` and `IAQCON` carry no `INTENT`
!> attribute in the current declaration (both are read-only in this routine).
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written; version 4.0 completed 1995-08-08. |
!> | 1997-05-08 | RAH | 4.1 | New locals `ICOL1`, `JTYPE`, `LYR`, `NLYRI`; simplified the null-connectivity test and amended its comment; generic intrinsics; removed the illegal `DATA` statement for `JVSALN`. |
!> | 1997-05-22 | RAH | 4.1 | Fixed an error setting `JLMAX` (used `JLMIN`, not `ILMIN`); scrapped the "null connectivity" message (error 1047). |
!> | 1997-06-30 | RAH | 4.1 | Moved `NAQCON`/`IAQCON` from `VSINIT.INC` into the argument list and swapped their indices (see [[vsread]]). |
!> | 1997-07-03 | RAH | 4.1 | Initialised `JVSALN` to `0` (previously `IUNDEF`) once and for all, but only for active elements and only up to `NLYR(iel)+1`. |
!> | 1997-07-10 | RAH | 4.1 | Redefined `IUNDEF` (previously `9999`); used `NMOD` instead of `100`; added detail to the `ERROR` message; put labels in order; rewrote loop 110 and fixed an error there (multiply `JLYR` by `NMOD+1` on first assignment); trapped invalid `JLYR`. |
!> | 1997-07-11 | RAH | 4.1 | New local `ZSMALL`; rewrote loop 200 and fixed errors there: set `JVSALN` on both sides for user-defined connectivity, corrected the expressions for `ILMIN` and similar, and generalised the default strategy (previously it checked/set a single embedded layer, missing some, else matched soils, else moved down a layer). Used `-1` for `IUNDEF`. |
!> | 1997-07-14 | RAH | 4.1 | Left bank-link faces at zero (never used anyway); moved the loop 200 criterion to the start (previously at the end); set `ISOILP=0` for `ILYR=NLYRI` and used `JSOILP`. |
!> | 1997-07-21 | RAH | 4.1 | Made `JVSALN` always either `0` or `NMOD*imin+imax`. |
!> | 1997-08-13 | RAH | 4.1 | Stopped giving up on a face when no match is found for `ILYR`/`JLYR`. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote labelled `GOTO` loops (default-connectivity initialisation, per-pair layer counters, the layer-matching search, and the connected-range walk) as `DO`/`DO WHILE` constructs with `CYCLE`/`EXIT`; no change to the matching arithmetic. |
!> @endhistory
   SUBROUTINE VSCONL(NAQCON, IAQCON)

! Input arguments

      INTEGER :: NAQCON       !! Number of user-defined aquifer connectivity records.
      INTEGER :: IAQCON(4, *)  !! User aquifer connectivity records: element/layer pairs for adjacent columns.
! Locals, etc
!INTRINSIC MAX, MIN, MOD
      INTEGER :: NMOD
      DOUBLEPRECISION ZSMALL
      PARAMETER(NMOD=NLYREE + 1, ZSMALL=1D-6)
      INTEGER :: I, J, ILYR, JLYR, IEL, JEL, IFA, JFA, NLYRI, NLYRJ
      INTEGER :: ILMIN, ILMAX, JLMIN, JLMAX, IRANGE, JRANGE, ISOIL, &
                 JSOIL
      INTEGER :: ISKIP, JSKIP, ISOILP, JSOILP, I1, I2, ICOL1, K, KEL
      INTEGER :: ILDUM(NLYREE), JLDUM(NLYREE)
      DOUBLEPRECISION ZSZBOT
      LOGICAL :: IOK, MOVEJ, TEST1, BDONE(NELEE)
      CHARACTER(LEN=132) :: MSG

      DATA BDONE/NELEE*.FALSE./
!----------------------------------------------------------------------*
      IF (BEXBK) THEN
         ICOL1 = 1
      ELSE
         ICOL1 = total_no_links + 1

      END IF
! ----- default is null connectivity
      DO IFA = 1, 4
         DO IEL = 1, total_no_elements
            DO ILYR = 1, NLYR(IEL) + 1
               JVSALN(IEL, ILYR, IFA) = 0
            END DO
         END DO
      END DO

! Main loop over (faces of) column elements
!___________________________________________*

      element_loop: DO IEL = ICOL1, total_no_elements
         NLYRI = NLYR(IEL)

         face_loop: DO IFA = 1, 4
            JEL = ICMREF(IEL, IFA + 4)
            ! null connectivity for boundary faces, branched channels & link flanks

            ! 1. Skip rest of loop if face already processed using CYCLE
            IF (JEL < ICOL1 .OR. (IEL <= total_no_links .AND. JEL > total_no_links)) CYCLE face_loop
            IF (BDONE(JEL)) CYCLE face_loop

            ! ... else process BOTH sides of face
            NLYRJ = NLYR(JEL)
            JFA = ICMREF(IEL, IFA + 8)

            ! 2. Replaced the 102 and 104 loops with array slicing
            ILDUM(1:NLYRI) = -1
            JLDUM(1:NLYRJ) = -1

            aqcon_loop: DO I = 1, NAQCON
               I1 = IAQCON(1, I)
               I2 = IAQCON(3, I)

               ! * does entry I belong to the current pair of elements?
               IF (IEL == I1 .AND. JEL == I2) THEN
                  K = 2
               ELSEIF (IEL == I2 .AND. JEL == I1) THEN
                  K = 4
               ELSE
                  ! 3. Replaced GOTO 110 with CYCLE
                  CYCLE aqcon_loop
               END IF

               ILYR = IAQCON(K, I)
               JLYR = IAQCON(6 - K, I)
               MSG = ' '

               IF (ILYR < 0 .OR. ILYR > NLYRI) THEN
                  ! * ILYR out of range
                  KEL = IEL
                  WRITE (MSG, 9381) ILYR, I, IEL, NLYRI
               ELSEIF (JLYR < 0 .OR. JLYR > NLYRJ) THEN
                  ! * JLYR out of range
                  KEL = JEL
                  WRITE (MSG, 9381) JLYR, I, JEL, NLYRJ
               ELSE
                  IF (ILYR > 0) THEN
                     JRANGE = ILDUM(ILYR)
                     TEST1 = JLYR == 0 .AND. JRANGE > 0
                     IF (JRANGE == 0 .OR. TEST1) THEN
                        ! * invalid
                        KEL = IEL
                        JRANGE = MOD(JLYR + JRANGE, NMOD)
                        WRITE (MSG, 9382) IEL, ILYR, JRANGE, JEL, I
                     ELSE
                        IF (JRANGE < 0) JRANGE = NMOD*NLYRJ + 1
                        JLMIN = MIN(JLYR, JRANGE/NMOD)
                        JLMAX = MAX(JLYR, MOD(JRANGE, NMOD))
                        ILDUM(ILYR) = NMOD*JLMIN + JLMAX
                     END IF
                  END IF

                  IF (JLYR > 0) THEN
                     IRANGE = JLDUM(JLYR)
                     TEST1 = ILYR == 0 .AND. IRANGE > 0
                     IF (IRANGE == 0 .OR. TEST1) THEN
                        ! * invalid
                        KEL = JEL
                        IRANGE = MOD(ILYR + IRANGE, NMOD)
                        WRITE (MSG, 9382) JEL, JLYR, IRANGE, IEL, I
                     ELSE
                        IF (IRANGE < 0) IRANGE = NMOD*NLYRI + 1
                        ILMIN = MIN(ILYR, IRANGE/NMOD)
                        ILMAX = MAX(ILYR, MOD(IRANGE, NMOD))
                        JLDUM(JLYR) = NMOD*ILMIN + ILMAX
                     END IF
                  END IF
               END IF

               ! * note: MSG for ILYR>0.and.JRANGE=0 is lost
               ! * if also JLYR>0.and.IRANGE=0
               IF (MSG /= ' ') THEN
                  CALL RAISE_ERROR(ERRLVL_error, 1038, FID_logfile, KEL, 0, MSG)
                  NVSERR = NVSERR + 1
               END IF
            END DO aqcon_loop

            ! set ILYR & JLYR to numbers of layers immediately below soil zone
            ZSZBOT = ZGRUND(IEL) - DCSTOT - ZSMALL

            ! 4. Replaced 120 and 140 loops with EXIT searches
            find_ilyr: DO ILYR = NLYRI, 1, -1
               IF (ZLYRBT(IEL, ILYR) < ZSZBOT) EXIT find_ilyr
            END DO find_ilyr

            ZSZBOT = ZGRUND(JEL) - DCSTOT - ZSMALL

            find_jlyr: DO JLYR = NLYRJ, 1, -1
               IF (ZLYRBT(JEL, JLYR) < ZSZBOT) EXIT find_jlyr
            END DO find_jlyr

            ! --- start of loop over layers (downwards from top of aquifer zone)
            ! 5. Replaced the massive 200 GOTO loop with a DO WHILE
            layer_matching: DO WHILE (ILYR > 0 .AND. JLYR > 0)
               ISOIL = NTSOIL(IEL, ILYR)
               JSOIL = NTSOIL(JEL, JLYR)
               JRANGE = ILDUM(ILYR)
               IRANGE = JLDUM(JLYR)

               IF (JRANGE == 0 .OR. (IRANGE > 0 .AND. JRANGE < 0)) THEN
                  ! * null
                  ILYR = ILYR - 1
               ELSEIF (IRANGE == 0 .OR. (JRANGE > 0 .AND. IRANGE < 0)) THEN
                  ! * null
                  JLYR = JLYR - 1
               ELSEIF (JRANGE > 0) THEN
                  ! * user-specified
                  JLMIN = JRANGE/NMOD
                  ILMIN = IRANGE/NMOD

                  ! 6. Replaced the 210 GOTO jump with another DO WHILE
                  ! * repeat until the whole connected range is processed
                  process_range: DO WHILE (ILMIN <= ILYR)
                     ILMAX = ILYR
                     DO ILYR = ILMAX, ILMIN, -1
                        JRANGE = ILDUM(ILYR)
                        JVSALN(IEL, ILYR, IFA) = MAX(0, JRANGE)
                        IF (JRANGE > 0) JLMIN = MIN(JLMIN, JRANGE/NMOD)
                     END DO

                     JLMAX = JLYR
                     DO JLYR = JLMAX, JLMIN, -1
                        IRANGE = JLDUM(JLYR)
                        JVSALN(JEL, JLYR, JFA) = MAX(0, IRANGE)
                        IF (IRANGE > 0) ILMIN = MIN(ILMIN, IRANGE/NMOD)
                     END DO
                  END DO process_range

               ELSEIF (ISOIL == JSOIL) THEN
                  ! * matching soils
                  JVSALN(IEL, ILYR, IFA) = JLYR*NMOD + JLYR
                  JVSALN(JEL, JLYR, JFA) = ILYR*NMOD + ILYR
                  ILYR = ILYR - 1
                  JLYR = JLYR - 1
               ELSE
                  ! * decide whether to move down column IEL or JEL:
                  ! * set type of soil above
                  ISOILP = 0
                  IF (ILYR < NLYRI) ISOILP = NTSOIL(IEL, ILYR + 1)
                  JSOILP = 0
                  IF (JLYR < NLYRJ) JSOILP = NTSOIL(JEL, JLYR + 1)

                  ! * look for next matching soil or user-specification
                  search_i: DO I = ILYR - 1, 1, -1
                     IF (NTSOIL(IEL, I) == JSOIL .OR. ILDUM(I) >= 0) EXIT
                  END DO search_i
                  ISKIP = ILYR - I

                  search_j: DO J = JLYR - 1, 1, -1
                     IF (NTSOIL(JEL, J) == ISOIL .OR. JLDUM(J) >= 0) EXIT
                  END DO search_j
                  JSKIP = JLYR - J

                  ! * choose smallest skip; or preserve soil continuity
                  MOVEJ = (ISOIL == ISOILP) .OR. (JSOIL /= JSOILP)
                  MOVEJ = (JSKIP < ISKIP) .OR. (JSKIP == ISKIP .AND. MOVEJ)
                  MOVEJ = (J > 0) .AND. MOVEJ

                  IF (MOVEJ) MOVEJ = JLDUM(J) < 0

                  ! * would there be any point moving down IEL?
                  IOK = I > 0
                  IF (IOK) IOK = ILDUM(I) < 0

                  ! * the choice is made
                  IF (MOVEJ) THEN
                     JLYR = J
                  ELSEIF (IOK) THEN
                     ILYR = I
                  ELSE
                     ILYR = ILYR - 1
                     JLYR = JLYR - 1
                  END IF
               END IF
            END DO layer_matching
            ! * process next pair of layers happens naturally by looping the WHILE

         END DO face_loop

         BDONE(IEL) = .TRUE.

      END DO element_loop

! Formats
!_________*
9381  FORMAT('Layer', I3, ' out of range, IAQCON entry', I3, &
       &      ' (element', I5, ' has', I3, ' layers)')

9382  FORMAT('Invalid null connection, element', I5, ':', &
       &      ' layer', I3, ' already connected to layer', I3, ', element', I5, &
       &      ' (see IAQCON entry', I3, ')')
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
!> count raises fatal error 1040 via the contained `ABORT_VSIN`.
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
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written; version 4.0 completed 1996-10-21. |
!> | 1997-01-22 | RAH | 4.1 | Removed long/leading comments and lower-case code; amended the externals list; extended the [[vsfunc]] argument list. |
!> | 1997-05-12 | RAH | 4.1 | Swapped the `IVSSTO`/`VSKR` indices and scrapped the local arrays `ICSDUM`/`CKRDUM`; likewise swapped `DELTAZ`, `ZVSNOD`, `VSPSI` and scrapped `CPSDUM`; scrapped the outputs `VSETAN`/`VSKRN`; rationalised and initialised loops 800 and 950; generic intrinsics; made more use of `ISTART`; put labels in order. |
!> | 1997-05-22 | RAH | 4.1 | Defaulted `NWELTP` to 1; used `GOTO` for errors and fixed an error in message 1041. |
!> | 1997-06-30 | RAH | 4.1 | Brought `NAQCON`/`IAQCON` from `VSINIT.INC`, swapped their indices, and passed them to [[vsread]] and [[vsconl]]; used format 9010 in place of 9020; replaced `NGDBGN` with `NLF+1`. |
!> | 2026-04-06/07 | SvB | 4.6 | Replaced the `GOTO 8900`-based fatal-error exit with the contained `ABORT_VSIN` routine, called from the two error sites; converted well/spring search loops and the `INITYP=3` layer loop from labelled `GOTO`s to named `DO`/`EXIT` constructs. Same error conditions and search results. |
!> @endhistory
   SUBROUTINE VSIN()

      ! Assumed external module dependencies providing global variables:
      ! LLEE, NVSEE, total_no_elements, total_no_links, top_cell_no, BEXBK,
      ! NVSERR, NVSWL, NVSLF, NVSLH, NVSBF, NVSBH, WLD, LFB, LHB, BFB, BHB,
      ! NWELBT, NWELTP, NVSSPC, NLYRBT, ZGRUND, NVSWLI, VSZWLB, VSZWLT, ZVSNOD,
      ! VSSPD, DELTAZ, INITYP, ZVSPSL, ZLYRBT, VSIPSD, VSI, VSPSI, NLYR, NTSOIL,
      ! IVSSTO, NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, VSPETA, VSPDKR, VSPDET,
      ! VSKR, ERRLVL_error, ERRLVL_fatal, FID_logfile, ERROR, INITIALISE_AL_C2, VSREAD, VSCONL,
      ! VSCONC, VSSOIL, VSFUNC, half, GTZERO, LTZERO

      IMPLICIT NONE

      ! Locals
      CHARACTER(132) :: MSG
      INTEGER :: IEL, ICL, ILYR, ICBOT, ICTOP, IW, IELIN, ISTART, NAQCON
      INTEGER :: IAQCON(4, NVSEE), ISDUM(LLEE)
      DOUBLE PRECISION :: DZ, RDUM, ZGI, ZMIN
      DOUBLE PRECISION :: CDUM1(LLEE), CDUM2(LLEE), CDUM3(LLEE), CDUM4(LLEE)

      !----------------------------------------------------------------------*

      ! top_cell_no is unknown at this point. But the code to caculate top_cell_no
      ! uses DELTAZ and ZVSNOD so these use llee
      CALL INITIALISE_AL_C2()

      WRITE (FID_logfile, 9010) 'Start', ' '

      NVSERR = 0
      IF (BEXBK) THEN
         ISTART = 1
      ELSE
         ISTART = total_no_links + 1
      END IF

      ! call VSREAD to read from input data file
      CALL VSREAD(NAQCON, IAQCON)

      ! Trap configuration errors immediately
      IF (NVSERR > 0) THEN
         CALL ABORT_VSIN()
         RETURN
      END IF

      ! read first lines of time-varying files
      IF (NVSWL > 0) READ (WLD, *)
      IF (NVSLF > 0) READ (LFB, *)
      IF (NVSLH > 0) READ (LHB, *)
      IF (NVSBF > 0) READ (BFB, *)
      IF (NVSBH > 0) READ (BHB, *)

      ! call VSCONL and VSCONC to set up connectivity arrays for layers and cells
      CALL VSCONL(NAQCON, IAQCON)
      CALL VSCONC()

      ! set up cell numbers for wells and springs
      ! set defaults
      DO IEL = 1, total_no_elements
         NWELBT(IEL) = 1
         NWELTP(IEL) = 1
         NVSSPC(IEL) = 0
      END DO

      element_loop_wells_springs: DO IEL = total_no_links + 1, total_no_elements
         ICBOT = NLYRBT(IEL, 1)
         ZGI = ZGRUND(IEL)
         IW = NVSWLI(IEL)

         IF (IW > 0) THEN
            ! Find bottom well node
            RDUM = ZGI - VSZWLB(IW)
            find_bottom: DO ICL = ICBOT, top_cell_no
               IF (RDUM <= ZVSNOD(ICL, IEL)) EXIT find_bottom
            END DO find_bottom
            NWELBT(IEL) = ICL

            ! Find top well node (looping backwards)
            RDUM = ZGI - VSZWLT(IW)
            find_top: DO ICL = top_cell_no, ICBOT, -1
               IF (RDUM >= ZVSNOD(ICL, IEL)) EXIT find_top
            END DO find_top
            NWELTP(IEL) = ICL
         END IF

         RDUM = VSSPD(IEL)

         IF (GTZERO(RDUM)) THEN
            RDUM = ZGI - RDUM

            ! Find specific node based on delta Z
            find_spc: DO ICL = ICBOT, top_cell_no
               DZ = ABS(ZVSNOD(ICL, IEL) - RDUM)
               IF (DZ <= half*DELTAZ(ICL, IEL)) EXIT find_spc
            END DO find_spc
            NVSSPC(IEL) = ICL
         END IF

      END DO element_loop_wells_springs

      ! call VSSOIL to set up soil property tables
      CALL VSSOIL()

      ! set up initial conditions (read from file unit VSI, if required)
      ! type 1 - uniform phreatic surface depth, equilibrium psi profile
      IF (INITYP == 1) THEN
         DO IEL = 1, total_no_elements
            ZVSPSL(IEL) = MAX(ZLYRBT(IEL, 1), ZGRUND(IEL) - VSIPSD)
         END DO

         ! type 2 - varying phreatic surface level, equilibrium psi profile
      ELSE IF (INITYP == 2) THEN
         READ (VSI, '(A)')
         READ (VSI, *) (ZVSPSL(IEL), IEL=ISTART, total_no_elements)

         ! type 3 - 3-dimensional field of psi values (+ init. psl for output)
      ELSE
         READ (VSI, '(A)')

         element_loop_vsi: DO IEL = ISTART, total_no_elements
            READ (VSI, *) IELIN

            IF (IELIN /= IEL) THEN
               NVSERR = NVSERR + 1
               WRITE (MSG, 9040) IEL
               CALL RAISE_ERROR(ERRLVL_error, 1041, FID_logfile, 0, 0, MSG)
               CALL ABORT_VSIN()
               RETURN
            END IF

            ICBOT = NLYRBT(IEL, 1)
            ICTOP = top_cell_no

            READ (VSI, *) VSPSI(ICBOT:ICTOP, IEL)

            ZMIN = ZVSNOD(ICBOT, IEL) - half*DELTAZ(ICBOT, IEL)

            search_loop: DO ICL = ICBOT, ICTOP
               IF (LTZERO(VSPSI(ICL, IEL))) EXIT search_loop
            END DO search_loop

            ICL = MAX(ICBOT, ICL - 1)
            ZVSPSL(IEL) = MAX(ZMIN, ZVSNOD(ICL, IEL) + VSPSI(ICL, IEL))

         END DO element_loop_vsi

      END IF

      ! set up equilibrium psi profile for types 1 or 2
      IF (INITYP == 1 .OR. INITYP == 2) THEN
         equilibrium_profile_loop: DO IEL = 1, total_no_elements
            DO ICL = NLYRBT(IEL, 1), top_cell_no
               VSPSI(ICL, IEL) = ZVSPSL(IEL) - ZVSNOD(ICL, IEL)
            END DO
         END DO equilibrium_profile_loop
      END IF

      ! set up initial relative conductivities for all elements
      init_cond_loop: DO IEL = ISTART, total_no_elements

         DO ILYR = 1, NLYR(IEL)
            DO ICL = NLYRBT(IEL, ILYR), NLYRBT(IEL, ILYR + 1) - 1
               ISDUM(ICL) = NTSOIL(IEL, ILYR)
               IVSSTO(ICL, IEL) = 0
            END DO
         END DO

         ICBOT = NLYRBT(IEL, 1)
         ICTOP = top_cell_no

         CALL VSFUNC(NVSSOL, NSOLEE, VSPPSI, VSPTHE, VSPKR, &
                     VSPETA, VSPDKR, VSPDET, IEL, ICBOT, ICTOP, ISDUM(ICBOT), &
                     VSPSI(ICBOT, IEL), IVSSTO(ICBOT, IEL), CDUM1, CDUM2, VSKR(ICBOT, IEL), &
                     CDUM3, CDUM4)

      END DO init_cond_loop

      WRITE (FID_logfile, 9010) 'End', '   '

      RETURN

      ! FORMAT STATEMENTS for the host subroutine
9010  FORMAT(/'!!', 78('#')/1X, A, ' of VSS data ', A, 60('#')/80('#'))
9040  FORMAT('Error reading VSS initial conditions for element ', I4, '.')

   CONTAINS

      !> Reports the accumulated VSS data-reading/initialisation error count
      !> and stops via fatal error 1040. Replaces the legacy `GOTO 8900` exit
      !> from [[vsin]].
      SUBROUTINE ABORT_VSIN()
         WRITE (MSG, 9030) NVSERR
         CALL RAISE_ERROR(ERRLVL_fatal, 1040, FID_logfile, 0, 0, MSG)

         ! Format statement scoped correctly to the internal subroutine
9030     FORMAT(I4, ' Errors have occurred in VSS data reading ', 'or initialisation.')
      END SUBROUTINE ABORT_VSIN

   END SUBROUTINE VSIN

!> Adds inter-column exchange coefficients to the column system.
!>
!> `VSINTC` assembles the base tridiagonal system for one VSS column before
!> [[vscolm]] adds upper, lower, well, spring, lateral-boundary, and
!> stream-aquifer terms. It combines storage, vertical inter-cell flow, internal
!> lateral exchange to already known neighbour heads, and existing source/sink
!> terms `CQ`.
!>
!> Required entry conditions are `1 <= ICBOT <= ICTOP <= LLEE` and `DT > 0`. In
!> addition, for every face `j` with `JELDUM(j) > 0` and `JCBC(j) /= 9`, and
!> every cell `i` with `JCACN(j,i) /= 0`, both `k = JCACN(j,i)` and
!> `k1 = k + JCDEL1(k,j)` must lie in `1:LLEE`.
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
!> `CQ` is already premultiplied by the cell volume factor in [[vssim]], as of
!> the 1997-05-14 change recorded below. This routine treats it as an assembled
!> residual/source term, not as a flux density to be scaled again.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-20 | GP | 4.0 | Written; version 4.0 completed 1995-06-22. |
!> | 1997-01-20 | RAH | 4.1 | Rewritten to use fewer operations and to stop overwriting the input arrays. |
!> | 1997-01-26 | RAH | 4.1 | Dispensed with the inputs `IEL`, `CB*P`, `CD*P`, `CDFM`, and `C*G`; passed data through arguments instead of `COMMON`. |
!> | 1997-02-03 | RAH | 4.1 | Replaced input `CV` with `CA0` and `CDELZ`. |
!> | 1997-02-10 | RAH | 4.1 | Made the input `SIGMA` a local. |
!> | 1997-05-14 | RAH | 4.1 | `CQ` is now pre-multiplied by `CA0*CDELZ` by the caller (see [[vssim]]); swapped the `JCACN` indices. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote the lateral-terms `GOTO`-skip logic as `CYCLE` on named loops; added the `PURE` attribute (the routine performs no I/O and modifies only its `INTENT(OUT)` dummy arguments). No change to the assembled coefficients. |
!> @endhistory
   PURE SUBROUTINE VSINTC(LLEE, ICBOT, ICTOP, JELDUM, JCBC, JCACN, &
                          JCDEL1, CA0, CDELZ, CZ, CZ1, DT, CETA, CDETA, CQ, CPSI, CPSIN, CF, &
                          CDF, CBETM, CDBETM, CDBTMM, CPSI1, CPSIN1, CGAM1, CGAM2, CDGAM1, &
                          CDGAM2, CA, CB, CC, CR, H)

      ! Assumed external module dependencies providing global variables:
      ! zero

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE                  !! Declared cell dimension for neighbour arrays.
      INTEGER, INTENT(IN) :: ICBOT                 !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                 !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: JELDUM(4)             !! Adjacent element id by face; values below 1 disable regular lateral coupling.
      INTEGER, INTENT(IN) :: JCBC(4)               !! Boundary type by face; type 9 is skipped here.
      INTEGER, INTENT(IN) :: JCACN(4, ICBOT:ICTOP)  !! Adjacent-cell index by face and active cell.
      INTEGER, INTENT(IN) :: JCDEL1(LLEE, 4)        !! Neighbour-column split offset used for second connected cells.
      DOUBLE PRECISION, INTENT(IN) :: CA0           !! Plan area of the current element.
      DOUBLE PRECISION, INTENT(IN) :: CZ1(LLEE, 4)   !! Adjacent-cell node elevations by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Active-cell thicknesses.
      DOUBLE PRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP) !! Active-cell node elevations.
      DOUBLE PRECISION, INTENT(IN) :: CETA(ICBOT:ICTOP) !! Storage coefficient by active cell.
      DOUBLE PRECISION, INTENT(IN) :: DT            !! Timestep length.
      DOUBLE PRECISION, INTENT(IN) :: CDETA(ICBOT:ICTOP) !! Derivative of storage coefficient by active cell.
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Current pressure heads.
      DOUBLE PRECISION, INTENT(IN) :: CPSIN(ICBOT:ICTOP) !! Previous-timestep pressure heads.
      DOUBLE PRECISION, INTENT(IN) :: CF(ICBOT:ICTOP) !! Internal conductance contribution to the diagonal.
      DOUBLE PRECISION, INTENT(IN) :: CDF(ICBOT:ICTOP) !! Derivative of `CF` with respect to pressure head.
      DOUBLE PRECISION, INTENT(IN) :: CQ(ICBOT:ICTOP) !! Assembled cell source/sink terms.
      DOUBLE PRECISION, INTENT(IN) :: CBETM(ICBOT:ICTOP + 1) !! Vertical inter-cell conductance below each active cell.
      DOUBLE PRECISION, INTENT(IN) :: CDBETM(ICBOT:ICTOP + 1) !! Derivative of `CBETM` with respect to the lower cell.
      DOUBLE PRECISION, INTENT(IN) :: CDBTMM(ICBOT:ICTOP + 1) !! Derivative of `CBETM` with respect to the upper cell.
      DOUBLE PRECISION, INTENT(IN) :: CPSI1(LLEE, 4) !! Adjacent current pressure heads by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CPSIN1(LLEE, 4) !! Adjacent previous-timestep pressure heads by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CGAM1(LLEE, 4) !! Primary lateral coupling conductance.
      DOUBLE PRECISION, INTENT(IN) :: CDGAM1(LLEE, 4) !! Derivative of `CGAM1` with respect to local pressure head.
      DOUBLE PRECISION, INTENT(IN) :: CDGAM2(LLEE, 4) !! Derivative of `CGAM2` with respect to local pressure head.
      DOUBLE PRECISION, INTENT(IN) :: CGAM2(LLEE, 4) !! Secondary split-cell lateral coupling conductance.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CA(ICBOT:ICTOP) !! Lower diagonal for the tridiagonal column system.
      DOUBLE PRECISION, INTENT(OUT) :: CB(ICBOT:ICTOP) !! Diagonal for the tridiagonal column system.
      DOUBLE PRECISION, INTENT(OUT) :: CC(ICBOT:ICTOP) !! Upper diagonal for the tridiagonal column system.
      DOUBLE PRECISION, INTENT(OUT) :: CR(ICBOT:ICTOP) !! Right-hand side for the tridiagonal column system.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(OUT) :: H(ICBOT - 1:ICTOP + 1) !! Workspace for effective hydraulic heads.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: SIGMA = 1.0D0, OMSIG = 1.0D0 - SIGMA
      INTEGER :: I, J, K, K1, P
      DOUBLE PRECISION :: CBETMI, CBETPI, CDBETP, CDBMMI, CDBTPP, CDFM, CDFP, CDG
      DOUBLE PRECISION :: CFI, CGI, DPSI, HI, HK, HK1, HM, HP, VODT

      !----------------------------------------------------------------------*

      ! Prepare effective hydraulic heads
      I = ICBOT - 1
      H(I) = zero

      DO I = ICBOT, ICTOP
         H(I) = SIGMA*CPSI(I) + OMSIG*CPSIN(I) + CZ(I)
      END DO

      I = ICTOP + 1
      H(I) = zero

      ! Set coefficients, omitting lateral terms
      DO I = ICBOT, ICTOP
         P = I + 1
         HM = H(I - 1)
         HI = H(I)
         HP = H(P)
         CFI = CF(I)
         CBETMI = CBETM(I)
         CBETPI = CBETM(P)
         CDBTPP = CDBETM(P)
         CDBMMI = CDBTMM(I)
         CDBETP = CDBTMM(P)
         CDFM = CDBMMI
         CDFP = CDBTPP
         VODT = CDELZ(I)*CA0/DT
         CGI = CETA(I)*VODT
         CDG = CDETA(I)*VODT
         DPSI = CPSI(I) - CPSIN(I)

         CA(I) = SIGMA*CBETMI - HI*CDFM + HM*CDBMMI
         CC(I) = SIGMA*CBETPI - HI*CDFP + HP*CDBTPP
         CB(I) = HM*CDBETM(I) - HI*CDF(I) + HP*CDBETP - &
                 (SIGMA*CFI + DPSI*CDG + CGI)
         CR(I) = -(HM*CBETMI - HI*CFI + HP*CBETPI - DPSI*CGI + CQ(I))
      END DO

      ! Add lateral terms
      face_loop: DO J = 1, 4

         IF (JELDUM(J) < 1 .OR. JCBC(J) == 9) CYCLE face_loop

         internal_cell_loop: DO I = ICBOT, ICTOP
            K = JCACN(J, I)
            IF (K == 0) CYCLE internal_cell_loop

            K1 = JCDEL1(K, J) + K
            HK = SIGMA*CPSI1(K, J) + OMSIG*CPSIN1(K, J) + CZ1(K, J)
            HK1 = SIGMA*CPSI1(K1, J) + OMSIG*CPSIN1(K1, J) + CZ1(K1, J)

            CB(I) = CB(I) + HK*CDGAM1(I, J) + HK1*CDGAM2(I, J)
            CR(I) = CR(I) - HK*CGAM1(I, J) - HK1*CGAM2(I, J)

         END DO internal_cell_loop

      END DO face_loop

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
!>
!> @note
!> None of this routine's dummy arguments carry an `INTENT` attribute in the
!> current declarations, unlike most other routines in this module.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written. |
!> | 1997-01-20 | RAH | 4.1 | Removed leading comments and lower-case code; combined `IF`-blocks; introduced the local `CQVDUM`. |
!> | 1997-01-31 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks; declared `CDQDUM` as `DBLE` rather than `DOUBLEPRECISION`. |
!> @endhistory
   SUBROUTINE VSLOWR(JCBC, CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, &
                     CKR, CDKR, CB, CR, CQV)
!
! Input arguments
      INTEGER :: JCBC           !! Bottom boundary type: 6 flow, 7 head, otherwise no-flow/free-drainage fallback.
      DOUBLEPRECISION :: CA0    !! Plan area of the current element.
      DOUBLEPRECISION :: CZ     !! Bottom-cell node elevation.
      DOUBLEPRECISION :: CDELZ  !! Bottom-cell thickness.
      DOUBLEPRECISION :: CKZS   !! Saturated vertical hydraulic conductivity for the bottom-cell soil.
      DOUBLEPRECISION :: CBF    !! Prescribed bottom-flow boundary value.
      DOUBLEPRECISION :: CBH    !! Prescribed bottom-head boundary value.
      DOUBLEPRECISION :: CPSI   !! Bottom-cell pressure head.
      DOUBLEPRECISION :: CKR    !! Bottom-cell relative hydraulic conductivity.
      DOUBLEPRECISION :: CDKR   !! Derivative of `CKR` with respect to pressure head.
!
! In+out arguments
      DOUBLEPRECISION :: CB  !! Bottom-cell matrix diagonal term.
      DOUBLEPRECISION :: CR  !! Bottom-cell right-hand side term.
!
! Output arguments
      DOUBLEPRECISION :: CQV   !! Bottom vertical boundary flux.
!
! Locals, etc
      DOUBLEPRECISION CDQDUM, CQVDUM, DH, KSODZ
!
!----------------------------------------------------------------------*
!
! column base flow (type 6)
      IF (JCBC .EQ. 6) THEN
         CQVDUM = CBF

         CDQDUM = zero
! column base head (type 7)
      ELSEIF (JCBC .EQ. 7) THEN
         DH = CBH - CZ - CPSI
         KSODZ = CKZS/(half*CDELZ)
         CQVDUM = KSODZ*CKR*DH

         CDQDUM = KSODZ*(CDKR*DH - CKR)
! no flow (970131: Check column base free drainage (type 8)!)
      ELSE
         CQVDUM = zero

         CDQDUM = zero

      END IF
      CQV = CQVDUM
      CB = CB + CA0*CDQDUM

      CR = CR - CA0*CQVDUM
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
!> | `VS01` | VSD title | Printed to `FID_logfile`. |
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
!> The category/layer work buffers (`IVSDUM_VSREAD`, `IVSCAT_VSREAD`,
!> `ISDUM_VSREAD`, `RVSDUM_VSREAD`, `RSDUM_VSREAD`, `BDONE_VSREAD`) are
!> allocated once in module state by [[initialise_vsread_buffers]], called at
!> the start of every `VSREAD` entry, rather than being routine-local arrays as
!> in the historical `.F`-era implementation; see the module-level history for
!> why.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written; version 4.0 completed 1996-01-31. |
!> | 1997-02-13 | RAH | 4.1 | Initialised `NLBTYP`, `NLBCAT`, `NVSWLC`, `NBBTYP`, and `NBBCAT`; reversed the `NVSLFL`, `NVSLHL`, and `NVSLGL` subscripts (see [[vssim]]). |
!> | 1997-05-22 | RAH | 4.1 | Initialised `NVSWLI`; fixed errors: used `TBKR` rather than `TBTHE` in loop 21 (`IVSFLG=2`), and added `-1` to `NVSLHT` and `NVSLGT`. |
!> | 1997-06-30 | RAH | 4.1 | Brought `NAQCON`/`IAQCON` from `VSINIT.INC` into the argument list and swapped their indices, fixing an error in the `ALREAD` call; restricted the `VS08b` `ALREAD` call to `NLF > 0`. |
!> | 1997-08-05 | RAH | 4.1 | Ensured `NLBCAT`, `NBBCAT`, and `NVSWLC` are all at least 1. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote the labelled `GOTO`-driven "find first free layer slot" and "count link-bed layers" searches (both duplicated for category and per-element data) as `DO WHILE` loops with `CYCLE`d element-skip logic. Same search results. |
!> | 2026-05-03 | SvB | 4.6 | Moved `IVSDUM`, `IVSCAT`, `ISDUM`, `RVSDUM`, `RSDUM`, and `BDONE` from routine-local arrays into allocatable module state (`*_VSREAD`), allocated by [[initialise_vsread_buffers]], to fix a stack-related crash from their combined size. |
!> @endhistory
   SUBROUTINE VSREAD(NAQCON, IAQCON)

      ! Assumed external module dependencies providing global variables:
      ! LLEE, NELEE, NLYREE, NSEE, NVSEE, total_no_elements, NVSWLI, NLBTYP,
      ! NBBTYP, NVSWLC, NLBCAT, NBBCAT, ALREAD, VSD, FID_logfile, IDUM, DUMMY, BFAST,
      ! BSOILP, BHELEV, NS, NCSZON, NCRBED, INITYP, VSIPSD, VSZMIN, VSZMAX,
      ! VSWV, VSWL, IVSFLG, IVSNTB, VSK3D, VSPOR, VSTRES, VSPSS, VSVGN, VSALPH,
      ! VSPPOR, ERROR, ERRLVL_fatal, TBPSI, TBTHE, TBKR, TBTHEC, TBKRC, zero, two, one,
      ! DCSZON, DCSTOT, DCRBED, DCRTOT, BEXBK, total_no_links, NX, NY, ICMXY,
      ! ICMREF, NLYR, NTSOIL, ZLYRBT, ZGRUND, ICMBK, ZBEFF, NGDBGN, ERRLVL_error,
      ! ISRBED, DRBED, NVSWL, NVSSP, NVSLF, NVSLH, NVSLG, NVSBF, NVSBH, NVSBD,
      ! NVSLFN, NVSLHN, NVSLGN, NVSLFT, NVSLHT, NVSLGT, NVSLFL, NVSLHL, NVSLGL,
      ! NVSWLT, VSZWLB, VSZWLT, NVSSPT, VSSPD, VSSPZ, VSSPCO

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(INOUT) :: NAQCON       !! Number of user-defined aquifer connectivity records read from `VS10`.
      INTEGER, INTENT(INOUT) :: IAQCON(4, NVSEE) !! User-defined aquifer connectivity records read from `VS10a`.

      ! Locals
      INTEGER :: I, I0, IBK, ICAT, IEL, ILYR, IS, ISP, IW, IWT, IX, IXY0, IY
      INTEGER :: ICOUNT, LCOUNT
      INTEGER :: NUM_CATEGORIES_TYPES, NELEM, NCOUNT, NDUM, NSP, NW
      INTEGER :: ILB, NLB, ITYP, NLDUM, ISDUM1, IDUM1(1)
      DOUBLE PRECISION :: DCSDUM(0:LLEE)
      DOUBLE PRECISION :: DCSNOD(LLEE), DCRDUM(0:LLEE), DCRNOD(LLEE), SIG, PDUM
      DOUBLE PRECISION :: XDUM(NVSEE), YDUM(NVSEE), Y2DUM(NVSEE), UDUM(NVSEE)
      CHARACTER(LEN=80)  :: CDUM
      CHARACTER(LEN=132) :: MSG

      !----------------------------------------------------------------------*
      ! Initialization

      CALL initialise_vsread_buffers()

      DO IEL = 1, total_no_elements
         NVSWLI(IEL) = 0
         NLBTYP(IEL) = 0
         NBBTYP(IEL) = 0
         NVSWLC(IEL) = 1
         NLBCAT(IEL) = 1
         NBBCAT(IEL) = 1
      END DO

      ! VS01 ----- main data file title
      CALL ALREAD(1, VSD, FID_logfile, ':VS01', 1, 1, 0, CDUM, IDUM, DUMMY)
      WRITE (FID_logfile, '(/, 1X, A, /)') TRIM(CDUM)

      ! VS02 ----- logical flags
      READ (VSD, '(A)') CDUM
      READ (VSD, *) BFAST, BSOILP, BHELEV

      ! VS03 ----- integer variables
      CALL ALREAD(2, VSD, FID_logfile, ':VS03', 4, 1, 0, CDUM, IDUM, DUMMY)
      NS = IDUM(1)
      NCSZON = IDUM(2)
      NCRBED = IDUM(3)
      INITYP = IDUM(4)

      ! VS04 ----- real variables
      CALL ALREAD(3, VSD, FID_logfile, ':VS04', 5, 1, 0, CDUM, IDUM, DUMMY)
      VSIPSD = DUMMY(1)
      VSZMIN = DUMMY(2)
      VSZMAX = DUMMY(3) + 1.0D-6
      VSWV = DUMMY(4)
      VSWL = DUMMY(5)

      ! VS05 ----- physical property data
      CALL ALREAD(7, VSD, FID_logfile, ':VS05', NSEE, 8, NS, CDUM, ISDUM_VSREAD, RSDUM_VSREAD)

      DO IS = 1, NS
         IVSFLG(IS) = ISDUM_VSREAD(IS, 2)
         IVSNTB(IS) = ISDUM_VSREAD(IS, 3)
         VSK3D(IS, 1) = RSDUM_VSREAD(IS, 1)/(3600.0D0*24.0D0)
         VSK3D(IS, 2) = RSDUM_VSREAD(IS, 2)/(3600.0D0*24.0D0)
         VSK3D(IS, 3) = RSDUM_VSREAD(IS, 3)/(3600.0D0*24.0D0)
         VSPOR(IS) = RSDUM_VSREAD(IS, 4)
         VSTRES(IS) = RSDUM_VSREAD(IS, 5)
         VSPSS(IS) = RSDUM_VSREAD(IS, 6)
         VSVGN(IS) = RSDUM_VSREAD(IS, 7)
         VSALPH(IS) = RSDUM_VSREAD(IS, 8)
         VSPPOR(IS) = VSPOR(IS)
      END DO

      ! VS05a ---- soil characteristic function tabulated data
      DO IS = 1, NS
         IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
            READ (VSD, *) ISDUM1
            IF (IS /= ISDUM1) THEN
               WRITE (MSG, 9030) IS
               CALL RAISE_ERROR(ERRLVL_fatal, 1051, FID_logfile, 0, 0, MSG)
            END IF

            DO I = 1, IVSNTB(IS)
               READ (VSD, *) TBPSI(I, IS), TBTHE(I, IS), TBKR(I, IS)
            END DO

            ! set up cubic spline coefficients for theta, using log(psi)
            ! based on routines 'spline' and 'splint' in NUMERICAL RECIPES
            ! FOR FORTRAN (..UNFINISHED), pp 109 and 110
            ! NB assumes 'natural' boundary conditions (ie zero 2nd derivatives)
            DO I = 1, IVSNTB(IS)
               XDUM(I) = LOG10(-TBPSI(I, IS))
               YDUM(I) = TBTHE(I, IS)
            END DO

            NDUM = IVSNTB(IS)
            Y2DUM(1) = zero
            UDUM(1) = zero
            Y2DUM(NDUM) = zero

            DO I = 2, NDUM - 1
               SIG = (XDUM(I) - XDUM(I - 1))/(XDUM(I + 1) - XDUM(I - 1))
               PDUM = SIG*Y2DUM(I - 1) + two
               Y2DUM(I) = (SIG - one)/PDUM
               UDUM(I) = (6.0D0*((YDUM(I + 1) - YDUM(I))/ &
                                 (XDUM(I + 1) - XDUM(I)) - (YDUM(I) - YDUM(I - 1)) &
                                 /(XDUM(I) - XDUM(I - 1)))/(XDUM(I + 1) - XDUM(I - 1)) &
                          - SIG*UDUM(I - 1))/PDUM
            END DO

            DO I = NDUM - 1, 1, -1
               Y2DUM(I) = Y2DUM(I)*Y2DUM(I + 1) + UDUM(I)
            END DO

            DO I = 1, NDUM
               TBTHEC(I, IS) = Y2DUM(I)
            END DO

            ! if required, set up cubic spline coefficients for Kr similarly
            IF (IVSFLG(IS) == 2) THEN
               DO I = 1, IVSNTB(IS)
                  YDUM(I) = TBKR(I, IS)
               END DO

               Y2DUM(1) = zero
               UDUM(1) = zero
               Y2DUM(NDUM) = zero

               DO I = 2, NDUM - 1
                  SIG = (XDUM(I) - XDUM(I - 1))/(XDUM(I + 1) - XDUM(I - 1))
                  PDUM = SIG*Y2DUM(I - 1) + two
                  Y2DUM(I) = (SIG - one)/PDUM
                  UDUM(I) = (6.0D0*((YDUM(I + 1) - YDUM(I))/ &
                                    (XDUM(I + 1) - XDUM(I)) - (YDUM(I) - YDUM(I - 1))/ &
                                    (XDUM(I) - XDUM(I - 1)))/(XDUM(I + 1) - XDUM(I - 1)) &
                             - SIG*UDUM(I - 1))/PDUM
               END DO

               DO I = NDUM - 1, 1, -1
                  Y2DUM(I) = Y2DUM(I)*Y2DUM(I + 1) + UDUM(I)
               END DO

               DO I = 1, NDUM
                  TBKRC(I, IS) = Y2DUM(I)
               END DO
            END IF
         END IF
      END DO

      ! VS06 ----- soil zone cell sizes (start at the ground surface)
      IF (NCSZON > 0) THEN
         CALL ALREAD(3, VSD, FID_logfile, ':VS06', NCSZON, 1, 0, CDUM, IDUM, DCSZON)
      END IF
      WRITE (FID_logfile, *) 'DCSZON: ', (DCSZON(I), I=1, NCSZON)

      DCSTOT = zero
      DCSDUM(0) = zero

      DO I = 1, NCSZON
         DCSTOT = DCSTOT + DCSZON(I)
         DCSDUM(I) = DCSTOT
         DCSNOD(I) = half*(DCSDUM(I) + DCSDUM(I - 1))
      END DO

      DCSNOD(NCSZON + 1) = DCSTOT + VSZMIN

      ! VS07 ----- river bed cell sizes (start at the bed surface)
      IF (NCRBED > 0) THEN
         CALL ALREAD(3, VSD, FID_logfile, ':VS07', NCRBED, 1, 0, CDUM, IDUM, DCRBED)
      END IF
      WRITE (FID_logfile, *) 'DCRBED: ', (DCRBED(I), I=1, NCRBED)

      DCRTOT = zero
      DCRDUM(0) = zero

      DO I = 1, NCRBED
         DCRTOT = DCRTOT + DCRBED(I)
         DCRDUM(I) = DCRTOT
         DCRNOD(I) = half*(DCRDUM(I) + DCRDUM(I - 1))
      END DO

      DCRNOD(NCRBED + 1) = DCRTOT + VSZMIN

      ! VS08 ----- soil/lithology layer definition data
      ! --- read no. of categories and elements
      CALL ALREAD(2, VSD, FID_logfile, ':VS08', 2, 1, 0, CDUM, IDUM, DUMMY)
      NUM_CATEGORIES_TYPES = IDUM(1)
      NELEM = IDUM(2)

      ! --- category data
      IF (NUM_CATEGORIES_TYPES == 0) THEN
         ! expect all elements to be input individually
         IF (BEXBK) THEN
            NCOUNT = total_no_elements - 2*total_no_links
         ELSE
            NCOUNT = total_no_elements - total_no_links
         END IF

      ELSE
         ! initialise arrays
         DO IEL = 1, NELEE
            DO ILYR = 1, NLYREE
               IVSDUM_VSREAD(IEL, ILYR) = 0
               RVSDUM_VSREAD(IEL, ILYR) = zero
            END DO
         END DO

         ! read layer data
         CALL ALREAD(6, VSD, FID_logfile, ':VS08a', NELEE, NLYREE, NUM_CATEGORIES_TYPES, CDUM, IVSDUM_VSREAD, RVSDUM_VSREAD)

         ! for NUM_CATEGORIES_TYPES = 1, set all elements = category 1
         IF (NUM_CATEGORIES_TYPES == 1) THEN
            DO IEL = 1, total_no_elements
               IVSCAT_VSREAD(IEL) = 1
            END DO

            ! for > 1 category read in categories for links (if required) and grids
         ELSE
            IF (BEXBK .AND. total_no_links > 0) THEN
               CALL ALREAD(2, VSD, FID_logfile, ':VS08b', total_no_links, 1, NUM_CATEGORIES_TYPES, CDUM, IVSCAT_VSREAD, DUMMY)
            END IF

            CALL ALREAD(4, VSD, FID_logfile, ':VS08c', NX, NY, NUM_CATEGORIES_TYPES, CDUM, IDUM, DUMMY)

            DO IY = 1, NY
               IXY0 = (IY - 1)*NX
               DO IX = 1, NX
                  IEL = ICMXY(IX, IY)
                  IF (IEL /= 0) IVSCAT_VSREAD(IEL) = IDUM(IXY0 + IX)
               END DO
            END DO
         END IF

         ! move layer data into elements for ...
         NCOUNT = 0
         element_category_loop: DO IEL = 1, total_no_elements
            IF (ICMREF(IEL, 1) == 1 .OR. ICMREF(IEL, 1) == 2 .OR. &
                (.NOT. BEXBK .AND. ICMREF(IEL, 1) == 3)) CYCLE element_category_loop

            IF (IVSCAT_VSREAD(IEL) == 0) THEN
               NCOUNT = NCOUNT + 1
            ELSE
               BDONE_VSREAD(IEL) = .TRUE.
               ICAT = IVSCAT_VSREAD(IEL)
               ICOUNT = 0

               ! Modern DO WHILE replacing GOTO 350 / 355
               DO WHILE (IVSDUM_VSREAD(ICAT, ICOUNT + 1) /= 0)
                  ICOUNT = ICOUNT + 1
               END DO

               ! ...grids
               IF (ICMREF(IEL, 1) == 0) THEN
                  NLYR(IEL) = ICOUNT
                  DO ILYR = 1, NLYR(IEL)
                     NTSOIL(IEL, ILYR) = IVSDUM_VSREAD(ICAT, ILYR)
                     ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - RVSDUM_VSREAD(ICAT, ILYR)
                  END DO

                  ! ...banks
               ELSE
                  DO I = 1, 2
                     IBK = ICMBK(IEL, I)
                     BDONE_VSREAD(IBK) = .TRUE.
                     NLYR(IBK) = ICOUNT
                     DO ILYR = 1, NLYR(IBK)
                        NTSOIL(IBK, ILYR) = IVSDUM_VSREAD(ICAT, ILYR)
                        ZLYRBT(IBK, ILYR) = ZGRUND(IBK) - RVSDUM_VSREAD(ICAT, ILYR)
                     END DO
                  END DO

                  ! ...links (NB uses data from bank 2, which is identical to bank 1)
                  LCOUNT = 0

                  ! Modern DO WHILE replacing GOTO 390 / 395
                  DO WHILE (RVSDUM_VSREAD(ICAT, LCOUNT + 1) >= ZGRUND(IBK) - ZBEFF(IEL) + VSZMIN)
                     LCOUNT = LCOUNT + 1
                  END DO

                  NLYR(IEL) = LCOUNT
                  DO ILYR = 1, NLYR(IEL)
                     NTSOIL(IEL, ILYR) = NTSOIL(IBK, ILYR)
                     ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
                  END DO
               END IF
            END IF
         END DO element_category_loop
      END IF

      ! check no. of category elements consistent with no. of individual elements
      IF (NCOUNT /= NELEM) THEN
         WRITE (MSG, 9000) NCOUNT
         CALL RAISE_ERROR(ERRLVL_fatal, 1032, FID_logfile, 0, 0, MSG)
      END IF

      ! --- element data
      IF (NELEM /= 0) THEN
         ! initialise variables
         DO IEL = 1, NELEE
            DO ILYR = 1, NLYREE
               IVSDUM_VSREAD(IEL, ILYR) = 0
               RVSDUM_VSREAD(IEL, ILYR) = zero
            END DO
         END DO

         ! read layer data
         CALL ALREAD(6, VSD, FID_logfile, ':VS08d', NELEE, NLYREE, NELEM, CDUM, IVSDUM_VSREAD, RVSDUM_VSREAD)

         element_data_loop: DO IEL = 1, total_no_elements
            ! ignore banks, links (if no banks), and elements already processed
            IF (BDONE_VSREAD(IEL) .OR. ICMREF(IEL, 1) == 1 .OR. ICMREF(IEL, 1) == 2 .OR. &
                (.NOT. BEXBK .AND. ICMREF(IEL, 1) == 3)) CYCLE element_data_loop

            BDONE_VSREAD(IEL) = .TRUE.
            ICOUNT = 0

            DO WHILE (IVSDUM_VSREAD(IEL, ICOUNT + 1) /= 0)
               ICOUNT = ICOUNT + 1
            END DO

            ! ...grids
            IF (ICMREF(IEL, 1) == 0) THEN
               NLYR(IEL) = ICOUNT
               DO ILYR = 1, NLYR(IEL)
                  NTSOIL(IEL, ILYR) = IVSDUM_VSREAD(IEL, ILYR)
                  ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - RVSDUM_VSREAD(IEL, ILYR)
               END DO

               ! ...banks
            ELSE
               DO I = 1, 2
                  IBK = ICMBK(IEL, I)
                  BDONE_VSREAD(IBK) = .TRUE.
                  NLYR(IBK) = ICOUNT
                  DO ILYR = 1, NLYR(IBK)
                     NTSOIL(IBK, ILYR) = IVSDUM_VSREAD(IEL, ILYR)
                     ZLYRBT(IBK, ILYR) = ZGRUND(IBK) - RVSDUM_VSREAD(IEL, ILYR)
                  END DO
               END DO

               ! ...links
               LCOUNT = 0
               DO WHILE (RVSDUM_VSREAD(IEL, LCOUNT + 1) >= ZGRUND(IBK) - ZBEFF(IEL) + VSZMIN)
                  LCOUNT = LCOUNT + 1
               END DO

               NLYR(IEL) = LCOUNT
               DO ILYR = 1, NLYR(IEL)
                  NTSOIL(IEL, ILYR) = NTSOIL(IBK, ILYR)
                  ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
               END DO
            END IF
         END DO element_data_loop
      END IF

      ! adjust horizon boundaries in soil zone to match computational mesh
      ! and set up ZLYRBT for ground surface
      adjust_horizon_loop: DO IEL = NGDBGN, total_no_elements
         layer_adjust_loop: DO ILYR = NLYR(IEL), 1, -1
            IF (ZGRUND(IEL) - ZLYRBT(IEL, ILYR) > DCSTOT + VSZMIN) EXIT layer_adjust_loop

            search_zone_loop: DO I = 1, NCSZON + 1
               IF (DCSNOD(I) > ZGRUND(IEL) - ZLYRBT(IEL, ILYR)) THEN
                  ZLYRBT(IEL, ILYR) = ZGRUND(IEL) - DCSDUM(I - 1)
                  CYCLE layer_adjust_loop
               END IF
            END DO search_zone_loop
         END DO layer_adjust_loop

         ZLYRBT(IEL, NLYR(IEL) + 1) = ZGRUND(IEL)
      END DO adjust_horizon_loop

      IF (BEXBK) THEN
         DO IEL = 1, total_no_links
            IBK = ICMBK(IEL, 1)
            DO ILYR = 1, NLYR(IEL)
               ZLYRBT(IEL, ILYR) = ZLYRBT(IBK, ILYR)
            END DO
         END DO
      END IF

      ! check that all elements have been set up
      check_done_loop: DO IEL = 1, total_no_elements
         IF (.NOT. BEXBK .AND. ICMREF(IEL, 1) /= 0) CYCLE check_done_loop
         IF (.NOT. BDONE_VSREAD(IEL)) THEN
            WRITE (MSG, 9020) IEL
            CALL RAISE_ERROR(ERRLVL_error, 1033, FID_logfile, 0, 0, MSG)
         END IF
      END DO check_done_loop

      ! VS09 ----- channel bed layer
      IF (total_no_links > 0 .AND. BEXBK) THEN
         ! read soil types for each link
         CALL ALREAD(2, VSD, FID_logfile, ':VS09', total_no_links, 1, 1, CDUM, ISRBED, DUMMY)

         ! read bed depths for each link
         CALL ALREAD(3, VSD, FID_logfile, ':VS09a', total_no_links, 1, 1, CDUM, IDUM, DRBED)

         ! set up channel bed layer for each link
         DO IEL = 1, total_no_links
            IF (DRBED(IEL) > VSZMIN) THEN
               NLYR(IEL) = NLYR(IEL) + 1
               NTSOIL(IEL, NLYR(IEL)) = ISRBED(IEL)
               ZLYRBT(IEL, NLYR(IEL)) = ZBEFF(IEL) - DRBED(IEL)

               IF (ZLYRBT(IEL, NLYR(IEL)) < ZLYRBT(IEL, NLYR(IEL) - 1) + VSZMIN) THEN
                  NLYR(IEL) = NLYR(IEL) - 1
                  NTSOIL(IEL, NLYR(IEL)) = ISRBED(IEL)
               END IF
            END IF
         END DO

         ! adjust horizon boundaries in river bed to match computational mesh
         ! and set up ZLYRBT for river bed surface
         bed_adjust_loop: DO IEL = 1, total_no_links
            layer_bed_loop: DO ILYR = NLYR(IEL), 1, -1
               IF (ZGRUND(IEL) - ZLYRBT(IEL, ILYR) > DCRTOT + VSZMIN) EXIT layer_bed_loop

               search_bed_loop: DO I = 1, NCRBED + 1
                  IF (DCRNOD(I) > ZGRUND(IEL) - ZLYRBT(IEL, ILYR)) THEN
                     ZLYRBT(IEL, ILYR) = ZBEFF(IEL) - DCRDUM(I - 1)
                     CYCLE layer_bed_loop
                  END IF
               END DO search_bed_loop
            END DO layer_bed_loop

            ZLYRBT(IEL, NLYR(IEL) + 1) = ZBEFF(IEL)
         END DO bed_adjust_loop
      END IF

      ! VS10 ----- aquifer zone user-defined connectivities
      ! FIX: Read into the IDUM array first to satisfy strict array-interface
      ! requirements, then assign the value to the scalar NAQCON.
      CALL ALREAD(2, VSD, FID_logfile, ':VS10', 1, 1, 0, CDUM, IDUM, DUMMY)
      NAQCON = IDUM(1)

      IF (NAQCON > 0) THEN
         CALL ALREAD(2, VSD, FID_logfile, ':VS10a', 4, NAQCON, 0, CDUM, IAQCON, DUMMY)
      END IF

      ! VS11 ----- no. of categories for boundary conditions
      CALL ALREAD(2, VSD, FID_logfile, ':VS11', 8, 1, 0, CDUM, IDUM, DUMMY)
      NVSWL = IDUM(1)
      NVSSP = IDUM(2)
      NVSLF = IDUM(3)
      NVSLH = IDUM(4)
      NVSLG = IDUM(5)
      NVSBF = IDUM(6)
      NVSBH = IDUM(7)
      NVSBD = IDUM(8)

      ! wells -----------------------------------------------
      ! VS12 ----- no. of wells
      IF (NVSWL > 0) THEN
         CALL ALREAD(2, VSD, FID_logfile, ':VS12', 1, 1, 0, CDUM, IDUM, DUMMY)
         NW = IDUM(1)

         ! VS12a ---- element, category number, and target element
         CALL ALREAD(2, VSD, FID_logfile, ':VS12a', 3, NW, 0, CDUM, IDUM, DUMMY)
         DO IW = 1, NW
            I0 = 3*(IW - 1)
            IEL = IDUM(I0 + 1)
            NVSWLC(IEL) = MAX(1, IDUM(I0 + 2))
            IWT = IDUM(I0 + 3)
            IF (IWT > 0) NVSWLT(IWT) = IEL
            NVSWLI(IEL) = IW
         END DO

         ! VS12b ---- depth below ground of bottom and top of well screen
         CALL ALREAD(3, VSD, FID_logfile, ':VS12b', 2, NW, 0, CDUM, IDUM, DUMMY)
         DO IW = 1, NW
            VSZWLB(IW) = DUMMY(2*(IW - 1) + 1)
            VSZWLT(IW) = DUMMY(2*(IW - 1) + 2)
         END DO
      END IF

      ! springs ---------------------------------------------
      ! VS13 ----- no. of springs
      IF (NVSSP > 0) THEN
         NSP = NVSSP
         ! VS13a ---- element and target element
         CALL ALREAD(2, VSD, FID_logfile, ':VS13a', 2, NSP, 0, CDUM, IDUM, DUMMY)
         DO ISP = 1, NSP
            IEL = IDUM(2*(ISP - 1) + 1)
            IF (IDUM(2*(ISP - 1) + 2) > 0) NVSSPT(IDUM(2*(ISP - 1) + 2)) = IEL
         END DO

         ! VS13b ---- depth of spring source below ground, elevation of
         !            discharge point, spring coefficient
         CALL ALREAD(3, VSD, FID_logfile, ':VS13b', 3, NSP, 0, CDUM, IDUM1, DUMMY)
         DO ISP = 1, NSP
            IEL = IDUM(2*(ISP - 1) + 1)
            VSSPD(IEL) = DUMMY(3*(ISP - 1) + 1)
            VSSPZ(IEL) = DUMMY(3*(ISP - 1) + 2)
            VSSPCO(IEL) = DUMMY(3*(ISP - 1) + 3)
         END DO
      END IF

      ! lateral boundary conditions -------------------------
      ! VS14 ----- grid of codes (types)
      NDUM = MAX(NVSLF, NVSLH, NVSLG)

      IF (NDUM > 0) THEN
         CALL ALREAD(4, VSD, FID_logfile, ':VS14', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1)*NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NLBTYP(IEL) = IDUM(IXY0 + IX)
            END DO
         END DO

         ! VS15 ----- grid of category numbers
         CALL ALREAD(4, VSD, FID_logfile, ':VS15', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1)*NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NLBCAT(IEL) = MAX(1, IDUM(IXY0 + IX))
            END DO
         END DO

         ! VS16 ----- No. of lateral boundary categories (flow, head, and head gr
         ! with b.c/s set only on selected layers
         ! initialise arrays to default values for reading in time-series data
         DO ICAT = 1, NDUM
            NVSLFN(ICAT) = 0
            NVSLHN(ICAT) = 0
            NVSLGN(ICAT) = 0
         END DO

         NVSLFT = NVSLF
         NVSLHT = NVSLH
         NVSLGT = NVSLG

         CALL ALREAD(2, VSD, FID_logfile, ':VS16', 1, 1, 0, CDUM, IDUM, DUMMY)
         NLB = IDUM(1)

         DO ILB = 1, NLB
            ! VS16a ---- b.c. type, category, no. of layers
            CALL ALREAD(2, VSD, FID_logfile, ':VS16a', 3, 1, 0, CDUM, IDUM, DUMMY)
            ITYP = IDUM(1)
            ICAT = IDUM(2)
            NLDUM = IDUM(3)

            ! VS16b ---- layer numbers
            CALL ALREAD(2, VSD, FID_logfile, ':VS16b', NLDUM, 1, 0, CDUM, IDUM, DUMMY)

            IF (ITYP == 3) THEN
               NVSLFN(ICAT) = NLDUM
               NVSLFT = NVSLFT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLFL(I, ICAT) = IDUM(I)
               END DO
            END IF

            IF (ITYP == 4) THEN
               NVSLHN(ICAT) = NLDUM
               NVSLHT = NVSLHT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLHL(I, ICAT) = IDUM(I)
               END DO
            END IF

            IF (ITYP == 5) THEN
               NVSLGN(ICAT) = NLDUM
               NVSLGT = NVSLGT + NLDUM - 1
               DO I = 1, NLDUM
                  NVSLGL(I, ICAT) = IDUM(I)
               END DO
            END IF
         END DO
      END IF

      ! bottom boundary conditions --------------------------
      ! VS17 ----- grid of codes (types)
      NDUM = MAX(NVSBF, NVSBH, NVSBD)

      IF (NDUM > 0) THEN
         IF (total_no_links > 0 .AND. BEXBK) THEN
            CALL ALREAD(2, VSD, FID_logfile, ':VS17', total_no_links, 1, 1, CDUM, IDUM, DUMMY)
            DO IEL = 1, total_no_links
               NBBTYP(IEL) = IDUM(IEL)
               NBBTYP(total_no_links + IEL) = IDUM(IEL)
               NBBTYP(2*total_no_links + IEL) = IDUM(IEL)
            END DO
         END IF

         CALL ALREAD(4, VSD, FID_logfile, ':VS17', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1)*NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NBBTYP(IEL) = IDUM(IXY0 + IX)
            END DO
         END DO

         ! VS18 ----- grid of category numbers
         IF (total_no_links > 0 .AND. BEXBK) THEN
            CALL ALREAD(2, VSD, FID_logfile, ':VS18', total_no_links, 1, 1, CDUM, IDUM, DUMMY)
            DO IEL = 1, total_no_links
               ICAT = MAX(1, IDUM(IEL))
               NBBCAT(IEL) = ICAT
               NBBCAT(total_no_links + IEL) = ICAT
               NBBCAT(2*total_no_links + IEL) = ICAT
            END DO
         END IF

         CALL ALREAD(4, VSD, FID_logfile, ':VS18', NX, NY, NDUM, CDUM, IDUM, DUMMY)
         DO IY = 1, NY
            IXY0 = (IY - 1)*NX
            DO IX = 1, NX
               IEL = ICMXY(IX, IY)
               IF (IEL /= 0) NBBCAT(IEL) = MAX(1, IDUM(IXY0 + IX))
            END DO
         END DO
      END IF

      RETURN

      ! FORMAT statements
9000  FORMAT('Error in number of VSS layer elements. NELEM should be ', I4)
9020  FORMAT('Error reading VSS layers for element ', I4, '.')
9030  FORMAT('Soil type ', I4, ' not expected for soil property tables.')

   END SUBROUTINE VSREAD

!> Adds stream-aquifer interaction terms to the column system.
!>
!> `VSSAI` applies the channel-aquifer exchange correction (added 1998-11 by
!> SPA) for boundary types `JCBC = 9` (no explicit banks) and `JCBC = 10`
!> (explicit banks) on one face of the column assembled by [[vscolm]]. Entry
!> conditions are `1 <= FACE <= 4`, `ICBOT <= ICBED+1, ICTOP`, and `CDELL > 0`.
!>
!> The lowest affected cell is `ICBOT` for `JCBC = 9` (the stream bed is
!> effectively at the base of the land element) or `ICBED+1` for `JCBC = 10`
!> (interaction starts above the explicit river-bed cell). For each affected
!> cell,
!>
!> \[
!>   \Delta h = CZS - CZ_c - CPSI_c .
!> \]
!>
!> The channel-to-aquifer contact area is limited when the channel water depth
!> is low or the cell would otherwise be losing water to a shallow channel:
!>
!> \[
!>   f =
!>   \begin{cases}
!>     \min(1,\;depadj/CDELZ_c), & \Delta h > 0,\\
!>     1, & \text{otherwise},
!>   \end{cases}
!>   \qquad A/L = {f\,CAIJ(FACE,c)\over CDELL}.
!> \]
!>
!> The exchange flux and its derivative are
!>
!> \[
!>   Q_c = CKIJ_c\,\Delta h\,(A/L),\qquad
!>   {dQ_c\over d\psi_c} = -CKIJ_c\,(A/L).
!> \]
!>
!> `CQH(FACE,c)` stores the diagnostic flux, and the linearised term is added
!> as `CB(c) += dQ_c/d\psi_c` and `CR(c) -= Q_c`.
!>
!> @note
!> `CDKIJ` is a dummy argument but not used in the current formula: the
!> derivative term omits the `CDKIJ*DH` contribution used in the original 1994
!> formula, matching the 1998-11 SPA revision noted in the header comments.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written; version 4.0 completed 1996-01-15. |
!> | 1997-01-21 | RAH | 4.1 | Declared `IDUM` as `INTEGER` rather than `DOUBLEPRECISION`; introduced `AOL`, `DH`, and `KIJ` to reduce the number of operations. |
!> | 1997-02-03 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks; added explanatory comments. |
!> | 1997-02-11 | RAH | 4.1 | Removed the outputs `CQBKB` and `CQBKF` (now handled in [[vssim]]). |
!> | 1997-05-14 | RAH | 4.1 | Added the argument `FACE` and a leading dimension to `CAIJ` and `CQH`. |
!> | 1998-11-03 | SPA | - | Added the `depadj` channel-depth contact-area limit and changed the derivative definition to the current form. |
!> | 2026-04-06/07 | SvB | 4.6 | Added the `PURE` attribute; no other change. |
!> @endhistory
   PURE SUBROUTINE VSSAI(FACE, JCBC, ICBOT, ICTOP, ICBED, CDELL, CZ, &
                         CAIJ, CZS, CPSI, CKIJ, CDKIJ, CB, CR, CQH, depadj, cdelz)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: FACE                      !! Boundary face number, in `1:4`.
      INTEGER, INTENT(IN) :: JCBC                      !! Stream-aquifer boundary type, normally 9 or 10.
      INTEGER, INTENT(IN) :: ICBOT                     !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                     !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICBED                     !! River-bed cell index used for bank interaction.
      DOUBLE PRECISION, INTENT(IN) :: CDELL             !! Distance scale normal to the stream-aquifer face.
      DOUBLE PRECISION, INTENT(IN) :: CZS               !! Adjacent channel water-surface elevation.
      DOUBLE PRECISION, INTENT(IN) :: depadj            !! Channel-depth adjustment for contact-area limiting.
      DOUBLE PRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP)   !! Active-cell node elevations.
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Current pressure heads.
      DOUBLE PRECISION, INTENT(IN) :: CAIJ(4, ICBOT:ICTOP) !! Face areas by face and active cell.
      DOUBLE PRECISION, INTENT(IN) :: cdelz(ICBOT:ICTOP) !! Active-cell thicknesses used in the contact-area limit.
      DOUBLE PRECISION, INTENT(IN) :: CKIJ(ICBOT:ICTOP) !! Lateral hydraulic conductivity terms on this face.
      DOUBLE PRECISION, INTENT(IN) :: CDKIJ(ICBOT:ICTOP) !! Unused conductivity derivatives retained for the legacy interface.

      ! In+out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: CB(ICBOT:ICTOP) !! Matrix diagonal terms updated by stream-aquifer exchange.
      DOUBLE PRECISION, INTENT(INOUT) :: CR(ICBOT:ICTOP) !! Right-hand side terms updated by stream-aquifer exchange.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CQH(4, ICBOT:ICTOP) !! Diagnostic lateral fluxes on the stream-aquifer face.

      ! Locals
      INTEGER :: ICL, IDUM
      DOUBLE PRECISION :: QDUM, DQDUM, AOL, DH, KIJ, DDUM

      !----------------------------------------------------------------------*

      ! set lowest cell in exposed bank face
      IF (JCBC == 9) THEN
         ! in effect stream bed is at base of current land element
         IDUM = ICBOT
      ELSE
         ! stream-aquifer interaction with banks
         IDUM = ICBED + 1
      END IF

      ! loop over appropriate cells
      cell_loop: DO ICL = IDUM, ICTOP

         DH = CZS - CZ(ICL) - CPSI(ICL)

         ! !!!!! change to calculation of AOL for flow out of channel
         ! limits flows if depth of water in channel is low, or zero
         ! SPA, 03/11/98
         DDUM = 1.0D0
         IF (GTZERO(DH)) DDUM = MIN(ONE, depadj/cdelz(ICL))

         AOL = (DDUM*CAIJ(FACE, ICL))/CDELL
         KIJ = CKIJ(ICL)

         ! !!!! SPA, 03/11/98.  Change definition of flow derivative
         ! DQDUM =   ( CDKIJ(ICL)*DH - KIJ ) * AOL
         DQDUM = -KIJ*AOL

         QDUM = KIJ*DH*AOL
         CQH(FACE, ICL) = QDUM

         CB(ICL) = CB(ICL) + DQDUM
         CR(ICL) = CR(ICL) - QDUM

      END DO cell_loop

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
! 26/1/96

!> Builds soil/lithology hydraulic-property lookup tables.
!>
!> `VSSOIL` is called once by [[vsin]] to generate the pressure-head lookup
!> tables (`VSPPSI`, `VSPTHE`, `VSPKR`, `VSPETA`, and their derivatives
!> `VSPDTH`/`VSPDKR`/`VSPDET`) interpolated at runtime by [[vsfunc]]. The table
!> size is `NVSSOL = min(100,NSOLEE)` when `BFAST` is set, or
!> `min(500,NSOLEE)` otherwise.
!>
!> Rows `5:NVSSOL-1` cover a log-spaced pressure-head range from
!> \(-10^{-2}\) to \(-10^4\), with each soil/lithology type (`1:NS`) evaluated
!> according to its `IVSFLG` option:
!>
!> | `IVSFLG` | Model | Formula |
!> |:---------|:------|:--------|
!> | 1 | van Genuchten | \(\theta=\theta_r+(\theta_s-\theta_r)(1+(\alpha\lvert\psi\rvert)^n)^{-m}\), \(m=1-1/n\); `VSPDET` is set to zero rather than the commented-out analytic derivative. |
!> | 2 | user table | Natural cubic-spline interpolation of `TBTHE`/`TBKR` in `log10(-psi)` from [[vsread]], scaled by `VSPOR`. |
!> | 3 | exponential | \(\theta=\theta_r+(\theta_s-\theta_r)e^{\alpha\psi}\), \(K_r=e^{\alpha\psi}\). |
!> | 4 | tabulated theta / Averjanov Kr (for SHETRAN V3.4 compatibility) | Not implemented; the routine stops with `UNFINISHED code for soil properties type 4`. |
!>
!> Row `NVSSOL` is set to fixed dry-end values (`VSPTHE=VSTRES`, conductivity
!> and derivatives zero, `VSPPSI=-1e6`). For `IVSFLG` 2 or 4, storage
!> derivatives `VSPDTH`/`VSPDET` for interior rows are then overwritten by
!> finite differences of the interpolated `VSPTHE`/`VSPDTH` values with respect
!> to `VSPPSI`.
!>
!> Rows `1:4` extend the table to near-saturation, working down in pressure
!> head from `VSPPSI(4)=0`: `VSPKR` is fixed at 1, `VSPETA`/`VSPDTH`/`VSPDKR`
!> are carried down from rows 5/4 or set to `VSPSS`/zero, and each `VSPTHE`
!> row is built recursively from the row above using the corresponding
!> `VSPETA`/`VSPSS`:
!>
!> \[
!>   VSPTHE(4)=VSPOR,\quad
!>   VSPTHE(k) = VSPTHE(k{+}1) + VSPETA(k{+}1)\bigl(VSPPSI(k)-VSPPSI(k{+}1)\bigr)
!>   \ \text{for } k=3,2,
!> \]
!> \[
!>   VSPTHE(1) = VSPTHE(2) + VSPSS\bigl(VSPPSI(1)-VSPPSI(2)\bigr).
!> \]
!>
!> Finally, for rows `5:NVSSOL` the routine rescales `VSPKR` using a
!> DSATG-style saturation ratio,
!> \[
!>   K_r(i) = \left({\theta(i)-\theta_r\over\theta_s-\theta_r}\right)^2,
!> \]
!> so that `Kr` approaches unity at saturation even for van Genuchten `n < 2`,
!> where the original curve drops rapidly and unphysically below one just below
!> saturation. This overwrite runs after the derivative tables are already
!> finalised. If `BSOILP` is set, the completed tables are printed to `FID_logfile`.
!>
!> @note
!> The DSATG saturation-ratio rescale (see above) replaces `VSPKR` without
!> recomputing `VSPDKR`, so the relative-conductivity derivative used later by
!> [[vsfunc]] does not correspond to the final `VSPKR` curve for
!> `IVSFLG = 1` or `3`. This is a pre-existing characteristic of the table
!> construction, not something introduced by the 2026 modernisation.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written. Called only from [[vsin]]. |
!> | 2026-04-06/07 | SvB | 4.6 | Replaced the manual `EDUM**x` exponentiation (`EDUM` a hardcoded `e` constant) with the `EXP` intrinsic for the `IVSFLG=3` branch; equivalent result. |
!> | 2026-04-10 | SvB | 4.6 | Fixed the near-saturation `VSPTHE` initialisation: row 3 had collapsed to `VSPTHE(3,IS) = VSPOR(IS)` (the same value as row 4, with no correction term), which is now corrected to the recursive `VSPTHE(4,IS) + VSPETA(4,IS)*(VSPPSI(3)-VSPPSI(4))` form shown above. |
!> @endhistory
   SUBROUTINE VSSOIL()

      ! Assumed external module dependencies providing global variables:
      ! NSEE, NSOLEE, BFAST, NVSSOL, VSPPSI, NS, IVSFLG, VSPOR, VSTRES,
      ! VSALPH, VSVGN, VSPTHE, VSPDTH, VSPKR, VSPDKR, VSPETA, VSPDET, VSPSS,
      ! TBPSI, TBTHE, TBTHEC, TBKR, TBKRC, BSOILP, FID_logfile, zero, one, two, three

      IMPLICIT NONE

      ! Locals
      INTEGER :: I, IS, NDUM
      INTEGER :: NTBPOS(NSEE) = 1
      DOUBLE PRECISION :: RVSSOL, PSI, DDDUM
      DOUBLE PRECISION :: DDTSAT, DDTRES, DDA, DDN, DDM, DD1M1, DDTSMR
      DOUBLE PRECISION :: DDAP, DDAPN, DDAPN1, DDAPM, DDAPM1, DDAPM2, DDTCAP
      DOUBLE PRECISION :: DDTC, DDTCM, DDTCM1, DDTCM2, DDDTCP
      DOUBLE PRECISION :: PLOG, PLOGLO, PLOGHI, ADUM, BDUM, HDUM, RKRDUM

      ! set up size of internal look-up tables
      IF (BFAST) THEN
         NVSSOL = MIN(100, NSOLEE)
      ELSE
         NVSSOL = MIN(500, NSOLEE)
      END IF

      RVSSOL = DBLE(NVSSOL)

      ! loop over NVSSOL divisions of the soil property tables
      ! (NB. low values of I correspond to wet soils)
      ! psi ranges from -(10**-2) to -(10**4)
      psi_loop: DO I = 5, NVSSOL - 1

         PSI = -(10.0D0**(-two + 6.0D0*DBLE(I - 5)/RVSSOL))
         VSPPSI(I) = PSI

         ! set up property data for each soil type
         soil_loop: DO IS = 1, NS

            ! ... 1 (Van Genuchten)
            IF (IVSFLG(IS) == 1) THEN
               DDTSAT = VSPOR(IS)
               DDTRES = VSTRES(IS)
               DDA = VSALPH(IS)*100.0D0
               DDN = VSVGN(IS)
               DDM = one - (one/DDN)
               DD1M1 = (one/DDM) - one
               DDTSMR = DDTSAT - DDTRES
               DDAP = -DDA*PSI
               DDAPN = DDAP**DDN
               DDAPN1 = DDAP**(DDN - one)
               DDAPM = (one + DDAPN)**DDM
               DDAPM1 = (one + DDAPN)**(DDM + one)
               DDAPM2 = (one + DDAPN)**(DDM + two)
               DDDTCP = DDA*DDM*DDN*DDAPN1/DDAPM1

               VSPTHE(I, IS) = DDTRES + DDTSMR/DDAPM
               VSPDTH(I, IS) = DDTSMR*DDDTCP

               DDTCAP = MAX(1.0D-10, (VSPTHE(I, IS) - DDTRES)/DDTSMR)
               DDTC = one - (DDTCAP**(one/DDM))
               DDTCM = DDTC**DDM
               DDTCM1 = DDTC**(DDM - one)
               DDTCM2 = (one - DDTCM)**two

               VSPKR(I, IS) = SQRT(DDTCAP)*DDTCM2

               ! Commented out legacy derivative code maintained for reference
               ! VSPDKR(I,IS) = DSQRT(DDTCAP)*(one-DDTCM)*
               !  (half*(one-DDTCM)/DDTCAP + two*DDTCM1*DDTCAP**DD1M1) * DDDTCP

               DDDUM = (DDA*DDA*DDM*DDN*DDTSMR*DDAPN1/DDAPM2)* &
                       ((DDN - one)*(one + DDAPN) + (DDM + one)*DDN*DDAPN1)
               VSPETA(I, IS) = VSPTHE(I, IS)*VSPSS(IS)/VSPOR(IS) + VSPDTH(I, IS)

               ! VSPDET(I,IS) = VSPDTH(I,IS)*VSPSS(IS)/VSPOR(IS) + DDDUM
               VSPDET(I, IS) = zero

               ! ... 2 (tabulated theta and Kr)
            ELSE IF (IVSFLG(IS) == 2) THEN

               ! check for correct location in input table
               ! Safely bounds check using DO WHILE instead of simple IF
               DO WHILE (PSI < TBPSI(NTBPOS(IS) + 1, IS))
                  NTBPOS(IS) = NTBPOS(IS) + 1
               END DO

               NDUM = NTBPOS(IS)

               ! evaluate cubic spline polynomial for theta and Kr
               PLOG = LOG10(-PSI)
               PLOGHI = LOG10(-TBPSI(NDUM + 1, IS))
               PLOGLO = LOG10(-TBPSI(NDUM, IS))
               HDUM = PLOGHI - PLOGLO
               ADUM = (PLOGHI - PLOG)/HDUM
               BDUM = (PLOG - PLOGLO)/HDUM

               VSPTHE(I, IS) = ADUM*TBTHE(NDUM, IS) + BDUM*TBTHE(NDUM + 1, IS) + &
                               ((ADUM**three - ADUM)*TBTHEC(NDUM, IS) + &
                                (BDUM**three - BDUM)*TBTHEC(NDUM + 1, IS))* &
                               (HDUM**two)/6.0D0

               VSPTHE(I, IS) = VSPOR(IS)*VSPTHE(I, IS)

               VSPKR(I, IS) = ADUM*TBKR(NDUM, IS) + BDUM*TBKR(NDUM + 1, IS) + &
                              ((ADUM**three - ADUM)*TBKRC(NDUM, IS) + &
                               (BDUM**three - BDUM)*TBKRC(NDUM + 1, IS))* &
                              (HDUM**two)/6.0D0

               ! ... 3 (exponential)
            ELSE IF (IVSFLG(IS) == 3) THEN

               ! Replaced EDUM**(VSALPH * PSI) hack with precise EXP intrinsic
               DDDUM = EXP(VSALPH(IS)*PSI)
               VSPTHE(I, IS) = VSTRES(IS) + (VSPOR(IS) - VSTRES(IS))*DDDUM
               VSPDTH(I, IS) = (VSPOR(IS) - VSTRES(IS))*VSALPH(IS)*DDDUM

               VSPKR(I, IS) = DDDUM
               VSPDKR(I, IS) = VSALPH(IS)*DDDUM

               VSPETA(I, IS) = VSPTHE(I, IS)*VSPSS(IS)/VSPOR(IS) + VSPDTH(I, IS)
               VSPDET(I, IS) = VSPDTH(I, IS)*VSPSS(IS)/VSPOR(IS) + VSPDTH(I, IS)*VSALPH(IS)

               ! ... 4 (tabulated theta and Averjanov Kr)
            ELSE IF (IVSFLG(IS) == 4) THEN
               WRITE (*, '(A)') 'ERROR: Unfinished code for soil properties type 4.'
               CALL ERR_STOP(255)
            END IF

         END DO soil_loop
      END DO psi_loop

      ! set up property data for extreme dry conditions
      VSPPSI(NVSSOL) = -1.0D6
      DO IS = 1, NS
         VSPTHE(NVSSOL, IS) = VSTRES(IS)
         VSPKR(NVSSOL, IS) = zero
         VSPETA(NVSSOL, IS) = zero
         VSPDTH(NVSSOL, IS) = zero
         VSPDKR(NVSSOL, IS) = zero
         VSPDET(NVSSOL, IS) = zero
      END DO

      ! set up storage term for tabulated data
      DO I = 5, NVSSOL - 1
         DO IS = 1, NS
            IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
               VSPDTH(I, IS) = (VSPTHE(I + 1, IS) - VSPTHE(I, IS))/(VSPPSI(I + 1) - VSPPSI(I))
               VSPETA(I, IS) = VSPTHE(I, IS)*VSPSS(IS)/VSPOR(IS) + VSPDTH(I, IS)
            END IF
         END DO
      END DO

      DO I = 5, NVSSOL - 1
         DO IS = 1, NS
            IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
               VSPDET(I, IS) = VSPDTH(I, IS)*VSPSS(IS)/VSPOR(IS) + &
                               (VSPDTH(I + 1, IS) - VSPDTH(I, IS))/(VSPPSI(I + 1) - VSPPSI(I))
            END IF
         END DO
      END DO

      ! set up property data for extreme wet conditions
      VSPPSI(4) = zero
      VSPPSI(3) = 2.5D-1
      VSPPSI(2) = 5.0D-1
      VSPPSI(1) = 1.0D6

      wet_conditions_loop: DO IS = 1, NS

         ! Converted line-by-line assignments into high-performance array slices
         VSPKR(1:4, IS) = one
         VSPETA(3:4, IS) = VSPETA(5, IS)
         VSPETA(1:2, IS) = VSPSS(IS)
         VSPDTH(4, IS) = VSPDTH(5, IS)

         VSPTHE(4, IS) = VSPOR(IS)
         VSPTHE(3, IS) = VSPTHE(4, IS) + VSPETA(4, IS)*(VSPPSI(3) - VSPPSI(4))
         VSPTHE(2, IS) = VSPTHE(3, IS) + VSPETA(3, IS)*(VSPPSI(2) - VSPPSI(3))
         VSPTHE(1, IS) = VSPTHE(2, IS) + VSPSS(IS)*(VSPPSI(1) - VSPPSI(2))

         VSPDTH(1:3, IS) = zero
         VSPDKR(4, IS) = VSPDKR(5, IS)
         VSPDKR(1:3, IS) = zero
         VSPDET(1:4, IS) = zero

      END DO wet_conditions_loop

      ! DSATG-specific code - adjust relative conductivity curves so that
      ! Kr approaches unity at saturation (for values of VG-n less than 2,
      ! the value of Kr drops rapidly and unphysically less than one near satu...)
      dsatg_loop: DO IS = 1, NS
         RKRDUM = VSPOR(IS) - VSTRES(IS)
         ! Replace inner loop with high-performance array operation
         VSPKR(5:NVSSOL, IS) = ((VSPTHE(5:NVSSOL, IS) - VSTRES(IS))/RKRDUM)**two
      END DO dsatg_loop

      ! write soil property tables to PRI file
      IF (BSOILP) THEN
         WRITE (FID_logfile, 905) NS, NVSSOL
         DO IS = 1, NS
            WRITE (FID_logfile, 910) IS
            DO I = 1, NVSSOL
               WRITE (FID_logfile, 920) I, VSPPSI(I), VSPTHE(I, IS), VSPETA(I, IS), VSPKR(I, IS), &
                  VSPDTH(I, IS), VSPDET(I, IS), VSPDKR(I, IS)
            END DO
         END DO
      END IF

      RETURN

      ! FORMAT STATEMENTS
905   FORMAT(/'VSS physical soil/lithology property data'/ &
              '========================================='/ &
              I3, ' soils'/ &
              I3, ' values in soil property tables')

910   FORMAT(/ &
         3X, '  Soil property tables for soil/lithology type: ', I3/ &
         3X, '  -------------------------------------------------'// &
         3X, '      psi         theta          eta            Kr      ', &
         ' d(the)/d(psi) d(eta)/d(psi)  d(Kr)/d(psi)'/ &
         3X, '   (VSPPSI)      (VSPTHE)      (VSPETA)       (VSPKR)   ', &
         '   (VSPDTH)      (VSPDET)       (VSPDKR)  '/ &
         3X, '  ------------  ------------  ------------  ------------', &
         '  ------------  ------------  ------------')

920   FORMAT(I3, 7(2X, G14.6))

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
!>
!> @note
!> None of this routine's dummy arguments carry an `INTENT` attribute in the
!> current declarations.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written. |
!> | 1997-01-20 | RAH | 4.1 | Removed the leading comments; introduced the local `DHDUM`. |
!> | 1997-01-27 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks. |
!> @endhistory
   SUBROUTINE VSSPR(CZ, CZSP, CCS, CPSI, CKR, CDKR, CB, CR, CQSP)
!
! Input arguments
      DOUBLEPRECISION CZ    !! Spring-cell node elevation.
      DOUBLEPRECISION CZSP  !! Spring discharge elevation.
      DOUBLEPRECISION CCS   !! Spring conductance coefficient.
      DOUBLEPRECISION CPSI  !! Spring-cell pressure head.
      DOUBLEPRECISION CKR   !! Spring-cell relative hydraulic conductivity.
      DOUBLEPRECISION CDKR  !! Derivative of `CKR` with respect to pressure head.
!
! In+out arguments
      DOUBLEPRECISION CB !! Spring-cell matrix diagonal term.
      DOUBLEPRECISION CR !! Spring-cell right-hand side term.
!
! Output arguments
      DOUBLEPRECISION CQSP !! Spring discharge; zero when the spring is inactive.
!
! Locals, etc
      DOUBLEPRECISION DHDUM
!
!----------------------------------------------------------------------*
!
      DHDUM = CPSI + CZ - CZSP

      IF (GEZERO(DHDUM)) THEN

         CQSP = CCS*CKR*DHDUM
         CR = CR + CQSP

         CB = CB - CCS*CDKR

      ELSE

         CQSP = zero

      END IF
   END SUBROUTINE VSSPR

!> Adds the upper infiltration/exfiltration boundary to the top VSS cell.
!>
!> `VSUPPR` forms the top-boundary contribution for one VSS column. The input
!> `CDNET` is the net surface-water depth available over the timestep after
!> evaporation has been applied by [[vssim]], and `CKZS` is the vertical
!> saturated conductivity of the top cell. The routine uses the model flux
!> convention that `CQINF > 0` is upward from the subsurface to the surface, so
!> infiltration is negative. Entry conditions: `CDELZ > 0` and `DT > 0`.
!>
!> The water-availability limit is
!> \[
!>   q_{\rm in} = {d_{\rm net} \over \Delta t},
!> \]
!> the rate that would exhaust the available surface depth during the timestep
!> (Fortran name `QIN`). The hydraulic-capacity expression is
!> \[
!>   q_{\rm out} =
!>   {K_{zs} \over \Delta z/2}
!>   \left[\psi -
!>   \left(\max(d_{\rm net},0)+{\Delta z\over2}\right)\right],
!> \]
!> where `CPSI` is top-cell pressure head and `CDELZ` is top-cell thickness
!> (Fortran name `QOUT`).
!>
!> If available water is limiting (`q_in < -q_out`), the returned flux is
!> `CQINF = -q_in` and the derivative contribution is set to zero. Otherwise
!> the boundary is hydraulic-capacity limited, or exfiltrating, and
!> `CQINF = q_out` with derivative `CKZS/(CDELZ/2)`.
!>
!> The column-system updates are
!> \[
!>   CB \leftarrow CB - {K_{zs}\over\Delta z/2}\,A,\qquad
!>   CR \leftarrow CR + q_{\rm inf} A,
!> \]
!> except in the water-limited case where the coefficient term is zero.
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
!> @note
!> None of this routine's dummy arguments carry an `INTENT` attribute in the
!> current declarations.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written; version 4.0 completed 1995-12-20. |
!> | 1997-01-20 | RAH | 4.1 | Removed leading/long comments and lower-case code; used the generic `MAX`; rearranged expressions; stopped including `AL.G`. |
!> | 1997-01-27 | RAH | 4.1 | Passed data through arguments instead of `COMMON`. |
!> | 1997-05-14 | RAH | 4.1 | Replaced `CDW + (CQP - CEW)*DT` with the single input `CDNET` (see [[vssim]]). |
!> | 1998-11-04 | RAH | 4.2 | Renamed the `DUM?` locals to `QIN` and similar. |
!> @endhistory
   SUBROUTINE VSUPPR(CA0, CDELZ, CKZS, DT, CDNET, CPSI, CB, CR, &
                     CQINF)
! Input arguments
      DOUBLEPRECISION CA0    !! Plan area of the current element.
      DOUBLEPRECISION CDELZ  !! Top-cell thickness.
      DOUBLEPRECISION CKZS   !! Saturated vertical hydraulic conductivity for the top-cell soil.
      DOUBLEPRECISION DT     !! Timestep length.
      DOUBLEPRECISION CDNET  !! Net available surface-water depth after evaporation.
      DOUBLEPRECISION CPSI   !! Top-cell pressure head.
! In+out arguments

      DOUBLEPRECISION CB !! Top-cell matrix diagonal term.
      DOUBLEPRECISION CR !! Top-cell right-hand side term.
! Output arguments

      DOUBLEPRECISION CQINF !! Calculated upward-positive infiltration/exfiltration rate.
! Locals, etc
!INTRINSIC MAX

      DOUBLEPRECISION QIN, QOUT, CDQINF, DZO2

      DZO2 = half*CDELZ
      QIN = CDNET/DT
      CDQINF = CKZS/DZO2

      QOUT = CDQINF*(CPSI - (MAX(CDNET, ZERO) + DZO2))
! infiltration (limited by available water) or evaporation

      IF (QIN .LT. -QOUT) THEN
         CQINF = -QIN

         CDQINF = ZERO
! infiltration (limited by soil properties) or exfiltration

      ELSE

         CQINF = QOUT

      END IF
! add into right-hand-side of column tridiagonal system
      CB = CB - CDQINF*CA0

      CR = CR + CQINF*CA0
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
!> Entry conditions: `ICWLBT <= ICWLTP`;
!> `1 <= ICSOIL(ICWLBT:ICWLTP) <= NSEE`; and positive `CA0`, screened-cell
!> thicknesses including `CDELZ(ICWLTP+1)`, and a positive total
!> conductivity-depth weight \(W\) from
!> `VSK3D(ICSOIL(ICWLBT:ICWLTP),1:2)`.
!>
!> @note
!> The pressure-head reduction factor is evaluated explicitly. The routine does
!> not add a diagonal coefficient for the dependence of \(f_i\) on `CPSI(i)`, so
!> well abstraction changes affect the nonlinear iteration only through the next
!> column assembly. The sign convention assumes positive `CQWIN` is abstraction
!> from the VSS column.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written; version 4.0 completed 1995-02-28. |
!> | 1997-01-20 | RAH | 4.1 | Used generic intrinsics; introduced the local `QDUM`. |
!> | 1997-01-27 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks. |
!> | 1997-02-07 | RAH | 4.1 | Redefined `CQWI` to be divided by `CA0`; removed the output `CQW`. |
!> | 1997-05-14 | RAH | 4.1 | Replaced `LLEE`/`CKIJS` with the new arguments `NSEE`, `ICSOIL`, and `VSK3D`; rearranged the `QDUM` expression. |
!> | 2026-04-06/07 | SvB | 4.6 | Added the `PURE` attribute; no other change. |
!> @endhistory
   PURE SUBROUTINE VSWELL(NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL, CA0, &
                          CDELZ, CQWIN, CPSI, CR, CQWI, RKZDUM)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NSEE                    !! Declared soil-type dimension for conductivity arrays.
      INTEGER, INTENT(IN) :: ICWLBT                  !! Bottom screened well cell.
      INTEGER, INTENT(IN) :: ICWLTP                  !! Top screened well cell.
      INTEGER, INTENT(IN) :: ICSOIL(ICWLBT:ICWLTP)   !! Soil type by screened cell.
      DOUBLE PRECISION, INTENT(IN) :: CA0             !! Plan area of the current element.
      DOUBLE PRECISION, INTENT(IN) :: CQWIN           !! Prescribed total well abstraction rate.
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICWLBT:ICWLTP + 1) !! Screened-cell thicknesses plus the cell above the screen top.
      DOUBLE PRECISION, INTENT(IN) :: VSK3D(NSEE, 2)   !! Saturated x/y hydraulic conductivity by soil type.
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICWLBT:ICWLTP) !! Current pressure heads in screened cells.

      ! In+out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: CR(ICWLBT:ICWLTP) !! Right-hand side terms updated with realised abstraction.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT)   :: CQWI(ICWLBT:ICWLTP) !! Realised well abstraction rate per cell area.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: RKZDUM(ICWLBT:ICWLTP) !! Workspace for conductivity-depth weights.

      ! Locals
      INTEGER :: ICL, SOIL
      DOUBLE PRECISION :: RKZTOT, DZDUM, PDUM, QDUM, RKZ

      !----------------------------------------------------------------------*

      ! The value of CQWIN is the prescribed abstraction rate (m3/s).
      ! The actual abstraction rate CQWI (m/s) may be less than this if some
      ! of the aquifer around the well screen becomes unsaturated
      ! (ie if CPSI(ICL) < DZDUM below).

      ! Calculate product of mean lateral hydraulic conductivity & cell depth
      ! Kept as scalar DO loop to maximize performance on small cell slices
      RKZTOT = ZERO

      rkz_loop: DO ICL = ICWLBT, ICWLTP
         SOIL = ICSOIL(ICL)
         RKZ = HALF*(VSK3D(SOIL, 1) + VSK3D(SOIL, 2))*CDELZ(ICL)
         RKZDUM(ICL) = RKZ
         RKZTOT = RKZ + RKZTOT
      END DO rkz_loop

      ! Calculate flow into well for each cell, & add into matrix coefficients
      well_flow_loop: DO ICL = ICWLBT, ICWLTP
         DZDUM = HALF*(CDELZ(ICL) + CDELZ(ICL + 1))
         PDUM = MIN(DZDUM, MAX(CPSI(ICL), ZERO))

         QDUM = CQWIN*(RKZDUM(ICL)/RKZTOT)*(PDUM/DZDUM)
         CQWI(ICL) = QDUM/CA0

         CR(ICL) = QDUM + CR(ICL)
      END DO well_flow_loop

   END SUBROUTINE VSWELL

END MODULE VSmod
