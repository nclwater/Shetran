!> summary: Soil and boundary parameters, initial-condition options and setup switches.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> Everything the variably saturated subsurface component reads from the `VSD`
!> data file (`VS01`--`VS18`) and then treats as fixed: the zone and river-bed
!> discretisation controls, the soil hydraulic parameters for each `IVSFLG`
!> option, the boundary-condition category counts and types, and the
!> initial-condition selector.
!>
!> | Input | Meaning in the component |
!> |:------|:-------------------------|
!> | `BFAST` | Chooses 100 or `min(500,NSOLEE)` soil lookup entries in [[vs_soil_tables:VSSOIL]]. |
!> | `BSOILP` | Prints generated soil hydraulic lookup tables. |
!> | `BHELEV` | Interprets boundary head data as elevations rather than depths below ground. |
!> | `INITYP = 1` | Initialises an equilibrium profile from uniform phreatic-surface depth `VSIPSD`. |
!> | `INITYP = 2` | Initialises equilibrium profiles from phreatic-surface elevations in `VSI`. |
!> | `INITYP = 3` | Reads initial potentials for every cell from `VSI`. |
!> | `VSWV`, `VSWL` | Control w-mean averaging of vertical and lateral hydraulic conductivity. |
!>
!> | `IVSFLG` | Manual option | Implementation status |
!> |:---------|:--------------|:----------------------|
!> | 1 | van Genuchten water retention and conductivity parameters | Implemented. |
!> | 2 | user tables for \(\theta(\psi)\) and \(K_r(\psi)\) | Implemented with spline interpolation over input tables. |
!> | 3 | exponential functions | Implemented. |
!> | 4 | user table for \(\theta(\psi)\) and Averjanov \(K(\theta)\) | Parsed as a legacy option, but stops in [[vs_soil_tables:VSSOIL]]. |
!>
!> Boundary-condition categories follow the manual `VS11`--`VS18` groups:
!> pumping wells, springs, lateral flow/head/head-gradient boundaries, and
!> aquifer-base flow/head boundaries. The values that *vary with time* are in
!> [[vs_boundaries]]; only the fixed category definitions are here.
!>
!> This module carries the variables of the legacy `VSCOM1.INC` and
!> `VSINIT.INC` include groups. Module state is public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995--1998 | GP / RAH | 4.0--4.2 | Created the VSS component and its `.INC` include groups. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS Fortran sources into a single Fortran 90 module. |
!> | 2026-03 to 2026-05 | SB / SvB | 4.6 | Modernisation pass, and moved `VSREAD`'s read buffers to allocatable module state to avoid a stack-related crash. |
!> | 2026-09-10 | SvB | - | Split out of VSmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_config

   USE array_limits, ONLY: LLEE, nelee, nlfee, NLYREE, NSEE, NVSEE

   IMPLICIT NONE


! Legacy VSCOM1.INC global VSS variables retained as module state.
   LOGICAL :: BLOWP  !! Lower-boundary output print-control flag retained from legacy VSCOM1 state.
   LOGICAL :: BHELEV !! True when lateral boundary head inputs are elevations; false when they are depths below ground.
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
   INTEGER :: IVSSTO(LLEE, NELEE) !! Stored soil lookup-table interval by VSS cell and element.
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
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: VSKR !! Relative hydraulic conductivity by VSS cell and element.
   DOUBLEPRECISION :: WLNOW(NVSEE)        !! Current well abstraction values.
   DOUBLEPRECISION :: RLFNOW(NLYREE, NVSEE) !! Current lateral-flow boundary values.
   DOUBLEPRECISION :: RLHNOW(NLYREE, NVSEE) !! Current lateral-head boundary values.
   DOUBLEPRECISION :: RLGNOW(NLYREE, NVSEE) !! Current lateral-gradient boundary values.
   DOUBLEPRECISION :: RBFNOW(NVSEE)       !! Current bottom-flow boundary values.
   DOUBLEPRECISION :: RBHNOW(NVSEE)       !! Current bottom-head boundary values.

! Legacy VSINIT.INC initialisation variables retained as module state.
   LOGICAL :: BFAST  !! True to use the shorter generated soil lookup table.
   LOGICAL :: BSOILP !! True to print generated soil lookup tables.
   INTEGER :: IVSFLG(NSEE) !! Soil hydraulic-property option by soil type.
   INTEGER :: IVSNTB(NSEE) !! Number of tabulated hydraulic-property rows by soil type.
   INTEGER :: NVSERR       !! Accumulated VSS input/setup error count.
   INTEGER :: INITYP       !! Initial pressure-head option from the VSS input file.
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

END MODULE vs_config

