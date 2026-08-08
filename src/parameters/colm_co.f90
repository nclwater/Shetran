!> summary: Previous-timestep water state for column contaminant and nitrate transport.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `COLM_CO` replaces the legacy `COLM.CO` common blocks. It retains the
!> element water state that transfers overland and variably saturated
!> subsurface results into the contaminant column calculations. When
!> contaminant transport starts, [[frmod:incm]] seeds the old-state arrays.
!> On later contaminant steps, [[cmmod:colmw]] copies one column's retained
!> values into [[colm_c2]], prepares the corresponding current values, and
!> writes those values back here for the next step.
!>
!> [[cmmod:cmsim]] passes `VSTHEO` to [[mnmod:mncont]] before `COLMW` advances
!> the column state, so the nitrate component receives the preceding
!> contaminant-step water contents. After the column sweep, `CMSIM` refreshes
!> `RSZWLO` from the current VSS well fluxes; both `COLMW` and
!> [[cmmod:linkw]] use that retained value when constructing irrigation input.
!>
!> | State group | Meaning and update path |
!> |:------------|:------------------------|
!> | `DSWO`, `ZONEO` | Previous surface-water depth and nondimensional active-column depth, updated by `COLMW`. |
!> | `QIO`, `QQRFO`, `QQQSWO`, `QQO`, `UUAJPO` | Previous precipitation, base, surface, subsurface, and vertical flows. |
!> | `GGAMMO` | Previous dynamic/dead-space water-coupling rate used by the contaminant equations. |
!> | `VSTHEO` | Previous effective cell water content used by contaminant transport and nitrate. |
!> | `RSZWLO` | Previous total well flux at each source element, refreshed after a contaminant timestep. |
!>
!> Every allocatable has a first extent of `total_no_elements`; the column
!> state is principally populated for non-link elements. Cell arrays have a
!> second extent of `top_cell_no+1` to include required interface/sentinel
!> positions, and face arrays use four lateral faces. Surface and subsurface
!> face flows are positive into a column. Vertical flow is positive upward,
!> making downward net precipitation negative. [[initialise_colm_co]]
!> allocates the arrays once when contaminant transport is enabled. There is
!> no corresponding deallocator, so they remain allocated until process
!> termination.
!>
!> The module has no active `PRIVATE` statement. Its ten declared arrays and
!> all four names imported from `SGLOBAL` are therefore public. The fixed
!> capacities `NELEE` and `LLEE` have no active use here but are retained to
!> preserve the current module interface.
!>
!> @warning
!> The startup values established by `INCM` do not always use the convention
!> of values later retained by `COLMW`. `INCM` copies raw `QVSH` values into
!> `QQO`; later updates store the solver-scaled composite flow
!> `Q1*ZONE1*ROH/KSP`. It also seeds `QQRFO` from `QVSV(NCOLMB,...)`, whereas
!> later updates use the lower interface `UUAJP1(NCEBOT-1)`.
!>
!> For L-shaped banks, `INCM` divides the width-weighted vertical flux used to
!> seed `UUAJPO` by `ROH`; the corresponding ordinary-cell assignment in
!> `COLMW` does not. Consequently, the first old-state values can have
!> different scaling or interface selection from subsequent values. This
!> documentation transfer records but does not alter that behaviour.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.1 | Original version written. |
!> | 1991-06-16 | JE | 3.1 | Completed. |
!> | 1994-08-08 | GP | 4.0 | Replaced `TH3O` with `VSTHEO`. |
!> | 1997-02-20 | RAH | 4.1 | Added explicit typing. |
!> | 1998-03-08 | RAH | 4.2 | Removed `WELDRO`. |
!> | 1998-11-03 | RAH | - | Removed `ERUZO`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> | 2026-03-30 | SB | 4.6.1 | Made the ten old-state arrays allocatable and added `initialise_colm_co`. |
!> @endhistory
MODULE COLM_CO
   USE SGLOBAL, ONLY : NELEE, LLEE, total_no_elements, top_cell_no
   IMPLICIT NONE

   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: DSWO     !! Previous surface-water depth above ground by non-link column (m).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: QIO      !! Previous net-precipitation flow; downward input is negative (m3/s).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: QQRFO    !! Previous upward water-volume flow through the column base (m3/s).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: RSZWLO   !! Previous total VSS well flux at each source element (m/s).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ZONEO    !! Previous nondimensional active-column depth, `(ZGRUND-ZCOLMB)/Z2`.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: GGAMMO !! Previous dynamic/dead-space coupling rate by column and cell (1/s).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: QQQSWO !! Previous surface-water volume flow into each column face (m3/s).
   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: QQO  !! Previous solver-scaled subsurface flow into each cell face (m3/s).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: UUAJPO !! Previous upward vertical water flux by column and cell interface (m/s).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VSTHEO !! Previous effective volumetric water content by column and cell.

!PRIVATE :: NELEE, LLEE


CONTAINS

!> Allocates and zero-initializes the persistent previous-column water state.
!>
!> [[run_sim:simulation]] calls this routine once when contaminant transport
!> is enabled, after the active model dimensions and contaminant count are
!> available and before the main simulation loop. [[frmod:incm]] later replaces
!> the zero safety values with the initial hydrological state for active
!> non-link columns when the contaminant component reaches its start time.
!>
!> | Arrays | Allocated shape | Initial value |
!> |:-------|:----------------|:--------------|
!> | `DSWO`, `QIO`, `QQRFO`, `RSZWLO`, `ZONEO` | `(total_no_elements)` | `0.0D0` |
!> | `GGAMMO`, `UUAJPO`, `VSTHEO` | `(total_no_elements, top_cell_no+1)` | `0.0D0` |
!> | `QQQSWO` | `(total_no_elements, 4)` | `0.0D0` |
!> | `QQO` | `(total_no_elements, top_cell_no+1, 4)` | `0.0D0` |
!>
!> The routine has no dummy arguments and mutates all ten allocatables in
!> `COLM_CO`. Both dimension variables must already describe the active model,
!> and every array must be unallocated. Its six `ALLOCATE` statements are
!> unconditional and have no `STAT=` handling. An allocation failure, or a
!> second call encountering the already-allocated objects in the first
!> statement, therefore causes Fortran error termination; there is no rollback
!> or cleanup path for earlier statements. No current routine deallocates this
!> state.
!>
!> @note
!> Zero is an allocation-time safety value, not the physical starting state.
!> `INCM` establishes the active old-state entries before the first
!> contaminant update; entries outside the active element/cell ranges retain
!> zero unless another consumer writes them.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03-30 | SB | 4.6.1 | Added active-size allocation and zero-initialization for ten old-state arrays. |
!> @endhistory
   SUBROUTINE initialise_colm_co()

      allocate   (DSWO(total_no_elements),QIO(total_no_elements))
      allocate   (QQRFO(total_no_elements),RSZWLO(total_no_elements))
      allocate   (ZONEO(total_no_elements))
      allocate   (GGAMMO(total_no_elements,top_cell_no+1),QQQSWO(total_no_elements,4))
      allocate   (QQO(total_no_elements,top_cell_no+1,4))
      allocate   (UUAJPO(total_no_elements,top_cell_no+1),VSTHEO(total_no_elements,top_cell_no+1))
      DSWO=0.0d0
      QIO=0.0d0
      QQRFO=0.0d0
      RSZWLO=0.0d0
      ZONEO=0.0d0
      GGAMMO=0.0d0
      QQQSWO=0.0d0
      QQO=0.0d0
      UUAJPO=0.0d0
      VSTHEO=0.0d0

   END SUBROUTINE initialise_colm_co

END MODULE COLM_CO
