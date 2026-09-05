!> summary: Column-base, face-overlap, and well-flow state for contaminant transport.
!> author: JE, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `COLM_CG` replaces the legacy `COLM.CG` common blocks. [[frmod:incm]]
!> establishes the column bases, contaminant scaling, and lateral face-overlap
!> topology. During simulation, [[cmmod:colmw]] prepares the current column's
!> well fluxes and [[cmmod:colmsm]] uses the retained overlap mapping to obtain
!> adjacent-cell concentrations.
!>
!> | State group | Lifetime and principal use |
!> |:------------|:---------------------------|
!> | `NCOLMB`, `ZCOLMB`, `SCL`, `OODO` | Persistent geometry and scaling established by `INCM`. |
!> | `JKZCOL`, `JOLFN`, `NOL`, `NOLCE` | Setup-only allocatable workspace released after the first `INCM` call. |
!> | `NOLBT`, `NOLCEA` | Allocatable overlap mapping retained for contaminant timesteps. |
!> | `WELDRA` | Per-column cell workspace overwritten by `COLMW`. |
!> | `JBTLYR` | Inactive fixed-size legacy storage. |
!>
!> Overlap arrays are indexed by element and one of four lateral faces;
!> record arrays add an overlap-record index. `NOLBT` instead uses a cell
!> index and includes `top_cell_no+1` as a one-past-the-end sentinel. The
!> allocation helpers size these arrays from `total_no_elements` and
!> `top_cell_no`; fixed-size arrays retain the `NELEE` or `LLEE` capacity.
!> The module has no active `PRIVATE` statement, so its declared state and all
!> six names imported from `SGLOBAL` remain public. `NVEE` and `NOLEE` have no
!> active reference here but are retained because this transfer does not alter
!> the module interface.
!>
!> @warning
!> [[cmmod:cmrd]] reads the default and per-column base-cell choices from
!> manual records `CM7` and `CM11`, but current `INCM` then unconditionally
!> replaces every active `NCOLMB` value with `NLYRBT(NCL,1)`. Those manual
!> choices therefore do not survive the current initialization path.
!>
!> `JKZCOL` is zero-initialized, adjusted only by reading and rewriting its
!> own entries during bank setup, and has no downstream reader. Its current
!> values do not affect a calculation. In the `INCM` branch where an adjacent
!> cell is split (`JDEL==1`), the corresponding `JOLFN` overlap weights are not
!> assigned and retain their initialized value of zero. This documentation
!> transfer does not change either behaviour.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked and tidied the text. |
!> | 1991-07-16 | JE | 3.1 | Reordered names in `WELPRO`. |
!> | 1997-02-24 | RAH | 4.1 | Added explicit typing and separated `WELPRI` from mixed-type `WELPRO`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> | 2026-03-30 | SB | 4.6.1 | Retired unused legacy arrays, made six overlap arrays allocatable, and added allocation helpers. |
!> @endhistory
MODULE COLM_CG

   USE SGLOBAL, ONLY: NELEE, LLEE, NVEE, NOLEE, total_no_elements, top_cell_no

   USE MOD_PARAMETERS, ONLY: I_P
   USE MOD_ERROR, ONLY: errstat_alloc, errstat_dealloc

   IMPLICIT NONE

   INTEGER :: JBTLYR(NELEE)  !! Unused legacy bottom-soil-layer index by element.
   INTEGER :: NCOLMB(NELEE)  !! Bottom active contaminant cell by non-link element.

   DOUBLEPRECISION ZCOLMB(NELEE)  !! Elevation of each non-link element's column base (m).

   DOUBLEPRECISION SCL   !! Integer-overlap conversion factor, `1/32500`.
   DOUBLEPRECISION OODO  !! Reciprocal reference dispersion coefficient, `1/D0` (s/m2).

   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: JKZCOL  !! Inactive setup-only lateral-transmissivity weights.
   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: JOLFN   !! Setup-only overlap shares encoded on a 32500 scale.
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: NOL       !! Number of overlap records by element and face.
   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: NOLBT   !! First overlap record by element, local cell, and face.
   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: NOLCE   !! Local cell index by element, overlap record, and face.
   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: NOLCEA  !! Adjacent cell index by element, overlap record, and face.

   DOUBLEPRECISION WELDRA(LLEE)  !! Current column's signed VSS well-flow flux by cell (m/s).

!PRIVATE :: NELEE, LLEE, NVEE, NOLEE

CONTAINS

!> Allocates and zero-initializes the active-size face-overlap arrays.
!>
!> [[run_sim:simulation]] calls this routine once when contaminant transport
!> is enabled, after the model dimensions are available and before the main
!> simulation loop. [[frmod:incm]] later populates the arrays during the first
!> contaminant initialization.
!>
!> | Arrays | Allocated shape | Initial value | Later lifetime |
!> |:-------|:----------------|:--------------|:---------------|
!> | `JKZCOL`, `JOLFN`, `NOLCE` | `(total_no_elements, 2*top_cell_no+1, 4)` | Zero | Released by [[deallocate_colm_cg]]. |
!> | `NOLCEA` | `(total_no_elements, 2*top_cell_no+1, 4)` | Zero | Retained for contaminant timesteps. |
!> | `NOL` | `(total_no_elements, 4)` | Zero | Released by `deallocate_colm_cg`. |
!> | `NOLBT` | `(total_no_elements, top_cell_no+1, 4)` | Zero | Retained for contaminant timesteps. |
!>
!> The routine has no dummy arguments and mutates six allocatable variables in
!> `COLM_CG`. Every allocation is unconditional and has no `STAT=` handler, so
!> all six arrays must be unallocated and both dimension variables established
!> on entry. The normal run path is intentionally one-shot: even after
!> `deallocate_colm_cg`, `NOLBT` and `NOLCEA` remain allocated, so a second call
!> would fail unless the caller first deallocated those retained arrays too.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03-30 | SB | 4.6.1 | Added active-size allocation and zero-initialization for six overlap arrays. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE initialise_colm_cg()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "COLM_CG:initialise_colm_cg"

      allocate (JKZCOL(total_no_elements, 2*top_cell_no + 1, 4), STAT=ios)
      CALL errstat_alloc(ios, "JKZCOL", location)
      allocate (JOLFN(total_no_elements, 2*top_cell_no + 1, 4), STAT=ios)
      CALL errstat_alloc(ios, "JOLFN", location)
      allocate (NOL(total_no_elements, 4), STAT=ios)
      CALL errstat_alloc(ios, "NOL", location)
      allocate (NOLBT(total_no_elements, top_cell_no + 1, 4), STAT=ios)
      CALL errstat_alloc(ios, "NOLBT", location)
      allocate (NOLCE(total_no_elements, 2*top_cell_no + 1, 4), STAT=ios)
      CALL errstat_alloc(ios, "NOLCE", location)
      allocate (NOLCEA(total_no_elements, 2*top_cell_no + 1, 4), STAT=ios)
      CALL errstat_alloc(ios, "NOLCEA", location)

      ! Initialise to default values
      JKZCOL = 0
      JOLFN = 0
      NOL = 0
      NOLBT = 0
      NOLCE = 0
      NOLCEA = 0

   END SUBROUTINE initialise_colm_cg

!> Releases the four overlap arrays needed only during contaminant setup.
!>
!> [[run_sim:simulation]] calls this routine immediately after the first
!> [[frmod:incm]] call has finished building column and bank geometry.
!> `JKZCOL`, `JOLFN`, `NOL`, and `NOLCE` have no later active consumer and are
!> deallocated. `NOLBT` and `NOLCEA` deliberately remain allocated because
!> [[cmmod:colmsm]] uses them during every contaminant timestep.
!>
!> The routine has no dummy arguments. Each `DEALLOCATE` statement is
!> unconditional and has no `STAT=` handler, so all four setup-only arrays must
!> be allocated on entry. Calling this routine before [[initialise_colm_cg]] or
!> calling it twice would cause a Fortran runtime error and could leave only a
!> prefix of the four arrays released. No current routine deallocates the two
!> retained mappings; they remain allocated until process termination.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03-30 | SB | 4.6.1 | Added partial cleanup for setup-only overlap arrays. |
!> @endhistory
   SUBROUTINE deallocate_colm_cg()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "COLM_CG:deallocate_colm_cg"

      deallocate (JKZCOL, STAT=ios)
      CALL errstat_dealloc(ios, "JKZCOL", location)
      deallocate (JOLFN, STAT=ios)
      CALL errstat_dealloc(ios, "JOLFN", location)
      deallocate (NOL, STAT=ios)
      CALL errstat_dealloc(ios, "NOL", location)
      deallocate (NOLCE, STAT=ios)
      CALL errstat_dealloc(ios, "NOLCE", location)

   END SUBROUTINE deallocate_colm_cg

END MODULE COLM_CG
