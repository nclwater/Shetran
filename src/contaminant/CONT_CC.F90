!> summary: Shared state and properties for contaminant transport.
!>
!> Replaces the legacy `CONT.CC` common blocks with module data for contaminant
!> boundary conditions, concentrations, adsorption and decay properties,
!> retardation, and channel-bed exchange. [[cmmod]] reads and advances this
!> state, while [[frmod]] derives coefficients and establishes initial values.
!> [[mnmod]] supplies nitrate and plant source/sink terms through `SSS1` and
!> `SSS2`, and [[visualisation_interface_left]] exposes current concentrations.
!>
!> Fixed-size arrays use the compile-time maxima imported from `SGLOBAL`. The
!> eight allocatable runtime arrays use active extents set by
!> `initialise_cont_cc`: concentration and source arrays are indexed by element,
!> cell, and contaminant; bank-retardation arrays add a bank-side index after the
!> link index. Module state is public by default and is mutated during
!> initialisation and simulation.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.1 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed the original implementation. |
!> | 1991-06-18 | JE | 3.1 | Added the `WELC` well-concentration block. |
!> | 1997-02-24 | RAH | 4.1 | Added explicit typing. |
!> | 2004-11 | JE | - | Converted to Fortran 95. |
!> | 2026-03 | SB | 4.6.1 | Made eight runtime arrays allocatable and added active-size allocation. |
!> @endhistory
MODULE CONT_CC
   USE SGLOBAL, ONLY: NELEE, NCONEE, LLEE, NSEE, NSEDEE, NLFEE, total_no_elements, top_cell_no, total_no_links

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE MOD_ERROR, ONLY: errstat_alloc

   IMPLICIT NONE

   DOUBLEPRECISION CCAPB(NELEE, NCONEE)  !! Base concentration boundary by element and contaminant.
   DOUBLEPRECISION CCPBO(NELEE, NCONEE)  !! Legacy companion to `CCAPB`; not referenced by current source.
   DOUBLEPRECISION CCAPE(NELEE, NCONEE)  !! External-inflow concentration by element and contaminant.
   DOUBLEPRECISION CCAPI(NCONEE)        !! Current rainfall concentration by contaminant.
   DOUBLEPRECISION CCAPIO(NCONEE)       !! Previous rainfall concentration by contaminant.
   DOUBLEPRECISION CCAPR(NELEE, NCONEE)  !! Base flux-boundary concentration by element and contaminant.
   DOUBLEPRECISION CCAPRO(NELEE, NCONEE) !! Legacy companion to `CCAPR`; not referenced by current source.
   DOUBLEPRECISION IIICF(NCONEE)        !! Current dry-deposition rate by contaminant.
   DOUBLEPRECISION IIICFO(NCONEE)       !! Previous dry-deposition rate by contaminant.

   DOUBLEPRECISION CCCCW(NELEE, NCONEE) !! Well-water concentration by well/element and contaminant.

   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: CCCC  !! Current dynamic-region concentration.
   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: CCCCO !! Previous dynamic-region concentration.
   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: SSSS  !! Current dead-space-region concentration.
   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: SSSSO !! Previous dead-space-region concentration.

   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: SSS1 !! Dynamic-region nitrate/plant source-sink term.
   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: SSS2 !! Dead-space-region nitrate/plant source-sink term.

   DOUBLEPRECISION GCPLA(NCONEE)  !! Scaled chemical-decay coefficient by contaminant.
   DOUBLEPRECISION GGLMSO(NCONEE) !! Input chemical-decay constant by contaminant.

   DOUBLEPRECISION CCAPIN(NCONEE) !! Uniform or link-element initial concentration by contaminant.

   DOUBLEPRECISION ALPHA(NSEE, NCONEE)   !! Exchange coefficient between soil regions by soil and contaminant.
   DOUBLEPRECISION FADS(NSEE, NCONEE)    !! Dynamic-region fraction of adsorption sites by soil and contaminant.
   DOUBLEPRECISION GNN(NCONEE)          !! Freundlich isotherm exponent by contaminant.
   DOUBLEPRECISION KDDLS(NSEDEE, NCONEE) !! Reference distribution coefficient by sediment size and contaminant.
   DOUBLEPRECISION KDDSOL(NSEE, NCONEE)  !! Derived soil distribution coefficient by soil and contaminant.

   INTEGER NCON !! Number of active contaminants.

   DOUBLEPRECISION, DIMENSION(:, :, :, :), ALLOCATABLE :: FCPBKO !! Dynamic-region bank retardation storage.
   DOUBLEPRECISION, DIMENSION(:, :, :, :), ALLOCATABLE :: GCPBKO !! Dead-space-region bank retardation storage.
   DOUBLEPRECISION FSF(NLFEE, NCONEE)  !! Stream-water retardation coefficient by link and contaminant.
   DOUBLEPRECISION FSFC(NLFEE, NCONEE) !! Concentration derivative of `FSF`.
   DOUBLEPRECISION FSFT(NLFEE, NCONEE) !! Time derivative of `FSF`.
   DOUBLEPRECISION RSW(NELEE, NCONEE)  !! Surface-water retardation coefficient by element and contaminant.
   DOUBLEPRECISION RSWC(NELEE, NCONEE) !! Concentration derivative of `RSW`.
   DOUBLEPRECISION RSWT(NELEE, NCONEE) !! Time derivative of `RSW`.

   DOUBLEPRECISION ALPHBD(NCONEE) !! Exchange coefficient between channel-bed layers by contaminant.
   DOUBLEPRECISION ALPHBS(NCONEE) !! Stream-water/channel-bed exchange coefficient by contaminant.

!PRIVATE :: NELEE, NCONEE, LLEE, NSEE, NSEDEE, NLFEE

CONTAINS

!> Allocates and zero-initialises the active contaminant state arrays.
!>
!> [[run_sim:simulation]] calls this routine when contaminant transport is
!> enabled, after [[visualisation_interface_left:get_ncon_early]] has read
!> `NCON` and the model dimensions have been established.
!>
!> | Arrays | Allocated shape | Initial value |
!> |:-------|:----------------|:--------------|
!> | `CCCC`, `CCCCO`, `SSSS`, `SSSSO`, `SSS1`, `SSS2` | `(total_no_elements, top_cell_no + 1, NCON)` | Zero |
!> | `FCPBKO`, `GCPBKO` | `(total_no_links, 2, top_cell_no + 1, NCON)` | Zero |
!>
!> The routine has no dummy arguments and mutates allocatable state in
!> `CONT_CC`. Allocation is unconditional and has no `STAT=` handler, so all
!> eight arrays must be unallocated on entry. The current run path calls the
!> routine once and does not explicitly deallocate these arrays; a second call
!> in the same process would therefore fail unless the caller first deallocates
!> them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | SB | 4.6.1 | Added active-size allocation for contaminant state arrays. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE initialise_cont_cc()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "CONT_CC:initialise_cont_cc"

      allocate (cccc(total_no_elements, top_cell_no + 1, ncon), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CCCC", location, emsg)
      allocate (cccco(total_no_elements, top_cell_no + 1, ncon), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CCCCO", location, emsg)
      allocate (ssss(total_no_elements, top_cell_no + 1, ncon), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "SSSS", location, emsg)
      allocate (sssso(total_no_elements, top_cell_no + 1, ncon), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "SSSFO", location, emsg)
      allocate (sss1(total_no_elements, top_cell_no + 1, ncon), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "SSS1", location, emsg)
      allocate (sss2(total_no_elements, top_cell_no + 1, ncon), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "SSS2", location, emsg)
      allocate (FCPBKO(total_no_links, 2, top_cell_no + 1, ncon), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "FCPBKO", location, emsg)
      allocate (GCPBKO(total_no_links, 2, top_cell_no + 1, ncon), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "GCPBKO", location, emsg)

      ! Initialise to default values
      cccc = 0
      cccco = 0
      ssss = 0
      sssso = 0
      sss1 = 0
      sss2 = 0
      FCPBKO = 0
      GCPBKO = 0

   END SUBROUTINE initialise_cont_cc

END MODULE CONT_CC
