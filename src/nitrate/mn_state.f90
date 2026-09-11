!> summary: Carbon and nitrogen pools, process rates and the per-cell nitrate state.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> Everything the nitrate component carries between timesteps: the litter,
!> humus and manure carbon pools with their nitrogen counterparts, the ammonium
!> and nitrate stores, the rate constants and the environmental response
!> factors, and the three derived types that hold the configuration, the
!> workspace and the plant state.
!>
!> The pools are indexed by element and vertical cell. A name ending in `1` is
!> the previous-timestep value of the name without it, which is how the
!> component forms its increments. Module state is public by default; it was
!> private inside `MNmod`, and has to be visible now that the routines that
!> read it are in eight sibling modules.
!>
!> @warning
!> The module has retained state and no reset or deallocation path, so it is
!> not re-entrant and assumes one model run with fixed dimensions per process.
!>
!> `NPL`, `NPLTYP`, `GMCPBB`, `PFONE` and `GMCBBO` are also declared, in
!> separate storage, by [[cm_plant_state]], and `GMCPBB` means different things
!> on the two sides. Both copies are kept unchanged; see
!> `docs/todo/issue_plant_declarations.md`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> | 2026-09-10 | SvB | - | Split out of MNmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mn_state

   USE array_limits, ONLY: nelee, NPELEE, NPLTEE, NSEE

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: MN_CONFIG_TYPE, MN_WORKSPACE_TYPE, MN_PLANT_STATE_TYPE, cahum, calit, caman, cdort, chum, &
             chum1, clit, clit1, cman, cman1, denit, dummy4, dummy6, edeth, emph, emt, enph, ent, gam, &
             gamtmp, imamm, imdiff, imnit, isimtf, kd1, kd2, khum, klit, kman, knit, kvol, miner, &
             naamm, namm, namm1, nanit, ndnit, ndsnt, nlit, nlit1, nman, nman1, ntrf, plamm, plnit, &
             plup, pphi, snit, temp, vol, MN_PLANT_NVALEE, MN_CONFIG, MN_WORK, MN_PLANT_STATE, &
             MN_INITIALISED, MN_ALLOCATED_NEL, MN_ALLOCATED_NCETOP

   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: cahum  !! External carbon-addition rate assigned to humus.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: calit  !! External carbon-addition rate assigned to litter.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: caman  !! External carbon-addition rate assigned to manure.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: cdort  !! Carbon-dioxide production rate from organic-matter turnover.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: chum   !! Humus carbon at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: chum1  !! Updated humus carbon.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: clit   !! Litter carbon at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: clit1  !! Updated litter carbon.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: cman   !! Manure carbon at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: cman1  !! Updated manure carbon.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: denit  !! Denitrification loss rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: dummy4 !! Transposed element/cell workspace for MN input checks.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: dummy6 !! Element/cell workspace for MN input checks.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: edeth  !! Water-content response factor for denitrification.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: emph   !! Matric-potential response factor for mineralisation.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: emt    !! Temperature response factor for mineralisation.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: enph   !! Matric-potential response factor for nitrification.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: ent    !! Temperature response factor for nitrification.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: gam    !! Net mineralisation rate after deficit adjustment.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: gamtmp !! Unadjusted net mineralisation rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: imamm  !! Ammonium immobilisation rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: imdiff !! Unmet immobilisation demand carried to the next timestep.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: imnit  !! Nitrate immobilisation rate.
   LOGICAL, DIMENSION(:, :), ALLOCATABLE :: isimtf        !! Whether an immobilisation deficit suppresses litter/manure turnover.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: kd1    !! Denitrification carbon-demand coefficient.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: kd2    !! Denitrification nitrate-availability coefficient.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: khum   !! Humus decomposition-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: klit   !! Litter decomposition-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: kman   !! Manure decomposition-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: knit   !! Nitrification-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: kvol   !! Ammonia-volatilisation-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: miner  !! Gross mineralisation rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: naamm  !! Ammonium addition/deposition rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: namm   !! Ammonium concentration at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: namm1  !! Updated ammonium concentration.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: nanit  !! Nitrate addition/deposition rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: ndnit  !! Dimensional nitrate concentration in dynamic water.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: ndsnt  !! Dimensional nitrate concentration in dead-space water.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: nlit   !! Litter nitrogen at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: nlit1  !! Updated litter nitrogen.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: nman   !! Manure nitrogen at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: nman1  !! Updated manure nitrogen.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: ntrf   !! Nitrification rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: plamm  !! Actual ammonium plant-uptake rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: plnit  !! Actual nitrate plant-uptake rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: plup   !! Potential plant-nitrogen-uptake rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: pphi   !! Dynamic-water fraction used to partition uptake.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: snit   !! Total nitrate source/sink diagnostic rate.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: temp   !! Soil temperature used by MN response factors.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: vol    !! Ammonia-volatilisation loss rate.
   INTEGER, PARAMETER :: MN_PLANT_NVALEE = 30
   TYPE :: MN_CONFIG_TYPE
      INTEGER :: NBOTCE
      DOUBLE PRECISION :: AMMDDR, AMMWDR, CNRBIO, CNRHUM, FE, FH, GNN
      DOUBLE PRECISION :: KPLAMM, KPLNIT, KUAMM, KUNIT, MNCREF, NITDDR, NITWDR
      DOUBLE PRECISION :: Q10M, Q10N
      DOUBLE PRECISION :: KDDSOL(NSEE)
      LOGICAL :: ISBOTC, ISQ10
   END TYPE MN_CONFIG_TYPE

   TYPE :: MN_WORKSPACE_TYPE
      INTEGER, ALLOCATABLE :: IDUM(:)
      LOGICAL, ALLOCATABLE :: LDUM(:)
      DOUBLE PRECISION, ALLOCATABLE :: DUMMY(:)
      DOUBLE PRECISION, ALLOCATABLE :: CDPTHB(:), CLTFCT(:), CMNFCT(:)
      DOUBLE PRECISION, ALLOCATABLE :: CNRAL(:), CNRALT(:), CNRAM(:), CNRAMN(:)
      DOUBLE PRECISION, ALLOCATABLE :: CTOT(:), NAMFCT(:), NDPTHB(:), NTOT(:)
   END TYPE MN_WORKSPACE_TYPE

   TYPE :: MN_PLANT_STATE_TYPE
      INTEGER :: NVALUE(NPLTEE)
      INTEGER :: NPL(NELEE), NPLTYP(NELEE, NPELEE)
      DOUBLE PRECISION :: CDI(NPLTEE, MN_PLANT_NVALEE), CDIT(NPLTEE, MN_PLANT_NVALEE)
      DOUBLE PRECISION :: CLAIMX(NPLTEE)
      DOUBLE PRECISION :: CROPTM(NELEE, NPELEE), GMCPBB(NELEE, NPELEE)
      DOUBLE PRECISION :: MASSB(NELEE, NPELEE), PFONE(NELEE, NPELEE)
      LOGICAL :: ISCROP(NELEE, NPELEE)
   END TYPE MN_PLANT_STATE_TYPE

   TYPE(MN_CONFIG_TYPE) :: MN_CONFIG
   TYPE(MN_WORKSPACE_TYPE) :: MN_WORK
   TYPE(MN_PLANT_STATE_TYPE) :: MN_PLANT_STATE
   LOGICAL :: MN_INITIALISED = .FALSE.
   INTEGER :: MN_ALLOCATED_NEL = 0, MN_ALLOCATED_NCETOP = 0

END MODULE mn_state

