!> summary: Snowpack depth, temperature, density and the meltwater slugs in transit.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> The condition of the snowpack on each element: whether there is one, how
!> deep it is, its temperature and relative density, and how many meltwater
!> slugs are still travelling through it. [[snowmelt]] advances all of it;
!> [[et_process]] reads `ISPACK` and the coupling control to decide whether an
!> element is snow-covered, and the visualisation interface reads the depth.
!>
!> Depths are in mm of snow, not mm of water; `RHOSAR` is a relative density
!> (specific gravity), so the water equivalent is the product of the two.
!> Module state is public by default and has no declaration initialization.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C, AL_D; see docs/rename/proposal.md. |
!> @endhistory
MODULE snow_state

   USE array_limits, ONLY: nelee

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: ISPACK, MSM, NSMT, NSMC, RHOSAR, SD, TS, SF, smelt, tmelt


! Per-element snowpack presence and the melt-coupling control.
   LOGICAL, DIMENSION(NELEE) :: ISPACK !! Whether a snowpack is present on each element.
   INTEGER :: MSM        !! Snowmelt method: 0 disabled, 1 degree-day, or 2 energy-budget.
   INTEGER :: NSMT   !! Current ET/snowmelt coupling control for one element.
   INTEGER :: NSMC(NELEE) !! Number of meltwater slugs still travelling through each snowpack.

! Snowpack density, depth, temperature and current snowfall.
   DOUBLEPRECISION :: RHOSAR(NELEE)  !! Snow relative-density/specific-gravity state by element.
   DOUBLEPRECISION :: SD(NELEE)     !! Snowpack depth by element (mm of snow).
   DOUBLEPRECISION :: TS(NELEE)     !! Snowpack temperature by element (degrees C).
   DOUBLEPRECISION :: SF(NELEE)     !! Current snowfall depth by element (mm of snow).


! Meltwater slugs still travelling through the pack (from SMmod).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: smelt !! Routed meltwater slugs by slug number and element (mm water).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: tmelt !! Release time for each routed meltwater slug (h).

END MODULE snow_state

