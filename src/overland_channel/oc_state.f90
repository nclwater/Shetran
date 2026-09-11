!> summary: Overland and channel flow state: face discharges, the current step and the flow derivatives.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> What the overland and channel solver carries from one step to the next: the
!> signed discharge through every element face, the channel cross-sectional
!> area, the topology code grids that say which faces are channel and which
!> overland, and the flow derivatives the implicit solve forms. `OCNOW` and
!> `OCNEXT` bound the current overland/channel step, which is shorter than and
!> nested inside the coupled model timestep.
!>
!> Discharges are volumetric (m3/s), positive into the indexed element.
!> `QMAX` is a diagnostic limit rather than state: a nonpositive value disables
!> the check. Module state is public by default and, apart from parameter
!> constants, has no declaration initialization.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C, AL_D; see docs/rename/proposal.md. |
!> | 2026-09-11 | SvB | - | Gained the Strickler roughness grids, `dtoc`, and the `HRFZZ`/`qsazz`/`xstab` arrays behind the abstracted accessors. |
!> @endhistory
MODULE oc_state

   USE array_limits, ONLY: nelee, nlfee, nxee, nyee

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: QOC, ARXL, QMAX, OCNOW, OCNEXT, LCODEX, LCODEY, DQ0ST, DQIST, DQIST2, STRXX, STRYY, dtoc, &
             HRFZZ, qsazz, xstab


! Face discharge and channel cross-section.
   DOUBLEPRECISION, DIMENSION(NELEE, 4) :: QOC  !! Signed overland/channel discharge through each element face (m3/s).
   DOUBLEPRECISION, DIMENSION(NLFEE) :: ARXL    !! Current channel-flow cross-sectional area by link (m2).
   DOUBLEPRECISION :: QMAX    !! Maximum permitted overland/channel face discharge magnitude (m3/s); nonpositive disables the check.
   DOUBLEPRECISION :: OCNOW  !! Start time of the current overland/channel step (h).
   DOUBLEPRECISION :: OCNEXT !! Duration of the current overland/channel step (h).

! Face topology codes and the implicit-solve derivatives.
   INTEGER :: LCODEX(NXEE,NYEE)   !! X-face overland/channel topology code grid.
   INTEGER :: LCODEY(NXEE,NYEE)   !! Y-face overland/channel topology code grid.
   DOUBLEPRECISION :: DQ0ST(NELEE,4) !! Face-flow derivative with respect to the local element state.
   DOUBLEPRECISION :: DQIST(NELEE,4) !! Face-flow derivative with respect to the adjacent element state.
   DOUBLEPRECISION :: DQIST2(NLFEE,3) !! Confluence-flow derivative by branch record and branch position.


! Solver working state and the abstracted accessors' storage (from OCmod2, OCQDQMOD, OCmod).
   DOUBLEPRECISION    :: STRXX(NELEE)      !! X-direction Strickler roughness, or negative storage-depth marker.
   DOUBLEPRECISION    :: STRYY(NELEE)      !! Y-direction Strickler roughness.
   DOUBLEPRECISION    :: dtoc                   !! OC timestep in seconds.
   DOUBLEPRECISION, DIMENSION(NELEE)          :: HRFZZ    !! Water-surface elevation by element; abstracted for AD and solver access.
   DOUBLEPRECISION, DIMENSION(NELEE, 4)        :: qsazz    !! Face discharge by element and face; positive into the indexed element.
   DOUBLEPRECISION, DIMENSION(:, :, :), ALLOCATABLE :: xstab
   !! Channel lookup table: depth, conveyance, and conveyance slope by row and link.
   !! Allocated once by [[initialise_ocmod]] to shape `(3,NXSCEE,total_no_links)`.

END MODULE oc_state

