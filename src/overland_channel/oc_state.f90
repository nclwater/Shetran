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
!> @endhistory
MODULE oc_state

   USE array_limits, ONLY: nelee, nlfee, nxee, nyee

   IMPLICIT NONE


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

END MODULE oc_state

