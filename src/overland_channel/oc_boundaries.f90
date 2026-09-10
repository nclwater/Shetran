!> summary: The overland/channel boundary-condition records and their time series.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> Which elements and faces carry an imposed flow or head boundary, and what
!> that boundary currently is. `NOCBCC` maps an element to its boundary record
!> and `NOCBCD` holds the element, face, boundary type and time-series category
!> of each record.
!>
!> The module is separate from [[oc_state]] because the boundary series are
!> read and advanced independently of the flow solution, and separate from
!> [[oc_input]] because the solver reads them every step. Module state is
!> public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_D; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_boundaries

   USE array_limits, ONLY: nelee, NOCTAB

   IMPLICIT NONE

   INTEGER :: NOCBCC(NELEE)       !! Overland/channel boundary-condition record number by element.
   INTEGER :: NOCBCD(NOCTAB,4)    !! OC boundary records: element, face, boundary type, and time-series category.

END MODULE oc_boundaries

