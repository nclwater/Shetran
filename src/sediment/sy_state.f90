!> summary: Sediment yield state: the size-fraction stores, fluxes and balance errors.
!> author: GP, Newcastle University; RJL, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> The state of the sediment transport solution, held per element and per
!> particle-size fraction. `SBERR` is the accumulated balance error, which the
!> sediment driver reports at the end of a run; the remainder of the module,
!> the deposition stores and fluxes, arrives in step 06 from `SED_CS`.
!>
!> Module state is public by default and has no declaration initialization.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C; see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_state

   USE array_limits, ONLY: nelee, NSEDEE

   IMPLICIT NONE

   DOUBLEPRECISION, DIMENSION(NELEE, NSEDEE) :: SBERR !! Sediment balance-error state by element and size fraction.

END MODULE sy_state

