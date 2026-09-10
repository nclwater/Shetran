!> summary: Timestep-reduction requests written by error reporting and read by timestep control.
!> author: GP; AB/RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> Two flags and nothing else. Error reporting sets them and timestep control
!> consumes them, so they cannot live in either: a module holding both the error
!> reporter and these flags would have to be used by timestep control, which the
!> error reporter itself needs. Keeping them alone here is what makes that
!> dependency one-directional.
!>
!> @warning
!> Neither flag has a declaration initializer, and every [[error_reporting:RAISE_ERROR]]
!> call clears both before setting one for error 1024/1030 or 1060.
!> Consequently an intervening error call can erase a pending
!> timestep-reduction request before [[timestep_control:TMSTEP]] consumes it. This is the
!> current behaviour, documented rather than changed.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of sglobal; see docs/rename/proposal.md. |
!> @endhistory
MODULE runtime_flags

   IMPLICIT NONE

   LOGICAL :: flag_runtime_reduction_errors !! Latest `ERROR` call requested timestep reduction for error 1024 or 1030.
   LOGICAL :: flag_runtime_reduction_e1060 !! Latest `ERROR` call requested the separate timestep reduction for error 1060.

END MODULE runtime_flags

