!> summary: The two shared scratch arrays the legacy input path passes around.
!> author: GP, Newcastle University; RJL; RAH; JE, Newcastle University; SB, Newcastle University
!>
!> `IDUM` and `DUMMY` are scratch buffers, not model state: a reader fills one,
!> an expansion routine reads it, and the contents mean nothing between calls.
!> They are module variables only because the legacy readers pass them from one
!> caller to the next, and they are here — alone, in a module that does nothing
!> else — so that being scratch is the first thing visible about them.
!>
!> They should become local variables passed explicitly; that is a change to
!> the reader interfaces and is deliberately not part of this move.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C; see docs/rename/proposal.md. |
!> @endhistory
MODULE input_workspace

   USE array_limits, ONLY: nelee, nxee, nyee

   IMPLICIT NONE

   INTEGER, DIMENSION(NXEE*NYEE) :: IDUM !! Integer workspace for spatial/category input.
   DOUBLEPRECISION, DIMENSION(NELEE) :: DUMMY  !! Floating-point workspace for spatial input and validation.

END MODULE input_workspace

