!> summary: The legacy floating-point-trap entry point.
!> author: AB / RAH, Newcastle University; J. Ewen, Newcastle University; Sven Berendsen
!>
!> [[ALTRAP]] is a startup compatibility entry point kept for the historical
!> call from the program unit. It is not input handling, which is why it is
!> here rather than with the readers it used to sit beside.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of mod_load_filedata; see docs/rename/proposal.md. |
!> @endhistory
MODULE platform_traps

   USE MOD_PARAMETERS, ONLY: I_P
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_warn

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: ALTRAP

CONTAINS

   !> Preserves the legacy startup hook for floating-point trap configuration.
   !>
   !> [[shetran]] calls `ALTRAP` once after command-line processing and before
   !> opening model files. The former platform-specific `IEEE_HANDLER` call has
   !> been commented out since version 4g-pc. The current routine sets its local
   !> status to zero and returns, so it enables no IEEE exceptions and can never
   !> issue its retained warning 13.
   !>
   !> @note
   !> The interface was removed as a no-op in April 2026 and restored during the
   !> May rebase because the main program still called it. Its presence must not
   !> be interpreted as active floating-point exception trapping.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-09-30 | RAH | 3.4.1 | Created the floating-point trap setup hook (legacy SSR79). |
   !> | 2000-03-07 | StevenB | 4g-pc | Removed the platform-specific IEEE handler calls. |
   !> | 2026-04-04 | SvB | - | Removed the no-op routine. |
   !> | 2026-05-11 | SvB | - | Restored the public no-op interface during the current-code rebase. |
   !> @endhistory
   SUBROUTINE ALTRAP()

      ! Locals, etc
      INTEGER(kind=I_P), parameter :: OUT = 0 !! Retained diagnostic unit for the unreachable warning path.

      INTEGER(kind=I_P) :: I !! Legacy trap-setup status, unconditionally set to zero.

      ! Code -----------------------------------------------------------------

      !   I = IEEE_HANDLER( 'set', 'common', ABORT )
      I = 0
      IF (I .NE. 0) CALL RAISE_ERROR(ERRLVL_warn, 13, OUT, 0, 0, &
                                     'Could not set traps for floating-point exceptions')

      RETURN
   END SUBROUTINE ALTRAP

END MODULE platform_traps

