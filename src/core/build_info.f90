!> summary: Version number, development flag and banner text.
!> author: GP; AB/RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> Four compile-time values identifying the build. `frame_setup` prints the
!> banner and version to the PRI output and writes the version to the legacy
!> binary results file; `RUNFIL` is the historical rundata filename prefix.
!>
!> @note
!> `SHEVER` is a legacy numeric major/minor value printed in the PRI output and
!> written to the binary results file. It does not encode the full project patch
!> version. `RUNFIL` is still passed to command-line setup as the historical
!> rundata prefix, but that routine does not currently read the argument.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of sglobal; see docs/rename/proposal.md. |
!> @endhistory
MODULE build_info

   USE MOD_PARAMETERS, ONLY: R8P

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SHEVER, BDEVER, BANNER, RUNFIL

   REAL(KIND=R8P), PARAMETER :: SHEVER = 4.7_R8P !! SHETRAN version number (Major.Minor format).
   LOGICAL, PARAMETER :: BDEVER = .TRUE. !! Development version flag. `.TRUE.` for development, `.FALSE.` for release.
   CHARACTER(*), PARAMETER :: BANNER = 'SHETRAN Hydrological Model' !! Banner for local implementation.
   CHARACTER(*), PARAMETER :: RUNFIL = 'rundata_' !! Base filename for run data files.

END MODULE build_info

