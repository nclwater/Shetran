!> summary: Selected rundata paths, catchment name and command-line run options.
!> author: GP; AB/RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> Where the run's input came from and how it was launched. Command-line setup
!> establishes `DIRQQ`, `cnam` and `rootdir` from the selected rundata file, and
!> sets `error_mode` from the `-error` option; `FROPEN` then fills the three
!> visualisation pathnames from nonblank rundata records 48 to 50.
!>
!> | Name | Written by | Meaning |
!> |:-----|:-----------|:--------|
!> | `DIRQQ`, `cnam`, `rootdir` | [[command_line:get_dir_and_catch]] | Rundata directory, catchment name, and launch directory. |
!> | `filnam` | Command-line setup and [[frame_setup:FROPEN]] | Rundata path, then mutable `FROPEN` record buffer. |
!> | Visualisation filenames | [[frame_setup:FROPEN]] | Nonblank records 48--50 provide plan, check, and HDF5 paths. |
!>
!> `filnam` is a shared mutable buffer, not a stable record of the rundata path:
!> `FROPEN` reuses it for every record it reads.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of sglobal; see docs/rename/proposal.md. |
!> @endhistory
MODULE run_context

   USE MOD_PARAMETERS, ONLY: LENGTH_FILEPATH

   IMPLICIT NONE

   CHARACTER(LEN=LENGTH_FILEPATH) :: DIRQQ !! Parent directory of the selected rundata file; may be `.` or a pathname.
   CHARACTER(LEN=LENGTH_FILEPATH) :: filnam !! Mutable filename buffer used by command-line setup and rundata-record reading.
   CHARACTER(LEN=LENGTH_FILEPATH) :: cnam !! Catchment name derived from the selected rundata filename stem.
   CHARACTER(LEN=LENGTH_FILEPATH) :: rootdir !! Process working directory captured at command-line setup.
   CHARACTER(LEN=LENGTH_FILEPATH) :: hdf5filename !! HDF5 pathname from nonblank rundata record 50.
   CHARACTER(LEN=LENGTH_FILEPATH) :: visualisation_plan_filename !! Visualisation-plan pathname from nonblank rundata record 48.
   CHARACTER(LEN=LENGTH_FILEPATH) :: visualisation_check_filename !! Visualisation-check pathname from nonblank rundata record 49.
   LOGICAL :: error_mode !! State of command-line option `-error`; suppresses the interactive wait in [[error_reporting:ERR_STOP]].

END MODULE run_context

