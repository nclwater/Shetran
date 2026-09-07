!> @brief Resolves the rundata file, catchment name, and working directories.
!> @author Stephen Birkinshaw, Newcastle University
!> @author Sven Berendsen, Newcastle University
!>
!> `GETDIRQQ` implements the command-line selection stage used once by
!> [[shetran]] before any model file is opened. Its sole public procedure,
!> [[get_dir_and_catch]], validates a direct filename or a `catchments.txt`
!> lookup and returns the normalized rundata path, its directory, a derived
!> catchment name, and the launch working directory. All helper procedures and
!> the optional dialog buffer are private.
!>
!> | Build and invocation | Current selection behavior |
!> |:---------------------|:---------------------------|
!> | Any build, `-f <path>` | Select the named rundata file. |
!> | Any build, `-c [name]` | Look up a catchment name in `catchments.txt`. |
!> | Intel Fortran QuickWin on Windows, no arguments or `-a` | Open the native file-selection dialog. |
!> | Other builds, no arguments or `-a` | Print portable usage text and stop with status 1. |
!>
!> QuickWin support exists only when CMake enables `SHETRAN_HAVE_QUICKWIN`,
!> which currently requires `ENABLE_QUICKWIN`, Windows, and Intel Fortran.
!> Ordinary builds depend only on Fortran `GET_COMMAND_ARGUMENT` and
!> `stdlib_system` path routines.
!>
!> @warning
!> The user manual still says that a no-argument run opens a dialog on every
!> build and that a bare filename is accepted without `-f`. Neither is true for
!> the current portable build. The manual also assigns interactive-wait behavior
!> to `-error`, but the current flag has no consumer and cannot change stopping
!> behavior.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-03-05 | SvB | - | Formatted and cleaned the original Intel-specific selector. |
!> | 2026-04-01 | SvB | - | Replaced the entry workflow with portable command-argument handling. |
!> | 2026-05-28 | SB | - | Generalised the selector to work across compilers and operating systems, keeping the Intel/Windows `-a` popup available. |
!> | 2026-06-08 | SvB | - | Adopted `stdlib_system` paths and made Intel QuickWin conditional. |
!> | 2026-06-19 | SB | 4.6.4 | Revised cross-platform command-line selection and diagnostics. |
!> | 2026-07-08--11 | SteveB / SvB | 4.6.4 | Reconciled dialog and direct-file results and restored `join_path`. |
!> @endhistory
MODULE GETDIRQQ

   USE mod_parameters
   USE sglobal, ONLY : error_mode
   USE stdlib_system, ONLY : base_name, dir_name, get_cwd, join_path

#ifdef SHETRAN_HAVE_QUICKWIN
   USE IFWIN
#endif

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: get_dir_and_catch
   PUBLIC :: rundata_from_file_dialog

   !> Whether the rundata file was chosen through the QuickWin file dialog
   !> rather than named on the command line. Only such a run owns a console
   !> window that vanishes on exit, so only such a run needs the closing delay
   !> in [[shetran]]. Always `.FALSE.` in a build without QuickWin support.
   LOGICAL, PROTECTED :: rundata_from_file_dialog = .FALSE.

#ifdef SHETRAN_HAVE_QUICKWIN
   CHARACTER(LEN=LENGTH_FILEPATH) :: FileName !! NUL-terminated QuickWin dialog filename buffer.
#endif

CONTAINS

   !> @brief Selects and validates the rundata file and derives run identity paths.
   !>
   !> This is the only public module procedure and is called by [[shetran]] at
   !> process startup. `runfil` is a retained, unread compatibility argument.
   !> Before examining the command line the routine stores the launch working
   !> directory in `rootdir`, clears `SGLOBAL:error_mode`, and counts arguments
   !> with the standard Fortran command-line intrinsics.
   !>
   !> | First argument | Current action |
   !> |:---------------|:---------------|
   !> | `-f` | Require argument 2 and use it as the rundata path. |
   !> | `-c` | Search `catchments.txt` for argument 2, or for `default` when absent. |
   !> | `-a`, QuickWin build | Open a Windows file dialog filtered for `*rundata*.txt` and all files. |
   !> | no arguments, QuickWin build | Synthesize `-a` and open that dialog. |
   !> | no arguments, non-QuickWin build | Synthesize `-f`, diagnose its missing filename, and stop. |
   !> | Legacy dialog aliases, non-QuickWin build | Diagnose that interactive selection requires Intel QuickWin and stop. |
   !> | Anything else | Report an unrecognized option and stop. |
   !>
   !> The legacy dialog aliases are `-m`, `-af`, `-sd`, `-pattern`, `-delinc`,
   !> and `-results`. A non-QuickWin build diagnoses these aliases together with
   !> `-a`; in a QuickWin build only `-a` is recognized and the aliases reach the
   !> default unrecognized-option error.
   !>
   !> `catchments.txt` is resolved relative to the launch working directory,
   !> despite one diagnostic calling it the executable directory. The file is
   !> read on fixed unit 875 as alternating records: a character catchment key,
   !> then a list-directed rundata path. Keys are matched case-sensitively after
   !> trimming. Open errors, incomplete pairs, EOF without a match, and a missing
   !> key all terminate through the contained `print_usage_and_stop` helper.
   !>
   !> After selection, `INQUIRE` must confirm that the rundata file exists. A
   !> basename-only input produces `dirqq='.'`; otherwise `dir_name` and
   !> `base_name` split the path. `join_path` reconstructs `fn` with the platform
   !> separator, while `dirqq` deliberately has no appended separator. The
   !> private `derive_catch_from_filename` helper removes the
   !> final extension and an exact lowercase `rundata_` prefix when present.
   !> Unlike the old branch, other filename stems are accepted as catchment names.
   !>
   !> In a QuickWin build, a successful dialog result is copied only through its
   !> first NUL character. [[comdlger]] handles a nonzero extended dialog error;
   !> an ordinary cancel has `bret=.FALSE.` and stops through the usage helper.
   !>
   !> @warning
   !> The contained `set_error_mode_from_arguments` helper recognizes the exact,
   !> case-sensitive token `-error` anywhere in the arguments, but it runs only
   !> after the mode-specific selection block. Several early failures therefore
   !> stop before the flag is scanned. More importantly, no current routine reads
   !> `error_mode`, so setting it has no observable effect on termination.
   !> @endwarning
   !>
   !> @note
   !> All returned character values use caller-provided fixed-length buffers.
   !> Ordinary Fortran blank padding or truncation applies; this routine does not
   !> diagnose a caller buffer that is too short.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 2020-03-05 | SvB | - | Formatted and cleaned the original selector. |
   !> | 2026-04-01--13 | SvB | - | Replaced Intel-only arguments and paths, removed labels, and restored catchment derivation. |
   !> | 2026-05-08 | SB | - | Added the retained `-error` command-line flag. |
   !> | 2026-05-28 | SB | - | Generalised the routine to work across compilers and operating systems, keeping the Intel/Windows `-a` popup available. |
   !> | 2026-06-08 | SvB | - | Switched to `stdlib_system`, conditional QuickWin, and standard command-line intrinsics. |
   !> | 2026-06-19 | SB | 4.6.4 | Updated option validation and catchment lookup handling. |
   !> | 2026-07-08--11 | SteveB / SvB | 4.6.4 | Reconciled direct and dialog paths and restored `join_path`. |
   !> @endhistory
   SUBROUTINE get_dir_and_catch(runfil, fn, catch, dirqq, rootdir)

      CHARACTER(LEN=*), INTENT(IN)  :: runfil  !! Retained historical argument; not read.
      CHARACTER(LEN=*), INTENT(OUT) :: fn      !! Validated rundata path reconstructed with `join_path`.
      CHARACTER(LEN=*), INTENT(OUT) :: catch   !! Final filename stem with a lowercase `rundata_` prefix removed.
      CHARACTER(LEN=*), INTENT(OUT) :: dirqq   !! Rundata directory without an appended separator, or `.`.
      CHARACTER(LEN=*), INTENT(OUT) :: rootdir !! Launch working directory, or `.` when `get_cwd` fails.

      CHARACTER(LEN=*), PARAMETER :: catchment_file = 'catchments.txt' !! Launch-directory lookup filename.
      CHARACTER(LEN=LENGTH_LINE) :: message !! Deferred command-line diagnostic.
      CHARACTER(LEN=LENGTH_LINE) :: dum1    !! Catchment key read from the lookup file.
      CHARACTER(LEN=LENGTH_LINE) :: dum2    !! Rundata path read from the lookup file.
      CHARACTER(LEN=LENGTH_LINE) :: code    !! First argument or synthesized default mode.
      CHARACTER(LEN=LENGTH_FILEPATH) :: cli_argument !! Selected path or catchment key while resolving it.
      CHARACTER(LEN=LENGTH_FILEPATH) :: fn_part      !! Basename of the selected rundata path.
      LOGICAL :: ex                !! File-existence result.
      LOGICAL :: found_catchment   !! Whether the requested lookup key was matched.
      INTEGER :: ios               !! Lookup-file I/O status.
      INTEGER :: na                !! Number of command-line arguments.
#ifdef SHETRAN_HAVE_QUICKWIN
      INTEGER(KIND=I_P) :: ierror !! Extended Windows common-dialog error code.
      LOGICAL(KIND=4) :: bret     !! Whether the Windows file dialog selected a file.
      CHARACTER(LEN=LENGTH_FILEPATH) :: allfilters !! NUL-delimited dialog filters.
      CHARACTER(LEN=60) :: dlgtitle !! NUL-terminated dialog title.
      TYPE(T_OPENFILENAME) :: opn    !! Windows common-file-dialog configuration.
      INTEGER :: null_pos            !! First NUL position in the selected filename.
#endif

      CALL get_current_dir(rootdir)

      error_mode = .FALSE.
      na = COMMAND_ARGUMENT_COUNT()

      IF (na > 0) THEN
         CALL GET_COMMAND_ARGUMENT(1, code)
      ELSE
#ifdef SHETRAN_HAVE_QUICKWIN
            code = '-a'  !popup window is default if there is a fortran compiler on Windows
#else
            code = '-f'  !otherwise filename is default and user must provide it as an argument
#endif
        ENDIF

      message = ''

      SELECT CASE(TRIM(code))
#ifdef SHETRAN_HAVE_QUICKWIN
      CASE('-a')
         FileName = CHAR(0)
         allfilters = 'rundata files (*rundata*.txt)' // CHAR(0) // '*rundata*.txt' // CHAR(0) // &
                       'All files (*.*)' // CHAR(0) // '*.*' // CHAR(0) // CHAR(0)
         dlgtitle = 'Select a SHETRAN rundata file'C

         opn%lStructSize       = 0
         opn%HWNDOWNER         = NULL
         opn%HINSTANCE         = NULL
         opn%LPSTRFILTER       = NULL
         opn%LPSTRCUSTOMFILTER = NULL
         opn%NMAXCUSTFILTER    = 0
         opn%NFILTERINDEX      = 0
         opn%LPSTRFILE         = NULL
         opn%NMAXFILE          = 0
         opn%LPSTRFILETITLE    = NULL
         opn%NMAXFILETITLE     = 0
         opn%LPSTRINITIALDIR   = NULL
         opn%LPSTRTITLE        = NULL
         opn%FLAGS             = 0
         opn%NFILEOFFSET       = 0
         opn%NFILEEXTENSION    = 0
         opn%LPSTRDEFEXT       = NULL
         opn%LCUSTDATA         = 0
         opn%LPFNHOOK          = NULL
         opn%LPTEMPLATENAME    = NULL
         opn%PVRESERVED        = NULL
         opn%DWRESERVED        = 0
         opn%FLAGSEX           = 0

         opn%lStructSize       = SIZEOF(opn)
         opn%LPSTRFILTER       = LOC(allfilters)
         opn%NFILTERINDEX      = 1
         opn%LPSTRFILE         = LOC(FileName)
         opn%NMAXFILE          = LEN(FileName)
         opn%LPSTRTITLE        = LOC(dlgtitle)
         opn%FLAGS             = OFN_EXPLORER + OFN_FILEMUSTEXIST + OFN_PATHMUSTEXIST + OFN_NOCHANGEDIR

         bret = GETOPENFILENAME(opn)
         CALL comdlger(ierror)

         IF (.NOT. bret) THEN
            CALL print_usage_and_stop('No rundata file selected')
         END IF

         null_pos = INDEX(FileName, CHAR(0))
         IF (null_pos > 1) THEN
            cli_argument = FileName(1:null_pos - 1)
         ELSE
            cli_argument = FileName
         END IF

         ! The dialog supplied the file, so this run owns a console window that
         ! closes on exit. Ask [[shetran]] to pause before it does.
         rundata_from_file_dialog = .TRUE.
#endif

      CASE('-f')
         IF (na < 2) THEN
            CALL print_usage_and_stop('Missing filename. Usage: shetran -f filename.txt')
         END IF
         CALL GET_COMMAND_ARGUMENT(2, cli_argument)

      CASE('-c')
         IF (na < 2) THEN
            cli_argument = 'default'
         ELSE
            CALL GET_COMMAND_ARGUMENT(2, cli_argument)
         END IF

         INQUIRE(FILE=catchment_file, EXIST=ex)
         IF (ex) THEN
            OPEN(UNIT=875, FILE=catchment_file, STATUS='OLD', IOSTAT=ios)
            IF (ios /= 0) CALL print_usage_and_stop('Error reading catchment file')

            found_catchment = .FALSE.

            read_catchment: DO
               READ(875, '(A)', IOSTAT=ios) dum1
               IF (ios /= 0) EXIT read_catchment

               READ(875, *, IOSTAT=ios) dum2
               IF (ios /= 0) EXIT read_catchment

               IF (TRIM(dum1) == TRIM(cli_argument)) THEN
                  cli_argument = dum2
                  found_catchment = .TRUE.
                  EXIT read_catchment
               END IF
            END DO read_catchment

            CLOSE(875)

            IF (.NOT. found_catchment) THEN
               CALL print_usage_and_stop('Cannot find catchment ' // TRIM(cli_argument) // ' in ' // catchment_file)
            END IF
         ELSE
            message = 'Cannot find file ' // catchment_file // ' in executable directory'
         END IF

#ifndef SHETRAN_HAVE_QUICKWIN
      CASE('-a', '-m', '-af', '-sd', '-pattern', '-delinc', '-results')
         CALL print_usage_and_stop('Interactive file selection requires Intel QuickWin on Windows. Use: shetran -f filename.txt')
#endif

      CASE DEFAULT
         message = 'Unrecognised command line argument ' // TRIM(code) // '. Recognise only -a, -c and -f'
      END SELECT

      CALL set_error_mode_from_arguments()

      IF (message /= '') CALL print_usage_and_stop(message)

      INQUIRE(FILE=cli_argument, EXIST=ex)
      IF (.NOT. ex) THEN
         IF (LEN_TRIM(cli_argument) == 0) THEN
            message = 'Missing filename. Use: shetran -f filename.txt'
         ELSE
            message = 'Cannot find rundata file ' // TRIM(cli_argument)
         END IF
         CALL handle_command_line_error(message)
      END IF

      IF (INDEX(cli_argument, '/') == 0 .AND. INDEX(cli_argument, '\') == 0) THEN
         dirqq = '.'
         fn_part = TRIM(cli_argument)
      ELSE
         dirqq = dir_name(TRIM(cli_argument))
         fn_part = base_name(TRIM(cli_argument))
      END IF

      ! Reconstruct the validated rundata path with the platform separator.
      ! DIRQQ remains a directory path without an appended separator.
      fn = join_path(TRIM(dirqq), TRIM(fn_part))
      catch = derive_catch_from_filename(fn_part)

   CONTAINS

      !> @brief Derives a catchment name from the selected rundata basename.
      !>
      !> The final dot and following extension are removed only when the dot is
      !> beyond position 1. If the remaining stem is longer than eight characters
      !> and begins with the exact lowercase prefix `rundata_`, that prefix is
      !> removed. Every other stem, including mixed-case prefixes and dotfiles,
      !> is returned unchanged after trimming. The function neither validates the
      !> extension nor requires the legacy prefix.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2026-04-13 | SvB | Extracted catchment-name derivation while removing labelled control flow. |
      !> | 2026-07-08 | SB | Applied derivation to the basename so directory names do not affect the catchment. |
      !> @endhistory
      FUNCTION derive_catch_from_filename(filename) RESULT(catch_name)
         CHARACTER(LEN=*), INTENT(IN) :: filename !! Rundata basename, normally including its extension.
         CHARACTER(LEN=LENGTH_FILEPATH) :: catch_name !! Derived fixed-buffer catchment name.
         CHARACTER(LEN=LENGTH_FILEPATH) :: stem !! Working basename stem.
         INTEGER :: dot_pos !! Position of the final extension separator.

         stem = TRIM(filename)
         dot_pos = INDEX(stem, '.', BACK=.TRUE.)
         IF (dot_pos > 1) stem = stem(1:dot_pos - 1)

         IF (LEN_TRIM(stem) > 8) THEN
            IF (stem(1:8) == 'rundata_') THEN
               catch_name = TRIM(stem(9:))
               RETURN
            END IF
         END IF

         catch_name = TRIM(stem)
      END FUNCTION derive_catch_from_filename

      !> @brief Prints a startup selection error and portable usage, then stops.
      !>
      !> The supplied message is prefixed with `ERROR:`. Two usage lines describe
      !> the supported `-f` and `-c` forms, after which `STOP 1` terminates the
      !> process. This helper handles option, dialog-cancel, and catchment-lookup
      !> failures that occur before final file-existence validation.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2026-04-06 | SvB | Replaced the shared terminal-label error path with a contained helper. |
      !> | 2026-06-08 | SvB | Updated the helper for the portable `-f`/`-c` interface. |
      !> @endhistory
      SUBROUTINE print_usage_and_stop(err_msg)
         CHARACTER(LEN=*), INTENT(IN) :: err_msg !! Specific command-line or lookup failure.

         WRITE(*, '(A)') 'ERROR: ' // TRIM(err_msg)
         WRITE(*, '(A)') 'Usage: shetran -f rundata_file.txt'
         WRITE(*, '(A)') '   or: shetran -c catchment_name'
         STOP 1
      END SUBROUTINE print_usage_and_stop

      !> @brief Scans command arguments for the retained `-error` flag.
      !>
      !> Every argument from 1 through the host-associated count `na` is read.
      !> An exact case-sensitive token `-error` sets `SGLOBAL:error_mode` true;
      !> other tokens leave it unchanged. [[get_dir_and_catch]] clears the flag
      !> before selection and calls this helper after its option-specific work.
      !>
      !> No current source reads `error_mode`, so this mutation is retained for
      !> compatibility but has no observable runtime effect.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2026-05-08 | SB | Added the `-error` option to command-line setup. |
      !> | 2026-06-08 | SvB | Reworked detection to scan the standard Fortran argument list. |
      !> @endhistory
      SUBROUTINE set_error_mode_from_arguments()
         INTEGER :: arg_index !! One-based command-argument index.
         CHARACTER(LEN=LENGTH_LINE) :: argument !! Argument currently being tested.

         DO arg_index = 1, na
            CALL GET_COMMAND_ARGUMENT(arg_index, argument)
            SELECT CASE(TRIM(argument))
            CASE('-error')
               error_mode = .TRUE.
            END SELECT
         END DO
      END SUBROUTINE set_error_mode_from_arguments

   END SUBROUTINE get_dir_and_catch

   !> @brief Returns the launch working directory through Fortran stdlib.
   !>
   !> `stdlib_system:get_cwd` returns an allocatable string. When that string is
   !> allocated, its value is assigned to the caller's fixed-length buffer;
   !> otherwise the routine returns `.` as a portable current-directory fallback.
   !> The routine does not change the process working directory.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-06-08 | SvB | Replaced Intel drive-directory inquiry with Fortran stdlib. |
   !> @endhistory
   SUBROUTINE get_current_dir(current_dir)

      CHARACTER(LEN=*), INTENT(OUT) :: current_dir !! Launch directory in the caller's fixed buffer.
      CHARACTER(LEN=:), ALLOCATABLE :: cwd !! Working directory allocated by `get_cwd`.

      CALL get_cwd(cwd)

      IF (ALLOCATED(cwd)) THEN
         current_dir = cwd
      ELSE
         current_dir = '.'
      END IF

   END SUBROUTINE get_current_dir

   !> @brief Reports failure of final rundata-file validation and stops.
   !>
   !> The supplied message is prefixed with `ERROR:`, followed by the same `-f`
   !> and `-c` usage lines emitted by the contained startup-error helper. The
   !> process then terminates with `STOP 1`. The current caller uses this routine
   !> only after `INQUIRE` reports that the selected rundata file does not exist.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-01 | SvB | Added the portable command-line failure path. |
   !> @endhistory
   SUBROUTINE handle_command_line_error(error_msg)

      CHARACTER(LEN=*), INTENT(IN) :: error_msg !! Missing or nonexistent rundata-file diagnostic.

      WRITE(*, '(A)') 'ERROR: ' // TRIM(error_msg)
      WRITE(*, '(A)') 'Usage: shetran -f rundata_file.txt'
      WRITE(*, '(A)') '   or: shetran -c catchment_name'
      STOP 1

   END SUBROUTINE handle_command_line_error

#ifdef SHETRAN_HAVE_QUICKWIN
   !> @brief Reports Intel QuickWin common-file-dialog failures.
   !>
   !> This private routine is compiled only when `SHETRAN_HAVE_QUICKWIN` is
   !> defined. It obtains `COMMDLGEXTENDEDERROR()` after `GETOPENFILENAME`,
   !> returns that value in `iret`, and maps known Windows common-dialog errors
   !> to explanatory text.
   !>
   !> | Error family | Handled constants |
   !> |:-------------|:------------------|
   !> | Resource lookup | `CDERR_FINDRESFAILURE`, `CDERR_LOCKRESFAILURE`, `CDERR_LOADRESFAILURE` |
   !> | Initialization and structure | `CDERR_INITIALIZATION`, `CDERR_LOADSTRFAILURE`, `CDERR_STRUCTSIZE` |
   !> | Memory | `CDERR_MEMALLOCFAILURE`, `CDERR_MEMLOCKFAILURE` |
   !> | Instance, hook, and template | `CDERR_NOHINSTANCE`, `CDERR_NOHOOK`, `CDERR_NOTEMPLATE` |
   !> | Filename dialog | `FNERR_BUFFERTOOSMALL`, `FNERR_INVALIDFILENAME`, `FNERR_SUBCLASSFAILURE` |
   !> | Other nonzero result | `Unknown error number` |
   !>
   !> A zero result returns silently; this is also the normal extended-error
   !> value when the user cancels the dialog, which the caller handles through
   !> its separate logical return. A nonzero result prints the fixed failure
   !> heading and mapped message, then executes an unnumbered `STOP`.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | Legacy | - | Added Windows common-dialog extended-error reporting. |
   !> | 2020-03-05 | SvB | Formatted and cleaned the error mapping. |
   !> | 2026-06-08 | SvB | Restricted the dialog helper to conditional QuickWin builds. |
   !> @endhistory
   SUBROUTINE comdlger(iret)

      INTEGER(KIND=I_P), INTENT(OUT) :: iret !! Windows extended common-dialog error code.
      CHARACTER(30) :: msg1  !! Fixed dialog-failure heading.
      CHARACTER(210) :: msg2 !! Diagnostic selected from `iret`.

      iret = COMMDLGEXTENDEDERROR()
      msg1 = 'FILE OPEN DIALOG FAILURE'

      SELECT CASE(iret)
      CASE (CDERR_FINDRESFAILURE)
         msg2 = 'The common dialog box procedure failed to find a specified resource.'
      CASE (CDERR_INITIALIZATION)
         msg2 = 'The common dialog box procedure failed during initialization.'
      CASE (CDERR_LOCKRESFAILURE)
         msg2 = 'The common dialog box procedure failed to lock a specified resource.'
      CASE (CDERR_LOADRESFAILURE)
         msg2 = 'The common dialog box procedure failed to load a specified resource.'
      CASE (CDERR_LOADSTRFAILURE)
         msg2 = 'The common dialog box procedure failed to load a specified string.'
      CASE (CDERR_MEMALLOCFAILURE)
         msg2 = 'The common dialog box procedure was unable to allocate memory for internal structures.'
      CASE (CDERR_MEMLOCKFAILURE)
         msg2 = 'The common dialog box procedure was unable to lock memory associated with a handle.'
      CASE (CDERR_NOHINSTANCE)
         msg2 = 'The common dialog box requires an instance handle but none was provided.'
      CASE (CDERR_NOHOOK)
         msg2 = 'The common dialog box requires a hook procedure but none was provided.'
      CASE (CDERR_NOTEMPLATE)
         msg2 = 'The common dialog box requires a template but none was provided.'
      CASE (CDERR_STRUCTSIZE)
         msg2 = 'The common dialog box structure size is invalid.'
      CASE (FNERR_BUFFERTOOSMALL)
         msg2 = 'The buffer for a filename is too small.'
      CASE (FNERR_INVALIDFILENAME)
         msg2 = 'A filename is invalid.'
      CASE (FNERR_SUBCLASSFAILURE)
         msg2 = 'An attempt to subclass a list box failed because insufficient memory was available.'
      CASE DEFAULT
         msg2 = 'Unknown error number'
      END SELECT

      IF (iret /= 0) THEN
         PRINT *, msg1
         PRINT *, msg2
         STOP
      END IF

   END SUBROUTINE comdlger
#endif

END MODULE GETDIRQQ
