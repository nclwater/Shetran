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
!> | Other builds, no arguments or `-a` | Print portable usage text and stop with status 255. |
!> | Any build, `-wait-on-error` anywhere | Wait for Enter before a fatal termination. |
!> | Any build, any other option or argument | Print a diagnostic and usage text and stop with status 255. |
!>
!> QuickWin support exists only when CMake enables `SHETRAN_HAVE_QUICKWIN`,
!> which currently requires `ENABLE_QUICKWIN`, Windows, and Intel Fortran.
!> Ordinary builds depend only on Fortran `GET_COMMAND_ARGUMENT` and
!> `stdlib_system` path routines. All failures terminate through
!> [[mod_error:ERR_STOP]], which waits for the user in a dialog launch or when
!> `-wait-on-error` was given.
!>
!> @warning
!> The user manual still says that a no-argument run opens a dialog on every
!> build and that a bare filename is accepted without `-f`. Neither is true for
!> the current portable build. The manual still describes the former `-error`
!> option, which is now rejected in favour of `-wait-on-error`.
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
!> | 2026-10-07 | SvB | - | Routed all failures through `ERR_STOP` and requested its wait for dialog launches. |
!> | 2026-10-08 | SvB | - | Renamed `-error` to the opt-in `-wait-on-error`; unknown options and stray arguments now stop with a diagnostic. |
!> @endhistory
MODULE GETDIRQQ

   USE mod_parameters
   USE mod_error, ONLY: ERR_STOP, err_set_wait_on_exit, err_set_wait_on_error
   USE stdlib_strings, ONLY: to_string
   USE stdlib_system, ONLY: base_name, dir_name, get_cwd, join_path

#ifdef SHETRAN_HAVE_QUICKWIN
   USE IFWIN
#endif

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: get_dir_and_catch
   PUBLIC :: rundata_from_file_dialog

   !> Whether the rundata file was chosen through the QuickWin file dialog
   !> rather than named on the command line. Only such a run owns a console
   !> window that vanishes on exit, so only such a run needs the closing wait
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
   !> directory in `rootdir`. The contained `parse_arguments` helper then
   !> classifies every argument; options may appear in any order. The
   !> `-wait-on-error` request is passed to [[mod_error:err_set_wait_on_error]]
   !> before the first recorded command-line error, if any, is reported.
   !>
   !> | Selection option | Current action |
   !> |:-----------------|:---------------|
   !> | `-f <path>` | Use the path as the rundata file; a missing path is an error. |
   !> | `-c [name]` | Search `catchments.txt` for the name, or for `default` when absent. |
   !> | `-a`, QuickWin build | Open a Windows file dialog filtered for `*rundata*.txt` and all files. |
   !> | none, QuickWin build | Synthesize `-a` and open that dialog. |
   !> | none, non-QuickWin build | Synthesize `-f`, diagnose its missing filename, and stop. |
   !>
   !> An unknown option (including the former `-error`), a second selection
   !> option, a stray positional argument, or a legacy dialog alias in a
   !> non-QuickWin build stops with a diagnostic and usage text.
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
   !> Entering the `-a` branch calls [[mod_error:err_set_wait_on_exit]], so that
   !> every later [[mod_error:ERR_STOP]], including the dialog failures and the
   !> final existence check, waits for the user before the console closes.
   !>
   !> Because the whole command line is scanned before any diagnostic is
   !> issued, every termination path honours `-wait-on-error`.
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
   !> | 2026-10-07 | SvB | - | Requested the `ERR_STOP` wait for dialog launches and scanned `-error` before selection. |
   !> | 2026-10-08 | SvB | - | Replaced `-error` with `-wait-on-error`, parsed all arguments up front, and rejected unknown options. |
   !> @endhistory
   SUBROUTINE get_dir_and_catch(runfil, fn, catch, dirqq, rootdir)

      CHARACTER(LEN=*), INTENT(IN)  :: runfil  !! Retained historical argument; not read.
      CHARACTER(LEN=*), INTENT(OUT) :: fn      !! Validated rundata path reconstructed with `join_path`.
      CHARACTER(LEN=*), INTENT(OUT) :: catch   !! Final filename stem with a lowercase `rundata_` prefix removed.
      CHARACTER(LEN=*), INTENT(OUT) :: dirqq   !! Rundata directory without an appended separator, or `.`.
      CHARACTER(LEN=*), INTENT(OUT) :: rootdir !! Launch working directory, or `.` when `get_cwd` fails.

      CHARACTER(LEN=*), PARAMETER :: catchment_file = 'catchments.txt' !! Launch-directory lookup filename.
#ifdef SHETRAN_HAVE_QUICKWIN
      CHARACTER(LEN=*), PARAMETER :: known_options = '-a, -c [name], -f <file> and -wait-on-error' !! Options listed in unknown-option diagnostics.
#else
      CHARACTER(LEN=*), PARAMETER :: known_options = '-c [name], -f <file> and -wait-on-error' !! Options listed in unknown-option diagnostics.
#endif
      CHARACTER(LEN=LENGTH_LINE) :: message !! First deferred command-line diagnostic.
      CHARACTER(LEN=LENGTH_LINE) :: dum1    !! Catchment key read from the lookup file.
      CHARACTER(LEN=LENGTH_LINE) :: dum2    !! Rundata path read from the lookup file.
      CHARACTER(LEN=LENGTH_LINE) :: code    !! Selection option, or the synthesized default mode.
      CHARACTER(LEN=LENGTH_FILEPATH) :: cli_argument !! Selected path or catchment key while resolving it.
      CHARACTER(LEN=LENGTH_FILEPATH) :: fn_part      !! Basename of the selected rundata path.
      LOGICAL :: ex                !! File-existence result.
      LOGICAL :: found_catchment   !! Whether the requested lookup key was matched.
      LOGICAL :: has_value         !! Whether the selection option was followed by a value.
      LOGICAL :: wait_on_error     !! Whether `-wait-on-error` was given.
      INTEGER :: ios               !! Lookup-file I/O status.
#ifdef SHETRAN_HAVE_QUICKWIN
      INTEGER(KIND=I_P) :: ierror !! Extended Windows common-dialog error code.
      LOGICAL(KIND=4) :: bret     !! Whether the Windows file dialog selected a file.
      CHARACTER(LEN=LENGTH_FILEPATH) :: allfilters !! NUL-delimited dialog filters.
      CHARACTER(LEN=60) :: dlgtitle !! NUL-terminated dialog title.
      TYPE(T_OPENFILENAME) :: opn    !! Windows common-file-dialog configuration.
      INTEGER :: null_pos            !! First NUL position in the selected filename.
#endif

      CALL get_current_dir(rootdir)

      ! Scan the whole command line before reporting anything, so that every
      ! ERR_STOP below, including one for a bad option, honours -wait-on-error.
      CALL parse_arguments()
      CALL err_set_wait_on_error(wait_on_error)
      IF (message /= '') CALL print_usage_and_stop(message)

      IF (code == '') THEN
#ifdef SHETRAN_HAVE_QUICKWIN
         code = '-a'  !popup window is default if there is a fortran compiler on Windows
#else
         code = '-f'  !otherwise filename is default and user must provide it as an argument
#endif
      END IF

      SELECT CASE (TRIM(code))
#ifdef SHETRAN_HAVE_QUICKWIN
      CASE ('-a')
         ! The dialog launch owns a console window that closes on exit, so
         ! every termination from here on waits for the user.
         CALL err_set_wait_on_exit(.TRUE.)

         FileName = CHAR(0)
         allfilters = 'rundata files (*rundata*.txt)'//CHAR(0)//'*rundata*.txt'//CHAR(0)// &
                      'All files (*.*)'//CHAR(0)//'*.*'//CHAR(0)//CHAR(0)
         dlgtitle = 'Select a SHETRAN rundata file'C

         opn%lStructSize = 0
         opn%HWNDOWNER = NULL
         opn%HINSTANCE = NULL
         opn%LPSTRFILTER = NULL
         opn%LPSTRCUSTOMFILTER = NULL
         opn%NMAXCUSTFILTER = 0
         opn%NFILTERINDEX = 0
         opn%LPSTRFILE = NULL
         opn%NMAXFILE = 0
         opn%LPSTRFILETITLE = NULL
         opn%NMAXFILETITLE = 0
         opn%LPSTRINITIALDIR = NULL
         opn%LPSTRTITLE = NULL
         opn%FLAGS = 0
         opn%NFILEOFFSET = 0
         opn%NFILEEXTENSION = 0
         opn%LPSTRDEFEXT = NULL
         opn%LCUSTDATA = 0
         opn%LPFNHOOK = NULL
         opn%LPTEMPLATENAME = NULL
         opn%PVRESERVED = NULL
         opn%DWRESERVED = 0
         opn%FLAGSEX = 0

         opn%lStructSize = SIZEOF(opn)
         opn%LPSTRFILTER = LOC(allfilters)
         opn%NFILTERINDEX = 1
         opn%LPSTRFILE = LOC(FileName)
         opn%NMAXFILE = LEN(FileName)
         opn%LPSTRTITLE = LOC(dlgtitle)
         opn%FLAGS = OFN_EXPLORER + OFN_FILEMUSTEXIST + OFN_PATHMUSTEXIST + OFN_NOCHANGEDIR

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

         ! The dialog supplied the file, so ask [[shetran]] to wait for the user
         ! at normal completion as well.
         rundata_from_file_dialog = .TRUE.
#endif

      CASE ('-f')
         IF (.NOT. has_value) THEN
            CALL print_usage_and_stop('Missing filename after -f')
         END IF

      CASE ('-c')
         IF (.NOT. has_value) cli_argument = 'default'

         INQUIRE (FILE=catchment_file, EXIST=ex)
         IF (ex) THEN
            OPEN (UNIT=875, FILE=catchment_file, STATUS='OLD', IOSTAT=ios)
            IF (ios /= 0) CALL print_usage_and_stop('Error reading catchment file')

            found_catchment = .FALSE.

            read_catchment: DO
               READ (875, '(A)', IOSTAT=ios) dum1
               IF (ios /= 0) EXIT read_catchment

               READ (875, *, IOSTAT=ios) dum2
               IF (ios /= 0) EXIT read_catchment

               IF (TRIM(dum1) == TRIM(cli_argument)) THEN
                  cli_argument = dum2
                  found_catchment = .TRUE.
                  EXIT read_catchment
               END IF
            END DO read_catchment

            CLOSE (875)

            IF (.NOT. found_catchment) THEN
               CALL print_usage_and_stop('Cannot find catchment '//TRIM(cli_argument)//' in '//catchment_file)
            END IF
         ELSE
            message = 'Cannot find file '//catchment_file//' in executable directory'
         END IF
      END SELECT

      IF (message /= '') CALL print_usage_and_stop(message)

      INQUIRE (FILE=cli_argument, EXIST=ex)
      IF (.NOT. ex) THEN
         IF (LEN_TRIM(cli_argument) == 0) THEN
            message = 'Missing filename. Use: shetran -f filename.txt'
         ELSE
            message = 'Cannot find rundata file '//TRIM(cli_argument)
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
      !> Delegates to [[handle_command_line_error]]. This helper handles option,
      !> dialog-cancel, and catchment-lookup failures that occur before final
      !> file-existence validation.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2026-04-06 | SvB | Replaced the shared terminal-label error path with a contained helper. |
      !> | 2026-06-08 | SvB | Updated the helper for the portable `-f`/`-c` interface. |
      !> | 2026-10-07 | SvB | Terminated through `ERR_STOP`. |
      !> | 2026-10-08 | SvB | Delegated to the identical module-level helper. |
      !> @endhistory
      SUBROUTINE print_usage_and_stop(err_msg)
         CHARACTER(LEN=*), INTENT(IN) :: err_msg !! Specific command-line or lookup failure.

         CALL handle_command_line_error(err_msg)
      END SUBROUTINE print_usage_and_stop

      !> @brief Classifies every command argument and records the first error.
      !>
      !> Sets the host variables `code`, `cli_argument`, `has_value`,
      !> `wait_on_error`, and `message`. Every argument is visited, so
      !> `-wait-on-error` is honoured wherever it appears, but only the first
      !> problem is kept in `message`; [[get_dir_and_catch]] reports it.
      !>
      !> | Argument | Action |
      !> |:---------|:-------|
      !> | `-wait-on-error` | Sets `wait_on_error`. |
      !> | `-f`, `-c`, `-a` (QuickWin only) | Becomes `code`; a second selection option is an error. |
      !> | Legacy dialog aliases, non-QuickWin build | Error: interactive selection requires Intel QuickWin. |
      !> | The argument after `-f` or `-c` | Becomes `cli_argument` unless it starts with `-`. |
      !> | `-error` | Error naming its replacement `-wait-on-error`. |
      !> | Any other `-...` token | Error: unknown option. |
      !> | Any other token | Error: unexpected argument. |
      !>
      !> The legacy dialog aliases are `-a`, `-m`, `-af`, `-sd`, `-pattern`,
      !> `-delinc`, and `-results`; in a QuickWin build all but `-a` are unknown
      !> options. A rundata path that itself starts with `-` must be given with a
      !> directory prefix, e.g. `./-name.txt`.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2026-10-08 | SvB | Replaced the `-error` scan with a full classification; unknown options are errors. |
      !> @endhistory
      SUBROUTINE parse_arguments()
         INTEGER :: na        !! Number of command-line arguments.
         INTEGER :: arg_index !! One-based index of the argument being classified.
         CHARACTER(LEN=LENGTH_FILEPATH) :: argument      !! Argument being classified.
         CHARACTER(LEN=LENGTH_FILEPATH) :: next_argument !! Candidate value for a selection option.

         code = ''
         cli_argument = ''
         has_value = .FALSE.
         wait_on_error = .FALSE.
         message = ''

         na = COMMAND_ARGUMENT_COUNT()
         arg_index = 0

         scan_arguments: DO WHILE (arg_index < na)
            arg_index = arg_index + 1
            CALL read_argument(arg_index, argument)

            SELECT CASE (TRIM(argument))
            CASE ('-wait-on-error')
               wait_on_error = .TRUE.

#ifdef SHETRAN_HAVE_QUICKWIN
            CASE ('-a', '-c', '-f')
#else
            CASE ('-c', '-f')
#endif
               IF (code /= '') THEN
                  CALL record_error('Only one rundata selection option may be given; found both '// &
                                    TRIM(code)//' and '//TRIM(argument))
               END IF
               code = argument

               IF (TRIM(argument) /= '-a' .AND. arg_index < na) THEN
                  CALL read_argument(arg_index + 1, next_argument)
                  IF (LEN_TRIM(next_argument) > 0 .AND. next_argument(1:1) /= '-') THEN
                     arg_index = arg_index + 1
                     cli_argument = next_argument
                     has_value = .TRUE.
                  END IF
               END IF

#ifndef SHETRAN_HAVE_QUICKWIN
            CASE ('-a', '-m', '-af', '-sd', '-pattern', '-delinc', '-results')
               CALL record_error('Interactive file selection ('//TRIM(argument)// &
                                 ') requires Intel QuickWin on Windows. Use: shetran -f filename.txt')
#endif

            CASE ('-error')
               CALL record_error('Unknown command line option -error; it has been renamed to -wait-on-error')

            CASE DEFAULT
               IF (LEN_TRIM(argument) == 0) THEN
                  CALL record_error('Empty command line argument at position '//to_string(arg_index))
               ELSE IF (argument(1:1) == '-') THEN
                  CALL record_error('Unknown command line option '//TRIM(argument)// &
                                    '. Recognised options are '//known_options)
               ELSE
                  CALL record_error('Unexpected command line argument '//TRIM(argument)// &
                                    '. A rundata filename must follow -f, a catchment name must follow -c')
               END IF
            END SELECT
         END DO scan_arguments
      END SUBROUTINE parse_arguments

      !> @brief Reads one command argument, recording an error if it is truncated.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2026-10-08 | SvB | Initial version. |
      !> @endhistory
      SUBROUTINE read_argument(arg_index, argument)
         INTEGER, INTENT(IN) :: arg_index            !! One-based command-argument index.
         CHARACTER(LEN=*), INTENT(OUT) :: argument   !! Argument text, blank padded.
         INTEGER :: arg_status !! `GET_COMMAND_ARGUMENT` status; nonzero when truncated or unavailable.

         CALL GET_COMMAND_ARGUMENT(arg_index, argument, STATUS=arg_status)
         IF (arg_status /= 0) THEN
            CALL record_error('Command line argument '//to_string(arg_index)//' is longer than '// &
                              to_string(LEN(argument))//' characters')
         END IF
      END SUBROUTINE read_argument

      !> @brief Keeps the first command-line diagnostic in the host `message`.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2026-10-08 | SvB | Initial version. |
      !> @endhistory
      SUBROUTINE record_error(err_msg)
         CHARACTER(LEN=*), INTENT(IN) :: err_msg !! Command-line diagnostic.

         IF (message == '') message = err_msg
      END SUBROUTINE record_error

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
   !> The supplied message is prefixed with `ERROR:`, followed by usage lines
   !> for the `-f`, `-c`, (QuickWin builds) `-a`, and `-wait-on-error` forms.
   !> The process then terminates with `ERR_STOP(255)`. It is called directly
   !> after `INQUIRE` reports that the selected rundata file does not exist, and
   !> through the contained `print_usage_and_stop` for all earlier failures.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-01 | SvB | Added the portable command-line failure path. |
   !> | 2026-10-07 | SvB | Terminated through `ERR_STOP`. |
   !> | 2026-10-08 | SvB | Documented `-a` and `-wait-on-error` in the usage lines. |
   !> @endhistory
   SUBROUTINE handle_command_line_error(error_msg)

      CHARACTER(LEN=*), INTENT(IN) :: error_msg !! Command-line, lookup, or nonexistent rundata-file diagnostic.

      WRITE (*, '(A)') 'ERROR: '//TRIM(error_msg)
      WRITE (*, '(A)') 'Usage: shetran -f rundata_file.txt [-wait-on-error]'
      WRITE (*, '(A)') '   or: shetran -c [catchment_name] [-wait-on-error]'
#ifdef SHETRAN_HAVE_QUICKWIN
      WRITE (*, '(A)') '   or: shetran [-a] [-wait-on-error]'
#endif
      WRITE (*, '(A)') '-wait-on-error: wait for Enter before exiting on a fatal error.'
      CALL ERR_STOP(255)

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
   !> heading and mapped message, then terminates through `ERR_STOP(255)`.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | Legacy | - | Added Windows common-dialog extended-error reporting. |
   !> | 2020-03-05 | SvB | Formatted and cleaned the error mapping. |
   !> | 2026-06-08 | SvB | Restricted the dialog helper to conditional QuickWin builds. |
   !> | 2026-10-07 | SvB | Replaced the unnumbered `STOP` with `ERR_STOP(255)`. |
   !> @endhistory
   SUBROUTINE comdlger(iret)

      INTEGER(KIND=I_P), INTENT(OUT) :: iret !! Windows extended common-dialog error code.
      CHARACTER(30) :: msg1  !! Fixed dialog-failure heading.
      CHARACTER(210) :: msg2 !! Diagnostic selected from `iret`.

      iret = COMMDLGEXTENDEDERROR()
      msg1 = 'FILE OPEN DIALOG FAILURE'

      SELECT CASE (iret)
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
         CALL ERR_STOP(255)
      END IF

   END SUBROUTINE comdlger
#endif

END MODULE GETDIRQQ
