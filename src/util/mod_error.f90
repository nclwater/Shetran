!> summary: Central diagnostic reporting, error accounting, and program termination.
!> author: R. A. Heath, Newcastle University; Sven Berendsen, Southampton University
!>
!> This module owns SHETRAN's shared error-handling interface: the numbered
!> diagnostic reporter [[mod_error:ERROR]], the termination routine
!> [[mod_error:ERR_STOP]], the severity selectors passed to `ERROR`, the
!> per-code occurrence counters, and the default primary print unit. It was
!> extracted from [[sglobal]] so that error handling is no longer coupled to
!> the global capacity/state module, and so that its consumers declare the
!> dependency explicitly.
!>
!> The two timestep-reduction request flags remain in [[sglobal]]: `ERROR`
!> writes them and [[rest:TMSTEP]] consumes them, and keeping them there
!> avoids a circular dependency between this module and `sglobal`.
!>
!> @note
!> The `ERROR` interface is unchanged from its former `sglobal` form so that
!> the existing call sites continue to work; only the names of the severity
!> selectors and the print unit have changed.
!> @endnote
!>
!> @todo Figure a way to do `flag_runtime_reduction_errors` and
!> `flag_runtime_reduction_e1060` more elegantly - best might be with the
!> general state tracker.
!> @todo Split error message initialization and printing into separate methods
!> for better modularity.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-08-31 | SvB | - | Initial version, extracted from [[sglobal]] with the selectors, counters, and print unit renamed. |
!> | 2026-08-31 | SvB | - | Split fatal from ordinary termination in `ALSTOP`, added `err_set_wait_on_exit`, and widened the help-line buffer to `LENGTH_LINE`. |
!> @endhistory
MODULE mod_error

   USE MOD_PARAMETERS, ONLY: I_P, LENGTH_FILEPATH, LENGTH_LINE
   USE SGLOBAL, ONLY: UZNOW, EARRAY, rootdir, error_mode, &
                      flag_runtime_reduction_errors, flag_runtime_reduction_e1060
   USE stdlib_strings, ONLY: to_string

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: RAISE_ERROR, ERR_STOP, err_set_wait_on_exit
   PUBLIC :: errstat_fileopen, errstat_fileclose
   PUBLIC :: errstat_alloc, errstat_dealloc
   PUBLIC :: errstat_read, errstat_write
   PUBLIC :: ERRLVL_fatal, ERRLVL_error, ERRLVL_warn
   PUBLIC :: FID_logfile
   PUBLIC :: ERR_limit_error_codes

   ! --------------------------------------------------------------------
   ! Severity selectors
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P), PARAMETER :: ERRLVL_fatal = 1 !! Fatal error severity passed to `ERROR`.
   INTEGER(KIND=I_P), PARAMETER :: ERRLVL_error = 2 !! Nonfatal error severity passed to `ERROR`.
   INTEGER(KIND=I_P), PARAMETER :: ERRLVL_warn = 3 !! Warning severity passed to `ERROR`.

   ! --------------------------------------------------------------------
   ! Diagnostic codes reported by the standardised status checks
   !
   ! These sit in the general library group (0000--0100), clear of the
   ! codes 1--14 already issued by [[mod_load_filedata]].
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_fileopen = 20 !! Code reported for a failed file open.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_fileclose = 21 !! Code reported for a failed file close.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_allocate = 22 !! Code reported for a failed allocation.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_deallocate = 23 !! Code reported for a failed deallocation.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_read = 24 !! Code reported for a failed read.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_write = 25 !! Code reported for a failed write.

   ! --------------------------------------------------------------------
   ! Error accounting
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P), PARAMETER :: ERR_limit_error_codes = 100 !! Greatest error-code remainder represented in each module-group counter.
   INTEGER(KIND=I_P) :: error_counter(0:ERR_limit_error_codes, 0:3) = 0 !! Occurrence counts by error-code remainder and module group.
   INTEGER(KIND=I_P) :: error_counter_total = 0 !! Total number of errors and warnings recorded by `ERROR`.
   LOGICAL :: flag_wait_on_exit = .FALSE. !! Whether `ERR_STOP` waits for the user before terminating.

   ! --------------------------------------------------------------------
   ! Diagnostic output destinations
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P), PARAMETER :: FID_logfile = 23 !! Default Fortran unit for primary PRI output.
   CHARACTER(LEN=LENGTH_FILEPATH) :: helppath !! Help-directory fragment set to `/helpmessages` by each `ERROR` call.

CONTAINS

   !> summary: Selects whether [[mod_error:ERR_STOP]] waits before terminating.
   !> author: S. Berendsen, Southampton University
   !>
   !> Intended for interactive launches, where the console window closes as
   !> soon as the process exits and the user would otherwise never get to read
   !> the final diagnostics. `error_mode` still overrides the request, so a run
   !> started with `-error` stays noninteractive either way.
   !>
   !> @note
   !> No current caller sets this flag, so `ERR_STOP` does not yet wait on the
   !> strength of it alone. Wiring it to the launch mode is pending.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-08-31 | SvB | Initial version. |
   !> @endhistory
   SUBROUTINE err_set_wait_on_exit(wait)
      LOGICAL, INTENT(IN) :: wait !! `.TRUE.` requests a wait for user input before termination.

      flag_wait_on_exit = wait
   END SUBROUTINE err_set_wait_on_exit

   !> summary: Standardised check for opening file return status.
   !> author: S. Berendsen, Southampton University
   !>
   !> Standardised check for opening file return status.
   !>
   !> Pass `iomsg` the string filled by the `IOMSG=` specifier of the failing
   !> `OPEN` statement to have the processor's explanatory text for `status`
   !> included in the diagnostic.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-08-31 | SvB | Initial version. |
   !> | 2026-09-06 | SvB | Report the `status` value and the optional `IOMSG=` text. |
   !> @endhistory
   SUBROUTINE errstat_fileopen(status, filename, iomsg)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! Return status from file opening.
      CHARACTER(LEN=*), INTENT(IN) :: filename !! Name of the file being opened.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: iomsg !! Text from the `IOMSG=` specifier of the failing `OPEN`.

      CHARACTER(LEN=LENGTH_LINE) :: msg !! Constructed message for the error report.

      IF (status /= 0) THEN
         msg = 'Error opening file: '//TRIM(filename)//' (status '//to_string(status)//')'
         IF (PRESENT(iomsg)) THEN
            IF (LEN_TRIM(iomsg) > 0) msg = TRIM(msg)//': '//TRIM(iomsg)
         END IF
         CALL RAISE_ERROR(ERRLVL_fatal, ERRCODE_fileopen, FID_logfile, 0, 0, TRIM(msg))
      END IF
   END SUBROUTINE errstat_fileopen

   !> summary: Standardised check for closing file return status.
   !> author: S. Berendsen, Southampton University
   !>
   !> Standardised check for closing file return status.
   !>
   !> Pass `iomsg` the string filled by the `IOMSG=` specifier of the failing
   !> `CLOSE` statement to have the processor's explanatory text for `status`
   !> included in the diagnostic.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-08-31 | SvB | Initial version. |
   !> | 2026-09-06 | SvB | Report the `status` value and the optional `IOMSG=` text. |
   !> @endhistory
   SUBROUTINE errstat_fileclose(status, filename, iomsg)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! Return status from file closing.
      CHARACTER(LEN=*), INTENT(IN) :: filename !! Name of the file being closed.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: iomsg !! Text from the `IOMSG=` specifier of the failing `CLOSE`.

      CHARACTER(LEN=LENGTH_LINE) :: msg !! Constructed message for the error report.

      IF (status /= 0) THEN
         msg = 'Error closing file: '//TRIM(filename)//' (status '//to_string(status)//')'
         IF (PRESENT(iomsg)) THEN
            IF (LEN_TRIM(iomsg) > 0) msg = TRIM(msg)//': '//TRIM(iomsg)
         END IF
         CALL RAISE_ERROR(ERRLVL_fatal, ERRCODE_fileclose, FID_logfile, 0, 0, TRIM(msg))
      END IF
   END SUBROUTINE errstat_fileclose

   !> summary: Standardised check for allocate memory return status.
   !> author: S. Berendsen, Southampton University
   !>
   !> Standardised check for allocate memory return status.
   !>
   !> Pass `errmsg` the string filled by the `ERRMSG=` specifier of the failing
   !> `ALLOCATE` statement to have the processor's explanatory text for `status`
   !> included in the diagnostic.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-08-31 | SvB | Initial version. |
   !> | 2026-09-06 | SvB | Report the `status` value and the optional `ERRMSG=` text. |
   !> @endhistory
   SUBROUTINE errstat_alloc(status, variable, location, errmsg)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! Return status from allocate memory.
      CHARACTER(LEN=*), INTENT(IN) :: variable !! Name of the variable being allocated.
      CHARACTER(LEN=*), INTENT(IN) :: location !! Location where the memory was allocated.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: errmsg !! Text from the `ERRMSG=` specifier of the failing `ALLOCATE`.

      CHARACTER(LEN=LENGTH_LINE) :: msg !! Constructed message for the error report.

      IF (status /= 0) THEN
         msg = 'Error allocating memory for '//TRIM(variable)//' at '//TRIM(location)// &
               ' (status '//to_string(status)//')'
         IF (PRESENT(errmsg)) msg = TRIM(msg)//': '//TRIM(errmsg)
         CALL RAISE_ERROR(ERRLVL_fatal, ERRCODE_allocate, FID_logfile, 0, 0, TRIM(msg))
      END IF
   END SUBROUTINE errstat_alloc

   !> summary: Standardised check for deallocating memory return status.
   !> author: S. Berendsen, Southampton University
   !>
   !> Standardised check for deallocating memory return status.
   !>
   !> Pass `errmsg` the string filled by the `ERRMSG=` specifier of the failing
   !> `DEALLOCATE` statement to have the processor's explanatory text for
   !> `status` included in the diagnostic.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-08-31 | SvB | Initial version. |
   !> | 2026-09-06 | SvB | Report the `status` value and the optional `ERRMSG=` text. |
   !> @endhistory
   SUBROUTINE errstat_dealloc(status, variable, location, errmsg)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! Return status from deallocate memory.
      CHARACTER(LEN=*), INTENT(IN) :: variable !! Name of the variable being deallocated.
      CHARACTER(LEN=*), INTENT(IN) :: location !! Location where the memory was deallocated.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: errmsg !! Text from the `ERRMSG=` specifier of the failing `DEALLOCATE`.

      CHARACTER(LEN=LENGTH_LINE) :: msg !! Constructed message for the error report.

      IF (status /= 0) THEN
         msg = 'Error deallocating memory for '//TRIM(variable)//' at '//TRIM(location)// &
               ' (status '//to_string(status)//')'
         IF (PRESENT(errmsg)) msg = TRIM(msg)//': '//TRIM(errmsg)
         CALL RAISE_ERROR(ERRLVL_fatal, ERRCODE_deallocate, FID_logfile, 0, 0, TRIM(msg))
      END IF
   END SUBROUTINE errstat_dealloc

   !> summary: Standardised check for reading data return status.
   !> author: S. Berendsen, Southampton University
   !>
   !> Standardised check for reading data return status.
   !> For special end-of-file or end-of-record conditions, the caller should
   !> check `status` and handle them before calling this routine.
   !>
   !> Pass `iomsg` the string filled by the `IOMSG=` specifier of the failing
   !> `READ` statement to have the processor's explanatory text for `status`
   !> included in the diagnostic.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-08-31 | SvB | Initial version. |
   !> | 2026-09-06 | SvB | Report the `status` value and the `IOMSG=` text. |
   !> @endhistory
   SUBROUTINE errstat_read(status, location, iomsg, filename, linenumber)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! Return status from file opening.
      CHARACTER(LEN=*), INTENT(IN) :: location !! Location where the data was read.
      CHARACTER(LEN=*), INTENT(IN) :: iomsg !! Text from the `IOMSG=` specifier of the failing `READ`.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename !! Name from which file this data was read.
      INTEGER(KIND=I_P), INTENT(IN), OPTIONAL :: linenumber !! Line number in the file being read.

      CHARACTER(LEN=LENGTH_LINE) :: msg !! Constructed message for the error report.

      IF (status /= 0) THEN
         msg = 'Error reading data at '//TRIM(location)
         IF (PRESENT(filename)) msg = TRIM(msg)//' from file '//TRIM(filename)
         IF (PRESENT(linenumber)) msg = TRIM(msg)//' at line '//to_string(linenumber)
         msg = TRIM(msg)//' (status '//to_string(status)//')'
         IF (LEN_TRIM(iomsg) > 0) msg = TRIM(msg)//': '//TRIM(iomsg)
         CALL RAISE_ERROR(ERRLVL_fatal, ERRCODE_read, FID_logfile, 0, 0, TRIM(msg))
      END IF
   END SUBROUTINE errstat_read

   !> summary: Standardised check for opening file return status.
   !> author: S. Berendsen, Southampton University
   !>
   !> Standardised check for opening file return status.
   !>
   !> Pass `iomsg` the string filled by the `IOMSG=` specifier of the failing
   !> `WRITE` statement to have the processor's explanatory text for `status`
   !> included in the diagnostic.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-08-31 | SvB | Initial version. |
   !> | 2026-09-06 | SvB | Report the `status` value and the `IOMSG=` text. |
   !> @endhistory
   SUBROUTINE errstat_write(status, location, iomsg, filename)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! Return status from file opening.
      CHARACTER(LEN=*), INTENT(IN) :: location !! Location where the data is supposed to be written to.
      CHARACTER(LEN=*), INTENT(IN) :: iomsg !! Text from the `IOMSG=` specifier of the failing `WRITE`.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename !! Name of the file being written to.

      CHARACTER(LEN=LENGTH_LINE) :: msg !! Constructed message for the error report.

      IF (status /= 0) THEN
         msg = 'Error writing data at '//TRIM(location)
         IF (PRESENT(filename)) msg = TRIM(msg)//' to file '//TRIM(filename)
         msg = TRIM(msg)//' (status '//to_string(status)//')'
         IF (LEN_TRIM(iomsg) > 0) msg = TRIM(msg)//': '//TRIM(iomsg)
         CALL RAISE_ERROR(ERRLVL_fatal, ERRCODE_write, FID_logfile, 0, 0, TRIM(msg))
      END IF
   END SUBROUTINE errstat_write

   !> summary: Reports a SHETRAN diagnostic, records it, and terminates fatal runs.
   !>
   !> @author R. A. Heath, Newcastle University
   !>
   !> This is the shared reporter used by 159 active call sites across the
   !> input utilities, process modules, simulation driver, and visualisation
   !> interface. In agreement with User Manual section 1.6.6, ordinary calls
   !> write a numbered diagnostic to a component print unit such as PRI, SPR,
   !> CPR, or MNPR. `IEL` and `CELL` add spatial context when their zero sentinel
   !> is not used, and the module time `UZNOW` supplies the reported time [h].
   !>
   !> Message selectors have the following current behavior:
   !>
   !> | `ETYPE` | Immediate record and accounting | Control behavior |
   !> |:--------|:--------------------------------|:-----------------|
   !> | `ERRLVL_fatal=1` | Writes a `FATAL ERROR` header and `TEXT` to `OUT`; increments `error_counter_total` and, for a representable code, `error_counter`. | Prints the summary, then calls [[mod_error:ALSTOP]] for error termination. |
   !> | `ERRLVL_error=2` | Writes an `ERROR` header and `TEXT`; increments the counters as above. | Returns to the caller. |
   !> | `ERRLVL_warn=3` | Writes a `WARNING` header and `TEXT`; increments the counters as above. | Returns to the caller. |
   !> | `0` | Writes `TEXT` without a severity header and does not increment either counter. | `ERRNUM=0` would also request a summary; current callers use code 12 only for continuation text from [[mod_load_filedata:ALCHK]] and [[mod_load_filedata:ALCHKI]]. |
   !>
   !> Every call writes `TEXT` to `OUT`, even if `ETYPE` lies outside zero
   !> through three. Only selectors one through three receive a formatted
   !> severity line, and only `ERRLVL_fatal` terminates. A zero `IEL`
   !> suppresses both spatial fields; otherwise a zero `CELL` suppresses only
   !> the cell field. Codes 1003 and 1024 append a numeric line read from
   !> `EARRAY(1)`.
   !>
   !> The per-code summary decomposes `ERRNUM` into `AMODL=ERRNUM/1000` and
   !> `ERRN=MOD(ERRNUM,1000)`. `error_counter(0:ERR_limit_error_codes,0:3)`,
   !> with `ERR_limit_error_codes=100`, can therefore represent only these
   !> inclusive domains:
   !>
   !> | Representable codes | Current component convention |
   !> |:--------------------|:-----------------------------|
   !> | 0000--0100 | General library and input utilities. |
   !> | 1000--1100 | Water-flow components. |
   !> | 2000--2100 | Sediment component. |
   !> | 3000--3100 | Contaminant and nitrate components. |
   !>
   !> `error_counter_total` counts every nonzero-selector call regardless of
   !> whether its code is representable. `error_counter` aggregates by code, not
   !> by severity. A fatal call or any regular call with `ERRNUM=0` writes the
   !> summary heading to standard output and `OUT`, obtains the connected
   !> filename for `OUT`, and tells the user to inspect it. Per-code counts,
   !> constructed help paths, help text, and the final total are then written
   !> only to standard output.
   !>
   !> Errors 1024 and 1030 request the stronger timestep reduction through
   !> `flag_runtime_reduction_errors`; error 1060 requests the separate
   !> reduction through `flag_runtime_reduction_e1060`. [[rest:TMSTEP]] divides
   !> its proposed timestep by 100 or 10 respectively, subject to a 0.0003 h
   !> floor, and clears the flags after consuming them.
   !>
   !> @warning
   !> This routine clears both timestep-reduction flags at the start of every
   !> call. Consequently the flags describe only the most recent diagnostic:
   !> any later unrelated error or warning can erase a pending request before
   !> `TMSTEP` reads it.
   !> @endwarning
   !>
   !> @warning
   !> No current source assignment initializes `EARRAY(1)`, although codes 1003
   !> and 1024 print it as maximum head difference and surface-water depth.
   !> Those appended numeric diagnostics are therefore undefined.
   !> @endwarning
   !>
   !> @warning
   !> Current calls with codes 4820, 4998, 4999, and 2107 lie outside the
   !> representable `error_counter` domains. Their immediate messages,
   !> `error_counter_total` increments, and fatal termination still occur, but
   !> their per-code counts and help-file lookups are omitted from the summary.
   !> The manual's numbered component list is also offset from the
   !> zero-through-three code groups used by the routine.
   !> @endwarning
   !>
   !> @warning
   !> Summary lookup uses the launch working directory followed by
   !> `/helpmessages`, a forward slash, and a four-digit code with a `.txt`
   !> extension; failed opens are silent, help lines are limited to
   !> `LENGTH_LINE` characters, and no `helpmessages` directory is present in
   !> this repository.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 1994-10-08 | RAH | Created v3.4.1 from v3.4: introduced severity zero, local/range-checked counters, conditional element/cell fields, help summaries, and fatal dispatch through `ALSTOP`. |
   !> | 1997-08-04 | RAH | Restored `EARRAY(1)` output for error 1024 in v4.1. |
   !> | 1997-08-11 | RAH | Added the legacy external declaration after the include block. |
   !> | 2020-07-07 | SB | Added the 1024/1030 and 1060 flags used to reduce the subsequent timestep. |
   !> | 2026-03-28 | SvB | Converted the interface and locals to selected kinds with input intents, replaced the `CTYPE` data statement, and added the initial FORD block. |
   !> | 2026-04-13 | SvB | Replaced labelled summary/help loops and error branches with named loops and `IOSTAT` handling. |
   !> | 2026-05-08 | SB | Reworked summary output to name the selected print file and write the summary heading to both standard output and `OUT`. |
   !> | 2026-05-10 | SvB | Removed the interactive wait before help-file lookup for noninteractive scripted use. |
   !> | 2026-08-31 | SvB | Moved from [[sglobal]] to [[mod_error]] and renamed the selectors and counters. |
   !> @endhistory
   SUBROUTINE RAISE_ERROR(ETYPE, ERRNUM, OUT, IEL, CELL, TEXT)

      IMPLICIT NONE

      ! IO-related parameters and variables
      INTEGER(KIND=I_P), INTENT(IN) :: ETYPE  !! Severity selector: 0--3.
      INTEGER(KIND=I_P), INTENT(IN) :: ERRNUM !! Diagnostic code; zero requests a summary.
      INTEGER(KIND=I_P), INTENT(IN) :: OUT    !! Connected formatted unit receiving the immediate diagnostic.
      INTEGER(KIND=I_P), INTENT(IN) :: IEL    !! Element identifier; zero omits both element and cell fields.
      INTEGER(KIND=I_P), INTENT(IN) :: CELL   !! VSS cell identifier; zero omits the cell field.
      CHARACTER(LEN=*), INTENT(IN) :: TEXT   !! Immediate diagnostic or continuation text.

      INTEGER(KIND=I_P), PARAMETER :: NONE = 0 !! No-severity selector.
      INTEGER(KIND=I_P), PARAMETER :: HLP = 8 !! Fixed unit used for an available help file.

      ! Local variables
      CHARACTER(LEN=256) :: FIL, fname !! Constructed help path and name queried for `OUT`.
      CHARACTER(LEN=LENGTH_LINE) :: HLPMSG !! One help-file line.

      INTEGER(KIND=I_P) :: COUNT, ERRN, AMODL !! Summary count, code remainder, and component group.
      INTEGER(KIND=I_P) :: IO_STATUS !! Help-file open/read status.

      LOGICAL :: VALID !! Counter-index validity.

      ! Modernization Fix: Replaced legacy DATA statement with a strict PARAMETER array
      CHARACTER(LEN=11), PARAMETER :: CTYPE(3) = ['FATAL ERROR', '      ERROR', '    WARNING'] !! Labels for `ETYPE` 1--3.

      !-------------------------------------------------------------------*

      helppath = '/helpmessages'

      ! SB 07072020 reduce timestep if there are errors 1024,1030,1060
      flag_runtime_reduction_errors = .FALSE.
      flag_runtime_reduction_e1060 = .FALSE.

      ! Write general error message
      ! ---------------------------
      IF (ETYPE >= 1 .AND. ETYPE <= 3) THEN
         IF (ETYPE == ERRLVL_fatal) WRITE (OUT, '(//)')

         IF (IEL == 0) THEN
            WRITE (OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW
         ELSE IF (CELL == 0) THEN
            WRITE (OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW, IEL
         ELSE
            WRITE (OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW, IEL, CELL
         END IF
      END IF

      WRITE (OUT, '(8X,A)') TEXT

      ! Decompose ERRNUM and update counters
      ! ------------------------------------
      IF (ETYPE /= NONE) THEN
         error_counter_total = error_counter_total + 1
         AMODL = ERRNUM/1000
         ERRN = MOD(ERRNUM, 1000)

         VALID = (AMODL >= 0 .AND. AMODL <= 3 .AND. ERRN >= 0 .AND. ERRN <= ERR_limit_error_codes)
         IF (VALID) error_counter(ERRN, AMODL) = error_counter(ERRN, AMODL) + 1
      END IF

      ! Write specific error messages
      ! -----------------------------
      IF (ERRNUM == 1003) THEN
         WRITE (OUT, 91003) EARRAY(1)
         ! 970804
      ELSE IF (ERRNUM == 1024) THEN
         WRITE (OUT, 91024) EARRAY(1)
         !
      END IF

      ! SB 07072020 reduce timestep if there are errors 1024,1030,1060
      IF (ERRNUM == 1024 .OR. ERRNUM == 1030) THEN
         flag_runtime_reduction_errors = .TRUE.
      END IF
      IF (ERRNUM == 1060) THEN
         flag_runtime_reduction_e1060 = .TRUE.
      END IF

      ! Write summary
      ! -------------
      IF (ETYPE == ERRLVL_fatal .OR. ERRNUM == 0) THEN
         WRITE (*, '(/,A,/,A,/)') &
            ' ### Error Summary and Advice ###', &
            '     ------------------------'
         WRITE (OUT, '(/,A,/,A,/)') &
            ' ### Error Summary and Advice ###', &
            '     ------------------------'
         INQUIRE (OUT, NAME=fname)

         IF (error_counter_total > 0) WRITE (*, '(A,A,A/)') &
            ' ==> Check the pri file: "', trim(fname), '" for more details <=='

         module_loop: DO AMODL = 0, 3
            error_loop: DO ERRN = 0, ERR_limit_error_codes
               COUNT = error_counter(ERRN, AMODL)

               IF (COUNT > 0) THEN
                  ! Print number of occurrences
                  WRITE (*, 9500) ERRN + AMODL*1000, COUNT

                  ! Print contents of help file (if any)
                  WRITE (FIL, 9200) TRIM(rootdir)//TRIM(helppath)//'/', AMODL, ERRN, '.txt'

                  OPEN (HLP, FILE=FIL, STATUS='OLD', IOSTAT=IO_STATUS)
                  IF (IO_STATUS == 0) THEN
                     read_help: DO
                        READ (HLP, '(A)', IOSTAT=IO_STATUS) HLPMSG
                        IF (IO_STATUS /= 0) EXIT read_help
                        WRITE (*, '(A)') trim(HLPMSG)
                     END DO read_help
                     CLOSE (HLP)
                  END IF

                  WRITE (*, *)

               END IF
            END DO error_loop
         END DO module_loop

         WRITE (*, 9600) error_counter_total
      END IF

      ! Stop?
      ! -----
      IF (ETYPE == ERRLVL_fatal) CALL ERR_STOP(1)

      ! String format statements
      ! ------------------------
9100  FORMAT(/' !!!', A, I5.4, ' at time =', F12.2, ' hours': &
         &        ', iel =', I5:', cell =', I5)
9200  FORMAT(A, I1, I3.3, A)

9500  FORMAT(' No. of occurrences of error number', I5.4, ' is', I6)
9600  FORMAT(/' ### End of summary: recorded error count is', I7, ' ###'/)
91003 FORMAT(' MAXIMUM DIFFERENCE (DHMAX) = ', G12.6, ' METRES')
! 970804
91024 FORMAT(' DEPTH OF SURFACE WATER BELOW GROUND = ', G12.6, ' METRES')
!
   END SUBROUTINE RAISE_ERROR

   !> summary: Terminates the run, distinguishing fatal from ordinary exits.
   !>
   !> A positive `error_number` selects error termination through `ERROR STOP`,
   !> so that the process reports a nonzero status to whatever launched it.
   !> Omitting the argument selects an ordinary `STOP`. [[mod_error:ERROR]]
   !> passes `1` after it has printed the fatal-error summary; the
   !> unrecoverable conditions detected directly in the process modules pass
   !> `255`.
   !>
   !> When `flag_wait_on_exit` has been set through
   !> [[mod_error:err_set_wait_on_exit]], the routine prompts and blocks on
   !> standard input first, so that an interactively launched console window
   !> does not close before the diagnostics can be read. `error_mode` (the
   !> `-error` command-line option) suppresses that wait unconditionally, which
   !> keeps scripted and batch runs noninteractive.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 1994-09-17 | RAH | Created the v3.4.1 routine; the dated source note was added on 1994-09-30. |
   !> | 2000-03-07 | SB | Removed the legacy IEEE calls for the v4g-pc version. |
   !> | 2026-03-28 | SvB | Converted `FLAG` to selected integer kind with input intent, replaced the legacy pause with an explicit prompt/read, and added the initial FORD block. |
   !> | 2026-05-08 | SB | Skipped the interactive prompt when `error_mode` (the `-error` command-line flag) was set. |
   !> | 2026-08-31 | SvB | Made the argument optional, split fatal from ordinary termination, and gated the wait on `flag_wait_on_exit`. |
   !> @endhistory
   SUBROUTINE ERR_STOP(error_number)
      INTEGER(KIND=I_P), INTENT(IN), OPTIONAL :: error_number !! Termination code; positive requests fatal error termination.

      LOGICAL :: is_fatal !! Whether to take the error-termination path.

      is_fatal = .FALSE.
      IF (PRESENT(error_number)) is_fatal = (error_number > 0)

      IF (flag_wait_on_exit .AND. .NOT. error_mode) THEN
         IF (is_fatal) THEN
            WRITE (*, '(A)') 'FATAL ERROR: Program will terminate. Press Enter to exit...'
         ELSE
            WRITE (*, '(A)') 'Program will terminate. Press Enter to exit...'
         END IF
         READ (*, *)
      END IF

      IF (is_fatal) STOP 'Program terminating due to fatal error'

      STOP 'Program terminating'

   END SUBROUTINE ERR_STOP

END MODULE mod_error
