!> summary: Standardised `IOSTAT`/`STAT` checks that report through [[error_reporting]].
!> author: Sven Berendsen, Southampton University
!>
!> One wrapper per I/O and allocation operation: pass it the status the
!> statement returned and it reports a numbered diagnostic if the status is
!> nonzero, and does nothing if it is not. Most of the tree reaches error
!> handling through these rather than by calling [[error_reporting:RAISE_ERROR]]
!> directly, which is why they are the half of the former `mod_error` with the
!> wider consumer list.
!>
!> The dependency runs one way only — this module calls
!> [[error_reporting]] and is never called back — so the two halves can be
!> compiled and read separately.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of mod_error; see docs/rename/proposal.md. |
!> @endhistory
MODULE error_status

   USE MOD_PARAMETERS, ONLY: I_P, LENGTH_FILEPATH, LENGTH_LINE
   USE file_units, ONLY: FID_logfile
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal
   USE stdlib_strings, ONLY: to_string

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: errstat_fileopen, errstat_fileclose
   PUBLIC :: errstat_alloc, errstat_dealloc
   PUBLIC :: errstat_read, errstat_write
   PUBLIC :: errstat_rewind


   ! --------------------------------------------------------------------
   ! Diagnostic codes reported by the standardised status checks
   !
   ! These sit in the general library group (0000--0100), clear of the
   ! codes 1--14 already issued by the legacy readers in [[record_readers]].
   ! --------------------------------------------------------------------
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_fileopen = 20 !! Code reported for a failed file open.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_fileclose = 21 !! Code reported for a failed file close.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_allocate = 22 !! Code reported for a failed allocation.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_deallocate = 23 !! Code reported for a failed deallocation.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_read = 24 !! Code reported for a failed read.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_write = 25 !! Code reported for a failed write.
   INTEGER(KIND=I_P), PARAMETER :: ERRCODE_rewind = 26 !! Code reported for a failed rewind.

CONTAINS

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
   !> Both `filename` and `fid` are optional, but at least one of them should be
   !> given so that the report identifies the file. If `filename` is absent the
   !> name is recovered from `fid` with an `INQUIRE`, which works as long as the
   !> unit is still connected to a named file. Where no name can be established
   !> the unit number is reported instead.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-08-31 | SvB | Initial version. |
   !> | 2026-09-06 | SvB | Report the `status` value and the optional `IOMSG=` text. |
   !> | 2026-09-06 | SvB | Added `fid` and derive the filename from it via `INQUIRE`. |
   !> @endhistory
   SUBROUTINE errstat_fileclose(status, filename, fid, iomsg)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! Return status from file closing.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename !! Name of the file being closed.
      INTEGER(KIND=I_P), INTENT(IN), OPTIONAL :: fid !! Unit ID of the file being closed.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: iomsg !! Text from the `IOMSG=` specifier of the failing `CLOSE`.

      CHARACTER(LEN=LENGTH_LINE) :: msg !! Constructed message for the error report.
      CHARACTER(LEN=LENGTH_FILEPATH) :: name !! Name of the file, either given or inquired from `fid`.
      INTEGER(KIND=I_P) :: stat_inquire !! Return status of the `INQUIRE` recovering the name.
      LOGICAL :: is_open !! `.TRUE.` if `fid` is still connected to a file.
      LOGICAL :: is_named !! `.TRUE.` if the file connected to `fid` has a name.

      IF (status /= 0) THEN
         name = ''
         IF (PRESENT(filename)) THEN
            name = filename
         ELSE IF (PRESENT(fid)) THEN
            INQUIRE (UNIT=fid, OPENED=is_open, NAMED=is_named, NAME=name, IOSTAT=stat_inquire)
            IF (stat_inquire /= 0 .OR. .NOT. is_open .OR. .NOT. is_named) name = ''
         END IF

         IF (LEN_TRIM(name) > 0) THEN
            msg = 'Error closing file: '//TRIM(name)
         ELSE IF (PRESENT(fid)) THEN
            msg = 'Error closing file on unit '//to_string(fid)
         ELSE
            msg = 'Error closing file'
         END IF

         msg = TRIM(msg)//' (status '//to_string(status)//')'
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

   !> summary: Standardised check for rewinding file return status.
   !> author: S. Berendsen, Southampton University
   !>
   !> Standardised check for rewinding file return status.
   !>
   !> Pass `iomsg` the string filled by the `IOMSG=` specifier of the failing
   !> `REWIND` statement to have the processor's explanatory text for `status`
   !> included in the diagnostic.
   !>
   !> Both `filename` and `fid` are optional, but at least one of them should be
   !> given so that the report identifies the file. If `filename` is absent the
   !> name is recovered from `fid` with an `INQUIRE`, which works as long as the
   !> unit is still connected to a named file. Where no name can be established
   !> the unit number is reported instead.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-09-07 | SvB | Initial version. |
   !> @endhistory
   SUBROUTINE errstat_rewind(status, filename, fid, iomsg)
      INTEGER(KIND=I_P), INTENT(IN) :: status !! Return status from file rewinding.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename !! Name of the file being rewound.
      INTEGER(KIND=I_P), INTENT(IN), OPTIONAL :: fid !! Unit ID of the file being rewound.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: iomsg !! Text from the `IOMSG=` specifier of the failing `REWIND`.

      CHARACTER(LEN=LENGTH_LINE) :: msg !! Constructed message for the error report.
      CHARACTER(LEN=LENGTH_FILEPATH) :: name !! Name of the file, either given or inquired from `fid`.
      INTEGER(KIND=I_P) :: stat_inquire !! Return status of the `INQUIRE` recovering the name.
      LOGICAL :: is_open !! `.TRUE.` if `fid` is still connected to a file.
      LOGICAL :: is_named !! `.TRUE.` if the file connected to `fid` has a name.

      IF (status /= 0) THEN
         name = ''
         IF (PRESENT(filename)) THEN
            name = filename
         ELSE IF (PRESENT(fid)) THEN
            INQUIRE (UNIT=fid, OPENED=is_open, NAMED=is_named, NAME=name, IOSTAT=stat_inquire)
            IF (stat_inquire /= 0 .OR. .NOT. is_open .OR. .NOT. is_named) name = ''
         END IF

         IF (LEN_TRIM(name) > 0) THEN
            msg = 'Error rewinding file: '//TRIM(name)
         ELSE IF (PRESENT(fid)) THEN
            msg = 'Error rewinding file on unit '//to_string(fid)
         ELSE
            msg = 'Error rewinding file'
         END IF

         msg = TRIM(msg)//' (status '//to_string(status)//')'
         IF (PRESENT(iomsg)) THEN
            IF (LEN_TRIM(iomsg) > 0) msg = TRIM(msg)//': '//TRIM(iomsg)
         END IF
         CALL RAISE_ERROR(ERRLVL_fatal, ERRCODE_rewind, FID_logfile, 0, 0, TRIM(msg))
      END IF
   END SUBROUTINE errstat_rewind

END MODULE error_status

