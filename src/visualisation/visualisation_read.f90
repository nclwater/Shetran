!> summary: Visualisation plan-file reading utilities.
!>
!> This module provides the small parser used by the visualisation metadata
!> layer. It strips comments and separators from the visualisation plan file,
!> then exposes typed readers for character, integer, and real values while
!> collecting consistent diagnostic messages for malformed input.
!>
!> Parser flow:
!>
!> | Stage | Behaviour |
!> |:------|:----------|
!> | `COPY` | Calls `STRIP` for the selected visualisation plan file and opens the stripped stream on `vp_in`. |
!> | `STRIP` | Checks the first token, removes `!` comments, trims blanks, splits on `:` and `^`, and writes `temporary.txt`. |
!> | `R_C` | Reads the next nonblank character plus the remaining character token. |
!> | `R_I` / `R_R` | Read scalar, vector, or two-to-five-value integer/real sequences through generic interfaces. |
!> | `ERROR` | Writes accumulated diagnostics to the check file and console, then stops. |
!>
!> `STRIP` accepts printable ASCII characters 32:126 only; tabs and other
!> non-printing characters are treated as fatal input errors.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 199912 | JE | - | Created line-by-line stripping utility for informative text in data files. |
!> | 200407 | JE | SHEGRAPH 2.0 | Created visualisation plan reader for SHEGRAPH. |
!> | 20050809 | NETT | - | Preserved zero-length-line guards while trimming blanks in `STRIP`. |
!> @endhistory
MODULE visualisation_read

   USE visualisation_read_parser, ONLY: visualisation_token_reader, transform_visualisation_record, &
                                        visualisation_title_matches, VIS_READ_OK, VIS_READ_END, &
                                        VIS_MAX_RECORD_LENGTH, VIS_RECORD_BUFFER_LENGTH

!JE for SHEGRAPH Version 2.0 Created July 2004
   IMPLICIT NONE

   INTEGER, PARAMETER :: vp_in = 48, vp_out = 49  !read and write numbers for visualisation_plan files
   CHARACTER(100)     :: mess = '', mess2 = '', mess3 = ''

   TYPE(visualisation_token_reader) :: reader

   INTERFACE R_C; MODULE PROCEDURE R_C; END INTERFACE
   INTERFACE R_I; MODULE PROCEDURE R_I_0, R_I_1, R_I_M; END INTERFACE
   INTERFACE R_R; MODULE PROCEDURE R_R_0, R_R_1, R_R_M; END INTERFACE

   PRIVATE
   PUBLIC :: vp_in, vp_out, mess, mess2, mess3, error_visualisation, R_C, R_I, R_R, COPY

CONTAINS

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE copy(dirqq, filename)
      CHARACTER(*), INTENT(IN) :: dirqq, filename

      CALL strip(file=filename, u=vp_in, checktitle='visualisation plan', delimiter='!', &
                 separator=(/':', '^'/), dir=dirqq)
      CALL reader%reset(vp_in)
   END SUBROUTINE copy

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_c(text, r)
      CHARACTER(*), INTENT(IN)  :: text
      CHARACTER(*), INTENT(OUT) :: r

      INTEGER :: status
      CHARACTER(:), ALLOCATABLE :: detail

      CALL reader%read_text(r, status, detail)
      IF (status /= VIS_READ_OK) CALL parser_error(text, 'text', status, detail)
   END SUBROUTINE r_c

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_ii(text, r)
      CHARACTER(*), INTENT(IN) :: text
      INTEGER, INTENT(OUT)     :: r

      INTEGER :: status
      CHARACTER(:), ALLOCATABLE :: detail

      CALL reader%read_integer(r, status, detail)
      IF (status /= VIS_READ_OK) CALL parser_error(text, 'integer', status, detail)
   END SUBROUTINE r_ii

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_rr(text, r)
      CHARACTER(*), INTENT(IN) :: text
      REAL, INTENT(OUT)        :: r

      INTEGER :: status
      CHARACTER(:), ALLOCATABLE :: detail

      CALL reader%read_real(r, status, detail)
      IF (status /= VIS_READ_OK) CALL parser_error(text, 'real', status, detail)
   END SUBROUTINE r_rr

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_i_0(text, r)
      CHARACTER(*), INTENT(IN) :: text
      INTEGER, INTENT(OUT)     :: r

      CALL r_ii(text, r)
   END SUBROUTINE r_i_0

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   SUBROUTINE r_i_m(text, i1, i2, i3, i4, i5)
      CHARACTER(*), INTENT(IN)       :: text
      INTEGER, INTENT(OUT)           :: i1, i2
      INTEGER, INTENT(OUT), OPTIONAL :: i3, i4, i5

      CALL R_I(text, i1)
      CALL R_I(text, i2)
      IF (PRESENT(i3)) CALL R_I(text, i3)
      IF (PRESENT(i4)) CALL R_I(text, i4)
      IF (PRESENT(i5)) CALL R_I(text, i5)
   END SUBROUTINE r_i_m

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_i_1(text, sz, r)
      CHARACTER(*), INTENT(IN)           :: text
      INTEGER, INTENT(IN)                :: sz
      INTEGER, DIMENSION(sz), INTENT(OUT) :: r

      INTEGER :: i

      DO i = 1, sz
         CALL R_I(text, r(i))
      END DO
   END SUBROUTINE r_i_1

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_r_0(text, r)
      CHARACTER(*), INTENT(IN) :: text
      REAL, INTENT(OUT)        :: r

      CALL r_rr(text, r)
   END SUBROUTINE r_r_0

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   SUBROUTINE r_r_m(text, r1, r2, r3, r4, r5)
      CHARACTER(*), INTENT(IN)    :: text
      REAL, INTENT(OUT)           :: r1, r2
      REAL, INTENT(OUT), OPTIONAL :: r3, r4, r5

      CALL R_R(text, r1)
      CALL R_R(text, r2)
      IF (PRESENT(r3)) CALL R_R(text, r3)
      IF (PRESENT(r4)) CALL R_R(text, r4)
      IF (PRESENT(r5)) CALL R_R(text, r5)
   END SUBROUTINE r_r_m

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_r_1(text, sz, r)
      CHARACTER(*), INTENT(IN)     :: text
      INTEGER, INTENT(IN)          :: sz
      REAL, DIMENSION(sz), INTENT(OUT) :: r

      INTEGER :: i

      DO i = 1, sz
         CALL R_R(text, r(i))
      END DO
   END SUBROUTINE r_r_1

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE parser_error(context, expected, status, detail)
      CHARACTER(*), INTENT(IN) :: context, expected, detail
      INTEGER, INTENT(IN)      :: status

      mess = ''
      mess2 = ''
      mess3 = ''
      IF (status == VIS_READ_END) THEN
         WRITE (mess, '(A,A,A)') TRIM(context), ' - unexpected end of file while reading ', TRIM(expected)
      ELSE
         WRITE (mess, '(A,A,A)') TRIM(context), ' - failed to read ', TRIM(expected)
         mess2 = detail
      END IF
      CALL error_visualisation()
   END SUBROUTINE parser_error

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE error_visualisation()
      CHARACTER(27), PARAMETER :: mm = '*** VISUALISATION ERROR ***'

      WRITE (vp_out, '(/A)') mm
      WRITE (vp_out, '(A)') TRIM(mess)
      WRITE (vp_out, '(A)') TRIM(mess2)
      WRITE (vp_out, '(A)') TRIM(mess3)
      PRINT '(/A)', mm
      PRINT '(A)', TRIM(mess)
      IF (mess2 /= '') PRINT '(A)', TRIM(mess2)
      IF (mess3 /= '') PRINT '(A)', TRIM(mess3)
      STOP
   END SUBROUTINE error_visualisation

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE strip(file, u, checktitle, delimiter, separator, dir)
      INTEGER, INTENT(IN)                :: u
      CHARACTER(*), INTENT(IN)           :: file
      CHARACTER(*), INTENT(IN)           :: checktitle
      CHARACTER, INTENT(IN)              :: delimiter
      CHARACTER, INTENT(IN)              :: separator(2)
      CHARACTER(*), INTENT(IN), OPTIONAL :: dir

      INTEGER :: source_unit, output_unit, ios, status, lineno, i
      CHARACTER(VIS_RECORD_BUFFER_LENGTH) :: record
      CHARACTER(512) :: iomsg, tempfile
      CHARACTER(:), ALLOCATABLE :: detail
      CHARACTER(VIS_MAX_RECORD_LENGTH), ALLOCATABLE :: segments(:)
      LOGICAL :: opened

      IF (PRESENT(dir)) THEN
         tempfile = TRIM(dir)//'/temporary.txt'
      ELSE
         tempfile = 'temporary.txt'
      END IF

      INQUIRE (UNIT=u, OPENED=opened)
      IF (opened) CLOSE (UNIT=u, STATUS='keep')

      iomsg = ''
      OPEN (NEWUNIT=source_unit, FILE=file, STATUS='old', ACTION='read', IOSTAT=ios, IOMSG=iomsg)
      IF (ios /= 0) THEN
         mess = 'failed to open '//TRIM(file)
         mess2 = TRIM(iomsg)
         CALL error_visualisation()
         RETURN
      END IF

      record = ''
      READ (source_unit, '(A)', IOSTAT=ios, IOMSG=iomsg) record
      IF (ios /= 0 .OR. .NOT. visualisation_title_matches(record, checktitle)) THEN
         mess = 'wrong key in '//TRIM(file)
         mess2 = 'Read '//TRIM(ADJUSTL(record))//' expecting '//TRIM(checktitle)
         IF (ios /= 0) mess3 = TRIM(iomsg)
         CLOSE (source_unit)
         CALL error_visualisation()
         RETURN
      END IF

      iomsg = ''
      OPEN (NEWUNIT=output_unit, FILE=TRIM(tempfile), STATUS='replace', ACTION='write', &
            IOSTAT=ios, IOMSG=iomsg)
      IF (ios /= 0) THEN
         mess = 'failed to create '//TRIM(tempfile)
         mess2 = TRIM(iomsg)
         CLOSE (source_unit)
         CALL error_visualisation()
         RETURN
      END IF

      lineno = 1
      DO
         record = ''
         iomsg = ''
         READ (source_unit, '(A)', IOSTAT=ios, IOMSG=iomsg) record
         IF (IS_IOSTAT_END(ios)) EXIT
         lineno = lineno + 1
         IF (ios /= 0) THEN
            WRITE (mess, '(A,I0,A)') 'failed to read line ', lineno, ' from '//TRIM(file)
            mess2 = TRIM(iomsg)
            CLOSE (source_unit)
            CLOSE (output_unit)
            CALL error_visualisation()
            RETURN
         END IF

         CALL transform_visualisation_record(record, delimiter, separator, segments, status, detail)
         IF (status /= VIS_READ_OK) THEN
            WRITE (mess, '(A,I0,A)') 'invalid input at line ', lineno, ' in '//TRIM(file)
            mess2 = detail
            IF (INDEX(detail, 'ASCII character 9 ') > 0) &
               mess3 = 'This is probably a tab character - remove or replace it with spaces'
            CLOSE (source_unit)
            CLOSE (output_unit)
            CALL error_visualisation()
            RETURN
         END IF

         DO i = 1, SIZE(segments)
            iomsg = ''
            WRITE (output_unit, '(A)', IOSTAT=ios, IOMSG=iomsg) TRIM(segments(i))
            IF (ios /= 0) THEN
               mess = 'failed to write '//TRIM(tempfile)
               mess2 = TRIM(iomsg)
               CLOSE (source_unit)
               CLOSE (output_unit)
               CALL error_visualisation()
               RETURN
            END IF
         END DO
      END DO

      CLOSE (source_unit)
      CLOSE (output_unit)

      iomsg = ''
      OPEN (UNIT=u, FILE=TRIM(tempfile), STATUS='old', ACTION='readwrite', IOSTAT=ios, IOMSG=iomsg)
      IF (ios /= 0) THEN
         mess = 'failed to open stripped visualisation plan '//TRIM(tempfile)
         mess2 = TRIM(iomsg)
         CALL error_visualisation()
      END IF
   END SUBROUTINE strip

END MODULE visualisation_read
