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

   IMPLICIT NONE

   INTEGER, PARAMETER :: vp_in=48  !! Unit number for the stripped visualisation-plan input stream.
   INTEGER, PARAMETER :: vp_out=49 !! Unit number for the visualisation check-file output stream.
   LOGICAL, PARAMETER :: T=.TRUE.  !! Short logical true constant retained for legacy parser code.
   LOGICAL, PARAMETER :: F=.FALSE. !! Short logical false constant retained for legacy parser code.
   CHARACTER(100)     :: mess=''   !! Primary parser diagnostic message.
   CHARACTER(100)     :: mess2=''  !! Secondary parser diagnostic message.
   CHARACTER(100)     :: mess3=''  !! Tertiary parser diagnostic message.
   CHARACTER, PARAMETER :: di(10)=(/'0','1','2','3','4','5','6','7','8','9'/) !! Integer digit characters.
   CHARACTER, PARAMETER :: dr(12)=(/'-','.','0','1','2','3','4','5','6','7','8','9'/) !! Real-token characters.

   INTERFACE R_C ; MODULE PROCEDURE R_C                      ; ENDINTERFACE
   INTERFACE R_I ; MODULE PROCEDURE R_I_0, R_I_1, R_I_M ; ENDINTERFACE
   INTERFACE R_R ; MODULE PROCEDURE R_R_0, R_R_1, R_R_M ; ENDINTERFACE


   PRIVATE
   PUBLIC :: vp_in, vp_out, mess, mess2, mess3, ERROR, R_C, R_I, R_R, COPY

CONTAINS


!> Opens and strips the visualisation plan file into the parser input stream.
   SUBROUTINE copy(dirqq, filename)
      CHARACTER(*), INTENT(IN) :: dirqq !! Catchment directory used for temporary parser files.
      CHARACTER(*), INTENT(IN) :: filename !! Visualisation plan filename to read.
      CALL STRIP(file=filename, u=vp_in, checktitle='visualisation plan', delimiter='!', separator=(/':','^'/), DIR=dirqq)
   END SUBROUTINE copy

!> Reads a character token from the stripped visualisation plan stream.
   SUBROUTINE r_c(text, r)
      INTEGER                   :: i    !! Unused legacy workspace.
      LOGICAL                   :: eor  !! Unused legacy end-of-record flag.
      CHARACTER(*), INTENT(IN)  :: text !! Field name used in diagnostics.
      CHARACTER(*), INTENT(OUT) :: r    !! Character value read from the stream.
      CHARACTER                 :: c    !! Unused legacy character workspace.
      CALL READ_A_LINE(text, r)
   END SUBROUTINE r_c

!> Reads a nonblank character and then the remainder of a character value.
   SUBROUTINE read_a_line(text, r)
      CHARACTER(*), INTENT(IN)  :: text !! Field name used in diagnostics.
      CHARACTER(*), INTENT(OUT) :: r    !! Character value read from the stream.
      CHARACTER                 :: c    !! First nonblank character.
      CALL FIND_FIRST_CHARACTER(text, c, exclude=' ')
      r(1:1) = c
      IF(LEN(r)>1) READ(vp_in,*) r(2:)
   END SUBROUTINE read_a_line


!> Reads one integer token from the stripped visualisation plan stream.
!>
!> Integer tokens are buffered in an eight-character field. Values longer than
!> this or containing a non-digit character produce a fatal parser error.
   SUBROUTINE r_ii(text, r)
      INTEGER, PARAMETER             :: szb = 8 !! Maximum integer-token length.
      INTEGER, INTENT(OUT)           :: r       !! Integer value read.
      INTEGER                        :: i       !! Token character count.
      CHARACTER(*), INTENT(IN)       :: text    !! Field name used in diagnostics.

      CHARACTER                      :: c       !! Current input character.
      CHARACTER(szb)                 :: b       !! Buffered token text.
      b   = REPEAT(' ',szb)
      CALL FIND_FIRST_CHARACTER(text, c, di)
      i = 0
      DO WHILE(c/=' ')
         IF(.NOT.ANY(c==di)) GOTO 95
         i=i+1
         IF(i>szb) GOTO 95
         b(i:i)=c
         READ(vp_in,'(A1)',ERR=90, EOR=80, ADVANCE='NO') c
      ENDDO
80    READ(b,*) r

      RETURN
90    WRITE(mess,*) 'Error when trying to read integer'//TRIM(text)    ; GOTO 100
95    b(i+1:i+1)=c
      WRITE(mess,*) TRIM(text)//' - Expecting integer, but read '//b ; GOTO 100
100   CALL ERROR()
   END SUBROUTINE r_ii

!> Advances the parser to the next accepted character.
!>
!> End-of-record recurses until a character can be read. When `exclude` is
!> present, excluded characters are skipped. The optional `d` argument is passed
!> by numeric readers as an allowed-character set, but the legacy test is kept as
!> implemented and validation is performed by the caller.
   RECURSIVE SUBROUTINE find_first_character(text, c, d, exclude)
      CHARACTER, INTENT(OUT)                        :: c       !! Character found in the stream.
      CHARACTER, DIMENSION(:), INTENT(IN), OPTIONAL :: d       !! Legacy character set argument from numeric readers.
      CHARACTER(*), INTENT(IN)                      :: text    !! Field name used in diagnostics.
      CHARACTER, INTENT(IN), OPTIONAL               :: exclude !! Character to skip.
      READ(vp_in,'(A1)',ERR=90, EOR=92, ADVANCE='NO') c
      DO
         IF (PRESENT(d)) THEN
            IF(ANY(c/=d)) EXIT
         ELSEIF(PRESENT(exclude)) THEN
            if(c/=exclude) EXIT
         ENDIF
         READ(vp_in,'(A1)',ERR=90, EOR=92, ADVANCE='NO') c
      ENDDO
      RETURN
92    CALL FIND_FIRST_CHARACTER(text, c, d, exclude)
      RETURN
90    WRITE(mess,*) 'Error when trying to read integer'//TRIM(text)    ; GOTO 100
100   CALL ERROR()
   END SUBROUTINE find_first_character



!> Reads one real token from the stripped visualisation plan stream.
!>
!> Real tokens are buffered in a 20-character field and may contain digits,
!> decimal point, and minus sign. Exponents are not in the accepted character
!> set, matching the legacy parser.
   SUBROUTINE r_rr(text,r)
      INTEGER, PARAMETER             :: szb = 20 !! Maximum real-token length.
      INTEGER                        :: i        !! Token character count.
      REAL, INTENT(OUT)              :: r        !! Real value read.
      CHARACTER(*), INTENT(IN)       :: text     !! Field name used in diagnostics.
      CHARACTER                      :: c        !! Current input character.
      CHARACTER(szb)                 :: b        !! Buffered token text.
      b   = REPEAT(' ',szb)
      CALL FIND_FIRST_CHARACTER(text, c, dr)
      i = 0
      DO WHILE(c/=' ')
         IF(.NOT.ANY(c==dr)) GOTO 95
         i=i+1
         IF(i>szb) GOTO 95
         b(i:i)=c
         READ(vp_in,'(A1)',ERR=90, EOR=80, ADVANCE='NO') c
      ENDDO
80    READ(b,*) r

      RETURN
90    WRITE(mess,*) 'Error when trying to read real'//TRIM(text)    ; GOTO 100
95    WRITE(mess,*) TRIM(text)//' - Expecting real, but read '//b ; GOTO 100
100   CALL ERROR()
   END SUBROUTINE r_rr

!> Interface wrapper for reading a scalar integer.
   SUBROUTINE r_i_0(text, r)
      INTEGER, INTENT(OUT)     :: r    !! Integer value read.
      CHARACTER(*), INTENT(IN) :: text !! Field name used in diagnostics.
      CALL R_II(text, r)
   END SUBROUTINE r_i_0

!> Interface wrapper for reading two to five scalar integers.
   SUBROUTINE r_i_m(text, i1, i2, i3, i4, i5)
      INTEGER, INTENT(OUT)           :: i1   !! First integer value read.
      INTEGER, INTENT(OUT)           :: i2   !! Second integer value read.
      INTEGER, INTENT(OUT), OPTIONAL :: i3   !! Optional third integer value read.
      INTEGER, INTENT(OUT), OPTIONAL :: i4   !! Optional fourth integer value read.
      INTEGER, INTENT(OUT), OPTIONAL :: i5   !! Optional fifth integer value read.
      CHARACTER(*), INTENT(IN)       :: text !! Field name used in diagnostics.
      CALL R_I(text, I1)
      CALL R_I(text, i2)
      IF(PRESENT(i3)) CALL R_I(text, i3)
      IF(PRESENT(i4)) CALL R_I(text, i4)
      IF(PRESENT(i5)) CALL R_I(text, i5)
   END SUBROUTINE r_i_m

!> Interface wrapper for reading an integer vector.
   SUBROUTINE r_i_1(text, sz, r)
      INTEGER, INTENT(IN)                 :: sz   !! Number of integer values to read.
      INTEGER, DIMENSION(sz), INTENT(OUT) :: r    !! Integer values read.
      INTEGER                             :: i    !! Vector index.
      CHARACTER(*), INTENT(IN)            :: text !! Field name used in diagnostics.
      DO i=1,sz
         CALL R_I(text, r(i))
      ENDDO
   END SUBROUTINE r_i_1

!> Interface wrapper for reading a scalar real.
   SUBROUTINE r_r_0(text, r)
      REAL, INTENT(OUT)        :: r    !! Real value read.
      CHARACTER(*), INTENT(IN) :: text !! Field name used in diagnostics.
      CALL R_RR(text, r)
   END SUBROUTINE r_r_0

!> Interface wrapper for reading two to five scalar reals.
   SUBROUTINE r_r_m(text, r1, r2, r3, r4, r5)
      REAL, INTENT(OUT)           :: r1   !! First real value read.
      REAL, INTENT(OUT)           :: r2   !! Second real value read.
      REAL, INTENT(OUT), OPTIONAL :: r3   !! Optional third real value read.
      REAL, INTENT(OUT), OPTIONAL :: r4   !! Optional fourth real value read.
      REAL, INTENT(OUT), OPTIONAL :: r5   !! Optional fifth real value read.
      CHARACTER(*), INTENT(IN)    :: text !! Field name used in diagnostics.
      CALL R_R(text, r1)
      CALL R_R(text, r2)
      IF(PRESENT(r3)) CALL R_R(text, r3)
      IF(PRESENT(r4)) CALL R_R(text, r4)
      IF(PRESENT(r5)) CALL R_R(text, r5)
   END SUBROUTINE r_r_m

!> Interface wrapper for reading a real vector.
   SUBROUTINE r_r_1(text, sz, r)
      INTEGER, INTENT(IN)              :: sz   !! Number of real values to read.
      REAL, DIMENSION(sz), INTENT(OUT) :: r    !! Real values read.
      INTEGER                          :: i    !! Vector index.
      CHARACTER(*), INTENT(IN)         :: text !! Field name used in diagnostics.
      DO i=1,sz
         CALL R_R(text, r(i))
      ENDDO
   END SUBROUTINE r_r_1

!> Reports a visualisation parser error and stops the program.
   SUBROUTINE error()
      CHARACTER(27), PARAMETER :: mm='*** VISUALISATION ERROR ***' !! Parser error banner.
      WRITE(vp_out,'(/A)') mm
      WRITE(vp_out,88) TRIM(mess)
      WRITE(vp_out,88) TRIM(mess2)
      WRITE(vp_out,88) TRIM(mess3)
      PRINT '(/A)', mm
      PRINT 88, TRIM(mess)
      IF(mess2/='') PRINT 88, TRIM(mess2)
      IF(mess3/='') PRINT 88, TRIM(mess3)
      STOP
88    FORMAT(A)
   END SUBROUTINE error

!> Strips comments, line separators, and leading/trailing blanks from an input file.
!>
!> The stripped content is written to a temporary file and reopened on unit `u`
!> so the typed readers can consume a compact token stream.
!>
!> Entry conditions:
!>
!> | Item | Requirement |
!> |:-----|:------------|
!> | `file` | Existing input file whose first token equals `checktitle`. |
!> | `delimiter` | Character that begins an ignored comment tail on each line. |
!> | `separator` | Two characters that split one physical line into multiple parser records. |
!> | `dir` | Optional directory used for `temporary.txt`; current code appends `/temporary.txt`. |
   SUBROUTINE strip(file, u, checktitle, delimiter, separator, dir)
      IMPLICIT NONE
      INTEGER, INTENT(IN)         :: u      !! Unit number for the stripped read file.
      INTEGER                     :: i      !! Current input/write start index.
      INTEGER                     :: j      !! Last nonblank character position on the current line.
      INTEGER                     :: k      !! Separator-search index.
      INTEGER                     :: llen=500 !! Maximum allowed input line length.
      INTEGER                     :: io=0   !! Input I/O status; `-1` indicates end-of-file.
      INTEGER                     :: nunit=100 !! Unit number for the temporary stripped file.
      INTEGER                     :: ichar  !! ASCII code for the current character.
      INTEGER                     :: lineno !! Physical input-file line number.
      CHARACTER (*), INTENT(IN)   :: file   !! Filename to strip and reopen.
      CHARACTER (*), INTENT(IN)   :: checktitle !! Expected first token in `file`.
      CHARACTER(*), INTENT(IN), OPTIONAL :: dir !! Optional directory for the temporary stripped file.
      CHARACTER,     INTENT(IN)   :: delimiter  !! Character marking the beginning of an ignored comment tail.
      CHARACTER,     INTENT(IN)   :: separator(2) !! Characters that split one input line into parser records.
      CHARACTER                   :: ch     !! Current input character.
      CHARACTER (LEN(checktitle)) :: dum    !! First token read from `file`.
      CHARACTER, DIMENSION(:), ALLOCATABLE :: store !! Current physical input line before splitting.
      CHARACTER(13), PARAMETER             :: tf='temporary.txt' !! Temporary stripped filename.
      CHARACTER(250)                       :: tempfile !! Full temporary-file path.
      LOGICAL                              :: opened   !! True when a unit is already open.

      IF(PRESENT(dir)) THEN
         tempfile=TRIM(dir)//'/'//tf
      ELSE
         tempfile = tf
      ENDIF

      INQUIRE(UNIT=u, OPENED=opened) ; IF (opened) CLOSE(UNIT=u, STATUS='keep')
      OPEN(UNIT=u, FILE=file, STATUS='OLD', ERR=910)
      READ(u,*) dum
      IF(dum/=checktitle) GOTO 900

      INQUIRE(UNIT=nunit, OPENED=opened) ; IF (opened) CLOSE(UNIT=nunit, STATUS='keep')
      OPEN(UNIT=nunit, FILE=TRIM(tempfile), STATUS='REPLACE')

      io=0
      lineno = 1
      ALLOCATE (store(llen))
      DO WHILE (io/=-1)                               !io-1 is for end-of-file
         lineno = lineno + 1
         i=0
         READ(u,'(A1)', IOSTAT=IO, ADVANCE='NO') ch
         DO WHILE (ch/=delimiter .AND. IO==0)
            i=i+1
            IF(i>llen) THEN
               mess = 'System message: input data line too long in STRIP'
               CALL ERROR()
            ENDIF
            ichar = IACHAR(ch)
            IF(ichar<32 .OR. ichar>126) THEN
               WRITE(mess, '(A,I3)')  TRIM(file)//' contains ASCII character number ',ichar
               WRITE(mess2,'(A,I3,A,I4)') 'At character position ', i, ' in line ', lineno
               IF(ichar==9) WRITE(mess3,'(A)') 'This is probably a tab character - remove or replace with spaces'
               CALL ERROR()
            ENDIF
            store(i) = ch
            READ(u,'(A1)', IOSTAT=IO, ADVANCE='NO') ch
         END DO
         IF(io==0) READ(u,'(A1)', IOSTAT=IO, ADVANCE='YES')  !to item up for next input line
         j=i
         IF(j>0) THEN
            DO WHILE (j>0 .AND. store(j)==' ')           !strip off trailing blanks
               j=j-1
               IF(j==0) EXIT
            ENDDO
         ENDIF
         i=1
         DO WHILE (i<j .AND. store(i)==' ')           !strip off leading blanks
            i = i+1
         ENDDO

         k = i
         DO WHILE(k<=j)
            DO WHILE (ALL(store(k)/=separator) .AND. k<=j)     !find line breaks
               k = k+1
            ENDDO
            IF(k>i) WRITE(nunit,99)store(i:k-1)
            k = k+1
            DO WHILE (k<j .AND. store(k)==' ')     !strip off leading blanks
               k = k+1
            ENDDO
            i = k
         ENDDO
      ENDDO
      CLOSE (UNIT=u, STATUS='keep') ; CLOSE (UNIT=nunit)
      OPEN(UNIT=u, FILE=TRIM(tempfile))
      DEALLOCATE (store)
      RETURN
900   mess = 'wrong key in '//TRIM(file)
      mess2 = ' Read '//TRIM(dum)//' expecting '//TRIM(checktitle)
      GOTO 1000
910   mess = ' failed to open '//TRIM(file)
1000  CALL ERROR()
99    FORMAT(1000A)
   END SUBROUTINE strip
END MODULE visualisation_read
