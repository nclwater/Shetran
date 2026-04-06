!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_read

   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END


!JE for SHEGRAPH Version 2.0 Created July 2004
   IMPLICIT NONE

   INTEGER, PARAMETER :: vp_in=48, vp_out=49  !read and write numbers for visualisation_plan files
   LOGICAL, PARAMETER :: T=.TRUE., F=.FALSE.
   CHARACTER(100)     :: mess='', mess2='', mess3=''
   CHARACTER, PARAMETER           :: di(10)=(/'0','1','2','3','4','5','6','7','8','9'/), &
      dr(12)=(/'-','.','0','1','2','3','4','5','6','7','8','9'/)

   INTERFACE R_C ; MODULE PROCEDURE R_C                      ; ENDINTERFACE
   INTERFACE R_I ; MODULE PROCEDURE R_I_0, R_I_1, R_I_M ; ENDINTERFACE
   INTERFACE R_R ; MODULE PROCEDURE R_R_0, R_R_1, R_R_M ; ENDINTERFACE


   PRIVATE
   PUBLIC :: vp_in, vp_out, mess, mess2, mess3, ERROR, R_C, R_I, R_R, COPY

CONTAINS


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE copy(dirqq, filename)
      CHARACTER(*), INTENT(IN) :: dirqq, filename
!CALL STRIP(file=TRIM(dirqq)//'input/visualisation_plan.txt', u=vp_in, checktitle='visualisation plan', delimiter='!', separator=(/':','^'/), DIR=dirqq)
      CALL STRIP(file=filename, u=vp_in, checktitle='visualisation plan', delimiter='!', separator=(/':','^'/), DIR=dirqq)
   END SUBROUTINE copy

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_c(text, r)
      INTEGER                   :: i
      LOGICAL                   :: eor
      CHARACTER(*), INTENT(IN)  :: text
      CHARACTER(*), INTENT(OUT) :: r
      CHARACTER                 :: c
      CALL READ_A_LINE(text, r)
   END SUBROUTINE r_c

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE read_a_line(text, r)
      CHARACTER(*), INTENT(IN)  :: text
      CHARACTER(*), INTENT(OUT) :: r
      CHARACTER                 :: c
      CALL FIND_FIRST_CHARACTER(text, c, exclude=' ')
      r(1:1) = c
      IF(LEN(r)>1) READ(vp_in,*) r(2:)
   END SUBROUTINE read_a_line


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE r_ii(text, r)
      ! Read an integer
      ! Note: Assuming vp_in, mess, and di are made available via host association/modules
      IMPLICIT NONE 

      INTEGER, PARAMETER       :: szb = 8
      INTEGER, INTENT(OUT)     :: r
      CHARACTER(*), INTENT(IN) :: text

      INTEGER                  :: i, ios
      CHARACTER                :: c
      CHARACTER(szb)           :: b

      b = REPEAT(' ', szb)
      CALL FIND_FIRST_CHARACTER(text, c, di)
      i = 0

      read_loop: DO WHILE (c /= ' ')
         
         ! Check for non-digit characters OR buffer overflow (replacing GOTO 95)
         IF (.NOT. ANY(c == di) .OR. i >= szb) THEN
            ! Safely append the bad character only if we have room
            IF (i < szb) b(i + 1 : i + 1) = c
            WRITE(mess,*) TRIM(text) // ' - Expecting integer, but read ' // TRIM(b)
            CALL ERROR()
            RETURN
         END IF

         i = i + 1
         b(i:i) = c

         ! Read the next character using IOSTAT instead of ERR/EOR jump labels
         READ(vp_in, '(A1)', IOSTAT=ios, ADVANCE='NO') c

         IF (ios > 0) THEN
            ! Hard Read Error (replacing ERR=90)
            WRITE(mess,*) 'Error when trying to read integer ' // TRIM(text)
            CALL ERROR()
            RETURN
         ELSE IF (ios < 0) THEN
            ! End of Record (EOR) or End of File (EOF) (replacing EOR=80)
            EXIT read_loop
         END IF

      END DO read_loop

      ! Parse the final integer (replacing label 80)
      READ(b, *) r

   END SUBROUTINE r_ii



!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   RECURSIVE SUBROUTINE find_first_character(text, c, d, exclude)

      ! Assumed external module dependencies providing global variables:
      ! vp_in, mess, ERROR
      
      IMPLICIT NONE

      CHARACTER, INTENT(OUT)                        :: c
      CHARACTER, DIMENSION(:), INTENT(IN), OPTIONAL :: d
      CHARACTER(*), INTENT(IN)                      :: text
      CHARACTER, INTENT(IN), OPTIONAL               :: exclude ! don't count this as a character

      INTEGER :: ios

      !----------------------------------------------------------------------*

      read_loop: DO
         
         ! Single READ statement handling all iterations using IOSTAT
         READ(vp_in, '(A1)', IOSTAT=ios, ADVANCE='NO') c

         IF (ios > 0) THEN
            ! Hard Read Error (replacing ERR=90 & GOTO 100)
            WRITE(mess,*) 'Error when trying to read integer' // TRIM(text)
            CALL ERROR()
            RETURN
            
         ELSE IF (ios < 0) THEN
            ! End of Record (EOR) or End of File (EOF) (replacing EOR=92)
            CALL find_first_character(text, c, d, exclude)
            RETURN
         END IF

         ! Process the valid character
         IF (PRESENT(d)) THEN
            IF (ANY(c /= d)) EXIT read_loop
         ELSE IF (PRESENT(exclude)) THEN
            IF (c /= exclude) EXIT read_loop
         END IF
         
      END DO read_loop

   END SUBROUTINE find_first_character



!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE r_rr(text, r)
      ! Read a real
      ! Assumed external module dependencies providing global variables:
      ! vp_in, mess, dr, ERROR
      IMPLICIT NONE

      INTEGER, PARAMETER       :: szb = 20
      REAL, INTENT(OUT)        :: r
      CHARACTER(*), INTENT(IN) :: text
      
      INTEGER                  :: i, ios
      CHARACTER                :: c
      CHARACTER(szb)           :: b

      !----------------------------------------------------------------------*

      b = REPEAT(' ', szb)
      CALL find_first_character(text, c, dr)
      i = 0

      read_loop: DO WHILE (c /= ' ')
         
         ! Check for non-valid characters OR buffer overflow (replacing GOTO 95)
         IF (.NOT. ANY(c == dr) .OR. i >= szb) THEN
            WRITE(mess,*) TRIM(text) // ' - Expecting real, but read ' // TRIM(b)
            CALL ERROR()
            RETURN
         END IF

         i = i + 1
         b(i:i) = c

         ! Read the next character using IOSTAT instead of ERR/EOR jump labels
         READ(vp_in, '(A1)', IOSTAT=ios, ADVANCE='NO') c

         IF (ios > 0) THEN
            ! Hard Read Error (replacing ERR=90)
            WRITE(mess,*) 'Error when trying to read real' // TRIM(text)
            CALL ERROR()
            RETURN
         ELSE IF (ios < 0) THEN
            ! End of Record (EOR) or End of File (EOF) (replacing EOR=80)
            EXIT read_loop
         END IF

      END DO read_loop

      ! Parse the final real (replacing label 80)
      READ(b, *) r

   END SUBROUTINE r_rr



!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_i_0(text, r)
      INTEGER, INTENT(OUT)     :: r
      CHARACTER(*), INTENT(IN) :: text
      CALL R_II(text, r)
   END SUBROUTINE r_i_0

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   SUBROUTINE r_i_m(text, i1, i2, i3, i4, i5)
      INTEGER, INTENT(OUT)           :: i1, i2
      INTEGER, INTENT(OUT), OPTIONAL :: i3, i4, i5
      CHARACTER(*), INTENT(IN)       :: text
      CALL R_I(text, I1)
      CALL R_I(text, i2)
      IF(PRESENT(i3)) CALL R_I(text, i3)
      IF(PRESENT(i4)) CALL R_I(text, i4)
      IF(PRESENT(i5)) CALL R_I(text, i5)
   END SUBROUTINE r_i_m

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_i_1(text, sz, r)
      INTEGER, INTENT(IN)                 :: sz
      INTEGER, DIMENSION(sz), INTENT(OUT) :: r
      INTEGER                             :: i
      CHARACTER(*), INTENT(IN)            :: text
      DO i=1,sz
         CALL R_I(text, r(i))
      ENDDO
   END SUBROUTINE r_i_1

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_r_0(text, r)
      REAL, INTENT(OUT)     :: r
      CHARACTER(*), INTENT(IN) :: text
      CALL R_RR(text, r)
   END SUBROUTINE r_r_0

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   SUBROUTINE r_r_m(text, r1, r2, r3, r4, r5)
      REAL, INTENT(OUT)           :: r1, r2
      REAL, INTENT(OUT), OPTIONAL :: r3, r4, r5
      CHARACTER(*), INTENT(IN)    :: text
      CALL R_R(text, r1)
      CALL R_R(text, r2)
      IF(PRESENT(r3)) CALL R_R(text, r3)
      IF(PRESENT(r4)) CALL R_R(text, r4)
      IF(PRESENT(r5)) CALL R_R(text, r5)
   END SUBROUTINE r_r_m

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE r_r_1(text, sz, r)
      INTEGER, INTENT(IN)                 :: sz
      REAL, DIMENSION(sz), INTENT(OUT) :: r
      INTEGER                          :: i
      CHARACTER(*), INTENT(IN)         :: text
      DO i=1,sz
         CALL R_R(text, r(i))
      ENDDO
   END SUBROUTINE r_r_1

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE error()
      CHARACTER(27), PARAMETER :: mm='*** VISUALISATION ERROR ***'
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



!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE strip(file, u, checktitle, delimiter, separator, dir)
      ! JE 12/99 Strips informative text from data files
      ! Works line by line - removes delimiter and all following text
      ! Writes stripped file to "newfile"
      
      ! Assumed external module dependencies providing global variables:
      ! mess, mess2, mess3, ERROR
      IMPLICIT NONE

      INTEGER, INTENT(IN)                 :: u                 ! unit number for read file
      CHARACTER (*), INTENT(IN)           :: file              ! filename for read
      CHARACTER (*), INTENT(IN)           :: checktitle        ! expected text on first line of read file
      CHARACTER(*), INTENT(IN), OPTIONAL  :: dir
      CHARACTER, INTENT(IN)               :: delimiter         ! marks the begining of text for stripping
      CHARACTER, INTENT(IN)               :: separator(2)      ! break to create new line

      INTEGER :: i, j, k, llen = 500                           ! max allowed length for input lines
      INTEGER :: io = 0, nunit = 100                           ! io error no and unit number for new file
      INTEGER :: ichar, lineno

      CHARACTER                           :: ch
      CHARACTER (LEN(checktitle))         :: dum
      CHARACTER, DIMENSION(:), ALLOCATABLE:: store
      CHARACTER(13), PARAMETER            :: tf = 'temporary.txt'
      CHARACTER(250)                      :: tempfile
      LOGICAL                             :: opened

      !----------------------------------------------------------------------*

      IF (PRESENT(dir)) THEN
         tempfile = TRIM(dir) // '/' // tf
      ELSE
         tempfile = tf
      END IF

      INQUIRE(UNIT=u, OPENED=opened) 
      IF (opened) CLOSE(UNIT=u, STATUS='keep')
      
      ! Open file with IOSTAT instead of ERR jump label
      OPEN(UNIT=u, FILE=file, STATUS='OLD', IOSTAT=io)
      IF (io /= 0) THEN
         mess = ' failed to open ' // TRIM(file)
         CALL ERROR()
         RETURN
      END IF
      
      READ(u, *, IOSTAT=io) dum
      IF (io /= 0 .OR. dum /= checktitle) THEN
         mess = 'wrong key in ' // TRIM(file)
         mess2 = ' Read ' // TRIM(dum) // ' expecting ' // TRIM(checktitle)
         CALL ERROR()
         RETURN
      END IF

      INQUIRE(UNIT=nunit, OPENED=opened) 
      IF (opened) CLOSE(UNIT=nunit, STATUS='keep')
      
      OPEN(UNIT=nunit, FILE=TRIM(tempfile), STATUS='REPLACE')

      io = 0
      lineno = 1
      ALLOCATE (store(llen))
      
      ! Use IOSTAT_END instead of hardcoded -1 for EOF checking
      read_lines: DO WHILE (io /= IOSTAT_END .AND. io /= -1)
         lineno = lineno + 1
         i = 0
         READ(u, '(A1)', IOSTAT=io, ADVANCE='NO') ch
         
         ! Catch EOF or Hard Read Errors immediately
         IF (io == IOSTAT_END .OR. io == -1 .OR. io > 0) EXIT read_lines

         parse_chars: DO WHILE (ch /= delimiter .AND. io == 0)
            i = i + 1
            IF (i > llen) THEN
               mess = 'System message: input data line too long in STRIP'
               CALL ERROR()
               RETURN
            END IF
            
            ichar = IACHAR(ch)
            IF (ichar < 32 .OR. ichar > 126) THEN
               WRITE(mess, '(A,I3)') TRIM(file) // ' contains ASCII character number ', ichar
               WRITE(mess2, '(A,I3,A,I4)') 'At character position ', i, ' in line ', lineno
               IF (ichar == 9) WRITE(mess3, '(A)') 'This is probably a tab character - removed or replace with spaces'
               CALL ERROR()
               RETURN
            END IF
            
            store(i) = ch
            READ(u, '(A1)', IOSTAT=io, ADVANCE='NO') ch
         END DO parse_chars
         
         IF (io == 0) READ(u, '(A1)', IOSTAT=io, ADVANCE='YES')  ! to item up for next input line
         
         j = i
         IF (j > 0) THEN  !.NETT  090805
            DO WHILE (j > 0 .AND. store(j) == ' ')  ! strip off trailing blanks
               j = j - 1
               IF (j == 0) EXIT  !.NETT  090805
            END DO
         END IF
         
         i = 1
         DO WHILE (i < j .AND. store(i) == ' ')  ! strip off leading blanks
            i = i + 1
         END DO

         k = i
         DO WHILE (k <= j)
            DO WHILE (ALL(store(k) /= separator) .AND. k <= j)  ! find line breaks
               k = k + 1
            END DO
            
            ! Replaced FORMAT label 99 with inline standard Fortran format string
            IF (k > i) WRITE(nunit, '(*(A))') store(i:k-1)
            
            k = k + 1
            DO WHILE (k < j .AND. store(k) == ' ')  ! strip off leading blanks
               k = k + 1
            END DO
            i = k
         END DO
         
      END DO read_lines
      
      CLOSE (UNIT=u, STATUS='keep') 
      CLOSE (UNIT=nunit)
      OPEN(UNIT=u, FILE=TRIM(tempfile))
      DEALLOCATE (store)
      
   END SUBROUTINE strip

END MODULE visualisation_read
