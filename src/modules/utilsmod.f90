MODULE utilsmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the utility .F files
   USE SGLOBAL
   USE AL_G, ONLY : NGDBGN, NX, NY, ICMXY, ICMREF
   USE AL_C, ONLY : icmbk
   IMPLICIT NONE

   DOUBLEPRECISION, PARAMETER :: eps=1.0d-15
   CHARACTER(128)             :: msg

   PRIVATE
   PUBLIC :: TRIDAG, DCOPY, HOUR_FROM_DATE, TERPO1, FINPUT, HINPUT, AREADI, AREADR, &
      JEMATMUL_VM, JEMATMUL_MM, INVERTMAT, DATE_FROM_HOUR, RAN2 !OPEN_FILE !GET_START_END_IMPACT
CONTAINS

!SSSSSS SUBROUTINE get_start_end_impact
!SUBROUTINE get_start_end_impact(iu, s, e)
!INTEGER, INTENT(IN)          :: iu
!INTEGER                      :: st(5), et(5)
!DOUBLEPRECISION, INTENT(OUT) :: s, e
!CHARACTER(256)               :: fname, msg
!fname   = TRIM(dirqq)//TRIM(cnam)//'_'//'impact_time_window.txt'
!CALL OPEN_FILE(iu, fname, 'READ', 'FORMATTED')
!READ(iu,*, err=99, end=99) st
!READ(iu,*, err=99, end=99) et
!s = HOUR_FROM_DATE(st(1), st(2), st(3), st(4), st(5))
!e = HOUR_FROM_DATE(et(1), et(2), et(3), et(4), et(5))
!CLOSE(iu)
!RETURN
!99 WRITE(msg, *) 'Error or end of file reading file: '//TRIM(fname)
!CALL ERROR(fffatal, 3009, pppri, 0, 0, msg)
!END SUBROUTINE get_start_end_impact


!SSSSSS  SUBROUTINE open_file
!SUBROUTINE open_file(iu, fn, act, form, access)
!INTEGER, INTENT(IN)      :: iu
!LOGICAL                   :: ex
!CHARACTER(*), INTENT(IN) :: fn, act, form
!CHARACTER(*), INTENT(IN), OPTIONAL :: access
!CHARACTER(28)                      :: ac
!IF(act=='READ') THEN
!    INQUIRE(file=TRIM(fn), EXIST=ex)
!    IF(.NOT.ex) THEN
!        PRINT*, 'Failed to find '// TRIM(fn)
!        RETURN
!    ENDIF
!ENDIF
!IF(PRESENT(access)) THEN
!    ac = access
!ELSE
!    ac = 'SEQUENTIAL'
!ENDIF
!OPEN(iu, FILE=fn, ACTION=act, FORM=form, ACCESS=ac)
!PRINT '(3A)', 'OPENED  '//TRIM(fn)//'  '//TRIM(act)//'  '//TRIM(form)
!END SUBROUTINE OPEN_FILE

!SSSSSS subroutine dcopy (n, dx, incx, dy, incy)
   subroutine dcopy (n, dx, incx, dy, incy)
!     copies vector x to vector y
      INTEGER, INTENT(IN)                        :: n, incx, incy !size and increments
      DOUBLEPRECISION, DIMENSION(*), INTENT(IN)  :: dx
      DOUBLEPRECISION, DIMENSION(*), INTENT(OUT) :: dy
      INTEGER                                    :: i, ix, iy
      IF(n<-0) THEN
         RETURN
      ELSEIF((incx==1).AND.(incy==1)) THEN
         dy(1:n) = dx(1:n)
      ELSE
         ix = 1
         iy = 1
         IF(incx<0) ix=(-n + 1)*incx + 1
         IF(incy<0) iy=(-n + 1)*incy + 1
         DO i = 1, n
            dy(iy) = dx(ix)
            ix     = ix + incx
            iy     = iy + incy
         ENDDO
      ENDIF
   END SUBROUTINE dcopy


!SSSSSS SUBROUTINE FINPUT (IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, &
   SUBROUTINE FINPUT (IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, &
      FNEXT, NINP, ARRAY)
!----------------------------------------------------------------------
!
! GENERAL SUBROUTINE TO READ IN BREAKPOINT TIME-SERIES OF FLUX DATA.
! DATA ARE AVERAGED OVER A SIMULATION TIMESTEP.
!
! PARAMETERS:
!        (INPUT)  IIN     FILE UNIT NUMBER FOR READING DATA
!        (INPUT)  TIH     START TIME OF SIMULATION SINCE REFERENCE DATE
!        (INPUT)  SIMNOW  START TIME OF CURRENT SIMULATION TIMESTEP
!        (INPUT)  SIMSTP  CURRENT SIMULATION TIMESTEP
! (INPUT/OUTPUT)  INLAST  LAST TIME OF READING DATA
! (INPUT/OUTPUT)  INTIME  CURRENT TIME FOR READING DATA
! (INPUT/OUTPUT)  FNEXT   CURRENT VALUE VALID UP TO TIME 'INTIME'
!        (INPUT)  NINP    NUMBER OF DATA ITEMS TO READ
!       (OUTPUT)  ARRAY   ARRAY OF DATA ITEMS
!
!----------------------------------------------------------------------
      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)             :: IIN, NINP
      DOUBLE PRECISION, INTENT(IN)    :: TIH, SIMNOW, SIMSTP
      DOUBLE PRECISION, INTENT(INOUT) :: INLAST, INTIME
      DOUBLE PRECISION, INTENT(INOUT) :: FNEXT(NINP)
      DOUBLE PRECISION, INTENT(OUT)   :: ARRAY(NINP)

      ! Local Variables
      INTEGER                         :: TIME(5), read_stat
      DOUBLE PRECISION                :: SIMEND
!----------------------------------------------------------------------

      SIMEND = SIMNOW + SIMSTP

      ! CHECK IF ANY DATA NEEDS TO BE READ
      IF (INTIME >= SIMEND) THEN
         ! Replaced DO loop with array slicing
         ARRAY(1:NINP) = FNEXT(1:NINP)
         RETURN
      END IF

      ! SAVE CURRENT DATA IN OUTPUT ARRAY
      ! Replaced DO 10 loop with array slicing
      ARRAY(1:NINP) = (INTIME - SIMNOW) * FNEXT(1:NINP)

      ! READ DATA AND ADD INTO TOTALS UNTIL END OF SIMULATION TIMESTEP
      ! Replaced the GOTO 20 loop with a modern DO block
      read_loop: DO

         ! 1. Replaced implied DO loops with slicing and END=9999 with IOSTAT
         READ (IIN, *, IOSTAT=read_stat) TIME(1:5), FNEXT(1:NINP)

         ! FATAL ERROR - END OF FILE REACHED - SET INTIME TO INDICATE ERROR
         IF (read_stat < 0) THEN
            INTIME = marker999
            RETURN
         END IF

         INLAST = INTIME
         INTIME = HOUR_FROM_DATE(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH

         IF (INTIME < SIMEND) THEN
            ! Replaced DO 30 loop with array slicing
            ARRAY(1:NINP) = ARRAY(1:NINP) + ((INTIME - INLAST) * FNEXT(1:NINP))
            ! Naturally cycles to the top of read_loop instead of GOTO 20
         ELSE
            ! Replaced DO 40 loop with array slicing
            ARRAY(1:NINP) = ARRAY(1:NINP) + ((SIMEND - INLAST) * FNEXT(1:NINP))
            EXIT read_loop
         END IF

      END DO read_loop

      ! CALCULATE AVERAGE OVER SIMULATION TIMESTEP
      ! Replaced DO 50 loop with array slicing
      ARRAY(1:NINP) = ARRAY(1:NINP) / SIMSTP

      ! RETURN TO CALLING ROUTINE
      RETURN

   END SUBROUTINE FINPUT
!----------------------------------------------------------------------


   !SSSSSS SUBROUTINE HINPUT
   SUBROUTINE HINPUT (IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, HLAST, HNEXT, NINP, ARRAY)
   !----------------------------------------------------------------------
   !
   ! GENERAL SUBROUTINE TO READ IN BREAKPOINT TIME-SERIES OF HEAD DATA.
   ! HEAD DATA ARE INTERPOLATED ONTO THE MID-POINT OF THE SIMULATION TIMESTEP
   !
   ! PARAMETERS:
   !        (INPUT)  IIN     FILE UNIT NUMBER FOR READING DATA
   !        (INPUT)  TIH     START TIME OF SIMULATION SINCE REFERENCE DATE
   !        (INPUT)  SIMNOW  START TIME OF CURRENT SIMULATION TIMESTEP
   !        (INPUT)  SIMSTP  CURRENT SIMULATION TIMESTEP
   ! (INPUT/OUTPUT)  INLAST  LAST TIME OF READING DATA
   ! (INPUT/OUTPUT)  INTIME  CURRENT TIME FOR READING DATA
   ! (INPUT/OUTPUT)  HLAST   LAST VALUE READ FROM INPUT FILE AT TIME 'INLAST'
   ! (INPUT/OUTPUT)  HNEXT   NEXT VALUE READ FROM INPUT FILE AT TIME 'INTIME'
   !        (INPUT)  NINP    NUMBER OF DATA ITEMS TO READ
   !       (OUTPUT)  ARRAY   ARRAY OF INTERPOLATED DATA ITEMS
   !
   !----------------------------------------------------------------------

      ! Assumed external module dependencies providing global variables:
      ! HOUR_FROM_DATE, marker999
      
      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)             :: IIN, NINP
      DOUBLE PRECISION, INTENT(IN)    :: TIH, SIMNOW, SIMSTP
      DOUBLE PRECISION, INTENT(INOUT) :: INLAST, INTIME
      DOUBLE PRECISION, INTENT(INOUT) :: HLAST (NINP), HNEXT (NINP)
      DOUBLE PRECISION, INTENT(OUT)   :: ARRAY (NINP)

      ! Locals
      INTEGER          :: TIME (5), ios
      DOUBLE PRECISION :: SIMEND, SIMMID

      !----------------------------------------------------------------------

      SIMEND = SIMNOW + SIMSTP
      SIMMID = SIMNOW + 0.5D0 * SIMSTP

      time_loop: DO
         
         ! IF MID-POINT OF TIMESTEP PASSED, INTERPOLATE DATA
         IF (INTIME >= SIMMID .AND. INLAST < SIMMID) THEN
            ! Replaced DO loop 20 with native array slice assignment
            ARRAY(1:NINP) = HLAST(1:NINP) + (HNEXT(1:NINP) - HLAST(1:NINP)) * &
                            ((SIMMID - INLAST) / (INTIME - INLAST))
         END IF
         
         ! READ DATA UNTIL END OF SIMULATION TIMESTEP
         IF (INTIME < SIMEND) THEN
            
            ! Replaced DO loop 30 with native array slice assignment
            HLAST(1:NINP) = HNEXT(1:NINP)
            
            ! Read using IOSTAT to gracefully catch End-of-File
            READ (IIN, *, IOSTAT=ios) TIME(1:5), HNEXT(1:NINP)
            
            IF (ios /= 0) THEN
               ! End of file or read error reached
               INTIME = marker999
               EXIT time_loop
            END IF
            
            INLAST = INTIME
            INTIME = HOUR_FROM_DATE(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH
            
         ELSE
            ! INTIME >= SIMEND, loop termination condition met natively
            EXIT time_loop
         END IF
         
      END DO time_loop

   END SUBROUTINE HINPUT


!FFFFFF DOUBLEPRECISION FUNCTION hour_from_date
   FUNCTION hour_from_date(KYEAR, KMTH, KDAY, KHOUR, KMIN)  RESULT(r)
!----------------------------------------------------------------------*
!  THIS FUNCTION CALCULATES HOURS SINCE 1.JANUARY YEAR 1950 AT 0 HOUR
!  LEAP YEARS ARE TAKEN INTO ACCOUNT
!----------------------------------------------------------------------*
! Version:  SHETRAN/AL/HOUR/4.2
! Modifications:
! RAH  09.12.93  3.4.1  Remove IMPLICIT INTEGER*2 (I-N).
! RAH  980611  4.2 !Replace 60. with 6D1 to eliminate rounding error.
!                   Explicit typing.
!----------------------------------------------------------------------*
! Entry requirements:
!  KYEAR.ge.1949    KMTH.ge.1    KMTH.le.12
      INTEGER         :: kyear, kmth, kday, khour, kmin
      INTEGER         :: d, check(6)
      DOUBLEPRECISION :: r
      d = DAYS_IN_YEARS_SINCE_1950(kyear)+ DAYS_TO_START_MONTH(kmth, kyear) + kday
      r = DBLE(d*24 + khour) + DBLE(kmin) / 6d1
      r= r+ 0.0000028  !add 1/100 of a second to sort out round error with mins
      check = DATE_FROM_HOUR(r)
      IF(check(1)/=kyear .OR.check(2)/=kmth .OR. check(3)/=kday .or. check(4)/=khour .or. check(5)/=kmin) THEN
         write (*,'(A)') ' There is a problem with a date that has been entered'
         write (*,'(A,5(1x,I0))') 'The Year, month,day,hour,minute values entered are: ', kyear, kmth, kday, khour, kmin
         write(*,'(''paused, type [enter] to continue'')')
         read (*,*)
         stop
      ENDIF
      !     * days arising from entire years (asasuming KYEAR.ge.1949) ...
      !mmday = (kyear - 1950) * 365 + (kyear - 1949) / 4
      !     * ... plus entire days this year (not including today) ...
      !mmday = mmday + mdays(kmth) + kday - 1
      !     * ... not forgetting that MDAYS is defined for non-leap years
      !IF (MOD (KYEAR,4) .EQ.0.AND.KMTH.GT.2) mmday = mmday + 1
   END FUNCTION hour_from_date

!FFFFFF FUNCTION days_in_years_since_1950
   FUNCTION days_in_years_since_1950(y) RESULT(r)
      INTEGER, INTENT(IN) :: y
      INTEGER             :: i, r
      r = (y - 1950) * 365
      DO i=1952, y-1, 4
         IF(IS_LEAP(i)) r=r+1
      ENDDO
   END FUNCTION days_in_years_since_1950


!FFFFFF FUNCTION is_leap
   FUNCTION is_leap(y) RESULT(r)
!A year will be a leap year if it is divisible by 4 but not by 100.
!If a year is divisible by 4 and by 100, it is not a leap year unless it is also divisible by 400.
      INTEGER, INTENT(IN) :: y
      LOGICAL  :: r
      IF(MOD(y,4)==0) THEN
         IF(MOD(y,100)==0) THEN
            r = MOD(y,400)==0
         ELSE
            r = .TRUE.
         ENDIF
      ELSE
         r = .FALSE.
      ENDIF
   END FUNCTION is_leap


!FFFFFF FUNCTION days_to_start_month
   FUNCTION days_to_start_month(m, y) RESULT(r)
      INTEGER, INTENT(IN) :: m, y
      INTEGER, PARAMETER  ::  sd(12)=[0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
      INTEGER             :: r
      IF(M<1) THEN
         WRITE(MSG,*) 'Date problem, probably with rainfall or evaporation - are their start dates specified correctly in their files?'
         CALL ERROR (FFFATAL, 4820, pppri, 0, 0, msg)
      ENDIF
      r = sd(m)
      IF(IS_LEAP(y).AND. m>2) r = r + 1
   END FUNCTION days_to_start_month

!FFFFFF FUNCTION date_from_hour
   FUNCTION date_from_hour(h) RESULT(r)
      INTEGER                     :: r(6) !year, month, day, hour, min, sec
      INTEGER                     :: hours, days, year, month, mthdays, mins, sec
      DOUBLEPRECISION, INTENT(IN) :: h
      DOUBLEPRECISION             :: rmins

      hours = INT(h)
      rmins = 60*(h-hours)
      mins  = INT(rmins)
      sec   = INT(60*(rmins-mins))
      days  = hours/24
      year  = 1950 + days/366  !note, 366 is correct (to underpredict)
      DO WHILE(days>DAYS_IN_YEARS_SINCE_1950(year+1))
         year = year + 1
      ENDDO

      mthdays = days - DAYS_IN_YEARS_SINCE_1950(year)
      month   = 1 + mthdays/32             !note, 32 is correct (to underpredict)
      IF(month<12) THEN                    !avoid month+1=13 in test (dont combine tests)
         IF(mthdays>DAYS_TO_START_MONTH(month+1, year)) month = month + 1
      ENDIF

      r(1) = year
      r(2) = month
      r(3) = mthdays - DAYS_TO_START_MONTH(month, year) !days
      r(4) = hours - 24*days                            !hours
      r(5) = mins                                       !minutes
      r(6) = sec
      IF(r(3)==0) THEN
         print*,' date trap -DAY'
         stop
      ENDIF
   END FUNCTION date_from_hour

!FFFFFF FUNCTION jematmul_mm
   FUNCTION jematmul_mm(b, c, n1, n2, n3) RESULT(a)
! A = B * C
      INTEGER, INTENT(IN)          :: n1, n2, n3
      DOUBLEPRECISION, INTENT(IN)  :: b(n2,n1), c(n3,n2)
      DOUBLEPRECISION              :: a(n3,n1)
      INTEGER                      :: i, j, k
      DO i=1,n3
         DO j=1,n1
            a(i,j) = zero
            DO k=1,n2
               a(i,j) = a(i,j) + b(k,j)*c(i,k)
            ENDDO
         ENDDO
      ENDDO
   END FUNCTION jematmul_mm


!FFFFFF FUNCTION jematmul_vm
   FUNCTION jematmul_vm(b, c, n1, n2)  RESULT(a)
! A = B * C
      INTEGER, INTENT(IN)          :: n1, n2
      DOUBLEPRECISION, INTENT(IN)  :: b(n2,n1),c(n2)
      DOUBLEPRECISION              :: a(n1)
      INTEGER                      :: i, k
      DO i=1,n1
         a(i) = zero
         DO k=1,n2
            a(i) = a(i) + b(k,i) * c(k)
         ENDDO
      ENDDO
   END FUNCTION jematmul_vm




!SSSSSS SUBROUTINE TERPO1 (YCURR, TCURR, YTAB, TTAB, NCT, YINIT, NPAR, I)
   SUBROUTINE TERPO1 (YCURR, TCURR, YTAB, TTAB, NCT, YINIT, NPAR, I)
!----------------------------------------------------------------------*
!
!     SERVICE SUBROUTINE TO INTERPOLATE VALUES FOR ONE-DIMENSIONAL
!                     TIME-VARYING PARAMETERS
!        VARIABLE LISTING:
!        YCURR = CURRENT VALUE OF PARAMETER
!        YTAB  = TABULATED RELATIVE VALUES OF PARAMETER
!        YINIT = INITIAL OR REFERENCE VALUE OF PARAMETER
!        TCURR = CURRENT TIME (HOURS)
!        TTAB  = TABULATED VALUES OF TIME (DAYS)
!        NCT   = COUNTER FOR POSITION IN TABULATED ARRAYS
!        NPAR  = SIZE OF PARAMETER ARRAY
!        I     = POSITION IN PARAMETER ARRAY
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/ET/TERPO1/4.1
! Modifications:
! RAH  941005 3.4.1 Remove IMPLICIT INTEGER*2.
! RAH  970516  4.1  Explicit typing.  *TAB asasumed size (were 20).
!                   Scrap redundant arg ITAB "SIZE OF TABULATED ARRAYS".
!----------------------------------------------------------------------*
! Input arguments
      INTEGER :: NPAR, I
      DOUBLEPRECISION TCURR

      DOUBLEPRECISION YTAB (NPAR, * ), TTAB (NPAR, * ), YINIT (NPAR)
! In+out arguments

      INTEGER :: NCT (NPAR)
! Output arguments

      DOUBLEPRECISION YCURR (NPAR)
! Locals, etc
      INTEGER :: ITERP, NCTERP


      DOUBLEPRECISION DIFFA, DIFFB, DIFFC, YREL
!----------------------------------------------------------------------*
      NCTERP = NCT (I)
      ITERP = INT((TCURR / 24.0 - TTAB (I, NCTERP) ) / (TTAB (I, NCTERP + 1) &
         - TTAB (I, NCTERP) ))
      NCTERP = NCTERP + ITERP
      DIFFA = YTAB (I, NCTERP + 1) - YTAB (I, NCTERP)
      DIFFB = (TTAB (I, NCTERP + 1) - TTAB (I, NCTERP) ) * 24.0
      DIFFC = TCURR - TTAB (I, NCTERP) * 24.0
      YREL = YTAB (I, NCTERP) + DIFFC * DIFFA / DIFFB
      YCURR (I) = YREL * YINIT (I)

      NCT (I) = NCTERP

   END SUBROUTINE TERPO1
! 18/8/94


   !SSSSSS SUBROUTINE TRIDAG (A, B, C, R, U, N)
   PURE SUBROUTINE TRIDAG (A, B, C, R, U, N)
   !----------------------------------------------------------------------*
   !                            SOLVES FOR VECTOR U OF LENGTH N
   !                            THE TRIDIAGONAL SET A,B,C WHERE
   !                            R IS THE R.H.S.
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN)             :: N
      ! Explicit-shape arrays guarantee zero copy-in/copy-out overhead
      DOUBLE PRECISION, INTENT(IN)    :: A(N), B(N), C(N), R(N)
      DOUBLE PRECISION, INTENT(INOUT) :: U(N)

      ! Locals
      INTEGER :: J
      DOUBLE PRECISION :: GAM(N), BET, OOBET

      BET = B(1)
      OOBET = 1.0d0 / BET
      U(1) = OOBET * R(1)

      forward_sweep: DO J = 2, N
         GAM(J) = OOBET * C(J-1)
         BET    = B(J) - A(J) * GAM(J)
         OOBET  = 1.0d0 / BET
         U(J)   = OOBET * (R(J) - A(J) * U(J-1))
      END DO forward_sweep

      backward_sweep: DO J = N - 1, 1, -1
         U(J) = U(J) - GAM(J+1) * U(J+1)
      END DO backward_sweep

   END SUBROUTINE TRIDAG



!SSSSSS SUBROUTINE  invertmat
   SUBROUTINE invertmat(a,n,icod)
      INTEGER, INTENT(IN)                            :: n
      INTEGER, INTENT(OUT)                           :: icod
      INTEGER                                        :: i, j, indx(n)
      DOUBLEPRECISION, DIMENSION(n,n), INTENT(INOUT) :: a
      DOUBLEPRECISION, DIMENSION(n,n)                :: y
      DOUBLEPRECISION                                :: d
      LOGICAL                                        :: issing, ret

      ret=.FALSE.
      icod = 0
      IF(n<1) THEN
         icod = 1
      ELSEIF(n==1) THEN
         ret=.TRUE.
         IF (ABS(A(1,1))<=eps) THEN
            icod = 1
         ELSE
            A(1,1) = one / A(1,1)
         ENDIF
      ELSE
         y = zero
         DO i=1,n
            y(i,i) = one
         ENDDO
         CALL LUDCMP(a, n, indx, d, issing)
         IF(issing) THEN
            icod=1
         ELSE
            DO j=1,n
               CALL LUBKSB(a, n, indx, y(:,j))
            ENDDO
            a = y
         ENDIF
      ENDIF
   END SUBROUTINE invertmat


!SSSSSS SUBROUTINE lubksb(a, n, indx, b)
   SUBROUTINE lubksb(a, n, indx, b)
!----------------------------------------------------------------------*
! Solves the linear system A*x = b using LU Decomposition.
! 'a' is the LU-decomposed matrix output from 'ludcmp'.
! 'indx' is the row permutation vector output from 'ludcmp'.
! 'b' is the right-hand side vector on input, and contains the
!     solution vector 'x' on output.
!----------------------------------------------------------------------*
      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)             :: n
      INTEGER, INTENT(IN)             :: indx(n)
      DOUBLE PRECISION, INTENT(IN)    :: a(n,n)
      DOUBLE PRECISION, INTENT(INOUT) :: b(n)

      ! Local Variables
      INTEGER                         :: i, ii, ll
      DOUBLE PRECISION                :: asum
!----------------------------------------------------------------------*

      ii = 0

      ! 1. Forward Substitution (Solving L*y = b)
      forward_sub: DO i = 1, n
         ll = indx(i)
         asum = b(ll)
         b(ll) = b(i)

         IF (ii /= 0) THEN
            ! Replaced inner j loop with DOT_PRODUCT
            asum = asum - DOT_PRODUCT(a(i, ii:i-1), b(ii:i-1))
         ELSEIF (NOTZERO(asum)) THEN
            ! Optimization: Record the first non-zero element to
            ! avoid doing math on a bunch of leading zeros.
            ii = i
         END IF

         b(i) = asum
      END DO forward_sub

      ! 2. Backward Substitution (Solving U*x = y)
      backward_sub: DO i = n, 1, -1
         ! Replaced inner j loop with DOT_PRODUCT
         ! Note: when i=n, the slice i+1:n is empty, so DOT_PRODUCT safely returns 0.0
         asum = b(i) - DOT_PRODUCT(a(i, i+1:n), b(i+1:n))

         b(i) = asum / a(i, i)
      END DO backward_sub

   END SUBROUTINE lubksb



!SSSSSS SUBROUTINE ludcmp(a, n, indx, d, issing)
   SUBROUTINE ludcmp(a, n, indx, d, issing)
!----------------------------------------------------------------------*
! Performs LU Decomposition on matrix 'a' using partial pivoting.
! 'a' is replaced by its LU decomposition.
! 'indx' records the row permutations.
! 'd' outputs +1 or -1 depending on whether row swaps were even or odd.
! 'issing' is flagged .TRUE. if the matrix is singular.
!----------------------------------------------------------------------*
      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)             :: n
      DOUBLE PRECISION, INTENT(INOUT) :: a(n,n)
      INTEGER, INTENT(OUT)            :: indx(n)
      DOUBLE PRECISION, INTENT(OUT)   :: d
      LOGICAL, INTENT(OUT)            :: issing

      ! Local Variables
      INTEGER                         :: i, imax, j
      DOUBLE PRECISION                :: aamax, dum, vv(n), dum_row(n)
      DOUBLE PRECISION, PARAMETER     :: TINY = 1.0d-20
!----------------------------------------------------------------------*

      issing = .FALSE.
      d = 1.0d0

      ! 1. Replaced inner search loop with MAXVAL and array slicing
      ! Calculate implicit scaling information for each row
      DO i = 1, n
         aamax = MAXVAL(ABS(a(i, 1:n)))

         IF (ISZERO(aamax)) THEN
            issing = .TRUE.
            RETURN ! Singular matrix, exit immediately
         END IF

         vv(i) = 1.0d0 / aamax
      END DO

      ! Crout's Algorithm
      outer_col_loop: DO j = 1, n

         ! 2. Replaced nested loop 13 with modern DOT_PRODUCT
         ! Upper triangular part
         upper_loop: DO i = 1, j - 1
            a(i, j) = a(i, j) - DOT_PRODUCT(a(i, 1:i-1), a(1:i-1, j))
         END DO upper_loop

         aamax = 0.0d0
         imax = j

         ! 3. Replaced nested loop 15 with DOT_PRODUCT
         ! Lower triangular part and pivot search
         lower_loop: DO i = j, n
            a(i, j) = a(i, j) - DOT_PRODUCT(a(i, 1:j-1), a(1:j-1, j))

            dum = vv(i) * ABS(a(i, j))
            IF (dum >= aamax) THEN
               imax = i
               aamax = dum
            END IF
         END DO lower_loop

         ! Row swapping (Pivoting)
         IF (j /= imax) THEN
            ! 4. Replaced the element-by-element loop 17 with whole-array row slices
            dum_row(1:n) = a(imax, 1:n)
            a(imax, 1:n) = a(j, 1:n)
            a(j, 1:n)    = dum_row(1:n)

            d = -d
            vv(imax) = vv(j)
         END IF

         indx(j) = imax

         IF (ISZERO(a(j, j))) a(j, j) = TINY

         ! 5. Replaced loop 18 with direct column scaling
         IF (j /= n) THEN
            dum = 1.0d0 / a(j, j)
            a(j+1:n, j) = a(j+1:n, j) * dum
         END IF

      END DO outer_col_loop

   END SUBROUTINE ludcmp


!SSSSSS SUBROUTINE AREADI (IAOUT, KON, INF, IOF, INUM)
   SUBROUTINE AREADI (IAOUT, KON, INF, IOF, INUM)
!----------------------------------------------------------------------*
!
!      SERVICE SUBROUTINE TO READ AND PRINT AN INTEGER ARRAY
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/AL/AREADI/4.1
! Modifications:
! RAH  940928 3.4.1 Add IMPLICIT (was in AL.P).
! GP  24/7/95  4.0  Initialize IAOUT (if KON = 0 or 1).
! RAH  970804  4.1  Explicit typing (note TITLE was implicit double).
!----------------------------------------------------------------------*
!
!    PARAMETER LIST :
!    IA   : ARRAY TO BE READ
!    KON  : CONTROL PARAMETER : KON=0 : READ GRID ARRAY (IA)
!                                       CONVERT TO ELEMENT ARRAY (IAOUT)
!                                       NO PRINT
!                               K0N=1 : READ GRID ARRAY (IA)
!                                       CONVERT TO ELEMENT ARRAY (IAOUT)
!                                       PRINT GRID ARRAY (IA)
!                               KON=2 : NO READ;   CONVERT
!                                       INPUT ARRAY (IAOUT) TO GRID (IA)
!                                       PRINT GRID ARRAY (IA)
!
!    INF  : FILE UNIT NUMBER FOR READING FROM
!    IOF  : FILE UNIT NUMBER FOR PRINTING TO
!
!    INUM : RANGE OF NUMBERS TO BE READ IN (EG. NO. OF MET. STATIONS)
!           NB. IF SET TO ZERO, OLD FORMAT (20I4) WILL BE USED.
!
!    ALSO INCLUDES THE POSSIBILITY OF FILLING AN INTEGER ARRAY
!    WITH DEFAULT VALUES IN WHICH CASE SHOULD HAVE
!                               KON=3
!                               INF=REQUIRED DEFAULT VALUE
!
!----------------------------------------------------------------------*
      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: KON, INF, IOF, INUM
      INTEGER, INTENT(OUT) :: IAOUT(:)
      INTEGER              :: I, I1, I2, IEL, J, K, L, LAL, LL1, NNX, NXX
      INTEGER              :: IA(NXEE, NYEE)
      CHARACTER(4)         :: TITLE(20)
!----------------------------------------------------------------------*

!^^^^^^FILL IN SECTION
!
      IF (KON == 3) THEN
         ! Replaced DO loop with array slicing
         IAOUT(NGDBGN : total_no_elements) = INF
         RETURN
      END IF

!^^^^^^READ SECTION
!
! CHECK I/O FORMATS OK FOR PRINTING ARRAY (LIMIT CURRENTLY SET TO 200)
!
      IF ((INUM > 0 .AND. INUM < 10) .AND. NX > 500) THEN
         WRITE (IOF, "(' ', 'NX greater than 500. Change I/O formats in AREADI', /, 'Program aborted.')")
         STOP
      END IF

      IF (KON == 0 .OR. KON == 1) THEN
         READ (INF, '(20A4)') TITLE

         y_read_loop: DO I1 = 1, NY
            K = NY + 1 - I1
            IF (INUM > 0 .AND. INUM < 10) THEN
               ! Replaced implied DO loop with array slicing
               READ (INF, '(I7, 1X, 500I1)') I2, IA(1:NX, K)
               IF (I2 /= K) THEN
                  WRITE (IOF, "(/,/,2X, 'ERROR IN DATA ', 20A4, /,/,2X, 'IN THE VICINITY OF LINE K=', I5)") TITLE, I2
                  STOP
               END IF
            ELSE
               READ (INF, '(I7)') I2
               IF (I2 /= K) THEN
                  WRITE (IOF, "(/,/,2X, 'ERROR IN DATA ', 20A4, /,/,2X, 'IN THE VICINITY OF LINE K=', I5)") TITLE, I2
                  STOP
               END IF
               ! Note: Used list-directed read (*) as per your original commented-out line 30
               READ (INF, *) IA(1:NX, K)
            END IF
         END DO y_read_loop

!^^^^^^CONVERT GRID ARRAY TO ELEMENT ARRAY ...
!
         ! Replaced DO loop with array slicing
         IAOUT(1:total_no_elements) = 0

         grid_to_elem_x: DO I = 1, NX
            grid_to_elem_y: DO J = 1, NY
               IEL = ICMXY(I, J)
               IF (IEL /= 0) IAOUT(IEL) = IA(I, J)
            END DO grid_to_elem_y
         END DO grid_to_elem_x

!^^^^^^ ... OR CONVERT ELEMENT ARRAY TO GRID ARRAY
!
      ELSE
         ! Replaced nested DO 66 loops with modern array zeroing
         IA(1:NX, 1:NY) = 0

         elem_to_grid_loop: DO IEL = NGDBGN, total_no_elements
            IF (ICMREF(IEL, 1) == 0) THEN
               I = ICMREF(IEL, 2)
               J = ICMREF(IEL, 3)
               IA(I, J) = IAOUT(IEL)
            END IF
         END DO elem_to_grid_loop
      END IF

!^^^^^^PRINT SECTION
!
      IF (KON == 0) RETURN

      IF (KON == 1) WRITE (IOF, "(/, 20A4)") TITLE

! CHECK FOR ALL ZEROES
!
      IF (I_ISZERO_A2(IA(1:NX, 1:NY))) THEN
         WRITE (IOF, "(' ALL VALUES ZERO', /, ' ===============', /)")
         RETURN
      END IF

      NNX = (NX - 1) / 10 + 1

      IF (INUM > 0 .AND. INUM < 10) THEN
         print_compact_loop: DO I1 = 1, NY
            K = NY + 1 - I1
            WRITE (IOF, "(' ', 'K=', I4, 1X, 500I1)") K, IA(1:NX, K)
         END DO print_compact_loop
      ELSE
         print_blocks_loop: DO L = 1, NNX
            LAL = L * 10
            LL1 = LAL - 9
            ! Replaced MIN0 with modern generic MIN
            NXX = MIN(NX, LAL)

            WRITE (IOF, "('0', 9X, 10('J=',I3,6X), /)") (I, I = LL1, LAL)

            print_rows_loop: DO I1 = 1, NY
               K = NY + 1 - I1
               WRITE (IOF, "(' ', 'K=', I4, 2X, 10(I6,5X))") K, IA(LL1:NXX, K)
            END DO print_rows_loop
         END DO print_blocks_loop
      END IF

      WRITE (IOF, "(/,/,2X, 80('*'), /,/)")

   END SUBROUTINE AREADI


!SSSSSS SUBROUTINE AREADR (AOUT, KON, INF, IOF)
   SUBROUTINE AREADR (AOUT, KON, INF, IOF)
!----------------------------------------------------------------------*
!
!      SERVICE SUBROUTINE TO READ AND PRINT A DOUBLEPRECISION,TWO-DIMENSIONAL ARRAY
!      (IN DOUBLEPRECISION)
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/AL/AREADR/4.1
! Modifications:
! RAH  940928 3.4.1 Add IMPLICIT (was in AL.P).
! RAH  970804  4.1  Explicit typing (note TITLE was implicit double).
!----------------------------------------------------------------------*
!
!     PARAMETER LIST :
!     A    : ARRAY TO BE READ
!     KON  : CONTROL PARAMETER : KON=0 : READ GRID ARRAY (A)
!                                        CONVERT TO ELEMENT ARRAY (AOUT)
!                                        NO PRINT
!                                K0N=1 : READ GRID ARRAY (A)
!                                        CONVERT TO ELEMENT ARRAY (AOUT)
!                                        PRINT GRID ARRAY (A)
!                                KON=2 : NO READ;   CONVERT
!                                        INPUT ARRAY (AOUT) TO GRID (A)
!                                        PRINT GRID ARRAY (A)
!
!     INF  : FILE UNIT NUMBER FOR READING FROM
!     IOF  : FILE UNIT NUMBER FOR PRINTING TO
!
!----------------------------------------------------------------------*
! Commons and constants
      IMPLICIT NONE

! Input arguments
      INTEGER :: KON, INF, IOF

! In|out arguments
      DOUBLE PRECISION :: AOUT(NELEE)

! Locals, etc
      INTEGER :: I, J, K, L, I1, I2, IEL, IEL1, IEL2, LAL, LL1, NNX, NXX
      DOUBLE PRECISION :: B1, B2, A(NXEE, NYEE)
      CHARACTER(LEN=4) :: TITLE(20)
!----------------------------------------------------------------------*

!^^^^^^READ SECTION
!
      IF (KON == 0 .OR. KON == 1) THEN
         READ (INF, '(20A4)') TITLE

         y_read_loop: DO I1 = 1, NY
            READ (INF, '(I7)') I2
            K = NY + 1 - I1

            IF (I2 /= K) THEN
               WRITE (IOF, "(/,/,2X, 'ERROR IN DATA ', 20A4, /,/,2X, 'IN THE VICINITY OF LINE K=', I5)") TITLE, I2
               STOP
            END IF

            ! 1. Replaced implied DO loop with array slicing
            READ (INF, '(10G7.0)') A(1:NX, K)
         END DO y_read_loop

!^^^^^^CONVERT GRID ARRAY TO ELEMENT ARRAY
!
         grid_to_elem_x: DO I = 1, NX
            grid_to_elem_y: DO J = 1, NY
               IEL = ICMXY(I, J)
               IF (IEL /= 0) AOUT(IEL) = A(I, J)
            END DO grid_to_elem_y
         END DO grid_to_elem_x

!^^^^^^CONVERT ELEMENT ARRAY TO GRID ARRAY
!
      ELSE
         ! 2. Replaced the nested DO 66 loops with modern array zeroing
         A(1:NX, 1:NY) = zero

         elem_to_grid_loop: DO IEL = NGDBGN, total_no_elements
            IF (ICMREF(IEL, 1) == 0) THEN
               I = ICMREF(IEL, 2)
               J = ICMREF(IEL, 3)
               A(I, J) = AOUT(IEL)
            END IF
         END DO elem_to_grid_loop
      END IF

!^^^^^^PRINT SECTION
!
      IF (KON == 0) RETURN

      IF (KON == 1) WRITE (IOF, "(/, 20A4)") TITLE

! CHECK FOR ALL ZEROES
!
      IF (ISZERO_A(AOUT(1:total_no_elements))) THEN
         WRITE(IOF, "(' ALL VALUES ZERO', /, ' ===============', /)")
         RETURN
      END IF

! PRINT ARRAY
!
      NNX = (NX - 1) / 10 + 1

      print_blocks_loop: DO L = 1, NNX
         LAL = L * 10
         LL1 = LAL - 9
         ! 3. Replaced MIN0 with modern generic MIN
         NXX = MIN(NX, LAL)

         WRITE (IOF, "('0', 9X, 10('J=',I3,6X), /)") (I, I = LL1, LAL)

         print_rows_loop: DO I1 = 1, NY
            K = NY + 1 - I1
            ! Replaced implied DO loop with array slicing
            WRITE (IOF, "(' ', 'K=', I4, 2X, 10G11.4)") K, A(LL1:NXX, K)
         END DO print_rows_loop
      END DO print_blocks_loop

      WRITE (IOF, "(/, 10X, 'LINK ', 6X, 'BANK1 ', 5X, 'BANK2 ', /)")

      link_print_loop: DO I = 1, total_no_links
         B1 = zero
         B2 = zero
         IEL1 = ICMBK(I, 1)
         IEL2 = ICMBK(I, 2)

         IF (IEL1 > 0) B1 = AOUT(IEL1)
         IF (IEL2 > 0) B2 = AOUT(IEL2)

         WRITE (IOF, "(1X, 'L= ', I4, 2X, 3G11.4)") I, AOUT(I), B1, B2
      END DO link_print_loop

      WRITE (IOF, "(/,/,2X, 120('*'), /,/)")

   END SUBROUTINE AREADR


! 12/8/94
!FFFFFF FUNCTION ran2
   FUNCTION ran2(idum)
      INTEGER, PARAMETER     :: IM1=2147483563,IM2=2147483399,IMM1=IM1-1, &
         IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791, &
         NTAB=32,NDIV=1+IMM1/NTAB
      INTEGER, INTENT(INOUT) :: idum
      INTEGER                :: j, k
      INTEGER, SAVE          :: IDUM2=123456789, iy=0, iv(NTAB)=0
      REAL                   :: ran2
      REAL, PARAMETER        :: EPS=1.2e-7, RNMX=1.-EPS, AM=1./IM1
      IF(idum.le.0) THEN
         idum  = MAX(-idum,1)
         idum2 = idum
         DO j=NTAB+8,1,-1
            k    = idum/IQ1
            idum = IA1*(idum-k*IQ1)-k*IR1
            IF(idum.lt.0) idum=idum+IM1
            IF(j.le.NTAB) iv(j)=idum
         ENDDO
         iy = iv(1)
      ENDIF
      k = idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      IF(idum.lt.0) idum=idum+IM1
      k     = idum2/IQ2
      idum2 = IA2*(idum2-k*IQ2)-k*IR2
      IF(idum2.lt.0) idum2=idum2+IM2
      j     = 1+iy/NDIV
      iy    = iv(j)-idum2
      iv(j) = idum
      if(iy.lt.1)iy=iy+IMM1
      ran2 = MIN(AM*iy,RNMX)
   END FUNCTION ran2
END MODULE utilsmod


!!SSSSSS SUBROUTINE ADDMM (A, B, C, NL, NC, NASIZE)
!SUBROUTINE ADDMM (A, B, C, NL, NC, NASIZE)
!!=======================================================================
!!
!!       UTILITAIRE - ADDITION DE MATRICES   A = B + C
!!                    A,B,C SONT DES MATRICES (NL,NC)
!!-----------------------------------------------------------------------
!!
!!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!!      IMPLICIT INTEGER (I-N)
!INTEGER, INTENT(IN) :: nl, nc, nasize
!INTEGER :: I, j
!DOUBLEPRECISION, INTENT(IN)  :: B(NASIZE,NASIZE), C(NASIZE,NASIZE)
!DOUBLEPRECISION, INTENT(OUT) :: A(NASIZE,NASIZE)
!!
!DO 10, J = 1, NL
!   DO 11, I = 1, NC
!      A (I, J) = B (I, J) + C (I, J)
!   11    END DO
!   10 END DO
!!
!END subroutine ADDMM
!! 12/8/94
!!
!!-----------------------------------------------------------------------
!
!
!
!!SSSSSS SUBROUTINE ADDVV (A, B, C, N)
!SUBROUTINE ADDVV (A, B, C, N)
!!=======================================================================
!!
!!       UTILITAIRE - ADDITION VECTORIELLE   A = B + C
!!                    A,B,C SONT DES VECTEURS (N)
!!
!!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!!      IMPLICIT INTEGER (I-N)
!INTEGER, INTENT(IN) :: n
!INTEGER :: I
!DOUBLEPRECISION, INTENT(IN)  :: B(N), C(N)
!DOUBLEPRECISION, INTENT(OUT) :: A(N)
!!
!DO 10, I = 1, N
!   A (I) = B (I) + C (I)
!   10 END DO
!!
!END SUBROUTINE ADDVV
!
!!SSSSSS SUBROUTINE CHSGN (A, NL, NC, NASIZE)
!SUBROUTINE CHSGN (A, NL, NC, NASIZE)
!!=======================================================================
!!
!!       UTILITAIRE - CHAGEMENT DE SIGNE D'UNE MATRICE  A = -A
!!                    A EST UNE MATRICE (NL,NC)
!!-----------------------------------------------------------------------
!!
!!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!!      IMPLICIT INTEGER (I-N)
!INTEGER, INTENT(IN) :: nl, nc, nasize
!INTEGER :: i, j
!DOUBLEPRECISION :: A(NASIZE, NASIZE)
!!
!DO 10, J = 1, NL
!   DO 11, I = 1, NC
!      A (I, J) = - A (I, J)
!   11    END DO
!   10 END DO
!!
!RETURN
!END SUBROUTINE CHSGN
!
!
!!SSSSSS SUBROUTINE DIFVV (A, B, C, N, NASIZE)
!SUBROUTINE DIFVV (A, B, C, N, NASIZE)
!!=======================================================================
!!
!!       UTILITAIRE - SOUSTRACTION VECTORIELLE   A = B - C
!!                    A,B,C SONT DES VECTEURS (N)
!!-----------------------------------------------------------------------
!!
!!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!!      IMPLICIT INTEGER (I-N)
!INTEGER, INTENT(IN) :: n, nasize
!INTEGER :: i
!DOUBLEPRECISION :: A(NASIZE), B(NASIZE), C(NASIZE)
!!
!DO 10, I = 1, N
!   A (I) = B (I) - C (I)
!   10 END DO
!!
!END subroutine DIFVV
!!
!!----------------------------------------------------------------------
!
!!SSSSSS SUBROUTINE PMINVM
!SUBROUTINE PMINVM(A, N, ICOD)
!!=======================================================================
!!
!!       UTILITAIRE - INVERSION MATRICIELLE   A = INVERSE DE A
!!
!!             A EST UNE MATRICE (N,N)
!!        (R)  ICOD=0  POUR UNE INVERSION CORRECTE
!!             ICOD=1  POUR UNE MATRICE SINGULIERE
!!             TR(N)   = TABLE DE TRAVAIL REELLE
!!             LC(N,2) = TABLE DE TRAVAIL ENTIERE
!!-----------------------------------------------------------------------
!!
!!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!!      IMPLICIT INTEGER (I-N)
!!JE JAN 2009 loop restructure for AD
!INTEGER, INTENT(IN)            :: n !, nasize
!INTEGER, INTENT(OUT)           :: icod
!INTEGER                        :: i, j, k, km1, ipiv, jpiv
!!INTEGER, INTENT(OUT)           :: LC(NASIZE, 2)
!!DOUBLEPRECISION, INTENT(OUT)   :: TR(NASIZE)
!INTEGER                        :: LC(N, 2)
!DOUBLEPRECISION, INTENT(INOUT) :: A(N, N)
!DOUBLEPRECISION                 :: TR(N)
!DOUBLEPRECISION                :: pivot, pivinv, aijpiv
!LOGICAL                        :: DIADOM, cycle15, cycle20, cycle21, ret
!!cc       character*70 ooo
!!
!ret=.FALSE.
!ICOD = 0
!IF (N.LE.0) THEN
!    ICOD = 1
!    ret=.TRUE.
!ELSEIF (N.EQ.1) THEN
!    ret=.TRUE.
!    IF (ABS (A (1, 1) ) .LE.EPS) THEN
!        ICOD = 1
!    ELSE
!        A (1, 1) = one / A (1, 1)
!    ENDIF
!ENDIF
!IF(ret) RETURN
!!
!! CHECK IF MATRIX IS DIAGONALLY DOMINANT
!!
!!cc      dimin = 1.0e10
!!cc      omax = 0.0
!
!!        ooo = ''
!!        do 4 j=1,n
!!        if (i.eq.j .and. dabs(a(i,j)).lt.dabs(dimin))
!!     -    dimin = dabs(a(i,j))
!!        if (i.ne.j .and. dabs(a(i,j)).gt.dabs(omax))
!!     -    omax = dabs(a(i,j))
!!         if (a(i,j).ne.0.0) then
!!            ooo(j:j) = 'X'
!!          else
!!            ooo(j:j) = '.'
!!          endif
!! 4      continue
!!        write(*,*) ooo
!diadom = .TRUE.
!DO I = 1, N
!    IF(.NOT.diadom) CYCLE
!    DO J = 1, N
!        IF(.NOT.diadom) CYCLE
!        IF (ABS (A (I, I) ) .LT.ABS (A (I, J) ) ) diadom = .FALSE.
!        IF (ABS (A (I, I) ) .LT.ABS (A (J, I) ) ) diadom = .FALSE.
!    ENDDO
!ENDDO
!
!out10 : DO K = 1, N
!    IF(icod==1) CYCLE out10
!    !.... RECHERCHE DU PIVOT MAXIMUM (IPIV,JPIV)
!    !     --------------------------------------
!    !
!    KM1 = K - 1
!    PIVOT = ZERO
!    !
!    ! CHECK ONLY DIAGONAL ELEMENTS IF DIAGONALLY DOMINANT
!    !
!    IF (DIADOM) THEN
!        out15 : DO IPIV = 1, N
!            cycle15=.FALSE.
!            IF (KM1.GT.0) THEN
!                out17 : DO I = 1, KM1
!                    IF(cycle15) CYCLE out17
!                    IF (IPIV.EQ.LC (I, 1) ) cycle15=.TRUE. !GOTO 15
!                ENDDO out17
!            ENDIF
!            IF(cycle15) CYCLE out15
!            IF (ABS (A (IPIV, IPIV) ) .GT.ABS (PIVOT) ) THEN
!                PIVOT = A (IPIV, IPIV)
!                LC (K, 1) = IPIV
!                LC (K, 2) = IPIV
!            ENDIF
!        ENDDO out15
!    !
!    ! OTHERWISE, CHECK ALL ELEMENTS
!    !
!   ELSE
!        out20 : DO IPIV = 1, N
!            cycle20=.FALSE.
!            out21 : DO JPIV = 1, N
!                IF(cycle20) CYCLE out21
!                cycle21=.FALSE.
!                IF (KM1.GT.0) THEN
!                    out22 : DO I = 1, KM1
!                        IF(cycle20.OR.cycle21) CYCLE out22
!                        IF (IPIV.EQ.LC (I, 1) ) cycle20=.TRUE.
!                        IF (JPIV.EQ.LC (I, 2) ) cycle21=.TRUE.
!                    ENDDO out22
!                ENDIF
!                IF(cycle20.OR.cycle21) CYCLE out21
!                IF (ABS (A (IPIV, JPIV) ) .GT.ABS (PIVOT) ) THEN
!                    PIVOT = A (IPIV, JPIV)
!                    LC (K, 1) = IPIV
!                    LC (K, 2) = JPIV
!                ENDIF
!            ENDDO out21
!        ENDDO out20
!   ENDIF
!    !
!    IF (ABS (PIVOT) .LE.EPS) THEN
!        ICOD = 1
!        CYCLE out10
!    ENDIF
!    !
!    !.... INVERSION PROPREMENT DITE
!    !     -------------------------
!    IPIV = LC (K, 1)
!    JPIV = LC (K, 2)
!    PIVINV = one / PIVOT
!    DO J = 1, N
!        A (IPIV, J) = A (IPIV, J) * PIVINV
!    ENDDO
!    A (IPIV, JPIV) = PIVINV
!    DO I = 1, N
!        IF (I.NE.IPIV) THEN
!            AIJPIV = A (I, JPIV)
!            A (I, JPIV) = - AIJPIV * PIVINV
!            DO J = 1, N
!                IF (J.NE.JPIV) A (I, J) = A (I, J) - AIJPIV * A (IPIV, J)
!            ENDDO
!        ENDIF
!    ENDDO
!ENDDO out10
!
!IF(icod==1) RETURN
!
!!.... REMISE EN ORDRE
!!     ---------------
!DO J = 1, N
!    DO I = 1, N
!        IPIV = LC(I, 1)
!        JPIV = LC(I, 2)
!        TR (JPIV) = A(IPIV, J)
!    ENDDO
!    A (:,J) = TR
!ENDDO
!!
!DO I = 1, N
!    DO J = 1, N
!        IPIV = LC (J, 1)
!        JPIV = LC (J, 2)
!        TR (IPIV) = A (I, JPIV)
!    ENDDO
!    A(I,:) = TR
!ENDDO
!!
!!cccc      IF (DIADOM) THEN
!!        WRITE(*,*) 'DIAGONALLY DOMINANT'
!!      ELSE
!!        WRITE (*,*) 'NOT DIAG. DOM.'
!!      ENDIF
!!cccc      write(*,*) 'diag min, off max = ',dimin,omax
!END SUBROUTINE PMINVM
!
!!SSSSSS SUBROUTINE MULMM (A, B, C, N1, N2, N3, NASIZE)
!SUBROUTINE MULMM (A, B, C, N1, N2, N3, NASIZE)
!!=======================================================================
!!
!!       UTILITAIRE - MULTIPLICATION MATRICIELLE   A = B * C
!!                    A EST UNE MATRICE (N1,N3)
!!                    B EST UNE MATRICE (N1,N2)
!!                    C EST UNE MATRICE (N2,N3)
!!-----------------------------------------------------------------------
!!
!!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!!      IMPLICIT INTEGER (I-N)
!INTEGER, INTENT(IN) :: N1, N2, N3, NASIZE
!INTEGER :: i, j, k
!DOUBLEPRECISION ::  A(NASIZE, NASIZE), B(NASIZE, NASIZE), C(NASIZE,NASIZE)
!DO 10, J = 1, N1
!   DO 11, I = 1, N3
!      A (I, J) = ZERO
!      DO 12, K = 1, N2
!         A (I, J) = A (I, J) + B (K, J) * C (I, K)
!   12       END DO
!   11    END DO
!   10 END DO
!!
!END SUBROUTINE MULMM
!
!!SSSSSS SUBROUTINE MULMV (A, B, C, NL, NC, NASIZE)
!SUBROUTINE MULMV (A, B, C, NL, NC, NASIZE)
!!=======================================================================
!!
!!       UTILITAIRE - MULTIPLICATION MATRICE-VECTEUR  A = B * C
!!                    B EST UNE MATRICE (NL,NC)
!!                    A,C SONT DES VECTEURS (NL)
!!
!!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!!      IMPLICIT INTEGER (I-N)
!INTEGER, INTENT(IN) :: NL, NC, NASIZE
!INTEGER :: i, k
!DOUBLEPRECISION :: A(NASIZE), B(NASIZE, NASIZE), C(NASIZE)
!!
!DO 10, I = 1, NL
!   A (I) = ZERO
!   DO 11, K = 1, NC
!      A (I) = A (I) + B (K, I) * C (K)
!   11    END DO
!   10 END DO
!!
!END SUBROUTINE MULMV
