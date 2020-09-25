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
!
INTEGER :: IIN, NINP, TIME (5)  
DOUBLEPRECISION TIH, SIMNOW, SIMSTP, INLAST, INTIME, FNEXT (NINP), ARRAY (NINP)
INTEGER :: i, j
DOUBLEPRECISION :: SIMEND
!
SIMEND = SIMNOW + SIMSTP  
!
! CHECK IF ANY DATA NEEDS TO BE READ
!
IF (INTIME.GE.SIMEND) THEN  
   DO 5 I = 1, NINP  
    5    ARRAY (I) = FNEXT (I)  
   GOTO 1000  
ENDIF  
!
! SAVE CURRENT DATA IN OUTPUT ARRAY
!
DO 10 I = 1, NINP  
   ARRAY (I) = (INTIME-SIMNOW) * FNEXT (I)  
   10 END DO  
!
! READ DATA AND ADD INTO TOTALS UNTIL END OF SIMULATION TIMESTEP
!
   20 READ (IIN, *, END = 9999) (TIME (I), I = 1, 5), (FNEXT (J), &
 J = 1, NINP)
INLAST = INTIME  
INTIME = HOUR_FROM_DATE(TIME (1), TIME (2), TIME (3), TIME (4), TIME (5) ) &
 - TIH
!
IF (INTIME.LT.SIMEND) THEN  
   DO 30 I = 1, NINP  
      ARRAY (I) = ARRAY (I) + ( (INTIME-INLAST) * FNEXT (I) )  
   30    END DO  
   GOTO 20  
ELSE  
   DO 40 I = 1, NINP  
      ARRAY (I) = ARRAY (I) + ( (SIMEND-INLAST) * FNEXT (I) )  
   40    END DO  
ENDIF  
!
! CALCULATE AVERAGE OVER SIMULATION TIMESTEP
!
DO 50 I = 1, NINP  
   ARRAY (I) = ARRAY (I) / SIMSTP  
   50 END DO  
!
! RETURN TO CALLING ROUTINE
!
 1000 RETURN  
!
! FATAL ERROR - END OF FILE REACHED - SET INTIME TO INDICATE ERROR
!
 9999 INTIME = marker999  
RETURN  
!
END SUBROUTINE FINPUT
!
!----------------------------------------------------------------------



!SSSSSS SUBROUTINE HINPUT (IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, &
SUBROUTINE HINPUT (IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, &
 HLAST, HNEXT, NINP, ARRAY)
!----------------------------------------------------------------------
!
! GENERAL SUBROUTINE TO READ IN BREAKPOINT TIME-SERIES OF HEAD DATA.
! HEAD DATA ARE INTERPOLATED ONTO THE MID-POINT OF THE SIMULATION TIMEST
!
! PARAMETERS:
!        (INPUT)  IIN     FILE UNIT NUMBER FOR READING DATA
!        (INPUT)  TIH     START TIME OF SIMULATION SINCE REFERENCE DATE
!        (INPUT)  SIMNOW  START TIME OF CURRENT SIMULATION TIMESTEP
!        (INPUT)  SIMSTP  CURRENT SIMULATION TIMESTEP
! (INPUT/OUTPUT)  INLAST  LAST TIME OF READING DATA
! (INPUT/OUTPUT)  INTIME  CURRENT TIME FOR READING DATA
! (INPUT/OUTPUT)  HLAST   LAST VALUE READ FROM INPUT FILE AT TIME 'INLAS
! (INPUT/OUTPUT)  HNEXT   NEXT VALUE READ FROM INPUT FILE AT TIME 'INTIM
!        (INPUT)  NINP    NUMBER OF DATA ITEMS TO READ
!       (OUTPUT)  ARRAY   ARRAY OF INTERPOLATED DATA ITEMS
!
!----------------------------------------------------------------------
!
INTEGER :: IIN, NINP, TIME (5) 
INTEGER :: i, j
DOUBLEPRECISION TIH, SIMNOW, SIMSTP, INLAST, INTIME, ARRAY (NINP), &
 HLAST (NINP), HNEXT (NINP)
DOUBLEPRECISION :: simend, simmid
LOGICAL :: goto10, markertest
!
SIMEND = SIMNOW + SIMSTP  
SIMMID = SIMNOW + 0.5 * SIMSTP  
!
! IF MID-POINT OF TIMESTEP PASSED, INTERPOLATE DATA

DO
    10 IF (INTIME.GE.SIMMID.AND.INLAST.LT.SIMMID) THEN  
        DO 20 I = 1, NINP  
            ARRAY (I) = HLAST (I) + (HNEXT (I) - HLAST (I) ) * ( (SIMMID-INLAST) / (INTIME-INLAST) )
        20 ENDDO  
    ENDIF  
    ! READ DATA UNTIL END OF SIMULATION TIMESTEP
    goto10 = .FALSE.
    IF (INTIME.LT.SIMEND) THEN  
        DO 30 I = 1, NINP  
            HLAST (I) = HNEXT (I)  
        30 ENDDO  
        READ (IIN, *, END = 9999) (TIME (I), I = 1, 5), (HNEXT (J), J = 1, NINP)
        INLAST = INTIME  
        INTIME = HOUR_FROM_DATE(TIME (1), TIME (2), TIME (3), TIME (4), TIME (5)) - TIH
        goto10 = .TRUE.  
    ENDIF
    markertest = .FALSE.
    GOTO 223
        9999 INTIME = marker999
        markertest=.TRUE.
    223 CONTINUE
    IF(.NOT.goto10 .OR. markertest) EXIT
ENDDO  
!RETURN  
!! FATAL ERROR - END OF FILE REACHED
! 9999 INTIME = marker999  
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
    print*,' date trap'
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
SUBROUTINE TRIDAG (A, B, C, R, U, N)  
!                            SOLVES FOR VECTOR U OF LENGTH N
!                            THE TRIDIAGONAL SET A,B,C WHERE
!                            R IS THE R.H.S.
!      IMPLICIT INTEGER (I-N)
!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!                             TO BE COMPATIBLE WITH INCLUDE FILE AL.P
INTEGER, INTENT(IN) :: N 
INTEGER             :: j
DOUBLEPRECISION     :: A(:), B(:), C(:), R(:), U(:), GAM(n), bet, oobet
BET  = B(1)
oobet = one/bet
U(1) = oobet * R(1) 
DO J = 2, N  
   GAM(J) = oobet*C(J-1)
   BET    = B(J) - A(J) * GAM(J)
   oobet  = one/bet 
   U(J)   = oobet*(R(J) - A(J) * U(J-1))  
ENDDO  
DO J = N-1,1,-1  
   U(J) = U(J) - GAM(J+1) * U(J+1)  
ENDDO  
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

      SUBROUTINE lubksb(a,n,indx,b)
      INTEGER         :: n, indx(n)
      doubleprecision :: a(n,n), b(n)
      INTEGER         :: i, ii, j, ll
      doubleprecision :: asum
      ii=0
      do 12 i=1,n
        ll=indx(i)
        asum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            asum=asum-a(i,j)*b(j)
11        continue
        else if (notzero(asum)) then
          ii=i
        endif
        b(i)=asum
12    continue
      do 14 i=n,1,-1
        asum=b(i)
        do 13 j=i+1,n
          asum=asum-a(i,j)*b(j)
13      continue
        b(i)=asum/a(i,i)
14    continue
      END SUBROUTINE lubksb
      
      SUBROUTINE ludcmp(a,n,indx,d, issing)
      INTEGER              :: n,indx(n)
      doubleprecision      :: d,a(n,n),TINY
      PARAMETER (TINY=1.0d-20)
      INTEGER              :: i,imax,j,k
      doubleprecision      :: aamax,dum,asum,vv(n)
      LOGICAL, INTENT(out) :: issing
      issing=.FALSE.
      d=1.
      do 12 i=1,n
        IF(issing) CYCLE
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        IF (ISZERO(aamax)) THEN
            issing=.TRUE.  !pause 'singular matrix in ludcmp'
            CYCLE
        ENDIF    
        vv(i)=1./aamax
12    continue
      IF(issing) RETURN
      do 19 j=1,n
        do 14 i=1,j-1
          asum=a(i,j)
          do 13 k=1,i-1
            asum=asum-a(i,k)*a(k,j)
13        continue
          a(i,j)=asum
14      continue
        aamax=0.
        do 16 i=j,n
          asum=a(i,j)
          do 15 k=1,j-1
            asum=asum-a(i,k)*a(k,j)
15        continue
          a(i,j)=asum
          dum=vv(i)*abs(asum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(iszero(a(j,j)))a(j,j)=TINY
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
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
!            NB. IF SET TO ZERO, OLD FORMAT (20I4) WILL BE USED.
!
!    ALSO INCLUDES THE POSSIBILITY OF FILLING AN INTEGER ARRAY
!    WITH DEFAULT VALUES IN WHICH CASE SHOULD HAVE
!                               KON=3
!                               INF=REQUIRED DEFAULT VALUE
!
!----------------------------------------------------------------------*
INTEGER, INTENT(IN)  :: KON, INF, IOF, INUM  
INTEGER, INTENT(OUT) :: IAOUT(:)  
INTEGER              :: I, I1, I2, IEL, J, K, L, LAL, LL1, NNX, NXX, IA (NXEE,NYEE)
CHARACTER(4)         :: TITLE (20)  
!----------------------------------------------------------------------*
!
!^^^^^^FILL IN SECTION
!
IF (KON.EQ.3) THEN  
   DO 2 IEL = NGDBGN, total_no_elements  
      IAOUT (IEL) = INF  
    2    END DO  
   RETURN  

ENDIF  
!
!^^^^^^READ SECTION
!
! CHECK I/O FORMATS OK FOR PRINTING ARRAY (LIMIT CURRENTLY SET TO 200)
!
IF ( (INUM.GT.0.AND.INUM.LT.10) .AND.NX.GT.500) THEN  
   WRITE (IOF, 5)  
    5 FORMAT  (' ', 'NX greater than 500. Change I/O formats in AREADI' &
&              / 'Program aborted.' )
   STOP  
ENDIF  
!
IF (KON.EQ.0.OR.KON.EQ.1) THEN  
!
    READ (INF, 10) TITLE  
        10 FORMAT   (20A4)  
    DO 40 I1 = 1, NY  
        K = NY + 1 - I1  
        IF (INUM.GT.0.AND.INUM.LT.10) THEN  
            READ (INF, 15) I2, (IA (J, K), J = 1, NX)  
                15 FORMAT      (I7, 1X, 500I1)  
            IF (I2.NE.K) THEN  
                WRITE (IOF, 18) TITLE, I2  
                STOP  
            ENDIF  
        ELSE  
            READ (INF, 20) I2  
                20 FORMAT       (I7)  
            IF (I2.NE.K) THEN  
                WRITE (IOF, 18) TITLE, I2  
                STOP  
            ENDIF  
  !          READ (INF, 30) (IA (J, K), J = 1, NX)  
              READ (INF, *) (IA (J, K), J = 1, NX)  
              30 FORMAT(20I4)  
        ENDIF  

    40 ENDDO  
18 FORMAT(//2X, 'ERROR IN DATA ', 20A4, //2X, 'IN THE VICINITY OF LINE K=', I5)
!^^^^^^CONVERT GRID ARRAY TO ELEMENT ARRAY ...
!
   DO 62 IEL = 1, total_no_elements  
      IAOUT (IEL) = 0  

   62    END DO  
   DO 64 I = 1, NX  
      DO 64 J = 1, NY  
         IEL = ICMXY (I, J)  
         IF (IEL.NE.0) IAOUT (IEL) = IA (I, J)  

   64    CONTINUE  
!
!^^^^^^ ... OR CONVERT ELEMENT ARRAY TO GRID ARRAY
!
ELSE  
!
   DO 66 I = 1, NX  
      DO 66 J = 1, NY  
         IA (I, J) = 0 
   66    CONTINUE  
   DO 68 IEL = NGDBGN, total_no_elements  
      IF (ICMREF (IEL, 1) .EQ.0) THEN  
         I = ICMREF (IEL, 2)  
         J = ICMREF (IEL, 3)  
         IA (I, J) = IAOUT (IEL)  
      ENDIF  
   68    END DO  
!

ENDIF  
!
!^^^^^^PRINT SECTION
!
IF (KON.EQ.0) RETURN !GOTO 180  
!
IF (KON.EQ.1) WRITE (IOF, 80) TITLE  

   80 FORMAT (/ 20A4)  
!
! CHECK FOR ALL ZEROES
!
!DO 110 I1 = 1, NX  
!   DO 110 I2 = 1, NY  
!      IF (IA (I1, I2) .EQ.0) GOTO 110  
!      GOTO 130  
!  110 CONTINUE
IF(I_ISZERO_A2(ia(1:nx,1:ny))) THEN 
    WRITE (IOF, 120)  
    120 FORMAT (' ALL VALUES ZERO'/' ==============='/)  
    RETURN !GOTO 180
ENDIF  
!
130 NNX = (NX - 1) / 10 + 1  

IF (INUM.GT.0.AND.INUM.LT.10) THEN  
    DO 127 I1 = 1, NY  
        K = NY + 1 - I1  
        WRITE (IOF, 125) K, (IA (J, K), J = 1, NX)  
        125 FORMAT    (' ', 'K=', I4, 1X, 500I1)  
    127 END DO  

ELSE  
    DO 170 L = 1, NNX  
        LAL = L * 10  
        LL1 = LAL - 9  
        NXX = MIN0 (NX, LAL)  
        WRITE (IOF, 140) (I, I = LL1, LAL)  
        140 FORMAT     ('0', 9X, 10('J=',I3,6X), /)  
        DO 150 I1 = 1, NY  
            K = NY + 1 - I1  
        150       WRITE (IOF, 160) K, (IA (J, K), J = LL1, NXX)  
        160 FORMAT     (' ', 'K=', I4, 2X, 10(I6,5X))  
    170 END DO  
ENDIF  
WRITE (IOF, 90)  

   90 FORMAT (//2X, 80('*'), //)  

  180 CONTINUE  
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
! Input arguments

INTEGER :: KON, INF, IOF  
! In|out arguments

DOUBLEPRECISION AOUT (NELEE)  
! Locals, etc
INTEGER :: I, J, K, L, I1, I2, IEL, IEL1, IEL2, LAL, LL1, NNX, &
 NXX
DOUBLEPRECISION B1, B2, A (NXEE, NYEE)  


CHARACTER (LEN=4) :: TITLE (20)  
!----------------------------------------------------------------------*
!
!^^^^^^READ SECTION
!
IF (KON.EQ.0.OR.KON.EQ.1) THEN  
!
   READ (INF, 10) TITLE  
   10 FORMAT   (20A4)  
   DO 40 I1 = 1, NY  
      READ (INF, 20) I2  
   20 FORMAT     (I7)  
      K = NY + 1 - I1  
      IF (I2.NE.K) THEN  
         WRITE (IOF, 25) TITLE, I2  
   25 FORMAT       (//2X, 'ERROR IN DATA ', 20A4, //2X, &
&        'IN THE VICINITY OF LINE K=', I5)
         STOP  
      ENDIF  
      READ (INF, 30) (A (J, K), J = 1, NX)  
   30 FORMAT     (10G7.0)  
   40    END DO  
!
!^^^^^^CONVERT GRID ARRAY TO ELEMENT ARRAY
!
   DO 64 I = 1, NX  
      DO 64 J = 1, NY  
         IEL = ICMXY (I, J)  
         IF (IEL.NE.0) AOUT (IEL) = A (I, J)  
   64    CONTINUE  
!
!^^^^^^CONVERT ELEMENT ARRAY TO GRID ARRAY
!
ELSE  
!
   DO 66 I = 1, NX  
      DO 66 J = 1, NY  
         A (I, J) = zero  
   66    CONTINUE  
   DO 68 IEL = NGDBGN, total_no_elements  
      IF (ICMREF (IEL, 1) .EQ.0) THEN  
         I = ICMREF (IEL, 2)  
         J = ICMREF (IEL, 3)  
         A (I, J) = AOUT (IEL)  
      ENDIF  
   68    END DO  
!
ENDIF  
!
!^^^^^^PRINT SECTION
!
IF (KON.EQ.0) RETURN !GOTO 180  
!
IF (KON.EQ.1) WRITE (IOF, 80) TITLE  
   80 FORMAT (/ 20A4)  
!
! CHECK FOR ALL ZEROES
!
!DO 110 I = 1, NEL  
!   IF (ISZERO(AOUT (I))) GOTO 110  
!   GOTO 130  
!  110 END DO  
!WRITE (IOF, 120)  
!  120 FORMAT (/ ' ALL VALUES ZERO'/' ==============='/)  
!GOTO 180

IF(ISZERO_A(aout(1:total_no_elements))) THEN 
    WRITE(IOF, 120)  
    120 FORMAT (' ALL VALUES ZERO'/' ==============='/)  
    RETURN !GOTO 180
ENDIF  

  
!
! PRINT ARRAY
!
  130 NNX = (NX - 1) / 10 + 1  
DO 170 L = 1, NNX  
   LAL = L * 10  
   LL1 = LAL - 9  
   NXX = MIN0 (NX, LAL)  
   WRITE (IOF, 140) (I, I = LL1, LAL)  
  140 FORMAT   ('0', 9X, 10('J=',I3,6X), /)  
   DO 150 I1 = 1, NY  
      K = NY + 1 - I1  
  150    WRITE (IOF, 160) K, (A (J, K), J = LL1, NXX)  
  160 FORMAT   (' ', 'K=', I4, 2X, 10G11.4)  
  170 END DO  
!
WRITE (IOF, 200)  
  200 FORMAT (/, 10X, 'LINK ', 6X, 'BANK1 ', 5X, 'BANK2 ', /)  
DO 175 I = 1, total_no_links  
   B1 = zero  
   B2 = zero
   IEL1 = ICMBK (I, 1)  
   IEL2 = ICMBK (I, 2)  
   IF (IEL1.GT.0) B1 = AOUT (IEL1)  
   IF (IEL2.GT.0) B2 = AOUT (IEL2)  
   WRITE (IOF, 210) I, AOUT (I), B1, B2  
  210 FORMAT   (1X, 'L= ', I4, 2X, 3G11.4)  
  175 END DO  
!
WRITE (IOF, 90)  
   90 FORMAT (//2X, 120('*'), //)  
!
  180 CONTINUE  
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