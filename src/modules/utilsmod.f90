!> summary: General numerical, date/time, and input helper routines.
!> author: JE, Newcastle University; SB, Newcastle University
!>
!> `utilsmod` contains shared utility routines used across SHETRAN. These
!> include vector copying, breakpoint time-series reading, date/hour conversion,
!> one-dimensional interpolation, tridiagonal linear solves, matrix products,
!> matrix inversion helpers, integer/real array readers, and a random-number
!> generator.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2008-12 | JE | 4.3.5F90 | Created during conversion to Fortran 90, replacing utility `.F` files. |
!> | 2026-03 | SB | 4.6 | Added date error trapping. |
!> @endhistory
MODULE utilsmod

USE SGLOBAL
USE AL_G, ONLY : NGDBGN, NX, NY, ICMXY, ICMREF
USE AL_C, ONLY : icmbk
IMPLICIT NONE

DOUBLEPRECISION, PARAMETER :: eps=1.0d-15 !! Singularity/zero tolerance used by matrix inversion.
CHARACTER(128)             :: msg         !! Error-message buffer passed to `ERROR`.

PRIVATE
PUBLIC :: TRIDAG, DCOPY, HOUR_FROM_DATE, TERPO1, FINPUT, HINPUT, AREADI, AREADR, &
          JEMATMUL_VM, JEMATMUL_MM, INVERTMAT, DATE_FROM_HOUR, RAN2 !OPEN_FILE !GET_START_END_IMPACT
CONTAINS


!> Copies a double-precision vector into another vector.
!>
!> This is the BLAS `dcopy` operation implemented locally, including support
!> for non-unit and negative increments.
!>
!> @note The routine follows the simple BLAS indexing convention but does not
!> validate `incx` or `incy`. Zero increments and overlapping source/destination
!> storage are therefore caller responsibilities.
!> @endnote
subroutine dcopy (n, dx, incx, dy, incy)
!     copies vector x to vector y
INTEGER, INTENT(IN)                        :: n    !! Number of values to copy.
INTEGER, INTENT(IN)                        :: incx !! Increment between values in `dx`.
INTEGER, INTENT(IN)                        :: incy !! Increment between values in `dy`.
DOUBLEPRECISION, DIMENSION(*), INTENT(IN)  :: dx   !! Source vector.
DOUBLEPRECISION, DIMENSION(*), INTENT(OUT) :: dy   !! Destination vector.
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



!> Reads breakpoint flux time-series data and averages over a timestep.
!>
!> The routine accumulates piecewise-constant flux values over the current
!> simulation timestep and returns timestep-average values.
!>
!> `FINPUT` is the general reader for breakpoint flux time series. Input records
!> contain a date/time followed by `NINP` flux values. Each flux value is treated
!> as constant over the interval ending at its record time `INTIME`, and the
!> returned `ARRAY(j)` is the average over the current simulation timestep
!> `[SIMNOW, SIMNOW+SIMSTP]`.
!>
!> Parameters are:
!>
!> | Argument | Intent | Meaning |
!> |:---------|:-------|:--------|
!> | `IIN` | input | File unit number for reading data. |
!> | `TIH` | input | Simulation start time since the reference date, in hours. |
!> | `SIMNOW` | input | Start time of the current simulation timestep, in model hours. |
!> | `SIMSTP` | input | Current simulation timestep length, in hours. |
!> | `INLAST` | input/output | Last breakpoint time read, relative to `TIH`. |
!> | `INTIME` | input/output | Current breakpoint time up to which `FNEXT` is valid. |
!> | `FNEXT` | input/output | Current flux vector valid up to `INTIME`; overwritten by newly read interval values. |
!> | `NINP` | input | Number of flux items to read from each record. |
!> | `ARRAY` | output | Timestep-average flux vector. |
!>
!> If the existing breakpoint already extends beyond the timestep end,
!> `ARRAY = FNEXT`. Otherwise the code integrates each piecewise-constant
!> segment and divides by the timestep:
!>
!> \[
!> ARRAY_j =
!> \frac{1}{SIMSTP}
!> \sum_m \Delta t_m\,F_{j,m},
!> \]
!>
!> where each \(\Delta t_m\) is the overlap between the current simulation
!> timestep and one breakpoint interval. Input dates are converted with
!> [[hour_from_date]] and shifted by `TIH`. If end-of-file is reached before a
!> complete timestep average can be formed, `INTIME` is set to `marker999`.
!>
!> @note A newly read record value is applied over `(INLAST, INTIME]`, where
!> `INTIME` is the record time just read. The caller is expected to maintain
!> `FNEXT`, `INLAST`, and `INTIME` between calls.
!> @endnote
SUBROUTINE FINPUT (IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, &
 FNEXT, NINP, ARRAY)
INTEGER, INTENT(IN)             :: IIN          !! File unit number for reading data.
INTEGER, INTENT(IN)             :: NINP         !! Number of flux items to read from each record.
DOUBLEPRECISION, INTENT(IN)     :: TIH          !! Simulation start time since the reference date, in hours.
DOUBLEPRECISION, INTENT(IN)     :: SIMNOW       !! Start time of the current simulation timestep, in model hours.
DOUBLEPRECISION, INTENT(IN)     :: SIMSTP       !! Current simulation timestep length, in hours.
DOUBLEPRECISION, INTENT(INOUT)  :: INLAST       !! Last breakpoint time read, relative to `TIH`.
DOUBLEPRECISION, INTENT(INOUT)  :: INTIME       !! Current breakpoint time up to which `FNEXT` is valid.
DOUBLEPRECISION, INTENT(INOUT)  :: FNEXT(NINP)  !! Flux vector valid up to `INTIME`; overwritten by new records.
DOUBLEPRECISION, INTENT(OUT)    :: ARRAY(NINP)  !! Timestep-average flux vector.
INTEGER                         :: TIME(5)      !! Calendar date/time fields read from the input record.
INTEGER                         :: i, j
DOUBLEPRECISION                 :: SIMEND
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



!> Reads breakpoint head time-series data and interpolates to timestep midpoint.
!>
!> The routine advances through input records until it can interpolate head data
!> at the midpoint of the current simulation timestep.
!>
!> `HINPUT` is the general reader for breakpoint head time series. Unlike
!> [[finput]], head values are not averaged as fluxes; they are linearly
!> interpolated to the midpoint of the current timestep,
!> `SIMMID = SIMNOW + 0.5*SIMSTP`.
!>
!> Parameters are:
!>
!> | Argument | Intent | Meaning |
!> |:---------|:-------|:--------|
!> | `IIN` | input | File unit number for reading data. |
!> | `TIH` | input | Simulation start time since the reference date, in hours. |
!> | `SIMNOW` | input | Start time of the current simulation timestep, in model hours. |
!> | `SIMSTP` | input | Current simulation timestep length, in hours. |
!> | `INLAST` | input/output | Previous breakpoint time, relative to `TIH`. |
!> | `INTIME` | input/output | Next breakpoint time, relative to `TIH`. |
!> | `HLAST` | input/output | Head vector read at `INLAST`. |
!> | `HNEXT` | input/output | Head vector read at `INTIME`; overwritten when new records are read. |
!> | `NINP` | input | Number of head values to read from each record. |
!> | `ARRAY` | output | Head vector interpolated to the timestep midpoint. |
!>
!> Once the midpoint lies between the stored breakpoint times,
!> \(INLAST < SIMMID \le INTIME\), the interpolation is
!>
!> \[
!> ARRAY_j =
!> HLAST_j + (HNEXT_j-HLAST_j)
!> \frac{SIMMID-INLAST}{INTIME-INLAST}.
!> \]
!>
!> The routine then continues reading records until the current timestep end is
!> covered. Input dates are converted with [[hour_from_date]] and shifted by
!> `TIH`. If end-of-file is reached unexpectedly, `INTIME` is set to
!> `marker999`.
!>
!> @note The interpolation assignment is made only when
!> `INLAST < SIMMID <= INTIME`. The caller must carry `HLAST`, `HNEXT`,
!> `INLAST`, and `INTIME` between calls so the timestep midpoint is bracketed,
!> or can be bracketed by reading additional records.
!> @endnote
SUBROUTINE HINPUT (IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, &
 HLAST, HNEXT, NINP, ARRAY)
INTEGER, INTENT(IN)             :: IIN          !! File unit number for reading data.
INTEGER, INTENT(IN)             :: NINP         !! Number of head values to read from each record.
DOUBLEPRECISION, INTENT(IN)     :: TIH          !! Simulation start time since the reference date, in hours.
DOUBLEPRECISION, INTENT(IN)     :: SIMNOW       !! Start time of the current simulation timestep, in model hours.
DOUBLEPRECISION, INTENT(IN)     :: SIMSTP       !! Current simulation timestep length, in hours.
DOUBLEPRECISION, INTENT(INOUT)  :: INLAST       !! Previous breakpoint time, relative to `TIH`.
DOUBLEPRECISION, INTENT(INOUT)  :: INTIME       !! Next breakpoint time, relative to `TIH`.
DOUBLEPRECISION, INTENT(INOUT)  :: HLAST(NINP)  !! Head vector read at `INLAST`.
DOUBLEPRECISION, INTENT(INOUT)  :: HNEXT(NINP)  !! Head vector read at `INTIME`; overwritten by new records.
DOUBLEPRECISION, INTENT(OUT)    :: ARRAY(NINP)  !! Head vector interpolated to the timestep midpoint.
INTEGER                         :: TIME(5)      !! Calendar date/time fields read from the input record.
INTEGER                         :: i, j
DOUBLEPRECISION                 :: simend, simmid
LOGICAL                         :: goto10, markertest
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



!> Converts a calendar date/time to simulation hours since 1950-01-01 00:00.
!>
!> Leap years are accounted for. The function checks the round trip through
!> `DATE_FROM_HOUR` and stops with a diagnostic if the supplied date is invalid.
!>
!> Entry requirements:
!>
!> | Requirement | Reason |
!> |:------------|:-------|
!> | `KYEAR >= 1949` | Required by the legacy year-offset calculation. |
!> | `1 <= KMTH <= 12` | Required before indexing the month-offset table. |
!>
!> The legacy comments describe the returned value as hours since 1 January
!> 1950 at 00:00. The implemented convention is the one used by the paired
!> [[date_from_hour]] routine and includes the one-based calendar day in the
!> accumulated day count:
!>
!> \[
!> r = 24\left(D_y + D_m + KDAY\right) + KHOUR + \frac{KMIN}{60},
!> \]
!>
!> where `D_y` is the number of days in complete years since 1950, including
!> leap years, and `D_m` is the number of complete days before month `KMTH` in
!> `KYEAR`. Thus `1950-01-01 00:00` maps to 24 hours under this convention, not
!> zero. A small one-hundredth-second offset is added to avoid minute-level
!> roundoff errors in the reverse conversion check.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-12-09 | RAH | 3.4.1 | Removed `IMPLICIT INTEGER*2 (I-N)`. |
!> | 1998-06-11 | RAH | 4.2 | Replaced `60.` with `6D1` to eliminate rounding error; added explicit typing. |
!> @endhistory
 FUNCTION hour_from_date(KYEAR, KMTH, KDAY, KHOUR, KMIN)  RESULT(r)
INTEGER, INTENT(IN) :: kyear    !! Calendar year.
INTEGER, INTENT(IN) :: kmth     !! Calendar month number.
INTEGER, INTENT(IN) :: kday     !! Calendar day of month.
INTEGER, INTENT(IN) :: khour    !! Hour of day.
INTEGER, INTENT(IN) :: kmin     !! Minute of hour.
INTEGER             :: d        !! One-based day count used by the implemented model-hour convention.
INTEGER             :: check(6) !! Date returned by the round-trip validity check.
DOUBLEPRECISION     :: r        !! Model hour count under the SHETRAN date convention.
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

!> Returns the number of days in complete years since 1950-01-01.
!>
!> Leap days are counted by iterating over candidate leap years from 1952 up to
!> `y-1`, using [[is_leap]] for the Gregorian leap-year rule.
FUNCTION days_in_years_since_1950(y) RESULT(r)
INTEGER, INTENT(IN) :: y !! Year at the end of the counted interval.
INTEGER             :: i !! Candidate leap year.
INTEGER             :: r !! Days in complete years from 1950-01-01 to year `y`.
r = (y - 1950) * 365
DO i=1952, y-1, 4
    IF(IS_LEAP(i)) r=r+1
ENDDO
END FUNCTION days_in_years_since_1950


!> Returns whether a year is a leap year in the Gregorian calendar.
FUNCTION is_leap(y) RESULT(r)
!A year will be a leap year if it is divisible by 4 but not by 100.
!If a year is divisible by 4 and by 100, it is not a leap year unless it is also divisible by 400.
INTEGER, INTENT(IN) :: y !! Calendar year to test.
LOGICAL             :: r !! True when `y` is a Gregorian leap year.
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



!> Returns the day offset to the start of a month in a given year.
!>
!> Month offsets are zero-based (`January -> 0`). Leap years add one day for
!> months after February. The routine traps `m < 1` through `ERROR`, but it does
!> not explicitly guard `m > 12` before indexing the month table.
FUNCTION days_to_start_month(m, y) RESULT(r)
INTEGER, INTENT(IN) :: m      !! Calendar month number.
INTEGER, INTENT(IN) :: y      !! Calendar year used for leap-day adjustment.
INTEGER, PARAMETER  :: sd(12)=[0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334] !! Non-leap offsets.
INTEGER             :: r      !! Day offset to the start of month `m`.
IF(M<1) THEN
    WRITE(MSG,*) 'Date problem, probably with rainfall or evaporation - are their start dates specified correctly in their files?'
    CALL ERROR (FFFATAL, 4820, pppri, 0, 0, msg)
ENDIF
r = sd(m)
IF(IS_LEAP(y).AND. m>2) r = r + 1
END FUNCTION days_to_start_month



!> Converts the model hour count used by [[hour_from_date]] to date components.
!>
!> The result array is `[year, month, day, hour, minute, second]`. The
!> conversion uses deliberately low initial estimates (`days/366` for the year
!> and `mthdays/32` for the month), then increments to the correct year/month.
!> A day value of zero triggers a stop as a date-trapping guard.
FUNCTION date_from_hour(h) RESULT(r)
DOUBLEPRECISION, INTENT(IN) :: h       !! Model hour count under the SHETRAN date convention.
INTEGER                    :: r(6)    !! Date as `[year, month, day, hour, minute, second]`.
INTEGER                    :: hours   !! Whole hours in `h`.
INTEGER                    :: days    !! Whole days in `h`.
INTEGER                    :: year    !! Calendar year estimate/refinement.
INTEGER                    :: month   !! Calendar month estimate/refinement.
INTEGER                    :: mthdays !! Days elapsed within the current year.
INTEGER                    :: mins    !! Whole minutes in the fractional hour.
INTEGER                    :: sec     !! Whole seconds in the fractional minute.
DOUBLEPRECISION            :: rmins   !! Fractional-hour remainder converted to minutes.

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



!> Multiplies two dense matrices using explicit loops.
!>
!> With the declared storage, the returned array satisfies
!>
!> \[
!>   A(i,j)=\sum_{k=1}^{n2} C(i,k)\,B(k,j),
!> \]
!>
!> for `A(n3,n1)`, `B(n2,n1)`, and `C(n3,n2)`. In conventional matrix
!> notation this is `A = C * B`, despite the old inline comment `A = B * C`.
FUNCTION jematmul_mm(b, c, n1, n2, n3) RESULT(a)
! A = B * C
INTEGER, INTENT(IN)         :: n1       !! Number of columns in the returned matrix.
INTEGER, INTENT(IN)         :: n2       !! Shared inner dimension.
INTEGER, INTENT(IN)         :: n3       !! Number of rows in the returned matrix.
DOUBLEPRECISION, INTENT(IN) :: b(n2,n1) !! Right-hand matrix in declared storage.
DOUBLEPRECISION, INTENT(IN) :: c(n3,n2) !! Left-hand matrix in declared storage.
DOUBLEPRECISION             :: a(n3,n1) !! Matrix product `C * B`.
INTEGER                     :: i, j, k
DO i=1,n3
    DO j=1,n1
        a(i,j) = zero
        DO k=1,n2
            a(i,j) = a(i,j) + b(k,j)*c(i,k)
        ENDDO
    ENDDO
ENDDO
END FUNCTION jematmul_mm


!> Multiplies a dense matrix by a vector using explicit loops.
!>
!> The returned vector satisfies
!>
!> \[
!>   A_i=\sum_{k=1}^{n2} B(k,i)\,C_k,
!> \]
!>
!> so the declared `B(n2,n1)` is used as the transpose of the conventional
!> `n1 x n2` matrix.
FUNCTION jematmul_vm(b, c, n1, n2)  RESULT(a)
! A = B * C
INTEGER, INTENT(IN)         :: n1      !! Length of the returned vector.
INTEGER, INTENT(IN)         :: n2      !! Shared inner dimension.
DOUBLEPRECISION, INTENT(IN) :: b(n2,n1) !! Matrix stored transposed relative to conventional notation.
DOUBLEPRECISION, INTENT(IN) :: c(n2)   !! Input vector.
DOUBLEPRECISION             :: a(n1)   !! Matrix-vector product.
INTEGER                     :: i, k
DO i=1,n1
    a(i) = zero
    DO k=1,n2
        a(i) = a(i) + b(k,i) * c(k)
    ENDDO
ENDDO
END FUNCTION jematmul_vm



!> Interpolates a one-dimensional time-varying parameter.
!>
!> The routine updates one parameter value from a table of relative values and
!> tabulated times, using the current simulation time in hours.
!>
!> `TERPO1` is a service routine for time-varying parameters whose tabulated
!> values are stored as relative multipliers. The arguments are:
!>
!> | Argument | Meaning |
!> |:---------|:--------|
!> | `YCURR` | Current parameter array to update. |
!> | `YTAB` | Tabulated relative values of the parameter. |
!> | `YINIT` | Initial or reference parameter values. |
!> | `TCURR` | Current simulation time, in hours. |
!> | `TTAB` | Tabulated times, in days. |
!> | `NCT` | Current table-position counter for each parameter. |
!> | `NPAR` | Size of the parameter array. |
!> | `I` | Parameter-array position being updated. |
!>
!> The routine advances `NCT(I)` to the interval containing `TCURR/24`, then
!> linearly interpolates the relative multiplier:
!>
!> \[
!> Y_{rel} =
!> YTAB_{I,k}
!> + \frac{TCURR-24\,TTAB_{I,k}}
!>        {24\,(TTAB_{I,k+1}-TTAB_{I,k})}
!>   \left(YTAB_{I,k+1}-YTAB_{I,k}\right),
!> \]
!>
!> where \(k=NCT(I)\) after the interval update. The absolute value returned to
!> the model is
!>
!> \[
!> YCURR_I = Y_{rel}\,YINIT_I.
!> \]
!>
!> @note `NCT(I)` may jump by more than one interval because `ITERP` is computed
!> with integer division of the time offset by the current interval length. The
!> caller must provide increasing `TTAB` values and enough table entries for the
!> updated `NCT(I)+1`; no bounds or zero-interval checks are made here.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-05 | RAH | 3.4.1 | Removed `IMPLICIT INTEGER*2`. |
!> | 1997-05-16 | RAH | 4.1 | Added explicit typing; made `*TAB` assumed-size arrays; removed redundant `ITAB` argument. |
!> @endhistory
SUBROUTINE TERPO1 (YCURR, TCURR, YTAB, TTAB, NCT, YINIT, NPAR, I)
INTEGER, INTENT(IN)             :: NPAR          !! Size of the parameter array.
INTEGER, INTENT(IN)             :: I             !! Parameter-array position being updated.
DOUBLEPRECISION, INTENT(IN)     :: TCURR         !! Current simulation time, in hours.
DOUBLEPRECISION, INTENT(IN)     :: YTAB(NPAR,*)  !! Tabulated relative values of the parameter.
DOUBLEPRECISION, INTENT(IN)     :: TTAB(NPAR,*)  !! Tabulated times, in days.
DOUBLEPRECISION, INTENT(IN)     :: YINIT(NPAR)   !! Initial or reference parameter values.
INTEGER, INTENT(INOUT)          :: NCT(NPAR)     !! Current table-position counter for each parameter.
DOUBLEPRECISION, INTENT(INOUT)  :: YCURR(NPAR)   !! Current parameter array to update.
INTEGER                         :: ITERP, NCTERP
DOUBLEPRECISION                 :: DIFFA, DIFFB, DIFFC, YREL
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



!> Solves a tridiagonal linear system.
!>
!> This is the Thomas algorithm for a tridiagonal matrix with lower diagonal
!> `A`, diagonal `B`, upper diagonal `C`, right-hand side `R`, and solution `U`.
!> It solves for the vector `U` of length `N` in
!>
!> \[
!> A_i U_{i-1} + B_i U_i + C_i U_{i+1} = R_i,
!> \qquad i=1,\ldots,N,
!> \]
!>
!> with the usual endpoint interpretation that `A(1)` and `C(N)` are not used.
!> The routine performs a forward elimination followed by back substitution,
!> overwriting only the output vector `U` and local work array `GAM`.
!>
!> @note No pivoting or zero-pivot protection is performed. `B(1)` and every
!> subsequent reduced diagonal `BET` must be non-zero.
!> @endnote
SUBROUTINE TRIDAG (A, B, C, R, U, N)
INTEGER, INTENT(IN)             :: N      !! Number of equations.
DOUBLEPRECISION, INTENT(IN)     :: A(:)   !! Lower diagonal; `A(1)` is not used.
DOUBLEPRECISION, INTENT(IN)     :: B(:)   !! Main diagonal.
DOUBLEPRECISION, INTENT(IN)     :: C(:)   !! Upper diagonal; `C(N)` is not used.
DOUBLEPRECISION, INTENT(IN)     :: R(:)   !! Right-hand-side vector.
DOUBLEPRECISION, INTENT(OUT)    :: U(:)   !! Solution vector.
INTEGER                         :: j
DOUBLEPRECISION                 :: GAM(n), bet, oobet
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



!> Inverts a dense matrix in place using LU decomposition.
!>
!> `invertmat` replaces the input matrix `A` by `A^{-1}`. For `N=1` it returns
!> the scalar reciprocal directly. For `N>1` it forms the identity matrix,
!> factors `A` with [[ludcmp]], and solves
!>
!> \[
!> A x_j = e_j,\qquad j=1,\ldots,N,
!> \]
!>
!> with [[lubksb]] for each identity-column right-hand side \(e_j\). The solved
!> columns \(x_j\) are then copied back into `A`, giving
!>
!> \[
!> A^{-1} = [x_1\;x_2\;\cdots\;x_N].
!> \]
!>
!> `ICOD=0` indicates success. `ICOD=1` indicates an invalid size, a zero
!> scalar, or a singular matrix detected by the LU factorisation.
!>
!> @note For `N > 1`, the input matrix is passed directly to [[ludcmp]] and is
!> overwritten by the LU factors before singularity status is known. If
!> `ICOD=1` is returned after factorisation, `A` should not be assumed to retain
!> the original matrix.
!> @endnote
SUBROUTINE invertmat(a,n,icod)
INTEGER, INTENT(IN)                            :: n    !! Matrix order.
INTEGER, INTENT(OUT)                           :: icod !! Status code: `0` success, `1` failure.
INTEGER                                        :: i, j, indx(n)
DOUBLEPRECISION, DIMENSION(n,n), INTENT(INOUT) :: a    !! Matrix to replace with its inverse.
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



!> Solves an LU-decomposed linear system by back substitution.
!>
!> `lubksb` applies the row permutation stored in `indx` and overwrites `b` with
!> the solution vector for the matrix factors produced by [[ludcmp]]. This is
!> the Numerical Recipes LU back-substitution algorithm used by [[invertmat]].
!>
!> After [[ludcmp]], the array `A` stores the combined lower and upper
!> triangular factors of a pivoted decomposition
!>
!> \[
!> P A_{orig} = L U,
!> \]
!>
!> where `L` has an implicit unit diagonal and `U` is stored on and above the
!> diagonal. `lubksb` solves
!>
!> \[
!> L y = P b,\qquad U x = y,
!> \]
!>
!> by forward substitution followed by back substitution, returning `x` in
!> `b`. The `ii` marker skips leading zero terms in the permuted right-hand
!> side, matching the Numerical Recipes implementation.
      SUBROUTINE lubksb(a,n,indx,b)
      INTEGER, INTENT(IN)         :: n       !! Matrix order.
      INTEGER, INTENT(IN)         :: indx(n) !! Pivot-row indices from `ludcmp`.
      doubleprecision, INTENT(IN) :: a(n,n)  !! Combined LU factors from `ludcmp`.
      doubleprecision, INTENT(INOUT) :: b(n) !! Right-hand side on entry; solution on exit.
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



!> Performs LU decomposition with partial pivoting.
!>
!> `ludcmp` factors `a` in place, records pivot rows in `indx`, returns the
!> parity factor `d`, and sets `issing` when the matrix is singular or has a
!> zero scaling row. The factorisation is used by [[invertmat]] before
!> [[lubksb]] solves each right-hand side.
!>
!> The decomposition is a scaled partial-pivoting LU factorisation. For each
!> row, the scaling value
!>
!> \[
!> v_i = \frac{1}{\max_j |a_{ij}|}
!> \]
!>
!> is used to choose the pivot row that maximises \(v_i |a_{ij}|\) in the
!> current column. Row swaps are recorded in `INDX`, and each swap changes the
!> sign of `D`. On successful return, `A` stores `L` below the diagonal and `U`
!> on and above the diagonal:
!>
!> \[
!> P A_{orig} = L U.
!> \]
!>
!> If a row has zero scale the matrix is singular and `ISSING` is set. If a
!> selected pivot is exactly zero after elimination, the routine substitutes the
!> small value `TINY=1.0d-20`, preserving the legacy Numerical Recipes behaviour.
      SUBROUTINE ludcmp(a,n,indx,d, issing)
      INTEGER, INTENT(IN)       :: n       !! Matrix order.
      INTEGER, INTENT(OUT)      :: indx(n) !! Pivot-row index for each column.
      doubleprecision, INTENT(OUT) :: d    !! Pivot-parity factor.
      doubleprecision, INTENT(INOUT) :: a(n,n) !! Matrix overwritten by combined LU factors.
      doubleprecision      :: TINY
      PARAMETER (TINY=1.0d-20)
      INTEGER              :: i,imax,j,k
      doubleprecision      :: aamax,dum,asum,vv(n)
      LOGICAL, INTENT(out) :: issing !! True if a zero scaling row marks the matrix singular.
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



!> Reads and optionally echoes an integer grid/element array.
!>
!> `AREADI` implements the legacy `KON` control modes for integer AL input:
!> read a grid and convert it to element order, convert an existing element
!> array back to a grid, or read and print a grid without conversion. The
!> resulting integer element array is returned in `IAOUT`.
!>
!> Control modes are:
!>
!> | `KON` | Action |
!> |:------|:-------|
!> | 0 | Read grid array `IA`, convert it to element array `IAOUT`, and do not print. |
!> | 1 | Read grid array `IA`, convert it to element array `IAOUT`, and print the grid array. |
!> | 2 | Do not read; convert the input element array `IAOUT` back to grid array `IA` and print it. |
!> | 3 | Fill `IAOUT(NGDBGN:total_no_elements)` with the default value supplied in `INF`; no file read is performed. |
!>
!> Parameters are:
!>
!> | Argument | Meaning |
!> |:---------|:--------|
!> | `IAOUT` | Integer element array returned by the routine, or input element array when `KON=2`. |
!> | `KON` | Control parameter selecting read/convert/print/default-fill behaviour. |
!> | `INF` | Input file unit for read modes; default integer value when `KON=3`. |
!> | `IOF` | Output file unit used when printing the grid array. |
!> | `INUM` | Expected range/count of integer codes; zero selects old `20I4` input. |
!>
!> For read modes, the grid-to-element mapping is
!>
!> \[
!> IAOUT_{ICMXY(i,j)} = IA_{i,j}
!> \quad\text{for each active grid cell } ICMXY(i,j)\ne0.
!> \]
!>
!> For `KON=2`, the reverse reporting grid is assembled from grid elements using
!>
!> \[
!> IA_{ICMREF(iel,2),ICMREF(iel,3)} = IAOUT_{iel}
!> \quad\text{where } ICMREF(iel,1)=0.
!> \]
!>
!> @note Only `KON=0`, `1`, and `3` are tested explicitly. Any other value uses
!> the `KON=2` convert-and-print path. The single-digit integer grid format
!> (`0 < INUM < 10`) is hard-limited to `NX <= 500`.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-09-28 | RAH | 3.4.1 | Added explicit `IMPLICIT` statement in the original source. |
!> | 1995-07-24 | GP | 4.0 | Initialised `IAOUT` when `KON=0` or `KON=1`. |
!> | 1997-08-04 | RAH | 4.1 | Added explicit typing; corrected `TITLE` from implicit double precision. |
!> @endhistory
SUBROUTINE AREADI (IAOUT, KON, INF, IOF, INUM)
INTEGER, INTENT(IN)    :: KON      !! Control parameter selecting read/convert/print/default-fill behaviour.
INTEGER, INTENT(IN)    :: INF      !! Input file unit for read modes; default integer value when `KON=3`.
INTEGER, INTENT(IN)    :: IOF      !! Output file unit used when printing the grid array.
INTEGER, INTENT(IN)    :: INUM     !! Expected range/count of integer codes; zero selects old `20I4` input.
INTEGER, INTENT(INOUT) :: IAOUT(:) !! Integer element array; also input when converting elements back to grid.
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



!> Reads and optionally echoes a double-precision grid/element array.
!>
!> `AREADR` mirrors [[AREADI]] for floating-point input. Depending on `KON`, it
!> reads grid values and converts them to SHETRAN element order, converts an
!> existing element array for reporting, or reads and prints a grid directly.
!>
!> Control modes are:
!>
!> | `KON` | Action |
!> |:------|:-------|
!> | 0 | Read double-precision grid array `A`, convert it to element array `AOUT`, and do not print. |
!> | 1 | Read double-precision grid array `A`, convert it to element array `AOUT`, and print the grid array. |
!> | 2 | Do not read; convert the input element array `AOUT` back to grid array `A` and print it. |
!>
!> Parameters are:
!>
!> | Argument | Meaning |
!> |:---------|:--------|
!> | `AOUT` | Double-precision element array returned by the routine, or input element array when `KON=2`. |
!> | `KON` | Control parameter selecting read/convert/print behaviour. |
!> | `INF` | Input file unit for read modes. |
!> | `IOF` | Output file unit used when printing the grid and link/bank values. |
!>
!> For read modes, the grid-to-element mapping is
!>
!> \[
!> AOUT_{ICMXY(i,j)} = A_{i,j}
!> \quad\text{for each active grid cell } ICMXY(i,j)\ne0.
!> \]
!>
!> For `KON=2`, the reverse reporting grid is assembled from grid elements using
!>
!> \[
!> A_{ICMREF(iel,2),ICMREF(iel,3)} = AOUT_{iel}
!> \quad\text{where } ICMREF(iel,1)=0.
!> \]
!>
!> Printed output also includes link values and their associated bank-element
!> values through `ICMBK`.
!>
!> @note Only `KON=0` and `KON=1` read from `INF`; any other value uses the
!> convert-and-print path. The all-zero print shortcut tests the element array
!> `AOUT(1:total_no_elements)`, then printed output includes the grid plus
!> link/bank values.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-09-28 | RAH | 3.4.1 | Added explicit `IMPLICIT` statement in the original source. |
!> | 1997-08-04 | RAH | 4.1 | Added explicit typing; corrected `TITLE` from implicit double precision. |
!> @endhistory
SUBROUTINE AREADR (AOUT, KON, INF, IOF)
INTEGER, INTENT(IN)              :: KON        !! Control parameter selecting read/convert/print behaviour.
INTEGER, INTENT(IN)              :: INF        !! Input file unit for read modes.
INTEGER, INTENT(IN)              :: IOF        !! Output file unit used when printing the grid and link/bank values.
DOUBLEPRECISION, INTENT(INOUT)   :: AOUT(NELEE) !! Double-precision element array; input when `KON` is not 0 or 1.
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
!> Returns a pseudo-random number from the legacy `ran2` generator.
!>
!> The generator updates `idum` in place and returns a uniform variate in
!> `(0,1)`. This is the combined multiplicative generator used in legacy
!> Numerical Recipes code, retained for reproducibility of existing workflows.
!>
!> Passing `idum <= 0` reinitialises the saved shuffle table and secondary seed.
!> Subsequent calls use saved module-local generator state, so independent random
!> streams require explicit reseeding and are not thread-independent.
 FUNCTION ran2(idum)
 INTEGER, PARAMETER     :: IM1=2147483563,IM2=2147483399,IMM1=IM1-1, &
                           IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791, &
                           NTAB=32,NDIV=1+IMM1/NTAB
 INTEGER, INTENT(INOUT) :: idum !! Seed/state value; `idum <= 0` reinitialises the saved stream.
 INTEGER                :: j, k
 INTEGER, SAVE          :: IDUM2=123456789, iy=0, iv(NTAB)=0
 REAL                   :: ran2 !! Uniform variate in `(0,1)`.
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
