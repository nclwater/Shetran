!> summary: General numerical, date/time, and input helper routines.
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
!> | 2026-03-19 | SB | 4.6 | [[hour_from_date]] gained an informative error message for invalid dates (previously a bare `print*,' date trap'`). |
!> | 2026-04-01 | SB | | Removed two long-dead commented-out routines, `get_start_end_impact` and `open_file`, that were unused and unreachable. |
!> | 2026-04-03 | SvB | | Modernised [[finput]], [[lubksb]], [[ludcmp]], [[areadi]], and [[areadr]] to free-form style: `IMPLICIT NONE`, explicit `INTENT`, array-slice assignment, `DOT_PRODUCT`, and named `DO`/`IF` blocks in place of `GOTO`s and labelled loops (partly AI-assisted). |
!> | 2026-04-04 | SvB | | Auto-formatted all source files for consistent indentation and line length. |
!> | 2026-04-06 | SvB | | Removed the remaining `GOTO`s from [[hinput]]; made [[tridag]] `PURE` and changed its array arguments from assumed-shape to explicit-shape to guarantee no copy-in/copy-out overhead. |
!> | 2026-04-13 | SvB | | Removed the remaining labelled `DO` loops and modernised [[invertmat]]; made [[dcopy]], the date/leap-year helper functions, [[jematmul_mm]], [[jematmul_vm]], [[terpo1]], [[invertmat]], [[lubksb]], and [[ludcmp]] `PURE`; fixed [[dcopy]]'s `n<-0` typo to `n<=0` and its `dy` argument's intent from `OUT` to `INOUT`. |
!> | 2026-05-10 | SvB | | Replaced the interactive pause-and-stop in [[hour_from_date]] with `ERROR STOP`, so an invalid date halts non-interactively. |
!> @endhistory
MODULE utilsmod
   USE SGLOBAL
   USE mod_error, ONLY : RAISE_ERROR, ERRLVL_fatal, FID_logfile, ERR_STOP
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
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-13 | SvB | Corrected the `n<-0` typo to `n<=0` (behaviour-preserving for `n=0`, since both the array-slice and strided branches already reduce to zero-trip no-ops); changed `dy`'s intent from `OUT` to `INOUT`, since an `OUT` array can be copied back from an uninitialised compiler temporary and overwrite elements skipped by a non-unit stride. |
   !> @endhistory
   PURE SUBROUTINE dcopy(n, dx, incx, dy, incy)
   !----------------------------------------------------------------------*
   !     copies vector x to vector y
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: n    !! Number of values to copy.
      INTEGER, INTENT(IN) :: incx !! Increment between values in `dx`.
      INTEGER, INTENT(IN) :: incy !! Increment between values in `dy`.
      DOUBLE PRECISION, DIMENSION(*), INTENT(IN) :: dx !! Source vector.

      ! Input/Output arguments
      ! Modernization Fix: MUST be INOUT. If incy > 1, an OUT declaration
      ! would destroy the interleaved elements that are skipped by the stride!
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: dy !! Destination vector.

      ! Locals
      INTEGER :: i, ix, iy

   !----------------------------------------------------------------------*

      ! Modernization Fix: Corrected strange 'n<-0' syntax to standard <= 0
      IF (n <= 0) THEN
         RETURN
      ELSE IF (incx == 1 .AND. incy == 1) THEN
         dy(1:n) = dx(1:n)
      ELSE
         ix = 1
         iy = 1
         IF (incx < 0) ix = (-n + 1) * incx + 1
         IF (incy < 0) iy = (-n + 1) * incy + 1

         DO i = 1, n
            dy(iy) = dx(ix)
            ix     = ix + incx
            iy     = iy + incy
         END DO
      END IF

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
   !> complete timestep average can be formed, `INTIME` is set to `MARKER999`.
   !>
   !> @note A newly read record value is applied over `(INLAST, INTIME]`, where
   !> `INTIME` is the record time just read. The caller is expected to maintain
   !> `FNEXT`, `INLAST`, and `INTIME` between calls.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-03 | SvB | Replaced the `GOTO`-driven read loop and implied-`DO` slicing with a named `DO`/`EXIT` loop, `IOSTAT`-based end-of-file detection, and array-slice assignment. |
   !> @endhistory
   SUBROUTINE FINPUT(IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, &
                     FNEXT, NINP, ARRAY)
   !----------------------------------------------------------------------
   !
   ! GENERAL SUBROUTINE TO READ IN BREAKPOINT TIME-SERIES OF FLUX DATA.
   ! DATA ARE AVERAGED OVER A SIMULATION TIMESTEP.
   !
   !----------------------------------------------------------------------
      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)             :: IIN    !! File unit number for reading data.
      INTEGER, INTENT(IN)             :: NINP   !! Number of flux items to read from each record.
      DOUBLE PRECISION, INTENT(IN)    :: TIH    !! Simulation start time since the reference date, in hours.
      DOUBLE PRECISION, INTENT(IN)    :: SIMNOW !! Start time of the current simulation timestep, in model hours.
      DOUBLE PRECISION, INTENT(IN)    :: SIMSTP !! Current simulation timestep length, in hours.
      DOUBLE PRECISION, INTENT(INOUT) :: INLAST !! Last breakpoint time read, relative to `TIH`.
      DOUBLE PRECISION, INTENT(INOUT) :: INTIME !! Current breakpoint time up to which `FNEXT` is valid.
      DOUBLE PRECISION, INTENT(INOUT) :: FNEXT(NINP) !! Flux vector valid up to `INTIME`; overwritten by new records.
      DOUBLE PRECISION, INTENT(OUT)   :: ARRAY(NINP) !! Timestep-average flux vector.

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
            INTIME = MARKER999
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
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-06 | SvB | Replaced the `GOTO`-driven read loop with a named `DO`/`EXIT` loop and `IOSTAT`-based end-of-file detection, and the implied-`DO` interpolation loop with array-slice assignment. |
   !> @endhistory
   SUBROUTINE HINPUT (IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, HLAST, HNEXT, NINP, ARRAY)
   !----------------------------------------------------------------------
   !
   ! GENERAL SUBROUTINE TO READ IN BREAKPOINT TIME-SERIES OF HEAD DATA.
   ! HEAD DATA ARE INTERPOLATED ONTO THE MID-POINT OF THE SIMULATION TIMESTEP
   !
   !----------------------------------------------------------------------

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)             :: IIN    !! File unit number for reading data.
      INTEGER, INTENT(IN)             :: NINP   !! Number of head values to read from each record.
      DOUBLE PRECISION, INTENT(IN)    :: TIH    !! Simulation start time since the reference date, in hours.
      DOUBLE PRECISION, INTENT(IN)    :: SIMNOW !! Start time of the current simulation timestep, in model hours.
      DOUBLE PRECISION, INTENT(IN)    :: SIMSTP !! Current simulation timestep length, in hours.
      DOUBLE PRECISION, INTENT(INOUT) :: INLAST !! Previous breakpoint time, relative to `TIH`.
      DOUBLE PRECISION, INTENT(INOUT) :: INTIME !! Next breakpoint time, relative to `TIH`.
      DOUBLE PRECISION, INTENT(INOUT) :: HLAST (NINP) !! Head vector read at `INLAST`.
      DOUBLE PRECISION, INTENT(INOUT) :: HNEXT (NINP) !! Head vector read at `INTIME`; overwritten by new records.
      DOUBLE PRECISION, INTENT(OUT)   :: ARRAY (NINP) !! Head vector interpolated to the timestep midpoint.

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



   !> Converts a calendar date/time to simulation hours since 1950-01-01 00:00.
   !>
   !> Leap years are accounted for. The function checks the round trip through
   !> `DATE_FROM_HOUR` and halts with a diagnostic if the supplied date is invalid.
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
   !> | 2026-03-19 | SB | 4.6 | Replaced the bare `print*,' date trap'` with a message reporting the offending year/month/day/hour/minute values. |
   !> | 2026-05-10 | SvB | | Replaced the interactive pause-and-`STOP` with `ERROR STOP`, so an invalid date halts non-interactively instead of waiting for console input. |
   !> @endhistory
   FUNCTION hour_from_date(kyear, kmth, kday, khour, kmin) RESULT(r)
   !----------------------------------------------------------------------*
   !  THIS FUNCTION CALCULATES HOURS SINCE 1.JANUARY YEAR 1950 AT 0 HOUR
   !  LEAP YEARS ARE TAKEN INTO ACCOUNT
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: kyear !! Calendar year.
      INTEGER, INTENT(IN) :: kmth  !! Calendar month number.
      INTEGER, INTENT(IN) :: kday  !! Calendar day of month.
      INTEGER, INTENT(IN) :: khour !! Hour of day.
      INTEGER, INTENT(IN) :: kmin  !! Minute of hour.

      ! Return variable
      DOUBLE PRECISION :: r !! Model hour count under the SHETRAN date convention.

      ! Locals
      INTEGER :: d        !! One-based day count used by the implemented model-hour convention.
      INTEGER :: check(6) !! Date returned by the round-trip validity check.

   !----------------------------------------------------------------------*

      d = DAYS_IN_YEARS_SINCE_1950(kyear) + DAYS_TO_START_MONTH(kmth, kyear) + kday
      r = DBLE(d * 24 + khour) + DBLE(kmin) / 6.0D1

      ! Modernization Fix: Added D0 suffix to prevent single-precision truncation
      r = r + 0.0000028D0  ! add 1/100 of a second to sort out round error with mins

      check = DATE_FROM_HOUR(r)

      IF (check(1) /= kyear .OR. check(2) /= kmth .OR. check(3) /= kday .OR. &
          check(4) /= khour .OR. check(5) /= kmin) THEN

         WRITE (*, '(A)') ' There is a problem with a date that has been entered'
         WRITE (*, '(A,5(1x,I0))') 'The Year, month,day,hour,minute values entered are: ', kyear, kmth, kday, khour, kmin
         CALL ERR_STOP(255)

      END IF

   END FUNCTION hour_from_date



   !> Returns the number of days in complete years since 1950-01-01.
   !>
   !> Leap days are counted by iterating over candidate leap years from 1952 up to
   !> `y-1`, using [[is_leap]] for the Gregorian leap-year rule.
   PURE FUNCTION days_in_years_since_1950(y) RESULT(r)
   !----------------------------------------------------------------------*
   ! Calculates the total days in whole years elapsed since 1950.
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: y !! Year at the end of the counted interval.

      ! Return variable
      INTEGER :: r !! Days in complete years from 1950-01-01 to year `y`.

      ! Locals
      INTEGER :: i !! Candidate leap year.

   !----------------------------------------------------------------------*

      r = (y - 1950) * 365

      ! Loop steps by 4 (starting from the first leap year after 1950)
      leap_loop: DO i = 1952, y - 1, 4
         IF (IS_LEAP(i)) r = r + 1
      END DO leap_loop

   END FUNCTION days_in_years_since_1950



   !> Returns whether a year is a leap year in the Gregorian calendar.
   !>
   !> A year will be a leap year if it is divisible by 4 but not by 100.
   !> If a year is divisible by 4 and by 100, it is not a leap year unless
   !> it is also divisible by 400.
   PURE FUNCTION is_leap(y) RESULT(r)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: y !! Calendar year to test.
      LOGICAL :: r             !! True when `y` is a Gregorian leap year.

      IF (MOD(y, 4) == 0) THEN
         IF (MOD(y, 100) == 0) THEN
            r = (MOD(y, 400) == 0)
         ELSE
            r = .TRUE.
         END IF
      ELSE
         r = .FALSE.
      END IF

   END FUNCTION is_leap



   !> Returns the day offset to the start of a month in a given year.
   !>
   !> Month offsets are zero-based (`January -> 0`). Leap years add one day for
   !> months after February. The routine traps `m < 1` through `ERROR`, but it does
   !> not explicitly guard `m > 12` before indexing the month table.
   FUNCTION days_to_start_month(m, y) RESULT(r)
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: m !! Calendar month number.
      INTEGER, INTENT(IN) :: y !! Calendar year used for leap-day adjustment.
      INTEGER :: r             !! Day offset to the start of month `m`.

      INTEGER, PARAMETER :: sd(12) = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334] !! Non-leap offsets.

      IF (m < 1) THEN
         WRITE(MSG, *) 'Date problem, probably with rainfall or evaporation - are their start dates specified correctly in their files?'
         CALL RAISE_ERROR(ERRLVL_fatal, 4820, FID_logfile, 0, 0, MSG)
      END IF

      r = sd(m)
      IF (IS_LEAP(y) .AND. m > 2) r = r + 1

   END FUNCTION days_to_start_month



   !> Converts the model hour count used by [[hour_from_date]] to date components.
   !>
   !> The result array is `[year, month, day, hour, minute, second]`. The
   !> conversion uses deliberately low initial estimates (`days/366` for the year
   !> and `mthdays/32` for the month), then increments to the correct year/month.
   !> A day value of zero triggers a stop as a date-trapping guard.
   FUNCTION date_from_hour(h) RESULT(r)
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN) :: h !! Model hour count under the SHETRAN date convention.
      INTEGER :: r(6) ! year, month, day, hour, min, sec

      INTEGER :: hours, days, year, month, mthdays, mins, sec
      DOUBLE PRECISION :: rmins

      hours = INT(h)
      rmins = 60.0D0 * (h - DBLE(hours))
      mins  = INT(rmins)
      sec   = INT(60.0D0 * (rmins - DBLE(mins)))
      days  = hours / 24
      year  = 1950 + days / 366  ! note, 366 is correct (to underpredict)

      DO WHILE(days > DAYS_IN_YEARS_SINCE_1950(year + 1))
         year = year + 1
      END DO

      mthdays = days - DAYS_IN_YEARS_SINCE_1950(year)
      month   = 1 + mthdays / 32 ! note, 32 is correct (to underpredict)

      IF (month < 12) THEN       ! avoid month+1=13 in test (dont combine tests)
         IF (mthdays > DAYS_TO_START_MONTH(month + 1, year)) month = month + 1
      END IF

      r(1) = year
      r(2) = month
      r(3) = mthdays - DAYS_TO_START_MONTH(month, year) ! days
      r(4) = hours - 24 * days                          ! hours
      r(5) = mins                                       ! minutes
      r(6) = sec

      IF (r(3) == 0) THEN
         PRINT *, ' date trap -DAY'
         CALL ERR_STOP(255)
      END IF

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
   !>
   !> @note The local `ZERO` parameter shadows the identical module-wide `ZERO`
   !> constant brought in via `USE SGLOBAL`; the added declaration is redundant
   !> (both equal `0.0D0`) but harmless. [[jematmul_vm]] below still relies on
   !> the module-wide constant directly.
   !> @endnote
   PURE FUNCTION jematmul_mm(b, c, n1, n2, n3) RESULT(a)
   !----------------------------------------------------------------------*
   ! A = B * C  (Note: Indexing implies A(i,j) = sum(B(k,j)*C(i,k)))
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n1 !! Number of columns in the returned matrix.
      INTEGER, INTENT(IN) :: n2 !! Shared inner dimension.
      INTEGER, INTENT(IN) :: n3 !! Number of rows in the returned matrix.
      DOUBLE PRECISION, INTENT(IN) :: b(n2, n1) !! Right-hand matrix in declared storage.
      DOUBLE PRECISION, INTENT(IN) :: c(n3, n2) !! Left-hand matrix in declared storage.
      DOUBLE PRECISION :: a(n3, n1)              !! Matrix product `C * B`.

      INTEGER :: i, j, k

      ! Modernization Fix: ZERO was undeclared
      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0

      DO i = 1, n3
         DO j = 1, n1
            a(i, j) = ZERO
            DO k = 1, n2
               a(i, j) = a(i, j) + b(k, j) * c(i, k)
            END DO
         END DO
      END DO

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
   PURE FUNCTION jematmul_vm(b, c, n1, n2) RESULT(a)
   !----------------------------------------------------------------------*
   ! A = B * C
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n1 !! Length of the returned vector.
      INTEGER, INTENT(IN) :: n2 !! Shared inner dimension.
      DOUBLE PRECISION, INTENT(IN) :: b(n2, n1) !! Matrix stored transposed relative to conventional notation.
      DOUBLE PRECISION, INTENT(IN) :: c(n2)     !! Input vector.
      DOUBLE PRECISION :: a(n1)                  !! Matrix-vector product.

      INTEGER :: i, k

      DO i = 1, n1
         a(i) = ZERO
         DO k = 1, n2
            a(i) = a(i) + b(k, i) * c(k)
         END DO
      END DO

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
   !> | 2026-04-13 | SvB | | Made the routine `PURE`; gave `NCT` and `YCURR` explicit `INOUT` intent (they were previously declared with no intent attribute, an implicit F77-style dummy) to formalise that both are read and updated. |
   !> @endhistory
   PURE SUBROUTINE TERPO1(YCURR, TCURR, YTAB, TTAB, NCT, YINIT, NPAR, I)
   !----------------------------------------------------------------------*
   !
   !     SERVICE SUBROUTINE TO INTERPOLATE VALUES FOR ONE-DIMENSIONAL
   !                   TIME-VARYING PARAMETERS
   !
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NPAR !! Size of the parameter array.
      INTEGER, INTENT(IN) :: I    !! Parameter-array position being updated.
      DOUBLE PRECISION, INTENT(IN) :: TCURR         !! Current simulation time, in hours.
      DOUBLE PRECISION, INTENT(IN) :: YTAB(NPAR, *)  !! Tabulated relative values of the parameter.
      DOUBLE PRECISION, INTENT(IN) :: TTAB(NPAR, *)  !! Tabulated times, in days.
      DOUBLE PRECISION, INTENT(IN) :: YINIT(NPAR)    !! Initial or reference parameter values.

      ! Input/Output arguments
      ! Modernization Fix: MUST be INOUT to preserve array elements other than index 'I'
      INTEGER, INTENT(INOUT) :: NCT(NPAR)          !! Current table-position counter for each parameter.
      DOUBLE PRECISION, INTENT(INOUT) :: YCURR(NPAR) !! Current parameter array to update.

      ! Locals, etc
      INTEGER :: ITERP, NCTERP
      DOUBLE PRECISION :: DIFFA, DIFFB, DIFFC, YREL

   !----------------------------------------------------------------------*

      NCTERP = NCT(I)

      ! Calculate interval jump (time is in hours, TTAB is in days)
      ITERP = INT((TCURR / 24.0D0 - TTAB(I, NCTERP)) / &
                  (TTAB(I, NCTERP + 1) - TTAB(I, NCTERP)))
      NCTERP = NCTERP + ITERP

      ! Interpolate
      DIFFA = YTAB(I, NCTERP + 1) - YTAB(I, NCTERP)
      DIFFB = (TTAB(I, NCTERP + 1) - TTAB(I, NCTERP)) * 24.0D0
      DIFFC = TCURR - TTAB(I, NCTERP) * 24.0D0

      YREL = YTAB(I, NCTERP) + DIFFC * DIFFA / DIFFB
      YCURR(I) = YREL * YINIT(I)

      NCT(I) = NCTERP

   END SUBROUTINE TERPO1



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
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-06 | SvB | Made the routine `PURE`. |
   !> | 2026-04-06 | SvB | Changed `A`, `B`, `C`, `R`, and `U` from assumed-shape (`(:)`) to explicit-shape (`(N)`) arguments, guaranteeing no copy-in/copy-out overhead for non-contiguous actual arguments. |
   !> @endhistory
   PURE SUBROUTINE TRIDAG (A, B, C, R, U, N)
   !----------------------------------------------------------------------*
   !                            SOLVES FOR VECTOR U OF LENGTH N
   !                            THE TRIDIAGONAL SET A,B,C WHERE
   !                            R IS THE R.H.S.
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN)             :: N !! Number of equations.
      ! Explicit-shape arrays guarantee zero copy-in/copy-out overhead
      DOUBLE PRECISION, INTENT(IN)    :: A(N) !! Lower diagonal; `A(1)` is not used.
      DOUBLE PRECISION, INTENT(IN)    :: B(N) !! Main diagonal.
      DOUBLE PRECISION, INTENT(IN)    :: C(N) !! Upper diagonal; `C(N)` is not used.
      DOUBLE PRECISION, INTENT(IN)    :: R(N) !! Right-hand-side vector.
      DOUBLE PRECISION, INTENT(INOUT) :: U(N) !! Solution vector.

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
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-13 | SvB | Made the routine `PURE`; removed the local `ret` flag, which was set on the `N=1` branch but never read anywhere (the original `N<1`/`N=1`/else structure already dispatched correctly without it). |
   !> @endhistory
   PURE SUBROUTINE invertmat(a, n, icod)
   !----------------------------------------------------------------------*
   ! Inverts a square matrix 'a' of size 'n' using LU decomposition.
   ! Returns icod = 0 (success) or icod = 1 (singular/failure).
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: n !! Matrix order.

      ! Output arguments
      INTEGER, INTENT(OUT) :: icod !! Status code: `0` success, `1` failure.

      ! Input/Output arguments
      DOUBLE PRECISION, DIMENSION(n,n), INTENT(INOUT) :: a !! Matrix to replace with its inverse.

      ! Locals
      INTEGER :: i, j
      INTEGER :: indx(n)
      DOUBLE PRECISION, DIMENSION(n,n) :: y
      DOUBLE PRECISION :: d
      LOGICAL :: issing

   !----------------------------------------------------------------------*

      icod = 0

      IF (n < 1) THEN
         icod = 1

      ELSE IF (n == 1) THEN
         IF (ABS(a(1,1)) <= EPS) THEN
            icod = 1
         ELSE
            a(1,1) = ONE / a(1,1)
         END IF

      ELSE
         ! Initialize 'y' as the identity matrix
         y = ZERO
         DO i = 1, n
            y(i,i) = ONE
         END DO

         ! Perform LU Decomposition
         CALL LUDCMP(a, n, indx, d, issing)

         IF (issing) THEN
            icod = 1
         ELSE
            ! Back-substitute against each column of the identity matrix
            DO j = 1, n
               CALL LUBKSB(a, n, indx, y(:, j))
            END DO

            ! The array 'y' now contains the inverse; copy it back to 'a'
            a = y
         END IF

      END IF

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
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-03 | SvB | Replaced the labelled inner-product loops with `DOT_PRODUCT` over array sections. |
   !> @endhistory
   PURE SUBROUTINE lubksb(a, n, indx, b)
   !----------------------------------------------------------------------*
   ! Solves the linear system A*x = b using LU Decomposition.
   ! 'a' is the LU-decomposed matrix output from 'ludcmp'.
   ! 'indx' is the row permutation vector output from 'ludcmp'.
   ! 'b' is the right-hand side vector on input, and contains the
   !     solution vector 'x' on output.
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)             :: n       !! Matrix order.
      INTEGER, INTENT(IN)             :: indx(n) !! Pivot-row indices from `ludcmp`.
      DOUBLE PRECISION, INTENT(IN)    :: a(n,n)  !! Combined LU factors from `ludcmp`.
      DOUBLE PRECISION, INTENT(INOUT) :: b(n)    !! Right-hand side on entry; solution on exit.

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
         ELSE IF (NOTZERO(asum)) THEN
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
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:-----|:-------|:------------|
   !> | 2026-04-03 | SvB | Replaced the labelled inner-product loops with `DOT_PRODUCT` over array sections, `MAXVAL` for the row-scaling search, and whole-row array slices for pivot swapping. |
   !> @endhistory
   PURE SUBROUTINE ludcmp(a, n, indx, d, issing)
   !----------------------------------------------------------------------*
   ! Performs LU Decomposition on matrix 'a' using partial pivoting.
   ! 'a' is replaced by its LU decomposition.
   ! 'indx' records the row permutations.
   ! 'd' outputs +1 or -1 depending on whether row swaps were even or odd.
   ! 'issing' is flagged .TRUE. if the matrix is singular.
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)             :: n       !! Matrix order.
      DOUBLE PRECISION, INTENT(INOUT) :: a(n,n)  !! Matrix overwritten by combined LU factors.
      INTEGER, INTENT(OUT)            :: indx(n) !! Pivot-row index for each column.
      DOUBLE PRECISION, INTENT(OUT)   :: d       !! Pivot-parity factor.
      LOGICAL, INTENT(OUT)            :: issing  !! True if a zero scaling row marks the matrix singular.

      ! Local Variables
      INTEGER                         :: i, imax, j
      DOUBLE PRECISION                :: aamax, dum, vv(n), dum_row(n)
      DOUBLE PRECISION, PARAMETER     :: TINY = 1.0D-20

   !----------------------------------------------------------------------*

      issing = .FALSE.
      d = 1.0D0

      ! 1. Calculate implicit scaling information for each row
      DO i = 1, n
         aamax = MAXVAL(ABS(a(i, :)))

         IF (ISZERO(aamax)) THEN
            issing = .TRUE.
            RETURN ! Singular matrix, exit immediately
         END IF

         vv(i) = 1.0D0 / aamax
      END DO

      ! Crout's Algorithm
      outer_col_loop: DO j = 1, n

         ! 2. Upper triangular part
         upper_loop: DO i = 1, j - 1
            a(i, j) = a(i, j) - DOT_PRODUCT(a(i, 1:i-1), a(1:i-1, j))
         END DO upper_loop

         aamax = 0.0D0
         imax = j

         ! 3. Lower triangular part and pivot search
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
            ! 4. Whole-array row slices for rapid memory swapping
            dum_row(:) = a(imax, :)
            a(imax, :) = a(j, :)
            a(j, :)    = dum_row(:)

            d = -d
            vv(imax) = vv(j)
         END IF

         indx(j) = imax

         IF (ISZERO(a(j, j))) a(j, j) = TINY

         ! 5. Direct column scaling
         IF (j /= n) THEN
            dum = 1.0D0 / a(j, j)
            a(j+1:n, j) = a(j+1:n, j) * dum
         END IF

      END DO outer_col_loop

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
   !> | 2026-04-03 | SvB | | Replaced numbered-`FORMAT`/labelled-`DO` I/O with named `DO` loops, inline `FORMAT` strings, and array-slice reads/assignments. |
   !> @endhistory
   SUBROUTINE AREADI (IAOUT, KON, INF, IOF, INUM)
!----------------------------------------------------------------------*
!
!      SERVICE SUBROUTINE TO READ AND PRINT AN INTEGER ARRAY
!
!----------------------------------------------------------------------*
      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: KON  !! Control parameter selecting read/convert/print/default-fill behaviour.
      INTEGER, INTENT(IN)  :: INF  !! Input file unit for read modes; default integer value when `KON=3`.
      INTEGER, INTENT(IN)  :: IOF  !! Output file unit used when printing the grid array.
      INTEGER, INTENT(IN)  :: INUM !! Expected range/count of integer codes; zero selects old `20I4` input.
      INTEGER, INTENT(OUT) :: IAOUT(:) !! Integer element array; also input when converting elements back to grid.
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
         CALL ERR_STOP(255)
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
                  CALL ERR_STOP(255)
               END IF
            ELSE
               READ (INF, '(I7)') I2
               IF (I2 /= K) THEN
                  WRITE (IOF, "(/,/,2X, 'ERROR IN DATA ', 20A4, /,/,2X, 'IN THE VICINITY OF LINE K=', I5)") TITLE, I2
                  CALL ERR_STOP(255)
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
   !> @note `KON`, `INF`, and `IOF` are declared with no `INTENT` attribute here,
   !> unlike most other routines in this module. This is retained legacy F77-style
   !> behaviour, not something introduced by the recent modernisation.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-09-28 | RAH | 3.4.1 | Added explicit `IMPLICIT` statement in the original source. |
   !> | 1997-08-04 | RAH | 4.1 | Added explicit typing; corrected `TITLE` from implicit double precision. |
   !> | 2026-04-03 | SvB | | Replaced numbered-`FORMAT`/labelled-`DO` I/O with named `DO` loops, inline `FORMAT` strings, and array-slice reads/assignments. |
   !> @endhistory
   SUBROUTINE AREADR (AOUT, KON, INF, IOF)
!----------------------------------------------------------------------*
!
!      SERVICE SUBROUTINE TO READ AND PRINT A DOUBLEPRECISION,TWO-DIMENSIONAL ARRAY
!      (IN DOUBLEPRECISION)
!
!----------------------------------------------------------------------*
! Commons and constants
      IMPLICIT NONE

! Input arguments
      INTEGER :: KON !! Control parameter selecting read/convert/print behaviour.
      INTEGER :: INF !! Input file unit for read modes.
      INTEGER :: IOF !! Output file unit used when printing the grid and link/bank values.

! In|out arguments
      DOUBLE PRECISION :: AOUT(NELEE) !! Double-precision element array; input when `KON` is not 0 or 1.

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
               CALL ERR_STOP(255)
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



   !> Returns a pseudo-random number from the legacy `ran2` generator.
   !>
   !> Long period (> 2 x 10^18) random number generator of L'Ecuyer with
   !> Bays-Durham shuffle and added safeguards. The generator updates `idum` in
   !> place and returns a uniform variate in `(0,1)`, exclusive. This is the
   !> combined multiplicative generator used in legacy Numerical Recipes code,
   !> retained for reproducibility of existing workflows.
   !>
   !> Passing `idum <= 0` reinitialises the saved shuffle table and secondary seed.
   !> Subsequent calls use saved module-local generator state, so independent random
   !> streams require explicit reseeding and are not thread-independent.
   FUNCTION ran2(idum)
   !----------------------------------------------------------------------*
   ! Call with idum a negative integer to initialize; thereafter, do not
   ! alter idum between successive deviates in a sequence.
   !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Dummy argument MUST be INOUT because the seed updates.
      ! This side-effect strictly prevents the function from being PURE.
      INTEGER, INTENT(INOUT) :: idum !! Seed/state value; `idum <= 0` reinitialises the saved stream.

      ! Return type (Explicitly Single Precision as per standard NR)
      REAL :: ran2 !! Uniform variate in `(0,1)`.

      ! Magic parameters for the dual LCGs and shuffle table
      INTEGER, PARAMETER :: IM1  = 2147483563
      INTEGER, PARAMETER :: IM2  = 2147483399
      INTEGER, PARAMETER :: IMM1 = IM1 - 1
      INTEGER, PARAMETER :: IA1  = 40014
      INTEGER, PARAMETER :: IA2  = 40692
      INTEGER, PARAMETER :: IQ1  = 53668
      INTEGER, PARAMETER :: IQ2  = 52774
      INTEGER, PARAMETER :: IR1  = 12211
      INTEGER, PARAMETER :: IR2  = 3791
      INTEGER, PARAMETER :: NTAB = 32
      INTEGER, PARAMETER :: NDIV = 1 + IMM1 / NTAB

      ! Type-safe real parameters
      REAL, PARAMETER    :: EPS  = 1.2E-7
      REAL, PARAMETER    :: RNMX = 1.0E0 - EPS
      REAL, PARAMETER    :: AM   = 1.0E0 / REAL(IM1)

      ! Saved internal state
      INTEGER, SAVE :: idum2 = 123456789
      INTEGER, SAVE :: iy = 0
      INTEGER, SAVE :: iv(NTAB) = 0

      ! Locals
      INTEGER :: j, k

   !----------------------------------------------------------------------*

      ! Initialization block
      IF (idum <= 0) THEN
         idum = MAX(-idum, 1)
         idum2 = idum

         ! Load the shuffle table (after 8 warm-up passes)
         DO j = NTAB + 8, 1, -1
            k = idum / IQ1
            idum = IA1 * (idum - k * IQ1) - k * IR1
            IF (idum < 0) idum = idum + IM1
            IF (j <= NTAB) iv(j) = idum
         END DO
         iy = iv(1)
      END IF

      ! Start normal generation block
      ! First LCG
      k = idum / IQ1
      idum = IA1 * (idum - k * IQ1) - k * IR1
      IF (idum < 0) idum = idum + IM1

      ! Second LCG
      k = idum2 / IQ2
      idum2 = IA2 * (idum2 - k * IQ2) - k * IR2
      IF (idum2 < 0) idum2 = idum2 + IM2

      ! Bays-Durham shuffle
      j = 1 + iy / NDIV
      iy = iv(j) - idum2
      iv(j) = idum

      IF (iy < 1) iy = iy + IMM1

      ! Return the generated value, preventing exact endpoint bounds
      ran2 = MIN(AM * REAL(iy), RNMX)

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
