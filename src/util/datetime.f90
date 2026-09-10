!> summary: Conversion between calendar dates and the model's absolute hour count.
!> author: J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> The model keeps time as a count of hours from an origin in 1950;
!> [[hour_from_date]] converts a calendar date and time into that count and
!> [[date_from_hour]] converts back. The three leap-year and month-length
!> helpers are private to this module.
!>
!> An invalid date is fatal: [[hour_from_date]] reports it and stops, because
!> continuing would silently place the whole run at the wrong time.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of utilsmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE datetime

   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERR_STOP
   USE file_units, ONLY: FID_logfile
   USE linear_algebra, ONLY: msg

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: hour_from_date, date_from_hour

CONTAINS

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
      r = DBLE(d*24 + khour) + DBLE(kmin)/6.0D1

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

      r = (y - 1950)*365

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
    WRITE (MSG, *) 'Date problem, probably with rainfall or evaporation - are their start dates specified correctly in their files?'
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
      rmins = 60.0D0*(h - DBLE(hours))
      mins = INT(rmins)
      sec = INT(60.0D0*(rmins - DBLE(mins)))
      days = hours/24
      year = 1950 + days/366  ! note, 366 is correct (to underpredict)

      DO WHILE (days > DAYS_IN_YEARS_SINCE_1950(year + 1))
         year = year + 1
      END DO

      mthdays = days - DAYS_IN_YEARS_SINCE_1950(year)
      month = 1 + mthdays/32 ! note, 32 is correct (to underpredict)

      IF (month < 12) THEN       ! avoid month+1=13 in test (dont combine tests)
         IF (mthdays > DAYS_TO_START_MONTH(month + 1, year)) month = month + 1
      END IF

      r(1) = year
      r(2) = month
      r(3) = mthdays - DAYS_TO_START_MONTH(month, year) ! days
      r(4) = hours - 24*days                          ! hours
      r(5) = mins                                       ! minutes
      r(6) = sec

      IF (r(3) == 0) THEN
         PRINT *, ' date trap -DAY'
         CALL ERR_STOP(255)
      END IF

   END FUNCTION date_from_hour

END MODULE datetime

