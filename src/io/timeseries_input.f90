!> summary: The breakpoint readers for meteorological and boundary time series.
!> author: J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> [[FINPUT]] and [[HINPUT]] advance a breakpoint time series to the current
!> model time, reading further records as needed. They return `marker999` when
!> the series is exhausted, which is the end-of-input sentinel the callers test
!> for; a genuine read error is distinguished from an expected end of file and
!> reported.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of utilsmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE timeseries_input

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, marker999
   USE datetime, ONLY: hour_from_date
   USE error_status, ONLY: errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: FINPUT, HINPUT

CONTAINS

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
      CHARACTER(LEN=LENGTH_LINE)      :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER     :: location = 'utilsmod:FINPUT' !! Location string for read-error reports.

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
      ARRAY(1:NINP) = (INTIME - SIMNOW)*FNEXT(1:NINP)

      ! READ DATA AND ADD INTO TOTALS UNTIL END OF SIMULATION TIMESTEP
      ! Replaced the GOTO 20 loop with a modern DO block
      read_loop: DO

         ! 1. Replaced implied DO loops with slicing and END=9999 with IOSTAT
         READ (IIN, *, IOSTAT=read_stat, IOMSG=emsg) TIME(1:5), FNEXT(1:NINP)

         ! FATAL ERROR - END OF FILE REACHED - SET INTIME TO INDICATE ERROR
         IF (read_stat < 0) THEN
            INTIME = MARKER999
            RETURN
         END IF

         ! a positive status is a genuine read error, not an expected end of file
         CALL errstat_read(read_stat, location, emsg)

         INLAST = INTIME
         INTIME = HOUR_FROM_DATE(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH

         IF (INTIME < SIMEND) THEN
            ! Replaced DO 30 loop with array slicing
            ARRAY(1:NINP) = ARRAY(1:NINP) + ((INTIME - INLAST)*FNEXT(1:NINP))
            ! Naturally cycles to the top of read_loop instead of GOTO 20
         ELSE
            ! Replaced DO 40 loop with array slicing
            ARRAY(1:NINP) = ARRAY(1:NINP) + ((SIMEND - INLAST)*FNEXT(1:NINP))
            EXIT read_loop
         END IF

      END DO read_loop

      ! CALCULATE AVERAGE OVER SIMULATION TIMESTEP
      ! Replaced DO 50 loop with array slicing
      ARRAY(1:NINP) = ARRAY(1:NINP)/SIMSTP

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
   SUBROUTINE HINPUT(IIN, TIH, SIMNOW, SIMSTP, INLAST, INTIME, HLAST, HNEXT, NINP, ARRAY)
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
      DOUBLE PRECISION, INTENT(INOUT) :: HLAST(NINP) !! Head vector read at `INLAST`.
      DOUBLE PRECISION, INTENT(INOUT) :: HNEXT(NINP) !! Head vector read at `INTIME`; overwritten by new records.
      DOUBLE PRECISION, INTENT(OUT)   :: ARRAY(NINP) !! Head vector interpolated to the timestep midpoint.

      ! Locals
      INTEGER          :: TIME(5), ios
      DOUBLE PRECISION :: SIMEND, SIMMID
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'utilsmod:HINPUT' !! Location string for read-error reports.

      !----------------------------------------------------------------------

      SIMEND = SIMNOW + SIMSTP
      SIMMID = SIMNOW + 0.5D0*SIMSTP

      time_loop: DO

         ! IF MID-POINT OF TIMESTEP PASSED, INTERPOLATE DATA
         IF (INTIME >= SIMMID .AND. INLAST < SIMMID) THEN
            ! Replaced DO loop 20 with native array slice assignment
            ARRAY(1:NINP) = HLAST(1:NINP) + (HNEXT(1:NINP) - HLAST(1:NINP))* &
                            ((SIMMID - INLAST)/(INTIME - INLAST))
         END IF

         ! READ DATA UNTIL END OF SIMULATION TIMESTEP
         IF (INTIME < SIMEND) THEN

            ! Replaced DO loop 30 with native array slice assignment
            HLAST(1:NINP) = HNEXT(1:NINP)

            ! Read using IOSTAT to gracefully catch End-of-File
            READ (IIN, *, IOSTAT=ios, IOMSG=emsg) TIME(1:5), HNEXT(1:NINP)

            IF (ios < 0) THEN
               ! End of file reached
               INTIME = marker999
               EXIT time_loop
            END IF

            ! a positive status is a genuine read error, not an expected end of file
            CALL errstat_read(ios, location, emsg)

            INLAST = INTIME
            INTIME = HOUR_FROM_DATE(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH

         ELSE
            ! INTIME >= SIMEND, loop termination condition met natively
            EXIT time_loop
         END IF

      END DO time_loop

   END SUBROUTINE HINPUT

END MODULE timeseries_input

