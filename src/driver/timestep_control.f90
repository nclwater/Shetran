!> summary: Choosing the length of the next coupled model timestep.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[TMSTEP]] decides how long the next coupled timestep may be. It is bounded
!> by `TMAX`, grown at most by the factor `PALFA`, cut short by the next
!> meteorological input time, and cut short again when the rainfall in the step
!> would exceed `PMAX` or when error reporting has requested a reduction
!> through [[runtime_flags]]. `BSOFT` shortens the first steps of a run.
!>
!> This is the one consumer of the two timestep-reduction request flags, which
!> is why they are in a module of their own rather than with the error
!> reporter that sets them.
!>
!> `PREST` is set to `1+PALFA` during frame initialisation and never read.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP / RAH | 2.0-4.2 | Developed the frame driver, the meteorological reader and the timestep control. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the remaining frame `.F` files to Fortran 90. |
!> | 2015-2026 | SB / SvB | 4.5-4.6 | Added the separate temperature streams, the dated meteorological reader and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of rest, AL_D; see docs/rename/proposal.md. |
!> @endhistory
MODULE timestep_control

   USE element_geometry, ONLY: total_no_elements, total_no_links
   USE simulation_clock, ONLY: NSTEP, TIH, UZNEXT, UZNOW
   USE runtime_flags, ONLY: flag_runtime_reduction_e1060, flag_runtime_reduction_errors
   USE run_control, ONLY: BEXSM, BHOTRD, BSOFT
   USE file_units, ONLY: EPD, FID_logfile, PRD, TAH, TAL
   USE met_forcing, ONLY: DTMET2, DTMET3, ISTA, NM, NRAIN, NRAINC, precip_m_per_s, TA
   USE met_input, ONLY: MELAST, METIME, METIN, pinp
   USE snow_state, ONLY: SD
   USE et_config, ONLY: BMETDATES
   USE datetime, ONLY: hour_from_date
   USE error_reporting, ONLY: RAISE_ERROR, ERR_STOP, ERRLVL_fatal

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: TMSTEP
   PUBLIC :: TMAX, PALFA, PMAX, PREST

   DOUBLEPRECISION :: PMAX    !! Maximum rainfall depth permitted in one model timestep (mm).
   DOUBLEPRECISION :: PALFA   !! Fractional timestep growth factor used by `TMSTEP`.
   DOUBLEPRECISION :: TMAX    !! Maximum/basic coupled model timestep, capped at two hours (h).
   DOUBLEPRECISION :: PREST  !! Unused legacy value set to `1+PALFA` during frame initialization.

CONTAINS

!> Computes the next simulation timestep and reads any required meteorological data.
!>
!> `TMSTEP` is called once per model step. The candidate timestep is limited
!> by soft-start growth, snowmelt conditions, forcing-data record boundaries,
!> the configured maximum timestep, and runtime reductions triggered by
!> selected flow errors; it then advances the meteorological data ([[metin]])
!> needed for the chosen step. This routine is the main point where
!> meteorological file timing and hydrological stability controls meet before
!> the next model step is taken.
!>
!> The candidate timestep is first reduced by these controls:
!>
!> | Control | Code expression | Effect |
!> |:--------|:-----------------|:-------|
!> | Growth from previous step | `UZNEXT*(1+PALFA)` | Prevents abrupt timestep expansion. |
!> | Soft start | `TMAX*0.05*1.03**NSTEP` for the first 102 steps when `BSOFT` is true | Starts the run with smaller steps; disabled for hot starts. |
!> | Snowmelt | `0.5` h when snow is present and any met station has `TA>0` | Limits melt-period steps. |
!> | Runtime errors | `UZNEXT/10` or `UZNEXT/100`, lower-bounded by `0.0003` h | Retries after selected flow errors (`flag_runtime_reduction_errors`/`flag_runtime_reduction_e1060`, cleared after use). |
!>
!> For date-aware forcing (`BMETDATES`) the first call checks that `PRD`,
!> `EPD`, and optional `TAH`/`TAL` records do not start after the simulation
!> start date. It also skips older records until the first record whose date
!> is within about `0.01` h of `TIH` or later, then backspaces so [[metin]]
!> can read that record.
!>
!> Precipitation is accumulated over the candidate timestep by splitting at
!> meteorological record boundaries:
!>
!> \[
!> PTOT_i = \sum_m \Delta t_m\,PINP_{i,m}.
!> \]
!>
!> If any accumulated station total would exceed `PMAX`, the timestep is
!> reduced to the crossing time; if the resulting `UZNEXT` still falls below
!> \(5\times10^{-5}\) h the run stops fatally (`ERROR` code 1025) since this
!> normally indicates a data problem. The final element precipitation rate is
!> then
!>
!> \[
!> precip\_m\_per\_s(e) =
!> \frac{PTOT_{NRAINC(e)}}{UZNEXT\,3.6\times10^6}.
!> \]
!>
!> Finally `METIN(2)` reads or interpolates PE and time-varying
!> vegetation/canopy parameters needed for the timestep.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-07 | GP | 3.4 | Reworked `UZNEXT` algorithm and added soft-start controls. |
!> | 1994-10-03 | RAH | 3.4.1 | Added legacy double-precision typing. |
!> | 1996-07-17 | GP | 4.0 | Limited timestep during snowmelt. |
!> | 1998-10-20 | RAH | 4.2 | Reworked control flow and initialisation. |
!> | 2020-07-07 | SB | - | Added timestep reduction after selected runtime errors. |
!> | 2026-03-19 | SB | 4.6.1 | Added date-aware checks for meteorological forcing files. |
!> | 2026-04-06 | SvB | 4.6.1 | Replaced `GOTO`-driven control flow with named `DO`/`CYCLE`/`EXIT` loop constructs. |
!> @endhistory
   SUBROUTINE TMSTEP
      IMPLICIT NONE

! Locals, etc
      INTEGER             :: I, IEL, IFLAG, IOS
      DOUBLE PRECISION    :: TEND, TSNOW, TSOFT, UZTEST, PTOT(NRAIN)
      LOGICAL             :: EXITT, SMFLAG
      LOGICAL             :: PRDFIRST = .TRUE., PRDFIRST1 = .TRUE.
      LOGICAL             :: EPDFIRST = .TRUE., EPDFIRST1 = .TRUE.
      LOGICAL             :: TAHFIRST = .TRUE., TAHFIRST1 = .TRUE.
      LOGICAL             :: TALFIRST = .TRUE., TALFIRST1 = .TRUE.
      INTEGER             :: prdyear, prdmonth, prdday, prdhour, prdminute
      INTEGER             :: epdyear, epdmonth, epdday, epdhour, epdminute
      INTEGER             :: tahyear, tahmonth, tahday, tahhour, tahminute
      INTEGER             :: talyear, talmonth, talday, talhour, talminute
      DOUBLE PRECISION    :: prddate, epddate, tahdate, taldate
!----------------------------------------------------------------------*

! ----------------------------------------------------------------------
!  1.  COMPUTE EXPECTED TiMeSTEP
! ----------------------------------------------------------------------
      ! CALCULATE REDUCED TIMESTEP FOR SOFTSTART
      TSOFT = TMAX

      ! sb soft start not needed for hot start?
      IF (BHOTRD) BSOFT = .FALSE.

      IF (BSOFT .AND. NSTEP <= 102) TSOFT = TMAX*0.05d0*1.03d0**NSTEP

      ! CALCULATE REDUCED TIMESTEP FOR SNOWMELT
      TSNOW = TMAX
      IF (BEXSM) THEN
         SMFLAG = .FALSE.
         DO I = 1, NM
            IF (TA(I) > 0.0d0) SMFLAG = .TRUE.
         END DO

         IF (SMFLAG) THEN
            snowmelt_check: DO IEL = total_no_links + 1, total_no_elements
               IF (SD(IEL) > 0.0d0) THEN
                  TSNOW = 0.5d0
                  EXIT snowmelt_check
               END IF
            END DO snowmelt_check
         END IF
      END IF

      ! SET TIMESTEP LENGTH
      UZNEXT = MIN(UZNEXT*(1.0d0 + PALFA), TSOFT, TSNOW)

      ! SB 07072020 reduce timestep if there are errors 1024,1030,1060
      IF (flag_runtime_reduction_e1060) THEN
         UZNEXT = MAX(0.0003d0, UZNEXT/10.0d0)
      ELSEIF (flag_runtime_reduction_errors) THEN
         UZNEXT = MAX(0.0003d0, UZNEXT/100.0d0)
      END IF

      flag_runtime_reduction_e1060 = .FALSE.
      flag_runtime_reduction_errors = .FALSE.

! ----------------------------------------------------------------------
!  2.  READ METEOROLOGICAL DATA AND REDUCE TMSTEP IF NECESSARY
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
!  2a.   check the start date is not before any met data occurs
! ----------------------------------------------------------------------
      IF (BMETDATES .AND. PRDFIRST1) THEN
         PRDFIRST1 = .FALSE.
         READ (prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            prdyear, prdmonth, prdday, prdhour, prdminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the precipitation time series file. '// &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            CALL ERR_STOP(255)
         END IF

         BACKSPACE (prd)
         prddate = HOUR_FROM_DATE(prdyear, prdmonth, prdday, prdhour, prdminute)

         ! check simulation start time plus precipitation time step length plus 0.01
         ! is greater than or equal to the first precipitation time series date.
         ! The 0.01 values is a bit arbitrary
         IF (tih + dtmet2 + 0.01d0 < prddate) THEN
            WRITE (*, '(A)') ' The precipitation data starts after the simulation start date. '// &
               'Check the precipitation data dates and the start time of the simulation'
            CALL ERR_STOP(255)
         END IF
      END IF

      IF (BMETDATES .AND. EPDFIRST1) THEN
         EPDFIRST1 = .FALSE.
         READ (epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            epdyear, epdmonth, epdday, epdhour, epdminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the potential evaporation time series file. '// &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            CALL ERR_STOP(255)
         END IF

         BACKSPACE (epd)
         epddate = HOUR_FROM_DATE(epdyear, epdmonth, epdday, epdhour, epdminute)

         IF (tih + dtmet3 + 0.01d0 < epddate) THEN
            WRITE (*, '(A)') ' The potential evaporation data starts after the simulation start date. '// &
               'Check the potential evaporation data dates and the start time of the simulation'
            CALL ERR_STOP(255)
         END IF
      END IF

      IF (BMETDATES .AND. TAHFIRST1 .AND. ISTA) THEN
         TAHFIRST1 = .FALSE.
         READ (tah, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            tahyear, tahmonth, tahday, tahhour, tahminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the maximum temperature time series file. '// &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            CALL ERR_STOP(255)
         END IF

         BACKSPACE (tah)
         tahdate = HOUR_FROM_DATE(tahyear, tahmonth, tahday, tahhour, tahminute)

         IF (tih + dtmet3 + 0.01d0 < tahdate) THEN
            WRITE (*, '(A)') ' The maximum temperature data starts after the simulation start date. '// &
               'Check the maximum temperature dates and the start time of the simulation'
            CALL ERR_STOP(255)
         END IF
      END IF

      IF (BMETDATES .AND. TALFIRST1 .AND. ISTA) THEN
         TALFIRST1 = .FALSE.
         READ (tal, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            talyear, talmonth, talday, talhour, talminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the minimum temperature time series file. '// &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            CALL ERR_STOP(255)
         END IF

         BACKSPACE (tal)
         taldate = HOUR_FROM_DATE(talyear, talmonth, talday, talhour, talminute)

         IF (tih + dtmet3 + 0.01d0 < taldate) THEN
            WRITE (*, '(A)') ' The minimum temperature data starts after the simulation start date. '// &
               'Check the minimum temperature dates and the start time of the simulation'
            CALL ERR_STOP(255)
         END IF
      END IF

! ----------------------------------------------------------------------
!  2b.   If the met data has dates then the first values can be ignored
!        if the simulation start date is after the met data start date
! ----------------------------------------------------------------------
      IF (BMETDATES .AND. PRDFIRST) THEN
         DO
            READ (prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               prdyear, prdmonth, prdday, prdhour, prdminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the precipitation time series file. '// &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
               WRITE (*, '(A)') ' Check the format of the precipitation time series file '// &
                  'and the end date is not before the start date of the simulation'
               CALL ERR_STOP(255)
            END IF

            prddate = HOUR_FROM_DATE(prdyear, prdmonth, prdday, prdhour, prdminute)
            ! use the precipitation at this step if it is within 0.01 hour of the start date.
            ! Otherwise use the next precipitation file. The 0.01 values is a bit arbitrary
            IF (prddate + 0.01d0 > tih) THEN
               PRDFIRST = .FALSE.
               BACKSPACE (prd)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. EPDFIRST) THEN
         DO
            READ (epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               epdyear, epdmonth, epdday, epdhour, epdminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the potential evaporation time series file. '// &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the potential evaporation time series file '// &
                  'and the end date is not before the start date of the simulation'
               CALL ERR_STOP(255)
            END IF

            epddate = HOUR_FROM_DATE(epdyear, epdmonth, epdday, epdhour, epdminute)
            IF (epddate + 0.01d0 > tih) THEN
               EPDFIRST = .FALSE.
               BACKSPACE (epd)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. TAHFIRST .AND. ISTA) THEN
         DO
            READ (tah, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               tahyear, tahmonth, tahday, tahhour, tahminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the maximum temperature time series file. '// &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the maximum daily temperature time series file '// &
                  'and the end date is not before the start date of the simulation'
               CALL ERR_STOP(255)
            END IF

            tahdate = HOUR_FROM_DATE(tahyear, tahmonth, tahday, tahhour, tahminute)
            IF (tahdate + 0.01d0 > tih) THEN
               TAHFIRST = .FALSE.
               BACKSPACE (tah)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. TALFIRST .AND. ISTA) THEN
         DO
            READ (tal, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               talyear, talmonth, talday, talhour, talminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the minimum daily temperature time series file. '// &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the minimum daily temperature time series file '// &
                  'and the end date is not before the start date of the simulation'
               CALL ERR_STOP(255)
            END IF

            taldate = HOUR_FROM_DATE(talyear, talmonth, talday, talhour, talminute)
            IF (taldate + 0.01d0 > tih) THEN
               TALFIRST = .FALSE.
               BACKSPACE (tal)
               EXIT
            END IF
         END DO
      END IF

! set period of validity of current data
      EXITT = .FALSE.

      timestep_reduction_loop: DO
         TEND = MIN(UZNOW + UZNEXT, METIME)

         ! store first period of precipitation using array slicing
         PTOT(1:NRAIN) = (TEND - UZNOW)*PINP(1:NRAIN)

         IF (EXITT) EXIT timestep_reduction_loop

         ! test if timestep reduction required without reading any prec. data
         DO I = 1, NRAIN
            IF (PTOT(I) > PMAX) THEN
               EXITT = .TRUE.
               UZNEXT = MIN(UZNEXT, PMAX/PINP(I))
            END IF
         END DO

         ! If we didn't trigger an exit condition, break the loop naturally
         IF (.NOT. EXITT) EXIT timestep_reduction_loop
      END DO timestep_reduction_loop

! read in prec. data if required, test for timestep reduction,
! and accumulate total prec.
      meteorological_loop: DO WHILE (.NOT. EXITT .AND. METIME < UZNOW + UZNEXT)
         IFLAG = 1
         CALL METIN(IFLAG)

         DO I = 1, NRAIN
            IF (PTOT(I) + (METIME - MELAST)*PINP(I) > PMAX) THEN
               EXITT = .TRUE.
               UZTEST = MELAST - UZNOW + (PMAX - PTOT(I))/PINP(I)
               UZNEXT = MIN(UZNEXT, UZTEST)
            END IF
         END DO

         TEND = MIN(UZNOW + UZNEXT, METIME)

         ! Accumulate using array slicing
         PTOT(1:NRAIN) = PTOT(1:NRAIN) + (TEND - MELAST)*PINP(1:NRAIN)
      END DO meteorological_loop

! check for invalid timestep (could be a result of data errors)
      IF (UZNEXT < 5.0D-5) THEN
         WRITE (FID_logfile, "(////'UZNEXT = ',G14.6, /' TSOFT = ',G14.6, /'MELAST = ',G14.6, "// &
            "/'METIME = ',G14.6 /, 'PREC.STN.   PINP        PTOT'/)") &
            UZNEXT, TSOFT, MELAST, METIME
         WRITE (FID_logfile, "(4X,I4,2G14.6)") (I, PINP(I), PTOT(I), I=1, NRAIN)
         CALL RAISE_ERROR(ERRLVL_fatal, 1025, FID_logfile, 0, 0, 'INVALID TIMESTEP')
      END IF

      ! calculate average value over timestep (& convert mm/h to m/s)
      DO IEL = 1, total_no_elements
         precip_m_per_s(IEL) = PTOT(NRAINC(IEL))/UZNEXT/3.6E6
      END DO

      ! read in breakpoint PE for this timestep (if required)
      IFLAG = 2
      CALL METIN(IFLAG)

   END SUBROUTINE TMSTEP

END MODULE timestep_control

