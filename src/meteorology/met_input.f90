!> summary: Reading the meteorological input series and advancing the vegetation tables.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[METIN]] advances every meteorological series to the current model time and
!> writes the results into [[met_forcing]]; it also interpolates the
!> time-varying vegetation parameters in [[et_config]] through
!> [[interpolation:TERPO1]]. [[READ_DATED_RECORD]] and
!> [[RESIZE_MET_RECORD]] implement the dated record format, growing the record
!> buffer as needed.
!>
!> The buffer starts at `LENGTH_LINEVERYLONG` characters and each value is
!> allowed `LENGTH_TEXT_R8P`; the `IOSTAGE_*` and `IOS_SHORT_RECORD` values
!> distinguish a short record from a genuine read failure.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP / RAH | 2.0-4.2 | Developed the frame driver, the meteorological reader and the timestep control. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the remaining frame `.F` files to Fortran 90. |
!> | 2015-2026 | SB / SvB | 4.5-4.6 | Added the separate temperature streams, the dated meteorological reader and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of rest; see docs/rename/proposal.md. |
!> @endhistory
MODULE met_input

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, LENGTH_LINEVERYLONG, LENGTH_TEXT_R8P, &
                             one, zero
   USE array_limits, ONLY: NVEE
   USE simulation_clock, ONLY: TIMEUZ, UZNEXT, UZNOW
   USE file_units, ONLY: EPD, FID_logfile, MED, PRD, TAH, TAL
   USE run_control, ONLY: BHOTRD, BHOTTI
   USE met_forcing, ONLY: DTMET, DTMET2, DTMET3, ISTA, NM, NRAIN, OBSPE, RN, TA, U, VPD
   USE et_state, ONLY: CLAI, NV, PLAI, VHT
   USE et_config, ONLY: BMETAL, BMETDATES, BMETP, CLAI1, CSTCA1, CSTCAP, DEL, MEASPE, MODECL, &
                        MODECS, MODEPL, MODEVH, NCTCLA, NCTCST, NCTPLA, NCTVHT, PLAI1, &
                        RELCLA, RELCST, RELPLA, RELVHT, TIMCLA, TIMCST, TIMPLA, TIMVHT, VHT1
   USE interpolation, ONLY: TERPO1
   USE datetime, ONLY: hour_from_date
   USE error_reporting, ONLY: ERR_STOP
   USE error_status, ONLY: errstat_alloc, errstat_dealloc, errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: METIN
   PUBLIC :: metime, melast, eptime, pinp

   DOUBLEPRECISION :: pinp(nvee + 10) = zero !! Current precipitation input by rain station, used by `METIN` and `TMSTEP` (mm/hr).
   DOUBLEPRECISION :: METIME = zero !! End time of the current precipitation/full-meteorological record window (h).
   DOUBLEPRECISION :: MELAST = zero !! Start time of the current precipitation/full-meteorological record window (h).
   DOUBLEPRECISION :: EPTIME = zero !! End time of the current potential-evaporation record window (h).
   INTEGER, PARAMETER :: RECORD_HEADROOM = 10 !! Characters kept free at the end of `MET_RECORD`; a record reaching into them is treated as too long for the buffer.
   INTEGER, PARAMETER :: IOSTAGE_NONE = 0 !! `READ_DATED_RECORD` completed without an error.
   INTEGER, PARAMETER :: IOSTAGE_RECORD = 1 !! `READ_DATED_RECORD` failed while reading the timestamp and record text.
   INTEGER, PARAMETER :: IOSTAGE_VALUES = 2 !! `READ_DATED_RECORD` failed while parsing the values of the record.
   INTEGER, PARAMETER :: IOS_SHORT_RECORD = 1 !! `IOS` reported by `READ_DATED_RECORD` for a record holding fewer values than expected.

   ! Dated meteorological record buffer, see READ_DATED_RECORD -----------------
   CHARACTER(LEN=:), ALLOCATABLE :: MET_RECORD !! Reusable buffer holding the value part of the dated meteorological record currently being read.
   LOGICAL :: MET_RECORD_SIZED = .FALSE. !! `.TRUE.` once `MET_RECORD` has been resized from its initial capacity to fit the first data line read.

CONTAINS

   !> Reads one dated meteorological record: its timestamp and `NVALUES` values.
   !>
   !> The record text is read into the module buffer `MET_RECORD`, whose capacity
   !> is proportional to the number of values actually expected rather than to a
   !> fixed worst case. The buffer starts at `LENGTH_LINEVERYLONG` characters and
   !> is resized once the first data line has been read successfully, to
   !>
   !> \[
   !> capacity = \max(NVALUES \cdot LENGTH\_TEXT\_R8P,\; len\_trim(record)) +
   !>            RECORD\_HEADROOM .
   !> \]
   !>
   !> Because all dated files (`PRD`, `EPD`, `TAH`, `TAL`) share the buffer, a
   !> later call needing more room grows it again; the capacity therefore
   !> converges to the widest record in use and no per-record allocation occurs.
   !>
   !> A record whose text reaches into the last `RECORD_HEADROOM` characters may
   !> have been truncated. The buffer is then grown (doubled, capped at
   !> `LENGTH_LINEVERYLONG`), the record is re-read after a `BACKSPACE`, and the
   !> run stops with a diagnostic if even `LENGTH_LINEVERYLONG` characters are
   !> not enough.
   !>
   !> The timestamp is parsed once per record into `DATEHOUR` using
   !> [[datetime:hour_from_date]]; the seconds field is consumed but not used.
   !> `DATEHOUR` and `VALUES` are left unchanged when the read fails, so an
   !> end-of-file caller keeps whatever fallback it has already set.
   !>
   !> `IOS` follows the usual convention (`<0` end of file, `>0` error) and
   !> `IOSTAGE` reports which step failed, so callers can keep their own
   !> file-specific messages. Only the record read reports end of file: a record
   !> that exists but carries fewer than `NVALUES` values is reported as the
   !> error `IOS_SHORT_RECORD` instead, because it is a data problem rather than
   !> the end of the series.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 2026-08-22 | SvB | - | Initial version, replacing the fixed 100000-character record buffer in [[metin]]. |
   !> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
   !> @endhistory
   SUBROUTINE READ_DATED_RECORD(UNIT, NVALUES, DATEHOUR, VALUES, IOS, IOSTAGE)
      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)             :: UNIT     !! Unit of the dated meteorological file to read from.
      INTEGER, INTENT(IN)             :: NVALUES  !! Number of values expected after the timestamp.
      DOUBLE PRECISION, INTENT(INOUT) :: DATEHOUR !! Record timestamp in SHETRAN hours; unchanged when the record could not be read.
      DOUBLE PRECISION, INTENT(INOUT) :: VALUES(:)!! Receives `VALUES(1:NVALUES)`; unchanged when the record could not be read.
      INTEGER, INTENT(OUT)            :: IOS      !! Status of the read: `0` success, `<0` end of file, `>0` error.
      INTEGER, INTENT(OUT)            :: IOSTAGE  !! Step that failed: `IOSTAGE_NONE`, `IOSTAGE_RECORD`, or `IOSTAGE_VALUES`.

      ! Locals
      INTEGER :: YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
      INTEGER :: NEEDED, TRIMMED

      INTEGER(KIND=I_P) :: status
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      !----------------------------------------------------------------------*

      IOSTAGE = IOSTAGE_NONE
      NEEDED = NVALUES*LENGTH_TEXT_R8P + RECORD_HEADROOM

      IF (.NOT. ALLOCATED(MET_RECORD)) THEN
         ! start from the reserved capacity; the first data line sets the real size
         ALLOCATE (CHARACTER(LEN=LENGTH_LINEVERYLONG) :: MET_RECORD, STAT=status, ERRMSG=emsg)
         CALL errstat_alloc(status, "MET_RECORD", "met_input:READ_DATED_RECORD", emsg)
         MET_RECORD_SIZED = .FALSE.
      ELSE IF (MET_RECORD_SIZED .AND. LEN(MET_RECORD) < NEEDED) THEN
         ! a wider file than the one that sized the buffer
         CALL RESIZE_MET_RECORD(MIN(NEEDED, LENGTH_LINEVERYLONG))
      END IF

      read_record: DO
         READ (UNIT, 9000, IOSTAT=IOS) YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, MET_RECORD

         IF (IOS /= 0) THEN
            IOSTAGE = IOSTAGE_RECORD
            RETURN
         END IF

         TRIMMED = LEN_TRIM(MET_RECORD)
         IF (TRIMMED <= LEN(MET_RECORD) - RECORD_HEADROOM) EXIT read_record

         ! the record filled the buffer: grow and read it again, or give up
         IF (LEN(MET_RECORD) >= LENGTH_LINEVERYLONG) THEN
            WRITE (*, 9010) ' Error reading a dated meteorological time series file. A record needs more than ', &
               LENGTH_LINEVERYLONG, ' characters for ', NVALUES, &
               ' values. Reduce the number of stations or the column width of the file.'
            CALL ERR_STOP(255)
         END IF

         CALL RESIZE_MET_RECORD(MIN(2*LEN(MET_RECORD), LENGTH_LINEVERYLONG))
         BACKSPACE (UNIT)
      END DO read_record

      DATEHOUR = HOUR_FROM_DATE(YEAR, MONTH, DAY, HOUR, MINUTE)

      READ (MET_RECORD, *, IOSTAT=IOS) VALUES(1:NVALUES)
      IF (IOS /= 0) THEN
         ! a record that exists but holds too few values is a data error, not an end of file
         IF (IOS < 0) IOS = IOS_SHORT_RECORD
         IOSTAGE = IOSTAGE_VALUES
         RETURN
      END IF

      IF (.NOT. MET_RECORD_SIZED) THEN
         ! first full data line read: make the buffer proportional to the record
         CALL RESIZE_MET_RECORD(MIN(MAX(NEEDED, TRIMMED + RECORD_HEADROOM), LENGTH_LINEVERYLONG))
         MET_RECORD_SIZED = .TRUE.
      END IF

      RETURN

      ! FORMAT STATEMENTS
9000  FORMAT(I4, 1X, I2, 1X, I2, 1X, I2, 1X, I2, 1X, I2, 1X, A)
9010  FORMAT(A, I0, A, I0, A)

   END SUBROUTINE READ_DATED_RECORD

   !> Reallocates the dated meteorological record buffer `MET_RECORD` to `CAPACITY` characters.
   !>
   !> The buffer contents are not preserved; callers resize it only between
   !> records or immediately before re-reading a record.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 2026-08-22 | SvB | - | Initial version. |
   !> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
   !> @endhistory
   SUBROUTINE RESIZE_MET_RECORD(CAPACITY)
      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN) :: CAPACITY !! New buffer length in characters.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.

      !----------------------------------------------------------------------*

      IF (ALLOCATED(MET_RECORD)) THEN
         IF (LEN(MET_RECORD) == CAPACITY) RETURN
         DEALLOCATE (MET_RECORD, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "MET_RECORD", "met_input:RESIZE_MET_RECORD", emsg)
      END IF

      ALLOCATE (CHARACTER(LEN=CAPACITY) :: MET_RECORD)

   END SUBROUTINE RESIZE_MET_RECORD

   !> Reads or interpolates meteorological forcing required by ET, interception, and snowmelt.
   !>
   !> `METIN` advances precipitation, potential evaporation, radiation, wind,
   !> temperature, vapour pressure deficit, and (via [[interpolation:TERPO1]]) the
   !> current time-varying canopy-storage-capacity, plant/land-cover leaf-area,
   !> and vegetation-height values in [[et_config]] needed for the current
   !> simulation time. In date-aware mode, [[tmstep]] first checks and positions
   !> the dated forcing files; `METIN` then consumes the selected records and
   !> converts their ISO-8601-like date fields to SHETRAN hours using
   !> [[datetime:hour_from_date]].
   !>
   !> | Mode | Files and records | Code path |
   !> |:-----|:-------------------|:----------|
   !> | `BMETAL=.FALSE.` | Full meteorological data in `MED`, updated every `DTMET` hours. If `NM=NRAIN`, rainfall and meteorological data share the same station distribution and are read together; optional measured PE follows on the next `MED` record. If `NM/=NRAIN`, meteorological and rainfall data are read from separate `MED` record groups. | Reads `RN`, `U`, `TA`, `DEL`, `VPD`, optional `OBSPE`, and `PINP`. |
   !> | `BMETAL=.TRUE.` | Separate precipitation (`PRD`) and potential-evaporation (`EPD`) files, updated every `DTMET2` and `DTMET3` hours respectively. Optional date-aware files carry an ISO-8601-like first column when `BMETDATES=.TRUE.`, with optional companion max/min-temperature files (`TAH`/`TAL`) when `ISTA` is enabled. | `PINP` is read from `PRD`; potential evaporation `PEIN`/`OBSPE` is read from `EPD`. |
   !>
   !> The principal variables and units are:
   !>
   !> | Variable | Meaning | Input units | Internal use |
   !> |:---------|:--------|:------------|:-------------|
   !> | `ISITE` | Station identifier. | - | Read but not used for interpolation here. |
   !> | `METIME` | Validity time of the current meteorological data. | h | Advanced by `DTMET`, `DTMET2`, or `DTMET3`. |
   !> | `DTMET` | Full meteorological-data interval. | h | `MED` update interval. |
   !> | `DTMET2` | Precipitation-data interval. | h | `PRD` update interval. |
   !> | `DTMET3` | Potential-evaporation-data interval. | h | `EPD` update interval. |
   !> | `PINP` | Precipitation. | mm/hr in `MED`; interval depth over `DTMET2` in `PRD` | Stored as a rate in mm/hr for timestep accumulation in [[tmstep]]. |
   !> | `OBSPE` | Measured potential evaporation/evapotranspiration. | mm/hr in `MED`; interval depth over `DTMET3` in `EPD` | Converted to mm/s for ET calculations. |
   !> | `RN` | Net radiation. | W/m^2 | Used by ET. |
   !> | `U` | Wind speed. | m/s | Used by ET. |
   !> | `TA` | Air temperature. | C | Used by ET and snowmelt; from max/min temperature average when `ISTA` is enabled. |
   !> | `DEL` | Slope of saturation vapour pressure versus temperature. | mb/C | Used by ET. |
   !> | `VPD` | Vapour pressure deficit. | mb | Used by ET. |
   !> | `PA` | Atmospheric pressure. | mb | Read from `MED` into the local `PA` but not otherwise used. |
   !> | `IDATA` | Data-quality indicator. | - | Read from `MED` but not used. |
   !>
   !> For separate `PRD`/`EPD` files the input is an interval amount; the code
   !> converts it to a rate before later timestep averaging:
   !>
   !> \[
   !> PINP_i = \frac{PRD_i}{DTMET2},\qquad
   !> PEIN_i = \frac{EPD_i}{DTMET3}.
   !> \]
   !>
   !> [[tmstep]] later accumulates `PINP` over the model timestep and converts
   !> the average precipitation to `precip_m_per_s` with
   !> `PTOT / UZNEXT / 3.6E6`. For separate `EPD` input, `METIN` accumulates
   !> potential evaporation over the current model timestep,
   !>
   !> \[
   !> PETOT_i = \sum_m \Delta t_m\,PEIN_{i,m},
   !> \]
   !>
   !> then stores the ET-module value as
   !>
   !> \[
   !> OBSPE_i = \frac{PETOT_i}{UZNEXT\,3600},
   !> \]
   !>
   !> in mm/s. When max/min temperature forcing is available, the air
   !> temperature used at the end of the timestep is the simple average
   !> \(TA_i=(TAHIGH_i+TALOW_i)/2\). If an input file ends, the first
   !> occurrence is reported to the `.pri` output and remaining precipitation
   !> or PE values are set to zero, while missing optional max/min
   !> temperatures default to 10 C; a malformed record is a fatal
   !> `ERROR STOP` rather than end-of-file. The legacy comment notes that
   !> precipitation is averaged over the computational timestep elsewhere;
   !> that averaging is performed by [[tmstep]].
   !>
   !> Finally, `METIN` updates any time-varying vegetation parameters flagged
   !> in [[et_config]] (`MODECS`, `MODEPL`, `MODECL`, `MODEVH`) by calling
   !> [[interpolation:TERPO1]] at the current `TIMEUZ` (see [[simulation_clock]]) for canopy-storage
   !> capacity (`CSTCAP`), plant leaf area (`PLAI`), land-cover leaf area
   !> (`CLAI`), and vegetation height (`VHT`), for every vegetation type `1:NV`.
   !>
   !> @note
   !> For dated `PRD`/`EPD`/`TAH`/`TAL` files the parsed dates are used for
   !> start-file checks and initial positioning in [[tmstep]]. Within this
   !> routine the active record windows are still advanced by `DTMET2` and
   !> `DTMET3`. The declared locals `PER`, `TAHIGHT`, and `TALOWT` are never
   !> referenced in the current body.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-10-01 | RAH | 3.4.1 | Added legacy double-precision typing. |
   !> | 1996-12-28 | RAH | 4.1 | Initialised `PELAST`; moved data from `SPEC.ET`; removed redundant interpolation argument. |
   !> | 2026-03-19 | SB | 4.6.1 | Added optional date-aware meteorological input handling (`BMETDATES`, `TAH`/`TAL`). |
   !> | 2026-04-06 | SvB | 4.6.1 | Replaced `GOTO`-driven control flow with named `DO`/`CYCLE`/`EXIT` loop constructs. |
   !> | 2026-05-10 | SvB | - | Replaced interactive "press enter to continue" prompts after fatal read errors with `ERROR STOP`. |
   !> | 2026-08-22 | SvB | - | Moved dated record reading into [[met_input:READ_DATED_RECORD]], replacing the fixed 100000-character line buffer. |
   !> @endhistory
   SUBROUTINE METIN(IFLAG)
      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IFLAG !! Read mode: `1` advances precipitation records; `2` advances potential evaporation, temperature, and time-varying vegetation parameters.

      ! Locals, etc
      INTEGER             :: I, IDATA, ISITE, K, NN
      DOUBLE PRECISION    :: EPLAST, TCURR, TEND
      DOUBLE PRECISION    :: PA(NVEE), PEIN(NVEE), PETOT(NVEE), PER(NRAIN)
      DOUBLE PRECISION    :: TAHIGHT(NVEE), TALOWT(NVEE), TAHIGH(NVEE), TALOW(NVEE)
      LOGICAL             :: FIRSTNOPRD = .TRUE., FIRSTNOEPD1 = .TRUE., FIRSTNOEPD2 = .TRUE.
      LOGICAL             :: FIRSTNOMET1 = .TRUE., FIRSTNOMET2 = .TRUE., FIRSTNOMET3 = .TRUE.
      LOGICAL             :: FIRSTNOMET4 = .TRUE., FIRSTNOMET5 = .TRUE.
      INTEGER             :: ios, iostage
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'met_input:METIN' !! Location string for read-error reports.
      DOUBLE PRECISION    :: prddate, epddate, tahdate, taldate
      !----------------------------------------------------------------------*

      ! record timestamps are read for validation only, so they start defined
      prddate = ZERO
      epddate = ZERO
      tahdate = ZERO
      taldate = ZERO

      IF (BMETAL) THEN

         ! READ PREC. & OBSERVED POT. EVAPOTRANSPIRATION BREAKPOINT FORMAT FILES
         !-----------------------------------------------------------------------

         ! PRECIPITATION
         ! read only one line of file (unless hotstarted run)

         IF (IFLAG == 1) THEN
            precip_read_loop: DO
               IF (BMETDATES) THEN
                  CALL READ_DATED_RECORD(prd, NRAIN, prddate, PINP, ios, iostage)

                  IF (ios > 0) THEN
                     IF (iostage == IOSTAGE_RECORD) THEN
                        WRITE (*, 9020) ' Error reading the precipitation time series file. '// &
                           'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ', &
                           NRAIN, ' values on each row'
                     ELSE
                        WRITE (*, 9020) ' Error reading the precipitation time series file. '// &
                           'This should have the date in the iso 8601 format followed by ', NRAIN, ' values'
                     END IF
                     CALL ERR_STOP(255)
                  END IF

                  IF (ios < 0) THEN
                     IF (FIRSTNOPRD) THEN
                        WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of prd data', 'All remaining values will be zero'
                        FIRSTNOPRD = .FALSE.
                     END IF
                     PINP(1:NRAIN) = ZERO
                  END IF

               ELSE
                  READ (PRD, *, IOSTAT=ios) PINP(1:NRAIN)

                  IF (ios > 0) THEN
                     WRITE (*, 9020) ' Error reading the precipitation time series file. This should have ', &
                        NRAIN, ' values on each row with no dates in the first column (see ET1)'
                     CALL ERR_STOP(255)
                  END IF

                  IF (ios < 0) THEN
                     IF (FIRSTNOPRD) THEN
                        WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of prd data', 'All remaining values will be zero'
                        FIRSTNOPRD = .FALSE.
                     END IF
                     PINP(1:NRAIN) = ZERO
                  END IF
               END IF

               PINP(1:NRAIN) = PINP(1:NRAIN)/dtmet2
               MELAST = METIME
               METIME = METIME + dtmet2

               IF (.NOT. (BHOTRD .AND. METIME < BHOTTI)) EXIT precip_read_loop
            END DO precip_read_loop

         ELSE
            ! POT. EVAP and TEMPERATURE DATA READ PART 1

            ! first check for hotstarted run
            IF (BHOTRD .AND. EPTIME < BHOTTI) THEN
               hotstart_epd_loop: DO
                  ! epd and temperature files have dates
                  IF (BMETDATES) THEN
                     CALL READ_DATED_RECORD(epd, NM, epddate, PEIN, ios, iostage)

                     IF (ios > 0) THEN
                        IF (iostage == IOSTAGE_RECORD) THEN
                           WRITE (*, 9020) ' Error reading the potential evaporation time series file. '// &
                              'This should have the date in iso 8601 format followed by ', NM, ' values on each row'
                        ELSE
                           WRITE (*, 9022) ' Error reading potential evap data values from line.'
                        END IF
                        CALL ERR_STOP(255)
                     END IF

                     IF (ios < 0) THEN
                        IF (FIRSTNOEPD2) THEN
                           WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                           FIRSTNOEPD2 = .FALSE.
                        END IF
                        PEIN(1:NM) = ZERO
                     END IF

                     IF (ISTA) THEN
                        CALL READ_DATED_RECORD(TAH, NM, tahdate, TAHIGH, ios, iostage)
                        IF (ios > 0) THEN
                           IF (iostage == IOSTAGE_RECORD) THEN
                              WRITE (*, 9022) ' Error reading max temp time series file.'
                           ELSE
                              WRITE (*, 9022) ' Error reading max temp values from line.'
                           END IF
                           CALL ERR_STOP(255)
                        END IF
                        IF (ios < 0) TAHIGH(1:NM) = 10.0d0
                     END IF

                     IF (ISTA) THEN
                        CALL READ_DATED_RECORD(TAL, NM, taldate, TALOW, ios, iostage)
                        IF (ios > 0) THEN
                           IF (iostage == IOSTAGE_RECORD) THEN
                              WRITE (*, 9022) ' Error reading min temp time series file.'
                           ELSE
                              WRITE (*, 9022) ' Error reading min temp values from line.'
                           END IF
                           CALL ERR_STOP(255)
                        END IF
                        IF (ios < 0) TALOW(1:NM) = 10.0d0
                     END IF

                     PEIN(1:NM) = PEIN(1:NM)/dtmet3
                     EPLAST = EPTIME
                     EPTIME = EPTIME + dtmet3

                     IF (.NOT. (BHOTRD .AND. EPTIME < BHOTTI)) EXIT hotstart_epd_loop

                     ! epd and temperature files DO NOT have dates
                  ELSE
                     READ (EPD, *, IOSTAT=ios) PEIN(1:NM)
                     IF (ios > 0) THEN
                        WRITE (*, 9020) ' Error reading the potential evaporation time series file. This should have ', &
                           NM, ' values on each row with no dates in the first column'
                        CALL ERR_STOP(255)
                     END IF

                     IF (ios < 0) THEN
                        IF (FIRSTNOEPD1) THEN
                           WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                           FIRSTNOEPD1 = .FALSE.
                        END IF
                        PEIN(1:NM) = ZERO
                     END IF

                     IF (ISTA) THEN
                        READ (TAH, *, IOSTAT=ios, IOMSG=emsg) TAHIGH(1:NM)
                        IF (ios > 0) CALL errstat_read(ios, TRIM(location)//' (max temp file)', emsg)
                        IF (ios < 0) TAHIGH(1:NM) = 10.0d0
                     END IF

                     IF (ISTA) THEN
                        READ (TAL, *, IOSTAT=ios, IOMSG=emsg) TALOW(1:NM)
                        IF (ios > 0) CALL errstat_read(ios, TRIM(location)//' (min temp file)', emsg)
                        IF (ios < 0) TALOW(1:NM) = 10.0d0
                     END IF

                     PEIN(1:NM) = PEIN(1:NM)/dtmet3
                     EPLAST = EPTIME
                     EPTIME = EPTIME + dtmet3

                     IF (.NOT. (BHOTRD .AND. EPTIME < BHOTTI)) EXIT hotstart_epd_loop
                  END IF
               END DO hotstart_epd_loop
            END IF
            ! end of check for hotstarted run

            ! calculate average PE value over computational timestep
            TEND = MIN(UZNOW + UZNEXT, EPTIME)
            PETOT(1:NM) = (TEND - UZNOW)*PEIN(1:NM)

            ! POT. EVAP and TEMPERATURE DATA READ PART 2
            ! check if it is time to read in PET data
            IF (EPTIME < UZNOW + UZNEXT) THEN
               pet_read_loop: DO
                  ! epd and temperature files have dates
                  IF (BMETDATES) THEN
                     CALL READ_DATED_RECORD(epd, NM, epddate, PEIN, ios, iostage)

                     IF (ios > 0) THEN
                        IF (iostage == IOSTAGE_RECORD) THEN
                           WRITE (*, 9022) ' Error reading PET file.'
                        ELSE
                           WRITE (*, 9022) ' Error reading PET values from line.'
                        END IF
                        CALL ERR_STOP(255)
                     END IF

                     IF (ios < 0) THEN
                        IF (FIRSTNOEPD2) THEN
                           WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                           FIRSTNOEPD2 = .FALSE.
                        END IF
                        PEIN(1:NM) = ZERO
                     END IF

                     IF (ISTA) THEN
                        CALL READ_DATED_RECORD(TAH, NM, tahdate, TAHIGH, ios, iostage)
                        IF (ios > 0) THEN
                           IF (iostage == IOSTAGE_RECORD) THEN
                              WRITE (*, 9022) ' Error reading max temp file.'
                           ELSE
                              WRITE (*, 9022) ' Error reading max temp values from line.'
                           END IF
                           CALL ERR_STOP(255)
                        END IF
                        IF (ios < 0) TAHIGH(1:NM) = 10.0d0
                     END IF

                     IF (ISTA) THEN
                        CALL READ_DATED_RECORD(TAL, NM, taldate, TALOW, ios, iostage)
                        IF (ios > 0) THEN
                           IF (iostage == IOSTAGE_RECORD) THEN
                              WRITE (*, 9022) ' Error reading min temp file.'
                           ELSE
                              WRITE (*, 9022) ' Error reading min temp values from line.'
                           END IF
                           CALL ERR_STOP(255)
                        END IF
                        IF (ios < 0) TALOW(1:NM) = 10.0d0
                     END IF

                     PEIN(1:NM) = PEIN(1:NM)/dtmet3
                     EPLAST = EPTIME
                     EPTIME = EPTIME + dtmet3
                     TEND = MIN(UZNOW + UZNEXT, EPTIME)
                     PETOT(1:NM) = PETOT(1:NM) + (TEND - EPLAST)*PEIN(1:NM)

                     IF (.NOT. (EPTIME < UZNOW + UZNEXT)) EXIT pet_read_loop

                     ! epd and temperature files DO NOT have dates
                  ELSE
                     READ (EPD, *, IOSTAT=ios, IOMSG=emsg) PEIN(1:NM)
                     IF (ios > 0) CALL errstat_read(ios, TRIM(location)//' (PET file)', emsg)

                     IF (ios < 0) THEN
                        IF (FIRSTNOEPD2) THEN
                           WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                           FIRSTNOEPD2 = .FALSE.
                        END IF
                        PEIN(1:NM) = ZERO
                     END IF

                     IF (ISTA) THEN
                        READ (TAH, *, IOSTAT=ios, IOMSG=emsg) TAHIGH(1:NM)
                        IF (ios > 0) CALL errstat_read(ios, TRIM(location)//' (max temp file)', emsg)
                        IF (ios < 0) TAHIGH(1:NM) = 10.0d0
                     END IF

                     IF (ISTA) THEN
                        READ (TAL, *, IOSTAT=ios, IOMSG=emsg) TALOW(1:NM)
                        IF (ios > 0) CALL errstat_read(ios, TRIM(location)//' (min temp file)', emsg)
                        IF (ios < 0) TALOW(1:NM) = 10.0d0
                     END IF

                     PEIN(1:NM) = PEIN(1:NM)/dtmet3
                     EPLAST = EPTIME
                     EPTIME = EPTIME + dtmet3
                     TEND = MIN(UZNOW + UZNEXT, EPTIME)
                     PETOT(1:NM) = PETOT(1:NM) + (TEND - EPLAST)*PEIN(1:NM)

                     IF (.NOT. (EPTIME < UZNOW + UZNEXT)) EXIT pet_read_loop
                  END IF
               END DO pet_read_loop
            END IF

            OBSPE(1:NM) = PETOT(1:NM)/UZNEXT/3600.0d0
            ! for simplicity the temperature used is the value at the end of the timestep
            TA(1:NM) = (TAHIGH(1:NM) + TALOW(1:NM))/2.0d0

         END IF

         ! PRINT OUT INPUT DATA
         IF (BMETP) THEN
            WRITE (FID_logfile, 9130) METIME
            DO I = 1, NM
               WRITE (FID_logfile, 9140) I, PINP(I), PEIN(I)
            END DO
         END IF

      ELSE
         ! READ ALL MET. DATA IN FIXED TIME INTERVAL (USUALLY HOURLY) FORMAT
         !------------------------------------------------------------------
         IF (IFLAG == 2) RETURN

         IF (NRAIN == NM) THEN
            !-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE EQUAL
            IF (BMETP) WRITE (FID_logfile, 9100)

            !-----LOOP ON NUMBER OF MET SITES
            read_equal_loop: DO
               MELAST = METIME
               METIME = METIME + DTMET

               DO I = 1, NM
                  READ (MED, 9030, IOSTAT=ios) ISITE, NN, PINP(I), RN(I), U(I), PA(I), TA(I), DEL(I), VPD(I), IDATA

                  IF (ios < 0) THEN
                     IF (FIRSTNOMET1) THEN
                        WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
                        FIRSTNOMET1 = .FALSE.
                     END IF
                     ISITE = 1
                     NN = 1
                     PINP(I) = ZERO
                     RN(I) = ZERO
                     U(I) = ZERO
                     PA(I) = ZERO
                     TA(I) = 10.0d0
                     DEL(I) = ONE
                     VPD(I) = 3.0d0
                     IDATA = 1000
                  END IF

                  IF (BMETP) WRITE (FID_logfile, 9040) ISITE, METIME, PINP(I), RN(I), U(I), TA(I), DEL(I), VPD(I)

                  IF (MEASPE(I) == 0) CYCLE

                  ! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
                  READ (MED, 9050, IOSTAT=ios) OBSPE(I)
                  IF (ios < 0) THEN
                     IF (FIRSTNOMET2) THEN
                        WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
                        FIRSTNOMET2 = .FALSE.
                     END IF
                     OBSPE(I) = 0.0d0
                  END IF

                  ! CONVERT TO MM/S
                  OBSPE(I) = OBSPE(I)/3600.0d0
               END DO

               ! READ TO START SIMULATION TIME, IF HOTSTART
               IF (.NOT. (BHOTRD .AND. METIME < BHOTTI)) EXIT read_equal_loop
            END DO read_equal_loop

         ELSE
            !-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE UNEQUAL
            IF (BMETP) WRITE (FID_logfile, 9110)

            !-----LOOP ON NUMBER OF MET SITES
            read_unequal_loop: DO
               MELAST = METIME
               METIME = METIME + DTMET

               DO I = 1, NM
                  READ (MED, 9060, IOSTAT=ios) ISITE, NN, RN(I), U(I), PA(I), TA(I), DEL(I), VPD(I), IDATA

                  IF (ios < 0) THEN
                     IF (FIRSTNOMET3) THEN
                        WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
                        FIRSTNOMET3 = .FALSE.
                     END IF
                     ISITE = 1
                     NN = 1
                     RN(I) = ZERO
                     U(I) = ZERO
                     PA(I) = ZERO
                     TA(I) = 10.0d0
                     DEL(I) = ONE
                     VPD(I) = 3.0d0
                     IDATA = 1000
                  END IF

                  IF (BMETP) WRITE (FID_logfile, 9070) ISITE, METIME, RN(I), U(I), TA(I), DEL(I), VPD(I)

                  IF (MEASPE(I) == 0) CYCLE

                  ! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
                  READ (MED, 9050, IOSTAT=ios) OBSPE(I)
                  IF (ios < 0) THEN
                     IF (FIRSTNOMET4) THEN
                        WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
                        FIRSTNOMET4 = .FALSE.
                     END IF
                     OBSPE(I) = 0.0d0
                  END IF

                  ! CONVERT TO MM/S
                  OBSPE(I) = OBSPE(I)/3600.0d0
               END DO

               IF (BMETP) WRITE (FID_logfile, 9120)

               !-----LOOP ON NUMBER OF RAIN SITES
               DO I = 1, NRAIN
                  READ (MED, 9080, IOSTAT=ios) ISITE, NN, PINP(I), IDATA
                  IF (ios < 0) THEN
                     IF (FIRSTNOMET5) THEN
                        WRITE (FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
                        FIRSTNOMET5 = .FALSE.
                     END IF
                     PINP(I) = 0.0d0
                  END IF

                  IF (BMETP) WRITE (FID_logfile, 9090) ISITE, METIME, PINP(I)
               END DO

               ! READ TO SIMULATION START TIME, IF HOTSTART
               IF (.NOT. (BHOTRD .AND. METIME < BHOTTI)) EXIT read_unequal_loop
            END DO read_unequal_loop

         END IF
      END IF

      !--------------------------------------------
      !     CHECK TIME-VARYING MODEL PARAMETERS
      !--------------------------------------------
      TCURR = TIMEUZ
      DO K = 1, NV
         ! sb 04032025 for dynamically allocated arrays use NV not NVEE
         IF (MODECS(K) /= 0) CALL TERPO1(CSTCAP, TCURR, RELCST, TIMCST, NCTCST, CSTCA1, NV, K)
         IF (MODEPL(K) /= 0) CALL TERPO1(PLAI, TCURR, RELPLA, TIMPLA, NCTPLA, PLAI1, NV, K)
         IF (MODECL(K) /= 0) CALL TERPO1(CLAI, TCURR, RELCLA, TIMCLA, NCTCLA, CLAI1, NV, K)
         IF (MODEVH(K) /= 0) CALL TERPO1(VHT, TCURR, RELVHT, TIMVHT, NCTVHT, VHT1, NV, K)
      END DO

      RETURN

      ! FORMAT STATEMENTS
9010  FORMAT(///, A6, G12.4, A8, /, A18, /, A33, ///)
9020  FORMAT(A, I0, A)
9022  FORMAT(A)
9030  FORMAT(2I6, 4G12.6, /, 12X, 3G12.6, I12)
9040  FORMAT('0', 8X, I6, F8.2, 5X, 2(3F12.6, '  NOT_USED  '))
9050  FORMAT(12X, G12.6)
9060  FORMAT(2I6, 12X, 3G12.6, /, 12X, 3G12.6, I12)
9070  FORMAT('0', 8X, I6, F8.2, 5X, 2(2F12.6, '  NOT_USED  ':F12.6))
9080  FORMAT(2I6, G12.6, 24X, I12)
9090  FORMAT('0', 9X, I6, F8.2, 5X, F12.6, '  NOT_USED  ')
9100  FORMAT(//, 1X, 'MET DATA - SITE    TIME      RAINFALL    NET RADN', 4X, &
         'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
9110  FORMAT(//, 1X, 'MET DATA - SITE    TIME      NET RADN', 4X, &
         'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
9120  FORMAT(//, 1X, 'RAIN DATA - SITE    TIME      RAINFALL         IDATA')
9130  FORMAT(//, 1X, 'MET DATA -  TIME :', F8.2, /, ' STATION           RAINFALL      POT. EVAP.(MM/HR)')
9140  FORMAT(4X, I2, 9X, F10.3, 9X, F10.3)

   END SUBROUTINE METIN

END MODULE met_input

