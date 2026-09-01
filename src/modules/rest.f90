!> @brief Miscellaneous run-control, meteorological input, and water-balance routines.
!>
!> `rest` collects legacy routines that do not naturally belong to one of the
!> process-specific modules. [[extra_output]] writes the end-of-run error
!> summary and spatially averaged water-balance totals to the `.pri` output.
!> [[balwat]] maintains the per-column/link cumulative water-balance
!> diagnostic `WBERR`. [[metin]] reads or interpolates the meteorological
!> forcing (precipitation, potential evaporation, radiation, wind,
!> temperature, and time-varying vegetation/canopy parameters) needed as the
!> simulation clock advances. [[tmstep]] computes the next model timestep,
!> subject to soft-start, snowmelt, meteorological record-boundary, and
!> runtime-error-driven limits, and calls [[metin]] to keep forcing data
!> current for the chosen step.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2005-01-25 | SB | - | Added spatially averaged cumulative-flux and end-of-run storage summary output to `extra_output`. |
!> | 2008-12 | JE | 4.3.5F90 | Created during Fortran 90 conversion, to collect `.F` routines without another natural module home. |
!> | 2020-07-07 | SB | - | Added timestep reduction in `TMSTEP` after selected flow errors (1024, 1030, 1060). |
!> | 2026-03-19 | SB | 4.6.1 | Added optional date-aware (ISO 8601) precipitation, potential-evaporation, and max/min-temperature file input (`BMETDATES`) to `METIN` and `TMSTEP`. |
!> | 2026-04-05 | SvB | 4.6.1 | Replaced the `ALINIT` call in `BALWAT` with a direct array-slice assignment. |
!> | 2026-04-06 | SvB | 4.6.1 | Replaced `GOTO`-driven control flow in `METIN` and `TMSTEP` with named `DO`/`CYCLE`/`EXIT` constructs. |
!> | 2026-05-10 | SvB | - | Removed interactive "press enter to continue" prompts after fatal read errors in `METIN`/`TMSTEP`; replaced with `ERROR STOP`. |
!> | 2026-08-22 | SvB | - | Added `READ_DATED_RECORD`, which reads dated meteorological records through a buffer sized to the record instead of a fixed 100000-character line. |
!> @endhistory
MODULE rest
   USE SGLOBAL
!USE SGLOBAL,    ONLY : NELEE, NVEE
   USE AL_G,    ONLY : icmref
   USE AL_C,    ONLY : ARXL, CWIDTH, CLAI,DELTAZ, DTUZ, EEVAP, ERUZ, tih, &
      NLYRBT, NV, &
      PLAI, PNETTO, QVSBF, QVSWEL,  QBKF, QOC, QVSH, UZNEXT, VSTHE, WBERR
   USE AL_D,    ONLY :  flerrc, balanc, syerrc, cmerrc, nstep, carea, DTMET2, BHOTRD, &
      BHOTTI, EPD, NM, PRD, NRAIN, DTMET3, PE, DTMET, MED, RN, OBSPE, &
      U, TA, VPD, TMAX, VHT, TIMEUZ, SD, PALFA, BEXSM, PMAX, precip_m_per_s, NRAINC, &
      tah, tal, ista
   USE ETmod,    ONLY : MODECS, CSTCAP, RELCST, TIMCST, NCTCST, CSTCA1, MODEPL, RELPLA, TIMPLA, NCTPLA, &
      PLAI1, MODECL, RELCLA, TIMCLA, NCTCLA, CLAI1, MODEVH, RELVHT, TIMVHT, NCTVHT, &
      VHT1, BMETP, BMETAL, BMETDATES, MEASPE, del
   USE FRmod,    ONLY : BSOFT
   USE UTILSMOD, ONLY : HOUR_FROM_DATE, TERPO1
   USE mod_error, ONLY : RAISE_ERROR, ERRLVL_fatal, FID_logfile, ERR_STOP
   USE OCmod2,   ONLY : GETHRF
   USE MOD_PARAMETERS, ONLY : LENGTH_LINEVERYLONG, LENGTH_TEXT_R8P
!USE PERTURBATIONS, ONLY : GETSPACETIME1
   IMPLICIT NONE

   LOGICAL :: FIRST_balwat=.TRUE. !! `.TRUE.` until `BALWAT` has initialised `STORW_balwat` and `WBERR` on its first call.
   DOUBLEPRECISION :: STORW_balwat(NELEE)=zero !! Water storage depth for each element/link at the previous `BALWAT` call (m).
   DOUBLEPRECISION :: pinp(nvee+10)=zero !! Current precipitation input by rain station, used by `METIN` and `TMSTEP` (mm/hr).
   DOUBLEPRECISION :: METIME=zero !! End time of the current precipitation/full-meteorological record window (h).
   DOUBLEPRECISION :: MELAST=zero !! Start time of the current precipitation/full-meteorological record window (h).
   DOUBLEPRECISION :: EPTIME=zero !! End time of the current potential-evaporation record window (h).

   ! Dated meteorological record buffer, see READ_DATED_RECORD -----------------
   INTEGER, PARAMETER :: RECORD_HEADROOM = 10 !! Characters kept free at the end of `MET_RECORD`; a record reaching into them is treated as too long for the buffer.
   INTEGER, PARAMETER :: IOSTAGE_NONE   = 0 !! `READ_DATED_RECORD` completed without an error.
   INTEGER, PARAMETER :: IOSTAGE_RECORD = 1 !! `READ_DATED_RECORD` failed while reading the timestamp and record text.
   INTEGER, PARAMETER :: IOSTAGE_VALUES = 2 !! `READ_DATED_RECORD` failed while parsing the values of the record.
   INTEGER, PARAMETER :: IOS_SHORT_RECORD = 1 !! `IOS` reported by `READ_DATED_RECORD` for a record holding fewer values than expected.

   CHARACTER(LEN=:), ALLOCATABLE :: MET_RECORD !! Reusable buffer holding the value part of the dated meteorological record currently being read.
   LOGICAL :: MET_RECORD_SIZED = .FALSE. !! `.TRUE.` once `MET_RECORD` has been resized from its initial capacity to fit the first data line read.

   PRIVATE

   PUBLIC :: BALWAT, TMSTEP, EXTRA_OUTPUT, &
      metime, melast, eptime, pinp


CONTAINS

!> Writes end-of-run error counts and spatially averaged water-balance summaries.
!>
!> `extra_output` is called once after the simulation loop completes. It
!> prints the `FLERRC`/`SYERRC`/`CMERRC` flow, sediment, and contaminant error
!> counters (indices 0-100, offset by 1000/2000/3000 respectively for the
!> printed error number), the normal-completion line to standard output, and
!> catchment-averaged cumulative-flux and end-of-run storage totals to the
!> `.pri` output, using [[al_d]]'s `BALANC` accumulator and `CAREA`:
!>
!> | `BALANC` index | Quantity |
!> |:---------------|:---------|
!> | 7 | Cumulative precipitation |
!> | 8 | Cumulative canopy evaporation |
!> | 9 | Cumulative soil/surface evaporation |
!> | 10 | Cumulative transpiration |
!> | 11 | Cumulative aquifer flow |
!> | 12 | Cumulative discharge |
!> | 13 | Canopy storage |
!> | 14 | Snow storage |
!> | 15 | Subsurface storage |
!> | 16 | Surface storage |
!> | 17 | Channel storage |
!>
!> Each total is printed as `BALANC(i) * 1000 / CAREA` (mm), converting the
!> volume accumulator (m^3) to a depth over the catchment plan area (m^2).
!>
!> @note
!> As documented on [[al_d]], no current routine assigns `FLERRC`, `SYERRC`,
!> or `CMERRC`; the error-count section of this output is therefore always
!> zero in the current build.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2005-01-25 | SB | - | Added the spatially averaged cumulative-flux and storage summary output. |
!> @endhistory
   SUBROUTINE extra_output()
      INTEGER :: i
      DOUBLEPRECISION    :: car
      WRITE(FID_logfile, 1400)
      DO I = 0, 100
         IF (FLERRC (I) .GT.0) WRITE(FID_logfile, 1500) I + 1000, FLERRC (I)
      END DO
      DO I = 0, 100
         IF (SYERRC (I) .GT.0) WRITE(FID_logfile, 1500) I + 2000, SYERRC (I)
      END DO
      DO I = 0, 100
         IF (CMERRC (I) .GT.0) WRITE(FID_logfile, 1500) I + 3000, CMERRC (I)
      END DO
      WRITE(FID_logfile, 1600)
1400  FORMAT(// 'Error message asummary'/)
1500  FORMAT('No. of occurences of error number ',I4,': ',I6)

1600  FORMAT(/ 'End of error message asummary')
!<<<
      WRITE(FID_logfile, '(////)')
      WRITE(FID_logfile, 9900) UZNOW, NSTEP
!
      WRITE ( *, * )

      WRITE ( *, *) 'Normal completion of SHETRAN run'

!^^^^^sb 250105 mass balnce output
      WRITE(FID_logfile, '(////)')
      WRITE(FID_logfile,  * ) ' Spatially Averaged Totals (mm) over the simulation'
      WRITE(FID_logfile, '(A20,F10.2)') 'Cum Prec = ', balanc (7) * 1000 / &
         carea
      WRITE(FID_logfile, '(A20,F10.2)') 'Cum Can. Evap = ', balanc (8) * 1000 / &
         carea
      car = carea
      WRITE(FID_logfile, '(A20,F10.2)') 'Cum Soil+Sur Evp = ', balanc (9) &
         * 1000 / car
      WRITE(FID_logfile, '(A20,F10.2)') 'Cum Trans = ', balanc (10) * 1000 / &
         carea
      WRITE(FID_logfile, '(A20,F10.2)') 'Cum Aqu. Flow = ', balanc (11) &
         * 1000 / carea

      WRITE(FID_logfile, '(A20,F10.2)') 'Cum Discharge = ', balanc (12) &
         * 1000 / carea
      WRITE(FID_logfile, '(//)')
      WRITE(FID_logfile,  * ) ' Storage totals (mm) at the end of the simulation'
      WRITE(FID_logfile, '(A20,F10.2)') 'Canopy Stor = ', balanc (13) * 1000 / &
         carea
      WRITE(FID_logfile, '(A20,F10.2)') 'Snow Store = ', balanc (14) * 1000 / &
         carea
      WRITE(FID_logfile, '(A20,F10.2)') 'Subsur Stor = ', balanc (15) * 1000 / &
         carea
      WRITE(FID_logfile, '(A20,F10.2)') 'Surface Stor = ', balanc (16) * 1000 / &
         carea
      WRITE(FID_logfile, '(A20,F10.2)') 'Channel Stor = ', balanc (17) * 1000 / &
         carea
9900  FORMAT ('Normal completion of SHETRAN run: ',F10.2, ' hours, ', &
      &        I7,' steps.' /)
   END SUBROUTINE extra_output




!> Updates the cumulative water-balance error [[al_c]]`:WBERR` for each column or link.
!>
!> The routine computes the change in stored surface/subsurface water since
!> the previous call and compares it with the net supplied depth over the
!> last timestep (precipitation, evaporation, subsurface exchange, well flow,
!> overland flow, and lateral subsurface advection). The residual is
!> accumulated in `WBERR` as a diagnostic depth in metres.
!>
!> The stored depth used by the balance is
!>
!> \[
!> S_{iel} =
!> \begin{cases}
!> ARXL_{iel}/CWIDTH_{iel}, & \text{channel links (}ICMREF(iel,1)=3\text{)},\\
!> HRF_{iel}-ZGRUND_{iel}, & \text{otherwise},
!> \end{cases}
!> + \sum_{k=NLYRBT(iel,1)}^{LL} \Delta z_{k,iel}\,\theta_{k,iel},
!> \]
!>
!> where \(\theta\) is `VSTHE` and `HRF` is read through [[ocmod2:gethrf]].
!> The storage change is \(\Delta S = S_{iel}-S^{old}_{iel}\), where
!> \(S^{old}\) is the previous call's `STORW_balwat`. On the first call
!> (`FIRST_balwat`) `WBERR` is initialised to zero and `STORW_balwat` is
!> primed with \(S\), but no residual is added because no previous storage
!> state is available.
!>
!> On subsequent calls the supplied rate depth before timestep conversion is
!>
!> \[
!> I_{iel} =
!> PNETTO_{iel} - EEVAP_{iel} + QVSBF_{iel} - QVSWEL_{iel}
!> - \sum_k ERUZ_{iel,k}
!> + \frac{Q_{adv}}{AREA_{iel}},
!> \]
!>
!> with channel-bank exchange \(Q_{adv} = -QBKF_{iel,1}-QBKF_{iel,2}\) for
!> channel links, and zero otherwise, before the paired face-direction terms
!> are added for \(j=1,2\):
!>
!> \[
!> Q_{adv} \leftarrow Q_{adv}
!> - QOC_{iel,j} + QOC_{iel,j+2}
!> + \sum_k \left(QVSH_{j,k,iel}+QVSH_{j+2,k,iel}\right).
!> \]
!>
!> The timestep input depth is `DEPTHI = I * DTUZ`, and the diagnostic update
!> is
!>
!> \[
!> WBERR_{iel} \leftarrow WBERR_{iel} + \Delta S - DEPTHI .
!> \]
!>
!> @note
!> This routine has no dummy arguments. It reads and updates shared grid,
!> geometry, flow, and water-level state from `SGLOBAL`, `AL_C`, `AL_D`, and
!> `AL_G`, and calls [[ocmod2:gethrf]] for the current surface water level.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Standard header, explicit declarations, extra comments, and first-pass storage initialisation. |
!> | 1995-02-20 | GP | 4.0 | Updated for the VSS module and revised subsurface flow variables. |
!> | 1997-02-17 | RAH | 4.1 | Swapped array subscripts for `QVSH`, `DELTAZ`, and `VSTHE`; renamed local counters. |
!> | 2026-04-05 | SvB | 4.6.1 | Replaced the `ALINIT` call with a direct `WBERR` array-slice assignment and replaced the `GOTO 400` skip on the first call with the `IF (.NOT. FIRST_balwat)` block. |
!> @endhistory
   SUBROUTINE BALWAT
      IMPLICIT NONE

      DOUBLE PRECISION :: DELSTO, DEPTHI, DEPTHS, asum, asumQ
      INTEGER          :: ITYPE, JDUM, CELL, IEL

      !----------------------------------------------------------------------*
      ! Initialization
      ! --------------

      IF (FIRST_balwat) WBERR(1:total_no_elements) = ZERO

      ! Loop Over Columns
      ! -----------------
      DO IEL = 1, total_no_elements
         ITYPE = ICMREF (IEL, 1)

         ! Calculate depth of water stored and change since previous step
         ! --------------------------------------------------------------
         ! * surface
         IF (ITYPE == 3) THEN
            asum = ARXL (IEL) / CWIDTH (IEL)
         ELSE
            asum = GETHRF (IEL) - ZGRUND (IEL)
         END IF

         ! * sub-surface
         DO CELL = NLYRBT (IEL, 1), top_cell_no
            asum = asum + DELTAZ (CELL, IEL) * VSTHE (CELL, IEL)
         END DO

         DEPTHS = asum

         ! * net increase this timestep
         DELSTO = DEPTHS - STORW_balwat (IEL)

         ! * save new value for use next timestep
         STORW_balwat (IEL) = DEPTHS

         ! Calculate net depth of water supplied over the previous step
         ! ------------------------------------------------------------
         ! * ... but only if we have a bona fide value for DELSTO

         IF (.NOT. FIRST_balwat) THEN

            ! * sources and sinks
            asum = PNETTO (IEL) - EEVAP (IEL) + QVSBF (IEL) - QVSWEL (IEL)
            DO CELL = NLYRBT (IEL, 1), top_cell_no
               asum = asum - ERUZ (IEL, CELL)
            END DO

            ! * advection
            IF (ITYPE == 3) THEN
               asumQ = -QBKF (IEL, 1) - QBKF (IEL, 2)
            ELSE
               asumQ = ZERO
            END IF

            DO JDUM = 1, 2
               asumQ = asumQ - QOC (IEL, JDUM) + QOC (IEL, JDUM + 2)
               DO CELL = NLYRBT (IEL, 1), top_cell_no
                  asumQ = asumQ + QVSH (JDUM, CELL, IEL) + QVSH (JDUM + 2, CELL, IEL)
               END DO
            END DO

            asum = asum + asumQ / cellarea (IEL)

            ! * convert from rate to depth
            DEPTHI = asum * DTUZ

            ! Update the cumulative water balance error as a depth
            ! ----------------------------------------------------
            WBERR (IEL) = WBERR (IEL) + DELSTO - DEPTHI

         END IF

      END DO

      ! Epilogue
      ! --------
      FIRST_balwat = .FALSE.

   END SUBROUTINE BALWAT


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
   !> [[utilsmod:hour_from_date]]; the seconds field is consumed but not used.
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
   !> @endhistory
   SUBROUTINE READ_DATED_RECORD (UNIT, NVALUES, DATEHOUR, VALUES, IOS, IOSTAGE)
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
   !----------------------------------------------------------------------*

      IOSTAGE = IOSTAGE_NONE
      NEEDED  = NVALUES * LENGTH_TEXT_R8P + RECORD_HEADROOM

      IF (.NOT. ALLOCATED(MET_RECORD)) THEN
         ! start from the reserved capacity; the first data line sets the real size
         ALLOCATE (CHARACTER(LEN=LENGTH_LINEVERYLONG) :: MET_RECORD)
         MET_RECORD_SIZED = .FALSE.
      ELSE IF (MET_RECORD_SIZED .AND. LEN(MET_RECORD) < NEEDED) THEN
         ! a wider file than the one that sized the buffer
         CALL RESIZE_MET_RECORD (MIN(NEEDED, LENGTH_LINEVERYLONG))
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

         CALL RESIZE_MET_RECORD (MIN(2 * LEN(MET_RECORD), LENGTH_LINEVERYLONG))
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
         CALL RESIZE_MET_RECORD (MIN(MAX(NEEDED, TRIMMED + RECORD_HEADROOM), LENGTH_LINEVERYLONG))
         MET_RECORD_SIZED = .TRUE.
      END IF

      RETURN

      ! FORMAT STATEMENTS
9000  FORMAT (I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2,1X,A)
9010  FORMAT (A, I0, A, I0, A)

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
   !> @endhistory
   SUBROUTINE RESIZE_MET_RECORD (CAPACITY)
      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN) :: CAPACITY !! New buffer length in characters.
   !----------------------------------------------------------------------*

      IF (ALLOCATED(MET_RECORD)) THEN
         IF (LEN(MET_RECORD) == CAPACITY) RETURN
         DEALLOCATE (MET_RECORD)
      END IF

      ALLOCATE (CHARACTER(LEN=CAPACITY) :: MET_RECORD)

   END SUBROUTINE RESIZE_MET_RECORD


   !> Reads or interpolates meteorological forcing required by ET, interception, and snowmelt.
   !>
   !> `METIN` advances precipitation, potential evaporation, radiation, wind,
   !> temperature, vapour pressure deficit, and (via [[utilsmod:terpo1]]) the
   !> current time-varying canopy-storage-capacity, plant/land-cover leaf-area,
   !> and vegetation-height values in [[etmod]] needed for the current
   !> simulation time. In date-aware mode, [[tmstep]] first checks and positions
   !> the dated forcing files; `METIN` then consumes the selected records and
   !> converts their ISO-8601-like date fields to SHETRAN hours using
   !> [[utilsmod:hour_from_date]].
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
   !> in [[etmod]] (`MODECS`, `MODEPL`, `MODECL`, `MODEVH`) by calling
   !> [[utilsmod:terpo1]] at the current `TIMEUZ` (see [[al_d]]) for canopy-storage
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
   !> | 2026-08-22 | SvB | - | Moved dated record reading into [[rest:read_dated_record]], replacing the fixed 100000-character line buffer. |
   !> @endhistory
   SUBROUTINE METIN (IFLAG)
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
                  CALL READ_DATED_RECORD (prd, NRAIN, prddate, PINP, ios, iostage)

                  IF (ios > 0) THEN
                     IF (iostage == IOSTAGE_RECORD) THEN
                        WRITE (*, 9020) ' Error reading the precipitation time series file. ' // &
                           'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ', &
                           NRAIN, ' values on each row'
                     ELSE
                        WRITE (*, 9020) ' Error reading the precipitation time series file. ' // &
                           'This should have the date in the iso 8601 format followed by ', NRAIN, ' values'
                     END IF
                     CALL ERR_STOP(255)
                  END IF

                  IF (ios < 0) THEN
                     IF (FIRSTNOPRD) THEN
                        WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of prd data', 'All remaining values will be zero'
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
                        WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of prd data', 'All remaining values will be zero'
                        FIRSTNOPRD = .FALSE.
                     END IF
                     PINP(1:NRAIN) = ZERO
                  END IF
               END IF

               PINP(1:NRAIN) = PINP(1:NRAIN) / dtmet2
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
                     CALL READ_DATED_RECORD (epd, NM, epddate, PEIN, ios, iostage)

                     IF (ios > 0) THEN
                        IF (iostage == IOSTAGE_RECORD) THEN
                           WRITE (*, 9020) ' Error reading the potential evaporation time series file. ' // &
                              'This should have the date in iso 8601 format followed by ', NM, ' values on each row'
                        ELSE
                           WRITE (*, 9022) ' Error reading potential evap data values from line.'
                        END IF
                        CALL ERR_STOP(255)
                     END IF

                     IF (ios < 0) THEN
                        IF (FIRSTNOEPD2) THEN
                           WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                           FIRSTNOEPD2 = .FALSE.
                        END IF
                        PEIN(1:NM) = ZERO
                     END IF

                     IF (ISTA) THEN
                        CALL READ_DATED_RECORD (TAH, NM, tahdate, TAHIGH, ios, iostage)
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
                        CALL READ_DATED_RECORD (TAL, NM, taldate, TALOW, ios, iostage)
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

                     PEIN(1:NM) = PEIN(1:NM) / dtmet3
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
                           WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                           FIRSTNOEPD1 = .FALSE.
                        END IF
                        PEIN(1:NM) = ZERO
                     END IF

                     IF (ISTA) THEN
                        READ (TAH, *, IOSTAT=ios) TAHIGH(1:NM)
                        IF (ios > 0) STOP 'Error reading max temp file'
                        IF (ios < 0) TAHIGH(1:NM) = 10.0d0
                     END IF

                     IF (ISTA) THEN
                        READ (TAL, *, IOSTAT=ios) TALOW(1:NM)
                        IF (ios > 0) STOP 'Error reading min temp file'
                        IF (ios < 0) TALOW(1:NM) = 10.0d0
                     END IF

                     PEIN(1:NM) = PEIN(1:NM) / dtmet3
                     EPLAST = EPTIME
                     EPTIME = EPTIME + dtmet3

                     IF (.NOT. (BHOTRD .AND. EPTIME < BHOTTI)) EXIT hotstart_epd_loop
                  END IF
               END DO hotstart_epd_loop
            END IF
            ! end of check for hotstarted run

            ! calculate average PE value over computational timestep
            TEND = MIN(UZNOW + UZNEXT, EPTIME)
            PETOT(1:NM) = (TEND - UZNOW) * PEIN(1:NM)

            ! POT. EVAP and TEMPERATURE DATA READ PART 2
            ! check if it is time to read in PET data
            IF (EPTIME < UZNOW + UZNEXT) THEN
               pet_read_loop: DO
                  ! epd and temperature files have dates
                  IF (BMETDATES) THEN
                     CALL READ_DATED_RECORD (epd, NM, epddate, PEIN, ios, iostage)

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
                           WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                           FIRSTNOEPD2 = .FALSE.
                        END IF
                        PEIN(1:NM) = ZERO
                     END IF

                     IF (ISTA) THEN
                        CALL READ_DATED_RECORD (TAH, NM, tahdate, TAHIGH, ios, iostage)
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
                        CALL READ_DATED_RECORD (TAL, NM, taldate, TALOW, ios, iostage)
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

                     PEIN(1:NM) = PEIN(1:NM) / dtmet3
                     EPLAST = EPTIME
                     EPTIME = EPTIME + dtmet3
                     TEND = MIN(UZNOW + UZNEXT, EPTIME)
                     PETOT(1:NM) = PETOT(1:NM) + (TEND - EPLAST) * PEIN(1:NM)

                     IF (.NOT. (EPTIME < UZNOW + UZNEXT)) EXIT pet_read_loop

                  ! epd and temperature files DO NOT have dates
                  ELSE
                     READ (EPD, *, IOSTAT=ios) PEIN(1:NM)
                     IF (ios > 0) STOP 'Error reading PET file'

                     IF (ios < 0) THEN
                        IF (FIRSTNOEPD2) THEN
                           WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                           FIRSTNOEPD2 = .FALSE.
                        END IF
                        PEIN(1:NM) = ZERO
                     END IF

                     IF (ISTA) THEN
                        READ (TAH, *, IOSTAT=ios) TAHIGH(1:NM)
                        IF (ios > 0) STOP 'Error reading max temp file'
                        IF (ios < 0) TAHIGH(1:NM) = 10.0d0
                     END IF

                     IF (ISTA) THEN
                        READ (TAL, *, IOSTAT=ios) TALOW(1:NM)
                        IF (ios > 0) STOP 'Error reading min temp file'
                        IF (ios < 0) TALOW(1:NM) = 10.0d0
                     END IF

                     PEIN(1:NM) = PEIN(1:NM) / dtmet3
                     EPLAST = EPTIME
                     EPTIME = EPTIME + dtmet3
                     TEND = MIN(UZNOW + UZNEXT, EPTIME)
                     PETOT(1:NM) = PETOT(1:NM) + (TEND - EPLAST) * PEIN(1:NM)

                     IF (.NOT. (EPTIME < UZNOW + UZNEXT)) EXIT pet_read_loop
                  END IF
               END DO pet_read_loop
            END IF

            OBSPE(1:NM) = PETOT(1:NM) / UZNEXT / 3600.0d0
            ! for simplicity the temperature used is the value at the end of the timestep
            TA(1:NM) = (TAHIGH(1:NM) + TALOW(1:NM)) / 2.0d0

         END IF

         ! PRINT OUT INPUT DATA
         IF (BMETP) THEN
            WRITE(FID_logfile, 9130) METIME
            DO I = 1, NM
               WRITE(FID_logfile, 9140) I, PINP(I), PEIN(I)
            END DO
         END IF

      ELSE
         ! READ ALL MET. DATA IN FIXED TIME INTERVAL (USUALLY HOURLY) FORMAT
         !------------------------------------------------------------------
         IF (IFLAG == 2) RETURN

         IF (NRAIN == NM) THEN
            !-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE EQUAL
            IF (BMETP) WRITE(FID_logfile, 9100)

            !-----LOOP ON NUMBER OF MET SITES
            read_equal_loop: DO
               MELAST = METIME
               METIME = METIME + DTMET

               DO I = 1, NM
                  READ (MED, 9030, IOSTAT=ios) ISITE, NN, PINP(I), RN(I), U(I), PA(I), TA(I), DEL(I), VPD(I), IDATA

                  IF (ios < 0) THEN
                     IF (FIRSTNOMET1) THEN
                        WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
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

                  IF (BMETP) WRITE(FID_logfile, 9040) ISITE, METIME, PINP(I), RN(I), U(I), TA(I), DEL(I), VPD(I)

                  IF (MEASPE(I) == 0) CYCLE

                  ! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
                  READ (MED, 9050, IOSTAT=ios) OBSPE(I)
                  IF (ios < 0) THEN
                     IF (FIRSTNOMET2) THEN
                        WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
                        FIRSTNOMET2 = .FALSE.
                     END IF
                     OBSPE(I) = 0.0d0
                  END IF

                  ! CONVERT TO MM/S
                  OBSPE(I) = OBSPE(I) / 3600.0d0
               END DO

               ! READ TO START SIMULATION TIME, IF HOTSTART
               IF (.NOT. (BHOTRD .AND. METIME < BHOTTI)) EXIT read_equal_loop
            END DO read_equal_loop

         ELSE
            !-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE UNEQUAL
            IF (BMETP) WRITE(FID_logfile, 9110)

            !-----LOOP ON NUMBER OF MET SITES
            read_unequal_loop: DO
               MELAST = METIME
               METIME = METIME + DTMET

               DO I = 1, NM
                  READ (MED, 9060, IOSTAT=ios) ISITE, NN, RN(I), U(I), PA(I), TA(I), DEL(I), VPD(I), IDATA

                  IF (ios < 0) THEN
                     IF (FIRSTNOMET3) THEN
                        WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
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

                  IF (BMETP) WRITE(FID_logfile, 9070) ISITE, METIME, RN(I), U(I), TA(I), DEL(I), VPD(I)

                  IF (MEASPE(I) == 0) CYCLE

                  ! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
                  READ (MED, 9050, IOSTAT=ios) OBSPE(I)
                  IF (ios < 0) THEN
                     IF (FIRSTNOMET4) THEN
                        WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
                        FIRSTNOMET4 = .FALSE.
                     END IF
                     OBSPE(I) = 0.0d0
                  END IF

                  ! CONVERT TO MM/S
                  OBSPE(I) = OBSPE(I) / 3600.0d0
               END DO

               IF (BMETP) WRITE(FID_logfile, 9120)

               !-----LOOP ON NUMBER OF RAIN SITES
               DO I = 1, NRAIN
                  READ (MED, 9080, IOSTAT=ios) ISITE, NN, PINP(I), IDATA
                  IF (ios < 0) THEN
                     IF (FIRSTNOMET5) THEN
                        WRITE(FID_logfile, 9010) 'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
                        FIRSTNOMET5 = .FALSE.
                     END IF
                     PINP(I) = 0.0d0
                  END IF

                  IF (BMETP) WRITE(FID_logfile, 9090) ISITE, METIME, PINP(I)
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
9010  FORMAT (///, A6, G12.4, A8, /, A18, /, A33, ///)
9020  FORMAT (A, I0, A)
9022  FORMAT (A)
9030  FORMAT (2I6, 4G12.6, /, 12X, 3G12.6, I12)
9040  FORMAT ('0', 8X, I6, F8.2, 5X, 2(3F12.6,'  NOT_USED  '))
9050  FORMAT (12X, G12.6)
9060  FORMAT (2I6, 12X, 3G12.6, /, 12X, 3G12.6, I12)
9070  FORMAT ('0', 8X, I6, F8.2, 5X, 2(2F12.6,'  NOT_USED  ':F12.6))
9080  FORMAT (2I6, G12.6, 24X, I12)
9090  FORMAT ('0', 9X, I6, F8.2, 5X, F12.6, '  NOT_USED  ')
9100  FORMAT (//, 1X, 'MET DATA - SITE    TIME      RAINFALL    NET RADN', 4X, &
              'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
9110  FORMAT (//, 1X, 'MET DATA - SITE    TIME      NET RADN', 4X, &
              'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
9120  FORMAT (//, 1X, 'RAIN DATA - SITE    TIME      RAINFALL         IDATA')
9130  FORMAT (//, 1X, 'MET DATA -  TIME :', F8.2, /, ' STATION           RAINFALL      POT. EVAP.(MM/HR)')
9140  FORMAT (4X, I2, 9X, F10.3, 9X, F10.3)

   END SUBROUTINE METIN



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

      IF (BSOFT .AND. NSTEP <= 102) TSOFT = TMAX * 0.05d0 * 1.03d0**NSTEP

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
      UZNEXT = MIN(UZNEXT * (1.0d0 + PALFA), TSOFT, TSNOW)

      ! SB 07072020 reduce timestep if there are errors 1024,1030,1060
      IF (flag_runtime_reduction_e1060) THEN
         UZNEXT = MAX(0.0003d0, UZNEXT / 10.0d0)
      ELSEIF (flag_runtime_reduction_errors) THEN
         UZNEXT = MAX(0.0003d0, UZNEXT / 100.0d0)
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
         READ(prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            prdyear, prdmonth, prdday, prdhour, prdminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the precipitation time series file. ' // &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            CALL ERR_STOP(255)
         END IF

         BACKSPACE(prd)
         prddate = HOUR_FROM_DATE(prdyear, prdmonth, prdday, prdhour, prdminute)

         ! check simulation start time plus precipitation time step length plus 0.01
         ! is greater than or equal to the first precipitation time series date.
         ! The 0.01 values is a bit arbitrary
         IF (tih + dtmet2 + 0.01d0 < prddate) THEN
            WRITE (*, '(A)') ' The precipitation data starts after the simulation start date. ' // &
               'Check the precipitation data dates and the start time of the simulation'
            CALL ERR_STOP(255)
         END IF
      END IF

      IF (BMETDATES .AND. EPDFIRST1) THEN
         EPDFIRST1 = .FALSE.
         READ(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            epdyear, epdmonth, epdday, epdhour, epdminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the potential evaporation time series file. ' // &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            CALL ERR_STOP(255)
         END IF

         BACKSPACE(epd)
         epddate = HOUR_FROM_DATE(epdyear, epdmonth, epdday, epdhour, epdminute)

         IF (tih + dtmet3 + 0.01d0 < epddate) THEN
            WRITE (*, '(A)') ' The potential evaporation data starts after the simulation start date. ' // &
               'Check the potential evaporation data dates and the start time of the simulation'
            CALL ERR_STOP(255)
         END IF
      END IF

      IF (BMETDATES .AND. TAHFIRST1 .AND. ISTA) THEN
         TAHFIRST1 = .FALSE.
         READ(tah, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            tahyear, tahmonth, tahday, tahhour, tahminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the maximum temperature time series file. ' // &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            CALL ERR_STOP(255)
         END IF

         BACKSPACE(tah)
         tahdate = HOUR_FROM_DATE(tahyear, tahmonth, tahday, tahhour, tahminute)

         IF (tih + dtmet3 + 0.01d0 < tahdate) THEN
            WRITE (*, '(A)') ' The maximum temperature data starts after the simulation start date. ' // &
               'Check the maximum temperature dates and the start time of the simulation'
            CALL ERR_STOP(255)
         END IF
      END IF

      IF (BMETDATES .AND. TALFIRST1 .AND. ISTA) THEN
         TALFIRST1 = .FALSE.
         READ(tal, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            talyear, talmonth, talday, talhour, talminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the minimum temperature time series file. ' // &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            CALL ERR_STOP(255)
         END IF

         BACKSPACE(tal)
         taldate = HOUR_FROM_DATE(talyear, talmonth, talday, talhour, talminute)

         IF (tih + dtmet3 + 0.01d0 < taldate) THEN
            WRITE (*, '(A)') ' The minimum temperature data starts after the simulation start date. ' // &
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
            READ(prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               prdyear, prdmonth, prdday, prdhour, prdminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the precipitation time series file. ' // &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
               WRITE (*, '(A)') ' Check the format of the precipitation time series file ' // &
                  'and the end date is not before the start date of the simulation'
               CALL ERR_STOP(255)
            END IF

            prddate = HOUR_FROM_DATE(prdyear, prdmonth, prdday, prdhour, prdminute)
            ! use the precipitation at this step if it is within 0.01 hour of the start date.
            ! Otherwise use the next precipitation file. The 0.01 values is a bit arbitrary
            IF (prddate + 0.01d0 > tih) THEN
               PRDFIRST = .FALSE.
               BACKSPACE(prd)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. EPDFIRST) THEN
         DO
            READ(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               epdyear, epdmonth, epdday, epdhour, epdminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the potential evaporation time series file. ' // &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the potential evaporation time series file ' // &
                  'and the end date is not before the start date of the simulation'
               CALL ERR_STOP(255)
            END IF

            epddate = HOUR_FROM_DATE(epdyear, epdmonth, epdday, epdhour, epdminute)
            IF (epddate + 0.01d0 > tih) THEN
               EPDFIRST = .FALSE.
               BACKSPACE(epd)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. TAHFIRST .AND. ISTA) THEN
         DO
            READ(tah, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               tahyear, tahmonth, tahday, tahhour, tahminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the maximum temperature time series file. ' // &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the maximum daily temperature time series file ' // &
                  'and the end date is not before the start date of the simulation'
               CALL ERR_STOP(255)
            END IF

            tahdate = HOUR_FROM_DATE(tahyear, tahmonth, tahday, tahhour, tahminute)
            IF (tahdate + 0.01d0 > tih) THEN
               TAHFIRST = .FALSE.
               BACKSPACE(tah)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. TALFIRST .AND. ISTA) THEN
         DO
            READ(tal, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               talyear, talmonth, talday, talhour, talminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the minimum daily temperature time series file. ' // &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the minimum daily temperature time series file ' // &
                  'and the end date is not before the start date of the simulation'
               CALL ERR_STOP(255)
            END IF

            taldate = HOUR_FROM_DATE(talyear, talmonth, talday, talhour, talminute)
            IF (taldate + 0.01d0 > tih) THEN
               TALFIRST = .FALSE.
               BACKSPACE(tal)
               EXIT
            END IF
         END DO
      END IF

! set period of validity of current data
      EXITT = .FALSE.

      timestep_reduction_loop: DO
         TEND = MIN(UZNOW + UZNEXT, METIME)

         ! store first period of precipitation using array slicing
         PTOT(1:NRAIN) = (TEND - UZNOW) * PINP(1:NRAIN)

         IF (EXITT) EXIT timestep_reduction_loop

         ! test if timestep reduction required without reading any prec. data
         DO I = 1, NRAIN
            IF (PTOT(I) > PMAX) THEN
               EXITT = .TRUE.
               UZNEXT = MIN(UZNEXT, PMAX / PINP(I))
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
            IF (PTOT(I) + (METIME - MELAST) * PINP(I) > PMAX) THEN
               EXITT = .TRUE.
               UZTEST = MELAST - UZNOW + (PMAX - PTOT(I)) / PINP(I)
               UZNEXT = MIN(UZNEXT, UZTEST)
            END IF
         END DO

         TEND = MIN(UZNOW + UZNEXT, METIME)

         ! Accumulate using array slicing
         PTOT(1:NRAIN) = PTOT(1:NRAIN) + (TEND - MELAST) * PINP(1:NRAIN)
      END DO meteorological_loop

! check for invalid timestep (could be a result of data errors)
      IF (UZNEXT < 5.0D-5) THEN
         WRITE(FID_logfile, "(////'UZNEXT = ',G14.6, /' TSOFT = ',G14.6, /'MELAST = ',G14.6, " // &
            "/'METIME = ',G14.6 /, 'PREC.STN.   PINP        PTOT'/)") &
            UZNEXT, TSOFT, MELAST, METIME
         WRITE(FID_logfile, "(4X,I4,2G14.6)") (I, PINP(I), PTOT(I), I = 1, NRAIN)
         CALL RAISE_ERROR(ERRLVL_fatal, 1025, FID_logfile, 0, 0, 'INVALID TIMESTEP')
      END IF

      ! calculate average value over timestep (& convert mm/h to m/s)
      DO IEL = 1, total_no_elements
         precip_m_per_s(IEL) = PTOT(NRAINC(IEL)) / UZNEXT / 3.6E6
      END DO

      ! read in breakpoint PE for this timestep (if required)
      IFLAG = 2
      CALL METIN(IFLAG)

   END SUBROUTINE TMSTEP

END MODULE rest
