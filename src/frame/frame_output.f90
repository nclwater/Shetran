!> summary: The regular text and CSV output series, and the print-file selectors.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[FROUTPUT]] is the per-timestep output routine: it samples the current
!> state, accumulates it over the output interval, and writes the discharge,
!> mass-balance, water-table, sediment and contaminant text series at the
!> configured times. Its work is split across 20 contained helpers — the
!> phases, the sampling, the accumulation, the formatting and the I/O — which
!> travel inside it.
!>
!> The `BP*` flags are the print-file selectors read from the frame input, and
!> [[frame_setup]] reads them.
!>
!> @warning
!> [[FROUTPUT]] declares saved local variables named `next_hour`, `qoctot`,
!> `uzold`, `sedtot`, `sedfinetot` and `contamtot`. They shadow the same-named
!> public module variables imported by [[simulation_driver]] for automatic
!> differentiation. Consequently the public copies retain their
!> declaration-time values while output uses the local copies; only
!> module-level `icounter2` is updated by the current output path. The private
!> module `hour_now`, `uznowt` and `qoctotextra` are likewise shadowed and
!> unused.
!>
!> [[write_dis]] has no caller in the current source.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed and standardised the FR frame, including impermeable-bed defaults, `BSOFT`, `TIM` migration to `AL_D`, result output, and hot-start/rescue handling. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the FR `.F` files into a single Fortran 90 module. |
!> | 2020-05 | SB | 4.5 | Added ZQ-module variables and support. |
!> | 2026-03 | SB | 4.6 | Added allocation-based initialisation, date-aware meteorological input, the outlet sediment/contaminant text series and the water-table output. |
!> | 2026-05-03 | SvB | 4.6.1 | Decomposed `FROUTPUT` into phase, sampling, accumulation, formatting, and I/O helpers without changing its output contracts. |
!> | 2026-09-11 | SvB | - | Split out of FRmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE frame_output

   USE stdlib_system, ONLY: join_path
   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, RHO_SEDIMENT
   USE element_geometry, ONLY: CAREA, top_cell_no, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF
   USE channel_geometry, ONLY: BEXBK
   USE run_context, ONLY: cnam, DIRQQ
   USE run_control, ONLY: BEXCM, BEXSY, BHOTRD, BHOTTI, isextradis, isextrapsl, TOUTPUT
   USE simulation_clock, ONLY: TIH, UZNOW
   USE file_units, ONLY: DIS, DIS2, disextra, FID_logfile, MAS, pslextra, VSE
   USE water_balance, ONLY: BALANC, MBFACE, MBLINK
   USE vs_state, ONLY: NLYRBT, VSPSI, ZVSPSL
   USE oc_state, ONLY: QOC
   USE oc_boundaries, ONLY: NOCBCC, NOCBCD
   USE sy_state, ONLY: NSED, QSED
   USE cm_parameters, ONLY: CCCC
   USE datetime, ONLY: date_from_hour
   USE error_reporting, ONLY: RAISE_ERROR, ERR_STOP, ERRLVL_fatal
   USE error_status, ONLY: errstat_alloc, errstat_dealloc, errstat_fileopen

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: FROUTPUT
   PUBLIC :: BPPNET, BPEPOT, BPQOC, BPDEP, BPQF, BPQH, BPQSZ, BPHSZ, BPBAL, BPSD
   PUBLIC :: next_hour, icounter2, qoctot, uzold, sedtot, sedfinetot, contamtot

   LOGICAL :: BPPNET !! Print net precipitation arrays.
   LOGICAL :: BPEPOT !! Print potential-evaporation arrays.
   LOGICAL :: BPQOC  !! Print overland/channel discharge arrays.
   LOGICAL :: BPDEP  !! Print surface-water-depth arrays.
   LOGICAL :: BPQF   !! Print river-level and river-flow arrays.
   LOGICAL :: BPQH   !! Print infiltration arrays.
   LOGICAL :: BPQSZ  !! Print saturated-zone flow arrays.
   LOGICAL :: BPHSZ  !! Print phreatic-surface-level arrays.
   LOGICAL :: BPBAL  !! Print water-balance arrays.
   LOGICAL :: BPSD   !! Print snow-depth arrays.
   INTEGER, SAVE   :: next_hour = 1     !! AD-exported compatibility copy; shadowed by [[froutput]] and remains 1.
   INTEGER, SAVE   :: icounter2 = 0     !! Next whole-day mass-balance output threshold (h).
   INTEGER         :: hour_now          !! Unused module copy shadowed by [[froutput]].
   DOUBLEPRECISION :: qoctot = 0.0d0    !! AD-exported compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION :: uzold = 0.0d0     !! AD-exported compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION :: uznowt            !! Unused module copy shadowed by [[froutput]].
   DOUBLEPRECISION :: sedtot = 0.0d0    !! Public compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION :: sedfinetot = 0.0d0 !! Public compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION :: contamtot = 0.0d0 !! Public compatibility copy shadowed by [[froutput]].
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: qoctotextra
   !! Unused private module copy shadowed by [[froutput]].


CONTAINS

!> @brief Manages additional text time-series output.
!>
!> The routine handles start, timestep, and final-state phases for CSV-style discharge,
!> extra discharge stations, water-table depth, sediment, fine sediment, and
!> contaminant outlet series. It keeps running totals between calls and formats
!> time using `DATE_FROM_HOUR`.
!>
!> `SIMPOS` selects the phase:
!>
!> | `SIMPOS` value | Behaviour |
!> |:---------------|:----------|
!> | `start` | Read optional extra-output control files, open CSV/text outputs, find the outlet link/face, initialise averaging counters. |
!> | starts with `main` | Accumulate current timestep values, write regular output when a `TOUTPUT` interval boundary is crossed, write every-timestep discharge through [[write_dis2]], and write daily mass-balance/optional water-table rows. |
!> | any other value | Write end-of-simulation phreatic-surface and pressure-head output to `VSE` for use as VSI-style initial conditions. |
!>
!> | Contained helper group | Routines |
!> |:-----------------------|:---------|
!> | Start/setup | `initialise_output`, `initialise_extra_discharge_points`, `allocate_extra_discharge`, `initialise_extra_water_table_output`, `find_mass_balance_outlet`, `write_discharge_header`, `initialise_sediment_output`, `initialise_contaminant_output` |
!> | Timestep sampling/averaging | `write_main_output`, `sample_current_values`, `accumulate_interval`, `write_completed_regular_outputs`, `restart_accumulators` |
!> | Formatting/final state | `write_regular_outputs`, `timestamp_from_output_hour`, `write_periodic_mass_balance`, `write_final_state` |
!> | I/O checks | `write_checked`, `stop_on_io_error`, `fatal_on_io_error` |
!>
!> FORD exposes those contained routines on the source page rather than as
!> separate procedure pages; each still has an adjacent source header below.
!>
!> Opened or written outputs are:
!>
!> | Output | Unit/source | Contents |
!> |:-------|:------------|:---------|
!> | `DIS2` | rundata unit 44 | Every-model-timestep outlet discharge with absolute date/time. |
!> | `MAS` | rundata unit 43 | Daily spatially averaged cumulative balance and storage terms in mm over `CAREA`. |
!> | `DIS` | rundata unit 41 | Regular `TOUTPUT`-interval outlet discharge, with optional extra channels. |
!> | `output_<catchment>_water_table_depth.csv` | local unit 683 when `ISEXTRAPSL` | Selected element water-table depth below ground; negative means surface-water depth. |
!> | `output_<catchment>_sediment_all.csv` and `_sediment_fine.csv` | local units 681/682 when `BEXSY` | Outlet sediment discharge for all fractions and fraction 1. |
!> | `output_<catchment>_contaminant.csv` | local unit 684 when `BEXCM` | Outlet relative concentration for contaminant 1. |
!>
!> @warning
!> Two existing write-error messages call `DIS2` unit 41 and `DIS` unit 44.
!> The actual `AL_D` parameter assignments, and therefore the files written,
!> are `DIS=41` and `DIS2=44` as shown above.
!> @endwarning
!>
!> Extra discharge points are read from `DISEXTRA` as `(element, face)` pairs and
!> silently filtered when the element number exceeds `total_no_links`. Extra
!> water-table output elements are read from `PSLEXTRA` and filtered when the
!> element number exceeds `total_no_elements`.
!>
!> @warning
!> Optional-point validation checks only those upper element/link bounds. Zero
!> or negative identifiers, discharge faces outside 1:4, and negative requested
!> counts are not rejected here and can fail during allocation or later indexing.
!> @endwarning
!>
!> When result-file output has not provided `MBLINK`/`MBFACE`, the `start` phase
!> scans channel links and selects the last external OC boundary with boundary
!> type 7, i.e. a weir boundary. If no outlet is found, outlet discharge,
!> sediment, and contaminant series use zero values.
!>
!> Regular discharge, sediment, and contaminant records are accumulated in
!> normalised output time `UZNOW/TOUTPUT`. The value written for `outputhour =
!> next_hour-1` is the mean over the preceding `TOUTPUT` interval and is dated at
!> the interval start. If one model step crosses more than one output interval,
!> intermediate intervals are filled with the current timestep value.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2005-2024 | SB | 4.x | Added every-step and regular discharge, mass-balance, virtual-station, water-table, sediment, and contaminant text output. |
!> | 2026-05-03 | SvB | 4.6.1 | Split the monolithic phase logic into contained helpers. |
!> | 2026-09-06 | SvB | - | Checked every output-file `OPEN` through [[error_status:errstat_fileopen]], reporting `IOSTAT`/`IOMSG`. |
!> | 2026-09-07 | SvB | - | Status-checked the remaining bare header/state `WRITE`s via `write_checked` / `stop_on_io_error`. |
!> @endhistory
   SUBROUTINE FROUTPUT(SIMPOS)

      IMPLICIT NONE

      ! Dummy arguments
      CHARACTER(LEN=5), INTENT(IN) :: SIMPOS

      ! Parameters
      INTEGER, PARAMETER :: SEDALLUNIT = 681
      INTEGER, PARAMETER :: SEDFINEUNIT = 682
      INTEGER, PARAMETER :: PSLFILEUNIT = 683
      INTEGER, PARAMETER :: CONTAMUNIT = 684

      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0
      DOUBLE PRECISION, PARAMETER :: ONE = 1.0D0

      ! Locals: strings
      CHARACTER(LEN=20)  :: disextratext, pslextratext
      CHARACTER(LEN=256) :: filnam
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! IOMSG= text from a failed file OPEN.

      ! Locals: scalars
      INTEGER :: L, iface, nminel, i, j, iel, ios
      INTEGER :: hour_now
      DOUBLE PRECISION :: qocav, qocold
      DOUBLE PRECISION :: sedav, sedfineav, contamav
      DOUBLE PRECISION :: uznowt

      ! Persistent state between calls
      INTEGER, SAVE :: disextrapoints = 0
      INTEGER, SAVE :: pslextrapoints = 0
      DOUBLE PRECISION, SAVE :: uzold = ZERO
      DOUBLE PRECISION, SAVE :: next_hour = ZERO
      DOUBLE PRECISION, SAVE :: qoctot = ZERO
      DOUBLE PRECISION, SAVE :: sedtot = ZERO
      DOUBLE PRECISION, SAVE :: sedfinetot = ZERO
      DOUBLE PRECISION, SAVE :: contamtot = ZERO

      ! Persistent optional-output metadata/state
      INTEGER, ALLOCATABLE, SAVE :: pslextraelement(:)
      INTEGER, ALLOCATABLE, SAVE :: disextraelement(:), disextraface(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: qocavextra(:)

      SELECT CASE (SIMPOS)

      CASE ('start')
         CALL initialise_output()

      CASE DEFAULT
         IF (SIMPOS(1:4) == 'main') THEN
            CALL write_main_output()
         ELSE
            CALL write_final_state()
         END IF

      END SELECT

   CONTAINS

!> @brief Opens and primes the regular and optional runtime output streams.
!>
!> The routine reads optional station lists, writes the `DIS2`, `MAS`, and `DIS`
!> headings, locates the outlet weir, and starts the normalised `TOUTPUT`
!> interval counters. Hot starts seed the previous interval from `BHOTTI`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-04-22 | SB | Added the `DIS2` every-timestep discharge heading line. |
!> | 2026-05-03 | SvB | Extracted the start phase from the monolithic `FROUTPUT` implementation. |
!> @endhistory
      SUBROUTINE initialise_output()
         ! Initialise regular and optional output streams.  The optional point
         ! lists are compacted in-place: invalid element/link IDs are skipped and
         ! the retained count is written back to disextrapoints/pslextrapoints.

         IF (ISextradis) CALL initialise_extra_discharge_points()
         IF (ISextrapsl) CALL initialise_extra_water_table_output()

         CALL write_checked(dis2, &
                            'Simulated discharge at the outlet at every model timestep.', &
                            'Error writing to the discharge every timestep at the catchment outlet file '// &
                            '(unit 41 in the rundata file)')

         CALL write_checked(dis2, &
                            'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet_Discharge(m3/s)', &
                            'Error writing the every-timestep discharge column header to the catchment outlet '// &
                            'file (unit 41 in the rundata file)')

         CALL write_checked(mas, &
                            'Spatially Averaged Totals (mm) over the simulation', &
                            'Error writing to the the mass balance data file (unit 43 in the rundata file)')

         CALL write_checked(mas, &
                            'Time(Hours),'// &
                            'Cumulative_Precipitation,'// &
                            'Cumulative_Canopy_Evaporation,'// &
                            'Cumulative_Soil_Evaporation,'// &
                            'Cumulative_Transpiration,'// &
                            'Cumulative_Aquifer_Flow,'// &
                            'Cumulative_Discharge,'// &
                            'Canopy_Storage,'// &
                            'Snow_Storage,'// &
                            'Subsurface_Storage,'// &
                            'Land_Surface_Storage,'// &
                            'Channel_Storage', &
                            'Error writing the column header to the mass balance data file (unit 43 in the rundata file)')

         WRITE (dis, '(A,F8.2,A)', IOSTAT=ios) &
            'Simulated discharge(m3/s) at the outlet - regular timestep ', &
            TOUTPUT, &
            ' hours. Simulated discharge is the mean value over the timestep '// &
            'with the date at the start of the timestep'
         CALL stop_on_io_error(ios, &
                               'Error writing to the regular discharge at the catchment outlet file '// &
                               '(unit 44 in the rundata file)')

         CALL find_mass_balance_outlet()
         CALL write_discharge_header()

         uznowt = uznow/TOUTPUT
         next_hour = DBLE(INT(uznowt)) + ONE

         ! Hotstart first time is correct.
         IF (BHOTRD) uzold = DBLE(INT(bhotti/TOUTPUT))

         IF (bexsy) CALL initialise_sediment_output()
         IF (bexcm) CALL initialise_contaminant_output()

      END SUBROUTINE initialise_output

!> @brief Reads and compacts the optional virtual-discharge station list.
!>
!> Each retained record supplies a channel-link number and face. Requests whose
!> link exceeds `total_no_links` are silently discarded; malformed input raises
!> fatal frame error 1068.
!>
!> @warning
!> Non-positive link numbers and face numbers outside 1:4 are retained without
!> validation and will later be used as `QOC` indices.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted optional discharge-point setup from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE initialise_extra_discharge_points()
         READ (disextra, *, IOSTAT=ios)
         CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

         READ (disextra, *, IOSTAT=ios) disextratext, disextrapoints
         CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

         CALL allocate_extra_discharge(disextrapoints)

         j = 0
         DO i = 1, disextrapoints
            READ (disextra, *, IOSTAT=ios) L, iface
            CALL fatal_on_io_error(ios, 1068, 'no or incorrect data in extra discharge points file')

            ! Silently ignore discharge requests beyond the link range, matching
            ! the original behaviour while keeping the retained list compact.
            IF (L <= total_no_links) THEN
               j = j + 1
               disextraelement(j) = L
               disextraface(j) = iface
            END IF
         END DO

         disextrapoints = j
      END SUBROUTINE initialise_extra_discharge_points

!> @brief Reallocates and zeroes persistent arrays for `n` virtual discharge stations.
!>
!> This includes retained link/face identifiers, current samples, and
!> interval-integrated discharge. Existing allocations are discarded.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised virtual-station allocation during the output refactor. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
      SUBROUTINE allocate_extra_discharge(n)
         INTEGER, INTENT(IN) :: n

         INTEGER(KIND=I_P) :: ios
         CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
         CHARACTER(LEN=*), PARAMETER :: location = "frame_output:allocate_extra_discharge"

         IF (ALLOCATED(disextraelement)) THEN
            DEALLOCATE (disextraelement, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "disextraelement", location, emsg)
         END IF
         IF (ALLOCATED(disextraface)) THEN
            DEALLOCATE (disextraface, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "disextraface", location, emsg)
         END IF
         IF (ALLOCATED(qocavextra)) THEN
            DEALLOCATE (qocavextra, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "qocavextra", location, emsg)
         END IF
         IF (ALLOCATED(qoctotextra)) THEN
            DEALLOCATE (qoctotextra, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "qoctotextra", location, emsg)
         END IF

         ALLOCATE (disextraelement(n), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "disextraelement", location, emsg)
         ALLOCATE (disextraface(n), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "disextraface", location, emsg)
         ALLOCATE (qocavextra(n), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "qocavextra", location, emsg)
         ALLOCATE (qoctotextra(n), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "qoctotextra", location, emsg)

         disextraelement = 0
         disextraface = 0
         qocavextra = ZERO
         qoctotextra = ZERO
      END SUBROUTINE allocate_extra_discharge

!> @brief Reads selected water-table elements and opens their CSV output.
!>
!> Element numbers above `total_no_elements` are silently discarded. Valid
!> entries become columns containing `ZGRUND-ZVSPSL` in metres below ground;
!> a negative result denotes ponded surface water. Input/open failures use frame
!> error 1069.
!>
!> @warning
!> Non-positive element numbers are retained without validation and will later
!> be used to index `ZGRUND` and `ZVSPSL`.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted optional water-table setup from `FROUTPUT`. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
      SUBROUTINE initialise_extra_water_table_output()

         INTEGER(KIND=I_P) :: ios
         CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
         CHARACTER(LEN=*), PARAMETER :: location = "frame_output:initialise_extra_water_table_output"

         READ (pslextra, *, IOSTAT=ios)
         CALL fatal_on_io_error(ios, 1069, &
                                'no or incorrect data in input_CATCH_water_table_depth file')

         READ (pslextra, *, IOSTAT=ios) pslextratext, pslextrapoints
         CALL fatal_on_io_error(ios, 1069, &
                                'no or incorrect data in input_CATCH_water_table_depth file')

         IF (ALLOCATED(pslextraelement)) THEN
            DEALLOCATE (pslextraelement, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "pslextraelement", location, emsg)
         END IF
         ALLOCATE (pslextraelement(pslextrapoints), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "pslextraelement", location, emsg)
         pslextraelement = 0

         j = 0
         DO i = 1, pslextrapoints
            READ (pslextra, *, IOSTAT=ios) iel
            CALL fatal_on_io_error(ios, 1069, &
                                   'no or incorrect data in input_CATCH_water_table_depth file')

            ! Silently ignore water-table requests beyond the element range,
            ! preserving the original compaction behaviour.
            IF (iel <= total_no_elements) THEN
               j = j + 1
               pslextraelement(j) = iel
            END IF
         END DO

         pslextrapoints = j

         filnam = join_path(DIRQQ, 'output_'//TRIM(cnam)//'_water_table_depth.csv')
         OPEN (PSLFILEUNIT, FILE=filnam, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, filnam, emsg)

         CALL write_checked(PSLFILEUNIT, &
                            'Water_Table_depth(m_below_ground). A negative number '// &
                            'means there is surface water with the absolute value '// &
                            'the depth of surface water', &
                            'Error writing to the water-table depth output file '//TRIM(filnam))

         WRITE (PSLFILEUNIT, '(A,*(A,I0))', IOSTAT=ios) 'Time(hours)', &
            (', Element-', pslextraelement(j), j=1, pslextrapoints)
         CALL stop_on_io_error(ios, 'Error writing the column header to the water-table depth output file '//TRIM(filnam))
      END SUBROUTINE initialise_extra_water_table_output

!> @brief Selects the outlet link and face used by text and mass-balance output.
!>
!> The search resets `MBLINK` and `MBFACE`, scans every external channel face,
!> and retains the last boundary whose OC boundary-condition type is 7 (weir).
!> Both values remain zero when no such outlet exists.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted outlet discovery from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE find_mass_balance_outlet()
         ! Find outlet link for mass-balance output when no reservoir files exist.
         ! The outlet must be a weir boundary condition, type 7.
         mblink = 0
         mbface = 0

         DO L = 1, total_no_links
            DO iface = 1, 4
               IF (ICMREF(L, 4 + iface) == 0 .AND. NOCBCC(L) > 0) THEN
                  IF (NOCBCD(NOCBCC(L), 3) == 7) THEN
                     mblink = L
                     mbface = NOCBCD(NOCBCC(L), 2)
                  END IF
               END IF
            END DO
         END DO
      END SUBROUTINE find_mass_balance_outlet

!> @brief Writes the regular-discharge CSV column heading.
!>
!> The first discharge column identifies `MBLINK`; when virtual stations are
!> enabled, one `Channel-<link>` column is appended for every retained point.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted discharge-header formatting from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_discharge_header()
         IF (ISextradis) THEN
            WRITE (dis, '(*(A,I0))', IOSTAT=ios) &
               'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet-', &
               mblink, (',Channel-', disextraelement(j), j=1, disextrapoints)
            CALL stop_on_io_error(ios, &
                                  'Error writing the column header to the regular discharge at the catchment '// &
                                  'outlet file (unit 44 in the rundata file)')
         ELSE
            CALL write_checked(dis, &
                               'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet-Discharge', &
                               'Error writing the column header to the regular discharge at the catchment '// &
                               'outlet file (unit 44 in the rundata file)')
         END IF
      END SUBROUTINE write_discharge_header

!> @brief Opens and labels the total- and fine-sediment outlet CSV files.
!>
!> Output is enabled only when the sediment component is active. Total sediment
!> combines all fractions; fine sediment is fraction 1. Both fluxes are reported
!> in kg/s as interval means dated at the interval start.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted sediment-output setup from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE initialise_sediment_output()
         filnam = join_path(DIRQQ, 'output_'//TRIM(cnam)//'_sediment_all.csv')
         OPEN (SEDALLUNIT, FILE=filnam, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, filnam, emsg)

         filnam = join_path(DIRQQ, 'output_'//TRIM(cnam)//'_sediment_fine.csv')
         OPEN (SEDFINEUNIT, FILE=filnam, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, filnam, emsg)

         WRITE (SEDALLUNIT, '(A)', IOSTAT=ios) &
            'Sediment discharge at the outlet - All Sediments. '// &
            'This is the mean value over the timestep with the date at the start of the timestep'
         CALL stop_on_io_error(ios, 'Error writing to the sed-all-daily-output.csv file')
         CALL write_checked(SEDALLUNIT, &
                            'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet-Discharge(kg/s)', &
                            'Error writing the column header to the sed-all-daily-output.csv file')

         WRITE (SEDFINEUNIT, '(A)', IOSTAT=ios) &
            'Sediment discharge at the outlet - Fine Sediments. '// &
            'This is the mean value over the timestep with the date at the start of the timestep'
         CALL stop_on_io_error(ios, 'Error writing to the sed-fine-daily-output.csv file')
         CALL write_checked(SEDFINEUNIT, &
                            'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Outlet-Discharge(kg/s)', &
                            'Error writing the column header to the sed-fine-daily-output.csv file')

         sedav = ZERO
      END SUBROUTINE initialise_sediment_output

!> @brief Opens and labels the contaminant-one outlet CSV file.
!>
!> The series contains the interval mean of `CCCC(MBLINK,top_cell_no,1)`,
!> described by the file as relative concentration.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted contaminant-output setup from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE initialise_contaminant_output()
         filnam = join_path(DIRQQ, 'output_'//TRIM(cnam)//'_contaminant.csv')
         OPEN (CONTAMUNIT, FILE=filnam, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, filnam, emsg)

         WRITE (CONTAMUNIT, '(A)', IOSTAT=ios) &
            'Contaminant Relative Concentration (contaminant 1) at the outlet. '// &
            'This is the mean value over the timestep with the date at the start of the timestep.'
         CALL stop_on_io_error(ios, 'Error writing to the contaminant.csv file')
         CALL write_checked(CONTAMUNIT, &
                            'Date_yyyy-mm-dd_HH:MM:SS,Time(hours),Relative_concentration', &
                            'Error writing the column header to the contaminant.csv file')
      END SUBROUTINE initialise_contaminant_output

!> @brief Processes all additional output for one model timestep.
!>
!> Current outlet values are sampled, integrated over normalised output time,
!> emitted at every crossed regular boundary, and retained for the unfinished
!> interval. The routine also writes every-step discharge and scheduled
!> mass-balance/water-table rows before advancing `uzold`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted the timestep phase from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_main_output()
         ! Accumulate mean values in normalised output-time units.  When the
         ! current model time crosses one or more regular output boundaries, write
         ! one row for the just-completed interval and fill any skipped regular
         ! intervals with the current timestep average.

         CALL sample_current_values(qocav, sedav, sedfineav, contamav)

         uznowt = uznow/TOUTPUT
         hour_now = INT(uznowt)

         IF (hour_now < INT(next_hour)) THEN
            CALL accumulate_interval(uznowt - uzold, qocav, sedav, sedfineav, contamav)
         ELSE
            CALL accumulate_interval(next_hour - uzold, qocav, sedav, sedfineav, contamav)
            CALL write_completed_regular_outputs(hour_now, qocav, sedav, sedfineav, contamav)
            CALL restart_accumulators(uznowt - next_hour, qocav, sedav, sedfineav, contamav)

            next_hour = next_hour + ONE
         END IF

         CALL WRITE_DIS2(mbface, qocav, uznow)
         CALL write_periodic_mass_balance()

         uzold = uznowt

         ! A 1-D run may have no configured outlet face.
         IF (mblink == 0 .AND. mbface == 0) THEN
            qocav = ZERO
         ELSE
            qocold = qoc(mblink, mbface)
         END IF

      END SUBROUTINE write_main_output

!> @brief Samples outlet discharge, sediment flux, and contaminant concentration.
!>
!> When no outlet was found, all returned values are zero. Otherwise total
!> sediment is the sum of all `QSED` fractions multiplied by `RHO_SEDIMENT`, fine
!> sediment is fraction 1, and contaminant output is the top-cell concentration
!> of contaminant 1. Optional station discharges are also refreshed.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted outlet sampling from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE sample_current_values(q_out, sed_out, sedfine_out, contam_out)
         DOUBLE PRECISION, INTENT(OUT) :: q_out
         DOUBLE PRECISION, INTENT(OUT) :: sed_out
         DOUBLE PRECISION, INTENT(OUT) :: sedfine_out
         DOUBLE PRECISION, INTENT(OUT) :: contam_out

         IF (mblink == 0 .AND. mbface == 0) THEN
            q_out = ZERO
            sed_out = ZERO
            sedfine_out = ZERO
            contam_out = ZERO
         ELSE
            q_out = qoc(mblink, mbface)

            IF (bexsy) THEN
               sed_out = ZERO
               DO i = 1, nsed
                  sed_out = sed_out + QSED(mblink, i, mbface)*RHO_SEDIMENT
               END DO
               sedfine_out = QSED(mblink, 1, mbface)*RHO_SEDIMENT
            ELSE
               sed_out = ZERO
               sedfine_out = ZERO
            END IF

            IF (bexcm) THEN
               contam_out = CCCC(mblink, top_cell_no, 1)
            ELSE
               contam_out = ZERO
            END IF
         END IF

         IF (ISextradis) THEN
            DO i = 1, disextrapoints
               qocavextra(i) = qoc(disextraelement(i), disextraface(i))
            END DO
         END IF
      END SUBROUTINE sample_current_values

!> @brief Integrates current samples over part of a regular output interval.
!>
!> `dt` is measured in units of `TOUTPUT`, so the accumulated values become
!> interval means when a complete unit interval is written. Sediment,
!> contaminant, and virtual-station totals are updated only when enabled.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted interval accumulation from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE accumulate_interval(dt, q_mean, sed_mean, sedfine_mean, contam_mean)
         DOUBLE PRECISION, INTENT(IN) :: dt
         DOUBLE PRECISION, INTENT(IN) :: q_mean
         DOUBLE PRECISION, INTENT(IN) :: sed_mean
         DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
         DOUBLE PRECISION, INTENT(IN) :: contam_mean

         qoctot = qoctot + q_mean*dt

         IF (bexsy) THEN
            sedtot = sedtot + sed_mean*dt
            sedfinetot = sedfinetot + sedfine_mean*dt
         END IF

         IF (bexcm) contamtot = contamtot + contam_mean*dt

         IF (ISextradis) THEN
            do i = 1, disextrapoints
               qoctotextra(i) = qoctotextra(i) + qocavextra(i)*dt
            end do
         END IF
      END SUBROUTINE accumulate_interval

!> @brief Writes a completed regular interval and fills any crossed intervals.
!>
!> The accumulated interval is timestamped at `next_hour-1`. If one model
!> timestep spans further boundaries, those intermediate rows use the current
!> sample directly, matching the legacy averaging behaviour.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted boundary-crossing output from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_completed_regular_outputs(hour_now, q_mean, sed_mean, sedfine_mean, contam_mean)
         INTEGER, INTENT(IN) :: hour_now
         DOUBLE PRECISION, INTENT(IN) :: q_mean
         DOUBLE PRECISION, INTENT(IN) :: sed_mean
         DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
         DOUBLE PRECISION, INTENT(IN) :: contam_mean

         DOUBLE PRECISION :: output_hour

         ! output_hour = next_hour - 1.0D0:
         !     mean value over the regular timestep, timestamped at the start
         !     of the timestep.
         ! output_hour = next_hour:
         !     equivalent mean value timestamped at the end of the timestep.
         output_hour = next_hour - ONE

         CALL write_regular_outputs(output_hour, ABS(qoctot), disextrapoints, qoctotextra, &
                                    sedtot, sedfinetot, contamtot)

         DO i = INT(next_hour) + 1, hour_now
            next_hour = DBLE(i)
            output_hour = next_hour - ONE

            CALL write_regular_outputs(output_hour, ABS(q_mean), disextrapoints, qocavextra, &
                                       sed_mean, sedfine_mean, contam_mean)
         END DO
      END SUBROUTINE write_completed_regular_outputs

!> @brief Seeds interval accumulators with the portion after an output boundary.
!>
!> Each enabled total is replaced by its current sample multiplied by `dt`,
!> where `dt` is the remaining fraction of the current `TOUTPUT` interval.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted post-boundary state handling from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE restart_accumulators(dt, q_mean, sed_mean, sedfine_mean, contam_mean)
         DOUBLE PRECISION, INTENT(IN) :: dt
         DOUBLE PRECISION, INTENT(IN) :: q_mean
         DOUBLE PRECISION, INTENT(IN) :: sed_mean
         DOUBLE PRECISION, INTENT(IN) :: sedfine_mean
         DOUBLE PRECISION, INTENT(IN) :: contam_mean

         qoctot = q_mean*dt

         IF (bexsy) THEN
            sedtot = sed_mean*dt
            sedfinetot = sedfine_mean*dt
         END IF

         IF (bexcm) contamtot = contam_mean*dt

         IF (ISextradis) THEN
            qoctotextra(1:disextrapoints) = qocavextra(1:disextrapoints)*dt
            do i = 1, disextrapoints
               qoctotextra(i) = qocavextra(i)*dt
            end do
         END IF
      END SUBROUTINE restart_accumulators

!> @brief Writes one timestamped row to each enabled regular-output stream.
!>
!> `output_hour` is an interval index and is converted to elapsed hours using
!> `TOUTPUT`. Outlet discharge is supplied already non-negative; optional
!> station discharges are made absolute. Sediment and contaminant rows are
!> written only when their components are active.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised regular CSV row formatting during the output refactor. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
      SUBROUTINE write_regular_outputs(output_hour, discharge, disextrapoints, discharge_extra, &
                                       sediment, sediment_fine, contaminant)
         DOUBLE PRECISION, INTENT(IN) :: output_hour
         DOUBLE PRECISION, INTENT(IN) :: discharge
         INTEGER, INTENT(IN) :: disextrapoints
         DOUBLE PRECISION, INTENT(IN) :: discharge_extra(:)
         DOUBLE PRECISION, INTENT(IN) :: sediment
         DOUBLE PRECISION, INTENT(IN) :: sediment_fine
         DOUBLE PRECISION, INTENT(IN) :: contaminant

         CHARACTER(LEN=32) :: stamp
         DOUBLE PRECISION  :: elapsed

         CHARACTER(len=32), DIMENSION(:), allocatable :: buf
         CHARACTER(len=32) :: bufdis

         INTEGER(KIND=I_P) :: ios
         CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
         CHARACTER(LEN=*), PARAMETER :: location = "frame_output:write_regular_outputs"

         SAVE buf

         IF (ALLOCATED(buf)) THEN
            DEALLOCATE (buf, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "buf", location, emsg)
         END IF
         ALLOCATE (buf(disextrapoints), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "buf", location, emsg)
         buf = ''

         elapsed = output_hour*TOUTPUT
         stamp = timestamp_from_output_hour(output_hour)

         write (bufdis, '(F20.5)') discharge
         bufdis = adjustl(bufdis)
         if (ISextradis) then
            do j = 1, disextrapoints
               write (buf(j), '(F20.5)') abs(discharge_extra(j))
               buf(j) = adjustl(buf(j))
            end do
           WRITE (dis, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis), (',', trim(buf(j)), j=1, disextrapoints)
         else
            WRITE (dis, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis)
         end if

         if (bexsy) then
            write (bufdis, '(F20.5)') sediment
            bufdis = adjustl(bufdis)
            write (SEDALLUNIT, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis)
            write (bufdis, '(F20.5)') sediment_fine
            bufdis = adjustl(bufdis)
            write (SEDFINEUNIT, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis)
         end if
         if (bexcm) then
            write (bufdis, '(F20.5)') contaminant
            bufdis = adjustl(bufdis)
            write (CONTAMUNIT, '(A,A1,F0.3,*(A1,A))') TRIM(stamp), ',', elapsed, ',', trim(bufdis)
         end if

      END SUBROUTINE write_regular_outputs

!> @brief Converts a regular-output interval index to an absolute timestamp.
!>
!> The timestamp represents `TIH + output_hour*TOUTPUT` and is formatted
!> `yyyy-mm-dd HH:MM:SS`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised output timestamp generation. |
!> @endhistory
      FUNCTION timestamp_from_output_hour(output_hour) RESULT(stamp)
         DOUBLE PRECISION, INTENT(IN) :: output_hour
         CHARACTER(LEN=32) :: stamp
         INTEGER :: c(6)

         c = DATE_FROM_HOUR(tih + output_hour*TOUTPUT)

         WRITE (stamp, '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
            c(1), c(2), c(3), c(4), c(5), c(6)
      END FUNCTION timestamp_from_output_hour

!> @brief Writes scheduled catchment-average balance and water-table rows.
!>
!> Once `UZNOW` exceeds `icounter2`, cumulative balance/storage entries
!> `BALANC(7:17)` are converted from cubic metres to millimetres over `CAREA`
!> and written to `MAS`. The threshold then advances by 24 h. Selected
!> water-table depths are written on the same schedule.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted periodic mass-balance output from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_periodic_mass_balance()
         IF (uznow <= icounter2) RETURN

         WRITE (mas, '(F16.3,11('','',F16.3))') uznow, &
            balanc(7)*1000.0D0/carea, &
            balanc(8)*1000.0D0/carea, &
            balanc(9)*1000.0D0/carea, &
            balanc(10)*1000.0D0/carea, &
            balanc(11)*1000.0D0/carea, &
            balanc(12)*1000.0D0/carea, &
            balanc(13)*1000.0D0/carea, &
            balanc(14)*1000.0D0/carea, &
            balanc(15)*1000.0D0/carea, &
            balanc(16)*1000.0D0/carea, &
            balanc(17)*1000.0D0/carea

         icounter2 = icounter2 + 24.0D0

         IF (ISextrapsl) THEN
            WRITE (PSLFILEUNIT, '(F10.2,*(1A,F10.2))') uznow, &
               (',', zgrund(pslextraelement(i)) - zvspsl(pslextraelement(i)), &
                i=1, pslextrapoints)
         END IF
      END SUBROUTINE write_periodic_mass_balance

!> @brief Writes the final phreatic surface and pressure heads for VSI reuse.
!>
!> With banks active, output begins at element 1; otherwise channel links are
!> omitted and output begins at `total_no_links+1`. Each included element writes
!> `VSPSI` from its bottom active layer through `top_cell_no`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Extracted end-of-simulation state output from `FROUTPUT`. |
!> @endhistory
      SUBROUTINE write_final_state()
         ! The individual records are written unconditionally; ios carries the
         ! first failure through to a single fatal check at the end.
         ios = 0
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios) 'Output at end of simulation for use as initial conditions in vsi file'
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios) 'This output is by element number'
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios)
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios) 'phreatic surface level '

         IF (bexbk) THEN
            nminel = 1
         ELSE
            nminel = total_no_links + 1
         END IF

         IF (ios == 0) WRITE (vse, '(10(1X,F9.3))', IOSTAT=ios) (zvspsl(j), j=nminel, total_no_elements)
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios)
         IF (ios == 0) WRITE (vse, *, IOSTAT=ios) 'Heads at end of simulation'

         DO iel = 1, total_no_elements
            IF (ios == 0 .AND. (bexbk .OR. iel > total_no_links)) THEN
               WRITE (vse, '(I7)', IOSTAT=ios) iel
               IF (ios == 0) WRITE (vse, '(10(1X,F9.3))', IOSTAT=ios) &
                  (VSPSI(j, iel), j=nlyrbt(iel, 1), top_cell_no)
            END IF
         END DO

         CALL stop_on_io_error(ios, &
                               'Error writing the end-of-simulation VSS state to the vsi initial-conditions '// &
                               'file (unit 42 in the rundata file)')
      END SUBROUTINE write_final_state

!> @brief Writes one text record and applies the standard fatal output check.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised checked heading writes during the output refactor. |
!> @endhistory
      SUBROUTINE write_checked(unit, line, error_message)
         INTEGER, INTENT(IN) :: unit
         CHARACTER(LEN=*), INTENT(IN) :: line
         CHARACTER(LEN=*), INTENT(IN) :: error_message

         WRITE (unit, '(A)', IOSTAT=ios) line
         CALL stop_on_io_error(ios, error_message)
      END SUBROUTINE write_checked

!> @brief Converts a nonzero output status into a console diagnostic and `ERROR STOP`.
!>
!> The supplied message is followed by a reminder to close software that may
!> have locked the output file. A zero status returns without side effects.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised fatal text-output handling. |
!> @endhistory
      SUBROUTINE stop_on_io_error(io_status, message)
         INTEGER, INTENT(IN) :: io_status
         CHARACTER(LEN=*), INTENT(IN) :: message

         IF (io_status == 0) RETURN

         WRITE (*, '(A)') message
         WRITE (*, '(A)') 'Check it is not open in other software (e.g. Excel)'
         CALL ERR_STOP(255)
      END SUBROUTINE stop_on_io_error

!> @brief Routes a nonzero input/output status through the shared frame error service.
!>
!> On failure, `error_code` and `message` are passed to
!> `ERROR(ERRLVL_fatal,...)`; a zero status returns normally.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-05-03 | SvB | Centralised numbered fatal I/O checks during the output refactor. |
!> @endhistory
      SUBROUTINE fatal_on_io_error(io_status, error_code, message)
         INTEGER, INTENT(IN) :: io_status
         INTEGER, INTENT(IN) :: error_code
         CHARACTER(LEN=*), INTENT(IN) :: message

         IF (io_status /= 0) CALL RAISE_ERROR(ERRLVL_fatal, error_code, FID_logfile, 0, 0, message)
      END SUBROUTINE fatal_on_io_error

   END SUBROUTINE FROUTPUT

!> @brief Writes one discharge value using the configured mass-balance face sign convention.
!>
!> Faces 1 and 2 preserve the sign of `qoo`; faces 3 and 4 reverse it before
!> writing to the regular discharge unit `DIS`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | Legacy | - | - | Added regular discharge output using the OC face sign convention. |
!> | 2026-04-04 | SvB | 4.6.1 | Standardised Fortran formatting without changing the sign rule. |
!> @endhistory
   SUBROUTINE write_dis(mbface, qoo)
      INTEGER, INTENT(IN)            :: mbface
      DOUBLEPRECISION, INTENT(IN)    :: qoo
      DOUBLEPRECISION                :: qd
      IF ((mbface == 1) .OR. (mbface == 2)) THEN
         qd = qoo
      ELSE
         qd = -qoo
      END IF
      WRITE (dis, '(F20.8)') qd
   END SUBROUTINE write_dis

!> @brief Writes one timestamped discharge record using the configured face sign convention.
!>
!> Faces 1 and 2 preserve the sign of `qoo`; faces 3 and 4 reverse it. The
!> timestamp is `TIH + TME` converted with [[datetime:date_from_hour]], and the
!> row is written to `DIS2` as date/time, simulation hour, and discharge.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2006-03 | SB | 4.x | Added outlet discharge at every model timestep. |
!> | 2026-07-08 | SB | 4.6.1 | Made numeric text formatting explicit and checked output writes. |
!> @endhistory
   SUBROUTINE write_dis2(mbface, qoo, tme)
      INTEGER, INTENT(IN)            :: mbface
      INTEGER                        :: c(6)
      DOUBLEPRECISION, INTENT(IN)    :: qoo, tme
      DOUBLEPRECISION                :: qd
      CHARACTER(128)                 :: dum
      CHARACTER(len=32)              :: bufdis2
      IF ((mbface == 1) .OR. (mbface == 2)) THEN
         qd = qoo
      ELSE
         qd = -qoo
      END IF
      c = DATE_FROM_HOUR(tih + tme)
      WRITE (dum, '(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') c(1), '-', c(2), '-', c(3), ' ', c(4), ':', c(5), ':', c(6)
      WRITE (bufdis2, '(F20.5)') qd
      bufdis2 = adjustl(bufdis2)
      WRITE (dis2, '(A,A1,F0.5,A1,A)') TRIM(dum), ',', tme, ',', TRIM(bufdis2)
   END SUBROUTINE write_dis2

END MODULE frame_output

