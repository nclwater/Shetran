!> summary: Run setup: rundata, the frame input, file opening and component initialisation.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> What happens before the first timestep. [[FROPEN]] reads the rundata file
!> and opens every data and output file named in it; [[INFR]] reads the frame
!> input records; [[FRINIT]] then establishes the grid through
!> [[frame_geometry]] and calls each active component's setup routine —
!> [[et_input:INET]], [[snow_input:INSM]], [[bank_setup:INBK]],
!> [[cm_input:INCM]], [[cm_plant:INPL]], [[vs_input:VSIN]],
!> [[oc_driver:OCINI]] and [[zq_tables:ReadZQTable]] — or the corresponding
!> stand-in when the component is disabled.
!>
!> This is orchestration and data-marshalling code, not a hydrological
!> formulation: it is the bookkeeping that lets the components share geometry,
!> file units, time controls and output definitions.
!>
!> @note
!> [[DINET]], [[DINOC]] and [[DOCIN]] are the stand-ins for disabled
!> components. `DINET` prints a message and sets `BMETAL`, `DINOC` prints a
!> message, and `DOCIN` has an empty body and no caller at all. They are kept
!> as they are.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed and standardised the FR frame, including impermeable-bed defaults, `BSOFT`, `TIM` migration to `AL_D`, result output, and hot-start/rescue handling. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the FR `.F` files into a single Fortran 90 module. |
!> | 2020-05 | SB | 4.5 | Added ZQ-module variables and support. |
!> | 2026-03 | SB | 4.6 | Added allocation-based initialisation, date-aware meteorological input, the outlet sediment/contaminant text series and the water-table output. |
!> | 2026-09-11 | SvB | - | Split out of FRmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE frame_setup

   USE stdlib_system, ONLY: join_path
   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, IOSTAT_END
   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, LENGTH_FILEPATH, I_P, zero
   USE array_limits, ONLY: LLEE, max_no_snowmelt_slugs, NCONEE, nelee, nlfee, NLYREE, &
                           NOCTAB, NOLEE, NPELEE, NPLTEE, NSEDEE, NSEE, NUZTAB, NVBP, &
                           NVEE, NVSEE, NCLASS, NXE, nxee, NXSCEE, NYE, nyee
   USE build_info, ONLY: BANNER, BDEVER, SHEVER
   USE run_context, ONLY: cnam, DIRQQ, filnam, hdf5filename, &
                          visualisation_check_filename, visualisation_plan_filename
   USE run_control, ONLY: BEXCM, BEXET, BEXEX, BEXOC, BEXSM, BEXSY, BEXSZ, BEXUZ, BFRTS1, &
                          BFRTS2, BHOTPR, BHOTRD, BHOTST, BHOTTI, BINFRP, BSOFT, HOTIME, &
                          isextradis, isextrapsl, TCH, TITLE, TOUTPUT, TSH, msg
   USE element_geometry, ONLY: DXIN, DYIN, ISORT, NXEP1, NXM1, NXP1, NYEP1, NYM1, NYP1, &
                              top_cell_no, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, INGRID, NGDBGN, NX, NY
   USE channel_geometry, ONLY: BEXBK
   USE simulation_clock, ONLY: TIH, TIMEUZ, TTH, UZNEXT, UZNOW, UZVAL
   USE file_units, ONLY: BKD, ETD, FID_logfile, FRD, HOT, OCD, PPD, SMD, TIM, VSD, VSI
   USE met_forcing, ONLY: ISTA, NM, NMC, NRAIN, NRAINC
   USE timestep_control, ONLY: PALFA, PMAX, PREST, TMAX
   USE water_balance, ONLY: MBDAY, MBMON, MBYEAR
   USE frame_geometry, ONLY: FRDIM, FRIND
   USE frame_output, ONLY: BPBAL, BPDEP, BPEPOT, BPHSZ, BPPNET, BPQF, BPQH, BPQOC, BPQSZ, BPSD
   USE legacy_result_files, ONLY: ALLOUT, BSTORE, BTIME, DTAO, FRRESP, IAOUT, PSTART, RESFIL
   USE et_config, ONLY: BMETAL
   USE et_input, ONLY: INET
   USE et_process, ONLY: INITIALISE_ETMOD
   USE et_state, ONLY: CSTORE, initialise_al_c3, NV, NVC
   USE snow_input, ONLY: INSM
   USE snow_state, ONLY: MSM, NSMC, SD, smelt, tmelt, TS
   USE vs_input, ONLY: VSIN
   USE vs_state, ONLY: NS, VSPSI
   USE oc_driver, ONLY: OCINI
   USE oc_validation, ONLY: OCLTL
   USE oc_node_solver, ONLY: sethrf, setqsa
   USE oc_state, ONLY: DQ0ST, DQIST, DQIST2, LCODEX, LCODEY, OCNOW, QMAX, QOC
   USE bank_setup, ONLY: INBK
   USE cm_input, ONLY: INCM
   USE cm_solver_flags, ONLY: ISMN
   USE zq_tables, ONLY: iszq, ReadZQTable
   USE grid_arrays, ONLY: AREADI, AREADR
   USE datetime, ONLY: hour_from_date
   USE error_reporting, ONLY: ERR_STOP
   USE error_status, ONLY: errstat_fileclose, errstat_fileopen, errstat_read, errstat_rewind

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: FROPEN, FRINIT

CONTAINS

!> @brief Runs the main frame initialisation sequence.
!>
!> `FRINIT` calls the common frame reader, component-specific input routines,
!> geometry/index setup, VSS initialisation, bank/sediment/contaminant setup,
!> hot-start reading, allocation routines, and output header preparation before
!> the first timestep is executed.
!>
!> The initialisation uses shared model dimensions and flags including
!> `top_cell_no`, `total_no_elements`, `NGDBGN`, `total_no_links`, `ICMREF`,
!> `UZNEXT`, `CSTORE`, `BEXBK`, `BEXET`,
!> `BEXOC`, `BEXSM`, `DTAO`, `BHOTRD`, `BINFRP`, and `BSTORE`. It updates
!> meteorological/rainfall category arrays `NMC` and `NRAINC`, consumes the
!> component input/output units already opened by [[fropen]] (`BFB`, `BHB`,
!> `BKD`, `CMB`, `CMD`, `CMP`, `CMT`,
!> `EPD`, `ETD`, `FRD`, `HOT`, `LFB`, `LGB`, `LHB`, `MED`, `OCD`, `OFB`, `OHB`,
!> `PPD`, `PRD`, `FID_logfile`, `RES`, `SMD`, `SPR`, `SYD`, `TIM`, `VED`, `VSD`, `VSI`,
!> `WLD`), and initialises run state such as `BHOTTI`, `HOTIME`, `OCNOW`,
!> `TIMEUZ`, `UZNEXT`, `UZNOW`, `MSM`, and `ALLOUT`.
!>
!> | Stage | Main calls/actions |
!> |:------|:-------------------|
!> | Frame input and allocation | [[infr]], `INITIALISE_AL_C3`, `INITIALISE_ETMOD`. |
!> | Optional component input | [[inet]]/[[dinet]], [[insm]], [[oc_driver:OCINI]]/[[dinoc]]. |
!> | Geometry and subsurface | [[frdim]], [[inbk]] when banks are active, then [[vs_input:VSIN]]. |
!> | Link forcing setup | Copy meteorological/rainfall station codes from the first adjacent non-link element to each channel link. |
!> | Reservoir tables | [[zq_tables:ReadZQTable]] when `ISZQ` is true. |
!> | Hot-start | Scan `HOT` until `HOTIME >= BHOTTI`, restore water-flow arrays through `SETHRF`/`SETQSA`, and write restart output via [[frresp]]. |
!>
!> @note
!> Input units are rewound rather than closed after reading. This preserves the
!> legacy automatic-differentiation workflow noted by the in-line comments.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-05 | RAH | 3.4.1 | Added restart checks and passed simulation time to result output. |
!> | 1996-07-24 | GP | 4.0 | Replaced the separate UZ/SZ/EX initialisation path with VSS. |
!> | 1997-1998 | RAH | 4.1-4.2 | Removed redundant legacy state and standardised restart/output setup. |
!> | 2007-03-01 | SB | 4g-pc | Changed the `AIOSTO` `DATA` statement initialisation. |
!> | 2026-03 | SB | 4.6 | Added allocation-based ET/vegetation setup and ZQ-table initialisation. |
!> @endhistory
   SUBROUTINE FRINIT()

      IMPLICIT NONE

      ! Locals, etc
      INTEGER :: IEL, IFACE, JEL, K, ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed `REWIND` or `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'FRmod:FRINIT' !! Location string for read-error reports.
      DOUBLE PRECISION :: rdd(NELEE), rddq(NELEE, 4)
      CHARACTER(LEN=20) :: AIOSTO
      CHARACTER(LEN=10) :: atemp

      DATA AIOSTO/'11111111111111111111'/

      ! OPEN I/O DATA FILES
      ! CALL FROPEN  !moded to main routine
      !
      ! CALL INITIALIZATION SUBROUTINES.
      ! NOTE: THE ORDER IN WHICH THE SUBROUTINES ARE CALLED IS IMPORTANT.

      CALL INFR
      CALL INITIALISE_AL_C3
      CALL INITIALISE_ETMOD

      IF (BEXET) CALL INET
      IF (.NOT. BEXET) CALL DINET

      IF (BEXSM) CALL INSM
      IF (.NOT. BEXSM) MSM = 0

      IF (BEXOC) CALL OCINI()
      IF (.NOT. BEXOC) CALL DINOC

      ! CALCULATE ELEMENT DIMENSIONS AND AREAS
      ! (MUST BE CALLED AFTER OCINI AND BEFORE VSIN)
      CALL FRDIM(BINFRP)

      IF (BEXBK) CALL INBK

      CALL VSIN

      ! SET UP MET. & RAINFALL STATIONS FOR THE CHANNEL (= ADJACENT BANK/GRID)
      link_loop: DO IEL = 1, total_no_links
         NMC(IEL) = 1
         NRAINC(IEL) = 1

         DO IFACE = 1, 4
            JEL = ICMREF(IEL, 4 + IFACE)
            IF (JEL > 0) THEN
               IF (ICMREF(JEL, 1) /= 3 .AND. NMC(JEL) > 0 .AND. NRAINC(JEL) > 0) THEN
                  NMC(IEL) = NMC(JEL)
                  NRAINC(IEL) = NRAINC(JEL)
                  CYCLE link_loop
               END IF
            END IF
         END DO
      END DO link_loop

      ! ZQ Module 200520
      IF (iszq) CALL ReadZQTable

      ! close data input file units
      REWIND (FRD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (FRD) for AD
      CALL errstat_rewind(ios, fid=FRD, iomsg=emsg)
      REWIND (VSD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (VSD) for AD
      CALL errstat_rewind(ios, fid=VSD, iomsg=emsg)
      REWIND (OCD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (OCD) for AD
      CALL errstat_rewind(ios, fid=OCD, iomsg=emsg)
      REWIND (ETD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (ETD) for AD
      CALL errstat_rewind(ios, fid=ETD, iomsg=emsg)
      REWIND (SMD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (SMD) for AD
      CALL errstat_rewind(ios, fid=SMD, iomsg=emsg)
      REWIND (BKD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (BKD) for AD
      CALL errstat_rewind(ios, fid=BKD, iomsg=emsg)
      REWIND (VSI, IOSTAT=ios, IOMSG=emsg) ! CLOSE (VSI) for AD
      CALL errstat_rewind(ios, fid=VSI, iomsg=emsg)
      ! CALL RES FILE INPUT ROUTINE, IF REQUIRED
      ! IF (BSTORE) CALL INRES(BINFRP)
      REWIND (PPD, IOSTAT=ios, IOMSG=emsg) ! CLOSE (PPD) for AD
      CALL errstat_rewind(ios, fid=PPD, iomsg=emsg)

      ! UPDATE HOTSTART TIME AND READ FROM FILE IF BHOTRD = TRUE
      HOTIME = zero

      IF (BHOTRD) THEN

         hotstart_read: DO
            READ (HOT, *, IOSTAT=ios, IOMSG=emsg) atemp, HOTIME, UZNEXT, top_cell_no, atemp, &
               (CSTORE(IEL), IEL=NGDBGN, total_no_elements), atemp, &
               (rdd(IEL), IEL=1, total_no_elements), atemp, &
               ((rddq(IEL, K), IEL=1, total_no_elements), K=1, 4), atemp, &
               ((QOC(IEL, K), IEL=1, total_no_elements), K=1, 4), atemp, &
               ((DQ0ST(IEL, K), IEL=1, total_no_elements), K=1, 4), atemp, &
               ((DQIST(IEL, K), IEL=1, total_no_elements), K=1, 4), atemp, &
               ((DQIST2(IEL, K), IEL=1, NGDBGN - 1), K=1, 3), atemp, &
               (SD(IEL), IEL=NGDBGN, total_no_elements), atemp, &
               (TS(IEL), IEL=NGDBGN, total_no_elements), atemp, &
               (NSMC(IEL), IEL=NGDBGN, total_no_elements), atemp, &
               ((SMELT(K, IEL), K=1, NSMC(IEL)), IEL=NGDBGN, total_no_elements), atemp, &
               ((tmelt(K, IEL), K=1, NSMC(IEL)), IEL=NGDBGN, total_no_elements), atemp, &
               ((VSPSI(k, iel), k=1, top_cell_no), IEL=1, total_no_elements)

            ! Gracefully exit if end of hotstart file is reached
            IF (ios < 0) THEN
               WRITE (FID_logfile, '(/ A)') ' WARNING: END OF HOTSTART FILE REACHED'
               EXIT hotstart_read
            END IF

            ! a positive status is a genuine read error, not an expected end of file
            CALL errstat_read(ios, location, emsg)

            DO IEL = 1, total_no_elements
               CALL SETHRF(IEL, rdd(IEL))
               DO K = 1, 4
                  CALL SETQSA(IEL, K, rddq(IEL, K))
               END DO
            END DO

            ! Keep reading lines if HOTIME is less than the target BHOTTI
            IF (HOTIME >= BHOTTI) EXIT hotstart_read

         END DO hotstart_read

         WRITE (FID_logfile, '(// A, F10.2, A /)') ' ^^^ HOTSTART OF SIMULATION AT TIME ', HOTIME, ' ^^^'

         ALLOUT = HOTIME + DTAO
         UZNOW = HOTIME
         OCNOW = HOTIME
         UZVAL = UZNOW + UZNEXT
         TIMEUZ = HOTIME
         BHOTTI = HOTIME

         ! --- WRITE SET OF DATA TO RES FILES AT HOTSTART TIME
         CALL FRRESP(AIOSTO, UZNOW, .FALSE.)

      END IF

   END SUBROUTINE FRINIT

!> @brief Opens the run-data controlled input and output files.
!>
!> `FROPEN` prints the SHETRAN banner, opens the frame/run files, reads file
!> names and unit assignments, and prepares legacy output streams used by
!> initialisation and runtime reporting.
!>
!> The routine opens the rundata file `FILNAM` on unit 2 and a run log named
!> `info_<catchment>_SHETRAN_log.txt` on unit 61. It then reads label/name pairs
!> from the rundata file. Blank names and `0` mark optional files as unused;
!> otherwise most names are prefixed with `DIRQQ` and opened on their numeric
!> unit.
!>
!> | Units or entry | Behaviour |
!> |:---------------|:----------|
!> | 10:47 | Ordinary input/output files; missing unit 45 or 46 disables station output, missing unit 47 disables extra discharge output. |
!> | 48 | Stored as `visualisation_plan_filename`; not opened here. |
!> | 49 | Stored as `visualisation_check_filename`; not opened here. |
!> | 50 | Stored as `hdf5filename`; not opened here. |
!> | 51 | A blank name or `0` disables ZQ only; EOF here disables ZQ and every later optional group. A nonblank name is opened exactly as read. |
!> | 52 | Optional extra phreatic-surface-level output configuration, prefixed with `DIRQQ`. |
!> | 53 | Optional nitrate configuration file, prefixed with `DIRQQ`. |
!> | 54:60 | Additional optional nitrate files, prefixed with `DIRQQ`. |
!>
!> Special side effects:
!>
!> | Condition | Side effect |
!> |:----------|:------------|
!> | Routine entry | `ISTA`, `ISEXTRADIS`, `ISZQ`, `ISEXTRAPSL`, and `ISMN` are initially assumed true. |
!> | Unit 22 opened | `BTIME=.TRUE.`, writes an initial message to `TIM`, then rewinds it. |
!> | Unit 27 opened | `RESFIL` stores the resolved filename. |
!> | Required early file list ends before unit 14 | Stops with `ABNORMAL END`. |
!>
!> The contained `read_rundata_record` helper consumes one complete physical
!> record, so an empty record is distinct from EOF. `unit_context` labels read
!> diagnostics; `stop_eof_error` and `stop_rundata_open_error` report the
!> terminal messages and stop through [[error_reporting:ERR_STOP]]. Every other file
!> `OPEN` is checked through [[error_status:errstat_fileopen]], which reports the
!> `IOSTAT`/`IOMSG` and terminates. FORD lists these contained routines in the
!> source page rather than emitting separate procedure pages.
!>
!> @warning
!> Optional ZQ unit 51 is opened using the filename exactly as read, unlike most
!> other optional file entries that are prefixed with `DIRQQ`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Included the catchment name in run-file diagnostics. |
!> | 1997-02-23 | RAH | 4.0 | Standardised file status, time-series setup, and unit assignments. |
!> | 2013-12-16 | SB | - | Missing unit 45 or 46 disables station output (`ISTA`). |
!> | 2015-04-22 | SB | - | Missing unit 47 disables extra discharge output (`ISEXTRADIS`). |
!> | 2020-07-08 | SB | 4.5 | Added the optional ZQ table file on unit 51. |
!> | 2024-03-12 | SB | - | Added the optional extra phreatic-surface-level output configuration on unit 52, and moved the run log to unit 53. |
!> | 2025-09-25 | SB | 4.5.3 | Added the optional nitrate configuration files on units 53--60. |
!> | 2026-04 | SvB | 4.6.1 | Replaced platform-specific path handling with `join_path`. |
!> | 2026-05-11 | SB | - | Added error checking on the initial rundata-file `OPEN`, stopping instead of proceeding silently on failure. |
!> | 2026-07-11 | SvB | 4.6.1 | Distinguished blank records, EOF, and genuine rundata read errors. |
!> | 2026-09-06 | SvB | - | Routed every non-rundata file `OPEN` through [[error_status:errstat_fileopen]] (reporting `IOSTAT`/`IOMSG`) and removed the local `stop_open_error` helper. |
!> | 2026-09-06 | SvB | - | Checked every `CLOSE` of the rundata unit through [[error_status:errstat_fileclose]], which recovers the filename from the unit. |
!> @endhistory
   SUBROUTINE FROPEN

      IMPLICIT NONE

      INTEGER(KIND=I_P) :: I
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg
      LOGICAL :: at_eof
      CHARACTER(LEN=LENGTH_FILEPATH) :: FILNAM2

      !
      BTIME = .FALSE.

      ! WRITE BANNER HEADER TO SCREEN
      WRITE (*, *)
      WRITE (*, *) '**************************'
      WRITE (*, *) BANNER
      WRITE (*, *) '**************************'
      WRITE (*, *)

      ista = .TRUE.
      isextradis = .TRUE.
      iszq = .TRUE.
      isextrapsl = .TRUE.
      ismn = .TRUE.

      OPEN (2, FILE=FILNAM, STATUS='OLD', IOSTAT=ios, IOMSG=emsg)
      IF (ios /= 0) CALL stop_rundata_open_error(FILNAM, emsg)

      FILNAM2 = join_path(DIRQQ, 'info_'//TRIM(CNAM)//'_SHETRAN_log.txt')

      OPEN (61, FILE=FILNAM2, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileopen(ios, FILNAM2, emsg)

      CALL read_rundata_record(FILNAM, at_eof, 'rundata header')
      IF (at_eof) CALL stop_eof_error(CNAM)

      WRITE (61, '(A)') FILNAM
      WRITE (61, *)

      ! Main file reading loop
      DO I = 10, 50
         CALL read_rundata_record(FILNAM, at_eof, unit_context(I, 'description'))
         IF (at_eof) THEN
            IF (I < 14) CALL stop_eof_error(CNAM)
            iszq = .FALSE.
            isextrapsl = .FALSE.
            ismn = .FALSE.
            CLOSE (2, IOSTAT=ios, IOMSG=emsg)
            CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
            RETURN
         END IF

         WRITE (61, '(A)') FILNAM
         CALL read_rundata_record(FILNAM, at_eof, unit_context(I, 'filename'))
         IF (at_eof) THEN
            IF (I < 14) CALL stop_eof_error(CNAM)
            iszq = .FALSE.
            isextrapsl = .FALSE.
            ismn = .FALSE.
            CLOSE (2, IOSTAT=ios, IOMSG=emsg)
            CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
            RETURN
         END IF

         IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
            IF (I == 45 .OR. I == 46) ista = .FALSE.
            IF (I == 47) isextradis = .FALSE.

            WRITE (61, '("- NOT USED")')
         ELSE
            FILNAM = join_path(DIRQQ, TRIM(FILNAM))
            IF (I == 48) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               visualisation_plan_filename = FILNAM
            ELSE IF (I == 49) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               visualisation_check_filename = FILNAM
            ELSE IF (I == 50) THEN
               WRITE (61, '("FILE ",I3," IS ",A)') I, FILNAM
               hdf5filename = FILNAM
            ELSE
               WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') I, FILNAM

               OPEN (I, FILE=FILNAM, IOSTAT=ios, IOMSG=emsg)
               CALL errstat_fileopen(ios, FILNAM, emsg)

               IF (I == 27) RESFIL = FILNAM
               IF (I == 22) THEN
                  BTIME = .TRUE.
                  WRITE (TIM, *) 'Reading data sets ...'
                  REWIND (TIM, IOSTAT=ios, IOMSG=emsg)
                  CALL errstat_rewind(ios, fid=TIM, iomsg=emsg)
               END IF
            END IF
         END IF
      END DO

      ! ZQ Module 2020-05-20
      CALL read_rundata_record(FILNAM, at_eof, unit_context(51, 'description'))
      IF (at_eof) THEN
         iszq = .FALSE.
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      CALL read_rundata_record(FILNAM, at_eof, unit_context(51, 'filename'))
      IF (at_eof) THEN
         iszq = .FALSE.
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         iszq = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         OPEN (51, FILE=FILNAM, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, FILNAM, emsg)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 51, FILNAM
      END IF

      !extra psl 110324
      CALL read_rundata_record(FILNAM, at_eof, unit_context(52, 'description'))
      IF (at_eof) THEN
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      CALL read_rundata_record(FILNAM, at_eof, unit_context(52, 'filename'))
      IF (at_eof) THEN
         isextrapsl = .FALSE.
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         isextrapsl = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         FILNAM2 = join_path(DIRQQ, TRIM(FILNAM))
         OPEN (52, FILE=FILNAM2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, FILNAM2, emsg)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 52, FILNAM2
      END IF

      !nitrate component 230925
      CALL read_rundata_record(FILNAM, at_eof, unit_context(53, 'description'))
      IF (at_eof) THEN
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      WRITE (61, '(A)') FILNAM
      CALL read_rundata_record(FILNAM, at_eof, unit_context(53, 'filename'))
      IF (at_eof) THEN
         ismn = .FALSE.
         CLOSE (2, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose(ios, fid=2, iomsg=emsg)
         RETURN
      END IF

      IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
         ismn = .FALSE.
         WRITE (61, '("- NOT USED")')
      ELSE
         FILNAM = join_path(DIRQQ, TRIM(FILNAM))
         OPEN (53, FILE=FILNAM, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileopen(ios, FILNAM, emsg)
         WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') 53, FILNAM
      END IF

      ! Remaining nitrate files
      DO I = 54, 60
         CALL read_rundata_record(FILNAM, at_eof, unit_context(I, 'description'))
         IF (at_eof) EXIT

         WRITE (61, '(A)') FILNAM
         CALL read_rundata_record(FILNAM, at_eof, unit_context(I, 'filename'))
         IF (at_eof) EXIT

         IF (FILNAM == ' ' .OR. FILNAM == '0') THEN
            WRITE (61, '("- NOT USED")')
         ELSE
            FILNAM = join_path(DIRQQ, TRIM(FILNAM))
            OPEN (I, FILE=FILNAM, IOSTAT=ios, IOMSG=emsg)
            CALL errstat_fileopen(ios, FILNAM, emsg)
            WRITE (61, '("OPENING FILE UNIT ",I3," TO FILE ",A)') I, FILNAM
         END IF
      END DO

      CLOSE (2, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_fileclose(ios, fid=2, iomsg=emsg)

      RETURN

   CONTAINS

!> @brief Reads one complete physical record from the rundata file on unit 2.
!>
!> A blank record is a successful read and is returned as blanks in `line`.
!> End of file sets `at_eof`; every other input error reports the catchment and
!> supplied record `context` on `ERROR_UNIT`, then terminates with `ERROR STOP`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-07-11 | SvB | Distinguished genuine read failures from normal end of file using whole-record input. |
!> @endhistory
      SUBROUTINE read_rundata_record(line, at_eof, context)
         CHARACTER(LEN=*), INTENT(OUT) :: line
         LOGICAL, INTENT(OUT) :: at_eof
         CHARACTER(LEN=*), INTENT(IN) :: context

         INTEGER :: read_status
         CHARACTER(LEN=512) :: message

         ! An '(A)' read consumes exactly one physical record.  In particular,
         ! a blank record is a successful read whose result is all blanks.
         line = ''
         message = ''
         at_eof = .FALSE.
         READ (2, '(A)', IOSTAT=read_status, IOMSG=message) line

         IF (read_status == 0) RETURN
         IF (read_status == IOSTAT_END) THEN
            at_eof = .TRUE.
            RETURN
         END IF

         WRITE (ERROR_UNIT, '(A)') 'ERROR READING RUNDATA FILE '//TRIM(CNAM)// &
            ' ('//TRIM(context)//'): '//TRIM(message)
         CALL ERR_STOP(255)
      END SUBROUTINE read_rundata_record

!> @brief Formats the rundata unit number and record kind for an input diagnostic.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-07-11 | SvB | Added contextual diagnostics for whole-record rundata input. |
!> @endhistory
      FUNCTION unit_context(unit, record_kind) RESULT(context)
         INTEGER, INTENT(IN) :: unit
         CHARACTER(LEN=*), INTENT(IN) :: record_kind
         CHARACTER(LEN=64) :: context

         WRITE (context, '("unit ",I0,1X,A)') unit, TRIM(record_kind)
      END FUNCTION unit_context

      ! Internal helpers to cleanly exit without jumping to bottom labels
!> @brief Reports an unexpected early end of the rundata file and stops the run.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-04-06 | SvB | Replaced the legacy branch to a shared terminal label. |
!> @endhistory
      SUBROUTINE stop_eof_error(c_name)
         CHARACTER(LEN=*), INTENT(IN) :: c_name
         WRITE (*, '("UNEXPECTED -EOF- ON FILE ",A)') c_name
         CALL ERR_STOP(255)
      END SUBROUTINE stop_eof_error

!> @brief Reports failure to open the rundata file and stops the run.
!>
!> Takes the pathname that was actually opened, so that the message names the
!> file the user can go and look at.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-08-31 | SvB | Split from `stop_rundata_error`, which reported the catchment name instead of the path. |
!> | 2026-09-06 | SvB | Added IOMSG output |
!> @endhistory
      SUBROUTINE stop_rundata_open_error(f_name, emsg)
         CHARACTER(LEN=*), INTENT(IN) :: f_name
         CHARACTER(LEN=*), INTENT(IN) :: emsg
         WRITE (*, '("Error opening the rundata file ",A)') TRIM(f_name)
         WRITE (*, '("Error message: ",A)') TRIM(emsg)
         CALL ERR_STOP(255)
      END SUBROUTINE stop_rundata_open_error

   END SUBROUTINE FROPEN

!> @brief Reads global frame data shared by multiple components.
!>
!> This includes model size, simulation dates, grid spacing, output controls,
!> component execution flags, meteorological/vegetation/soil codes, ground
!> levels, link maps, printing controls, and the optional regular-output interval.
!>
!> | Stage | Main records/actions |
!> |:------|:---------------------|
!> | Run identity and dimensions | Print version/banner, read catchment title, `NX`, `NY`, simulation start/end, and sediment/contaminant start dates. |
!> | Grid and output timing | Read `DXIN`, `DYIN`, `DTAO`, `IAOUT`, `BINFRP`, `BFRTS1`, `BFRTS2`, `BSTORE`, and `PSTART`. |
!> | Timestep controls | Read `PMAX`, `PALFA`, `QMAX`, `TMAX`, and `BSOFT`; cap `TMAX` at 2 h and set `PREST=1+PALFA`. |
!> | Optional printed arrays | Read `BPPNET`, `BPEPOT`, `BPQOC`, `BPDEP`, `BPQF`, `BPQH`, `BPQSZ`, `BPHSZ`, `BPBAL`, and `BPSD` only when `IAOUT=2`. |
!> | Component flags and hot start | Read optional-component flags `BEXSM`, `BEXBK`, `BEXSY`, `BEXCM`, then hot-start controls. |
!> | Codes and geometry | Read station/type counts, discard obsolete river-lining record, read default codes, grid mask, OC link-code layouts, and call [[frind]]. |
!> | Distributed arrays | Read `ZGRUND`; read or default `NMC`, `NRAINC`, and `NVC`; read optional `TOUTPUT`. |
!>
!> Common data read and initialised include:
!>
!> | Data group | Variables |
!> |:-----------|:----------|
!> | Input and echo units | `FRD` and `FID_logfile`, already opened by [[fropen]] |
!> | Job title | run title text |
!> | Model size | `NX`, `NY` |
!> | Simulation start time | `ISYEAR`, `ISMTH`, `ISDAY`, `ISHOUR`, `ISMIN` |
!> | Simulation end time | `IEYEAR`, `IEMTH`, `IEDAY`, `IEHOUR`, `IEMIN` |
!> | Sediment and contaminant start times | `JSYEAR`...`JSMIN`, `JCYEAR`...`JCMIN`; converted to `TSH`/`TCH` only when the component is enabled. |
!> | Node spacing | `DXIN` in x direction, `DYIN` in y direction |
!> | Printing/output control | `DTAO`, `IAOUT`, `BINFRP`, `BFRTS1`, `BFRTS2`, `BSTORE`, `PSTART`, `TOUTPUT` |
!> | Printed-result selection | `BPPNET`, `BPEPOT`, `BPQOC`, `BPDEP`, `BPQF`, `BPQH`, `BPQSZ`, `BPHSZ`, `BPBAL`, `BPSD` |
!> | Component execution control | `BEXSM`, `BEXBK`, `BEXSY`, `BEXCM`; `BEXET`, `BEXUZ`, `BEXOC`, `BEXSZ`, and `BEXEX` are forced true. |
!> | Counts | `NM`, `NRAIN`, `NV`, `NS`; local `NLYRCT` is read and echoed only |
!> | Default met/rain/vegetation codes | `IDMC`, `IDRA`, `IDVE`; `IDLYR` is read but not used here. |
!> | Elevations and geometry | `ZGRUND`, `INGRID`, `LCODEX`, `LCODEY`, `ICMREF` |
!> | Distributed codes | `NMC`, `NRAINC`, `NVC` |
!>
!> The main grid mask is read by row label from top to bottom (`K=NY...1`).
!> Input value `1` is converted to internal catchment value `0`; every other
!> value is converted to `-1`. The row label is checked and a mismatch stops the
!> run immediately.
!>
!> @note
!> The obsolete river-lining record `FR30/FR31` is still consumed from the input
!> stream, but its values are not stored or converted by the active code.
!> @endnote
!>
!> @note
!> `TOUTPUT` is optional. If record `FR52/FR53` is absent or unreadable, the
!> routine uses a 24 h averaging interval.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed common frame input, component flags, grid codes, and output controls. |
!> | 2015-02-13 | SB | - | Added the optional `TOUTPUT` interval for regular text output. |
!> | 2026-03 | SB | 4.6 | Added current hard-coded array-capacity reporting to the print file. |
!> @endhistory
   SUBROUTINE INFR

      IMPLICIT NONE

      INTEGER :: nxplus, isyear, ismth, isday, ishour, ismin, ieyear, iemth, ieday, iehour, iemin, &
                 jsyear, jsmth, jsday, jshour, jsmin, jcyear, jcmth, jcday, jchour, jcmin, j, k, &
                 nlyrct, ipr, idmc, idra, idve, idlyr, i1, i2, i, ipflg, iel, ios
      DOUBLE PRECISION :: tthx
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'FRmod:INFR' !! Location string for read-error reports.

      WRITE (FID_logfile, 10)
10    FORMAT('1', //T10, '                                E'/T10, &
             ' EUROPEAN HYDROLOGIC SYSTEM  S  H  E  SYSTEME HYDROLOGIQUE EUROPEEN'/T10, &
             '                                S'/)

      ! PRINT THE CURRENT VERSION NUMBER
      IF (BDEVER) THEN
         WRITE (FID_logfile, 16) SHEVER
      ELSE
         WRITE (FID_logfile, 15) SHEVER
      END IF
16    FORMAT(/'SHETRAN VERSION NUMBER: ', F5.1, ' ')
15    FORMAT(/'SHETRAN VERSION NUMBER: ', F5.1)

      WRITE (FID_logfile, 17) BANNER
17    FORMAT(/A80/)

      write (FID_logfile, *)
      write (FID_logfile, *)
      write (FID_logfile, '(A)') ' SHETRAN file folder = '
      write (FID_logfile, '(1X,A)') DIRQQ
      write (FID_logfile, '(A)') ' SHETRAN rundata name = '
      write (FID_logfile, '(A)') ' rundata_'//trim(cnam)//'.txt'
      write (FID_logfile, *)
      write (FID_logfile, *)
      write (FID_logfile, *)

! READ AND PRINT JOB TITLE.
      ! :FR1
      WRITE (FID_logfile, '(A)') 'Catchment Name '
      WRITE (FID_logfile, '(A)') '************** '
      READ (FRD, '(A)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)

      WRITE (FID_logfile, '(A)') TITLE

      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Fixed array sizes in this version of SHETRAN '
      WRITE (FID_logfile, '(A)') '******************************************** '
    WRITE (FID_logfile, '(A)') 'Grid points in x,y directions, river links, total no of elements. THESE ARE THE MOST IMPROTANT ONES'
      WRITE (FID_logfile, '(4(A,I0))') ' NXEE = ', nxee, '  NYEE = ', nyee, '  NLFEE = ', nlfee, '  NELEE = ', nelee
      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Grid points in vertical'
      WRITE (FID_logfile, '(1(A,I0))') ' LLEE = ', llee
      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Vegetation types, soil typess (NVEE also used for number of precipitation and pet stations)'
      WRITE (FID_logfile, '(2(A,I0))') ' NVEE = ', nvee, '  NSEE = ', nsee
      WRITE (FID_logfile, *)
      WRITE(FID_logfile, '(A)') 'Tables in the VSS component, time varying veg breakpoints, Tables in the ET component (max number of PSI/RCF/FET values, Maximum number of ssoi layers'
      WRITE (FID_logfile, '(4(A,I0))') ' NVSEE = ', NVSEE, '  NVBP = ', NVBP, '  NUZTAB = ', NUZTAB, '  NLYREE = ', NLYREE
      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Tables used in OC component, sediment sze fractions'
      WRITE (FID_logfile, '(2(A,I0))') ' NOCTAB = ', NOCTAB, '  NSEDEE = ', NSEDEE
      WRITE (FID_logfile, *)
      WRITE(FID_logfile, '(A)') 'Number of contaminants, number of overlaps, number of plants in an element, total number of plants for contaminants'
      WRITE (FID_logfile, '(4(A,I0))') ' NCONEE = ', NCONEE, '  NOLEE = ', NOLEE, '  NPLTEE = ', NPLTEE, '  NPELEE = ', NPELEE
      WRITE (FID_logfile, *)
      WRITE (FID_logfile, '(A)') 'Number of snow meltwater slugs, Size of internal tables for channel conveyance'
      WRITE (FID_logfile, '(2(A,I0))') ' max_no_snowmelt_slugs = ', max_no_snowmelt_slugs, '  NXSCEE = ', NXSCEE
      WRITE (FID_logfile, *)

      WRITE (FID_logfile, 20)
20    FORMAT(/' ^^^ ENTER INFR ^^^')

      ! READ AND PRINT MODEL SIZE, TOTAL SIMULATION TIME, GRID SIZES AND
      ! PRINTING CONTROL.
      ! :FR2
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) NX, NY
      CALL errstat_read(ios, location, emsg)
      NXPLUS = 0

      ! :FR4
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN
      CALL errstat_read(ios, location, emsg)

      ! :FR6
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN
      CALL errstat_read(ios, location, emsg)

      ! READ START TIMES FOR SEDIMENT AND CONTAMINANT COMPONENTS
      ! :FR7a
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN
      CALL errstat_read(ios, location, emsg)

      ! :FR7c
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN
      CALL errstat_read(ios, location, emsg)

      NXM1 = NX - 1
      NYM1 = NY - 1
      NXP1 = NX + 1
      NYP1 = NY + 1

      ! :FR8
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(10F7.0)', IOSTAT=ios, IOMSG=emsg) (DXIN(J), J=1, NXM1)
      CALL errstat_read(ios, location, emsg)

      ! :FR10
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(10F7.0)', IOSTAT=ios, IOMSG=emsg) (DYIN(K), K=1, NYM1)
      CALL errstat_read(ios, location, emsg)

      ! :FR12
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(F7.0, I7, 4L7, F7.0)', IOSTAT=ios, IOMSG=emsg) DTAO, IAOUT, BINFRP, BFRTS1, BFRTS2, BSTORE, PSTART
      CALL errstat_read(ios, location, emsg)

      ! :FR20
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(4F7.0,L7)', IOSTAT=ios, IOMSG=emsg) PMAX, PALFA, QMAX, TMAX, BSOFT
      CALL errstat_read(ios, location, emsg)

      ! PMAX = one
      ! PALFA = 0.15D0
      IF (TMAX > 2.0D0) THEN
         WRITE (FID_logfile, *) '^^^ TIMESTEP LIMITED TO 2 HOURS ^^^'
         TMAX = 2.0D0
      END IF

      PREST = (1.0D0 + PALFA)

      IF (IAOUT == 2) THEN
         ! :FR22
         READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
         CALL errstat_read(ios, location, emsg)
         READ (FRD, '(10L7)', IOSTAT=ios, IOMSG=emsg) BPPNET, BPEPOT, BPQOC, BPDEP, BPQF, BPQH, BPQSZ, BPHSZ, BPBAL, BPSD
         CALL errstat_read(ios, location, emsg)
      END IF

      ! ---- BEX** = TRUE FOR EXECUTION AND FALSE FOR NO EXECUTION
      !      NOTE: COMPONENTS FR,ET,UZ,OC,SZ,EX ARE ALWAYS INCLUDED
      ! :FR24
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(10L7)', IOSTAT=ios, IOMSG=emsg) BEXSM, BEXBK, BEXSY, BEXCM
      CALL errstat_read(ios, location, emsg)
      BEXET = .TRUE.
      BEXUZ = .TRUE.
      BEXOC = .TRUE.
      BEXSZ = .TRUE.
      BEXEX = .TRUE.

      ! LOGICAL PARAMETERS FOR HOT START
      ! :FR26
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(2L7, 2F7.2)', IOSTAT=ios, IOMSG=emsg) BHOTRD, BHOTPR, BHOTTI, BHOTST
      CALL errstat_read(ios, location, emsg)

      ! PRINT INITIALISATION DATA
      WRITE (FID_logfile, 150) NX, NY
150   FORMAT('0'//, ' GRID SPECIFICATION'/80('*')//, ' NX = ', I4, 21X, 'NY = ', I4)
      WRITE (FID_logfile, 160) (DXIN(J), J=1, NXM1)
160   FORMAT('0', 'H-H GRID SIZES (METERS) IN X-DIRECTION', /, (1X, 10G11.4))
      WRITE (FID_logfile, 170) (DYIN(K), K=1, NYM1)
170   FORMAT('0', 'H-H GRID SIZES (METERS) IN Y-DIRECTION', /, (1X, 10G11.4))
      WRITE (FID_logfile, 200)
200   FORMAT(' ', 80('*'))

      ! CONVERT STARTTIME AND ENDTIME TO HOURS.
      TIH = HOUR_FROM_DATE(ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN)
      TTH = HOUR_FROM_DATE(IEYEAR, IEMTH, IEDAY, IEHOUR, IEMIN)
      TTHX = TTH - TIH

      WRITE (FID_logfile, 210) ISYEAR, ISMTH, ISDAY, ISHOUR, ISMIN, IEYEAR, &
         IEMTH, IEDAY, IEHOUR, IEMIN, TTHX
210   FORMAT('0'//, ' START OF SIMULATION  : ', 5I6, /, &
             ' END OF SIMULATION    : ', 5I6, /, &
             ' LENGTH OF SIMULATION : ', F10.2, ' HOURS.')

      ! store start time for mass balance
      mbyear = isyear
      mbmon = ismth
      mbday = isday

      IF (BEXSY) THEN
         TSH = HOUR_FROM_DATE(JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN)
         WRITE (FID_logfile, 211) JSYEAR, JSMTH, JSDAY, JSHOUR, JSMIN, (TSH - TIH)
211      FORMAT(//' START OF SEDIMENT SIMULATION  : ', 5I6, / &
                 '           AT SIMULATION HOUR  : ', F8.2)
      END IF

      IF (BEXCM) THEN
         TCH = HOUR_FROM_DATE(JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN)
         WRITE (FID_logfile, 212) JCYEAR, JCMTH, JCDAY, JCHOUR, JCMIN, (TCH - TIH)
212      FORMAT(//' START OF CONTAMINANT SIMULATION  : ', 5I6, / &
                 '               AT SIMULATION HOUR  : ', F8.2)
      END IF

      WRITE (FID_logfile, 215) TMAX
215   FORMAT('0', //, ' BASIC TIMESTEP (HOURS) :', F8.3)

      WRITE (FID_logfile, 220) DTAO
220   FORMAT('0'//, ' PRINTING CONTROL - ALL RESULTS PRINTED AT', &
             ' INTERVALS OF DTAO = ', F7.2, ' HOURS.')

      IF (.NOT. BSTORE) WRITE (FID_logfile, 230)
230   FORMAT('0'//, ' RESULTS NOT REQUIRED ON FILE STORE.')

      IF (BSTORE) WRITE (FID_logfile, 240)
240   FORMAT('0'//, ' RESULTS RECORDED ON FILE STORE.')

      ! READ AND PRINT NM,NRAIN,NV AND NS.
      ! :FR28
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(5I7)', IOSTAT=ios, IOMSG=emsg) NM, NRAIN, NV, NS, NLYRCT
      CALL errstat_read(ios, location, emsg)
      WRITE (FID_logfile, 260) NM, NRAIN, NV, NS, NLYRCT
260   FORMAT('0'//, ' NO. OF METEOROLOGICAL SITES = ', I3, /, &
             ' NO. OF RAINFALL STATIONS = ', I3, /, &
             ' NO. OF VEGETATION TYPES = ', I3, /, &
             ' NO. OF SOIL TYPES = ', I3, /, &
             ' NO. OF SOIL HORIZON CATEGORIES = ', I3)

      ! READ RIVER LINING PARAMETERS.  BLOWP,DB,CCB,BEXTS1
      ! :FR30
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)

      ! SET PRINTING CONTROL FOR SUBROUTINES AREADR AND AREADI.
      IPR = 0
      IF (BINFRP) IPR = 1

      ! READ DEFAULT VALUES FOR MET,RAIN,VEG,SOIL-CODES. APPLIED WHEN > 0
      ! :FR32
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (FRD, '(6I7)', IOSTAT=ios, IOMSG=emsg) IDMC, IDRA, IDVE, IDLYR
      CALL errstat_read(ios, location, emsg)
      WRITE (FID_logfile, 300) IDMC, IDRA, IDVE, IDLYR
300   FORMAT('0', /, ' DEFAULT METEOROLOGICAL STATION CODE =', I3, /, &
             1X, 'DEFAULT RAINFALL STATION CODE       =', I3, /, &
             1X, 'DEFAULT VEGETATION GRID CODE        =', I3, /, &
             1X, 'DEFAULT SOIL HORIZON CATEGORY CODE  =', I3)

      ! READ IN MAIN CATCHMENT DEFINITION ARRAY, INGRID
      ! (NB. THIS IS NOT READ IN USING AREAD ROUTINES, AS THE
      ! INDEX ARRAY ICMREF HASN'T BEEN SET UP YET)
      !
      ! :FR34
      READ (FRD, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      IF (BINFRP) WRITE (FID_logfile, '( / 20A4)') TITLE

      DO I1 = 1, NY
         K = NY + 1 - I1
         READ (FRD, '(I7, 1X, 500I1)', IOSTAT=ios, IOMSG=emsg) I2, (INGRID(J, K), J=1, NX)
         CALL errstat_read(ios, location, emsg)
         IF (BINFRP) WRITE (FID_logfile, '(I7, 1X, 500I1)') I2, (INGRID(J, K), J=1, NX)

         ! Catchment array definition check
         IF (I2 /= K) THEN
            WRITE (FID_logfile, 314) TITLE, I2
314         FORMAT(//2X, 'ERROR IN DATA ', 20A4, //2X, 'IN THE VICINITY OF ', &
                    'LINE K= ', I5)
            CALL ERR_STOP(255)
         END IF
      END DO

      ! SET INGRID TO BE ITS INTERNAL VALUES FOR SHE (=0 IN CATCHMENT, -1 OTHER)
      DO I = 1, NX
         DO J = 1, NY
            IF (INGRID(I, J) == 1) THEN
               INGRID(I, J) = 0
            ELSE
               INGRID(I, J) = -1
            END IF
         END DO
      END DO

      ! READ THE CODES FOR OVERLAND/CHANNEL FLOW GRID BOUNDARIES
      ! :FR35a
      CALL OCLTL(NXP1, NY, LCODEX, NXE, NYE, FRD, FID_logfile, BINFRP)
      ! :FR35c
      CALL OCLTL(NX, NYP1, LCODEY, NXE, NYE, FRD, FID_logfile, BINFRP)

      ! INITIALISE GLOBAL INDEX ARRAY
      CALL FRIND(BINFRP)

      ! READ / PRINT ARRAYS ZGRUND, NMC, NRAIN, NVC.
      ! SET EQUAL TO DEFAULT VALUES IF THESE ARE TO BE USED.
      !
      ! :FR37
      CALL AREADR(ZGRUND, IPR, FRD, FID_logfile)

      IPFLG = 3
      ! :FR43
      IF (IDMC > 0) CALL AREADI(NMC, IPFLG, IDMC, FID_logfile, NM)
      IF (IDMC <= 0) CALL AREADI(NMC, IPR, FRD, FID_logfile, NM)
      ! :FR46
      IF (IDRA > 0) CALL AREADI(NRAINC, IPFLG, IDRA, FID_logfile, NRAIN)
      IF (IDRA <= 0) CALL AREADI(NRAINC, IPR, FRD, FID_logfile, NRAIN)
      ! :FR49
      IF (IDVE > 0) CALL AREADI(NVC, IPFLG, IDVE, FID_logfile, NV)
      IF (IDVE <= 0) CALL AREADI(NVC, IPR, FRD, FID_logfile, NV)

      ! :FR52
      READ (FRD, '(20A4)', IOSTAT=ios) TITLE
      IF (ios == 0) READ (FRD, *, IOSTAT=ios) TOUTPUT

      ! Check if the optional outputs read successfully
      IF (ios /= 0) TOUTPUT = 24.0D0

      ! INITIALIZATION OF SOME PARAMETERS.
      ALLOUT = DTAO + PSTART
      NXEP1 = NXE + 1
      NYEP1 = NYE + 1

      ! INITIALISATION OF ISORT ARRAY
      DO IEL = 1, total_no_elements
         ISORT(IEL) = IEL
      END DO

      WRITE (FID_logfile, 430)
430   FORMAT('0'//, ' EXIT INFR')

   END SUBROUTINE INFR

!> @brief Supplies ET defaults when the evapotranspiration component is disabled.
!>
!> The routine only writes an `ENTER DINET` message and sets `BMETAL=.TRUE.`.
!> The commented assignments to rainfall, evaporation, interception, root-zone
!> evaporation, drainage, and soil evaporation are inactive. In the current frame
!> initialisation, ET is forced active by [[infr]], so this dummy path is retained
!> mainly for legacy component structure.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1991 | GP | 0.1-0.2 | Added and reduced the legacy dummy-component initialisation set. |
!> | 2026-04 | SvB | 4.6.1 | Retained the inactive compatibility hook during control-flow cleanup. |
!> @endhistory
   SUBROUTINE DINET

      IMPLICIT NONE

      WRITE (*, '(/, /, "ENTER DINET")')
      BMETAL = .TRUE.

      ! PNET=0.0003
      ! PE=0.0
      ! EINT=0.0
      ! ERZ=0.0
      ! DRAIN=0.0
      ! ESOIL=0.0

   END SUBROUTINE DINET

!> @brief Supplies overland/channel defaults when that component is disabled.
!>
!> This routine belongs to the legacy SHETRAN-UK dummy component set (DUM),
!> which contains dummy versions of OC, ET, UZ, SZ, and EXSZOC routines. These
!> minimal dummy components are not currently used. `DINOC` only writes an
!> `ENTER DINOC` message and returns.
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-06 | GP | 0.1 | Added dummy components for use with V-catchment tests. |
!> | 1991-12 | GP | 0.2 | Reduced to minimal versions, not currently used. |
!> @endhistory
   SUBROUTINE DINOC

      IMPLICIT NONE

      WRITE (*, '(/, /, "ENTER DINOC")')

   END SUBROUTINE DINOC

!> @brief Retains the no-op overland/channel input hook required by the legacy component structure.
!>
!> `DOCIN` performs no work and has no side effects.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1991 | GP | 0.1-0.2 | Added the legacy dummy OC component hooks. |
!> | 2026-04-13 | SvB | 4.6.1 | Marked the no-op input hook pure. |
!> @endhistory
   PURE SUBROUTINE DOCIN

      IMPLICIT NONE

   END SUBROUTINE DOCIN

END MODULE frame_setup

