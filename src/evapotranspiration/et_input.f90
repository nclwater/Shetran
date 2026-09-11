!> summary: The `ET2`--`ET18` evapotranspiration input records.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[INET]] reads the evapotranspiration data groups into [[et_config]] and
!> opens the meteorological input streams. It is called once, from frame
!> initialisation, after [[et_process:INITIALISE_ETMOD]] has allocated the
!> run-sized state.
!>
!> This module holds exactly one procedure. The parameters `INET` writes are in
!> [[et_config]] rather than here, so that the reader and the process module
!> both depend on the data and not on each other; see [[et_config]]'s header.
!> `INET` was private inside `FRmod` and is public here because frame
!> initialisation now has to call across a module boundary to reach it.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-02 to 1998-10 | GP / RAH | 2.0--4.2 | Developed and reorganised the combined ET component. |
!> | 2008-12 | JE | 4.3.5F90 | Combined the former ET Fortran sources into a single Fortran 90 module. |
!> | 2026-03-19 | SB | 4.6 | Added date-aware meteorological input and the run-sized allocator. |
!> | 2026-04-05 to 2026-04-14 | SvB | - | Removed `ALINIT`/GOTOs and added resistance error 4998. |
!> | 2026-05-03 | SvB | - | Resized `DEL` and explicitly initialized `IUNDEF`. |
!> | 2026-09-10 | SvB | - | Split out of FRmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE et_input

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE
   USE array_limits, ONLY: NVEE
   USE element_geometry, ONLY: total_no_elements
   USE grid_topology, ONLY: NGDBGN
   USE simulation_clock, ONLY: TIMEUZ
   USE AL_D, ONLY: DTMET, DTMET2, DTMET3, ISTA, NM, precip_m_per_s
   USE run_control, ONLY: BHOTRD
   USE file_units, ONLY: EPD, ETD, FID_logfile, MED, PRD, TAH, TAL
   USE et_config, ONLY: BAR, BINETP, BMETAL, BMETDATES, BMETP, CB, CK, CLAI1, CSTCA1, &
                        CSTCAP, FET, MEASPE, MODE, MODECL, MODECS, MODEPL, MODEVH, NCTCLA, &
                        NCTCST, NCTPLA, NCTVHT, NF, PLAI1, PS1, RA, RC, RCF, RELCLA, &
                        RELCST, RELPLA, RELVHT, RTOP, TIMCLA, TIMCST, TIMPLA, TIMVHT, VHT1
   USE et_state, ONLY: CLAI, CSTORE, NRD, NV, PLAI, RDF, RDL, VHT
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal
   USE error_status, ONLY: errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: INET

CONTAINS

!> @brief Reads evapotranspiration input and initialises ET state.
!>
!> `INET` reads meteorological/vegetation mode flags, canopy and aerodynamic
!> parameters, time-varying canopy/ground-cover/leaf-area/height tables, and
!> root density functions used by [[et_process]].
!>
!> It assumes meteorological-site codes and vegetation codes have already been
!> read by the global initialisation routines. Variable names follow the IH SHE
!> Report 8 convention used by the legacy manual and code.
!>
!> | Phase | Main action |
!> |:------|:------------|
!> | Reset state | Clear vegetation defaults, reset `precip_m_per_s`, reset `TIMEUZ`, and clear `CSTORE` only when not reading a hot start. |
!> | ET control records | Read print flags, `BMETAL`, optional `BMETDATES`, input timesteps, and measured-potential-evaporation flags. |
!> | Vegetation loop | Read ET8 parameters, optional time-varying parameter tables, optional `PS1`/`RCF`/`FET` tables, and root-density values. |
!> | Time-series priming | Read and discard the first row from `PRD`/`EPD` when `BMETAL` is true, otherwise from `MED`; also check `TAH`/`TAL` when station temperature output is active. |
!>
!> Shared inputs are:
!>
!> | Group | Variables |
!> |:------|:----------|
!> | ET and meteorological file units | `EPD`, `ETD`, `MED`, `PRD`, `FID_logfile` |
!> | Run dimensions | `total_no_elements`, `NGDBGN`, `NM`, `NRAIN`, `NV` |
!> | Restart control | `BHOTRD` |
!> | Local aerodynamic-array extent | `NVEE` |
!>
!> Initialised shared state is:
!>
!> | Group | Variables |
!> |:------|:----------|
!> | ET timing | `DTMET`, `DTMET2`, `DTMET3`, `TIMEUZ` |
!> | Vegetation/root state | `NRD`, `CLAI`, `RDL`, `PLAI`, `VHT`, `RDF` |
!> | Rainfall and canopy storage | `precip_m_per_s`, `CSTORE` |
!> | ET mode/control flags | `MEASPE`, `MODE`, `NF`, `BMETP`, `BINETP`, `BMETAL`, `BMETDATES`, `BAR` |
!> | Time-varying parameter controls | `MODECS`, `MODEPL`, `MODECL`, `MODEVH`, `NCTCST`, `NCTPLA`, `NCTCLA`, `NCTVHT` |
!> | Canopy/aerodynamic/resistance tables | `CB`, `CK`, `CSTCAP`, `CSTCA1`, `RA`, `RC`, `RTOP`, `PLAI1`, `CLAI1`, `VHT1` |
!> | Soil-moisture-tension tables | `PS1`, `RCF`, `FET` |
!> | Time-varying ratio/time tables | `RELCST`/`TIMCST`, `RELPLA`/`TIMPLA`, `RELCLA`/`TIMCLA`, `RELVHT`/`TIMVHT` |
!>
!> Key ET variables and units are:
!>
!> | Variable | Meaning | Units |
!> |:---------|:--------|:------|
!> | `RA` | Aerodynamic resistance. | s/m |
!> | `RC` | Stomatal/canopy resistance. | s/m |
!> | `CSTCAP` | Canopy storage capacity. | mm |
!> | `CSTORE` | Canopy storage. | mm |
!> | `CK` | Canopy drainage parameter. | mm/s |
!> | `CB` | Canopy drainage parameter. | 1/mm |
!> | `ZO` | Zero-plane displacement. | m |
!> | `ZD` | Roughness height. | m |
!> | `ZU` | Height of anemometer. | m |
!> | `PS1` | Average soil-moisture tension. | m |
!> | `RCF` | Canopy resistance corresponding to `PS1`. | s/m |
!> | `FET` | Actual/potential evapotranspiration ratio `EA/EP`. | nondimensional |
!> | `RDF` | Root distribution function. | nondimensional |
!> | `PLAI` | Ground-cover index. | nondimensional |
!> | `CLAI` | Canopy leaf-area index. | nondimensional |
!> | `VHT` | Canopy height. | m |
!> | `MEASPE` | `0` if potential evaporation is not measured; `1` if measured. | flag |
!> | `BMETDATES` | `TRUE` when PRD/EPD/temperature time-series records include a leading date column. | flag |
!> | `DTMET` | Timestep for full meteorological-data input. | hr |
!> | `DTMET2` | Timestep for precipitation-data input. | hr |
!> | `DTMET3` | Timestep for potential-evaporation-data input. | hr |
!>
!> The `PS1`/`RCF`/`FET` table is read when `MODE` is neither 1 nor 4. For
!> `MODE=1` and `MODE=4`, the table is skipped and the constant `RC` value is
!> reported. When `BAR` is true, the top aerodynamic resistance term is computed
!> as
!>
!> \[
!> RTOP = \frac{\log^2((ZU-ZD)/ZO)}{0.41^2}.
!> \]
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | Legacy | GP | 3.4 | Removed direct meteorological priming from ET input. |
!> | 1994-1998 | RAH | 3.4.1-4.2 | Standardised typing and revised resistance/time-varying tables. |
!> | 2007-04-30 | SB | - | Added `DTMET2`/`DTMET3` to convert breakpoint meteorological data to regularly spaced data. |
!> | 2026-03 | SB | 4.6 | Added date-aware meteorological input and allocatable ET tables. |
!> @endhistory
   SUBROUTINE INET

      IMPLICIT NONE

      ! --- LOCAL VARIABLES ---

      ! Scalars
      INTEGER          :: I, IEL, IIMEAS, J, JJ, JJJ, N1, N2, ios, N
      DOUBLE PRECISION :: DEPTH, ASUM
      CHARACTER(LEN=80):: HEAD
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'FRmod:INET' !! Location string for read-error reports.

      ! Missing local arrays used for Energy Budget calculations
      ! Defined with NVEE size as per the common block logic
      DOUBLE PRECISION :: ZU(NVEE), ZD(NVEE), ZO(NVEE)

      ! Constants
      DOUBLE PRECISION, PARAMETER :: VKSQ = 0.1681D0 ! (0.41^2)

      ! INITIAL VALUES
      init_veg_loop: DO I = 1, NV
         CSTCAP(I) = 0.0D0
         RC(I) = 0.0D0
         BAR(I) = .FALSE.
         MODE(I) = 0
      END DO init_veg_loop

      ! CHECK IF HOTSTART
      IF (.NOT. BHOTRD) THEN
         init_store_loop: DO IEL = NGDBGN, total_no_elements
            CSTORE(IEL) = 0.0D0
         END DO init_store_loop
      END IF

      precip_m_per_s = 0.0D0
      TIMEUZ = 0.0D0

      !-----READ PRINTCONTROL PARAMETERS
      !:ET1
      READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
      CALL errstat_read(ios, location, emsg)

      ! new code 10202026 BMETDATES added
      ! if true then the prd, epd and temperature files contain dates in the first column
      ! for backwards compatibility the default is false and BMETDATES will not be present in line ET1
      BMETDATES = .FALSE.
      READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
      CALL errstat_read(ios, location, emsg)
      emsg = ''
      READ (HEAD, '(4L7)', IOSTAT=ios) BMETP, BINETP, BMETAL, BMETDATES
      IF (ios /= 0) THEN
         READ (HEAD, '(3L7)', IOSTAT=ios, IOMSG=emsg) BMETP, BINETP, BMETAL
         BMETDATES = .FALSE.
      END IF

      ! either the 4-logical or the fallback 3-logical form must parse cleanly
      CALL errstat_read(ios, location, emsg)

      !-----READ TIMESTEP FOR INPUT OF MET AND RAINDATA,
      !     TIMECONSTANT FOR RAINFALL DISTRIBUTION
      !:ET3
      READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
      CALL errstat_read(ios, location, emsg)
      ! Read the breakpoint interval and the regular interpolation intervals.
      READ (ETD, *, IOSTAT=ios, IOMSG=emsg) DTMET, DTMET2, DTMET3
      CALL errstat_read(ios, location, emsg)

      !-----READ WHETHER POTENTIAL EVAP IS MEASURED AND THEREFORE TO
      !     BE READ IN DIRECTLY FOR EACH MET STATION IN TURN.
      !     MEASPE = 0 : POTENTIAL EVAP NOT MEASURED
      !            = 1 : POTENTIAL EVAP MEASURED
      !:ET5
      READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
      CALL errstat_read(ios, location, emsg)
      READ (ETD, '(10I7)', IOSTAT=ios, IOMSG=emsg) (MEASPE(IIMEAS), IIMEAS=1, NM)
      CALL errstat_read(ios, location, emsg)

      !  LOOP ON VEGETATION TYPES....
      veg_type_loop: DO I = 1, NV

         IF (BINETP) WRITE (FID_logfile, "('0'//1X, 'VEGETATION TYPE', I6/1X, 22('*'))") I

         !:ET7
         READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
         CALL errstat_read(ios, location, emsg)
         IF (BINETP) WRITE (FID_logfile, "('0'//1X, A)") TRIM(HEAD)

         !  READ PARAMETER DATA
         READ (ETD, '(L7, 5F7.0, I7/I7, 4F7.0, I7, 3F7.0)', IOSTAT=ios, IOMSG=emsg) &
            BAR(I), RA(I), ZU(I), ZD(I), ZO(I), RC(I), MODE(I), NF(I), &
            PLAI(I), CSTCAP(I), CK(I), CB(I), NRD(I), CLAI(I), VHT(I), RDL(I)
         CALL errstat_read(ios, location, emsg)

         IF (BINETP) WRITE (FID_logfile, "('0', 1X, 'ET COMPONENT WITH MODE', I6, 2X, 'OPERATION')") MODE(I)

         !-----WRITE PARAMETER DATA
         IF (BINETP) WRITE (FID_logfile, "('0', 'PARAMETERS'/1X, 10('*')//10X, 'PLAI', F15.8/10X, "// &
                            "'CSTCAP', F13.8/10X, 'CK', F17.8/10X, 'CB', F17.8/10X, "// &
                            "'CLAI', F15.8/10X, 'VHT', F16.8/10X, 'RDL', F16.8)") &
            PLAI(I), CSTCAP(I), CK(I), CB(I), CLAI(I), VHT(I), RDL(I)

         IF (BAR(I) .AND. BINETP) WRITE (FID_logfile, "(' ', 10X, 'VARIABLE RA WITH'/10X, 'ZO', F17.4/10X, "// &
                                         "'ZD', F18.4/10X, 'ZU', F17.4)") ZO(I), ZD(I), ZU(I)

         IF (.NOT. BAR(I) .AND. BINETP) WRITE (FID_logfile, "(' ', 10X, 'CONSTANT RA =', F10.4)") RA(I)

         !    READ TABULAR VARIATION OF TIME-VARYING PARAMETERS
         !:ET9
         READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
         CALL errstat_read(ios, location, emsg)

         !-----READ MODE: 0=CONSTANT; 1=TIME-VARYING
         READ (ETD, '(4I7)', IOSTAT=ios, IOMSG=emsg) MODECS(I), MODEPL(I), MODECL(I), MODEVH(I)
         CALL errstat_read(ios, location, emsg)

         !-----CHECK MODE FOR TIME-VARYING CSTCAP
         IF (BINETP) WRITE (FID_logfile, "('0', 1X, 'MODE FOR CSTCAP FOR VEGETATION', I3, ' IS', I3, 3X, "// &
                            "'(0=CONSTANT; 1=TIME-VARYING)')") I, MODECS(I)

         IF (MODECS(I) /= 0) THEN
            NCTCST(I) = 1
            CSTCA1(I) = CSTCAP(I)

            !-----READ NUMBER OF VALUES IN CSTCAP VARIATION TABLE
            !:ET11(1/4)
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            READ (ETD, '(I7)', IOSTAT=ios, IOMSG=emsg) JJJ
            CALL errstat_read(ios, location, emsg)
            !:ET13(1/4)
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            IF (BINETP) WRITE (FID_logfile, "('0'//1X, A)") TRIM(HEAD)

            !-----READ TIME-VARYING CSTCAP VALUES
            cstcap_loop: DO JJ = 1, JJJ
               READ (ETD, *, IOSTAT=ios, IOMSG=emsg) RELCST(I, JJ), TIMCST(I, JJ)
               CALL errstat_read(ios, location, emsg)
               IF (BINETP) WRITE (FID_logfile, "(2G10.3)") RELCST(I, JJ), TIMCST(I, JJ)
            END DO cstcap_loop
         END IF

         !-----CHECK MODE FOR TIME-VARYING PLAI
         IF (BINETP) WRITE (FID_logfile, "('0', 1X, 'MODE FOR PLAI FOR VEGETATION', I3, ' IS', I3, 3X, "// &
                            "'(0=CONSTANT; 1=TIME-VARYING)')") I, MODEPL(I)

         IF (MODEPL(I) /= 0) THEN
            NCTPLA(I) = 1
            PLAI1(I) = PLAI(I)

            !-----READ NUMBER OF VALUES IN PLAI VARIATION TABLE
            !:ET11(2/4)
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            READ (ETD, '(I7)', IOSTAT=ios, IOMSG=emsg) JJJ
            CALL errstat_read(ios, location, emsg)
            !:ET13(2/4)
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            IF (BINETP) WRITE (FID_logfile, "('0'//1X, A)") TRIM(HEAD)

            !-----READ TIME-VARYING PLAI VALUES
            plai_loop: DO JJ = 1, JJJ
               READ (ETD, *, IOSTAT=ios, IOMSG=emsg) RELPLA(I, JJ), TIMPLA(I, JJ)
               CALL errstat_read(ios, location, emsg)
               IF (BINETP) WRITE (FID_logfile, "(2G10.3)") RELPLA(I, JJ), TIMPLA(I, JJ)
            END DO plai_loop
         END IF

         !-----CHECK MODE FOR TIME-VARYING CLAI
         IF (BINETP) WRITE (FID_logfile, "('0', 1X, 'MODE FOR CLAI FOR VEGETATION', I3, ' IS', I3, 3X, "// &
                            "'(0=CONSTANT; 1=TIME-VARYING)')") I, MODECL(I)

         IF (MODECL(I) /= 0) THEN
            NCTCLA(I) = 1
            CLAI1(I) = CLAI(I)

            !-----READ NUMBER OF VALUES IN CLAI VARIATION TABLE
            !:ET11(3/4)
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            READ (ETD, '(I7)', IOSTAT=ios, IOMSG=emsg) JJJ
            CALL errstat_read(ios, location, emsg)
            !:ET13(3/4)
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            IF (BINETP) WRITE (FID_logfile, "('0'//1X, A)") TRIM(HEAD)

            !-----READ TIME-VARYING CLAI VALUES
            clai_loop: DO JJ = 1, JJJ
               READ (ETD, *, IOSTAT=ios, IOMSG=emsg) RELCLA(I, JJ), TIMCLA(I, JJ)
               CALL errstat_read(ios, location, emsg)
               IF (BINETP) WRITE (FID_logfile, "(2G10.3)") RELCLA(I, JJ), TIMCLA(I, JJ)
            END DO clai_loop
         END IF

         !-----CHECK MODE FOR TIME-VARYING VHT
         IF (BINETP) WRITE (FID_logfile, "('0', 1X, 'MODE FOR VHT FOR VEGETATION', I3, ' IS', I3, 3X, "// &
                            "'(0=CONSTANT; 1=TIME-VARYING)')") I, MODEVH(I)

         IF (MODEVH(I) /= 0) THEN
            NCTVHT(I) = 1
            VHT1(I) = VHT(I)

            !-----READ NUMBER OF VALUES IN VHT VARIATION TABLE
            !:ET11(4/4)
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            READ (ETD, '(I7)', IOSTAT=ios, IOMSG=emsg) JJJ
            CALL errstat_read(ios, location, emsg)
            !:ET13(4/4)
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            IF (BINETP) WRITE (FID_logfile, "('0'//1X, A)") TRIM(HEAD)

            !-----READ TIME-VARYING VHT VALUES
            vht_loop: DO JJ = 1, JJJ
               READ (ETD, *, IOSTAT=ios, IOMSG=emsg) RELVHT(I, JJ), TIMVHT(I, JJ)
               CALL errstat_read(ios, location, emsg)
               IF (BINETP) WRITE (FID_logfile, "(2G10.3)") RELVHT(I, JJ), TIMVHT(I, JJ)
            END DO vht_loop
         END IF

         !    END OF READING TIME-VARYING PARAMETERS

         !-----CHECK MODE FOR EVAPOTRANSPIRATION CALCULATIONS
         IF (MODE(I) /= 1 .AND. MODE(I) /= 4) THEN
            !  READ AND WRITE PSI/RCF/FET FUNCTION DATA.
            !:ET15
            READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
            CALL errstat_read(ios, location, emsg)
            N1 = NF(I)
            READ (ETD, '(3F7.2)', IOSTAT=ios, IOMSG=emsg) (PS1(I, J), RCF(I, J), FET(I, J), J=1, N1)
            CALL errstat_read(ios, location, emsg)

            IF (BINETP) WRITE (FID_logfile, "('0'//1X, A)") TRIM(HEAD)
            IF (BINETP) WRITE (FID_logfile, "(' ', 3F10.2)") (PS1(I, J), RCF(I, J), FET(I, J), J=1, N1)
         ELSE
            WRITE (FID_logfile, "(' ', 10X, 'CONSTANT RC =', F10.4)") RC(I)
         END IF

         !-----READ AND WRITE ROOT DENSITY FUNCTION DATA
         !:ET17
         READ (ETD, '(A)', IOSTAT=ios, IOMSG=emsg) HEAD
         CALL errstat_read(ios, location, emsg)
         !  NOTE THAT IT IS ASSUMED HERE THAT DEPTHS CORRESPOND
         !  TO THE NODE DEPTHS FOR THE UZ SOLUTION, SO THAT
         !  EACH NODE IN THE ROOT ZONE HAS A CORRESPONDING RDF
         !  VALUE.  THE VALUES SHOULD BE INPUT FROM THE SURFACE
         !  DOWNWARDS.
         IF (BINETP) WRITE (FID_logfile, "('0'//1X, A)") TRIM(HEAD)

         ASUM = 0.0D0
         N2 = NRD(I)

         rdf_loop: DO J = 1, N2
            READ (ETD, '(2F7.4)', IOSTAT=ios, IOMSG=emsg) DEPTH, RDF(I, J)
            CALL errstat_read(ios, location, emsg)
            IF (BINETP) WRITE (FID_logfile, "(' ', 2F15.6)") DEPTH, RDF(I, J)
            ASUM = ASUM + RDF(I, J)
         END DO rdf_loop

         IF (BINETP) WRITE (FID_logfile, "('0', 1X, 'SUM OF RDF VALUES IS', F10.4)") ASUM

         IF (BAR(I)) RTOP(I) = LOG((ZU(I) - ZD(I))/ZO(I))**2/VKSQ

      END DO veg_type_loop
      !-----END OF VEGETATION LOOP

      !    READ IN METEOROLOGICAL DATA
      IF (BMETAL) THEN
         READ (PRD, *, IOSTAT=ios)
         IF (ios /= 0) CALL RAISE_ERROR(ERRLVL_fatal, 1063, FID_logfile, 0, 0, 'no data in prd file')

         READ (EPD, *, IOSTAT=ios)
         IF (ios /= 0) CALL RAISE_ERROR(ERRLVL_fatal, 1064, FID_logfile, 0, 0, 'no data in epd file')
      ELSE
         READ (MED, *, IOSTAT=ios)
         IF (ios /= 0) CALL RAISE_ERROR(ERRLVL_fatal, 1065, FID_logfile, 0, 0, 'no data in med file')
      END IF

      IF (ISTA) THEN
         READ (TAH, *, IOSTAT=ios)
         IF (ios /= 0) CALL RAISE_ERROR(ERRLVL_fatal, 1066, FID_logfile, 0, 0, 'no data in air temp - high file')

         READ (TAL, *, IOSTAT=ios)
         IF (ios /= 0) CALL RAISE_ERROR(ERRLVL_fatal, 1067, FID_logfile, 0, 0, 'no data in air temp - low file')
      END IF

   END SUBROUTINE INET

END MODULE et_input

