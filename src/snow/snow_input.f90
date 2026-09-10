!> summary: The `SM4`--`SM14` snowmelt input records.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> [[INSM]] reads the snowmelt data groups into [[snow_config]] and sets the
!> initial snowpack: either one uniform depth and temperature, or the spatial
!> fields of records `SM11`/`SM14` when `NSD=1`. It is called once, from frame
!> initialisation, and only when snowmelt is enabled.
!>
!> This module holds exactly one procedure; the parameters it writes are in
!> [[snow_config]]. `INSM` was private inside `FRmod` and is public here
!> because frame initialisation now has to call across a module boundary to
!> reach it.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1990-06 to 1998-03 | GP / RAH | 2.2--4.2 | Developed the snowmelt component: variable snowpack, `PNSNOW`, and explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90 and replaced the `SM.F` files. |
!> | 2026-04-03 to 2026-04-13 | SvB | 4.6.1 | Modernisation pass: removed the `1H0` Hollerith descriptor and the `GOTO`-driven control flow, added `IMPLICIT NONE`/`INTENT`, and pre-computed the repeated temperature-ratio subexpression. |
!> | 2026-09-10 | SvB | - | Split out of FRmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE snow_input

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, zero
   USE element_geometry, ONLY: total_no_elements
   USE grid_topology, ONLY: NGDBGN
   USE AL_D, ONLY: NM
   USE file_units, ONLY: FID_logfile, SMD
   USE grid_arrays, ONLY: AREADR
   USE snow_config, ONLY: BINSMP, DDF, HEAD, IMET, NSD, RHODEF, RHOS, ZDS, ZOS, ZUS
   USE snow_state, ONLY: MSM, NSMC, RHOSAR, SD, SF, TS
   USE error_status, ONLY: errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: INSM

CONTAINS

!> @brief Reads snowmelt component input and initialises snowpack state.
!>
!> Key snowmelt variables and units are:
!>
!> | Variable | Meaning | Units |
!> |:---------|:--------|:------|
!> | `UNIFSD` | Snow depth when a uniform initial snow depth is supplied. | mm snow |
!> | `SD` | Snow depth. | mm snow |
!> | `DDF` | Degree-day factor. | mm/s/C |
!> | `RHOS` | Specific gravity of snow. | - |
!> | `TSIN` | Initial snow temperature. | C |
!> | `TS` | Snow temperature. | C |
!> | `NSMC` | Number of meltwater slugs being routed through the snowpack. | - |
!> | `MSM` | Snowmelt method flag: `1` degree-day, `2` energy budget. | - |
!> | `ZOS`, `ZDS`, `ZUS` | Snow aerodynamic roughness, zero-plane displacement, and anemometer height for energy-budget snowmelt. | m |
!> | `IMET` | Meteorological station element numbers for energy-budget windspeed correction. | element |
!>
!> | Branch | Input and initialisation |
!> |:-------|:-------------------------|
!> | `MSM=1` | Degree-day method; `TSIN` is forced to zero and energy-budget aerodynamic/location records are skipped. |
!> | `MSM/=1` | Energy-budget method; reads `ZOS`, `ZDS`, `ZUS`, and `IMET(1:NM)`. |
!> | `NSD=0` | Uniform initial snowpack; sets all `RHOSAR` to default `RHOS`, reads one `UNIFSD`, then sets all `SD` to that depth. |
!> | `NSD/=0` | Spatial snowpack; reads distributed `SD` and `RHOSAR` arrays with `AREADR`. |
!>
!> After either snowpack branch, `NSMC` is reset to zero, `TS` is set to the
!> effective `TSIN`, and snowfall `SF` is set to zero for every non-link element.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1981-03 | JCB | - | Created snowmelt component (SM). |
!> | 1989-09 | GP | 2.1 | SHE88 implementation on Newcastle AMDAHL. |
!> | 1990-06 | GP | 2.2 | Variable snowpack, low-temp correction, shallow pack, SHETRAN amendments. |
!> | 1992-11 | SPA | 3.x | Removed incorrect snowpack temperature control, further low-temp correction. |
!> @endhistory
   SUBROUTINE INSM

      IMPLICIT NONE

      ! Locals
      INTEGER :: N, IEL, I, ios
      DOUBLE PRECISION :: TSIN, UNIFSD
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'FRmod:INSM' !! Location string for read-error reports.

      ! READ PRINT CONTROL PARAMETERS
      READ (SMD, '(20A4)', IOSTAT=ios, IOMSG=emsg) HEAD
      CALL errstat_read(ios, location, emsg)
      READ (SMD, '(L7)', IOSTAT=ios, IOMSG=emsg) BINSMP
      CALL errstat_read(ios, location, emsg)
      IF (BINSMP) WRITE (FID_logfile, '(///1X, 20A4)') HEAD

      ! READ SNOWMELT DATA
      READ (SMD, '(20A4)', IOSTAT=ios, IOMSG=emsg) HEAD
      CALL errstat_read(ios, location, emsg)
      READ (SMD, '(2F7.5,F7.2,2I7)', IOSTAT=ios, IOMSG=emsg) DDF, RHOS, TSIN, NSD, MSM
      CALL errstat_read(ios, location, emsg)
      RHODEF = RHOS

      ! Added by spa, 05/11/92.  Snowpack temp no longer needed
      ! for degree day method.  Therefore if msm=1, tsin=0.
      IF (MSM == 1) TSIN = ZERO

      IF (BINSMP) WRITE (FID_logfile, 801) DDF, RHOS, TSIN, MSM

      ! Execute Energy Budget specific reads if MSM > 1
      IF (MSM /= 1) THEN
         ! READ ENERGY BUDGET DATA
         READ (SMD, '(20A4)', IOSTAT=ios, IOMSG=emsg) HEAD
         CALL errstat_read(ios, location, emsg)
         READ (SMD, '(3F7.5)', IOSTAT=ios, IOMSG=emsg) ZOS, ZDS, ZUS
         CALL errstat_read(ios, location, emsg)

         IF (BINSMP) WRITE (FID_logfile, 803) ZOS, ZDS, ZUS

         ! METEOROLOGICAL (WINDSPEED) DATA LOCATION
         READ (SMD, '(20A4)', IOSTAT=ios, IOMSG=emsg) HEAD
         CALL errstat_read(ios, location, emsg)
         READ (SMD, '(10I7)', IOSTAT=ios, IOMSG=emsg) (IMET(N), N=1, NM)
         CALL errstat_read(ios, location, emsg)

         IF (BINSMP) THEN
            WRITE (FID_logfile, 715)
            station_loop: DO N = 1, NM
               WRITE (FID_logfile, '(3X, I4, 10X, I4)') N, IMET(N)
            END DO station_loop
         END IF
      END IF

      ! IS SNOWDEPTH UNIFORM?
      IF (NSD == 0) THEN
         uniform_rho_loop: DO IEL = ngdbgn, total_no_elements
            rhosar(IEL) = RHODEF
         END DO uniform_rho_loop

         ! UNIFORM SNOWDEPTH (MM OF SNOW)
         READ (SMD, '(20A4)', IOSTAT=ios, IOMSG=emsg) HEAD
         CALL errstat_read(ios, location, emsg)
         READ (SMD, '(F7.1)', IOSTAT=ios, IOMSG=emsg) UNIFSD
         CALL errstat_read(ios, location, emsg)

         uniform_sd_loop: DO IEL = ngdbgn, total_no_elements
            SD(IEL) = UNIFSD
         END DO uniform_sd_loop

         IF (BINSMP) WRITE (FID_logfile, '(/, 1X, "INITIAL SNOWPACK HAS UNIFORM THICKNESS =", F7.1, 1X, "MM")') UNIFSD
      ELSE
         ! NONUNIFORM SNOWDEPTH (MM OF SNOW)
         I = 0
         IF (BINSMP) I = 1
         CALL AREADR(SD, I, SMD, FID_logfile)
         CALL AREADR(rhosar, I, SMD, FID_logfile)
      END IF

      ! Epilogue Element Processing
      epilogue_loop: DO IEL = ngdbgn, total_no_elements
         ! SET COUNTER FOR SNOWMELT ROUTINE
         NSMC(IEL) = 0
         ! SET SNOW TEMPERATURES
         TS(IEL) = TSIN
         ! SET SNOWFALL
         SF(IEL) = ZERO
      END DO epilogue_loop

      ! FORMAT STATEMENTS

801   FORMAT(/, 'DEGREE DAY FACTOR DDF =', F7.5, 1X, 'MM/S/C', &
              5X, 'SNOW SPECIFIC GRAVITY RHOS =', F7.5/ &
              5X, 'INITIAL SNOW TEMPERATURE =', F7.2, 1X, 'C'/ &
              5X, 'SNOWMELT CALCULATED BY DEGREE DAY IF MSM IS 1', &
              ' AND BY ENERGY BUDGET IF MSM IS 2', 5X, 'MSM =', I3)

803   FORMAT(/, 'ENERGY BUDGET DATA', 3X, 'ROUGHNESS ZOS =', F7.5, 1X, 'M'/ &
              21X, 'ZERO PLANE DISPLACEMENT ZDS =', F7.5, 1X, 'M'/ &
              21X, 'HEIGHT OF ANEMOMETER ZUS =', F7.5, 1X, 'M')

715   FORMAT(/' LOCATION OF MET. STATIONS: '/ &
              ' STATION NO.   ELEMENT NO.')

   END SUBROUTINE INSM

END MODULE snow_input

