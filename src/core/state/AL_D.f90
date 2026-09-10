!> summary: Shared controls, forcing, flow state, and legacy result metadata.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> `AL_D` replaces the legacy `AL.D` common blocks used by SHETRAN's flow
!> components. [[frmod]] establishes the grid, run controls, component flags,
!> file metadata, hotstart state, and water-balance calendar. [[rest]] updates
!> meteorological forcing and timestep control; [[et_process]], [[snowmelt]],
!> [[ocmod]], and [[ocqdqmod]] produce the process arrays; [[zq_tables]] allocates
!> the optional reservoir-table metadata.
!>
!> Fixed arrays retain compile-time capacity bounds. Active element, link,
!> meteorological-site, rainfall-station, vegetation, legacy-result-set, and OC
!> boundary-table ranges are established separately by the corresponding
!> counts. Module state is public by default and, apart from parameter
!> constants, has no declaration initialization.
!>
!> The legacy `BALANC` water-volume accumulator uses these active entries:
!>
!> | Entries | Meaning |
!> |:--------|:--------|
!> | 1:6 | Precipitation, canopy evaporation, soil/surface evaporation, transpiration, base flow, and outlet discharge since the last report (m3). |
!> | 7:12 | Cumulative totals of entries 1:6 (m3). |
!> | 13:17 | Canopy, snow, subsurface, surface-water, and channel storage (m3). |
!> | 18:19 | Current-period and cumulative aquifer-channel exchange (m3). |
!> | 20 | Declared capacity entry; not assigned by current [[mass_balance_report:FRMB]]. |
!>
!> @warning
!> Manual section 2.2 says `PMAX` and `PALFA` are hardcoded and their FR20
!> values ignored. In the current [[frame_setup:INFR]], the assignments that would
!> hardcode them are commented out: both values are read from FR20 and used by
!> [[timestep_control:TMSTEP]]. `TOUTPUT` is likewise an output interval, not an absolute
!> next-output time.
!>
!> No current source routine explicitly assigns `MBLINK`, `MBFACE`, or
!> `MBFLAG`, although [[mass_balance_report:FRMB]] reads them every timestep. The legacy
!> binary-result metadata headed by `NSET`, and the three `*ERRC` error-count
!> arrays read by [[run_summary:extra_output]], also have no current producer.
!> Standard Fortran therefore regards these values as undefined; a compiler's
!> zero-filled static storage is not a portable initialization. This
!> documentation transfer does not alter that behavior.
!>
!> `BEXTS1`, `NEXPO`, `WIDTF`, `ZBED`, `HFLBED`, `ZFBED`, `DZFBED`, `LROOT`,
!> `HFLBNK`, `EPOTR`, `CMEAN`, `SMEAN`, and `ADMEAN` are inactive legacy
!> storage with no current reference outside this module. `NGRID` is zeroed by
!> `FRIND` but is not subsequently read.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-03 | GP | 3.0 | Original version written. |
!> | 1991-06 | GP | 3.1 | Added new shared variables including `NEXPO`. |
!> | 1992-02 | GP | 3.3 | Added soil-layer arrays. |
!> | 1992-06 | GP | 3.4 | Moved selected state to `AL_C`, added flow, snow, error, and storage arrays, and removed obsolete constants. |
!> | 1994-09-28 | RAH | 3.4.1 | Applied the standard header, declared all variables, and removed `INTEGER*2`. |
!> | 1996-01-03 | GP | 4.0 | Moved VSS state to `AL_C`, removed redundant SZ/UZ/EX state, and added mass-balance/result controls. |
!> | 1997-02 | RAH | 4.1 | Removed redundant derived dimensions and obsolete state variables. |
!> | 1998-01 to 1998-11 | RAH | 4.2 | Removed obsolete overland/channel and storage state, reduced the OC boundary table, and defined 14 legacy result classes. |
!> | 2004-07 | JE | - | Converted the shared state to Fortran 95. |
!> | 2015-02 to 2015-04 | SB | - | Added separate temperature streams, configurable regular-output timing, and extra discharge-point input. |
!> | 2020-05-20 | SB | 4.5.0 | Added the ZQ data unit and reservoir-table metadata. |
!> | 2024-03-12 | SB | - | Added the optional phreatic-surface output-point input. |
!> @endhistory
MODULE AL_D
   USE array_limits, ONLY: nelee, NVEE, nxee, nyee, NCONEE, nlfee, NSETEE, LLEE, NOCTAB, NCLASS
   IMPLICIT NONE



! Static integer controls.
   INTEGER :: NM         !! Number of active meteorological sites.
   INTEGER :: NRAIN      !! Number of active rainfall stations.
   INTEGER :: NSET       !! Number of legacy binary result sets; no current producer was found.
   INTEGER :: MBLINK     !! Link whose selected face supplies outlet discharge to the catchment balance.
   INTEGER :: MBFACE     !! Face of `MBLINK` used for catchment-balance discharge.
   INTEGER :: MBFLAG     !! Catchment-balance schedule: 1 daily, any other value monthly.

! Time-dependent integer state.
   INTEGER :: MBYEAR !! Calendar year of the next mass-balance report.
   INTEGER :: MBMON  !! Calendar month of the next mass-balance report.
   INTEGER :: MBDAY  !! Calendar day of the next mass-balance report.

! Static real controls. Absolute times use the same hour count as `TIH`;
! timestep and interval values are in hours unless stated otherwise.
   DOUBLEPRECISION :: PSTART  !! Simulation-relative start time for legacy printed/result output (h).
   DOUBLEPRECISION :: DTMET   !! Combined meteorological input interval (h).
   DOUBLEPRECISION :: BHOTTI  !! Requested/read hotstart time (h).
   DOUBLEPRECISION :: BHOTST  !! Interval between hotstart outputs (h).
   DOUBLEPRECISION :: PMAX    !! Maximum rainfall depth permitted in one model timestep (mm).
   DOUBLEPRECISION :: PALFA   !! Fractional timestep growth factor used by `TMSTEP`.
   DOUBLEPRECISION :: TMAX    !! Maximum/basic coupled model timestep, capped at two hours (h).
   DOUBLEPRECISION :: DTMET2  !! Separate precipitation input interval (h).
   DOUBLEPRECISION :: DTMET3  !! Separate potential-evaporation input interval (h).
   DOUBLEPRECISION :: TOUTPUT !! Interval for regular text/CSV outputs; defaults to 24 h (h).

! Per-step scalar state.
   DOUBLEPRECISION :: PREST  !! Unused legacy value set to `1+PALFA` during frame initialization.
   DOUBLEPRECISION :: HOTIME !! Current/last hotstart time (h).

! Process and optional-file switches.
   LOGICAL :: BEXET      !! Whether evapotranspiration is active; current frame setup always sets true.
   LOGICAL :: BEXUZ      !! Whether the legacy upper-zone process is active; current frame setup always sets true.
   LOGICAL :: BEXEX      !! Whether legacy exchange is active; current frame setup always sets true.
   LOGICAL :: BEXOC      !! Whether overland/channel flow is active; current frame setup always sets true.
   LOGICAL :: BEXSZ      !! Whether saturated-zone flow is active; current frame setup always sets true.
   LOGICAL :: BEXSM      !! Whether snowmelt is enabled by FR25.
   LOGICAL :: BHOTPR     !! Whether periodic hotstart output is enabled.
   LOGICAL :: BHOTRD     !! Whether initial state is read from the hotstart file.
   LOGICAL :: BEXSY      !! Whether sediment transport is enabled by FR25.
   LOGICAL :: BEXCM      !! Whether contaminant transport is enabled by FR25.
   LOGICAL :: ISTA       !! Whether separate maximum/minimum air-temperature streams are available.
   LOGICAL :: isextradis !! Whether the extra-discharge point-selection input is available.
   LOGICAL :: isextrapsl !! Whether the extra phreatic-surface point-selection input is available.

! Static integer arrays.
   INTEGER :: IOCORS(NSETEE)      !! Contaminant/sediment selector for each legacy result set.
   INTEGER :: NMC(NELEE)          !! Meteorological-site category by element.
   INTEGER :: IODATA(NSETEE)      !! Data-type number for each legacy result set.
   INTEGER :: NRAINC(NELEE)       !! Rainfall-station category by element.
   INTEGER :: IOELEM(NSETEE)      !! Positive element number or negative element-class number by legacy result set.
   INTEGER :: IORES(NSETEE)       !! Open unformatted output unit by legacy result set.
   INTEGER :: ICLIST(NELEE,NCLASS) !! Element numbers belonging to each legacy output class.
   INTEGER :: ICLNUM(NCLASS)      !! Number of elements in each legacy output class.



! Static real arrays.
   DOUBLEPRECISION :: IOSTA(NSETEE)  !! Start time for each legacy result set (h).
   DOUBLEPRECISION :: IOSTEP(NSETEE) !! Output interval for each legacy result set (h).
   DOUBLEPRECISION :: IOEND(NSETEE)  !! End time for each legacy result set (h).

! Time-dependent real arrays.
   DOUBLEPRECISION :: precip_m_per_s(NELEE) !! Precipitation rate mapped directly to each element (m/s).
   DOUBLEPRECISION :: OBSPE(NVEE)   !! Potential-evaporation forcing by meteorological site (mm/s).
   DOUBLEPRECISION :: TA(NVEE)      !! Air temperature by meteorological site (degrees C).
   DOUBLEPRECISION :: U(NVEE)       !! Wind speed by meteorological site (m/s).
   DOUBLEPRECISION :: VPD(NVEE)     !! Vapour-pressure deficit by meteorological site (mb).
   DOUBLEPRECISION :: RN(NVEE)      !! Net radiation by meteorological site (W/m2).
   DOUBLEPRECISION :: IOTIME(NSETEE) !! Next output time for each legacy result set (h).
   DOUBLEPRECISION :: BALANC(20)     !! Catchment water-volume terms described in the module table (m3).

   CHARACTER(len=200) :: RESFIL !! Path used as the stem for legacy unformatted result files.

!PRIVATE :: NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, NOCTAB
END MODULE AL_D
