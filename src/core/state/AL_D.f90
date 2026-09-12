!> summary: Shared controls, forcing, flow state, and legacy result metadata.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> `AL_D` replaces the legacy `AL.D` common blocks used by SHETRAN's flow
!> components. [[frmod]] establishes the grid, run controls, component flags,
!> file metadata, hotstart state, and water-balance calendar. [[rest]] updates
!> meteorological forcing and timestep control; [[etmod]], [[smmod]],
!> [[ocmod]], and [[ocqdqmod]] produce the process arrays; [[zqmod]] allocates
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
!> | 20 | Declared capacity entry; not assigned by current [[frmod:frmb]]. |
!>
!> @warning
!> Manual section 2.2 says `PMAX` and `PALFA` are hardcoded and their FR20
!> values ignored. In the current [[frmod:infr]], the assignments that would
!> hardcode them are commented out: both values are read from FR20 and used by
!> [[rest:tmstep]]. `TOUTPUT` is likewise an output interval, not an absolute
!> next-output time.
!>
!> No current source routine explicitly assigns `MBLINK`, `MBFACE`, or
!> `MBFLAG`, although [[frmod:frmb]] reads them every timestep. The legacy
!> binary-result metadata headed by `NSET`, and the three `*ERRC` error-count
!> arrays read by [[rest:extra_output]], also have no current producer.
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
   USE SGLOBAL, ONLY : NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, NOCTAB
   IMPLICIT NONE

   INTEGER, PARAMETER :: NCLASS = 14 !! Number of element classes supported by the legacy binary-result format.

! File units occupy their rundata positions.
   INTEGER, PARAMETER :: FRD = 10      !! Frame/common data input unit.
   INTEGER, PARAMETER :: OCD = 12      !! Overland/channel data input unit.
   INTEGER, PARAMETER :: ETD = 13      !! Evapotranspiration data input unit.
   INTEGER, PARAMETER :: PPD = 14      !! Reserved precipitation-data input unit.
   INTEGER, PARAMETER :: SMD = 15      !! Optional snowmelt data input unit.
   INTEGER, PARAMETER :: BKD = 16      !! Optional bank-element data input unit.
   INTEGER, PARAMETER :: MED = 19      !! Combined meteorological time-series input unit.
   INTEGER, PARAMETER :: PRD = 20      !! Precipitation time-series input unit.
   INTEGER, PARAMETER :: EPD = 21      !! Potential-evaporation time-series input unit.
   INTEGER, PARAMETER :: TIM = 22      !! Time-counter/status file unit.
   INTEGER, PARAMETER :: RES = 27      !! Legacy unformatted result-metadata file unit.
   INTEGER, PARAMETER :: HOT = 28      !! Hotstart input/output unit.
   INTEGER, PARAMETER :: VED = 30      !! Reserved vegetation-data input unit.
   INTEGER, PARAMETER :: OFB = 37      !! Time-varying overland/channel flow-boundary unit.
   INTEGER, PARAMETER :: OHB = 38      !! Time-varying overland/channel head-boundary unit.
   INTEGER, PARAMETER :: DIS = 41      !! Regular outlet-discharge CSV output unit.
   INTEGER, PARAMETER :: VSE = 42      !! Hotstart/VSS-state output unit.
   INTEGER, PARAMETER :: MAS = 43      !! Mass-balance output unit.
   INTEGER, PARAMETER :: DIS2 = 44     !! Every-timestep outlet-discharge output unit.
   INTEGER, PARAMETER :: TAH = 45      !! Maximum-air-temperature time-series input unit.
   INTEGER, PARAMETER :: TAL = 46      !! Minimum-air-temperature time-series input unit.
   INTEGER, PARAMETER :: disextra = 47 !! Input unit selecting additional discharge element/face points.
   INTEGER, PARAMETER :: zqd = 51      !! ZQ reservoir/weir-table data input unit.
   INTEGER, PARAMETER :: pslextra = 52 !! Input unit selecting additional phreatic-surface elements.

! Static integer controls.
   INTEGER :: MSM        !! Snowmelt method: 0 disabled, 1 degree-day, or 2 energy-budget.
   INTEGER :: NM         !! Number of active meteorological sites.
   INTEGER :: NRAIN      !! Number of active rainfall stations.
   INTEGER :: NSET       !! Number of legacy binary result sets; no current producer was found.
   INTEGER :: MBLINK     !! Link whose selected face supplies outlet discharge to the catchment balance.
   INTEGER :: MBFACE     !! Face of `MBLINK` used for catchment-balance discharge.
   INTEGER :: MBFLAG     !! Catchment-balance schedule: 1 daily, any other value monthly.
   INTEGER :: NXP1       !! Active grid helper value `NX+1`.
   INTEGER :: NYP1       !! Active grid helper value `NY+1`.
   INTEGER :: NXM1       !! Active grid helper value `NX-1`.
   INTEGER :: NYM1       !! Active grid helper value `NY-1`.
   INTEGER :: NXEP1      !! Capacity helper value `NXE+1`.
   INTEGER :: NYEP1      !! Capacity helper value `NYE+1`.
   INTEGER :: NoZQTables !! Number of reservoir ZQ tables read from `zqd`.
   INTEGER :: ZQTableRef !! Index of the ZQ table selected for the current link-face calculation.
   INTEGER, PARAMETER :: NXE = NXEE !! Legacy x workspace capacity alias.
   INTEGER, PARAMETER :: NYE = NYEE !! Legacy y workspace capacity alias.

! Time-dependent integer state.
   INTEGER :: NSTEP  !! Current coupled simulation timestep number.
   INTEGER :: NRPD   !! Legacy precipitation-record counter.
   INTEGER :: NSMT   !! Current ET/snowmelt coupling control for one element.
   INTEGER :: MBYEAR !! Calendar year of the next mass-balance report.
   INTEGER :: MBMON  !! Calendar month of the next mass-balance report.
   INTEGER :: MBDAY  !! Calendar day of the next mass-balance report.

! Static real controls. Absolute times use the same hour count as `TIH`;
! timestep and interval values are in hours unless stated otherwise.
   DOUBLEPRECISION :: PSTART  !! Simulation-relative start time for legacy printed/result output (h).
   DOUBLEPRECISION :: DTMET   !! Combined meteorological input interval (h).
   DOUBLEPRECISION :: QMAX    !! Maximum permitted overland/channel face discharge magnitude (m3/s); nonpositive disables the check.
   DOUBLEPRECISION :: BHOTTI  !! Requested/read hotstart time (h).
   DOUBLEPRECISION :: BHOTST  !! Interval between hotstart outputs (h).
   DOUBLEPRECISION :: PMAX    !! Maximum rainfall depth permitted in one model timestep (mm).
   DOUBLEPRECISION :: PALFA   !! Fractional timestep growth factor used by `TMSTEP`.
   DOUBLEPRECISION :: TMAX    !! Maximum/basic coupled model timestep, capped at two hours (h).
   DOUBLEPRECISION :: CAREA   !! Total active catchment plan area (m2).
   DOUBLEPRECISION :: BWIDTH  !! Nominal explicit-bank width used in frame geometry (m).
   DOUBLEPRECISION :: TTH     !! Simulation end as an absolute hour count (h).
   DOUBLEPRECISION :: DTMET2  !! Separate precipitation input interval (h).
   DOUBLEPRECISION :: DTMET3  !! Separate potential-evaporation input interval (h).
   DOUBLEPRECISION :: TOUTPUT !! Interval for regular text/CSV outputs; defaults to 24 h (h).

! Per-step scalar state.
   DOUBLEPRECISION :: UZVAL  !! Next upper-zone/VSS solution time used in hotstart handling (h).
   DOUBLEPRECISION :: OCNOW  !! Start time of the current overland/channel step (h).
   DOUBLEPRECISION :: OCNEXT !! Duration of the current overland/channel step (h).
   DOUBLEPRECISION :: HRUZ   !! Current element's surface-water depth workspace (m).
   DOUBLEPRECISION :: PNET   !! Current element's net throughfall rate workspace (mm/s).
   DOUBLEPRECISION :: PE     !! Current element's available potential-evaporation rate (mm/s).
   DOUBLEPRECISION :: EINT   !! Current element's canopy-interception evaporation over the step (mm).
   DOUBLEPRECISION :: ERZ    !! Current element's root-zone extraction rate workspace (mm/s).
   DOUBLEPRECISION :: DRAIN  !! Current element's canopy drainage over the step (mm).
   DOUBLEPRECISION :: ESOIL  !! Current element's soil-evaporation rate workspace (mm/s).
   DOUBLEPRECISION :: AE     !! Current cell's actual-evapotranspiration rate workspace (mm/s).
   DOUBLEPRECISION :: CSTOLD !! Current element's canopy storage at step start (mm).
   DOUBLEPRECISION :: CPLAI  !! Current element's intercepted-area fraction, `min(CLAI,1)*PLAI`.
   DOUBLEPRECISION :: PREST  !! Unused legacy value set to `1+PALFA` during frame initialization.
   DOUBLEPRECISION :: TIMEUZ !! Current elapsed ET/snow/VSS model time (h).
   DOUBLEPRECISION :: HOTIME !! Current/last hotstart time (h).

! Process and optional-file switches.
   LOGICAL :: BEXET      !! Whether evapotranspiration is active; current frame setup always sets true.
   LOGICAL :: BEXUZ      !! Whether the legacy upper-zone process is active; current frame setup always sets true.
   LOGICAL :: BEXEX      !! Whether legacy exchange is active; current frame setup always sets true.
   LOGICAL :: BEXOC      !! Whether overland/channel flow is active; current frame setup always sets true.
   LOGICAL :: BEXSZ      !! Whether saturated-zone flow is active; current frame setup always sets true.
   LOGICAL :: BEXSM      !! Whether snowmelt is enabled by FR25.
   LOGICAL :: BEXTS1     !! Inactive legacy first time-series extension switch.
   LOGICAL :: BHOTPR     !! Whether periodic hotstart output is enabled.
   LOGICAL :: BHOTRD     !! Whether initial state is read from the hotstart file.
   LOGICAL :: BEXSY      !! Whether sediment transport is enabled by FR25.
   LOGICAL :: BEXCM      !! Whether contaminant transport is enabled by FR25.
   LOGICAL :: ISTA       !! Whether separate maximum/minimum air-temperature streams are available.
   LOGICAL :: isextradis !! Whether the extra-discharge point-selection input is available.
   LOGICAL :: iszq       !! Whether reservoir ZQ-table routing is enabled.
   LOGICAL :: isextrapsl !! Whether the extra phreatic-surface point-selection input is available.

! Static integer arrays.
   INTEGER :: NGRID(NELEE)        !! Legacy element list zeroed by `FRIND` and not subsequently read.
   INTEGER :: INGRID(NXEE,NYEE)   !! Catchment mask: zero inside the active catchment and -1 outside.
   INTEGER :: IOCORS(NSETEE)      !! Contaminant/sediment selector for each legacy result set.
   INTEGER :: NMC(NELEE)          !! Meteorological-site category by element.
   INTEGER :: LCODEX(NXEE,NYEE)   !! X-face overland/channel topology code grid.
   INTEGER :: IODATA(NSETEE)      !! Data-type number for each legacy result set.
   INTEGER :: NRAINC(NELEE)       !! Rainfall-station category by element.
   INTEGER :: LCODEY(NXEE,NYEE)   !! Y-face overland/channel topology code grid.
   INTEGER :: IOELEM(NSETEE)      !! Positive element number or negative element-class number by legacy result set.
   INTEGER :: NOCBCC(NELEE)       !! Overland/channel boundary-condition record number by element.
   INTEGER :: NOCBCD(NOCTAB,4)    !! OC boundary records: element, face, boundary type, and time-series category.
   INTEGER :: IORES(NSETEE)       !! Open unformatted output unit by legacy result set.
   INTEGER :: ICLIST(NELEE,NCLASS) !! Element numbers belonging to each legacy output class.
   INTEGER :: NEXPO(NLFEE,2)      !! Inactive legacy link-exposure array.
   INTEGER :: ICLNUM(NCLASS)      !! Number of elements in each legacy output class.
   INTEGER, DIMENSION(:), ALLOCATABLE :: ZQTableLink !! Channel-link number for each ZQ table.
   INTEGER, DIMENSION(:), ALLOCATABLE :: ZQTableFace !! Channel-link face number for each ZQ table.

   INTEGER :: NSMC(NELEE) !! Number of meltwater slugs still travelling through each snowpack.

   INTEGER :: FLERRC(0:100) !! Legacy flow error counts read at shutdown; no current producer was found.
   INTEGER :: SYERRC(0:100) !! Legacy sediment error counts read at shutdown; no current producer was found.
   INTEGER :: CMERRC(0:100) !! Legacy contaminant error counts read at shutdown; no current producer was found.

! Static real arrays.
   DOUBLEPRECISION :: DXIN(NXEE)     !! Grid-centre spacing in the x direction; active entries are `1:NX-1` (m).
   DOUBLEPRECISION :: DYIN(NYEE)     !! Grid-centre spacing in the y direction; active entries are `1:NY-1` (m).
   DOUBLEPRECISION :: WIDTF(NLFEE)   !! Inactive legacy link face-width array.
   DOUBLEPRECISION :: ZBED(NELEE)    !! Inactive legacy impermeable-bed elevation array.
   DOUBLEPRECISION :: HFLBED(NLFEE)  !! Inactive legacy link-bed head array.
   DOUBLEPRECISION :: ZFBED(NLFEE)   !! Inactive legacy link-bed elevation array.
   DOUBLEPRECISION :: DZFBED(NLFEE)  !! Inactive legacy link-bed elevation-difference array.
   DOUBLEPRECISION :: LROOT(NVEE)    !! Inactive legacy root-depth array.
   DOUBLEPRECISION :: HFLBNK(NLFEE)  !! Inactive legacy bank-head array.
   DOUBLEPRECISION :: IOSTA(NSETEE)  !! Start time for each legacy result set (h).
   DOUBLEPRECISION :: IOSTEP(NSETEE) !! Output interval for each legacy result set (h).
   DOUBLEPRECISION :: IOEND(NSETEE)  !! End time for each legacy result set (h).
   DOUBLEPRECISION :: RHOSAR(NELEE)  !! Snow relative-density/specific-gravity state by element.
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ZQweirSill !! Weir-sill elevation for each ZQ table (m).

! Time-dependent real arrays.
   DOUBLEPRECISION :: CSTORE(NELEE) !! Canopy interception storage by element (mm).
   DOUBLEPRECISION :: ERZA(NELEE)   !! Root-zone extraction rate by element (m/s).
   DOUBLEPRECISION :: EPOT(NELEE)   !! Potential-evaporation rate by element (m/s).
   DOUBLEPRECISION :: EINTA(NELEE)  !! Canopy-interception evaporation rate by element (m/s).
   DOUBLEPRECISION :: EPOTR(NVEE)   !! Inactive legacy potential-evaporation array by vegetation type.
   DOUBLEPRECISION :: SD(NELEE)     !! Snowpack depth by element (mm of snow).
   DOUBLEPRECISION :: TS(NELEE)     !! Snowpack temperature by element (degrees C).
   DOUBLEPRECISION :: SF(NELEE)     !! Current snowfall depth by element (mm of snow).
   DOUBLEPRECISION :: S(LLEE)       !! Current column's volumetric root/soil extraction sink by VSS cell (s-1).
   DOUBLEPRECISION :: precip_m_per_s(NELEE) !! Precipitation rate mapped directly to each element (m/s).
   DOUBLEPRECISION :: OBSPE(NVEE)   !! Potential-evaporation forcing by meteorological site (mm/s).
   DOUBLEPRECISION :: TA(NVEE)      !! Air temperature by meteorological site (degrees C).
   DOUBLEPRECISION :: U(NVEE)       !! Wind speed by meteorological site (m/s).
   DOUBLEPRECISION :: VPD(NVEE)     !! Vapour-pressure deficit by meteorological site (mb).
   DOUBLEPRECISION :: RN(NVEE)      !! Net radiation by meteorological site (W/m2).
   DOUBLEPRECISION :: VHT(NVEE)     !! Current vegetation height by vegetation type (m).
   DOUBLEPRECISION :: IOTIME(NSETEE) !! Next output time for each legacy result set (h).
   DOUBLEPRECISION :: DQ0ST(NELEE,4) !! Face-flow derivative with respect to the local element state.
   DOUBLEPRECISION :: DQIST(NELEE,4) !! Face-flow derivative with respect to the adjacent element state.
   DOUBLEPRECISION :: DQIST2(NLFEE,3) !! Confluence-flow derivative by branch record and branch position.
   DOUBLEPRECISION :: ESWA(NELEE)    !! Surface-water evaporation rate by element (m/s).
   DOUBLEPRECISION :: BALANC(20)     !! Catchment water-volume terms described in the module table (m3).
   DOUBLEPRECISION :: CMEAN(NELEE,2,NCONEE)  !! Inactive legacy dissolved-contaminant mean accumulator.
   DOUBLEPRECISION :: SMEAN(NELEE,2,NCONEE)  !! Inactive legacy dead-space-contaminant mean accumulator.
   DOUBLEPRECISION :: ADMEAN(NELEE,2,NCONEE) !! Inactive legacy adsorbed-contaminant mean accumulator.

   CHARACTER(len=200) :: RESFIL !! Path used as the stem for legacy unformatted result files.

!PRIVATE :: NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, NOCTAB
END MODULE AL_D
