!> summary: Shared flow-component state and file-unit constants.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> `AL_D` stores common state used primarily by the SHETRAN flow components.
!> It includes file-unit constants, model-size and timing controls, process
!> activation flags, input/output scheduling arrays, hotstart state, snow state,
!> overland/channel flow storage, meteorological forcing arrays, and reservoir
!> ZQ-table metadata.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-03 | GP | 3.0 | Original version written. |
!> | 1991-06 | GP | 3.1 | Added new variables including `NEXPO`. |
!> | 1992-02 | GP | 3.3 | Added soil-layer arrays. |
!> | 1992-06 | GP | 3.4 | Moved selected variables to `AL_C`, added flow/snow/storage arrays, and removed obsolete constants. |
!> | 1994-09-28 | RAH | 3.4.1 | Applied standard header, declared variables, and removed `INTEGER*2`. |
!> | 1996-01-03 | GP | 4.0 | Moved selected VSS variables to `AL_C`, removed redundant SZ/UZ/EX variables, and added mass-balance arrays. |
!> | 1997-02 | RAH | 4.1 | Removed redundant derived dimensions and obsolete state variables. |
!> | 1998-01-19 | RAH | 4.2 | Removed obsolete OC and storage variables; defined `NCLASS`. |
!> | 2004-07 | JE | - | Converted to Fortran 95. |
!> | 2020-05-20 | SB | - | Added ZQ table file unit and reservoir table metadata variables. |
!> @endhistory
MODULE AL_D
   USE SGLOBAL, ONLY : NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, NOCTAB
   IMPLICIT NONE

! ----- Constants

   INTEGER, PARAMETER :: NCLASS = 14 !! Number of output/result classes used by result setup.

! ----- File unit numbers

! Values were historically assigned in FRINIT.
   INTEGER, PARAMETER :: FRD = 10      !! Frame data-file unit.
   INTEGER, PARAMETER :: OCD = 12      !! Overland/channel data-file unit.
   INTEGER, PARAMETER :: ETD = 13      !! Evapotranspiration data-file unit.
   INTEGER, PARAMETER :: PPD = 14      !! Precipitation data-file unit.
   INTEGER, PARAMETER :: SMD = 15      !! Snowmelt data-file unit.
   INTEGER, PARAMETER :: BKD = 16      !! Bank data-file unit.
   INTEGER, PARAMETER :: MED = 19      !! Meteorological data-file unit.
   INTEGER, PARAMETER :: PRD = 20      !! Precipitation time-series file unit.
   INTEGER, PARAMETER :: EPD = 21      !! Potential-evaporation time-series file unit.
   INTEGER, PARAMETER :: TIM = 22      !! Timing/control input file unit.
   INTEGER, PARAMETER :: RES = 27      !! Restart/result state file unit.
   INTEGER, PARAMETER :: HOT = 28      !! Hotstart file unit.
   INTEGER, PARAMETER :: VED = 30      !! Vegetation data-file unit.
   INTEGER, PARAMETER :: OFB = 37      !! Overland/channel flow-boundary file unit.
   INTEGER, PARAMETER :: OHB = 38      !! Overland/channel head-boundary file unit.
   INTEGER, PARAMETER :: DIS = 41      !! Discharge output file unit.
   INTEGER, PARAMETER :: VSE = 42      !! VSS output file unit.
   INTEGER, PARAMETER :: MAS = 43      !! Mass-balance output file unit.
   INTEGER, PARAMETER :: DIS2 = 44     !! Secondary discharge output file unit.
   INTEGER, PARAMETER :: TAH = 45      !! Additional high-temperature output/input unit.
   INTEGER, PARAMETER :: TAL = 46      !! Additional low-temperature output/input unit.
   INTEGER, PARAMETER :: disextra = 47 !! Extra discharge output file unit.
   INTEGER, PARAMETER :: zqd = 51      !! ZQ reservoir-table data-file unit.
   INTEGER, PARAMETER :: pslextra = 52 !! Extra phreatic-surface output file unit.


! ----- Static integer variables

   INTEGER :: MSM        !! Snowmelt option/control flag.
   INTEGER :: NM         !! Number of meteorological stations or records in use.
   INTEGER :: NRAIN      !! Number of rainfall stations or records in use.
   INTEGER :: NSET       !! Number of requested output/result sets.
   INTEGER :: MBLINK     !! Link selected for detailed mass-balance reporting.
   INTEGER :: MBFACE     !! Face selected for detailed mass-balance reporting.
   INTEGER :: MBFLAG     !! Mass-balance reporting control flag.
   INTEGER :: NXP1       !! `NX + 1` grid helper dimension.
   INTEGER :: NYP1       !! `NY + 1` grid helper dimension.
   INTEGER :: NXM1       !! `NX - 1` grid helper dimension.
   INTEGER :: NYM1       !! `NY - 1` grid helper dimension.
   INTEGER :: NXEP1      !! `NXEE + 1` workspace helper dimension.
   INTEGER :: NYEP1      !! `NYEE + 1` workspace helper dimension.
   INTEGER :: NoZQTables !! Number of ZQ reservoir tables loaded from the ZQ file.
   INTEGER :: ZQTableRef !! Active ZQ table reference for the current link/face.
   INTEGER, PARAMETER :: NXE = NXEE !! X dimension retained for legacy interfaces.
   INTEGER, PARAMETER :: NYE = NYEE !! Y dimension retained for legacy interfaces.

! ----- Time-dependent integer variables

   INTEGER :: NSTEP !! Current simulation timestep number.
   INTEGER :: NRPD  !! Current rainfall/precipitation data record index.
   INTEGER :: NSMT  !! Snowmelt/ET coupling state for the current element.
   INTEGER :: MBYEAR !! Current mass-balance output year.
   INTEGER :: MBMON  !! Current mass-balance output month.
   INTEGER :: MBDAY  !! Current mass-balance output day.

! ----- Static floating-point variables

   DOUBLEPRECISION :: PSTART  !! Simulation start time for precipitation/meteorological processing.
   DOUBLEPRECISION :: DTMET   !! Meteorological input interval.
   DOUBLEPRECISION :: QMAX    !! Maximum allowed overland/channel face flow before warning/error handling.
   DOUBLEPRECISION :: BHOTTI  !! Hotstart input time.
   DOUBLEPRECISION :: BHOTST  !! Hotstart output/start time.
   DOUBLEPRECISION :: PMAX    !! Maximum precipitation timestep-change control.
   DOUBLEPRECISION :: PALFA   !! Timestep growth factor used by `TMSTEP`.
   DOUBLEPRECISION :: TMAX    !! Basic maximum flow timestep.
   DOUBLEPRECISION :: CAREA   !! Catchment area.
   DOUBLEPRECISION :: BWIDTH  !! Bank width parameter.
   DOUBLEPRECISION :: TTH     !! Simulation end time in hours.
   DOUBLEPRECISION :: DTMET2  !! Secondary meteorological input interval.
   DOUBLEPRECISION :: DTMET3  !! Potential-evaporation input interval.
   DOUBLEPRECISION :: TOUTPUT !! Next scheduled output time.

! ----- Time-dependent floating-point variables

   DOUBLEPRECISION :: UZVAL  !! Current unsaturated-zone time value used by flow scheduling.
   DOUBLEPRECISION :: OCNOW  !! Current overland/channel simulation time.
   DOUBLEPRECISION :: OCNEXT !! Current overland/channel timestep.
   DOUBLEPRECISION :: HRUZ   !! Upper-zone water depth or head work value.
   DOUBLEPRECISION :: PNET   !! Net precipitation through canopy for the active element.
   DOUBLEPRECISION :: PE     !! Potential evapotranspiration for the active element.
   DOUBLEPRECISION :: EINT   !! Canopy interception evaporation for the active element.
   DOUBLEPRECISION :: ERZ    !! Root-zone extraction for the active element.
   DOUBLEPRECISION :: DRAIN  !! Canopy drainage for the active element.
   DOUBLEPRECISION :: ESOIL  !! Soil evaporation for the active element.
   DOUBLEPRECISION :: AE     !! Actual evapotranspiration for the active element.
   DOUBLEPRECISION :: CSTOLD !! Previous canopy store for the active element.
   DOUBLEPRECISION :: CPLAI  !! Current plant-area/leaf-area interception factor.
   DOUBLEPRECISION :: PREST  !! Precipitation time-series state value.
   DOUBLEPRECISION :: TIMEUZ !! Current upper-zone/ET simulation time.
   DOUBLEPRECISION :: HOTIME !! Hotstart time read from or written to `HOT`.

! ----- Static logical variables

   LOGICAL :: BEXET      !! True when the ET component is active.
   LOGICAL :: BEXUZ      !! True when the upper-zone component is active.
   LOGICAL :: BEXEX      !! True when the exfiltration/exchange component is active.
   LOGICAL :: BEXOC      !! True when overland/channel flow is active.
   LOGICAL :: BEXSZ      !! True when saturated-zone flow is active.
   LOGICAL :: BEXSM      !! True when snowmelt is active.
   LOGICAL :: BEXTS1     !! True when the first time-series extension is active.
   LOGICAL :: BHOTPR     !! True when hotstart output is enabled.
   LOGICAL :: BHOTRD     !! True when hotstart input is enabled.
   LOGICAL :: BEXSY      !! True when sediment transport is active.
   LOGICAL :: BEXCM      !! True when contaminant transport is active.
   LOGICAL :: ISTA       !! Station/input-state flag used by meteorological processing.
   LOGICAL :: isextradis !! True when extra discharge output is enabled.
   LOGICAL :: iszq       !! True when ZQ reservoir-table routing is enabled.
   LOGICAL :: isextrapsl !! True when extra phreatic-surface output is enabled.

! ----- Static integer arrays

   INTEGER :: NGRID(NELEE)        !! Grid-element indicator/list by element.
   INTEGER :: INGRID(NXEE,NYEE)   !! Grid map from x-y coordinates to element number.
   INTEGER :: IOCORS(NSETEE)      !! Output coordinate/system selector by output set.
   INTEGER :: NMC(NELEE)          !! Meteorological category by element.
   INTEGER :: LCODEX(NXEE,NYEE)   !! X-direction link/code grid.
   INTEGER :: IODATA(NSETEE)      !! Output data type by output set.
   INTEGER :: NRAINC(NELEE)       !! Rainfall category by element.
   INTEGER :: LCODEY(NXEE,NYEE)   !! Y-direction link/code grid.
   INTEGER :: IOELEM(NSETEE)      !! Output element/link id by output set.
   INTEGER :: NOCBCC(NELEE)       !! OC boundary category by element.
   INTEGER :: NOCBCD(NOCTAB,4)    !! OC boundary category definition table.
   INTEGER :: IORES(NSETEE)       !! Output result unit/index by output set.
   INTEGER :: ICLIST(NELEE,NCLASS) !! Element lists by output/result class.
   INTEGER :: NEXPO(NLFEE,2)      !! Link exposure/output helper by link and side.
   INTEGER :: ICLNUM(NCLASS)      !! Number of elements in each output/result class.
   INTEGER, ALLOCATABLE :: ZQTableLink(:) !! Link id for each ZQ table metadata record.
   INTEGER, ALLOCATABLE :: ZQTableFace(:) !! Face id for each ZQ table metadata record.

! ----- Time-dependent integer arrays

   INTEGER :: NSMC(NELEE) !! Snowmelt calculation state by element.

!970212 TEMPORARY!
   INTEGER :: FLERRC(0:100) !! Flow-component error counts by error code.
   INTEGER :: SYERRC(0:100) !! Sediment-component error counts by error code.
   INTEGER :: CMERRC(0:100) !! Contaminant-component error counts by error code.

! ----- Static floating-point arrays

   DOUBLEPRECISION :: DXIN(NXEE)     !! Input x-coordinate/grid-spacing values.
   DOUBLEPRECISION :: DYIN(NYEE)     !! Input y-coordinate/grid-spacing values.
   DOUBLEPRECISION :: WIDTF(NLFEE)   !! Link face/flow width.
   DOUBLEPRECISION :: ZBED(NELEE)    !! Bed elevation by element.
   DOUBLEPRECISION :: HFLBED(NLFEE)  !! Bed-level flow/head helper by link.
   DOUBLEPRECISION :: ZFBED(NLFEE)   !! Flow bed elevation by link.
   DOUBLEPRECISION :: DZFBED(NLFEE)  !! Bed elevation increment/change by link.
   DOUBLEPRECISION :: LROOT(NVEE)    !! Root-zone depth by vegetation type.
   DOUBLEPRECISION :: HFLBNK(NLFEE)  !! Bank-level flow/head helper by link.
   DOUBLEPRECISION :: IOSTA(NSETEE)  !! Output start time by output set.
   DOUBLEPRECISION :: IOSTEP(NSETEE) !! Output interval by output set.
   DOUBLEPRECISION :: IOEND(NSETEE)  !! Output end time by output set.
   DOUBLEPRECISION :: RHOSAR(NELEE)  !! Snowpack density by element.
   DOUBLEPRECISION, ALLOCATABLE :: ZQweirSill(:) !! Weir sill level for each ZQ table metadata record.

! ----- Time-dependent floating-point arrays

   DOUBLEPRECISION :: CSTORE(NELEE)      !! Canopy store by element.
   DOUBLEPRECISION :: ERZA(NELEE)        !! Root-zone extraction by element.
   DOUBLEPRECISION :: EPOT(NELEE)        !! Potential evapotranspiration by element.
   DOUBLEPRECISION :: EINTA(NELEE)       !! Interception evaporation by element.
   DOUBLEPRECISION :: EPOTR(NVEE)        !! Potential evapotranspiration by vegetation type.
   DOUBLEPRECISION :: SD(NELEE)          !! Snow depth by element.
   DOUBLEPRECISION :: TS(NELEE)          !! Snow temperature by element.
   DOUBLEPRECISION :: SF(NELEE)          !! Snowfall rate/depth state by element.
   DOUBLEPRECISION :: S(LLEE)            !! Vertical root-zone extraction work array.
   DOUBLEPRECISION :: precip_m_per_s(NELEE) !! Precipitation rate by element in metres per second.
   DOUBLEPRECISION :: OBSPE(NVEE)        !! Observed potential evaporation/ET by vegetation or station.
   DOUBLEPRECISION :: TA(NVEE)           !! Air temperature by vegetation or station.
   DOUBLEPRECISION :: U(NVEE)            !! Wind speed by vegetation or station.
   DOUBLEPRECISION :: VPD(NVEE)          !! Vapour pressure deficit by vegetation or station.
   DOUBLEPRECISION :: RN(NVEE)           !! Net radiation by vegetation or station.
   DOUBLEPRECISION :: VHT(NVEE)          !! Vegetation height by vegetation type.
   DOUBLEPRECISION :: IOTIME(NSETEE)     !! Current/next output time by output set.
   DOUBLEPRECISION :: DQ0ST(NELEE,4)     !! OC face-flow derivative with respect to local storage/head.
   DOUBLEPRECISION :: DQIST(NELEE,4)     !! OC face-flow derivative with respect to adjacent storage/head.
   DOUBLEPRECISION :: DQIST2(NLFEE,3)    !! OC confluence derivative helper by link and branch.
   DOUBLEPRECISION :: ESWA(NELEE)        !! Evaporation from surface water by element.
   DOUBLEPRECISION :: BALANC(20)         !! Water-balance accumulator terms.
   DOUBLEPRECISION :: CMEAN(NELEE,2,NCONEE)  !! Contaminant mean output/state accumulator.
   DOUBLEPRECISION :: SMEAN(NELEE,2,NCONEE)  !! Sorbed/solid contaminant mean output/state accumulator.
   DOUBLEPRECISION :: ADMEAN(NELEE,2,NCONEE) !! Adsorbed contaminant mean output/state accumulator.

! ----- Static character variables

   CHARACTER(LEN=200) :: RESFIL !! Restart/result file name.

!PRIVATE :: NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, NOCTAB
END MODULE AL_D
