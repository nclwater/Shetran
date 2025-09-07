!> summary: Defines common variables for the primary flow components.
!> author: G. Parkin, R. A. Heath, J. Ewen, S Birkinshaw (Newcastle University)
!> date: 2020-05-20
!>
!> This module contains a large collection of global variables and arrays
!> that are shared across the main flow components of the SHETRAN model,
!> including overland flow, river channel flow, and evapotranspiration.
!> It includes file unit numbers, static grid and link properties,
!> and time-dependent simulation arrays specific to flow calculations.
!>
!> @note This module is a legacy component derived from an old `INCLUDE` file.
!> The refactoring plan in `docs/reports/refactor_parameters/plan_parameter_refactor.md`
!> outlines renaming this module to `flow_component_vars.f90` and potentially
!> splitting it further.
!>
! @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | 1991-03 | GP | v3.0: Written. |
!> | 1991-06 | GP | v3.1: Added NEXPO. |
!> | 1992-02 | GP | v3.3: Added soil layer arrays. |
!> | 1992-06 | GP | v3.4: Moved variables to AL.C for hotstart. Added new arrays. |
!> | 1994-09-28 | RAH | v3.4.1: Modernized declarations, removed INTEGER*2, standard header. |
!> | 1996-01-03 | GP | v4.0: Moved many variables to AL.C and SPEC.ET. Removed many others. Added new mass balance variables. |
!> | 1997-02-12 | RAH | v4.1: Removed redundant "derived dimensions". |
!> | 1997-02-13 | RAH | v4.1: Removed numerous redundant SZ, UZ, and EX arrays. |
!> | 1998-01-19 | RAH | v4.2: Removed redundant WSOC, WOCEV, WOCLI, WOCR arrays. |
!> | 1998-03-06 | RAH | v4.2: Removed many redundant time-dependent and storage arrays. Made RHOSAR static. |
!> | 1998-03-07 | RAH | v4.2: Reduced size of NOCBCD from 5 to 4. |
!> | 1998-07-13 | RAH | v4.2: Removed I/NCATUZ. |
!> | 1998-07-16 | RAH | v4.2: Defined NCLASS. |
!> | 1998-11-03 | RAH | v4.2: Removed NSOIL. |
!> | 2004-07 | JE | Converted to Fortran 95. |
!> | 2020-05-20 | SB | Added ZQ table variables. |
!> | 2024-09-05 | Gemini | Converted documentation to FORD format. |
MODULE AL_D
   USE SGLOBAL, ONLY : NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, &
      NOCTAB
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: NCLASS
   PUBLIC :: FRD, OCD, ETD, PPD, SMD, BKD, MED, PRD, EPD, TIM, RES, HOT, &
      VED, OFB, OHB, DIS, VSE, MAS, DIS2, TAH, TAL, disextra, zqd, pslextra
   PUBLIC :: MSM, NM, NRAIN, NSET, MBLINK, MBFACE, MBFLAG, NXP1, NYP1, NXM1, &
      NYM1, NXEP1, NYEP1, NoZQTables, ZQTableRef, NXE, NYE
   PUBLIC :: NSTEP, NRPD, NSMT, MBYEAR, MBMON, MBDAY
   PUBLIC :: PSTART, DTMET, QMAX, BHOTTI, BHOTST, PMAX, PALFA, TMAX, CAREA, &
      BWIDTH, TTH, DTMET2, DTMET3, TOUTPUT
   PUBLIC :: UZVAL, OCNOW, OCNEXT, HRUZ, PNET, PE, EINT, ERZ, DRAIN, ESOIL, &
      AE, CSTOLD, CPLAI, PREST, TIMEUZ, HOTIME
   PUBLIC :: BEXET, BEXUZ, BEXEX, BEXOC, BEXSZ, BEXSM, BEXTS1, BHOTPR, &
      BHOTRD, BEXSY, BEXCM, ISTA, isextradis, iszq, isextrapsl
   PUBLIC :: NGRID, INGRID, IOCORS, NMC, LCODEX, IODATA, NRAINC, LCODEY, &
      IOELEM, NOCBCC, NOCBCD, IORES, ICLIST, NEXPO, ICLNUM, ZQTableLink, &
      ZQTableFace
   PUBLIC :: NSMC, FLERRC, SYERRC, CMERRC
   PUBLIC :: DXIN, DYIN, WIDTF, ZBED, HFLBED, ZFBED, DZFBED, LROOT, HFLBNK, &
      IOSTA, IOSTEP, IOEND, RHOSAR, ZQweirSill
   PUBLIC :: CSTORE, ERZA, EPOT, EINTA, EPOTR, SD, TS, SF, S, precip_m_per_s, &
      OBSPE, TA, U, VPD, RN, VHT, IOTIME, DQ0ST, DQIST, DQIST2, ESWA, &
      BALANC, CMEAN, SMEAN, ADMEAN
   PUBLIC :: RESFIL

   !-----------------------------------------------------------------------
   ! Constants
   !-----------------------------------------------------------------------
   INTEGER, PARAMETER :: NCLASS = 14 !! Number of classes for output results.

   !-----------------------------------------------------------------------
   ! File Unit Numbers
   !-----------------------------------------------------------------------
   INTEGER, PARAMETER :: FRD = 10 !! Framework data file.
   INTEGER, PARAMETER :: OCD = 12 !! Overland/channel data file.
   INTEGER, PARAMETER :: ETD = 13 !! Evapotranspiration data file.
   INTEGER, PARAMETER :: PPD = 14 !! Precipitation data file.
   INTEGER, PARAMETER :: SMD = 15 !! Snowmelt data file.
   INTEGER, PARAMETER :: BKD = 16 !! Bank data file.
   INTEGER, PARAMETER :: MED = 19 !! Meteorological data file.
   INTEGER, PARAMETER :: PRD = 20 !! Precipitation results file.
   INTEGER, PARAMETER :: EPD = 21 !! Evaporation results file.
   INTEGER, PARAMETER :: TIM = 22 !! Time series data file.
   INTEGER, PARAMETER :: RES = 27 !! Main results file.
   INTEGER, PARAMETER :: HOT = 28 !! Hotstart file.
   INTEGER, PARAMETER :: VED = 30 !! Vegetation data file.
   INTEGER, PARAMETER :: OFB = 37 !! Overland flow binary file.
   INTEGER, PARAMETER :: OHB = 38 !! Overland head binary file.
   INTEGER, PARAMETER :: DIS = 41 !! Discharge results file.
   INTEGER, PARAMETER :: VSE = 42 !! VS component error file.
   INTEGER, PARAMETER :: MAS = 43 !! Mass balance file.
   INTEGER, PARAMETER :: DIS2 = 44 !! Secondary discharge file.
   INTEGER, PARAMETER :: TAH = 45 !! Time analysis high-frequency file.
   INTEGER, PARAMETER :: TAL = 46 !! Time analysis low-frequency file.
   INTEGER, PARAMETER :: disextra = 47 !! Extra discharge file.
   INTEGER, PARAMETER :: zqd = 51 !! ZQ table data file.
   INTEGER, PARAMETER :: pslextra = 52 !! Extra phreatic surface level file.

   !-----------------------------------------------------------------------
   ! Static Integer Variables
   !-----------------------------------------------------------------------
   INTEGER :: MSM !! Snowmelt calculation method (1=degree day, 2=energy budget).
   INTEGER :: NM !! Number of meteorological stations.
   INTEGER :: NRAIN !! Number of rainfall stations.
   INTEGER :: NSET !! Number of output parameter sets.
   INTEGER :: MBLINK !! Mass balance link index for monitoring.
   INTEGER :: MBFACE !! Mass balance face index for monitoring.
   INTEGER :: MBFLAG !! Mass balance output frequency flag.
   INTEGER :: NXP1 !! Grid dimension NX + 1 (helper variable).
   INTEGER :: NYP1 !! Grid dimension NY + 1 (helper variable).
   INTEGER :: NXM1 !! Grid dimension NX - 1 (helper variable).
   INTEGER :: NYM1 !! Grid dimension NY - 1 (helper variable).
   INTEGER :: NXEP1 !! Grid dimension NXE + 1 (helper variable).
   INTEGER :: NYEP1 !! Grid dimension NYE + 1 (helper variable).
   INTEGER :: NoZQTables !! Number of ZQ (stage-discharge) tables defined.
   INTEGER :: ZQTableRef !! Reference index for current ZQ table in use.
   INTEGER, PARAMETER :: NXE = NXEE !! Alias for max grid points in x-direction.
   INTEGER, PARAMETER :: NYE = NYEE !! Alias for max grid points in y-direction.

   !-----------------------------------------------------------------------
   ! Time-Dependent Integer Variables
   !-----------------------------------------------------------------------
   INTEGER :: NSTEP !! Current simulation timestep number.
   INTEGER :: NRPD !! Rainfall data reading counter.
   INTEGER :: NSMT !! Snowmelt timestep counter.
   INTEGER :: MBYEAR !! Current year for mass balance calculations.
   INTEGER :: MBMON !! Current month for mass balance calculations.
   INTEGER :: MBDAY !! Current day for mass balance calculations.

   !-----------------------------------------------------------------------
   ! Static Floating-Point Variables
   !-----------------------------------------------------------------------
   DOUBLEPRECISION :: PSTART !! Simulation start time [hours].
   DOUBLEPRECISION :: DTMET !! Meteorological data timestep [hours].
   DOUBLEPRECISION :: QMAX !! Maximum flow rate threshold for warnings [m³/s].
   DOUBLEPRECISION :: BHOTTI !! Hotstart begin time [hours].
   DOUBLEPRECISION :: BHOTST !! Hotstart output timestep [hours].
   DOUBLEPRECISION :: PMAX !! Maximum precipitation rate [mm/s].
   DOUBLEPRECISION :: PALFA !! Convergence acceleration parameter [-].
   DOUBLEPRECISION :: TMAX !! Maximum simulation timestep [hours].
   DOUBLEPRECISION :: CAREA !! Catchment area [m²].
   DOUBLEPRECISION :: BWIDTH !! Channel bank width [m].
   DOUBLEPRECISION :: TTH !! Total simulation time [hours].
   DOUBLEPRECISION :: DTMET2 !! Secondary meteorological timestep [hours].
   DOUBLEPRECISION :: DTMET3 !! Tertiary meteorological timestep [hours].
   DOUBLEPRECISION :: TOUTPUT !! Output time interval [hours].

   !-----------------------------------------------------------------------
   ! Time-Dependent Floating-Point Variables
   !-----------------------------------------------------------------------
   DOUBLEPRECISION :: UZVAL !! Unsaturated zone validation parameter.
   DOUBLEPRECISION :: OCNOW !! Current overland/channel flow time [hours].
   DOUBLEPRECISION :: OCNEXT !! Next overland/channel flow time [hours].
   DOUBLEPRECISION :: HRUZ !! Unsaturated zone head value [m].
   DOUBLEPRECISION :: PNET !! Net precipitation [mm].
   DOUBLEPRECISION :: PE !! Potential evapotranspiration [mm/s].
   DOUBLEPRECISION :: EINT !! Interception evaporation [mm/s].
   DOUBLEPRECISION :: ERZ !! Evapotranspiration from root zone [mm/s].
   DOUBLEPRECISION :: DRAIN !! Drainage rate [mm/s].
   DOUBLEPRECISION :: ESOIL !! Soil evaporation [mm/s].
   DOUBLEPRECISION :: AE !! Actual evapotranspiration [mm/s].
   DOUBLEPRECISION :: CSTOLD !! Previous canopy storage [mm].
   DOUBLEPRECISION :: CPLAI !! Current plant leaf area index [-].
   DOUBLEPRECISION :: PREST !! Residual precipitation [mm].
   DOUBLEPRECISION :: TIMEUZ !! Unsaturated zone time [hours].
   DOUBLEPRECISION :: HOTIME !! Hotstart time [hours].

   !-----------------------------------------------------------------------
   ! Static Logical Variables
   !-----------------------------------------------------------------------
   LOGICAL :: BEXET !! Flag indicating evapotranspiration component is active.
   LOGICAL :: BEXUZ !! Flag indicating unsaturated zone component is active.
   LOGICAL :: BEXEX !! Flag indicating external routines are active.
   LOGICAL :: BEXOC !! Flag indicating overland/channel flow component is active.
   LOGICAL :: BEXSZ !! Flag indicating saturated zone component is active.
   LOGICAL :: BEXSM !! Flag indicating snowmelt component is active.
   LOGICAL :: BEXTS1 !! Flag indicating time series 1 output is active.
   LOGICAL :: BHOTPR !! Flag indicating hotstart print output is active.
   LOGICAL :: BHOTRD !! Flag indicating hotstart read is active.
   LOGICAL :: BEXSY !! Flag indicating sediment yield component is active.
   LOGICAL :: BEXCM !! Flag indicating contaminant component is active.
   LOGICAL :: ISTA !! Flag indicating statistical analysis is active.
   LOGICAL :: isextradis !! Flag indicating extra discharge output is active.
   LOGICAL :: iszq !! Flag indicating ZQ table functionality is active.
   LOGICAL :: isextrapsl !! Flag indicating extra phreatic surface level output is active.

   !-----------------------------------------------------------------------
   ! Static Integer Arrays
   !-----------------------------------------------------------------------
   INTEGER, DIMENSION(NELEE) :: NGRID !! Grid element mapping.
   INTEGER, DIMENSION(NXEE,NYEE) :: INGRID !! Inverse grid mapping (x,y to element index).
   INTEGER, DIMENSION(NSETEE) :: IOCORS !! Output set coordinates.
   INTEGER, DIMENSION(NELEE) :: NMC !! Meteorological station category for each element.
   INTEGER, DIMENSION(NXEE,NYEE) :: LCODEX, LCODEY !! Link codes.
   INTEGER, DIMENSION(NSETEE) :: IODATA, IOELEM, IORES !! Output set data, element, and results flags.
   INTEGER, DIMENSION(NELEE) :: NRAINC !! Rainfall station category for each element.
   INTEGER, DIMENSION(NELEE) :: NOCBCC !! Overland flow boundary condition category.
   INTEGER, DIMENSION(NOCTAB,4) :: NOCBCD !! Overland flow boundary condition data.
   INTEGER, DIMENSION(NELEE,NCLASS) :: ICLIST !! List of elements for each output class.
   INTEGER, DIMENSION(NLFEE,2) :: NEXPO !! Link exponent data.
   INTEGER, DIMENSION(NCLASS) :: ICLNUM !! Number of elements in each output class.
   INTEGER, DIMENSION(:), ALLOCATABLE :: ZQTableLink, ZQTableFace !! Metadata for a single ZQ table.

   !-----------------------------------------------------------------------
   ! Time-Dependent Integer Arrays
   !-----------------------------------------------------------------------
   INTEGER, DIMENSION(NELEE) :: NSMC !! Snowmelt category for each element.
   !970212 TEMPORARY!
   INTEGER :: FLERRC(0:100) !! Temporary error counter for flow component.
   INTEGER :: SYERRC(0:100) !! Temporary error counter for sediment component.
   INTEGER :: CMERRC(0:100) !! Temporary error counter for contaminant component.

   !-----------------------------------------------------------------------
   ! Static Floating-Point Arrays
   !-----------------------------------------------------------------------
   DOUBLEPRECISION, DIMENSION(NXEE) :: DXIN !! Grid spacing in x-direction [m].
   DOUBLEPRECISION, DIMENSION(NYEE) :: DYIN !! Grid spacing in y-direction [m].
   DOUBLEPRECISION, DIMENSION(NLFEE) :: WIDTF !! Width of river link [m].
   DOUBLEPRECISION, DIMENSION(NELEE) :: ZBED !! Bed elevation [m].
   DOUBLEPRECISION, DIMENSION(NLFEE) :: HFLBED !! Height of flow in channel bed [m].
   DOUBLEPRECISION, DIMENSION(NLFEE) :: ZFBED !! Elevation of channel bed floor [m].
   DOUBLEPRECISION, DIMENSION(NLFEE) :: DZFBED !! Thickness of channel bed sediment layer [m].
   DOUBLEPRECISION, DIMENSION(NVEE) :: LROOT !! Root length [m].
   DOUBLEPRECISION, DIMENSION(NLFEE) :: HFLBNK !! Height of flow in channel bank [m].
   DOUBLEPRECISION, DIMENSION(NSETEE) :: IOSTA !! Output set start time [hours].
   DOUBLEPRECISION, DIMENSION(NSETEE) :: IOSTEP !! Output set time step [hours].
   DOUBLEPRECISION, DIMENSION(NSETEE) :: IOEND !! Output set end time [hours].
   DOUBLEPRECISION, DIMENSION(NELEE) :: RHOSAR !! Saturated zone density [kg/m^3].
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ZQweirSill !! Weir sill elevations for ZQ tables [m].

   !-----------------------------------------------------------------------
   ! Time-Dependent Floating-Point Arrays
   !-----------------------------------------------------------------------
   DOUBLEPRECISION, DIMENSION(NELEE) :: CSTORE !! Canopy storage [mm].
   DOUBLEPRECISION, DIMENSION(NELEE) :: ERZA !! Actual evapotranspiration from root zone [mm].
   DOUBLEPRECISION, DIMENSION(NELEE) :: EPOT !! Potential evapotranspiration [mm/s].
   DOUBLEPRECISION, DIMENSION(NELEE) :: EINTA !! Interception evaporation [mm].
   DOUBLEPRECISION, DIMENSION(NVEE) :: EPOTR !! Potential evapotranspiration rate [mm/s].
   DOUBLEPRECISION, DIMENSION(NELEE) :: SD !! Snow depth [mm water equivalent].
   DOUBLEPRECISION, DIMENSION(NELEE) :: TS !! Snowpack temperature [C].
   DOUBLEPRECISION, DIMENSION(NELEE) :: SF !! Snowfall [mm].
   DOUBLEPRECISION, DIMENSION(LLEE) :: S !! Source/sink term for UZ flow [1/s].
   DOUBLEPRECISION, DIMENSION(nelee) :: precip_m_per_s !! Precipitation rate [m/s].
   DOUBLEPRECISION, DIMENSION(NVEE) :: OBSPE !! Observed precipitation [mm/hour].
   DOUBLEPRECISION, DIMENSION(NVEE) :: TA !! Air temperature [°C].
   DOUBLEPRECISION, DIMENSION(NVEE) :: U !! Wind speed [m/s].
   DOUBLEPRECISION, DIMENSION(NVEE) :: VPD !! Vapour pressure deficit [kPa].
   DOUBLEPRECISION, DIMENSION(NVEE) :: RN !! Net radiation [W/m^2].
   DOUBLEPRECISION, DIMENSION(NVEE) :: VHT !! Vegetation height [m].
   DOUBLEPRECISION, DIMENSION(NSETEE) :: IOTIME !! Output times for sets.
   DOUBLEPRECISION, DIMENSION(NELEE,4) :: DQ0ST !! Overland flow storage change [m^3/s].
   DOUBLEPRECISION, DIMENSION(NELEE,4) :: DQIST !! Interflow storage change [m^3/s].
   DOUBLEPRECISION, DIMENSION(NLFEE,3) :: DQIST2 !! Link interflow storage change [m^3/s].
   DOUBLEPRECISION, DIMENSION(NELEE) :: ESWA !! Actual soil evaporation [mm].
   DOUBLEPRECISION, DIMENSION(20) :: BALANC !! Mass balance components.
   DOUBLEPRECISION, DIMENSION(NELEE,2,NCONEE) :: CMEAN !! Mean contaminant concentration in solution [kg/m^3].
   DOUBLEPRECISION, DIMENSION(NELEE,2,NCONEE) :: SMEAN !! Mean contaminant concentration in sediment [kg/kg].
   DOUBLEPRECISION, DIMENSION(NELEE,2,NCONEE) :: ADMEAN !! Mean adsorbed contaminant concentration [kg/kg].

   !-----------------------------------------------------------------------
   ! Static Character Variables
   !-----------------------------------------------------------------------
   CHARACTER(200) :: RESFIL !! Results filename.

END MODULE AL_D
