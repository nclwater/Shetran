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
!> @history
!> | Date       | Author | Version | Description                                                              |
!> |:-----------|:-------|:--------|:-------------------------------------------------------------------------|
!> | 1991-03-01 | GP     | 3.0     | Original `INCLUDE` file written.                                         |
!> | 1991-06-01 | GP     | 3.1     | Added NEXPO.                                                             |
!> | 1992-02-01 | GP     | 3.3     | Added soil layer arrays.                                                 |
!> | 1992-06-01 | GP     | 3.4     | Moved variables to AL.C for hotstart. Added new arrays.                  |
!> | 1994-09-28 | RAH    | 3.4.1   | Modernized declarations, removed INTEGER*2, standard header.             |
!> | 1996-01-03 | GP     | 4.0     | Moved many variables to AL.C and SPEC.ET. Removed many others. Added new mass balance variables. |
!> | 1997-02-12 | RAH    | 4.1     | Removed redundant "derived dimensions".                                  |
!> | 1997-02-13 | RAH    | 4.1     | Removed numerous redundant SZ, UZ, and EX arrays.                        |
!> | 1998-01-19 | RAH    | 4.2     | Removed redundant WSOC, WOCEV, WOCLI, WOCR arrays.                       |
!> | 1998-03-06 | RAH    | 4.2     | Removed many redundant time-dependent and storage arrays. Made RHOSAR static. |
!> | 1998-03-07 | RAH    | 4.2     | Reduced size of NOCBCD from 5 to 4.                                      |
!> | 1998-07-13 | RAH    | 4.2     | Removed I/NCATUZ.                                                        |
!> | 1998-07-16 | RAH    | 4.2     | Defined NCLASS.                                                          |
!> | 1998-11-03 | RAH    | 4.2     | Removed NSOIL.                                                           |
!> | 2004-07-01 | JE     | -       | Converted to Fortran 95.                                                 |
!> | 2020-05-20 | SB     | -       | Added ZQ table variables.                                                |
!> | 2025-08-11 | AI     | -       | Added KIND parameters and FORD docs.                                     |
MODULE AL_D
   USE SGLOBAL, ONLY : NELEE, NVEE, NXEE, NYEE, NCONEE, NLFEE, NSETEE, LLEE, &
      NOCTAB
   USE MOD_PARAMETERS, ONLY: R8P, I_P, LENGTH_FILEPATH
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
   INTEGER(KIND=I_P), PARAMETER :: NCLASS = 14 !! Number of classes for output results.

   !-----------------------------------------------------------------------
   ! File Unit Numbers
   !-----------------------------------------------------------------------
   INTEGER(KIND=I_P), PARAMETER :: FRD = 10 !! Framework data file.
   INTEGER(KIND=I_P), PARAMETER :: OCD = 12 !! Overland/channel data file.
   INTEGER(KIND=I_P), PARAMETER :: ETD = 13 !! Evapotranspiration data file.
   INTEGER(KIND=I_P), PARAMETER :: PPD = 14 !! Precipitation data file.
   INTEGER(KIND=I_P), PARAMETER :: SMD = 15 !! Snowmelt data file.
   INTEGER(KIND=I_P), PARAMETER :: BKD = 16 !! Bank data file.
   INTEGER(KIND=I_P), PARAMETER :: MED = 19 !! Meteorological data file.
   INTEGER(KIND=I_P), PARAMETER :: PRD = 20 !! Precipitation results file.
   INTEGER(KIND=I_P), PARAMETER :: EPD = 21 !! Evaporation results file.
   INTEGER(KIND=I_P), PARAMETER :: TIM = 22 !! Time series data file.
   INTEGER(KIND=I_P), PARAMETER :: RES = 27 !! Main results file.
   INTEGER(KIND=I_P), PARAMETER :: HOT = 28 !! Hotstart file.
   INTEGER(KIND=I_P), PARAMETER :: VED = 30 !! Vegetation data file.
   INTEGER(KIND=I_P), PARAMETER :: OFB = 37 !! Overland flow binary file.
   INTEGER(KIND=I_P), PARAMETER :: OHB = 38 !! Overland head binary file.
   INTEGER(KIND=I_P), PARAMETER :: DIS = 41 !! Discharge results file.
   INTEGER(KIND=I_P), PARAMETER :: VSE = 42 !! VS component error file.
   INTEGER(KIND=I_P), PARAMETER :: MAS = 43 !! Mass balance file.
   INTEGER(KIND=I_P), PARAMETER :: DIS2 = 44 !! Secondary discharge file.
   INTEGER(KIND=I_P), PARAMETER :: TAH = 45 !! Time analysis high-frequency file.
   INTEGER(KIND=I_P), PARAMETER :: TAL = 46 !! Time analysis low-frequency file.
   INTEGER(KIND=I_P), PARAMETER :: disextra = 47 !! Extra discharge file.
   INTEGER(KIND=I_P), PARAMETER :: zqd = 51 !! ZQ table data file.
   INTEGER(KIND=I_P), PARAMETER :: pslextra = 52 !! Extra phreatic surface level file.

   !-----------------------------------------------------------------------
   ! Static Integer Variables
   !-----------------------------------------------------------------------
   INTEGER(KIND=I_P) :: MSM !! Snowmelt calculation method (1=degree day, 2=energy budget).
   INTEGER(KIND=I_P) :: NM !! Number of meteorological stations.
   INTEGER(KIND=I_P) :: NRAIN !! Number of rainfall stations.
   INTEGER(KIND=I_P) :: NSET !! Number of output parameter sets.
   INTEGER(KIND=I_P) :: MBLINK !! Mass balance link index for monitoring.
   INTEGER(KIND=I_P) :: MBFACE !! Mass balance face index for monitoring.
   INTEGER(KIND=I_P) :: MBFLAG !! Mass balance output frequency flag.
   INTEGER(KIND=I_P) :: NXP1 !! Grid dimension NX + 1 (helper variable).
   INTEGER(KIND=I_P) :: NYP1 !! Grid dimension NY + 1 (helper variable).
   INTEGER(KIND=I_P) :: NXM1 !! Grid dimension NX - 1 (helper variable).
   INTEGER(KIND=I_P) :: NYM1 !! Grid dimension NY - 1 (helper variable).
   INTEGER(KIND=I_P) :: NXEP1 !! Grid dimension NXE + 1 (helper variable).
   INTEGER(KIND=I_P) :: NYEP1 !! Grid dimension NYE + 1 (helper variable).
   INTEGER(KIND=I_P) :: NoZQTables !! Number of ZQ (stage-discharge) tables defined.
   INTEGER(KIND=I_P) :: ZQTableRef !! Reference index for current ZQ table in use.
   INTEGER(KIND=I_P), PARAMETER :: NXE = NXEE !! Alias for max grid points in x-direction.
   INTEGER(KIND=I_P), PARAMETER :: NYE = NYEE !! Alias for max grid points in y-direction.

   !-----------------------------------------------------------------------
   ! Time-Dependent Integer Variables
   !-----------------------------------------------------------------------
   INTEGER(KIND=I_P) :: NSTEP !! Current simulation timestep number.
   INTEGER(KIND=I_P) :: NRPD !! Rainfall data reading counter.
   INTEGER(KIND=I_P) :: NSMT !! Snowmelt timestep counter.
   INTEGER(KIND=I_P) :: MBYEAR !! Current year for mass balance calculations.
   INTEGER(KIND=I_P) :: MBMON !! Current month for mass balance calculations.
   INTEGER(KIND=I_P) :: MBDAY !! Current day for mass balance calculations.

   !-----------------------------------------------------------------------
   ! Static Floating-Point Variables
   !-----------------------------------------------------------------------
   REAL(KIND=R8P) :: PSTART !! Simulation start time [hours].
   REAL(KIND=R8P) :: DTMET !! Meteorological data timestep [hours].
   REAL(KIND=R8P) :: QMAX !! Maximum flow rate threshold for warnings [m³/s].
   REAL(KIND=R8P) :: BHOTTI !! Hotstart begin time [hours].
   REAL(KIND=R8P) :: BHOTST !! Hotstart output timestep [hours].
   REAL(KIND=R8P) :: PMAX !! Maximum precipitation rate [mm/s].
   REAL(KIND=R8P) :: PALFA !! Convergence acceleration parameter [-].
   REAL(KIND=R8P) :: TMAX !! Maximum simulation timestep [hours].
   REAL(KIND=R8P) :: CAREA !! Catchment area [m²].
   REAL(KIND=R8P) :: BWIDTH !! Channel bank width [m].
   REAL(KIND=R8P) :: TTH !! Total simulation time [hours].
   REAL(KIND=R8P) :: DTMET2 !! Secondary meteorological timestep [hours].
   REAL(KIND=R8P) :: DTMET3 !! Tertiary meteorological timestep [hours].
   REAL(KIND=R8P) :: TOUTPUT !! Output time interval [hours].

   !-----------------------------------------------------------------------
   ! Time-Dependent Floating-Point Variables
   !-----------------------------------------------------------------------
   REAL(KIND=R8P) :: UZVAL !! Unsaturated zone validation parameter.
   REAL(KIND=R8P) :: OCNOW !! Current overland/channel flow time [hours].
   REAL(KIND=R8P) :: OCNEXT !! Next overland/channel flow time [hours].
   REAL(KIND=R8P) :: HRUZ !! Unsaturated zone head value [m].
   REAL(KIND=R8P) :: PNET !! Net precipitation [mm].
   REAL(KIND=R8P) :: PE !! Potential evapotranspiration [mm/s].
   REAL(KIND=R8P) :: EINT !! Interception evaporation [mm/s].
   REAL(KIND=R8P) :: ERZ !! Evapotranspiration from root zone [mm/s].
   REAL(KIND=R8P) :: DRAIN !! Drainage rate [mm/s].
   REAL(KIND=R8P) :: ESOIL !! Soil evaporation [mm/s].
   REAL(KIND=R8P) :: AE !! Actual evapotranspiration [mm/s].
   REAL(KIND=R8P) :: CSTOLD !! Previous canopy storage [mm].
   REAL(KIND=R8P) :: CPLAI !! Current plant leaf area index [-].
   REAL(KIND=R8P) :: PREST !! Residual precipitation [mm].
   REAL(KIND=R8P) :: TIMEUZ !! Unsaturated zone time [hours].
   REAL(KIND=R8P) :: HOTIME !! Hotstart time [hours].

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
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NGRID !! Grid element mapping.
   INTEGER(KIND=I_P), DIMENSION(NXEE,NYEE) :: INGRID !! Inverse grid mapping (x,y to element index).
   INTEGER(KIND=I_P), DIMENSION(NSETEE) :: IOCORS !! Output set coordinates.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NMC !! Meteorological station category for each element.
   INTEGER(KIND=I_P), DIMENSION(NXEE,NYEE) :: LCODEX !! Link codes for x-direction.
   INTEGER(KIND=I_P), DIMENSION(NXEE,NYEE) :: LCODEY !! Link codes for y-direction.
   INTEGER(KIND=I_P), DIMENSION(NSETEE) :: IODATA !! Output set data flags.
   INTEGER(KIND=I_P), DIMENSION(NSETEE) :: IOELEM !! Output set element flags.
   INTEGER(KIND=I_P), DIMENSION(NSETEE) :: IORES !! Output set results flags.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NRAINC !! Rainfall station category for each element.
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NOCBCC !! Overland flow boundary condition category.
   INTEGER(KIND=I_P), DIMENSION(NOCTAB,4) :: NOCBCD !! Overland flow boundary condition data.
   INTEGER(KIND=I_P), DIMENSION(NELEE,NCLASS) :: ICLIST !! List of elements for each output class.
   INTEGER(KIND=I_P), DIMENSION(NLFEE,2) :: NEXPO !! Link exponent data.
   INTEGER(KIND=I_P), DIMENSION(NCLASS) :: ICLNUM !! Number of elements in each output class.
   INTEGER(KIND=I_P), DIMENSION(:), ALLOCATABLE :: ZQTableLink !! Link metadata for a single ZQ table.
   INTEGER(KIND=I_P), DIMENSION(:), ALLOCATABLE :: ZQTableFace !! Face metadata for a single ZQ table.

   !-----------------------------------------------------------------------
   ! Time-Dependent Integer Arrays
   !-----------------------------------------------------------------------
   INTEGER(KIND=I_P), DIMENSION(NELEE) :: NSMC !! Snowmelt category for each element.
   !970212 TEMPORARY!
   INTEGER(KIND=I_P) :: FLERRC(0:100) !! Temporary error counter for flow component.
   INTEGER(KIND=I_P) :: SYERRC(0:100) !! Temporary error counter for sediment component.
   INTEGER(KIND=I_P) :: CMERRC(0:100) !! Temporary error counter for contaminant component.

   !-----------------------------------------------------------------------
   ! Static Floating-Point Arrays
   !-----------------------------------------------------------------------
   REAL(KIND=R8P), DIMENSION(NXEE) :: DXIN !! Grid spacing in x-direction [m].
   REAL(KIND=R8P), DIMENSION(NYEE) :: DYIN !! Grid spacing in y-direction [m].
   REAL(KIND=R8P), DIMENSION(NLFEE) :: WIDTF !! Width of river link [m].
   REAL(KIND=R8P), DIMENSION(NELEE) :: ZBED !! Bed elevation [m].
   REAL(KIND=R8P), DIMENSION(NLFEE) :: HFLBED !! Height of flow in channel bed [m].
   REAL(KIND=R8P), DIMENSION(NLFEE) :: ZFBED !! Elevation of channel bed floor [m].
   REAL(KIND=R8P), DIMENSION(NLFEE) :: DZFBED !! Thickness of channel bed sediment layer [m].
   REAL(KIND=R8P), DIMENSION(NVEE) :: LROOT !! Root length [m].
   REAL(KIND=R8P), DIMENSION(NLFEE) :: HFLBNK !! Height of flow in channel bank [m].
   REAL(KIND=R8P), DIMENSION(NSETEE) :: IOSTA !! Output set start time [hours].
   REAL(KIND=R8P), DIMENSION(NSETEE) :: IOSTEP !! Output set time step [hours].
   REAL(KIND=R8P), DIMENSION(NSETEE) :: IOEND !! Output set end time [hours].
   REAL(KIND=R8P), DIMENSION(NELEE) :: RHOSAR !! Saturated zone density [kg/m^3].
   REAL(KIND=R8P), DIMENSION(:), ALLOCATABLE :: ZQweirSill !! Weir sill elevations for ZQ tables [m].

   !-----------------------------------------------------------------------
   ! Time-Dependent Floating-Point Arrays
   !-----------------------------------------------------------------------
   REAL(KIND=R8P), DIMENSION(NELEE) :: CSTORE !! Canopy storage [mm].
   REAL(KIND=R8P), DIMENSION(NELEE) :: ERZA !! Actual evapotranspiration from root zone [mm].
   REAL(KIND=R8P), DIMENSION(NELEE) :: EPOT !! Potential evapotranspiration [mm/s].
   REAL(KIND=R8P), DIMENSION(NELEE) :: EINTA !! Interception evaporation [mm].
   REAL(KIND=R8P), DIMENSION(NVEE) :: EPOTR !! Potential evapotranspiration rate [mm/s].
   REAL(KIND=R8P), DIMENSION(NELEE) :: SD !! Snow depth [mm water equivalent].
   REAL(KIND=R8P), DIMENSION(NELEE) :: TS !! Snowpack temperature [C].
   REAL(KIND=R8P), DIMENSION(NELEE) :: SF !! Snowfall [mm].
   REAL(KIND=R8P), DIMENSION(LLEE) :: S !! Source/sink term for UZ flow [1/s].
   REAL(KIND=R8P), DIMENSION(nelee) :: precip_m_per_s !! Precipitation rate [m/s].
   REAL(KIND=R8P), DIMENSION(NVEE) :: OBSPE !! Observed precipitation [mm/hour].
   REAL(KIND=R8P), DIMENSION(NVEE) :: TA !! Air temperature [°C].
   REAL(KIND=R8P), DIMENSION(NVEE) :: U !! Wind speed [m/s].
   REAL(KIND=R8P), DIMENSION(NVEE) :: VPD !! Vapour pressure deficit [kPa].
   REAL(KIND=R8P), DIMENSION(NVEE) :: RN !! Net radiation [W/m^2].
   REAL(KIND=R8P), DIMENSION(NVEE) :: VHT !! Vegetation height [m].
   REAL(KIND=R8P), DIMENSION(NSETEE) :: IOTIME !! Output times for sets.
   REAL(KIND=R8P), DIMENSION(NELEE,4) :: DQ0ST !! Overland flow storage change [m^3/s].
   REAL(KIND=R8P), DIMENSION(NELEE,4) :: DQIST !! Interflow storage change [m^3/s].
   REAL(KIND=R8P), DIMENSION(NLFEE,3) :: DQIST2 !! Link interflow storage change [m^3/s].
   REAL(KIND=R8P), DIMENSION(NELEE) :: ESWA !! Actual soil evaporation [mm].
   REAL(KIND=R8P), DIMENSION(20) :: BALANC !! Mass balance components.
   REAL(KIND=R8P), DIMENSION(NELEE,2,NCONEE) :: CMEAN !! Mean contaminant concentration in solution [kg/m^3].
   REAL(KIND=R8P), DIMENSION(NELEE,2,NCONEE) :: SMEAN !! Mean contaminant concentration in sediment [kg/kg].
   REAL(KIND=R8P), DIMENSION(NELEE,2,NCONEE) :: ADMEAN !! Mean adsorbed contaminant concentration [kg/kg].

   !-----------------------------------------------------------------------
   ! Static Character Variables
   !-----------------------------------------------------------------------
   CHARACTER(LEN=LENGTH_FILEPATH) :: RESFIL !! Results filename.

END MODULE AL_D
