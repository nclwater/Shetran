!> summary: Central table of every Fortran unit number; keeps uniqueness checkable in one place.
!> author: GP; RJL; RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> Every named Fortran unit number the model uses, in one table. They were
!> previously split between `AL_C`, `AL_D` and `mod_error`, which made the one
!> property that matters about them — that no two names share a number unless
!> that is intended — impossible to check by reading. Nothing here is a
!> calculation; the module is a lookup table.
!>
!> The numbers occupy their rundata record positions, so they are not free to
!> renumber. `SFB` and `SRB` are non-opened placeholders retained by the
!> sediment interface and deliberately sit outside the rundata range.
!>
!> Adding a unit means adding it here and checking it against the numbers
!> already listed.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C, AL_D, mod_error; see docs/rename/proposal.md. |
!> @endhistory
MODULE file_units

   USE MOD_PARAMETERS, ONLY: I_P

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SFB, SRB, VSD, SYD, CMD, SPR, CMP, BUG, VSI, WLD, LFB, LHB, LGB, BFB, BHB, CMT, CMB, MND, &
             MNFC, MNFN, MNPL, MNPR, MNOUT1, MNOUT2, MNOUTPL, FRD, OCD, ETD, PPD, SMD, BKD, MED, PRD, &
             EPD, TIM, RES, HOT, VED, OFB, OHB, DIS, VSE, MAS, DIS2, TAH, TAL, disextra, zqd, pslextra, &
             FID_logfile

   INTEGER, PARAMETER :: SFB = 9876   !! Placeholder for the unimplemented sediment flow-boundary stream.
   INTEGER, PARAMETER :: SRB = 9877   !! Placeholder for the unimplemented sediment rating/boundary stream.
   INTEGER, PARAMETER :: VSD = 11     !! Variably saturated subsurface data input unit.
   INTEGER, PARAMETER :: SYD = 17     !! Sediment data input unit.
   INTEGER, PARAMETER :: CMD = 18     !! Contaminant data input unit.
   INTEGER, PARAMETER :: SPR = 24     !! Formatted sediment diagnostic/output unit.
   INTEGER, PARAMETER :: CMP = 25     !! Formatted contaminant diagnostic/output unit (called `CPR` in the manual).
   INTEGER, PARAMETER :: BUG = 26     !! Developer debug-output unit.
   INTEGER, PARAMETER :: VSI = 29     !! VSS initial-condition input unit.
   INTEGER, PARAMETER :: WLD = 31     !! Time-varying well-abstraction input unit.
   INTEGER, PARAMETER :: LFB = 32     !! Time-varying lateral subsurface-flow boundary unit.
   INTEGER, PARAMETER :: LHB = 33     !! Time-varying lateral subsurface-head boundary unit.
   INTEGER, PARAMETER :: LGB = 34     !! Time-varying lateral head-gradient boundary unit.
   INTEGER, PARAMETER :: BFB = 35     !! Time-varying aquifer-base flow boundary unit.
   INTEGER, PARAMETER :: BHB = 36     !! Time-varying aquifer-base head boundary unit.
   INTEGER, PARAMETER :: CMT = 39     !! First contaminant-migration boundary input unit.
   INTEGER, PARAMETER :: CMB = 40     !! Second contaminant-migration boundary input unit.
   INTEGER, PARAMETER :: MND = 53     !! Nitrogen and carbon data input unit.
   INTEGER, PARAMETER :: MNFC = 54    !! External carbon-input unit.
   INTEGER, PARAMETER :: MNFN = 55    !! External nitrogen-input unit.
   INTEGER, PARAMETER :: MNPL = 56    !! Nitrate plant-uptake input unit.
   INTEGER, PARAMETER :: MNPR = 57    !! Formatted nitrate diagnostic/output unit.
   INTEGER, PARAMETER :: MNOUT1 = 58  !! Nitrate carbon extra-output unit.
   INTEGER, PARAMETER :: MNOUT2 = 59  !! Nitrate nitrogen extra-output unit.
   INTEGER, PARAMETER :: MNOUTPL = 60 !! Nitrate plant-output unit.
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
   INTEGER(KIND=I_P), PARAMETER :: FID_logfile = 23 !! Default Fortran unit for primary PRI output.

END MODULE file_units

