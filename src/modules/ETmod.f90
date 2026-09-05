!> @brief Computes canopy interception, evapotranspiration, and land-surface evaporation fluxes.
!>
!> `ETmod` owns the evapotranspiration component's vegetation controls,
!> meteorological selectors, lookup tables, and current-column work arrays.
!> [[initialise_etmod]] allocates its run-sized state after the catchment
!> dimensions are known. Each timestep [[etsim]] visits every land element,
!> prepares its bank geometry and pressure-head profile, and delegates to the
!> private [[etin]] and [[et]] calculation path.
!>
!> [[frmod:inet]] reads user-manual records `ET2`--`ET18` into this module
!> after [[initialise_etmod]] has run. [[rest:metin]] supplies the current
!> meteorological values and advances time-varying vegetation parameters.
!> [[run_sim:simulation]] then calls [[etsim]] before the variably saturated
!> subsurface calculation on every model step. The resulting rainfall,
!> interception, root-extraction, soil-evaporation, and surface-water fluxes
!> are stored in shared `AL_C`/`AL_D` arrays used by water, contaminant, and
!> mass-balance calculations.
!>
!> | State group | Producer or updater | Principal consumer |
!> |:------------|:--------------------|:-------------------|
!> | `BMET*`, `MEASPE`, vegetation controls and lookup tables | [[frmod:inet]] | [[rest:metin]], [[et]] |
!> | `REL*`, `TIM*`, `NCT*`, and `*1` reference values | [[frmod:inet]] and [[rest:metin]] | [[utilsmod:terpo1]] |
!> | `DEL` | [[rest:metin]] | [[et]] |
!> | `PSI4`, `UZALFA` | [[etsim]] | [[et]] and exported AD state |
!>
!> `NCTCST`, `NCTPLA`, `NCTCLA`, and `NCTVHT` are current lower-breakpoint
!> cursors. [[frmod:inet]] initializes each enabled cursor to one; the number
!> of rows read from the corresponding manual `ET12` record remains local to
!> `INET`. [[utilsmod:terpo1]] advances the cursor while interpolating a ratio
!> from `REL*`/`TIM*`, then multiplies it by the fixed `*1` reference value.
!>
!> @warning
!> Private [[etchk2]] has no caller in the current source. If called, its
!> equality check would accept only `RDL=0`, although manual record `ET8`
!> permits a positive channel-root fraction. Consequently current ET input is
!> not validated by that routine.
!> @endwarning
!>
!> @warning
!> The module is mutable, single-run state. [[initialise_etmod]] allocates
!> every allocatable unconditionally, has no `STAT=` handling, and there is no
!> matching deallocator; a repeated call or allocation failure terminates via
!> the Fortran runtime.
!> @endwarning
!>
!> @note
!> Manual `ET2` allows a fourth, optional `BMETDATES` value. Current
!> [[frmod:inet]] first attempts `(4L7)` and falls back to the legacy three
!> logical values, defaulting `BMETDATES` false. The flag applies to separate
!> precipitation, potential-evaporation, and temperature series handled by
!> [[rest:metin]].
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-02 to 1998-10 | GP / RAH | 2.0--4.2 | Developed and reorganised the combined ET component. |
!> | 2008-12 | JE | 4.3.5F90 | Combined the former ET Fortran sources into this Fortran 90 module. |
!> | 2026-03-19 | SB | 4.6 | Added date-aware meteorological input and the run-sized allocator. |
!> | 2026-04-05 to 2026-04-14 | SvB | - | Removed `ALINIT`/GOTOs and added resistance error 4998. |
!> | 2026-05-03 | SvB | - | Resized `DEL` and explicitly initialized `IUNDEF`. |
!> @endhistory
MODULE ETmod

   USE SGLOBAL
   USE AL_G,     ONLY : ICMREF, NGDBGN, ICMREF
   USE AL_C,     ONLY : NVC, DTUZ, NRD, RDF, ERUZ, DELTAZ, CLAI, PNETTO, DRAINA, ESOILA, &
      NHBED, PLAI, NVSWLT, QVSWEL, eevap, UZNEXT, CWIDTH, &
      FHBED, NLYRBT, vspsi, NV
   USE AL_D,     ONLY : NMC, NRAINC, NM, NRAIN, U, PE, OBSPE, RN, VPD, PNET, precip_m_per_s, CPLAI, EINT, CSTOLD, CSTORE, &
      EPOT, EINTA, ERZA, ESWA, BEXSM, DRAIN, ERZ, AE, HRUZ, ESOIL, &
      NSMT, S, TIMEUZ, BWIDTH, &
      sf, sd, ts, nsmc !THESE NEEDED ONLY FOR AD
   USE mod_load_filedata,    ONLY : ALCHK

   USE MOD_PARAMETERS, ONLY : LENGTH_LINE, I_P
   USE MOD_ERROR, ONLY : errstat_alloc,  RAISE_ERROR, ERRLVL_fatal, ERRLVL_warn, FID_logfile

   USE UTILSMOD, ONLY : DCOPY
   USE SMmod,    ONLY : SMIN, &
      smelt, tmelt !THESE NEEDED ONLY FOR AD
!NEEDED ONLY FOR AD
   USE SMmod,    ONLY : rhos
   USE OCMOD2, ONLY  : GETHRF
   IMPLICIT NONE

   DOUBLEPRECISION, PARAMETER :: LAMDA=2465000. !! Latent heat of vaporisation used by the Penman equations (J/kg).
   DOUBLEPRECISION, PARAMETER :: GAMMA=0.659 !! Psychrometric constant used with `DEL` (mb/degree C).
   DOUBLEPRECISION, PARAMETER :: RHO=1.2 !! Fixed air density (kg/m3).
   DOUBLEPRECISION, PARAMETER :: CP=1003. !! Fixed specific heat capacity of air (J/kg/degree C).

   LOGICAL :: BAR(NVEE) !! Manual `ET8` selector: compute `RA` from wind when true; retain its input constant otherwise.
   LOGICAL :: BMETP !! Manual `ET2` selector for echoing meteorological input to the print file.
   LOGICAL :: BINETP !! Manual `ET2` selector for echoing ET parameter input to the print file.
   LOGICAL :: BMETAL !! Manual `ET2` selector for separate `PRD`/`EPD` forcing rather than combined `MED` forcing.
   LOGICAL :: BMETDATES !! Optional manual `ET2` selector for ISO-8601 dates in separate forcing files.

   INTEGER :: MODE(NVEE) !! Manual `ET8` actual-ET mode by vegetation type.
   INTEGER :: NF(NVEE) !! Number of active `PS1`/`RCF`/`FET` rows by vegetation type.
   INTEGER :: MEASPE(NVEE) !! Manual `ET6` measured-potential-evaporation selector by meteorological site.
   INTEGER :: MODECS(NVEE) !! Zero for constant `CSTCAP`; any nonzero value enables time interpolation.
   INTEGER :: MODEPL(NVEE) !! Zero for constant `PLAI`; any nonzero value enables time interpolation.
   INTEGER :: MODECL(NVEE) !! Zero for constant `CLAI`; any nonzero value enables time interpolation.
   INTEGER :: MODEVH(NVEE) !! Zero for constant `VHT`; any nonzero value enables time interpolation.
   INTEGER :: NCTCST(NVEE) !! Current lower-breakpoint cursor for canopy-storage interpolation.
   INTEGER :: NCTPLA(NVEE) !! Current lower-breakpoint cursor for ground-cover interpolation.
   INTEGER :: NCTCLA(NVEE) !! Current lower-breakpoint cursor for canopy-LAI interpolation.
   INTEGER :: NCTVHT(NVEE) !! Current lower-breakpoint cursor for vegetation-height interpolation.

   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: RA !! Aerodynamic resistance by vegetation type (s/m).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: RC !! Canopy resistance by vegetation type (s/m).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: RTOP !! Wind-independent `RA*U` factor by vegetation type.
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: CSTCAP !! Canopy storage capacity by vegetation type (mm).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: CK !! Canopy drainage coefficient by vegetation type (mm/s).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: CB !! Canopy drainage exponent coefficient by vegetation type (1/mm).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: DEL !! Saturation vapour-pressure slope by meteorological site (mb/degree C).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: PSI4 !! Current land-column pressure heads copied from `VSPSI` (m).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: UZALFA !! Current bank/channel root-access weighting by vertical cell.
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: CSTCA1 !! Initial/reference canopy storage capacity by vegetation type (mm).
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: PLAI1 !! Initial/reference maximum ground-cover proportion by vegetation type.
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: CLAI1 !! Initial/reference canopy leaf-area index by vegetation type.
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: VHT1 !! Initial/reference vegetation height by vegetation type (m).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: PS1 !! Manual `ET16` soil-moisture-tension table (m).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: FET !! Manual `ET16` actual/potential ET ratio table.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RCF !! Manual `ET16` canopy-resistance table (s/m).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RELCST !! Relative canopy-storage values by vegetation and breakpoint.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: TIMCST !! Canopy-storage breakpoint times (days).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RELPLA !! Relative ground-cover values by vegetation and breakpoint.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: TIMPLA !! Ground-cover breakpoint times (days).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RELCLA !! Relative canopy-LAI values by vegetation and breakpoint.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: TIMCLA !! Canopy-LAI breakpoint times (days).
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: RELVHT !! Relative vegetation-height values by vegetation and breakpoint.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: TIMVHT !! Vegetation-height breakpoint times (days).

   CHARACTER(132) :: msg !! Shared private warning/fatal diagnostic buffer.
   PRIVATE
   PUBLIC :: ETSIM, BMETP, BINETP, BMETAL, BMETDATES, MEASPE, CSTCAP, RC, BAR, RA, MODE, &
      NF, CK, CB, MODECS, MODEPL, MODECL, MODEVH, NCTCST, CSTCA1, RELCST, TIMCST, &
      NCTPLA, PLAI1, RELPLA, TIMPLA, NCTCLA, CLAI1, NCTVHT, VHT1, RELVHT, TIMVHT, &
      PS1, RCF, FET, RTOP, RELCLA, TIMCLA, del, &
      psi4, uzalfa, INITIALISE_ETMOD !THESE NEEDED ONLY FOR AD
CONTAINS


!> @brief Allocates and zero-initialises the run-sized ET state.
!>
!> [[frmod:frinit]] calls this routine after [[frmod:infr]] has established active
!> vegetation (`NV`), meteorological (`NM`), and rainfall (`NRAIN`) counts and
!> before [[frmod:inet]] reads the ET data. All 24 allocatables are initialized to
!> double-precision zero after allocation.
!>
!> | Arrays | Allocated shape | Role |
!> |:-------|:----------------|:-----|
!> | `RA`, `RC`, `RTOP`, `CSTCAP`, `CK`, `CB` | `NV` | Per-vegetation physical controls. |
!> | `CSTCA1`, `PLAI1`, `CLAI1`, `VHT1` | `NV` | Per-vegetation reference values. |
!> | `DEL` | `MAX(NV,NM,NRAIN)` | Forcing slope sized for the largest active legacy index domain. |
!> | `PSI4`, `UZALFA` | `LLEE` | Current vertical-column workspace. |
!> | `PS1`, `FET`, `RCF` | `NV x NUZTAB` | Soil-tension lookup tables. |
!> | `REL*`, `TIM*` | `NV x NVBP` | Time-varying vegetation ratios and breakpoint times. |
!>
!> The larger `DEL` extent is intentional current behaviour: [[rest:metin]]
!> writes it by meteorological-site index, while the three active counts can
!> differ. `NVEE` remains the common compile-time capacity for vegetation,
!> meteorological, and rainfall categories.
!>
!> @warning
!> Allocation is unconditional and has no `STAT=` branch. Call exactly once
!> per process after valid positive run dimensions have been established.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03-19 | SB | 4.6 | Added the allocator while converting ET arrays to run-sized storage. |
!> | 2026-05-03 | SvB | - | Expanded `DEL` to `MAX(NV,NM,NRAIN)` to avoid undersizing the meteorological domain. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE INITIALISE_ETMOD()

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = 'ETmod:INITIALISE_ETMOD'

      ALLOCATE (RA(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RA",location, emsg)
      ALLOCATE (RC(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RC",location, emsg)
      ALLOCATE (RTOP(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RTOP",location, emsg)
      ALLOCATE (CSTCAP(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CSTCAP",location, emsg)
      ALLOCATE (CK(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CK",location, emsg)
      ALLOCATE (CB(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CB",location, emsg)
      ALLOCATE (DEL(MAX(NV, NM, NRAIN)), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DEL",location, emsg)
      ALLOCATE (PSI4(LLEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "PSI4",location, emsg)
      ALLOCATE (UZALFA(LLEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "UZALFA",location, emsg)
      ALLOCATE (CSTCA1(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CSTCA1",location, emsg)
      ALLOCATE (PLAI1(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "PLAI1",location, emsg)
      ALLOCATE (CLAI1(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "CLAI1",location, emsg)
      ALLOCATE (VHT1(NV), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "VHT1",location, emsg)
      ALLOCATE (PS1(NV,NUZTAB), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "PS1",location, emsg)
      ALLOCATE (FET(NV,NUZTAB), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "FET",location, emsg)
      ALLOCATE (RCF(NV,NUZTAB), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RCF",location, emsg)
      ALLOCATE (RELCST(NV,NVBP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RELCST",location, emsg)
      ALLOCATE (TIMCST(NV,NVBP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TIMCST",location, emsg)
      ALLOCATE (RELPLA(NV,NVBP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RELPLA",location, emsg)
      ALLOCATE (TIMPLA(NV,NVBP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TIMPLA",location, emsg)
      ALLOCATE (RELCLA(NV,NVBP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RELCLA",location, emsg)
      ALLOCATE (TIMCLA(NV,NVBP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TIMCLA",location, emsg)
      ALLOCATE (RELVHT(NV,NVBP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RELVHT",location, emsg)
      ALLOCATE (TIMVHT(NV,NVBP), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TIMVHT",location, emsg)

      ! -----Zero-initialise all allocatables
      RA=0.0d0
      RC=0.0d0
      RTOP=0.0d0
      cstcap=0.0d0
      ck=0.0d0
      cb=0.0d0
      del=0.0d0
      PSI4=0.0d0
      UZALFA=0.0d0
      CSTCA1=0.0d0
      PLAI1=0.0d0
      CLAI1=0.0d0
      VHT1=0.0d0
      PS1=0.0d0
      FET=0.0d0
      RCF=0.0d0
      RELCST=0.0d0
      TIMCST=0.0d0
      RELPLA=0.0d0
      TIMPLA=0.0d0
      RELCLA=0.0d0
      TIMCLA=0.0d0
      RELVHT=0.0d0
      TIMVHT=0.0d0

   END SUBROUTINE INITIALISE_ETMOD

!> @brief Computes interception and evapotranspiration for one land element.
!>
!> `IEL` selects meteorological site `MS=NMC(IEL)` and vegetation type
!> `N=NVC(IEL)`. The routine updates shared scalar ET state and the current
!> element's canopy store, root extraction, and cell sinks; [[etin]] converts
!> the millimetre-based results into the model's metre/second arrays.
!>
!> Potential evaporation comes either from measured `OBSPE(MS)` or from the
!> Penman numerator. With `BAR(N)` true, aerodynamic resistance is updated as
!>
!> \[
!> RA_N =
!> \begin{cases}
!> RTOP_N/U_{MS}, & U_{MS}>0,\\
!> 10^{10}, & U_{MS}\leq0.
!> \end{cases}
!> \]
!>
!> The current code then defines
!>
!> \[
!> BOTTOM=LAMDA(DEL_{MS}+GAMMA),
!> \]
!>
!> and either sets `PE=OBSPE(MS)` and `TOP=PE*BOTTOM`, or calculates
!>
!> \[
!> TOP=\max\left(0,
!> RN_{MS}DEL_{MS}+\frac{RHO\,CP\,VPD_{MS}}{RA_N}\right),
!> \qquad PE=TOP/BOTTOM.
!> \]
!>
!> `PE` and actual ET are in mm/s inside this routine. `EINT`, `DRAIN`, and
!> `CSTORE` are timestep depths in mm. Before canopy storage limitation,
!>
!> \[
!> PNET_0=1000P_r(1-CPLAI)DTUZ,\qquad
!> EINT_0=PE\,CPLAI\,DTUZ,
!> \]
!>
!> \[
!> Q=CPLAI(1000P_r-PE).
!> \]
!>
!> | Canopy-store state | Current branch |
!> |:-------------------|:---------------|
!> | `CSTOLD>CSTCAP` and `Q>0` | Integrates exponential drainage while adding net canopy supply. |
!> | `CSTOLD>CSTCAP` and `Q<=0` | Draws down the store; drainage continues only while it remains above capacity. |
!> | `CSTOLD<=CSTCAP` | Scales interception evaporation by wet-canopy fraction `F1`, then drains any excess above capacity. |
!>
!> The exponential drainage relation uses `CK` and `CB`; each branch recovers
!> drainage by canopy mass balance. Final throughfall is
!> `PNET=(PNET_0+DRAIN)/DTUZ`.
!>
!> Actual ET is evaluated for each rooted cell, counting downward from the
!> surface. The manual's modes have these current implementations:
!>
!> | `MODE(N)` | Actual-ET calculation |
!> |:----------|:----------------------|
!> | 1 | Uses constant `RC(N)` in the Penman--Monteith expression; a nonnegative `PSI4` sets `AE=PE`. |
!> | 2 | Linearly interpolates `RC(N)` from the `PS1`/`RCF` table and applies Penman--Monteith. |
!> | 3 | Linearly interpolates `FE=AE/PE` from `PS1`/`FET`, then sets `AE=PE*FE`. |
!>
!> Modes 1 and 2 use
!>
!> \[
!> AE=\frac{TOP}
!> {LAMDA\left(DEL_{MS}+GAMMA(1+RC_N/RA_N)\right)}.
!> \]
!>
!> Values below/above the tension-table range use its first/last row; a
!> nonnegative pressure head uses the last `RCF` value in mode 2 and `FE=1` in
!> mode 3. Interior values are linearly interpolated between adjacent `PS1`
!> rows. Any mode other than 2 or 3 follows the mode-1 branch; notably
!> [[frmod:inet]] also treats legacy `MODE=4` as a constant-`RC` case even
!> though the manual documents only modes 1--3.
!>
!> Root extraction is applied only when no surface water is present:
!>
!> \[
!> E_k=AE\,CPLAI(1-F_1)\frac{RDF_{N,k}}{1+UZALFA_k}.
!> \]
!>
!> The routine accumulates `ERZ` in mm/s, converts each `E_k` to m/s in
!> `ERUZ(IEL,II)`, and writes volumetric sink
!> `S(II)=ERUZ(IEL,II)/DELTAZ(II,IEL)`. Top-cell soil evaporation is
!> `ESOIL=0.5*AE*(1-CPLAI)`. For bank elements the loop is extended to the
!> exposed channel-bed cell. A root range deeper than `top_cell_no` is clipped
!> and warning 4999 is emitted only on the first occurrence.
!>
!> @warning
!> Fatal error 4998 checks `RA<=0` only while calculating unmeasured potential
!> evaporation. Measured `PE` combined with mode 1 or 2 can still divide by an
!> invalid `RA`. The zero-capacity repair also runs only after evaluating
!> `CT1/CSTCAP`, and the logarithmic canopy branches assume valid nonzero
!> `CB` and positive logarithm arguments.
!> @endwarning
!>
!> @warning
!> Mode 3 with `NF=1` and `PSI4==PS1(N,1)<0` enters an empty interpolation
!> loop and can use an undefined `FE`; the manual says the sole row should
!> cover every negative tension. Equal adjacent `PS1` rows are likewise not
!> guarded. An ordinary element with `NRD(N)=0` skips the complete cell loop,
!> leaving `ESOIL` and per-cell extraction state unchanged. Other assumed
!> nonzero denominators include `DTUZ`, `DELTAZ`, and `1+UZALFA`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1992-09-08 | JE | 3.4 | Corrected repeated assignment of root-extraction state. |
!> | 1992-12-11 | GP | 3.4 | Moved dry-soil evaporation and the final top-cell sink update to [[etin]]. |
!> | 1995-07-13 | GP | 4.0 | Removed the former mode-4 calculation and adopted current subsurface-layer state. |
!> | 1997-05-15 | RAH | 4.1 | Swapped `DELTAZ` indices and explicitly typed the routine. |
!> | 1998-10-21 to 1998-11-03 | RAH | 4.2 | Reworked control flow, resistance state, outputs, and root-cell handling. |
!> | 2007-09-04 | SB | - | Revised zero/small-capacity canopy-storage handling and evaporation mass balance. |
!> | 2015-05-27 | SB | - | Limited top-cell soil evaporation to half the short-grass estimate. |
!> | 2026-04-06 | SvB | - | Replaced the remaining table-search GOTOs with structured loops. |
!> | 2026-04-14 | SvB | - | Restored forcing-site indexing and added resistance error 4998. |
!> @endhistory
   SUBROUTINE ET (IEL)
      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IEL !! Land-element number to process.

      ! Locals, etc
      DOUBLE PRECISION, PARAMETER :: RABIG = 1.0D10 !! Calm-wind aerodynamic-resistance substitute (s/m).
      INTEGER :: II !! Current vertical-cell index, numbered upward from the aquifer bed.
      INTEGER :: IL !! Channel link associated with a bank element.
      INTEGER :: ITYPE !! Element type/bank side from `ICMREF(IEL,1)`.
      INTEGER :: K !! Number of root/exposed-bed cells processed from the surface downward.
      INTEGER :: KF !! Active tension-table row count for the current vegetation type.
      INTEGER :: KK !! Surface-down root-density index.
      INTEGER :: KL !! Tension-table search index.
      INTEGER :: M1 !! Current actual-ET mode.
      INTEGER :: MR !! Rainfall-station index retained from legacy code but otherwise unused.
      INTEGER :: MS !! Meteorological-site index for the element.
      INTEGER :: N !! Vegetation-type index for the element.
      DOUBLE PRECISION :: BOTTOM !! Penman denominator based on latent heat, `DEL`, and `GAMMA`.
      DOUBLE PRECISION :: CALC !! Temporary logarithmic/interpolation value.
      DOUBLE PRECISION :: CT1 !! Candidate canopy storage after supply or evaporation (mm).
      DOUBLE PRECISION :: DFET !! Difference between adjacent `FET` rows.
      DOUBLE PRECISION :: DPS1 !! Difference between adjacent `PS1` rows (m).
      DOUBLE PRECISION :: DRCF !! Difference between adjacent `RCF` rows (s/m).
      DOUBLE PRECISION :: DUM !! Current cell extraction, first in mm/s and then m/s.
      DOUBLE PRECISION :: F1 !! Wet-canopy/available-storage fraction applied to interception and transpiration.
      DOUBLE PRECISION :: FE !! Actual/potential ET ratio for mode 3.
      DOUBLE PRECISION :: Q !! Net supply rate to canopy storage (mm/s).
      DOUBLE PRECISION :: TOP !! Penman numerator, or measured `PE*BOTTOM`.
      DOUBLE PRECISION :: XPSTOR !! Exponential canopy-storage integration term.
      LOGICAL, SAVE :: first = .TRUE. !! Once-only guard for root-depth warning 4999.

      !----------------------------------------------------------------------*
      !-----------------
      !  Preliminaries
      !-----------------
      !-----Local indices
      MS = NMC (IEL)
      MR = NRAINC (IEL)
      N = NVC (IEL)

      !-----Aerodynamic resistance (unless constant)
      IF (BAR (N)) THEN
         IF (U (MS) > ZERO) THEN
            RA (N) = RTOP (N) / U (MS)
         ELSE
            RA (N) = RABIG
         END IF
      END IF

      !-----Potential evapotranspiration & Penman equation numerator
      !! sb 20/6/07 has del been defined here? I think not
      BOTTOM = LAMDA * (DEL (MS) + GAMMA)
      IF (MEASPE (MS) /= 0) THEN
         !---------PE ALREADY KNOWN AS A MEASURED QUANTITY
         PE = OBSPE (MS)
         TOP = PE * BOTTOM
      ELSE
         !---------PE MUST BE CALCULATED USING PENMAN EQUATION
         IF (RA (N) <= ZERO) THEN
            WRITE(msg, '(A,I0,A,I0,A,ES24.16E3)') 'invalid aerodynamic resistance in ET: IEL=', IEL, ' N=', N, ' RA=', RA(N)
            CALL RAISE_ERROR(ERRLVL_fatal, 4998, FID_logfile, IEL, 0, msg)
         END IF
         TOP = MAX (ZERO, RN (MS) * DEL (MS) + RHO * CP * VPD (MS) / RA (N))
         !         TOP = TOP * 1D3 / densityOfWater   is implied!
         PE = TOP / BOTTOM
      END IF

      !--------------------------------------
      !  INTERCEPTION COMPONENT CALCULATION
      !--------------------------------------
      !-----NET RAIN NOT FALLING ON VEGETATION (mm)
      PNET = precip_m_per_s(IEL) * 1000.0D0 * (ONE - CPLAI) * DTUZ

      !-----EVAPORATION OF INTERCEPTED RAIN (mm)
      EINT = PE * CPLAI * DTUZ

      !-----NET SUPPLY TO CANOPY (mm/s)
      Q = CPLAI * (precip_m_per_s(IEL) * 1000.0D0 - PE)

      !-----Update storage of, & calculate drainage from, canopy
      !! sb 4/9/07 note that the canopy storage is often greater than canopy s
      !! hence with very small cstcap, canopy evap. is often quite large
      CSTOLD = CSTORE (IEL)

      !sb 4/9/07 changed GE to GT to stop error if cstcap=0
      IF (CSTOLD > CSTCAP (N)) THEN
         F1 = ONE
         IF (Q > ZERO) THEN
            !------------------------------
            !--CASE OF CSTORE>=CSTCAP , Q>0
            CALC = CB (N) * (CSTOLD - CSTCAP (N) + DTUZ * Q)
            DUM = CB (N) * (CSTOLD - CSTCAP (N))
            CALC = CALC - LOG (CK (N) * EXP (CALC) - CK (N) * EXP (DUM) + Q)
            CSTORE (IEL) = MAX (ZERO, CSTCAP (N) + (LOG (Q) + CALC) / CB (N))
            DRAIN = -CSTORE (IEL) + CSTOLD + Q * DTUZ
         ELSE
            !-------------------------------
            !--CASE OF CSTORE>=CSTCAP , Q<=0
            CT1 = CSTOLD + DTUZ * Q
            IF (CT1 <= CSTCAP (N)) THEN
               CSTORE (IEL) = MAX (ZERO, CT1)
               IF (CT1 < ZERO) EINT = EINT + CT1
               DRAIN = ZERO
            ELSE
               XPSTOR = EXP (-CB (N) * (CT1 - CSTCAP (N)))
               CALC = LOG (DTUZ * CB (N) * CK (N) + XPSTOR)
               CSTORE (IEL) = MAX (ZERO, CSTCAP (N) - CALC / CB (N))
               DRAIN = -CSTORE (IEL) + CSTOLD + Q * DTUZ
            END IF
         END IF
      ELSE
         !-----------------------
         !--CASE OF CSTORE<CSTCAP
         CT1 = CSTOLD + DTUZ * CPLAI * precip_m_per_s(IEL) * 1000.0D0
         F1 = MIN (CT1 / CSTCAP (N), ONE)

         !sb 4/9/07
         IF (LEZERO(CSTCAP(N))) THEN
            IF (LEZERO(CT1)) THEN
               F1 = ZERO
            ELSE
               F1 = ONE
            END IF
         END IF
         !end of sb 4/9/07

         EINT = EINT * F1
         CT1 = CT1 - EINT

         IF (CT1 > CSTCAP (N)) THEN
            XPSTOR = EXP (-CB (N) * (CT1 - CSTCAP (N)))
            CALC = LOG (DTUZ * CB (N) * CK (N) + XPSTOR)
            CSTORE (IEL) = MAX (ZERO, CSTCAP (N) - CALC / CB (N))
            DRAIN = -CSTORE (IEL) + CT1
         ELSE
            CSTORE (IEL) = MAX (ZERO, CT1)
            !sb 4/9/07 remove loss of evap if evap is more than rain plus storage
            IF (CT1 < ZERO) THEN
               F1 = (EINT + CT1) / EINT
               EINT = EINT + CT1
            END IF
            !end of sb 4/9/07
            DRAIN = ZERO
         END IF
      END IF

      !-----TOTAL THROUGHFALLMM AND MM/S
      PNET = PNET + DRAIN
      PNET = PNET / DTUZ

      !------------------------------------------------
      !  EVAPOTRANSPIRATION COMPONENT CALCULATIONS
      !------------------------------------------------
      !
      !  NOTE THAT POTENTIAL (PE) AND ACTUAL (AE)
      !  EVAPOTRANSPIRATION RATES ARE CALCULATED IN MM/SEC
      !
      M1 = MODE (N)
      K = NRD (N)
      ERZ = ZERO

      ! CALCULATE EXPOSED BED CELL, EXTEND LOOP 310 TO CHANNEL BED.
      ! CALCULATE PLANT UPTAKE FROM STREAM FOR BANK ELEMENTS ONLY
      !
      ITYPE = ICMREF (IEL, 1)
      IF (ITYPE == 1 .OR. ITYPE == 2) THEN
         IL = ICMREF (IEL, 4)
         K = MAX (top_cell_no - NHBED (IL, ITYPE), K)
      END IF

      IF (top_cell_no - K < 0) THEN
         K = top_cell_no
         WRITE(msg,'(A)') 'root zone extends below aquifer bed. Values below aquifer bed are ignored'
         IF (first) THEN
            CALL RAISE_ERROR(ERRLVL_warn, 4999, FID_logfile, 0, 0, msg)
            first = .FALSE.
         END IF
      END IF

      !     Count from the top cell down (for RDF subscript)
      DO KK = 1, K
         !        Corresponding bottom-up cell number
         II = top_cell_no - KK + 1

         IF (M1 /= 2 .AND. M1 /= 3) THEN
            !--------------------------------
            !  MODE 1 CALCULATIONS.....
            !--------------------------------
            IF (PSI4 (II) >= ZERO) THEN
               AE = PE
            ELSE
               AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE + RC (N) / RA (N))))
            END IF

         ELSE IF (M1 == 2) THEN
            !--------------------------------
            !  MODE 2 CALCULATIONS.....
            !--------------------------------
            !--LINEAR INTERPOLATION FOR VALUE OF RC DEPENDENT ON PSI4 (EITHER
            !--WATER CONTENT OR TENSION BUT MUST BE COMPATIBLE WITH UZ COMP.)
            KF = NF (N)
            IF (PSI4 (II) >= ZERO) THEN
               RC (N) = RCF (N, KF)
            ELSE IF (PSI4 (II) <= PS1 (N, 1)) THEN
               RC (N) = RCF (N, 1)
            ELSE IF (PSI4 (II) > PS1 (N, KF)) THEN
               RC (N) = RCF (N, KF)
            ELSE
               ! Modernized interpolation loop replacing GOTO logic
               DO KL = 2, KF
                  IF (PSI4 (II) <= PS1 (N, KL)) THEN
                     DPS1 = PS1 (N, KL) - PS1 (N, KL - 1)
                     DRCF = RCF (N, KL) - RCF (N, KL - 1)
                     CALC = (PSI4 (II) - PS1 (N, KL - 1)) * DRCF / DPS1
                     RC (N) = RCF (N, KL - 1) + CALC
                     EXIT
                  END IF
               END DO
            END IF

            AE = TOP / (LAMDA * (DEL (MS) + GAMMA * (ONE + RC (N) / RA (N))))

         ELSE IF (M1 == 3) THEN
            !--------------------------------
            !  MODE 3 CALCULATIONS.....
            !--------------------------------
            !  CALCULATE AE/PE RATIO DEPENDENT ON PSI4 BY LINEAR INTERPOLATION
            KF = NF (N)
            IF (PSI4 (II) >= ZERO) THEN
               FE = ONE
            ELSE IF (PSI4 (II) < PS1 (N, 1)) THEN
               FE = FET (N, 1)
            ELSE IF (PSI4 (II) > PS1 (N, KF)) THEN
               FE = FET (N, KF)
            ELSE
               ! Modernized interpolation loop replacing GOTO logic
               DO KL = 2, KF
                  IF (PSI4 (II) <= PS1 (N, KL)) THEN
                     DFET = FET (N, KL) - FET (N, KL - 1)
                     DPS1 = PS1 (N, KL) - PS1 (N, KL - 1)
                     CALC = (PSI4 (II) - PS1 (N, KL - 1)) * DFET / DPS1
                     FE = FET (N, KL - 1) + CALC
                     EXIT
                  END IF
               END DO
            END IF

            AE = PE * FE

         END IF

         !-----PUT PLANT UPTAKE INTO GLOBAL ARRAY FOR CONTAMINANTS
         !-----AE IS IN MM/S AND S IS IN M/S
         DUM = ZERO
         IF (HRUZ <= ZERO) DUM = AE * CPLAI * (ONE - F1) * RDF (N, KK) / (ONE + UZALFA (II))

         ERZ = ERZ + DUM
         DUM = DUM * 1.0D-3
         ERUZ (IEL, II) = DUM

         IF (NOTZERO(DUM)) THEN
            S (II) = DUM / DELTAZ (II, IEL)
         ELSE
            S (II) = ZERO
         END IF

         !-----CALCULATE SOIL-EVAPORATION : ESOIL IN MM/S
         ! sb 270515 soil evap should be less than short grass evap
         IF (II == top_cell_no) ESOIL = 0.5D0 * AE * (1.0D0 - CPLAI)

      END DO

   END SUBROUTINE ET




!> @brief Retained private checker for the vegetation channel-root fraction.
!>
!> This routine passes `RDL(1:NV)` to [[mod_load_filedata:alchk]] with exact
!> relation `EQ`, object zero, tolerance zero, and error action 2. `LDUM1` is
!> overwritten with the per-vegetation failure mask and the saved `NERR`
!> counter is incremented. Any nonzero final count then raises fatal error
!> 1000 on `PRI`.
!>
!> | Entry requirement | Current reason |
!> |:------------------|:---------------|
!> | `NV>=1` | Defines the explicit shapes and checked range. |
!> | `PRI` open for formatted output | Receives `ALCHK` and fatal diagnostics. |
!> | `LDUM1` extent at least `NV` | Used as the complete failure-mask workspace. |
!>
!> @warning
!> No current code calls `ETCHK2`, and it is private. Moreover, its equality
!> test rejects every nonzero `RDL`, whereas manual `ET8` defines positive
!> `RDL` as the proportion of bank-element roots taking water from the channel.
!> The active [[frmod:inet]] path reads `RDL` without invoking this checker.
!> @endwarning
!>
!> @note
!> `NERR` is initialized by a `DATA` statement and therefore has implicit
!> saved state; it is not reset at routine entry. `FATAL` is a retained unused
!> local parameter. These details are documented without changing the legacy
!> interface or behaviour.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-11-03 | RAH | 4.2 | Added the ET checker from the overland/channel checking pattern. |
!> | 2026-05-03 | SvB | - | Made the zero-valued fixed-index argument `IUNDEF` an explicitly initialized parameter. |
!> @endhistory
   SUBROUTINE ETCHK2 (PRI, NV, RDL, LDUM1)
      INTEGER :: PRI !! Unit receiving check and fatal-error diagnostics.
      INTEGER :: NV !! Number of vegetation entries to check.
      DOUBLEPRECISION :: RDL(NV) !! Manual `ET8` channel-root fractions tested against exact zero.
      LOGICAL :: LDUM1(NV) !! Per-entry failure-mask workspace overwritten by `ALCHK`.
      INTEGER :: FATAL !! Retained unused fatal-action constant.
      INTEGER :: ERR !! Nonfatal check action passed to `ALCHK`.
      PARAMETER (FATAL = 1, ERR = 2)
      INTEGER, PARAMETER :: IUNDEF = 0 !! Placeholder outer subscript for the one-dimensional diagnostic.
      INTEGER :: NERR !! Saved cumulative check-failure count.
      DATA NERR / 0 /

      CALL ALCHK (ERR, 1062, PRI, 1, NV, IUNDEF, IUNDEF, 'RDL(veg)', &
         'EQ', ZERO1, ZERO , RDL, NERR, LDUM1)

      IF (NERR.GT.0) CALL RAISE_ERROR(ERRLVL_fatal, 1000, PRI, 0, 0, 'Error(s) detected while checking ET input data')
   END SUBROUTINE ETCHK2



!> @brief Coordinates snow/ET processing and exports fluxes for one element.
!>
!> `ETIN` forms current canopy area
!> `CPLAI=MIN(CLAI(N),1)*PLAI(N)`, where `N=NVC(IEL)`. With snowmelt enabled,
!> the first [[smmod:smin]] call decides whether freezing/snowpack processing
!> has already supplied the ET state or sets `NSMT` to request [[et]]. If
!> `NSMT` is nonzero, `ET` runs and a second `SMIN` call may melt an existing
!> snowpack. Without snowmelt, `ET` always runs.
!>
!> The routine subtracts interception evaporation from the potential rate and
!> exports the legacy millimetre quantities as water-flow rates:
!>
!> | Output | Current assignment | Units |
!> |:-------|:-------------------|:------|
!> | `PNETTO(IEL)` | `PNET/1000` | m/s |
!> | `EPOT(IEL)` | `(PE-EINT/DTUZ)/1000` | m/s |
!> | `EINTA(IEL)` | `EINT/(1000*DTUZ)` | m/s |
!> | `DRAINA(IEL)` | `DRAIN/(1000*DTUZ)` | m/s |
!> | `ERZA(IEL)` | `ERZ/1000` | m/s |
!> | `ESOILA(IEL)` | `ESOIL/1000` | m/s |
!>
!> If `NVSWLT(IEL)` identifies an irrigation well, its `QVSWEL` rate is
!> multiplied by `cellarea(WEL)/cellarea(IEL)` and added to `PNETTO`.
!>
!> When `HRUZ` indicates surface water at timestep start, the trial depth is
!>
!> \[
!> h'=HRF(IEL)-ZGRUND(IEL)+(PNETTO-EPOT)DTUZ.
!> \]
!>
!> If `h'` remains nonnegative, all available potential evaporation is assigned
!> to `ESWA`. If it is negative, `EDUM=-h'/DTUZ` is the unsatisfied part:
!> `ESWA=EPOT-EDUM`, and `EDUM` becomes soil evaporation unless top-cell
!> `PSI4<-150` m. In that branch `HRUZ` and the shared scalar `PNET` are set to
!> zero. With no initial surface water, `ESWA` is zero and [[et]] has already
!> calculated `ESOILA`.
!>
!> Finally `EEVAP=ESWA+ESOILA`, and soil evaporation is added to the current
!> top-cell sink as `ESOILA/DELTAZ`. Root-extraction contributions written by
!> [[et]] are already present in `S`.
!>
!> @warning
!> The unit conversions and area/thickness scaling assume positive `DTUZ`,
!> `cellarea(IEL)`, and top-cell `DELTAZ`. A nonzero `NVSWLT(IEL)` is also
!> assumed to be a valid index for both `QVSWEL` and `cellarea`; this routine
!> performs no bounds or denominator checks.
!> @endwarning
!>
!> @warning
!> If the first [[smmod:smin]] call leaves `NSMT=0`, [[et]] is skipped and
!> `ETIN` exports the scalars left by `SMET`. Current `SMET` does not assign
!> `DRAIN`, and it can leave `PNET` unchanged when there is neither snowpack nor
!> precipitation to trigger `SM`; `DRAINA` and sometimes `PNETTO` can therefore
!> inherit a previous element's value. `SMET` also zeroes `S(1:NRD)` although
!> `ET` writes root sinks at the surface-indexed cells
!> `top_cell_no-NRD+1:top_cell_no`. This cross-module state behaviour is not
!> repaired here.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1992-12-11 | GP | 3.4 | Moved dry-soil and surface-water evaporation partitioning into this wrapper. |
!> | 1994-10-01 | RAH | 3.4.1 | Added explicit legacy double-precision typing. |
!> | 1995-01-18 | GP | 4.0 | Adopted `NVSWLT`, `QVSWEL`, and `DELTAZ` for irrigation and cell scaling. |
!> | 1997-05-16 | RAH | 4.1 | Swapped `DELTAZ` indices, removed redundant outputs, and bounded `CPLAI`. |
!> | 2026-04-06 to 2026-04-07 | SvB | - | Structured snow/ET flow and made conversions double precision. |
!> @endhistory
   SUBROUTINE ETIN (IEL)
      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: IEL !! Land-element number to process.

      ! Locals, etc
      INTEGER :: MR !! Rainfall-station index retained from legacy code but otherwise unused.
      INTEGER :: MS !! Meteorological-site index retained from legacy code but otherwise unused.
      INTEGER :: N !! Vegetation-type index for the element.
      INTEGER :: WEL !! Irrigation-well/transfer element selected by `NVSWLT(IEL)`.
      DOUBLE PRECISION :: EDUM !! Potential evaporation not supplied by initial surface water (m/s).

      !----------------------------------------------------------------------*
      MS = NMC (IEL)
      MR = NRAINC (IEL)
      N = NVC (IEL)

      ! CALCULATE INTERCEPTION AREA OF VEGETATION
      CPLAI = MIN (CLAI (N), ONE) * PLAI (N)

      ! CHECK FOR SNOWMELT CALCULATIONS, & SOLVE ET IF NECESSARY.
      ! NSMT IS AUTOMATICALLY SET TO 1 IF ET-CALCS FOR TEMP > 0 ARE NEEDED
      NSMT = 0
      IF (BEXSM) CALL SMIN (IEL)

      ! Modernized logic to eliminate GOTO 10
      IF (NSMT /= 0 .OR. .NOT. BEXSM) THEN
         CALL ET (IEL)
         IF (BEXSM) CALL SMIN (IEL)
      END IF

      !-----Calculate potential evapotranspiration
      PE = PE - EINT / DTUZ

      !-----STORE RESULTS IN ARRAYS
      ! Upgraded constants to strict double precision
      PNETTO (IEL) = PNET / 1000.0D0
      EPOT (IEL)   = PE / 1000.0D0
      EINTA (IEL)  = EINT / (1000.0D0 * DTUZ)
      DRAINA (IEL) = DRAIN / (1000.0D0 * DTUZ)
      ERZA (IEL)   = ERZ / 1000.0D0
      ESOILA (IEL) = ESOIL / 1000.0D0

      ! ADD IRRIGATION FLUX FROM WELLS INTO PNETTO
      WEL = NVSWLT (IEL)

      IF (WEL /= 0) THEN
         PNETTO (IEL) = PNETTO (IEL) + QVSWEL (WEL) * (cellarea (WEL) / cellarea (IEL))
      END IF

      ! Calculations for HRUZ(net), ESWA, EEVAP, ESOILA
      ! If surface water exists at start of timestep, available potential
      !   evaporation is partitioned into evaporation from surface water and
      !   evaporation from the soil (assuming the soil is near saturation).
      ! If no surface water exists, evaporation from the soil has already been
      !   calculated in the ET subroutine.
      ! ESOILA switched off for evap. from dry soil when surface water
      ! initially exists GP 11/12/92
      IF (GTZERO(HRUZ)) THEN
         HRUZ = getHRF(IEL) - ZGRUND (IEL) + (PNETTO (IEL) - EPOT (IEL)) * DTUZ

         IF (LTZERO(HRUZ)) THEN
            EDUM = -HRUZ / DTUZ
            ESWA (IEL) = EPOT (IEL) - EDUM

            IF (PSI4 (top_cell_no) < -150.0D0) THEN
               ESOILA (IEL) = zero
            ELSE
               ESOILA (IEL) = EDUM
            END IF

            HRUZ = zero
            PNET = zero
         ELSE
            ESOILA (IEL) = zero
            ESWA (IEL) = EPOT (IEL)
         END IF
      ELSE
         ESWA (IEL) = zero
      END IF

      EEVAP (IEL) = ESWA (IEL) + ESOILA (IEL)

      S (top_cell_no) = S (top_cell_no) + ESOILA (IEL) / DELTAZ (top_cell_no, IEL)

   END SUBROUTINE ETIN

!> @brief Advances evapotranspiration and interception for every land element.
!>
!> This is the public timestep driver called by `run_sim:SIMULATION` after
!> [[rest:tmstep]] has selected `UZNEXT` and updated meteorological forcing.
!> It converts the upper-zone step from hours to seconds and advances the ET
!> clock:
!>
!> \[
!> DTUZ=3600\,UZNEXT,\qquad TIMEUZ\leftarrow TIMEUZ+UZNEXT.
!> \]
!>
!> The loop covers land elements `NGDBGN:total_no_elements`; channel-link
!> elements precede `NGDBGN` and are handled separately by the simulation
!> driver. For an explicit bank element (`ICMREF(IEL,1)` equal to 1 or 2),
!> the associated link `IL=ICMREF(IEL,4)` supplies root access to channel-bed
!> water. The dimensionless weighting is
!>
!> \[
!> ALFA=\frac{0.5\,CWIDTH(IL)}{BWIDTH}.
!> \]
!>
!> With `ICE=NHBED(IL,ITYPE)+2`, cells `1:ICE-2` receive `ALFA`, cell
!> `ICE-1` receives `ALFA*FHBED(IL,ITYPE)`, and `ICE:top_cell_no` is reset to
!> zero. For ordinary land elements `ICE=1`, so the complete active
!> `UZALFA` range is zero.
!>
!> The routine then sets current surface-water depth
!> `HRUZ=GETHRF(IEL)-ZGRUND(IEL)`, copies the active VSS pressure-head range
!>
!> \[
!> PSI4(k)=VSPSI(k,IEL),\qquad
!> k=NLYRBT(IEL,1),\ldots,top\_cell\_no,
!> \]
!>
!> and invokes [[etin]] for the element. `UZALFA`, `PSI4`, and other scalar
!> ET work values are therefore scratch state for the element most recently
!> prepared, not independent per-element arrays.
!>
!> @warning
!> The driver assumes [[initialise_etmod]] has run and that `BWIDTH` is
!> nonzero. It also trusts the `ICMREF`/`NHBED` link mapping and
!> `NLYRBT(IEL,1):top_cell_no` bounds; no local allocation, bounds, or geometry
!> validation precedes the slice assignments and `DCOPY`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-08 to 1995-10-03 | GP | 4.0 | Created and completed the ET timestep controller. |
!> | 1997-05-16 | RAH | 4.1 | Swapped `VSPSI` indices and explicitly typed the routine. |
!> | 1998-11-03 | RAH | 4.2 | Removed redundant soil output and replaced loops with `ALINIT`/`DCOPY`. |
!> | 2026-04-05 | SvB | - | Replaced `ALINIT` with slices while retaining pressure-profile `DCOPY`. |
!> @endhistory
   SUBROUTINE ETSIM ()
      IMPLICIT NONE

      ! Locals, etc
      INTEGER :: ICE !! First cell above the bank/channel-bed root-access range, then active VSS base cell.
      INTEGER :: IEL !! Current land-element number.
      INTEGER :: IL !! Channel link associated with a bank element.
      INTEGER :: ITYPE !! Element type/bank side from `ICMREF(IEL,1)`.
      DOUBLE PRECISION :: ALFA !! Channel-width to grid-width root-access weighting.

      !----------------------------------------------------------------------*

      DTUZ = UZNEXT * 3600.0D0

      TIMEUZ = TIMEUZ + UZNEXT

      ! Loop over land-elements
      DO IEL = NGDBGN, total_no_elements
         ITYPE = ICMREF (IEL, 1)

         IF (ITYPE == 1 .OR. ITYPE == 2) THEN
            IL = ICMREF (IEL, 4)
            ALFA = 0.5D0 * CWIDTH (IL) / BWIDTH
            ICE = NHBED (IL, ITYPE) + 2

            ! Replaced ALINIT with array slice
            UZALFA (1 : ICE - 2) = ALFA
            UZALFA (ICE - 1) = ALFA * FHBED (IL, ITYPE)
         ELSE
            ICE = 1
         END IF

         ! Replaced ALINIT with array slice starting at index ICE
         IF (ICE <= top_cell_no) UZALFA (ICE : top_cell_no) = ZERO

         HRUZ = getHRF(IEL) - ZGRUND (IEL)
         ICE = NLYRBT (IEL, 1)

         CALL DCOPY (top_cell_no - ICE + 1, VSPSI (ICE, IEL), 1, PSI4 (ICE), 1)

         CALL ETIN (IEL)

      END DO

   END SUBROUTINE ETSIM

END MODULE ETmod
