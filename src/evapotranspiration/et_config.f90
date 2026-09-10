!> summary: Evapotranspiration input parameters and the time-varying vegetation tables.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> Everything the ET calculation is configured with: the meteorological
!> selectors, the per-vegetation resistances and canopy-capacity parameters,
!> the interpolation mode switches, and the breakpoint tables for canopy
!> storage, leaf-area index, ground cover and vegetation height.
!> [[et_input:INET]] reads records `ET2`--`ET18` into it and
!> [[et_process]] reads it every timestep.
!>
!> This module exists to keep those two apart. `INET` writes these 41 values
!> and `et_process` reads them; if they stayed with the reader, the reader and
!> the process module would each need the other. Everything here is written
!> once at setup, apart from the breakpoint cursors and the current
!> interpolated values.
!>
!> | State group | Producer or updater | Principal consumer |
!> |:------------|:--------------------|:-------------------|
!> | `BMET*`, `MEASPE`, vegetation controls and lookup tables | [[et_input:INET]] | [[met_input:METIN]], [[et_process:ET]] |
!> | `REL*`, `TIM*`, `NCT*`, and `*1` reference values | [[et_input:INET]] and [[met_input:METIN]] | [[interpolation:TERPO1]] |
!> | `DEL` | [[met_input:METIN]] | [[et_process:ET]] |
!> | `PSI4`, `UZALFA` | [[et_process:ETSIM]] | [[et_process:ET]] and exported AD state |
!>
!> `NCTCST`, `NCTPLA`, `NCTCLA` and `NCTVHT` are current lower-breakpoint
!> cursors. [[et_input:INET]] initializes each enabled cursor to one; the number
!> of rows read from the corresponding manual `ET12` record remains local to
!> `INET`. [[interpolation:TERPO1]] advances the cursor while interpolating a
!> ratio from `REL*`/`TIM*`, then multiplies it by the fixed `*1` reference
!> value. Module state is public by default.
!>
!> @note
!> Manual `ET2` allows a fourth, optional `BMETDATES` value. Current
!> [[et_input:INET]] first attempts `(4L7)` and falls back to the legacy three
!> logical values, defaulting `BMETDATES` false. The flag applies to separate
!> precipitation, potential-evaporation and temperature series handled by
!> [[met_input:METIN]].
!>
!> `msg` is a shared diagnostic buffer rather than a parameter. It was private
!> to `ETmod`; it is public here because [[et_process]] writes into it.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-02 to 1998-10 | GP / RAH | 2.0--4.2 | Developed and reorganised the combined ET component. |
!> | 2008-12 | JE | 4.3.5F90 | Combined the former ET Fortran sources into a single Fortran 90 module. |
!> | 2026-03-19 | SB | 4.6 | Added date-aware meteorological input and the run-sized allocator. |
!> | 2026-04-05 to 2026-04-14 | SvB | - | Removed `ALINIT`/GOTOs and added resistance error 4998. |
!> | 2026-05-03 | SvB | - | Resized `DEL` and explicitly initialized `IUNDEF`. |
!> | 2026-09-10 | SvB | - | Split out of ETmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE et_config

   USE array_limits, ONLY: NVEE

   IMPLICIT NONE

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
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: PS1 !! Manual `ET16` soil-moisture-tension table (m).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: FET !! Manual `ET16` actual/potential ET ratio table.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RCF !! Manual `ET16` canopy-resistance table (s/m).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RELCST !! Relative canopy-storage values by vegetation and breakpoint.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: TIMCST !! Canopy-storage breakpoint times (days).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RELPLA !! Relative ground-cover values by vegetation and breakpoint.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: TIMPLA !! Ground-cover breakpoint times (days).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RELCLA !! Relative canopy-LAI values by vegetation and breakpoint.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: TIMCLA !! Canopy-LAI breakpoint times (days).
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: RELVHT !! Relative vegetation-height values by vegetation and breakpoint.
   DOUBLEPRECISION, DIMENSION(:, :), ALLOCATABLE :: TIMVHT !! Vegetation-height breakpoint times (days).
   CHARACTER(132) :: msg !! Shared private warning/fatal diagnostic buffer.

END MODULE et_config

