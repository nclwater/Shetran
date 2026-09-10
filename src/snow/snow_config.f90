!> summary: Snowmelt parameters, the initial snowpack settings and the heat-flux components.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> What the snowmelt calculation is configured with, read from records
!> `SM4`--`SM14` by [[snow_input:INSM]]: the degree-day factor, the default
!> snow specific gravity, the roughness and reference heights for the
!> energy-budget method, the initial-snowpack mode, and the per-vegetation
!> meteorological station map.
!>
!> The four `HF*` values and `USM`/`ESM`/`TOPNET`/`PNSNOW` are per-element
!> intermediates of one melt calculation rather than configuration; they are
!> module variables because [[snowmelt:SM]] and [[snowmelt:SMET]] share them,
!> and they mean nothing between elements.
!>
!> | Name | Meaning | Unit |
!> |:-----|:--------|:-----|
!> | `HFC` | Atmospheric-convection heat flux over the timestep. | J/m^2 |
!> | `HFE` | Heat from evaporation/condensation phase change. | J/m^2 |
!> | `HFR` | Heat from rainfall or snowfall. | J/m^2 |
!> | `HFT` | Total heat flux from air, ground and radiation to snow. | J/m^2 |
!>
!> Keeping the parameters here rather than with the reader is what stops
!> [[snow_input]] and [[snowmelt]] depending on each other; the reasoning is
!> the same as for [[et_config]]. Module state is public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1990-06 to 1998-03 | GP / RAH | 2.2--4.2 | Developed the snowmelt component: variable snowpack, `PNSNOW`, and explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90 and replaced the `SM.F` files. |
!> | 2026-04-03 to 2026-04-13 | SvB | 4.6.1 | Modernisation pass: removed the `1H0` Hollerith descriptor and the `GOTO`-driven control flow, added `IMPLICIT NONE`/`INTENT`, and pre-computed the repeated temperature-ratio subexpression. |
!> | 2026-09-10 | SvB | - | Split out of SMmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE snow_config

   USE array_limits, ONLY: NVEE

   IMPLICIT NONE

   DOUBLEPRECISION :: USM    !! Snowmelt during the current timestep (mm snow).
   DOUBLEPRECISION :: DDF    !! Degree-day melt factor (mm/s/C).
   DOUBLEPRECISION :: RHOS   !! Active snow specific gravity for the current element.
   DOUBLEPRECISION :: ESM    !! Snow depth lost to evaporation or sublimation (mm snow).
   DOUBLEPRECISION :: HFC    !! Atmospheric-convection heat flux over the timestep (J/m^2).
   DOUBLEPRECISION :: HFR    !! Heat supplied by rainfall or snowfall over the timestep (J/m^2).
   DOUBLEPRECISION :: HFE    !! Latent heat term for evaporation or condensation over the timestep (J/m^2).
   DOUBLEPRECISION :: HFT    !! Net heat flux available to the snowpack over the timestep (J/m^2).
   DOUBLEPRECISION :: ZUS    !! Anemometer height above ground for energy-budget snowmelt (m).
   DOUBLEPRECISION :: ZDS    !! Zero-plane displacement height for snow aerodynamic exchange (m).
   DOUBLEPRECISION :: ZOS    !! Snow-surface roughness height for aerodynamic exchange (m).
   DOUBLEPRECISION :: RHODEF !! Default snow specific gravity used when spatial `RHOSAR` is zero.
   DOUBLEPRECISION :: TOPNET !! Water input to the snowpack before routing (mm water).
   DOUBLEPRECISION :: PNSNOW !! Water depth passed into or released from the snowpack in the current step (mm water).
   LOGICAL         :: BINSMP !! Snow-input echo-print flag.
   INTEGER         :: IMET(NVEE) !! Meteorological-station element index for each vegetation type in energy-budget mode.
   INTEGER         :: NSD         !! Initial snowpack mode: uniform (`0`) or spatial (`1`).
   DOUBLEPRECISION :: HEAD(20)   !! Snow input title/header workspace retained for legacy I/O.

END MODULE snow_config

