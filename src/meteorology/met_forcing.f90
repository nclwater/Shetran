!> summary: The current meteorological forcing values and the station mapping.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> What the weather is doing now, as the model sees it: air temperature, wind
!> speed, vapour-pressure deficit, net radiation and potential evaporation per
!> meteorological site, precipitation mapped to each element, and the
!> per-element maps `NMC` and `NRAINC` that say which site and which rainfall
!> station an element belongs to. `DTMET`, `DTMET2` and `DTMET3` are the input
!> intervals of the combined, precipitation and potential-evaporation series.
!>
!> [[met_input:METIN]] writes all of it; the evapotranspiration, snowmelt and
!> water-balance calculations read it. The values and the reader were in
!> different modules before this split — the reader in `rest`, the values in
!> `AL_D` — and are now next to each other. Module state is public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP / RAH | 2.0-4.2 | Developed the frame driver, the meteorological reader and the timestep control. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the remaining frame `.F` files to Fortran 90. |
!> | 2015-2026 | SB / SvB | 4.5-4.6 | Added the separate temperature streams, the dated meteorological reader and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of AL_D; see docs/rename/proposal.md. |
!> @endhistory
MODULE met_forcing

   USE array_limits, ONLY: nelee, NVEE

   IMPLICIT NONE

   INTEGER :: NM         !! Number of active meteorological sites.
   INTEGER :: NRAIN      !! Number of active rainfall stations.
   DOUBLEPRECISION :: DTMET   !! Combined meteorological input interval (h).
   DOUBLEPRECISION :: DTMET2  !! Separate precipitation input interval (h).
   DOUBLEPRECISION :: DTMET3  !! Separate potential-evaporation input interval (h).
   LOGICAL :: ISTA       !! Whether separate maximum/minimum air-temperature streams are available.
   INTEGER :: NMC(NELEE)          !! Meteorological-site category by element.
   INTEGER :: NRAINC(NELEE)       !! Rainfall-station category by element.
   DOUBLEPRECISION :: precip_m_per_s(NELEE) !! Precipitation rate mapped directly to each element (m/s).
   DOUBLEPRECISION :: OBSPE(NVEE)   !! Potential-evaporation forcing by meteorological site (mm/s).
   DOUBLEPRECISION :: TA(NVEE)      !! Air temperature by meteorological site (degrees C).
   DOUBLEPRECISION :: U(NVEE)       !! Wind speed by meteorological site (m/s).
   DOUBLEPRECISION :: VPD(NVEE)     !! Vapour-pressure deficit by meteorological site (mb).
   DOUBLEPRECISION :: RN(NVEE)      !! Net radiation by meteorological site (W/m2).

END MODULE met_forcing

