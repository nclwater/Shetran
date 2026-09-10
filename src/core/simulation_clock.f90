!> summary: Simulation time, timestep number and the coupled ET/VSS step lengths.
!> author: GP; RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> Where the simulation has got to. `TIH` and `TTH` bound the run as absolute
!> hour counts from `HOUR_FROM_DATE`; `UZNOW`, `TIMEUZ` and `NSTEP` advance
!> through it, and `DTUZ`, `UZNEXT` and `UZVAL` carry the current coupled
!> ET/VSS step. Nearly every component reads at least one of these, and the
!> timestep controller writes them, so they are gathered here rather than in any
!> one component.
!>
!> Absolute times are hour counts on the same origin as `TIH`. `DTUZ` is the one
!> value in seconds; the rest are in hours. `UZNOW` starts at zero and is
!> replaced by the stored time on a hot start.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C, AL_D, sglobal; see docs/rename/proposal.md. |
!> @endhistory
MODULE simulation_clock

   USE MOD_PARAMETERS, ONLY: R8P

   IMPLICIT NONE

   REAL(KIND=R8P) :: UZNOW !! Current simulation time measured from the configured start [h].
   DOUBLEPRECISION TIH !! Simulation start as an absolute hour count returned by `HOUR_FROM_DATE` (h).
   DOUBLEPRECISION DTUZ   !! Current coupled VSS/ET timestep in seconds (s).
   DOUBLEPRECISION UZNEXT !! Current model timestep expressed in hours (h).
   INTEGER :: NSTEP  !! Current coupled simulation timestep number.
   DOUBLEPRECISION :: TTH     !! Simulation end as an absolute hour count (h).
   DOUBLEPRECISION :: UZVAL  !! Next upper-zone/VSS solution time used in hotstart handling (h).
   DOUBLEPRECISION :: TIMEUZ !! Current elapsed ET/snow/VSS model time (h).

END MODULE simulation_clock

