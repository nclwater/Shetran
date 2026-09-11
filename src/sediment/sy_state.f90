!> summary: Sediment yield state: the size-fraction stores, fluxes and balance errors.
!> author: GP, Newcastle University; RJL, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> The state of the sediment transport solution, held per element, per link and
!> per particle-size fraction: the loose hillslope and channel-bed stores, the
!> composition fractions, the erosion and infiltration rates, the solid-sediment
!> face discharges, and `SBERR`, the accumulated balance error the driver
!> reports at the end of a run.
!>
!> [[simulation_driver:SIMULATION]] passes the principal state arrays to
!> [[sy_driver:SYMAIN]], which reads, validates and advances them.
!> [[cm_input:INCM]] supplies a three-size-class fallback when sediment
!> transport is disabled, and [[cm_driver:CMSIM]] uses the state to couple
!> particulate transport and channel-bed changes to contaminant transport.
!> Frame output and [[visualisation_interface_left]] expose selected results.
!>
!> Array bounds are the compile-time capacities from [[array_limits]]; only the
!> active link (`1:NLF`), element (`1:NEL`), soil (`1:NS`) and sediment-class
!> (`1:NSED`) slices contain model state. Link-end indices are `1:2`, while the
!> face index of `QSED` is `1:4`. Module state is public by default and has no
!> declaration initialization.
!>
!> @warning
!> No complete sediment-enabled producer was found for `FBTSD`, `NSOBED` or
!> `QLINK`. `INCM`/`CMSIM` establish them only on the no-sediment fallback
!> path, apart from `INCM` changing a zero `NSOBED` value to one immediately
!> before one use. `QDEFF` is likewise zeroed only on the fallback path,
!> although every current contaminant calculation multiplies it by a local
!> zero. This module itself supplies no defaults.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 to 2004-11 | JE / GP / RAH / SB | 3.0--4.27 | Created `SED.CS`, moved `QLINK`/`QDEFF` in from `LINK.CW`, added `QSED`, `DCBED` and `DCBSED`, and converted to Fortran 95. |
!> | 2026-09-10 | SvB | - | Split out of AL_C (`SBERR`) and SED_CS (the remainder); see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_state

   USE array_limits, ONLY: nelee, nlfee, NSEDEE, NSEE

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SBERR, ARBDEP, DLS, GINFD, GINFS, GNU, GNUBK, DCBED, DCBSED, FDEL, FBETA, FBTSD, PBSED, &
             PLS, SOSDFN, SOFN, NSOBED, NSED, QLINK, QDEFF, QSED

   DOUBLEPRECISION, DIMENSION(NELEE, NSEDEE) :: SBERR !! Sediment balance-error state by element and size fraction.


! Sediment stores, composition fractions and discharges (from SED_CS).
   DOUBLEPRECISION ARBDEP(NLFEE)        !! Deposited-sediment cross-sectional area by link [m2].
   DOUBLEPRECISION DLS(NELEE)            !! Loose/bed sediment depth by active element [m].
   DOUBLEPRECISION GINFD(NLFEE,NSEDEE)   !! Sediment-volume infiltration rate used for the deep-bed term [m3/s].
   DOUBLEPRECISION GINFS(NLFEE,NSEDEE)   !! Sediment-volume infiltration rate used for the bed-surface term [m3/s].
   DOUBLEPRECISION GNU(NELEE)            !! Ground-surface erosion depth rate by column element [m/s].
   DOUBLEPRECISION GNUBK(NLFEE)          !! Lateral bank-erosion depth rate by link [m/s].
   DOUBLEPRECISION DCBED(NLFEE)          !! Total thickness of the active upper channel-bed layer by link [m].
   DOUBLEPRECISION DCBSED(NLFEE,NSEDEE)  !! Thickness contribution of each size class in the active upper bed layer [m].
   DOUBLEPRECISION FDEL(NELEE,NSEDEE) !! Mobile-sediment settled-depth/water-depth ratio by element and size class.
   DOUBLEPRECISION FBETA(NELEE,NSEDEE)  !! Loose/bed sediment composition fraction by element and size class.
   DOUBLEPRECISION FBTSD(NLFEE,NSEDEE)  !! Newly deposited sediment composition fraction by link and size class.
   DOUBLEPRECISION PBSED(NLFEE)        !! Channel-bed sediment porosity by link.
   DOUBLEPRECISION PLS(NELEE)          !! Loose-sediment porosity by column element.
   DOUBLEPRECISION SOSDFN(NSEE,NSEDEE) !! Un-eroded-soil mass fraction by soil type and sediment size class.
   DOUBLEPRECISION SOFN(NSEE,NSEDEE)   !! Three-class fallback soil fraction read from contaminant input.
   INTEGER NSOBED(NLFEE) !! Parent-material soil-type index at the stream bed by link.
   INTEGER NSED !! Number of active sediment size classes.
   DOUBLEPRECISION QLINK(NLFEE,2)       !! Water discharge at the two link ends [m3/s].
   DOUBLEPRECISION QDEFF(NLFEE,2)       !! Effective-flow correction at the two link ends [m3/s].
   DOUBLEPRECISION QSED(NELEE,NSEDEE,4) !! Solid-sediment volume discharge by element, size class, and face [m3/s].

END MODULE sy_state

