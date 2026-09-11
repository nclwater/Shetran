!> summary: The time-varying lateral, base and well boundary series.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> The current, previous and next values of every time-varying subsurface
!> boundary: the pumped well rates and the lateral flow, head and
!> head-gradient series, and the aquifer-base flow and head series.
!> [[vs_driver:VSPREP]] advances them through
!> [[timeseries_input:FINPUT]]/[[timeseries_input:HINPUT]] and
!> [[vs_driver:VSSIM]] reads them.
!>
!> The `*LST`/`*TIM` pairs hold the last value read and the time it applies
!> from; `*PRV` and `*NXT` bracket the current model time. The category
!> definitions these series belong to are in [[vs_config]]. Module state is
!> public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995--1998 | GP / RAH | 4.0--4.2 | Created the VSS component and its `.INC` include groups. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS Fortran sources into a single Fortran 90 module. |
!> | 2026-03 to 2026-05 | SB / SvB | 4.6 | Modernisation pass, and moved `VSREAD`'s read buffers to allocatable module state to avoid a stack-related crash. |
!> | 2026-09-10 | SvB | - | Split out of VSmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_boundaries

   USE MOD_PARAMETERS, ONLY: zero
   USE array_limits, ONLY: NVSEE

   IMPLICIT NONE

   DOUBLEPRECISION :: WLLAST = zero        !! Previous well-input record time.
   DOUBLEPRECISION :: WLTIME = zero        !! Current/next well-input record time.
   DOUBLEPRECISION :: RWELIN(NVSEE) = zero !! Current well abstraction input values.
   DOUBLEPRECISION :: RLFLST = zero        !! Previous lateral-flow boundary record time.
   DOUBLEPRECISION :: RLFTIM = zero        !! Current/next lateral-flow boundary record time.
   DOUBLEPRECISION :: RLFPRV(NVSEE) = zero !! Previous lateral-flow boundary values.
   DOUBLEPRECISION :: RLHLST = zero        !! Previous lateral-head boundary record time.
   DOUBLEPRECISION :: RLHTIM = zero        !! Current/next lateral-head boundary record time.
   DOUBLEPRECISION :: RLHPRV(NVSEE) = zero !! Previous lateral-head boundary values.
   DOUBLEPRECISION :: RLHNXT(NVSEE) = zero !! Next lateral-head boundary values.
   DOUBLEPRECISION :: RLGLST = zero        !! Previous lateral-gradient boundary record time.
   DOUBLEPRECISION :: RLGTIM = zero        !! Current/next lateral-gradient boundary record time.
   DOUBLEPRECISION :: RLGPRV(NVSEE) = zero !! Previous lateral-gradient boundary values.
   DOUBLEPRECISION :: RLGNXT(NVSEE) = zero !! Next lateral-gradient boundary values.
   DOUBLEPRECISION :: RBFLST = zero        !! Previous base-flow boundary record time.
   DOUBLEPRECISION :: RBFTIM = zero        !! Current/next base-flow boundary record time.
   DOUBLEPRECISION :: RBFPRV(NVSEE) = zero !! Previous base-flow boundary values.
   DOUBLEPRECISION :: RBHLST = zero        !! Previous base-head boundary record time.
   DOUBLEPRECISION :: RBHTIM = zero        !! Current/next base-head boundary record time.
   DOUBLEPRECISION :: RBHPRV(NVSEE) = zero !! Previous base-head boundary values.
   DOUBLEPRECISION :: RBHNXT(NVSEE) = zero !! Next base-head boundary values.
   DOUBLEPRECISION :: RLFDUM(NVSEE) = zero !! Lateral-flow interpolation workspace.
   DOUBLEPRECISION :: RLHDUM(NVSEE) = zero !! Lateral-head interpolation workspace.
   DOUBLEPRECISION :: RLGDUM(NVSEE) = zero !! Lateral-gradient interpolation workspace.
   LOGICAL :: FIRSTvssim = .TRUE.          !! True until `VSSIM` has cached column metadata.

END MODULE vs_boundaries

