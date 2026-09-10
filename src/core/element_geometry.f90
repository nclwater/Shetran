!> summary: Element counts, plan dimensions, areas, ground elevation and processing order.
!> author: GP; RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> The size and shape of the active problem, as opposed to the capacities in
!> [[array_limits]]. [[frame_geometry:FRIND]] establishes the element and link counts,
!> [[frame_geometry:FRDIM]] the plan dimensions and their product after overlap
!> corrections, and [[vs_input:VSIN]] the greatest active VSS cell index; frame and
!> OC setup fill `ZGRUND`.
!>
!> Active element, link and vertical-cell ranges are normally
!> `1:total_no_elements`, the link range established by `FRIND`, and the
!> per-element range `NLYRBT(element,1):top_cell_no`. The three counts start at
!> `-1` so that a consumer reading them before setup is visibly wrong rather
!> than plausibly zero. Module state is public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C, AL_D, sglobal; see docs/rename/proposal.md. |
!> @endhistory
MODULE element_geometry

   USE MOD_PARAMETERS, ONLY: I_P, R8P
   USE ARRAY_LIMITS, ONLY: NELEE, NXEE, NYEE

   IMPLICIT NONE

   INTEGER(KIND=I_P) :: total_no_elements = -1 !! Active total number of grid, bank, and channel-link elements.
   INTEGER(KIND=I_P) :: total_no_links = -1 !! Active number of channel links; link elements occupy the first indices.
   INTEGER(KIND=I_P) :: top_cell_no = -1 !! Greatest active VSS cell index across all element columns.
   REAL(KIND=R8P), DIMENSION(nelee) :: cellarea !! Plan area of each active element, `DXQQ*DYQQ` [m2].
   REAL(KIND=R8P), DIMENSION(nelee) :: DXQQ !! Corrected x-direction plan dimension of each active element [m].
   REAL(KIND=R8P), DIMENSION(nelee) :: DYQQ !! Corrected y-direction plan dimension of each active element [m].
   REAL(KIND=R8P), DIMENSION(nelee) :: ZGRUND !! Ground-surface elevation of each active element [m].
   INTEGER, DIMENSION(NELEE) :: NBFACE !! External boundary-face number by boundary element; zero otherwise.
   DOUBLEPRECISION, DIMENSION(NELEE, 4) :: DHF !! Distance from an element node/centroid to each face (m).
   INTEGER, DIMENSION(NELEE) :: ISORT    !! Element processing order, normally sorted from highest water level downward.
   INTEGER :: NXP1       !! Active grid helper value `NX+1`.
   INTEGER :: NYP1       !! Active grid helper value `NY+1`.
   INTEGER :: NXM1       !! Active grid helper value `NX-1`.
   INTEGER :: NYM1       !! Active grid helper value `NY-1`.
   INTEGER :: NXEP1      !! Capacity helper value `NXE+1`.
   INTEGER :: NYEP1      !! Capacity helper value `NYE+1`.
   DOUBLEPRECISION :: CAREA   !! Total active catchment plan area (m2).
   DOUBLEPRECISION :: BWIDTH  !! Nominal explicit-bank width used in frame geometry (m).
   DOUBLEPRECISION :: DXIN(NXEE)     !! Grid-centre spacing in the x direction; active entries are `1:NX-1` (m).
   DOUBLEPRECISION :: DYIN(NYEE)     !! Grid-centre spacing in the y direction; active entries are `1:NY-1` (m).

END MODULE element_geometry

