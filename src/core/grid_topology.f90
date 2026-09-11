!> summary: Active grid extent, element/coordinate lookup and face connectivity.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University
!>
!> How the model translates between grid coordinates, element numbers,
!> neighbouring faces and channel topology. [[frame_setup:INFR]] reads the active grid
!> dimensions and the catchment mask; [[frame_geometry:FRIND]] constructs `ICMREF`,
!> `ICMXY`, `ICMRF2` and `NGDBGN`. Flow, VSS, sediment, contaminant, input,
!> result and visualisation routines then all read them.
!>
!> `ICMREF(element,column)` has this layout. Internal hydrological faces use
!> the manual's east/north/west/south order, which differs from the
!> north/east/south/west ordering exposed in HDF5 visualisation output.
!>
!> | Column(s) | Meaning |
!> |:----------|:--------|
!> | 1 | Element type: 0 grid element, 1 or 2 explicit bank side, 3 channel link. |
!> | 2:3 | Grid x and y indices associated with the element. |
!> | 4 | Associated channel-link number for a link/bank; grid entries are not a general link lookup and may contain the legacy marker 9999. |
!> | 5:8 | Adjacent element references on faces 1 east, 2 north, 3 west, and 4 south. Zero denotes an external boundary; a negative value denotes an `ICMRF2` confluence record. |
!> | 9:12 | Reciprocal face number in the adjacent element for faces 1:4; an external boundary points back to the same face. |
!>
!> `NGDBGN` is `total_no_links+1`, so it is the first non-link element—not
!> necessarily the first grid element. When explicit banks are enabled, bank
!> elements occupy the range immediately following the links and grid elements
!> begin after both bank blocks. `ICMXY` is assigned only at active catchment
!> coordinates; callers use the catchment mask `INGRID` before reading entries
!> outside that domain. Module state has no declaration initialization. The
!> module is `PRIVATE` by default and exports every name it declares through an
!> explicit `PUBLIC` list, so the imported array limits are not re-exported.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C, AL_D, AL_G; see docs/rename/proposal.md. |
!> @endhistory
MODULE grid_topology

   USE ARRAY_LIMITS, ONLY: NELEE, NLFEE, NXEE, NYEE

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: ICMRF2, INGRID, NX, NY, NGDBGN, ICMREF, ICMXY

   INTEGER, DIMENSION(NLFEE, 6) :: ICMRF2 !! Multi-link branch map: adjacent elements in columns 1:3 and their faces in 4:6.
   INTEGER :: INGRID(NXEE,NYEE)   !! Catchment mask: zero inside the active catchment and -1 outside.
   INTEGER :: NX     !! Number of grid positions in the active x direction.
   INTEGER :: NY     !! Number of grid positions in the active y direction.
   INTEGER :: NGDBGN !! First non-link element number, always `total_no_links+1`.
   INTEGER :: ICMREF(NELEE,12) !! Element metadata, neighbours, and reciprocal-face mapping described above.
   INTEGER :: ICMXY(NXEE,NYEE) !! Active grid-coordinate to element-number lookup.

END MODULE grid_topology

