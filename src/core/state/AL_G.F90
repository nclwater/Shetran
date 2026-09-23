!> summary: Grid dimensions and element-topology lookup state.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University
!>
!> `AL_G` replaces the legacy `AL.G` common blocks. [[frmod:infr]] reads the
!> active grid dimensions and [[frmod:frind]] constructs `ICMREF`, `ICMXY`,
!> and `NGDBGN`. Flow, VSS, sediment, contaminant, input, result, and
!> visualisation routines then use these arrays to translate between grid
!> coordinates, element numbers, neighbouring faces, and channel topology.
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
!> coordinates; callers use the catchment mask before reading entries outside
!> that domain. Module state is public by default and has no declaration
!> initialization.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-03 | GP | 3.0 | Original version written. |
!> | 1994-10-01 | RAH | 3.4.1 | Declared all variables, removed `INTEGER*2`, tidied comments, and applied the standard header. |
!> | 1998-03-07 | RAH | 4.2 | Applied cosmetic updates. |
!> | 2004-07 | JE | - | Converted the grid state to Fortran 95. |
!> @endhistory
MODULE AL_G
   USE SGLOBAL, ONLY : NELEE, NXEE, NYEE
   IMPLICIT NONE

   INTEGER :: NX     !! Number of grid positions in the active x direction.
   INTEGER :: NY     !! Number of grid positions in the active y direction.
   INTEGER :: NGDBGN !! First non-link element number, always `total_no_links+1`.

   INTEGER :: ICMREF(NELEE,12) !! Element metadata, neighbours, and reciprocal-face mapping described above.
   INTEGER :: ICMXY(NXEE,NYEE) !! Active grid-coordinate to element-number lookup.

!PRIVATE :: NELEE, NXEE, NYEE
END MODULE AL_G
