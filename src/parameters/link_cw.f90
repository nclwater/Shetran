!> summary: Bed geometry and retained water state used by contaminant link preparation.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University
!>
!> `LINK_CW` replaces the legacy `LINK.CW` common blocks. It combines run-wide
!> bed-depth inputs, fixed per-link bed geometry, retained state from the last
!> contaminant step, and one pointer table for the link currently prepared by
!> [[cmmod:linkw]]. [[frmod:incm]] initializes the per-link arrays after
!> [[cmmod:cmrd]] reads the contaminant data; `LINKW` then advances the mutable
!> state before [[cmmod:linksm]] calculates retardation and solves each link.
!>
!> | State group | Initialization and update |
!> |:------------|:--------------------------|
!> | `DBS`, `DBDI` | `CMRD` reads manual records `CM15` and `CM17` once. |
!> | `ACPBSG`, `ACPBI` | `INCM` derives fixed scaled bed areas once for each link. |
!> | `ACPBDO`, `ACPSFO` | `INCM` seeds them; `LINKW` reads then overwrites the current link entry. |
!> | `THBEDO`, `THBED` | `INCM` seeds both; `LINKW` shifts current to old and recalculates current moisture. |
!> | `LENDA` | `LINKW` overwrites all six entries for each current link. |
!>
!> The four area arrays are dimensionless cross-sectional areas scaled by
!> `Z2**2`. `THBED` is the thickness-weighted volumetric moisture content of
!> the two bank regions within the stream bed, capped at bed porosity `PBSED`;
!> `THBEDO` retains its preceding value for [[cmmod:fret]]. `LENDA(slot)` maps
!> each of the six adjacent-link slots to end 1 or 2 of that adjacent link when
!> `LINKW` reads `QLINK` and `QDEFF` from [[sed_cs]].
!>
!> `ACPSFO` is not an immutable previous-step snapshot throughout the ordered
!> catchment sweep. `LINKW` overwrites the current link entry before reading
!> adjacent entries, so an adjacent-link value is current if that link has
!> already been visited and previous otherwise. Boundary slots use the newly
!> stored current-link value. This existing in-place update is documented
!> without changing it.
!>
!> The unrestricted `USE LINK_CC1` also makes `KS`, `KSPBK`, and the
!> use-associated `LLEE` accessible through this module. Together with
!> use-associated `NLFEE`, they and the nine variables declared here are public
!> because the `PRIVATE` statement remains commented out. The nine declared
!> variables have no declaration initializers.
!>
!> @warning
!> The `THBED` recalculation in current `LINKW` uses the bank-2 deep-bed cell
!> boundary for both bank sides. If the two `NCEBD` indices differ, its bank-1
!> weighting can omit cells or read an unpopulated thickness; see [[link_cc1]]
!> for the indexing details. This documentation transfer does not alter it.
!> @endwarning
!>
!> @note
!> Manual record `CM17` requires `DBDI > DBS` and says that `DBDI` must not
!> equal `2*DBI`. No `DBI` name or definition was found elsewhere in the
!> current source or manual, and `CMRD` does not validate either statement.
!> The unexplained `DBI` wording is therefore retained rather than silently
!> interpreted as another variable.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-20 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1991-06-16 | JE | 3.1 | Added `ACPBSG`, `ACPSFO`, and now-absent `OLDB`/`QSTRM`; renamed `DBDM` as `DBDI`. |
!> | 1991-06-18 | JE | 3.1 | Included `LINK.CC1`. |
!> | 1991-08-26 | JE | 3.1 | Removed block `OLOL`. |
!> | 1993-02-08 | GP | 3.4 | Moved `QLINK` and `QDEFF` to `SED_CS`. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> | 1998-03-08 | RAH | 4.2 | Amended comments. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE LINK_CW
   USE SGLOBAL, ONLY : NLFEE
   USE LINK_CC1
   IMPLICIT NONE

   DOUBLEPRECISION :: ACPBDO(NLFEE) !! Last stored scaled bed/deposited-material area for each link.
   DOUBLEPRECISION :: ACPBSG(NLFEE) !! Fixed scaled bed-surface area, `DBS*CWIDTH/Z2SQ`.
   DOUBLEPRECISION :: ACPBI(NLFEE)  !! Baseline scaled bed/deposited-material area before `ARBDEP` is added.
   DOUBLEPRECISION :: ACPSFO(NLFEE) !! Last stored scaled stream-water area for each link.

   DOUBLEPRECISION :: DBS  !! Depth from the river bed to the base of the surface-bed layer (m), from `CM15`.
   DOUBLEPRECISION :: DBDI !! Initial depth from the river bed to the base of the deep-bed layer (m), from `CM17`.

   INTEGER :: LENDA(6) !! End number on the adjacent link represented by each current-link connection slot.

   DOUBLEPRECISION :: THBED(NLFEE)  !! Current thickness-weighted stream-bed volumetric moisture content.
   DOUBLEPRECISION :: THBEDO(NLFEE) !! Preceding stream-bed moisture content passed to `FRET`.
!PRIVATE :: NLFEE
END MODULE LINK_CW
