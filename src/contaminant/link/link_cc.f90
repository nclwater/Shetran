!> summary: Shared three-compartment workspace for contaminant transport through one stream link.
!> author: JE, Newcastle University
!>
!> `LINK_CC` replaces the legacy `LINK.CC` common blocks. For each link,
!> [[cmmod:linkw]] prepares scaled areas, wet/dry state, bank geometry, and
!> signed water-exchange numbers. [[cmmod:linksm]] then loads concentrations,
!> storage/retardation factors, and source terms for one contaminant at a time
!> before [[cmmod:link]] solves the coupled stream-water (`SF`), bed-surface
!> (`BS`), and bed/deep-material (`BD`) equations.
!>
!> | State group | Producer and lifetime |
!> |:------------|:----------------------|
!> | Areas, erosion, `NCEBK`, Peclet numbers, `USCP` | `LINKW`, once for the current link and timestep. |
!> | Concentrations, retardation, inputs, sinks, transfers | `LINKSM`, once per contaminant before `LINK`. |
!> | `*1Q` and `GCPLAQ` | Updated after each solve and used as parent state by the next numeric contaminant. |
!>
!> This is shared workspace for the link most recently prepared, not
!> persistent storage indexed by link. Areas are scaled by `Z2**2`; Peclet-like
!> exchange numbers are dimensionless and positive into the current link.
!> Two-entry arrays represent the two bank sides. In six-entry arrays, slots
!> 1:3 describe connections at link end 1 and slots 4:6 those at end 2. Cell
!> arrays use the fixed `LLEE` capacity and the active exposed-bank range
!> `NCEBK(side):NCETOP`.
!>
!> Suffix `C` denotes a concentration derivative and suffix `T` a scaled-time
!> derivative. `USCP` is a numeric zero/one wetness indicator rather than a
!> logical. `ECP*` names are intended compartment sink terms, but current
!> `LINKSM` resets all nine to zero for every contaminant, so link plant uptake
!> is inactive. The module has no active `PRIVATE` statement; its 63 variables
!> and the imported capacity `LLEE` are public. The 63 workspace variables have
!> no declaration initializers.
!>
!> @warning
!> `LINKSM` passes `ISADNL` from [[is_cc]] to every link retardation
!> calculation, but current [[cmmod:cmrd]] reads manual record `CM13` into a
!> shadowing local variable. The module flag used here therefore remains
!> undefined under standard Fortran, so the linear/nonlinear adsorption path
!> for link calculations is not reliably selected.
!>
!> When updating `FCPSW1(side)`, `LINKSM` compares the current bank top-cell
!> concentration with `CCPBK(side,NCONT)`. The second `CCPBK` subscript is a
!> cell index, so this selects cell `1:NCON` according to contaminant number,
!> not the bank top cell used by the current value. This documentation transfer
!> records but does not alter that indexing.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-20 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1991-07-16 | JE | 3.1 | Removed the `AL.P` include and `NCETOP` definition; replaced `FCPBK1`/`GCPBK1` with `FCPBK`/`GCPBK`. |
!> | 1991-07-16 | JE | 3.1 | Renamed legacy common block `CONC` as `CONCL` to avoid a name clash. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE LINK_CC
   USE SGLOBAL, ONLY : LLEE
   IMPLICIT NONE

   DOUBLEPRECISION :: ACPBD1  !! Current scaled bed/deposited-material area, `ACPBI+ARBDEP/Z2SQ`.
   DOUBLEPRECISION :: ACPBDT  !! Scaled-time change in `ACPBD1`; zero for a dry link.
   DOUBLEPRECISION :: ACPBS   !! Scaled bed-surface-layer area copied from `ACPBSG`.
   DOUBLEPRECISION :: ACSFA1(6) !! Scaled stream-water areas for the six adjacent-link/end slots.
   DOUBLEPRECISION :: ACPSF1  !! Current scaled stream-water area, `ARXL/Z2SQ`.
   DOUBLEPRECISION :: ACPSFT  !! Scaled-time change in stream-water area; zero for a dry link.

   DOUBLEPRECISION :: CCBD1Q       !! Updated deep-bed concentration of the preceding numeric contaminant.
   DOUBLEPRECISION :: CCPBK(2,LLEE) !! Previous dynamic-region concentrations in the two adjacent banks.
   DOUBLEPRECISION :: CCPGS1(2)    !! Current ground-surface-cell concentration in each adjacent bank.
   DOUBLEPRECISION :: CCBS1Q       !! Updated bed-surface concentration of the preceding numeric contaminant.
   DOUBLEPRECISION :: CCSFA1(6)    !! Current stream-water concentrations for adjacent-link/end slots.
   DOUBLEPRECISION :: CCSF1Q       !! Updated stream-water concentration of the preceding numeric contaminant.
   DOUBLEPRECISION :: SCPBK(2,LLEE) !! Previous dead-space concentrations in the two adjacent banks.

   DOUBLEPRECISION :: WCPBD1 !! Signed scaled coefficient for change in bed/deposited area.
   DOUBLEPRECISION :: VCPBK1 !! Scaled bank-erosion rate, `Z2OD*GNUBK`.

   DOUBLEPRECISION :: ICP1   !! Scaled dry-deposition input with the link-equation sign convention.
   DOUBLEPRECISION :: ICPSBD !! Deep-bed contaminant source from infiltrating sediment.
   DOUBLEPRECISION :: ICSBDC !! Concentration derivative of `ICPSBD`.
   DOUBLEPRECISION :: ICSBDT !! Scaled-time derivative of `ICPSBD`.
   DOUBLEPRECISION :: ICPSBS !! Bed-surface contaminant source from infiltrating sediment.
   DOUBLEPRECISION :: ICSBSC !! Concentration derivative of `ICPSBS`.
   DOUBLEPRECISION :: ICSBST !! Scaled-time derivative of `ICPSBS`.
   DOUBLEPRECISION :: QCP1   !! Scaled rainfall and irrigation/well contaminant input.

   DOUBLEPRECISION :: FCPBD        !! Deep-bed storage/retardation factor.
   DOUBLEPRECISION :: FCPBDC       !! Concentration derivative of `FCPBD`.
   DOUBLEPRECISION :: FCBD1Q       !! Updated `FCPBD` for the preceding numeric contaminant.
   DOUBLEPRECISION :: FCPBDT       !! Scaled-time derivative of `FCPBD`.
   DOUBLEPRECISION :: FCPBK(2,LLEE) !! Dynamic-region storage factors in adjacent bank cells.
   DOUBLEPRECISION :: FCPBS        !! Bed-surface storage/retardation factor.
   DOUBLEPRECISION :: FCPBSC       !! Concentration derivative of `FCPBS`.
   DOUBLEPRECISION :: FCBS1Q       !! Updated `FCPBS` for the preceding numeric contaminant.
   DOUBLEPRECISION :: FCPBST       !! Scaled-time derivative of `FCPBS`.
   DOUBLEPRECISION :: FCPSW1(2)    !! Updated surface-water storage factor for each adjacent bank.
   DOUBLEPRECISION :: FCPSD        !! Storage factor for newly deposited sediment at stream concentration.
   DOUBLEPRECISION :: FCPSDC       !! Concentration derivative of `FCPSD`.
   DOUBLEPRECISION :: FCPSDT       !! Scaled-time derivative of `FCPSD`.
   DOUBLEPRECISION :: FCPSF        !! Stream-water/mobile-sediment storage factor.
   DOUBLEPRECISION :: FCSFA1(6)    !! Updated stream-water storage factors for adjacent-link/end slots.
   DOUBLEPRECISION :: FCPSFC       !! Concentration derivative of `FCPSF`.
   DOUBLEPRECISION :: FCSF1Q       !! Updated `FCPSF` for the preceding numeric contaminant.
   DOUBLEPRECISION :: FCPSFT       !! Scaled-time derivative of `FCPSF`.
   DOUBLEPRECISION :: GCPBK(2,LLEE) !! Dead-space storage factors in adjacent bank cells.

   INTEGER :: NCEBK(2) !! First exposed bank-cell index on each side of the link.

   DOUBLEPRECISION :: PCPBK1(2,LLEE) !! Signed bank-cell exchange numbers; positive flow enters the link.
   DOUBLEPRECISION :: PCPSB1(2)      !! Signed bank-bed exchange numbers; positive flow enters the link.
   DOUBLEPRECISION :: PCSFA1(6)      !! Signed exchange numbers for adjacent-link/end slots.
   DOUBLEPRECISION :: PCPSW1(2)      !! Signed bank surface-water exchange numbers.
   DOUBLEPRECISION :: PCSFM1         !! Signed stream-water exchange number at link end 1.
   DOUBLEPRECISION :: PCSFP1         !! Signed stream-water exchange number at link end 2.

   DOUBLEPRECISION :: ECPBD  !! Intended deep-bed sink; currently set to zero by `LINKSM`.
   DOUBLEPRECISION :: ECPBDC !! Intended concentration derivative of `ECPBD`; currently zero.
   DOUBLEPRECISION :: ECPBDT !! Intended scaled-time derivative of `ECPBD`; currently zero.
   DOUBLEPRECISION :: ECPBS  !! Intended bed-surface sink; currently set to zero by `LINKSM`.
   DOUBLEPRECISION :: ECPBSC !! Intended concentration derivative of `ECPBS`; currently zero.
   DOUBLEPRECISION :: ECPBST !! Intended scaled-time derivative of `ECPBS`; currently zero.
   DOUBLEPRECISION :: ECPSF  !! Intended stream-water sink; currently set to zero by `LINKSM`.
   DOUBLEPRECISION :: ECPSFC !! Intended concentration derivative of `ECPSF`; currently zero.
   DOUBLEPRECISION :: ECPSFT !! Intended scaled-time derivative of `ECPSF`; currently zero.

   DOUBLEPRECISION :: USCP !! Numeric wetness indicator: one for stream water, zero for a dry link.

   DOUBLEPRECISION :: ACSBD1 !! Scaled transfer coefficient between deep bed and bed surface.
   DOUBLEPRECISION :: ACSBS1 !! Scaled transfer coefficient between bed surface and stream water.
   DOUBLEPRECISION :: GCPLAL !! Decay/generation coefficient for the current contaminant.
   DOUBLEPRECISION :: GCPLAQ !! Decay/generation coefficient of the preceding numeric contaminant.
!PRIVATE :: LLEE
END MODULE LINK_CC
