!> summary: Nondimensional link length and bank-cell thickness workspace for contaminant transport.
!> author: JE, Newcastle University
!>
!> `LINK_CC1` replaces the legacy `LINK.CC1` common block. The two values are
!> shared workspace for the stream link most recently prepared by
!> [[cmmod:linkw]], not arrays retaining data for every link. [[cmmod:linksm]]
!> uses `KS` to scale rainfall and irrigation/well input, and [[cmmod:link]]
!> uses both values in the three-compartment link equations.
!>
!> | Variable | Assignment in `LINKW` | Current consumers |
!> |:---------|:----------------------|:------------------|
!> | `KS` | `CLENTH(NLINK)/Z2`, once for the current link. | Link-length scaling in `LINKW`, `LINKSM`, and `LINK`. |
!> | `KSPBK` | `DELTAZ/Z2` for `NCEBD+1:NCETOP`. | Bed weighting (`LINKW`); bank mass (`LINK`). |
!>
!> Both quantities are dimensionless because physical lengths are normalized
!> by the contaminant reference length `Z2`. The first `KSPBK` subscript
!> selects one of the two banks; the second uses the fixed `LLEE` cell capacity.
!> Entries below `NCEBD(side)+1` are not populated for the current link. The
!> active exposed-bank range later summed by `LINK`, `NHBED(side)+1:NCETOP`, is
!> within the populated range.
!>
!> [[link_cw]] imports this module without restricting accessibility, so
!> `LINKW` and `LINKSM` currently reach the variables transitively through
!> `LINK_CW`; `LINK` also imports `LINK_CC1` directly. This module likewise has
!> no active `PRIVATE` statement: `KS`, `KSPBK`, and the use-associated `LLEE`
!> are public. The two workspace variables have no declaration initializers.
!>
!> @warning
!> After populating `KSPBK`, current `LINKW` calculates bed moisture for both
!> banks using the value of `NDUM` left by bank 2, rather than recomputing
!> `NCEBD(NLINK,side)+1` inside that loop as the corresponding startup code in
!> `FRmod` does. If the two banks have different `NCEBD` cell indices, the
!> bank-1 weighting can omit cells or read a `KSPBK(1,cell)` entry outside the
!> range populated for that bank. This documentation transfer records but does
!> not alter the indexing.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-18 | JE | 3.1 | Original version written. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE LINK_CC1
   USE SGLOBAL, ONLY : LLEE
   IMPLICIT NONE
   DOUBLEPRECISION :: KS             !! Current link length divided by the reference length `Z2`.
   DOUBLEPRECISION :: KSPBK(2,LLEE)  !! Bank-cell thickness divided by `Z2`, for the two adjacent banks.
!PRIVATE :: LLEE
END MODULE LINK_CC1
