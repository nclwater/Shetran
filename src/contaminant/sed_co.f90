!> summary: Previous sediment state used by the contaminant column and link equations.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> `SED_CO` replaces the legacy `SED.CO` common blocks. It retains selected
!> values from the sediment/contaminant interface in [[sed_cs]] so that
!> [[cmmod:colm]] and [[cmmod:fret]] can form old-state storage, erosion, and
!> retardation terms. The active entries cover soil and bank columns
!> `NLF+1:NEL`, channel links `1:NLF`, and sediment classes `1:NSED`; the
!> declared extents are compile-time capacities.
!>
!> | State | Initializer/updater | Consumer |
!> |:------|:--------------------|:---------|
!> | `DLSO`, `GNUO` | [[frmod:incm]]; shifted by [[cmmod:colmw]]. | Previous values in `COLM`. |
!> | `FBETAO`, column `FDELO` | No startup seed; shifted by [[cmmod:colmsm]]. | Column `FRET` calls. |
!> | `FBBEDO`, link `FDELO`, `FBTSDO` | `INCM`; shifted by [[cmmod:linksm]]. | Link `FRET` calls. |
!>
!> `FDELO` is the previous value of the manual's dimensionless `FDEL`: for
!> each sediment size class, this is the ratio of its hypothetical settled
!> depth to water depth, not a particle-composition fraction. `FBETAO`,
!> `FBBEDO`, and `FBTSDO` are size-class composition fractions. `FBBEDO`
!> combines loose channel-bed sediment with underlying parent material.
!>
!> Despite the legacy header's reference to old surface-water depth, this
!> module contains no such depth variable. Previous column surface-water depth
!> is held in `DSWO` in [[colm_co]]. This module supplies no declaration
!> initializers. Its six arrays and the three imported `SGLOBAL` capacities are
!> public because no `PRIVATE` statement is active.
!>
!> @warning
!> `INCM` does not initialize `FBETAO` or the column entries of `FDELO` before
!> `COLMSM` first reads them. Their old fractions, and therefore the first
!> column retardation update, can be undefined under standard Fortran. For links,
!> `INCM` initializes `FBTSDO` from `FBTSD`; as recorded in [[sed_cs]], no
!> complete sediment-enabled producer was found for current `FBTSD`.
!>
!> `COLMSM` and `LINKSM` overwrite their saved composition arrays inside the
!> loop over contaminants. Thus only contaminant 1 sees the sediment fraction
!> change from the previous timestep; later numeric contaminants receive the
!> current fractions as both the old and new `FRET` inputs. This documentation
!> transfer records but does not change either behavior.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed the original implementation. |
!> | 1997-03-14 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE SED_CO
   USE SGLOBAL, ONLY : NELEE, NLFEE, NSEDEE
   IMPLICIT NONE

   DOUBLEPRECISION DLSO (NELEE) !! Previous loose/bed-sediment depth by element [m]; actively used for columns.
   DOUBLEPRECISION GNUO (NELEE) !! Previous ground-surface erosion depth rate by element [m/s]; actively used for columns.

   DOUBLEPRECISION FBETAO (NELEE, NSEDEE) !! Previous loose/bed-sediment composition fraction by element and size class.
   DOUBLEPRECISION FDELO (NELEE, NSEDEE)  !! Previous `FDEL` settled-depth/water-depth ratio by element and size class.
   DOUBLEPRECISION FBBEDO (NLFEE, NSEDEE) !! Previous combined channel-bed composition fraction by link and size class.
   DOUBLEPRECISION FBTSDO (NLFEE, NSEDEE) !! Previous newly deposited sediment composition fraction by link and size class.
!PRIVATE :: NELEE, NLFEE, NSEDEE
end MODULE SED_CO
