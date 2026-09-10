!> summary: Compile-time capacity bounds shared by more than one component.
!> author: GP; AB/RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> These are capacities, not the active problem size. Setup reads the active
!> dimensions from the input files and must keep every active index within the
!> corresponding capacity here; the manual's array-size table describes the
!> configured values. Almost every component declares at least one fixed-size
!> array against one of these bounds, which is why they sit in a module of their
!> own with no dependencies but the numeric kinds.
!>
!> | Capacity group | Parameters | Bounded data |
!> |:---------------|:-----------|:-------------|
!> | Horizontal topology | `NXEE`, `NYEE`, `NLFEE`, `NELEE` | Grid extents, links, and all elements. |
!> | Vertical subsurface | `LLEE`, `NLYREE`, `NSEE`, `NVSEE` | Cells, layer boundaries, soil types, and VSS tables. |
!> | Vegetation and forcing | `NVEE`, `NVBP`, `NUZTAB` | Vegetation/meteorological series, breakpoints, and ET entries. |
!> | Process and output | `NSETEE`, `NOCTAB`, `NSEDEE`, `NCONEE`, `NOLEE` | Result sets and OC, sediment, and contaminant tables. |
!> | Plants, snow, channel tables | `NPLTEE`, `NPELEE`, `max_no_snowmelt_slugs`, `NXSCEE` | Types/slots, slugs, and samples. |
!> | Legacy result classes and aliases | `NCLASS`, `NXE`, `NYE` | Legacy binary-result element classes and the `NXEE`/`NYEE` workspace aliases. |
!>
!> `NOLEE`, `NPLTEE` and the `NXE`/`NYE` aliases are written in terms of other
!> parameters in this module, so the whole group has to travel together.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_D, sglobal; see docs/rename/proposal.md. |
!> @endhistory
MODULE array_limits

   USE MOD_PARAMETERS, ONLY: I_P

   IMPLICIT NONE

   INTEGER(KIND=I_P), PARAMETER :: nxee = 1000 !! Maximum basic-grid extent in the x direction.
   INTEGER(KIND=I_P), PARAMETER :: nyee = 1000 !! Maximum basic-grid extent in the y direction.
   INTEGER(KIND=I_P), PARAMETER :: nlfee = 20000 !! Maximum number of channel links.
   INTEGER(KIND=I_P), PARAMETER :: nelee = 250000 !! Maximum total number of grid, bank, and channel-link elements.
   INTEGER(KIND=I_P), PARAMETER :: LLEE = 50 !! Maximum number of vertical computational cells per element.
   INTEGER(KIND=I_P), PARAMETER :: NVEE = 250000 !! Vegetation/meteorological-series capacity, including rainfall stations.
   INTEGER(KIND=I_P), PARAMETER :: NSEE = 1000 !! Maximum number of soil types.
   INTEGER(KIND=I_P), PARAMETER :: NVSEE = 20 !! Maximum number of entries in VSS lookup and boundary tables.
   INTEGER(KIND=I_P), PARAMETER :: NVBP = 140 !! Maximum time-varying vegetation breakpoints per vegetation type.
   INTEGER(KIND=I_P), PARAMETER :: NUZTAB = 20 !! Maximum PSI, RCF, and FET lookup entries per vegetation type.
   INTEGER(KIND=I_P), PARAMETER :: NLYREE = 20 !! Soil-layer boundary capacity (maximum layers plus one).
   INTEGER(KIND=I_P), PARAMETER :: NSETEE = 45 !! Maximum output sets in legacy binary results metadata.
   INTEGER(KIND=I_P), PARAMETER :: NOCTAB = 20 !! Maximum OC roughness, cross-section, or boundary-table category count.
   INTEGER(KIND=I_P), PARAMETER :: NSEDEE = 7 !! Maximum number of sediment size fractions.
   INTEGER(KIND=I_P), PARAMETER :: NCONEE = 3 !! Maximum number of numeric contaminants.
   INTEGER(KIND=I_P), PARAMETER :: NOLEE = 2*LLEE !! Maximum contaminant column-overlap entries.
   INTEGER(KIND=I_P), PARAMETER :: NPLTEE = NVEE !! Maximum number of contaminant plant types.
   INTEGER(KIND=I_P), PARAMETER :: NPELEE = 2 !! Maximum number of contaminant plant slots per element.
   INTEGER(KIND=I_P), PARAMETER :: max_no_snowmelt_slugs = 400 !! Maximum stored snowmelt-slug records per element.
   INTEGER(KIND=I_P), PARAMETER :: NXSCEE = 100000 !! Number of samples in each channel cross-section/conveyance table.
   INTEGER, PARAMETER :: NCLASS = 14 !! Number of element classes supported by the legacy binary-result format.
   INTEGER, PARAMETER :: NXE = NXEE !! Legacy x workspace capacity alias.
   INTEGER, PARAMETER :: NYE = NYEE !! Legacy y workspace capacity alias.

END MODULE array_limits

