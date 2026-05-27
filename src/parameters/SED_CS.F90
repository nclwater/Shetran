!> summary: Sediment and contaminant interface state.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `SED_CS` replaces the legacy `SED.CS` common blocks. It stores the shared
!> sediment/contaminant interface variables: deposited sediment area and depth,
!> infiltration and erosion rates, sediment fractions and porosities, stream-bed
!> parent-material soil type, sediment count, link flows, and particle flow
!> rates.
!>
!> Main state groups:
!>
!> | Group | Symbols | Purpose |
!> |:------|:--------|:--------|
!> | Deposited and loose sediment | `ARBDEP`, `DLS`, `DCBED`, `DCBSED` | Link/element sediment stores and active bed layers. |
!> | Generation/infiltration | `GNU`, `GNUBK`, `GINFD`, `GINFS` | Hillslope, bank, and link source/sink rates. |
!> | Fractions/porosity | `FDEL`, `FBETA`, `FBTSD`, `PBSED`, `PLS`, `SOSDFN`, `SOFN` | Mobile, bed, and soil fractions. |
!> | Routing | `QLINK`, `QDEFF`, `QSED` | Water-link and particle-flow rates used by sediment and contaminant transport. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.0 | Original version written. |
!> | 1991-06-16 | JE | 3.1 | Completed. |
!> | 1993-02-08 | GP | 3.4 | Brought `QLINK`/`QDEFF` from `LINK.CW`, added `SEDDIA`, and renamed `SDPOR` as `SDEPOR`. |
!> | 1994-10-02 | RAH | 3.4.1 | Applied standard header, declared variables, added `QSED`, and removed redundant `SEDDIA`. |
!> | 1997-02-20 | RAH | 4.1 | Separated `SDEPOI` from mixed-type `SDEPOR`. |
!> | 1998-03-08 | RAH | 4.2 | Removed `PSD`. |
!> | 1999-01-27 | SB | 4.27 | Added `DCBED` and `DCBSED`. |
!> | 2004-11 | JE | - | Converted to Fortran 95. |
!> @endhistory
MODULE sed_cs
      USE SGLOBAL, ONLY : NELEE, NLFEE, NSEDEE, NSEE
      IMPLICIT NONE
      DOUBLEPRECISION :: ARBDEP(NLFEE)       !! Accumulated cross-sectional area of deposited sediment by link.
      DOUBLEPRECISION :: DLS(NELEE)          !! Loose or bed sediment depth by element/link.
      DOUBLEPRECISION :: GINFD(NLFEE,NSEDEE) !! Dynamic/deposited-bed sediment infiltration rate by link and size class.
      DOUBLEPRECISION :: GINFS(NLFEE,NSEDEE) !! Stream-water/suspended sediment infiltration rate by link and size class.
      DOUBLEPRECISION :: GNU(NELEE)          !! Unsaturated-zone erosion/source rate by element.
      DOUBLEPRECISION :: GNUBK(NLFEE)        !! Bank erosion/source rate by link.
      DOUBLEPRECISION :: DCBED(NLFEE)        !! Active upper-bed sediment depth by link.
      DOUBLEPRECISION :: DCBSED(NLFEE,NSEDEE) !! Active upper-bed sediment depth by link and size class.

      DOUBLEPRECISION :: FDEL(NELEE,NSEDEE) !! Mobile sediment concentration fraction by element/link and size class.

      DOUBLEPRECISION :: FBETA(NELEE,NSEDEE) !! Loose/bed sediment composition fraction by element/link and size class.
      DOUBLEPRECISION :: FBTSD(NLFEE,NSEDEE) !! Transported or newly deposited sediment fraction by link and size class.

      DOUBLEPRECISION :: PBSED(NLFEE)        !! Channel-bed sediment porosity by link.
      DOUBLEPRECISION :: PLS(NELEE)          !! Loose-sediment porosity by element.
      DOUBLEPRECISION :: SOSDFN(NSEE,NSEDEE) !! Soil particle-size composition used when sediment is active.
      DOUBLEPRECISION :: SOFN(NSEE,NSEDEE)   !! Soil particle-size composition used to seed `SOSDFN` if sediment is inactive.

      INTEGER :: NSOBED(NLFEE) !! Soil type for parent material at the stream bed by link.

      INTEGER :: NSED !! Number of active sediment size classes.

      DOUBLEPRECISION :: QLINK(NLFEE,2)       !! Link water-flow rates at the two link ends.
      DOUBLEPRECISION :: QDEFF(NLFEE,2)       !! Effective sediment-link flow correction at the two link ends.
      DOUBLEPRECISION :: QSED(NELEE,NSEDEE,4) !! Particle-flow rate by element/link, size class, and face.
!PRIVATE :: NELEE, NLFEE, NSEDEE, NSEE
END MODULE sed_cs
