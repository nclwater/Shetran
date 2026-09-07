!> summary: Shared sediment state for sediment yield and contaminant transport.
!>
!> Replaces the legacy `SED.CS` common blocks. [[run_sim:simulation]] passes
!> the principal state arrays to [[symod:symain]], which reads, validates, and
!> advances sediment stores, erosion, infiltration, and face discharges.
!> [[frmod:incm]] supplies a three-size-class fallback when sediment transport
!> is disabled, and [[cmmod:cmsim]] uses the state to couple particulate
!> transport and channel-bed changes to contaminant transport. Frame output and
!> [[visualisation_interface_left]] expose selected results.
!>
!> Array bounds are compile-time capacities imported from `SGLOBAL`; only the
!> active link (`1:NLF`), element (`1:NEL`), soil (`1:NS`), and sediment-class
!> (`1:NSED`) slices contain model state. Link-end indices are `1:2`, while the
!> face index of `QSED` is `1:4`. Module state and the imported capacities are
!> public by default.
!>
!> Current-code initialization caveat: no complete sediment-enabled producer
!> was found for `FBTSD`, `NSOBED`, or `QLINK`. `INCM`/`CMSIM` establish them
!> only on the no-sediment fallback path, apart from `INCM` changing a zero
!> `NSOBED` value to one immediately before one use. `QDEFF` is likewise zeroed
!> only on the fallback path, although every current contaminant calculation
!> multiplies it by a local zero. This module itself supplies no defaults.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.0 | Original version written. |
!> | 1991-06-16 | JE | 3.1 | Completed the original implementation. |
!> | 1993-02-08 | GP | 3.4 | Moved `QLINK` and `QDEFF` from `LINK.CW`, added `SEDDIA`, and renamed `SDPOR` as `SDEPOR`. |
!> | 1994-10-02 | RAH | 3.4.1 | Standardized the header and declarations, added `QSED`, and removed redundant `SEDDIA`. |
!> | 1997-02-20 | RAH | 4.1 | Separated `SDEPOI` from the mixed-type `SDEPOR` block. |
!> | 1998-03-08 | RAH | 4.2 | Removed `PSD`. |
!> | 1999-01-27 | SB | 4.27 | Added `DCBED` and `DCBSED`. |
!> | 2004-11 | JE | - | Converted to Fortran 95. |
!> @endhistory
MODULE sed_cs
   USE SGLOBAL, ONLY : NELEE, NLFEE, NSEDEE, NSEE
   IMPLICIT NONE

   DOUBLEPRECISION ARBDEP(NLFEE)        !! Deposited-sediment cross-sectional area by link [m2].
   DOUBLEPRECISION DLS(NELEE)            !! Loose/bed sediment depth by active element [m].
   DOUBLEPRECISION GINFD(NLFEE,NSEDEE)   !! Sediment-volume infiltration rate used for the deep-bed term [m3/s].
   DOUBLEPRECISION GINFS(NLFEE,NSEDEE)   !! Sediment-volume infiltration rate used for the bed-surface term [m3/s].
   DOUBLEPRECISION GNU(NELEE)            !! Ground-surface erosion depth rate by column element [m/s].
   DOUBLEPRECISION GNUBK(NLFEE)          !! Lateral bank-erosion depth rate by link [m/s].
   DOUBLEPRECISION DCBED(NLFEE)          !! Total thickness of the active upper channel-bed layer by link [m].
   DOUBLEPRECISION DCBSED(NLFEE,NSEDEE)  !! Thickness contribution of each size class in the active upper bed layer [m].

   DOUBLEPRECISION FDEL(NELEE,NSEDEE) !! Mobile-sediment settled-depth/water-depth ratio by element and size class.

   DOUBLEPRECISION FBETA(NELEE,NSEDEE)  !! Loose/bed sediment composition fraction by element and size class.
   DOUBLEPRECISION FBTSD(NLFEE,NSEDEE)  !! Newly deposited sediment composition fraction by link and size class.

   DOUBLEPRECISION PBSED(NLFEE)        !! Channel-bed sediment porosity by link.
   DOUBLEPRECISION PLS(NELEE)          !! Loose-sediment porosity by column element.
   DOUBLEPRECISION SOSDFN(NSEE,NSEDEE) !! Un-eroded-soil mass fraction by soil type and sediment size class.
   DOUBLEPRECISION SOFN(NSEE,NSEDEE)   !! Three-class fallback soil fraction read from contaminant input.

   INTEGER NSOBED(NLFEE) !! Parent-material soil-type index at the stream bed by link.

   INTEGER NSED !! Number of active sediment size classes.

   DOUBLEPRECISION QLINK(NLFEE,2)       !! Water discharge at the two link ends [m3/s].
   DOUBLEPRECISION QDEFF(NLFEE,2)       !! Effective-flow correction at the two link ends [m3/s].
   DOUBLEPRECISION QSED(NELEE,NSEDEE,4) !! Solid-sediment volume discharge by element, size class, and face [m3/s].
!PRIVATE :: NELEE, NLFEE, NSEDEE, NSEE
END MODULE sed_cs
