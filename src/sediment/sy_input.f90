!> summary: The `SY01`--`SY64` data groups and the initial sediment state.
!> author: AB / RAH / BTL, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> [[SYREAD]] reads the sediment data groups; [[SYINIT]] establishes the
!> initial loose hillslope sediment and the two channel-bed layers from them.
!>
!> @warning
!> The manual defines sediment boundary-condition input groups `SY61`--`SY64`,
!> but explicitly states that the sediment boundary-condition routines have not
!> yet been implemented. This matches the current empty [[SYBC]] routine.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into a single Fortran 90 module. |
!> | 2026-04 to 2026-05 | SvB | 4.6.1 | Modernised the whole component: free-form layout, `IMPLICIT NONE`/`INTENT` throughout, structured control flow in place of `GOTO`s, compile-time `PARAMETER`s for the cached first-call constants, and `symain`'s work arrays moved to allocate-once module storage. |
!> | 2026-09-10 | SvB | - | Split out of SYmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_input

   USE MOD_PARAMETERS, ONLY: half, zero
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERRLVL_warn
   USE float_compare, ONLY: dimje
   USE linear_algebra, ONLY: dcopy
   USE record_readers, ONLY: ALREAD
   USE spatial_fields, ONLY: ALALLF
   USE sy_transport_capacity, ONLY: SYDR

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SYBC, SYINIT, SYREAD

CONTAINS

!> Placeholder for time-varying sediment boundary flows.
!>
!> The current implementation is intentionally empty; boundary sediment fluxes
!> are instead handled through existing arrays and setup pathways in [[symain]].
!> The manual's `SY61`-`SY64` boundary records are therefore validated/read as
!> metadata, but no time-varying sediment boundary flux is applied here.
   SUBROUTINE SYBC
!STOP ' FATAL ERROR!!  Sediment boundary flows not yet implemented'
   END SUBROUTINE SYBC

!> Initialises sediment state arrays on the first SY pass.
!>
!> `SYINIT` builds the saved sediment state used by the first sediment time
!> step. It does not read input; it converts already checked SY and WAT arrays
!> into active-layer depths, old-water storage, and per-soil representative
!> sediment sizes.
!>
!> Initialisation groups:
!>
!> | Group | Action |
!> |:------|:-------|
!> | Erosion/source arrays | Zeros column surface erosion `GNU`, link bank erosion `GNUBK`, bed deposition accumulator `ARBDEP`, and infiltration accumulators `GINFD`/`GINFS`. |
!> | Flow-memory arrays | Copies current channel cross-sectional area `ARXL` to `ARXLOL`, sets bankfull depth `DBFULL = ZBFULL - ZGRUND`, and zeros every `QSED(:,sed,face)`. |
!> | Channel bed state | Splits each initial bed depth `DLS(link)` into an active upper layer limited by `DCBEDO` and a lower layer containing the remainder. |
!> | Land-column state | Stores initial surface-water depth `DWATOL = HRF - ZGRUND` for each non-channel element. |
!> | Soil-size summary | Uses [[sydr]] at percentile `0.5` to derive the median representative diameter `DRSO50(soil)` from `SOSDFN` and `DRSED`. |
!>
!> For channel links, the active and lower bed depths are
!>
!> \[
!>   DCBED = \min(DLS, DCBEDO),
!>   \qquad
!>   DDBED = \max(DLS-DCBED,0),
!> \]
!>
!> and each class receives its initial share through
!>
!> \[
!>   DCBSED_s = DCBED\,FBETA_s,
!>   \qquad
!>   DDBSED_s = DDBED\,FBETA_s .
!> \]
!>
!> The conversion factor `FETA` maps eroded in-place soil solid volume to
!> settled sediment volume. It uses bank soil porosity for links and top-layer
!> soil porosity for land elements:
!>
!> \[
!>   FETA_{link} = {1-THSAT(NTSOBK)\over 1-PBSED}, \qquad
!>   FETA_{col} = {1-THSAT(NTSOTP)\over 1-PLS}.
!> \]
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-05 | SvB | 4.6.1 | Replaced the `ALINIT` zeroing of `GNU`, `GNUBK`, `ARBDEP`, `GINFD`, `GINFS`, and `QSED` with array-slice assignment. |
!> | 2026-04-06 | SvB | 4.6.1 | Removed `GOTO`-driven control flow. |
!> @endhistory
   SUBROUTINE SYINIT(NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, NTSOBK, ARXL, DCBEDO, DLS, &
                     FBETA, DRSED, HRF, PBSED, PLS, SOSDFN, THSAT, ZGRUND, NTSOTP, ZBFULL, ARBDEP, &
                     ARXLOL, DCBED, DCBSED, DDBSED, DRSO50, DWATOL, FETA, GINFD, GINFS, GNU, GNUBK, &
                     QSED, DBFULL)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NEL   !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE !! Link-array dimension.
      INTEGER, INTENT(IN) :: NS    !! Number of soil types.
      INTEGER, INTENT(IN) :: NSED  !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NSEE  !! Soil-type array dimension.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      INTEGER, INTENT(IN) :: NTSOBK(NLFEE) !! Bank soil type by link.
      INTEGER, INTENT(IN) :: NTSOTP(NLF + 1:NEL) !! Top soil type by land element.
      DOUBLE PRECISION, INTENT(IN) :: DCBEDO !! Active upper channel-bed layer thickness.
      DOUBLE PRECISION, INTENT(IN) :: ARXL(NLFEE) !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(IN) :: DLS(NEL)    !! Initial loose/bed sediment depth by element.
      DOUBLE PRECISION, INTENT(IN) :: DRSED(NSED) !! Representative sediment particle diameters.
      DOUBLE PRECISION, INTENT(IN) :: FBETA(NELEE, NSED) !! Initial sediment composition by element and size class.
      DOUBLE PRECISION, INTENT(IN) :: HRF(NLF + 1:NEL)   !! Initial land-element water level/head.
      DOUBLE PRECISION, INTENT(IN) :: PBSED(NLFEE)       !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(IN) :: PLS(NLF + 1:NEL)   !! Loose-sediment porosity by land element.
      DOUBLE PRECISION, INTENT(IN) :: SOSDFN(NSEE, NSED) !! Soil sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(IN) :: THSAT(NS)      !! Saturated water content by soil type.
      DOUBLE PRECISION, INTENT(IN) :: ZBFULL(NLFEE)  !! Bankfull elevation/depth by link.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND(NEL)    !! Ground or bed elevation by element.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: ARBDEP(NLFEE) !! Accumulated channel-bed elevation/depth change.
      DOUBLE PRECISION, INTENT(OUT) :: ARXLOL(NLFEE) !! Previous channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(OUT) :: DBFULL(NLFEE) !! Bankfull depth by link.
      DOUBLE PRECISION, INTENT(OUT) :: DCBED(NLFEE)  !! Active upper-bed layer depth by link.
      DOUBLE PRECISION, INTENT(OUT) :: DCBSED(NLFEE, NSED) !! Upper-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(OUT) :: DDBSED(NLFEE, NSED) !! Lower-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(OUT) :: DRSO50(NS)          !! Median soil particle diameter by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: DWATOL(NLF + 1:NEL) !! Previous water depth by land element.
      DOUBLE PRECISION, INTENT(OUT) :: FETA(NEL) !! Soil-to-sediment solid-volume conversion factor by element.
      DOUBLE PRECISION, INTENT(OUT) :: GINFD(NLFEE, NSED) !! Fine infiltration diagnostic/source for deposited material.
      DOUBLE PRECISION, INTENT(OUT) :: GINFS(NLFEE, NSED) !! Fine infiltration diagnostic/source for suspended material.
      DOUBLE PRECISION, INTENT(OUT) :: GNU(NLF + 1:NEL) !! Hillslope erosion rate by land element.
      DOUBLE PRECISION, INTENT(OUT) :: GNUBK(NLFEE)     !! Lateral bank erosion rate by link.
      DOUBLE PRECISION, INTENT(OUT) :: QSED(NELEE, NSEDEE, 4) !! Sediment flux by element, size class, and face.

      ! Locals, etc
      DOUBLE PRECISION :: DCBEDE, DDBEDE, DLSE, FBETAE
      INTEGER          :: IEL, LINK, SED, SOIL, FACE

      ! External functions implicitly called
      ! DOUBLE PRECISION :: DIMJE, SYDR

      !----------------------------------------------------------------------*

      ! * Initialize surface erosion rates in each column (Replaced ALINIT)
      GNU(NLF + 1:NEL) = ZERO

      IF (NLF > 0) THEN
         ! * Initialize bank erosion rates in each link (Replaced ALINIT)
         GNUBK(1:NLF) = ZERO

         ! * Zero bed sediment accumulator (Replaced ALINIT)
         ARBDEP(1:NLF) = ZERO

         ! * Set old river c/s area equal to current river c/s area
         CALL DCOPY(NLF, ARXL, 1, ARXLOL, 1)
      END IF

      ! * Loop over sediment types
      DO SED = 1, NSED

         IF (NLF > 0) THEN
            ! * Initialize infiltration rates (Replaced ALINIT)
            GINFD(1:NLF, SED) = ZERO
            GINFS(1:NLF, SED) = ZERO
         END IF

         ! * Initialize sediment flow rates (Replaced ALINIT)
         DO FACE = 1, 4
            QSED(1:NEL, SED, FACE) = ZERO
         END DO

         ! * Next sediment type
      END DO

      ! * Loop over links
      DO LINK = 1, NLF
         DLSE = DLS(LINK)

         ! * Set ratio of bank soil to bed sediment solid volume fractions
         FETA(LINK) = (1.0D0 - THSAT(NTSOBK(LINK)))/(1.0D0 - PBSED(LINK))

         ! * Set bank full depth
         DBFULL(LINK) = ZBFULL(LINK) - ZGRUND(LINK)

         ! * Bed layer depths
         DCBEDE = MIN(DLSE, DCBEDO)
         DDBEDE = DIMJE(DLSE, DCBEDE)
         DCBED(LINK) = DCBEDE

         ! * Loop over sediment types
         DO SED = 1, NSED
            ! * Initialize sediment depths in both bed layers
            FBETAE = FBETA(LINK, SED)
            DCBSED(LINK, SED) = DCBEDE*FBETAE
            DDBSED(LINK, SED) = DDBEDE*FBETAE
         END DO

         ! * Next link
      END DO

      ! * Loop over column elements
      DO IEL = NLF + 1, NEL
         ! * Set ratio: surface soil to loose sediment solid vol fractions
         FETA(IEL) = (1.0D0 - THSAT(NTSOTP(IEL)))/(1.0D0 - PLS(IEL))

         ! * Calculate initial surface water depth
         DWATOL(IEL) = HRF(IEL) - ZGRUND(IEL)
      END DO

      ! * Calculate median particle diameter for each soil type
      DO SOIL = 1, NS
         DRSO50(SOIL) = SYDR(HALF, NSEE, NSED, SOSDFN(SOIL, 1), DRSED)
      END DO

   END SUBROUTINE SYINIT

!> Reads sediment-yield input data.
!>
!> `SYREAD` loads model flags, particle sizes, soil erodibility, vegetation drip
!> parameters, channel-bank and bed properties, initial loose/bed sediment
!> states, suspended concentrations, and sediment boundary categories.
!>
!> Input record groups:
!>
!> | Records | Data read |
!> |:--------|:----------|
!> | `SY01`-`SY02` | Title and sediment-file version. A version mismatch raises warning 2011, not a fatal error. |
!> | `SY11`-`SY12` | Sediment counts, formula switches, check/substep controls, and scalar concentration/bed/fine controls. Channel-only items are read only when `NLF > 0`. |
!> | `SY21`-`SY24` | Representative particle diameters, soil erodibility/density/clay/bank parameters, soil sediment-size fractions, and vegetation drip parameters. |
!> | `SY31`-`SY32` | Channel bank soil type and bed-sediment porosity, read only when channel links exist. |
!> | `SY41`-`SY43` | Ground cover, rock cover, and loose-sediment porosity, distributed by `ALALLF` over land-column elements. |
!> | `SY51`-`SY53` | Initial loose/bed depth, initial loose/bed composition, and initial mobile sediment concentrations for all elements. |
!> | `SY61`-`SY64` | Sediment boundary counts, boundary definitions, steady flux categories, and steady rating-curve categories. |
!>
!> The distributed `SY52` read accepts the special negative-category option from
!> `ALALLF`. When selected, the routine replaces the read `FBETA` values with
!> the sediment-size fractions of each element's soil type: bank soil `NTSOBK`
!> for links and top-column soil `NTSOTP` for land elements.
!>
!> Boundary types are stored in `NSYBCD(:,2)` as read, but the category index in
!> `NSYBCD(:,3)` is condensed for storage: type 2 categories are appended after
!> type 1 categories, and type 4 categories after type 3 categories. This matches
!> the later validation and boundary metadata layout.
!>
!> Fatal setup errors are raised for insufficient `NELEE` workspace, `NSED`
!> outside `1:NSEDEE`, too many boundary elements/categories, or a boundary type
!> outside `1:4`.
   SUBROUTINE SYREAD(BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, &
                     NSYBEE, NSYCEE, NTSOTP, NV, NX, NXEE, NYEE, NY, SPR, SYD, SYVER, ABC, ALPHA, &
                     BBC, BKB, CONCOB, DCBEDO, DLS, DRDRIP, DRSED, DLSMAX, FBETA, FBIC, FCG, &
                     FCROCK, FDEL, FDRIP, FICRIT, FPCLAY, FPCRIT, GBC, GKF, GKR, ISACKW, ISGSED, &
                     ISSYOK, ISTEC, ISUSED, NEPS, NFINE, NSED, NSYB, NSYBCD, NSYC, NTSOBK, PBSED, &
                     PLS, RHOSO, SOSDFN, XDRIP, IDUM, DUMMY, DUMSED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NEL    !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NS     !! Number of soil types.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      INTEGER, INTENT(IN) :: NSEE   !! Soil-type array dimension.
      INTEGER, INTENT(IN) :: NSYBEE !! Sediment-boundary array dimension.
      INTEGER, INTENT(IN) :: NSYCEE !! Sediment-boundary-category array dimension.
      INTEGER, INTENT(IN) :: NTSOTP(NLF + 1:NEL) !! Top soil type by land element.
      INTEGER, INTENT(IN) :: NV   !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NX   !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NYEE !! Grid-row workspace dimension.
      INTEGER, INTENT(IN) :: NY   !! Number of grid rows.
      INTEGER, INTENT(IN) :: SYD  !! Static sediment input unit.
      INTEGER, INTENT(IN) :: SPR  !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)   !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2) !! Face-neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)   !! Element number at each grid location.
      LOGICAL, INTENT(IN) :: BEXBK        !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE) !! True for north-south channel links.
      CHARACTER(LEN=*), INTENT(IN) :: SYVER !! Expected sediment input-file version string.

      ! Output arguments
      INTEGER, INTENT(OUT) :: ISACKW !! Channel transport-capacity option.
      INTEGER, INTENT(OUT) :: ISGSED !! Overland transport-capacity option.
      INTEGER, INTENT(OUT) :: ISSYOK !! Dynamic sediment input-check interval.
      INTEGER, INTENT(OUT) :: ISTEC  !! Critical-shear calculation option.
      INTEGER, INTENT(OUT) :: ISUSED !! Sediment velocity option.
      INTEGER, INTENT(OUT) :: NEPS   !! Number of sediment substeps per water timestep.
      INTEGER, INTENT(OUT) :: NFINE  !! Number of fine sediment classes.
      INTEGER, INTENT(OUT) :: NSED   !! Number of sediment size classes.
      INTEGER, INTENT(OUT) :: NSYB   !! Number of sediment boundary entries.
      INTEGER, INTENT(OUT) :: NSYBCD(NSYBEE, 3) !! Sediment boundary element, type, and category metadata.
      INTEGER, INTENT(OUT) :: NSYC(4) !! Number of sediment boundary categories by boundary type.
      INTEGER, INTENT(OUT) :: NTSOBK(NLFEE) !! Bank soil type by link.
      DOUBLE PRECISION, INTENT(OUT) :: ABC(NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `A`.
      DOUBLE PRECISION, INTENT(OUT) :: ALPHA !! Fine-sediment settling/resuspension critical-shear ratio.
      DOUBLE PRECISION, INTENT(OUT) :: BBC(NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `B`.
      DOUBLE PRECISION, INTENT(OUT) :: BKB(NS)   !! Bank erodibility by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: CONCOB     !! Mobile concentration threshold for overbank exchange.
      DOUBLE PRECISION, INTENT(OUT) :: DCBEDO     !! Active upper channel-bed layer thickness.
      DOUBLE PRECISION, INTENT(OUT) :: DRDRIP(NV) !! Canopy drip drop diameter by vegetation type.
      DOUBLE PRECISION, INTENT(OUT) :: DRSED(NSEDEE) !! Representative sediment particle diameters.
      DOUBLE PRECISION, INTENT(OUT) :: FBIC   !! Fine-bed fraction threshold for infiltration.
      DOUBLE PRECISION, INTENT(OUT) :: FDRIP(NV) !! Canopy drip fraction by vegetation type.
      DOUBLE PRECISION, INTENT(OUT) :: FICRIT !! Fine-concentration threshold for infiltration.
      DOUBLE PRECISION, INTENT(OUT) :: FPCLAY(NS) !! Clay fraction by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: FPCRIT !! Maximum sediment concentration fraction.
      DOUBLE PRECISION, INTENT(OUT) :: GBC(NSEDEE, NSYCEE) !! Steady boundary sediment input by class/category.
      DOUBLE PRECISION, INTENT(OUT) :: GKF(NS)   !! Flow detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: GKR(NS)   !! Rainfall detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: PBSED(NLFEE) !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(OUT) :: RHOSO(NS) !! Soil bulk density by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: SOSDFN(NSEE, NSEDEE) !! Soil sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: XDRIP(NV) !! Canopy drip fall height by vegetation type.
      DOUBLE PRECISION, INTENT(OUT) :: DLSMAX     !! Loose-sediment depth above which hillslope soil erosion is suppressed.

      ! INOUT Output Arrays (modified via ALALLF slices/subroutines)
      DOUBLE PRECISION, INTENT(INOUT) :: DLS(NEL) !! Initial loose/bed sediment depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA(NELEE, NSEDEE) !! Initial sediment composition by element and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FCG(NLF + 1:NEL)     !! Ground-cover fraction by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: FCROCK(NLF + 1:NEL)  !! Rock-cover fraction by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: FDEL(NELEE, NSEDEE)  !! Initial mobile sediment concentration fraction.
      DOUBLE PRECISION, INTENT(INOUT) :: PLS(NLF + 1:NEL)     !! Loose-sediment porosity by land element.

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT) :: IDUM !! Integer workspace for distributed reads.
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY !! Floating-point workspace for distributed reads.
      DOUBLE PRECISION, DIMENSION(NLFEE*NSEDEE), INTENT(INOUT) :: DUMSED !! Flattened sediment-size workspace for distributed reads.

      CHARACTER(80)  :: CDUM
      CHARACTER(132) :: MSG
      CHARACTER(8)   :: SYDVER
      INTEGER :: BB, IDUM0, I0, IEL, ICAT, ITYPE, NC, NUM_CATEGORIES_TYPES, NNN, NREQ, SED, SOIL

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------
      !
      !     * Check status of data file
      CALL ALREAD(0, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)

      !     * Print SY job title
      CALL ALREAD(1, SYD, SPR, ':SY01', 1, 1, IDUM0, CDUM, IDUM, DUMMY)
      WRITE (SPR, '(/1X,A/)') CDUM

      !     * Check & print version number
      CALL ALREAD(1, SYD, SPR, ':SY02', 1, 1, IDUM0, SYDVER, IDUM, DUMMY)

      !     * [miss off last character to allow eg '3.4.1' is ok in '3.4.1a' ]
      IF (INDEX(SYDVER, SYVER(:LEN(SYVER) - 1)) == 0) THEN
         WRITE (MSG, 9011) SYVER, SYDVER
         CALL RAISE_ERROR(ERRLVL_warn, 2011, SPR, 0, 0, MSG)
      ELSE
         WRITE (SPR, '(4X,2A/)') 'SY Module Version ', SYVER
      END IF

      ! 1. Static Variables
      ! -------------------
      !
      !     * Check workspace array size: part 1
      NREQ = 8
      IF (NELEE < NREQ) THEN
         WRITE (MSG, 9005) NELEE, NREQ
         CALL RAISE_ERROR(ERRLVL_fatal, 2005, SPR, 0, 0, MSG)
      END IF

      !     * Integer
      NNN = 5
      IF (NLF > 0) NNN = 8
      CALL ALREAD(2, SYD, SPR, ':SY11', NNN, 1, IDUM0, CDUM, IDUM, DUMMY)
      NSED = IDUM(1)
      ISGSED = IDUM(2)
      ISTEC = IDUM(3)
      ISSYOK = IDUM(4)
      NEPS = IDUM(5)

      IF (NLF > 0) THEN
         ISACKW = IDUM(6)
         ISUSED = IDUM(7)
         NFINE = IDUM(8)
      END IF

      IF (NSED < 1 .OR. NSED > NSEDEE) THEN
         WRITE (MSG, 9006) NSED, NSEDEE
         CALL RAISE_ERROR(ERRLVL_fatal, 2006, SPR, 0, 0, MSG)
      END IF

      !     * Floating-point
      NNN = 2
      IF (NLF > 0) NNN = 7
      CALL ALREAD(3, SYD, SPR, ':SY12', NNN, 1, IDUM0, CDUM, IDUM, DUMMY)
      FPCRIT = DUMMY(1)
      DLSMAX = DUMMY(2)

      IF (NLF > 0) THEN
         ALPHA = DUMMY(3)
         CONCOB = DUMMY(4)
         DCBEDO = DUMMY(5)
         FBIC = DUMMY(6)
         FICRIT = DUMMY(7)
      END IF

      ! 2. Sediment, Soil & Vegetation Properties
      ! -----------------------------------------
      !
      !     * Check workspace array size: part 2
      NREQ = MAX(MAX(5, NSED)*NS, 3*NV)
      IF (NELEE < NREQ) THEN
         WRITE (MSG, 9005) NELEE, NREQ
         CALL RAISE_ERROR(ERRLVL_fatal, 2005, SPR, 0, 0, MSG)
      END IF

      !     * Sediment
      CALL ALREAD(3, SYD, SPR, ':SY21', NSED, 1, IDUM0, CDUM, IDUM, DRSED)

      !     * Soil
      CALL ALREAD(3, SYD, SPR, ':SY22', 5, NS, IDUM0, CDUM, IDUM, DUMMY)
      CALL DCOPY(NS, DUMMY(1), 5, GKR, 1)
      CALL DCOPY(NS, DUMMY(2), 5, GKF, 1)
      CALL DCOPY(NS, DUMMY(3), 5, RHOSO, 1)
      CALL DCOPY(NS, DUMMY(4), 5, FPCLAY, 1)
      CALL DCOPY(NS, DUMMY(5), 5, BKB, 1)

      !     * Soil composition
      CALL ALREAD(3, SYD, SPR, ':SY23', NSED, NS, IDUM0, CDUM, IDUM, DUMMY)

      DO SED = 1, NSED
         CALL DCOPY(NS, DUMMY(SED), NSED, SOSDFN(1, SED), 1)
      END DO

      !     * Vegetation
      CALL ALREAD(3, SYD, SPR, ':SY24', 3, NV, IDUM0, CDUM, IDUM, DUMMY)
      CALL DCOPY(NV, DUMMY(1), 3, XDRIP, 1)
      CALL DCOPY(NV, DUMMY(2), 3, DRDRIP, 1)
      CALL DCOPY(NV, DUMMY(3), 3, FDRIP, 1)

      ! 3. Link Element Properties
      ! --------------------------
      !
      IF (NLF > 0) THEN
         ! * Bank soil type
         CALL ALREAD(2, SYD, SPR, ':SY31', NLF, 1, IDUM0, CDUM, NTSOBK, DUMMY)

         ! * Porosity of bed sediment
         CALL ALREAD(3, SYD, SPR, ':SY32', NLF, 1, IDUM0, CDUM, IDUM, PBSED)
      END IF

      ! 4. Column-element Properties
      ! ----------------------------
      !
      !     * Ground cover
      CALL ALALLF(1, 1, 0, SYD, SPR, ':SY41', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
                  ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FCG, IDUM, DUMMY)

      !     * Rock cover
      CALL ALALLF(1, 1, 0, SYD, SPR, ':SY42', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
                  ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FCROCK, IDUM, DUMMY)

      !     * Porosity of loose sediment
      CALL ALALLF(1, 1, 0, SYD, SPR, ':SY43', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
                  ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, PLS, IDUM, DUMMY)

      ! 5. All-element Initialization
      ! -----------------------------
      !
      !     * Initial depth of loose/bed sediment
      CALL ALALLF(0, 1, 0, SYD, SPR, ':SY51', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
                  ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, DLS, IDUM, DUMMY)

      !     * Initial composition of loose/bed sediment ...
      CALL ALALLF(0, NSED, -1, SYD, SPR, ':SY52', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, &
                  ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FBETA, IDUM, DUMMY)

      !     ... with special option to inherit composition of soil
      IF (NUM_CATEGORIES_TYPES < 0) THEN
         DO IEL = 1, NLF
            SOIL = NTSOBK(IEL)
            CALL DCOPY(NSED, SOSDFN(SOIL, 1), NSEE, FBETA(IEL, 1), NELEE)
         END DO

         DO IEL = NLF + 1, NEL
            SOIL = NTSOTP(IEL)
            CALL DCOPY(NSED, SOSDFN(SOIL, 1), NSEE, FBETA(IEL, 1), NELEE)
         END DO
      END IF

      !     * Initial concentrations of suspended sediment
      CALL ALALLF(0, NSED, 0, SYD, SPR, ':SY53', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, &
                  ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FDEL, IDUM, DUMMY)

      ! 6. Boundary Data
      ! ----------------
      !
      !     * No of inflow boundary elements & no of categories of each type
      CALL ALREAD(2, SYD, SPR, ':SY61', 5, 1, IDUM0, CDUM, IDUM, DUMMY)
      NSYB = IDUM(1)
      DO ITYPE = 1, 4
         NSYC(ITYPE) = IDUM(1 + ITYPE)
      END DO

      IF (NSYB > 0) THEN
         IF (NSYB > NSYBEE) THEN
            WRITE (MSG, 9007) NSYB, NSYBEE
            CALL RAISE_ERROR(ERRLVL_fatal, 2007, SPR, 0, 0, MSG)
         END IF

         ! * Check workspace array size: part 3
         NREQ = MAX(3*NSYB, NSED*NSYC(1), NSED*2*NSYC(3))
         IF (NELEE < NREQ) THEN
            WRITE (MSG, 9005) NELEE, NREQ
            CALL RAISE_ERROR(ERRLVL_fatal, 2005, SPR, 0, 0, MSG)
         END IF

         ! * Integer boundary data
         CALL ALREAD(2, SYD, SPR, ':SY62', 3, NSYB, IDUM0, CDUM, IDUM, DUMMY)
         I0 = 0

         DO BB = 1, NSYB
            IEL = IDUM(I0 + 1)
            ITYPE = IDUM(I0 + 2)
            ICAT = IDUM(I0 + 3)

            IF (ITYPE < 1 .OR. ITYPE > 4) THEN
               WRITE (MSG, 9008) BB, ITYPE
               CALL RAISE_ERROR(ERRLVL_fatal, 2008, SPR, 0, 0, MSG)
            END IF

            ! * condense 4 into 2 by adding cats 2 & 4 to lists for 1 & 3
            IF (MOD(ITYPE, 2) == 0) ICAT = ICAT + NSYC(ITYPE - 1)
            NSYBCD(BB, 1) = IEL
            NSYBCD(BB, 2) = ITYPE
            NSYBCD(BB, 3) = ICAT
            I0 = I0 + 3
         END DO

         ! * Steady flux data
         NC = NSYC(1)
         IF (NC > 0) THEN
            IF (NC > NSYCEE) THEN
               WRITE (MSG, 9009) NSYC(1), NSYCEE
               CALL RAISE_ERROR(ERRLVL_fatal, 2009, SPR, 0, 0, MSG)
            END IF

            CALL ALREAD(3, SYD, SPR, ':SY63', NSED, NC, IDUM0, CDUM, IDUM, DUMMY)
            DO SED = 1, NSED
               CALL DCOPY(NC, DUMMY(SED), NSED, GBC(SED, 1), NSEDEE)
            END DO
         END IF

         ! * Steady rating curve data
         NC = NSYC(3)
         IF (NC > 0) THEN
            IF (NC > NSYCEE) THEN
               WRITE (MSG, 9010) NSYC(3), NSYCEE
               CALL RAISE_ERROR(ERRLVL_fatal, 2010, SPR, 0, 0, MSG)
            END IF

            CALL ALREAD(3, SYD, SPR, ':SY64', NSED*2, NC, IDUM0, CDUM, IDUM, DUMMY)
            DO SED = 1, NSED
               CALL DCOPY(NC, DUMMY(2*SED - 1), 2*NSED, ABC(SED, 1), NSEDEE)
               CALL DCOPY(NC, DUMMY(2*SED), 2*NSED, BBC(SED, 1), NSEDEE)
            END DO
         END IF
      END IF

      ! 7. Epilogue
      ! -----------
      !
      !     * Close the data file
      CALL ALREAD(-1, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)

      RETURN

      ! Format Statements ----------------------------------------------------
9003  FORMAT(1X, A)

9005  FORMAT('Workspace available is NELEE = ', I5, &
             '; workspace required in subroutine SYREAD is ', I6)

9006  FORMAT('No. of size groups NSED=', I4, &
             ' is not in range [1,NSEDEE=', I3, ']')

9007  FORMAT('No. of boundaries NSYB=', I5, &
             ' is greater than NSYBEE=', I4, ']')

9008  FORMAT('Boundary type NSYBCD(', I4, ',2)=', I2, &
             ' is not is the range [1,4]')

9009  FORMAT('No. of steady flux categories NSYC(1)=', I4, &
             ' is greater than NSYCEE=', I3, ']')

9010  FORMAT('No. of steady rating categories NSYC(3)=', I4, &
             ' is greater than NSYCEE=', I3, ']')

9011  FORMAT('SY module is version ', A, '; SYD data file is version ', A)

   END SUBROUTINE SYREAD

END MODULE sy_input

