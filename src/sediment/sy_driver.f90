!> summary: The sediment timestep driver, its workspace allocation and the mass-balance placeholder.
!> author: AB / RAH / BTL, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> [[SYMAIN]] is the component's top-level per-timestep routine: it reads and
!> validates the input on the first call, initialises the loose hillslope
!> sediment and the two channel-bed layers, derives the hydraulic quantities
!> from the water modules, then calls the erosion, capacity and routing
!> routines in turn and updates the mobile concentration, bed and output
!> arrays. [[simulation_driver:SIMULATION]] passes it the sediment state
!> arrays and calls nothing else in the component.
!>
!> `INITIALISE_SYMAIN_WORKSPACE` allocates [[sy_workspace]]'s arrays once, on
!> the first call.
!>
!> @warning
!> Sediment mass-balance output is still a placeholder in [[BALSED]].
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
MODULE sy_driver

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, zero
   USE array_limits, ONLY: nelee, nlfee, NLYREE, NSEDEE, NSEE, NVEE, nxee, nyee
   USE error_status, ONLY: errstat_alloc
   USE linear_algebra, ONLY: dcopy
   USE sy_config, ONLY: ABC_symain, ACKW_symain, ALPHA_symain, ARXLOL_symain, BBC_symain, &
                        BKB_symain, CONCOB_symain, DBFULL_symain, DCBEDO_symain, &
                        DDBSED_symain, DLSMAX_symain, DRDRIP_symain, DRSED_symain, &
                        DRSO50_symain, DWATOL_symain, FBIC_symain, FCG_symain, &
                        FCROCK_symain, FDRIP_symain, FETA_symain, FICRIT_symain, &
                        FPCLAY_symain, FPCRIT_symain, GBC_symain, GKF_symain, GKR_symain, &
                        ISACKW_symain, ISGSED_symain, ISSYOK_symain, ISTEC_symain, &
                        ISUSED_symain, NEPS_symain, NFINE_symain, NSYB_symain, &
                        NSYBCD_symain, NSYBEE, NSYC_symain, NSYCEE, NTSOBK_symain, &
                        NTSOTP_symain, PASS_symain, RHOSO_symain, SYNOW_symain, XDRIP_symain
   USE sy_workspace, ONLY: BARM, CONCI, DCIPRM, DDIPRM, DRDROP, DUMSED, DWAT1, EPSB, &
                           FQCONF, IDUM1A, IDUM1X, LDUM, LRAIN, QSDWAT, QSEDB, QWATB, &
                           SLOPEJ, TAUJ, TAUK, VCFMAX, VINFMX
   USE sy_input, ONLY: SYBC, SYINIT, SYREAD
   USE sy_validation, ONLY: SYERR0, SYERR1, SYERR2, SYERR3
   USE sy_transport_capacity, ONLY: SYCLTR
   USE sy_hillslope, ONLY: SYCOLM, SYFINE, SYOVER
   USE sy_channel, ONLY: SYBED, SYBKER, SYLINK, SYWAT

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SYMAIN, BALSED

CONTAINS

!> Allocates [[symain]]'s work arrays once, on its first call.
!>
!> `SYMAIN` is the controlling routine for the sediment yield module and has no
!> separate `SYINI`-style initialisation entry point. Its first call,
!> identified by `PASS_symain == 1`, performs the sediment checks, input read,
!> and static initialisation. These arrays are needed during that first-pass
!> work and during every later timestep call.
!>
!> The arrays used to be automatic (stack) local arrays in `SYMAIN`, which
!> could overflow the stack for large models under Windows. They are now
!> module-level `ALLOCATABLE` work arrays that live on the heap, allocated
!> once here rather than costing an allocation on every sediment timestep.
!> `SYMAIN` overwrites or clears the arrays as needed before use.
!>
!> @note An intermediate version of this change made the arrays local
!> `ALLOCATABLE`s in `SYMAIN` but re-`ALLOCATE`d them, without a `DEALLOCATE`,
!> on every call; that would fail from the second `SYMAIN` call onwards. The
!> `IF (.NOT. ALLOCATED(...))` guard below was added to allocate only once.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
   SUBROUTINE INITIALISE_SYMAIN_WORKSPACE()
      IMPLICIT NONE

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "SYmod:initialise_symain_workspace"

      IF (.NOT. ALLOCATED(BARM)) THEN
         ALLOCATE (BARM(NLFEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "BARM", location, emsg)
         ALLOCATE (CONCI(NLFEE, NSEDEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "CONCI", location, emsg)
         ALLOCATE (DCIPRM(NLFEE, NSEDEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "DCIPRM", location, emsg)
         ALLOCATE (DDIPRM(NLFEE, NSEDEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "DDIPRM", location, emsg)
         ALLOCATE (DRDROP(NELEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "DRDROP", location, emsg)
         ALLOCATE (DUMSED(NLFEE*NSEDEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "DUMSED", location, emsg)
         ALLOCATE (DWAT1(NELEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "DWAT1", location, emsg)
         ALLOCATE (EPSB(NLFEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "EPSB", location, emsg)
         ALLOCATE (FQCONF(NLFEE, 3), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "FQCONF", location, emsg)
         ALLOCATE (IDUM1A(NELEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IDUM1A", location, emsg)
         ALLOCATE (IDUM1X(NELEE + 3), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IDUM1X", location, emsg)
         ALLOCATE (LDUM(NELEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "LDUM", location, emsg)
         ALLOCATE (LRAIN(NELEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "LRAIN", location, emsg)
         ALLOCATE (QSDWAT(NLFEE, NSEDEE, 4), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "QSDWAT", location, emsg)
         ALLOCATE (QSEDB(NSEDEE, NSYBEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "QSEDB", location, emsg)
         ALLOCATE (QWATB(NSYBEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "QWATB", location, emsg)
         ALLOCATE (SLOPEJ(NELEE, 4), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "SLOPEJ", location, emsg)
         ALLOCATE (TAUJ(NELEE, 4), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "TAUJ", location, emsg)
         ALLOCATE (TAUK(NELEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "TAUK", location, emsg)
         ALLOCATE (VCFMAX(NLFEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "VCFMAX", location, emsg)
         ALLOCATE (VINFMX(NLFEE), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "VINFMX", location, emsg)
      END IF

   END SUBROUTINE INITIALISE_SYMAIN_WORKSPACE

!> Controls the sediment-yield component for setup and timestep execution.
!>
!> `SYMAIN` is the top-level driver for the optional `SY` sediment component.
!> Its interface is dimensioned with the shared array-size constants from
!> `SGLOBAL`/`AL.P` (`NELEE`, `NLFEE`, `NLYREE`, `NSEDEE`, `NSEE`, `NVEE`,
!> `NXEE`) because `NSED` is not known until the sediment input file is read
!> and channel arrays must remain valid when `NLF = 0`.
!>
!> On the first call (`PASS_symain = 1`) the routine performs setup:
!>
!> | Step | Routine/action |
!> |:-----|:---------------|
!> | Work arrays | [[initialise_symain_workspace]] allocates this routine's heap work arrays. |
!> | Interface dimensions | [[syerr0]] checks scalar bounds and file units. |
!> | Static water interface | [[syerr1]] checks topology, geometry, soil, and vegetation indices. |
!> | Column soil mapping | Stores the current top-layer soil type in `NTSOTP_symain`. |
!> | Sediment input | [[syread]] reads the manual `SY01`-`SY64` data groups. |
!> | Sediment validation | [[syerr2]] checks particle sizes, controls, properties, initial state, and boundary metadata. |
!> | Initial state | [[syinit]] initialises bed layers, loose sediment, concentrations, rates, and saved old-time water geometry. |
!>
!> On later calls it performs one water-flow time step. Optional dynamic input
!> checking is controlled by `ISSYOK`: values less than 1 disable checking;
!> otherwise [[syerr3]] runs on the first non-initialisation call and then every
!> `ISSYOK` water-flow calls. The check verifies the current water state and the
!> `ISORT` donor-before-receptor routing order.
!> [[sywat]] derives water-dependent depths, slopes, shear stresses, rainfall,
!> and confluence weights; [[syover]] and [[sybker]] calculate hillslope and
!> bank erosion.
!>
!> The water step is split into `NEPS` sediment substeps of length
!>
!> \[
!>   DTSY = DTUZ / NEPS .
!> \]
!>
!> In each substep, `QSED` is reset, boundary metadata is visited through
!> [[sybc]] when present, channel capacity/advection coefficients are prepared
!> by [[sycltr]], fine-sediment limits by [[syfine]], and elements are processed
!> in `ISORT` order. Link elements call [[sylink]]; land elements call
!> [[sycolm]]. Outgoing sediment fluxes are immediately copied to regular
!> neighbours through `ICMREF` or distributed through confluence branches with
!> `ICMRF2` and `FQCONF`. After all elements are routed, [[sybed]] updates the
!> two channel-bed layers, old water depths/cross-sectional areas are saved, and
!> the sediment clock is advanced before being reset exactly to `UZNOW`.
!>
!> @note The boundary-condition branch reflects the original intended structure,
!> but [[sybc]] is currently an empty routine in this source file. The input
!> metadata can be read and checked, but time-varying sediment boundary fluxes
!> are not implemented here.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-05-04 | SvB | 4.6.1 | Changed this routine's large work arrays (`CONCI`, `DCIPRM`, `DDIPRM`, `DRDROP`, `DUMSED`, `DWAT1`, `EPSB`, `FQCONF`, `IDUM1A`, `IDUM1X`, `LDUM`, `LRAIN`, `QSDWAT`, `QSEDB`, `QWATB`, `SLOPEJ`, `TAUJ`, `TAUK`, `VCFMAX`, `VINFMX`, `BARM`) from automatic (stack) local arrays to local `ALLOCATABLE`s, to avoid a stack-overflow crash on Windows for large models. |
!> | 2026-05-10 | SvB | 4.6.1 | Moved those work arrays to module scope, allocated once by [[initialise_symain_workspace]] (see that routine's own history and notes). |
!> @endhistory
   SUBROUTINE SYMAIN(NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD, ICMBK, ICMREF, ICMRF2, ICMXY, &
                     NBFACE, NLYR, NTSOIL, NVC, AREA, CLENTH, CWIDTH, DHF, DXQQ, DYQQ, THSAT, &
                     ZBFULL, ZGRUND, BEXBK, LINKNS, ISORT, DTUZ, TIH, UZNOW, ARXL, CLAI, DRAINA, &
                     HRF, PLAI, PNETTO, QOC, NSED, PBSED, PLS, SOSDFN, ARBDEP, DLS, FBETA, FDEL, &
                     GINFD, GINFS, GNU, GNUBK, QSED, DCBED, DCBSED, IDUM, DUMMY)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NEL !! Number of elements.
      INTEGER, INTENT(IN) :: NLF !! Number of channel links.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NV  !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: SFB !! Sediment boundary file unit.
      INTEGER, INTENT(IN) :: SPR !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: SRB !! Sediment rating-boundary file unit.
      INTEGER, INTENT(IN) :: SYD !! Static sediment input unit.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)   !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMRF2(NLFEE, 3, 2) !! Confluence branch reference map.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)   !! Element number at each grid location.
      INTEGER, INTENT(IN) :: NBFACE(NEL)       !! Number of boundary faces by element.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE) !! Soil type index for each element layer.
      INTEGER, INTENT(IN) :: ISORT(NEL) !! Donor-before-receptor element routing order.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND(NEL) !! Ground or bed elevation by element.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: TIH   !! Initial simulation time in hours (unused in this routine).
      DOUBLE PRECISION, INTENT(IN) :: UZNOW !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(INOUT) :: ARXL(NLFEE) !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(INOUT) :: CLAI(NV)    !! Current canopy leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: DRAINA(NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
      DOUBLE PRECISION, INTENT(INOUT) :: HRF(NEL)    !! Water level/head by element.
      DOUBLE PRECISION, INTENT(INOUT) :: PLAI(NV)    !! Potential/maximum leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: PNETTO(NLF + 1:NEL) !! Net precipitation/effective rainfall by land element.
      DOUBLE PRECISION, INTENT(IN) :: QOC(NELEE, 4) !! Face water fluxes.
      LOGICAL, INTENT(IN) :: BEXBK        !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE) !! True for north-south channel links.

      ! Checked by SYERR1 via ALCHK/ALCHKI interfaces
      INTEGER, INTENT(INOUT) :: ICMREF(NELEE, 4, 2:3) !! Face-neighbour and reverse-face reference map.
      INTEGER, INTENT(INOUT) :: NLYR(NLF + 1:NEL) !! Number of soil layers in each land element.
      INTEGER, INTENT(INOUT) :: NVC(NLF + 1:NEL)  !! Vegetation type by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: AREA(NEL)     !! Element plan area.
      DOUBLE PRECISION, INTENT(INOUT) :: CLENTH(NLFEE) !! Channel-link length.
      DOUBLE PRECISION, INTENT(INOUT) :: CWIDTH(NLFEE) !! Channel width by link.
      DOUBLE PRECISION, INTENT(INOUT) :: DHF(NELEE, 4) !! Face-to-face hydraulic distance.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NLF + 1:NEL) !! Land-element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NLF + 1:NEL) !! Land-element length.
      DOUBLE PRECISION, INTENT(INOUT) :: THSAT(NS)     !! Saturated water content by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: ZBFULL(NLFEE) !! Bankfull elevation/depth by link.

      ! Input/output arguments
      INTEGER, INTENT(INOUT) :: NSED !! Number of sediment size classes.
      DOUBLE PRECISION, INTENT(INOUT) :: PBSED(NLFEE)   !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(INOUT) :: PLS(NLF + 1:NEL) !! Loose-sediment porosity by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: SOSDFN(NSEE, NSEDEE) !! Soil sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: ARBDEP(NLFEE) !! Accumulated channel-bed elevation/depth change.
      DOUBLE PRECISION, INTENT(INOUT) :: DLS(NEL)      !! Loose/bed sediment depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: DCBED(NLFEE)  !! Active upper-bed layer depth by link.
      DOUBLE PRECISION, INTENT(INOUT) :: DCBSED(NLFEE, NSEDEE) !! Upper-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA(NELEE, NSEDEE)  !! Sediment composition fraction by element and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FDEL(NELEE, NSEDEE)   !! Mobile sediment concentration fraction by element and size class.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: GINFD(NLFEE, NSEDEE) !! Fine infiltration diagnostic/source for deposited material.
      DOUBLE PRECISION, INTENT(OUT) :: GINFS(NLFEE, NSEDEE) !! Fine infiltration diagnostic/source for suspended material.
      DOUBLE PRECISION, INTENT(OUT) :: GNU(NLF + 1:NEL) !! Hillslope erosion rate by land element.
      DOUBLE PRECISION, INTENT(OUT) :: GNUBK(NLFEE)     !! Lateral bank erosion rate by link.
      DOUBLE PRECISION, INTENT(OUT) :: QSED(NELEE, NSEDEE, 4) !! Sediment flux by element, size class, and face.

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT)      :: IDUM  !! Integer workspace for checks and reads.
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY !! Floating-point workspace for checks and reads.

      ! Locals, etc
      CHARACTER(LEN=*), PARAMETER :: SYVER = '4.2.7'

      INTEGER :: FACE, FADJ, I, IADJ, IB, IBR, IEL, N, P, SED, SOIL

      DOUBLE PRECISION :: DTSY
      DOUBLE PRECISION :: CONCIE(NSEDEE)
      DOUBLE PRECISION :: DCBSEE(NSEDEE), DCIPRE(NSEDEE)
      DOUBLE PRECISION :: DDBSEE(NSEDEE)
      DOUBLE PRECISION :: DDIPRE(NSEDEE)
      DOUBLE PRECISION :: FBETAE(NSEDEE), FCC(NVEE), FDELE(NSEDEE)
      DOUBLE PRECISION :: GINFDE(NSEDEE), GINFSE(NSEDEE)
      DOUBLE PRECISION :: QSDWAE(NSEDEE, 4), QSEDE(NSEDEE, 4)
      DOUBLE PRECISION :: QWAT(4)
      DOUBLE PRECISION :: SLOPEE(4), SOSDFE(NSEDEE)
      DOUBLE PRECISION :: TAUJE(4)

      LOGICAL :: DOUBT

      !----------------------------------------------------------------------*

      PASS_symain = PASS_symain + 1
      IF (PASS_symain == 1) THEN

         ! --------------------- Initialization step ----------------------------*

         CALL INITIALISE_SYMAIN_WORKSPACE()

         ! * Check array bounds & input variables
         CALL SYERR0(NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE, NV, NVEE, NX, NXEE, NY, &
                     SPR, SYD)

         ! * Check static/initializing input arrays
         CALL SYERR1(NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, NXEE, NYEE, NY, SPR, BEXBK, &
                     LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, NTSOIL, NVC, THSAT, CLENTH, &
                     CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, ARXL, HRF(NLF + 1), ZGRUND, IDUM, &
                     IDUM1X, LDUM)

         ! * Store top-layer soil type for each column element
         DO IEL = NLF + 1, NEL
            NTSOTP_symain(IEL) = NTSOIL(IEL, NLYR(IEL))
         END DO

         ! * Read SY input data file
         CALL SYREAD(BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, NELEE, NLF, NLFEE, NS, NSEDEE, &
                     NSEE, NSYBEE, NSYCEE, NTSOTP_symain(NLF + 1), NV, NX, NXEE, NYEE, NY, &
                     SPR, SYD, SYVER, ABC_symain, ALPHA_symain, BBC_symain, BKB_symain, &
                     CONCOB_symain, DCBEDO_symain, DLS, DRDRIP_symain, DRSED_symain, &
                     DLSMAX_symain, FBETA, FBIC_symain, FCG_symain(NLF + 1), &
                     FCROCK_symain(NLF + 1), FDEL, FDRIP_symain, FICRIT_symain, FPCLAY_symain, &
                     FPCRIT_symain, GBC_symain, GKF_symain, GKR_symain, ISACKW_symain, &
                     ISGSED_symain, ISSYOK_symain, ISTEC_symain, ISUSED_symain, NEPS_symain, &
                     NFINE_symain, NSED, NSYB_symain, NSYBCD_symain, NSYC_symain, &
                     NTSOBK_symain, PBSED, PLS, RHOSO_symain, SOSDFN, XDRIP_symain, IDUM, &
                     DUMMY, DUMSED)

         ! * Check SY input data
         CALL SYERR2(NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV, &
                     NSYB_symain, NSYBEE, NSYC_symain, NSYCEE, SPR, ICMREF, ISUSED_symain, &
                     NEPS_symain, NFINE_symain, SFB, SRB, ALPHA_symain, DCBEDO_symain, &
                     FPCRIT_symain, DLSMAX_symain, NTSOBK_symain, NSYBCD_symain, NBFACE, &
                     DRSED_symain, BKB_symain, GKF_symain, GKR_symain, RHOSO_symain, SOSDFN, &
                     DRDRIP_symain, FDRIP_symain, XDRIP_symain, PBSED, FCG_symain(NLF + 1), &
                     FCROCK_symain(NLF + 1), PLS, DLS, FBETA, FDEL, ABC_symain, BBC_symain, &
                     GBC_symain, IDUM, DUMMY, LDUM)

         ! * Static variables and initialization
         CALL SYINIT(NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, NTSOBK_symain, ARXL, &
                     DCBEDO_symain, DLS, FBETA, DRSED_symain, HRF(NLF + 1), PBSED, PLS, &
                     SOSDFN, THSAT, ZGRUND, NTSOTP_symain(NLF + 1), ZBFULL, ARBDEP, &
                     ARXLOL_symain, DCBED, DCBSED, DDBSED_symain, DRSO50_symain, &
                     DWATOL_symain(NLF + 1), FETA_symain, GINFD, GINFS, GNU, GNUBK, QSED, &
                     DBFULL_symain)

         !------------------- End of initialization step -----------------------*

      ELSE
         !---------------------- Simulation step -------------------------------*

         ! Check Input
         ! -----------
         ! * Check time-varying input variables
         DOUBT = ISSYOK_symain > 0
         IF (DOUBT) DOUBT = MOD(PASS_symain - 2, ISSYOK_symain) == 0

         IF (DOUBT) THEN
            CALL SYERR3(NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, ICMRF2, ISORT, DTUZ, CLAI, &
                        PLAI, ARXL, DRAINA, PNETTO, HRF, ZGRUND, QOC, IDUM, IDUM1A, IDUM1X, &
                        LDUM)
         END IF

         ! Quantities Independent of Sub-timestep
         ! --------------------------------------
         ! * Water-flow related variables
         CALL SYWAT(NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, DHF, DRDRIP_symain, &
                    LINKNS, ZBFULL, ZGRUND, CLAI, DRAINA, HRF, PLAI, PNETTO, QOC, &
                    DRDROP(NLF + 1), DWAT1, FCC, FQCONF, LRAIN(NLF + 1), SLOPEJ, TAUJ, TAUK)

         ! * Erosion rates for all column elements
         CALL SYOVER(ISTEC_symain, NEL, NLF, NS, NV, FCC, LRAIN(NLF + 1), XDRIP_symain, &
                     DRDRIP_symain, FDRIP_symain, DRAINA, GKR_symain, DWAT1(NLF + 1), &
                     DRDROP(NLF + 1), FCG_symain(NLF + 1), FCROCK_symain(NLF + 1), &
                     DRSO50_symain, TAUK(NLF + 1), FPCLAY_symain, GKF_symain, RHOSO_symain, &
                     NTSOTP_symain(NLF + 1), NVC, GNU, DUMMY, DLS, DLSMAX_symain)

         ! * Erosion rates for all link elements
         IF (NLF > 0) THEN
            CALL SYBKER(ISTEC_symain, NLF, NS, FPCLAY_symain, RHOSO_symain, DRSO50_symain, &
                        TAUK, CWIDTH, DWAT1, BKB_symain, NTSOBK_symain, FETA_symain, CLENTH, &
                        DBFULL_symain, EPSB, GNUBK)
         END IF

         ! SY Sub-timestep Loop
         ! --------------------
         DTSY = DTUZ/NEPS_symain
         DO N = 1, NEPS_symain

            ! Initialization
            ! --------------
            ! Replaced ALINIT with array slices
            DO FACE = 1, 4
               DO SED = 1, NSED
                  QSED(1:NEL, SED, FACE) = ZERO
               END DO
            END DO

            ! Boundary Conditions
            ! -------------------

            IF (NSYB_symain > 0) THEN

               ! * Gather water "outflow" rates (should be negative)
               DO IB = 1, NSYB_symain
                  IEL = NSYBCD_symain(IB, 1)
                  FACE = NBFACE(IEL)
                  QWATB(IB) = SIGN(1, 2 - FACE)*QOC(IEL, FACE)
               END DO

               ! * Read time-varying flux data & calculate sediment flows
               CALL SYBC

               ! * Load boundary flows into QSED array
               DO IB = 1, NSYB_symain
                  IEL = NSYBCD_symain(IB, 1)
                  FACE = NBFACE(IEL)
                  CALL DCOPY(NSED, QSEDB(1, IB), 1, QSED(IEL, 1, FACE), NELEE)
               END DO

            END IF

            ! Quantities Independent of Sediment Flux
            ! ---------------------------------------
            IF (NLF > 0) THEN
               ! * Transport capacity & advection coefficients
               CALL SYCLTR(CONCOB_symain, FPCRIT_symain, ISACKW_symain, ISUSED_symain, NELEE, &
                           NFINE_symain, NLF, NLFEE, NSED, NSEDEE, &
                           DRSED_symain(NFINE_symain + 1), ARXL, CWIDTH, DCBED, LINKNS, DWAT1, &
                           QOC, SLOPEJ, DCBSED(1, NFINE_symain + 1), &
                           FDEL(1, NFINE_symain + 1), TAUJ, ACKW_symain(1, NFINE_symain + 1), &
                           CONCI, QSDWAT, DUMMY, DUMSED)

               ! * Settling, infiltration & armouring
               IF (NFINE_symain > 0) THEN
                  CALL SYFINE(DRSED_symain(1), FBIC_symain, FICRIT_symain, NLF, ALPHA_symain, &
                              DTSY, AREA, DCBSED, FBETA, FDEL, PBSED, TAUK, VCFMAX, VINFMX, &
                              BARM)
               END IF
            END IF

            ! One Element at a Time
            ! ---------------------
            DO I = 1, NEL
               IEL = ISORT(I)

               ! * Gather common sub-arrays
               CALL DCOPY(NSED, FDEL(IEL, 1), NELEE, FDELE, 1)
               DO FACE = 1, 4
                  QWAT(FACE) = SIGN(1, 2 - FACE)*QOC(IEL, FACE)
                  QSEDE(1:NSED, FACE) = QSED(IEL, 1:NSED, FACE)
               END DO

               IF (IEL <= NLF) THEN
                  ! ** Link element **
                  ! * Gather link-specific sub-arrays
                  SOIL = NTSOBK_symain(IEL)
                  CALL DCOPY(NSED, SOSDFN(SOIL, 1), NSEE, SOSDFE, 1)
                  CALL DCOPY(NSED, CONCI(IEL, 1), NLFEE, CONCIE, 1)
                  CALL DCOPY(NSED, DCBSED(IEL, 1), NLFEE, DCBSEE, 1)
                  CALL DCOPY(NSED, DDBSED_symain(IEL, 1), NLFEE, DDBSEE, 1)
                  DO FACE = 1, 4
                     CALL DCOPY(NSED, QSDWAT(IEL, 1, FACE), NLFEE, QSDWAE(1, FACE), 1)
                  END DO

                  ! * Solve transport equation
                  CALL SYLINK(NFINE_symain, NSED, NSEDEE, DTSY, AREA(IEL), &
                              ARXLOL_symain(IEL), ARXL(IEL), CLENTH(IEL), EPSB(IEL), &
                              PBSED(IEL), VINFMX(IEL), BARM(IEL), VCFMAX(IEL), CONCIE, &
                              DCBSEE, DDBSEE, QSDWAE, QWAT, SOSDFE, FDELE, QSEDE, DCIPRE, &
                              DDIPRE, GINFDE, GINFSE)

                  ! * Scatter link-specific results
                  CALL DCOPY(NSED, DCIPRE, 1, DCIPRM(IEL, 1), NLFEE)
                  CALL DCOPY(NSED, DDIPRE, 1, DDIPRM(IEL, 1), NLFEE)
                  CALL DCOPY(NSED, GINFDE, 1, GINFD(IEL, 1), NLFEE)
                  CALL DCOPY(NSED, GINFSE, 1, GINFS(IEL, 1), NLFEE)

               ELSE
                  ! ** Column element **
                  ! * Gather column-specific sub-arrays
                  SOIL = NTSOTP_symain(IEL)
                  CALL DCOPY(NSED, SOSDFN(SOIL, 1), NSEE, SOSDFE, 1)
                  CALL DCOPY(NSED, FBETA(IEL, 1), NELEE, FBETAE, 1)
                  CALL DCOPY(4, SLOPEJ(IEL, 1), NELEE, SLOPEE, 1)
                  CALL DCOPY(4, TAUJ(IEL, 1), NELEE, TAUJE, 1)

                  ! * Solve transport equation for this column element
                  CALL SYCOLM(AREA(IEL), DTSY, DWAT1(IEL), DWATOL_symain(IEL), DXQQ(IEL), &
                              DYQQ(IEL), FETA_symain(IEL), GNU(IEL), ISGSED_symain, NSED, &
                              FPCRIT_symain, PLS(IEL), NSEDEE, DRSED_symain, QWAT, SLOPEE, &
                              SOSDFE, TAUJE, DLS(IEL), FBETAE, FDELE, QSEDE, DUMMY, DUMSED)

                  ! * Scatter column-specific results
                  CALL DCOPY(NSED, FBETAE, 1, FBETA(IEL, 1), NELEE)
               END IF

               ! * Scatter common results ...
               CALL DCOPY(NSED, FDELE, 1, FDEL(IEL, 1), NELEE)
               DO FACE = 1, 4
                  CALL DCOPY(NSED, QSEDE(1, FACE), 1, QSED(IEL, 1, FACE), NELEE)

                  ! ... and propagate sediment flow rates at outflow faces
                  IF (QWAT(FACE) > ZERO) THEN
                     IADJ = ICMREF(IEL, FACE, 2)

                     IF (IADJ > 0) THEN
                        ! * regular neighbour
                        FADJ = ICMREF(IEL, FACE, 3)
                        DO SED = 1, NSED
                           QSED(IADJ, SED, FADJ) = -QSEDE(SED, FACE)
                        END DO

                     ELSE IF (IADJ < 0) THEN
                        ! * neighbour is a confluence node
                        IBR = -IADJ
                        DO P = 1, 3
                           IADJ = ICMRF2(IBR, P, 1)
                           IF (IADJ > 0) THEN
                              ! * prospect is active
                              FADJ = ICMRF2(IBR, P, 2)
                              DO SED = 1, NSED
                                 QSED(IADJ, SED, FADJ) = QSED(IADJ, SED, FADJ) - &
                                                         QSEDE(SED, FACE)*FQCONF(IBR, P)
                              END DO
                           END IF
                        END DO
                     END IF

                  END IF
               END DO

            END DO

            ! Channel Bed Update
            ! ------------------
            IF (NLF > 0) THEN
               CALL SYBED(DCBEDO_symain, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, DDIPRM, &
                          ARBDEP, DLS, FBETA, DCBSED, DDBSED_symain, DCBED)
            END IF

            ! Store Old-time Values & Update Timer
            ! ------------------------------------
            CALL DCOPY(NEL - NLF, DWAT1(NLF + 1), 1, DWATOL_symain(NLF + 1), 1)
            IF (NLF > 0) CALL DCOPY(NLF, ARXL, 1, ARXLOL_symain, 1)

            SYNOW_symain = SYNOW_symain + DTSY/3600.0D0

         END DO

         !--------------------- End of simulation step -------------------------*
      END IF

      ! Epilogue
      ! --------
      ! Ensure that current time value is exactly correct
      SYNOW_symain = UZNOW

   END SUBROUTINE SYMAIN

!> Placeholder for sediment mass-balance output.
!>
!> Sediment process state is updated by [[symain]], but this routine currently
!> performs no accumulation, checking, state mutation, or reporting. It is
!> called from the main simulation loop only to preserve the historical component
!> interface for sediment balances.
   SUBROUTINE BALSED
   end subroutine BALSED

END MODULE sy_driver

