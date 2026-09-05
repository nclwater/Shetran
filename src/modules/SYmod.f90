!> summary: SHETRAN sediment erosion, transport, deposition, and bed updating.
!>
!> The `SY` component is the optional sediment module described in section 2.8
!> of the SHETRAN User Guide and Data Input Manual. It reads the `SY01`-`SY64`
!> data groups, checks their consistency, initialises loose hillslope sediment
!> and the two channel-bed layers, derives hydraulic quantities from the water
!> modules, calculates hillslope and bank erosion, evaluates transport capacity,
!> routes each sediment size fraction, and updates mobile concentration,
!> loose-sediment, bed-sediment, infiltration, and output arrays.
!>
!> The code uses the manual's non-dimensional mobile concentration `FDEL` rather
!> than storing concentration directly. Appendix B defines, for size fraction
!> \(i\),
!>
!> \[
!>   c_i = FDEL_i \rho
!> \]
!>
!> where \(c_i\) is mass concentration in the water column and \(\rho\) is the
!> bulk density of the material after hypothetical settling: channel bed
!> sediment for links (`PBSED`) and loose hillslope sediment for land elements
!> (`PLS`). In the code this convention appears in arrays such as `FDEL`,
!> `FBETA`, `DLS`, `DCBSED`, and `DDBSED`.
!>
!> Important manual switches are implemented as follows:
!>
!> | Switch | Manual meaning | Implemented code path |
!> |:-------|:---------------|:----------------------|
!> | `ISGSED = 0` | Yalin overland-flow capacity | [[syovtr]] Yalin branch, using median available diameter and [[sycrit]]. |
!> | `ISGSED = 1` | Engelund-Hansen overland-flow capacity | [[syovtr]] stream-power branch. |
!> | other `ISGSED` | zero overland transport capacity | [[syovtr]] leaves capacity at zero. |
!> | `ISTEC = 1` | critical shear from fractional clay content | [[sycrit]] quick clay-content relation. |
!> | other `ISTEC` | Shields formula | [[sycrit]] piecewise Shields-style relation. |
!> | `ISACKW = 0` | Engelund-Hansen channel capacity | [[syengh]] in [[sycltr]]. |
!> | `ISACKW = 1` | Ackers-White channel capacity | [[syackw]] in [[sycltr]]. |
!> | `ISACKW = 2` | Ackers-White-Day channel capacity | [[syackw]] with bed-percentile modification. |
!> | `ISUSED = 0` | non-fines move at water speed | [[sycltr]] velocity assignment. |
!> | `ISUSED = 1` | non-fines may move slower than water | [[sycltr]] shear-dependent velocity assignment. |
!>
!> Sediment is represented by `NSED` size groups in increasing diameter
!> (`DRSED`). The manual restricts `NFINE` to 0 or 1; when `NFINE = 1`, the
!> smallest size group is treated as fine material. Fine sediment is capacity
!> limited in channels by `FPCRIT`, always travels at the water velocity, may
!> infiltrate into the bed after settling, and may be protected from
!> resuspension by armouring. Non-fine channel capacity is calculated by
!> Ackers-White, Ackers-White-Day, or Engelund-Hansen as selected by `ISACKW`.
!> Overland capacity uses the Yalin or Engelund-Hansen option selected by
!> `ISGSED`; the manual notes that these formulae were derived for
!> non-cohesive channel transport and their suitability for rainfall-driven
!> overland flow is uncertain.
!>
!> Key state and limiting parameters follow the manual definitions:
!>
!> | Parameter | Role in this module |
!> |:----------|:--------------------|
!> | `FPCRIT` | Maximum `FDEL` for each channel size group and maximum total overland `FDEL`. |
!> | `DLSMAX` | Hillslope loose-sediment depth at which underlying soil erosion is suppressed. |
!> | `DCBEDO` | Active upper channel-bed thickness controlling exchange with the lower layer. |
!> | `ALPHA` | Ratio of fine-sediment settling to resuspension critical shear stress. |
!> | `FBIC`, `FICRIT` | Fine-sediment bed-fraction and concentration thresholds controlling infiltration. |
!> | `CONCOB` | Mobile concentration threshold used for overbank sediment exchange. |
!>
!> Programmer's map:
!>
!> | Routine | Main responsibility |
!> |:--------|:--------------------|
!> | [[symain]] | Top-level time-step driver for reading, initialisation, erosion, transport, routing, and outputs. |
!> | [[syread]] / [[syerr1]]-[[syerr3]] | Read and validate `SY` input data. |
!> | [[syinit]] / [[sywat]] | Initialise sediment state and derive water-dependent geometry, slopes, flows, and shear stresses. |
!> | [[syover]] / [[sybker]] | Calculate ground-surface and channel-bank erosion. |
!> | [[sycolm]] / [[sylink]] | Route sediment through land elements and channel links. |
!> | [[sycltr]], [[syackw]], [[syengh]], [[syovtr]], [[sycrit]] | Compute capacity, velocities, and critical shear stress. |
!> | [[sybed]] / [[syfine]] | Update two-layer channel bed storage, fine-sediment settling, infiltration, and armouring limits. |
!> | [[initialise_symain_workspace]] | Allocates [[symain]]'s heap work arrays once, on its first call. |
!>
!> @warning
!> The manual defines sediment boundary-condition input groups `SY61`-`SY64`,
!> but explicitly states that the sediment boundary-condition routines have not
!> yet been implemented. This matches the current empty [[sybc]] routine.
!> Sediment mass-balance output is also still a placeholder in [[balsed]].
!> @endwarning
!>
!> @note
!> The module-level `LOGICAL :: FIRST_syackw` declared below is no longer read
!> or written anywhere in this file. The one-time Ackers-White constant setup
!> it used to guard (`K2_syackw`, `DGRMAX_syackw`, `ROOT32_syackw`) was
!> converted to compile-time `PARAMETER`s local to [[syackw]] because the
!> constants it depends on (`GRAVTY`, `RHOSED`, `RHOWAT`, `VISCOS` from
!> `CONST_SY`) are themselves `PARAMETER`s. `FIRST_syackw` is dead state left
!> behind by that change.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into this Fortran 90 module. |
!> | 2026-03-19 | SB | 4.6 | Updated `NTSOIL` dimensions for current array layout. |
!> | 2026-04-03 | SvB | 4.6.1 | Reformatted the whole file to modern free-form indentation, added `IMPLICIT NONE`/`INTENT` attributes throughout, and corrected a `USE` import pointing at the wrong module. |
!> | 2026-04-05 | SvB | 4.6.1 | Removed the `ALINIT` helper entirely, replacing every zero/constant array initialisation with a Fortran array-slice assignment; briefly tried replacing `DCOPY` calls with array slices too, reverted the same day after a measured performance regression, then restored `DCOPY` only where it still mattered. |
!> | 2026-04-06 | SvB | 4.6.1 | Removed all `GOTO`-driven control flow in favour of structured `IF`/`DO`/`CYCLE`/`EXIT` blocks. |
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime-cached "first call" constant setup in [[syackw]], [[sycltr]], [[sycrit]], and [[syengh]] with compile-time `PARAMETER`s (safe because the underlying `CONST_SY` constants are themselves `PARAMETER`s), and replaced several `DIMJE`/unguarded-exponentiation patterns with branchless `MAX`/`SIGN` forms for performance, removing the resulting unused-variable warnings. |
!> | 2026-04-12 to 2026-04-13 | SvB | 4.6.1 | Further modernisation, and removal of the remaining labelled `DO` loops; legacy statement functions `FDGR`/`FA` ([[syackw]]), `FNQOUT` ([[syerr3]]), and `FQOUT` ([[sywat]]) were replaced by internal `FUNCTION`s. |
!> | 2026-05-02 to 2026-05-03 | SvB | 4.6.1 | Post-merge fixes, including restoring the [[balsed]] placeholder and its `PUBLIC` export; replaced an uninitialised local `IUNDEF` "don't care" argument to `ALCHK`/`ALCHKI` in [[syerr1]], [[syerr2]], and [[syerr3]] with an explicit `PARAMETER = 0`, avoiding an uninitialised-variable diagnostic under gfortran. |
!> | 2026-05-04 to 2026-05-10 | SvB | 4.6.1 | Moved [[symain]]'s large work arrays from automatic (stack) local arrays to module-level `ALLOCATABLE` storage allocated once by [[initialise_symain_workspace]], fixing a stack-overflow crash under Windows; an intermediate state that re-`ALLOCATE`d these arrays on every call was corrected by adding the allocate-once guard. |
!> @endhistory
MODULE SYmod
   USE SGLOBAL
!USE AL_P
   USE mod_load_filedata, ONLY : ALCHKI, ALCHK, ALALLF, ALREAD

   USE MOD_PARAMETERS, ONLY : I_P
   USE MOD_ERROR, ONLY : errstat_alloc, RAISE_ERROR, ERRLVL_fatal, ERRLVL_error, ERRLVL_warn, FID_logfile

   USE UTILSMOD, ONLY : DCOPY
   USE CONST_SY

   IMPLICIT NONE

   LOGICAL         :: FIRST_syackw=.TRUE. !! Unused; see the module-level note above `MODULE SYmod`.

   LOGICAL         :: FIRST_syfine=.TRUE. !! True until the fine-sediment settling velocity `WSED_syfine` has been cached.
   DOUBLEPRECISION :: WSED_syfine         !! Cached fine-sediment settling velocity, set on the first call to [[syfine]].


   INTEGER, PARAMETER  :: NSYBEE= 40       !! Maximum number of sediment boundary entries.
   INTEGER, PARAMETER  :: NSYCEE=10        !! Maximum number of sediment boundary categories.
   INTEGER          :: ISACKW_symain       !! Channel transport-capacity option.
   INTEGER          :: ISGSED_symain       !! Overland transport-capacity option.
   INTEGER          :: ISSYOK_symain       !! Dynamic sediment input-check interval.
   INTEGER          :: ISTEC_symain        !! Critical-shear calculation option.
   INTEGER          :: ISUSED_symain       !! Sediment velocity option.
   INTEGER          :: NEPS_symain         !! Number of sediment substeps per water timestep.
   INTEGER          :: NFINE_symain        !! Number of fine sediment classes; manual allows 0 or 1.
   INTEGER          :: NSYB_symain         !! Number of sediment boundary entries.
   INTEGER          :: NSYBCD_symain(NSYBEE,3) !! Sediment boundary element, type, and category metadata.
   INTEGER          :: NSYC_symain(4)      !! Number of sediment boundary categories by boundary type.
   INTEGER          :: NTSOBK_symain(NLFEE) !! Bank soil type by channel link.
   INTEGER          :: PASS_symain=0       !! Saved call counter for sediment setup/timestep control.
   INTEGER          :: NTSOTP_symain(NELEE) !! Top soil type by element.
   DOUBLEPRECISION  :: ALPHA_symain        !! Fine-sediment settling/resuspension critical-shear ratio.
   DOUBLEPRECISION  :: CONCOB_symain       !! Mobile concentration threshold for overbank exchange.
   DOUBLEPRECISION  :: DCBEDO_symain       !! Active upper channel-bed layer thickness.
   DOUBLEPRECISION  :: FBIC_symain         !! Fine-bed fraction threshold for infiltration.
   DOUBLEPRECISION  :: FICRIT_symain       !! Fine-concentration threshold for infiltration.
   DOUBLEPRECISION  :: FPCRIT_symain       !! Maximum sediment concentration fraction.
   DOUBLEPRECISION  :: SYNOW_symain        !! Current sediment simulation time.
   DOUBLEPRECISION  :: DLSMAX_symain       !! Loose-sediment depth above which hillslope soil erosion is suppressed.
   DOUBLEPRECISION  :: DDBSED_symain(NLFEE, NSEDEE) !! Lower channel-bed sediment depth by link and size class.
   DOUBLEPRECISION  :: ABC_symain(NSEDEE, NSYCEE)   !! Boundary rating-curve coefficient `A` by sediment class/category.
   DOUBLEPRECISION  :: ACKW_symain(5, NSEDEE)       !! Ackers-White cached coefficients by sediment class.
   DOUBLEPRECISION  :: ARXLOL_symain(NLFEE)         !! Previous channel cross-sectional area by link.
   DOUBLEPRECISION  :: BBC_symain(NSEDEE, NSYCEE)   !! Boundary rating-curve coefficient `B` by sediment class/category.
   DOUBLEPRECISION  :: BKB_symain(NSEE)      !! Channel-bank erodibility by soil type.
   DOUBLEPRECISION  :: DBFULL_symain(NLFEE)  !! Bankfull depth by channel link.
   DOUBLEPRECISION  :: DRDRIP_symain(NVEE)   !! Canopy drip drop diameter by vegetation type.
   DOUBLEPRECISION  :: DRSED_symain(NSEDEE)  !! Representative sediment particle diameter by size class.
   DOUBLEPRECISION  :: DRSO50_symain(NSEE)   !! Median soil particle diameter by soil type.
   DOUBLEPRECISION  :: DWATOL_symain(NELEE)  !! Previous surface/channel water depth by element.
   DOUBLEPRECISION  :: FCG_symain(NELEE)     !! Ground-cover fraction by element.
   DOUBLEPRECISION  :: FCROCK_symain(NELEE)  !! Rock-cover fraction by element.
   DOUBLEPRECISION  :: FDRIP_symain(NVEE)    !! Canopy drip fraction by vegetation type.
   DOUBLEPRECISION  :: FETA_symain(NELEE)    !! Soil-to-sediment solid-volume conversion factor by element.
   DOUBLEPRECISION  :: FPCLAY_symain(NSEE)   !! Clay fraction by soil type.
   DOUBLEPRECISION  :: GBC_symain(NSEDEE, NSYCEE) !! Steady boundary sediment input by class/category.
   DOUBLEPRECISION  :: GKF_symain(NSEE)      !! Flow detachment coefficient by soil type.
   DOUBLEPRECISION  :: GKR_symain(NSEE)      !! Rainfall detachment coefficient by soil type.
   DOUBLEPRECISION  :: RHOSO_symain(NSEE)    !! Soil bulk density by soil type.
   DOUBLEPRECISION  :: XDRIP_symain(NVEE)    !! Canopy drip fall height by vegetation type.

   ! [[symain]] work arrays, allocated once by [[initialise_symain_workspace]].
   ! Named as they were when they were SYMAIN's own local variables, so unlike
   ! the scalars above they do not carry a "_symain" suffix.
   INTEGER, ALLOCATABLE :: IDUM1A (:) !! Integer workspace for [[syerr3]].
   INTEGER, ALLOCATABLE :: IDUM1X (:) !! Integer workspace for [[syerr1]].
   DOUBLE PRECISION, ALLOCATABLE :: CONCI (:, :)  !! Capacity concentration by link and sediment class, from [[sycltr]].
   DOUBLE PRECISION, ALLOCATABLE :: DCIPRM (:, :) !! Interim upper-bed sediment depth by link/class, from [[sylink]].
   DOUBLE PRECISION, ALLOCATABLE :: DDIPRM (:, :) !! Interim lower-bed sediment depth by link/class, from [[sylink]].
   DOUBLE PRECISION, ALLOCATABLE :: DRDROP (:) !! Effective raindrop/drip diameter by land element, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: DUMSED (:) !! Sediment-sized floating-point workspace passed to [[sycltr]]/[[sycolm]].
   DOUBLE PRECISION, ALLOCATABLE :: DWAT1 (:)  !! Surface/channel water depth by element, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: EPSB (:)   !! Bank erosion sediment source by link, from [[sybker]].
   DOUBLE PRECISION, ALLOCATABLE :: FQCONF (:, :) !! Confluence outflow fractions for receiving branches, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: LRAIN (:)  !! Effective direct rainfall rate by land element, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: QSDWAT (:, :, :) !! Sediment advection coefficient for outflow faces, from [[sycltr]].
   DOUBLE PRECISION, ALLOCATABLE :: QSEDB (:, :) !! Boundary sediment flow by class and boundary entry, from [[sybc]].
   DOUBLE PRECISION, ALLOCATABLE :: QWATB (:)    !! Boundary water outflow rate by boundary entry.
   DOUBLE PRECISION, ALLOCATABLE :: SLOPEJ (:, :) !! Face water-surface slope, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: TAUJ (:, :)   !! Face shear stress, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: TAUK (:)      !! Representative element/link shear stress, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: VCFMAX (:) !! Maximum fine volume available for settling/infiltration, from [[syfine]].
   DOUBLE PRECISION, ALLOCATABLE :: VINFMX (:) !! Maximum fine infiltration volume, from [[syfine]].
   LOGICAL, ALLOCATABLE :: BARM (:) !! True where fine sediment is protected by bed armouring, from [[syfine]].
   LOGICAL, ALLOCATABLE :: LDUM (:) !! Logical workspace for `ALCHK`/`ALCHKI` checks in [[syerr1]]-[[syerr3]].

   DOUBLE PRECISION, PARAMETER :: K1_syovtr = 0.05D0 * RHOWAT**2 / ((RHOSED - RHOWAT)**2 * SQRT(GRAVTY)) !! Engelund-Hansen overland-capacity coefficient.
   DOUBLE PRECISION, PARAMETER :: K3_syovtr = 2.45D0 * (RHOSED / RHOWAT)**(-0.4D0) / SQRT((RHOSED - RHOWAT) * GRAVTY) !! Yalin overland-capacity coefficient.
   DOUBLE PRECISION, PARAMETER :: K4_syovtr = 0.635D0 / SQRT(RHOWAT) !! Yalin overland-capacity coefficient.

   PRIVATE

   PUBLIC :: SYMAIN, issyok_symain, balsed

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
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
   SUBROUTINE INITIALISE_SYMAIN_WORKSPACE()
      IMPLICIT NONE

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "SYmod:initialise_symain_workspace"

      IF (.NOT. ALLOCATED(BARM)) THEN
         ALLOCATE (BARM (NLFEE), STAT=ios)
         CALL errstat_alloc(ios, "BARM", location)
         ALLOCATE (CONCI (NLFEE, NSEDEE), STAT=ios)
         CALL errstat_alloc(ios, "CONCI", location)
         ALLOCATE (DCIPRM (NLFEE, NSEDEE), STAT=ios)
         CALL errstat_alloc(ios, "DCIPRM", location)
         ALLOCATE (DDIPRM (NLFEE, NSEDEE), STAT=ios)
         CALL errstat_alloc(ios, "DDIPRM", location)
         ALLOCATE (DRDROP (NELEE), STAT=ios)
         CALL errstat_alloc(ios, "DRDROP", location)
         ALLOCATE (DUMSED (NLFEE * NSEDEE), STAT=ios)
         CALL errstat_alloc(ios, "DUMSED", location)
         ALLOCATE (DWAT1 (NELEE), STAT=ios)
         CALL errstat_alloc(ios, "DWAT1", location)
         ALLOCATE (EPSB (NLFEE), STAT=ios)
         CALL errstat_alloc(ios, "EPSB", location)
         ALLOCATE (FQCONF (NLFEE, 3), STAT=ios)
         CALL errstat_alloc(ios, "FQCONF", location)
         ALLOCATE (IDUM1A (NELEE), STAT=ios)
         CALL errstat_alloc(ios, "IDUM1A", location)
         ALLOCATE (IDUM1X (NELEE + 3), STAT=ios)
         CALL errstat_alloc(ios, "IDUM1X", location)
         ALLOCATE (LDUM (NELEE), STAT=ios)
         CALL errstat_alloc(ios, "LDUM", location)
         ALLOCATE (LRAIN (NELEE), STAT=ios)
         CALL errstat_alloc(ios, "LRAIN", location)
         ALLOCATE (QSDWAT (NLFEE, NSEDEE, 4), STAT=ios)
         CALL errstat_alloc(ios, "QSDWAT", location)
         ALLOCATE (QSEDB (NSEDEE, NSYBEE), STAT=ios)
         CALL errstat_alloc(ios, "QSEDB", location)
         ALLOCATE (QWATB (NSYBEE), STAT=ios)
         CALL errstat_alloc(ios, "QWATB", location)
         ALLOCATE (SLOPEJ (NELEE, 4), STAT=ios)
         CALL errstat_alloc(ios, "SLOPEJ", location)
         ALLOCATE (TAUJ (NELEE, 4), STAT=ios)
         CALL errstat_alloc(ios, "TAUJ", location)
         ALLOCATE (TAUK (NELEE), STAT=ios)
         CALL errstat_alloc(ios, "TAUK", location)
         ALLOCATE (VCFMAX (NLFEE), STAT=ios)
         CALL errstat_alloc(ios, "VCFMAX", location)
         ALLOCATE (VINFMX (NLFEE), STAT=ios)
         CALL errstat_alloc(ios, "VINFMX", location)
      END IF

   END SUBROUTINE INITIALISE_SYMAIN_WORKSPACE



!> Calculates Ackers-White channel transport capacity for non-fine sediment.
!>
!> This is the `ISACKW = 1` or `ISACKW = 2` channel-capacity option described
!> in the sediment manual. It is called by [[sycltr]] only for non-fine size
!> groups (`NFINE+1:NSED`); fine material is handled separately because the
!> manual limits fine channel capacity directly by `FPCRIT`.
!>
!> For a sediment diameter \(d\), relative submerged density
!> \(s - 1 = \rho_s / \rho_w - 1\), and kinematic viscosity \(\nu\), the
!> routine first forms the Ackers-White dimensionless grain size
!>
!> \[
!>   D_* = \max\left(1, \min\left(d\left({g(s-1)\over\nu^2}\right)^{1/3},
!>          10^{1/0.56}\right)\right).
!> \]
!>
!> For the standard Ackers-White option (`ISACKW = 1`), the per-size parameters
!> cached in `ACKW` are
!>
!> \[
!> \begin{aligned}
!>   n &= \max(1 - 0.56\log_{10}D_*, 0),\\
!>   A &= 0.14 + {0.23\over\sqrt{D_*}},\\
!>   m &= 1.34 + {9.66\over D_*},\\
!>   C &= 10^{(2.86-\log_{10}D_*)\log_{10}D_* - 3.53}.
!> \end{aligned}
!> \]
!>
!> For the Day modification (`ISACKW = 2`), the mobility threshold \(A\) is
!> recalculated for each link end from the current bed-size distribution:
!>
!> \[
!>   d_a = 1.62D_{50}\left({D_{16}\over D_{84}}\right)^{0.28},\qquad
!>   A_s = A(d_a)\left(0.6 + 0.4\sqrt{d_a/d_s}\right),
!> \]
!>
!> where \(D_{16}\), \(D_{50}\), and \(D_{84}\) are obtained from `DCBSED`
!> with [[sydr]], and \(d_s\) is the representative diameter of the current
!> size class.
!>
!> For each outflowing link end, with outflow \(Q\), water depth \(h\), mean
!> velocity \(U = Q/ARXL\), shear velocity \(u_*=\sqrt{\tau/\rho_w}\), and
!> \(U_g = U / \{\sqrt{32}\log_{10}(10h/d)\}\), the mobility and capacity are
!>
!> \[
!>   F = {u_*^n U_g^{1-n}\over\sqrt{g(s-1)d}},
!> \]
!>
!> \[
!>   G = d{Q\over h}C\max\left({F\over A}-1,0\right)^m
!>       \left({U\over u_*}\right)^n .
!> \]
!>
!> `GSED(link,sed)` accumulates \(G\) over the outflowing ends of each link.
!> Concentration caps, sediment velocity selection, and exchange with
!> suspended and bed material are applied later in [[sycltr]] and [[sylink]].
!>
!> @note Link ends are mapped onto `QOC` faces 1 and 3 for east-west links, or
!> faces 2 and 4 for north-south links. Only outflowing ends contribute to
!> `GSED`; non-outflowing ends leave the accumulated capacity unchanged.
!> @endnote
!>
!> @note The base of the capacity exponent \((F/A-1)\) is computed once as
!> `BASE` and the `**MAW` power and `GSED` accumulation are skipped entirely
!> when `BASE <= 0`. This is equivalent to the original
!> `DIMJE(F/A,1)**MAW`-based formula (which forces a zero contribution the
!> same way) but avoids evaluating a real power of a known-zero base.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-06 | SvB | 4.6.1 | Removed `GOTO`-driven control flow; replaced the legacy statement functions `FDGR`/`FA` with internal `FUNCTION`s. |
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `K2_syackw`/`DGRMAX_syackw`/`ROOT32_syackw` with compile-time `PARAMETER`s; skipped the capacity exponentiation/accumulation when its base is non-positive (see the preceding note on `BASE`). |
!> @endhistory
   PURE SUBROUTINE SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, DRSED, ARXL, DCBSED, DWAT1, &
      QOC, TAUJ, ACKW, GSED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISACKW !! Ackers-White option: 1 standard, 2 Day bed-percentile modification.
      INTEGER, INTENT(IN) :: NFINE  !! Number of fine sediment classes excluded from this calculation.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      LOGICAL, INTENT(IN)          :: LINKNS (NLF)                  !! True for north-south channel links.
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NFINE + 1:NSED)        !! Representative non-fine particle diameters.
      DOUBLE PRECISION, INTENT(IN) :: ARXL (NLF)                    !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1 (NLF)                   !! Channel water depth by link.
      DOUBLE PRECISION, INTENT(IN) :: QOC (NELEE, 4)                !! Face water fluxes.
      DOUBLE PRECISION, INTENT(IN) :: DCBSED (NLFEE, NFINE + 1:NSED) !! Active-bed sediment depth by link and non-fine class.
      DOUBLE PRECISION, INTENT(IN) :: TAUJ (NELEE, 4)               !! Face shear stress.

      ! In/Out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: ACKW (5, NFINE + 1:NSED) !! Cached Ackers-White parameters by non-fine class.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT)   :: GSED (NLF, NFINE + 1:NSED) !! Channel transport capacity by link and non-fine class.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: DGRSML = 1.0D-4
      DOUBLE PRECISION, PARAMETER :: F16 = 0.16D0, F50 = 0.5D0, F56 = 0.56D0, F84 = 0.84D0
      DOUBLE PRECISION, PARAMETER :: THIRD = 1.0D0 / 3.0D0

      DOUBLE PRECISION, PARAMETER :: KRHO = RHOSED / RHOWAT - 1.0D0
      DOUBLE PRECISION, PARAMETER :: K2_syackw = (GRAVTY * KRHO / VISCOS**2)**THIRD
      DOUBLE PRECISION, PARAMETER :: DGRMAX_syackw = 10.0D0**(ONE / F56) + DGRSML
      DOUBLE PRECISION, PARAMETER :: ROOT32_syackw = SQRT(32.0D0)
      DOUBLE PRECISION :: AAW, ARXLE, CAW, DAAA, DBED16, DBED50, DBED84, DGR
      DOUBLE PRECISION :: DSED, DWAT1E, FGR, G, H10, LGR, MAW
      DOUBLE PRECISION :: NAW, QK, UGR, USTR, UK, BASE
      INTEGER          :: FACE, IEND, LINK, NFP1, NNF, SED, SGN

      !----------------------------------------------------------------------*

      ! Initialization
      NNF = NSED - NFINE
      NFP1 = NFINE + 1

      DO SED = NFP1, NSED
         DGR = FDGR (DRSED (SED))
         LGR = LOG10 (DGR)
         ACKW (1, SED) = MAX (ZERO, ONE - F56 * LGR) ! Replaced DIMJE with standard intrinsic
         IF (ISACKW == 1) ACKW (2, SED) = FA (DGR)
         ACKW (3, SED) = 1.34D0 + 9.66D0 / DGR
         ACKW (4, SED) = 10.0D0**( (2.86D0 - LGR) * LGR - 3.53D0)
         ACKW (5, SED) = ONE / SQRT (GRAVTY * KRHO * DRSED (SED))
      END DO

      ! Zero GSED array slice
      GSED(:, :) = ZERO

      ! Loop over ends of each link
      DO IEND = 1, 3, 2
         SGN = 2 - IEND

         ! Loop over all channel links
         DO LINK = 1, NLF

            ! Determine face equivalent to this end, and flow rate there
            FACE = IEND
            IF (LINKNS (LINK)) FACE = FACE + 1
            QK = SGN * QOC (LINK, FACE)

            ! Check that this end is outflowing
            IF (QK > ZERO) THEN

               ! Copy array elements to local variables
               ARXLE = ARXL (LINK)
               DWAT1E = DWAT1 (LINK)
               H10 = 10.0D0 * DWAT1E

               ! Determine shear velocity and water flow velocity
               USTR = SQRT (TAUJ (LINK, FACE) / RHOWAT)
               UK = ZERO
               IF (ARXLE > ZERO) UK = QK / ARXLE

               ! Set A-W parameters for the Day modification if needed
               IF (ISACKW == 2) THEN

                  DBED84 = SYDR (F84, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))

                  IF (DBED84 > ZERO) THEN
                     DBED50 = SYDR (F50, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))
                     DBED16 = SYDR (F16, NLFEE, NNF, DCBSED (LINK, NFP1), DRSED (NFP1))
                     DAAA = 1.62D0 * DBED50 * (DBED16 / DBED84)**0.28D0
                  ELSE
                     DAAA = ZERO
                  END IF

                  DGR = FDGR (DAAA)
                  AAW = FA (DGR)

                  DO SED = NFP1, NSED
                     ACKW (2, SED) = AAW * (0.6D0 + 0.4D0 * SQRT (DAAA / DRSED (SED)))
                  END DO

               END IF

               ! Loop over sediment types
               DO SED = NFP1, NSED

                  ! Set A-W parameters for this Sediment size group
                  NAW = ACKW (1, SED)
                  AAW = ACKW (2, SED)
                  MAW = ACKW (3, SED)
                  CAW = ACKW (4, SED)
                  DSED = DRSED (SED)

                  ! Calculate particle mobility
                  UGR = ZERO
                  IF (DSED < H10) UGR = UK / (ROOT32_syackw * LOG10 (H10 / DSED))
                  FGR = ACKW (5, SED)
                  IF (NAW > ZERO) FGR = FGR * USTR**NAW
                  IF (NAW < ONE) FGR = FGR * UGR**(ONE - NAW)

                  ! Determine discharge capacity for this end
                  ! High-Performance Fix: Do not perform exponentiation (0.0**MAW) if base is zero or less.
                  IF (DWAT1E > ZERO) THEN
                     BASE = (FGR / AAW) - ONE
                     IF (BASE > ZERO) THEN
                        G = DSED * (QK / DWAT1E) * CAW * (BASE**MAW)
                        IF (NAW > ZERO) G = G * (UK / USTR)**NAW

                        ! Determine the total discharge capacity of both ends
                        GSED (LINK, SED) = GSED (LINK, SED) + G
                     END IF
                  END IF

               END DO

            END IF

         END DO
      END DO

   CONTAINS

      !> Clamps a scaled diameter to the Ackers-White dimensionless grain-size range \([1,D_{*,max}]\).
      ELEMENTAL FUNCTION FDGR(DUM_VAL) RESULT(RES)
         DOUBLE PRECISION, INTENT(IN) :: DUM_VAL !! Scaled particle diameter.
         DOUBLE PRECISION :: RES !! Clamped dimensionless grain size \(D_*\).
         RES = MAX (ONE, MIN (K2_syackw * DUM_VAL, DGRMAX_syackw))
      END FUNCTION FDGR

      !> Ackers-White mobility-threshold parameter \(A\) for a given dimensionless grain size.
      ELEMENTAL FUNCTION FA(DUM_VAL) RESULT(RES)
         DOUBLE PRECISION, INTENT(IN) :: DUM_VAL !! Dimensionless grain size \(D_*\).
         DOUBLE PRECISION :: RES !! Mobility-threshold parameter \(A\).
         RES = 0.14D0 + 0.23D0 / SQRT (DUM_VAL)
      END FUNCTION FA

   END SUBROUTINE SYACKW



!> Placeholder for time-varying sediment boundary flows.
!>
!> The current implementation is intentionally empty; boundary sediment fluxes
!> are instead handled through existing arrays and setup pathways in [[symain]].
!> The manual's `SY61`-`SY64` boundary records are therefore validated/read as
!> metadata, but no time-varying sediment boundary flux is applied here.
   SUBROUTINE SYBC
!STOP ' FATAL ERROR!!  Sediment boundary flows not yet implemented'
   END SUBROUTINE SYBC



!> Updates stream-bed depth and composition after channel sediment routing.
!>
!> `SYBED` applies the manual's `DCBEDO` rule for the two channel-bed layers.
!> The inputs `DCIPRM` and `DDIPRM` are the interim post-routing depths of each
!> sediment size fraction in the upper active layer and the lower bed layer.
!> For each link, the routine first sums them to obtain interim layer depths
!>
!> \[
!>   D_c' = \sum_s DCIPRM_s,\qquad
!>   D_d' = \sum_s DDIPRM_s,\qquad
!>   D_{ls}^{new} = D_c' + D_d' .
!> \]
!>
!> The active upper-layer thickness is then limited by
!>
!> \[
!>   D_c^{new} = \min(D_{ls}^{new}, DCBEDO),
!> \]
!>
!> so excess upper-layer deposition is transferred to the lower layer, while
!> lower-layer material replenishes the upper layer after erosion where
!> available. The fractions of the interim upper and lower layers retained in
!> the new active layer are
!>
!> \[
!>   a_c = {\min(D_c',D_c^{new})\over D_c'},\qquad
!>   a_d = {D_c^{new}-\min(D_c',D_c^{new})\over D_d'},
!> \]
!>
!> with zero used when a denominator is zero. For each size class,
!>
!> \[
!>   DCBSED_s = a_c DCIPRM_s + a_d DDIPRM_s,\qquad
!>   DDBSED_s = DCIPRM_s + DDIPRM_s - DCBSED_s .
!> \]
!>
!> The routine also updates total bed depth `DLS`, accumulated bed-depth change
!> `ARBDEP = ARBDEP + CWIDTH*(DLS_new-DLS_old)`, active-layer depth `DCBED`,
!> and whole-bed composition `FBETA_s = (DCIPRM_s+DDIPRM_s)/DLS_new`.
!>
!> @note If the new total bed depth is zero, `FBETA` is not overwritten for that
!> link; it retains its previous values even though `DLS` and `DCBED` become
!> zero.
!> @endnote
   PURE SUBROUTINE SYBED(DCBEDO, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, &
      DDIPRM, ARBDEP, DLS, FBETA, DCBSED, DDBSED, DCBED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE !! Link-array dimension.
      INTEGER, INTENT(IN) :: NSED  !! Number of sediment size classes.
      DOUBLE PRECISION, INTENT(IN) :: DCBEDO !! Target active upper channel-bed layer thickness.
      DOUBLE PRECISION, INTENT(IN) :: CWIDTH(NLF) !! Channel width by link.
      DOUBLE PRECISION, INTENT(IN) :: DCIPRM(NLFEE, NSED) !! Interim upper-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(IN) :: DDIPRM(NLFEE, NSED) !! Interim lower-bed sediment depth by link and size class.

      ! Input/output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: ARBDEP(NLF) !! Accumulated channel-bed elevation/depth change.
      DOUBLE PRECISION, INTENT(INOUT) :: DLS(NLF)     !! Total channel-bed sediment depth.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA(NELEE, NSED) !! Whole-bed sediment fraction by element/link and size class.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: DCBSED(NLFEE, NSED) !! Updated upper-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(OUT) :: DDBSED(NLFEE, NSED) !! Updated lower-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(OUT) :: DCBED(NLF) !! Updated active upper-bed layer depth by link.

      ! Locals, etc
      INTEGER :: LINK, SED
      DOUBLE PRECISION :: AC, AD, DCBEDZ, DCC, DCNEW, DDBEDZ, DLSNEW, DLSOLD
      DOUBLE PRECISION :: DCIPP, DDIPP, DCINEW, SUMSED

      !----------------------------------------------------------------------*

      ! * Loop over links
      link_loop: DO LINK = 1, NLF

         ! * Calculate interim bed layer thicknesses
         DCBEDZ = 0.0D0
         DDBEDZ = 0.0D0

         sum_loop: DO SED = 1, NSED
            DCBEDZ = DCBEDZ + DCIPRM(LINK, SED)
            DDBEDZ = DDBEDZ + DDIPRM(LINK, SED)
         END DO sum_loop

         ! * Reset variables that are independent of size group
         DLSOLD = DLS(LINK)
         DLSNEW = DCBEDZ + DDBEDZ
         DLS(LINK) = DLSNEW

         ARBDEP(LINK) = ARBDEP(LINK) + CWIDTH(LINK) * (DLSNEW - DLSOLD)
         DCNEW = MIN(DLSNEW, DCBEDO)
         DCBED(LINK) = DCNEW

         ! * What fraction of the interim top layer remains in the top
         ! * layer, and what fraction of the interim bottom layer becomes
         ! * part of the top?
         DCC = MIN(DCBEDZ, DCNEW)
         AC = 0.0D0
         AD = 0.0D0

         IF (DCBEDZ > 0.0D0) AC = DCC / DCBEDZ
         IF (DDBEDZ > 0.0D0) AD = (DCNEW - DCC) / DDBEDZ

         ! * Loop over sediment size groups
         sed_loop: DO SED = 1, NSED

            ! * Interim layer depths
            DCIPP = DCIPRM(LINK, SED)
            DDIPP = DDIPRM(LINK, SED)

            ! * Total depth (for this size group)
            SUMSED = DCIPP + DDIPP

            ! * New top layer depth
            DCINEW = AC * DCIPP + AD * DDIPP
            DCBSED(LINK, SED) = DCINEW

            ! * New bottom layer depth
            DDBSED(LINK, SED) = SUMSED - DCINEW

            ! * Composition of both layers together
            IF (DLSNEW > 0.0D0) FBETA(LINK, SED) = (SUMSED / DLSNEW)

         END DO sed_loop

      END DO link_loop

   END SUBROUTINE SYBED



!> Calculates lateral channel-bank erosion rates.
!>
!> This routine uses the manual's bank-soil properties: `NTSOBK` selects the
!> bank soil type, `BKB` is the channel-bank erodibility coefficient, `RHOSO`
!> is the bulk dry soil density, and `FPCLAY` is used only when `ISTEC = 1`
!> selects the clay-content critical-shear option in [[sycrit]].
!>
!> For each link, the flow shear stress `TAUK` is adjusted by an empirical bank
!> aspect-ratio coefficient. With water depth \(h\), channel width \(w\), and
!>
!> \[
!>   x = {1\over\max(0.25,h/w)},
!> \]
!>
!> the multiplier is
!>
!> \[
!>   k = 0.05
!>       + 0.41\min(x,1)
!>       + 0.22\min(\max(x-1,0),1)
!>       + 0.035\max(x-2,0).
!> \]
!>
!> The critical shear stress \(\tau_c\) is calculated by [[sycrit]] from the
!> bank-soil median diameter `DRSO50`, the selected `ISTEC` method, and the
!> current shear. The lateral bank-erosion rate is then
!>
!> \[
!>   GNUBK = {BKB\max(k\tau-\tau_c,0)\over \tau_c\,RHOSO}.
!> \]
!>
!> The released sediment source for the link accounts for both banks, the
!> bank-to-bed solid-volume conversion `FETA`, link length, and the wetted bank
!> height capped by bankfull depth:
!>
!> \[
!>   EPSB = 2\,FETA\,CLENTH\,GNUBK\,\min(h,DBFULL).
!> \]
   PURE SUBROUTINE SYBKER(ISTEC, NLF, NS, FPCLAY, RHOSO, DRSO50, TAUK, &
      CWIDTH, DWAT1, BKB, NTSOBK, FETA, CLENTH, DBFULL, EPSB, GNUBK)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISTEC !! Critical-shear calculation option.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NS    !! Number of soil types.
      INTEGER, INTENT(IN) :: NTSOBK(NLF) !! Bank soil type by link.
      DOUBLE PRECISION, INTENT(IN) :: FPCLAY(NS) !! Clay fraction by soil type.
      DOUBLE PRECISION, INTENT(IN) :: RHOSO(NS)  !! Soil bulk density by soil type.
      DOUBLE PRECISION, INTENT(IN) :: DRSO50(NS) !! Median soil particle diameter by soil type.
      DOUBLE PRECISION, INTENT(IN) :: BKB(NS)    !! Bank erodibility by soil type.
      DOUBLE PRECISION, INTENT(IN) :: TAUK(NLF)  !! Channel/link shear stress.
      DOUBLE PRECISION, INTENT(IN) :: CWIDTH(NLF) !! Channel width by link.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1(NLF)  !! Channel water depth by link.
      DOUBLE PRECISION, INTENT(IN) :: FETA(NLF)   !! Soil-to-sediment solid-volume conversion factor by link.
      DOUBLE PRECISION, INTENT(IN) :: CLENTH(NLF) !! Channel-link length.
      DOUBLE PRECISION, INTENT(IN) :: DBFULL(NLF) !! Bankfull depth by link.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: EPSB(NLF)  !! Bank erosion sediment source by link.
      DOUBLE PRECISION, INTENT(OUT) :: GNUBK(NLF) !! Lateral bank erosion rate by link.

      ! Locals, etc
      DOUBLE PRECISION, PARAMETER :: A1 = 0.05D0, B1 = 0.41D0, B2 = 0.22D0, B3 = 0.035D0
      DOUBLE PRECISION, PARAMETER :: QUART = 1.0D0 / 4.0D0

      INTEGER :: BKSOIL, LINK
      DOUBLE PRECISION :: DWAT1E, GNUBKE, K, TAUEC, TAUKE, X

      !----------------------------------------------------------------------*

      ! * Loop over channel links
      link_loop: DO LINK = 1, NLF

         BKSOIL = NTSOBK(LINK)
         DWAT1E = DWAT1(LINK)
         TAUKE  = TAUK(LINK)

         ! * Calculate aspect ratio coefficient ( see Notes )
         X = ONE / MAX(QUART, DWAT1E / CWIDTH(LINK))
         K = A1 + B1 * MIN(X, ONE) + B2 * MIN(DIMJE(X, ONE), ONE) &
            + B3 * DIMJE(X, TWO)

         ! * Obtain critical shear stress for bank erosion
         CALL SYCRIT(ISTEC, DRSO50(BKSOIL), TAUKE, FPCLAY(BKSOIL), TAUEC)

         ! * Calculate bank erosion rate
         GNUBKE = BKB(BKSOIL) * DIMJE(K * TAUKE, TAUEC) / (TAUEC * RHOSO(BKSOIL))
         GNUBK(LINK) = GNUBKE

         ! * Calculate rate of release of sediments for each link
         EPSB(LINK) = TWO * FETA(LINK) * CLENTH(LINK) * GNUBKE * &
            MIN(DWAT1E, DBFULL(LINK))

      END DO link_loop

   END SUBROUTINE SYBKER



!> Determines channel sediment transport capacity and advection coefficients.
!>
!> `SYCLTR` implements the manual's channel transport switches before channel
!> routing in [[sylink]]. Non-fine streamwise capacity `GSED` is calculated by
!> [[syengh]] when `ISACKW = 0`, or by [[syackw]] when `ISACKW = 1` or `2`.
!> Fine sediment capacity is not calculated by these formulae; for every fine
!> size group, the notional channel capacity concentration is set directly to
!> `FPCRIT`.
!>
!> The routine also builds the advection coefficients `QSDWAT` used to move
!> sediment with water through outflow faces. If `ISUSED = 0`, the manual's
!> water-speed option is used for every size class:
!>
!> \[
!>   QSDWAT_{s,f} = Q_f \qquad (Q_f > 0).
!> \]
!>
!> If `ISUSED = 1`, fine material still moves at the water speed, while each
!> non-fine fraction at link ends is limited by a shear-dependent velocity:
!>
!> \[
!>   QSDWAT_s =
!>   \min\left(
!>     {8.5\,ARXL\over\sqrt{\rho_w}}\,
!>     \sqrt{\max(\tau-\sqrt{\tau\tau_c},0)},\; Q
!>   \right),
!> \]
!>
!> where \(\tau_c\) is returned by [[sycrit]] using the Shields option for the
!> current particle diameter. `QSWSUM` accumulates these end-face coefficients
!> for use in converting discharge capacity into concentration capacity.
!>
!> For each non-fine fraction, capacity concentration is allocated using the
!> larger of a small background share, the existing suspended composition, and
!> the active-bed composition:
!>
!> \[
!>   f_s = \max\left(0.05,\,
!>                  {FDEL_s\over\sum_n FDEL_n},\,
!>                  {DCBSED_s\over DCBED}\right),
!> \]
!>
!> \[
!>   CONCI_s =
!>   \begin{cases}
!>     \min\left(FPCRIT,\; f_s\,GSED_s / QSWSUM_s\right), & QSWSUM_s > 0,\\
!>     0, & QSWSUM_s = 0.
!>   \end{cases}
!> \]
!>
!> For side outflows with `ISUSED = 1`, non-fine exchange is suppressed until
!> the notional concentration exceeds the manual overbank threshold `CONCOB`:
!>
!> \[
!>   QSDWAT_{s,side} =
!>   \begin{cases}
!>     Q_{side}{\max(CONCI_s-CONCOB,0)\over CONCI_s}, & CONCI_s > 0,\\
!>     0, & CONCI_s = 0.
!>   \end{cases}
!> \]
!>
!> @note `QSDWAT` entries are assigned only for outflow faces handled by this
!> routine. Callers should not interpret non-outflow entries as newly computed
!> values unless they have been cleared before the call.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-05 | SvB | 4.6.1 | Replaced the `ALINIT` zeroing of `QSWSUM` and the per-fine-class `ALINIT` of `CONCI` with array-slice assignment. |
!> | 2026-04-06 | SvB | 4.6.1 | Removed `GOTO`-driven control flow. |
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `K1_sycltr` with a compile-time `PARAMETER`. |
!> @endhistory
   SUBROUTINE SYCLTR (CONCOB, FPCRIT, ISACKW, ISUSED, NELEE, NFINE, NLF, NLFEE, NSED, NSEDEE,     &
      DRSED, ARXL, CWIDTH, DCBED, LINKNS, DWAT1, QOC, SLOPEJ, DCBSED, FDEL, TAUJ, &
      ACKW, CONCI, QSDWAT, GSED, QSWSUM)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISACKW !! Channel transport-capacity option.
      INTEGER, INTENT(IN) :: ISUSED !! Sediment velocity option.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NFINE  !! Number of fine sediment classes.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      DOUBLE PRECISION, INTENT(IN) :: CONCOB !! Mobile concentration threshold for overbank exchange.
      DOUBLE PRECISION, INTENT(IN) :: FPCRIT !! Maximum sediment concentration fraction.
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NFINE + 1:NSED) !! Representative non-fine particle diameters.
      DOUBLE PRECISION, INTENT(IN) :: ARXL (NLF)    !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(IN) :: CWIDTH (NLF)  !! Channel width by link.
      DOUBLE PRECISION, INTENT(IN) :: DCBED (NLF)   !! Active upper-bed layer depth by link.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1 (NLF)   !! Channel water depth by link.
      DOUBLE PRECISION, INTENT(IN) :: QOC (NELEE, 4) !! Face water fluxes.
      DOUBLE PRECISION, INTENT(IN) :: SLOPEJ (NELEE, 4) !! Face water-surface slopes.
      DOUBLE PRECISION, INTENT(IN) :: DCBSED (NLFEE, NFINE + 1:NSED) !! Active-bed sediment depth by link and non-fine class.
      DOUBLE PRECISION, INTENT(IN) :: FDEL (NELEE, NFINE + 1:NSED)   !! Mobile sediment concentration fraction by element and non-fine class.
      DOUBLE PRECISION, INTENT(IN) :: TAUJ (NELEE, 4) !! Face shear stress.
      LOGICAL, INTENT(IN)          :: LINKNS (NLF)    !! True for north-south channel links.

      ! Input/output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: ACKW (5, NFINE + 1:NSED) !! Cached Ackers-White parameters by non-fine class.

      ! Output arguments
      ! NB: QSDWAT defined for outflow faces only
      DOUBLE PRECISION, INTENT(OUT)   :: CONCI (NLFEE, NSED)      !! Capacity concentration by link and sediment class.
      DOUBLE PRECISION, INTENT(OUT)   :: QSDWAT (NLFEE, NSEDEE, 4) !! Sediment advection coefficient for outflow faces only.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: GSED (NLF, NFINE + 1:NSED) !! Channel transport capacity workspace.
      DOUBLE PRECISION, INTENT(INOUT) :: QSWSUM (NLF, NSED)         !! Sum of outflowing sediment advection coefficients by link/class.

      ! Locals, etc
      DOUBLE PRECISION, PARAMETER :: ZZ5 = 0.05D0
      DOUBLE PRECISION, PARAMETER :: k1_sycltr = 8.5D0 / SQRT (RHOWAT)

      INTEGER :: FACE, IEND, ISIDE, LINK, NFP1, NSDWAT, SED, SGN
      DOUBLE PRECISION :: CONCID, DCSUM, DUM, FDSUM, FRACT, KQ, QK
      DOUBLE PRECISION :: TAUEC, TAUD, QSW, FRACT1, FRACT2
      LOGICAL :: BODD

      !----------------------------------------------------------------------*

      NFP1 = NFINE + 1

      ! Replaced ALINIT with array slice operation
      QSWSUM(1:NLF, 1:NSED) = ZERO

      ! Streamwise capacity discharge rates ...
      ! ---------------------------------------
      !
      !     ... using specified method
      !
      IF (ISACKW == 1 .OR. ISACKW == 2) THEN
         CALL SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, DRSED, ARXL, DCBSED, DWAT1, &
            QOC, TAUJ, ACKW, GSED)
      ELSE
         CALL SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, QOC, LINKNS, SLOPEJ, GSED)
      END IF


      ! Advection Coefficients (outflow faces only) Part 1 ...
      ! ------------------------------------------------------
      !
      !     ... for size groups which move with water velocity
      !
      NSDWAT = NFINE
      IF (ISUSED == 0) NSDWAT = NSED

      IF (NSDWAT > 0) THEN
         ! * All faces (both ends and sides)
         DO FACE = 1, 4
            SGN = SIGN (1, 2 - FACE)
            BODD = MOD (FACE, 2) == 1

            ! * All links (but skip over non-outflow faces)
            DO LINK = 1, NLF
               QK = SGN * QOC (LINK, FACE)
               IF (GTZERO(QK)) THEN

                  ! * Set QSWSUM increment for link ends only
                  QSW = ZERO
                  IF (BODD .NEQV. LINKNS (LINK)) QSW = QK

                  ! * Fines only, or all size groups, as appropriate
                  DO SED = 1, NSDWAT
                     QSDWAT (LINK, SED, FACE) = QK
                     ! * Don't actually need QSWSUM for fines, but ...
                     QSWSUM (LINK, SED) = QSWSUM (LINK, SED) + QSW
                  END DO

               END IF
            END DO
         END DO
      END IF


      ! Advection Coefficients (outflow faces only)  Part 2 ...
      ! -------------------------------------------------------
      !
      !     ... at link ends for each size group which moves with an
      !         independent velocity.
      !
      IF (ISUSED == 1) THEN

         ! * Loop over both ends ( of every link )
         DO IEND = 1, 3, 2
            SGN = 2 - IEND

            ! * Loop over every link (but skip over non-outflow faces)
            DO LINK = 1, NLF
               FACE = IEND
               IF (LINKNS (LINK)) FACE = FACE + 1
               QK = SGN * QOC (LINK, FACE)

               IF (GTZERO(QK)) THEN

                  TAUD = TAUJ (LINK, FACE)
                  KQ = K1_sycltr * ARXL (LINK)

                  ! * Loop over non-fine size groups
                  DO SED = NFP1, NSED
                     CALL SYCRIT (0, DRSED (SED), TAUD, DUM, TAUEC)
                     QSW = MIN (KQ * SQRT (DIMJE(TAUD, SQRT (TAUD * TAUEC))), QK)
                     QSDWAT (LINK, SED, FACE) = QSW
                     QSWSUM (LINK, SED) = QSWSUM (LINK, SED) + QSW
                  END DO

               END IF

               ! * Next link
            END DO

            ! * Next end
         END DO

      END IF


      ! Determine notional particle concentrations at flow capacity
      ! -----------------------------------------------------------
      !
      ! * Loop over fines (Replaced ALINIT loop with single slice assignment)
      ! Note: Assuming FPCRIT is properly assigned to the entire NLF dimension for each fine
      CONCI(1:NLF, 1:NFINE) = FPCRIT

      ! * Loop over links
      DO LINK = 1, NLF

         ! * Determine denominators for scaling factors
         FDSUM = ZERO
         DO SED = NFP1, NSED
            FDSUM = FDSUM + FDEL (LINK, SED)
         END DO
         IF (ISZERO(FDSUM)) FDSUM = ONE

         DCSUM = DCBED (LINK)
         IF (ISZERO(DCSUM)) DCSUM = ONE

         ! * Loop over non-fines
         DO SED = NFP1, NSED
            QSW = QSWSUM (LINK, SED)
            IF (GTZERO(QSW)) THEN
               FRACT1 = FDEL (LINK, SED) / FDSUM
               FRACT2 = DCBSED (LINK, SED) / DCSUM
               FRACT = MAX (ZZ5, FRACT1, FRACT2)
               CONCID = MIN (FPCRIT, FRACT * GSED (LINK, SED) / QSW)
            ELSE
               CONCID = ZERO
            END IF
            CONCI (LINK, SED) = CONCID
         END DO

      END DO


      ! Advection Coefficients (outflow faces only) Part 3 ...
      ! ------------------------------------------------------
      !
      !     ... at link sides, for each size group which moves at an
      !         independent velocity.
      !
      IF (ISUSED == 1) THEN

         ! * Loop over both sides
         DO ISIDE = 2, 4, 2
            SGN = 3 - ISIDE

            ! * Loop over every link (but skip over non-outflow sides)
            DO LINK = 1, NLF
               FACE = ISIDE
               IF (LINKNS (LINK)) FACE = ISIDE - 1
               QK = SGN * QOC (LINK, FACE)

               IF (GTZERO(QK)) THEN

                  ! * Loop over non-fine size groups
                  DO SED = NFP1, NSED
                     DUM = CONCI (LINK, SED)
                     IF (GTZERO(DUM)) DUM = QK * DIMJE(DUM, CONCOB) / DUM
                     QSDWAT (LINK, SED, FACE) = DUM
                  END DO

               END IF

               ! * Next link
            END DO

            ! * Next side
         END DO

      END IF

   END SUBROUTINE SYCLTR



!> Routes sediment in overland flow for one column element.
!>
!> `SYCOLM` solves the overland-flow sediment balance for one land element over
!> one sediment time step. It works with the manual's non-dimensional mobile
!> concentration `FDEL`, loose-sediment depth `DLS`, loose-sediment composition
!> `FBETA`, loose-sediment porosity `PLS`, and total overland concentration
!> limit `FPCRIT`.
!>
!> For each face, positive `QWAT` is treated as outflow and negative `QWAT` as
!> inflow. With loose-sediment solid fraction
!>
!> \[
!>   f_{ls} = 1 - PLS ,
!> \]
!>
!> incoming particulate fluxes are converted to settled-volume units as
!>
!> \[
!>   q_s^{in} = -\sum_{Q_f \le 0}{QSEDE_{s,f}\over f_{ls}} .
!> \]
!>
!> The water volume available for storage plus outgoing transport is
!>
!> \[
!>   V_w = DWAT1\,AREAE + \Delta t\sum_{Q_f>0} Q_f .
!> \]
!>
!> New detachment from ground-surface erosion is added as
!>
!> \[
!>   \Delta D_{ls} = FETA\,GNU\,\Delta t ,
!> \]
!>
!> and the available settled sediment volume for size class \(s\) is
!>
!> \[
!>   V_s =
!>   \left(FDEL_s^{old}DWATO + DLS\,FBETA_s
!>         + \Delta D_{ls}SOSDF_s\right)AREAE
!>   + q_s^{in}\Delta t .
!> \]
!>
!> The supply-limited total particulate discharge rate is
!>
!> \[
!>   G_{supply} = f_{ls}\left(\sum_s V_s\right)
!>                {\sum_{Q_f>0}Q_f\over V_w},
!> \]
!>
!> while the capacity-limited rate is the overland transport capacity from
!> [[syovtr]], additionally bounded by the manual's total mobile concentration
!> limit:
!>
!> \[
!>   G_{cap} = \min(G_{SYOVTR},\,FPCRIT\sum_{Q_f>0}Q_f).
!> \]
!>
!> The transported fraction is therefore
!>
!> \[
!>   a = {\min(G_{cap},G_{supply})\over G_{supply}},
!> \]
!>
!> with `a = 0` when no sediment or no carrying water is available. Outputs are
!> then updated as
!>
!> \[
!>   DLS^{new} = {(1-a)\sum_s V_s\over AREAE},\qquad
!>   FDEL_s^{new} = {aV_s\over V_w},
!> \]
!>
!> \[
!>   QSEDE_{s,f}^{out} = f_{ls}Q_fFDEL_s^{new}\quad(Q_f>0).
!> \]
!>
!> If no loose sediment remains, `FBETA` is reset to the surface-soil
!> composition `SOSDF`; otherwise it is set from the remaining `V_s` mix.
!>
!> @note `QSEDE` is updated only for faces listed as outflows in this call.
!> Inflow and no-flow faces are read as incoming fluxes but are not cleared or
!> overwritten before return.
!> @endnote
   PURE SUBROUTINE SYCOLM (AREAE, DTSY, DWAT1E, DWATOE, DXQQE, DYQQE, FETAE, GNUE, ISGSED, NSED,       &
      FPCRIT, PLSE, NSEDEE, DRSED, QWAT, SLOPEE, SOSDFE, TAUJE, DLSE, FBETAE,     &
      FDELE, QSEDE, Q, VDSED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISGSED !! Overland transport-capacity option.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      DOUBLE PRECISION, INTENT(IN) :: AREAE  !! Element plan area.
      DOUBLE PRECISION, INTENT(IN) :: DTSY   !! Sediment substep duration.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1E !! Current surface water depth.
      DOUBLE PRECISION, INTENT(IN) :: DWATOE !! Previous surface water depth.
      DOUBLE PRECISION, INTENT(IN) :: DXQQE  !! Element width.
      DOUBLE PRECISION, INTENT(IN) :: DYQQE  !! Element length.
      DOUBLE PRECISION, INTENT(IN) :: FETAE  !! Soil-to-sediment solid-volume conversion factor.
      DOUBLE PRECISION, INTENT(IN) :: GNUE   !! Hillslope erosion rate.
      DOUBLE PRECISION, INTENT(IN) :: FPCRIT !! Maximum sediment concentration fraction.
      DOUBLE PRECISION, INTENT(IN) :: PLSE   !! Loose-sediment porosity.
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NSED) !! Representative particle diameters by size class.
      DOUBLE PRECISION, INTENT(IN) :: QWAT (4)     !! Outward water flux by face.
      DOUBLE PRECISION, INTENT(IN) :: SLOPEE (4)   !! Water-surface slope by face.
      DOUBLE PRECISION, INTENT(IN) :: SOSDFE (NSED) !! Source soil sediment-size fraction.
      DOUBLE PRECISION, INTENT(IN) :: TAUJE (4)     !! Face shear stress.

      ! Input/output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: DLSE       !! Loose-sediment depth in the land element.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETAE (NSED) !! Loose-sediment composition by size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FDELE (NSED)  !! Mobile sediment concentration fraction by size class.
      DOUBLE PRECISION, INTENT(INOUT) :: QSEDE (NSEDEE, 4) !! Sediment flux by size class and face.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: Q (NSED)     !! Workspace for outgoing sediment flux by size class.
      DOUBLE PRECISION, INTENT(INOUT) :: VDSED (NSED) !! Workspace for available sediment volume by size class.

      ! Locals, etc
      INTEGER :: FACE, J (4), JLC, NOUT, SED
      DOUBLE PRECISION :: A1, A2, A3, B1, B2, DBETA, DDLS, FD, FLS, G
      DOUBLE PRECISION :: GJSUM, GSUM, QK, QWSUM, VD, VDSUM, VDWAT

      !----------------------------------------------------------------------*

      ! Initialization
      ! --------------
      !
      QWSUM = ZERO
      VDSUM = ZERO
      FLS = ONE - PLSE

      ! Replaced ALINIT with Fortran array slice
      Q(1:NSED) = ZERO

      ! Water & Sediment Budgets
      ! ------------------------
      !
      !     * Calculate water discharge & particulate supply rates
      !     * ( both non-negative ), and make a list of outflow faces
      NOUT = 0
      DO FACE = 1, 4
         QK = QWAT (FACE)
         IF (QK > ZERO) THEN
            ! * Outflow face
            QWSUM = QWSUM + QK
            NOUT = NOUT + 1
            J (NOUT) = FACE
         ELSE
            ! * Inflow or no-flow face
            DO SED = 1, NSED
               Q (SED) = Q (SED) - QSEDE (SED, FACE) / FLS
            END DO
         END IF
      END DO

      !     * Calculate volume of water + volume of discharged water
      VDWAT = DWAT1E * AREAE + QWSUM * DTSY

      !     * Calculate volume of stored sediment plus volume of
      !     * discharged sediment for each fraction ( must be non-negative )
      DDLS = FETAE * GNUE * DTSY
      DO SED = 1, NSED
         DBETA = DLSE * FBETAE (SED) + DDLS * SOSDFE (SED)
         VD = (FDELE (SED) * DWATOE + DBETA) * AREAE + Q (SED) * DTSY
         VDSUM = VDSUM + VD
         VDSED (SED) = VD
      END DO


      ! Sediment Discharge
      ! ------------------
      !
      !     Note: The only outputs from this section are the coefficients
      !           A1 and B1 required by the next section.
      !
      !     * Discharge rate based upon SUPPLY, assuming unlimited capacity
      GSUM = ZERO
      IF (GTZERO(VDWAT)) GSUM = FLS * VDSUM * (QWSUM / VDWAT)

      !     * Is discharge possible?
      IF (GTZERO(GSUM)) THEN

         ! * Yes ( implies VDSUM > 0 )
         !
         ! * Discharge rate based upon flow CAPACITY ...
         CALL SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, DRSED, QWAT, SLOPEE, TAUJE, GJSUM)

         ! ... with additional upper limit based on total suspended load
         G = MIN (GJSUM, QWSUM * FPCRIT)

         ! * Transport is governed by the lower of the two rates
         !   (take MIN before dividing, in case G>>GSUM)
         A1 = MIN (G, GSUM) / GSUM
         B1 = VDWAT

      ELSE

         ! * Either no sediment available, or no water to carry it
         !
         ! * Zero discharge case ( any sediment is deposited )
         A1 = ZERO
         B1 = ONE

      END IF


      ! Define Output Variables
      ! -----------------------
      !
      !     * Update depth of loose sediments
      DLSE = (ONE - A1) * VDSUM / AREAE

      !     * Evaluate coefficients for FBETAE
      IF (GTZERO(DLSE)) THEN
         ! * Composition of loose sediment is given by VDSED
         A2 = ONE
         B2 = VDSUM
         A3 = ZERO
      ELSE
         ! * No loose sediment left: adopt composition of surface soil
         A2 = ZERO
         B2 = ONE
         A3 = ONE
      END IF

      !     * Update compositions of suspended and loose sediments, and set
      !     * sediment flow rates for each outflow face.
      !     * ( don't pre-invert B1 or B2: they may be small! )
      DO SED = 1, NSED
         VD = VDSED (SED)
         FD = (A1 * VD) / B1
         FDELE (SED) = FD
         FBETAE (SED) = A2 * VD / B2 + A3 * SOSDFE (SED)

         DO JLC = 1, NOUT
            FACE = J (JLC)
            QSEDE (SED, FACE) = FLS * QWAT (FACE) * FD
         END DO
      END DO

   END SUBROUTINE SYCOLM



!> Calculates critical shear stress for incipient sediment motion.
!>
!> `SYCRIT` implements the two critical-shear options referenced by the
!> sediment manual's `ISTEC` switch. Callers pass `FLAG = 1` for the simple
!> clay-content relation, and any other value for the Shields-style relation.
!>
!> With `FLAG = 1`, the critical shear stress is calculated directly from the
!> fractional clay content `FPCLAE`:
!>
!> \[
!>   \tau_c = 0.493\exp\left(1.83\ln(10)\,FPCLAE\right).
!> \]
!>
!> Otherwise, the routine computes a particle Reynolds number using the current
!> applied shear `TAUX`, representative diameter `DRX50`, water density
!> \(\rho_w\), and kinematic viscosity \(\nu\):
!>
!> \[
!>   R_* =
!>   \max\left(0.03,\min\left({DRX50\sqrt{TAUX}\over\sqrt{\rho_w}\nu},
!>   400\right)\right).
!> \]
!>
!> A coefficient pair \((a,b)\) is selected from the interval containing
!> \(R_*\):
!>
!> | Range of \(R_*\) | \(a\) | \(b\) |
!> |:-----------------|:------|:------|
!> | \(0.03 \le R_* \le 1\) | 0.1 | -0.3 |
!> | \(1 < R_* \le 6\) | 0.1 | -0.62 |
!> | \(6 < R_* \le 30\) | 0.033 | 0 |
!> | \(30 < R_* \le 135\) | 0.013 | 0.28 |
!> | \(135 < R_* \le 400\) | 0.03 | 0.1 |
!>
!> The Shields-style critical stress is then
!>
!> \[
!>   \tau_c = a(\rho_s-\rho_w)g\,DRX50\,R_*^b .
!> \]
!>
!> `TAUX` is used only to place the particle in the appropriate \(R_*\) class;
!> the returned value is `TAUEC`.
!>
!> @note The `IS` class index is still selected with the original branchless
!> `SF(RSTR,R)=0.5-\mathrm{sign}(0.5,R-RSTR)` switch function, summed over the
!> four interval boundaries, rather than an `IF`/`ELSE IF` chain: an
!> intermediate modernisation replaced it with branches and was reverted for
!> performance, restoring this original form.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `K1_sycrit`/`K2_sycrit`/`K3_sycrit` with compile-time `PARAMETER`s, and replaced the legacy `DATA`-initialised `AEC`/`BEC` arrays with array constructors. |
!> @endhistory
   PURE SUBROUTINE SYCRIT (FLAG, DRX50, TAUX, FPCLAE, TAUEC)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: FLAG !! Critical-shear option: 1 clay relation, otherwise Shields relation.
      DOUBLE PRECISION, INTENT(IN) :: DRX50  !! Median particle diameter.
      DOUBLE PRECISION, INTENT(IN) :: TAUX   !! Current shear stress used to select Shields class.
      DOUBLE PRECISION, INTENT(IN) :: FPCLAE !! Clay fraction for the clay-content relation.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: TAUEC !! Returned critical shear stress.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: R0 = 3.0D-2, R1 = 1.0D0
      DOUBLE PRECISION, PARAMETER :: R2 = 6.0D0, R3 = 30.0D0, R4 = 135.0D0, R5 = 400.0D0

      DOUBLE PRECISION, PARAMETER :: AEC(5) = [0.1D0, 0.1D0, 0.033D0, 0.013D0, 0.03D0]
      DOUBLE PRECISION, PARAMETER :: BEC(5) = [-0.3D0, -0.62D0, 0.0D0, 0.28D0, 0.1D0]

      ! High-Performance Fix: Compile-time evaluation of constants
      ! (Completely replaces the runtime FIRST_sycrit block)
      DOUBLE PRECISION, PARAMETER :: K1_sycrit = 1.0D0 / (SQRT(RHOWAT) * VISCOS)
      DOUBLE PRECISION, PARAMETER :: K2_sycrit = (RHOSED - RHOWAT) * GRAVTY
      DOUBLE PRECISION, PARAMETER :: K3_sycrit = 1.83D0 * LOG(10.0D0)

      INTEGER :: IS
      DOUBLE PRECISION :: RSTR

      ! Legacy branchless statement function
      DOUBLE PRECISION :: SF, RSTR_DUM, R_DUM
      SF(RSTR_DUM, R_DUM) = HALF - SIGN(HALF, R_DUM - RSTR_DUM)

      !----------------------------------------------------------------------*

      IF (FLAG == 1) THEN
         ! Quick method
         TAUEC = 0.493D0 * EXP(K3_sycrit * FPCLAE)
      ELSE
         ! Shields method
         RSTR = MAX(R0, MIN(DRX50 * SQRT(TAUX) * K1_sycrit, R5))

         ! Performance Reversion: Branchless execution
         IS = NINT(ONE + SF(RSTR, R1) + SF(RSTR, R2) + SF(RSTR, R3) + SF(RSTR, R4))

         TAUEC = AEC(IS) * K2_sycrit * DRX50 * (RSTR**BEC(IS))
      END IF

   END SUBROUTINE SYCRIT



!> Returns a percentile grain diameter from a discrete size distribution.
!>
!> `SYDR` is used where the sediment routines need a representative diameter
!> from a discrete size distribution, for example bed-material \(D_{16}\),
!> \(D_{50}\), and \(D_{84}\) in [[syackw]], or soil median diameter in
!> [[syinit]]. `D(1:N)` contains the particle diameters. The corresponding
!> distribution weights are read from `F`, but only every `INCF`-th element is
!> relevant:
!>
!> \[
!>   w_i = F(1 + (i-1)INCF),\qquad d_i = D(i).
!> \]
!>
!> `FSED` is the requested fraction in the range 0-1 rather than a percentage.
!> The code treats the discrete distribution as values located at the supplied
!> diameters and integrates with trapezoidal midpoints. With total weight
!> \(W=\sum_i w_i\), it searches for the first interval whose doubled
!> cumulative midpoint sum exceeds
!>
!> \[
!>   2FSED\,W .
!> \]
!>
!> It then linearly interpolates between the two bracketing diameters:
!>
!> \[
!>   d = d_{hi} - (d_{hi}-d_{lo}){S-2FSED\,W\over w_{lo}+w_{hi}},
!> \]
!>
!> where \(S\) is the doubled cumulative midpoint sum at the selected interval.
!> A zero requested fraction or null distribution returns zero.
!>
!> @note The routine assumes the supplied distribution weights are non-negative
!> and ordered with increasing diameters. It does not normalise or validate the
!> weights; upstream input checks are responsible for valid sediment
!> distributions.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-06 | SvB | 4.6.1 | Replaced the `GOTO`-driven search loop with `EXIT search_loop`. |
!> @endhistory
   PURE DOUBLE PRECISION FUNCTION SYDR (FSED, INCF, N, F, D)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: INCF !! Stride between distribution weights in `F`.
      INTEGER, INTENT(IN) :: N    !! Number of sediment size classes.
      DOUBLE PRECISION, INTENT(IN) :: FSED !! Target cumulative fraction.
      DOUBLE PRECISION, INTENT(IN) :: F(1 + (N - 1) * INCF) !! Sediment distribution weights.
      DOUBLE PRECISION, INTENT(IN) :: D(N) !! Representative particle diameters.

      ! Locals
      DOUBLE PRECISION, PARAMETER  :: ALMOST = 0.9999D0
      DOUBLE PRECISION :: DR, DRHI, DRLO, F02, FLO, FHI, FSUM2, FTOT
      INTEGER :: FRPTR, SED

      !----------------------------------------------------------------------*

      ! * Initialize local variables
      FHI = 0.0d0
      DRHI = 0.0d0
      FSUM2 = 0.0d0
      FTOT = 0.0d0
      FRPTR = 1

      ! * Double the selected 'percentile' (actually a fraction 0-1)
      ! * and scale it relative to the sum of distribution ratios
      DO SED = 1, N
         FTOT = FTOT + F(FRPTR)
         FRPTR = FRPTR + INCF
      END DO

      F02 = 2.0d0 * FSED * FTOT

      IF (ISZERO(F02)) THEN
         ! * Zeroth percentile or null distribution
         DR = 0.0d0

      ELSE
         ! * Reset fraction pointer
         FRPTR = 1

         ! * Loop over sediment types until target percentile surpassed
         search_loop: DO SED = 1, N

            ! * Calculate midpoint of cumulative fraction (doubled)
            FLO = FHI
            DRLO = DRHI
            FHI = F(FRPTR)
            DRHI = D(SED)
            FSUM2 = FSUM2 + FLO + FHI

            ! * Break out of loop if target percentile has been reached
            ! * (allowing for rounding error)
            IF (FSUM2 >= F02 * ALMOST) EXIT search_loop

            ! * Increment fraction pointer
            FRPTR = FRPTR + INCF

         END DO search_loop

         ! * Interpolate between last two Fraction/Diameter pairs to find
         ! * target percentile.
         ! * Note :- Combination of precondition FSED<1 and use of ALMOST
         ! * should ensure (FLO+FHI) > 0
         DR = DRHI - (DRHI - DRLO) * (FSUM2 - F02) / (FLO + FHI)

      END IF

      SYDR = DR

   END FUNCTION SYDR



!> Calculates Engelund-Hansen channel transport capacity.
!>
!> This is the `ISACKW = 0` channel-capacity option used by [[sycltr]]. As in
!> the manual, it applies only to non-fine sediment fractions
!> (`NFINE+1:NSED`); fine sediment is capacity-limited separately by `FPCRIT`.
!>
!> The routine zeroes `GSED`, then loops over the two link ends and adds a
!> contribution only where the selected face is outflowing and the link is wet.
!> Link orientation is handled through `LINKNS`, so the upstream/downstream end
!> is mapped to the correct `QOC` and `SLOPEJ` face.
!>
!> For each outflowing end, with discharge \(Q\), bed slope \(S\), channel
!> width \(w\), water depth \(h\), representative particle diameter \(d_s\),
!> and relative submerged density \(R=\rho_s/\rho_w-1\), the implemented
!> Engelund-Hansen capacity increment is
!>
!> \[
!>   \Delta G_s =
!>   {0.05\,Q^2S^{3/2}\over \sqrt{g}\,R^2\,w\sqrt{h}\,d_s}.
!> \]
!>
!> `GSED(link,s)` is the sum of \(\Delta G_s\) over outflowing ends of that
!> link. [[sycltr]] later converts this streamwise discharge capacity into a
!> notional concentration capacity and applies `FPCRIT`.
!>
!> @note The implemented formula uses `SLOPEJ**1.5` directly for each outflowing
!> end. It relies on the upstream water-interface calculations and validation to
!> provide non-negative channel slopes for active outflows.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-05 | SvB | 4.6.1 | Replaced the `ALINIT` zeroing of `GSED` with a whole-array slice assignment. |
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `KG_syengh` with a compile-time `PARAMETER`. |
!> @endhistory
   PURE SUBROUTINE SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, QOC, LINKNS, SLOPEJ, GSED)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NFINE !! Number of fine sediment classes excluded from this calculation.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NSED  !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NFINE + 1:NSED) !! Representative non-fine particle diameters.
      DOUBLE PRECISION, INTENT(IN) :: CWIDTH (NLF) !! Channel width by link.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1 (NLF)  !! Channel water depth by link.
      DOUBLE PRECISION, INTENT(IN) :: QOC (NELEE, 4)    !! Face water fluxes.
      DOUBLE PRECISION, INTENT(IN) :: SLOPEJ (NELEE, 4) !! Face water-surface slopes.
      LOGICAL, INTENT(IN)          :: LINKNS (NLF) !! True for north-south channel links.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: GSED (NLF, NFINE + 1:NSED) !! Channel transport capacity by link and non-fine class.

      ! Locals, etc
      INTEGER          :: FACE, IEND, LINK, NFP1, SED, SGN
      DOUBLE PRECISION :: DWAT1E, GD, QK
      DOUBLE PRECISION, PARAMETER :: KG_syengh = 0.05D0 / (SQRT (GRAVTY) * (RHOSED / RHOWAT - 1.0D0)**2)

      ! External/Module functions implicitly referenced
      ! LOGICAL :: GTZERO

      !----------------------------------------------------------------------*

      ! * Initialization
      NFP1 = NFINE + 1

      ! Replaced ALINIT with a whole-array slice assignment
      GSED(:, :) = ZERO

      ! * Loop over ends of link
      DO IEND = 1, 3, 2
         SGN = 2 - IEND

         ! * Loop over links
         DO LINK = 1, NLF

            ! * Determine current face number, outflow rate & water depth
            FACE = IEND
            IF (LINKNS (LINK)) FACE = FACE + 1
            QK = SGN * QOC (LINK, FACE)
            DWAT1E = DWAT1 (LINK)

            ! * Increment capacity rate for non-dry outflow ends only
            IF (GTZERO(DWAT1E) .AND. GTZERO(QK)) THEN

               ! * Loop invariant
               GD = QK**2 * SLOPEJ (LINK, FACE)**1.5D0 * KG_syengh / (CWIDTH (LINK) * SQRT (DWAT1E))

               ! * All sediment types
               DO SED = NFP1, NSED
                  GSED (LINK, SED) = GD / DRSED (SED) + GSED (LINK, SED)
               END DO

            END IF

            ! * Next link
         END DO

         ! * Next iend
      END DO

   END SUBROUTINE SYENGH



!> Checks scalar dimensions and file units passed through the water-sediment interface.
!>
!> `SYERR0` is the first consistency guard for the sediment component. It
!> verifies that the static workspace dimensions supplied by the water model can
!> contain the sediment arrays before any `SY` input is read or state is
!> initialised.
!>
!> The checks require:
!>
!> | Quantity | Required relation |
!> |:---------|:------------------|
!> | `NELEE` | `>= max(NEL,NV,NX*NY)` |
!> | `NLFEE` | `>= max(1,NLF)` |
!> | `NLYREE`, `NSEDEE` | both `> 0` |
!> | `NSEE` | `>= NS` |
!> | `NVEE` | `>= NV` |
!> | `NXEE` | `>= NX` and `<= 9999` |
!> | `SPR`, `SYD` | both non-negative unit numbers |
!> | `NLF` | `0 <= NLF < NEL` |
!> | `NS`, `NV`, `NX`, `NY` | all `> 0` |
!>
!> Each failed relation is reported through `ALCHKI` on the sediment print unit
!> `SPR`. If any failures are found, the routine raises fatal error 2000 before
!> returning.
   SUBROUTINE SYERR0(NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, &
      NSEE, NV, NVEE, NX, NXEE, NY, SPR, SYD)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NEL   !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE !! Link-array dimension.
      INTEGER, INTENT(IN) :: NLYREE !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NS    !! Number of soil types.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      INTEGER, INTENT(IN) :: NSEE  !! Soil-type array dimension.
      INTEGER, INTENT(IN) :: NV    !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NVEE  !! Vegetation-array dimension.
      INTEGER, INTENT(IN) :: NX    !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NY    !! Number of grid rows.
      INTEGER, INTENT(IN) :: SPR   !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: SYD   !! Static sediment input unit.

      ! Modernization Fix: Added IZERO_ARR to replace the undeclared IZERO1
      ! and made IUNDEF a parameter to prevent passing uninitialized memory.
      INTEGER, PARAMETER :: IZERO_ARR(1) = [0]
      INTEGER, PARAMETER :: IUNDEF = 0

      INTEGER :: NERR, JEDUMDUM
      INTEGER :: IDUMS(1), IDUMO(1)
      LOGICAL :: LDUM1(1)

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------

      ! * Initialize local counters
      NERR = 0

      ! 1. Array Sizes
      ! --------------

      ! NELEE
      IDUMS(1) = NELEE
      IDUMO(1) = MAX(NEL, NV, NX * NY)
      CALL ALCHKI(ERRLVL_error, 2054, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! NLFEE
      IDUMS(1) = NLFEE
      IDUMO(1) = MAX(1, NLF)
      CALL ALCHKI(ERRLVL_error, 2055, SPR, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! NLYREE, NSEDEE
      IDUMS(1) = MIN(NLYREE, NSEDEE)
      CALL ALCHKI(ERRLVL_error, 2056, SPR, 1, 1, IUNDEF, IUNDEF, '[ NLYREE, NSEDEE ]', 'GT', IZERO_ARR, IDUMS, NERR, LDUM1)

      ! NSEE
      IDUMS(1) = NSEE
      IDUMO(1) = NS
      CALL ALCHKI(ERRLVL_error, 2057, SPR, 1, 1, IUNDEF, IUNDEF, 'NSEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! NVEE
      IDUMS(1) = NVEE
      IDUMO(1) = NV
      CALL ALCHKI(ERRLVL_error, 2058, SPR, 1, 1, IUNDEF, IUNDEF, 'NVEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! NXEE
      IDUMS(1) = NXEE
      IDUMO(1) = NX
      CALL ALCHKI(ERRLVL_error, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      IDUMO(1) = 9999
      CALL ALCHKI(ERRLVL_error, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'LE', IDUMO, IDUMS, NERR, LDUM1)

      ! 2. Unit Numbers
      ! ---------------

      ! SPR, SYD
      IDUMS(1) = MIN(SPR, SYD)
      CALL ALCHKI(ERRLVL_error, 2060, SPR, 1, 1, IUNDEF, IUNDEF, '[ SPR, SYD ]', 'GE', IZERO_ARR, IDUMS, NERR, LDUM1)

      ! 3. Number of Entities
      ! ---------------------

      ! NLF
      IDUMS(1) = NLF
      IDUMO(1) = NEL
      CALL ALCHKI(ERRLVL_error, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO_ARR, IDUMS, NERR, LDUM1)
      CALL ALCHKI(ERRLVL_error, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)

      ! NS, NV, NX, NY
      JEDUMDUM = MIN(NS, NV)
      IDUMS(1) = MIN(JEDUMDUM, NX, NY)
      CALL ALCHKI(ERRLVL_error, 2062, SPR, 1, 1, IUNDEF, IUNDEF, '[ NS, NV, NX, NY ]', 'GT', IZERO_ARR, IDUMS, NERR, LDUM1)

      ! 4. Epilogue
      ! -----------

      IF (NERR > 0) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 2000, SPR, 0, 0, 'Error(s) detected while checking WAT-SY interface variables')
      END IF

   END SUBROUTINE SYERR0



!> Checks static water-flow arrays required by the sediment component.
!>
!> `SYERR1` validates the static and initial water-model data that the sediment
!> routines rely on after [[syerr0]] has confirmed the workspace dimensions.
!> These checks protect the later erosion, routing, and bed-update routines from
!> invalid topology, out-of-range soil/vegetation indices, and non-physical
!> geometry.
!>
!> Main validation groups:
!>
!> | Group | Checks performed |
!> |:------|:-----------------|
!> | Element indexing | `ICMXY` and, when banks exist, `ICMBK` define exactly the expected column/bank elements with unique element identities. |
!> | Face adjacency | `ICMREF` neighbours are in range and regular element faces reflect back to the originating element and face. |
!> | Branch adjacency | `ICMRF2` branch references are unique, in range, and mirrored consistently through `ICMREF`. |
!> | Bank-neighbour topology | Each banked link has at least one neighbouring grid element when explicit banks are enabled. |
!> | Soil state | `THSAT(soil) <= 1`. |
!> | Channel geometry | `CLENTH >= 0`, `CWIDTH > 0`, `ZBFULL >= ZGRUND`, and `ARXL >= 0` for each link. |
!> | Column geometry/state | `DXQQ > 0`, `DYQQ > 0`, `HRF >= ZGRUND`, valid `NLYR`, valid top-layer soil type `NTSOIL(iel,NLYR)`, and valid vegetation type `NVC`. |
!> | Element geometry | `AREA > 0` and all face distances `DHF > 0`. |
!>
!> @note Bank-neighbour checks use the bank face normal to the link:
!> `FACE = 2*BANK`, decremented for north-south links, and require at least
!> one active grid neighbour across the two banks of each channel link.
!> @endnote
!>
!> Each failed relation is reported through `ALCHK` or `ALCHKI` on the sediment
!> print unit `SPR`. If any failures are found, the routine raises fatal error
!> 2001 before returning.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-05-03 | SvB | 4.6.1 | Replaced an uninitialised local `IUNDEF` "don't care" argument to `ALCHK`/`ALCHKI` with an explicit `PARAMETER = 0`. |
!> @endhistory
   SUBROUTINE SYERR1(NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, &
      NXEE, NYEE, NY, SPR, BEXBK, LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, &
      NTSOIL, NVC, THSAT, CLENTH, CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, &
      ARXL, HRF, ZGRUND, IDUM, IDUM1X, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: NEL    !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NLYREE !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NS     !! Number of soil types.
      INTEGER, INTENT(IN) :: NV     !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NX     !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE   !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NYEE   !! Grid-row workspace dimension.
      INTEGER, INTENT(IN) :: NY     !! Number of grid rows.
      INTEGER, INTENT(IN) :: SPR    !! Sediment diagnostic output unit.
      LOGICAL, INTENT(IN) :: BEXBK       !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE) !! True for north-south channel links.

      ! Read-Only Arrays (Used for reference or copied to scratchpads)
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)   !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)   !! Element number at each grid location.
      INTEGER, INTENT(IN) :: ICMRF2(NLFEE, 3, 2) !! Confluence branch reference map.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE) !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND(NEL) !! Ground or bed elevation by element.

      ! Arrays checked by ALCHK/ALCHKI (routines may use INTENT(INOUT) interfaces)
      INTEGER, INTENT(INOUT) :: ICMREF(NELEE, 4, 2:3) !! Face-neighbour and reverse-face reference map.
      INTEGER, INTENT(INOUT) :: NLYR(NLF + 1:NEL) !! Number of soil layers in each land element.
      INTEGER, INTENT(INOUT) :: NVC(NLF + 1:NEL)  !! Vegetation type by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: THSAT(NS)      !! Saturated water content by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: CLENTH(NLFEE)  !! Channel-link length.
      DOUBLE PRECISION, INTENT(INOUT) :: CWIDTH(NLFEE)  !! Channel width by link.
      DOUBLE PRECISION, INTENT(INOUT) :: ZBFULL(NLFEE)  !! Bankfull elevation/depth by link.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NLF + 1:NEL) !! Land-element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NLF + 1:NEL) !! Land-element length.
      DOUBLE PRECISION, INTENT(INOUT) :: AREA(NEL)      !! Element plan area.
      DOUBLE PRECISION, INTENT(INOUT) :: DHF(NELEE, 4)  !! Face-to-face hydraulic distance.
      DOUBLE PRECISION, INTENT(INOUT) :: ARXL(NLFEE)    !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(INOUT) :: HRF(NLF + 1:NEL) !! Land-element water level/head.

      ! Workspace arguments (INTENT(INOUT) as scratch space)
      INTEGER, INTENT(INOUT) :: IDUM(NXEE * NYEE) !! Integer workspace for identity checks.
      INTEGER, INTENT(INOUT) :: IDUM1X(-1:NEL + 1) !! Integer workspace for element identity checks.
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE) !! Logical workspace for element checks.

      ! Strict array/scalar parameters for shape matching in ALCHK
      INTEGER, PARAMETER          :: IZERO_ARR(1) = [0], IONE_ARR(1) = [1]
      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0

      INTEGER :: BANK, COUNT, FACE, FADJ, FEL
      INTEGER :: IADJ, IBR, IBRADJ, ICOL1, IEL, IELP, ILYR, IX, IY
      INTEGER :: LINK, NCOL, NELP, NERR, P, PADJ
      INTEGER, PARAMETER :: IUNDEF = 0
      INTEGER :: IDUM1(2)
      LOGICAL :: BKXYOK, REFOK

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------
      NERR = 0
      ICOL1 = NLF + 1
      NELP = NEL + 1

      ! 1. Index Arrays
      ! ---------------

      ! ICMBK, ICMXY
      COUNT = NERR
      NCOL = 0

      DO IEL = 0, NLF
         IDUM1X(IEL) = 1
      END DO

      DO IEL = ICOL1, NELP
         IDUM1X(IEL) = 0
      END DO

      DO IY = 1, NY
         DO IX = 1, NX
            IEL = MAX(0, MIN(ICMXY(IX, IY), NELP))
            IDUM1X(IEL) = IDUM1X(IEL) + 1
            NCOL = NCOL + MIN(IEL, 1)
         END DO
      END DO

      IF (BEXBK .AND. NLF > 0) THEN
         NCOL = NCOL + 2 * NLF
         DO BANK = 1, 2
            DO LINK = 1, NLF
               IEL = MAX(0, MIN(ICMBK(LINK, BANK), NELP))
               IDUM1X(IEL) = IDUM1X(IEL) + 1
            END DO
         END DO
      END IF

      IDUM1(1) = NEL - NLF
      IDUM1X(0) = NCOL

      CALL ALCHKI(ERRLVL_error, 2075, SPR, 1, 1, IUNDEF, IUNDEF, '#_column_elements', 'EQ', IDUM1, IDUM1X(0:), NERR, LDUM)
      CALL ALCHKI(ERRLVL_error, 2076, SPR, 1, NEL, IUNDEF, IUNDEF, 'element_count(iel)', 'EQ', IONE_ARR, IDUM1X(1:), NERR, LDUM)

      BKXYOK = COUNT == NERR

      ! ICMREF part 1
      IDUM1(1) = NEL
      IDUM1(2) = -NLFEE
      REFOK = .TRUE.

      DO FACE = 1, 4
         COUNT = NERR

         CALL ALCHKI(ERRLVL_error, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)', 'LE', IDUM1(1:1), ICMREF(1:, FACE, 2), NERR, LDUM)
         CALL ALCHKI(ERRLVL_error, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)', 'GE', IDUM1(2:2), ICMREF(1:, FACE, 2), NERR, LDUM)

         IF (COUNT == NERR) THEN
            DO IEL = 1, NEL
               IADJ = ICMREF(IEL, FACE, 2)
               IF (IADJ <= 0) THEN
                  IDUM(IEL) = 0
               ELSE
                  FADJ = ICMREF(IEL, FACE, 3)
                  IF (FADJ < 1 .OR. FADJ > 4) THEN
                     IDUM(IEL) = 1
                  ELSE
                     IF (ICMREF(IADJ, FADJ, 2) /= IEL) THEN
                        IDUM(IEL) = 2
                     ELSE
                        IDUM(IEL) = 0
                        IF (ICMREF(IADJ, FADJ, 3) /= FACE) IDUM(IEL) = 3
                     END IF
                  END IF
               END IF
            END DO
            CALL ALCHKI(ERRLVL_error, 2078, SPR, 1, NEL, FACE, IUNDEF, 'status_of_ICMREF(iel,face)', 'EQ', IZERO_ARR, IDUM, NERR, LDUM)
         END IF
         REFOK = REFOK .AND. COUNT == NERR
      END DO

      ! ICMREF part 2 (bank element neighbours)
      IF (NLF > 0 .AND. BEXBK .AND. BKXYOK .AND. REFOK) THEN
         IDUM1X(-1) = -2
         IDUM1X(0) = 0

         DO IEL = 1, NEL
            IDUM1X(IEL) = -2
         END DO

         DO IY = 1, NY
            DO IX = 1, NX
               IEL = MAX(0, ICMXY(IX, IY))
               IDUM1X(IEL) = MIN(IEL, 1)
            END DO
         END DO

         DO LINK = 1, NLF
            IDUM(LINK) = 0
         END DO

         DO BANK = 1, 2
            DO LINK = 1, NLF
               IEL = ICMBK(LINK, BANK)
               FACE = 2 * BANK
               IF (LINKNS(LINK)) FACE = FACE - 1
               IADJ = MAX(-1, ICMREF(IEL, FACE, 2))
               IDUM(LINK) = IDUM(LINK) + IDUM1X(IADJ)
            END DO
         END DO

         CALL ALCHKI(ERRLVL_error, 2079, SPR, 1, NLF, IUNDEF, IUNDEF, '#_grids_neighbouring_banks(link)', 'GT', IZERO_ARR, IDUM, NERR, LDUM)
      END IF

      ! ICMRF2
      IF (REFOK) THEN
         DO IBR = 1, NLFEE
            IDUM(IBR) = -1
         END DO

         DO FACE = 1, 4
            DO IEL = 1, NEL
               IADJ = ICMREF(IEL, FACE, 2)
               IF (IADJ < 0) THEN
                  IBR = -IADJ
                  IF (IDUM(IBR) >= 0) THEN
                     IDUM(IBR) = IDUM(IBR) + 1
                  ELSE
                     IDUM(IBR) = 0

                     DO P = 1, 3
                        IADJ = ICMRF2(IBR, P, 1)
                        IF (IADJ > NEL) THEN
                           IDUM(IBR) = IDUM(IBR) + P * 10
                        ELSE IF (IADJ > 0) THEN
                           FADJ = ICMRF2(IBR, P, 2)
                           IF (FADJ < 1 .OR. FADJ > 4) THEN
                              IDUM(IBR) = IDUM(IBR) + P * 100
                           ELSE
                              IBRADJ = -ICMREF(IADJ, FADJ, 2)
                              IF (IBRADJ < 1 .OR. IBRADJ > NLFEE) THEN
                                 IDUM(IBR) = IDUM(IBR) + P * 1000
                              ELSE

                                 search_padj: DO PADJ = 1, 3
                                    IELP = ICMRF2(IBRADJ, PADJ, 1)
                                    IF (IELP == IEL) THEN
                                       FEL = ICMRF2(IBRADJ, PADJ, 2)
                                       IF (FEL == FACE) EXIT search_padj
                                    END IF
                                 END DO search_padj

                                 IF (PADJ > 3) IDUM(IBR) = IDUM(IBR) + P * 10000

                              END IF
                           END IF
                        END IF
                     END DO
                  END IF
               END IF
            END DO
         END DO

         CALL ALCHKI(ERRLVL_error, 2080, SPR, 1, NLFEE, IUNDEF, IUNDEF, 'status_of_ICMRF2(branch)', 'LE', IZERO_ARR, IDUM, NERR, LDUM)
      END IF

      ! 2. Soil Properties
      ! ------------------
      CALL ALCHK(ERRLVL_error, 2063, SPR, 1, NS, IUNDEF, IUNDEF, 'THSAT(soil)', 'LE', ONE_ARR, ZERO_VAL, THSAT, NERR, LDUM)

      ! 3. Link Properties & Initial State
      ! ----------------------------------
      IF (NLF > 0) THEN
         CALL ALCHK(ERRLVL_error, 2064, SPR, 1, NLF, IUNDEF, IUNDEF, 'CLENTH(link)', 'GE', ZERO_ARR, ZERO_VAL, CLENTH, NERR, LDUM)
         CALL ALCHK(ERRLVL_error, 2065, SPR, 1, NLF, IUNDEF, IUNDEF, 'CWIDTH(link)', 'GT', ZERO_ARR, ZERO_VAL, CWIDTH, NERR, LDUM)
         CALL ALCHK(ERRLVL_error, 2066, SPR, 1, NLF, IUNDEF, IUNDEF, 'ZBFULL(link)', 'GEa', ZGRUND(1:), ZERO_VAL, ZBFULL, NERR, LDUM)
         CALL ALCHK(ERRLVL_error, 2067, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', 'GE', ZERO_ARR, ZERO_VAL, ARXL, NERR, LDUM)
      END IF

      ! 4. Column Properties & Initial State
      ! ------------------------------------
      CALL ALCHK(ERRLVL_error, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DXQQ(iel)', 'GT', ZERO_ARR, ZERO_VAL, DXQQ(ICOL1:), NERR, LDUM)
      CALL ALCHK(ERRLVL_error, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DYQQ(iel)', 'GT', ZERO_ARR, ZERO_VAL, DYQQ(ICOL1:), NERR, LDUM)
      CALL ALCHK(ERRLVL_error, 2069, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', 'GEa', ZGRUND(ICOL1:), ZERO_VAL, HRF(ICOL1:), NERR, LDUM)

      COUNT = NERR
      IDUM1(1) = NLYREE
      CALL ALCHKI(ERRLVL_error, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'GT', IZERO_ARR, NLYR(ICOL1:), NERR, LDUM)
      CALL ALCHKI(ERRLVL_error, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'LE', IDUM1(1:1), NLYR(ICOL1:), NERR, LDUM)

      IF (COUNT == NERR) THEN
         DO IEL = ICOL1, NEL
            ILYR = NLYR(IEL)
            IDUM(IEL) = NTSOIL(IEL, ILYR)
         END DO
         IDUM1(1) = NS
         CALL ALCHKI(ERRLVL_error, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NTSOIL[iel,NLYR(iel)]', 'GT', IZERO_ARR, IDUM(ICOL1:), NERR, LDUM)
         CALL ALCHKI(ERRLVL_error, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NTSOIL[iel,NLYR(iel)]', 'LE', IDUM1(1:1), IDUM(ICOL1:), NERR, LDUM)
      END IF

      COUNT = NERR
      IDUM1(1) = NV
      CALL ALCHKI(ERRLVL_error, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'GT', IZERO_ARR, NVC(ICOL1:), NERR, LDUM)
      CALL ALCHKI(ERRLVL_error, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'LE', IDUM1(1:1), NVC(ICOL1:), NERR, LDUM)

      ! 5. Element Properties
      ! ---------------------
      CALL ALCHK(ERRLVL_error, 2073, SPR, 1, NEL, IUNDEF, IUNDEF, 'AREA(iel)', 'GT', ZERO_ARR, ZERO_VAL, AREA, NERR, LDUM)
      DO FACE = 1, 4
         CALL ALCHK(ERRLVL_error, 2074, SPR, 1, NEL, FACE, IUNDEF, 'DHF(iel,face)', 'GT', ZERO_ARR, ZERO_VAL, DHF(1:, FACE), NERR, LDUM)
      END DO

      ! 6. Epilogue
      ! -----------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 2001, SPR, 0, 0, 'Error(s) detected while checking static/initial WAT-SY interface')
      END IF

   END SUBROUTINE SYERR1



!> Checks sediment input arrays and category assignments.
!>
!> `SYERR2` validates the sediment data read from the manual's `SY11`-`SY64`
!> input groups after the water-sediment interface has been checked. It also
!> normalises several scalar values through the local `IDUM`/`DUMMY` work arrays
!> after `ALCHK`/`ALCHKI` have applied bounds.
!>
!> Main validation groups:
!>
!> | Group | Checks performed |
!> |:------|:-----------------|
!> | Control scalars | `NEPS >= 1`, `FPCRIT >= 0`, `DLSMAX >= 0`, and enough `NELEE` workspace for sediment work arrays. |
!> | Channel-only controls | When links exist, `0 <= ISUSED <= 1`, `0 <= NFINE <= min(1,NSED-1)`, `ALPHA >= 0` if fines exist, and `DCBEDO >= 0`. |
!> | Particle sizes | `DRSED(1) > 0` and subsequent representative diameters are non-decreasing. |
!> | Soil properties | `GKR >= 0`, `GKF >= 0`, `RHOSO > 0`, `BKB >= 0` for channel runs, and each `SOSDFN` row is non-negative and sums to 1 within tolerance. |
!> | Vegetation/drip properties | `XDRIP >= 0`, `DRDRIP > 0`, and `FDRIP >= 0`. |
!> | Link properties | Bank soil type `NTSOBK` is in `1:NS`, and bed porosity satisfies `0 <= PBSED < 1`. |
!> | Column properties | `FCROCK <= 1`, `FCG <= 1-FCROCK`, and loose-sediment porosity `0 <= PLS < 1`. |
!> | Initial state | `DLS >= 0`, all `FBETA >= 0` with each element summing to 1, and all mobile concentrations `FDEL >= 0`. |
!> | Boundary metadata | If sediment boundary records exist, category storage is large enough, boundary elements and faces are valid external faces, category numbers are in range, `GBC >= 0`, `ABC >= 0`, `BBC > 0`, and selected boundary file/unit references are non-negative. |
!>
!> The manual notes that sediment boundary-condition routines are not yet
!> implemented; this routine still validates `SY61`-`SY64` metadata and rating
!> coefficients so invalid input is caught consistently. If any failures are
!> found, fatal error 2000 is raised before returning.
!>
!> @note The workspace check labelled `NELEE` compares the required sediment
!> workspace with `NXEE*NYEE` stored in `IDUM`, not directly with the `NELEE`
!> argument. Later guarded sections still use `NELEE` to avoid overrunning
!> local arrays.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-05-04 | SvB | 4.6.1 | Changed the local `RDUM` workspace from an automatic array sized `NXEE*NYEE` to `ALLOCATABLE`, to reduce stack usage. |
!> | 2026-05-03 | SvB | 4.6.1 | Replaced an uninitialised local `IUNDEF` "don't care" argument to `ALCHK`/`ALCHKI` with an explicit `PARAMETER = 0`. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV, NSYB, NSYBEE, &
      NSYC, NSYCEE, SPR, ICMREF, ISUSED, NEPS, NFINE, SFB, SRB, ALPHA, DCBEDO,      &
      FPCRIT, DLSMAX, NTSOBK, NSYBCD, NBFACE, DRSED, BKB, GKF, GKR, RHOSO, SOSDFN,  &
      DRDRIP, FDRIP, XDRIP, PBSED, FCG, FCROCK, PLS, DLS, FBETA, FDEL, ABC, BBC,    &
      GBC, IDUM, DUMMY, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: NXEE   !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NYEE   !! Grid-row workspace dimension.
      INTEGER, INTENT(IN) :: NEL    !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NS     !! Number of soil types.
      INTEGER, INTENT(IN) :: NSEE   !! Soil-type array dimension.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      INTEGER, INTENT(IN) :: NV     !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NSYB   !! Number of sediment boundary entries.
      INTEGER, INTENT(IN) :: NSYBEE !! Sediment-boundary array dimension.
      INTEGER, INTENT(IN) :: NSYC (4) !! Number of sediment boundary categories by boundary type.
      INTEGER, INTENT(IN) :: NSYCEE !! Sediment-boundary-category array dimension.
      INTEGER, INTENT(IN) :: SPR    !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: ICMREF (NELEE, 4, 2:2) !! Face-neighbour reference map.
      INTEGER, INTENT(IN) :: NBFACE (NEL) !! Number of boundary faces by element.
      INTEGER, INTENT(IN) :: SFB !! Sediment boundary file unit.
      INTEGER, INTENT(IN) :: SRB !! Sediment rating-boundary file unit.

      ! Input/Output arguments (Variables modified via ALCHK/ALCHKI checking/casting)
      INTEGER, INTENT(INOUT) :: ISUSED !! Sediment velocity option.
      INTEGER, INTENT(INOUT) :: NEPS   !! Number of sediment substeps per water timestep.
      INTEGER, INTENT(INOUT) :: NFINE  !! Number of fine sediment classes.
      INTEGER, INTENT(INOUT) :: NTSOBK (NLFEE)     !! Bank soil type by link.
      INTEGER, INTENT(INOUT) :: NSYBCD (NSYBEE, 3) !! Sediment boundary element, type, and category metadata.
      DOUBLE PRECISION, INTENT(INOUT) :: ALPHA  !! Fine-sediment settling/resuspension critical-shear ratio.
      DOUBLE PRECISION, INTENT(INOUT) :: DCBEDO !! Active upper channel-bed layer thickness.
      DOUBLE PRECISION, INTENT(INOUT) :: FPCRIT !! Maximum sediment concentration fraction.
      DOUBLE PRECISION, INTENT(INOUT) :: DLSMAX !! Loose-sediment depth above which hillslope soil erosion is suppressed.
      DOUBLE PRECISION, INTENT(INOUT) :: DRSED (NSED) !! Representative sediment particle diameters.
      DOUBLE PRECISION, INTENT(INOUT) :: BKB (NS)   !! Bank erodibility by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: GKF (NS)   !! Flow detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: GKR (NS)   !! Rainfall detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: RHOSO (NS) !! Soil bulk density by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: SOSDFN (NSEE, NSED) !! Soil sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: DRDRIP (NV) !! Canopy drip drop diameter by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: FDRIP (NV)  !! Canopy drip fraction by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: XDRIP (NV)  !! Canopy drip fall height by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: PBSED (NLFEE) !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(INOUT) :: FCG (NLF + 1:NEL)    !! Ground-cover fraction by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: FCROCK (NLF + 1:NEL) !! Rock-cover fraction by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: PLS (NLF + 1:NEL)    !! Loose-sediment porosity by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: DLS (NEL) !! Loose/bed sediment depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA (NELEE, NSED) !! Sediment composition fraction by element and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FDEL (NELEE, NSED)  !! Mobile sediment concentration fraction by element and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: ABC (NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `A` by sediment class/category.
      DOUBLE PRECISION, INTENT(INOUT) :: BBC (NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `B` by sediment class/category.
      DOUBLE PRECISION, INTENT(INOUT) :: GBC (NSEDEE, NSYCEE) !! Steady boundary sediment input by class/category.

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT)      :: IDUM  !! Integer workspace for grid/category checks.
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY !! Floating-point workspace for element checks.
      LOGICAL, DIMENSION(NELEE), INTENT(INOUT)          :: LDUM  !! Logical workspace for element checks.

      DOUBLE PRECISION, PARAMETER :: TOL = 1.0D-10

      INTEGER :: BB, COUNT, FACE, ICAT, IEL, ITYPE, NERR
      INTEGER, PARAMETER :: IUNDEF = 0
      INTEGER :: SED, SOIL, jedumdum
      INTEGER :: IDUM1 (1)
      DOUBLE PRECISION, ALLOCATABLE :: RDUM (:)

      INTEGER(KIND=I_P) :: ios

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------
      !     * Local counter
      ALLOCATE (RDUM (NXEE*NYEE), STAT=ios)
      CALL errstat_alloc(ios, "RDUM", "SYmod:SYERR2")

      NERR = 0

      ! 1. Static Variables
      ! -------------------

      ! NEPS
      IDUM (1) = NEPS
      CALL ALCHKI (ERRLVL_error, 2012, SPR, 1, 1, IUNDEF, IUNDEF, 'NEPS', 'GE', IONE1, IDUM, NERR, LDUM)
      NEPS = IDUM (1)

      ! FPCRIT
      DUMMY (1) = FPCRIT
      CALL ALCHK (ERRLVL_error, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'FPCRIT', 'GE', ZERO1, ZERO1 (1), DUMMY, NERR, LDUM)
      FPCRIT = DUMMY (1)

      ! DLSMAX
      DUMMY (1) = DLSMAX
      CALL ALCHK (ERRLVL_error, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'DLSMAX', 'GE', ZERO1, ZERO1 (1), DUMMY, NERR, LDUM)
      DLSMAX = DUMMY (1)

      IF (NLF > 0) THEN
         ! ISUSED
         IDUM (1) = ISUSED
         CALL ALCHKI (ERRLVL_error, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', 'GE', IZERO1, IDUM, NERR, LDUM)
         CALL ALCHKI (ERRLVL_error, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', 'LE', IONE1, IDUM, NERR, LDUM)
         ISUSED = IDUM (1)

         ! NFINE
         IDUM (1) = NFINE
         IDUM1 (1) = MIN (1, NSED - 1)
         CALL ALCHKI (ERRLVL_error, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', 'GE', IZERO1, IDUM, NERR, LDUM)
         CALL ALCHKI (ERRLVL_error, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', 'LE', IDUM1, IDUM, NERR, LDUM)
         NFINE = IDUM (1)

         ! ALPHA
         IF (NFINE > 0) THEN
            DUMMY (1) = ALPHA
            CALL ALCHK (ERRLVL_error, 2016, SPR, 1, 1, IUNDEF, IUNDEF, 'ALPHA', 'GE', ZERO1, ZERO1 (1), DUMMY, NERR, LDUM)
            ALPHA = DUMMY (1)
         END IF

         ! DCBEDO
         DUMMY (1) = DCBEDO
         CALL ALCHK (ERRLVL_error, 2017, SPR, 1, 1, IUNDEF, IUNDEF, 'DCBEDO', 'GE', ZERO1, ZERO1 (1), DUMMY, NERR, LDUM)
         DCBEDO = DUMMY (1)
      END IF

      ! NELEE
      IDUM (1) = NXEE * NYEE
      jedumdum = IDIMJE(NSED, NFINE)
      jedumdum = jedumdum * NLF
      IDUM1(1) = MAX(NSED, jedumdum)

      ! * (including local workspace requirements)
      IDUM1 (1) = MAX (IDUM1 (1), NS, NSYB * 2)
      CALL ALCHKI (ERRLVL_error, 2018, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUM1, IDUM, NERR, LDUM)


      ! 2. Sediment, Soil & Vegetation Properties
      ! -----------------------------------------
      !
      ! * Not enough workspace? (Converted GOTO 300 to block IF)
      IF (NELEE >= MAX (NSED, NS)) THEN

         ! DRSED
         COUNT = NERR
         CALL ALCHK (ERRLVL_error, 2019, SPR, 1, 1, IUNDEF, IUNDEF, 'DRSED(sed)', 'GT', ZERO1, ZERO1 (1), DRSED (1), NERR, LDUM)

         IF (NSED > 1 .AND. NERR == COUNT) THEN
            CALL DCOPY (NSED - 1, DRSED, 1, RDUM, 1)
            IDUM(1:NSED - 1) = INT (RDUM(1:NSED - 1))
            CALL ALCHK (ERRLVL_error, 2019, SPR, 2, NSED, IUNDEF, IUNDEF, 'DRSED(sed)', 'GEa', RDUM, ZERO1 (1), DRSED (2), NERR, LDUM)
         END IF

         ! GKR
         CALL ALCHK (ERRLVL_error, 2020, SPR, 1, NS, IUNDEF, IUNDEF, 'GKR(soil)', 'GE', ZERO1, ZERO1 (1), GKR, NERR, LDUM)
         ! GKF
         CALL ALCHK (ERRLVL_error, 2021, SPR, 1, NS, IUNDEF, IUNDEF, 'GKF(soil)', 'GE', ZERO1, ZERO1 (1), GKF, NERR, LDUM)
         ! RHOSO
         CALL ALCHK (ERRLVL_error, 2022, SPR, 1, NS, IUNDEF, IUNDEF, 'RHOSO(soil)', 'GT', ZERO1, ZERO1 (1), RHOSO, NERR, LDUM)

         ! BKB
         IF (NLF > 0) THEN
            CALL ALCHK (ERRLVL_error, 2023, SPR, 1, NS, IUNDEF, IUNDEF, 'BKB(soil)', 'GE', ZERO1, ZERO1 (1), BKB, NERR, LDUM)
         END IF

         ! SOSDFN
         DUMMY(1:NS) = ZERO1 (1)
         DO SED = 1, NSED
            DO SOIL = 1, NS
               DUMMY (SOIL) = DUMMY (SOIL) + SOSDFN (SOIL, SED)
            END DO
            CALL ALCHK (ERRLVL_error, 2024, SPR, 1, NS, SED, IUNDEF, 'SOSDFN(soil,sed)', 'GE', ZERO1, ZERO1 (1), SOSDFN (1, SED), NERR, LDUM)
         END DO
         CALL ALCHK (ERRLVL_error, 2024, SPR, 1, NS, IUNDEF, IUNDEF, 'SOSDFN[*][sum_over_sed](soil)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)

         ! XDRIP
         CALL ALCHK (ERRLVL_error, 2025, SPR, 1, NV, IUNDEF, IUNDEF, 'XDRIP(veg)', 'GE', ZERO1, ZERO1 (1), XDRIP, NERR, LDUM)
         ! DRDRIP
         CALL ALCHK (ERRLVL_error, 2026, SPR, 1, NV, IUNDEF, IUNDEF, 'DRDRIP(veg)', 'GT', ZERO1, ZERO1 (1), DRDRIP, NERR, LDUM)
         ! FDRIP
         CALL ALCHK (ERRLVL_error, 2027, SPR, 1, NV, IUNDEF, IUNDEF, 'FDRIP(veg)', 'GE', ZERO1, ZERO1 (1), FDRIP, NERR, LDUM)

      END IF


      ! 3. Link Element Properties
      ! --------------------------
      !
      IF (NLF > 0) THEN
         ! NTSOBK
         IDUM (1) = NS
         CALL ALCHKI (ERRLVL_error, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'GE', IONE1, NTSOBK, NERR, LDUM)
         CALL ALCHKI (ERRLVL_error, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'LE', IDUM, NTSOBK, NERR, LDUM)
         ! PBSED
         CALL ALCHK (ERRLVL_error, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', 'GE', ZERO1, ZERO1 (1), PBSED, NERR, LDUM)
         CALL ALCHK (ERRLVL_error, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', 'LT', ONE1, ZERO1 (1), PBSED, NERR, LDUM)
      END IF


      ! 4. Column-element Properties
      ! ----------------------------
      !
      ! FCROCK
      CALL ALCHK (ERRLVL_error, 2030, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCROCK(iel)', 'LE', ONE1, ZERO1 (1), FCROCK, NERR, LDUM)

      ! FCG
      DO IEL = NLF + 1, NEL
         DUMMY (IEL) = ONE1 (1) - FCROCK (IEL)
      END DO
      CALL ALCHK (ERRLVL_error, 2031, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCG(iel)', 'LEa', DUMMY (NLF + 1), ZERO1 (1), FCG, NERR, LDUM)

      ! PLS
      CALL ALCHK (ERRLVL_error, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'GE', ZERO1, ZERO1 (1), PLS, NERR, LDUM)
      CALL ALCHK (ERRLVL_error, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'LT', ONE1, ZERO1 (1), PLS, NERR, LDUM)


      ! 5. All-element Initialization
      ! -----------------------------
      !
      ! DLS
      CALL ALCHK (ERRLVL_error, 2033, SPR, 1, NEL, IUNDEF, IUNDEF, 'DLS(iel)', 'GE', ZERO1, ZERO1 (1), DLS, NERR, LDUM)

      ! FBETA
      DUMMY(1:NEL) = ZERO1 (1)
      DO SED = 1, NSED
         DO IEL = 1, NEL
            DUMMY (IEL) = DUMMY (IEL) + FBETA (IEL, SED)
         END DO
         CALL ALCHK (ERRLVL_error, 2034, SPR, 1, NEL, SED, IUNDEF, 'FBETA(iel,sed)', 'GE', ZERO1, ZERO1 (1), FBETA (1, SED), NERR, LDUM)
      END DO
      CALL ALCHK (ERRLVL_error, 2034, SPR, 1, NEL, IUNDEF, IUNDEF, 'FBETA[*][sum_over_sed](iel)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)

      ! FDEL
      DO SED = 1, NSED
         CALL ALCHK (ERRLVL_error, 2035, SPR, 1, NEL, SED, IUNDEF, 'FDEL(iel,sed)', 'GE', ZERO1, ZERO1 (1), FDEL (1, SED), NERR, LDUM)
      END DO


      ! 6. Boundary Data
      ! ----------------
      !
      IF (NSYB > 0) THEN
         IF (NELEE >= NSYB * 2) THEN

            ! NSYCEE
            IDUM (1) = NSYCEE
            IDUM1 (1) = MAX (NSYC (1) + NSYC (2), NSYC (3) + NSYC (4))
            CALL ALCHKI (ERRLVL_error, 2036, SPR, 1, 1, IUNDEF, IUNDEF, 'NSYCEE', 'GE', IDUM1, IDUM, NERR, LDUM)

            ! NSYBCD(BB,1)
            COUNT = NERR
            IDUM1 (1) = NEL
            CALL ALCHKI (ERRLVL_error, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', 'GE', IONE1, NSYBCD, NERR, LDUM)
            CALL ALCHKI (ERRLVL_error, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', 'LE', IDUM1, NSYBCD, NERR, LDUM)

            ! NBFACE
            IF (COUNT == NERR) THEN
               DO BB = 1, NSYB
                  IEL = NSYBCD (BB, 1)
                  IDUM (BB) = NBFACE (IEL)
               END DO
               IDUM1 (1) = 4
               CALL ALCHKI (ERRLVL_error, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, 'NBFACE[NSYBCD[*][1]](bdry)', 'GE', IONE1, IDUM, NERR, LDUM)
               CALL ALCHKI (ERRLVL_error, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, 'NBFACE[NSYBCD[*][1]](bdry)', 'LE', IDUM1, IDUM, NERR, LDUM)
            END IF

            ! ICMREF
            IF (COUNT == NERR) THEN
               DO BB = 1, NSYB
                  IEL = NSYBCD (BB, 1)
                  FACE = NBFACE (IEL)
                  IDUM (BB) = ICMREF (IEL, FACE, 2)
               END DO
               CALL ALCHKI (ERRLVL_error, 2039, SPR, 1, NSYB, IUNDEF, IUNDEF, 'ICMREF[NSYBCD[*][1]][NBFACE][2](bdry)', 'EQ', IZERO1, IDUM, NERR, LDUM)
            END IF

            ! NSYBCD(BB,3)
            DO BB = 1, NSYB
               ITYPE = NSYBCD (BB, 2)
               IDUM (BB) = 1
               IF (MOD (ITYPE, 2) == 0) IDUM (BB) = IDUM (BB) + NSYC (ITYPE - 1)
               IDUM (NSYB + BB) = IDUM (BB) + NSYC (ITYPE)
            END DO
            CALL ALCHKI (ERRLVL_error, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', 'GE', IDUM, NSYBCD (1, 3), NERR, LDUM)
            CALL ALCHKI (ERRLVL_error, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', 'LE', IDUM (NSYB + 1), NSYBCD (1, 3), NERR, LDUM)

            ! GBC
            DO ICAT = 1, NSYC (1)
               CALL ALCHK (ERRLVL_error, 2041, SPR, 1, NSED, ICAT, IUNDEF, 'GBC(sed,icat)', 'GE', ZERO1, ZERO1 (1), GBC (1, ICAT), NERR, LDUM)
            END DO

            ! ABC
            DO ICAT = 1, NSYC (3)
               CALL ALCHK (ERRLVL_error, 2042, SPR, 1, NSED, ICAT, IUNDEF, 'ABC(sed,icat)', 'GE', ZERO1, ZERO1 (1), ABC (1, ICAT), NERR, LDUM)
            END DO

            ! BBC
            DO ICAT = 1, NSYC (3)
               CALL ALCHK (ERRLVL_error, 2043, SPR, 1, NSED, ICAT, IUNDEF, 'BBC(sed,icat)', 'GT', ZERO1, ZERO1 (1), BBC (1, ICAT), NERR, LDUM)
            END DO

            ! SFB
            IF (NSYC (2) > 0) THEN
               IDUM (1) = SFB
               CALL ALCHKI (ERRLVL_error, 2044, SPR, 1, 1, IUNDEF, IUNDEF, 'SFB', 'GE', IZERO1, IDUM, NERR, LDUM)
            END IF

            ! SRB
            IF (NSYC (2) > 0) THEN
               IDUM (1) = SRB
               CALL ALCHKI (ERRLVL_error, 2045, SPR, 1, 1, IUNDEF, IUNDEF, 'SRB', 'GE', IZERO1, IDUM, NERR, LDUM)
            END IF
         END IF
      END IF


      ! 7. Epilogue
      ! -----------
      !
      IF (NERR > 0) CALL RAISE_ERROR (ERRLVL_fatal, 2000, SPR, 0, 0, 'Error(s) detected while checking SY input data')

   END SUBROUTINE SYERR2



!> Checks time-dependent water-flow values before a sediment timestep.
!>
!> `SYERR3` validates the current water-model state passed to the sediment
!> component. It is called at runtime, after the static interface checks, to
!> catch non-physical transient values and routing-order inconsistencies before
!> erosion and sediment advection use them.
!>
!> Main validation groups:
!>
!> | Group | Checks performed |
!> |:------|:-----------------|
!> | Time step | `DTUZ >= 0`. |
!> | Vegetation state | `CLAI >= 0` and `0 <= PLAI <= 1`. |
!> | Link state | `ARXL >= 0` for active links. |
!> | Column water inputs | `DRAINA >= 0` and `DRAINA <= PNETTO` within tolerance. |
!> | Element water level | `HRF >= ZGRUND`. |
!> | Flow consistency | Adjacent regular faces must not both discharge into each other (`status=1`), branch outflows must have a receiving neighbour (`status=2`), and donor elements must precede receptors in `ISORT`. |
!>
!> Face outflow is interpreted as
!>
!> \[
!>   Q_{out}(iel,face) = \operatorname{sign}(1,2-face)\,QOC(iel,face).
!> \]
!>
!> With the implemented Fortran `SIGN` rule, positive `QOC` is outflow on faces
!> 1 and 2, while negative `QOC` is outflow on faces 3 and 4.
!>
!> The routine builds `JSORT`, the inverse of `ISORT`, and `JMIN`, the earliest
!> receptor position required by each donor. Any flow-order failure is reported
!> with `ALCHKI`. If errors are found, the relevant transient arrays are written
!> to `SPR`, then error 2003 is raised before returning.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-06 | SvB | 4.6.1 | Replaced the `GOTO 640` non-discharge-face skip with `CYCLE element_loop`, and the legacy statement function used to evaluate face outflow with the internal `FUNCTION` `FNQOUT`. |
!> | 2026-05-03 | SvB | 4.6.1 | Replaced an uninitialised local `IUNDEF` "don't care" argument to `ALCHK`/`ALCHKI` with an explicit `PARAMETER = 0`. |
!> @endhistory
   SUBROUTINE SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, &
      ICMRF2, ISORT, DTUZ, CLAI, PLAI, ARXL, DRAINA, PNETTO, HRF, &
      ZGRUND, QOC, IQ, JMIN, JSORT, LDUM)

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN) :: NEL   !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE !! Link-array dimension.
      INTEGER, INTENT(IN) :: NV    !! Number of vegetation types.
      INTEGER, INTENT(IN) :: SPR   !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: ICMREF (NELEE, 4, 2:3) !! Face-neighbour and reverse-face reference map.
      INTEGER, INTENT(IN) :: ICMRF2 (NLFEE, 3, 2)   !! Confluence branch reference map.
      INTEGER, INTENT(IN) :: ISORT (NEL) !! Donor-before-receptor element routing order.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(INOUT) :: CLAI (NV)    !! Current canopy leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: PLAI (NV)    !! Potential/maximum leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: ARXL (NLFEE) !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(INOUT) :: DRAINA (NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
      DOUBLE PRECISION, INTENT(INOUT) :: HRF (NEL)    !! Water level/head by element.
      DOUBLE PRECISION, INTENT(IN) :: PNETTO (NLF + 1:NEL) !! Net precipitation/effective rainfall by land element.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND (NEL) !! Ground or bed elevation by element.
      DOUBLE PRECISION, INTENT(IN) :: QOC (NELEE, 4) !! Face water fluxes.

      ! Workspace arguments
      INTEGER :: IQ (NEL)    !! Per-face donor/receptor flow-consistency status by element.
      INTEGER :: JMIN (NEL)  !! Earliest required `ISORT` position for each element's receptors.
      INTEGER :: JSORT (0:NEL + 1) !! Inverse of `ISORT`: position of each element in the routing order.
      LOGICAL :: LDUM (NELEE) !! Logical workspace for `ALCHK`/`ALCHKI` checks.

      DOUBLE PRECISION, PARAMETER :: TOL = 1.0D-7
      !
      INTEGER :: FACE, FADJ, I, IADJ, IBR, IEL, J, NELP, NERR, P
      INTEGER, PARAMETER :: IUNDEF = 0
      DOUBLE PRECISION :: QADJ, QMIN
      DOUBLE PRECISION :: DUM1 (1)

      !----------------------------------------------------------------------*
      !
      ! 0. Preliminaries
      ! ----------------
      !
      !     * Initialize local counter
      NERR = 0
      !
      !
      ! 1. Variables
      ! ------------
      !
      ! DTUZ
      DUM1 (1) = DTUZ
      CALL ALCHK (ERRLVL_error, 2046, SPR, 1, 1, IUNDEF, IUNDEF, 'DTUZ', 'GE', &
         zero1, zero1 (1), DUM1, NERR, LDUM)
      !
      !
      ! 2. Vegetative State
      ! -------------------
      !
      ! CLAI
      CALL ALCHK (ERRLVL_error, 2047, SPR, 1, NV, IUNDEF, IUNDEF, 'CLAI(veg)', &
         'GE', zero1, zero1 (1), CLAI, NERR, LDUM)
      ! PLAI
      CALL ALCHK (ERRLVL_error, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
         'GE', zero1, zero1 (1), PLAI, NERR, LDUM)
      CALL ALCHK (ERRLVL_error, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
         'LE', ONE1, ZERO1 (1), PLAI, NERR, LDUM)
      !
      !
      ! 3. Link State
      ! -------------
      !
      IF (NLF > 0) THEN
         !
         ! ARXL
         CALL ALCHK (ERRLVL_error, 2049, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
            'GE', zero1, zero1 (1), ARXL, NERR, LDUM)
         !
      END IF
      !
      !
      ! 4. Columnar State
      ! -----------------
      !
      ! DRAINA
      CALL ALCHK (ERRLVL_error, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'GE', zero1, zero1 (1), DRAINA, NERR, LDUM)
      ! 10.10.94  Ought to fix WAT module so that we don't need TOL
      CALL ALCHK (ERRLVL_error, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'LEa', PNETTO, TOL, DRAINA, NERR, LDUM)
      !
      !
      ! 5. Elemental State
      ! ------------------
      !
      ! HRF
      CALL ALCHK (ERRLVL_error, 2051, SPR, 1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', &
         'GEa', ZGRUND, ZERO1 (1), HRF, NERR, LDUM)
      !
      !
      ! 6. Flux/Ordering
      ! ----------------
      !
      ! ISORT & QOC
      !     * Set JSORT = inverse of ISORT & initialize upper bound JMIN
      !       (note that JSORT has overspill elements )
      NELP = NEL + 1
      DO J = 0, NELP
         JSORT (J) = NELP
      END DO

      DO I = 1, NEL
         IEL = ISORT (I)
         J = MAX (0, MIN (IEL, NELP))
         JSORT (J) = I
         JMIN (I) = NELP
      END DO

      !     * At this point any element not listed in ISORT has a JSORT
      !       value of NELP, which is guaranteed to fail the test below
      !     * Update JMIN (used as object of JSORT test) & set QOC status IQ
      DO FACE = 1, 4

         element_loop: DO IEL = 1, NEL
            !          * innocent until proven guilty
            IQ (IEL) = 0

            !          * non-discharge faces are ok (Cycle directly replaces GOTO 640)
            IF (FNQOUT(IEL, FACE) <= ZERO1 (1)) CYCLE element_loop

            IADJ = ICMREF (IEL, FACE, 2)

            IF (IADJ > 0) THEN
               FADJ = ICMREF (IEL, FACE, 3)
               QADJ = FNQOUT(IADJ, FADJ)
               !             * do both elements discharge into the same face?
               IF (QADJ > ZERO1 (1)) IQ (IEL) = 1
               !             * IEL must precede IADJ in the ISORT list
               JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL))

            ELSE IF (IADJ < 0) THEN
               IBR = - IADJ
               QMIN = ONE1 (1)

               DO P = 1, 3
                  IADJ = ICMRF2 (IBR, P, 1)
                  IF (IADJ > 0) THEN
                     FADJ = ICMRF2 (IBR, P, 2)
                     QADJ = FNQOUT(IADJ, FADJ)
                     QMIN = MIN (QADJ, QMIN)
                     IF (QADJ < zero1 (1)) THEN
                        !                      * IEL must precede IADJ in the ISORT list
                        JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL))
                     END IF
                  END IF
               END DO

               !             * discharge from IEL has nowhere to go?
               IF (QMIN >= zero1 (1)) IQ (IEL) = 2
            END IF
         END DO element_loop

         !        * Check QOC status at this FACE for all elements
         CALL ALCHKI (ERRLVL_error, 2052, SPR, 1, NEL, FACE, IUNDEF, &
            'status_of_QOC(iel,face)', 'EQ', IZERO1, IQ, NERR, LDUM)

      END DO

      !     * Check that each donor element listed in ISORT occurs before
      !       each of its receptors, and that all elements are listed
      CALL ALCHKI (ERRLVL_error, 2053, SPR, 1, NEL, IUNDEF, IUNDEF, &
         'position_in_ISORT(iel)', 'LTa', JMIN, JSORT (1), NERR, LDUM)
      !
      !
      ! 7. Epilogue
      ! -----------
      !
      IF (NERR > 0) THEN
         !
         WRITE (SPR, 9100) 'DTUZ', DTUZ
         WRITE (SPR, 9100) 'CLAI[veg=1,...,NV]', CLAI
         WRITE (SPR, 9100) 'PLAI[veg=1,...,NV]', PLAI
         WRITE (SPR, 9100) 'ARXL[link=1,...,NLF]', (ARXL (IEL), IEL = 1, NLF)
         WRITE (SPR, 9100) 'DRAINA[col=NLF+1,...,NEL]', DRAINA
         WRITE (SPR, 9100) 'PNETTO[col=NLF+1,...,NEL]', PNETTO
         WRITE (SPR, 9100) 'HRF[iel=1,...,NEL]', (HRF(IEL), IEL = 1, NEL)
         WRITE (SPR, 9100) 'ZGRUND[iel=1,...,NEL]', ZGRUND
         WRITE (SPR, 9200) 'ISORT[iel=1,...,NEL]', ISORT
         WRITE (SPR, 9200) 'position_in_ISORT[iel=1,...,NEL]', (JSORT (IEL), IEL = 1, NEL)

         DO FACE = 1, 4
            WRITE (SPR, 9150) 'QOC[iel=1,...,NEL][face=', FACE, ']', (QOC (IEL, FACE), IEL = 1, NEL)
         END DO
         !
         CALL RAISE_ERROR (ERRLVL_error, 2003, SPR, 0, 0, 'Error(s) detected while checking time-dependent WAT-SY interface')
         !
      END IF

      RETURN

      ! FORMAT STATEMENTS safely at the bottom
9100  FORMAT(1X,A,     ':'/1P,(8E10.2))
9150  FORMAT(1X,A,I1,A,':'/1P,(8E10.2))
9200  FORMAT(1X,A,     ':'/   (16I5  ))

   CONTAINS

      !> Outflow rate at one element/face, positive for outflow (see the routine's face-sign note).
      PURE DOUBLE PRECISION FUNCTION FNQOUT(ELEM, FCE)
         INTEGER, INTENT(IN) :: ELEM !! Element index.
         INTEGER, INTENT(IN) :: FCE  !! Face index (1-4).
         FNQOUT = SIGN(1.0D0, 2.0D0 - DBLE(FCE)) * QOC(ELEM, FCE)
      END FUNCTION FNQOUT

   END SUBROUTINE SYERR3



!> Evaluates fine-sediment settling, infiltration, and armouring limits.
!>
!> `SYFINE` implements the manual's special handling for the single fine
!> sediment fraction (`NFINE = 1`) in channel links. Fines are not assigned a
!> non-fine transport formula; instead [[sycltr]] limits their mobile
!> concentration by `FPCRIT`, and this routine supplies the settling,
!> infiltration, and armouring limits used later by [[sylink]].
!>
!> On the first call, the fine-particle settling velocity is cached from
!> Stokes' law:
!>
!> \[
!>   w_s = {d_f^2 g(\rho_s-\rho_w)\over 18\rho_w\nu},
!> \]
!>
!> where \(d_f\) is `DRSEDF`. For each link, [[sycrit]] is called with the
!> Shields option to obtain the fine-particle critical shear \(\tau_c\). The
!> amount of fine material that can be present in the active upper bed layer is
!>
!> \[
!>   VCFMAX =
!>   AREA\left[
!>     DCBF
!>     + FDELF\,w_s\,\Delta t\,
!>       {\max(\alpha\tau_c-\tau,0)\over \alpha\tau_c}
!>   \right],
!> \]
!>
!> with the settling increment omitted when \(\alpha\tau_c = 0\). This is the
!> existing fine depth in the upper layer plus the amount that can settle under
!> the manual ratio `ALPHA` of settling to resuspension critical shear.
!>
!> The armouring flag is set when bed shear is not strong enough to move the
!> fine material:
!>
!> \[
!>   BARM = (\tau \le \tau_c).
!> \]
!>
!> Potential infiltration into the lower bed layer is allowed only while the
!> fine fraction in the bed is below `FBIC`. If so, it is limited to the mobile
!> fine concentration above the manual threshold `FICRIT`, converted from pore
!> concentration using bed porosity:
!>
!> \[
!>   VINFMX =
!>   w_s\,AREA\,\Delta t\,
!>   \max\left(FDELF - {FICRIT\over 1-PBSED},0\right).
!> \]
!>
!> Otherwise `VINFMX` is zero.
!>
!> @note The settling velocity is saved after the first call. The caller must
!> treat `DRSEDF` as fixed for the simulation, which matches the static
!> sediment-size input.
!> @endnote
!>
!> @note `DUM` is passed to [[sycrit]] as its clay-fraction argument without
!> being set first. This is harmless: [[sycrit]] is called here with `FLAG=0`
!> (the Shields option), which never reads that argument.
!> @endnote
   SUBROUTINE SYFINE(DRSEDF, FBIC, FICRIT, NLF, ALPHA, DTSY, AREA, &
      DCBF, FBETAF, FDELF, PBSED, TAUK, VCFMAX, VINFMX, BARM)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NLF !! Number of channel links.
      DOUBLE PRECISION, INTENT(IN) :: DRSEDF !! Representative fine-sediment particle diameter.
      DOUBLE PRECISION, INTENT(IN) :: FBIC   !! Fine-bed fraction threshold for infiltration.
      DOUBLE PRECISION, INTENT(IN) :: FICRIT !! Fine-concentration threshold for infiltration.
      DOUBLE PRECISION, INTENT(IN) :: ALPHA  !! Fine-sediment settling/resuspension critical-shear ratio.
      DOUBLE PRECISION, INTENT(IN) :: DTSY   !! Sediment substep duration.
      DOUBLE PRECISION, INTENT(IN) :: AREA(NLF)   !! Link bed/contact area used for fine exchange.
      DOUBLE PRECISION, INTENT(IN) :: DCBF(NLF)   !! Active-bed fine sediment depth.
      DOUBLE PRECISION, INTENT(IN) :: PBSED(NLF)  !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(IN) :: FBETAF(NLF) !! Fine fraction in the active bed by link.
      DOUBLE PRECISION, INTENT(IN) :: FDELF(NLF)  !! Mobile fine-sediment concentration fraction by link.
      DOUBLE PRECISION, INTENT(IN) :: TAUK(NLF)   !! Channel/link shear stress.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: VCFMAX(NLF) !! Maximum fine volume available for settling/infiltration.
      DOUBLE PRECISION, INTENT(OUT) :: VINFMX(NLF) !! Maximum fine infiltration volume.
      LOGICAL, INTENT(OUT) :: BARM(NLF) !! True where fine sediment is protected by bed armouring.

      ! Locals, etc
      INTEGER :: LINK
      DOUBLE PRECISION :: DUM, TAUEC, VMAX
      DOUBLE PRECISION :: AREA_L, DCFMXL, FDELFL, TAUKL

      !----------------------------------------------------------------------*

      ! * Calculate settling velocity for fines ( first call only )
      IF (FIRST_syfine) THEN
         FIRST_syfine = .FALSE.
         WSED_syfine = DRSEDF**2 * GRAVTY * (RHOSED - RHOWAT) / (18.0D0 * RHOWAT * VISCOS)
      END IF

      ! * Loop over channel links
      link_loop: DO LINK = 1, NLF

         TAUKL  = TAUK(LINK)
         AREA_L = AREA(LINK)
         FDELFL = FDELF(LINK)

         ! * Calculate critical shear stress for fines
         CALL SYCRIT(0, DRSEDF, TAUKL, DUM, TAUEC)

         ! * Calculate potential fines in upper layer
         ! * (existing fines + settling)
         DUM = ALPHA * TAUEC
         IF (DUM > 0.0D0) DUM = DIMJE(DUM, TAUKL) / DUM
         DCFMXL = DCBF(LINK) + FDELFL * WSED_syfine * DUM * DTSY
         VCFMAX(LINK) = DCFMXL * AREA_L

         ! * Can fines be armoured ?
         BARM(LINK) = (TAUKL <= TAUEC)

         ! * Calculate potential infiltration rate
         VMAX = 0.0D0
         IF (FBETAF(LINK) < FBIC) THEN
            VMAX = WSED_syfine * AREA_L * DIMJE(FDELFL, FICRIT / (1.0D0 - PBSED(LINK))) * DTSY
         END IF
         VINFMX(LINK) = VMAX

      END DO link_loop

   END SUBROUTINE SYFINE



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
   SUBROUTINE SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, NTSOBK, ARXL, DCBEDO, DLS, &
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
      INTEGER, INTENT(IN) :: NTSOBK (NLFEE) !! Bank soil type by link.
      INTEGER, INTENT(IN) :: NTSOTP (NLF + 1:NEL) !! Top soil type by land element.
      DOUBLE PRECISION, INTENT(IN) :: DCBEDO !! Active upper channel-bed layer thickness.
      DOUBLE PRECISION, INTENT(IN) :: ARXL (NLFEE) !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(IN) :: DLS (NEL)    !! Initial loose/bed sediment depth by element.
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NSED) !! Representative sediment particle diameters.
      DOUBLE PRECISION, INTENT(IN) :: FBETA (NELEE, NSED) !! Initial sediment composition by element and size class.
      DOUBLE PRECISION, INTENT(IN) :: HRF (NLF + 1:NEL)   !! Initial land-element water level/head.
      DOUBLE PRECISION, INTENT(IN) :: PBSED (NLFEE)       !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(IN) :: PLS (NLF + 1:NEL)   !! Loose-sediment porosity by land element.
      DOUBLE PRECISION, INTENT(IN) :: SOSDFN (NSEE, NSED) !! Soil sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(IN) :: THSAT (NS)      !! Saturated water content by soil type.
      DOUBLE PRECISION, INTENT(IN) :: ZBFULL (NLFEE)  !! Bankfull elevation/depth by link.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND (NEL)    !! Ground or bed elevation by element.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: ARBDEP (NLFEE) !! Accumulated channel-bed elevation/depth change.
      DOUBLE PRECISION, INTENT(OUT) :: ARXLOL (NLFEE) !! Previous channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(OUT) :: DBFULL (NLFEE) !! Bankfull depth by link.
      DOUBLE PRECISION, INTENT(OUT) :: DCBED (NLFEE)  !! Active upper-bed layer depth by link.
      DOUBLE PRECISION, INTENT(OUT) :: DCBSED (NLFEE, NSED) !! Upper-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(OUT) :: DDBSED (NLFEE, NSED) !! Lower-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(OUT) :: DRSO50 (NS)          !! Median soil particle diameter by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: DWATOL (NLF + 1:NEL) !! Previous water depth by land element.
      DOUBLE PRECISION, INTENT(OUT) :: FETA (NEL) !! Soil-to-sediment solid-volume conversion factor by element.
      DOUBLE PRECISION, INTENT(OUT) :: GINFD (NLFEE, NSED) !! Fine infiltration diagnostic/source for deposited material.
      DOUBLE PRECISION, INTENT(OUT) :: GINFS (NLFEE, NSED) !! Fine infiltration diagnostic/source for suspended material.
      DOUBLE PRECISION, INTENT(OUT) :: GNU (NLF + 1:NEL) !! Hillslope erosion rate by land element.
      DOUBLE PRECISION, INTENT(OUT) :: GNUBK (NLFEE)     !! Lateral bank erosion rate by link.
      DOUBLE PRECISION, INTENT(OUT) :: QSED (NELEE, NSEDEE, 4) !! Sediment flux by element, size class, and face.

      ! Locals, etc
      DOUBLE PRECISION :: DCBEDE, DDBEDE, DLSE, FBETAE
      INTEGER          :: IEL, LINK, SED, SOIL, FACE

      ! External functions implicitly called
      ! DOUBLE PRECISION :: DIMJE, SYDR

      !----------------------------------------------------------------------*

      ! * Initialize surface erosion rates in each column (Replaced ALINIT)
      GNU (NLF + 1 : NEL) = ZERO

      IF (NLF > 0) THEN
         ! * Initialize bank erosion rates in each link (Replaced ALINIT)
         GNUBK(1:NLF) = ZERO

         ! * Zero bed sediment accumulator (Replaced ALINIT)
         ARBDEP(1:NLF) = ZERO

         ! * Set old river c/s area equal to current river c/s area
         CALL DCOPY (NLF, ARXL, 1, ARXLOL, 1)
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
         DLSE = DLS (LINK)

         ! * Set ratio of bank soil to bed sediment solid volume fractions
         FETA (LINK) = (1.0D0 - THSAT (NTSOBK (LINK))) / (1.0D0 - PBSED (LINK))

         ! * Set bank full depth
         DBFULL (LINK) = ZBFULL (LINK) - ZGRUND (LINK)

         ! * Bed layer depths
         DCBEDE = MIN (DLSE, DCBEDO)
         DDBEDE = DIMJE(DLSE, DCBEDE)
         DCBED (LINK) = DCBEDE

         ! * Loop over sediment types
         DO SED = 1, NSED
            ! * Initialize sediment depths in both bed layers
            FBETAE = FBETA (LINK, SED)
            DCBSED (LINK, SED) = DCBEDE * FBETAE
            DDBSED (LINK, SED) = DDBEDE * FBETAE
         END DO

         ! * Next link
      END DO


      ! * Loop over column elements
      DO IEL = NLF + 1, NEL
         ! * Set ratio: surface soil to loose sediment solid vol fractions
         FETA (IEL) = (1.0D0 - THSAT (NTSOTP (IEL))) / (1.0D0 - PLS (IEL))

         ! * Calculate initial surface water depth
         DWATOL (IEL) = HRF (IEL) - ZGRUND (IEL)
      END DO


      ! * Calculate median particle diameter for each soil type
      DO SOIL = 1, NS
         DRSO50 (SOIL) = SYDR (HALF, NSEE, NSED, SOSDFN (SOIL, 1), DRSED)
      END DO

   END SUBROUTINE SYINIT



!> Routes sediment through one channel link.
!>
!> `SYLINK` solves the channel-link sediment balance for one sediment time step
!> after [[sycltr]] has supplied capacity concentrations `CONCI` and advection
!> coefficients `QSDWAE`. Volumes are handled in settled-bed units using the bed
!> solid fraction
!>
!> \[
!>   f_b = 1 - PBSED .
!> \]
!>
!> Faces with positive `QWAT` are outflows; all other faces are treated as
!> inflows for sediment already stored in `QSEDE`. For each size class \(s\),
!> processed from largest to smallest, the incoming settled-volume rate is
!>
!> \[
!>   q_s^{in} = -{1\over f_b}\sum_{in} QSEDE_{s,f},
!> \]
!>
!> and the water volume available for suspended storage plus outgoing advection
!> is
!>
!> \[
!>   V_w = ARXL\,CLENTH + \Delta t\sum_{out} QSDWAE_{s,f}.
!> \]
!>
!> The settled sediment volume available before infiltration and armouring is
!>
!> \[
!>   V_{max,s} =
!>     FDEL_s\,ARXLO\,CLENTH
!>     + DCBSED_s\,AREA
!>     + \Delta t\left(q_s^{in}+EPSB\,SOSDF_s\right),
!> \]
!>
!> combining old suspended load, active upper-bed material, inflow, and bank
!> erosion. Non-fines have no infiltration or armouring in this routine. For
!> fines, infiltration is limited by [[syfine]]:
!>
!> \[
!>   V_{inf} = \min(VINFMX,\min(VCFMAX,V_{max,s})).
!> \]
!>
!> If `BARM` is true, a fraction of the remaining fine material can be armoured
!> according to the ratio of already processed non-fine material in the interim
!> and old active layers:
!>
!> \[
!>   V_{arm} =
!>   {\min(SUMN,SUMP)\over SUMN}\max(\min(VCFMAX,V_{max,s})-V_{inf},0),
!> \]
!>
!> with zero armouring when `SUMN` is zero. The transport supply is therefore
!>
!> \[
!>   V_{supply} = \max(V_{max,s}-V_{inf},0)-V_{arm}.
!> \]
!>
!> The suspended-plus-discharged volume is limited by supply and by the
!> notional capacity concentration from [[sycltr]]:
!>
!> \[
!>   V_{trans} =
!>   \min\left(V_{supply},\,{CONCI_s\over f_b}V_w\right).
!> \]
!>
!> The outputs for the next bed update and channel routing are then
!>
!> \[
!>   FDEL_s^{new} = {V_{trans}\over V_w},\qquad
!>   DCIPR_s = {\max(V_{max,s}-V_{inf},0)-V_{trans}\over AREA},
!> \]
!>
!> \[
!>   DDIPR_s = DDBSED_s + {V_{inf}\over AREA},\qquad
!>   QSEDE_{s,f}^{out} = QSDWAE_{s,f}FDEL_s^{new}f_b .
!> \]
!>
!> `GINFD` and `GINFS` both receive the fine infiltration rate
!> \(V_{inf}/\Delta t\); for non-fines this rate is zero.
!>
!> @note Only faces in the outflow list are overwritten in `QSEDE`. Inflow and
!> no-flow faces are read as incoming sediment fluxes and are left unchanged.
!> @endnote
   PURE SUBROUTINE SYLINK(NFINE, NSED, NSEDEE, DTSY, AREAE, ARXLOE, &
      ARXLE, CLENTE, EPSBE, PBSEDE, VINFME, BARME, VCFMAE, CONCIE, &
      DCBSEE, DDBSEE, QSDWAE, QWAT, SOSDFE, FDELE, QSEDE, DCIPRE, &
      DDIPRE, GINFDE, GINFSE)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NFINE  !! Number of fine sediment classes.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      LOGICAL, INTENT(IN) :: BARME  !! True where fine sediment is protected by bed armouring.
      DOUBLE PRECISION, INTENT(IN) :: DTSY   !! Sediment substep duration.
      DOUBLE PRECISION, INTENT(IN) :: AREAE  !! Link bed/contact area.
      DOUBLE PRECISION, INTENT(IN) :: ARXLOE !! Previous channel cross-sectional area.
      DOUBLE PRECISION, INTENT(IN) :: ARXLE  !! Current channel cross-sectional area.
      DOUBLE PRECISION, INTENT(IN) :: CLENTE !! Channel-link length.
      DOUBLE PRECISION, INTENT(IN) :: EPSBE  !! Bank erosion sediment source.
      DOUBLE PRECISION, INTENT(IN) :: PBSEDE !! Channel-bed sediment porosity.
      DOUBLE PRECISION, INTENT(IN) :: CONCIE(NSED) !! Capacity concentration by sediment class.
      DOUBLE PRECISION, INTENT(IN) :: DCBSEE(NSED) !! Active-bed sediment depth by size class.
      DOUBLE PRECISION, INTENT(IN) :: DDBSEE(NSED) !! Lower-bed sediment depth by size class.
      DOUBLE PRECISION, INTENT(IN) :: QWAT(4)      !! Outward water flux by face.
      DOUBLE PRECISION, INTENT(IN) :: QSDWAE(NSEDEE, 4) !! Sediment advection coefficient by class and face.
      DOUBLE PRECISION, INTENT(IN) :: SOSDFE(NSED)      !! Source soil sediment-size fraction.
      DOUBLE PRECISION, INTENT(IN) :: VCFMAE !! Maximum fine volume available for settling/infiltration.
      DOUBLE PRECISION, INTENT(IN) :: VINFME !! Maximum fine infiltration volume.

      ! Input/output arguments
      ! Note: QSEDE must remain INOUT as it reads inflow faces and writes outflow faces
      DOUBLE PRECISION, INTENT(INOUT) :: FDELE(NSED) !! Mobile sediment concentration fraction by size class.
      DOUBLE PRECISION, INTENT(INOUT) :: QSEDE(NSEDEE, 4) !! Sediment flux by size class and face.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: DCIPRE(NSED) !! Interim upper-bed sediment depth by size class.
      DOUBLE PRECISION, INTENT(OUT) :: DDIPRE(NSED) !! Interim lower-bed sediment depth by size class.
      DOUBLE PRECISION, INTENT(OUT) :: GINFDE(NSED) !! Fine infiltration diagnostic/source for deposited material.
      DOUBLE PRECISION, INTENT(OUT) :: GINFSE(NSED) !! Fine infiltration diagnostic/source for suspended material.

      ! Locals, etc
      INTEGER :: FACE, J(4), JI, K(4), KI, NIN, NOUT, SED
      DOUBLE PRECISION :: AREAEI, DCBEEE, DCIPEE, DTSYI, FDC, FDELEE, GINF
      DOUBLE PRECISION :: OMPB, OMPBI, QSEDIN, SUM, SUMN, SUMP
      DOUBLE PRECISION :: VCFS, VCARM, VDMAX, VDSEDS, VDSED, VDWAT, VINF, VSTRAN

      !----------------------------------------------------------------------*

      ! Initialization
      ! --------------

      ! * Make lists of outflow and inflow faces
      NIN = 0
      NOUT = 0
      face_loop: DO FACE = 1, 4
         IF (QWAT(FACE) > 0.0D0) THEN
            NOUT = NOUT + 1
            J(NOUT) = FACE
         ELSE
            NIN = NIN + 1
            K(NIN) = FACE
         END IF
      END DO face_loop

      SUMP = 0.0D0
      SUMN = 0.0D0
      OMPB = 1.0D0 - PBSEDE
      OMPBI = 1.0D0 / OMPB
      DTSYI = 1.0D0 / DTSY
      AREAEI = 1.0D0 / AREAE

      ! Loop over size groups ( largest to smallest )
      ! ---------------------------------------------

      ! * Loop over sediment types ( largest to smallest )
      sed_loop: DO SED = NSED, 1, -1
         DCBEEE = DCBSEE(SED)

         ! Water and sediment budgets
         ! --------------------------

         ! * Calculate sediment inflow rate
         SUM = 0.0D0
         inflow_loop: DO KI = 1, NIN
            SUM = SUM + QSEDE(SED, K(KI))
         END DO inflow_loop
         QSEDIN = -SUM * OMPBI

         ! * Volume of water remaining + advective water discharge
         SUM = 0.0D0
         outflow_loop: DO JI = 1, NOUT
            SUM = SUM + QSDWAE(SED, J(JI))
         END DO outflow_loop
         VDWAT = ARXLE * CLENTE + SUM * DTSY

         ! * Sediment available for resuspension/transport/infiltration
         ! * /armouring
         VDMAX = FDELE(SED) * ARXLOE * CLENTE + DCBEEE * AREAE + &
            (QSEDIN + EPSBE * SOSDFE(SED)) * DTSY

         ! Infiltration and Armouring
         ! --------------------------

         ! * Sediment volumes subject to infiltration & armouring resp.
         IF (SED > NFINE) THEN
            ! * Non-fines
            VINF = 0.0D0
            VCARM = 0.0D0
         ELSE
            ! * Fines
            VCFS = MIN(VCFMAE, VDMAX)
            VINF = MIN(VINFME, VCFS)
            ! * ( SUMN/SUMP calculated below, summed over earlier passes )
            FDC = 0.0D0
            IF (BARME .AND. SUMN > 0.0D0) FDC = MIN(SUMN, SUMP) / SUMN
            VCARM = FDC * DIMJE(VCFS, VINF)
         END IF

         ! * Volume in and above top layer after infiltration ...
         VDSEDS = DIMJE(VDMAX, VINF)
         ! * ... minus armoured volume ( = SUPPLY limit for transport )
         VDSED = DIMJE(VDSEDS, VCARM)

         ! * Infiltration rates for each layer
         GINF = VINF * DTSYI
         GINFDE(SED) = GINF
         GINFSE(SED) = GINF

         ! Other output variables
         ! ----------------------

         ! * Sediment remaining in suspension + sediment discharged
         ! * - limited by either SUPPLY or CAPACITY
         VSTRAN = MIN(VDSED, CONCIE(SED) * OMPBI * VDWAT)

         ! * Concentration in suspension ('relative density')
         FDELEE = 0.0D0
         IF (VDWAT > 0.0D0) FDELEE = VSTRAN / VDWAT
         FDELE(SED) = FDELEE

         ! * Interim layer depths
         DCIPEE = DIMJE(VDSEDS, VSTRAN) * AREAEI
         DCIPRE(SED) = DCIPEE
         DDIPRE(SED) = DDBSEE(SED) + VINF * AREAEI

         ! * Particulate discharge rates at outflow faces
         discharge_loop: DO JI = 1, NOUT
            QSEDE(SED, J(JI)) = QSDWAE(SED, J(JI)) * FDELEE * OMPB
         END DO discharge_loop

         ! Epilogue
         ! --------

         ! * Depth of non-fines in interim and old top layers
         ! * ( used above on final pass: definition point must be later )
         ! * ( than reference point                                     )
         SUMP = SUMP + DCIPEE
         SUMN = SUMN + DCBEEE

      END DO sed_loop

   END SUBROUTINE SYLINK



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
   SUBROUTINE SYMAIN (NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD, ICMBK, ICMREF, ICMRF2, ICMXY, &
      NBFACE, NLYR, NTSOIL, NVC, AREA, CLENTH, CWIDTH, DHF, DXQQ, DYQQ, THSAT,    &
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
      INTEGER, INTENT(IN) :: ICMBK (NLFEE, 2)   !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMRF2 (NLFEE, 3, 2) !! Confluence branch reference map.
      INTEGER, INTENT(IN) :: ICMXY (NXEE, NY)   !! Element number at each grid location.
      INTEGER, INTENT(IN) :: NBFACE (NEL)       !! Number of boundary faces by element.
      INTEGER, INTENT(IN) :: NTSOIL (NEL, NLYREE) !! Soil type index for each element layer.
      INTEGER, INTENT(IN) :: ISORT (NEL) !! Donor-before-receptor element routing order.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND (NEL) !! Ground or bed elevation by element.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: TIH   !! Initial simulation time in hours (unused in this routine).
      DOUBLE PRECISION, INTENT(IN) :: UZNOW !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(INOUT) :: ARXL (NLFEE) !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(INOUT) :: CLAI (NV)    !! Current canopy leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: DRAINA (NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
      DOUBLE PRECISION, INTENT(INOUT) :: HRF (NEL)    !! Water level/head by element.
      DOUBLE PRECISION, INTENT(INOUT) :: PLAI (NV)    !! Potential/maximum leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: PNETTO (NLF + 1:NEL) !! Net precipitation/effective rainfall by land element.
      DOUBLE PRECISION, INTENT(IN) :: QOC (NELEE, 4) !! Face water fluxes.
      LOGICAL, INTENT(IN) :: BEXBK        !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS (NLFEE) !! True for north-south channel links.

      ! Checked by SYERR1 via ALCHK/ALCHKI interfaces
      INTEGER, INTENT(INOUT) :: ICMREF (NELEE, 4, 2:3) !! Face-neighbour and reverse-face reference map.
      INTEGER, INTENT(INOUT) :: NLYR (NLF + 1:NEL) !! Number of soil layers in each land element.
      INTEGER, INTENT(INOUT) :: NVC (NLF + 1:NEL)  !! Vegetation type by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: AREA (NEL)     !! Element plan area.
      DOUBLE PRECISION, INTENT(INOUT) :: CLENTH (NLFEE) !! Channel-link length.
      DOUBLE PRECISION, INTENT(INOUT) :: CWIDTH (NLFEE) !! Channel width by link.
      DOUBLE PRECISION, INTENT(INOUT) :: DHF (NELEE, 4) !! Face-to-face hydraulic distance.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ (NLF + 1:NEL) !! Land-element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ (NLF + 1:NEL) !! Land-element length.
      DOUBLE PRECISION, INTENT(INOUT) :: THSAT (NS)     !! Saturated water content by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: ZBFULL (NLFEE) !! Bankfull elevation/depth by link.

      ! Input/output arguments
      INTEGER, INTENT(INOUT) :: NSED !! Number of sediment size classes.
      DOUBLE PRECISION, INTENT(INOUT) :: PBSED (NLFEE)   !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(INOUT) :: PLS (NLF + 1:NEL) !! Loose-sediment porosity by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: SOSDFN (NSEE, NSEDEE) !! Soil sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: ARBDEP (NLFEE) !! Accumulated channel-bed elevation/depth change.
      DOUBLE PRECISION, INTENT(INOUT) :: DLS (NEL)      !! Loose/bed sediment depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: DCBED (NLFEE)  !! Active upper-bed layer depth by link.
      DOUBLE PRECISION, INTENT(INOUT) :: DCBSED (NLFEE, NSEDEE) !! Upper-bed sediment depth by link and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA (NELEE, NSEDEE)  !! Sediment composition fraction by element and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FDEL (NELEE, NSEDEE)   !! Mobile sediment concentration fraction by element and size class.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: GINFD (NLFEE, NSEDEE) !! Fine infiltration diagnostic/source for deposited material.
      DOUBLE PRECISION, INTENT(OUT) :: GINFS (NLFEE, NSEDEE) !! Fine infiltration diagnostic/source for suspended material.
      DOUBLE PRECISION, INTENT(OUT) :: GNU (NLF + 1:NEL) !! Hillslope erosion rate by land element.
      DOUBLE PRECISION, INTENT(OUT) :: GNUBK (NLFEE)     !! Lateral bank erosion rate by link.
      DOUBLE PRECISION, INTENT(OUT) :: QSED (NELEE, NSEDEE, 4) !! Sediment flux by element, size class, and face.

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT)      :: IDUM  !! Integer workspace for checks and reads.
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY !! Floating-point workspace for checks and reads.

      ! Locals, etc
      CHARACTER (LEN=*), PARAMETER :: SYVER = '4.2.7'

      INTEGER :: FACE, FADJ, I, IADJ, IB, IBR, IEL, N, P, SED, SOIL

      DOUBLE PRECISION :: DTSY
      DOUBLE PRECISION :: CONCIE (NSEDEE)
      DOUBLE PRECISION :: DCBSEE (NSEDEE), DCIPRE (NSEDEE)
      DOUBLE PRECISION :: DDBSEE (NSEDEE)
      DOUBLE PRECISION :: DDIPRE (NSEDEE)
      DOUBLE PRECISION :: FBETAE (NSEDEE), FCC (NVEE), FDELE (NSEDEE)
      DOUBLE PRECISION :: GINFDE (NSEDEE), GINFSE (NSEDEE)
      DOUBLE PRECISION :: QSDWAE (NSEDEE, 4), QSEDE (NSEDEE, 4)
      DOUBLE PRECISION :: QWAT (4)
      DOUBLE PRECISION :: SLOPEE (4), SOSDFE (NSEDEE)
      DOUBLE PRECISION :: TAUJE (4)

      LOGICAL :: DOUBT

      !----------------------------------------------------------------------*

      PASS_symain = PASS_symain + 1
      IF (PASS_symain == 1) THEN

         ! --------------------- Initialization step ----------------------------*

         CALL INITIALISE_SYMAIN_WORKSPACE()

         ! * Check array bounds & input variables
         CALL SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE, NV, NVEE, NX, NXEE, NY, &
            SPR, SYD)

         ! * Check static/initializing input arrays
         CALL SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, NXEE, NYEE, NY, SPR, BEXBK,   &
            LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, NTSOIL, NVC, THSAT, CLENTH,   &
            CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, ARXL, HRF (NLF + 1), ZGRUND, IDUM, &
            IDUM1X, LDUM)

         ! * Store top-layer soil type for each column element
         DO IEL = NLF + 1, NEL
            NTSOTP_symain (IEL) = NTSOIL (IEL, NLYR (IEL))
         END DO

         ! * Read SY input data file
         CALL SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, NELEE, NLF, NLFEE, NS, NSEDEE,  &
            NSEE, NSYBEE, NSYCEE, NTSOTP_symain (NLF + 1), NV, NX, NXEE, NYEE, NY,    &
            SPR, SYD, SYVER, ABC_symain, ALPHA_symain, BBC_symain, BKB_symain,        &
            CONCOB_symain, DCBEDO_symain, DLS, DRDRIP_symain, DRSED_symain,           &
            DLSMAX_symain, FBETA, FBIC_symain, FCG_symain (NLF + 1),                  &
            FCROCK_symain (NLF + 1), FDEL, FDRIP_symain, FICRIT_symain, FPCLAY_symain,&
            FPCRIT_symain, GBC_symain, GKF_symain, GKR_symain, ISACKW_symain,         &
            ISGSED_symain, ISSYOK_symain, ISTEC_symain, ISUSED_symain, NEPS_symain,   &
            NFINE_symain, NSED, NSYB_symain, NSYBCD_symain, NSYC_symain,              &
            NTSOBK_symain, PBSED, PLS, RHOSO_symain, SOSDFN, XDRIP_symain, IDUM,      &
            DUMMY, DUMSED)

         ! * Check SY input data
         CALL SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV,           &
            NSYB_symain, NSYBEE, NSYC_symain, NSYCEE, SPR, ICMREF, ISUSED_symain,     &
            NEPS_symain, NFINE_symain, SFB, SRB, ALPHA_symain, DCBEDO_symain,         &
            FPCRIT_symain, DLSMAX_symain, NTSOBK_symain, NSYBCD_symain, NBFACE,       &
            DRSED_symain, BKB_symain, GKF_symain, GKR_symain, RHOSO_symain, SOSDFN,   &
            DRDRIP_symain, FDRIP_symain, XDRIP_symain, PBSED, FCG_symain (NLF + 1),   &
            FCROCK_symain (NLF + 1), PLS, DLS, FBETA, FDEL, ABC_symain, BBC_symain,   &
            GBC_symain, IDUM, DUMMY, LDUM)

         ! * Static variables and initialization
         CALL SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, NTSOBK_symain, ARXL,      &
            DCBEDO_symain, DLS, FBETA, DRSED_symain, HRF (NLF + 1), PBSED, PLS,       &
            SOSDFN, THSAT, ZGRUND, NTSOTP_symain (NLF + 1), ZBFULL, ARBDEP,           &
            ARXLOL_symain, DCBED, DCBSED, DDBSED_symain, DRSO50_symain,               &
            DWATOL_symain (NLF + 1), FETA_symain, GINFD, GINFS, GNU, GNUBK, QSED,     &
            DBFULL_symain)

         !------------------- End of initialization step -----------------------*

      ELSE
         !---------------------- Simulation step -------------------------------*

         ! Check Input
         ! -----------
         ! * Check time-varying input variables
         DOUBT = ISSYOK_symain > 0
         IF (DOUBT) DOUBT = MOD (PASS_symain - 2, ISSYOK_symain) == 0

         IF (DOUBT) THEN
            CALL SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, ICMRF2, ISORT, DTUZ, CLAI,    &
               PLAI, ARXL, DRAINA, PNETTO, HRF, ZGRUND, QOC, IDUM, IDUM1A, IDUM1X,    &
               LDUM)
         END IF

         ! Quantities Independent of Sub-timestep
         ! --------------------------------------
         ! * Water-flow related variables
         CALL SYWAT (NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, DHF, DRDRIP_symain,       &
            LINKNS, ZBFULL, ZGRUND, CLAI, DRAINA, HRF, PLAI, PNETTO, QOC,              &
            DRDROP (NLF + 1), DWAT1, FCC, FQCONF, LRAIN (NLF + 1), SLOPEJ, TAUJ, TAUK)

         ! * Erosion rates for all column elements
         CALL SYOVER (ISTEC_symain, NEL, NLF, NS, NV, FCC, LRAIN (NLF + 1), XDRIP_symain,       &
            DRDRIP_symain, FDRIP_symain, DRAINA, GKR_symain, DWAT1 (NLF + 1),         &
            DRDROP (NLF + 1), FCG_symain (NLF + 1), FCROCK_symain (NLF + 1),          &
            DRSO50_symain, TAUK (NLF + 1), FPCLAY_symain, GKF_symain, RHOSO_symain,   &
            NTSOTP_symain (NLF + 1), NVC, GNU, DUMMY, DLS, DLSMAX_symain)

         ! * Erosion rates for all link elements
         IF (NLF > 0) THEN
            CALL SYBKER (ISTEC_symain, NLF, NS, FPCLAY_symain, RHOSO_symain, DRSO50_symain,     &
               TAUK, CWIDTH, DWAT1, BKB_symain, NTSOBK_symain, FETA_symain, CLENTH,   &
               DBFULL_symain, EPSB, GNUBK)
         END IF


         ! SY Sub-timestep Loop
         ! --------------------
         DTSY = DTUZ / NEPS_symain
         DO N = 1, NEPS_symain

            ! Initialization
            ! --------------
            ! Replaced ALINIT with array slices
            DO FACE = 1, 4
               DO SED = 1, NSED
                  QSED (1:NEL, SED, FACE) = ZERO
               END DO
            END DO

            ! Boundary Conditions
            ! -------------------

            IF (NSYB_symain > 0) THEN

               ! * Gather water "outflow" rates (should be negative)
               DO IB = 1, NSYB_symain
                  IEL = NSYBCD_symain (IB, 1)
                  FACE = NBFACE (IEL)
                  QWATB (IB) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
               END DO

               ! * Read time-varying flux data & calculate sediment flows
               CALL SYBC

               ! * Load boundary flows into QSED array
               DO IB = 1, NSYB_symain
                  IEL = NSYBCD_symain (IB, 1)
                  FACE = NBFACE (IEL)
                  CALL DCOPY (NSED, QSEDB (1, IB), 1, QSED (IEL, 1, FACE), NELEE)
               END DO

            END IF

            ! Quantities Independent of Sediment Flux
            ! ---------------------------------------
            IF (NLF > 0) THEN
               ! * Transport capacity & advection coefficients
               CALL SYCLTR (CONCOB_symain, FPCRIT_symain, ISACKW_symain, ISUSED_symain, NELEE,  &
                  NFINE_symain, NLF, NLFEE, NSED, NSEDEE,                             &
                  DRSED_symain (NFINE_symain + 1), ARXL, CWIDTH, DCBED, LINKNS, DWAT1,&
                  QOC, SLOPEJ, DCBSED (1, NFINE_symain + 1),                          &
                  FDEL (1, NFINE_symain + 1), TAUJ, ACKW_symain (1, NFINE_symain + 1),&
                  CONCI, QSDWAT, DUMMY, DUMSED)

               ! * Settling, infiltration & armouring
               IF (NFINE_symain > 0) THEN
                  CALL SYFINE (DRSED_symain (1), FBIC_symain, FICRIT_symain, NLF, ALPHA_symain, &
                     DTSY, AREA, DCBSED, FBETA, FDEL, PBSED, TAUK, VCFMAX, VINFMX,    &
                     BARM)
               END IF
            END IF

            ! One Element at a Time
            ! ---------------------
            DO I = 1, NEL
               IEL = ISORT (I)

               ! * Gather common sub-arrays
               CALL DCOPY (NSED, FDEL (IEL, 1), NELEE, FDELE, 1)
               DO FACE = 1, 4
                  QWAT (FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
                  QSEDE (1:NSED, FACE) = QSED (IEL, 1:NSED, FACE)
               END DO

               IF (IEL <= NLF) THEN
                  ! ** Link element **
                  ! * Gather link-specific sub-arrays
                  SOIL = NTSOBK_symain (IEL)
                  CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)
                  CALL DCOPY (NSED, CONCI (IEL, 1), NLFEE, CONCIE, 1)
                  CALL DCOPY (NSED, DCBSED (IEL, 1), NLFEE, DCBSEE, 1)
                  CALL DCOPY (NSED, DDBSED_symain (IEL, 1), NLFEE, DDBSEE, 1)
                  DO FACE = 1, 4
                     CALL DCOPY (NSED, QSDWAT (IEL, 1, FACE), NLFEE, QSDWAE (1, FACE), 1)
                  END DO

                  ! * Solve transport equation
                  CALL SYLINK (NFINE_symain, NSED, NSEDEE, DTSY, AREA (IEL),                    &
                     ARXLOL_symain (IEL), ARXL (IEL), CLENTH (IEL), EPSB (IEL),       &
                     PBSED (IEL), VINFMX (IEL), BARM (IEL), VCFMAX (IEL), CONCIE,     &
                     DCBSEE, DDBSEE, QSDWAE, QWAT, SOSDFE, FDELE, QSEDE, DCIPRE,      &
                     DDIPRE, GINFDE, GINFSE)

                  ! * Scatter link-specific results
                  CALL DCOPY (NSED, DCIPRE, 1, DCIPRM (IEL, 1), NLFEE)
                  CALL DCOPY (NSED, DDIPRE, 1, DDIPRM (IEL, 1), NLFEE)
                  CALL DCOPY (NSED, GINFDE, 1, GINFD (IEL, 1), NLFEE)
                  CALL DCOPY (NSED, GINFSE, 1, GINFS (IEL, 1), NLFEE)

               ELSE
                  ! ** Column element **
                  ! * Gather column-specific sub-arrays
                  SOIL = NTSOTP_symain (IEL)
                  CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)
                  CALL DCOPY (NSED, FBETA (IEL, 1), NELEE, FBETAE, 1)
                  CALL DCOPY (4, SLOPEJ (IEL, 1), NELEE, SLOPEE, 1)
                  CALL DCOPY (4, TAUJ (IEL, 1), NELEE, TAUJE, 1)

                  ! * Solve transport equation for this column element
                  CALL SYCOLM (AREA (IEL), DTSY, DWAT1 (IEL), DWATOL_symain (IEL), DXQQ (IEL),  &
                     DYQQ (IEL), FETA_symain (IEL), GNU (IEL), ISGSED_symain, NSED,   &
                     FPCRIT_symain, PLS (IEL), NSEDEE, DRSED_symain, QWAT, SLOPEE,    &
                     SOSDFE, TAUJE, DLS (IEL), FBETAE, FDELE, QSEDE, DUMMY, DUMSED)

                  ! * Scatter column-specific results
                  CALL DCOPY (NSED, FBETAE, 1, FBETA (IEL, 1), NELEE)
               END IF

               ! * Scatter common results ...
               CALL DCOPY (NSED, FDELE, 1, FDEL (IEL, 1), NELEE)
               DO FACE = 1, 4
                  CALL DCOPY (NSED, QSEDE (1, FACE), 1, QSED (IEL, 1, FACE), NELEE)

                  ! ... and propagate sediment flow rates at outflow faces
                  IF (QWAT (FACE) > ZERO) THEN
                     IADJ = ICMREF (IEL, FACE, 2)

                     IF (IADJ > 0) THEN
                        ! * regular neighbour
                        FADJ = ICMREF (IEL, FACE, 3)
                        DO SED = 1, NSED
                           QSED (IADJ, SED, FADJ) = -QSEDE (SED, FACE)
                        END DO

                     ELSE IF (IADJ < 0) THEN
                        ! * neighbour is a confluence node
                        IBR = -IADJ
                        DO P = 1, 3
                           IADJ = ICMRF2 (IBR, P, 1)
                           IF (IADJ > 0) THEN
                              ! * prospect is active
                              FADJ = ICMRF2 (IBR, P, 2)
                              DO SED = 1, NSED
                                 QSED (IADJ, SED, FADJ) = QSED (IADJ, SED, FADJ) - &
                                    QSEDE (SED, FACE) * FQCONF (IBR, P)
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
               CALL SYBED (DCBEDO_symain, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, DDIPRM,      &
                  ARBDEP, DLS, FBETA, DCBSED, DDBSED_symain, DCBED)
            END IF

            ! Store Old-time Values & Update Timer
            ! ------------------------------------
            CALL DCOPY (NEL - NLF, DWAT1 (NLF + 1), 1, DWATOL_symain (NLF + 1), 1)
            IF (NLF > 0) CALL DCOPY (NLF, ARXL, 1, ARXLOL_symain, 1)

            SYNOW_symain = SYNOW_symain + DTSY / 3600.0D0

         END DO

         !--------------------- End of simulation step -------------------------*
      END IF

      ! Epilogue
      ! --------
      ! Ensure that current time value is exactly correct
      SYNOW_symain = UZNOW

   END SUBROUTINE SYMAIN



!> Calculates ground-surface (hillslope) erosion for each column element.
!>
!> `SYOVER` implements the manual's rainsplash-plus-overland-flow detachment
!> model. Detachment is suppressed once loose sediment reaches `DLSMAX`; this
!> is the `BTL 25.04.95` extension noted in the routine's history.
!>
!> For each vegetation type, a coefficient pair \((C,c)\) is selected by
!> canopy fall height `XDRIP` and drip diameter `DRDRIP`, giving the
!> per-drip momentum factor
!>
!> \[
!>   TGMD =
!>   {\pi\rho_w^2g\over 6}\,c\left(1-e^{-2\,XDRIP/c}\right)DRDRIP^3\,FDRIP .
!> \]
!>
!> For each land element, a rainfall-intensity class selects coefficients
!> \((a,b)\) for the direct-rainfall momentum term, and the drip-momentum term
!> reuses `TGMD` scaled by the canopy-drip rainfall `DRAINA`:
!>
!> \[
!>   GMR = (1-FCC)\,a\,LRAIN^b,\qquad GMD = TGMD\times DRAINA .
!> \]
!>
!> Rainsplash detachment attenuates with ponding depth relative to the median
!> drop diameter `DRDROP`:
!>
!> \[
!>   DR = GKR\,e^{-\max(0,\,DWAT1/DRDROP - 1)}\,(1-FCG-FCROCK)\,(GMR+GMD).
!> \]
!>
!> Overland-flow detachment uses the critical shear stress from [[sycrit]]:
!>
!> \[
!>   DF = GKF\,(1-FCROCK)\,{\max(0,\,TAUK-\tau_c)\over\tau_c}.
!> \]
!>
!> The erosion rate is then
!>
!> \[
!>   GNU =
!>   \begin{cases}
!>     (DR+DF)/RHOSO, & DLS < DLSMAX,\\
!>     0, & DLS \ge DLSMAX.
!>   \end{cases}
!> \]
!>
!> @note The rainfall-intensity and drip-size class selections (`ISGMR`,
!> `ISCD`) still use the original branchless switch function
!> `SF2(x,y)=0.5+\mathrm{sign}(0.5,x-y)`, summed over class boundaries, rather
!> than an `IF`/`ELSE IF` chain: an intermediate modernisation replaced it
!> with branches and was reverted for performance, restoring this original
!> form (unchanged since the 1994 Fortran 77 version).
!> @endnote
   SUBROUTINE SYOVER (ISTEC, NEL, NLF, NS, NV, FCC, LRAIN, XDRIP, &
      DRDRIP, FDRIP, DRAINA, GKR, DWAT1, DRDROP, FCG, FCROCK, DRSO50, &
      TAUK, FPCLAY, GKF, RHOSO, NTSOTP, NVC, GNU, TGMD, DLS, DLSMAX)

      IMPLICIT NONE

      ! Input/Output arguments
      INTEGER, INTENT(IN) :: ISTEC !! Critical-shear calculation option.
      INTEGER, INTENT(IN) :: NEL   !! Number of elements.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NS    !! Number of soil types.
      INTEGER, INTENT(IN) :: NV    !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NTSOTP (NLF + 1:NEL) !! Top soil type by land element.
      INTEGER, INTENT(IN) :: NVC (NLF + 1:NEL)    !! Vegetation type by land element.
      DOUBLE PRECISION, INTENT(IN) :: FCC (NV)   !! Canopy/ground sheltering fraction by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: LRAIN (NLF + 1:NEL) !! Effective direct rainfall rate by land element.
      DOUBLE PRECISION, INTENT(IN) :: XDRIP (NV) !! Canopy drip fall height by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: DRDRIP (NV) !! Canopy drip drop diameter by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: FDRIP (NV)  !! Canopy drip fraction by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: DRAINA (NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
      DOUBLE PRECISION, INTENT(IN) :: GKR (NS)   !! Rainfall detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1 (NLF + 1:NEL)  !! Surface water depth by land element.
      DOUBLE PRECISION, INTENT(IN) :: DRDROP (NLF + 1:NEL) !! Effective raindrop/drop diameter by land element.
      DOUBLE PRECISION, INTENT(IN) :: FCG (NLF + 1:NEL)    !! Ground-cover fraction by land element.
      DOUBLE PRECISION, INTENT(IN) :: FCROCK (NLF + 1:NEL) !! Rock-cover fraction by land element.
      DOUBLE PRECISION, INTENT(IN) :: DRSO50 (NS) !! Median soil particle diameter by soil type.
      DOUBLE PRECISION, INTENT(IN) :: TAUK (NLF + 1:NEL) !! Overland-flow shear stress by land element.
      DOUBLE PRECISION, INTENT(IN) :: FPCLAY (NS) !! Clay fraction by soil type.
      DOUBLE PRECISION, INTENT(IN) :: GKF (NS)    !! Flow detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(IN) :: RHOSO (NS)  !! Soil bulk density by soil type.
      DOUBLE PRECISION, INTENT(IN) :: DLS (NEL)   !! Loose-sediment depth by element.
      DOUBLE PRECISION, INTENT(IN) :: DLSMAX      !! Loose-sediment depth above which soil erosion is suppressed.
      DOUBLE PRECISION, INTENT(OUT) :: GNU (NLF + 1:NEL) !! Hillslope erosion rate by land element.
      DOUBLE PRECISION, INTENT(OUT) :: TGMD (NV)  !! Workspace for canopy-drip momentum by vegetation type.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: X1 = 7.5D0, D1 = 3.3D-3, L1 = 2.78D-6, L2 = 1.39D-5
      DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979323846D0
      DOUBLE PRECISION, PARAMETER :: CLALIM = 1.0D0 / L2

      INTEGER :: ISCD, IEL, ISGMR, ISOIL, NVEG
      DOUBLE PRECISION :: CD, FCROCE, DRDRPE, DR, DF
      DOUBLE PRECISION :: LRAINE, GMD, GMR, PRSGOS, TAUEC, TAUKE, XDRIPE

      DOUBLE PRECISION, PARAMETER :: AD(4)  = [3214.9D0, 583.4D0, 133.1D0, 29.9D0]
      DOUBLE PRECISION, PARAMETER :: BD(4)  = [1.6896D0, 1.5545D0, 1.4242D0, 1.2821D0]
      DOUBLE PRECISION, PARAMETER :: ADD(4) = [0.0D0, 0.0D0, 1.93D0, 5.14D0]
      DOUBLE PRECISION, PARAMETER :: BDD(4) = [2200.0D0, 2200.0D0, 1640.0D0, 660.0D0]

      ! Legacy branchless statement function
      DOUBLE PRECISION :: SF2, SX, SY
      SF2(SX, SY) = HALF + SIGN(HALF, SX - SY)

      !----------------------------------------------------------------------*

      PRSGOS = PI * RHOWAT * RHOWAT * GRAVTY / 6.0D0

      DO NVEG = 1, NV
         XDRIPE = XDRIP(NVEG)
         DRDRPE = DRDRIP(NVEG)

         ! Performance Reversion: Branchless execution
         ISCD = 1 + NINT(SF2(XDRIPE, X1) + 2.0D0 * SF2(DRDRPE, D1))

         CD = ADD(ISCD) + DRDRPE * BDD(ISCD)
         TGMD(NVEG) = PRSGOS * CD * (ONE - EXP(-2.0D0 * XDRIPE / CD)) * (DRDRPE**3) * FDRIP(NVEG)
      END DO

      DO IEL = NLF + 1, NEL
         ISOIL = NTSOTP(IEL)
         NVEG = NVC(IEL)
         LRAINE = LRAIN(IEL)
         FCROCE = FCROCK(IEL)
         TAUKE = TAUK(IEL)

         ! Performance Reversion: Branchless execution
         ISGMR = MIN(4, 1 + NINT(SF2(LRAINE, L1)) + INT(LRAINE * CLALIM))

         GMR = (ONE - FCC(NVEG)) * AD(ISGMR) * (LRAINE**BD(ISGMR))
         GMD = TGMD(NVEG) * DRAINA(IEL)

         DR = GKR(ISOIL) * EXP(-MAX(ZERO, (DWAT1(IEL) / DRDROP(IEL)) - ONE)) * &
            (ONE - FCG(IEL) - FCROCE) * (GMR + GMD)

         CALL SYCRIT (ISTEC, DRSO50(ISOIL), TAUKE, FPCLAY(ISOIL), TAUEC)

         DF = GKF(ISOIL) * (ONE - FCROCE) * MAX(ZERO, TAUKE - TAUEC) / TAUEC

         IF (DLS(IEL) < DLSMAX) THEN
            GNU(IEL) = (DR + DF) / RHOSO(ISOIL)
         ELSE
            GNU(IEL) = ZERO
         END IF
      END DO

   END SUBROUTINE SYOVER



!> Calculates overland-flow sediment transport capacity for one element.
!>
!> `SYOVTR` implements the manual's overland transport-capacity switch
!> `ISGSED` for the current land element. The manual notes that both available
!> formulae were derived for non-cohesive channel transport and their use for
!> rainfall-driven overland flow is uncertain; [[sycolm]] applies the additional
!> total concentration cap `FPCRIT` after this routine returns.
!>
!> The representative particle diameter \(d_{50}\) is the median diameter of
!> the currently available sediment mix, obtained from [[sydr]] with `VDSED`
!> and `DRSED`.
!>
!> Capacity is summed only over faces with positive `QWAT`. The face length
!> follows the OC face convention used by the code:
!>
!> | Faces | Length |
!> |:------|:-------|
!> | 1 and 3 | `DYQQE` |
!> | 2 and 4 | `DXQQE` |
!>
!> For `ISGSED = 1`, the Engelund-Hansen-style branch uses water depth \(h\),
!> face discharge \(Q_f\), and slope \(S_f\):
!>
!> \[
!>   G_f =
!>   {0.05\rho_w^2\,Q_f^2S_f^{3/2}\over
!>    (\rho_s-\rho_w)^2\sqrt{g}\,L_f\sqrt{h}\,d_{50}} .
!> \]
!>
!> For `ISGSED = 0`, the Yalin-style branch first obtains a critical shear
!> stress \(\tau_c\) from [[sycrit]] using the Shields option and computes
!>
!> \[
!>   T = {\max(\tau_f-\tau_c,0)\over\tau_c},\qquad
!>   A = {2.45(\rho_s/\rho_w)^{-0.4}\over
!>        \sqrt{(\rho_s-\rho_w)g}}\sqrt{\tau_c/d_{50}},
!> \]
!>
!> \[
!>   G_f =
!>   {0.635\over\sqrt{\rho_w}}\sqrt{\tau_f}\,d_{50}L_f
!>   \left(T-{\log(1+AT)\over A}\right).
!> \]
!>
!> Any other `ISGSED` value gives zero capacity. The returned `GJSUM` is
!> \(\sum_f G_f\) over outflowing faces.
!>
!> @note The Engelund-Hansen branch is skipped when `DWAT1E <= 0`, returning
!> zero capacity. Both active formula branches use powers or square roots of
!> slope/shear directly, so upstream hydraulic calculations are expected to
!> provide non-negative active-face values.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-07 | SvB | 4.6.1 | Replaced the runtime "first call" caching of `K1_syovtr`/`K3_syovtr`/`K4_syovtr` with compile-time `PARAMETER`s (declared at module scope, since they no longer need `SAVE`d state). |
!> | 2026-04-12 | SvB | 4.6.1 | Precalculated the four face lengths into `FLJ_ARRAY` instead of a per-face `MOD` test. |
!> @endhistory
   PURE SUBROUTINE SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, &
      DRSED, QWAT, SLOPEE, TAUJE, GJSUM)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: ISGSED !! Overland transport-capacity option.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      DOUBLE PRECISION, INTENT(IN) :: DXQQE  !! Element width.
      DOUBLE PRECISION, INTENT(IN) :: DYQQE  !! Element length.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1E !! Current surface water depth.
      DOUBLE PRECISION, INTENT(IN) :: VDSED (NSED) !! Available sediment volume by size class.
      DOUBLE PRECISION, INTENT(IN) :: DRSED (NSED) !! Representative particle diameters by size class.
      DOUBLE PRECISION, INTENT(IN) :: QWAT (4)   !! Outward water flux by face.
      DOUBLE PRECISION, INTENT(IN) :: SLOPEE (4) !! Water-surface slope by face.
      DOUBLE PRECISION, INTENT(IN) :: TAUJE (4)  !! Face shear stress.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: GJSUM !! Total overland sediment transport capacity for the element.

      ! Locals
      DOUBLE PRECISION :: K2, AJ, DRD50, FTAU, DUM, GJ, GSUM
      DOUBLE PRECISION :: LJ, TAUEC, TAUJEE
      INTEGER :: FACE, NOUT, I, J(4)
      DOUBLE PRECISION :: FLJ_ARRAY(4)

      !----------------------------------------------------------------------*
      ! Preliminaries
      ! -------------
      !

      ! Initialize variables
      GSUM = ZERO

      ! High-Performance Fix: Pre-calculate face lengths into an array instead of using MOD()
      FLJ_ARRAY = [DYQQE, DXQQE, DYQQE, DXQQE]

      ! Obtain median diameter of sediment available for discharge
      DRD50 = SYDR(HALF, 1, NSED, VDSED, DRSED)

      ! Count and record faces with outflow
      NOUT = 0
      DO FACE = 1, 4
         IF (QWAT(FACE) > ZERO) THEN
            NOUT = NOUT + 1
            J(NOUT) = FACE
         END IF
      END DO

      !
      ! Transport Capacity
      ! ------------------
      !
      IF (ISGSED == 1 .AND. DWAT1E > ZERO) THEN

         ! ^^^ ENGELUND-HANSEN METHOD ^^^

         ! Precalculate constant over faces (note K2 may be very small)
         K2 = SQRT(DWAT1E) * DRD50

         ! Loop over faces with outflow
         DO I = 1, NOUT
            FACE = J(I)

            ! Discharge capacity at this face
            LJ = FLJ_ARRAY(FACE)
            GJ = (K1_syovtr * QWAT(FACE)**2 * SLOPEE(FACE)**1.5D0) / (LJ * K2)

            ! Accumulated discharge capacity for this element
            GSUM = GSUM + GJ
         END DO

      ELSE IF (ISGSED == 0) THEN

         ! ^^^^^^^ YALIN METHOD ^^^^^^^^^

         ! Loop over faces with outflow
         DO I = 1, NOUT
            FACE = J(I)

            ! Get face length
            LJ = FLJ_ARRAY(FACE)

            ! Obtain critical shear stress at the ground surface
            TAUJEE = TAUJE(FACE)
            CALL SYCRIT(0, DRD50, TAUJEE, DUM, TAUEC)

            ! Calculate discharge capacity at this face
            ! High-Performance Fixes: MAX replaces DIMJE, LOG1P replaces LOG(1+X) for precision
            FTAU = MAX(ZERO, TAUJEE - TAUEC) / TAUEC
            AJ = K3_syovtr * SQRT(TAUEC / DRD50)
            GJ = K4_syovtr * SQRT(TAUJEE) * DRD50 * LJ * (FTAU - LOG(1.0D0 + AJ * FTAU) / AJ)

            ! Accumulated capacity for this element
            GSUM = GSUM + GJ
         END DO

      ELSE
         ! ^^^ Zero capacity ^^^
      END IF

      GJSUM = GSUM

   END SUBROUTINE SYOVTR



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
   SUBROUTINE SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, &
      NSYBEE, NSYCEE, NTSOTP, NV, NX, NXEE, NYEE, NY, SPR, SYD, SYVER, ABC, ALPHA,   &
      BBC, BKB, CONCOB, DCBEDO, DLS, DRDRIP, DRSED, DLSMAX, FBETA, FBIC, FCG,        &
      FCROCK, FDEL, FDRIP, FICRIT, FPCLAY, FPCRIT, GBC, GKF, GKR, ISACKW, ISGSED,    &
      ISSYOK, ISTEC, ISUSED, NEPS, NFINE, NSED, NSYB, NSYBCD, NSYC, NTSOBK, PBSED,   &
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
      INTEGER, INTENT(IN) :: NTSOTP (NLF + 1:NEL) !! Top soil type by land element.
      INTEGER, INTENT(IN) :: NV   !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NX   !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NYEE !! Grid-row workspace dimension.
      INTEGER, INTENT(IN) :: NY   !! Number of grid rows.
      INTEGER, INTENT(IN) :: SYD  !! Static sediment input unit.
      INTEGER, INTENT(IN) :: SPR  !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: ICMBK (NLFEE, 2)   !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF (NELEE, 4, 2:2) !! Face-neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY (NXEE, NY)   !! Element number at each grid location.
      LOGICAL, INTENT(IN) :: BEXBK        !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS (NLFEE) !! True for north-south channel links.
      CHARACTER (LEN=*), INTENT(IN) :: SYVER !! Expected sediment input-file version string.

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
      INTEGER, INTENT(OUT) :: NSYBCD (NSYBEE, 3) !! Sediment boundary element, type, and category metadata.
      INTEGER, INTENT(OUT) :: NSYC (4) !! Number of sediment boundary categories by boundary type.
      INTEGER, INTENT(OUT) :: NTSOBK (NLFEE) !! Bank soil type by link.
      DOUBLE PRECISION, INTENT(OUT) :: ABC (NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `A`.
      DOUBLE PRECISION, INTENT(OUT) :: ALPHA !! Fine-sediment settling/resuspension critical-shear ratio.
      DOUBLE PRECISION, INTENT(OUT) :: BBC (NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `B`.
      DOUBLE PRECISION, INTENT(OUT) :: BKB (NS)   !! Bank erodibility by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: CONCOB     !! Mobile concentration threshold for overbank exchange.
      DOUBLE PRECISION, INTENT(OUT) :: DCBEDO     !! Active upper channel-bed layer thickness.
      DOUBLE PRECISION, INTENT(OUT) :: DRDRIP (NV) !! Canopy drip drop diameter by vegetation type.
      DOUBLE PRECISION, INTENT(OUT) :: DRSED (NSEDEE) !! Representative sediment particle diameters.
      DOUBLE PRECISION, INTENT(OUT) :: FBIC   !! Fine-bed fraction threshold for infiltration.
      DOUBLE PRECISION, INTENT(OUT) :: FDRIP (NV) !! Canopy drip fraction by vegetation type.
      DOUBLE PRECISION, INTENT(OUT) :: FICRIT !! Fine-concentration threshold for infiltration.
      DOUBLE PRECISION, INTENT(OUT) :: FPCLAY (NS) !! Clay fraction by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: FPCRIT !! Maximum sediment concentration fraction.
      DOUBLE PRECISION, INTENT(OUT) :: GBC (NSEDEE, NSYCEE) !! Steady boundary sediment input by class/category.
      DOUBLE PRECISION, INTENT(OUT) :: GKF (NS)   !! Flow detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: GKR (NS)   !! Rainfall detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: PBSED (NLFEE) !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(OUT) :: RHOSO (NS) !! Soil bulk density by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: SOSDFN (NSEE, NSEDEE) !! Soil sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: XDRIP (NV) !! Canopy drip fall height by vegetation type.
      DOUBLE PRECISION, INTENT(OUT) :: DLSMAX     !! Loose-sediment depth above which hillslope soil erosion is suppressed.

      ! INOUT Output Arrays (modified via ALALLF slices/subroutines)
      DOUBLE PRECISION, INTENT(INOUT) :: DLS (NEL) !! Initial loose/bed sediment depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA (NELEE, NSEDEE) !! Initial sediment composition by element and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FCG (NLF + 1:NEL)     !! Ground-cover fraction by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: FCROCK (NLF + 1:NEL)  !! Rock-cover fraction by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: FDEL (NELEE, NSEDEE)  !! Initial mobile sediment concentration fraction.
      DOUBLE PRECISION, INTENT(INOUT) :: PLS (NLF + 1:NEL)     !! Loose-sediment porosity by land element.

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT) :: IDUM !! Integer workspace for distributed reads.
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY !! Floating-point workspace for distributed reads.
      DOUBLE PRECISION, DIMENSION(NLFEE * NSEDEE), INTENT(INOUT) :: DUMSED !! Flattened sediment-size workspace for distributed reads.

      CHARACTER(80)  :: CDUM
      CHARACTER(132) :: MSG
      CHARACTER(8)   :: SYDVER
      INTEGER :: BB, IDUM0, I0, IEL, ICAT, ITYPE, NC, NUM_CATEGORIES_TYPES, NNN, NREQ, SED, SOIL

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------
      !
      !     * Check status of data file
      CALL ALREAD (0, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)

      !     * Print SY job title
      CALL ALREAD (1, SYD, SPR, ':SY01', 1, 1, IDUM0, CDUM, IDUM, DUMMY)
      WRITE (SPR, '(/1X,A/)') CDUM

      !     * Check & print version number
      CALL ALREAD (1, SYD, SPR, ':SY02', 1, 1, IDUM0, SYDVER, IDUM, DUMMY)

      !     * [miss off last character to allow eg '3.4.1' is ok in '3.4.1a' ]
      IF (INDEX (SYDVER, SYVER (:LEN (SYVER) - 1) ) == 0) THEN
         WRITE (MSG, 9011) SYVER, SYDVER
         CALL RAISE_ERROR (ERRLVL_warn, 2011, SPR, 0, 0, MSG)
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
         CALL RAISE_ERROR (ERRLVL_fatal, 2005, SPR, 0, 0, MSG)
      END IF

      !     * Integer
      NNN = 5
      IF (NLF > 0) NNN = 8
      CALL ALREAD (2, SYD, SPR, ':SY11', NNN, 1, IDUM0, CDUM, IDUM, DUMMY)
      NSED = IDUM (1)
      ISGSED = IDUM (2)
      ISTEC = IDUM (3)
      ISSYOK = IDUM (4)
      NEPS = IDUM (5)

      IF (NLF > 0) THEN
         ISACKW = IDUM (6)
         ISUSED = IDUM (7)
         NFINE = IDUM (8)
      END IF

      IF (NSED < 1 .OR. NSED > NSEDEE) THEN
         WRITE (MSG, 9006) NSED, NSEDEE
         CALL RAISE_ERROR (ERRLVL_fatal, 2006, SPR, 0, 0, MSG)
      END IF

      !     * Floating-point
      NNN = 2
      IF (NLF > 0) NNN = 7
      CALL ALREAD (3, SYD, SPR, ':SY12', NNN, 1, IDUM0, CDUM, IDUM, DUMMY)
      FPCRIT = DUMMY (1)
      DLSMAX = DUMMY (2)

      IF (NLF > 0) THEN
         ALPHA = DUMMY (3)
         CONCOB = DUMMY (4)
         DCBEDO = DUMMY (5)
         FBIC = DUMMY (6)
         FICRIT = DUMMY (7)
      END IF


      ! 2. Sediment, Soil & Vegetation Properties
      ! -----------------------------------------
      !
      !     * Check workspace array size: part 2
      NREQ = MAX (MAX (5, NSED) * NS, 3 * NV)
      IF (NELEE < NREQ) THEN
         WRITE (MSG, 9005) NELEE, NREQ
         CALL RAISE_ERROR (ERRLVL_fatal, 2005, SPR, 0, 0, MSG)
      END IF

      !     * Sediment
      CALL ALREAD (3, SYD, SPR, ':SY21', NSED, 1, IDUM0, CDUM, IDUM, DRSED)

      !     * Soil
      CALL ALREAD (3, SYD, SPR, ':SY22', 5, NS, IDUM0, CDUM, IDUM, DUMMY)
      CALL DCOPY (NS, DUMMY (1), 5, GKR, 1)
      CALL DCOPY (NS, DUMMY (2), 5, GKF, 1)
      CALL DCOPY (NS, DUMMY (3), 5, RHOSO, 1)
      CALL DCOPY (NS, DUMMY (4), 5, FPCLAY, 1)
      CALL DCOPY (NS, DUMMY (5), 5, BKB, 1)

      !     * Soil composition
      CALL ALREAD (3, SYD, SPR, ':SY23', NSED, NS, IDUM0, CDUM, IDUM, DUMMY)

      DO SED = 1, NSED
         CALL DCOPY (NS, DUMMY (SED), NSED, SOSDFN (1, SED), 1)
      END DO

      !     * Vegetation
      CALL ALREAD (3, SYD, SPR, ':SY24', 3, NV, IDUM0, CDUM, IDUM, DUMMY)
      CALL DCOPY (NV, DUMMY (1), 3, XDRIP, 1)
      CALL DCOPY (NV, DUMMY (2), 3, DRDRIP, 1)
      CALL DCOPY (NV, DUMMY (3), 3, FDRIP, 1)


      ! 3. Link Element Properties
      ! --------------------------
      !
      IF (NLF > 0) THEN
         ! * Bank soil type
         CALL ALREAD (2, SYD, SPR, ':SY31', NLF, 1, IDUM0, CDUM, NTSOBK, DUMMY)

         ! * Porosity of bed sediment
         CALL ALREAD (3, SYD, SPR, ':SY32', NLF, 1, IDUM0, CDUM, IDUM, PBSED)
      END IF


      ! 4. Column-element Properties
      ! ----------------------------
      !
      !     * Ground cover
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY41', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
         ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FCG, IDUM, DUMMY)

      !     * Rock cover
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY42', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
         ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FCROCK, IDUM, DUMMY)

      !     * Porosity of loose sediment
      CALL ALALLF (1, 1, 0, SYD, SPR, ':SY43', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
         ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, PLS, IDUM, DUMMY)


      ! 5. All-element Initialization
      ! -----------------------------
      !
      !     * Initial depth of loose/bed sediment
      CALL ALALLF (0, 1, 0, SYD, SPR, ':SY51', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, &
         ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, DLS, IDUM, DUMMY)

      !     * Initial composition of loose/bed sediment ...
      CALL ALALLF (0, NSED, - 1, SYD, SPR, ':SY52', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE,   &
         ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FBETA, IDUM, DUMMY)

      !     ... with special option to inherit composition of soil
      IF (NUM_CATEGORIES_TYPES < 0) THEN
         DO IEL = 1, NLF
            SOIL = NTSOBK (IEL)
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), NELEE)
         END DO

         DO IEL = NLF + 1, NEL
            SOIL = NTSOTP (IEL)
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), NELEE)
         END DO
      END IF

      !     * Initial concentrations of suspended sediment
      CALL ALALLF (0, NSED, 0, SYD, SPR, ':SY53', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE,     &
         ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, FDEL, IDUM, DUMMY)


      ! 6. Boundary Data
      ! ----------------
      !
      !     * No of inflow boundary elements & no of categories of each type
      CALL ALREAD (2, SYD, SPR, ':SY61', 5, 1, IDUM0, CDUM, IDUM, DUMMY)
      NSYB = IDUM (1)
      DO ITYPE = 1, 4
         NSYC (ITYPE) = IDUM (1 + ITYPE)
      END DO

      IF (NSYB > 0) THEN
         IF (NSYB > NSYBEE) THEN
            WRITE (MSG, 9007) NSYB, NSYBEE
            CALL RAISE_ERROR (ERRLVL_fatal, 2007, SPR, 0, 0, MSG)
         END IF

         ! * Check workspace array size: part 3
         NREQ = MAX (3 * NSYB, NSED * NSYC (1), NSED * 2 * NSYC (3) )
         IF (NELEE < NREQ) THEN
            WRITE (MSG, 9005) NELEE, NREQ
            CALL RAISE_ERROR (ERRLVL_fatal, 2005, SPR, 0, 0, MSG)
         END IF

         ! * Integer boundary data
         CALL ALREAD (2, SYD, SPR, ':SY62', 3, NSYB, IDUM0, CDUM, IDUM, DUMMY)
         I0 = 0

         DO BB = 1, NSYB
            IEL = IDUM (I0 + 1)
            ITYPE = IDUM (I0 + 2)
            ICAT = IDUM (I0 + 3)

            IF (ITYPE < 1 .OR. ITYPE > 4) THEN
               WRITE (MSG, 9008) BB, ITYPE
               CALL RAISE_ERROR (ERRLVL_fatal, 2008, SPR, 0, 0, MSG)
            END IF

            ! * condense 4 into 2 by adding cats 2 & 4 to lists for 1 & 3
            IF (MOD (ITYPE, 2) == 0) ICAT = ICAT + NSYC (ITYPE - 1)
            NSYBCD (BB, 1) = IEL
            NSYBCD (BB, 2) = ITYPE
            NSYBCD (BB, 3) = ICAT
            I0 = I0 + 3
         END DO

         ! * Steady flux data
         NC = NSYC (1)
         IF (NC > 0) THEN
            IF (NC > NSYCEE) THEN
               WRITE (MSG, 9009) NSYC (1), NSYCEE
               CALL RAISE_ERROR (ERRLVL_fatal, 2009, SPR, 0, 0, MSG)
            END IF

            CALL ALREAD (3, SYD, SPR, ':SY63', NSED, NC, IDUM0, CDUM, IDUM, DUMMY)
            DO SED = 1, NSED
               CALL DCOPY (NC, DUMMY (SED), NSED, GBC (SED, 1), NSEDEE)
            END DO
         END IF

         ! * Steady rating curve data
         NC = NSYC (3)
         IF (NC > 0) THEN
            IF (NC > NSYCEE) THEN
               WRITE (MSG, 9010) NSYC (3), NSYCEE
               CALL RAISE_ERROR (ERRLVL_fatal, 2010, SPR, 0, 0, MSG)
            END IF

            CALL ALREAD (3, SYD, SPR, ':SY64', NSED * 2, NC, IDUM0, CDUM, IDUM, DUMMY)
            DO SED = 1, NSED
               CALL DCOPY (NC, DUMMY (2 * SED - 1), 2 * NSED, ABC (SED, 1), NSEDEE)
               CALL DCOPY (NC, DUMMY (2 * SED), 2 * NSED, BBC (SED, 1), NSEDEE)
            END DO
         END IF
      END IF


      ! 7. Epilogue
      ! -----------
      !
      !     * Close the data file
      CALL ALREAD ( - 1, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)

      RETURN

      ! Format Statements ----------------------------------------------------
9003  FORMAT ( 1X,A )

9005  FORMAT ('Workspace available is NELEE = ', I5, &
         '; workspace required in subroutine SYREAD is ', I6 )

9006  FORMAT ('No. of size groups NSED=', I4, &
         ' is not in range [1,NSEDEE=', I3, ']')

9007  FORMAT ('No. of boundaries NSYB=', I5, &
         ' is greater than NSYBEE=', I4, ']')

9008  FORMAT ('Boundary type NSYBCD(', I4, ',2)=', I2, &
         ' is not is the range [1,4]')

9009  FORMAT ('No. of steady flux categories NSYC(1)=', I4, &
         ' is greater than NSYCEE=', I3, ']')

9010  FORMAT ('No. of steady rating categories NSYC(3)=', I4, &
         ' is greater than NSYCEE=', I3, ']')

9011  FORMAT ('SY module is version ', A, '; SYD data file is version ', A)

   END SUBROUTINE SYREAD



!> Derives water-dependent geometry, slopes, shear stresses, and rainfall for the sediment component.
!>
!> `SYWAT` calculates every quantity the sediment routines need that is purely
!> a function of the current water-flow state, ahead of erosion and transport
!> calculations in [[symain]].
!>
!> For each land element, the effective median raindrop/leaf-drip diameter
!> combines a minimum drop size with the drip contribution and a splash-derived
!> term:
!>
!> \[
!>   D = \max\left(D_{min},\; DRDRIP\,{DRAINA\over PNETTO},\;
!>                 0.01935\,PNETTO^{0.182}\right)\qquad(PNETTO>0),
!> \]
!>
!> and the direct (non-drip) rainfall rate is
!>
!> \[
!>   LRAIN = {\max(PNETTO-DRAINA,0)\over 1-FCC}\qquad(FCC<1).
!> \]
!>
!> For every element and face, the water surface slope and ground-surface shear
!> stress are derived from neighbouring water levels `HRF` and face distances
!> `DHF`, extrapolating across boundary and confluence-node faces as needed:
!>
!> \[
!>   SLOPEJ = {|H_{iel}-H_{adj}|\over DHF_{iel}+DHF_{adj}},\qquad
!>   TAUJ = \rho_w g\,DWAT1\,SLOPEJ .
!> \]
!>
!> At bank faces between a link and a land element, both water levels are
!> capped below by the bankfull elevation `ZBFULL` before computing slope, so
!> below-bank flow does not contribute to bank shear. Confluence-node faces
!> additionally set `FQCONF`, the fraction of node outflow attributed to each
!> receiving branch, used later by [[symain]] to distribute sediment fluxes.
!> The representative link/element shear stress `TAUK` is the `TAUJ` value at
!> the face carrying the largest absolute discharge.
!>
!> @note `SLOPEJ`, `TAUJ`, `FQCONF`, `LRAIN`, and `DRDROP` are fully
!> zero-initialised at the start of the routine, then only overwritten for the
!> faces/elements this routine actually computes (link element side faces are
!> skipped for `SLOPEJ`/`TAUJ`, and confluence branches are skipped for
!> `FQCONF` unless they are the active outflow). This is a modernisation of the
!> original behaviour, which left those entries at their previous, indeterminate
!> values; no code reads `SLOPEJ`/`TAUJ` at link side faces (see the manual note
!> below), so the change does not affect any consumer of these arrays.
!> @endnote
!>
!> @note `FQCONF` is defined only for branches flowing into a node; `SLOPEJ` and
!> `TAUJ` are not defined (by design) at side faces of links.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-06 | SvB | 4.6.1 | Removed `GOTO`-driven control flow; replaced the legacy statement function used for face outflow with the internal `FUNCTION` `FQOUT`. |
!> | 2026-05-03 | SvB | 4.6.1 | Added the explicit zero-initialisation of `SLOPEJ`/`TAUJ`/`FQCONF`/`LRAIN`/`DRDROP` described in the preceding note. |
!> @endhistory
   PURE SUBROUTINE SYWAT(NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, &
      DHF, DRDRIP, LINKNS, ZBFULL, ZGRUND, CLAI, DRAINA, HRF, PLAI, &
      PNETTO, QOC, DRDROP, DWAT1, FCC, FQCONF, LRAIN, SLOPEJ, TAUJ, &
      TAUK)

      IMPLICIT NONE

      ! Input arguments
      ! NB: Don't use NLF as array size: it may be zero
      INTEGER, INTENT(IN) :: NEL   !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE !! Link-array dimension.
      INTEGER, INTENT(IN) :: NV    !! Number of vegetation types.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:3) !! Face-neighbour and reverse-face reference map.
      INTEGER, INTENT(IN) :: ICMRF2(NLFEE, 3, 2)   !! Confluence branch reference map.
      INTEGER, INTENT(IN) :: NVC(NLF + 1 : NEL) !! Vegetation type by land element.
      DOUBLE PRECISION, INTENT(IN) :: CLAI(NV)   !! Current canopy leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: DHF(NELEE, 4) !! Face-to-face hydraulic distance.
      DOUBLE PRECISION, INTENT(IN) :: DRAINA(NLF + 1 : NEL) !! Canopy-drip rainfall reaching the ground.
      DOUBLE PRECISION, INTENT(IN) :: DRDRIP(NV) !! Canopy drip drop diameter by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: HRF(NEL)   !! Water level/head by element.
      DOUBLE PRECISION, INTENT(IN) :: PLAI(NV)   !! Potential/maximum leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: PNETTO(NLF + 1 : NEL) !! Net precipitation/effective rainfall by land element.
      DOUBLE PRECISION, INTENT(IN) :: QOC(NELEE, 4)  !! Face water fluxes.
      DOUBLE PRECISION, INTENT(IN) :: ZBFULL(NLFEE)  !! Bankfull elevation/depth by link.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND(NEL)    !! Ground or bed elevation by element.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE) !! True for north-south channel links.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: DRDROP(NLF + 1 : NEL) !! Effective raindrop/drop diameter by land element.
      DOUBLE PRECISION, INTENT(OUT) :: DWAT1(NEL) !! Surface/channel water depth by element.
      DOUBLE PRECISION, INTENT(OUT) :: FCC(NV)    !! Canopy/ground sheltering fraction by vegetation type.
      DOUBLE PRECISION, INTENT(OUT) :: FQCONF(NLFEE, 3)  !! Confluence outflow fractions for receiving branches.
      DOUBLE PRECISION, INTENT(OUT) :: LRAIN(NLF + 1 : NEL) !! Effective direct rainfall rate by land element.
      DOUBLE PRECISION, INTENT(OUT) :: SLOPEJ(NELEE, 4) !! Face water-surface slopes.
      DOUBLE PRECISION, INTENT(OUT) :: TAUJ(NELEE, 4)   !! Face shear stress.
      DOUBLE PRECISION, INTENT(OUT) :: TAUK(NEL) !! Representative element/link shear stress.

      ! Locals, etc
      DOUBLE PRECISION, PARAMETER :: DRDMIN = 1.0D-4

      DOUBLE PRECISION :: DRAINE, DWAT1E, FCCE, HRFE, PNETTE, SLOPEE, TAUJE
      DOUBLE PRECISION :: D, DA, DE, HA, HE, L
      DOUBLE PRECISION :: Q, QABS, QMAX, QOUT, QOUTX(0:3), QSUM, TAUMAX, ZBF
      INTEGER :: FACE, IADJ, IBR, ICOL, IEL, IELP
      INTEGER :: KADJ, KEL, KELP, LINK, P, PADJ, PIN, POUT, VEG
      LOGICAL :: BSIDE

      !----------------------------------------------------------------------*

      ! Modernization Fix: Fully initialize INTENT(OUT) arrays to prevent garbage memory
      ! on elements skipped by the internal logic (like side faces)
      SLOPEJ = 0.0D0
      TAUJ   = 0.0D0
      FQCONF = 0.0D0
      LRAIN  = 0.0D0
      DRDROP = 0.0D0

      ! Loop over Vegetation Types
      ! --------------------------
      !
      !     * Calculate ground fraction sheltered from rain by canopy
      FCC(1:NV) = PLAI(1:NV) * MIN(CLAI(1:NV), 1.0D0)

      ! Loop over Column Elements
      ! -------------------------
      !
      column_loop: DO ICOL = NLF + 1, NEL
         ! * Avoid multiple array references
         DRAINE = DRAINA(ICOL)
         PNETTE = PNETTO(ICOL)
         VEG = NVC(ICOL)
         FCCE = FCC(VEG)

         ! * Calculate median raindrop/leaf-drip diameter
         D = DRDMIN
         IF (PNETTE > 0.0D0) THEN
            D = MAX(D, DRDRIP(VEG) * (DRAINE / PNETTE), 0.01935D0 * PNETTE**0.182D0)
         END IF
         DRDROP(ICOL) = D

         ! * Calculate rainfall rate
         L = 0.0D0
         IF (FCCE < 1.0D0) L = DIMJE(PNETTE, DRAINE) / (1.0D0 - FCCE)
         LRAIN(ICOL) = L
      END DO column_loop

      ! Loop over All Elements
      ! ----------------------
      !
      element_loop: DO IEL = 1, NEL
         ! * Avoid multiple array references
         HRFE = HRF(IEL)

         ! * Calculate (& store) surface water depth
         DWAT1E = DIMJE(HRFE, ZGRUND(IEL))
         DWAT1(IEL) = DWAT1E

         ! * Initialize maximum flow & shear stress
         QMAX = 0.0D0
         TAUMAX = 0.0D0

         ! Loop over Faces ...
         ! -------------------
         ! ... of this element, in order to set FQCONF, SLOPEJ and TAUJ,
         ! and to find a value for TAUK
         !
         face_loop: DO FACE = 1, 4

            ! * Not interested in link element side faces
            BSIDE = IEL <= NLF
            IF (BSIDE) BSIDE = (MOD(FACE, 2) == 1) .EQV. LINKNS(IEL)
            IF (BSIDE) CYCLE face_loop

            ! * Discharge rate
            QOUT = FQOUT(IEL, FACE)

            ! * No-flow faces are special case
            IF (ISZERO(QOUT)) THEN
               ! * (consider weirs and branch nodes for example)
               SLOPEJ(IEL, FACE) = 0.0D0
               TAUJ(IEL, FACE) = 0.0D0
               CYCLE face_loop
            END IF

            ! * Find neighbouring element, & its face (also set FQCONF)
            KEL = FACE
            IADJ = ICMREF(IEL, KEL, 2)
            IF (IADJ == 0) THEN
               ! * This is a boundary face; extrapolate from behind ...
               KEL = 1 + MOD(FACE + 1, 4)
               IADJ = ICMREF(IEL, KEL, 2)
            END IF

            IF (IADJ == 0) THEN
               ! * ... unless that's a boundary too; then go for slope=0
               IADJ = IEL
               KADJ = KEL
            ELSE IF (IADJ > 0) THEN
               ! * Neighbour is a regular element
               KADJ = ICMREF(IEL, KEL, 3)
            ELSE
               ! * Extra things to do if neighbour is a confluence node
               ! * Branch index
               IBR = -IADJ

               ! * Initialize locals for prospect-loop:
               ! - gross discharge from the node
               QSUM = 0.0D0
               ! - prospects with maximal inflow/outflow
               PIN = 0
               POUT = 0
               ! - discharge from node (let this branch be prospect 0)
               QOUTX(0) = -FQOUT(IEL, KEL)

               ! * Loop over Prospects
               DO P = 1, 3
                  IELP = ICMRF2(IBR, P, 1)
                  IF (IELP > 0) THEN
                     KELP = ICMRF2(IBR, P, 2)
                     Q = -FQOUT(IELP, KELP)
                     QSUM = QSUM + MAX(0.0D0, Q)
                     IF (Q < QOUTX(PIN)) PIN = P
                     IF (Q > QOUTX(POUT)) POUT = P
                  ELSE
                     Q = 0.0D0
                  END IF
                  QOUTX(P) = Q
               END DO

               ! * Redefine neighbour as link with maximal outflow ...
               PADJ = POUT
               ! * ... unless node is at inflow face for this element
               IF (QOUTX(0) > 0.0D0) PADJ = PIN

               IF (PADJ > 0) THEN
                  IADJ = ICMRF2(IBR, PADJ, 1)
                  KADJ = ICMRF2(IBR, PADJ, 2)
               ELSE
                  ! * (no obvious candidate: go for slope=0)
                  IADJ = IEL
                  KADJ = KEL
               END IF

               ! * Calculate node outflow fractions if appropriate
               IF (QOUT > 0.0D0 .AND. KEL == FACE) THEN
                  ! * NB: Need precondition on QOC to ensure QSUM.GT.0
                  DO P = 1, 3
                     FQCONF(IBR, P) = MAX(0.0D0, QOUTX(P)) / QSUM
                  END DO
               END IF

            END IF

            ! * Calculate water surface slope
            HE = HRFE
            HA = HRF(IADJ)
            DE = DHF(IEL, KEL)
            DA = DHF(IADJ, KADJ)

            IF ((IEL <= NLF) .NEQV. (IADJ <= NLF)) THEN
               ! * this is a bank face; use bank-full elevation as cut-off
               LINK = MIN(IEL, IADJ)
               ZBF = ZBFULL(LINK)
               IF (HE <= ZBF) THEN
                  HE = ZBF
                  DE = 0.0D0
               END IF
               IF (HA <= ZBF) THEN
                  HA = ZBF
                  IF (DE > 0.0D0) DA = 0.0D0
               END IF
            END IF

            SLOPEE = ABS(HE - HA) / (DE + DA)
            SLOPEJ(IEL, FACE) = SLOPEE

            ! * Calculate flow shear stress at the ground surface
            TAUJE = RHOWAT * GRAVTY * DWAT1E * SLOPEE
            TAUJ(IEL, FACE) = TAUJE

            ! * Find maximum flow rate so far and TAUJ for that face
            QABS = ABS(QOUT)
            IF (QABS > QMAX) THEN
               QMAX = QABS
               TAUMAX = TAUJE
            END IF

            ! * Next face
         END DO face_loop

         ! * Set representative shear stress equal to maximum over faces
         TAUK(IEL) = TAUMAX

         ! * Next element
      END DO element_loop

   CONTAINS

      !> Outflow rate at one element/face, positive for outflow (see the manual face-sign convention in [[syerr3]]).
      PURE DOUBLE PRECISION FUNCTION FQOUT(IEL, FACE)
         INTEGER, INTENT(IN) :: IEL  !! Element index.
         INTEGER, INTENT(IN) :: FACE !! Face index (1-4).
         FQOUT = SIGN(1, 2 - FACE) * QOC(IEL, FACE)
      END FUNCTION FQOUT

   END SUBROUTINE SYWAT



!> Placeholder for sediment mass-balance output.
!>
!> Sediment process state is updated by [[symain]], but this routine currently
!> performs no accumulation, checking, state mutation, or reporting. It is
!> called from the main simulation loop only to preserve the historical component
!> interface for sediment balances.
   SUBROUTINE BALSED
   end subroutine BALSED

END MODULE SYmod
