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
!>
!> @warning
!> The manual defines sediment boundary-condition input groups `SY61`-`SY64`,
!> but explicitly states that the sediment boundary-condition routines have not
!> yet been implemented. This matches the current empty [[sybc]] routine.
!> Sediment mass-balance output is also still a placeholder in [[balsed]].
!> @endwarning
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into this Fortran 90 module. |
!> | 2026-03 | SB | 4.6 | Updated `NTSOIL` dimensions for current array layout. |
!> @endhistory
MODULE SYmod
USE SGLOBAL
!USE AL_P
USE mod_load_filedata, ONLY : ALINIT, ALCHKI, ALCHK, ALALLF, ALREAD  !, HELPPATH
USE mod_load_filedata, ONLY : ERROR, ERRC, ERRNEE, ERRTOT !AD NEEDS THIS
USE UTILSMOD, ONLY : DCOPY
IMPLICIT NONE

LOGICAL         :: FIRST_syackw=.TRUE.  !! True until Ackers-White constants have been initialised.
DOUBLEPRECISION :: K2_syackw            !! Ackers-White grain-size scaling constant.
DOUBLEPRECISION :: DGRMAX_syackw        !! Upper bound for Ackers-White dimensionless grain size.
DOUBLEPRECISION :: ROOT32_syackw        !! Cached `sqrt(32)` for Ackers-White velocity scaling.

LOGICAL         :: FIRST_sycltr=.TRUE.  !! True until channel-transport constants have been initialised.
DOUBLEPRECISION :: k1_sycltr            !! Sediment settling/velocity conversion constant used by channel transport.

LOGICAL          :: FIRST_sycrit=.TRUE. !! True until critical-shear constants have been initialised.
DOUBLEPRECISION  :: K1_sycrit           !! Shields critical-shear coefficient for small particles.
DOUBLEPRECISION  :: K2_sycrit           !! Shields critical-shear coefficient for transition particles.
DOUBLEPRECISION  :: K3_sycrit           !! Shields critical-shear coefficient for large particles.

LOGICAL         :: FIRST_syengh=.TRUE.  !! True until Engelund-Hansen constants have been initialised.
DOUBLEPRECISION :: KG_syengh            !! Engelund-Hansen transport coefficient.

LOGICAL         :: FIRST_syfine=.TRUE.  !! True until fine-sediment constants have been initialised.
DOUBLEPRECISION :: WSED_syfine          !! Fine-sediment settling velocity.


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

LOGICAL         :: FIRST_syovtr= .TRUE.  !! True until overland-transport constants have been initialised.
DOUBLEPRECISION :: K1_syovtr             !! Engelund-Hansen overland-capacity coefficient.
DOUBLEPRECISION :: K3_syovtr             !! Yalin overland-capacity coefficient.
DOUBLEPRECISION :: K4_syovtr             !! Yalin overland-capacity coefficient.

PRIVATE
PUBLIC :: SYMAIN, BALSED, & !REST NEEDED ONLY FOR AD
          issyok_symain
CONTAINS

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
SUBROUTINE SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, &
 DRSED, ARXL, DCBSED, DWAT1, QOC, TAUJ, ACKW, GSED)

USE CONST_SY

INTEGER :: ISACKW                  !! Ackers-White option: 1 standard, 2 Day bed-percentile modification.
INTEGER :: NFINE                   !! Number of fine sediment classes excluded from this calculation.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NSED                    !! Number of sediment size classes.
DOUBLEPRECISION DRSED (NFINE+1:NSED) !! Representative non-fine particle diameters.
DOUBLEPRECISION ARXL (NLF)         !! Channel cross-sectional area by link.
DOUBLEPRECISION DWAT1 (NLF)        !! Channel water depth by link.
DOUBLEPRECISION QOC (NELEE, 4)     !! Face water fluxes.
DOUBLEPRECISION DCBSED (NLFEE, NFINE+1:NSED) !! Active-bed sediment depth by link and non-fine class.
DOUBLEPRECISION TAUJ (NELEE, 4)    !! Face shear stress.
LOGICAL :: LINKNS (NLF)            !! True for north-south channel links.
DOUBLEPRECISION ACKW (5, NFINE+1:NSED) !! Cached Ackers-White parameters by non-fine class.
DOUBLEPRECISION GSED (NLF, NFINE+1:NSED) !! Channel transport capacity by link and non-fine class.
!
! Locals, etc
DOUBLEPRECISION DGRSML, F16, F50, F56, F84, KRHO, THIRD
PARAMETER (KRHO = RHOSED / RHOWAT - 1, DGRSML = 1D-4)
PARAMETER (F16 = 0.16D0, F50 = 0.5d0, F56 = 0.56D0, F84 = 0.84D0)
PARAMETER (THIRD = 1 / 3D0)
!
DOUBLEPRECISION AAW, ARXLE, CAW, DAAA, DBED16, DBED50, DBED84, &
 DGR
DOUBLEPRECISION DSED, DUM, DWAT1E, FGR, G, H10, LGR, &
 MAW
DOUBLEPRECISION NAW, QK, UGR, USTR, UK
INTEGER :: FACE, IEND, LINK, NFP1, NNF, SED, SGN
!
DOUBLEPRECISION FDGR, FA
!

!
FDGR (DUM) = MAX (ONE, MIN (K2_syackw * DUM, DGRMAX_syackw) )
FA (DUM) = 0.14 + 0.23 / SQRT (DUM)
!
!----------------------------------------------------------------------*
!
!
!     * Initialization
NNF = NSED-NFINE
NFP1 = NFINE+1
IF (FIRST_syackw) THEN
   FIRST_syackw = .FALSE.
   K2_syackw = (GRAVTY * KRHO / VISCOS**2) **THIRD
   DGRMAX_syackw = 1D1** (ONE / F56) + DGRSML
   ROOT32_syackw = SQRT (32D0)
   DO 100 SED = NFP1, NSED
      DGR = FDGR (DRSED (SED) )
      LGR = LOG10 (DGR)
      ACKW (1, SED) = DIMJE(ONE, F56 * LGR)
      IF (ISACKW.EQ.1) ACKW (2, SED) = FA (DGR)
      ACKW (3, SED) = 1.34 + 9.66 / DGR
      ACKW (4, SED) = 10** ( (2.86 - LGR) * LGR - 3.53)
      ACKW (5, SED) = ONE / SQRT (GRAVTY * KRHO * DRSED (SED) )
  100    END DO
ENDIF
!
!
!     * Zero GSED
CALL ALINIT (ZERO, NLF * NNF, GSED)
!
!     * Loop over ends of each link
DO 500 IEND = 1, 3, 2
   SGN = 2 - IEND
!
!        * Loop over all channel links
   DO 400 LINK = 1, NLF
!
!           * Determine face equivalent to this end, and flow rate there
      FACE = IEND
      IF (LINKNS (LINK) ) FACE = FACE+1
      QK = SGN * QOC (LINK, FACE)
!
!           * Check that this end is outflowing
      IF (QK.GT.ZERO) THEN
!
!              * Copy array elements to local variables
         ARXLE = ARXL (LINK)
         DWAT1E = DWAT1 (LINK)
         H10 = 10 * DWAT1E
!
!              * Determine shear velocity and water flow velocity
         USTR = SQRT (TAUJ (LINK, FACE) / RHOWAT)
         UK = ZERO
         IF (ARXLE.GT.ZERO) UK = QK / ARXLE
!
!
!              * Set A-W parameters for the Day modification if needed
         IF (ISACKW.EQ.2) THEN
!
            DBED84 = SYDR (F84, NLFEE, NNF, DCBSED (LINK, NFP1), &
             DRSED (NFP1) )
!
            IF (DBED84.GT.ZERO) THEN
               DBED50 = SYDR (F50, NLFEE, NNF, DCBSED (LINK, NFP1) &
                , DRSED (NFP1) )
               DBED16 = SYDR (F16, NLFEE, NNF, DCBSED (LINK, NFP1) &
                , DRSED (NFP1) )
               DAAA = 1.62 * DBED50 * (DBED16 / DBED84) **0.28
            ELSE
               DAAA = ZERO
            ENDIF
!
            DGR = FDGR (DAAA)
            AAW = FA (DGR)
            DO 200 SED = NFP1, NSED
               ACKW (2, SED) = AAW * (0.6 + 0.4 * SQRT (DAAA / &
                DRSED (SED) ) )
  200             END DO
!
         ENDIF
!
!
!              * Loop over sediment types
         DO 300 SED = NFP1, NSED
!
!                 * Set A-W parameters for this Sediment size group
            NAW = ACKW (1, SED)
            AAW = ACKW (2, SED)
            MAW = ACKW (3, SED)
            CAW = ACKW (4, SED)
            DSED = DRSED (SED)
!
!                 * Calculate particle mobility
            UGR = ZERO
            IF (DSED.LT.H10) UGR = UK / (ROOT32_syackw * LOG10 (H10 / &
             DSED) )
            FGR = ACKW (5, SED)
            IF (NAW.GT.ZERO) FGR = FGR * USTR**NAW
            IF (NAW.LT.ONE) FGR = FGR * UGR** (ONE-NAW)
!
!                 * Determine discharge capacity for this end
            G = ZERO
            IF (DWAT1E.GT.ZERO) G = DSED * (QK / DWAT1E) * CAW * &
             DIMJE(FGR / AAW, ONE) **MAW
            IF (NAW.GT.ZERO.AND.G.GT.ZERO) G = G * (UK / USTR) ** &
             NAW
!
!                 * Determine the total discharge capacity of both ends
            GSED (LINK, SED) = GSED (LINK, SED) + G
!
!              * Next sediment type
  300          END DO
!
!
!           * End of outflow check
      ENDIF
!
!        * Next link
  400    END DO
!
!     * Other end of link
  500 END DO
!
!
END SUBROUTINE SYACKW



!> Placeholder for time-varying sediment boundary flows.
!>
!> The current implementation is intentionally empty; boundary sediment fluxes
!> are instead handled through existing arrays and setup pathways in [[symain]].
!> The manual's `SY61`-`SY64` boundary records are therefore validated/read as
!> metadata, but no time-varying sediment boundary flux is applied here.
SUBROUTINE SYBC
!!!!STOP ' FATAL ERROR!!  Sediment boundary flows not yet implemented'
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
SUBROUTINE SYBED (DCBEDO, NELEE, NLF, NLFEE, NSED, CWIDTH, DCIPRM, &
 DDIPRM, ARBDEP, DLS, FBETA, DCBSED, DDBSED, DCBED)

INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NSED                    !! Number of sediment size classes.
DOUBLEPRECISION DCBEDO             !! Target active upper channel-bed layer thickness.
DOUBLEPRECISION CWIDTH (NLF)       !! Channel width by link.
DOUBLEPRECISION DCIPRM (NLFEE, NSED) !! Interim upper-bed sediment depth by link and size class.
DOUBLEPRECISION DDIPRM (NLFEE, NSED) !! Interim lower-bed sediment depth by link and size class.
DOUBLEPRECISION ARBDEP (NLF)       !! Accumulated channel-bed elevation/depth change.
DOUBLEPRECISION DLS (NLF)          !! Total channel-bed sediment depth.
DOUBLEPRECISION FBETA (NELEE, NSED) !! Whole-bed sediment fraction by element/link and size class.
DOUBLEPRECISION DCBSED (NLFEE, NSED) !! Updated upper-bed sediment depth by link and size class.
DOUBLEPRECISION DDBSED (NLFEE, NSED) !! Updated lower-bed sediment depth by link and size class.
DOUBLEPRECISION DCBED (NLF)        !! Updated active upper-bed layer depth by link.
!
! Locals, etc
DOUBLEPRECISION AC, AD, DCBEDZ, DCC, DCNEW, DDBEDZ, DLSNEW, &
 DLSOLD
DOUBLEPRECISION DCIPP, DDIPP, DCINEW, SUMSED
INTEGER :: LINK, SED
!
!
!----------------------------------------------------------------------*
!
!
!     * Loop over links
DO 300 LINK = 1, NLF
!
!
!        * Calculate interim bed layer thicknesses
   DCBEDZ = 0
   DDBEDZ = 0
   DO 100 SED = 1, NSED
      DCBEDZ = DCBEDZ + DCIPRM (LINK, SED)
      DDBEDZ = DDBEDZ + DDIPRM (LINK, SED)
  100    END DO
!
!        * Reset variables that are independent of size group
   DLSOLD = DLS (LINK)
   DLSNEW = DCBEDZ + DDBEDZ
   DLS (LINK) = DLSNEW
   ARBDEP (LINK) = ARBDEP (LINK) + CWIDTH (LINK) * (DLSNEW - &
    DLSOLD)
   DCNEW = MIN (DLSNEW, DCBEDO)
   DCBED (LINK) = DCNEW
!
!        * What fraction of the interim top layer remains in the top
!        *  layer, and what fraction of the interim bottom layer becomes
!        *  part of the top?
   DCC = MIN (DCBEDZ, DCNEW)
   AC = 0
   AD = 0
   IF (DCBEDZ.GT.0) AC = DCC / DCBEDZ
   IF (DDBEDZ.GT.0) AD = (DCNEW - DCC) / DDBEDZ
!
!        * Loop over sediment size groups
   DO 200 SED = 1, NSED
!
!           * Interim layer depths
      DCIPP = DCIPRM (LINK, SED)
      DDIPP = DDIPRM (LINK, SED)
!
!           * Total depth (for this size group)
      SUMSED = DCIPP + DDIPP
!
!           * New top layer depth
      DCINEW = AC * DCIPP + AD * DDIPP
      DCBSED (LINK, SED) = DCINEW
!
!           * New bottom layer depth
      DDBSED (LINK, SED) = SUMSED-DCINEW
!
!           * Composition of both layers together
      IF (DLSNEW.GT.0) FBETA (LINK, SED) = (SUMSED / DLSNEW)
!
!        * Next sediment type
  200    END DO
!
!
!     * Next link
  300 END DO
!
!
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
SUBROUTINE SYBKER (ISTEC, NLF, NS, FPCLAY, RHOSO, DRSO50, TAUK, &
 CWIDTH, DWAT1, BKB, NTSOBK, FETA, CLENTH, DBFULL, EPSB, GNUBK)

INTEGER :: ISTEC                   !! Critical-shear calculation option.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NS                      !! Number of soil types.
INTEGER :: NTSOBK (NLF)            !! Bank soil type by link.
DOUBLEPRECISION FPCLAY (NS)        !! Clay fraction by soil type.
DOUBLEPRECISION RHOSO (NS)         !! Soil bulk density by soil type.
DOUBLEPRECISION DRSO50 (NS)        !! Median soil particle diameter by soil type.
DOUBLEPRECISION TAUK (NLF)         !! Channel/link shear stress.
DOUBLEPRECISION CWIDTH (NLF)       !! Channel width by link.
DOUBLEPRECISION DWAT1 (NLF)        !! Channel water depth by link.
DOUBLEPRECISION BKB (NS)           !! Bank erodibility by soil type.
DOUBLEPRECISION FETA (NLF)         !! Soil-to-sediment solid-volume conversion factor by link.
DOUBLEPRECISION CLENTH (NLF)       !! Channel-link length.
DOUBLEPRECISION DBFULL (NLF)       !! Bankfull depth by link.
DOUBLEPRECISION EPSB (NLF)         !! Bank erosion sediment source by link.
DOUBLEPRECISION GNUBK (NLF)        !! Lateral bank erosion rate by link.
!
! Locals, etc
DOUBLEPRECISION A1, B1, B2, B3, QUART
PARAMETER (A1 = 0.05D0, B1 = 0.41D0, B2 = 0.22D0, B3 = 0.035D0)
PARAMETER (QUART = 1.0d0 / 4.0D0)
!
INTEGER :: BKSOIL, LINK
DOUBLEPRECISION DWAT1E, GNUBKE, K, TAUEC, TAUKE, X
!
!
!----------------------------------------------------------------------*
!
!
!     * Loop over channel links
DO 100 LINK = 1, NLF
   BKSOIL = NTSOBK (LINK)
   DWAT1E = DWAT1 (LINK)
   TAUKE = TAUK (LINK)
!
!        * Calculate aspect ratio coefficient ( see Notes )
   X = ONE / MAX (QUART, DWAT1E / CWIDTH (LINK) )
   K = A1 + B1 * MIN (X, ONE) + B2 * MIN (DIMJE(X, ONE), ONE) &
    + B3 * DIMJE(X, TWO)
!
!        * Obtain critical shear stress for bank erosion
   CALL SYCRIT (ISTEC, DRSO50 (BKSOIL), TAUKE, FPCLAY (BKSOIL), &
    TAUEC)
!
!        * Calculate bank erosion rate
   GNUBKE = BKB (BKSOIL) * DIMJE(K * TAUKE, TAUEC) / (TAUEC * &
    RHOSO (BKSOIL) )
   GNUBK (LINK) = GNUBKE
!
!        * Calculate rate of release of sediments for each link
   EPSB (LINK) = TWO * FETA (LINK) * CLENTH (LINK) * GNUBKE * MIN &
    (DWAT1E, DBFULL (LINK) )
!
!     * Next link
  100 END DO
!
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
SUBROUTINE SYCLTR (CONCOB, FPCRIT, ISACKW, ISUSED, NELEE, NFINE, &
 NLF, NLFEE, NSED, NSEDEE, DRSED, ARXL, CWIDTH, DCBED, LINKNS, &
 DWAT1, QOC, SLOPEJ, DCBSED, FDEL, TAUJ, ACKW, CONCI, QSDWAT, GSED, &
 QSWSUM)

USE CONST_SY

INTEGER :: ISACKW                  !! Channel transport-capacity option.
INTEGER :: ISUSED                  !! Sediment velocity option.
INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NFINE                   !! Number of fine sediment classes.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NSED                    !! Number of sediment size classes.
INTEGER :: NSEDEE                  !! Sediment-size array dimension.
DOUBLEPRECISION CONCOB             !! Mobile concentration threshold for overbank exchange.
DOUBLEPRECISION FPCRIT             !! Maximum sediment concentration fraction.
DOUBLEPRECISION DRSED (NFINE+1:NSED) !! Representative non-fine particle diameters.
DOUBLEPRECISION ARXL (NLF)         !! Channel cross-sectional area by link.
DOUBLEPRECISION CWIDTH (NLF)       !! Channel width by link.
DOUBLEPRECISION DCBED (NLF)        !! Active upper-bed layer depth by link.
DOUBLEPRECISION DWAT1 (NLF)        !! Channel water depth by link.
DOUBLEPRECISION QOC (NELEE, 4)     !! Face water fluxes.
DOUBLEPRECISION SLOPEJ (NELEE, 4)  !! Face water-surface slopes.
DOUBLEPRECISION DCBSED (NLFEE, NFINE+1:NSED) !! Active-bed sediment depth by link and non-fine class.
DOUBLEPRECISION FDEL (NELEE, NFINE+1:NSED)   !! Mobile sediment concentration fraction by element and non-fine class.
DOUBLEPRECISION TAUJ (NELEE, 4)    !! Face shear stress.
LOGICAL :: LINKNS (NLF)            !! True for north-south channel links.
DOUBLEPRECISION ACKW (5, NFINE+1:NSED) !! Cached Ackers-White parameters by non-fine class.
DOUBLEPRECISION CONCI (NLFEE, NSED)    !! Capacity concentration by link and sediment class.
DOUBLEPRECISION QSDWAT (NLFEE, NSEDEE, 4) !! Sediment advection coefficient for outflow faces only.
DOUBLEPRECISION GSED (NLF, NFINE+1:NSED) !! Channel transport capacity workspace.
DOUBLEPRECISION QSWSUM (NLF, NSED)      !! Sum of outflowing sediment advection coefficients by link/class.
!
! Locals, etc
DOUBLEPRECISION ZZ5
PARAMETER (ZZ5 = 0.05D0)
!
INTEGER :: FACE, IEND, ISIDE, LINK, NFP1, NSDWAT, SED, SGN
DOUBLEPRECISION CONCID, DCSUM, DUM, FDSUM, FRACT, KQ, QK

DOUBLEPRECISION TAUEC, TAUD, QSW, FRACT1, FRACT2
LOGICAL :: BODD

!
!----------------------------------------------------------------------*
!
!
! Initialization
! --------------
!
IF (FIRST_sycltr) THEN
   FIRST_sycltr = .FALSE.
   K1_sycltr = 8.5 / SQRT (RHOWAT)
ENDIF
NFP1 = NFINE+1
CALL ALINIT (ZERO, NSED * NLF, QSWSUM)
!
!
! Streamwise capacity discharge rates ...
! ---------------------------------------
!
!     ... using specified method
!
IF (ISACKW.EQ.1.OR.ISACKW.EQ.2) THEN
   CALL SYACKW (NELEE, NLF, NLFEE, NFINE, NSED, ISACKW, LINKNS, &
    DRSED, ARXL, DCBSED, DWAT1, QOC, TAUJ, ACKW, GSED)
ELSE
   CALL SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, &
    QOC, LINKNS, SLOPEJ, GSED)
ENDIF
!
!
! Advection Coefficients (outflow faces only) Part 1 ...
! ------------------------------------------------------
!
!     ... for size groups which move with water velocity
!
NSDWAT = NFINE
IF (ISUSED.EQ.0) NSDWAT = NSED
IF (NSDWAT.GT.0) THEN
!
!        * All faces (both ends and sides)
   DO 320 FACE = 1, 4
      SGN = SIGN (1, 2 - FACE)
      BODD = MOD (FACE, 2) .EQ.1
!
!           * All links (but skip over non-outflow faces)
      DO 310 LINK = 1, NLF
         QK = SGN * QOC (LINK, FACE)
         IF (GTZERO(QK)) THEN
!
!                 * Set QSWSUM increment for link ends only
            QSW = ZERO
            IF (BODD.NEQV.LINKNS (LINK) ) QSW = QK
!
!                 * Fines only, or all size groups, as appropriate
            DO 300 SED = 1, NSDWAT
               QSDWAT (LINK, SED, FACE) = QK
!                    * Don't actually need QSWSUM for fines, but ...
               QSWSUM (LINK, SED) = QSWSUM (LINK, SED) + QSW
  300             END DO
!
         ENDIF
  310       END DO
!
  320    END DO
!
ENDIF
!
!
! Advection Coefficients (outflow faces only)  Part 2 ...
! -------------------------------------------------------
!
!     ... at link ends for each size group which moves with an
!         independent velocity.
!
IF (ISUSED.EQ.1) THEN
!
!        * Loop over both ends ( of every link )
   DO 420 IEND = 1, 3, 2
      SGN = 2 - IEND
!
!           * Loop over every link (but skip over non-outflow faces)
      DO 410 LINK = 1, NLF
         FACE = IEND
         IF (LINKNS (LINK) ) FACE = FACE+1
         QK = SGN * QOC (LINK, FACE)
         IF (GTZERO(QK)) THEN
!
            TAUD = TAUJ (LINK, FACE)
            KQ = K1_sycltr * ARXL (LINK)
!
!                 * Loop over non-fine size groups
            DO 400 SED = NFP1, NSED
               CALL SYCRIT (0, DRSED (SED), TAUD, DUM, TAUEC)
               QSW = MIN (KQ * SQRT (DIMJE(TAUD, SQRT (TAUD * &
                TAUEC) ) ), QK)
               QSDWAT (LINK, SED, FACE) = QSW
               QSWSUM (LINK, SED) = QSWSUM (LINK, SED) + QSW
  400             END DO
!
         ENDIF
!
!           * Next link
  410       END DO
!
!        * Next end
  420    END DO
!
ENDIF
!
!
! Determine notional particle concentrations at flow capacity
! -----------------------------------------------------------
!
!     * Loop over fines
DO 500 SED = 1, NFINE
   CALL ALINIT (FPCRIT, NLF, CONCI (1, SED) )
  500 END DO
!
!     * Loop over links
DO 530 LINK = 1, NLF
!
!        * Determine denominators for scaling factors
   FDSUM = ZERO
   DO 510 SED = NFP1, NSED
      FDSUM = FDSUM + FDEL (LINK, SED)
  510    END DO
   IF (ISZERO(FDSUM)) FDSUM = ONE
   DCSUM = DCBED (LINK)
   IF (ISZERO(DCSUM)) DCSUM = ONE
!
!        * Loop over non-fines
   DO 520 SED = NFP1, NSED
      QSW = QSWSUM (LINK, SED)
      IF (GTZERO(QSW)) THEN
         FRACT1 = FDEL (LINK, SED) / FDSUM
         FRACT2 = DCBSED (LINK, SED) / DCSUM
         FRACT = MAX (ZZ5, FRACT1, FRACT2)
         CONCID = MIN (FPCRIT, FRACT * GSED (LINK, SED) / QSW)
      ELSE
         CONCID = ZERO
      ENDIF
      CONCI (LINK, SED) = CONCID
  520    END DO
!
  530 END DO
!
!
! Advection Coefficients (outflow faces only) Part 3 ...
! ------------------------------------------------------
!
!     ... at link sides, for each size group which moves at an
!         independent velocity.
!
IF (ISUSED.EQ.1) THEN
!
!        * Loop over both sides
   DO 620 ISIDE = 2, 4, 2
      SGN = 3 - ISIDE
!
!           * Loop over every link (but skip over non-outflow sides)
      DO 610 LINK = 1, NLF
         FACE = ISIDE
         IF (LINKNS (LINK) ) FACE = ISIDE-1
         QK = SGN * QOC (LINK, FACE)
         IF (GTZERO(QK)) THEN
!
!                 * Loop over non-fine size groups
            DO 600 SED = NFP1, NSED
               DUM = CONCI (LINK, SED)
               IF (GTZERO(DUM)) DUM = QK * DIMJE(DUM, CONCOB) &
                / DUM
               QSDWAT (LINK, SED, FACE) = DUM
  600             END DO
!
         ENDIF
!
!           * Next link
  610       END DO
!
!        * Next side
  620    END DO
!
ENDIF
!
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
SUBROUTINE SYCOLM (AREAE, DTSY, DWAT1E, DWATOE, DXQQE, DYQQE, &
 FETAE, GNUE, ISGSED, NSED, FPCRIT, PLSE, NSEDEE, DRSED, QWAT, &
 SLOPEE, SOSDFE, TAUJE, DLSE, FBETAE, FDELE, QSEDE, Q, VDSED)

INTEGER :: ISGSED                  !! Overland transport-capacity option.
INTEGER :: NSED                    !! Number of sediment size classes.
INTEGER :: NSEDEE                  !! Sediment-size array dimension.
DOUBLEPRECISION AREAE              !! Element plan area.
DOUBLEPRECISION DTSY               !! Sediment substep duration.
DOUBLEPRECISION DWAT1E             !! Current surface water depth.
DOUBLEPRECISION DWATOE             !! Previous surface water depth.
DOUBLEPRECISION DXQQE              !! Element width.
DOUBLEPRECISION DYQQE              !! Element length.
DOUBLEPRECISION FETAE              !! Soil-to-sediment solid-volume conversion factor.
DOUBLEPRECISION GNUE               !! Hillslope erosion rate.
DOUBLEPRECISION FPCRIT             !! Maximum sediment concentration fraction.
DOUBLEPRECISION PLSE               !! Loose-sediment porosity.
DOUBLEPRECISION DRSED (NSED)       !! Representative particle diameters by size class.
DOUBLEPRECISION QWAT (4)           !! Outward water flux by face.
DOUBLEPRECISION SLOPEE (4)         !! Water-surface slope by face.
DOUBLEPRECISION SOSDFE (NSED)      !! Source soil sediment-size fraction.
DOUBLEPRECISION TAUJE (4)          !! Face shear stress.
DOUBLEPRECISION DLSE               !! Loose-sediment depth in the land element.
DOUBLEPRECISION FBETAE (NSED)      !! Loose-sediment composition by size class.
DOUBLEPRECISION FDELE (NSED)       !! Mobile sediment concentration fraction by size class.
DOUBLEPRECISION QSEDE (NSEDEE, 4)  !! Sediment flux by size class and face.
DOUBLEPRECISION Q (NSED)           !! Workspace for outgoing sediment flux by size class.
DOUBLEPRECISION VDSED (NSED)       !! Workspace for available sediment volume by size class.
!
! Locals, etc
!
INTEGER :: FACE, J (4), JLC, NOUT, SED
DOUBLEPRECISION A1, A2, A3, B1, B2, DBETA, DDLS, FD, FLS, G
DOUBLEPRECISION GJSUM, GSUM, QK, QWSUM, VD, VDSUM, VDWAT
!
!
!----------------------------------------------------------------------*
!
!
! Initialization
! --------------
!
QWSUM = ZERO
VDSUM = ZERO
FLS = ONE-PLSE
CALL ALINIT (ZERO, NSED, Q)
!
!
! Water & Sediment Budgets
! ------------------------
!
!     * Calculate water discharge & particulate supply rates
!     *  ( both non-negative ), and make a list of outflow faces
NOUT = 0
DO 200 FACE = 1, 4
   QK = QWAT (FACE)
   IF (QK.GT.ZERO) THEN
!           * Outflow face
      QWSUM = QWSUM + QK
      NOUT = NOUT + 1
      J (NOUT) = FACE
   ELSE
!           * Inflow or no-flow face
      DO 100 SED = 1, NSED
         Q (SED) = Q (SED) - QSEDE (SED, FACE) / FLS
  100       END DO
   ENDIF
  200 END DO
!
!     * Calculate volume of water + volume of discharged water
VDWAT = DWAT1E * AREAE+QWSUM * DTSY
!
!     * Calculate volume of stored sediment plus volume of
!     *  discharged sediment for each fraction ( must be non-negative )
DDLS = FETAE * GNUE * DTSY
DO 300 SED = 1, NSED
   DBETA = DLSE * FBETAE (SED) + DDLS * SOSDFE (SED)
   VD = (FDELE (SED) * DWATOE+DBETA) * AREAE+Q (SED) * DTSY
   VDSUM = VDSUM + VD
   VDSED (SED) = VD
  300 END DO
!
!
! Sediment Discharge
! ------------------
!
!     Note: The only outputs from this section are the coefficients
!           A1 and B1 required by the next section.
!
!     * Discharge rate based upon SUPPLY, assuming unlimited capacity
GSUM = ZERO
IF (GTZERO(VDWAT)) GSUM = FLS * VDSUM * (QWSUM / VDWAT)
!
!     * Is discharge possible?
IF (GTZERO(GSUM)) THEN
!
!        * Yes ( implies VDSUM > 0 )
!
!        * Discharge rate based upon flow CAPACITY ...
   CALL SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, DRSED, &
    QWAT, SLOPEE, TAUJE, GJSUM)
!
!        ... with additional upper limit based on total suspended load
   G = MIN (GJSUM, QWSUM * FPCRIT)
!
!        * Transport is governed by the lower of the two rates
!          (take MIN before dividing, in case G>>GSUM)
   A1 = MIN (G, GSUM) / GSUM
   B1 = VDWAT
!
ELSE
!
!        * Either no sediment available, or no water to carry it
!
!        * Zero discharge case ( any sediment is deposited )
   A1 = ZERO
   B1 = ONE
!
ENDIF
!
!
! Define Output Variables
! -----------------------
!
!     * Update depth of loose sediments
DLSE = (ONE-A1) * VDSUM / AREAE
!
!     * Evaluate coefficients for FBETAE
IF (GTZERO(DLSE)) THEN
!        * Composition of loose sediment is given by VDSED
   A2 = ONE
   B2 = VDSUM
   A3 = ZERO
ELSE
!        * No loose sediment left: adopt composition of surface soil
   A2 = ZERO
   B2 = ONE
   A3 = ONE
ENDIF
!
!     * Update compositions of suspended and loose sediments, and set
!     *  sediment flow rates for each outflow face.
!     *  ( don't pre-invert B1 or B2: they may be small! )
DO 500 SED = 1, NSED
   VD = VDSED (SED)
   FD = (A1 * VD) / B1
   FDELE (SED) = FD
   FBETAE (SED) = A2 * VD / B2 + A3 * SOSDFE (SED)
   DO 400 JLC = 1, NOUT
      FACE = J (JLC)
      QSEDE (SED, FACE) = FLS * QWAT (FACE) * FD
  400    END DO
  500 END DO
!
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
SUBROUTINE SYCRIT (FLAG, DRX50, TAUX, FPCLAE, TAUEC)

! Commons and distributed constants
USE CONST_SY

INTEGER :: FLAG                    !! Critical-shear option: 1 clay relation, otherwise Shields relation.
DOUBLEPRECISION DRX50              !! Median particle diameter.
DOUBLEPRECISION TAUX               !! Current shear stress used to select Shields class.
DOUBLEPRECISION FPCLAE             !! Clay fraction for the clay-content relation.
DOUBLEPRECISION TAUEC              !! Returned critical shear stress.
!
! Locals, etc
DOUBLEPRECISION R0, R1, R2, R3, R4, R5
PARAMETER (R0 = 3D-2, R1 = 1D0)
PARAMETER (R2 = 6D0, R3 = 30D0, R4 = 135D0, R5 = 4D2)
!
INTEGER :: IS
DOUBLEPRECISION AEC (5), BEC (5), RSTR, R, SF
!
!     * Define constants for use in calculating TAUEC.
DATA AEC / 0.1d0, 0.1d0, 0.033d0, 0.013d0, 0.03d0 /
DATA BEC / - 0.3d0, - 0.62d0, 0.0d0, 0.28d0, 0.1d0 /
!
!     * Note, Classes for RSTR :-
!     *    Class i applies to R(i-1) < RSTR <= Ri
!     *    Class 1 ALSO includes RSTR = R0
!     *    RSTR is truncated to lie in the range [R0,R5]
!     *
!
!     * Define switch function, used to determine class for AEC and BEC.
SF (RSTR, R) = HALF - SIGN (HALF, R - RSTR)
!
!----------------------------------------------------------------------*
!
!     * Calculate constants during first call to this routine
IF (FIRST_sycrit) THEN
   K1_sycrit = 1.0 / (SQRT (RHOWAT) * VISCOS)
   K2_sycrit = (RHOSED-RHOWAT) * GRAVTY
   K3_sycrit = 1.83d0 * LOG (10.0d0)
   FIRST_sycrit = .FALSE.
ENDIF
!
!     * Choose method of calculating TAUEC
IF (FLAG.EQ.1) THEN
!
!
!        * Quick method
!
   TAUEC = 0.493d0 * EXP (K3_sycrit * FPCLAE)
!
ELSE
!
!
!        * Shields method
!
!        * Calculate Particle Reynolds Number
   RSTR = MAX (R0, MIN (DRX50 * SQRT (TAUX) * K1_sycrit, R5) )
!
!        * Select coefficient pair for calculating TAUEC
   IS = NINT (ONE+SF (RSTR, R1) + SF (RSTR, R2) + SF (RSTR, R3) &
    + SF (RSTR, R4) )
!
!        * Calculate Critical Shear Stress
   TAUEC = AEC (IS) * K2_sycrit * DRX50 * RSTR**BEC (IS)
!
ENDIF
!
!
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
DOUBLEPRECISION FUNCTION SYDR (FSED, INCF, N, F, D)
!
INTEGER :: INCF                    !! Stride between distribution weights in `F`.
INTEGER :: N                       !! Number of sediment size classes.
DOUBLEPRECISION FSED               !! Target cumulative fraction.
DOUBLEPRECISION F (1 + (N - 1) * INCF) !! Sediment distribution weights.
DOUBLEPRECISION D (N)              !! Representative particle diameters.

!
! Locals, etc
DOUBLEPRECISION ALMOST
PARAMETER (ALMOST = 0.9999D0)
!
DOUBLEPRECISION DR, DRHI, DRLO, F02, FLO, FHI, FSUM2, FTOT
INTEGER :: FRPTR, SED
!
!----------------------------------------------------------------------*
!
!     * Initialize local variables
FHI = 0
DRHI = 0
FSUM2 = 0
FTOT = 0
FRPTR = 1
!
!     * Double the selected 'percentile' (actually a fraction 0-1)
!     *  and scale it relative to the sum of distribution ratios
DO 100 SED = 1, N
   FTOT = FTOT + F (FRPTR)
   FRPTR = FRPTR + INCF
  100 END DO
F02 = 2 * FSED * FTOT
!
IF (ISZERO(F02)) THEN
!
!        * Zeroth percentile or null distribution
   DR = 0
!
ELSE
!
!        * Reset fraction pointer
   FRPTR = 1
!
!        * Loop over sediment types until target percentile surpassed
   DO 200 SED = 1, N
!
!           * Calculate midpoint of cumulative fraction (doubled)
      FLO = FHI
      DRLO = DRHI
      FHI = F (FRPTR)
      DRHI = D (SED)
      FSUM2 = FSUM2 + FLO + FHI
!
!           * Break out of loop if target percentile has been reached
!           *  ( allowing for rounding error )
      IF (FSUM2.GE.F02 * ALMOST) GOTO 300
!                                          ^^^^^^^^
!
!           * Increment fraction pointer
      FRPTR = FRPTR + INCF
!
  200    END DO
!
!        * Interpolate between last two Fraction/Diameter pairs to find
!        *  target percentile.
!        *  Note :- Combination of precondition FSED<1 and use of ALMOST
!        *          should ensure (FLO+FHI) > 0
  300    DR = DRHI - (DRHI - DRLO) * (FSUM2 - F02) / (FLO + FHI)
!
ENDIF
!
SYDR = DR
!
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
SUBROUTINE SYENGH (NFINE, NLF, NSED, NELEE, DRSED, CWIDTH, DWAT1, &
 QOC, LINKNS, SLOPEJ, GSED)

USE CONST_SY

! Commons and distributed constants

INTEGER :: NFINE                   !! Number of fine sediment classes excluded from this calculation.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NSED                    !! Number of sediment size classes.
INTEGER :: NELEE                   !! Element-array dimension.
DOUBLEPRECISION DRSED (NFINE+1:NSED) !! Representative non-fine particle diameters.
DOUBLEPRECISION CWIDTH (NLF)       !! Channel width by link.
DOUBLEPRECISION DWAT1 (NLF)        !! Channel water depth by link.
DOUBLEPRECISION QOC (NELEE, 4)     !! Face water fluxes.
DOUBLEPRECISION SLOPEJ (NELEE, 4)  !! Face water-surface slopes.
LOGICAL :: LINKNS (NLF)            !! True for north-south channel links.
DOUBLEPRECISION GSED (NLF, NFINE+1:NSED) !! Channel transport capacity by link and non-fine class.
!
! Locals, etc
INTEGER :: FACE, IEND, LINK, NFP1, SED, SGN
DOUBLEPRECISION DWAT1E, GD, QK
!
!----------------------------------------------------------------------*
!
!
!     * Initialization
IF (FIRST_syengh) THEN
   FIRST_syengh = .FALSE.
   KG_syengh = 0.05d0 / (SQRT (GRAVTY) * (RHOSED / RHOWAT - 1) **2)
ENDIF
NFP1 = NFINE+1
CALL ALINIT (ZERO, NLF * (NSED-NFINE), GSED)
!
!     * Loop over ends of link
DO 300 IEND = 1, 3, 2
   SGN = 2 - IEND
!
!        * Loop over links
   DO 200 LINK = 1, NLF
!
!           * Determine current face number, outflow rate & water depth
      FACE = IEND
      IF (LINKNS (LINK) ) FACE = FACE+1
      QK = SGN * QOC (LINK, FACE)
      DWAT1E = DWAT1 (LINK)
!
!           * Increment capacity rate for non-dry outflow ends only
      IF (GTZERO(DWAT1E).AND.GTZERO(QK)) THEN
!
!              * Loop invariant

         GD = QK**2 * SLOPEJ (LINK, FACE) **1.5 * KG_syengh / (CWIDTH ( &
          LINK) * SQRT (DWAT1E) )
!              * All sediment types
         DO 100 SED = NFP1, NSED
            GSED (LINK, SED) = GD / DRSED (SED) + GSED (LINK, SED)
  100          END DO
!
      ENDIF
!
!        * Next link
  200    END DO
!
!     * Next iend
  300 END DO
!
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
SUBROUTINE SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, &
 NSEE, NV, NVEE, NX, NXEE, NY, SPR, SYD)

INTEGER :: NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE
INTEGER :: NV, NVEE, NX, NXEE, NY, SPR, SYD
!
! Locals, etc
INTEGER :: FATAL, ERR
PARAMETER (FATAL = 1, ERR = 2)
!
INTEGER :: IUNDEF, NERR, jedumdum
INTEGER :: IDUMS (1), IDUMO (1)
LOGICAL :: LDUM1 (1)
!
!
!----------------------------------------------------------------------*
!
! 0. Preliminaries
! ----------------
!
!     * Initialize local counter
NERR = 0
!
!
! 1. Array Sizes
! --------------
!
!NELEE
IDUMS (1) = NELEE
IDUMO (1) = MAX (NEL, NV, NX * NY)
CALL ALCHKI (ERR, 2054, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
!NLFEE
IDUMS (1) = NLFEE
IDUMO (1) = MAX (1, NLF)
CALL ALCHKI (ERR, 2055, SPR, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
!NLYREE, NSEDEE
IDUMS (1) = MIN (NLYREE, NSEDEE)
CALL ALCHKI (ERR, 2056, SPR, 1, 1, IUNDEF, IUNDEF, '[ NLYREE, NSEDEE ]', 'GT', IZERO1, IDUMS, NERR, LDUM1)
!NSEE
IDUMS (1) = NSEE
IDUMO (1) = NS
CALL ALCHKI (ERR, 2057, SPR, 1, 1, IUNDEF, IUNDEF, 'NSEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
!NVEE
IDUMS (1) = NVEE
IDUMO (1) = NV
CALL ALCHKI (ERR, 2058, SPR, 1, 1, IUNDEF, IUNDEF, 'NVEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
!NXEE
IDUMS (1) = NXEE
IDUMO (1) = NX
CALL ALCHKI (ERR, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', &
 IDUMO, IDUMS, NERR, LDUM1)
IDUMO (1) = 9999
CALL ALCHKI (ERR, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'LE', &
 IDUMO, IDUMS, NERR, LDUM1)
!
!
! 2. Unit Numbers
! ---------------
!
!SPR, SYD
IDUMS (1) = MIN (SPR, SYD)
CALL ALCHKI (ERR, 2060, SPR, 1, 1, IUNDEF, IUNDEF, '[ SPR, SYD ]', &
 'GE', IZERO1, IDUMS, NERR, LDUM1)
!
!
! 3. Number of Entities
! ---------------------
!
!NLF
IDUMS (1) = NLF
IDUMO (1) = NEL
CALL ALCHKI (ERR, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', &
 IZERO1, IDUMS, NERR, LDUM1)
CALL ALCHKI (ERR, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', &
 IDUMO, IDUMS, NERR, LDUM1)
!NS, NV, NX, NY
jedumdum = MIN (NS, NV)
!""AD IDUMS (1) = MIN (NS, NV, NX, NY)
IDUMS (1) = MIN (jedumdum, NX, NY)
CALL ALCHKI (ERR, 2062, SPR, 1, 1, IUNDEF, IUNDEF, '[ NS, NV, NX, NY ]', 'GT', IZERO1, IDUMS, NERR, LDUM1)
!
!
! 4. Epilogue
! -----------
!
IF (NERR.GT.0) CALL ERROR (FATAL, 2000, SPR, 0, 0, 'Error(s) detected while checking WAT-SY interface variables')
!
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
SUBROUTINE SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, &
 NXEE, NYEE, NY, SPR, BEXBK, LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, &
 NTSOIL, NVC, THSAT, CLENTH, CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, &
 ARXL, HRF, ZGRUND, IDUM, IDUM1X, LDUM)

INTEGER :: NEL                     !! Number of elements.
INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NLYREE                  !! Soil-layer array dimension.
INTEGER :: NS                      !! Number of soil types.
INTEGER :: NV                      !! Number of vegetation types.
INTEGER :: NX                      !! Number of grid columns.
INTEGER :: NXEE                    !! Grid-column array dimension.
INTEGER :: NYEE                    !! Grid-row workspace dimension.
INTEGER :: NY                      !! Number of grid rows.
INTEGER :: SPR                     !! Sediment diagnostic output unit.
INTEGER :: ICMBK (NLFEE, 2)        !! Bank-element numbers for each channel link.
INTEGER :: ICMXY (NXEE, NY)        !! Element number at each grid location.
INTEGER :: ICMREF (NELEE, 4, 2:3)  !! Face-neighbour and reverse-face reference map.
INTEGER :: ICMRF2 (NLFEE, 3, 2)    !! Confluence branch reference map.
INTEGER :: NLYR (NLF + 1:NEL)      !! Number of soil layers in each land element.
INTEGER :: NTSOIL (NEL, NLYREE)    !! Soil type index for each element layer.
INTEGER :: NVC (NLF + 1:NEL)       !! Vegetation type by land element.
DOUBLEPRECISION THSAT (NS)         !! Saturated water content by soil type.
DOUBLEPRECISION CLENTH (NLFEE)     !! Channel-link length.
DOUBLEPRECISION CWIDTH (NLFEE)     !! Channel width by link.
DOUBLEPRECISION ZBFULL (NLFEE)     !! Bankfull elevation/depth by link.
DOUBLEPRECISION DXQQ (NLF + 1:NEL) !! Land-element width.
DOUBLEPRECISION DYQQ (NLF + 1:NEL) !! Land-element length.
DOUBLEPRECISION AREA (NEL)         !! Element plan area.
DOUBLEPRECISION DHF (NELEE, 4)     !! Face-to-face hydraulic distance.
DOUBLEPRECISION ARXL (NLFEE)       !! Channel cross-sectional area by link.
DOUBLEPRECISION HRF (NLF + 1:NEL)  !! Land-element water level/head.
DOUBLEPRECISION ZGRUND (NEL)       !! Ground or bed elevation by element.
LOGICAL :: BEXBK                   !! True when bank elements are represented.
LOGICAL :: LINKNS (NLFEE)          !! True for north-south channel links.
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM !! Integer workspace for identity checks.
INTEGER :: IDUM1X ( - 1:NEL + 1)   !! Integer workspace for element identity checks.
LOGICAL :: LDUM (NELEE)            !! Logical workspace for element checks.
!
! Locals, etc
INTEGER :: FATAL, ERR
PARAMETER (FATAL = 1, ERR = 2)
!
INTEGER :: BANK, COUNT, FACE, FADJ, FEL
INTEGER :: IADJ, IBR, IBRADJ, ICOL1, IEL, IELP, ILYR, IUNDEF, IX, &
 IY
INTEGER :: LINK, NCOL, NELP, NERR, P, PADJ
INTEGER :: IDUM1 (2)
LOGICAL :: BKXYOK, REFOK
!
!----------------------------------------------------------------------*
!
! 0. Preliminaries
! ----------------
!
!     * local counter
NERR = 0
!     * position of 1st column element
ICOL1 = NLF + 1
!     * number of elements plus one
NELP = NEL + 1
!
!
! 1. Index Arrays
! ---------------
!
!ICMBK, ICMXY
COUNT = NERR
!     * initialize column-element counter & marker array
NCOL = 0
DO 110 IEL = 0, NLF
   IDUM1X (IEL) = 1
  110 END DO
DO 115 IEL = ICOL1, NELP
   IDUM1X (IEL) = 0
  115 END DO
!     * count active grid elements and mark them
DO 125 IY = 1, NY
   DO 120 IX = 1, NX
      IEL = MAX (0, MIN (ICMXY (IX, IY), NELP) )
      IDUM1X (IEL) = IDUM1X (IEL) + 1
      NCOL = NCOL + MIN (IEL, 1)
  120    END DO
  125 END DO
!     * similarly for bank elements (if present all must be active)
IF (BEXBK.AND.NLF.GT.0) THEN
   NCOL = NCOL + 2 * NLF
   DO 135 BANK = 1, 2
      DO 130 LINK = 1, NLF
         IEL = MAX (0, MIN (ICMBK (LINK, BANK), NELP) )
         IDUM1X (IEL) = IDUM1X (IEL) + 1
  130       END DO
  135    END DO
ENDIF
!     * watch out for gate-crashers
IDUM1 (1) = NEL - NLF
IDUM1X (0) = NCOL
CALL ALCHKI (ERR, 2075, SPR, 1, 1, IUNDEF, IUNDEF, '#_column_elements', 'EQ', IDUM1, IDUM1X (0) , NERR, LDUM)
!     * check that each element has a unique identity
CALL ALCHKI (ERR, 2076, SPR, 1, NEL, IUNDEF, IUNDEF, &
 'element_count(iel)', 'EQ', IONE1, IDUM1X (1) , NERR, LDUM)
!     * was everything ok?
BKXYOK = COUNT.EQ.NERR
!
!ICMREF part 1
IDUM1 (1) = NEL
IDUM1 (2) = - NLFEE
REFOK = .TRUE.
DO 145 FACE = 1, 4
   COUNT = NERR
!        * check that all neighbours are within range
CALL ALCHKI (ERR, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)' &
&, 'LE', IDUM1 (1) , ICMREF (1, FACE, 2) , NERR, LDUM)
CALL ALCHKI (ERR, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)' &
&, 'GE', IDUM1 (2) , ICMREF (1, FACE, 2) , NERR, LDUM)
!        * check regular faces for range and consistency
   IF (COUNT.EQ.NERR) THEN
      DO 140 IEL = 1, NEL
         IADJ = ICMREF (IEL, FACE, 2)
         IF (IADJ.LE.0) THEN
!                 * not a regular face
            IDUM (IEL) = 0
         ELSE
            FADJ = ICMREF (IEL, FACE, 3)
            IF (FADJ.LT.1.OR.FADJ.GT.4) THEN
!                    * bad face value
               IDUM (IEL) = 1
            ELSE
               IF (ICMREF (IADJ, FADJ, 2) .NE.IEL) THEN
!                       * bad reflection
                  IDUM (IEL) = 2
               ELSE
                  IDUM (IEL) = 0
!                       * faces don't match?
                  IF (ICMREF (IADJ, FADJ, 3) .NE.FACE) IDUM (IEL) &
                   = 3
               ENDIF
            ENDIF
         ENDIF
  140       END DO
      CALL ALCHKI (ERR, 2078, SPR, 1, NEL, FACE, IUNDEF, &
       'status_of_ICMREF(iel,face)', 'EQ', IZERO1, IDUM, NERR, LDUM)
   ENDIF
!        * is everything still ok?
   REFOK = REFOK.AND.COUNT.EQ.NERR
  145 END DO
!
!ICMREF part 2 (bank element neighbours)
IF (NLF.GT.0.AND.BEXBK.AND.BKXYOK.AND.REFOK) THEN
!        * set marker array (disallow non-grids other than zero)
   IDUM1X ( - 1) = - 2
   IDUM1X (0) = 0
   DO 150 IEL = 1, NEL
      IDUM1X (IEL) = - 2
  150    END DO
   DO 165 IY = 1, NY
      DO 160 IX = 1, NX
         IEL = MAX (0, ICMXY (IX, IY) )
         IDUM1X (IEL) = MIN (IEL, 1)
  160       END DO
  165    END DO
!        * count number of grid neighours for each link
   DO 170 LINK = 1, NLF
      IDUM (LINK) = 0
  170    END DO
   DO 185 BANK = 1, 2
      DO 180 LINK = 1, NLF
         IEL = ICMBK (LINK, BANK)
         FACE = 2 * BANK
         IF (LINKNS (LINK) ) FACE = FACE-1
         IADJ = MAX ( - 1, ICMREF (IEL, FACE, 2) )
         IDUM (LINK) = IDUM (LINK) + IDUM1X (IADJ)
  180       END DO
  185    END DO
CALL ALCHKI (ERR, 2079, SPR, 1, NLF, IUNDEF, IUNDEF, '#_grids_neighbouring_banks(link)', 'GT', IZERO1, IDUM, NERR, LDUM)
ENDIF
!
!ICMRF2
IF (REFOK) THEN
!        * initialize status array
   DO 190 IBR = 1, NLFEE
      IDUM (IBR) = - 1
  190    END DO
!        * check each prospect of each branch
   DO 198 FACE = 1, 4
      DO 196 IEL = 1, NEL
         IADJ = ICMREF (IEL, FACE, 2)
         IF (IADJ.LT.0) THEN
            IBR = - IADJ
            IF (IDUM (IBR) .GE.0) THEN
!                    * duplicate reference
               IDUM (IBR) = IDUM (IBR) + 1
            ELSE
!                    * initialize status
               IDUM (IBR) = 0
               DO 194 P = 1, 3
                  IADJ = ICMRF2 (IBR, P, 1)
                  IF (IADJ.GT.NEL) THEN
!                          * neighbour out of range
                     IDUM (IBR) = IDUM (IBR) + P * 10
                  ELSEIF (IADJ.GT.0) THEN
                     FADJ = ICMRF2 (IBR, P, 2)
                     IF (FADJ.LT.1.OR.FADJ.GT.4) THEN
!                             * bad face value
                        IDUM (IBR) = IDUM (IBR) + P * 100
                     ELSE
                        IBRADJ = - ICMREF (IADJ, FADJ, 2)
                        IF (IBRADJ.LT.1.OR.IBRADJ.GT.NLFEE) THEN
!                                * bad mirror branch
                           IDUM (IBR) = IDUM (IBR) + P * 1000
                        ELSE
                           DO 192 PADJ = 1, 3
                              IELP = ICMRF2 (IBRADJ, PADJ, 1)
                              IF (IELP.EQ.IEL) THEN
                              FEL = ICMRF2 (IBRADJ, PADJ, 2)
                              IF (FEL.EQ.FACE) GOTO 193
                              ENDIF
  192                            END DO
!                                * can't find a reference in the mirror
                           IDUM (IBR) = IDUM (IBR) + P * 10000
  193                            CONTINUE
                        ENDIF
                     ENDIF
                  ENDIF
  194                END DO
            ENDIF
         ENDIF
  196       END DO
  198    END DO
   CALL ALCHKI (ERR, 2080, SPR, 1, NLFEE, IUNDEF, IUNDEF, &
    'status_of_ICMRF2(branch)', 'LE', IZERO1, IDUM, NERR, LDUM)
ENDIF
!
!
! 2. Soil Properties
! ------------------
!
!THSAT
CALL ALCHK (ERR, 2063, SPR, 1, NS, IUNDEF, IUNDEF, 'THSAT(soil)', &
 'LE', ONE1, ZERO1 (1) , THSAT, NERR, LDUM)
!
!
! 3. Link Properties & Initial State
! ----------------------------------
!
IF (NLF.GT.0) THEN
!
!CLENTH
CALL ALCHK (ERR, 2064, SPR, 1, NLF, IUNDEF, IUNDEF, 'CLENTH(link)' &
&, 'GE', ZERO1, ZERO1 (1) , CLENTH, NERR, LDUM)
!CWIDTH
CALL ALCHK (ERR, 2065, SPR, 1, NLF, IUNDEF, IUNDEF, 'CWIDTH(link)' &
&, 'GT', ZERO1, ZERO1 (1) , CWIDTH, NERR, LDUM)
!ZBFULL
CALL ALCHK (ERR, 2066, SPR, 1, NLF, IUNDEF, IUNDEF, 'ZBFULL(link)' &
&, 'GEa', ZGRUND, ZERO1 (1) , ZBFULL, NERR, LDUM)
!ARXL
CALL ALCHK (ERR, 2067, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
&'GE', ZERO1, ZERO1 (1) , ARXL, NERR, LDUM)
!
ENDIF
!
!
! 4. Column Properties & Initial State
! ------------------------------------
!
!DXQQ
CALL ALCHK (ERR, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DXQQ(iel)', 'GT', ZERO1, ZERO1 (1) , DXQQ, NERR, LDUM)
!DYQQ
CALL ALCHK (ERR, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DYQQ(iel)', 'GT', ZERO1, ZERO1 (1) , DYQQ, NERR, LDUM)
!HRF
CALL ALCHK (ERR, 2069, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'HRF(iel)' , 'GEa', ZGRUND (ICOL1) , ZERO1 (1) , HRF, NERR, LDUM)
!NLYR
COUNT = NERR
IDUM1 (1) = NLYREE
CALL ALCHKI (ERR, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'GT', IZERO1, NLYR, NERR, LDUM)
CALL ALCHKI (ERR, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'LE', IDUM1, NLYR, NERR, LDUM)
!NTSOIL
IF (COUNT.EQ.NERR) THEN
   DO 410 IEL = ICOL1, NEL
      ILYR = NLYR (IEL)
      IDUM (IEL) = NTSOIL (IEL, ILYR)
  410    END DO
   IDUM1 (1) = NS
   CALL ALCHKI (ERR, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, &
    'NTSOIL[iel,NLYR(iel)]', 'GT', IZERO1, IDUM (ICOL1) , NERR, &
    LDUM)
   CALL ALCHKI (ERR, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, &
    'NTSOIL[iel,NLYR(iel)]', 'LE', IDUM1, IDUM (ICOL1) , NERR, &
    LDUM)
ENDIF
!NVC
COUNT = NERR
IDUM1 (1) = NV
CALL ALCHKI (ERR, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'GT', IZERO1, NVC, NERR, LDUM)
CALL ALCHKI (ERR, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'LE', IDUM1, NVC, NERR, LDUM)
!
!
! 5. Element Properties
! ---------------------
!
!AREA
CALL ALCHK (ERR, 2073, SPR, 1, NEL, IUNDEF, IUNDEF, 'AREA(iel)', &
 'GT', ZERO1, ZERO1 (1) , AREA, NERR, LDUM)
!DHF
DO 510 FACE = 1, 4
CALL ALCHK (ERR, 2074, SPR, 1, NEL, FACE, IUNDEF, 'DHF(iel,face)', &
& 'GT', ZERO1, ZERO1 (1) , DHF (1, FACE) , NERR, LDUM)
  510 END DO
!
!
! 6. Epilogue
! -----------
!
IF (NERR.GT.0) CALL ERROR (FATAL, 2001, SPR, 0, 0, 'Error(s) detected while checking static/initial WAT-SY interface')
!
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
SUBROUTINE SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, &
 NV, NSYB, NSYBEE, NSYC, NSYCEE, SPR, ICMREF, ISUSED, NEPS, NFINE, &
 SFB, SRB, ALPHA, DCBEDO, FPCRIT, DLSMAX, NTSOBK, NSYBCD, NBFACE, &
 DRSED, BKB, GKF, GKR, RHOSO, SOSDFN, DRDRIP, FDRIP, XDRIP, PBSED, &
 FCG, FCROCK, PLS, DLS, FBETA, FDEL, ABC, BBC, GBC, IDUM, DUMMY, &
 LDUM)

INTEGER :: NXEE                    !! Grid-column array dimension.
INTEGER :: NYEE                    !! Grid-row workspace dimension.
INTEGER :: NEL                     !! Number of elements.
INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NS                      !! Number of soil types.
INTEGER :: NSEE                    !! Soil-type array dimension.
INTEGER :: NSED                    !! Number of sediment size classes.
INTEGER :: NSEDEE                  !! Sediment-size array dimension.
INTEGER :: NV                      !! Number of vegetation types.
INTEGER :: NSYB                    !! Number of sediment boundary entries.
INTEGER :: NSYBEE                  !! Sediment-boundary array dimension.
INTEGER :: NSYC (4)                !! Number of sediment boundary categories by boundary type.
INTEGER :: NSYCEE                  !! Sediment-boundary-category array dimension.
INTEGER :: SPR                     !! Sediment diagnostic output unit.
INTEGER :: ICMREF (NELEE, 4, 2:2)  !! Face-neighbour reference map.
INTEGER :: ISUSED                  !! Sediment velocity option.
INTEGER :: NEPS                    !! Number of sediment substeps per water timestep.
INTEGER :: NFINE                   !! Number of fine sediment classes.
INTEGER :: SFB                     !! Sediment boundary file unit.
INTEGER :: SRB                     !! Sediment rating-boundary file unit.
INTEGER :: NTSOBK (NLFEE)          !! Bank soil type by link.
INTEGER :: NSYBCD (NSYBEE, 3)      !! Sediment boundary element, type, and category metadata.
INTEGER :: NBFACE (NEL)            !! Number of boundary faces by element.
DOUBLEPRECISION ALPHA              !! Fine-sediment settling/resuspension critical-shear ratio.
DOUBLEPRECISION DCBEDO             !! Active upper channel-bed layer thickness.
DOUBLEPRECISION FPCRIT             !! Maximum sediment concentration fraction.
DOUBLEPRECISION DRSED (NSED)       !! Representative sediment particle diameters.
DOUBLEPRECISION BKB (NS)           !! Bank erodibility by soil type.
DOUBLEPRECISION GKF (NS)           !! Flow detachment coefficient by soil type.
DOUBLEPRECISION GKR (NS)           !! Rainfall detachment coefficient by soil type.
DOUBLEPRECISION RHOSO (NS)         !! Soil bulk density by soil type.
DOUBLEPRECISION SOSDFN (NSEE, NSED) !! Soil sediment-size fractions by soil type.
DOUBLEPRECISION DRDRIP (NV)        !! Canopy drip drop diameter by vegetation type.
DOUBLEPRECISION FDRIP (NV)         !! Canopy drip fraction by vegetation type.
DOUBLEPRECISION XDRIP (NV)         !! Canopy drip fall height by vegetation type.
DOUBLEPRECISION PBSED (NLFEE)      !! Channel-bed sediment porosity by link.
DOUBLEPRECISION FCG (NLF + 1:NEL)  !! Ground-cover fraction by land element.
DOUBLEPRECISION FCROCK (NLF + 1:NEL) !! Rock-cover fraction by land element.
DOUBLEPRECISION PLS (NLF + 1:NEL)  !! Loose-sediment porosity by land element.
DOUBLEPRECISION DLS (NEL)          !! Loose/bed sediment depth by element.
DOUBLEPRECISION FBETA (NELEE, NSED) !! Sediment composition fraction by element and size class.
DOUBLEPRECISION FDEL (NELEE, NSED)  !! Mobile sediment concentration fraction by element and size class.
DOUBLEPRECISION ABC (NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `A`.
DOUBLEPRECISION BBC (NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `B`.
DOUBLEPRECISION GBC (NSEDEE, NSYCEE) !! Steady boundary sediment input by class/category.
DOUBLEPRECISION DLSMAX             !! Loose-sediment depth above which hillslope soil erosion is suppressed.
DOUBLEPRECISION rdum(nxee*nyee)    !! Floating-point workspace for global grid checks.
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM !! Integer workspace for grid/category checks.
DOUBLEPRECISION DUMMY (NELEE)      !! Floating-point workspace for element checks.
LOGICAL :: LDUM (NELEE)            !! Logical workspace for element checks.
!
! Locals, etc
INTEGER :: FATAL, ERR
DOUBLEPRECISION TOL
PARAMETER (FATAL = 1, ERR = 2, TOL = 1D-10)
!
INTEGER :: BB, COUNT, FACE, ICAT, IUNDEF, IEL, ITYPE, NERR
INTEGER :: SED, SOIL, jedumdum
INTEGER :: IDUM1 (1)
!
!
!
!----------------------------------------------------------------------*
!
!
! 0. Preliminaries
! ----------------
!
!     * Local counter
NERR = 0
!
!
! 1. Static Variables
! -------------------
!
!NEPS
IDUM (1) = NEPS
CALL ALCHKI (ERR, 2012, SPR, 1, 1, IUNDEF, IUNDEF, 'NEPS', 'GE', &
 IONE1, IDUM, NERR, LDUM)
NEPS = IDUM (1)
!FPCRIT
DUMMY (1) = FPCRIT
CALL ALCHK (ERR, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'FPCRIT', 'GE', &
 ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
FPCRIT = DUMMY (1)
!DLSMAX
DUMMY (1) = DLSMAX
CALL ALCHK (ERR, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'DLSMAX', 'GE', &
 ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
DLSMAX = DUMMY (1)
! >>
IF (NLF.GT.0) THEN
! >>
!ISUSED
   IDUM (1) = ISUSED
   CALL ALCHKI (ERR, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', &
    'GE', IZERO1, IDUM, NERR, LDUM)
   CALL ALCHKI (ERR, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', &
    'LE', IONE1, IDUM, NERR, LDUM)
   ISUSED = IDUM (1)
!NFINE
   IDUM (1) = NFINE
   IDUM1 (1) = MIN (1, NSED-1)
   CALL ALCHKI (ERR, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', &
    'GE', IZERO1, IDUM, NERR, LDUM)
   CALL ALCHKI (ERR, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', &
    'LE', IDUM1, IDUM, NERR, LDUM)
   NFINE = IDUM (1)
!ALPHA
   IF (NFINE.GT.0) THEN
      DUMMY (1) = ALPHA
      CALL ALCHK (ERR, 2016, SPR, 1, 1, IUNDEF, IUNDEF, 'ALPHA', &
       'GE', ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
      ALPHA = DUMMY (1)
   ENDIF
!DCBEDO
   DUMMY (1) = DCBEDO
   CALL ALCHK (ERR, 2017, SPR, 1, 1, IUNDEF, IUNDEF, 'DCBEDO', &
    'GE', ZERO1, ZERO1 (1) , DUMMY, NERR, LDUM)
   DCBEDO = DUMMY (1)
!<<
ENDIF
!<<
!NELEE
IDUM (1) = NXEE*NYEE
!!!!IDUM1(1) = MAX( NSED, NLF*DIM(NSED,NFINE) )  !AD
jedumdum = IDIMJE(NSED, NFINE)
jedumdum = jedumdum * NLF
idum1(1) = MAX(nsed, jedumdum)
!     * (including local workspace requirements)
IDUM1 (1) = MAX (IDUM1 (1), NS, NSYB * 2)
CALL ALCHKI (ERR, 2018, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', &
 IDUM1, IDUM, NERR, LDUM)
!
!
! 2. Sediment, Soil & Vegetation Properties
! -----------------------------------------
!
!     * Not enough workspace?
IF (NELEE.LT.MAX (NSED, NS) ) GOTO 300
!
!DRSED
COUNT = NERR
CALL ALCHK (ERR, 2019, SPR, 1, 1, IUNDEF, IUNDEF, 'DRSED(sed)', &
 'GT', ZERO1, ZERO1 (1) , DRSED (1) , NERR, LDUM)

 !original code
 !IF ( NSED.GT.1 .AND. NERR.EQ.COUNT ) THEN
 !        CALL DCOPY( NSED-1, DRSED, 1, IDUM, 1 )
 !        CALL ALCHK    ( ERR,2019,SPR,    2,NSED,IUNDEF,IUNDEF,
 !    $          'DRSED(sed)','GEa',IDUM ,ZERO(1),   DRSED(2),NERR,LDUM )
 !     ENDIF

IF (NSED.GT.1.AND.NERR.EQ.COUNT) THEN
         !CALL DCOPY( NSED-1, DRSED, 1, IDUM, 1 )
   CALL DCOPY (NSED-1, DRSED, 1, RDUM, 1)
   idum(1:NSED-1) = INT (rdum(1:NSED-1))
CALL ALCHK (ERR, 2019, SPR, 2, NSED, IUNDEF, IUNDEF, 'DRSED(sed)', &
& 'GEa', RDUM, ZERO1 (1) , DRSED (2) , NERR, LDUM)
!     $          'DRSED(sed)','GEa',IDUM ,ZERO(1),   DRSED(2),NERR,LDUM
ENDIF
!GKR
CALL ALCHK (ERR, 2020, SPR, 1, NS, IUNDEF, IUNDEF, 'GKR(soil)', &
 'GE', zero1, zero1 (1) , GKR, NERR, LDUM)
!GKF
CALL ALCHK (ERR, 2021, SPR, 1, NS, IUNDEF, IUNDEF, 'GKF(soil)', &
 'GE', zero1, zero1 (1) , GKF, NERR, LDUM)
!RHOSO
CALL ALCHK (ERR, 2022, SPR, 1, NS, IUNDEF, IUNDEF, 'RHOSO(soil)', &
 'GT', zero1, zero1 (1) , RHOSO, NERR, LDUM)
!BKB
IF (NLF.GT.0) CALL ALCHK (ERR, 2023, SPR, 1, NS, IUNDEF, IUNDEF, &
 'BKB(soil)', 'GE', zero1, zero1 (1) , BKB, NERR, LDUM)
!SOSDFN
CALL ALINIT (ZERO1 (1), NS, DUMMY)
DO 220 SED = 1, NSED
   DO 210 SOIL = 1, NS
      DUMMY (SOIL) = DUMMY (SOIL) + SOSDFN (SOIL, SED)
  210    END DO
CALL ALCHK (ERR, 2024, SPR, 1, NS, SED, IUNDEF, 'SOSDFN(soil,sed)' &
&, 'GE', zero1, zero1 (1) , SOSDFN (1, SED) , NERR, LDUM)
  220 END DO
CALL ALCHK (ERR, 2024, SPR, 1, NS, IUNDEF, IUNDEF, 'SOSDFN[*][sum_over_sed](soil)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)
!XDRIP
CALL ALCHK (ERR, 2025, SPR, 1, NV, IUNDEF, IUNDEF, 'XDRIP(veg)', &
 'GE', zero1, zero1 (1) , XDRIP, NERR, LDUM)
!DRDRIP
CALL ALCHK (ERR, 2026, SPR, 1, NV, IUNDEF, IUNDEF, 'DRDRIP(veg)', &
 'GT', zero1, zero1 (1) , DRDRIP, NERR, LDUM)
!FDRIP
CALL ALCHK (ERR, 2027, SPR, 1, NV, IUNDEF, IUNDEF, 'FDRIP(veg)', &
 'GE', zero1, zero1 (1) , FDRIP, NERR, LDUM)
!
!
! 3. Link Element Properties
! --------------------------
!
  300 IF (NLF.GT.0) THEN
!
!NTSOBK
   IDUM (1) = NS
CALL ALCHKI (ERR, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'GE', IONE1, NTSOBK, NERR, LDUM)
CALL ALCHKI (ERR, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'LE', IDUM, NTSOBK, NERR, LDUM)
!PBSED
CALL ALCHK (ERR, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', &
& 'GE', zero1, zero1 (1) , PBSED, NERR, LDUM)
CALL ALCHK (ERR, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', &
& 'LT', ONE1, ZERO1 (1) , PBSED, NERR, LDUM)
!
ENDIF
!
!
! 4. Column-element Properties
! ----------------------------
!
!FCROCK
CALL ALCHK (ERR, 2030, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCROCK(iel)', 'LE', ONE1, ZERO1 (1) , FCROCK, NERR, LDUM)
!FCG
DO 410 IEL = NLF + 1, NEL
   DUMMY (IEL) = ONE1 (1) - FCROCK (IEL)
  410 END DO
CALL ALCHK (ERR, 2031, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCG(iel)', 'LEa', DUMMY (NLF + 1) , ZERO1 (1) , FCG, NERR, LDUM)
!PLS
CALL ALCHK (ERR, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'GE', zero1, zero1 (1) , PLS, NERR, LDUM)
CALL ALCHK (ERR, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'LT', ONE1, ZERO1 (1) , PLS, NERR, LDUM)
!
!
! 5. All-element Initialization
! -----------------------------
!
!DLS
CALL ALCHK (ERR, 2033, SPR, 1, NEL, IUNDEF, IUNDEF, 'DLS(iel)', &
 'GE', zero1, zero1 (1) , DLS, NERR, LDUM)
!FBETA
CALL ALINIT (ZERO1 (1), NEL, DUMMY)
DO 520 SED = 1, NSED
   DO 510 IEL = 1, NEL
      DUMMY (IEL) = DUMMY (IEL) + FBETA (IEL, SED)
  510    END DO
CALL ALCHK (ERR, 2034, SPR, 1, NEL, SED, IUNDEF, 'FBETA(iel,sed)', &
& 'GE', zero1, zero1 (1) , FBETA (1, SED) , NERR, LDUM)
  520 END DO
CALL ALCHK (ERR, 2034, SPR, 1, NEL, IUNDEF, IUNDEF, 'FBETA[*][sum_over_sed](iel)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)
!FDEL
DO 530 SED = 1, NSED
CALL ALCHK (ERR, 2035, SPR, 1, NEL, SED, IUNDEF, 'FDEL(iel,sed)', &
&'GE', zero1, zero1 (1) , FDEL (1, SED) , NERR, LDUM)
  530 END DO
!
!
! 6. Boundary Data
! ----------------
!
IF (NSYB.GT.0) THEN
!Not enough workspace?
   IF (NELEE.LT.NSYB * 2) GOTO 700
!NSYCEE
   IDUM (1) = NSYCEE
   IDUM1 (1) = MAX (NSYC (1) + NSYC (2), NSYC (3) + NSYC (4) )
   CALL ALCHKI (ERR, 2036, SPR, 1, 1, IUNDEF, IUNDEF, 'NSYCEE', &
    'GE', IDUM1, IDUM, NERR, LDUM)
!NSYBCD(BB,1)
   COUNT = NERR
   IDUM1 (1) = NEL
CALL ALCHKI (ERR, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', &
& 'GE', IONE1, NSYBCD, NERR, LDUM)
CALL ALCHKI (ERR, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', &
& 'LE', IDUM1, NSYBCD, NERR, LDUM)
!NBFACE
   IF (COUNT.EQ.NERR) THEN
      DO 610 BB = 1, NSYB
         IEL = NSYBCD (BB, 1)
         IDUM (BB) = NBFACE (IEL)
  610       END DO
      IDUM1 (1) = 4
      CALL ALCHKI (ERR, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, &
       'NBFACE[NSYBCD[*][1]](bdry)', 'GE', IONE1, IDUM, NERR, LDUM)
      CALL ALCHKI (ERR, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, &
       'NBFACE[NSYBCD[*][1]](bdry)', 'LE', IDUM1, IDUM, NERR, LDUM)
   ENDIF
!ICMREF
   IF (COUNT.EQ.NERR) THEN
      DO 620 BB = 1, NSYB
         IEL = NSYBCD (BB, 1)
         FACE = NBFACE (IEL)
         IDUM (BB) = ICMREF (IEL, FACE, 2)
  620       END DO
      CALL ALCHKI (ERR, 2039, SPR, 1, NSYB, IUNDEF, IUNDEF, &
       'ICMREF[NSYBCD[*][1]][NBFACE][2](bdry)', 'EQ', IZERO1, IDUM, &
       NERR, LDUM)
   ENDIF
!NSYBCD(BB,3)
   DO 630 BB = 1, NSYB
      ITYPE = NSYBCD (BB, 2)
      IDUM (BB) = 1
      IF (MOD (ITYPE, 2) .EQ.0) IDUM (BB) = IDUM (BB) + NSYC ( &
       ITYPE-1)
      IDUM (NSYB + BB) = IDUM (BB) + NSYC (ITYPE)
  630    END DO
CALL ALCHKI (ERR, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', &
& 'GE', IDUM, NSYBCD (1, 3) , NERR, LDUM)
CALL ALCHKI (ERR, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', &
& 'LE', IDUM (NSYB + 1) , NSYBCD (1, 3) , NERR, LDUM)
!GBC
   DO 640 ICAT = 1, NSYC (1)
CALL ALCHK (ERR, 2041, SPR, 1, NSED, ICAT, IUNDEF, 'GBC(sed,icat)' &
&, 'GE', zero1, zero1 (1) , GBC (1, ICAT) , NERR, LDUM)
  640    END DO
!ABC
   DO 650 ICAT = 1, NSYC (3)
CALL ALCHK (ERR, 2042, SPR, 1, NSED, ICAT, IUNDEF, 'ABC(sed,icat)' &
&, 'GE', zero1, zero1 (1) , ABC (1, ICAT) , NERR, LDUM)
  650    END DO
!BBC
   DO 660 ICAT = 1, NSYC (3)
CALL ALCHK (ERR, 2043, SPR, 1, NSED, ICAT, IUNDEF, 'BBC(sed,icat)' &
&, 'GT', zero1, zero1 (1) , BBC (1, ICAT) , NERR, LDUM)
  660    END DO
!SFB
   IF (NSYC (2) .GT.0) THEN
      IDUM (1) = SFB
      CALL ALCHKI (ERR, 2044, SPR, 1, 1, IUNDEF, IUNDEF, 'SFB', &
       'GE', IZERO1, IDUM, NERR, LDUM)
   ENDIF
!SRB
   IF (NSYC (2) .GT.0) THEN
      IDUM (1) = SRB
      CALL ALCHKI (ERR, 2045, SPR, 1, 1, IUNDEF, IUNDEF, 'SRB', &
       'GE', IZERO1, IDUM, NERR, LDUM)
   ENDIF
ENDIF
!
!
! 7. Epilogue
! -----------
!
  700 IF (NERR.GT.0) CALL ERROR (FATAL, 2000, SPR, 0, 0, 'Error(s) detected while checking SY input data')
!
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
SUBROUTINE SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, &
 ICMRF2, ISORT, DTUZ, CLAI, PLAI, ARXL, DRAINA, PNETTO, HRF, &
 ZGRUND, QOC, IQ, JMIN, JSORT, LDUM)

INTEGER :: NEL                     !! Number of elements.
INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NV                      !! Number of vegetation types.
INTEGER :: SPR                     !! Sediment diagnostic output unit.
INTEGER :: ICMREF (NELEE, 4, 2:3)  !! Face-neighbour and reverse-face reference map.
INTEGER :: ICMRF2 (NLFEE, 3, 2)    !! Confluence branch reference map.
INTEGER :: ISORT (NEL)             !! Donor-before-receptor element routing order.
DOUBLEPRECISION DTUZ               !! Unsaturated-zone timestep in seconds.
DOUBLEPRECISION CLAI (NV)          !! Current canopy leaf-area index by vegetation type.
DOUBLEPRECISION PLAI (NV)          !! Potential/maximum leaf-area index by vegetation type.
DOUBLEPRECISION ARXL (NLFEE)       !! Channel cross-sectional area by link.
DOUBLEPRECISION DRAINA (NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
DOUBLEPRECISION PNETTO (NLF + 1:NEL) !! Net precipitation/effective rainfall by land element.
DOUBLEPRECISION HRF (NEL)          !! Water level/head by element.
DOUBLEPRECISION ZGRUND (NEL)       !! Ground or bed elevation by element.
DOUBLEPRECISION QOC (NELEE, 4)     !! Face water fluxes.
DOUBLEPRECISION rdum(nelee)        !! Floating-point workspace for reporting failed arrays.
INTEGER :: IQ (NEL)                !! Workspace for routing-order checks.
INTEGER :: JMIN (NEL)              !! Earliest required receptor position by donor.
INTEGER :: JSORT (0:NEL + 1)       !! Inverse of `ISORT` with sentinel entries.
LOGICAL :: LDUM (NELEE)            !! Logical workspace for element/face checks.
!
! Locals, etc
INTEGER :: FATAL, ERR
DOUBLEPRECISION TOL
PARAMETER (FATAL = 1, ERR = 2, TOL = 1D-7)
!
INTEGER :: FACE, FADJ, I, IADJ, IBR, IEL, IUNDEF, J, NELP, NERR, &
 P
DOUBLEPRECISION FNQOUT, QADJ, QMIN
DOUBLEPRECISION DUM1 (1)
!
!     * Water discharge rate
FNQOUT (IEL, FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
!
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
!DTUZ
DUM1 (1) = DTUZ
CALL ALCHK (ERR, 2046, SPR, 1, 1, IUNDEF, IUNDEF, 'DTUZ', 'GE', &
 zero1, zero1 (1) , DUM1, NERR, LDUM)
!
!
! 2. Vegetative State
! -------------------
!
!CLAI
CALL ALCHK (ERR, 2047, SPR, 1, NV, IUNDEF, IUNDEF, 'CLAI(veg)', &
 'GE', zero1, zero1 (1) , CLAI, NERR, LDUM)
!PLAI
CALL ALCHK (ERR, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
 'GE', zero1, zero1 (1) , PLAI, NERR, LDUM)
CALL ALCHK (ERR, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
 'LE', ONE1, ZERO1 (1) , PLAI, NERR, LDUM)
!
!
! 3. Link State
! -------------
!
IF (NLF.GT.0) THEN
!
!ARXL
CALL ALCHK (ERR, 2049, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
&'GE', zero1, zero1 (1) , ARXL, NERR, LDUM)
!
ENDIF
!
!
! 4. Columnar State
! -----------------
!
!DRAINA
CALL ALCHK (ERR, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'GE', zero1, zero1 (1) , DRAINA, NERR, LDUM)
! 10.10.94  Ought to fix WAT module so that we don't need TOL
CALL ALCHK (ERR, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'LEa', PNETTO, TOL, DRAINA, NERR, LDUM)
!
!
! 5. Elemental State
! ------------------
!
!HRF
CALL ALCHK (ERR, 2051, SPR, 1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', &
 'GEa', ZGRUND, ZERO1 (1) , HRF, NERR, LDUM)
!
!
! 6. Flux/Ordering
! ----------------
!
!ISORT & QOC
!     * Set JSORT = inverse of ISORT & initialize upper bound JMIN
!       (note that JSORT has overspill elements )
NELP = NEL + 1
DO 610 J = 0, NELP
   JSORT (J) = NELP
  610 END DO
DO 620 I = 1, NEL
   IEL = ISORT (I)
   J = MAX (0, MIN (IEL, NELP) )
   JSORT (J) = I
   JMIN (I) = NELP
  620 END DO
!     * At this point any element not listed in ISORT has a JSORT
!       value of NELP, which is guaranteed to fail the test below
!     * Update JMIN (used as object of JSORT test) & set QOC status IQ
DO 650 FACE = 1, 4
   DO 640 IEL = 1, NEL
!           * innocent until proven guilty
      IQ (IEL) = 0
!           * non-discharge faces are ok
      IF (FNQOUT (IEL, FACE) .LE.ZERO1 (1) ) GOTO 640
!                                              ^^^^^^^^
      IADJ = ICMREF (IEL, FACE, 2)
      IF (IADJ.GT.0) THEN
         FADJ = ICMREF (IEL, FACE, 3)
         QADJ = FNQOUT (IADJ, FADJ)
!              * do both elements discharge into the same face?
         IF (QADJ.GT.ZERO1 (1) ) IQ (IEL) = 1
!              * IEL must precede IADJ in the ISORT list
         JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL) )
      ELSEIF (IADJ.LT.0) THEN
         IBR = - IADJ
         QMIN = ONE1 (1)
         DO 630 P = 1, 3
            IADJ = ICMRF2 (IBR, P, 1)
            IF (IADJ.GT.0) THEN
               FADJ = ICMRF2 (IBR, P, 2)
               QADJ = FNQOUT (IADJ, FADJ)
               QMIN = MIN (QADJ, QMIN)
               IF (QADJ.LT.zero1 (1) ) THEN
!                       * IEL must precede IADJ in the ISORT list
                  JMIN (IEL) = MIN (JSORT (IADJ), JMIN (IEL) )
               ENDIF
            ENDIF
  630          END DO
!              * discharge from IEL has nowhere to go?
         IF (QMIN.GE.zero1 (1) ) IQ (IEL) = 2
      ENDIF
  640    END DO
!        * Check QOC status at this FACE for all elements
   CALL ALCHKI (ERR, 2052, SPR, 1, NEL, FACE, IUNDEF, &
    'status_of_QOC(iel,face)', 'EQ', IZERO1, IQ, NERR, LDUM)
  650 END DO
!     * Check that each donor element listed in ISORT occurs before
!       each of its receptors, and that all elements are listed
CALL ALCHKI (ERR, 2053, SPR, 1, NEL, IUNDEF, IUNDEF, &
 'position_in_ISORT(iel)', 'LTa', JMIN, JSORT (1) , NERR, LDUM)
!
!
! 7. Epilogue
! -----------
!
IF (NERR.GT.0) THEN
!
   WRITE (SPR, 9100) 'DTUZ', DTUZ
   WRITE (SPR, 9100) 'CLAI[veg=1,...,NV]', CLAI
   WRITE (SPR, 9100) 'PLAI[veg=1,...,NV]', PLAI
   rdum(1:nlf)=ARXL(1:nlf)  !AD
   WRITE (SPR, 9100) 'ARXL[link=1,...,NLF]', (rdum (IEL) , IEL = 1, NLF)
   WRITE (SPR, 9100) 'DRAINA[col=NLF+1,...,NEL]', DRAINA
   WRITE (SPR, 9100) 'PNETTO[col=NLF+1,...,NEL]', PNETTO
   rdum(1:nel)=hrf(1:nel)  !AD
   WRITE (SPR, 9100) 'HRF[iel=1,...,NEL]', rdum(1:nel)
   WRITE (SPR, 9100) 'ZGRUND[iel=1,...,NEL]', ZGRUND
   WRITE (SPR, 9200) 'ISORT[iel=1,...,NEL]', ISORT
   WRITE (SPR, 9200) 'position_in_ISORT[iel=1,...,NEL]', (JSORT ( &
    IEL) , IEL = 1, NEL)
   DO 710 FACE = 1, 4
      WRITE (SPR, 9150) 'QOC[iel=1,...,NEL][face=', FACE, ']', &
       (QOC (IEL, FACE) , IEL = 1, NEL)
  710    END DO
!
CALL ERROR (ERR, 2003, SPR, 0, 0, 'Error(s) detected'//' while checking time-dependent WAT-SY interface')
!
ENDIF
!
 9100 FORMAT(1X,A,     ':'/1P,(8E10.2))
 9150 FORMAT(1X,A,I1,A,':'/1P,(8E10.2))
 9200 FORMAT(1X,A,     ':'/   (16I5  ))
!
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
SUBROUTINE SYFINE (DRSEDF, FBIC, FICRIT, NLF, ALPHA, DTSY, AREA, &
 DCBF, FBETAF, FDELF, PBSED, TAUK, VCFMAX, VINFMX, BARM)

USE CONST_SY

INTEGER :: NLF                     !! Number of channel links.
DOUBLEPRECISION DRSEDF             !! Representative fine-sediment particle diameter.
DOUBLEPRECISION FBIC               !! Fine-bed fraction threshold for infiltration.
DOUBLEPRECISION FICRIT             !! Fine-concentration threshold for infiltration.
DOUBLEPRECISION ALPHA              !! Fine-sediment settling/resuspension critical-shear ratio.
DOUBLEPRECISION DTSY               !! Sediment substep duration.
DOUBLEPRECISION AREA (NLF)         !! Link bed/contact area used for fine exchange.
DOUBLEPRECISION DCBF (NLF)         !! Active-bed fine sediment depth.
DOUBLEPRECISION PBSED (NLF)        !! Channel-bed sediment porosity by link.
DOUBLEPRECISION FBETAF (NLF)       !! Fine fraction in the active bed by link.
DOUBLEPRECISION FDELF (NLF)        !! Mobile fine-sediment concentration fraction by link.
DOUBLEPRECISION TAUK (NLF)         !! Channel/link shear stress.
DOUBLEPRECISION VCFMAX (NLF)       !! Maximum fine volume available for settling/infiltration.
DOUBLEPRECISION VINFMX (NLF)       !! Maximum fine infiltration volume.
LOGICAL :: BARM (NLF)              !! True where fine sediment is protected by bed armouring.
!
! Locals, etc
DOUBLEPRECISION DUM, TAUEC, VMAX
DOUBLEPRECISION ADOUBLEPRECISION, DCFMXL, FDELFL, TAUKL
INTEGER :: LINK
!
!----------------------------------------------------------------------*
!
!
!     * Calculate settling velocity for fines ( first call only )
IF (FIRST_syfine) THEN
   FIRST_syfine = .FALSE.
   WSED_syfine = DRSEDF**2 * GRAVTY * (RHOSED-RHOWAT) / (18 * RHOWAT * &
    VISCOS)
ENDIF
!
!     * Loop over channel links
DO 100 LINK = 1, NLF
!
   TAUKL = TAUK (LINK)
   ADOUBLEPRECISION = AREA (LINK)
   FDELFL = FDELF (LINK)
!
!        * Calculate critical shear stress for fines
   CALL SYCRIT (0, DRSEDF, TAUKL, DUM, TAUEC)
!
!        * Calculate potential fines in upper layer
!        *  (existing fines + settling)
   DUM = ALPHA * TAUEC
   IF (DUM.GT.0) DUM = DIMJE(DUM, TAUKL) / DUM
   DCFMXL = DCBF (LINK) + FDELFL * WSED_syfine * DUM * DTSY
   VCFMAX (LINK) = DCFMXL * ADOUBLEPRECISION
!
!        * Can fines be armoured ?
   BARM (LINK) = TAUKL.LE.TAUEC
!
!        * Calculate potential infiltration rate
   VMAX = 0
   IF (FBETAF (LINK) .LT.FBIC) VMAX = WSED_syfine * ADOUBLEPRECISION * DIMJE(FDELFL, &
    FICRIT / (1 - PBSED (LINK) ) ) * DTSY
   VINFMX (LINK) = VMAX
!
!     * End of link loop
  100 END DO
!
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
!> | Flow-memory arrays | Copies current channel cross-sectional area `ARXL` to `ARXLOL` and zeros every `QSED(:,sed,face)`. |
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
SUBROUTINE SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, &
 NTSOBK, ARXL, DCBEDO, DLS, FBETA, DRSED, HRF, PBSED, PLS, SOSDFN, &
 THSAT, ZGRUND, NTSOTP, ZBFULL, ARBDEP, ARXLOL, DCBED, DCBSED, &
 DDBSED, DRSO50, DWATOL, FETA, GINFD, GINFS, GNU, GNUBK, QSED, &
 DBFULL)

INTEGER :: NEL                     !! Number of elements.
INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NS                      !! Number of soil types.
INTEGER :: NSED                    !! Number of sediment size classes.
INTEGER :: NSEE                    !! Soil-type array dimension.
INTEGER :: NSEDEE                  !! Sediment-size array dimension.
INTEGER :: NTSOBK (NLFEE)          !! Bank soil type by link.
INTEGER :: NTSOTP (NLF + 1:NEL)    !! Top soil type by land element.
DOUBLEPRECISION ARXL (NLFEE)       !! Channel cross-sectional area by link.
DOUBLEPRECISION DCBEDO             !! Active upper channel-bed layer thickness.
DOUBLEPRECISION DLS (NEL)          !! Initial loose/bed sediment depth by element.
DOUBLEPRECISION DRSED (NSED)       !! Representative sediment particle diameters.
DOUBLEPRECISION FBETA (NELEE, NSED) !! Initial sediment composition by element and size class.
DOUBLEPRECISION HRF (NLF + 1:NEL)  !! Initial land-element water level/head.
DOUBLEPRECISION PBSED (NLFEE)      !! Channel-bed sediment porosity by link.
DOUBLEPRECISION PLS (NLF + 1:NEL)  !! Loose-sediment porosity by land element.
DOUBLEPRECISION SOSDFN (NSEE, NSED) !! Soil sediment-size fractions by soil type.
DOUBLEPRECISION THSAT (NS)         !! Saturated water content by soil type.
DOUBLEPRECISION ZBFULL (NLFEE)     !! Bankfull elevation/depth by link.
DOUBLEPRECISION ZGRUND (NEL)       !! Ground or bed elevation by element.
DOUBLEPRECISION ARBDEP (NLFEE)     !! Accumulated channel-bed elevation/depth change.
DOUBLEPRECISION ARXLOL (NLFEE)     !! Previous channel cross-sectional area by link.
DOUBLEPRECISION DBFULL (NLFEE)     !! Bankfull depth by link.
DOUBLEPRECISION DCBED (NLFEE)      !! Active upper-bed layer depth by link.
DOUBLEPRECISION DCBSED (NLFEE, NSED) !! Upper-bed sediment depth by link and size class.
DOUBLEPRECISION DDBSED (NLFEE, NSED) !! Lower-bed sediment depth by link and size class.
DOUBLEPRECISION DRSO50 (NS)        !! Median soil particle diameter by soil type.
DOUBLEPRECISION DWATOL (NLF + 1:NEL) !! Previous water depth by land element.
DOUBLEPRECISION FETA (NEL)         !! Soil-to-sediment solid-volume conversion factor by element.
DOUBLEPRECISION GINFD (NLFEE, NSED) !! Fine infiltration diagnostic/source for deposited material.
DOUBLEPRECISION GINFS (NLFEE, NSED) !! Fine infiltration diagnostic/source for suspended material.
DOUBLEPRECISION GNU (NLF + 1:NEL)  !! Hillslope erosion rate by land element.
DOUBLEPRECISION GNUBK (NLFEE)      !! Lateral bank erosion rate by link.
DOUBLEPRECISION QSED (NELEE, NSEDEE, 4) !! Sediment flux by element, size class, and face.
!
! Locals, etc
!
DOUBLEPRECISION DCBEDE, DDBEDE, DLSE, FBETAE
INTEGER :: IEL, LINK, SED, SOIL, FACE
!
!
!----------------------------------------------------------------------*
!
!
!     * Initialize surface erosion rates in each column
CALL ALINIT (ZERO, NEL - NLF, GNU (NLF + 1) )
!
IF (NLF.GT.0) THEN
!
!        * Initialize bank erosion rates in each link
   CALL ALINIT (ZERO, NLF, GNUBK)
!
!        * Zero bed sediment accumulator
   CALL ALINIT (ZERO, NLF, ARBDEP)
!
!        * Set old river c/s area equal to current river c/s area
   CALL DCOPY (NLF, ARXL, 1, ARXLOL, 1)
!
ENDIF
!
!
!     * Loop over sediment types
DO 200 SED = 1, NSED
!
   IF (NLF.GT.0) THEN
!
!           * Initialize infiltration rates
      CALL ALINIT (ZERO, NLF, GINFD (1, SED) )
      CALL ALINIT (ZERO, NLF, GINFS (1, SED) )
!
   ENDIF
!
!        * Initialize sediment flow rates
   DO 100 FACE = 1, 4
      CALL ALINIT (ZERO, NEL, QSED (1, SED, FACE) )
  100    END DO
!
!     * Next sediment type
  200 END DO
!
!
!     * Loop over links
DO 400 LINK = 1, NLF
   DLSE = DLS (LINK)
!
!        * Set ratio of bank soil to bed sediment solid volume fractions
   FETA (LINK) = (1 - THSAT (NTSOBK (LINK) ) ) / (1 - PBSED (LINK) &
    )
!
!        * Set bank full depth
   DBFULL (LINK) = ZBFULL (LINK) - ZGRUND (LINK)
!
!        * Bed layer depths
   DCBEDE = MIN (DLSE, DCBEDO)
   DDBEDE = DIMJE(DLSE, DCBEDE)
   DCBED (LINK) = DCBEDE
!
!        * Loop over sediment types
   DO 300 SED = 1, NSED
!
!           * Initialize sediment depths in both bed layers
      FBETAE = FBETA (LINK, SED)
      DCBSED (LINK, SED) = DCBEDE * FBETAE
      DDBSED (LINK, SED) = DDBEDE * FBETAE
!
!        * Next sediment type
  300    END DO
!
!     * Next link
  400 END DO
!
!
!     * Loop over column elements
DO 500 IEL = NLF + 1, NEL
!
!        * Set ratio: surface soil to loose sediment solid vol fractions
   FETA (IEL) = (1 - THSAT (NTSOTP (IEL) ) ) / (1 - PLS (IEL) )
!
!        * Calculate initial surface water depth
   DWATOL (IEL) = HRF (IEL) - ZGRUND (IEL)
!
  500 END DO
!
!
!     * Calculate median particle diameter for each soil type
DO 600 SOIL = 1, NS
   DRSO50 (SOIL) = SYDR (HALF, NSEE, NSED, SOSDFN (SOIL, 1), &
    DRSED)
  600 END DO
!
!
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
SUBROUTINE SYLINK (NFINE, NSED, NSEDEE, DTSY, AREAE, ARXLOE, &
 ARXLE, CLENTE, EPSBE, PBSEDE, VINFME, BARME, VCFMAE, CONCIE, &
 DCBSEE, DDBSEE, QSDWAE, QWAT, SOSDFE, FDELE, QSEDE, DCIPRE, &
 DDIPRE, GINFDE, GINFSE)

INTEGER :: NFINE                   !! Number of fine sediment classes.
INTEGER :: NSED                    !! Number of sediment size classes.
INTEGER :: NSEDEE                  !! Sediment-size array dimension.
LOGICAL :: BARME                   !! True where fine sediment is protected by bed armouring.
DOUBLEPRECISION DTSY               !! Sediment substep duration.
DOUBLEPRECISION AREAE              !! Link bed/contact area.
DOUBLEPRECISION ARXLOE             !! Previous channel cross-sectional area.
DOUBLEPRECISION ARXLE              !! Current channel cross-sectional area.
DOUBLEPRECISION CLENTE             !! Channel-link length.
DOUBLEPRECISION EPSBE              !! Bank erosion sediment source for the link.
DOUBLEPRECISION PBSEDE             !! Channel-bed sediment porosity.
DOUBLEPRECISION CONCIE (NSED)      !! Capacity concentration by sediment class.
DOUBLEPRECISION DCBSEE (NSED)      !! Current upper-bed sediment depth by size class.
DOUBLEPRECISION DDBSEE (NSED)      !! Current lower-bed sediment depth by size class.
DOUBLEPRECISION QWAT (4)           !! Outward water flux by face.
DOUBLEPRECISION QSDWAE (NSEDEE, 4) !! Sediment advection coefficient by size class and face.
DOUBLEPRECISION SOSDFE (NSED)      !! Bank/source sediment-size fraction.
DOUBLEPRECISION VCFMAE             !! Maximum fine volume available for settling/infiltration.
DOUBLEPRECISION VINFME             !! Maximum fine infiltration volume.
DOUBLEPRECISION FDELE (NSED)       !! Mobile sediment concentration fraction by size class.
DOUBLEPRECISION QSEDE (NSEDEE, 4)  !! Sediment flux by size class and face.
DOUBLEPRECISION DCIPRE (NSED)      !! Interim upper-bed sediment depth for later bed update.
DOUBLEPRECISION DDIPRE (NSED)      !! Interim lower-bed sediment depth for later bed update.
DOUBLEPRECISION GINFDE (NSED)      !! Fine infiltration diagnostic/source for deposited material.
DOUBLEPRECISION GINFSE (NSED)      !! Fine infiltration diagnostic/source for suspended material.
!
! Locals, etc
INTEGER :: FACE, J (4), JI, K (4), KI, NIN, NOUT, SED
DOUBLEPRECISION AREAEI, DCBEEE, DCIPEE, DTSYI, FDC, FDELEE, GINF
DOUBLEPRECISION OMPB, OMPBI, QSEDIN, SUM, SUMN, SUMP
DOUBLEPRECISION VCFS, VCARM, VDMAX, VDSEDS, VDSED, VDWAT, VINF, &
 VSTRAN
!
!
!----------------------------------------------------------------------*
!
!
! Initialization
! --------------
!
!     * Make lists of outflow and inflow faces
NIN = 0
NOUT = 0
DO 100 FACE = 1, 4
   IF (QWAT (FACE) .GT.0) THEN
      NOUT = NOUT + 1
      J (NOUT) = FACE
   ELSE
      NIN = NIN + 1
      K (NIN) = FACE
   ENDIF
  100 END DO
!
SUMP = 0
SUMN = 0
OMPB = 1 - PBSEDE
OMPBI = 1 / OMPB
DTSYI = 1 / DTSY
AREAEI = 1 / AREAE
!
!
! Loop over size groups ( largest to smallest )
! ---------------------------------------------
!
!     * Loop over sediment types ( largest to smallest )
DO 500 SED = NSED, 1, - 1
   DCBEEE = DCBSEE (SED)
!
!
!        Water and sediment budgets
!        --------------------------
!
!        * Calculate sediment inflow rate
   SUM = 0
   DO 200 KI = 1, NIN
      SUM = SUM + QSEDE (SED, K (KI) )
  200    END DO
   QSEDIN = - SUM * OMPBI
!
!        * Volume of water remaining + advective water discharge
   SUM = 0
   DO 300 JI = 1, NOUT
      SUM = SUM + QSDWAE (SED, J (JI) )
  300    END DO
   VDWAT = ARXLE * CLENTE+SUM * DTSY
!
!        * Sediment available for resuspension/transport/infiltration
!        *   /armouring
   VDMAX = FDELE (SED) * ARXLOE * CLENTE+DCBEEE * AREAE+ (QSEDIN + &
    EPSBE * SOSDFE (SED) ) * DTSY
!
!
!        Infiltration and Armouring
!        --------------------------
!
!        * Sediment volumes subject to infiltration & armouring resp.
   IF (SED.GT.NFINE) THEN
!           * Non-fines
      VINF = 0
      VCARM = 0
   ELSE
!           * Fines
      VCFS = MIN (VCFMAE, VDMAX)
      VINF = MIN (VINFME, VCFS)
!           * ( SUMN/SUMP calculated below, summed over earlier passes )
      FDC = 0
      IF (BARME.AND.0.LT.SUMN) FDC = MIN (SUMN, SUMP) / SUMN
      VCARM = FDC * DIMJE(VCFS, VINF)
   ENDIF
!
!        * Volume in and above top layer after infiltration ...
   VDSEDS = DIMJE(VDMAX, VINF)
!        * ... minus armoured volume ( = SUPPLY limit for transport )
   VDSED = DIMJE(VDSEDS, VCARM)
!
!        * Infiltration rates for each layer
   GINF = VINF * DTSYI
   GINFDE (SED) = GINF
   GINFSE (SED) = GINF
!
!
!        Other output variables
!        ----------------------
!
!        * Sediment remaining in suspension + sediment discharged
!        * - limited by either SUPPLY or CAPACITY
   VSTRAN = MIN (VDSED, CONCIE (SED) * OMPBI * VDWAT)
!
!        * Concentration in suspension ('relative density')
   FDELEE = 0
   IF (VDWAT.GT.0) FDELEE = VSTRAN / VDWAT
   FDELE (SED) = FDELEE
!
!        * Interim layer depths
   DCIPEE = DIMJE(VDSEDS, VSTRAN) * AREAEI
   DCIPRE (SED) = DCIPEE
   DDIPRE (SED) = DDBSEE (SED) + VINF * AREAEI
!
!        * Particulate discharge rates at outflow faces
   DO 400 JI = 1, NOUT
      QSEDE (SED, J (JI) ) = QSDWAE (SED, J (JI) ) * FDELEE * &
       OMPB
  400    END DO
!
!
!        Epilogue
!        --------
!
!        * Depth of non-fines in interim and old top layers
!        *  ( used above on final pass: definition point must be later )
!        *  ( than reference point                                     )
   SUMP = SUMP + DCIPEE
   SUMN = SUMN + DCBEEE
!
!     * Next sediment type
  500 END DO
!
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
SUBROUTINE SYMAIN (NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD, &
 ICMBK, ICMREF, ICMRF2, ICMXY, NBFACE, NLYR, NTSOIL, NVC, AREA, &
 CLENTH, CWIDTH, DHF, DXQQ, DYQQ, THSAT, ZBFULL, ZGRUND, BEXBK, &
 LINKNS, ISORT, DTUZ, TIH, UZNOW, ARXL, CLAI, DRAINA, HRF, PLAI, &
 PNETTO, QOC, NSED, PBSED, PLS, SOSDFN, ARBDEP, DLS, FBETA, FDEL, &
 GINFD, GINFS, GNU, GNUBK, QSED, DCBED, DCBSED, IDUM, DUMMY)
INTEGER :: NEL                     !! Number of elements.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NS                      !! Number of soil types.
INTEGER :: NV                      !! Number of vegetation types.
INTEGER :: NX                      !! Number of grid columns.
INTEGER :: NY                      !! Number of grid rows.
INTEGER :: SFB                     !! Sediment boundary file unit.
INTEGER :: SPR                     !! Sediment diagnostic output unit.
INTEGER :: SRB                     !! Sediment rating-boundary file unit.
INTEGER :: SYD                     !! Static sediment input unit.
INTEGER :: ICMBK (NLFEE, 2)        !! Bank-element numbers for each channel link.
INTEGER :: ICMREF (NELEE, 4, 2:3)  !! Face-neighbour and reverse-face reference map.
INTEGER :: ICMRF2 (NLFEE, 3, 2)    !! Confluence branch reference map.
INTEGER :: ICMXY (NXEE, NY)        !! Element number at each grid location.
INTEGER :: NBFACE (NEL)            !! Number of boundary faces by element.
INTEGER :: NLYR (NLF + 1:NEL)      !! Number of soil layers in each land element.
INTEGER :: NTSOIL (NEL, NLYREE)    !! Soil type index for each element layer.
INTEGER :: NVC (NLF + 1:NEL)       !! Vegetation type by land element.
INTEGER :: ISORT (NEL)             !! Donor-before-receptor element routing order.
DOUBLEPRECISION AREA (NEL)         !! Element plan area.
DOUBLEPRECISION CLENTH (NLFEE)     !! Channel-link length.
DOUBLEPRECISION CWIDTH (NLFEE)     !! Channel width by link.
DOUBLEPRECISION DHF (NELEE, 4)     !! Face-to-face hydraulic distance.
DOUBLEPRECISION DXQQ (NLF + 1:NEL) !! Land-element width.
DOUBLEPRECISION DYQQ (NLF + 1:NEL) !! Land-element length.
DOUBLEPRECISION THSAT (NS)         !! Saturated water content by soil type.
DOUBLEPRECISION ZBFULL (NLFEE)     !! Bankfull elevation/depth by link.
DOUBLEPRECISION ZGRUND (NEL)       !! Ground or bed elevation by element.
DOUBLEPRECISION DTUZ               !! Unsaturated-zone timestep in seconds.
DOUBLEPRECISION TIH                !! Initial simulation time in hours.
DOUBLEPRECISION UZNOW              !! Current unsaturated-zone simulation time.
DOUBLEPRECISION ARXL (NLFEE)       !! Channel cross-sectional area by link.
DOUBLEPRECISION CLAI (NV)          !! Current canopy leaf-area index by vegetation type.
DOUBLEPRECISION DRAINA (NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
DOUBLEPRECISION HRF (NEL)          !! Water level/head by element.
DOUBLEPRECISION PLAI (NV)          !! Potential/maximum leaf-area index by vegetation type.
DOUBLEPRECISION PNETTO (NLF + 1:NEL) !! Net precipitation/effective rainfall by land element.
DOUBLEPRECISION QOC (NELEE, 4)     !! Face water fluxes.
LOGICAL :: BEXBK                   !! True when bank elements are represented.
LOGICAL :: LINKNS (NLFEE)          !! True for north-south channel links.
INTEGER :: NSED                    !! Number of sediment size classes.
DOUBLEPRECISION PBSED (NLFEE)      !! Channel-bed sediment porosity by link.
DOUBLEPRECISION PLS (NLF + 1:NEL)  !! Loose-sediment porosity by land element.
DOUBLEPRECISION SOSDFN (NSEE, NSEDEE) !! Soil sediment-size fractions by soil type.
DOUBLEPRECISION ARBDEP (NLFEE)     !! Accumulated channel-bed elevation/depth change.
DOUBLEPRECISION DLS (NEL)          !! Loose/bed sediment depth by element.
DOUBLEPRECISION DCBED (NLFEE)      !! Active upper-bed layer depth by link.
DOUBLEPRECISION DCBSED (NLFEE, NSEDEE) !! Upper-bed sediment depth by link and size class.
DOUBLEPRECISION FBETA (NELEE, NSEDEE)  !! Sediment composition fraction by element and size class.
DOUBLEPRECISION FDEL (NELEE, NSEDEE)   !! Mobile sediment concentration fraction by element and size class.
DOUBLEPRECISION GINFD (NLFEE, NSEDEE)  !! Fine infiltration diagnostic/source for deposited material.
DOUBLEPRECISION GINFS (NLFEE, NSEDEE)  !! Fine infiltration diagnostic/source for suspended material.
DOUBLEPRECISION GNU (NLF + 1:NEL)      !! Hillslope erosion rate by land element.
DOUBLEPRECISION GNUBK (NLFEE)          !! Lateral bank erosion rate by link.
DOUBLEPRECISION QSED (NELEE, NSEDEE, 4) !! Sediment flux by element, size class, and face.
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM  !! Integer workspace for checks and reads.
DOUBLEPRECISION DUMMY (NELEE)          !! Floating-point workspace for checks and reads.
!
! Locals, etc
!
CHARACTER (LEN=*) :: SYVER

!
!        -- SY module version number --
PARAMETER (SYVER = '4.2.7')
!        ------------------------------
!
!INTEGER :: ISACKW, ISGSED, ISSYOK, ISTEC, ISUSED, NEPS, NFINE, NSYB
!INTEGER :: NSYBCD (NSYBEE, 3), NSYC (4), NTSOBK (NLFEE)
!
!INTEGER :: PASS, NTSOTP (NELEE)
!
INTEGER :: FACE, FADJ, I, IADJ, IB, IBR, IEL, N, P, SED, SOIL
INTEGER :: IDUM1A (NELEE), IDUM1X (NELEE+3)
!
!DOUBLEPRECISION ALPHA, CONCOB, DCBEDO, FBIC, FICRIT, FPCRIT, SYNOW
!DOUBLEPRECISION DLSMAX, DDBSED (NLFEE, NSEDEE)
!DOUBLEPRECISION ABC (NSEDEE, NSYCEE), ACKW (5, NSEDEE), ARXLOL (NLFEE)
!DOUBLEPRECISION BBC (NSEDEE, NSYCEE), BKB (NSEE)
!DOUBLEPRECISION DBFULL (NLFEE)
!DOUBLEPRECISION DRDRIP (NVEE), DRSED (NSEDEE), DRSO50 (NSEE)
!DOUBLEPRECISION DWATOL (NELEE)
!DOUBLEPRECISION FCG (NELEE), FCROCK (NELEE), FDRIP (NVEE), FETA (NELEE)
!DOUBLEPRECISION FPCLAY (NSEE)
!DOUBLEPRECISION GBC (NSEDEE, NSYCEE), GKF (NSEE), GKR (NSEE)
!DOUBLEPRECISION RHOSO (NSEE), XDRIP (NVEE)
!
DOUBLEPRECISION DTSY
DOUBLEPRECISION CONCI (NLFEE, NSEDEE), CONCIE (NSEDEE)
DOUBLEPRECISION DCBSEE (NSEDEE), DCIPRE (NSEDEE)
DOUBLEPRECISION DCIPRM (NLFEE, NSEDEE), DDBSEE (NSEDEE)
DOUBLEPRECISION DDIPRE (NSEDEE), DDIPRM (NLFEE, NSEDEE)
DOUBLEPRECISION DRDROP (NELEE), DUMSED (NLFEE * NSEDEE), DWAT1 ( &
 NELEE)
DOUBLEPRECISION EPSB (NLFEE)
DOUBLEPRECISION FBETAE (NSEDEE), FCC (NVEE), FDELE (NSEDEE)
DOUBLEPRECISION FQCONF (NLFEE, 3), GINFDE (NSEDEE), GINFSE ( &
 NSEDEE)
DOUBLEPRECISION LRAIN (NELEE)
DOUBLEPRECISION QSDWAE (NSEDEE, 4), QSDWAT (NLFEE, NSEDEE, 4)
DOUBLEPRECISION QSEDB (NSEDEE, NSYBEE), QSEDE (NSEDEE, 4)
DOUBLEPRECISION QWAT (4), QWATB (NSYBEE)
DOUBLEPRECISION SLOPEE (4), SLOPEJ (NELEE, 4), SOSDFE (NSEDEE)
DOUBLEPRECISION TAUJ (NELEE, 4), TAUJE (4), TAUK (NELEE)
DOUBLEPRECISION VCFMAX (NLFEE), VINFMX (NLFEE)
!
LOGICAL :: DOUBT, BARM (NLFEE), LDUM (NELEE)
!
!----------------------------------------------------------------------*
!
PASS_symain = PASS_symain + 1
IF (PASS_symain.EQ.1) THEN
!
!                     ---------------------
!--------------------- Initialization step ----------------------------*
!                     ---------------------
!
!        * Check array bounds & input variables
   CALL SYERR0 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, NSEE, &
    NV, NVEE, NX, NXEE, NY, SPR, SYD)
!
!        * Check static/initializing input arrays
   CALL SYERR1 (NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, NXEE, NYEE, &
    NY, SPR, BEXBK, LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, &
    NTSOIL, NVC, THSAT, CLENTH, CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, &
    DHF, ARXL, HRF (NLF + 1), ZGRUND, IDUM, IDUM1X, LDUM)
!
!        * Store top-layer soil type for each column element
   DO 100 IEL = NLF + 1, NEL
      NTSOTP_symain (IEL) = NTSOIL (IEL, NLYR (IEL) )
  100    END DO
!
!        * Read SY input data file
   CALL SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, NELEE, &
    NLF, NLFEE, NS, NSEDEE, NSEE, NSYBEE, NSYCEE, NTSOTP_symain (NLF + 1), &
    NV, NX, NXEE, NYEE, NY, SPR, SYD, SYVER, ABC_symain, ALPHA_symain, BBC_symain, BKB_symain, &
    CONCOB_symain, DCBEDO_symain, DLS, DRDRIP_symain, DRSED_symain, DLSMAX_symain, FBETA, FBIC_symain, FCG_symain ( &
    NLF + 1), FCROCK_symain (NLF + 1), FDEL, FDRIP_symain, FICRIT_symain, FPCLAY_symain, &
    FPCRIT_symain, GBC_symain, GKF_symain, GKR_symain, ISACKW_symain, ISGSED_symain, ISSYOK_symain, ISTEC_symain, ISUSED_symain, &
    NEPS_symain, NFINE_symain, NSED, NSYB_symain, NSYBCD_symain, NSYC_symain, NTSOBK_symain, PBSED, PLS, &
    RHOSO_symain, SOSDFN, XDRIP_symain, IDUM, DUMMY, DUMSED)
!
!        * Check SY input data
   CALL SYERR2 (NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, &
    NV, NSYB_symain, NSYBEE, NSYC_symain, NSYCEE, SPR, ICMREF, ISUSED_symain, NEPS_symain, &
    NFINE_symain, SFB, SRB, ALPHA_symain, DCBEDO_symain, FPCRIT_symain, DLSMAX_symain, NTSOBK_symain, NSYBCD_symain, &
    NBFACE, DRSED_symain, BKB_symain, GKF_symain, GKR_symain, RHOSO_symain, SOSDFN, DRDRIP_symain, FDRIP_symain, &
    XDRIP_symain, PBSED, FCG_symain (NLF + 1), FCROCK_symain (NLF + 1), PLS, DLS, FBETA, &
    FDEL, ABC_symain, BBC_symain, GBC_symain, IDUM, DUMMY, LDUM)
!
!        * Static variables and initialization
   CALL SYINIT (NEL, NS, NSED, NSEE, NLF, NELEE, NSEDEE, NLFEE, &
    NTSOBK_symain, ARXL, DCBEDO_symain, DLS, FBETA, DRSED_symain, HRF (NLF + 1), &
    PBSED, PLS, SOSDFN, THSAT, ZGRUND, NTSOTP_symain (NLF + 1), ZBFULL, &
    ARBDEP, ARXLOL_symain, DCBED, DCBSED, DDBSED_symain, DRSO50_symain, DWATOL_symain (NLF + 1) &
    , FETA_symain, GINFD, GINFS, GNU, GNUBK, QSED, DBFULL_symain)
!
!
!------------------- End of initialization step -----------------------*
!
ELSE
!                      -----------------
!---------------------- Simulation step -------------------------------*
!                      -----------------
!
!
! Check Input
! -----------
!
!        * Check time-varying input variables
   DOUBT = ISSYOK_symain.GT.0
   IF (DOUBT) DOUBT = MOD (PASS_symain - 2, ISSYOK_symain) .EQ.0
   IF (DOUBT) CALL SYERR3 (NEL, NELEE, NLF, NLFEE, NV, SPR, &
    ICMREF, ICMRF2, ISORT, DTUZ, CLAI, PLAI, ARXL, DRAINA, PNETTO, &
    HRF, ZGRUND, QOC, IDUM, IDUM1A, IDUM1X, LDUM)
!
!
! Quantities Independent of Sub-timestep
! --------------------------------------
!
!        * Water-flow related variables

   CALL SYWAT (NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, &
    DHF, DRDRIP_symain, LINKNS, ZBFULL, ZGRUND, CLAI, DRAINA, HRF, PLAI, &
    PNETTO, QOC, DRDROP (NLF + 1), DWAT1, FCC, FQCONF, LRAIN (NLF + &
    1), SLOPEJ, TAUJ, TAUK)
!
!        * Erosion rates for all column elements

   CALL SYOVER (ISTEC_symain, NEL, NLF, NS, NV, FCC, LRAIN (NLF + 1), &
    XDRIP_symain, DRDRIP_symain, FDRIP_symain, DRAINA, GKR_symain, DWAT1 (NLF + 1), DRDROP ( &
    NLF + 1), FCG_symain (NLF + 1), FCROCK_symain (NLF + 1), DRSO50_symain, TAUK (NLF + &
    1), FPCLAY_symain, GKF_symain, RHOSO_symain, NTSOTP_symain (NLF + 1), NVC, GNU, DUMMY, DLS, &
    DLSMAX_symain)
!
!        * Erosion rates for all link elements

   IF (NLF.GT.0) CALL SYBKER (ISTEC_symain, NLF, NS, FPCLAY_symain, RHOSO_symain, &
    DRSO50_symain, TAUK, CWIDTH, DWAT1, BKB_symain, NTSOBK_symain, FETA_symain, CLENTH, DBFULL_symain, &
    EPSB, GNUBK)
!
!
!
! SY Sub-timestep Loop
! --------------------
!
   DTSY = DTUZ / NEPS_symain
   DO 290 N = 1, NEPS_symain
!
!
!           Initialization
!           --------------
!
      DO 150 FACE = 1, 4
         DO 140 SED = 1, NSED
            CALL ALINIT (ZERO, NEL, QSED (1, SED, FACE) )
  140          END DO
  150       END DO
!
!
!           Boundary Conditions
!           -------------------
!
      IF (NSYB_symain.GT.0) THEN
!
!              * Gather water "outflow" rates (should be negative)
         DO 210 IB = 1, NSYB_symain
            IEL = NSYBCD_symain (IB, 1)
            FACE = NBFACE (IEL)
            QWATB (IB) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
  210          END DO
!
!              * Read time-varying flux data & calculate sediment flows
         CALL SYBC
!
!              * Load boundary flows into QSED array
         DO 220 IB = 1, NSYB_symain
            IEL = NSYBCD_symain (IB, 1)
            FACE = NBFACE (IEL)
            CALL DCOPY (NSED, QSEDB (1, IB), 1, QSED (IEL, 1, &
             FACE), NELEE)
  220          END DO
!
      ENDIF
!
!
!           Quantities Independent of Sediment Flux
!           ---------------------------------------
!
      IF (NLF.GT.0) THEN
!
!              * Transport capacity & advection coefficients
         CALL SYCLTR (CONCOB_symain, FPCRIT_symain, ISACKW_symain, ISUSED_symain, NELEE, &
          NFINE_symain, NLF, NLFEE, NSED, NSEDEE, DRSED_symain (NFINE_symain+1), &
          ARXL, CWIDTH, DCBED, LINKNS, DWAT1, QOC, SLOPEJ, DCBSED ( &
          1, NFINE_symain+1), FDEL (1, NFINE_symain+1), TAUJ, ACKW_symain (1, NFINE_symain+1), &
          CONCI, QSDWAT, DUMMY, DUMSED)
!
!              * Settling, infiltration & armouring
         IF (NFINE_symain.GT.0) CALL SYFINE (DRSED_symain (1), FBIC_symain, FICRIT_symain, &
          NLF, ALPHA_symain, DTSY, AREA, DCBSED, FBETA, FDEL, PBSED, TAUK, &
          VCFMAX, VINFMX, BARM)
!
      ENDIF
!
!
!           One Element at a Time
!           ---------------------
!
      DO 270 I = 1, NEL
         IEL = ISORT (I)
!
!              * Gather common sub-arrays
         CALL DCOPY (NSED, FDEL (IEL, 1), NELEE, FDELE, 1)
         DO 225 FACE = 1, 4
            QWAT (FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
            CALL DCOPY (NSED, QSED (IEL, 1, FACE), NELEE, QSEDE ( &
             1, FACE), 1)
  225          END DO
!
         IF (IEL.LE.NLF) THEN
!
!                 ** Link element **
!
!                 * Gather link-specific sub-arrays
            SOIL = NTSOBK_symain (IEL)
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)
            CALL DCOPY (NSED, CONCI (IEL, 1), NLFEE, CONCIE, 1)
            CALL DCOPY (NSED, DCBSED (IEL, 1), NLFEE, DCBSEE, 1)
            CALL DCOPY (NSED, DDBSED_symain (IEL, 1), NLFEE, DDBSEE, 1)
            DO 226 FACE = 1, 4
               CALL DCOPY (NSED, QSDWAT (IEL, 1, FACE), NLFEE, &
                QSDWAE (1, FACE), 1)
  226             END DO
!
!                 * Solve transport equation
            CALL SYLINK (NFINE_symain, NSED, NSEDEE, DTSY, AREA (IEL), &
             ARXLOL_symain (IEL), ARXL (IEL), CLENTH (IEL), EPSB (IEL), &
             PBSED (IEL), VINFMX (IEL), BARM (IEL), VCFMAX (IEL), &
             CONCIE, DCBSEE, DDBSEE, QSDWAE, QWAT, SOSDFE, FDELE, &
             QSEDE, DCIPRE, DDIPRE, GINFDE, GINFSE)
!
!                 * Scatter link-specific results
            CALL DCOPY (NSED, DCIPRE, 1, DCIPRM (IEL, 1), NLFEE)
            CALL DCOPY (NSED, DDIPRE, 1, DDIPRM (IEL, 1), NLFEE)
            CALL DCOPY (NSED, GINFDE, 1, GINFD (IEL, 1), NLFEE)
            CALL DCOPY (NSED, GINFSE, 1, GINFS (IEL, 1), NLFEE)
!
         ELSE
!
!                 ** Column element **
!
!                 * Gather column-specific sub-arrays
            SOIL = NTSOTP_symain (IEL)
            CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, SOSDFE, 1)
            CALL DCOPY (NSED, FBETA (IEL, 1), NELEE, FBETAE, 1)
            CALL DCOPY (4, SLOPEJ (IEL, 1), NELEE, SLOPEE, 1)
            CALL DCOPY (4, TAUJ (IEL, 1), NELEE, TAUJE, 1)
!
!                 * Solve transport equation for this column element
            CALL SYCOLM (AREA (IEL), DTSY, DWAT1 (IEL), DWATOL_symain ( &
             IEL), DXQQ (IEL), DYQQ (IEL), FETA_symain (IEL), GNU (IEL), &
             ISGSED_symain, NSED, FPCRIT_symain, PLS (IEL), NSEDEE, DRSED_symain, QWAT, &
             SLOPEE, SOSDFE, TAUJE, DLS (IEL), FBETAE, FDELE, &
             QSEDE, DUMMY, DUMSED)
!
!                 * Scatter column-specific results
            CALL DCOPY (NSED, FBETAE, 1, FBETA (IEL, 1), NELEE)
!
         ENDIF
!
!              * Scatter common results ...
         CALL DCOPY (NSED, FDELE, 1, FDEL (IEL, 1), NELEE)
         DO 260 FACE = 1, 4
            CALL DCOPY (NSED, QSEDE (1, FACE), 1, QSED (IEL, 1, &
             FACE), NELEE)
!
!                 ... and propagate sediment flow rates at outflow faces
            IF (QWAT (FACE) .GT.ZERO) THEN
               IADJ = ICMREF (IEL, FACE, 2)
               IF (IADJ.GT.0) THEN
!                       * regular neighbour
                  FADJ = ICMREF (IEL, FACE, 3)
                  DO 240 SED = 1, NSED
                     QSED (IADJ, SED, FADJ) = - QSEDE (SED, FACE)
  240                   END DO
               ELSEIF (IADJ.LT.0) THEN
!                       * neighbour is a confluence node
                  IBR = - IADJ
                  DO 255 P = 1, 3
                     IADJ = ICMRF2 (IBR, P, 1)
                     IF (IADJ.GT.0) THEN
!                             * prospect is active
                        FADJ = ICMRF2 (IBR, P, 2)
                        DO 250 SED = 1, NSED
                           QSED (IADJ, SED, FADJ) = QSED (IADJ, &
                            SED, FADJ) - QSEDE (SED, FACE) * &
                            FQCONF (IBR, P)
  250                         END DO
                     ENDIF
  255                   END DO
               ENDIF
            ENDIF
!
  260          END DO
!
  270       END DO
!
!
!           Channel Bed Update
!           ------------------
!
      IF (NLF.GT.0) CALL SYBED (DCBEDO_symain, NELEE, NLF, NLFEE, NSED, &
       CWIDTH, DCIPRM, DDIPRM, ARBDEP, DLS, FBETA, DCBSED, DDBSED_symain, &
       DCBED)
!
!
!           Store Old-time Values & Update Timer
!           ------------------------------------
!
      CALL DCOPY (NEL - NLF, DWAT1 (NLF + 1), 1, DWATOL_symain (NLF + 1), &
       1)
      IF (NLF.GT.0) CALL DCOPY (NLF, ARXL, 1, ARXLOL_symain, 1)
      SYNOW_symain = SYNOW_symain + DTSY / 36D2
!
  290    END DO
!
!
!--------------------- End of simulation step -------------------------*
!
ENDIF
!
!
! Epilogue
! --------
!
!     Ensure that current time value is exactly correct
SYNOW_symain = UZNOW
!
!
END SUBROUTINE SYMAIN



!> Calculates ground-surface erosion for land elements.
!>
!> `SYOVER` calculates the hillslope soil detachment source `GNU` for each land
!> element. It combines detachment by direct rainfall/canopy drip with
!> detachment by overland-flow shear, then converts from mass detachment to an
!> equivalent soil-depth erosion rate using `RHOSO`.
!>
!> For each vegetation type, the drip momentum term is precomputed from canopy
!> drip fall height `XDRIP`, drip diameter `DRDRIP`, and drip fraction `FDRIP`.
!> The empirical drip coefficient is selected from the classes
!> `XDRIP < 7.5 m` / `>= 7.5 m` and `DRDRIP < 3.3e-3 m` / `>= 3.3e-3 m`:
!>
!> \[
!>   C_D = ADD_i + BDD_i\,DRDRIP,
!> \]
!>
!> \[
!>   TGMD =
!>   {\pi\rho_w^2g\over 6}
!>   C_D\left(1-\exp(-2XDRIP/C_D)\right)DRDRIP^3FDRIP.
!> \]
!>
!> For each land element, the direct rainfall momentum class is selected from
!> `LRAIN` using thresholds \(2.78\times10^{-6}\), \(1.39\times10^{-5}\), and
!> \(2.78\times10^{-5}\). With class coefficients `AD_i` and `BD_i`,
!>
!> \[
!>   GMR = (1-FCC)\,AD_i\,LRAIN^{BD_i},\qquad
!>   GMD = TGMD\,DRAINA .
!> \]
!>
!> Rainfall/drip detachment is reduced by surface water depth, ground cover, and
!> rock cover:
!>
!> \[
!>   D_R =
!>   GKR\,
!>   \exp\left[-\max(DWAT1/DRDROP-1,0)\right]\,
!>   (1-FCG-FCROCK)(GMR+GMD).
!> \]
!>
!> Overland-flow detachment uses [[sycrit]] with the manual `ISTEC` switch to
!> obtain \(\tau_c\) from soil median diameter `DRSO50` and clay fraction
!> `FPCLAY`, then computes
!>
!> \[
!>   D_F = GKF(1-FCROCK){\max(TAUK-\tau_c,0)\over\tau_c}.
!> \]
!>
!> If the current loose-sediment depth is below `DLSMAX`,
!>
!> \[
!>   GNU = {D_R + D_F\over RHOSO};
!> \]
!>
!> otherwise the underlying soil is treated as protected and `GNU` is set to
!> zero, matching the manual definition of `DLSMAX`.
!>
!> @note The internal class switch treats equality with a threshold as belonging
!> to the higher class. The `DLSMAX` test is also a hard cutoff: `DLS < DLSMAX`
!> permits erosion, while `DLS >= DLSMAX` sets `GNU` to zero.
!> @endnote
SUBROUTINE SYOVER (ISTEC, NEL, NLF, NS, NV, FCC, LRAIN, XDRIP, &
 DRDRIP, FDRIP, DRAINA, GKR, DWAT1, DRDROP, FCG, FCROCK, DRSO50, &
 TAUK, FPCLAY, GKF, RHOSO, NTSOTP, NVC, GNU, TGMD, DLS, DLSMAX)

USE CONST_SY

INTEGER :: ISTEC                   !! Critical-shear calculation option.
INTEGER :: NEL                     !! Number of elements.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NS                      !! Number of soil types.
INTEGER :: NV                      !! Number of vegetation types.
INTEGER :: NTSOTP (NLF + 1:NEL)    !! Top soil type by land element.
INTEGER :: NVC (NLF + 1:NEL)       !! Vegetation type by land element.
DOUBLEPRECISION FCC (NV)           !! Canopy/ground sheltering fraction by vegetation type.
DOUBLEPRECISION LRAIN (NLF + 1:NEL) !! Effective direct rainfall rate by land element.
DOUBLEPRECISION XDRIP (NV)         !! Canopy drip fall height by vegetation type.
DOUBLEPRECISION DRDRIP (NV)        !! Canopy drip drop diameter by vegetation type.
DOUBLEPRECISION FDRIP (NV)         !! Canopy drip fraction by vegetation type.
DOUBLEPRECISION DRAINA (NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
DOUBLEPRECISION GKR (NS)           !! Rainfall detachment coefficient by soil type.
DOUBLEPRECISION DWAT1 (NLF + 1:NEL) !! Surface water depth by land element.
DOUBLEPRECISION DRDROP (NLF + 1:NEL) !! Effective raindrop/drop diameter by land element.
DOUBLEPRECISION FCG (NLF + 1:NEL)  !! Ground-cover fraction by land element.
DOUBLEPRECISION FCROCK (NLF + 1:NEL) !! Rock-cover fraction by land element.
DOUBLEPRECISION DRSO50 (NS)        !! Median soil particle diameter by soil type.
DOUBLEPRECISION TAUK (NLF + 1:NEL) !! Overland-flow shear stress by land element.
DOUBLEPRECISION FPCLAY (NS)        !! Clay fraction by soil type.
DOUBLEPRECISION GKF (NS)           !! Flow detachment coefficient by soil type.
DOUBLEPRECISION RHOSO (NS)         !! Soil bulk density by soil type.
DOUBLEPRECISION DLS (NEL)          !! Loose-sediment depth by element.
DOUBLEPRECISION DLSMAX             !! Loose-sediment depth above which soil erosion is suppressed.
DOUBLEPRECISION GNU (NLF + 1:NEL)  !! Hillslope erosion rate by land element.
DOUBLEPRECISION TGMD (NV)          !! Workspace for canopy-drip momentum by vegetation type.
!
! Locals, etc
DOUBLEPRECISION CLALIM, D1, L1, L2, X1
PARAMETER (X1 = 7.5D0, D1 = 3.3D-3, L1 = 2.78D-6, L2 = 1.39D-5)
PARAMETER (CLALIM = 1.0d0 / L2)
!
INTEGER :: ISCD, IEL, ISGMR, ISOIL, NVEG
DOUBLEPRECISION AD (4), ADD (4), BD (4), BDD (4), CD, FCROCE, &
 DRDRPE, DR, DF
DOUBLEPRECISION LRAINE, GMD, GMR, PRSGOS, TAUEC, TAUKE, XDRIPE
DOUBLEPRECISION SF2, SX, SY
!
! Define coefficients for use in calculating GMR and CD, respectively.
DATA AD / 3214.9, 583.4, 133.1, 29.9 /, BD / 1.6896, 1.5545, &
 1.4242, 1.2821 /
!       Class 1: 0.0  <= LRAIN < L1
!       Class 2: L1   <= LRAIN < L2
!       Class 3: L2   <= LRAIN < 2*L2
!       Class 4: 2*L2 <= LRAIN
DATA ADD / 0.0d0, 0.0d0, 1.93d0, 5.14d0 /, BDD / 2200.0d0, 2200.0d0, 1640.0d0, &
 660.0d0 /
!       Class 1: DRDRIP <  D1    XDRIP <  X1
!       Class 2: DRDRIP <  D1    XDRIP >= X1
!       Class 3: DRDRIP >= D1    XDRIP <  X1
!       Class 4: DRDRIP >= D1    XDRIP >= X1
!
!     * Define the switch function, used in calculating CD and GMR.
SF2 (SX, SY) = HALF + SIGN (HALF, SX - SY)
!
!----------------------------------------------------------------------*
!
!     * Initialize constant
PRSGOS = 4.0 * ATAN (ONE) * RHOWAT * RHOWAT * GRAVTY / 6.0
!
!     * Partial evaluation of GMD for each vegetation type
DO 100 NVEG = 1, NV
   XDRIPE = XDRIP (NVEG)
   DRDRPE = DRDRIP (NVEG)
!        * Select coefficient pair for CD equation
   ISCD = 1 + NINT (SF2 (XDRIPE, X1) + 2 * SF2 (DRDRPE, D1) )
   CD = ADD (ISCD) + DRDRPE * BDD (ISCD)
!        * Need precondition on DRDRIP to ensure CD>0
   TGMD (NVEG) = PRSGOS * CD * (ONE-EXP ( - 2.0 * XDRIPE / CD) ) &
    * DRDRPE**3 * FDRIP (NVEG)
  100 END DO
!
!     * Loop over all column elements
DO 200 IEL = NLF + 1, NEL
   ISOIL = NTSOTP (IEL)
   NVEG = NVC (IEL)
   LRAINE = LRAIN (IEL)
   FCROCE = FCROCK (IEL)
   TAUKE = TAUK (IEL)
!
!        * Select coefficient pair for GMR equation
   ISGMR = MIN (4, 1 + NINT (SF2 (LRAINE, L1) ) + INT (LRAINE * &
    CLALIM) )
!        * Evaluate sq momentum of rain drops
   GMR = (ONE-FCC (NVEG) ) * AD (ISGMR) * LRAINE**BD (ISGMR)
!
!        * Evaluate sq momentum of leaf drips
   GMD = TGMD (NVEG) * DRAINA (IEL)
!
!        * Evaluate soil detatchment rate due to drips and drops
   DR = GKR (ISOIL) * EXP ( - DIMJE(DWAT1 (IEL) / DRDROP (IEL), &
    ONE) ) * (ONE-FCG (IEL) - FCROCE) * (GMR + GMD)
!
!        * Obtain critical shear stress for current element
   CALL SYCRIT (ISTEC, DRSO50 (ISOIL), TAUKE, FPCLAY (ISOIL), &
    TAUEC)
!
!        * Evaluate soil detatchment rate due to overland flow
   DF = GKF (ISOIL) * (ONE-FCROCE) * DIMJE(TAUKE, TAUEC) / TAUEC
!
!        * Evaluate rate of erosion of ground surface
   If (DLS (IEL) .lt.DLSMAX) then
      GNU (IEL) = (DR + DF) / RHOSO (ISOIL)
   else
      GNU (IEL) = zero
   endif
!
  200 END DO
!
END SUBROUTINE SYOVER



!> Calculates overland-flow sediment transport capacity for one element.
!>
!> `SYOVTR` implements the manual's overland transport-capacity switch
!> `ISGSED` for the current land element. The manual notes that both available
!> formulae were derived for non-cohesive channel transport and their use for
!> rainfall-driven overland flow is uncertain; [[sycolm]] applies the additional
!> total concentration cap `FPCRIT` after this routine returns.
!>
!> The representative particle diameter is the median diameter of the currently
!> available sediment mix:
!>
!> \[
!>   d_{50} = [[sydr]](0.5, VDSED, DRSED).
!> \]
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
SUBROUTINE SYOVTR (DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, &
 DRSED, QWAT, SLOPEE, TAUJE, GJSUM)

USE CONST_SY

INTEGER :: ISGSED                  !! Overland transport-capacity option.
INTEGER :: NSED                    !! Number of sediment size classes.
DOUBLEPRECISION DXQQE              !! Element width.
DOUBLEPRECISION DYQQE              !! Element length.
DOUBLEPRECISION DWAT1E             !! Surface water depth.
DOUBLEPRECISION VDSED (NSED)       !! Available sediment volume/fraction by size class.
DOUBLEPRECISION DRSED (NSED)       !! Representative particle diameters by size class.
DOUBLEPRECISION K2                 !! Yalin formula coefficient.
DOUBLEPRECISION QWAT (4)           !! Outward water flux by face.
DOUBLEPRECISION SLOPEE (4)         !! Water-surface slope by face.
DOUBLEPRECISION TAUJE (4)          !! Face shear stress.
DOUBLEPRECISION GJSUM              !! Total overland transport capacity over outflowing faces.
!
! Locals, etc
!
DOUBLEPRECISION FLJ
DOUBLEPRECISION AJ, DRD50, FTAU, DUM, DYMXQQ, GJ, GSUM
DOUBLEPRECISION LJ, TAUEC, TAUJEE
INTEGER :: FACE, NOUT, I, J (4)

!
!     * Face length function ( DXQQE at evens, DYQQE at odds )
FLJ (FACE) = MOD (FACE, 2) * DYMXQQ + DXQQE
!
!----------------------------------------------------------------------*
!
! Preliminaries
! -------------
!
!     * Constants
IF (FIRST_syovtr) THEN
   K1_syovtr = 0.05d0 * RHOWAT**2 / ( (RHOSED-RHOWAT) **2 * SQRT (GRAVTY) )
   K3_syovtr = 2.45d0 * (RHOSED / RHOWAT) ** ( - 0.4d0) / SQRT ( (RHOSED- &
    RHOWAT) * GRAVTY)
   K4_syovtr = 0.635d0 / SQRT (RHOWAT)
   FIRST_syovtr = .FALSE.
ENDIF
!
!     * Initialize variables
GSUM = 0
DYMXQQ = DYQQE-DXQQE
!
!     * Obtain median diameter of sediment available for discharge
DRD50 = SYDR (HALF, 1, NSED, VDSED, DRSED)
!
!     * Count and record faces with outflow
NOUT = 0
DO 100 FACE = 1, 4
   IF (QWAT (FACE) .GT.0) THEN
      NOUT = NOUT + 1
      J (NOUT) = FACE
   ENDIF
  100 END DO
!
!
! Transport Capacity
! ------------------
!
IF (ISGSED.EQ.1.AND.DWAT1E.GT.0) THEN
!
!
!        ^^^ ENGELUND-HANSEN METHOD ^^^
!
!        * Precalculate constant over faces (note K2 may be very small)
   K2 = SQRT (DWAT1E) * DRD50
!
!        * Loop over faces with outflow
   DO 200 I = 1, NOUT
      FACE = J (I)
!
!           * Discharge capacity at this face
      LJ = FLJ (FACE)
      GJ = (K1_syovtr * QWAT (FACE) **2 * SLOPEE (FACE) **1.5) / (LJ * &
       K2)
!
!           * Accumulated discharge capacity for this element
      GSUM = GSUM + GJ
!
  200    END DO
!
!
ELSEIF (ISGSED.EQ.0) THEN
!
!
!        ^^^^^^^ YALIN METHOD ^^^^^^^^^
!
!        * Loop over faces with outflow
   DO 300 I = 1, NOUT
      FACE = J (I)
!
!           * Get face length
      LJ = FLJ (FACE)
!
!           * Obtain critical shear stress at the ground surface
      TAUJEE = TAUJE (FACE)
      CALL SYCRIT (0, DRD50, TAUJEE, DUM, TAUEC)
!
!           * Calculate discharge capacity at this face
      FTAU = DIMJE(TAUJEE, TAUEC) / TAUEC
      AJ = K3_syovtr * SQRT (TAUEC / DRD50)
      GJ = K4_syovtr * SQRT (TAUJEE) * DRD50 * LJ * (FTAU - LOG (1 + AJ * &
       FTAU) / AJ)
!
!           * Accumulated capacity for this element
      GSUM = GSUM + GJ
!
  300    END DO
!
!
ELSE
!
!        ^^^ Zero capacity ^^^
!
ENDIF
!
GJSUM = GSUM
!
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
SUBROUTINE SYREAD (BEXBK, ICMBK, ICMREF, ICMXY, LINKNS, NEL, &
 NELEE, NLF, NLFEE, NS, NSEDEE, NSEE, NSYBEE, NSYCEE, NTSOTP, NV, &
 NX, NXEE, NYEE, NY, SPR, SYD, SYVER, ABC, ALPHA, BBC, BKB, CONCOB, &
 DCBEDO, DLS, DRDRIP, DRSED, DLSMAX, FBETA, FBIC, FCG, FCROCK, &
 FDEL, FDRIP, FICRIT, FPCLAY, FPCRIT, GBC, GKF, GKR, ISACKW, &
 ISGSED, ISSYOK, ISTEC, ISUSED, NEPS, NFINE, NSED, NSYB, NSYBCD, &
 NSYC, NTSOBK, PBSED, PLS, RHOSO, SOSDFN, XDRIP, IDUM, DUMMY, &
 DUMSED)

!
! NB: Don't dimension arrays with NSED (undefined) or NLF (may be 0).
!
INTEGER :: NEL                     !! Number of elements.
INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NS                      !! Number of soil types.
INTEGER :: NSEDEE                  !! Sediment-size array dimension.
INTEGER :: NSEE                    !! Soil-type array dimension.
INTEGER :: NSYBEE                  !! Sediment-boundary array dimension.
INTEGER :: NSYCEE                  !! Sediment-boundary-category array dimension.
INTEGER :: NTSOTP (NLF + 1:NEL)    !! Top soil type by land element.
INTEGER :: NV                      !! Number of vegetation types.
INTEGER :: NX                      !! Number of grid columns.
INTEGER :: NXEE                    !! Grid-column array dimension.
INTEGER :: NYEE                    !! Grid-row workspace dimension.
INTEGER :: NY                      !! Number of grid rows.
INTEGER :: SYD                     !! Static sediment input unit.
INTEGER :: SPR                     !! Sediment diagnostic output unit.
INTEGER :: ICMBK (NLFEE, 2)        !! Bank-element numbers for each channel link.
INTEGER :: ICMREF (NELEE, 4, 2:2)  !! Face-neighbour reference map.
INTEGER :: ICMXY (NXEE, NY)        !! Element number at each grid location.
LOGICAL :: BEXBK                   !! True when bank elements are represented.
LOGICAL :: LINKNS (NLFEE)          !! True for north-south channel links.
CHARACTER (LEN=*) :: SYVER         !! Expected sediment input-file version string.
INTEGER :: ISACKW                  !! Channel transport-capacity option.
INTEGER :: ISGSED                  !! Overland transport-capacity option.
INTEGER :: ISSYOK                  !! Dynamic sediment input-check interval.
INTEGER :: ISTEC                   !! Critical-shear calculation option.
INTEGER :: ISUSED                  !! Sediment velocity option.
INTEGER :: NEPS                    !! Number of sediment substeps per water timestep.
INTEGER :: NFINE                   !! Number of fine sediment classes.
INTEGER :: NSED                    !! Number of sediment size classes.
INTEGER :: NSYB                    !! Number of sediment boundary entries.
INTEGER :: NSYBCD (NSYBEE, 3)      !! Sediment boundary element, type, and category metadata.
INTEGER :: NSYC (4)                !! Number of sediment boundary categories by boundary type.
INTEGER :: NTSOBK (NLFEE)          !! Bank soil type by link.
DOUBLEPRECISION ABC (NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `A`.
DOUBLEPRECISION ALPHA              !! Fine-sediment settling/resuspension critical-shear ratio.
DOUBLEPRECISION BBC (NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `B`.
DOUBLEPRECISION BKB (NS)           !! Bank erodibility by soil type.
DOUBLEPRECISION CONCOB             !! Mobile concentration threshold for overbank exchange.
DOUBLEPRECISION DCBEDO             !! Active upper channel-bed layer thickness.
DOUBLEPRECISION DLS (NEL)          !! Initial loose/bed sediment depth by element.
DOUBLEPRECISION DRDRIP (NV)        !! Canopy drip drop diameter by vegetation type.
DOUBLEPRECISION DRSED (NSEDEE)     !! Representative sediment particle diameters.
DOUBLEPRECISION FBETA (NELEE, NSEDEE) !! Initial sediment composition by element and size class.
DOUBLEPRECISION FBIC               !! Fine-bed fraction threshold for infiltration.
DOUBLEPRECISION FCG (NLF + 1:NEL)  !! Ground-cover fraction by land element.
DOUBLEPRECISION FCROCK (NLF + 1:NEL) !! Rock-cover fraction by land element.
DOUBLEPRECISION FDEL (NELEE, NSEDEE) !! Initial mobile sediment concentration fraction.
DOUBLEPRECISION FDRIP (NV)         !! Canopy drip fraction by vegetation type.
DOUBLEPRECISION FICRIT             !! Fine-concentration threshold for infiltration.
DOUBLEPRECISION FPCLAY (NS)        !! Clay fraction by soil type.
DOUBLEPRECISION FPCRIT             !! Maximum sediment concentration fraction.
DOUBLEPRECISION GBC (NSEDEE, NSYCEE) !! Steady boundary sediment input by class/category.
DOUBLEPRECISION GKF (NS)           !! Flow detachment coefficient by soil type.
DOUBLEPRECISION GKR (NS)           !! Rainfall detachment coefficient by soil type.
DOUBLEPRECISION PBSED (NLFEE)      !! Channel-bed sediment porosity by link.
DOUBLEPRECISION PLS (NLF + 1:NEL)  !! Loose-sediment porosity by land element.
DOUBLEPRECISION RHOSO (NS)         !! Soil bulk density by soil type.
DOUBLEPRECISION SOSDFN (NSEE, NSEDEE) !! Soil sediment-size fractions by soil type.
DOUBLEPRECISION XDRIP (NV)         !! Canopy drip fall height by vegetation type.
DOUBLEPRECISION DLSMAX             !! Loose-sediment depth above which hillslope soil erosion is suppressed.
INTEGER, DIMENSION(NXEE*NYEE) :: IDUM !! Integer workspace for distributed reads.
DOUBLEPRECISION DUMMY (NELEE)      !! Floating-point workspace for distributed reads.
DOUBLEPRECISION DUMSED (NLFEE * NSEDEE) !! Flattened sediment-size workspace for distributed reads.
!
! Locals, etc
INTEGER :: FATAL, WARN
PARAMETER (FATAL = 1, WARN = 3)
!
CHARACTER(80)  :: CDUM
CHARACTER(132) :: MSG
CHARACTER(8)   ::  SYDVER
INTEGER :: BB, IDUM0, I0, IEL, ICAT, ITYPE, NC, NUM_CATEGORIES_TYPES,  NNN, NREQ, &
 SED, SOIL
!
!----------------------------------------------------------------------*
!
!
! 0. Preliminaries
! ----------------
!
!     * Check status of data file
CALL ALREAD (0, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, DUMMY)
!
!     * Print SY job title
CALL ALREAD (1, SYD, SPR, ':SY01', 1, 1, IDUM0, CDUM, IDUM, DUMMY)
WRITE (SPR, '(/1X,A/)') CDUM
!
!     * Check & print version number
CALL ALREAD (1, SYD, SPR, ':SY02', 1, 1, IDUM0, SYDVER, IDUM, &
 DUMMY)
!     * [miss off last character to allow eg '3.4.1' is ok in '3.4.1a' ]
IF (INDEX (SYDVER, SYVER (:LEN (SYVER) - 1) ) .EQ.0) THEN
   WRITE (MSG, 9011) SYVER, SYDVER
   CALL ERROR (WARN, 2011, SPR, 0, 0, MSG)
ELSE
   WRITE (SPR, '(4X,2A/)') 'SY Module Version ', SYVER
ENDIF
!
!
! 1. Static Variables
! -------------------
!
!     * Check workspace array size: part 1
NREQ = 8
IF (NELEE.LT.NREQ) GOTO 8000
!
!     * Integer
NNN = 5
IF (NLF.GT.0) NNN = 8
CALL ALREAD (2, SYD, SPR, ':SY11', NNN, 1, IDUM0, CDUM, IDUM, &
 DUMMY)
NSED = IDUM (1)
ISGSED = IDUM (2)
ISTEC = IDUM (3)
ISSYOK = IDUM (4)
NEPS = IDUM (5)
IF (NLF.GT.0) THEN
   ISACKW = IDUM (6)
   ISUSED = IDUM (7)
   NFINE = IDUM (8)
ENDIF
IF (NSED.LT.1.OR.NSED.GT.NSEDEE) GOTO 8110
!
!     * Floating-point
NNN = 2
IF (NLF.GT.0) NNN = 7
CALL ALREAD (3, SYD, SPR, ':SY12', NNN, 1, IDUM0, CDUM, IDUM, &
 DUMMY)
FPCRIT = DUMMY (1)
DLSMAX = DUMMY (2)
IF (NLF.GT.0) THEN
   ALPHA = DUMMY (3)
   CONCOB = DUMMY (4)
   DCBEDO = DUMMY (5)
   FBIC = DUMMY (6)
   FICRIT = DUMMY (7)
ENDIF
!
!
! 2. Sediment, Soil & Vegetation Properties
! -----------------------------------------
!
!     * Check workspace array size: part 2
NREQ = MAX (MAX (5, NSED) * NS, 3 * NV)
IF (NELEE.LT.NREQ) GOTO 8000
!
!     * Sediment
CALL ALREAD (3, SYD, SPR, ':SY21', NSED, 1, IDUM0, CDUM, IDUM, &
 DRSED)
!
!     * Soil
CALL ALREAD (3, SYD, SPR, ':SY22', 5, NS, IDUM0, CDUM, IDUM, &
 DUMMY)
CALL DCOPY (NS, DUMMY (1), 5, GKR, 1)
CALL DCOPY (NS, DUMMY (2), 5, GKF, 1)
CALL DCOPY (NS, DUMMY (3), 5, RHOSO, 1)
CALL DCOPY (NS, DUMMY (4), 5, FPCLAY, 1)
CALL DCOPY (NS, DUMMY (5), 5, BKB, 1)
!
!     * Soil composition
CALL ALREAD (3, SYD, SPR, ':SY23', NSED, NS, IDUM0, CDUM, IDUM, &
 DUMMY)
DO 200 SED = 1, NSED
   CALL DCOPY (NS, DUMMY (SED), NSED, SOSDFN (1, SED), 1)
  200 END DO
!
!     * Vegetation
CALL ALREAD (3, SYD, SPR, ':SY24', 3, NV, IDUM0, CDUM, IDUM, &
 DUMMY)
CALL DCOPY (NV, DUMMY (1), 3, XDRIP, 1)
CALL DCOPY (NV, DUMMY (2), 3, DRDRIP, 1)
CALL DCOPY (NV, DUMMY (3), 3, FDRIP, 1)
!
!
! 3. Link Element Properties
! --------------------------
!
IF (NLF.GT.0) THEN
!
!        * Bank soil type
   CALL ALREAD (2, SYD, SPR, ':SY31', NLF, 1, IDUM0, CDUM, NTSOBK, &
    DUMMY)
!
!        * Porosity of bed sediment
   CALL ALREAD (3, SYD, SPR, ':SY32', NLF, 1, IDUM0, CDUM, IDUM, &
    PBSED)
!
ENDIF
!
!
! 4. Column-element Properties
! ----------------------------
!
!     * Ground cover
CALL ALALLF (1, 1, 0, SYD, SPR, ':SY41', NEL, NLF, NX, NY, NELEE, &
 NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  FCG, IDUM, &
 DUMMY)
!
!     * Rock cover
CALL ALALLF (1, 1, 0, SYD, SPR, ':SY42', NEL, NLF, NX, NY, NELEE, &
 NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  FCROCK, &
 IDUM, DUMMY)
!
!     * Porosity of loose sediment
CALL ALALLF (1, 1, 0, SYD, SPR, ':SY43', NEL, NLF, NX, NY, NELEE, &
 NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  PLS, IDUM, &
 DUMMY)
!
!
! 5. All-element Initialization
! -----------------------------
!
!     * Initial depth of loose/bed sediment
CALL ALALLF (0, 1, 0, SYD, SPR, ':SY51', NEL, NLF, NX, NY, NELEE, &
 NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  DLS, IDUM, &
 DUMMY)
!
!     * Initial composition of loose/bed sediment ...
CALL ALALLF (0, NSED, - 1, SYD, SPR, ':SY52', NEL, NLF, NX, NY, &
 NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  &
 FBETA, IDUM, DUMMY)
!
!     ... with special option to inherit composition of soil
IF (NUM_CATEGORIES_TYPES .LT.0) THEN
   DO 510 IEL = 1, NLF
      SOIL = NTSOBK (IEL)
      CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), &
       NELEE)
  510    END DO
   DO 520 IEL = NLF + 1, NEL
      SOIL = NTSOTP (IEL)
      CALL DCOPY (NSED, SOSDFN (SOIL, 1), NSEE, FBETA (IEL, 1), &
       NELEE)
  520    END DO
ENDIF
!
!     * Initial concentrations of suspended sediment
CALL ALALLF (0, NSED, 0, SYD, SPR, ':SY53', NEL, NLF, NX, NY, &
 NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  &
 FDEL, IDUM, DUMMY)
!
!
! 6. Boundary Data
! ----------------
!
!     * (see workspace check above)
!
!     * No of inflow boundary elements & no of categories of each type
CALL ALREAD (2, SYD, SPR, ':SY61', 5, 1, IDUM0, CDUM, IDUM, DUMMY)
NSYB = IDUM (1)
DO 600 ITYPE = 1, 4
   NSYC (ITYPE) = IDUM (1 + ITYPE)
  600 END DO
!
IF (NSYB.GT.0) THEN
!
   IF (NSYB.GT.NSYBEE) GOTO 8610
!
!        * Check workspace array size: part 3
   NREQ = MAX (3 * NSYB, NSED * NSYC (1), NSED * 2 * NSYC (3) )
   IF (NELEE.LT.NREQ) GOTO 8000
!
!        * Integer boundary data
   CALL ALREAD (2, SYD, SPR, ':SY62', 3, NSYB, IDUM0, CDUM, IDUM, &
    DUMMY)
   I0 = 0
   DO 610 BB = 1, NSYB
      IEL = IDUM (I0 + 1)
      ITYPE = IDUM (I0 + 2)
      ICAT = IDUM (I0 + 3)
      IF (ITYPE.LT.1.OR.ITYPE.GT.4) GOTO 8620
!           * condense 4 into 2 by adding cats 2 & 4 to lists for 1 & 3
      IF (MOD (ITYPE, 2) .EQ.0) ICAT = ICAT + NSYC (ITYPE-1)
      NSYBCD (BB, 1) = IEL
      NSYBCD (BB, 2) = ITYPE
      NSYBCD (BB, 3) = ICAT
      I0 = I0 + 3
  610    END DO
!
!        * Steady flux data
   NC = NSYC (1)
   IF (NC.GT.0) THEN
      IF (NC.GT.NSYCEE) GOTO 8612
      CALL ALREAD (3, SYD, SPR, ':SY63', NSED, NC, IDUM0, CDUM, &
       IDUM, DUMMY)
      DO 620 SED = 1, NSED
         CALL DCOPY (NC, DUMMY (SED), NSED, GBC (SED, 1), NSEDEE)
  620       END DO
   ENDIF
!
!        * Steady rating curve data
   NC = NSYC (3)
   IF (NC.GT.0) THEN
      IF (NC.GT.NSYCEE) GOTO 8614
      CALL ALREAD (3, SYD, SPR, ':SY64', NSED * 2, NC, IDUM0, &
       CDUM, IDUM, DUMMY)
      DO 630 SED = 1, NSED
         CALL DCOPY (NC, DUMMY (2 * SED-1), 2 * NSED, ABC (SED, 1) &
          , NSEDEE)
         CALL DCOPY (NC, DUMMY (2 * SED), 2 * NSED, BBC (SED, 1), &
          NSEDEE)
  630       END DO
   ENDIF
!
ENDIF
!
!
! 7. Epilogue
! -----------
!
!     * Close the data file
CALL ALREAD ( - 1, SYD, SPR, 'SYD', 1, 1, IDUM0, CDUM, IDUM, &
 DUMMY)
!
RETURN
!
!
! Error Branches & Formats
! ------------------------
!
!     * Insufficient workspace
 8000 WRITE (MSG, 9005) NELEE, NREQ
CALL ERROR (FATAL, 2005, SPR, 0, 0, MSG)
!
!     * NSED not in [1,NSEDEE]
 8110 WRITE (MSG, 9006) NSED, NSEDEE
CALL ERROR (FATAL, 2006, SPR, 0, 0, MSG)
!
!     * NSYB > NSYBEE
 8610 WRITE (MSG, 9007) NSYB, NSYBEE
CALL ERROR (FATAL, 2007, SPR, 0, 0, MSG)
!
!     * NSYC(1) > NSYCEE
 8612 WRITE (MSG, 9009) NSYC (1), NSYCEE
CALL ERROR (FATAL, 2009, SPR, 0, 0, MSG)
!
!     * NSYC(3) > NSYCEE
 8614 WRITE (MSG, 9010) NSYC (3), NSYCEE
CALL ERROR (FATAL, 2010, SPR, 0, 0, MSG)
!
!     * ITYPE is not in the range [1,4]
 8620 WRITE (MSG, 9008) BB, ITYPE
CALL ERROR (FATAL, 2008, SPR, 0, 0, MSG)
!
!
 9003 FORMAT ( 1X,A )
!
 9005 FORMAT ('Workspace available is NELEE = ', I5, &
&        '; workspace required in subroutine SYREAD is ',I6 )
!
 9006 FORMAT ('No. of size groups NSED=',I4, &
&        ' is not in range [1,NSEDEE=',I3,']')
!
 9007 FORMAT ('No. of boundaries NSYB=',I5, &
&        ' is greater than NSYBEE=',I4,']')
!
 9008 FORMAT ('Boundary type NSYBCD(',I4,',2)=',I2, &
&        ' is not is the range [1,4]')
!
 9009 FORMAT ('No. of steady flux categories NSYC(1)=',I4, &
&        ' is greater than NSYCEE=',I3,']')
!
 9010 FORMAT ('No. of steady rating categories NSYC(3)=',I4, &
&        ' is greater than NSYCEE=',I3,']')
!
 9011 FORMAT ('SY module is version ',A,'; SYD data file is version ',A)
!
!
END SUBROUTINE SYREAD



!> Derives sediment-driver variables from water-flow state.
!>
!> `SYWAT` is the hydrology-to-sediment adapter called once per water-flow time
!> step by [[symain]]. It derives the rainfall, depth, slope, shear, and
!> confluence-splitting quantities used by [[syover]], [[sybker]], [[sycltr]],
!> [[sycolm]], and [[sylink]]. Arrays are dimensioned with `NLFEE`/`NELEE`
!> rather than `NLF` where needed because channel links may be absent.
!>
!> For each vegetation type, the fraction of ground sheltered from direct rain
!> is
!>
!> \[
!>   FCC = PLAI\,\min(CLAI,1).
!> \]
!>
!> For each land element, the effective drop diameter and uncovered rainfall
!> rate are
!>
!> \[
!>   DRDROP =
!>   \max\left(10^{-4},\;DRDRIP\,{DRAINA\over PNETTO},\;
!>             0.01935\,PNETTO^{0.182}\right)
!> \]
!>
!> when `PNETTO > 0`, otherwise `DRDROP = 1e-4`, and
!>
!> \[
!>   LRAIN = {\max(PNETTO-DRAINA,0)\over 1-FCC}
!> \]
!>
!> when `FCC < 1`, otherwise zero.
!>
!> For every element, the surface water depth is
!>
!> \[
!>   DWAT1 = \max(HRF-ZGRUND,0).
!> \]
!>
!> For each active face, outflow is interpreted as
!>
!> \[
!>   Q_{out}(iel,face)=\operatorname{sign}(1,2-face)\,QOC(iel,face).
!> \]
!>
!> Link side faces are skipped; `SLOPEJ` and `TAUJ` are therefore not defined
!> for those faces. No-flow faces are assigned zero slope and zero shear.
!>
!> Regular neighbours use `ICMREF`; confluence branches use `ICMRF2`. For
!> confluence outflows, `FQCONF(branch,p)` stores the positive prospect-flow
!> fraction used later to distribute sediment fluxes. These fractions are only
!> set for true outflow through the original confluence face and require a
!> positive gross prospect outflow. Boundary faces extrapolate from the opposite
!> face when possible, otherwise they use zero slope.
!>
!> Face slope and shear are then
!>
!> \[
!>   S_f = {|H_e-H_a|\over D_e+D_a},\qquad
!>   \tau_f = \rho_w g\,DWAT1\,S_f .
!> \]
!>
!> Across channel-bank faces, water levels below bankfull are clipped to
!> `ZBFULL` before the slope calculation. `TAUK(iel)` is set to the shear stress
!> on the face with the largest absolute water flux; if no considered face has
!> flow, `TAUK(iel)` remains zero.
SUBROUTINE SYWAT (NEL, NELEE, NLF, NLFEE, NV, NVC, ICMREF, ICMRF2, &
 DHF, DRDRIP, LINKNS, ZBFULL, ZGRUND, CLAI, DRAINA, HRF, PLAI, &
 PNETTO, QOC, DRDROP, DWAT1, FCC, FQCONF, LRAIN, SLOPEJ, TAUJ, &
 TAUK)

! Commons and distributed constants
USE CONST_SY

! NB: Don't use NLF as array size: it may be zero.
INTEGER :: NEL                     !! Number of elements.
INTEGER :: NELEE                   !! Element-array dimension.
INTEGER :: NLF                     !! Number of channel links.
INTEGER :: NLFEE                   !! Link-array dimension.
INTEGER :: NV                      !! Number of vegetation types.
INTEGER :: ICMREF (NELEE, 4, 2:3)  !! Face-neighbour and reverse-face reference map.
INTEGER :: ICMRF2 (NLFEE, 3, 2)    !! Confluence branch reference map.
INTEGER :: NVC (NLF + 1:NEL)       !! Vegetation type by land element.
DOUBLEPRECISION CLAI (NV)          !! Current canopy leaf-area index by vegetation type.
DOUBLEPRECISION DHF (NELEE, 4)     !! Face-to-face hydraulic distance.
DOUBLEPRECISION DRAINA (NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
DOUBLEPRECISION DRDRIP (NV)        !! Canopy drip drop diameter by vegetation type.
DOUBLEPRECISION HRF (NEL)          !! Water level/head by element.
DOUBLEPRECISION PLAI (NV)          !! Potential/maximum leaf-area index by vegetation type.
DOUBLEPRECISION PNETTO (NLF + 1:NEL) !! Net precipitation/effective rainfall by land element.
DOUBLEPRECISION QOC (NELEE, 4)     !! Face water fluxes.
DOUBLEPRECISION ZBFULL (NLFEE)     !! Bankfull elevation/depth by link.
DOUBLEPRECISION ZGRUND (NEL)       !! Ground or bed elevation by element.
LOGICAL :: LINKNS (NLFEE)          !! True for north-south channel links.
DOUBLEPRECISION DRDROP (NLF + 1:NEL) !! Effective raindrop/drop diameter by land element.
DOUBLEPRECISION DWAT1 (NEL)        !! Surface/channel water depth by element.
DOUBLEPRECISION FCC (NV)           !! Canopy/ground sheltering fraction by vegetation type.
DOUBLEPRECISION FQCONF (NLFEE, 3)  !! Confluence outflow fractions for receiving branches.
DOUBLEPRECISION LRAIN (NLF + 1:NEL) !! Effective direct rainfall rate by land element.
DOUBLEPRECISION SLOPEJ (NELEE, 4)  !! Face water-surface slopes.
DOUBLEPRECISION TAUJ (NELEE, 4)    !! Face shear stress.
DOUBLEPRECISION TAUK (NEL)         !! Representative element/link shear stress.
! NB: FQCONF defined only for branches flowing INTO a node;
!     SLOPEJ & TAUJ not defined at side faces of links.
!
! Locals, etc
DOUBLEPRECISION DRDMIN
PARAMETER (DRDMIN = 1D-4)
!
DOUBLEPRECISION DRAINE, DWAT1E, FCCE, HRFE, PNETTE, SLOPEE, TAUJE
DOUBLEPRECISION D, DA, DE, HA, HE, L
DOUBLEPRECISION Q, QABS, QMAX, QOUT, QOUTX (0:3), QSUM, TAUMAX, &
 ZBF
DOUBLEPRECISION FQOUT
INTEGER :: FACE, IADJ, IBR, ICOL, IEL, IELP
INTEGER :: KADJ, KEL, KELP, LINK, P, PADJ, PIN, POUT, VEG
LOGICAL :: BSIDE
!
FQOUT (IEL, FACE) = SIGN (1, 2 - FACE) * QOC (IEL, FACE)
!
!----------------------------------------------------------------------*
!
! Loop over Vegetation Types
! --------------------------
!
!     * Calculate ground fraction sheltered from rain by canopy
DO 100 VEG = 1, NV
   FCC (VEG) = PLAI (VEG) * MIN (CLAI (VEG), ONE)
  100 END DO
!
!
! Loop over Column Elements
! -------------------------
!
DO 200 ICOL = NLF + 1, NEL
!
!        * Avoid multiple array references
   DRAINE = DRAINA (ICOL)
   PNETTE = PNETTO (ICOL)
   VEG = NVC (ICOL)
   FCCE = FCC (VEG)
!
!        * Calculate median raindrop/leaf-drip diameter
   D = DRDMIN
   IF (PNETTE.GT.ZERO) D = MAX (D, DRDRIP (VEG) * (DRAINE / &
    PNETTE), 0.01935d0 * PNETTE**0.182d0)
   DRDROP (ICOL) = D
!
!        * Calculate rainfall rate
   L = ZERO
   IF (FCCE.LT.ONE) L = DIMJE(PNETTE, DRAINE) / (ONE-FCCE)
   LRAIN (ICOL) = L
!
  200 END DO
!
!
! Loop over All Elements
! ----------------------
!
DO 390 IEL = 1, NEL
!
!        * Avoid multiple array references
   HRFE = HRF (IEL)
!
!        * Calculate (& store) surface water depth
   DWAT1E = DIMJE(HRFE, ZGRUND (IEL) )
   DWAT1 (IEL) = DWAT1E
!
!        * Initialize maximum flow & shear stress
   QMAX = ZERO
   TAUMAX = ZERO
!
!        Loop over Faces ...
!        -------------------
!
!        ... of this element, in order to set FQCONF, SLOPEJ and TAUJ,
!        and to find a value for TAUK
!
   DO 350 FACE = 1, 4
!
!           * Not interested in link element side faces
      BSIDE = IEL.LE.NLF
      IF (BSIDE) BSIDE = MOD (FACE, 2) .EQ.1.EQV.LINKNS (IEL)
      IF (BSIDE) GOTO 350
!                      ^^^^^^^^
!
!           * Discharge rate
      QOUT = FQOUT (IEL, FACE)
!
!           * No-flow faces are special case
      IF (ISZERO(QOUT)) THEN
!              * (consider weirs and branch nodes for example)
         SLOPEJ (IEL, FACE) = ZERO
         TAUJ (IEL, FACE) = ZERO
         GOTO 350
!              ^^^^^^^^
      ENDIF
!
!           * Find neighbouring element, & its face (also set FQCONF)
      KEL = FACE
      IADJ = ICMREF (IEL, KEL, 2)
      IF (IADJ.EQ.0) THEN
!              * This is a boundary face; extrapolate from behind ...
         KEL = 1 + MOD (FACE+1, 4)
         IADJ = ICMREF (IEL, KEL, 2)
      ENDIF
      IF (IADJ.EQ.0) THEN
!              * ... unless that's a boundary too; then go for slope=0
         IADJ = IEL
         KADJ = KEL
      ELSEIF (IADJ.GT.0) THEN
!              * Neighbour is a regular element
         KADJ = ICMREF (IEL, KEL, 3)
      ELSE
!
!              * Extra things to do if neighbour is a confluence node
!
!              * Branch index
         IBR = - IADJ
!
!              * Initialize locals for prospect-loop:
!              - gross discharge from the node
         QSUM = ZERO
!              - prospects with maximal inflow/outflow
         PIN = 0
         POUT = 0
!              - discharge from node (let this branch be prospect 0)
         QOUTX (0) = - FQOUT (IEL, KEL)
!
!              * Loop over Prospects
         DO 300 P = 1, 3
            IELP = ICMRF2 (IBR, P, 1)
            IF (IELP.GT.0) THEN
               KELP = ICMRF2 (IBR, P, 2)
               Q = - FQOUT (IELP, KELP)
               QSUM = QSUM + MAX (ZERO, Q)
               IF (Q.LT.QOUTX (PIN) ) PIN = P
               IF (Q.GT.QOUTX (POUT) ) POUT = P
            ELSE
               Q = ZERO
            ENDIF
            QOUTX (P) = Q
  300          END DO
!
!              * Redefine neighbour as link with maximal outflow ...
         PADJ = POUT
!              * ... unless node is at inflow face for this element
         IF (QOUTX (0) .GT.ZERO) PADJ = PIN
         IF (PADJ.GT.0) THEN
            IADJ = ICMRF2 (IBR, PADJ, 1)
            KADJ = ICMRF2 (IBR, PADJ, 2)
         ELSE
!                 * (no obvious candidate: go for slope=0)
            IADJ = IEL
            KADJ = KEL
         ENDIF
!
!              * Calculate node outflow fractions if appropriate
         IF (QOUT.GT.ZERO.AND.KEL.EQ.FACE) THEN
!                 * NB: Need precondition on QOC to ensure QSUM.GT.0
            DO 320 P = 1, 3
               FQCONF (IBR, P) = MAX (ZERO, QOUTX (P) ) / QSUM
  320             END DO
         ENDIF
!
      ENDIF
!
!           * Calculate water surface slope
      HE = HRFE
      HA = HRF (IADJ)
      DE = DHF (IEL, KEL)
      DA = DHF (IADJ, KADJ)
      IF (IEL.LE.NLF.NEQV.IADJ.LE.NLF) THEN
!              * this is a bank face; use bank-full elevation as cut-off
         LINK = MIN (IEL, IADJ)
         ZBF = ZBFULL (LINK)
         IF (HE.LE.ZBF) THEN
            HE = ZBF
            DE = ZERO
         ENDIF
         IF (HA.LE.ZBF) THEN
            HA = ZBF
            IF (DE.GT.ZERO) DA = ZERO
         ENDIF
      ENDIF
      SLOPEE = ABS (HE-HA) / (DE+DA)
      SLOPEJ (IEL, FACE) = SLOPEE
!
!           * Calculate flow shear stress at the ground surface
      TAUJE = RHOWAT * GRAVTY * DWAT1E * SLOPEE
      TAUJ (IEL, FACE) = TAUJE
!
!           * Find maximum flow rate so far and TAUJ for that face
      QABS = ABS (QOUT)
      IF (QABS.GT.QMAX) THEN
         QMAX = QABS
         TAUMAX = TAUJE
      ENDIF
!
!        * Next face
  350    END DO
!
!        * Set representative shear stress equal to maximum over faces
   TAUK (IEL) = TAUMAX
!
!     * Next element
  390 END DO
!
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
