!> summary: Sediment input parameters, size-class properties and the option switches.
!> author: AB / RAH / BTL, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> Everything the sediment component reads from the `SY01`--`SY64` data groups
!> and then treats as fixed: the size-class diameters and densities, the
!> erodibility and drip-detachment coefficients, the limiting concentrations
!> and depths, and the switches that select between the transport formulae.
!> [[sy_input:SYREAD]] writes it and [[sy_driver:SYMAIN]] reads it.
!>
!> The `_symain` suffix records that these were `SYMAIN`'s own saved locals
!> before they became module state; the names are unchanged by this move.
!>
!> | Parameter | Role in the component |
!> |:----------|:----------------------|
!> | `FPCRIT_symain` | Maximum `FDEL` for each channel size group and maximum total overland `FDEL`. |
!> | `DLSMAX_symain` | Hillslope loose-sediment depth at which underlying soil erosion is suppressed. |
!> | `DCBEDO_symain` | Active upper channel-bed thickness controlling exchange with the lower layer. |
!> | `ALPHA_symain` | Ratio of fine-sediment settling to resuspension critical shear stress. |
!> | `FBIC_symain`, `FICRIT_symain` | Fine-sediment bed-fraction and concentration thresholds controlling infiltration. |
!> | `CONCOB_symain` | Mobile concentration threshold used for overbank sediment exchange. |
!>
!> The switches select the formulations documented in section 2.8 of the User
!> Guide: `ISGSED_symain` the overland capacity (Yalin or Engelund-Hansen),
!> `ISTEC_symain` the critical-shear relation, `ISACKW_symain` the channel
!> capacity (Engelund-Hansen, Ackers-White or Ackers-White-Day), and
!> `ISUSED_symain` whether non-fines may move slower than the water. Module
!> state is public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into a single Fortran 90 module. |
!> | 2026-04 to 2026-05 | SvB | 4.6.1 | Modernised the whole component: free-form layout, `IMPLICIT NONE`/`INTENT` throughout, structured control flow in place of `GOTO`s, compile-time `PARAMETER`s for the cached first-call constants, and `symain`'s work arrays moved to allocate-once module storage. |
!> | 2026-09-10 | SvB | - | Split out of SYmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_config

   USE array_limits, ONLY: nelee, nlfee, NSEDEE, NSEE, NVEE

   IMPLICIT NONE

   INTEGER, PARAMETER  :: NSYBEE = 40       !! Maximum number of sediment boundary entries.
   INTEGER, PARAMETER  :: NSYCEE = 10        !! Maximum number of sediment boundary categories.
   INTEGER          :: ISACKW_symain       !! Channel transport-capacity option.
   INTEGER          :: ISGSED_symain       !! Overland transport-capacity option.
   INTEGER          :: ISSYOK_symain       !! Dynamic sediment input-check interval.
   INTEGER          :: ISTEC_symain        !! Critical-shear calculation option.
   INTEGER          :: ISUSED_symain       !! Sediment velocity option.
   INTEGER          :: NEPS_symain         !! Number of sediment substeps per water timestep.
   INTEGER          :: NFINE_symain        !! Number of fine sediment classes; manual allows 0 or 1.
   INTEGER          :: NSYB_symain         !! Number of sediment boundary entries.
   INTEGER          :: NSYBCD_symain(NSYBEE, 3) !! Sediment boundary element, type, and category metadata.
   INTEGER          :: NSYC_symain(4)      !! Number of sediment boundary categories by boundary type.
   INTEGER          :: NTSOBK_symain(NLFEE) !! Bank soil type by channel link.
   INTEGER          :: PASS_symain = 0       !! Saved call counter for sediment setup/timestep control.
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

END MODULE sy_config

