!> summary: Ground-surface erosion and sediment routing over land elements.
!> author: AB / RAH / BTL, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> The hillslope half of the transport calculation: [[SYOVER]] computes
!> raindrop- and flow-driven detachment of the ground surface, [[SYCOLM]]
!> routes each size fraction through a land element, and [[SYFINE]] handles
!> fine-sediment settling, infiltration into the bed and the armouring limit.
!> The channel half is in [[sy_channel]].
!>
!> `SYFINE`'s cached settling velocity `WSED_syfine` and its first-call flag
!> live in [[sy_transport_capacity]], where `variables.csv` places them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into a single Fortran 90 module. |
!> | 2026-04 to 2026-05 | SvB | 4.6.1 | Modernised the whole component: free-form layout, `IMPLICIT NONE`/`INTENT` throughout, structured control flow in place of `GOTO`s, compile-time `PARAMETER`s for the cached first-call constants, and `symain`'s work arrays moved to allocate-once module storage. |
!> | 2026-09-10 | SvB | - | Split out of SYmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_hillslope

   USE MOD_PARAMETERS, ONLY: half, one, zero, GRAVITY, RHO_SEDIMENT, &
                             RHO_WATER_SEDIMENT, NU_WATER
   USE float_compare, ONLY: dimje, gtzero
   USE sy_transport_capacity, ONLY: SYCRIT, SYOVTR, FIRST_syfine, WSED_syfine

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SYCOLM, SYFINE, SYOVER

CONTAINS

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
   PURE SUBROUTINE SYCOLM(AREAE, DTSY, DWAT1E, DWATOE, DXQQE, DYQQE, FETAE, GNUE, ISGSED, NSED, &
                          FPCRIT, PLSE, NSEDEE, DRSED, QWAT, SLOPEE, SOSDFE, TAUJE, DLSE, FBETAE, &
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
      DOUBLE PRECISION, INTENT(IN) :: DRSED(NSED) !! Representative particle diameters by size class.
      DOUBLE PRECISION, INTENT(IN) :: QWAT(4)     !! Outward water flux by face.
      DOUBLE PRECISION, INTENT(IN) :: SLOPEE(4)   !! Water-surface slope by face.
      DOUBLE PRECISION, INTENT(IN) :: SOSDFE(NSED) !! Source soil sediment-size fraction.
      DOUBLE PRECISION, INTENT(IN) :: TAUJE(4)     !! Face shear stress.

      ! Input/output arguments
      DOUBLE PRECISION, INTENT(INOUT) :: DLSE       !! Loose-sediment depth in the land element.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETAE(NSED) !! Loose-sediment composition by size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FDELE(NSED)  !! Mobile sediment concentration fraction by size class.
      DOUBLE PRECISION, INTENT(INOUT) :: QSEDE(NSEDEE, 4) !! Sediment flux by size class and face.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: Q(NSED)     !! Workspace for outgoing sediment flux by size class.
      DOUBLE PRECISION, INTENT(INOUT) :: VDSED(NSED) !! Workspace for available sediment volume by size class.

      ! Locals, etc
      INTEGER :: FACE, J(4), JLC, NOUT, SED
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
         QK = QWAT(FACE)
         IF (QK > ZERO) THEN
            ! * Outflow face
            QWSUM = QWSUM + QK
            NOUT = NOUT + 1
            J(NOUT) = FACE
         ELSE
            ! * Inflow or no-flow face
            DO SED = 1, NSED
               Q(SED) = Q(SED) - QSEDE(SED, FACE)/FLS
            END DO
         END IF
      END DO

      !     * Calculate volume of water + volume of discharged water
      VDWAT = DWAT1E*AREAE + QWSUM*DTSY

      !     * Calculate volume of stored sediment plus volume of
      !     * discharged sediment for each fraction ( must be non-negative )
      DDLS = FETAE*GNUE*DTSY
      DO SED = 1, NSED
         DBETA = DLSE*FBETAE(SED) + DDLS*SOSDFE(SED)
         VD = (FDELE(SED)*DWATOE + DBETA)*AREAE + Q(SED)*DTSY
         VDSUM = VDSUM + VD
         VDSED(SED) = VD
      END DO

      ! Sediment Discharge
      ! ------------------
      !
      !     Note: The only outputs from this section are the coefficients
      !           A1 and B1 required by the next section.
      !
      !     * Discharge rate based upon SUPPLY, assuming unlimited capacity
      GSUM = ZERO
      IF (GTZERO(VDWAT)) GSUM = FLS*VDSUM*(QWSUM/VDWAT)

      !     * Is discharge possible?
      IF (GTZERO(GSUM)) THEN

         ! * Yes ( implies VDSUM > 0 )
         !
         ! * Discharge rate based upon flow CAPACITY ...
         CALL SYOVTR(DXQQE, DYQQE, ISGSED, DWAT1E, NSED, VDSED, DRSED, QWAT, SLOPEE, TAUJE, GJSUM)

         ! ... with additional upper limit based on total suspended load
         G = MIN(GJSUM, QWSUM*FPCRIT)

         ! * Transport is governed by the lower of the two rates
         !   (take MIN before dividing, in case G>>GSUM)
         A1 = MIN(G, GSUM)/GSUM
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
      DLSE = (ONE - A1)*VDSUM/AREAE

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
         VD = VDSED(SED)
         FD = (A1*VD)/B1
         FDELE(SED) = FD
         FBETAE(SED) = A2*VD/B2 + A3*SOSDFE(SED)

         DO JLC = 1, NOUT
            FACE = J(JLC)
            QSEDE(SED, FACE) = FLS*QWAT(FACE)*FD
         END DO
      END DO

   END SUBROUTINE SYCOLM

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
         WSED_syfine = DRSEDF**2*GRAVITY*(RHO_SEDIMENT - RHO_WATER_SEDIMENT)/(18.0D0*RHO_WATER_SEDIMENT*NU_WATER)
      END IF

      ! * Loop over channel links
      link_loop: DO LINK = 1, NLF

         TAUKL = TAUK(LINK)
         AREA_L = AREA(LINK)
         FDELFL = FDELF(LINK)

         ! * Calculate critical shear stress for fines
         CALL SYCRIT(0, DRSEDF, TAUKL, DUM, TAUEC)

         ! * Calculate potential fines in upper layer
         ! * (existing fines + settling)
         DUM = ALPHA*TAUEC
         IF (DUM > 0.0D0) DUM = DIMJE(DUM, TAUKL)/DUM
         DCFMXL = DCBF(LINK) + FDELFL*WSED_syfine*DUM*DTSY
         VCFMAX(LINK) = DCFMXL*AREA_L

         ! * Can fines be armoured ?
         BARM(LINK) = (TAUKL <= TAUEC)

         ! * Calculate potential infiltration rate
         VMAX = 0.0D0
         IF (FBETAF(LINK) < FBIC) THEN
            VMAX = WSED_syfine*AREA_L*DIMJE(FDELFL, FICRIT/(1.0D0 - PBSED(LINK)))*DTSY
         END IF
         VINFMX(LINK) = VMAX

      END DO link_loop

   END SUBROUTINE SYFINE

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
   SUBROUTINE SYOVER(ISTEC, NEL, NLF, NS, NV, FCC, LRAIN, XDRIP, &
                     DRDRIP, FDRIP, DRAINA, GKR, DWAT1, DRDROP, FCG, FCROCK, DRSO50, &
                     TAUK, FPCLAY, GKF, RHOSO, NTSOTP, NVC, GNU, TGMD, DLS, DLSMAX)

      IMPLICIT NONE

      ! Input/Output arguments
      INTEGER, INTENT(IN) :: ISTEC !! Critical-shear calculation option.
      INTEGER, INTENT(IN) :: NEL   !! Number of elements.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NS    !! Number of soil types.
      INTEGER, INTENT(IN) :: NV    !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NTSOTP(NLF + 1:NEL) !! Top soil type by land element.
      INTEGER, INTENT(IN) :: NVC(NLF + 1:NEL)    !! Vegetation type by land element.
      DOUBLE PRECISION, INTENT(IN) :: FCC(NV)   !! Canopy/ground sheltering fraction by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: LRAIN(NLF + 1:NEL) !! Effective direct rainfall rate by land element.
      DOUBLE PRECISION, INTENT(IN) :: XDRIP(NV) !! Canopy drip fall height by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: DRDRIP(NV) !! Canopy drip drop diameter by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: FDRIP(NV)  !! Canopy drip fraction by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: DRAINA(NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
      DOUBLE PRECISION, INTENT(IN) :: GKR(NS)   !! Rainfall detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(IN) :: DWAT1(NLF + 1:NEL)  !! Surface water depth by land element.
      DOUBLE PRECISION, INTENT(IN) :: DRDROP(NLF + 1:NEL) !! Effective raindrop/drop diameter by land element.
      DOUBLE PRECISION, INTENT(IN) :: FCG(NLF + 1:NEL)    !! Ground-cover fraction by land element.
      DOUBLE PRECISION, INTENT(IN) :: FCROCK(NLF + 1:NEL) !! Rock-cover fraction by land element.
      DOUBLE PRECISION, INTENT(IN) :: DRSO50(NS) !! Median soil particle diameter by soil type.
      DOUBLE PRECISION, INTENT(IN) :: TAUK(NLF + 1:NEL) !! Overland-flow shear stress by land element.
      DOUBLE PRECISION, INTENT(IN) :: FPCLAY(NS) !! Clay fraction by soil type.
      DOUBLE PRECISION, INTENT(IN) :: GKF(NS)    !! Flow detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(IN) :: RHOSO(NS)  !! Soil bulk density by soil type.
      DOUBLE PRECISION, INTENT(IN) :: DLS(NEL)   !! Loose-sediment depth by element.
      DOUBLE PRECISION, INTENT(IN) :: DLSMAX      !! Loose-sediment depth above which soil erosion is suppressed.
      DOUBLE PRECISION, INTENT(OUT) :: GNU(NLF + 1:NEL) !! Hillslope erosion rate by land element.
      DOUBLE PRECISION, INTENT(OUT) :: TGMD(NV)  !! Workspace for canopy-drip momentum by vegetation type.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: X1 = 7.5D0, D1 = 3.3D-3, L1 = 2.78D-6, L2 = 1.39D-5
      DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979323846D0
      DOUBLE PRECISION, PARAMETER :: CLALIM = 1.0D0/L2

      INTEGER :: ISCD, IEL, ISGMR, ISOIL, NVEG
      DOUBLE PRECISION :: CD, FCROCE, DRDRPE, DR, DF
      DOUBLE PRECISION :: LRAINE, GMD, GMR, PRSGOS, TAUEC, TAUKE, XDRIPE

      DOUBLE PRECISION, PARAMETER :: AD(4) = [3214.9D0, 583.4D0, 133.1D0, 29.9D0]
      DOUBLE PRECISION, PARAMETER :: BD(4) = [1.6896D0, 1.5545D0, 1.4242D0, 1.2821D0]
      DOUBLE PRECISION, PARAMETER :: ADD(4) = [0.0D0, 0.0D0, 1.93D0, 5.14D0]
      DOUBLE PRECISION, PARAMETER :: BDD(4) = [2200.0D0, 2200.0D0, 1640.0D0, 660.0D0]

      ! Legacy branchless statement function
      DOUBLE PRECISION :: SF2, SX, SY
      SF2(SX, SY) = HALF + SIGN(HALF, SX - SY)

      !----------------------------------------------------------------------*

      PRSGOS = PI*RHO_WATER_SEDIMENT*RHO_WATER_SEDIMENT*GRAVITY/6.0D0

      DO NVEG = 1, NV
         XDRIPE = XDRIP(NVEG)
         DRDRPE = DRDRIP(NVEG)

         ! Performance Reversion: Branchless execution
         ISCD = 1 + NINT(SF2(XDRIPE, X1) + 2.0D0*SF2(DRDRPE, D1))

         CD = ADD(ISCD) + DRDRPE*BDD(ISCD)
         TGMD(NVEG) = PRSGOS*CD*(ONE - EXP(-2.0D0*XDRIPE/CD))*(DRDRPE**3)*FDRIP(NVEG)
      END DO

      DO IEL = NLF + 1, NEL
         ISOIL = NTSOTP(IEL)
         NVEG = NVC(IEL)
         LRAINE = LRAIN(IEL)
         FCROCE = FCROCK(IEL)
         TAUKE = TAUK(IEL)

         ! Performance Reversion: Branchless execution
         ISGMR = MIN(4, 1 + NINT(SF2(LRAINE, L1)) + INT(LRAINE*CLALIM))

         GMR = (ONE - FCC(NVEG))*AD(ISGMR)*(LRAINE**BD(ISGMR))
         GMD = TGMD(NVEG)*DRAINA(IEL)

         DR = GKR(ISOIL)*EXP(-MAX(ZERO, (DWAT1(IEL)/DRDROP(IEL)) - ONE))* &
              (ONE - FCG(IEL) - FCROCE)*(GMR + GMD)

         CALL SYCRIT(ISTEC, DRSO50(ISOIL), TAUKE, FPCLAY(ISOIL), TAUEC)

         DF = GKF(ISOIL)*(ONE - FCROCE)*MAX(ZERO, TAUKE - TAUEC)/TAUEC

         IF (DLS(IEL) < DLSMAX) THEN
            GNU(IEL) = (DR + DF)/RHOSO(ISOIL)
         ELSE
            GNU(IEL) = ZERO
         END IF
      END DO

   END SUBROUTINE SYOVER

END MODULE sy_hillslope

