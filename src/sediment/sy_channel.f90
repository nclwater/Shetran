!> summary: Channel-bank erosion, link routing, bed updating and the derived channel hydraulics.
!> author: AB / RAH / BTL, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> The channel half of the transport calculation: [[SYWAT]] derives the
!> geometry, slopes, flows and shear stresses from the water modules,
!> [[SYBKER]] computes lateral bank erosion, [[SYLINK]] routes each size
!> fraction along a link, and [[SYBED]] updates the two-layer bed storage.
!> The hillslope half is in
!> [[sy_hillslope]].
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into a single Fortran 90 module. |
!> | 2026-04 to 2026-05 | SvB | 4.6.1 | Modernised the whole component: free-form layout, `IMPLICIT NONE`/`INTENT` throughout, structured control flow in place of `GOTO`s, compile-time `PARAMETER`s for the cached first-call constants, and `symain`'s work arrays moved to allocate-once module storage. |
!> | 2026-09-10 | SvB | - | Split out of SYmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_channel

   USE MOD_PARAMETERS, ONLY: one, two, GRAVITY, RHO_WATER_SEDIMENT
   USE float_compare, ONLY: dimje, iszero
   USE sy_transport_capacity, ONLY: SYCRIT
   USE sy_state, ONLY: face_outflow

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SYBED, SYBKER, SYLINK, SYWAT

CONTAINS

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

         ARBDEP(LINK) = ARBDEP(LINK) + CWIDTH(LINK)*(DLSNEW - DLSOLD)
         DCNEW = MIN(DLSNEW, DCBEDO)
         DCBED(LINK) = DCNEW

         ! * What fraction of the interim top layer remains in the top
         ! * layer, and what fraction of the interim bottom layer becomes
         ! * part of the top?
         DCC = MIN(DCBEDZ, DCNEW)
         AC = 0.0D0
         AD = 0.0D0

         IF (DCBEDZ > 0.0D0) AC = DCC/DCBEDZ
         IF (DDBEDZ > 0.0D0) AD = (DCNEW - DCC)/DDBEDZ

         ! * Loop over sediment size groups
         sed_loop: DO SED = 1, NSED

            ! * Interim layer depths
            DCIPP = DCIPRM(LINK, SED)
            DDIPP = DDIPRM(LINK, SED)

            ! * Total depth (for this size group)
            SUMSED = DCIPP + DDIPP

            ! * New top layer depth
            DCINEW = AC*DCIPP + AD*DDIPP
            DCBSED(LINK, SED) = DCINEW

            ! * New bottom layer depth
            DDBSED(LINK, SED) = SUMSED - DCINEW

            ! * Composition of both layers together
            IF (DLSNEW > 0.0D0) FBETA(LINK, SED) = (SUMSED/DLSNEW)

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
      DOUBLE PRECISION, PARAMETER :: QUART = 1.0D0/4.0D0

      INTEGER :: BKSOIL, LINK
      DOUBLE PRECISION :: DWAT1E, GNUBKE, K, TAUEC, TAUKE, X

      !----------------------------------------------------------------------*

      ! * Loop over channel links
      link_loop: DO LINK = 1, NLF

         BKSOIL = NTSOBK(LINK)
         DWAT1E = DWAT1(LINK)
         TAUKE = TAUK(LINK)

         ! * Calculate aspect ratio coefficient ( see Notes )
         X = ONE/MAX(QUART, DWAT1E/CWIDTH(LINK))
         K = A1 + B1*MIN(X, ONE) + B2*MIN(DIMJE(X, ONE), ONE) &
             + B3*DIMJE(X, TWO)

         ! * Obtain critical shear stress for bank erosion
         CALL SYCRIT(ISTEC, DRSO50(BKSOIL), TAUKE, FPCLAY(BKSOIL), TAUEC)

         ! * Calculate bank erosion rate
         GNUBKE = BKB(BKSOIL)*DIMJE(K*TAUKE, TAUEC)/(TAUEC*RHOSO(BKSOIL))
         GNUBK(LINK) = GNUBKE

         ! * Calculate rate of release of sediments for each link
         EPSB(LINK) = TWO*FETA(LINK)*CLENTH(LINK)*GNUBKE* &
                      MIN(DWAT1E, DBFULL(LINK))

      END DO link_loop

   END SUBROUTINE SYBKER

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
      OMPBI = 1.0D0/OMPB
      DTSYI = 1.0D0/DTSY
      AREAEI = 1.0D0/AREAE

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
         QSEDIN = -SUM*OMPBI

         ! * Volume of water remaining + advective water discharge
         SUM = 0.0D0
         outflow_loop: DO JI = 1, NOUT
            SUM = SUM + QSDWAE(SED, J(JI))
         END DO outflow_loop
         VDWAT = ARXLE*CLENTE + SUM*DTSY

         ! * Sediment available for resuspension/transport/infiltration
         ! * /armouring
         VDMAX = FDELE(SED)*ARXLOE*CLENTE + DCBEEE*AREAE + &
                 (QSEDIN + EPSBE*SOSDFE(SED))*DTSY

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
            IF (BARME .AND. SUMN > 0.0D0) FDC = MIN(SUMN, SUMP)/SUMN
            VCARM = FDC*DIMJE(VCFS, VINF)
         END IF

         ! * Volume in and above top layer after infiltration ...
         VDSEDS = DIMJE(VDMAX, VINF)
         ! * ... minus armoured volume ( = SUPPLY limit for transport )
         VDSED = DIMJE(VDSEDS, VCARM)

         ! * Infiltration rates for each layer
         GINF = VINF*DTSYI
         GINFDE(SED) = GINF
         GINFSE(SED) = GINF

         ! Other output variables
         ! ----------------------

         ! * Sediment remaining in suspension + sediment discharged
         ! * - limited by either SUPPLY or CAPACITY
         VSTRAN = MIN(VDSED, CONCIE(SED)*OMPBI*VDWAT)

         ! * Concentration in suspension ('relative density')
         FDELEE = 0.0D0
         IF (VDWAT > 0.0D0) FDELEE = VSTRAN/VDWAT
         FDELE(SED) = FDELEE

         ! * Interim layer depths
         DCIPEE = DIMJE(VDSEDS, VSTRAN)*AREAEI
         DCIPRE(SED) = DCIPEE
         DDIPRE(SED) = DDBSEE(SED) + VINF*AREAEI

         ! * Particulate discharge rates at outflow faces
         discharge_loop: DO JI = 1, NOUT
            QSEDE(SED, J(JI)) = QSDWAE(SED, J(JI))*FDELEE*OMPB
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
!> | 2026-09-20 | SvB | - | Replaced `FQOUT` with the shared [[sy_state:face_outflow]], which [[syerr3]] duplicated as `FNQOUT`. |
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
      INTEGER, INTENT(IN) :: NVC(NLF + 1:NEL) !! Vegetation type by land element.
      DOUBLE PRECISION, INTENT(IN) :: CLAI(NV)   !! Current canopy leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: DHF(NELEE, 4) !! Face-to-face hydraulic distance.
      DOUBLE PRECISION, INTENT(IN) :: DRAINA(NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
      DOUBLE PRECISION, INTENT(IN) :: DRDRIP(NV) !! Canopy drip drop diameter by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: HRF(NEL)   !! Water level/head by element.
      DOUBLE PRECISION, INTENT(IN) :: PLAI(NV)   !! Potential/maximum leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: PNETTO(NLF + 1:NEL) !! Net precipitation/effective rainfall by land element.
      DOUBLE PRECISION, INTENT(IN) :: QOC(NELEE, 4)  !! Face water fluxes.
      DOUBLE PRECISION, INTENT(IN) :: ZBFULL(NLFEE)  !! Bankfull elevation/depth by link.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND(NEL)    !! Ground or bed elevation by element.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE) !! True for north-south channel links.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: DRDROP(NLF + 1:NEL) !! Effective raindrop/drop diameter by land element.
      DOUBLE PRECISION, INTENT(OUT) :: DWAT1(NEL) !! Surface/channel water depth by element.
      DOUBLE PRECISION, INTENT(OUT) :: FCC(NV)    !! Canopy/ground sheltering fraction by vegetation type.
      DOUBLE PRECISION, INTENT(OUT) :: FQCONF(NLFEE, 3)  !! Confluence outflow fractions for receiving branches.
      DOUBLE PRECISION, INTENT(OUT) :: LRAIN(NLF + 1:NEL) !! Effective direct rainfall rate by land element.
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
      TAUJ = 0.0D0
      FQCONF = 0.0D0
      LRAIN = 0.0D0
      DRDROP = 0.0D0

      ! Loop over Vegetation Types
      ! --------------------------
      !
      !     * Calculate ground fraction sheltered from rain by canopy
      FCC(1:NV) = PLAI(1:NV)*MIN(CLAI(1:NV), 1.0D0)

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
            D = MAX(D, DRDRIP(VEG)*(DRAINE/PNETTE), 0.01935D0*PNETTE**0.182D0)
         END IF
         DRDROP(ICOL) = D

         ! * Calculate rainfall rate
         L = 0.0D0
         IF (FCCE < 1.0D0) L = DIMJE(PNETTE, DRAINE)/(1.0D0 - FCCE)
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
            QOUT = face_outflow(QOC, IEL, FACE)

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
               QOUTX(0) = -face_outflow(QOC, IEL, KEL)

               ! * Loop over Prospects
               DO P = 1, 3
                  IELP = ICMRF2(IBR, P, 1)
                  IF (IELP > 0) THEN
                     KELP = ICMRF2(IBR, P, 2)
                     Q = -face_outflow(QOC, IELP, KELP)
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
                     FQCONF(IBR, P) = MAX(0.0D0, QOUTX(P))/QSUM
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

            SLOPEE = ABS(HE - HA)/(DE + DA)
            SLOPEJ(IEL, FACE) = SLOPEE

            ! * Calculate flow shear stress at the ground surface
            TAUJE = RHO_WATER_SEDIMENT*GRAVITY*DWAT1E*SLOPEE
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

   END SUBROUTINE SYWAT

END MODULE sy_channel

