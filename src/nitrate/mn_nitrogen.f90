!> summary: Ammonium storage, nitrification, mineralisation and litter nitrogen.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> The mineral-nitrogen half of the component: [[mnamm]] advances the ammonium
!> store, [[mnnit]] nitrification and denitrification, [[MNGAM]] the coupled
!> mineralisation/immobilisation rate, and [[mnltn]] the nitrogen in the litter
!> pool. The carbon side is in [[mn_organic_matter]] and the factors these
!> routines scale by are in [[mn_environment]].
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> | 2026-09-10 | SvB | - | Split out of MNmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mn_nitrogen

   USE error_reporting, ONLY: RAISE_ERROR
   USE mn_state, ONLY: calit, cdort, chum, chum1, clit, clit1, cman, cman1, denit, edeth, &
                       emph, emt, enph, ent, gam, gamtmp, imamm, imdiff, imnit, isimtf, &
                       kd1, kd2, khum, klit, kman, knit, kvol, miner, naamm, namm, namm1, &
                       nanit, ndnit, ndsnt, nlit, nlit1, nman, nman1, ntrf, plamm, plnit, &
                       plup, pphi, snit, vol

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: mnamm, mngam, mnltn, mnnit

CONTAINS

!> @brief Updates dissolved ammonium concentration for all active soil cells.
!>
!> `mnamm` iterates the ammonium mass balance with adsorption retardation,
!> mineralisation/immobilisation, nitrification, volatilisation, plant uptake,
!> and external ammonium input. Non-convergence of the cell iteration reports
!> error 3018.
!>
!> The active vertical range is `NBOTCE:NCETOP` when `ISBOTC` is true, otherwise
!> `NCOLMB(element):NCETOP`. Within each soil layer, the iteration solves for
!> `NAMM1` using the nonlinear ammonium retardation factor
!>
!> \[
!> R_\mathrm{amm}=1+
!> \frac{KDDSOL_s\,(NAMM/MNCREF)^{GNN-1}}{\theta}.
!> \]
!>
!> At each iteration the half-step concentration
!> \(NAMM_h=(NAMM+NAMM1)/2\) drives the process terms:
!>
!> | Term | Implemented expression |
!> |:-----|:-----------------------|
!> | Mineralisation | `MINER=GAM` and `IMAMM=0` when `GAM>=0`. |
!> | Immobilisation | `MINER=0`, `IMAMM=min(-GAM, KUAMM*NAMM_h)` when `GAM<0`. |
!> | Nitrification | `NTRF=theta_h*KNIT*ENT*ENPH*NAMM_h`. |
!> | Volatilisation | `VOL=theta_h*KVOL*EMT*NAMM_h`. |
!> | Plant uptake | `PLAMM=min(PLUP*(PPHI*NAMM_h/(NDNIT+NAMM_h)+(1-PPHI)*NAMM_h/(NDSNT+NAMM_h)), VSTHE*KPLAMM*NAMM_h)`. |
!>
!> The new concentration is
!>
!> \[
!> NAMM1 =
!> \frac{\theta_o\,NAMM\,R_o
!>       + DTUZ(-PLAMM+MINER-IMAMM-NTRF-VOL+NAAMM)}
!>      {\theta\,R_1}.
!> \]
!>
!> Up to 20 iterations are allowed per cell. Convergence uses the squared
!> relative change in `NAMM1`; the tolerance is \(10^{-12}\).
   SUBROUTINE mnamm(llee, mnpr, nbotce, ncetop, nel, nelee, nlf, nlyree, ns, ncolmb, nlyr, nlyrbt, ntsoil, gnn, kplamm, kuamm, &
      mncref, kddsol, dtuz, vsthe, vstheo, isbotc)

      IMPLICIT NONE

      ! input arguments
      INTEGER, INTENT(IN) :: llee  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: mnpr  !! MN diagnostic output unit used for warning messages.
      INTEGER, INTENT(IN) :: nbotce  !! Lowest cell included when bottom-cell truncation is active.
      INTEGER, INTENT(IN) :: ncetop  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: nel  !! Number of elements.
      INTEGER, INTENT(IN) :: nelee  !! Element-array dimension.
      INTEGER, INTENT(IN) :: nlf  !! Number of overland/channel links excluded from land-column updates.
      INTEGER, INTENT(IN) :: nlyree  !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: ns  !! Number of soil types.
      INTEGER, INTENT(IN) :: ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(IN) :: nlyr(nelee)  !! Number of soil layers in each element.
      INTEGER, INTENT(IN) :: nlyrbt(nel, nlyree)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: ntsoil(nel, nlyree)  !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: gnn  !! Nonlinear ammonium adsorption exponent.
      DOUBLE PRECISION, INTENT(IN) :: kplamm  !! First-order ammonium plant-uptake limit.
      DOUBLE PRECISION, INTENT(IN) :: kuamm  !! First-order ammonium immobilisation limit.
      DOUBLE PRECISION, INTENT(IN) :: mncref  !! Reference nitrogen concentration.
      DOUBLE PRECISION, INTENT(IN) :: kddsol(ns)  !! Soil ammonium adsorption coefficient.
      DOUBLE PRECISION, INTENT(IN) :: dtuz  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: vsthe(ncetop, nel)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: vstheo(nel, ncetop + 1)  !! Previous volumetric water content.
      LOGICAL, INTENT(IN) :: isbotc  !! True when the fixed lower active cell `NBOTCE` is used.

      ! locals
      INTEGER :: jsoil, jlyr, nbotm, ncebot, ncl, nelm, niters, ntime
      INTEGER :: warn
      DOUBLE PRECISION :: dum, dum1, dum2, errtol, namm1o
      DOUBLE PRECISION :: nammh, retamm, retamm1, ttheth, werr1, wer1sq
      CHARACTER(LEN=132) :: msg

      ! * parameters for the iteration loop within the subroutine
      PARAMETER(niters=20, warn=3)
      PARAMETER(errtol=1.0d-12)

      !-------------------------------------------------------------------*

      DO nelm = nlf + 1, nel
         IF (isbotc) THEN
            nbotm = nbotce
         ELSE
            nbotm = ncolmb(nelm)
         END IF

         ncebot = nbotm

         DO jlyr = 1, nlyr(nelm)
            jsoil = ntsoil(nelm, jlyr)

            layer_loop: DO ncl = MAX(ncebot, nlyrbt(nelm, jlyr)), nlyrbt(nelm, jlyr + 1) - 1

               ! * initialise local variables
               nammh = namm(nelm, ncl)
               namm1o = 0.0d0

               ! * old retardation factor for ammonium adsorption
               retamm = 1.0d0 + (kddsol(jsoil)*(namm(nelm, ncl)/mncref)**(gnn - 1.0d0))/vstheo(nelm, ncl)

               ttheth = (vsthe(ncl, nelm) + vstheo(nelm, ncl))/2.0d0

               ! * iteration loop to calculate the new ammonium nitrogen
               ! * concentrations in the soil water
               iteration_loop: DO ntime = 1, niters

                  ! * new retardation factor for ammonium adsorption
                  retamm1 = 1.0d0 + (kddsol(jsoil)*(namm1(nelm, ncl)/mncref)**(gnn - 1.0d0))/vsthe(ncl, nelm)

                  ! * calculation of both the mineralisation rate and the
                  ! * immobilisation rate of ammonium
                  IF (gam(nelm, ncl) >= 0.0d0) THEN
                     miner(nelm, ncl) = gam(nelm, ncl)
                     imamm(nelm, ncl) = 0.0d0
                  ELSE
                     miner(nelm, ncl) = 0.0d0
                     imamm(nelm, ncl) = MIN(-gam(nelm, ncl), kuamm*nammh)
                  END IF

                  ! * calculation of the nitrification rate
                  ntrf(nelm, ncl) = ttheth*knit(nelm, ncl)*ent(nelm, ncl)*enph(nelm, ncl)*nammh

                  ! * calculation of the ammonia volatilisation rate
                  vol(nelm, ncl) = ttheth*kvol(nelm, ncl)*emt(nelm, ncl)*nammh

                  ! * calculation of the plant uptake rate of ammonium
                  IF (nammh > 0.0d0) THEN
                     dum1 = plup(nelm, ncl)*(pphi(nelm, ncl)*nammh/(ndnit(nelm, ncl) + nammh) + &
                        (1.0d0 - pphi(nelm, ncl))*nammh/(ndsnt(nelm, ncl) + nammh))
                  ELSE
                     dum1 = 0.0d0
                  END IF
                  dum2 = vsthe(ncl, nelm)*kplamm*nammh
                  plamm(nelm, ncl) = MIN(dum1, dum2)

                  ! * calculation of the concentration of ammonium in solution
                  ! * at timestep n + 1
                  dum = -plamm(nelm, ncl) + miner(nelm, ncl) - imamm(nelm, ncl) - ntrf(nelm, ncl) - vol(nelm, ncl) + naamm(nelm, ncl)
                  namm1(nelm, ncl) = 1.0d0/(vsthe(ncl, nelm)*retamm1)*(vstheo(nelm, ncl)*namm(nelm, ncl)*retamm + dtuz*dum)

                  ! * ammonium conc at timestep n + 1/2 is calculated for use
                  ! * in the new calculation of the ammonium
                  nammh = (namm1(nelm, ncl) + namm(nelm, ncl))/2.0d0

                  ! * relative error between iterations to see if the
                  ! * iteration is converging.
                  IF (namm1(nelm, ncl) /= 0.0d0) THEN
                     werr1 = (namm1(nelm, ncl) - namm1o)/namm1(nelm, ncl)
                  ELSE IF (namm1o == 0.0d0) THEN
                     werr1 = 0.0d0
                  ELSE
                     werr1 = 1.0d0
                  END IF

                  ! * square of the errors, in order to make them positive
                  wer1sq = werr1*werr1
                  namm1o = namm1(nelm, ncl)

                  ! * break out of loop if the error in the iteration
                  ! * is less than the error tolerance
                  IF (wer1sq < errtol) EXIT iteration_loop

               END DO iteration_loop

               ! * If the DO loop ran all the way through to niters without
               ! * exiting early, it has failed to converge
               IF (ntime > niters) THEN
                  WRITE (msg, 9000) wer1sq
                  CALL RAISE_ERROR(warn, 3018, mnpr, 0, 0, msg)
               END IF

            END DO layer_loop
         END DO
      END DO

9000  FORMAT('iteration loop in mnamm failed to converge with error = ', g15.7)

   END SUBROUTINE mnamm

!> @brief Calculates net mineralisation or immobilisation for each active soil cell.
!>
!> Positive `gam` values represent net mineralisation and negative values
!> represent immobilisation demand. If immobilisation previously exceeded
!> available mineral nitrogen, litter and manure decomposition are temporarily
!> suppressed until mineralisation has repaid the stored deficit.
!>
!> The manual supplies `FE`, `FH`, `CNRBIO`, and `CNRHUM` in `MN12`, and the
!> depth-varying humus, litter, and manure decomposition parameters through
!> `MN15`-`MN20`. For a cell, the routine first averages old and new pool
!> values, for example \(\bar{C}_h = (C_h + C_h^1)/2\), and forms the
!> environmental reduction factor
!> over `NBOTCE:NCETOP` when `ISBOTC` is true, otherwise over
!> `NCOLMB(element):NCETOP`.
!>
!> \[
!> E = E_T E_\psi.
!> \]
!>
!> With \(K_l'\) and \(K_m'\) equal to `KLIT` and `KMAN` normally, but set to
!> zero while an earlier immobilisation deficit is being repaid, the raw net
!> mineralisation/immobilisation rate is
!>
!> \[
!> \begin{aligned}
!> \Gamma = E\{&
!> K_l'[\bar{N}_l - \bar{C}_l(1-FE)FH/CNRHUM
!>          - \bar{C}_l FE/CNRBIO]\\
!> &+ KHUM\,\bar{C}_h(1/CNRHUM - FE/CNRBIO)\\
!> &+ K_m'[\bar{N}_m - FE\,\bar{C}_m/CNRBIO]\}.
!> \end{aligned}
!> \]
!>
!> `GAMTMP` stores this raw \(\Gamma\). If `ISIMTF` is set, `IMDIFF` stores the
!> remaining immobilisation deficit. Mineralisation over the timestep first
!> repays that deficit: if \(\Gamma\Delta t \ge IMDIFF\), the exported `GAM`
!> becomes \((\Gamma\Delta t - IMDIFF)/\Delta t\) and the flag is cleared;
!> otherwise `IMDIFF` is reduced by \(\Gamma\Delta t\) and `GAM` is set to zero.
   SUBROUTINE MNGAM(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, CNRHUM, CNRBIO, FE, FH, DTUZ, ISBOTC)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: NBOTCE  !! Lowest cell included when bottom-cell truncation is active.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column updates.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      DOUBLE PRECISION, INTENT(IN) :: CNRBIO  !! Biomass carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: CNRHUM  !! Humus carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: FE  !! Efficiency fraction for organic carbon turnover.
      DOUBLE PRECISION, INTENT(IN) :: FH  !! Humification fraction.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      LOGICAL, INTENT(IN) :: ISBOTC  !! True when the fixed lower active cell `NBOTCE` is used.

      ! Locals
      INTEGER :: NBOTM, NELM, NCL
      DOUBLE PRECISION :: CHUMH, CLITH, CMANH, DUM, DUM1, ERF
      DOUBLE PRECISION :: KLITTP, KMANTP, NLITH, NMANH

      !-------------------------------------------------------------------*

      column_loop: DO NELM = NLF + 1, NEL

         ! Determine bottom cell boundary
         IF (ISBOTC) THEN
            NBOTM = NBOTCE
         ELSE
            NBOTM = NCOLMB(NELM)
         END IF

         cell_loop: DO NCL = NBOTM, NCETOP

            ! Calculate average concentrations
            CHUMH = (CHUM(NELM, NCL) + CHUM1(NELM, NCL))/2.0D0
            CLITH = (CLIT(NELM, NCL) + CLIT1(NELM, NCL))/2.0D0
            CMANH = (CMAN(NELM, NCL) + CMAN1(NELM, NCL))/2.0D0
            NLITH = (NLIT(NELM, NCL) + NLIT1(NELM, NCL))/2.0D0
            NMANH = (NMAN(NELM, NCL) + NMAN1(NELM, NCL))/2.0D0

            ! * if immobilisation is not equal to the potential
            ! * immobilisation then the decomposition of the litter pool
            ! * and the manure pool are temporarily stopped
            IF (ISIMTF(NELM, NCL)) THEN
               KLITTP = 0.0D0
               KMANTP = 0.0D0
            ELSE
               KLITTP = KLIT(NELM, NCL)
               KMANTP = KMAN(NELM, NCL)
            END IF

            ERF = EMT(NELM, NCL)*EMPH(NELM, NCL)

            DUM = KLITTP*ERF*(NLITH - CLITH*(1.0D0 - FE)*FH/CNRHUM - CLITH*FE/CNRBIO)
            DUM1 = DUM + KHUM(NELM, NCL)*ERF*CHUMH*(1.0D0/CNRHUM - FE/CNRBIO)

            GAM(NELM, NCL) = DUM1 + KMANTP*ERF*(NMANH - FE*CMANH/CNRBIO)

            ! * if potential immobilisation is greater than actual
            ! * immobilisation checks how much mineralisation has
            ! * compensated for the difference
            GAMTMP(NELM, NCL) = GAM(NELM, NCL)

            IF (ISIMTF(NELM, NCL)) THEN
               IF (GAM(NELM, NCL)*DTUZ >= IMDIFF(NELM, NCL)) THEN
                  GAM(NELM, NCL) = (GAM(NELM, NCL)*DTUZ - IMDIFF(NELM, NCL))/DTUZ
                  IMDIFF(NELM, NCL) = 0.0D0
                  ISIMTF(NELM, NCL) = .FALSE.
               ELSE
                  IMDIFF(NELM, NCL) = IMDIFF(NELM, NCL) - GAM(NELM, NCL)*DTUZ
                  GAM(NELM, NCL) = 0.0D0
               END IF
            END IF

         END DO cell_loop
      END DO column_loop

   END SUBROUTINE MNGAM

!> @brief Updates the litter nitrogen pool.
!>
!> Litter nitrogen is advanced with the same environmental reduction terms used
!> for carbon turnover, including immobilisation-limited suppression of
!> litter/manure decomposition. Non-convergence is reported as warning 3017.
!>
!> The manual supplies the biomass C:N ratio `CNRBIO` and efficiency fraction
!> `FE` in `MN12`; `CNRALT` is the litter C:N ratio from the active external
!> carbon input (`MNFC32`) after [[mnint2]] has converted the addition to a
!> cell-based rate. For each active cell the routine uses \(E = E_T E_\psi\)
!> and midpoint carbon pools from the updated carbon calculation. The active
!> vertical range is `NBOTCE:NCETOP` when `ISBOTC` is true, otherwise
!> `NCOLMB(element):NCETOP`.
!>
!> With \(K_l'\) and \(K_m'\) equal to `KLIT` and `KMAN` normally, but set to
!> zero while an immobilisation deficit is being repaid, the fixed-point
!> iteration solves
!>
!> \[
!> N_l^{n+1} = N_l^n + \Delta t\{-K_l'E\bar{N}_l
!>             + FE\,K_l'E\bar{C}_l/CNRBIO
!>             + FE\,KHUM\,E\bar{C}_h/CNRBIO
!>             + C_l^{add}/CNRALT
!>             + FE\,K_m'E\bar{C}_m/CNRBIO\}.
!> \]
!>
!> The midpoint nitrogen value is updated as
!> \(\bar{N}_l=(N_l^n+N_l^{n+1})/2\). Iteration stops when the squared relative
!> change in `NLIT1` is below `1D-12`. If convergence is not reached after 20
!> iterations the routine reports warning `3017` and leaves the last iterate in
!> place.
!>
!> @note `FH` is passed to this routine but is not used by the active
!> calculation.
!> @endnote
   SUBROUTINE mnltn(llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, cnrbio, fe, fh, dtuz, cnralt, isbotc)

      IMPLICIT NONE

      ! input arguments
      INTEGER, INTENT(IN) :: llee  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: mnpr  !! MN diagnostic output unit used for warning messages.
      INTEGER, INTENT(IN) :: nbotce  !! Lowest cell included when bottom-cell truncation is active.
      INTEGER, INTENT(IN) :: ncetop  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: nel  !! Number of elements.
      INTEGER, INTENT(IN) :: nelee  !! Element-array dimension.
      INTEGER, INTENT(IN) :: nlf  !! Number of overland/channel links excluded from land-column updates.
      INTEGER, INTENT(IN) :: ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      DOUBLE PRECISION, INTENT(IN) :: cnrbio  !! Biomass carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: fe  !! Efficiency fraction for organic carbon turnover.
      DOUBLE PRECISION, INTENT(IN) :: fh  !! Humification fraction; passed through but not used.
      DOUBLE PRECISION, INTENT(IN) :: dtuz  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: cnralt(nelee)  !! Element litter C:N ratio for active additions.
      LOGICAL, INTENT(IN) :: isbotc  !! True when the fixed lower active cell `NBOTCE` is used.

      ! locals
      INTEGER :: nbotm, ncl, nelm, niters, ntime, warn
      DOUBLE PRECISION :: chumh, clith, cmanh, dum, errtol, erf
      DOUBLE PRECISION :: klittp, kmantp, nlith
      DOUBLE PRECISION :: nlit1o, werr1, wer1sq
      CHARACTER(LEN=132) :: msg

      ! * parameters for the iteration loop within the subroutine
      PARAMETER(niters=20, warn=3)
      PARAMETER(errtol=1.0d-12)

      !-------------------------------------------------------------------*

      DO nelm = nlf + 1, nel
         IF (isbotc) THEN
            nbotm = nbotce
         ELSE
            nbotm = ncolmb(nelm)
         END IF

         layer_loop: DO ncl = nbotm, ncetop

            ! * initialise local variables
            chumh = (chum(nelm, ncl) + chum1(nelm, ncl))/2.0d0
            clith = (clit(nelm, ncl) + clit1(nelm, ncl))/2.0d0
            cmanh = (cman(nelm, ncl) + cman1(nelm, ncl))/2.0d0
            nlith = nlit(nelm, ncl)
            nlit1o = 0.0d0

            ! * if immobilisation is not equal to the potential
            ! * immobilisation then the decomposition of the litter pool
            ! * and the manure pool are temporarily stopped
            IF (isimtf(nelm, ncl)) THEN
               klittp = 0.0d0
               kmantp = 0.0d0
            ELSE
               klittp = klit(nelm, ncl)
               kmantp = kman(nelm, ncl)
            END IF

            erf = emt(nelm, ncl)*emph(nelm, ncl)

            ! * iteration loop to calculate the new nitrogen litter
            ! * concentrations
            iteration_loop: DO ntime = 1, niters

               dum = -klittp*erf*nlith + fe*klittp*erf*clith/cnrbio
               dum = dum + fe*khum(nelm, ncl)*erf*chumh/cnrbio + calit(nelm, ncl)/cnralt(nelm)
               dum = dum + fe*kmantp*erf*cmanh/cnrbio

               nlit1(nelm, ncl) = nlit(nelm, ncl) + dtuz*dum

               ! * litter conc at timestep n +1/2 is calculated for use
               ! * in the new calculation of the litter
               nlith = (nlit1(nelm, ncl) + nlit(nelm, ncl))/2.0d0

               ! * relative error between iterations to see if the
               ! * iteration is converging.
               IF (nlit1(nelm, ncl) /= 0.0d0) THEN
                  werr1 = (nlit1(nelm, ncl) - nlit1o)/nlit1(nelm, ncl)
               ELSE IF (nlit1o == 0.0d0) THEN
                  werr1 = 0.0d0
               ELSE
                  werr1 = 1.0d0
               END IF

               ! * square of the errors, in order to make them positive
               wer1sq = werr1*werr1

               nlit1o = nlit1(nelm, ncl)

               ! * break out of loop if the error in the iteration
               ! * is less than the error tolerance
               IF (wer1sq < errtol) EXIT iteration_loop

            END DO iteration_loop

            ! * the do loop has continued to niters and has thus
            ! * failed to converge
            IF (ntime > niters) THEN
               WRITE (msg, 9000) wer1sq
               CALL RAISE_ERROR(warn, 3017, mnpr, 0, 0, msg)
            END IF

         END DO layer_loop
      END DO

9000  FORMAT('iteration loop in mnltn failed to converge with error = ', g15.7)

   END SUBROUTINE mnltn

!> @brief Calculates nitrate source/sink terms for dynamic and dead-space water.
!>
!> The nitrate balance combines immobilisation, denitrification, plant uptake,
!> nitrification input from ammonium, fertiliser input, and the mobile/immobile
!> partitioning factor. The resulting rates are converted to the
!> non-dimensional `sss1` and `sss2` source terms used by the contaminant
!> transport solver.
!>
!> The manual supplies nitrate immobilisation and plant uptake constants
!> `KUNIT` and `KPLNIT` in `MN11`, and denitrification parameters `KD1` and
!> `KD2` through `MN25`-`MN28`. For each active cell the routine uses the
!> average water content \(\bar{\theta}=(\theta^n+\theta^{n+1})/2\), average
!> ammonium \(\bar{N}_{amm}\), dynamic nitrate \(N_d\), dead-space nitrate
!> \(N_s\), and mobile fraction \(\phi_m\). The active vertical range is
!> `NBOTCE:NCETOP` when `ISBOTC` is true, otherwise
!> `NCOLMB(element):NCETOP`.
!>
!> If net mineralisation `GAM` is negative, nitrate immobilisation is limited by
!> both the remaining immobilisation demand after ammonium immobilisation and
!> first-order nitrate availability:
!>
!> \[
!> I_d = \min(-GAM-I_{amm}, KUNIT\,N_d),\qquad
!> I_s = \min(-GAM-I_{amm}, KUNIT\,N_s),
!> \]
!>
!> otherwise \(I_d=I_s=0\). Denitrification is
!>
!> \[
!> D_d = \bar{\theta}\min(KD1\,E_T\,E_\theta\,C_{dort}, KD2\,N_d),
!> \qquad
!> D_s = \bar{\theta}\min(KD1\,E_T\,E_\theta\,C_{dort}, KD2\,N_s).
!> \]
!>
!> Plant nitrate uptake is limited by the plant demand share and by first-order
!> uptake:
!>
!> \[
!> P_d = \min\left(PLUP\,\frac{N_d}{N_d+\bar{N}_{amm}},
!>                 \bar{\theta}KPLNIT\,N_d\right),
!> \]
!>
!> with the same expression for \(P_s\) using \(N_s\); the demand-share term is
!> zero when the corresponding nitrate concentration is zero. The dynamic and
!> dead-space nitrate rates are then
!>
!> \[
!> R_d = -P_d + NTRF - D_d - I_d + N_{nit}^{add},\qquad
!> R_s = -P_s + NTRF - D_s - I_s + N_{nit}^{add}.
!> \]
!>
!> They are partitioned and converted to contaminant-source terms as
!>
!> \[
!> SSS1 = -\frac{\phi_m R_d Z2^2}{D0\,MNCREF},\qquad
!> SSS2 = -\frac{(1-\phi_m)R_s Z2^2}{D0\,MNCREF}.
!> \]
!>
!> Diagnostic totals are stored as weighted sums: `DENIT`, `PLNIT`, `SNIT`, and
!> `IMNIT`. If total actual immobilisation remains less than the potential
!> demand \(-GAM\), `ISIMTF` is set and `IMDIFF` stores the remaining deficit
!> over the current timestep.
!>
!> When `ISBOTC` is true, source/sink terms below the real column bottom and
!> above `NBOTCE` are explicitly zeroed after the active range is processed.
   subroutine mnnit (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,d0,kplnit,kunit,mncref,z2,dtuz,vsthe,vstheo,isbotc,sss1,sss2)

      integer llee  !! Maximum soil-cell dimension.
      integer nbotce  !! Lowest cell included when bottom-cell truncation is active.
      integer ncetop  !! Top soil-cell index.
      integer nel  !! Number of elements.
      integer nelee  !! Element-array dimension.
      integer nlf  !! Number of overland/channel links excluded from land-column updates.
      integer ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      double precision d0  !! Reference diffusion/dispersion scale used by CM.
      double precision kplnit  !! First-order nitrate plant-uptake limit.
      double precision kunit  !! First-order nitrate immobilisation limit.
      double precision mncref  !! Reference nitrogen concentration.
      double precision z2  !! Vertical length scale used by CM source conversion.
      double precision dtuz  !! Unsaturated-zone timestep in seconds.
      !double precision cdort(nelee,llee),edeth(nelee,llee)
      !double precision emt(nelee,llee),gam(nelee,llee)
      !double precision imamm(nelee,llee)
      !double precision kd1(nelee,llee),kd2(nelee,llee)
      !double precision namm(nelee,llee)
      !double precision namm1(nelee,llee)
      !double precision nanit(nelee,llee),ndnit(nelee,llee)
      !double precision ndsnt(nelee,llee)
      !double precision ntrf(nelee,llee),plup(nelee,llee)
      !double precision pphi(nelee,llee)
      double precision vsthe(ncetop, nel)  !! Current volumetric water content.
      double precision vstheo(nel, ncetop + 1)  !! Previous volumetric water content.
      logical isbotc  !! True when the fixed lower active cell `NBOTCE` is used.
      !
      ! input/output arguments
      !double precision imdiff(nelee,llee)
      !logical isimtf(nelee,llee)
      !
      ! output arguments
      !double precision denit(nelee,llee)
      !double precision imnit(nelee,llee)
      !double precision plnit(nelee,llee),snit(nelee,llee)
      double precision sss1(nel, ncetop + 1)  !! Dynamic-region CM source/sink array.
      double precision sss2(nel, ncetop + 1)  !! Dead-space CM source/sink array.
      ! locals
      integer nbotm, ncl, nelm
      double precision dednt, dedsnt, dum1, dum2, imdnt, imdsnt, imrat
      double precision nammh, pldnt, pldsnt, s1, s2, sdnit, sdsnt, ttheth
      !
      !
      !-------------------------------------------------------------------*
      !
      do nelm = nlf + 1, nel
         if (isbotc) then
            nbotm = nbotce
         else
            nbotm = ncolmb(nelm)
         end if
         do ncl = nbotm, ncetop
            !
            !           * initialisation of local variable
            ttheth = (vsthe(ncl, nelm) + vstheo(nelm, ncl))/2.0d0
            nammh = (namm(nelm, ncl) + namm1(nelm, ncl))/2.0d0
            !
            !
            !           * calculation of immobilisation rate of dynamic
            !           * region nitrate
            if (gam(nelm, ncl) >= 0.0d0) then
               imdnt = 0.0d0
               imdsnt = 0.0d0
            else
               imdnt = min(-gam(nelm, ncl) - imamm(nelm, ncl), kunit*ndnit(nelm, ncl))
               imdsnt = min(-gam(nelm, ncl) - imamm(nelm, ncl), kunit*ndsnt(nelm, ncl))
            end if
            !
            !           * calculation of the denitrification rate
            dednt = ttheth*min(kd1(nelm, ncl)*emt(nelm, ncl)*edeth(nelm, ncl)*cdort(nelm, ncl), kd2(nelm, ncl)*ndnit(nelm &
               , ncl))
            dedsnt = ttheth*min(kd1(nelm, ncl)*emt(nelm, ncl)*edeth(nelm, ncl)*cdort(nelm, ncl), kd2(nelm, ncl)*ndsnt(nelm, &
               ncl))
            denit(nelm, ncl) = pphi(nelm, ncl)*dednt + (1 - pphi(nelm, ncl))*dedsnt
            !
            !           * calculation of the plant uptake rate of dynamic
            !           * region nitrate
            if (ndnit(nelm, ncl) > 0.0d0) then
               dum1 = plup(nelm, ncl)*ndnit(nelm, ncl)/(ndnit(nelm, ncl) + nammh)
            else
               dum1 = 0.0d0
            end if
            dum2 = ttheth*kplnit*ndnit(nelm, ncl)
            pldnt = min(dum1, dum2)
            !
            !           * calculation of the plant uptake rate of dead space
            !           * region nitrate
            if (ndsnt(nelm, ncl) > 0.0d0) then
               dum1 = plup(nelm, ncl)*ndsnt(nelm, ncl)/(ndsnt(nelm, ncl) + nammh)
            else
               dum1 = 0.0d0
            end if
            dum2 = ttheth*kplnit*ndsnt(nelm, ncl)
            pldsnt = min(dum1, dum2)
            plnit(nelm, ncl) = pphi(nelm, ncl)*pldnt + (1 - pphi(nelm, ncl))*pldsnt
            !
            !
            !           * calculation of the source/sink term of dynamic region
            !           * nitrate at timestep n + 1
            sdnit = -pldnt + ntrf(nelm, ncl) - dednt - imdnt + nanit(nelm, ncl)
            s1 = pphi(nelm, ncl)*sdnit
            !
            !           * non dimensinal source/sink term
            sss1(nelm, ncl) = -s1*z2*z2/(d0*mncref)
            !
            !
            !           * calculation of the source/sink term for dead space region
            !           * nitrate at timestep n + 1
            sdsnt = -pldsnt + ntrf(nelm, ncl) - dedsnt - imdsnt + nanit(nelm, ncl)
            s2 = (1 - pphi(nelm, ncl))*sdsnt
            !
            !           * non dimensinal source/sink term
            sss2(nelm, ncl) = -s2*z2*z2/(d0*mncref)
            !
            snit(nelm, ncl) = s1 + s2
            !
            !           * immobilisation rate
            imnit(nelm, ncl) = pphi(nelm, ncl)*imdnt + (1.0d0 - pphi(nelm, ncl))*imdsnt
            !
            imrat = imamm(nelm, ncl) + imnit(nelm, ncl)
            !
            !           * tests if the ponential immobilisation is greater than the
            !           * actual immobilisation
            if (-gam(nelm, ncl) > imrat) then
               isimtf(nelm, ncl) = .true.
               imdiff(nelm, ncl) = (-gam(nelm, ncl) - imrat)*dtuz
            end if
            !
         end do
         !
         if (isbotc) then
            do ncl = ncolmb(nelm), nbotce - 1
               sss1(nelm, ncl) = 0.0d0
               sss2(nelm, ncl) = 0.0d0
            end do
         end if
         !
      end do
   end subroutine mnnit

END MODULE mn_nitrogen

