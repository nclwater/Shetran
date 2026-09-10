!> summary: Soil temperature and the temperature and moisture response factors.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> The environmental factors that scale every turnover rate in the component.
!> [[MNTEMP]] solves the soil-temperature profile; the five short `mnE*`
!> functions return the temperature and moisture-potential multipliers applied
!> to mineralisation, nitrification and denitrification.
!>
!> They are gathered here because they are read by all four process modules and
!> write nothing but their own factors, so they can be understood without the
!> turnover formulations that use them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> | 2026-09-10 | SvB | - | Split out of MNmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mn_environment

   USE linear_algebra, ONLY: TRIDAG
   USE mn_state, ONLY: edeth, emph, emt, enph, ent, temp

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: mnedth, mnemph, mnemt, mnenph, mnent, mntemp

CONTAINS

!> @brief Calculates the water-content reduction factor for denitrification.
!>
!> The manual defines the spatial denitrification parameters `KD1` and `KD2`
!> through the `MN25`-`MN28` category/depth tables. This routine supplies the
!> separate moisture response multiplier used with those parameters. For each
!> active land-column cell it forms the relative saturation
!>
!> \[
!> S_r = \frac{\theta}{\phi}
!> \]
!>
!> from `VSTHE` (`\theta`, volumetric water content) and `VSPOR` (`\phi`, soil
!> porosity), then applies the legacy segmented relationship
!>
!> \[
!> E_\theta =
!> \begin{cases}
!> 1, & S_r > 1,\\
!> -7 + 8S_r, & 0.9 < S_r \le 1,\\
!> -1.6 + 2S_r, & 0.8 < S_r \le 0.9,\\
!> 0, & S_r \le 0.8.
!> \end{cases}
!> \]
!>
!> Thus denitrification is switched off at or below 80 percent saturation,
!> increases linearly to 0.2 between 80 and 90 percent saturation, increases
!> linearly to 1.0 between 90 percent saturation and saturation, and remains
!> capped at 1.0 above saturation.
!>
!> The active vertical range follows the module convention: `NBOTCE:NCETOP` when
!> `ISBOTC` is true, otherwise `NCOLMB(element):NCETOP`, with lower bounds also
!> clipped to the current soil-layer base in the layer loop.
   SUBROUTINE mnedth(llee, nbotce, ncetop, nel, nelee, nlf, nlyree, ns, &
      ncolmb, nlyr, nlyrbt, ntsoil, vsthe, vspor, isbotc)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: llee  !! Maximum soil-cell dimension.
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
      DOUBLE PRECISION, INTENT(IN) :: vsthe(ncetop, nel)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: vspor(ns)  !! Soil porosity by soil type.
      LOGICAL, INTENT(IN) :: isbotc  !! True when the fixed lower active cell `NBOTCE` is used.

      ! Locals
      INTEGER :: jlyr, jsoil, nbotm, nce, ncebot, nelm
      DOUBLE PRECISION :: relsat

      !-------------------------------------------------------------------*

      element_loop: DO nelm = nlf + 1, nel

         IF (isbotc) THEN
            nbotm = nbotce
         ELSE
            nbotm = ncolmb(nelm)
         END IF

         ncebot = nbotm

         layer_loop: DO jlyr = 1, nlyr(nelm)
            jsoil = ntsoil(nelm, jlyr)

            cell_loop: DO nce = MAX(ncebot, nlyrbt(nelm, jlyr)), nlyrbt(nelm, jlyr + 1) - 1

               ! A segmented relationship is being used with the
               ! relative saturation falling into one of four bands
               relsat = vsthe(nce, nelm)/vspor(jsoil)

               IF (relsat > 1.0d0) THEN
                  edeth(nelm, nce) = 1.0d0
               ELSE IF (relsat > 0.9d0) THEN
                  edeth(nelm, nce) = -7.0d0 + 8.0d0*relsat
               ELSE IF (relsat > 0.8d0) THEN
                  edeth(nelm, nce) = -1.6d0 + 2.0d0*relsat
               ELSE
                  edeth(nelm, nce) = 0.0d0
               END IF

            END DO cell_loop
         END DO layer_loop
      END DO element_loop

   END SUBROUTINE mnedth

!> @brief Calculates the matric-potential reduction factor for mineralisation.
!>
!> The manual supplies the humus, litter, and manure decomposition parameter
!> fields through `MN15`-`MN20`, with optional Q10 temperature controls for
!> mineralisation in `MN35`/`MN35a`. This routine supplies the separate matric-
!> potential multiplier applied to mineralisation. For each active land-column
!> cell it evaluates the pressure head/matric potential `\psi` from `VSPSI`
!> and stores
!>
!> \[
!> E_\psi =
!> \begin{cases}
!> 0.6, & \psi > -0.01,\\
!> 1.05 + 0.225\log_{10}(-\psi), & -0.6 < \psi \le -0.01,\\
!> 1.0, & -3.0 < \psi \le -0.6,\\
!> 1.136 - 0.284\log_{10}(-\psi), & -10000 < \psi \le -3.0,\\
!> 0.0, & \psi \le -10000.
!> \end{cases}
!> \]
!>
!> The response is therefore reduced in very wet cells, reaches its maximum
!> over the intermediate matric-potential range, and declines to zero under
!> very dry conditions.
!>
!> The active vertical range is `NBOTCE:NCETOP` when `ISBOTC` is true, otherwise
!> `NCOLMB(element):NCETOP`.
   subroutine mnemph(llee, nbotce, ncetop, nel, nelee, nlf, ncolmb, vspsi, isbotc)

      integer llee  !! Maximum soil-cell dimension.
      integer nbotce  !! Lowest cell included when bottom-cell truncation is active.
      integer ncetop  !! Top soil-cell index.
      integer nel  !! Number of elements.
      integer nelee  !! Element-array dimension.
      integer nlf  !! Number of overland/channel links excluded from land-column updates.
      integer ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      double precision vspsi(ncetop, nel)  !! Matric potential/pressure head by cell and element.
      logical isbotc  !! True when the fixed lower active cell `NBOTCE` is used.
      !
      !
      ! output arguments
      !double precision emph(nelee,llee)
      !
      ! locals
      integer nbotm, ncl, nelm
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
            !          * a segmented relationship is being used with the
            !          * matric potential falling into one of five bands
            if (vspsi(ncl, nelm) > -0.1d-1) then
               emph(nelm, ncl) = 0.6
            elseif (vspsi(ncl, nelm) > -0.6d0) then
               emph(nelm, ncl) = 1.05d0 + 0.225d0*log10(-vspsi(ncl, nelm))
            elseif (vspsi(ncl, nelm) > -3.0d0) then
               emph(nelm, ncl) = 1.0d0
            elseif (vspsi(ncl, nelm) > -1.0d4) then
               emph(nelm, ncl) = 1.136d0 - 0.284d0*log10(-vspsi(ncl, nelm))
            else
               emph(nelm, ncl) = 0.0d0
            end if
            !
         end do
      end do
      !
   end subroutine mnemph

!> @brief Calculates the temperature reduction factor for mineralisation.
!>
!> The manual's `MN35` flag (`ISQ10`) selects whether temperature reduction
!> factors use a Q10 function, and `MN35a` supplies `Q10M` for mineralisation
!> when that option is enabled. If `ISQ10` is true, this routine stores
!>
!> \[
!> E_T = Q10M^{(T - 30) / 10}
!> \]
!>
!> where `T` is the cell temperature in `TEMP`. If `ISQ10` is false, the legacy
!> segmented temperature response is used:
!>
!> \[
!> E_T =
!> \begin{cases}
!> 1.0, & T \ge 30,\\
!> -0.5 + 0.05T, & 20 < T < 30,\\
!> -0.1 + 0.03T, & 10 < T \le 20,\\
!> 0.02T, & 0 < T \le 10,\\
!> 0.0, & T \le 0.
!> \end{cases}
!> \]
!>
!> The Q10 branch is used exactly as written and is not capped at 1.0 for
!> temperatures above 30 degrees C. The active vertical range is `NBOTCE:NCETOP`
!> when `ISBOTC` is true, otherwise `NCOLMB(element):NCETOP`.
   subroutine mnemt(llee, nbotce, ncetop, nel, nelee, nlf, ncolmb, q10m, isbotc, isq10)

      integer llee  !! Maximum soil-cell dimension.
      integer nbotce  !! Lowest cell included when bottom-cell truncation is active.
      integer ncetop  !! Top soil-cell index.
      integer nel  !! Number of elements.
      integer nelee  !! Element-array dimension.
      integer nlf  !! Number of overland/channel links excluded from land-column updates.
      integer ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      double precision q10m  !! Q10 coefficient for mineralisation temperature response.
      !temp(nelee,llee)
      logical isbotc  !! True when the fixed lower active cell `NBOTCE` is used.
      logical isq10  !! True when Q10 temperature response is selected.
      !
      ! output arguments
      !double precision emt(nelee,llee)
      !
      ! locals
      integer nbotm, ncl, nelm
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
            !
            !         * the reduction factor can be calculated either using a segmented
            !         * relationship or a q10 factor
            if (isq10) then
               emt(nelm, ncl) = q10m**((temp(nelm, ncl) - 30.0d0)/10.0d0)
               !
            else
               !             * a segmented relationship is being used with the
               !             * temperature falling into one of five bands
               if (temp(nelm, ncl) >= 30.0d0) then
                  emt(nelm, ncl) = 1.0d0
               elseif (temp(nelm, ncl) > 20.0d0) then
                  emt(nelm, ncl) = -0.5d0 + 0.5d-1*temp(nelm, ncl)
               elseif (temp(nelm, ncl) > 10.0d0) then
                  emt(nelm, ncl) = -0.1d0 + 0.3d-1*temp(nelm, ncl)
               elseif (temp(nelm, ncl) > 0.0d0) then
                  emt(nelm, ncl) = 0.2d-1*temp(nelm, ncl)
               else
                  emt(nelm, ncl) = 0.0d0
               end if
               !
            end if
            !
         end do
      end do
      !
   end subroutine mnemt

!> @brief Calculates the matric-potential reduction factor for nitrification.
!>
!> The manual supplies the spatial nitrification parameter field through the
!> `MN21`/`MN22` category and depth tables, with optional Q10 temperature
!> controls in `MN35`/`MN35a`. This routine supplies the separate matric-
!> potential multiplier applied to nitrification. For each active land-column
!> cell it evaluates the pressure head/matric potential `\psi` from `VSPSI`
!> and stores
!>
!> \[
!> E_\psi =
!> \begin{cases}
!> 0.6, & \psi > -0.01,\\
!> 1.05 + 0.225\log_{10}(-\psi), & -0.6 < \psi \le -0.01,\\
!> 1.0, & -3.0 < \psi \le -0.6,\\
!> 1.136 - 0.284\log_{10}(-\psi), & -10000 < \psi \le -3.0,\\
!> 0.0, & \psi \le -10000.
!> \end{cases}
!> \]
!>
!> The active implementation therefore keeps nitrification partly active under
!> very wet conditions, reaches its maximum over the intermediate matric-
!> potential range, and declines to zero under very dry conditions.
!>
!> @history
!>
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 1996-01-22 | Legacy MN development | Replaced the older wet-condition response with the active values above, including `0.6` in the wettest band. |
!> @endhistory
   subroutine mnenph(llee, nbotce, ncetop, nel, nelee, nlf, ncolmb, vspsi, isbotc)

      integer llee  !! Maximum soil-cell dimension.
      integer nbotce  !! Lowest cell included when bottom-cell truncation is active.
      integer ncetop  !! Top soil-cell index.
      integer nel  !! Number of elements.
      integer nelee  !! Element-array dimension.
      integer nlf  !! Number of overland/channel links excluded from land-column updates.
      integer ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      double precision vspsi(ncetop, nel)  !! Matric potential/pressure head by cell and element.
      logical isbotc  !! True when the fixed lower active cell `NBOTCE` is used.
      !
      !
      ! output arguments
      !double precision enph(nelee,llee)
      !
      ! locals
      integer nbotm, ncl, nelm
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
            !           * a segmented relationship is being used with the
            !           * matric potential falling into one of five bands
            !
            if (vspsi(ncl, nelm) > -0.1d-1) then
               enph(nelm, ncl) = 0.6
            elseif (vspsi(ncl, nelm) > -0.6d0) then
               enph(nelm, ncl) = 1.05d0 + 0.225d0*log10(-vspsi(ncl, nelm))
            elseif (vspsi(ncl, nelm) > -3.0d0) then
               enph(nelm, ncl) = 1.0d0
            elseif (vspsi(ncl, nelm) > -1.0d4) then
               enph(nelm, ncl) = 1.136d0 - 0.284d0*log10(-vspsi(ncl, nelm))
            else
               enph(nelm, ncl) = 0.0d0
            end if
            !
         end do
      end do
      !
   end subroutine mnenph

!> @brief Calculates the temperature reduction factor for nitrification.
!>
!> The manual's `MN35` flag (`ISQ10`) selects whether temperature reduction
!> factors use a Q10 function, and `MN35a` supplies `Q10N` for nitrification
!> when that option is enabled. If `ISQ10` is true, this routine stores
!>
!> \[
!> E_T = Q10N^{(T - 30) / 10}
!> \]
!>
!> where `T` is the cell temperature in `TEMP`. If `ISQ10` is false, the legacy
!> segmented temperature response is used:
!>
!> \[
!> E_T =
!> \begin{cases}
!> 1.0, & T \ge 30,\\
!> -0.5 + 0.05T, & 20 < T < 30,\\
!> -0.1 + 0.03T, & 10 < T \le 20,\\
!> -0.05 + 0.025T, & 2 < T \le 10,\\
!> 0.0, & T \le 2.
!> \end{cases}
!> \]
!>
!> The Q10 branch is used exactly as written and is not capped at 1.0 for
!> temperatures above 30 degrees C. The active vertical range is `NBOTCE:NCETOP`
!> when `ISBOTC` is true, otherwise `NCOLMB(element):NCETOP`.
   subroutine mnent(llee, nbotce, ncetop, nel, nelee, nlf, ncolmb, q10n, isbotc, isq10)

      integer llee  !! Maximum soil-cell dimension.
      integer nbotce  !! Lowest cell included when bottom-cell truncation is active.
      integer ncetop  !! Top soil-cell index.
      integer nel  !! Number of elements.
      integer nelee  !! Element-array dimension.
      integer nlf  !! Number of overland/channel links excluded from land-column updates.
      integer ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      double precision q10n  !! Q10 coefficient for nitrification temperature response.
      !temp(nelee,llee)
      logical isbotc  !! True when the fixed lower active cell `NBOTCE` is used.
      logical isq10  !! True when Q10 temperature response is selected.
      !
      ! output arguments
      !double precision ent(nelee,llee)
      !
      ! locals
      integer nbotm, ncl, nelm
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
            !
            !           * the reduction factor can be calculated either using a segmented
            !           * relationship or a q10 factor
            if (isq10) then
               ent(nelm, ncl) = q10n**((temp(nelm, ncl) - 30.0d0)/10.0d0)
               !
            else
               !             * a segmented relationship is being used with the
               !             * temperature falling into one of five bands
               if (temp(nelm, ncl) >= 30.0d0) then
                  ent(nelm, ncl) = 1.0d0
               elseif (temp(nelm, ncl) > 20.0d0) then
                  ent(nelm, ncl) = -0.5d0 + 0.5d-1*temp(nelm, ncl)
               elseif (temp(nelm, ncl) > 10.0d0) then
                  ent(nelm, ncl) = -0.1d0 + 0.3d-1*temp(nelm, ncl)
               elseif (temp(nelm, ncl) > 2.0d0) then
                  ent(nelm, ncl) = -0.5d-1 + 0.25d-1*temp(nelm, ncl)
               else
                  ent(nelm, ncl) = 0.0d0
               end if
               !
            end if
            !
         end do
      end do
      !
   end subroutine mnent

!> @brief Updates soil temperature for the MN environmental response factors.
!>
!> `mntemp` solves a one-dimensional heat-diffusion profile with prescribed
!> surface air temperature and a fixed deep boundary temperature, then maps the
!> solved profile onto each active SHETRAN soil cell.
!>
!> The driving air temperature is `TA`, read from the manual's meteorological
!> input records. The routine uses the first meteorological site's air
!> temperature and sets the ground-surface boundary to
!>
!> \[
!> T_1 = T_{air} + 2.
!> \]
!>
!> The internal temperature profile has `NUM = 11` nodes, initialised to
!> 12 deg C and saved between calls. With thermal diffusivity
!> `DIFF = 2D-5`, timestep `DTUZ`, and model depth scale `Z2`, the diffusion
!> coefficient used in the finite-difference equations is
!>
!> \[
!> k = DIFF\left(\frac{NUM-1}{Z2}\right)^2 .
!> \]
!>
!> For unknown node tendencies \(\omega_i\), where
!> \(T_i^{n+1}=T_i^n+\Delta t\,\omega_i\), the interior tridiagonal rows solve
!>
!> \[
!> -k\Delta t\,\omega_{i-1} + (1+2k\Delta t)\omega_i
!> -k\Delta t\,\omega_{i+1}
!> = k(T_{i-1}^n-2T_i^n+T_{i+1}^n).
!> \]
!>
!> The first unknown node uses the prescribed surface temperature \(T_1\) in
!> the right-hand side. The deepest node uses a one-sided lower boundary:
!>
!> \[
!> -k\Delta t\,\omega_{N-1} + (1+k\Delta t)\omega_N
!> = k(T_{N-1}^n-T_N^n).
!> \]
!>
!> After [[tridag]] solves the tridiagonal system, the routine places the
!> temperature nodes at equal 1 m intervals from 0 to `DEPTHC = 10` m and
!> linearly interpolates the solved profile to each SHETRAN cell-centre depth.
!> Cells deeper than the deepest temperature node are assigned the deepest-node
!> temperature.
!>
!> Cell depths are accumulated from the top cell downward over
!> `NCOLMB(element):NCETOP`; this routine does not use `ISBOTC`/`NBOTCE`.
!> After all columns are mapped, the saved temperature profile `TEMPR` is
!> replaced by the newly solved profile for the next call.
!>
!> @note Although `TA` originates in the meteorological state, the only current
!> caller is [[mncont]], which first sets every `TA(1:NV)` value to 10 deg C.
!> Consequently this routine currently receives 10 deg C and prescribes a
!> 12 deg C surface boundary on every call.
!> @endnote
   SUBROUTINE MNTEMP(LLEE, NCETOP, NEL, NELEE, NLF, NV, NCOLMB, Z2, DELTAZ, ZVSNOD, DTUZ, TA)

      USE linear_algebra, ONLY: TRIDAG

      IMPLICIT NONE

      ! * input arguments
      ! * static
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column updates.
      INTEGER, INTENT(IN) :: NV  !! Number of vegetation/meteorological temperature entries.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      DOUBLE PRECISION, INTENT(IN) :: Z2  !! Vertical length scale for the temperature diffusion calculation.
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: ZVSNOD(LLEE, NEL)  !! Vertical node elevation/depth by cell and element.

      ! * varying
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: TA(NV)  !! Air temperature input; only the first value is used.

      ! locals etc
      INTEGER :: IEL, NCE, NCEBOT, NCELLS, NNUM, NSERCH
      INTEGER, PARAMETER :: NUM = 11

      DOUBLE PRECISION :: CELLDP, CELLFC, KFCT, GRDTEM
      DOUBLE PRECISION :: AMAT(NUM), BMAT(NUM), CMAT(NUM), DEPTH(NUM)
      DOUBLE PRECISION :: RHS(NUM), OME(NUM), TEMPR1(NUM)

      DOUBLE PRECISION, PARAMETER :: DEPTHC = 10.0D0
      DOUBLE PRECISION, PARAMETER :: DIFF = 2.0D-5
      DOUBLE PRECISION, PARAMETER :: DIFFGA = 2.0D0

      ! Saved temperature profile carried between timesteps.
      DOUBLE PRECISION, SAVE :: TEMPR(NUM) = 12.0D0

      !--------------------------------------------------------------------*

      KFCT = DIFF*((NUM - 1.0D0)/Z2)*((NUM - 1.0D0)/Z2)

      ! * ground temperature is equal to the air temperature plus a
      ! * constant value
      GRDTEM = TA(1) + DIFFGA
      TEMPR1(1) = GRDTEM

      ! * position in the matrix are one lower than in the column,
      ! * this is because the ground surface value is known
      RHS(1) = KFCT*GRDTEM + KFCT*(-2.0D0*TEMPR(2) + TEMPR(3))
      RHS(NUM - 1) = (TEMPR(NUM - 1) - TEMPR(NUM))*KFCT

      AMAT(1) = 0.0D0
      BMAT(1) = 1.0D0 + 2.0D0*KFCT*DTUZ
      CMAT(1) = -KFCT*DTUZ

      AMAT(NUM - 1) = -KFCT*DTUZ
      BMAT(NUM - 1) = 1.0D0 + KFCT*DTUZ
      CMAT(NUM - 1) = 0.0D0

      DO NCE = 2, NUM - 2
         AMAT(NCE) = -KFCT*DTUZ
         BMAT(NCE) = 1.0D0 + 2.0D0*KFCT*DTUZ
         CMAT(NCE) = -KFCT*DTUZ
         RHS(NCE) = KFCT*(TEMPR(NCE) - 2.0D0*TEMPR(NCE + 1) + TEMPR(NCE + 2))
      END DO

      CALL TRIDAG(AMAT, BMAT, CMAT, RHS, OME, NUM - 1)

      ! * new temperature at each node
      DO NCE = 2, NUM
         TEMPR1(NCE) = TEMPR(NCE) + OME(NCE - 1)*DTUZ
      END DO

      ! * depth of each node
      DEPTH(1) = 0.0D0
      DO NNUM = 2, NUM
         DEPTH(NNUM) = DEPTHC/DBLE(NUM - 1) + DEPTH(NNUM - 1)
      END DO

      element_loop: DO IEL = NLF + 1, NEL
         NCEBOT = NCOLMB(IEL)
         NSERCH = 2

         cell_loop: DO NCE = NCETOP, NCEBOT, -1
            ! * calculation of the depth of the cell
            IF (NCE == NCETOP) THEN
               CELLDP = 0.5D0*DELTAZ(NCE, IEL)
            ELSE
               CELLDP = (ZVSNOD(NCE + 1, IEL) - ZVSNOD(NCE, IEL)) + CELLDP
            END IF

            IF (CELLDP >= DEPTH(NUM)) THEN
               DO NCELLS = NCE, NCEBOT, -1
                  TEMP(IEL, NCELLS) = TEMPR1(NUM)
               END DO
               EXIT cell_loop
            END IF

            ! * which two temperature nodes is the cell between ?
            search_loop: DO NNUM = NSERCH, NUM
               IF (CELLDP <= DEPTH(NNUM)) THEN
                  NSERCH = NNUM
                  EXIT search_loop
               END IF
            END DO search_loop

            ! * linear interpolation between the temperature nodes
            CELLFC = (CELLDP - DEPTH(NSERCH - 1))/(DEPTH(NSERCH) - DEPTH(NSERCH - 1))
            TEMP(IEL, NCE) = (1.0D0 - CELLFC)*TEMPR1(NSERCH - 1) + CELLFC*TEMPR1(NSERCH)
         END DO cell_loop
      END DO element_loop

      ! Update the saved temperature state for the next timestep
      TEMPR(1:NUM) = TEMPR1(1:NUM)

   END SUBROUTINE MNTEMP

END MODULE mn_environment

