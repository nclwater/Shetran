!> summary: Litter, humus and manure carbon turnover, and the CO2 released by it.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> The carbon half of the component: [[mnlthm]] advances the litter and humus
!> pools, [[mnman]] the manure pool, and [[mnco2]] accounts for the carbon
!> dioxide the turnover releases. The nitrogen released alongside it is handled
!> in [[mn_nitrogen]].
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> | 2026-09-10 | SvB | - | Split out of MNmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mn_organic_matter

   USE error_reporting, ONLY: RAISE_ERROR
   USE mn_state, ONLY: cahum, calit, caman, cdort, chum, chum1, clit, clit1, cman, cman1, &
                       emph, emt, isimtf, khum, klit, kman, nman, nman1

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: mnco2, mnlthm, mnman

CONTAINS

!> @brief Calculates cumulative carbon dioxide production from organic matter turnover.
!>
!> The calculation combines humus, litter, and manure carbon pools with
!> temperature and matric-potential modifiers and suppresses litter/manure
!> decomposition where immobilisation is limiting.
!>
!> For each active land-column cell the routine uses average old/new carbon
!> pools:
!>
!> \[
!> C_h=\frac{CHUM+CHUM1}{2},\quad
!> C_l=\frac{CLIT+CLIT1}{2},\quad
!> C_m=\frac{CMAN+CMAN1}{2}.
!> \]
!>
!> If `ISIMTF` is true, litter and manure decomposition rates are temporarily
!> set to zero. Otherwise the stored `KLIT` and `KMAN` rates are used. Carbon
!> dioxide production is then
!>
!> \[
!> CDORT = (1-FE)(1-FH)K_{lit}EMT\,EMPH\,C_l
!>       + (1-FE)KHUM\,EMT\,EMPH\,C_h
!>       + (1-FE)K_{man}EMT\,EMPH\,C_m .
!> \]
   subroutine mnco2(llee, nbotce, ncetop, nel, nelee, nlf, ncolmb, fe, fh, isbotc)

      integer llee  !! Maximum soil-cell dimension.
      integer nbotce  !! Lowest cell included when bottom-cell truncation is active.
      integer ncetop  !! Top soil-cell index.
      integer nel  !! Number of elements.
      integer nelee  !! Element-array dimension.
      integer nlf  !! Number of overland/channel links excluded from land-column updates.
      integer ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      double precision fe  !! Efficiency fraction for organic carbon turnover.
      double precision fh  !! Humification fraction.
      !double precision chum(nelee,llee)
      !double precision chum1(nelee,llee),clit(nelee,llee)
      !double precision clit1(nelee,llee),cman(nelee,llee)
      !double precision cman1(nelee,llee)
      !double precision emph(nelee,llee),emt(nelee,llee)
      !double precision khum(nelee,llee),klit(nelee,llee)
      !double precision kman(nelee,llee)
      logical isbotc  !! True when the fixed lower active cell `NBOTCE` is used.
      !logical isimtf(nelee,llee)
      !
      ! output arguments
      !double precision cdort(nelee,llee)
      !
      ! local variables
      integer nbotm, ncl, nelm
      double precision chumh, clith, cmanh, dum, erf, klittp, kmantp
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
            !          * initialise local variables
            chumh = (chum(nelm, ncl) + chum1(nelm, ncl))/2.0d0
            clith = (clit(nelm, ncl) + clit1(nelm, ncl))/2.0d0
            cmanh = (cman(nelm, ncl) + cman1(nelm, ncl))/2.0d0
            !
            !         * if immobilisation is not equal to the potential
            !         * immobilisation then the decomposition of the litter pool
            !         * and the manure pool are temporarily stopped
            if (isimtf(nelm, ncl)) then
               klittp = 0.0d0
               kmantp = 0.0d0
            else
               klittp = klit(nelm, ncl)
               kmantp = kman(nelm, ncl)
            end if
            !
            erf = emt(nelm, ncl)*emph(nelm, ncl)
            dum = (1 - fe)*(1 - fh)*klittp*erf*clith
            dum = dum + (1 - fe)*khum(nelm, ncl)*erf*chumh
            dum = dum + (1 - fe)*kmantp*erf*cmanh
            !
            cdort(nelm, ncl) = dum
            !
         end do
      end do
      !
      !
   end subroutine mnco2

!> @brief Updates litter and humus carbon pools.
!>
!> The routine solves the coupled litter-humus carbon balance with a fixed-point
!> iteration using mid-timestep pool estimates. Non-convergence within the
!> iteration limit is reported as warning 3016.
!>
!> The manual supplies the organic-matter efficiency fraction `FE` and
!> humification fraction `FH` in `MN12`, and the humus, litter, and manure
!> decomposition parameters through `MN15`-`MN20`. For each active cell the
!> routine uses \(E = E_T E_\psi\). The active vertical range is
!> `NBOTCE:NCETOP` when `ISBOTC` is true, otherwise
!> `NCOLMB(element):NCETOP`. `CALIT` and `CAHUM` are the cell-based external
!> carbon additions prepared by [[mnint2]].
!>
!> With \(K_l'\) and \(K_m'\) equal to `KLIT` and `KMAN` normally, but set to
!> zero while an immobilisation deficit is being repaid, the fixed-point
!> iteration solves
!>
!> \[
!> C_l^{n+1} = C_l^n + \Delta t\{K_l'E\bar{C}_l(FE-1)
!>             + FE\,E\,KHUM\,\bar{C}_h
!>             + FE\,E\,K_m'\bar{C}_m + C_l^{add}\},
!> \]
!>
!> \[
!> C_h^{n+1} = C_h^n + \Delta t\{(1-FE)FH\,K_l'E\bar{C}_l
!>             - KHUM\,E\,\bar{C}_h + C_h^{add}\}.
!> \]
!>
!> The midpoint values are updated as
!> \(\bar{C}_l=(C_l^n+C_l^{n+1})/2\) and
!> \(\bar{C}_h=(C_h^n+C_h^{n+1})/2\); manure uses
!> \(\bar{C}_m=(C_m^n+C_m^{n+1})/2\). Iteration stops when the squared relative
!> changes in both `CLIT1` and `CHUM1` are below `1D-12`. If convergence is not
!> reached after 20 iterations the routine reports warning `3016` and leaves the
!> last iterate in place.
   SUBROUTINE mnlthm(llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, fe, fh, dtuz, isbotc)

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
      DOUBLE PRECISION, INTENT(IN) :: fe  !! Efficiency fraction for organic carbon turnover.
      DOUBLE PRECISION, INTENT(IN) :: fh  !! Humification fraction.
      DOUBLE PRECISION, INTENT(IN) :: dtuz  !! Unsaturated-zone timestep in seconds.
      LOGICAL, INTENT(IN) :: isbotc  !! True when the fixed lower active cell `NBOTCE` is used.

      ! locals
      INTEGER :: nbotm, ncl, nelm, niters, ntime, warn
      DOUBLE PRECISION :: chum1o, chumh, clit1o, clith, cmanh, dum, errtol, erf
      DOUBLE PRECISION :: klittp, kmantp, werr1, wer1sq, werr2, wer2sq
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
            clith = clit(nelm, ncl)
            chumh = chum(nelm, ncl)
            chum1o = 0.0d0
            clit1o = 0.0d0
            cmanh = (cman(nelm, ncl) + cman1(nelm, ncl))/2.0d0

            ! * if immobilisation is not equal to the potential
            ! * immobilisation then the decomposition of the litter and
            ! * and manure pools are temporarily stopped
            IF (isimtf(nelm, ncl)) THEN
               kmantp = 0.0d0
               klittp = 0.0d0
            ELSE
               kmantp = kman(nelm, ncl)
               klittp = klit(nelm, ncl)
            END IF

            erf = emt(nelm, ncl)*emph(nelm, ncl)

            ! * iteration loop to calculate the new carbon litter
            ! * and humus concentrations
            iteration_loop: DO ntime = 1, niters

               dum = klittp*erf*clith*(fe - 1.0d0) + fe*erf*khum(nelm, ncl)*chumh
               dum = dum + fe*erf*kmantp*cmanh + calit(nelm, ncl)
               clit1(nelm, ncl) = clit(nelm, ncl) + dtuz*dum

               ! * litter conc at timestep n +1/2 is calculated for use
               ! * in the new calculation of the humus
               clith = (clit1(nelm, ncl) + clit(nelm, ncl))/2.0d0

               dum = (1.0d0 - fe)*fh*klittp*erf*clith - khum(nelm, ncl)*erf*chumh + cahum(nelm, ncl)
               chum1(nelm, ncl) = chum(nelm, ncl) + dtuz*dum

               ! * humus conc. at timestep n+1/2 is calculated. this is
               ! * for use in the new calculation of the litter at the
               ! * next iteration
               chumh = (chum1(nelm, ncl) + chum(nelm, ncl))/2.0d0

               ! * relative error between iterations in both litter and
               ! * humus pools in order to check the iteration is converging.
               IF (clit1(nelm, ncl) /= 0.0d0) THEN
                  werr1 = (clit1(nelm, ncl) - clit1o)/clit1(nelm, ncl)
               ELSE IF (clit1o == 0.0d0) THEN
                  werr1 = 0.0d0
               ELSE
                  werr1 = 1.0d0
               END IF

               IF (chum1(nelm, ncl) /= 0.0d0) THEN
                  werr2 = (chum1(nelm, ncl) - chum1o)/chum1(nelm, ncl)
               ELSE IF (chum1o == 0.0d0) THEN
                  werr2 = 0.0d0
               ELSE
                  werr2 = 1.0d0
               END IF

               ! * square of the errors, in order to make them positive
               wer1sq = werr1*werr1
               wer2sq = werr2*werr2

               clit1o = clit1(nelm, ncl)
               chum1o = chum1(nelm, ncl)

               ! * break out of loop if the error in both iterations
               ! * is less than the error tolerance
               IF ((wer1sq < errtol) .AND. (wer2sq < errtol)) EXIT iteration_loop

            END DO iteration_loop

            ! * the do loop has continued to niters and has thus
            ! * failed to converge
            IF (ntime > niters) THEN
               WRITE (msg, 9000) wer1sq, wer2sq
               CALL RAISE_ERROR(warn, 3016, mnpr, 0, 0, msg)
            END IF

         END DO layer_loop
      END DO

9000  FORMAT('iteration loop in mnlthm failed to converge with error = ', g15.7, g15.7)

   END SUBROUTINE mnlthm

!> @brief Updates manure carbon and nitrogen pools.
!>
!> Manure pools are integrated with a mid-timestep iteration, using the
!> temperature and matric-potential reduction factors and the scheduled manure
!> addition rate. Non-convergence is reported as warning 3015.
!>
!> The manual supplies manure decomposition categories and depth tables in
!> `MN19`/`MN20`. Time-varying external carbon input supplies the manure carbon
!> fraction (`MNFC41`) and manure C:N ratio (`MNFC42`), which [[mnint2]]
!> converts to `CAMAN` and `CNRAMN`. For each active cell the routine uses
!> \(E = E_T E_\psi\). The active vertical range is `NBOTCE:NCETOP` when
!> `ISBOTC` is true, otherwise `NCOLMB(element):NCETOP`.
!>
!> With \(K_m'\) equal to `KMAN` normally, but set to zero while an
!> immobilisation deficit is being repaid, the fixed-point iteration solves
!>
!> \[
!> C_m^{n+1} = C_m^n + \Delta t(-K_m'E\bar{C}_m + C_m^{add}),
!> \]
!>
!> \[
!> N_m^{n+1} = N_m^n + \Delta t(-K_m'E\bar{N}_m + C_m^{add}/CNRAMN).
!> \]
!>
!> The midpoint values are updated as
!> \(\bar{C}_m=(C_m^n+C_m^{n+1})/2\) and
!> \(\bar{N}_m=(N_m^n+N_m^{n+1})/2\). Iteration stops when the squared relative
!> changes in both `CMAN1` and `NMAN1` are below `1D-12`. If convergence is not
!> reached after 20 iterations the routine reports warning `3015` and leaves the
!> last iterate in place.
   SUBROUTINE mnman(llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, dtuz, cnramn, isbotc)

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
      DOUBLE PRECISION, INTENT(IN) :: dtuz  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: cnramn(nelee)  !! Element manure C:N ratio for active additions.
      LOGICAL, INTENT(IN) :: isbotc  !! True when the fixed lower active cell `NBOTCE` is used.

      ! locals
      INTEGER :: nbotm, ncl, nelm, niters, ntime, warn
      DOUBLE PRECISION :: cman1o, cmanh, dum, errtol, erf
      DOUBLE PRECISION :: kmantp, nman1o, nmanh
      DOUBLE PRECISION :: wer1sq, werr1, wer2sq, werr2
      CHARACTER(LEN=132) :: msg

      ! * parameters for the iteration loop within the subroutine
      ! * niters is the maximum number of acceptable iterations
      ! * and errtol is the squared error below which the iteration
      ! * will stop before niters is reached
      PARAMETER(niters=20, warn=3)
      PARAMETER(errtol=1.0d-12)

      !-------------------------------------------------------------------*

      ! * main loop which goes through every cell in the soil column
      DO nelm = nlf + 1, nel
         IF (isbotc) THEN
            nbotm = nbotce
         ELSE
            nbotm = ncolmb(nelm)
         END IF

         layer_loop: DO ncl = nbotm, ncetop

            ! * initialise local variables
            cmanh = cman(nelm, ncl)
            nmanh = nman(nelm, ncl)
            cman1o = 0.0d0
            nman1o = 0.0d0

            ! * if immobilisation is not equal to the potential
            ! * immobilisation then the decomposition of the manure pool
            ! * is temporarily stopped
            IF (isimtf(nelm, ncl)) THEN
               kmantp = 0.0d0
            ELSE
               kmantp = kman(nelm, ncl)
            END IF

            erf = emt(nelm, ncl)*emph(nelm, ncl)

            ! * iteration loop to calculate the new manure concentrations
            iteration_loop: DO ntime = 1, niters

               dum = -kmantp*erf*cmanh + caman(nelm, ncl)
               cman1(nelm, ncl) = cman(nelm, ncl) + dtuz*dum

               dum = -kmantp*erf*nmanh + caman(nelm, ncl)/cnramn(nelm)
               nman1(nelm, ncl) = nman(nelm, ncl) + dtuz*dum

               ! * calculates the relative error in the iteration
               IF (cman1(nelm, ncl) /= 0.0d0) THEN
                  werr1 = (cman1(nelm, ncl) - cman1o)/cman1(nelm, ncl)
               ELSE IF (cman1o == 0.0d0) THEN
                  werr1 = 0.0d0
               ELSE
                  werr1 = 1.0d0
               END IF

               IF (nman1(nelm, ncl) /= 0.0d0) THEN
                  werr2 = (nman1(nelm, ncl) - nman1o)/nman1(nelm, ncl)
               ELSE IF (nman1o == 0.0d0) THEN
                  werr2 = 0.0d0
               ELSE
                  werr2 = 1.0d0
               END IF

               ! * calculates the squared error, so that they are positive
               wer1sq = werr1*werr1
               wer2sq = werr2*werr2

               ! * updates the conc. at timestep n + 1/2 and the old conc.
               cmanh = (cman1(nelm, ncl) + cman(nelm, ncl))/2.0d0
               cman1o = cman1(nelm, ncl)
               nmanh = (nman1(nelm, ncl) + nman(nelm, ncl))/2.0d0
               nman1o = nman1(nelm, ncl)

               ! * break out of loop if error in both iterations is
               ! * less than the error tolerance
               IF ((wer1sq < errtol) .AND. (wer2sq < errtol)) EXIT iteration_loop

            END DO iteration_loop

            ! * the do loop has continued to niters and has thus
            ! * failed to converge
            IF (ntime > niters) THEN
               WRITE (msg, 9000) wer1sq, wer2sq
               CALL RAISE_ERROR(warn, 3015, mnpr, 0, 0, msg)
            END IF

         END DO layer_loop
      END DO

9000  FORMAT('iteration loop in mnman failed to converge with error = ', g15.7, g15.7)

   END SUBROUTINE mnman

END MODULE mn_organic_matter

