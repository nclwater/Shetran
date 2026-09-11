!> summary: The soil hydraulic-property lookup tables and their generation.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> Soil and lithology hydraulic properties are not evaluated from their
!> formulae during the solve: [[VSSOIL]] tabulates water content, relative
!> conductivity, storage coefficient and their derivatives against pressure
!> head once, per soil type, and [[vs_column_solver:VSFUNC]] interpolates the
!> tables afterwards.
!>
!> The generated table size is `NVSSOL = min(100,NSOLEE)` when `BFAST` is set,
!> or `min(500,NSOLEE)` otherwise; `NSOLEE = 200` bounds the storage. Which
!> formulation is tabulated is selected by `IVSFLG`; see [[vs_config]].
!>
!> This module carries the variables of the legacy `VSSOIL.INC` include group.
!> Module state is public by default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995--1998 | GP / RAH | 4.0--4.2 | Created the VSS component and its `.INC` include groups. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS Fortran sources into a single Fortran 90 module. |
!> | 2026-03 to 2026-05 | SB / SvB | 4.6 | Modernisation pass, and moved `VSREAD`'s read buffers to allocatable module state to avoid a stack-related crash. |
!> | 2026-09-10 | SvB | - | Split out of VSmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_soil_tables

   USE MOD_PARAMETERS, ONLY: one, two, three, zero
   USE array_limits, ONLY: NSEE
   USE error_reporting, ONLY: ERR_STOP
   USE file_units, ONLY: FID_logfile
   USE vs_config, ONLY: BFAST, BSOILP, IVSFLG, TBKR, TBKRC, TBPSI, TBTHE, TBTHEC, VSALPH, &
                        VSTRES, VSVGN
   USE vs_state, ONLY: NS, VSPOR

   IMPLICIT NONE

   PUBLIC :: VSSOIL


! Legacy VSSOIL.INC soil-parameter tables retained as module state.
   INTEGER :: NSOLEE !! Maximum number of generated soil lookup-table rows.
   PARAMETER(NSOLEE=200)
   DOUBLEPRECISION :: VSPPSI(NSOLEE)        !! Soil lookup pressure-head ordinates.
   DOUBLEPRECISION :: VSPTHE(NSOLEE, NSEE)   !! Soil lookup volumetric water content.
   DOUBLEPRECISION :: VSPKR(NSOLEE, NSEE)    !! Soil lookup relative hydraulic conductivity.
   DOUBLEPRECISION :: VSPETA(NSOLEE, NSEE)   !! Soil lookup storage coefficient.
   DOUBLEPRECISION :: VSPDTH(NSOLEE, NSEE)   !! Soil lookup derivative `d(theta)/d(psi)`.
   DOUBLEPRECISION :: VSPDKR(NSOLEE, NSEE)   !! Soil lookup derivative `d(K_r)/d(psi)`.
   DOUBLEPRECISION :: VSPDET(NSOLEE, NSEE)   !! Soil lookup derivative `d(eta)/d(psi)`.
   DOUBLEPRECISION :: VSPSS(NSEE)           !! Specific storage by soil type.
   DOUBLEPRECISION :: VSPPOR(NSEE)          !! Porosity copied from the wider soil parameter state.
   INTEGER :: NVSSOL                        !! Number of active soil lookup-table rows.

CONTAINS

! 26/1/96

!> Builds soil/lithology hydraulic-property lookup tables.
!>
!> `VSSOIL` is called once by [[vsin]] to generate the pressure-head lookup
!> tables (`VSPPSI`, `VSPTHE`, `VSPKR`, `VSPETA`, and their derivatives
!> `VSPDTH`/`VSPDKR`/`VSPDET`) interpolated at runtime by [[vsfunc]]. The table
!> size is `NVSSOL = min(100,NSOLEE)` when `BFAST` is set, or
!> `min(500,NSOLEE)` otherwise.
!>
!> Rows `5:NVSSOL-1` cover a log-spaced pressure-head range from
!> \(-10^{-2}\) to \(-10^4\), with each soil/lithology type (`1:NS`) evaluated
!> according to its `IVSFLG` option:
!>
!> | `IVSFLG` | Model | Formula |
!> |:---------|:------|:--------|
!> | 1 | van Genuchten | \(\theta=\theta_r+(\theta_s-\theta_r)(1+(\alpha\lvert\psi\rvert)^n)^{-m}\), \(m=1-1/n\); `VSPDET` is set to zero rather than the commented-out analytic derivative. |
!> | 2 | user table | Natural cubic-spline interpolation of `TBTHE`/`TBKR` in `log10(-psi)` from [[vsread]], scaled by `VSPOR`. |
!> | 3 | exponential | \(\theta=\theta_r+(\theta_s-\theta_r)e^{\alpha\psi}\), \(K_r=e^{\alpha\psi}\). |
!> | 4 | tabulated theta / Averjanov Kr (for SHETRAN V3.4 compatibility) | Not implemented; the routine stops with `UNFINISHED code for soil properties type 4`. |
!>
!> Row `NVSSOL` is set to fixed dry-end values (`VSPTHE=VSTRES`, conductivity
!> and derivatives zero, `VSPPSI=-1e6`). For `IVSFLG` 2 or 4, storage
!> derivatives `VSPDTH`/`VSPDET` for interior rows are then overwritten by
!> finite differences of the interpolated `VSPTHE`/`VSPDTH` values with respect
!> to `VSPPSI`.
!>
!> Rows `1:4` extend the table to near-saturation, working down in pressure
!> head from `VSPPSI(4)=0`: `VSPKR` is fixed at 1, `VSPETA`/`VSPDTH`/`VSPDKR`
!> are carried down from rows 5/4 or set to `VSPSS`/zero, and each `VSPTHE`
!> row is built recursively from the row above using the corresponding
!> `VSPETA`/`VSPSS`:
!>
!> \[
!>   VSPTHE(4)=VSPOR,\quad
!>   VSPTHE(k) = VSPTHE(k{+}1) + VSPETA(k{+}1)\bigl(VSPPSI(k)-VSPPSI(k{+}1)\bigr)
!>   \ \text{for } k=3,2,
!> \]
!> \[
!>   VSPTHE(1) = VSPTHE(2) + VSPSS\bigl(VSPPSI(1)-VSPPSI(2)\bigr).
!> \]
!>
!> Finally, for rows `5:NVSSOL` the routine rescales `VSPKR` using a
!> DSATG-style saturation ratio,
!> \[
!>   K_r(i) = \left({\theta(i)-\theta_r\over\theta_s-\theta_r}\right)^2,
!> \]
!> so that `Kr` approaches unity at saturation even for van Genuchten `n < 2`,
!> where the original curve drops rapidly and unphysically below one just below
!> saturation. This overwrite runs after the derivative tables are already
!> finalised. If `BSOILP` is set, the completed tables are printed to `FID_logfile`.
!>
!> @note
!> The DSATG saturation-ratio rescale (see above) replaces `VSPKR` without
!> recomputing `VSPDKR`, so the relative-conductivity derivative used later by
!> [[vsfunc]] does not correspond to the final `VSPKR` curve for
!> `IVSFLG = 1` or `3`. This is a pre-existing characteristic of the table
!> construction, not something introduced by the 2026 modernisation.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written. Called only from [[vsin]]. |
!> | 2026-04-06/07 | SvB | 4.6 | Replaced the manual `EDUM**x` exponentiation (`EDUM` a hardcoded `e` constant) with the `EXP` intrinsic for the `IVSFLG=3` branch; equivalent result. |
!> | 2026-04-10 | SvB | 4.6 | Fixed the near-saturation `VSPTHE` initialisation: row 3 had collapsed to `VSPTHE(3,IS) = VSPOR(IS)` (the same value as row 4, with no correction term), which is now corrected to the recursive `VSPTHE(4,IS) + VSPETA(4,IS)*(VSPPSI(3)-VSPPSI(4))` form shown above. |
!> @endhistory
   SUBROUTINE VSSOIL()

      ! Assumed external module dependencies providing global variables:
      ! NSEE, NSOLEE, BFAST, NVSSOL, VSPPSI, NS, IVSFLG, VSPOR, VSTRES,
      ! VSALPH, VSVGN, VSPTHE, VSPDTH, VSPKR, VSPDKR, VSPETA, VSPDET, VSPSS,
      ! TBPSI, TBTHE, TBTHEC, TBKR, TBKRC, BSOILP, FID_logfile, zero, one, two, three

      IMPLICIT NONE

      ! Locals
      INTEGER :: I, IS, NDUM
      INTEGER :: NTBPOS(NSEE) = 1
      DOUBLE PRECISION :: RVSSOL, PSI, DDDUM
      DOUBLE PRECISION :: DDTSAT, DDTRES, DDA, DDN, DDM, DD1M1, DDTSMR
      DOUBLE PRECISION :: DDAP, DDAPN, DDAPN1, DDAPM, DDAPM1, DDAPM2, DDTCAP
      DOUBLE PRECISION :: DDTC, DDTCM, DDTCM1, DDTCM2, DDDTCP
      DOUBLE PRECISION :: PLOG, PLOGLO, PLOGHI, ADUM, BDUM, HDUM, RKRDUM

      ! set up size of internal look-up tables
      IF (BFAST) THEN
         NVSSOL = MIN(100, NSOLEE)
      ELSE
         NVSSOL = MIN(500, NSOLEE)
      END IF

      RVSSOL = DBLE(NVSSOL)

      ! loop over NVSSOL divisions of the soil property tables
      ! (NB. low values of I correspond to wet soils)
      ! psi ranges from -(10**-2) to -(10**4)
      psi_loop: DO I = 5, NVSSOL - 1

         PSI = -(10.0D0**(-two + 6.0D0*DBLE(I - 5)/RVSSOL))
         VSPPSI(I) = PSI

         ! set up property data for each soil type
         soil_loop: DO IS = 1, NS

            ! ... 1 (Van Genuchten)
            IF (IVSFLG(IS) == 1) THEN
               DDTSAT = VSPOR(IS)
               DDTRES = VSTRES(IS)
               DDA = VSALPH(IS)*100.0D0
               DDN = VSVGN(IS)
               DDM = one - (one/DDN)
               DD1M1 = (one/DDM) - one
               DDTSMR = DDTSAT - DDTRES
               DDAP = -DDA*PSI
               DDAPN = DDAP**DDN
               DDAPN1 = DDAP**(DDN - one)
               DDAPM = (one + DDAPN)**DDM
               DDAPM1 = (one + DDAPN)**(DDM + one)
               DDAPM2 = (one + DDAPN)**(DDM + two)
               DDDTCP = DDA*DDM*DDN*DDAPN1/DDAPM1

               VSPTHE(I, IS) = DDTRES + DDTSMR/DDAPM
               VSPDTH(I, IS) = DDTSMR*DDDTCP

               DDTCAP = MAX(1.0D-10, (VSPTHE(I, IS) - DDTRES)/DDTSMR)
               DDTC = one - (DDTCAP**(one/DDM))
               DDTCM = DDTC**DDM
               DDTCM1 = DDTC**(DDM - one)
               DDTCM2 = (one - DDTCM)**two

               VSPKR(I, IS) = SQRT(DDTCAP)*DDTCM2

               ! Commented out legacy derivative code maintained for reference
               ! VSPDKR(I,IS) = DSQRT(DDTCAP)*(one-DDTCM)*
               !  (half*(one-DDTCM)/DDTCAP + two*DDTCM1*DDTCAP**DD1M1) * DDDTCP

               DDDUM = (DDA*DDA*DDM*DDN*DDTSMR*DDAPN1/DDAPM2)* &
                       ((DDN - one)*(one + DDAPN) + (DDM + one)*DDN*DDAPN1)
               VSPETA(I, IS) = VSPTHE(I, IS)*VSPSS(IS)/VSPOR(IS) + VSPDTH(I, IS)

               ! VSPDET(I,IS) = VSPDTH(I,IS)*VSPSS(IS)/VSPOR(IS) + DDDUM
               VSPDET(I, IS) = zero

               ! ... 2 (tabulated theta and Kr)
            ELSE IF (IVSFLG(IS) == 2) THEN

               ! check for correct location in input table
               ! Safely bounds check using DO WHILE instead of simple IF
               DO WHILE (PSI < TBPSI(NTBPOS(IS) + 1, IS))
                  NTBPOS(IS) = NTBPOS(IS) + 1
               END DO

               NDUM = NTBPOS(IS)

               ! evaluate cubic spline polynomial for theta and Kr
               PLOG = LOG10(-PSI)
               PLOGHI = LOG10(-TBPSI(NDUM + 1, IS))
               PLOGLO = LOG10(-TBPSI(NDUM, IS))
               HDUM = PLOGHI - PLOGLO
               ADUM = (PLOGHI - PLOG)/HDUM
               BDUM = (PLOG - PLOGLO)/HDUM

               VSPTHE(I, IS) = ADUM*TBTHE(NDUM, IS) + BDUM*TBTHE(NDUM + 1, IS) + &
                               ((ADUM**three - ADUM)*TBTHEC(NDUM, IS) + &
                                (BDUM**three - BDUM)*TBTHEC(NDUM + 1, IS))* &
                               (HDUM**two)/6.0D0

               VSPTHE(I, IS) = VSPOR(IS)*VSPTHE(I, IS)

               VSPKR(I, IS) = ADUM*TBKR(NDUM, IS) + BDUM*TBKR(NDUM + 1, IS) + &
                              ((ADUM**three - ADUM)*TBKRC(NDUM, IS) + &
                               (BDUM**three - BDUM)*TBKRC(NDUM + 1, IS))* &
                              (HDUM**two)/6.0D0

               ! ... 3 (exponential)
            ELSE IF (IVSFLG(IS) == 3) THEN

               ! Replaced EDUM**(VSALPH * PSI) hack with precise EXP intrinsic
               DDDUM = EXP(VSALPH(IS)*PSI)
               VSPTHE(I, IS) = VSTRES(IS) + (VSPOR(IS) - VSTRES(IS))*DDDUM
               VSPDTH(I, IS) = (VSPOR(IS) - VSTRES(IS))*VSALPH(IS)*DDDUM

               VSPKR(I, IS) = DDDUM
               VSPDKR(I, IS) = VSALPH(IS)*DDDUM

               VSPETA(I, IS) = VSPTHE(I, IS)*VSPSS(IS)/VSPOR(IS) + VSPDTH(I, IS)
               VSPDET(I, IS) = VSPDTH(I, IS)*VSPSS(IS)/VSPOR(IS) + VSPDTH(I, IS)*VSALPH(IS)

               ! ... 4 (tabulated theta and Averjanov Kr)
            ELSE IF (IVSFLG(IS) == 4) THEN
               WRITE (*, '(A)') 'ERROR: Unfinished code for soil properties type 4.'
               CALL ERR_STOP(255)
            END IF

         END DO soil_loop
      END DO psi_loop

      ! set up property data for extreme dry conditions
      VSPPSI(NVSSOL) = -1.0D6
      DO IS = 1, NS
         VSPTHE(NVSSOL, IS) = VSTRES(IS)
         VSPKR(NVSSOL, IS) = zero
         VSPETA(NVSSOL, IS) = zero
         VSPDTH(NVSSOL, IS) = zero
         VSPDKR(NVSSOL, IS) = zero
         VSPDET(NVSSOL, IS) = zero
      END DO

      ! set up storage term for tabulated data
      DO I = 5, NVSSOL - 1
         DO IS = 1, NS
            IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
               VSPDTH(I, IS) = (VSPTHE(I + 1, IS) - VSPTHE(I, IS))/(VSPPSI(I + 1) - VSPPSI(I))
               VSPETA(I, IS) = VSPTHE(I, IS)*VSPSS(IS)/VSPOR(IS) + VSPDTH(I, IS)
            END IF
         END DO
      END DO

      DO I = 5, NVSSOL - 1
         DO IS = 1, NS
            IF (IVSFLG(IS) == 2 .OR. IVSFLG(IS) == 4) THEN
               VSPDET(I, IS) = VSPDTH(I, IS)*VSPSS(IS)/VSPOR(IS) + &
                               (VSPDTH(I + 1, IS) - VSPDTH(I, IS))/(VSPPSI(I + 1) - VSPPSI(I))
            END IF
         END DO
      END DO

      ! set up property data for extreme wet conditions
      VSPPSI(4) = zero
      VSPPSI(3) = 2.5D-1
      VSPPSI(2) = 5.0D-1
      VSPPSI(1) = 1.0D6

      wet_conditions_loop: DO IS = 1, NS

         ! Converted line-by-line assignments into high-performance array slices
         VSPKR(1:4, IS) = one
         VSPETA(3:4, IS) = VSPETA(5, IS)
         VSPETA(1:2, IS) = VSPSS(IS)
         VSPDTH(4, IS) = VSPDTH(5, IS)

         VSPTHE(4, IS) = VSPOR(IS)
         VSPTHE(3, IS) = VSPTHE(4, IS) + VSPETA(4, IS)*(VSPPSI(3) - VSPPSI(4))
         VSPTHE(2, IS) = VSPTHE(3, IS) + VSPETA(3, IS)*(VSPPSI(2) - VSPPSI(3))
         VSPTHE(1, IS) = VSPTHE(2, IS) + VSPSS(IS)*(VSPPSI(1) - VSPPSI(2))

         VSPDTH(1:3, IS) = zero
         VSPDKR(4, IS) = VSPDKR(5, IS)
         VSPDKR(1:3, IS) = zero
         VSPDET(1:4, IS) = zero

      END DO wet_conditions_loop

      ! DSATG-specific code - adjust relative conductivity curves so that
      ! Kr approaches unity at saturation (for values of VG-n less than 2,
      ! the value of Kr drops rapidly and unphysically less than one near satu...)
      dsatg_loop: DO IS = 1, NS
         RKRDUM = VSPOR(IS) - VSTRES(IS)
         ! Replace inner loop with high-performance array operation
         VSPKR(5:NVSSOL, IS) = ((VSPTHE(5:NVSSOL, IS) - VSTRES(IS))/RKRDUM)**two
      END DO dsatg_loop

      ! write soil property tables to PRI file
      IF (BSOILP) THEN
         WRITE (FID_logfile, 905) NS, NVSSOL
         DO IS = 1, NS
            WRITE (FID_logfile, 910) IS
            DO I = 1, NVSSOL
               WRITE (FID_logfile, 920) I, VSPPSI(I), VSPTHE(I, IS), VSPETA(I, IS), VSPKR(I, IS), &
                  VSPDTH(I, IS), VSPDET(I, IS), VSPDKR(I, IS)
            END DO
         END DO
      END IF

      RETURN

      ! FORMAT STATEMENTS
905   FORMAT(/'VSS physical soil/lithology property data'/ &
              '========================================='/ &
              I3, ' soils'/ &
              I3, ' values in soil property tables')

910   FORMAT(/ &
         3X, '  Soil property tables for soil/lithology type: ', I3/ &
         3X, '  -------------------------------------------------'// &
         3X, '      psi         theta          eta            Kr      ', &
         ' d(the)/d(psi) d(eta)/d(psi)  d(Kr)/d(psi)'/ &
         3X, '   (VSPPSI)      (VSPTHE)      (VSPETA)       (VSPKR)   ', &
         '   (VSPDTH)      (VSPDET)       (VSPDKR)  '/ &
         3X, '  ------------  ------------  ------------  ------------', &
         '  ------------  ------------  ------------')

920   FORMAT(I3, 7(2X, G14.6))

   END SUBROUTINE VSSOIL

END MODULE vs_soil_tables

