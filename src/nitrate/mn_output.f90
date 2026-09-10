!> summary: The nitrate and carbon budget output files.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> [[MNOUT]] writes the cumulative nitrogen and carbon budgets to `MNPR`,
!> `MNOUT1` and `MNOUT2`. It is called at the end of every [[mn_driver:MNMAIN]]
!> and accumulates its totals from its own first call.
!>
!> @warning
!> The nitrogen loss and addition labels do not match all the terms included in
!> their totals. This is documented rather than corrected.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> | 2026-09-10 | SvB | - | Split out of MNmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mn_output

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE array_limits, ONLY: LLEE, nelee, NLYREE
   USE error_status, ONLY: errstat_alloc, errstat_write
   USE mn_state, ONLY: cahum, calit, caman, cdort, chum, chum1, clit, clit1, cman, cman1, &
                       denit, gamtmp, imamm, imnit, miner, naamm, namm, namm1, nanit, nlit, &
                       nlit1, nman, nman1, ntrf, plamm, plnit, snit, vol

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: mnout

CONTAINS

!> @brief Accumulates and writes mineral nitrogen and carbon budget outputs.
!>
!> `mnout` keeps saved cumulative arrays and writes area-normalised summaries to
!> `MNOUT1` (carbon) and `MNOUT2` (nitrogen). Active cells follow the module
!> convention: `NBOTCE:NCETOP` when `ISBOTC` is true, otherwise
!> `NCOLMB(element):NCETOP`.
!>
!> | Stage | Accounting |
!> | --- | --- |
!> | First call | Allocate saved cumulative flux arrays, zero them over active soil-layer cells, compute total land area, and write initial carbon and nitrogen stores. |
!> | Every call | Accumulate cell-depth-integrated rates over the current timestep, including ammonium/nitrate additions, organic additions, CO2 production, denitrification, mineralisation, immobilisation, nitrification, plant uptake, source/sink totals, and volatilisation. |
!> | Periodic output | When `UZNOW >= MNSTRT + 24*NPRNT`, recompute current nitrogen and carbon stores from updated pools, increment `NPRNT`, and write current total/addition/loss summaries normalised by total land area. Ammonium storage uses the nonlinear retardation factor \(1 + KDDSOL(NAMM1/MNCREF)^{GNN-1}/VSTHE\). |
!>
!> The routine does not reset cumulative flux arrays after each write; reported
!> additions and losses are cumulative since the initial `MNOUT` call.
!>
!> @warning The printed nitrogen labels describe the current calculations only
!> imperfectly. `TOTADN` contains organic-N additions, ammonium additions, and
!> nitrate immobilisation (`IMNITT`), but omits the accumulated nitrate addition
!> `ADNITT`. `TOTLOS` contains volatilisation, ammonium plant uptake, and
!> nitrification, but omits nitrate plant uptake and denitrification. The stored
!> `TOTN` likewise includes ammonium and organic pools but not dissolved nitrate.
!> These retained accounting expressions are documented, not corrected here.
!> @endwarning
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> | 2026-09-07 | SvB | - | Status-checked the carbon/nitrogen budget `WRITE`s through [[error_status:errstat_write]]. |
   SUBROUTINE MNOUT(MNOUT1, MNOUT2, NBOTCE, NCETOP, NEL, NLF, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, CNRHUM, GNN, MNCREF, DELTAZ, &
      KDDSOL, PPHI, DTUZ, UZNOW, DXQQ, DYQQ, CNRALT, CNRAMN, VSTHE, VSTHEO, ISBOTC)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: MNOUT1  !! Carbon budget output unit.
      INTEGER, INTENT(IN) :: MNOUT2  !! Nitrogen budget output unit.
      INTEGER, INTENT(IN) :: NBOTCE  !! Lowest cell included when bottom-cell truncation is active.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column output.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(IN) :: NLYR(NELEE)  !! Number of soil layers in each element.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE)  !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: CNRHUM  !! Humus carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: GNN  !! Nonlinear ammonium adsorption exponent.
      DOUBLE PRECISION, INTENT(IN) :: MNCREF  !! Reference nitrogen concentration.
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: KDDSOL(NS)  !! Soil ammonium adsorption coefficient.
      DOUBLE PRECISION, INTENT(IN) :: PPHI(NELEE, LLEE)  !! Mobile-water partition factor.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: DXQQ(NELEE)  !! Element width.
      DOUBLE PRECISION, INTENT(IN) :: DYQQ(NELEE)  !! Element length.
      DOUBLE PRECISION, INTENT(IN) :: CNRALT(NELEE)  !! Element litter C:N ratio for active additions.
      DOUBLE PRECISION, INTENT(IN) :: CNRAMN(NELEE)  !! Element manure C:N ratio for active additions.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: VSTHEO(NEL, NCETOP + 1)  !! Previous volumetric water content.
      LOGICAL, INTENT(IN) :: ISBOTC  !! True when the fixed lower active cell `NBOTCE` is used.

      ! Locals etc.
      INTEGER, PARAMETER :: HRPRNT = 24
      INTEGER :: JLYR, JSOIL, NBOTM, NCEBOT, NCL, NELM
      CHARACTER(LEN=60) :: MSG
      DOUBLE PRECISION :: RETAMM
      DOUBLE PRECISION :: TOTADC, TOTADN, TOTC, TOTCO2, TOTLOS, TOTN

      ! Saved Static State
      INTEGER, SAVE :: NPRNT = 0, PASS = 0
      DOUBLE PRECISION, SAVE :: MNSTRT = 0.0D0, TAREA = 0.0D0

      ! Allocatable workspace
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE, SAVE :: ADAMMT, ADDCT, ADNITT, ADORNT, CDOTOT, DETOT, GAMTOT, IMAMMT
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE, SAVE :: IMNITT, MINTOT, NTRTOT, PLAMMT, PLNITT, STOT, VOLTOT

      ! declarations for output for specific cells (Commented to suppress unused var warnings)
      ! INTEGER, PARAMETER :: nout = 9
      ! INTEGER :: noutl, n1, n2
      ! INTEGER :: noutel(nout) = [457, 457, 457, 457, 457, 457, 457, 457, 457]
      ! INTEGER :: noutce(nout) = [10, 20, 30, 32, 35, 38, 40, 41, 42]

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "MNmod:MNOUT"

      !-------------------------------------------------------------------*

      PASS = PASS + 1

      ! * if it is the first pass the initial concentrations are printed
      IF (PASS == 1) THEN

         ALLOCATE (ADAMMT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "ADAMMT", location, emsg)
         ALLOCATE (ADDCT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "ADDCT", location, emsg)
         ALLOCATE (ADNITT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "ADNITT", location, emsg)
         ALLOCATE (ADORNT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "ADORNT", location, emsg)
         ALLOCATE (CDOTOT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "CDOTOT", location, emsg)
         ALLOCATE (DETOT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "DETOT", location, emsg)

         ALLOCATE (GAMTOT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "GAMTOT", location, emsg)
         ALLOCATE (IMAMMT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IMAMMT", location, emsg)
         ALLOCATE (IMNITT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IMNITT", location, emsg)
         ALLOCATE (MINTOT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "MINTOT", location, emsg)
         ALLOCATE (NTRTOT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "NTRTOT", location, emsg)
         ALLOCATE (PLAMMT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "PLAMMT", location, emsg)

         ALLOCATE (PLNITT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "PLNITT", location, emsg)
         ALLOCATE (STOT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "STOT", location, emsg)
         ALLOCATE (VOLTOT(NEL, NCETOP), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "VOLTOT", location, emsg)

         TOTC = 0.0D0
         TOTN = 0.0D0
         TAREA = 0.0D0

         DO NELM = NLF + 1, NEL
            IF (ISBOTC) THEN
               NBOTM = NBOTCE
            ELSE
               NBOTM = NCOLMB(NELM)
            END IF

            TAREA = TAREA + DXQQ(NELM)*DYQQ(NELM)
            NCEBOT = NBOTM

            DO JLYR = 1, NLYR(NELM)
               JSOIL = NTSOIL(NELM, JLYR)
               DO NCL = MAX(NCEBOT, NLYRBT(NELM, JLYR)), NLYRBT(NELM, JLYR + 1) - 1
                  ADAMMT(NELM, NCL) = 0.0D0
                  ADDCT(NELM, NCL) = 0.0D0
                  ADNITT(NELM, NCL) = 0.0D0
                  ADORNT(NELM, NCL) = 0.0D0
                  CDOTOT(NELM, NCL) = 0.0D0
                  DETOT(NELM, NCL) = 0.0D0
                  GAMTOT(NELM, NCL) = 0.0D0
                  IMAMMT(NELM, NCL) = 0.0D0
                  IMNITT(NELM, NCL) = 0.0D0
                  MINTOT(NELM, NCL) = 0.0D0
                  NTRTOT(NELM, NCL) = 0.0D0
                  PLAMMT(NELM, NCL) = 0.0D0
                  PLNITT(NELM, NCL) = 0.0D0
                  STOT(NELM, NCL) = 0.0D0
                  VOLTOT(NELM, NCL) = 0.0D0

                  RETAMM = 1.0D0 + (KDDSOL(JSOIL)*(NAMM(NELM, NCL)/MNCREF)**(GNN - 1.0D0))/VSTHEO(NELM, NCL)

                  TOTN = TOTN + DELTAZ(NCL, NELM)*DXQQ(NELM)*DYQQ(NELM)*(NAMM(NELM, NCL)*VSTHEO(NELM, NCL)*RETAMM + &
                     NLIT(NELM, NCL) + NMAN(NELM, NCL) + CHUM(NELM, NCL)/CNRHUM)

                  TOTC = TOTC + DELTAZ(NCL, NELM)*DXQQ(NELM)*DYQQ(NELM)*(CMAN(NELM, NCL) + CLIT(NELM, NCL) + CHUM(NELM, NCL))
               END DO
            END DO
         END DO

         MNSTRT = UZNOW

         ios = 0
         IF (ios == 0) WRITE (MNOUT2, '(/A30,G16.8)', IOSTAT=ios, IOMSG=emsg) 'initial nitrogen (kg n m-2) = ', TOTN/TAREA
         IF (ios == 0) WRITE (MNOUT1, '(/A28,G16.8)', IOSTAT=ios, IOMSG=emsg) 'initial carbon (kg c m-2) = ', TOTC/TAREA
         CALL errstat_write(ios, location//' (initial carbon/nitrogen budget)', emsg)
      END IF

      ! Main simulation timestep updates
      DO NELM = NLF + 1, NEL
         IF (ISBOTC) THEN
            NBOTM = NBOTCE
         ELSE
            NBOTM = NCOLMB(NELM)
         END IF

         DO NCL = NBOTM, NCETOP
            ADAMMT(NELM, NCL) = ADAMMT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*NAAMM(NELM, NCL)
            ADDCT(NELM, NCL) = ADDCT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*(CAMAN(NELM, NCL) + CAHUM(NELM, NCL) + CALIT(NELM, NCL))
            ADNITT(NELM, NCL) = ADNITT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*NANIT(NELM, NCL)
            ADORNT(NELM, NCL) = ADORNT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * (CAMAN(NELM, NCL) / CNRAMN(NELM) + CAHUM(NELM, NCL) / CNRHUM + CALIT(NELM, NCL) / CNRALT(NELM))
            CDOTOT(NELM, NCL) = CDOTOT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*CDORT(NELM, NCL)
            DETOT(NELM, NCL) = DETOT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*DENIT(NELM, NCL)
            GAMTOT(NELM, NCL) = GAMTOT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*GAMTMP(NELM, NCL)
            IMAMMT(NELM, NCL) = IMAMMT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*IMAMM(NELM, NCL)
            IMNITT(NELM, NCL) = IMNITT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*IMNIT(NELM, NCL)
            MINTOT(NELM, NCL) = MINTOT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*MINER(NELM, NCL)
            NTRTOT(NELM, NCL) = NTRTOT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*NTRF(NELM, NCL)
            PLAMMT(NELM, NCL) = PLAMMT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*PLAMM(NELM, NCL)
            PLNITT(NELM, NCL) = PLNITT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*PLNIT(NELM, NCL)
            STOT(NELM, NCL) = STOT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*SNIT(NELM, NCL)
            VOLTOT(NELM, NCL) = VOLTOT(NELM, NCL) + DTUZ*DELTAZ(NCL, NELM)*VOL(NELM, NCL)
         END DO
      END DO

      ! Output reporting block
      IF (UZNOW >= HRPRNT*NPRNT + MNSTRT) THEN
         TOTADN = 0.0D0
         TOTADC = 0.0D0
         TOTLOS = 0.0D0
         TOTN = 0.0D0
         TOTC = 0.0D0
         TOTCO2 = 0.0D0

         ! Form the current area-integrated totals from the cumulative arrays.
         DO NELM = NLF + 1, NEL
            IF (ISBOTC) THEN
               NBOTM = NBOTCE
            ELSE
               NBOTM = NCOLMB(NELM)
            END IF
            NCEBOT = NBOTM

            DO JLYR = 1, NLYR(NELM)
               JSOIL = NTSOIL(NELM, JLYR)
               DO NCL = MAX(NCEBOT, NLYRBT(NELM, JLYR)), NLYRBT(NELM, JLYR + 1) - 1

                  RETAMM = 1.0D0 + (KDDSOL(JSOIL)*(NAMM1(NELM, NCL)/MNCREF)**(GNN - 1.0D0))/VSTHE(NCL, NELM)

                  ! * sum of concentrations over all the cells
                  TOTLOS = TOTLOS + DXQQ(NELM)*DYQQ(NELM)*(VOLTOT(NELM, NCL) + PLAMMT(NELM, NCL) + NTRTOT(NELM, NCL))
                  TOTADN = TOTADN + DXQQ(NELM)*DYQQ(NELM)*(ADORNT(NELM, NCL) + ADAMMT(NELM, NCL) + IMNITT(NELM, NCL))
                  TOTADC = TOTADC + DXQQ(NELM)*DYQQ(NELM)*ADDCT(NELM, NCL)

                  TOTN = TOTN + DELTAZ(NCL, NELM)*DXQQ(NELM)*DYQQ(NELM)*(NAMM1(NELM, NCL)*VSTHE(NCL, NELM)*RETAMM + &
                     NLIT1(NELM, NCL) + NMAN1(NELM, NCL) + CHUM1(NELM, NCL)/CNRHUM)

                  TOTC = TOTC + DELTAZ(NCL, NELM)*DXQQ(NELM)*DYQQ(NELM)*(CMAN1(NELM, NCL) + CLIT1(NELM, NCL) + CHUM1(NELM, NCL))
                  TOTCO2 = TOTCO2 + DXQQ(NELM)*DYQQ(NELM)*CDOTOT(NELM, NCL)
               END DO
            END DO
         END DO

         NPRNT = NPRNT + 1

         ios = 0
         IF (ios == 0) WRITE (MNOUT1, '(///A7,G12.5,A6)', IOSTAT=ios, IOMSG=emsg) 'time = ', UZNOW, ' hours'
         IF (ios == 0) WRITE (MNOUT2, '(///A7,G12.5,A6)', IOSTAT=ios, IOMSG=emsg) 'time = ', UZNOW, ' hours'

         IF (ios == 0) WRITE (MNOUT2, '(A28,G16.8)', IOSTAT=ios, IOMSG=emsg) 'total nitrogen (kg n m-2) = ', TOTN/TAREA
         IF (ios == 0) WRITE (MNOUT2, '(A33,G16.8)', IOSTAT=ios, IOMSG=emsg) 'total nitrogen added (kg n m-2)= ', TOTADN/TAREA
         IF (ios == 0) WRITE (MNOUT2, '(A32,G16.8)', IOSTAT=ios, IOMSG=emsg) 'total nitrogen lost (kg n m-2) = ', TOTLOS/TAREA
         IF (ios == 0) WRITE (MNOUT1, '(A26,G16.8)', IOSTAT=ios, IOMSG=emsg) 'total carbon (kg c m-2) = ', TOTC/TAREA
         IF (ios == 0) WRITE (MNOUT1, '(A32,G16.8)', IOSTAT=ios, IOMSG=emsg) 'total carbon added (kg c m-2) = ', TOTADC/TAREA
         IF (ios == 0) WRITE (MNOUT1, '(A28,G16.8)', IOSTAT=ios, IOMSG=emsg) 'total co2 lost (kg c m-2) = ', TOTCO2/TAREA
         CALL errstat_write(ios, location//' (periodic carbon/nitrogen budget)', emsg)
      END IF

   END SUBROUTINE MNOUT

END MODULE mn_output

