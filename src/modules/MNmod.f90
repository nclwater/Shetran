!> @brief Models carbon turnover and mineral-nitrogen cycling in soil columns.
!>
!> `MNmod` implements SHETRAN's optional Nitrate Component. It is enabled when
!> [[frmod:fropen]] finds the main nitrate-data file on unit `MND` (53), and is
!> called from [[cmmod:cmsim]] rather than acting as an independent transport
!> solver. The contaminant component transports dissolved nitrate; this module
!> calculates ammonium storage, organic carbon and nitrogen turnover,
!> mineralisation and immobilisation, nitrification, denitrification, ammonia
!> volatilisation, deposition, fertiliser and organic additions, plant uptake,
!> environmental response factors, and the coupled nitrate source/sink terms.
!>
!> The main nitrate-data file supplies the process constants and spatial fields
!> in records `MN11`--`MN60`. Scheduled inorganic-nitrogen and organic-carbon
!> additions come from `MNFN` and `MNFC`; plant-uptake data come from `MNPL`.
!> Diagnostics are written to `MNPR`, `MNOUT1`, `MNOUT2`, and `MNOUTPL`. See the
!> User Guide's *Nitrate component* and *Nitrate component data input* sections
!> for the record definitions and units.
!>
!> Runtime control is split between [[mninitialise]], [[mncont]], and [[mnmain]].
!> `MNINITIALISE` allocates persistent state and workspace, reads and validates
!> static data, initialises plant state and process pools, and clears `SSS1` and
!> `SSS2`. Later `MNCONT` calls update plant uptake before `MNMAIN` reads
!> scheduled additions, advances the process pools, populates the contaminant
!> source/sink arrays, and writes cumulative output. The module has retained
!> state and no reset/deallocation path, so it is not re-entrant and assumes one
!> model run with fixed dimensions per process.
!>
!> To preserve legacy timing, the first `CMSIM` call performs only
!> `MNINITIALISE`; its contaminant solve uses zero MN source/sink terms. MN
!> process updates begin on the following `CMSIM` call.
!>
!> @note The implementation overwrites `TA(1:NV)` with 10 deg C in [[mncont]],
!> hard-codes the mobile-water uptake fraction `PPHI` to 0.5 in [[mnint2]], and
!> accumulates [[mnout]] budgets from that routine's first call.
!> @endnote
!>
!> @warning Several retained current-code limitations affect interpretation:
!> [[mnplant]] stores every vegetation table in row `NV` and can inspect saved,
!> uninitialised `ISCROP` flags; [[mnred1]] leaves `Q10M` and `Q10N` undefined
!> when Q10 mode is disabled although [[mnerr2]] still checks them; and the
!> nitrogen loss/addition labels in [[mnout]] do not match all terms included in
!> their totals. These behaviours are documented here and are not corrected by
!> this documentation transfer.
!> @endwarning
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> @endhistory
module MNmod


    use sglobal, only : llee, nconee, nelee, nlfee, nlyree, npelee, npltee, nsee, nvee, nxee, nyee, error
    use mod_load_filedata,    only : alallf, alalli, alchk, alchki, alintp, alred2, alredc, alredf, alredi, alredl
    use utilsmod, only: hour_from_date, tridag


   IMPLICIT NONE

   PRIVATE
   PUBLIC    :: mnamm, mnco2, mncont, mnedth, mnemph, mnemt, mnenph, mnent   ! subroutine names
   PUBLIC    :: mnerr0, mnerr1, mnerr2, mnerr3, mnerr4, mngam, mninit, mnint2
   PUBLIC    :: mnlthm, mnltn, mnmain, mnman, mnnit, mnout, mnplant, mnred1, mnred2, mntemp
   PUBLIC    :: mninitialise, mnisinitialised

   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: cahum  !! External carbon-addition rate assigned to humus.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: calit  !! External carbon-addition rate assigned to litter.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: caman  !! External carbon-addition rate assigned to manure.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: cdort  !! Carbon-dioxide production rate from organic-matter turnover.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: chum   !! Humus carbon at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: chum1  !! Updated humus carbon.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: clit   !! Litter carbon at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: clit1  !! Updated litter carbon.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: cman   !! Manure carbon at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: cman1  !! Updated manure carbon.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: denit  !! Denitrification loss rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: dummy4 !! Transposed element/cell workspace for MN input checks.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: dummy6 !! Element/cell workspace for MN input checks.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: edeth  !! Water-content response factor for denitrification.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: emph   !! Matric-potential response factor for mineralisation.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: emt    !! Temperature response factor for mineralisation.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: enph   !! Matric-potential response factor for nitrification.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ent    !! Temperature response factor for nitrification.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: gam    !! Net mineralisation rate after deficit adjustment.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: gamtmp !! Unadjusted net mineralisation rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: imamm  !! Ammonium immobilisation rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: imdiff !! Unmet immobilisation demand carried to the next timestep.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: imnit  !! Nitrate immobilisation rate.
   LOGICAL, DIMENSION(:,:), ALLOCATABLE :: isimtf        !! Whether an immobilisation deficit suppresses litter/manure turnover.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: kd1    !! Denitrification carbon-demand coefficient.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: kd2    !! Denitrification nitrate-availability coefficient.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: khum   !! Humus decomposition-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: klit   !! Litter decomposition-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: kman   !! Manure decomposition-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: knit   !! Nitrification-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: kvol   !! Ammonia-volatilisation-rate coefficient.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: miner  !! Gross mineralisation rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: naamm  !! Ammonium addition/deposition rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: namm   !! Ammonium concentration at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: namm1  !! Updated ammonium concentration.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nanit  !! Nitrate addition/deposition rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ndnit  !! Dimensional nitrate concentration in dynamic water.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ndsnt  !! Dimensional nitrate concentration in dead-space water.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nlit   !! Litter nitrogen at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nlit1  !! Updated litter nitrogen.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nman   !! Manure nitrogen at the start of the timestep.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nman1  !! Updated manure nitrogen.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ntrf   !! Nitrification rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: plamm  !! Actual ammonium plant-uptake rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: plnit  !! Actual nitrate plant-uptake rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: plup   !! Potential plant-nitrogen-uptake rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: pphi   !! Dynamic-water fraction used to partition uptake.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: snit   !! Total nitrate source/sink diagnostic rate.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: temp   !! Soil temperature used by MN response factors.
   DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: vol    !! Ammonia-volatilisation loss rate.

   INTEGER, PARAMETER :: MN_PLANT_NVALEE = 30

   TYPE :: MN_CONFIG_TYPE
      INTEGER :: NBOTCE
      DOUBLE PRECISION :: AMMDDR, AMMWDR, CNRBIO, CNRHUM, FE, FH, GNN
      DOUBLE PRECISION :: KPLAMM, KPLNIT, KUAMM, KUNIT, MNCREF, NITDDR, NITWDR
      DOUBLE PRECISION :: Q10M, Q10N
      DOUBLE PRECISION :: KDDSOL(NSEE)
      LOGICAL :: ISBOTC, ISQ10
   END TYPE MN_CONFIG_TYPE

   TYPE :: MN_WORKSPACE_TYPE
      INTEGER, ALLOCATABLE :: IDUM(:)
      LOGICAL, ALLOCATABLE :: LDUM(:)
      DOUBLE PRECISION, ALLOCATABLE :: DUMMY(:)
      DOUBLE PRECISION, ALLOCATABLE :: CDPTHB(:), CLTFCT(:), CMNFCT(:)
      DOUBLE PRECISION, ALLOCATABLE :: CNRAL(:), CNRALT(:), CNRAM(:), CNRAMN(:)
      DOUBLE PRECISION, ALLOCATABLE :: CTOT(:), NAMFCT(:), NDPTHB(:), NTOT(:)
   END TYPE MN_WORKSPACE_TYPE

   TYPE :: MN_PLANT_STATE_TYPE
      INTEGER :: NVALUE(NPLTEE)
      INTEGER :: NPL(NELEE), NPLTYP(NELEE, NPELEE)
      DOUBLE PRECISION :: CDI(NPLTEE, MN_PLANT_NVALEE), CDIT(NPLTEE, MN_PLANT_NVALEE)
      DOUBLE PRECISION :: CLAIMX(NPLTEE)
      DOUBLE PRECISION :: CROPTM(NELEE, NPELEE), GMCPBB(NELEE, NPELEE)
      DOUBLE PRECISION :: MASSB(NELEE, NPELEE), PFONE(NELEE, NPELEE)
      LOGICAL :: ISCROP(NELEE, NPELEE)
   END TYPE MN_PLANT_STATE_TYPE

   TYPE(MN_CONFIG_TYPE) :: MN_CONFIG
   TYPE(MN_WORKSPACE_TYPE) :: MN_WORK
   TYPE(MN_PLANT_STATE_TYPE) :: MN_PLANT_STATE
   LOGICAL :: MN_INITIALISED = .FALSE.
   INTEGER :: MN_ALLOCATED_NEL = 0, MN_ALLOCATED_NCETOP = 0

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
   SUBROUTINE mnamm (llee, mnpr, nbotce, ncetop, nel, nelee, nlf, nlyree, ns, ncolmb, nlyr, nlyrbt, ntsoil, gnn, kplamm, kuamm, &
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
      PARAMETER (niters = 20, warn = 3)
      PARAMETER (errtol = 1.0d-12)

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
               retamm = 1.0d0 + (kddsol(jsoil) * (namm(nelm, ncl) / mncref)**(gnn - 1.0d0)) / vstheo(nelm, ncl)

               ttheth = (vsthe(ncl, nelm) + vstheo(nelm, ncl)) / 2.0d0

               ! * iteration loop to calculate the new ammonium nitrogen
               ! * concentrations in the soil water
               iteration_loop: DO ntime = 1, niters

                  ! * new retardation factor for ammonium adsorption
                  retamm1 = 1.0d0 + (kddsol(jsoil) * (namm1(nelm, ncl) / mncref)**(gnn - 1.0d0)) / vsthe(ncl, nelm)

                  ! * calculation of both the mineralisation rate and the
                  ! * immobilisation rate of ammonium
                  IF (gam(nelm, ncl) >= 0.0d0) THEN
                     miner(nelm, ncl) = gam(nelm, ncl)
                     imamm(nelm, ncl) = 0.0d0
                  ELSE
                     miner(nelm, ncl) = 0.0d0
                     imamm(nelm, ncl) = MIN(-gam(nelm, ncl), kuamm * nammh)
                  END IF

                  ! * calculation of the nitrification rate
                  ntrf(nelm, ncl) = ttheth * knit(nelm, ncl) * ent(nelm, ncl) * enph(nelm, ncl) * nammh

                  ! * calculation of the ammonia volatilisation rate
                  vol(nelm, ncl) = ttheth * kvol(nelm, ncl) * emt(nelm, ncl) * nammh

                  ! * calculation of the plant uptake rate of ammonium
                  IF (nammh > 0.0d0) THEN
                     dum1 = plup(nelm, ncl) * (pphi(nelm, ncl) * nammh / (ndnit(nelm, ncl) + nammh) + &
                            (1.0d0 - pphi(nelm, ncl)) * nammh / (ndsnt(nelm, ncl) + nammh))
                  ELSE
                     dum1 = 0.0d0
                  END IF
                  dum2 = vsthe(ncl, nelm) * kplamm * nammh
                  plamm(nelm, ncl) = MIN(dum1, dum2)

                  ! * calculation of the concentration of ammonium in solution
                  ! * at timestep n + 1
                  dum = -plamm(nelm, ncl) + miner(nelm, ncl) - imamm(nelm, ncl) - ntrf(nelm, ncl) - vol(nelm, ncl) + naamm(nelm, ncl)
                  namm1(nelm, ncl) = 1.0d0 / (vsthe(ncl, nelm) * retamm1) * (vstheo(nelm, ncl) * namm(nelm, ncl) * retamm + dtuz * dum)

                  ! * ammonium conc at timestep n + 1/2 is calculated for use
                  ! * in the new calculation of the ammonium
                  nammh = (namm1(nelm, ncl) + namm(nelm, ncl)) / 2.0d0

                  ! * relative error between iterations to see if the
                  ! * iteration is converging.
                  IF (namm1(nelm, ncl) /= 0.0d0) THEN
                     werr1 = (namm1(nelm, ncl) - namm1o) / namm1(nelm, ncl)
                  ELSE IF (namm1o == 0.0d0) THEN
                     werr1 = 0.0d0
                  ELSE
                     werr1 = 1.0d0
                  END IF

                  ! * square of the errors, in order to make them positive
                  wer1sq = werr1 * werr1
                  namm1o = namm1(nelm, ncl)

                  ! * break out of loop if the error in the iteration
                  ! * is less than the error tolerance
                  IF (wer1sq < errtol) EXIT iteration_loop

               END DO iteration_loop

               ! * If the DO loop ran all the way through to niters without
               ! * exiting early, it has failed to converge
               IF (ntime > niters) THEN
                  WRITE (msg, 9000) wer1sq
                  CALL ERROR(warn, 3018, mnpr, 0, 0, msg)
               END IF

            END DO layer_loop
         END DO
      END DO

9000  FORMAT('iteration loop in mnamm failed to converge with error = ', g15.7)

   END SUBROUTINE mnamm

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
   subroutine mnco2 (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,fe,fh,isbotc)

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
      integer nbotm,ncl,nelm
      double precision chumh,clith,cmanh,dum,erf,klittp,kmantp
      !
      !-------------------------------------------------------------------*
      !
      do nelm = nlf+1,nel
         if (isbotc) then
            nbotm = nbotce
         else
            nbotm = ncolmb(nelm)
         endif
         do ncl = nbotm,ncetop
            !
            !          * initialise local variables
            chumh = ( chum(nelm,ncl) + chum1(nelm,ncl) )/2.0d0
            clith = ( clit(nelm,ncl) + clit1(nelm,ncl) )/2.0d0
            cmanh = ( cman(nelm,ncl) + cman1(nelm,ncl) )/2.0d0
            !
            !         * if immobilisation is not equal to the potential
            !         * immobilisation then the decomposition of the litter pool
            !         * and the manure pool are temporarily stopped
            if (isimtf(nelm,ncl)) then
               klittp=0.0d0
               kmantp=0.0d0
            else
               klittp=klit(nelm,ncl)
               kmantp=kman(nelm,ncl)
            endif
            !
            erf = emt(nelm,ncl)*emph(nelm,ncl)
            dum = (1-fe)*(1-fh)*klittp*erf*clith
            dum = dum + (1-fe)*khum(nelm,ncl)*erf*chumh
            dum = dum + (1-fe)*kmantp*erf*cmanh
            !
            cdort(nelm,ncl) = dum
            !
         enddo
      enddo
      !
      !
   end subroutine mnco2

!> @brief Reports whether the mineral-nitrogen component has completed setup.
   LOGICAL FUNCTION MNISINITIALISED()
      MNISINITIALISED = MN_INITIALISED
   END FUNCTION MNISINITIALISED

!> @brief Allocates persistent mineral-nitrogen state and timestep workspace.
   SUBROUTINE MNALLOCATE(NEL, NCETOP)
      INTEGER, INTENT(IN) :: NEL, NCETOP

      IF (ALLOCATED(CAHUM)) THEN
         IF (MN_ALLOCATED_NEL /= NEL .OR. MN_ALLOCATED_NCETOP /= NCETOP) &
            ERROR STOP 'MN state was already allocated with different dimensions'
         RETURN
      END IF

      ALLOCATE(CAHUM(NEL,NCETOP), CALIT(NEL,NCETOP), CAMAN(NEL,NCETOP), CDORT(NEL,NCETOP), &
               CHUM(NEL,NCETOP), CHUM1(NEL,NCETOP), CLIT(NEL,NCETOP), CLIT1(NEL,NCETOP), &
               CMAN(NEL,NCETOP), CMAN1(NEL,NCETOP))

      ALLOCATE(DENIT(NEL,NCETOP), DUMMY4(NCETOP,NEL), DUMMY6(NEL,NCETOP))
      ALLOCATE(EDETH(NEL,NCETOP), EMPH(NEL,NCETOP), EMT(NEL,NCETOP), ENPH(NEL,NCETOP), ENT(NEL,NCETOP))
      ALLOCATE(GAM(NEL,NCETOP), GAMTMP(NEL,NCETOP), IMAMM(NEL,NCETOP), IMDIFF(NEL,NCETOP), &
               IMNIT(NEL,NCETOP), ISIMTF(NEL,NCETOP))
      ALLOCATE(KD1(NEL,NCETOP), KD2(NEL,NCETOP), KHUM(NEL,NCETOP), KLIT(NEL,NCETOP), &
               KMAN(NEL,NCETOP), KNIT(NEL,NCETOP), KVOL(NEL,NCETOP))
      ALLOCATE(MINER(NEL,NCETOP))
      ALLOCATE(NAAMM(NEL,NCETOP), NAMM(NEL,NCETOP), NAMM1(NEL,NCETOP), NANIT(NEL,NCETOP), &
               NDNIT(NEL,NCETOP), NDSNT(NEL,NCETOP), NLIT(NEL,NCETOP), NLIT1(NEL,NCETOP), &
               NMAN(NEL,NCETOP), NMAN1(NEL,NCETOP), NTRF(NEL,NCETOP))
      ALLOCATE(PLAMM(NEL,NCETOP), PLNIT(NEL,NCETOP), PLUP(NEL,NCETOP), PPHI(NEL,NCETOP))
      ALLOCATE(SNIT(NEL,NCETOP), TEMP(NEL,NCETOP), VOL(NEL,NCETOP))

      ALLOCATE(MN_WORK%IDUM(NELEE), MN_WORK%DUMMY(NELEE), MN_WORK%LDUM(NELEE))
      ALLOCATE(MN_WORK%CDPTHB(NELEE), MN_WORK%CLTFCT(NELEE), MN_WORK%CMNFCT(NELEE), &
               MN_WORK%CNRAL(NELEE), MN_WORK%CNRALT(NELEE), MN_WORK%CNRAM(NELEE), &
               MN_WORK%CNRAMN(NELEE), MN_WORK%CTOT(NELEE), MN_WORK%NAMFCT(NELEE), &
               MN_WORK%NDPTHB(NELEE), MN_WORK%NTOT(NELEE))

      MN_ALLOCATED_NEL = NEL
      MN_ALLOCATED_NCETOP = NCETOP
   END SUBROUTINE MNALLOCATE

!> @brief Performs the explicit one-time setup for the mineral-nitrogen component.
!>
!> `CMSIM` calls this routine instead of advancing MN on its first call after
!> contaminant setup. This deliberately preserves the legacy one-call delay:
!> initial MN source/sink terms are zero for that contaminant solve, and the
!> first MN process timestep occurs on the following `CMSIM` call.
   SUBROUTINE MNINITIALISE(MND, MNFC, MNFN, MNPL, MNPR, MNOUTPL, NCETOP, NCON, NEL, NLF, NS, NV, NX, NY, &
                           ICMBK, ICMREF, ICMXY, NCOLMB, NLYR, NVC, NLYRBT, NTSOIL, D0, TIH, RHOPL, Z2, &
                           DELONE, DXQQ, DYQQ, VSPOR, DELTAZ, PLAI, ZVSNOD, BEXBK, LINKNS, CLAI, TA, SSS1, SSS2)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: MND, MNFC, MNFN, MNPL, MNPR, MNOUTPL
      INTEGER, INTENT(IN) :: NCETOP, NCON, NEL, NLF, NS, NV, NX, NY
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2), ICMREF(NELEE, 4, 2:2), ICMXY(NXEE, NY)
      INTEGER, INTENT(IN) :: NVC(NELEE), NLYRBT(NEL, NLYREE), NTSOIL(NEL, NLYREE)
      INTEGER, INTENT(INOUT) :: NCOLMB(NELEE), NLYR(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: D0, TIH, RHOPL, Z2
      DOUBLE PRECISION, INTENT(IN) :: DELONE(NPLTEE), PLAI(NV), CLAI(NV)
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NELEE), DYQQ(NELEE), VSPOR(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(LLEE, NEL), ZVSNOD(LLEE, NEL), TA(NV)
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP + 1), SSS2(NEL, NCETOP + 1)
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS(NLFEE)

      INTEGER, PARAMETER :: NMNEEE = 9, NMNTEE = 10
      INTEGER :: NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E
      INTEGER :: NMN27E, NMN43E, NMN53E
      INTEGER, ALLOCATABLE :: CELEM(:), KD1ELM(:), KD2ELM(:), KHELEM(:), KLELEM(:)
      INTEGER, ALLOCATABLE :: KMELEM(:), KNELEM(:), KVELEM(:), NAELEM(:)
      INTEGER :: NMN15T(NMNEEE), NMN17T(NMNEEE), NMN19T(NMNEEE)
      INTEGER :: NMN21T(NMNEEE), NMN23T(NMNEEE), NMN25T(NMNEEE)
      INTEGER :: NMN27T(NMNEEE), NMN43T(NMNEEE), NMN53T(NMNEEE)
      INTEGER, ALLOCATABLE :: DUMMY2(:, :), IDUM1X(:)
      INTEGER :: DUMMY3(NLYREE)
      DOUBLE PRECISION :: CLITFR, CNRLIT
      DOUBLE PRECISION, ALLOCATABLE :: CTOTTP(:), DAMHLF(:), DCHLF(:), NAMTOP(:)
      DOUBLE PRECISION :: CCONC(NMNEEE, NMNTEE), CDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KD1CNC(NMNEEE, NMNTEE), KD1DTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KD2CNC(NMNEEE, NMNTEE), KD2DTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KHCONC(NMNEEE, NMNTEE), KHDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KLCONC(NMNEEE, NMNTEE), KLDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KMCONC(NMNEEE, NMNTEE), KMDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KNCONC(NMNEEE, NMNTEE), KNDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KVCONC(NMNEEE, NMNTEE), KVDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: NACONC(NMNEEE, NMNTEE), NADPTH(NMNEEE, NMNTEE)
      LOGICAL :: ISICCD, ISIAMD
      LOGICAL :: LDUM2(LLEE)

      IF (MN_INITIALISED) ERROR STOP 'MNINITIALISE was called more than once'

      CALL MNALLOCATE(NEL, NCETOP)
      TA(1:NV) = 10.0D0
      CALL MNPLANTINITIALISE(MNPL, MNOUTPL, NEL, NLF, NV, NVC, RHOPL, DELONE, DXQQ, DYQQ, PLAI, CLAI)

      ALLOCATE(CELEM(NELEE), KD1ELM(NELEE), KD2ELM(NELEE), KHELEM(NELEE), KLELEM(NELEE), &
               KMELEM(NELEE), KNELEM(NELEE), KVELEM(NELEE), NAELEM(NELEE))
      ALLOCATE(DUMMY2(NLYREE, NELEE), IDUM1X(NELEE + 3))
      ALLOCATE(CTOTTP(NELEE), DAMHLF(NELEE), DCHLF(NELEE), NAMTOP(NELEE))

      CALL MNERR0(LLEE, MND, MNFC, MNFN, MNPR, NCETOP, NCON, NCONEE, NEL, NELEE, NLF, NLFEE, NLYREE, NMNEEE, NMNTEE, NS, NSEE, NV, &
                  NVEE, NX, NXEE, NY)
      CALL MNERR1(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NLFEE, NLYREE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, &
                  NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, ZVSNOD, BEXBK, LINKNS, DUMMY2, DUMMY3, MN_WORK%IDUM, IDUM1X, &
                  MN_WORK%LDUM, LDUM2)
      CALL MNRED1(MND, MNPR, NEL, NELEE, NLF, NLFEE, NMNEEE, NMNTEE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, BEXBK, LINKNS, &
                  MN_CONFIG%NBOTCE, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, CELEM(NLF + 1:NEL), &
                  KD1ELM(NLF + 1:NEL), KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), &
                  KNELEM(NLF + 1:NEL), KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, &
                  NMN27T, NMN43T, NMN53T, MN_CONFIG%AMMDDR, MN_CONFIG%AMMWDR, CLITFR, MN_CONFIG%CNRBIO, MN_CONFIG%CNRHUM, CNRLIT, &
                  MN_CONFIG%FE, MN_CONFIG%FH, MN_CONFIG%GNN, MN_CONFIG%KPLAMM, MN_CONFIG%KPLNIT, MN_CONFIG%KUAMM, MN_CONFIG%KUNIT, &
                  MN_CONFIG%MNCREF, MN_CONFIG%NITDDR, MN_CONFIG%NITWDR, MN_CONFIG%Q10M, MN_CONFIG%Q10N, CCONC, CDPTH, &
                  CTOTTP(NLF + 1:NEL), DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), KD1CNC, KD1DTH, KD2CNC, KD2DTH, MN_CONFIG%KDDSOL, &
                  KHCONC, KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP(NLF + 1:NEL), &
                  ISICCD, ISIAMD, MN_CONFIG%ISQ10, MN_WORK%IDUM, MN_WORK%DUMMY)
      CALL MNERR2(MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, &
                  NMN53E, NMNEEE, NMNTEE, NS, CELEM(NLF + 1:NEL), KD1ELM(NLF + 1:NEL), KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), &
                  KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), KNELEM(NLF + 1:NEL), KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NMN15T, &
                  NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, MN_CONFIG%AMMDDR, MN_CONFIG%AMMWDR, CLITFR, &
                  MN_CONFIG%CNRBIO, MN_CONFIG%CNRHUM, CNRLIT, MN_CONFIG%FE, MN_CONFIG%FH, MN_CONFIG%GNN, MN_CONFIG%KPLAMM, &
                  MN_CONFIG%KPLNIT, MN_CONFIG%KUAMM, MN_CONFIG%KUNIT, MN_CONFIG%MNCREF, MN_CONFIG%NITDDR, MN_CONFIG%NITWDR, &
                  MN_CONFIG%Q10M, MN_CONFIG%Q10N, CCONC, CDPTH, CTOTTP(NLF + 1:NEL), DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), &
                  KD1CNC, KD1DTH, KD2CNC, KD2DTH, MN_CONFIG%KDDSOL, KHCONC, KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, &
                  KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP(NLF + 1:NEL), ISICCD, ISIAMD, MN_WORK%LDUM)
      CALL MNINIT(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, &
                  NMN53E, NMNEEE, NMNTEE, CELEM(NLF + 1:NEL), KD1ELM(NLF + 1:NEL), KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), &
                  KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), KNELEM(NLF + 1:NEL), KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NCOLMB, &
                  NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, CLITFR, CNRLIT, CCONC, CDPTH, &
                  CTOTTP(NLF + 1:NEL), DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), DELTAZ, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KHCONC, &
                  KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP(NLF + 1:NEL), &
                  ZVSNOD, ISICCD, ISIAMD, SSS1, SSS2, MN_CONFIG%ISBOTC)

      MN_INITIALISED = .TRUE.
   END SUBROUTINE MNINITIALISE

!> @brief Controls the mineral nitrogen component from the contaminant timestep.
!>
!> `MNCONT` is called by [[cmmod:cmsim]] after [[mninitialise]] has allocated
!> and initialised the mineral-nitrogen component. It computes potential plant
!> nitrogen uptake with [[mnplant]], then calls [[mnmain]] to advance mineral
!> nitrogen state and fill `SSS1` and `SSS2`, which replace the contaminant
!> source/sink arrays used by the CM transport equations.
!>
!> | Phase | Main action |
!> |:------|:------------|
!> | Temporary temperature setup | Set every vegetation air-temperature entry `TA(1:NV)` to 10.0 before plant uptake and the main nitrogen update. |
!> | Plant uptake | Call [[mnplant]] to calculate nitrogen plant uptake demand and related plant output. |
!> | Main MN update | Call [[mnmain]] to initialise/check/read inputs on the first pass and then update ammonium/nitrate source-sink terms. |
!>
!> The dissolved nitrate concentration fields are supplied through the CM arrays
!> `cccc` and `ssss`; ammonium, litter, humus, manure, and process-rate pools are
!> held internally by `MNmod`. Rates and pools are evaluated over land columns
!> from `NLF+1:NEL`; channel links are not treated as nitrogen soil columns.
!>
!> @note `MNCONT` overwrites the incoming `TA` values with 10 deg C before
!> [[mnplant]] and [[mnmain]] are called.
!> @endnote
!>
!> @warning The legacy source comments note that [[mnplant]] has limited input
!> checking. The main nitrogen update path performs more extensive validation in
!> [[mnerr0]], [[mnerr1]], [[mnerr2]], [[mnerr3]], and [[mnerr4]].
!> @endwarning
!>
!> @warning [[cmmod:cmsim]] passes `ICMREF(1:NEL,5)` to the explicit-shape
!> `ICMREF(NEL,4,2:2)` dummy. The MN checks then index four faces, relying on
!> contiguous storage from columns 5--8 beyond the declared one-column actual
!> section. This retained coupling is compiler-sensitive and is not changed
!> here.
!> @endwarning
   SUBROUTINE MNCONT(MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NEL, NLF, NS, NV, NX, NY, &
                     ICMBK, ICMREF, ICMXY, NCOLMB, NLYR, NRD, NLYRBT, NTSOIL, &
                     D0, TIH, RHOPL, Z2, DELONE, DXQQ, DYQQ, VSPOR, DELTAZ, RDF, ZVSNOD, BEXBK, &
                     LINKNS, DTUZ, UZNOW, CLAI, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, SSS1, SSS2)

      IMPLICIT NONE

      ! --- Input arguments ---
      ! Static
      INTEGER, INTENT(IN) :: MNFC  !! Scheduled carbon-addition input unit.
      INTEGER, INTENT(IN) :: MNFN  !! Scheduled nitrogen-addition input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: MNOUT1  !! Carbon budget output unit.
      INTEGER, INTENT(IN) :: MNOUT2  !! Nitrogen budget output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NV  !! Number of vegetation/meteorological entries.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLF, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NEL, 4, 2:2)  !! Neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NX, NY)  !! Element number at each grid location.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, *)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, *)  !! Soil type index for each element layer.

      DOUBLE PRECISION, INTENT(IN) :: D0  !! Reference diffusion/dispersion scale used by CM.
      DOUBLE PRECISION, INTENT(IN) :: TIH  !! Initial simulation time in hours.
      DOUBLE PRECISION, INTENT(IN) :: RHOPL  !! Plant dry-matter density used by uptake calculation.
      DOUBLE PRECISION, INTENT(IN) :: Z2  !! Vertical length scale used by CM and MN temperature diffusion.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLF)  !! True for north-south channel links.

      ! Varying
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)  !! Dynamic-region nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)  !! Dead-space nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: VSPSI(NCETOP, NEL)  !! Matric potential/pressure head by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: VSTHEO(NEL, NCETOP + 1)  !! Previous volumetric water content.

      ! --- In/Out arguments (Propagated up from MNMAIN / MNPLANT strict architectures) ---
      INTEGER, INTENT(INOUT) :: NCOLMB(NEL)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(INOUT) :: NLYR(NEL)  !! Number of soil layers in each element.
      INTEGER, INTENT(INOUT) :: NRD(NV)  !! Rooting depth in cell counts by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: DELONE(*)  !! Initial plant biomass/cover scaling by plant type.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NEL)  !! Element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NEL)  !! Element length.
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)  !! Soil porosity by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(*)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: RDF(NV, *)  !! Root density fraction by vegetation type and cell.
      DOUBLE PRECISION, INTENT(INOUT) :: ZVSNOD(*)  !! Vertical node elevation/depth by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: CLAI(NV)  !! Current canopy leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NEL)  !! Net precipitation/effective rainfall by element.
      DOUBLE PRECISION, INTENT(INOUT) :: TA(NV)  !! Air temperature overwritten with 10 deg C before MN calculations.

      ! --- Output arguments ---
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP + 1)  !! Dynamic-region CM source/sink array.
      DOUBLE PRECISION, INTENT(OUT) :: SSS2(NEL, NCETOP + 1)  !! Dead-space CM source/sink array.

      ! --- Local variables ---
      INTEGER :: I

   !----------------------------------------------------------------------*

      IF (.NOT. MN_INITIALISED) ERROR STOP 'MNCONT called before MNINITIALISE'

      ! Retained MN behaviour: use a fixed 10 deg C temperature input.
      DO I = 1, NV
         TA(I) = 10.0D0
      END DO

      CALL MNPLANT(NCETOP, NEL, NLF, NV, NCOLMB, NRD, RHOPL, DELONE, DXQQ, DYQQ, DELTAZ, RDF, DTUZ, UZNOW, CLAI)

      CALL MNMAIN(MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NEL, NLF, NS, NV, NX, NY, ICMBK, &
                  ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, &
                  ZVSNOD, BEXBK, LINKNS, DTUZ, UZNOW, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, &
                  SSS1, SSS2)

   END SUBROUTINE MNCONT

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
   SUBROUTINE mnedth (llee, nbotce, ncetop, nel, nelee, nlf, nlyree, ns, &
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
               relsat = vsthe(nce, nelm) / vspor(jsoil)

               IF (relsat > 1.0d0) THEN
                  edeth(nelm, nce) = 1.0d0
               ELSE IF (relsat > 0.9d0) THEN
                  edeth(nelm, nce) = -7.0d0 + 8.0d0 * relsat
               ELSE IF (relsat > 0.8d0) THEN
                  edeth(nelm, nce) = -1.6d0 + 2.0d0 * relsat
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
   subroutine mnemph (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,vspsi,isbotc)

      integer llee  !! Maximum soil-cell dimension.
      integer nbotce  !! Lowest cell included when bottom-cell truncation is active.
      integer ncetop  !! Top soil-cell index.
      integer nel  !! Number of elements.
      integer nelee  !! Element-array dimension.
      integer nlf  !! Number of overland/channel links excluded from land-column updates.
      integer ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      double precision vspsi(ncetop,nel)  !! Matric potential/pressure head by cell and element.
      logical isbotc  !! True when the fixed lower active cell `NBOTCE` is used.
      !
      !
      ! output arguments
      !double precision emph(nelee,llee)
      !
      ! locals
      integer nbotm,ncl,nelm
      !
      !-------------------------------------------------------------------*
      !
      do nelm = nlf+1,nel
         if (isbotc) then
            nbotm = nbotce
         else
            nbotm = ncolmb(nelm)
         endif
         do ncl = nbotm,ncetop
            !
            !          * a segmented relationship is being used with the
            !          * matric potential falling into one of five bands
            if (vspsi(ncl,nelm)>-0.1d-1) then
               emph(nelm,ncl) = 0.6
            elseif (vspsi(ncl,nelm)>-0.6d0) then
               emph(nelm,ncl) = 1.05d0 + 0.225d0*log10(-vspsi(ncl,nelm))
            elseif (vspsi(ncl,nelm)>-3.0d0) then
               emph(nelm,ncl) = 1.0d0
            elseif (vspsi(ncl,nelm)>-1.0d4) then
               emph(nelm,ncl) =1.136d0 - 0.284d0*log10(-vspsi(ncl,nelm))
            else
               emph(nelm,ncl) = 0.0d0
            endif
            !
         enddo
      enddo
      !
   end    subroutine mnemph

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
   subroutine mnemt (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,q10m,isbotc,isq10)

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
      integer nbotm,ncl,nelm
      !
      !-------------------------------------------------------------------*
      !
      do nelm = nlf+1,nel
         if (isbotc) then
            nbotm = nbotce
         else
            nbotm = ncolmb(nelm)
         endif
         do ncl = nbotm,ncetop
            !
            !
            !         * the reduction factor can be calculated either using a segmented
            !         * relationship or a q10 factor
            if (isq10) then
               emt(nelm,ncl) = q10m**((temp(nelm,ncl)-30.0d0)/10.0d0)
               !
            else
               !             * a segmented relationship is being used with the
               !             * temperature falling into one of five bands
               if (temp(nelm,ncl)>=30.0d0) then
                  emt(nelm,ncl) = 1.0d0
               elseif (temp(nelm,ncl)>20.0d0) then
                  emt(nelm,ncl) = -0.5d0 + 0.5d-1 * temp(nelm,ncl)
               elseif (temp(nelm,ncl)>10.0d0) then
                  emt(nelm,ncl) = -0.1d0 + 0.3d-1 * temp(nelm,ncl)
               elseif (temp(nelm,ncl)>0.0d0) then
                  emt(nelm,ncl) = 0.2d-1 * temp(nelm,ncl)
               else
                  emt(nelm,ncl) = 0.0d0
               endif
               !
            endif
            !
         enddo
      enddo
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
   subroutine mnenph (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,vspsi,isbotc)

      integer llee  !! Maximum soil-cell dimension.
      integer nbotce  !! Lowest cell included when bottom-cell truncation is active.
      integer ncetop  !! Top soil-cell index.
      integer nel  !! Number of elements.
      integer nelee  !! Element-array dimension.
      integer nlf  !! Number of overland/channel links excluded from land-column updates.
      integer ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      double precision vspsi(ncetop,nel)  !! Matric potential/pressure head by cell and element.
      logical isbotc  !! True when the fixed lower active cell `NBOTCE` is used.
      !
      !
      ! output arguments
      !double precision enph(nelee,llee)
      !
      ! locals
      integer nbotm,ncl,nelm
      !
      !-------------------------------------------------------------------*
      !
      do nelm = nlf+1,nel
         if (isbotc) then
            nbotm = nbotce
         else
            nbotm = ncolmb(nelm)
         endif
         do ncl = nbotm,ncetop
            !
            !           * a segmented relationship is being used with the
            !           * matric potential falling into one of five bands
            !
            if (vspsi(ncl,nelm)>-0.1d-1) then
               enph(nelm,ncl) = 0.6
            elseif (vspsi(ncl,nelm)>-0.6d0) then
               enph(nelm,ncl) = 1.05d0 + 0.225d0*log10(-vspsi(ncl,nelm))
            elseif (vspsi(ncl,nelm)>-3.0d0) then
               enph(nelm,ncl) = 1.0d0
            elseif (vspsi(ncl,nelm)>-1.0d4) then
               enph(nelm,ncl) =1.136d0 - 0.284d0*log10(-vspsi(ncl,nelm))
            else
               enph(nelm,ncl) = 0.0d0
            endif
            !
         enddo
      enddo
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
   subroutine mnent (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,q10n,isbotc,isq10)

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
      integer nbotm,ncl,nelm
      !
      !-------------------------------------------------------------------*
      !
      do nelm = nlf+1,nel
         if (isbotc) then
            nbotm = nbotce
         else
            nbotm = ncolmb(nelm)
         endif
         do ncl = nbotm,ncetop
            !
            !
            !           * the reduction factor can be calculated either using a segmented
            !           * relationship or a q10 factor
            if (isq10) then
               ent(nelm,ncl) = q10n**((temp(nelm,ncl)-30.0d0)/10.0d0)
               !
            else
               !             * a segmented relationship is being used with the
               !             * temperature falling into one of five bands
               if (temp(nelm,ncl)>=30.0d0) then
                  ent(nelm,ncl) = 1.0d0
               elseif (temp(nelm,ncl)>20.0d0) then
                  ent(nelm,ncl) = -0.5d0 + 0.5d-1 * temp(nelm,ncl)
               elseif (temp(nelm,ncl)>10.0d0) then
                  ent(nelm,ncl) = -0.1d0 + 0.3d-1 * temp(nelm,ncl)
               elseif (temp(nelm,ncl)>2.0d0) then
                  ent(nelm,ncl) = -0.5d-1 + 0.25d-1 * temp(nelm,ncl)
               else
                  ent(nelm,ncl) = 0.0d0
               endif
               !
            endif
            !
         enddo
      enddo
      !
   end subroutine mnent

!> @brief Checks fixed MN array dimensions, entity counts, and selected file units.
!>
!> `mnerr0` validates the static bounds needed before MN arrays are used.
!>
!> | Group | Checks |
!> | --- | --- |
!> | Fixed array limits | `LLEE >= NCETOP`; `NCONEE >= NCON`; `NELEE >= NEL`; `NLFEE >= max(1, NLF)`; `NLYREE > 0`; `NSEE >= NS`; `NVEE >= NV`; `NXEE >= NX` and `NXEE <= 9999`; `NMNEEE > 0`; `NMNTEE > 0`. |
!> | Entity counts | `0 <= NLF < NEL`; `min(NCETOP, NS, NV) > 0`; `min(NX, NY) > 0`. |
!> | Contaminant contract | MN is coupled to exactly one contaminant species: `NCON == 1`. |
!> | File units | Only `MND`, `MNFC`, `MNFN`, and `MNPR` are checked here, and all must be non-negative. |
!>
!> Detailed failures use errors `3020`-`3033`; any failure is followed by
!> fatal summary error `3010`.
   SUBROUTINE MNERR0(LLEE, MND, MNFC, MNFN, MNPR, NCETOP, NCON, NCONEE, NEL, NELEE, NLF, NLFEE, NLYREE, NMNEEE, NMNTEE, NS, NSEE, NV, NVEE, NX, NXEE, NY)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: MND  !! Static MND input unit.
      INTEGER, INTENT(IN) :: MNFC  !! Scheduled carbon-addition input unit.
      INTEGER, INTENT(IN) :: MNFN  !! Scheduled nitrogen-addition input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NCON  !! Number of contaminant species coupled to MN.
      INTEGER, INTENT(IN) :: NCONEE  !! Contaminant-species array dimension.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NLYREE  !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NMNEEE  !! Maximum number of MN category entries.
      INTEGER, INTENT(IN) :: NMNTEE  !! Maximum number of MN table entries.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NSEE  !! Soil-type array dimension.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NV  !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NVEE  !! Vegetation-type array dimension.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2

      INTEGER, PARAMETER :: IUNDEF = 0

      INTEGER :: NERR
      INTEGER :: IDUMS(1), IDUMO(1)
      LOGICAL :: LDUM1(1)

      ! Replaced implicitly-saved DATA blocks with proper PARAMETER arrays
      INTEGER, PARAMETER :: IZERO(1) = [0]
      INTEGER, PARAMETER :: IONE(1)  = [1]

   !-------------------------------------------------------------------*

   ! 0. preliminaries
   ! ----------------
   ! initialize local counter
      NERR = 0

   ! 1. array sizes
   ! --------------

   ! llee
      IDUMS(1) = LLEE
      IDUMO(1) = NCETOP
      CALL ALCHKI(ERR, 3020, MNPR, 1, 1, IUNDEF, IUNDEF, 'llee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

   ! nconee
      IDUMS(1) = NCONEE
      IDUMO(1) = NCON
      CALL ALCHKI(ERR, 3021, MNPR, 1, 1, IUNDEF, IUNDEF, 'nconee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

   ! nelee
      IDUMS(1) = NELEE
      IDUMO(1) = NEL
      CALL ALCHKI(ERR, 3022, MNPR, 1, 1, IUNDEF, IUNDEF, 'nelee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

   ! nlfee
      IDUMS(1) = NLFEE
      IDUMO(1) = MAX(1, NLF)
      CALL ALCHKI(ERR, 3023, MNPR, 1, 1, IUNDEF, IUNDEF, 'nlfee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

   ! nlyree
      IDUMS(1) = NLYREE
      CALL ALCHKI(ERR, 3024, MNPR, 1, 1, IUNDEF, IUNDEF, 'nlyree', 'GT', IZERO, IDUMS, NERR, LDUM1)

   ! nsee
      IDUMS(1) = NSEE
      IDUMO(1) = NS
      CALL ALCHKI(ERR, 3025, MNPR, 1, 1, IUNDEF, IUNDEF, 'nsee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

   ! nvee
      IDUMS(1) = NVEE
      IDUMO(1) = NV
      CALL ALCHKI(ERR, 3026, MNPR, 1, 1, IUNDEF, IUNDEF, 'nvee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

   ! nxee
      IDUMS(1) = NXEE
      IDUMO(1) = NX
      CALL ALCHKI(ERR, 3027, MNPR, 1, 1, IUNDEF, IUNDEF, 'nxee', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      IDUMO(1) = 9999
      CALL ALCHKI(ERR, 3027, MNPR, 1, 1, IUNDEF, IUNDEF, 'nxee', 'LE', IDUMO, IDUMS, NERR, LDUM1)

   ! nmneee
      IDUMS(1) = NMNEEE
      CALL ALCHKI(ERR, 3028, MNPR, 1, 1, IUNDEF, IUNDEF, 'nmneee', 'GT', IZERO, IDUMS, NERR, LDUM1)

   ! nmntee
      IDUMS(1) = NMNTEE
      CALL ALCHKI(ERR, 3028, MNPR, 1, 1, IUNDEF, IUNDEF, 'nmntee', 'GT', IZERO, IDUMS, NERR, LDUM1)


   ! 2. number of entities
   ! ---------------------

   ! nlf
      IDUMS(1) = NLF
      IDUMO(1) = NEL
      CALL ALCHKI(ERR, 3029, MNPR, 1, 1, IUNDEF, IUNDEF, 'nlf', 'GE', IZERO, IDUMS, NERR, LDUM1)
      CALL ALCHKI(ERR, 3029, MNPR, 1, 1, IUNDEF, IUNDEF, 'nlf', 'LT', IDUMO, IDUMS, NERR, LDUM1)

   ! ncetop,ns,nv
      IDUMS(1) = MIN(NCETOP, NS, NV)
      CALL ALCHKI(ERR, 3030, MNPR, 1, 1, IUNDEF, IUNDEF, '[ncetop,ns,nv]', 'GT', IZERO, IDUMS, NERR, LDUM1)

   ! nx, ny
      IDUMS(1) = MIN(NX, NY)
      CALL ALCHKI(ERR, 3031, MNPR, 1, 1, IUNDEF, IUNDEF, '[ nx, ny ]', 'GT', IZERO, IDUMS, NERR, LDUM1)

   ! ncon
      IDUMS(1) = NCON
      CALL ALCHKI(ERR, 3032, MNPR, 1, 1, IUNDEF, IUNDEF, 'ncon', 'EQ', IONE, IDUMS, NERR, LDUM1)

   ! 3. unit numbers
   ! ---------------

   ! mnd,mnfc,mnfn,mnpr
      IDUMS(1) = MIN(MND, MNFC, MNFN, MNPR)
      CALL ALCHKI(ERR, 3033, MNPR, 1, 1, IUNDEF, IUNDEF, '[mnd,mnpr]', 'GE', IZERO, IDUMS, NERR, LDUM1)

   ! 4. epilogue
   ! -----------
      IF (NERR > 0) THEN
         CALL ERROR(FATAL, 3010, MNPR, 0, 0, 'error(s) detected while checking cm-mn interface variables')
      END IF

   END SUBROUTINE MNERR0

!> @brief Checks the static contaminant-to-MN interface variables.
!>
!> `mnerr1` validates the spatial indexing and soil-column geometry handed to
!> the mineral-nitrogen component before initialisation.
!>
!> | Group | Checks |
!> | --- | --- |
!> | Grid and bank identities | Active grid entries in `ICMXY`, plus both bank elements for each link when `BEXBK` is true, must account for exactly `NEL-NLF` column elements (`2075`). Every model element must be represented once (`2076`). |
!> | Bank neighbours | If the identity check passed and banks exist, each link must have at least one bank with an active grid neighbour (`2079`). The checked face is `2*bank`, decremented for north-south links, and the neighbour is read from `ICMREF(element,face,2)`. |
!> | Reference values | `D0 > 0` and `Z2 > 0`. |
!> | Soil properties | Soil porosity satisfies `0 < VSPOR(soil) <= 1`. |
!> | Column geometry | Land-column `DXQQ` and `DYQQ` are positive; `1 <= NLYR <= NLYREE`; `NLYRBT` is strictly increasing and the top-layer boundary equals `NCETOP+1`; `NTSOIL` is in `1:NS`; `0 < NCOLMB <= NCETOP`; active `DELTAZ` values are positive; `ZVSNOD(nce+1,iel) > ZVSNOD(nce,iel)`. |
!> | Time | Initial simulation time `TIH >= 0`. |
!>
!> Detailed interface failures use errors `3035`-`3046`; any failure is followed
!> by fatal summary error `3011`.
   SUBROUTINE MNERR1(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NLFEE, NLYREE, NS, NX, NXEE, NY, ICMBK, ICMREF, &
                     ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, ZVSNOD, &
                     BEXBK, LINKNS, DUMMY2, DUMMY3, IDUM, IDUM1X, LDUM, LDUM2)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NLYREE  !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Neighbour reference map used to validate bank adjacency.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)  !! Element number at each grid location.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE)  !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: D0  !! Reference diffusion/dispersion scale used by CM.
      DOUBLE PRECISION, INTENT(IN) :: TIH  !! Initial simulation time in hours.
      DOUBLE PRECISION, INTENT(IN) :: Z2  !! Vertical length scale used by CM and MN temperature diffusion.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)  !! True for north-south channel links.

      ! Input/Output Arrays (Tested by ALCHK/ALCHKI; subject to internal data reset)
      INTEGER, INTENT(INOUT) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(INOUT) :: NLYR(NELEE)  !! Number of soil layers in each element.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NELEE)  !! Element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NELEE)  !! Element length.
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)  !! Soil porosity by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: ZVSNOD(LLEE, NEL)  !! Vertical node elevation/depth by cell and element.

      ! Workspace arguments (INTENT(INOUT) as they are used for scratch space)
      INTEGER, INTENT(INOUT) :: DUMMY2(NLYREE, NELEE)  !! Integer workspace for layer membership checks.
      INTEGER, INTENT(INOUT) :: DUMMY3(NLYREE)  !! Integer workspace for layer checks.
      INTEGER, INTENT(INOUT) :: IDUM(NELEE)  !! Integer workspace for element accounting.
      INTEGER, INTENT(INOUT) :: IDUM1X(-1:NEL+1)  !! Integer workspace for element identity checks.
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)  !! Logical workspace for element accounting.
      LOGICAL, INTENT(INOUT) :: LDUM2(LLEE)  !! Logical workspace for cell/layer checks.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: BANK, BOTLYR, COUNT, FACE
      INTEGER :: IADJ, ICOL1, IEL, IX, IY
      INTEGER :: LINK, NCE, NCEBOT, NCOL, NELP
      INTEGER :: NERR, NLAYER, TOPLYR
      INTEGER :: IDUM1(2)
      DOUBLE PRECISION :: DUMS(1)
      LOGICAL :: BKXYOK

      INTEGER, PARAMETER :: IZERO_ARR(1) = [0], IONE_ARR(1) = [1]
      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      INTEGER, PARAMETER :: IUNDEF = 0

   !-------------------------------------------------------------------*

   ! 0. preliminaries
   ! ----------------
      NERR = 0
      ICOL1 = NLF + 1
      NELP  = NEL + 1

   ! 1. index arrays
   ! ---------------

   ! icmbk, icmxy
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

      IDUM1(1)  = NEL - NLF
      IDUM1X(0) = NCOL

      CALL ALCHKI(ERR, 2075, MNPR, 1, 1, IUNDEF, IUNDEF, '#_column_elements', 'EQ', IDUM1, IDUM1X(0:0), NERR, LDUM)
      CALL ALCHKI(ERR, 2076, MNPR, 1, NEL, IUNDEF, IUNDEF, 'element_count(iel)', 'EQ', IONE_ARR, IDUM1X(1:NEL), NERR, LDUM)

      BKXYOK = (COUNT == NERR)

   ! icmref (bank element neighbours)
      IF (NLF > 0 .AND. BEXBK .AND. BKXYOK) THEN
         IDUM1X(-1) = -2
         IDUM1X(0)  = 0
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
         CALL ALCHKI(ERR, 2079, MNPR, 1, NLF, IUNDEF, IUNDEF, '#_grids_neighbouring_banks(link)', 'GT', IZERO_ARR, IDUM, NERR, LDUM)
      END IF


   ! 2. contaminant reference values
   ! -------------------------------

   ! d0
      DUMS(1) = D0
      CALL ALCHK(ERR, 3035, MNPR, 1, 1, IUNDEF, IUNDEF, 'd0', 'GT', ZERO_ARR, ZERO_VAL, DUMS, NERR, LDUM)

   ! z2
      DUMS(1) = Z2
      CALL ALCHK(ERR, 3036, MNPR, 1, 1, IUNDEF, IUNDEF, 'z2', 'GT', ZERO_ARR, ZERO_VAL, DUMS, NERR, LDUM)


   ! 3. soil properties
   ! ------------------
   ! vspor
      CALL ALCHK(ERR, 3037, MNPR, 1, NS, IUNDEF, IUNDEF, 'vspor(soil)', 'LE', ONE_ARR, ZERO_VAL, VSPOR, NERR, LDUM)
      CALL ALCHK(ERR, 3037, MNPR, 1, NS, IUNDEF, IUNDEF, 'vspor(soil)', 'GT', ZERO_ARR, ZERO_VAL, VSPOR, NERR, LDUM)


   ! 4. column properties
   ! --------------------

   ! dxqq
      CALL ALCHK(ERR, 3039, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'dxqq(iel)', 'GT', ZERO_ARR, ZERO_VAL, DXQQ(ICOL1:NEL), NERR, LDUM)
   ! dyqq
      CALL ALCHK(ERR, 3039, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'dyqq(iel)', 'GT', ZERO_ARR, ZERO_VAL, DYQQ(ICOL1:NEL), NERR, LDUM)

   ! nlyr
      COUNT = NERR
      IDUM1(1) = 1
      CALL ALCHKI(ERR, 3041, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'nlyr(iel)', 'GE', IDUM1, NLYR(ICOL1:NEL), NERR, LDUM)
      IDUM1(1) = NLYREE
      CALL ALCHKI(ERR, 3041, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'nlyr(iel)', 'LE', IDUM1, NLYR(ICOL1:NEL), NERR, LDUM)

   ! nlyrbt
      IF (COUNT == NERR) THEN
         DO NLAYER = 1, NLYREE
            DO IEL = 1, NEL
               DUMMY2(NLAYER, IEL) = NLYRBT(IEL, NLAYER)
            END DO
         END DO
         DO IEL = ICOL1, NEL
            BOTLYR = 1
            TOPLYR = NLYR(IEL)
            DUMMY3(BOTLYR) = 0
            DO NLAYER = BOTLYR, TOPLYR
               DUMMY3(NLAYER + 1) = DUMMY2(NLAYER, IEL)
            END DO

            CALL ALCHKI(ERR, 3042, MNPR, BOTLYR, TOPLYR + 1, IEL, IUNDEF, 'nlyrbt[nlyr,iel]', 'GTa', DUMMY3(BOTLYR:TOPLYR+1), &
                        DUMMY2(BOTLYR:TOPLYR+1, IEL), NERR, LDUM2)

            IDUM1(1) = NCETOP + 1
            CALL ALCHKI(ERR, 3042, MNPR, TOPLYR, TOPLYR, IEL, IUNDEF, 'nlyrbt[toplyr,iel]', 'EQ', IDUM1(1:1), DUMMY2(TOPLYR+1:TOPLYR+1, IEL), NERR, LDUM2)
         END DO
      END IF

   ! ntsoil
      IF (COUNT == NERR) THEN
         DO NLAYER = 1, NLYREE
            DO IEL = 1, NEL
               DUMMY2(NLAYER, IEL) = NTSOIL(IEL, NLAYER)
            END DO
         END DO
         DO IEL = ICOL1, NEL
            BOTLYR = 1
            TOPLYR = NLYR(IEL)
            CALL ALCHKI(ERR, 3043, MNPR, BOTLYR, TOPLYR, IEL, IUNDEF, 'ntsoil[nlyr,iel]', 'GT', IZERO_ARR, DUMMY2(BOTLYR:TOPLYR, IEL), NERR, LDUM2)
            IDUM1(1) = NS
            CALL ALCHKI(ERR, 3043, MNPR, BOTLYR, TOPLYR, IEL, IUNDEF, 'ntsoil[nlyr,iel]', 'LE', IDUM1(1:1), DUMMY2(BOTLYR:TOPLYR, IEL), NERR, LDUM2)
         END DO
      END IF

   ! ncolmb
      IDUM1(1) = NCETOP
      CALL ALCHKI(ERR, 3044, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ncolmb(iel)', 'GT', IZERO_ARR, NCOLMB(ICOL1:NEL), NERR, LDUM)
      CALL ALCHKI(ERR, 3044, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ncolmb(iel)', 'LE', IDUM1, NCOLMB(ICOL1:NEL), NERR, LDUM)

   ! deltz,zvsnod
      DO IEL = ICOL1, NEL
         DO NCE = NCOLMB(IEL), NCETOP
            DUMMY4(NCE, IEL) = DELTAZ(NCE, IEL)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3045, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'deltaz[ncl,iel]', 'GT', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO
      DO IEL = ICOL1, NEL
         DO NCE = NCOLMB(IEL), NCETOP - 1
            DUMS(1) = ZVSNOD(NCE, IEL)
            DUMMY4(NCE+1, IEL) = ZVSNOD(NCE+1, IEL)
            CALL ALCHK(ERR, 3045, MNPR, NCE + 1, NCE + 1, IEL, IUNDEF, 'zvsnod', 'GT', DUMS(1:1), ZERO_VAL, DUMMY4(NCE+1:NCE+1, IEL), NERR, LDUM2)
         END DO
      END DO

   ! 5. time properties
   ! ------------------
   ! tih
      DUMS(1) = TIH
      CALL ALCHK(ERR, 3046, MNPR, 1, 1, IUNDEF, IUNDEF, 'tih', 'GE', ZERO_ARR, ZERO_VAL, DUMS, NERR, LDUM)


   ! 6. epilogue
   ! -----------
      IF (NERR > 0) THEN
         CALL ERROR(FATAL, 3011, MNPR, 0, 0, 'error(s) detected while checking static/initial interface')
      END IF

   END SUBROUTINE MNERR1

!> @brief Checks static mineral-nitrogen input read by [[mnred1]].
!>
!> `mnerr2` validates the nitrogen and carbon data file after [[mnred1]] has
!> loaded it. Land-column checks run over elements `NLF+1:NEL`.
!>
!> | Group | Checks |
!> | --- | --- |
!> | Uptake and immobilisation | `KUAMM`, `KPLAMM`, `KUNIT`, and `KPLNIT` are non-negative. |
!> | Carbon cycling scalars | `0 <= FE <= 1`, `0 <= FH <= 1`, and `CNRBIO`, `CNRHUM`, and `CNRLIT` are positive. The initial-carbon litter fraction `CLITFR` must be in `0:1`. |
!> | Temperature and deposition scalars | `Q10M` and `Q10N` are non-negative; ammonium and nitrate dry/wet deposition rates are non-negative; `MNCREF > 0`. |
!> | Initial carbon | If `ISICCD` is true, decay-function inputs require `CTOTTP >= 0` and `DCHLF > 0`. Otherwise `CELEM > 0`, initial-carbon table depths start at zero and increase, and table concentrations are non-negative. |
!> | Initial ammonium | If `ISIAMD` is true, decay-function inputs require `NAMTOP >= 0` and `DAMHLF > 0`. Otherwise `NAELEM > 0`, initial-ammonium table depths start at zero and increase, and table concentrations are non-negative. |
!> | Depth-varying process tables | Category ids for `KHUM`, `KLIT`, `KMAN`, `KNIT`, `KVOL`, `KD1`, and `KD2` are positive. Their depth tables start at zero, subsequent depths increase, and table values are non-negative. |
!> | Ammonium adsorption and active depth | `KDDSOL(soil) >= 0` and `NBOTCE < NCETOP`. |
!>
!> Detailed failures use errors `3048`-`3064`; any failure is followed by fatal
!> summary error `3012`.
   SUBROUTINE MNERR2(MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, &
                     NMNEEE, NMNTEE, NS, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, KMELEM, KNELEM, KVELEM, NAELEM, NMN15T, NMN17T, NMN19T, NMN21T, &
                     NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, AMMDDR, AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT, FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT, &
                     MNCREF, NITDDR, NITWDR, Q10M, Q10N, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KDDSOL, KHCONC, KHDPTH, &
                     KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ISICCD, ISIAMD, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NBOTCE  !! Lowest cell included when bottom-cell truncation is active.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column checks.
      INTEGER, INTENT(IN) :: NMN15E  !! Number of humus category entries.
      INTEGER, INTENT(IN) :: NMN17E  !! Number of litter category entries.
      INTEGER, INTENT(IN) :: NMN19E  !! Number of manure category entries.
      INTEGER, INTENT(IN) :: NMN21E  !! Number of nitrification category entries.
      INTEGER, INTENT(IN) :: NMN23E  !! Number of volatilisation category entries.
      INTEGER, INTENT(IN) :: NMN25E  !! Number of KD1 denitrification category entries.
      INTEGER, INTENT(IN) :: NMN27E  !! Number of KD2 denitrification category entries.
      INTEGER, INTENT(IN) :: NMN43E  !! Number of initial-carbon category entries.
      INTEGER, INTENT(IN) :: NMN53E  !! Number of initial-ammonium category entries.
      INTEGER, INTENT(IN) :: NMNEEE  !! Maximum number of MN category entries.
      INTEGER, INTENT(IN) :: NMNTEE  !! Maximum number of MN table entries.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NMN15T(NMNEEE)  !! Humus table length by category.
      INTEGER, INTENT(IN) :: NMN17T(NMNEEE)  !! Litter table length by category.
      INTEGER, INTENT(IN) :: NMN19T(NMNEEE)  !! Manure table length by category.
      INTEGER, INTENT(IN) :: NMN21T(NMNEEE)  !! Nitrification table length by category.
      INTEGER, INTENT(IN) :: NMN23T(NMNEEE)  !! Volatilisation table length by category.
      INTEGER, INTENT(IN) :: NMN25T(NMNEEE)  !! KD1 table length by category.
      INTEGER, INTENT(IN) :: NMN27T(NMNEEE)  !! KD2 table length by category.
      INTEGER, INTENT(IN) :: NMN43T(NMNEEE)  !! Initial-carbon table length by category.
      INTEGER, INTENT(IN) :: NMN53T(NMNEEE)  !! Initial-ammonium table length by category.
      DOUBLE PRECISION, INTENT(IN) :: AMMDDR  !! Dry ammonium deposition rate.
      DOUBLE PRECISION, INTENT(IN) :: AMMWDR  !! Wet ammonium deposition coefficient.
      DOUBLE PRECISION, INTENT(IN) :: CLITFR  !! Fraction of initial organic carbon assigned to litter.
      DOUBLE PRECISION, INTENT(IN) :: CNRBIO  !! Biomass carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: CNRHUM  !! Humus carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: CNRLIT  !! Litter carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: FE  !! Efficiency fraction for organic carbon turnover.
      DOUBLE PRECISION, INTENT(IN) :: FH  !! Humification fraction.
      DOUBLE PRECISION, INTENT(IN) :: GNN  !! Nonlinear ammonium adsorption exponent.
      DOUBLE PRECISION, INTENT(IN) :: KPLAMM  !! First-order ammonium plant-uptake limit.
      DOUBLE PRECISION, INTENT(IN) :: KPLNIT  !! First-order nitrate plant-uptake limit.
      DOUBLE PRECISION, INTENT(IN) :: KUAMM  !! First-order ammonium immobilisation limit.
      DOUBLE PRECISION, INTENT(IN) :: KUNIT  !! First-order nitrate immobilisation limit.
      DOUBLE PRECISION, INTENT(IN) :: MNCREF  !! Reference nitrogen concentration.
      DOUBLE PRECISION, INTENT(IN) :: NITDDR  !! Dry nitrate deposition rate.
      DOUBLE PRECISION, INTENT(IN) :: NITWDR  !! Wet nitrate deposition coefficient.
      DOUBLE PRECISION, INTENT(IN) :: Q10M  !! Q10 coefficient for mineralisation.
      DOUBLE PRECISION, INTENT(IN) :: Q10N  !! Q10 coefficient for nitrification.
      LOGICAL, INTENT(IN) :: ISICCD  !! True when initial carbon uses decay-function input.
      LOGICAL, INTENT(IN) :: ISIAMD  !! True when initial ammonium uses decay-function input.

      ! Arguments tested by ALCHK/ALCHKI (Strict INTENT(INOUT) to satisfy dummy arguments)
      INTEGER, INTENT(INOUT) :: CELEM(NLF+1:NEL)  !! Initial-carbon category by element.
      INTEGER, INTENT(INOUT) :: KD1ELM(NLF+1:NEL)  !! KD1 denitrification category by element.
      INTEGER, INTENT(INOUT) :: KD2ELM(NLF+1:NEL)  !! KD2 denitrification category by element.
      INTEGER, INTENT(INOUT) :: KHELEM(NLF+1:NEL)  !! Humus decomposition category by element.
      INTEGER, INTENT(INOUT) :: KLELEM(NLF+1:NEL)  !! Litter decomposition category by element.
      INTEGER, INTENT(INOUT) :: KMELEM(NLF+1:NEL)  !! Manure decomposition category by element.
      INTEGER, INTENT(INOUT) :: KNELEM(NLF+1:NEL)  !! Nitrification category by element.
      INTEGER, INTENT(INOUT) :: KVELEM(NLF+1:NEL)  !! Volatilisation category by element.
      INTEGER, INTENT(INOUT) :: NAELEM(NLF+1:NEL)  !! Initial-ammonium category by element.
      DOUBLE PRECISION, INTENT(INOUT) :: CCONC(NMNEEE,NMNTEE)  !! Initial-carbon profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: CDPTH(NMNEEE,NMNTEE)  !! Initial-carbon profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: CTOTTP(NLF+1:NEL)  !! Top total-carbon value for decay initialisation.
      DOUBLE PRECISION, INTENT(INOUT) :: DAMHLF(NLF+1:NEL)  !! Ammonium decay half-depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: DCHLF(NLF+1:NEL)  !! Carbon decay half-depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: KD1CNC(NMNEEE,NMNTEE)  !! KD1 denitrification profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KD1DTH(NMNEEE,NMNTEE)  !! KD1 denitrification profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KD2CNC(NMNEEE,NMNTEE)  !! KD2 denitrification profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KD2DTH(NMNEEE,NMNTEE)  !! KD2 denitrification profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KDDSOL(NS)  !! Soil ammonium adsorption coefficient.
      DOUBLE PRECISION, INTENT(INOUT) :: KHCONC(NMNEEE,NMNTEE)  !! Humus decomposition profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KHDPTH(NMNEEE,NMNTEE)  !! Humus decomposition profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KLCONC(NMNEEE,NMNTEE)  !! Litter decomposition profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KLDPTH(NMNEEE,NMNTEE)  !! Litter decomposition profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KMCONC(NMNEEE,NMNTEE)  !! Manure decomposition profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KMDPTH(NMNEEE,NMNTEE)  !! Manure decomposition profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KNCONC(NMNEEE,NMNTEE)  !! Nitrification profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KNDPTH(NMNEEE,NMNTEE)  !! Nitrification profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KVCONC(NMNEEE,NMNTEE)  !! Volatilisation profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KVDPTH(NMNEEE,NMNTEE)  !! Volatilisation profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: NACONC(NMNEEE,NMNTEE)  !! Initial-ammonium profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: NADPTH(NMNEEE,NMNTEE)  !! Initial-ammonium profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: NAMTOP(NLF+1:NEL)  !! Top ammonium value for decay initialisation.

      ! Workspace arguments
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)  !! Logical workspace for element checks.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2, WARN = 3
      INTEGER :: ICOL1, NELMTY, NERR, NTAB

      ! Safe scalar passing arrays
      INTEGER :: IDUMS(1), IDUMO(1)
      DOUBLE PRECISION :: PREVDP_ARR(1), DUMS_ARR(1)

      INTEGER, PARAMETER :: IZERO_ARR(1) = [0]
      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      INTEGER, PARAMETER :: IUNDEF = 0

   !-------------------------------------------------------------------*

   ! 0. preliminaries
   ! ----------------
      NERR = 0
      ICOL1 = NLF + 1

   ! 1. spatially constant decomposition parameters
   ! ---------------------------
   ! kuamm,kplamm
      DUMS_ARR(1) = MIN(KUAMM, KPLAMM)
      CALL ALCHK(ERR, 3050, MNPR, 1, 1, IUNDEF, IUNDEF, '[ kuamm,kplamm ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! kunit,kplnit
      DUMS_ARR(1) = MIN(KUNIT, KPLNIT)
      CALL ALCHK(ERR, 3050, MNPR, 1, 1, IUNDEF, IUNDEF, '[ kunit,kplnit ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! 2. other parameters
   ! -------------------
   ! fe, fh
      DUMS_ARR(1) = MIN(FE, FH)
      CALL ALCHK(ERR, 3055, MNPR, 1, 1, IUNDEF, IUNDEF, '[ fe,fh ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)
      DUMS_ARR(1) = MAX(FE, FH)
      CALL ALCHK(ERR, 3055, MNPR, 1, 1, IUNDEF, IUNDEF, '[ fe,fh ]', 'LE', ONE_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! cnrbio,cnrhum
      DUMS_ARR(1) = MIN(CNRBIO, CNRHUM)
      CALL ALCHK(ERR, 3056, MNPR, 1, 1, IUNDEF, IUNDEF, '[ cnrbio,cnrhum ]', 'GT', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! q10m, q10n
      DUMS_ARR(1) = MIN(Q10M, Q10N)
      CALL ALCHK(ERR, 3057, MNPR, 1, 1, IUNDEF, IUNDEF, '[ q10m, q10n ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! ammddr, ammwdr
      DUMS_ARR(1) = MIN(AMMDDR, AMMWDR)
      CALL ALCHK(ERR, 3058, MNPR, 1, 1, IUNDEF, IUNDEF, '[ ammddr,ammwdr ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! nitddr, nitwdr
      DUMS_ARR(1) = MIN(NITDDR, NITWDR)
      CALL ALCHK(ERR, 3058, MNPR, 1, 1, IUNDEF, IUNDEF, '[ nitddr, nitwdr ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! mncref
      DUMS_ARR(1) = MNCREF
      CALL ALCHK(ERR, 3059, MNPR, 1, 1, IUNDEF, IUNDEF, 'mncref', 'GT', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! 3. initial concentrations
   ! -------------------------
   !    * carbon pool
   !    * -----------
      IF (ISICCD) THEN
         ! *ctottp
         CALL ALCHK(ERR, 3060, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ctottp(iel)', 'GE', ZERO_ARR, ZERO_VAL, CTOTTP, NERR, LDUM)
         ! *dchlf
         CALL ALCHK(ERR, 3061, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'dchlf(iel)', 'GT', ZERO_ARR, ZERO_VAL, DCHLF, NERR, LDUM)
      ELSE
         ! *celem
         CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'celem(iel)', 'GT', IZERO_ARR, CELEM, NERR, LDUM)

         ! *cdpth
         DO NELMTY = 1, NMN43E
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'cdpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, CDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
            DO NTAB = 2, NMN43T(NELMTY)
               PREVDP_ARR(1) = CDPTH(NELMTY, NTAB - 1)
               CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'cdpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, CDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
            END DO
         END DO

         ! *cconc
         DO NELMTY = 1, NMN43E
            DO NTAB = 1, NMN43T(NELMTY)
               CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'cconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, CCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
            END DO
         END DO
      END IF

   !  * carbon litter fraction and carbon/nitrogen ratio
   !  clitfr
      DUMS_ARR(1) = CLITFR
      CALL ALCHK(ERR, 3062, MNPR, 1, 1, IUNDEF, IUNDEF, 'clitfr', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)
      CALL ALCHK(ERR, 3062, MNPR, 1, 1, IUNDEF, IUNDEF, 'clitfr', 'LE', ONE_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)
   !  cnrlit
      DUMS_ARR(1) = CNRLIT
      CALL ALCHK(ERR, 3063, MNPR, 1, 1, IUNDEF, IUNDEF, 'cnrlit', 'GT', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)


   !    * ammonium pool
   !    * -------------
      IF (ISIAMD) THEN
         ! * namtop
         CALL ALCHK(ERR, 3060, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'namtop(iel)', 'GE', ZERO_ARR, ZERO_VAL, NAMTOP, NERR, LDUM)
         ! * damhlf
         CALL ALCHK(ERR, 3061, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'damhlf(iel)', 'GT', ZERO_ARR, ZERO_VAL, DAMHLF, NERR, LDUM)
      ELSE
         ! *naelem
         CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'naelem(iel)', 'GT', IZERO_ARR, NAELEM, NERR, LDUM)

         ! *nadpth
         DO NELMTY = 1, NMN53E
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'nadpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, NADPTH(NELMTY:NELMTY, 1), NERR, LDUM)
            DO NTAB = 2, NMN53T(NELMTY)
               PREVDP_ARR(1) = NADPTH(NELMTY, NTAB - 1)
               CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'nadpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, NADPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
            END DO
         END DO

         ! *naconc
         DO NELMTY = 1, NMN53E
            DO NTAB = 1, NMN53T(NELMTY)
               CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'naconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, NACONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
            END DO
         END DO
      END IF

   ! 4. spatially varying parameters
   ! -------------------------------

   ! 4.1 kh
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'khelem(iel)', 'GT', IZERO_ARR, KHELEM, NERR, LDUM)
      DO NELMTY = 1, NMN15E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'khdpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KHDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN15T(NELMTY)
            PREVDP_ARR(1) = KHDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'khdpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KHDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN15E
         DO NTAB = 1, NMN15T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'khconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KHCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

   ! 4.2 kl
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'klelem(iel)', 'GT', IZERO_ARR, KLELEM, NERR, LDUM)
      DO NELMTY = 1, NMN17E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kldpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KLDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN17T(NELMTY)
            PREVDP_ARR(1) = KLDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kldpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KLDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN17E
         DO NTAB = 1, NMN17T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'klconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KLCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

   ! 4.3 km
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'kmelem(iel)', 'GT', IZERO_ARR, KMELEM, NERR, LDUM)
      DO NELMTY = 1, NMN19E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kmdpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KMDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN19T(NELMTY)
            PREVDP_ARR(1) = KMDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kmdpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KMDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN19E
         DO NTAB = 1, NMN19T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kmconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KMCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

   ! 4.4 kn
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'knelem(iel)', 'GT', IZERO_ARR, KNELEM, NERR, LDUM)
      DO NELMTY = 1, NMN21E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kndpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KNDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN21T(NELMTY)
            PREVDP_ARR(1) = KNDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kndpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KNDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN21E
         DO NTAB = 1, NMN21T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'knconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KNCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

   ! 4.5 kv
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'kvelem(iel)', 'GT', IZERO_ARR, KVELEM, NERR, LDUM)
      DO NELMTY = 1, NMN23E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kvdpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KVDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN23T(NELMTY)
            PREVDP_ARR(1) = KVDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kvdpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KVDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN23E
         DO NTAB = 1, NMN23T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kvconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KVCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

   ! 4.6 kd1
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'kd1elm(iel)', 'GT', IZERO_ARR, KD1ELM, NERR, LDUM)
      DO NELMTY = 1, NMN25E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kd1dth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KD1DTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN25T(NELMTY)
            PREVDP_ARR(1) = KD1DTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kd1dth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KD1DTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN25E
         DO NTAB = 1, NMN25T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kd1cnc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KD1CNC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

   ! 4.7 kd2
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'kd2elm(iel)', 'GT', IZERO_ARR, KD2ELM, NERR, LDUM)
      DO NELMTY = 1, NMN27E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kd2dth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KD2DTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN27T(NELMTY)
            PREVDP_ARR(1) = KD2DTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kd2dth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KD2DTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN27E
         DO NTAB = 1, NMN27T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kd2cnc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KD2CNC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

   ! 5. ammonium adsorption parameters
   ! ---------------------------------
   !    * kddsol
      CALL ALCHK(ERR, 3048, MNPR, 1, NS, IUNDEF, IUNDEF, 'kddsol(ns)', 'GE', ZERO_ARR, ZERO_VAL, KDDSOL, NERR, LDUM)

   ! 6. bottom cell for nitrogen transformations
   ! -------------------------------------------
   !    * nbotce
      IDUMO(1) = NCETOP
      IDUMS(1) = NBOTCE
      CALL ALCHKI(ERR, 3049, MNPR, 1, 1, IUNDEF, IUNDEF, 'nbotce', 'LT', IDUMO, IDUMS, NERR, LDUM)

   ! 7. epilogue
   ! -----------
      IF (NERR > 0) THEN
         CALL ERROR(FATAL, 3012, MNPR, 0, 0, 'error(s) detected whilst checking the static input data')
      END IF

   END SUBROUTINE MNERR2

!> @brief Checks time-dependent MN inputs and updated state variables.
!>
!> `mnerr3` validates the dynamic CM-MN interface over active land-column cells
!> `NCOLMB(element):NCETOP` for elements `NLF+1:NEL`.
!>
!> | Group | Checks |
!> | --- | --- |
!> | Time | `DTUZ > 0`. On the first call only, `UZNOW >= 0`; the later-call monotonic-time check is present in comments but not active. |
!> | Nitrate concentrations | Dynamic-region concentration `CCCC` and dead-space concentration `SSSS` are non-negative. |
!> | Organic pools | Updated humus carbon, litter carbon, manure carbon, litter nitrogen, and manure nitrogen pools are non-negative. |
!> | Ammonium pool | Updated ammonium concentration `NAMM1` is non-negative. |
!> | Soil water and uptake | Current and previous soil-water contents satisfy `0 < VSTHE <= 1` and `0 < VSTHEO <= 1`; plant uptake `PLUP >= 0`. |
!> | Rainfall input | Net precipitation/effective rainfall `PNETTO >= 0` for land-column elements. |
!>
!> Detailed failures use errors `3065`-`3072`; any failure is followed by fatal
!> summary error `3013`.
   SUBROUTINE MNERR3(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, UZNOW, CCCC, &
                     PNETTO, SSSS, VSTHE, VSTHEO, LDUM, LDUM2)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column checks.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)  !! Dynamic-region nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)  !! Dead-space nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: VSTHEO(NEL, NCETOP + 1)  !! Previous volumetric water content.

      ! Arguments tested directly by ALCHK (Must be INTENT(INOUT) to satisfy dummy arguments)
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NELEE)  !! Net precipitation/effective rainfall by element.

      ! Workspace arguments (INTENT(INOUT) because they act as scratch space)
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)  !! Logical workspace for element checks.
      LOGICAL, INTENT(INOUT) :: LDUM2(LLEE)  !! Logical workspace for cell checks.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: ICOL1, IEL, NCEBOT, NERR, NCE
      DOUBLE PRECISION :: DUMMY4(NCETOP, NEL)
      DOUBLE PRECISION :: DUMS_ARR(1)

      ! Protected static state variables
      INTEGER, SAVE :: PASS = 0
      DOUBLE PRECISION, SAVE :: UZPREV(1) = [0.0D0]

      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0], THIRTY_ARR(1) = [30.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      INTEGER, PARAMETER :: IUNDEF = 0

   !-------------------------------------------------------------------*

   ! 0. preliminaries
   ! ----------------
      NERR = 0
      ICOL1 = NLF + 1
      PASS = PASS + 1

   ! 1. variables
   ! ------------

   ! dtuz
      DUMS_ARR(1) = DTUZ
      CALL ALCHK(ERR, 3065, MNPR, 1, 1, IUNDEF, IUNDEF, 'dtuz', 'GT', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

   ! uznow
      IF (PASS == 1) THEN
         DUMS_ARR(1) = UZNOW
         CALL ALCHK(ERR, 3066, MNPR, 1, 1, IUNDEF, IUNDEF, 'uznow', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)
         UZPREV(1) = UZNOW
      ELSE
         ! temporarily remove this sb 240925 as it is not compiling
         ! DUMS_ARR(1) = UZNOW
         ! CALL ALCHK(ERR, 3066, MNPR, 1, 1, IUNDEF, IUNDEF, 'uznow', 'gt', UZPREV, ZERO_VAL, DUMS_ARR, NERR, LDUM)
         UZPREV(1) = UZNOW
      END IF

   ! 2. nitrate concentrations
   ! -------------------------

   ! cccc, ssss
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = CCCC(IEL, NCE)
         END DO
      END DO

      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3067, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'cccc[iel,ncl]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = SSSS(IEL, NCE)
         END DO
      END DO

      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3067, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'ssss[iel,ncl]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

   ! 3. organic and inorganic pools
   ! ------------------------------

      ! chum1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = CHUM1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'chum1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! clit1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = CLIT1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'clit1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! cman1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = CMAN1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'cman1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! nlit1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = NLIT1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'nlit1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! nman1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = NMAN1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'nman1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! namm1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = NAMM1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3069, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'namm1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

   ! 4. soil conditions
   ! ------------------

   ! vsthe
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = VSTHE(NCE, IEL)
         END DO
      END DO

      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3070, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'vsthe[ncl,iel]', 'GT', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
         CALL ALCHK(ERR, 3070, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'vsthe[ncl,iel]', 'LE', ONE_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

   ! vstheo
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = VSTHEO(IEL, NCE)
         END DO
      END DO

      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3070, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'vstheo[ncl,iel]', 'GT', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
         CALL ALCHK(ERR, 3070, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'vstheo[ncl,iel]', 'LE', ONE_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

   ! plup
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = PLUP(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3071, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'plup[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

   ! 5. envoironmental conditions
   ! ----------------------------

   ! pnetto
      CALL ALCHK(ERR, 3072, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'pnetto(iel)', 'GE', ZERO_ARR, ZERO_VAL, PNETTO(ICOL1:NEL), NERR, LDUM)

   ! 6. epilogue
   ! -----------
      IF (NERR > 0) THEN
         CALL ERROR(FATAL, 3013, MNPR, 0, 0, 'error(s) detected whilst checking the time dependent' // ' variables from cm -mn interface')
      END IF

   END SUBROUTINE MNERR3

!> @brief Checks time-varying fertiliser and organic addition data from [[mnred2]].
!>
!> `mnerr4` validates only the scheduled additions that are active for the
!> current timestep, over land-column elements `NLF+1:NEL`.
!>
!> | Active flag | Checked records | Bounds |
!> | --- | --- | --- |
!> | `ISADDN` | Total inorganic nitrogen `NTOT`, ammonium fraction `NAMFCT`, nitrogen banding depth `NDPTHB`. | `NTOT >= 0`, `0 <= NAMFCT <= 1`, `NDPTHB >= 0`. |
!> | `ISADDC` | Total carbon `CTOT`, carbon banding depth `CDPTHB`, litter fraction `CLTFCT`, manure fraction `CMNFCT`, litter C:N ratio `CNRAL`, manure C:N ratio `CNRAM`. | `CTOT >= 0`, `CDPTHB >= 0`, `CLTFCT >= 0`, `CMNFCT >= 0`, `CLTFCT+CMNFCT <= 1`; when `CTOT > 0`, both C:N ratios must be positive. |
!>
!> Detailed failures use errors `3080`-`3087`; any failure is followed by fatal
!> summary error `3014`.
   SUBROUTINE MNERR4(MNPR, NEL, NELEE, NLF, CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, ISADDC, ISADDN, &
                     DUMMY, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column checks.
      LOGICAL, INTENT(IN) :: ISADDC  !! True when a carbon-addition event is active.
      LOGICAL, INTENT(IN) :: ISADDN  !! True when a nitrogen-addition event is active.

      ! Arguments tested directly by ALCHK (Must be INTENT(INOUT) to satisfy dummy arguments)
      DOUBLE PRECISION, INTENT(INOUT) :: CDPTHB(NLF+1:NEL)  !! Carbon banding depth.
      DOUBLE PRECISION, INTENT(INOUT) :: CLTFCT(NLF+1:NEL)  !! Litter fraction of added carbon.
      DOUBLE PRECISION, INTENT(INOUT) :: CMNFCT(NLF+1:NEL)  !! Manure fraction of added carbon.
      DOUBLE PRECISION, INTENT(INOUT) :: CNRAL(NLF+1:NEL)  !! Carbon-to-nitrogen ratio for added litter.
      DOUBLE PRECISION, INTENT(INOUT) :: CNRAM(NLF+1:NEL)  !! Carbon-to-nitrogen ratio for added manure.
      DOUBLE PRECISION, INTENT(INOUT) :: CTOT(NLF+1:NEL)  !! Total external carbon addition.
      DOUBLE PRECISION, INTENT(INOUT) :: NAMFCT(NLF+1:NEL)  !! Ammonium fraction of added inorganic nitrogen.
      DOUBLE PRECISION, INTENT(INOUT) :: NDPTHB(NLF+1:NEL)  !! Nitrogen banding depth.
      DOUBLE PRECISION, INTENT(INOUT) :: NTOT(NLF+1:NEL)  !! Total external inorganic nitrogen addition.

      ! Workspace arguments (INTENT(INOUT) because they act as scratch space)
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)  !! Floating-point workspace for range checks.
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)  !! Logical workspace for range checks.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: ICOL1, IEL, NERR

      DOUBLE PRECISION, PARAMETER :: ONE_ARR(1) = [1.0D0], ZERO_ARR(1) = [0.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      INTEGER, PARAMETER :: IUNDEF = 0

   !-------------------------------------------------------------------*

   ! 0. preliminaries
   ! ----------------

      NERR = 0
      ICOL1 = NLF + 1

   ! 1. inorganic fertilizer
   ! -----------------------
      IF (ISADDN) THEN
         ! ntot
         CALL ALCHK(ERR, 3080, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ntot(iel)', 'GE', ZERO_ARR, ZERO_VAL, NTOT, NERR, LDUM)

         ! namfct
         CALL ALCHK(ERR, 3081, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'namfct(iel)', 'GE', ZERO_ARR, ZERO_VAL, NAMFCT, NERR, LDUM)
         CALL ALCHK(ERR, 3081, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'namfct(iel)', 'LE', ONE_ARR, ZERO_VAL, NAMFCT, NERR, LDUM)

         ! ndpthb
         CALL ALCHK(ERR, 3082, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ndpthb(iel)', 'GE', ZERO_ARR, ZERO_VAL, NDPTHB, NERR, LDUM)
      END IF

   ! 2. organic fertilizer
   ! -----------------------
      IF (ISADDC) THEN
         ! ctot
         CALL ALCHK(ERR, 3083, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ctot(iel)', 'GE', ZERO_ARR, ZERO_VAL, CTOT, NERR, LDUM)

         ! cdpthb
         CALL ALCHK(ERR, 3084, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'cdpthb(iel)', 'GE', ZERO_ARR, ZERO_VAL, CDPTHB, NERR, LDUM)

         ! cltfct
         CALL ALCHK(ERR, 3085, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'cltfct(iel)', 'GE', ZERO_ARR, ZERO_VAL, CLTFCT, NERR, LDUM)

         ! cmnfct
         CALL ALCHK(ERR, 3085, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'cmnfct(iel)', 'GE', ZERO_ARR, ZERO_VAL, CMNFCT, NERR, LDUM)

         ! cmnfct + cltfct
         DO IEL = ICOL1, NEL
            DUMMY(IEL) = CLTFCT(IEL) + CMNFCT(IEL)
         END DO
         CALL ALCHK(ERR, 3086, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'cltfct+cmnfct(iel)', 'LE', ONE_ARR, ZERO_VAL, DUMMY(ICOL1:NEL), NERR, LDUM)

         ! cnral, cnram
         DO IEL = ICOL1, NEL
            IF (CTOT(IEL) > 0.0D0) THEN
               CALL ALCHK(ERR, 3087, MNPR, IEL, IEL, IUNDEF, IUNDEF, 'cnral(iel)', 'GT', ZERO_ARR, ZERO_VAL, CNRAL(IEL:IEL), NERR, LDUM)
               CALL ALCHK(ERR, 3087, MNPR, IEL, IEL, IUNDEF, IUNDEF, 'cnram(iel)', 'GT', ZERO_ARR, ZERO_VAL, CNRAM(IEL:IEL), NERR, LDUM)
            END IF
         END DO
      END IF

   ! 3. epilogue
   ! -----------
      IF (NERR > 0) THEN
         CALL ERROR(FATAL, 3014, MNPR, 0, 0, 'error(s) detected whilst checking the time dependent' // ' fertilizer input variables')
      END IF

   END SUBROUTINE MNERR4

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
            CHUMH = (CHUM(NELM, NCL) + CHUM1(NELM, NCL)) / 2.0D0
            CLITH = (CLIT(NELM, NCL) + CLIT1(NELM, NCL)) / 2.0D0
            CMANH = (CMAN(NELM, NCL) + CMAN1(NELM, NCL)) / 2.0D0
            NLITH = (NLIT(NELM, NCL) + NLIT1(NELM, NCL)) / 2.0D0
            NMANH = (NMAN(NELM, NCL) + NMAN1(NELM, NCL)) / 2.0D0

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

            ERF = EMT(NELM, NCL) * EMPH(NELM, NCL)

            DUM = KLITTP * ERF * (NLITH - CLITH * (1.0D0 - FE) * FH / CNRHUM - CLITH * FE / CNRBIO)
            DUM1 = DUM + KHUM(NELM, NCL) * ERF * CHUMH * (1.0D0 / CNRHUM - FE / CNRBIO)

            GAM(NELM, NCL) = DUM1 + KMANTP * ERF * (NMANH - FE * CMANH / CNRBIO)

            ! * if potential immobilisation is greater than actual
            ! * immobilisation checks how much mineralisation has
            ! * compensated for the difference
            GAMTMP(NELM, NCL) = GAM(NELM, NCL)

            IF (ISIMTF(NELM, NCL)) THEN
               IF (GAM(NELM, NCL) * DTUZ >= IMDIFF(NELM, NCL)) THEN
                  GAM(NELM, NCL) = (GAM(NELM, NCL) * DTUZ - IMDIFF(NELM, NCL)) / DTUZ
                  IMDIFF(NELM, NCL) = 0.0D0
                  ISIMTF(NELM, NCL) = .FALSE.
               ELSE
                  IMDIFF(NELM, NCL) = IMDIFF(NELM, NCL) - GAM(NELM, NCL) * DTUZ
                  GAM(NELM, NCL) = 0.0D0
               END IF
            END IF

         END DO cell_loop
      END DO column_loop

   END SUBROUTINE MNGAM

!> @brief Initialises MN pools, parameters, and source/sink terms.
!>
!> `mninit` prepares the land-column MN state over `NLF+1:NEL` and
!> `NCOLMB(element):NCETOP`. It first clears immobilisation-deficit state
!> (`IMDIFF=0`, `ISIMTF=.false.`), then initialises carbon, ammonium, and
!> depth-varying process parameters.
!>
!> | Quantity | Mode | Implemented calculation |
!> | --- | --- | --- |
!> | Initial organic carbon | `ISICCD` true | Exponential profile \(C=C_{top}\exp(-0.693\,z/D_{1/2})\), using `CTOTTP` and `DCHLF`; `CLIT1=CLITFR*C`, `CHUM1=(1-CLITFR)*C`, `NLIT1=CLIT1/CNRLIT`, and manure pools start at zero. |
!> | Initial organic carbon | `ISICCD` false | Interpolate category/profile table `CELEM`, `CCONC`, `CDPTH` with `ALINTP`; split the interpolated total using `CLITFR`, derive `NLIT1`, and set manure pools to zero. |
!> | Initial ammonium | `ISIAMD` true | Exponential profile \(N_{amm}=NAMTOP\exp(-0.693\,z/DAMHLF)\). |
!> | Initial ammonium | `ISIAMD` false | Interpolate category/profile table `NAELEM`, `NACONC`, `NADPTH` with `ALINTP`. |
!> | Process parameters | Always table-based | Interpolate `KHUM`, `KLIT`, `KMAN`, `KNIT`, `KVOL`, `KD1`, and `KD2` from their category/profile tables with `ALINTP`. |
!>
!> The profile depth `z` starts at half the top-cell thickness and then advances
!> downward using adjacent `ZVSNOD` differences. After interpolation,
!> `ISBOTC` is true only if the configured `NBOTCE` is at or below every land
!> column bottom (`NBOTCE >= NCOLMB(element)` for all land elements), and the CM
!> source/sink arrays `SSS1` and `SSS2` are reset to zero.
   SUBROUTINE MNINIT(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, &
                     NMNEEE, NMNTEE, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, KMELEM, KNELEM, KVELEM, NAELEM, NCOLMB, NMN15T, NMN17T, NMN19T, NMN21T, &
                     NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, CLITFR, CNRLIT, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, DELTAZ, KD1CNC, KD1DTH, KD2CNC, &
                     KD2DTH, KHCONC, KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ZVSNOD, ISICCD, &
                     ISIAMD, SSS1, SSS2, ISBOTC)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: NBOTCE  !! Requested lower active cell for nitrogen transformations.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column updates.
      INTEGER, INTENT(IN) :: NMN15E  !! Number of humus category entries.
      INTEGER, INTENT(IN) :: NMN17E  !! Number of litter category entries.
      INTEGER, INTENT(IN) :: NMN19E  !! Number of manure category entries.
      INTEGER, INTENT(IN) :: NMN21E  !! Number of nitrification category entries.
      INTEGER, INTENT(IN) :: NMN23E  !! Number of volatilisation category entries.
      INTEGER, INTENT(IN) :: NMN25E  !! Number of KD1 denitrification category entries.
      INTEGER, INTENT(IN) :: NMN27E  !! Number of KD2 denitrification category entries.
      INTEGER, INTENT(IN) :: NMN43E  !! Number of initial-carbon category entries.
      INTEGER, INTENT(IN) :: NMN53E  !! Number of initial-ammonium category entries.
      INTEGER, INTENT(IN) :: NMNEEE  !! Maximum number of MN category entries.
      INTEGER, INTENT(IN) :: NMNTEE  !! Maximum number of MN table entries.
      INTEGER, INTENT(IN) :: CELEM(NLF+1:NEL)  !! Initial-carbon category by element.
      INTEGER, INTENT(IN) :: KD1ELM(NLF+1:NEL)  !! KD1 denitrification category by element.
      INTEGER, INTENT(IN) :: KD2ELM(NLF+1:NEL)  !! KD2 denitrification category by element.
      INTEGER, INTENT(IN) :: KHELEM(NLF+1:NEL)  !! Humus decomposition category by element.
      INTEGER, INTENT(IN) :: KLELEM(NLF+1:NEL)  !! Litter decomposition category by element.
      INTEGER, INTENT(IN) :: KMELEM(NLF+1:NEL)  !! Manure decomposition category by element.
      INTEGER, INTENT(IN) :: KNELEM(NLF+1:NEL)  !! Nitrification category by element.
      INTEGER, INTENT(IN) :: KVELEM(NLF+1:NEL)  !! Volatilisation category by element.
      INTEGER, INTENT(IN) :: NAELEM(NLF+1:NEL)  !! Initial-ammonium category by element.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(IN) :: NMN15T(NMNEEE)  !! Humus table length by category.
      INTEGER, INTENT(IN) :: NMN17T(NMNEEE)  !! Litter table length by category.
      INTEGER, INTENT(IN) :: NMN19T(NMNEEE)  !! Manure table length by category.
      INTEGER, INTENT(IN) :: NMN21T(NMNEEE)  !! Nitrification table length by category.
      INTEGER, INTENT(IN) :: NMN23T(NMNEEE)  !! Volatilisation table length by category.
      INTEGER, INTENT(IN) :: NMN25T(NMNEEE)  !! KD1 table length by category.
      INTEGER, INTENT(IN) :: NMN27T(NMNEEE)  !! KD2 table length by category.
      INTEGER, INTENT(IN) :: NMN43T(NMNEEE)  !! Initial-carbon table length by category.
      INTEGER, INTENT(IN) :: NMN53T(NMNEEE)  !! Initial-ammonium table length by category.

      DOUBLE PRECISION, INTENT(IN) :: CLITFR  !! Fraction of initial organic carbon assigned to litter.
      DOUBLE PRECISION, INTENT(IN) :: CNRLIT  !! Initial litter carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: CCONC(NMNEEE,NMNTEE)  !! Initial-carbon profile values.
      DOUBLE PRECISION, INTENT(IN) :: CDPTH(NMNEEE,NMNTEE)  !! Initial-carbon profile depths.
      DOUBLE PRECISION, INTENT(IN) :: CTOTTP(NLF+1:NEL)  !! Top total-carbon value for decay initialisation.
      DOUBLE PRECISION, INTENT(IN) :: DAMHLF(NLF+1:NEL)  !! Ammonium decay half-depth by element.
      DOUBLE PRECISION, INTENT(IN) :: DCHLF(NLF+1:NEL)  !! Carbon decay half-depth by element.
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE,NEL)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: KD1CNC(NMNEEE,NMNTEE)  !! KD1 denitrification profile values.
      DOUBLE PRECISION, INTENT(IN) :: KD1DTH(NMNEEE,NMNTEE)  !! KD1 denitrification profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KD2CNC(NMNEEE,NMNTEE)  !! KD2 denitrification profile values.
      DOUBLE PRECISION, INTENT(IN) :: KD2DTH(NMNEEE,NMNTEE)  !! KD2 denitrification profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KHCONC(NMNEEE,NMNTEE)  !! Humus decomposition profile values.
      DOUBLE PRECISION, INTENT(IN) :: KHDPTH(NMNEEE,NMNTEE)  !! Humus decomposition profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KLCONC(NMNEEE,NMNTEE)  !! Litter decomposition profile values.
      DOUBLE PRECISION, INTENT(IN) :: KLDPTH(NMNEEE,NMNTEE)  !! Litter decomposition profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KMCONC(NMNEEE,NMNTEE)  !! Manure decomposition profile values.
      DOUBLE PRECISION, INTENT(IN) :: KMDPTH(NMNEEE,NMNTEE)  !! Manure decomposition profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KNCONC(NMNEEE,NMNTEE)  !! Nitrification profile values.
      DOUBLE PRECISION, INTENT(IN) :: KNDPTH(NMNEEE,NMNTEE)  !! Nitrification profile depths.
      DOUBLE PRECISION, INTENT(IN) :: KVCONC(NMNEEE,NMNTEE)  !! Volatilisation profile values.
      DOUBLE PRECISION, INTENT(IN) :: KVDPTH(NMNEEE,NMNTEE)  !! Volatilisation profile depths.
      DOUBLE PRECISION, INTENT(IN) :: NACONC(NMNEEE,NMNTEE)  !! Initial-ammonium profile values.
      DOUBLE PRECISION, INTENT(IN) :: NADPTH(NMNEEE,NMNTEE)  !! Initial-ammonium profile depths.
      DOUBLE PRECISION, INTENT(IN) :: NAMTOP(NLF+1:NEL)  !! Top ammonium value for decay initialisation.
      DOUBLE PRECISION, INTENT(IN) :: ZVSNOD(LLEE,NEL)  !! Vertical node elevation/depth by cell and element.

      LOGICAL, INTENT(IN) :: ISICCD  !! True when initial carbon uses decay-function input.
      LOGICAL, INTENT(IN) :: ISIAMD  !! True when initial ammonium uses decay-function input.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP+1)  !! Dynamic-region CM source/sink array reset by this routine.
      DOUBLE PRECISION, INTENT(OUT) :: SSS2(NEL, NCETOP+1)  !! Dead-space CM source/sink array reset by this routine.
      LOGICAL, INTENT(OUT) :: ISBOTC  !! True when `NBOTCE` is valid for all land columns.

      ! Locals
      INTEGER :: NCL, NELM
      DOUBLE PRECISION :: CTOT, DEPTH

   !-------------------------------------------------------------------*

   ! Initialize control arrays
      init_loop: DO NELM = NLF + 1, NEL
         DO NCL = NCOLMB(NELM), NCETOP
            IMDIFF(NELM, NCL) = 0.0D0
            ISIMTF(NELM, NCL) = .FALSE.
         END DO
      END DO init_loop

   ! * calculation of the initial conc. in the carbon pools
   ! * ----------------------------------------------------
      IF (ISICCD) THEN
         ! * an exponential decay rate down the column is used
         decay_c_loop: DO NELM = NLF + 1, NEL
            DO NCL = NCETOP, NCOLMB(NELM), -1
               IF (NCL == NCETOP) THEN
                  DEPTH = DELTAZ(NCETOP, NELM) / 2.0D0
               ELSE
                  DEPTH = DEPTH + (ZVSNOD(NCL + 1, NELM) - ZVSNOD(NCL, NELM))
               END IF

               ! * concentration in the organic pools, the manure pool is set to 0
               CTOT = CTOTTP(NELM) * EXP(-0.693D0 * DEPTH / DCHLF(NELM))
               CLIT1(NELM, NCL) = CTOT * CLITFR
               CHUM1(NELM, NCL) = CTOT * (1.0D0 - CLITFR)
               NLIT1(NELM, NCL) = CLIT1(NELM, NCL) / CNRLIT
               CMAN1(NELM, NCL) = 0.0D0
               NMAN1(NELM, NCL) = 0.0D0
            END DO
         END DO decay_c_loop
      ELSE
         ! * typical columns are used with linear interpolation between table values
         CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN43E, NMNEEE, NMNTEE, CELEM, NCOLMB(NLF+1:NEL), NMN43T, &
                     CCONC, CDPTH, DELTAZ, ZVSNOD, DUMMY6)

         interp_c_loop: DO NELM = NLF + 1, NEL
            DO NCL = NCOLMB(NELM), NCETOP
               CLIT1(NELM, NCL) = CLITFR * DUMMY6(NELM, NCL)
               CHUM1(NELM, NCL) = (1.0D0 - CLITFR) * DUMMY6(NELM, NCL)
               CMAN1(NELM, NCL) = 0.0D0
               NLIT1(NELM, NCL) = CLIT1(NELM, NCL) / CNRLIT
               NMAN1(NELM, NCL) = 0.0D0
            END DO
         END DO interp_c_loop
      END IF

   ! * calculation of the initial conc. in the ammonium pool
   ! * ----------------------------------------------------
      IF (ISIAMD) THEN
         ! * exponential decay
         decay_n_loop: DO NELM = NLF + 1, NEL
            DO NCL = NCETOP, NCOLMB(NELM), -1
               IF (NCL == NCETOP) THEN
                  DEPTH = DELTAZ(NCETOP, NELM) / 2.0D0
               ELSE
                  DEPTH = DEPTH + (ZVSNOD(NCL + 1, NELM) - ZVSNOD(NCL, NELM))
               END IF
               NAMM1(NELM, NCL) = NAMTOP(NELM) * EXP(-0.693D0 * DEPTH / DAMHLF(NELM))
            END DO
         END DO decay_n_loop
      ELSE
         ! * typical columns are used with linear interpolation between table values
         CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN53E, NMNEEE, NMNTEE, NAELEM, NCOLMB(NLF+1:NEL), NMN53T, &
                     NACONC, NADPTH, DELTAZ, ZVSNOD, NAMM1)
      END IF

   ! * calculation of the initial values for the decomposition params
   ! * --------------------------------------------------------------

      ! * khum
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN15E, NMNEEE, NMNTEE, KHELEM, NCOLMB(NLF+1:NEL), NMN15T, &
                  KHCONC, KHDPTH, DELTAZ, ZVSNOD, KHUM)

      ! * klit
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN17E, NMNEEE, NMNTEE, KLELEM, NCOLMB(NLF+1:NEL), NMN17T, &
                  KLCONC, KLDPTH, DELTAZ, ZVSNOD, KLIT)

      ! * kman
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN19E, NMNEEE, NMNTEE, KMELEM, NCOLMB(NLF+1:NEL), NMN19T, &
                  KMCONC, KMDPTH, DELTAZ, ZVSNOD, KMAN)

      ! * knit
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN21E, NMNEEE, NMNTEE, KNELEM, NCOLMB(NLF+1:NEL), NMN21T, &
                  KNCONC, KNDPTH, DELTAZ, ZVSNOD, KNIT)

      ! * kvol
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN23E, NMNEEE, NMNTEE, KVELEM, NCOLMB(NLF+1:NEL), NMN23T, &
                  KVCONC, KVDPTH, DELTAZ, ZVSNOD, KVOL)

      ! * kd1
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN25E, NMNEEE, NMNTEE, KD1ELM, NCOLMB(NLF+1:NEL), NMN25T, &
                  KD1CNC, KD1DTH, DELTAZ, ZVSNOD, KD1)

      ! * kd2
      CALL ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NMN27E, NMNEEE, NMNTEE, KD2ELM, NCOLMB(NLF+1:NEL), NMN27T, &
                  KD2CNC, KD2DTH, DELTAZ, ZVSNOD, KD2)

   ! * calculation of whether the specified bottom cell is greater
   ! * than the bottom cell in any of the soil columns. if this is
   ! * the case isbotc is true
      ISBOTC = .TRUE.
      DO NELM = NLF + 1, NEL
         IF (NBOTCE < NCOLMB(NELM)) THEN
            ISBOTC = .FALSE.
         END IF
      END DO

   ! * set the source/sink terms to zero
      sink_zero_loop: DO NELM = NLF + 1, NEL
         DO NCL = NCOLMB(NELM), NCETOP
            SSS1(NELM, NCL) = 0.0D0
            SSS2(NELM, NCL) = 0.0D0
         END DO
      END DO sink_zero_loop

   END SUBROUTINE MNINIT

!> @brief Converts time-varying MN inputs into cell-based process rates.
!>
!> `mnint2` carries forward previous pool values, dimensionalises nitrate
!> concentrations, assigns mobile fractions, distributes mineral and organic
!> additions over the specified banding depth, and adds wet/dry deposition to
!> the top active cell.
!>
!> The time-varying inputs come from the manual's external carbon (`MNFC`) and
!> external inorganic nitrogen/fertilizer (`MNFN`) files. Effective rainfall is
!> converted from SHETRAN flow units to millimetres per second as
!> `Pnet_mm = 1000 * PNETTO`, and dimensionless nitrate concentrations are
!> dimensionalised using the `MN14` reference concentration:
!>
!> \[
!> N_d = C\,MNCREF,\qquad N_s = S\,MNCREF.
!> \]
!>
!> The mobile-water nitrate fraction `PPHI` is currently assigned a fixed value
!> of `0.500` in every active cell; the previous call to the `PHI` function is
!> still present only as a comment.
!>
!> For an inorganic nitrogen addition with total `NTOT`, ammonium fraction
!> `NAMFCT`, banding depth `NDPTHB`, cell thickness `\Delta z`, and timestep
!> `\Delta t`, the top-cell-only case (`NDPTHB = 0`) uses
!>
!> \[
!> N_{amm}^{add} = \frac{NTOT\,NAMFCT}{\Delta z_{top}\Delta t},\qquad
!> N_{nit}^{add} = \frac{NTOT(1-NAMFCT)}{\Delta z_{top}\Delta t}.
!> \]
!>
!> When `NDPTHB > 0`, cells fully inside the band use `NDPTHB` in place of
!> `\Delta z_{top}`. The cell cut by the banding depth is multiplied by
!> \(f = d_{overlap}/NDPTHB\) and divided by that cell's own `\Delta z`; cells
!> below the band receive zero addition.
!>
!> Organic carbon additions use the same banding logic with `CTOT`, `CDPTHB`,
!> `CLTFCT`, and `CMNFCT`:
!>
!> \[
!> C_{lit}^{add} = \frac{CTOT\,CLTFCT}{D\Delta t},\quad
!> C_{man}^{add} = \frac{CTOT\,CMNFCT}{D\Delta t},\quad
!> C_{hum}^{add} = \frac{CTOT(1-CLTFCT-CMNFCT)}{D\Delta t},
!> \]
!>
!> where `D` is the top-cell thickness, the banding depth, or the partially
!> overlapped cell thickness with the overlap fraction applied. If no organic
!> carbon is active for an element, `CNRALT` and `CNRAMN` are set to `999.0` and
!> the carbon-addition rates are zeroed. Dry and wet deposition are finally
!> added to the top cell as
!>
!> \[
!> N_{amm}^{dep} = \frac{AMMDDR + AMMWDR\,Pnet_{mm}}{\Delta z_{top}},\qquad
!> N_{nit}^{dep} = \frac{NITDDR + NITWDR\,Pnet_{mm}}{\Delta z_{top}}.
!> \]
   SUBROUTINE MNINT2(LLEE, NCETOP, NEL, NELEE, NLF, NLYREE, NCOLMB, NLYR, NLYRBT, NTSOIL, AMMDDR, AMMWDR, MNCREF, NITDDR, NITWDR, &
                     DELTAZ, DTUZ, CCCC, CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, &
                     PNETTO, SSSS, VSTHE, ISADDC, ISADDN, CNRALT, CNRAMN, DUMMY)

      IMPLICIT NONE

      ! Input arguments
      ! * stationary
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column updates.
      INTEGER, INTENT(IN) :: NLYREE  !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(IN) :: NLYR(NELEE)  !! Number of soil layers in each element.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE)  !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: AMMDDR  !! Dry ammonium deposition rate.
      DOUBLE PRECISION, INTENT(IN) :: AMMWDR  !! Wet ammonium deposition coefficient.
      DOUBLE PRECISION, INTENT(IN) :: MNCREF  !! Reference nitrogen concentration.
      DOUBLE PRECISION, INTENT(IN) :: NITDDR  !! Dry nitrate deposition rate.
      DOUBLE PRECISION, INTENT(IN) :: NITWDR  !! Wet nitrate deposition coefficient.
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.

      ! * time dependent
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)  !! Dynamic-region nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: CDPTHB(NLF + 1:NEL)  !! Carbon banding depth.
      DOUBLE PRECISION, INTENT(IN) :: CLTFCT(NLF + 1:NEL)  !! Litter fraction of added carbon.
      DOUBLE PRECISION, INTENT(IN) :: CMNFCT(NLF + 1:NEL)  !! Manure fraction of added carbon.
      DOUBLE PRECISION, INTENT(IN) :: CNRAL(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added litter.
      DOUBLE PRECISION, INTENT(IN) :: CNRAM(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added manure.
      DOUBLE PRECISION, INTENT(IN) :: CTOT(NLF + 1:NEL)  !! Total external carbon addition.
      DOUBLE PRECISION, INTENT(IN) :: NAMFCT(NLF + 1:NEL)  !! Ammonium fraction of added inorganic nitrogen.
      DOUBLE PRECISION, INTENT(IN) :: NDPTHB(NLF + 1:NEL)  !! Nitrogen banding depth.
      DOUBLE PRECISION, INTENT(IN) :: NTOT(NLF + 1:NEL)  !! Total external inorganic nitrogen addition.
      DOUBLE PRECISION, INTENT(IN) :: PNETTO(NELEE)  !! Net precipitation/effective rainfall by element.
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)  !! Dead-space nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      LOGICAL, INTENT(IN) :: ISADDC  !! True when a carbon-addition event is active.
      LOGICAL, INTENT(IN) :: ISADDN  !! True when a nitrogen-addition event is active.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CNRALT(NELEE)  !! Element litter C:N ratio for active additions.
      DOUBLE PRECISION, INTENT(OUT) :: CNRAMN(NELEE)  !! Element manure C:N ratio for active additions.

      ! Workspace
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)  !! Floating-point workspace.

      ! Locals etc.
      INTEGER :: JLYR, JSOIL, NCEBOT, NCE, NCL, NELM
      DOUBLE PRECISION :: FRACDP, KSPTOT

   !-------------------------------------------------------------------*

   ! 1. set old concentrations to new values
   ! ---------------------------------------
      col_init_loop: DO NELM = NLF + 1, NEL

         DO NCL = NCOLMB(NELM), NCETOP
            CMAN(NELM, NCL) = CMAN1(NELM, NCL)
            NMAN(NELM, NCL) = NMAN1(NELM, NCL)
            CLIT(NELM, NCL) = CLIT1(NELM, NCL)
            CHUM(NELM, NCL) = CHUM1(NELM, NCL)
            NLIT(NELM, NCL) = NLIT1(NELM, NCL)
            NAMM(NELM, NCL) = NAMM1(NELM, NCL)
         END DO

   ! 2. calculate the effective rain on the ground surface in mm s-1
   ! ----------------------------------------------------------------
         DUMMY(NELM) = PNETTO(NELM) * 1.0D3

   ! 3. convert nitrate concentrations from non dimensional units
   ! ------------------------------------------------------------
         DO NCL = NCOLMB(NELM), NCETOP
            NDNIT(NELM, NCL) = CCCC(NELM, NCL) * MNCREF
            NDSNT(NELM, NCL) = SSSS(NELM, NCL) * MNCREF
         END DO

   ! 4. calculation of the mobile fraction for every element in every cell
   ! ---------------------------------------------------------------------
         NCEBOT = NCOLMB(NELM)
         DO JLYR = 1, NLYR(NELM)
            JSOIL = NTSOIL(NELM, JLYR)
            DO NCL = MAX(NCEBOT, NLYRBT(NELM, JLYR)), NLYRBT(NELM, JLYR + 1) - 1
               ! sb 240925 set value to 0.5 (which is the value set in cmmod.f90 in function phi
               PPHI(NELM, NCL) = 0.500D0
            END DO
         END DO

      END DO col_init_loop

   ! 5. addition of nitrate and ammonium for each element in each cell
   ! -----------------------------------------------------------------
      IF (ISADDN) THEN
         col_nitrate_loop: DO NELM = NLF + 1, NEL

            IF (NTOT(NELM) > 0.0D0) THEN

               ! * there is no banding of the input and only the top cell
               ! * receives fertiliser
               IF (NDPTHB(NELM) == 0.0D0) THEN
                  NAAMM(NELM, NCETOP) = NTOT(NELM) * NAMFCT(NELM) / (DELTAZ(NCETOP, NELM) * DTUZ)
                  NANIT(NELM, NCETOP) = NTOT(NELM) * (1.0D0 - NAMFCT(NELM)) / (DELTAZ(NCETOP, NELM) * DTUZ)
                  DO NCE = NCOLMB(NELM), NCETOP - 1
                     NAAMM(NELM, NCE) = 0.0D0
                     NANIT(NELM, NCE) = 0.0D0
                  END DO

               ! * there is banding of the input
               ELSE
                  KSPTOT = 0.0D0
                  DO NCE = NCETOP, NCOLMB(NELM), -1
                     KSPTOT = KSPTOT + DELTAZ(NCE, NELM)
                     ! * the banding depth is to below this element
                     IF (KSPTOT <= NDPTHB(NELM)) THEN
                        NAAMM(NELM, NCE) = NTOT(NELM) * NAMFCT(NELM) / (NDPTHB(NELM) * DTUZ)
                        NANIT(NELM, NCE) = NTOT(NELM) * (1.0D0 - NAMFCT(NELM)) / (NDPTHB(NELM) * DTUZ)
                     ! * the banding depth is to within this element
                     ELSE IF ((KSPTOT - DELTAZ(NCE, NELM)) <= NDPTHB(NELM)) THEN
                        FRACDP = (NDPTHB(NELM) - KSPTOT + DELTAZ(NCE, NELM)) / NDPTHB(NELM)
                        NAAMM(NELM, NCE) = NTOT(NELM) * NAMFCT(NELM) * FRACDP / (DELTAZ(NCE, NELM) * DTUZ)
                        NANIT(NELM, NCE) = NTOT(NELM) * (1.0D0 - NAMFCT(NELM)) * FRACDP / (DELTAZ(NCE, NELM) * DTUZ)
                     ! * the depth of the element is below the banding depth
                     ELSE
                        NAAMM(NELM, NCE) = 0.0D0
                        NANIT(NELM, NCE) = 0.0D0
                     END IF
                  END DO
               END IF

            ELSE
               DO NCE = NCOLMB(NELM), NCETOP
                  NAAMM(NELM, NCE) = 0.0D0
                  NANIT(NELM, NCE) = 0.0D0
               END DO
            END IF
         END DO col_nitrate_loop
      ELSE
         zero_nitrate_loop: DO NELM = NLF + 1, NEL
            DO NCE = NCOLMB(NELM), NCETOP
               NAAMM(NELM, NCE) = 0.0D0
               NANIT(NELM, NCE) = 0.0D0
            END DO
         END DO zero_nitrate_loop
      END IF

   ! 6. addition of organic matter for each element in each cell
   ! -----------------------------------------------------------
      IF (ISADDC) THEN
         col_organic_loop: DO NELM = NLF + 1, NEL

            IF (CTOT(NELM) > 0.0D0) THEN
               CNRALT(NELM) = CNRAL(NELM)
               CNRAMN(NELM) = CNRAM(NELM)

               ! * there is no banding of the input and only the top cell receives fertiliser
               IF (CDPTHB(NELM) == 0.0D0) THEN
                  CALIT(NELM, NCETOP) = CTOT(NELM) * CLTFCT(NELM) / (DELTAZ(NCETOP, NELM) * DTUZ)
                  CAMAN(NELM, NCETOP) = CTOT(NELM) * CMNFCT(NELM) / (DELTAZ(NCETOP, NELM) * DTUZ)
                  CAHUM(NELM, NCETOP) = CTOT(NELM) * (1.0D0 - CLTFCT(NELM) - CMNFCT(NELM)) / (DELTAZ(NCETOP, NELM) * DTUZ)
                  DO NCE = NCOLMB(NELM), NCETOP - 1
                     CALIT(NELM, NCE) = 0.0D0
                     CAMAN(NELM, NCE) = 0.0D0
                     CAHUM(NELM, NCE) = 0.0D0
                  END DO

               ! * there is banding of the input
               ELSE
                  KSPTOT = 0.0D0
                  DO NCE = NCETOP, NCOLMB(NELM), -1
                     KSPTOT = KSPTOT + DELTAZ(NCE, NELM)
                     ! * the banding depth is to below this element
                     IF (KSPTOT <= CDPTHB(NELM)) THEN
                        CALIT(NELM, NCE) = CTOT(NELM) * CLTFCT(NELM) / (CDPTHB(NELM) * DTUZ)
                        CAMAN(NELM, NCE) = CTOT(NELM) * CMNFCT(NELM) / (CDPTHB(NELM) * DTUZ)
                        CAHUM(NELM, NCE) = CTOT(NELM) * (1.0D0 - CLTFCT(NELM) - CMNFCT(NELM)) / (CDPTHB(NELM) * DTUZ)
                     ! * the banding depth is to within this element
                     ELSE IF ((KSPTOT - DELTAZ(NCE, NELM)) <= CDPTHB(NELM)) THEN
                        FRACDP = (CDPTHB(NELM) - (KSPTOT - DELTAZ(NCE, NELM))) / CDPTHB(NELM)
                        CALIT(NELM, NCE) = CTOT(NELM) * CLTFCT(NELM) * FRACDP / (DELTAZ(NCE, NELM) * DTUZ)
                        CAMAN(NELM, NCE) = CTOT(NELM) * CMNFCT(NELM) * FRACDP / (DELTAZ(NCE, NELM) * DTUZ)
                        CAHUM(NELM, NCE) = CTOT(NELM) * (1.0D0 - CLTFCT(NELM) - CMNFCT(NELM)) * FRACDP / (DELTAZ(NCE, NELM) * DTUZ)
                     ! * the depth of the element is below the banding depth
                     ELSE
                        CALIT(NELM, NCE) = 0.0D0
                        CAMAN(NELM, NCE) = 0.0D0
                        CAHUM(NELM, NCE) = 0.0D0
                     END IF
                  END DO
               END IF

            ELSE
               ! * set to 999 to avoid divide by zero errors
               CNRALT(NELM) = 999.0D0
               CNRAMN(NELM) = 999.0D0
               DO NCE = NCOLMB(NELM), NCETOP
                  CALIT(NELM, NCE) = 0.0D0
                  CAMAN(NELM, NCE) = 0.0D0
                  CAHUM(NELM, NCE) = 0.0D0
               END DO
            END IF
         END DO col_organic_loop

      ELSE
         zero_organic_loop: DO NELM = NLF + 1, NEL
            ! * set to 999 to avoid divide by zero errors
            CNRALT(NELM) = 999.0D0
            CNRAMN(NELM) = 999.0D0
            DO NCE = NCOLMB(NELM), NCETOP
               CALIT(NELM, NCE) = 0.0D0
               CAMAN(NELM, NCE) = 0.0D0
               CAHUM(NELM, NCE) = 0.0D0
            END DO
         END DO zero_organic_loop
      END IF

   ! 7. addition of wet and dry deposition on fertilizer rate
   ! --------------------------------------------------------
      depo_loop: DO NELM = NLF + 1, NEL
         NAAMM(NELM, NCETOP) = NAAMM(NELM, NCETOP) + AMMDDR / DELTAZ(NCETOP, NELM) + AMMWDR * DUMMY(NELM) / DELTAZ(NCETOP, NELM)
         NANIT(NELM, NCETOP) = NANIT(NELM, NCETOP) + NITDDR / DELTAZ(NCETOP, NELM) + NITWDR * DUMMY(NELM) / DELTAZ(NCETOP, NELM)
      END DO depo_loop

   END SUBROUTINE MNINT2

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
   SUBROUTINE mnlthm (llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, fe, fh, dtuz, isbotc)

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
      PARAMETER (niters = 20, warn = 3)
      PARAMETER (errtol = 1.0d-12)

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
            cmanh = (cman(nelm, ncl) + cman1(nelm, ncl)) / 2.0d0

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

            erf = emt(nelm, ncl) * emph(nelm, ncl)

            ! * iteration loop to calculate the new carbon litter
            ! * and humus concentrations
            iteration_loop: DO ntime = 1, niters

               dum = klittp * erf * clith * (fe - 1.0d0) + fe * erf * khum(nelm, ncl) * chumh
               dum = dum + fe * erf * kmantp * cmanh + calit(nelm, ncl)
               clit1(nelm, ncl) = clit(nelm, ncl) + dtuz * dum

               ! * litter conc at timestep n +1/2 is calculated for use
               ! * in the new calculation of the humus
               clith = (clit1(nelm, ncl) + clit(nelm, ncl)) / 2.0d0

               dum = (1.0d0 - fe) * fh * klittp * erf * clith - khum(nelm, ncl) * erf * chumh + cahum(nelm, ncl)
               chum1(nelm, ncl) = chum(nelm, ncl) + dtuz * dum

               ! * humus conc. at timestep n+1/2 is calculated. this is
               ! * for use in the new calculation of the litter at the
               ! * next iteration
               chumh = (chum1(nelm, ncl) + chum(nelm, ncl)) / 2.0d0

               ! * relative error between iterations in both litter and
               ! * humus pools in order to check the iteration is converging.
               IF (clit1(nelm, ncl) /= 0.0d0) THEN
                  werr1 = (clit1(nelm, ncl) - clit1o) / clit1(nelm, ncl)
               ELSE IF (clit1o == 0.0d0) THEN
                  werr1 = 0.0d0
               ELSE
                  werr1 = 1.0d0
               END IF

               IF (chum1(nelm, ncl) /= 0.0d0) THEN
                  werr2 = (chum1(nelm, ncl) - chum1o) / chum1(nelm, ncl)
               ELSE IF (chum1o == 0.0d0) THEN
                  werr2 = 0.0d0
               ELSE
                  werr2 = 1.0d0
               END IF

               ! * square of the errors, in order to make them positive
               wer1sq = werr1 * werr1
               wer2sq = werr2 * werr2

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
               CALL ERROR(warn, 3016, mnpr, 0, 0, msg)
            END IF

         END DO layer_loop
      END DO

9000  FORMAT('iteration loop in mnlthm failed to converge with error = ', g15.7, g15.7)

   END SUBROUTINE mnlthm

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
   SUBROUTINE mnltn (llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, cnrbio, fe, fh, dtuz, cnralt, isbotc)

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
      PARAMETER (niters = 20, warn = 3)
      PARAMETER (errtol = 1.0d-12)

      !-------------------------------------------------------------------*

      DO nelm = nlf + 1, nel
         IF (isbotc) THEN
            nbotm = nbotce
         ELSE
            nbotm = ncolmb(nelm)
         END IF

         layer_loop: DO ncl = nbotm, ncetop

            ! * initialise local variables
            chumh = (chum(nelm, ncl) + chum1(nelm, ncl)) / 2.0d0
            clith = (clit(nelm, ncl) + clit1(nelm, ncl)) / 2.0d0
            cmanh = (cman(nelm, ncl) + cman1(nelm, ncl)) / 2.0d0
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

            erf = emt(nelm, ncl) * emph(nelm, ncl)

            ! * iteration loop to calculate the new nitrogen litter
            ! * concentrations
            iteration_loop: DO ntime = 1, niters

               dum = -klittp * erf * nlith + fe * klittp * erf * clith / cnrbio
               dum = dum + fe * khum(nelm, ncl) * erf * chumh / cnrbio + calit(nelm, ncl) / cnralt(nelm)
               dum = dum + fe * kmantp * erf * cmanh / cnrbio

               nlit1(nelm, ncl) = nlit(nelm, ncl) + dtuz * dum

               ! * litter conc at timestep n +1/2 is calculated for use
               ! * in the new calculation of the litter
               nlith = (nlit1(nelm, ncl) + nlit(nelm, ncl)) / 2.0d0

               ! * relative error between iterations to see if the
               ! * iteration is converging.
               IF (nlit1(nelm, ncl) /= 0.0d0) THEN
                  werr1 = (nlit1(nelm, ncl) - nlit1o) / nlit1(nelm, ncl)
               ELSE IF (nlit1o == 0.0d0) THEN
                  werr1 = 0.0d0
               ELSE
                  werr1 = 1.0d0
               END IF

               ! * square of the errors, in order to make them positive
               wer1sq = werr1 * werr1

               nlit1o = nlit1(nelm, ncl)

               ! * break out of loop if the error in the iteration
               ! * is less than the error tolerance
               IF (wer1sq < errtol) EXIT iteration_loop

            END DO iteration_loop

            ! * the do loop has continued to niters and has thus
            ! * failed to converge
            IF (ntime > niters) THEN
               WRITE (msg, 9000) wer1sq
               CALL ERROR(warn, 3017, mnpr, 0, 0, msg)
            END IF

         END DO layer_loop
      END DO

9000  FORMAT('iteration loop in mnltn failed to converge with error = ', g15.7)

   END SUBROUTINE mnltn

!> @brief Advances the explicitly initialised mineral-nitrogen component.
!>
!> [[mninitialise]] performs all static checks, reads the MND file, initialises
!> process state, and allocates the persistent timestep workspace. `MNMAIN`
!> therefore contains only the timestep update and performs no heap allocation.
!>
!> | Phase | Call order | Purpose |
!> | --- | --- | --- |
!> | Timestep input | [[mnerr3]] -> [[mnred2]] -> [[mnerr4]] -> [[mnint2]] | Check dynamic CM-MN state, read scheduled MNFC/MNFN additions, validate them, and convert concentrations/additions/deposition to cell-based rates. |
!> | Environment | [[mntemp]] -> [[mnemt]] -> [[mnent]] -> [[mnemph]] -> [[mnenph]] -> [[mnedth]] | Update soil temperature and temperature, matric-potential, and saturation response factors. |
!> | Carbon and nitrogen pools | [[mnman]] -> [[mnlthm]] -> [[mnltn]] -> [[mnco2]] -> [[mngam]] -> [[mnamm]] -> [[mnnit]] | Update manure, litter, humus, carbon dioxide production, mineralisation/immobilisation, ammonium, and nitrate source/sink terms. |
!> | Output | [[mnout]] | Write requested detailed MN diagnostics. |
!>
!> Static parameters read by [[mnred1]], including deposition rates, Q10 values,
!> reaction constants, `MNCREF`, and `ISBOTC`, are retained in `MN_CONFIG`.
   SUBROUTINE MNMAIN(MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NEL, NLF, NS, NV, NX, NY, ICMBK, &
                     ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, &
                     ZVSNOD, BEXBK, LINKNS, DTUZ, UZNOW, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, &
                     SSS1, SSS2)

      IMPLICIT NONE

      ! Input arguments
      ! * static
      INTEGER, INTENT(IN) :: MNFC  !! Scheduled carbon-addition input unit.
      INTEGER, INTENT(IN) :: MNFN  !! Scheduled nitrogen-addition input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: MNOUT1  !! Carbon budget output unit.
      INTEGER, INTENT(IN) :: MNOUT2  !! Nitrogen budget output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NV  !! Number of vegetation/meteorological entries.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)  !! Element number at each grid location.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE)  !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: D0  !! Reference diffusion/dispersion scale used by CM.
      DOUBLE PRECISION, INTENT(IN) :: TIH  !! Initial simulation time in hours.
      DOUBLE PRECISION, INTENT(IN) :: Z2  !! Vertical length scale used by CM and MN temperature diffusion.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)  !! True for north-south channel links.

      ! * varying
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)  !! Dynamic-region nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)  !! Dead-space nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: TA(NV)  !! Air temperature by vegetation/meteorological entry.
      DOUBLE PRECISION, INTENT(IN) :: VSPSI(NCETOP, NEL)  !! Matric potential/pressure head by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: VSTHEO(NEL, NCETOP + 1)  !! Previous volumetric water content.

      ! Input/Output arguments (Propagated up from MNERR1, MNERR3 requirements)
      INTEGER, INTENT(INOUT) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(INOUT) :: NLYR(NELEE)  !! Number of soil layers in each element.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NELEE)  !! Element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NELEE)  !! Element length.
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)  !! Soil porosity by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: ZVSNOD(LLEE, NEL)  !! Vertical node elevation/depth by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NELEE)  !! Net precipitation/effective rainfall by element.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP + 1)  !! Dynamic-region CM source/sink array.
      DOUBLE PRECISION, INTENT(OUT) :: SSS2(NEL, NCETOP + 1)  !! Dead-space CM source/sink array.

      LOGICAL :: ISADDC, ISADDN
      LOGICAL :: LDUM2(LLEE)

   !-------------------------------------------------------------------*

      IF (.NOT. MN_INITIALISED) ERROR STOP 'MNMAIN called before MNINITIALISE'

         ! * checks time varying input variables from cm - mn interface
      CALL MNERR3(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, UZNOW, CCCC, PNETTO, SSSS, VSTHE, VSTHEO, MN_WORK%LDUM, LDUM2)

         ! * reads time varying input data
      CALL MNRED2(MNFC, MNFN, MNPR, NEL, NELEE, NLF, NLFEE, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, DTUZ, TIH, UZNOW, BEXBK, LINKNS, &
                  MN_WORK%CDPTHB(NLF + 1:NEL), MN_WORK%CLTFCT(NLF + 1:NEL), MN_WORK%CMNFCT(NLF + 1:NEL), MN_WORK%CNRAL(NLF + 1:NEL), &
                  MN_WORK%CNRAM(NLF + 1:NEL), MN_WORK%CTOT(NLF + 1:NEL), MN_WORK%NAMFCT(NLF + 1:NEL), MN_WORK%NDPTHB(NLF + 1:NEL), &
                  MN_WORK%NTOT(NLF + 1:NEL), ISADDC, ISADDN, MN_WORK%IDUM, MN_WORK%DUMMY)

         ! * checks time dependent input data read in mnred2
      CALL MNERR4(MNPR, NEL, NELEE, NLF, MN_WORK%CDPTHB(NLF + 1:NEL), MN_WORK%CLTFCT(NLF + 1:NEL), &
                  MN_WORK%CMNFCT(NLF + 1:NEL), MN_WORK%CNRAL(NLF + 1:NEL), MN_WORK%CNRAM(NLF + 1:NEL), MN_WORK%CTOT(NLF + 1:NEL), &
                  MN_WORK%NAMFCT(NLF + 1:NEL), MN_WORK%NDPTHB(NLF + 1:NEL), MN_WORK%NTOT(NLF + 1:NEL), ISADDC, ISADDN, &
                  MN_WORK%DUMMY, MN_WORK%LDUM)

         ! * modifies data read in mnred2 into suitable units and form for the rest of the program
      CALL MNINT2(LLEE, NCETOP, NEL, NELEE, NLF, NLYREE, NCOLMB, NLYR, NLYRBT, NTSOIL, MN_CONFIG%AMMDDR, MN_CONFIG%AMMWDR, &
                  MN_CONFIG%MNCREF, MN_CONFIG%NITDDR, MN_CONFIG%NITWDR, DELTAZ, DTUZ, CCCC, MN_WORK%CDPTHB(NLF + 1:NEL), &
                  MN_WORK%CLTFCT(NLF + 1:NEL), MN_WORK%CMNFCT(NLF + 1:NEL), MN_WORK%CNRAL(NLF + 1:NEL), MN_WORK%CNRAM(NLF + 1:NEL), &
                  MN_WORK%CTOT(NLF + 1:NEL), MN_WORK%NAMFCT(NLF + 1:NEL), MN_WORK%NDPTHB(NLF + 1:NEL), MN_WORK%NTOT(NLF + 1:NEL), &
                  PNETTO, SSSS, VSTHE, ISADDC, ISADDN, MN_WORK%CNRALT, MN_WORK%CNRAMN, MN_WORK%DUMMY)

         ! * environmental reduction factors are calculated
      CALL MNTEMP(LLEE, NCETOP, NEL, NELEE, NLF, NV, NCOLMB, Z2, DELTAZ, ZVSNOD, DTUZ, TA)
      CALL MNEMT(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%Q10M, MN_CONFIG%ISBOTC, MN_CONFIG%ISQ10)
      CALL MNENT(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%Q10N, MN_CONFIG%ISBOTC, MN_CONFIG%ISQ10)
      CALL MNEMPH(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, VSPSI, MN_CONFIG%ISBOTC)
      CALL MNENPH(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, VSPSI, MN_CONFIG%ISBOTC)
      CALL MNEDTH(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NLYREE, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, VSTHE, VSPOR, &
                  MN_CONFIG%ISBOTC)

         ! * new concentration of carbon and nitrogen manure pools
      CALL MNMAN(LLEE, MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, MN_WORK%CNRAMN, MN_CONFIG%ISBOTC)

         ! * new concentration of carbon litter and humus pools
      CALL MNLTHM(LLEE, MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%FE, MN_CONFIG%FH, DTUZ, MN_CONFIG%ISBOTC)

         ! * new concentration of nitrogen litter pool
      CALL MNLTN(LLEE, MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%CNRBIO, MN_CONFIG%FE, MN_CONFIG%FH, DTUZ, &
                 MN_WORK%CNRALT, MN_CONFIG%ISBOTC)

         ! * carbon dioxide production
      CALL MNCO2(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%FE, MN_CONFIG%FH, MN_CONFIG%ISBOTC)

         ! * mineralization/immobilisation rate
      CALL MNGAM(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, MN_CONFIG%CNRHUM, MN_CONFIG%CNRBIO, MN_CONFIG%FE, &
                 MN_CONFIG%FH, DTUZ, MN_CONFIG%ISBOTC)

         ! * new concentration of ammonium
      CALL MNAMM(LLEE, MNPR, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NLYREE, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, MN_CONFIG%GNN, &
                 MN_CONFIG%KPLAMM, MN_CONFIG%KUAMM, MN_CONFIG%MNCREF, MN_CONFIG%KDDSOL, DTUZ, VSTHE, VSTHEO, MN_CONFIG%ISBOTC)

         ! * new nitrate concentration in dynamic and dead space regions
      CALL MNNIT(LLEE, MN_CONFIG%NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, D0, MN_CONFIG%KPLNIT, MN_CONFIG%KUNIT, MN_CONFIG%MNCREF, &
                 Z2, DTUZ, VSTHE, VSTHEO, MN_CONFIG%ISBOTC, SSS1, SSS2)

         ! * extra output that may be required that is printed in this subroutine
      CALL MNOUT(MNOUT1, MNOUT2, MN_CONFIG%NBOTCE, NCETOP, NEL, NLF, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, MN_CONFIG%CNRHUM, &
                 MN_CONFIG%GNN, MN_CONFIG%MNCREF, DELTAZ, MN_CONFIG%KDDSOL, PPHI, DTUZ, UZNOW, DXQQ, DYQQ, MN_WORK%CNRALT, &
                 MN_WORK%CNRAMN, VSTHE, VSTHEO, MN_CONFIG%ISBOTC)

   END SUBROUTINE MNMAIN

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
   SUBROUTINE mnman (llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, dtuz, cnramn, isbotc)

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
      PARAMETER (niters = 20, warn = 3)
      PARAMETER (errtol = 1.0d-12)

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

            erf = emt(nelm, ncl) * emph(nelm, ncl)

            ! * iteration loop to calculate the new manure concentrations
            iteration_loop: DO ntime = 1, niters

               dum = -kmantp * erf * cmanh + caman(nelm, ncl)
               cman1(nelm, ncl) = cman(nelm, ncl) + dtuz * dum

               dum = -kmantp * erf * nmanh + caman(nelm, ncl) / cnramn(nelm)
               nman1(nelm, ncl) = nman(nelm, ncl) + dtuz * dum

               ! * calculates the relative error in the iteration
               IF (cman1(nelm, ncl) /= 0.0d0) THEN
                  werr1 = (cman1(nelm, ncl) - cman1o) / cman1(nelm, ncl)
               ELSE IF (cman1o == 0.0d0) THEN
                  werr1 = 0.0d0
               ELSE
                  werr1 = 1.0d0
               END IF

               IF (nman1(nelm, ncl) /= 0.0d0) THEN
                  werr2 = (nman1(nelm, ncl) - nman1o) / nman1(nelm, ncl)
               ELSE IF (nman1o == 0.0d0) THEN
                  werr2 = 0.0d0
               ELSE
                  werr2 = 1.0d0
               END IF

               ! * calculates the squared error, so that they are positive
               wer1sq = werr1 * werr1
               wer2sq = werr2 * werr2

               ! * updates the conc. at timestep n + 1/2 and the old conc.
               cmanh = (cman1(nelm, ncl) + cman(nelm, ncl)) / 2.0d0
               cman1o = cman1(nelm, ncl)
               nmanh = (nman1(nelm, ncl) + nman(nelm, ncl)) / 2.0d0
               nman1o = nman1(nelm, ncl)

               ! * break out of loop if error in both iterations is
               ! * less than the error tolerance
               IF ((wer1sq < errtol) .AND. (wer2sq < errtol)) EXIT iteration_loop

            END DO iteration_loop

            ! * the do loop has continued to niters and has thus
            ! * failed to converge
            IF (ntime > niters) THEN
               WRITE (msg, 9000) wer1sq, wer2sq
               CALL ERROR(warn, 3015, mnpr, 0, 0, msg)
            END IF

         END DO layer_loop
      END DO

9000  FORMAT('iteration loop in mnman failed to converge with error = ', g15.7, g15.7)

   END SUBROUTINE mnman

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
      double precision vsthe(ncetop,nel)  !! Current volumetric water content.
      double precision vstheo(nel,ncetop+1)  !! Previous volumetric water content.
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
      double precision sss1(nel,ncetop+1)  !! Dynamic-region CM source/sink array.
      double precision sss2(nel,ncetop+1)  !! Dead-space CM source/sink array.
      ! locals
      integer nbotm,ncl,nelm
      double precision dednt,dedsnt,dum1,dum2,imdnt,imdsnt,imrat
      double precision nammh,pldnt,pldsnt,s1,s2,sdnit,sdsnt,ttheth
      !
      !
      !-------------------------------------------------------------------*
      !
      do nelm = nlf+1,nel
         if (isbotc) then
            nbotm = nbotce
         else
            nbotm = ncolmb(nelm)
         endif
         do ncl = nbotm,ncetop
            !
            !           * initialisation of local variable
            ttheth = (vsthe(ncl,nelm) + vstheo(nelm,ncl))/2.0d0
            nammh = (namm(nelm,ncl) + namm1(nelm,ncl))/2.0d0
            !
            !
            !           * calculation of immobilisation rate of dynamic
            !           * region nitrate
            if (gam(nelm,ncl)>=0.0d0) then
               imdnt = 0.0d0
               imdsnt = 0.0d0
            else
               imdnt =min (-gam(nelm,ncl)-imamm(nelm,ncl),kunit*ndnit(nelm,ncl))
               imdsnt =min(-gam(nelm,ncl)-imamm(nelm,ncl),kunit*ndsnt(nelm,ncl))
            endif
            !
            !           * calculation of the denitrification rate
            dednt = ttheth *min (kd1(nelm,ncl)*emt(nelm,ncl)*edeth(nelm,ncl)*cdort(nelm,ncl),kd2(nelm,ncl)*ndnit(nelm &
               ,ncl))
            dedsnt = ttheth*min(kd1(nelm,ncl)*emt(nelm,ncl)*edeth(nelm,ncl)*cdort(nelm,ncl),kd2(nelm,ncl)*ndsnt(nelm, &
               ncl))
            denit(nelm,ncl) = pphi(nelm,ncl)*dednt+ (1-pphi(nelm,ncl))*dedsnt
            !
            !           * calculation of the plant uptake rate of dynamic
            !           * region nitrate
            if (ndnit(nelm,ncl)>0.0d0) then
               dum1 = plup(nelm,ncl)*ndnit(nelm,ncl)/(ndnit(nelm,ncl)+nammh)
            else
               dum1 = 0.0d0
            endif
            dum2 = ttheth*kplnit*ndnit(nelm,ncl)
            pldnt = min(dum1,dum2)
            !
            !           * calculation of the plant uptake rate of dead space
            !           * region nitrate
            if (ndsnt(nelm,ncl)>0.0d0) then
               dum1 = plup(nelm,ncl)*ndsnt(nelm,ncl)/(ndsnt(nelm,ncl)+nammh)
            else
               dum1 = 0.0d0
            endif
            dum2 = ttheth*kplnit*ndsnt(nelm,ncl)
            pldsnt = min(dum1,dum2)
            plnit(nelm,ncl) = pphi(nelm,ncl)*pldnt+ (1-pphi(nelm,ncl))*pldsnt
            !
            !
            !           * calculation of the source/sink term of dynamic region
            !           * nitrate at timestep n + 1
            sdnit = -pldnt+ntrf(nelm,ncl)-dednt-imdnt+nanit(nelm,ncl)
            s1 = pphi(nelm,ncl)*sdnit
            !
            !           * non dimensinal source/sink term
            sss1(nelm,ncl) =  - s1 * z2 * z2 / ( d0 * mncref )
            !
            !
            !           * calculation of the source/sink term for dead space region
            !           * nitrate at timestep n + 1
            sdsnt = - pldsnt + ntrf(nelm,ncl)- dedsnt - imdsnt + nanit(nelm,ncl)
            s2 = ( 1 - pphi(nelm,ncl))*sdsnt
            !
            !           * non dimensinal source/sink term
            sss2(nelm,ncl) = - s2 * z2 * z2 / ( d0 * mncref )
            !
            snit(nelm,ncl) = s1 + s2
            !
            !           * immobilisation rate
            imnit(nelm,ncl) =  pphi(nelm,ncl)*imdnt+ (1.0d0-pphi(nelm,ncl))*imdsnt
            !
            imrat = imamm (nelm,ncl)+ imnit(nelm,ncl)
            !
            !           * tests if the ponential immobilisation is greater than the
            !           * actual immobilisation
            if (-gam(nelm,ncl)>imrat) then
               isimtf(nelm,ncl)=.true.
               imdiff(nelm,ncl)= (-gam(nelm,ncl)-imrat)*dtuz
            endif
            !
         enddo
         !
         if (isbotc) then
            do ncl = ncolmb(nelm),nbotce-1
               sss1(nelm,ncl) = 0.0d0
               sss2(nelm,ncl) = 0.0d0
            enddo
         endif
         !
      enddo
   end subroutine mnnit

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

   !-------------------------------------------------------------------*

      PASS = PASS + 1

   ! * if it is the first pass the initial concentrations are printed
      IF (PASS == 1) THEN

         ALLOCATE(ADAMMT(NEL, NCETOP), ADDCT(NEL, NCETOP), ADNITT(NEL, NCETOP), ADORNT(NEL, NCETOP), CDOTOT(NEL, NCETOP), DETOT(NEL, NCETOP))
         ALLOCATE(GAMTOT(NEL, NCETOP), IMAMMT(NEL, NCETOP), IMNITT(NEL, NCETOP), MINTOT(NEL, NCETOP), NTRTOT(NEL, NCETOP), PLAMMT(NEL, NCETOP))
         ALLOCATE(PLNITT(NEL, NCETOP), STOT(NEL, NCETOP), VOLTOT(NEL, NCETOP))

         TOTC = 0.0D0
         TOTN = 0.0D0
         TAREA = 0.0D0

         DO NELM = NLF + 1, NEL
            IF (ISBOTC) THEN
               NBOTM = NBOTCE
            ELSE
               NBOTM = NCOLMB(NELM)
            END IF

            TAREA = TAREA + DXQQ(NELM) * DYQQ(NELM)
            NCEBOT = NBOTM

            DO JLYR = 1, NLYR(NELM)
               JSOIL = NTSOIL(NELM, JLYR)
               DO NCL = MAX(NCEBOT, NLYRBT(NELM, JLYR)), NLYRBT(NELM, JLYR + 1) - 1
                  ADAMMT(NELM, NCL)  = 0.0D0
                  ADDCT(NELM, NCL)   = 0.0D0
                  ADNITT(NELM, NCL)  = 0.0D0
                  ADORNT(NELM, NCL)  = 0.0D0
                  CDOTOT(NELM, NCL)  = 0.0D0
                  DETOT(NELM, NCL)   = 0.0D0
                  GAMTOT(NELM, NCL)  = 0.0D0
                  IMAMMT(NELM, NCL)  = 0.0D0
                  IMNITT(NELM, NCL)  = 0.0D0
                  MINTOT(NELM, NCL)  = 0.0D0
                  NTRTOT(NELM, NCL)  = 0.0D0
                  PLAMMT(NELM, NCL)  = 0.0D0
                  PLNITT(NELM, NCL)  = 0.0D0
                  STOT(NELM, NCL)    = 0.0D0
                  VOLTOT(NELM, NCL)  = 0.0D0

                  RETAMM = 1.0D0 + (KDDSOL(JSOIL) * (NAMM(NELM, NCL) / MNCREF)**(GNN - 1.0D0)) / VSTHEO(NELM, NCL)

                  TOTN = TOTN + DELTAZ(NCL, NELM) * DXQQ(NELM) * DYQQ(NELM) * (NAMM(NELM, NCL) * VSTHEO(NELM, NCL) * RETAMM + &
                         NLIT(NELM, NCL) + NMAN(NELM, NCL) + CHUM(NELM, NCL) / CNRHUM)

                  TOTC = TOTC + DELTAZ(NCL, NELM) * DXQQ(NELM) * DYQQ(NELM) * (CMAN(NELM, NCL) + CLIT(NELM, NCL) + CHUM(NELM, NCL))
               END DO
            END DO
         END DO

         MNSTRT = UZNOW

         WRITE(MNOUT2, '(/A30,G16.8)') 'initial nitrogen (kg n m-2) = ', TOTN / TAREA
         WRITE(MNOUT1, '(/A28,G16.8)') 'initial carbon (kg c m-2) = ', TOTC / TAREA
      END IF

   ! Main simulation timestep updates
      DO NELM = NLF + 1, NEL
         IF (ISBOTC) THEN
            NBOTM = NBOTCE
         ELSE
            NBOTM = NCOLMB(NELM)
         END IF

         DO NCL = NBOTM, NCETOP
            ADAMMT(NELM, NCL) = ADAMMT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * NAAMM(NELM, NCL)
            ADDCT(NELM, NCL)  = ADDCT(NELM, NCL)  + DTUZ * DELTAZ(NCL, NELM) * (CAMAN(NELM, NCL) + CAHUM(NELM, NCL) + CALIT(NELM, NCL))
            ADNITT(NELM, NCL) = ADNITT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * NANIT(NELM, NCL)
            ADORNT(NELM, NCL) = ADORNT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * (CAMAN(NELM, NCL) / CNRAMN(NELM) + CAHUM(NELM, NCL) / CNRHUM + CALIT(NELM, NCL) / CNRALT(NELM))
            CDOTOT(NELM, NCL) = CDOTOT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * CDORT(NELM, NCL)
            DETOT(NELM, NCL)  = DETOT(NELM, NCL)  + DTUZ * DELTAZ(NCL, NELM) * DENIT(NELM, NCL)
            GAMTOT(NELM, NCL) = GAMTOT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * GAMTMP(NELM, NCL)
            IMAMMT(NELM, NCL) = IMAMMT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * IMAMM(NELM, NCL)
            IMNITT(NELM, NCL) = IMNITT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * IMNIT(NELM, NCL)
            MINTOT(NELM, NCL) = MINTOT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * MINER(NELM, NCL)
            NTRTOT(NELM, NCL) = NTRTOT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * NTRF(NELM, NCL)
            PLAMMT(NELM, NCL) = PLAMMT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * PLAMM(NELM, NCL)
            PLNITT(NELM, NCL) = PLNITT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * PLNIT(NELM, NCL)
            STOT(NELM, NCL)   = STOT(NELM, NCL)   + DTUZ * DELTAZ(NCL, NELM) * SNIT(NELM, NCL)
            VOLTOT(NELM, NCL) = VOLTOT(NELM, NCL) + DTUZ * DELTAZ(NCL, NELM) * VOL(NELM, NCL)
         END DO
      END DO

   ! Output reporting block
      IF (UZNOW >= HRPRNT * NPRNT + MNSTRT) THEN
         TOTADN = 0.0D0
         TOTADC = 0.0D0
         TOTLOS = 0.0D0
         TOTN   = 0.0D0
         TOTC   = 0.0D0
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

                  RETAMM = 1.0D0 + (KDDSOL(JSOIL) * (NAMM1(NELM, NCL) / MNCREF)**(GNN - 1.0D0)) / VSTHE(NCL, NELM)

                  ! * sum of concentrations over all the cells
                  TOTLOS = TOTLOS + DXQQ(NELM) * DYQQ(NELM) * (VOLTOT(NELM, NCL) + PLAMMT(NELM, NCL) + NTRTOT(NELM, NCL))
                  TOTADN = TOTADN + DXQQ(NELM) * DYQQ(NELM) * (ADORNT(NELM, NCL) + ADAMMT(NELM, NCL) + IMNITT(NELM, NCL))
                  TOTADC = TOTADC + DXQQ(NELM) * DYQQ(NELM) * ADDCT(NELM, NCL)

                  TOTN = TOTN + DELTAZ(NCL, NELM) * DXQQ(NELM) * DYQQ(NELM) * (NAMM1(NELM, NCL) * VSTHE(NCL, NELM) * RETAMM + &
                         NLIT1(NELM, NCL) + NMAN1(NELM, NCL) + CHUM1(NELM, NCL) / CNRHUM)

                  TOTC = TOTC + DELTAZ(NCL, NELM) * DXQQ(NELM) * DYQQ(NELM) * (CMAN1(NELM, NCL) + CLIT1(NELM, NCL) + CHUM1(NELM, NCL))
                  TOTCO2 = TOTCO2 + DXQQ(NELM) * DYQQ(NELM) * CDOTOT(NELM, NCL)
               END DO
            END DO
         END DO

         NPRNT = NPRNT + 1

         WRITE(MNOUT1, '(///A7,G12.5,A6)') 'time = ', UZNOW, ' hours'
         WRITE(MNOUT2, '(///A7,G12.5,A6)') 'time = ', UZNOW, ' hours'

         WRITE(MNOUT2, '(A28,G16.8)') 'total nitrogen (kg n m-2) = ', TOTN / TAREA
         WRITE(MNOUT2, '(A33,G16.8)') 'total nitrogen added (kg n m-2)= ', TOTADN / TAREA
         WRITE(MNOUT2, '(A32,G16.8)') 'total nitrogen lost (kg n m-2) = ', TOTLOS / TAREA
         WRITE(MNOUT1, '(A26,G16.8)') 'total carbon (kg c m-2) = ', TOTC / TAREA
         WRITE(MNOUT1, '(A32,G16.8)') 'total carbon added (kg c m-2) = ', TOTADC / TAREA
         WRITE(MNOUT1, '(A28,G16.8)') 'total co2 lost (kg c m-2) = ', TOTCO2 / TAREA
      END IF

   END SUBROUTINE MNOUT

!> @brief Calculates potential plant nitrogen uptake by rooted cell.
!>
!> Plant uptake is based on canopy leaf area, canopy-density correction,
!> changing plant biomass, rooting depth, and root density fractions. The
!> routine is adapted from the SHETRAN plant component and preserves its
!> simplified assumptions for mixed vegetation in a grid cell.
!>
!> The manual's plant-uptake file `MNPL` supplies a title (`MNP1`) and, for
!> each vegetation type, a canopy-density function table (`MNP10`/`MNP11`) as
!> pairs of density factor `CDI` and time `CDIT` in days from the simulation
!> start. The routine linearly interpolates this table at `UZNOW/24`; if the
!> current time is beyond the table, the canopy-density factor is set to 1.
!> [[mnplantinitialise]] reads this file, writes the title to `MNOUTPL`, closes
!> both units, and initialises retained plant-mixture and mass state. `MNPLANT`
!> then calculates potential uptake on every call after resetting `PLUP` over
!> `NCOLMB(element):NCETOP`.
!>
!> Important plant-index variables retained from the legacy MPL-based logic are:
!>
!> | Variable | Meaning |
!> |:---------|:--------|
!> | `NPLTEE` | Total number of plant types; normally set to the same value as `NVEE`. |
!> | `NPELEE` | Maximum number of plant types in one element; normally set to 2. |
!> | `NPLANT` | Plant slot number within the current element. |
!> | `JPLTY` | Actual vegetation/plant type represented by `NPLANT`. |
!>
!> For plant type \(p\) in element \(e\), the estimated above-ground plant mass
!> is
!>
!> \[
!> M_{e,p} =
!> \frac{CLAI_p\,DELONE_p\,CDI_p(t)}{CLAIMX_p}
!> PFONE_{e,p}\,DXQQ_e\,DYQQ_e\,RHOPL .
!> \]
!>
!> The potential nitrogen uptake demand is based on the positive mass-change
!> rate \(\dot{M}_{e,p}=(M_{e,p}^{new}-M_{e,p}^{old})/\Delta t\). Negative
!> mass change marks cropping and produces no uptake. For growing plants the
!> nitrogen fraction \(f_N\) is a legacy age function of time since crop
!> emergence:
!>
!> \[
!> f_N =
!> \begin{cases}
!> 0.022, & t_c < 360,\\
!> 0.017, & 360 \le t_c < 720,\\
!> 0.015, & 720 \le t_c < 1080,\\
!> 0.012, & t_c \ge 1080.
!> \end{cases}
!> \]
!>
!> The rooted-cell potential uptake added to `PLUP` is then
!>
!> \[
!> PLUP_{e,c} \mathrel{+}=
!> \frac{\dot{M}_{e,p}\,f_N\,RDF_{p,k}}
!>      {\Delta z_{e,c}\,DXQQ_e\,DYQQ_e},
!> \]
!>
!> where `k = NCETOP - c + 1` indexes the root-density fraction and uptake is
!> applied only from the bottom rooted cell `NCETOP - NRD(JPLTY)` to `NCETOP`.
!> The final nitrate/ammonium availability limits are applied later by
!> [[mnnit]] and [[mnamm]].
!>
!> @note The legacy comments describe this as reasonable for deciduous trees and
!> arable crops, but less suitable for permanent grassland where `CLAI` may be
!> held nearly constant in the ET data. The implementation also keeps several
!> MPL-era simplifications: hard-coded `CLAIMX = 2`, at most two plant types per
!> element, plant type 1 as every second type, a named linear-search
!> interpolation loop, and saved state across calls. `MNOUTPL` receives only the
!> input title before both plant units are closed; no timestep plant values are
!> written.
!> @endnote
!>
!> @warning The current table-read loop stores every vegetation type's `MNP11`
!> values in `CDI(NV,*)` and `CDIT(NV,*)`, rather than row `i`, and does not
!> verify that `NVALUE(i)` is at most the fixed limit `NVALEE=30`. The saved
!> `ISCROP` flags are not initialised before their first possible test. Also,
!> `NRBOT=NCETOP-NRD(JPLTY)` is included in the root loop, giving `NRD+1` cell
!> indices when the complete range is valid. These current behaviours can make
!> multi-vegetation uptake or crop-reset results undefined.
!> @endwarning
   SUBROUTINE MNPLANTINITIALISE(MNPL, MNOUTPL, NEL, NLF, NV, NVC, RHOPL, DELONE, DXQQ, DYQQ, PLAI, CLAI)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: MNPL, MNOUTPL, NEL, NLF, NV
      INTEGER, INTENT(IN) :: NVC(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: RHOPL, DELONE(NPLTEE), DXQQ(NELEE), DYQQ(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: PLAI(NV), CLAI(NV)

      INTEGER :: I, JPLTY, NDATA, NELM, NPLANT, NTB
      INTEGER :: IDUM(1)
      DOUBLE PRECISION :: DUMMY(MN_PLANT_NVALEE * 2)
      CHARACTER(LEN=200) :: CDUM(1)

      CALL ALRED2(0, MNPL, MNOUTPL, 'mnptin')
      CALL ALREDC(0, MNPL, MNOUTPL, ':MNP1', 1, 1, CDUM)
      WRITE (MNOUTPL, '(/1x,A/)') CDUM

      DO I = 1, NV
         CALL ALREDI(0, MNPL, MNOUTPL, ':MNP10', 1, 1, IDUM)
         MN_PLANT_STATE%NVALUE(I) = IDUM(1)
         NDATA = IDUM(1) * 2
         CALL ALREDF(0, MNPL, MNOUTPL, ':MNP11', NDATA, 1, DUMMY)

         DO NTB = 1, IDUM(1)
            MN_PLANT_STATE%CDI(NV, NTB) = DUMMY(2 * NTB - 1)
            MN_PLANT_STATE%CDIT(NV, NTB) = DUMMY(2 * NTB)
         END DO
      END DO

      CLOSE (MNPL)
      CLOSE (MNOUTPL)

      DO NELM = NLF + 1, NEL
         DO I = 1, NPLTEE
            MN_PLANT_STATE%CLAIMX(I) = 2.0D0
         END DO

         MN_PLANT_STATE%NPLTYP(NELM, 1) = NVC(NELM)
         MN_PLANT_STATE%PFONE(NELM, 1) = PLAI(MN_PLANT_STATE%NPLTYP(NELM, 1))

         IF (MN_PLANT_STATE%PFONE(NELM, 1) >= 0.99D0) THEN
            MN_PLANT_STATE%NPL(NELM) = 1
         ELSE
            MN_PLANT_STATE%PFONE(NELM, 2) = 1.0D0 - MN_PLANT_STATE%PFONE(NELM, 1)
            MN_PLANT_STATE%NPL(NELM) = 2
         END IF

         DO I = 1, NEL
            MN_PLANT_STATE%NPLTYP(I, 2) = 1
         END DO

         DO NPLANT = 1, MN_PLANT_STATE%NPL(NELM)
            JPLTY = MN_PLANT_STATE%NPLTYP(NELM, NPLANT)
            MN_PLANT_STATE%GMCPBB(NELM, NPLANT) = &
               CLAI(JPLTY) * DELONE(JPLTY) / MN_PLANT_STATE%CLAIMX(JPLTY)
            MN_PLANT_STATE%MASSB(NELM, NPLANT) = MN_PLANT_STATE%GMCPBB(NELM, NPLANT) * &
               MN_PLANT_STATE%PFONE(NELM, NPLANT) * DXQQ(NELM) * DYQQ(NELM) * RHOPL
            MN_PLANT_STATE%CROPTM(NELM, NPLANT) = 0.0D0
         END DO
      END DO
   END SUBROUTINE MNPLANTINITIALISE

   SUBROUTINE mnplant (ncetop, nel, nlf, nv, ncolmb, nrd, rhopl, delone, dxqq, dyqq, deltaz, rdf, dtuz, uznow, clai)

      IMPLICIT NONE

      ! input arguments
      INTEGER, INTENT(IN) :: ncetop  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: nel  !! Number of elements.
      INTEGER, INTENT(IN) :: nlf  !! Number of overland/channel links excluded from land-column uptake.
      INTEGER, INTENT(IN) :: nv  !! Number of vegetation types.
      INTEGER, INTENT(IN) :: ncolmb(nelee)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(IN) :: nrd(nv)  !! Rooting depth in cell counts by vegetation type.
      DOUBLE PRECISION, INTENT(IN) :: rhopl  !! Plant dry-matter density used by uptake calculation.
      DOUBLE PRECISION, INTENT(IN) :: delone(npltee)  !! Initial plant biomass/cover scaling by plant type.
      DOUBLE PRECISION, INTENT(IN) :: dxqq(nelee)  !! Element width.
      DOUBLE PRECISION, INTENT(IN) :: dyqq(nelee)  !! Element length.
      DOUBLE PRECISION, INTENT(IN) :: deltaz(llee, nel)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(IN) :: rdf(nv, llee)  !! Root density fraction by vegetation type and cell.

      !     * time dependent
      DOUBLE PRECISION, INTENT(IN) :: dtuz  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: uznow  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: clai(nv)  !! Current canopy leaf-area index by vegetation type.

      INTEGER :: jplty, nelm, nplant, nrbot
      INTEGER :: i, nce, ndum
      DOUBLE PRECISION :: cdfnc, chgmas, fn, massbo, tmsncr
      DOUBLE PRECISION :: dum, dum2

      !----------------------------------------------------------------------*

      DO nelm = nlf + 1, nel
         DO nce = ncolmb(nelm), ncetop
            plup(nelm, nce) = 0.0d0
         END DO
      END DO

      DO nelm = nlf + 1, nel
         DO nplant = 1, MN_PLANT_STATE%npl(nelm)
            jplty = MN_PLANT_STATE%npltyp(nelm, nplant)

            age_search_loop: DO i = 2, MN_PLANT_STATE%nvalue(jplty)
               IF ((uznow / 24.0d0) < MN_PLANT_STATE%cdit(jplty, i)) THEN
                  dum = (MN_PLANT_STATE%cdi(jplty, i) - MN_PLANT_STATE%cdi(jplty, i - 1)) / &
                        (MN_PLANT_STATE%cdit(jplty, i) - MN_PLANT_STATE%cdit(jplty, i - 1))
                  dum2 = uznow / 24.0d0 - MN_PLANT_STATE%cdit(jplty, i - 1)
                  cdfnc = MN_PLANT_STATE%cdi(jplty, i - 1) + dum * dum2
                  EXIT age_search_loop
               END IF
            END DO age_search_loop

            ! Use the full-density factor after the last table time.
            IF (i > MN_PLANT_STATE%nvalue(jplty)) cdfnc = 1.0d0

            nrbot = ncetop - nrd(jplty)
            MN_PLANT_STATE%gmcpbb(nelm, nplant) = clai(jplty) * delone(jplty) * cdfnc / MN_PLANT_STATE%claimx(jplty)
            massbo = MN_PLANT_STATE%massb(nelm, nplant)
            MN_PLANT_STATE%massb(nelm, nplant) = MN_PLANT_STATE%gmcpbb(nelm, nplant) * &
               MN_PLANT_STATE%pfone(nelm, nplant) * dxqq(nelm) * dyqq(nelm) * rhopl
            chgmas = (MN_PLANT_STATE%massb(nelm, nplant) - massbo) / dtuz

            IF (chgmas < 0.0d0) THEN
               MN_PLANT_STATE%iscrop(nelm, nplant) = .TRUE.
            ELSE IF (clai(jplty) > 0.0d0) THEN
               IF (MN_PLANT_STATE%iscrop(nelm, nplant)) THEN
                  MN_PLANT_STATE%croptm(nelm, nplant) = uznow
                  MN_PLANT_STATE%iscrop(nelm, nplant) = .FALSE.
               END IF

               tmsncr = uznow - MN_PLANT_STATE%croptm(nelm, nplant)

               IF (tmsncr < 360.0d0) THEN
                  fn = 0.022d0
               ELSE IF (tmsncr < 720.0d0) THEN
                  fn = 0.017d0
               ELSE IF (tmsncr < 1080.0d0) THEN
                  fn = 0.015d0
               ELSE
                  fn = 0.012d0
               END IF

               DO nce = nrbot, ncetop
                  ndum = ncetop - nce + 1
                  plup(nelm, nce) = plup(nelm, nce) + chgmas * fn * rdf(jplty, ndum) / &
                     (deltaz(nce, nelm) * dxqq(nelm) * dyqq(nelm))
               END DO
            END IF
         END DO
      END DO
   END SUBROUTINE mnplant

!> @brief Reads static mineral nitrogen input data.
!>
!> `mnred1` reads the MND file once during [[mninitialise]], echoes the
!> nitrate title to `MNPR`, and fills the static parameter arrays that are later
!> validated by [[mnerr2]] and interpolated by [[mninit]].
!>
!> | Records | Data read |
!> | --- | --- |
!> | `MN11`-`MN14` | Ammonium/nitrate immobilisation and plant-uptake constants, organic-matter fractions and C:N ratios, dry/wet deposition rates, and `MNCREF`. |
!> | `MN15`-`MN28` | Category assignments and depth/value tables for `KHUM`, `KLIT`, `KMAN`, `KNIT`, `KVOL`, `KD1`, and `KD2`. Each category count must be in `1:NMNEEE` and each table length in `1:MNMTEE`; failures are fatal errors `3090` and `3091`. |
!> | `MN30`-`MN31` | Soil ammonium adsorption factor `KDDSOL(soil)` and power `GNN`. |
!> | `MN35`-`MN35a` | Q10 temperature-response flag `ISQ10`; `Q10M` and `Q10N` are read only when `ISQ10` is true. |
!> | `MN40`-`MN46` | Initial-carbon mode. If `ISICCD` is true, read decay-profile inputs `CTOTTP` and `DCHLF`; otherwise read category/profile tables `CELEM`, `CCONC`, and `CDPTH`. `CLITFR` and `CNRLIT` are always read. |
!> | `MN50`-`MN54` | Initial-ammonium mode. If `ISIAMD` is true, read decay-profile inputs `NAMTOP` and `DAMHLF`; otherwise read category/profile tables `NAELEM`, `NACONC`, and `NADPTH`. |
!> | `MN60` | Bottom cell `NBOTCE`, below which nitrogen transformations are not considered when it is valid for all columns. |
!>
!> Spatial category and profile fields are read with `ALALLI`/`ALALLF`, using the
!> grid, bank, and neighbour maps passed from the frame setup. The routine calls
!> `ALRED2` both before and after reading the MND file.
!>
!> @warning `Q10M` and `Q10N` are not assigned when `ISQ10` is false, although
!> [[mnerr2]] unconditionally reads and checks both values. Their values are
!> therefore undefined on that current-code path.
!> @endwarning
   SUBROUTINE MNRED1(MND, MNPR, NEL, NELEE, NLF, NLFEE, NMNEEE, NMNTEE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, BEXBK, LINKNS, NBOTCE, &
                     NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, &
                     KMELEM, KNELEM, KVELEM, NAELEM, NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, AMMDDR, &
                     AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT, FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT, MNCREF, NITDDR, NITWDR, Q10M, &
                     Q10N, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KDDSOL, KHCONC, KHDPTH, KLCONC, KLDPTH, &
                     KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ISICCD, ISIAMD, ISQ10, IDUM, DUMMY)

      USE SGLOBAL, ONLY : nyee

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: MND  !! Static MND input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NMNEEE  !! Maximum number of MN category entries.
      INTEGER, INTENT(IN) :: NMNTEE  !! Maximum number of MN table entries.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)  !! Element number at each grid location.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)  !! True for north-south channel links.

      ! Output arguments
      INTEGER, INTENT(OUT) :: NBOTCE  !! Lowest cell included when bottom-cell truncation is active.
      INTEGER, INTENT(OUT) :: NMN15E  !! Number of humus category entries.
      INTEGER, INTENT(OUT) :: NMN17E  !! Number of litter category entries.
      INTEGER, INTENT(OUT) :: NMN19E  !! Number of manure category entries.
      INTEGER, INTENT(OUT) :: NMN21E  !! Number of nitrification category entries.
      INTEGER, INTENT(OUT) :: NMN23E  !! Number of volatilisation category entries.
      INTEGER, INTENT(OUT) :: NMN25E  !! Number of KD1 denitrification category entries.
      INTEGER, INTENT(OUT) :: NMN27E  !! Number of KD2 denitrification category entries.
      INTEGER, INTENT(OUT) :: NMN43E  !! Number of initial-carbon category entries.
      INTEGER, INTENT(OUT) :: NMN53E  !! Number of initial-ammonium category entries.
      INTEGER, INTENT(OUT) :: CELEM(NLF+1:NEL)  !! Initial-carbon category by element.
      INTEGER, INTENT(OUT) :: KD1ELM(NLF+1:NEL)  !! KD1 denitrification category by element.
      INTEGER, INTENT(OUT) :: KD2ELM(NLF+1:NEL)  !! KD2 denitrification category by element.
      INTEGER, INTENT(OUT) :: KHELEM(NLF+1:NEL)  !! Humus decomposition category by element.
      INTEGER, INTENT(OUT) :: KLELEM(NLF+1:NEL)  !! Litter decomposition category by element.
      INTEGER, INTENT(OUT) :: KMELEM(NLF+1:NEL)  !! Manure decomposition category by element.
      INTEGER, INTENT(OUT) :: KNELEM(NLF+1:NEL)  !! Nitrification category by element.
      INTEGER, INTENT(OUT) :: KVELEM(NLF+1:NEL)  !! Volatilisation category by element.
      INTEGER, INTENT(OUT) :: NAELEM(NLF+1:NEL)  !! Initial-ammonium category by element.
      INTEGER, INTENT(OUT) :: NMN15T(NMNEEE)  !! Humus table length by category.
      INTEGER, INTENT(OUT) :: NMN17T(NMNEEE)  !! Litter table length by category.
      INTEGER, INTENT(OUT) :: NMN19T(NMNEEE)  !! Manure table length by category.
      INTEGER, INTENT(OUT) :: NMN21T(NMNEEE)  !! Nitrification table length by category.
      INTEGER, INTENT(OUT) :: NMN23T(NMNEEE)  !! Volatilisation table length by category.
      INTEGER, INTENT(OUT) :: NMN25T(NMNEEE)  !! KD1 table length by category.
      INTEGER, INTENT(OUT) :: NMN27T(NMNEEE)  !! KD2 table length by category.
      INTEGER, INTENT(OUT) :: NMN43T(NMNEEE)  !! Initial-carbon table length by category.
      INTEGER, INTENT(OUT) :: NMN53T(NMNEEE)  !! Initial-ammonium table length by category.

      DOUBLE PRECISION, INTENT(OUT) :: AMMDDR  !! Dry ammonium deposition rate.
      DOUBLE PRECISION, INTENT(OUT) :: AMMWDR  !! Wet ammonium deposition coefficient.
      DOUBLE PRECISION, INTENT(OUT) :: CLITFR  !! Fraction of initial organic carbon assigned to litter.
      DOUBLE PRECISION, INTENT(OUT) :: CNRBIO  !! Biomass carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(OUT) :: CNRHUM  !! Humus carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(OUT) :: CNRLIT  !! Initial litter carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(OUT) :: FE  !! Efficiency fraction for organic carbon turnover.
      DOUBLE PRECISION, INTENT(OUT) :: FH  !! Humification fraction.
      DOUBLE PRECISION, INTENT(OUT) :: GNN  !! Nonlinear ammonium adsorption exponent.
      DOUBLE PRECISION, INTENT(OUT) :: KPLAMM  !! First-order ammonium plant-uptake limit.
      DOUBLE PRECISION, INTENT(OUT) :: KPLNIT  !! First-order nitrate plant-uptake limit.
      DOUBLE PRECISION, INTENT(OUT) :: KUAMM  !! First-order ammonium immobilisation limit.
      DOUBLE PRECISION, INTENT(OUT) :: KUNIT  !! First-order nitrate immobilisation limit.
      DOUBLE PRECISION, INTENT(OUT) :: MNCREF  !! Reference nitrogen concentration.
      DOUBLE PRECISION, INTENT(OUT) :: NITDDR  !! Dry nitrate deposition rate.
      DOUBLE PRECISION, INTENT(OUT) :: NITWDR  !! Wet nitrate deposition coefficient.
      DOUBLE PRECISION, INTENT(OUT) :: Q10M  !! Q10 coefficient for mineralisation.
      DOUBLE PRECISION, INTENT(OUT) :: Q10N  !! Q10 coefficient for nitrification.
      DOUBLE PRECISION, INTENT(OUT) :: CCONC(NMNEEE, NMNTEE)  !! Initial-carbon profile values.
      DOUBLE PRECISION, INTENT(OUT) :: CDPTH(NMNEEE, NMNTEE)  !! Initial-carbon profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: CTOTTP(NLF+1:NEL)  !! Top total-carbon value for decay initialisation.
      DOUBLE PRECISION, INTENT(OUT) :: DAMHLF(NLF+1:NEL)  !! Ammonium decay half-depth by element.
      DOUBLE PRECISION, INTENT(OUT) :: DCHLF(NLF+1:NEL)  !! Carbon decay half-depth by element.
      DOUBLE PRECISION, INTENT(OUT) :: KD1CNC(NMNEEE, NMNTEE)  !! KD1 denitrification profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KD1DTH(NMNEEE, NMNTEE)  !! KD1 denitrification profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KD2CNC(NMNEEE, NMNTEE)  !! KD2 denitrification profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KD2DTH(NMNEEE, NMNTEE)  !! KD2 denitrification profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KDDSOL(NS)  !! Soil ammonium adsorption coefficient.
      DOUBLE PRECISION, INTENT(OUT) :: KHCONC(NMNEEE, NMNTEE)  !! Humus decomposition profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KHDPTH(NMNEEE, NMNTEE)  !! Humus decomposition profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KLCONC(NMNEEE, NMNTEE)  !! Litter decomposition profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KLDPTH(NMNEEE, NMNTEE)  !! Litter decomposition profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KMCONC(NMNEEE, NMNTEE)  !! Manure decomposition profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KMDPTH(NMNEEE, NMNTEE)  !! Manure decomposition profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KNCONC(NMNEEE, NMNTEE)  !! Nitrification profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KNDPTH(NMNEEE, NMNTEE)  !! Nitrification profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: KVCONC(NMNEEE, NMNTEE)  !! Volatilisation profile values.
      DOUBLE PRECISION, INTENT(OUT) :: KVDPTH(NMNEEE, NMNTEE)  !! Volatilisation profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: NACONC(NMNEEE, NMNTEE)  !! Initial-ammonium profile values.
      DOUBLE PRECISION, INTENT(OUT) :: NADPTH(NMNEEE, NMNTEE)  !! Initial-ammonium profile depths.
      DOUBLE PRECISION, INTENT(OUT) :: NAMTOP(NLF+1:NEL)  !! Top ammonium value for decay initialisation.

      LOGICAL, INTENT(OUT) :: ISICCD  !! True when initial carbon uses decay-function input.
      LOGICAL, INTENT(OUT) :: ISIAMD  !! True when initial ammonium uses decay-function input.
      LOGICAL, INTENT(OUT) :: ISQ10  !! True when Q10 temperature response is selected.

      ! Workspace arguments (INTENT(INOUT) because they act as read buffers)
      INTEGER, INTENT(INOUT) :: IDUM(NELEE)  !! Integer workspace for spatial reads.
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)  !! Floating-point workspace for spatial reads.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1
      INTEGER :: NC, NCAT, NDATA, NTB
      INTEGER :: NMNT(1), IDUMS(1)
      CHARACTER(LEN=200) :: CDUM(1)
      LOGICAL :: LDUM(1)

   !-------------------------------------------------------------------*

   ! preliminaries
   ! -------------
   ! * check status of data file
      CALL ALRED2(0, MND, MNPR, 'MND')

   ! * print title for nitrate simulation
      CALL ALREDC(0, MND, MNPR, ':MN01', 1, 1, CDUM)
      WRITE(MNPR, '(/1X,A/)') CDUM(1)


   ! decomposition parameter rates
   ! -----------------------------
   ! * decomposition parameters for ammonium immobilisation,
   ! * plant uptake of ammonium,immobilisation of nitrate
   ! * and plant uptake of nitrate
      CALL ALREDF(0, MND, MNPR, ':MN11', 4, 1, DUMMY)
      KUAMM  = DUMMY(1)
      KPLAMM = DUMMY(2)
      KUNIT  = DUMMY(3)
      KPLNIT = DUMMY(4)

   ! further parameters
   ! ------------------
   ! * organic matter effeciency fraction and humification fraction
   ! * and carbon to nitrogen ratio in the biomass and humus
      CALL ALREDF(0, MND, MNPR, ':MN12', 4, 1, DUMMY)
      FE     = DUMMY(1)
      FH     = DUMMY(2)
      CNRBIO = DUMMY(3)
      CNRHUM = DUMMY(4)

   ! * dry and wet deposition rates of ammonium and nitrate
      CALL ALREDF(0, MND, MNPR, ':MN13', 4, 1, DUMMY)
      AMMDDR = DUMMY(1)
      AMMWDR = DUMMY(2)
      NITDDR = DUMMY(3)
      NITWDR = DUMMY(4)

   ! * reference contaminant concentration
      CALL ALREDF(0, MND, MNPR, ':MN14', 1, 1, DUMMY)
      MNCREF = DUMMY(1)


   ! spatially varying decomposition parameter rates
   ! -----------------------------------------------

   ! khum
   ! ----
   ! * find out how many typical element catagories
      CALL ALREDI(0, MND, MNPR, ':MN15a', 1, 1, IDUM)
      NMN15E = IDUM(1)
      IF ((NMN15E > NMNEEE) .OR. (NMN15E <= 0)) THEN
         CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn15 in mn data file')
      END IF

   ! * read the catagory type for each element into the element number
      CALL ALALLI(NMN15E, MND, MNPR, ':MN15b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KHELEM, IDUM)

   ! * table of values for each typical element
      DO NC = 1, NMN15E
         CALL ALREDI(0, MND, MNPR, ':MN16a', 1, 1, NMNT)
         NMN15T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn16a in mn data file')
         END IF

         NDATA = NMNT(1) * 2
         CALL ALREDF(0, MND, MNPR, ':MN16b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KHDPTH(NC, NTB) = DUMMY(2 * NTB - 1)
            KHCONC(NC, NTB) = DUMMY(2 * NTB)
         END DO
      END DO

   ! klit
   ! ----
      CALL ALREDI(0, MND, MNPR, ':MN17a', 1, 1, IDUM)
      NMN17E = IDUM(1)
      IF ((NMN17E > NMNEEE) .OR. (NMN17E <= 0)) THEN
         CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn17 in mn data file')
      END IF

      CALL ALALLI(NMN17E, MND, MNPR, ':MN17b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KLELEM, IDUM)

      DO NC = 1, NMN17E
         CALL ALREDI(0, MND, MNPR, ':MN18a', 1, 1, NMNT)
         NMN17T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn18a in mn data file')
         END IF

         NDATA = NMNT(1) * 2
         CALL ALREDF(0, MND, MNPR, ':MN18b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KLDPTH(NC, NTB) = DUMMY(2 * NTB - 1)
            KLCONC(NC, NTB) = DUMMY(2 * NTB)
         END DO
      END DO

   ! kman
   ! ----
      CALL ALREDI(0, MND, MNPR, ':MN19a', 1, 1, IDUM)
      NMN19E = IDUM(1)
      IF ((NMN19E > NMNEEE) .OR. (NMN19E <= 0)) THEN
         CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn19 in mn data file')
      END IF

      CALL ALALLI(NMN19E, MND, MNPR, ':MN19b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KMELEM, IDUM)

      DO NC = 1, NMN19E
         CALL ALREDI(0, MND, MNPR, ':MN20a', 1, 1, NMNT)
         NMN19T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn20a in mn data file')
         END IF

         NDATA = NMNT(1) * 2
         CALL ALREDF(0, MND, MNPR, ':MN20b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KMDPTH(NC, NTB) = DUMMY(2 * NTB - 1)
            KMCONC(NC, NTB) = DUMMY(2 * NTB)
         END DO
      END DO

   ! knit
   ! ----
      CALL ALREDI(0, MND, MNPR, ':MN21a', 1, 1, IDUM)
      NMN21E = IDUM(1)
      IF ((NMN21E > NMNEEE) .OR. (NMN21E <= 0)) THEN
         CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn21 in mn data file')
      END IF

      CALL ALALLI(NMN21E, MND, MNPR, ':MN21b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KNELEM, IDUM)

      DO NC = 1, NMN21E
         CALL ALREDI(0, MND, MNPR, ':MN22a', 1, 1, NMNT)
         NMN21T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn22a in mn data file')
         END IF

         NDATA = NMNT(1) * 2
         CALL ALREDF(0, MND, MNPR, ':MN22b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KNDPTH(NC, NTB) = DUMMY(2 * NTB - 1)
            KNCONC(NC, NTB) = DUMMY(2 * NTB)
         END DO
      END DO

   ! kvol
   ! ----
      CALL ALREDI(0, MND, MNPR, ':MN23a', 1, 1, IDUM)
      NMN23E = IDUM(1)
      IF ((NMN23E > NMNEEE) .OR. (NMN23E <= 0)) THEN
         CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn23 in mn data file')
      END IF

      CALL ALALLI(NMN23E, MND, MNPR, ':MN23b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KVELEM, IDUM)

      DO NC = 1, NMN23E
         CALL ALREDI(0, MND, MNPR, ':MN24a', 1, 1, NMNT)
         NMN23T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn24a in mn data file')
         END IF

         NDATA = NMNT(1) * 2
         CALL ALREDF(0, MND, MNPR, ':MN24b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KVDPTH(NC, NTB) = DUMMY(2 * NTB - 1)
            KVCONC(NC, NTB) = DUMMY(2 * NTB)
         END DO
      END DO

   ! kd1
   ! ----
      CALL ALREDI(0, MND, MNPR, ':MN25a', 1, 1, IDUM)
      NMN25E = IDUM(1)
      IF ((NMN25E > NMNEEE) .OR. (NMN25E <= 0)) THEN
         CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn25 in mn data file')
      END IF

      CALL ALALLI(NMN25E, MND, MNPR, ':MN25b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KD1ELM, IDUM)

      DO NC = 1, NMN25E
         CALL ALREDI(0, MND, MNPR, ':MN26a', 1, 1, NMNT)
         NMN25T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn26a in mn data file')
         END IF

         NDATA = NMNT(1) * 2
         CALL ALREDF(0, MND, MNPR, ':MN26b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KD1DTH(NC, NTB) = DUMMY(2 * NTB - 1)
            KD1CNC(NC, NTB) = DUMMY(2 * NTB)
         END DO
      END DO

   ! kd2
   ! ----
      CALL ALREDI(0, MND, MNPR, ':MN27a', 1, 1, IDUM)
      NMN27E = IDUM(1)
      IF ((NMN27E > NMNEEE) .OR. (NMN27E <= 0)) THEN
         CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn27 in mn data file')
      END IF

      CALL ALALLI(NMN27E, MND, MNPR, ':MN27b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, KD2ELM, IDUM)

      DO NC = 1, NMN27E
         CALL ALREDI(0, MND, MNPR, ':MN28a', 1, 1, NMNT)
         NMN27T(NC) = NMNT(1)
         IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
            CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn28a in mn data file')
         END IF

         NDATA = NMNT(1) * 2
         CALL ALREDF(0, MND, MNPR, ':MN28b', NDATA, 1, DUMMY)
         DO NTB = 1, NMNT(1)
            KD2DTH(NC, NTB) = DUMMY(2 * NTB - 1)
            KD2CNC(NC, NTB) = DUMMY(2 * NTB)
         END DO
      END DO

   ! ammonium adsorption
   ! -------------------
   ! * kd parameter
      CALL ALREDF(0, MND, MNPR, ':MN30', NS, 1, KDDSOL)
   ! * power parameter n
      CALL ALREDF(0, MND, MNPR, ':MN31', 1, 1, DUMMY)
      GNN = DUMMY(1)

   ! temperature effect within the soil
   ! ----------------------------------
   ! * for the environmental reduction factor for temperature is a q10
   ! * function being used ? if it is the q10 factors are needed
      CALL ALREDL(0, MND, MNPR, ':MN35', 1, 1, LDUM)
      ISQ10 = LDUM(1)
      IF (ISQ10) THEN
         CALL ALREDF(0, MND, MNPR, ':MN35a', 2, 1, DUMMY)
         Q10M = DUMMY(1)
         Q10N = DUMMY(2)
      END IF

   ! values used to calculate the initial concentrations in the organic pls
   ! ----------------------------------------------------------------------
   ! * for the initial conditions of the carbon litter pool either
   ! * a decay function for each element or an typical elem. is defined
      CALL ALREDL(0, MND, MNPR, ':MN40', 1, 1, LDUM)
      ISICCD = LDUM(1)

      IF (ISICCD) THEN
         ! * total carbon concentration at the ground surface
         CALL ALALLF(1, 1, 0, MND, MNPR, ':MN41', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, CTOTTP, IDUM, DUMMY)
         ! * depth at which carbon conc. reduced by half
         CALL ALALLF(1, 1, 0, MND, MNPR, ':MN42', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, DCHLF, IDUM, DUMMY)
      ELSE
         ! * find out how many typical element catagories
         CALL ALREDI(0, MND, MNPR, ':MN43a', 1, 1, IDUM)
         NMN43E = IDUM(1)
         IF ((NMN43E > NMNEEE) .OR. (NMN43E <= 0)) THEN
            CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn43 in mn data file')
         END IF

         ! * read the catagory type for each element into the element number
         CALL ALALLI(NMN43E, MND, MNPR, ':MN43b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, CELEM, IDUM)

         ! * table of values for each typical element
         DO NC = 1, NMN43E
            CALL ALREDI(0, MND, MNPR, ':MN44a', 1, 1, NMNT)
            NMN43T(NC) = NMNT(1)
            IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
               CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn44a in mn data file')
            END IF

            NDATA = NMNT(1) * 2
            CALL ALREDF(0, MND, MNPR, ':MN44b', NDATA, 1, DUMMY)
            DO NTB = 1, NMNT(1)
               CDPTH(NC, NTB) = DUMMY(2 * NTB - 1)
               CCONC(NC, NTB) = DUMMY(2 * NTB)
            END DO
         END DO
      END IF

   ! * proportion of the carbon in the litter and biomass pool
      CALL ALREDF(0, MND, MNPR, ':MN45', 1, 1, DUMMY)
      CLITFR = DUMMY(1)

   ! * carbon to nitrgen ratio in the litter fraction
      CALL ALREDF(0, MND, MNPR, ':MN46', 1, 1, DUMMY)
      CNRLIT = DUMMY(1)

   ! values used to calculate the initial concentrations in the ammoniumpool
   ! ----------------------------------------------------------------------
      CALL ALREDL(0, MND, MNPR, ':MN50', 1, 1, LDUM)
      ISIAMD = LDUM(1)

      IF (ISIAMD) THEN
         ! * total ammonium concentration at the ground surface
         CALL ALALLF(1, 1, 0, MND, MNPR, ':MN51', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, NAMTOP, IDUM, DUMMY)
         ! * depth at which ammonium conc. reduced by half
         CALL ALALLF(1, 1, 0, MND, MNPR, ':MN52', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCAT, DAMHLF, IDUM, DUMMY)
      ELSE
         ! * find out how many typical element catagories
         CALL ALREDI(0, MND, MNPR, ':MN53a', 1, 1, IDUM)
         NMN53E = IDUM(1)
         IF ((NMN53E > NMNEEE) .OR. (NMN53E <= 0)) THEN
            CALL ERROR(FATAL, 3090, MNPR, 0, 0, 'error in ncat in :mn53 in mn data file')
         END IF

         ! * read the catagory type for each element into the element number
         CALL ALALLI(NMN53E, MND, MNPR, ':MN53b', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NAELEM, IDUM)

         ! * table of values for each typical element
         DO NC = 1, NMN53E
            CALL ALREDI(0, MND, MNPR, ':MN54a', 1, 1, NMNT)
            NMN53T(NC) = NMNT(1)
            IF ((NMNT(1) > NMNTEE) .OR. (NMNT(1) <= 0)) THEN
               CALL ERROR(FATAL, 3091, MNPR, 0, 0, 'error in nmnt in :mn54a in mn data file')
            END IF

            NDATA = NMNT(1) * 2
            CALL ALREDF(0, MND, MNPR, ':MN54b', NDATA, 1, DUMMY)
            DO NTB = 1, NMNT(1)
               NADPTH(NC, NTB) = DUMMY(2 * NTB - 1)
               NACONC(NC, NTB) = DUMMY(2 * NTB)
            END DO
         END DO
      END IF

   ! cell below which no nitrogen transformations are considered
   ! -----------------------------------------------------------
      CALL ALREDI(0, MND, MNPR, ':MN60', 1, 1, IDUMS)
      NBOTCE = IDUMS(1)

   ! epilogue
   ! --------
      CALL ALRED2(1, MND, MNPR, 'MND')

   END SUBROUTINE MNRED1

!> @brief Reads scheduled nitrogen and carbon additions for the current timestep.
!>
!> `mnred2` maintains saved next-event times for the external inorganic nitrogen
!> (`MNFN`) and external carbon/organic nitrogen (`MNFC`) files. Times read from
!> `MNFN01` and `MNFC01` are converted with [[utilsmod:hour_from_date]] and
!> shifted by the simulation start hour `TIH`.
!>
!> | File | Activation test | Records read when active | Flag |
!> | --- | --- | --- | --- |
!> | `MNFN` | `UZNOW + DTUZ/3600 > INTIMN` | `MNFN11` total nitrogen, `MNFN21` banding depth, `MNFN31` ammonium fraction, then the next `MNFN01` time. | `ISADDN=.true.` |
!> | `MNFC` | `UZNOW + DTUZ/3600 > INTIMC` | `MNFC11` total carbon, `MNFC21` banding depth, `MNFC31` litter fraction, `MNFC32` litter C:N, `MNFC41` manure fraction, `MNFC42` manure C:N, then the next `MNFC01` time. | `ISADDC=.true.` |
!>
!> If a file is not active in the current timestep, only its flag is set false;
!> the previous data arrays are not overwritten. [[mnerr4]] and [[mnint2]] gate
!> their use with `ISADDN` and `ISADDC`.
!>
!> The source assumes at most one nitrogen and one carbon event per timestep. If
!> more are scheduled, only the first active event is read and the next event
!> remains queued for a later call.
   SUBROUTINE MNRED2(MNFC, MNFN, MNPR, NEL, NELEE, NLF, NLFEE, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, DTUZ, TIH, UZNOW, BEXBK, LINKNS, &
                     CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, ISADDC, ISADDN, IDUM, DUMMY)

      USE UTILSMOD, ONLY : hour_from_date
      USE SGLOBAL, ONLY : nyee

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: MNFC  !! Scheduled carbon-addition input unit.
      INTEGER, INTENT(IN) :: MNFN  !! Scheduled nitrogen-addition input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Neighbour reference map.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)  !! Element number at each grid location.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: TIH  !! Initial simulation time in hours.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)  !! True for north-south channel links.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CDPTHB(NLF + 1:NEL)  !! Carbon banding depth.
      DOUBLE PRECISION, INTENT(OUT) :: CLTFCT(NLF + 1:NEL)  !! Litter fraction of added carbon.
      DOUBLE PRECISION, INTENT(OUT) :: CMNFCT(NLF + 1:NEL)  !! Manure fraction of added carbon.
      DOUBLE PRECISION, INTENT(OUT) :: CNRAL(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added litter.
      DOUBLE PRECISION, INTENT(OUT) :: CNRAM(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added manure.
      DOUBLE PRECISION, INTENT(OUT) :: CTOT(NLF + 1:NEL)  !! Total external carbon addition.
      DOUBLE PRECISION, INTENT(OUT) :: NAMFCT(NLF + 1:NEL)  !! Ammonium fraction of added inorganic nitrogen.
      DOUBLE PRECISION, INTENT(OUT) :: NDPTHB(NLF + 1:NEL)  !! Nitrogen banding depth.
      DOUBLE PRECISION, INTENT(OUT) :: NTOT(NLF + 1:NEL)  !! Total external inorganic nitrogen addition.
      LOGICAL, INTENT(OUT) :: ISADDC  !! True when a carbon-addition event is active.
      LOGICAL, INTENT(OUT) :: ISADDN  !! True when a nitrogen-addition event is active.

      ! Workspace arguments (INTENT(INOUT) because they act as read buffers)
      INTEGER, INTENT(INOUT) :: IDUM(NELEE)  !! Integer workspace for spatial reads.
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)  !! Floating-point workspace for spatial reads.

      ! Locals
      INTEGER :: NCAT
      INTEGER :: TIME(5)

      ! Saved state variables
      INTEGER, SAVE :: INTIMC, INTIMN
      INTEGER, SAVE :: PASS = 0

   !-------------------------------------------------------------------*

      PASS = PASS + 1

   ! 1. check data files are open and read first input times
   ! -------------------------------------------------------
      IF (PASS == 1) THEN
         ! * check status of nitrogen fertilizer data file
         CALL ALRED2(0, MNFN, MNPR, 'MNFM')

         ! * time of first nitrogen fertilizer addition
         CALL ALREDI(0, MNFN, MNPR, ':MNFN01', 5, 1, TIME)
         INTIMN = INT(hour_from_date(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH)

         ! * check status of carbon fertilizer data file
         CALL ALRED2(0, MNFC, MNPR, 'MNFC')

         ! * time of first carbon fertilizer addition
         CALL ALREDI(0, MNFC, MNPR, ':MNFC01', 5, 1, TIME)
         INTIMC = INT(hour_from_date(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH)
      END IF


   ! 2. read nitrogen data file if fertilization occurs in this timestep
   ! -------------------------------------------------------------------
      IF ((UZNOW + DTUZ / 3.6D3) > INTIMN) THEN
         ISADDN = .TRUE.

         ! * total nitrogen fertilizer in each element (kg n m-2)
         CALL ALALLF(1, 1, 0, MNFN, MNPR, ':MNFN11', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, NTOT, IDUM, DUMMY)

         ! * depth the fertilizer is banded over (m)
         CALL ALALLF(1, 1, 0, MNFN, MNPR, ':MNFN21', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, NDPTHB, IDUM, DUMMY)

         ! * ammonium fraction (the remainder is nitrate )
         CALL ALALLF(1, 1, 0, MNFN, MNPR, ':MNFN31', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, NAMFCT, IDUM, DUMMY)

         ! * time of next nitrogen fertilizer addition
         CALL ALREDI(0, MNFN, MNPR, ':MNFN01', 5, 1, TIME)
         INTIMN = INT(hour_from_date(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH)

      ELSE
         ISADDN = .FALSE.
      END IF


   ! 3. read carbon data file if fertilization occurs in this timestep
   ! -----------------------------------------------------------------
      IF ((UZNOW + DTUZ / 3.6D3) > INTIMC) THEN
         ISADDC = .TRUE.

         ! * total carbon fertilizer in each element (kg n m-2)
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC11', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, CTOT, IDUM, DUMMY)

         ! * depth the fertilizer is banded over (m)
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC21', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, CDPTHB, IDUM, DUMMY)

         ! * litter fraction
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC31', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, CLTFCT, IDUM, DUMMY)

         ! * carbon/nitrogen ratio of the litter
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC32', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, CNRAL, IDUM, DUMMY)

         ! * manure fraction (the remainder from the litter and manure is humus)
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC41', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, CMNFCT, IDUM, DUMMY)

         ! * carbon/nitrogen ratio of the manure
         CALL ALALLF(1, 1, 0, MNFC, MNPR, ':MNFC42', NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
                     NCAT, CNRAM, IDUM, DUMMY)

         ! * time of next carbon fertilizer addition
         CALL ALREDI(0, MNFC, MNPR, ':MNFC01', 5, 1, TIME)
         INTIMC = INT(hour_from_date(TIME(1), TIME(2), TIME(3), TIME(4), TIME(5)) - TIH)

      ELSE
         ISADDC = .FALSE.
      END IF

   END SUBROUTINE MNRED2

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

      USE UTILSMOD, ONLY: TRIDAG

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

      KFCT = DIFF * ((NUM - 1.0D0) / Z2) * ((NUM - 1.0D0) / Z2)

      ! * ground temperature is equal to the air temperature plus a
      ! * constant value
      GRDTEM = TA(1) + DIFFGA
      TEMPR1(1) = GRDTEM

      ! * position in the matrix are one lower than in the column,
      ! * this is because the ground surface value is known
      RHS(1) = KFCT * GRDTEM + KFCT * (-2.0D0 * TEMPR(2) + TEMPR(3))
      RHS(NUM - 1) = (TEMPR(NUM - 1) - TEMPR(NUM)) * KFCT

      AMAT(1) = 0.0D0
      BMAT(1) = 1.0D0 + 2.0D0 * KFCT * DTUZ
      CMAT(1) = -KFCT * DTUZ

      AMAT(NUM - 1) = -KFCT * DTUZ
      BMAT(NUM - 1) = 1.0D0 + KFCT * DTUZ
      CMAT(NUM - 1) = 0.0D0

      DO NCE = 2, NUM - 2
         AMAT(NCE) = -KFCT * DTUZ
         BMAT(NCE) = 1.0D0 + 2.0D0 * KFCT * DTUZ
         CMAT(NCE) = -KFCT * DTUZ
         RHS(NCE) = KFCT * (TEMPR(NCE) - 2.0D0 * TEMPR(NCE + 1) + TEMPR(NCE + 2))
      END DO

      CALL TRIDAG(AMAT, BMAT, CMAT, RHS, OME, NUM - 1)

      ! * new temperature at each node
      DO NCE = 2, NUM
         TEMPR1(NCE) = TEMPR(NCE) + OME(NCE - 1) * DTUZ
      END DO

      ! * depth of each node
      DEPTH(1) = 0.0D0
      DO NNUM = 2, NUM
         DEPTH(NNUM) = DEPTHC / DBLE(NUM - 1) + DEPTH(NNUM - 1)
      END DO

      element_loop: DO IEL = NLF + 1, NEL
         NCEBOT = NCOLMB(IEL)
         NSERCH = 2

         cell_loop: DO NCE = NCETOP, NCEBOT, -1
            ! * calculation of the depth of the cell
            IF (NCE == NCETOP) THEN
               CELLDP = 0.5D0 * DELTAZ(NCE, IEL)
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
            CELLFC = (CELLDP - DEPTH(NSERCH - 1)) / (DEPTH(NSERCH) - DEPTH(NSERCH - 1))
            TEMP(IEL, NCE) = (1.0D0 - CELLFC) * TEMPR1(NSERCH - 1) + CELLFC * TEMPR1(NSERCH)
         END DO cell_loop
      END DO element_loop

      ! Update the saved temperature state for the next timestep
      TEMPR(1:NUM) = TEMPR1(1:NUM)

   END SUBROUTINE MNTEMP

END MODULE MNmod
