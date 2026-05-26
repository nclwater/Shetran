!> summary: Nitrate/mineral nitrogen cycling and plant uptake.
!>
!> This module implements the optional SHETRAN Nitrate Component described in
!> the User Guide and Data Input Manual. The component requires the contaminant
!> component and is coupled through [[cmmod]] rather than run as an independent
!> transport solver. It updates ammonium and nitrate process terms in soil
!> water, carbon and nitrogen turnover in humus/litter/manure pools,
!> mineralisation and immobilisation, nitrification, denitrification, ammonia
!> volatilisation, dry/wet deposition, plant uptake, environmental response
!> factors, input checking, interpolation of spatially varying parameters, and
!> nitrate/carbon/nitrogen output reporting.
!>
!> The manual's main nitrate input file (`MND`) supplies the nitrate title,
!> decomposition and uptake constants for ammonium and nitrate (`MN11`),
!> organic-matter efficiency and humification parameters (`MN12`), ammonium and
!> nitrate deposition rates (`MN13`), reference concentration for nonlinear
!> adsorption (`MN14`), category/depth tables for humus, litter, manure,
!> nitrification, ammonia volatilisation, and denitrification (`MN15`-`MN28`),
!> ammonium adsorption parameters (`MN30`, `MN31`), Q10 controls for
!> mineralisation and nitrification (`MN35`, `MN35a`), initial carbon and
!> ammonium conditions (`MN40`-`MN54`), and the lower cell limit for nitrogen
!> transformations (`MN60`).
!>
!> Additional manual-defined files provide time-varying external carbon inputs
!> (`MNFC`), external inorganic nitrogen/fertilizer inputs (`MNFN`), nitrogen
!> plant-uptake parameters (`MNPL`), printed diagnostics (`MNPR`), extra carbon
!> and nitrogen output (`MNOUT1`, `MNOUT2`), and plant nitrogen output
!> (`MNOUTPL`).
!>
!> Nitrate concentrations are transported by the contaminant solver; this module
!> supplies ammonium/nitrate reaction, uptake, deposition, and source/sink terms
!> for the dynamic and dead-space soil-water regions. At present [[mnerr0]]
!> expects the contaminant interface to provide the configured single nitrogen
!> contaminant species.
!>
!> The runtime path is split between first-call setup and later timesteps:
!> [[MNCONT]] allocates state and calls [[mnplant]] then [[mnmain]]; [[mnmain]]
!> reads static MND data only on its first call, while later calls read
!> scheduled MNFC/MNFN additions, update environmental factors and process
!> pools, form contaminant source/sink terms, and write cumulative MN outputs.
!>
!> @note Legacy implementation details that affect interpretation include
!> [[MNCONT]] overwriting `TA(1:NV)` with `10.0`, [[mnint2]] hard-coding
!> `PPHI=0.500`, and [[mnout]] reporting additions/losses cumulatively since its
!> first call.
!> @endnote
!>
!> @warning Plant uptake is calculated by [[mnplant]] before the main nitrogen
!> update. The legacy source comments state that this routine has weak input
!> checking, so changes to plant uptake parameters should be reviewed against
!> the manual's nitrate plant-uptake input file (`MNPL`) and tested carefully.
!> @endwarning
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | SB | 4.6 | Capitalised `MNCONT` for Linux builds. |
!> | 2026-03 | SB | 4.6 | Changed key interface/work arrays to allocatable storage. |
!>
!> The allocatable arrays in the March 2026 change include `VSTHEO`, `NLYRBT`,
!> `NTSOIL`, `DELTAZ`, `RDF`, `ZVSNOD`, `CCCC`, `SSSS`, `SSS1`, and `SSS2`.
!> @endhistory
module MNmod

    use sglobal, only : llee, nconee, nelee, nlfee, nlyree, npelee, npltee, nsee, nvee, nxee, nyee, error
    use mod_load_filedata,    only : alallf, alalli, alchk, alchki, alintp, alred2, alredc, alredf, alredi, alredl
    use utilsmod, only: hour_from_date, tridag


    PUBLIC    :: mnamm, mnco2, MNCONT, mnedth, mnemph, mnemt, mnenph, mnent   ! subroutine names
    PUBLIC    :: mnerr0, mnerr1, mnerr2, mnerr3, mnerr4, mngam, mninit, mnint2
	PUBLIC    :: mnlthm, mnltn, mnmain, mnman, mnnit, mnout, mnplant, mnred1, mnred2, mntemp


    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: cahum  !! External carbon addition rate assigned to humus.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: calit  !! External carbon addition rate assigned to litter.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: caman  !! External carbon addition rate assigned to manure.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: cdort  !! Carbon dioxide production rate from organic matter turnover.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: chum   !! Humus carbon at the start of the timestep.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: chum1  !! Updated humus carbon.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: clit   !! Litter carbon at the start of the timestep.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: clit1  !! Updated litter carbon.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: cman   !! Manure carbon at the start of the timestep.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: cman1  !! Updated manure carbon.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: denit  !! Denitrification loss rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: dummy4 !! Floating-point workspace array for MN input checks.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: dummy6 !! Floating-point workspace array for MN input checks.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: edeth  !! Water-content reduction factor for denitrification.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: emph   !! Matric-potential reduction factor for mineralisation.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: emt    !! Temperature reduction factor for mineralisation.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: enph   !! Matric-potential reduction factor for nitrification.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ent    !! Temperature reduction factor for nitrification.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: gam    !! Net mineralisation rate after immobilisation constraints.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: gamtmp !! Net mineralisation rate before immobilisation-deficit adjustment.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: imamm  !! Ammonium immobilisation rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: imdiff !! Immobilisation demand that could not be met in the current timestep.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: imnit  !! Nitrate immobilisation rate.
    LOGICAL, DIMENSION(:,:), ALLOCATABLE :: isimtf        !! True where immobilisation shortage suppresses litter/manure turnover.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: kd1    !! Denitrification carbon-demand coefficient.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: kd2    !! Denitrification nitrate-availability coefficient.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: khum   !! Humus decomposition rate coefficient.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: klit   !! Litter decomposition rate coefficient.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: kman   !! Manure decomposition rate coefficient.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: knit   !! Nitrification rate coefficient.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: kvol   !! Ammonia volatilisation rate coefficient.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: miner  !! Gross mineralisation rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: naamm  !! Ammonium addition/deposition rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: namm   !! Ammonium concentration at the start of the timestep.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: namm1  !! Updated ammonium concentration.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nanit  !! Nitrate addition/deposition rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ndnit  !! Nitrate half-saturation denominator for plant uptake partitioning.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ndsnt  !! Ammonium half-saturation denominator for plant uptake partitioning.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nlit   !! Litter nitrogen at the start of the timestep.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nlit1  !! Updated litter nitrogen.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nman   !! Manure nitrogen at the start of the timestep.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: nman1  !! Updated manure nitrogen.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: ntrf   !! Nitrification rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: plamm  !! Actual ammonium plant uptake rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: plnit  !! Actual nitrate plant uptake rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: plup   !! Potential plant nitrogen uptake rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: pphi   !! Mobile-water partition factor for ammonium/nitrate uptake.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: snit   !! Total nitrate source/sink diagnostic rate.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: temp   !! Soil temperature used by MN response factors.
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: vol    !! Ammonia volatilisation loss rate.

    CONTAINS


!> Updates dissolved ammonium concentration for all active soil cells.
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
subroutine mnamm (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,nlyree,ns,ncolmb,nlyr,nlyrbt,ntsoil,gnn,kplamm,kuamm, &
    mncref,kddsol,dtuz,vsthe,vstheo,isbotc)

    ! externals
    !use sglobal, only : error
    !       external      error
    integer llee                    !! Maximum soil-cell dimension.
    integer mnpr                    !! MN diagnostic output unit used for warning messages.
    integer nbotce                  !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column updates.
    integer nlyree                  !! Soil-layer array dimension.
    integer ns                      !! Number of soil types.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nlyr(nelee)             !! Number of soil layers in each element.
    integer nlyrbt(nel,nlyree)      !! Bottom cell index of each soil layer.
    integer ntsoil(nel,nlyree)      !! Soil type index for each element layer.
    double precision gnn            !! Nonlinear ammonium adsorption exponent.
    double precision kplamm         !! First-order ammonium plant-uptake limit.
    double precision kuamm          !! First-order ammonium immobilisation limit.
    double precision mncref         !! Reference nitrogen concentration.
    double precision kddsol(ns)     !! Soil ammonium adsorption coefficient.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision vsthe(ncetop,nel)     !! Current volumetric water content.
    double precision vstheo(nel,ncetop+1)  !! Previous volumetric water content.
    logical isbotc                  !! True when the fixed lower active cell `NBOTCE` is used.
    ! locals
    integer          jsoil,jlyr,nbotm,ncebot,ncl,nelm,niters,ntime
    integer          warn
    !
    double precision dum,dum1,dum2,errtol,namm1o
    double precision nammh,retamm,retamm1,ttheth,werr1,wer1sq
    !
    character        msg*132
    !
    !      * parameters for the iteration loop within the subroutine
    !      * niters is the maximum number of accepteble interations
    !      * and errtol is the squared error below which the interation
    !      * will stop before niters is reached
    parameter ( niters = 20, warn = 3)
    parameter ( errtol = 1.0d-12)
    !
    !-------------------------------------------------------------------*
    !
    do nelm = nlf+1,nel
        if (isbotc) then
            nbotm = nbotce
        else
            nbotm = ncolmb(nelm)
        endif
        ncebot = nbotm
        do jlyr = 1,nlyr(nelm)
            jsoil = ntsoil(nelm,jlyr)
            do 150 ncl =max(ncebot,nlyrbt(nelm,jlyr)),nlyrbt(nelm,jlyr+1)-1
                !
                !           * initialise local variables
                nammh = namm(nelm,ncl)
                namm1o = 0.0d0
                !           * old retardation factor for ammonium adsorption
                retamm = 1.0 +(kddsol(jsoil)*(namm(nelm,ncl)/mncref)**(gnn-1))/vstheo(nelm,ncl)
                !
                ttheth = (vsthe(ncl,nelm) + vstheo(nelm,ncl))/2.0d0
                !
                !           *  iteration loop to calcalate the new ammonium nitrogen
                !           *  concentrations in the soil water
                do ntime = 1,niters
                    !
                    !              * new retardation factor for ammonium adsorption
                    retamm1 = 1.0 +(kddsol(jsoil)*(namm1(nelm,ncl)/mncref)**(gnn-1))/vsthe(ncl,nelm)
                    !
                    !              * calculation of both the mineralisation rate and the
                    !              * immobilisation rate of ammonium
                    if (gam(nelm,ncl)>=0.0d0) then
                        miner(nelm,ncl) = gam(nelm,ncl)
                        imamm(nelm,ncl) = 0.0d0
                    else
                        miner(nelm,ncl) = 0.0d0
                        imamm(nelm,ncl) =min( -gam(nelm,ncl) , kuamm*nammh )
                    endif
                    !
                    !              * calculation of the nitrification rate
                    ntrf(nelm,ncl)= ttheth * knit(nelm,ncl) * ent(nelm,ncl)* enph(nelm,ncl) * nammh
                    !
                    !              * calculation of the ammonia volatilisation rate
                    vol(nelm,ncl) = ttheth * kvol(nelm,ncl) * emt(nelm,ncl)* nammh
                    !
                    !              * calculation of the plant uptake rate of ammonium
                    if (nammh>0.0d0) then
                        dum1 = plup(nelm,ncl) *(pphi(nelm,ncl)*nammh/(ndnit(nelm,ncl)+nammh)+ (1-pphi(nelm,ncl))* &
                        nammh/(ndsnt(nelm,ncl)+nammh))
                    else
                        dum1 = 0.0d0
                    endif
                    dum2 = vsthe(ncl,nelm) * kplamm * nammh
                    plamm(nelm,ncl) = min (dum1,dum2)
                    !
                    !
                    !              * calculation of the concentration of ammonium in solutn
                    !              * at timestep n + 1
                    dum = -plamm(nelm,ncl) +miner(nelm,ncl) -imamm(nelm,ncl)- ntrf(nelm,ncl) - vol(nelm,ncl) + &
                     naamm(nelm,ncl)
                    namm1(nelm,ncl) = 1/(vsthe(ncl,nelm)*retamm1)*(vstheo(nelm,ncl)*namm(nelm,ncl)*retamm + dtuz*dum)
                    !
                    !              *  ammonium conc at timestep n +1/2 is calculated for use
                    !              *  in the new calculation of the ammonium
                    nammh = (namm1(nelm,ncl)+namm(nelm,ncl))/ 2.0d0
                    !
                    !
                    !              *  relative error between iterations to see if the
                    !              *  iteration is converging.
                    if (namm1(nelm,ncl)/=0.0d0) then
                        werr1 = (namm1(nelm,ncl) - namm1o) / namm1(nelm,ncl)
                    elseif (namm1o==0.0d0) then
                        werr1 = 0.0d0
                    else
                        werr1 = 1.0d0
                    endif
                    !
                    !              * square of the errors, in order to make them positive
                    wer1sq = werr1*werr1
                    !
                    namm1o = namm1(nelm,ncl)
                    !
                    !              *  break out of loop if the error in the iteration
                    !              *  is less than the error tolerence
                    if (wer1sq<errtol) goto 150
                    !                                    ********
                    !
                enddo
                !
                !          *  the do loop has continued to niters and has thus
                !          *  failed to converge
                write (msg,9000) wer1sq
                call error( warn, 3018, mnpr, 0, 0, msg )
                !
                !
150         continue
        enddo
    enddo
    !
    9000 format('iteration loop in mnamm failed to converge with error = ',g15.7)
    !
end subroutine mnamm


!> Calculates cumulative carbon dioxide production from organic matter turnover.
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

    integer llee              !! Maximum soil-cell dimension.
    integer nbotce            !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop            !! Top soil-cell index.
    integer nel               !! Number of elements.
    integer nelee             !! Element-array dimension.
    integer nlf               !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)     !! Lowest active soil cell in each land-column element.
    double precision fe       !! Efficiency fraction for organic carbon turnover.
    double precision fh       !! Humification fraction.
    logical isbotc            !! True when the fixed lower active cell `NBOTCE` is used.
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



!> Controls the mineral nitrogen component from the contaminant timestep.
!>
!> `MNCONT` is called by [[cmmod:cmsim]] when the mineral nitrogen option is
!> active. It allocates MN work arrays on the first call, computes potential
!> plant nitrogen uptake with [[mnplant]], then calls [[mnmain]] to read or
!> update mineral nitrogen state and to fill the contaminant source/sink arrays
!> `sss1` and `sss2` used by the CM transport equations.
!>
!> | Phase | Main action |
!> |:------|:------------|
!> | First call only | Allocate all MN carbon, nitrogen, process-rate, environmental-factor, adsorption, plant-uptake, and workspace arrays with shape based on `NEL` and `NCETOP`. |
!> | Temporary temperature setup | Set every vegetation air-temperature entry `TA(1:NV)` to 10.0 before plant uptake and the main nitrogen update. |
!> | Plant uptake | Call [[mnplant]] to calculate nitrogen plant uptake demand and related plant output. |
!> | Main MN update | Call [[mnmain]] to initialise/check/read inputs on the first pass and then update ammonium/nitrate source-sink terms. |
!>
!> The dissolved nitrate concentration fields are supplied through the CM arrays
!> `cccc` and `ssss`; ammonium, litter, humus, manure, and process-rate pools are
!> held internally by `MNmod`. Rates and pools are evaluated over land columns
!> from `NLF+1:NEL`; channel links are not treated as nitrogen soil columns.
!>
!> @note The `TA=10.0` assignment is implemented as temporary code in the source.
!> It overwrites the incoming `TA` values passed to `MNCONT` before [[mnplant]]
!> and [[mnmain]] are called.
!> @endnote
!>
!> @warning The legacy source comments note that [[mnplant]] has limited input
!> checking. The main nitrogen update path performs more extensive validation in
!> [[mnerr0]], [[mnerr1]], [[mnerr2]], [[mnerr3]], and [[mnerr4]].
!> @endwarning
subroutine MNCONT(mnd,mnfc,mnfn,mnpl,mnpr,mnout1,mnout2,mnoutpl,ncetop,ncon,nel,nlf,ns,nv,nx,ny,icmbk,icmref, &
    icmxy,ncolmb,nlyr,nrd,nvc,nlyrbt,ntsoil,d0,tih,rhopl,z2,delone,dxqq,dyqq,vspor,deltaz,plai,rdf,zvsnod,bexbk, &
    linkns,dtuz,uznow,clai,cccc,pnetto,ssss,ta,vspsi,vsthe,vstheo,sss1,sss2 )

    integer mnd                     !! Static MND input unit.
    integer mnfc                    !! Scheduled carbon-addition input unit.
    integer mnfn                    !! Scheduled nitrogen-addition input unit.
    integer mnpl                    !! Plant-uptake input unit.
    integer mnpr                    !! MN diagnostic output unit.
    integer mnout1                  !! Carbon budget output unit.
    integer mnout2                  !! Nitrogen budget output unit.
    integer mnoutpl                 !! Plant nitrogen output unit.
    integer ncetop                  !! Top soil-cell index.
    integer ncon                    !! Number of contaminant species coupled to MN.
    integer nel                     !! Number of elements.
    integer nlf                     !! Number of overland/channel links.
    integer ns                      !! Number of soil types.
    integer nv                      !! Number of vegetation/meteorological entries.
    integer nx                      !! Number of grid columns.
    integer ny                      !! Number of grid rows.
    integer icmbk(nlfee,2)          !! Bank-element numbers for each channel link.
    integer icmref(nelee,4,2:2)     !! Neighbour reference map.
    integer icmxy(nxee,ny)          !! Element number at each grid location.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nlyr(nelee)             !! Number of soil layers in each element.
    integer nrd(nv)                 !! Rooting depth in cell counts by vegetation type.
    integer nvc(nelee)              !! Vegetation type index by element.
    integer nlyrbt(nel,nlyree)      !! Bottom cell index of each soil layer.
    integer ntsoil(nel,nlyree)      !! Soil type index for each element layer.
    double precision d0             !! Reference diffusion/dispersion scale used by CM.
    double precision tih            !! Initial simulation time in hours.
    double precision rhopl          !! Plant dry-matter density used by uptake calculation.
    double precision z2             !! Vertical length scale used by CM and MN temperature diffusion.
    double precision delone(npltee) !! Initial plant biomass/cover scaling by plant type.
    double precision dxqq(nelee)    !! Element width.
    double precision dyqq(nelee)    !! Element length.
    double precision vspor(ns)      !! Soil porosity by soil type.
    double precision deltaz(llee,nel) !! Cell thickness by cell and element.
    double precision plai(nv)       !! Plant leaf-area index by vegetation type.
    double precision rdf(nv,llee)   !! Root density fraction by vegetation type and cell.
    double precision zvsnod(llee,nel) !! Vertical node elevation/depth by cell and element.
    logical bexbk                   !! True when bank elements are represented.
    logical linkns(nlfee)           !! True for north-south channel links.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision uznow          !! Current unsaturated-zone simulation time.
    double precision clai(nv)       !! Current canopy leaf-area index by vegetation type.
    double precision cccc(nel,ncetop+1) !! Dynamic-region nitrate concentration.
    double precision pnetto(nelee)  !! Net precipitation/effective rainfall by element.
    double precision ssss(nel,ncetop+1) !! Dead-space nitrate concentration.
    double precision ta(nv)         !! Air temperature by vegetation/meteorological entry.
    double precision vspsi(ncetop,nel)  !! Matric potential/pressure head by cell and element.
    double precision vsthe(ncetop,nel)  !! Current volumetric water content.
    double precision vstheo(nel,ncetop+1) !! Previous volumetric water content.
    double precision sss1(nel,ncetop+1)  !! Dynamic-region CM source/sink array.
    double precision sss2(nel,ncetop+1)  !! Dead-space CM source/sink array.
    integer pass

    save pass
    data pass /0 /


    pass = pass + 1

    if (pass==1) then
        allocate   (cahum(nel,ncetop),calit(nel,ncetop),caman(nel,ncetop),cdort(nel,ncetop),chum(nel,ncetop),chum1(nel,ncetop),clit(nel,ncetop),clit1(nel,ncetop),cman(nel,ncetop),cman1(nel,ncetop))
        allocate   (denit(nel,ncetop),dummy4(ncetop,nel),dummy6(nel,ncetop))
        allocate   (edeth(nel,ncetop),emph(nel,ncetop),emt(nel,ncetop),enph(nel,ncetop),ent(nel,ncetop))
        allocate   (gam(nel,ncetop),gamtmp(nel,ncetop),imamm(nel,ncetop),imdiff(nel,ncetop),imnit(nel,ncetop),isimtf(nel,ncetop))
        allocate   (kd1(nel,ncetop),kd2(nel,ncetop),khum(nel,ncetop),klit(nel,ncetop),kman(nel,ncetop),knit(nel,ncetop),kvol(nel,ncetop))
        allocate   (miner(nel,ncetop))
        allocate   (naamm(nel,ncetop),namm(nel,ncetop),namm1(nel,ncetop),nanit(nel,ncetop),ndnit(nel,ncetop),ndsnt(nel,ncetop),nlit(nel,ncetop),nlit1(nel,ncetop),nman(nel,ncetop),nman1(nel,ncetop),ntrf(nel,ncetop))
        allocate   (plamm(nel,ncetop),plnit(nel,ncetop),plup(nel,ncetop),pphi(nel,ncetop))
        allocate   (snit(nel,ncetop),temp(nel,ncetop),vol(nel,ncetop))
    endif


    !
    !
    !----------------------------------------------------------------------*
    ! temp code	(sb 1/3/01)
    do i = 1,nv
	    ta(i) = 10.0
    enddo


    call mnplant(mnpl,mnoutpl,ncetop,nel,nlf,nv,ncolmb,nrd,nvc,rhopl,delone,dxqq,dyqq,deltaz,plai,rdf,dtuz,uznow, &
        clai)
    call mnmain(mnd,mnfc,mnfn,mnpr,mnout1,mnout2,ncetop,ncon,nel,nlf,ns,nv,nx,ny,icmbk,icmref,icmxy,ncolmb,nlyr, &
        nlyrbt,ntsoil,d0,tih,z2,dxqq,dyqq,vspor,deltaz,zvsnod,bexbk,linkns,dtuz,uznow,cccc,pnetto,ssss,ta,vspsi, &
        vsthe,vstheo,sss1,sss2 )

end subroutine MNCONT


!> Calculates the water-content reduction factor for denitrification.
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
subroutine mnedth (llee,nbotce,ncetop,nel,nelee,nlf,nlyree,ns,ncolmb,nlyr,nlyrbt,ntsoil,vsthe,vspor,isbotc )

    integer llee                    !! Maximum soil-cell dimension.
    integer nbotce                  !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column updates.
    integer nlyree                  !! Soil-layer array dimension.
    integer ns                      !! Number of soil types.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nlyr(nelee)             !! Number of soil layers in each element.
    integer nlyrbt(nel,nlyree)      !! Bottom cell index of each soil layer.
    integer ntsoil(nel,nlyree)      !! Soil type index for each element layer.
    double precision vsthe(ncetop,nel) !! Current volumetric water content.
    double precision vspor(ns)      !! Soil porosity by soil type.
    logical isbotc                  !! True when the fixed lower active cell `NBOTCE` is used.
    ! locals etc.
    integer jlyr,jsoil,nbotm,nce,ncebot,nelm
    double precision relsat
    !
    !-------------------------------------------------------------------*
    !
    do nelm = nlf+1,nel
        if (isbotc) then
            nbotm = nbotce
        else
            nbotm = ncolmb(nelm)
        endif
        ncebot = nbotm
        do jlyr = 1,nlyr(nelm)
            jsoil = ntsoil(nelm,jlyr)
            do nce =max0(ncebot,nlyrbt(nelm,jlyr)),nlyrbt(nelm,jlyr+1)-1
                !
                !              * a segmented relationship is being used with the
                !              * relative saturation falling into one of four bands
                relsat = vsthe(nce,nelm) / vspor(jsoil)
                if (relsat>1.0d0) then
                    edeth(nelm,nce) = 1.0d0
                elseif (relsat>0.9d0) then
                    edeth(nelm,nce) = -7.0d0 + 8.0d0 * relsat
                elseif (relsat>0.8d0) then
                    edeth(nelm,nce) = -1.6d0 + 2.0d0 * relsat
                else
                    edeth(nelm,nce) = 0.0d0
                endif
                !
            enddo
        enddo
    enddo
    !
end subroutine mnedth



!> Calculates the matric-potential reduction factor for mineralisation.
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

    integer llee                    !! Maximum soil-cell dimension.
    integer nbotce                  !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    double precision vspsi(ncetop,nel) !! Matric potential/pressure head by cell and element.
    logical isbotc                  !! True when the fixed lower active cell `NBOTCE` is used.
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
end	subroutine mnemph



!> Calculates the temperature reduction factor for mineralisation.
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

    integer llee              !! Maximum soil-cell dimension.
    integer nbotce            !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop            !! Top soil-cell index.
    integer nel               !! Number of elements.
    integer nelee             !! Element-array dimension.
    integer nlf               !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)     !! Lowest active soil cell in each land-column element.
    double precision q10m     !! Q10 coefficient for mineralisation temperature response.
    logical isbotc            !! True when the fixed lower active cell `NBOTCE` is used.
    logical isq10             !! True when Q10 temperature response is selected.
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


!> Calculates the matric-potential reduction factor for nitrification.
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
!> @note Older wet-condition formulae remain in comments in the source. The
!> active code uses the 1996 temporary-change values shown above, so the very
!> wet branch is `0.6` rather than zero.
!> @endnote
subroutine mnenph (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,vspsi,isbotc)

    integer llee                    !! Maximum soil-cell dimension.
    integer nbotce                  !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    double precision vspsi(ncetop,nel) !! Matric potential/pressure head by cell and element.
    logical isbotc                  !! True when the fixed lower active cell `NBOTCE` is used.
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
            !           * temporary change 22/1/96 to increase nitrification
            !           * in wet conditions
            if (vspsi(ncl,nelm)>-0.1d-1) then
                enph(nelm,ncl) = 0.6
            elseif (vspsi(ncl,nelm)>-0.6d0) then
                enph(nelm,ncl) = 1.05d0 + 0.225d0*log10(-vspsi(ncl,nelm))
                !
                !            if (vspsi(ncl,nelm)>-0.1d-1) then
                !              enph(nelm,ncl) = 0.0d0
                !            elseif (vspsi(ncl,nelm)>-0.6d0) then
                !              enph(nelm,ncl) =1.125d0 + 0.562d0*log10(-vspsi(ncl,nelm))
                !
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


!> Calculates the temperature reduction factor for nitrification.
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

    integer llee              !! Maximum soil-cell dimension.
    integer nbotce            !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop            !! Top soil-cell index.
    integer nel               !! Number of elements.
    integer nelee             !! Element-array dimension.
    integer nlf               !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)     !! Lowest active soil cell in each land-column element.
    double precision q10n     !! Q10 coefficient for nitrification temperature response.
    logical isbotc            !! True when the fixed lower active cell `NBOTCE` is used.
    logical isq10             !! True when Q10 temperature response is selected.
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



!> Checks fixed MN array dimensions, entity counts, and selected file units.
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
subroutine mnerr0(llee,mnd,mnfc,mnfn,mnpr,ncetop,ncon,nconee,nel,nelee,nlf,nlfee,nlyree,nmneee,nmntee,ns,nsee,nv, &
    nvee,nx,nxee,ny )

    ! externals
    !use sglobal, only : error
    !use mod_load_filedata ,    only : alchki
    !       external      alchki,error
    !
    integer       llee       !! Maximum soil-cell dimension.
    integer       mnd        !! Static MND input unit.
    integer       mnfc       !! Scheduled carbon-addition input unit.
    integer       mnfn       !! Scheduled nitrogen-addition input unit.
    integer       mnpr       !! MN diagnostic output unit.
    integer       ncetop     !! Top soil-cell index.
    integer       ncon       !! Number of contaminant species coupled to MN.
    integer       nconee     !! Contaminant-species array dimension.
    integer       nel        !! Number of elements.
    integer       nelee      !! Element-array dimension.
    integer       nlf        !! Number of overland/channel links.
    integer       nlfee      !! Link-array dimension.
    integer       nlyree     !! Soil-layer array dimension.
    integer       nmneee     !! Maximum number of MN category entries.
    integer       nmntee     !! Maximum number of MN table entries.
    integer       ns         !! Number of soil types.
    integer       nsee       !! Soil-type array dimension.
    integer       nx         !! Number of grid columns.
    integer       nxee       !! Grid-column array dimension.
    integer       nv         !! Number of vegetation types.
    integer       nvee       !! Vegetation-type array dimension.
    integer       ny         !! Number of grid rows.
    ! locals etc.
    integer       fatal, err
    parameter     ( fatal = 1, err = 2 )
    !
    integer       iundef,nerr
    integer       idums(1),idumo(1),izero(1),ione(1)
    logical       ldum1(1)
    !
    data izero / 0 /
    data ione / 1 /
    !
    !
    !-------------------------------------------------------------------*
    !
    ! 0. preliminaries
    ! ----------------
    !
    !  initialize local counter
    nerr = 0
    !
    !
    ! 1. array sizes
    ! --------------
    !
    !llee
    idums(1) = llee
    idumo(1) = ncetop
    call alchki( err,3020,mnpr,1,1,iundef,iundef,'llee','GE',idumo,idums,nerr,ldum1)
    !
    !nconee
    idums(1) = nconee
    idumo(1) = ncon
    call alchki( err,3021,mnpr,1,1,iundef,iundef,'nconee','GE',idumo,idums,nerr,ldum1)
    !
    !nelee
    idums(1) = nelee
    !	idumo(1) = max(nel,ns)
    idumo(1) = nel
    call alchki( err,3022,mnpr,1,1,iundef,iundef,'nelee','GE',idumo,idums,nerr,ldum1)
    !
    !nlfee
    idums(1) = nlfee
    idumo(1) = max( 1, nlf )
    call alchki( err,3023,mnpr,1,1,iundef,iundef,'nlfee','GE',idumo,idums,nerr,ldum1 )
    !
    !nlyree
    idums(1) = nlyree
    call alchki( err,3024,mnpr,1,1,iundef,iundef,'nlyree','GT',izero,idums,nerr,ldum1)
    !
    !nsee
    idums(1) = nsee
    idumo(1) = ns
    call alchki( err,3025,mnpr,1,1,iundef,iundef,'nsee','GE',idumo,idums,nerr,ldum1)
    !
    !nvee
    idums(1) = nvee
    idumo(1) = nv
    call alchki( err,3026,mnpr,1,1,iundef,iundef,'nvee','GE',idumo,idums,nerr,ldum1)
    !
    !nxee
    idums(1) = nxee
    idumo(1) = nx
    call alchki( err,3027,mnpr,1,1,iundef,iundef,'nxee','GE',idumo,idums,nerr,ldum1 )
    idumo(1) = 9999
    call alchki( err,3027,mnpr,1,1,iundef,iundef,'nxee','LE',idumo,idums,nerr,ldum1 )
    !
    !nmneee
    idums(1) = nmneee
    call alchki( err,3028,mnpr,1,1,iundef,iundef,'nmneee','GT',izero,idums,nerr,ldum1)
    !
    !nlyree
    idums(1) = nmntee
    call alchki( err,3028,mnpr,1,1,iundef,iundef,'nmntee','GT',izero,idums,nerr,ldum1)
    !
    !
    !
    ! 2. number of entities
    ! ---------------------
    !
    !nlf
    idums(1) = nlf
    idumo(1) = nel
    call alchki( err,3029,mnpr,1,1,iundef,iundef,'nlf','GE',izero,idums,nerr,ldum1)
    call alchki( err,3029,mnpr,1,1,iundef,iundef,'nlf','LT',idumo,idums,nerr,ldum1)
    !
    !ncetop,ns,nv
    idums(1) = min(ncetop,ns,nv)
    call alchki( err,3030,mnpr,1,1,iundef,iundef,'[ncetop,ns,nv]','GT',izero,idums,nerr,ldum1)
    !
    !nx, ny
    idums(1) = min( nx, ny )
    call alchki( err,3031,mnpr,1,1,iundef,iundef,'[ nx, ny ]','GT',izero,idums,nerr,ldum1 )
    !
    !ncon
    idums(1) = ncon
    call alchki( err,3032,mnpr,1,1,iundef,iundef,'ncon','EQ',ione,idums,nerr,ldum1)
    !
    ! 3. unit numbers
    ! ---------------
    !
    ! mnd,mnfc,mnfn,mnpr
    idums(1) = min(mnd,mnfc,mnfn,mnpr)
    call alchki( err,3033,mnpr,1,1,iundef,iundef,'[mnd,mnpr]','GE',izero,idums,nerr,ldum1)
    ! 4. epilogue
    ! -----------
    !
    if (nerr>0) call error( fatal, 3010, mnpr, 0 , 0,'error(s) detectedwhile checking cm-mn interface variables')
    !
end subroutine mnerr0



!> Checks the static contaminant-to-MN interface variables.
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
subroutine mnerr1(llee,mnpr,ncetop,nel,nelee,nlf,nlfee,nlyree,ns,nx,nxee,ny,icmbk,icmref,icmxy,ncolmb,nlyr,nlyrbt &
    ,ntsoil,d0,tih,z2,dxqq,dyqq,vspor,deltaz,zvsnod,bexbk,linkns,dummy2,dummy3,idum,idum1x,ldum,ldum2)

    ! externals
    !use sglobal, only : error
    !       use sglobal
    !use mod_load_filedata ,    only : alchk,alchki
    !       external      alchk,alchki,error
    !
    integer llee                    !! Maximum soil-cell dimension.
    integer mnpr                    !! MN diagnostic output unit.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links.
    integer nlfee                   !! Link-array dimension.
    integer nlyree                  !! Soil-layer array dimension.
    integer ns                      !! Number of soil types.
    integer nx                      !! Number of grid columns.
    integer nxee                    !! Grid-column array dimension.
    integer ny                      !! Number of grid rows.
    integer icmbk(nlfee,2)          !! Bank-element numbers for each channel link.
    integer icmref(nelee,4,2:2)     !! Neighbour reference map used to validate bank adjacency.
    integer icmxy(nxee,ny)          !! Element number at each grid location.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nlyr(nelee)             !! Number of soil layers in each element.
    integer nlyrbt(nel,nlyree)      !! Bottom cell index of each soil layer.
    integer ntsoil(nel,nlyree)      !! Soil type index for each element layer.
    double precision d0             !! Reference diffusion/dispersion scale used by CM.
    double precision tih            !! Initial simulation time in hours.
    double precision z2             !! Vertical length scale used by CM and MN temperature diffusion.
    double precision dxqq(nelee)    !! Element width.
    double precision dyqq(nelee)    !! Element length.
    double precision vspor(ns)      !! Soil porosity by soil type.
    double precision deltaz(llee,nel) !! Cell thickness by cell and element.
    double precision zvsnod(llee,nel) !! Vertical node elevation/depth by cell and element.
    logical bexbk                   !! True when bank elements are represented.
    logical linkns(nlfee)           !! True for north-south channel links.
    integer dummy2(nlyree,nelee)    !! Integer workspace for layer membership checks.
    integer dummy3(nlyree)          !! Integer workspace for layer checks.
    integer idum(nelee)             !! Integer workspace for element accounting.
    integer idum1x(-1:nel+1)        !! Integer workspace for element identity checks.
    logical ldum(nelee)             !! Logical workspace for element accounting.
    logical ldum2(llee)             !! Logical workspace for cell/layer checks.
    ! locals etc.
    integer           fatal, err
    parameter         ( fatal = 1, err = 2 )
    !
    integer          bank,botlyr,count,face
    integer          iadj,icol1,iel,iundef,ix,iy
    integer          link,nce,ncebot,ncol,nelp
    integer          nerr,nlayer,toplyr
    integer          idum1(2),ione(1),izero(1)
    double precision dums(1),one(1),zero(1)
    logical          bkxyok
    !
    data izero,ione/ 0, 1/, zero,one/ 0d0, 1d0/
    !
    !
    !
    !-------------------------------------------------------------------*
    !
    ! 0. preliminaries
    ! ----------------
    !
    !  initialize local counter
    nerr = 0
    !  position of 1st column element
    icol1 = nlf + 1
    !  number of elements plus one
    nelp  = nel + 1
    !
    !
    ! 1. index arrays
    ! ---------------
    !
    !icmbk, icmxy
    count = nerr
    !     * initialize column-element counter & marker array
    ncol = 0
    do iel = 0, nlf
        idum1x(iel) = 1
    enddo
    do iel = icol1, nelp
        idum1x(iel) = 0
    enddo
    !     * count active grid elements and mark them
    do iy = 1, ny
        do ix = 1, nx
            iel         = max( 0, min( icmxy(ix,iy), nelp ) )
            idum1x(iel) = idum1x(iel) + 1
            ncol        = ncol + min( iel, 1 )
        enddo
    enddo
    !     * similarly for bank elements (if present all must be active)
    if (bexbk .and. nlf>0 ) then
        ncol = ncol + 2*nlf
        do bank = 1, 2
            do link = 1, nlf
                iel         = max( 0, min( icmbk(link,bank), nelp ) )
                idum1x(iel) = idum1x(iel) + 1
            enddo
        enddo
    endif
    !     * watch out for gate-crashers
    idum1(1)  = nel - nlf
    idum1x(0) = ncol
    call alchki      ( err,2075,mnpr,    1,1    ,iundef,iundef,'#_column_elements','EQ',idum1             ,idum1x(0), &
    nerr,ldum )
    !     * check that each element has a unique identity
    call alchki      ( err,2076,mnpr,    1,nel  ,iundef,iundef,'element_count(iel)','EQ',ione              ,idum1x(1) &
    ,nerr,ldum )
    !     * was everything ok?
    bkxyok = count == nerr
    !
    !icmref (bank element neighbours)
    if ( nlf>0 .and. bexbk .and. bkxyok ) then
        !        * set marker array (disallow non-grids other than zero)
        idum1x(-1) = -2
        idum1x( 0) =  0
        do iel = 1, nel
            idum1x(iel) = -2
        enddo
        do iy = 1, ny
            do ix = 1, nx
                iel         = max( 0, icmxy(ix,iy) )
                idum1x(iel) = min( iel, 1 )
            enddo
        enddo
        !        * count number of grid neighours for each link
        do link = 1, nlf
            idum(link) = 0
        enddo
        do bank = 1, 2
            do link = 1, nlf
                iel        = icmbk(link,bank)
                face       = 2*bank
                if ( linkns(link) ) face = face - 1
                iadj       = max( -1, icmref(iel,face,2) )
                !               iadj       = max( -1, icmref(iel,face+4) ) this is what is was changed call for icmref
                idum(link) = idum(link) + idum1x(iadj)
            enddo
        enddo
        call alchki   ( err,2079,mnpr,    1,nlf  ,iundef,iundef,'#_grids_neighbouring_banks(link)','GT',izero, idum, &
        nerr,ldum )
    endif
    !
    !
    !
    !
    ! 2. contaminant reference values
    ! -------------------------------
    !
    ! d0
    dums(1) = d0
    call alchk( err,3035,mnpr,1,1,iundef,iundef,'d0','GT',zero,zero(1),dums,nerr,ldum)
    !
    ! z2
    dums(1) = z2
    call alchk( err,3036,mnpr,1,1,iundef,iundef,'z2','GT',zero,zero(1),dums,nerr,ldum)
    !
    !
    ! 3. soil properties
    ! ------------------
    !
    !vspor
    call alchk ( err,3037,mnpr,1,ns,iundef,iundef,'vspor(soil)','LE',one,zero(1),vspor,nerr,ldum)
    call alchk ( err,3037,mnpr,1,ns,iundef,iundef,'vspor(soil)','GT',zero,zero(1),vspor,nerr,ldum)
    !
    !
    ! 4. column properties
    ! --------------------
    !
    !dxqq
    !dxqq passes from icol1 to correspond to subj in alchk
    call alchk ( err,3039,mnpr,icol1,nel,iundef,iundef,'dxqq(iel)','GT',zero,zero(1),dxqq(icol1),nerr,ldum)
    !dyqq
    call alchk ( err,3039,mnpr,icol1,nel,iundef,iundef,'dyqq(iel)','GT',zero,zero(1),dyqq(icol1),nerr,ldum)
    !
    !nlyr
    count = nerr
    idum1(1) = 1
    call alchki ( err,3041,mnpr,icol1,nel,iundef,iundef,'nlyr(iel)','GE',idum1,nlyr(icol1),nerr,ldum)
    idum1(1) = nlyree
    call alchki ( err,3041,mnpr,icol1,nel,iundef,iundef,'nlyr(iel)','LE',idum1,nlyr(icol1),nerr,ldum)
    !
    !nlyrbt
    if (count==nerr) then
        do nlayer = 1,nlyree
            do iel = 1,nel
                dummy2(nlayer,iel) = nlyrbt(iel,nlayer)
            enddo
        enddo
        do iel = icol1,nel
            botlyr = 1
            toplyr = nlyr(iel)
            !           * soil type in the layer below
            dummy3(botlyr) = 0
            do nlayer = botlyr,toplyr
                dummy3(nlayer+1)=dummy2(nlayer,iel)
            enddo
            call alchki ( err,3042,mnpr,botlyr,toplyr+1,iel,iundef,'nlyrbt[nlyr,iel]','GTa',dummy3(botlyr), &
            dummy2(botlyr,iel),nerr,ldum2)
            call alchki ( err,3042,mnpr,toplyr,toplyr,iel,iundef,'nlyrbt[toplyr,iel]','EQ',ncetop+1,dummy2(toplyr+1, &
            iel),nerr,ldum2)
        enddo
    endif
    !
    !ntsoil
    if (count==nerr) then
        do nlayer = 1,nlyree
            do iel = 1,nel
                dummy2(nlayer,iel) = ntsoil(iel,nlayer)
            enddo
        enddo
        do iel = icol1,nel
            botlyr = 1
            toplyr = nlyr(iel)
            call alchki ( err,3043,mnpr,botlyr,toplyr,iel,iundef,'ntsoil[nlyr,iel]','GT',izero,dummy2(botlyr,iel), &
            nerr,ldum2)
            idum1(1) = ns
            call alchki ( err,3043,mnpr,botlyr,toplyr,iel,iundef,'ntsoil[nlyr,iel]','LE',idum1(1),dummy2(botlyr,iel), &
            nerr,ldum2)
        enddo
    endif
    !
    !ncolmb
    idum1(1)=ncetop
    call alchki ( err,3044,mnpr,icol1,nel,iundef,iundef,'ncolmb(iel)','GT',izero,ncolmb(icol1),nerr,ldum)
    call alchki ( err,3044,mnpr,icol1,nel,iundef,iundef,'ncolmb(iel)','LE',idum1,ncolmb(icol1),nerr,ldum)
    !
    !deltz,zvsnod
    do iel = icol1,nel
        do nce = ncolmb(iel),ncetop
            dummy4(nce,iel) = deltaz(nce,iel)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3045,mnpr,ncebot,ncetop,iel,iundef,'deltaz[ncl,iel]','GT',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
    enddo
    do iel = icol1,nel
        do nce = ncolmb(iel),ncetop-1
            dums(1) = zvsnod(nce,iel)
            call alchk ( err,3045,mnpr,nce+1,nce+1,iel,iundef,'zvsnod','GT',dums(1),zero(1),zvsnod(nce+1,iel),nerr, &
            ldum2)
        enddo
    enddo
    !
    !
    ! 5. time properties
    ! ------------------
    !
    ! tih
    call alchk ( err,3046,mnpr,1,1,iundef,iundef,'tih','GE',zero,zero(1),tih,nerr,ldum)
    !
    ! 6. epilogue
    ! -----------
    !
    if (nerr>0) call error(fatal, 3011, mnpr, 0, 0,'error(s) detected while checking static/initial interface')
    !
end subroutine mnerr1



!> Checks static mineral-nitrogen input read by [[mnred1]].
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
!>
!> @note As implemented, the monotonicity check for KD2 table depths validates
!> the first `KD2DTH` entry but then references `KD1DTH` for later entries. The
!> KD2 interpolation path elsewhere uses `KD2DTH`.
!> @endnote
subroutine mnerr2(mnpr,nbotce,ncetop,nel,nelee,nlf,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e &
    ,nmneee,nmntee,ns,celem,kd1elm,kd2elm,khelem,klelem,kmelem,knelem,kvelem,naelem,nmn15t,nmn17t,nmn19t,nmn21t, &
    nmn23t,nmn25t,nmn27t,nmn43t,nmn53t,ammddr,ammwdr,clitfr,cnrbio,cnrhum,cnrlit,fe,fh,gnn,kplamm,kplnit,kuamm,kunit, &
    mncref,nitddr,nitwdr,q10m,q10n,cconc,cdpth,ctottp,damhlf,dchlf,kd1cnc,kd1dth,kd2cnc,kd2dth,kddsol,khconc,khdpth, &
    klconc,kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth,namtop,isiccd,isiamd,ldum)

    ! externals
    !use sglobal, only : error
    !use mod_load_filedata ,    only : alchk,alchki
    !       external      alchk,error
    !
    integer mnpr                    !! MN diagnostic output unit.
    integer nbotce                  !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column checks.
    integer nmn15e                  !! Number of humus category entries.
    integer nmn17e                  !! Number of litter category entries.
    integer nmn19e                  !! Number of manure category entries.
    integer nmn21e                  !! Number of nitrification category entries.
    integer nmn23e                  !! Number of volatilisation category entries.
    integer nmn25e                  !! Number of KD1 denitrification category entries.
    integer nmn27e                  !! Number of KD2 denitrification category entries.
    integer nmn43e                  !! Number of initial-carbon category entries.
    integer nmn53e                  !! Number of initial-ammonium category entries.
    integer nmneee                  !! Maximum number of MN category entries.
    integer nmntee                  !! Maximum number of MN table entries.
    integer ns                      !! Number of soil types.
    integer celem(nlf+1:nel)        !! Initial-carbon category by element.
    integer kd1elm(nlf+1:nel)       !! KD1 denitrification category by element.
    integer kd2elm(nlf+1:nel)       !! KD2 denitrification category by element.
    integer khelem(nlf+1:nel)       !! Humus decomposition category by element.
    integer klelem(nlf+1:nel)       !! Litter decomposition category by element.
    integer kmelem(nlf+1:nel)       !! Manure decomposition category by element.
    integer knelem(nlf+1:nel)       !! Nitrification category by element.
    integer kvelem(nlf+1:nel)       !! Volatilisation category by element.
    integer naelem(nlf+1:nel)       !! Initial-ammonium category by element.
    integer nmn15t(nmneee)          !! Humus table length by category.
    integer nmn17t(nmneee)          !! Litter table length by category.
    integer nmn19t(nmneee)          !! Manure table length by category.
    integer nmn21t(nmneee)          !! Nitrification table length by category.
    integer nmn23t(nmneee)          !! Volatilisation table length by category.
    integer nmn25t(nmneee)          !! KD1 table length by category.
    integer nmn27t(nmneee)          !! KD2 table length by category.
    integer nmn43t(nmneee)          !! Initial-carbon table length by category.
    integer nmn53t(nmneee)          !! Initial-ammonium table length by category.
    double precision ammddr         !! Dry ammonium deposition rate.
    double precision ammwdr         !! Wet ammonium deposition coefficient.
    double precision clitfr         !! Fraction of initial organic carbon assigned to litter.
    double precision cnrbio         !! Biomass carbon-to-nitrogen ratio.
    double precision cnrhum         !! Humus carbon-to-nitrogen ratio.
    double precision cnrlit         !! Litter carbon-to-nitrogen ratio.
    double precision fe             !! Efficiency fraction for organic carbon turnover.
    double precision fh             !! Humification fraction.
    double precision gnn            !! Nonlinear ammonium adsorption exponent.
    double precision kplamm         !! First-order ammonium plant-uptake limit.
    double precision kplnit         !! First-order nitrate plant-uptake limit.
    double precision kuamm          !! First-order ammonium immobilisation limit.
    double precision kunit          !! First-order nitrate immobilisation limit.
    double precision mncref         !! Reference nitrogen concentration.
    double precision nitddr         !! Dry nitrate deposition rate.
    double precision nitwdr         !! Wet nitrate deposition coefficient.
    double precision q10m           !! Q10 coefficient for mineralisation.
    double precision q10n           !! Q10 coefficient for nitrification.
    double precision cconc(nmneee,nmntee)  !! Initial-carbon profile values.
    double precision cdpth(nmneee,nmntee)  !! Initial-carbon profile depths.
    double precision ctottp(nlf+1:nel)     !! Top total-carbon value for decay initialisation.
    double precision damhlf(nlf+1:nel)     !! Ammonium decay half-depth by element.
    double precision dchlf(nlf+1:nel)      !! Carbon decay half-depth by element.
    double precision kd1cnc(nmneee,nmntee) !! KD1 denitrification profile values.
    double precision kd1dth(nmneee,nmntee) !! KD1 denitrification profile depths.
    double precision kd2cnc(nmneee,nmntee) !! KD2 denitrification profile values.
    double precision kd2dth(nmneee,nmntee) !! KD2 denitrification profile depths.
    double precision kddsol(ns)            !! Soil ammonium adsorption coefficient.
    double precision khconc(nmneee,nmntee) !! Humus decomposition profile values.
    double precision khdpth(nmneee,nmntee) !! Humus decomposition profile depths.
    double precision klconc(nmneee,nmntee) !! Litter decomposition profile values.
    double precision kldpth(nmneee,nmntee) !! Litter decomposition profile depths.
    double precision kmconc(nmneee,nmntee) !! Manure decomposition profile values.
    double precision kmdpth(nmneee,nmntee) !! Manure decomposition profile depths.
    double precision knconc(nmneee,nmntee) !! Nitrification profile values.
    double precision kndpth(nmneee,nmntee) !! Nitrification profile depths.
    double precision kvconc(nmneee,nmntee) !! Volatilisation profile values.
    double precision kvdpth(nmneee,nmntee) !! Volatilisation profile depths.
    double precision naconc(nmneee,nmntee) !! Initial-ammonium profile values.
    double precision nadpth(nmneee,nmntee) !! Initial-ammonium profile depths.
    double precision namtop(nlf+1:nel)     !! Top ammonium value for decay initialisation.
    logical isiccd                  !! True when initial carbon uses decay-function input.
    logical isiamd                  !! True when initial ammonium uses decay-function input.
    logical ldum(nelee)             !! Logical workspace for element checks.
    ! locals etc.
    integer          icol1,iundef,nelmty,nerr,ntab
    integer          fatal,err, warn
    integer          izero(1)
    parameter (fatal = 1, err = 2 , warn = 3)
    !
    double precision dtmax,prevdp(1),thetmn
    double precision dums(1),dum(1)
    double precision one(1),zero(1)
    !
    data izero/ 0 /
    data dtmax,thetmn /7.2d3,1.0d-1 /
    data one,zero / 1.0d0, 0d0 /
    !
    !
    !-------------------------------------------------------------------*
    !
    ! 0. preliminaries
    ! ----------------
    !
    !  initialize local counter
    nerr = 0
    !  position of 1st column element
    icol1 = nlf+1
    !
    !
    ! 1. spatially constant decomposition parameters
    ! ---------------------------
    !
    ! kuamm,kplamm
    dums(1) = min( kuamm,kplamm )
    call alchk ( err,3050,mnpr,1,1,iundef,iundef,'[ kuamm,kplamm ]','GE',zero,zero(1),dums,nerr,ldum)
    !
    !
    ! kunit,kplnit
    dums(1) = min( kunit,kplnit )
    call alchk ( err,3050,mnpr,1,1,iundef,iundef,'[ kunit,kplnit ]','GE',zero,zero(1),dums,nerr,ldum)
    !
    !
    ! 2. other parameters
    ! -------------------
    !
    ! fe, fh
    dums(1) = min(fe,fh)
    call alchk ( err,3055,mnpr,1,1,iundef,iundef,'[ fe,fh ]','GE',zero,zero(1),dums,nerr,ldum)
    dums(1) = max(fe,fh)
    call alchk ( err,3055,mnpr,1,1,iundef,iundef,'[ fe,fh ]','LE',one,zero(1),dums,nerr,ldum)
    !
    ! cnrbio,cnrhum
    dums(1) = min(cnrbio,cnrhum)
    call alchk ( err,3056,mnpr,1,1,iundef,iundef,'[ cnrbio,cnrhum ]','GT',zero,zero(1),dums,nerr,ldum)
    !
    ! q10m, q10n
    dums(1) = min(q10m, q10n)
    call alchk ( err,3057,mnpr,1,1,iundef,iundef,'[ q10m, q10n ]','GE',zero,zero(1),dums,nerr,ldum)
    !
    ! ammddr, ammwdr
    dums(1) = min(ammddr, ammwdr)
    call alchk ( err,3058,mnpr,1,1,iundef,iundef,'[ ammddr,ammwdr ]','GE',zero,zero(1),dums,nerr,ldum)
    !
    ! nitddr, nitwdr
    dums(1) = min(nitddr, nitwdr)
    call alchk ( err,3058,mnpr,1,1,iundef,iundef,'[ nitddr, nitwdr ]','GE',zero,zero(1),dums,nerr,ldum)
    !
    ! mncref
    dums(1) = mncref
    call alchk( err,3059,mnpr,1,1,iundef,iundef,'mncref','GT',zero,zero(1),dums,nerr,ldum)
    !
    ! 3. initial concnetrations
    ! -------------------------
    !
    !     * carbon pool
    !     * -----------
    if (isiccd) then
        !
        !       *ctottp
        call alchk ( err,3060,mnpr,icol1,nel,iundef,iundef,'ctottp(iel)','GE',zero,zero(1),ctottp,nerr,ldum)
        !
        !       *dchlf
        call alchk ( err,3061,mnpr,icol1,nel,iundef,iundef,'dchlf(iel)','GT',zero,zero(1),dchlf,nerr,ldum)
        !
    else
        !
        !       *celem
        call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'celem(iel)','GT',izero,celem,nerr,ldum)
        !
        !       *cdpth
        do nelmty = 1,nmn43e
            call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'cdpth[nmne,1]','EQ',zero,zero(1),cdpth(nelmty,1),nerr, &
            ldum)
            do ntab = 2,nmn43t(nelmty)
                prevdp= cdpth(nelmty,ntab-1)
                call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'cdpth[nmne,ntab]','GT',prevdp,zero(1), &
                cdpth(nelmty,ntab), nerr,ldum)
            enddo
        enddo
        !
        !       *cconc
        do nelmty = 1,nmn43e
            do ntab = 1,nmn43t(nelmty)
                call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'cconc[nmne,ntab]','GE',zero,zero(1), &
                cconc(nelmty,ntab), nerr,ldum)
            enddo
        enddo
        !
    endif
    !
    !   * carbon litter fraction and carbon/nitrogen ratio
    !   clitfr
    call alchk ( err,3062,mnpr,1,1,iundef,iundef,'clitfr','GE',zero,zero(1),clitfr,nerr,ldum)
    call alchk ( err,3062,mnpr,1,1,iundef,iundef,'clitfr','LE',one,zero(1),clitfr,nerr,ldum)
    !   cnrlit
    call alchk ( err,3063,mnpr,1,1,iundef,iundef,'cnrlit','GT',zero,zero(1),cnrlit,nerr,ldum)
    !
    !
    !     * ammonium pool
    !     * -------------
    if (isiamd) then
        !
        !       * namtop
        call alchk ( err,3060,mnpr,icol1,nel,iundef,iundef,'namtop(iel)','GE',zero,zero(1),namtop,nerr,ldum)
        !
        !        * depth for half the concentration to be present
        !        * damhlf
        call alchk ( err,3061,mnpr,icol1,nel,iundef,iundef,'damhlf(iel)','GT',zero,zero(1),damhlf,nerr,ldum)
    else
        !
        !       *naelem
        call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'naelem(iel)','GT',izero,naelem,nerr,ldum)
        !
        !       *nadpth
        do nelmty = 1,nmn53e
            call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'nadpth[nmne,1]','EQ',zero,zero(1),nadpth(nelmty,1), &
            nerr,ldum)
            do ntab = 2,nmn53t(nelmty)
                prevdp = nadpth(nelmty,ntab-1)
                call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'nadpth[nmne,ntab]','GT',prevdp,zero(1), &
                nadpth(nelmty,ntab), nerr,ldum)
            enddo
        enddo
        !
        !       *naconc
        do nelmty = 1,nmn53e
            do ntab = 1,nmn53t(nelmty)
                call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'naconc[nmne,ntab]','GE',zero,zero(1), &
                naconc(nelmty,ntab), nerr,ldum)
            enddo
        enddo
        !
    endif
    !
    !
    ! 4. spatially varying parameters
    ! -------------------------------
    ! 4.1 kh
    ! ------
    !    * khelem
    call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'khelem(iel)','GT',izero,khelem,nerr,ldum)
    !
    !       *khdpth
    do nelmty = 1,nmn15e
        call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'khdpth[nmne,1]','EQ',zero,zero(1),khdpth(nelmty,1),nerr,ldum)
        do ntab = 2,nmn15t(nelmty)
            prevdp = khdpth(nelmty,ntab-1)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'khdpth[nmne,ntab]','GT',prevdp,zero(1), &
            khdpth(nelmty,ntab), nerr,ldum)
        enddo
    enddo
    !
    !       *khconc
    do nelmty = 1,nmn15e
        do ntab = 1,nmn15t(nelmty)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'khconc[nmne,ntab]','GE',zero,zero(1),khconc(nelmty, &
            ntab), nerr,ldum)
        enddo
    enddo
    !
    ! 4.2 kl
    ! -------
    !    * klelem
    call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'klelem(iel)','GT',izero,klelem,nerr,ldum)
    !
    !       *kldpth
    do nelmty = 1,nmn17e
        call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'kldpth[nmne,1]','EQ',zero,zero(1),kldpth(nelmty,1),nerr,ldum)
        do ntab = 2,nmn17t(nelmty)
            prevdp = kldpth(nelmty,ntab-1)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kldpth[nmne,ntab]','GT',prevdp,zero(1), &
            kldpth(nelmty,ntab), nerr,ldum)
        enddo
    enddo
    !
    !       *klconc
    do nelmty = 1,nmn17e
        do ntab = 1,nmn17t(nelmty)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'klconc[nmne,ntab]','GE',zero,zero(1),klconc(nelmty, &
            ntab), nerr,ldum)
        enddo
    enddo
    !
    ! 4.3 km
    ! -------
    !    * kmelem
    call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'kmelem(iel)','GT',izero,kmelem,nerr,ldum)
    !
    !       *kmdpth
    do nelmty = 1,nmn19e
        call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'kmdpth[nmne,1]','EQ',zero,zero(1),kmdpth(nelmty,1),nerr,ldum)
        do ntab = 2,nmn19t(nelmty)
            prevdp = kmdpth(nelmty,ntab-1)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kmdpth[nmne,ntab]','GT',prevdp,zero(1), &
            kmdpth(nelmty,ntab), nerr,ldum)
        enddo
    enddo
    !
    !       *kmconc
    do nelmty = 1,nmn19e
        do ntab = 1,nmn19t(nelmty)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kmconc[nmne,ntab]','GE',zero,zero(1),kmconc(nelmty, &
            ntab), nerr,ldum)
        enddo
    enddo
    !
    ! 4.4 kn
    ! -------
    !    * knelem
    call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'knelem(iel)','GT',izero,knelem,nerr,ldum)
    !
    !       *kndpth
    do nelmty = 1,nmn21e
        call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'kndpth[nmne,1]','EQ',zero,zero(1),kndpth(nelmty,1),nerr,ldum)
        do ntab = 2,nmn21t(nelmty)
            prevdp = kndpth(nelmty,ntab-1)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kndpth[nmne,ntab]','GT',prevdp,zero(1), &
            kndpth(nelmty,ntab), nerr,ldum)
        enddo
    enddo
    !
    !       *knconc
    do nelmty = 1,nmn21e
        do ntab = 1,nmn21t(nelmty)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'knconc[nmne,ntab]','GE',zero,zero(1),knconc(nelmty, &
            ntab), nerr,ldum)
        enddo
    enddo
    !
    ! 4.5 kv
    ! -------
    !    * kvelem
    call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'kvelem(iel)','GT',izero,kvelem,nerr,ldum)
    !
    !       *kvdpth
    do nelmty = 1,nmn23e
        call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'kvdpth[nmne,1]','EQ',zero,zero(1),kvdpth(nelmty,1),nerr,ldum)
        do ntab = 2,nmn23t(nelmty)
            prevdp = kvdpth(nelmty,ntab-1)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kvdpth[nmne,ntab]','GT',prevdp,zero(1), &
            kvdpth(nelmty,ntab), nerr,ldum)
        enddo
    enddo
    !
    !       *kvconc
    do nelmty = 1,nmn23e
        do ntab = 1,nmn23t(nelmty)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kvconc[nmne,ntab]','GE',zero,zero(1),kvconc(nelmty, &
            ntab), nerr,ldum)
        enddo
    enddo
    !
    ! 4.6 kd1
    ! -------
    !    * kd1elm
    call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'kd1elm(iel)','GT',izero,kd1elm,nerr,ldum)
    !
    !       *kd1dth
    do nelmty = 1,nmn25e
        call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'kd1dth[nmne,1]','EQ',zero,zero(1),kd1dth(nelmty,1),nerr,ldum)
        do ntab = 2,nmn25t(nelmty)
            prevdp = kd1dth(nelmty,ntab-1)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kd1dth[nmne,ntab]','GT',prevdp,zero(1), &
            kd1dth(nelmty,ntab), nerr,ldum)
        enddo
    enddo
    !
    !       *kd1cnc
    do nelmty = 1,nmn25e
        do ntab = 1,nmn25t(nelmty)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kd1cnc[nmne,ntab]','GE',zero,zero(1),kd1cnc(nelmty, &
            ntab), nerr,ldum)
        enddo
    enddo
    !
    ! 4.7 kd2
    ! -------
    !    * kd2elm
    call alchki ( err,3064,mnpr,icol1,nel,iundef,iundef,'kd2elm(iel)','GT',izero,kd2elm,nerr,ldum)
    !
    !       *kd2dth
    do nelmty = 1,nmn27e
        call alchk ( err,3064,mnpr,nelmty,nelmty,1,iundef,'kd2dth[nmne,1]','EQ',zero,zero(1),kd2dth(nelmty,1),nerr,ldum)
        do ntab = 2,nmn27t(nelmty)
            prevdp = kd1dth(nelmty,ntab-1)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kd1dth[nmne,ntab]','GT',prevdp,zero(1), &
            kd1dth(nelmty,ntab), nerr,ldum)
        enddo
    enddo
    !
    !       *kd2cnc
    do nelmty = 1,nmn27e
        do ntab = 1,nmn27t(nelmty)
            call alchk ( err,3064,mnpr,nelmty,nelmty,ntab,iundef,'kd2cnc[nmne,ntab]','GE',zero,zero(1),kd2cnc(nelmty, &
            ntab), nerr,ldum)
        enddo
    enddo
    !
    ! 5. ammonium adsorption parameters
    ! ---------------------------------
    !     * kddsol
    call alchk ( err,3048,mnpr,1,ns,iundef,iundef,'kddsol(ns)','GE',zero,zero(1),kddsol,nerr,ldum)
    !
    ! 6. bottom cell for nitrogen transformations
    ! -------------------------------------------
    !     * nbotce
    call alchki( err,3049,mnpr,1,1,iundef,iundef,'nbotce','LT',ncetop,nbotce,nerr,ldum)
    !
    ! 7. epilogue
    ! -----------
    !
    if (nerr>0) call error(fatal,3012,mnpr,0,0,'error(s) detected whilst checking the static input data')
    !
    !
end subroutine mnerr2



!> Checks time-dependent MN inputs and updated state variables.
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
subroutine mnerr3(llee,mnpr,ncetop,nel,nelee,nlf,ncolmb,dtuz,uznow,cccc, &
    pnetto,ssss,vsthe,vstheo,ldum,ldum2 )

    ! externals
    !use sglobal, only : error
    !use mod_load_filedata ,    only : alchk
    !       external      alchk,error
    !
    integer llee                    !! Maximum soil-cell dimension.
    integer mnpr                    !! MN diagnostic output unit.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column checks.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision uznow          !! Current unsaturated-zone simulation time.
    double precision cccc(nel,ncetop+1) !! Dynamic-region nitrate concentration.
    double precision pnetto(nelee)  !! Net precipitation/effective rainfall by element.
    double precision ssss(nel,ncetop+1) !! Dead-space nitrate concentration.
    double precision vsthe(ncetop,nel)  !! Current volumetric water content.
    double precision vstheo(nel,ncetop+1) !! Previous volumetric water content.
    logical ldum(nelee)             !! Logical workspace for element checks.
    logical ldum2(llee)             !! Logical workspace for cell checks.
    ! locals etc.
    integer          fatal,err
    parameter (fatal = 1, err = 2 )
    !
    integer          icol1,iel,iundef,ncebot,nerr,pass,nce
    double precision zero(1),one(1),thirty(1)
    double precision dums(1),uzprev(1)
    !
    save pass,uzprev
    !
    data zero,one,thirty / 0d0, 1.0d0, 30.0d0 /
    data pass /0 /
    !
    !
    !-------------------------------------------------------------------*
    !
    ! 0. preliminaries
    ! ----------------
    !
    !  initialize local counter
    nerr = 0
    !  1st column element
    icol1 = nlf + 1
    !
    pass = pass + 1
    !
    ! 1. variables
    ! ------------
    !
    ! dtuz
    dums(1) = dtuz
    call alchk ( err,3065,mnpr,1,1,iundef,iundef,'dtuz','GT',zero,zero(1),dums,nerr,ldum)
    !
    ! uznow
    if (pass==1) then
        call alchk ( err,3066,mnpr,1,1,iundef,iundef,'uznow','GE',zero,zero(1),uznow,nerr,ldum)
        uzprev(1) = uznow
    else
        ! temporarily remove this sb 240925 as it is not compiling
        !          call alchk ( err,3066,mnpr,1,1,iundef,iundef,
        !     $    'uznow','gt',uzprev(1),zero(1),uznow,nerr,ldum)
        uzprev(1) = uznow
    endif
    !
    ! 2. nitrate concentrations
    ! -------------------------
    !
    ! cccc, ssss
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = cccc(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3067,mnpr,ncebot,ncetop,iel,iundef,'cccc[iel,ncl]','GE',zero,zero(1),dummy4(ncebot,iel),nerr &
        ,ldum2)
    enddo
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = ssss(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3067,mnpr,ncebot,ncetop,iel,iundef,'ssss[iel,ncl]','GE',zero,zero(1),dummy4(ncebot,iel),nerr &
        ,ldum2)
    enddo
    !
    !
    ! 3. organic and inorganic pools
    ! ------------------------------
    !
    ! chum1
    !do iel = 1,nelee
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = chum1(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3068,mnpr,ncebot,ncetop,iel,iundef,'chum1[ncl,iel]','GE',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
    enddo
    !
    ! clit1
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = clit1(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3068,mnpr,ncebot,ncetop,iel,iundef,'clit1[ncl,iel]','GE',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
    enddo
    !
    ! cman1
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = cman1(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3068,mnpr,ncebot,ncetop,iel,iundef,'cman1[ncl,iel]','GE',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
    enddo
    !
    ! nlit1
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = nlit1(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3068,mnpr,ncebot,ncetop,iel,iundef,'nlit1[ncl,iel]','GE',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
    enddo
    !
    ! nman1
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = nman1(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3068,mnpr,ncebot,ncetop,iel,iundef,'nman1[ncl,iel]','GE',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
    enddo
    !
    ! namm1
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = namm1(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3069,mnpr,ncebot,ncetop,iel,iundef,'namm1[ncl,iel]','GE',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
    enddo
    !
    !
    ! 4. soil conditions
    ! ------------------
    !
    !
    ! vsthe
    ! sb 250925  vsthe is now dynamically allocated so nelee is changed to nel
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = vsthe(nce,iel)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3070,mnpr,ncebot,ncetop,iel,iundef,'vsthe[ncl,iel]','GT',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
        call alchk ( err,3070,mnpr,ncebot,ncetop,iel,iundef,'vsthe[ncl,iel]','LE',one,zero(1),dummy4(ncebot,iel),nerr &
        ,ldum2)
    enddo
    !
    ! vstheo
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = vstheo(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3070,mnpr,ncebot,ncetop,iel,iundef,'vstheo[ncl,iel]','GT',zero,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
        call alchk ( err,3070,mnpr,ncebot,ncetop,iel,iundef,'vstheo[ncl,iel]','LE',one,zero(1),dummy4(ncebot,iel), &
        nerr,ldum2)
    enddo
    !
    ! plup
    do iel = 1,nel
        do nce = 1,ncetop
            dummy4(nce,iel) = plup(iel,nce)
        enddo
    enddo
    do iel = icol1,nel
        ncebot = ncolmb(iel)
        call alchk ( err,3071,mnpr,ncebot,ncetop,iel,iundef,'plup[ncl,iel]','GE',zero,zero(1),dummy4(ncebot,iel),nerr &
        ,ldum2)
    enddo
    !
    !
    ! 5. envoironmental conditions
    ! ----------------------------
    !
    ! pnetto
    call alchk ( err,3072,mnpr,icol1,nel,iundef,iundef,'pnetto(iel)','GE',zero,zero(1),pnetto(icol1),nerr,ldum)
    !
    ! 6. epilogue
    ! -----------
    !
    if (nerr>0) call error(fatal,3013,mnpr,0,0, &
    'error(s) detected whilst checking the time dependent'//' variables from cm -mn interface')
    !
    !
end subroutine mnerr3



!> Checks time-varying fertiliser and organic addition data from [[mnred2]].
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
subroutine mnerr4 ( mnpr,nel,nelee,nlf,cdpthb,cltfct,cmnfct,cnral,cnram,ctot,namfct,ndpthb,ntot,isaddc,isaddn, &
    dummy,ldum )

    ! externals
    !use sglobal, only : error
    !use mod_load_filedata ,    only : alchk
    !       external      alchk,error
    !
    !
    integer mnpr                    !! MN diagnostic output unit.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column checks.
    double precision cdpthb(nlf+1:nel) !! Carbon banding depth.
    double precision cltfct(nlf+1:nel) !! Litter fraction of added carbon.
    double precision cmnfct(nlf+1:nel) !! Manure fraction of added carbon.
    double precision cnral(nlf+1:nel)  !! Carbon-to-nitrogen ratio for added litter.
    double precision cnram(nlf+1:nel)  !! Carbon-to-nitrogen ratio for added manure.
    double precision ctot(nlf+1:nel)   !! Total external carbon addition.
    double precision namfct(nlf+1:nel) !! Ammonium fraction of added inorganic nitrogen.
    double precision ndpthb(nlf+1:nel) !! Nitrogen banding depth.
    double precision ntot(nlf+1:nel)   !! Total external inorganic nitrogen addition.
    logical isaddc                  !! True when a carbon-addition event is active.
    logical isaddn                  !! True when a nitrogen-addition event is active.
    double precision dummy(nelee)   !! Floating-point workspace for range checks.
    logical ldum(nelee)             !! Logical workspace for range checks.
    ! locals etc.
    integer          fatal,err
    parameter (fatal = 1, err = 2 )
    integer          icol1,iel,iundef,nerr
    double precision one(1),zero(1)
    !
    data one,zero / 1.0d0, 0d0 /
    !
    !-------------------------------------------------------------------*
    !
    ! 0. preliminaries
    ! ----------------
    !
    !  initialize local counter
    nerr = 0
    !  position of 1st column element
    icol1 = nlf+1
    !
    !
    ! 1. inorganic fertilizer
    ! -----------------------
    !
    if (isaddn) then
        !
        ! ntot
        call alchk ( err,3080,mnpr,icol1,nel,iundef,iundef,'ntot(iel)','GE',zero,zero(1),ntot,nerr,ldum)
        !
        ! namfct
        call alchk ( err,3081,mnpr,icol1,nel,iundef,iundef,'namfct(iel)','GE',zero,zero(1),namfct,nerr,ldum)
        call alchk ( err,3081,mnpr,icol1,nel,iundef,iundef,'namfct(iel)','LE',one,zero(1),namfct,nerr,ldum)
        !
        ! ndpthb
        call alchk ( err,3082,mnpr,icol1,nel,iundef,iundef,'ndpthb(iel)','GE',zero,zero(1),ndpthb,nerr,ldum)
        !
    endif
    !
    ! 2. organic fertilizer
    ! -----------------------
    !
    if (isaddc) then
        !
        ! ctot
        call alchk ( err,3083,mnpr,icol1,nel,iundef,iundef,'ctot(iel)','GE',zero,zero(1),ctot,nerr,ldum)
        !
        ! cdpthb
        call alchk ( err,3084,mnpr,icol1,nel,iundef,iundef,'cdpthb(iel)','GE',zero,zero(1),cdpthb,nerr,ldum)
        !
        ! cltfct
        call alchk ( err,3085,mnpr,icol1,nel,iundef,iundef,'cltfct(iel)','GE',zero,zero(1),cltfct,nerr,ldum)
        !
        ! cmnfct
        call alchk ( err,3085,mnpr,icol1,nel,iundef,iundef,'cmnfct(iel)','GE',zero,zero(1),cmnfct,nerr,ldum)
        !
        ! cmnfct + cltfct
        do iel = icol1,nel
            dummy(iel) = cltfct(iel) + cmnfct(iel)
        enddo
        call alchk ( err,3086,mnpr,icol1,nel,iundef,iundef,'cltfct+cmnfct(iel)','LE',one,zero(1),dummy(icol1),nerr,ldum)
        !
        ! cnral,cnram
        do iel = icol1,nel
            if (ctot(iel)>0.0d0) then
                call alchk ( err,3087,mnpr,iel,iel,iundef,iundef,'cnral(iel)','GT',zero,zero(1),cnral(iel),nerr,ldum)
                call alchk ( err,3087,mnpr,iel,iel,iundef,iundef,'cnram(iel)','GT',zero,zero(1),cnram(iel),nerr,ldum)
            endif
        enddo
        !
    endif
    ! 3. epilogue
    ! -----------
    !
    !
    if (nerr>0) call error(fatal,3014,mnpr,0,0, &
    'error(s) detected whilst checking the time dependent'//' fertilizer input variables')
    !
    !
end subroutine mnerr4



!> Calculates net mineralisation or immobilisation for each active soil cell.
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
subroutine mngam (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,cnrhum,cnrbio,fe,fh,dtuz, &
    isbotc )

    integer llee              !! Maximum soil-cell dimension.
    integer nbotce            !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop            !! Top soil-cell index.
    integer nel               !! Number of elements.
    integer nelee             !! Element-array dimension.
    integer nlf               !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)     !! Lowest active soil cell in each land-column element.
    double precision cnrbio   !! Biomass carbon-to-nitrogen ratio.
    double precision cnrhum   !! Humus carbon-to-nitrogen ratio.
    double precision fe       !! Efficiency fraction for organic carbon turnover.
    double precision fh       !! Humification fraction.
    double precision dtuz     !! Unsaturated-zone timestep in seconds.
    logical isbotc            !! True when the fixed lower active cell `NBOTCE` is used.
    ! locals
    integer nbotm,nelm,ncl
    double precision chumh,clith,cmanh,dum,dum1,erf
    double precision klittp,kmantp,nlith,nmanh
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
            chumh = ( chum(nelm,ncl) + chum1(nelm,ncl) )/2.0d0
            clith = ( clit(nelm,ncl) + clit1(nelm,ncl) )/2.0d0
            cmanh = ( cman(nelm,ncl) + cman1(nelm,ncl) )/2.0d0
            nlith = ( nlit(nelm,ncl) + nlit1(nelm,ncl) )/2.0d0
            nmanh = ( nman(nelm,ncl) + nman1(nelm,ncl) )/2.0d0
            !
            !           * if immobilisation is not equal to the potential
            !           * immobilisation then the decomposition of the litter pool
            !           * and the manure pool are temporarily stopped
            if (isimtf(nelm,ncl)) then
                klittp=0.0d0
                kmantp=0.0d0
            else
                klittp=klit(nelm,ncl)
                kmantp=kman(nelm,ncl)
            endif
            erf = emt(nelm,ncl)*emph(nelm,ncl)
            dum = klittp*erf* (nlith- clith*(1.0-fe)*fh/cnrhum- clith*fe/cnrbio)
            dum1 = dum+ khum(nelm,ncl)*erf*chumh*(1.0d0/cnrhum-fe/cnrbio)
            gam(nelm,ncl) = dum1+ kmantp*erf*(nmanh-fe*cmanh/cnrbio)
            !           * if potential immobilisation is greater than actual
            !           * immobilisation checks how much mineralisation has
            !           * compensated for the difference
            gamtmp(nelm,ncl) = gam(nelm,ncl)
            if (isimtf(nelm,ncl)) then
                if (gam(nelm,ncl)*dtuz>=imdiff(nelm,ncl)) then
                    gam(nelm,ncl) =(gam(nelm,ncl)*dtuz-imdiff(nelm,ncl))/dtuz
                    imdiff(nelm,ncl) = 0.0d0
                    isimtf(nelm,ncl) = .false.
                else
                    imdiff(nelm,ncl) =imdiff(nelm,ncl) - gam(nelm,ncl)*dtuz
                    gam(nelm,ncl) = 0.0d0
                endif
            endif
            !
        enddo
    enddo
end subroutine mngam



!> Initialises MN pools, parameters, and source/sink terms.
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
subroutine mninit(llee,nbotce,ncetop,nel,nelee,nlf,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e &
    ,nmneee,nmntee,celem,kd1elm,kd2elm,khelem,klelem,kmelem,knelem,kvelem,naelem,ncolmb,nmn15t,nmn17t,nmn19t,nmn21t, &
    nmn23t,nmn25t,nmn27t,nmn43t,nmn53t,clitfr,cnrlit,cconc,cdpth,ctottp,damhlf,dchlf,deltaz,kd1cnc,kd1dth,kd2cnc, &
    kd2dth,khconc,khdpth,klconc,kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth,namtop,zvsnod,isiccd, &
    isiamd,sss1,sss2,isbotc)

    ! externals
    !use mod_load_filedata ,    only : alintp
    !       external alintp
    !
    integer llee                    !! Maximum soil-cell dimension.
    integer nbotce                  !! Requested lower active cell for nitrogen transformations.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column updates.
    integer nmn15e                  !! Number of humus category entries.
    integer nmn17e                  !! Number of litter category entries.
    integer nmn19e                  !! Number of manure category entries.
    integer nmn21e                  !! Number of nitrification category entries.
    integer nmn23e                  !! Number of volatilisation category entries.
    integer nmn25e                  !! Number of KD1 denitrification category entries.
    integer nmn27e                  !! Number of KD2 denitrification category entries.
    integer nmn43e                  !! Number of initial-carbon category entries.
    integer nmn53e                  !! Number of initial-ammonium category entries.
    integer nmneee                  !! Maximum number of MN category entries.
    integer nmntee                  !! Maximum number of MN table entries.
    integer celem(nlf+1:nel)        !! Initial-carbon category by element.
    integer kd1elm(nlf+1:nel)       !! KD1 denitrification category by element.
    integer kd2elm(nlf+1:nel)       !! KD2 denitrification category by element.
    integer khelem(nlf+1:nel)       !! Humus decomposition category by element.
    integer klelem(nlf+1:nel)       !! Litter decomposition category by element.
    integer kmelem(nlf+1:nel)       !! Manure decomposition category by element.
    integer knelem(nlf+1:nel)       !! Nitrification category by element.
    integer kvelem(nlf+1:nel)       !! Volatilisation category by element.
    integer naelem(nlf+1:nel)       !! Initial-ammonium category by element.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nmn15t(nmneee)          !! Humus table length by category.
    integer nmn17t(nmneee)          !! Litter table length by category.
    integer nmn19t(nmneee)          !! Manure table length by category.
    integer nmn21t(nmneee)          !! Nitrification table length by category.
    integer nmn23t(nmneee)          !! Volatilisation table length by category.
    integer nmn25t(nmneee)          !! KD1 table length by category.
    integer nmn27t(nmneee)          !! KD2 table length by category.
    integer nmn43t(nmneee)          !! Initial-carbon table length by category.
    integer nmn53t(nmneee)          !! Initial-ammonium table length by category.
    double precision clitfr         !! Fraction of initial organic carbon assigned to litter.
    double precision cnrlit         !! Initial litter carbon-to-nitrogen ratio.
    double precision cconc(nmneee,nmntee)  !! Initial-carbon profile values.
    double precision cdpth(nmneee,nmntee)  !! Initial-carbon profile depths.
    double precision ctottp(nlf+1:nel)     !! Top total-carbon value for decay initialisation.
    double precision damhlf(nlf+1:nel)     !! Ammonium decay half-depth by element.
    double precision dchlf(nlf+1:nel)      !! Carbon decay half-depth by element.
    double precision deltaz(llee,nel)      !! Cell thickness by cell and element.
    double precision kd1cnc(nmneee,nmntee) !! KD1 denitrification profile values.
    double precision kd1dth(nmneee,nmntee) !! KD1 denitrification profile depths.
    double precision kd2cnc(nmneee,nmntee) !! KD2 denitrification profile values.
    double precision kd2dth(nmneee,nmntee) !! KD2 denitrification profile depths.
    double precision khconc(nmneee,nmntee) !! Humus decomposition profile values.
    double precision khdpth(nmneee,nmntee) !! Humus decomposition profile depths.
    double precision klconc(nmneee,nmntee) !! Litter decomposition profile values.
    double precision kldpth(nmneee,nmntee) !! Litter decomposition profile depths.
    double precision kmconc(nmneee,nmntee) !! Manure decomposition profile values.
    double precision kmdpth(nmneee,nmntee) !! Manure decomposition profile depths.
    double precision knconc(nmneee,nmntee) !! Nitrification profile values.
    double precision kndpth(nmneee,nmntee) !! Nitrification profile depths.
    double precision kvconc(nmneee,nmntee) !! Volatilisation profile values.
    double precision kvdpth(nmneee,nmntee) !! Volatilisation profile depths.
    double precision naconc(nmneee,nmntee) !! Initial-ammonium profile values.
    double precision nadpth(nmneee,nmntee) !! Initial-ammonium profile depths.
    double precision namtop(nlf+1:nel)     !! Top ammonium value for decay initialisation.
    double precision zvsnod(llee,nel)      !! Vertical node elevation/depth by cell and element.
    logical isiccd                  !! True when initial carbon uses decay-function input.
    logical isiamd                  !! True when initial ammonium uses decay-function input.
    double precision sss1(nel,ncetop+1) !! Dynamic-region CM source/sink array reset by this routine.
    double precision sss2(nel,ncetop+1) !! Dead-space CM source/sink array reset by this routine.
    logical isbotc                  !! True when `NBOTCE` is valid for all land columns.
    ! locals etc.
    integer ncl,nelm
    double precision ctot,depth
    !
    !
    !-------------------------------------------------------------------*
    !
    do nelm = nlf+1,nel
        do ncl = ncolmb(nelm),ncetop
            !
            imdiff(nelm,ncl) = 0.0d0
            isimtf(nelm,ncl) = (.false.)
            !
        enddo
    enddo
    !
    !
    !     * calculation of the initial conc. in the carbon pools
    !     * ----------------------------------------------------
    if (isiccd) then
        !
        !       * an exponential decay rate down the column is used
        do nelm = nlf + 1, nel
            do ncl = ncetop,ncolmb(nelm),-1
                if (ncl==ncetop) then
                    depth =  deltaz(ncetop,nelm)/2.0d0
                else
                    depth = depth + (zvsnod(ncl+1,nelm) - zvsnod(ncl,nelm))
                endif
                !     * concentration in the organic pools, the manure pool is set to 0
                ctot = ctottp(nelm)* exp(-0.693*depth/dchlf(nelm))
                clit1(nelm,ncl) = ctot * clitfr
                chum1(nelm,ncl) = ctot * ( 1.0d0 - clitfr )
                nlit1(nelm,ncl) = clit1(nelm,ncl)/cnrlit
                cman1(nelm,ncl) = 0.0d0
                nman1(nelm,ncl) = 0.0d0
            enddo
        enddo
        !
        !
        !     * typical columns are used with linear interpolation between
        !     * table values
    else
        call alintp(llee,ncetop,nel,nelee,nlf,nmn43e,nmneee,nmntee,celem,ncolmb(nlf+1),nmn43t,cconc,cdpth,deltaz, &
        zvsnod,dummy6)
        do nelm = nlf+1,nel
            do ncl = ncolmb(nelm),ncetop
                clit1(nelm,ncl) = clitfr*dummy6(nelm,ncl)
                chum1(nelm,ncl) = (1.0d0-clitfr)*dummy6(nelm,ncl)
                cman1(nelm,ncl) = 0.0d0
                nlit1(nelm,ncl) = clit1(nelm,ncl)/cnrlit
                nman1(nelm,ncl) = 0.0d0
            enddo
        enddo
    endif
    !
    !
    !     * calculation of the initial conc. in the ammonium pool
    !     * ----------------------------------------------------
    if (isiamd) then
        !
        do nelm = nlf + 1, nel
            do ncl = ncetop,ncolmb(nelm),-1
                if (ncl==ncetop) then
                    depth =  deltaz(ncetop,nelm)/2.0d0
                else
                    depth = depth + (zvsnod(ncl+1,nelm) - zvsnod(ncl,nelm))
                endif
                !
                namm1(nelm,ncl)=namtop(nelm)* exp(-0.693*depth/damhlf(nelm))
                !
            enddo
        enddo
        !
        !
        !     * typical columns are used with linear interpolation between
        !     * table values
    else
        call alintp(llee,ncetop,nel,nelee,nlf,nmn53e,nmneee,nmntee,naelem,ncolmb(nlf+1),nmn53t,naconc,nadpth,deltaz, &
        zvsnod,namm1)
    endif
    !
    !
    !     * calculation of the initial values for the decomposition params
    !     * --------------------------------------------------------------
    !
    !     * khum
    call alintp(llee,ncetop,nel,nelee,nlf,nmn15e,nmneee,nmntee,khelem,ncolmb(nlf+1),nmn15t,khconc,khdpth,deltaz, &
    zvsnod,khum)
    !
    !     * klit
    call alintp(llee,ncetop,nel,nelee,nlf,nmn17e,nmneee,nmntee,klelem,ncolmb(nlf+1),nmn17t,klconc,kldpth,deltaz, &
    zvsnod,klit)
    !
    !     * kman
    call alintp(llee,ncetop,nel,nelee,nlf,nmn19e,nmneee,nmntee,kmelem,ncolmb(nlf+1),nmn19t,kmconc,kmdpth,deltaz, &
    zvsnod,kman)
    !
    !     * knit
    call alintp(llee,ncetop,nel,nelee,nlf,nmn21e,nmneee,nmntee,knelem,ncolmb(nlf+1),nmn21t,knconc,kndpth,deltaz, &
    zvsnod,knit)
    !
    !     * kvol
    call alintp(llee,ncetop,nel,nelee,nlf,nmn23e,nmneee,nmntee,kvelem,ncolmb(nlf+1),nmn23t,kvconc,kvdpth,deltaz, &
    zvsnod,kvol)
    !
    !     * kd1
    call alintp(llee,ncetop,nel,nelee,nlf,nmn25e,nmneee,nmntee,kd1elm,ncolmb(nlf+1),nmn25t,kd1cnc,kd1dth,deltaz, &
    zvsnod,kd1)
    !
    !     * kd2
    call alintp(llee,ncetop,nel,nelee,nlf,nmn27e,nmneee,nmntee,kd2elm,ncolmb(nlf+1),nmn27t,kd2cnc,kd2dth,deltaz, &
    zvsnod,kd2)
    !
    !
    !     * calculation of whether the specified bottom cell is greater
    !     * than the bottom cell in any of the soil columns. if this is
    !     * the case isbotc is true
    isbotc = .true.
    do nelm = nlf+1,nel
        if (nbotce<ncolmb(nelm)) then
            isbotc = .false.
        endif
    enddo
    !
    !     * set the source/sink terms to zero
    do nelm = nlf+1,nel
        do ncl = ncolmb(nelm),ncetop
            sss1(nelm,ncl) = 0.0
            sss2(nelm,ncl) = 0.0
        enddo
    enddo
    !
end subroutine mninit



!> Converts time-varying MN inputs into cell-based process rates.
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
subroutine mnint2 ( llee,ncetop,nel,nelee,nlf,nlyree,ncolmb,nlyr,nlyrbt,ntsoil,ammddr,ammwdr,mncref,nitddr,nitwdr &
    ,deltaz,dtuz,cccc,cdpthb,cltfct,cmnfct,cnral,cnram,ctot,namfct,ndpthb,ntot, &
    pnetto,ssss,vsthe,isaddc,isaddn,cnralt,cnramn, &
    dummy)
    ! externals
    !       external         phi
    integer llee                    !! Maximum soil-cell dimension.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column updates.
    integer nlyree                  !! Soil-layer array dimension.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nlyr(nelee)             !! Number of soil layers in each element.
    integer nlyrbt(nel,nlyree)      !! Bottom cell index of each soil layer.
    integer ntsoil(nel,nlyree)      !! Soil type index for each element layer.
    double precision ammddr         !! Dry ammonium deposition rate.
    double precision ammwdr         !! Wet ammonium deposition coefficient.
    double precision mncref         !! Reference nitrogen concentration.
    double precision nitddr         !! Dry nitrate deposition rate.
    double precision nitwdr         !! Wet nitrate deposition coefficient.
    double precision deltaz(llee,nel) !! Cell thickness by cell and element.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision cccc(nel,ncetop+1) !! Dynamic-region nitrate concentration.
    double precision cdpthb(nlf+1:nel) !! Carbon banding depth.
    double precision cltfct(nlf+1:nel) !! Litter fraction of added carbon.
    double precision cmnfct(nlf+1:nel) !! Manure fraction of added carbon.
    double precision cnral(nlf+1:nel)  !! Carbon-to-nitrogen ratio for added litter.
    double precision cnram(nlf+1:nel)  !! Carbon-to-nitrogen ratio for added manure.
    double precision ctot(nlf+1:nel)   !! Total external carbon addition.
    double precision namfct(nlf+1:nel) !! Ammonium fraction of added inorganic nitrogen.
    double precision ndpthb(nlf+1:nel) !! Nitrogen banding depth.
    double precision ntot(nlf+1:nel)   !! Total external inorganic nitrogen addition.
    double precision pnetto(nelee)     !! Net precipitation/effective rainfall by element.
    double precision ssss(nel,ncetop+1) !! Dead-space nitrate concentration.
    double precision vsthe(ncetop,nel)  !! Current volumetric water content.
    logical isaddc                  !! True when a carbon-addition event is active.
    logical isaddn                  !! True when a nitrogen-addition event is active.
    double precision cnralt(nelee)  !! Element litter C:N ratio for active additions.
    double precision cnramn(nelee)  !! Element manure C:N ratio for active additions.
    double precision dummy(nelee)   !! Floating-point workspace.
    ! locals etc.
    integer jlyr,jsoil,ncebot,nce,ncl,nelm
    double precision fracdp,ksptot
    !
    !
    !-------------------------------------------------------------------*
    !
    ! 1. set old concentrations to new values
    ! ---------------------------------------
    !
    do nelm = nlf+1,nel
        !
        do ncl = ncolmb(nelm),ncetop
            cman(nelm,ncl) = cman1(nelm,ncl)
            nman(nelm,ncl) = nman1(nelm,ncl)
            clit(nelm,ncl) = clit1(nelm,ncl)
            chum(nelm,ncl) = chum1(nelm,ncl)
            nlit(nelm,ncl) = nlit1(nelm,ncl)
            namm(nelm,ncl) = namm1(nelm,ncl)
        enddo
        !
        !
        ! 2. calculate the effective rain on the ground saurface in mm s-1
        ! ----------------------------------------------------------------
        !
        !
        dummy(nelm) = pnetto(nelm)*1.0d3
        !
        !
        ! 3. convert nitrate concentrations from non dimensional units
        ! ------------------------------------------------------------
        !
        do ncl = ncolmb(nelm),ncetop
            ndnit(nelm,ncl) = cccc(nelm,ncl)*mncref
            ndsnt(nelm,ncl) = ssss(nelm,ncl)*mncref
        enddo
        !
        !
        ! 4. calculation of the mobile fraction for every element in every cell
        ! ---------------------------------------------------------------------
        !
        ncebot = ncolmb(nelm)
        do jlyr = 1,nlyr(nelm)
            jsoil = ntsoil(nelm,jlyr)
            do ncl =max(ncebot,nlyrbt(nelm,jlyr)),nlyrbt(nelm,jlyr+1)-1
                !               pphi(nelm,ncl) = phi(jsoil,vsthe(ncl,nelm))
                ! sb 240925 set value to 0.5 (which is the value set in cmmod.f90 in function phi
                pphi(nelm,ncl) = 0.500
            enddo
        enddo
        !
    enddo
    !
    !
    ! 5. addition of nitrate and ammonium for each element in each cell
    ! -----------------------------------------------------------------
    !
    if (isaddn) then
        !
        do nelm = nlf+1,nel
            !
            if (ntot(nelm)>0.0d0) then
                !
                !            * there is no banding of the input and only the top cell
                !            * receives fertiliser
                if (ndpthb(nelm)==0.0d0) then
                    naamm(nelm,ncetop) = ntot(nelm)*namfct(nelm)/(deltaz(ncetop,nelm)*dtuz)
                    nanit(nelm,ncetop) = ntot(nelm)*(1-namfct(nelm))/(deltaz(ncetop,nelm)*dtuz)
                    do nce = ncolmb(nelm),ncetop-1
                        naamm(nelm,nce) = 0.0d0
                        nanit(nelm,nce) = 0.0d0
                    enddo
                    !
                    !            * there is banding of the input
                else
                    ksptot = 0.0d0
                    do nce = ncetop,ncolmb(nelm),-1
                        ksptot = ksptot + deltaz(nce,nelm)
                        !                  * the banding depth is to below this elememt
                        if (ksptot<=ndpthb(nelm)) then
                            naamm(nelm,nce) = ntot(nelm)*namfct(nelm)/(ndpthb(nelm)*dtuz)
                            nanit(nelm,nce) = ntot(nelm)*(1-namfct(nelm))/(ndpthb(nelm)*dtuz)
                            !                  * the banding depth is to within this element
                        elseif((ksptot-deltaz(nce,nelm))<=ndpthb(nelm)) then
                            fracdp = (ndpthb(nelm)-ksptot+deltaz(nce,nelm))/ndpthb(nelm)
                            naamm(nelm,nce) = ntot(nelm)*namfct(nelm)*fracdp/(deltaz(nce,nelm)*dtuz)
                            nanit(nelm,nce) =ntot(nelm)*(1-namfct(nelm))*fracdp/(deltaz(nce,nelm)*dtuz)
                            !                  * the depth of the element is below the banding depth
                        else
                            naamm(nelm,nce) = 0.0d0
                            nanit(nelm,nce) = 0.0d0
                        endif
                    enddo
                endif
                !
            else
                do nce = ncolmb(nelm),ncetop
                    naamm(nelm,nce) = 0.0d0
                    nanit(nelm,nce) = 0.0d0
                enddo
            endif
        enddo
        !
    else
        do nelm = nlf+1,nel
            do nce = ncolmb(nelm),ncetop
                naamm(nelm,nce) = 0.0d0
                nanit(nelm,nce) = 0.0d0
            enddo
        enddo
    endif
    !
    !
    ! 6. addition of organic matter for each element in each cell
    ! -----------------------------------------------------------
    !
    if (isaddc) then
        !
        do nelm = nlf+1,nel
            !
            if (ctot(nelm)>0.0d0) then
                cnralt(nelm) = cnral(nelm)
                cnramn(nelm) = cnram(nelm)
                !
                !            * there is no banding of the input and only the top cell
                !            * receives fertiliser
                if (cdpthb(nelm)==0.0d0) then
                    calit(nelm,ncetop) = ctot(nelm)*cltfct(nelm)/(deltaz(ncetop,nelm)*dtuz)
                    caman(nelm,ncetop) = ctot(nelm)*cmnfct(nelm)/(deltaz(ncetop,nelm)*dtuz)
                    cahum(nelm,ncetop) =ctot(nelm)*(1-cltfct(nelm)-cmnfct(nelm))/(deltaz(ncetop,nelm)*dtuz)
                    do nce = ncolmb(nelm),ncetop-1
                        calit(nelm,nce) = 0.0d0
                        caman(nelm,nce) = 0.0d0
                        cahum(nelm,nce) = 0.0d0
                    enddo
                    !
                    !            * there is banding of the input
                else
                    ksptot = 0.0d0
                    do nce = ncetop,ncolmb(nelm),-1
                        ksptot = ksptot + deltaz(nce,nelm)
                        !                  * the banding depth is to below this elememt
                        if (ksptot<=cdpthb(nelm)) then
                            calit(nelm,nce) = ctot(nelm)*cltfct(nelm)/(cdpthb(nelm)*dtuz)
                            caman(nelm,nce) = ctot(nelm)*cmnfct(nelm)/(cdpthb(nelm)*dtuz)
                            cahum(nelm,nce) =ctot(nelm)*(1-cltfct(nelm)-cmnfct(nelm))/(cdpthb(nelm)*dtuz)
                            !                  * the banding depth is to within this element
                        elseif((ksptot-deltaz(nce,nelm))<=cdpthb(nelm)) then
                            fracdp = (cdpthb(nelm)-(ksptot-deltaz(nce,nelm)))/cdpthb(nelm)
                            calit(nelm,nce) = ctot(nelm)*cltfct(nelm)*fracdp/(deltaz(nce,nelm)*dtuz)
                            caman(nelm,nce) = ctot(nelm)*cmnfct(nelm)*fracdp/(deltaz(nce,nelm)*dtuz)
                            cahum(nelm,nce) =ctot(nelm)*(1-cltfct(nelm)-cmnfct(nelm))*fracdp/(deltaz(nce,nelm)*dtuz)
                            !                  * the depth of the element is below the banding depth
                        else
                            calit(nelm,nce) = 0.0d0
                            caman(nelm,nce) = 0.0d0
                            cahum(nelm,nce) = 0.0d0
                        endif
                    enddo
                endif
                !
            else
                !            * set to 999 to avoid divide by zero errors
                cnralt(nelm) = 999.0d0
                cnramn(nelm) = 999.0d0
                do nce = ncolmb(nelm),ncetop
                    calit(nelm,nce) = 0.0d0
                    caman(nelm,nce) = 0.0d0
                    cahum(nelm,nce) = 0.0d0
                enddo
            endif
        enddo
        !
    else
        do nelm = nlf+1,nel
            !         * set to 999 to avoid divide by zero errors
            cnralt(nelm) = 999.0d0
            cnramn(nelm) = 999.0d0
            do nce = ncolmb(nelm),ncetop
                calit(nelm,nce) = 0.0d0
                caman(nelm,nce) = 0.0d0
                cahum(nelm,nce) = 0.0d0
            enddo
        enddo
    endif
    !
    !
    ! 7. addition of wet and dry deposition on fertilizer rate
    ! --------------------------------------------------------
    !
    do nelm = nlf+1,nel
        naamm(nelm,ncetop) = naamm(nelm,ncetop)+ ammddr/deltaz(ncetop,nelm)+ ammwdr*dummy(nelm)/deltaz(ncetop,nelm)
        nanit(nelm,ncetop) = nanit(nelm,ncetop)+ nitddr/deltaz(ncetop,nelm)+ nitwdr*dummy(nelm)/deltaz(ncetop,nelm)
    enddo
    !
    !
end subroutine mnint2



!> Updates litter and humus carbon pools.
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
subroutine mnlthm (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,fe,fh,dtuz,isbotc)
    ! externals
    !use sglobal, only : error
    !       external     error
    !
    integer llee              !! Maximum soil-cell dimension.
    integer mnpr              !! MN diagnostic output unit used for warning messages.
    integer nbotce            !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop            !! Top soil-cell index.
    integer nel               !! Number of elements.
    integer nelee             !! Element-array dimension.
    integer nlf               !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)     !! Lowest active soil cell in each land-column element.
    double precision fe       !! Efficiency fraction for organic carbon turnover.
    double precision fh       !! Humification fraction.
    double precision dtuz     !! Unsaturated-zone timestep in seconds.
    logical isbotc            !! True when the fixed lower active cell `NBOTCE` is used.
    ! locals
    integer    nbotm,ncl,nelm,niters,ntime
    integer    warn
    !
    double precision chum1o,chumh,clit1o,clith,cmanh,dum,errtol,erf
    double precision klittp,kmantp
    double precision werr1,wer1sq,werr2,wer2sq
    !
    character        msg*132
    !
    !      * parameters for the iteration loop within the subroutine
    !      * niters is the maximum number of accepteble interations
    !      * and errtol is the squared error below which the interation
    !      * will stop before niters is reached
    parameter ( niters = 20, warn = 3)
    parameter ( errtol = 1.0d-12)
    !
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
        do 100 ncl = nbotm,ncetop
            !
            !           * initialise local variables
            clith = clit(nelm,ncl)
            chumh = chum(nelm,ncl)
            chum1o = 0.0d0
            clit1o = 0.0d0
            cmanh = (cman(nelm,ncl) + cman1(nelm,ncl)) / 2.0d0
            !
            !           * if immobilisation is not equal to the potential
            !           * immobilisation then the decomposition of the litter and
            !           * and manure pools are temporarily stopped
            if (isimtf(nelm,ncl)) then
                kmantp=0.0d0
                klittp=0.0d0
            else
                kmantp=kman(nelm,ncl)
                klittp=klit(nelm,ncl)
            endif
            !
            erf = emt(nelm,ncl)*emph(nelm,ncl)
            !           *  iteration loop to calcalate the new carbon litter
            !           *  and humus concentrations
            do ntime = 1,niters
                !
                !
                dum = klittp*erf*clith*(fe-1)+ fe*erf*khum(nelm,ncl)*chumh
                dum = dum + fe*erf*kmantp*cmanh+ calit(nelm,ncl)
                clit1(nelm,ncl) = clit(nelm,ncl) + dtuz * dum
                !
                !              *  litter conc at timestep n +1/2 is calculated for use
                !              *  in the new calculation of the humus
                clith = (clit1(nelm,ncl)+clit(nelm,ncl))/ 2.0d0
                !
                dum = (1-fe)*fh*klittp*erf*clith- khum(nelm,ncl)*erf*chumh + cahum(nelm,ncl)
                chum1(nelm,ncl) = chum(nelm,ncl) + dtuz * dum
                !
                !              *  humus conc. at timestep n+1/2 is calculated. this is
                !              *  for use in the new calculation of the litter at the
                !              *  next iteration
                chumh = (chum1(nelm,ncl)+chum(nelm,ncl) )/2.0d0
                !
                !              *  relative error between iterations in both litter and
                !              *  humus pools in order to check the iteration
                !              *  is converging.
                if (clit1(nelm,ncl)/=0.0d0) then
                    werr1 = (clit1(nelm,ncl) - clit1o) / clit1(nelm,ncl)
                elseif (clit1o==0.0d0) then
                    werr1 = 0.0d0
                else
                    werr1 = 1.0d0
                endif
                !
                if (chum1(nelm,ncl)/=0.0d0) then
                    werr2 = (chum1(nelm,ncl) - chum1o) / chum1(nelm,ncl)
                elseif (chum1o==0.0d0) then
                    werr2 = 0.0d0
                else
                    werr2 = 1.0d0
                endif
                !
                !              * square of the errors, in order to make them positive
                wer1sq = werr1*werr1
                wer2sq = werr2*werr2
                !
                clit1o = clit1(nelm,ncl)
                chum1o = chum1(nelm,ncl)
                !
                !              *  break out of loop if the error in both iterations
                !              *  is less than the error tolerence
                if ((wer1sq<errtol).and.(wer2sq<errtol))goto 100
                !                                                            ********
                !
            enddo
            !
            !          *  the do loop has continued to niters and has thus
            !          *  failed to converge
            write (msg,9000) wer1sq,wer2sq
            call error( warn, 3016, mnpr, 0, 0, msg )
            !
            !
100     continue
    enddo
    !
    9000 format('iteration loop in mnlthm failed to converge with error = ',g15.7,g15.7)
    !
end subroutine mnlthm



!> Updates the litter nitrogen pool.
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
subroutine mnltn (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,cnrbio,fe,fh,dtuz,cnralt,isbotc)

    ! externals
    !use sglobal, only : error
    !       external     error
    !
    integer llee              !! Maximum soil-cell dimension.
    integer mnpr              !! MN diagnostic output unit used for warning messages.
    integer nbotce            !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop            !! Top soil-cell index.
    integer nel               !! Number of elements.
    integer nelee             !! Element-array dimension.
    integer nlf               !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)     !! Lowest active soil cell in each land-column element.
    double precision cnrbio   !! Biomass carbon-to-nitrogen ratio.
    double precision fe       !! Efficiency fraction for organic carbon turnover.
    double precision fh       !! Humification fraction; passed through but not used.
    double precision dtuz     !! Unsaturated-zone timestep in seconds.
    double precision cnralt(nelee) !! Element litter C:N ratio for active additions.
    logical isbotc            !! True when the fixed lower active cell `NBOTCE` is used.
    ! locals
    integer   nbotm,ncl,nelm,niters,ntime
    integer   warn
    !
    double precision chumh,clith,cmanh,dum,errtol,erf
    double precision klittp,kmantp,nlith
    double precision nlit1o, werr1, wer1sq
    !
    character        msg*132
    !
    !      * parameters for the iteration loop within the subroutine
    !      * niters is the maximum number of accepteble interations
    !      * and errtol is the squared error below which the interation
    !      * will stop before niters is reached
    parameter ( niters = 20, warn = 3)
    parameter ( errtol = 1.0d-12)
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
        do 100 ncl = nbotm,ncetop
            !          * initialise local variables
            chumh = ( chum(nelm,ncl) + chum1(nelm,ncl) )/2.0d0
            clith = ( clit(nelm,ncl) + clit1(nelm,ncl) )/2.0d0
            cmanh = ( cman(nelm,ncl) + cman1(nelm,ncl) )/2.0d0
            nlith = nlit(nelm,ncl)
            nlit1o = 0.0d0
            !
            !
            !          * if immobilisation is not equal to the potential
            !          * immobilisation then the decomposition of the litter pool
            !          * and the manure pool are temporarily stopped
            if (isimtf(nelm,ncl)) then
                klittp=0.0d0
                kmantp=0.0d0
            else
                klittp=klit(nelm,ncl)
                kmantp=kman(nelm,ncl)
            endif
            !
            erf = emt(nelm,ncl)*emph(nelm,ncl)
            !
            !          *  iteration loop to calcalate the new nitrogen litter
            !          *  concentrations
            do ntime = 1,niters
                !
                !
                dum = -klittp*erf*nlith+ fe*klittp*erf*clith/cnrbio
                dum = dum + fe*khum(nelm,ncl)*erf*chumh/cnrbio+ calit(nelm,ncl)/cnralt(nelm)
                dum = dum + fe*kmantp*erf*cmanh /cnrbio
                !
                !
                nlit1(nelm,ncl) = nlit(nelm,ncl) + dtuz * dum
                !
                !            *  litter conc at timestep n +1/2 is calculated for use
                !            *  in the new calculation of the litter
                nlith = (nlit1(nelm,ncl) + nlit(nelm,ncl))/ 2.0d0
                !
                !
                !            *  relative error between iterations to see if the
                !            *  iteration is converging.
                if (nlit1(nelm,ncl)/=0.0d0) then
                    werr1 = (nlit1(nelm,ncl) - nlit1o) / nlit1(nelm,ncl)
                elseif (nlit1o==0.0d0) then
                    werr1 = 0.0d0
                else
                    werr1 = 1.0d0
                endif
                !
                !            * square of the errors, in order to make them positive
                wer1sq = werr1*werr1
                !
                nlit1o = nlit1(nelm,ncl)
                !
                !            *  break out of loop if the error in the iteration
                !            *  is less than the error tolerence
                if (wer1sq<errtol) goto 100
                !                                  ********
                !
            enddo
            !
            !          *  the do loop has continued to niters and has thus
            !          *  failed to converge
            write (msg,9000) wer1sq
            call error( warn, 3017, mnpr, 0, 0, msg )
            !
            !
100     continue
    enddo
    !
    9000 format('iteration loop in mnltn failed to converge with error = ',g15.7)
    !
end subroutine mnltn



!> Main mineral nitrogen setup and timestep driver.
!>
!> `mnmain` uses a saved call counter. The first call performs static checks,
!> reads the MND file, and initialises state; subsequent calls run the timestep
!> update and optional output.
!>
!> | Phase | Call order | Purpose |
!> | --- | --- | --- |
!> | First call | [[mnerr0]] -> [[mnerr1]] -> [[mnred1]] -> [[mnerr2]] -> [[mninit]] | Check array/interface consistency, read static nitrate data, validate it, interpolate initial pools and process parameters, and reset source/sink arrays. |
!> | Later calls | [[mnerr3]] -> [[mnred2]] -> [[mnerr4]] -> [[mnint2]] | Check dynamic CM-MN state, read scheduled MNFC/MNFN additions, validate them, and convert concentrations/additions/deposition to cell-based rates. |
!> | Environment | [[mntemp]] -> [[mnemt]] -> [[mnent]] -> [[mnemph]] -> [[mnenph]] -> [[mnedth]] | Update soil temperature and temperature, matric-potential, and saturation response factors. |
!> | Carbon and nitrogen pools | [[mnman]] -> [[mnlthm]] -> [[mnltn]] -> [[mnco2]] -> [[mngam]] -> [[mnamm]] -> [[mnnit]] | Update manure, litter, humus, carbon dioxide production, mineralisation/immobilisation, ammonium, and nitrate source/sink terms. |
!> | Output | [[mnout]] | Write requested detailed MN diagnostics. |
!>
!> Static parameters read by [[mnred1]], including deposition rates, Q10 values,
!> reaction constants, `MNCREF`, and `ISBOTC`, are saved between calls.
subroutine mnmain(mnd,mnfc,mnfn,mnpr,mnout1,mnout2,ncetop,ncon,nel,nlf,ns,nv,nx,ny,icmbk,icmref,icmxy,ncolmb,nlyr &
    ,nlyrbt,ntsoil,d0,tih,z2,dxqq,dyqq,vspor,deltaz,zvsnod,bexbk,linkns,dtuz,uznow,cccc,pnetto,ssss,ta,vspsi, &
    vsthe,vstheo,sss1,sss2 )

    ! externals
    !external mnamm,mnco2,mnedth,mnemph
    !external mnemt,mnenph,mnent,mnerr0,mnerr1,mnerr2
    !external mnerr3,mnerr4,mngam,mninit,mnint2
    !external mnlthm,mnltn,mnman,mnnit,mnout,mnred1,mnred2
    !
    integer mnd                     !! Static MND input unit.
    integer mnfc                    !! Scheduled carbon-addition input unit.
    integer mnfn                    !! Scheduled nitrogen-addition input unit.
    integer mnpr                    !! MN diagnostic output unit.
    integer mnout1                  !! Carbon budget output unit.
    integer mnout2                  !! Nitrogen budget output unit.
    integer ncetop                  !! Top soil-cell index.
    integer ncon                    !! Number of contaminant species coupled to MN.
    integer nel                     !! Number of elements.
    integer nlf                     !! Number of overland/channel links.
    integer ns                      !! Number of soil types.
    integer nv                      !! Number of vegetation/meteorological entries.
    integer nx                      !! Number of grid columns.
    integer ny                      !! Number of grid rows.
    integer icmbk(nlfee,2)          !! Bank-element numbers for each channel link.
    integer icmref(nelee,4,2:2)     !! Neighbour reference map.
    integer icmxy(nxee,ny)          !! Element number at each grid location.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nlyr(nelee)             !! Number of soil layers in each element.
    integer nlyrbt(nel,nlyree)      !! Bottom cell index of each soil layer.
    integer ntsoil(nel,nlyree)      !! Soil type index for each element layer.
    double precision d0             !! Reference diffusion/dispersion scale used by CM.
    double precision tih            !! Initial simulation time in hours.
    double precision z2             !! Vertical length scale used by CM and MN temperature diffusion.
    double precision dxqq(nelee)    !! Element width.
    double precision dyqq(nelee)    !! Element length.
    double precision vspor(ns)      !! Soil porosity by soil type.
    double precision deltaz(llee,nel) !! Cell thickness by cell and element.
    double precision zvsnod(llee,nel) !! Vertical node elevation/depth by cell and element.
    logical bexbk                   !! True when bank elements are represented.
    logical linkns(nlfee)           !! True for north-south channel links.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision uznow          !! Current unsaturated-zone simulation time.
    double precision cccc(nel,ncetop+1) !! Dynamic-region nitrate concentration.
    double precision pnetto(nelee)  !! Net precipitation/effective rainfall by element.
    double precision ssss(nel,ncetop+1) !! Dead-space nitrate concentration.
    double precision ta(nv)         !! Air temperature by vegetation/meteorological entry.
    double precision vspsi(ncetop,nel)  !! Matric potential/pressure head by cell and element.
    double precision vsthe(ncetop,nel)  !! Current volumetric water content.
    double precision vstheo(nel,ncetop+1) !! Previous volumetric water content.
    double precision sss1(nel,ncetop+1)  !! Dynamic-region CM source/sink array.
    double precision sss2(nel,ncetop+1)  !! Dead-space CM source/sink array.
    ! locals etc.
    !
    !
    !     * array sizes for maximum number of typical elements in input data
    !     * and number of values in a depth/conc table
    !     * nmneee is the number of category types and this must be less than
    !     * or equal to nine. nmntee is the max. number of values in the table
    !     * for each category type
    !     * in incm.f two corresponding parameter values are declared and these
    !     * are called ncatee and ntabee
    integer nmneee,nmntee
    parameter (nmneee = 9, nmntee = 10)
    !
    !     those saved
    integer nbotce,pass
    !
    !     those not saved
    integer nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e
    integer nmn27e,nmn43e,nmn53e
    integer celem(nelee),kd1elm(nelee),kd2elm(nelee)
    integer khelem(nelee),klelem(nelee),kmelem(nelee)
    integer knelem(nelee),kvelem(nelee)
    integer naelem(nelee)
    integer nmn15t(nmneee),nmn17t(nmneee),nmn19t(nmneee)
    integer nmn21t(nmneee),nmn23t(nmneee),nmn25t(nmneee)
    integer nmn27t(nmneee)
    integer nmn43t(nmneee),nmn53t(nmneee)
    integer dummy2(nlyree,nelee),dummy3(nlyree)
    integer idum(nelee),idum1x(nelee+3)
    !
    !     those saved
    double precision ammddr,ammwdr,cnrbio,cnrhum,fe,fh,gnn
    double precision kplamm,kplnit,kuamm,kunit,mncref,nitddr
    double precision nitwdr,q10m,q10n
    !double precision chum1(nelee,llee)
    !double precision clit1(nelee,llee),cman1(nelee,llee)
    !double precision imdiff(nelee,llee)
    !double precision kd1(nelee,llee),kd2(nelee,llee)
    double precision kddsol(nsee)
    !double precision khum(nelee,llee),klit(nelee,llee)
    !double precision kman(nelee,llee),knit(nelee,llee)
    !double precision kvol(nelee,llee)
    !double precision namm1(nelee,llee)
    !double precision nlit1(nelee,llee),nman1(nelee,llee)
    !
    !     those not saved
    double precision clitfr,cnrlit
    double precision cdpthb(nelee),cltfct(nelee)
    double precision cmnfct(nelee),cnral(nelee),cnralt(nelee)
    double precision cnram(nelee),cnramn(nelee)
    double precision ctot(nelee),ctottp(nelee)
    double precision damhlf(nelee),dchlf(nelee)
    double precision namfct(nelee),namtop(nelee),ndpthb(nelee)
    double precision ntot(nelee)
    !double precision cahum(nelee,llee)
    !double precision calit(nelee,llee),caman(nelee,llee)
    !double precision cdort(nelee,llee)
    !double precision chum(nelee,llee)
    !double precision clit(nelee,llee)
    double precision cconc(nmneee,nmntee),cdpth(nmneee,nmntee)
    !double precision cman(nelee,llee)
    !double precision denit(nelee,llee)
    !double precision edeth(nelee,llee),emph(nelee,llee)
    !double precision emt(nelee,llee),enph(nelee,llee)
    !double precision ent(nelee,llee),gam(nelee,llee)
    !double precision gamtmp(nelee,llee),imamm(nelee,llee)
    !double precision imnit(nelee,llee)
    double precision kd1cnc(nmneee,nmntee),kd1dth(nmneee,nmntee)
    double precision kd2cnc(nmneee,nmntee),kd2dth(nmneee,nmntee)
    double precision khconc(nmneee,nmntee),khdpth(nmneee,nmntee)
    double precision klconc(nmneee,nmntee),kldpth(nmneee,nmntee)
    double precision kmconc(nmneee,nmntee),kmdpth(nmneee,nmntee)
    double precision knconc(nmneee,nmntee),kndpth(nmneee,nmntee)
    double precision kvconc(nmneee,nmntee),kvdpth(nmneee,nmntee)
    !double precision miner(nelee,llee),naamm(nelee,llee)
    double precision naconc(nmneee,nmntee),nadpth(nmneee,nmntee)
    !double precision namm(nelee,llee)
    !double precision nanit(nelee,llee),ndnit(nelee,llee)
    !double precision ndsnt(nelee,llee)
    !double precision nlit(nelee,llee)
    !double precision nman(nelee,llee)
    !double precision ntrf(nelee,llee)
    !double precision plamm(nelee,llee)
    !double precision plnit(nelee,llee)
    !double precision pphi(nelee,llee),snit(nelee,llee)
    !double precision temp(nelee,llee)
    !double precision vol(nelee,llee)
    !
    double precision dummy(nelee)
    !double precision dummy4(llee,nelee)
    !double precision dummy6(nelee,llee)
    !
    !     those saved
    logical isbotc,isq10
    !logical isimtf(nelee,llee)
    !
    !     those not saved
    logical isaddc,isaddn,isiccd,isiamd
    logical ldum(nelee),ldum2(llee)
    !
    save nbotce,pass
    save ammddr,ammwdr,cnrbio,cnrhum,fe,fh,gnn
    save kplamm,kplnit,kuamm,kunit,mncref,nitddr
    save nitwdr,q10m,q10n
    !save chum1,clit1,cman1
    !save imdiff
    !save kd1,kd2,kddsol,khum,klit,kman,knit,kvol
    !save namm1
    !save nlit1,nman1
    save isbotc,isq10
    !save isimtf
    data pass / 0 /
    !
    !
    !
    pass = pass + 1
    if (pass==1) then
        !
        !                        ----------------------
        !------------------------ initialization step  ---------------------*
        !                        ----------------------
        !
        !        * check array dimensions
        call mnerr0(llee,mnd,mnfc,mnfn,mnpr,ncetop,ncon,nconee,nel,nelee,nlf,nlfee,nlyree,nmneee,nmntee,ns,nsee,nv, &
        nvee,nx,nxee,ny )
        !
        !
        !
        !        * checks static input variables from cm - mn interface
        call mnerr1(llee,mnpr,ncetop,nel,nelee,nlf,nlfee,nlyree,ns,nx,nxee,ny,icmbk,icmref,icmxy,ncolmb,nlyr,nlyrbt, &
        ntsoil,d0,tih,z2,dxqq,dyqq,vspor,deltaz,zvsnod,bexbk,linkns,dummy2,dummy3,idum,idum1x,ldum,ldum2)
        !
        !        * read the input data files
        call mnred1(mnd,mnpr,nel,nelee,nlf,nlfee,nmneee,nmntee,ns,nx,nxee,ny,icmbk,icmref,icmxy,bexbk,linkns,nbotce, &
        nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e,celem(nlf+1),kd1elm(nlf+1),kd2elm(nlf+1), &
        khelem(nlf+1),klelem(nlf+1),kmelem(nlf+1),knelem(nlf+1),kvelem(nlf+1),naelem(nlf+1),nmn15t,nmn17t,nmn19t, &
        nmn21t,nmn23t,nmn25t,nmn27t,nmn43t,nmn53t,ammddr,ammwdr,clitfr,cnrbio,cnrhum,cnrlit,fe,fh,gnn,kplamm,kplnit, &
        kuamm,kunit,mncref,nitddr,nitwdr,q10m,q10n,cconc,cdpth,ctottp(nlf+1),damhlf(nlf+1),dchlf(nlf+1),kd1cnc,kd1dth &
        ,kd2cnc,kd2dth,kddsol,khconc,khdpth,klconc,kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth, &
        namtop(nlf+1),isiccd,isiamd,isq10,idum,dummy )
        !
        !
        !        * checks static input data read in mnred1
        call mnerr2(mnpr,nbotce,ncetop,nel,nelee,nlf,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e, &
        nmneee,nmntee,ns,celem(nlf+1),kd1elm(nlf+1),kd2elm(nlf+1),khelem(nlf+1),klelem(nlf+1),kmelem(nlf+1), &
        knelem(nlf+1),kvelem(nlf+1),naelem(nlf+1),nmn15t,nmn17t,nmn19t,nmn21t,nmn23t,nmn25t,nmn27t,nmn43t,nmn53t, &
        ammddr,ammwdr,clitfr,cnrbio,cnrhum,cnrlit,fe,fh,gnn,kplamm,kplnit,kuamm,kunit,mncref,nitddr,nitwdr,q10m,q10n, &
        cconc,cdpth,ctottp(nlf+1),damhlf(nlf+1),dchlf(nlf+1),kd1cnc,kd1dth,kd2cnc,kd2dth,kddsol,khconc,khdpth,klconc, &
        kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth,namtop(nlf+1),isiccd,isiamd,ldum)
        !
        !        * initilialises variables
        call mninit(llee,nbotce,ncetop,nel,nelee,nlf,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e, &
        nmneee,nmntee,celem(nlf+1),kd1elm(nlf+1),kd2elm(nlf+1),khelem(nlf+1),klelem(nlf+1),kmelem(nlf+1),knelem(nlf+ &
        1),kvelem(nlf+1),naelem(nlf+1),ncolmb,nmn15t,nmn17t,nmn19t,nmn21t,nmn23t,nmn25t,nmn27t,nmn43t,nmn53t,clitfr, &
        cnrlit,cconc,cdpth,ctottp(nlf+1),damhlf(nlf+1),dchlf(nlf+1),deltaz,kd1cnc,kd1dth,kd2cnc,kd2dth,khconc,khdpth, &
        klconc,kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth,namtop(nlf+1),zvsnod,isiccd,isiamd, &
        sss1,sss2,isbotc)
        !
        !
        !
        !----------------------- end of initialization step------------------*
        !
    else
        !                        -----------------
        !------------------------ simulation step ---------------------------*
        !                        -----------------
        !
        !
        !        * checks time varying input variables from cm -mn interface
        call mnerr3(llee,mnpr,ncetop,nel,nelee,nlf,ncolmb,dtuz,uznow,cccc, &
        pnetto,ssss,vsthe,vstheo,ldum,ldum2 )
        !
        !        * reads time varying input data
        call mnred2 ( mnfc,mnfn,mnpr,nel,nelee,nlf,nlfee,nx,nxee,ny,icmbk,icmref,icmxy,dtuz,tih,uznow,bexbk,linkns, &
        cdpthb(nlf+1),cltfct(nlf+1),cmnfct(nlf+1),cnral(nlf+1),cnram(nlf+1),ctot(nlf+1),namfct(nlf+1),ndpthb(nlf+1), &
        ntot(nlf+1),isaddc,isaddn,idum,dummy)
        !
        !        * checks time dependent input data read in mnred2
        call mnerr4 ( mnpr,nel,nelee,nlf,cdpthb(nlf+1),cltfct(nlf+1),cmnfct(nlf+1),cnral(nlf+1),cnram(nlf+1),ctot(nlf &
        +1),namfct(nlf+1),ndpthb(nlf+1),ntot(nlf+1),isaddc,isaddn,dummy,ldum )
        !
        !
        !        * modifies data read in mnred2 into suitable units and form
        !        * for the rest of the program
        call mnint2 ( llee,ncetop,nel,nelee,nlf,nlyree,ncolmb,nlyr,nlyrbt,ntsoil,ammddr,ammwdr,mncref,nitddr,nitwdr, &
        deltaz,dtuz,cccc,cdpthb(nlf+1),cltfct(nlf+1),cmnfct(nlf+1),cnral(nlf+1),cnram(nlf+1), &
        ctot(nlf+1),namfct(nlf+1),ndpthb(nlf+1),ntot(nlf+1),pnetto,ssss,vsthe,isaddc,isaddn, &
        cnralt,cnramn,dummy)
        !
        !
        call mntemp (llee,ncetop,nel,nelee,nlf,nv,ncolmb,z2,deltaz,zvsnod,dtuz,ta)
        !
        !           * environmental reduction factors are calculated
        call mnemt (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,q10m,isbotc,isq10)
        call mnent (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,q10n,isbotc,isq10)
        call mnemph (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,vspsi,isbotc)
        call mnenph (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,vspsi,isbotc)
        call mnedth (llee,nbotce,ncetop,nel,nelee,nlf,nlyree,ns,ncolmb,nlyr,nlyrbt,ntsoil,vsthe,vspor,isbotc)

        !
        !
        !           * new concentration of carbon and nitrogen manure pools
        call mnman (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,dtuz,cnramn,isbotc)
        !
        !
        !         * new concentration of carbon litter and humus pools
        call mnlthm (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,fe,fh,dtuz,isbotc)
        !
        !
        !         * new concentration of nitrogen litter pool
        call mnltn (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,cnrbio,fe,fh,dtuz,cnralt,isbotc)
        !
        !         * carbon dioxide production
        call mnco2 (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,fe,fh,isbotc)
        !
        !         * mineralization/immobilisation rate
        call mngam (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,cnrhum,cnrbio,fe,fh,dtuz,isbotc)
        !
        !
        !         * new concentration of ammonium
        call mnamm (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,nlyree,ns,ncolmb,nlyr,nlyrbt,ntsoil,gnn,kplamm,kuamm,mncref &
        ,kddsol,dtuz,vsthe,vstheo,isbotc)
        !
        !         * new nitrate concentration in dynamic and dead space regions
        call mnnit (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,d0,kplnit,kunit,mncref,z2,dtuz,vsthe,vstheo,isbotc,sss1,sss2)
        !
        !
        !
        !     * extra output that may be required that is printed in this
        !     * subroutine
        call mnout (mnout1,mnout2,nbotce,ncetop,nel,nlf,ns,ncolmb,nlyr,nlyrbt,ntsoil,cnrhum,gnn,mncref,deltaz,kddsol, &
        pphi,dtuz,uznow,dxqq,dyqq,cnralt,cnramn,vsthe,vstheo,isbotc)
        !
        !
        !------------------------end of simulation step---------------------*
    endif
    !
    !
end subroutine mnmain



!> Updates manure carbon and nitrogen pools.
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
subroutine mnman (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,dtuz,cnramn,isbotc)

    ! externals
    !use sglobal, only : error
    !       external     error
    !
    integer llee              !! Maximum soil-cell dimension.
    integer mnpr              !! MN diagnostic output unit used for warning messages.
    integer nbotce            !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop            !! Top soil-cell index.
    integer nel               !! Number of elements.
    integer nelee             !! Element-array dimension.
    integer nlf               !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)     !! Lowest active soil cell in each land-column element.
    double precision dtuz     !! Unsaturated-zone timestep in seconds.
    double precision cnramn(nelee) !! Element manure C:N ratio for active additions.
    logical isbotc            !! True when the fixed lower active cell `NBOTCE` is used.
    ! locals
    integer          nbotm,ncl,nelm,niters,ntime
    integer          warn
    !
    double precision cman1o,cmanh,dum,errtol,erf
    double precision kmantp,nman1o,nmanh
    double precision wer1sq,werr1,wer2sq,werr2
    !
    character        msg*132
    !
    !      * parameters for the iteration loop within the subroutine
    !      * niters is the maximum number of accepteble interations
    !      * and errtol is the squared error below which the interation
    !      * will stop before niters is reached
    parameter ( niters = 20, warn = 3)
    parameter ( errtol = 1.0d-12)
    !
    !
    !
    !-------------------------------------------------------------------*
    !
    !      *  main loop which goes through every cell in the soil column
    do nelm = nlf+1,nel
        if (isbotc) then
            nbotm = nbotce
        else
            nbotm = ncolmb(nelm)
        endif
        do 100 ncl = nbotm,ncetop
            !            * initialise local variables
            cmanh = cman(nelm,ncl)
            nmanh = nman(nelm,ncl)
            cman1o = 0.0d0
            nman1o = 0.0d0
            !
            !           * if immobilisation is not equal to the potential
            !           * immobilisation then the decomposition of the manure pool
            !           * is temporarily stopped
            if (isimtf(nelm,ncl)) then
                kmantp=0.0d0
            else
                kmantp=kman(nelm,ncl)
            endif
            !
            erf = emt(nelm,ncl)*emph(nelm,ncl)
            !
            !           * iteration loop to calcalate the new manure concentrations
            do ntime = 1,niters
                !
                dum = -kmantp*erf*cmanh + caman(nelm,ncl)
                cman1(nelm,ncl) = cman(nelm,ncl) + dtuz * dum
                !
                dum = -kmantp*erf*nmanh+ caman(nelm,ncl) / cnramn(nelm)
                nman1(nelm,ncl) = nman(nelm,ncl) + dtuz * dum
                !
                !             * calcultes the relative error in the iteration
                if (cman1(nelm,ncl)/=0.0d0) then
                    werr1 = (cman1(nelm,ncl) - cman1o) / cman1(nelm,ncl)
                elseif (cman1o==0.0d0) then
                    werr1 = 0.0d0
                else
                    werr1 = 1.0d0
                endif
                !
                if (nman1(nelm,ncl)/=0.0d0) then
                    werr2 = (nman1(nelm,ncl) - nman1o) / nman1(nelm,ncl)
                elseif (nman1o==0.0d0) then
                    werr2 = 0.0d0
                else
                    werr2 = 1.0d0
                endif
                !
                !             * calculates the squred error, so that they are positive
                wer1sq = werr1*werr1
                wer2sq = werr2*werr2
                !
                !             * updates the conc. at timestep n + 1/2 and the old conc.
                cmanh = (cman1(nelm,ncl)+cman(nelm,ncl)) /2.0d0
                cman1o = cman1(nelm,ncl)
                nmanh = (nman1(nelm,ncl)+nman(nelm,ncl)) /2.0d0
                nman1o = nman1(nelm,ncl)
                !
                !             * break out of loop if error in both iterations is
                !             * less than the error tolerance
                if ((wer1sq<errtol).and.(wer2sq<errtol)) goto 100
                !                                                            ********
            enddo
            !
            !          *  the do loop has continued to niters and has thus
            !          *  failed to converge
            write (msg,9000) wer1sq,wer2sq
            call error( warn, 3015, mnpr, 0, 0, msg )
            !
            !
100    continue
    enddo
    !
    9000 format('iteration loop in mnman failed to converge with error = ',g15.7,g15.7)
    !
end subroutine mnman



!> Calculates nitrate source/sink terms for dynamic and dead-space water.
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

    integer llee                    !! Maximum soil-cell dimension.
    integer nbotce                  !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links excluded from land-column updates.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    double precision d0             !! Reference diffusion/dispersion scale used by CM.
    double precision kplnit         !! First-order nitrate plant-uptake limit.
    double precision kunit          !! First-order nitrate immobilisation limit.
    double precision mncref         !! Reference nitrogen concentration.
    double precision z2             !! Vertical length scale used by CM source conversion.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision vsthe(ncetop,nel) !! Current volumetric water content.
    double precision vstheo(nel,ncetop+1) !! Previous volumetric water content.
    logical isbotc                  !! True when the fixed lower active cell `NBOTCE` is used.
    double precision sss1(nel,ncetop+1) !! Dynamic-region CM source/sink array.
    double precision sss2(nel,ncetop+1) !! Dead-space CM source/sink array.
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



!> Accumulates and writes mineral nitrogen and carbon budget outputs.
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
!> | Store totals | Recompute current nitrogen and carbon stores from updated pools. Ammonium storage uses the nonlinear retardation factor \(1 + KDDSOL(NAMM1/MNCREF)^{GNN-1}/VSTHE\). |
!> | Periodic output | When `UZNOW >= MNSTRT + 24*NPRNT`, increment `NPRNT` and write current total/addition/loss summaries normalised by total land area. |
!>
!> The routine does not reset cumulative flux arrays after each write; reported
!> additions and losses are cumulative since the initial `MNOUT` call.
subroutine mnout (mnout1,mnout2,nbotce,ncetop,nel,nlf,ns,ncolmb,nlyr,nlyrbt,ntsoil,cnrhum,gnn,mncref,deltaz, &
    kddsol,pphi,dtuz,uznow,dxqq,dyqq,cnralt,cnramn,vsthe,vstheo,isbotc)

    integer mnout1                  !! Carbon budget output unit.
    integer mnout2                  !! Nitrogen budget output unit.
    integer nbotce                  !! Lowest cell included when bottom-cell truncation is active.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nlf                     !! Number of overland/channel links excluded from land-column output.
    integer ns                      !! Number of soil types.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nlyr(nelee)             !! Number of soil layers in each element.
    integer nlyrbt(nel,nlyree)      !! Bottom cell index of each soil layer.
    integer ntsoil(nel,nlyree)      !! Soil type index for each element layer.
    double precision cnrhum         !! Humus carbon-to-nitrogen ratio.
    double precision gnn            !! Nonlinear ammonium adsorption exponent.
    double precision mncref         !! Reference nitrogen concentration.
    double precision deltaz(llee,nel) !! Cell thickness by cell and element.
    double precision kddsol(ns)     !! Soil ammonium adsorption coefficient.
    double precision pphi(nelee,llee) !! Mobile-water partition factor.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision uznow          !! Current unsaturated-zone simulation time.
    double precision dxqq(nelee)    !! Element width.
    double precision dyqq(nelee)    !! Element length.
    double precision cnralt(nelee)  !! Element litter C:N ratio for active additions.
    double precision cnramn(nelee)  !! Element manure C:N ratio for active additions.
    double precision vsthe(ncetop,nel)  !! Current volumetric water content.
    double precision vstheo(nel,ncetop+1) !! Previous volumetric water content.
    logical isbotc                  !! True when the fixed lower active cell `NBOTCE` is used.
    ! locals etc.
    integer hrprnt
    parameter (hrprnt = 24)
    !
    integer jlyr,jsoil,nbotm,ncebot,ncl,nelm,nprnt,pass
    character msg*60
    !
    double precision mnstrt,retamm,tarea
    double precision totadc,totadn,totc,totco2,totlos,totn


    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     adammt,addct,adnitt,adornt,cdotot,detot,gamtot,imammt
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     imnitt,mintot,ntrtot,plammt,plnitt,stot,voltot

    !
    save nprnt,pass
    save mnstrt,tarea
    save adammt,addct,adnitt,adornt,cdotot,detot,gamtot,imammt
    save imnitt,mintot,ntrtot,plammt,plnitt,stot,voltot
    !
    !      declarations for output for specific cells
    integer noutl,nout,n1,n2
    parameter (nout = 9)
    integer noutel(nout),noutce(nout)
    data nprnt,pass/ 0,0 /
    !
    !      output for specific cells
    data noutel/ 457,457,457,457,457,457,457,457,457 /
    !data noutel/ 1,1,1,1,1,1,1,1,1 /
    data noutce/10,20,30,32,35,38,40,41,42/
    !do noutl  = 1,nout
    !    write (msg,9000)noutl
    !    open (102+noutl,file=msg)
    !enddo
    !
    !9000  format ( i1,'.dat')
    !
    !-------------------------------------------------------------------*
    !
    pass = pass + 1
    !
    !     * if it is the first pass the initial concentrations are printed
    if (pass==1) then
        !
        !

        allocate   (adammt(nel,ncetop),addct(nel,ncetop),adnitt(nel,ncetop),adornt(nel,ncetop),cdotot(nel,ncetop),detot(nel,ncetop))
        allocate   (gamtot(nel,ncetop),imammt(nel,ncetop),imnitt(nel,ncetop),mintot(nel,ncetop),ntrtot(nel,ncetop),plammt(nel,ncetop))
        allocate   (plnitt(nel,ncetop),stot(nel,ncetop),voltot(nel,ncetop))


        totc = 0d0
        totn = 0d0
        tarea = 0d0
        !
        do nelm = nlf+1,nel
            if (isbotc) then
                nbotm = nbotce
            else
                nbotm = ncolmb(nelm)
            endif
            !
            tarea = tarea + dxqq(nelm)*dyqq(nelm)
            !
            ncebot = nbotm
            do jlyr = 1,nlyr(nelm)
                jsoil = ntsoil(nelm,jlyr)
                do ncl =max(ncebot,nlyrbt(nelm,jlyr)),nlyrbt(nelm,jlyr+1)-1
                    adammt(nelm,ncl)  = 0.0d0
                    addct(nelm,ncl)  = 0.0d0
                    adnitt(nelm,ncl)  = 0.0d0
                    adornt(nelm,ncl)  = 0.0d0
                    cdotot(nelm,ncl)  = 0.0d0
                    detot(nelm,ncl)  = 0.0d0
                    gamtot(nelm,ncl)  = 0.0d0
                    imammt(nelm,ncl)  = 0.0d0
                    imnitt(nelm,ncl)  = 0.0d0
                    mintot(nelm,ncl)  = 0.0d0
                    ntrtot(nelm,ncl)  = 0.0d0
                    plammt(nelm,ncl)  = 0.0d0
                    plnitt(nelm,ncl)  = 0.0d0
                    stot(nelm,ncl)  = 0.0d0
                    voltot(nelm,ncl)  = 0.0d0
                    retamm = 1.0 +(kddsol(jsoil)*(namm(nelm,ncl)/mncref)**(gnn-1))/vstheo(nelm,ncl)
                    totn =totn + deltaz(ncl,nelm)*dxqq(nelm)*dyqq(nelm)*( namm(nelm,ncl)*vstheo(nelm,ncl)*retamm+ &
                     nlit(nelm,ncl) + nman(nelm,ncl)+ chum(nelm,ncl)/cnrhum )
                    totc = totc + deltaz(ncl,nelm)*dxqq(nelm)*dyqq(nelm)*( cman(nelm,ncl)+ clit(nelm,ncl) + chum(nelm &
                    ,ncl) )
                enddo
            enddo
        enddo
        !
        mnstrt = uznow
        !
        write (mnout2,'(/a30,g16.8)') 'initial nitrogen (kg n m-2) = ',totn/tarea
        write (mnout1,'(/a28,g16.8)') 'initial carbon (kg c m-2) = ',totc/tarea
        !
        !        output for specific cells
        !do noutl = 1,nout
        !    write (102+noutl,'(a13,a13,a13,a13,a13,a13,a13,a13,a13,a13,a13,a13,a13,a13,a13,a13,a13)') 'time','humus', &
        !    'litter','manure','ammonium','addamm','addnit','addcarbon','addorgn','mineral','nitrif','plantnit', &
        !    'plantamm','denit','immamm','immnit','source/sink'
        !enddo
        !
    endif
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
            adammt(nelm,ncl) = adammt(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*naamm(nelm,ncl)
            addct(nelm,ncl) = addct(nelm,ncl) + dtuz*deltaz(ncl,nelm)*(caman(nelm,ncl)+cahum(nelm,ncl)+calit(nelm,ncl))
            adnitt(nelm,ncl) = adnitt(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*nanit(nelm,ncl)
            adornt(nelm,ncl) = adornt(nelm,ncl) +dtuz*deltaz(ncl,nelm)*(caman(nelm,ncl)/cnramn(nelm)+ cahum(nelm,ncl)/cnrhum+ calit(nelm,ncl)/cnralt(nelm))
            cdotot(nelm,ncl) = cdotot(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*cdort(nelm,ncl)
            detot(nelm,ncl) = detot(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*denit(nelm,ncl)
            gamtot(nelm,ncl) = gamtot(nelm,ncl)+dtuz*deltaz(ncl,nelm)*gamtmp(nelm,ncl)
            imammt(nelm,ncl) = imammt(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*imamm(nelm,ncl)
            imnitt(nelm,ncl) = imnitt(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*imnit(nelm,ncl)
            mintot(nelm,ncl) = mintot(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*miner(nelm,ncl)
            ntrtot(nelm,ncl) = ntrtot(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*ntrf(nelm,ncl)
            plammt(nelm,ncl) = plammt(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*plamm(nelm,ncl)
            plnitt(nelm,ncl) = plnitt(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*plnit(nelm,ncl)
            stot(nelm,ncl) = stot(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*snit(nelm,ncl)
            voltot(nelm,ncl) = voltot(nelm,ncl)+ dtuz*deltaz(ncl,nelm)*vol(nelm,ncl)
            !
        enddo
    enddo
    !
    totadn = 0d0
    totadc = 0d0
    totlos = 0d0
    totn = 0d0
    totc = 0d0
    totco2 = 0d0
    !
    do nelm = nlf+1,nel
        if (isbotc) then
            nbotm = nbotce
        else
            nbotm = ncolmb(nelm)
        endif
        ncebot = nbotm
        do jlyr = 1,nlyr(nelm)
            jsoil = ntsoil(nelm,jlyr)
            do ncl =max(ncebot,nlyrbt(nelm,jlyr)),nlyrbt(nelm,jlyr+1)-1
                !
                retamm = 1.0 +(kddsol(jsoil)*(namm1(nelm,ncl)/mncref)**(gnn-1))/vsthe(ncl,nelm)
                !
                !             * sum of concentrations over all the cells
                totlos = totlos + dxqq(nelm)*dyqq(nelm)*(voltot(nelm,ncl)+ plammt(nelm,ncl) + ntrtot(nelm,ncl))
                totadn = totadn + dxqq(nelm)*dyqq(nelm)*(adornt(nelm,ncl) + adammt(nelm,ncl) + imnitt(nelm,ncl))
                totadc = totadc + dxqq(nelm)*dyqq(nelm)*addct(nelm,ncl)
                totn = totn+ deltaz(ncl,nelm)*dxqq(nelm)*dyqq(nelm)*(namm1(nelm,ncl)*vsthe(ncl,nelm)*retamm+ &
                 nlit1(nelm,ncl) + nman1(nelm,ncl)+ chum1(nelm,ncl)/cnrhum)
                totc = totc + deltaz(ncl,nelm)*dxqq(nelm)*dyqq(nelm)*( cman1(nelm,ncl)+ clit1(nelm,ncl) + chum1(nelm, &
                ncl) )
                totco2 = totco2 + dxqq(nelm)*dyqq(nelm)*cdotot(nelm,ncl)
                !
            enddo
        enddo
    enddo
    !
    !
    if (uznow>=hrprnt*nprnt + mnstrt) then
        !
        nprnt = nprnt + 1
        !
        write(mnout1,'(///a7,g12.5,a6)') 'time = ',uznow,' hours'
        write(mnout2,'(///a7,g12.5,a6)') 'time = ',uznow,' hours'
        !
        !        output for specific cells
        !do noutl = 1,nout
        !    n1 = noutel(noutl)
        !    n2 = noutce(noutl)
        !    write (102+noutl,'(g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5 &
        !    ,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5,1x,g12.5)') uznow,chum1(n1,n2),clit1(n1,n2), &
        !    cman1(n1,n2),namm1(n1,n2),adammt(n1,n2),adnitt(n1,n2),addct(n1,n2),adornt(n1,n2),mintot(n1,n2),ntrtot(n1, &
        !    n2),plnitt(n1,n2),plammt(n1,n2),detot(n1,n2),imammt(n1,n2),imnitt(n1,n2),stot(n1,n2)
        !    988   continue
        !enddo
        !
        write (mnout2,'(a28,g16.8)') 'total nitrogen (kg n m-2) = ',totn/tarea
        write (mnout2,'(a33,g16.8)')'total nitrogen added (kg n m-2)= ',totadn/tarea
        write (mnout2,'(a32,g16.8)')'total nitrogen lost (kg n m-2) = ',totlos/tarea
        write (mnout1,'(a26,g16.8)') 'total carbon (kg c m-2) = ',totc/tarea
        write (mnout1, '(a32,g16.8)')'total carbon added (kg c m-2) = ',totadc/tarea
        write (mnout1, '(a28,g16.8)')'total co2 lost (kg c m-2) = ',totco2/tarea
        !
    endif
    !
end subroutine mnout



!> Calculates potential plant nitrogen uptake by rooted cell.
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
!> The first call only reads this file, writes the title to `MNOUTPL`, closes
!> both units, and initialises saved plant-mixture and mass state. Potential
!> uptake is calculated on later calls, after `PLUP` has been reset to zero over
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
!> element, a hard-coded second plant type, saved state across calls, GOTO-based
!> interpolation, and limited validation of the `MNPL` canopy-density table.
!> As implemented, the canopy-density table read loop stores all table values in
!> `CDI(NV,*)` and `CDIT(NV,*)` rather than `CDI(i,*)` and `CDIT(i,*)`, and the
!> saved `ISCROP` flags are not explicitly initialised before their first
!> possible use.
!> Good cleanup targets are to move plant-mixture setup into input data, replace
!> hard-coded constants with documented parameters, isolate interpolation in a
!> small checked helper, and add tests for crop reset, table extrapolation, and
!> mixed-vegetation uptake partitioning.
!> @endnote
subroutine mnplant(mnpl,mnoutpl,ncetop,nel,nlf,nv,ncolmb,nrd,nvc,rhopl,delone,dxqq,dyqq,deltaz,plai,rdf,dtuz, &
    uznow,clai)
    integer mnpl                    !! Plant-uptake input unit.
    integer mnoutpl                 !! Plant nitrogen output unit.
    integer ncetop                  !! Top soil-cell index.
    integer nel                     !! Number of elements.
    integer nlf                     !! Number of overland/channel links excluded from land-column uptake.
    integer nv                      !! Number of vegetation types.
    integer ncolmb(nelee)           !! Lowest active soil cell in each land-column element.
    integer nrd(nv)                 !! Rooting depth in cell counts by vegetation type.
    integer nvc(nelee)              !! Vegetation type index by element.
    double precision rhopl          !! Plant dry-matter density used by uptake calculation.
    double precision delone(npltee) !! Initial plant biomass/cover scaling by plant type.
    double precision dxqq(nelee)    !! Element width.
    double precision dyqq(nelee)    !! Element length.
    double precision deltaz(llee,nel) !! Cell thickness by cell and element.
    double precision plai(nv)       !! Plant leaf-area index by vegetation type.
    double precision rdf(nv,llee)   !! Root density fraction by vegetation type and cell.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision uznow          !! Current unsaturated-zone simulation time.
    double precision clai(nv)       !! Current canopy leaf-area index by vegetation type.
    ! locals
    !
    !     * maximum number of values in the input data for canopy density
    integer nvalee
    parameter (nvalee=30)
    !
    !     * those saved
    integer nvalue(npltee),pass
    integer npl(nelee),npltyp(nelee,npelee)
    double precision cdi(npltee,nvalee),cdit(npltee,nvalee)
    double precision claimx(npltee)
    double precision croptm(nelee,npelee)
    double precision gmcpbb(nelee,npelee)
    double precision massb(nelee,npelee)
    double precision pfone(nelee,npelee)
    logical iscrop(nelee,npelee)
    !     * those not saved
    integer jplty,ndata,nelm,nplant,nrbot,ntb
    integer i
    integer idum(1)
    double precision cdfnc,chgmas,fn,massbo,tmsncr
    double precision dum,dum2
    double precision dummy(nvalee*2)
    !      * temporary variable to test this subroutine
    character msg*32
    character*200  cdum(1)
    !
    save nvalue,pass
    save npl,npltyp
    save cdi,cdit
    save claimx,croptm,gmcpbb,massb,pfone
    save iscrop
    !
    data pass/ 0 /
    !
    !----------------------------------------------------------------------*
    !
    pass = pass + 1
    if (pass==1) then
        !
        !----------------------------------------------------------------------*
        !     initialising step
        !----------------------------------------------------------------------*
        !
        !        extra data for the canopy density index
        !        this is used to correct the canopy leaf area index so that the
        !        plant uptake of nitrogen is more accurate
        !
        !        * check status of data file
        call alred2(0,mnpl,mnoutpl,'mnptin')
        !
        !        * print title for data file
        call alredc(0,mnpl,mnoutpl,':MNP1',1,1,cdum)
        write (mnoutpl, '(/1x,a/)') cdum
        !
        do i = 1,nv
            call alredi ( 0,mnpl,mnoutpl,':MNP10',1,1,idum )
            nvalue(i) = idum(1)
            ndata = idum(1)*2
            call alredf(0,mnpl,mnoutpl,':MNP11',ndata,1,dummy)
            do ntb = 1,idum(1)
                cdi(nv,ntb) = dummy(2*ntb-1)
                cdit(nv,ntb) = dummy(2*ntb)
            enddo
        enddo
        close (mnpl)
        close (mnoutpl)
        do nelm = nlf+1,nel
            !
            ! **************** temporary
            !               hard code the maximum leaf area index
            do i = 1,npltee
                claimx(i) = 2.0d0
            enddo
            !
            ! *************** temporary
            !                 set number of plant types on each column
            !                 temporarily, only two plant types are allowed on each
            !                 column and the total plai is one
            !                 second plant type number is set in block data
            !
            npltyp(nelm,1) = nvc(nelm)
            pfone(nelm,1) = plai(npltyp(nelm,1))
            if ( pfone(nelm,1) >= 0.99 ) then
                npl(nelm) = 1
            else
                pfone(nelm,2) = 1.0d0 - pfone(nelm,1)
                npl(nelm) = 2
            endif
            !* sb 5/3/01 add data from pldat.f
            !* all second plant types on a grid square are equal to 1i=1,nel
   	        do I=1,nel
	            NPLTYP(i,2)= 1
	        enddo


            do nplant=1,npl(nelm)
                !                 plant type number
                jplty = npltyp(nelm,nplant)
                gmcpbb(nelm,nplant) = clai(jplty) * delone(jplty)/ claimx(jplty)
                !                 initialise for mass in compartment b
                massb(nelm,nplant) = gmcpbb(nelm,nplant)* pfone(nelm,nplant) * dxqq(nelm) * dyqq(nelm) * rhopl
                croptm(nelm,nplant) = 0d0
            enddo
            !
        enddo
        !
        !      * temporary output to test this subroutine
        !       i = 0
        !       do 787 nelm = 1,1
        !          do 788 ncl = 69,64,-5
        !              i = i + 1
        !              write (msg,9000) nelm,ncl
        !              open(120+i,file = msg)
        !              write (120+i,'(2a13)') 'time','pot-plant-up'
        !  788  continue
        !  787  continue
        !
        ! 9000  format ( '../cep2.',i3,'.',i2)
        !      * end of temporary code
        !
        !----------------------------------------------------------------------*
        !     simulation step
        !----------------------------------------------------------------------*
        !
    else
        do nelm = nlf+1,nel
            do nce = ncolmb(nelm),ncetop
                plup(nelm,nce) = 0d0
            enddo
        enddo
        !
        do nelm = nlf+1,nel
            do nplant = 1,npl(nelm)
                !              plant type number
                jplty = npltyp(nelm,nplant)
                !             linear interpolation to calculate the canopy density
                !             function at this particular time
                do i = 2,nvalue(jplty)
                    if ((uznow/24.0)<cdit(jplty,i)) then
                        dum = (cdi(jplty,i)-cdi(jplty,i-1))/ (cdit(jplty,i) -  cdit(jplty,i-1))
                        dum2 = uznow/24.0 - cdit(jplty,i-1)
                        cdfnc = cdi(jplty,i-1) + dum*dum2
                        goto 460
                        !                   ********
                    endif
                enddo
                !
                !             if the time is greater than any specified in the file
                !             then cdfnc is set to 1.0
                cdfnc = 1.0
                !                 number of bottom rooted cell
                460          nrbot = ncetop - nrd(jplty)
                !
                gmcpbb(nelm,nplant) = clai(jplty)*delone(jplty)*cdfnc/ claimx(jplty)
                massbo = massb(nelm,nplant)
                massb(nelm,nplant) = gmcpbb(nelm,nplant)* pfone(nelm,nplant) * dxqq(nelm) * dyqq(nelm) * rhopl
                chgmas = (massb(nelm,nplant)-massbo) /dtuz
                !
                !              * there has been cropping
                if (chgmas<0d0) then
                    iscrop(nelm,nplant) = .true.
                    !
                    !              * plant uptake only if plants are growing
                elseif (clai(jplty)>0d0) then
                    !
                    !                 * first emergence of the crop since cropping
                    if (iscrop(nelm,nplant)) then
                        croptm(nelm,nplant) = uznow
                        iscrop(nelm,nplant) = .false.
                    endif
                    !
                    tmsncr = uznow - croptm(nelm,nplant)
                    !                 * proportion of nitrate depends on the age of crop
                    if (tmsncr<360)  then
                        fn = 0.022
                    elseif (tmsncr<720) then
                        fn = 0.017
                    elseif (tmsncr<1080) then
                        fn = 0.015
                    else
                        fn = 0.012
                    endif
                    !
                    do nce = nrbot,ncetop
                        ndum = ncetop - nce + 1
                        plup(nelm,nce) = plup(nelm,nce)+ chgmas * fn * rdf(jplty,ndum)/(deltaz(nce,nelm) * &
                         dxqq(nelm) * dyqq(nelm))
                    enddo
                endif
            enddo
        enddo
        !      * temporary output to test this subroutine
        !         i = 0
        !         do 987 nelm = 1,1
        !            do 988 ncl = 69,64,-5
        !               i = i + 1
        !               write (120+i,'(g12.5,1x,g12.5)') uznow,plup(nelm,ncl)
        !  988    continue
        !  987    continue
        !      * end of temporary code
    endif
end subroutine mnplant



!> Reads static mineral nitrogen input data.
!>
!> `mnred1` reads the MND file once during [[mnmain]] initialisation, echoes the
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
!> @note `Q10M` and `Q10N` are not assigned here when `ISQ10` is false, although
!> [[mnerr2]] still receives those variables.
!> @endnote
subroutine mnred1(mnd,mnpr,nel,nelee,nlf,nlfee,nmneee,nmntee,ns,nx,nxee,ny,icmbk,icmref,icmxy,bexbk,linkns,nbotce &
    ,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e,celem,kd1elm,kd2elm,khelem,klelem,kmelem,knelem, &
    kvelem,naelem,nmn15t,nmn17t,nmn19t,nmn21t,nmn23t,nmn25t,nmn27t,nmn43t,nmn53t,ammddr,ammwdr,clitfr,cnrbio,cnrhum, &
    cnrlit,fe,fh,gnn,kplamm,kplnit,kuamm,kunit,mncref,nitddr,nitwdr,q10m,q10n,cconc,cdpth,ctottp,damhlf,dchlf,kd1cnc, &
    kd1dth,kd2cnc,kd2dth,kddsol,khconc,khdpth,klconc,kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth, &
    namtop,isiccd,isiamd,isq10,idum,dummy )

    ! externals
    ! nyee needed as alallf changed to require it
    !use sglobal, only : nyee, error
    !use mod_load_filedata , only : alalli, alredc,alredl,alredf,alallf,alredi,alred2
    !       external alallf,alredc,alredf,alredl,alred2,error,alalli
    integer mnd                     !! Static MND input unit.
    integer mnpr                    !! MN diagnostic output unit.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links.
    integer nlfee                   !! Link-array dimension.
    integer nmneee                  !! Maximum number of MN category entries.
    integer nmntee                  !! Maximum number of MN table entries.
    integer ns                      !! Number of soil types.
    integer nx                      !! Number of grid columns.
    integer nxee                    !! Grid-column array dimension.
    integer ny                      !! Number of grid rows.
    integer icmbk(nlfee,2)          !! Bank-element numbers for each channel link.
    integer icmref(nelee,4,2:2)     !! Neighbour reference map.
    integer icmxy(nxee,ny)          !! Element number at each grid location.
    logical bexbk                   !! True when bank elements are represented.
    logical linkns(nlfee)           !! True for north-south channel links.
    integer nbotce                  !! Lowest cell included when bottom-cell truncation is active.
    integer nmn15e                  !! Number of humus category entries.
    integer nmn17e                  !! Number of litter category entries.
    integer nmn19e                  !! Number of manure category entries.
    integer nmn21e                  !! Number of nitrification category entries.
    integer nmn23e                  !! Number of volatilisation category entries.
    integer nmn25e                  !! Number of KD1 denitrification category entries.
    integer nmn27e                  !! Number of KD2 denitrification category entries.
    integer nmn43e                  !! Number of initial-carbon category entries.
    integer nmn53e                  !! Number of initial-ammonium category entries.
    integer celem(nlf+1:nel)        !! Initial-carbon category by element.
    integer kd1elm(nlf+1:nel)       !! KD1 denitrification category by element.
    integer kd2elm(nlf+1:nel)       !! KD2 denitrification category by element.
    integer khelem(nlf+1:nel)       !! Humus decomposition category by element.
    integer klelem(nlf+1:nel)       !! Litter decomposition category by element.
    integer kmelem(nlf+1:nel)       !! Manure decomposition category by element.
    integer knelem(nlf+1:nel)       !! Nitrification category by element.
    integer kvelem(nlf+1:nel)       !! Volatilisation category by element.
    integer naelem(nlf+1:nel)       !! Initial-ammonium category by element.
    integer nmn15t(nmneee)          !! Humus table length by category.
    integer nmn17t(nmneee)          !! Litter table length by category.
    integer nmn19t(nmneee)          !! Manure table length by category.
    integer nmn21t(nmneee)          !! Nitrification table length by category.
    integer nmn23t(nmneee)          !! Volatilisation table length by category.
    integer nmn25t(nmneee)          !! KD1 table length by category.
    integer nmn27t(nmneee)          !! KD2 table length by category.
    integer nmn43t(nmneee)          !! Initial-carbon table length by category.
    integer nmn53t(nmneee)          !! Initial-ammonium table length by category.
    double precision ammddr         !! Dry ammonium deposition rate.
    double precision ammwdr         !! Wet ammonium deposition coefficient.
    double precision clitfr         !! Fraction of initial organic carbon assigned to litter.
    double precision cnrbio         !! Biomass carbon-to-nitrogen ratio.
    double precision cnrhum         !! Humus carbon-to-nitrogen ratio.
    double precision cnrlit         !! Initial litter carbon-to-nitrogen ratio.
    double precision fe             !! Efficiency fraction for organic carbon turnover.
    double precision fh             !! Humification fraction.
    double precision gnn            !! Nonlinear ammonium adsorption exponent.
    double precision kplamm         !! First-order ammonium plant-uptake limit.
    double precision kplnit         !! First-order nitrate plant-uptake limit.
    double precision kuamm          !! First-order ammonium immobilisation limit.
    double precision kunit          !! First-order nitrate immobilisation limit.
    double precision mncref         !! Reference nitrogen concentration.
    double precision nitddr         !! Dry nitrate deposition rate.
    double precision nitwdr         !! Wet nitrate deposition coefficient.
    double precision q10m           !! Q10 coefficient for mineralisation.
    double precision q10n           !! Q10 coefficient for nitrification.
    double precision cconc(nmneee,nmntee)  !! Initial-carbon profile values.
    double precision cdpth(nmneee,nmntee)  !! Initial-carbon profile depths.
    double precision ctottp(nlf+1:nel)     !! Top total-carbon value for decay initialisation.
    double precision damhlf(nlf+1:nel)     !! Ammonium decay half-depth by element.
    double precision dchlf(nlf+1:nel)      !! Carbon decay half-depth by element.
    double precision kd1cnc(nmneee,nmntee) !! KD1 denitrification profile values.
    double precision kd1dth(nmneee,nmntee) !! KD1 denitrification profile depths.
    double precision kd2cnc(nmneee,nmntee) !! KD2 denitrification profile values.
    double precision kd2dth(nmneee,nmntee) !! KD2 denitrification profile depths.
    double precision kddsol(ns)            !! Soil ammonium adsorption coefficient.
    double precision khconc(nmneee,nmntee) !! Humus decomposition profile values.
    double precision khdpth(nmneee,nmntee) !! Humus decomposition profile depths.
    double precision klconc(nmneee,nmntee) !! Litter decomposition profile values.
    double precision kldpth(nmneee,nmntee) !! Litter decomposition profile depths.
    double precision kmconc(nmneee,nmntee) !! Manure decomposition profile values.
    double precision kmdpth(nmneee,nmntee) !! Manure decomposition profile depths.
    double precision knconc(nmneee,nmntee) !! Nitrification profile values.
    double precision kndpth(nmneee,nmntee) !! Nitrification profile depths.
    double precision kvconc(nmneee,nmntee) !! Volatilisation profile values.
    double precision kvdpth(nmneee,nmntee) !! Volatilisation profile depths.
    double precision naconc(nmneee,nmntee) !! Initial-ammonium profile values.
    double precision nadpth(nmneee,nmntee) !! Initial-ammonium profile depths.
    double precision namtop(nlf+1:nel)     !! Top ammonium value for decay initialisation.
    logical isiccd                  !! True when initial carbon uses decay-function input.
    logical isiamd                  !! True when initial ammonium uses decay-function input.
    logical isq10                   !! True when Q10 temperature response is selected.
    integer idum(nelee)             !! Integer workspace for spatial reads.
    double precision dummy(nelee)   !! Floating-point workspace for spatial reads.
    ! locals etc.
    !
    integer       fatal,nc,ncat,ndata,nmnt(1),ntb
    parameter     (fatal = 1)
    character*200  cdum(1)
    logical       ldum(1)
    !
    !
    !-------------------------------------------------------------------*
    !
    !
    ! preliminaries
    ! -------------
    !
    !     * check status of data file
    call alred2(0,mnd,mnpr,'MND')
    !
    !     * print title for nitrate simulation
    call alredc(0,mnd,mnpr,':MN01',1,1,cdum)
    write (mnpr, '(/1x,a/)') cdum
    !
    !
    ! decomposition parameter rates
    ! -----------------------------
    !
    !     * decomposition parameters for ammonium immobilisation,
    !     * plant uptake of ammonium,immobilisation of nitrate
    !     * and plant uptake of nitrate
    call alredf(0,mnd,mnpr,':MN11',4,1,dummy)
    kuamm = dummy(1)
    kplamm = dummy(2)
    kunit = dummy(3)
    kplnit = dummy(4)
    !
    !
    ! further parameters
    ! ------------------
    !
    !     * organic matter effeciency fraction and humification fraction
    !     * and carbon to nitrogen ratio in the biomass and humus
    call alredf(0,mnd,mnpr,':MN12',4,1,dummy)
    fe = dummy(1)
    fh = dummy(2)
    cnrbio = dummy(3)
    cnrhum = dummy(4)
    !
    !     * dry and wet deposition rates of ammonium and nitrate
    call alredf(0,mnd,mnpr,':MN13',4,1,dummy)
    ammddr = dummy(1)
    ammwdr = dummy(2)
    nitddr = dummy(3)
    nitwdr = dummy(4)
    !
    !     * reference contaminant concentration
    call alredf(0,mnd,mnpr,':MN14',1,1,dummy)
    mncref = dummy(1)
    !
    !
    ! spatially varying decomposition parameter rates
    ! -----------------------------------------------
    !
    !    khum
    !    ----
    !    * find out how many typical element catagories
    call alredi ( 0,mnd,mnpr,':MN15a',1,1,idum )
    nmn15e = idum(1)
    if ((nmn15e>nmneee).or.(nmn15e<=0)) then
        call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn15 in mn data file')
    endif
    !
    !        * read the catagory type for each element into the element
    !        * number
    call alalli(nmn15e,mnd,mnpr,':MN15b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns,khelem(nlf+1) &
    ,idum)
    !
    !        * table of values for each typical element
    do nc = 1,nmn15e
        call alredi(0,mnd,mnpr,':MN16a',1,1,nmnt)
        !
        nmn15t(nc) = nmnt(1)
        if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
            call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn16a in mn data file')
        endif
        !
        ndata = nmnt(1)*2
        call alredf(0,mnd,mnpr,':MN16b',ndata,1,dummy)
        do ntb = 1,nmnt(1)
            khdpth(nc,ntb) = dummy(2*ntb-1)
            khconc(nc,ntb) = dummy(2*ntb)
        enddo
    enddo
    !
    !    klit
    !    ----
    !    * find out how many typical element catagories
    call alredi ( 0,mnd,mnpr,':MN17a',1,1,idum )
    nmn17e = idum(1)
    if ((nmn17e>nmneee).or.(nmn17e<=0)) then
        call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn17 in mn data file')
    endif
    !
    !        * read the catagory type for each element into the element
    !        * number
    call alalli(nmn17e,mnd,mnpr,':MN17b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns,klelem(nlf+1) &
    ,idum)
    !
    !        * table of values for each typical element
    do nc = 1,nmn17e
        call alredi(0,mnd,mnpr,':MN18a',1,1,nmnt)
        !
        nmn17t(nc) = nmnt(1)
        if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
            call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn18a in mn data file')
        endif
        !
        ndata = nmnt(1)*2
        call alredf(0,mnd,mnpr,':MN18b',ndata,1,dummy)
        do ntb = 1,nmnt(1)
            kldpth(nc,ntb) = dummy(2*ntb-1)
            klconc(nc,ntb) = dummy(2*ntb)
        enddo
    enddo
    !
    !    kman
    !    ----
    !    * find out how many typical element catagories
    call alredi ( 0,mnd,mnpr,':MN19a',1,1,idum )
    nmn19e = idum(1)
    if ((nmn19e>nmneee).or.(nmn19e<=0)) then
        call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn19 in mn data file')
    endif
    !
    !        * read the catagory type for each element into the element
    !        * number
    call alalli(nmn19e,mnd,mnpr,':MN19b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns,kmelem(nlf+1) &
    ,idum)
    !
    !        * table of values for each typical element
    do nc = 1,nmn19e
        call alredi(0,mnd,mnpr,':MN20a',1,1,nmnt)
        !
        nmn19t(nc) = nmnt(1)
        if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
            call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn20a in mn data file')
        endif
        !
        ndata = nmnt(1)*2
        call alredf(0,mnd,mnpr,':MN20b',ndata,1,dummy)
        do ntb = 1,nmnt(1)
            kmdpth(nc,ntb) = dummy(2*ntb-1)
            kmconc(nc,ntb) = dummy(2*ntb)
        enddo
    enddo
    !
    !    knit
    !    ----
    !    * find out how many typical element catagories
    call alredi ( 0,mnd,mnpr,':MN21a',1,1,idum )
    nmn21e = idum(1)
    if ((nmn21e>nmneee).or.(nmn21e<=0)) then
        call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn21 in mn data file')
    endif
    !
    !        * read the catagory type for each element into the element
    !        * number
    call alalli(nmn21e,mnd,mnpr,':MN21b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns,knelem(nlf+1) &
    ,idum)
    !
    !        * table of values for each typical element
    do nc = 1,nmn21e
        call alredi(0,mnd,mnpr,':MN22a',1,1,nmnt)
        !
        nmn21t(nc) = nmnt(1)
        if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
            call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn22a in mn data file')
        endif
        !
        ndata = nmnt(1)*2
        call alredf(0,mnd,mnpr,':MN22b',ndata,1,dummy)
        do ntb = 1,nmnt(1)
            kndpth(nc,ntb) = dummy(2*ntb-1)
            knconc(nc,ntb) = dummy(2*ntb)
        enddo
    enddo
    !
    !    kvol
    !    ----
    !    * find out how many typical element catagories
    call alredi ( 0,mnd,mnpr,':MN23a',1,1,idum )
    nmn23e = idum(1)
    if ((nmn23e>nmneee).or.(nmn23e<=0)) then
        call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn23 in mn data file')
    endif
    !
    !        * read the catagory type for each element into the element
    !        * number
    call alalli(nmn23e,mnd,mnpr,':MN23b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns,kvelem(nlf+1) &
    ,idum)
    !
    !        * table of values for each typical element
    do nc = 1,nmn23e
        call alredi(0,mnd,mnpr,':MN24a',1,1,nmnt)
        !
        nmn23t(nc) = nmnt(1)
        if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
            call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn24a in mn data file')
        endif
        !
        ndata = nmnt(1)*2
        call alredf(0,mnd,mnpr,':MN24b',ndata,1,dummy)
        do ntb = 1,nmnt(1)
            kvdpth(nc,ntb) = dummy(2*ntb-1)
            kvconc(nc,ntb) = dummy(2*ntb)
        enddo
    enddo
    !
    !    kd1
    !    ----
    !    * find out how many typical element catagories
    call alredi ( 0,mnd,mnpr,':MN25a',1,1,idum )
    nmn25e = idum(1)
    if ((nmn25e>nmneee).or.(nmn25e<=0)) then
        call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn25 in mn data file')
    endif
    !
    !        * read the catagory type for each element into the element
    !        * number
    call alalli(nmn25e,mnd,mnpr,':MN25b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns,kd1elm(nlf+1) &
    ,idum)
    !
    !        * table of values for each typical element
    do nc = 1,nmn25e
        call alredi(0,mnd,mnpr,':MN26a',1,1,nmnt)
        !
        nmn25t(nc) = nmnt(1)
        if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
            call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn26a in mn data file')
        endif
        !
        ndata = nmnt(1)*2
        call alredf(0,mnd,mnpr,':MN26b',ndata,1,dummy)
        do ntb = 1,nmnt(1)
            kd1dth(nc,ntb) = dummy(2*ntb-1)
            kd1cnc(nc,ntb) = dummy(2*ntb)
        enddo
    enddo
    !
    !    kd2
    !    ----
    !    * find out how many typical element catagories
    call alredi ( 0,mnd,mnpr,':MN27a',1,1,idum )
    nmn27e = idum(1)
    if ((nmn27e>nmneee).or.(nmn27e<=0)) then
        call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn27 in mn data file')
    endif
    !
    !        * read the catagory type for each element into the element
    !        * number
    call alalli(nmn27e,mnd,mnpr,':MN27b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns,kd2elm(nlf+1) &
    ,idum)
    !
    !        * table of values for each typical element
    do nc = 1,nmn27e
        call alredi(0,mnd,mnpr,':MN28a',1,1,nmnt)
        !
        nmn27t(nc) = nmnt(1)
        if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
            call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn28a in mn data file')
        endif
        !
        ndata = nmnt(1)*2
        call alredf(0,mnd,mnpr,':MN28b',ndata,1,dummy)
        do ntb = 1,nmnt(1)
            kd2dth(nc,ntb) = dummy(2*ntb-1)
            kd2cnc(nc,ntb) = dummy(2*ntb)
        enddo
    enddo
    !
    ! ammonium adsorption
    ! -------------------
    !
    !     * kd parameter
    call alredf(0,mnd,mnpr,':MN30',ns,1,kddsol)
    !
    !     * power parameter n
    call alredf(0,mnd,mnpr,':MN31',1,1,gnn)
    !
    !
    ! temperature effect within the soil
    ! ----------------------------------
    !
    !     * for the environmental reduction factor for temperature is a q10
    !     * function being used ? if it is the q10 factors are needed
    call alredl(0,mnd,mnpr,':MN35',1,1,ldum)
    isq10 = ldum(1)
    if (isq10) then
        call alredf(0,mnd,mnpr,':MN35a',2,1,dummy)
        q10m = dummy(1)
        q10n = dummy(2)
    endif
    !
    !
    ! values used to calculate the initial concentrations in the organic pls
    ! ----------------------------------------------------------------------
    !     * for the initial conditions of the carbon litter pool either
    !     * a decay function for each element or an typical elem. is defined
    call alredl(0,mnd,mnpr,':MN40',1,1,ldum)
    isiccd = ldum(1)
    if (isiccd) then
        !
        !        * total carbon concentration at the ground surface
        call alallf(1,1,0,mnd,mnpr,':MN41',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns,ncat, &
        ctottp,idum,dummy)
        !        * depth at which carbon conc. reduced by half
        call alallf(1,1,0,mnd,mnpr,':MN42',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns,ncat, &
        dchlf,idum,dummy)
        !
    else
        !
        !       * find out how many typical element catagories
        call alredi ( 0,mnd,mnpr,':MN43a',1,1,idum )
        nmn43e = idum(1)
        if ((nmn43e>nmneee).or.(nmn43e<=0)) then
            call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn43 in mn data file')
        endif
        !
        !        * read the catagory type for each element into the element
        !        * number
        call alalli(nmn43e,mnd,mnpr,':MN43b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns,celem(nlf &
        +1),idum)
        !
        !        * table of values for each typical element
        do nc = 1,nmn43e
            call alredi(0,mnd,mnpr,':MN44a',1,1,nmnt)
            !
            nmn43t(nc) = nmnt(1)
            if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
                call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn44a in mn data file')
            endif
            !
            ndata = nmnt(1)*2
            call alredf(0,mnd,mnpr,':MN44b',ndata,1,dummy)
            do ntb = 1,nmnt(1)
                cdpth(nc,ntb) = dummy(2*ntb-1)
                cconc(nc,ntb) = dummy(2*ntb)
            enddo
        enddo
        !
        !
    endif
    !
    !     * proportion of the carbon in the litter and biomass pool
    call alredf(0,mnd,mnpr,':MN45',1,1,dummy)
    clitfr = dummy(1)
    !
    !     * carbon to nitrgen ratio in the litter fraction
    call alredf(0,mnd,mnpr,':MN46',1,1,dummy)
    cnrlit = dummy(1)
    !
    !
    !
    !values used to calculate the initial concentrations in the ammoniumpool
    ! ----------------------------------------------------------------------
    !
    !
    call alredl(0,mnd,mnpr,':MN50',1,1,ldum)
    isiamd = ldum(1)
    if (isiamd) then
        !
        !        * total ammonium concentration at the ground surface
        call alallf(1,1,0,mnd,mnpr,':MN51',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns,ncat, &
        namtop,idum,dummy)
        !
        !        * depth at which ammonium conc. reduced by half
        call alallf(1,1,0,mnd,mnpr,':MN52',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns,ncat, &
        damhlf,idum,dummy)
        !
    else
        !
        !       * find out how many typical element catagories
        call alredi ( 0,mnd,mnpr,':MN53a',1,1,idum )
        nmn53e = idum(1)
        if ((nmn53e>nmneee).or.(nmn53e<=0)) then
            call error(fatal, 3090,mnpr,0,0,'error in ncat in :mn53 in mn data file')
        endif
        !
        !        * read the catagory type for each element into the element
        !        * number
        call alalli(nmn53e,mnd,mnpr,':MN53b',nel,nlf,nx,ny,nelee,nlfee,nxee,icmxy,icmbk,icmref,bexbk,linkns, &
        naelem(nlf+1),idum)
        !
        !        * table of values for each typical element
        do nc = 1,nmn53e
            call alredi(0,mnd,mnpr,':MN54a',1,1,nmnt)
            !
            nmn53t(nc) = nmnt(1)
            if ((nmnt(1)>nmntee).or.(nmnt(1)<=0)) then
                call error(fatal, 3091,mnpr,0,0,'error in nmnt in :mn54a in mn data file')
            endif
            !
            ndata = nmnt(1)*2
            call alredf(0,mnd,mnpr,':MN54b',ndata,1,dummy)
            do ntb = 1,nmnt(1)
                nadpth(nc,ntb) = dummy(2*ntb-1)
                naconc(nc,ntb) = dummy(2*ntb)
            enddo
        enddo
        !
    endif
    !
    !cell below which no nitrogen transformations are considered
    !-----------------------------------------------------------
    !
    call alredi(0,mnd,mnpr,':MN60',1,1,nbotce)
    !
    !
    ! epilogue
    ! --------
    !
    call alred2(1,mnd,mnpr,'MND')
    !
end subroutine mnred1



!> Reads scheduled nitrogen and carbon additions for the current timestep.
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
subroutine mnred2 ( mnfc,mnfn,mnpr,nel,nelee,nlf,nlfee,nx,nxee,ny,icmbk,icmref,icmxy,dtuz,tih,uznow,bexbk,linkns, &
    cdpthb,cltfct,cmnfct,cnral,cnram,ctot,namfct,ndpthb,ntot,isaddc,isaddn,idum,dummy)

    !
    ! externals
    !       double precision                     hour
    !use utilsmod, only: hour_from_date
    !use sglobal, only : nyee
    !use mod_load_filedata , only : alred2,alredi,alallf
    !       external         alallf,alredi,alred2,hour
    integer mnfc                    !! Scheduled carbon-addition input unit.
    integer mnfn                    !! Scheduled nitrogen-addition input unit.
    integer mnpr                    !! MN diagnostic output unit.
    integer nel                     !! Number of elements.
    integer nelee                   !! Element-array dimension.
    integer nlf                     !! Number of overland/channel links.
    integer nlfee                   !! Link-array dimension.
    integer nx                      !! Number of grid columns.
    integer nxee                    !! Grid-column array dimension.
    integer ny                      !! Number of grid rows.
    integer icmbk(nlfee,2)          !! Bank-element numbers for each channel link.
    integer icmref(nelee,4,2:2)     !! Neighbour reference map.
    integer icmxy(nxee,ny)          !! Element number at each grid location.
    double precision dtuz           !! Unsaturated-zone timestep in seconds.
    double precision tih            !! Initial simulation time in hours.
    double precision uznow          !! Current unsaturated-zone simulation time.
    logical bexbk                   !! True when bank elements are represented.
    logical linkns(nlfee)           !! True for north-south channel links.
    double precision cdpthb(nlf+1:nel) !! Carbon banding depth.
    double precision cltfct(nlf+1:nel) !! Litter fraction of added carbon.
    double precision cmnfct(nlf+1:nel) !! Manure fraction of added carbon.
    double precision cnral(nlf+1:nel)  !! Carbon-to-nitrogen ratio for added litter.
    double precision cnram(nlf+1:nel)  !! Carbon-to-nitrogen ratio for added manure.
    double precision ctot(nlf+1:nel)   !! Total external carbon addition.
    double precision namfct(nlf+1:nel) !! Ammonium fraction of added inorganic nitrogen.
    double precision ndpthb(nlf+1:nel) !! Nitrogen banding depth.
    double precision ntot(nlf+1:nel)   !! Total external inorganic nitrogen addition.
    logical isaddc                  !! True when a carbon-addition event is active.
    logical isaddn                  !! True when a nitrogen-addition event is active.
    integer idum(nelee)             !! Integer workspace for spatial reads.
    double precision dummy(nelee)   !! Floating-point workspace for spatial reads.
    ! locals etc.
    !
    integer       intimc,intimn,ncat,pass
    integer       time(5)
    !
    !
    save intimc,intimn,pass
    !
    data pass / 0 /
    !
    !-------------------------------------------------------------------*
    !
    pass = pass + 1
    !
    ! 1. check data files are open and read first input times
    ! -------------------------------------------------------
    if (pass==1) then
        !        * check status of nitrogen fertilizer data file
        call alred2(0,mnfn,mnpr,'MNFM')
        !
        !        * time of first nitrogen fertilizer addition
        call alredi(0,mnfn,mnpr,':MNFN01',5,1,time)
        intimn = hour_from_date(time(1),time(2),time(3),time(4),time(5))-tih
        !
        !        * check status of carbon fertilizer data file
        call alred2(0,mnfc,mnpr,'MNFC')
        !
        !        * time of first carbon fertilizer addition
        call alredi(0,mnfc,mnpr,':MNFC01',5,1,time)
        intimc = hour_from_date(time(1),time(2),time(3),time(4),time(5))-tih
        !
    endif
    !
    !
    ! 2. read nitrogen data file if fertilization occurs in this timestep
    ! -------------------------------------------------------------------
    !
    if ((uznow + dtuz/3.6d3)>intimn) then
        !
        isaddn = .true.
        !        * total nitrogen fertilizer in each element (kg n m-2)
        call alallf(1,1,0,mnfn,mnpr,':MNFN11',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,ntot,idum,dummy)
        !
        !        * depth the fertilizer is banded over (m)
        call alallf(1,1,0,mnfn,mnpr,':MNFN21',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,ndpthb,idum,dummy)
        !
        !        * ammonium fraction (the remainder is nitrate )
        call alallf(1,1,0,mnfn,mnpr,':MNFN31',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,namfct,idum,dummy)
        !
        !        * time of next nitrogen fertilizer addition
        call alredi(0,mnfn,mnpr,':MNFN01',5,1,time)
        intimn = hour_from_date(time(1),time(2),time(3),time(4),time(5))-tih
        !
    else
        isaddn = .false.
        !
    endif
    !
    !
    ! 3. read carbon data file if fertilization occurs in this timestep
    ! -----------------------------------------------------------------
    !
    !
    if ((uznow + dtuz/3.6d3)>intimc) then
        !
        isaddc = .true.
        !
        !        * total carbon fertilizer in each element (kg n m-2)
        call alallf(1,1,0,mnfc,mnpr,':MNFC11',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,ctot,idum,dummy)
        !
        !        * depth the fertilizer is banded over (m)
        call alallf(1,1,0,mnfc,mnpr,':MNFC21',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,cdpthb,idum,dummy)
        !
        !        * litter fraction
        call alallf(1,1,0,mnfc,mnpr,':MNFC31',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,cltfct,idum,dummy)
        !
        !        * carbon/nitrogen ratio of the litter
        call alallf(1,1,0,mnfc,mnpr,':MNFC32',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,cnral,idum,dummy)
        !
        !        * manure fraction (the remainder from the litter and manure is humus)
        call alallf(1,1,0,mnfc,mnpr,':MNFC41',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,cmnfct,idum,dummy)
        !
        !        * carbon/nitrogen ratio of the manure
        call alallf(1,1,0,mnfc,mnpr,':MNFC42',nel,nlf,nx,ny,nelee,nlfee,nxee,nyee,icmxy,icmbk,icmref,bexbk,linkns, &
        ncat,cnram,idum,dummy)
        !
        !
        !        * time of next carbon fertilizer addition
        call alredi(0,mnfc,mnpr,':MNFC01',5,1,time)
        intimc = hour_from_date(time(1),time(2),time(3),time(4),time(5))-tih
        !
    else
        isaddc = .false.
        !
    endif
    !
end subroutine mnred2



!> Updates soil temperature for the MN environmental response factors.
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
subroutine mntemp (llee,ncetop,nel,nelee,nlf,nv,ncolmb,z2,deltaz,zvsnod,dtuz,ta)

    use utilsmod, only: tridag
    integer llee                 !! Maximum soil-cell dimension.
    integer ncetop               !! Top soil-cell index.
    integer nel                  !! Number of elements.
    integer nelee                !! Element-array dimension.
    integer nlf                  !! Number of overland/channel links excluded from land-column updates.
    integer nv                   !! Number of vegetation/meteorological temperature entries.
    integer ncolmb(nelee)        !! Lowest active soil cell in each land-column element.
    double precision z2          !! Vertical length scale for the temperature diffusion calculation.
    double precision deltaz(llee,nel) !! Cell thickness by cell and element.
    double precision zvsnod(llee,nel) !! Vertical node elevation/depth by cell and element.
    double precision dtuz        !! Unsaturated-zone timestep in seconds.
    double precision ta(nv)      !! Air temperature input; only the first value is used.
    ! locals etc
    !
    integer iel,nce,ncebot,ncells,nnum,nserch,num
    parameter (num = 11)
    !
    double precision celldp,cellfc,depthc
    double precision diff,diffga,kfct,grdtem
    double precision amat(num),bmat(num),cmat(num),depth(num)
    double precision rhs(num),ome(num), tempr(num), tempr1(num)
    parameter (depthc = 10)
    parameter (diff = 2.0d-5)
    parameter (diffga = 2.0d0)
    !
    save tempr
    !
    data tempr / num*12.0 /
    !
    !--------------------------------------------------------------------*
    kfct = diff * ((num-1)/z2) * ((num-1)/z2)
    !
    !     * ground temperature is equal to the air temperature plus a
    !     * constant value
    grdtem = ta(1) + diffga
    tempr1(1) = grdtem
    !
    !     * position in the matrix are one lower than in the column,
    !     * this is because the ground surface value is known
    rhs(1) = kfct*grdtem  + kfct*(-2*tempr(2)+tempr(3))
    rhs(num-1) = (tempr(num-1)-tempr(num))*kfct
    amat(1) = 0
    bmat(1) = 1 + 2*kfct*dtuz
    cmat(1) = -kfct*dtuz
    amat(num-1) = -kfct*dtuz
    bmat(num-1) = 1 + kfct*dtuz
    cmat(num-1) = 0
    do nce = 2,num-2
        amat(nce) = -kfct*dtuz
        bmat(nce) = 1 + 2*kfct*dtuz
        cmat(nce) = -kfct*dtuz
        rhs(nce) = kfct* (tempr(nce)-2*tempr(nce+1)+tempr(nce+2))
    enddo
    !
    call tridag(amat,bmat,cmat,rhs,ome,num-1)
    !
    !
    !     * new temperature at each node
    do nce = 2,num
        tempr1(nce) = tempr(nce) + ome(nce-1)*dtuz
    enddo
    !
    !     * depth of each node
    depth(1) = 0
    do nnum = 2,num
        depth(nnum) = depthc / (num-1) + depth(nnum - 1)
    enddo
    do 500 iel = nlf+1,nel
        ncebot = ncolmb(iel)
        nserch = 2
        do nce = ncetop,ncebot,-1
            !           * calculation of the depth of the cell
            if (nce==ncetop) then
                celldp = 0.5 * deltaz(nce,iel)
            else
                celldp = (zvsnod(nce+1,iel) - zvsnod(nce,iel)) + celldp
            endif
            !
            if (celldp>=depth(num)) then
                do ncells = nce,ncebot,-1
                    temp(iel,ncells) = tempr1(num)
                enddo
                goto 500
                !              ********
            endif
            !
            !           * which two temperature nodes is the cell between ?
            do nnum = nserch,num
                if (celldp<=depth(nnum)) goto 800
                !                                         ********
            enddo
            !
            800       nserch = nnum
            !
            !           * linear interpolation between the temperature nodes
            cellfc = (celldp-depth(nnum-1))/(depth(nnum)-depth(nnum-1))
            temp(iel,nce) = (1-cellfc) * tempr1(nnum-1)+  cellfc * tempr1(nnum)
        enddo
500 continue
    !
    do nce = 1,num
        tempr(nce) = tempr1(nce)
    enddo
    !
    !
end subroutine mntemp

END MODULE MNmod
