module MNmod

    
!-------------------------- Start of MNmod --------------------------*
!----------------------------------------------------------------------*
! Version:  SHETRAN/4.6
! Modifications:
!   SB        Mar 26    4.6   Capitalize MNCONT so it works in Linux
!                              change the following as now allocatable 
!                              vstheo, nlyrbt, ntsoil, deltaz, rdf, zvsnod, cccc, ssss, sss1, sss2
!----------------------------------------------------------------------*
    
    
    use sglobal, only : llee, nconee, nelee, nlfee, nlyree, npelee, npltee, nsee, nvee, nxee, nyee, error
    use mod_load_filedata,    only : alallf, alalli, alchk, alchki, alintp, alred2, alredc, alredf, alredi, alredl
    use utilsmod, only: hour_from_date, tridag
    	

    PUBLIC    :: mnamm, mnco2, MNCONT, mnedth, mnemph, mnemt, mnenph, mnent   ! subroutine names
    PUBLIC    :: mnerr0, mnerr1, mnerr2, mnerr3, mnerr4, mngam, mninit, mnint2
	PUBLIC    :: mnlthm, mnltn, mnmain, mnman, mnnit, mnout, mnplant, mnred1, mnred2, mntemp
    
    
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     cahum,calit,caman,cdort,chum,chum1,clit,clit1,cman,cman1
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     denit,dummy4,dummy6
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     edeth,emph,emt,enph,ent
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     gam,gamtmp,imamm,imdiff,imnit
    LOGICAL, DIMENSION(:,:), ALLOCATABLE ::     isimtf
	DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     kd1,kd2,khum,klit,kman,knit,kvol
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     miner
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     naamm,namm,namm1,nanit,ndnit,ndsnt,nlit,nlit1,nman,nman1,ntrf
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     plamm,plnit,plup,pphi
    DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE ::     snit,temp,vol
    
    CONTAINS


subroutine mnamm (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,nlyree,ns,ncolmb,nlyr,nlyrbt,ntsoil,gnn,kplamm,kuamm, &
    mncref,kddsol,dtuz,vsthe,vstheo,isbotc)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the concentration of ammonium nitrogen per unit volume of, 
    ! solution at timestep n+1
    ! failure of iteration loop to converge produces error no 3018
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    ! externals
    !use sglobal, only : error
    !       external      error
    ! input arguments
    integer llee,mnpr,nbotce,ncetop,nel,nelee,nlf,nlyree,ns
    integer ncolmb(nelee),nlyr(nelee)
    integer nlyrbt(nel,nlyree),ntsoil(nel,nlyree)
    double precision gnn,kplamm,kuamm,mncref
    double precision kddsol(ns)
    double precision dtuz
    !double precision emt(nelee,llee),enph(nelee,llee)
    !double precision ent(nelee,llee)
    !double precision gam(nelee,llee)
    !double precision knit(nelee,llee),kvol(nelee,llee)
    !double precision naamm(nelee,llee)
    !double precision namm(nelee,llee)
    !double precision ndnit(nelee,llee),ndsnt(nelee,llee)
    !double precision plup(nelee,llee)
    !double precision pphi(nelee,llee)
    double precision vsthe(ncetop,nel),vstheo(nel,ncetop+1)
    logical isbotc
    !
    ! output arguments
    !double precision imamm(nelee,llee),miner(nelee,llee)
    !double precision namm1(nelee,llee)
    !double precision ntrf(nelee,llee),plamm(nelee,llee)
    !double precision vol(nelee,llee)
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


subroutine mnco2 (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,fe,fh,isbotc)
    !
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the concentration of carbon dioxide produced
    ! upto timestep n+1
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision fe,fh
    !double precision chum(nelee,llee)
    !double precision chum1(nelee,llee),clit(nelee,llee)
    !double precision clit1(nelee,llee),cman(nelee,llee)
    !double precision cman1(nelee,llee)
    !double precision emph(nelee,llee),emt(nelee,llee)
    !double precision khum(nelee,llee),klit(nelee,llee)
    !double precision kman(nelee,llee)
    logical isbotc
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



subroutine MNCONT(mnd,mnfc,mnfn,mnpl,mnpr,mnout1,mnout2,mnoutpl,ncetop,ncon,nel,nlf,ns,nv,nx,ny,icmbk,icmref, &
    icmxy,ncolmb,nlyr,nrd,nvc,nlyrbt,ntsoil,d0,tih,rhopl,z2,delone,dxqq,dyqq,vspor,deltaz,plai,rdf,zvsnod,bexbk, &
    linkns,dtuz,uznow,clai,cccc,pnetto,ssss,ta,vspsi,vsthe,vstheo,sss1,sss2 )
    !--------------------------------------------------------------------*
    !
    ! controlling mn subroutine from the other main ones are called
    ! mnplant is very poorly written and there is no checking of data, it
    ! is based on mpl component of shetran with only the relevant lines
    ! included
    !
    ! mnmain everything is checked
    !
    !--------------------------------------------------------------------*
    ! version: 4.2               notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    !
    ! commons and distributed constants
    !      include  'al.p.mn'
    !     al.p: llee nelee nlfee nlyree nxee
    !
    !     n.b. dont dimension with nlf it may be zero
    ! externals
    !external mnmain,mnplant
    !
    !
    ! input arguments
    !      * static
    integer mnd,mnfc,mnfn,mnpl,mnpr,mnout1,mnout2,mnoutpl
    integer ncetop,ncon,nel,nlf,ns,nv,nx,ny
    integer icmbk(nlfee,2),icmref(nelee,4,2:2),icmxy(nxee,ny)
    integer ncolmb(nelee),nlyr(nelee)
    integer nrd(nv),nvc(nelee)
    integer nlyrbt(nel,nlyree),ntsoil(nel,nlyree)
    double precision d0,tih,rhopl,z2
    double precision delone(npltee)
    double precision dxqq(nelee),dyqq(nelee)
    double precision vspor(ns)
    double precision deltaz(llee,nel),plai(nv)
    double precision rdf(nv,llee),zvsnod(llee,nel)
    logical bexbk,linkns(nlfee)
    !      * varying
    double precision dtuz,uznow
    double precision clai(nv)
    double precision cccc(nel,ncetop+1)
    double precision pnetto(nelee)
    double precision ssss(nel,ncetop+1)
    double precision ta(nv),vspsi(ncetop,nel)
    double precision vsthe(ncetop,nel),vstheo(nel,ncetop+1)
    !
    !
    ! ouput arguments
    double precision sss1(nel,ncetop+1),sss2(nel,ncetop+1)
    !
    ! local arguments
    !double precision plup(nelee,llee)
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

	
subroutine mnedth (llee,nbotce,ncetop,nel,nelee,nlf,nlyree,ns,ncolmb,nlyr,nlyrbt,ntsoil,vsthe,vspor,isbotc )
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the moisture environmental reduction factor
    ! for denitrification
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf,nlyree,ns
    integer ncolmb(nelee),nlyr(nelee)
    integer nlyrbt(nel,nlyree),ntsoil(nel,nlyree)
    double precision vsthe(ncetop,nel),vspor(ns)
    logical isbotc
    !
    !
    ! output arguments
    !double precision edeth(nelee,llee)
    !
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
	
	
	
subroutine mnemph (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,vspsi,isbotc)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the matric potential environmental reduction factor
    ! for mineralization
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision vspsi(ncetop,nel)
    logical isbotc
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
end	subroutine mnemph
	
	
	
subroutine mnemt (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,q10m,isbotc,isq10)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the temperature environmental reduction factor
    ! for mineralization
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision q10m
    !temp(nelee,llee)
    logical isbotc,isq10
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
	
	
subroutine mnenph (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,vspsi,isbotc)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the matric potential environmental reduction factor
    ! for mineralization
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision vspsi(ncetop,nel)
    logical isbotc
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
	
	
subroutine mnent (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,q10n,isbotc,isq10)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the temperature environmental reduction factor
    ! for nitrification
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision q10n
    !temp(nelee,llee)
    logical isbotc,isq10
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
	
subroutine mnerr0(llee,mnd,mnfc,mnfn,mnpr,ncetop,ncon,nconee,nel,nelee,nlf,nlfee,nlyree,nmneee,nmntee,ns,nsee,nv, &
    nvee,nx,nxee,ny )
    !
    !--------------------------------------------------------------------*
    !
    ! checks array dimensions
    ! error numbers 3010,3020-3034
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    !use sglobal, only : error
    !use mod_load_filedata ,    only : alchki
    !       external      alchki,error
    !
    ! input arguments
    integer       llee,mnd,mnfc,mnfn,mnpr,ncetop,ncon,nconee,nel
    integer       nelee,nlf,nlfee,nlyree,nmneee,nmntee,ns,nsee
    integer       nx,nxee,nv,nvee,ny
    !
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


subroutine mnerr1(llee,mnpr,ncetop,nel,nelee,nlf,nlfee,nlyree,ns,nx,nxee,ny,icmbk,icmref,icmxy,ncolmb,nlyr,nlyrbt &
    ,ntsoil,d0,tih,z2,dxqq,dyqq,vspor,deltaz,zvsnod,bexbk,linkns,dummy2,dummy3,idum,idum1x,ldum,ldum2)
    !
    !--------------------------------------------------------------------*
    !
    ! checks static input variables from cm -mn interface
    ! error numbers 3011,3035-3047 and 2075-2079 for index arrays
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    !use sglobal, only : error
    !       use sglobal
    !use mod_load_filedata ,    only : alchk,alchki
    !       external      alchk,alchki,error
    !
    ! input arguments
    integer llee,mnpr,ncetop,nel,nelee,nlf,nlfee,nlyree,ns
    integer nx,nxee,ny
    integer icmbk(nlfee,2),icmref(nelee,4,2:2),icmxy(nxee,ny)
    integer ncolmb(nelee),nlyr(nelee)
    integer nlyrbt(nel,nlyree),ntsoil(nel,nlyree)
    double precision d0,tih,z2
    double precision dxqq(nelee),dyqq(nelee)
    double precision vspor(ns)
    double precision deltaz(llee,nel),zvsnod(llee,nel)
    logical bexbk,linkns(nlfee)
    !
    ! workspace arguments
    integer           dummy2(nlyree,nelee),dummy3(nlyree)
    integer           idum(nelee),idum1x(-1:nel+1)
    !double precision  dummy4(llee,nelee)
    logical           ldum(nelee),ldum2(llee)
    !
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


subroutine mnerr2(mnpr,nbotce,ncetop,nel,nelee,nlf,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e &
    ,nmneee,nmntee,ns,celem,kd1elm,kd2elm,khelem,klelem,kmelem,knelem,kvelem,naelem,nmn15t,nmn17t,nmn19t,nmn21t, &
    nmn23t,nmn25t,nmn27t,nmn43t,nmn53t,ammddr,ammwdr,clitfr,cnrbio,cnrhum,cnrlit,fe,fh,gnn,kplamm,kplnit,kuamm,kunit, &
    mncref,nitddr,nitwdr,q10m,q10n,cconc,cdpth,ctottp,damhlf,dchlf,kd1cnc,kd1dth,kd2cnc,kd2dth,kddsol,khconc,khdpth, &
    klconc,kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth,namtop,isiccd,isiamd,ldum)
    !--------------------------------------------------------------------*
    !
    ! checks static input data read in from mnred1 subroutine
    ! error numbers 3012,3048-3064
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    !use sglobal, only : error
    !use mod_load_filedata ,    only : alchk,alchki
    !       external      alchk,error
    !
    ! input arguments
    integer mnpr,nbotce,ncetop,nel,nelee,nlf
    integer nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e
    integer nmn27e,nmn43e,nmn53e
    integer nmneee,nmntee,ns
    integer celem(nlf+1:nel),kd1elm(nlf+1:nel),kd2elm(nlf+1:nel)
    integer khelem(nlf+1:nel),klelem(nlf+1:nel),kmelem(nlf+1:nel)
    integer knelem(nlf+1:nel),kvelem(nlf+1:nel)
    integer naelem(nlf+1:nel)
    integer nmn15t(nmneee),nmn17t(nmneee),nmn19t(nmneee)
    integer nmn21t(nmneee),nmn23t(nmneee),nmn25t(nmneee)
    integer nmn27t(nmneee)
    integer nmn43t(nmneee),nmn53t(nmneee)
    double precision ammddr,ammwdr,clitfr,cnrbio,cnrhum,cnrlit
    double precision fe,fh,gnn,kplamm,kplnit,kuamm,kunit
    double precision mncref,nitddr,nitwdr,q10m,q10n
    double precision cconc(nmneee,nmntee),cdpth(nmneee,nmntee)
    double precision ctottp(nlf+1:nel),damhlf(nlf+1:nel)
    double precision dchlf(nlf+1:nel)
    double precision kd1cnc(nmneee,nmntee),kd1dth(nmneee,nmntee)
    double precision kd2cnc(nmneee,nmntee),kd2dth(nmneee,nmntee)
    double precision kddsol(ns)
    double precision khconc(nmneee,nmntee),khdpth(nmneee,nmntee)
    double precision klconc(nmneee,nmntee),kldpth(nmneee,nmntee)
    double precision kmconc(nmneee,nmntee),kmdpth(nmneee,nmntee)
    double precision knconc(nmneee,nmntee),kndpth(nmneee,nmntee)
    double precision kvconc(nmneee,nmntee),kvdpth(nmneee,nmntee)
    double precision naconc(nmneee,nmntee),nadpth(nmneee,nmntee)
    double precision namtop(nlf+1:nel)
    logical isiccd,isiamd
    !
    ! workspace arguments
    logical           ldum(nelee)
    !
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


subroutine mnerr3(llee,mnpr,ncetop,nel,nelee,nlf,ncolmb,dtuz,uznow,cccc, &
    pnetto,ssss,vsthe,vstheo,ldum,ldum2 )
    !
    !--------------------------------------------------------------------*
    !
    ! checks time dependent input variables from cm -mn interface and the
    ! concentrations calculated in this component are positive
    ! error numbers 3013 and 3065-3079
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    !use sglobal, only : error
    !use mod_load_filedata ,    only : alchk
    !       external      alchk,error
    !
    ! input arguments
    integer llee,mnpr,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision dtuz,uznow
    double precision cccc(nel,ncetop+1)
    !double precision chum1(nelee,llee),clit1(nelee,llee)
    !double precision cman1(nelee,llee)
    !double precision namm1(nelee,llee),nlit1(nelee,llee)
    !double precision nman1(nelee,llee)
    !double precision plup(nelee,llee)
    double precision pnetto(nelee)
    double precision ssss(nel,ncetop+1)
    double precision vsthe(ncetop,nel),vstheo(nel,ncetop+1)
    !
    ! workspace arguments
    !double precision  dummy4(llee,nelee)
    logical           ldum(nelee),ldum2(llee)
    !
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

subroutine mnerr4 ( mnpr,nel,nelee,nlf,cdpthb,cltfct,cmnfct,cnral,cnram,ctot,namfct,ndpthb,ntot,isaddc,isaddn, &
    dummy,ldum )
    !
    !--------------------------------------------------------------------*
    !
    !  checks time varying dependent data read in mnred2
    !  error numbers 3014 and 3080-3089
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    !use sglobal, only : error
    !use mod_load_filedata ,    only : alchk
    !       external      alchk,error
    !
    !
    ! input arguments
    integer mnpr,nel,nelee,nlf
    double precision cdpthb(nlf+1:nel),cltfct(nlf+1:nel)
    double precision cmnfct(nlf+1:nel),cnral(nlf+1:nel)
    double precision cnram(nlf+1:nel),ctot(nlf+1:nel)
    double precision namfct(nlf+1:nel),ndpthb(nlf+1:nel)
    double precision ntot(nlf+1:nel)
    logical isaddc,isaddn
    !
    ! workspace arguments
    double precision dummy(nelee)
    logical          ldum(nelee)
    !
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


subroutine mngam (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,cnrhum,cnrbio,fe,fh,dtuz, &
    isbotc )
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the mineralisation/immobilisation rate. if mngam is
    ! positive then this is the mineralisation rate. if mngam is negative
    ! then this is the immobilisation rate
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision cnrbio,cnrhum
    double precision fe,fh
    double precision dtuz
    !double precision chum(nelee,llee),chum1(nelee,llee)
    !double precision clit(nelee,llee),clit1(nelee,llee)
    !double precision cman(nelee,llee),cman1(nelee,llee)
    !double precision emph(nelee,llee),emt(nelee,llee)
    !double precision klit(nelee,llee),khum(nelee,llee)
    !double precision kman(nelee,llee)
    !double precision nlit(nelee,llee),nlit1(nelee,llee)
    !double precision nman(nelee,llee),nman1(nelee,llee)
    logical isbotc
    !
    ! input/output arguments
    !double precision imdiff(nelee,llee)
    !logical isimtf(nelee,llee)
    !
    ! output arguments
    !double precision gam(nelee,llee),gamtmp(nelee,llee)
    !
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

subroutine mninit(llee,nbotce,ncetop,nel,nelee,nlf,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e &
    ,nmneee,nmntee,celem,kd1elm,kd2elm,khelem,klelem,kmelem,knelem,kvelem,naelem,ncolmb,nmn15t,nmn17t,nmn19t,nmn21t, &
    nmn23t,nmn25t,nmn27t,nmn43t,nmn53t,clitfr,cnrlit,cconc,cdpth,ctottp,damhlf,dchlf,deltaz,kd1cnc,kd1dth,kd2cnc, &
    kd2dth,khconc,khdpth,klconc,kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth,namtop,zvsnod,isiccd, &
    isiamd,sss1,sss2,isbotc)
    !
    !--------------------------------------------------------------------*
    !
    ! initialises the global variables
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    !use mod_load_filedata ,    only : alintp
    !       external alintp
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf
    integer nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e
    integer nmn27e,nmn43e,nmn53e
    integer nmneee,nmntee
    integer celem(nlf+1:nel),kd1elm(nlf+1:nel),kd2elm(nlf+1:nel)
    integer khelem(nlf+1:nel),klelem(nlf+1:nel),kmelem(nlf+1:nel)
    integer knelem(nlf+1:nel),kvelem(nlf+1:nel)
    integer naelem(nlf+1:nel),ncolmb(nelee)
    integer nmn15t(nmneee),nmn17t(nmneee),nmn19t(nmneee)
    integer nmn21t(nmneee),nmn23t(nmneee),nmn25t(nmneee)
    integer nmn27t(nmneee)
    integer nmn43t(nmneee),nmn53t(nmneee)
    double precision clitfr,cnrlit
    double precision cconc(nmneee,nmntee),cdpth(nmneee,nmntee)
    double precision ctottp(nlf+1:nel),damhlf(nlf+1:nel)
    double precision dchlf(nlf+1:nel)
    double precision deltaz(llee,nel)
    double precision kd1cnc(nmneee,nmntee),kd1dth(nmneee,nmntee)
    double precision kd2cnc(nmneee,nmntee),kd2dth(nmneee,nmntee)
    double precision khconc(nmneee,nmntee),khdpth(nmneee,nmntee)
    double precision klconc(nmneee,nmntee),kldpth(nmneee,nmntee)
    double precision kmconc(nmneee,nmntee),kmdpth(nmneee,nmntee)
    double precision knconc(nmneee,nmntee),kndpth(nmneee,nmntee)
    double precision kvconc(nmneee,nmntee),kvdpth(nmneee,nmntee)
    double precision naconc(nmneee,nmntee),nadpth(nmneee,nmntee)
    double precision namtop(nlf+1:nel)
    double precision zvsnod(llee,nel)
    logical isiccd,isiamd
    !
    ! output arguments
    !double precision chum1(nelee,llee),clit1(nelee,llee)
    !double precision clit1(nelee,llee)
    !double precision cman1(nelee,llee)
    !double precision imdiff(nelee,llee)
    !double precision kd1(nelee,llee),kd2(nelee,llee)
    !double precision khum(nelee,llee),klit(nelee,llee)
    !double precision kman(nelee,llee),knit(nelee,llee)
    !double precision kvol(nelee,llee)
    !double precision namm1(nelee,llee),nlit1(nelee,llee)
    !double precision nman1(nelee,llee)
    double precision sss1(nel,ncetop+1),sss2(nel,ncetop+1)
    logical isbotc
    !double precision isimtf(nelee,llee)
    !
    ! workspace
    !double precision dummy6(nelee,llee)
    !
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

subroutine mnint2 ( llee,ncetop,nel,nelee,nlf,nlyree,ncolmb,nlyr,nlyrbt,ntsoil,ammddr,ammwdr,mncref,nitddr,nitwdr &
    ,deltaz,dtuz,cccc,cdpthb,cltfct,cmnfct,cnral,cnram,ctot,namfct,ndpthb,ntot, &
    pnetto,ssss,vsthe,isaddc,isaddn,cnralt,cnramn, &
    dummy)
    !
    !--------------------------------------------------------------------*
    !
    !  modifies the time varying input variables into suitable units
    !  for the rest of the program
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    !       external         phi
    ! input arguments
    !      * stationary
    integer llee,ncetop,nel,nelee,nlf,nlyree
    integer ncolmb(nelee),nlyr(nelee)
    integer nlyrbt(nel,nlyree),ntsoil(nel,nlyree)
    double precision ammddr,ammwdr
    double precision mncref,nitddr,nitwdr
    double precision deltaz(llee,nel)
    !      * time dependent
    double precision dtuz
    double precision cccc(nel,ncetop+1)
    double precision cdpthb(nlf+1:nel)
    !double precision chum1(nelee,llee),clit1(nelee,llee)
    double precision cltfct(nlf+1:nel)
    !double precision cman1(nelee,llee)
    double precision cmnfct(nlf+1:nel)
    double precision cnral(nlf+1:nel),cnram(nlf+1:nel)
    double precision ctot(nlf+1:nel)
    double precision namfct(nlf+1:nel)
    !double precision namm1(nelee,llee)
    double precision ndpthb(nlf+1:nel)
    !double precision nlit1(nelee,llee)
    !double precision nman1(nelee,llee)
    double precision  ntot(nlf+1:nel)
    double precision pnetto(nelee)
    double precision ssss(nel,ncetop+1),vsthe(ncetop,nel)
    logical isaddc,isaddn
    !
    ! output arguments
    !double precision cahum(nelee,llee),calit(nelee,llee)
    !double precision caman(nelee,llee)
    !double precision chum(nelee,llee)
    !double precision clit(nelee,llee),cman(nelee,llee)
    double precision cnralt(nelee),cnramn(nelee)
    !double precision naamm(nelee,llee),namm(nelee,llee)
    !double precision nanit(nelee,llee)
    !double precision ndnit(nelee,llee),ndsnt(nelee,llee)
    !double precision nlit(nelee,llee),nman(nelee,llee)
    !double precision pphi(nelee,llee)
    !
    ! workspace
    double precision dummy(nelee)
    !
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

subroutine mnlthm (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,fe,fh,dtuz,isbotc)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the concentration in the carbon litter and
    !  humus pools at timestep n+1
    ! failure of iteration loop to converge produces error no 3016
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    ! externals
    !use sglobal, only : error
    !       external     error
    !
    ! input arguments
    integer llee,mnpr,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision fe,fh
    double precision dtuz
    !double precision cahum(nelee,llee),calit(nelee,llee)
    !double precision chum(nelee,llee),clit(nelee,llee)
    !double precision cman(nelee,llee),cman1(nelee,llee)
    !double precision emph(nelee,llee),emt(nelee,llee)
    !double precision khum(nelee,llee),klit(nelee,llee)
    !double precision kman(nelee,llee)
    logical isbotc
    !logical isimtf(nelee,llee)
    ! output arguments
    !double precision chum1(nelee,llee)
    !double precision clit1(nelee,llee)
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

subroutine mnltn (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,cnrbio,fe,fh,dtuz,cnralt,isbotc)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the concentration in the nitrogen litter
    ! pool at timestep n+1
    ! failure of iteration loop to converge produces error no 3017
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    ! externals
    !use sglobal, only : error
    !       external     error
    !
    ! input arguments
    integer llee,mnpr,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision cnrbio,fe,fh
    double precision dtuz
    !double precision calit(nelee,llee),chum(nelee,llee)
    !double precision chum1(nelee,llee),clit(nelee,llee)
    !double precision clit1(nelee,llee),cman(nelee,llee)
    !double precision cman1(nelee,llee)
    double precision cnralt(nelee)
    !double precision emph(nelee,llee),emt(nelee,llee)
    !double precision khum(nelee,llee),klit(nelee,llee)
    !double precision kman(nelee,llee)
    !double precision nlit(nelee,llee)
    logical isbotc
    !logical isimtf(nelee,llee)
    ! output arguments
    !double precision nlit1(nelee,llee)
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


subroutine mnmain(mnd,mnfc,mnfn,mnpr,mnout1,mnout2,ncetop,ncon,nel,nlf,ns,nv,nx,ny,icmbk,icmref,icmxy,ncolmb,nlyr &
    ,nlyrbt,ntsoil,d0,tih,z2,dxqq,dyqq,vspor,deltaz,zvsnod,bexbk,linkns,dtuz,uznow,cccc,pnetto,ssss,ta,vspsi, &
    vsthe,vstheo,sss1,sss2 )
    !--------------------------------------------------------------------*
    !
    ! main mn subroutine from which all the others are called
    !
    !--------------------------------------------------------------------*
    ! version: 4.2               notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    !
    ! commons and distributed constants
    !      include  'al.p.mn'
    !use sglobal, only : llee, nelee, nlfee, nlyree, nxee, nsee,nvee, nconee
    !
    ! constants referenced
    !     al.p: llee nconee nelee nlfee nlyree nsee nvee nxee
    !
    !     n.b. dont dimension with nlf it may be zero
    ! externals
    !external mnamm,mnco2,mnedth,mnemph
    !external mnemt,mnenph,mnent,mnerr0,mnerr1,mnerr2
    !external mnerr3,mnerr4,mngam,mninit,mnint2
    !external mnlthm,mnltn,mnman,mnnit,mnout,mnred1,mnred2
    !
    ! input arguments
    !      * static
    integer mnd,mnfc,mnfn,mnpr,mnout1,mnout2
    integer ncetop,ncon,nel,nlf,ns,nv,nx,ny
    integer icmbk(nlfee,2),icmref(nelee,4,2:2),icmxy(nxee,ny)
    integer ncolmb(nelee),nlyr(nelee)
    integer nlyrbt(nel,nlyree),ntsoil(nel,nlyree)
    double precision d0,tih,z2
    double precision dxqq(nelee),dyqq(nelee)
    double precision vspor(ns)
    double precision deltaz(llee,nel),zvsnod(llee,nel)
    logical bexbk,linkns(nlfee)
    !      * varying
    double precision dtuz,uznow
    double precision cccc(nel,ncetop+1)
    !double precision plup(nelee,llee)
    double precision pnetto(nelee)
    double precision ssss(nel,ncetop+1)
    double precision ta(nv),vspsi(ncetop,nel)
    double precision vsthe(ncetop,nel),vstheo(nel,ncetop+1)
    !
    !
    ! ouput arguments
    double precision sss1(nel,ncetop+1),sss2(nel,ncetop+1)
    !
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

subroutine mnman (llee,mnpr,nbotce,ncetop,nel,nelee,nlf,ncolmb,dtuz,cnramn,isbotc)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the concentration in the carbon and nitrogen
    !  manure pools at timestep n+1
    ! failure of iteration loop to converge produces error no 3015
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    ! externals
    !use sglobal, only : error
    !       external     error
    !
    ! input arguments
    integer llee,mnpr,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision dtuz
    !double precision caman(nelee,llee),cman(nelee,llee)
    double precision cnramn(nelee)
    !double precision emph(nelee,llee)
    !double precision emt(nelee,llee),kman(nelee,llee)
    !double precision nman(nelee,llee)
    logical isbotc
    !double precision isimtf(nelee,llee)
    !
    ! output arguments
    !double precision cman1(nelee,llee)
    !double precision nman1(nelee,llee)
    !
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


subroutine mnnit (llee,nbotce,ncetop,nel,nelee,nlf,ncolmb,d0,kplnit,kunit,mncref,z2,dtuz,vsthe,vstheo,isbotc,sss1,sss2)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the concentration of dynamic nitrate concentration per
    ! unit volume of solution at timestep n+1
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! input arguments
    integer llee,nbotce,ncetop,nel,nelee,nlf
    integer ncolmb(nelee)
    double precision d0,kplnit,kunit,mncref,z2
    double precision dtuz
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
    double precision vsthe(ncetop,nel),vstheo(nel,ncetop+1)
    logical isbotc
    !
    ! input/output arguments
    !double precision imdiff(nelee,llee)
    !logical isimtf(nelee,llee)
    !
    ! output arguments
    !double precision denit(nelee,llee)
    !double precision imnit(nelee,llee)
    !double precision plnit(nelee,llee),snit(nelee,llee)
    double precision sss1(nel,ncetop+1),sss2(nel,ncetop+1)
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

subroutine mnout (mnout1,mnout2,nbotce,ncetop,nel,nlf,ns,ncolmb,nlyr,nlyrbt,ntsoil,cnrhum,gnn,mncref,deltaz, &
    kddsol,pphi,dtuz,uznow,dxqq,dyqq,cnralt,cnramn,vsthe,vstheo,isbotc)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates total inputs and outputs and writes output to files
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! commons and distributed constants
    !      include  'al.p'
    !use sglobal, only: nelee, nlyree, llee
    !
    ! input arguments
    integer mnout1,mnout2,nbotce,ncetop,nel,nlf,ns
    integer ncolmb(nelee),nlyr(nelee)
    integer nlyrbt(nel,nlyree),ntsoil(nel,nlyree)
    double precision cnrhum,gnn,mncref
    double precision deltaz(llee,nel),kddsol(ns)
    double precision pphi(nelee,llee)
    double precision dtuz,uznow
    double precision dxqq(nelee),dyqq(nelee)
    !double precision cahum(nelee,llee),calit(nelee,llee)
    !double precision caman(nelee,llee),cdort(nelee,llee)
    !double precision chum(nelee,llee),chum1(nelee,llee)
    !double precision chum(nelee,llee)
    !double precision clit(nelee,llee),clit1(nelee,llee)
    !double precision cman(nelee,llee),cman1(nelee,llee)
    double precision cnralt(nelee),cnramn(nelee)
    !double precision denit(nelee,llee),gamtmp(nelee,llee)
    !double precision imamm(nelee,llee),imnit(nelee,llee)
    !double precision miner(nelee,llee),namm(nelee,llee)
    !double precision namm1(nelee,llee)
    !double precision naamm(nelee,llee),nanit(nelee,llee)
    !double precision nlit(nelee,llee),nlit1(nelee,llee)
    !double precision nman(nelee,llee)
    !double precision nman1(nelee,llee)
    !double precision ntrf(nelee,llee)
    !double precision plamm(nelee,llee)
    !double precision plnit(nelee,llee)
    !double precision snit(nelee,llee)
    double precision vsthe(ncetop,nel),vstheo(nel,ncetop+1)
    !double precision vol(nelee,llee)
    logical isbotc
    !
    !
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
    !!
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
        !!        output for specific cells
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


subroutine mnplant(mnpl,mnoutpl,ncetop,nel,nlf,nv,ncolmb,nrd,nvc,rhopl,delone,dxqq,dyqq,deltaz,plai,rdf,dtuz, &
    uznow,clai)
    !--------------------------------------------------------------------*
    !
    ! subroutine used to calculate the potential nitrogen uptake by plants
    ! this is calculted from the canopy leaf area index of the plants
    ! this is generally reasonable for decidous trees and arable crops
    ! but not so for permanent grassland where the clai is often considered
    ! to be constant. (see etd file)
    !
    ! the basis of the programming is from the mpl component (with only
    ! the relevant bits included ). thus the code is of poor quality.
    !
    ! important varaibles:
    ! npltee is the total number of plant types and it is generally
    !    set to the same value as nvee
    ! npelee is the number of plants in any element and it is
    !    generally set to 2.
    ! nplant is the number of the plant type on the element
    ! jplty is an actual plant type on the element
    !
    ! additionally there is an extra data file used here which gives data
    ! on the canopy density. this modifies the calculation of the mass of
    ! the plants (the canopy leaf area index is not sufficient)
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! commons and distributed constants
    !      include  'al.p.mn'
    !use sglobal, only : llee, nelee, npelee, npltee, nvee
    !use mod_load_filedata , only : alredf,alredi,alred2,alalli,alredl,alredc
    ! externals
    !       external alredc,alredf,alredl,alred2,error,alalli
    !
    !
    ! constants referenced
    !     al.p:  llee nelee npelee npltee nvee
    !
    ! input arguments
    !     * static
    integer mnpl,mnoutpl,ncetop,nel,nlf,nv
    integer ncolmb(nelee)
    integer nrd(nv),nvc(nelee)
    double precision rhopl
    double precision delone(npltee),dxqq(nelee),dyqq(nelee)
    double precision deltaz(llee,nel),plai(nv)
    double precision rdf(nv,llee)
    !     * time dependent
    double precision dtuz,uznow
    double precision clai(nv)
    !
    ! output arguments
    !double precision plup(nelee,llee)
    !
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

subroutine mnred1(mnd,mnpr,nel,nelee,nlf,nlfee,nmneee,nmntee,ns,nx,nxee,ny,icmbk,icmref,icmxy,bexbk,linkns,nbotce &
    ,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e,nmn27e,nmn43e,nmn53e,celem,kd1elm,kd2elm,khelem,klelem,kmelem,knelem, &
    kvelem,naelem,nmn15t,nmn17t,nmn19t,nmn21t,nmn23t,nmn25t,nmn27t,nmn43t,nmn53t,ammddr,ammwdr,clitfr,cnrbio,cnrhum, &
    cnrlit,fe,fh,gnn,kplamm,kplnit,kuamm,kunit,mncref,nitddr,nitwdr,q10m,q10n,cconc,cdpth,ctottp,damhlf,dchlf,kd1cnc, &
    kd1dth,kd2cnc,kd2dth,kddsol,khconc,khdpth,klconc,kldpth,kmconc,kmdpth,knconc,kndpth,kvconc,kvdpth,naconc,nadpth, &
    namtop,isiccd,isiamd,isq10,idum,dummy )
    !--------------------------------------------------------------------*
    !
    ! reads input from files
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    ! nyee needed as alallf changed to require it
    !use sglobal, only : nyee, error
    !use mod_load_filedata , only : alalli, alredc,alredl,alredf,alallf,alredi,alred2
    !       external alallf,alredc,alredf,alredl,alred2,error,alalli
    ! input arguments
    integer mnd,mnpr,nel,nelee,nlf,nlfee,nmneee,nmntee,ns,nx,nxee,ny
    integer icmbk(nlfee,2),icmref(nelee,4,2:2),icmxy(nxee,ny)
    logical bexbk,linkns(nlfee)
    !
    ! ouput arguments
    integer nbotce,nmn15e,nmn17e,nmn19e,nmn21e,nmn23e,nmn25e
    integer nmn27e,nmn43e,nmn53e
    integer celem(nlf+1:nel),kd1elm(nlf+1:nel),kd2elm(nlf+1:nel)
    integer khelem(nlf+1:nel),klelem(nlf+1:nel),kmelem(nlf+1:nel)
    integer knelem(nlf+1:nel),kvelem(nlf+1:nel)
    integer naelem(nlf+1:nel)
    integer nmn15t(nmneee),nmn17t(nmneee),nmn19t(nmneee)
    integer nmn21t(nmneee),nmn23t(nmneee),nmn25t(nmneee)
    integer nmn27t(nmneee)
    integer nmn43t(nmneee),nmn53t(nmneee)
    double precision ammddr,ammwdr,clitfr,cnrbio,cnrhum,cnrlit
    double precision fe,fh,gnn,kplamm,kplnit
    double precision kuamm,kunit,mncref,nitddr,nitwdr
    double precision q10m,q10n
    double precision cconc(nmneee,nmntee),cdpth(nmneee,nmntee)
    double precision ctottp(nlf+1:nel),damhlf(nlf+1:nel)
    double precision dchlf(nlf+1:nel)
    double precision kd1cnc(nmneee,nmntee),kd1dth(nmneee,nmntee)
    double precision kd2cnc(nmneee,nmntee),kd2dth(nmneee,nmntee)
    double precision kddsol(ns)
    double precision khconc(nmneee,nmntee),khdpth(nmneee,nmntee)
    double precision klconc(nmneee,nmntee),kldpth(nmneee,nmntee)
    double precision kmconc(nmneee,nmntee),kmdpth(nmneee,nmntee)
    double precision knconc(nmneee,nmntee),kndpth(nmneee,nmntee)
    double precision kvconc(nmneee,nmntee),kvdpth(nmneee,nmntee)
    double precision naconc(nmneee,nmntee),nadpth(nmneee,nmntee)
    double precision namtop(nlf+1:nel)
    logical isiccd,isiamd,isq10
    !
    ! workspace arguments
    integer          idum (nelee)
    double precision dummy(nelee)
    !
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

subroutine mnred2 ( mnfc,mnfn,mnpr,nel,nelee,nlf,nlfee,nx,nxee,ny,icmbk,icmref,icmxy,dtuz,tih,uznow,bexbk,linkns, &
    cdpthb,cltfct,cmnfct,cnral,cnram,ctot,namfct,ndpthb,ntot,isaddc,isaddn,idum,dummy)
    !--------------------------------------------------------------------*
    !
    ! reads input from time dependent files
    ! it is assumed for simplicity that there is never more than one
    ! fertilizer addition in a timestep (this should be a valid assumption
    ! because farmers do not add fertilizer more than once a day and there
    ! is a maximum timestep of two hours). if there is more than one
    ! fertilizer addition the remainder are read in following timesteps.
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    ! externals
    !       double precision                     hour
    !use utilsmod, only: hour_from_date
    !use sglobal, only : nyee
    !use mod_load_filedata , only : alred2,alredi,alallf
    !       external         alallf,alredi,alred2,hour
    ! input arguments
    integer mnfc,mnfn,mnpr,nel,nelee,nlf,nlfee,nx,nxee,ny
    integer icmbk(nlfee,2),icmref(nelee,4,2:2),icmxy(nxee,ny)
    double precision dtuz,tih,uznow
    logical bexbk,linkns(nlfee)
    !
    ! ouput arguments
    double precision cdpthb(nlf+1:nel),cltfct(nlf+1:nel)
    double precision cmnfct(nlf+1:nel),cnral(nlf+1:nel)
    double precision cnram(nlf+1:nel),ctot(nlf+1:nel)
    double precision namfct(nlf+1:nel),ndpthb(nlf+1:nel)
    double precision ntot(nlf+1:nel)
    logical isaddc,isaddn
    !
    ! workspace arguments
    integer          idum (nelee)
    double precision dummy(nelee)
    !
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


subroutine mntemp (llee,ncetop,nel,nelee,nlf,nv,ncolmb,z2,deltaz,zvsnod,dtuz,ta)
    !
    !--------------------------------------------------------------------*
    !
    ! calculates the temperature in every cell
    !
    !--------------------------------------------------------------------*
    ! version:                   notes:
    ! module: mn                 program: shetran
    ! modifications
    !--------------------------------------------------------------------*
    !
    use utilsmod, only: tridag
    !      * input arguments
    !     * static
    integer llee,ncetop,nel,nelee,nlf,nv
    integer ncolmb(nelee)
    double precision z2
    double precision deltaz(llee,nel),zvsnod(llee,nel)
    !
    !     * varying
    double precision dtuz,ta(nv)
    !
    ! output arguments
    !double precision temp(nelee,llee)
    !
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
	
	