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


   IMPLICIT NONE

   PRIVATE
   PUBLIC    :: mnamm, mnco2, mncont, mnedth, mnemph, mnemt, mnenph, mnent   ! subroutine names
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
   SUBROUTINE mnamm (llee, mnpr, nbotce, ncetop, nel, nelee, nlf, nlyree, ns, ncolmb, nlyr, nlyrbt, ntsoil, gnn, kplamm, kuamm, &
                     mncref, kddsol, dtuz, vsthe, vstheo, isbotc)

      ! Assumed external module dependencies providing global variables:
      ! gam, miner, imamm, namm, namm1, knit, ent, enph, ntrf, kvol, emt,
      ! vol, plup, pphi, ndnit, ndsnt, naamm, plamm, ERROR

      IMPLICIT NONE

      ! input arguments
      INTEGER, INTENT(IN) :: llee, mnpr, nbotce, ncetop, nel, nelee, nlf, nlyree, ns
      INTEGER, INTENT(IN) :: ncolmb(nelee), nlyr(nelee)
      INTEGER, INTENT(IN) :: nlyrbt(nel, nlyree), ntsoil(nel, nlyree)
      DOUBLE PRECISION, INTENT(IN) :: gnn, kplamm, kuamm, mncref
      DOUBLE PRECISION, INTENT(IN) :: kddsol(ns)
      DOUBLE PRECISION, INTENT(IN) :: dtuz
      DOUBLE PRECISION, INTENT(IN) :: vsthe(ncetop, nel), vstheo(nel, ncetop + 1)
      LOGICAL, INTENT(IN) :: isbotc

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
                  ! PERF FIX: Restored external format label
                  WRITE (msg, 9000) wer1sq
                  CALL ERROR(warn, 3018, mnpr, 0, 0, msg)
               END IF

            END DO layer_loop
         END DO
      END DO

9000  FORMAT('iteration loop in mnamm failed to converge with error = ', g15.7)

   END SUBROUTINE mnamm



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



   !SSSSSS SUBROUTINE MNCONT
   SUBROUTINE MNCONT(MND, MNFC, MNFN, MNPL, MNPR, MNOUT1, MNOUT2, MNOUTPL, NCETOP, NCON, NEL, NLF, NS, NV, NX, NY, &
                     ICMBK, ICMREF, ICMXY, NCOLMB, NLYR, NRD, NVC, NLYRBT, NTSOIL, &
                     D0, TIH, RHOPL, Z2, DELONE, DXQQ, DYQQ, VSPOR, DELTAZ, PLAI, RDF, ZVSNOD, BEXBK, &
                     LINKNS, DTUZ, UZNOW, CLAI, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, SSS1, SSS2)
   !--------------------------------------------------------------------*
   ! controlling mn subroutine from the other main ones are called
   ! mnplant is very poorly written and there is no checking of data, it
   ! is based on mpl component of shetran with only the relevant lines
   ! included
   !
   ! mnmain everything is checked
   !--------------------------------------------------------------------*
   ! version: 4.2             notes:
   ! module: mn               program: shetran
   !--------------------------------------------------------------------*

      ! Assumed module dependencies providing global allocatable arrays:
      ! USE MN_MODULE, ONLY : cahum, calit, caman, cdort, chum, chum1, clit, clit1, &
      !                       cman, cman1, denit, dummy4, dummy6, edeth, emph, emt, &
      !                       enph, ent, gam, gamtmp, imamm, imdiff, imnit, isimtf, &
      !                       kd1, kd2, khum, klit, kman, knit, kvol, miner, naamm, &
      !                       namm, namm1, nanit, ndnit, ndsnt, nlit, nlit1, nman, &
      !                       nman1, ntrf, plamm, plnit, plup, pphi, snit, temp, vol

      IMPLICIT NONE

      ! --- Input arguments ---
      ! Static
      INTEGER, INTENT(IN) :: MND, MNFC, MNFN, MNPL, MNPR, MNOUT1, MNOUT2, MNOUTPL
      INTEGER, INTENT(IN) :: NCETOP, NCON, NEL, NLF, NS, NV, NX, NY
      INTEGER, INTENT(IN) :: ICMBK(NLF, 2), ICMREF(NEL, 4, 2:2), ICMXY(NX, NY)
      INTEGER, INTENT(IN) :: NLYRBT(NEL, *), NTSOIL(NEL, *)

      DOUBLE PRECISION, INTENT(IN) :: D0, TIH, RHOPL, Z2
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS(NLF)

      ! Varying
      DOUBLE PRECISION, INTENT(IN)    :: DTUZ, UZNOW
      DOUBLE PRECISION, INTENT(IN)    :: CCCC(NEL, NCETOP + 1)
      DOUBLE PRECISION, INTENT(IN)    :: SSSS(NEL, NCETOP + 1)
      DOUBLE PRECISION, INTENT(IN)    :: VSPSI(NCETOP, NEL)
      DOUBLE PRECISION, INTENT(IN)    :: VSTHE(NCETOP, NEL), VSTHEO(NEL, NCETOP + 1)

      ! --- In/Out arguments (Propagated up from MNMAIN / MNPLANT strict architectures) ---
      INTEGER, INTENT(INOUT) :: NCOLMB(NEL), NLYR(NEL)
      INTEGER, INTENT(INOUT) :: NRD(NV), NVC(NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: DELONE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NEL), DYQQ(NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(*), PLAI(NV)
      DOUBLE PRECISION, INTENT(INOUT) :: RDF(NV, *), ZVSNOD(*)
      DOUBLE PRECISION, INTENT(INOUT) :: CLAI(NV)
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: TA(NV) ! Modified by temporary code

      ! --- Output arguments ---
      DOUBLE PRECISION, INTENT(OUT)   :: SSS1(NEL, NCETOP + 1), SSS2(NEL, NCETOP + 1)

      ! --- Local variables ---
      INTEGER :: I

   !----------------------------------------------------------------------*

      ! Modernization Fix: Replaced legacy PASS counter with robust ALLOCATED check
      IF (.NOT. ALLOCATED(cahum)) THEN
         ALLOCATE(cahum(nel,ncetop), calit(nel,ncetop), caman(nel,ncetop), cdort(nel,ncetop), &
                  chum(nel,ncetop), chum1(nel,ncetop), clit(nel,ncetop), clit1(nel,ncetop), &
                  cman(nel,ncetop), cman1(nel,ncetop))

         ALLOCATE(denit(nel,ncetop), dummy4(ncetop,nel), dummy6(nel,ncetop))

         ALLOCATE(edeth(nel,ncetop), emph(nel,ncetop), emt(nel,ncetop), enph(nel,ncetop), ent(nel,ncetop))

         ALLOCATE(gam(nel,ncetop), gamtmp(nel,ncetop), imamm(nel,ncetop), imdiff(nel,ncetop), &
                  imnit(nel,ncetop), isimtf(nel,ncetop))

         ALLOCATE(kd1(nel,ncetop), kd2(nel,ncetop), khum(nel,ncetop), klit(nel,ncetop), &
                  kman(nel,ncetop), knit(nel,ncetop), kvol(nel,ncetop))

         ALLOCATE(miner(nel,ncetop))

         ALLOCATE(naamm(nel,ncetop), namm(nel,ncetop), namm1(nel,ncetop), nanit(nel,ncetop), &
                  ndnit(nel,ncetop), ndsnt(nel,ncetop), nlit(nel,ncetop), nlit1(nel,ncetop), &
                  nman(nel,ncetop), nman1(nel,ncetop), ntrf(nel,ncetop))

         ALLOCATE(plamm(nel,ncetop), plnit(nel,ncetop), plup(nel,ncetop), pphi(nel,ncetop))

         ALLOCATE(snit(nel,ncetop), temp(nel,ncetop), vol(nel,ncetop))
      END IF

   !----------------------------------------------------------------------*
   ! temp code    (sb 1/3/01)
      DO I = 1, NV
         TA(I) = 10.0D0
      END DO

      CALL MNPLANT(MNPL, MNOUTPL, NCETOP, NEL, NLF, NV, NCOLMB, NRD, NVC, RHOPL, DELONE, DXQQ, DYQQ, &
                   DELTAZ, PLAI, RDF, DTUZ, UZNOW, CLAI)

      CALL MNMAIN(MND, MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NCON, NEL, NLF, NS, NV, NX, NY, ICMBK, &
                  ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, &
                  ZVSNOD, BEXBK, LINKNS, DTUZ, UZNOW, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, &
                  SSS1, SSS2)

   END SUBROUTINE MNCONT



   !SSSSSS SUBROUTINE mnedth
   SUBROUTINE mnedth (llee, nbotce, ncetop, nel, nelee, nlf, nlyree, ns, &
         ncolmb, nlyr, nlyrbt, ntsoil, vsthe, vspor, isbotc)
   !--------------------------------------------------------------------*
   ! Calculates the moisture environmental reduction factor
   ! for denitrification
   !--------------------------------------------------------------------*
   ! version:                   notes:
   ! module: mn                 program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! Assumed external module dependencies providing global variables:
      ! edeth (nelee, llee)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: llee, nbotce, ncetop, nel, nelee, nlf, nlyree, ns
      INTEGER, INTENT(IN) :: ncolmb(nelee), nlyr(nelee)
      INTEGER, INTENT(IN) :: nlyrbt(nel, nlyree), ntsoil(nel, nlyree)
      DOUBLE PRECISION, INTENT(IN) :: vsthe(ncetop, nel), vspor(ns)
      LOGICAL, INTENT(IN) :: isbotc

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
   end    subroutine mnemph



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



   !SSSSSS SUBROUTINE MNERR0
   SUBROUTINE MNERR0(LLEE, MND, MNFC, MNFN, MNPR, NCETOP, NCON, NCONEE, NEL, NELEE, NLF, NLFEE, NLYREE, NMNEEE, NMNTEE, NS, NSEE, NV, NVEE, NX, NXEE, NY)
   !--------------------------------------------------------------------*
   !
   ! checks array dimensions
   ! error numbers 3010,3020-3034
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE, MND, MNFC, MNFN, MNPR, NCETOP, NCON, NCONEE, NEL
      INTEGER, INTENT(IN) :: NELEE, NLF, NLFEE, NLYREE, NMNEEE, NMNTEE, NS, NSEE
      INTEGER, INTENT(IN) :: NX, NXEE, NV, NVEE, NY

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2

      ! Modernization Fix: Lock IUNDEF to prevent uninitialized memory passing
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



   !SSSSSS SUBROUTINE MNERR1
   SUBROUTINE MNERR1(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NLFEE, NLYREE, NS, NX, NXEE, NY, ICMBK, ICMREF, &
                     ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, ZVSNOD, &
                     BEXBK, LINKNS, DUMMY2, DUMMY3, IDUM, IDUM1X, LDUM, LDUM2)
   !--------------------------------------------------------------------*
   !
   ! checks static input variables from cm -mn interface
   ! error numbers 3011,3035-3047 and 2075-2079 for index arrays
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NLFEE, NLYREE, NS
      INTEGER, INTENT(IN) :: NX, NXEE, NY
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2), ICMREF(NELEE, 4, 2:2), ICMXY(NXEE, NY)
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE), NTSOIL(NEL, NLYREE)
      DOUBLE PRECISION, INTENT(IN) :: D0, TIH, Z2
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS(NLFEE)

      ! Input/Output Arrays (Tested by ALCHK/ALCHKI; subject to internal data reset)
      INTEGER, INTENT(INOUT) :: NCOLMB(NELEE), NLYR(NELEE)
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NELEE), DYQQ(NELEE)
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(LLEE, NEL), ZVSNOD(LLEE, NEL)

      ! Workspace arguments (INTENT(INOUT) as they are used for scratch space)
      INTEGER, INTENT(INOUT) :: DUMMY2(NLYREE, NELEE), DUMMY3(NLYREE)
      INTEGER, INTENT(INOUT) :: IDUM(NELEE), IDUM1X(-1:NEL+1)
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE), LDUM2(LLEE)

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: BANK, BOTLYR, COUNT, FACE
      INTEGER :: IADJ, ICOL1, IEL, IX, IY
      INTEGER :: LINK, NCE, NCEBOT, NCOL, NELP
      INTEGER :: NERR, NLAYER, TOPLYR
      INTEGER :: IDUM1(2)
      DOUBLE PRECISION :: DUMS(1)
      LOGICAL :: BKXYOK

      ! Modernization Fix: Strict array/scalar parameters for shape matching in ALCHK
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



   !SSSSSS SUBROUTINE MNERR2
   SUBROUTINE MNERR2(MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, &
                     NMNEEE, NMNTEE, NS, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, KMELEM, KNELEM, KVELEM, NAELEM, NMN15T, NMN17T, NMN19T, NMN21T, &
                     NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, AMMDDR, AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT, FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT, &
                     MNCREF, NITDDR, NITWDR, Q10M, Q10N, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KDDSOL, KHCONC, KHDPTH, &
                     KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ISICCD, ISIAMD, LDUM)
   !--------------------------------------------------------------------*
   !
   ! checks static input data read in from mnred1 subroutine
   ! error numbers 3012,3048-3064
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF
      INTEGER, INTENT(IN) :: NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E
      INTEGER, INTENT(IN) :: NMN27E, NMN43E, NMN53E
      INTEGER, INTENT(IN) :: NMNEEE, NMNTEE, NS
      INTEGER, INTENT(IN) :: NMN15T(NMNEEE), NMN17T(NMNEEE), NMN19T(NMNEEE)
      INTEGER, INTENT(IN) :: NMN21T(NMNEEE), NMN23T(NMNEEE), NMN25T(NMNEEE)
      INTEGER, INTENT(IN) :: NMN27T(NMNEEE)
      INTEGER, INTENT(IN) :: NMN43T(NMNEEE), NMN53T(NMNEEE)
      DOUBLE PRECISION, INTENT(IN) :: AMMDDR, AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT
      DOUBLE PRECISION, INTENT(IN) :: FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT
      DOUBLE PRECISION, INTENT(IN) :: MNCREF, NITDDR, NITWDR, Q10M, Q10N
      LOGICAL, INTENT(IN) :: ISICCD, ISIAMD

      ! Arguments tested by ALCHK/ALCHKI (Strict INTENT(INOUT) to satisfy dummy arguments)
      INTEGER, INTENT(INOUT) :: CELEM(NLF+1:NEL), KD1ELM(NLF+1:NEL), KD2ELM(NLF+1:NEL)
      INTEGER, INTENT(INOUT) :: KHELEM(NLF+1:NEL), KLELEM(NLF+1:NEL), KMELEM(NLF+1:NEL)
      INTEGER, INTENT(INOUT) :: KNELEM(NLF+1:NEL), KVELEM(NLF+1:NEL)
      INTEGER, INTENT(INOUT) :: NAELEM(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: CCONC(NMNEEE,NMNTEE), CDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: CTOTTP(NLF+1:NEL), DAMHLF(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: DCHLF(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: KD1CNC(NMNEEE,NMNTEE), KD1DTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: KD2CNC(NMNEEE,NMNTEE), KD2DTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: KDDSOL(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: KHCONC(NMNEEE,NMNTEE), KHDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: KLCONC(NMNEEE,NMNTEE), KLDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: KMCONC(NMNEEE,NMNTEE), KMDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: KNCONC(NMNEEE,NMNTEE), KNDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: KVCONC(NMNEEE,NMNTEE), KVDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: NACONC(NMNEEE,NMNTEE), NADPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(INOUT) :: NAMTOP(NLF+1:NEL)

      ! Workspace arguments
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2, WARN = 3
      INTEGER :: ICOL1, NELMTY, NERR, NTAB

      ! Safe scalar passing arrays
      INTEGER :: IDUMS(1), IDUMO(1)
      DOUBLE PRECISION :: PREVDP_ARR(1), DUMS_ARR(1)

      ! Modernization Fix: Strict parameter shapes and locked IUNDEF
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



   !SSSSSS SUBROUTINE MNERR3
   SUBROUTINE MNERR3(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, UZNOW, CCCC, &
                     PNETTO, SSSS, VSTHE, VSTHEO, LDUM, LDUM2)
   !--------------------------------------------------------------------*
   !
   ! checks time dependent input variables from cm -mn interface and the
   ! concentrations calculated in this component are positive
   ! error numbers 3013 and 3065-3079
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! Assumed global variables provided via host module:
      ! USE MN_MODULE, ONLY: chum1, clit1, cman1, namm1, nlit1, nman1, plup

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: LLEE, MNPR, NCETOP, NEL, NELEE, NLF
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: DTUZ, UZNOW
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL), VSTHEO(NEL, NCETOP + 1)

      ! Arguments tested directly by ALCHK (Must be INTENT(INOUT) to satisfy dummy arguments)
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NELEE)

      ! Workspace arguments (INTENT(INOUT) because they act as scratch space)
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE), LDUM2(LLEE)

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: ICOL1, IEL, NCEBOT, NERR, NCE
      DOUBLE PRECISION :: DUMMY4(NCETOP, NEL)
      DOUBLE PRECISION :: DUMS_ARR(1)

      ! Protected static state variables
      INTEGER, SAVE :: PASS = 0
      DOUBLE PRECISION, SAVE :: UZPREV(1) = [0.0D0]

      ! Modernization Fix: Strict parameter shapes and locked IUNDEF
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



   !SSSSSS SUBROUTINE MNERR4
   SUBROUTINE MNERR4(MNPR, NEL, NELEE, NLF, CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, ISADDC, ISADDN, &
                     DUMMY, LDUM)
   !--------------------------------------------------------------------*
   !
   !  checks time varying dependent data read in mnred2
   !  error numbers 3014 and 3080-3089
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: MNPR, NEL, NELEE, NLF
      LOGICAL, INTENT(IN) :: ISADDC, ISADDN

      ! Arguments tested directly by ALCHK (Must be INTENT(INOUT) to satisfy dummy arguments)
      DOUBLE PRECISION, INTENT(INOUT) :: CDPTHB(NLF+1:NEL), CLTFCT(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: CMNFCT(NLF+1:NEL), CNRAL(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: CNRAM(NLF+1:NEL), CTOT(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: NAMFCT(NLF+1:NEL), NDPTHB(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: NTOT(NLF+1:NEL)

      ! Workspace arguments (INTENT(INOUT) because they act as scratch space)
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)
      LOGICAL, INTENT(INOUT)          :: LDUM(NELEE)

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: ICOL1, IEL, NERR

      ! Modernization Fix: Separate Array parameters (for OBJ) and Scalar parameters (for TOL)
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
               ! Modern Fix: Array slice (IEL:IEL) to satisfy F77 array interface
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



   !SSSSSS SUBROUTINE MNGAM
   SUBROUTINE MNGAM(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, CNRHUM, CNRBIO, FE, FH, DTUZ, ISBOTC)
   !--------------------------------------------------------------------*
   !
   ! calculates the mineralisation/immobilisation rate. if mngam is
   ! positive then this is the mineralisation rate. if mngam is negative
   ! then this is the immobilisation rate
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! Assumed global variables provided via host module:
      ! USE MN_MODULE, ONLY: chum, chum1, clit, clit1, cman, cman1, nlit, &
      !                      nlit1, nman, nman1, isimtf, klit, kman, &
      !                      emt, emph, khum, gam, gamtmp, imdiff

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: CNRBIO, CNRHUM
      DOUBLE PRECISION, INTENT(IN) :: FE, FH
      DOUBLE PRECISION, INTENT(IN) :: DTUZ
      LOGICAL, INTENT(IN) :: ISBOTC

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

            ! Modernization Fix: Enforced strict double-precision (1.0D0) to prevent precision loss
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



   !SSSSSS SUBROUTINE MNINIT
   SUBROUTINE MNINIT(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, &
                     NMNEEE, NMNTEE, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, KMELEM, KNELEM, KVELEM, NAELEM, NCOLMB, NMN15T, NMN17T, NMN19T, NMN21T, &
                     NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, CLITFR, CNRLIT, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, DELTAZ, KD1CNC, KD1DTH, KD2CNC, &
                     KD2DTH, KHCONC, KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ZVSNOD, ISICCD, &
                     ISIAMD, SSS1, SSS2, ISBOTC)
   !--------------------------------------------------------------------*
   !
   ! initialises the global variables
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! Assumed global variables provided via host module:
      ! USE MN_MODULE, ONLY: chum1, clit1, cman1, imdiff, kd1, kd2, khum,
      !                      klit, kman, knit, kvol, namm1, nlit1, nman1,
      !                      isimtf, dummy6

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF
      INTEGER, INTENT(IN) :: NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E
      INTEGER, INTENT(IN) :: NMN27E, NMN43E, NMN53E
      INTEGER, INTENT(IN) :: NMNEEE, NMNTEE
      INTEGER, INTENT(IN) :: CELEM(NLF+1:NEL), KD1ELM(NLF+1:NEL), KD2ELM(NLF+1:NEL)
      INTEGER, INTENT(IN) :: KHELEM(NLF+1:NEL), KLELEM(NLF+1:NEL), KMELEM(NLF+1:NEL)
      INTEGER, INTENT(IN) :: KNELEM(NLF+1:NEL), KVELEM(NLF+1:NEL)
      INTEGER, INTENT(IN) :: NAELEM(NLF+1:NEL), NCOLMB(NELEE)
      INTEGER, INTENT(IN) :: NMN15T(NMNEEE), NMN17T(NMNEEE), NMN19T(NMNEEE)
      INTEGER, INTENT(IN) :: NMN21T(NMNEEE), NMN23T(NMNEEE), NMN25T(NMNEEE)
      INTEGER, INTENT(IN) :: NMN27T(NMNEEE)
      INTEGER, INTENT(IN) :: NMN43T(NMNEEE), NMN53T(NMNEEE)

      DOUBLE PRECISION, INTENT(IN) :: CLITFR, CNRLIT
      DOUBLE PRECISION, INTENT(IN) :: CCONC(NMNEEE,NMNTEE), CDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: CTOTTP(NLF+1:NEL), DAMHLF(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: DCHLF(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE,NEL)
      DOUBLE PRECISION, INTENT(IN) :: KD1CNC(NMNEEE,NMNTEE), KD1DTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: KD2CNC(NMNEEE,NMNTEE), KD2DTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: KHCONC(NMNEEE,NMNTEE), KHDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: KLCONC(NMNEEE,NMNTEE), KLDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: KMCONC(NMNEEE,NMNTEE), KMDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: KNCONC(NMNEEE,NMNTEE), KNDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: KVCONC(NMNEEE,NMNTEE), KVDPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: NACONC(NMNEEE,NMNTEE), NADPTH(NMNEEE,NMNTEE)
      DOUBLE PRECISION, INTENT(IN) :: NAMTOP(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: ZVSNOD(LLEE,NEL)

      LOGICAL, INTENT(IN) :: ISICCD, ISIAMD

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP+1), SSS2(NEL, NCETOP+1)
      LOGICAL, INTENT(OUT)          :: ISBOTC

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
         ! Modern Fix: Passed NCOLMB array slice instead of scalar memory address
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



   !SSSSSS SUBROUTINE MNINT2
   SUBROUTINE MNINT2(LLEE, NCETOP, NEL, NELEE, NLF, NLYREE, NCOLMB, NLYR, NLYRBT, NTSOIL, AMMDDR, AMMWDR, MNCREF, NITDDR, NITWDR, &
                     DELTAZ, DTUZ, CCCC, CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, &
                     PNETTO, SSSS, VSTHE, ISADDC, ISADDN, CNRALT, CNRAMN, DUMMY)
   !--------------------------------------------------------------------*
   !
   !  modifies the time varying input variables into suitable units
   !  for the rest of the program
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! Assumed global variables provided via host module:
      ! USE MN_MODULE, ONLY: cman, cman1, nman, nman1, clit, clit1, chum,
      !                      chum1, nlit, nlit1, namm, namm1, ndnit, ndsnt,
      !                      pphi, naamm, nanit, calit, caman, cahum

      IMPLICIT NONE

      ! Input arguments
      ! * stationary
      INTEGER, INTENT(IN) :: LLEE, NCETOP, NEL, NELEE, NLF, NLYREE
      INTEGER, INTENT(IN) :: NCOLMB(NELEE), NLYR(NELEE)
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE), NTSOIL(NEL, NLYREE)
      DOUBLE PRECISION, INTENT(IN) :: AMMDDR, AMMWDR
      DOUBLE PRECISION, INTENT(IN) :: MNCREF, NITDDR, NITWDR
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE, NEL)

      ! * time dependent
      DOUBLE PRECISION, INTENT(IN) :: DTUZ
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)
      DOUBLE PRECISION, INTENT(IN) :: CDPTHB(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: CLTFCT(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: CMNFCT(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: CNRAL(NLF + 1:NEL), CNRAM(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: CTOT(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: NAMFCT(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: NDPTHB(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: NTOT(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(IN) :: PNETTO(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1), VSTHE(NCETOP, NEL)
      LOGICAL, INTENT(IN) :: ISADDC, ISADDN

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CNRALT(NELEE), CNRAMN(NELEE)

      ! Workspace
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)

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
   SUBROUTINE mnlthm (llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, fe, fh, dtuz, isbotc)

      ! Assumed external module dependencies providing global variables:
      ! clit, chum, cman, cman1, isimtf, kman, klit, emt, emph, khum,
      ! calit, clit1, cahum, chum1, ERROR

      IMPLICIT NONE

      ! input arguments
      INTEGER, INTENT(IN) :: llee, mnpr, nbotce, ncetop, nel, nelee, nlf
      INTEGER, INTENT(IN) :: ncolmb(nelee)
      DOUBLE PRECISION, INTENT(IN) :: fe, fh, dtuz
      LOGICAL, INTENT(IN) :: isbotc

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
               ! PERF FIX: Restored external format label
               WRITE (msg, 9000) wer1sq, wer2sq
               CALL ERROR(warn, 3016, mnpr, 0, 0, msg)
            END IF

         END DO layer_loop
      END DO

9000  FORMAT('iteration loop in mnlthm failed to converge with error = ', g15.7, g15.7)

   END SUBROUTINE mnlthm



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
   SUBROUTINE mnltn (llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, cnrbio, fe, fh, dtuz, cnralt, isbotc)

      ! Assumed external module dependencies providing global variables:
      ! chum, chum1, clit, clit1, cman, cman1, nlit, nlit1, isimtf,
      ! klit, kman, emt, emph, khum, calit, ERROR

      IMPLICIT NONE

      ! input arguments
      INTEGER, INTENT(IN) :: llee, mnpr, nbotce, ncetop, nel, nelee, nlf
      INTEGER, INTENT(IN) :: ncolmb(nelee)
      DOUBLE PRECISION, INTENT(IN) :: cnrbio, fe, fh
      DOUBLE PRECISION, INTENT(IN) :: dtuz
      DOUBLE PRECISION, INTENT(IN) :: cnralt(nelee)
      LOGICAL, INTENT(IN) :: isbotc

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
               ! PERF FIX: Restored external format label
               WRITE (msg, 9000) wer1sq
               CALL ERROR(warn, 3017, mnpr, 0, 0, msg)
            END IF

         END DO layer_loop
      END DO

9000  FORMAT('iteration loop in mnltn failed to converge with error = ', g15.7)

   END SUBROUTINE mnltn



   !SSSSSS SUBROUTINE MNMAIN
   SUBROUTINE MNMAIN(MND, MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NCON, NEL, NLF, NS, NV, NX, NY, ICMBK, &
                     ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, &
                     ZVSNOD, BEXBK, LINKNS, DTUZ, UZNOW, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, &
                     SSS1, SSS2)
   !--------------------------------------------------------------------*
   !
   ! main mn subroutine from which all the others are called
   !
   !--------------------------------------------------------------------*
   ! version: 4.2             notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! USE SGLOBAL, ONLY : llee, nelee, nlfee, nlyree, nxee, nsee, nvee, nconee

      IMPLICIT NONE

      ! Input arguments
      ! * static
      INTEGER, INTENT(IN) :: MND, MNFC, MNFN, MNPR, MNOUT1, MNOUT2
      INTEGER, INTENT(IN) :: NCETOP, NCON, NEL, NLF, NS, NV, NX, NY
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2), ICMREF(NELEE, 4, 2:2), ICMXY(NXEE, NY)
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE), NTSOIL(NEL, NLYREE)
      DOUBLE PRECISION, INTENT(IN) :: D0, TIH, Z2
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS(NLFEE)

      ! * varying
      DOUBLE PRECISION, INTENT(IN) :: DTUZ, UZNOW
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)
      DOUBLE PRECISION, INTENT(IN) :: TA(NV), VSPSI(NCETOP, NEL)
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL), VSTHEO(NEL, NCETOP + 1)

      ! Input/Output arguments (Propagated up from MNERR1, MNERR3 requirements)
      INTEGER, INTENT(INOUT) :: NCOLMB(NELEE), NLYR(NELEE)
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NELEE), DYQQ(NELEE)
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(LLEE, NEL), ZVSNOD(LLEE, NEL)
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NELEE)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: SSS1(NEL, NCETOP + 1), SSS2(NEL, NCETOP + 1)

      ! Constants
      INTEGER, PARAMETER :: NMNEEE = 9, NMNTEE = 10

      ! Protected Static State Variables (Replacing F77 SAVE blocks)
      INTEGER, SAVE :: NBOTCE, PASS = 0
      DOUBLE PRECISION, SAVE :: AMMDDR, AMMWDR, CNRBIO, CNRHUM, FE, FH, GNN
      DOUBLE PRECISION, SAVE :: KPLAMM, KPLNIT, KUAMM, KUNIT, MNCREF, NITDDR
      DOUBLE PRECISION, SAVE :: NITWDR, Q10M, Q10N
      DOUBLE PRECISION, SAVE :: KDDSOL(NSEE)
      LOGICAL, SAVE :: ISBOTC, ISQ10

      ! Locals (Not saved)
      INTEGER :: NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E
      INTEGER :: NMN27E, NMN43E, NMN53E
      INTEGER :: CELEM(NELEE), KD1ELM(NELEE), KD2ELM(NELEE)
      INTEGER :: KHELEM(NELEE), KLELEM(NELEE), KMELEM(NELEE)
      INTEGER :: KNELEM(NELEE), KVELEM(NELEE)
      INTEGER :: NAELEM(NELEE)
      INTEGER :: NMN15T(NMNEEE), NMN17T(NMNEEE), NMN19T(NMNEEE)
      INTEGER :: NMN21T(NMNEEE), NMN23T(NMNEEE), NMN25T(NMNEEE)
      INTEGER :: NMN27T(NMNEEE)
      INTEGER :: NMN43T(NMNEEE), NMN53T(NMNEEE)
      INTEGER :: DUMMY2(NLYREE, NELEE), DUMMY3(NLYREE)
      INTEGER :: IDUM(NELEE), IDUM1X(NELEE + 3)

      DOUBLE PRECISION :: CLITFR, CNRLIT
      DOUBLE PRECISION :: CDPTHB(NELEE), CLTFCT(NELEE)
      DOUBLE PRECISION :: CMNFCT(NELEE), CNRAL(NELEE), CNRALT(NELEE)
      DOUBLE PRECISION :: CNRAM(NELEE), CNRAMN(NELEE)
      DOUBLE PRECISION :: CTOT(NELEE), CTOTTP(NELEE)
      DOUBLE PRECISION :: DAMHLF(NELEE), DCHLF(NELEE)
      DOUBLE PRECISION :: NAMFCT(NELEE), NAMTOP(NELEE), NDPTHB(NELEE)
      DOUBLE PRECISION :: NTOT(NELEE)
      DOUBLE PRECISION :: CCONC(NMNEEE, NMNTEE), CDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KD1CNC(NMNEEE, NMNTEE), KD1DTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KD2CNC(NMNEEE, NMNTEE), KD2DTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KHCONC(NMNEEE, NMNTEE), KHDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KLCONC(NMNEEE, NMNTEE), KLDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KMCONC(NMNEEE, NMNTEE), KMDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KNCONC(NMNEEE, NMNTEE), KNDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: KVCONC(NMNEEE, NMNTEE), KVDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: NACONC(NMNEEE, NMNTEE), NADPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION :: DUMMY(NELEE)

      LOGICAL :: ISADDC, ISADDN, ISICCD, ISIAMD
      LOGICAL :: LDUM(NELEE), LDUM2(LLEE)

   !-------------------------------------------------------------------*

      PASS = PASS + 1

      IF (PASS == 1) THEN
         !------------------------ initialization step  ---------------------*

         ! * check array dimensions
         CALL MNERR0(LLEE, MND, MNFC, MNFN, MNPR, NCETOP, NCON, NCONEE, NEL, NELEE, NLF, NLFEE, NLYREE, NMNEEE, NMNTEE, NS, NSEE, NV, &
                     NVEE, NX, NXEE, NY)

         ! * checks static input variables from cm - mn interface
         CALL MNERR1(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NLFEE, NLYREE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, NCOLMB, NLYR, NLYRBT, &
                     NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, ZVSNOD, BEXBK, LINKNS, DUMMY2, DUMMY3, IDUM, IDUM1X, LDUM, LDUM2)

         ! * read the input data files
         CALL MNRED1(MND, MNPR, NEL, NELEE, NLF, NLFEE, NMNEEE, NMNTEE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, BEXBK, LINKNS, NBOTCE, &
                     NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, CELEM(NLF + 1:NEL), KD1ELM(NLF + 1:NEL), &
                     KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), KNELEM(NLF + 1:NEL), &
                     KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, &
                     AMMDDR, AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT, FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT, MNCREF, NITDDR, &
                     NITWDR, Q10M, Q10N, CCONC, CDPTH, CTOTTP(NLF + 1:NEL), DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), KD1CNC, KD1DTH, &
                     KD2CNC, KD2DTH, KDDSOL, KHCONC, KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, &
                     NADPTH, NAMTOP(NLF + 1:NEL), ISICCD, ISIAMD, ISQ10, IDUM, DUMMY)

         ! * checks static input data read in mnred1
         CALL MNERR2(MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, &
                     NMNEEE, NMNTEE, NS, CELEM(NLF + 1:NEL), KD1ELM(NLF + 1:NEL), KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), &
                     KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), KNELEM(NLF + 1:NEL), KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NMN15T, &
                     NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, AMMDDR, AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT, &
                     FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT, MNCREF, NITDDR, NITWDR, Q10M, Q10N, CCONC, CDPTH, CTOTTP(NLF + 1:NEL), &
                     DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), KD1CNC, KD1DTH, KD2CNC, KD2DTH, KDDSOL, KHCONC, KHDPTH, KLCONC, KLDPTH, &
                     KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP(NLF + 1:NEL), ISICCD, ISIAMD, LDUM)

         ! * initialises variables
         CALL MNINIT(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, &
                     NMNEEE, NMNTEE, CELEM(NLF + 1:NEL), KD1ELM(NLF + 1:NEL), KD2ELM(NLF + 1:NEL), KHELEM(NLF + 1:NEL), &
                     KLELEM(NLF + 1:NEL), KMELEM(NLF + 1:NEL), KNELEM(NLF + 1:NEL), KVELEM(NLF + 1:NEL), NAELEM(NLF + 1:NEL), NCOLMB, &
                     NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, CLITFR, CNRLIT, CCONC, CDPTH, &
                     CTOTTP(NLF + 1:NEL), DAMHLF(NLF + 1:NEL), DCHLF(NLF + 1:NEL), DELTAZ, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KHCONC, &
                     KHDPTH, KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP(NLF + 1:NEL), &
                     ZVSNOD, ISICCD, ISIAMD, SSS1, SSS2, ISBOTC)

         !----------------------- end of initialization step------------------*

      ELSE
         !------------------------ simulation step ---------------------------*

         ! * checks time varying input variables from cm - mn interface
         CALL MNERR3(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, UZNOW, CCCC, PNETTO, SSSS, VSTHE, VSTHEO, LDUM, LDUM2)

         ! * reads time varying input data
         CALL MNRED2(MNFC, MNFN, MNPR, NEL, NELEE, NLF, NLFEE, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, DTUZ, TIH, UZNOW, BEXBK, LINKNS, &
                     CDPTHB(NLF + 1:NEL), CLTFCT(NLF + 1:NEL), CMNFCT(NLF + 1:NEL), CNRAL(NLF + 1:NEL), CNRAM(NLF + 1:NEL), &
                     CTOT(NLF + 1:NEL), NAMFCT(NLF + 1:NEL), NDPTHB(NLF + 1:NEL), NTOT(NLF + 1:NEL), ISADDC, ISADDN, IDUM, DUMMY)

         ! * checks time dependent input data read in mnred2
         CALL MNERR4(MNPR, NEL, NELEE, NLF, CDPTHB(NLF + 1:NEL), CLTFCT(NLF + 1:NEL), CMNFCT(NLF + 1:NEL), CNRAL(NLF + 1:NEL), &
                     CNRAM(NLF + 1:NEL), CTOT(NLF + 1:NEL), NAMFCT(NLF + 1:NEL), NDPTHB(NLF + 1:NEL), NTOT(NLF + 1:NEL), ISADDC, &
                     ISADDN, DUMMY, LDUM)

         ! * modifies data read in mnred2 into suitable units and form for the rest of the program
         CALL MNINT2(LLEE, NCETOP, NEL, NELEE, NLF, NLYREE, NCOLMB, NLYR, NLYRBT, NTSOIL, AMMDDR, AMMWDR, MNCREF, NITDDR, NITWDR, &
                     DELTAZ, DTUZ, CCCC, CDPTHB(NLF + 1:NEL), CLTFCT(NLF + 1:NEL), CMNFCT(NLF + 1:NEL), CNRAL(NLF + 1:NEL), &
                     CNRAM(NLF + 1:NEL), CTOT(NLF + 1:NEL), NAMFCT(NLF + 1:NEL), NDPTHB(NLF + 1:NEL), NTOT(NLF + 1:NEL), PNETTO, &
                     SSSS, VSTHE, ISADDC, ISADDN, CNRALT, CNRAMN, DUMMY)

         ! * environmental reduction factors are calculated
         CALL MNTEMP(LLEE, NCETOP, NEL, NELEE, NLF, NV, NCOLMB, Z2, DELTAZ, ZVSNOD, DTUZ, TA)
         CALL MNEMT(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, Q10M, ISBOTC, ISQ10)
         CALL MNENT(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, Q10N, ISBOTC, ISQ10)
         CALL MNEMPH(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, VSPSI, ISBOTC)
         CALL MNENPH(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, VSPSI, ISBOTC)
         CALL MNEDTH(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NLYREE, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, VSTHE, VSPOR, ISBOTC)

         ! * new concentration of carbon and nitrogen manure pools
         CALL MNMAN(LLEE, MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, CNRAMN, ISBOTC)

         ! * new concentration of carbon litter and humus pools
         CALL MNLTHM(LLEE, MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, FE, FH, DTUZ, ISBOTC)

         ! * new concentration of nitrogen litter pool
         CALL MNLTN(LLEE, MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, CNRBIO, FE, FH, DTUZ, CNRALT, ISBOTC)

         ! * carbon dioxide production
         CALL MNCO2(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, FE, FH, ISBOTC)

         ! * mineralization/immobilisation rate
         CALL MNGAM(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, CNRHUM, CNRBIO, FE, FH, DTUZ, ISBOTC)

         ! * new concentration of ammonium
         CALL MNAMM(LLEE, MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF, NLYREE, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, GNN, KPLAMM, KUAMM, &
                    MNCREF, KDDSOL, DTUZ, VSTHE, VSTHEO, ISBOTC)

         ! * new nitrate concentration in dynamic and dead space regions
         CALL MNNIT(LLEE, NBOTCE, NCETOP, NEL, NELEE, NLF, NCOLMB, D0, KPLNIT, KUNIT, MNCREF, Z2, DTUZ, VSTHE, VSTHEO, ISBOTC, &
                    SSS1, SSS2)

         ! * extra output that may be required that is printed in this subroutine
         CALL MNOUT(MNOUT1, MNOUT2, NBOTCE, NCETOP, NEL, NLF, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, CNRHUM, GNN, MNCREF, DELTAZ, &
                    KDDSOL, PPHI, DTUZ, UZNOW, DXQQ, DYQQ, CNRALT, CNRAMN, VSTHE, VSTHEO, ISBOTC)

         !------------------------end of simulation step---------------------*
      END IF

   END SUBROUTINE MNMAIN



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
   SUBROUTINE mnman (llee, mnpr, nbotce, ncetop, nel, nelee, nlf, ncolmb, dtuz, cnramn, isbotc)

      ! Assumed external module dependencies providing global variables:
      ! cman, cman1, nman, nman1, isimtf, kman, emt, emph, caman, ERROR

      IMPLICIT NONE

      ! input arguments
      INTEGER, INTENT(IN) :: llee, mnpr, nbotce, ncetop, nel, nelee, nlf
      INTEGER, INTENT(IN) :: ncolmb(nelee)
      DOUBLE PRECISION, INTENT(IN) :: dtuz
      DOUBLE PRECISION, INTENT(IN) :: cnramn(nelee)
      LOGICAL, INTENT(IN) :: isbotc

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



   !SSSSSS SUBROUTINE MNOUT
   SUBROUTINE MNOUT(MNOUT1, MNOUT2, NBOTCE, NCETOP, NEL, NLF, NS, NCOLMB, NLYR, NLYRBT, NTSOIL, CNRHUM, GNN, MNCREF, DELTAZ, &
                    KDDSOL, PPHI, DTUZ, UZNOW, DXQQ, DYQQ, CNRALT, CNRAMN, VSTHE, VSTHEO, ISBOTC)
   !--------------------------------------------------------------------*
   !
   ! calculates total inputs and outputs and writes output to files
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! Assumed global variables provided via host module:
      ! USE MN_MODULE, ONLY: llee, nelee, nlyree, namm, nlit, nman, chum,
      !                      cman, clit, naamm, caman, cahum, calit, nanit,
      !                      cdort, denit, gamtmp, imamm, imnit, miner,
      !                      ntrf, plamm, plnit, snit, vol, namm1, nlit1,
      !                      nman1, chum1, cman1, clit1

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: MNOUT1, MNOUT2, NBOTCE, NCETOP, NEL, NLF, NS
      INTEGER, INTENT(IN) :: NCOLMB(NELEE), NLYR(NELEE)
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE), NTSOIL(NEL, NLYREE)
      DOUBLE PRECISION, INTENT(IN) :: CNRHUM, GNN, MNCREF
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE, NEL), KDDSOL(NS)
      DOUBLE PRECISION, INTENT(IN) :: PPHI(NELEE, LLEE)
      DOUBLE PRECISION, INTENT(IN) :: DTUZ, UZNOW
      DOUBLE PRECISION, INTENT(IN) :: DXQQ(NELEE), DYQQ(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: CNRALT(NELEE), CNRAMN(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL), VSTHEO(NEL, NCETOP + 1)
      LOGICAL, INTENT(IN) :: ISBOTC

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

      TOTADN = 0.0D0
      TOTADC = 0.0D0
      TOTLOS = 0.0D0
      TOTN   = 0.0D0
      TOTC   = 0.0D0
      TOTCO2 = 0.0D0

   ! Progressive sum logic restored exactly as provided
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

   ! Output reporting block
      IF (UZNOW >= HRPRNT * NPRNT + MNSTRT) THEN

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


   !--------------------------------------------------------------------*
   SUBROUTINE mnplant (mnpl, mnoutpl, ncetop, nel, nlf, nv, ncolmb, nrd, nvc, rhopl, delone, dxqq, dyqq, deltaz, plai, rdf, dtuz, &
                       uznow, clai)
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

      ! Assumed external module dependencies providing global variables:
      ! llee, nelee, npelee, npltee, nvee, plup, alredf, alredi, alred2,
      ! alalli, alredl, alredc

      IMPLICIT NONE

      ! input arguments
      !     * static
      INTEGER, INTENT(IN) :: mnpl, mnoutpl, ncetop, nel, nlf, nv
      INTEGER, INTENT(IN) :: ncolmb(nelee)
      INTEGER, INTENT(IN) :: nrd(nv), nvc(nelee)
      DOUBLE PRECISION, INTENT(IN) :: rhopl
      DOUBLE PRECISION, INTENT(IN) :: delone(npltee), dxqq(nelee), dyqq(nelee)
      DOUBLE PRECISION, INTENT(IN) :: deltaz(llee, nel), plai(nv)
      DOUBLE PRECISION, INTENT(IN) :: rdf(nv, llee)

      !     * time dependent
      DOUBLE PRECISION, INTENT(IN) :: dtuz, uznow
      DOUBLE PRECISION, INTENT(IN) :: clai(nv)

      ! locals
      !     * maximum number of values in the input data for canopy density
      INTEGER, PARAMETER :: nvalee = 30

      !     * those saved
      INTEGER, SAVE :: nvalue(npltee), pass = 0
      INTEGER, SAVE :: npl(nelee), npltyp(nelee, npelee)
      DOUBLE PRECISION, SAVE :: cdi(npltee, nvalee), cdit(npltee, nvalee)
      DOUBLE PRECISION, SAVE :: claimx(npltee)
      DOUBLE PRECISION, SAVE :: croptm(nelee, npelee)
      DOUBLE PRECISION, SAVE :: gmcpbb(nelee, npelee)
      DOUBLE PRECISION, SAVE :: massb(nelee, npelee)
      DOUBLE PRECISION, SAVE :: pfone(nelee, npelee)
      LOGICAL, SAVE :: iscrop(nelee, npelee)

      !     * those not saved
      INTEGER :: jplty, ndata, nelm, nplant, nrbot, ntb
      INTEGER :: i, nce, ndum
      INTEGER :: idum(1)
      DOUBLE PRECISION :: cdfnc, chgmas, fn, massbo, tmsncr
      DOUBLE PRECISION :: dum, dum2
      DOUBLE PRECISION :: dummy(nvalee * 2)

      !      * temporary variable to test this subroutine
      CHARACTER(LEN=32) :: msg
      CHARACTER(LEN=200) :: cdum(1)

      !----------------------------------------------------------------------*

      pass = pass + 1

      IF (pass == 1) THEN
         !----------------------------------------------------------------------*
         !     initialising step
         !----------------------------------------------------------------------*
         !
         !        extra data for the canopy density index
         !        this is used to correct the canopy leaf area index so that the
         !        plant uptake of nitrogen is more accurate
         !
         !        * check status of data file
         CALL alred2(0, mnpl, mnoutpl, 'mnptin')

         !        * print title for data file
         CALL alredc(0, mnpl, mnoutpl, ':MNP1', 1, 1, cdum)
         WRITE (mnoutpl, '(/1x,A/)') cdum

         DO i = 1, nv
            CALL alredi (0, mnpl, mnoutpl, ':MNP10', 1, 1, idum)
            nvalue(i) = idum(1)
            ndata = idum(1) * 2
            CALL alredf(0, mnpl, mnoutpl, ':MNP11', ndata, 1, dummy)

            DO ntb = 1, idum(1)
               cdi(nv, ntb) = dummy(2 * ntb - 1)
               cdit(nv, ntb) = dummy(2 * ntb)
            END DO
         END DO

         CLOSE (mnpl)
         CLOSE (mnoutpl)

         DO nelm = nlf + 1, nel
            ! **************** temporary
            !                hard code the maximum leaf area index
            DO i = 1, npltee
               claimx(i) = 2.0d0
            END DO

            ! *************** temporary
            !                 set number of plant types on each column
            !                 temporarily, only two plant types are allowed on each
            !                 column and the total plai is one
            !                 second plant type number is set in block data

            npltyp(nelm, 1) = nvc(nelm)
            pfone(nelm, 1) = plai(npltyp(nelm, 1))

            IF (pfone(nelm, 1) >= 0.99d0) THEN
               npl(nelm) = 1
            ELSE
               pfone(nelm, 2) = 1.0d0 - pfone(nelm, 1)
               npl(nelm) = 2
            END IF

            !* sb 5/3/01 add data from pldat.f
            !* all second plant types on a grid square are equal to 1i=1,nel
            DO i = 1, nel
               npltyp(i, 2) = 1
            END DO

            DO nplant = 1, npl(nelm)
               ! plant type number
               jplty = npltyp(nelm, nplant)
               gmcpbb(nelm, nplant) = clai(jplty) * delone(jplty) / claimx(jplty)
               ! initialise for mass in compartment b
               massb(nelm, nplant) = gmcpbb(nelm, nplant) * pfone(nelm, nplant) * dxqq(nelm) * dyqq(nelm) * rhopl
               croptm(nelm, nplant) = 0.0d0
            END DO
         END DO

      ELSE
         DO nelm = nlf + 1, nel
            DO nce = ncolmb(nelm), ncetop
               plup(nelm, nce) = 0.0d0
            END DO
         END DO

         DO nelm = nlf + 1, nel
            DO nplant = 1, npl(nelm)
               jplty = npltyp(nelm, nplant)

               age_search_loop: DO i = 2, nvalue(jplty)
                  IF ((uznow / 24.0d0) < cdit(jplty, i)) THEN
                     dum = (cdi(jplty, i) - cdi(jplty, i - 1)) / (cdit(jplty, i) - cdit(jplty, i - 1))
                     dum2 = uznow / 24.0d0 - cdit(jplty, i - 1)
                     cdfnc = cdi(jplty, i - 1) + dum * dum2
                     EXIT age_search_loop
                  END IF
               END DO age_search_loop

               ! PERF FIX: Only assign 1.0d0 if the loop completed without exiting early.
               ! This exactly mimics the old GOTO 460 bypass with zero pipeline stalls.
               IF (i > nvalue(jplty)) cdfnc = 1.0d0

               nrbot = ncetop - nrd(jplty)
               gmcpbb(nelm, nplant) = clai(jplty) * delone(jplty) * cdfnc / claimx(jplty)
               massbo = massb(nelm, nplant)
               massb(nelm, nplant) = gmcpbb(nelm, nplant) * pfone(nelm, nplant) * dxqq(nelm) * dyqq(nelm) * rhopl
               chgmas = (massb(nelm, nplant) - massbo) / dtuz

               IF (chgmas < 0.0d0) THEN
                  iscrop(nelm, nplant) = .TRUE.
               ELSE IF (clai(jplty) > 0.0d0) THEN
                  IF (iscrop(nelm, nplant)) THEN
                     croptm(nelm, nplant) = uznow
                     iscrop(nelm, nplant) = .FALSE.
                  END IF

                  tmsncr = uznow - croptm(nelm, nplant)

                  ! Calculate uptake fraction based on crop age
                  IF (tmsncr < 360.0d0) THEN
                     fn = 0.022d0
                  ELSE IF (tmsncr < 720.0d0) THEN
                     fn = 0.017d0
                  ELSE IF (tmsncr < 1080.0d0) THEN
                     fn = 0.015d0
                  ELSE
                     fn = 0.012d0
                  END IF

                  ! Distribute the mass uptake across the root zone layers
                  DO nce = nrbot, ncetop
                     ndum = ncetop - nce + 1
                     plup(nelm, nce) = plup(nelm, nce) + chgmas * fn * rdf(jplty, ndum) / (deltaz(nce, nelm) * dxqq(nelm) * dyqq(nelm))
                  END DO
               END IF
            END DO
         END DO
      END IF
   END SUBROUTINE mnplant



   !SSSSSS SUBROUTINE MNRED1
   SUBROUTINE MNRED1(MND, MNPR, NEL, NELEE, NLF, NLFEE, NMNEEE, NMNTEE, NS, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, BEXBK, LINKNS, NBOTCE, &
                     NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, &
                     KMELEM, KNELEM, KVELEM, NAELEM, NMN15T, NMN17T, NMN19T, NMN21T, NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, AMMDDR, &
                     AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT, FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT, MNCREF, NITDDR, NITWDR, Q10M, &
                     Q10N, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KDDSOL, KHCONC, KHDPTH, KLCONC, KLDPTH, &
                     KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ISICCD, ISIAMD, ISQ10, IDUM, DUMMY)
   !--------------------------------------------------------------------*
   !
   ! reads input from files
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! Modernization Fix: Restored required global variable for ALALLF interface
      USE SGLOBAL, ONLY : nyee

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: MND, MNPR, NEL, NELEE, NLF, NLFEE, NMNEEE, NMNTEE, NS, NX, NXEE, NY
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2), ICMREF(NELEE, 4, 2:2), ICMXY(NXEE, NY)
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS(NLFEE)

      ! Output arguments
      INTEGER, INTENT(OUT) :: NBOTCE, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E
      INTEGER, INTENT(OUT) :: NMN27E, NMN43E, NMN53E
      INTEGER, INTENT(OUT) :: CELEM(NLF+1:NEL), KD1ELM(NLF+1:NEL), KD2ELM(NLF+1:NEL)
      INTEGER, INTENT(OUT) :: KHELEM(NLF+1:NEL), KLELEM(NLF+1:NEL), KMELEM(NLF+1:NEL)
      INTEGER, INTENT(OUT) :: KNELEM(NLF+1:NEL), KVELEM(NLF+1:NEL)
      INTEGER, INTENT(OUT) :: NAELEM(NLF+1:NEL)
      INTEGER, INTENT(OUT) :: NMN15T(NMNEEE), NMN17T(NMNEEE), NMN19T(NMNEEE)
      INTEGER, INTENT(OUT) :: NMN21T(NMNEEE), NMN23T(NMNEEE), NMN25T(NMNEEE)
      INTEGER, INTENT(OUT) :: NMN27T(NMNEEE)
      INTEGER, INTENT(OUT) :: NMN43T(NMNEEE), NMN53T(NMNEEE)

      DOUBLE PRECISION, INTENT(OUT) :: AMMDDR, AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT
      DOUBLE PRECISION, INTENT(OUT) :: FE, FH, GNN, KPLAMM, KPLNIT
      DOUBLE PRECISION, INTENT(OUT) :: KUAMM, KUNIT, MNCREF, NITDDR, NITWDR
      DOUBLE PRECISION, INTENT(OUT) :: Q10M, Q10N
      DOUBLE PRECISION, INTENT(OUT) :: CCONC(NMNEEE, NMNTEE), CDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: CTOTTP(NLF+1:NEL), DAMHLF(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(OUT) :: DCHLF(NLF+1:NEL)
      DOUBLE PRECISION, INTENT(OUT) :: KD1CNC(NMNEEE, NMNTEE), KD1DTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: KD2CNC(NMNEEE, NMNTEE), KD2DTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: KDDSOL(NS)
      DOUBLE PRECISION, INTENT(OUT) :: KHCONC(NMNEEE, NMNTEE), KHDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: KLCONC(NMNEEE, NMNTEE), KLDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: KMCONC(NMNEEE, NMNTEE), KMDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: KNCONC(NMNEEE, NMNTEE), KNDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: KVCONC(NMNEEE, NMNTEE), KVDPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: NACONC(NMNEEE, NMNTEE), NADPTH(NMNEEE, NMNTEE)
      DOUBLE PRECISION, INTENT(OUT) :: NAMTOP(NLF+1:NEL)

      LOGICAL, INTENT(OUT) :: ISICCD, ISIAMD, ISQ10

      ! Workspace arguments (INTENT(INOUT) because they act as read buffers)
      INTEGER, INTENT(INOUT)          :: IDUM(NELEE)
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)

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
      ! Modern Fix: Wrapped scalar in size-1 array to satisfy ALREDI
      CALL ALREDI(0, MND, MNPR, ':MN60', 1, 1, IDUMS)
      NBOTCE = IDUMS(1)

   ! epilogue
   ! --------
      CALL ALRED2(1, MND, MNPR, 'MND')

   END SUBROUTINE MNRED1



   !SSSSSS SUBROUTINE MNRED2
   SUBROUTINE MNRED2(MNFC, MNFN, MNPR, NEL, NELEE, NLF, NLFEE, NX, NXEE, NY, ICMBK, ICMREF, ICMXY, DTUZ, TIH, UZNOW, BEXBK, LINKNS, &
                     CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, ISADDC, ISADDN, IDUM, DUMMY)
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
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      ! Modernization Fix: Restored required modules for interfaces and time calculation
      USE UTILSMOD, ONLY : hour_from_date
      USE SGLOBAL, ONLY : nyee

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: MNFC, MNFN, MNPR, NEL, NELEE, NLF, NLFEE, NX, NXEE, NY
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2), ICMREF(NELEE, 4, 2:2), ICMXY(NXEE, NY)
      DOUBLE PRECISION, INTENT(IN) :: DTUZ, TIH, UZNOW
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS(NLFEE)

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CDPTHB(NLF + 1:NEL), CLTFCT(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(OUT) :: CMNFCT(NLF + 1:NEL), CNRAL(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(OUT) :: CNRAM(NLF + 1:NEL), CTOT(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(OUT) :: NAMFCT(NLF + 1:NEL), NDPTHB(NLF + 1:NEL)
      DOUBLE PRECISION, INTENT(OUT) :: NTOT(NLF + 1:NEL)
      LOGICAL, INTENT(OUT) :: ISADDC, ISADDN

      ! Workspace arguments (INTENT(INOUT) because they act as read buffers)
      INTEGER, INTENT(INOUT)          :: IDUM(NELEE)
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)

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


   !SSSSSS SUBROUTINE MNTEMP
   SUBROUTINE MNTEMP(LLEE, NCETOP, NEL, NELEE, NLF, NV, NCOLMB, Z2, DELTAZ, ZVSNOD, DTUZ, TA)
   !--------------------------------------------------------------------*
   !
   ! calculates the temperature in every cell
   !
   !--------------------------------------------------------------------*
   ! version:                 notes:
   ! module: mn               program: shetran
   ! modifications
   !--------------------------------------------------------------------*

      USE UTILSMOD, ONLY: TRIDAG

      ! Assumed external module dependencies providing global variables:
      ! USE MN_MODULE, ONLY: TEMP

      IMPLICIT NONE

      ! * input arguments
      ! * static
      INTEGER, INTENT(IN) :: LLEE, NCETOP, NEL, NELEE, NLF, NV
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)
      DOUBLE PRECISION, INTENT(IN) :: Z2
      DOUBLE PRECISION, INTENT(IN) :: DELTAZ(LLEE, NEL), ZVSNOD(LLEE, NEL)

      ! * varying
      DOUBLE PRECISION, INTENT(IN) :: DTUZ, TA(NV)

      ! locals etc
      INTEGER :: IEL, NCE, NCEBOT, NCELLS, NNUM, NSERCH
      INTEGER, PARAMETER :: NUM = 11

      DOUBLE PRECISION :: CELLDP, CELLFC, KFCT, GRDTEM
      DOUBLE PRECISION :: AMAT(NUM), BMAT(NUM), CMAT(NUM), DEPTH(NUM)
      DOUBLE PRECISION :: RHS(NUM), OME(NUM), TEMPR1(NUM)

      DOUBLE PRECISION, PARAMETER :: DEPTHC = 10.0D0
      DOUBLE PRECISION, PARAMETER :: DIFF = 2.0D-5
      DOUBLE PRECISION, PARAMETER :: DIFFGA = 2.0D0

      ! Saved state initialization replacing legacy DATA statement
      ! Modern Fix: Scalar broadcast initialization is safer than array constructor looping
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
      ! Modern Fix: Vectorized array assignment replaces the DO loop
      TEMPR(1:NUM) = TEMPR1(1:NUM)

   END SUBROUTINE MNTEMP

END MODULE MNmod
