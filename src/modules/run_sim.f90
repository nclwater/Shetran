MODULE run_sim

! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       This is the comutational core - it runs the simulation, timestep by timestep
!                       Code was extracted from shetrn.f and modifed to create this module
USE SGLOBAL
USE SED_CS,   ONLY : nsed,pbsed,pls,sosdfn,arbdep,dls,fbeta,fdel,&
                     ginfd,ginfs,gnu,gnubk,qsed,dcbed,dcbsed 
!USE SGLOBAL, ONLY : nxee, nyee, nlfee, nvee, nelee, &
!                 llee, NVSEE, NLYREE, NOCTAB, NXSCEE !NEEDED ONLY FOR AD
USE AL_G, ONLY : nx, ny, icmref,icmxy,ngdbgn
USE AL_C, ONLY : uznext, pnetto, arxl, dtuz, eevap, icmbk, &
                 nvswlt, qvswel, tih, ns, nv, sfb, spr, srb, syd, icmrf2, nbface, &
                 nlyr,ntsoil,nvc,clenth,cwidth, &
                 dhf,vspor, zbfull,bexbk,linkns,isort,clai,draina,plai,qoc,idum,dummy, cmp
                 
USE AL_D, ONLY : eswa, nstep, ocnext, epot, nmc, obspe, &
                 ocnow, bexsy, bexcm, precip_m_per_s, &
                 mbflag, bhotpr, hotime, hot, cstore, dq0st,&
                 dqist, dqist2, sd,ts, nsmc, bhotst, tim, tth, bhotrd, tmax
USE FRmod,  ONLY : tsh, tch, bstore, btime
USE VSmod,    ONLY : VSSIM, &
                     RLFTIM, icsoilsv !THESE NEEDED ONLY FOR AD
USE CMmod,    ONLY : CMSIM  !"JE"
USE ETmod,    ONLY : ETSIM, &
                     psi4, uzalfa !TH,ESE NEEDED ONLY FOR AD
USE rest,     ONLY : BALWAT, TMSTEP, &
                     metime, melast, eptime, pinp
                     !start_impact_window, end_impact_window, per_rain, mx_cnt_rain, cnt_rain !these here only for AD
USE FRmod,    ONLY : INCM, FRINIT
USE OCmod,    ONLY : OCSIM
USE OCQDQMOD, ONLY : STRXX, STRYY       
USE OCmod2,   ONLY : GETHRF, &
                     HRFZZ !HRFZZ NEEDED ONLY FOR AD
USE FRmod,    ONLY : FRSORT, FROUTPUT, FRMB, FRRESP, DATE_FROM_HOUR
USE SYmod,    ONLY : SYMAIN, BALSED  !"JE"
USE VISUALISATION_INTERFACE_RIGHT, ONLY : RECORD_VISUALISATION_DATA         !VISVISVIS
USE VISUALISATION_INTERFACE_LEFT,  ONLY : GET_NSED_EARLY, GET_NCON_EARLY    !VISVISVIS
!NEEDED ONLY FOR AD                 
USE AL_C,       ONLY : eruz                 
USE AL_D,       ONLY : mblink, mbface, ae, s, erz, esoil, eint, pnet, timeuz, drain, sf, pe, u, vht, rn, vpd, ta
USE colm_c1,    ONLY : z2sq   !"JE"
USE ocmod,      ONLY : qfnext, hoclst, hocprv, qocfin, hocnxt, hocnxv
USE OCQDQMOD,   ONLY : hocnow, qocf, xafull !, firstocqdq
USE OCmod2,     ONLY : hrfzz, qsazz !NEEDED ONLY FOR AD
USE vsmod,      ONLY : rlfdum, rlgnxt, firstvssim, rbhlst, rlhlst, vsaijsv, jcbcsv, rbhprv, rlglst, rlhprv, rbfprv, &
                       rlgprv, rlfprv, rwelin, rbhtim, wltime, rlhdum, rbhnxt, rlhtim, rlgdum, rlhnxt, rbftim, rlgtim, &
                       wlnow, vskr, rlfnow, rbfnow, ivssto, rlhnow, rbhnow
USE SMmod,    ONLY : rhos, smelt, tmelt
USE al_c,       ONLY : qh, qvswli, vsthe, vspsi, qvsh, qvsv, qbkb, qbkf, esoila, eruz
USE ETmod,    ONLY : rc, ra, cstcap, del, &
                     nctcst, nctvht, nctcla, nctpla !these here only for AD 
USE SYmod,      ONLY : issyok_symain  !"JE"
USE FRmod,      ONLY : qoctot, uzold, &
                       next_hour, icounter2  !these here only for AD 
USE CONT_CC,    ONLY: initialise_cont_cc
USE COLM_CG,    ONLY: initialise_colm_cg,deallocate_colm_cg
USE COLM_CO,    ONLY: initialise_colm_co
!USE PERTURBATIONS, ONLY : LOAD_PERTURBATIONS, spatial1
IMPLICIT NONE

PRIVATE
PUBLIC :: simulation

CONTAINS


!SSSSSS SUBROUTINE simulation
!----------------------------------------------------------------------*
! SUBROUTINE SIMULATION
! Description: Runs the main simulation loop set up in the SHETRAN program.
! It initializes the required data, outputs starting information, and then
! loops over time steps, sequentially calling the Evapotranspiration (ET),
! Variably Saturated Subsurface (VSS), Overland Channel (OC), and optional
! Sediment Yield (SY) and Contaminant Transport (CM) components.
!----------------------------------------------------------------------*
SUBROUTINE SIMULATION
   USE, INTRINSIC :: iso_fortran_env, ONLY: OUTPUT_UNIT

   ! Locals
   INTEGER :: ptub, j, iel, k
   REAL(KIND=4), PARAMETER :: rzero = 0.0e0
   INTEGER, PARAMETER :: niosto = 50
   LOGICAL :: bsy, bcm, cmfrst, syfrst
   CHARACTER(LEN=niosto) :: aiosto
   DOUBLE PRECISION, DIMENSION(nelee) :: hrf
   INTEGER, SAVE :: icounter3 = 0
   INTEGER :: c(6)
   CHARACTER(LEN=128) :: dum

   !-----------------------------------------------------------------
   !                     INITIALISATION
   !-----------------------------------------------------------------

   ! Open standard output (Unit 6 is conventionally OUTPUT_UNIT)
   OPEN(UNIT=OUTPUT_UNIT, FORM='formatted')

   syfrst = .TRUE.
   cmfrst = .TRUE.

   CALL FRINIT
   CALL RECORD_VISUALISATION_DATA(rzero) ! Visualisation data record
   CALL FRSORT  
   IF (.NOT. BHOTRD) UZNEXT = TMAX  
   CALL FROUTPUT('start')  ! Initialization output (sb 08/03/06)

   ! Format and print the simulation start date
   c = DATE_FROM_HOUR(tih)
   WRITE(dum, '(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') &
      c(1), '-', c(2), '-', c(3), 'T', c(4), ':', c(5), ':', c(6)
   WRITE(OUTPUT_UNIT, '(A,A)') ' Simulation Start Date = ', TRIM(dum)
   
   ! Format and print the simulation end date
   c = DATE_FROM_HOUR(tth)
   WRITE(dum, '(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') &
      c(1), '-', c(2), '-', c(3), 'T', c(4), ':', c(5), ':', c(6)
   WRITE(OUTPUT_UNIT, '(A,A)') ' Simulation End Date   = ', TRIM(dum)

   WRITE(OUTPUT_UNIT, *) 
   WRITE(OUTPUT_UNIT, 9750) TTH - TIH

   !------------------------------------------------------------------
   !                    MAIN SIMULATION LOOP
   !------------------------------------------------------------------
   IF (bexsy) CALL GET_NSED_EARLY()  ! Visualisation data
   IF (bexcm) THEN
      CALL GET_NCON_EARLY()          ! Visualisation data
      CALL initialise_cont_cc()      ! Dynamically allocate contaminant transport arrays
      CALL initialise_colm_cg()      ! Dynamically allocate face overlap and lateral transmissivity values
      CALL initialise_colm_co()      ! Dynamically allocate water variables for SUBROUTINE COLM                     
   END IF
   
   CALL RECORD_VISUALISATION_DATA(rzero) ! Visualisation data record

   ! Main loop iterates until UZNOW reaches the total simulation time (TTH - TIH)
   DO 
      CALL TMSTEP   ! Set timestep
      
      NSTEP = NSTEP + 1  
      OCNEXT = UZNEXT  
      
      !-----------------------------------
      !         ET COMPONENT
      !-----------------------------------
      CALL ETSIM  
      
      !-----------------------------------
      !         VSS COMPONENT
      !-----------------------------------
      CALL VSSIM  
      UZNOW = UZNOW + UZNEXT  
      
      ! Post-processing
      ! Calculate rainfall into the channel, including any conjunctive use
      ! transfer of water from wells
      DO IEL = 1, total_no_links  
         EPOT(IEL) = OBSPE(NMC(IEL)) / 1000.0d0  
         PNETTO(IEL) = precip_m_per_s(iel)  
         
         ESWA(IEL) = MIN(EPOT(IEL), ARXL(IEL) / (cellarea(IEL) * DTUZ))
         EEVAP(IEL) = ESWA(IEL)  
         
         IF (NVSWLT(IEL) /= 0) THEN
            PNETTO(IEL) = PNETTO(IEL) + QVSWEL(NVSWLT(IEL)) * cellarea(NVSWLT(IEL)) / cellarea(IEL)
         END IF
      END DO  
      
      !-----------------------------------
      !         OC COMPONENT
      !-----------------------------------
      CALL OCSIM  
      OCNOW = UZNOW  
      
      !-----------------------------------
      !         SY/CM COMPONENTS
      !-----------------------------------
      bsy = bexsy .AND. (UZNOW >= (TSH - TIH))  
      bcm = bexcm .AND. (UZNOW >= (TCH - TIH))  
      
      ! Call sort routine to re-evaluate elevations
      CALL FRSORT  
      
      ! Call sediment and contaminant routines, if requested
      IF (bsy) THEN  
         DO iel = 1, total_no_elements
            hrf(iel) = gethrf(iel)
         END DO
         
         CALL SYMAIN(total_no_elements, total_no_links, NS, NV, NX, NY, SFB, SPR, SRB, SYD, ICMBK, &
                     ICMREF(1, 5), ICMRF2, ICMXY, NBFACE, NLYR(total_no_links + 1), NTSOIL, &
                     NVC(total_no_links + 1), cellarea, CLENTH, CWIDTH, DHF, DXQQ(total_no_links + 1), &
                     DYQQ(total_no_links + 1), VSPOR, ZBFULL, ZGRUND, BEXBK, LINKNS, ISORT, DTUZ, &
                     TIH, UZNOW, ARXL, CLAI, DRAINA(total_no_links + 1), hrf, PLAI, &
                     PNETTO(total_no_links + 1), QOC, NSED, PBSED, PLS(total_no_links + 1), SOSDFN, &
                     ARBDEP, DLS, FBETA, FDEL, GINFD, GINFS, GNU(total_no_links + 1), GNUBK, QSED, &
                     DCBED, DCBSED, IDUM, DUMMY)
                     
         ! Legacy commented-out call preserved
         ! CALL SYMAIN (NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD, ICMBK, ICMREF (1, 5), &
         ! ICMRF2, ICMXY, NBFACE, NLYR (NLF + 1), NTSOIL, NVC (NLF + 1), AREA, CLENTH, CWIDTH, &
         ! DHF, DXQQ (NLF + 1), DYQQ (NLF + 1), VSPOR, ZBFULL, ZGRUND, BEXBK, LINKNS, ISORT, &
         ! DTUZ, TIH, UZNOW, ARXL, CLAI, DRAINA (NLF + 1), HRF, PLAI, PNETTO (NLF + 1), QOC, &
         ! NSED, PBSED, PLS (NLF + 1),SOSDFN, ARBDEP, DLS, FBETA, FDEL, GINFD, GINFS, GNU (NLF + 1), &
         ! GNUBK, QSED, DCBED, DCBSED, IDUM, DUMMY)
      END IF  
      
      IF (bcm) THEN  
         IF (bexsy .AND. (.NOT. bsy)) THEN
            CALL ERROR(FFFATAL, 1041, CMP, 0, 0, 'Start-time for sediment is later than for contaminants')
         END IF
         IF (cmfrst) THEN  
            CALL INCM(bexsy)  
            cmfrst = .FALSE.  
            aiosto = '00000000000000000000000000000001111111111'  
            IF (BSTORE) CALL FRRESP(aiosto, ZERO, .FALSE.)  
            CALL deallocate_colm_cg()
         ELSE  
            CALL CMSIM(bexsy)  
         END IF  
      END IF  
      
      !-----------------------------------
      !         RESULTS OUTPUT
      !-----------------------------------
      ! Mass balance errors
      CALL BALWAT  
      
      ! Make mass balance output called daily (sb 8/3/06)
      mbflag = 1  
      CALL FRMB  
      IF (bsy) CALL BALSED  
      
      ! Unformatted 'RES' file output
      aiosto = '1111111111111111111111111111111111111111111111111'  
      
      ! dsat specific - for contaminant averaging
      IF (BSTORE) CALL FRRESP(aiosto, UZNOW, .FALSE.)  
      
      ! Hotstart output
      IF (BHOTPR) THEN  
         IF (UZNOW >= HOTIME) THEN  
            ! uznow=current time (hours)
            ! uznext-= next time(hours)
            ! cstore = canopy storage (mm)
            ! gethrf = surface water elevation(m)
            ! QSAzz = overland flow?
            ! QOC = overland flow
            ! DQ0ST = flow derivatives
            ! DQIST = flow derivatives
            ! DQIST2 = flow derivatives
            ! SD = snow pack depth
            ! TS = snow temperature
            ! NSMC = COUNTER USED IN ROUTING MELTWATER THROUGH SNOWPACK
            ! SMELT = water in meltwater slug?
            ! TMELT = temperature of eltwater slug?
            ! vspsi = soil water potentials
            WRITE(HOT, *) "time= ", UZNOW, UZNEXT, top_cell_no, &
                          "cstore= ", (CSTORE(IEL), IEL = NGDBGN, total_no_elements), &
                          "HRF= ", (getHRF(IEL), IEL = 1, total_no_elements), &
                          "QSA= ", ((QSAzz(IEL, K), IEL = 1, total_no_elements), K = 1, 4), &
                          "QOC= ", ((QOC(IEL, K), IEL = 1, total_no_elements), K = 1, 4), &
                          "DQ0ST= ", ((DQ0ST(IEL, K), IEL = 1, total_no_elements), K = 1, 4), &
                          "DQIST= ", ((DQIST(IEL, K), IEL = 1, total_no_elements), K = 1, 4), &
                          "DQIST2= ", ((DQIST2(IEL, K), IEL = 1, NGDBGN - 1), K = 1, 3), &
                          "SD= ", (SD(IEL), IEL = NGDBGN, total_no_elements), &
                          "TS= ", (TS(IEL), IEL = NGDBGN, total_no_elements), &
                          "NSMC= ", (NSMC(IEL), IEL = NGDBGN, total_no_elements), &
                          "SMELT= ", ((SMELT(K, IEL), K = 1, NSMC(IEL)), IEL = NGDBGN, total_no_elements), &
                          "TMELT= ", ((TMelt(K, IEL), K = 1, NSMC(IEL)), IEL = NGDBGN, total_no_elements), &
                          "vspsi= ", ((VSPSI(j, iel), j = 1, top_cell_no), IEL = 1, total_no_elements)
            HOTIME = HOTIME + BHOTST  
         END IF  
      END IF  
      
      ! Time-counter file
      IF (BTIME) THEN  
         REWIND(TIM)  
         WRITE(TIM, 9800) UZNOW, NSTEP  
      END IF  
      
      CALL RECORD_VISUALISATION_DATA(REAL(uznow, KIND=4))  
      CALL FROUTPUT('main ')  
      
      IF (uznow > icounter3) THEN  
         ! Replaces the old '+' carriage control by writing ACHAR(13) to return to start of line, 
         ! and ADVANCE='NO' to prevent jumping down a line.
         WRITE(OUTPUT_UNIT, 9751, ADVANCE='NO') ACHAR(13), uznow, MIN(100.0d0 * uznow / (TTH - TIH), 100.0d0)
         FLUSH(OUTPUT_UNIT)
         icounter3 = icounter3 + 24  
      END IF  
      
      IF (UZNOW >= (TTH - TIH)) EXIT
   END DO

   WRITE(OUTPUT_UNIT, *) ! Advance line cleanly when simulation completes

9750 FORMAT(' Length of Simulation =', F12.2, ' hours '//)  
9751 FORMAT(A, 'Simulation Timestep =', F12.2, ' hours   % Completed = ', F6.2)  
9800 FORMAT('Current time = ', F10.2, ' hours. Number of steps = ', I7 /)  
9900 FORMAT('Normal completion of SHETRAN run: ', F10.2, ' hours, ', I7, ' steps.' /)

END SUBROUTINE SIMULATION

END MODULE run_sim