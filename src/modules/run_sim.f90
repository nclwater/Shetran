!> summary: Main SHETRAN simulation time-step driver.
!> author: JE, Newcastle University; Stephen Birkinshaw, Newcastle University
!>
!> This module contains the central simulation driver for SHETRAN. It
!> initializes the framework state, records initial visualisation output, then
!> advances the coupled hydrological, sediment, contaminant, snow, vegetation,
!> and output components through the model time window.
!>
!> The driver coordinates the component sequence rather than implementing a
!> single numerical method itself. Its main responsibilities are to select the
!> current time step, call the evapotranspiration and variably saturated
!> subsurface components, update simulation time, route overland/channel flow,
!> conditionally run sediment and contaminant components, maintain water and
!> sediment balances, write hotstart/state output, and report progress.
!>
!> The main loop uses this high-level order:
!>
!> | Stage | Main calls/state updates |
!> |:------|:-------------------------|
!> | Timestep selection | [[rest:tmstep]], increment `NSTEP`, copy `UZNEXT` to `OCNEXT`. |
!> | Land hydrology | [[etmod:etsim]], then [[vsmod:vssim]]. |
!> | Time advance | `UZNOW = UZNOW + UZNEXT`; channel rainfall, evaporation, and well-transfer terms are updated for links. |
!> | Surface routing | [[ocmod:ocsim]], then `OCNOW = UZNOW`. |
!> | Optional sediment | [[symod:symain]] when `BEXSY` and `UZNOW >= TSH-TIH`. |
!> | Optional contaminants | [[frmod:incm]] on the first active contaminant step, then [[cmmod:cmsim]] on later active steps. |
!> | Output and balances | [[rest:balwat]], [[frmod:frmb]], optional [[symod:balsed]], result/hotstart/time-counter output, visualisation, and [[frmod:froutput]]. |
!>
!> @note Contaminant setup is intentionally split: contaminant and column helper
!> arrays are allocated before the loop when `BEXCM` is true, but
!> [[frmod:incm]] is called on the first active contaminant timestep and
!> `CMSIM` is called only on subsequent active timesteps.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2008-12 | JE | 4.3.5F90 | Created during the Fortran 90 conversion by extracting the computational core from `shetrn.f`. |
!> | 2026-03 | SB | 4.6 | Added `DATE_FROM_HOUR` reporting and calls for contaminant/column allocation setup and cleanup. |
!> @endhistory
!>
!> @note The module has a large dependency surface because it orchestrates most
!> SHETRAN process modules and shared state arrays. Changes here should be
!> checked against component ordering, mass-balance output, hotstart output, and
!> visualisation side effects.
!> @endnote
!>
MODULE run_sim

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
      psi4, uzalfa !THESE NEEDED ONLY FOR AD
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


!> Runs the SHETRAN simulation from the configured start time to end time.
!>
!> `SIMULATION` is the top-level time-stepping routine called after the model
!> has been configured. It initializes framework and output state, enters the
!> main time loop, asks `TMSTEP` for the next time step, calls the process
!> modules in the required order, writes daily and event-driven output, and
!> exits when `UZNOW` reaches `TTH - TIH`.
!>
!> The routine has no dummy arguments. It operates through module variables
!> imported from `SGLOBAL`, `AL_C`, `AL_D`, `FRmod`, `ETmod`, `VSmod`, `OCmod`,
!> `SYmod`, `CMmod`, `rest`, the visualisation interfaces, and supporting
!> parameter modules.
!>
!> Main loop sequence:
!>
!> | Step | Action |
!> |:-----|:-------|
!> | Select timestep | `TMSTEP` sets `UZNEXT`; `NSTEP` increments and `OCNEXT=UZNEXT`. |
!> | Land hydrology | `ETSIM` runs first, then `VSSIM`; only after both does `UZNOW` advance. |
!> | Link forcing | Channel/link `EPOT`, `PNETTO`, `ESWA`, `EEVAP`, and well additions are updated. |
!> | Surface routing | `OCSIM` advances overland/channel hydraulics, then `OCNOW=UZNOW`. |
!> | Sediment and contaminants | `FRSORT` refreshes ordering; `SYMAIN` runs when sediment is active; `INCM` runs only on the first active contaminant step, with `CMSIM` on later active steps. |
!> | Output | Water balance, monthly balance, optional sediment balance, result output, hot-start output, visualisation, progress, and `FROUTPUT('main ')` are written. |
!>
!> Hot-start output fields:
!>
!> | Field | Meaning |
!> |:------|:--------|
!> | `time` | Current time `UZNOW`, next step `UZNEXT`, and active top-cell number. |
!> | `cstore` | Canopy storage for land/bank elements. |
!> | `HRF` | Surface-water elevation from `getHRF`. |
!> | `QSA` | Overland face flow from `QSAZZ`. |
!> | `QOC` | Overland/channel face flow. |
!> | `DQ0ST`, `DQIST`, `DQIST2` | Stored flow derivatives. |
!> | `SD`, `TS` | Snowpack depth and snow temperature. |
!> | `NSMC`, `SMELT`, `TMELT` | Snowmelt routing slug count, water amount, and release time. |
!> | `VSPSI` | Variably saturated pressure-head profile. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2008-12 | JE | 4.3.5F90 | Created as the timestep-by-timestep computational core. |
!> | 2026-03 | SB | 4.6 | Added human-readable simulation dates and contaminant allocation setup/cleanup calls. |
!> @endhistory
!>
!> @note Component ordering is hydrologically significant: ET and VSS are run
!> before the model time is advanced; overland/channel flow is run after
!> rainfall, evaporation, and well transfers are updated; sediment and
!> contaminant calls are gated by their configured start times.
!> @endnote
!>
   SUBROUTINE SIMULATION
      INTEGER                                       :: ptub, j
      REAL(4), PARAMETER                            :: rzero = 0.0e0
      INTEGER, PARAMETER                            :: niosto = 50
      INTEGER                                       :: iel,k
      LOGICAL                                       :: bsy, bcm, cmfrst,syfrst
      CHARACTER(NIOSTO)                             :: aiosto
      DOUBLEPRECISION, DIMENSION(nelee)             :: hrf
      INTEGER, SAVE                                 :: icounter3 = 0
      INTEGER  :: c(6)
      CHARACTER(128)    :: dum
      real :: start_time, current_time, elapsed_time


!-----------------------------------------------------------------
!                       INITIALISATION
!-----------------------------------------------------------------

      open(unit=6,form='formatted',carriagecontrol='fortran')

      SYFRST = .TRUE.
      cmfrst = .TRUE.

      CALL FRINIT
      CALL RECORD_VISUALISATION_DATA (rzero)!VISVISVIS
      CALL FRSORT
      IF (.NOT.BHOTRD) UZNEXT = TMAX
      CALL FROUTPUT ('start')  !^^^^^^ sb 08/03/06

      c = DATE_FROM_HOUR(tih)
      WRITE(dum,'(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') c(1),'-',c(2),'-',c(3),' ', c(4),':',c(5),':',c(6)
      write(6,'(A,A)') ' Simulation Start Date = ',trim(dum)
      c = DATE_FROM_HOUR(tth)
      WRITE(dum,'(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') c(1),'-',c(2),'-',c(3),' ', c(4),':',c(5),':',c(6)
      write(6,'(A,A)') ' Simulation End Date = ',trim(dum)

      write(6,*)
      write(6,9750) TTH - TIH

      write(6,'(A)') ' SHETRAN file folder = '
      write(6,'(1X,A)') DIRQQ
      write(6,'(A)') ' SHETRAN rundata name = '
      write(6,'(A)') ' rundata_'//trim(cnam)//'.txt'
      write(6,*)
      write(6,*)
      write(6,*)

      call cpu_time(start_time)


!------------------------------------------------------------------
!                     MAIN SIMULATION LOOP
!------------------------------------------------------------------
      IF (bexsy) CALL GET_NSED_EARLY ()     !VISVISVIS
      IF (bexcm) then
         CALL GET_NCON_EARLY ()     !VISVISVIS
         call initialise_cont_cc()  !dynamically allocate contaminnant tranport arrays
         call initialise_colm_cg()  ! dynamically allocate FACE OVERLAP AND LATERALTRANSMISIVITY VALUES
         call initialise_colm_co()  ! dynamically allocate WATER VARIABLES USED IN  THE PREPARATION FOR RUNNING SUBROUTINE COLM
      endif
      CALL RECORD_VISUALISATION_DATA (rzero)!VISVISVIS

      DO
         CALL TMSTEP   !set timestep
         !print'(F14.2)', uznow
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
         ! post-processing
         ! CALCULATE RAINFALL INTO THE CHANNEL, INCLUDING ANY CONJUNCTIVE USE
         ! TRANSFER OF WATER FROM WELLS
         DO IEL = 1, total_no_links
            EPOT (IEL) = OBSPE (NMC (IEL) ) / 1000.
            !PNETTO (IEL) = precip_m_per_s(NMC (IEL) )
            PNETTO (IEL) = precip_m_per_s(iel)
            !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            ESWA (IEL) = MIN (EPOT (IEL), ARXL (IEL) / (cellarea (IEL) * DTUZ))
            EEVAP (IEL) = ESWA (IEL)
            !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            IF (NVSWLT (IEL) .NE.0) PNETTO (IEL) = PNETTO (IEL) + QVSWEL ( &
               NVSWLT (IEL) ) * cellarea (NVSWLT (IEL) ) / cellarea (IEL)
         ENDDO
         !-----------------------------------
         !         OC COMPONENT
         !-----------------------------------
         CALL OCSIM
         OCNOW = UZNOW
         !-----------------------------------
         !         SY/CM COMPONENTS
         !-----------------------------------
         BSY = BEXSY.AND.UZNOW.GE. (TSH - TIH)
         BCM = BEXCM.AND.UZNOW.GE. (TCH - TIH)
         ! Call sort routine, if required
         !970616      IF ( BSY .OR. BCM ) CALL FRSORT
         CALL FRSORT
         !^^^^^^
         ! CALL SEDIMENT AND CONTAMINANT ROUTINES, IF REQUESTED
         IF (BSY) THEN
            do iel = 1,total_no_elements
               hrf(iel) = gethrf(iel)
            enddo
            CALL SYMAIN (total_no_elements, total_no_links, NS, NV, NX, NY, SFB, SPR, SRB, SYD, ICMBK, ICMREF (1, 5), &
               ICMRF2, ICMXY, NBFACE, NLYR (total_no_links + 1), NTSOIL, NVC (total_no_links + 1), cellarea, CLENTH, CWIDTH, &
               DHF, DXQQ (total_no_links + 1), DYQQ (total_no_links + 1), VSPOR, ZBFULL, ZGRUND, BEXBK, LINKNS, ISORT, &
               DTUZ, TIH, UZNOW, ARXL, CLAI, DRAINA (total_no_links + 1), HRF, PLAI, PNETTO (total_no_links + 1), QOC, &
               NSED, PBSED, PLS (total_no_links + 1),SOSDFN, ARBDEP, DLS, FBETA, FDEL, GINFD, GINFS, GNU (total_no_links + 1), &
               GNUBK, QSED, DCBED, DCBSED, IDUM, DUMMY)
!        CALL SYMAIN (NEL, NLF, NS, NV, NX, NY, SFB, SPR, SRB, SYD, ICMBK, ICMREF (1, 5), &
!        ICMRF2, ICMXY, NBFACE, NLYR (NLF + 1), NTSOIL, NVC (NLF + 1), AREA, CLENTH, CWIDTH, &
!        DHF, DXQQ (NLF + 1), DYQQ (NLF + 1), VSPOR, ZBFULL, ZGRUND, BEXBK, LINKNS, ISORT, &
!        DTUZ, TIH, UZNOW, ARXL, CLAI, DRAINA (NLF + 1), HRF, PLAI, PNETTO (NLF + 1), QOC, &
!        NSED, PBSED, PLS (NLF + 1),SOSDFN, ARBDEP, DLS, FBETA, FDEL, GINFD, GINFS, GNU (NLF + 1), &
!        GNUBK, QSED, DCBED, DCBSED, IDUM, DUMMY)
         ENDIF
         IF (BCM) THEN
            IF (BEXSY.AND. (.NOT.BSY) ) CALL ERROR(FFFATAL, 1041, CMP, 0, 0, &
               'Start-time for sediment is later than for contaminants')
            IF (CMFRST) THEN
               CALL INCM (BEXSY)
               CMFRST = .FALSE.
               AIOSTO = '00000000000000000000000000000001111111111'
               IF (BSTORE) CALL FRRESP (AIOSTO, ZERO, .FALSE.)
               call deallocate_colm_cg()
            ELSE
               CALL CMSIM (BEXSY)
            ENDIF
         ENDIF
         !-----------------------------------
         !         RESULTS OUTPUT
         !-----------------------------------
         ! mass balance errors
         CALL BALWAT
         ! sb 8/3/06 make mass balance output called daily
         mbflag = 1
         CALL FRMB
         IF (BSY) CALL BALSED    !"JE"
         ! unformatted 'RES' file output
         ! !testcc temporary code to NOT output data type 46 here
         ! sb 990128 incorporate sediment output
         !      AIOSTO = '1111111111111111111111111111111111111111111111111'
         !      AIOSTO = '1111111111111111111100000000000111111111111111111'
         !      AIOSTO = '1111111111111111111100000000000111111111111011111'
         ! sb 990128
         AIOSTO = '1111111111111111111111111111111111111111111111111'
         ! !testcc end of temporary code
         ! dsat specific - for contaminant averaging
         IF (BSTORE) CALL FRRESP (AIOSTO, UZNOW, .FALSE.)
         ! hotstart output
         IF (BHOTPR) THEN
            IF (UZNOW.GE.HOTIME) THEN
               WRITE (HOT,*) "time= ",UZNOW, UZNEXT, top_cell_no,"cstore= ", (CSTORE (IEL), IEL = NGDBGN, &
                  total_no_elements),"HRF= ", (getHRF (IEL), IEL = 1, total_no_elements),"QSA= ", ( (QSAzz (IEL, K), IEL = 1, &
                  total_no_elements), K = 1, 4),"QOC= ", ( (QOC (IEL, K), IEL = 1, total_no_elements), K = 1, 4), &
                  "DQ0ST= ",( (DQ0ST (IEL, K), IEL = 1, total_no_elements), K = 1, 4),"DQIST= ", ( (DQIST (IEL, &
                  K), IEL = 1, total_no_elements), K = 1, 4),"DQIST2= ", ( (DQIST2 (IEL, K), IEL = 1, &
                  NGDBGN - 1), K = 1, 3),"SD= ", (SD (IEL), IEL = NGDBGN, total_no_elements), &
                  "TS= ", (TS (IEL), IEL = NGDBGN, total_no_elements),"NSMC= ", (NSMC (IEL), IEL = NGDBGN, &
                  total_no_elements),"SMELT= ", ( (SMELT (K, IEL), K = 1, NSMC (IEL) ), IEL = NGDBGN, &
                  total_no_elements),"TMELT= ", ( (TMelt (K, IEL), K = 1, NSMC (IEL) ), IEL = NGDBGN, &
                  total_no_elements),"vspsi= ", ( (VSPSI (j, iel), j = 1, top_cell_no), IEL = 1, total_no_elements)
               HOTIME = HOTIME+BHOTST
            ENDIF
         ENDIF
         ! time-couter file
         IF (BTIME) THEN
            REWIND (TIM)
            WRITE (TIM, 9800) UZNOW, NSTEP
         ENDIF
         CALL RECORD_VISUALISATION_DATA (REAL(uznow, KIND=4))  !VISVISVIS
         CALL FROUTPUT('main ')  !sb 02/05/07 additional output
         IF(uznow > icounter3) then
            call cpu_time(current_time)
            write(6,9752) uznow, min(100*uznow/(TTH - TIH),100.00),int(current_time - start_time), int((current_time - start_time)/(uznow/(TTH - TIH))-(current_time - start_time))

            ! This code should work but it can produce garbage output so I reverted to using the '+' in the format statement
            !write(6,'(A)',advance='no') achar(13)
            !write(6,9751,advance='no') uznow, min(100*uznow/(TTH - TIH),100.00),int(current_time - start_time), int((current_time - start_time)/(uznow/(TTH - TIH))-(current_time - start_time))
            !call flush(6)
            icounter3 = icounter3 + 24
         endif
         IF (UZNOW>=(TTH - TIH) ) EXIT
      ENDDO

! this line is to clear the progress line after the simulation has finished
      WRITE (6,'(A)') '                                                                                                                                '


9750  FORMAT (' Length of Simulation =',F12.2,' hours '//)
!9751 FORMAT ('Simulation = ',F0.1,' hrs, % Compl. = ', f0.2,', Elapsed/Remaining = ', I0, ' / ', I0, ' sec. ')
9752  FORMAT ('+','Simulation = ',F0.1,' hrs, % Compl. = ', f0.2,', Elapsed/Remaining = ', I0, ' / ', I0, ' sec. ')
9800  FORMAT ('Current time = ',F10.2,' hours. Number of steps = ',I7 /)
9900  FORMAT ('Normal completion of SHETRAN run: ',F10.2, ' hours, ', I7,' steps.' /)
   END SUBROUTINE simulation
END MODULE run_sim
