!> @brief Main SHETRAN simulation time-step driver.
!>
!> `run_sim` contains [[simulation]], the top-level loop that advances the
!> model from the configured start time to the end time. It coordinates the
!> process modules rather than implementing a numerical method itself:
!> selecting the current time step, running the land-hydrology and
!> surface-routing components, conditionally running the sediment and
!> contaminant components, maintaining water/sediment mass-balance
!> diagnostics, and writing hotstart/result/progress output.
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
!> @note Contaminant setup is intentionally split: contaminant and column
!> helper arrays are allocated before the loop when `BEXCM` is true, but
!> [[frmod:incm]] is called on the first active contaminant timestep and
!> `CMSIM` is called only on subsequent active timesteps.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1999-01-28 | SB | - | Incorporated sediment output into the `AIOSTO` result-type selection. |
!> | 2006-03-08 | SB | - | Made mass-balance output (`FRMB`) a daily call. |
!> | 2007-05-02 | SB | - | Added an additional `FROUTPUT('main ')` call. |
!> | 2008-12 | JE | 4.3.5F90 | Created during the Fortran 90 conversion by extracting the computational core from `shetrn.f`. |
!> | 2026-03-19 | SB | 4.6.1 | Added `DATE_FROM_HOUR`-based reporting of the simulation start/end dates, and added the contaminant/column-array allocation and cleanup calls (`initialise_cont_cc`, `initialise_colm_cg`, `initialise_colm_co`, `deallocate_colm_cg`). |
!> | 2026-04-03 | SvB | 4.6.1 | Replaced `OPEN(UNIT=6, ..., carriagecontrol='fortran')` with `OPEN(UNIT=OUTPUT_UNIT, ...)` from `iso_fortran_env` (portable output-unit handling) as part of a wider restructuring of the main loop. |
!> | 2026-04-23 | SB | 4.6.1 | Added elapsed/remaining wall-clock time reporting (`cpu_time`) to the progress line, and reordered the run-configuration diagnostics printed at start-up. |
!> | 2026-04-28 | SB | 4.6.1 | Reworded the progress-line format and added the line that clears the progress display once the simulation loop exits. |
!> | 2026-05-02 | SvB | 4.6.1 | Removed the pre-loop "Length of Simulation" message during a branch merge; the `9750`/`9900` `FORMAT` labels are no longer referenced by any `WRITE`. |
!> | 2026-05-03 | SvB | 4.6.1 | Changed the sediment-yield elevation buffer `hrf` from a fixed-size array to `ALLOCATABLE`, allocated only when sediment yield (`BEXSY`) is active, to reduce static memory use. |
!> @endhistory
!>
!> @note The module has a large dependency surface because it orchestrates
!> most SHETRAN process modules and shared state arrays. Changes here should
!> be checked against component ordering, mass-balance output, hotstart
!> output, and visualisation side effects.
!> @endnote
!>
MODULE run_sim

   USE SGLOBAL
   USE mod_error, ONLY : RAISE_ERROR, ERRLVL_fatal
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


!> Runs the SHETRAN simulation from the configured start time to end time.
!>
!> `SIMULATION` is the top-level time-stepping routine called after the
!> model has been configured (see [[shetran]]). It initializes framework and
!> output state, enters the main time loop, asks [[rest:tmstep]] for the next
!> time step, calls the process modules in the required order, writes daily
!> and event-driven output, and exits when `UZNOW` reaches `TTH - TIH`.
!>
!> The routine has no dummy arguments. It operates through module variables
!> imported from `SGLOBAL`, `AL_C`, `AL_D`, `FRmod`, `ETmod`, `VSmod`,
!> `OCmod`, `SYmod`, `CMmod`, `rest`, the visualisation interfaces, and
!> supporting parameter modules.
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
!> Sediment yield uses a per-element surface-water-elevation buffer, `hrf`,
!> which is allocated (to `total_no_elements`) only when `BEXSY` is true,
!> since it is otherwise unused.
!>
!> Progress is reported once per simulated day (`icounter3` tracks the next
!> reporting time in hours) as a single self-overwriting line, using
!> `ACHAR(13)` (carriage return) with `ADVANCE='NO'` and an explicit `FLUSH`,
!> and includes the elapsed and estimated remaining wall-clock time from
!> `cpu_time`.
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
!> @note Component ordering is hydrologically significant: ET and VSS are
!> run before the model time is advanced; overland/channel flow is run after
!> rainfall, evaporation, and well transfers are updated; sediment and
!> contaminant calls are gated by their configured start times.
!> @endnote
!>
!> @note The locals `ptub` and `elapsed_time` are declared but not
!> referenced in the current routine body.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1999-01-28 | SB | - | Incorporated sediment output into the `AIOSTO` result-type selection. |
!> | 2006-03-08 | SB | - | Made mass-balance output (`FRMB`) a daily call. |
!> | 2007-05-02 | SB | - | Added an additional `FROUTPUT('main ')` call. |
!> | 2008-12 | JE | 4.3.5F90 | Extracted the timestep loop from the legacy `shetrn.f` main program into this Fortran 90 computational core. |
!> | 2026-03-19 | SB | 4.6.1 | Added human-readable simulation start/end dates and contaminant allocation setup/cleanup calls. |
!> | 2026-04-03 | SvB | 4.6.1 | Restructured the routine to use `OUTPUT_UNIT` for all console output instead of the non-standard `carriagecontrol='fortran'` unit-6 open. |
!> | 2026-04-23 | SB | 4.6.1 | Added elapsed/remaining wall-clock progress reporting via `cpu_time`. |
!> | 2026-05-03 | SvB | 4.6.1 | Changed `hrf` to an allocatable array, allocated only when sediment yield is active. |
!> @endhistory
!>
   SUBROUTINE SIMULATION
      USE, INTRINSIC :: iso_fortran_env, ONLY: OUTPUT_UNIT

      ! Locals
      INTEGER :: ptub, j, iel, k
      REAL(KIND=4), PARAMETER :: rzero = 0.0e0
      INTEGER, PARAMETER :: niosto = 50
      LOGICAL :: bsy, bcm, cmfrst, syfrst
      CHARACTER(LEN=niosto) :: aiosto
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: hrf
      INTEGER, SAVE :: icounter3 = 0
      INTEGER :: c(6)
      CHARACTER(LEN=128) :: dum
      REAL :: start_time, current_time, elapsed_time

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
         c(1), '-', c(2), '-', c(3), ' ', c(4), ':', c(5), ':', c(6)
      WRITE(OUTPUT_UNIT, '(A,A)') ' Simulation Start Date = ', TRIM(dum)

      ! Format and print the simulation end date
      c = DATE_FROM_HOUR(tth)
      WRITE(dum, '(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') &
         c(1), '-', c(2), '-', c(3), ' ', c(4), ':', c(5), ':', c(6)
      WRITE(OUTPUT_UNIT, '(A,A)') ' Simulation End Date   = ', TRIM(dum)

      write(OUTPUT_UNIT,'(A)') ' SHETRAN file folder = '
      write(OUTPUT_UNIT,'(1X,A)') DIRQQ
      write(OUTPUT_UNIT,'(A)') ' SHETRAN rundata name = '
      write(OUTPUT_UNIT,'(A)') ' rundata_'//trim(cnam)//'.txt'
      write(OUTPUT_UNIT,*)
      write(OUTPUT_UNIT,*)
      write(OUTPUT_UNIT,*)

      call cpu_time(start_time)


      !------------------------------------------------------------------
      !                     MAIN SIMULATION LOOP
      !------------------------------------------------------------------
      IF (bexsy) THEN
          ALLOCATE(hrf(total_no_elements))
          CALL GET_NSED_EARLY ()     !VISVISVIS
          ENDIF
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
              IF (BEXSY.AND. (.NOT.BSY) ) CALL RAISE_ERROR(ERRLVL_fatal, 1041, CMP, 0, 0, &
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
              write(OUTPUT_UNIT,9751,advance="no") achar(13), uznow, min(100*uznow/(TTH - TIH),100.00),int(current_time - start_time), int((current_time - start_time)/(uznow/(TTH - TIH))-(current_time - start_time))
              call flush(OUTPUT_UNIT)
              icounter3 = icounter3 + 24
              endif
          IF (UZNOW>=(TTH - TIH) ) EXIT
          ENDDO

      ! this line is to clear the progress line after the simulation has finished
      WRITE (OUTPUT_UNIT,'(A)') '                                                                                                                                '


      9750 FORMAT (' Length of Simulation =',F12.2,' hours '//)
      9751 FORMAT (A,'Simulation = ',F0.1,' hrs, % Compl. = ', f0.2,', Elapsed/Remaining = ', I0, ' / ', I0, ' sec. ')
      9800 FORMAT ('Current time = ',F10.2,' hours. Number of steps = ',I7 /)
      9900 FORMAT ('Normal completion of SHETRAN run: ',F10.2, ' hours, ', I7,' steps.' /)
   END SUBROUTINE simulation

END MODULE run_sim
