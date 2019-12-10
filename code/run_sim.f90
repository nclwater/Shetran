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
                     psi4, uzalfa !THESE NEEDED ONLY FOR AD
USE rest,     ONLY : BALWAT, TMSTEP, &
                     metime, melast, eptime, pinp
                     !start_impact_window, end_impact_window, per_rain, mx_cnt_rain, cnt_rain !these here only for AD
USE FRmod,    ONLY : INCM, FRINIT
USE OCmod,    ONLY : OCSIM
USE OCQDQMOD, ONLY : STRXX, STRYY       
USE OCmod2,   ONLY : GETHRF, &
                     HRFZZ !HRFZZ NEEDED ONLY FOR AD
USE FRmod,    ONLY : FRSORT, FROUTPUT, FRMB, FRRESP  
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
!USE PERTURBATIONS, ONLY : LOAD_PERTURBATIONS, spatial1
IMPLICIT NONE

PRIVATE
PUBLIC :: simulation

CONTAINS


!SSSSSS SUBROUTINE simulation
SUBROUTINE SIMULATION
!Runs the simulation set up in PROGRAM SHETRN4F90
INTEGER                                       :: ptub, j
REAL(4), PARAMETER                            :: rzero = 0.0e0
INTEGER, PARAMETER                            :: niosto = 50
INTEGER                                       :: iel,k
LOGICAL                                       :: bsy, bcm, cmfrst,syfrst
CHARACTER(NIOSTO)                             :: aiosto
DOUBLEPRECISION, DIMENSION(nelee)             :: hrf
INTEGER, SAVE                                 :: icounter3 = 0


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

write(6,9750) TTH - TIH

!------------------------------------------------------------------
!                     MAIN SIMULATION LOOP
!------------------------------------------------------------------
IF (bexsy) CALL GET_NSED_EARLY ()     !VISVISVIS
IF (bexcm) CALL GET_NCON_EARLY ()     !VISVISVIS
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
        write(6,9751) uznow, min(100*uznow/(TTH - TIH),100.00)
        icounter3 = icounter3 + 24  
    endif  
    IF (UZNOW>=(TTH - TIH) ) EXIT
ENDDO


9750 FORMAT ('  Length of Simulation =',F12.2,' hours. '//)  
9751 FORMAT ('+','Simulation Timestep =',F12.2,' hours   % Completed = ', f6.2)  
9800 FORMAT ('Current time = ',F10.2,' hours. Number of steps = ',I7 /)  
9900 FORMAT ('Normal completion of SHETRAN run: ',F10.2, ' hours, ', I7,' steps.' /)
END SUBROUTINE simulation
END MODULE run_sim