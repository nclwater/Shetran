PROGRAM SHETRN4F90
USE SGLOBAL
USE AL_D,                          ONLY : nstep  !, dtmet2, nrain, nrainc
USE ALmod,                         ONLY : ALTRAP
USE GETDIRQQ,                      ONLY : GET_DIR_AND_CATCH  
USE FRmod,                         ONLY : FROPEN, FROUTPUT  
USE VISUALISATION_INTERFACE_RIGHT, ONLY : RECORD_VISUALISATION_DATA  !VISVISVIS
!USE VISUALISATION_INTERFACE_LEFT,  ONLY : ELEMENT, S_ELEVATION       !VISVISVIS
!USE VISUALISATION_INTERFACE_LEFT, ONLY : GET_NSED_EARLY,  GET_NCON_EARLY ! sb is this needed here
USE REST,                          ONLY : EXTRA_OUTPUT
USE RUN_SIM,                       ONLY : SIMULATION
!USE UTILSMOD,                      ONLY : OPEN_FILE, GET_START_END_IMPACT

IMPLICIT NONE

!INTEGER, PARAMETER                           :: iu=8798, ires=iu+133
!INTEGER                                      :: patternno, szmonte2, i, telno, tlnno, tcelln, na, no_rain_stations, &
!                                                no_rain_data_in_impact_window, gauge, val
!INTEGER, DIMENSION(:), ALLOCATABLE           :: countcells                                                
!DOUBLEPRECISION                              :: qmmm, start_i, end_i, delinc, step_rain
!DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE   :: d_strickler
!DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: d_rain
!CHARACTER(256)                               :: fname, msg, text, results

        !DEC$ DEFINE exact_test=2
        !DEC$ IF(exact_test==1)
!            INTEGER                                    :: szeg, jj
!            DOUBLEPRECISION                            :: unperturbed, grad
!            DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: exact_grad
        !DEC$ ENDIF

CALL ERROR ( - 999, 0, 0, 0, 0, 'Initialise error messages')  
CALL GET_DIR_AND_CATCH (runfil, filnam, cnam, dirqq, rootdir)

CALL ALTRAP
NSTEP = 0  
UZNOW = zero  
CALL FROPEN  !opens all the data files


CALL SIMULATION
CALL FROUTPUT('end  ') 
CALL EXTRA_OUTPUT()
CALL RECORD_VISUALISATION_DATA (REAL(uznow, KIND = 4) , 'end')  

call sleepqq(500)


END PROGRAM SHETRN4F90