!-------------------------------------------------------------------------------
! 
!> @file mod_parameters.f90
!> @ingroup lib_util
!
!> @author Stephen Birkinshaw, Newcastle University
! 
!> @brief 
!! SHETran
!
!> @mainpage SHETran
!! Hydrological modelling package SHETran.
! 
! REVISION HISTORY:
! <Incomplete>
! 20191210 - v4.4.6 - SteveB - Added Hotstart ability
! 20200305 -        - SvenB  - Cleanups
!
!-------------------------------------------------------------------------------
PROGRAM SHETRAN
    USE SGLOBAL
    USE AL_D,                          ONLY : nstep
    USE mod_load_filedata,             ONLY : ALTRAP
    USE GETDIRQQ,                      ONLY : GET_DIR_AND_CATCH  
    USE FRmod,                         ONLY : FROPEN, FROUTPUT  
    USE VISUALISATION_INTERFACE_RIGHT, ONLY : RECORD_VISUALISATION_DATA  !VISVISVIS
    USE REST,                          ONLY : EXTRA_OUTPUT
    USE RUN_SIM,                       ONLY : SIMULATION
    
    IMPLICIT NONE
    
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
    
    CALL sleepqq(500)

END PROGRAM SHETRAN