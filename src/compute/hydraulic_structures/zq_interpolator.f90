module zq_interpolator

!-------------------------------------------------------------------------------
!
!> @file zq_interpolator.f90
!
!> @author Refactored from ZQmod.f90 by GitHub Copilot
!> @author Original: Daryl Hughes, Newcastle University
!> @author Original: Stephen Birkinshaw, Newcastle University
!> @author Original: Sven Berendsen, Newcastle University
!
!> @brief
!! ZQ table interpolation and value lookup functionality.
!! This module handles the interpolation of discharge values from ZQ tables
!! based on upstream water elevation and sluice operation schedules.
!
!> @details
!! Provides the core functionality to:
!! - Select appropriate weir equation based on stage thresholds
!! - Handle sluice operation timing
!! - Interpolate discharge values from ZQ lookup tables
!! - Apply stage-discharge relationships for hydraulic structures
!
! REVISION HISTORY:
! 2025-09-05 - Refactored from original ZQmod.f90 get_ZQTable_value function
!
!-------------------------------------------------------------------------------

   USE sglobal,        ONLY: UZNOW                                                 ! UZNOW is sim time (hours)
   USE AL_C,           ONLY: DTUZ,UZNEXT                                           ! DZ is sim time (seconds),  UZNEXT is time step to be added to previous time to get current time
   USE mod_parameters                                                              ! general parameters
   USE zq_data_types                                                               ! ZQ data structures

   IMPLICIT NONE

   ! set everything to private by default
   PRIVATE

   ! what is public from this module?
   PUBLIC                                          :: get_ZQTable_value            ! function name

CONTAINS

   !---------------------------------------------------------------------------
   !> @author Dary Hughes, Newcastle University
   !> @author Stephen Birkinshaw, Newcastle University
   !> @author Sven Berendsen, Newcastle University
   !
   !> @brief
   ! ZQTable uses the ZQ array from ReadZQTable to calculate downstream flow (Qd)
   ! It activates the specified ZQcol using the ZQTableOpHour from ReadZQTable
   !
   !
   ! REVISION HISTORY:
   ! ? - DH - Initial version
   ! ? - SB - Reworked for inclusion in SHETRAN4.4.6.Res2
   ! 2025-09-05 - Refactored into separate module
   !
   !> @param[in]   ZQref, Zu
   !> @param[return]  Qd
   !---------------------------------------------------------------------------
   FUNCTION get_ZQTable_value(ZQref,zu) RESULT(qd)

      ! IO variables
      INTEGER(kind=I_P), INTENT(IN)   :: ZQref    !< reference number of weir
      REAL(kind=R8P), INTENT(IN)      :: Zu       !< Zu = upstream stage
      REAL(kind=R8P)                  :: Qd       !< Qd = downstream discharge

      ! general variables
      INTEGER(kind=I_P)               :: i        !< loop counter

      ! Code -----------------------------------------------------------------

      ! start sluice operation loop
      IF (INT(UZNOW + ZQTableOpHour(ZQref)) / 24 >                            &
      &           INT(UZNOW + ZQTableOpHour(ZQref) - UZNEXT) / 24) THEN               ! if current day integer > previous day INT(UZNOW), then operate sluices:
         !WRITE(778, *), 'new day'                                            ! write for test purposes

         ! select weir equation (Zcol) based on which range of stages Zu falls into
         ! NB if Zu < min ZQ threshold, will return an error
         DO i = nZQcols(ZQref), 2, -1                                        ! start loop in descending order of ZQ thresholds
            IF(Zu > headerRealArray(i,ZQref)) THEN                          ! test if Zu > ZQ threshold
               zcol(ZQref) = i                                             ! if TRUE, then pass i (nZQcol) to zcol...
               EXIT                                                        ! ...and exit
            ELSEIF(Zu > headerRealArray(2,ZQref)) THEN                      ! elseif Zu is greater than the minimum ZQ threshold ->restart loop
            ELSE                                                            ! else Zu is below threshold, print warning and exit loop
               PRINT*,                                                     &
               &                       'warning: Zu is below minimum ZQthreshold defined in ZQtable'
               EXIT
            ENDIF
         END DO
      ENDIF                                                                   ! end sluice operation loop



      ! look up z value in ZQ array which matches Zu and return corresponding Qd
      DO i = 1, nZQrows(ZQref)                                                ! start loop through rows for a given table
         IF(Zu > ZQ(i, 1, ZQref)) THEN                                       ! if Zu is greater than the ith value in the z column...
            Qd = -999                                                       ! return dummy value -999
         ELSE
            Qd = ZQ(i, zcol(ZQref), ZQref)                                  ! when Zu is found, finds Qd from zcol, and assigns to Qd
            EXIT                                                            ! exit loop, preserving Qd. NB STOP wipes variable assignment
         END IF
      END DO

      !PRINT*, ZQref,zu,qd                                                     ! NB duplicates print from OCMOD2 line 664

      ! write everytimestep outputs to 778.fort
      !IF(UZNOW <0.1) THEN                                                     ! write header at sim start
      !    WRITE(778, *), '        UZNOW,      Zu,         Qd'
      !    WRITE(778, *), '        i,      zcol'
      !ENDIF
      !WRITE(778,'(6(f12.2,1a))')  uznow,  ',', &                              ! write real output
      !                            Zu,     ',', &
      !                            Qd,     ','
      !WRITE(778, *)               i,      ',', &                              ! write integer output
      !                           zcol,   ','

   END FUNCTION get_ZQTable_value

END MODULE zq_interpolator
