module ZQmod

!-------------------------------------------------------------------------------
!
!> @file ZQmod.f90
!
!> @author Daryl Hughes, Newcastle University
!> @author Stephen Birkinshaw, Newcastle University
!> @author Sven Berendsen, Newcastle University
!
!> @brief
!! This script is the enginge of the reservoir model designed by Daryl Hughes and Steve Birksinshaw in 2020.
!
!> @details
!! For models which contain reservoirs, it outputs downstream discharge as a function of upstream water elevation.
!! The user must create an elevation-discharge (ZQ) set up file and reference this in the RunDatafile (module 51).
!! This file may contain multiple ZQ tables i.e. for multiple channel links
!! These can be created in Excel etc., and saved as .txt. The file should be space-delimited, so may require replacement of tabs with spaces
!! The ZQtables require a Z column (first), followed by at least one discharge column.
!! These should have names along the format 'ZQ>##.##' i.e. discharge at elevations above this threshold
!! The number of rows and the interval between Zs is arbitrary (for example, 0.01m intervals would be suitable)
!
! REVISION HISTORY:
! ? - DH - Initial version
! ? - SB - Reworked for inclusion in SHETRAN4.4.6.Res2
!
!-------------------------------------------------------------------------------

   USE sglobal,    ONLY: UZNOW                                                 ! UZNOW is sim time (hours)
   USE AL_C,       ONLY: DTUZ,UZNEXT                                           ! DZ is sim time (seconds),  UZNEXT is time step to be added to previous time to get current time
   USE AL_D,       ONLY: zqd,NoZQTables,ZQTableLink,ZQTableFace,ZQweirSill     ! these are specifically for ZQmod
   USE mod_parameters                                                          ! general parameters


   IMPLICIT NONE

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Variables

   ! set everything to private by default
   PRIVATE

   ! module variables
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQcols                  !< use to dimension allocatable arrays
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: nZQrows                  !< use to dimension allocatable arrays
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: zcol                     !< use to dimension allocatable arrays
   REAL(kind=R8P), DIMENSION(:,:), ALLOCATABLE     :: headerRealArray          !< real array to store weirEq stage thresholds
   REAL(kind=R8P), DIMENSION(:,:,:), ALLOCATABLE   :: ZQ                       !< ZQ = 2D array (nZQrows, nZQcols)
   INTEGER(kind=I_P), DIMENSION(:), ALLOCATABLE    :: ZQTableOpHour            !< the hour at which sluices are operated
   INTEGER(kind=I_P)                               :: ZQTableRef               !< the reference number of the ZQtable

   ! what is public from this module?
   PUBLIC                                          :: ReadZQTable, get_ZQTable_value   ! subroutine names

CONTAINS


!---------------------------------------------------------------------------
!> @author Dary Hughes, Newcastle University
!> @author Stephen Birkinshaw, Newcastle University
!> @author Sven Berendsen, Newcastle University
!
!> @brief
!! ReadZQTable reads in the user-defined ZQ file, which includes ZQ tables
!! and ZQ meta data.
!! ZQ table(s) contain column and row headers, and the actual values needed.
!! Metadata includes the number of ZQ tables needed (i.e. reservoirs in the
!! model), link and face numbers, and operation hours.
!! These are converted into a 2D array and used as a lookuptable.
!
!
! REVISION HISTORY:
! ? - DH - Initial version
! ? - SB - Reworked for inclusion in SHETRAN4.4.6.Res2
!---------------------------------------------------------------------------
   SUBROUTINE ReadZQTable()

      ! general variables
      INTEGER(KIND=I_P)                               :: i, j, k, printRow, printCol, pos  !< useful local integers
      INTEGER                                         :: ios                               !< I/O status integer

      ! specific variables
      CHARACTER(LEN=120)                              :: headerRaw                         !< stores the entire first line
      CHARACTER(LEN=9), DIMENSION(:,:), ALLOCATABLE   :: headerRawArray                    !< character array for header names
      CHARACTER(LEN=9), DIMENSION(:,:), ALLOCATABLE   :: headerCharArray                   !< array for trimmed header names
      INTEGER(KIND=I_P)                               :: maxnumberRows, maxnumberCols      !< use to dimension allocatable arrays
      LOGICAL                                         :: IsZQreadOK = .FALSE.              !< sets initial value for error catching

      INTEGER(KIND=I_P)                               :: fid_ZQ_log                        !< file-id of the ZQ-table-logfile

      ! Code -----------------------------------------------------------------
      OPEN(NEWUNIT=fid_ZQ_log, FILE='output_readZQTable.txt', IOSTAT=ios)
      IF (ios /= 0) CALL handle_zq_error()

      ! read ZQ tables
      READ(zqd, *, IOSTAT=ios)                                                        ! skip line 1
      IF (ios /= 0) CALL handle_zq_error()

      READ(zqd, *, IOSTAT=ios) NoZQTables                                             ! read line 2
      IF (ios /= 0) CALL handle_zq_error()

      ALLOCATE(nZQcols(NoZQTables))
      ALLOCATE(nZQrows(NoZQTables))
      ALLOCATE(zcol(NoZQTables))
      ALLOCATE(ZQTableLink(NoZQTables))
      ALLOCATE(ZQTableFace(NoZQTables))
      ALLOCATE(ZQTableOpHour(NoZQTables))
      ALLOCATE(ZQWeirSill(NoZQTables))

      DO i = 1, NoZQTables                                                            ! loop through ZQtables
         DO j = 1, 9
            READ(zqd, *, IOSTAT=ios)                                                ! skip lines 3-11
            IF (ios /= 0) CALL handle_zq_error()
         END DO

         READ(zqd, *, IOSTAT=ios) nZQrows(i)                                         ! read line 12
         IF (ios /= 0) CALL handle_zq_error()

         READ(zqd, *, IOSTAT=ios)                                                    ! skip line 13
         IF (ios /= 0) CALL handle_zq_error()

         READ(zqd, "(A)", IOSTAT=ios) headerRaw                                      ! read line 14
         IF (ios /= 0) CALL handle_zq_error()

         nZQcols(i) = 0                                                              ! initialise nZQcols counter

         ! Robust token counting loop
         headerRaw = ADJUSTL(headerRaw)
         DO WHILE (LEN_TRIM(headerRaw) > 0)
            pos = INDEX(headerRaw, " ")
            IF (pos == 0) THEN
               headerRaw = ""                                                      ! No more spaces, reached the last element
            ELSE
               headerRaw = ADJUSTL(headerRaw(pos+1:))                              ! Advance string past space
            END IF
            nZQcols(i) = nZQcols(i) + 1
         END DO

         DO j = 1, nZQrows(i)
            READ(zqd, *, IOSTAT=ios)                                                ! read ZQ table as zqd
            IF (ios /= 0) CALL handle_zq_error()
         END DO
      END DO

      maxnumberRows = MAXVAL(nZQrows)
      maxnumberCols = MAXVAL(nZQcols)

      ! allocate array dimensions using maxnumberRows and maxnumberCols
      ALLOCATE(ZQ(maxnumberRows, maxnumberCols, NoZQTables))
      ALLOCATE(headerRawArray(maxnumberCols, NoZQTables))
      ALLOCATE(headerCharArray(maxnumberCols, NoZQTables))
      ALLOCATE(headerRealArray(maxnumberCols, NoZQTables))
      REWIND(zqd)

      ! read ZQ metadata
      READ(zqd, *, IOSTAT=ios)                                                        ! skip line 1
      READ(zqd, *, IOSTAT=ios)                                                        ! skip line 2

      DO i = 1, NoZQTables

         zcol(i) = 2                                                                 ! set zcol=2 to start with

         READ(zqd, *, IOSTAT=ios)                                                    ! skip line 3
         IF (ios /= 0) CALL handle_zq_error()
         READ(zqd, *, IOSTAT=ios) ZQTableRef                                         ! read line 4
         IF (ios /= 0) CALL handle_zq_error()
         READ(zqd, *, IOSTAT=ios)                                                    ! skip line 5
         IF (ios /= 0) CALL handle_zq_error()
         READ(zqd, *, IOSTAT=ios) ZQTableLink(i)                                     ! read line 6
         IF (ios /= 0) CALL handle_zq_error()
         READ(zqd, *, IOSTAT=ios)                                                    ! skip line 7
         IF (ios /= 0) CALL handle_zq_error()
         READ(zqd, *, IOSTAT=ios) ZQTableFace(i)                                     ! read line 8
         IF (ios /= 0) CALL handle_zq_error()
         READ(zqd, *, IOSTAT=ios)                                                    ! skip line 9
         IF (ios /= 0) CALL handle_zq_error()
         READ(zqd, *, IOSTAT=ios) ZQTableOpHour(i)                                   ! read line 10
         IF (ios /= 0) CALL handle_zq_error()

         READ(zqd, *, IOSTAT=ios)                                                    ! skip line 11
         READ(zqd, *, IOSTAT=ios)                                                    ! skip line 12
         READ(zqd, *, IOSTAT=ios)                                                    ! skip line 13
         READ(zqd, "(A)", IOSTAT=ios) headerRaw                                      ! read line 14
         IF (ios /= 0) CALL handle_zq_error()

         ! convert headerRaw to headerRawArray
         headerRaw = ADJUSTL(headerRaw)
         DO j = 1, nZQcols(i)
            pos = INDEX(headerRaw, " ")
            IF (pos == 0) pos = LEN_TRIM(headerRaw) + 1
            headerRawArray(j,i) = headerRaw(1:pos-1)
            IF (pos <= LEN(headerRaw)) THEN
               headerRaw = ADJUSTL(headerRaw(pos+1:))
            ELSE
               headerRaw = ""
            END IF
         END DO

         ! convert headerRawArray to headerCharArray and then headerRealArray e.g. 'ZQ>96.8' -> 96.80
         headerCharArray(1,i) = 'Z'                                                  ! set the col/row header as 'Z'
         DO j = 2, nZQcols(i)
            pos = INDEX(headerRawArray(j,i), '>')
            headerCharArray(j,i) = headerRawArray(j,i)(pos+1:)
            READ(headerCharArray(j,i), *, IOSTAT=ios) headerRealArray(j,i)
            IF (ios /= 0) CALL handle_zq_error()
         END DO

         ! read ZQweirSill as lowest value of headers
         ZQweirSill(i) = headerRealArray(2,i)

         DO j = 1, nZQrows(i)
            READ(zqd, *, IOSTAT=ios) (ZQ(j, k, i), k = 1, nZQcols(i))
            IF (ios /= 0) CALL handle_zq_error()
         END DO

         ! write ZQTables to fid_ZQ_log.fort
         WRITE(fid_ZQ_log, *) 'ZQTableRef   =', ZQTableRef
         WRITE(fid_ZQ_log, *) 'ZQTableLink  =', ZQTableLink(i)
         WRITE(fid_ZQ_log, *) 'ZQTableFace  =', ZQTableFace(i)
         WRITE(fid_ZQ_log, *) 'ZQTableOpHour=', ZQTableOpHour(i)
         WRITE(fid_ZQ_log, *) 'nZQcols      =', nZQcols(i)
         WRITE(fid_ZQ_log, *) 'nZQrows      =', nZQrows(i)

         ! Uses the modern unlimited repeat formatter "(*(...))"
         WRITE(fid_ZQ_log, '(A, *(A10))') 'ZQ headers: ', headerRawArray(1:nZQcols(i), i)

         DO printRow = 1, nZQrows(i)
            WRITE(fid_ZQ_log, '(*(F12.3))') (ZQ(printRow, printCol, i), printCol = 1, nZQcols(i))
         END DO
      END DO

      CLOSE(zqd)
      CLOSE(fid_ZQ_log)

      RETURN

   CONTAINS

      !> @brief Centralized error handling replacing legacy GOTO jumps
      SUBROUTINE handle_zq_error()
         PRINT *, 'error reading ZQ table'
         ! Uses F2008+ standard ERROR STOP to safely exit execution with a status code
         ERROR STOP 255
      END SUBROUTINE handle_zq_error

   END SUBROUTINE ReadZQTable


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

END MODULE ZQmod

