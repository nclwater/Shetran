module zq_table_reader

!-------------------------------------------------------------------------------
!
!> @file zq_table_reader.f90
!
!> @author Refactored from ZQmod.f90 by GitHub Copilot
!> @author Original: Daryl Hughes, Newcastle University
!> @author Original: Stephen Birkinshaw, Newcastle University
!> @author Original: Sven Berendsen, Newcastle University
!
!> @brief
!! ZQ table file reading and parsing functionality.
!! This module handles reading elevation-discharge tables from user-defined files
!! and parsing the metadata associated with each reservoir/weir structure.
!
!> @details
!! Reads ZQ table files containing:
!! - Number of ZQ tables needed
!! - Table dimensions (rows and columns)
!! - Header information with stage thresholds
!! - Link and face numbers for hydraulic connections
!! - Operation schedules for sluices
!! - The actual elevation-discharge lookup data
!
! REVISION HISTORY:
! 2025-09-05 - Refactored from original ZQmod.f90 ReadZQTable subroutine
!
!-------------------------------------------------------------------------------

   USE AL_D,           ONLY: zqd,NoZQTables,ZQTableLink,ZQTableFace,ZQweirSill     ! ZQ-specific data from AL_D
   USE mod_parameters                                                              ! general parameters
   USE zq_data_types                                                               ! ZQ data structures

   IMPLICIT NONE

   ! set everything to private by default
   PRIVATE

   ! what is public from this module?
   PUBLIC                                          :: ReadZQTable                   ! subroutine name

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
   ! 2025-09-05 - Refactored into separate module
   !---------------------------------------------------------------------------
   SUBROUTINE ReadZQTable()

      ! general variables
      INTEGER(kind=I_P)                               :: i, j, k, printRow, printCol, pos     !< useful local integers

      ! specific variables
      CHARACTER(LEN = 120)                            :: headerRaw                            !< stores the entire first line of the ZQtable file
      CHARACTER(LEN = 9), DIMENSION(:,:), ALLOCATABLE :: headerRawArray                       !< character array to store ZQtable header names
      CHARACTER(LEN = 9), DIMENSION(:,:), ALLOCATABLE :: headerCharArray                      !< character array to store trimmed ZQtable header names
      INTEGER(kind=I_P)                               :: maxnumberRows, maxnumberCols         !< use to dimension allocatable arrays
      LOGICAL                                         :: IsZQreadOK=.FALSE.                   !< sets initial value for error catching

      INTEGER(kind=I_P)                               :: fid_ZQ_log                           !< file-id of the ZQ-table-logfile


      ! Code -----------------------------------------------------------------
      OPEN(newunit = fid_ZQ_log,FILE='output_readZQTable.txt', ERR=101)

      ! read ZQ tables
      READ(zqd,*)                                                             ! skip line 1 ': NUMBER OF ZQ TABLES NEEDED'
      READ(zqd,*, END = 101) NoZQTables                                       ! read line 2 as NoZQTables. This is used to allocate the number of ZQ arrays expected

      ALLOCATE(nZQcols(NoZQTables))
      ALLOCATE(nZQrows(NoZQTables))
      ALLOCATE(zcol(NoZQTables))
      ALLOCATE(ZQTableLink(NoZQTables))
      ALLOCATE(ZQTableFace(NoZQTables))
      ALLOCATE(ZQTableOpHour(NoZQTables))
      ALLOCATE(ZQWeirSill(NoZQTables))

      DO i = 1,NoZQTables                                                     ! start loop through number of ZQtables defined
         DO j = 1,9
            read(zqd,*)                                                     ! skip lines 3-11
         ENDDO

         read(zqd,*, END = 101) nZQrows(i)                                   ! read line 12 as number of ZQrows (nZQrows)
         read(zqd,*)                                                         ! skip line 13
         read(zqd, "(A)", END = 101) headerRaw                               ! read line 14 as headerRaw

         nZQcols(i) = 0                                                      ! initialise nZQcols counter
         DO WHILE(LEN(TRIM(headerRaw)) > 0)                                  ! start loop through headerRaw count nZQcols using space delimiters
            pos        = INDEX(headerRaw, " ")                              ! store position of first space delimiter
            headerRaw  = headerRaw(pos+1:)                                  ! store remaining headerRaw (from pos+1 to end) as headerRaw
            nZQcols(i) = nZQcols(i) + 1                                     ! increase nZQcols counter
         END DO

         DO j=1,nZQrows(i)
            read(zqd,*)                                                     ! read ZQ table as zqd
         ENDDO
      ENDDO

      maxnumberRows = maxval(nZQrows)
      maxnumberCols = maxval(nZQcols)

      ! allocate array dimensions using maxnumberRows and maxnumberCols
      ALLOCATE(ZQ(maxnumberRows,maxnumberCols,NoZQTables))
      ALLOCATE(headerRawArray(maxnumberCols,NoZQTables))
      ALLOCATE(headerCharArray(maxnumberCols,NoZQTables))
      ALLOCATE(headerRealArray(maxnumberCols,NoZQTables))
      REWIND (zqd)

      ! read ZQ metadata
      read(zqd,*)                                                             ! skip line 1
      read(zqd,*)                                                             ! skip line 2

      DO i = 1,NoZQTables

         zcol(i) = 2                                                         ! set zcol=2 to start with

         read(zqd,*)                                                         ! skip line 3
         read(zqd,*, END = 101)ZQTableRef                                    ! read line 4 as ZQTableRef
         read(zqd,*)                                                         ! skip line 5
         read(zqd,*, END = 101)ZQTableLink(i)                                ! read line 6 as ZQTableLink
         read(zqd,*)                                                         ! skip line 7
         read(zqd,*, END = 101)ZQTableFace(i)                                ! read line 8 as ZQTableFace
         read(zqd,*)                                                         ! skip line 9
         read(zqd,*, END = 101)ZQTableOpHour(i)                              ! read line 10 as ZQTableOpHour
         read(zqd,*)                                                         ! skip line 11
         read(zqd,*)                                                         ! skip line 12
         read(zqd,*)                                                         ! skip line 13
         read(zqd, "(A)", END = 101)headerRaw                                ! read in line as headerRaw

         ! convert headerRaw to headerRawArray
         DO j = 1,nZQcols(i)                                                 ! start loop through headerRaw count nZQcols using space delimiters
            pos                 = INDEX(headerRaw, " ")                     ! store position of first space delimiter
            headerRawArray(j,i) = headerRaw(1:pos)                          ! store substring of headerRaw from 1 to pos (1st col)
            headerRaw           = headerRaw(pos+1:)                         ! store remaining headerRaw (from pos+1 to end) as headerRaw
         END DO

         ! convert headerRawArray to headerCharArray and then headerRealArray e.g. 'ZQ>96.8' -> 96.80
         headerCharArray(1,i) = 'Z'                                          ! set the col/row header as 'Z'
         DO j = 2,nZQcols(i)                                                 ! start loop, skipping first item as this is the col header
            headerCharArray(j,i) = headerRawArray(j,i)(                     &
            &                                          INDEX(headerRawArray(j,i),'>')+1:)   ! return numpart of alphanum string header, by finding index of substring '>', and adding 1
            read(headerCharArray(j,i),*) headerRealArray(j,i)               ! convert character to real
         END DO

         ! read ZQweirSill as lowest value of headers
         ZQweirSill(i) = headerRealArray(2,i)                                ! NB: this relies on the user ensuring that the ZQtable file cols start from minimum and ascend
         !     left to right. Error catch needed?

         DO j = 1, nZQrows(i)                                                ! for subsequent lines in file(1), do the following:
            read(zqd,*, END = 101) (ZQ(j,k,i), k=1, nZQcols(i))             ! implied do: read 1st value as ZQ(i,1) 2nd as ZQ(i,2)...
         END DO


         ! write ZQTables to fid_ZQ_log.fort
         WRITE(fid_ZQ_log, *) 'ZQTableRef   =', ZQTableRef
         WRITE(fid_ZQ_log, *) 'ZQTableLink  =', ZQTableLink(i)
         WRITE(fid_ZQ_log, *) 'ZQTableFace  =', ZQTableFace(i)
         WRITE(fid_ZQ_log, *) 'ZQTableOpHour=', ZQTableOpHour(i)
         WRITE(fid_ZQ_log, *) 'nZQcols      =', nZQcols(i)
         WRITE(fid_ZQ_log, *) 'nZQrows      =', nZQrows(i)
         WRITE(fid_ZQ_log, '(5(A))') 'ZQ headers: ',                        &
         &               headerRawArray(1:nZQcols(i),i)                                  ! write headers, character format
         DO printRow = 1, nZQrows(i)                                         ! specify which rows to print
            WRITE(fid_ZQ_log,'(5(f12.3))')                                  &
            &                   (ZQ(printRow, printCol,i), printCol=1,nZQcols(i))           ! implied do: in array ZQ, print each col, real format
         END DO
      ENDDO

      CLOSE(zqd)                                                              ! close file zqd
      CLOSE(fid_ZQ_log)

      return

      ! error management
101   CONTINUE
      PRINT*,'error reading ZQ table'
      STOP 255

   END SUBROUTINE ReadZQTable

END MODULE zq_table_reader
