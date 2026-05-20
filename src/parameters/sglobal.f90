!> summary: Legacy global constants, run state, and error helpers.
!>
!> This module contains SHETRAN-wide size limits, version information, run
!> filenames, catchment paths, global time, geometry arrays, numeric comparison
!> helpers, and the legacy error-reporting interface. It is a Fortran 90
!> consolidation of earlier COMMON-block include files and remains widely used
!> by the older model components.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-02 | GP | 2.0 | SHE88 implementation on Newcastle AMDAHL. |
!> | 1989-03 | GP | 2.1 | Added SZ drain boundary dimensions. |
!> | 1989-04 | GP | 2.2 | Integrated sediment-yield dimensions and overall version number. |
!> | 1990-03 | GP | 3.0 | Added development version flag and further dimension variables. |
!> | 1992-01 | GP | 3.3 | Updated vegetation breakpoint and ET table dimensions. |
!> | 1994-09-30 | RAH | 3.4.1 | Standardised declarations, headers, dimensions, and comments. |
!> | 1996-10-24 | GP | 4.0 | Altered model dimensions and added VSS module dimensions. |
!> | 1997-02-18 | RAH | 4.1 | Removed redundant dimensions and updated version/banner metadata. |
!> | 1998-02-20 | RAH | 4.2 | Updated version/banner and removed unused dimensions. |
!> | 2004-07 | JE | - | Converted to Fortran 95 during SHEGRAPH v2 integration. |
!> | 2009-01 | JE | 4.3.5F90 | Created module form as part of Fortran 90 conversion, replacing `AL_P` and related global include files. |
!> | 2026-03 | SB | 4.6 | Increased array sizes after 2-D and 3-D arrays became allocatable, including `NXOCEE=4*NXEE`. |
!> @endhistory
MODULE sglobal
!USE BUFF_DISK
   USE MOD_PARAMETERS, ONLY : I_P, R8P, LENGTH_FILEPATH
IMPLICIT NONE
! Legacy AL_P distributed constants and global state.
! 970218 temporary block removed to temporary.f90, JE 170704.
!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!      IMPLICIT INTEGER (I-N)

!------------ SHE VERSION NUMBER

!     (MUST BE IN FORMAT XX.Y WHERE XX = MAJOR PART OF VERSION NUMBER,
!                                    Y = MINOR PART )
      DOUBLEPRECISION, PARAMETER :: SHEVER=4.6
!
!------------ DEVELOPMENT VERSION FLAG

!     SET TO .TRUE. FOR DEVELOPMENT VERSION OF CODE
!     SET TO .FALSE. FOR FINAL STATIC VERSION
      LOGICAL, PARAMETER :: BDEVER=.TRUE.

!------------ BANNER HEADER
!     Description of local implementation: software, architecture, date
      CHARACTER(*), PARAMETER ::BANNER='SHETRAN Hydrological Model'

!------------ RUNDATA FILENAME (THE CATCHMENT NAME ENTERED AT THE START
!     OF A SIMULATION RUN IS APPENDED TO THIS FILENAME)
      CHARACTER(*), PARAMETER :: RUNFIL='rundata_'

!------------ ARRAY SIZES (must be greater than zero!)

! --- Grid points in x,y directions, river links, total no of elements
!16 Sep 94  NB  INFR references elements LCODEX(NX+1) and LCODEY(NY+1)!
!30 Sep 94  NB  NELEE is also used as size of workspace arrays.
!Jan 2009   JE  this link broken - it wastes memory - workspace now set separately
      INTEGER, PARAMETER :: nxee=1000, nyee=1000, nlfee=20000, nelee=250000  !sv4.5
!      INTEGER, PARAMETER :: nxee=1000, nyee=1000, nlfee=20000, nelee=250000  !sv4.6
!      INTEGER, PARAMETER :: nxee=400, nyee=400, nlfee=2000, nelee=80000  !sv4.5
      INTEGER            :: total_no_elements=-1, total_no_links=-1, top_cell_no=-1, szmonte=-1, &
                            ran2monte1=-1, ran2monte2=-1, pcmonte=-1
      INTEGER(1), DIMENSION(:,:), ALLOCATABLE :: montec
      
! --- GRID POINTS IN VERTICAL PLUS ONE
      !INTEGER, PARAMETER :: LLEE=50
      INTEGER, PARAMETER :: LLEE=50  

! --- VEGETATION TYPES, SOIL TYPES (NVEE also used for number of precipitation and pet stations)
      INTEGER, PARAMETER :: NVEE=250000,NSEE=1000

! --- TABLES USED IN VSS COMPONENT
      INTEGER, PARAMETER :: NVSEE=20

! --- TIME VARYING VEG BREAKPOINTS
      INTEGER, PARAMETER  :: NVBP=140

! --- TABLES USED IN ET COMPONENT (MAX. NO. OF PSI/RCF/FET VALUES)
      INTEGER, PARAMETER :: NUZTAB=20

! --- MAXIMUM NUMBER OF SOIL LAYERS + 1
      INTEGER, PARAMETER :: NLYREE=20

! --- OUTPUT SETS (FOR 'RES' FILE OUTPUT)
      INTEGER, PARAMETER :: NSETEE=45

! --- MAXIMUM NUMBER OF ELEMENTS (GRIDS, BANKS AND LINKS) IN A ROW
!      INTEGER, PARAMETER :: NXOCEE=2000
      INTEGER, PARAMETER :: NXOCEE=4*nxee
      
! --- TABLES USED IN OC COMPONENT (MAX. OF NO. OF ROUGHNESS CATEGORIES,
!      NO. OF CHANNEL X-SECTION CATEGORIES, NO. OF OC BOUNDARY ELEMENTS)
      INTEGER, PARAMETER :: NOCTAB=20

! --- SEDIMENT SIZE FRACTIONS
      INTEGER, PARAMETER :: NSEDEE=7

! --- NUMBER OF CONTAMINANTS, NUMBER OF OVERLAPS
      INTEGER, PARAMETER :: NCONEE=3, NOLEE=2*LLEE

! --- NO. OF PLANTS IN AN ELEMENT, TOTAL NO. OF PLANTS, FOR CONTAMINANTS
      INTEGER, PARAMETER :: NPLTEE=NVEE, NPELEE=2
      
      INTEGER, PARAMETER :: max_no_snowmelt_slugs=400

      CHARACTER(256)     :: DIRQQ, filnam, cnam, rootdir   !catchment directory and name
      CHARACTER(256)     :: hdf5filename, visualisation_plan_filename, visualisation_check_filename
      

      INTEGER, PARAMETER :: NXSCEE=100000
!END MODULE AL_P
INTEGER, PARAMETER :: ERRNEE = 100
INTEGER, PARAMETER ::   FFFATAL = 1, &  
                        EEERR = 2, &
                        WWWARN = 3, &
                        pppri  = 23
DOUBLEPRECISION :: UZNOW 
DOUBLEPRECISION, PARAMETER :: marker999=999999.9D0
INTEGER, PARAMETER         :: izero=0, izero1(1)=0, ione=1, ione1(1)=1, imarker=INT(marker999)
DOUBLEPRECISION, PARAMETER :: zero=0.0d0, zero1(1)=0.0d0, half=0.5d0, one=1.0d0, one1(1)=1.0d0, &
                              two=2.0d0, three=3.0d0, five=5.0d0, vsmall=1.0d-20
DOUBLEPRECISION EARRAY(1)
INTEGER            :: ERRC(0:ERRNEE,0:3)=0, ERRTOT=0
CHARACTER(128)     :: helppath
LOGICAL :: ISERROR
LOGICAL :: ISERROR2
LOGICAL :: error_mode

DOUBLEPRECISION, DIMENSION(NELEE) :: cellarea,   &  !cell area
                                     DXQQ, DYQQ, &  !face lengths
                                     ZGRUND         !surface elevation
                                     
CHARACTER(32) :: text32
!PRIVATE
!PUBLIC :: izero, izero1, ione, ione1, zero, zero1, half, one, one1, two, three, five, marker999, &
!          IDIMJE, DIMJE, &
!          ISZERO, ISZERO_A, LTZERO, LEZERO, GEZERO, GTZERO, NOTZERO, ISONE, NOTONE, &
!          EQMARKER, I_ISZERO_A2, fatal, err, warn, pri, &
!          ERROR, ERRC, ERRNEE, HELPPATH, ERRTOT, UZNOW, &
!          cellarea, DXQQ, DYQQ, ZGRUND
CONTAINS


!> Returns whether a double-precision value matches the integer missing-value marker.
LOGICAL FUNCTION eqmarker(a) !needed for ad
DOUBLEPRECISION, INTENT(IN) :: a !! Value to compare with `marker999`.
eqmarker = INT(a)==imarker
END FUNCTION eqmarker

!> Returns whether a value is greater than zero.
LOGICAL FUNCTION gtzero(a)
DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
gtzero = a>zero
END FUNCTION gtzero

!> Returns whether a value is zero within tolerance or greater than zero.
LOGICAL FUNCTION gezero(a)
DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a>=zero
gezero = ISZERO(a) .OR. a>zero
END FUNCTION gezero

!> Returns whether a value is less than zero.
LOGICAL FUNCTION ltzero(a)
DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
ltzero = a<zero
END FUNCTION ltzero

!> Returns whether a value is zero within tolerance or less than zero.
LOGICAL FUNCTION lezero(a)
DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a<=zero
lezero = ISZERO(a) .OR. a<zero
END FUNCTION lezero

!> Returns whether a value is numerically zero using `vsmall`.
LOGICAL FUNCTION iszero(a)
DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a==zero
iszero = ABS(a)<vsmall
END FUNCTION iszero

!> Returns whether all values in a vector are numerically zero.
LOGICAL FUNCTION iszero_a(a)
INTEGER :: i
DOUBLEPRECISION, DIMENSION(:), INTENT(IN) :: a !! Values to test.
iszero_a=.TRUE.
DO i=1,SIZE(a)
    IF(.NOT.iszero_a) CYCLE     !FOR AD
    iszero_a = iszero(a(i))
ENDDO
END FUNCTION iszero_a

!> Returns whether all values in an integer matrix are zero.
LOGICAL FUNCTION i_iszero_a2(a)
INTEGER                             :: i, j
INTEGER, DIMENSION(:,:), INTENT(IN) :: a !! Integer values to test.
i_iszero_a2=.TRUE.
DO i=1,SIZE(a, DIM=1)
    DO j=1,SIZE(a, DIM=2)
        IF(.NOT.i_iszero_a2) CYCLE     !FOR AD
        i_iszero_a2 = a(i,j)==0
    ENDDO
ENDDO
END FUNCTION i_iszero_a2


!> Returns whether a value is not numerically zero.
LOGICAL FUNCTION notzero(a)
DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a/=zero
notzero = .NOT.ISZERO(a)
END FUNCTION notzero

!> Returns whether a value is numerically one using `vsmall`.
LOGICAL FUNCTION isone(a)
DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a==one
isone = ABS(a-one)<vsmall
END FUNCTION isone

!> Returns whether a value is not numerically one.
LOGICAL FUNCTION notone(a)
DOUBLEPRECISION, INTENT(IN) :: a !! Value to test.
!r = a/=one
notone = .NOT.ISONE(a)
END FUNCTION notone


!> Returns `max(x-y,0)` for integer values.
INTEGER FUNCTION idimje(x,y)  !AD PROBLEM
INTEGER, INTENT(IN) :: x !! Left-hand value.
INTEGER, INTENT(IN) :: y !! Right-hand value.
IF(x>y) THEN
    idimje = x-y
ELSE
    idimje = 0
ENDIF
END FUNCTION idimje

!> Returns `max(x-y,0)` for double-precision values.
DOUBLEPRECISION FUNCTION dimje(x,y) !AD PROBLEM
DOUBLEPRECISION, INTENT(IN) :: x !! Left-hand value.
DOUBLEPRECISION, INTENT(IN) :: y !! Right-hand value.
IF(x>y) THEN
    dimje = x-y
ELSE
    dimje = zero
ENDIF
END FUNCTION dimje

!> Reports a SHETRAN error or warning, updates counters, and may stop the run.
!>
!> This is the legacy error-reporting routine used by older components. It
!> formats the message with simulation time and optional element/cell context,
!> records per-module error counts, prints available help-message files in the
!> final summary, and sets timestep-reduction flags for selected flow errors.
SUBROUTINE ERROR(ETYPE, ERRNUM, OUT, IEL, CELL, TEXT)

      ! Assumed global variables provided via host module:
      ! I_P, FFFATAL, EEERR, WWWARN, UZNOW, ERRTOT, ERRC, ERRNEE,
      ! EARRAY, ISERROR, ISERROR2, rootdir, helppath, dirqq

      IMPLICIT NONE

      ! IO-related parameters and variables
      INTEGER(KIND=I_P), INTENT(IN) :: ETYPE  !! The type of error (FFFATAL, EEERR, WWWARN). -999 triggers a help path check.
      INTEGER(KIND=I_P), INTENT(IN) :: ERRNUM !! The unique error number code.
      INTEGER(KIND=I_P), INTENT(IN) :: OUT    !! The output file unit for the message.
      INTEGER(KIND=I_P), INTENT(IN) :: IEL    !! The element number where the error occurred (optional).
      INTEGER(KIND=I_P), INTENT(IN) :: CELL   !! The cell number where the error occurred (optional).
      CHARACTER(LEN=*),  INTENT(IN) :: TEXT   !! The descriptive error text.

      INTEGER(KIND=I_P), PARAMETER :: NONE = 0
      ! Assumes ERRNEE is accessible from host module
      INTEGER(KIND=I_P), PARAMETER :: ERRCEE = (1 + ERRNEE) * 4
      INTEGER(KIND=I_P), PARAMETER :: HLP = 8

      ! Local variables
      CHARACTER(LEN=*), PARAMETER :: PATH1 = '/shetran/'
      CHARACTER(LEN=256) :: FIL,fname
      CHARACTER(LEN=256)  :: HLPMSG
      CHARACTER(LEN=1)   :: cc
      CHARACTER(LEN=1), PARAMETER :: slash = '/'

      INTEGER(KIND=I_P) :: COUNT, ERRN, AMODL
      INTEGER(KIND=I_P) :: IO_STATUS
      INTEGER(KIND=I_P) :: helpcheck !! Status from checking for help directory.

      LOGICAL :: VALID, present

      ! Modernization Fix: Replaced legacy DATA statement with a strict PARAMETER array
      CHARACTER(LEN=11), PARAMETER :: CTYPE(3) = ['FATAL ERROR', '      ERROR', '    WARNING']

      !-------------------------------------------------------------------*

      helppath = '/helpmessages'

      ! SB 07072020 potentially reduce timestep if there are errors 1024,1030,1060
      ISERROR  = .FALSE.
      ISERROR2 = .FALSE.


      ! Write general error message
      ! ---------------------------
      IF (ETYPE >= 1 .AND. ETYPE <= 3) THEN
         IF (ETYPE == FFFATAL) WRITE(OUT, '(//)')

         IF (IEL == 0) THEN
            WRITE(OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW
         ELSE IF (CELL == 0) THEN
            WRITE(OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW, IEL
         ELSE
            WRITE(OUT, 9100) CTYPE(ETYPE), ERRNUM, UZNOW, IEL, CELL
         END IF
      END IF

      WRITE(OUT, '(8X,A)') TEXT

      ! Decompose ERRNUM and update counters
      ! ------------------------------------
      IF (ETYPE /= NONE) THEN
         ERRTOT = ERRTOT + 1
         AMODL  = ERRNUM / 1000
         ERRN   = MOD(ERRNUM, 1000)

         VALID  = (AMODL >= 0 .AND. AMODL <= 3 .AND. ERRN >= 0 .AND. ERRN <= ERRNEE)
         IF (VALID) ERRC(ERRN, AMODL) = ERRC(ERRN, AMODL) + 1
      END IF

      ! Write specific error messages
      ! -----------------------------
      IF (ERRNUM == 1003) THEN
         WRITE(OUT, 91003) EARRAY(1)
         ! 970804
      ELSE IF (ERRNUM == 1024) THEN
         WRITE(OUT, 91024) EARRAY(1)
         !
      END IF

      ! SB 07072020 reduce timestep if there are errors 1024,1030,1060
      IF (ERRNUM == 1024 .OR. ERRNUM == 1030) THEN
         ISERROR = .TRUE.
      END IF
      IF (ERRNUM == 1060) THEN
         ISERROR2 = .TRUE.
      END IF

      ! Write summary
      ! -------------
      IF (ETYPE == FFFATAL .OR. ERRNUM == 0) THEN
          WRITE(*,'(/,A,/,A,/)') &
              ' ### Error Summary and Advice ###', &
              '     ------------------------' 
          WRITE(OUT,'(/,A,/,A,/)') &
              ' ### Error Summary and Advice ###', &
              '     ------------------------'
          inquire(out,name=fname)

         IF (ERRTOT > 0) WRITE(*, '(A,A,A/)') ' ==> Check the pri file: "', trim(fname), '" for more details <=='

         module_loop: DO AMODL = 0, 3
            error_loop: DO ERRN = 0, ERRNEE
               COUNT = ERRC(ERRN, AMODL)

               IF (COUNT > 0) THEN
                  ! Print number of occurrences
                  WRITE(*, 9500) ERRN + AMODL * 1000, COUNT
                  WRITE(OUT, 9500) ERRN + AMODL * 1000, COUNT
                  WRITE(*, *) 
                  WRITE(OUT, *) 


                  ! Print contents of help file (if any)
                  WRITE(FIL, 9200) TRIM(rootdir) // TRIM(helppath) // '/', AMODL, ERRN, '.txt'
                  OPEN(HLP, FILE=FIL, STATUS='OLD', IOSTAT=IO_STATUS)
                  IF (IO_STATUS == 0) THEN
                      read_help: DO
                          READ(HLP, '(A)', IOSTAT=IO_STATUS) HLPMSG
                          IF (IO_STATUS /= 0) EXIT read_help
                          WRITE(*, '(A)') trim(HLPMSG)
                          WRITE(OUT, '(A)') trim(HLPMSG)
                      END DO read_help
                      CLOSE(HLP)
                  END IF

                  WRITE(*, *)
                  WRITE(OUT, *)
               END IF
            END DO error_loop
         END DO module_loop

         WRITE(*, 9600) ERRTOT

      END IF

      ! Stop?
      ! -----
      IF (ETYPE == FFFATAL) CALL ALSTOP(1)

      ! String format statements
      ! ------------------------
9100  FORMAT(/ ' !!!', A, I5.4, ' at time =', F12.2, ' hours': &
      &        ', iel =', I6:', cell =', I5 )
9200  FORMAT(A,I1,I3.3,A)
9500  FORMAT(' No. of occurrences of error number',I5.4,' is',I6)
9600  FORMAT(/' ### End of summary: recorded error count is',I7,' ###'/)
91003 FORMAT(' MAXIMUM DIFFERENCE (DHMAX) = ',G12.6,' METRES')
! 970804
91024 FORMAT(' DEPTH OF SURFACE WATER BELOW GROUND = ',G12.6,' METRES')
!
   END SUBROUTINE ERROR


   !> Stops the program, typically after a fatal error.
   !>
   !> This subroutine is called to stop the program, typically after a fatal
   !> error. It provides a final message to the user before termination.
   !>
   !> @history
   !> | Date | Author | Description |
   !> |:----:|:------:|-------------|
   !> | 1994-09-17 | RAH | v3.4.1: File created. |
   !> | 2000-03-07 | SB | v4g-pc: Removed IEEE calls for PC version. |
   SUBROUTINE ALSTOP (FLAG)
      INTEGER(KIND=I_P), INTENT(IN) :: FLAG !! A flag indicating the reason for stopping. If > 0, it's a fatal error.
      ! if error_mode is true then there is no need to press enter to continue

      IF (FLAG.GT.0) THEN
          if (error_mode) then
              STOP 'Program terminating due to fatal error'
          else
              WRITE(*, '(A)') 'FATAL ERROR: Program will terminate. Press Enter to exit...'
              READ(*,*)
              STOP 'Program terminating due to fatal error'
          endif
      ENDIF
   END SUBROUTINE ALSTOP
    
    
END MODULE sglobal
