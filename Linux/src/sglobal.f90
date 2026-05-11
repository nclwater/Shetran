MODULE sglobal
! JE  1/09   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the al_p etc
!USE BUFF_DISK
   USE MOD_PARAMETERS, ONLY : I_P, R8P, LENGTH_FILEPATH
IMPLICIT NONE
!MODULE AL_P
!IMPLICIT NONE
!-------------------- START OF AL.P -----------------------------------*
!
!     Distributed constants for all components (mainly array sizes)
!
!       THIS FILE CAN BE TAILORED TO SUIT A PARTICULAR CATCHMENT
!
!----------------------------------------------------------------------*
! Version:  AL_P.F95/4.30
! Modifications:
!   GP  FEB 89  2.0     'SHE88' IMPLEMENTATION ON NEWCASTLE AMDAHL
!   GP  MAR 89  2.1     ADD NREFE8 FOR NEW SZ DRAIN VARIABLES
!                       + ADD DIMENSION NSZBOU (NO. OF SZ BNDRY PNTS)
!                       + ADD DERIVED DIMN. NSZB40
!   GP  APR 89  2.2     INTEGRATE SED. YIELD
!                       + ADD OVERALL VERSION NUMBER
!   GP  MAR 90  3.0     ADD DEVELOPMENT VERSION FLAG
!                       + FURTHER DIMENSION VARIABLES
!   GP  JAN 92  3.3     ADD NVBP AND CHANGE DEFINITIONS OF NUZTAB/NOCTAB
!   GP          3.4     Add NPLTEE,NPELEE.  Update SHEVER,BANNER,
!                       NELEE,NLFEE,LLEE,NVEE,NSEE,NLYREE,NUZTAB.
!  RAH  30.09.94  Version 3.4.1 by AB/RAH, adapted from version 3.4:
!                  no INTEGER*2; declare all types; amend BANNER,
!                  BDEVER,NXEE,NYEE,NLFEE,LLEE,NSEE,NXOCEE,NSEDEE;
!                  standard header; move amendment history to separate
!                  file; set NELEE=NXEE*NYEE; alter comments;
!                  characters size (*); no IMPLICIT statements.
!  GP  961024  4.0  Alter NELEE,NXEE,NYEE,NLFEE,LLEE,NVEE,NSEE,NVBP,
!                   NUZTAB,NLYREE,NSETEE,NXOCEE,NSEDEE,NCONEE.
!                   Add NRDEE,NVSEE for new VSS module.
!                   NUZTAB is now for ET only (was UZ too).
! RAH  970117       Update SHEVER,BDEVER,BANNER.
! RAH  970218  4.1  Remove NRDEE (redundant).  Set SHEVER,BDEVER,BANNER.
!      970220       Restore history.
! RAH  980220  4.2  Update SHEVER,BANNER.  Remove NWELEE,NSZBOU,NPSITH.
!  JE  JULY 04 ---  Convert to FORTRAN 95, as part of integration of SHEGRAPH Version 2
! SB Mar 26  4.6   Increase array sizes now all the 2 and 3D arrays are allocatable
!                   NXOCEE=4*nxee

!----------------------------------------------------------------------*

!*970218 TEMPORARY!  REMOVED TO temporary.f90 je 170704
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


!SSSSSS LOGICAL FUNCTION eqmarker
LOGICAL FUNCTION eqmarker(a) !needed for ad
DOUBLEPRECISION, INTENT(IN) :: a
eqmarker = INT(a)==imarker
END FUNCTION eqmarker

!SSSSSS LOGICAL FUNCTION gtzero
LOGICAL FUNCTION gtzero(a)
DOUBLEPRECISION, INTENT(IN) :: a
gtzero = a>zero
END FUNCTION gtzero

!SSSSSS LOGICAL FUNCTION gezero
LOGICAL FUNCTION gezero(a)
DOUBLEPRECISION, INTENT(IN) :: a
!r = a>=zero
gezero = ISZERO(a) .OR. a>zero
END FUNCTION gezero

!SSSSSS LOGICAL FUNCTION ltzero
LOGICAL FUNCTION ltzero(a)
DOUBLEPRECISION, INTENT(IN) :: a
ltzero = a<zero
END FUNCTION ltzero

!SSSSSS LOGICAL FUNCTION lezero
LOGICAL FUNCTION lezero(a)
DOUBLEPRECISION, INTENT(IN) :: a
!r = a<=zero
lezero = ISZERO(a) .OR. a<zero
END FUNCTION lezero

!SSSSSS LOGICAL FUNCTION iszero
LOGICAL FUNCTION iszero(a)
DOUBLEPRECISION, INTENT(IN) :: a
!r = a==zero
iszero = ABS(a)<vsmall
END FUNCTION iszero

!SSSSSS LOGICAL FUNCTION iszero_a
LOGICAL FUNCTION iszero_a(a)
INTEGER :: i
DOUBLEPRECISION, DIMENSION(:), INTENT(IN) :: a
iszero_a=.TRUE.
DO i=1,SIZE(a)
    IF(.NOT.iszero_a) CYCLE     !FOR AD
    iszero_a = iszero(a(i))
ENDDO
END FUNCTION iszero_a

!SSSSSS LOGICAL FUNCTION i_iszero_a2
LOGICAL FUNCTION i_iszero_a2(a)
INTEGER                             :: i, j
INTEGER, DIMENSION(:,:), INTENT(IN) :: a
i_iszero_a2=.TRUE.
DO i=1,SIZE(a, DIM=1)
    DO j=1,SIZE(a, DIM=2)
        IF(.NOT.i_iszero_a2) CYCLE     !FOR AD
        i_iszero_a2 = a(i,j)==0
    ENDDO
ENDDO
END FUNCTION i_iszero_a2


!SSSSSS LOGICAL FUNCTION notzero
LOGICAL FUNCTION notzero(a)
DOUBLEPRECISION, INTENT(IN) :: a
!r = a/=zero
notzero = .NOT.ISZERO(a)
END FUNCTION notzero

!SSSSSS LOGICAL FUNCTION isone
LOGICAL FUNCTION isone(a)
DOUBLEPRECISION, INTENT(IN) :: a
!r = a==one
isone = ABS(a-one)<vsmall
END FUNCTION isone

!SSSSSS LOGICAL FUNCTION notone
LOGICAL FUNCTION notone(a)
DOUBLEPRECISION, INTENT(IN) :: a
!r = a/=one
notone = .NOT.ISONE(a)
END FUNCTION notone


!FFFFFF INTEGER FUNCTION IDIMJE(a,b)
INTEGER FUNCTION idimje(x,y)  !AD PROBLEM
INTEGER, INTENT(IN) :: x, y
IF(x>y) THEN
    idimje = x-y
ELSE
    idimje = 0
ENDIF
END FUNCTION idimje

!FFFFFF DOUBLEPRECISION FUNCTION dimje(a,b)
DOUBLEPRECISION FUNCTION dimje(x,y) !AD PROBLEM
DOUBLEPRECISION, INTENT(IN) :: x, y
IF(x>y) THEN
    dimje = x-y
ELSE
    dimje = zero
ENDIF
END FUNCTION dimje

!SSSSSS SUBROUTINE ERROR 
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