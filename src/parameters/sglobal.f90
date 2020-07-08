MODULE sglobal
! JE  1/09   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the al_p etc
!USE BUFF_DISK
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
!----------------------------------------------------------------------*

!*970218 TEMPORARY!  REMOVED TO temporary.f90 je 170704
!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
!      IMPLICIT INTEGER (I-N)

!------------ SHE VERSION NUMBER

!     (MUST BE IN FORMAT XX.Y WHERE XX = MAJOR PART OF VERSION NUMBER,
!                                    Y = MINOR PART )
      DOUBLEPRECISION, PARAMETER :: SHEVER=4.4
!
!------------ DEVELOPMENT VERSION FLAG

!     SET TO .TRUE. FOR DEVELOPMENT VERSION OF CODE
!     SET TO .FALSE. FOR FINAL STATIC VERSION
      LOGICAL, PARAMETER :: BDEVER=.TRUE.

!------------ BANNER HEADER
!     Description of local implementation: software, architecture, date
      CHARACTER(*), PARAMETER ::BANNER='SHETRAN Version 4.4 for PC - incorporating SHEGRAPH Version 2'

!------------ RUNDATA FILENAME (THE CATCHMENT NAME ENTERED AT THE START
!     OF A SIMULATION RUN IS APPENDED TO THIS FILENAME)
      CHARACTER(*), PARAMETER :: RUNFIL='rundata_'

!------------ ARRAY SIZES (must be greater than zero!)

! --- Grid points in x,y directions, river links, total no of elements
!16 Sep 94  NB  INFR references elements LCODEX(NX+1) and LCODEY(NY+1)!
!30 Sep 94  NB  NELEE is also used as size of workspace arrays.
!Jan 2009   JE  this link broken - it wastes memory - workspace now set separately
      !INTEGER, PARAMETER :: nxee=200, nyee=200, nlfee=10000, nelee=40000  !sv4 large
      INTEGER, PARAMETER :: nxee=400, nyee=400, nlfee=10000, nelee=40000  !sv4 large
      !INTEGER, PARAMETER :: nxee=35, nyee=40, nlfee=240, nelee=900  !Dunsop200
      !INTEGER, PARAMETER :: NXEE=40, NYEE=40, NLFEE=132, NELEE=320  !Cobres
      !INTEGER, PARAMETER :: NXEE=12, NYEE=12, NLFEE=4, NELEE=30
      !INTEGER, PARAMETER :: NXEE=40, NYEE=40, NLFEE=40, NELEE=800  !slapton
      INTEGER            :: total_no_elements=-1, total_no_links=-1, top_cell_no=-1, szmonte=-1, &
                            ran2monte1=-1, ran2monte2=-1, pcmonte=-1
      INTEGER(1), DIMENSION(:,:), ALLOCATABLE :: montec
      
! --- GRID POINTS IN VERTICAL PLUS ONE
      !INTEGER, PARAMETER :: LLEE=50
      INTEGER, PARAMETER :: LLEE=50  !Cobres

! --- VEGETATION TYPES, SOIL TYPES
      INTEGER, PARAMETER :: NVEE=200, NSEE=1000

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
      INTEGER, PARAMETER :: NXOCEE=500

! --- TABLES USED IN OC COMPONENT (MAX. OF NO. OF ROUGHNESS CATEGORIES,
!      NO. OF CHANNEL X-SECTION CATEGORIES, NO. OF OC BOUNDARY ELEMENTS)
      INTEGER, PARAMETER :: NOCTAB=20

! --- SEDIMENT SIZE FRACTIONS
      INTEGER, PARAMETER :: NSEDEE=7

! --- NUMBER OF CONTAMINANTS, NUMBER OF OVERLAPS
      INTEGER, PARAMETER :: NCONEE=3, NOLEE=2*LLEE

! --- NO. OF PLANTS IN AN ELEMENT, TOTAL NO. OF PLANTS, FOR CONTAMINANTS
      INTEGER, PARAMETER :: NPLTEE=NVEE, NPELEE=2
      
      INTEGER, PARAMETER :: max_no_snowmelt_slugs=50

      CHARACTER(256)     :: DIRQQ, filnam, cnam, rootdir   !catchment directory and name
      CHARACTER(256)     :: hdf5filename, visualisation_plan_filename, visualisation_check_filename
      

!      INTEGER, PARAMETER :: NXSCEE=100000
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
SUBROUTINE ERROR (ETYPE, ERRNUM, OUT, IEL, CELL, TEXT)  

!""USE DFLIB, ONLY:FULLPATHQQ, GETDRIVEDIRQQ, GETCHARQQ  
!----------------------------------------------------------------------*
!
! Print a message or error asummary (and optionally stop the program)
!
!----------------------------------------------------------------------*
! Version:  3.4.1       Notes:  SSR72
!  Module:  AL        Program:  SHETRAN
! Modifications:
!  RAH  08.10.94  Version 3.4.1 by AB/RAH from version 3.4:
!                  replace common counter arrays with local ERRC;
!                  extend ERRNUM range below 1000; introduce ETYPE=0;
!                  print IEL, CELL only if non-zero; print help files
!                  along with final asummary; declare everything;
!                  no INTEGER*2; test subscript ranges; tidy comments;
!                  call ALSTOP to stop; use local IFATAL etc instead
!                  of common FATAL etc; 1024 no longer uses EARRAY.
! RAH  970804  4.1  (Use EARRAY for 1024.)
! RAH  970811       (EXTERNAL after INCLUDE.)
!----------------------------------------------------------------------*
! Commons and constants
IMPLICIT NONE
! Imported constants
!   AL.P : SHEVER
! Input common
!   AL.C : UZNOW, EARRAY(1)
! Input arguments
INTEGER :: ERRNUM  
INTEGER :: ETYPE, OUT, IEL, CELL  

CHARACTER (LEN=*) :: TEXT  
! Locals, etc
INTEGER :: NONE, ERRCEE, HLP  
PARAMETER (NONE = 0, ERRCEE = (1 + ERRNEE) * 4)  
PARAMETER (HLP = 8)  
CHARACTER (LEN=*) :: PATH1  
PARAMETER (PATH1 = '/shetran/')  
INTEGER :: COUNT, ERRN, AMODL  
CHARACTER (11) :: CTYPE (3)
CHARACTER(256) :: FIL
CHARACTER(80)  :: HLPMSG  
LOGICAL :: VALID  

DATA CTYPE / 'FATAL ERROR', '      ERROR', '    WARNING' /  
!----------------------------------------------------------------------*
INTEGER :: helpcheck  
CHARACTER :: cc
character, parameter :: slash='/'
 
LOGICAL :: present  

helppath = '\helpmessages'  

!**SB 07072020 reduce timestep if there are errors 1024,1030,1060
ISERROR = .FALSE.
ISERROR2 = .FALSE.

IF (ETYPE == - 999) THEN  
   present = .TRUE.;
   !helppath = ''  
                                                             !nett 09080
   !""helpcheck = FULLPATHQQ ('helpmessages', helppath)
helpcheck = 60
          ! IF(helpcheck/=0) INQUIRE(FILE=helppath, EXIST=present)
          !IF(.NOT.present) THEN
                                 !nett 090805
   IF (helpcheck == 0) THEN  
      PRINT * , "Failedtofindthe'helpmessages'directory"  
    PRINT * , '  (which contains the help message files)'  
      PRINT * , "Itsnamemustbe'helpmessages'"  
      !""helpcheck = GETDRIVEDIRQQ (helppath)  
      IF (helpcheck /= 0) THEN  
         !!PRINT *, "anditmustbein"//TRIM (helppath)  
      ENDIF  
      PRINT * , "Type's'tostopor'c'tocontinue"  
      !""cc = GETCHARQQ ()
      cc = 'c'
      DO WHILE (cc /= 'c'.AND.cc /= 's'.AND.cc /= 'C'.AND.cc /= 'S')
      !""cc = GETCHARQQ ()  
      ENDDO  
      IF (cc == 's'.OR.cc == 'S') STOP  
   ENDIF  
   RETURN  


ENDIF  
! Write general error message
! ---------------------------
IF (ETYPE.GE.1.AND.ETYPE.LE.3) THEN  
   IF (ETYPE.EQ.FFFATAL) WRITE (OUT, '(//)')  
   IF (IEL.EQ.0) THEN  
      WRITE (OUT, 9100) CTYPE (ETYPE), ERRNUM, UZNOW  
   ELSEIF (CELL.EQ.0) THEN  
      WRITE (OUT, 9100) CTYPE (ETYPE), ERRNUM, UZNOW, IEL  
   ELSE  
      WRITE (OUT, 9100) CTYPE (ETYPE), ERRNUM, UZNOW, IEL, CELL  
   ENDIF  
ENDIF  



WRITE (OUT, '(8X,A)') TEXT  
! Decompose ERRNUM and update counters
! ------------------------------------
IF (ETYPE.NE.NONE) THEN  
   ERRTOT = ERRTOT + 1  
   AMODL = ERRNUM / 1000  
   ERRN = MOD (ERRNUM, 1000)  
   VALID = &
    AMODL.GE.0.AND.AMODL.LE.3.AND.ERRN.GE.0.AND.ERRN.LE.ERRNEE
   IF (VALID) ERRC (ERRN, AMODL) = ERRC (ERRN, AMODL) + 1  



ENDIF  
! Write specific error messages
! -----------------------------
IF (ERRNUM.EQ.1003) THEN  
   WRITE (OUT, 91003) EARRAY (1)  
!*970804
ELSEIF (ERRNUM.EQ.1024) THEN  
   WRITE (OUT, 91024) EARRAY (1)  
!*



ENDIF  


!**SB 07072020 reduce timestep if there are errors 1024,1030,1060
IF ((ERRNUM.EQ.1024).OR.(ERRNUM.EQ.1030)) THEN
    ISERROR=.TRUE.
ENDIF
IF (ERRNUM.EQ.1060) THEN
    ISERROR2=.TRUE.
ENDIF
! Write asummary
! -------------

IF (ETYPE.EQ.FFFATAL.OR.ERRNUM.EQ.0) THEN  
WRITE ( * , '(//A/A/)') ' ### Error asummary and Advice ###', '  ------------------------'

IF (ERRTOT.GT.0) WRITE ( * , '(A/)') ' ==> Check printed output files for more details <=='
   DO 50 AMODL = 0, 3  
      DO 10 ERRN = 0, ERRNEE  
         COUNT = ERRC (ERRN, AMODL)  

         IF (COUNT.GT.0) THEN  
!             * Print number of occurrences

            WRITE ( *, 9500) ERRN + AMODL * 1000, COUNT  
!             * Print contents of help file (if any)
            WRITE (FIL, 9200) trim(rootdir)//TRIM (helppath) //'\', AMODL, ERRN  
            print*,dirqq,rootdir
            print*,fil
            pause
            OPEN (HLP, FILE = FIL, STATUS = 'OLD', ERR = 7)  
    5             READ (HLP, '(A)', ERR = 7, END = 7) HLPMSG  
            WRITE ( * , '(A)') HLPMSG  
            GOTO 5  
    7             CLOSE (HLP)  

            WRITE ( *, * )  
         ENDIF  
   10       END DO  
   50    END DO  

   WRITE ( *, 9600) ERRTOT  



ENDIF  
! Stop?
! -----

IF (ETYPE.EQ.FFFATAL) CALL ALSTOP (1)  
 9100 FORMAT(/ ' !!!', A, I5.4, ' at time =', F12.2, ' hours': &
&         ', iel =', I5:', cell =', I5 )
 9200 FORMAT(A,I1,I3.3)  
 9500 FORMAT(' No. of occurrences of error number',I5.4,' is',I6)  
 9600 FORMAT(/' ### End of asummary: recorded error count is',I7,' ###'/)  
91003 FORMAT(' MAXIMUM DIFFERENCE (DHMAX) = ',G12.6,' METRES')  
!*970804
91024 FORMAT(' DEPTH OF SURFACE WATER BELOW GROUND = ',G12.6,' METRES')  
!*
END SUBROUTINE ERROR

!SSSSSS SUBROUTINE ALSTOP (FLAG)  
SUBROUTINE ALSTOP (FLAG)  
!
!----------------------------------------------------------------------*
!
! Perform any system-level tasks and terminate the program.
!
!----------------------------------------------------------------------*
! Version:  3.4.1 (sol/sun)    Notes:  SSR80
!  Module:  AL               Program:  SHETRAN
! Modifications:
!  RAH  30.09.94  Version 3.4.1.  File created 17.09.94.
!  SB 7/3/00 Version 4g-pc remove ieee calls
!----------------------------------------------------------------------*
!
! Input arguments
INTEGER :: FLAG  
!
!
!      CALL IEEE_FLAGS( 'clear', 'exception', 'all', OUT )
!
IF (FLAG.GT.0) THEN  
   PAUSE  
   STOP 'Program terminating due to fatal error'  
ENDIF  
!!!STOP  
!
END SUBROUTINE ALSTOP
END MODULE sglobal