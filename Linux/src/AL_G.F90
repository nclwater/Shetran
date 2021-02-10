MODULE AL_G
USE SGLOBAL, ONLY : NELEE, NXEE, NYEE
IMPLICIT NONE
!-------------------- START OF AL.G -----------------------------------*
!
!  Include file for grid-related variables used in all components
!
!----------------------------------------------------------------------*
! Version:  AL_G.F95/4.3
! Modifications:
!   GP        MAR 91    3.0     WRITTEN
!  RAH  01.10.94  Version 3.4.1 from version 3.4: declare everything;
!                  no INTEGER*2; tidy comments; standard header.!
! RAH  980307  4.2  Cosmetics.
!  JE  JULY 04 ---  Converted to FORTRAN 95
!----------------------------------------------------------------------*

! ----- INTEGER VARIABLES
!
      INTEGER ::  NX, NY, & ! no.grid squares in x and y directions
                  NGDBGN    !1st grid element no. ????
! ----- INTEGER ARRAYS
!
      INTEGER         ICMREF(NELEE,12), &
                      ICMXY(NXEE,NYEE)  !grid element no ???? -DONT KNOW
!PRIVATE :: NELEE, NXEE, NYEE
END MODULE AL_G
