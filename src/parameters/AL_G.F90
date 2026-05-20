!> summary: Grid geometry and element indexing state.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University
!>
!> `AL_G` contains the core grid dimensions and lookup arrays that map between
!> SHETRAN grid positions, element numbers, and element reference attributes.
!> The arrays are shared by several model components during setup and process
!> calculations.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-03 | GP | 3.0 | Original version written. |
!> | 1994-10-01 | RAH | 3.4.1 | Declared all variables, removed `INTEGER*2`, tidied comments, and applied standard header. |
!> | 1998-03-07 | RAH | 4.2 | Cosmetic updates. |
!> | 2004-07 | JE | - | Converted to Fortran 95. |
!> @endhistory
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

