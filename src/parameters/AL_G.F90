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

! ----- INTEGER VARIABLES
!
      INTEGER ::  NX, NY, & !! Number of grid squares in the x and y directions.
                  NGDBGN    !! Element number at which grid elements begin.
! ----- INTEGER ARRAYS
!
      INTEGER         ICMREF(NELEE,12), &
                      ICMXY(NXEE,NYEE)  !! Grid-to-element lookup array.
!PRIVATE :: NELEE, NXEE, NYEE
END MODULE AL_G
