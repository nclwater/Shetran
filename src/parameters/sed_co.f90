!> summary: Defines old sediment variables for preparing COLM and LINK subroutines.
!> author: J. Ewen
!> date: 2008-08-12
!>
!> This module contains "old" values of sediment-related variables used in
!> preparation for running the COLM and LINK subroutines. These variables
!> store state from the previous timestep, such as sediment depth and erosion rates.
!> It was converted from a legacy `INCLUDE` file.
!>
!> @history
!> | Date       | Author | Version  | Description                               |
!> |:-----------|:-------|:---------|:------------------------------------------|
!> | 1991-04-?? | JE     | 3.0      | Original `INCLUDE` file written.          |
!> | 1991-06-13 | JE     | 3.1      | Completed.                                |
!> | 1997-03-14 | RAH    | 4.1      | Explicit typing.                          |
!> | 2008-08-12 | JE     | 4.3.5F90 | Converted to Fortran 90 module.           |
!> | 2025-08-11 | AI     | -        | Added FORD docs, modernized declarations. |
MODULE SED_CO

   USE SGLOBAL, ONLY : NELEE, NLFEE, NSEDEE
   USE MOD_PARAMETERS, ONLY: R8P
   IMPLICIT NONE

   ! --------------------------------------------------------------------------
   ! Public variables
   PUBLIC :: DLSO, GNUO
   PUBLIC :: FBETAO, FDELO
   PUBLIC :: FBBEDO, FBTSDO

   ! Code =====================================================================

   ! The legacy COMMON block /SDDEPO/ was replaced by these module variables.
   REAL(KIND=R8P) :: DLSO(NELEE)   !! Old value of depth of loose sediment.
   REAL(KIND=R8P) :: GNUO(NELEE)   !! Old value of the rate of erosion of the unsaturated zone.

   REAL(KIND=R8P) :: FBETAO(NELEE, NSEDEE)
   REAL(KIND=R8P) :: FDELO(NELEE, NSEDEE)
   REAL(KIND=R8P) :: FBBEDO(NLFEE, NSEDEE)
   REAL(KIND=R8P) :: FBTSDO(NLFEE, NSEDEE)

end MODULE SED_CO
