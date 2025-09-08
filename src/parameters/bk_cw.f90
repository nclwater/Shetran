!> summary: Defines water-related variables for bank-channel interaction.
!> author: J. Ewen, G. Parkin, R. A. Heath (Newcastle University)
!> date: 2008-12-01
!>
!> This module contains variables used to manage the interaction between
!> river banks and channel links, specifically for water flow calculations.
!> It includes mapping arrays for bank connections and cell indices.
!>
!> @note This module is a legacy component derived from an old `INCLUDE` file.
!> The refactoring plan in `docs/reports/refactoR8Parameters/plan_parameter_refactor.md`
!> outlines renaming this module to `bank_wateR8Params.f90` and moving it to
!> `src/parameters/components/`.
!>
! @history
!> | Date | Author | Description |
!> |:----:|:------:|-------------|
!> | 1991-06-13 | JE | v3.1: Written. |
!> | 1991-06-16 | JE | v3.1: Corrected syntax errors. |
!> | 1991-07-17 | JE | v3.1: Reordered names in BKOL and LFBK. |
!> | 1995-03-10 | GP | v4.0: Replaced BFSCL with BK* variables. |
!> | 1997-02-19 | RAH | v4.1: Removed redundant variables. Explicit typing. Separated /LFBKI/. |
!> | 1997-02-20 | RAH | v4.1: Amended descriptions of NCEBD, FNCEBD. |
!> | 1998-03-08 | RAH | v4.2: Removed OLBD. |
!> | 2008-12-01 | JE | v4.3.5: Converted to Fortran 90. |
!> | 2024-09-05 | AI | Added KIND parameters and FORD docs. |
MODULE BK_CW

   USE SGLOBAL, ONLY : NLFEE, LLEE
   USE MOD_PARAMETERS, ONLY : I_P, R8P

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: FNCEBD, NBANK, NCEAB, NCEBD, QQRVO

! Imported constants
!                      LLEE,NLFEE
   REAL(KIND=R8P), DIMENSION(NLFEE, 2) :: FNCEBD !! Fraction of cell NCEBD+1 which lies below the bed deep layer.

!COMMON / LFBK / FNCEBD
!                             FRACTION OF CELL NCEBD+1 WHICH LIES
!                             BELOW THE BED DEEP LAYER
   INTEGER(KIND=I_P), DIMENSION(NLFEE, 2) :: NBANK !! Number for the bank adjacent to a link.
   INTEGER(KIND=I_P), DIMENSION(NLFEE, 2) :: NCEAB !! Number for the lowest cell to exchange water with the stream.
   INTEGER(KIND=I_P), DIMENSION(NLFEE, 2) :: NCEBD !! Number of the highest cell which lies fully below the bed deep layer.

!COMMON / LFBKI / NBANK, NCEAB, NCEBD
!                             NUMBER FOR THE BANK ADJACENT TO A LINK;
!                             NUMBER FOR THE LOWEST CELL TO EXCHANGE
!                             WATER WITH THE STREAM;
!                             NUMBER OF THE HIGHEST CELL WHICH LIES
!                             FULLY BELOW THE BED DEEP LAYER
   REAL(KIND=R8P), DIMENSION(NLFEE, LLEE, 2) :: QQRVO !! Not currently used, candidate for review/removal.

END MODULE BK_CW
