!> summary: Column water-flow scaling and numbering variables.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> This module replaces the legacy `COLM.C1` common blocks. It stores scale
!> references, finite-difference constants, and column-numbering values used by
!> the `COLM` water and contaminant column calculations.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.1 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1991-06-16 | JE | 3.1 | Removed references to `LNCONT` and `LNSOIL`. |
!> | 1991-07-17 | JE | 3.1 | Reordered names in `CLNUM`. |
!> | 1991-08-26 | JE | 3.1 | Moved parameter `NCETOP` to `BLOCKCLNUM`. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing and split mixed-type `CLNUM`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE COLM_C1
   IMPLICIT NONE
   DOUBLEPRECISION :: D0     !! Reference diffusion scale for contaminant column equations.
   DOUBLEPRECISION :: Z2     !! Reference column depth used to nondimensionalise vertical geometry.
   DOUBLEPRECISION :: Z2SQ   !! Squared reference depth, `Z2**2`.
   DOUBLEPRECISION :: Z2OD   !! Reference depth divided by the diffusion scale, `Z2/D0`.
   DOUBLEPRECISION :: Z2SQOD !! Squared reference depth divided by the diffusion scale, `Z2**2/D0`.

   DOUBLEPRECISION :: CST1   !! Column convection coefficient, `Z2/(AREA*D0*ZONE1)`.
   DOUBLEPRECISION :: CST2   !! Column area-scaling coefficient, `Z2/(AREA*D0)`.
   DOUBLEPRECISION :: CST3   !! Bottom-cell convection coefficient, `CST2/KSP(NCEBOT)`.
   DOUBLEPRECISION :: SGMA   !! Implicit finite-difference weighting factor.
   DOUBLEPRECISION :: SGSQ   !! Squared finite-difference weighting factor, `SGMA**2`.
   DOUBLEPRECISION :: SGTSE  !! Sigma-weighted scaled timestep, `SGMA*TSE`.
   DOUBLEPRECISION :: SGSTSE !! Squared-sigma weighted scaled timestep, `SGSQ*TSE`.
   DOUBLEPRECISION :: OMSGMA !! Explicit finite-difference weighting complement, `1-SGMA`.
   DOUBLEPRECISION :: OPSGL  !! Liquid-phase storage factor, `1+SGTSE*GCAPLA`.
   DOUBLEPRECISION :: OPSGSL !! Sorbed-phase storage factor, `1+SGSTSE*GCAPLA`.
   DOUBLEPRECISION :: TSE    !! Scaled contaminant timestep, `D0*DTUZ/Z2SQ`.

   DOUBLEPRECISION :: FNCPSF !! Fraction of the highest cell below the phreatic surface.

   INTEGER :: NCEBOT !! Bottom active column cell index.
   INTEGER :: NCETOP !! Top active column cell index.
   INTEGER :: NCEPSF !! Highest active cell treated as below the phreatic surface.

END MODULE COLM_C1
