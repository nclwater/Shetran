!> summary: Scaling and active-cell state for the contaminant column solver.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> `COLM_C1` replaces the legacy `COLM.C1` common blocks. It combines fixed
!> nondimensionalisation references with mutable timestep, column, and
!> contaminant state used by the column transport routines.
!>
!> | Producer | Shared values prepared | Update point |
!> |:---------|:-----------------------|:-------------|
!> | [[frmod:incm]] | `D0`, `Z2`, their derived scales, `SGMA`, `SGSQ`, `OMSGMA`, and `NCETOP` | Contaminant initialisation. |
!> | [[cmmod:cmsim]] | `TSE` | Once per contaminant timestep. |
!> | [[cmmod:colmw]] | `SGTSE`, `SGSTSE`, `NCEBOT`, `NCEPSF`, and `CST1:CST3` | Before solving each column. |
!> | [[cmmod:colmsm]] | `OPSGL` and `OPSGSL` | Before [[cmmod:colm]] for each contaminant. |
!>
!> `CMSIM` processes elements sequentially, so the per-column and
!> per-contaminant scalars describe only the solver state most recently
!> prepared by `COLMW` and `COLMSM`; they are not independent per-column data.
!> `INCM` currently fixes the reference dispersion coefficient `D0` at
!> \(10^{-3}\,\mathrm{m^2\,s^{-1}}\), the reference column length `Z2` at 50 m,
!> and `SGMA` at one, giving a fully implicit finite-difference weighting.
!>
!> `NCEPSF` is a legacy phreatic-surface name. Current `COLMW` always sets it
!> to `NCETOP`, allowing lateral transport through the ground-surface cell.
!> `FNCPSF` has no current producer or consumer and must not be assumed valid.
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
   DOUBLEPRECISION D0      !! Reference dispersion coefficient, currently `1.0D-3` m2/s.
   DOUBLEPRECISION Z2      !! Reference soil-column length, currently 50 m.
   DOUBLEPRECISION Z2SQ    !! Squared reference length, `Z2*Z2` (m2).
   DOUBLEPRECISION Z2OD    !! Reference length divided by dispersion scale, `Z2/D0` (s/m).
   DOUBLEPRECISION Z2SQOD  !! Reference dispersive timescale, `Z2SQ/D0` (s).

   DOUBLEPRECISION CST1    !! Lateral-flow scaling, `CST2/ZONE1`, for the current column.
   DOUBLEPRECISION CST2    !! Surface-flow scaling, `Z2/(AREA(NCL)*D0)`, for the current column.
   DOUBLEPRECISION CST3    !! Base-flow scaling, `CST2/KSP(NCEBOT)`, for the current column.
   DOUBLEPRECISION SGMA    !! Implicit finite-difference weight; currently one.
   DOUBLEPRECISION SGSQ    !! Squared implicit weight, `SGMA*SGMA`.
   DOUBLEPRECISION SGTSE   !! Sigma-weighted scaled timestep, `SGMA*TSE`.
   DOUBLEPRECISION SGSTSE  !! Squared-sigma weighted scaled timestep, `SGSQ*TSE`.
   DOUBLEPRECISION OMSGMA  !! Explicit finite-difference weight, `1-SGMA`; currently zero.
   DOUBLEPRECISION OPSGL   !! Current-contaminant factor `1+SGTSE*GCAPLA`.
   DOUBLEPRECISION OPSGSL  !! Current-contaminant factor `1+SGSTSE*GCAPLA`.
   DOUBLEPRECISION TSE     !! Dimensionless contaminant timestep, `D0*DTUZ/Z2SQ`.

   DOUBLEPRECISION FNCPSF  !! Unused legacy phreatic-surface cell fraction; never initialized in current source.

   INTEGER :: NCEBOT  !! Bottom active cell of the current column, `NCOLMB(NCL)`.
   INTEGER :: NCETOP  !! Ground-surface cell index, set to `top_cell_no` by `INCM`.
   INTEGER :: NCEPSF  !! Upper lateral-transport cell index; currently set to `NCETOP`.

END MODULE COLM_C1
