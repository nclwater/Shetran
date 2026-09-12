!> summary: Per-column water state used by the contaminant transport solver.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> `COLM_C2` replaces the second legacy `COLM.C2` common-block include.
!> [[cmmod:colmw]] gathers the previous and current water, sediment, erosion,
!> geometry, and flow state for one column and updates the corresponding
!> persistent arrays in [[colm_co]] and [[sed_co]]. For each contaminant,
!> [[cmmod:colmsm]] then prepares the dry-deposition and water-input source
!> terms before [[cmmod:colm]] assembles and solves the column equations.
!> [[cmmod:plcolm]] also consumes selected cell properties for plant uptake.
!>
!> These module variables are shared workspace for the column most recently
!> prepared by `COLMW`, not arrays of state for every element. A suffix `1`
!> generally denotes the current timestep and no suffix the previous timestep;
!> source-term suffixes `T` and `C` denote time- and concentration-derivative
!> components. Cell arrays have the capacity bound `LLEE`, while their active
!> range is determined by `NCEBOT:NCETOP` and the required interface cells.
!> The module supplies no initial values, and all its state and the imported
!> capacity `LLEE` are public because no `PRIVATE` statement is active.
!>
!> `TTTLSE` is a fixed assumed moisture content for loose sediment, not an
!> input value. Surface and subsurface face flows are arranged as positive into
!> the column. Vertical fluxes are positive upward, so downward net rainfall in
!> `QI` and `QI1` is negative.
!>
!> @warning
!> The manual's `CM57` mobile-water fractions are read into a local array by
!> [[cmmod:cmrd]], but [[cmmod:phi]] currently returns `0.5` for every soil and water
!> content. Consequently, active `PPHI` and `PPHI1` entries are always `0.5`.
!>
!> At the top interface, `COLM` evaluates `PPHI`, `PPHI1`, `TTHET`, and
!> `TTHET1` at `NCETOP+1`, whereas `COLMW` populates those arrays only through
!> `NCETOP`. The associated dispersion value is set to zero, but the four
!> operands themselves are undefined in standard Fortran. This documentation
!> transfer does not change that current behaviour.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-22 | JE | 3.1 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Completed. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE COLM_C2
   USE SGLOBAL, ONLY : LLEE
   IMPLICIT NONE
   DOUBLEPRECISION ICAP   !! Prior scaled dry-deposition source, `-Z2OD*IIICFO(NCONT)`.
   DOUBLEPRECISION ICAPT  !! Time-derivative part of `ICAP`; currently set to zero.
   DOUBLEPRECISION ICAPC  !! Concentration derivative of `ICAP`; currently set to zero.
   DOUBLEPRECISION QCAP   !! Prior scaled rainfall and irrigation/well contaminant source.
   DOUBLEPRECISION QCAPT  !! Timestep change in the scaled rainfall and irrigation/well source.
   DOUBLEPRECISION QCAPC  !! Concentration derivative of `QCAP`; currently set to zero.

   DOUBLEPRECISION DDA     !! Current column's y-direction plan dimension, `DYQQ(NCL)` (m).
   DOUBLEPRECISION DDB     !! Current column's x-direction plan dimension, `DXQQ(NCL)` (m).
   DOUBLEPRECISION DDDLS   !! Previous loose-sediment depth for the current column (m).
   DOUBLEPRECISION DDDLS1  !! Current loose-sediment depth for the current column (m).
   DOUBLEPRECISION DDDSW   !! Previous surface-water depth for the current column (m).
   DOUBLEPRECISION DDDSW1  !! Current surface-water depth for the current column (m).
   DOUBLEPRECISION GGGNU   !! Previous ground-surface erosion depth rate (m/s).
   DOUBLEPRECISION GGGNU1  !! Current ground-surface erosion depth rate (m/s).
   DOUBLEPRECISION KSP(LLEE)   !! Nondimensional cell thickness, `DELTAZ/Z2`.
   DOUBLEPRECISION KSPP(LLEE)  !! Nondimensional spacing between adjacent cell nodes.
   DOUBLEPRECISION ZONE    !! Previous nondimensional active-column depth.
   DOUBLEPRECISION ZONE1   !! Current nondimensional depth, `(ZGRUND-ZCOLMB)/Z2`.

   DOUBLEPRECISION TTTLSE  !! Assumed loose-sediment moisture content; fixed at `1.0D-4`.

   DOUBLEPRECISION QQQSW(4)   !! Previous surface-water volume flow into each column face (m3/s).
   DOUBLEPRECISION QQQSW1(4)  !! Current surface-water volume flow into each column face (m3/s).

   DOUBLEPRECISION GGAMM(LLEE)   !! Previous dynamic/dead-space water-coupling rate by cell (1/s).
   DOUBLEPRECISION GGAMM1(LLEE)  !! Current dynamic/dead-space water-coupling rate by cell (1/s).
   DOUBLEPRECISION PPHI(LLEE)    !! Previous mobile-water fraction by cell; currently `0.5`.
   DOUBLEPRECISION PPHI1(LLEE)   !! Current mobile-water fraction by cell; currently `0.5`.
   DOUBLEPRECISION QQ(LLEE,4)    !! Previous solver-scaled subsurface flow by cell and face (m3/s).
   DOUBLEPRECISION QQ1(LLEE,4)   !! Current solver-scaled subsurface flow by cell and face (m3/s).
   DOUBLEPRECISION TTHET(LLEE)   !! Previous volumetric water content by cell.
   DOUBLEPRECISION TTHET1(LLEE)  !! Current volumetric water content by cell.
   DOUBLEPRECISION UUAJP(LLEE)   !! Previous upward vertical water flux by cell interface (m/s).
   DOUBLEPRECISION UUAJP1(LLEE)  !! Current upward vertical water flux by cell interface (m/s).

   DOUBLEPRECISION QQRF   !! Previous upward water-volume flow through the column base (m3/s).
   DOUBLEPRECISION QQRF1  !! Current upward water-volume flow through the column base (m3/s).
   DOUBLEPRECISION QI     !! Previous net-precipitation volume flow; downward input is negative (m3/s).
   DOUBLEPRECISION QI1    !! Current net-precipitation volume flow; downward input is negative (m3/s).
!PRIVATE :: LLEE

END MODULE COLM_C2
