!> summary: Per-column contaminant state used to assemble the transport equations.
!> author: JE, Newcastle University; RAH, Newcastle University
!>
!> `COLM_CC` replaces the legacy `COLM.CC` common blocks. It is mutable
!> workspace for the column and contaminant most recently prepared by
!> [[cmmod:colmsm]], with plant terms optionally supplied by
!> [[cmmod:plcolm]]. [[cmmod:colm]] consumes this state, solves the coupled
!> dynamic-region and dead-space equations, and returns updated concentrations
!> and decay-generation terms.
!>
!> The dynamic region contains the mobile fraction of the soil water and its
!> assigned adsorption sites; the dead-space region contains the remaining
!> immobile water and adsorption sites. `CCAP`/`COLCAP` are the updated/prior
!> dynamic-region concentrations, while `SCAP`/`SOLCAP` are the corresponding
!> dead-space concentrations. They must not be interpreted as dissolved and
!> sorbed-solid concentrations.
!>
!> `COLMSM` processes contaminants in numeric order. Before the first pass it
!> clears the four generation arrays. Each later pass consumes the decay
!> generation left by the preceding contaminant and then `COLM` replaces those
!> values with generation from the active contaminant for the following
!> daughter contaminant. This ordering is part of the shared-state contract.
!>
!> Cell arrays have capacity `LLEE`; normal equation assembly uses
!> `NCEBOT:NCETOP`, with concentration and dispersion halo entries prepared
!> where required. Face arrays use the four lateral faces of the current
!> column. A suffix `1` identifies current water-state data, while `T`, `C`,
!> and `S` identify time-, dynamic-concentration-, and dead-space-concentration
!> derivatives as appropriate. The module supplies no initial values, and all
!> its state and imported `LLEE` are public because no `PRIVATE` statement is
!> active.
!>
!> `AALPSO` copies the soil-region exchange coefficient from manual input
!> record `CM53`; it is not longitudinal dispersivity. `FFSO` copies the
!> dynamic-region fraction of adsorption sites from record `CM55`. Although
!> `DDOD` and `DDOD1` represent prior and current dispersion states,
!> [[cmmod:disp]] currently ignores its arguments and returns the fixed value
!> `3.0D-8`, so both arrays receive the same active-cell value after scaling.
!>
!> @warning
!> `ESSCAP`, `ESSCPC`, and `ESSCPT` are subtracted from the top-cell equation,
!> but current source has no active assignment or initializer for them. The
!> commented legacy `PLDAT` block intended all three to be zero, thereby
!> disabling plant uptake from surface water and loose sediment. Their values
!> are undefined by standard Fortran in the current implementation; this
!> documentation transfer does not change that behaviour.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04-26 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked, no changes. |
!> | 1991-06-16 | JE | 3.1 | Removed references to `CCPRV`. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE COLM_CC
   USE SGLOBAL, ONLY : LLEE
   IMPLICIT NONE

   DOUBLEPRECISION GCAPLA  !! Active contaminant's scaled chemical-decay coefficient, `GCPLA(NCONT)`.

   DOUBLEPRECISION CSWA(4)    !! Explicit adjacent/boundary surface-water concentration by face.
   DOUBLEPRECISION CSWAT(4)   !! Scaled time derivative of the adjacent concentration in `CSWA`.
   DOUBLEPRECISION RRRSWA(4)  !! Adjacent/boundary surface-water retardation factor by face.
   DOUBLEPRECISION RRRSAT(4)  !! Total scaled time derivative of `RRRSWA`, including concentration change.
   DOUBLEPRECISION RRRLS      !! Loose-sediment retardation factor for the current column.
   DOUBLEPRECISION RRRLSC     !! Dynamic-concentration derivative of `RRRLS`.
   DOUBLEPRECISION RRRLST     !! Scaled time derivative of `RRRLS`.
   DOUBLEPRECISION RRRSW      !! Surface-water retardation factor for the current column.
   DOUBLEPRECISION RRRSWC     !! Dynamic-concentration derivative of `RRRSW`.
   DOUBLEPRECISION RRRSWT     !! Scaled time derivative of `RRRSW`.

   DOUBLEPRECISION CCAPA(LLEE,4)   !! Explicit lateral upwind candidate by subsurface cell and face.
   DOUBLEPRECISION CCAPAT(LLEE,4)  !! Scaled time derivative of `CCAPA`; nonzero for implicit bank coupling.

   DOUBLEPRECISION CCAP(LLEE)    !! Updated dynamic-region concentration returned by `COLM`.
   DOUBLEPRECISION COLCAP(LLEE)  !! Prior dynamic-region concentration supplied to `COLM`.
   DOUBLEPRECISION SCAP(LLEE)    !! Updated dead-space-region concentration returned by `COLM`.
   DOUBLEPRECISION SOLCAP(LLEE)  !! Prior dead-space-region concentration supplied to `COLM`.

   DOUBLEPRECISION EDCAP(LLEE)   !! Net dynamic-region source/sink term subtracted in `COLM`.
   DOUBLEPRECISION EDCAPC(LLEE)  !! Dynamic-concentration derivative of `EDCAP`.
   DOUBLEPRECISION EDCAPT(LLEE)  !! Scaled time derivative of `EDCAP`.
   DOUBLEPRECISION ESCAP(LLEE)   !! Net dead-space source/sink term subtracted in `COLM`.
   DOUBLEPRECISION ESCAPS(LLEE)  !! Dead-space-concentration derivative of `ESCAP`.
   DOUBLEPRECISION ESCAPT(LLEE)  !! Scaled time derivative of `ESCAP`.
   DOUBLEPRECISION ESSCAP        !! Top-cell surface-water/loose-sediment plant-uptake term; uninitialized.
   DOUBLEPRECISION ESSCPC        !! Dynamic-concentration derivative of `ESSCAP`; uninitialized.
   DOUBLEPRECISION ESSCPT        !! Scaled time derivative of `ESSCAP`; uninitialized.

   DOUBLEPRECISION DDOD(LLEE)   !! Prior effective longitudinal dispersion divided by `D0`.
   DOUBLEPRECISION DDOD1(LLEE)  !! Current effective longitudinal dispersion divided by `D0`.

   DOUBLEPRECISION GNERD(LLEE)   !! Dynamic-region decay generation passed between successive contaminants.
   DOUBLEPRECISION GNDSE(LLEE)   !! Dead-space decay generation passed between successive contaminants.
   DOUBLEPRECISION GND2(LLEE)    !! Time-derivative component of dynamic-region decay generation.
   DOUBLEPRECISION GNDSE2(LLEE)  !! Time-derivative component of dead-space decay generation.

   DOUBLEPRECISION AALPSO(LLEE)  !! Exchange coefficient between dynamic and dead-space soil regions.
   DOUBLEPRECISION FFSO(LLEE)    !! Fraction of adsorption sites assigned to the dynamic region.
   DOUBLEPRECISION GGNNSO(LLEE)  !! Active contaminant's Freundlich exponent, repeated by cell.
   DOUBLEPRECISION KKDSO(LLEE)   !! Derived soil distribution coefficient by cell and active contaminant.

   DOUBLEPRECISION CCPRF   !! Advected base-flux concentration; zero for a fixed-concentration base.
   DOUBLEPRECISION CCPRFT  !! Scaled time derivative of `CCPRF`; currently always zero.
!PRIVATE :: LLEE
END MODULE COLM_CC
