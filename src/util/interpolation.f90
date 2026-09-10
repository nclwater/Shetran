!> summary: One-dimensional interpolation of tabulated input against depth or time.
!> author: AB / RAH, Newcastle University; J. Ewen, Newcastle University; Sven Berendsen
!>
!> Two independent interpolators that were in different modules and do the same
!> kind of work: [[TERPO1]] interpolates a breakpoint series, and [[ALINTP]]
!> spreads a depth table over the vertical cells of a column. Both take their
!> table and their target through arguments, so this module holds no state.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of mod_load_filedata, utilsmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE interpolation

   USE MOD_PARAMETERS, ONLY: I_P, R8P, two

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: TERPO1, ALINTP

CONTAINS

   !> Interpolates category-specific depth profiles onto active column cells.
   !>
   !> For each non-link element `NELM`, `NCATTY(NELM)` selects a table with
   !> `NTAB(category)` depth/value pairs. The top cell is assigned the first
   !> table value directly. For successively deeper cells down to
   !> `NCOLMB(NELM)`, the routine advances through the ordered table and uses
   !> linear interpolation:
   !>
   !> \[
   !> V(z)=V_{j-1}+(V_j-V_{j-1})
   !>       \frac{z-z_{j-1}}{z_j-z_{j-1}}.
   !> \]
   !>
   !> Here `z` is accumulated cell-centre depth from `DELTAZ` and `ZVSNOD`.
   !> Values at or below the final table depth are clamped to the final value.
   !> The value units are those of `TABLE_CONCENTRATION`; despite its legacy
   !> name this argument also carries nitrate process-parameter profiles in
   !> [[mn_input]]. [[cm_input:INCM]] uses the routine for initial contaminant
   !> concentration profiles. Depths and vertical geometry are in metres.
   !>
   !> Only `CELL_CONCENTRATION(NLF+1:NEL,NCOLMB(element):NCETOP)` is defined.
   !> Link rows, cells below each active column, and any unused capacity are
   !> outside the result domain. This is the interpolation described for the
   !> contaminant and nitrate depth tables in the manual and legacy SSR51.
   !>
   !> @warning
   !> Category codes and table counts are used without bounds checks. Every
   !> active category needs a first entry at depth zero and strictly increasing
   !> depths. A column with cells below the top also needs at least two entries;
   !> otherwise `NTABLE` can identify a nonexistent second entry or the
   !> denominator can be zero. The routine assumes cell-centre depths increase
   !> monotonically while cell indices descend.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | - | - | - | Created the category depth-table interpolation routine. |
   !> | 2025-10 | SB | 4.5.3 | Changed the result extent from capacity bounds to active `NEL` and `NCETOP` bounds. |
   !> | 2026-04-06 | SvB | - | Replaced the interval-search jump with a named-loop exit. |
   !> @endhistory
   SUBROUTINE ALINTP(LLEE, NCETOP, NEL, NELEE, NLF, NUM_CATEGORIES_TYPES, &
                     MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCATTY, &
                     NCOLMB, NTAB, TABLE_CONCENTRATION, TABLE_WATER_DEPTH, &
                     DELTAZ, ZVSNOD, CELL_CONCENTRATION)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, R8P, two

      IMPLICIT NONE

      ! INPUT ARGUMENTS
      INTEGER(kind=I_P), INTENT(IN) :: LLEE !! Vertical-cell capacity extent of the geometry arrays.
      INTEGER(kind=I_P), INTENT(IN) :: NCETOP !! Top active VSS cell index and result second extent.
      INTEGER(kind=I_P), INTENT(IN) :: NEL !! Number of active elements and result first extent.
      INTEGER(kind=I_P), INTENT(IN) :: NELEE !! Element capacity extent of the geometry arrays.
      INTEGER(kind=I_P), INTENT(IN) :: NLF !! Number of link elements excluded from interpolation.
      INTEGER(kind=I_P), INTENT(IN) :: NUM_CATEGORIES_TYPES !! Number of active depth-profile categories.
      INTEGER(kind=I_P), INTENT(IN) :: MAX_NUM_CATEGORY_TYPES !! Allocated first extent of the table arrays.
      INTEGER(kind=I_P), INTENT(IN) :: MAX_NUM_DATA_PAIRS !! Allocated second extent of the table arrays.
      INTEGER(kind=I_P), INTENT(IN) :: NCATTY(NLF + 1:NEL) !! Profile category by non-link element.
      INTEGER(kind=I_P), INTENT(IN) :: NCOLMB(NLF + 1:NEL) !! Bottom active column-cell index by non-link element.
      INTEGER(kind=I_P), INTENT(IN) :: NTAB(NUM_CATEGORIES_TYPES) !! Active depth/value-pair count by category.

      REAL(kind=R8P), INTENT(IN) :: TABLE_CONCENTRATION(MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS) !! Profile value by category and table entry.
      REAL(kind=R8P), INTENT(IN) :: TABLE_WATER_DEPTH(MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS) !! Depth below ground surface by category and entry (m).
      REAL(kind=R8P), INTENT(IN) :: DELTAZ(LLEE, NELEE) !! VSS cell thickness by cell and element (m).
      REAL(kind=R8P), INTENT(IN) :: ZVSNOD(LLEE, NELEE) !! VSS node elevation by cell and element (m).

      ! OUTPUT ARGUMENTS
      REAL(kind=R8P), INTENT(OUT) :: CELL_CONCENTRATION(NEL, NCETOP) !! Interpolated profile values; only active non-link cells are assigned.

      ! LOCALS ETC.
      INTEGER(kind=I_P) :: NCL !! Current VSS cell index.
      INTEGER(kind=I_P) :: NELM !! Current non-link element index.
      INTEGER(kind=I_P) :: NCATG !! Profile category selected for `NELM`.
      INTEGER(kind=I_P) :: NINTB !! Active table-entry count for `NCATG`.
      INTEGER(kind=I_P) :: NTABLE !! Upper bracketing table-entry index.
      INTEGER(kind=I_P) :: NTHRTB !! First candidate upper bracket for the next deeper cell.
      REAL(kind=R8P) :: DEPTH !! Accumulated current cell-centre depth below the surface (m).

      ! Code =================================================================

      element_loop: DO NELM = NLF + 1, NEL
         ! Category number for the element
         NCATG = NCATTY(NELM)

         ! Number of values in the table for this category number
         NINTB = NTAB(NCATG)

         ! The first depth in the table must be zero and the top
         ! cell is set to take the concentration at this depth
         CELL_CONCENTRATION(NELM, NCETOP) = TABLE_CONCENTRATION(NCATG, 1)
         DEPTH = DELTAZ(NCETOP, NELM)/two
         NTHRTB = 2

         cell_loop: DO NCL = NCETOP - 1, NCOLMB(NELM), -1

            DEPTH = DEPTH + (ZVSNOD(NCL + 1, NELM) - ZVSNOD(NCL, NELM))

            ! The depth of the cell is greater than the lowest depth in
            ! the table and the cell takes the value of the concentration
            ! at the lowest specified depth
            IF (DEPTH >= TABLE_WATER_DEPTH(NCATG, NINTB)) THEN
               CELL_CONCENTRATION(NELM, NCL) = TABLE_CONCENTRATION(NCATG, NINTB)
               CYCLE cell_loop
            END IF

            ! Find the correct interval for interpolation
            search_loop: DO NTABLE = NTHRTB, NINTB
               IF (DEPTH <= TABLE_WATER_DEPTH(NCATG, NTABLE)) EXIT search_loop
               NTHRTB = NTHRTB + 1
            END DO search_loop

            ! Calculate concentration by linear interpolation
            CELL_CONCENTRATION(NELM, NCL) = &
               TABLE_CONCENTRATION(NCATG, NTABLE - 1) + &
               (TABLE_CONCENTRATION(NCATG, NTABLE) - TABLE_CONCENTRATION(NCATG, NTABLE - 1))* &
               ((DEPTH - TABLE_WATER_DEPTH(NCATG, NTABLE - 1))/ &
                (TABLE_WATER_DEPTH(NCATG, NTABLE) - TABLE_WATER_DEPTH(NCATG, NTABLE - 1)))

         END DO cell_loop
      END DO element_loop

   END SUBROUTINE ALINTP

   !> Interpolates a one-dimensional time-varying parameter.
   !>
   !> The routine updates one parameter value from a table of relative values and
   !> tabulated times, using the current simulation time in hours.
   !>
   !> `TERPO1` is a service routine for time-varying parameters whose tabulated
   !> values are stored as relative multipliers. The arguments are:
   !>
   !> | Argument | Meaning |
   !> |:---------|:--------|
   !> | `YCURR` | Current parameter array to update. |
   !> | `YTAB` | Tabulated relative values of the parameter. |
   !> | `YINIT` | Initial or reference parameter values. |
   !> | `TCURR` | Current simulation time, in hours. |
   !> | `TTAB` | Tabulated times, in days. |
   !> | `NCT` | Current table-position counter for each parameter. |
   !> | `NPAR` | Size of the parameter array. |
   !> | `I` | Parameter-array position being updated. |
   !>
   !> The routine advances `NCT(I)` to the interval containing `TCURR/24`, then
   !> linearly interpolates the relative multiplier:
   !>
   !> \[
   !> Y_{rel} =
   !> YTAB_{I,k}
   !> + \frac{TCURR-24\,TTAB_{I,k}}
   !>        {24\,(TTAB_{I,k+1}-TTAB_{I,k})}
   !>   \left(YTAB_{I,k+1}-YTAB_{I,k}\right),
   !> \]
   !>
   !> where \(k=NCT(I)\) after the interval update. The absolute value returned to
   !> the model is
   !>
   !> \[
   !> YCURR_I = Y_{rel}\,YINIT_I.
   !> \]
   !>
   !> @note `NCT(I)` may jump by more than one interval because `ITERP` is computed
   !> with integer division of the time offset by the current interval length. The
   !> caller must provide increasing `TTAB` values and enough table entries for the
   !> updated `NCT(I)+1`; no bounds or zero-interval checks are made here.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-10-05 | RAH | 3.4.1 | Removed `IMPLICIT INTEGER*2`. |
   !> | 1997-05-16 | RAH | 4.1 | Added explicit typing; made `*TAB` assumed-size arrays; removed redundant `ITAB` argument. |
   !> | 2026-04-13 | SvB | | Made the routine `PURE`; gave `NCT` and `YCURR` explicit `INOUT` intent (they were previously declared with no intent attribute, an implicit F77-style dummy) to formalise that both are read and updated. |
   !> @endhistory
   PURE SUBROUTINE TERPO1(YCURR, TCURR, YTAB, TTAB, NCT, YINIT, NPAR, I)
      !----------------------------------------------------------------------*
      !
      !     SERVICE SUBROUTINE TO INTERPOLATE VALUES FOR ONE-DIMENSIONAL
      !                   TIME-VARYING PARAMETERS
      !
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NPAR !! Size of the parameter array.
      INTEGER, INTENT(IN) :: I    !! Parameter-array position being updated.
      DOUBLE PRECISION, INTENT(IN) :: TCURR         !! Current simulation time, in hours.
      DOUBLE PRECISION, INTENT(IN) :: YTAB(NPAR, *)  !! Tabulated relative values of the parameter.
      DOUBLE PRECISION, INTENT(IN) :: TTAB(NPAR, *)  !! Tabulated times, in days.
      DOUBLE PRECISION, INTENT(IN) :: YINIT(NPAR)    !! Initial or reference parameter values.

      ! Input/Output arguments
      ! Modernization Fix: MUST be INOUT to preserve array elements other than index 'I'
      INTEGER, INTENT(INOUT) :: NCT(NPAR)          !! Current table-position counter for each parameter.
      DOUBLE PRECISION, INTENT(INOUT) :: YCURR(NPAR) !! Current parameter array to update.

      ! Locals, etc
      INTEGER :: ITERP, NCTERP
      DOUBLE PRECISION :: DIFFA, DIFFB, DIFFC, YREL

      !----------------------------------------------------------------------*

      NCTERP = NCT(I)

      ! Calculate interval jump (time is in hours, TTAB is in days)
      ITERP = INT((TCURR/24.0D0 - TTAB(I, NCTERP))/ &
                  (TTAB(I, NCTERP + 1) - TTAB(I, NCTERP)))
      NCTERP = NCTERP + ITERP

      ! Interpolate
      DIFFA = YTAB(I, NCTERP + 1) - YTAB(I, NCTERP)
      DIFFB = (TTAB(I, NCTERP + 1) - TTAB(I, NCTERP))*24.0D0
      DIFFC = TCURR - TTAB(I, NCTERP)*24.0D0

      YREL = YTAB(I, NCTERP) + DIFFC*DIFFA/DIFFB
      YCURR(I) = YREL*YINIT(I)

      NCT(I) = NCTERP

   END SUBROUTINE TERPO1

END MODULE interpolation

