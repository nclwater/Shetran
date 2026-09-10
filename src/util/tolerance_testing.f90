!> summary: Numeric tolerance testing
!> author: GP; AB/RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> Several methods for tolerance testing of especially floats. Extracted from SGLOBAL.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-08-31 | SvB | - | Extracted from SGLOBAL |
!> @endhistory
MODULE tolerance_testing

   USE MOD_PARAMETERS, ONLY: I_P, R8P
   USE mod_parameters, ONLY: zero, one, vsmall, imarker

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: eqmarker, gtzero, gezero, ltzero, lezero, iszero, iszero_a, &
             i_iszero_a2, notzero, isone, notone, &
             idimje, dimje

contains

!> summary: Detects the time-series end marker by integer truncation.
!>
!> Returns true when `INT(a)==imarker`, where `imarker` is
!> `INT(marker999)=999999`. For positive finite input this accepts the entire
!> half-open interval `999999.0 <= a < 1000000.0`, not only the real marker
!> value `999999.9`; it performs neither exact real equality nor a tolerance
!> comparison.
!>
!> [[timeseries_input:FINPUT]] writes `marker999` after end-of-file, while
!> [[timeseries_input:HINPUT]] writes it after any failed read. [[oc_boundaries:OCEXT]] tests
!> its head and flux boundary times, and [[vs_driver:VSPREP]] tests the well,
!> lateral-flow, lateral-head, lateral-head-gradient, base-flow, and base-head
!> times. Each current caller supplies a scalar and raises a fatal error for
!> the boundary data when this function returns true.
!>
!> The scalar dummy makes the `ELEMENTAL` function callable with either a
!> scalar or an array; an array call returns a conformable logical array. The
!> function has no side effects.
!>
!> @warning
!> A legitimate time in the accepted integer bucket is indistinguishable
!> from the marker. Conversion with `INT` also assumes `a` is finite and lies
!> within the range representable by the default integer kind; the function
!> performs no guard for unsupported values.
!> @endwarning
!>
!> @note
!> The pre-FORD source labelled this wrapper “needed for AD” but recorded no
!> further rationale.
!> @endnote
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION eqmarker(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate time value [h].
      eqmarker = INT(a) == imarker
   END FUNCTION eqmarker

!> summary: Tests whether a value is strictly positive.
!>
!> Returns the result of `a > 0`. Unlike [[gezero]], this function does not
!> use the module tolerance `vsmall`: zero and every negative value,
!> including values within the zero band, return false. An unordered
!> comparison with a NaN also returns false.
!>
!> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
!> no side effects. Current callers use scalar hydraulic, soil, sediment,
!> snowmelt, and contaminant values to guard calculations that require a
!> positive quantity.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION gtzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      gtzero = a > zero
   END FUNCTION gtzero

!> summary: Tests whether a value is positive or within the zero band.
!>
!> Returns `iszero(a) .OR. a > 0`. Because [[iszero]] uses the strict test
!> `ABS(a) < vsmall`, this is equivalent to `a > -vsmall` for finite input:
!> small negative values inside the band are accepted, but `-vsmall` is not.
!> A NaN returns false.
!>
!> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
!> no side effects. Current scalar callers use the result in contaminant and
!> variably saturated subsurface calculations.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION gezero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      gezero = ISZERO(a) .OR. a > zero
   END FUNCTION gezero

!> summary: Tests whether a value is strictly negative.
!>
!> Returns the result of `a < 0`. Unlike [[lezero]], this function does not
!> use the module tolerance `vsmall`: zero and every positive value,
!> including values within the zero band, return false. An unordered
!> comparison with a NaN also returns false.
!>
!> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
!> no side effects. Current callers use scalar contaminant, ET, snowmelt,
!> and variably saturated subsurface values to guard negative-value paths.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION ltzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      ltzero = a < zero
   END FUNCTION ltzero

!> summary: Tests whether a value is negative or within the zero band.
!>
!> Returns `iszero(a) .OR. a < 0`. Because [[iszero]] uses the strict test
!> `ABS(a) < vsmall`, this is equivalent to `a < vsmall` for finite input:
!> small positive values inside the band are accepted, but `vsmall` is not.
!> A NaN returns false.
!>
!> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
!> no side effects. Current scalar callers use the result in ET and snowmelt
!> calculations.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION lezero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      lezero = ISZERO(a) .OR. a < zero
   END FUNCTION lezero

!> summary: Tests whether a value lies strictly inside the zero band.
!>
!> Returns `ABS(a) < vsmall`, where `vsmall` is `1.0e-20_R8P`. The boundary
!> values `-vsmall` and `vsmall` therefore return false. NaNs and infinities
!> also return false.
!>
!> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
!> no side effects. This predicate supplies the zero-band semantics used by
!> [[gezero]], [[lezero]], [[notzero]], and [[iszero_a]], as well as by
!> numerical checks throughout the simulation modules.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION iszero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      iszero = ABS(a) < vsmall
   END FUNCTION iszero

!> summary: Tests whether every value in a rank-one array is in the zero band.
!>
!> Applies [[iszero]] to each element and returns true only if every element
!> satisfies `ABS(a(i)) < vsmall`. A zero-size array returns true because the
!> result is initialized to true and the loop has no iterations.
!>
!> [[grid_arrays:AREADR]] is the only current caller; it passes the active slice
!> `AOUT(1:total_no_elements)` so that an all-zero real grid can be printed
!> compactly. The function is `PURE` and has no side effects.
!>
!> @note
!> After the first false result the loop continues, but the `CYCLE` statement
!> prevents any further array elements from being evaluated. The retained
!> source comment labels this control-flow form “FOR AD” without identifying
!> the automatic-differentiation tool or further rationale.
!> @endnote
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the array and loop index to selected kinds, made the helper pure, and added the initial FORD block. |
!> @endhistory
   PURE LOGICAL FUNCTION iszero_a(a)
      INTEGER(KIND=I_P) :: i !! Array index.
      REAL(KIND=R8P), DIMENSION(:), INTENT(IN) :: a !! Values to test against the strict zero band.
      iszero_a = .TRUE.
      DO i = 1, SIZE(a)
         IF (.NOT. iszero_a) CYCLE     !FOR AD
         iszero_a = iszero(a(i))
      END DO
   END FUNCTION iszero_a

!> summary: Tests whether every value in a rank-two integer array is zero.
!>
!> Uses exact integer equality and returns true only if every `a(i,j)` is
!> zero. An array with either extent equal to zero returns true because the
!> result is initialized to true and no element changes it.
!>
!> [[grid_arrays:AREADI]] is the only current caller; it passes the active slice
!> `IA(1:NX,1:NY)` so that an all-zero integer grid can be printed compactly.
!> The function is `PURE` and has no side effects.
!>
!> @note
!> After the first false result the nested loops continue, but the `CYCLE`
!> statement prevents any further array elements from being evaluated. The
!> retained source comment labels this control-flow form “FOR AD” without
!> identifying the automatic-differentiation tool or further rationale.
!> @endnote
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the array and loop indices to selected kinds, made the helper pure, and added the initial FORD block. |
!> @endhistory
   PURE LOGICAL FUNCTION i_iszero_a2(a)
      INTEGER(KIND=I_P)                             :: i, j !! Array indices.
      INTEGER(KIND=I_P), DIMENSION(:, :), INTENT(IN) :: a !! Integer values to test for exact zero.
      i_iszero_a2 = .TRUE.
      DO i = 1, SIZE(a, DIM=1)
         DO j = 1, SIZE(a, DIM=2)
            IF (.NOT. i_iszero_a2) CYCLE     !FOR AD
            i_iszero_a2 = a(i, j) == 0
         END DO
      END DO
   END FUNCTION i_iszero_a2

!> summary: Tests whether a value lies outside the strict zero band.
!>
!> Returns the logical complement of [[iszero]]. It is false only when
!> `ABS(a) < vsmall`; the boundary values `-vsmall` and `vsmall` return true.
!> Because `iszero` returns false for a NaN, this function returns true for a
!> NaN.
!>
!> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
!> no side effects. Current callers use scalar values throughout the frame,
!> ET, OC, VSS, contaminant, and input-utility code.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION notzero(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      notzero = .NOT. ISZERO(a)
   END FUNCTION notzero

!> summary: Tests whether a value lies strictly inside the band around one.
!>
!> Returns `ABS(a-one) < vsmall`, using the same `1.0e-20_R8P` tolerance as
!> [[iszero]]. A mathematically exact tolerance boundary is excluded, as are
!> NaNs and infinities.
!>
!> @note
!> With the current gfortran `R8P` kind, `vsmall` is narrower than the spacing
!> between representable values around one. For that build this predicate is
!> therefore true only for the exactly represented value `1.0_R8P`.
!> @endnote
!>
!> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
!> no side effects. [[vs_column_solver:VSCOEF]] is the only current consumer and uses a
!> scalar result to select the arithmetic-mean conductivity case.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION isone(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      isone = ABS(a - one) < vsmall
   END FUNCTION isone

!> summary: Tests whether a value lies outside the strict band around one.
!>
!> Returns the logical complement of [[isone]]. A mathematically exact
!> tolerance boundary returns true. Because `isone` returns false for a NaN,
!> this function returns true for a NaN.
!>
!> @note
!> With the current gfortran `R8P` kind, `vsmall` is narrower than the spacing
!> between representable values around one. For that build this predicate is
!> therefore false only for the exactly represented value `1.0_R8P`.
!> @endnote
!>
!> The `ELEMENTAL` interface accepts a scalar or a conformable array and has
!> no side effects. [[vs_column_solver:VSCOEF]] is the only current consumer and uses a
!> scalar result to decide whether exponentiation is required.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the helper to selected real kind, made it elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION notone(a)
      REAL(KIND=R8P), INTENT(IN) :: a !! Candidate value.
      notone = .NOT. ISONE(a)
   END FUNCTION notone

!> summary: Returns the positive part of an integer difference.
!>
!> Returns `x-y` when `x > y`, and zero otherwise. As an `ELEMENTAL`
!> function it accepts scalar or conformable array arguments and has no side
!> effects. Current scalar calls use it to derive non-fine sediment capacity
!> in [[sy_validation:SYERR2]] and to round an input allocation extent upward in
!> [[spatial_fields:ALSPRD]].
!>
!> @warning
!> The function result is default integer because the function statement does
!> not declare `INTEGER(KIND=I_P)`, although both arguments use `I_P`. The
!> subtraction and conversion are not checked for overflow or an
!> unrepresentable result if those kinds differ on another compiler.
!> @endwarning
!>
!> @note
!> The pre-FORD source labelled this wrapper “AD PROBLEM” but recorded no
!> further rationale.
!> @endnote
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the arguments to selected integer kind, made the helper elemental, and added the initial FORD block; the default-integer result was retained. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION idimje(x, y)
      INTEGER(KIND=I_P), INTENT(IN) :: x !! Minuend.
      INTEGER(KIND=I_P), INTENT(IN) :: y !! Subtrahend.
      IF (x > y) THEN
         idimje = x - y
      ELSE
         idimje = 0
      END IF
   END FUNCTION idimje

!> summary: Returns the positive part of a selected-kind real difference.
!>
!> Returns `x-y` when `x > y`, and `0.0_R8P` otherwise. An unordered
!> comparison, including one involving a NaN, follows the latter branch and
!> returns zero. The function does not apply `vsmall`.
!>
!> The `ELEMENTAL` interface accepts scalar or conformable array arguments
!> and has no side effects. Current scalar calls truncate negative depth,
!> stress, concentration, storage, and transport differences in OC and
!> sediment calculations.
!>
!> @note
!> The pre-FORD source labelled this wrapper “AD PROBLEM” but recorded no
!> further rationale.
!> @endnote
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2026-03-28 | SvB | Converted the result and arguments to selected real kind, made the helper elemental, and added the initial FORD block. |
!> @endhistory
   ELEMENTAL FUNCTION dimje(x, y)
      REAL(KIND=R8P) :: dimje !! Positive difference.
      REAL(KIND=R8P), INTENT(IN) :: x !! Minuend.
      REAL(KIND=R8P), INTENT(IN) :: y !! Subtrahend.
      IF (x > y) THEN
         dimje = x - y
      ELSE
         dimje = zero
      END IF
   END FUNCTION dimje

END MODULE tolerance_testing
