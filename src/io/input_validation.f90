!> summary: Range and relation checks on values just read from an input file.
!> author: AB / RAH, Newcastle University; JE, Newcastle University
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
!>
!> [[ALCHK]] and [[ALCHKI]] check a real or integer array against a bound or
!> against another array, and report every entry that fails through
!> [[error_reporting:RAISE_ERROR]]. They are called by the component readers
!> immediately after reading, so a bad input record is reported against the
!> record rather than as a later modelling failure.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of mod_load_filedata; see docs/rename/proposal.md. |
!> @endhistory
MODULE input_validation

   USE MOD_PARAMETERS, ONLY: I_P, R8P
   USE error_reporting, ONLY: RAISE_ERROR

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: ALCHK, ALCHKI

CONTAINS

   !> Checks real values against a scalar or element-wise relation.
   !>
   !> `ALCHK` tests `SUBJ(N0:N1)` against either the scalar `OBJ(N0)` or the
   !> corresponding `OBJ(i)`. The last character `a` selects the array form;
   !> otherwise the object index remains `N0`. The first two operator
   !> characters select the relation:
   !>
   !> | `OP(1:2)` | Required relation | Failure test for finite values |
   !> |:----------|:------------------|:-------------------------------|
   !> | `LT` | `SUBJ < OBJ` | `SUBJ-OBJ >= TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !> | `GT` | `SUBJ > OBJ` | `OBJ-SUBJ >= TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !> | `LE` | `SUBJ <= OBJ` | `SUBJ-OBJ > TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !> | `GE` | `SUBJ >= OBJ` | `OBJ-SUBJ > TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !> | Any other second character | approximate equality | `ABS(SUBJ-OBJ) > TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !>
   !> Every failure sets `NOTOK(i)` and increments cumulative `COUNT`. A
   !> negative `ACTION` first replaces each failing subject with its comparison
   !> value; `ABS(ACTION)` is then passed to [[error_reporting:RAISE_ERROR]] as the severity
   !> selector. The diagnostic gives the lowest-index failure and, for a
   !> nonfatal action, a continuation reports how many other entries failed.
   !> Up to three indices are inferred from commas in `SNAME`; `IX2` and `IX3`
   !> supply the fixed outer indices. This is the legacy SSR62 real checker.
   !>
   !> @warning
   !> `OP` must contain at least two characters and `TOL` is assumed finite and
   !> nonnegative. IEEE NaNs make every comparison used here false and can
   !> therefore pass validation; infinities can likewise produce unordered
   !> differences. No finite-value check is performed.
   !> @endwarning
   !>
   !> @note
   !> Legacy comments said [[ALCHKI]] was generated from this routine by a
   !> makefile. No such generator exists in the current build; the two source
   !> bodies are now maintained separately and should remain behaviorally
   !> aligned.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-07-22 | - | - | Initial version. |
   !> | 1994-08-17 | AB/RAH | 3.4.1 | Revised the relation checker. |
   !> | 2026-04-06 | SvB | - | Replaced the subscript-parser jump with a named-loop exit. |
   !> @endhistory
   SUBROUTINE ALCHK(ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME, &
                    OP, OBJ, TOL, SUBJ, COUNT, NOTOK)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, R8P, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: ACTION !! Signed error severity; a negative value also resets failures.
      INTEGER(kind=I_P), INTENT(IN) :: ERRNUM !! Diagnostic code passed to `ERROR`.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Diagnostic output unit passed to `ERROR`.
      INTEGER(kind=I_P), INTENT(IN) :: N0 !! First checked vector index.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! Last checked vector index.
      INTEGER(kind=I_P), INTENT(IN) :: IX2 !! Fixed second subscript printed when `SNAME` implies two dimensions.
      INTEGER(kind=I_P), INTENT(IN) :: IX3 !! Fixed third subscript printed when `SNAME` implies three dimensions.
      CHARACTER(LEN=*), INTENT(IN) :: SNAME !! Display name whose comma syntax controls printed subscript count.
      CHARACTER(LEN=*), INTENT(IN) :: OP !! Two-character relation, optionally suffixed by `a` for an object array.
      REAL(kind=R8P), INTENT(IN) :: OBJ(N0:*) !! Scalar comparison at `N0` or element-wise comparison sequence.
      REAL(kind=R8P), INTENT(IN) :: TOL !! Relative tolerance used in every real relation.

      ! Input/output arguments
      REAL(kind=R8P), INTENT(INOUT) :: SUBJ(N0:N1) !! Values checked and, for negative `ACTION`, reset on failure.
      INTEGER(kind=I_P), INTENT(INOUT):: COUNT !! Cumulative failure count, incremented once per nonconforming value.

      ! Workspace arguments
      LOGICAL, INTENT(OUT) :: NOTOK(N0:N1) !! Per-value failure mask.

      ! Locals, etc
      INTEGER(kind=I_P) :: COUNT0 !! `COUNT` on entry.
      INTEGER(kind=I_P) :: COUNT1 !! Failures found by this call.
      INTEGER(kind=I_P) :: I !! Subject index.
      INTEGER(kind=I_P) :: INCOBJ !! Object-index increment: zero for scalar, one for element-wise comparison.
      INTEGER(kind=I_P) :: IOBJ !! Current object index.
      INTEGER(kind=I_P) :: IX(3) !! Indices printed for the lowest-index failure.
      INTEGER(kind=I_P) :: NDIM !! Number of printed indices inferred from `SNAME`, capped at three.
      INTEGER(kind=I_P) :: P !! Diagnostic index-list iterator.
      INTEGER(kind=I_P) :: POS1 !! Previous delimiter position while parsing `SNAME`.
      INTEGER(kind=I_P) :: POS2 !! Next parenthesis/comma position while parsing `SNAME`.
      INTEGER(kind=I_P) :: SGN !! Direction multiplier: one for less relations, minus one for greater relations.
      INTEGER(kind=I_P) :: SLEN !! Declared length of `SNAME`.
      REAL(kind=R8P) :: SB !! Subject value retained for the lowest-index failure.
      REAL(kind=R8P) :: OB !! Object value retained for the lowest-index failure.
      REAL(kind=R8P) :: rrr !! Diagnostic copy of `SB`, retained from the AD-oriented implementation.
      LOGICAL :: BRESET !! True when failures are replaced by object values.
      CHARACTER(len=9) :: CACT !! `Checking` or `Resetting` diagnostic verb.
      CHARACTER(len=132) :: MSG !! Error/continuation message buffer.
      CHARACTER :: OP1 !! First relation character; `G` reverses the comparison direction.
      CHARACTER :: OP2 !! Second relation character; `T`, `E`, or another character selects the test.

      ! Code =================================================================

      ! How many subscripts are there? (ignore any after the 3rd)
      ! ------------------------------
      SLEN = LEN(SNAME)
      POS1 = 0
      POS2 = INDEX(SNAME, '(')

      dim_loop: DO NDIM = 0, 2
         IF (POS2 > POS1 .AND. POS2 < SLEN) THEN
            IF (NDIM == 1) IX(2) = IX2
            IF (NDIM == 2) IX(3) = IX3
            POS1 = POS2
            POS2 = POS1 + INDEX(SNAME(POS1 + 1:), ',')
         ELSE
            EXIT dim_loop
         END IF
      END DO dim_loop

      ! If this point is traversed normally, NDIM=3; if exited early, NDIM<3

      ! What action is required?
      ! ------------------------
      BRESET = ACTION < 0
      OP1 = OP(1:1)
      OP2 = OP(2:2)
      SGN = +1
      IF (OP1 == 'G') SGN = -1

      INCOBJ = 0
      IF (OP(LEN(OP):) == 'a') INCOBJ = 1

      ! Store test results in logical workspace array
      ! ---------------------------------------------
      ! Note:  i Code is replicated to enable vectorization of loops.
      !       ii "Requirements" are approximate if TOL>0.

      IOBJ = N0

      IF (OP2 == 'T') THEN
         ! require SUBJ < OBJ or SUBJ > OBJ (depending on SGN)
         DO I = N0, N1
            SB = SUBJ(I)
            OB = OBJ(IOBJ)
            NOTOK(I) = SGN*(SB - OB) >= TOL*MAX(ABS(SB), ABS(OB))
            IOBJ = IOBJ + INCOBJ
         END DO

      ELSE IF (OP2 == 'E') THEN
         ! require SUBJ <= OBJ or SUBJ >= OBJ (depending on SGN)
         DO I = N0, N1
            SB = SUBJ(I)
            OB = OBJ(IOBJ)
            NOTOK(I) = SGN*(SB - OB) > TOL*MAX(ABS(SB), ABS(OB))
            IOBJ = IOBJ + INCOBJ
         END DO

      ELSE
         ! require SUBJ == OBJ
         DO I = N0, N1
            SB = SUBJ(I)
            OB = OBJ(IOBJ)
            NOTOK(I) = ABS(SB - OB) > TOL*MAX(ABS(SB), ABS(OB))
            IOBJ = IOBJ + INCOBJ
         END DO
      END IF

      ! Count the non-conformances and fix them if required
      ! ---------------------------------------------------
      ! Note: Non-vectorizing loop: keep it short

      COUNT0 = COUNT
      IOBJ = N0 + INCOBJ*(N1 - N0)

      ! step backwards so that IX(1), SB & OB refer to 1st non-conformer
      DO I = N1, N0, -1
         IF (NOTOK(I)) THEN
            COUNT = COUNT + 1
            IX(1) = I
            SB = SUBJ(I)
            OB = OBJ(IOBJ)
            IF (BRESET) SUBJ(I) = OB
         END IF
         IOBJ = IOBJ - INCOBJ
      END DO

      ! Report findings
      ! ---------------
      COUNT1 = COUNT - COUNT0
      IF (COUNT1 > 0) THEN
         CACT = 'Checking'
         IF (BRESET) CACT = 'Resetting'

         ! print the first occurrence ...
         rrr = SB  !AD
         WRITE (MSG, 9000) CACT, SNAME, OP(:2), OB, rrr, (IX(P), P=1, NDIM)
         CALL RAISE_ERROR(ABS(ACTION), ERRNUM, OUNIT, 0, 0, MSG)

         IF (COUNT1 > 1) THEN
            ! ... and allude to any others
            WRITE (MSG, 9010) COUNT1 - 1
            CALL RAISE_ERROR(0, 12, OUNIT, 0, 0, MSG)
         END IF
      END IF

      ! Format Statements ----------------------------------------------------
9000  FORMAT(A, 1X, A, ': expected .', A, '.', 1P, G15.7, ' but found', G15.7: &
             ' at position', I5, 2(:, ',', I4))
9010  FORMAT('... and similarly at', I4, &
             ' other positions in the same vector')

   END SUBROUTINE ALCHK

   !> Checks integer values against a scalar or element-wise relation.
   !>
   !> `ALCHKI` is the exact-integer counterpart of [[ALCHK]]. The optional
   !> trailing `a` in `OP` selects `OBJ(i)`; otherwise every subject is compared
   !> with `OBJ(N0)`.
   !>
   !> | `OP(1:2)` | Required relation | Failure test |
   !> |:----------|:------------------|:-------------|
   !> | `LT` | `SUBJ < OBJ` | `SUBJ-OBJ >= 0` |
   !> | `GT` | `SUBJ > OBJ` | `OBJ-SUBJ >= 0` |
   !> | `LE` | `SUBJ <= OBJ` | `SUBJ-OBJ > 0` |
   !> | `GE` | `SUBJ >= OBJ` | `OBJ-SUBJ > 0` |
   !> | Any other second character | equality | `ABS(SUBJ-OBJ) > 0` |
   !>
   !> Failure accounting, optional reset, severity selection, `SNAME` subscript
   !> parsing, and reporting are identical to `ALCHK`: `NOTOK` receives the
   !> mask, `COUNT` is cumulative, and a negative `ACTION` resets bad subjects
   !> before calling [[error_reporting:RAISE_ERROR]] with `ABS(ACTION)`.
   !>
   !> @warning
   !> `OP` must contain at least two characters. The subtraction, sign
   !> multiplication, and `ABS` operations use `INTEGER(kind=I_P)` without
   !> overflow checks, so extreme operands can invalidate the relation test.
   !> @endwarning
   !>
   !> @note
   !> The legacy makefile-generation warning is obsolete: no current build rule
   !> generates this routine from `ALCHK`; the two bodies are maintained by hand.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-07-22 | - | - | Initial version. |
   !> | 1994-08-17 | AB/RAH | 3.4.1 | Revised the integer relation checker. |
   !> | 2026-04-06 | SvB | - | Replaced the subscript-parser jump with a named-loop exit. |
   !> @endhistory
   SUBROUTINE ALCHKI(ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME, &
                     OP, OBJ, SUBJ, COUNT, NOTOK)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: ACTION !! Signed error severity; a negative value also resets failures.
      INTEGER(kind=I_P), INTENT(IN) :: ERRNUM !! Diagnostic code passed to `ERROR`.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Diagnostic output unit passed to `ERROR`.
      INTEGER(kind=I_P), INTENT(IN) :: N0 !! First checked vector index.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! Last checked vector index.
      INTEGER(kind=I_P), INTENT(IN) :: IX2 !! Fixed second subscript printed for a two-dimensional display name.
      INTEGER(kind=I_P), INTENT(IN) :: IX3 !! Fixed third subscript printed for a three-dimensional display name.
      CHARACTER(LEN=*), INTENT(IN) :: SNAME !! Display name whose comma syntax controls printed subscript count.
      CHARACTER(LEN=*), INTENT(IN) :: OP !! Two-character relation, optionally suffixed by `a` for an object array.
      INTEGER(kind=I_P), INTENT(IN) :: OBJ(N0:*) !! Scalar comparison at `N0` or element-wise comparison sequence.

      ! Input/output arguments
      INTEGER(kind=I_P), INTENT(INOUT) :: SUBJ(N0:N1) !! Values checked and optionally reset.
      INTEGER(kind=I_P), INTENT(INOUT) :: COUNT !! Cumulative failure count.

      ! Workspace arguments
      LOGICAL, INTENT(OUT) :: NOTOK(N0:N1) !! Per-value failure mask.

      ! Locals, etc
      INTEGER(kind=I_P) :: COUNT0 !! `COUNT` on entry.
      INTEGER(kind=I_P) :: COUNT1 !! Failures found by this call.
      INTEGER(kind=I_P) :: I !! Subject index.
      INTEGER(kind=I_P) :: INCOBJ !! Object-index increment: zero for scalar, one for element-wise comparison.
      INTEGER(kind=I_P) :: IOBJ !! Current object index.
      INTEGER(kind=I_P) :: IX(3) !! Indices printed for the lowest-index failure.
      INTEGER(kind=I_P) :: NDIM !! Number of printed indices inferred from `SNAME`, capped at three.
      INTEGER(kind=I_P) :: P !! Diagnostic index-list iterator.
      INTEGER(kind=I_P) :: POS1 !! Previous delimiter position while parsing `SNAME`.
      INTEGER(kind=I_P) :: POS2 !! Next parenthesis/comma position while parsing `SNAME`.
      INTEGER(kind=I_P) :: SGN !! Direction multiplier: one for less relations, minus one for greater relations.
      INTEGER(kind=I_P) :: SLEN !! Declared length of `SNAME`.
      INTEGER(kind=I_P) :: SB !! Subject value retained for the lowest-index failure.
      INTEGER(kind=I_P) :: OB !! Object value retained for the lowest-index failure.
      INTEGER(kind=I_P) :: iii !! Diagnostic copy of `SB`, retained from the AD-oriented implementation.
      LOGICAL :: BRESET !! True when failures are replaced by object values.
      CHARACTER(len=9) :: CACT !! `Checking` or `Resetting` diagnostic verb.
      CHARACTER(len=132) :: MSG !! Error/continuation message buffer.
      CHARACTER :: OP1 !! First relation character; `G` reverses comparison direction.
      CHARACTER :: OP2 !! Second relation character; `T`, `E`, or another character selects the test.

      ! Code =================================================================

      ! How many subscripts are there? (ignore any after the 3rd)
      ! ------------------------------
      SLEN = LEN(SNAME)
      POS1 = 0
      POS2 = INDEX(SNAME, '(')

      dim_loop: DO NDIM = 0, 2
         IF (POS2 > POS1 .AND. POS2 < SLEN) THEN
            IF (NDIM == 1) IX(2) = IX2
            IF (NDIM == 2) IX(3) = IX3
            POS1 = POS2
            POS2 = POS1 + INDEX(SNAME(POS1 + 1:), ',')
         ELSE
            EXIT dim_loop
         END IF
      END DO dim_loop

      ! If this point is traversed normally, NDIM=3; if exited early, NDIM<3

      ! What action is required?
      ! ------------------------
      BRESET = ACTION < 0
      OP1 = OP(1:1)
      OP2 = OP(2:2)
      SGN = +1
      IF (OP1 == 'G') SGN = -1

      INCOBJ = 0
      IF (OP(LEN(OP):) == 'a') INCOBJ = 1

      ! Store test results in logical workspace array
      ! ---------------------------------------------
      ! Note:  i Code is replicated to enable vectorization of loops.

      IOBJ = N0

      IF (OP2 == 'T') THEN
         ! require SUBJ < OBJ or SUBJ > OBJ (depending on SGN)
         DO I = N0, N1
            SB = SUBJ(I)
            OB = OBJ(IOBJ)
            NOTOK(I) = SGN*(SB - OB) >= 0
            IOBJ = IOBJ + INCOBJ
         END DO

      ELSE IF (OP2 == 'E') THEN
         ! require SUBJ <= OBJ or SUBJ >= OBJ (depending on SGN)
         DO I = N0, N1
            SB = SUBJ(I)
            OB = OBJ(IOBJ)
            NOTOK(I) = SGN*(SB - OB) > 0
            IOBJ = IOBJ + INCOBJ
         END DO

      ELSE
         ! require SUBJ == OBJ
         DO I = N0, N1
            SB = SUBJ(I)
            OB = OBJ(IOBJ)
            NOTOK(I) = ABS(SB - OB) > 0
            IOBJ = IOBJ + INCOBJ
         END DO
      END IF

      ! Count the non-conformances and fix them if required
      ! ---------------------------------------------------
      ! Note: Non-vectorizing loop: keep it short

      COUNT0 = COUNT
      IOBJ = N0 + INCOBJ*(N1 - N0)

      ! step backwards so that IX(1), SB & OB refer to 1st non-conformer
      DO I = N1, N0, -1
         IF (NOTOK(I)) THEN
            COUNT = COUNT + 1
            IX(1) = I
            SB = SUBJ(I)
            OB = OBJ(IOBJ)
            IF (BRESET) SUBJ(I) = OB
         END IF
         IOBJ = IOBJ - INCOBJ
      END DO

      ! Report findings
      ! ---------------
      COUNT1 = COUNT - COUNT0
      IF (COUNT1 > 0) THEN
         CACT = 'Checking'
         IF (BRESET) CACT = 'Resetting'

         ! print the first occurrence ...
         iii = SB !AD
         WRITE (MSG, 9000) CACT, SNAME, OP(:2), OB, iii, (IX(P), P=1, NDIM)
         CALL RAISE_ERROR(ABS(ACTION), ERRNUM, OUNIT, 0, 0, MSG)

         IF (COUNT1 > 1) THEN
            ! ... and allude to any others
            WRITE (MSG, 9010) COUNT1 - 1
            CALL RAISE_ERROR(0, 12, OUNIT, 0, 0, MSG)
         END IF
      END IF

      ! Format Statements ----------------------------------------------------
9000  FORMAT(A, 1X, A, ': expected .', A, '.', I12, ' but found', I12: &
             ' at position', I5, 2(:, ',', I4))
9010  FORMAT('... and similarly at', I4, &
             ' other positions in the same vector')

   END SUBROUTINE ALCHKI

END MODULE input_validation

