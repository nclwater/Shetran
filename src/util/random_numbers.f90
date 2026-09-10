!> summary: The `ran2` uniform pseudo-random generator.
!> author: J. Ewen, Newcastle University; Sven Berendsen
!>
!> One routine, kept in a module of its own because it belongs to no component
!> and depends on nothing. It carries its own state through its argument, so
!> the module holds no data.
!>
!> No active code path calls it: the Monte Carlo names it was written for are
!> among the retained-but-unused state in [[legacy_retained]].
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of utilsmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE random_numbers

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: ran2

CONTAINS

   !> Returns a pseudo-random number from the legacy `ran2` generator.
   !>
   !> Long period (> 2 x 10^18) random number generator of L'Ecuyer with
   !> Bays-Durham shuffle and added safeguards. The generator updates `idum` in
   !> place and returns a uniform variate in `(0,1)`, exclusive. This is the
   !> combined multiplicative generator used in legacy Numerical Recipes code,
   !> retained for reproducibility of existing workflows.
   !>
   !> Passing `idum <= 0` reinitialises the saved shuffle table and secondary seed.
   !> Subsequent calls use saved module-local generator state, so independent random
   !> streams require explicit reseeding and are not thread-independent.
   FUNCTION ran2(idum)
      !----------------------------------------------------------------------*
      ! Call with idum a negative integer to initialize; thereafter, do not
      ! alter idum between successive deviates in a sequence.
      !----------------------------------------------------------------------*

      IMPLICIT NONE

      ! Dummy argument MUST be INOUT because the seed updates.
      ! This side-effect strictly prevents the function from being PURE.
      INTEGER, INTENT(INOUT) :: idum !! Seed/state value; `idum <= 0` reinitialises the saved stream.

      ! Return type (Explicitly Single Precision as per standard NR)
      REAL :: ran2 !! Uniform variate in `(0,1)`.

      ! Magic parameters for the dual LCGs and shuffle table
      INTEGER, PARAMETER :: IM1 = 2147483563
      INTEGER, PARAMETER :: IM2 = 2147483399
      INTEGER, PARAMETER :: IMM1 = IM1 - 1
      INTEGER, PARAMETER :: IA1 = 40014
      INTEGER, PARAMETER :: IA2 = 40692
      INTEGER, PARAMETER :: IQ1 = 53668
      INTEGER, PARAMETER :: IQ2 = 52774
      INTEGER, PARAMETER :: IR1 = 12211
      INTEGER, PARAMETER :: IR2 = 3791
      INTEGER, PARAMETER :: NTAB = 32
      INTEGER, PARAMETER :: NDIV = 1 + IMM1/NTAB

      ! Type-safe real parameters
      REAL, PARAMETER    :: EPS = 1.2E-7
      REAL, PARAMETER    :: RNMX = 1.0E0 - EPS
      REAL, PARAMETER    :: AM = 1.0E0/REAL(IM1)

      ! Saved internal state
      INTEGER, SAVE :: idum2 = 123456789
      INTEGER, SAVE :: iy = 0
      INTEGER, SAVE :: iv(NTAB) = 0

      ! Locals
      INTEGER :: j, k

      !----------------------------------------------------------------------*

      ! Initialization block
      IF (idum <= 0) THEN
         idum = MAX(-idum, 1)
         idum2 = idum

         ! Load the shuffle table (after 8 warm-up passes)
         DO j = NTAB + 8, 1, -1
            k = idum/IQ1
            idum = IA1*(idum - k*IQ1) - k*IR1
            IF (idum < 0) idum = idum + IM1
            IF (j <= NTAB) iv(j) = idum
         END DO
         iy = iv(1)
      END IF

      ! Start normal generation block
      ! First LCG
      k = idum/IQ1
      idum = IA1*(idum - k*IQ1) - k*IR1
      IF (idum < 0) idum = idum + IM1

      ! Second LCG
      k = idum2/IQ2
      idum2 = IA2*(idum2 - k*IQ2) - k*IR2
      IF (idum2 < 0) idum2 = idum2 + IM2

      ! Bays-Durham shuffle
      j = 1 + iy/NDIV
      iy = iv(j) - idum2
      iv(j) = idum

      IF (iy < 1) iy = iy + IMM1

      ! Return the generated value, preventing exact endpoint bounds
      ran2 = MIN(AM*REAL(iy), RNMX)

   END FUNCTION ran2

END MODULE random_numbers

