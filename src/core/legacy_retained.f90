!> summary: Retained state documented as having no current producer or consumer.
!> author: GP; RAH; J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> This module exists to make a fact visible rather than to serve any
!> calculation. Every name in it was declared in `sglobal`, `AL_C` or `AL_D` and
!> is documented there as inactive: never written, never read, or written once
!> and never read back. Collecting them in one place means the rest of the
!> `core/` modules hold only live state, and it turns "is this used?" into a
!> question about one file.
!>
!> Nothing here is deleted. Whether each name is dead, or is a producer that was
!> lost, is a separate question per name and needs the hydrological context that
!> the reorganisation deliberately does not settle.
!>
!> @warning
!> Standard Fortran regards these values as undefined: they have no declaration
!> initializers and no producer. A compiler's zero-filled static storage is not
!> a portable initialization, so a consumer added later must set a value before
!> reading one.
!>
!> `EARRAY(1)` is printed for errors 1003 and 1024, but no current assignment to
!> `EARRAY` exists in the source tree.
!>
!> The three `*ERRC` error-count arrays are read by [[run_summary:extra_output]] at
!> shutdown and have no current producer. `NGRID` is zeroed by `FRIND` but is
!> not subsequently read. `NHSAT` has neither producer nor consumer.
!>
!> The retained Monte Carlo names (`szmonte`, `ran2monte1`, `ran2monte2`,
!> `pcmonte`, and `montec`) and `text32` have no current consumers. Their more
!> specific historical meanings cannot be established from the active code;
!> `montec` is never allocated.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_C, AL_D, sglobal; see docs/rename/proposal.md. |
!> @endhistory
MODULE legacy_retained

   USE MOD_PARAMETERS, ONLY: I_P, R8P
   USE ARRAY_LIMITS, ONLY: NELEE, NLFEE, NVEE, NCONEE

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: szmonte, ran2monte1, ran2monte2, pcmonte, montec, EARRAY, text32, NHSAT, NRPD, BEXTS1, &
             NGRID, NEXPO, FLERRC, SYERRC, CMERRC, WIDTF, ZBED, HFLBED, ZFBED, DZFBED, LROOT, HFLBNK, &
             EPOTR, CMEAN, SMEAN, ADMEAN

   INTEGER(KIND=I_P) :: szmonte = -1 !! Inactive retained Monte Carlo state; no current consumer.
   INTEGER(KIND=I_P) :: ran2monte1 = -1 !! Inactive retained Monte Carlo state; no current consumer.
   INTEGER(KIND=I_P) :: ran2monte2 = -1 !! Inactive retained Monte Carlo state; no current consumer.
   INTEGER(KIND=I_P) :: pcmonte = -1 !! Inactive retained Monte Carlo state; no current consumer.
   INTEGER(KIND=I_P), DIMENSION(:, :), ALLOCATABLE :: montec !! Inactive Monte Carlo array; never allocated by current code.
   REAL(KIND=R8P) :: EARRAY(1) !! Numeric context read for errors 1003/1024; no current producer initializes it.
   CHARACTER(32) :: text32 !! Inactive retained shared text workspace; no current consumer.
   INTEGER, DIMENSION(NELEE) :: NHSAT    !! Unused legacy saturation-state array with no current producer or consumer.
   INTEGER :: NRPD   !! Legacy precipitation-record counter.
   LOGICAL :: BEXTS1     !! Inactive legacy first time-series extension switch.
   INTEGER :: NGRID(NELEE)        !! Legacy element list zeroed by `FRIND` and not subsequently read.
   INTEGER :: NEXPO(NLFEE,2)      !! Inactive legacy link-exposure array.
   INTEGER :: FLERRC(0:100) !! Legacy flow error counts read at shutdown; no current producer was found.
   INTEGER :: SYERRC(0:100) !! Legacy sediment error counts read at shutdown; no current producer was found.
   INTEGER :: CMERRC(0:100) !! Legacy contaminant error counts read at shutdown; no current producer was found.
   DOUBLEPRECISION :: WIDTF(NLFEE)   !! Inactive legacy link face-width array.
   DOUBLEPRECISION :: ZBED(NELEE)    !! Inactive legacy impermeable-bed elevation array.
   DOUBLEPRECISION :: HFLBED(NLFEE)  !! Inactive legacy link-bed head array.
   DOUBLEPRECISION :: ZFBED(NLFEE)   !! Inactive legacy link-bed elevation array.
   DOUBLEPRECISION :: DZFBED(NLFEE)  !! Inactive legacy link-bed elevation-difference array.
   DOUBLEPRECISION :: LROOT(NVEE)    !! Inactive legacy root-depth array.
   DOUBLEPRECISION :: HFLBNK(NLFEE)  !! Inactive legacy bank-head array.
   DOUBLEPRECISION :: EPOTR(NVEE)   !! Inactive legacy potential-evaporation array by vegetation type.
   DOUBLEPRECISION :: CMEAN(NELEE,2,NCONEE)  !! Inactive legacy dissolved-contaminant mean accumulator.
   DOUBLEPRECISION :: SMEAN(NELEE,2,NCONEE)  !! Inactive legacy dead-space-contaminant mean accumulator.
   DOUBLEPRECISION :: ADMEAN(NELEE,2,NCONEE) !! Inactive legacy adsorbed-contaminant mean accumulator.

END MODULE legacy_retained

