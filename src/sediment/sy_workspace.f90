!> summary: [[sy_driver:SYMAIN]]'s heap work arrays and per-timestep scratch.
!> author: AB / RAH / BTL, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> These are not model state: they are the intermediate quantities one sediment
!> timestep needs — the derived slopes, shear stresses, water depths and
!> per-class discharges — which `SYMAIN` computes and then passes down to the
!> erosion, capacity and routing routines. Nothing here means anything between
!> timesteps.
!>
!> They are module variables, and allocated once rather than automatic, because
!> as `SYMAIN`'s own locals they overflowed the stack on Windows. That is the
!> only reason they are visible at all, and it is why they are here rather than
!> mixed in with [[sy_config]] or [[sy_state]]. Module state is public by
!> default.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into a single Fortran 90 module. |
!> | 2026-04 to 2026-05 | SvB | 4.6.1 | Modernised the whole component: free-form layout, `IMPLICIT NONE`/`INTENT` throughout, structured control flow in place of `GOTO`s, compile-time `PARAMETER`s for the cached first-call constants, and `symain`'s work arrays moved to allocate-once module storage. |
!> | 2026-09-10 | SvB | - | Split out of SYmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_workspace

   IMPLICIT NONE


   ! [[sy_driver:symain]] work arrays, allocated once by
   ! [[sy_driver:initialise_symain_workspace]]. Named as they were when they were
   ! SYMAIN's own local variables, so unlike the scalars in [[sy_config]] they do
   ! not carry a "_symain" suffix.
   INTEGER, ALLOCATABLE :: IDUM1A(:) !! Integer workspace for [[syerr3]].
   INTEGER, ALLOCATABLE :: IDUM1X(:) !! Integer workspace for [[syerr1]].
   DOUBLE PRECISION, ALLOCATABLE :: CONCI(:, :)  !! Capacity concentration by link and sediment class, from [[sycltr]].
   DOUBLE PRECISION, ALLOCATABLE :: DCIPRM(:, :) !! Interim upper-bed sediment depth by link/class, from [[sylink]].
   DOUBLE PRECISION, ALLOCATABLE :: DDIPRM(:, :) !! Interim lower-bed sediment depth by link/class, from [[sylink]].
   DOUBLE PRECISION, ALLOCATABLE :: DRDROP(:) !! Effective raindrop/drip diameter by land element, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: DUMSED(:) !! Sediment-sized floating-point workspace passed to [[sycltr]]/[[sycolm]].
   DOUBLE PRECISION, ALLOCATABLE :: DWAT1(:)  !! Surface/channel water depth by element, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: EPSB(:)   !! Bank erosion sediment source by link, from [[sybker]].
   DOUBLE PRECISION, ALLOCATABLE :: FQCONF(:, :) !! Confluence outflow fractions for receiving branches, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: LRAIN(:)  !! Effective direct rainfall rate by land element, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: QSDWAT(:, :, :) !! Sediment advection coefficient for outflow faces, from [[sycltr]].
   DOUBLE PRECISION, ALLOCATABLE :: QSEDB(:, :) !! Boundary sediment flow by class and boundary entry, from [[sybc]].
   DOUBLE PRECISION, ALLOCATABLE :: QWATB(:)    !! Boundary water outflow rate by boundary entry.
   DOUBLE PRECISION, ALLOCATABLE :: SLOPEJ(:, :) !! Face water-surface slope, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: TAUJ(:, :)   !! Face shear stress, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: TAUK(:)      !! Representative element/link shear stress, from [[sywat]].
   DOUBLE PRECISION, ALLOCATABLE :: VCFMAX(:) !! Maximum fine volume available for settling/infiltration, from [[syfine]].
   DOUBLE PRECISION, ALLOCATABLE :: VINFMX(:) !! Maximum fine infiltration volume, from [[syfine]].
   LOGICAL, ALLOCATABLE :: BARM(:) !! True where fine sediment is protected by bed armouring, from [[syfine]].
   LOGICAL, ALLOCATABLE :: LDUM(:) !! Logical workspace for `ALCHK`/`ALCHKI` checks in [[syerr1]]-[[syerr3]].

END MODULE sy_workspace

