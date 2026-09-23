!> summary: Shared state and parameters for contaminant and nitrate plant uptake.
!> author: JE, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `PLANT_CC` replaces the legacy `PLANT.CC` common blocks and the associated
!> `PLDAT` block-data initialization. Most state belongs to the two-compartment
!> contaminant plant model: [[frmod:inpl]] prepares the run-wide plant types,
!> cover, root distribution, and initial compartment-B mass; each timestep
!> [[cmmod:plprep]] updates canopy-dependent factors; and
!> [[cmmod:plcolm]] with [[cmmod:plant]] advances the persistent plant
!> concentrations. The nitrate path is separate: [[mnmod:mncont]] passes only
!> `RHOPL` and `DELONE` to [[mnmod:mnplant]], which maintains its other plant
!> state locally.
!>
!> | State group | Producer | Lifetime or consumer |
!> |:------------|:---------|:---------------------|
!> | Plant layout | `INPL` | Run-wide properties, root fractions, and initial compartment-B mass. |
!> | Timestep canopy | `PLPREP` | `PFTWO` and `DELFOU` consumed by `PLCOLM`. |
!> | Plant concentrations | `PLCOLM`, `PLANT` | Persistent `BCPAA` and `BCPBB`. |
!> | Contaminant chain | `PLCOLM`, `PLANT` | `GENAA` and `GENBB` carried to the next numeric contaminant. |
!> | Current-call workspace | `PLCOLM` | Scalar mass, uptake, and decay values consumed by `PLANT`. |
!>
!> The current declaration initializers differ in two places from the commented
!> legacy `PLDAT` block. `DELFOU` now starts at `1.0`, although legacy `PLDAT`
!> initialized only `FLEFT`; conversely, `FLEFT` has lost its legacy `1.0`
!> initializer. The current `NPLTYP=1` initializer also covers every plant slot,
!> whereas the legacy block initialized only slot 2. Other retained defaults
!> are `BCPAA=BCPBB=0.0`, `DELONE=0.5`, `DELTWO=0.9`, `DELTHR=1.0`, and
!> `RHOPL=500.0`.
!>
!> The module has no active `PRIVATE` statement. Its 29 declarations and the
!> six imported `SGLOBAL` capacities are therefore public; imported `NLFEE` is
!> not referenced in this module. `NTEMP1` and `NTEMP2` are unused remnants of
!> the former block-data initializer.
!>
!> @warning
!> `PDZF3` is declared with extents `(NELEE,NPELEE,LLEE)`, corresponding to
!> `(element,plant slot,layer)`, but every current producer and consumer indexes
!> it as `(element,cell,plant slot)`. Because `NPELEE=2`, `INPL` writes beyond
!> the declared second dimension whenever its cell index exceeds 2, and
!> `PLCOLM` reads the same transposed convention. This documentation transfer
!> records but does not correct the declaration/runtime mismatch.
!>
!> `XXI` has no assignment or declaration initializer in the current source,
!> although `COLMW` and `PLCOLM` use it to weight dynamic- and dead-space
!> contaminant terms. `FLEFT` is also undefined when `PLPREP` uses it for a
!> plant type whose canopy leaf area is zero. In addition, the contaminant
!> plant path is gated by the currently unassigned `ISPLT` flag documented in
!> [[is_cc]].
!>
!> `INPL` sets `NPLT=NV`, but supplies `PMASS` and `PF2MAX` only for plant types
!> 1--3 and `PKMAX` only for those types and contaminant 1. No guard restricts
!> active plant types or contaminants to those initialized entries. Values
!> outside that subset remain undefined if the contaminant plant path runs.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-04 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked, no changes. |
!> | 1993-03-16 | JE | 3.4 | Completed the plant contaminant implementation. |
!> | 1997-02-24 | RAH | 4.1 | Added explicit typing. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> | 2025-10-07 | SB | 4.5.3 | Reused `RHOPL` and `DELONE` in the nitrate plant-uptake path. |
!> @endhistory
MODULE PLANT_CC
   USE SGLOBAL, ONLY : NELEE, NLFEE, LLEE, NPELEE, NCONEE, NPLTEE
   IMPLICIT NONE

   INTEGER, PARAMETER :: NTEMP1=2*NELEE*NPELEE*NCONEE !! Unused legacy element count for initializing `BCPAA` and `BCPBB`.
   INTEGER, PARAMETER :: NTEMP2=NPLTEE*NCONEE         !! Unused legacy plant-type/contaminant element count.

   DOUBLEPRECISION GENAA (NPELEE) !! Compartment-A decay generation carried to the next numeric contaminant, by plant slot.
   DOUBLEPRECISION GENBB (NPELEE) !! Compartment-B decay generation carried to the next numeric contaminant, by plant slot.
   DOUBLEPRECISION :: GCPL         !! Active contaminant's dimensionless decay coefficient, copied from `GCPLA` by `PLCOLM`.
   DOUBLEPRECISION :: GMCPAA       !! Current dimensionless compartment-A mass factor, `1-DELONE`.
   DOUBLEPRECISION :: GMCPBB       !! Current dimensionless compartment-B mass factor, `PFTWO*DELONE/PF2MAX`.
   DOUBLEPRECISION :: GMCBBD       !! Change in the compartment-B mass factor per dimensionless timestep.
   DOUBLEPRECISION :: QCPAA        !! Scaled contaminant uptake rate assigned to plant compartment A.
   DOUBLEPRECISION :: QCPBB        !! Scaled contaminant uptake rate assigned to plant compartment B.
   DOUBLEPRECISION :: RHOPL=500.0d0 !! Plant-material density used to scale contaminant and nitrate plant mass.

   DOUBLEPRECISION :: BCPAA (NELEE, NPELEE, NCONEE)=0.0d0 !! Persistent relative contaminant concentration in plant compartment A.
   DOUBLEPRECISION :: BCPBB (NELEE, NPELEE, NCONEE)=0.0d0 !! Persistent relative contaminant concentration in plant compartment B.

   DOUBLEPRECISION :: DELONE (NPLTEE)=0.5 !! Fraction assigned to the canopy-dependent annual-growth compartment B, by plant type.
   DOUBLEPRECISION :: DELTWO (NPLTEE)=0.9 !! Fraction of released compartment-B contaminant returned directly to the top cell.
   DOUBLEPRECISION :: DELTHR (NPLTEE)=1.0 !! Multiplier on compartment-B contaminant uptake capacity, by plant type.
   DOUBLEPRECISION :: DELFOU (NPLTEE)=1.0 !! Current recycling fraction for falling compartment-B mass, selected by `PLPREP`.
   DOUBLEPRECISION :: FLEFT (NPLTEE)      !! Zero-canopy fallback for `DELFOU`; currently uninitialized.

   DOUBLEPRECISION GMCBBO (NELEE, NPELEE) !! Previous compartment-B mass factor, used to calculate `GMCBBD`.

   INTEGER :: NPL (NELEE)              !! Number of active plant slots on each soil column.
   INTEGER :: NPLTYP (NELEE, NPELEE)=1 !! Plant type assigned to each soil-column plant slot; all entries initially 1.
   INTEGER :: NPLT                     !! Total number of plant types, set to `NV` by `INPL`.

   DOUBLEPRECISION PKMAX (NPLTEE, NCONEE) !! Maximum contaminant uptake coefficient by plant type and contaminant.
   DOUBLEPRECISION PMASS (NPLTEE)          !! Maximum plant-material mass per unit ground area, by plant type.

   DOUBLEPRECISION PFONE (NELEE, NPELEE) !! Fractional ground cover of each soil-column plant slot.
   DOUBLEPRECISION PFTWO (NPLTEE)        !! Current canopy leaf area index copied from `CLAI`, by plant type.
   DOUBLEPRECISION PF2MAX (NPLTEE)       !! Maximum canopy leaf area index used to normalize `PFTWO`, by plant type.
   DOUBLEPRECISION PDZF3 (NELEE, NPELEE, LLEE) !! Root fraction; declaration and current access orders disagree as warned above.

   DOUBLEPRECISION XXI !! Uninitialized weighting applied with the mobile-water fraction in contaminant transport and plant uptake.
!PRIVATE :: NELEE, NLFEE, LLEE, NPELEE, NCONEE, NPLTEE
END MODULE PLANT_CC
