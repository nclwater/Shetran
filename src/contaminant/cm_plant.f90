!> summary: The optional two-compartment plant contaminant path.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> [[PLPREP]] prepares the plant state for a timestep, [[PLCOLM]] couples a
!> soil column to the plant compartments, and [[PLANT]] advances the plant
!> uptake and translocation. [[INPL]], which came from `FRmod`, reads the plant
!> input records.
!>
!> The path is optional and inactive unless the plant input is present.
!> Its state is in [[cm_plant_state]].
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993--1998 | GP / RAH / SB | 3.4--4.2 | Developed and reorganised the contaminant transport routines. |
!> | 2008-12 | JE | 4.3.5F90 | Created `CMmod` while converting the former CM `COLM` and `LINK` Fortran sources to Fortran 90. |
!> | 2020-03-05 | SvB | - | Replaced the complete `SGLOBAL` include with selected imports. |
!> | 2026-09-10 | SvB | - | Split out of CMmod, FRmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE cm_plant

   USE MOD_PARAMETERS, ONLY: one, two, zero
   USE element_geometry, ONLY: total_no_elements, total_no_links
   USE et_state, ONLY: CLAI, NRD, NV, NVC, PLAI, RDF
   USE cm_parameters, ONLY: GCPLA
   USE cm_column_scaling, ONLY: NCETOP, Z2, Z2SQOD
   USE cm_plant_state, ONLY: DELONE, GMCBBO, NPL, NPLT, NPLTYP, PDZF3, PF2MAX, PFONE, &
                             PKMAX, PMASS
   USE float_compare, ONLY: gezero, gtzero, ltzero, notzero

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: INPL, PLCOLM, PLPREP

CONTAINS

!> @brief Calculates plant uptake and advances plant concentrations for one column and contaminant.
!>
!> For every plant type present in `NCL`, rooted cells run from
!> `NCETOP-NRD(JPLTY)` through `NCETOP`. The current mobile and dead-space
!> contributions are
!>
!> \[
!> C_d=XXI\,PPHI\,COLCAP,\qquad
!> C_s=(1-XXI\,PPHI)SOLCAP,\qquad C_t=C_d+C_s.
!> \]
!>
!> Root distribution `PDZF3`, canopy factor `PFTWO/PF2MAX`, maximum uptake
!> `PKMAX`, and `PFONE` distribute uptake into the `EDCAP` and `ESCAP` source
!> terms consumed by [[colm]]. Total uptake is partitioned into the two plant
!> compartments as
!>
!> \[
!> Q=\frac{\sum_k U_k}
!> {PMASS[(1-DELONE)+DELTHR\,DELONE\,PFTWO/PF2MAX]},
!> \]
!> \[
!> QCPAA=(1-DELONE)Q,\qquad
!> QCPBB=DELTHR\,DELONE(PFTWO/PF2MAX)Q.
!> \]
!>
!> Loss of compartment-B mass is recycled to the rooted soil cells before
!> [[plant]] advances `BCPAA` and `BCPBB`. On contaminant 1, parent-generation
!> work arrays are reset for the new chain.
!>
!> @warning The rooted-cell uptake expression divides by `TDUM=C_d+C_s`
!> without a zero guard, and also assumes nonzero `KSP`, `PF2MAX`, `PMASS`, and
!> the uptake-partition denominator.
!> @endwarning
   SUBROUTINE PLCOLM(NCL, NCONT)

      USE cm_parameters
      USE cm_column_scaling
      ! NB COLM.C1 includes AL.P
      USE cm_column_water
      USE cm_column_state
      USE cm_plant_state

      ! Include parameter statements, water/contaminant
      ! interface COMMON blocks, and plant COMMON blocks
      ! called just before routine COLM

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: NCL !! Active land-column element number.
      INTEGER, INTENT(IN) :: NCONT !! Active contaminant number in decay-chain order.

      ! Locals
      INTEGER :: JPLANT, NCE, JPLTY, NRBOT
      DOUBLE PRECISION :: D1DUM, D2DUM, D3DUM, D4DUM, O2DUM, F1DUM, F2DUM, PKDUM
      DOUBLE PRECISION :: PMDUM, SUM, Z2DUM, XDUM, CDUM, SDUM, TDUM, DUM, EDDUM
      DOUBLE PRECISION :: QDUM, BCDUM, DUM1, DUM3, BCPAA1, BCPBB1

      !----------------------------------------------------------------------*

      IF (NCONT == 1) THEN
         ! Set generation variables to zero if call is for first contaminant
         init_gen_loop: DO JPLANT = 1, NPL(NCL)
            GENAA(JPLANT) = ZERO
            GENBB(JPLANT) = ZERO
         END DO init_gen_loop
      END IF

      ! Set uptake variables to zero in preparation for summing net uptake
      ! over all plant types on column NCL
      init_uptake_loop: DO NCE = 1, NCETOP
         EDCAP(NCE) = ZERO
         EDCAPC(NCE) = ZERO
         EDCAPT(NCE) = ZERO
         ESCAP(NCE) = ZERO
         ESCAPS(NCE) = ZERO
         ESCAPT(NCE) = ZERO
      END DO init_uptake_loop

      ! Main calculation loops
      plant_type_loop: DO JPLANT = 1, NPL(NCL)
         ! For each plant type on soil column NCL

         JPLTY = NPLTYP(NCL, JPLANT)
         ! Plant type number

         NRBOT = NCETOP - NRD(JPLTY)
         ! Number of bottom rooted cell

         D1DUM = DELONE(JPLTY)
         D2DUM = DELTWO(JPLTY)
         D3DUM = DELTHR(JPLTY)
         D4DUM = DELFOU(JPLTY)
         O2DUM = ONE - D2DUM
         F1DUM = PFONE(NCL, JPLANT)
         F2DUM = PFTWO(JPLTY)/PF2MAX(JPLTY)
         PKDUM = PKMAX(JPLTY, NCONT)
         PMDUM = PMASS(JPLTY)

         GCPL = GCPLA(NCONT)
         ! Non dimensioned decay variable, set up in MUZ

         GMCPAA = (ONE - D1DUM)
         GMCPBB = F2DUM*D1DUM
         SUM = ZERO
         Z2DUM = Z2SQOD*F2DUM*PKDUM

         rooted_cell_loop: DO NCE = NRBOT, NCETOP
            XDUM = XXI*PPHI(NCE)
            CDUM = XDUM*COLCAP(NCE)
            SDUM = (ONE - XDUM)*SOLCAP(NCE)
            TDUM = CDUM + SDUM
            DUM = Z2DUM*PDZF3(NCL, NCE, JPLANT)*TDUM
            SUM = SUM + DUM

            EDDUM = DUM*F1DUM/(TDUM*(Z2*KSP(NCE)))
            EDCAP(NCE) = EDCAP(NCE) + CDUM*EDDUM
            ESCAP(NCE) = ESCAP(NCE) + SDUM*EDDUM

            ! Set net scaled uptake rates for use in routine COLM
            ! ----- NB sums up over all plant types
            ! ----- NB THE RECYLING TERMS FOR EDCAP AND ESCAP ARE ADDED BELOW
         END DO rooted_cell_loop

         QDUM = SUM/(PMDUM*(GMCPAA + (D3DUM*GMCPBB)))
         QCPAA = GMCPAA*QDUM
         QCPBB = D3DUM*GMCPBB*QDUM

         ! Evaluate scaled values for Qa and Qb using equations in section 3 of WRSRU/TR/9107/12
         GMCBBD = (GMCPBB - GMCBBO(NCL, JPLANT))/TSE
         GMCBBO(NCL, JPLANT) = GMCPBB

         IF (LTZERO(GMCBBD)) THEN
            BCDUM = BCPBB(NCL, JPLANT, NCONT)
            DUM1 = F1DUM*D4DUM*BCDUM*GMCBBD
            DUM3 = O2DUM*PDZF3(NCL, NCETOP, JPLANT)

            EDCAP(NCETOP) = EDCAP(NCETOP) + DUM1*(D2DUM + DUM3)/(Z2*KSP(NCETOP)*RHOPL)

            recycling_loop: DO NCE = NRBOT, NCETOP - 1
               EDCAP(NCE) = EDCAP(NCE) + DUM1*DUM3/(Z2*KSP(NCE)*RHOPL)
            END DO recycling_loop
         END IF

         ! Call solve routine and update concentrations
         CALL PLANT(JPLANT, BCPAA(NCL, JPLANT, NCONT), BCPAA1, &
                    BCPBB(NCL, JPLANT, NCONT), BCPBB1, TSE)

         BCPAA(NCL, JPLANT, NCONT) = BCPAA1
         BCPBB(NCL, JPLANT, NCONT) = BCPBB1

      END DO plant_type_loop

   END SUBROUTINE PLCOLM

!> @brief Advances the two plant contaminant compartments for one plant type.
!>
!> This is the legacy two-compartment difference model identified by the source
!> as WRSRU/TR/9107/12 section 4.
!>
!> Shared values prepared by [[plcolm]] define scaled compartment masses
!> `GMCPAA`/`GMCPBB`, uptake rates `QCPAA`/`QCPBB`, decay `GCPL`, and the
!> compartment-B mass derivative `GMCBBD`. For positive A mass,
!>
!> \[
!> W_A=\frac{RHOPL\,QCPAA+GMCPAA(GENAA-GCPL\,BCAA)}
!> {GMCPAA(1+GCPL\,TSE)},\qquad BCAA1=BCAA+TSE\,W_A.
!> \]
!>
!> Compartment B uses the same decay/generation balance. Nonnegative mass
!> change subtracts `BCBB*GMCBBD` from its numerator; negative mass change
!> subtracts `GMCBBD*TSE` from its denominator. A missing compartment, or a
!> zero B denominator, produces zero concentration. At exit `GENAA` and `GENBB`
!> retain decay from the old concentrations for the next contaminant in the
!> numeric parent/product chain.
   SUBROUTINE PLANT(JPLANT, BCAA, BCAA1, BCBB, BCBB1, TSE)

      USE cm_plant_state

      IMPLICIT NONE

      ! Dummy arguments
      INTEGER, INTENT(IN) :: JPLANT !! Plant occurrence index within the current column.
      DOUBLE PRECISION, INTENT(IN) :: BCAA !! Old compartment-A concentration.
      DOUBLE PRECISION, INTENT(IN) :: BCBB !! Old compartment-B concentration.
      DOUBLE PRECISION, INTENT(IN) :: TSE !! Dimensionless contaminant timestep.
      DOUBLE PRECISION, INTENT(OUT) :: BCAA1 !! Updated compartment-A concentration.
      DOUBLE PRECISION, INTENT(OUT) :: BCBB1 !! Updated compartment-B concentration.

      ! Locals
      DOUBLE PRECISION :: GDUM, WCPAA, TOPDUM, BOTDUM, WCPBB

      !----------------------------------------------------------------------*

      GDUM = ONE + GCPL*TSE

      IF (GTZERO(GMCPAA)) THEN
         WCPAA = (RHOPL*QCPAA + GMCPAA*(GENAA(JPLANT) - GCPL*BCAA))/(GMCPAA*GDUM)
         BCAA1 = BCAA + WCPAA*TSE
      ELSE
         ! No plant mass in compartment A
         BCAA1 = ZERO
      END IF

      IF (GTZERO(GMCPBB)) THEN
         TOPDUM = RHOPL*QCPBB + GMCPBB*(GENBB(JPLANT) - GCPL*BCBB)
         BOTDUM = GMCPBB*GDUM

         IF (GEZERO(GMCBBD)) THEN
            TOPDUM = TOPDUM - BCBB*GMCBBD
         ELSE
            BOTDUM = BOTDUM - GMCBBD*TSE
         END IF

         IF (NOTZERO(BOTDUM)) THEN
            WCPBB = TOPDUM/BOTDUM
            BCBB1 = BCBB + WCPBB*TSE
         ELSE
            BCBB1 = ZERO
         END IF
      ELSE
         ! No mass in compartment B
         BCBB1 = ZERO
      END IF

      ! Decay generation values to be used for next contaminant
      GENAA(JPLANT) = GCPL*BCAA
      GENBB(JPLANT) = GCPL*BCBB

   END SUBROUTINE PLANT

!> @brief Prepares canopy-dependent plant factors for the current timestep.
!>
!> For every plant type, current leaf-area index is copied to `PFTWO`, whose
!> ratio to `PF2MAX` scales uptake in [[plcolm]]. `DELFOU` is set to one while
!> `CLAI` is nonzero and to residual fraction `FLEFT` after canopy loss:
!>
!> \[
!> PFTWO_p=CLAI_p,\qquad
!> DELFOU_p=\begin{cases}1,&CLAI_p\ne0,\\FLEFT_p,&CLAI_p=0.\end{cases}
!> \]
   SUBROUTINE PLPREP

      USE cm_plant_state
      ! Include parameter statements, water/contaminant
      ! interface COMMON blocks, and plant COMMON blocks

      IMPLICIT NONE

      ! Locals
      INTEGER :: JPLTY

      !----------------------------------------------------------------------*

      ! Set f2 delta4 for each plant type
      plant_type_loop: DO JPLTY = 1, NPLT

         PFTWO(JPLTY) = CLAI(JPLTY)

         IF (NOTZERO(PFTWO(JPLTY))) THEN
            DELFOU(JPLTY) = ONE
         ELSE
            DELFOU(JPLTY) = FLEFT(JPLTY)
         END IF

      END DO plant_type_loop

   END SUBROUTINE PLPREP

!> @brief Initialises contaminant plant-uptake arrays.
!>
!> `INPL` initialises the SHETRAN-UK plant contaminant migration component
!> (MPL). The current implementation maps vegetation classes to plant uptake
!> compartments and root fractions, including legacy hard-coded plant parameters.
!>
!> | Plant type | `PMASS` | `PF2MAX` | `PKMAX(:,1)` |
!> |:-----------|--------:|---------:|-------------:|
!> | 1 | 2 | 2 | \(1.5\times10^{-8}\) |
!> | 2 | 3 | 6 | \(3.0\times10^{-8}\) |
!> | 3 | 20 | 10 | \(3.0\times10^{-8}\) |
!>
!> Each non-link element is assigned a primary plant type from `NVC`. The
!> primary plant fraction is `PFONE(:,1)=PLAI(NVC)`. If that fraction is less
!> than 0.99, the routine creates a second plant compartment with fraction
!> `1-PFONE(:,1)`; the second plant type is assumed to have been set elsewhere
!> in legacy block data. Root fractions are copied from `RDF` into `PDZF3` from
!> the top contaminant cell downward, and the old plant compartment-B mass is
!> initialised as
!>
!> \[
!> GMCBBO = \frac{CLAI}{PF2MAX}\,DELONE .
!> \]
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-03-18 | JE | 3.4 | Implemented the MPL plant contaminant migration component initialisation. |
!> @endhistory
   SUBROUTINE INPL

      USE cm_plant_state
      USE cm_column_scaling

      IMPLICIT NONE

      ! Locals
      INTEGER :: NCL, JPLANT, JPLTY, NCE, NDUM
      DOUBLE PRECISION :: D1DUM, RDUM

      NPLT = NV
      ! Number of top cell in column, and number of plant types

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ gp 30/3/93
      pmass(1) = TWO
      pmass(2) = 3.0D0
      pmass(3) = 20.0D0

      pf2max(1) = TWO
      pf2max(2) = 6.0D0
      pf2max(3) = 10.0D0

      pkmax(1, 1) = 1.5D-8
      pkmax(2, 1) = 3.0D-8
      pkmax(3, 1) = 3.0D-8
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ temp. for dsatd2

      column_loop: DO NCL = total_no_links + 1, total_no_elements

         NPLTYP(NCL, 1) = NVC(NCL)
         PFONE(NCL, 1) = PLAI(NPLTYP(NCL, 1))

         IF (PFONE(NCL, 1) >= 0.99D0) THEN
            NPL(NCL) = 1
         ELSE
            PFONE(NCL, 2) = ONE - PFONE(NCL, 1)
            NPL(NCL) = 2
         END IF

         ! ^^^^^^^^^^^^^^^ TEMPORARY
         ! Set number of plant types on each column
         ! Temporarily, only two plant types are allowed on each
         ! column and the total PLAI is one
         ! Second plant type number is set in BLOCK DATA

         plant_loop: DO JPLANT = 1, NPL(NCL)

            JPLTY = NPLTYP(NCL, JPLANT)
            ! Plant type number

            root_density_loop: DO NCE = NCETOP, 2, -1
               NDUM = NCETOP - NCE + 1
               PDZF3(NCL, NCE, JPLANT) = RDF(JPLTY, NDUM)
            END DO root_density_loop
            ! Set root density function

            D1DUM = DELONE(JPLTY)
            RDUM = CLAI(JPLTY)/PF2MAX(JPLTY)

            GMCBBO(NCL, JPLANT) = RDUM*D1DUM
            ! Initialise old value for mass in compartment b

         END DO plant_loop

      END DO column_loop

   END SUBROUTINE INPL

END MODULE cm_plant

