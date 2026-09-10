!> summary: One subsurface timestep of contaminant transport, for every active contaminant.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> [[CMSIM]] advances all active contaminants through the catchment for one VSS
!> timestep. For each element in `ISORT` order it runs the column path
!> ([[cm_column:COLMW]] then [[cm_column:COLMSM]]) or the channel path
!> ([[cm_channel:LINKW]] then [[cm_channel:LINKSM]]), and it is the only
!> routine [[simulation_driver:SIMULATION]] calls in the component.
!>
!> Elements are processed **serially**, and that is not incidental: the column
!> and channel solvers share the mutable work arrays in the `cm_column_*` and
!> `cm_link_*` modules, which hold one column or link at a time rather than
!> independent state per element. Contaminants are processed in numeric order
!> so that a decay product can use the immediately preceding contaminant as its
!> parent.
!>
!> When mineral nitrogen is enabled, the first `CMSIM` call runs
!> [[mn_driver:MNINITIALISE]] and later calls run [[mn_driver:MNCONT]] before
!> transport; [[cm_column:COLMSM]] substitutes the resulting `SSS1`/`SSS2`
!> source and sink terms into the column equations. Sediment transport supplies
!> the link sediment fluxes when it is active; otherwise `CMSIM` derives only
!> the water-flow directions.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993--1998 | GP / RAH / SB | 3.4--4.2 | Developed and reorganised the contaminant transport routines. |
!> | 2008-12 | JE | 4.3.5F90 | Created `CMmod` while converting the former CM `COLM` and `LINK` Fortran sources to Fortran 90. |
!> | 2020-03-05 | SvB | - | Replaced the complete `SGLOBAL` include with selected imports. |
!> | 2026-09-10 | SvB | - | Split out of CMmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE cm_driver

   USE simulation_clock, ONLY: DTUZ, TIH, UZNOW
   USE element_geometry, ONLY: DXQQ, DYQQ, ISORT, NEL => total_no_elements, &
                              nlf => total_no_links
   USE grid_topology, ONLY: ICMREF, ICMXY, NX, NY
   USE channel_geometry, ONLY: BEXBK, ICMBK, LINKNS
   USE file_units, ONLY: MND, MNFC, MNFN, MNOUT1, MNOUT2, MNOUTPL, MNPL, MNPR
   USE et_state, ONLY: CLAI, NRD, NV, NVC, PLAI, PNETTO, RDF
   USE vs_state, ONLY: DELTAZ, NLYR, NLYRBT, NS, NTSOIL, QVSWEL, VSPOR, VSPSI, VSTHE, ZVSNOD
   USE oc_state, ONLY: QOC
   USE sy_state, ONLY: QLINK
   USE AL_D, ONLY: TA
   USE cm_parameters, ONLY: CCCC, CCCCO, NCON, SSSS, SSSSO
   USE cm_column_geometry, ONLY: NCOLMB
   USE cm_column_previous, ONLY: RSZWLO, VSTHEO
   USE cm_column_scaling, ONLY: D0, NCETOP, Z2, Z2SQ
   USE cm_plant_state, ONLY: DELONE
   USE cm_solver_flags, ONLY: ISMN, ISPLT
   USE cm_column, ONLY: COLMSM, COLMW
   USE cm_channel, ONLY: LINKSM, LINKW
   USE cm_plant, ONLY: PLPREP
   USE mn_driver, ONLY: MNCONT, MNINITIALISE, MNISINITIALISED

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CMSIM

CONTAINS

!> @brief Advances every active contaminant through the catchment for one timestep.
!>
!> If `ISSDON` is false, the routine first reconstructs the two link-end flows
!> from `QOC`: north--south links use `(-QOC(:,2),QOC(:,4))`, and east--west
!> links use `(-QOC(:,1),QOC(:,3))`. It then establishes the dimensionless
!> contaminant timestep
!>
!> \[
!> TSE = D0\,DTUZ/Z2SQ .
!> \]
!>
!> When `ISMN` is true, the first call performs [[mn_driver:MNINITIALISE]] but does
!> not advance the MN processes; later calls run [[mn_driver:MNCONT]]. The optional
!> plant preparation follows, after which `ISORT` determines the serial sweep:
!> land elements call [[colmw]] then [[colmsm]], and links call [[linkw]] then
!> [[linksm]]. Finally the current link and column concentrations are copied to
!> `CCCCO`/`SSSSO` for the next time level. `RSZWLO` is refreshed from `QVSWEL`
!> inside every contaminant pass, so the same assignment is repeated `NCON`
!> times for each land element.
!>
!> @note The initialization-only first MN call is intentional legacy behaviour.
!> That contaminant solve uses the zero source/sink terms produced by
!> `MNINITIALISE`; the first MN process timestep occurs on the following
!> `CMSIM` call.
!> @endnote
!>
!> @warning Plant preparation is gated by the currently unassigned `ISPLT`
!> module flag described in [[cm_solver_flags]].
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought the former `AL.P` implicit declarations into the routine. |
!> | 1995-03-22 | RAH | 4.0 | Replaced `RSZWEL` with `QVSWEL` for VSS. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> | 2025-09-23 | SB | - | Added the mineral-nitrogen call and source/sink coupling. |
!> @endhistory
   SUBROUTINE CMSIM(ISSDON)

      ! Commons and constants
      USE sy_state, ONLY: QLINK
      USE cm_parameters
      USE cm_column_scaling
      USE cm_column_previous
      USE cm_column_geometry
      USE cm_link_water
      USE cm_plant_state
      USE simulation_clock, ONLY: UZNOW
      USE AL_D, ONLY: TA

      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: ISSDON !! True when sediment transport has already supplied current link flows.

      ! Locals
      INTEGER :: NLINK, NDUM, NELM, NCONT, NCE

      !----------------------------------------------------------------------*

      ! IF THE SEDIMENT CODE IS NOT RUNNING, SET UP FLOWS INTO LINKS
      IF (.NOT. ISSDON) THEN
         link_flow_loop: DO NLINK = 1, NLF
            IF (LINKNS(NLINK)) THEN
               QLINK(NLINK, 1) = -QOC(NLINK, 2)
               QLINK(NLINK, 2) = QOC(NLINK, 4)
            ELSE
               QLINK(NLINK, 1) = -QOC(NLINK, 1)
               QLINK(NLINK, 2) = QOC(NLINK, 3)
            END IF
         END DO link_flow_loop
      END IF

      ! SET NON-DIMENSIONED TIME STEP
      TSE = D0*DTUZ/Z2SQ

      ! SB 230925 call nitrate component
      IF (ismn) THEN
         ! Modern Fix: Replaced 'ICMREF(1,5)' with explicit array slice 'ICMREF(1:NEL, 5)'
         ! to prevent rank-mismatch and AD aliasing compiler crashes.
         IF (.NOT. MNISINITIALISED()) THEN
            CALL MNINITIALISE(MND, MNFC, MNFN, MNPL, MNPR, MNOUTPL, NCETOP, NCON, NEL, NLF, NS, NV, NX, NY, &
                              ICMBK, ICMREF(1:NEL, 5), ICMXY, NCOLMB, NLYR, NVC, NLYRBT, NTSOIL, &
                              D0, TIH, RHOPL, Z2, DELONE, DXQQ, DYQQ, VSPOR, DELTAZ, PLAI, ZVSNOD, &
                              BEXBK, LINKNS, CLAI, TA, SSS1, SSS2)
         ELSE
            CALL MNCONT(MNFC, MNFN, MNPR, MNOUT1, MNOUT2, NCETOP, NEL, NLF, NS, NV, NX, NY, &
                        ICMBK, ICMREF(1:NEL, 5), ICMXY, NCOLMB, NLYR, NRD, NLYRBT, NTSOIL, &
                        D0, TIH, RHOPL, Z2, DELONE, DXQQ, DYQQ, VSPOR, DELTAZ, RDF, ZVSNOD, BEXBK, LINKNS, &
                        DTUZ, uznow, CLAI, CCCC, PNETTO, SSSS, TA, VSPSI, VSTHE, VSTHEO, SSS1, SSS2)
         END IF
      END IF

      ! Prepare for plant uptake calculations
      IF (ISPLT) CALL PLPREP

      ! STEP THROUGH COLUMNS AND LINKS UPDATING THE CONCENTRATIONS IN THE
      ! CATCHMENT ARRAYS CCCC AND SSSS
      update_loop: DO NDUM = 1, NEL
         NELM = ISORT(NDUM)
         IF (NELM > NLF) THEN
            CALL COLMW(NELM)
            CALL COLMSM(NELM)
         ELSE
            CALL LINKW(NELM)
            CALL LINKSM(NELM)
         END IF
      END DO update_loop

      ! SAVE THE NEW CONCENTRATIONS, FOR THE ENTIRE CATCHMENT, FOR USE AT THE NEXT TIME LEVEL
      ! High-Performance Fix: Replaced inner 'DO 12/14 NCE' loops with vectorized array slices
      contaminant_loop: DO NCONT = 1, NCON

         link_save_loop: DO NELM = 1, NLF
            DO NCE = NCETOP - 2, NCETOP
               CCCCO(NELM, NCE, NCONT) = CCCC(NELM, NCE, NCONT)
            END DO
         END DO link_save_loop

         column_save_loop: DO NELM = NLF + 1, NEL
            ! Put here temporarily after introduction of irrigation
            RSZWLO(NELM) = QVSWEL(NELM)

            DO NCE = NLYRBT(NELM, 1), NCETOP
               CCCCO(NELM, NCE, NCONT) = CCCC(NELM, NCE, NCONT)
               SSSSO(NELM, NCE, NCONT) = SSSS(NELM, NCE, NCONT)
            END DO
         END DO column_save_loop

      END DO contaminant_loop

   END SUBROUTINE CMSIM

END MODULE cm_driver

