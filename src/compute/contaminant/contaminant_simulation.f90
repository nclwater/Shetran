MODULE contaminant_simulation
! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.
! RAH  950322  4.0  New VSS: replace RSZWEL with QVSWEL.
! RAH  970313  4.1  Explicit typing.
! [REFACTORING] 19/08/2025 - Extracted from CMmod.f90 as simulation module
!                           Contains main CMSIM routine and simulation logic
!
   USE contaminant_common
   USE contaminant_column_solver
   USE contaminant_link_solver
   ! TODO: Re-enable plant functionality when properly integrated
   ! USE contaminant_plant
   USE SED_CS
   USE CONT_CC
   USE COLM_C1
   USE COLM_CO
   USE LINK_CW

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: cm_simulate_timestep, cm_finalize

CONTAINS

   !SSSSSS SUBROUTINE CM_SIMULATE_TIMESTEP (ISSDON)
   SUBROUTINE cm_simulate_timestep (ISSDON)
      !----------------------------------------------------------------------*
      !                             ENTRY POINT TO THE CONTAMINANT COMPONENTS
      !                             WHEN UPDATING THE CONTAMINANT
      !                             CONCENTRATIONS FOR THE WHOLE CATCHMENT
      !                             FOR ONE TIME STEP
      !----------------------------------------------------------------------*
      ! Version:  /SHETRAN/MUZ/CMSIM/4.1
      ! Modifications:
      ! RAH  941003 3.4.1 Bring IMPLICIT from AL.P.
      ! RAH  950322  4.0  New VSS: replace RSZWEL with QVSWEL.
      ! RAH  970313  4.1  Explicit typing.
      !----------------------------------------------------------------------*

      ! Input arguments
      LOGICAL, INTENT(IN) :: ISSDON
      !                             ANSWER TO: IS SEDIMENT CODE ACTIVE?
      !
      ! Locals, etc
      INTEGER :: NLINK, NDUM, NELM, NCONT, NCE
      !
      !----------------------------------------------------------------------*
      !
      IF (.NOT.ISSDON) THEN
         DO 100 NLINK = 1, NLF
            IF (LINKNS (NLINK) ) THEN
               QLINK (NLINK, 1) = - QOC (NLINK, 2)
               QLINK (NLINK, 2) = QOC (NLINK, 4)
            ELSE
               QLINK (NLINK, 1) = - QOC (NLINK, 1)
               QLINK (NLINK, 2) = QOC (NLINK, 3)
            ENDIF
100      END DO
      ENDIF
      !                             IF THE SEDIMENT CODE IS NOT RUNNING, SET
      !                             UP FLOWS INTO LINKS

      TSE = D0 * DTUZ / Z2SQ
      !                            SET NON-DIMENSIONED TIME STEP

      ! TODO: Re-enable plant preparation when plant module is integrated
      ! IF (ISPLT) CALL PLPREP
      !                            Prepare for plant uptake calculations
      DO 1 NDUM = 1, NEL
         NELM = ISORT (NDUM)
         IF (NELM.GT.NLF) THEN
            CALL COLMW (NELM)
            CALL COLMSM (NELM)
         ELSE
            CALL LINKW (NELM)
            CALL LINKSM (NELM)
         ENDIF
1     END DO
      !                             STEP THROUGH COLUMNS AND LINKS
      !                             UPDATING THE CONCENTRATIONS IN THE
      !                             CATCHMENT ARRAYS CCCC AND SSSS
      DO 10 NCONT = 1, NCON
         DO 11 NELM = 1, NLF
            DO 12 NCE = NCETOP - 2, NCETOP
               CCCCO (NELM, NCE, NCONT) = CCCC (NELM, NCE, NCONT)
12          END DO
11       END DO
         DO 13 NELM = NLF + 1, NEL
            !#######################################################################
            RSZWLO (NELM) = QVSWEL (NELM)
            !                               put here temporarily after introduction
            !                                of irrigation
            !#######################################################################
            DO 14 NCE = NLYRBT (NELM, 1), NCETOP
               CCCCO (NELM, NCE, NCONT) = CCCC (NELM, NCE, NCONT)
               SSSSO (NELM, NCE, NCONT) = SSSS (NELM, NCE, NCONT)
14          END DO
13       END DO
10    END DO
      !                             SAVE THE NEW CONCENTRATIONS, FOR THE
      !                             ENTIRE CATCHMENT, FOR USE AT THE NEXT
      !                             TIME LEVEL
   END SUBROUTINE cm_simulate_timestep

   !SSSSSS SUBROUTINE CM_FINALIZE
   SUBROUTINE cm_finalize
      !----------------------------------------------------------------------*
      !                             CALLED FROM WATER FLOW COMPONENTS.
      !                             TIDIES UP AT END OF SIMULATION.
      !----------------------------------------------------------------------*
      ! [REFACTORING] 05/09/2025 - Moved from CMmod.f90 interface module
      !                           Handles contaminant simulation cleanup
      !----------------------------------------------------------------------*

      ! Currently no cleanup operations needed
      ! This routine is maintained for future expansion and API compatibility
      RETURN
   END SUBROUTINE cm_finalize

END MODULE contaminant_simulation
