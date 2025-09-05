MODULE contaminant_interface
!----------------------------------------------------------------------*
!
! Interface module for contaminant transport system
! Provides wrapper subroutines that maintain the original CMmod interface
! while delegating to the refactored implementation modules
!
!----------------------------------------------------------------------*
! [REFACTORING] 05/09/2025 - Created as interface wrapper module
!                           Maintains compatibility with existing code
!                           that calls CMSIM, CMFIN, and CMRD directly
!----------------------------------------------------------------------*

   USE contaminant_common
   USE contaminant_data_reader
   USE contaminant_simulation

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: CMSIM, CMFIN, CMRD

CONTAINS

   !SSSSSS SUBROUTINE CMFIN
   SUBROUTINE CMFIN
      !----------------------------------------------------------------------*
      !                             CALLED FROM WATER FLOW COMPONENTS.
      !                             TIDIES UP AT END OF SIMULATION.
      !----------------------------------------------------------------------*
      ! [REFACTORING] 05/09/2025 - Moved from CMmod.f90 interface module
      !                           Delegates to contaminant_simulation module
      !----------------------------------------------------------------------*

      ! Delegate to simulation module
      CALL cm_finalize()
   END SUBROUTINE CMFIN

   !SSSSSS SUBROUTINE CMRD (CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, &
   SUBROUTINE CMRD (CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, &
      NSEE, NS, NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY, NLYRBE, ICMXY, &
      ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  NCATTY, NCON, NCOLMB, NTAB, &
      DBS, DBDI, CCAPI, CCAPE, CCAPR, CCAPB,TABLE_CONCENTRATION, TABLE_WATER_DEPTH, IIICF, SOFN, &
      GNN, GGLMSO, ALPHBD, ALPHBS, KDDLS, ALPHA, FADS, &
      ISCNSV, IDUM, DUMMY)
      !----------------------------------------------------------------------*
      !                             READ CONTAMINANT DATA INPUT FILE
      !----------------------------------------------------------------------*
      ! [REFACTORING] 05/09/2025 - Moved from CMmod.f90 interface module
      !                           Delegates to contaminant_data_reader module
      !----------------------------------------------------------------------*

      ! Input arguments
      INTEGER, INTENT(IN) :: CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, NSEE, NS
      INTEGER, INTENT(IN) :: NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY
      INTEGER, INTENT(IN) :: ICMXY (NXEE, NY), ICMBK (NLFEE, 2), ICMREF (NELEE, 4, 2:2)
      INTEGER, INTENT(IN) :: NLYRBE (NLF + 1:NEL)
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS (NLFEE)
      !
      ! Output arguments
      INTEGER, INTENT(OUT) :: NCON
      INTEGER, INTENT(OUT) :: NUM_CATEGORIES_TYPES (NCONEE), NCATTY (NELEE, NCONEE)
      INTEGER, INTENT(OUT) :: NCOLMB (NLF + 1:NEL), NTAB (MAX_NUM_CATEGORY_TYPES, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: DBS, DBDI
      DOUBLEPRECISION, INTENT(OUT) :: CCAPI (NCONEE), CCAPE (NELEE, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: CCAPR (NELEE, NCONEE), CCAPB (NELEE, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: TABLE_CONCENTRATION (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: TABLE_WATER_DEPTH (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: IIICF (NCONEE), SOFN (NSEE, 3)
      DOUBLEPRECISION, INTENT(OUT) :: GNN (NCONEE), GGLMSO (NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: ALPHBD (NCONEE), ALPHBS (NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: KDDLS (NSEDEE, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: ALPHA (NSEE, NCONEE), FADS (NSEE, NCONEE)
      LOGICAL, INTENT(OUT) :: ISCNSV (NCONEE)
      !
      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT) :: IDUM
      DOUBLEPRECISION, INTENT(INOUT) :: DUMMY (NELEE)
      !
      ! Delegate to data reader module
      CALL cm_read_data(CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, &
         NSEE, NS, NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY, NLYRBE, ICMXY, &
         ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  NCATTY, NCON, NCOLMB, NTAB, &
         DBS, DBDI, CCAPI, CCAPE, CCAPR, CCAPB,TABLE_CONCENTRATION, TABLE_WATER_DEPTH, IIICF, SOFN, &
         GNN, GGLMSO, ALPHBD, ALPHBS, KDDLS, ALPHA, FADS, &
         ISCNSV, IDUM, DUMMY)
   END SUBROUTINE CMRD

   !SSSSSS SUBROUTINE CMSIM (ISSDON)
   SUBROUTINE CMSIM (ISSDON)
      !----------------------------------------------------------------------*
      !                             ENTRY POINT TO THE CONTAMINANT COMPONENTS
      !                             WHEN UPDATING THE CONTAMINANT
      !                             CONCENTRATIONS FOR THE WHOLE CATCHMENT
      !                             FOR ONE TIME STEP
      !----------------------------------------------------------------------*
      ! [REFACTORING] 05/09/2025 - Moved from CMmod.f90 interface module
      !                           Delegates to contaminant_simulation module
      !----------------------------------------------------------------------*

      LOGICAL, INTENT(IN) :: ISSDON
      !                             ANSWER TO: IS SEDIMENT CODE ACTIVE?

      ! Delegate to simulation module
      CALL cm_simulate_timestep(ISSDON)
   END SUBROUTINE CMSIM

END MODULE contaminant_interface
