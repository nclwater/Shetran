MODULE OCmod2
! Main interface module for overland channel flow calculations
! Provides backward compatibility with original OCmod2 interface
! This module has been refactored into separate functional modules:
!   - oc_parameters: Mathematical and physical constants
!   - oc_data_management: Water surface elevations, discharges, cross-section tables
!   - oc_node_flows: Flow calculations at confluences and junctions  
!   - oc_hydraulic_calculations: Conveyance calculations and weir flow equations
!   - oc_channel_flow_types: Different types of channel flow calculations
!   - oc_flow_control: Flow control and mass conservation
!
! Original: JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces part of the OC.F files

USE SGLOBAL
USE oc_parameters
USE oc_data_management
USE oc_node_flows
USE oc_hydraulic_calculations  
USE oc_channel_flow_types
USE oc_flow_control

IMPLICIT NONE

! Re-export all public interfaces for backward compatibility
PUBLIC :: GETHRF, SETHRF, GETQSA, SETQSA, GETQSA_ALL, & ! Data management
          OCNODE, FNODE, &                               ! Node flows
          OCCODE, CONVEYAN, QWEIR, &                     ! Hydraulic calculations
          OCQBC, OCQBNK, OCQGRD, OCQLNK, OCQMLN, &      ! Channel flow types
          OCFIX, &                                       ! Flow control
          XSTAB, hrfzz, qsazz, &                        ! Data arrays
          initialise_ocmod                               ! Initialization

CONTAINS

! Provide a unified initialization interface
SUBROUTINE initialise_ocmod()
    CALL initialise_oc_data()
END SUBROUTINE initialise_ocmod

END MODULE OCmod2
