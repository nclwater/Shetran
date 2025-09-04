MODULE SYmod
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Replaces the SY .F files
!
! REFACTORED: This module now serves as a pure interface to the
!             modularized sediment yield calculation modules.
!             Original functionality preserved in src/compute/sediment/

   ! Import all functionality from the modular sediment modules
   USE sediment_common
   USE sediment_initialization
   USE sediment_transport_capacity
   USE sediment_erosion
   USE sediment_flow_dynamics
   USE sediment_bed_processes
   USE sediment_integration

   IMPLICIT NONE

   ! Re-export all public interfaces to maintain backward compatibility
   PUBLIC

END MODULE SYmod
