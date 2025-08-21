MODULE FRmod
! Main interface module for Framework functionality
! This module provides the public interface while delegating to specialized modules
! Created as part of FRmod refactoring - maintains backward compatibility
!
! This is a pure pass-through interface - all implementations are in specialized modules

   USE framework_shared, ONLY: INFR, DINET, DINOC, INET, INSM, INBK, INCM, FRDIM, &
      TITLE, TSH, TCH, BSOFT, BSTORE, qoctot, uzold, btime, next_hour, icounter2
   USE framework_initialization, ONLY: FRINIT, FRLTL
   USE framework_output_manager, ONLY: FROPEN, FROUTPUT, FRRESP
   USE framework_mass_balance, ONLY: FRMB
   USE framework_spatial_setup, ONLY: FRIND
   USE framework_element_sorting, ONLY: FRSORT

   IMPLICIT NONE

   ! Re-export all public interfaces from sub-modules
   PUBLIC :: FROPEN, FRINIT, FRSORT, FROUTPUT, FRMB, FRRESP, FRIND, FRLTL, INCM
   PUBLIC :: INFR, DINET, DINOC, INET, INSM, INBK, FRDIM
   PUBLIC :: qoctot, uzold, bsoft, tsh, tch, bstore, btime, next_hour, icounter2, TITLE

END MODULE FRmod
