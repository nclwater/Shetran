MODULE OCmod
! Main interface for Overland Channel (OC) module
! This module maintains the same external API as the original monolithic OCmod
! All implementation details have been refactored into separate modules
!
! REFACTORING HISTORY:
! 19 Aug 2025  Refactored large monolithic OCmod.f90.sav (2002 lines, 71KB)
!              into 7 focused modules organized by functionality:
!              - oc_common_data.f90 (shared variables)
!              - oc_initialization.f90 (setup & validation)
!              - oc_simulation.f90 (main computation engine)
!              - oc_compute.f90 (matrix calculations)
!              - oc_input.f90 (data reading)
!              - oc_output.f90 (results output)
!              - oc_utils.f90 (helper functions)
!
!FROM ORIGINAL SPEC_OC HISTORICAL DOCUMENTATION:
!-------------------------- Start of SPEC.OC --------------------------*
!
! ^^^ COMMON FILE OF SPECIFICATIONS OF OC COMPONENT VARIABLES.
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/SPEC.OC/4.2
! Modifications:
!   GP        FEB 89    2.0   'SHE88' IMPLEMENTATION ON NEWCASTLE AMDAHL
!   GP        AUG 89    2.1     ADD LOGICAL BIOWAT
!   GP        NOV 89    2.2     ADD VARIABLES FOR NEW IMPLICIT OC
!   GP        APR 91    3.0     SHETRAN REWRITE
!                               + INCLUDING IMPLICIT SCHEME AND BANKS
!   GP        JAN 92    3.2     ADD WLMIN, DQIST2(NLFEE,3)
!   GP        JUN 92    3.4     VARIABLES MOVED TO AL.D FOR HOTSTART
!                               (arrays QSA,DQ0ST,DQIST,DQIST2).
!  GP  960103  4.0  Move NOCBCC,NOCBCD to AL.D.
! RAH  970221  4.1  Correct spelling: NCATR was NACTR.  Explicit typing.
!                   Remove CFDEB,SURFS,SURFZ,DDDZST (redundant).
! RAH  971215  4.2  Remove LONT (see OCSIM).  Move DD,EE,GG to OCSIM.
!      980107       Move AA,BB,CC,FF to OCSIM (see also OCABC).
!      980119       Amend mod note above (3.4 was 3.3).
!                   Remove PT,TEMPS (see OCINI).  Move DET to OCINI.
!      980120       Remove NCATRE (see OCINI,OCBLOC - deleted).
!                   Move KONT,NCATR,CDRS,CATR,BIOWAT to OCINI.
!      980121       Move NOCBC,NOCPB to OCBC.
!                   Move NDEFCT,NXDEF,XDEFH,XDEFW to OCPLF.
!      980205       Move IXER to OCREAD.
!      980210       Move NXOC to OCIND.
!      980212       Remove NROWFN (see OCSIM,OCIND). Mv WLMIN to OCQMLN.
!                   Increase NROWST size by 1, reduce XCONV,XDERIV by 1.
!      980225       Swap COCBCD subscripts - also in
!                   OCINI,OCREAD,OCBC,OCPLF,OCQLNK,OCQBC.
!      980226       Move DTOC to OCSIM.  Add TDC,TFC (see OCINI,OCSIM).
!      980408       Move ROOT2G to OCQBNK.
!      980424       Merge XSECTH,XCONV,XDERIV into XSTAB
!                   (see OCINI,OCXS,OCSIM).
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*
! Requirements:
!  NXSCEE.ge.2
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE oc_initialization, ONLY: OCINI
   USE oc_simulation, ONLY: OCSIM
   USE oc_utils, ONLY: OCLTL, LINKNO
   USE oc_common_data, ONLY: qfnext, hoclst, hocprv, qocfin, hocnxt, hocnxv
   USE oc_validation, ONLY: OCCHK0, OCCHK1, OCCHK2

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: OCINI, OCSIM, OCLTL, LINKNO, &
      qfnext, hoclst, hocprv, qocfin, hocnxt, hocnxv, &
      OCCHK0, OCCHK1, OCCHK2

END MODULE OCmod
