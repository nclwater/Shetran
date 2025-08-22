!MMMMMM MODULE hydraulic_variables
MODULE hydraulic_variables
! Extracted from OCQDQMOD.F90 - Common variables for hydraulic flow calculations
! JE  1/09   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactored: 2025-08-22 - Split from OCQDQMOD for better organization

   USE SGLOBAL
   USE AL_C ,     ONLY : ICMRF2, CWIDTH, DHF, ZBFULL, CLENTH
   USE AL_G ,     ONLY : ICMREF, ICMXY
   USE AL_D ,     ONLY : DQ0ST, DQIST, DQIST2, NOCBCC, NOCBCD, NoZQTables,ZQTableRef, ZQTableLink,ZQTableFace
   USE OCmod2 ,   ONLY : GETHRF, OCQMLN, SETQSA, OCQBNK, OCQGRD, OCQLNK, OCQBC

   IMPLICIT NONE

   ! Module variables for hydraulic flow calculations
   DOUBLEPRECISION    :: XAFULL(NLFEE), COCBCD(5, NOCTAB)
   DOUBLEPRECISION    :: HOCNOW (NOCTAB), QOCF (NOCTAB)
   DOUBLEPRECISION    :: STRXX(NELEE), STRYY(NELEE)
!LOGICAL            :: firstocqdq=.TRUE.

   PUBLIC :: XAFULL, COCBCD, HOCNOW, QOCF, STRXX, STRYY

END MODULE hydraulic_variables
