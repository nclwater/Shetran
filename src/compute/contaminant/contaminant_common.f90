MODULE contaminant_common
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! [REFACTORING] 19/08/2025 - Extracted from CMmod.f90 as common data module
!                           Contains shared variables, constants and types used across
!                           contaminant transport modules
!
   USE SGLOBAL, ONLY :                                                             &
      nlf=>total_no_links, area=>cellarea, NEL=>total_no_elements,                  &
      NOTZERO, ZERO, ONE, TWO, HALF,                                                &
      ISZERO, GTZERO, LTZERO, GEZERO, DYQQ, DXQQ, ZGRUND, ERROR
   USE OCMOD2,  ONLY : hrf=>hrfzz
   USE AL_C
   USE AL_G
   USE IS_CC
   USE UTILSMOD, ONLY : TRIDAG, DCOPY
   USE mod_load_filedata, ONLY: ALALLI, ALINIT, ALREDC, ALREDF, ALREDI, ALREDL, ALRED2

   IMPLICIT NONE

   ! Module variables - shared across contaminant modules
   INTEGER :: JBK, JFLINK, JSOL(LLEE), NWORK(4), NLINKA, NCWELL
   DOUBLEPRECISION :: VELDUM (LLEE), QQQWEL, QQQWL1, QQRV(LLEE), ROH(LLEE)
   LOGICAL :: ISBDY (4)

   INTEGER:: count = 0
   INTEGER :: LWORK(6), NBK(2), nwell
   LOGICAL :: islk(2)
   DOUBLEPRECISION ::  qqqdum, QQQSL1

   ! Legacy COMMON block variables (kept for compatibility)
   !COMMON / WTOCI / JBK, JFLINK, JSOL, NWORK, NLINKA, NCWELL
   !COMMON / WTOC / VELDUM, QQQWEL, QQQWL1, QQRV, ROH
   !COMMON / WTOCL / ISBDY
   !COMMON / LK1 / ISLK (2), LWORK (6), NBK (2), qqqdum, QQQSL1
   !common / temp / nwell
   !!##### nwell and qqqdum used in temporary irrigation code###########

   ! Make all variables public for use in other contaminant modules
   PUBLIC

END MODULE contaminant_common
