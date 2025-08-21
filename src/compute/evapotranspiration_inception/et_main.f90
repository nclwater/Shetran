MODULE et_main
!----------------------------------------------------------------------*
!
! Main controller for evapotranspiration calculations
! Contains ETSIM subroutine - the main entry point
!
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE et_variables
   USE et_integration
   USE AL_G,     ONLY : ICMREF, NGDBGN
   USE AL_C,     ONLY : DTUZ, UZNEXT, CWIDTH, FHBED, NLYRBT, vspsi, NHBED
   USE AL_D,     ONLY : TIMEUZ, BWIDTH, HRUZ
   USE mod_load_filedata, ONLY : ALINIT
   USE UTILSMOD, ONLY : DCOPY
   USE OCMOD2,   ONLY : getHRF

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: ETSIM

CONTAINS

!SSSSSS SUBROUTINE ETSIM
   SUBROUTINE ETSIM ()
!----------------------------------------------------------------------*
! Controlling routine for evapotranspiration/interception module
!----------------------------------------------------------------------*
! Version:  SHETRAN/ET/ETSIM/4.2
! Modifications:
!  GP  08.08.94  written (v4.0 finished 3/10/95)
! RAH  970516  4.1  Swap VSPSI indices. Amend comments. Explicit typing.
! RAH  981103  4.2  Scrap AL.D output NSOIL (see ET).
!                   Replace DO-loops with calls to ALINIT & DCOPY.
!----------------------------------------------------------------------*
! Commons and constants
!
! Locals, etc
      INTEGER :: ICE, IEL, IL, ITYPE
      DOUBLEPRECISION ALFA

      DTUZ = UZNEXT * 3600.0D0
      TIMEUZ = TIMEUZ + UZNEXT

! Loop over land-elements
      DO IEL = NGDBGN, total_no_elements
         ITYPE = ICMREF (IEL, 1)
         IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) THEN
            IL = ICMREF (IEL, 4)
            ALFA = 0.5 * CWIDTH (IL) / BWIDTH
            ICE = NHBED (IL, ITYPE) + 2
            CALL ALINIT (ALFA, ICE-2, UZALFA)
            UZALFA (ICE-1) = ALFA * FHBED (IL, ITYPE)
         ELSE
            ICE = 1
         ENDIF

         IF (ICE.LE.top_cell_no) CALL ALINIT (ZERO, top_cell_no - ICE+1, UZALFA (ICE) )

         HRUZ = getHRF(IEL) - ZGRUND (IEL)
         ICE = NLYRBT (IEL, 1)

         CALL DCOPY (top_cell_no - ICE+1, VSPSI (ICE, IEL), 1, PSI4 (ICE), &
            1)

         CALL ETIN (IEL)

      END DO
   END SUBROUTINE ETSIM

END MODULE et_main
