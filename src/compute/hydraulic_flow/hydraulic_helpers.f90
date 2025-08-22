!MMMMMM MODULE hydraulic_helpers
MODULE hydraulic_helpers
! Extracted from OCQDQMOD.F90 - Helper functions for hydraulic flow calculations
! Refactored: 2025-08-22 - Split from OCQDQMOD for better organization

   USE SGLOBAL
   USE hydraulic_variables, ONLY : STRXX, STRYY

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: fstr, fdqq

CONTAINS

!FFFFFF FUNCTION fstr
   FUNCTION fstr(jel,face) RESULT(r)
      ! Calculate stress/roughness parameter based on element and face direction
      ! For faces 1&3 (north/south): use STRXX, for faces 2&4 (east/west): use STRYY
      INTEGER, INTENT(IN) :: jel, face
      DOUBLEPRECISION     :: r
!mult = DBLE(MOD(face, 2))
!r    = mult * strxx(jel) + (one-mult) * stryy(jel)
      IF(face==1 .OR. face==3) THEN
         r = strxx(jel)
      ELSE
         r = stryy(jel)
      ENDIF
   END FUNCTION fstr


!FFFFFF FUNCTION fdqq
   FUNCTION fdqq(jel, face) RESULT(r)
      ! Calculate grid spacing parameter based on element and face direction
      ! For faces 1&3 (north/south): use DYQQ, for faces 2&4 (east/west): use DXQQ
      INTEGER, INTENT(IN) :: jel, face
      DOUBLEPRECISION     :: r
!mult = DBLE(MOD(face,2))
!r    = mult * dyqq(jel) + (one-mult) * dxqq(jel)
      IF(face==1 .OR. face==3) THEN
         r = dyqq(jel)
      ELSE
         r = dxqq(jel)
      ENDIF
   END FUNCTION fdqq

END MODULE hydraulic_helpers
