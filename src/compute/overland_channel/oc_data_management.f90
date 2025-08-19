MODULE oc_data_management
! Module for data management in overland channel flow calculations
! Handles water surface elevations, discharges, and cross-section tables
! Extracted from OCmod2.f90 as part of refactoring

   USE SGLOBAL
   IMPLICIT NONE

! Data arrays for water surface elevations and discharges
   DOUBLEPRECISION, DIMENSION(NELEE)          :: HRFZZ    ! water surface elevation - here for data abstraction AD
   DOUBLEPRECISION, DIMENSION(NELEE,4)        :: qsazz    ! discharge elevation - here for data abstraction AD
   DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: xstab ! cross-section table

   PRIVATE
   PUBLIC :: GETHRF, SETHRF, GETQSA, SETQSA, GETQSA_ALL, XSTAB, &
      hrfzz, qsazz, initialise_oc_data

CONTAINS

!FFFFFF DOUBLEPRECISION FUNCTION gethrf
   DOUBLEPRECISION FUNCTION gethrf(i)
      INTEGER, INTENT(IN) :: i
      gethrf = hrfzz(i)
   END FUNCTION gethrf

!SSSSSS SUBROUTINE sethrf
   SUBROUTINE sethrf(i,v)
      INTEGER, INTENT(IN)         :: i
      DOUBLEPRECISION, INTENT(IN) :: v
      hrfzz(i) = v
   END SUBROUTINE sethrf

!FFFFFFR DOUBLEPRECISION FUNCTION getqsa
   DOUBLEPRECISION FUNCTION getqsa(i,j)
      INTEGER, INTENT(IN) :: i, j
      getqsa = qsazz(i,j)
   END FUNCTION getqsa

!SSSSSS SUBROUTINE setQSA
   SUBROUTINE setqsa(i,j, v)
      INTEGER, INTENT(IN)         :: i, j
      DOUBLEPRECISION, INTENT(IN) :: v
      qsazz(i,j) = v
   END SUBROUTINE setqsa

!FFFFFFR DOUBLEPRECISION FUNCTION getqsa_all
   FUNCTION getqsa_all(n)
      INTEGER, INTENT(IN)             :: n
      DOUBLEPRECISION, DIMENSION(n,4) :: getqsa_all
      getqsa_all = qsazz(1:n,:)
   END FUNCTION getqsa_all

!SSSSSS SUBROUTINE initialise_oc_data
   SUBROUTINE initialise_oc_data()
      ALLOCATE(xstab(3,nxscee,total_no_links))
   END SUBROUTINE initialise_oc_data

END MODULE oc_data_management
