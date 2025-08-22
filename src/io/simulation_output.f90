MODULE simulation_output
! Extracted from rest.f90 - Error reporting and completion status
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactored: 2025-08-22 - Split from rest module for better organization

   USE SGLOBAL
   USE AL_D,    ONLY : flerrc, balanc, syerrc, cmerrc, nstep, carea

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: extra_output

CONTAINS

!SSSSSS SUBROUTINE extra_output
   SUBROUTINE extra_output()
      INTEGER :: i
      DOUBLEPRECISION    :: car
      WRITE(PPPRI, 1400)
      DO I = 0, 100
         IF (FLERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 1000, FLERRC (I)
      END DO
      DO I = 0, 100
         IF (SYERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 2000, SYERRC (I)
      END DO
      DO I = 0, 100
         IF (CMERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 3000, CMERRC (I)
      END DO
      WRITE(PPPRI, 1600)
1400  FORMAT(// 'Error message asummary'/)
1500  FORMAT('No. of occurences of error number ',I4,': ',I6)

1600  FORMAT(/ 'End of error message asummary')
!<<<
      WRITE(PPPRI, '(////)')
      WRITE(PPPRI, 9900) UZNOW, NSTEP
!
      WRITE ( *, * )

      WRITE ( *, *) 'Normal completion of SHETRAN run'
!^^^^^sb 250105 mass balnce output
      WRITE(PPPRI, '(////)')
      WRITE(PPPRI,  * ) ' Spatially Averaged Totals (mm) over the simulation'
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Prec = ', balanc (7) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Can. Evap = ', balanc (8) * 1000 / &
         carea
      car = carea
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Soil+Sur Evp = ', balanc (9) &
         * 1000 / car
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Trans = ', balanc (10) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Aqu. Flow = ', balanc (11) &
         * 1000 / carea

      WRITE(PPPRI, '(A20,F10.2)') 'Cum Discharge = ', balanc (12) &
         * 1000 / carea
      WRITE(PPPRI, '(//)')
      WRITE(PPPRI,  * ) ' Storage totals (mm) at the end of the simulation'
      WRITE(PPPRI, '(A20,F10.2)') 'Canopy Stor = ', balanc (13) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Snow Store = ', balanc (14) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Subsur Stor = ', balanc (15) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Surface Stor = ', balanc (16) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Channel Stor = ', balanc (17) * 1000 / &
         carea
9900  FORMAT ('Normal completion of SHETRAN run: ',F10.2, ' hours, ', &
      &        I7,' steps.' /)
   END SUBROUTINE extra_output

END MODULE simulation_output
