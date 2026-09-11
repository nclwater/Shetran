!> summary: The end-of-run summary written to the print file.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[extra_output]] writes the closing summary: the catchment area, the number
!> of timesteps taken, the final water-balance totals from
!> [[water_balance]], and the per-component error counts. [[shetran]] calls it
!> once, after the simulation returns.
!>
!> @warning
!> The three `*ERRC` error-count arrays it reports are in
!> [[legacy_retained]] and have no current producer, so the counts it prints
!> are whatever their storage happened to contain.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP / RAH | 2.0-4.2 | Developed the frame driver, the meteorological reader and the timestep control. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the remaining frame `.F` files to Fortran 90. |
!> | 2015-2026 | SB / SvB | 4.5-4.6 | Added the separate temperature streams, the dated meteorological reader and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of rest; see docs/rename/proposal.md. |
!> @endhistory
MODULE run_summary

   USE element_geometry, ONLY: CAREA
   USE simulation_clock, ONLY: NSTEP, UZNOW
   USE file_units, ONLY: FID_logfile
   USE legacy_retained, ONLY: CMERRC, FLERRC, SYERRC
   USE water_balance, ONLY: BALANC

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: extra_output

CONTAINS

!> Writes end-of-run error counts and spatially averaged water-balance summaries.
!>
!> `extra_output` is called once after the simulation loop completes. It
!> prints the `FLERRC`/`SYERRC`/`CMERRC` flow, sediment, and contaminant error
!> counters (indices 0-100, offset by 1000/2000/3000 respectively for the
!> printed error number), the normal-completion line to standard output, and
!> catchment-averaged cumulative-flux and end-of-run storage totals to the
!> `.pri` output, using [[water_balance]]'s `BALANC` accumulator and `CAREA`:
!>
!> | `BALANC` index | Quantity |
!> |:---------------|:---------|
!> | 7 | Cumulative precipitation |
!> | 8 | Cumulative canopy evaporation |
!> | 9 | Cumulative soil/surface evaporation |
!> | 10 | Cumulative transpiration |
!> | 11 | Cumulative aquifer flow |
!> | 12 | Cumulative discharge |
!> | 13 | Canopy storage |
!> | 14 | Snow storage |
!> | 15 | Subsurface storage |
!> | 16 | Surface storage |
!> | 17 | Channel storage |
!>
!> Each total is printed as `BALANC(i) * 1000 / CAREA` (mm), converting the
!> volume accumulator (m^3) to a depth over the catchment plan area (m^2).
!>
!> @note
!> As documented on [[legacy_retained]], no current routine assigns `FLERRC`, `SYERRC`,
!> or `CMERRC`; the error-count section of this output is therefore always
!> zero in the current build.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2005-01-25 | SB | - | Added the spatially averaged cumulative-flux and storage summary output. |
!> @endhistory
   SUBROUTINE extra_output()
      INTEGER :: i
      DOUBLEPRECISION    :: car
      WRITE (FID_logfile, 1400)
      DO I = 0, 100
         IF (FLERRC(I) .GT. 0) WRITE (FID_logfile, 1500) I + 1000, FLERRC(I)
      END DO
      DO I = 0, 100
         IF (SYERRC(I) .GT. 0) WRITE (FID_logfile, 1500) I + 2000, SYERRC(I)
      END DO
      DO I = 0, 100
         IF (CMERRC(I) .GT. 0) WRITE (FID_logfile, 1500) I + 3000, CMERRC(I)
      END DO
      WRITE (FID_logfile, 1600)
1400  FORMAT(//'Error message asummary'/)
1500  FORMAT('No. of occurences of error number ', I4, ': ', I6)

1600  FORMAT(/'End of error message asummary')
!<<<
      WRITE (FID_logfile, '(////)')
      WRITE (FID_logfile, 9900) UZNOW, NSTEP
!
      WRITE (*, *)

      WRITE (*, *) 'Normal completion of SHETRAN run'

!^^^^^sb 250105 mass balnce output
      WRITE (FID_logfile, '(////)')
      WRITE (FID_logfile, *) ' Spatially Averaged Totals (mm) over the simulation'
      WRITE (FID_logfile, '(A20,F10.2)') 'Cum Prec = ', balanc(7)*1000/ &
         carea
      WRITE (FID_logfile, '(A20,F10.2)') 'Cum Can. Evap = ', balanc(8)*1000/ &
         carea
      car = carea
      WRITE (FID_logfile, '(A20,F10.2)') 'Cum Soil+Sur Evp = ', balanc(9) &
         *1000/car
      WRITE (FID_logfile, '(A20,F10.2)') 'Cum Trans = ', balanc(10)*1000/ &
         carea
      WRITE (FID_logfile, '(A20,F10.2)') 'Cum Aqu. Flow = ', balanc(11) &
         *1000/carea

      WRITE (FID_logfile, '(A20,F10.2)') 'Cum Discharge = ', balanc(12) &
         *1000/carea
      WRITE (FID_logfile, '(//)')
      WRITE (FID_logfile, *) ' Storage totals (mm) at the end of the simulation'
      WRITE (FID_logfile, '(A20,F10.2)') 'Canopy Stor = ', balanc(13)*1000/ &
         carea
      WRITE (FID_logfile, '(A20,F10.2)') 'Snow Store = ', balanc(14)*1000/ &
         carea
      WRITE (FID_logfile, '(A20,F10.2)') 'Subsur Stor = ', balanc(15)*1000/ &
         carea
      WRITE (FID_logfile, '(A20,F10.2)') 'Surface Stor = ', balanc(16)*1000/ &
         carea
      WRITE (FID_logfile, '(A20,F10.2)') 'Channel Stor = ', balanc(17)*1000/ &
         carea
9900  FORMAT('Normal completion of SHETRAN run: ', F10.2, ' hours, ', &
      &        I7, ' steps.'/)
   END SUBROUTINE extra_output

END MODULE run_summary

