!> summary: The run title, the component switches, the hotstart settings and the run controls.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> What the run is configured to do, as opposed to where it has got to. The
!> `BEX*` flags say which components are active, the `BHOT*` values control
!> hotstart reading and writing, `TSH` and `TCH` are the sediment and
!> contaminant start times, and `TITLE` is the run title read from the frame
!> input — and reused as the current input-section heading by each component
!> reader.
!>
!> Frame setup writes all of it; every component reads some of it. It is
!> data-only and depends on nothing, which is what lets the component setup
!> routines that were lifted out of `FRmod` read the run controls without
!> depending on `FRmod` itself.
!>
!> @note
!> `BEXET`, `BEXUZ`, `BEXEX`, `BEXOC` and `BEXSZ` are always set true by the
!> current frame setup; only `BEXSM`, `BEXSY` and `BEXCM` are actually
!> selectable, through record FR25.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991--1998 | GP / RAH | 3.0--4.2 | Developed the frame run controls and the component switches. |
!> | 2020--2024 | SB | 4.5--4.6 | Added the ZQ, extra-discharge and phreatic-surface selectors. |
!> | 2026-09-11 | SvB | - | Split out of AL_D and FRmod; see docs/rename/proposal.md. Created in step 11 rather than step 12, because bank_setup's INBK reads TITLE. |
!> | 2026-09-11 | SvB | - | Dropped the shared `msg` buffer, which no procedure in the tree reads or writes (D17). |
!> @endhistory
MODULE run_control


   IMPLICIT NONE

   DOUBLEPRECISION :: BHOTTI  !! Requested/read hotstart time (h).
   DOUBLEPRECISION :: BHOTST  !! Interval between hotstart outputs (h).
   DOUBLEPRECISION :: TOUTPUT !! Interval for regular text/CSV outputs; defaults to 24 h (h).
   DOUBLEPRECISION :: HOTIME !! Current/last hotstart time (h).
   LOGICAL :: BEXET      !! Whether evapotranspiration is active; current frame setup always sets true.
   LOGICAL :: BEXUZ      !! Whether the legacy upper-zone process is active; current frame setup always sets true.
   LOGICAL :: BEXEX      !! Whether legacy exchange is active; current frame setup always sets true.
   LOGICAL :: BEXOC      !! Whether overland/channel flow is active; current frame setup always sets true.
   LOGICAL :: BEXSZ      !! Whether saturated-zone flow is active; current frame setup always sets true.
   LOGICAL :: BEXSM      !! Whether snowmelt is enabled by FR25.
   LOGICAL :: BHOTPR     !! Whether periodic hotstart output is enabled.
   LOGICAL :: BHOTRD     !! Whether initial state is read from the hotstart file.
   LOGICAL :: BEXSY      !! Whether sediment transport is enabled by FR25.
   LOGICAL :: BEXCM      !! Whether contaminant transport is enabled by FR25.
   LOGICAL :: isextradis !! Whether the extra-discharge point-selection input is available.
   LOGICAL :: isextrapsl !! Whether the extra phreatic-surface point-selection input is available.
   DOUBLEPRECISION :: TSH    !! Sediment-component start time measured from the run start (h).
   DOUBLEPRECISION :: TCH    !! Contaminant-component start time measured from the run start (h).
   LOGICAL :: BFRTS1 !! Print the calculation sequence to the screen during simulation.
   LOGICAL :: BFRTS2 !! Print values exchanged between the frame and components each timestep.
   LOGICAL :: BINFRP !! Echo frame input data to the print file.
   LOGICAL :: BSOFT  !! Enable the shortened-timestep soft start.
   CHARACTER(LEN=80) :: TITLE !! Current run title or input-section heading.

END MODULE run_control

