!> summary: Logical switches for contaminant component options.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University
!>
!> `IS_CC` stores contaminant-component logical flags that control whether
!> optional contaminant pathways or sub-processes are active during a run.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-01 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked, no changes. |
!> | - | GP | 3.4 | Added `ISPLT` and renamed `LGIC` as `LGIC4`. |
!> | 1997-02-21 | RAH | 4.1 | Amended comments. |
!> | 1998-03-08 | RAH | 4.2 | Amended history. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE IS_CC
IMPLICIT NONE
LOGICAL :: ISADNL !! Flag for additional contaminant/nitrate logic.
LOGICAL :: ISBK   !! Flag for bank-related contaminant calculations.
LOGICAL :: ISFLXB !! Flag for contaminant flux-boundary processing.
LOGICAL :: ISPLT  !! Flag for plant-related contaminant calculations.
LOGICAL :: ISMN   !! Flag for mineral/nitrogen contaminant coupling.

END MODULE IS_CC
