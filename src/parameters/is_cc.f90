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
!---------------------------- Start of IS.CC --------------------------*
!
!                      INCLUDE FILE FOR LOGICAL VARIABLES FOR THE
!                      CONTAMINANT COMPONENT
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/IS.CC/4.2
! Modifications:
!                          JE     1/5/91   3.0      WRITTEN
!                          JE    13/6/91   3.1      CHECKED, NO CHANGES
!  GP          3.4  Add ISPLT.  Rename LGIC as LGIC4.
! RAH  970221  4.1  Amend comment.
! RAH  980308  4.2  Amend history.
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*

! Commons
   IMPLICIT NONE
   LOGICAL :: ISADNL, ISBK, ISFLXB, ISPLT, ISMN

END MODULE IS_CC
