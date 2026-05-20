!> summary: Bank water-state variables used by the contaminant component.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University
!>
!> `BK_CW` replaces the legacy `BK.CW` common blocks for bank water variables.
!> It stores bank/link adjacency, vertical cell exchange limits, fractions of
!> cells below the bed deep layer, and previous bank flow state used by
!> contaminant and water-preparation routines.
!>
!> Bank elements are not a separate process solver in this module. They are
!> narrow channel-adjacent elements created by the frame geometry routines and
!> initialised through [[frmod:inbk]]. The manual describes their assumptions:
!> bank elements are associated with channel links, use separately supplied bank
!> data, and represent exchange between channel, overbank flow, VSS, sediment,
!> and contaminant calculations.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-06-13 | JE | 3.1 | Original version written. |
!> | 1991-06-16 | JE | 3.1 | Corrected syntax errors. |
!> | 1991-07-17 | JE | 3.1 | Reordered names in `BKOL` and `LFBK`. |
!> | 1995-03-10 | GP | 4.0 | Replaced `BFSCL` with `BK*` variables. |
!> | 1997-02-19 | RAH | 4.1 | Removed redundant variables, added explicit typing, and separated `LFBKI` from mixed-type `LFBK`. |
!> | 1997-02-20 | RAH | - | Amended descriptions of `NCEBD` and `FNCEBD`. |
!> | 1998-03-08 | RAH | 4.2 | Removed `OLBD`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> @endhistory
MODULE BK_CW
!--------------------------- Start of BK.CW ---------------------------*
!
!  CM COMPONENT INCLUDE-FILE FOR WATER VARIABLES FOR BANKS
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/INCLUDE/BK.CW/4.2
! Modifications:
!                          JE     13/6/91   3.1     WRITTEN
!                          JE     16/6/91   3.1     SYNTAX ERRORS
!                                                   CORRECTED
!                          JE     17/7/91   3.1     REORDERED NAMES IN
!                                                   BKOL AND LFBK
!  GP  950310  4.0  Replace BFSCL (see LINKW,COLMW) with BK*.
! RAH  970219  4.1  Remove FNOLCH,NOLCH,JKZCH,LINKF,NBKA,BKTHE,BKQV,BKQH
!                   (redundant).  Explicit typing.
!                   Separate /LFBKI/ from mixed-type /LFBK/.
! RAH  970220       Amend descriptions of NCEBD,FNCEBD.
! RAH  980308  4.2  Remove OLBD (see INCM).
! JE  12/08   4.3.5F90  Convert to FORTRAN90
!----------------------------------------------------------------------*

! Imported constants
!                      LLEE,NLFEE
   USE SGLOBAL, ONLY : NLFEE, LLEE
   IMPLICIT NONE
   DOUBLEPRECISION FNCEBD (NLFEE, 2)

!COMMON / LFBK / FNCEBD
!                             FRACTION OF CELL NCEBD+1 WHICH LIES
!                             BELOW THE BED DEEP LAYER
   INTEGER :: NBANK (NLFEE, 2), NCEAB (NLFEE, 2), NCEBD (NLFEE, 2)

!COMMON / LFBKI / NBANK, NCEAB, NCEBD
!                             NUMBER FOR THE BANK ADJACENT TO A LINK;
!                             NUMBER FOR THE LOWEST CELL TO EXCHANGE
!                             WATER WITH THE STREAM;
!                             NUMBER OF THE HIGHEST CELL WHICH LIES
!                             FULLY BELOW THE BED DEEP LAYER
   DOUBLEPRECISION QQRVO (NLFEE, LLEE, 2)
!PRIVATE :: NLFEE, LLEE
END MODULE BK_CW
