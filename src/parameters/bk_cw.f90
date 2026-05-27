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
USE SGLOBAL, ONLY : NLFEE, LLEE
IMPLICIT NONE
DOUBLEPRECISION FNCEBD (NLFEE, 2)  !! Fraction of cell `NCEBD+1` lying below the bed deep layer.
INTEGER :: NBANK (NLFEE, 2)        !! Bank element adjacent to each link and bank side.
INTEGER :: NCEAB (NLFEE, 2)        !! Lowest VSS cell exchanging water with the stream.
INTEGER :: NCEBD (NLFEE, 2)        !! Highest VSS cell lying fully below the bed deep layer.
DOUBLEPRECISION QQRVO (NLFEE, LLEE, 2)  !! Previous bank vertical flow values by link, layer, and bank side.
!PRIVATE :: NLFEE, LLEE
END MODULE BK_CW
