!> summary: Bank geometry and exchange indices used by contaminant transport.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University
!>
!> `BK_CW` replaces the legacy `BK.CW` common blocks for bank water variables.
!> During contaminant initialisation, [[frmod:incm]] identifies the bank element
!> on each side of every active channel link and establishes the bank-cell limits
!> and fractional cell at the lower boundary of the deep-bed layer. Transport
!> routines including [[cmmod:colmw]] and [[cmmod:linkw]] consume this geometry.
!>
!> The first array index is bounded by the link capacity `NLFEE`; the second
!> selects bank side 1 or 2. `QQRVO` additionally has the cell-capacity bound
!> `LLEE`, but it has no producer or consumer in the current source, so its
!> contents must not be assumed to be initialised or valid.
!>
!> Bank elements are channel-adjacent data structures rather than a separate
!> process solver. The bank-element section of the user manual describes their
!> geometric assumptions and input data. [[frmod:inbk]] reads the bank properties;
!> this module holds only the contaminant-coupling geometry prepared by `INCM`.
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
   DOUBLEPRECISION FNCEBD (NLFEE, 2)  !! Fraction of cell `NCEBD+1` below the deep-bed boundary.
   INTEGER :: NBANK (NLFEE, 2)  !! Adjacent bank-element number for each link and bank side.
   INTEGER :: NCEAB (NLFEE, 2)  !! Lowest bank cell exchanging with the stream; currently set to `NHBED`.
   INTEGER :: NCEBD (NLFEE, 2)  !! Highest bank cell lying fully below the deep-bed boundary.
   DOUBLEPRECISION QQRVO (NLFEE, LLEE, 2)  !! Unused legacy bank-flow storage, shaped by link, cell, and side.
!PRIVATE :: NLFEE, LLEE
END MODULE BK_CW
