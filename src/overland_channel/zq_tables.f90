!> summary: The optional reservoir stage--discharge (ZQ) tables and their metadata.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> When reservoir routing is enabled, each controlled link face has a tabulated
!> relation between water level and discharge that replaces the usual weir or
!> channel formula. This module holds those tables and the metadata saying
!> which link and face each one applies to, plus the weir-sill elevation below
!> which the table does not apply.
!>
!> `iszq` says whether the feature is enabled at all; when it is not, the
!> allocatable arrays are never allocated. Module state is public by default.
!>
!> @warning
!> `ZQTableRef` is declared twice in the pre-reorganisation source — once here
!> and once in the reader — with different documented meanings. Both are kept:
!> the reader's copy arrives in step 11 as `ZQTableRefRead`. Whether the two
!> are really one quantity is deliberately left open.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of AL_D; see docs/rename/proposal.md. |
!> @endhistory
MODULE zq_tables

   IMPLICIT NONE

   INTEGER :: NoZQTables !! Number of reservoir ZQ tables read from `zqd`.
   INTEGER :: ZQTableRef !! Index of the ZQ table selected for the current link-face calculation.
   LOGICAL :: iszq       !! Whether reservoir ZQ-table routing is enabled.
   INTEGER, DIMENSION(:), ALLOCATABLE :: ZQTableLink !! Channel-link number for each ZQ table.
   INTEGER, DIMENSION(:), ALLOCATABLE :: ZQTableFace !! Channel-link face number for each ZQ table.
   DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ZQweirSill !! Weir-sill elevation for each ZQ table (m).

END MODULE zq_tables

