!> summary: Shared logical state controlling contaminant and nitrate calculation paths.
!> author: JE, Newcastle University; GP, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `IS_CC` replaces the logical common block in the legacy `IS.CC` include.
!> [[cmmod]] use-associates all five flags. [[frmod]] additionally imports
!> `ISPLT` and `ISMN` for plant initialization and rundata file handling.
!>
!> | Flag | Intended/current lifecycle and principal consumers |
!> |:-----|:--------------------------------------------------|
!> | `ISFLXB` | Manual `CM5` base-boundary mode; read by [[cmmod:cmrd]] and consumed by `COLMSM` and `COLM`. |
!> | `ISADNL` | Manual `CM13` nonlinear-adsorption mode used by `COLMSM`, [[cmmod:linksm]], and [[cmmod:slvclm]]. |
!> | `ISBK` | Per-column bank workspace set by [[cmmod:colmw]] and then consumed by `COLMW` and [[cmmod:colmsm]]. |
!> | `ISPLT` | Intended run-wide contaminant plant-uptake switch gating [[frmod:inpl]], [[cmmod:plprep]], and [[cmmod:plcolm]]. |
!> | `ISMN` | Rundata unit 53 nitrate switch set by [[frmod:fropen]]; gates [[mnmod:mncont]] and `COLMSM` nitrate sources. |
!>
!> The module declares no `PRIVATE` statement, so all five flags are public.
!> None has a declaration initializer. `ISBK` is assigned immediately before
!> its active per-column consumers, and `FROPEN` establishes `ISMN` before
!> component initialization.
!>
!> @warning
!> In [[cmmod:cmrd]], local declarations named `ISFLXB` and `ISADNL` shadow
!> the module variables. Manual records `CM5` and `CM13` are therefore read
!> into locals; `ISFLXB` controls only the input-array setup within `CMRD`, and
!> the local `ISADNL` has no subsequent use there. The module flags later read
!> by `COLMSM`, `COLM`, `LINKSM`, and `SLVCLM` remain undefined under standard
!> Fortran.
!>
!> `ISPLT` likewise has no assignment anywhere in the current source before
!> `INPL`, `PLPREP`, and `PLCOLM` test it, so the contaminant plant-uptake path
!> also depends on undefined logical state. This documentation transfer records
!> but does not change these current behaviours.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1991-05-01 | JE | 3.0 | Original version written. |
!> | 1991-06-13 | JE | 3.1 | Checked with no changes. |
!> | - | GP | 3.4 | Added `ISPLT` and renamed legacy `LGIC` as `LGIC4`. |
!> | 1997-02-21 | RAH | 4.1 | Amended the comments. |
!> | 1998-03-08 | RAH | 4.2 | Amended the history. |
!> | 2008-12 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> | 2025-10-07 | SB | 4.5.3 | Added `ISMN` for the nitrate component. |
!> @endhistory
MODULE IS_CC
   IMPLICIT NONE

   LOGICAL :: ISADNL !! Intended nonlinear-adsorption flag from `CM13`; module value is currently unassigned.
   LOGICAL :: ISBK   !! True while the current contaminant column is a bank element.
   LOGICAL :: ISFLXB !! Intended flux-concentration base-boundary flag from `CM5`; module value is currently unassigned.
   LOGICAL :: ISPLT  !! Intended contaminant plant-uptake flag; currently unassigned.
   LOGICAL :: ISMN   !! Nitrate-component flag established from rundata file unit 53.

END MODULE IS_CC
