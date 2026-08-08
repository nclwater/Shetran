!> summary: Portable numeric-kind requests, text-buffer lengths, and legacy missing-value sentinels.
!> author: Sven Berendsen, Newcastle University
!> date: 2020-03-05
!>
!> `mod_parameters` centralizes the numeric kinds and character-buffer sizes
!> used by the newer SHETRAN infrastructure. `R8P`/`R4P` and `I8P` through
!> `I1P` use `selected_real_kind`/`selected_int_kind` to request minimum decimal
!> capabilities from the compiler. Their names do not guarantee a byte width
!> or a particular numeric kind value.
!>
!> `R_P` and `I_P` are project aliases currently set to `R8P` and `I4P`.
!> They do not change the processor's intrinsic default real or integer kinds.
!> All 16 parameters are public because the module has no `PRIVATE` statement.
!>
!> | Parameter group | Current use outside this module |
!> |:----------------|:--------------------------------|
!> | `R8P`, `I_P` | Numeric declarations in [[sglobal]], [[mod_load_filedata]], and [[zqmod]]. |
!> | `LENGTH_FILEPATH`, `LENGTH_LINE` | Path, command-line, and diagnostic buffers in `SGLOBAL` and [[getdirqq]]. |
!> | `LENGTH_LINELONG` | Diagnostic-detail buffers in [[visualisation_read_parser]]. |
!> | Remaining kind, buffer, and `NAN_*` parameters | No named external consumer; retained public API. |
!>
!> These are internal compile-time constants and have no user-manual input
!> record.
!>
!> The `NAN_*` parameters are ordinary finite sentinel values, not IEEE NaNs.
!> Code must compare or recognize them explicitly; IEEE NaN propagation and
!> predicates do not apply. No current source outside this module uses them.
!>
!> @note
!> A `selected_*_kind` inquiry returns a negative value when the processor
!> cannot provide the requested model. This module has no fallback or explicit
!> guard; a consumer attempting to use such a result as a kind will fail to
!> compile. The requested models are available in the tested gfortran build.
!> @endnote
!>
!> @note
!> The original 2020 source attributes the kind-selection definitions to PENF
!> and records GPL, BSD-2-Clause, BSD-3-Clause, or MIT licensing. This preserves
!> that existing attribution without making a new provenance or licence
!> determination.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-03-05 | SB | - | Initial version. |
!> | 2026-03-28 | SvB | - | Converted the file documentation to FORD style as an example. |
!> @endhistory
module mod_parameters

   implicit none

   ! KIND constants -----------------------------------------------------------
   integer, parameter :: R8P = selected_real_kind(15,307) !! Real kind with precision >= 15 and range >= 307.
   integer, parameter :: R4P = selected_real_kind(6,37)   !! Real kind with precision >= 6 and range >= 37.
   integer, parameter :: R_P = R8P                        !! Project-selected real-kind alias, currently `R8P`.

   integer, parameter :: I8P = selected_int_kind(18) !! Integer kind with decimal range >= 18.
   integer, parameter :: I4P = selected_int_kind(9)  !! Integer kind with decimal range >= 9.
   integer, parameter :: I2P = selected_int_kind(4)  !! Integer kind with decimal range >= 4.
   integer, parameter :: I1P = selected_int_kind(2)  !! Integer kind with decimal range >= 2.
   integer, parameter :: I_P = I4P                   !! Project-selected integer-kind alias, currently `I4P`.


   ! String constants ---------------------------------------------------------
   integer(kind=I_P), parameter :: LENGTH_FILEPATH     =    260 !! Project path buffer; classic Windows `MAX_PATH` size.
   integer(kind=I_P), parameter :: LENGTH_LINE         =    256 !! Short text and diagnostic buffer length.
   integer(kind=I_P), parameter :: LENGTH_LINELONG     =  16384 !! Long text buffer; used for parser diagnostics.
   integer(kind=I_P), parameter :: LENGTH_LINEVERYLONG = 262144 !! Reserved very-long text buffer; currently unused.


   ! NaN equivalents ----------------------------------------------------------
   real(kind=R8P), parameter    :: NAN_REAL_R8P = -9.35d30     !! Finite missing-value sentinel stored as `R8P`.
   real(kind=R_P), parameter    :: NAN_REAL_R_P = NAN_REAL_R8P !! Same sentinel stored with project real kind `R_P`.
   integer(kind=I_P), parameter :: NAN_INT_I4P  = -9999        !! Finite integer sentinel; `I_P` currently equals `I4P`.
   integer(kind=I_P), parameter :: NAN_INT_I_P  = NAN_INT_I4P  !! Alias of `NAN_INT_I4P` for project integer kind `I_P`.

end module mod_parameters
