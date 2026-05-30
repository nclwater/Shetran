!> summary: Portable kind, string-length, and missing-value parameters.
!> author: Sven Berendsen, Newcastle University
!>
!> This module centralizes the kind parameters, string-length limits, and legacy
!> missing-value sentinels used across SHETRAN. The kind constants use
!> `selected_real_kind` and `selected_int_kind` so declarations request decimal
!> precision/range rather than compiler-specific kind numbers.
!>
!> @note `R_P` and `I_P` are the project defaults for real and integer
!> declarations. `R_P` currently maps to `R8P`, and `I_P` currently maps to
!> `I4P`.
!> @endnote
!>
!> @note The kind-parameter pattern was lifted from PENF, where it was available
!> under GPL/BSD/MIT-compatible terms.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-03-05 | SB | - | Initial version. |
!> @endhistory
module mod_parameters

   implicit none

   ! KIND constants -----------------------------------------------------------
   integer, parameter :: R8P = selected_real_kind(15,307) !! Real kind: 15 decimal digits, exponent range about 307.
   integer, parameter :: R4P = selected_real_kind(6,37)   !! Real kind: 6 decimal digits, exponent range about 37.
   integer, parameter :: R_P = R8P                        !! Default real kind.

   integer, parameter :: I8P = selected_int_kind(18) !! Integer kind covering at least 18 decimal digits.
   integer, parameter :: I4P = selected_int_kind(9)  !! Integer kind covering at least 9 decimal digits.
   integer, parameter :: I2P = selected_int_kind(4)  !! Integer kind covering at least 4 decimal digits.
   integer, parameter :: I1P = selected_int_kind(2)  !! Integer kind covering at least 2 decimal digits.
   integer, parameter :: I_P = I4P                   !! Default integer kind.


   ! String constants ---------------------------------------------------------
   integer(kind=I_P), parameter :: LENGTH_FILEPATH     =    260 !! Maximum file-path length for Windows compatibility.
   integer(kind=I_P), parameter :: LENGTH_LINE         =    256 !! Standard text-line buffer length.
   integer(kind=I_P), parameter :: LENGTH_LINELONG     =  16384 !! Long text-line buffer length.
   integer(kind=I_P), parameter :: LENGTH_LINEVERYLONG = 262144 !! Very long text-line buffer length.


   ! NaN equivalents ----------------------------------------------------------
   real(kind=R8P), parameter    :: NAN_REAL_R8P = -9.35d30     !! Legacy missing-value sentinel for `R8P` reals.
   real(kind=R_P), parameter    :: NAN_REAL_R_P = NAN_REAL_R8P !! Legacy missing-value sentinel for default reals.
   integer(kind=I_P), parameter :: NAN_INT_I4P  = -9999        !! Legacy missing-value sentinel for `I4P` integers.
   integer(kind=I_P), parameter :: NAN_INT_I_P  = NAN_INT_I4P  !! Legacy missing-value sentinel for default integers.

end module mod_parameters
