!> summary: Portable kind, string-length, and missing-value parameters.
!> author: Sven Berendsen, Newcastle University
!>
!> This module centralizes the kind parameters and common scalar constants used
!> across SHETRAN. The real and integer kind values are selected with the Fortran
!> intrinsic `selected_real_kind` and `selected_int_kind` so declarations can
!> request decimal precision/range rather than relying on compiler-specific kind
!> numbers. See the GNU Fortran documentation for `selected_real_kind`:
!> https://gcc.gnu.org/onlinedocs/gfortran/SELECTED_005fREAL_005fKIND.html
!>
!> It also defines common string-length limits and legacy missing-value sentinels
!> used where IEEE NaN values or optional data structures are not available in the
!> surrounding code.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2020-03-05 | SB | - | Initial version. |
!> @endhistory
!>
!> @note `R_P` and `I_P` are the project defaults for real and integer
!> declarations. `R_P` currently maps to `R8P`, and `I_P` currently maps to
!> `I4P`.
!> @endnote
module mod_parameters

    implicit none

    ! KIND constants -----------------------------------------------------------
    ! (Lifted from PENF <- there under GPL/BSD2 or 3/MIT)
    integer, parameter   :: R8P  = selected_real_kind(15,307)   !< 15 digits, range \([10^{-307} , 10^{+307}  - 1]\);
                                                                !! 64 bits.
    integer, parameter   :: R4P  = selected_real_kind(6,37)     !< 6  digits, range \([10^{-37}  , 10^{+37}   - 1]\);
                                                                !! 32 bits.
    integer, parameter   :: R_P  = R8P                          !< Default real precision.

    integer, parameter   :: I8P  = selected_int_kind(18)        !< Range \([-2^{63},+2^{63} - 1]\), 19 digits plus sign;
                                                                !!  64 bits.
    integer, parameter   :: I4P  = selected_int_kind(9)         !< Range \([-2^{31},+2^{31} - 1]\), 10 digits plus sign;
                                                                !!  32 bits.
    integer, parameter   :: I2P  = selected_int_kind(4)         !< Range \([-2^{15},+2^{15} - 1]\), 5  digits plus sign;
                                                                !!  16 bits.
    integer, parameter   :: I1P  = selected_int_kind(2)         !< Range \([-2^{7} ,+2^{7}  - 1]\), 3  digits plus sign;
                                                                !!  8  bits.
    integer, parameter   :: I_P  = I4P                          !< Default integer precision.


    ! String constants ---------------------------------------------------------
    integer(kind=I_P), parameter  :: LENGTH_FILEPATH     =    260   !< max path length. Set to 260 for Windows
                                                                    !! compatability.
    integer(kind=I_P), parameter  :: LENGTH_LINE         =    256   !< max. line length (a lot of editors have a limit
                                                                    !! of 1024, some 4096).
    integer(kind=I_P), parameter  :: LENGTH_LINELONG     =  16384   !< A long line.
    integer(kind=I_P), parameter  :: LENGTH_LINEVERYLONG = 262144   !< A very long line.


    ! NaN equivalents ----------------------------------------------------------
    real(kind=R8P), parameter    :: NAN_REAL_R8P = -9.35d30         !< NaN equivalent for manual use - Real8.
    real(kind=R_P), parameter    :: NAN_REAL_R_P = NAN_REAL_R8P     !< NaN equivalent for manual use - RealDefault.
    integer(kind=I_P), parameter :: NAN_INT_I4P  = -9999            !< NaN equivalent for manual use - Int4.
    integer(kind=I_P), parameter :: NAN_INT_I_P  = NAN_INT_I4P      !< NaN equivalent for manual use - IntDefault.

 end module mod_parameters
