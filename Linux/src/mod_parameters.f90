!-------------------------------------------------------------------------------
! 
!> @file mod_parameters.f90
!> @ingroup lib_util
!
!> @author Sven Berendsen, Newcastle University
! 
!> @brief 
!! Portability: Constants for use in variable size definitions and computations.
! 
! REVISION HISTORY:
! 2020.03.05 - SB - Initial version
!
!-------------------------------------------------------------------------------
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