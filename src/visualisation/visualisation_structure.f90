!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_structure
! Standard Fortran replacement for visualization structure module
! Removed Cray pointers, INT_PTR_KIND, and DEC$ directives
! JE for SHEGRAPH Version 2.0 Created July 2004, modernized 2025
USE ISO_FORTRAN_ENV, ONLY: INT64
USE ISO_C_BINDING, ONLY: INT_PTR_KIND => C_INTPTR_T
IMPLICIT NONE

INTEGER, PARAMETER :: iundef      = -1,      & !undefined
                      i_not_exist = iundef, &
                      defi4(4)    = (/iundef, iundef, iundef, iundef/), &
                      csz         = 70         !name length
REAL, PARAMETER    :: zero=0.0, half=0.5,                           &
                      rundef=-1.0, & !undefined
                      r_not_exist = rundef, &
                      defr4(4)       = (/r_not_exist, r_not_exist, r_not_exist, r_not_exist/)
LOGICAL, PARAMETER :: t=.TRUE., f=.FALSE.

INTEGER, PARAMETER :: no_types=8

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE aord
INTEGER, POINTER :: a
END TYPE aord

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE CYPHER
CHARACTER      :: nemonic
CHARACTER(CSZ) :: typ
END TYPE CYPHER
TYPE(CYPHER), PARAMETER :: cyph(no_types)=        &
      (/cypher('B', 'real_banks'),                &
        cypher('E', 'integer_banks'),             &
        cypher('F', 'integer_rivers'),            &
        cypher('G', 'real_middle_and_edges'),     &
        cypher('I', 'integer_middle'),            &
        cypher('L', 'real_rivers'),               &
        cypher('M', 'real_middle'),               &
        cypher('N', 'integer_middle_and_edges')/)

!EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE integer_edges !for middle of cell and edges
INTEGER :: e(4) = iundef    !edge N, E, S, W
END TYPE integer_edges
TYPE(INTEGER_EDGES), PARAMETER :: &
        default_integer_edges = INTEGER_EDGES(defi4), &
        dfie                  = default_integer_edges

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE real_edges !for middle of cell and edges
REAL :: e(4) = rundef  !edges N, E, S, W
END TYPE real_edges
TYPE(REAL_EDGES), PARAMETER :: &
        default_real_edges = REAL_EDGES(defr4), &
        dfre               = default_real_edges

!MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE integer_middle !for middle of cell
INTEGER :: m = rundef  !middle
END TYPE integer_middle
TYPE(INTEGER_MIDDLE), PARAMETER :: &
        default_integer_middle = INTEGER_MIDDLE(r_not_exist), &
        dfim                   = default_integer_middle

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE real_middle !for middle of cell
REAL :: m    = rundef  !middle
END TYPE real_middle
TYPE(REAL_MIDDLE), PARAMETER :: &
        default_real_middle = REAL_MIDDLE(r_not_exist), &
        dfrm                = default_real_middle

!MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES 
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE integer_middle_and_edges !for middle of cell and edges
PRIVATE
INTEGER :: m    = iundef, &  !middle
           b(4) = iundef, &  !bank N, E, S, W
           r(4) = iundef     !river N, E, S, W
END TYPE integer_middle_and_edges
TYPE(INTEGER_MIDDLE_AND_EDGES), PARAMETER :: &
        default_integer_middle_and_edges = INTEGER_MIDDLE_AND_EDGES(i_not_exist, defi4, defi4), &
        dfime                            = default_integer_middle_and_edges

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE real_middle_and_edges !for middle of cell and edges
! Removed SEQUENCE for portability
REAL :: m    = rundef, &  !middle
        b(4) = rundef, &  !bank N, E, S, W
        r(4) = rundef     !river N, E, S, W
END TYPE real_middle_and_edges
TYPE(REAL_MIDDLE_AND_EDGES), PARAMETER :: &
        default_real_middle_and_edges = REAL_MIDDLE_AND_EDGES(r_not_exist, defr4, defr4), &
        dfrme                         = default_real_middle_and_edges

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE integer_radial !for middle of cell and edges
INTEGER :: m(4) = iundef, &  !middle
           b(4) = iundef, &  !bank N, E, S, W
           r(4) = iundef     !river N, E, S, W
END TYPE integer_radial
TYPE(INTEGER_RADIAL), PARAMETER :: &
        default_integer_radial = INTEGER_RADIAL(i_not_exist, defi4, defi4), &
        dfir                  = default_integer_radial

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE real_radial !for middle of cell and edges
REAL :: m(4) = rundef, &  !middle
        b(4) = rundef, &  !bank N, E, S, W
        r(4) = rundef     !river N, E, S, W
END TYPE real_radial
TYPE(REAL_RADIAL), PARAMETER :: &
        default_real_radial = REAL_radial(r_not_exist, defr4, defr4), &
        dfrr                = default_real_radial

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE BS
PRIVATE
REAL                                          :: time=zero
TYPE(REAL_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(BS), POINTER                             :: previous=>NULL(), next=>NULL()
END TYPE BS

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE ES
PRIVATE
REAL                                             :: time=zero
TYPE(INTEGER_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(ES), POINTER                                :: previous=>NULL(), next=>NULL()
END TYPE ES

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE FS
PRIVATE
REAL                                             :: time=zero
TYPE(INTEGER_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(FS), POINTER                                :: previous=>NULL(), next=>NULL()
END TYPE FS

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE GS
PRIVATE
! Removed SEQUENCE for portability
REAL                                                     :: time=zero
TYPE(REAL_MIDDLE_AND_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(GS), POINTER                                        :: previous=>NULL(), next=>NULL()
END TYPE GS

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE IS
PRIVATE
REAL                                              :: time=zero
TYPE(INTEGER_MIDDLE), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(IS), POINTER                                 :: previous=>NULL(), next=>NULL()
END TYPE IS

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE LS
PRIVATE
REAL                                          :: time=zero
TYPE(REAL_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(LS), POINTER                             :: previous=>NULL(), next=>NULL()
END TYPE LS

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE MS
PRIVATE
REAL                                           :: time=zero
TYPE(MS), POINTER                              :: previous=>NULL(), next=>NULL()
TYPE(REAL_MIDDLE), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
END TYPE MS

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE NS
PRIVATE
REAL                                                       :: time=zero
TYPE(INTEGER_MIDDLE_AND_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(NS), POINTER                                           :: previous=>NULL(), next=>NULL()
END TYPE NS

! Module-level linked list heads (replaces Cray pointer mechanism)
TYPE(BS), POINTER :: head_bs => NULL(), tail_bs => NULL()
TYPE(ES), POINTER :: head_es => NULL(), tail_es => NULL()  
TYPE(FS), POINTER :: head_fs => NULL(), tail_fs => NULL()
TYPE(GS), POINTER :: head_gs => NULL(), tail_gs => NULL()
TYPE(IS), POINTER :: head_is => NULL(), tail_is => NULL()
TYPE(LS), POINTER :: head_ls => NULL(), tail_ls => NULL()
TYPE(MS), POINTER :: head_ms => NULL(), tail_ms => NULL()
TYPE(NS), POINTER :: head_ns => NULL(), tail_ns => NULL()

INTERFACE SAVE_ITEMS_WORTH ; MODULE PROCEDURE SAVE_ITEMS_WORTH_I, SAVE_ITEMS_WORTH_R ; ENDINTERFACE

PRIVATE
PUBLIC :: FOR_NEW_TIME, SAVE_ITEMS_WORTH, TIME_COUNT, MBR_COUNT, GET_MBR, GET_HDF5_I, GET_HDF5_R, &
          GET_HDF5_TIME, csz

CONTAINS

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
! Simple stub implementations to allow compilation - these would need proper implementation
REAL FUNCTION get_hdf5_time(typ, node_id) RESULT(r)
INTEGER, INTENT(IN)      :: node_id  
CHARACTER(*), INTENT(IN) :: typ
! Placeholder implementation
r = 0.0
WRITE(*,*) 'WARNING: get_hdf5_time not fully implemented in standard version'
END FUNCTION get_hdf5_time

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION get_hdf5_i(typ, sz, szo, node_id, ilow, jlow, klow) RESULT(r)
INTEGER, INTENT(IN)                                     :: ilow, jlow, klow, node_id
INTEGER, DIMENSION(6), INTENT(IN)                       :: sz, szo
INTEGER, DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)) :: r
CHARACTER(*), INTENT(IN)                                :: typ
! Placeholder implementation
r = 0
WRITE(*,*) 'WARNING: get_hdf5_i not fully implemented in standard version'
END FUNCTION get_hdf5_i

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION get_hdf5_r(typ, sz, szo, node_id, ilow, jlow, klow) RESULT(r)
INTEGER, INTENT(IN)                                     :: ilow, jlow, klow, node_id
INTEGER, DIMENSION(6), INTENT(IN)                       :: sz, szo
REAL, DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6))    :: r
CHARACTER(*), INTENT(IN)                                :: typ
! Placeholder implementation
r = 0.0
WRITE(*,*) 'WARNING: get_hdf5_r not fully implemented in standard version'
END FUNCTION get_hdf5_r

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION get_mbr(typ) RESULT(r)
INTEGER                             :: n
CHARACTER(2), INTENT(IN)            :: typ
CHARACTER(6), DIMENSION(:), POINTER :: r
CHARACTER(6), PARAMETER             :: sq(1)=(/'square'/),                            &
                                       bk(4)=(/'N-bank','E-bank','S-bank','W-bank'/), &
                                       rv(4)=(/'N-link','E-link','S-link','W-link'/)
n = MBR_COUNT(typ)
ALLOCATE(r(n))
SELECT CASE(typ)
CASE('BS') ; r = bk
CASE('ES') ; r = bk
CASE('FS') ; r = rv
CASE('GS') ; r = (/sq,bk,rv/)
CASE('IS') ; r = sq
CASE('LS') ; r = rv
CASE('MS') ; r = sq
CASE('NS') ; r = (/sq,bk,rv/)
END SELECT
END FUNCTION get_mbr

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
INTEGER FUNCTION TIME_COUNT(typ, dummy) RESULT(r)
INTEGER, INTENT(IN)      :: dummy  ! Placeholder to maintain interface
CHARACTER(*), INTENT(IN) :: typ
! Placeholder implementation
r = 1
WRITE(*,*) 'WARNING: TIME_COUNT not fully implemented in standard version'
END FUNCTION TIME_COUNT

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION mbr_count(typ) RESULT(r)
!no of members
CHARACTER(*), INTENT(IN) :: typ
SELECT CASE(typ)
CASE('BS') ; r = 4
CASE('ES') ; r = 4
CASE('FS') ; r = 4
CASE('GS') ; r = 9
CASE('IS') ; r = 1
CASE('LS') ; r = 4
CASE('MS') ; r = 1
CASE('NS') ; r = 9
END SELECT
END FUNCTION mbr_count

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE save_items_worth_i(c, typ, a, b, klow, khigh, e, d, save_this, dummy)
! Removed DEC$ ATTRIBUTES DLLEXPORT for portability
! Fixed parameter list to match calling convention with 10th parameter
INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
INTEGER(INT_PTR_KIND), INTENT(IN) :: dummy
INTEGER, DIMENSION(khigh-klow+1), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)             :: c
CHARACTER(*), INTENT(IN)          :: typ
! Placeholder implementation
WRITE(*,*) 'WARNING: save_items_worth_i not fully implemented in standard version'
END SUBROUTINE save_items_worth_i

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE save_items_worth_r(c, typ, a, b, klow, khigh, e, d, save_this, dummy)
! Removed DEC$ ATTRIBUTES DLLEXPORT for portability
! Fixed parameter list to match calling convention with 10th parameter
INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
INTEGER(INT_PTR_KIND), INTENT(IN) :: dummy
REAL, DIMENSION(khigh-klow+1), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)          :: c
CHARACTER(*), INTENT(IN)       :: typ
! Placeholder implementation
WRITE(*,*) 'WARNING: save_items_worth_r not fully implemented in standard version'
END SUBROUTINE save_items_worth_r

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME(typ, time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, dummy1, dummy2)
! Removed DEC$ ATTRIBUTES DLLEXPORT for portability
! Fixed parameter types to match calling convention - dummy1 and dummy2 are INT_PTR_KIND
INTEGER, INTENT(IN)                :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND), INTENT(IN)  :: dummy1, dummy2
REAL, INTENT(IN)                   :: time
CHARACTER(*), INTENT(IN)           :: typ
! Placeholder implementation
WRITE(*,*) 'WARNING: FOR_NEW_TIME not fully implemented in standard version'
END SUBROUTINE FOR_NEW_TIME

END MODULE visualisation_structure
