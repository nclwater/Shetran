!> summary: Shared visualisation metadata pass-through state.
!>
!> This module stores geometry, element classification, filenames, and output
!> dimensions passed from SHETRAN into the visualisation layer. The [[send_p]]
!> routine receives typed values by keyword and updates or allocates the module
!> state used later by the HDF5/visualisation output routines.
MODULE visualisation_pass
IMPLICIT NONE

INTEGER                              :: north, east, south, west, grid_nx, grid_ny, &
                                        top_cell, nel, nsed, ncon, ver
INTEGER, DIMENSION(:,:), ALLOCATABLE :: SU_NUMBER, BANK_NO, RIVER_NO
LOGICAL, DIMENSION(:),   ALLOCATABLE :: IS_SQUARE, IS_BANK, IS_LINK
CHARACTER(256)                        :: DIRQQ, ROOTDIR, hdf5filename, planfile, checkfile

INTEGER, PARAMETER                    :: freelimit=360000, szlimit=360000
CHARACTER(256)                        :: dumtext

PRIVATE
PUBLIC ::     north,     east,     south,    west, &
              grid_nx,   grid_ny,  top_cell, nel,  &
              SU_NUMBER,                           &
              BANK_NO,  RIVER_NO,                  &
              IS_SQUARE, IS_BANK,  IS_LINK,        &
              EXISTS,    SEND_P,   DIRQQ,          &
              nsed,      ncon,     ver,            &
              ROOTDIR, hdf5filename, planfile, checkfile

CONTAINS

!> Returns whether an integer index refers to an existing item.
ELEMENTAL LOGICAL FUNCTION exists(i) RESULT(r)
INTEGER, INTENT(IN) :: i !! Index or element number to test.
r = i>0
END FUNCTION exists

!> Stores a named visualisation value or allocates a named visualisation array.
!>
!> The caller supplies `text` as the key and provides the matching optional
!> scalar, logical array, integer array, or character value. This preserves the
!> legacy pass-through interface between the core model and the visualisation
!> modules while keeping the state in one module.
SUBROUTINE send_p(text, ii, L1, d2, cc, da, db)
integer, save :: coun=0
INTEGER, INTENT(IN)                            :: da !! First dimension for optional array arguments.
INTEGER, INTENT(IN)                            :: db !! Second dimension for optional two-dimensional integer arrays.
INTEGER,                 INTENT(IN),  OPTIONAL :: ii !! Integer scalar value associated with `text`.
INTEGER, DIMENSION(da,db), INTENT(IN),  OPTIONAL :: d2 !! Integer array value associated with `text`.
LOGICAL, DIMENSION(da),   INTENT(IN),  OPTIONAL :: L1 !! Logical array value associated with `text`.
CHARACTER(*),            INTENT(IN)            :: text !! Name of the visualisation state item to set.
CHARACTER(*),            INTENT(IN), OPTIONAL  :: cc !! Character value associated with `text`.
coun = coun + 1
SELECT CASE(text)
CASE('north')      ; north    = ii
CASE('east')       ; east     = ii
CASE('south')      ; south    = ii
CASE('west')       ; west     = ii
    CASE('grid_nx')
        IF(szlimit>freelimit) PRINT*, 'THIS IS AN ILLEGAL COPY OF THE SHEGRAPH DLL 23/1/08'
        IF(ii>szlimit) THEN
            WRITE(dumtext,'(A,I4,A,I4,A)') '******* Grid size limit exceeded.  Limit is ',szlimit,' by ',szlimit,' cells'
            PRINT*, TRIM(dumtext)
            STOP
        ELSE
            grid_nx  = ii
        ENDIF
    CASE('grid_ny')
        IF(szlimit>freelimit) PRINT*, 'THIS IS AN ILLEGAL COPY OF THE SHEGRAPH DLL 23/1/08'
        IF(ii>szlimit) THEN
            WRITE(dumtext,'(A,I4,A,I4,A)') '******* Grid size limit exceeded.  Limit is ',szlimit,' by ',szlimit,' cells'
            PRINT*, TRIM(dumtext)
            STOP
        ELSE
            grid_ny  = ii
        ENDIF

CASE('top_cell')   ; top_cell = ii
CASE('nel')        ; nel      = ii
CASE('dirqq')      ; dirqq    = cc
CASE('is_square')  ; ALLOCATE(IS_SQUARE(nel))             ; IS_SQUARE = L1
CASE('is_bank')    ; ALLOCATE(IS_BANK(nel))               ; IS_BANK   = L1
CASE('is_link')    ; ALLOCATE(IS_LINK(nel))               ; IS_LINK   = L1
CASE('su')         ; ALLOCATE(SU_NUMBER(grid_nx,grid_ny)) ; SU_NUMBER = d2  !on HDF5 grid, not SHETRAN grid
CASE('bank_no')    ; ALLOCATE(BANK_NO(nel,4))             ; BANK_NO   = d2
CASE('river_no')   ; ALLOCATE(RIVER_NO(nel,4))            ; RIVER_NO  = d2
CASE('nsed')       ; nsed = ii
CASE('ncon')       ; ncon = ii
CASE('ver')        ; ver  = ii
CASE('rootdir')    ; rootdir = cc
CASE('hdf5fname')  ; hdf5filename=cc
CASE('planfile')   ; planfile=cc
CASE('checkfile')  ; checkfile=cc
CASE DEFAULT ; PRINT*, 'FAILED IN PASS  '//TRIM(text)//'  '//TRIM(cc) ; STOP
END SELECT
!PRINT*, coun, '******************'//TRIM(text)//'  '//TRIM(cc)
END SUBROUTINE send_p

END MODULE visualisation_pass
