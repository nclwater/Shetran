!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_pass
IMPLICIT NONE

INTEGER                              :: north, east, south, west, grid_nx, grid_ny, &
                                        top_cell, nel, nsed, ncon, ver
INTEGER, DIMENSION(:,:), ALLOCATABLE :: SU_NUMBER, BANK_NO, RIVER_NO
LOGICAL, DIMENSION(:),   ALLOCATABLE :: IS_SQUARE, IS_BANK, IS_LINK
CHARACTER(256)                        :: DIRQQ, ROOTDIR, hdf5filename, planfile, checkfile
!DEC$ DEFINE ISKEY=0
!DEC$ IF(ISKEY==0)
    INTEGER, PARAMETER                    :: freelimit=360000, szlimit=360000
    CHARACTER(256)                        :: dumtext
!DEC$ ENDIF

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

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL LOGICAL FUNCTION exists(i) RESULT(r)
INTEGER, INTENT(IN) :: i
r = i>0
END FUNCTION exists

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE send_p(text, ii, L1, d2, cc, da, db)
!DEC$ ATTRIBUTES DLLEXPORT :: send_p
integer, save :: coun=0
INTEGER, INTENT(IN)                            :: da, db
INTEGER,                 INTENT(IN),  OPTIONAL :: ii
INTEGER, DIMENSION(da,db), INTENT(IN),  OPTIONAL :: d2
LOGICAL, DIMENSION(da),   INTENT(IN),  OPTIONAL :: L1
CHARACTER(*),            INTENT(IN)            :: text
CHARACTER(*),            INTENT(IN), OPTIONAL  :: cc
coun = coun + 1
SELECT CASE(text)
CASE('north')      ; north    = ii
CASE('east')       ; east     = ii
CASE('south')      ; south    = ii
CASE('west')       ; west     = ii
!DEC$ IF(ISKEY==1)
    CASE('grid_nx')    ; grid_nx  = ii
    CASE('grid_ny')    ; grid_ny  = ii
!DEC$ ELSE
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
!DEC$ ENDIF
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