!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_metadata
!DEC$ REAL:4
!JE for SHEGRAPH Version 2.0 Created July 2004
USE VISUALISATION_PASS,      ONLY : SU_NUMBER, BANK_NO, RIVER_NO, EXISTS, nel, &
                                    IS_SQUARE, IS_BANK, IS_LINK, TOP_CELL, DIRQQ, nsed, ncon, &
                                    planfile, checkfile
USE VISUALISATION_READ,      ONLY : vp_in, vp_out, mess, mess2, mess3, ERROR, R_C, R_I, R_R, COPY
USE VISUALISATION_STRUCTURE, ONLY : MBR_COUNT, GET_MBR, csz
IMPLICIT NONE
INTEGER, PARAMETER                    :: ndim=6 !max no of dimensions in HDF5 file
REAL, DIMENSION(:), ALLOCATABLE, SAVE :: previous_time, next_time
LOGICAL, PARAMETER                    :: T=.TRUE., F=.FALSE.
REAL, PARAMETER                       :: zero = 0.0

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE ttime
PRIVATE
    INTEGER :: number, sz
    REAL, DIMENSION(:), POINTER  :: tstep=>NULL(), tstop=>NULL()  !step and stoptime
END TYPE ttime
TYPE(ttime), DIMENSION(:), POINTER :: times
TYPE(ttime), POINTER               :: sstatic
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE llist
PRIVATE
    INTEGER                        :: number, sz=0, indx=0 !number, size, and index
    CHARACTER(12)                  :: basis  !'grid_as_grid', 'grid_as_list' or 'list'
    CHARACTER(7)                   :: scope  !'all', 'squares', 'banks', 'rivers'
    INTEGER, DIMENSION(:), POINTER :: a
END TYPE llist
TYPE(LLIST), DIMENSION(:), POINTER :: lists=>NULL()
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE mask
PRIVATE
    INTEGER                          :: number, ilow, ihigh, jlow, jhigh, &
                                        listno  !first list assoc with this mask
    LOGICAL, DIMENSION(:,:), POINTER :: ma
END TYPE mask
TYPE(MASK), DIMENSION(:), POINTER :: masks=>NULL()
TYPE(MASK), POINTER               :: whole_grid=>NULL()
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE item
PRIVATE
    INTEGER              :: users_number=0, users_no_for_link_or_mask=0, users_no_for_times=0, &
                            sediment_no=0, contaminant_no=0
    INTEGER(INT_PTR_KIND())           :: first=0, latest=0
    CHARACTER(8)         :: name=''
    CHARACTER(2)         :: typ=''
    CHARACTER(csz)       :: title='*S' !for plots and printouts
    CHARACTER(8)         :: units=''
    CHARACTER(12)        :: basis='grid_as_grid'  !'grid_as_grid', 'grid_as_list' or 'list'
    CHARACTER(7)         :: scope='all'          !'all', 'squares', 'banks', 'rivers'
    CHARACTER(11)        :: extra_dimensions = '-'
    LOGICAL              :: isgrid = F,                &
                            istimeseries = F,          &
                            varies_with_sediment=F,    &
                            varies_with_contaminant=F, &
                            implemented=F
    INTEGER              :: layers(2)=(/0,0/)  !bottom and top layers
    TYPE(MASK), POINTER  :: amask=>NULL()
    TYPE(LLIST), POINTER :: alist=>NULL()
    TYPE(TTIME), POINTER :: atime=>NULL()
END TYPE item
TYPE(ITEM), DIMENSION(:), POINTER :: items=>NULL()

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE hdf5_item
    INTEGER              :: users_number = 0,              &
                            users_no_for_link_or_mask = 0, &
                            users_no_for_times=0,          &
                            ilow = 0,                      &
                            ihigh = 0,                     &
                            jlow = 0,                      &
                            jhigh = 0,                     &
                            klow = 0,                      &
                            khigh = 0,                     &
                            no_extra_dimensions = 0,       &
                            tstep_no = 1,                  &
                            sz   = 0,                      & !size of list
                            sediment_no   = 0,             & !sediment no
                            contaminant_no = 0               !contaminant no
    INTEGER, DIMENSION(:), POINTER :: dimensions, &  !dimensions
                                      szorder,    &
                                      list           !list of ssu numbers
    CHARACTER(8)         :: name=''
    CHARACTER(2)         :: typ=''
    CHARACTER(csz)       :: title='*S' !for plots and printouts
    CHARACTER(8)         :: units=''
    CHARACTER(12)        :: basis='grid_as_grid'  !'grid_as_grid', 'grid_as_list' or 'list'
    CHARACTER(7)         :: scope='all'          !'all', 'squares', 'banks', 'rivers'
    CHARACTER(11)        :: extra_dimensions = '-'
    CHARACTER(6), DIMENSION(:), POINTER :: names_of_extra_dimensions, &
                                           names_of_dimensions,       & 
                                           !'row', 'el-lst', 'column', 'el-typ', 'extra', 'time'
                                           mbr
                                           !'square', N-bank, etc
    LOGICAL              :: isgrid       = F,        &
                            istimeseries = F,        &
                            isreal       = T,        &
                            varies_with_sediment=F,  &
                            varies_with_contaminant=F

END TYPE hdf5_item
TYPE(HDF5_ITEM), DIMENSION(:), POINTER :: hdf5_items=>NULL()



INTEGER                  :: no_times=0, no_lists=0, no_masks=0, no_items=0, no_static_items=0
INTEGER, PARAMETER       :: sp=50  !DEBUG compiler bug  should be <sp>X in writes, but had to make it 50X
REAL, PARAMETER          :: small = 0.001
CHARACTER(4), PARAMETER  :: keywords(7)         = (/'item', 'list', 'mask', 'time', 'stop', 'kill', 'diag'/)
CHARACTER(12),PARAMETER  :: basis(3)            = (/'grid_as_grid', 'grid_as_list', 'list_as_list'/)
CHARACTER(7), PARAMETER  :: scope(4)            = (/'all', 'squares', 'banks', 'rivers'/)
CHARACTER(11), PARAMETER :: extra_dimensions(4) = (/'-','faces','X_Y', 'left_right'/)
LOGICAL                  :: diagnostics=F


PRIVATE
PUBLIC :: REGISTER_STATIC_VISUALISATION_METADATA,         &
          REGISTER_DYNAMIC_VISUALISATION_METADATA,        &
          GET_METADATA_C, GET_METADATA_L, GET_METADATA_I, &
          GET_METADATA_I_FIRST,                           &
          SET_METADATA_I,                                 &
          TIME_TO_RECORD,                                 &
          HDF5_ITEM, HDF5_ITEMS, ndim,                    &
          GET_METADATA_HDF5_I, GET_METADATA_HDF5_L, GET_METADATA_HDF5_C, &
          INCREMENT_HDF5_TSTEP_NO, csz

CONTAINS


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE INCREMENT_HDF5_TSTEP_NO(mn)
INTEGER, INTENT(IN) :: mn
hdf5_items(mn)%tstep_no = hdf5_items(mn)%tstep_no + 1
END SUBROUTINE INCREMENT_HDF5_TSTEP_NO


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
LOGICAL FUNCTION time_to_record(n, time) RESULT(r)
!DEC$ ATTRIBUTES DLLEXPORT :: time_to_record
INTEGER, INTENT(IN)                   :: n
INTEGER                               :: i
REAL, INTENT(IN)                      :: time  !hours
LOGICAL, SAVE :: first = T
IF(first) THEN
    first = F
    ALLOCATE(previous_time(no_items), next_time(no_items))
    previous_time = zero
    next_time     = GET_NEXT_TIME( (/(i,i=1,no_items)/) )
ENDIF
    IF(time==0.0) THEN
    r = T
ELSEIF(time>=next_time(n)-small) THEN
    r = T
    previous_time(n) = next_time(n)
    next_time(n)    = GET_NEXT_TIME(n)
ELSE
    r = F
ENDIF
END FUNCTION time_to_record

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL REAL FUNCTION get_next_time(n) RESULT(r)
INTEGER, INTENT(IN) :: n
INTEGER             :: j
j = 0
DO
    j = j + 1
    IF(items(n)%atime%tstop(j)>previous_time(n)) EXIT
ENDDO
r = MIN(items(n)%atime%tstop(j), previous_time(n) + items(n)%atime%tstep(j))
END FUNCTION get_next_time


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE FUNCTION get_metadata_c(i, text) RESULT(r)
!DEC$ ATTRIBUTES DLLEXPORT :: get_metadata_c
INTEGER, INTENT(IN)      :: i
CHARACTER(*), INTENT(IN) :: text
CHARACTER(csz)           :: r
SELECT CASE(text)
CASE('basis') ; r=items(i)%basis
CASE('name')  ; r=items(i)%name
CASE('title') ; r=items(i)%title
CASE('typ')   ; r=items(i)%typ
CASE('units') ; r=items(i)%units
CASE('scope') ; r=items(i)%scope
CASE('extra_dimensions') ; r = items(i)%extra_dimensions
CASE DEFAULT ; r='failed ito find '//TRIM(text)//' in get_metadata_c'
END SELECT
END FUNCTION get_metadata_c

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL FUNCTION get_metadata_HDF5_c(i, text, e) RESULT(r)
INTEGER, INTENT(IN)           :: i
INTEGER, INTENT(IN), OPTIONAL :: e
CHARACTER(*), INTENT(IN)      :: text
CHARACTER(csz)                :: r
SELECT CASE(text)
CASE('basis')                     ; r=hdf5_items(i)%basis
CASE('el-typ')                    ; r=hdf5_items(i)%mbr(e)
CASE('name')                      ; r=hdf5_items(i)%name
CASE('names_of_dimensions')       ; r=hdf5_items(i)%names_of_dimensions(e)
CASE('names_of_extra_dimensions') ; r=hdf5_items(i)%names_of_extra_dimensions(e)
CASE('title')                     ; r=hdf5_items(i)%title
CASE('typ')                       ; r=hdf5_items(i)%typ
CASE('units')                     ; r=hdf5_items(i)%units
CASE('scope')                     ; r=hdf5_items(i)%scope
CASE DEFAULT ; r='failed ito find '//TRIM(text)//' in get_hdf5_metadata_c'
END SELECT
END FUNCTION get_metadata_HDF5_c

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION get_metadata_i(i, text, su) RESULT(r)
!DEC$ ATTRIBUTES DLLEXPORT :: get_metadata_i
INTEGER, INTENT(IN)           :: i
INTEGER, INTENT(IN), OPTIONAL :: su
CHARACTER(*), INTENT(IN)      :: text
SELECT CASE(text)
CASE('ext')      ; r=NO_EXTRA_DIMENSIONS(items(i)%extra_dimensions)
!!CASE('first')    ; r=items(i)%first
CASE('ilow')     ; IF(items(i)%isgrid) THEN ; r=items(i)%amask%ilow  ; ELSE ; r=1                 ; ENDIF
CASE('ihigh')    ; IF(items(i)%isgrid) THEN ; r=items(i)%amask%ihigh ; ELSE ; r=items(i)%alist%sz ; ENDIF
CASE('jlow')     ; IF(items(i)%isgrid) THEN ; r=items(i)%amask%jlow  ; ELSE ; r=1                 ; ENDIF
CASE('jhigh')    ; IF(items(i)%isgrid) THEN ; r=items(i)%amask%jhigh ; ELSE ; r=1                 ; ENDIF
CASE('klow')     ; r=items(i)%layers(1)
CASE('khigh')    ; r=items(i)%layers(2)
!!CASE('latest')   ; r=items(i)%latest
CASE('no_items') ; r=no_items
CASE('su')       ; r=items(i)%alist%a(su)
CASE('sz')       ; r=items(i)%alist%sz
CASE('nsed')     ; r=items(i)%sediment_no
CASE('ncon')     ; r=items(i)%contaminant_no
CASE DEFAULT     ; r=HUGE(1)
END SELECT
END FUNCTION get_metadata_i

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER(INT_PTR_KIND()) FUNCTION get_metadata_i_first(i, text, su) RESULT(r)
!DEC$ ATTRIBUTES DLLEXPORT :: get_metadata_i_first
INTEGER, INTENT(IN)           :: i
INTEGER, INTENT(IN), OPTIONAL :: su
CHARACTER(*), INTENT(IN)      :: text
SELECT CASE(text)
CASE('first')    ; r=items(i)%first
CASE('latest')   ; r=items(i)%latest
CASE DEFAULT     ; r=HUGE(1_8)
END SELECT
END FUNCTION get_metadata_i_first

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION get_metadata_hdf5_i(i, text, e) RESULT(r)
INTEGER, INTENT(IN)           :: i
INTEGER, INTENT(IN), OPTIONAL :: e
CHARACTER(*), INTENT(IN)      :: text
SELECT CASE(text)
CASE('dimensions')          ; r=hdf5_items(i)%dimensions(e)
CASE('ext')                 ; r=NO_EXTRA_DIMENSIONS(hdf5_items(i)%extra_dimensions)
CASE('ilow')                ; r=hdf5_items(i)%ilow
CASE('ihigh')               ; r=hdf5_items(i)%ihigh
CASE('jlow')                ; r=hdf5_items(i)%jlow
CASE('jhigh')               ; r=hdf5_items(i)%jhigh
CASE('klow')                ; r=hdf5_items(i)%klow
CASE('khigh')               ; r=hdf5_items(i)%khigh
CASE('list')                ; r=hdf5_items(i)%list(e)
CASE('no_extra_dimensions') ; r=hdf5_items(i)%no_extra_dimensions
CASE('no_mbr')              ; r = SIZE(hdf5_items(i)%mbr,DIM=1)
CASE('no_items')            ; r=no_items
CASE('no_dimensions')       ; r=COUNT(hdf5_items(i)%dimensions/=0)
CASE('sz')                  ; r=hdf5_items(i)%sz
CASE('szorder')             ; r=hdf5_items(i)%szorder(e)
CASE('tstep_no')            ; r=hdf5_items(i)%tstep_no
CASE('users_number')        ; r=hdf5_items(i)%users_number
CASE('nsed')                ; r=hdf5_items(i)%sediment_no
CASE('ncon')                ; r=hdf5_items(i)%contaminant_no
CASE DEFAULT                ; r=HUGE(1)
END SELECT
END FUNCTION get_metadata_hdf5_i

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE set_metadata_i(i, text, a)
!DEC$ ATTRIBUTES DLLEXPORT :: set_metadata_i
INTEGER, INTENT(IN)           :: i
INTEGER(INT_PTR_KIND()), INTENT(IN)        :: a
CHARACTER(*), INTENT(IN)      :: text
SELECT CASE(text)
CASE('first')  ; items(i)%first  = a
CASE('latest') ; items(i)%latest = a
END SELECT
END SUBROUTINE set_metadata_i

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE LOGICAL FUNCTION get_metadata_L(I, text, a, b) RESULT(r)
!DEC$ ATTRIBUTES DLLEXPORT :: get_metadata_L
INTEGER, INTENT(IN)           :: i
INTEGER, INTENT(IN), OPTIONAL :: a,b
CHARACTER(*), INTENT(IN)      :: text
SELECT CASE(text)
CASE('on')           ; r=items(i)%amask%ma(a,b)
CASE('isgrid')       ; r=items(i)%isgrid
CASE('istimeseries') ; r=items(i)%istimeseries
CASE('isreal')          ; r = ANY(items(i)%typ(1:1)==(/'B','G','L','M'/))
CASE('varies_with_sediment')    ; r=items(i)%varies_with_sediment
CASE('varies_with_contaminant') ; r=items(i)%varies_with_contaminant
CASE DEFAULT         ; r = F
END SELECT
END FUNCTION get_metadata_L


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE LOGICAL FUNCTION get_metadata_HDF5_L(I, text) RESULT(r)
INTEGER, INTENT(IN)           :: i
CHARACTER(*), INTENT(IN)      :: text
SELECT CASE(text)
CASE('isgrid')                  ; r=hdf5_items(i)%isgrid
CASE('istimeseries')            ; r=hdf5_items(i)%istimeseries
CASE('isreal')                  ; r = ANY(hdf5_items(i)%typ(1:1)==(/'B','G','L','M'/))
CASE('varies_with_sediment')    ; r=hdf5_items(i)%varies_with_sediment
CASE('varies_with_contaminant') ; r=hdf5_items(i)%varies_with_contaminant
CASE DEFAULT         ; r = F
END SELECT
END FUNCTION get_metadata_HDF5_L


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE read_dynamic_visualisation_metadata()
INTEGER              :: i
CHARACTER(4)         :: now
CALL COPY(DIRQQ, planfile)
!CALL STRIP(file='input-files/visualisation_plan.txt', u=ur, checktitle='visualisation plan', delimiter='!', separator=(/':','^'/))
!!!WRITE(vp_in,'(/A)') 'Opened '//TRIM(DIRQQ)//'/'//'input/visualisation_plan.txt'
now = CYCLE_TILL_KEYWORD()
IF(now/='diag') THEN
    WRITE(vp_in,'(A)') 'TO GET DIAGNOSTIC INFO IN THIS CHECK FILE'
    WRITE(vp_in,'(A)') 'ADD A LINE CONTAINING diag IN VISUALISATION_PLAN.TXT'
    WRITE(vp_in,'(A)') 'PUT THIS LINE BEFORE ANY ITEMS, MASKS, ETC'
ENDIF
DO WHILE(now/='stop' .AND. now/='kill')
    CALL HANDLE(now)
    now = CYCLE_TILL_KEYWORD()
ENDDO
IF(now=='kill') THEN
    WRITE(vp_in,*)
    WRITE(vp_in,*)'KILLED RUN so the visualisation plan can be checked'
    PRINT*
    PRINT*, 'KILLED RUN so visualisation plan can be checked'
    PRINT*, 'Look in output/check_visualisation_plan.txt'
    PRINT*
    STOP
ELSE
!    CALL CHECK()
ENDIF
DO i=no_static_items+1,no_items
    CALL LINK_USERS_NUMBERS_TO_INDEXES(items(i))
ENDDO
CALL FINAL_CHECK_OF_ITEM()

END SUBROUTINE read_dynamic_visualisation_metadata


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE register_static_visualisation_metadata(name, typ, units, title, szi, szj, extra_dimensions, varies_with_elevation)
!DEC$ ATTRIBUTES DLLEXPORT :: register_static_visualisation_metadata
INTEGER, INTENT(IN)      :: szi, szj
CHARACTER(*), INTENT(IN) :: name, units, title, extra_dimensions
CHARACTER, INTENT(IN)    :: typ
LOGICAL, INTENT(IN)      :: varies_with_elevation
TYPE(ITEM), POINTER      :: ii
CALL WRITE_STA_VARIABLE(name, units, title, extra_dimensions, varies_with_elevation)
CALL INCREMENT_item(items,1)
no_static_items = no_static_items + 1
ii                  => items(no_items)
ii%name             =  name
ii%istimeseries     = F
ii%typ              =  typ//'S'  !s for static
ii%title            =  title
ii%units            =  units
ii%basis            =  'grid_as_grid'
ii%scope            =  'all'
ii%extra_dimensions = extra_dimensions
ii%isgrid           =  T
IF(varies_with_elevation) THEN ; ii%layers =(/1,TOP_CELL/) ; ELSE ; ii%layers=(/0,0/) ; ENDIF
ii%amask  => POINT_TO_WHOLE_GRID(szi,szj)
ii%atime  => POINT_TO_STATIC()
END SUBROUTINE register_static_visualisation_metadata


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE write_sta_variable(name, units, title, extra_dimensions, varies_with_elev)
CHARACTER(*), INTENT(IN)         :: name, units, title, extra_dimensions
LOGICAL, INTENT(IN)              :: varies_with_elev
LOGICAL, SAVE                    :: first=T
CHARACTER(LEN(extra_dimensions)) :: ed
IF(extra_dimensions=='-') THEN ; ed = '-' ; ELSE ; ed=extra_dimensions ; ENDIF
IF(first) THEN
    first = F
   !! OPEN(unit=uw, FILE=TRIM(DIRQQ)//'/'//'output/check_visualisation_plan.txt', ACTION='WRITE', STATUS='UNKNOWN')
   !print*, 'CHECKFILE1 = ', TRIM(checkfile)
    OPEN(unit=vp_out, FILE=checkfile, ACTION='WRITE', STATUS='UNKNOWN')
    WRITE(vp_out,'(A)') 'Full list of constants recorded in the HDF5 file'
    WRITE(vp_out,'(A)') 'E-varies with subsurface elevation'
ENDIF
WRITE(vp_out,'(A8, A8, A9, A12, A70)') name, V_ELEV(varies_with_elev), units, ed, title
END SUBROUTINE write_sta_variable
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE write_dyn_variable(name, units, title, extra_dimensions, varies_with_elev, varies_with_sed, varies_with_con)
CHARACTER(*), INTENT(IN)         :: name, units, title, extra_dimensions
LOGICAL, INTENT(IN)              :: varies_with_elev, varies_with_sed, varies_with_con
LOGICAL, SAVE                    :: first=T
IF(first) THEN
    first = F
!    OPEN(unit=uw, FILE=TRIM(DIRQQ)//'/'//'output/check_visualisation_plan.txt', ACTION='WRITE', STATUS='UNKNOWN')
    !print*, 'CHECKFILE2 = ', TRIM(checkfile)
    OPEN(unit=vp_out, FILE=checkfile, ACTION='WRITE', STATUS='UNKNOWN')
    WRITE(vp_out,'(A80)') REPEAT('-',80)
    WRITE(vp_out,'(A)') 'Full list of variables that can be recorded in the HDF5 file'
    WRITE(vp_out,'(A)') 'E-varies with subsurface elevation; C-varies with contaminant no; S-varies with sediment fraction no'
ENDIF
WRITE(vp_out,'(A8, A8, A9, A12, A70)') name, V_E_SED_CON(varies_with_elev,varies_with_sed,varies_with_con), units, extra_dimensions, title
END SUBROUTINE write_dyn_variable
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE CHARACTER(7) FUNCTION v_e_sed_con(v,s,c) RESULT(r)
INTEGER             :: p
LOGICAL, INTENT(IN) :: v,s,c
r = REPEAT(' ',LEN(r))
p = 3
IF(v) THEN ; r(p:p)='E' ; p=p+2 ; ENDIF
IF(s) THEN ; r(p:p)='S' ; p=p+2 ; ENDIF
IF(c) r(p:p)='C'
END FUNCTION v_e_sed_con
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE CHARACTER(5) FUNCTION v_elev(v) RESULT(r)
INTEGER             :: p
LOGICAL, INTENT(IN) :: v
r = REPEAT(' ',LEN(r))
p = 3
IF(v) r(p:p)='E'
END FUNCTION v_elev


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE register_dynamic_visualisation_metadata(jj, final, name, typ, units, title, &
           extra_dimensions, varies_with_elevation, varies_with_sed, varies_with_con, implemented)
!DEC$ ATTRIBUTES DLLEXPORT :: register_dynamic_visualisation_metadata
INTEGER                                  :: i
INTEGER, INTENT(IN)                      :: jj
CHARACTER(*), INTENT(IN)                 :: name, units, title, extra_dimensions
CHARACTER, INTENT(IN)                    :: typ
LOGICAL, INTENT(IN)                      :: final, & !is final call to this routine
                                            varies_with_elevation, varies_with_sed, varies_with_con, implemented
LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE :: found
TYPE(ITEM), POINTER                      :: ii=>NULL()
LOGICAL, SAVE                            :: first=T
IF(jj==1) THEN
    IF(implemented) CALL WRITE_DYN_VARIABLE(name, units, title, extra_dimensions, varies_with_elevation, varies_with_sed, varies_with_con)
    RETURN
ENDIF
IF(first) THEN
    first= F
    CALL READ_DYNAMIC_VISUALISATION_METADATA()
    ALLOCATE(found(NO_static_items+1:no_items))
    found = F
ENDIF
DO i=no_static_items+1, no_items
    IF(items(i)%name /= name) CYCLE
        found(i)                   = T
        ii                         =>items(i)
        ii%istimeseries            = T
        ii%varies_with_sediment    = varies_with_sed
        ii%varies_with_contaminant = varies_with_con
        ii%implemented             = implemented
        ii%units                   = units
        ii%title                   = title
        ii%extra_dimensions        = extra_dimensions
        ii%typ = ALTER_DYNAMIC_TYPE(typ, ii)//'S'  !match up defined types and user's request
ENDDO
IF(final) THEN
    DO i=no_static_items+1, no_items
        IF(.NOT.found(i)) THEN
            WRITE(mess,*) TRIM(items(i)%name)//' not recognised as dynamic variable'
            CALL ERROR()
        ELSEIF(.NOT.items(i)%implemented) THEN
            WRITE(mess,*) TRIM(items(i)%name)//' is listed in documentation'
            WRITE(mess2,*)'but has not yet been implemented '
            WRITE(mess3,*)'see the variable variables list in check_visualisation_plan.txt'
            CALL ERROR()
        ENDIF
        ii  =>items(i)
        CALL CHECK_ITEM(ii)
        IF(diagnostics) WRITE(vp_out,'(50X,A)') 'read item'
        WRITE(vp_out,'(A,I3,9A,I3,A,I3,A,2I3, 2(A,I3))')    &
                     'ITEM:',                ii%users_number,              &
                     '  NAME:',              ii%name,                      &
                     '  BASIS: ',            ii%basis,                     &
                     '  SCOPE:',             ii%scope,                     &
                     '  EXTRA_DIMENSIONS: ', ii%extra_dimensions,          &
                     '  GRID/LIST NUMBER: ', ii%users_no_for_link_or_mask, &
                     '  TIMES NUMBER:',      ii%users_no_for_times,        &
                     '  LAYERS: ',           ii%layers,                    &
                     '  SEDIMENT_NO: ',      ii%sediment_no,               &
                     '  CONTAMINANT_NO: ',   ii%contaminant_no
    ENDDO

    CALL CREATE_HDF5_METADATA()
ENDIF

END SUBROUTINE register_dynamic_visualisation_metadata

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE create_hdf5_metadata()
INTEGER                  :: mn, nex
TYPE(ITEM), POINTER      :: ii
TYPE(HDF5_ITEM), POINTER :: hh
ALLOCATE(hdf5_items(no_items))
DO mn=1,no_items
    ii => items(mn)
    hh => hdf5_ITEMS(mn)
    hh%users_number              = ii%users_number
    hh%users_no_for_link_or_mask = ii%users_no_for_link_or_mask
    hh%users_no_for_times        = ii%users_no_for_times
    hh%name                      = ii%name
    hh%typ                       = ii%typ
    hh%title                     = ii%title
    hh%units                     = ii%units
    hh%basis                     = ii%basis
    hh%scope                     = ii%scope
    nex                          = NO_EXTRA_DIMENSIONS(ii%extra_dimensions)
    hh%no_extra_dimensions       = nex
    hh%extra_dimensions          = GET_METADATA_C(mn,'extra_dimensions')
    ALLOCATE(hh%names_of_extra_dimensions(nex))
    hh%names_of_extra_dimensions = NAMES_of_EXTRA_DIMENSIONS(nex, ii%extra_dimensions)
    hh%isgrid                    = ii%isgrid
    hh%isreal                    = GET_METADATA_L(mn,'isreal')
    hh%istimeseries              = ii%istimeseries
    hh%varies_with_sediment      = ii%varies_with_sediment
    hh%varies_with_contaminant   = ii%varies_with_contaminant
    hh%ilow   = GET_METADATA_I(mn,'ilow')
    hh%ihigh  = GET_METADATA_I(mn,'ihigh')
    hh%jlow   = GET_METADATA_I(mn,'jlow')
    hh%jhigh  = GET_METADATA_I(mn,'jhigh')
    hh%klow   = GET_METADATA_I(mn,'klow')
    hh%khigh  = GET_METADATA_I(mn,'khigh')
    hh%sediment_no    = GET_METADATA_I(mn,'nsed')
    hh%contaminant_no = GET_METADATA_I(mn,'ncon')
   ALLOCATE(hh%dimensions(ndim), hh%names_of_dimensions(ndim), hh%szorder(ndim))
    CALL GET_SZ_CR(hh)
    hh%mbr => GET_MBR(hh%typ)
    IF(.NOT.hh%isgrid) THEN
        hh%sz = ii%alist%sz
        ALLOCATE(hh%list(hh%sz))
        hh%list = ii%alist%a
    ENDIF
ENDDO
END SUBROUTINE create_hdf5_metadata


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE GET_SZ_CR(h)
INTEGER                  :: r, mbr, nextra
TYPE(HDF5_ITEM), POINTER :: h
mbr    = MBR_COUNT(h%typ)
nextra = h%no_extra_dimensions
IF(h%isgrid) THEN
    r=5 ; h%names_of_dimensions(r)='column'
ELSE
    r=6 ; h%names_of_dimensions(r)='el-lst'
ENDIF
h%dimensions(r) = h%ihigh-h%ilow+1
h%szorder(1) = r
IF(h%isgrid) THEN
    r=6 ; h%dimensions(r) = h%jhigh-h%jlow+1
ELSE
    r=5 ; h%dimensions(r) = 0
ENDIF
      h%names_of_dimensions(r)='row'    ;                                  ; h%szorder(2) = r
r=3 ; h%names_of_dimensions(r)='layer'  ; IF(h%khigh>0) THEN ; h%dimensions(r)=h%khigh-h%klow+1 ; ELSE ; h%dimensions(r)=0 ; ENDIF ; h%szorder(3) = r
r=4 ; h%names_of_dimensions(r)='el_typ' ; h%dimensions(r)=mbr              ; IF(h%dimensions(r)==1) h%dimensions(r)=0  ; h%szorder(4) = r
r=2 ; h%names_of_dimensions(r)='extra'  ; h%dimensions(r)=nextra           ; IF(h%dimensions(r)==1) h%dimensions(r)=0  ; h%szorder(5) = r
r=1 ; h%names_of_dimensions(r)='time'   ; IF(h%istimeseries) THEN ; h%dimensions(r)=1 ; ELSE ; h%dimensions(r)=0 ; ENDIF ; h%szorder(6) = r
END SUBROUTINE GET_SZ_CR


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION calc_rank(h) RESULT(r)
INTEGER                  :: mbr
TYPE(HDF5_ITEM), POINTER :: h
mbr = MBR_COUNT(h%typ)
r   = 1
IF(h%jhigh-h%jlow>1)        r=r+1
IF(h%khigh-h%klow>1)        r=r+1
IF(mbr>1)                   r=r+1
IF(h%no_extra_dimensions>1) r=r+1
IF(h%istimeseries)          r=r+1
END FUNCTION calc_rank


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
CHARACTER FUNCTION alter_dynamic_type(typ, ii) RESULT(r)
CHARACTER, INTENT(IN)     :: typ
TYPE(ITEM), INTENT(INOUT) :: ii
IF(typ=='W') THEN
    WRITE(mess,*) 'cannot handle type W data' ; CALL ERROR()
    ii%typ = 'W*'
    RETURN
ENDIF
r = '$'
IF(.NOT.ii%isgrid) THEN !is being treated as list
    SELECT CASE(typ)
    CASE('C') ; r='V'
    CASE('G') ; r='M'
    CASE('H') ; r='X'
    CASE('L') ; r='M'
    CASE('Q') ; r='Z'
    END SELECT
ELSE                    !is being treated as grid
    SELECT CASE(ii%scope)
    CASE('all') ; r=typ
    CASE('squares')
        SELECT CASE(typ)
        CASE('C') ; r='V'
        CASE('G') ; r='M'
        CASE('H') ; r='X'
        CASE('L') ; r='L'
        CASE('Q') ; r='Z'
        END SELECT
    CASE('banks')
        SELECT CASE(typ)
        CASE('C') ; r='K'
        CASE('G') ; r='B'
        CASE('H') ; r='T'
        CASE('L') ; r='L'
        CASE('Q') ; r='A'
        END SELECT
    CASE('rivers')
        SELECT CASE(typ)
        CASE('C') ; r='O'
        CASE('G') ; r='L'
        CASE('H') ; r='U'
        CASE('L') ; r='L'
        CASE('Q') ; r='D'
        END SELECT
    END SELECT
ENDIF
END FUNCTION alter_dynamic_type



!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE HANDLE(now)
INTEGER                  :: orig
CHARACTER(4), INTENT(IN) :: now
SELECT CASE(now)
CASE('item')
    CALL INCREMENT_item(items,1)
    CALL READ_ITEM(items(no_items))
CASE('list')
    CALL INCREMENT_LIST(lists,1)
    CALL READ_LIST(lists(no_lists))
    orig = no_lists

    CALL INCREMENT_LIST(lists,1)
    lists(no_lists) = MAKE_LIST_FROM_LIST(lists(orig), 'squares')

    CALL INCREMENT_LIST(lists,1)
    lists(no_lists) = MAKE_LIST_FROM_LIST(lists(orig), 'banks')

    CALL INCREMENT_LIST(lists,1)
    lists(no_lists) = MAKE_LIST_FROM_LIST(lists(orig), 'rivers')
CASE('mask')
    CALL INCREMENT_MASK(masks,1)
    CALL READ_MASK(masks(no_masks), off=(/'=','.'/))

    CALL INCREMENT_LIST(lists,1)
    lists(no_lists) = MAKE_LIST_FROM_MASK(masks(no_masks), 'all')
    masks(no_masks)%listno = no_lists

    CALL INCREMENT_LIST(lists,1)
    lists(no_lists) = MAKE_LIST_FROM_MASK(masks(no_masks), 'squares')

    CALL INCREMENT_LIST(lists,1)
    lists(no_lists) = MAKE_LIST_FROM_MASK(masks(no_masks), 'banks')

    CALL INCREMENT_LIST(lists,1)
    lists(no_lists) = MAKE_LIST_FROM_MASK(masks(no_masks), 'rivers')
CASE('time')
    CALL INCREMENT_TIME(times,1)
    CALL READ_TIME(times(no_times))
CASE('diag')
    diagnostics = T
END SELECT
END SUBROUTINE HANDLE

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE link_users_numbers_to_indexes(it)
INTEGER                   :: uun
TYPE(ITEM), INTENT(INOUT) :: it
uun = it%users_no_for_link_or_mask
SELECT CASE(it%basis)
CASE('grid_as_grid')
    it%amask =>POINT_TO_MASK(uun)
    it%isgrid = T
CASE('grid_as_list')
    it%alist => POINT_TO_LIST(uun, it%basis, it%scope)
CASE('list_as_list')
    it%alist => POINT_TO_LIST(uun, it%basis, it%scope)
END SELECT
it%atime =>POINT_TO_TIME(it%users_no_for_times)
END SUBROUTINE link_users_numbers_to_indexes



!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION point_to_static() RESULT(r)
TYPE(TTIME), POINTER :: r
LOGICAL, SAVE        :: first=T
IF(first) THEN
    first    =  F
    ALLOCATE(sstatic)
    r  => sstatic
    r%number = 999
    r%sz     = 1
    ALLOCATE(r%tstep(1), r%tstop(1))
    r%tstep(1) = HUGE(1.0)
    r%tstop(1) = HUGE(1.0)
ENDIF
r => sstatic
END FUNCTION point_to_static


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION point_to_whole_grid(i,j) RESULT(r)
TYPE(MASK), POINTER :: r
INTEGER, INTENT(IN) :: i,j
LOGICAL, SAVE :: first=T
IF(first) THEN
    first    =  F
    ALLOCATE(whole_grid)
    r => whole_grid
    r%number = 999
    r%ilow   =1
    r%ihigh  =i
    r%jlow   =1
    r%jhigh  =j
    ALLOCATE(r%ma(i,j))
    r%ma = T
ENDIF
r => whole_grid
END FUNCTION point_to_whole_grid



!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION extra(s) RESULT(r)
CHARACTER(*), INTENT(IN) :: s
SELECT CASE(s)
CASE('all')    ; r=0
CASE('squares') ; r=1
CASE('banks')  ; r=2
CASE('rivers')  ; r=3
END SELECT
END FUNCTION extra

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION point_to_mask(users_no_for_link_or_mask) RESULT(r)
INTEGER, INTENT(IN) :: users_no_for_link_or_mask
INTEGER             :: I
TYPE(MASK), POINTER :: r
r=>NULL()
DO i=1,no_masks
    IF(masks(i)%number==users_no_for_link_or_mask) THEN
        r=>masks(i)
        EXIT
    ENDIF
ENDDO
IF(.NOT.ASSOCIATED(r)) THEN
    WRITE(mess,'(A,I3)') 'Failed to find mask ',users_no_for_link_or_mask
    CALL ERROR()
ENDIF
END FUNCTION point_to_mask

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION point_to_list(users_no_for_link_or_mask, basis, scope) RESULT(r)
INTEGER                  :: i, j
INTEGER, INTENT(IN)      :: users_no_for_link_or_mask
CHARACTER(*), INTENT(IN) :: basis, scope
TYPE(LLIST), POINTER :: r
r=>null()
j = 0
IF(basis=='grid_as_list') THEN
    DO i=1,no_masks
        IF(masks(i)%number==users_no_for_link_or_mask) THEN
            j = i
            EXIT
        ENDIF
    ENDDO
    IF(j==0) THEN
        WRITE(mess,'(A,I3)') 'Failed to find mask ',users_no_for_link_or_mask
        CALL ERROR()
    ENDIF
    r => lists(masks(i)%listno+EXTRA(scope))
ELSE
    DO i=1,no_lists
        IF(lists(i)%number==users_no_for_link_or_mask) THEN
            j = i
            EXIT
        ENDIF
    ENDDO
    IF(j==0) THEN
        WRITE(mess,'(A,I3)') 'Failed to find list ',users_no_for_link_or_mask
        CALL ERROR()
    ENDIF
    r =>lists(j+EXTRA(scope))
ENDIF

END FUNCTION point_to_list

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION point_to_time(users_no_for_times) RESULT(r)
INTEGER, INTENT(IN)  :: users_no_for_times
INTEGER              :: I
TYPE(TTIME), POINTER :: r
r=>NULL()
DO i=1,no_times
    IF(times(i)%number==users_no_for_times) THEN
        r=>times(i)
        EXIT
    ENDIF
ENDDO
IF(.NOT.ASSOCIATED(r)) THEN
    WRITE(mess,'(A,I3)') 'Failed to find times data set ',users_no_for_times
    CALL ERROR()
ENDIF
END FUNCTION point_to_time

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
TYPE(ITEM) FUNCTION get_item(i) RESULT(r)
INTEGER, INTENT(IN) :: i
r = items(i)
END FUNCTION get_item


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION no_of_items(text) RESULT(r)
CHARACTER(*), INTENT(IN), OPTIONAL :: text
IF(PRESENT(text)) THEN
    r = no_items
ELSE
    SELECT CASE(text)
    CASE('static')  ; r=no_static_items
    CASE('dynamic') ; r=no_items-no_static_items
    END SELECT
ENDIF
END FUNCTION no_of_items



!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
CHARACTER(4) FUNCTION cycle_till_keyword() RESULT(r)
WRITE(vp_out,*)
IF(diagnostics) WRITE(vp_out,'(50X,A)') 'looking for keyword'
DO
    CALL R_C('keyword', r)
    IF(ANY(r==keywords)) EXIT
ENDDO
IF(diagnostics) WRITE(vp_out,'(50X,A)') 'found keyword ', r
RETURN
r = 'stop'
END FUNCTION cycle_till_keyword

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE read_item(s)
INTEGER                   :: number
CHARACTER(csz)            :: dum, name
TYPE(ITEM), INTENT(INOUT) :: s
        IF(diagnostics) WRITE(vp_out,'(50X,A)') 'reading a item'
DO
    CALL R_C(' ',dum)
    IF(dum=='ENDITEM') EXIT
    SELECT CASE(dum)
        CASE('NUMBER')           ; CALL R_I('NUMBER',s%users_number)
        CASE('NAME')             ; CALL R_C('NAME',  s%name)
        CASE('BASIS')            ; CALL R_C('basis', s%basis)
        CASE('SCOPE')            ; CALL R_C('SCOPE', s%scope)
        CASE('EXTRA_DIMENSIONS') ; CALL R_C('EXTRA_DIMENSIONS', s%extra_dimensions)
        CASE('GRID_OR_LIST_NO')  ; CALL R_I('no for GRID or LIST',s%users_no_for_link_or_mask)
        CASE('TIMES')            ; CALL R_I('no for TIMES',s%users_no_for_times)
        CASE('LAYERS')           ; CALL R_I('LAYERS',2,s%layers)
                                   IF(s%layers(1)>s%layers(2)) s%layers = s%layers(2:1:-1)
        CASE('SEDIMENT_NO')      ; CALL R_I('no for sediment',s%sediment_no)
        CASE('CONTAMINANT_NO')   ; CALL R_I('no for contaminant',s%contaminant_no)
        CASE('as_above')
            name           = s%name
            number         = s%users_number
            s              = items(no_items-1)
            s%name         = name
            s%users_number = number
!            s%group_with   = no_items-1
        CASE DEFAULT
            WRITE(mess,'(A,I4)') TRIM(dum)//'  Unrecognised heading in item number',s%users_number
            CALL ERROR()
    END SELECT
ENDDO
END SUBROUTINE read_item




!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE check_item(a)
TYPE(ITEM), INTENT(IN) :: a
IF (ALL(a%basis/=basis)) THEN
    WRITE(mess,'(2A)') a%basis,'  BASIS NOT RECOGNISED'
    WRITE(mess2,'(A,10A14)') ' SHOULD BE ONE OF: ',basis
    CALL ERROR()
ENDIF
IF (ALL(a%scope/=scope)) THEN
    WRITE(mess,'(2A)')  a%scope,'SCOPE NOT RECOGNISED'
    WRITE(mess2,'(A,10A8)') 'SHOULD BE ONE OF: ',scope
    CALL ERROR()
ENDIF
IF (ALL(a%extra_dimensions/=extra_dimensions)) THEN
    WRITE(mess,'(2A)')  a%extra_dimensions,'EXTRA_DIMENSION NOT RECOGNISED'
    WRITE(mess2,'(A,10A8)') 'SHOULD BE ONE OF: ',extra_dimensions
    CALL ERROR()
ENDIF
IF(a%varies_with_sediment) THEN
    IF(a%sediment_no<1 .OR. a%sediment_no>nsed) THEN
        WRITE(mess,'(A,I4,A,I4,A)')  'IN ITEM ', a%users_number, ' SEDIMENT No ',a%sediment_no, ' DOES NOT EXIST' 
        CALL ERROR()
    ENDIF
ELSEIF(a%sediment_no/=0) THEN
    WRITE(mess,'(A,I4,A,I4,A)')  'IN ITEM ', a%users_number, ' SEDIMENT No ',a%sediment_no, ' SHOULD NOT BE SPECIFIED' 
    CALL ERROR()
ENDIF
IF(a%varies_with_contaminant) THEN
    IF(a%contaminant_no<1 .OR. a%contaminant_no>ncon) THEN
        WRITE(mess,'(A,I4,A,I4,A)')  'IN ITEM ',a%users_number, ' CONTAMINANT No ',a%contaminant_no, ' DOES NOT EXIST'
        CALL ERROR()
    ENDIF
ELSEIF(a%contaminant_no/=0) THEN
    WRITE(mess,'(A,I4,A,I4,A)')  'IN ITEM ', a%users_number, ' SEDIMENT No ',a%contaminant_no, ' SHOULD NOT BE SPECIFIED' 
    CALL ERROR()
ENDIF

END SUBROUTINE check_item

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE final_check_of_item()
INTEGER             :: i, cnt
TYPE(ITEM), POINTER :: a
cnt = 0
DO i=1,SIZE(items,dim=1)
    a => items(i)
    IF(a%basis=='grid_as_list' .OR. a%basis=='list_as_list') THEN
        IF(a%alist%sz<1) THEN
            WRITE(vp_out,'(A,I6,A)') 'zero sized list for item ', a%users_number, ' MUST ELIMINATE THIS ITEM'
            cnt = cnt + 1
        ENDIF
    ENDIF
    IF(a%layers(1)<0 .OR. a%layers(2)>TOP_CELL) THEN
        WRITE(vp_out,'(A,I6,A,I6)') 'layer range must lie between 1 and', TOP_CELL, ' in item', a%users_number
            cnt = cnt + 1
    ENDIF
ENDDO
IF(cnt>0) CALL ERROR()
END SUBROUTINE final_check_of_item


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE read_time(t)
INTEGER                    :: i
TYPE(TTIME), INTENT(INOUT) :: t
        IF(diagnostics) WRITE(vp_out,'(50X,A)') 'reading times'
CALL R_I('TIMES number and size', t%number, t%sz)
ALLOCATE(t%tstep(t%sz+1), t%tstop(t%sz+1))
DO i=1,t%sz
    CALL R_R('TIMES',t%tstep(i), t%tstop(i))
ENDDO
t%tstep(t%sz+1) = HUGE(1.0)
t%tstop(t%sz+1) = HUGE(1.0)
        IF(diagnostics) WRITE(vp_out,'(50X,A)') 'read times'
        WRITE(vp_out,'(A)') REPEAT('=',2*sp)
        WRITE(vp_out,'(A, I2)') 'TIMES NUMBER', t%number
        DO i=1,t%sz
            WRITE(vp_out,'(2F15.3)') t%tstep(i), t%tstop(i)
        ENDDO
END SUBROUTINE read_time


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE read_list(L)
INTEGER                    :: i, cnt
TYPE(LLIST), INTENT(INOUT) :: L
    IF(diagnostics) WRITE(vp_out,'(50X,A)') 'reading a list'
L%scope = 'all'
L%indx  = no_lists
CALL R_I('list NO AND SIZE',L%number, L%sz)
    WRITE(vp_out,'(A)') REPEAT('=',2*sp)
    WRITE(vp_out,'(A,I2,A,I4,2A)') 'LIST NUMBER ', L%number, '  SIZE:', L%sz, '  SCOPE: ', L%scope
ALLOCATE(L%a(L%sz))
CALL R_I('list', L%sz, L%a)
        IF(diagnostics) WRITE(vp_out,'(50X,A)') 'read list'
        WRITE(vp_out,'(<L%sz>I5)') L%a
cnt = 0
DO i=1,SIZE(L%a)
    IF(L%a(i)<1 .OR. L%a(i)>nel) THEN
        WRITE(vp_out,'(A,I6,A,I6,A)') 'element no ', L%a(i), ' in list ', L%number, ' does not exist'
        cnt = cnt + 1
    ENDIF
ENDDO
IF(cnt>0) CALL ERROR()
END SUBROUTINE read_list

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
TYPE(LLIST) FUNCTION make_list_from_mask(m, txt) RESULT(r)
INTEGER                  :: num
CHARACTER(*), INTENT(IN) :: txt
TYPE(MASK), INTENT(IN)   :: m
r%scope = txt
num     = GET_NUM(txt)
r%sz    = num*COUNT(m%ma) ; ALLOCATE(r%a(r%sz)) ; r%a = 0
CALL LOOPS()
IF(diagnostics) WRITE(vp_out,'(50X,A)') 'creating a '//TRIM(txt)//' list from mask'
CALL SORT(r%sz, r%a)
WRITE(vp_out,'(A,I3,A,i5,2A)') '-----'//' list from mask number',m%number,' size:', r%sz, ' scope: ', r%scope
IF(diagnostics) WRITE(vp_out,'(50X,A)') 'created list'
WRITE(vp_out,'(<20>I5)') r%a

CONTAINS

    !cscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscsc
    SUBROUTINE loops()
    INTEGER :: c, i, j, su
    c = 1
    DO i=m%ilow,m%ihigh
        DO j=m%jlow,m%jhigh
            IF(m%ma(i,j)) THEN  !effective mask
                su = SU_NUMBER(i,j)
                    IF(txt=='squares' .OR. txt=='all') THEN ; r%a(c)=su                              ; c=c+1 ; ENDIF
                    IF(txt=='banks'   .OR. txt=='all') THEN ; r%a(c:c+3) = BANK_NO(su,(/1,2,3,4/))   ; c=c+4 ; ENDIF
                    IF(txt=='rivers'  .OR. txt=='all') THEN ; r%a(c:c+3) = RIVER_NO(su, (/1,2,3,4/)) ; c=c+4 ; ENDIF
            ENDIF
        ENDDO
    ENDDO  
    END SUBROUTINE loops

END FUNCTION make_list_from_mask

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
TYPE(LLIST) FUNCTION make_list_from_list(L, txt) RESULT(r)
INTEGER                  :: num
CHARACTER(*), INTENT(IN) :: txt
TYPE(LLIST), INTENT(IN)  :: L
r%scope = txt
num    = GET_NUM(txt)
r%sz   = L%sz ; ALLOCATE(r%a(r%sz)) ; r%a=0
CALL LISTS()
IF(diagnostics) WRITE(vp_out,'(50X,A)') 'creating a '//TRIM(txt)//' list from list'
CALL SORT(r%sz, r%a)
WRITE(vp_out,'(A,I3,A,I4,2A)') '-----list from list number',L%number,' size:', r%sz, ' scope :', r%scope
IF(diagnostics) WRITE(vp_out,'(50X,A)') 'created list'
WRITE(vp_out,'(<20>I5)') r%a

CONTAINS

    !cscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscsc
    SUBROUTINE lists()
    INTEGER :: c, i, j, su
    LOGICAL :: iss
    c = 1
    DO i=1,L%sz
        su  = L%a(i)
        iss = F
        SELECT CASE(txt)
            CASE('squares') ; IF(IS_SQUARE(su)) iss=T
            CASE('banks')   ; IF(IS_BANK(su))   iss=T
            CASE('rivers')  ; IF(IS_LINK(su))   iss=T
        END SELECT
        IF(iss) THEN ; r%a(c)=su ; c=c+1 ; ENDIF
    ENDDO  
    END SUBROUTINE lists
END FUNCTION make_list_from_list

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION get_num(txt) RESULT(r)
CHARACTER(*), INTENT(IN) :: txt
SELECT CASE(txt)
    CASE('all')     ; r=9
    CASE('squares') ; r=1
    CASE('banks')   ; r=4
    CASE('rivers')  ; r=4
END SELECT
END FUNCTION get_num


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE sort(sza, a)
INTEGER, INTENT(INOUT)             :: sza
INTEGER                            :: i, j, szd
INTEGER, DIMENSION(:), POINTER     :: a
LOGICAL, DIMENSION(:), ALLOCATABLE :: d
szd = MAXVAL(a)
ALLOCATE(d(szd))
d = F
j = 0
DO i=1,sza
    IF(a(i)>0) THEN ; d(a(i)) = T ; j=j+1 ; ENDIF
ENDDO

j = COUNT(d)
IF(j<sza) THEN     !lose and that lie outside catchment
    sza = j
    DEALLOCATE(a)
    ALLOCATE(a(sza))
ENDIF

j = 1
DO i=1,szd
    IF(d(i)) THEN ; a(j)=i ; j=j+1 ; ENDIF
ENDDO

END SUBROUTINE sort

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE read_mask(m, off)
INTEGER                              :: i, j
CHARACTER, DIMENSION(:), INTENT(IN)  :: off
CHARACTER                            :: c
TYPE(MASK), INTENT(INOUT)            :: m
        IF(diagnostics) WRITE(vp_out,'(50X,A)') 'reading a mask'
CALL R_I('number,JLOW,JHIGH,ILOW,IHIGH',m%number, m%jlow, m%jhigh, m%ilow, m%ihigh)
IF(m%jlow>m%jhigh) THEN ; i=m%jlow ; m%jlow=m%jhigh ; m%jhigh=i ; ENDIF
IF(m%ilow>m%ihigh) THEN ; i=m%ilow ; m%ilow=m%ihigh ; m%ihigh=i ; ENDIF
        WRITE(vp_out,'(A)') REPEAT('=',2*sp)
        WRITE(vp_out,'(A,I2)') 'MASK NUMBER ', m%number
        WRITE(vp_out,'(4(A,I3,A,I3))') 'Rows:',m%jlow, ' to ', m%jhigh, '  Columns:', m%ilow, '  to ',m%ihigh
ALLOCATE(m%ma(m%ilow:m%ihigh, m%jlow:m%jhigh))

DO j=m%jlow,m%jhigh
    DO i=m%ilow,m%ihigh
        CALL R_C('mask element', c)
        m%ma(i,j) = ALL(c/=off)
    ENDDO
ENDDO

IF(diagnostics) WRITE(vp_out,'(50X,A)') 'mask read'

CALL MASK_WRITE('mask as read', 'T', 'F')

DO j=m%jlow,m%jhigh
    DO i=m%ilow,m%ihigh
        m%ma(i,j) = m%ma(i,j) .AND. EXISTS(SU_NUMBER(i,j))  !effective mask
    ENDDO
ENDDO
CALL MASK_WRITE('effective mask', 'T', '.')

CONTAINS

    !cscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscsc
    SUBROUTINE mask_write(txt, tr, fa)
    CHARACTER(*), INTENT(IN) :: txt
    CHARACTER, INTENT(IN) :: tr, fa
    CHARACTER, DIMENSION(SIZE(m%ma,DIM=1),SIZE(m%ma,DIM=2)) :: cc
    WRITE(vp_out,'(50X,A)') txt   
    WHERE(m%ma) ; cc=tr ; ELSEWHERE ; cc = fa ; ENDWHERE
    WRITE(vp_out, '(<SIZE(cc,DIM=1)>A)') cc
    END SUBROUTINE mask_write
END SUBROUTINE read_mask


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE INCREMENT_item(s,n)
TYPE(ITEM), DIMENSION(:), POINTER :: s,old=>NULL()
INCLUDE 'include_increment.f90'
no_items   = no_items + 1
END SUBROUTINE INCREMENT_item

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE INCREMENT_LIST(s,n)
TYPE(LLIST), DIMENSION(:), POINTER :: s,old=>NULL()
INCLUDE 'include_increment.f90'
no_lists   = no_lists + 1
END SUBROUTINE INCREMENT_LIST
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE INCREMENT_MASK(s,n)
TYPE(MASK), DIMENSION(:), POINTER :: s,old=>NULL()
INCLUDE 'include_increment.f90'
no_masks  = no_masks + 1
END SUBROUTINE INCREMENT_MASK
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE INCREMENT_TIME(s,n)
TYPE(TTIME), DIMENSION(:), POINTER :: s,old=>NULL()
INCLUDE 'include_increment.f90'
no_times  = no_times + 1
END SUBROUTINE INCREMENT_TIME

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION no_extra_dimensions(e_d) RESULT(r)
CHARACTER(*), INTENT(IN) :: e_d
SELECT CASE(e_d)
CASE('-')        ; r = 1
CASE('faces')       ; r = 4
CASE('left_right')  ; r = 2
CASE('X_Y')         ; r = 2
END SELECT
END FUNCTION no_extra_dimensions

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION names_of_extra_dimensions(n,e_d) RESULT(r)
INTEGER, INTENT(IN)        :: n
CHARACTER(*), INTENT(IN)   :: e_d
CHARACTER(6), DIMENSION(n) :: r
SELECT CASE(e_d)
CASE('-')        ; r = ''
CASE('faces')       ; r = (/'North', 'East', 'South', 'West'/)
CASE('left_right')  ; r = (/'left','right'/)
CASE('X_Y')         ; r = (/'x','y'/)
END SELECT
END FUNCTION names_of_extra_dimensions

END MODULE visualisation_metadata
