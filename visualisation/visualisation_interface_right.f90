!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_interface_right
!DEC$ REAL:4
!JE for SHEGRAPH Version 2.0 Created July 2004
USE VISUALISATION_INTERFACE_CENTRE,    ONLY : BANK_NO, ELEMENT, GRID_NX, GRID_NY, RIVER_NO, TOP_CELL, &
                                              IS_SQUARE, IS_BANK, IS_LINK,                            &
                                              north, east, south, west, EXISTS, NO_EL, csz, DIRQQ,    &
                                              SHETRAN_INTEGER_DATA, SHETRAN_REAL_DATA, OUTPUT_TYPE, GET_OUTPUT_TYPE, &
                                              NO_SED, NO_CON, VERSION, ROOTDIR, SHETRAN_LAYER,                       &
                                              hdf5filename, planfile, checkfile
USE VISUALISATION_INTERFACE_FAR_RIGHT, ONLY : G_C, G_L, G_I, S_I, G_I_F,                              &
                                              TIME_TO_RECORD,                                         &
                                              REGISTER_STATIC_VISUALISATION_METADATA,                 &
                                              REGISTER_DYNAMIC_VISUALISATION_METADATA,                &
                                              FOR_NEW_TIME, SAVE_ITEMS_WORTH,                         &
                                              SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP, &
                                              SEND_P

IMPLICIT NONE

INTEGER, DIMENSION(4), PARAMETER :: north_order = (/north, east, south, west/), & ! new face numbering
                                    normal_order = (/1,2,3,4/)
LOGICAL, PARAMETER    :: T=.TRUE., F=.FALSE.

REAL, PARAMETER :: zero=0.0

PRIVATE
PUBLIC :: RECORD_VISUALISATION_DATA, north_order

CONTAINS


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE record_visualisation_data(time, text)
INTEGER                                  :: i, j, jj, k, mn, nn, su, ilow, ihigh, jlow, jhigh, klow, khigh, sz, ext, nsed, ncon, n
INTEGER(INT_PTR_KIND())                               :: first, latest
LOGICAL                                  :: isgrid
REAL, INTENT(IN)                         :: time
INTEGER, DIMENSION(4)                    :: ee
CHARACTER(*), INTENT(IN), OPTIONAL       :: text
CHARACTER(2)                             :: typ
CHARACTER(8)                             :: ext_dim
CHARACTER(csz)                           :: name
LOGICAL, SAVE                            :: one=T, two=F
TYPE(OUTPUT_TYPE), DIMENSION(:), POINTER :: oty
IF(one) THEN
    one = F
    two = T
    CALL SEND_PASS(1)
    CALL SAVE_VISUALISATION_DATA_TO_DISK(1, 0.0)
    RETURN
ELSEIF(two) THEN
    two = F
    CALL SEND_PASS(2)
    oty         => GET_OUTPUT_TYPE('static')
    DO i=LBOUND(oty,DIM=1),UBOUND(oty,DIM=1)
        CALL REGISTER_STATIC_VISUALISATION_METADATA(oty(i)%name, oty(i)%typ, &
            oty(i)%units, oty(i)%title, GRID_NX(), GRID_NY(), oty(i)%extra_dimensions, oty(i)%varies_with_elevation)
    ENDDO
    DEALLOCATE(oty)
    oty     => GET_OUTPUT_TYPE('dynamic')
    DO k=1,2
        DO i=LBOUND(oty,DIM=1),UBOUND(oty,DIM=1)
            CALL REGISTER_DYNAMIC_VISUALISATION_METADATA(k, i==SIZE(oty).AND.k==2, oty(i)%name, oty(i)%typ, &
             oty(i)%units, oty(i)%title, oty(i)%extra_dimensions, oty(i)%varies_with_elevation,             &
             oty(i)%varies_with_sediment_no, oty(i)%varies_with_contaminant_no,         &
             oty(i)%implemented)
        ENDDO
    ENDDO
    DEALLOCATE(oty)
ENDIF

MM: DO mn=1,G_I(0,'no_items')
    
    IF(.NOT.TIME_TO_RECORD(mn,time)) CYCLE MM
    name   = G_C(mn,'name')
    typ    = G_C(mn,'typ')
    ilow   = G_I(mn,'ilow')
    ihigh  = G_I(mn,'ihigh')
    jlow   = G_I(mn,'jlow')
    jhigh  = G_I(mn,'jhigh')
    klow   = G_I(mn,'klow')
    khigh  = G_I(mn,'khigh')
    isgrid = G_L(mn,'isgrid')
    ext    = G_I(mn,'ext')
    IF(ext>0) THEN
        ext_dim = G_C(mn,'extra_dimensions')
        IF(ext_dim=='faces') THEN
            ee = north_order
        ELSE
            ee = normal_order
        ENDIF
    ENDIF
    first  = G_I_F(mn,'first')
    latest = G_I_F(mn,'latest')
    nsed   = G_I(mn,'nsed')
    ncon   = G_I(mn,'ncon')
    CALL FOR_NEW_TIME(typ, time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
    CALL S_I(mn,'first', first)
    CALL S_I(mn,'latest', latest)
    IF(.NOT.isgrid) THEN
        sz = G_I(mn,'sz')
        DO nn=1,sz
            su = G_I(mn,'su',nn)
            IF(su==0) CYCLE  !not a subunit _ so leave values at defaults
            CALL FILL_SELECT(name, typ, nn, 1, 1, su, klow, khigh, SHETRAN_LAYER((/(n,n=klow,khigh)/)), ee(1:ext), latest, nsed=nsed, ncon=ncon)
        ENDDO
    ELSE
        DO i=ilow,ihigh
            DO j=jlow,jhigh
                jj = SHETRAN_J(j)  !SHETRAN grid is upside down
                IF(.NOT.G_L(mn,'on', i, j)) CYCLE
                su = SU_NUMBER(i,j)
                IF(su==0) CYCLE  !not a subunit _ so leave values at defaults
            CALL FILL_SELECT(name, typ, i, j, jj, su, klow, khigh, SHETRAN_LAYER((/(n,n=klow,khigh)/)), ee(1:ext), latest, nsed=nsed, ncon=ncon)
            ENDDO
        ENDDO
    ENDIF
!            call dump(name, typ, mn, time, first, isgrid)
    CALL SAVE_VISUALISATION_DATA_TO_DISK(mn, time)
ENDDO MM

IF(PRESENT(text)) THEN
    IF(text=='end') CALL VISUALISATION_TIDY_UP()
ENDIF
END SUBROUTINE record_visualisation_data

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE fill_select(name, typ, a, b, bb, su, klow, khigh, silay, ee, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
CHARACTER(*), INTENT(IN)          :: name, typ
SELECT CASE(typ)
    CASE('BS') ; CALL FILL_B(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
    CASE('ES') ; CALL FILL_E(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
    CASE('FS') ; CALL FILL_F(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
    CASE('GS') ; CALL FILL_G(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
    CASE('IS') ; CALL FILL_I(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
    CASE('LS') ; CALL FILL_L(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
    CASE('MS') ; CALL FILL_M(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
    CASE('NS') ; CALL FILL_N(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
END SELECT
END SUBROUTINE fill_select


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE fill_b(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
INTEGER                           :: d, e, banks(4)
CHARACTER(*), INTENT(IN)          :: name, typ
banks  = BANK_NO(su,north_order)
DO d=1,4
    IF(EXISTS(banks(d))) THEN
        DO e=1,SIZE(ee)
              CALL SAVE_ITEMS_WORTH('e', typ, a, b, klow, khigh, e, d, &
               SHETRAN_REAL_DATA(name, banks(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
        ENDDO
    ENDIF
ENDDO
END SUBROUTINE fill_b


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE fill_e(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
INTEGER                           :: d, e, banks(4)
CHARACTER(*), INTENT(IN)          :: name, typ
banks  = BANK_NO(su,north_order)
DO d=1,4
    IF(EXISTS(banks(d))) THEN
        DO e=1,SIZE(ee)
              CALL SAVE_ITEMS_WORTH('e', typ, a, b, klow, khigh, e, d, &
              SHETRAN_INTEGER_DATA(name, banks(d), ix=a, iy=bb, ilay=silay, ext=ee(e)), latest)
        ENDDO
    ENDIF
ENDDO
END SUBROUTINE fill_e

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE fill_f(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
INTEGER                           :: d, e, rivers(4)
CHARACTER(*), INTENT(IN)          :: name, typ
rivers = RIVER_NO(su, north_order)
DO d=1,4
    IF(EXISTS(rivers(d))) THEN
        DO e=1,SIZE(ee)
              CALL SAVE_ITEMS_WORTH('e', typ, a, b, klow, khigh, e, d, &
              SHETRAN_INTEGER_DATA(name, rivers(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
        ENDDO
    ENDIF
ENDDO
END SUBROUTINE fill_f

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE  fill_g(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
INTEGER                           :: d, e, banks(4), rivers(4)
CHARACTER(*), INTENT(IN)          :: name, typ
DO e=1,SIZE(ee)
    CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
    SHETRAN_REAL_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
ENDDO
rivers  = RIVER_NO(su, north_order)
banks   = BANK_NO(su,north_order)
DO d=1,4  
    IF(EXISTS(banks(d))) THEN
        DO e=1,SIZE(ee)
            CALL SAVE_ITEMS_WORTH('b', typ, a, b, klow, khigh, e, d, &
            SHETRAN_REAL_DATA(name, banks(d),  ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
        ENDDO
    ENDIF
    IF(EXISTS(rivers(d))) THEN
        DO e=1,SIZE(ee)
              CALL SAVE_ITEMS_WORTH('r', typ, a, b, klow, khigh, e, d, &
              SHETRAN_REAL_DATA(name, rivers(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
        ENDDO
    ENDIF
ENDDO
END SUBROUTINE fill_g

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE  fill_i(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
INTEGER                           :: d, e, n
CHARACTER(*), INTENT(IN)          :: name, typ
DO e=1,SIZE(ee)
    CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
    SHETRAN_INTEGER_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
ENDDO
END SUBROUTINE fill_i

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE fill_L(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
INTEGER                           :: d, e, rivers(4)
CHARACTER(*), INTENT(IN)          :: name, typ
rivers = RIVER_NO(su, north_order)
DO d=1,4
    IF(EXISTS(rivers(d))) THEN
        DO e=1,SIZE(ee)
              CALL SAVE_ITEMS_WORTH('e', typ, a, b, klow, khigh, e, d, &
              SHETRAN_REAL_DATA(name, rivers(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
        ENDDO
    ENDIF
ENDDO
END SUBROUTINE fill_L

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE  fill_m(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
INTEGER                           :: d, e, n
CHARACTER(*), INTENT(IN)          :: name, typ
DO e=1,SIZE(ee)
    CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
    SHETRAN_REAL_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
ENDDO
END SUBROUTINE fill_m

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE  fill_n(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
INTEGER, INTENT(IN)               :: a, b, bb, su, klow, khigh, nsed, ncon
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(:), INTENT(IN) :: ee, silay
INTEGER                           :: d, e, banks(4), rivers(4)
CHARACTER(*), INTENT(IN)          :: name, typ
DO e=1,SIZE(ee)
    CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
            SHETRAN_INTEGER_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
ENDDO
rivers  = RIVER_NO(su, north_order)
banks   = BANK_NO(su,north_order)
DO d=1,4  
    IF(EXISTS(banks(d))) THEN
        DO e=1,SIZE(ee)
            CALL SAVE_ITEMS_WORTH('b', typ, a, b, klow, khigh, e, d, &
            SHETRAN_INTEGER_DATA(name, banks(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
        ENDDO
    ENDIF
    IF(EXISTS(rivers(d))) THEN
        DO e=1,SIZE(ee)
              CALL SAVE_ITEMS_WORTH('r', typ, a, b, klow, khigh, e, d, &
              SHETRAN_INTEGER_DATA(name, rivers(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
        ENDDO
    ENDIF
ENDDO
END SUBROUTINE fill_n

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE send_pass(jj)
INTEGER                              :: i, j, nx, ny, total_no_elements
INTEGER, INTENT(IN)                  :: jj
INTEGER, DIMENSION(:), ALLOCATABLE   :: iel
INTEGER, DIMENSION(:,:), ALLOCATABLE :: dum

SELECT CASE(jj)
CASE(1)
    CALL SEND_P('dirqq',     cc=dirqq, da=0, db=0)
    CALL SEND_P('rootdir',   cc=rootdir, da=0, db=0)
    CALL SEND_p('ver',       ii=VERSION(), da=0, db=0)
    CALL SEND_p('hdf5fname', cc=hdf5filename, da=0, db=0)
    CALL SEND_p('planfile',  cc=planfile, da=0, db=0)
    CALL SEND_p('checkfile', cc=checkfile, da=0, db=0)
CASE(2)
    total_no_elements = NO_EL()
    ALLOCATE(iel(total_no_elements)) ; iel = (/(i,i=1,total_no_elements)/)
    nx  = GRID_NX()
    ny  = GRID_NY()
    CALL SEND_P('north',     ii=north, da=0, db=0)
    CALL SEND_P('east',      ii=east, da=0, db=0)
    CALL SEND_P('south',     ii=south, da=0, db=0)
    CALL SEND_P('west',      ii=west, da=0, db=0)
    CALL SEND_P('grid_nx',   ii=nx, da=0, db=0)
    CALL SEND_P('grid_ny',   ii=ny, da=0, db=0)
    CALL SEND_P('top_cell',  ii=TOP_CELL(), da=0, db=0)
    CALL SEND_P('nel',  ii=total_no_elements, da=0, db=0)
    CALL SEND_P('nsed',      ii=NO_SED(), da=0, db=0)
    CALL SEND_P('ncon',      ii=NO_CON(), da=0, db=0)
    CALL SEND_P('is_square', L1=IS_SQUARE(iel), da=total_no_elements, db=0)
    CALL SEND_P('is_bank',   L1=IS_BANK(iel), da=total_no_elements, db=0)
    CALL SEND_P('is_link',   L1=IS_LINK(iel), da=total_no_elements, db=0)
    ALLOCATE(dum(nx,ny))
            DO i=1,nx ; dum(i,:) = SU_NUMBER(i,(/(j,j=1,ny)/))
            ENDDO
    CALL SEND_P('su', d2=dum, da=nx, db=ny)
    DEALLOCATE(dum)
    ALLOCATE(dum(total_no_elements,4))
    DO j=1,4
        WHERE(IS_SQUARE(iel)) ; dum(:,j)=BANK_NO(iel,j) ; ELSEWHERE ; dum(:,j)=0 ; ENDWHERE
    ENDDO
    CALL SEND_P('bank_no', d2=dum, da=total_no_elements, db=4)
    DO j=1,4
        WHERE(IS_SQUARE(iel)) ; dum(:,j)=RIVER_NO(iel,j) ; ELSEWHERE ; dum(:,j)=0 ; ENDWHERE
    ENDDO
    CALL SEND_P('river_no', d2=dum, da=total_no_elements, db=4)
    DEALLOCATE(dum)
END SELECT
END SUBROUTINE send_pass

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION su_number(i,j) RESULT(r)
INTEGER, INTENT(IN) :: i,j  !HDF5 indices
r = ELEMENT(i,SHETRAN_J(j))  !SHETRAN grid is upside down
END FUNCTION su_number

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
ELEMENTAL INTEGER FUNCTION shetran_j(sgv2j) RESULT(r) !grid y coordinate
INTEGER, INTENT(IN) :: sgv2j
r = GRID_NY() - sgv2j + 1
END FUNCTION shetran_j

END MODULE visualisation_interface_right