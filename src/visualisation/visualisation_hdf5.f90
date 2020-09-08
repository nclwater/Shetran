!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_hdf5
!DEC$ REAL:4
USE VISUALISATION_PASS,      ONLY : DIRQQ, ver, rootdir, hdf5filename
!DEC$ DEFINE ISKEY=0
!DEC$ IF(ISKEY==1)
    USE VISUALISATION_KEY,       ONLY : KTEST
!DEC$ ENDIF
USE VISUALISATION_METADATA,  ONLY : G_C=>GET_METADATA_C, G_L=>GET_METADATA_L, &
                                    G_I=>GET_METADATA_I, S_I=>SET_METADATA_I, &
                                    G_I_F=>GET_METADATA_I_FIRST,              &
                                    ndim,                                     &
                                    G_H5_I=>GET_METADATA_HDF5_I, G_H5_L=>GET_METADATA_HDF5_L, &
                                    G_H5_C=>GET_METADATA_HDF5_C, INCREMENT_HDF5_TSTEP_NO
USE VISUALISATION_STRUCTURE, ONLY : TIME_COUNT, GET_HDF5_I, GET_HDF5_R, GET_HDF5_TIME
USE VISUALISATION_MAP,       ONLY : GET_REAL_IMAGE_INDEX, GET_MAGNIFIED_SU_ARR
!USE HDF5,                    ONLY : H5OPEN_F,         &
!                                    H5PSET_DEFLATE_F, & 
!                                    H5SCOPY_F,        &
!                                    H5PSET_CHUNK_F,   &
!                                    H5TCOPY_F,        &
!                                    H5TSET_SIZE_F,    &
!                                    H5AWRITE_F,       &  
!                                    H5DWRITE_F,       &
!                                    H5DEXTEND_F,      &
!                                    H5SSELECT_HYPERSLAB_F, &
!                                    H5TCOPY_F,        &
!                                    H5TSET_SIZE_F,    &
!                                    H5SCREATE_SIMPLE_F, &
!                                    H5PCREATE_F,        &
!                                    H5FCREATE_F,        &
!                                    H5GCREATE_F,        &
!                                    H5ACREATE_F,        &
!                                    H5DCREATE_F,        &      
!                                    H5ACLOSE_F, &
!                                    H5DCLOSE_F, &       
!                                    H5SCLOSE_F, &
!                                    H5GCLOSE_F, &
!                                    H5FCLOSE_F, &
!                                    H5CLOSE_F,  &
!                                    HSIZE_T, HID_T

USE HDF5
USE H5IM
USE H5LT

                                    
IMPLICIT NONE

INTEGER                 :: error  !Error flag
INTEGER, SAVE           :: jndim(ndim)                      
INTEGER, PARAMETER      :: csz=70
REAL, PARAMETER         :: zero=0.0
LOGICAL, PARAMETER      :: T=.TRUE., F=.FALSE.


!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE ssz
    INTEGER(HSIZE_T), DIMENSION(:), POINTER :: a
END TYPE ssz
TYPE(ssz), DIMENSION(:), ALLOCATABLE, SAVE  :: szz, newsz  !recording array size, and its size after extension

INTEGER(HID_T), DIMENSION(:), ALLOCATABLE   :: dataset, dataspace, dtype, orig_dataspace, t_dataspace, t_dataset
INTEGER(HSIZE_T)                            :: t_newsz(1)
!INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: rank
INTEGER, DIMENSION(:), ALLOCATABLE :: rank
INTEGER(HID_T)                              :: orig_t_dataspace, group_static, group_dynamic, group_images, file, &
                                               group_magnified_integer
INTEGER(HID_T), SAVE                        :: dataset_compress_property, t_dataset_compress_property

PRIVATE
PUBLIC :: SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP

CONTAINS

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE initialise()
INTEGER                  :: ni, mn, jj
INTEGER, DIMENSION(ndim) :: hhdim
LOGICAL                  :: istimeseries
CHARACTER(csz)           :: name, namet
INTEGER(HID_T)           :: gp
INTEGER(HID_T), DIMENSION(:), ALLOCATABLE, SAVE   :: gp_var
INTEGER(HSIZE_T), DIMENSION(ndim)                 :: maxdims
INTEGER(HSIZE_T), PARAMETER                       :: one=1

jndim = (/(jj,jj=1,ndim)/)
ni    = G_I(0,'no_items')
ALLOCATE(dataset(ni), dataspace(ni), orig_dataspace(ni), dtype(ni),szz(ni), &
         newsz(ni), gp_var(ni), t_dataspace(ni), t_dataset(ni), rank(ni))

CALL H5OPEN_F(error)
!lined below needed only for compound datatypes
!CALL H5PCREATE_F(H5P_DATASET_XFER_F, dataset_transfer_property, error)
!CALL H5PSET_PRESERVE_F(dataset_transfer_property, .TRUE., error)
CALL H5PCREATE_F(H5P_DATASET_CREATE_F, dataset_compress_property, error)
CALL H5PCREATE_F(H5P_DATASET_CREATE_F, t_dataset_compress_property, error)
CALL H5PSET_DEFLATE_F(dataset_compress_property, 9, error)
CALL H5PSET_DEFLATE_F(t_dataset_compress_property, 9, error)

!CALL H5FCREATE_F(TRIM(DIRQQ)//'/'//'output/sssshegraph.h5', H5F_ACC_TRUNC_F, file, error)
 CALL H5FCREATE_F(TRIM(hdf5filename), H5F_ACC_TRUNC_F, file, error)

CALL H5GCREATE_F(file, 'CONSTANTS', group_static, error)
CALL H5GCREATE_F(file, 'VARIABLES', group_dynamic, error)

DO mn=1,ni
    hhdim = G_H5_I(mn, 'dimensions', jndim)
    rank(mn) = COUNT(hhdim/=0)
    ALLOCATE(szz(mn)%a(rank(mn)), newsz(mn)%a(rank(mn)))
    szz(mn)%a = PACK(hhdim, hhdim>0)
    
    maxdims(2:rank(mn)) = szz(mn)%a(2:rank(mn))  !fixed dimensions
    istimeseries = G_H5_L(mn, 'istimeseries')
    IF(istimeseries) THEN
        maxdims(1) = H5S_UNLIMITED_F
        namet      = 'time'
!        WRITE(name,'(I3)')G_H5_I(mn,'users_number')
!        name  = TRIM(name)//' '//TRIM(G_H5_C(mn,'name'))
        name = COMBINATION_NAME(mn)
        CALL H5GCREATE_F(group_dynamic, name, gp_var(mn), error)
        WRITE(name,'(I3)')G_H5_I(mn,'users_number')
        name  = 'value'
        gp    = gp_var(mn)
    ELSE
        maxdims(1) = szz(mn)%a(1)
        name       = TRIM(G_H5_C(mn,'name'))
        gp         = group_static
    ENDIF

    CALL H5SCREATE_SIMPLE_F(rank(mn), szz(mn)%a, orig_dataspace(mn), error, maxdims=maxdims(1:rank(mn)))
    CALL H5SCOPY_F(orig_dataspace(mn),dataspace(mn), error)

    CALL H5SCREATE_SIMPLE_F(1, (/one/), orig_t_dataspace, error, maxdims=maxdims(1:rank(mn)))
    CALL H5SCOPY_F(orig_t_dataspace, t_dataspace(mn), error)

    CALL H5PSET_CHUNK_F(dataset_compress_property, rank(mn), szz(mn)%a, error)
    CALL H5PSET_CHUNK_F(t_dataset_compress_property, 1, (/one/), error)

    IF(G_H5_L(mn,'isreal')) THEN ; dtype(mn)=H5T_NATIVE_REAL ; ELSE ; dtype(mn)=H5T_NATIVE_INTEGER ; ENDIF

    !CALL H5DCREATE_F(gp, name, dtype(mn), dataspace(mn), dataset(mn), error, creation_prp=dataset_compress_property)
CALL H5DCREATE_F(gp, name, dtype(mn), dataspace(mn), dataset(mn), error, dcpl_id=dataset_compress_property)  !160913

    CALL CREATE_VARIABLES_ATTRIBUTES(mn)

    IF(istimeseries) THEN
        !CALL H5DCREATE_F(gp, namet, H5T_NATIVE_REAL, t_dataspace(mn), t_dataset(mn), error, creation_prp=t_dataset_compress_property)
        CALL H5DCREATE_F(gp, namet, H5T_NATIVE_REAL, t_dataspace(mn), t_dataset(mn), error, dcpl_id=t_dataset_compress_property)
        CALL CREATE_TIME_ATTRIBUTES(mn)
    ENDIF

ENDDO

END SUBROUTINE initialise


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
CHARACTER(12) FUNCTION combination_name(mn) RESULT(r)
INTEGER, INTENT(IN) :: mn
CHARACTER(8)        :: dum
WRITE(r,'(I3)')G_H5_I(mn,'users_number')
dum = G_H5_C(mn,'name')
IF(G_H5_L(mn,'varies_with_sediment')) THEN
    WRITE(dum,'(A,I2)') TRIM(dum), G_H5_I(mn,'nsed')
ELSEIF(G_H5_L(mn,'varies_with_contaminant')) THEN
        WRITE(dum,'(A,I2)') TRIM(dum), G_H5_I(mn,'ncon')
ENDIF
r  = TRIM(r)//' '//TRIM(dum)
END FUNCTION combination_name

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE visualisation_tidy_up()
!DEC$ ATTRIBUTES DLLEXPORT :: visualisation_tidy_up
INTEGER :: ni, mn
LOGICAL :: istimeseries
ni           = G_I(0,'no_items')
DO mn=1,ni
    istimeseries = G_H5_L(mn, 'istimeseries')
    CALL H5DCLOSE_F(dataset(mn), error)
    IF(istimeseries) CALL H5DCLOSE_F(t_dataset(mn), error)
    CALL H5SCLOSE_F(dataspace(mn), error)
    CALL H5SCLOSE_F(orig_dataspace(mn), error)
ENDDO
CALL H5GCLOSE_F(group_static, error)
CALL H5GCLOSE_F(group_dynamic, error)
CALL H5GCLOSE_F(group_images, error)
CALL H5GCLOSE_F(group_magnified_integer, error)
CALL H5FCLOSE_F(file, error)
CALL H5CLOSE_F(error)
END SUBROUTINE visualisation_tidy_up


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE save_visualisation_data_to_disk(mn, time)
!DEC$ ATTRIBUTES DLLEXPORT :: save_visualisation_data_to_disk
INTEGER, INTENT(IN) :: mn
INTEGER, PARAMETER  :: buffer_length_for_storage=1
INTEGER             :: tc, tstep
REAL, INTENT(IN)    :: time
LOGICAL, SAVE       :: one=T, two=F, notflag=F

IF(notflag .AND. time>zero) THEN
    RETURN
ELSEIF(one) THEN
    one = F
    two = T
    !DEC$ IF(ISKEY==1)
        notflag = KTEST(ver, rootdir)
    !DEC$ ELSE
        notflag = F
    !DEC$ ENDIF
    RETURN
ELSEIF(two) THEN
    two = F
    CALL INITIALISE()
ENDIF
IF(time/=zero .AND. .NOT.G_L(mn, 'istimeseries')) RETURN !statics only saved at time=0
IF(time==zero) THEN
    tc = 1
ELSE
    CALL INCREMENT_HDF5_TSTEP_NO(mn)
    tstep          = G_H5_I(mn, 'tstep_no')
    newsz(mn)%a    = szz(mn)%a ; newsz(mn)%a(1) = tstep !hh%tstep_no
    t_newsz        = (/tstep/) !(/hh%tstep_no/)
    CALL H5DEXTEND_F(dataset(mn), newsz(mn)%a, error)
    CALL H5DEXTEND_F(t_dataset(mn), t_newsz, error)
    tc = TIME_COUNT(G_C(mn,'typ'),G_I_F(mn,'first'))
ENDIF
IF(time==zero .OR. tc==buffer_length_for_storage) &
        CALL WRITE_MN(mn, tc, time==zero, tstep, G_H5_L(mn,'isreal'), G_H5_I(mn,'szorder',jndim), G_H5_I(mn,'ilow'), G_H5_I(mn,'jlow'), G_H5_I(mn,'klow'))
!IF(mn==G_I(0,'no_items')) PRINT*,time !, 'RECODE HERE TO IMPROVE OUTPUT'

END SUBROUTINE save_visualisation_data_to_disk



!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE write_mn(mn, amount, firstwrites, tstep, isreal, szorder, ilow, jlow, klow)
INTEGER, INTENT(IN)                                :: mn, tstep, ilow, jlow, klow, amount !how many to copy to disk
INTEGER, DIMENSION(:), INTENT(IN)                  :: szorder
INTEGER                                            :: am, hhdim(ndim)
INTEGER(INT_PTR_KIND())                    :: first
INTEGER, DIMENSION(ndim)                           :: sz
INTEGER(HSIZE_T)                                   :: t_sz(7)
REAL                                               :: time
REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE          :: surf_elv
LOGICAL, INTENT(IN)                                :: firstwrites, isreal
LOGICAL                                            :: istimeseries
CHARACTER(2)                                       :: typ
CHARACTER(csz)                                     :: name
INTEGER(HID_T)                                     :: filespace, t_filespace
INTEGER(HSIZE_T), DIMENSION(ndim)                  :: start, t_start, ccount, t_ccount

name            = G_H5_C(mn,'name')
first           = G_I_F(mn,'first')
typ             = G_C(mn,'typ')
istimeseries    = G_L(mn,'istimeseries')
hhdim = G_H5_I(mn, 'dimensions', jndim)
sz    = MAX(1,hhdim)

IF(firstwrites) THEN
    CALL H5SCOPY_F(dataspace(mn), filespace, error)
    CALL H5SCOPY_F(t_dataspace(mn), t_filespace, error)
ELSE
    CALL H5SCREATE_SIMPLE_F(rank(mn), newsz(mn)%a,   filespace,   error) !create dataspacesv4_elevation
    CALL H5SCREATE_SIMPLE_F(1,    t_newsz, t_filespace, error) !create dataspace
ENDIF

start    = 0
t_start  = 0
ccount   = 1
t_ccount = 1
t_sz     = 0
t_sz(1)  = 1
DO am=1,amount
    IF(.NOT.firstwrites) THEN
        start(1) = tstep-amount+am-1
        CALL H5SSELECT_HYPERSLAB_F(filespace, H5S_SELECT_SET_F, start(1:rank(mn)), ccount(1:rank(mn)), error, block=szz(mn)%a)
        !for time data
        t_start(1) = tstep-amount+am-1
        CALL H5SSELECT_HYPERSLAB_F(t_filespace, H5S_SELECT_SET_F, t_start, t_ccount, error)   
    ENDIF

    IF(istimeseries) THEN
        time = GET_HDF5_TIME(typ, first)
        CALL H5DWRITE_F(t_dataset(mn), H5T_NATIVE_REAL, (/time/), &
                       t_sz, error, mem_space_id=orig_t_dataspace, file_space_id=t_filespace)
    ENDIF
        
    !NB *** first is updated in this loop   
    IF(isreal) THEN
        IF(name=='surf_elv') THEN
            ALLOCATE(surf_elv(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)))
            surf_elv = GET_HDF5_R(typ, sz, szorder, first, ilow, jlow, klow)
            CALL H5DWRITE_F(dataset(mn), dtype(mn), surf_elv, &
                    szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)
        ELSE
            CALL H5DWRITE_F(dataset(mn), dtype(mn), GET_HDF5_R(typ, sz, szorder, first, ilow, jlow, klow), &
                    szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)
        ENDIF
    ELSE
        CALL H5DWRITE_F(dataset(mn), dtype(mn), GET_HDF5_I(typ, sz, szorder, first, ilow, jlow, klow), &
                        szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)  !write to file
    ENDIF
ENDDO
CALL S_I(mn,'first', first)
CALL H5SCLOSE_F(filespace, error)
CALL H5SCLOSE_F(t_filespace, error)
IF(name=='number') CALL SAVE_NUMBERS_AS_SPREADSHEET(mn)
IF(name=='surf_elv') THEN
    CALL SAVE_SURF_ELEV_AS_MAP(mn, surf_elv(1,1,1,:,:,:), magnif=20)
    DEALLOCATE(surf_elv)
ENDIF
END SUBROUTINE write_mn

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE create_time_attributes(mn)
INTEGER, INTENT(IN)                     :: mn
!INTEGER(HSIZE_T)                        :: arank
INTEGER                        :: arank
INTEGER(HSIZE_T), DIMENSION(7)          :: tsz  !don't know why this could not be set at size 1 - compilation problem
INTEGER(HID_T)                          :: atype
INTEGER(HID_T)                          :: attribute, a_dataspace
!units
    CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
    CALL H5TSET_SIZE_F(atype, 5, error)
    arank  = 1
    tsz    = 0
    tsz(1) = 1
    CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
    CALL H5ACREATE_F(t_dataset(mn), 'units', atype, a_dataspace, attribute, error)
    CALL H5AWRITE_F(attribute, atype, (/'hours'/), tsz, error)
    CALL H5ACLOSE_F(attribute, error)
    CALL H5SCLOSE_F(a_dataspace, error)
END SUBROUTINE create_time_attributes

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE create_variables_attributes(mn)
INTEGER, INTENT(IN)                     :: mn
INTEGER                                 :: dd, ii, jj, no_dimensions
!INTEGER(HSIZE_T)                        :: arank
INTEGER                        :: arank
INTEGER(HSIZE_T), DIMENSION(7)          :: tsz  !don't know why this could not be set at size 1 - compilation problem
INTEGER(HID_T)                          :: atype
INTEGER(HID_T)                          :: attribute, a_dataspace
INTEGER                                 :: i
INTEGER, DIMENSION(:,:), ALLOCATABLE    :: pairs
CHARACTER(2)                            :: typ
CHARACTER(6), DIMENSION(:), ALLOCATABLE :: nme, nmed
!title
    CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
    CALL H5TSET_SIZE_F(atype, csz, error)
    arank  = 1
    tsz    = 0
    tsz(1) = 1
    CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
    CALL H5ACREATE_F(dataset(mn), 'title', atype, a_dataspace, attribute, error)
    CALL H5AWRITE_F(attribute, atype, G_H5_C(mn,'title'), tsz, error)
    CALL H5ACLOSE_F(attribute, error)
    CALL H5SCLOSE_F(a_dataspace, error)
!units
    CALL H5TSET_SIZE_F(atype, 8, error)
    tsz(1) = 1
    CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
    CALL H5ACREATE_F(dataset(mn), 'units', atype, a_dataspace, attribute, error)
    CALL H5AWRITE_F(attribute, atype, G_H5_C(mn,'units'), tsz, error)
    CALL H5ACLOSE_F(attribute, error)
    CALL H5SCLOSE_F(a_dataspace, error)
!basis
    CALL H5TSET_SIZE_F(atype, 12, error)
    tsz(1) = 1
    CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
    CALL H5ACREATE_F(dataset(mn), 'basis', atype, a_dataspace, attribute, error)
    CALL H5AWRITE_F(attribute, atype, G_H5_C(mn,'basis'), tsz, error)
    CALL H5ACLOSE_F(attribute, error)
    CALL H5SCLOSE_F(a_dataspace, error)
!scope
    CALL H5TSET_SIZE_F(atype, 7, error)
    tsz(1) = 1
    CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
    CALL H5ACREATE_F(dataset(mn), 'scope', atype, a_dataspace, attribute, error)
    CALL H5AWRITE_F(attribute, atype, G_H5_C(mn,'scope'), tsz, error)
    CALL H5ACLOSE_F(attribute, error)
    CALL H5SCLOSE_F(a_dataspace, error)
!names of dimensions
    CALL H5TSET_SIZE_F(atype, 6, error)
    no_dimensions = G_H5_I(mn,'no_dimensions')
    tsz(1)        = no_dimensions
    ALLOCATE(nmed(tsz(1)))
    ii = 0
    DO jj=1,ndim
        IF(G_H5_I(mn,'dimensions',jj)/=0) THEN ; ii=ii+1 ; nmed(ii)=G_H5_C(mn,'names_of_dimensions',jj) ; ENDIF
    ENDDO
    nmed = nmed(tsz(1):1:-1)
    CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error) !create attribute dataspac
    CALL H5ACREATE_F(dataset(mn), 'names of dimensions', atype, a_dataspace, attribute, error)
    CALL H5AWRITE_F(attribute, atype, nmed, tsz, error)
    CALL H5ACLOSE_F(attribute, error)
    CALL H5SCLOSE_F(a_dataspace, error)
    
    DO dd=1,no_dimensions
        CALL DIMENSION_ATTRIBUTES(nmed(dd))
    ENDDO
    DEALLOCATE(nmed)
!database type
!title
    CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
    CALL H5TSET_SIZE_F(atype, 1, error)
    arank  = 1
    tsz    = 0
    tsz(1) = 1
    typ    = G_H5_C(mn,'typ')
    CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
    CALL H5ACREATE_F(dataset(mn), 'database type', atype, a_dataspace, attribute, error)
    CALL H5AWRITE_F(attribute, atype, typ(1:1), tsz, error)
    CALL H5ACLOSE_F(attribute, error)
    CALL H5SCLOSE_F(a_dataspace, error)

CONTAINS

    !cscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscsc
    SUBROUTINE dimension_attributes(name)
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(csz)           :: dum(1)


    SELECT CASE(name)

    CASE('time')
        arank  = 1
        tsz(1) = 1
        dum    = 'has its own dataset'
        CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
        !CALL H5TSET_SIZE_F(atype, LEN_TRIM(dum(1)), error)
        CALL H5TSET_SIZE_F(atype, INT(LEN_TRIM(dum(1)),KIND=SIZE_T), error)  !160913
        CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
        CALL H5ACREATE_F(dataset(mn), 'time', atype, a_dataspace, attribute, error)
        CALL H5AWRITE_F(attribute, atype, dum, tsz, error)
        CALL H5ACLOSE_F(attribute, error)
        CALL H5SCLOSE_F(a_dataspace, error)
   
    CASE('column')
        arank  = 1
        tsz(1) = 2
        atype  = H5T_NATIVE_INTEGER
        CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
        CALL H5ACREATE_F(dataset(mn), 'column limits', atype, a_dataspace, attribute, error)
        CALL H5AWRITE_F(attribute, atype, (/G_H5_I(mn,'ilow'),G_H5_I(mn,'ihigh')/), tsz, error)
        CALL H5ACLOSE_F(attribute, error)
        CALL H5SCLOSE_F(a_dataspace, error)

    CASE('row')
        arank  = 1
        tsz(1) = 2
        atype  = H5T_NATIVE_INTEGER
        CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
        CALL H5ACREATE_F(dataset(mn), 'row limits', atype, a_dataspace, attribute, error)
        CALL H5AWRITE_F(attribute, atype, (/G_H5_I(mn,'jlow'),G_H5_I(mn,'jhigh')/), tsz, error)
        CALL H5ACLOSE_F(attribute, error)
        CALL H5SCLOSE_F(a_dataspace, error)

    CASE('el-lst')
        arank    = 2
        tsz(1:2) = (/2,G_H5_I(mn,'sz')/)
        atype    = H5T_NATIVE_INTEGER
        ALLOCATE(pairs(tsz(1),tsz(2)))
        pairs(1,:) = (/(i,i=1,tsz(2))/)
        pairs(2,:) = G_H5_I(mn,'list',(/(jj,jj=1,tsz(2))/))  !hh%list
        CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
        CALL H5ACREATE_F(dataset(mn), 'element nos.', atype, a_dataspace, attribute, error)
        CALL H5AWRITE_F(attribute, atype, pairs, tsz, error)
        CALL H5ACLOSE_F(attribute, error)
        CALL H5SCLOSE_F(a_dataspace, error)
        DEALLOCATE(pairs)
        tsz(2) = 0

    CASE('el_typ')
        arank  = 1
        tsz(1) = G_H5_I(mn, 'no_mbr')
        CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
        CALL H5TSET_SIZE_F(atype, 6, error)
        ALLOCATE(nme(tsz(1)))
        nme = G_H5_C(mn,'el-typ',(/(jj,jj=1,tsz(1))/))
        CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
        CALL H5ACREATE_F(dataset(mn), 'element types', atype, a_dataspace, attribute, error)
        CALL H5AWRITE_F(attribute, atype, nme, tsz, error)
        CALL H5ACLOSE_F(attribute, error)
        CALL H5SCLOSE_F(a_dataspace, error)
        DEALLOCATE(nme)

    CASE('layer')
        arank  = 1
        tsz(1) = 2
        atype  = H5T_NATIVE_INTEGER
        CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
        CALL H5ACREATE_F(dataset(mn), 'layer limits', atype, a_dataspace, attribute, error)
        CALL H5AWRITE_F(attribute, atype, (/G_H5_I(mn,'klow'),G_H5_I(mn,'khigh')/), tsz, error)
        CALL H5ACLOSE_F(attribute, error)
        CALL H5SCLOSE_F(a_dataspace, error)

    CASE('extra')
        arank = 1
        tsz(1) = G_H5_I(mn,'no_extra_dimensions') !SIZE(hh%names_of_extra_dimensions,DIM=1)
        CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
        CALL H5TSET_SIZE_F(atype, 6, error)
        ALLOCATE(nme(tsz(1)))
        nme = G_H5_C(mn, 'names_of_extra_dimensions', (/(jj,jj=1,tsz(1))/))
        CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error) !create attribute dataspac
        CALL H5ACREATE_F(dataset(mn), 'extra', atype, a_dataspace, attribute, error)
        CALL H5AWRITE_F(attribute, atype, nme, tsz, error)
        CALL H5ACLOSE_F(attribute, error)
        CALL H5SCLOSE_F(a_dataspace, error)
        DEALLOCATE(nme)
    END SELECT

    END SUBROUTINE dimension_attributes
END SUBROUTINE create_variables_attributes



!MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF
!MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF
!MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF MAP STUFF
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE save_surf_elev_as_map(mn, dat, magnif)
INTEGER, INTENT(IN)                :: mn, magnif
INTEGER                            :: sz(2)
REAL, DIMENSION(:,:,:), INTENT(IN) :: dat
CHARACTER(csz)                     :: name, title
WRITE(name,'(A,I1,A)') 'SV',ver,'_elevation'
WRITE(title,'(A,I1,A)') 'SV',ver,' surface elevation'
sz  = szz(mn)%a(2:3)
CALL ADD_AN_IMAGE_TO_GROUP(name, title, magnif, pic=GET_REAL_IMAGE_INDEX(sz, dat, magnif, mn))
END SUBROUTINE save_surf_elev_as_map

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE save_numbers_as_spreadsheet(mn)
INTEGER, INTENT(IN) :: mn
INTEGER, PARAMETER  :: magnif=20
INTEGER             :: sz(2)
sz = szz(mn)%a(2:3)
CALL ADD_MAGNIFIED_INTEGER_SPREADSHEET_TO_GROUP(mn, nme='numbering', magnif=magnif, magarr=GET_MAGNIFIED_SU_ARR(sz, magnif, mn))
END SUBROUTINE save_numbers_as_spreadsheet


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE add_an_image_to_group(name, title, magnif, pic)
INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: pic
INTEGER, INTENT(IN)                           :: magnif
INTEGER, PARAMETER                            :: mmax=256, vrange(2)=[0,mmax]
INTEGER                                       :: i, p, minvi, maxvi, arank, st
INTEGER(HID_T)                                :: dataspace, atype, attribute, a_dataspace, dataset
INTEGER(HSIZE_T), DIMENSION(1)                :: tsz
CHARACTER(*), INTENT(IN)                      :: name, title
TYPE(ssz)                                     :: aszz
REAL                                          :: minvr, maxvr
LOGICAL, SAVE                                 :: first = .TRUE.
INTEGER(HSIZE_T)                              :: wid, hei
CHARACTER(*), PARAMETER                       :: pal_name = "palette1"     ! Dataset name
INTEGER(HSIZE_T), DIMENSION(2)                :: pal_dims = [mmax,3] ! Dataset dimensions
INTEGER, DIMENSION(mmax*3)                    :: pal_data_in

IF(first) THEN
    pal_data_in                = [(MIN(mmax-1,4*i/3),i,i/2,i=1,mmax)]
    pal_data_in((MMAX-1)*3+1:) = [80,125,255]
    pal_data_in(1:3)           = [5,125,125]
    CALL H5GCREATE_F(file, 'CATCHMENT_MAPS', group_images, error)
    ALLOCATE(aszz%a(2))
    FIRST = .FALSE.
ENDIF

wid = SIZE(pic,DIM=1)
hei = SIZE(pic,DIM=2)

!CALL H5IMmake_image_8bit_F(group_images, name, wid, hei, pic, error)
CALL make_tidy_image_8(group_images, name, wid, hei,  pic, error)
CALL h5IMmake_palette_F(group_images, pal_name, pal_dims, pal_data_in, error)
CALL H5IMlink_palette_f(group_images, name, pal_name, error)
END SUBROUTINE add_an_image_to_group

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE make_tidy_image_8(loc_id, name, wid, hei, pic, err)
INTEGER, PARAMETER                            :: rank=2
INTEGER, INTENT(OUT)                          :: err
INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: pic
INTEGER(HID_T), INTENT(IN)                    :: loc_id
INTEGER(HSIZE_T), INTENT(IN)                  :: wid, hei
INTEGER(HSIZE_T), DIMENSION(rank)             :: dims
CHARACTER(*), INTENT(IN)                      :: name

dims = [wid,hei]
err  = 0
CALL H5LTmake_dataset_int_f(loc_id, name, 2, dims, pic, err)
!subroutine h5ltmake_dataset_int_f(loc_id, dset_name, rank, dims, buf, errcode)
!  integer(HID_T), intent(IN) :: loc_id           ! file or group identifier 
!  character(LEN=*), intent(IN) :: dset_name      ! name of the dataset 
!  integer, intent(IN) :: rank                    ! rank 
!  integer(HSIZE_T), dimension(*), intent(IN) :: dims ! size of the buffer buf  
!  integer, intent(IN), dimension(*) :: buf       ! data buffer 
!  integer :: errcode                             ! error code
!end subroutine h5ltmake_dataset_int_f


!subroutine h5ltset_attribute_string_f(loc_id, dset_name, attr_name, buf, errcode )
!  implicit none
!  integer(HID_T), intent(IN) :: loc_id           ! file or group identifier 
!  character(LEN=*), intent(IN) :: dset_name      ! name of the dataset 
!  character(LEN=*), intent(IN) :: attr_name      ! name of the attribute
!  integer :: errcode                             ! error code
!  character(LEN=*), intent(IN) :: buf            ! data buffer
!end subroutine h5ltset_attribute_string_f

CALL H5LTset_attribute_string_f(loc_id, name, "CLASS", "IMAGE", err)
CALL H5LTset_attribute_string_f(loc_id, name, "IMAGE_VERSION", "1.2", err)
CALL H5LTset_attribute_string_f(loc_id, name, "IMAGE_SUBCLASS", "IMAGE_INDEXED", err )

END SUBROUTINE make_tidy_image_8

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE add_magnified_integer_spreadsheet_to_group(mn, nme, magnif, magarr)
INTEGER, INTENT(IN)                     :: mn, magnif, magarr(:,:)
INTEGER(HID_T)                          :: dataspace, atype, attribute, a_dataspace, dataset
INTEGER                                 :: arank
INTEGER(HSIZE_T), DIMENSION(7)          :: tsz  !don't know why this could not be set at size 1 - compilation problem
TYPE(ssz)                               :: aszz
CHARACTER(*), INTENT(IN)                :: nme
CHARACTER(csz)                          :: title, name
LOGICAL, SAVE                           :: first = .TRUE.

IF(first) THEN
    first = .FALSE.
    CALL H5GCREATE_F(file, 'CATCHMENT_SPREADSHEETS', group_magnified_integer, error)
ENDIF

WRITE(name, '(A,I1,A)') 'SV', ver, '_'//TRIM(nme)
title = name
arank = 2
ALLOCATE(aszz%a(2))
aszz%a = SHAPE(magarr)
CALL H5SCREATE_SIMPLE_F(arank, aszz%a, dataspace, error)
CALL H5PSET_CHUNK_F    (dataset_compress_property, 2, aszz%a, error)
!CALL H5DCREATE_F       (group_magnified_integer, name, H5T_NATIVE_INTEGER, dataspace, dataset, error, creation_prp=dataset_compress_property)
CALL H5DCREATE_F       (group_magnified_integer, name, H5T_NATIVE_INTEGER, dataspace, dataset, error, dcpl_id=dataset_compress_property)
CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
CALL H5TSET_SIZE_F(atype, csz, error)
arank  = 1
tsz    = 0
tsz(1) = 1
!name attribute
CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
CALL H5ACREATE_F(dataset, 'title', atype, a_dataspace, attribute, error)
CALL H5AWRITE_F(attribute, atype, title, tsz, error)
CALL H5ACLOSE_F(attribute, error)
CALL H5SCLOSE_F(a_dataspace, error)
!maginfication attribute
CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
CALL H5ACREATE_F(dataset, 'magnification', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, magnif, tsz, error)
CALL H5ACLOSE_F(attribute, error)
CALL H5SCLOSE_F(a_dataspace, error)
CALL H5DWRITE_F(dataset, H5T_NATIVE_INTEGER, magarr, aszz%a, error)  !write to file

CALL H5DCLOSE_F(dataset, error)
CALL H5SCLOSE_F(dataspace, error)
END SUBROUTINE add_magnified_integer_spreadsheet_to_group

END MODULE visualisation_hdf5

! KEEP KEEP KEEP KEEP KEEP KEEP ************ USES COMPRESSION USES COMPRESSION
!IF(PRESENT(pic_int)) THEN
!    wid = SIZE(pic_int,DIM=1)
!    hei = SIZE(pic_int,DIM=2)
!    ALLOCATE(pic(SIZE(pic_int,DIM=1), SIZE(pic_int,DIM=2)))
!    minvi = MINVAL(pic_int)
!    maxvi = MAXVAL(pic_int)
!    pic  = mmax * (pic_int-minvi)/(maxvi-minvi)  !scaling
!ELSE IF(PRESENT(pic_real)) THEN
!    wid = SIZE(pic_real,DIM=1)
!    hei = SIZE(pic_real,DIM=2)
!!    ALLOCATE(pic(SIZE(pic_real,DIM=1), SIZE(pic_real,DIM=2)))
!!    minvr = MINVAL(pic_real)
!!    maxvr = MAXVAL(pic_real)
!!    pic  = mmax * (pic_real-minvr)/(maxvr-minvr)  !scaling
!!pic => GET_REAL_IMAGE_INDEX(sz, pic_real, mag, mn)
!ELSE IF(PRESENT(pic_l)) THEN
!    wid = SIZE(pic_L,DIM=1)
!    hei = SIZE(pic_L,DIM=2)
!    ALLOCATE(pic(SIZE(pic_L,DIM=1), SIZE(pic_L,DIM=2)))
!    DO i=1,SIZE(pic_L,DIM=1)
!        WHERE(pic_L(i,:))
!            pic(i,:) = mmax
!        ELSEWHERE
!            pic(i,:) = 1
!        ENDWHERE
!    ENDDO
!ELSE
!    RETURN
!ENDIF

!aszz%a = SHAPE(pic)
!arank = 2
!
!CALL H5SCREATE_SIMPLE_F(arank, aszz%a, dataspace, error)
!CALL H5PSET_CHUNK_F    (dataset_compress_property, 2, aszz%a, error)
!
!CALL H5DCREATE_F       (group_images, name, H5T_STD_U8BE, dataspace, dataset, error, creation_prp=dataset_compress_property)
!CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
!CALL H5TSET_SIZE_F(atype, csz, error)
!arank  = 1
!tsz    = 0
!tsz(1) = 1
!!name attribute
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'title', atype, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, atype, title, tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image class attribute
!CALL H5TSET_SIZE_F(atype, 6, error)
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'CLASS', atype, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, atype, "IMAGE", tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image subclass class attribute
!CALL H5TSET_SIZE_F(atype, 15, error)
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'IMAGE_SUBCLASS', atype, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, atype, 'IMAGE_GREYSCALE', tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image color model
!!CALL H5TSET_SIZE_F(atype, 4, error)
!!cALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!!CALL H5ACREATE_F(dataset, 'IMAGE_COLORMODEL', atype, a_dataspace, attribute, error)
!!CALL H5AWRITE_F(attribute, atype, 'RGB', tsz, error)
!!CALL H5ACLOSE_F(attribute, error)
!!CALL H5SCLOSE_F(a_dataspace, error)
!!image version
!CALL H5TSET_SIZE_F(atype, 4, error)
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'IMAGE_VERSION', atype, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, atype, '1', tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image white
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'IMAGE_WHITE_IS_ZERO', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, 1, tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!maginfication attribute
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'magnification', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, magnif, tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image MINMAX
!tsz(1) = 2
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'IMAGE_MINMAXRANGE', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, vrange, tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!

!p= LOC(pic)
!CALL H5DWRITE_F(dataset, H5T_STD_U8BE, pic4, aszz%a, error)  !write to file

!CALL H5DCLOSE_F(dataset, error)
!CALL H5SCLOSE_F(dataspace, error)
! KEEP KEEP KEEP KEEP KEEP KEEP ************ USES COMPRESSION USES COMPRESSION




!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!SUBROUTINE save_numbers_as_map_old(mn, file, dataset_compress_property)
!INTEGER, INTENT(IN)                     :: mn
!INTEGER, PARAMETER                      :: magnif=20, mmax=255  !built-in magnification
!INTEGER(HID_T), INTENT(IN)              :: file, dataset_compress_property
!INTEGER(HID_T)                          :: dataspace, atype, attribute, a_dataspace, dataset, group_plans
!INTEGER(HSIZE_T)                        :: arank
!INTEGER(HSIZE_T), DIMENSION(7)          :: tsz  !don't know why this could not be set at size 1 - compilation problem
!TYPE(ssz)                               :: aszz
!CHARACTER(csz)                          :: name, title
!INTEGER(HID_T)                          :: file2
!INTEGER                                 :: p
!INTEGER(1), DIMENSION(:,:), ALLOCATABLE :: pic
!INTEGER, DIMENSION(100,100)    :: pic4
!POINTER (p, pic4)
!!CALL H5FCREATE_F(TRIM(DIRQQ)//'/'//'output/test.h5', H5F_ACC_TRUNC_F, file2, error)
!
!CALL H5GCREATE_F(file, 'CATCHMENT_MAP', group_plans, error)
!!name  = 'SV4_numbering'
!WRITE(name,'(A,I1,A)') 'SV',ver,'_numbering'
!!title = 'SV4 element number'
!WRITE(title,'(A,I1,A)') 'SV',ver,' element number'
!arank = 2
!ALLOCATE(aszz%a(2))
!aszz%a = magnif*szz(mn)%a(2:3)
!CALL H5SCREATE_SIMPLE_F(arank, aszz%a, dataspace, error)
!CALL H5PSET_CHUNK_F    (dataset_compress_property, 2, aszz%a, error)
!
!CALL H5DCREATE_F       (group_plans, name, H5T_STD_U8BE, dataspace, dataset, error, creation_prp=dataset_compress_property)
!!    CALL H5DCREATE_F   (file2, name, H5T_NATIVE_INTEGER, dataspace, dataset, error, creation_prp=dataset_compress_property)
!CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
!CALL H5TSET_SIZE_F(atype, csz, error)
!arank  = 1
!tsz    = 0
!tsz(1) = 1
!!name attribute
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'title', atype, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, atype, title, tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image class attribute
!CALL H5TSET_SIZE_F(atype, 6, error)
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'CLASS', atype, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, atype, "IMAGE", tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image subclass class attribute
!CALL H5TSET_SIZE_F(atype, 15, error)
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'IMAGE_SUBCLASS', atype, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, atype, 'IMAGE_GREYSCALE', tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image version
!CALL H5TSET_SIZE_F(atype, 4, error)
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'IMAGE_VERSION', atype, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, atype, '1', tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image white
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'IMAGE_WHITE_IS_ZERO', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, 1, tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!maginfication attribute
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'magnification', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, magnif, tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!!image MINMAX
!tsz(1) = 2
!CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
!CALL H5ACREATE_F(dataset, 'IMAGE_MINMAXRANGE', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
!CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, (/0,255/), tsz, error)
!CALL H5ACLOSE_F(attribute, error)
!CALL H5SCLOSE_F(a_dataspace, error)
!
!ALLOCATE(pic(aszz%a(1), aszz%a(2)))
!pic = GET_NUMBER_ARR(aszz%a, magnif, mn)
!pic = pic * mmax/MAXVAL(pic)
!p = LOC(pic)
!
!CALL H5DWRITE_F(dataset, H5T_STD_U8BE, pic4, aszz%a, error)  !write to file
!
!CALL H5DCLOSE_F(dataset, error)
!CALL H5SCLOSE_F(dataspace, error)
!CALL H5GCLOSE_F(group_plans, error)
!END SUBROUTINE save_numbers_as_map_old

!!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!SUBROUTINE save_numbers_as_map(mn, magnif)
!INTEGER, INTENT(IN)                     :: mn, magnif
!INTEGER                                 :: sz(2)
!CHARACTER(csz)                          :: name, title
!LOGICAL, DIMENSION(:,:), POINTER        :: pic
!
!WRITE(name,'(A,I1,A)') 'SV',ver,'_rivers'
!WRITE(title,'(A,I1,A)') 'SV',ver,' rivers'
!
!sz  = szz(mn)%a(2:3)
!pic => GET_IS_LINK_MAGNIFIED(sz, magnif, mn)
!
!CALL ADD_AN_IMAGE_TO_GROUP(name, title, magnif, pic_L=pic)
!DEALLOCATE(pic)
!
!END SUBROUTINE save_numbers_as_map