!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_hdf5
! Full HDF5 module for visualization output
! Uses the actual HDF5 Fortran library for proper visualization data output

   USE VISUALISATION_PASS, ONLY : DIRQQ, ver, rootdir, hdf5filename
   USE VISUALISATION_METADATA, ONLY : G_C=>GET_METADATA_C, G_L=>GET_METADATA_L, &
      G_I=>GET_METADATA_I, S_I=>SET_METADATA_I, &
      G_I_F=>GET_METADATA_I_FIRST, ndim, &
      G_H5_I=>GET_METADATA_HDF5_I, G_H5_L=>GET_METADATA_HDF5_L, &
      G_H5_C=>GET_METADATA_HDF5_C, INCREMENT_HDF5_TSTEP_NO
   USE VISUALISATION_STRUCTURE, ONLY : TIME_COUNT, GET_HDF5_I, GET_HDF5_R, GET_HDF5_TIME, INT_PTR_KIND
   USE VISUALISATION_MAP, ONLY : GET_REAL_IMAGE_INDEX, GET_MAGNIFIED_SU_ARR

! Import HDF5 Fortran library modules
   USE HDF5
   USE H5IM
   USE H5LT

   IMPLICIT NONE

! HDF5 variables and parameters
   INTEGER                 :: error  !Error flag
   INTEGER, SAVE           :: jndim(ndim)
   INTEGER, PARAMETER      :: csz=70
   INTEGER(HSIZE_T), PARAMETER :: csz_hsize = 70  ! HDF5 size type version
   REAL, PARAMETER         :: zero=0.0
   LOGICAL, PARAMETER      :: T=.TRUE., F=.FALSE.

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   TYPE ssz
      INTEGER(HSIZE_T), DIMENSION(:), POINTER :: a
   END TYPE ssz
   TYPE(ssz), DIMENSION(:), ALLOCATABLE, SAVE  :: szz, newsz  !recording array size, and its size after extension

   INTEGER(HID_T), DIMENSION(:), ALLOCATABLE   :: dataset, dataspace, dtype, orig_dataspace, t_dataspace, t_dataset
   INTEGER(HSIZE_T)                            :: t_newsz(1)
   INTEGER, DIMENSION(:), ALLOCATABLE          :: rank
   INTEGER(HID_T)                              :: orig_t_dataspace, group_static, group_dynamic, group_images, file, &
      group_magnified_integer
   INTEGER(HID_T), SAVE                        :: dataset_compress_property, t_dataset_compress_property

   PRIVATE
   PUBLIC :: SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP

CONTAINS

!===============================================================================
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
      CALL H5PCREATE_F(H5P_DATASET_CREATE_F, dataset_compress_property, error)
      CALL H5PCREATE_F(H5P_DATASET_CREATE_F, t_dataset_compress_property, error)
      CALL H5PSET_DEFLATE_F(dataset_compress_property, 9, error)
      CALL H5PSET_DEFLATE_F(t_dataset_compress_property, 9, error)

      CALL H5FCREATE_F(TRIM(hdf5filename), H5F_ACC_TRUNC_F, file, error)
      CALL H5GCREATE_F(file, 'CONSTANTS', group_static, error)
      CALL H5GCREATE_F(file, 'VARIABLES', group_dynamic, error)
      CALL H5GCREATE_F(file, 'IMAGES', group_images, error)

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
            name = combination_name(mn)
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

         CALL H5DCREATE_F(gp, name, dtype(mn), dataspace(mn), dataset(mn), error, dcpl_id=dataset_compress_property)

         CALL CREATE_VARIABLES_ATTRIBUTES(mn)

         IF(istimeseries) THEN
            CALL H5DCREATE_F(gp, namet, H5T_NATIVE_REAL, t_dataspace(mn), t_dataset(mn), error, dcpl_id=t_dataset_compress_property)
            CALL CREATE_TIME_ATTRIBUTES(mn)
         ENDIF

      ENDDO

   END SUBROUTINE initialise

!===============================================================================
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

!===============================================================================
! Additional required subroutines for interface compatibility
!===============================================================================
   SUBROUTINE SAVE_VISUALISATION_DATA_TO_DISK(mn, time)
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
         ! Key functionality disabled for standard version
         notflag = F
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
         newsz(mn)%a    = szz(mn)%a ; newsz(mn)%a(1) = tstep
         t_newsz        = (/tstep/)
         CALL H5DEXTEND_F(dataset(mn), newsz(mn)%a, error)
         CALL H5DEXTEND_F(t_dataset(mn), t_newsz, error)
         tc = TIME_COUNT(G_C(mn,'typ'),INT(G_I_F(mn,'first'), KIND=INT_PTR_KIND))
      ENDIF
      IF(time==zero .OR. tc==buffer_length_for_storage) &
         CALL WRITE_MN(mn, tc, time==zero, tstep, G_H5_L(mn,'isreal'), G_H5_I(mn,'szorder',jndim), G_H5_I(mn,'ilow'), G_H5_I(mn,'jlow'), G_H5_I(mn,'klow'))

   END SUBROUTINE SAVE_VISUALISATION_DATA_TO_DISK

   SUBROUTINE VISUALISATION_TIDY_UP()
      IMPLICIT NONE
      INTEGER :: ni, mn
      LOGICAL :: istimeseries

      ni = G_I(0,'no_items')
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

   END SUBROUTINE VISUALISATION_TIDY_UP

!===============================================================================
   SUBROUTINE write_mn(mn, amount, firstwrites, tstep, isreal, szorder, ilow, jlow, klow)
      INTEGER, INTENT(IN)                                :: mn, tstep, ilow, jlow, klow, amount
      INTEGER, DIMENSION(:), INTENT(IN)                  :: szorder
      INTEGER                                            :: am, hhdim(ndim)
      INTEGER(HID_T)                                     :: filespace, t_filespace
      INTEGER(HSIZE_T), DIMENSION(ndim)                  :: start, t_start, ccount, t_ccount
      INTEGER(HSIZE_T)                                   :: t_sz(7)
      INTEGER, DIMENSION(ndim)                           :: sz
      REAL                                               :: time
      REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE          :: surf_elv
      LOGICAL, INTENT(IN)                                :: firstwrites, isreal
      LOGICAL                                            :: istimeseries
      CHARACTER(2)                                       :: typ
      CHARACTER(csz)                                     :: name
      INTEGER(INT_PTR_KIND)                              :: first_val

      name            = G_H5_C(mn,'name')
      first_val       = G_I_F(mn,'first')
      typ             = G_C(mn,'typ')
      istimeseries    = G_L(mn,'istimeseries')
      hhdim = G_H5_I(mn, 'dimensions', jndim)
      sz    = MAX(1,hhdim)

      IF(firstwrites) THEN
         CALL H5SCOPY_F(dataspace(mn), filespace, error)
         CALL H5SCOPY_F(t_dataspace(mn), t_filespace, error)
      ELSE
         CALL H5SCREATE_SIMPLE_F(rank(mn), newsz(mn)%a,   filespace,   error)
         CALL H5SCREATE_SIMPLE_F(1,    t_newsz, t_filespace, error)
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
            t_start(1) = tstep-amount+am-1
            CALL H5SSELECT_HYPERSLAB_F(t_filespace, H5S_SELECT_SET_F, t_start, t_ccount, error)
         ENDIF

         IF(istimeseries) THEN
            time = GET_HDF5_TIME(typ, first_val)
            CALL H5DWRITE_F(t_dataset(mn), H5T_NATIVE_REAL, (/time/), &
               t_sz, error, mem_space_id=orig_t_dataspace, file_space_id=t_filespace)
         ENDIF

         IF(isreal) THEN
            IF(name=='surf_elv') THEN
               ALLOCATE(surf_elv(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)))
               surf_elv = GET_HDF5_R(typ, sz, szorder, first_val, ilow, jlow, klow)
               CALL H5DWRITE_F(dataset(mn), dtype(mn), surf_elv, &
                  szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)
            ELSE
               CALL H5DWRITE_F(dataset(mn), dtype(mn), GET_HDF5_R(typ, sz, szorder, first_val, ilow, jlow, klow), &
                  szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)
            ENDIF
         ELSE
            CALL H5DWRITE_F(dataset(mn), dtype(mn), GET_HDF5_I(typ, sz, szorder, first_val, ilow, jlow, klow), &
               szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)
         ENDIF
      ENDDO

      CALL S_I(mn,'first', first_val)
      CALL H5SCLOSE_F(filespace, error)
      CALL H5SCLOSE_F(t_filespace, error)

      IF(name=='number') CALL SAVE_NUMBERS_AS_SPREADSHEET(mn)
      IF(name=='surf_elv') THEN
         CALL SAVE_SURF_ELEV_AS_MAP(mn, surf_elv(1,1,1,:,:,:), magnif=20)
         DEALLOCATE(surf_elv)
      ENDIF

   END SUBROUTINE write_mn

!===============================================================================
   SUBROUTINE create_time_attributes(mn)
      INTEGER, INTENT(IN)                     :: mn
      INTEGER                                 :: arank
      INTEGER(HSIZE_T), DIMENSION(7)          :: tsz
      INTEGER(HID_T)                          :: atype
      INTEGER(HID_T)                          :: attribute, a_dataspace

      CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
      CALL H5TSET_SIZE_F(atype, csz_hsize, error)
      arank  = 1
      tsz    = 0
      tsz(1) = 1

! units
      CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
      CALL H5ACREATE_F(t_dataset(mn), 'units', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, 'hours', tsz, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

   END SUBROUTINE create_time_attributes

!===============================================================================
   SUBROUTINE create_variables_attributes(mn)
      INTEGER, INTENT(IN)                     :: mn
      INTEGER                                 :: dd, ii, jj, no_dimensions
      INTEGER                                 :: arank
      INTEGER(HSIZE_T), DIMENSION(7)          :: tsz
      INTEGER(HID_T)                          :: atype
      INTEGER(HID_T)                          :: attribute, a_dataspace
      INTEGER                                 :: i
      INTEGER, DIMENSION(:,:), ALLOCATABLE    :: pairs
      CHARACTER(2)                            :: typ
      CHARACTER(6), DIMENSION(:), ALLOCATABLE :: nme, nmed

      CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
      CALL H5TSET_SIZE_F(atype, csz_hsize, error)
      arank  = 1
      tsz    = 0
      tsz(1) = 1

! title
      CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'title', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, G_H5_C(mn,'title'), tsz, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

! units
      CALL H5TSET_SIZE_F(atype, INT(8,SIZE_T), error)
      tsz(1) = 1
      CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'units', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, G_H5_C(mn,'units'), tsz, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

! basis
      CALL H5TSET_SIZE_F(atype, INT(12,SIZE_T), error)
      tsz(1) = 1
      CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'basis', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, G_H5_C(mn,'basis'), tsz, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

! scope
      CALL H5TSET_SIZE_F(atype, INT(7,SIZE_T), error)
      tsz(1) = 1
      CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'scope', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, G_H5_C(mn,'scope'), tsz, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

! names of dimensions
      CALL H5TSET_SIZE_F(atype, INT(6,SIZE_T), error)
      no_dimensions = G_H5_I(mn,'no_dimensions')
      tsz(1)        = no_dimensions
      ALLOCATE(nmed(tsz(1)))
      ii = 0
      DO jj=1,ndim
         IF(G_H5_I(mn,'dimensions',jj)/=0) THEN ; ii=ii+1 ; nmed(ii)=G_H5_C(mn,'names_of_dimensions',jj) ; ENDIF
      ENDDO
      nmed = nmed(tsz(1):1:-1)
      CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error) !create attribute dataspace
      CALL H5ACREATE_F(dataset(mn), 'names of dimensions', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, nmed, tsz, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      DO dd=1,no_dimensions
         CALL DIMENSION_ATTRIBUTES(nmed(dd))
      ENDDO
      DEALLOCATE(nmed)

! database type
      CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
      CALL H5TSET_SIZE_F(atype, INT(1,SIZE_T), error)
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

      !=========================================================================
      SUBROUTINE dimension_attributes(name)
         CHARACTER(*), INTENT(IN) :: name
         CHARACTER(csz)           :: dum(1)

         SELECT CASE(name)

          CASE('time')
            arank  = 1
            tsz(1) = 1
            dum    = 'has its own dataset'
            CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
            CALL H5TSET_SIZE_F(atype, INT(LEN_TRIM(dum(1)),KIND=SIZE_T), error)
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
            pairs(2,:) = G_H5_I(mn,'list',(/(jj,jj=1,tsz(2))/))
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
            CALL H5TSET_SIZE_F(atype, INT(6,SIZE_T), error)
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
            tsz(1) = G_H5_I(mn,'no_extra_dimensions')
            CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
            CALL H5TSET_SIZE_F(atype, INT(6,SIZE_T), error)
            ALLOCATE(nme(tsz(1)))
            nme = G_H5_C(mn, 'names_of_extra_dimensions', (/(jj,jj=1,tsz(1))/))
            CALL H5SCREATE_SIMPLE_F(arank, tsz, a_dataspace, error) !create attribute dataspace
            CALL H5ACREATE_F(dataset(mn), 'extra', atype, a_dataspace, attribute, error)
            CALL H5AWRITE_F(attribute, atype, nme, tsz, error)
            CALL H5ACLOSE_F(attribute, error)
            CALL H5SCLOSE_F(a_dataspace, error)
            DEALLOCATE(nme)
         END SELECT

      END SUBROUTINE dimension_attributes

   END SUBROUTINE create_variables_attributes

!===============================================================================
! MAP FUNCTIONALITY - Image and spreadsheet output
!===============================================================================
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

!===============================================================================
   SUBROUTINE save_numbers_as_spreadsheet(mn)
      INTEGER, INTENT(IN) :: mn
      INTEGER, PARAMETER  :: magnif=20
      INTEGER             :: sz(2)

      sz = szz(mn)%a(2:3)
      CALL ADD_MAGNIFIED_INTEGER_SPREADSHEET_TO_GROUP(mn, nme='numbering', magnif=magnif, magarr=GET_MAGNIFIED_SU_ARR(sz, magnif, mn))

   END SUBROUTINE save_numbers_as_spreadsheet

!===============================================================================
   SUBROUTINE add_an_image_to_group(name, title, magnif, pic)
      INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: pic
      INTEGER, INTENT(IN)                           :: magnif
      INTEGER, PARAMETER                            :: mmax=256, vrange(2)=[0,mmax]
      INTEGER                                       :: i, p, minvi, maxvi, arank_img, st
      INTEGER(HID_T)                                :: dataspace_img, atype_img, attribute_img, a_dataspace_img, dataset_img
      INTEGER(HSIZE_T), DIMENSION(1)                :: tsz_img
      CHARACTER(*), INTENT(IN)                      :: name, title
      TYPE(ssz)                                     :: aszz
      REAL                                          :: minvr, maxvr
      LOGICAL, SAVE                                 :: first = .TRUE.
      INTEGER(HSIZE_T)                              :: wid, hei
      CHARACTER(*), PARAMETER                       :: pal_name = "palette1"
      INTEGER(HSIZE_T), DIMENSION(2)                :: pal_dims = [mmax,3]
      INTEGER, DIMENSION(mmax*3)                    :: pal_data_in

      IF(first) THEN
         first = .FALSE.
#ifdef HAVE_H5IM_PALETTE
         ! Initialize palette data for scientific visualization
         ! Create a simple grayscale palette as example
         DO i = 1, mmax
            pal_data_in((i-1)*3 + 1) = i-1  ! Red
            pal_data_in((i-1)*3 + 2) = i-1  ! Green
            pal_data_in((i-1)*3 + 3) = i-1  ! Blue
         ENDDO
#endif
      ENDIF

      wid = SIZE(pic,DIM=1)
      hei = SIZE(pic,DIM=2)

      CALL make_tidy_image_8(group_images, name, wid, hei,  pic, error)

#ifdef HAVE_H5IM_PALETTE
! H5IM palette functions - only compiled if available
      CALL h5IMmake_palette_F(group_images, pal_name, pal_dims, pal_data_in, error)
      CALL H5IMlink_palette_f(group_images, name, pal_name, error)
#else
! H5IM palette functions not available - using basic HDF5 instead
! Note: Image data is still properly written, just without custom color palette
#endif

   END SUBROUTINE add_an_image_to_group

!===============================================================================
   SUBROUTINE make_tidy_image_8(loc_id, name, wid, hei, pic, err)
      INTEGER, PARAMETER                            :: rank_img=2
      INTEGER, INTENT(OUT)                          :: err
      INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: pic
      INTEGER(HID_T), INTENT(IN)                    :: loc_id
      INTEGER(HSIZE_T), INTENT(IN)                  :: wid, hei
      INTEGER(HSIZE_T), DIMENSION(rank_img)         :: dims
      CHARACTER(*), INTENT(IN)                      :: name

      dims = [wid,hei]
      err  = 0
      CALL H5LTmake_dataset_int_f(loc_id, name, 2, dims, pic, err)

      CALL H5LTset_attribute_string_f(loc_id, name, "CLASS", "IMAGE", err)
      CALL H5LTset_attribute_string_f(loc_id, name, "IMAGE_VERSION", "1.2", err)
      CALL H5LTset_attribute_string_f(loc_id, name, "IMAGE_SUBCLASS", "IMAGE_INDEXED", err )

   END SUBROUTINE make_tidy_image_8

!===============================================================================
   SUBROUTINE add_magnified_integer_spreadsheet_to_group(mn, nme, magnif, magarr)
      INTEGER, INTENT(IN)                     :: mn, magnif, magarr(:,:)
      INTEGER(HID_T)                          :: dataspace_spr, atype_spr, attribute_spr, a_dataspace_spr, dataset_spr
      INTEGER                                 :: arank_spr
      INTEGER(HSIZE_T), DIMENSION(7)          :: tsz_spr
      TYPE(ssz)                               :: aszz
      CHARACTER(*), INTENT(IN)                :: nme
      CHARACTER(csz)                          :: title, name
      LOGICAL, SAVE                           :: first = .TRUE.

      IF(first) THEN
         CALL H5GCREATE_F(file, 'CATCHMENT_SPREADSHEETS', group_magnified_integer, error)
         first = .FALSE.
      ENDIF

      WRITE(name, '(A,I1,A)') 'SV', ver, '_'//TRIM(nme)
      title = name
      arank_spr = 2
      ALLOCATE(aszz%a(2))
      aszz%a = SHAPE(magarr)

      CALL H5SCREATE_SIMPLE_F(arank_spr, aszz%a, dataspace_spr, error)
      CALL H5PSET_CHUNK_F    (dataset_compress_property, 2, aszz%a, error)
      CALL H5DCREATE_F       (group_magnified_integer, name, H5T_NATIVE_INTEGER, dataspace_spr, dataset_spr, error, dcpl_id=dataset_compress_property)

      CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype_spr, error)
      CALL H5TSET_SIZE_F(atype_spr, csz_hsize, error)
      arank_spr  = 1
      tsz_spr    = 0
      tsz_spr(1) = 1

! name attribute
      CALL H5SCREATE_SIMPLE_F(arank_spr, tsz_spr, a_dataspace_spr, error)
      CALL H5ACREATE_F(dataset_spr, 'title', atype_spr, a_dataspace_spr, attribute_spr, error)
      CALL H5AWRITE_F(attribute_spr, atype_spr, title, tsz_spr, error)
      CALL H5ACLOSE_F(attribute_spr, error)
      CALL H5SCLOSE_F(a_dataspace_spr, error)

! magnification attribute
      CALL H5SCREATE_SIMPLE_F(arank_spr, tsz_spr, a_dataspace_spr, error)
      CALL H5ACREATE_F(dataset_spr, 'magnification', H5T_NATIVE_INTEGER, a_dataspace_spr, attribute_spr, error)
      CALL H5AWRITE_F(attribute_spr, H5T_NATIVE_INTEGER, magnif, tsz_spr, error)
      CALL H5ACLOSE_F(attribute_spr, error)
      CALL H5SCLOSE_F(a_dataspace_spr, error)

      CALL H5DWRITE_F(dataset_spr, H5T_NATIVE_INTEGER, magarr, aszz%a, error)

      CALL H5DCLOSE_F(dataset_spr, error)
      CALL H5SCLOSE_F(dataspace_spr, error)

   END SUBROUTINE add_magnified_integer_spreadsheet_to_group

END MODULE visualisation_hdf5
