!> @brief Writes SHETRAN visualisation data and derived catchment products to HDF5.
!>
!> Static metadata items are stored below `/CONSTANTS`. Time-varying items are
!> stored in numbered groups below `/VARIABLES`, with separate `value` and
!> `time` datasets. The static `surf_elv` and `number` items also produce an
!> indexed elevation map below `/CATCHMENT_MAPS` and a magnified numbering grid
!> below `/CATCHMENT_SPREADSHEETS`.
!>
!> Metadata dimensions use SHETRAN's Fortran order. The HDF5 Fortran interface
!> presents them in reverse order to C-oriented readers, so the dimension-name
!> attribute is deliberately reversed as well. Consequently, time is the last
!> displayed axis in tools such as `h5dump`, although it is the first dimension
!> extended by this module.
!>
!> Metadata-driven value and time datasets use native default `REAL` or
!> `INTEGER` values and DEFLATE level 6. Derived products retain the datatypes
!> and filters selected by their individual helpers. HDF5 status values are
!> retained in the module variable `error`, but this module does not currently
!> report or recover from HDF5 failures.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced the HDF5 visualisation writer. |
!> | 2026-03-29 | SvB | Made temporary write arrays allocatable to avoid invalid storage and memory corruption. |
!> | 2026-04-07 | SvB | Made HDF5 size kinds portable for GFortran and closed temporary HDF5 identifiers. |
!> | 2026-04-08 | SB | Removed Intel-specific directives and legacy pointer code during the Intel IFX update. |
!> | 2026-04-14 | SvB | Guarded empty dimensions, enlarged names, and corrected time-dataspace ownership and cleanup. |
!> | 2026-08-23 | SvB | Changed the DEFLATE compression level from 9 to 6. |
!> @endhistory
MODULE visualisation_hdf5

   USE ISO_C_BINDING, ONLY: C_PTR

   USE VISUALISATION_PASS,      ONLY : DIRQQ, ver, rootdir, hdf5filename
   USE VISUALISATION_METADATA,  ONLY : G_C=>GET_METADATA_C, G_L=>GET_METADATA_L, &
      G_I=>GET_METADATA_I, S_PTR=>SET_METADATA_PTR, G_PTR=>GET_METADATA_PTR, ndim, &
      G_H5_I=>GET_METADATA_HDF5_I, G_H5_L=>GET_METADATA_HDF5_L, &
      G_H5_C=>GET_METADATA_HDF5_C, INCREMENT_HDF5_TSTEP_NO
   USE VISUALISATION_STRUCTURE, ONLY : TIME_COUNT, GET_HDF5_I, GET_HDF5_R, GET_HDF5_TIME
   USE VISUALISATION_MAP,       ONLY : GET_REAL_IMAGE_INDEX, GET_MAGNIFIED_SU_ARR

   USE HDF5
   USE H5IM
   USE H5LT

   USE MOD_PARAMETERS, ONLY : LENGTH_LINE, I_P
   USE MOD_ERROR, ONLY : errstat_alloc, errstat_dealloc

   IMPLICIT NONE

   INTEGER            :: error       !! Most recent HDF5 status code; currently not inspected.
   INTEGER, SAVE      :: jndim(ndim) !! Index vector `1:ndim` used for metadata array queries.
   INTEGER, PARAMETER :: csz=70      !! Character length used for generated names and string metadata.
   INTEGER, PARAMETER :: deflate_level=6 !! DEFLATE level shared by compressed datasets.
   REAL, PARAMETER    :: zero=0.0    !! Default-real zero used by the writer's exact time tests.
   LOGICAL, PARAMETER :: T=.TRUE.    !! Logical true shorthand used to initialise saved guards.
   LOGICAL, PARAMETER :: F=.FALSE.   !! Logical false shorthand used to initialise saved guards.


!> Holds the active HDF5 extents for one registered metadata item.
   TYPE ssz
      INTEGER(HSIZE_T), DIMENSION(:), POINTER :: a !! Extents in the HDF5 Fortran interface's order.
   END TYPE ssz
   TYPE(ssz), DIMENSION(:), ALLOCATABLE, SAVE :: szz   !! Initial/current write-block extents by item.
   TYPE(ssz), DIMENSION(:), ALLOCATABLE, SAVE :: newsz !! Extended dataset extents by item.

   INTEGER(HID_T), DIMENSION(:), ALLOCATABLE :: dataset        !! Value-dataset identifiers by item.
   INTEGER(HID_T), DIMENSION(:), ALLOCATABLE :: dataspace      !! Initial value-dataspace identifiers by item.
   INTEGER(HID_T), DIMENSION(:), ALLOCATABLE :: dtype          !! Native value datatype identifiers by item.
   INTEGER(HID_T), DIMENSION(:), ALLOCATABLE :: orig_dataspace !! Value memory-dataspace identifiers by item.
   INTEGER(HID_T), DIMENSION(:), ALLOCATABLE :: t_dataspace    !! Time-dataspace identifiers by item.
   INTEGER(HID_T), DIMENSION(:), ALLOCATABLE :: t_dataset      !! Time-dataset identifiers by item.
   INTEGER(HSIZE_T)                          :: t_newsz(1)     !! Extended time-dataset extent.
   INTEGER, DIMENSION(:), ALLOCATABLE        :: rank           !! Effective HDF5 rank by item.
   INTEGER(HID_T)                            :: orig_t_dataspace !! Shared one-value time memory dataspace.
   INTEGER(HID_T)                            :: group_static     !! `/CONSTANTS` group identifier.
   INTEGER(HID_T)                            :: group_dynamic    !! `/VARIABLES` group identifier.
   INTEGER(HID_T)                            :: group_images     !! Lazily created `/CATCHMENT_MAPS` identifier.
   INTEGER(HID_T)                            :: file             !! Visualisation HDF5 file identifier.
   INTEGER(HID_T)                            :: group_magnified_integer !! Lazily created spreadsheet group identifier.
   INTEGER(HID_T), SAVE :: dataset_compress_property   !! DEFLATE property list shared by value datasets.
   INTEGER(HID_T), SAVE :: t_dataset_compress_property !! DEFLATE property list shared by time datasets.

   PRIVATE
   PUBLIC :: SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP

CONTAINS

!> Creates the visualisation file and every metadata-driven HDF5 dataset.
!>
!> Visualisation metadata must already be registered and `hdf5filename` must
!> name a writable target. The target is truncated. Static datasets are created
!> directly in `/CONSTANTS`; each dynamic item gets a generated group below
!> `/VARIABLES` containing an unlimited `value` dataset and a matching unlimited
!> `time` dataset. Each dataset is chunked by its initial item shape and uses
!> DEFLATE level 6.
!>
!> Zero-rank metadata is represented by a one-element, rank-one dataset. The
!> routine is a one-shot initialiser: its allocatable state and saved group
!> identifiers are not prepared for a second invocation in the same process.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced the dataset initialisation. |
!> | 2026-04-07 | SvB | Made HDF5 dimensions portable to GFortran. |
!> | 2026-04-14 | SvB | Added the zero-rank stand-in and one shared unlimited time memory dataspace. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE initialise()
      INTEGER                  :: ni !! Number of registered visualisation items.
      INTEGER                  :: mn !! Current metadata-item index.
      INTEGER                  :: jj !! Dimension-index constructor variable.
      INTEGER, DIMENSION(ndim) :: hhdim !! Metadata extents including inactive zero entries.
      LOGICAL                  :: istimeseries !! Whether this item belongs below `/VARIABLES`.
      CHARACTER(csz)           :: name  !! Value dataset or dynamic group name.
      CHARACTER(csz)           :: namet !! Time dataset name.
      INTEGER(HID_T)           :: gp    !! Parent group identifier for the value dataset.
      INTEGER(HID_T), DIMENSION(:), ALLOCATABLE, SAVE :: gp_var !! Dynamic item groups; closed by `H5CLOSE_F`.
      INTEGER(HSIZE_T), DIMENSION(ndim) :: maxdims !! Maximum value extents, with time unlimited when present.
      INTEGER(HSIZE_T), DIMENSION(1)    :: t_maxdims !! Unlimited maximum extent for time datasets.
      INTEGER(HSIZE_T), PARAMETER       :: one=1 !! Initial time extent and time chunk length.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_HDF5:initialise"

      jndim = (/(jj,jj=1,ndim)/)
      ni    = G_I(0,'no_items')

      ALLOCATE(dataset(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "dataset", location, emsg)
      ALLOCATE(dataspace(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "dataspace", location, emsg)
      ALLOCATE(orig_dataspace(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "orig_dataspace", location, emsg)
      ALLOCATE(dtype(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "dtype", location, emsg)
      ALLOCATE(szz(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "szz", location, emsg)
      ALLOCATE(newsz(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "newsz", location, emsg)
      ALLOCATE(gp_var(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "gp_var", location, emsg)
      ALLOCATE(t_dataspace(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "t_dataspace", location, emsg)
      ALLOCATE(t_dataset(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "t_dataset", location, emsg)
      ALLOCATE(rank(ni), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "rank", location, emsg)

      CALL H5OPEN_F(error)
      CALL H5PCREATE_F(H5P_DATASET_CREATE_F, dataset_compress_property, error)
      CALL H5PCREATE_F(H5P_DATASET_CREATE_F, t_dataset_compress_property, error)
      CALL H5PSET_DEFLATE_F(dataset_compress_property, deflate_level, error)
      CALL H5PSET_DEFLATE_F(t_dataset_compress_property, deflate_level, error)

      CALL H5FCREATE_F(TRIM(hdf5filename), H5F_ACC_TRUNC_F, file, error)

      CALL H5GCREATE_F(file, 'CONSTANTS', group_static, error)
      CALL H5GCREATE_F(file, 'VARIABLES', group_dynamic, error)
      t_maxdims(1) = H5S_UNLIMITED_F
      CALL H5SCREATE_SIMPLE_F(1, (/one/), orig_t_dataspace, error, maxdims=t_maxdims)

      DO mn=1,ni
         hhdim = G_H5_I(mn, 'dimensions', jndim)
         rank(mn) = COUNT(hhdim>0)
         IF(rank(mn)==0) rank(mn) = 1
         ALLOCATE(szz(mn)%a(rank(mn)), newsz(mn)%a(rank(mn)))
         IF(COUNT(hhdim>0)>0) THEN
            szz(mn)%a = PACK(hhdim, hhdim>0)
         ELSE
            szz(mn)%a = 1
         ENDIF

         maxdims(2:rank(mn)) = szz(mn)%a(2:rank(mn))  !fixed dimensions
         istimeseries = G_H5_L(mn, 'istimeseries')
         IF(istimeseries) THEN
            maxdims(1) = H5S_UNLIMITED_F
            namet      = 'time'
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

         CALL H5SCOPY_F(orig_t_dataspace, t_dataspace(mn), error)

         CALL H5PSET_CHUNK_F(dataset_compress_property, rank(mn), szz(mn)%a, error)
         CALL H5PSET_CHUNK_F(t_dataset_compress_property, 1, (/one/), error)

         IF(G_H5_L(mn,'isreal')) THEN ; dtype(mn)=H5T_NATIVE_REAL ; ELSE ; dtype(mn)=H5T_NATIVE_INTEGER ; ENDIF

         CALL H5DCREATE_F(gp, name, dtype(mn), dataspace(mn), dataset(mn), error, &
            dcpl_id=dataset_compress_property)

         CALL CREATE_VARIABLES_ATTRIBUTES(mn)

         IF(istimeseries) THEN
            CALL H5DCREATE_F(gp, namet, H5T_NATIVE_REAL, t_dataspace(mn), &
               t_dataset(mn), error, dcpl_id=t_dataset_compress_property)
            CALL CREATE_TIME_ATTRIBUTES(mn)
         ENDIF

      ENDDO

   END SUBROUTINE initialise


!> Builds the HDF5 group name for one time-varying visualisation item.
!>
!> The result consists of the user's three-column output number, a space, the
!> metadata name, and—when applicable—the two-column sediment or contaminant
!> fraction. Sediment takes precedence if both variation flags are set. Leading
!> spaces in the `I3` output number are retained in the HDF5 group name.
!>
!> Callers must keep output numbers within `I3`, fraction numbers within `I2`,
!> and the name-plus-fraction within the eight characters available in `dum`;
!> otherwise formatted asterisks or truncation can make group names ambiguous.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced metadata-derived group names. |
!> | 2026-04-14 | SvB | Expanded the returned name from 12 to 70 characters. |
!> @endhistory
   CHARACTER(csz) FUNCTION combination_name(mn) RESULT(r)
      INTEGER, INTENT(IN) :: mn  !! Registered visualisation-item index.
      CHARACTER(8)        :: dum !! Metadata name and optional fraction suffix.
      WRITE(r,'(I3)')G_H5_I(mn,'users_number')
      dum = G_H5_C(mn,'name')
      IF(G_H5_L(mn,'varies_with_sediment')) THEN
         WRITE(dum,'(A,I2)') TRIM(dum), G_H5_I(mn,'nsed')
      ELSEIF(G_H5_L(mn,'varies_with_contaminant')) THEN
         WRITE(dum,'(A,I2)') TRIM(dum), G_H5_I(mn,'ncon')
      ENDIF
      r  = TRIM(r)//' '//TRIM(dum)
   END FUNCTION combination_name

!> Closes the HDF5 resources owned by the visualisation writer.
!>
!> This is the terminal operation for a run and assumes [[initialise]] has
!> completed. It closes each value and time dataset/dataspace, the shared time
!> dataspace, the top-level groups and file, and finally the HDF5 library.
!> Lazily created map and spreadsheet groups are closed unconditionally; normal
!> registration includes the static `surf_elv` and `number` items that create
!> them. Saved allocations and guards are not reset, so output cannot be
!> restarted safely in the same process.
!>
!> Dynamic per-item group identifiers and the two compression property lists
!> are left for `H5CLOSE_F` to release. Close failures are not propagated.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced HDF5 shutdown. |
!> | 2026-04-08 | SB | Removed the Intel `DLLEXPORT` directive. |
!> | 2026-04-14 | SvB | Closed the per-item and shared time dataspaces. |
!> @endhistory
   SUBROUTINE visualisation_tidy_up()
      INTEGER :: ni !! Number of registered visualisation items.
      INTEGER :: mn !! Current metadata-item index.
      LOGICAL :: istimeseries !! Whether this item owns a time dataset.
      ni           = G_I(0,'no_items')
      DO mn=1,ni
         istimeseries = G_H5_L(mn, 'istimeseries')
         CALL H5DCLOSE_F(dataset(mn), error)
         IF(istimeseries) CALL H5DCLOSE_F(t_dataset(mn), error)
         CALL H5SCLOSE_F(dataspace(mn), error)
         CALL H5SCLOSE_F(orig_dataspace(mn), error)
         CALL H5SCLOSE_F(t_dataspace(mn), error)
      ENDDO
      CALL H5SCLOSE_F(orig_t_dataspace, error)
      CALL H5GCLOSE_F(group_static, error)
      CALL H5GCLOSE_F(group_dynamic, error)
      CALL H5GCLOSE_F(group_images, error)
      CALL H5GCLOSE_F(group_magnified_integer, error)
      CALL H5FCLOSE_F(file, error)
      CALL H5CLOSE_F(error)
   END SUBROUTINE visualisation_tidy_up


!> Writes one registered visualisation item when its value is due.
!>
!> The public caller first invokes this routine once to arm the writer, then
!> registers all metadata before the second call triggers [[initialise]]. Static
!> items are accepted only at the exact sentinel `time == 0`; dynamic items
!> increment their own timestep, extend both datasets, count queued values and
!> pass a full buffer to [[write_mn]]. The current buffer length is one.
!>
!> `mn` must identify a registered item and dynamic `time` values are hours.
!> The retained `notflag` path is inactive after removal of the old Intel key
!> test. Exact default-real time comparisons are intentional legacy sentinels.
!>
!> @warning
!> The buffer test is equality with one, not a lower bound. If more than one
!> node is already queued, the datasets are extended by one timestep but the
!> queued nodes are not written on that call; ordinary callers therefore must
!> invoke the writer for every due value.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced deferred initialisation and buffered writes. |
!> | 2026-04-08 | SB | Replaced legacy pointer handling with `C_PTR` and removed the Intel-specific keyboard test. |
!> @endhistory
   SUBROUTINE save_visualisation_data_to_disk(mn, time)
      INTEGER, INTENT(IN) :: mn !! Registered visualisation-item index.
      INTEGER, PARAMETER  :: buffer_length_for_storage=1 !! Number of queued values written together.
      INTEGER             :: tc !! Number of values currently queued for the item.
      INTEGER             :: tstep !! One-based dynamic output index; unused for static writes.
      REAL, INTENT(IN)    :: time !! Simulation time in hours, or zero for a static value.
      LOGICAL, SAVE       :: one=T !! Guard that discards the pre-registration call.
      LOGICAL, SAVE       :: two=F !! Guard that initialises HDF5 on the next call.
      LOGICAL, SAVE       :: notflag=F !! Inactive legacy early-stop flag.
      TYPE(C_PTR)         :: first_ptr !! Head of the item's queued-value list.

      IF(notflag .AND. time>zero) THEN
         RETURN
      ELSEIF(one) THEN
         one = F
         two = T
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
         first_ptr = G_PTR(mn,'first')
         tc = TIME_COUNT(G_C(mn,'typ'), first_ptr)
      ENDIF
      IF(time==zero .OR. tc==buffer_length_for_storage) &
         CALL WRITE_MN(mn, tc, time==zero, tstep, G_H5_L(mn,'isreal'), &
         G_H5_I(mn,'szorder',jndim), G_H5_I(mn,'ilow'), &
         G_H5_I(mn,'jlow'), G_H5_I(mn,'klow'))

   END SUBROUTINE save_visualisation_data_to_disk



!> Copies queued values from the visualisation structure into HDF5.
!>
!> Dynamic writes select the newly extended hyperslab and store the matching
!> time value. Static writes use the original dataspace. The `GET_HDF5_R` and
!> `GET_HDF5_I` extractors consume each linked-list node and advance `first`,
!> which is written back to metadata after the loop. Real and integer values are
!> materialised in six-dimensional temporary arrays whose inactive extents are
!> one.
!>
!> The internal HDF5 dimension sequence is Fortran ordered; readers using the C
!> view see the reverse sequence. Static items named exactly `surf_elv` and
!> `number` additionally trigger the derived map and spreadsheet products.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced queued HDF5 value writes. |
!> | 2026-03-29 | SvB | Allocated temporary arrays from runtime dimensions to prevent invalid storage and memory corruption. |
!> | 2026-04-08 | SB | Replaced legacy integer addresses with `C_PTR`. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE write_mn(mn, amount, firstwrites, tstep, isreal, szorder, ilow, jlow, klow)
      INTEGER, INTENT(IN) :: mn !! Registered visualisation-item index.
      INTEGER, INTENT(IN) :: amount !! Number of queued nodes to copy.
      INTEGER, INTENT(IN) :: tstep !! Current one-based dynamic output index.
      INTEGER, INTENT(IN) :: ilow !! Lower column offset passed to the extractor.
      INTEGER, INTENT(IN) :: jlow !! Lower row offset passed to the extractor.
      INTEGER, INTENT(IN) :: klow !! Lower layer offset passed to the extractor.
      INTEGER, DIMENSION(:), INTENT(IN) :: szorder !! Mapping from metadata to storage dimensions.
      INTEGER                  :: am !! Queued-node counter.
      INTEGER                  :: hhdim(ndim) !! Metadata extents including inactive zero entries.
      TYPE(C_PTR)              :: first !! Current queued-node pointer; advanced by the extractor.
      INTEGER, DIMENSION(ndim) :: sz !! Extractor extents with inactive dimensions replaced by one.
      INTEGER(HSIZE_T)         :: t_sz(7) !! One-value time memory dimensions.
      REAL                     :: time !! Time in hours read from the current queued node.
      REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: surf_elv !! Surface-elevation value buffer.
      REAL, DIMENSION(:,:,:), ALLOCATABLE       :: temp_surf_map !! Map-shaped surface-elevation slice.
      REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: temp_r !! General real value buffer.
      INTEGER, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: temp_i !! General integer value buffer.
      LOGICAL, INTENT(IN) :: firstwrites !! True for the initial/static write path.
      LOGICAL, INTENT(IN) :: isreal !! True when this item's database values are real.
      LOGICAL             :: istimeseries !! Whether the item owns a time dataset.
      CHARACTER(2)        :: typ !! Visualisation structure type code.
      CHARACTER(csz)      :: name !! Metadata item name.
      INTEGER(HID_T)      :: filespace !! Value file dataspace for this write.
      INTEGER(HID_T)      :: t_filespace !! Time file dataspace for this write.
      INTEGER(HSIZE_T), DIMENSION(ndim) :: start !! Zero-based value hyperslab start.
      INTEGER(HSIZE_T), DIMENSION(ndim) :: t_start !! Zero-based time hyperslab start.
      INTEGER(HSIZE_T), DIMENSION(ndim) :: ccount !! Value hyperslab selection count.
      INTEGER(HSIZE_T), DIMENSION(ndim) :: t_ccount !! Time hyperslab selection count.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_HDF5:write_mn"

      ! setup
      name            = G_H5_C(mn,'name')
      first           = G_PTR(mn,'first')
      typ             = G_C(mn,'typ')
      istimeseries    = G_L(mn,'istimeseries')
      hhdim = G_H5_I(mn, 'dimensions', jndim)
      sz    = MAX(1,hhdim)

      IF(firstwrites) THEN
         CALL H5SCOPY_F(dataspace(mn), filespace, error)
         CALL H5SCOPY_F(t_dataspace(mn), t_filespace, error)
      ELSE
         CALL H5SCREATE_SIMPLE_F(rank(mn), newsz(mn)%a, filespace, error)
         CALL H5SCREATE_SIMPLE_F(1, t_newsz, t_filespace, error)
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
            time = GET_HDF5_TIME(typ, first)
            CALL H5DWRITE_F(t_dataset(mn), H5T_NATIVE_REAL, (/time/), &
               t_sz, error, mem_space_id=orig_t_dataspace, file_space_id=t_filespace)
         ENDIF

         ! The structure extractor consumes the current node and advances `first`.
         IF(isreal) THEN
            IF(name=='surf_elv') THEN
               IF(.NOT.ALLOCATED(surf_elv)) ALLOCATE(surf_elv(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)))
               CALL GET_HDF5_R(typ, sz, szorder, first, ilow, jlow, klow, surf_elv)
               CALL H5DWRITE_F(dataset(mn), dtype(mn), surf_elv, &
                  szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)
            ELSE
               IF(.NOT.ALLOCATED(temp_r)) ALLOCATE(temp_r(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)))
               CALL GET_HDF5_R(typ, sz, szorder, first, ilow, jlow, klow, temp_r)
               CALL H5DWRITE_F(dataset(mn), dtype(mn), temp_r, &
                  szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)
            ENDIF
         ELSE
            IF(.NOT.ALLOCATED(temp_i)) ALLOCATE(temp_i(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)))
            CALL GET_HDF5_I(typ, sz, szorder, first, ilow, jlow, klow, temp_i)
            CALL H5DWRITE_F(dataset(mn), dtype(mn), temp_i, &
               szz(mn)%a, error, mem_space_id=orig_dataspace(mn), file_space_id=filespace)
         ENDIF
      ENDDO
      CALL S_PTR(mn,'first', first)
      CALL H5SCLOSE_F(filespace, error)
      CALL H5SCLOSE_F(t_filespace, error)
      IF(ALLOCATED(temp_r)) DEALLOCATE(temp_r, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "temp_r", location, emsg)
      IF(ALLOCATED(temp_i)) DEALLOCATE(temp_i, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "temp_i", location, emsg)
      IF(name=='number') CALL SAVE_NUMBERS_AS_SPREADSHEET(mn)

      IF(name=='surf_elv') THEN
         ALLOCATE(temp_surf_map(sz(4), sz(5), sz(6)), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "temp_surf_map", location, emsg)
         temp_surf_map = surf_elv(1,1,1,:,:,:)
         CALL SAVE_SURF_ELEV_AS_MAP(mn, temp_surf_map, magnif=20)
         DEALLOCATE(temp_surf_map, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "temp_surf_map", location, emsg)
         DEALLOCATE(surf_elv, STAT=ios, ERRMSG=emsg)
         CALL errstat_dealloc(ios, "surf_elv", location, emsg)
      ENDIF
   END SUBROUTINE write_mn



!> Adds the `units = "hours"` attribute to an item's time dataset.
!>
!> The time dataset must already exist in `t_dataset(mn)`. Temporary HDF5
!> datatype, dataspace and attribute identifiers are closed before return.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced time units metadata. |
!> | 2026-04-07 | SvB | Added portable `SIZE_T`/`HSIZE_T` conversions and explicit identifier cleanup for GFortran. |
!> @endhistory
   SUBROUTINE create_time_attributes(mn)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: mn !! Registered visualisation-item index.
      INTEGER             :: arank !! Rank of the scalar-like attribute dataspace.
      INTEGER(HID_T)      :: atype !! Fixed-length character datatype identifier.
      INTEGER(HID_T)      :: attribute !! `units` attribute identifier.
      INTEGER(HID_T)      :: a_dataspace !! Attribute dataspace identifier.
      INTEGER(HSIZE_T)    :: dims1(1) !! One-element attribute extent.
      CHARACTER(5)        :: units_str = 'hours' !! Time-unit attribute value.

      CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
      CALL H5TSET_SIZE_F(atype, INT(5, KIND=SIZE_T), error)

      arank    = 1
      dims1(1) = 1

      CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
      CALL H5ACREATE_F(t_dataset(mn), 'units', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, units_str, dims1, error)

      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)
      CALL H5TCLOSE_F(atype, error)

   END SUBROUTINE create_time_attributes
!> Adds descriptive and dimension metadata to one value dataset.
!>
!> The attributes are `title`, `units`, `basis`, `scope`, `names of
!> dimensions`, `database type`, and the applicable per-dimension attributes
!> written by the contained `dimension_attributes` helper. Active names are reversed to
!> match the order exposed to C-oriented HDF5 readers. The database type stores
!> only the first character of the visualisation structure type code.
!>
!> | Dimension | Auxiliary attribute |
!> |:----------|:--------------------|
!> | `time` | Text noting that values have their own `time` dataset. |
!> | `column`, `row` | Inclusive lower and upper metadata limits. |
!> | `el-lst` | Two rows pairing local positions with element numbers. |
!> | `el_typ` | Element-type member labels. |
!> | `extra` | Extra-dimension member labels. |
!> | `layer` or unknown | None. |
!>
!> Zero-member element-type, element-list and extra dimensions receive a
!> one-entry placeholder so the HDF5 Fortran interface is never passed an empty
!> buffer. A metadata item with no active dimensions still requests a zero-length
!> `names of dimensions` attribute; any HDF5 failure is retained only in
!> `error`. No `layer limits` attribute is currently written.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced dataset and dimension attributes. |
!> | 2026-04-07 | SvB | Added portable HDF5 size kinds and closed local datatype identifiers. |
!> | 2026-04-14 | SvB | Guarded empty dimension-member arrays. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE create_variables_attributes(mn)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: mn !! Registered visualisation-item index.
      INTEGER             :: dd !! Active-dimension counter.
      INTEGER             :: ii !! Packed-dimension counter.
      INTEGER             :: jj !! Metadata member or dimension counter.
      INTEGER             :: no_dimensions !! Number of active metadata dimensions.
      INTEGER             :: arank !! Rank of the current attribute dataspace.
      INTEGER(HID_T)      :: atype !! Reusable character datatype identifier.
      INTEGER(HID_T)      :: attribute !! Current attribute identifier.
      INTEGER(HID_T)      :: a_dataspace !! Current attribute dataspace identifier.
      INTEGER             :: i !! Array-constructor index for element-list positions.
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: pairs !! Position/element-number pairs.
      CHARACTER(2)        :: typ !! Visualisation structure type code.
      CHARACTER(6), DIMENSION(:), ALLOCATABLE :: nme !! Dimension member labels.
      CHARACTER(6), DIMENSION(:), ALLOCATABLE :: nmed !! Active dimension names in file order.
      INTEGER(HSIZE_T) :: dims1(1) !! One-dimensional attribute extent.
      INTEGER(HSIZE_T) :: dims2(2) !! Two-dimensional attribute extents.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_HDF5:create_variables_attributes"

      CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
      CALL H5TSET_SIZE_F(atype, INT(csz, SIZE_T), error)

      arank    = 1
      dims1(1) = 1

      CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'title', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, G_H5_C(mn, 'title'), dims1, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      ! Units.
      CALL H5TSET_SIZE_F(atype, INT(8, SIZE_T), error)

      CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'units', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, G_H5_C(mn, 'units'), dims1, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      ! Basis.
      CALL H5TSET_SIZE_F(atype, INT(12, SIZE_T), error)

      CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'basis', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, G_H5_C(mn, 'basis'), dims1, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      ! Scope.
      CALL H5TSET_SIZE_F(atype, INT(7, SIZE_T), error)

      CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'scope', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, G_H5_C(mn, 'scope'), dims1, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      ! Active dimension names in the order seen by C-oriented readers.
      CALL H5TSET_SIZE_F(atype, INT(6, SIZE_T), error)

      no_dimensions = G_H5_I(mn, 'no_dimensions')
      dims1(1)      = INT(no_dimensions, HSIZE_T)

      ALLOCATE(nmed(no_dimensions), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "nmed", location, emsg)
      ii = 0
      DO jj = 1, ndim
         IF (G_H5_I(mn, 'dimensions', jj) /= 0) THEN
            ii = ii + 1
            nmed(ii) = G_H5_C(mn, 'names_of_dimensions', jj)
         END IF
      END DO

      nmed = nmed(no_dimensions:1:-1)

      CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'names of dimensions', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, nmed, dims1, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      DO dd = 1, no_dimensions
         CALL DIMENSION_ATTRIBUTES(nmed(dd))
      END DO

      DEALLOCATE(nmed, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "nmed", location, emsg)

      ! First character of the visualisation structure type code.
      CALL H5TSET_SIZE_F(atype, INT(1, SIZE_T), error)

      arank    = 1
      dims1(1) = 1
      typ      = G_H5_C(mn, 'typ')

      CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
      CALL H5ACREATE_F(dataset(mn), 'database type', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, typ(1:1), dims1, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      CALL H5TCLOSE_F(atype, error)

   CONTAINS

      !> Adds the auxiliary attribute associated with one active dimension.
      !>
      !> The mapping is:
      !>
      !> | Dimension | Attribute |
      !> |:----------|:----------|
      !> | `time` | Text noting that values have their own `time` dataset. |
      !> | `column`, `row` | Inclusive lower and upper metadata limits. |
      !> | `el-lst` | Two rows pairing local positions with element numbers. |
      !> | `el_typ` | Element-type member labels. |
      !> | `extra` | Extra-dimension member labels. |
      !>
      !> `layer` and unknown names deliberately add no auxiliary attribute.
      !> Empty member arrays use one blank or zero-filled placeholder entry.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2020-09-08 | SB | Introduced per-dimension metadata. |
      !> | 2026-04-07 | SvB | Isolated and closed string datatypes and ceased writing `layer limits`. |
      !> | 2026-04-14 | SvB | Guarded zero-length member arrays. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
      !> @endhistory
      SUBROUTINE dimension_attributes(name)
         CHARACTER(*), INTENT(IN) :: name !! Active dimension name in file order.
         CHARACTER(csz)           :: dum(1) !! Text value for the `time` attribute.
         INTEGER(HID_T)           :: local_atype !! Temporary fixed-length string datatype.
         INTEGER                  :: nvals !! Number of meaningful dimension members.

         INTEGER(KIND=I_P) :: ios
         CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
         CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_HDF5:dimension_attributes"

         SELECT CASE(name)

          CASE('time')
            arank    = 1
            dims1(1) = 1
            dum(1)   = 'has its own dataset'

            CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, local_atype, error)
            CALL H5TSET_SIZE_F(local_atype, INT(LEN_TRIM(dum(1)), SIZE_T), error)

            CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
            CALL H5ACREATE_F(dataset(mn), 'time', local_atype, a_dataspace, attribute, error)
            CALL H5AWRITE_F(attribute, local_atype, dum, dims1, error)
            CALL H5ACLOSE_F(attribute, error)
            CALL H5SCLOSE_F(a_dataspace, error)
            CALL H5TCLOSE_F(local_atype, error)

          CASE('column')
            arank    = 1
            dims1(1) = 2

            CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
            CALL H5ACREATE_F(dataset(mn), 'column limits', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
            CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, [G_H5_I(mn,'ilow'), G_H5_I(mn,'ihigh')], dims1, error)
            CALL H5ACLOSE_F(attribute, error)
            CALL H5SCLOSE_F(a_dataspace, error)

          CASE('row')
            arank    = 1
            dims1(1) = 2

            CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
            CALL H5ACREATE_F(dataset(mn), 'row limits', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
            CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, [G_H5_I(mn,'jlow'), G_H5_I(mn,'jhigh')], dims1, error)
            CALL H5ACLOSE_F(attribute, error)
            CALL H5SCLOSE_F(a_dataspace, error)

          CASE('el-lst')
            arank    = 2
            dims2(1) = 2
            nvals    = MAX(0, G_H5_I(mn, 'sz'))
            dims2(2) = MAX(1_HSIZE_T, INT(nvals, HSIZE_T))

            ALLOCATE(pairs(dims2(1), dims2(2)), STAT=ios, ERRMSG=emsg)
            CALL errstat_alloc(ios, "pairs", location, emsg)
            pairs = 0
            pairs(1,:) = [ (i, i = 1, INT(dims2(2))) ]
            IF(nvals>0) THEN
               DO jj = 1, nvals
                  pairs(2, jj) = G_H5_I(mn, 'list', jj)
               ENDDO
            ENDIF

            CALL H5SCREATE_SIMPLE_F(arank, dims2, a_dataspace, error)
            CALL H5ACREATE_F(dataset(mn), 'element nos.', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
            CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, pairs, dims2, error)
            CALL H5ACLOSE_F(attribute, error)
            CALL H5SCLOSE_F(a_dataspace, error)
            DEALLOCATE(pairs, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "pairs", location, emsg)

          CASE('el_typ')
            arank    = 1
            nvals    = MAX(0, G_H5_I(mn, 'no_mbr'))
            dims1(1) = MAX(1_HSIZE_T, INT(nvals, HSIZE_T))

            CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, local_atype, error)
            CALL H5TSET_SIZE_F(local_atype, INT(6, SIZE_T), error)

            ALLOCATE(nme(dims1(1)), STAT=ios, ERRMSG=emsg)
            CALL errstat_alloc(ios, "nme", location, emsg)
            nme = ''
            IF(nvals>0) THEN
               DO jj = 1, nvals
                  nme(jj) = G_H5_C(mn, 'el-typ', jj)
               ENDDO
            ENDIF

            CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
            CALL H5ACREATE_F(dataset(mn), 'element types', local_atype, a_dataspace, attribute, error)
            CALL H5AWRITE_F(attribute, local_atype, nme, dims1, error)
            CALL H5ACLOSE_F(attribute, error)
            CALL H5SCLOSE_F(a_dataspace, error)
            CALL H5TCLOSE_F(local_atype, error)
            DEALLOCATE(nme, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "nme", location, emsg)

          CASE('extra')
            arank    = 1
            nvals    = MAX(0, G_H5_I(mn, 'no_extra_dimensions'))
            dims1(1) = MAX(1_HSIZE_T, INT(nvals, HSIZE_T))

            CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, local_atype, error)
            CALL H5TSET_SIZE_F(local_atype, INT(6, SIZE_T), error)

            ALLOCATE(nme(dims1(1)), STAT=ios, ERRMSG=emsg)
            CALL errstat_alloc(ios, "nme", location, emsg)
            nme = ''
            IF(nvals>0) THEN
               DO jj = 1, nvals
                  nme(jj) = G_H5_C(mn, 'names_of_extra_dimensions', jj)
               ENDDO
            ENDIF

            CALL H5SCREATE_SIMPLE_F(arank, dims1, a_dataspace, error)
            CALL H5ACREATE_F(dataset(mn), 'extra', local_atype, a_dataspace, attribute, error)
            CALL H5AWRITE_F(attribute, local_atype, nme, dims1, error)
            CALL H5ACLOSE_F(attribute, error)
            CALL H5SCLOSE_F(a_dataspace, error)
            CALL H5TCLOSE_F(local_atype, error)
            DEALLOCATE(nme, STAT=ios, ERRMSG=emsg)
            CALL errstat_dealloc(ios, "nme", location, emsg)

         END SELECT

      END SUBROUTINE dimension_attributes
   END SUBROUTINE create_variables_attributes



!> Converts static surface elevation into a magnified indexed catchment map.
!>
!> The input is the map-shaped slice extracted from the `surf_elv` item. Palette
!> indices are computed by `GET_REAL_IMAGE_INDEX` using the item's row/column
!> extents and the requested magnification, then written as
!> `/CATCHMENT_MAPS/SV<ver>_elevation`. The current caller uses magnification 20.
!>
!> The `I1` version field supports only single-digit visualisation versions.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced the derived elevation map. |
!> | 2026-03-29 | SvB | Made the temporary image allocatable to prevent invalid storage and memory corruption. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE save_surf_elev_as_map(mn, dat, magnif)
      INTEGER, INTENT(IN) :: mn !! Registered index of the static `surf_elv` item.
      INTEGER, INTENT(IN) :: magnif !! Map magnification passed to the index generator.
      INTEGER             :: sz(2) !! Unmagnified column and row extents.
      REAL, DIMENSION(:,:,:), INTENT(IN) :: dat !! Surface elevation on the map grid.
      CHARACTER(csz) :: name !! HDF5 image dataset name.
      CHARACTER(csz) :: title !! Descriptive title passed to the image helper; currently unused there.
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: temp_pic !! Magnified palette-index image.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_HDF5:save_surf_elev_as_map"

      WRITE(name,'(A,I1,A)') 'SV',ver,'_elevation'
      WRITE(title,'(A,I1,A)') 'SV',ver,' surface elevation'
      sz  = szz(mn)%a(2:3)
      temp_pic = GET_REAL_IMAGE_INDEX(sz, dat, magnif, mn)
      CALL ADD_AN_IMAGE_TO_GROUP(name, title, magnif, pic=temp_pic)
      DEALLOCATE(temp_pic, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "temp_pic", location, emsg)
   END SUBROUTINE save_surf_elev_as_map

!> Converts static element numbers into a magnified integer grid.
!>
!> The grid is written as
!> `/CATCHMENT_SPREADSHEETS/SV<ver>_numbering` with a fixed magnification of 20.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced the derived element-number spreadsheet. |
!> | 2026-03-29 | SvB | Made the temporary magnified grid allocatable to prevent invalid storage and memory corruption. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE save_numbers_as_spreadsheet(mn)
      INTEGER, INTENT(IN) :: mn !! Registered index of the static `number` item.
      INTEGER, PARAMETER  :: magnif=20 !! Fixed spreadsheet magnification.
      INTEGER             :: sz(2) !! Unmagnified column and row extents.
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: temp_magarr !! Magnified element-number grid.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_HDF5:save_numbers_as_spreadsheet"

      sz = szz(mn)%a(2:3)
      temp_magarr = GET_MAGNIFIED_SU_ARR(sz, magnif, mn)
      CALL ADD_MAGNIFIED_INTEGER_SPREADSHEET_TO_GROUP(mn, nme='numbering', magnif=magnif, magarr=temp_magarr)
      DEALLOCATE(temp_magarr, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "temp_magarr", location, emsg)
   END SUBROUTINE save_numbers_as_spreadsheet


!> Adds an indexed elevation image and its colour palette to the map group.
!>
!> On the first call the routine creates `/CATCHMENT_MAPS` and constructs a
!> 256-entry RGB palette. It writes `pic` as a native-integer HDF5 dataset,
!> creates `palette1`, and links that palette to the indexed image. Despite the
!> helper's historical name, the current GFortran output stores 32-bit integer
!> indices rather than an eight-bit dataset.
!>
!> For entries `i = 1,...,256`, the base RGB sequence is
!> `(MIN(255,4*i/3), i, i/2)` using integer division. The first entry is then
!> set to `(5,125,125)` and the last to `(80,125,255)`.
!>
!> `pic` is declared optional for historical reasons but is dereferenced
!> unconditionally and is therefore required in practice. `title` and `magnif`
!> are also retained interface arguments but are not written as attributes.
!> Palette data is initialised only on the first call while palette creation is
!> attempted on every call, so this helper currently supports one image per
!> output file.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced indexed map and palette output. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE add_an_image_to_group(name, title, magnif, pic)
      INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: pic !! Magnified palette indices; required in practice.
      INTEGER, INTENT(IN) :: magnif !! Retained magnification argument; currently unused.
      INTEGER, PARAMETER  :: mmax=256 !! Number of entries in the colour palette.
      INTEGER, PARAMETER  :: vrange(2)=[0,mmax] !! Unused range retained from the superseded image writer.
      INTEGER :: i !! Palette-constructor index.
      INTEGER :: p !! Unused legacy address variable.
      INTEGER :: minvi, maxvi !! Unused legacy integer range.
      INTEGER :: arank, st !! Unused legacy HDF5 workspace.
      INTEGER(HID_T) :: dataspace, atype, attribute, a_dataspace, dataset !! Unused legacy HDF5 identifiers.
      INTEGER(HSIZE_T), DIMENSION(1) :: tsz !! Unused legacy attribute extent.
      CHARACTER(*), INTENT(IN) :: name !! HDF5 image dataset name.
      CHARACTER(*), INTENT(IN) :: title !! Retained image title; currently unused.
      TYPE(ssz) :: aszz !! Unused legacy extent wrapper allocated on the first call.
      REAL :: minvr, maxvr !! Unused legacy real range.
      LOGICAL, SAVE :: first = .TRUE. !! Guard for group and palette-data initialisation.
      INTEGER(HSIZE_T) :: wid !! First extent of `pic`.
      INTEGER(HSIZE_T) :: hei !! Second extent of `pic`.
      CHARACTER(*), PARAMETER :: pal_name = "palette1" !! Palette dataset name.
      INTEGER(HSIZE_T), DIMENSION(2) :: pal_dims = [mmax,3] !! Palette entry and RGB-component extents.
      INTEGER, DIMENSION(mmax*3) :: pal_data_in !! Flattened RGB palette values.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_HDF5:add_an_image_to_group"

      IF(first) THEN
         pal_data_in                = [(MIN(mmax-1,4*i/3),i,i/2,i=1,mmax)]
         pal_data_in((MMAX-1)*3+1:) = [80,125,255]
         pal_data_in(1:3)           = [5,125,125]
         CALL H5GCREATE_F(file, 'CATCHMENT_MAPS', group_images, error)
         ALLOCATE(aszz%a(2), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "aszz%a", location, emsg)
         FIRST = .FALSE.
      ENDIF

      wid = SIZE(pic,DIM=1)
      hei = SIZE(pic,DIM=2)

      CALL make_tidy_image_8(group_images, name, wid, hei,  pic, error)
      CALL h5IMmake_palette_F(group_images, pal_name, pal_dims, pal_data_in, error)
      CALL H5IMlink_palette_f(group_images, name, pal_name, error)
   END SUBROUTINE add_an_image_to_group

!> Writes a native-integer indexed-image dataset with HDF5 image attributes.
!>
!> `H5LTmake_dataset_int_f` creates a rank-two native-integer dataset, after
!> which `CLASS=IMAGE`, `IMAGE_VERSION=1.2`, and
!> `IMAGE_SUBCLASS=IMAGE_INDEXED` are attached. The name is historical: this
!> routine does not request an eight-bit datatype. `pic` is optional in the
!> interface but required in practice because it is passed unconditionally.
!>
!> `err` contains only the status of the last H5LT attribute call; earlier
!> failures can be overwritten.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced the compact indexed-image writer. |
!> @endhistory
   SUBROUTINE make_tidy_image_8(loc_id, name, wid, hei, pic, err)
      INTEGER, PARAMETER :: rank=2 !! Dataset rank.
      INTEGER, INTENT(OUT) :: err !! Most recent H5LT status code.
      INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: pic !! Palette indices; required in practice.
      INTEGER(HID_T), INTENT(IN) :: loc_id !! Parent HDF5 group identifier.
      INTEGER(HSIZE_T), INTENT(IN) :: wid !! First dataset extent.
      INTEGER(HSIZE_T), INTENT(IN) :: hei !! Second dataset extent.
      INTEGER(HSIZE_T), DIMENSION(rank) :: dims !! Native HDF5 dataset extents.
      CHARACTER(*), INTENT(IN) :: name !! HDF5 image dataset name.

      dims = [wid,hei]
      err  = 0
      CALL H5LTmake_dataset_int_f(loc_id, name, 2, dims, pic, err)

      CALL H5LTset_attribute_string_f(loc_id, name, "CLASS", "IMAGE", err)
      CALL H5LTset_attribute_string_f(loc_id, name, "IMAGE_VERSION", "1.2", err)
      CALL H5LTset_attribute_string_f(loc_id, name, "IMAGE_SUBCLASS", "IMAGE_INDEXED", err )

   END SUBROUTINE make_tidy_image_8
!> Writes a magnified integer grid to `/CATCHMENT_SPREADSHEETS`.
!>
!> The group is created on the first call. The dataset name is
!> `SV<ver>_<nme>`, its full shape is used as a DEFLATE chunk, and it receives a
!> title plus the integer magnification attribute. The `I0` version field avoids
!> overflow for multi-digit versions. `mn` is retained for interface symmetry
!> but is not used.
!>
!> The copied string datatype `atype` is not explicitly closed; the final
!> `H5CLOSE_F` call releases it with other remaining HDF5 resources.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Introduced magnified integer-grid output. |
!> | 2026-04-07 | SvB | Added portable dimension casts, multi-digit versions, and dataset/dataspace cleanup. |
!> @endhistory
   SUBROUTINE add_magnified_integer_spreadsheet_to_group(mn, nme, magnif, magarr)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: mn !! Retained visualisation-item index; currently unused.
      INTEGER, INTENT(IN) :: magnif !! Grid magnification stored as an attribute.
      INTEGER, INTENT(IN) :: magarr(:,:) !! Magnified integer grid.
      CHARACTER(*), INTENT(IN) :: nme !! Dataset-name suffix.
      INTEGER(HID_T) :: dataspace !! Grid dataspace identifier.
      INTEGER(HID_T) :: atype !! Fixed-length title datatype identifier.
      INTEGER(HID_T) :: attribute !! Current attribute identifier.
      INTEGER(HID_T) :: a_dataspace !! Current attribute dataspace identifier.
      INTEGER(HID_T) :: dataset !! Grid dataset identifier.
      INTEGER :: arank !! Dataset or attribute rank.
      INTEGER(HSIZE_T) :: dims(2) !! Grid dataset extents.
      INTEGER(HSIZE_T) :: adims(1) !! One-element attribute extent.
      CHARACTER(csz) :: title !! Dataset title attribute.
      CHARACTER(csz) :: name !! Versioned dataset name.
      LOGICAL, SAVE :: first = .TRUE. !! Guard for spreadsheet-group creation.

      IF (first) THEN
         first = .FALSE.
         CALL H5GCREATE_F(file, 'CATCHMENT_SPREADSHEETS', group_magnified_integer, error)
      END IF

      WRITE(name, '(A,I0,A)') 'SV', ver, '_' // TRIM(nme)
      title = name

      arank = 2
      dims(1) = INT(SIZE(magarr, 1), HSIZE_T)
      dims(2) = INT(SIZE(magarr, 2), HSIZE_T)

      CALL H5SCREATE_SIMPLE_F(arank, dims, dataspace, error)
      CALL H5PSET_CHUNK_F(dataset_compress_property, arank, dims, error)

      CALL H5DCREATE_F(group_magnified_integer, name, H5T_NATIVE_INTEGER, dataspace, &
         dataset, error, dcpl_id=dataset_compress_property)

      CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype, error)
      CALL H5TSET_SIZE_F(atype, INT(csz, SIZE_T), error)

      arank = 1
      adims(1) = 1

      CALL H5SCREATE_SIMPLE_F(arank, adims, a_dataspace, error)
      CALL H5ACREATE_F(dataset, 'title', atype, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, atype, title, adims, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      CALL H5SCREATE_SIMPLE_F(arank, adims, a_dataspace, error)
      CALL H5ACREATE_F(dataset, 'magnification', H5T_NATIVE_INTEGER, a_dataspace, attribute, error)
      CALL H5AWRITE_F(attribute, H5T_NATIVE_INTEGER, magnif, adims, error)
      CALL H5ACLOSE_F(attribute, error)
      CALL H5SCLOSE_F(a_dataspace, error)

      CALL H5DWRITE_F(dataset, H5T_NATIVE_INTEGER, magarr, dims, error)

      CALL H5DCLOSE_F(dataset, error)
      CALL H5SCLOSE_F(dataspace, error)

   END SUBROUTINE add_magnified_integer_spreadsheet_to_group



END MODULE visualisation_hdf5
