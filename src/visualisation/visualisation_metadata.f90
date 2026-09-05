!> @brief Owns the visualisation catalogue, plan parser, and HDF5 metadata view.
!>
!> The module builds two related catalogues. Private [[item]] records hold the
!> model-facing state used by [[visualisation_interface_right]] while values are
!> recorded. Public [[hdf5_item]] records are the dimension-oriented projection
!> consumed by [[visualisation_hdf5]]. Masks, element lists, and time schedules
!> are shared through pointer components rather than copied into every item.
!>
!> The normal one-shot lifecycle is:
!>
!> | Stage | Entry point | Effect |
!> |:------|:------------|:-------|
!> | Register constants | [[register_static_visualisation_metadata]] | Appends whole-grid static items. |
!> | Publish variables | [[register_dynamic_visualisation_metadata]], `jj=1` | Reports implemented choices. |
!> | Read plan | First dynamic call with `jj/=1` | Creates requested items and referenced records. |
!> | Match variables | Remaining `jj/=1` calls | Adds catalogue attributes and maps requested basis/scope to a storage type. |
!> | Finalize | Call with `final=.TRUE.` | Validates every request and builds `hdf5_items`. |
!> | Record/write | Getters and pointer setters | Exposes schedules, selectors, dimensions, and buffer handles. |
!>
!> Visualisation plan blocks are introduced by these exact lowercase keywords:
!>
!> | Keyword | Action |
!> |:--------|:-------|
!> | `item` | Reads one output request and its selectors. |
!> | `list` | Reads explicit element numbers and derives square, bank, and river subsets. |
!> | `mask` | Reads a grid mask and derives all/square/bank/river lists. |
!> | `time` | Reads output-step and stop-time pairs in hours. |
!> | `diag` | Enables verbose plan diagnostics. |
!> | `kill` | Stops after parsing so the check file can be inspected. |
!> | `stop` | Completes parsing and continues initialization. |
!>
!> | Selector | Accepted values |
!> |:---------|:----------------|
!> | Basis | `grid_as_grid`, `grid_as_list`, `list_as_list` |
!> | Scope | `all`, `squares`, `banks`, `rivers` |
!> | Extra dimension | `-`, `faces`, `X_Y`, `left_right` |
!>
!> The HDF5 projection always allocates six fixed slots: time (1), extra (2),
!> layer (3), element member (4), column (5), and row/list (6). A singleton
!> optional axis is represented by dimension zero and omitted from the HDF5
!> rank. `szorder` maps logical traversal order back to these fixed slots.
!>
!> @warning
!> This is process-lifetime, one-shot state. Saved first-call flags, global
!> counters, pointer arrays, schedules, and HDF5 projections have no reset or
!> cleanup path. Re-registration, concurrent use, or a second simulation in the
!> same process is unsupported.
!> @endwarning
!>
!> @warning
!> `times` and `sstatic` have no explicit `=>NULL()` initialization in current
!> source. Their first allocation/`ASSOCIATED` use therefore relies on the
!> compiler/runtime treating module pointer storage as initially disassociated;
!> the Fortran source does not establish that association status.
!> @endwarning
!>
!> @warning
!> Plan and lookup errors call
!> [[visualisation_read:error_visualisation]], which prints diagnostics and
!> executes `STOP`. The parser also deliberately stops on the `kill` keyword.
!> Allocation and I/O failures outside those explicit checks are not handled.
!> @endwarning
!>
!> @note
!> Fortran applies the bare `PRIVATE` statement below to the whole module. The
!> current FORD parser applies default accessibility in source order and may
!> label earlier private state as public; compiled visibility is defined by the
!> explicit public list.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Created the central visualisation metadata handler. |
!> | 2020-09-08 | SB | - | Added the visualisation sources to the repository. |
!> | 2026-03-29 | SvB | - | Replaced integer buffer addresses with `C_PTR` and made portability updates. |
!> | 2026-04-03 | SvB | - | Reworked character constructors, formats, and the nested list helper for current compilers. |
!> | 2026-04-14 | SvB | - | Added allocation guards needed by GFortran. |
!> | 2026-05-02 | SvB | - | Removed unresolved merge-conflict duplicates while retaining current list filtering. |
!> | 2026-07-12 | SvB | - | Inlined the four typed array-growth helpers after include-file removal. |
!> @endhistory
MODULE visualisation_metadata

   USE ISO_C_BINDING, ONLY: C_PTR, C_NULL_PTR
   USE VISUALISATION_PASS,      ONLY : SU_NUMBER, BANK_NO, RIVER_NO, EXISTS, nel, &
      IS_SQUARE, IS_BANK, IS_LINK, TOP_CELL, DIRQQ, nsed, ncon, &
      planfile, checkfile
   USE VISUALISATION_READ,      ONLY : vp_in, vp_out, mess, mess2, mess3, error_visualisation, R_C, R_I, R_R, COPY
   USE VISUALISATION_STRUCTURE, ONLY : MBR_COUNT, GET_MBR, csz

   USE MOD_PARAMETERS, ONLY : I_P
   USE MOD_ERROR, ONLY : errstat_alloc

   IMPLICIT NONE

   INTEGER, PARAMETER                    :: ndim = 6 !! Number of fixed slots in every HDF5 metadata record.
   REAL, DIMENSION(:), ALLOCATABLE, SAVE :: previous_time !! Last scheduled time accepted for each item, in hours.
   REAL, DIMENSION(:), ALLOCATABLE, SAVE :: next_time !! Next scheduled time for each item, in hours.
   LOGICAL, PARAMETER                    :: T = .TRUE.  !! Legacy shorthand for true.
   LOGICAL, PARAMETER                    :: F = .FALSE. !! Legacy shorthand for false.
   REAL, PARAMETER                       :: zero = 0.0  !! Initial scheduler time, in hours.

!> Piecewise output schedule read from a plan `time` block.
   TYPE ttime
      PRIVATE
      INTEGER :: number !! User-visible time-block number.
      INTEGER :: sz     !! Number of user-supplied interval/stop pairs.
      REAL, DIMENSION(:), POINTER :: tstep=>NULL() !! Output interval for each segment, plus a final sentinel.
      REAL, DIMENSION(:), POINTER :: tstop=>NULL() !! Segment stop time, plus a final sentinel.
   END TYPE ttime
   TYPE(ttime), DIMENSION(:), POINTER :: times   !! Dynamic time blocks; current declaration lacks `=>NULL()`.
   TYPE(ttime), POINTER               :: sstatic !! Shared static schedule; current declaration lacks `=>NULL()`.

!> Explicit or derived list of SHETRAN element numbers.
   TYPE llist
      PRIVATE
      INTEGER                        :: number !! User list/mask number for an original list; undefined for derived subsets.
      INTEGER                        :: sz=0   !! Number of sorted unique positive elements in `a`.
      INTEGER                        :: indx=0 !! Internal index assigned to an explicitly read list.
      CHARACTER(12)                  :: basis  !! Basis selector; not initialized for every derived list.
      CHARACTER(7)                   :: scope  !! `all`, `squares`, `banks`, or `rivers`.
      INTEGER, DIMENSION(:), POINTER :: a      !! Owned sorted element-number array.
   END TYPE llist
   TYPE(LLIST), DIMENSION(:), POINTER :: lists=>NULL() !! Original lists followed by their three derived subsets.

!> Rectangular plan mask and the offset of its derived-list group.
   TYPE mask
      PRIVATE
      INTEGER                          :: number !! User-visible mask number.
      INTEGER                          :: ilow   !! Inclusive first display column.
      INTEGER                          :: ihigh  !! Inclusive last display column.
      INTEGER                          :: jlow   !! Inclusive first display row.
      INTEGER                          :: jhigh  !! Inclusive last display row.
      INTEGER                          :: listno !! Index of the mask's derived `all` list.
      LOGICAL, DIMENSION(:,:), POINTER :: ma     !! Effective mask after excluding zero `SU_NUMBER` cells.
   END TYPE mask
   TYPE(MASK), DIMENSION(:), POINTER :: masks=>NULL()      !! User masks read from the plan.
   TYPE(MASK), POINTER               :: whole_grid=>NULL() !! Shared all-true mask for static items.

!> Model-facing metadata and buffer handles for one requested output item.
   TYPE item
      PRIVATE
      INTEGER :: users_number=0              !! User item number; static items retain zero.
      INTEGER :: users_no_for_link_or_mask=0 !! Referenced mask/list number.
      INTEGER :: users_no_for_times=0        !! Referenced time-block number.
      INTEGER :: sediment_no=0               !! Selected sediment fraction or zero.
      INTEGER :: contaminant_no=0            !! Selected contaminant or zero.
      TYPE(C_PTR) :: first = C_NULL_PTR       !! First structure-buffer node for this item.
      TYPE(C_PTR) :: latest = C_NULL_PTR      !! Latest structure-buffer node for this item.
      CHARACTER(8)         :: name=''         !! Exact catalogue selector.
      CHARACTER(2)         :: typ=''          !! Structure storage code, normally ending in `S`.
      CHARACTER(csz)       :: title='*S'      !! Plot/check-file title.
      CHARACTER(8)         :: units=''        !! Units label.
      CHARACTER(12)        :: basis='grid_as_grid' !! Grid/list representation selector.
      CHARACTER(7)         :: scope='all'     !! Element-class selector.
      CHARACTER(11)        :: extra_dimensions = '-' !! Extra-axis selector.
      LOGICAL              :: isgrid = F      !! Whether spatial data use a rectangular grid.
      LOGICAL              :: istimeseries = F !! Whether the item has a time axis.
      LOGICAL              :: varies_with_sediment=F !! Whether `sediment_no` is required.
      LOGICAL              :: varies_with_contaminant=F !! Whether `contaminant_no` is required.
      LOGICAL              :: implemented=F   !! Whether the interface catalogue supports the requested name.
      INTEGER              :: layers(2)=(/0,0/) !! Inclusive requested layer bounds; zero removes the layer axis.
      TYPE(MASK), POINTER  :: amask=>NULL()    !! Resolved mask for gridded output.
      TYPE(LLIST), POINTER :: alist=>NULL()    !! Resolved list for list output.
      TYPE(TTIME), POINTER :: atime=>NULL()    !! Resolved output schedule.
   END TYPE item
   TYPE(ITEM), DIMENSION(:), POINTER :: items=>NULL() !! Static items followed by parsed dynamic requests.

!> HDF5-facing projection of one model-facing [[item]].
   TYPE hdf5_item
      INTEGER :: users_number = 0              !! User item number.
      INTEGER :: users_no_for_link_or_mask = 0 !! Referenced user mask/list number.
      INTEGER :: users_no_for_times=0          !! Referenced user time-block number.
      INTEGER :: ilow = 0                      !! Inclusive first column or list position.
      INTEGER :: ihigh = 0                     !! Inclusive last column or list position.
      INTEGER :: jlow = 0                      !! Inclusive first row for a grid.
      INTEGER :: jhigh = 0                     !! Inclusive last row for a grid.
      INTEGER :: klow = 0                      !! Inclusive first layer.
      INTEGER :: khigh = 0                     !! Inclusive last layer.
      INTEGER :: no_extra_dimensions = 0       !! Physical extra-axis size; one is later suppressed as a dimension.
      INTEGER :: tstep_no = 1                  !! One-based HDF5 time-record counter.
      INTEGER :: sz = 0                        !! Number of element entries for list output.
      INTEGER :: sediment_no = 0               !! Selected sediment fraction or zero.
      INTEGER :: contaminant_no = 0             !! Selected contaminant or zero.
      INTEGER, DIMENSION(:), POINTER :: dimensions !! Sizes in the six fixed HDF5 slots.
      INTEGER, DIMENSION(:), POINTER :: szorder    !! Logical traversal axes expressed as fixed-slot indices.
      INTEGER, DIMENSION(:), POINTER :: list       !! Copied element numbers for list output.
      CHARACTER(8)         :: name=''              !! Catalogue selector.
      CHARACTER(2)         :: typ=''               !! Structure storage code.
      CHARACTER(csz)       :: title='*S'           !! Plot/check-file title.
      CHARACTER(8)         :: units=''             !! Units label.
      CHARACTER(12)        :: basis='grid_as_grid' !! Grid/list representation selector.
      CHARACTER(7)         :: scope='all'          !! Element-class selector.
      CHARACTER(11)        :: extra_dimensions = '-' !! Extra-axis selector.
      CHARACTER(6), DIMENSION(:), POINTER :: names_of_extra_dimensions !! Extra-axis member labels.
      CHARACTER(6), DIMENSION(:), POINTER :: names_of_dimensions       !! Labels for all six fixed slots.
      CHARACTER(6), DIMENSION(:), POINTER :: mbr                       !! Square/bank/river member labels.
      LOGICAL :: isgrid = F                    !! Whether spatial data use column and row axes.
      LOGICAL :: istimeseries = F              !! Whether fixed slot 1 is active.
      LOGICAL :: isreal = T                    !! Whether the structure stores real rather than integer values.
      LOGICAL :: varies_with_sediment=F        !! Whether a sediment selector was applied.
      LOGICAL :: varies_with_contaminant=F     !! Whether a contaminant selector was applied.

   END TYPE hdf5_item
   TYPE(HDF5_ITEM), DIMENSION(:), POINTER :: hdf5_items=>NULL() !! Public HDF5 projection, built once at finalization.



   INTEGER                  :: no_times=0        !! Number of user time blocks.
   INTEGER                  :: no_lists=0        !! Number of original and derived element lists.
   INTEGER                  :: no_masks=0        !! Number of user masks.
   INTEGER                  :: no_items=0        !! Total static and dynamic item count.
   INTEGER                  :: no_static_items=0 !! Static prefix length within `items`.
   INTEGER, PARAMETER       :: sp=50             !! Legacy check-file separator/indentation width.
   REAL, PARAMETER          :: small = 0.001     !! Scheduler comparison tolerance in hours (3.6 seconds).
   CHARACTER(4), PARAMETER  :: keywords(7) = &
      [character(len=4) :: 'item', 'list', 'mask', 'time', 'stop', 'kill', 'diag'] !! Plan block keywords.
   CHARACTER(12), PARAMETER :: basis(3) = &
      [character(len=12) :: 'grid_as_grid', 'grid_as_list', 'list_as_list'] !! Accepted basis selectors.
   CHARACTER(7), PARAMETER  :: scope(4) = &
      [character(len=7) :: 'all', 'squares', 'banks', 'rivers'] !! Accepted element scopes.
   CHARACTER(11), PARAMETER :: extra_dimensions(4) = &
      [character(len=11) :: '-', 'faces', 'X_Y', 'left_right'] !! Accepted extra-axis selectors.
   LOGICAL                  :: diagnostics=F !! Whether verbose plan-parser messages are written.


   PRIVATE
   PUBLIC :: REGISTER_STATIC_VISUALISATION_METADATA,         &
      REGISTER_DYNAMIC_VISUALISATION_METADATA,        &
      GET_METADATA_C, GET_METADATA_L, GET_METADATA_I, &
      GET_METADATA_PTR, SET_METADATA_PTR,             &
      TIME_TO_RECORD,                                 &
      HDF5_ITEM, HDF5_ITEMS, ndim,                    &
      GET_METADATA_HDF5_I, GET_METADATA_HDF5_L, GET_METADATA_HDF5_C, &
      INCREMENT_HDF5_TSTEP_NO, csz

CONTAINS


!> @brief Advances one HDF5 item's one-based output-record counter.
!>
!> No bounds or allocation check is made; `hdf5_items` must already have been
!> built by final dynamic registration.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added per-item HDF5 timestep accounting. |
!> @endhistory
   SUBROUTINE INCREMENT_HDF5_TSTEP_NO(mn)
      INTEGER, INTENT(IN) :: mn !! One-based HDF5 item index.
      hdf5_items(mn)%tstep_no = hdf5_items(mn)%tstep_no + 1
   END SUBROUTINE INCREMENT_HDF5_TSTEP_NO


!> @brief Reports whether an item is due at the supplied simulation time.
!>
!> On its first call the routine allocates schedule state for the final
!> `no_items` catalogue and obtains every first due time from [[get_next_time]].
!> Time zero is always recorded. A later time is due when it reaches the next
!> schedule within `small=0.001` hours; accepting it advances that item by one
!> scheduled interval.
!>
!> @warning
!> A call that jumps over several due times advances only one interval. Later
!> calls remain due until the stored schedule catches up. The saved arrays are
!> never resized or reset, so all item registration must be complete first.
!> Exact comparison with real zero is intentional current behavior.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added per-item piecewise output scheduling. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   LOGICAL FUNCTION time_to_record(n, time) RESULT(r)
      INTEGER, INTENT(IN) :: n    !! One-based model-facing item index.
      REAL, INTENT(IN)    :: time !! Current simulation time in hours.
      INTEGER             :: i    !! Array-constructor index used during first-call initialization.
      LOGICAL, SAVE       :: first = T !! First-call initialization guard.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_METADATA:time_to_record"

      IF(first) THEN
         first = F
         ALLOCATE(previous_time(no_items), STAT=ios)
         CALL errstat_alloc(ios, "previous_time", location)
         ALLOCATE(next_time(no_items), STAT=ios)
         CALL errstat_alloc(ios, "next_time", location)
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



!> @brief Computes the next scheduled output time for one item.
!>
!> The first stop time strictly greater than `previous_time(n)` selects the
!> active schedule segment. The result is the earlier of that segment's stop
!> and one interval after the previous time. [[read_time]] appends a `HUGE`
!> sentinel pair so a normally constructed schedule terminates the search.
!>
!> @warning
!> Schedule stops and steps are not validated. Non-increasing stops, nonpositive
!> steps, an unresolved schedule pointer, or a manually built schedule without
!> the sentinel can produce a stalled schedule or an out-of-bounds lookup.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added piecewise next-time calculation. |
!> @endhistory
   ELEMENTAL REAL FUNCTION get_next_time(n) RESULT(r)
      INTEGER, INTENT(IN) :: n !! One-based model-facing item index.
      INTEGER             :: j !! Active time-segment index.
      j = 0
      DO
         j = j + 1
         IF(items(n)%atime%tstop(j)>previous_time(n)) EXIT
      ENDDO
      r = MIN(items(n)%atime%tstop(j), previous_time(n) + items(n)%atime%tstep(j))
   END FUNCTION get_next_time


!> @brief Returns one character property from a model-facing item.
!>
!> Exact selectors are `basis`, `name`, `title`, `typ`, `units`, `scope`, and
!> `extra_dimensions`. An unknown selector returns a diagnostic string rather
!> than stopping. The caller must supply a valid one-based item index.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added model-facing character metadata lookup. |
!> @endhistory
   PURE FUNCTION get_metadata_c(i, text) RESULT(r)
      INTEGER, INTENT(IN)      :: i    !! One-based model-facing item index.
      CHARACTER(*), INTENT(IN) :: text !! Exact property selector.
      CHARACTER(csz)           :: r    !! Selected value or diagnostic text.
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



!> @brief Returns one character property from an HDF5-facing item.
!>
!> Scalar selectors are `basis`, `name`, `title`, `typ`, `units`, and `scope`.
!> `el-typ`, `names_of_dimensions`, and `names_of_extra_dimensions` require the
!> optional member index `e`. Unknown selectors return diagnostic text; invalid
!> item/member indices are unchecked.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added HDF5-facing character metadata lookup. |
!> @endhistory
   ELEMENTAL FUNCTION get_metadata_HDF5_c(i, text, e) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! One-based HDF5 item index.
      INTEGER, INTENT(IN), OPTIONAL :: e    !! Required member index for array-valued selectors.
      CHARACTER(*), INTENT(IN)      :: text !! Exact property selector.
      CHARACTER(csz)                :: r    !! Selected value or diagnostic text.
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



!> @brief Returns one integer property from a model-facing item.
!>
!> Supported selectors are `ext`, spatial/list bounds (`ilow`, `ihigh`, `jlow`,
!> `jhigh`), layer bounds (`klow`, `khigh`), `no_items`, `sz`, `nsed`, `ncon`,
!> and `su`. Grid bounds come from the resolved mask; list bounds are `1:sz`.
!> Selector `su` requires the optional list position `su`. Unknown selectors
!> return `HUGE(1)`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added model-facing integer metadata lookup. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION get_metadata_i(i, text, su) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! One-based model-facing item index.
      INTEGER, INTENT(IN), OPTIONAL :: su   !! List position required by selector `su`.
      CHARACTER(*), INTENT(IN)      :: text !! Exact property selector.
      SELECT CASE(text)
       CASE('ext')      ; r=NO_EXTRA_DIMENSIONS(items(i)%extra_dimensions)

       CASE('ilow')     ; IF(items(i)%isgrid) THEN ; r=items(i)%amask%ilow  ; ELSE ; r=1                 ; ENDIF
       CASE('ihigh')    ; IF(items(i)%isgrid) THEN ; r=items(i)%amask%ihigh ; ELSE ; r=items(i)%alist%sz ; ENDIF
       CASE('jlow')     ; IF(items(i)%isgrid) THEN ; r=items(i)%amask%jlow  ; ELSE ; r=1                 ; ENDIF
       CASE('jhigh')    ; IF(items(i)%isgrid) THEN ; r=items(i)%amask%jhigh ; ELSE ; r=1                 ; ENDIF
       CASE('klow')     ; r=items(i)%layers(1)
       CASE('khigh')    ; r=items(i)%layers(2)
       CASE('no_items') ; r=no_items
       CASE('su')       ; r=items(i)%alist%a(su)
       CASE('sz')       ; r=items(i)%alist%sz
       CASE('nsed')     ; r=items(i)%sediment_no
       CASE('ncon')     ; r=items(i)%contaminant_no
       CASE DEFAULT     ; r=HUGE(1)
      END SELECT
   END FUNCTION get_metadata_i



!> @brief Returns a structure-buffer handle from a model-facing item.
!>
!> Exact selectors `first` and `latest` return the corresponding `C_PTR`.
!> Unknown selectors return `C_NULL_PTR`. Optional `su` is a retained legacy
!> argument and is not used.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added buffer-handle lookup using the compiler's integer pointer representation. |
!> | 2026-03-29 | SvB | Replaced integer addresses with interoperable `C_PTR` handles. |
!> @endhistory
   ELEMENTAL FUNCTION get_metadata_ptr(i, text, su) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! One-based model-facing item index.
      INTEGER, INTENT(IN), OPTIONAL :: su   !! Retained unused legacy selector index.
      CHARACTER(*), INTENT(IN)      :: text !! Exact handle selector.
      TYPE(C_PTR)                    :: r    !! Selected handle or `C_NULL_PTR`.
      SELECT CASE(text)
       CASE('first') ; r=items(i)%first
       CASE('latest') ; r=items(i)%latest
       CASE DEFAULT ; r=C_NULL_PTR
      END SELECT
   END FUNCTION get_metadata_ptr



!> @brief Returns one integer property from an HDF5-facing item.
!>
!> Scalar selectors expose bounds, counts, user selectors, item count, and the
!> HDF5 timestep counter. `dimensions`, `list`, and `szorder` require optional
!> index `e`; `no_mbr` and `no_dimensions` derive array sizes/counts. Unknown
!> selectors return `HUGE(1)`. Indices and pointer association are unchecked.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added HDF5-facing integer metadata lookup. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION get_metadata_hdf5_i(i, text, e) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! One-based HDF5 item index.
      INTEGER, INTENT(IN), OPTIONAL :: e    !! Required index for array-valued selectors.
      CHARACTER(*), INTENT(IN)      :: text !! Exact property selector.
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



!> @brief Replaces one structure-buffer handle on a model-facing item.
!>
!> Exact selectors `first` and `latest` update the respective `C_PTR`; any
!> other selector silently performs no assignment. Ownership of the target
!> buffer remains with [[visualisation_structure]].
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added buffer-handle mutation using integer pointer values. |
!> | 2026-03-29 | SvB | Replaced integer addresses with interoperable `C_PTR` handles. |
!> @endhistory
   SUBROUTINE set_metadata_ptr(i, text, a)
      INTEGER, INTENT(IN)      :: i    !! One-based model-facing item index.
      TYPE(C_PTR), INTENT(IN)  :: a    !! Replacement structure-buffer handle.
      CHARACTER(*), INTENT(IN) :: text !! Exact handle selector.
      SELECT CASE(text)
       CASE('first') ; items(i)%first = a
       CASE('latest') ; items(i)%latest = a
      END SELECT
   END SUBROUTINE set_metadata_ptr



!> @brief Returns one logical property from a model-facing item.
!>
!> Selectors are `on`, `isgrid`, `istimeseries`, `isreal`,
!> `varies_with_sediment`, and `varies_with_contaminant`. `on` requires both
!> optional mask coordinates. Real storage is recognized by first type letter
!> B, G, L, or M. Unknown selectors return false.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added model-facing logical metadata lookup. |
!> @endhistory
   PURE LOGICAL FUNCTION get_metadata_L(I, text, a, b) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! One-based model-facing item index.
      INTEGER, INTENT(IN), OPTIONAL :: a    !! Mask first-dimension index required by `on`.
      INTEGER, INTENT(IN), OPTIONAL :: b    !! Mask second-dimension index required by `on`.
      CHARACTER(*), INTENT(IN)      :: text !! Exact property selector.
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



!> @brief Returns one logical property from an HDF5-facing item.
!>
!> Selectors are `isgrid`, `istimeseries`, `isreal`,
!> `varies_with_sediment`, and `varies_with_contaminant`. Real storage is
!> recognized by first type letter B, G, L, or M. Unknown selectors return
!> false; item bounds are unchecked.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added HDF5-facing logical metadata lookup. |
!> @endhistory
   PURE LOGICAL FUNCTION get_metadata_HDF5_L(I, text) RESULT(r)
      INTEGER, INTENT(IN)      :: i    !! One-based HDF5 item index.
      CHARACTER(*), INTENT(IN) :: text !! Exact property selector.
      SELECT CASE(text)
       CASE('isgrid')                  ; r=hdf5_items(i)%isgrid
       CASE('istimeseries')            ; r=hdf5_items(i)%istimeseries
       CASE('isreal')                  ; r = ANY(hdf5_items(i)%typ(1:1)==(/'B','G','L','M'/))
       CASE('varies_with_sediment')    ; r=hdf5_items(i)%varies_with_sediment
       CASE('varies_with_contaminant') ; r=hdf5_items(i)%varies_with_contaminant
       CASE DEFAULT         ; r = F
      END SELECT
   END FUNCTION get_metadata_HDF5_L


!> @brief Parses the dynamic visualisation plan and resolves its references.
!>
!> [[visualisation_read:COPY]] prepares the plan input. The routine scans block
!> keywords, delegates each content block to [[HANDLE]], honors `diag`, and
!> stops parsing on `stop` or `kill`. Normal completion links every dynamic item
!> to its mask/list and time records, checks empty lists and layer ranges, and
!> deletes the temporary plan stream.
!>
!> `kill` deliberately prints guidance and executes `STOP`. Missing/invalid
!> tokens are fatal through the visualisation reader. This routine is called
!> once by the saved first `jj/=1` dynamic-registration pass.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added visualisation-plan scanning and reference resolution. |
!> | 2026-04-14 | SvB | Routed parser failures through the current fatal visualisation error service. |
!> @endhistory
   SUBROUTINE read_dynamic_visualisation_metadata()
      INTEGER      :: i   !! Dynamic item index during reference resolution.
      CHARACTER(4) :: now !! Current lowercase plan-block keyword.
      CALL COPY(DIRQQ, planfile)
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
         ERROR STOP
      ELSE
      ENDIF
      DO i=no_static_items+1,no_items
         CALL LINK_USERS_NUMBERS_TO_INDEXES(items(i))
      ENDDO
      CALL FINAL_CHECK_OF_ITEM()
      CLOSE (UNIT=vp_in,status="delete")

   END SUBROUTINE read_dynamic_visualisation_metadata



!> @brief Appends one static catalogue item and its shared grid/time records.
!>
!> Static items are always whole-grid, scope `all`, and not time series. The
!> supplied one-letter base type receives the fixed `S` structure suffix.
!> Layer-varying items span `1:TOP_CELL`; others store `(0,0)`. The shared mask
!> and schedule are lazily created by [[point_to_whole_grid]] and
!> [[point_to_static]]. The interface must register all static items before any
!> dynamic plan is read.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added static visualisation-item registration. |
!> @endhistory
   SUBROUTINE register_static_visualisation_metadata(name, typ, units, title, szi, szj, extra_dimensions, varies_with_elevation)
      INTEGER, INTENT(IN)      :: szi !! Whole-grid first-dimension extent.
      INTEGER, INTENT(IN)      :: szj !! Whole-grid second-dimension extent.
      CHARACTER(*), INTENT(IN) :: name !! Catalogue selector.
      CHARACTER, INTENT(IN)    :: typ !! One-letter base structure type.
      CHARACTER(*), INTENT(IN) :: units !! Units label.
      CHARACTER(*), INTENT(IN) :: title !! Human-readable output title.
      CHARACTER(*), INTENT(IN) :: extra_dimensions !! Extra-axis selector.
      LOGICAL, INTENT(IN)      :: varies_with_elevation !! Whether all model layers form an axis.
      TYPE(ITEM), POINTER      :: ii !! Newly appended model-facing item.
      CALL WRITE_STA_VARIABLE(name, units, title, extra_dimensions, varies_with_elevation)
      CALL INCREMENT_item(items,1)
      no_static_items = no_static_items + 1
      ii                  => items(no_items)
      ii%name             =  name
      ii%istimeseries     = F
      ii%typ              =  typ//'S'
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


!> @brief Writes one static variable to the visualisation check file.
!>
!> The first call opens `checkfile` on `vp_out` and writes the constants header.
!> Every call writes the fixed-width name, elevation flag, units, extra-axis
!> selector, and title. The saved first-call state is never reset.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added static catalogue reporting. |
!> @endhistory
   SUBROUTINE write_sta_variable(name, units, title, extra_dimensions, varies_with_elev)
      CHARACTER(*), INTENT(IN)         :: name !! Catalogue selector.
      CHARACTER(*), INTENT(IN)         :: units !! Units label.
      CHARACTER(*), INTENT(IN)         :: title !! Human-readable title.
      CHARACTER(*), INTENT(IN)         :: extra_dimensions !! Extra-axis selector.
      LOGICAL, INTENT(IN)              :: varies_with_elev !! Whether to print the elevation flag.
      LOGICAL, SAVE                    :: first=T !! Check-file header/open guard.
      CHARACTER(LEN(extra_dimensions)) :: ed !! Fixed-length extra-axis field written to the report.
      IF(extra_dimensions=='-') THEN ; ed = '-' ; ELSE ; ed=extra_dimensions ; ENDIF
      IF(first) THEN
         first = F
         OPEN(unit=vp_out, FILE=checkfile, ACTION='WRITE', STATUS='UNKNOWN')
         WRITE(vp_out,'(A)') 'Full list of constants recorded in the HDF5 file'
         WRITE(vp_out,'(A)') 'E-varies with subsurface elevation'
      ENDIF
      WRITE(vp_out,'(A8, A8, A9, A12, A70)') name, V_ELEV(varies_with_elev), units, ed, title
   END SUBROUTINE write_sta_variable



!> @brief Writes one implemented dynamic variable to the check-file catalogue.
!>
!> The first call opens `checkfile` and writes the variable header after the
!> static catalogue. Each row contains availability flags produced by
!> [[v_e_sed_con]], units, extra-axis selector, and title. Only variables whose
!> interface catalogue marks them implemented reach this routine.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added dynamic catalogue reporting. |
!> | 2026-04-03 | SvB | Made character lengths and fixed-width report formatting portable. |
!> @endhistory
   SUBROUTINE write_dyn_variable(name, units, title, extra_dimensions, varies_with_elev, varies_with_sed, varies_with_con)
      CHARACTER(*), INTENT(IN) :: name !! Catalogue selector.
      CHARACTER(*), INTENT(IN) :: units !! Units label.
      CHARACTER(*), INTENT(IN) :: title !! Human-readable title.
      CHARACTER(*), INTENT(IN) :: extra_dimensions !! Extra-axis selector.
      LOGICAL, INTENT(IN)      :: varies_with_elev !! Whether an elevation/layer selector is available.
      LOGICAL, INTENT(IN)      :: varies_with_sed !! Whether a sediment selector is required.
      LOGICAL, INTENT(IN)      :: varies_with_con !! Whether a contaminant selector is required.
      LOGICAL, SAVE            :: first=T !! Check-file dynamic-header/open guard.
      IF(first) THEN
         first = F
         OPEN(unit=vp_out, FILE=checkfile, ACTION='WRITE', STATUS='UNKNOWN')
         WRITE(vp_out,'(A80)') REPEAT('-',80)
         WRITE(vp_out,'(A)') 'Full list of variables that can be recorded in the HDF5 file'
         WRITE(vp_out,'(A)') 'E-varies with subsurface elevation; C-varies with contaminant no; S-varies with sediment fraction no'
      ENDIF
      WRITE(vp_out,'(A8, A8, A9, A12, A70)') name, &
         V_E_SED_CON(varies_with_elev,varies_with_sed,varies_with_con), &
         units, extra_dimensions, title
   END SUBROUTINE write_dyn_variable



!> @brief Formats layer, sediment, and contaminant availability flags.
!>
!> The five interior character positions receive `E`, `S`, and `C` in that
!> order, separated by blanks when enabled; disabled positions remain blank.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added dynamic availability-flag formatting. |
!> @endhistory
   PURE CHARACTER(7) FUNCTION v_e_sed_con(v,s,c) RESULT(r)
      LOGICAL, INTENT(IN) :: v !! Whether to emit `E`.
      LOGICAL, INTENT(IN) :: s !! Whether to emit `S`.
      LOGICAL, INTENT(IN) :: c !! Whether to emit `C`.
      INTEGER             :: p !! Next flag position in the fixed-width result.
      r = REPEAT(' ',LEN(r))
      p = 3
      IF(v) THEN ; r(p:p)='E' ; p=p+2 ; ENDIF
      IF(s) THEN ; r(p:p)='S' ; p=p+2 ; ENDIF
      IF(c) r(p:p)='C'
   END FUNCTION v_e_sed_con



!> @brief Formats the static elevation-availability flag.
!>
!> A true input places `E` in the middle of the five-character result; false
!> returns blanks.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added static elevation-flag formatting. |
!> @endhistory
   PURE CHARACTER(5) FUNCTION v_elev(v) RESULT(r)
      LOGICAL, INTENT(IN) :: v !! Whether to emit `E`.
      INTEGER             :: p !! Fixed output position of the flag.
      r = REPEAT(' ',LEN(r))
      p = 3
      IF(v) r(p:p)='E'
   END FUNCTION v_elev



!> @brief Publishes and resolves the dynamic output catalogue in two passes.
!>
!> With `jj=1`, implemented interface entries are written to the check-file
!> catalogue and the routine returns. On the first `jj/=1` call the user plan is
!> parsed and a saved `found` array is allocated. Every subsequent catalogue
!> entry updates all requested items with the same name, including units,
!> dependency flags, and the type returned by [[alter_dynamic_type]].
!>
!> When `final` is true, every requested name must have matched an implemented
!> catalogue entry. Items are validated and reported, then
!> [[create_hdf5_metadata]] builds the writer-facing projection.
!>
!> @warning
!> Correct operation depends on the interface's exact two-pass call order and
!> one final call after every catalogue entry. Saved state has no reset. The
!> routine appends `S` to both static and dynamic structure storage types; that
!> suffix does not distinguish time dependence.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added two-pass matching of plan requests to the model output catalogue. |
!> | 2026-04-14 | SvB | Routed rejected/unimplemented requests through the current fatal error service. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE register_dynamic_visualisation_metadata(jj, final, name, typ, units, title, &
      extra_dimensions, varies_with_elevation, varies_with_sed, varies_with_con, implemented)
      INTEGER, INTENT(IN)      :: jj !! Registration pass: one publishes, any other value resolves.
      LOGICAL, INTENT(IN)      :: final !! Whether this is the final catalogue entry of the resolving pass.
      CHARACTER(*), INTENT(IN) :: name !! Interface catalogue selector.
      CHARACTER, INTENT(IN)    :: typ !! One-letter base storage type.
      CHARACTER(*), INTENT(IN) :: units !! Units label.
      CHARACTER(*), INTENT(IN) :: title !! Human-readable title.
      CHARACTER(*), INTENT(IN) :: extra_dimensions !! Extra-axis selector.
      LOGICAL, INTENT(IN)      :: varies_with_elevation !! Whether a layer selector is available.
      LOGICAL, INTENT(IN)      :: varies_with_sed !! Whether a sediment selector is required.
      LOGICAL, INTENT(IN)      :: varies_with_con !! Whether a contaminant selector is required.
      LOGICAL, INTENT(IN)      :: implemented !! Whether current extraction code supports the entry.
      INTEGER                  :: i !! Dynamic item index.
      LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE :: found !! Match flag for each parsed dynamic request.
      TYPE(ITEM), POINTER      :: ii=>NULL() !! Current matching/validated request.
      LOGICAL, SAVE            :: first=T !! Plan-reading and `found` allocation guard.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='register_dynamic_visualisation_metadata'


      IF(jj==1) THEN
         IF(implemented) CALL WRITE_DYN_VARIABLE(name, units, title, extra_dimensions, &
            varies_with_elevation, varies_with_sed, varies_with_con)
         RETURN
      ENDIF
      IF(first) THEN
         first= F
         CALL READ_DYNAMIC_VISUALISATION_METADATA()
         ALLOCATE(found(NO_static_items+1:no_items), STAT=ios)
         CALL errstat_alloc(ios, "found", location)
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
               CALL error_visualisation()
            ELSEIF(.NOT.items(i)%implemented) THEN
               WRITE(mess,*) TRIM(items(i)%name)//' is listed in documentation'
               WRITE(mess2,*)'but has not yet been implemented '
               WRITE(mess3,*)'see the variable variables list in check_visualisation_plan.txt'
               CALL error_visualisation()
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



!> @brief Projects every model-facing item into six-slot HDF5 metadata.
!>
!> Scalar fields are copied, extra-axis labels and six fixed dimension arrays
!> are allocated, [[GET_SZ_CR]] fills size/order metadata, and `GET_MBR` supplies
!> structure-member labels. List items receive an owned copy of their resolved
!> element list; grid items continue to use bounds only.
!>
!> @warning
!> The public `hdf5_items` pointer must be disassociated and every source item
!> fully resolved. There is no repeated-call, allocation-failure, or unsupported
!> structure-type recovery. The projection intentionally does not copy timing
!> schedules or structure-buffer handles. Copying the general character getter
!> into the 11-character extra-axis field triggers a conservative compiler
!> truncation warning, but every accepted selector is at most ten characters.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added the HDF5-oriented metadata projection. |
!> | 2026-04-14 | SvB | Supplied a safe default extra-axis size used by this projection. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE create_hdf5_metadata()
      INTEGER                  :: mn  !! Item index shared by the source and projection catalogues.
      INTEGER                  :: nex !! Physical size of the selected extra axis.
      TYPE(ITEM), POINTER      :: ii  !! Current model-facing source item.
      TYPE(HDF5_ITEM), POINTER :: hh  !! Current HDF5-facing destination item.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='create_hdf5_metadata'

      ALLOCATE(hdf5_items(no_items), STAT=ios)
      CALL errstat_alloc(ios, "hdf5_items", location)

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

         ALLOCATE(hh%names_of_extra_dimensions(nex), STAT=ios)
         CALL errstat_alloc(ios, "hh%names_of_extra_dimensions", location)

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

         ALLOCATE(hh%dimensions(ndim), STAT=ios)
         CALL errstat_alloc(ios, "hh%dimensions", location)
         ALLOCATE(hh%names_of_dimensions(ndim), STAT=ios)
         CALL errstat_alloc(ios, "hh%names_of_dimensions", location)
         ALLOCATE(hh%szorder(ndim), STAT=ios)
         CALL errstat_alloc(ios, "hh%szorder", location)

         CALL GET_SZ_CR(hh)
         hh%mbr => GET_MBR(hh%typ)
         IF(.NOT.hh%isgrid) THEN
            hh%sz = ii%alist%sz
            ALLOCATE(hh%list(hh%sz), STAT=ios)
            CALL errstat_alloc(ios, "hh%list", location)
            hh%list = ii%alist%a
         ENDIF
      ENDDO
   END SUBROUTINE create_hdf5_metadata


!> @brief Fills fixed HDF5 dimension slots and their logical traversal order.
!>
!> | Logical axis | Fixed slot | Active size |
!> |:-------------|:----------:|:------------|
!> | Column or element list | 5 for grid, 6 for list | Inclusive `ilow:ihigh` extent |
!> | Row | 6 for grid, 5 (disabled) for list | Inclusive `jlow:jhigh` extent |
!> | Layer | 3 | Inclusive layer extent when `khigh>0` |
!> | Element member | 4 | `MBR_COUNT(typ)`, disabled when one |
!> | Extra | 2 | Extra-axis size, disabled when one |
!> | Time | 1 | One for a time series, otherwise disabled |
!>
!> `szorder(1:6)` retains logical traversal order: spatial/list, row, layer,
!> member, extra, then time. All three destination arrays must already have
!> length `ndim`.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added six-slot dimension and traversal-order construction. |
!> @endhistory
   SUBROUTINE GET_SZ_CR(h)
      TYPE(HDF5_ITEM), POINTER :: h      !! HDF5 item whose allocated dimension arrays are filled.
      INTEGER                  :: r      !! Current fixed-slot index.
      INTEGER                  :: mbr    !! Number of structure element members.
      INTEGER                  :: nextra !! Physical extra-axis size.
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
      r=3
      h%names_of_dimensions(r)='layer'
      IF(h%khigh>0) THEN ; h%dimensions(r)=h%khigh-h%klow+1 ; ELSE ; h%dimensions(r)=0 ; ENDIF
      h%szorder(3) = r
      r=4
      h%names_of_dimensions(r)='el_typ'
      h%dimensions(r)=mbr
      IF(h%dimensions(r)==1) h%dimensions(r)=0
      h%szorder(4) = r
      r=2
      h%names_of_dimensions(r)='extra'
      h%dimensions(r)=nextra
      IF(h%dimensions(r)==1) h%dimensions(r)=0
      h%szorder(5) = r
      r=1
      h%names_of_dimensions(r)='time'
      IF(h%istimeseries) THEN ; h%dimensions(r)=1 ; ELSE ; h%dimensions(r)=0 ; ENDIF
      h%szorder(6) = r
   END SUBROUTINE GET_SZ_CR



!> @brief Estimates the active HDF5 rank from item bounds and flags.
!>
!> This private legacy helper has no current caller; the writer instead counts
!> nonzero entries in `dimensions`.
!>
!> @warning
!> The current tests use `high-low>1`, so a two-entry row or layer extent is not
!> counted. The function also starts at rank one unconditionally. It should not
!> be used as a substitute for counting the finalized dimension array.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added the now-unused direct rank estimator. |
!> @endhistory
   PURE INTEGER FUNCTION calc_rank(h) RESULT(r)
      TYPE(HDF5_ITEM), POINTER :: h   !! HDF5 item to inspect.
      INTEGER                  :: mbr !! Number of structure element members.
      mbr = MBR_COUNT(h%typ)
      r   = 1
      IF(h%jhigh-h%jlow>1)        r=r+1
      IF(h%khigh-h%klow>1)        r=r+1
      IF(mbr>1)                   r=r+1
      IF(h%no_extra_dimensions>1) r=r+1
      IF(h%istimeseries)          r=r+1
   END FUNCTION calc_rank



!> @brief Maps a base data type plus requested representation to a storage type.
!>
!> | Representation | Scope `all` | `squares` | `banks` | `rivers` |
!> |:---------------|:------------|:----------|:--------|:---------|
!> | Grid `C` | C | V | K | O |
!> | Grid `G` | G | M | B | L |
!> | Grid `H` | H | X | T | U |
!> | Grid `L` | L | L | L | L |
!> | Grid `Q` | Q | Z | A | D |
!> | List `C/G/H/L/Q` | V/M/X/M/Z | same | same | same |
!>
!> The caller appends `S`. Current implemented catalogue paths chiefly produce
!> the storage types supported by [[visualisation_structure]]; several legacy
!> letters in this table have no current structure implementation.
!>
!> @warning
!> Base type `W` is fatal. Unknown types/scopes retain `$`; validation does not
!> explicitly reject that result before later structure lookup. `ii` is modified
!> on the fatal `W` path even though the error service stops execution.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added basis/scope transformation of legacy visualisation types. |
!> | 2026-04-14 | SvB | Routed unsupported `W` data through the current fatal error service. |
!> @endhistory
   CHARACTER FUNCTION alter_dynamic_type(typ, ii) RESULT(r)
      CHARACTER, INTENT(IN)     :: typ !! One-letter base type from the interface catalogue.
      TYPE(ITEM), INTENT(INOUT) :: ii  !! Requested item whose basis/scope select the transformation.
      IF(typ=='W') THEN
         WRITE(mess,*) 'cannot handle type W data' ; CALL error_visualisation()
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



!> @brief Dispatches one parsed plan keyword to its block handler.
!>
!> `item` appends and reads one request. `list` reads an original list followed
!> by its square/bank/river subsets. `mask` reads an effective mask followed by
!> all/square/bank/river lists and stores the first-list offset. `time` appends a
!> schedule; `diag` enables verbose output. `stop` and `kill` are handled by the
!> outer parser and do not reach this routine.
!>
!> Derived list groups must remain in exactly this order because [[extra]] and
!> [[point_to_list]] resolve them by fixed offsets.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added plan-block dispatch and derived-list construction. |
!> | 2026-04-03 | SvB | Renamed the nested explicit-list filter to avoid collision with module `lists`. |
!> @endhistory
   SUBROUTINE HANDLE(now)
      CHARACTER(4), INTENT(IN) :: now  !! Exact lowercase plan-block keyword.
      INTEGER                  :: orig !! Index of an original explicit list while subsets are appended.
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

!> @brief Resolves one parsed item's mask/list and timing references.
!>
!> `grid_as_grid` points `amask` at the referenced mask and sets `isgrid` true.
!> `grid_as_list` resolves a mask-derived scoped list; `list_as_list` resolves an
!> explicit-list scoped subset. Every path resolves `atime` by user number.
!> Lookup failure is fatal.
!>
!> @warning
!> List paths do not explicitly reset `isgrid` false; normal freshly initialized
!> dynamic items already have false. An `as_above` item copied from a static
!> item can violate that assumption.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added user-number reference resolution. |
!> @endhistory
   SUBROUTINE link_users_numbers_to_indexes(it)
      TYPE(ITEM), INTENT(INOUT) :: it  !! Parsed item whose references are resolved in place.
      INTEGER                   :: uun !! Referenced user mask/list number.
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



!> @brief Returns the lazily created shared static-output schedule.
!>
!> On first use a singleton timing record numbered 999 is allocated with one
!> `HUGE(1.0)` interval and stop. Every static item points to this record. The
!> object is process-lifetime state and is never deallocated or reset.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added the shared static timing sentinel. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   FUNCTION point_to_static() RESULT(r)
      TYPE(TTIME), POINTER :: r !! Shared static schedule.
      LOGICAL, SAVE        :: first=T !! Lazy-allocation guard.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='point_to_static'

      IF(first) THEN
         first    =  F
         ALLOCATE(sstatic, STAT=ios)
         CALL errstat_alloc(ios, "sstatic", location)
         r  => sstatic
         r%number = 999
         r%sz     = 1
         ALLOCATE(r%tstep(1), STAT=ios)
         CALL errstat_alloc(ios, "r%tstep", location)
         ALLOCATE(r%tstop(1), STAT=ios)
         CALL errstat_alloc(ios, "r%tstop", location)
         r%tstep(1) = HUGE(1.0)
         r%tstop(1) = HUGE(1.0)
      ENDIF
      r => sstatic
   END FUNCTION point_to_static



!> @brief Returns the lazily created shared all-true grid mask.
!>
!> The first call allocates mask number 999 over bounds `(1:i,1:j)`. Later calls
!> return the same mask and ignore their dimensions, which is valid only because
!> all static registrations use one model grid.
!>
!> @warning
!> A later call with different extents does not resize or validate the mask.
!> Dimensions must be positive and stable before first use.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added the shared whole-grid mask. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   FUNCTION point_to_whole_grid(i,j) RESULT(r)
      INTEGER, INTENT(IN) :: i !! First-dimension extent used on first call.
      INTEGER, INTENT(IN) :: j !! Second-dimension extent used on first call.
      TYPE(MASK), POINTER :: r !! Shared all-true mask.
      LOGICAL, SAVE       :: first=T !! Lazy-allocation guard.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='point_to_whole_grid'

      IF(first) THEN
         first    =  F
         ALLOCATE(whole_grid, STAT=ios)
         CALL errstat_alloc(ios, "whole_grid", location)
         r => whole_grid
         r%number = 999
         r%ilow   =1
         r%ihigh  =i
         r%jlow   =1
         r%jhigh  =j
         ALLOCATE(r%ma(i,j), STAT=ios)
         CALL errstat_alloc(ios, "r%ma", location)
         r%ma = T
      ENDIF
      r => whole_grid
   END FUNCTION point_to_whole_grid



!> @brief Maps an element scope to its fixed derived-list offset.
!>
!> `all`, `squares`, `banks`, and `rivers` map to 0, 1, 2, and 3. This ordering
!> must match [[HANDLE]]. There is no default branch; callers rely on prior item
!> validation.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added fixed scope offsets for derived-list groups. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION extra(s) RESULT(r)
      CHARACTER(*), INTENT(IN) :: s !! Exact validated scope selector.
      SELECT CASE(s)
       CASE('all')    ; r=0
       CASE('squares') ; r=1
       CASE('banks')  ; r=2
       CASE('rivers')  ; r=3
      END SELECT
   END FUNCTION extra

!> @brief Finds a user mask and returns a pointer to its stored record.
!>
!> The first mask whose `number` equals the requested value is returned. A
!> missing number is fatal. Mask numbering uniqueness is not checked, so a
!> duplicate silently resolves to the first occurrence.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added user-number mask lookup. |
!> | 2026-04-14 | SvB | Routed lookup failure through the current fatal error service. |
!> @endhistory
   FUNCTION point_to_mask(users_no_for_link_or_mask) RESULT(r)
      INTEGER, INTENT(IN) :: users_no_for_link_or_mask !! User-visible mask number.
      TYPE(MASK), POINTER :: r !! Matching stored mask.
      INTEGER             :: i !! Mask-table index.
      r=>NULL()
      DO i=1,no_masks
         IF(masks(i)%number==users_no_for_link_or_mask) THEN
            r=>masks(i)
            EXIT
         ENDIF
      ENDDO
      IF(.NOT.ASSOCIATED(r)) THEN
         WRITE(mess,'(A,I3)') 'Failed to find mask ',users_no_for_link_or_mask
         CALL error_visualisation()
      ENDIF
   END FUNCTION point_to_mask

!> @brief Resolves a scoped list from a mask or explicit list user number.
!>
!> For `grid_as_list`, the mask lookup supplies its `listno` base; for
!> `list_as_list`, the original explicit list supplies the base. [[extra]] then
!> selects the all/original, square, bank, or river entry at offsets 0:3.
!> Missing source numbers are fatal.
!>
!> @warning
!> Correct resolution depends on contiguous four-entry groups in the order
!> established by [[HANDLE]]. Duplicate user numbers select the first source.
!> Any basis other than exact `grid_as_list` follows the explicit-list branch.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added mask/list scope resolution. |
!> | 2026-04-14 | SvB | Routed lookup failures through the current fatal error service. |
!> @endhistory
   FUNCTION point_to_list(users_no_for_link_or_mask, basis, scope) RESULT(r)
      INTEGER, INTENT(IN)      :: users_no_for_link_or_mask !! User-visible mask or list number.
      CHARACTER(*), INTENT(IN) :: basis !! Exact basis selector.
      CHARACTER(*), INTENT(IN) :: scope !! Exact validated scope selector.
      TYPE(LLIST), POINTER     :: r !! Resolved original or derived list.
      INTEGER                  :: i !! Mask/list search index.
      INTEGER                  :: j !! Located source index, or zero before a match.
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
            CALL error_visualisation()
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
            CALL error_visualisation()
         ENDIF
         r =>lists(j+EXTRA(scope))
      ENDIF

   END FUNCTION point_to_list

!> @brief Finds a user timing block and returns its stored record.
!>
!> The first matching `number` is returned. A missing number is fatal; duplicate
!> numbers are not rejected and resolve to the first occurrence.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added user-number timing lookup. |
!> | 2026-04-14 | SvB | Routed lookup failure through the current fatal error service. |
!> @endhistory
   FUNCTION point_to_time(users_no_for_times) RESULT(r)
      INTEGER, INTENT(IN)  :: users_no_for_times !! User-visible time-block number.
      TYPE(TTIME), POINTER :: r !! Matching stored schedule.
      INTEGER              :: i !! Time-table index.
      r=>NULL()
      DO i=1,no_times
         IF(times(i)%number==users_no_for_times) THEN
            r=>times(i)
            EXIT
         ENDIF
      ENDDO
      IF(.NOT.ASSOCIATED(r)) THEN
         WRITE(mess,'(A,I3)') 'Failed to find times data set ',users_no_for_times
         CALL error_visualisation()
      ENDIF
   END FUNCTION point_to_time

!> @brief Returns a shallow derived-type copy of one model-facing item.
!>
!> This private legacy helper has no current caller. Intrinsic assignment copies
!> scalar values and pointer association targets; it does not deep-copy masks,
!> lists, schedules, or structure-buffer storage. Bounds are unchecked.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added private item retrieval. |
!> @endhistory
   TYPE(ITEM) FUNCTION get_item(i) RESULT(r)
      INTEGER, INTENT(IN) :: i !! One-based model-facing item index.
      r = items(i)
   END FUNCTION get_item


!> @brief Legacy helper intended to report total, static, or dynamic counts.
!>
!> This private function has no current caller.
!>
!> @warning
!> The optional-argument test is reversed in current code. When `text` is
!> present the function always returns total `no_items`; when absent it illegally
!> references `text` in a `SELECT CASE`. It cannot safely provide the intended
!> static/dynamic query and is retained unchanged for documentation-only scope.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added the now-unused catalogue count helper. |
!> @endhistory
   PURE INTEGER FUNCTION no_of_items(text) RESULT(r)
      CHARACTER(*), INTENT(IN), OPTIONAL :: text !! Intended optional selector `static` or `dynamic`.
      IF(PRESENT(text)) THEN
         r = no_items
      ELSE
         SELECT CASE(text)
          CASE('static')  ; r=no_static_items
          CASE('dynamic') ; r=no_items-no_static_items
         END SELECT
      ENDIF
   END FUNCTION no_of_items



!> @brief Reads plan tokens until a recognized block keyword is found.
!>
!> Every unrecognized token is skipped. Diagnostics report the search and match.
!> End-of-file and read failures are handled fatally by `R_C`; the assignment
!> `r='stop'` after the explicit `RETURN` is unreachable legacy code.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added plan-keyword scanning. |
!> @endhistory
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

!> @brief Reads one `item` block into a model-facing request.
!>
!> Recognized headings set user number, name, basis, scope, extra axis,
!> mask/list number, timing number, layer range, sediment number, and
!> contaminant number. Reversed layer endpoints are normalized. Exact lowercase
!> `as_above` copies the previous item and then restores only the new name and
!> item number. `ENDITEM` terminates the block; unknown headings are fatal.
!>
!> @warning
!> `as_above` requires a preceding dynamic request. Intrinsic assignment is a
!> shallow copy and can inherit static mask/time pointers and `isgrid=.TRUE.` if
!> used on the first dynamic item, while list reference resolution does not
!> clear `isgrid`. No guard enforces this precondition. Item names are stored in
!> eight characters; longer plan values are silently truncated before catalogue
!> matching. The temporary used by `as_above` makes that fixed-width narrowing
!> visible as a compiler warning.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added item-block parsing and `as_above` inheritance. |
!> | 2026-04-14 | SvB | Routed unknown headings through the current fatal error service. |
!> @endhistory
   SUBROUTINE read_item(s)
      TYPE(ITEM), INTENT(INOUT) :: s      !! Newly appended request populated in place.
      INTEGER                   :: number !! New user number preserved across `as_above` copying.
      CHARACTER(csz)            :: dum    !! Current heading token.
      CHARACTER(csz)            :: name   !! New name preserved across `as_above` copying.
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
          CASE DEFAULT
            WRITE(mess,'(A,I4)') TRIM(dum)//'  Unrecognised heading in item number',s%users_number
            CALL error_visualisation()
         END SELECT
      ENDDO
   END SUBROUTINE read_item




!> @brief Validates selectors and component numbers on one resolved item.
!>
!> Basis, scope, and extra-axis values must occur in the module parameter
!> arrays. Sediment/contaminant-dependent variables require an in-range positive
!> selector; independent variables require zero. The first failure is fatal.
!>
!> @note
!> The diagnostic for a contaminant number supplied to an independent variable
!> currently says `SEDIMENT No`; validation itself still tests
!> `contaminant_no`. Item-number uniqueness, reference-number uniqueness, and
!> schedule validity are not checked here.
!> @endnote
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added item selector and component validation. |
!> | 2026-04-14 | SvB | Routed validation failures through the current fatal error service. |
!> @endhistory
   SUBROUTINE check_item(a)
      TYPE(ITEM), INTENT(IN) :: a !! Resolved item to validate.
      IF (ALL(a%basis/=basis)) THEN
         WRITE(mess,'(2A)') a%basis,'  BASIS NOT RECOGNISED'
         WRITE(mess2,'(A,10A14)') ' SHOULD BE ONE OF: ',basis
         CALL error_visualisation()
      ENDIF
      IF (ALL(a%scope/=scope)) THEN
         WRITE(mess,'(2A)')  a%scope,'SCOPE NOT RECOGNISED'
         WRITE(mess2,'(A,10A8)') 'SHOULD BE ONE OF: ',scope
         CALL error_visualisation()
      ENDIF
      IF (ALL(a%extra_dimensions/=extra_dimensions)) THEN
         WRITE(mess,'(2A)')  a%extra_dimensions,'EXTRA_DIMENSION NOT RECOGNISED'
         WRITE(mess2,'(A,10A8)') 'SHOULD BE ONE OF: ',extra_dimensions
         CALL error_visualisation()
      ENDIF
      IF(a%varies_with_sediment) THEN
         IF(a%sediment_no<1 .OR. a%sediment_no>nsed) THEN
            WRITE(mess,'(A,I4,A,I4,A)')  'IN ITEM ', a%users_number, ' SEDIMENT No ',a%sediment_no, ' DOES NOT EXIST'
            CALL error_visualisation()
         ENDIF
      ELSEIF(a%sediment_no/=0) THEN
         WRITE(mess,'(A,I4,A,I4,A)')  'IN ITEM ', a%users_number, ' SEDIMENT No ',a%sediment_no, ' SHOULD NOT BE SPECIFIED'
         CALL error_visualisation()
      ENDIF
      IF(a%varies_with_contaminant) THEN
         IF(a%contaminant_no<1 .OR. a%contaminant_no>ncon) THEN
            WRITE(mess,'(A,I4,A,I4,A)')  'IN ITEM ',a%users_number, ' CONTAMINANT No ',a%contaminant_no, ' DOES NOT EXIST'
            CALL error_visualisation()
         ENDIF
      ELSEIF(a%contaminant_no/=0) THEN
         WRITE(mess,'(A,I4,A,I4,A)')  'IN ITEM ', a%users_number, ' SEDIMENT No ',a%contaminant_no, ' SHOULD NOT BE SPECIFIED'
         CALL error_visualisation()
      ENDIF

   END SUBROUTINE check_item

!> @brief Performs catalogue-wide empty-list and layer-bound checks.
!>
!> Every list-based item must resolve to at least one element. Layer endpoints
!> may be zero to suppress the axis; otherwise they may not fall below zero or
!> above `TOP_CELL`. All failures are written before one fatal error call.
!>
!> @note
!> The test permits mixed ranges beginning at zero and does not require a
!> layer-varying variable to select layers. Those are current validation limits.
!> @endnote
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added final empty-list and layer-range checks. |
!> | 2026-04-14 | SvB | Routed aggregate failure through the current fatal error service. |
!> @endhistory
   SUBROUTINE final_check_of_item()
      INTEGER             :: i   !! Item index.
      INTEGER             :: cnt !! Number of reported failures.
      TYPE(ITEM), POINTER :: a   !! Current item.
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
      IF(cnt>0) CALL error_visualisation()
   END SUBROUTINE final_check_of_item


!> @brief Reads one piecewise timing block and appends a sentinel segment.
!>
!> The leading record supplies the user number and pair count. Each following
!> pair is `(tstep,tstop)` in hours. Arrays have size `sz+1`; the final step and
!> stop are both `HUGE(1.0)` so [[get_next_time]] can always terminate for a
!> valid schedule. User pairs are echoed to the check file.
!>
!> @warning
!> Pair count, positive step size, ascending stop times, and stop/step
!> consistency are not validated. Invalid schedules can stall or mis-schedule
!> [[time_to_record]]. Duplicate user time numbers resolve to the first block.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added timing-block parsing and terminal sentinel values. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE read_time(t)
      TYPE(TTIME), INTENT(INOUT) :: t !! Newly appended timing record populated in place.
      INTEGER                    :: i !! User timing-pair index.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='read_time'

      IF(diagnostics) WRITE(vp_out,'(50X,A)') 'reading times'
      CALL R_I('TIMES number and size', t%number, t%sz)

      ALLOCATE(t%tstep(t%sz+1), STAT=ios)
      CALL errstat_alloc(ios, "t%tstep", location)
      ALLOCATE(t%tstop(t%sz+1), STAT=ios)
      CALL errstat_alloc(ios, "t%tstop", location)

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


!> @brief Reads and validates one explicit element list.
!>
!> The input gives user list number, size, and exactly that many element
!> numbers. The original list receives scope `all` and its internal table index,
!> is echoed with a runtime-generated format, and requires every element to lie
!> in `1:nel`. All invalid elements are reported before a fatal error.
!>
!> @warning
!> Nonnegative list size, uniqueness, and user-list-number uniqueness are not
!> checked here. [[make_list_from_list]] later sorts and removes duplicates.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added explicit element-list parsing and range validation. |
!> | 2026-04-03 | SvB | Replaced a compiler-specific repeated-field format with a runtime format string. |
!> | 2026-04-14 | SvB | Routed aggregate range failure through the current fatal error service. |
!> @endhistory
   SUBROUTINE read_list(L)
      TYPE(LLIST), INTENT(INOUT) :: L !! Newly appended original list populated in place.
      INTEGER                    :: i !! Element position.
      INTEGER                    :: cnt !! Number of invalid elements.
      CHARACTER(LEN=100)         :: fmt_str !! Runtime repeated-integer output format.

      IF(diagnostics) WRITE(vp_out,'(50X,A)') 'reading a list'

      L%scope = 'all'
      L%indx  = no_lists
      CALL R_I('list NO AND SIZE',L%number, L%sz)
      WRITE(vp_out,'(A)') REPEAT('=',2*sp)
      WRITE(vp_out,'(A,I2,A,I4,2A)') 'LIST NUMBER ', L%number, '  SIZE:', L%sz, '  SCOPE: ', L%scope
      ALLOCATE(L%a(L%sz))
      CALL R_I('list', L%sz, L%a)
      IF(diagnostics) WRITE(vp_out,'(50X,A)') 'read list'
      WRITE(fmt_str, '("(", I0, "I5)")') L%sz
      WRITE(vp_out, fmt_str) L%a
      cnt = 0
      DO i=1,SIZE(L%a)
         IF(L%a(i)<1 .OR. L%a(i)>nel) THEN
            WRITE(vp_out,'(A,I6,A,I6,A)') 'element no ', L%a(i), ' in list ', L%number, ' does not exist'
            cnt = cnt + 1
         ENDIF
      ENDDO
      IF(cnt>0) CALL error_visualisation()
   END SUBROUTINE read_list

!> @brief Derives a sorted scoped element list from an effective grid mask.
!>
!> Capacity is `GET_NUM(txt)*COUNT(m%ma)`: one square, four banks, four rivers,
!> or all nine candidates per enabled cell. Nested [[loops]] fills candidates
!> from display-oriented `SU_NUMBER`, `BANK_NO`, and `RIVER_NO`; [[sort]] removes
!> absent zeros and duplicates and orders remaining element numbers.
!>
!> The result's `scope`, `sz`, and `a` are defined. Other private `llist`
!> bookkeeping fields remain undefined because mask-derived lists are located
!> through their group's stored array offset.
!>
!> @warning
!> `txt` must be one of the four validated scopes. Mask bounds must conform to
!> all transferred topology arrays. An empty effective mask reaches [[sort]]
!> with a zero-sized candidate array; that edge path is not explicitly guarded.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added topology-aware scoped lists derived from masks. |
!> @endhistory
   TYPE(LLIST) FUNCTION make_list_from_mask(m, txt) RESULT(r)
      TYPE(MASK), INTENT(IN)   :: m   !! Effective source mask and inclusive display bounds.
      CHARACTER(*), INTENT(IN) :: txt !! Exact derived scope.
      INTEGER                  :: num !! Maximum candidate elements contributed per enabled cell.
      r%scope = txt
      num     = GET_NUM(txt)
      r%sz    = num*COUNT(m%ma) ; ALLOCATE(r%a(r%sz)) ; r%a = 0
      CALL LOOPS()
      IF(diagnostics) WRITE(vp_out,'(50X,A)') 'creating a '//TRIM(txt)//' list from mask'
      CALL SORT(r%sz, r%a)
      WRITE(vp_out,'(A,I3,A,i5,2A)') '-----'//' list from mask number',m%number,' size:', r%sz, ' scope: ', r%scope
      IF(diagnostics) WRITE(vp_out,'(50X,A)') 'created list'
      WRITE(vp_out, '(20I5)') r%a

   CONTAINS

      !> Populates the result's candidate array in mask traversal order.
      !>
      !> The effective mask guarantees a positive gridsquare number. Topology
      !> arrays may contribute zero for absent banks/links; [[sort]] removes it.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2004-07 | JE | Added nested mask/topology traversal. |
      !> @endhistory
      SUBROUTINE loops()
         INTEGER :: c  !! Next candidate-array position.
         INTEGER :: i  !! Mask first-dimension index.
         INTEGER :: j  !! Mask second-dimension index.
         INTEGER :: su !! Positive gridsquare element number.
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

!> @brief Derives a sorted element-class subset from an explicit list.
!>
!> A result array with the original list capacity is filled by nested
!> [[filter_list_items]], then [[sort]] removes zeros/duplicates and shrinks it
!> to the unique matching square, bank, or river elements. The source list is
!> the scope-`all` entry, so only the three class subsets use this function.
!>
!> `num=GET_NUM(txt)` is retained but unused. Result bookkeeping other than
!> `scope`, `sz`, and `a` remains undefined; fixed group offsets locate it.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added scope subsets derived from explicit lists. |
!> | 2026-04-03 | SvB | Reworked and renamed the nested filter for current name-resolution rules. |
!> @endhistory
   FUNCTION make_list_from_list(L, txt) RESULT(r)
      TYPE(LLIST), INTENT(IN)      :: L   !! Validated original element list.
      CHARACTER(LEN=*), INTENT(IN) :: txt !! Exact class scope: squares, banks, or rivers.
      TYPE(LLIST)                  :: r   !! Derived sorted subset.
      INTEGER                      :: num !! Retained result of `GET_NUM`; unused by current filtering.

      r%scope = txt
      num = GET_NUM(txt)
      r%sz = L%sz

      ALLOCATE(r%a(r%sz))
      r%a = 0

      CALL filter_list_items()

      IF (diagnostics) THEN
         WRITE(vp_out, '(50X, A)') 'creating a ' // TRIM(txt) // ' list from list'
      END IF

      CALL SORT(r%sz, r%a)

      WRITE(vp_out, '(A, I3, A, I4, 2A)') '-----list from list number', L%number, &
         ' size:', r%sz, ' scope :', r%scope

      IF (diagnostics) THEN
         WRITE(vp_out, '(50X, A)') 'created list'
      END IF

      WRITE(vp_out, '(*(I5))') r%a

   CONTAINS

      !> Copies elements of the requested class into the result candidate array.
      !>
      !> Source elements were range-checked by [[read_list]], so the transferred
      !> type arrays may be indexed directly. Unmatched entries remain zero for
      !> the outer [[sort]] to remove.
      !>
      !> @history
      !> | Date | Author | Description |
      !> |:-----|:-------|:------------|
      !> | 2026-04-03 | SvB | Isolated the nested class filter under its collision-free current name. |
      !> @endhistory
      SUBROUTINE filter_list_items()
         INTEGER :: c   !! Next result candidate position.
         INTEGER :: i   !! Source-list position.
         INTEGER :: su  !! Current validated element number.
         LOGICAL :: iss !! Whether the current element matches `txt`.

         c = 1
         DO i = 1, L%sz
            su = L%a(i)
            iss = .FALSE.

            SELECT CASE(txt)
             CASE('squares')
               IF (IS_SQUARE(su)) iss = .TRUE.
             CASE('banks')
               IF (IS_BANK(su)) iss = .TRUE.
             CASE('rivers')
               IF (IS_LINK(su)) iss = .TRUE.
            END SELECT

            IF (iss) THEN
               r%a(c) = su
               c = c + 1
            END IF
         END DO
      END SUBROUTINE filter_list_items

   END FUNCTION make_list_from_list



!> @brief Returns the maximum candidate count contributed per selected cell.
!>
!> `all`, `squares`, `banks`, and `rivers` return 9, 1, 4, and 4. There is no
!> default branch; callers must supply a validated scope.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added candidate-capacity lookup for derived lists. |
!> @endhistory
   PURE INTEGER FUNCTION get_num(txt) RESULT(r)
      CHARACTER(*), INTENT(IN) :: txt !! Exact validated scope selector.
      SELECT CASE(txt)
       CASE('all')     ; r=9
       CASE('squares') ; r=1
       CASE('banks')   ; r=4
       CASE('rivers')  ; r=4
      END SELECT
   END FUNCTION get_num



!> @brief Removes nonpositive/duplicate candidates and sorts element numbers.
!>
!> A logical presence array indexed `1:MAXVAL(a)` marks positive candidates.
!> `COUNT` gives the unique result size; the pointer target is reallocated only
!> when that size shrinks. Scanning the presence array then writes ascending
!> element numbers. This is a set conversion, not a comparison sort.
!>
!> @warning
!> `sza` must equal the current extent of associated pointer `a`, and candidates
!> must be nonnegative. Memory scales with the greatest element number rather
!> than candidate count. Empty/all-zero input relies on zero-size intrinsic and
!> allocation behavior; negative candidates can index below `d`.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added presence-array deduplication and ascending ordering. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE sort(sza, a)
      INTEGER, INTENT(INOUT)             :: sza !! Candidate count on entry; unique positive count on return.
      INTEGER, DIMENSION(:), POINTER     :: a   !! Candidate array, possibly reallocated and sorted in place.
      INTEGER                            :: i   !! Candidate/presence-array index.
      INTEGER                            :: j   !! Count or next output position.
      INTEGER                            :: szd !! Largest candidate and presence-array extent.
      LOGICAL, DIMENSION(:), ALLOCATABLE :: d   !! Presence flags indexed by element number.
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='sort'


      szd = MAXVAL(a)
      ALLOCATE(d(szd), STAT=ios)
      CALL errstat_alloc(ios, "d", location)
      d = F
      j = 0
      DO i=1,sza
         IF(a(i)>0) THEN ; d(a(i)) = T ; j=j+1 ; ENDIF
      ENDDO

      j = COUNT(d)
      IF(j<sza) THEN     ! Remove absent zeroes and duplicate element numbers.
         sza = j
         DEALLOCATE(a)
         ALLOCATE(a(sza), STAT=ios)
         CALL errstat_alloc(ios, "a", location)
      ENDIF

      j = 1
      DO i=1,szd
         IF(d(i)) THEN ; a(j)=i ; j=j+1 ; ENDIF
      ENDDO

   END SUBROUTINE sort



!> @brief Reads a rectangular mask and removes non-model cells.
!>
!> The header order is user number, row bounds, then column bounds. Reversed
!> endpoints are normalized and a lower-bound-preserving logical array is
!> allocated. Any character not present in `off` enables a cell. The raw mask is
!> reported, then ANDed with positive `SU_NUMBER` existence and reported again.
!>
!> @warning
!> Mask extents are not checked against the display-oriented `SU_NUMBER` grid.
!> Out-of-range or nonpositive bounds can fail during allocation or effective
!> mask construction. Duplicate mask numbers resolve to the first occurrence.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added plan-mask parsing and catchment intersection. |
!> | 2026-03-29 | SvB | Moved mask reporting to the allocatable standalone [[mask_write]] helper. |
!> @endhistory
   SUBROUTINE read_mask(m, off)
      TYPE(MASK), INTENT(INOUT)           :: m   !! Newly appended mask populated in place.
      CHARACTER, DIMENSION(:), INTENT(IN) :: off !! Characters interpreted as disabled cells.
      INTEGER                             :: i   !! Column index and temporary bound during normalization.
      INTEGER                             :: j   !! Row index.
      CHARACTER                           :: c   !! One mask-cell token.
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

      CALL mask_write('mask as read', m%ma, 'T', 'F')

      DO j=m%jlow,m%jhigh
         DO i=m%ilow,m%ihigh
            m%ma(i,j) = m%ma(i,j) .AND. EXISTS(SU_NUMBER(i,j))  ! Exclude non-model grid cells.
         ENDDO
      ENDDO
      CALL mask_write('effective mask', m%ma, 'T', '.')
   END SUBROUTINE read_mask



!> @brief Writes a logical mask using caller-selected true/false characters.
!>
!> Empty dimensions return without output. Otherwise a same-shape character
!> array is allocated, filled with `tr`/`fa`, and written using a runtime format
!> whose repeat count equals the first dimension. Fortran array order therefore
!> emits one first-dimension run for each second-dimension row.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added mask rendering as a nested plan-reader helper. |
!> | 2026-03-29 | SvB | Made it standalone with an allocatable character buffer. |
!> | 2026-04-03 | SvB | Replaced the Intel repeat-count format extension with a runtime format. |
!> @endhistory
   SUBROUTINE mask_write(txt, ma, tr, fa)
      CHARACTER(*), INTENT(IN) :: txt    !! Heading written before the mask.
      LOGICAL, INTENT(IN)      :: ma(:,:) !! Mask values in display orientation.
      CHARACTER(1), INTENT(IN) :: tr     !! Character used for true entries.
      CHARACTER(1), INTENT(IN) :: fa     !! Character used for false entries.
      CHARACTER(1), ALLOCATABLE :: cc(:,:) !! Rendered character mask.
      CHARACTER(20)             :: fmt_str !! Runtime repeated-character format.

      IF (SIZE(ma, 1) == 0 .OR. SIZE(ma, 2) == 0) RETURN

      ALLOCATE(cc(SIZE(ma, 1), SIZE(ma, 2)))
      WHERE(ma)
         cc = tr
      ELSEWHERE
         cc = fa
      END WHERE

      WRITE(vp_out,'(50X,A)') txt
      ! Build the repeated-character format without a compiler extension.
      WRITE(fmt_str, '("(",I0,"A)")') SIZE(cc, 1)
      WRITE(vp_out, fmt_str) cc

      DEALLOCATE(cc)
   END SUBROUTINE mask_write


!> @brief Grows the model-facing item pointer array.
!>
!> Existing records are shallow-copied into a new target, preserving their
!> mask/list/time pointer associations and `C_PTR` values, then the old array
!> target is deallocated. The global item count advances by one.
!>
!> @warning
!> Although `n` controls allocated growth, `no_items` always increments by one;
!> all current callers therefore pass `n=1`. Association status of `s` must be
!> defined before entry. Allocation failure is not handled.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added typed item-array growth. |
!> | 2026-07-12 | SvB | Inlined the former shared include implementation. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE INCREMENT_item(s,n)
      TYPE(ITEM), DIMENSION(:), POINTER, INTENT(INOUT) :: s   !! Pointer array to grow and retarget.
      INTEGER, INTENT(IN)                             :: n   !! Number of new slots; current contract requires one.
      TYPE(ITEM), DIMENSION(:), POINTER                :: old !! Previous array target during copying.
      INTEGER                                          :: sz  !! Previous array extent.
      INTEGER(KIND=I_P)                                :: ios  !! IOSTAT value for allocation.
      CHARACTER(LEN=*), PARAMETER :: location='INCREMENT_item'

      IF (ASSOCIATED(s)) THEN
         sz = SIZE(s)
         old => s
         NULLIFY(s)
         ALLOCATE(s(sz+n), STAT=ios)
         CALL errstat_alloc(ios, "s", location)
         IF (sz > 0) s(1:sz) = old
         DEALLOCATE(old)
      ELSE
         ALLOCATE(s(n), STAT=ios)
         CALL errstat_alloc(ios, "s", location)
      END IF
      no_items   = no_items + 1
   END SUBROUTINE INCREMENT_item



!> @brief Grows the explicit/derived list pointer array.
!>
!> Existing records and their element-array pointer associations are
!> shallow-copied before the old list-array target is deallocated. `no_lists`
!> advances by one.
!>
!> @warning
!> `n` controls growth but the counter advances by one; every current caller
!> passes `n=1`. Association status must be defined and allocation is unchecked.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added typed list-array growth. |
!> | 2026-07-12 | SvB | Inlined the former shared include implementation. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE INCREMENT_LIST(s,n)
      TYPE(LLIST), DIMENSION(:), POINTER, INTENT(INOUT) :: s   !! Pointer array to grow and retarget.
      INTEGER, INTENT(IN)                              :: n   !! Number of new slots; current contract requires one.
      TYPE(LLIST), DIMENSION(:), POINTER                :: old !! Previous array target during copying.
      INTEGER                                           :: sz  !! Previous array extent.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='INCREMENT_LIST'

      IF (ASSOCIATED(s)) THEN
         sz = SIZE(s)
         old => s
         NULLIFY(s)
         ALLOCATE(s(sz+n), STAT=ios)
         CALL errstat_alloc(ios, "s", location)
         IF (sz > 0) s(1:sz) = old
         DEALLOCATE(old)
      ELSE
         ALLOCATE(s(n), STAT=ios)
         CALL errstat_alloc(ios, "s", location)
      END IF
      no_lists   = no_lists + 1
   END SUBROUTINE INCREMENT_LIST



!> @brief Grows the user-mask pointer array.
!>
!> Existing masks and their logical-array associations are shallow-copied before
!> the old array target is deallocated. `no_masks` advances by one.
!>
!> @warning
!> `n` controls growth but the counter advances by one; every current caller
!> passes `n=1`. Association status must be defined and allocation is unchecked.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added typed mask-array growth. |
!> | 2026-07-12 | SvB | Inlined the former shared include implementation. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE INCREMENT_MASK(s,n)
      TYPE(MASK), DIMENSION(:), POINTER, INTENT(INOUT) :: s   !! Pointer array to grow and retarget.
      INTEGER, INTENT(IN)                             :: n   !! Number of new slots; current contract requires one.
      TYPE(MASK), DIMENSION(:), POINTER                :: old !! Previous array target during copying.
      INTEGER                                          :: sz  !! Previous array extent.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='INCREMENT_MASK'

      IF (ASSOCIATED(s)) THEN
         sz = SIZE(s)
         old => s
         NULLIFY(s)
         ALLOCATE(s(sz+n), STAT=ios)
         CALL errstat_alloc(ios, "s", location)
         IF (sz > 0) s(1:sz) = old
         DEALLOCATE(old)
      ELSE
         ALLOCATE(s(n), STAT=ios)
         CALL errstat_alloc(ios, "s", location)
      END IF
      no_masks  = no_masks + 1
   END SUBROUTINE INCREMENT_MASK



!> @brief Grows the user timing-block pointer array.
!>
!> Existing schedules and their step/stop array associations are shallow-copied
!> before the old array target is deallocated. `no_times` advances by one.
!>
!> @warning
!> `n` controls growth but the counter advances by one; every current caller
!> passes `n=1`. The module pointer `times` has no explicit initial null
!> association, yet this routine immediately applies `ASSOCIATED(s)`.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added typed timing-array growth. |
!> | 2026-07-12 | SvB | Inlined the former shared include implementation. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE INCREMENT_TIME(s,n)
      TYPE(TTIME), DIMENSION(:), POINTER, INTENT(INOUT) :: s   !! Pointer array to grow and retarget.
      INTEGER, INTENT(IN)                              :: n   !! Number of new slots; current contract requires one.
      TYPE(TTIME), DIMENSION(:), POINTER                :: old !! Previous array target during copying.
      INTEGER                                           :: sz  !! Previous array extent.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location='INCREMENT_TIME'

      IF (ASSOCIATED(s)) THEN
         sz = SIZE(s)
         old => s
         NULLIFY(s)
         ALLOCATE(s(sz+n), STAT=ios)
         CALL errstat_alloc(ios, "s", location)
         IF (sz > 0) s(1:sz) = old
         DEALLOCATE(old)
      ELSE
         ALLOCATE(s(n), STAT=ios)
         CALL errstat_alloc(ios, "s", location)
      END IF
      no_times  = no_times + 1
   END SUBROUTINE INCREMENT_TIME



!> @brief Returns the physical size of an extra-axis selector.
!>
!> `faces` returns four; `left_right` and `X_Y` return two. `-` and unknown
!> selectors return one so an allocated placeholder label exists; [[GET_SZ_CR]]
!> later changes singleton extra axes to dimension zero.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added extra-axis sizing. |
!> | 2026-04-14 | SvB | Added the safe default size of one. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION no_extra_dimensions(e_d) RESULT(r)
      CHARACTER(*), INTENT(IN) :: e_d !! Exact extra-axis selector.
      SELECT CASE(e_d)
       CASE('-')        ; r = 1
       CASE('faces')       ; r = 4
       CASE('left_right')  ; r = 2
       CASE('X_Y')         ; r = 2
       CASE DEFAULT        ; r = 1
      END SELECT
   END FUNCTION no_extra_dimensions



!> @brief Returns fixed labels for one extra axis.
!>
!> `faces` labels are North, East, South, West; `left_right` labels are left and
!> right; `X_Y` labels are x and y; `-` is blank. The result is blank-initialized
!> before selection.
!>
!> @warning
!> `n` must agree with [[no_extra_dimensions]]: four for faces, two for the
!> paired axes, and one for `-`. Array assignment is not shape-guarded. Unknown
!> selectors leave blanks.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added extra-axis member labels. |
!> | 2026-04-03 | SvB | Made mixed-length character constructors explicit and portable. |
!> @endhistory
   PURE FUNCTION names_of_extra_dimensions(n, e_d) RESULT(r)
      INTEGER, INTENT(IN)            :: n   !! Result extent, normally from `no_extra_dimensions`.
      CHARACTER(LEN=*), INTENT(IN)   :: e_d !! Exact extra-axis selector.
      CHARACTER(LEN=6), DIMENSION(n) :: r   !! Fixed-width extra-axis labels.

      ! Initialize to blanks for `-`, unknown selectors, or surplus entries.
      r = ''

      SELECT CASE(e_d)
       CASE('-')
         r = ''
       CASE('faces')
         r = [CHARACTER(LEN=6) :: 'North', 'East', 'South', 'West']
       CASE('left_right')
         r = [CHARACTER(LEN=6) :: 'left', 'right']
       CASE('X_Y')
         r = [CHARACTER(LEN=6) :: 'x', 'y']
      END SELECT
   END FUNCTION names_of_extra_dimensions

END MODULE visualisation_metadata
