!> summary: Visualisation metadata catalogue and plan parser.
!>
!> This module owns the runtime catalogue of visualisation items requested for
!> HDF5 output. It registers static model variables, reads user-requested dynamic
!> items from the visualisation plan file, resolves masks/lists/timing blocks,
!> validates item dimensions, and creates the derived metadata consumed by the
!> HDF5 writer.
!>
!> Visualisation plan blocks:
!>
!> | Keyword | Action |
!> |:--------|:-------|
!> | `item` | Read one requested output variable and its grid/list, timing, layer, sediment, and contaminant selectors. |
!> | `list` | Read an explicit element list and derive square-only, bank-only, and river-only lists. |
!> | `mask` | Read a grid mask, remove inactive catchment cells, and derive scoped element lists. |
!> | `time` | Read output time-step and stop-time pairs for later `TIME_TO_RECORD` checks. |
!> | `diag` | Enable verbose visualisation-plan diagnostics in the check file. |
!> | `kill` | Stop after reading the plan so the check file can be inspected. |
!> | `stop` | Finish plan reading and continue the simulation. |
!>
!> Selector values:
!>
!> | Selector | Values | Meaning |
!> |:---------|:-------|:--------|
!> | `basis` | `grid_as_grid`, `grid_as_list`, `list_as_list` | Grid output, mask-derived list, or explicit list. |
!> | `scope` | `all`, `squares`, `banks`, `rivers` | Element classes retained in derived lists or gridded compound outputs. |
!> | `extra_dimensions` | `-`, `faces`, `X_Y`, `left_right` | Optional non-spatial dimension appended to the HDF5 item metadata. |
!>
!> `faces` labels are stored in north, east, south, west order for the output
!> file. Mask rows are read in the visualisation-plan order and then written
!> unchanged to the check file; row-orientation conversion is handled by the
!> interface layer that reads model data.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 200407 | JE | SHEGRAPH 2.0 | Created for SHEGRAPH visualisation metadata handling. |
!> | ? | ? | - | Item time-buffer pointers use `TYPE(C_PTR)` rather than integer pointer kinds. |
!> @endhistory
MODULE visualisation_metadata

   USE ISO_C_BINDING, ONLY: C_PTR, C_NULL_PTR
   USE VISUALISATION_PASS,      ONLY : SU_NUMBER, BANK_NO, RIVER_NO, EXISTS, nel, &
      IS_SQUARE, IS_BANK, IS_LINK, TOP_CELL, DIRQQ, nsed, ncon, &
      planfile, checkfile
   USE VISUALISATION_READ,      ONLY : vp_in, vp_out, mess, mess2, mess3, ERROR, R_C, R_I, R_R, COPY
   USE VISUALISATION_STRUCTURE, ONLY : MBR_COUNT, GET_MBR, csz

   IMPLICIT NONE

   INTEGER, PARAMETER                    :: ndim=6 !! Maximum number of dimensions in an HDF5 item.
   REAL, DIMENSION(:), ALLOCATABLE, SAVE :: previous_time !! Last output time recorded for each item.
   REAL, DIMENSION(:), ALLOCATABLE, SAVE :: next_time     !! Next scheduled output time for each item.
   LOGICAL, PARAMETER                    :: T=.TRUE.      !! Short logical true constant used by legacy code.
   LOGICAL, PARAMETER                    :: F=.FALSE.     !! Short logical false constant used by legacy code.
   REAL, PARAMETER                       :: zero = 0.0    !! Zero time initialiser.

!> Output timing definition from a `time` block in the visualisation plan.
   TYPE ttime
      PRIVATE
      INTEGER :: number !! User number for the `time` block.
      INTEGER :: sz     !! Number of time-step/stop-time pairs before the sentinel pair.
      REAL, DIMENSION(:), POINTER  :: tstep=>NULL() !! Output interval for each timing segment, in hours.
      REAL, DIMENSION(:), POINTER  :: tstop=>NULL() !! Stop time for each timing segment, in hours.
   END TYPE ttime
   TYPE(ttime), DIMENSION(:), POINTER :: times   !! Dynamic timing blocks read from the plan.
   TYPE(ttime), POINTER               :: sstatic !! Static-output timing sentinel.
!> Element list definition, including derived scope-specific lists.
   TYPE llist
      PRIVATE
      INTEGER                        :: number !! User list or source mask number.
      INTEGER                        :: sz=0   !! Number of active element numbers in `a`.
      INTEGER                        :: indx=0 !! Internal list-table index.
      CHARACTER(12)                  :: basis  !! Basis selector: `grid_as_grid`, `grid_as_list`, or `list_as_list`.
      CHARACTER(7)                   :: scope  !! Scope selector: `all`, `squares`, `banks`, or `rivers`.
      INTEGER, DIMENSION(:), POINTER :: a      !! Element numbers after scope filtering and sorting.
   END TYPE llist
   TYPE(LLIST), DIMENSION(:), POINTER :: lists=>NULL() !! All explicit and derived element lists.
!> Grid mask definition and its associated derived list index.
   TYPE mask
      PRIVATE
      INTEGER                          :: number !! User mask number.
      INTEGER                          :: ilow   !! First column covered by the mask.
      INTEGER                          :: ihigh  !! Last column covered by the mask.
      INTEGER                          :: jlow   !! First row covered by the mask.
      INTEGER                          :: jhigh  !! Last row covered by the mask.
      INTEGER                          :: listno !! First derived list associated with this mask.
      LOGICAL, DIMENSION(:,:), POINTER :: ma     !! Effective mask, after inactive catchment cells are removed.
   END TYPE mask
   TYPE(MASK), DIMENSION(:), POINTER :: masks=>NULL()      !! Masks read from the visualisation plan.
   TYPE(MASK), POINTER               :: whole_grid=>NULL() !! Singleton mask covering the full model grid.
!> User-facing visualisation item before conversion to HDF5 dimensions.
   TYPE item
      PRIVATE
      INTEGER :: users_number=0              !! User item number.
      INTEGER :: users_no_for_link_or_mask=0 !! Referenced user mask or list number.
      INTEGER :: users_no_for_times=0        !! Referenced user timing-block number.
      INTEGER :: sediment_no=0               !! Sediment fraction selector; zero when not sediment-dependent.
      INTEGER :: contaminant_no=0            !! Contaminant selector; zero when not contaminant-dependent.
      TYPE(C_PTR) :: first = C_NULL_PTR      !! First node in the item's time-buffer chain.
      TYPE(C_PTR) :: latest = C_NULL_PTR     !! Latest node in the item's time-buffer chain.
      CHARACTER(8)         :: name=''        !! Variable name used in the visualisation plan.
      CHARACTER(2)         :: typ=''         !! Internal visualisation type code plus static/dynamic suffix.
      CHARACTER(csz)       :: title='*S'     !! Title used for plots and printouts.
      CHARACTER(8)         :: units=''       !! Output units label.
      CHARACTER(12)        :: basis='grid_as_grid' !! Basis selector.
      CHARACTER(7)         :: scope='all'    !! Element scope selector.
      CHARACTER(11)        :: extra_dimensions = '-' !! Extra-dimension selector.
      LOGICAL              :: isgrid = F     !! True when the item is written as a grid rather than an element list.
      LOGICAL              :: istimeseries = F !! True for dynamic outputs.
      LOGICAL              :: varies_with_sediment=F !! True when a sediment fraction selector is required.
      LOGICAL              :: varies_with_contaminant=F !! True when a contaminant selector is required.
      LOGICAL              :: implemented=F  !! True once the model interface has registered the variable.
      INTEGER              :: layers(2)=(/0,0/) !! Bottom and top subsurface layer selectors; zero means no layer axis.
      TYPE(MASK), POINTER  :: amask=>NULL()  !! Associated mask for gridded output.
      TYPE(LLIST), POINTER :: alist=>NULL()  !! Associated element list for list output.
      TYPE(TTIME), POINTER :: atime=>NULL()  !! Associated output timing definition.
   END TYPE item
   TYPE(ITEM), DIMENSION(:), POINTER :: items=>NULL() !! Static and dynamic item catalogue.

!> HDF5-ready visualisation item metadata.
   TYPE hdf5_item
      INTEGER              :: users_number = 0              !! User item number.
      INTEGER              :: users_no_for_link_or_mask = 0 !! Referenced user mask or list number.
      INTEGER              :: users_no_for_times=0          !! Referenced user timing-block number.
      INTEGER              :: ilow = 0                      !! First column or list index.
      INTEGER              :: ihigh = 0                     !! Last column or list index.
      INTEGER              :: jlow = 0                      !! First row for gridded output.
      INTEGER              :: jhigh = 0                     !! Last row for gridded output.
      INTEGER              :: klow = 0                      !! First subsurface layer.
      INTEGER              :: khigh = 0                     !! Last subsurface layer.
      INTEGER              :: no_extra_dimensions = 0       !! Size of the selected extra dimension.
      INTEGER              :: tstep_no = 1                  !! Current HDF5 time-step record number.
      INTEGER              :: sz   = 0                      !! Size of `list` for list outputs.
      INTEGER              :: sediment_no   = 0             !! Sediment fraction selector.
      INTEGER              :: contaminant_no = 0            !! Contaminant selector.
      INTEGER, DIMENSION(:), POINTER :: dimensions !! Dimension sizes in HDF5 slot order.
      INTEGER, DIMENSION(:), POINTER :: szorder    !! Logical write order mapped onto HDF5 slot order.
      INTEGER, DIMENSION(:), POINTER :: list       !! Element numbers for list outputs.
      CHARACTER(8)         :: name=''              !! Variable name.
      CHARACTER(2)         :: typ=''               !! HDF5 visualisation type code plus static/dynamic suffix.
      CHARACTER(csz)       :: title='*S'           !! Plot/check-file title.
      CHARACTER(8)         :: units=''             !! Output units label.
      CHARACTER(12)        :: basis='grid_as_grid' !! Basis selector.
      CHARACTER(7)         :: scope='all'          !! Element scope selector.
      CHARACTER(11)        :: extra_dimensions = '-' !! Extra-dimension selector.
      CHARACTER(6), DIMENSION(:), POINTER :: names_of_extra_dimensions !! Labels for extra-dimension entries.
      CHARACTER(6), DIMENSION(:), POINTER :: names_of_dimensions       !! Labels for row/list/column/layer/type/extra/time axes.
      CHARACTER(6), DIMENSION(:), POINTER :: mbr                       !! Member labels such as square, bank, or link part.
      LOGICAL              :: isgrid       = F        !! True for gridded outputs.
      LOGICAL              :: istimeseries = F        !! True for dynamic outputs.
      LOGICAL              :: isreal       = T        !! True when the HDF5 value type is real.
      LOGICAL              :: varies_with_sediment=F  !! True when output varies by sediment fraction.
      LOGICAL              :: varies_with_contaminant=F !! True when output varies by contaminant.

   END TYPE hdf5_item
   TYPE(HDF5_ITEM), DIMENSION(:), POINTER :: hdf5_items=>NULL() !! HDF5-ready item catalogue.



   INTEGER                  :: no_times=0        !! Number of timing blocks read.
   INTEGER                  :: no_lists=0        !! Number of explicit and derived lists.
   INTEGER                  :: no_masks=0        !! Number of masks read.
   INTEGER                  :: no_items=0        !! Number of static and dynamic items.
   INTEGER                  :: no_static_items=0 !! Number of static items registered before dynamic plan reading.
   INTEGER, PARAMETER       :: sp=50             !! Legacy indentation width used in check-file writes.
   REAL, PARAMETER          :: small = 0.001     !! Time comparison tolerance, in hours.
   CHARACTER(4), PARAMETER  :: keywords(7) = (/'item', 'list', 'mask', 'time', 'stop', 'kill', 'diag'/) !! Plan keywords.
   CHARACTER(12),PARAMETER  :: basis(3) = (/'grid_as_grid', 'grid_as_list', 'list_as_list'/) !! Valid basis selectors.
   CHARACTER(7), PARAMETER  :: scope(4) = (/'all', 'squares', 'banks', 'rivers'/) !! Valid scope selectors.
   CHARACTER(11), PARAMETER :: extra_dimensions(4) = (/'-','faces','X_Y', 'left_right'/) !! Valid extra-dimension selectors.
   LOGICAL                  :: diagnostics=F     !! True when plan diagnostics are enabled.


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


!> Advances the HDF5 time-step counter for one metadata item.
   SUBROUTINE INCREMENT_HDF5_TSTEP_NO(mn)
      INTEGER, INTENT(IN) :: mn
      hdf5_items(mn)%tstep_no = hdf5_items(mn)%tstep_no + 1
   END SUBROUTINE INCREMENT_HDF5_TSTEP_NO


!> Returns whether item `n` should be recorded at the current simulation time.
!>
!> The first call allocates per-item timing state and schedules the first output
!> for each item. Time zero is always recorded. Later calls compare `time` with
!> the next scheduled output time using `small` as the tolerance, then advance
!> the schedule for that item.
   LOGICAL FUNCTION time_to_record(n, time) RESULT(r)
      INTEGER, INTENT(IN)                   :: n
      INTEGER                               :: i
      REAL, INTENT(IN)                      :: time  !! Current simulation time, in hours.
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

!> Returns the next scheduled output time for an item.
!>
!> The selected timing segment is the first `tstop` greater than the previous
!> output time. `read_time` appends a `HUGE` sentinel, so normal plan data should
!> always provide a terminating segment.
   ELEMENTAL REAL FUNCTION get_next_time(n) RESULT(r)
      INTEGER, INTENT(IN) :: n !! Item index.
      INTEGER             :: j !! Timing-segment index.
      j = 0
      DO
         j = j + 1
         IF(items(n)%atime%tstop(j)>previous_time(n)) EXIT
      ENDDO
      r = MIN(items(n)%atime%tstop(j), previous_time(n) + items(n)%atime%tstep(j))
   END FUNCTION get_next_time


!> Returns character metadata for a visualisation item.
!>
!> Valid `text` selectors are `basis`, `name`, `title`, `typ`, `units`, `scope`,
!> and `extra_dimensions`; unknown selectors return a diagnostic string.
   PURE FUNCTION get_metadata_c(i, text) RESULT(r)
      INTEGER, INTENT(IN)      :: i    !! Item index.
      CHARACTER(*), INTENT(IN) :: text !! Metadata selector.
      CHARACTER(csz)           :: r    !! Character metadata value.
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

!> Returns character metadata for an HDF5-ready item.
!>
!> Selectors that name dimension labels or member labels require optional index
!> `e`; unknown selectors return a diagnostic string.
   ELEMENTAL FUNCTION get_metadata_HDF5_c(i, text, e) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! HDF5 item index.
      INTEGER, INTENT(IN), OPTIONAL :: e    !! Dimension/member label index.
      CHARACTER(*), INTENT(IN)      :: text !! Metadata selector.
      CHARACTER(csz)                :: r    !! Character metadata value.
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

!> Returns integer metadata for a visualisation item.
!>
!> Bounds are returned as mask limits for gridded items and as list limits for
!> list items. Unknown selectors return `HUGE(1)`.
   ELEMENTAL INTEGER FUNCTION get_metadata_i(i, text, su) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! Item index.
      INTEGER, INTENT(IN), OPTIONAL :: su   !! Element-list position for selector `su`.
      CHARACTER(*), INTENT(IN)      :: text !! Metadata selector.
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

!> Returns a stored C pointer for an item's time-buffer chain.
   ELEMENTAL FUNCTION get_metadata_ptr(i, text, su) RESULT(r)
      INTEGER, INTENT(IN) :: i              !! Item index.
      INTEGER, INTENT(IN), OPTIONAL :: su   !! Unused compatibility argument.
      CHARACTER(*), INTENT(IN) :: text      !! Pointer selector: `first` or `latest`.
      TYPE(C_PTR) :: r                      !! Stored C pointer, or `C_NULL_PTR` for an unknown selector.
      SELECT CASE(text)
       CASE('first') ; r=items(i)%first
       CASE('latest') ; r=items(i)%latest
       CASE DEFAULT ; r=C_NULL_PTR
      END SELECT
   END FUNCTION get_metadata_ptr

!> Returns integer metadata for an HDF5-ready item.
!>
!> Selectors that access arrays use optional index `e`. Unknown selectors return
!> `HUGE(1)`.
   ELEMENTAL INTEGER FUNCTION get_metadata_hdf5_i(i, text, e) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! HDF5 item index.
      INTEGER, INTENT(IN), OPTIONAL :: e    !! Dimension, order, list, or member index.
      CHARACTER(*), INTENT(IN)      :: text !! Metadata selector.
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

!> Stores a C pointer for an item's first or latest time-buffer node.
   SUBROUTINE set_metadata_ptr(i, text, a)
      INTEGER, INTENT(IN) :: i           !! Item index.
      TYPE(C_PTR), INTENT(IN) :: a       !! Pointer value to store.
      CHARACTER(*), INTENT(IN) :: text   !! Pointer selector: `first` or `latest`.
      SELECT CASE(text)
       CASE('first') ; items(i)%first = a
       CASE('latest') ; items(i)%latest = a
      END SELECT
   END SUBROUTINE set_metadata_ptr

!> Returns logical metadata for a visualisation item.
!>
!> The `on` selector reads the associated mask at indices `a,b`; other selectors
!> describe item flags. Unknown selectors return false.
   PURE LOGICAL FUNCTION get_metadata_L(I, text, a, b) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! Item index.
      INTEGER, INTENT(IN), OPTIONAL :: a    !! Mask column index for selector `on`.
      INTEGER, INTENT(IN), OPTIONAL :: b    !! Mask row index for selector `on`.
      CHARACTER(*), INTENT(IN)      :: text !! Metadata selector.
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


!> Returns logical metadata for an HDF5-ready item.
   PURE LOGICAL FUNCTION get_metadata_HDF5_L(I, text) RESULT(r)
      INTEGER, INTENT(IN)           :: i    !! HDF5 item index.
      CHARACTER(*), INTENT(IN)      :: text !! Metadata selector.
      SELECT CASE(text)
       CASE('isgrid')                  ; r=hdf5_items(i)%isgrid
       CASE('istimeseries')            ; r=hdf5_items(i)%istimeseries
       CASE('isreal')                  ; r = ANY(hdf5_items(i)%typ(1:1)==(/'B','G','L','M'/))
       CASE('varies_with_sediment')    ; r=hdf5_items(i)%varies_with_sediment
       CASE('varies_with_contaminant') ; r=hdf5_items(i)%varies_with_contaminant
       CASE DEFAULT         ; r = F
      END SELECT
   END FUNCTION get_metadata_HDF5_L


!> Reads dynamic visualisation requests from the visualisation plan file.
!>
!> The plan is copied to the parser input unit, read block-by-block until `stop`
!> or `kill`, then user mask/list/time numbers are linked to their resolved
!> metadata. `kill` deliberately stops the simulation after writing the check
!> file.
   SUBROUTINE read_dynamic_visualisation_metadata()
      INTEGER              :: i   !! Dynamic item index.
      CHARACTER(4)         :: now !! Current visualisation-plan keyword.
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
      CLOSE (UNIT=vp_in,status="delete")

   END SUBROUTINE read_dynamic_visualisation_metadata


!> Registers one static visualisation variable supplied by the model interface.
   SUBROUTINE register_static_visualisation_metadata(name, typ, units, title, szi, szj, extra_dimensions, varies_with_elevation)
      INTEGER, INTENT(IN)      :: szi                  !! Number of columns in the static grid.
      INTEGER, INTENT(IN)      :: szj                  !! Number of rows in the static grid.
      CHARACTER(*), INTENT(IN) :: name                 !! Static variable name.
      CHARACTER(*), INTENT(IN) :: units                !! Units label.
      CHARACTER(*), INTENT(IN) :: title                !! Check-file and plotting title.
      CHARACTER(*), INTENT(IN) :: extra_dimensions     !! Extra-dimension selector.
      CHARACTER, INTENT(IN)    :: typ                  !! Base visualisation type code.
      LOGICAL, INTENT(IN)      :: varies_with_elevation !! True when the item has a layer axis.
      TYPE(ITEM), POINTER      :: ii                   !! Newly created static item.
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


!> Writes one static variable entry to the visualisation check file.
   SUBROUTINE write_sta_variable(name, units, title, extra_dimensions, varies_with_elev)
      CHARACTER(*), INTENT(IN)         :: name             !! Static variable name.
      CHARACTER(*), INTENT(IN)         :: units            !! Units label.
      CHARACTER(*), INTENT(IN)         :: title            !! Check-file title.
      CHARACTER(*), INTENT(IN)         :: extra_dimensions !! Extra-dimension selector.
      LOGICAL, INTENT(IN)              :: varies_with_elev !! True when the item has a layer axis.
      LOGICAL, SAVE                    :: first=T          !! True before the check file is opened.
      CHARACTER(LEN(extra_dimensions)) :: ed               !! Printed extra-dimension selector.
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
!> Writes one dynamic variable entry to the visualisation check file.
   SUBROUTINE write_dyn_variable(name, units, title, extra_dimensions, varies_with_elev, varies_with_sed, varies_with_con)
      CHARACTER(*), INTENT(IN)         :: name             !! Dynamic variable name.
      CHARACTER(*), INTENT(IN)         :: units            !! Units label.
      CHARACTER(*), INTENT(IN)         :: title            !! Check-file title.
      CHARACTER(*), INTENT(IN)         :: extra_dimensions !! Extra-dimension selector.
      LOGICAL, INTENT(IN)              :: varies_with_elev !! True when the item has a layer axis.
      LOGICAL, INTENT(IN)              :: varies_with_sed  !! True when sediment fraction is required.
      LOGICAL, INTENT(IN)              :: varies_with_con  !! True when contaminant number is required.
      LOGICAL, SAVE                    :: first=T          !! True before the check file is opened.
      IF(first) THEN
         first = F
!    OPEN(unit=uw, FILE=TRIM(DIRQQ)//'/'//'output/check_visualisation_plan.txt', ACTION='WRITE', STATUS='UNKNOWN')
         !print*, 'CHECKFILE2 = ', TRIM(checkfile)
         OPEN(unit=vp_out, FILE=checkfile, ACTION='WRITE', STATUS='UNKNOWN')
         WRITE(vp_out,'(A80)') REPEAT('-',80)
         WRITE(vp_out,'(A)') 'Full list of variables that can be recorded in the HDF5 file'
         WRITE(vp_out,'(A)') 'E-varies with subsurface elevation; C-varies with contaminant no; '// &
            'S-varies with sediment fraction no'
      ENDIF
      WRITE(vp_out,'(A8, A8, A9, A12, A70)') name, V_E_SED_CON(varies_with_elev,varies_with_sed,varies_with_con), &
         units, extra_dimensions, title
   END SUBROUTINE write_dyn_variable
!> Encodes elevation, sediment, and contaminant variability for the check file.
   PURE CHARACTER(7) FUNCTION v_e_sed_con(v,s,c) RESULT(r)
      INTEGER             :: p !! Character position used while packing flags.
      LOGICAL, INTENT(IN) :: v !! Elevation-varying flag.
      LOGICAL, INTENT(IN) :: s !! Sediment-varying flag.
      LOGICAL, INTENT(IN) :: c !! Contaminant-varying flag.
      r = REPEAT(' ',LEN(r))
      p = 3
      IF(v) THEN ; r(p:p)='E' ; p=p+2 ; ENDIF
      IF(s) THEN ; r(p:p)='S' ; p=p+2 ; ENDIF
      IF(c) r(p:p)='C'
   END FUNCTION v_e_sed_con
!> Encodes elevation variability for a static check-file entry.
   PURE CHARACTER(5) FUNCTION v_elev(v) RESULT(r)
      INTEGER             :: p !! Character position for the elevation flag.
      LOGICAL, INTENT(IN) :: v !! Elevation-varying flag.
      r = REPEAT(' ',LEN(r))
      p = 3
      IF(v) r(p:p)='E'
   END FUNCTION v_elev


!> Registers and validates dynamic visualisation variables.
!>
!> Calls with `jj==1` only write the catalogue of implemented variables to the
!> check file. The first later call reads the user plan. The `final` call checks
!> that every requested item was recognised and implemented, validates selectors,
!> and creates the HDF5 metadata table.
   SUBROUTINE register_dynamic_visualisation_metadata(jj, final, name, typ, units, title, &
      extra_dimensions, varies_with_elevation, varies_with_sed, varies_with_con, implemented)
      INTEGER                                  :: i          !! Dynamic item index.
      INTEGER, INTENT(IN)                      :: jj         !! Registration pass number.
      CHARACTER(*), INTENT(IN)                 :: name       !! Dynamic variable name.
      CHARACTER(*), INTENT(IN)                 :: units      !! Units label.
      CHARACTER(*), INTENT(IN)                 :: title      !! Check-file and plotting title.
      CHARACTER(*), INTENT(IN)                 :: extra_dimensions !! Extra-dimension selector.
      CHARACTER, INTENT(IN)                    :: typ        !! Base model visualisation type code.
      LOGICAL, INTENT(IN)                      :: final      !! True on the final registration call.
      LOGICAL, INTENT(IN)                      :: varies_with_elevation !! True when the item has a layer axis.
      LOGICAL, INTENT(IN)                      :: varies_with_sed       !! True when sediment fraction is required.
      LOGICAL, INTENT(IN)                      :: varies_with_con       !! True when contaminant number is required.
      LOGICAL, INTENT(IN)                      :: implemented           !! True when the variable can be output.
      LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE :: found      !! Per-request match flags for dynamic items.
      TYPE(ITEM), POINTER                      :: ii=>NULL() !! Matched dynamic item.
      LOGICAL, SAVE                            :: first=T    !! True before the user plan has been read.
      IF(jj==1) THEN
         IF(implemented) CALL WRITE_DYN_VARIABLE(name, units, title, extra_dimensions, varies_with_elevation, &
            varies_with_sed, varies_with_con)
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

!> Converts internal item metadata to the HDF5-ready metadata table.
!>
!> This copies stable item fields, resolves extra-dimension labels, calculates
!> dimension slots, and copies list element numbers for list outputs.
   SUBROUTINE create_hdf5_metadata()
      INTEGER                  :: mn  !! Item index.
      INTEGER                  :: nex !! Size of the selected extra dimension.
      TYPE(ITEM), POINTER      :: ii  !! Source internal item.
      TYPE(HDF5_ITEM), POINTER :: hh  !! Destination HDF5 metadata item.
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


!> Calculates HDF5 dimension sizes and dimension ordering for one item.
!>
!> HDF5 metadata uses six fixed slots. The write order is stored separately in
!> `szorder` so grid/list geometry, layers, member type, extra dimensions, and
!> time can be omitted by setting their size to zero.
!>
!> | Slot | Dimension label | Size source |
!> |:-----|:----------------|:------------|
!> | 1 | `time` | `1` for time series, otherwise `0`. |
!> | 2 | `extra` | Extra-dimension count; singletons are suppressed to `0`. |
!> | 3 | `layer` | `khigh-klow+1` when `khigh>0`, otherwise `0`. |
!> | 4 | `el_typ` | Structure member count; singletons are suppressed to `0`. |
!> | 5 | `column` or empty row slot | Number of columns for grids, otherwise `0`. |
!> | 6 | `row` or `el-lst` | Number of rows for grids, or list length for list outputs. |
   SUBROUTINE GET_SZ_CR(h)
      INTEGER                  :: r      !! HDF5 dimension slot currently being filled.
      INTEGER                  :: mbr    !! Number of structure members for the item type.
      INTEGER                  :: nextra !! Extra-dimension count.
      TYPE(HDF5_ITEM), POINTER :: h      !! HDF5 metadata item to update.
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
      h%names_of_dimensions(r)='row'
      h%szorder(2) = r
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


!> Calculates the nominal HDF5 rank for an item.
   PURE INTEGER FUNCTION calc_rank(h) RESULT(r)
      INTEGER                  :: mbr !! Number of structure members for the item type.
      TYPE(HDF5_ITEM), POINTER :: h   !! HDF5 metadata item.
      mbr = MBR_COUNT(h%typ)
      r   = 1
      IF(h%jhigh-h%jlow>1)        r=r+1
      IF(h%khigh-h%klow>1)        r=r+1
      IF(mbr>1)                   r=r+1
      IF(h%no_extra_dimensions>1) r=r+1
      IF(h%istimeseries)          r=r+1
   END FUNCTION calc_rank


!> Maps a model data type and requested scope to a visualisation storage type.
!>
!> `W` data are rejected. List outputs are collapsed to list-compatible storage
!> types, while gridded outputs preserve `typ` for `scope='all'` and remap
!> sub-scopes as follows:
!>
!> | Scope | `C` | `G` | `H` | `L` | `Q` |
!> |:------|:----|:----|:----|:----|:----|
!> | list output | `V` | `M` | `X` | `M` | `Z` |
!> | `squares` | `V` | `M` | `X` | `L` | `Z` |
!> | `banks` | `K` | `B` | `T` | `L` | `A` |
!> | `rivers` | `O` | `L` | `U` | `L` | `D` |
   CHARACTER FUNCTION alter_dynamic_type(typ, ii) RESULT(r)
      CHARACTER, INTENT(IN)     :: typ !! Base model visualisation type code.
      TYPE(ITEM), INTENT(INOUT) :: ii  !! Requested item, including basis and scope.
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



!> Dispatches one visualisation plan keyword block to its reader.
!>
!> `list` and `mask` blocks create derived scope-specific lists immediately
!> after the original source list/mask so `EXTRA(scope)` can later select them.
   SUBROUTINE HANDLE(now)
      INTEGER                  :: orig !! Index of the original explicit list before derived lists are appended.
      CHARACTER(4), INTENT(IN) :: now  !! Current visualisation-plan keyword.
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

!> Links user numbers in an item to the resolved mask/list/time metadata.
   SUBROUTINE link_users_numbers_to_indexes(it)
      INTEGER                   :: uun !! User mask or list number copied from the item.
      TYPE(ITEM), INTENT(INOUT) :: it  !! Item whose pointers and grid/list flag are updated.
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



!> Returns the static-output timing sentinel.
!>
!> The singleton timing block uses user number `999` and `HUGE` step/stop times
!> so static variables are metadata-only and are not scheduled as dynamic series.
   FUNCTION point_to_static() RESULT(r)
      TYPE(TTIME), POINTER :: r       !! Pointer to the singleton static timing block.
      LOGICAL, SAVE        :: first=T !! True until the singleton has been allocated.
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


!> Returns a mask covering the whole model grid.
!>
!> The singleton is sized by the first call; later calls return the same mask.
   FUNCTION point_to_whole_grid(i,j) RESULT(r)
      TYPE(MASK), POINTER :: r       !! Pointer to the singleton full-grid mask.
      INTEGER, INTENT(IN) :: i       !! Number of columns used when first allocating the mask.
      INTEGER, INTENT(IN) :: j       !! Number of rows used when first allocating the mask.
      LOGICAL, SAVE :: first=T       !! True until the singleton has been allocated.
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



!> Returns the list offset associated with an item scope.
   ELEMENTAL INTEGER FUNCTION extra(s) RESULT(r)
      CHARACTER(*), INTENT(IN) :: s !! Scope selector.
      SELECT CASE(s)
       CASE('all')    ; r=0
       CASE('squares') ; r=1
       CASE('banks')  ; r=2
       CASE('rivers')  ; r=3
      END SELECT
   END FUNCTION extra

!> Returns the mask matching a user mask number.
   FUNCTION point_to_mask(users_no_for_link_or_mask) RESULT(r)
      INTEGER, INTENT(IN) :: users_no_for_link_or_mask !! User mask number.
      INTEGER             :: I                         !! Mask-table index.
      TYPE(MASK), POINTER :: r                         !! Matching mask pointer.
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

!> Returns the list matching a user list/mask number, basis, and scope.
!>
!> For `grid_as_list`, the user number refers to a mask and `scope` selects one
!> of the four derived mask lists. For `list_as_list`, the user number refers to
!> an explicit list and `scope` selects the corresponding derived list.
   FUNCTION point_to_list(users_no_for_link_or_mask, basis, scope) RESULT(r)
      INTEGER                  :: i                         !! Mask/list table index.
      INTEGER                  :: j                         !! Non-zero match flag and matched index.
      INTEGER, INTENT(IN)      :: users_no_for_link_or_mask !! User mask or list number.
      CHARACTER(*), INTENT(IN) :: basis                     !! Basis selector.
      CHARACTER(*), INTENT(IN) :: scope                     !! Scope selector.
      TYPE(LLIST), POINTER :: r                             !! Matching explicit or derived list.
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

!> Returns the timing block matching a user time number.
   FUNCTION point_to_time(users_no_for_times) RESULT(r)
      INTEGER, INTENT(IN)  :: users_no_for_times !! User timing-block number.
      INTEGER              :: I                  !! Timing-table index.
      TYPE(TTIME), POINTER :: r                  !! Matching timing-block pointer.
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

!> Returns a copy of one internal visualisation item.
   TYPE(ITEM) FUNCTION get_item(i) RESULT(r)
      INTEGER, INTENT(IN) :: i !! Item index.
      r = items(i)
   END FUNCTION get_item


!> Returns the number of registered visualisation items.
!>
!> Current implementation returns the total when optional `text` is present.
!> The absent-argument branch contains legacy static/dynamic selector code but
!> cannot be used safely because it references the absent optional argument.
   PURE INTEGER FUNCTION no_of_items(text) RESULT(r)
      CHARACTER(*), INTENT(IN), OPTIONAL :: text !! Legacy selector, intended values `static` or `dynamic`.
      IF(PRESENT(text)) THEN
         r = no_items
      ELSE
         SELECT CASE(text)
          CASE('static')  ; r=no_static_items
          CASE('dynamic') ; r=no_items-no_static_items
         END SELECT
      ENDIF
   END FUNCTION no_of_items



!> Reads plan-file tokens until a recognised keyword is found.
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

!> Reads one requested visualisation item from the plan file.
!>
!> `as_above` copies the previous item and then restores the new item's name and
!> number. Layer limits are reordered if the user supplies them high-to-low.
   SUBROUTINE read_item(s)
      INTEGER                   :: number !! User item number preserved across `as_above`.
      CHARACTER(csz)            :: dum    !! Plan heading currently being read.
      CHARACTER(csz)            :: name   !! Item name preserved across `as_above`.
      TYPE(ITEM), INTENT(INOUT) :: s      !! Item populated from the plan block.
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




!> Validates one requested visualisation item.
!>
!> Checks selector names and enforces sediment/contaminant numbers only when
!> the registered variable varies with those dimensions.
   SUBROUTINE check_item(a)
      TYPE(ITEM), INTENT(IN) :: a !! Item to validate.
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

!> Performs cross-item validation after all requested items are linked.
   SUBROUTINE final_check_of_item()
      INTEGER             :: i   !! Item index.
      INTEGER             :: cnt !! Number of cross-item validation failures.
      TYPE(ITEM), POINTER :: a   !! Current item being checked.
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


!> Reads one output timing block from the plan file.
!>
!> A sentinel `HUGE` step/stop pair is appended so timing lookup can continue
!> past the final user-specified segment without a separate bounds check.
   SUBROUTINE read_time(t)
      INTEGER                    :: i !! Timing-pair index.
      TYPE(TTIME), INTENT(INOUT) :: t !! Timing block populated from the plan.
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


!> Reads one explicit element list from the plan file.
   SUBROUTINE read_list(L)
      INTEGER                    :: i   !! List-entry index.
      INTEGER                    :: cnt !! Number of invalid element numbers.
      TYPE(LLIST), INTENT(INOUT) :: L   !! Explicit list populated from the plan.
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

!> Builds a scoped element list from a mask.
!>
!> Scope expansion uses a nine-slot cell layout for `all`: one square, four
!> banks, and four river links. Zeros and duplicates are removed by `sort`.
   TYPE(LLIST) FUNCTION make_list_from_mask(m, txt) RESULT(r)
      INTEGER                  :: num !! Number of element slots contributed by each active cell.
      CHARACTER(*), INTENT(IN) :: txt !! Scope selector for the derived list.
      TYPE(MASK), INTENT(IN)   :: m   !! Source mask.
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

      !> Expands active masked cells into subunit, bank, and river element numbers.
      SUBROUTINE loops()
         INTEGER :: c  !! Output-list write position.
         INTEGER :: i  !! Mask column index.
         INTEGER :: j  !! Mask row index.
         INTEGER :: su !! Subunit number for the current mask cell.
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

!> Builds a scoped element list from an explicit element list.
   TYPE(LLIST) FUNCTION make_list_from_list(L, txt) RESULT(r)
      INTEGER                  :: num !! Number of element slots contributed by each source entry.
      CHARACTER(*), INTENT(IN) :: txt !! Scope selector for the derived list.
      TYPE(LLIST), INTENT(IN)  :: L   !! Source explicit list.
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

      !> Filters the source list down to elements matching the requested scope.
      SUBROUTINE lists()
         INTEGER :: c  !! Output-list write position.
         INTEGER :: i  !! Source-list index.
         INTEGER :: j  !! Unused legacy workspace.
         INTEGER :: su !! Current element number from the source list.
         LOGICAL :: iss !! True when `su` matches the requested scope.
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

!> Returns the number of element slots contributed by a scope.
   PURE INTEGER FUNCTION get_num(txt) RESULT(r)
      CHARACTER(*), INTENT(IN) :: txt !! Scope selector.
      SELECT CASE(txt)
       CASE('all')     ; r=9
       CASE('squares') ; r=1
       CASE('banks')   ; r=4
       CASE('rivers')  ; r=4
      END SELECT
   END FUNCTION get_num


!> Removes zeros and duplicates from an element list and sorts it ascending.
!>
!> The routine assumes at least one positive candidate value, because it sizes a
!> logical work array with `MAXVAL(a)`. Callers construct candidate lists from
!> existing model elements before calling it.
   SUBROUTINE sort(sza, a)
      INTEGER, INTENT(INOUT)             :: sza !! Input candidate count; output unique positive count.
      INTEGER                            :: i   !! Candidate or work-array index.
      INTEGER                            :: j   !! Count/write position.
      INTEGER                            :: szd !! Maximum positive element number used to size `d`.
      INTEGER, DIMENSION(:), POINTER     :: a   !! Element list, reallocated when zeros/duplicates are removed.
      LOGICAL, DIMENSION(:), ALLOCATABLE :: d   !! Presence map keyed by element number.
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

!> Reads one grid mask from the plan file and removes cells outside the catchment.
!>
!> Mask bounds are normalised to low/high order before reading. Characters in
!> `off` mark inactive cells; all other characters initially mark active cells,
!> then `EXISTS(SU_NUMBER(i,j))` removes locations outside the catchment.
   SUBROUTINE read_mask(m, off)
      INTEGER                              :: i   !! Mask column index or temporary bound swap value.
      INTEGER                              :: j   !! Mask row index.
      CHARACTER, DIMENSION(:), INTENT(IN)  :: off !! Characters that indicate an inactive mask cell.
      CHARACTER                            :: c   !! Mask character read from the plan file.
      TYPE(MASK), INTENT(INOUT)            :: m   !! Mask populated from the plan.
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
            m%ma(i,j) = m%ma(i,j) .AND. EXISTS(SU_NUMBER(i,j))  !effective mask
         ENDDO
      ENDDO
      CALL mask_write('effective mask', m%ma, 'T', '.')
   END SUBROUTINE read_mask

!> Writes a logical mask to the visualisation check file.
   SUBROUTINE mask_write(txt, ma, tr, fa)
      CHARACTER(*), INTENT(IN) :: txt    !! Check-file heading for the mask.
      LOGICAL, INTENT(IN)      :: ma(:,:) !! Mask to print.
      CHARACTER(1), INTENT(IN) :: tr     !! Character used for true cells.
      CHARACTER(1), INTENT(IN) :: fa     !! Character used for false cells.

      CHARACTER(1), ALLOCATABLE :: cc(:,:) !! Printable character copy of `ma`.
      CHARACTER(20)             :: fmt_str !! Runtime format replacing the old Intel `<SIZE>` extension.

      IF (SIZE(ma, 1) == 0 .OR. SIZE(ma, 2) == 0) RETURN

      ALLOCATE(cc(SIZE(ma, 1), SIZE(ma, 2)))
      WHERE(ma)
         cc = tr
      ELSEWHERE
         cc = fa
      END WHERE

      WRITE(vp_out,'(50X,A)') txt
      ! Dynamically build format string to replace Intel-specific <SIZE> extension
      WRITE(fmt_str, '("(",I0,"A)")') SIZE(cc, 1)
      WRITE(vp_out, fmt_str) cc

      DEALLOCATE(cc)
   END SUBROUTINE mask_write


!> Extends the internal visualisation item table.
   SUBROUTINE INCREMENT_item(s,n)
      TYPE(ITEM), DIMENSION(:), POINTER :: s        !! Pointer table to extend.
      TYPE(ITEM), DIMENSION(:), POINTER :: old=>NULL() !! Temporary pointer used by `include_increment.f90`.
      INCLUDE 'include_increment.f90'
      no_items   = no_items + 1
   END SUBROUTINE INCREMENT_item

!> Extends the internal list table.
   SUBROUTINE INCREMENT_LIST(s,n)
      TYPE(LLIST), DIMENSION(:), POINTER :: s        !! Pointer table to extend.
      TYPE(LLIST), DIMENSION(:), POINTER :: old=>NULL() !! Temporary pointer used by `include_increment.f90`.
      INCLUDE 'include_increment.f90'
      no_lists   = no_lists + 1
   END SUBROUTINE INCREMENT_LIST
!> Extends the internal mask table.
   SUBROUTINE INCREMENT_MASK(s,n)
      TYPE(MASK), DIMENSION(:), POINTER :: s        !! Pointer table to extend.
      TYPE(MASK), DIMENSION(:), POINTER :: old=>NULL() !! Temporary pointer used by `include_increment.f90`.
      INCLUDE 'include_increment.f90'
      no_masks  = no_masks + 1
   END SUBROUTINE INCREMENT_MASK
!> Extends the internal timing-block table.
   SUBROUTINE INCREMENT_TIME(s,n)
      TYPE(TTIME), DIMENSION(:), POINTER :: s        !! Pointer table to extend.
      TYPE(TTIME), DIMENSION(:), POINTER :: old=>NULL() !! Temporary pointer used by `include_increment.f90`.
      INCLUDE 'include_increment.f90'
      no_times  = no_times + 1
   END SUBROUTINE INCREMENT_TIME

!> Returns the number of entries in an extra-dimension selector.
!>
!> `faces` is north/east/south/west, `left_right` is left/right, and `X_Y` is
!> x/y. `-` is treated as a singleton so metadata arrays can still be allocated.
   ELEMENTAL INTEGER FUNCTION no_extra_dimensions(e_d) RESULT(r)
      CHARACTER(*), INTENT(IN) :: e_d !! Extra-dimension selector.
      SELECT CASE(e_d)
       CASE('-')        ; r = 1
       CASE('faces')       ; r = 4
       CASE('left_right')  ; r = 2
       CASE('X_Y')         ; r = 2
      END SELECT
   END FUNCTION no_extra_dimensions

!> Returns labels for an extra-dimension selector.
   FUNCTION names_of_extra_dimensions(n,e_d) RESULT(r)
      INTEGER, INTENT(IN)        :: n   !! Number of labels to return.
      CHARACTER(*), INTENT(IN)   :: e_d !! Extra-dimension selector.
      CHARACTER(6), DIMENSION(n) :: r   !! Labels for the selected extra dimension.
      SELECT CASE(e_d)
       CASE('-')        ; r = ''
       CASE('faces')       ; r = (/'North', 'East', 'South', 'West'/)
       CASE('left_right')  ; r = (/'left','right'/)
       CASE('X_Y')         ; r = (/'x','y'/)
      END SELECT
   END FUNCTION names_of_extra_dimensions

END MODULE visualisation_metadata
